//=--------------------------------------------------------------------------=
// Assert.Cpp
//=--------------------------------------------------------------------------=
// Assertion code for VS components
//=--------------------------------------------------------------------------=
// Copyright (c) 1997-2002, Microsoft Corporation
//			All Rights Reserved
// Information Contained Herein Is Proprietary and Confidential.
//=--------------------------------------------------------------------------=

#include "Pch.H"
#include <stdio.h>
#include "vsassert.h"
#include "StackWalk.h"
#include "Main.H"
#include "mem.h"
#include "Util.H"
#include "Dump.H"
#include "resource.h"
#include "dbghelp.h"
#ifdef DEBUG
#include "dbghlper.h"
#endif

// from ntstatus.h
//#define STATUS_ASSERTION_FAILURE         ((NTSTATUS)0xC0000420L)

#define MAX_RECURSE 100	// # of times we can display can recursively display an assert

//=--------------------------------------------------------------------------=
// Global data
//=--------------------------------------------------------------------------=
DWORD g_dwDebugThreadId;    // ID of the debugging thread, if it's running
typedef BOOL WINAPI MDPROC (HANDLE,DWORD,HANDLE,MINIDUMP_TYPE,
                            PMINIDUMP_EXCEPTION_INFORMATION,
                            PMINIDUMP_USER_STREAM_INFORMATION,
                            PMINIDUMP_CALLBACK_INFORMATION);
MDPROC *_gpfnMiniDumpWriteDump = 0;


// Used for assert to fool the compiler since otherwise it barks with C4127 on do { ... } while(false); in VSASSERT
bool ENTRYPOINT VsAssertReturnFalse() { return false; }

//=--------------------------------------------------------------------------=
// Private data
//=--------------------------------------------------------------------------=
typedef BOOL (WINAPI* ISDEBUGGERPRESENT)();

static TCHAR             _szFormat[]  = "ASSERT FAILED, PID:0x%lx(%d)  TID:0x%lx: %s, File: %s, Line: %d:\r\n\r\n%s\r\n\r\n";
static TCHAR             _szTitle[]  =  "MemSpect Assertion  (Abort = UAE, Retry = INT 3, Ignore = Continue, Ctrl-Ignore = Disable Assert, Shift-Ignore = Disable all Asserts)";
static TCHAR             _szLeakReportTitle[]  =  "MemSpect Assertion  (Abort = UAE, Retry = INT 3, Ignore = Continue, Ctrl-Ignore = View leaks, Shift-Ignore = Disable all Asserts)";

static int               _nRecurseCnt;
static BOOL              _fStartedDebugger = FALSE;
static ISDEBUGGERPRESENT _IsDebuggerPresent = NULL;

static CMutex _csDebugLock;
static IMMEDIATECALLBACK _pfnHostDebugWindow;
BOOL _fDisableAllAsserts;
BOOL _fThrowAllAsserts=TRUE;

#pragma pack (push, 1)

typedef struct _TIB {
    PVOID			ExceptionList;
    PVOID			StackLimit;
    PVOID			StackBase;
    PVOID			SubSystemTib;
    PVOID			Something1;
    PVOID			ArbitraryUserPointer;
    struct _TIB*	Self;
    WORD			Flags;
    WORD			Win16MutextCount;
    PVOID			DebugContext;
    DWORD			CurrentPriority;
    DWORD			MessageQueueSelector;
    PVOID*			TlsSlots;		// most likely an array
} TIB;

#pragma pack (pop)

#define OffsetTib 0x18


DEFINE_SWITCH(_fSingleThreadedAssert, "VsAssert", "Single threaded assert dialog.");

//=--------------------------------------------------------------------------=
// Data used to communicate with the debugging thread
//

enum DEBUG_ACTION
{
    ACTION_ASSERT,      // Cause an assert
    ACTION_CALLBACK,    // callback to given function
    ACTION_TERMINATE    // We're closing down.  Terminate the debug thread.
};

typedef HRESULT (*PTHREADCALLBACK)(INT_PTR dwData);

static struct DEBUG_THREAD_DATA
{
    DWORD           dwThreadId;   // debug thread id
    PVOID           pvExtra;      // extra data anyone may use
    HANDLE          hThread;      // handle to debugging thread
    HANDLE          hevtAction;   // to signal debug thread that something should happen
    HANDLE          hevtResume;   // set when the thread is going idle
    DEBUG_ACTION    nAction;      // action that hevtAction signifies
    LPCSTR          pszText;      // text to display
    LPCSTR          pszTitle;     // title to display
    UINT            mbFlags;      // message box flags
    BOOL            fAllowThrow;    // true if the message box should enable throwing-related controls
    BOOL            fDisableAll;    // true if the user checked the 'disable all asserts' check
    BOOL            fThrowAll;      // true if the user checked the 'throw when poss' check
    BOOL            fDisableThis;   // true if the user checked the 'disable this assert' check
    BOOL            fThrowThis;     // true if the user checked the 'throw this assert' check
    DWORD           dwRetVal;     // return value from the message box
    BOOL            fShutdown;    // Has debugging shutdown?
    BOOL            fOutOfProc;   // Whether the dialog should be shown out of proc
    PTHREADCALLBACK pfnCallback;  // user callback
    INT_PTR         dwCallback;   // callback parameter
    HRESULT         hrCallback;   // callback return code
    DWORD_PTR       dwStackAddr[100];
    UINT            uicStackAddr;
} _DebugData;


//=--------------------------------------------------------------------------=
// Private functions
//=--------------------------------------------------------------------------=
static BOOL          _OutputFileString(LPCSTR lpszString, LPCSTR lpszFile);
static void          _OutputMsgBoxString(_In_z_ LPCSTR lpszString, bool fOutOfProc, _Out_opt_ BOOL *pfDoInt3, _Out_opt_ BOOL *pfThrow, _Out_opt_ BOOL *pfDisableAssert, _Out_opt_ BOOL *pfAlwaysThrow);
static int           _DisplayAssertDialog();
static BOOL CALLBACK _AssertDlgProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
static WNDPROC       _pfnButtonSubclassProc;
static DWORD WINAPI  _DebugThreadProc(PVOID);
static VOID          _CreateDebugThread();
static BOOL          _PeekMessage(MSG *pMsg, HWND hwnd, UINT uFirst, UINT uLast, UINT uFlags);
static int           _DoAssertDialog(LPCSTR pszText, LPCSTR pszTitle, UINT mbFlags);
static int           _DisplayOutOfProcDialogBox();
static BOOL          _CanMiniDump();
static BOOL          _WriteMiniDump(IN const char *szDrive, IN const char *szDir, BOOL fIncludeHeap, _Out_ OUT char *szMiniDumpFileName);
static int           _WriteMiniDumpExceptionHandler(EXCEPTION_POINTERS* pException, HANDLE hFile, BOOL fIncludeHeap, _Out_ BOOL* pfSuccess);

//=--------------------------------------------------------------------------=
// VsDoThreadCallback
//=--------------------------------------------------------------------------=
// This is called by other internal stuff in vsassert that may want to
// perform some function on another thread.  Why would anyone every want to
// do this?  The primary reason is so things like callstack dumping can
// be done on a thread other than the one that's being dumped.
//
HRESULT VsDoThreadCallback
(
 FARPROC pfnCallback,
 INT_PTR dwData
 )
{
    ////
    // Has someone terminated the debug thread behind our back?  The OS will do this
    // in abnormal termination situations
    //
    if (_DebugData.hThread && WAIT_OBJECT_0 == WaitForSingleObject(_DebugData.hThread, 0))
    {
        CloseHandle(_DebugData.hThread);
        _DebugData.hThread = NULL;
    }

    if (_DebugData.hThread)
    {
        _DebugData.pfnCallback = (PTHREADCALLBACK)pfnCallback;
        _DebugData.dwCallback = dwData;
        _DebugData.nAction = ACTION_CALLBACK;
        SetEvent(_DebugData.hevtAction);
        WaitForSingleObject(_DebugData.hevtResume, INFINITE);
        return _DebugData.hrCallback;
    }
    else
    {
        return((PTHREADCALLBACK)pfnCallback)(dwData);
    }
}


//=--------------------------------------------------------------------------=
// VsInitDebugThread
//=--------------------------------------------------------------------------=
// Inits the debugging thread for action.
//
void VsInitDebugThread()
{
    ////
    // Create the debug handing thread and event.  Without the thread,
    // stack dumps from the assert dialog won't be accurate for the
    // asserting thread.  However, this is a small price to pay, so
    // creating the additional thread is optional.
    //
    memset(&_DebugData, 0, sizeof(_DebugData));
    _CreateDebugThread();
}


//=--------------------------------------------------------------------------=
// VsTerminateDebugThread
//=--------------------------------------------------------------------------=
// Terminates our debugging thread and frees resources.
//
void VsTerminateDebugThread()
{
    if (_DebugData.hThread)
    {
        _DebugData.nAction = ACTION_TERMINATE;
        SetEvent(_DebugData.hevtAction);

        ////
        // Now get rid of our thread.  It is possible that we
        // are being called within DllMain, in which case the
        // WaitForSingleObject call will deadlock (due to the OS
        // trying to call DLL_THREAD_ATTACH).  The thread proc
        // will set _DebugData.hThread to NULL when it goes away
        // so we loop here until this happens.  Why?  Because we
        // may have an assert up during shutdown that we really
        // want to see.
        //
        while (_DebugData.hThread && WAIT_TIMEOUT == WaitForSingleObject(_DebugData.hThread, 250));
    }

    if (_DebugData.hevtAction) CloseHandle(_DebugData.hevtAction);
    if (_DebugData.hevtResume) CloseHandle(_DebugData.hevtResume);

    memset(&_DebugData, 0, sizeof(_DebugData));
    _DebugData.fShutdown = TRUE;
    g_dwDebugThreadId = 0;
}


//=--------------------------------------------------------------------------=
// VsSetImmediateCallback
//=--------------------------------------------------------------------------=
// Sets up a callback between the host app and the debugging output routines.
// This is used so that things like VsDebugPrintf can get routed to the
// VB or VJ immediate window.
//
CLINKAGE BOOL ENTRYPOINT VsSetImmediateCallback
(
 IMMEDIATECALLBACK pImmediateCallback
 )
{
    _pfnHostDebugWindow = pImmediateCallback;
    return TRUE;
}


//=--------------------------------------------------------------------------=
// VsEnableAsserts
//=--------------------------------------------------------------------------=
// Enables/Disables the display of all asserts
//
CLINKAGE void ENTRYPOINT VsEnableAsserts
(
 BOOL fEnable
 )
{
    _fDisableAllAsserts = !fEnable;
}

// If we have an assert on shutdown, usually due to a memory leak reporting, we
// can crash due to lingering windows. If any of the windows have WndProcs in
// already-unloaded DLLs, the act of trying to show our leak dialog may end up
// sending messages to those windows. In some cases, these lingering windows
// have managed WndProcs and re-entering the CLR at this late stage is fatal.
// This class is used to discover, report and destroy these lingering windows.
class CLingeringWindows
{
public:
    CLingeringWindows();
    ~CLingeringWindows();

    int Enumerate(); // Returns the number of lingering windows. -1 on error
    
    void DestroyAndReport(); // Destroys the discovered lingering windows and
                     // reports them in an assert message.

private:
    static BOOL CALLBACK EnumWindowsProc(HWND hWnd, LPARAM lParam );
    BOOL EnumWindowsProc( HWND hWnd );

    // Use a simple linked list to record the class names of the destroyed
    // windows.
    struct WindowClassNameListNode
    {
        HWND m_hWnd;
        char m_achClassName[64];
        WindowClassNameListNode* m_pNext;
    };

    WindowClassNameListNode* m_pList; // Head of the linked list
    int m_nCount; // Number of nodes
};

CLingeringWindows::CLingeringWindows() : m_pList(NULL), m_nCount(0)
{
}

CLingeringWindows::~CLingeringWindows()
{
    // Free the nodes in the linked list
    while( m_pList != NULL )
    {
        WindowClassNameListNode* pNext = m_pList->m_pNext;
        DebugFree( m_pList );
        m_pList = pNext;
    }
}

int CLingeringWindows::Enumerate()
{
    m_nCount = 0;

    // Enumerate all windows to discover any that belong to this process.
    // An alternative technique might be to use EnumThreadWindows on the
    // current thread, but that might miss windows created on background
    // threads.
    if( !::EnumWindows( CLingeringWindows::EnumWindowsProc, reinterpret_cast< LPARAM >( this ) ) )
    {
        // Error enumerating
        return -1;
    }

    return m_nCount;
}

// EnumWindows call-back
BOOL CALLBACK CLingeringWindows::EnumWindowsProc(
    HWND hWnd,
    LPARAM lParam
    )
{
    // The parameter is the 'this' pointer.
    CLingeringWindows* pThis = reinterpret_cast< CLingeringWindows* >( lParam );
    return pThis->EnumWindowsProc( hWnd );
}

BOOL CLingeringWindows::EnumWindowsProc( HWND hWnd )
{
    // Select only windows beloging to this process.
    DWORD dwProcessId = 0;
    (void)::GetWindowThreadProcessId( hWnd, &dwProcessId );
    if( dwProcessId != ::GetCurrentProcessId() )
    {
        return TRUE; // keep going
    }

    // Found a window belonging to this process

    // Allocate a node for it
    WindowClassNameListNode* pNode = reinterpret_cast< WindowClassNameListNode* >( DebugAlloc( sizeof(WindowClassNameListNode) ) );
    if( !pNode )
    {
        // Allocation error. Cannot complete the enumeration
        return FALSE;
    }

    pNode->m_hWnd = hWnd;

    // Note: We cannot get the caption for this window because that would cause
    // WM_GETTEXT to be sent. We can, however get the window class name which
    // may be useful for debugging.
    if( !GetClassNameA( hWnd, pNode->m_achClassName, _countof(pNode->m_achClassName) ) )
    {
        strcpy_s( pNode->m_achClassName, "<unknown>" );
    }

    // Add the new node to the head of the list
    pNode->m_pNext = m_pList;
    m_pList = pNode;

    // Increment the count of discovered windows
    m_nCount++;

    return TRUE; // keep going
}

void CLingeringWindows::DestroyAndReport()
{
    if( m_nCount <= 0 )
    {
        return;
    }

    // Destroy the windows. As we go through the loop, also calculate the
    // the length of the message string we need for the class names.
    size_t cchLenClassNames = 0;
    
    // Iterate through the linked list
    for( WindowClassNameListNode* pNode = m_pList; pNode != NULL; pNode = pNode->m_pNext )
    {
        HWND hWnd = pNode->m_hWnd;

        // Don't call DestroyWindow directly, because that will send WM_DESTROY
        // to the WndProc.
        // First, force the WndProc to be DefWindowProc.
        ::SetWindowLongPtr( hWnd, GWLP_WNDPROC, reinterpret_cast< LONG_PTR >( DefWindowProc ) );
        ::DestroyWindow( hWnd );

        cchLenClassNames += strlen( pNode->m_achClassName );
    }

    // The message string will be:
    // VSASSERT destroyed the following n lingering windows in order to bring you the next assert.
    // hwnd=xxxxxxxx Class=nnnnnnn\r\n
    // hwnd=xxxxxxxx Class=nnnnnnn\r\n
    // etc.

    const char* szHwndLineFormat = "hwnd=%p Class=%s\r\n";

    // Compute the length of the fixed part for each line 
    const char szHwndLineTemplate[] = "hwnd= Class=\r\n";

    // %p is hexadecimal format for a pointer, so it needs 2 chars for each byte. (8 on 32bit systems, 16 on 64bit systems)
    // The -1 is because szHwndLineTemplate includes a terminating NUL that we don't actually need for the computation
    const size_t cchLineTemplateLength = _countof(szHwndLineTemplate) - 1 + 2 * sizeof(void*);

    cchLenClassNames += cchLineTemplateLength * m_nCount + 1; // +1 for terminator
    LPSTR szClassNames = reinterpret_cast< LPSTR >( DebugAlloc( cchLenClassNames ) );
    if( !szClassNames )
    {
        // Alloc failed. Just report a generic error
        BOOL bDummy = FALSE;
        VsDisplayDebugMessage(
            "VSASSERT destroyed %d lingering windows in order to bring you the following assert.", "Lingering Windows",
            __FILE__, __LINE__,
            g_dfAssertFlags,
            &bDummy,
            m_nCount );
        return;
    }

    LPSTR szPos = szClassNames;
    for( WindowClassNameListNode* pNode = m_pList; pNode != NULL; pNode = pNode->m_pNext )
    {
        int cch = sprintf_s( szPos, cchLenClassNames, szHwndLineFormat, pNode->m_hWnd, pNode->m_achClassName );
        if( cch != -1 )
        {
            cchLenClassNames -= cch;
            szPos += cch;
        }
    }

    BOOL bDummy = FALSE;
    VsDisplayDebugMessage(
        "VSASSERT destroyed the following %d lingering windows in order to bring you the next assert.\r\n%s", "Lingering Windows",
        __FILE__, __LINE__,
        g_dfAssertFlags,
        &bDummy,
        m_nCount,
        szClassNames );

    DebugFree(szClassNames);
}

//=--------------------------------------------------------------------------=
// VsManagedAssert
//=--------------------------------------------------------------------------=
// Simple assert call from managed code, called directly from our trace listener
//
CLINKAGE BOOL ENTRYPOINT VsManagedAssert
(
 LPCSTR  pszMsg,         // the message to display
 LPCSTR  pszFileName,    // the file the assert came from
 UINT    line,           // the line #
 BOOL*   pfDisableAssert // should we disable this assert
 )
{
  if(g_fStopOnVsAssert)
  {
      return TRUE;
  }
  return VsDisplayDebugMessage(pszMsg, "Assert from managed code", pszFileName ? pszFileName : "", line, 
      g_dfAssertFlags, pfDisableAssert);
}

//=--------------------------------------------------------------------------=
// VsAssert
//=--------------------------------------------------------------------------=
// Simple assert call, called directly from our assert macro.
//
// Note:  The textual name of this function is referenced in
//        VsDump.cpp in DumpThisThread.  If you change the
//        name of this call you should update debdump.cpp as well.
//
CLINKAGE BOOL ENTRYPOINT VsAssert
(
 LPCSTR  pszMsg,         // the message to display
 LPCSTR  pszAssert,      // the assertion comarison text
 LPCSTR  pszFile,        // the file the assert came from
 UINT    line,           // the line #
 BOOL   *pfDisableAssert // should we disable this assert
 )
{
    if( g_fShuttingDown && !(g_dfAssertFlags & DF_OUTOFPROCDIALOG) )
    {
        // If we are shutting down we need to see if there are any HWNDs hanging
        // around because, if there are, we can hit a race condition that can cause
        // a crash while displaying the assert dialog.
        CLingeringWindows linger;
        if( linger.Enumerate() > 0 )
        {
            linger.DestroyAndReport();
        }
    }
        
    return VsDisplayDebugMessage(pszMsg, pszAssert, pszFile, line, g_dfAssertFlags,
        pfDisableAssert);
}

//=--------------------------------------------------------------------------=
// VsAssertAndThrow
//=--------------------------------------------------------------------------=
// Simple assert call, called directly from our assert macro.
CLINKAGE BOOL ENTRYPOINT VsAssertAndThrow
(
 LPCSTR  pszMsg,         // the message to display
 LPCSTR  pszAssert,      // the assertion comarison text
 LPCSTR  pszFile,        // the file the assert came from
 UINT    line,           // the line #
 BOOL *pfThrow,                      // set to true if the user asked to throw. May be NULL
 BOOL *pfDisableAssert,      // set to true if the user disabled this assert. May be NULL
 BOOL *pfAlwaysThrow         // set to true if the user always-throwed this assert. May be NULL
 )
{
    return VsDisplayDebugMessageThrow(pszMsg, pszAssert, pszFile, line, g_dfAssertFlags,
        pfThrow, pfDisableAssert, pfAlwaysThrow);
}

BOOL ENTRYPOINT VsAssert
(
 LPCWSTR pszMsg, 
 LPCSTR  pszAssert, 
 LPCSTR  pszFile, 
 UINT    line, 
 BOOL   *pfDisableAssert
 )
{
    return VsAssertAndThrow(pszMsg, pszAssert, pszFile, line, NULL, pfDisableAssert, NULL);
}

BOOL ENTRYPOINT VsAssertAndThrow
(
 LPCWSTR pszMsg, 
 LPCSTR  pszAssert, 
 LPCSTR  pszFile, 
 UINT    line, 
 BOOL    *pfThrow,
 BOOL    *pfDisableAssert,
 BOOL    *pfAlwaysThrow
 )
{
    char sz[512];
    sz[0] = 0;

    const DWORD dwErr = GetLastError();

    if (pszMsg && *pszMsg)
    {
        WideCharToMultiByte(CP_ACP, 0, pszMsg, -1, sz, sizeof(sz), NULL, NULL);
        sz[sizeof(sz)-1] = 0;
    }
    const BOOL f = VsDisplayDebugMessageThrow(sz, pszAssert, pszFile, line, g_dfAssertFlags,
        pfThrow, pfDisableAssert, pfAlwaysThrow);
    SetLastError(dwErr);
    return f;
}

//=--------------------------------------------------------------------------=
// VsDebugPrint
//=--------------------------------------------------------------------------=
// Simple statement that outputs to the location specified
// in the g_dfPrintf flags without formatting.
//
CLINKAGE void ENTRYPOINT VsDebugPrint
(
 LPCSTR  pszMsg
 )
{
    // VsDebugOutput will restore the last error, so no
    // need to do it here.
    VsDebugOutput(g_dfPrintfFlags, pszMsg, NULL, NULL);
}

//=--------------------------------------------------------------------------=
// VsDebugPrintf
//=--------------------------------------------------------------------------=
// Simple Printf style statement that outputs to the location specified
// in the g_dfPrintf flags.
//
CLINKAGE void ENTRYPOINT VsDebugPrintf
(
 LPCSTR  pszMsg,
 ...
 )
{
    static TCHAR  szMsg[0x4000];
    va_list       arglist;
    BOOL          fArgs = FALSE;

    DWORD dwErr = GetLastError();

    // Try to determine if pszMsg has formatting stuff that
    // will require wvsprintf
    //
    for (LPSTR pszCh = (LPSTR)pszMsg; *pszCh; pszCh = CharNext(pszCh))
        if (*pszCh == '%')
        {
            pszCh = CharNext(pszCh);
            if (*pszCh != '%')
            {
                fArgs = TRUE;
                break;
            }
        }

        if (fArgs)
        {
            va_start(arglist, pszMsg);
            _vsnprintf(szMsg, _countof(szMsg), pszMsg, arglist );
            szMsg[_countof(szMsg) - 1] = 0;
            pszMsg = szMsg;
            va_end(arglist);
        }

        VsDebugOutput(g_dfPrintfFlags, pszMsg, NULL, NULL);

        SetLastError(dwErr);
}


//=--------------------------------------------------------------------------=
// VsDebugPrintIf
//=--------------------------------------------------------------------------=
// Simple Printf style statement that outputs to the location specified
// in the g_dfPrintf flags.  This is identical to VsDebugPrintf except that
// it takes an extra BOOL value.  If TRUE, the text will be printed.
//
// Why have this?  Mostly because I'm too lazy to change the 340 calls in
// the ruby tree to use "if(f) DEBUGPRINTF".
//
//
CLINKAGE void ENTRYPOINT VsDebugPrintIf
(
 BOOL    fPrint,
 LPCSTR  pszMsg,
 ...
 )
{
    static TCHAR  szMsg[1024];
    va_list       arglist;
    BOOL          fArgs = FALSE;

    if (!fPrint) return;

    DWORD dwErr = GetLastError();

    // Try to determine if pszMsg has formatting stuff that
    // will require wvsprintf
    //
    for (LPSTR pszCh = (LPSTR)pszMsg; *pszCh; pszCh = CharNext(pszCh))
        if (*pszCh == '%')
        {
            pszCh = CharNext(pszCh);
            if (*pszCh != '%')
            {
                fArgs = TRUE;
                break;
            }
        }

        if (fArgs)
        {
            va_start(arglist, pszMsg);
            _vsnprintf(szMsg, _countof(szMsg), pszMsg, arglist );
            szMsg[_countof(szMsg) - 1] = 0;
            pszMsg = szMsg;
            va_end(arglist);
        }

        VsDebugOutput(g_dfPrintfFlags, pszMsg, NULL, NULL);

        SetLastError(dwErr);
}

//=--------------------------------------------------------------------------=
// VsDisplayDebugMessage
//=--------------------------------------------------------------------------=
// Thin wrapper around VsDisplayDebugMessageThrowVa
CLINKAGE BOOL ENTRYPOINT VsDisplayDebugMessage
(
 LPCSTR  pszMsg,           // message to display
 LPCSTR  pszAssert,        // assert text
 LPCSTR  pszFile,          // file assert occurred in, may be NULL
 UINT    line,             // line number of assert, ignored if NULL pszFile
 int     flags,            // flags to send to VsDebugOutput.  These determine where the message goes
 BOOL   *pfDisableAssert,  // should we disable this assert
 ...                   // additional format parameters for pszMsg
 )
{
    va_list valArgList;
    va_start(valArgList, pfDisableAssert);
    BOOL fReturn=VsDisplayDebugMessageThrowVa(  pszMsg,
        pszAssert,
        pszFile,
        line,
        flags,
        NULL,
        pfDisableAssert,
        NULL,
        valArgList);
    va_end(valArgList);

    return fReturn;

}

//=--------------------------------------------------------------------------=
// VsDisplayDebugMessageThrow
//=--------------------------------------------------------------------------=
// This displays a standard debugging message.  It returns TRUE if the user
// has requested an Int3.  By putting the Int3 in the actual assert macro,
// VC will land on the correct line in the source base.
//
CLINKAGE BOOL ENTRYPOINT VsDisplayDebugMessageThrow
(
 LPCSTR  pszMsg,           // message to display
 LPCSTR  pszAssert,        // assert text
 LPCSTR  pszFile,          // file assert occurred in, may be NULL
 UINT    line,             // line number of assert, ignored if NULL pszFile
 int     flags,            // flags to send to VsDebugOutput.  These determine where the message goes
 BOOL *pfThrow,                      // set to true if the user asked to throw. May be NULL
 BOOL *pfDisableAssert,      // set to true if the user disabled this assert. May be NULL
 BOOL *pfAlwaysThrow,            // set to true if the user always-throwed this assert. May be NULL
 ...                   // additional format parameters for pszMsg
 )
{
    va_list valArgList;
    va_start(valArgList, pfAlwaysThrow);
    BOOL fReturn=VsDisplayDebugMessageThrowVa(  pszMsg,
        pszAssert,
        pszFile,
        line,
        flags,
        pfThrow,
        pfDisableAssert,
        pfAlwaysThrow,
        valArgList);
    va_end(valArgList);

    return fReturn;
}



//=--------------------------------------------------------------------------=
// VsDisplayDebugMessageThrow
//=--------------------------------------------------------------------------=
// This displays a standard debugging message.  It returns TRUE if the user
// has requested an Int3.  By putting the Int3 in the actual assert macro,
// VC will land on the correct line in the source base.
//
CLINKAGE BOOL ENTRYPOINT VsDisplayDebugMessageThrowVa
(
 LPCSTR  pszMsg,           // message to display
 LPCSTR  pszAssert,        // assert text
 LPCSTR  pszFile,          // file assert occurred in, may be NULL
 UINT    line,             // line number of assert, ignored if NULL pszFile
 int     flags,                            // flags to send to VsDebugOutput.  These determine where the message goes
 BOOL *pfThrow,                      // set to true if the user asked to throw. May be NULL
 BOOL *pfDisableAssert,      // set to true if the user disabled this assert. May be NULL
 BOOL *pfAlwaysThrow,            // set to true if the user always-throwed this assert. May be NULL
 va_list valArgList
 )
{
    static  TCHAR szMsg1[1024]; //  Just in case we have no room to alloc
    static  TCHAR szMsg2[2048]; //
    LPSTR   pszBuf = NULL;
    LPSTR   lpszText;
    BOOL    fDoInt3 = FALSE;
    BOOL    fArgs = FALSE;

    if (!g_fEnableAsserts)
    {
        return FALSE;
    }
    
    if ((flags & DF_ENABLED) == 0)
    {
        return FALSE;
    }



    DWORD dwErr = GetLastError();

    ////
    // Grab our debug mutex to ensure that we only get one assert at a time.
    // The order of grabbing / freezing looks like this:
    //
    //    Grab Mutex
    //    Freeze Threads
    //    Release Mutex
    //    Thaw Threads
    //
    // This prevents deadlock situations where we're waiting on a mutex that's
    // owned by a thread we've frozen.  It also means that we can't use scope
    // to automatically lock/unlock mutexes.  So be careful.
    //
    _csDebugLock.Request();

    ////
    // Freeze all threads except this thread and the debugging thread.
    // The threads will automatically resume when this macro goes out of
    // scope.
    //
    FreezeAllThreads();

    lpszText = (LPSTR)pszMsg;

    ////
    // If we had file/line info, use it
    //
    if (pszFile)
    {
        UINT cb = sizeof(TCHAR) * (lstrlen(_szFormat) + // format
            8                  + // max tid length
            lstrlen(pszAssert) + // max assert length
            8                  + // max line length
            lstrlen(pszFile)   + // file length
            (lstrlen(pszMsg ? pszMsg : pszAssert)));

        lpszText = pszBuf = (LPSTR)DebugAlloc(cb);
        if (!lpszText)
        {
            lpszText = szMsg1;
            cb = sizeof(szMsg1);
        }

        _snprintf(lpszText, cb, _szFormat, GetCurrentProcessId(), GetCurrentProcessId(), GetCurrentThreadId(), pszAssert, pszFile, line, pszMsg ? pszMsg : pszAssert);
    }

    // Try to determine if lpszText has formatting stuff that
    // will require wvsprintf
    //
    for (LPSTR pszCh = lpszText; *pszCh; pszCh = CharNext(pszCh))
        if (*pszCh == '%')
        {
            pszCh = CharNext(pszCh);
            if (*pszCh != '%')
            {
                fArgs = TRUE;
                break;
            }
        }

        if (fArgs)
        {
            _vsnprintf(szMsg2, _countof(szMsg2), lpszText, valArgList );
            szMsg2[_countof(szMsg2) - 1] = 0;
            lpszText = szMsg2;
        }

        VsDebugOutputThrow(flags, lpszText, &fDoInt3, pfThrow, pfDisableAssert, pfAlwaysThrow);

        if (pszBuf) DebugFree(pszBuf);

        ////
        // Now release the mutex.
        //
        _csDebugLock.Release();

        // FreezeAllThreads releases at end of scope

        SetLastError(dwErr);

        return fDoInt3;
}

//=--------------------------------------------------------------------------=
// VsDebugOutput
//=--------------------------------------------------------------------------=
// A thin wrapper around VsDebugOutputThrow
//
CLINKAGE VOID ENTRYPOINT VsDebugOutput
(
 int     dfOutput,     // where to send the string
 LPCSTR  pszOutputString,  // the debug string
 BOOL    *pfDoInt3,        // should caller fire Int3?
 BOOL        *pfDisableAssert        // set to true if the user disabled this assert. May be NULL
 )
{
    VsDebugOutputThrow( dfOutput, 
        pszOutputString,
        pfDoInt3,
        NULL,
        pfDisableAssert,
        NULL);
}


//=--------------------------------------------------------------------------=
// VsDebugOutputThrow
//=--------------------------------------------------------------------------=
// Outputs the given string to a place dictated by the value of dfOutput
//
CLINKAGE VOID ENTRYPOINT VsDebugOutputThrow
(
 int     dfOutput,     // where to send the string
 LPCSTR  pszOutputString,  // the debug string
 BOOL    *pfDoInt3,        // should caller fire Int3?
 BOOL    *pfThrow,                       // set to true if the user asked to throw. May be NULL
 BOOL    *pfDisableAssert,       // set to true if the user disabled this assert. May be NULL
 BOOL    *pfAlwaysThrow          // set to true if the user always-throwed this assert. May be NULL
 )
{
    DWORD dwErr = GetLastError();

    ////
    // Mask the requested output flags with those that are valid
    dfOutput &= g_dfEnabled;

    InterlockedIncrement((long *)&_nRecurseCnt);

    LOCK_MUTEX(&_csDebugLock); // So strings don't go to logs all harry-scary

    if (MAX_RECURSE < _nRecurseCnt)
    {
        ////
        // We may be recursing infinitely.  This is possible if someone caused
        // an assert in something that's being called by the debug code.
        //
        if (IDYES == MessageBox(NULL,
            "Detected infinite recursion in ASSERT.  Enter debugger?",
            "Infinite Recursion Detected",
            MB_ICONSTOP | MB_YESNO))
            Int3;

        ////
        // Don't allow the recursion to continue
        //
        InterlockedDecrement((long *)&_nRecurseCnt);
        return;
    }

    // Output the string to the Debugger window.
    if ( dfOutput & DF_OUTPUTDEBUGSTRING )
    {
        // Try to make up for various STUPID debuggers that can't handle more than 512 or 1024 characters at a time
        // by outputting in 512 byte chunks.
        //
        DWORD cbDebugString = lstrlen( pszOutputString );
        DWORD cbOffset = 0;
        while ( cbOffset < cbDebugString )
        {
            char szChunk[ 513 ];
            DWORD cbChunk = (DWORD)min( cbDebugString - cbOffset, sizeof( szChunk ) - 1 );
            memcpy( szChunk, pszOutputString + cbOffset, cbChunk );
            szChunk[ cbChunk ] = '\0';
            OutputDebugString( szChunk );

            cbOffset += cbChunk;
        }
    }

    if (dfOutput & DF_OUTPUTDIALOG)
    {
        bool fOutOfProc = (dfOutput & DF_OUTOFPROCDIALOG) != 0;
        // Output to a message box where we can do an int 3, etc.
        _OutputMsgBoxString(pszOutputString, fOutOfProc, pfDoInt3, pfThrow, pfDisableAssert, pfAlwaysThrow);
    }

    // Output to the host's debug/immediate window
    if ((dfOutput & DF_OUTPUTDEBUGWINDOW) && _pfnHostDebugWindow)
        _pfnHostDebugWindow((LPSTR)pszOutputString);

    InterlockedDecrement((long *)&_nRecurseCnt);

    SetLastError(dwErr);
}


//=--------------------------------------------------------------------------=
// VsEnsureDebuggerPresent
//=--------------------------------------------------------------------------=
// Ensures that a debugger is present.  If one isn't present, this will attempt
// to attach one.  If no debugger is installed on the machine, this will
// display a message and return FALSE.
//
CLINKAGE BOOL ENTRYPOINT VsEnsureDebuggerPresent()
{
    // If we've already done the work to start the debugger, we're fine
    //
    if (_fStartedDebugger)
    {
        return TRUE;
    }

    // First, if we don't have an IsDebuggerPresent API, look for one.
    //
    if (!_IsDebuggerPresent)
    {
        HMODULE hKernel = NULL;

        hKernel = GetModuleHandle ("Kernel32.dll");

        if (!hKernel) 
        {
            MessageBox(NULL, "Unable to attach to debugger because we could not find Kernel32.dll", "VsAssert", MB_OK | MB_ICONSTOP);
            return FALSE;
        }

        _IsDebuggerPresent = (ISDEBUGGERPRESENT)GetProcAddress (hKernel, "IsDebuggerPresent");

        if (!_IsDebuggerPresent)
        {
            MessageBox(NULL, "Unable to attach to debugger because we could not find a suitable IsDebuggerPresent API", "VsAssert", MB_OK | MB_ICONSTOP);
            return FALSE;
        }
    }

    // Now find out if the debugger is indeed present.
    //
    if (_IsDebuggerPresent())
    {
        _fStartedDebugger = TRUE;
    }
    else
    {
        // The debugger has not been started yet.  Do this here.
        //
        BOOL fJIT = FALSE;

        //
        //  Magic!  Location of the JIT debugger info...
        //
        TCHAR *szRegKey = TEXT("Software\\Microsoft\\Windows NT\\CurrentVersion\\AeDebug");
        LONG lRetVal;
        HKEY hKey;

        lRetVal = RegOpenKeyEx(
            HKEY_LOCAL_MACHINE,
            szRegKey,
            0,
            KEY_READ,
            &hKey);

        TCHAR szCommandLine[MAX_PATH];
        DWORD dwSize = MAX_PATH;
        if (lRetVal == ERROR_SUCCESS)
        {
            DWORD dwType;

            lRetVal = RegQueryValueEx(
                hKey,
                TEXT("Debugger"),
                0,
                &dwType,
                (BYTE *)szCommandLine,
                &dwSize);
            RegCloseKey(hKey);

            if (lRetVal == ERROR_SUCCESS)
            {
                fJIT = TRUE;

            }
        }
        else
        {
            //
            //  Try WIN.INI
            GetProfileString("AeDebug", "Debugger", "", szCommandLine,
                dwSize);

            if (strlen(szCommandLine) != 0)
            {
                fJIT = TRUE;
            }
        }

        if (!fJIT)
        {
            MessageBox(NULL, "Unable to attach to debugger because we could not find a debugger on this machine.", "VsAssert", MB_OK | MB_ICONSTOP);
            return FALSE;
        }

        HANDLE hEvent;

        //
        //  Now that we have the JIT debugger, try to start it.
        //  The JIT needs a process ID (ours), and an event.
        //
        SECURITY_ATTRIBUTES sa;
        memset(&sa, 0 , sizeof(sa));
        sa.nLength = sizeof(sa);
        sa.bInheritHandle = TRUE;

        hEvent = CreateEvent(&sa, TRUE, FALSE, NULL);

        if (hEvent != NULL)
        {
            TCHAR szCommand[2 * MAX_PATH];
            BOOL fResult;
            PROCESS_INFORMATION pi;

            sprintf_s(szCommand, _countof ( szCommand ), szCommandLine, GetCurrentProcessId(), hEvent);

            try
            {
                STARTUPINFO si;

                memset(&si, 0, sizeof(STARTUPINFO));
                si.cb = sizeof(STARTUPINFO);

                fResult = CreateProcess(
                    NULL,
                    szCommand,
                    NULL,
                    NULL,
                    TRUE,
                    0,
                    NULL,
                    NULL,
                    &si,
                    &pi);
            }
            catch (...)
            {
                fResult = FALSE;
            }

            if (fResult)
            {
                CloseHandle(pi.hProcess);
                CloseHandle(pi.hThread);

                _fStartedDebugger = TRUE;

                WaitForSingleObject(hEvent, INFINITE);
                CloseHandle(hEvent);
            }
            else 
            {
                TCHAR szPoof[2 * MAX_PATH + 100];
                sprintf_s(szPoof, _countof ( szPoof ), "Unable to invoke the debugger.  The invocation command we used was:\r\n\r\n%s", szCommand);
                MessageBox(NULL, szPoof, "VsAssert", MB_OK | MB_ICONSTOP);
                return FALSE;
            }

        }
    }

    return TRUE;
}

//=--------------------------------------------------------------------------=
// _CanMiniDump
//=--------------------------------------------------------------------------=
// Do we have the ability to create a minidump
//
BOOL _CanMiniDump()
{
    HMODULE hLib = LoadLibrary("dbghelp.dll");
    if (hLib != NULL)
    {
        _gpfnMiniDumpWriteDump = (MDPROC*)GetProcAddress(hLib,"MiniDumpWriteDump");
        if (_gpfnMiniDumpWriteDump)
            return TRUE;
    }
    return FALSE;
}



//=--------------------------------------------------------------------------=
// _WriteMiniDump
//=--------------------------------------------------------------------------=
// Write a minidump and return the file that we wrote the minidump to. The minidump
// filename must be size >= _MAX_PATH
//
BOOL 
_WriteMiniDump(IN const char *szDrive, IN const char *szDir, BOOL fIncludeHeap, _Out_ OUT char *szMiniDumpFileName)
{
    if (!_CanMiniDump()) {
        return FALSE;
    }

    // Get the path to the module which started this process
    const DWORD dwModNameLen = GetModuleFileName(NULL, szMiniDumpFileName, _MAX_PATH);

    if ( !dwModNameLen ) {
        // The API failed
        return FALSE;
    }

    if (szMiniDumpFileName[dwModNameLen] != 0) {
        // The modname len is wrong
        return FALSE;
    }

    // If a drive and directory were passed in, put the minidump there
    if (szDrive && szDir && szDrive[0] && szDir[0]) {

        char szFname[_MAX_FNAME];
        char szExt[_MAX_EXT];
        _splitpath(szMiniDumpFileName, NULL, NULL, szFname, szExt);

        _makepath_s(szMiniDumpFileName, _MAX_PATH, szDrive, szDir, szFname, szExt);
    }


    // The modname has to be small enough to fit in the buffer with a four digit hex code and
    // a dash, and a '.dmp' after it
    const DWORD dwMaxModName = _MAX_PATH - sizeof(".dmp") - 5;

    // if the function failed, or the length is too long, return false. 
    const DWORD dwDumpNameBaseLen = (DWORD)strlen(szMiniDumpFileName);
    if (dwDumpNameBaseLen > dwMaxModName) {
        return FALSE;
    }



    // Try to find a unique filename to write this dump to
    for (DWORD dwTry = 0; dwTry < 0x10001; dwTry++)
    {
        if (dwTry != 0) {
            // Except on the first try, include the try number in the name
            szMiniDumpFileName[dwDumpNameBaseLen] = '-';
            _itoa(dwTry - 1, &szMiniDumpFileName[dwDumpNameBaseLen+1], 16);
        }

        strcat(szMiniDumpFileName, ".dmp");

        if (GetFileAttributes(szMiniDumpFileName) == 0xffffffff && GetLastError() == ERROR_FILE_NOT_FOUND)
        {   
            HANDLE hFile = CreateFile(szMiniDumpFileName,
                GENERIC_WRITE,
                0,
                NULL,
                CREATE_ALWAYS,
                FILE_ATTRIBUTE_NORMAL,
                NULL
                );

            if (hFile == INVALID_HANDLE_VALUE) 
            {
                if (GetLastError() == ERROR_ACCESS_DENIED && 
                    (szDrive == NULL || szDir == NULL))
                {
                    // If we cannot write to the directory of the exe, we fall back to saving the ouput into %tmp%

                    if (!GetTempPath(_MAX_PATH, szMiniDumpFileName))
                    {
                        return FALSE;
                    }
                    
                    char szMiniDumpDrive[_MAX_DRIVE] = "";
                    char szMiniDumpDir[_MAX_DIR] = "";
                    _splitpath(szMiniDumpFileName, szMiniDumpDrive, szMiniDumpDir, NULL, NULL);
                    
                    return _WriteMiniDump(szMiniDumpDrive, szMiniDumpDir, fIncludeHeap, szMiniDumpFileName);
                }
                else
                {
                    return FALSE;
                }
            }

            BOOL ret = FALSE;
            __try
            {
                // Raise an exception to get the current thread context
                RaiseException(
                    STATUS_ASSERTION_FAILURE,  // exception code
                    EXCEPTION_NONCONTINUABLE,  // exception cannot be continued
                    0,                         // argument count
                    NULL                       // argument array
                    );
            }
            __except (_WriteMiniDumpExceptionHandler(GetExceptionInformation(), hFile, fIncludeHeap, &ret))
            {
                // minidump has been written
            }

            CloseHandle(hFile);

            if (!ret) {
                return FALSE;
            }

            return TRUE;    
        }
    }

    // all possible file names are already taken
    return FALSE;
}


//=--------------------------------------------------------------------------=
// _WriteMiniDumpExceptionHandler
//=--------------------------------------------------------------------------=
// Helper routine for _WriteMiniDump which is called from within the exception
// filter to actually save the dump. This is required because otherwise the
// callstack of this thread will be taken from within the minidump code and may
// not be readable.
//
int _WriteMiniDumpExceptionHandler(EXCEPTION_POINTERS* pException, HANDLE hFile, BOOL fIncludeHeap, _Out_ BOOL* pfSuccess)
{
    DWORD DumpType = fIncludeHeap ? (MiniDumpWithFullMemory | MiniDumpWithDataSegs) : MiniDumpNormal;

    MINIDUMP_EXCEPTION_INFORMATION exception = {0};
    exception.ThreadId = GetCurrentThreadId();
    exception.ExceptionPointers = pException;
    exception.ClientPointers = FALSE;

    const HANDLE hProcess = GetCurrentProcess();
    const DWORD processId = GetCurrentProcessId();

    BOOL ret = (*_gpfnMiniDumpWriteDump)(
        hProcess,
        processId,
        hFile,
        (MINIDUMP_TYPE)DumpType,
        &exception,
        NULL,
        NULL
        );

    if (!ret && fIncludeHeap)
    {
        // If we fail with a full dump, try again with just the minidump. Taking a minidump of your own
        // process can fail because the valid memory pages is changing as the dump is being taken.

        ret = (*_gpfnMiniDumpWriteDump)(
            hProcess,
            processId,
            hFile,
            MiniDumpNormal,
            &exception,
            NULL,
            NULL
            );
    }

    *pfSuccess = ret;

    // Execute the handler, this will cause this process to be terminated.
    return EXCEPTION_EXECUTE_HANDLER;
}


//=--------------------------------------------------------------------------=
// VsAssertSaveMiniDump
//=--------------------------------------------------------------------------=
// Save a minidump of the current state of this process. The minidump will be 
// written next to the exe which started the process.
//
BOOL ENTRYPOINT VsAssertWriteMiniDump()
{
    char szMiniDumpPath[_MAX_PATH + 1] = "";
    return _WriteMiniDump(NULL, NULL, FALSE, szMiniDumpPath);
}

CLINKAGE BOOL ENTRYPOINT VsAssertWriteMiniDumpEx(HANDLE hProcess, DWORD processId, char *szMiniDumpFullFileName, DWORD DumpType )
{
    BOOL fRetval = FALSE;
    if (_CanMiniDump()) {
        HANDLE hFile = CreateFile(szMiniDumpFullFileName,
            GENERIC_WRITE,
            0,
            NULL,
            CREATE_ALWAYS,
            FILE_ATTRIBUTE_NORMAL,
            NULL
            );

        if (hFile != INVALID_HANDLE_VALUE) 
        {

            fRetval = (*_gpfnMiniDumpWriteDump)(
                    hProcess,
                    processId,
                    hFile,
                    (MINIDUMP_TYPE)DumpType,
                    NULL,    //PMINIDUMP_EXCEPTION_INFORMATION ExceptionParam,
                    NULL,    //PMINIDUMP_USER_STREAM_INFORMATION UserStreamParam,
                    NULL     //PMINIDUMP_CALLBACK_INFORMATION CallbackParam
                    );

            CloseHandle(hFile);
        }

    }
    return fRetval;
}




//=--------------------------------------------------------------------------=
// _OutputFileString
//=--------------------------------------------------------------------------=
// Writes the given string out to the debug file.  The debug file was
// determined when we initialized -- when we first read in the debug options
// from the INI file.  Returns TRUE if the write was successful.
//
BOOL _OutputFileString
(
 LPCSTR  lpszString, // the string to output
 LPCSTR  lpszFile    // the file to open
 )
{
    HANDLE hfDebOutFile;

    if (!lpszFile) 
        return FALSE;  // kind of a bonehead thing to do

     hfDebOutFile = CreateFile(lpszFile,
        GENERIC_WRITE,
        0,
        NULL,
        OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL,
        NULL
        );

    if (hfDebOutFile == INVALID_HANDLE_VALUE)
    {
        hfDebOutFile = CreateFile(lpszFile,
            GENERIC_WRITE,
            0,
            NULL,
            CREATE_ALWAYS,
            FILE_ATTRIBUTE_NORMAL,
            NULL
            );
    }
    if (hfDebOutFile == INVALID_HANDLE_VALUE)
    {
        OutputDebugString("Could not open the debug output file\n.");
        return FALSE;
    }
    if (SetFilePointer(hfDebOutFile, 0l, 0l, FILE_END) == INVALID_SET_FILE_POINTER)
    {
        OutputDebugString("SetFilePointer failed");
    }
    else
    {
        DWORD dwBytesWritten = 0;
        WriteFile(hfDebOutFile, lpszString, lstrlen(lpszString), &dwBytesWritten, NULL);
    }
    
    CloseHandle(hfDebOutFile);

    return TRUE;
}


//=--------------------------------------------------------------------------=
// _OutputMsgBoxString
//=--------------------------------------------------------------------------=
// Displays the given string in an assert message/dialog box.  This will check
// to see if the VSASSERT environment variable is set.  If it is, it will
// route the assert (along with stack dump information) to the file and
// terminate the process
//
void _OutputMsgBoxString
(
 _In_z_ LPCSTR lpszString,
 bool fOutOfProc,                 // true if the dialog should be displayed out-of-proc
 _Out_opt_ BOOL *pfDoInt3,
 _Out_opt_ BOOL *pfThrow,         // set to true if the user asked to throw. May be NULL
 _Out_opt_ BOOL *pfDisableAssert, // set to true if the user disabled this assert. May be NULL
 _Out_opt_ BOOL *pfAlwaysThrow    // set to true if the user always-throwed this assert. May be NULL
 )
{
    if (pfDoInt3) *pfDoInt3 = FALSE;
    if (pfThrow) *pfThrow = TRUE;
    if (pfDisableAssert) *pfDisableAssert = FALSE;
    if (pfAlwaysThrow) *pfAlwaysThrow = TRUE;

    if (pfThrow)
    {
        _DebugData.fAllowThrow=true;
    }

    {
        // Send the string to the Message box
        if (!_fDisableAllAsserts)
        {
#ifndef MB_SERVICE_NOTIFICATION
# define MB_SERVICE_NOTIFICATION 0x200000
#endif //MB_SERVICE_NOTIFICATION

            _DebugData.fOutOfProc = fOutOfProc ? TRUE : FALSE;
            int iDlgValue=_DoAssertDialog(lpszString,  _szTitle,
                MB_ICONHAND | MB_ABORTRETRYIGNORE | MB_SYSTEMMODAL | MB_SERVICE_NOTIFICATION);

            // process the checkboxes
            // user can disable all or some
            if (_DebugData.fDisableAll)
            {
                _fDisableAllAsserts = TRUE;

                // every assert that can will end up throwing. 
                if (_DebugData.fThrowAll)
                {
                    _fThrowAllAsserts = TRUE;
                }
            }

            // This assertion can also be set to throw itself or always throw
            if (_DebugData.fDisableThis)
            {
                if(pfDisableAssert) 
                {
                    *pfDisableAssert=TRUE;
                }    

                if (pfAlwaysThrow)
                {
                    *pfAlwaysThrow=_DebugData.fThrowThis;
                }
            }

            switch (iDlgValue)
            {
            case IDABORT:
                TerminateProcess(GetCurrentProcess(), (UINT)-1);
                break;

            case IDRETRY:
                if (pfDoInt3) *pfDoInt3 = TRUE;
                break;

            case IDIGNORE:
                break;

            case IDC_THROW:
                if (pfThrow)
                {
                    *pfThrow=FALSE;
                }
                break;
            }
        }
        else
        {
            if (pfThrow)
            {
                *pfThrow=_fThrowAllAsserts;
            }
        }
    }
}

//=--------------------------------------------------------------------------=
// Take a minidump of this process
//=--------------------------------------------------------------------------=
static void HandleMinidumpCommand
(
 HWND hwnd   // Parent window for message boxes. May be NULL.
 )
{
    char szMiniDump[_MAX_PATH+1];
    if (_WriteMiniDump(NULL, NULL, TRUE, szMiniDump))
    {
        char szText[_MAX_PATH + _countof("Minidump written to ''.")];
        sprintf_s(szText, "Minidump written to '%s'.", szMiniDump);

        MessageBox(hwnd, szText, "MemSpect Assertion", MB_OK);
    }
    else
    {
        MessageBox(hwnd, "Failed to save minidump.", "MemSpect Assertion", MB_OK | MB_ICONERROR);
    }
}

static HRESULT InnerHandleCopyCommand
(
 HWND hwnd   // Window handle for the clipboard. May be NULL.
 )
{
    HRESULT hr = S_OK;

    ////
    // Write out the assert data for both dumps.
    //
    CDebugStream strm;
    DWORD dwWritten = 0;
    IfFailRet(strm.Write((PVOID)_DebugData.pszText,
        lstrlen(_DebugData.pszText), &dwWritten));

    ////
    // Write out the stack dump
    //
    {
        HCURSOR hOldCursor = SetCursor(LoadCursor(NULL, IDC_WAIT));
        DebDumpStackResolveSymbols(&strm, _DebugData.dwStackAddr, _DebugData.uicStackAddr, FALSE);
        SetCursor(hOldCursor);
    }
    ////
    // Write a NULL so we don't get random gunk at the end
    //
    IfFailRet(strm.Write("", 1, &dwWritten));

    ////
    // And publish the clipboard data
    //
    LPSTR psz = strm.GetBuffer();
    IfFalseRet(psz, E_OUTOFMEMORY);
    size_t cbClipData = sizeof(char) * (lstrlen(psz) + 1);
    HGLOBAL hGlob = ::GlobalAlloc(GMEM_MOVEABLE, cbClipData);
    IfFalseRet(hGlob, E_OUTOFMEMORY);
    memcpy( ::GlobalLock(hGlob), psz, cbClipData );
    GlobalUnlock(hGlob);

    if( ::OpenClipboard(hwnd) )
    {
        // Note: MSDN says that OpenClipboard(NULL), followed by EmptyClipboard, SetClipboardData
        // will fail. That doesn't appear to be true.
        ::EmptyClipboard();
        if( ::SetClipboardData(CF_TEXT, hGlob) )
        {
            hGlob = NULL; // The clipboard owns the memory now
        }
        ::CloseClipboard();
    }

    if( hGlob )
    {
        ::GlobalFree(hGlob);
    }

    return hr;
}

//=--------------------------------------------------------------------------=
// Copy the assert text and a call-stack to the clipboard
// Returns true
//=--------------------------------------------------------------------------=
static void HandleCopyCommand
(
 HWND hwnd   // Parent window for message boxes and the window handle for
             // the clipboard. May be NULL.
 )
{
    if( FAILED( InnerHandleCopyCommand( hwnd ) ) )
    {
        MessageBox(hwnd, "Unable to put data on the clipboard.", NULL, MB_OK | MB_ICONSTOP);
    }
}

//-----------------------------------------------------------------------------
// Out of process VSASSERT dialog handling
//
// Displaying the assert dialog out of proc ensures that the asserting process
// is completely frozen and not even pumping messages. This is more robust
// than trying to put up UI for threads which don't have message pumps or
// if you actually have an ASSERT within a message handler. It's also been
// very useful for reporting memory leaks on shutdown, when there are problems
// showing a message box after DLLs have unloaded (even though there are still
// lingering windows with WndProcs pointing back into those unloaded DLLs)
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// There is a shared memory block passed between the asserting process and the
// out-of-proc dialog.
//-----------------------------------------------------------------------------
struct OutOfProcSharedMemoryBlockHeader
{
    DEBUG_THREAD_DATA m_DebugData;
    size_t m_cchTitle;      // Length of title in chars
    size_t m_cchText;       // Length of text in chars
    DWORD m_pid;            // PID of the asserting process
    bool m_fSucceeded;      // Set to true if the out-of-proc handler succeeded
};
// Title and Text immediately follow the header as ANSI, NUL terminated strings
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// When running out-of-proc, we spin up a thread to watch for the asserting
// process going away. If it does, then this process should also terminate.
//-----------------------------------------------------------------------------
DWORD WINAPI OutOfProcWatchdogThreadProc( LPVOID lpParam )
{
    DWORD pid = reinterpret_cast< DWORD >(lpParam);

    HANDLE hProcess = ::OpenProcess(SYNCHRONIZE, FALSE, pid);
    if( hProcess )
    {
        ::WaitForSingleObject( hProcess, INFINITE );
        ::CloseHandle(hProcess);
        ::ExitProcess(0); // We should just die
    }
    return 0;
}

//#pragma comment(linker, "/EXPORT:VsAssertOutOfProcAssertDialogA=_VsAssertOutOfProcAssertDialogA@16,PRIVATE")


//-----------------------------------------------------------------------------
// We use RUNDLL32.exe as the process. RUNDLL32 can be told to 'run' any
// exported entrypoint in a DLL. So, we use this entry point in vsassert.dll
// itself.
//-----------------------------------------------------------------------------
extern "C" void CALLBACK VsAssertOutOfProcAssertDialogA(
    HWND hwnd,
    HINSTANCE hInst,
    LPCSTR szCommandLine,
    int nCmdShow )
{
    // The command line is the decimal value of the file mapping handle
    HANDLE hFileMapping;
    // scope
    {
#ifdef _WIN64
        char* szEnd;
        unsigned __int64 ui64Handle = _strtoui64( szCommandLine, &szEnd, 16 );
        if( ui64Handle == 0 )
        {
            // Bad handle
            return;
        }

        hFileMapping = reinterpret_cast< HANDLE >( ui64Handle );
#else
        char* szEnd;
        DWORD ulHandle = strtoul( szCommandLine, &szEnd, 16 );
        if( ulHandle == 0 )
        {
            // Bad handle
            return;
        }

        hFileMapping = reinterpret_cast< HANDLE >( ulHandle );
#endif
    }

    LPVOID pvMappedSection = MapViewOfFile( hFileMapping, FILE_MAP_READ | FILE_MAP_WRITE, 0, 0, 0 );
    if( !pvMappedSection )
    {
        return;
    }

    OutOfProcSharedMemoryBlockHeader* pHeader = static_cast< OutOfProcSharedMemoryBlockHeader* >(pvMappedSection);

    // If the asserting process dies while this process is running, we want to
    // cancel the dialog automatically. To do that, we create a new thread that
    // blocks on the other process handle. If the handle is signalled, then
    // this process should die too.
    HANDLE hWatchdogThread = ::CreateThread( NULL, 0, OutOfProcWatchdogThreadProc, reinterpret_cast< LPVOID >( pHeader->m_pid ), 0, NULL );

    // Copy data out of the mapped section
    memcpy( &_DebugData, &pHeader->m_DebugData, sizeof(_DebugData) );

    // Fix up the strings
    _DebugData.pszTitle = reinterpret_cast< LPCSTR >( pHeader + 1 );
    _DebugData.pszText = _DebugData.pszTitle + pHeader->m_cchTitle;

    //{
    //    char szBuf[1000+MAX_PATH];
    //    sprintf_s(szBuf, "Rundll32.exe process starting for assert. You can now attach a debugger\r\n Pid = %d\r\n Cause a DebugBreak() ?", GetCurrentProcessId());
    //    if (MessageBoxA(0,_DebugData.pszText,szBuf,MB_YESNO) == IDYES)
    //    {
    //        _asm int 3;
    //    }
    //}


    // Show the dialog box
    INT_PTR dlgResult = DialogBox( g_hinstDll, MAKEINTRESOURCE(IDD_DEBASSERT), hwnd, (DLGPROC)_AssertDlgProc );
    _DebugData.dwRetVal = static_cast< DWORD >(dlgResult); // Truncation in 64-bit builds

    // Copy parameters back to the shared memory section
    memcpy( &pHeader->m_DebugData, &_DebugData, sizeof(_DebugData) );

    pHeader->m_fSucceeded = true;
    ::UnmapViewOfFile( pHeader );

    ::TerminateThread(hWatchdogThread, 0);
    ::CloseHandle(hWatchdogThread);

    // CONSIDER:
    // Could call ExitProcess(0) here since running DllMain, checking heaps
    // etc. is not necessary for rundll32.exe
}

//-----------------------------------------------------------------------------
// Create the process to host our out-of-proc assert dialog
//-----------------------------------------------------------------------------
static HANDLE CreateVsAssertProcess( HANDLE hFileMapping, LPCTSTR txtAssertMsg )
{
    STARTUPINFO StartupInfo = { sizeof(STARTUPINFO) };
    PROCESS_INFORMATION ProcessInformation = {};

    // The command-line is:
    // rundll32.exe "[path-to-vsassert]",VsAssertOutOfProcAssertDialog 12345
    // where path-to-vsassert is the full path to this DLL and 12345 is the file handle in hex

    TCHAR achCommandLine[MAX_PATH*2];
	TCHAR txtMsgTrunc[MAX_PATH];
	auto nLen = strlen(txtAssertMsg);
	strncpy(txtMsgTrunc, txtAssertMsg, min( nLen, sizeof(txtMsgTrunc)) -1 );
	txtMsgTrunc[min(nLen,sizeof(txtMsgTrunc) -1 )] = 0;

    _stprintf_s( achCommandLine, _T("rundll32.exe \"%s\",VsAssertOutOfProcAssertDialog %p %s"),
        g_szVSAssertDllFullPathName,
        hFileMapping ,
		txtMsgTrunc // we add the string of the assert: that way any tool examining cmd line args can read the assert (like ProcExp)
		);

    if( !CreateProcess(
        NULL,
        achCommandLine, 
        NULL, 
        NULL, 
        TRUE,   // inherit handles
        CREATE_DEFAULT_ERROR_MODE | NORMAL_PRIORITY_CLASS, 
        NULL,
        NULL, 
        &StartupInfo,
        &ProcessInformation))
    {
        return NULL;
    }

    // We don't need the thread handle
    ::CloseHandle( ProcessInformation.hThread );

    // Return the process handle back to the caller
    return ProcessInformation.hProcess;
}

//-----------------------------------------------------------------------------
// Returns -1 if the out-of-proc dialog couldn't be shown, otherwise the
// return code from the dialog.
// May be IDNO if the user requested a minidump
// May be IDYES if the user requested a copy of the assert text and call-stack
//-----------------------------------------------------------------------------
static int InnerDisplayOutOfProcDialogBox( HANDLE hFileMapping )
{
    LPVOID pvMappedSection = MapViewOfFile( hFileMapping, FILE_MAP_READ | FILE_MAP_WRITE, 0, 0, 0 );
    if( NULL == pvMappedSection )
    {
        return -1;
    }
    
    OutOfProcSharedMemoryBlockHeader* pHeader = static_cast< OutOfProcSharedMemoryBlockHeader* >(pvMappedSection);

    // Set up the parameter block
    memset( pHeader, 0, sizeof(OutOfProcSharedMemoryBlockHeader) );
    memcpy( &pHeader->m_DebugData, &_DebugData, sizeof(_DebugData) );
    pHeader->m_pid = ::GetCurrentProcessId();
    pHeader->m_cchTitle = strlen( _DebugData.pszTitle ) + 1;
    pHeader->m_cchText = strlen( _DebugData.pszText ) + 1;

    // The strings immediately follow the header
    LPSTR szTextStart = reinterpret_cast< LPSTR >( pHeader + 1 );
    strcpy( szTextStart, _DebugData.pszTitle );
    strcpy( szTextStart + pHeader->m_cchTitle, _DebugData.pszText );

    // Create the RunDLL32 process
    HANDLE hProcess = CreateVsAssertProcess( hFileMapping, _DebugData.pszText );
    if( !hProcess )
    {
        ::UnmapViewOfFile( pHeader );
        return -1;
    }

    // Wait until the assert dialog is dismissed in the other process by
    // waiting on the process handle.

    //-------------------------------------------------------------------------
    // If you are stopped in the debugger at this line, here's what's going on:
    // Your application hit a failed assert and the VSASSERT mechanism is
    // showing an assert dialog in another process. The process is
    // "rundll32.exe". While that dialog is being displayed, this process (the
    // one with the failing assert) is frozen, waiting for rundll32.exe to exit
    //-------------------------------------------------------------------------
	auto done = false;
	while (!done)
	{
	    auto res =::WaitForSingleObject( hProcess, 5000 );
		switch(res)
		{ 
		case WAIT_OBJECT_0:
			done=true;
			break;
        case WAIT_TIMEOUT:
			// under a debugger, we'll eventually get here, where we can Set next statement. So set a bp here
			done = false;
			break;
		}
	}


    ::CloseHandle( hProcess );

    // Copy back out params, only if the dialog was shown
    bool fSucceess = pHeader->m_fSucceeded;
    
    if( fSucceess )
    {
        _DebugData.dwRetVal = pHeader->m_DebugData.dwRetVal;
        _DebugData.fAllowThrow = pHeader->m_DebugData.fAllowThrow;
        _DebugData.fDisableAll = pHeader->m_DebugData.fDisableAll;
        _DebugData.fThrowAll = pHeader->m_DebugData.fThrowAll;
        _DebugData.fDisableThis = pHeader->m_DebugData.fDisableThis;
        _DebugData.fThrowThis = pHeader->m_DebugData.fThrowThis;
    }

    ::UnmapViewOfFile(pHeader);

    if( !fSucceess )
    {
        return -1;
    }

    return _DebugData.dwRetVal;
}

//=--------------------------------------------------------------------------=
// _DoAssertDialogOutOfProc
// Returns -1 if the out-of-proc dialog couldn't be shown, otherwise the
// return code from the dialog.
//=--------------------------------------------------------------------------=
int _DisplayOutOfProcDialogBox()
{

    if (!g_pCDebFreezeAllThreads) // if the threads are frozen, then we can't call GetModuleFileName:         LdrLockLoaderLock(LDR_LOCK_LOADER_LOCK_FLAG_RAISE_ON_ERRORS, NULL, &LoaderLockCookie);
    {
        // if the Assert dialog process causes an Assert, then we have an infinite loop of processes being 
        // created, which is very hard to stop
        char szFname[_MAX_FNAME];
        _splitpath(g_szProcessExeFullPathName, NULL, NULL, szFname, NULL);
        if (lstrcmpi(szFname, "rundll32")==0)
        {
            return -1;
        }
    }
    

    
    // This is similar to the way Watson reports unhandled exceptions.

    // To run the assert dialog out-of-proc:
    // 1. Set up a shared memory block with the in/out parameters
    //    Use a memory mapped file (backed by the paging file)
    // 2. Use RunDLL32.exe as the 2nd process, and tell it to run our "OutOfProcCallback"
    // 3. Pass the handle to the to the shared memory block on the 'command line'
    // 4. Block this thread (by waiting on the process handle of RunDLL32) until
    //    RunDLL32 returns.
    // Inside OutOfProcCallback:
    // 1. Retrieve the shared memory block
    // 2. Display the dialog using the parameters in the shared memory block
    // 3. Update the return code in the shared memory block when the dialog returns

    // We want the file mapping handle to be available to the spawned process
    // So use a SECURITY_ATTRIBUTE that has "inherit handle"
    SECURITY_ATTRIBUTES SecurityAttributes = {
        sizeof( SECURITY_ATTRIBUTES ),  // nLength
        NULL,                           // lpSecurityDescriptor. NULL = default for calling process
        TRUE                            // bInheritHandle
    };

    size_t cbSharedMem = sizeof(OutOfProcSharedMemoryBlockHeader) +
        ( 1 + strlen(_DebugData.pszTitle) ) * sizeof(char) +
        ( 1 + strlen(_DebugData.pszText) ) * sizeof(char);

    HANDLE hFileMapping = CreateFileMapping(
        INVALID_HANDLE_VALUE,   // backed by paging file
        &SecurityAttributes,
        PAGE_READWRITE,
        0,
        static_cast< DWORD >(cbSharedMem), // Truncation in 64-bit
        NULL);

    if( !hFileMapping )
    {
        return -1;
    }

    int iRetVal = 0;

    // Some user actions (minidump and "copy stack") need to happen back in
    // the asserting process. The dialog will be dismissed and will need to
    // be reshown after taking the appropriate action. So, we need to loop
    // here.
    for( bool fContinue = true; fContinue; /*fContinue updated in loop*/ )
    {
        iRetVal = InnerDisplayOutOfProcDialogBox( hFileMapping );

        switch( iRetVal )
        {
        case IDNO:
            HandleMinidumpCommand(NULL);
            break;

        case IDYES:
            HandleCopyCommand(NULL);
            break;

        default:
            fContinue = false;
            break;
        }
    }

    ::CloseHandle( hFileMapping );

    return iRetVal;
}

//=--------------------------------------------------------------------------=
// _DoAssertDialog
//=--------------------------------------------------------------------------=
// Displays the assert dialog.  If possible, this does so in another thread.
// If not, it handles messages in the queue properly so as to not cause
// any problems on the primary thread.
//
int _DoAssertDialog
(
 LPCSTR pszText,
 LPCSTR pszTitle,
 UINT   mbFlags
 )
{
    int id;

    ////
    // Setup some data for the debug thread / assert dialog
    //
    _DebugData.pszText = pszText;
    _DebugData.pszTitle = pszTitle;
    _DebugData.mbFlags = mbFlags;
    _DebugData.uicStackAddr = 0;    

    ////
    // Has someone terminated the debug thread behind our back?  The OS will do this
    // in abnormal termination situations
    //
    if (_DebugData.hThread && WAIT_OBJECT_0 == WaitForSingleObject(_DebugData.hThread, 0))
    {
        CloseHandle(_DebugData.hThread);
        _DebugData.hThread = NULL;
    }

    if (_DebugData.hThread && !_DebugData.fOutOfProc) // No need to use the thread if we're going out-of-proc
    {
        ////
        // We're using our debug thread for the dialog, which means that it has it's
        // own message loop. This means we don't have to worry about what messages
        // are currently on queue in this thread so things are a lot simpler.
        //

        ////
        // This displays the dialog
        //
        _DebugData.nAction = ACTION_ASSERT;
        SetEvent(_DebugData.hevtAction);

        ////
        // And this freezes our thread until someone has decided what to do about it
        //
        WaitForSingleObject(_DebugData.hevtResume, INFINITE);

        id = (int)_DebugData.dwRetVal;
    }
    else if(!g_fShuttingDown && !_DebugData.fOutOfProc)
    {
        BOOL fWM_QUIT = FALSE;
        MSG  msg;

        ////
        // We have to share the message loop of this thread with the assert dialog.
        // We try to preserve as many messages as possible
        //
        MSG   rgSkip[1000]; // abitrary #
        short sSkip = 0;

        while (sSkip < sizeof(rgSkip) / sizeof(*rgSkip) &&
            _PeekMessage(&rgSkip[sSkip], NULL, NULL, NULL, PM_REMOVE))
            sSkip++;

        // Put up the dialog.
        id = _DisplayAssertDialog();

        ////
        // It's possible that the act of displaying the dialog forced a WM_QUIT message
        // on the queue.  This message would cause the dialog's message loop to promptly
        // terminate.  So, we check here and if it exists, we remove it and re-display
        // the dialog.
        //
        // (Note that we can't check first, because there may be another message on the
        // queue that causes DefWindowProc to generate the WM_QUIT).
        //
        if (_PeekMessage(&msg, NULL, WM_QUIT, WM_QUIT, PM_REMOVE))
        {
            fWM_QUIT = TRUE;
            id = _DisplayAssertDialog();
        }

        ////
        // Repost messages in the same order that we got them off.
        //
        for (short sMsg = 0; sMsg < sSkip; sMsg++)
            PostMessage(rgSkip[sMsg].hwnd, rgSkip[sMsg].message,
            rgSkip[sMsg].wParam, rgSkip[sMsg].lParam);

        if (fWM_QUIT)
            PostMessage(msg.hwnd, msg.message, msg.wParam, msg.lParam);
    }
    else
    {
        // Out of proc case handled inside _DisplayAssertDialog so it can fall
        // back to in-proc if necessary.
        id = _DisplayAssertDialog();
    }

    return id;
}

//=--------------------------------------------------------------------------=
// _PeekMessage
//=--------------------------------------------------------------------------=
// We commonly need to check to see if a given message is on queue, and we
// generally don't care which window it's posted to.  So, we do a PeekMessage
// with a NULL hwnd, right?  Wrong!  This will skip any message that's not
// posted to a window.  This function is essentially PeekMessage with the
// addition that it will peek a message that was posted with PostThreadMessage
//
BOOL _PeekMessage
(
 MSG *pMsg,
 HWND hwnd,
 UINT uFirst,
 UINT uLast,
 UINT uFlags
 )
{
    BOOL fRet = PeekMessage(pMsg, hwnd, uFirst, uLast, uFlags);

    ////
    // A NULL Window handle says we don't care which window the message belongs
    // to, but if the message doesn't belong to ANY window, PeekMessage
    // requires a -1 to retrieve it.
    //
    if (hwnd == NULL && !fRet)
        fRet = PeekMessage(pMsg, (HWND)-1, uFirst, uLast, uFlags);

    return fRet;
}

//=--------------------------------------------------------------------------=
// _CreateDebugThread
//=--------------------------------------------------------------------------=
// If the thread doesn't exist, this will create the debugging thread.
// It will only create the thread if it's safe to do so
//
VOID _CreateDebugThread
(
 )
{
    ////
    // We should already by locked, but let's be anal about this
    //
    LOCK_MUTEX(&_csDebugLock);

    ////
    // Can we, do we need to, and do we want to?
    //
    if ( _DebugData.hThread              ||
        FSWITCH(_fSingleThreadedAssert) ||
        _DebugData.fShutdown
        )
        return;

    ////
    // We create the thread structures in stages.  The last stage is the
    // actual thread.  If any stage fails, we return, so we can try
    // again on the next assert.  DebugTerminateThread cleans up all of
    // these independently.
    //

    if (!_DebugData.hevtAction)
    {
        _DebugData.hevtAction = CreateEvent(NULL, FALSE, FALSE, NULL);
        if (!_DebugData.hevtAction)
            return;
    }

    if (!_DebugData.hevtResume)
    {
        _DebugData.hevtResume = CreateEvent(NULL, FALSE, FALSE, NULL);
        if (!_DebugData.hevtResume)
            return;
    }

    if(g_fEnableFastGetStack)
    {
        TMUpdateLoadedModules();
    }
    
    _DebugData.hThread = CreateThread(NULL, 0, &_DebugThreadProc, NULL,
        0, &_DebugData.dwThreadId);
    g_dwDebugThreadId = _DebugData.dwThreadId;
}

//=--------------------------------------------------------------------------=
// _DebugThreadProc
//=--------------------------------------------------------------------------=
// This routine runs in another thread and handles the debug assert dialog.
// We run this in another thread for a several of reasons:
//
// 1.  We can exactally preserve the message queue for the app
// 2.  We can preserve the app's current modality settings
// 3.  We can get stack traces of all the frozen threads
//
static DWORD WINAPI _DebugThreadProc
(
 PVOID
 )
{
#ifdef DEBUG
    VsNameThisThread ("VSAssert Debug Thread");
#endif

    for (;;)
    {
        DWORD dw;

        if(g_fEnableFastGetStack) // we need to look for loaded modules only if we are getting stack
        {
            dw = WaitForSingleObject(_DebugData.hevtAction, g_dwUpdateLoadedModulesTimeout);
            if(WAIT_TIMEOUT == dw || _DebugData.nAction != ACTION_CALLBACK)
            {
                TMUpdateLoadedModules(); 
            }
        }
        else
            dw = WaitForSingleObject(_DebugData.hevtAction, INFINITE);

        if(WAIT_TIMEOUT != dw)
        {
            switch (_DebugData.nAction)
            {
                ////
                // Display the assert dialog
                //
            case ACTION_ASSERT:
                _DebugData.dwRetVal = (DWORD)_DisplayAssertDialog();
                SetEvent(_DebugData.hevtResume);
                break;

                ////
                // Callback into some user code
                //
            case ACTION_CALLBACK:
                if (_DebugData.pfnCallback)
                    _DebugData.hrCallback = _DebugData.pfnCallback(_DebugData.dwCallback);
                SetEvent(_DebugData.hevtResume);
                break;

                ////
                // Terminate the thread
                //
            case ACTION_TERMINATE:
                SetEvent(_DebugData.hevtResume);
                CloseHandle(_DebugData.hThread);
                _DebugData.hThread = NULL;
                return 0;
            }
        }    
    }
}

//=--------------------------------------------------------------------------=
// _DisplayAssertDialog
//=--------------------------------------------------------------------------=
// Does the actual displaying.
//
int _DisplayAssertDialog
(
 )
{
    if( _DebugData.fOutOfProc )
    {
        int iRet = _DisplayOutOfProcDialogBox();
        if( iRet != -1 )
        {
            return iRet;
        }

        // Out-of-proc failed. Fall back to in-proc
        _DebugData.fOutOfProc = FALSE;
    }

    int       nRetVal = 0;
    HWND      hwndActive = NULL;
    
    // VsWhidbey - #50830: we should not use an active window as a parent on shutdown, it might be a weird one, which gets destroyed while we show the assert dialog.
    if(!g_fShuttingDown) 
        hwndActive = GetActiveWindow();

    /*    
    if(hwndActive)
    {
        LONG lStyle = GetWindowLong(hwndActive, GWL_STYLE);
        char szClassName[100];
        *szClassName = 0;
        GetClassName(hwndActive, szClassName, 100);
        DWORD dwProcessID = 0;
        DWORD dwThreadID = GetWindowThreadProcessId(hwndActive, &dwProcessID);
        RECT rc;
        GetWindowRect(hwndActive, &rc);
        char szTitle[100];
        *szTitle = 0;
        GetWindowText(hwndActive, szTitle, 100);
        
        DEBUGPRINTF("\r\nActiveWindow:\r\n hActiveWnd = 0x%lx\r\n Class = %s\r\n Style = 0x%lx\r\n Thread = 0x%lx\r\n CurrentThread = 0x%lx\r\n Process = 0x%lx\r\n, CurrentProcess = 0x%lx\r\n", 
           hwndActive, szClassName, lStyle, dwThreadID, GetCurrentThreadId(), dwProcessID, GetCurrentProcessId());

        DEBUGPRINTF("rc = %d,%d,%d,%d\r\n Title = %s\r\n", rc.left, rc.top, rc.right, rc.bottom, szTitle);

    }
    */

    ////
    // Since DialogBox is a rather complex function, catch any exceptions here.
    // I have already seen it crash once in a SetWindowText call, so I know
    // that the OS isn't quite as stable here as it should be.
    //
    _try
    {
        nRetVal = (DWORD)DialogBox(g_hinstDll, MAKEINTRESOURCE(IDD_DEBASSERT), hwndActive, (DLGPROC)_AssertDlgProc);
    }
    _finally
    {
        if (AbnormalTermination())
            nRetVal = -1;
    }

    if (-1 == nRetVal)
    {
#define MAX_TEXT_LENGTH 2048        
        char szBuf[MAX_TEXT_LENGTH];
        const char * pszText;

        // cut the text if it is too long - it will not be visible anyway 

        if(strlen(_DebugData.pszText) > MAX_TEXT_LENGTH)
        {
            strncpy(szBuf, _DebugData.pszText, MAX_TEXT_LENGTH - 5);
            strcpy(&szBuf[MAX_TEXT_LENGTH - 5], " ...");
            pszText = szBuf;
        }
        else  
            pszText = _DebugData.pszText;

        nRetVal = (DWORD)MessageBox(hwndActive, pszText,
            _DebugData.pszTitle,
            _DebugData.mbFlags);
    }

    return nRetVal;
}

//=--------------------------------------------------------------------------=
// _AssertDlgProc
// CAUTION:
// This DlgProc can be running on a backgrdound thread, or even out-of-proc
// Be careful not to access variables outside the _DebugData block.
//=--------------------------------------------------------------------------=
// Dialog box procedure for the assert dialog
//
BOOL CALLBACK _AssertDlgProc
(
 HWND    hwnd,
 UINT    uMsg,
 WPARAM  wParam,
 LPARAM
 )
{
    LRESULT CALLBACK _AssertSubclassProc(HWND, UINT, WPARAM, LPARAM);

    switch (uMsg)
    {
    case WM_INITDIALOG:
        scope
        {
            int nBut;

            ////
            // Set us to the top of the zorder and put us somewhere nice on the screen
            //
            RECT rc;
            GetClientRect(hwnd, &rc);
            rc.left = (GetSystemMetrics(SM_CXSCREEN) - rc.right) / 2;
            rc.top = (GetSystemMetrics(SM_CYSCREEN) - rc.bottom) / 2;
            SetWindowPos(hwnd, NULL, rc.left, rc.top, 0, 0, SWP_NOSIZE|SWP_NOACTIVATE|SWP_NOZORDER);

            ////
            // Setup the tabstops on the edit
            //
            const int nTabStop = 8;
            SendDlgItemMessage(hwnd, IDC_ASSERT_TEXT, EM_SETTABSTOPS, 1, (LPARAM)&nTabStop);

            ////
            // Setup the assert text
            //
            SetWindowText(GetDlgItem(hwnd, IDC_ASSERT_TEXT), _DebugData.pszText);
            SendDlgItemMessage(hwnd, IDC_ASSERT_TEXT, LB_SETCURSEL, (UINT)-1, 0);

            // enable controls
            // throwing controls only visible for asserts with throw. Helps not to confuse others.
            ::EnableWindow(::GetDlgItem(hwnd,IDC_THROW), _DebugData.fAllowThrow);
            ::ShowWindow(::GetDlgItem(hwnd, IDC_THROWTHIS), _DebugData.fAllowThrow ? SW_SHOW : SW_HIDE);
            ::ShowWindow(::GetDlgItem(hwnd, IDC_THROWALL), _DebugData.fAllowThrow ? SW_SHOW : SW_HIDE);
            // These get enabled when the disable gets checked
            ::EnableWindow(::GetDlgItem(hwnd, IDC_THROWTHIS), false);
            ::EnableWindow(::GetDlgItem(hwnd, IDC_THROWALL), false);
            // These now start out checked
            ::SendDlgItemMessage(hwnd, IDC_THROWTHIS, BM_SETCHECK, true, 0);
            ::SendDlgItemMessage(hwnd, IDC_THROWALL, BM_SETCHECK, true, 0);

            // Disable the minidump button on the dialog if we can't
            //
            if (!_CanMiniDump())
                ::EnableWindow(::GetDlgItem(hwnd, IDNO), false);

            ////
            // Setup a subclass to the buttons for the help text.  The subclass
            // displays help at the bottom of the dialog when the mouse is on a
            // button.
            //
            _pfnButtonSubclassProc = (WNDPROC)GetWindowLongPtr(GetDlgItem(hwnd, IDABORT), GWLP_WNDPROC);
            for (nBut = IDABORT; nBut <= IDC_THROWTHIS; nBut++)
                SetWindowLongPtr(GetDlgItem(hwnd, nBut), GWLP_WNDPROC, (LONG_PTR)_AssertSubclassProc);

            ////
            // If we're out of proc, then we want to put something in the task bar to give
            // a decent chance of the user noticing.
            if( _DebugData.fOutOfProc )
            {
                ::SetWindowLongPtr( hwnd, GWL_EXSTYLE, ::GetWindowLongPtr( hwnd, GWL_EXSTYLE ) | WS_EX_APPWINDOW );
            }

            ////
            // If we're on another thread, we might not get focus.  So force it
            //
            SetForegroundWindow(hwnd);
        }
        return TRUE;

    case WM_MOUSEMOVE:
        SetWindowText(GetDlgItem(hwnd, IDC_ASSERT_HELP_TEXT), "");
        return FALSE;

    case WM_COMMAND:
        switch (LOWORD(wParam))
        {
            ////
            // We process IDNO and IDYES.  The rest get returned to
            // the caller.
            //
        case IDNO: // "Minidump"
            // If we're out-of-proc, then we need to exit back to the asserting process
            // in order to take a minidump.
            if( _DebugData.fOutOfProc )
            {
                EndDialog(hwnd, IDNO);
            }
            else
            {
                HandleMinidumpCommand(hwnd);
            }
            return TRUE;

        case IDYES: // "Copy"
            // If we're out-of-proc, then we need to exit back to the asserting process
            // in order to take a minidump.
            if( _DebugData.fOutOfProc )
            {
                EndDialog(hwnd, IDYES);
            }
            else
            {
                HandleCopyCommand(hwnd);
            }
            return TRUE;

        case IDABORT:
        case IDRETRY:
        case IDIGNORE:
        case IDC_THROW:
            scope
            {
                int nBut;

                for (nBut = IDABORT; nBut <= IDC_THROWTHIS; nBut++)
                    SetWindowLongPtr(GetDlgItem(hwnd, nBut), GWLP_WNDPROC, (LONG_PTR)_pfnButtonSubclassProc);

                // Pop the checkboxes back into the debug data
                _DebugData.fDisableAll=(BOOL)::SendDlgItemMessage(hwnd,IDC_DISABLEALL,BM_GETCHECK, 0, 0);
                if (_DebugData.fDisableAll)
                {
                    _DebugData.fThrowAll=(BOOL)::SendDlgItemMessage(hwnd,IDC_THROWALL,BM_GETCHECK, 0, 0);
                }

                _DebugData.fDisableThis=(BOOL)::SendDlgItemMessage(hwnd,IDC_DISABLETHIS,BM_GETCHECK, 0, 0);
                if (_DebugData.fDisableThis)
                {
                    _DebugData.fThrowThis=(BOOL)::SendDlgItemMessage(hwnd,IDC_THROWTHIS,BM_GETCHECK, 0, 0);
                }

                EndDialog(hwnd, LOWORD(wParam));
            }
            return TRUE;

        case IDC_DISABLETHIS:
            if (_DebugData.fAllowThrow)
            {
                ::EnableWindow(::GetDlgItem(hwnd,IDC_THROWTHIS),(BOOL)::SendDlgItemMessage(hwnd,IDC_DISABLETHIS,BM_GETCHECK, 0, 0));
            }
            break;

        case IDC_DISABLEALL:
            ::EnableWindow(::GetDlgItem(hwnd,IDC_DISABLETHIS),!(BOOL)::SendDlgItemMessage(hwnd,IDC_DISABLEALL,BM_GETCHECK, 0, 0));
            if (_DebugData.fAllowThrow)
            {
                if (::SendDlgItemMessage(hwnd,IDC_DISABLEALL,BM_GETCHECK, 0, 0))
                {
                    ::EnableWindow(::GetDlgItem(hwnd,IDC_THROWTHIS),false);
                }
                else
                {
                    ::EnableWindow(::GetDlgItem(hwnd,IDC_THROWTHIS),(BOOL)::SendDlgItemMessage(hwnd,IDC_DISABLETHIS,BM_GETCHECK, 0, 0));
                }

                ::EnableWindow(::GetDlgItem(hwnd,IDC_THROWALL),(BOOL)::SendDlgItemMessage(hwnd,IDC_DISABLEALL,BM_GETCHECK, 0, 0));
            }
            break;

        }
        return FALSE;
    }
    return FALSE;
}


//=--------------------------------------------------------------------------=
// _AssertSubclassProc
//=--------------------------------------------------------------------------=
// This subclasses the buttons on the assert dialog and provides help
// when the mouse passes over them.
//
LRESULT CALLBACK _AssertSubclassProc
(
 HWND    hwnd,
 UINT    uMsg,
 WPARAM  wParam,
 LPARAM  lParam
 )
{
    ////
    // We display help text.  I didn't think this was worth the effort to
    // localize, so I just put it here.
    //
    static const LPSTR szHelpText[] =
    { "Immediately terminates the process.",  // IDABORT
    "Loads the process into the debugger.  If no debugger is installed this will crash.", // IDRETRY
    "Ignores this assert and continues execution.\nPress Ctrl to disable this assert, Shift to disable all asserts.", // IDIGNORE
    "Copies this assert text and call-stack to the clipboard.",  // IDYES
    "Writes a minidump of the assertion.", // IDNO
    "Skips the throw that would have happened in retail if this exception was detected.", // IDC_THROW
    "Disables all assertions during this session of the program.", // IDC_DISABLEALL
    "Disables this assertion during this session of the program.", // IDC_DISABLETHIS
    "When all assertions are disabled, if the assertion condition is met, an exception is thrown.", // IDC_THROWALL
    "When this assertion is disabled, if its condition is met, an exception is thrown.", // IDC_THROWTHIS
    };

    if (WM_MOUSEMOVE == uMsg)
    {

        // Find the help string
        UINT uId = GetDlgCtrlID(hwnd);
        int index=uId - IDABORT;

        // Don't allow a fault here
        LPCSTR lpstrHelpText=NULL;
        if (index<(sizeof(szHelpText)/sizeof(szHelpText[0])))
        {
            lpstrHelpText=szHelpText[uId - IDABORT];
        }

        HWND hwndControl=GetDlgItem(GetParent(hwnd), IDC_ASSERT_HELP_TEXT);
        // Only display help if the relevant control is visible and enabled.
        if ( lpstrHelpText!=NULL && 
            ::IsWindow(hwnd) && 
            ::IsWindowEnabled(hwnd) && 
            ::IsWindowVisible(hwnd))
        {
            SetWindowText(hwndControl, lpstrHelpText);
        }
    }

    return CallWindowProc((WNDPROC)_pfnButtonSubclassProc, hwnd, uMsg, wParam, lParam);
}

/////
// This will show up in Debug.Threads in the VC debugger
/////
CLINKAGE VOID ENTRYPOINT VsNameThisThread (LPCSTR szThreadName)
{
    SetThreadName(GetCurrentThreadId(), szThreadName, 0);
}

CLINKAGE void __cdecl vsassertf(bool fTest, char *szFmt, ...)
{
	if (!fTest)
	{
        static BOOL fDisableThisAssert = FALSE;
        if (!fDisableThisAssert)
        {
            va_list    arglist;
            char szMsg[4096];
            va_start(arglist, szFmt);
            _vsnprintf(szMsg, _countof(szMsg), szFmt, arglist );
            szMsg[_countof(szMsg) - 1] = 0;
            va_end(arglist);

            if(g_fStopOnVsAssert ||
                VsAssert(szMsg, "xx" , __FILE__, __LINE__, &fDisableThisAssert))
            VsDebugBreak();
        }
    }
}

