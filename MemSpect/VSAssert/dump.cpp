//=--------------------------------------------------------------------------=
// VsDump.Cpp
//=--------------------------------------------------------------------------=
//
// This module provides a mechanism for dumping callstacks in-situ 
// without having to attach to a debugger.  Very handy when random 
// problems occur.
//
//=--------------------------------------------------------------------------=
//
// Copyright (c) 1988-1997, Microsoft Corporation
//			All Rights Reserved
// Information Contained Herein Is Proprietary and Confidential.
//
// Notes:
//    This code can have NO asserts.  It is usally called from the assert
//    mechanism.
//
//=--------------------------------------------------------------------------=

#include "Pch.H"
#include "main.h"
#include "dump.h"
#include "StackWalk.h"


//=--------------------------------------------------------------------------=
// DebDumpStack
//=--------------------------------------------------------------------------=
// Dumps a symbolic trace of stack information to the given stream.
//
HRESULT WINAPI DebDumpStack
(
 IStream *pStream,
 UINT     ifrStart,
 BOOL     fWriteEOS
)
{
    DWORD_PTR dwAddr[200];
    UINT uicAddr = GetStackAddrs(ifrStart+1, _countof(dwAddr), dwAddr);
    return DebDumpStackResolveSymbols(pStream, dwAddr, uicAddr, fWriteEOS);
}        
    

static const char s_szNoStackAvailable[] = "No stack available";

HRESULT WINAPI DebDumpStackResolveSymbols
(
 IStream   * pStream,
 DWORD_PTR * pdwAddr,
 UINT        uicAddr,
 BOOL        fWriteEOS
)
{
    if(!pStream)
        return E_INVALIDARG;
        
    HRESULT   hr = S_OK;
    DWORD     dwWritten;
    
    if(uicAddr)
    {    
        char      szBuf[1024];
        
        for(UINT i = 0; i < uicAddr; i++)
        {
        
            GetStringFromAddr(pdwAddr[i], szBuf, sizeof(szBuf));
            hr = pStream->Write(szBuf, (ULONG)strlen(szBuf), &dwWritten);
            IfFailGo(hr);
            hr = pStream->Write("\r\n", 2, &dwWritten);
            IfFailGo(hr);
        }
    }
    else
    {
        hr = pStream->Write(s_szNoStackAvailable, (ULONG)strlen(s_szNoStackAvailable), &dwWritten);
        IfFailGo(hr);
    }    
    
    if(fWriteEOS)
    {
        hr = pStream->Write("\0", 1, &dwWritten);
        IfFailGo(hr);
    }   
Error:
    return hr;
}        



#if 0
/*
#include "Pch.H"
#include "Main.H"
#include "Util.H"
#include "DbgHelp.H"
#include "TlHelp32.H"

////
// defines a dynamic link
//

struct DYNALINKFUNC {
LPCSTR  szFuncName;	// name of function
FARPROC * ppfnAddr;	// points to function address
};


///////////////////////////////////////////////////////////////////////////
//
// Imports from imagehlp.dll.
//

BOOL
(CALLBACK
 *StackWalkAPI)(
 DWORD                             MachineType,
 HANDLE                            hProcess,
 HANDLE                            hThread,
 LPSTACKFRAME                      StackFrame,
 LPVOID                            ContextRecord,
 PREAD_PROCESS_MEMORY_ROUTINE      ReadMemoryRoutine,
 PFUNCTION_TABLE_ACCESS_ROUTINE    FunctionTableAccessRoutine,
 PGET_MODULE_BASE_ROUTINE          GetModuleBaseRoutine,
 PTRANSLATE_ADDRESS_ROUTINE        TranslateAddress
 );


BOOL
(CALLBACK
 *SymInitializeAPI)(
 IN HANDLE   hProcess,
 IN LPSTR    UserSearchPath,
 IN BOOL     fInvadeProcess
 );

BOOL
(CALLBACK
 *SymLoadModuleAPI)(
 IN  HANDLE          hProcess,
 IN  HANDLE          hFile,
 IN  PSTR            ImageName,
 IN  PSTR            ModuleName,
 IN  INT_PTR         BaseOfDll,
 IN  DWORD           SizeOfDll
 );

BOOL
(CALLBACK
 *SymUnloadModuleAPI)(
 IN  HANDLE          hProcess,
 IN  INT_PTR         BaseOfDll
 );

BOOL
(CALLBACK
 *SymUnDNameAPI)(
 IN  PIMAGEHLP_SYMBOL sym,               // Symbol to undecorate
 OUT LPSTR            UnDecName,         // Buffer to store undecorated name in
 IN  DWORD            UnDecNameLength    // Size of the buffer
 );

BOOL
(CALLBACK
 *SymGetSymFromAddrAPI)(
 IN  HANDLE              hProcess,
 IN  INT_PTR             dwAddr,
 OUT PDWORD              pdwDisplacement,
 OUT PIMAGEHLP_SYMBOL    Symbol
 );

static const char   _szImageHlp[] = "dbghelp.dll";
static HINSTANCE    _hinstImageHlp;
static DYNALINKFUNC _rgImageHlpFuncs[] = 
{
    {"SymInitialize",	  (FARPROC *)&SymInitializeAPI},
    {"SymLoadModule",	  (FARPROC *)&SymLoadModuleAPI},
    {"SymUnloadModule",	  (FARPROC *)&SymUnloadModuleAPI},
    {"SymUnDName",	  (FARPROC *)&SymUnDNameAPI},
    {"SymGetSymFromAddr", (FARPROC *)&SymGetSymFromAddrAPI},
    {"StackWalk",	  (FARPROC *)&StackWalkAPI}
};
static UINT _cImageHlpFuncs = sizeof(_rgImageHlpFuncs) / sizeof(DYNALINKFUNC);

///////////////////////////////////////////////////////////////////////////
//
// Imports from psapi.dll.  This is only used for Win NT
//

typedef struct _MODULEINFO {
    LPVOID lpBaseOfDll;
    DWORD SizeOfImage;
    LPVOID EntryPoint;
} MODULEINFO, *LPMODULEINFO;

BOOL
(WINAPI
 *EnumProcessModulesAPI)(
 HANDLE hProcess,
 HMODULE *lphModule,
 DWORD cb,
 LPDWORD lpcbNeeded
 );

DWORD
(WINAPI
 *GetModuleFileNameExAAPI)(
 HANDLE hProcess,
 HMODULE hModule,
 LPSTR lpFilename,
 DWORD nSize
 );
#define GetModuleFileNameExAPI GetModuleFileNameExAAPI

BOOL
(WINAPI
 *GetModuleInformationAPI)(
 HANDLE hProcess,
 HMODULE hModule,
 LPMODULEINFO lpmodinfo,
 DWORD cb
 );

static const char   _szPsapi[]      = "psapi.dll";
static const char   _szModuleName[] = "vsassert.dll";
static const char   _szTooLateForStack[] = "Shutting down - no stack available\r\n";
static HINSTANCE    _hinstPsapi;
static DYNALINKFUNC _rgPsapiFuncs[] = 
{
    {"EnumProcessModules",    (FARPROC *)&EnumProcessModulesAPI},
    {"GetModuleFileNameExA",  (FARPROC *)&GetModuleFileNameExAAPI},
    {"GetModuleInformation",  (FARPROC *)&GetModuleInformationAPI}
};
static UINT _cPsapiFuncs = sizeof(_rgPsapiFuncs) / sizeof(DYNALINKFUNC);

///////////////////////////////////////////////////////////////////////////
//
// Imports from toolhlp32.dll.  This is only used on Win 95
//

HANDLE 
(WINAPI
 *CreateToolhelp32SnapshotAPI)(
 DWORD dwFlags,
 DWORD th32ProcessID
 );

BOOL 
(WINAPI
 *Module32FirstAPI)(
 HANDLE	    hSnapshot, 
 LPMODULEENTRY32 lpme
 );

BOOL
(WINAPI
 *Module32NextAPI)(
 HANDLE	    hSnapshot, 
 LPMODULEENTRY32 lpme
 );

static const char   _szToolHlp[] = "kernel32.dll";
static HINSTANCE    _hinstToolHlp;
static DYNALINKFUNC _rgToolHlpFuncs[] = 
{
    {"CreateToolhelp32Snapshot",  (FARPROC *)&CreateToolhelp32SnapshotAPI},
    {"Module32First",		  (FARPROC *)&Module32FirstAPI},
    {"Module32Next",		  (FARPROC *)&Module32NextAPI}
};
static UINT _cToolHlpFuncs = sizeof(_rgToolHlpFuncs) / sizeof(DYNALINKFUNC);

///////////////////////////////////////////////////////////////////////////
//
// Ok, we have some processor specific stuff here, set it all up with
// a few handy macros
//

#ifdef _ALPHA_

#define IMAGE_TYPE IMAGE_FILE_MACHINE_ALPHA
#define GET_PC(tc) ((INT_PTR) tc.IntRa)
#define GET_STACK(tc) ((INT_PTR) tc.IntSp)
#define GET_FRAME(tc) ((INT_PTR) tc.IntFp)

#else 
#ifdef _X86_

#define IMAGE_TYPE IMAGE_FILE_MACHINE_I386
#define GET_PC(tc) (tc.Eip)
#define GET_STACK(tc) (tc.Esp)
#define GET_FRAME(tc) (tc.Ebp)

#else
#ifdef _IA64_

#define IMAGE_TYPE IMAGE_FILE_MACHINE_IA64
// Win64Hack (MikhailA): No stack/context information on IA64 in WINNT.H yet (as of 1/10/2000)
//#define GET_PC(tc) (tc.IntT0)
//#define GET_STACK(tc) (tc.IntSp)
//#define GET_FRAME(tc) (tc.IntT1)

#else
#error Unknown processor type -- info here is processor specific
#endif
#endif 
#endif 

///////////////////////////////////////////////////////////////////////////
//
// This structure defines a module for us.  A list of modules is simply an
// array of these structures with a the last one containing NULL entries.
//
struct MODULE
{
    LPSTR	  pszFullPath;
    LPSTR	  pszFilename;
    INT_PTR	  dwBaseAddr;
    INT_PTR	  dwEndAddr;
};

// this structure packs paramters for calling through a thread callback
//
struct DUMPPARAMS
{
    DWORD    dwThreadId;
    IStream *pStream;
    int      nLines;
};

static const char _cszUnknownModule[] = "<Unknown>";

// located in assert.cpp
extern HRESULT VsDoThreadCallback(FARPROC pfnCallback, INT_PTR dwData);

//=--------------------------------------------------------------------------=
// Private functions
//=--------------------------------------------------------------------------=
static HRESULT  _QueryLoadDll(LPCSTR pszDll, HINSTANCE *phinst, DYNALINKFUNC *rgFuncs, UINT cFuncs);
static HRESULT  _LoadModuleList(MODULE **ppModList);
static VOID	_FreeModuleList(MODULE *rgModList);
static LPSTR	_GetFilenamePart(LPSTR pszFullPath);
static MODULE	*_FindModuleInList(MODULE *rgModList, INT_PTR dwAddr);
static VOID	_GetSymbolFromAddress(INT_PTR dwAddr, LPSTR pszName,  UINT cb);
static HRESULT  _DumpThreadCallback(INT_PTR dwData);
static HRESULT  _DumpThisThread(HANDLE hProcess, HANDLE hThread, DWORD dwTid, MODULE *rgModList, IStream *pStream, int nLines = 0);


//=--------------------------------------------------------------------------=
// DebDumpCurrentThread
//=--------------------------------------------------------------------------=
// Dumps a symbolic trace of stack information to the given stream.
//
HRESULT WINAPI DebDumpCurrentThread
(
 IStream *pStream,
 int      nLines
 )
{
    DUMPPARAMS dp = {GetCurrentThreadId(), pStream, nLines};

    return VsDoThreadCallback((FARPROC)_DumpThreadCallback, (INT_PTR)&dp);
}


//=--------------------------------------------------------------------------=
// DebDumpStack
//=--------------------------------------------------------------------------=
// Dumps a symbolic trace of stack information to the given stream.
//
HRESULT WINAPI DebDumpStack
(
 IStream *pStream
 )
{
    HANDLE    hProcess = GetCurrentProcess();
    HRESULT   hr = NOERROR;
    int	    nMod;
    DWORD     dwWritten;
    char      szBuf[MAX_PATH];

    ////
    // Constant string data
    //
    const char  cszModuleList[]	= "\r\nModule List:\r\n";
    const char  cszModuleEntry[]	= "\t%s\r\n";
    const char  cszLockList[]	= "\r\nLocked Mutex List:\r\n";

    if(!pStream)
        return E_POINTER;

    // Win 95's too damn stupid and crashes if we try to dump the stack during
    // a process detach.  Just disallow this in general.
    //
    if(g_fShuttingDown)
    {
        DWORD dwFoo;
        pStream->Write(_szTooLateForStack, sizeof(_szTooLateForStack), &dwFoo);
        return S_OK;
    }

    HCURSOR hOldCursor = SetCursor(LoadCursor(NULL, IDC_WAIT));

    ////
    // Demand load imagehlp.dll
    //
    if(!_hinstImageHlp)
    {
        IfFailRet(_QueryLoadDll(_szImageHlp, &_hinstImageHlp, _rgImageHlpFuncs, _cImageHlpFuncs));

        ////
        // Initialize imagehlp
        //
        IfFalseRet(SymInitializeAPI(hProcess, NULL, FALSE), E_FAIL);
    }

    ////
    // Generate a fresh module list
    //
    MODULE *rgModList = NULL;
    IfFailRet(_LoadModuleList(&rgModList));

    ////
    // It could be handy to have a list of modules in the trace as well.  Dump them
    //
    IfFailGo(pStream->Write((PVOID)cszModuleList, lstrlen(cszModuleList), &dwWritten));
    for(nMod = 0; rgModList[nMod].pszFullPath; nMod++)
    {
        wsprintf(szBuf, cszModuleEntry, rgModList[nMod].pszFullPath);
        IfFailGo(pStream->Write((PVOID)szBuf, lstrlen(szBuf), &dwWritten));
    }

    ////
    // And walk the stack.
    //
    scope
    {
        BOOL    fDumpCurrentThread = TRUE;  // dump the current thread out of loop?
        Thread  *pThread;

        FreezeAllThreads(); // makes the dump more reliable.

        Thread::GetThreadList(&pThread);

        while(pThread)
        {
            ////
            // If we're dumping the current thread within the loop, don't dump it below.
            //
            if(pThread->GetId() == GetCurrentThreadId())
                fDumpCurrentThread = FALSE;

            IfFailGo(_DumpThisThread(hProcess, pThread->GetHandle(), pThread->GetId(), rgModList, pStream));

            Thread *pThreadLast = pThread;
            pThread->GetNext(&pThread);
            pThreadLast->Release();
        }

        if(fDumpCurrentThread)
        {
            ////
            // Current thread wasn't found in the thread list.  Dump it now
            //
            IfFailGo(_DumpThisThread(hProcess, GetCurrentThread(), GetCurrentThreadId(), rgModList, pStream));
        }
    } // scope

Error:
    _FreeModuleList(rgModList);
    SetCursor(hOldCursor);

    return hr;
}


//=--------------------------------------------------------------------------=
// VsPrintCallstack
//=--------------------------------------------------------------------------=
// DebugPrints the current threads callstack, up to nLines lines.  If nLines
// is 0, all lines are printed
//
CLINKAGE VOID ENTRYPOINT VsPrintCallstack
(
 int nLines
 )
{
    CDebugStream strm;

    if(SUCCEEDED(DebDumpCurrentThread((IStream *)&strm, nLines)))
    {
        LPSTR psz = strm.GetBuffer();
        DEBUGPRINTF(psz);
    }
}


//
//
// Private functions
//
//


//=--------------------------------------------------------------------------=
// _DumpThisThread
//=--------------------------------------------------------------------------=
// Dumps the stack of hThread to pStream.
//
HRESULT _DumpThisThread
(
 HANDLE  hProcess,
 HANDLE  hThread,
 DWORD   dwTid,
 MODULE  *rgModList,
 IStream *pStream,
 int      nLines
 )
{
#ifdef _WIN64
    // Win64Hack (MikhailA): No stack/context information on IA64 in WINNT.H yet (as of 1/10/2000)
    return S_OK;
#else
    HRESULT   hr = NOERROR;
    DWORD	    dwWritten;
    char	    szBuf[200];
    BOOL      fFoundUs = FALSE;
    BOOL      fDumpEm  = FALSE;

    ////
    // Constant string data
    //
    const char  cszThreadHeader[] = "\r\nThread ID 0x%lX -------\r\n";
    const char  cszStackEntry[]	= "\t%s [%s]\r\n";
    const char  cszAssertLine[]	= "ASSERT:";

    wsprintf(szBuf, cszThreadHeader, dwTid);
    IfFailGo(pStream->Write(szBuf, lstrlen(szBuf), &dwWritten));

    ////
    // Stack data may not be accurate for the current thread - there's
    // only so much we can do with the stack frame wandering all over
    // the place.  If this is the current thread, print a warning message
    //
    if(dwTid == GetCurrentThreadId())
    {
        lstrcpy(szBuf, "<<<<<< Current Thread >>>>>>>\r\n");
        IfFailGo(pStream->Write(szBuf, lstrlen(szBuf), &dwWritten));
    }

    ////
    // Get the context for the thread we're messing with
    //
    CONTEXT threadcontext;
    threadcontext.ContextFlags = CONTEXT_CONTROL;
    IfFalseGo(GetThreadContext(hThread, &threadcontext), E_FAIL);

    ////
    // Setup the stackframe data
    //
    STACKFRAME stackframe;
    memset(&stackframe, 0, sizeof(stackframe));

    stackframe.AddrPC.Mode = AddrModeFlat;
    stackframe.AddrStack.Mode = AddrModeFlat;
    stackframe.AddrFrame.Mode = AddrModeFlat;

    stackframe.AddrPC.Offset = GET_PC(threadcontext);
    stackframe.AddrStack.Offset = GET_STACK(threadcontext);
    stackframe.AddrFrame.Offset = GET_FRAME(threadcontext);

    for(;;)
    {
        char szSym[MAX_PATH];

        ////
        // Get the current stack frame for this thread
        //
        if(!StackWalkAPI(IMAGE_TYPE, hProcess, hThread, &stackframe, 
            &threadcontext, NULL, NULL, NULL, NULL))
            break;

        MODULE *pMod = _FindModuleInList(rgModList, stackframe.AddrPC.Offset);

        ////
        // If the user has specified a specific number of lines, ignore anything
        // before and including US
        //
        if(nLines && !fDumpEm)
        {
            if(fFoundUs)
            {
                if(lstrcmpi(pMod->pszFilename, _szModuleName))
                    fDumpEm = TRUE;
                else
                    continue;
            }
            else
            {
                if(0 == lstrcmpi(pMod->pszFilename, _szModuleName))
                    fFoundUs = TRUE;

                continue;
            }
        }

        _GetSymbolFromAddress(stackframe.AddrPC.Offset, szSym, sizeof(szSym));

        wsprintf(szBuf, cszStackEntry, szSym, pMod->pszFilename);

        ////
        // Admittedly, this is a small hack.  We'd like to make it obvious
        // where an assert (if any) occurred, so we check the symbol address
        // against DebDispAssert.  If they match, we've got a winner
        // 
        if(!lstrcmp(szSym, "VsAssert"))
            lstrcat(szBuf, cszAssertLine);

        IfFailGo(pStream->Write(szBuf, lstrlen(szBuf), &dwWritten));

        // Want to support outputing only nLines of stack dump.  
        //
        if(nLines)
            if(0 == --nLines)
                break;
    }
Error:

    return hr;
#endif
}


//=--------------------------------------------------------------------------=
// _DumpThreadCallback
//=--------------------------------------------------------------------------=
// used to dump thread data from a callback.  We do this so we can dump
// a specific thread stack without interfering with it
//
HRESULT _DumpThreadCallback
(
 INT_PTR dwData
 )
{
    DUMPPARAMS *pdp = (DUMPPARAMS *)dwData;
    HANDLE      hProcess = GetCurrentProcess();
    HRESULT     hr = NOERROR;

    IfFalseRet(pdp->pStream, E_POINTER);

    // Win 95's too damn stupid and crashes if we try to dump the stack during
    // a process detach.  Just disallow this in general.
    //
    if(g_fShuttingDown)
    {
        DWORD dwFoo;
        pdp->pStream->Write(_szTooLateForStack, sizeof(_szTooLateForStack), &dwFoo);
        return S_OK;
    }

    ////
    // Demand load imagehlp.dll
    //
    if(!_hinstImageHlp)
    {
        IfFailRet(_QueryLoadDll(_szImageHlp, &_hinstImageHlp, _rgImageHlpFuncs, _cImageHlpFuncs));

        ////
        // Initialize imagehlp
        //
        IfFalseRet(SymInitializeAPI(hProcess, NULL, FALSE), E_FAIL);
    }

    ////
    // Generate a fresh module list
    //
    MODULE *rgModList = NULL;
    IfFailRet(_LoadModuleList(&rgModList));

    ////
    // And walk the stack.
    //
    scope
    {
        Thread  *pThread;

        FreezeAllThreads(); // makes the dump more reliable.

        // Find the thread of interest
        //
        Thread::GetThreadList(&pThread);

        while(pThread)
        {
            if(pThread->GetId() == pdp->dwThreadId)
                break;
            pThread->GetNext(&pThread);
        }

        IfFalseGo(pThread, E_FAIL);

        IfFailGo(_DumpThisThread(hProcess, pThread->GetHandle(), pThread->GetId(), rgModList, pdp->pStream, pdp->nLines));
    } // scope

    // throw a NULL terminator on there...
    //
    DWORD dwWritten;
    pdp->pStream->Write((PVOID)"\0", 1, &dwWritten);

    hr = S_OK;

Error:
    _FreeModuleList(rgModList);
    return hr;
}


//=--------------------------------------------------------------------------=
// _QueryLoadDll
//=--------------------------------------------------------------------------=
// Loads the given DLL and establishes the function pointers.  If the DLL
// can't be found this displays a message and allows the user to retry
//
static HRESULT _QueryLoadDll
(
 LPCSTR	pszDll,
 HINSTANCE	*phinst,
 DYNALINKFUNC	*rgFuncs,
 UINT		cFuncs
 )
{
    ////
    // Dll already loaded, just return
    //
    if(*phinst)
        return NOERROR;

    BOOL fSuccess = FALSE;
    while(!fSuccess)
    {
        HINSTANCE hinst = LoadLibrary(pszDll);

        if (hinst != NULL)
        {
            int iFunc;

            fSuccess = TRUE;

            for (iFunc = 0; iFunc < (int)cFuncs; ++iFunc) 
            {
                if ((*(rgFuncs[iFunc].ppfnAddr) = GetProcAddress(hinst, rgFuncs[iFunc].szFuncName))
                    == NULL)
                {
                    ////
                    // Did not successfully get function address.  
                    //
                    fSuccess = FALSE;
                    FreeLibrary(hinst);
                    break;
                }
            }
        }

        if(fSuccess)
        {
            ////
            // Hooray.  Groovy times.
            //
            *phinst = hinst;
            return NOERROR;
        }
        else
        {
            ////
            // We failed.  See if the user wants to retry.
            //
            char szMsg[MAX_PATH];
            wsprintf(szMsg, "Unable to load '%s'.  Retry?", pszDll);

            if(IDYES != MessageBox(NULL, szMsg, NULL, MB_YESNO | MB_ICONSTOP))
                break;
        }
    }
    return E_FAIL;
}


//=--------------------------------------------------------------------------=
// _LoadModuleList
//=--------------------------------------------------------------------------=
// Preloads the module list with all the modules currently in the process.
// If this fails, the contents of ppModList are unchanged.  Otherwise, 
// ppModList will point to a new module list.
// 
static HRESULT	_LoadModuleList
(
 MODULE **ppModList
 )
{
    HRESULT       hr = NOERROR;
    MODULE        *rgMods = NULL;
    UINT	        cMods = 0;
    HANDLE        hProcess = GetCurrentProcess();
    OSVERSIONINFO osVer;

    ////
    // Are we running under NT or Win 95?  Unfortunately there is no
    // unified codepath for both OS's
    //
    osVer.dwOSVersionInfoSize = sizeof(osVer);
    if(!GetVersionEx(&osVer))
        return E_OUTOFMEMORY;

    if(VER_PLATFORM_WIN32_NT == osVer.dwPlatformId)  // NT codepath
    {
        DWORD   cbMods;
        HMODULE *rgHMods = NULL;

        IfFailRet(_QueryLoadDll(_szPsapi, &_hinstPsapi, _rgPsapiFuncs, _cPsapiFuncs));

        ////
        // Load a list of module handles.
        //
        IfFalseRet(EnumProcessModulesAPI(hProcess, NULL, 0, &cbMods), E_FAIL);

        rgHMods = (HMODULE *)DebugAlloc(cbMods);
        IfFalseRet(rgHMods, E_OUTOFMEMORY);

        IfFalseGoto(EnumProcessModulesAPI(hProcess, rgHMods, cbMods, &cbMods), E_FAIL, NTError);

        ////
        // Now get some info about each handle, storing it in our very own MODULE struct
        //
        rgMods = (MODULE *)DebugAlloc(sizeof(MODULE) * ((cbMods / sizeof(HMODULE)) + 1));
        IfFalseGoto(rgMods, E_OUTOFMEMORY, NTError);

        for(cMods = 0; cMods < (cbMods / sizeof(HMODULE)); cMods++)
        {
            char	  szModName[MAX_PATH];
            MODULEINFO  mi;

            if(!GetModuleFileNameExAPI(hProcess, rgHMods[cMods], szModName, sizeof(szModName)))
                lstrcpy(szModName, _cszUnknownModule);

            rgMods[cMods].pszFullPath = (LPSTR)DebugAlloc(lstrlen(szModName) + 1);
            IfFalseGoto(rgMods[cMods].pszFullPath, E_OUTOFMEMORY, NTError);

            lstrcpy(rgMods[cMods].pszFullPath, szModName);

            ////
            // Find the filename from this full pathname
            //
            rgMods[cMods].pszFilename = _GetFilenamePart(rgMods[cMods].pszFullPath);

            ////
            // Now get the address range of this module
            //
            IfFalseGoto(GetModuleInformationAPI(hProcess, rgHMods[cMods], &mi, sizeof(mi)), E_FAIL, NTError);
            rgMods[cMods].dwBaseAddr = (INT_PTR)mi.lpBaseOfDll;
            rgMods[cMods].dwEndAddr = (INT_PTR)mi.lpBaseOfDll + mi.SizeOfImage - 1;
        }

        ////
        // We created an extra MODULE struct at the end to act as a sentinel
        //
        memset(&rgMods[cMods], 0, sizeof(MODULE));

NTError:
        if(rgHMods) DebugFree(rgHMods);
    }
    else	// Win 95 codepath
    {
        MODULEENTRY32 me;
        BOOL	  fContinue;
        HANDLE	  hSnap = NULL;

        IfFailRet(_QueryLoadDll(_szToolHlp, &_hinstToolHlp, _rgToolHlpFuncs, _cToolHlpFuncs));

        ////
        // Create a snapshot of the modules in memory.  We don't do this above, but it's
        // not as big a deal here because we typically have all threads frozen when we're
        // doing this.
        //
        hSnap = CreateToolhelp32SnapshotAPI(TH32CS_SNAPMODULE, GetCurrentProcessId());
        IfFalseRet(hSnap, E_OUTOFMEMORY);

        ////
        // Find out how many modules we've got
        //
        fContinue = Module32FirstAPI(hSnap, &me);

        while(fContinue)
        {
            cMods++;
            fContinue = Module32NextAPI(hSnap, &me);
        }

        ////
        // Now really get 'em
        //
        rgMods = (MODULE *)DebugAlloc(sizeof(MODULE) * (cMods + 1));
        IfFalseGoto(rgMods, E_OUTOFMEMORY, W95Error);

        cMods = 0;
        fContinue = Module32FirstAPI(hSnap, &me);

        while(fContinue)
        {
            ////
            // The filename of the module
            //
            rgMods[cMods].pszFullPath = (LPSTR)DebugAlloc(lstrlen(me.szModule) + 1);
            IfFalseGoto(rgMods[cMods].pszFullPath, E_OUTOFMEMORY, W95Error);

            lstrcpy(rgMods[cMods].pszFullPath, me.szModule);
            rgMods[cMods].pszFilename = _GetFilenamePart(rgMods[cMods].pszFullPath);

            ////
            // And it's address info
            //
            rgMods[cMods].dwBaseAddr = (INT_PTR)me.modBaseAddr;
            rgMods[cMods].dwEndAddr = (INT_PTR)me.modBaseAddr + me.modBaseSize - 1;

            cMods++;

            fContinue = Module32NextAPI(hSnap, &me);
        }

        ////
        // We created an extra MODULE struct at the end to act as a sentinel
        //
        memset(&rgMods[cMods], 0, sizeof(MODULE));

W95Error:
        if(hSnap) CloseHandle(hSnap);
    }

    ////
    // Did we succeed?
    //
    if(SUCCEEDED(hr))
    {
        ////
        // Load symbols for all the modules.  We don't care if this isn't successful
        // for everyone.
        //
        while(cMods--)
        {
            SymLoadModuleAPI(hProcess, NULL, rgMods[cMods].pszFullPath, 
                rgMods[cMods].pszFilename, rgMods[cMods].dwBaseAddr, 
                (LONG32) (rgMods[cMods].dwEndAddr - rgMods[cMods].dwBaseAddr + 1));
        }

        _FreeModuleList(*ppModList);
        *ppModList = rgMods;
    }
    else
    {
        _FreeModuleList(rgMods);
    }

    return hr;
}


//=--------------------------------------------------------------------------=
// _GetFilenamePart
//=--------------------------------------------------------------------------=
// Returns a pointer within the given path to the filename portion of 
// the path
//
static LPSTR _GetFilenamePart
(
 LPSTR pszFullPath
 )
{
    LPSTR pszName = pszFullPath;
    LPSTR pszLastSlash = NULL;

    while(*pszName)
    {
        if('\\' == *pszName)
            pszLastSlash = pszName;

        pszName = CharNext(pszName);
    }

    return(pszLastSlash ? CharNext(pszLastSlash) : pszFullPath);
}


//=--------------------------------------------------------------------------=
// _FreeModuleList
//=--------------------------------------------------------------------------=
// Frees the given module list.  This can be passed a NULL ptr.
//
static VOID _FreeModuleList
(
 MODULE  *rgModList
 )
{
    if(rgModList)
    {
        for(int nMod = 0; rgModList[nMod].pszFullPath; nMod++)
            DebugFree(rgModList[nMod].pszFullPath);

        DebugFree(rgModList);
    }
}


//=--------------------------------------------------------------------------=
// _FindModuleInList
//=--------------------------------------------------------------------------=
// Searches the given module list for a module containing the given address.
// If no matching module can be found, this will return a default module
// pointer.
//
static MODULE *_FindModuleInList
(
 MODULE  *rgModList,
 INT_PTR   dwAddr
 )
{
    static  MODULE  sDefaultMod = {(LPSTR)_cszUnknownModule, (LPSTR)_cszUnknownModule, 0, 0};

    for(int nMod = 0; rgModList[nMod].pszFullPath; nMod++)
    {
        if(dwAddr >= rgModList[nMod].dwBaseAddr &&
            dwAddr <= rgModList[nMod].dwEndAddr)
            return &rgModList[nMod];
    }

    return &sDefaultMod;
}


//=--------------------------------------------------------------------------=
// _GetSymbolFromAddress
//=--------------------------------------------------------------------------=
// Copies a symbol name corresponding to the given address into pszName
//
static VOID _GetSymbolFromAddress
(
 INT_PTR dwAddr, 
 LPSTR pszName,  
 UINT	cbName
 )
{
    const UINT cbMaxName = 255;
    BOOL  fSuccess = FALSE;

    DWORD	dwDisp;

    PIMAGEHLP_SYMBOL pSym = (PIMAGEHLP_SYMBOL)DebugAlloc(sizeof(IMAGEHLP_SYMBOL) + cbMaxName);
    if(!pSym) goto Error;

    pSym->SizeOfStruct = sizeof(IMAGEHLP_SYMBOL);
    pSym->MaxNameLength = cbMaxName;
    if(SymGetSymFromAddrAPI(GetCurrentProcess(), dwAddr, &dwDisp, pSym))
    {
        ////
        // Got the symbol, now format it nicely.
        //
        SymUnDNameAPI(pSym, pszName, cbName);
        fSuccess = TRUE;
    }

Error:
    if(pSym) DebugFree(pSym);

    ////
    // If we didn't get a symbol, format an address 
    //
    if(!fSuccess)
    {
        const char szUnknownFmt[] = "(0x%.8lX)";
        char  szOoog[30];

        wsprintf(szOoog, szUnknownFmt, dwAddr);
        lstrcpyn(pszName, szOoog, cbName);
        return;
    }
}
*/
#endif //0
