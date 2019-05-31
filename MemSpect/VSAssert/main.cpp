//=--------------------------------------------------------------------------=
// Main.Cpp
//=--------------------------------------------------------------------------=
// DLL Main code for VS assertions.  Also holds global debugging stuff
//=--------------------------------------------------------------------------=
// Copyright (c) 1997, Microsoft Corporation
//			All Rights Reserved
// Information Contained Herein Is Proprietary and Confidential.
//=--------------------------------------------------------------------------=

#include "Pch.H"
#include "vsassert.h"
#include "Main.H"
#include "Util.H"
#include "Mem.H"
#include "Detours30\detours.h"

//=--------------------------------------------------------------------------=
// We're the ones implementing DebugAlloc and the like, so we need access
// to unprotected heap allocation routines
//
#undef HeapAlloc
#undef HeapReAlloc
#undef HeapFree
#undef HeapSize

//=--------------------------------------------------------------------------=
// Externals
//=--------------------------------------------------------------------------=
extern void VsInitDebugThread();
extern void VsTerminateDebugThread();
extern bool DepersistOptions();

//=--------------------------------------------------------------------------=
// Global data
//=--------------------------------------------------------------------------=
int	  g_dfAssertFlags;      // how to output asserts
int	  g_dfPrintfFlags;      // how to output DebugPrintf
int	  g_dfEnabled;	        // bitmask specifying which flags are valid
HINSTANCE g_hinstDll;	        // The DLL instance handle
BOOL      g_fShuttingDown;      // have we received a process detach?

//=--------------------------------------------------------------------------=
// Private data
//=--------------------------------------------------------------------------=


static LONG             _lDebugHeapRefcnt;
HANDLE           g_hDebugHeap=0;
DWORD            _dwTls = INVALID_TLS;
DWORD           g_dwMainThread = 0; // in injection case, this is the injected thread
static CRITICAL_SECTION _csDebug;
static BOOL             _fcsInit = FALSE;
static DWORD            _dwThreadAllocCount;






//=--------------------------------------------------------------------------=
// DllMain
//=--------------------------------------------------------------------------=
// Main entrypoint, of course.  Here we track threads coming and going from
// the DLL
BOOL WINAPI DllMain
(
 HINSTANCE hInst,
 ULONG     ulReason,
 LPVOID
 )
{
    auto retval = true;
    auto ClearTlsValue = []()        
        {
            THREADGLOBAL *ptr ;
            if ((ptr = (THREADGLOBAL *)TlsGetValue(_dwTls)) != 0)
            {
                if (ptr == TLS_THREAD_DETACHED) // this thread has already detached
                {
                    ptr = (CThread *)1;//set bpt on this line
                }
                else
                {
                    TlsSetValue(_dwTls, TLS_THREAD_DETACHED);
                    _dwThreadAllocCount -= 2;
                }
            }
        };

    switch(ulReason)
    {
    case DLL_PROCESS_ATTACH:
        g_fShuttingDown = FALSE;
#if MSDEBUG
//		MessageBoxA(0, "Attach Debugger", "", 0);
#endif
		DetourRestoreAfterWith();
        InitializeCriticalSection(&_csDebug);
        _fcsInit = TRUE;

        if(INVALID_TLS == _dwTls)
        {
            g_dwMainThread = GetCurrentThreadId();
            _dwTls = TlsAlloc();
        }

        ASSERT(INVALID_TLS != _dwTls, "Out of TLS space");
        if(INVALID_TLS == _dwTls)
            return FALSE;

        g_hinstDll = hInst;

        ////
        // Set appropriate assert/debug printf defaults so we can
        // assert before we're initted
        //
        g_dfEnabled = 0xFFFFFFFF;
        g_dfAssertFlags = DF_DEFAULTASSERTOPTIONS;
        g_dfPrintfFlags = DF_DEFAULTPRINTOPTIONS;
        


        // read the options
        retval = DepersistOptions();
//        GdiTrace_Init();
        //
        // FALL THROUGH TO THREAD ATTACH
        //

    case DLL_THREAD_ATTACH:
        // In rare cases we can get a THREAD_ATTACH before we
        // get a process attach.  So, we check here
        //
        ASSERT(INVALID_TLS != _dwTls, "Out of TLS space and we can't do nuttin'");
        {
            CThread *pThread;
            if ((pThread = (CThread *) TlsGetValue(_dwTls)) == 0 || pThread == TLS_THREAD_DETACHED) // if we haven't set it yet
            {
#if MSDEBUG
                VSASSERT(pThread != TLS_THREAD_DETACHED,"TlsValue already used for thread ?"); // can this happen if the same thread is reused?
#endif MSDEBUG
                pThread = GetThreadData(/*fCreate=*/ true); // Force TLS value to be set
            }
        }
        break;

    case DLL_PROCESS_DETACH:
        {
            //char szBuf[1000+MAX_PATH];
            //sprintf_s(szBuf, "DLL_PROCESS_DETACH in target Process. You can now attach a debugger\r\n%s Pid = %d\r\n Cause a DebugBreak() ?", g_szProcessExeFullPathName, GetCurrentProcessId());
            //if (MessageBoxA(0,szBuf,"MemSpect shutdown",MB_YESNO) == IDYES)
            //{
            //    _asm int 3;
            //}
        }
        // We are now shutting down and need to note that.
        // If we assert here, say during heap destruction we'll 
        // need to cleanup lingering HWNDs to make sure we can 
        // actually show the assert dialog.
        g_fShuttingDown = TRUE;

		if (g_CodeMarkerActions)
		{
			g_CodeMarkerActions->freemem();
		}
		if (g_MapDynStackSymNamesWrapper)
		{
			g_MapDynStackSymNamesWrapper->freemem();
		}


        // clean up our TLS for the primary thread
        //
        CThread::Detach();

//        GdiTrace_Terminate();

        ////
        // Destroy the default heap
        //
        DestroyDefaultHeap();

        ClearTlsValue();


        TlsFree(_dwTls);
        _dwTls = INVALID_TLS;

        DeleteCriticalSection(&_csDebug);
        _fcsInit = FALSE;

        // This is a minor hack.  We cannot leagally destroy
        // the debug heap if there are outstanding allocations.  However,
        // we have two outstanding allocations for each thread that
        // doesn't get terminated by the time we get process detach.  
        // So, we keep track of these and then subtract them off
        // of the debug heap refcount.  This way, if there are
        // real memory leaks that caused the debug heap to leak,
        // we don't destroy them (because this could cause a crash
        // in shutdown), however, if the leaks are caused by threads
        // still hanging around, we can nuke them.
        //
        _lDebugHeapRefcnt -= _dwThreadAllocCount;
        
        if (g_hDebugHeap && _lDebugHeapRefcnt == 0)
        {
            HeapDestroy(g_hDebugHeap);
            g_hDebugHeap = NULL;
        }

        break;

    case DLL_THREAD_DETACH:
        CThread::Detach();

        ClearTlsValue();
        
        break;
    }

    return retval;
}



//=--------------------------------------------------------------------------=
// DebugAlloc
//=--------------------------------------------------------------------------=
// Memory allocator for debugging.  By using our own allocator, we can 
// allocate whenever we want.  Also, this can be called BEFORE the allocator
// has actually been setup (so asserts can happen before debug initialization
// happens.)
//
PVOID DebugAlloc
(
 SIZE_T cb
 )
{
    //VSASSERT(cb < 200000,"Internal alloc > 200000");
    
    ////
    // Get our heap, if we need to.  This is a pretty extreme way to
    // manage a heap.  DebugInit creates the heap and initializes the
    // refcount to 1, so normally all we do is increment a variable.
    //
    // The heap creation here allows us to allocate before and after
    // the debugging stuff has been setup
    //
    if(_fcsInit) 
        EnterCriticalSection(&_csDebug);
    if(!_lDebugHeapRefcnt++)
    {
        // as this DLL gets initialized, some globals aren't init'd before others. 
        // some static classes are init'd, which can happen before Real_* functions are init'd
        // if we have no detours, these are the same
        if (Real_HeapCreate == 0)
        {
            g_hDebugHeap = HeapCreate(0, 0, 0);
        }
        else
        {
            g_hDebugHeap = Real_HeapCreate(0, 0, 0);
        }
        if(!g_hDebugHeap)
        {
            DWORD dwErr = GetLastError();
            char  szErr[80];
            sprintf_s(szErr, _countof ( szErr ), "Failed to create debug heap.  Error code %lu", dwErr);
            FAIL(szErr);

            if(_fcsInit) 
                LeaveCriticalSection(&_csDebug);
            _lDebugHeapRefcnt--;
            return NULL;
        }
    }
    if(_fcsInit) 
        LeaveCriticalSection(&_csDebug);
    PVOID ptr;
    CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
                    // in particular: a heapalloc can cause a virtualalloc
    if (Real_RtlAllocHeap == 0)
    {
        ptr = HeapAlloc(g_hDebugHeap, 0, cb);
    }
    else
    {
        ptr = Real_RtlAllocHeap(g_hDebugHeap, 0, cb);
    }
    return ptr;
}


//=--------------------------------------------------------------------------=
// DebugRealloc
//=--------------------------------------------------------------------------=
// Debug reallocator.  
//
PVOID DebugRealloc
(
 PVOID pvOld,
 SIZE_T cb
 )
{
    ////
    // the heap refcound doesn't change here
    //
    PVOID ptr;
    if (Real_HeapReAlloc ==0)
    {
        ptr = HeapReAlloc(g_hDebugHeap, 0, pvOld, cb);
    }
    else
    {
        ptr = Real_HeapReAlloc(g_hDebugHeap, 0, pvOld, cb);
    }
    return ptr;
}


//=--------------------------------------------------------------------------=
// DebugFree
//=--------------------------------------------------------------------------=
// And of course, what's George without Gracie?
//
VOID  DebugFree
(
 PVOID pv
 )
{
    if (pv) // don't touch refcnt if null: else early heap destroy
    {
        CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
                    // in particular: a heapalloc can cause a virtualalloc
        if (Real_RtlFreeHeap == 0)
        {
            HeapFree(g_hDebugHeap, 0, pv);
        }
        else
        {
            Real_RtlFreeHeap(g_hDebugHeap, 0, pv);
        }

        ////
        // Take down the heap if we reach zero
        //
        if(_fcsInit) 
            EnterCriticalSection(&_csDebug);
        if(0 == --_lDebugHeapRefcnt)
        {
            HeapDestroy(g_hDebugHeap);
            g_hDebugHeap = NULL;
        }
        if(_fcsInit) 
            LeaveCriticalSection(&_csDebug);
    }
}


//=--------------------------------------------------------------------------=
// DebugSize
//=--------------------------------------------------------------------------=
// Debug version of HeapSize
//
INT_PTR DebugSize
(
 PVOID pv
 )
{
    INT_PTR ptr = 0;
    
    if(g_hDebugHeap)
    {
        
        if (Real_HeapSize == 0)
        {
            ptr = HeapSize(g_hDebugHeap, 0, pv);
        }
        else
        {
            ptr = Real_HeapSize(g_hDebugHeap, 0, pv);
        }
    }
    return ptr;
}


//=--------------------------------------------------------------------------=
// GetThreadData
//=--------------------------------------------------------------------------=
// Returns a pointer to our TLS block
//
THREADGLOBAL *GetThreadData(bool fCreate/* = false*/)
{
    THREADGLOBAL *ptd = NULL;

    if(_dwTls != INVALID_TLS)
    {
        // note: http://tkbgitvstfat01:8080/WorkItemTracking/WorkItem.aspx?artifactMoniker=911346
        // TlsGetValue calls SetLastError which destroys the prior value
        auto saveError = GetLastError();
        ptd = (THREADGLOBAL *)TlsGetValue(_dwTls);

        if (ptd == NULL || ptd == TLS_THREAD_DETACHED)
        {
            if (ptd == TLS_THREAD_DETACHED)
            {
                // the thread already called DLLMain THREAD_DETACH, we've already destroyed CThread, and don't want to detour
                // and we don't want to create a CThread (what would it's lifetime be?)
                // this is a source of false positives for leaks
                ptd = NULL; 
            }
            else
            {
                if (fCreate)
                {
                    CThread::Attach();
                    _dwThreadAllocCount += 2;
                }
            }
        }
        SetLastError(saveError);
    }

    return ptd;
}
