//=--------------------------------------------------------------------------=
// Util.H
//=--------------------------------------------------------------------------=
// Utility code for the VS debug interfaces
//=--------------------------------------------------------------------------=
// Copyright (c) 1997, Microsoft Corporation
//			All Rights Reserved
// Information Contained Herein Is Proprietary and Confidential.
//=--------------------------------------------------------------------------=

#ifndef _INC_UTIL_H
#define _INC_UTIL_H

#include "main.h"
#undef new

//=--------------------------------------------------------------------------=
// stuff to persist data to our ini file
//
BOOL VsSetOptionsFileName(_In_opt_ char * pszFileName);
char *VsGetOptionsFileName();
int  VsGetProfileInt(LPCTSTR pszAppName, LPCTSTR pszKey, int nDefault);
void VsWriteProfileInt(LPCTSTR pszAppName, LPCTSTR pszKey, int nValue);
void VsGetProfileString(LPCTSTR pszAppName, LPCTSTR pszKey, LPCTSTR pszDefault, _Out_z_cap_(dwReturnSize) LPTSTR pszReturn, DWORD dwReturnSize);
BOOL VsWriteProfileString(LPCTSTR pszAppName, LPCTSTR pszKey, LPCTSTR pszString);

BOOL IsUserInteractive(); // see if runnning from a service

//=--------------------------------------------------------------------------=
// Flags of interest
//
enum SYSFLAG
  {
  SYS_Win95 = 0,
  SYS_WinNT
  };

BOOL GetSysFlag(SYSFLAG flag);

#ifndef RTL_CRITICAL_SECTION_FLAG_FORCE_DEBUG_INFO
#define RTL_CRITICAL_SECTION_FLAG_FORCE_DEBUG_INFO 0x10000000
#endif
//=--------------------------------------------------------------------------=
// CMutex.  This is a simple mutex that is used to wrap a critical section
// object.
//
class CMutex
  {
  public:
    CMutex()
    { 
        DWORD dwFlags = RTL_CRITICAL_SECTION_FLAG_FORCE_DEBUG_INFO;
        // on some OS, this flags doesn't exist (W7)
        if (!InitializeCriticalSectionEx(&m_cs, 0, dwFlags))
        {
            dwFlags = 0;
            InitializeCriticalSectionEx(&m_cs, 0, dwFlags);
        }
        m_fCrit = TRUE; 
    }
    ~CMutex()         
    { 
        DeleteCriticalSection(&m_cs); 
        m_fCrit = FALSE; 
    }

    HRESULT Request() { 
        if(m_fCrit) 
            EnterCriticalSection(&m_cs); 
        return NOERROR; 
    }
    HRESULT Release() { 
        if(m_fCrit) 
            LeaveCriticalSection(&m_cs); 
        return NOERROR; 
    }

	CRITICAL_SECTION *GetCriticalSectionDangerous()
	{
		return &m_cs;
	}

  private:
    CRITICAL_SECTION m_cs;
    BOOL m_fCrit;
  };

#define MAXTHREADS 200 // if we go over this, then we're dead!

//=--------------------------------------------------------------------------=
// CLock.  This is a locking class for CMutex - you pass in a CMutex to
// the constructor and it locks.  The mutex is released when CLock goes out
// of scope.  Use the macro LOCK_MUTEX below to wrap the locking class.
//
class CLock
  {
  public:
    CLock(CMutex *pmxs) : m_pmxs(pmxs)  { 
        pmxs->Request(); 
    }
    ~CLock() { 
        m_pmxs->Release(); 
    }

  private:
    CMutex *m_pmxs;
  };

#define LOCK_MUTEX(mxs) CLock cl(mxs)
#define DECLARE_MUTEX(mxs) CMutex mxs

//=--------------------------------------------------------------------------=
// Support for tracking threads that enter/leave this DLL.  We use this
// for debug dumping, assertions, and all round nastiness.
//



class CThread
{
    friend class CDebFreezeAllThreads;
  public:
    CThread(); //must have constructor, so the operator new override doesn't use default ctor which memsets to 0 AND 
                //    so can create an array of these guyes
    HANDLE  GetHandle() { 
        return m_hThread; 
    }
    DWORD   GetId()     { 
        return m_dwThreadId; 
    }

    PVOID operator new(size_t size);

    void  operator delete(PVOID pv);
    ULONG GetGlobalPassCount()
    {
        return m_ulGlobalPassCount;
    }
#if MSDEBUG
    static void VerifyCThread();
#else
    #define VerifyCThread()
#endif
  public:
    static  void    Detach(DWORD dwThreadId = GetCurrentThreadId());
    static  CThread *   Attach(); //called for DLL_THREAD_ATTACH
    static  CThread *   Attach(DWORD dwThreadid); // if we discover a thread that wasn't added from DLL_THREAD_ATTACH (could have been before we got into process)

  private:
    static  CMutex  m_mxsListLock;
    static  CThread  *s_pRootThreadList; // static root points to next CThread in either the Freelist or the RootList
    // we need a place to put dynamically created per thread data that doesn't call any heap functions (which can recur to VirtualAlloc, etc.)
    static bool s_ThreadClassInitialized;
    static CThread s_aThreads[MAXTHREADS]; 
    static CThread *s_FreeThreadList;



    CThread  *m_pNext;   //points to next CThread in either the Freelist or the RootList
    DWORD   m_dwThreadId;
public:
    BYTE     m_ulDisableTrace;
private:
    HANDLE  m_hThread; // handle to thread: only used when thread is frozen. 
    ULONG m_ulGlobalPassCount; // at which thread was created (added to list)

};


#define THREADGLOBAL CThread

THREADGLOBAL *GetThreadData(bool fCreate = false);




//=--------------------------------------------------------------------------=
// CDebFreezeAllThreads.  This will automatically freeze all threads 
// except the debug thread and the current thread.  Use the
// FreezeAllThreads macro for convenience.
//
class CDebFreezeAllThreads
{
  public:
    CDebFreezeAllThreads();
    ~CDebFreezeAllThreads();

    CThread	   **m_rgThreads;
    int		   m_nThreads;
  private:
    CThread	   *m_rgThreadsStatic[MAXTHREADS];

  };

#define FreezeAllThreads() CDebFreezeAllThreads freeze

extern CDebFreezeAllThreads * g_pCDebFreezeAllThreads;


//=--------------------------------------------------------------------------=
// CDebugStream.  This is a simplified implementation if IStream and is used
// to out put all sorts of debug stuff.
//
class CDebugStream : public IStream
  {
  public:
    // IUnknown methods
    //
    STDMETHODIMP QueryInterface(REFIID riid, PVOID *ppv)
      {
      if(riid == IID_IUnknown || riid == IID_IStream)
        {
        *ppv = (IStream *)this;
        AddRef();
        return S_OK;
        }
      *ppv = NULL;
      return E_NOINTERFACE;
      }

    STDMETHODIMP_(ULONG) AddRef() { return ++m_cRefs; }
    STDMETHODIMP_(ULONG) Release() { return --m_cRefs; }

    // IStream methods
    //
    STDMETHOD(Read)(PVOID pv, ULONG cb, ULONG *pcb);
    STDMETHOD(Write)(void const *pv, ULONG cb, ULONG *pcb);
    STDMETHOD(Seek)(LARGE_INTEGER move, DWORD dwOrigin, ULARGE_INTEGER *pmoved);
    STDMETHODIMP SetSize(ULARGE_INTEGER) { return E_NOTIMPL; }
    STDMETHODIMP CopyTo(IStream *, ULARGE_INTEGER, ULARGE_INTEGER *, ULARGE_INTEGER *) { return E_NOTIMPL; }
    STDMETHODIMP Commit(DWORD) { return E_NOTIMPL; }
    STDMETHODIMP Revert() { return E_NOTIMPL; }
    STDMETHODIMP LockRegion(ULARGE_INTEGER, ULARGE_INTEGER, DWORD) { return E_NOTIMPL; }
    STDMETHODIMP UnlockRegion(ULARGE_INTEGER, ULARGE_INTEGER, DWORD) { return E_NOTIMPL; }
    STDMETHODIMP Stat(STATSTG *, DWORD) { return E_NOTIMPL; }
    STDMETHODIMP Clone(IStream **) { return E_NOTIMPL; }

    // Ways to create
    //
    CDebugStream()
      {
      m_cRefs = 1;
      m_dwPos = 0;
      m_dwSize = 0;
      m_psz = NULL;
      m_fReallocOk = TRUE;
      }

    CDebugStream(_In_z_ LPSTR psz, DWORD dwSize)
      {
      m_cRefs = 1;
      m_dwPos = 0;
      m_dwSize = dwSize;
      m_psz = psz;
      m_fReallocOk = FALSE;
      }

    // How to get the underlying buffer
    //
    LPSTR GetBuffer() { return m_psz; }

  private:
    ULONG m_cRefs;
    DWORD m_dwSize;
    DWORD m_dwPos;
    LPSTR m_psz;
    BOOL  m_fReallocOk;
  };

#endif // _INC_UTIL_H
