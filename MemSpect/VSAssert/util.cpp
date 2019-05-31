//=--------------------------------------------------------------------------=
// Util.Cpp
//=--------------------------------------------------------------------------=
// Utility code for the VS debug interfaces
//=--------------------------------------------------------------------------=
// Copyright (c) 1997, Microsoft Corporation
//			All Rights Reserved
// Information Contained Herein Is Proprietary and Confidential.
//=--------------------------------------------------------------------------=

#include "Pch.H"
#include "vsassert.h"
#include "Main.H"
#include "Util.H"
#include "mem.h"
#include "tlhelp32.h"
//=--------------------------------------------------------------------------=
// Global data
DEFINE_SWITCH(_fFreezeThreads, "VsAssert", "Freeze all threads during asserts (may deadlock)");

//=--------------------------------------------------------------------------=
// Private data
static char _szPersistFile[MAX_PATH] = "";

//
//
// CThread   - this is a simple class to track a thread.
//
//


// We must track each thread that comes and goes from the DLL.  Why?  Because
// our asserts can suspend all outside threads, and we need to know the list
//
CThread *CThread::s_pRootThreadList;
CMutex CThread::m_mxsListLock;

bool CThread::s_ThreadClassInitialized = 0;
CThread CThread::s_aThreads[MAXTHREADS]; 
CThread *CThread::s_FreeThreadList;

PVOID CThread::operator new(size_t size)
{
	CThread *pThread;
	VSASSERT(size == sizeof(CThread),"size mismatch in CThread operator new");
	/*
	pThread = reinterpret_cast<CThread*>( DebugAlloc(size));
	/*/
	LOCK_MUTEX(&m_mxsListLock);
	if (!s_ThreadClassInitialized)
	{
		s_ThreadClassInitialized=true;
		s_pRootThreadList = 0;  //nothing on list initially
		ZeroMemory(s_aThreads, sizeof(s_aThreads));
		s_FreeThreadList = pThread = &s_aThreads[0];
		for (int i = 0 ; i < MAXTHREADS ; i++, pThread++) // initialize freelist
		{
			if (i == MAXTHREADS-1)
			{
				pThread->m_pNext = 0;
			}
			else
			{
				pThread->m_pNext = &pThread[1];
			}
		}
	}
	pThread = s_FreeThreadList; //get 1st one in the freelist
	if (pThread)
	{
		s_FreeThreadList = pThread->m_pNext; // adjust freelist
	}
	else
	{
		/*
		When injecting, the TLS slots might need to expand, causing a recursion to HeapAlloc(ProcHeap), causing infinite recursion
		MemSpectDll.dll!VsAssert(const char * pszMsg, const char * pszAssert, const char * pszFile, unsigned int line, int * pfDisableAssert)  Line 501	C++
		MemSpectDll.dll!CThread::operator new(unsigned int size)  Line 74 + 0x36 bytes	C++
		MemSpectDll.dll!CThread::Attach()  Line 166 + 0x7 bytes	C++
		MemSpectDll.dll!GetThreadData(bool fCreate)  Line 465	C++
		MemSpectDll.dll!CDisableTrace::CanDetour()  Line 2526 + 0x7 bytes	C++
		MemSpectDll.dll!Mine_RtlAllocHeap(void * hHeap, unsigned long dwFlags, unsigned long dwBytes)  Line 3521 + 0xb bytes	C++
		>	KernelBase.dll!TlsSetValue(unsigned long dwTlsIndex, void * lpTlsValue)  Line 1983	C
		MemSpectDll.dll!CThread::Attach()  Line 178	C++
		MemSpectDll.dll!GetThreadData(bool fCreate)  Line 465	C++
		MemSpectDll.dll!CDisableTrace::CanDetour()  Line 2526 + 0x7 bytes	C++
		MemSpectDll.dll!Mine_RtlAllocHeap(void * hHeap, unsigned long dwFlags, unsigned long dwBytes)  Line 3521 + 0xb bytes	C++


		*/
		VSASSERT(false,"MaxThreads exceeeded");
	}
	//*/
	return pThread;
};

CThread::CThread()
{
	m_pNext = s_pRootThreadList; // add to linklist
	s_pRootThreadList = this; 
	m_ulGlobalPassCount = g_ulGlobalPassCount;
	m_hThread = 0;
	m_ulDisableTrace = 0;
	VerifyCThread();
}

void CThread::operator delete(PVOID pv)
{
	/*/
	DebugFree(pv);
	/*/
	LOCK_MUTEX(&m_mxsListLock);
	CThread *pThreadToDelete = reinterpret_cast<CThread*>(pv);
	CThread *pThread;
	// remove from live list, if found
	CThread *pPrev = 0;
	for (pThread = s_pRootThreadList ; pThread ; pThread=pThread->m_pNext)
	{
		if (pThread == pThreadToDelete)
		{
			if (pPrev)
			{
				pPrev->m_pNext = pThreadToDelete->m_pNext; // splice out
			}
			else
			{// the root is being deleted, so adjust the root
				s_pRootThreadList = pThreadToDelete->m_pNext; // splice out from root
			}
			break;
		}
		pPrev = pThread;
	}
	// now add the node to the freelist
	pThreadToDelete->m_pNext = s_FreeThreadList;
	s_FreeThreadList = pThreadToDelete;
	VerifyCThread();
	//*/
}

////=--------------------------------------------------------------------------=
//// CThread::Release
////=--------------------------------------------------------------------------=
//// Release call for a thread object
////
//ULONG CThread::Release
//(
// )
//{
//    if(0 == InterlockedDecrement(&m_cRefs))
//    {
//        CThread *pThread = this;
//        CloseHandle(m_hThread);
//        delete this;
//        ASSERT(s_pRootThreadList != pThread, "Should have already been removed from the threadlist");
//        VerifyCThread();
//        return 0;
//    }
//    return (ULONG)m_cRefs;
//}
//

//=--------------------------------------------------------------------------=
// CThread::Attach
//=--------------------------------------------------------------------------=
// Attaches the current thread to the threadlist
//
// static
CThread * CThread::Attach()
{
	LPVOID pv =TlsGetValue(_dwTls); 
	VSASSERT(pv == 0,"tls already set for CThread?");

	LOCK_MUTEX(&m_mxsListLock);
	VerifyCThread();
	CThread *pThread;
	DWORD   dwThreadId = GetCurrentThreadId();
	// first check to see if an entry has already been created, but the TLS is 0 (via Attach(dwThreadId)
	for (pThread = CThread::s_pRootThreadList ; pThread ; pThread = pThread->m_pNext)
	{
		if (pThread->m_dwThreadId == dwThreadId)
		{
			TlsSetValue(_dwTls, pThread);
			break;
		}
	}
	if (!pThread)
	{
		pThread = new CThread();//hThread, dwThreadId, s_pRootThreadList, g_ulGlobalPassCount)
		if(pThread) 
		{
			pThread->m_dwThreadId = dwThreadId;
			VSASSERT(pThread->m_hThread == 0,"hThread != 0?");
			TlsSetValue(_dwTls, pThread);
		}
	}
	return pThread;
}
//=--------------------------------------------------------------------------=
// CThread::Attach
//=--------------------------------------------------------------------------=
// Attaches the specified thread to the threadlist. It has already been determined not to exist
//
// static
CThread * CThread::Attach(DWORD dwThreadId)
{
	LOCK_MUTEX(&m_mxsListLock);
	VerifyCThread();
	CThread *pThread = new CThread();//hThread, dwThreadId, s_pRootThreadList, g_ulGlobalPassCount)
	if(pThread) 
	{
		pThread->m_dwThreadId = dwThreadId;
		pThread->m_ulGlobalPassCount = g_ulGlobalPassCount;
		// the TLS value will be set the next time CDisableTrace->GetThreadData is called
	}
	return pThread;
}



//=--------------------------------------------------------------------------=
// CThread::Detach
//=--------------------------------------------------------------------------=
// Removes the current thread from the threadlist.  We have to be very
// careful here because we don't do any sort of locking - we must be
// fairly autonomous here.
//
void CThread::Detach(DWORD dwThreadId)
{
	CThread *pThread, *pThreadPrev;


	// the thread is going to go away: we don't get called when the stack is freed, so we need
	//to free those allocations
	if (dwThreadId == GetCurrentThreadId())
	{
		CurrentThreadIsBeingDetached();
	}
	LOCK_MUTEX(&m_mxsListLock);

	for(pThread = s_pRootThreadList, pThreadPrev = NULL;
		pThread;
		pThreadPrev = pThread, pThread = pThread->m_pNext)
	{
		if(pThread->m_dwThreadId == dwThreadId) 
		{
			if (pThread->m_hThread != 0)
			{
				CloseHandle(pThread->m_hThread);
				pThread->m_hThread = 0;
			}
			delete pThread;
			ASSERT(s_pRootThreadList != pThread, "Should have already been removed from the threadlist");
			VerifyCThread();
			break;
		}
	}
}


#if MSDEBUG
void CThread::VerifyCThread()
{
	if (s_ThreadClassInitialized)
	{
		int n;
		CThread *pThread;
		for (n=0, pThread = s_FreeThreadList ; pThread ; pThread= pThread->m_pNext)
		{
			n++;
		}
		for (pThread = s_pRootThreadList ; pThread ; pThread= pThread->m_pNext)
		{
			n++;
		}
		if (n != MAXTHREADS)
		{
			VSASSERT(n == MAXTHREADS,"Thread links lists invalid");
		}
	}

}
#endif MSDEBUG
//
// CDebFreezeAllThreads:  This class freezes all threads on construction and
//			  resumes them on destruction.  Exceptions to this 
//			  list are the current thread (should be obvious), and
//			  the debugging thread.
//  Managed threads are suspended by the CLR: see CProcessGC::RuntimeSuspendFinished

//=--------------------------------------------------------------------------=
// CDebFreezeAllThreads::CDebFreezeAllThreads
//=--------------------------------------------------------------------------=
// On construction, freezes all threads but the current one and the debug 
// thread.  On termination, the threads resume.  This will only freeze/thaw
// threads that Rby_ThreadPool knows about.
//
CDebFreezeAllThreads::CDebFreezeAllThreads() : m_nThreads(0),
	m_rgThreads(m_rgThreadsStatic)
{
	//    if(!FSWITCH(_fFreezeThreads))
	//        return;

	DWORD	  dwCurThreadId = GetCurrentThreadId();
	DWORD	  dwDebThreadId = g_dwDebugThreadId;

	int	  nCurThreads = 0;
	int	  nMaxThreads = MAXTHREADS;

	LOCK_MUTEX(&CThread::m_mxsListLock);

	// first take a toolhelp snapshot to iterate the threads that we might have missed 
	//  (perhaps because we attached and didn't get a DLL_THREAD_ATTACH)
	// http://blogs.msdn.com/b/oldnewthing/archive/2006/02/23/537856.aspx
	HANDLE hthSnap = CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
	VSASSERTF((hthSnap != INVALID_HANDLE_VALUE,"toolhelp snap didn't work"));
	if (hthSnap != INVALID_HANDLE_VALUE)
	{
		THREADENTRY32 tentry;
		tentry.dwSize = sizeof(THREADENTRY32);
		auto pidCurrentProc = GetCurrentProcessId();
		if (Thread32First(hthSnap, &tentry))
		{
			do {
				if (tentry.dwSize >= FIELD_OFFSET(THREADENTRY32, th32OwnerProcessID) + sizeof(tentry.th32OwnerProcessID))
				{
					auto pid = tentry.th32OwnerProcessID;
					if (pid == pidCurrentProc)
					{
						auto tid = tentry.th32ThreadID;
						if (tid != dwCurThreadId && 
							tid  != g_dwThreadIdChildProcess  && 
							 tid != g_dwThreadIdMemSpectBgdThreadGCAndCleanupRCW
							 )
						{
							// now that we have a thread in our process, let's see if it's in our list
							bool fIsAlreadyInList = false;
							for (CThread *pThread = CThread::s_pRootThreadList;  pThread  ; pThread = pThread->m_pNext)
							{
								if (tid == pThread->GetId())
								{
									fIsAlreadyInList = true;
									break;
								}
							}
							if (!fIsAlreadyInList) // let's add it to the list
							{
								//char buf[1000];
								//sprintf_s(buf,sizeof(buf),"CDebFreezeAllThreads: found a thread we didn't know about %d", tid);
								//MessageBoxA(0,buf,"",0);
								CThread::Attach(tid);
								//VSASSERTF((false,"CDebFreezeAllThreads: found a thread we didn't know about %d", tid));
							}

						}
					}
				}
				tentry.dwSize = sizeof(THREADENTRY32);
			} while (Thread32Next(hthSnap, &tentry));
		}
		CloseHandle(hthSnap);
	}

	CThread  *pThreadList=CThread::s_pRootThreadList;
	if (pThreadList == NULL)
		return;

	CThread *pThread = pThreadList;

	while(pThread)
	{
		nCurThreads++;
		pThread = pThread->m_pNext;
	}

	////
	// We try not to allocate, hence the static chunk of thread slots.  If we
	// go over this, we will need to allocate.  Finally, if we can't allocate, we
	// cut our losses and only use the static amount.
	//
	if(nCurThreads < nMaxThreads)
	{
		m_rgThreads = m_rgThreadsStatic;
	}
	else
	{
		m_rgThreads = (CThread **)DebugAlloc(sizeof(CThread *) * nCurThreads);
		if(m_rgThreads)
			nMaxThreads = nCurThreads;
		else
			m_rgThreads = m_rgThreadsStatic;
	}

	////
	// Go through all the threads, suspending those that should get suspended
	//
	for(pThread = pThreadList; NULL != pThread; )
	{
		CThread *pNextInList = pThread->m_pNext;
		if(pThread->GetId() != dwCurThreadId && 
			pThread->GetId() != g_dwThreadIdChildProcess  && 
			pThread->GetId() != g_dwThreadIdMemSpectBgdThreadGCAndCleanupRCW &&
			m_nThreads < nMaxThreads)
		{
			VSASSERT(pThread->m_hThread == 0, "thread handle not 0?");
			// should we get all thread handles before suspending them one at a time?
			pThread->m_hThread = OpenThread(THREAD_QUERY_INFORMATION | THREAD_SUSPEND_RESUME, false, pThread->GetId()); //get thread handle so we can suspend/resume
			VSASSERTF((pThread->m_hThread  != INVALID_HANDLE_VALUE,"Couldn't open thread %d", pThread->GetId()));
			auto res = SuspendThread(pThread->m_hThread); // returns count, else -1 
			/* dllmain THREAD_DETACH works, but after detach, other DLLs have dllmain THREAD_DETACH which can allocate
			mem causing our CThread object to be recreated.
			*/
			if (res == -1)
			{
				auto err = GetLastError(); // ERROR_ACCESS_DENIED ?
				// failed to suspend. perhaps its not there any more
				CThread::Detach(pThread->GetId());
				//                pThread->Release();
				//// we can't release it while iterating the list, so we mark it
				//pThread->m_hThread = INVALID_HANDLE_VALUE;
			}
			else
			{
				m_rgThreads[m_nThreads++] = pThread;
			}
		}
		pThread = pNextInList;
	}
}


//=--------------------------------------------------------------------------=
// CDebFreezeAllThreads::~CDebFreezeAllThreads
//=--------------------------------------------------------------------------=
// Resumes all threads frozen in during construction.
//
CDebFreezeAllThreads::~CDebFreezeAllThreads()
{
	while(m_nThreads--)
	{
		HANDLE hThread = m_rgThreads[m_nThreads]->m_hThread;
		m_rgThreads[m_nThreads]->m_hThread = 0;
		ResumeThread(hThread);
		CloseHandle(hThread); // should this be done after all threads resumed?

		//        m_rgThreads[m_nThreads]->Release();
	}

	if(m_rgThreads != m_rgThreadsStatic)
		DebugFree(m_rgThreads);
}

//=--------------------------------------------------------------------------=
// VsSetOptionsFileName
//=--------------------------------------------------------------------------=
// By default it is MemSpect.ini, but somebody might want to change it
//
BOOL VsSetOptionsFileName(_In_opt_ char * pszFileName)
{
	char szOptionsFile[MAX_PATH];
	if(!pszFileName || !*pszFileName)
	{
		*szOptionsFile = 0;
		GetEnvironmentVariableA("USERPROFILE", szOptionsFile, (DWORD)(sizeof(szOptionsFile) - strlen("MemSpect.ini") - 1));
		if(*szOptionsFile)
		{
			strcat(szOptionsFile, "\\");
		}    
		strcat(szOptionsFile, "MemSpect.ini");
		pszFileName = szOptionsFile;
	}

	if(strlen(pszFileName) + 1 > sizeof(_szPersistFile))  
	{
		FAIL("VsSetOptionsFileName: file name is too long");
		return FALSE;
	}    

	strcpy(_szPersistFile, pszFileName);
	return TRUE;  
}

char *VsGetOptionsFileName()
{
	return _szPersistFile;
}

//=--------------------------------------------------------------------------=
// VsGetProfileInt
//=--------------------------------------------------------------------------=
// Our own special flavor of GetPrivateProfileInt
//
int VsGetProfileInt
	(
	LPCTSTR pszAppName,
	LPCTSTR pszKey,
	int	  nDefault
	)
{
	if(!*_szPersistFile)
	{
		VsSetOptionsFileName(NULL);
	}  
	return GetPrivateProfileInt(pszAppName, pszKey, nDefault, _szPersistFile);
}


//=--------------------------------------------------------------------------=
// VsWriteProfileInt
//=--------------------------------------------------------------------------=
// Our own special flavor to persist integers.
//
void VsWriteProfileInt
	(
	LPCTSTR pszAppName,
	LPCTSTR pszKey,
	int	  nValue
	)
{
	if(!*_szPersistFile)
	{
		VsSetOptionsFileName(NULL);
	}  

	char szBuf[20];
	sprintf_s(szBuf, _countof ( szBuf ), "%d", nValue);
	WritePrivateProfileString(pszAppName, pszKey, szBuf, _szPersistFile);
}


//=--------------------------------------------------------------------------=
// VsGetProfileString
//=--------------------------------------------------------------------------=
// Same as GetPrivateProfileString, but we control the filename.
//
void VsGetProfileString
	(
	LPCTSTR pszAppName,   // app name key
	LPCTSTR pszKey,       // key
	LPCTSTR pszDefault,   // default value
	_Out_z_cap_(dwReturnSize) LPTSTR  pszReturn,    // return buffer
	DWORD   dwReturnSize  // size of return buffer
	)
{
	if(!*_szPersistFile)
	{
		VsSetOptionsFileName(NULL);
	}  

	GetPrivateProfileString(pszAppName, pszKey, pszDefault, pszReturn , dwReturnSize, _szPersistFile);
}


//=--------------------------------------------------------------------------=
// VsWriteProfileString
//=--------------------------------------------------------------------------=
// Same as WritePrivateProfileString, but we control the filename.
//
BOOL VsWriteProfileString
	(
	LPCTSTR pszAppName,   // app name key
	LPCTSTR pszKey,       // key
	LPCTSTR pszString     // string to write
	)
{
	if(!*_szPersistFile)
	{
		VsSetOptionsFileName(NULL);
	}  

	return WritePrivateProfileString(pszAppName, pszKey, pszString, _szPersistFile);
}

BOOL IsUserInteractive() // see if runnning from a service
{
    HWINSTA winsta = GetProcessWindowStation();
    USEROBJECTFLAGS flags;
    DWORD len = 0;
    BOOL res = false;
    if (GetUserObjectInformation(winsta, UOI_FLAGS, &flags, sizeof(flags), &len))
    {
        if (flags.dwFlags & WSF_VISIBLE)
        {
            res = true;
        }
    }
    return res;

}





//
//
// DebSwitch support  - DebSwitch is a class that lives entirely in a
//                      public header.  It provides a way to extend
//                      debugging information automatically, and to persist
//                      this information to a file.
//

DebSwitch *g_pDebSwitchHead = NULL;


//=--------------------------------------------------------------------------=
// VsGetDebugSwitchHead
//=--------------------------------------------------------------------------=
// Externally linkable call that returns a pointer to the switch head node.
// this is used by the external class to like new instances of itself into
// our list.
//
CLINKAGE PVOID ENTRYPOINT VsGetDebSwitchHead()
{
	return (PVOID )&g_pDebSwitchHead;
}


//=--------------------------------------------------------------------------=
// VsLoadSwitchState
//=--------------------------------------------------------------------------=
// loads the value of this switch from the INI file
//
CLINKAGE VOID ENTRYPOINT VsLoadSwitchState
	(
	DebSwitch *pSwitch  // the switch that wants to be loaded
	)
{
	pSwitch->m_fSet = VsGetProfileInt(pSwitch->m_pszPackage, pSwitch->m_pszName, FALSE);
}


//
//
// CDebugStream - simple IStream implementation that we use for our own greedy
//                needs.
//
//


//=--------------------------------------------------------------------------=
// CDebugStream::Read
//=--------------------------------------------------------------------------=
// Performs an IStream::Read.
//
STDMETHODIMP CDebugStream::Read
	(
	PVOID pv, 
	ULONG cb, 
	ULONG *pcb
	)
{
	*pcb = 0;
	while(m_dwPos < m_dwSize && cb)
	{
		((PBYTE)pv)[*pcb] = m_psz[m_dwPos];
		m_dwPos++;
		cb--;
		(*pcb)++;
	}
	return S_OK;
}


//=--------------------------------------------------------------------------=
// CDebugStream::Write
//=--------------------------------------------------------------------------=
// Performs an IStream::Write.  If possible, this will grow the buffer
//
STDMETHODIMP CDebugStream::Write
	(
	void const *pv, 
	ULONG       cb, 
	ULONG      *pcb
	)
{
	*pcb = 0;

	// See if we can reallocate
	//
	if(m_dwPos + cb >= m_dwSize && m_fReallocOk)
	{
		DWORD dwSizeNew = m_dwSize + (cb / 256 + 1) * 256;
		LPSTR pszNew;

		if(m_psz)
			pszNew = (LPSTR)DebugRealloc(m_psz, dwSizeNew);
		else
			pszNew = (LPSTR)DebugAlloc(dwSizeNew);

		if(pszNew)
		{
			m_psz = pszNew;
			m_dwSize = dwSizeNew;
		}
	}

	while(m_dwPos < m_dwSize && cb)
	{
		m_psz[m_dwPos] = ((LPSTR)pv)[*pcb];
		m_dwPos++;
		cb--;
		(*pcb)++;
	}
	return m_dwPos < m_dwSize ? S_OK : STG_E_MEDIUMFULL;
}


//=--------------------------------------------------------------------------=
// CDebugStream::Seek
//=--------------------------------------------------------------------------=
// Performs an IStream::Seek.
//
STDMETHODIMP CDebugStream::Seek
	(
	LARGE_INTEGER   move, 
	DWORD           dwOrigin, 
	ULARGE_INTEGER *pnew
	)
{
	switch(dwOrigin)
	{
	case STREAM_SEEK_SET:
		m_dwPos = min(move.LowPart, m_dwSize - 1);
		break;

	case STREAM_SEEK_CUR:
		m_dwPos = min(move.LowPart + m_dwPos, m_dwSize - 1);
		break;

	case STREAM_SEEK_END:
		m_dwPos = max(0, (LONG)(m_dwSize - move.LowPart));
		break;
	}
	pnew->HighPart = 0;
	pnew->LowPart = m_dwPos;

	return S_OK;
}

#ifdef DEBUG

//=--------------------------------------------------------------------------=
// DebSwitchExt::DebSwitchExt
//=--------------------------------------------------------------------------=
// DebSwitchExt constructor
//
DebSwitchExt::DebSwitchExt(LPCSTR pszName, LPCSTR pszPackage, LPCSTR pszDesc) : DebSwitch(pszName, pszPackage, pszDesc)
{
	// Init copies to empty strings
	*m_rgszName = '\0';
	*m_rgszPackage = '\0';
	*m_rgszDesc = '\0';

	// Set member pointers to copies
	m_pszName = m_rgszName;
	m_pszPackage = m_rgszPackage;
	m_pszDesc = m_rgszDesc;

	// Copy the provided strings, truncating if they won't fit
	if (pszName) { strncat((LPSTR)m_rgszName, pszName, (sizeof(m_rgszName) / sizeof(*m_rgszName)) - 1); }
	if (pszPackage) { strncat((LPSTR)m_rgszPackage, pszPackage, (sizeof(m_rgszPackage) / sizeof(*m_rgszPackage)) - 1); }
	if (pszDesc) { strncat((LPSTR)m_rgszDesc, pszDesc, (sizeof(m_rgszDesc) / sizeof(*m_rgszDesc)) - 1); }
}
#endif

STDAPI_(PVOID) VsExternalAddSwitch
	(
	LPCSTR pszName,
	LPCSTR pszPackage,
	LPCSTR pszDesc
	)
{
	PVOID pv = NULL;

#ifdef DEBUG
	if (pszName != NULL && pszPackage != NULL && pszDesc != NULL)
	{
		VsIgnoreAllocs(TRUE);
		pv = new DebSwitchExt(pszName, pszPackage, pszDesc);
		VsIgnoreAllocs(FALSE);
	}
#endif

	return pv;
}

STDAPI_(BOOL) VsExternalQuerySwitch
	(
	PVOID pSwitch
	)
{
	BOOL fSet = FALSE;

#ifdef DEBUG
	if (pSwitch != NULL)
	{
		fSet = ((DebSwitchExt*)pSwitch)->m_fSet;
	}
#endif

	return fSet;
}

STDAPI_(VOID) VsExternalRemoveSwitch
	(
	PVOID pSwitch
	)
{
#ifdef DEBUG
	if (pSwitch != NULL)
	{
		delete (DebSwitchExt*)pSwitch;
	}
#endif
}


void makehex (DWORD dword, char *szBuf)// returns "0x12345678"
{
	char * ptr = (char *)&dword;
	*szBuf++ = '0';
	*szBuf++ = 'x';
	for (int i = 3 ; i >=0 ; i--)
	{
		char c = ptr[i];
		char x = (c >> 4 ) & 0xf;
		x += x < 10 ? '0' : 'a'-10;
		*szBuf++=x;
		x = c & 0xf;
		x += x < 10 ? '0' : 'a'-10;
		*szBuf++=x;
	}
	*szBuf++ = 0;
}


//=--------------------------------------------------------------------------=
// EOF
