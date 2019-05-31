#include "Pch.H"
#include "StackWalk.h"

#if defined(_M_IX86)

#include "dbghelp.h"
#include "vsassert.h"
#include "main.h"
#include "mem.h"
#include "modaddr.h"
#include <functional>

#ifdef VSASSERT_TEST
static DWORD s_dwGetModuleBaseTime = 0;
static DWORD s_dwShouldRememberAddress = 0;
static UINT s_uicAllocs = 0;
static UINT s_uicFaults = 0;
static UINT s_uicStackWalkTime = 0;
static UINT s_uicStackWalkFaultsTime = 0;
#endif //VSASSERT_TEST

// statistics, used only in collect leaks mode
static UINT  s_uicFast = 0;
static UINT  s_uicSlow = 0;
static DWORD s_dwSlowTime = 0;
static UINT  s_uicUpdateModules = 0;
static DWORD s_dwUpdateModulesTime = 0;




DECLARE_MUTEX(s_mxsStackWalk);    // Locking mutex
//static CMutexWithTimeout s_mxsStackWalk(100);  // Locking mutex with timeout

static LPVOID __stdcall FunctionTableAccess(HANDLE hProcess, ULONG_PTR dwPCAddress);
static ULONG_PTR __stdcall GetModuleBase(HANDLE hProcess, ULONG_PTR dwReturnAddress);

// fast stack backtrace functions

UINT FastGetStackAddrs(UINT ifrStart,                // How many stack elements to skip before starting.
                       UINT cfrTotal,                // How many to get (size of the pdwAddr array)
                       DWORD_PTR * pdwAddr,          // Stack addresses array
                       BOOL fSkipSomeDlls = FALSE);  // pay attention to skip dlls options or not




BOOL EnsureModAddrObject(HANDLE hProcess)
{
    if(!s_pModAddr)
    {
        s_pModAddr = new CModAddr(hProcess);
        if(!s_pModAddr)
            return FALSE;
    }  
    return TRUE;  
}

//-----------------------------------------------------------------------------

void UpdateLoadedModules()
{
    CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally

    DWORD dwTimeStart;
    if(g_fCollectLeaks)
        dwTimeStart = GetTickCount();

    LOCK_MUTEX(&s_mxsStackWalk);
    if(s_pModAddr)
        s_pModAddr->UpdateLoadedModules();
        
    if(g_fCollectLeaks)
    {
        s_uicUpdateModules++;
        s_dwUpdateModulesTime += GetTickCount() - dwTimeStart;
    }    
}

//-----------------------------------------------------------------------------

UINT GetStackAddrs
(
 UINT ifrStart,       // How many stack elements to skip before starting.
 UINT cfrTotal,       // How many to get (size of the pdwAddr array)
 DWORD_PTR * pdwAddr, // Stack addresses array
 BOOL fSkipSomeDlls   // pay attention to skip dlls options or not
 )
{

    
    /*
    CLockWithTimeout cl(&s_mxsStackWalk);
    
    if(!cl.IsLocked())
    {
      s_uicTimedOut++;
      return 0;
    }  
    */
    if (!g_nUseChildProcessForSymbols) // if we're using the current process for symbol res
    {
        LOCK_MUTEX(&s_mxsStackWalk); // we need to load modules for symbols
        if(!EnsureModAddrObject(0))
        {
            return 0;
        }
    }

    UINT uicAddrs = 0;
    if(g_fEnableFastGetStack)
    {
        uicAddrs = FastGetStackAddrs(ifrStart+1, cfrTotal, pdwAddr, fSkipSomeDlls);
        if(uicAddrs)
        {
            if(g_fCollectLeaks)
            {
                s_uicFast++;
            }    
            if (CHeapSpy::g_AssertOnStackFrame)
            {
                auto end = CHeapSpy::g_AssertOnStackFrame->m_pStlType->end();
                for (UINT i = 0 ; i < uicAddrs ; i++)
                {
                    auto res = CHeapSpy::g_AssertOnStackFrame->m_pStlType->find(pdwAddr[i]);
                    if (res != end)
                    {
                        VSASSERTF((false,"AssertOnStackFrame Addr = %x", pdwAddr[i]));
                        break;
                    }
                }

            }
        }  

    }
    return uicAddrs;
}  

//------------------------------------------------------------------------


// if g_fHandle4gigStacks==0, dwAddress is the address conditionally ORed with MANAGED_STACK_FLAG.
// if g_fHandle4gigStacks == 1, dwAddress is the key (of type StackFrameIndex) to the lookup map, which indicates the real address and a BOOL indicating Managed. 
BOOL GetStringFromAddr(DWORD_PTR dwAddress,	
                                            _Out_cap_(uicBuf) char * pszBuf, 
                                            UINT uicBuf, 
                                            BOOL fIsFunctionID /*= false*/,
                                            BOOL fNoFileLineInfo /*=false*/, 
                                            BOOL fIsCallingFromChildProcess /*=false*/)
{
    BOOL fRes = false;
    CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour

    LOCK_MUTEX(&s_mxsStackWalk);
    *pszBuf = 0;
    BOOL fManaged = false;

    if (!g_fHandle4gigStacks) // if we can use the high bit to indicate managed?
    {
        if (dwAddress & MANAGED_STACK_FLAG)
        {
            fManaged =true;
            dwAddress = dwAddress & ~ MANAGED_STACK_FLAG;
        }
    }

    if (fIsCallingFromChildProcess)
    {
        fManaged = true;
    }
    if (fManaged)
    {
        int retval = 0;
        WCHAR wszName[MAXCLASSNAMELEN];
        if (g_MapDynStackSymNamesWrapper)
        {
            auto res = g_MapDynStackSymNamesWrapper->m_pStlType->find(dwAddress);
            if (res != g_MapDynStackSymNamesWrapper->m_pStlType->end())
            {
                retval = res->second->GetSymName(wszName, dimensionof(wszName));
            }
        }
        if (retval == 0)
        {

			retval = GetFunctionOrClassNameFromIP(
                (UINT_PTR) dwAddress,  
                wszName, 
                dimensionof(wszName), 
                /*fIncludeModuleName =*/true,
                /*fIncludeFullPath=*/false,
                /*fIncludeSourceFile=*/true,
                fIsFunctionID ? true: false);
//            retval = ResolveFuncId(dwAddress, pszBuf, uicBuf, fNoFileLineInfo);
        }
        retval = WideCharToMultiByte(CP_THREAD_ACP, 0, wszName, retval+1, pszBuf,uicBuf, 0,0); // use "+1" to pass in nullchar, so it comes out
        return retval;
    }
    // this code is called from GetTrkBlkInfoForObj to resolve call stacks for some asserts
    // must be native symbols
    //if fIsCallingFromChildProcess, we are only being called for native symbols, regardless of 4gig flag
    if (!fIsCallingFromChildProcess)
    {

        if (g_nUseChildProcessForSymbols) // we're not using current process for symres, so just return hex. (note: managed already done by ResolveFuncId
        {
            // store hex for later resolution because resolving symbols may not work yet (child proc is just spinning up)
            makehex(dwAddress, pszBuf);
            char parms[8];
            ((DWORD *)parms)[0] = 1; //  file/lineinfo
            ((DWORD *)parms)[1] = dwAddress;
            int nLen = SendMsgToChild(ResolveSymbolFromTarg , sizeof(parms), parms, uicBuf, pszBuf); 

            return TRUE;
        }

        if(!EnsureModAddrObject(0))
            return FALSE;
    }
    fRes =  s_pModAddr->ResolveSymbol(dwAddress,	pszBuf, uicBuf, fNoFileLineInfo);
    return fRes;
}


CLINKAGE UINT ENTRYPOINT VsGetStackAddresses(UINT ifrStart, UINT cfrTotal, DWORD_PTR * pdwAddr)
{
    CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally
    return GetStackAddrs(ifrStart + 1, cfrTotal, pdwAddr);
}


CLINKAGE UINT ENTRYPOINT VsClearSymbols()
{
    CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally
    if (s_pModAddr)
    {// need to uninit dbghelp, MODULE_DATA too
        delete s_pModAddr;
        s_pModAddr  = NULL;
    }
    return 0;
}



//---------------------------------------------------------

//VsResolveSymbolsEx: VSAssert can resolve symbols from any process. In particular, we can let the child process bear the memory burden
CLINKAGE BOOL ENTRYPOINT VsResolveSymbolEx(HANDLE hProcess, DWORD_PTR dwAddress, _Out_cap_(uicBuf) char * pszBuf, UINT uicBuf, BOOL fNoFileLineInfo)
{
    BOOL fRetval = false;
    g_nUseChildProcessForSymbols = false; // Enable symbol resolution across processes:
                                            // This entrypoint VsResolveSymbolEx is called from SymbolResolver process (Memspect.exe)
                                            // VSAssert.dll is loaded into both child and target processes.
                                            // Setting g_nUseChildProcessForSymbols =false means the one in the child process can resolve syms
    CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally
    if (EnsureModAddrObject(hProcess))
    {
        //we want to suppress Drive not ready dialog when loading symbols
        // VSAssert.dll is built on drive D, so Dbghelp.dll looks for PDB on D:, and if it's a DVD drive with no media...

        UINT dwErrorMode = GetErrorMode();
        SetErrorMode(SEM_FAILCRITICALERRORS);

        fRetval =  s_pModAddr->ResolveSymbol(dwAddress,	pszBuf, uicBuf, fNoFileLineInfo);
        SetErrorMode(dwErrorMode);
    }
    return fRetval;
}

//---------------------------------------------------------

CLINKAGE BOOL ENTRYPOINT VsResolveSymbols(DWORD_PTR dwAddress, _Out_cap_(uicBuf) char * pszBuf, UINT uicBuf)
{
    CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally
    return GetStringFromAddr(dwAddress, pszBuf, uicBuf);
}
// given a filename, find the source code from symsrv.dll so we can examine the file
CLINKAGE BOOL ENTRYPOINT VsSymGetSourceFile(HANDLE hProcess, 
                                DWORD_PTR dwAddress, 
                                PCTSTR pszFileName,
                                _Out_cap_(uicBuf) char * pszBuf, 
                                UINT uicBuf)
{
    BOOL fRetval = false;
    CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally
    if (EnsureModAddrObject(hProcess))
    {
        fRetval =  s_pModAddr->VsSymGetSourceFile(dwAddress, pszFileName, pszBuf, uicBuf);
    }
    return fRetval;
}


//------------------------------------------------------------------------

BOOL GetModulesAddressSpace(_In_opt_ char ** ppszModules, UINT uicModules, DWORD_PTR * pdwAddressSpace)
{

    LOCK_MUTEX(&s_mxsStackWalk);
    /*
    CLockWithTimeout cl(&s_mxsStackWalk);
    
    if(!cl.IsLocked())
    {
      s_uicTimedOut++;
      return FALSE;
    } 
    */ 
    
    if(!EnsureModAddrObject(0))
        return FALSE;
        
    return s_pModAddr->FindModulesAddrSpace(ppszModules, uicModules, pdwAddressSpace);
}

//------------------------------------------------------------------------

void StackWalkClose()
{
    if(g_fCollectLeaks)
    {
         //// we do not want to create a leaks file just with this string
         //if(IsCreated(g_pszLeakReportFilename))
         //    DEBUGPRINTF("\r\n------Fast stacks: %d, %d msec\r\n------Slow stacks: %d, %d msec\r\n------ModuleUpdates: %d, %d msec\r\n", 
         //           s_uicFast, s_dwFastTime, s_uicSlow, s_dwSlowTime, s_uicUpdateModules, s_dwUpdateModulesTime);
    }         
    if(s_pModAddr)
    {
        delete s_pModAddr;
        s_pModAddr = NULL;
    }  
}  

//------------------------------------------------------------------------

void OnLoadModule(HMODULE hMod)
{
    if(!hMod)
        return;
        
    THREADGLOBAL *ptd = GetThreadData();
    if(!ptd || ptd->m_ulDisableTrace > 1)
        return;

    LOCK_MUTEX(&s_mxsStackWalk);
    /*
    CLockWithTimeout cl(&s_mxsStackWalk);
    
    if(!cl.IsLocked())
    {
      s_uicTimedOut++;
      return;
    }  
    */
    
    if(EnsureModAddrObject(0))
    {
        // this will add a new module to the list
        s_pModAddr->GetModuleBase(DWORD_PTR(hMod), TRUE);
    }    
}

//----------------------------------------------------------------------------

void OnSymbolsPathChanged()
{
    if(s_pModAddr)
        s_pModAddr->OnSymbolsPathChanged();
}

//-----------------------------------------------------------------------------
// Stack walk (slow stack) callbacks
//-----------------------------------------------------------------------------

static LPVOID __stdcall FunctionTableAccess(HANDLE hProcess, ULONG_PTR dwPCAddress)
{
    return SymFunctionTableAccess(hProcess, dwPCAddress);
}

//------------------------------------------------------------------------------

static ULONG_PTR __stdcall GetModuleBase(HANDLE hProcess, ULONG_PTR dwReturnAddress)
{
#ifdef VSASSERT_TEST
    DWORD dwTimeStart = GetTickCount();
    ULONG_PTR ret = (ULONG_PTR)s_pModAddr->GetModuleBase(dwReturnAddress);
    s_dwGetModuleBaseTime += GetTickCount() - dwTimeStart;
    return ret;
#else  
    return (ULONG_PTR)s_pModAddr->GetModuleBase(dwReturnAddress);
#endif //VSASSERT_TEST  
}

//-----------------------------------------------------------------------------
// Fast stack
//-----------------------------------------------------------------------------

UINT FastGetStackAddrs
(
 UINT ifrStart,       // How many stack elements to skip before starting.
 UINT cfrTotal, 
 DWORD_PTR * pdwAddr,
 BOOL fSkipSomeDlls
 )
{
    /*
    #define UPDATE_MODULES 1000
    static UINT s_uicAlloc = UPDATE_MODULES;
    static s_uicUpdated = 0;
    
    s_uicAlloc++;    
    if(s_uicAlloc > UPDATE_MODULES)
    {
        s_pModAddr->UpdateLoadedModules();
        s_uicAlloc = 0;
        s_uicUpdated++;
    }    
    */
    
    VSASSERTF((false,"dead code? fastgetstackaddrs "));
    return 0;
}


//------------------------------------------------------------------------

#else //_M_IX86

UINT GetStackAddrs(UINT ifrStart,  UINT cfrTotal, DWORD_PTR * pdwAddr, BOOL fSkipSomeDlls)
{
    return 0;
}

BOOL GetStringFromAddr(DWORD_PTR dwAddress,	char * pszBuf, UINT uicBuf, BOOL fNoFileLineInfo)
{
    if(uicBuf)
        *pszBuf = 0;
    return FALSE;  
}

void StackWalkClose()
{
}

BOOL GetModulesAddressSpace(char ** ppszModules, UINT uicModules, DWORD_PTR * pdwAddressSpace)
{
    return 0;
}

void UpdateLoadedModules()
{
}

void OnLoadModule(HMODULE hMod)
{
}

void OnSymbolsPathChanged()
{
}


#endif //_M_IX86
