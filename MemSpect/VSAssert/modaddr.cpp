#include "pch.h"

#if defined(_M_IX86)

#include <stdio.h>
#include "dbghelp.h"
#include "vsassert.h"
#include "main.h"
#include "modaddr.h"
#include "mem.h"
#include "StackWalk.h"
//#include "..\inc\version.h" // we want version.h from common, not vc one

//--------------------------------------------------------
// CModAddr
//--------------------------------------------------------

CModAddr * s_pModAddr = NULL;


BOOL CModAddr::sm_fSymInitialized = FALSE;
BOOL CModAddr::sm_fInitialized = FALSE;
DWORD CModAddr::s_dwDesiredOptions = 0;




PVOID CModAddr::operator new(size_t cb)
{
    return DebugAlloc(cb);
}

void CModAddr::operator delete(PVOID pv)
{
    return DebugFree(pv);
}

PSYMBOL_REGISTERED_CALLBACK64 g_SymbolCallBackFunction = 0;

CLINKAGE BOOL ENTRYPOINT VsSetSymbolCallbackFunction(PSYMBOL_REGISTERED_CALLBACK64 callbackAddr)
{
    g_SymbolCallBackFunction = callbackAddr;
    return true;
}

CModAddr::CModAddr(HANDLE hProcess /*= NULL */)
{
    m_pHead = NULL;
    m_pLastUsed = NULL;
    m_pPage = NULL;
    m_uicPageMods = 0;
    m_ppModulesToSkip = NULL;
    if (hProcess == NULL)
    {
        m_hProcess = GetCurrentProcess();
    }
    else
    {
        m_hProcess = hProcess;
    }
    
    {
//        VSASSERT(false,"attachdebug");

        DWORD dwOptions = SymGetOptions();
        dwOptions |= SYMOPT_LOAD_LINES 
#if MSDEBUG
            | SYMOPT_DEBUG
#endif MSDEBUG
            ;
        s_dwDesiredOptions = dwOptions; // record the options.
        SymSetOptions(dwOptions);

        char *sympath = g_pszSymbolsPath;
        if (sympath) 
        {
            // if we have one spec'd, let's prepend the curdir to get the sym of ourself (even if it's alongside the dll, the pdb won't 
            //  be found if there's a specific sympath
            char szDrive[MAX_PATH];
            char szDir[MAX_PATH];
            char szpath[MAX_SYMBOLS_PATH];
            _splitpath(g_szVSAssertDllFullPathName, szDrive, szDir, NULL, NULL);
            sprintf_s(szpath, dimensionof(szpath), "%s%s;%s",szDrive, szDir, g_pszSymbolsPath);
            sympath = szpath;
        }

        sm_fSymInitialized = SymInitialize(m_hProcess , sympath,/*fInvadeProcess=*/ FALSE);
        if (g_SymbolCallBackFunction)
        {
            if (SymRegisterCallback64(m_hProcess, g_SymbolCallBackFunction, 0) == 0)
            {
                VSASSERTF((false,"Couldn't set SymbolCallbackFunction %0 ", GetLastError()));
            }
        }
    }  
}

CModAddr::~CModAddr()
{
    // free pages;
    MODULE_DATA_PAGE * pPage = m_pPage;
    MODULE_DATA_PAGE * pPrevPage;
    while(pPage)
    {
        pPrevPage = pPage->pPrev;
        DebugFree(pPage);
        pPage = pPrevPage;
    }

    if(m_ppModulesToSkip)
        DebugFree(m_ppModulesToSkip);
    

    SymCleanup(m_hProcess); //release all symbol resources
    sm_fSymInitialized = false;
    sm_fInitialized = false;
}

//--------------------------------------------------------

void CModAddr::UpdateLoadedModules()
{
    HMODULE hMods[1024];
    DWORD cbNeeded; 

    // Get a list of all the modules in this process.
    if(psapi_EnumProcessModules(m_hProcess , hMods, sizeof(hMods), &cbNeeded)) 
    {
        int count = (cbNeeded / sizeof(HMODULE));
        for (int i = 0; i < count; i++) 
            FindModule((DWORD_PTR)hMods[i], TRUE);
    }
}

//--------------------------------------------------------

/*
BOOL CModAddr::EnsureSymInitialized()
{
    if(!sm_fInitialized)
    {
        sm_fInitialized = TRUE;
        // we are not supporting win9x
        OSVERSIONINFO osVer;
        osVer.dwOSVersionInfoSize = sizeof(osVer);
        if(GetVersionEx(&osVer) && VER_PLATFORM_WIN32_NT == osVer.dwPlatformId)
        {
            SymSetOptions(SYMOPT_LOAD_LINES);
            sm_fSymInitialized = SymInitialize(m_hProcess , NULL, FALSE);
            OnSymbolsPathChanged();
        }  
    }

    return sm_fSymInitialized; 
}
*/
//--------------------------------------------------------

void CModAddr::OnSymbolsPathChanged()
{
    if(sm_fSymInitialized)
    {
        SymSetSearchPath(GetCurrentProcess() , g_pszSymbolsPath);
    }
}

//--------------------------------------------------------

DWORD_PTR CModAddr::GetModuleBase(DWORD_PTR dwAddress, BOOL fhInst)
{
    MODULE_DATA * pModData = FindModule(dwAddress, fhInst);
    return pModData ? pModData->dwBaseAddress : 0;
}


CLINKAGE BOOL ENTRYPOINT VsLoadModuleInfo(HANDLE hProcess, 
                                PCTSTR pszModuleName,
                                DWORD_PTR dwBaseAddress, 
                                UINT nModuleSize)
{
    BOOL fRetval = false;
    if (EnsureModAddrObject(hProcess))
    {
        CComBSTR bstrName(pszModuleName);
        MODULEINFO mi;
        mi.EntryPoint = 0;
        mi.lpBaseOfDll = (LPVOID) dwBaseAddress;
        mi.SizeOfImage = nModuleSize;
        auto newmod = s_pModAddr->NewModule(dwBaseAddress,/*fhInst=*/  0, &mi, bstrName);
        if (newmod)
        {
            if (s_pModAddr->FindModule(dwBaseAddress, /*fhInst =*/0, /* MODULE_DATA *pNewData =*/ newmod))
            {
                fRetval = true;
            }
        }
    }

    return fRetval;
}



//--------------------------------------------------------

BOOL  CModAddr::EnsureLibLoaded(DWORD_PTR dwAddress)
{
    MODULE_DATA * pModData = FindModule(dwAddress);
    if(!pModData)
        return FALSE;

    if (SymGetOptions() != s_dwDesiredOptions) // somebody else might have changed the options from under us.
    {
        SymSetOptions(s_dwDesiredOptions);
    }

    if(!pModData->hInst && pModData->szFileName[0])
    {
        // some versions of dbghelp can find pdbs only in current dir
        char szOldCurDir[MAX_PATH];
        *szOldCurDir = 0;
        
        DWORD dwOldCurDir = ::GetCurrentDirectoryA(sizeof(szOldCurDir), szOldCurDir);
        
        char szModuleDir[2 * MAX_PATH];
        char szDrive[MAX_PATH];
        char szDir[MAX_PATH];
        
        _splitpath(pModData->szFileName, szDrive, szDir, NULL, NULL);
        _makepath_s(szModuleDir, szDrive, szDir, NULL, NULL);
        
        ::SetCurrentDirectoryA(szModuleDir);
        
        if(SymLoadModule(m_hProcess ,
                          NULL, 
                          pModData->szFileName,
                          NULL, 
                          pModData->dwBaseAddress, 
                          pModData->dwEndAddress - pModData->dwBaseAddress))
        {
            pModData->hInst = (HINSTANCE)pModData->dwBaseAddress;
        } 
        else
        {
            DEBUGPRINTF("SymLoadModule failed for %s\r\n", pModData->szFileName);
        } 
        
        // restore cur dir
        if(dwOldCurDir && dwOldCurDir < sizeof(szOldCurDir) && *szOldCurDir)
        {
            ::SetCurrentDirectoryA(szOldCurDir);
        }
    }  
    return pModData->hInst ? TRUE : FALSE;
}

//--------------------------------------------------------

CModAddr::MODULE_DATA * CModAddr::FindModuleByName(_In_opt_z_ char * pszDllName)
{
    if(pszDllName)
    {  
        // Slow, but it should not be called many times, so we don't care
        MODULE_DATA * p = m_pHead;
        char * pName;

        while(p)
        {
            pName = strrchr(p->szFileName, '\\');
            if(pName && 0 == _stricmp(pName+1, pszDllName))
                return p;

            p = p->pNext;
        }
    }
    return NULL;
}

//--------------------------------------------------------

UINT CModAddr::FindModulesAddrSpace(_In_opt_ char ** ppszModules, UINT uicModules, DWORD_PTR * pdwAddrSpace)
{
    UINT uicFound = 0;
    UINT i;

    if(ppszModules && pdwAddrSpace)
    {  
        // Slow, but it should not be called many times, so we don't care
        MODULE_DATA * p = m_pHead;
        char * pName;
        int iRet;

        while(p && uicFound < uicModules)
        {
            pName = strrchr(p->szFileName, '\\');
            if(pName)
            {
                for(i = 0; i < uicModules; i++)
                {
                    iRet = _stricmp(pName+1, ppszModules[i]);
                    if(0 == iRet)
                    {
                        pdwAddrSpace[uicFound * 2] = p->dwBaseAddress;
                        pdwAddrSpace[uicFound * 2 + 1] = p->dwEndAddress;
                        uicFound++;
                        break;
                    }
                    else if(iRet < 0)
                        break;
                }
            }
            p = p->pNext;
        }
    }
    return uicFound;
}

//--------------------------------------------------------

CModAddr::MODULE_DATA * CModAddr::FindModule(DWORD_PTR dwAddress, BOOL fhInst/*=NULL*/, MODULE_DATA *pNewData /*= NULL*/)
{
#ifdef VSASSERT_TEST
    static DWORD s_dwFindModuleTime = 0;
    DWORD dwTimeStart = GetTickCount();
#endif VSASSERT_TEST

    MODULE_DATA ** ppPrevNext = &m_pHead;
    MODULE_DATA * p = m_pHead;
   
    if(m_pLastUsed && m_pLastUsed->dwBaseAddress <= dwAddress)
    {
        if(dwAddress < m_pLastUsed->dwEndAddress)
            return m_pLastUsed;
        else  
        {
            ppPrevNext = &m_pLastUsed->pNext;   
            p = m_pLastUsed->pNext;
        }
    }

    while(p)
    {
        if(dwAddress < p->dwBaseAddress)
            break;
        if(dwAddress < p->dwEndAddress) 
        {
            m_pLastUsed = p;
            return p;
        }  
        ppPrevNext = &p->pNext;   
        p = p->pNext;
    }

    // cannot find the module, add it to the list
    MODULE_DATA * pNewModule;
    if (pNewData) // if user specified one to use
    {
        pNewModule = pNewData;
    }
    else
    {
        pNewModule = NewModule(dwAddress, fhInst);
    }

    if(pNewModule)
    {
        *ppPrevNext = pNewModule;
        pNewModule->pNext = p;
    }

    m_pLastUsed = pNewModule;

#ifdef VSASSERT_TEST
    s_dwFindModuleTime += GetTickCount() - dwTimeStart;
#endif VSASSERT_TEST

    return pNewModule;
}

//--------------------------------------------------------

CModAddr::MODULE_DATA * CModAddr::MakeSureWeHaveSpaceForNewModuleData()
{
    // make sure we have space for a new module data
    if(!m_pPage || m_uicPageMods >= MODULE_DATA_PAGE_SIZE)
    {
        MODULE_DATA_PAGE * pNewPage;
        pNewPage = (MODULE_DATA_PAGE *)DebugAlloc(sizeof(MODULE_DATA_PAGE));
        if(!pNewPage)
            return NULL;

        ZeroMemory(pNewPage, sizeof(MODULE_DATA_PAGE));
        pNewPage->pPrev = m_pPage;
        m_pPage = pNewPage;
        m_uicPageMods = 0;
    }
    return &m_pPage->Modules[m_uicPageMods];
}

CModAddr::MODULE_DATA * CModAddr::NewModule(
    DWORD_PTR dwAddress, 
    BOOL fhInst/*=false*/, 
    MODULEINFO *pMI /*=NULL*/,
    CComBSTR bstrName /* = NULL */
    )
{

    // Get module addresses and filename
    MODULE_DATA * pModData = MakeSureWeHaveSpaceForNewModuleData(), *pRetval =NULL;
    MODULEINFO mi = {0};
    if (pMI == NULL)
    {
        MEMORY_BASIC_INFORMATION memoryBasicInfo = {0};
        if(fhInst)
        {
            memoryBasicInfo.AllocationBase = (PVOID)dwAddress;
        }
        else
        {
            ::VirtualQueryEx(m_hProcess , (LPVOID) dwAddress, &memoryBasicInfo, sizeof(memoryBasicInfo));
        }
        psapi_GetModuleInformation(m_hProcess , (HMODULE)memoryBasicInfo.AllocationBase, &mi, sizeof(MODULEINFO));

        // kernel32's GetModuleFileName dead locks if called simultaneously with GetStackAddr
        //GetModuleFileNameA((HINSTANCE)memoryBasicInfo.AllocationBase,	pModData->szFileName, sizeof(pModData->szFileName));
        if (mi.SizeOfImage)
        {
            psapi_GetModuleFileNameExA(m_hProcess , (HMODULE)memoryBasicInfo.AllocationBase, pModData->szFileName, sizeof(pModData->szFileName));
        }
    }
    else
    {
        USES_CONVERSION;
        mi.lpBaseOfDll = pMI->lpBaseOfDll;
        mi.SizeOfImage = pMI->SizeOfImage;
        auto ptr = W2A(bstrName);
        strcpy(pModData->szFileName, ptr);
    }
    if (mi.SizeOfImage)
    {
        m_uicPageMods++;
        pModData->dwBaseAddress = (DWORD_PTR)mi.lpBaseOfDll;
        pModData->dwEndAddress = pModData->dwBaseAddress + mi.SizeOfImage;
        pRetval = pModData;
    }

    return pRetval;   
}


BOOL CModAddr::VsSymGetSourceFile(DWORD_PTR dwAddress, 
                            PCTSTR pszFileName,
                            _Out_cap_(uicBuf) char * pszBuf, 
                            UINT uicBuf)
{
    BOOL fretval = false;
    MODULE_DATA * pModData = FindModule(dwAddress);
    if(pModData)
    {
        if (s_pModAddr->EnsureLibLoaded(dwAddress))
        {
            fretval = SymGetSourceFile(m_hProcess, pModData->dwBaseAddress, 0, pszFileName, pszBuf, uicBuf);
        }
    }

    return  fretval;
}



//--------------------------------------------------------

BOOL CModAddr::ResolveSymbol(DWORD_PTR dwAddress,	_Out_z_cap_(uicBuf) char * pszBuf, UINT uicBuf, BOOL fNoFileLineInfo)
{
    if(!uicBuf || pszBuf == NULL)
        return FALSE;

    BOOL fRetval = TRUE;
    DWORD lastError = 0;

    //char * pszStart = pszBuf;

    EnsureLibLoaded(dwAddress);

    PSTR pszSymbol = NULL;

    IMAGEHLP_LINE li = {0};
    li.SizeOfStruct = sizeof(li);

    DWORD dw = 0;

    if (!fNoFileLineInfo)
    {
        if (SymGetLineFromAddr(m_hProcess, dwAddress, &dw, &li))
        {
            UINT len = _snprintf(pszBuf, uicBuf, "%s(%u) : ", li.FileName, li.LineNumber);
            pszBuf[uicBuf-1] = 0;

            if (uicBuf <= len)
            {
                VSFAIL("Buffer access size could be incorrect");
                return FALSE;
            }
            else
            {
                uicBuf -= len;
            }
            pszBuf += len;
        }
        else
        {
//            lastError = GetLastError();
        }
    }


    {
#define MAX_SYMBOLS 512
        // dbghelp has a bug in ansi functions, so we use unicode here
        IMAGEHLP_MODULEW64 mi = {0};
        mi.SizeOfStruct = sizeof(mi);

        LPSTR pszModule = NULL; 
        LPSTR pszSymbols = NULL; 
        char szModuleName[MAX_PATH] = "";

        char szUndec[MAX_SYMBOLS];

        DWORD64 dwOffset = 0;

        if(SymGetModuleInfoW64(m_hProcess, dwAddress, &mi))
        {
            WCHAR * pwszModule = wcsrchr(mi.ImageName, '\\');
            if (pwszModule == NULL)
                pwszModule = mi.ImageName;
            else
                pwszModule++;

            int cb = WideCharToMultiByte(CP_ACP,
                                         0,
                                         pwszModule,
                                         -1, // len,
                                         szModuleName,
                                         sizeof(szModuleName),
                                         NULL,
                                         NULL);                
            if(cb)
            {
                // make sure we have a zero terminated string
                szModuleName[sizeof(szModuleName) - 1] = 0;
                pszModule = szModuleName;    
            }   
        }
        else
        {
            lastError = GetLastError();
        }
        
        if(!pszModule) // cannot get module name info for some reason - use filename
        {
             MODULE_DATA * pModData = FindModule(dwAddress);
             if(pModData && *pModData->szFileName)
             {
                 char *p = strrchr(pModData->szFileName, '\\');
                 if(p)
                   p++;
                 else
                   p = pModData->szFileName;
                 strncpy(szModuleName, p, sizeof(szModuleName));  
             }
             pszModule = szModuleName;
        }

        __try
        {

            union 
            {
                CHAR rgchSymbol[sizeof(IMAGEHLP_SYMBOL64) + MAX_SYMBOLS];
                IMAGEHLP_SYMBOL64  sym;
            } sym;

            memset(&sym.sym, 0, sizeof(sym.sym));
            sym.sym.SizeOfStruct = sizeof(sym.sym);
            sym.sym.Address = dwAddress;
            sym.sym.MaxNameLength = MAX_SYMBOLS;

            if (SymGetSymFromAddr64(m_hProcess, dwAddress, &dwOffset, &sym.sym))
            {
                pszSymbol = sym.sym.Name;

                if (UnDecorateSymbolName(sym.sym.Name, szUndec, sizeof(szUndec)/sizeof(szUndec[0]), 
                    UNDNAME_NAME_ONLY))
                {
                    pszSymbol = szUndec;
                }
                else if (SymUnDName64(&sym.sym, szUndec, sizeof(szUndec)/sizeof(szUndec[0])))
                {
                    pszSymbol = szUndec;
                }
            }
            else
            {
                lastError = GetLastError();
            }
        }
        __except (EXCEPTION_EXECUTE_HANDLER)
        {
            ;
        }
        int nLen = 0;

        if(!pszModule)
        {
             if(fNoFileLineInfo)
             {
                strncpy(pszBuf, "*", uicBuf);
                nLen = 1;
             }
             else
             {
                nLen = _snprintf(pszBuf, uicBuf, "0x%08lX", dwAddress);
             }
        }    
        else if(!pszSymbol)  
        {
            if(fNoFileLineInfo)
                nLen = _snprintf(pszBuf, uicBuf, "%s!*", pszModule);
            else
                nLen = _snprintf(pszBuf, uicBuf, "%s!0x%08lX", pszModule, dwAddress);
        }    
        else  
            nLen = _snprintf(pszBuf, uicBuf, "%s!%s + %d bytes", pszModule, pszSymbol, (DWORD)dwOffset);

        pszBuf[uicBuf - 1] = 0;
        if (lastError!= 0 && nLen > 0)
        {
            _snprintf(pszBuf + nLen, uicBuf - nLen, " SymErr = 0x%x", lastError);
        }
    }  

    return TRUE;
}

//--------------------------------------------------------


//------------------------------------------------------------------
///////////////////////////////////////////////////////////////////////////
//
// Imports from ntdll.dll.  This is only used for Win NT
//

/*
    olgaark: for some reason if we call RtlCaptureStackBackTrace from ntdll.dll 
    it never works on WinXP when we call it from vsassert. It works sometimes beeing 
    called from other dlls, but not from here :( 
    
    VsCaptureStackBacktrace and VsWalkFrameChain (StackWalk.cpp) are almost exact copy (with some extractions 
    we do not need) of the ntdll's RtlCaptureStackBackTrace and RtlWalkFrameChain and it 
    seems to work everywhere.
*/

#if 0
/*
USHORT
(WINAPI
 * _RtlCaptureStackBackTrace)(
 ULONG FramesToSkip,
 ULONG FramesToCapture,
 PVOID *BackTrace,
 PULONG BackTraceHash OPTIONAL
 );


static DYNALINKFUNC _rgNtdllFuncs[] = 
{
    "RtlCaptureStackBackTrace",  (FARPROC *)&_RtlCaptureStackBackTrace
};

static UINT _cNtdllFuncs = sizeof(_rgNtdllFuncs) / sizeof(DYNALINKFUNC);

static const char   _szNtdll[]      = "ntdll.dll";
static HINSTANCE    _hinstNtdll;


USHORT ntdll_RtlCaptureStackBackTrace(
                                      ULONG FramesToSkip,
                                      ULONG FramesToCapture,
                                      PVOID *BackTrace,
                                      PULONG BackTraceHash
                                      )
{
    static BOOL s_fInitialized = FALSE;

    if(!s_fInitialized)
    {

        if(FAILED(QueryLoadDll(_szNtdll, &_hinstNtdll, _rgNtdllFuncs, _cNtdllFuncs)))
            return 0;
        s_fInitialized = TRUE;  
    }
    
    return _RtlCaptureStackBackTrace(FramesToSkip, FramesToCapture, BackTrace, BackTraceHash);
}   
*/
#endif //0  

//------------------------------------------------------------------
///////////////////////////////////////////////////////////////////////////
//
// Imports from psapi.dll.  This is only used for Win NT
//

BOOL
(WINAPI
 *_GetModuleInformation)(
 HANDLE hProcess,
 HMODULE hModule,
 LPMODULEINFO lpmodinfo,
 DWORD cb
 );

BOOL
(WINAPI
 *_EnumProcessModules)(
 HANDLE hProcess,
 HMODULE *lphModule,
 DWORD cb,
 LPDWORD lpcbNeeded
 );

DWORD
(WINAPI
*_GetModuleFileNameExA)(
    HANDLE hProcess,
    HMODULE hModule,
    LPSTR lpFilename,
    DWORD nSize
    );
    
static DYNALINKFUNC _rgPsapiFuncs[] = 
{
    "GetModuleInformation",  (FARPROC *)&_GetModuleInformation,
    "EnumProcessModules",    (FARPROC *)&_EnumProcessModules, 
    "GetModuleFileNameExA",  (FARPROC *)&_GetModuleFileNameExA
};

static UINT _cPsapiFuncs = sizeof(_rgPsapiFuncs) / sizeof(DYNALINKFUNC);

static const char   _szPsapi[]      = "psapi.dll";
static HINSTANCE _hinstPsapi;


BOOL psapi_GetModuleInformation(    
                                HANDLE hProcess,
                                HMODULE hModule,
                                LPMODULEINFO lpmodinfo,
                                DWORD cb
                                )
{
    if(!_hinstPsapi)
    {

        if(FAILED(QueryLoadDll(_szPsapi, &_hinstPsapi, _rgPsapiFuncs, _cPsapiFuncs)))
            return FALSE;
    }

    return _GetModuleInformation(hProcess, hModule, lpmodinfo, cb);
}

BOOL psapi_EnumProcessModules(HANDLE hProcess, HMODULE *lphModule, DWORD cb, LPDWORD lpcbNeeded)
{
    if(!_hinstPsapi)
    {

        if(FAILED(QueryLoadDll(_szPsapi, &_hinstPsapi, _rgPsapiFuncs, _cPsapiFuncs)))
            return FALSE;
    }

    return _EnumProcessModules(hProcess, lphModule, cb, lpcbNeeded);
}

DWORD psapi_GetModuleFileNameExA(HANDLE hProcess, HMODULE hModule, _Out_z_cap_(nSize) LPSTR lpFilename, DWORD nSize)
{
    if(!_hinstPsapi)
    {
        if(FAILED(QueryLoadDll(_szPsapi, &_hinstPsapi, _rgPsapiFuncs, _cPsapiFuncs)))
        return FALSE;
    }
      
    return _GetModuleFileNameExA(hProcess, hModule, lpFilename, nSize);
}

//=--------------------------------------------------------------------------=
// QueryLoadDll
//=--------------------------------------------------------------------------=
// Loads the given DLL and establishes the function pointers.  If the DLL
// can't be found this displays a message and allows the user to retry
//
static
HRESULT QueryLoadDll
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
     //       char szMsg[MAX_PATH];
       //     wsprintf(szMsg, "Unable to load '%s'.  Retry?", pszDll);

    //        if(IDYES != MessageBox(NULL, szMsg, NULL, MB_YESNO | MB_ICONSTOP))
                break;
        }
    }
    return E_FAIL;
}

/*
// does not work - does not return the same address as passed to ResolveSymbol
DWORD_PTR CModAddr::GetAddressFromString(LPSTR pszString)
{
    DWORD_PTR dwAddress = 0;
    char szBuf[MAX_SYMBOLS];
    char * pszFileName = NULL;
    char * pszLineNumber = NULL;
    char * pszModuleName = NULL;
    char * pszSymbolName = NULL;

    DWORD dwLineNumber = 0;
    DWORD dwOffset = 0;

    strncpy(szBuf, pszString, MAX_SYMBOLS);
    szBuf[MAX_SYMBOLS - 1] = 0;

    pszString = szBuf;

    char * p;

    p = strchr(pszString, '(');
    if(p)
    {
        *p = 0;
        pszFileName = pszString;
        pszLineNumber = p+1;
        pszString = p+1;
        p = strchr(pszLineNumber, ')');
        if(p)
        {
            *p = 0;
            dwLineNumber = (DWORD)atoi(pszLineNumber);
            pszString = p+1;
        }
    }  

    p = strstr(pszString, " : ");
    if(p)
    {
        *p = 0;
        pszModuleName = p+3;
    }
    else
        pszModuleName = pszString;

    p = strchr(pszModuleName, '!');
    if(p)
    {
        *p = 0;
        pszSymbolName = p+1;

        // remove extension
        p = strrchr(pszModuleName, '.');
        if(p)
            *p = 0;

        p = strstr(pszSymbolName, " + ");
        if(p)
        {
            *p = 0;
            dwOffset = (DWORD)atoi(p + 3);
        }
    }

    if(pszModuleName)
    {
        if(pszFileName)
        {
            LONG lDisplacement = 0;
            IMAGEHLP_LINE li = {0};
            li.SizeOfStruct = sizeof(li);

            if (SymGetLineFromName(m_hProcess, pszModuleName, pszFileName,
                dwLineNumber, &lDisplacement, &li))
            {
                return (DWORD_PTR)li.Address;
            }
        }

        if(pszSymbolName)
        {
            __try
            {

                union 
                {
                    //CHAR rgchSymbol[sizeof(IMAGEHLP_SYMBOL64) + MAX_SYMBOLS];
                    //IMAGEHLP_SYMBOL64  sym;
                    CHAR rgchSymbol[sizeof(IMAGEHLP_SYMBOL) + MAX_SYMBOLS];
                    IMAGEHLP_SYMBOL  sym;
                } sym;

                char szString[MAX_SYMBOLS];
                sprintf(szString, "%s!%s", pszModuleName, pszSymbolName);

                memset(&sym.sym, 0, sizeof(sym.sym));
                sym.sym.SizeOfStruct = sizeof(sym.sym);
                sym.sym.MaxNameLength = MAX_SYMBOLS;
                strcpy(sym.sym.Name, szString);

                BOOL f = SymGetSymFromName(m_hProcess, szString, &sym.sym);
                if(!f)
                {
                    strcpy(sym.sym.Name, pszSymbolName);  
                    f = SymGetSymFromName(m_hProcess, pszSymbolName, &sym.sym);  
                }

                if(f)
                    dwAddress =  (DWORD_PTR)sym.sym.Address + dwOffset;
            }
            __except (EXCEPTION_EXECUTE_HANDLER)
            {
                ;
            }
        }  
    }

    return dwAddress;
}
*/

#endif //_M_IX86



