//----------------------------------------------------------------------------- 
// Microsoft Confidential 
// Copyright 2000 Microsoft Corporation. All Rights Reserved. 
// 
//----------------------------------------------------------------------------- 

#pragma once

//--------------------------------------------------------
// CModAddr - helper class for storing addresses for loaded modules, finding a module, etc
//--------------------------------------------------------

#undef new  // we make our own


typedef struct MODULE_DATAINFO {
    LPVOID lpBaseOfDll;
    DWORD SizeOfImage;
    LPVOID EntryPoint;
} MODULEINFO, *LPMODULEINFO;

class CModAddr
{
public:
  struct MODULE_DATA
  {
      MODULE_DATA * pNext;
	  DWORD_PTR dwBaseAddress;
      DWORD_PTR dwEndAddress;
	  char szFileName[MAX_PATH];
	  HINSTANCE hInst;
  };

  #define MODULE_DATA_PAGE_SIZE 64 // number of elements on a page

  struct MODULE_DATA_PAGE
  {
    MODULE_DATA_PAGE * pPrev;
    MODULE_DATA Modules[MODULE_DATA_PAGE_SIZE];
  };

  // we should create this object in the debug heap, otherwise it
  // gets destroyed before we ready to report leaks    
  
  PVOID operator new    (size_t cb);
  void  operator delete (PVOID pv);
  
  CModAddr(HANDLE hProcess = NULL);
  ~CModAddr();
  
  void      UpdateLoadedModules();
  DWORD_PTR GetModuleBase(DWORD_PTR dwAddress, BOOL fhInst = FALSE);
  BOOL      EnsureLibLoaded(DWORD_PTR dwAddress);
  BOOL      ResolveSymbol(DWORD_PTR dwAddress, _Out_z_cap_(uicBuf) char * szBuf, UINT uicBuf, BOOL fNoFileLineInfo = FALSE);
  BOOL      VsSymGetSourceFile(DWORD_PTR dwAddress, 
                                PCTSTR pszFileName,
                                _Out_cap_(uicBuf) char * pszBuf, 
                                UINT uicBuf);
    
  //DWORD_PTR GetAddressFromString(LPSTR pszString);
  
  MODULE_DATA * FindModule(DWORD_PTR dwAddress, BOOL fhInst = FALSE, MODULE_DATA *pNewData = NULL); 
  MODULE_DATA * FindModuleByName(_In_opt_z_ char * pszDllName); // not a full path, just a name: for instance ole32.dll
  
  UINT          FindModulesAddrSpace(_In_opt_ char ** ppszModules, UINT uicModules, DWORD_PTR * pdwAddrSpace);
    
  //static BOOL      EnsureSymInitialized();
  static void      OnSymbolsPathChanged();
  
  
  MODULE_DATA * NewModule(DWORD_PTR dwAddress, BOOL fhInst = FALSE, MODULEINFO *pMI = NULL, CComBSTR bstrName = NULL);
protected:  
  
  MODULE_DATA *  m_pHead;
  MODULE_DATA *  m_pLastUsed;
  
  MODULE_DATA ** m_ppModulesToSkip;
  
  MODULE_DATA_PAGE *  m_pPage;
  UINT           m_uicPageMods;
  HANDLE m_hProcess; // inprocess: GetCurrentProcess(). Outof proc, process handle

  MODULE_DATA *  MakeSureWeHaveSpaceForNewModuleData();
  static DWORD s_dwDesiredOptions;
  static BOOL sm_fSymInitialized;
  static BOOL sm_fInitialized;
};

extern CModAddr * s_pModAddr ;

//------------------------------------------------------------------------
// helpers for import functions from a dll
//------------------------------------------------------------------------

struct DYNALINKFUNC {
  LPCSTR  szFuncName;	// name of function
  FARPROC * ppfnAddr;	// points to function address
};

HRESULT  QueryLoadDll(LPCSTR pszDll, HINSTANCE *phinst, DYNALINKFUNC *rgFuncs, UINT cFuncs);

//------------------------------------------------------------------------
// imports from psapi.dll
//------------------------------------------------------------------------

BOOL psapi_GetModuleInformation(HANDLE hProcess, HMODULE hModule, LPMODULEINFO lpmodinfo, DWORD cb);
BOOL psapi_EnumProcessModules(HANDLE hProcess, HMODULE *lphModule, DWORD cb, LPDWORD lpcbNeeded);
DWORD psapi_GetModuleFileNameExA(HANDLE hProcess, HMODULE hModule, _Out_z_cap_(nSize) LPSTR lpFilename, DWORD nSize);

//------------------------------------------------------------------------
// imports from ntdll.dll
//------------------------------------------------------------------------

USHORT ntdll_RtlCaptureStackBackTrace(
   ULONG FramesToSkip,
   ULONG FramesToCapture,
   PVOID *BackTrace,
   PULONG BackTraceHash);

