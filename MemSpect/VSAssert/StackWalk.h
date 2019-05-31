//----------------------------------------------------------------------------- 
// Microsoft Confidential 
// Copyright 2000 Microsoft Corporation. All Rights Reserved. 
// 
//----------------------------------------------------------------------------- 

#pragma once


UINT GetStackAddrs(UINT ifrStart, UINT cfrTotal, DWORD_PTR * pdwAddr, BOOL fSkipDlls = FALSE); 
BOOL GetStringFromAddr(DWORD_PTR dwAddress,	_Out_cap_(uicBuf) char * pszBuf, UINT uicBuf, BOOL fIsFunctionID = false, BOOL fNoFileLineInfo = FALSE, BOOL fIsCallingFromChildProcess=FALSE);
BOOL GetModulesAddressSpace(_In_opt_ char ** ppszModules, UINT uicModules, DWORD_PTR * pdwAddressSpace);
void StackWalkClose();

void UpdateLoadedModules();
void OnLoadModule(HMODULE hMod);
void OnSymbolsPathChanged();

UINT VsCaptureStackBackTrace(UINT FramesToSkip,
                             ULONG *BackTrace);

BOOL EnsureModAddrObject(HANDLE hProcess);

//BOOL ResolveFuncId(DWORD_PTR dwAddress,	_Out_z_cap_(uicBuf) char * pszBuf, UINT uicBuf, BOOL fNoFileLineInfo);

int GetFunctionOrClassNameFromIP(
    UINT_PTR ip,
    _Out_z_cap_(uicBuf) WCHAR * pszBuf, 
    UINT uicBuf, 
    bool fIncludeModuleName = true, 
    bool fIncludeFullPath = false,
    bool fIncludeSourceFile = false,
    bool fIsFunctionId = false
);


extern CComQIPtr<ICorProfilerInfo2 > g_pCorProfilerInfo ; // can be null
