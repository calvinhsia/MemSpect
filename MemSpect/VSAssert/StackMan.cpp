#include "Pch.H"
#include "atlcom.h"


#include "vsassert.h"
#include "mem.h"
#include "stackwalk.h"
#include "ntcurteb.h" // NtCurrentTeb() impl
#include "corsym.h"


#define dimensionof(a) 		(sizeof(a)/sizeof(*(a)))
#define PASTE2(x,y) x##y
#define PASTE(x,y) PASTE2(x,y)
#define __WFUNCDNAME__ PASTE(L, __FUNCDNAME__)
/*
Add these lines to environment strings to profile other CLR apps without using Attach
Set COR_ENABLE_PROFILING=1
Set COR_PROFILER={01673DDC-46F5-454F-84BC-F2F34564C2AD}
Set COR_PROFILER_PATH=C:\Users\calvinh\documents\visual studio 2010\Projects\VBInterop\Debug\ClrProf.dll
*/


CComQIPtr<ICorProfilerInfo2 > g_pCorProfilerInfo; // can be null

MapDynStackSymNamesWrapper *g_MapDynStackSymNamesWrapper = NULL;


void LogOutput(BOOL fLoggingOn, LPCWSTR wszFormat, ...)
{
    if (fLoggingOn)
    {
        SYSTEMTIME st;
        GetLocalTime(&st);
        WCHAR buf[1000];
        WCHAR tstamp[100];
        swprintf_s(tstamp,L"%2d/%02d/%02d %2d:%02d:%02d:%03d thrd=%d ", st.wMonth, st.wDay, st.wYear-2000, st.wHour, 
                        st.wMinute, st.wSecond, st.wMilliseconds, GetCurrentThreadId());
        OutputDebugStringW(tstamp);
        va_list insertionArgs;
        va_start(insertionArgs, wszFormat);
        _vsnwprintf_s(buf,sizeof(buf), wszFormat, insertionArgs);
        va_end(insertionArgs);
        OutputDebugStringW(buf);
        OutputDebugStringW(L"\r\n");
    }
}

CLINKAGE int ENTRYPOINT GetManagedSourceInfo(
        __in mdToken mdtokenMethod,
        __in ULONG32 ilOffset,
        __in WCHAR *pszModulename,
        __in IMetaDataImport * pIMetaDataImport,
        __in_ecount(nSizeOfszSourceLine)  WCHAR *pszSourceLine,
        __in int nSizeOfszSourceLine
    )
{
    int retval = 0;
    HRESULT hr;
    CComPtr<ISymUnmanagedBinder2> spBinder;
    if ((S_OK == spBinder.CoCreateInstance(__uuidof(CorSymBinder_SxS), nullptr, CLSCTX_INPROC_SERVER)))
    {
        VSASSERT(g_pszSymbolsPath,"no path set");
        CComBSTR bstrPath(g_pszSymbolsPath);
        CComPtr<ISymUnmanagedReader> spReader;
        if ((S_OK == spBinder->GetReaderForFile2(
            pIMetaDataImport,
            pszModulename,
            bstrPath.m_str,
            AllowRegistryAccess | AllowSymbolServerAccess | AllowOriginalPathAccess | AllowReferencePathAccess,
            &spReader)))
        {
            CComPtr<ISymUnmanagedMethod> spMethod;
            if (S_OK == (spReader->GetMethod(mdtokenMethod, &spMethod)))
            {
                ULONG32 cSeqPts;
                if ((S_OK ==spMethod->GetSequencePointCount(&cSeqPts)) && cSeqPts > 0)
                {
                    std::vector<ISymUnmanagedDocument*> docs(cSeqPts);
                    std::vector<ULONG32> offsets(cSeqPts), lines(cSeqPts);
                    std::shared_ptr<void> docsGuard(nullptr, 
                        [&](void*) { 
                            for each (auto p in docs) 
                            {
                                if (p != nullptr) {
                                    p->Release();
                                }
                            }
                        }
                    );
                    if (S_OK == spMethod->GetSequencePoints(cSeqPts, nullptr, &offsets[0], &docs[0], &lines[0], nullptr, nullptr, nullptr))
                    {
                        ULONG32 i = cSeqPts - 1;
                        do
                        {
                            if (ilOffset >= offsets[i] && lines[i] != 0xFEEFEE)
                            {
                                std::wstring url(1024, wchar_t());
                                ULONG32 urlLength = url.size();
                                while (true)
                                {
                                    hr = docs[i]->GetURL(urlLength, &urlLength, &url[0]);
                                    if (hr == HRESULT_FROM_WIN32(ERROR_INSUFFICIENT_BUFFER))
                                    {
                                        url.resize(urlLength);
                                    }
                                    else
                                    {
                                        break;
                                    }
                                }
                                if (hr == S_OK)
                                {
                                    _snwprintf_s(pszSourceLine,nSizeOfszSourceLine,  _TRUNCATE, L"%s(%d)", &url[0], lines[i]);
                                    retval = wcsnlen(pszSourceLine, nSizeOfszSourceLine);
                                }
                                break;
                            }
                        } while (i--);
                    }


                }

            }

        }
    }

    return retval;
}

HRESULT STDMETHODCALLTYPE MyGetClassLayout( 
    /* [in] */ ClassID classID,
    /* [out][in] */ COR_FIELD_OFFSET rFieldOffset[  ],
    /* [in] */ ULONG cFieldOffset,
    /* [out] */ ULONG *pcFieldOffset,
    /* [out] */ ULONG *pulClassSize)
{
	HRESULT hr = S_OK;
	static int fFirstTime = true;
	hr = g_pCorProfilerInfo->GetClassLayout(classID, rFieldOffset, cFieldOffset, pcFieldOffset,pulClassSize);
	if (hr == CORPROF_E_UNSUPPORTED_CALL_SEQUENCE) //0x80131363 CORPROF_E_UNSUPPORTED_CALL_SEQUENCE  http://blogs.msdn.com/b/davbr/archive/2008/12/23/why-we-have-corprof-e-unsupported-call-sequence.aspx
	{// hack to get around 666237 ClrProfiler returns CORPROF_E_UNSUPPORTED_CALL_SEQUENCE  for CorProfilerInfo->GetClassLayout
		if (fFirstTime)
		{
			fFirstTime = false;
			DWORD addrOfGetClassLayout;
			_asm
			{
				mov ebx, g_pCorProfilerInfo
				mov ecx, [ebx] // vtable
				add ecx, 0a0h	// vtbl offset of GetClassLayout
				mov eax, [ecx]  // deref ///
				mov [addrOfGetClassLayout], eax
			}
			DWORD oldProtect;
			if (!VirtualProtect((LPVOID)addrOfGetClassLayout, /*len*/ 100, PAGE_EXECUTE_READWRITE, &oldProtect))
			{
				auto err = GetLastError();
			}
			else
			{
				_asm 
				{
					mov eax, addrOfGetClassLayout
					add eax, 24h // 733940EA  - 733940C6   = 24h
					cmp [eax], 33h // 33 c9 == xor ecx, ecx
					jne badddd
					inc eax
					cmp [eax], 0c9h // 33 c9 == xor ecx, ecx
					jne badddd
					inc eax
					cmp [eax], 41h // 41 == inc ecx
					jne badddd
					mov [eax], 90h	// nop
					badddd:
				}

				if (!VirtualProtect((LPVOID)addrOfGetClassLayout, 100, oldProtect, &oldProtect)) // restore
				{
					auto err = GetLastError();
				}
				hr = g_pCorProfilerInfo->GetClassLayout(classID, rFieldOffset, cFieldOffset, pcFieldOffset,pulClassSize);
			}

		}
	}
	return hr;
}
/*





67BCDE9A B9 98 87 CF 67       mov         ecx,67CF8798h  
						hr = g_pCorProfilerInfo->GetClassLayout(classId, 0, 0, &cFieldOffset, &ulClassSize);
67BCDE9F E8 97 50 FA FF       call        ATL::CComPtrBase<ICorProfilerInfo2>::operator-> (67B72F3Bh)  
67BCDEA4 89 85 F4 EE FF FF    mov         dword ptr [ebp-110Ch],eax  
67BCDEAA 8D 8D 0C EF FF FF    lea         ecx,[ebp-10F4h]  
67BCDEB0 51                   push        ecx  
67BCDEB1 8D 95 44 F0 FF FF    lea         edx,[ebp-0FBCh]  
67BCDEB7 52                   push        edx  
67BCDEB8 6A 00                push        0  
67BCDEBA 6A 00                push        0  
67BCDEBC 8B 85 88 F0 FF FF    mov         eax,dword ptr [ebp-0F78h]  
67BCDEC2 50                   push        eax  
67BCDEC3 8B 8D F4 EE FF FF    mov         ecx,dword ptr [ebp-110Ch]  
67BCDEC9 51                   push        ecx  
67BCDECA 8B 95 F4 EE FF FF    mov         edx,dword ptr [ebp-110Ch]  
67BCDED0 8B 02                mov         eax,dword ptr [edx]  
67BCDED2 8B 88 A0 00 00 00    mov         ecx,dword ptr [eax+0A0h]  
67BCDED8 FF D1                call        ecx  



HRESULT ProfToEEInterfaceImpl::GetClassLayout(ClassID classID,
                                             COR_FIELD_OFFSET rFieldOffset[],
                                             ULONG cFieldOffset,
                                             ULONG *pcFieldOffset,
                                             ULONG *pulClassSize)
{
733940C6 55                   push        ebp  
733940C7 8B EC                mov         ebp,esp  
733940C9 83 E4 F8             and         esp,0FFFFFFF8h  
733940CC 83 EC 1C             sub         esp,1Ch  
733940CF 53                   push        ebx  
733940D0 56                   push        esi  
733940D1 57                   push        edi  
    CONTRACTL
    {
        // Yay!
        NOTHROW;

        // Yay!
        GC_NOTRIGGER;

        // Yay!
        MODE_ANY;

        // Yay!
        EE_THREAD_NOT_REQUIRED;

        // Yay!
        CANNOT_TAKE_LOCK;

        SO_NOT_MAINLINE;

        PRECONDITION(CheckPointer(rFieldOffset, NULL_OK));
        PRECONDITION(CheckPointer(pcFieldOffset));
        PRECONDITION(CheckPointer(pulClassSize,  NULL_OK));
    }
    CONTRACTL_END;
    
    PROFILER_TO_CLR_ENTRYPOINT_SYNC_EX(kP2EEAllowableAfterAttach,
        (LF_CORPROF, 
        LL_INFO1000, 
        "**PROF: GetClassLayout 0x%p.\n", 
        classID));
733940D2 A1 98 64 7B 73       mov         eax,dword ptr [g_profControlBlock (737B6490h)+8]  
733940D7 83 F8 01             cmp         eax,1  
733940DA 75 0E                jne         ProfToEEInterfaceImpl::GetClassLayout+24h (733940EAh)  
733940DC B8 67 13 13 80       mov         eax,80131367h  
733940E1 5F                   pop         edi  
733940E2 5E                   pop         esi  
733940E3 5B                   pop         ebx  
733940E4 8B E5                mov         esp,ebp  
733940E6 5D                   pop         ebp  
733940E7 C2 18 00             ret         18h  
733940EA 33 C9                xor         ecx,ecx  
733940EC 41                   inc         ecx  
733940ED E8 2B C7 FF FF       call        AreCallbackStateFlagsSet (7339081Dh)  
733940F2 85 C0                test        eax,eax  
733940F4 75 07                jne         ProfToEEInterfaceImpl::GetClassLayout+37h (733940FDh)  
733940F6 B8 63 13 13 80       mov         eax,80131363h  
733940FB EB E4                jmp         ProfToEEInterfaceImpl::GetClassLayout+1Bh (733940E1h)  

*/        


HRESULT STDMETHODCALLTYPE MyGetModuleMetaData( 
/* [in] */ ModuleID moduleId,
/* [in] */ DWORD dwOpenFlags,
/* [in] */ REFIID riid,
/* [out] */ IUnknown **ppOut)
{
	HRESULT hr = S_OK;
	static int fFirstTime = true;
	hr = g_pCorProfilerInfo->GetModuleMetaData(moduleId, dwOpenFlags, riid, ppOut);
	if (hr == CORPROF_E_UNSUPPORTED_CALL_SEQUENCE) //0x80131363 CORPROF_E_UNSUPPORTED_CALL_SEQUENCE  http://blogs.msdn.com/b/davbr/archive/2008/12/23/why-we-have-corprof-e-unsupported-call-sequence.aspx
	{// hack to get around 666237 ClrProfiler returns CORPROF_E_UNSUPPORTED_CALL_SEQUENCE  for CorProfilerInfo->GetModuleMetaData
		if (fFirstTime)
		{
			fFirstTime = false;
			DWORD addrOfGetmoduleMetaData;
			// 708F2A0C 
			_asm
			{
				mov ebx, g_pCorProfilerInfo
				mov ecx, [ebx] // vtable
				add ecx, 54h	// vtbl offset of GetModuleMetaData
				mov eax, [ecx]  // deref ///
				mov [addrOfGetmoduleMetaData], eax
			}
			DWORD oldProtect;
			if (!VirtualProtect((LPVOID)addrOfGetmoduleMetaData, /*len*/ 100, PAGE_EXECUTE_READWRITE, &oldProtect))
			{
				auto err = GetLastError();
			}
			else
			{
				_asm 
				{
					mov eax, addrOfGetmoduleMetaData
					add eax, 23h // 708F2A2f - 708F2A0C  = 23h
					cmp [eax], 33h // 33 c9 == xor ecx, ecx
					jne badddd
					inc eax
					cmp [eax], 0c9h // 33 c9 == xor ecx, ecx
					jne badddd
					inc eax
					cmp [eax], 41h // 41 == inc ecx
					jne badddd
					mov [eax], 90h	// nop
					badddd:
				}

				if (!VirtualProtect((LPVOID)addrOfGetmoduleMetaData, 100, oldProtect, &oldProtect)) // restore
				{
					auto err = GetLastError();
				}
				hr = g_pCorProfilerInfo->GetModuleMetaData(moduleId, dwOpenFlags, riid, ppOut);
			}

		}
	}
	return hr;
}

/*
BaseAddressAllocationBaseRegionSizeRegionEnd Size10        Num4K     Num64K    WSet      WSetPct   Data                                                                                  Protect           State         Type
706e0000  706e0000  00001000  706e0fff  4096          1         0.0625    4096      100       C:\Windows\Microsoft.NET\Framework\v4.0.30319\clr.dll                                 READONLY          COMMIT        MEM_IMAGE
706e1000  706e0000  00001000  706e1fff  4096          1         0.0625    4096      100       C:\Windows\Microsoft.NET\Framework\v4.0.30319\clr.dll                                 EXECUTE_READWRITE COMMIT        MEM_IMAGE
706e2000  706e0000  00634000  70d15fff  6504448       1588      99.25     1880064   29        C:\Windows\Microsoft.NET\Framework\v4.0.30319\clr.dll                                 EXECUTE_READ      COMMIT        MEM_IMAGE
70d16000  706e0000  0000b000  70d20fff  45056         11        0.6875    45056     100       C:\Windows\Microsoft.NET\Framework\v4.0.30319\clr.dll                                 READWRITE         COMMIT        MEM_IMAGE
70d21000  706e0000  00004000  70d24fff  16384         4         0.25      4096      25        C:\Windows\Microsoft.NET\Framework\v4.0.30319\clr.dll                                 WRITECOPY         COMMIT        MEM_IMAGE
70d25000  706e0000  00003000  70d27fff  12288         3         0.1875    12288     100       C:\Windows\Microsoft.NET\Framework\v4.0.30319\clr.dll                                 READWRITE         COMMIT        MEM_IMAGE
70d28000  706e0000  00001000  70d28fff  4096          1         0.0625    0         0         C:\Windows\Microsoft.NET\Framework\v4.0.30319\clr.dll                                 WRITECOPY         COMMIT        MEM_IMAGE
70d29000  706e0000  00001000  70d29fff  4096          1         0.0625    4096      100       C:\Windows\Microsoft.NET\Framework\v4.0.30319\clr.dll                                 READWRITE         COMMIT        MEM_IMAGE
70d2a000  706e0000  00003000  70d2cfff  12288         3         0.1875    12288     100       C:\Windows\Microsoft.NET\Framework\v4.0.30319\clr.dll                                 WRITECOPY         COMMIT        MEM_IMAGE
70d2d000  706e0000  0004d000  70d79fff  315392        77        4.8125    4096      1         C:\Windows\Microsoft.NET\Framework\v4.0.30319\clr.dll                                 READONLY          COMMIT        MEM_IMAGE

	hr = g_pCorProfilerInfo->GetModuleMetaData(moduleId, dwOpenFlags, riid, ppOut);
67AAD253 E8 E3 5C FC FF       call        ATL::CComPtrBase<ICorProfilerInfo2>::operator-> (67A72F3Bh)  
67AAD258 89 45 F4             mov         dword ptr [ebp-0Ch],eax  
67AAD25B 8B 45 14             mov         eax,dword ptr [ppOut]  
67AAD25E 50                   push        eax  
67AAD25F 8B 4D 10             mov         ecx,dword ptr [riid]  
67AAD262 51                   push        ecx  
67AAD263 8B 55 0C             mov         edx,dword ptr [dwOpenFlags]  
67AAD266 52                   push        edx  
67AAD267 8B 45 08             mov         eax,dword ptr [moduleId]  
67AAD26A 50                   push        eax  
67AAD26B 8B 4D F4             mov         ecx,dword ptr [ebp-0Ch]  
67AAD26E 51                   push        ecx  
67AAD26F 8B 55 F4             mov         edx,dword ptr [ebp-0Ch]  
67AAD272 8B 02                mov         eax,dword ptr [edx]  
67AAD274 8B 48 54             mov         ecx,dword ptr [eax+54h]  
67AAD277 FF D1                call        ecx  



HRESULT ProfToEEInterfaceImpl::GetModuleMetaData(ModuleID    moduleId,
    DWORD       dwOpenFlags,
    REFIID      riid,
    IUnknown    **ppOut)
{
708F2A0C 6A 3C                push        3Ch  
708F2A0E B8 20 23 C1 70       mov         eax,offset g_CwStd+3EA9F8h (70C12320h)  
708F2A13 E8 91 E6 DE FF       call        _EH_prolog3_catch (706E10A9h)  
    CONTRACTL
    {
        // Yay!
        NOTHROW;

        // Yay!
        GC_NOTRIGGER;

        // Yay!
        MODE_ANY;

        // Currently, this function is technically EE_THREAD_REQUIRED because
        // some functions in synch.cpp assert that there is a Thread object,
        // but we might be able to lift that restriction and make this be
        // EE_THREAD_NOT_REQUIRED.

        // PEFile::GetRWImporter & PEFile::GetEmitter &
        // GetReadablePublicMetaDataInterface take locks
        CAN_TAKE_LOCK;

        SO_NOT_MAINLINE;
    }
    CONTRACTL_END;

    PROFILER_TO_CLR_ENTRYPOINT_SYNC_EX(kP2EEAllowableAfterAttach,
        (LF_CORPROF, 
        LL_INFO1000, 
        "**PROF: GetModuleMetaData 0x%p, 0x%08x.\n", 
        moduleId, 
        dwOpenFlags));
708F2A18 A1 98 64 D1 70       mov         eax,dword ptr [g_profControlBlock (70D16490h)+8]  
708F2A1D 83 F8 01             cmp         eax,1  
708F2A20 75 0D                jne         ProfToEEInterfaceImpl::GetModuleMetaData+23h (708F2A2Fh)  
708F2A22 B8 67 13 13 80       mov         eax,80131367h  
708F2A27 E8 D4 E5 DE FF       call        _EH_epilog3 (706E1000h)  
708F2A2C C2 14 00             ret         14h  
708F2A2F 33 C9                xor         ecx,ecx  
    CONTRACTL
    {
        // Yay!
        NOTHROW;

        // Yay!
        GC_NOTRIGGER;

        // Yay!
        MODE_ANY;

        // Currently, this function is technically EE_THREAD_REQUIRED because
        // some functions in synch.cpp assert that there is a Thread object,
        // but we might be able to lift that restriction and make this be
        // EE_THREAD_NOT_REQUIRED.

        // PEFile::GetRWImporter & PEFile::GetEmitter &
        // GetReadablePublicMetaDataInterface take locks
        CAN_TAKE_LOCK;

        SO_NOT_MAINLINE;
    }
    CONTRACTL_END;

    PROFILER_TO_CLR_ENTRYPOINT_SYNC_EX(kP2EEAllowableAfterAttach,
        (LF_CORPROF, 
        LL_INFO1000, 
        "**PROF: GetModuleMetaData 0x%p, 0x%08x.\n", 
        moduleId, 
        dwOpenFlags));
708F2A31 41                   inc         ecx  
708F2A32 E8 E6 DD FF FF       call        AreCallbackStateFlagsSet (708F081Dh)  
708F2A37 85 C0                test        eax,eax  
708F2A39 75 07                jne         ProfToEEInterfaceImpl::GetModuleMetaData+36h (708F2A42h)  
708F2A3B B8 63 13 13 80       mov         eax,80131363h  
708F2A40 EB E5                jmp         ProfToEEInterfaceImpl::GetModuleMetaData+1Bh (708F2A27h)  

    if (moduleId == NULL)
708F2A42 8B 7D 0C             mov         edi,dword ptr [moduleId]  

*/

int GetFunctionOrClassNameFromIPEx(
    UINT_PTR ip,
    _Out_z_cap_(uicBuf) WCHAR * pszBuf, 
    UINT uicBuf, 
    bool fIncludeModuleName /*= true*/, 
    bool fIncludeFullPath /*= false*/,
    bool fIncludeSourceFile/*=false*/,
    bool fIsFunctionId /*=false*/) // for JIT, IP is a functionId, rather than an IP
{
    FunctionID functionID;
    int nResultLen =0;
    pszBuf[0] = 0;
    ClassID classID;
    ModuleID moduleID;
    mdToken mdtokenMethod;
    HRESULT hr =S_OK;
    CComPtr<IMetaDataImport> pIMetaDataImport = 0;
    if (!g_pCorProfilerInfo)
    {
        goto error;
    }
    if (ip == 0)
    {
        functionID = 0;
    }
    else
    {
        if (fIsFunctionId)
        {
            functionID = ip;
            ip = 0;
        }
        else
        {
            if ((hr = g_pCorProfilerInfo->GetFunctionFromIP((LPCBYTE) ip, &functionID)) != S_OK)
            {
                _snwprintf_s(pszBuf, uicBuf,_TRUNCATE, L"Error %s GetFunctionFromIP failed ip=%x hr=0x%x", __WFUNCDNAME__, ip, hr);
                goto error;
            }
        }
    }
#if MSDEBUG
//    functionID = 1; // force a crash to test exception handling

#endif MSDEBUG


    hr = g_pCorProfilerInfo->GetFunctionInfo2(functionID,0, &classID, &moduleID, &mdtokenMethod, 0, 0, 0);

    if (hr != S_OK)
    {
        _snwprintf_s(pszBuf, uicBuf,_TRUNCATE, L"Error %s GetFunctionInfo2 hr=0x%x", __WFUNCDNAME__, hr);
        goto error;
    }
    // 0x80131363 CORPROF_E_UNSUPPORTED_CALL_SEQUENCE  http://blogs.msdn.com/b/davbr/archive/2008/12/23/why-we-have-corprof-e-unsupported-call-sequence.aspx
    hr = MyGetModuleMetaData( moduleID, ofRead, IID_IMetaDataImport, (LPUNKNOWN *)&pIMetaDataImport );
    if (hr != S_OK)
    {
        if (hr == CORPROF_E_UNSUPPORTED_CALL_SEQUENCE)
        {
            _snwprintf_s(pszBuf, uicBuf,_TRUNCATE, L"CORPROF_E_UNSUPPORTED_CALL_SEQUENCE when trying to GetModuleMetaData moduleId = %x", moduleID);
        }
        else
        {
            _snwprintf_s(pszBuf, uicBuf,_TRUNCATE, L"Error %s GetModuleMetaData hr=0x%x", __WFUNCDNAME__, hr);
        }
        goto error;
    }
    mdToken mdTokClass = 0;
    WCHAR szClassName[MAXCLASSNAMELEN] = {0};
    WCHAR szMethodName[500] = {0};
    WCHAR szModulename[MAX_PATH]={0};
    WCHAR szSourceLine[MAX_PATH] = {0};
    hr  = pIMetaDataImport->GetMethodProps(mdtokenMethod, &mdTokClass, szMethodName, dimensionof(szMethodName), NULL,NULL,NULL,NULL,NULL,NULL);
    if (hr != S_OK)
    {
        _snwprintf_s(pszBuf, uicBuf,_TRUNCATE, L"Error %s GetMethodProps hr=0x%x", __WFUNCDNAME__, hr);
        goto error;
    }
    if (classID)
    {
        nResultLen= GetClassNameFromClassId(szClassName, dimensionof(szClassName), classID, /*ObjecId=*/0, /*fExpandSystemStringObjectId=*/0);
#if MSDEBUG
        VSASSERT(nResultLen, "Couldn't get clas name from classid?");
#endif MSDEBUG
    }
    else
    {
        if (mdTokClass)
        {
            mdToken tkExtends;
            ULONG typdef;
            hr = pIMetaDataImport->GetTypeDefProps(mdTokClass, szClassName, dimensionof(szClassName), &typdef, NULL, &tkExtends);

            if (hr == S_OK)
            {
                _snwprintf_s(pszBuf, uicBuf,_TRUNCATE, L"%s", szClassName);
				nResultLen = wcsnlen(pszBuf, uicBuf);

                HCORENUM henum = 0;
#define MAXGENERICPARAM 20
                mdToken tokBuf[MAXGENERICPARAM];
                ULONG nGenParams = 0;
                CComPtr<IMetaDataImport2> pIMetaDataImport2;
                hr = pIMetaDataImport->QueryInterface(IID_IMetaDataImport2, (void **)&pIMetaDataImport2);

                hr = pIMetaDataImport2->EnumMethodSpecs(&henum, mdTokClass, tokBuf, MAXGENERICPARAM, &nGenParams);
                if (hr == S_OK)
                {
                    for (ULONG i  = 0 ; i < nGenParams; i++)
                    {
                        //WCHAR ParmName[512];
                        mdToken tkParent;
                        PCCOR_SIGNATURE psig;
                        ULONG nSigLen = 0;

                        hr = pIMetaDataImport2->GetMethodSpecProps(tokBuf[i], &tkParent, &psig, &nSigLen);
                        if (hr == S_OK)
                        {
//xxx                            CComBSTR bstrType = ElemTypeToString(pIMetaDataImport, ELEMENT_TYPE_GENERICINST, &psig);
                        }
                    }
                    pIMetaDataImport->CloseEnum(henum);
                }


//                HCORENUM henum = 0;
//#define MAXGENERICPARAM 20
//                mdToken tokBuf[MAXGENERICPARAM];
//                ULONG nGenParams = 0;
//                hr = pIMetaDataImport->EnumGenericParams(&henum, mdTokClass,tokBuf,MAXGENERICPARAM,&nGenParams);
//                if (hr == S_OK)
//                {
//                    for (ULONG i  = 0 ; i < nGenParams; i++)
//                    {
//                        WCHAR ParmName[512];
//                        hr = pIMetaDataImport->GetGenericParamProps(tokBuf[i], NULL, NULL, NULL, NULL, ParmName, sizeof(ParmName), NULL);
//                    }
//                    pIMetaDataImport->CloseEnum(henum);
//                }
                hr = S_OK;
            }
        }
    }
    if (fIncludeModuleName || fIncludeSourceFile)
    {
        LPCBYTE pBaseLoadAddress;
        AssemblyID assemblyId;
        ULONG cchName = 0;
        hr = g_pCorProfilerInfo->GetModuleInfo(moduleID, &pBaseLoadAddress, dimensionof(szModulename), &cchName, szModulename, &assemblyId);
        if (hr != S_OK)
        {
            _snwprintf_s(pszBuf, uicBuf,_TRUNCATE, L"Error %s GetModuleInfo hr=0x%x", __WFUNCDNAME__, hr);
            goto error;
        }
    }
    if (fIncludeSourceFile && g_ShowManagedSourceInStacks)
    {
        ULONG32 cCodeInfos = 0;
        ULONG32 ilOffset=0;
        if ( ip != 0 && (S_OK == g_pCorProfilerInfo->GetCodeInfo2(functionID, 0, &cCodeInfos, nullptr)) && cCodeInfos > 0)
        {
            vector<COR_PRF_CODE_INFO,MySTLAlloc<COR_PRF_CODE_INFO> > codeInfos(MySTLAlloc<COR_PRF_CODE_INFO>(InternalHeapToUse));
            codeInfos.resize(cCodeInfos);
            if ((S_OK == g_pCorProfilerInfo->GetCodeInfo2(functionID, codeInfos.size(), nullptr, &codeInfos[0])))
            {
                UINT_PTR nativeOffset = 0;
                for each (auto codeInfo in codeInfos)
                {
                    if (ip >= codeInfo.startAddress && ip < codeInfo.startAddress + codeInfo.size)
                    {
                        nativeOffset += ip - codeInfo.startAddress;
                        break;
                    }
                    else
                    {
                        nativeOffset += codeInfo.size;
                    }
                }
                bool fGotit = false;
                ULONG32 cMaps = 0;
                if (S_OK == g_pCorProfilerInfo->GetILToNativeMapping(functionID, 0, &cMaps, nullptr) && cMaps > 0 )
                {
                    vector<COR_DEBUG_IL_TO_NATIVE_MAP, MySTLAlloc<COR_DEBUG_IL_TO_NATIVE_MAP>> maps(MySTLAlloc<COR_DEBUG_IL_TO_NATIVE_MAP>(InternalHeapToUse));
                    maps.resize(cMaps);
                    if (S_OK == g_pCorProfilerInfo->GetILToNativeMapping(functionID, cMaps, &cMaps, &maps[0]))
                    {
                        auto it = std::find_if(
                            maps.begin(),
                            maps.end(),
                            [&](const COR_DEBUG_IL_TO_NATIVE_MAP& m)
                            {
                                return nativeOffset >= m.nativeStartOffset && nativeOffset < m.nativeEndOffset;
                            });
                        if (it != maps.end())
                        {
                            ilOffset = it->ilOffset;
                            if (ilOffset != NO_MAPPING)
                            {
                                if (ilOffset == PROLOG)
                                {
                                    ilOffset = 0;
                                }
                                else if (ilOffset == EPILOG)
                                {
                                    ilOffset = -1;
                                }
                                fGotit = true;
                            }
                        }
                    }
                }
                if (fGotit || ip == 0)
                {
                    int nLen = GetManagedSourceInfo(mdtokenMethod, ilOffset, szModulename, pIMetaDataImport, szSourceLine, dimensionof(szSourceLine));
                }
            }

        }

    }
    if (!fIncludeFullPath && fIncludeModuleName)
    {
        WCHAR szFname[_MAX_FNAME];
        WCHAR szExt[_MAX_EXT]; // includes "."
        _wsplitpath_s(szModulename, NULL,0 , NULL,0, szFname, _MAX_FNAME, szExt, _MAX_EXT);
        _snwprintf_s(szModulename,_TRUNCATE, L"%s%s", szFname,szExt);
    }
    if (szSourceLine[0])
    {
        _snwprintf_s(pszBuf, uicBuf,_TRUNCATE, L"%s : %s!%s.%s", szSourceLine, szModulename, szClassName, szMethodName);
    }
    else
    {
        _snwprintf_s(pszBuf, uicBuf,_TRUNCATE, L"%s!%s.%s%s", szModulename, szClassName, szMethodName, szSourceLine);
    }
error:
	nResultLen = wcsnlen(pszBuf,uicBuf);
    return nResultLen;
}


int GetFunctionOrClassNameFromIP(
    UINT_PTR ip,
    _Out_z_cap_(uicBuf) WCHAR * pszBuf, 
    UINT uicBuf, 
    bool fIncludeModuleName /*= true*/, 
    bool fIncludeFullPath /*= false*/,
    bool fIncludeSourceFile/*=false*/,
    bool fIsFunctionId /*=false*/
)
{
    int retVal = 0;
	__try
	{
		retVal = GetFunctionOrClassNameFromIPEx(ip,  pszBuf, uicBuf, fIncludeModuleName, fIncludeFullPath, fIncludeSourceFile, fIsFunctionId);
	} __except(EXCEPTION_EXECUTE_HANDLER) {
		swprintf_s(pszBuf, uicBuf, L"FuncIdFail: %x", ip);
		retVal = wcsnlen(pszBuf,uicBuf);
	}
    return retVal;
}



#define PROF_NOT_IMP(methodName, ...) \
 STDMETHOD (methodName) (__VA_ARGS__) \
    { \
        LogOutput(m_fLoggingOn, __WFUNCDNAME__ );  \
        return S_OK;  \
    } \

class MyProfilerBase:
    public ICorProfilerCallback3
{
public:
    MyProfilerBase()
    {
        m_fLoggingOn = false;
    }
    virtual HRESULT OnInitializeForAttach(IUnknown *pICorProfilerInfoUnk, void* data, UINT datasize )=0;
    virtual HRESULT OnInitialize(IUnknown *pICorProfilerInfoUnk)=0;
    virtual void OnShutdown()=0;

    // ICorProfilerCallback2 >>

    // STARTUP/SHUTDOWN EVENTS
    STDMETHOD(Initialize)( IUnknown *pICorProfilerInfoUnk)
    {
        return OnInitialize(pICorProfilerInfoUnk);
    }

    STDMETHOD(Shutdown)()
    {
        OnShutdown();
        return S_OK;
    }

    // APPLICATION DOMAIN EVENTS
    PROF_NOT_IMP (AppDomainCreationStarted, AppDomainID appDomainId);
    PROF_NOT_IMP (AppDomainCreationFinished, AppDomainID appDomainId, HRESULT hr);
    PROF_NOT_IMP (AppDomainShutdownStarted, AppDomainID);
    PROF_NOT_IMP (AppDomainShutdownFinished, AppDomainID appDomainId, HRESULT hr);

    // ASSEMBLY EVENTS
    PROF_NOT_IMP (AssemblyLoadStarted, AssemblyID);
    PROF_NOT_IMP (AssemblyLoadFinished, AssemblyID, HRESULT);
    PROF_NOT_IMP (AssemblyUnloadStarted, AssemblyID);
    PROF_NOT_IMP (AssemblyUnloadFinished, AssemblyID assemblyID, HRESULT hr);

    // MODULE EVENTS
    PROF_NOT_IMP (ModuleLoadStarted, ModuleID);
    PROF_NOT_IMP (ModuleLoadFinished, ModuleID moduleID, HRESULT hr);
    PROF_NOT_IMP (ModuleUnloadStarted, ModuleID moduleId);
    PROF_NOT_IMP (ModuleUnloadFinished, ModuleID, HRESULT);
    PROF_NOT_IMP (ModuleAttachedToAssembly, ModuleID moduleID, AssemblyID assemblyID);

    // CLASS EVENTS
    PROF_NOT_IMP (ClassLoadStarted, ClassID classId);
    PROF_NOT_IMP (ClassLoadFinished, ClassID classId, HRESULT hr);
    PROF_NOT_IMP (ClassUnloadStarted, ClassID classId);
    PROF_NOT_IMP (ClassUnloadFinished, ClassID, HRESULT);
    PROF_NOT_IMP (FunctionUnloadStarted, FunctionID);

    // JIT EVENTS
    PROF_NOT_IMP (JITCompilationStarted, FunctionID functionID, BOOL fIsSafeToBlock);
    PROF_NOT_IMP (JITCompilationFinished, FunctionID functionID, HRESULT hrStatus, BOOL fIsSafeToBlock);
    PROF_NOT_IMP (JITCachedFunctionSearchStarted, FunctionID functionId, BOOL *pbUseCachedFunction);
    PROF_NOT_IMP (JITCachedFunctionSearchFinished, FunctionID, COR_PRF_JIT_CACHE);
    PROF_NOT_IMP (JITFunctionPitched, FunctionID);
    PROF_NOT_IMP (JITInlining, FunctionID, FunctionID, BOOL*);

    // THREAD EVENTS
    PROF_NOT_IMP (ThreadCreated, ThreadID);
    PROF_NOT_IMP (ThreadDestroyed, ThreadID);
    PROF_NOT_IMP (ThreadAssignedToOSThread, ThreadID, DWORD);

    // REMOTING EVENTS
    // Client-side events
    PROF_NOT_IMP (RemotingClientInvocationStarted);
    PROF_NOT_IMP (RemotingClientSendingMessage, GUID*, BOOL);
    PROF_NOT_IMP (RemotingClientReceivingReply, GUID*, BOOL);
    PROF_NOT_IMP (RemotingClientInvocationFinished);
    // Server-side events
    PROF_NOT_IMP (RemotingServerReceivingMessage, GUID*, BOOL);
    PROF_NOT_IMP (RemotingServerInvocationStarted);
    PROF_NOT_IMP (RemotingServerInvocationReturned);
    PROF_NOT_IMP (RemotingServerSendingReply, GUID*, BOOL);

    // CONTEXT EVENTS
    PROF_NOT_IMP (UnmanagedToManagedTransition, FunctionID, COR_PRF_TRANSITION_REASON);
    PROF_NOT_IMP (ManagedToUnmanagedTransition, FunctionID, COR_PRF_TRANSITION_REASON);

    // SUSPENSION EVENTS
    PROF_NOT_IMP (RuntimeSuspendStarted, COR_PRF_SUSPEND_REASON);
    PROF_NOT_IMP (RuntimeSuspendFinished);
    PROF_NOT_IMP (RuntimeSuspendAborted);
    PROF_NOT_IMP (RuntimeResumeStarted);
    PROF_NOT_IMP (RuntimeResumeFinished);
    PROF_NOT_IMP (RuntimeThreadSuspended, ThreadID);
    PROF_NOT_IMP (RuntimeThreadResumed, ThreadID);

    // GC EVENTS
    PROF_NOT_IMP (MovedReferences, ULONG cmovedObjectIDRanges, ObjectID oldObjectIDRangeStart[], ObjectID newObjectIDRangeStart[], ULONG cObjectIDRangeLength[]);
    PROF_NOT_IMP (ObjectAllocated, ObjectID objectID, ClassID classID);
    PROF_NOT_IMP (ObjectsAllocatedByClass, ULONG classCount, ClassID classIDs[], ULONG objects[]);
    PROF_NOT_IMP (ObjectReferences, ObjectID objectID, ClassID classID, ULONG cObjectRefs, ObjectID objectRefIDs[]);
    PROF_NOT_IMP (RootReferences, ULONG cRootRefs, ObjectID rootRefIDs[]);

    // Exception creation
    PROF_NOT_IMP (ExceptionThrown, ObjectID);

    // Exception Caught
    PROF_NOT_IMP (ExceptionCatcherEnter, FunctionID, ObjectID);
    PROF_NOT_IMP (ExceptionCatcherLeave);

    // Search phase
    PROF_NOT_IMP (ExceptionSearchFunctionEnter, FunctionID);
    PROF_NOT_IMP (ExceptionSearchFunctionLeave);
    PROF_NOT_IMP (ExceptionSearchFilterEnter, FunctionID);
    PROF_NOT_IMP (ExceptionSearchFilterLeave);
    PROF_NOT_IMP (ExceptionSearchCatcherFound, FunctionID);

    // Unwind phase
    PROF_NOT_IMP (ExceptionUnwindFunctionEnter, FunctionID);
    PROF_NOT_IMP (ExceptionUnwindFunctionLeave);
    PROF_NOT_IMP (ExceptionUnwindFinallyEnter, FunctionID);
    PROF_NOT_IMP (ExceptionUnwindFinallyLeave);

    PROF_NOT_IMP (ExceptionCLRCatcherFound); // Deprecated in .Net 2.0
    PROF_NOT_IMP (ExceptionCLRCatcherExecute); // Deprecated in .Net 2.0

    PROF_NOT_IMP (ExceptionOSHandlerEnter, FunctionID); // Not implemented
    PROF_NOT_IMP (ExceptionOSHandlerLeave, FunctionID); // Not implemented

    // IID_ICorProfilerCallback2 EVENTS
    PROF_NOT_IMP (ThreadNameChanged, ThreadID threadId, ULONG cchName, __in WCHAR name[]);
    PROF_NOT_IMP (GarbageCollectionStarted, int cGenerations, BOOL generationCollected[], COR_PRF_GC_REASON reason);
    PROF_NOT_IMP (SurvivingReferences, ULONG cSurvivingObjectIDRanges, ObjectID objectIDRangeStart[], ULONG cObjectIDRangeLength[]);
    PROF_NOT_IMP (GarbageCollectionFinished);
    PROF_NOT_IMP (FinalizeableObjectQueued, DWORD finalizerFlags, ObjectID objectID);
    PROF_NOT_IMP (RootReferences2, ULONG cRootRefs, ObjectID rootRefIds[], COR_PRF_GC_ROOT_KIND rootKinds[], COR_PRF_GC_ROOT_FLAGS rootFlags[], UINT_PTR rootIds[]);
    PROF_NOT_IMP (HandleCreated, GCHandleID handleId, ObjectID initialObjectId);
    PROF_NOT_IMP (HandleDestroyed, GCHandleID handleId);

    // COM CLASSIC VTable
    PROF_NOT_IMP (COMClassicVTableCreated, ClassID wrappedClassID, REFGUID implementedIID, void *pVTable, ULONG cSlots);

    PROF_NOT_IMP (COMClassicVTableDestroyed, ClassID wrappedClassID, REFGUID implementedIID, void *pVTable);
    // ICorProfilerCallback2 <<

    STDMETHOD (InitializeForAttach) ( IUnknown* punk, void* data, UINT datasize )
    {
        g_pCorProfilerInfo = punk;
        return OnInitializeForAttach(punk, data, datasize);
    }

    PROF_NOT_IMP (ProfilerAttachComplete);

    PROF_NOT_IMP (ProfilerDetachSucceeded);
    // ICorProfilerCallback3 <<



/*
    static  CComBSTR GetClassNameFromClassId(ClassID classId, bool fIncludeModuleName = true, bool fIncludeFullPath = 0)
    {
        CComBSTR bstrClassName(L"");
        if (classId == 0)
        {
            return L"";
        }
        ModuleID moduleID;
        mdTypeDef typeDefToken;
        HRESULT hr = g_pCorProfilerInfo->GetClassIDInfo(classId, &moduleID, &typeDefToken);
        if (hr != S_OK)
        {
            return LogOutput(true, L"Error %s GetClassIDInfo hr=0x%x.\n ", __WFUNCDNAME__, hr);
        }
        if (fIncludeModuleName)
        {
            WCHAR szModulename[MAX_PATH];
            LPCBYTE pBaseLoadAddress;
            AssemblyID assemblyId;
            ULONG cchName = 0;
            hr = g_pCorProfilerInfo->GetModuleInfo(moduleID, &pBaseLoadAddress, dimensionof(szModulename), &cchName, szModulename, &assemblyId);
            if (hr != S_OK)
            {
                return LogOutput(true, L"Error %s GetModuleInfo hr=0x%x.\n ", __WFUNCDNAME__, hr);
            }
            if (fIncludeFullPath)
            {
                bstrClassName.Append(szModulename);
            }
            else
            {
                char szFname[_MAX_FNAME];
                char szExt[_MAX_EXT]; // includes "."
                USES_CONVERSION;
                _splitpath_s(W2A(szModulename), NULL,0 , NULL,0, szFname, _MAX_FNAME, szExt, _MAX_EXT);
                bstrClassName.Append(szFname);
                bstrClassName.Append(szExt);
            }
            bstrClassName.Append(L"!");
        }
        CComPtr<IMetaDataImport> pIMetaDataImport = 0;
        hr = g_pCorProfilerInfo->GetModuleMetaData( moduleID, ofRead,IID_IMetaDataImport,(LPUNKNOWN *)&pIMetaDataImport );
        WCHAR wszTypeDef[512];
        DWORD cchTypeDef=0;
        hr = pIMetaDataImport->GetTypeDefProps( typeDefToken, wszTypeDef, dimensionof(wszTypeDef), &cchTypeDef, 0, 0 );
        if (hr != S_OK)
        {
            return LogOutput(true, L"Error %s GetTypeDefProps hr=0x%x.\n ", __WFUNCDNAME__, hr);
        }
        bstrClassName.Append(wszTypeDef);
        return bstrClassName;
    }
    */
protected:

    BOOL m_fLoggingOn; // logging at this inheritance level
};

CProcessGC *g_pCProcessGC =0;



// {01673DDC-46F5-454F-84BC-F2F34564C2AD}
extern const CLSID clsidMyProfiler; 
//{ 0x1673ddc, 0x46f5, 0x454f, { 0x84, 0xbc, 0xf2, 0xf3, 0x45, 0x64, 0xc2, 0xad } };

class MyProfiler:
    public CComObjectRootEx<CComSingleThreadModel>, 
    public CComCoClass<MyProfiler, &clsidMyProfiler>,
    public MyProfilerBase
{
    public:
        BEGIN_COM_MAP(MyProfiler)
            COM_INTERFACE_ENTRY_IID(clsidMyProfiler, MyProfiler)
            COM_INTERFACE_ENTRY(ICorProfilerCallback2)
            COM_INTERFACE_ENTRY(ICorProfilerCallback3)
        END_COM_MAP()
        DECLARE_NOT_AGGREGATABLE(MyProfiler)
        DECLARE_NO_REGISTRY()
public:
    MyProfiler()
    {
        m_fLoggingOn = false;
        if (!g_fIsChildProcess)
        {
            /*
            CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally
            if (MessageBoxW(0,
                LogOutput(true, L"CLR Profiler starting pid = %d. DebugBreak() ?", GetCurrentProcessId()), 
                L"Attach a debugger if you want", 
                MB_YESNO) == IDYES)
            {
                _asm int 3
            }
            */
        }
    }
    ~MyProfiler()
    {
        LogOutput(m_fLoggingOn,__WFUNCDNAME__ );
    }
    HRESULT OnInitializeForAttach(IUnknown *pICorProfilerInfoUnk, void* data, UINT datasize )
    {
        CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally
        LogOutput(m_fLoggingOn, __WFUNCDNAME__ );
        MessageBeep(MB_ICONINFORMATION);
        HRESULT hr = g_pCorProfilerInfo->SetEventMask(
            COR_PRF_ALLOWABLE_AFTER_ATTACH & ~COR_PRF_MONITOR_GC
            );
        if (hr != S_OK)
        {
            LogOutput(true, L"Error %s hr=0x%x.\n ", __WFUNCDNAME__, hr);
        }
        return hr;
    }
    HRESULT OnInitialize(IUnknown *pICorProfilerInfoUnk){
        CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally
#if MSDEBUG
//		Sleep(1000); // sleepimmersive
#endif MSDEBUG
        HRESULT hr = E_FAIL;
        LogOutput(m_fLoggingOn, __WFUNCDNAME__ );
        m_fLoggingOn = false;
        if (!g_fIsChildProcess && !g_fIsRunDLL)
        {
            g_pCorProfilerInfo = pICorProfilerInfoUnk;
            DoClrObjTrkToggle(g_TrackClrObjects);
            g_pCProcessGC = new CProcessGC();
            hr =S_OK;
        }
        return hr;
    }

    STDMETHOD  (ObjectAllocated) ( ObjectID objectID, ClassID classID)
    {
        HRESULT hr = S_OK;
        if (g_TrackClrObjects)
        {
            ULONG ulSize = 0;
            g_pCorProfilerInfo->GetObjectSize(objectID, &ulSize);
            hr = g_pCProcessGC->TrackObject(bt_ClrObject, (DWORD) objectID, ulSize, classID);

            if (m_fLoggingOn)
            {
                WCHAR wszClass[500] = {0};
                mdTypeDef tdToken;
                ModuleID moduleID;
                hr = g_pCorProfilerInfo->GetClassIDInfo(classID, &moduleID , &tdToken); 
                if (hr == S_OK)
                {
                    CComPtr<IMetaDataImport> pIMetaDataImport = 0;
                    hr = MyGetModuleMetaData( moduleID, ofRead, IID_IMetaDataImport, (LPUNKNOWN *)&pIMetaDataImport );
                    if (hr == S_OK)
                    {
                        ULONG cchBuf = dimensionof(wszClass);
                        hr = pIMetaDataImport->GetTypeDefProps( tdToken, wszClass, dimensionof(wszClass), &cchBuf, 0, 0 );
                    }
                }
                VSASSERT(true,"");
            }
            
        }
        return hr;
    }
    STDMETHOD (GarbageCollectionStarted) ( int cGenerations, BOOL generationCollected[], COR_PRF_GC_REASON reason)
    {
        return g_pCProcessGC->GarbageCollectionStarted(cGenerations, generationCollected, reason);
    }

    STDMETHOD (GarbageCollectionFinished)()
    {
        HRESULT hr = g_pCProcessGC->GarbageCollectionFinished();
        return hr;
    }


    STDMETHOD (MovedReferences) ( ULONG cmovedObjectIDRanges, ObjectID oldObjectIDRangeStart[], ObjectID newObjectIDRangeStart[], ULONG cObjectIDRangeLength[])
    {
        return g_pCProcessGC->MovedReferences(cmovedObjectIDRanges, oldObjectIDRangeStart, newObjectIDRangeStart, cObjectIDRangeLength);
        
    }
    STDMETHOD (SurvivingReferences) ( ULONG cSurvivingObjectIDRanges, ObjectID objectIDRangeStart[], ULONG cObjectIDRangeLength[])
    {
        return g_pCProcessGC->SurvivingReferences(cSurvivingObjectIDRanges, objectIDRangeStart, cObjectIDRangeLength);
    }

    STDMETHOD(RootReferences2) ( ULONG cRootRefs, ObjectID rootRefIds[], COR_PRF_GC_ROOT_KIND rootKinds[], COR_PRF_GC_ROOT_FLAGS rootFlags[], UINT_PTR rootIds[])
    {
        return g_pCProcessGC->RootReferences2(cRootRefs, rootRefIds, rootKinds, rootFlags, rootIds);
    }

    STDMETHOD (ObjectReferences) ( ObjectID objectID, ClassID classID, ULONG cObjectRefs, ObjectID objectRefIDs[])
    {
        return g_pCProcessGC->ObjectReferences( objectID, classID, cObjectRefs, objectRefIDs);
    }

    STDMETHOD (JITCompilationFinished)( FunctionID functionID, HRESULT hrStatus, BOOL fIsSafeToBlock)
    {
        return g_pCProcessGC->JITCompilationFinished(functionID, hrStatus, fIsSafeToBlock);
    }

    STDMETHOD (ExceptionThrown)(ObjectID objectID)
    {
        return g_pCProcessGC->ExceptionThrown(objectID);
    }



    STDMETHOD (HandleCreated) ( GCHandleID handleId, ObjectID initialObjectId)
    {
        return g_pCProcessGC->HandleCreated( handleId, initialObjectId);
    }

    STDMETHOD (HandleDestroyed)( GCHandleID handleId)
    {
        return g_pCProcessGC->HandleDestroyed(handleId);
    }


    STDMETHOD (ClassLoadFinished) ( ClassID classId, HRESULT hrStatus)
    {
        HRESULT hr = S_OK;
        if (hrStatus == S_OK)
        {
            hr = g_pCProcessGC->TrackObject(bt_ClrClass, (DWORD) classId, 0);
        }
        return hr;
    }

    STDMETHOD (ClassUnloadFinished) (ClassID classId, HRESULT hrStatus)
    {
        HRESULT hr = S_OK;
        if (hrStatus == S_OK)
        {
            hr = g_pCProcessGC->TrackObjectFree(bt_ClrClass, (DWORD) classId);
        }
        return hr;
    }

    STDMETHOD (ModuleLoadFinished) (ModuleID moduleID, HRESULT hrStatus)
    {
        HRESULT hr = S_OK;
        if (hrStatus == S_OK)
        {
            hr = g_pCProcessGC->TrackObject(bt_ClrModule, (DWORD) moduleID, 0);
        }
        return hr;
    }

    STDMETHOD (ModuleUnloadFinished) (ModuleID moduleID, HRESULT hrStatus)
    {
        HRESULT hr = S_OK;
        if (hrStatus == S_OK)
        {
            hr = g_pCProcessGC->TrackObjectFree(bt_ClrModule, (DWORD) moduleID);
        }
        return hr;
    }

    STDMETHOD (AssemblyLoadFinished) (AssemblyID assemblyID, HRESULT hrStatus)
    {
        HRESULT hr = S_OK;
        if (hrStatus == S_OK)
        {
            hr = g_pCProcessGC->TrackObject(bt_ClrAssembly, (DWORD) assemblyID, 0);
        }
        return hr;
    }

    STDMETHOD (AssemblyUnloadFinished) (AssemblyID assemblyID, HRESULT hrStatus)
    {
        HRESULT hr = S_OK;
        if (hrStatus == S_OK)
        {
            hr = g_pCProcessGC->TrackObjectFree(bt_ClrAssembly, (DWORD) assemblyID);
        }
        return hr;
    }


    STDMETHOD (AppDomainCreationFinished) (AppDomainID appDomainId, HRESULT hrStatus)
    {
        HRESULT hr = S_OK;
        if (hrStatus == S_OK)
        {
            hr = g_pCProcessGC->TrackObject(bt_ClrAppDomain, (DWORD) appDomainId, 0);
        }
        return hr;
    }

    STDMETHOD (AppDomainShutdownFinished) (AppDomainID appDomainId, HRESULT hrStatus)
    {
        HRESULT hr = S_OK;
        if (hrStatus == S_OK)
        {
            hr = g_pCProcessGC->TrackObjectFree(bt_ClrAppDomain, (DWORD) appDomainId);
        }
        return hr;
    }

    STDMETHOD (ThreadCreated) ( ThreadID managedthreadid)
    {
        HRESULT hr = S_OK;
        return hr;
    }
    STDMETHOD (ThreadDestroyed) ( ThreadID managedthreadid)
    {
        HRESULT hr = S_OK;
        return hr;
    }
    STDMETHOD (ThreadAssignedToOSThread) ( ThreadID managedthreadid, DWORD osThreadId)
    {
        HRESULT hr = S_OK;
        return hr;
    }



    STDMETHOD (RuntimeSuspendFinished)()
    {
        HRESULT hr = S_OK;
        if (g_pCProcessGC)
        {
            hr = g_pCProcessGC->RuntimeSuspendFinished();
        }
        return hr;
    }
    
    void OnShutdown()
    {
        CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally
        
        ShutDownRCWCleanupStuff();

        if (g_pCProcessGC != NULL)
        {
            delete g_pCProcessGC ;
            g_pCProcessGC = NULL;
        }

        LogOutput(m_fLoggingOn, __WFUNCDNAME__ );
        _AtlComModule.Term();
    }

    static DWORD m_dwEventMask; // current events being tracked

protected:
    BOOL m_fLoggingOn; // logging at this inheritance level
};


DWORD MyProfiler::m_dwEventMask=0; // current events being tracked

const CLSID clsidMyProfiler = 
{ 0x1673ddc, 0x46f5, 0x454f, { 0x84, 0xbc, 0xf2, 0xf3, 0x45, 0x64, 0xc2, 0xad } };

OBJECT_ENTRY_AUTO(clsidMyProfiler, MyProfiler)

//#pragma comment(linker, "/EXPORT:DllGetClassObject=_DllGetClassObject@12,PRIVATE")

STDAPI DllGetClassObject(__in REFCLSID rclsid, __in REFIID riid, __deref_out LPVOID FAR* ppv)
{
    HRESULT hr = E_FAIL;
    if (!g_fIsChildProcess)
    {
        if (!g_NativeOnly)
        {
            CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally
            hr = AtlComModuleGetClassObject(&_AtlComModule, rclsid, riid, ppv);
        }
    }
    return hr;
}

//CAtlModuleT<CComModule> _mod;

//CAtlComModule	_AtlComModule;
class CClrProfModule : public ATL::CAtlDllModuleT< CClrProfModule >
{
#if MSDEBUG
public:
    CClrProfModule()
    {
        int x=0;

    }
    ~CClrProfModule()
    {
        int x=0;

    }
#endif MSDEBUG
};

CClrProfModule _AtlModule;

//--------------------------------------------------------------
// VsWalkFrameChain
//--------------------------------------------------------------
/*
    Description:

    This function tries to walk the EBP chain and fill out a vector of
    return addresses. The function works only on x86. It is possible that
    the function cannot fill the requested number of callers because somewhere
    on the stack we have a function compiled FPO (the frame register (EBP) is
    used as a normal register. In this case the function will just return with
    a less then requested count. 

    Return value:

    The number of identified return addresses on the stack. This can be less
    then the Count requested if the stack ends or we encounter a FPO compiled
    function.

//--------------------------------------------------------------
    olgaark: for some reason if we call RtlCaptureStackBackTrace from ntdll.dll 
    it never works on WinXP when we call it from vsassert. It works sometimes beeing 
    called from other dlls, but not from here :( 
    
    VsCaptureStackBacktrace and VsWalkFrameChain are almost exact copies (with some extractions 
    we do not need) of the ntdll's RtlCaptureStackBackTrace and RtlWalkFrameChain and it 
    seems to work everywhere.

*/


#define SIZE_1_KB  ((ULONG_PTR) 0x400)
/*
ebp
*(int *)(ebp+4)
*(int *)ebp
*(int *)(*((int *)ebp)+4)
*(int *)*(int *)ebp
*(int *)(*((int *)*(int *)ebp)+4)
*(int *)*(int *)*(int *)ebp
*(int *)(*((int *)*(int *)*(int *)ebp)+4)



ShellShim__CorExeMain


--- f:\dd\ndp\clr\src\dlls\shell_shim\shellshim.cpp ----------------------------

//
// Initialize the shim, sets g_shimImplStatus, and returns the latest version
// Note: Need to call delete[] on the returned LPWSTR
//
void InitShimImpl()
{
7038637B 8B FF                mov         edi,edi  
7038637D 55                   push        ebp  
7038637E 81 EC 08 02 00 00    sub         esp,208h  
70386384 8D 6C 24 FC          lea         ebp,[esp-4]  
70386388 A1 80 10 3C 70       mov         eax,dword ptr [___security_cookie (703C1080h)]  
7038638D 33 C5                xor         eax,ebp  
7038638F 89 85 08 02 00 00    mov         dword ptr [ebp+208h],eax  
70386395 6A 24                push        24h  
70386397 B8 34 BC 3B 70       mov         eax,offset __ehhandler$?InitShimImpl@@YGXXZ (703BBC34h)  
7038639C E8 4F AE FF FF       call        _EH_prolog3 (703811F0h)  
    NewHolder<WCHAR> lpwszLatestVersion = NULL;
703863A1 E9 17 64 00 00       jmp         InitShimImpl+26h (7038C7BDh)  
703863A6 81 C5 0C 02 00 00    add         ebp,20Ch  
703863AC C9                   leave  
703863AD C3                   ret  

*/


HRESULT Framewalker::WalkSomeNativeFrames(ULONG StartingFp)
{
    HRESULT hr = S_OK;
    ULONG ReturnAddress;
    ULONG Fp = StartingFp;

    bool fNextFrameIsDefinitelyNative = true;// first frame is always native for sure, even when skipping
    
    while (nFrameIndex < g_nStackFrames && Fp)  // while we haven't filled the buffer yet
    {
        ReturnAddress = *((PULONG_PTR)(Fp + sizeof(ULONG_PTR)));
        if (ReturnAddress == 0 ) //|| ReturnAddress == 0xcccccccc)
        {
            hr = S_FALSE;
            break;
        }
        if ((StackStart < ReturnAddress && ReturnAddress < StackEnd) ) // the FP should be on the stack, but we can't return to the stack
        {
            hr = S_FALSE;
            break;
        }
        if (FramesToSkip == 0) // any more left to skip 
        {
            if (fNextFrameIsDefinitelyNative) // optimization: don't check if it's managed if we already know it's native
            {
                fNextFrameIsDefinitelyNative = false;
            }
            else
            {
                if (g_pCorProfilerInfo) // looking for managed?
                {
                    FunctionID functionID = 0;
                    g_pCorProfilerInfo->GetFunctionFromIP((LPCBYTE) ReturnAddress, &functionID);
                    if (functionID) // got managed code
                    {
                        break;
                    }
                }
            }
            if (!g_fHandle4gigStacks)
            {
                // SqlCE and ShellShim__CorExeMain seem to use FPO
//                VSASSERTF(((ReturnAddress & MANAGED_STACK_FLAG) == 0,"Stack address (%x) in high memory: use the fHandle4gigStacks INI file option File %s(%d)", ReturnAddress, __FILE__, __LINE__));
                BackTrace[nFrameIndex ++] = ReturnAddress;
            }
            else
            {
                StackFrameIndex ndx = GetOrCreateStackFrameIndexFromAddress((PVOID)ReturnAddress, /*fIsManaged=*/false);
                BackTrace[nFrameIndex ++] = ndx;
            }
        }
        else
        {
            FramesToSkip --;
        }
        
        ULONG NewFp = *(ULONG *)Fp; // get next stack frame (native or managed)
        if (! (Fp < NewFp  && NewFp < StackEnd)) 
        {
            hr = S_FALSE;
            break;
        }
        Fp = NewFp;
    }
    return hr; // S_OK means continue walking, else means stop walking
}

//static 
StackFrameIndex Framewalker::GetOrCreateStackFrameIndexFromAddress(LPVOID pAddress, bool fIsManaged)
{
    StackFrameIndex ndx = 0;
    VSASSERT(g_fHandle4gigStacks,"GetOrCreateStackFrameIndexFromAddress requires g_fHandle4gigStacks");
    if (g_StackFrameMapWrapper) // can be null when shutting down and yet another heap gets created
    {
        auto findRes = g_StackFrameMapWrapper->m_pStlType->find(pAddress);
        if (findRes == g_StackFrameMapWrapper->m_pStlType->end())
        {
            InterlockedIncrement((LONG *)&g_StackframeIndex);
            ndx = g_StackframeIndex;
            if (fIsManaged)
            {
                ndx |= MANAGED_STACK_FLAG;
            }
            g_StackFrameMapWrapper->m_pStlType->insert(pair<LPVOID,StackFrameIndex>(pAddress, ndx));                    

    //        auto test = GetRealAddressFromStackFrameIndex(ndx);
    //        VSASSERT(test.first == pAddress,"GetRealAddressFromStackFrameIndex not right");
        }
        else
        {
            ndx = findRes->second;
        }
    }
    return ndx;
}

//static 
pair<LPVOID, StackFrameIndex> Framewalker::GetRealAddressFromStackFrameIndex(StackFrameIndex ndx)
{
    pair<LPVOID, StackFrameIndex>  retval;
    StackFrameMap::iterator res = find_if(
        g_StackFrameMapWrapper->m_pStlType->begin(), 
        g_StackFrameMapWrapper->m_pStlType->end(), 
        [ndx](pair<LPVOID, BOOL>  p) ->bool
            {
                if (p.second == ndx)
                    return true;
                return false;
            }
        );
        
    if (res == g_StackFrameMapWrapper->m_pStlType->end())
    {
        retval = pair<LPVOID, StackFrameIndex>((LPVOID)0,0);
    }
    else
    {
        retval= pair<LPVOID, StackFrameIndex>(res->first, res->second);
    }
    return retval;;
}

//static 
void Framewalker::ShutDownFramewalker()
{
    if (g_StackFrameMapWrapper)
    {
        g_StackFrameMapWrapper->freemem();
        g_StackFrameMapWrapper = NULL;
    }
}

bool AddToDynStackSymNames(UINT_PTR ip,bool fIsFunctionId)
{
    bool fRetval = false;
#if MSDEBUG
    VSASSERT(g_DynStackSymRes,"dynstacksymnames");
#endif MSDEBUG
    if (g_MapDynStackSymNamesWrapper == NULL)
    {
        g_MapDynStackSymNamesWrapper = new (DebugAlloc(sizeof(MapDynStackSymNamesWrapper))) 
            MapDynStackSymNamesWrapper(less<FunctionID>(), MySTLAlloc<pair<FunctionID, CSymName *> > (InternalHeapToUse));
    }
    if (g_MapDynStackSymNamesWrapper->m_pStlType->find(ip) == g_MapDynStackSymNamesWrapper->m_pStlType->end())
    {
        WCHAR szbuf[MAXCLASSNAMELEN];
        CSymName *pCSymName ;
        auto result = GetFunctionOrClassNameFromIP(
            ip,  
            szbuf, 
            dimensionof(szbuf), 
            /*fIncludeModuleName =*/true, 
            /*fIncludeFullPath  = */false,
            /*fIncludeSourceFile= */true,
            fIsFunctionId
            );

        if (!result)
        {
            //_snwprintf_s(szbuf, dimensionof(szbuf), _TRUNCATE, L"%x", ip);
            //pCSymName = CSymName::CreateInstance(szbuf, CSymName::SymNameFlag_Error);
        }
        else
        {
            pCSymName = CSymName::CreateInstance(szbuf);
            auto res = g_MapDynStackSymNamesWrapper->m_pStlType->insert(pair<UINT_PTR, CSymName*>(ip, pCSymName));
            VSASSERTF((res.second == true,"errror inserting symname %s", szbuf));
            fRetval=true;
        }
    }
    return fRetval;
}


Framewalker::StackFrameMapWrapper * Framewalker::g_StackFrameMapWrapper  = NULL;

StackFrameIndex Framewalker::g_StackframeIndex = 0;

HRESULT __stdcall StackCallback(
    FunctionID functionID, 
    UINT_PTR ip, 
    COR_PRF_FRAME_INFO frameInfo, 
    ULONG32 contextSize, 
    BYTE context[], 
    void *clientData)
{
    HRESULT hr = S_OK;
    Framewalker *pFramewalker = (Framewalker*)clientData;
    if (functionID == 0) // native ?
    {
        CONTEXT *pContext = (CONTEXT *)context;
        hr = pFramewalker->WalkSomeNativeFrames(pContext->Ebp);
    }
    else
    {   // managed
        if (pFramewalker->FramesToSkip == 0)
        {
#if MSDEBUG
            FunctionID tempFuncId;
            hr = g_pCorProfilerInfo->GetFunctionFromIP((LPCBYTE)ip, &tempFuncId);
            VSASSERT(hr == S_OK && tempFuncId == functionID,"GetFunctionFromIp != Funcid");
#endif MSDEBUG
            // now we have the functionID of the caller
            if (g_DynStackSymRes)
            {
                AddToDynStackSymNames(functionID,/*bool fIsFunctionId=*/ true);
            }
            if (!g_fHandle4gigStacks)
            {
//                VSASSERTF(((functionID & MANAGED_STACK_FLAG) == 0,"managed Stack address in high memory (%x): use the fHandle4gigStacks INI file option File %s(%d)", functionID, __FILE__, __LINE__));
                pFramewalker->BackTrace[pFramewalker->nFrameIndex++] =(ULONG) (ip | MANAGED_STACK_FLAG );
            } 
            else
            {
                StackFrameIndex ndx = Framewalker::GetOrCreateStackFrameIndexFromAddress((PVOID)ip, /*fIsManaged=*/true);
                pFramewalker->BackTrace[pFramewalker->nFrameIndex++] =(ULONG) ndx;
            }
            if (pFramewalker->nFrameIndex >= g_nStackFrames)
            {
                hr  = S_FALSE;   // don't call us back anymore
            }
        }
        else
        {
            pFramewalker->FramesToSkip --; // extremely unlikely, but for the sake of completeness
        }
    }
    return hr;
}

UINT VsCaptureStackBackTrace(UINT FramesToSkip,
                             ULONG *BackTrace)
{
    HRESULT hr = E_FAIL;
    ULONG_PTR Fp;
    _asm mov Fp, ebp; // get the frame pointer using inline asm

    auto stackEnd = (ULONG_PTR)(NtCurrentTeb()->NtTib.StackBase)-4;

    Framewalker info = {
                                BackTrace, 
                                0, 
                                FramesToSkip,
                                Fp,
                                stackEnd
                                };
    hr = info.WalkSomeNativeFrames(Fp);
    if (hr == S_OK &&  // got some frames, still more to go
        g_pCorProfilerInfo &&  // do managed
        info.nFrameIndex  < g_nStackFrames ) // more frames to get?
    {
        hr = g_pCorProfilerInfo->DoStackSnapshot(0, &StackCallback, /*COR_PRF_SNAPSHOT_X86_OPTIMIZED |*/ COR_PRF_SNAPSHOT_REGISTER_CONTEXT, &info, 0, 0);
    }
    return info.nFrameIndex;    // return # frames found
}

//BOOL ResolveFuncId(
//    DWORD_PTR dwAddress,    
//    _Out_z_cap_(uicBuf) char * pszBuf, 
//    UINT uicBuf, 
//    BOOL fNoFileLineInfo)
//{
////    VSASSERT(CanDoManagedStackWalk(),"Managed stack walks aren't enabled");
//	BOOL retVal = true;
//
//	//Looking up this address seems to crash the target process.  The try-except below doesn't catch and avoid the crash, possibly
//	//due to stack corruption.  So ignore this particular address (and maybe any others that occur in the future) until we can 
//	//determine why it is being passed in.
//	if (dwAddress == 0x400000e5)
//	{
//		sprintf_s(pszBuf, uicBuf, "Error resolving managed address: %x", dwAddress);
//		return true;
//	}
//	else
//	{
//		__try
//		{
//			retVal = GetFunctionOrClassNameFromFunctionId((FunctionID) dwAddress,  pszBuf, uicBuf, /*fIncludeModuleName =*/true);
//		} __except(EXCEPTION_EXECUTE_HANDLER) {
//			//sprintf_s(pszBuf, uicBuf, "Error resolving managed address: %x", dwAddress);
//            strcpy_s(pszBuf, uicBuf, "SymResFail");
//			retVal = false;
//		}
//	}
//
//	return retVal;
//}

HRESULT DoForceGC(int fDoRCWCleanup, bool fFreezeThreads)
{
    HRESULT hr = S_OK;
    if (g_pCorProfilerInfo)
    {
		if ((g_CleanUpRCWs & 2) == 0) // do GC on main MemSpect thread (not-immersive)
		{
			if (fDoRCWCleanup)
			{
				// just do RCW cleanup on backthread
				GarbageCollectOrCleanupRCWs();
			}
			// do GC on main priv thread
			VSASSERTF((g_isImmersive == 0," Doing GC on main memspect thread requires non-immersive")); // technically, Java doesn't 
//            LockCritSect;

			hr = g_pCorProfilerInfo->ForceGC(); 
		}
		else
		{
			if (fDoRCWCleanup || g_isImmersive)
			{
				// can't call g_pCorProfilerInfo->ForceGC: immersive apps will run managed code on our private thread, so we must call GC on our other private thread which can run managed
				GarbageCollectOrCleanupRCWs();
			}
		}
    }
    return hr;
}

int GetTypeOfToken(_Out_z_cap_(noutbufSize) WCHAR *outbuf, int noutbufSize, CComPtr<IMetaDataImport> pIMetaDataImport, mdToken tok)
{
    HRESULT hr;
    outbuf[0] = 0;
#if MSDEBUG
    VSASSERTF((!IsNilToken(tok),"got nil token? %x", tok));
#endif MSDEBUG
    if (!IsNilToken(tok))
    {
        switch(TypeFromToken(tok))
        {
        case mdtTypeDef://0x02000000 within the module
            hr = pIMetaDataImport->GetTypeDefProps( tok, outbuf, noutbufSize,0,0,0);
			// could return hr	0x00131106 : Data value was truncated.	HRESULT
    //        swprintf_s(temp,dimensionof(temp),L"GENINST Argc %d %x %s", argCnt, tok, wtemp2);

            //while (argCnt--);
            //{
            //    *wtemp2=0;
            //    *pSig += CorSigUncompressToken(*pSig, &tkGeneric);
            //    if (TypeFromToken(tkGeneric) == mdtBaseType)
            //    {
            //        CComBSTR t = ElemTypeToString(pIMetaDataImport, (CorElementType) RidFromToken(tkGeneric), pSig);
            //        pRetval.Append("GenInst ");
            //        pRetval.Append (t);
            //    }
            //    else
            //    {
            //        hr = pIMetaDataImport->GetTypeDefProps( tkGeneric, wtemp2, dimensionof(wtemp2),0,0,0);
            //        VSASSERTF((hr == S_OK, "ElemTypeToString GetTypeDefProps 2 failed"));
            //        swprintf_s(temp,dimensionof(temp),L"GENINST Argc %d %x %s", argCnt, tkGeneric, wtemp2);
            //        pRetval.Append(temp);
            //    }
            //}

                    

            break;
        case mdtTypeRef:
            hr = pIMetaDataImport->GetTypeRefProps(tok,0, outbuf, noutbufSize, 0);
#if MSDEBUG
            VSASSERTF((hr == S_OK, "GetTypeOfToken GetTypeRefProps failed"));
#endif MSDEBUG

            break;
        case mdtTypeSpec:
#if MSDEBUG
            VSASSERTF((false, "GetTypeOfToken got mdtTypeSpec?"));
#endif MSDEBUG
            break;
        }
    }
    return wcsnlen(outbuf, noutbufSize);
}


int ElemTypeToString(
    _Out_z_cap_(noutbufSize) WCHAR *outbuf, 
    int noutbufSize, 
    CComPtr<IMetaDataImport> pIMetaDataImport,  
    CorElementType elemType, 
    PCCOR_SIGNATURE *pSig //=0
)
{
    int nResultLen = 0;
    outbuf[0] = 0;
    WCHAR *pwcElemName = NULL;
    switch(elemType)
    {
        case ELEMENT_TYPE_VOID:
            pwcElemName = L"Void";
            break;

        case ELEMENT_TYPE_BOOLEAN:
            pwcElemName = L"Boolean";
            break;

        case ELEMENT_TYPE_CHAR:
            pwcElemName = L"Char";
            break;

        case ELEMENT_TYPE_I1:
            pwcElemName = L"I1";
            break;

        case ELEMENT_TYPE_U1:
            pwcElemName = L"U1";
            break;

        case ELEMENT_TYPE_I2:
            pwcElemName = L"I2";
            break;

        case ELEMENT_TYPE_U2:
            pwcElemName = L"U2";
            break;

        case ELEMENT_TYPE_I4:
            pwcElemName = L"I4";
            break;

        case ELEMENT_TYPE_U4:
            pwcElemName = L"U4";
            break;

        case ELEMENT_TYPE_I8:
            pwcElemName = L"I8";
            break;

        case ELEMENT_TYPE_U8:
            pwcElemName = L"U8";
            break;

        case ELEMENT_TYPE_R4:
            pwcElemName = L"R4";
            break;

        case ELEMENT_TYPE_R8:
            pwcElemName = L"R8";
            break;

        case ELEMENT_TYPE_STRING:
            pwcElemName = L"String";
            break;

        case ELEMENT_TYPE_PTR:
            pwcElemName = L"Ptr";
            break;

        case ELEMENT_TYPE_BYREF:
            pwcElemName = L"ByRef";
            break;

        case ELEMENT_TYPE_VALUETYPE:
            pwcElemName = L"ValueType";
            break;
        case ELEMENT_TYPE_CLASS:
            if (pSig)
            {
                mdToken tok;
                *pSig += CorSigUncompressToken(*pSig, &tok);
                GetTypeOfToken(outbuf, noutbufSize, pIMetaDataImport, tok);
            }
            else
            {
                pwcElemName = L"Class";
            }
            break;

        case ELEMENT_TYPE_ARRAY:
        case ELEMENT_TYPE_SZARRAY:
            pwcElemName = L"Array";
            break;

        case ELEMENT_TYPE_OBJECT:
            pwcElemName = L"Object";
            break;

        case ELEMENT_TYPE_GENERICINST:
            if (pSig && pIMetaDataImport)
            {
                auto elemKind = 0;
                auto argCnt = 0;
                WCHAR wtemp2[500]={0};
                mdToken tkGeneric = 0;
                *pSig += CorSigUncompressData(*pSig,(ULONG *) &elemKind);
                *pSig += CorSigUncompressToken(*pSig, &tkGeneric);
                *pSig += CorSigUncompressData(*pSig,(ULONG *) &argCnt);
                
                //swprintf_s(temp,dimensionof(temp),L" GENINST Argc %d %x ek=%x ", argCnt, tkGeneric, elemKind);
                //pRetval.Append(temp);

				if (!IsNilToken(tkGeneric))
                {
                    nResultLen += GetTypeOfToken(outbuf, noutbufSize, pIMetaDataImport, tkGeneric);
                }
//                    swprintf_s(temp,dimensionof(temp),L"GENINST Argc %d %x %s", argCnt, tkGeneric, wtemp2);
                if (argCnt)
                {
                    if (wcsncpy_s(outbuf + nResultLen, noutbufSize- nResultLen, L"<",_TRUNCATE) == 0)
					{
						nResultLen +=1;
					}
                    for (int argNdx = 0 ; argNdx < argCnt ; argNdx++)
                    {
                        *wtemp2=0;
                        CorElementType elemt;
                        *pSig += CorSigUncompressData(*pSig, (ULONG *) &elemt);
                        nResultLen += ElemTypeToString(outbuf + nResultLen, noutbufSize - nResultLen, pIMetaDataImport, elemt, pSig);
                        if (argNdx +1< argCnt)
                        {
                            wcsncpy_s(outbuf+nResultLen, noutbufSize-nResultLen, L", ",_TRUNCATE);
                            nResultLen =wcsnlen(outbuf, noutbufSize);                        }
                    }
                    if (wcsncpy_s(outbuf+nResultLen, noutbufSize-nResultLen, L">",_TRUNCATE)==0)
					{
	                    nResultLen++;
					}
                }


//                swprintf_s(temp,dimensionof(temp),L"GENINST eKind %d  Tok = %x tokType = %x  #args = %d  %s", elemKind, tkGeneric, tokType, argCnt, wtemp2);
            }
            else
            {
                pwcElemName = L"GENERICINST";
            }
            break;

        case ELEMENT_TYPE_U:
            pwcElemName = L"UINT";
            break;

        case ELEMENT_TYPE_I:
            pwcElemName = L"INT"; // like an IntPtr array
            break;

        case ELEMENT_TYPE_CMOD_REQD:
            pwcElemName = L"CMOD_REQD";
            break;

        case ELEMENT_TYPE_CMOD_OPT:
            pwcElemName = L"CMOD_OPT";
            break;

        case ELEMENT_TYPE_VAR:
            {
                ULONG pos;
                *pSig += CorSigUncompressData(*pSig, &pos);
                ULONG next;
                *pSig += CorSigUncompressData(*pSig, &next);
                _snwprintf_s(outbuf, noutbufSize,_TRUNCATE ,L"arg # %d %x", pos, next);
            }
            break;
        default:
            _snwprintf_s(outbuf, noutbufSize,_TRUNCATE ,L"ELEMTYPE=%x", elemType);
            break;
            
    }
    if (pwcElemName)
    {
#if MSDEBUG
		VSASSERT(outbuf[0] == 0, "outbuf should be 0");
#endif MSDEBUG
		_snwprintf_s(outbuf, noutbufSize, _TRUNCATE, L"%s", pwcElemName);
    }
    nResultLen = wcsnlen(outbuf, noutbufSize);
    return nResultLen;
}


int GetSystemStringExpansion(ObjectID objectId, _Out_z_cap_(_SizeInWords) WCHAR *pwszStr, _In_ int _SizeInWords , int *pexceptionCode )
{
    int nLen = 0;
    __try
    {
        DWORD * ptr= (DWORD*) objectId;
        nLen = ptr[1];
        if (nLen > 0)
        {
            nLen = min(nLen, _SizeInWords - 1); // # unicode chars
            memcpy(pwszStr, (WCHAR *)&ptr[2], 2 * nLen); // need to make sure the entire string is accessible
            pwszStr[nLen ] = 0;
        }
    } __except(EXCEPTION_EXECUTE_HANDLER) {
        // swallow
        nLen = 0;
        *pexceptionCode = GetExceptionCode();
        *pwszStr = 0;
    }
    return nLen;

}

int GetNestedClassInfo(
    _Out_z_cap_(noutbufSize) WCHAR outbuf[], 
    int noutbufSize, 
    mdToken tdToken,
    CComPtr<IMetaDataImport> pIMetaDataImport, 
    ULONG32 cNumTypeArgs, 
    ClassID typeArgs[]
)
{
    HRESULT hr;
    int nResultLen = 0;
    outbuf[0] = 0;
    mdToken tdEnclosingClass;

    hr = pIMetaDataImport->GetNestedClassProps(tdToken, &tdEnclosingClass);
    if (hr == S_OK)
    {
        hr = pIMetaDataImport->GetTypeDefProps( tdEnclosingClass, outbuf, noutbufSize, 0, 0, 0 );
        nResultLen = wcsnlen(outbuf, noutbufSize);
        if (hr == S_OK)
        {
            if (cNumTypeArgs>0)
            {
                if (wcsncpy_s(outbuf + nResultLen, noutbufSize - nResultLen, L"<",_TRUNCATE) ==0)
				{
					nResultLen ++;
				}
                for (DWORD iArg = 0 ; iArg < cNumTypeArgs ; iArg++)
                {
                    nResultLen += GetClassNameFromClassId(outbuf + nResultLen, noutbufSize - nResultLen, typeArgs[iArg], /*ObjecId=*/0, /*fExpandSystemStringObjectId=*/0);
                    if (wcsncpy_s(outbuf + nResultLen, noutbufSize - nResultLen, (iArg == cNumTypeArgs -1 ?  L">" : L","),_TRUNCATE) == 0)
					{
	                    nResultLen++;
					}
                }
            }
        }
        else
        {
            wcsncpy_s(outbuf + nResultLen, noutbufSize - nResultLen, L"<EmptyNest>",_TRUNCATE);
            nResultLen  = wcsnlen(outbuf, noutbufSize);
        }
    }
    else
    {
        if (cNumTypeArgs)
        {
//                        bstrResult.Append("<NotNested>");
        }
    }
    return nResultLen;
}

int g_xdummy =0;
bool IsValidClassId(ClassID classId)
{
    bool retVal = true;
	__try
	{
//        classId = 0;
		 g_xdummy = *(int *)classId;
	} __except(EXCEPTION_EXECUTE_HANDLER) {

		retVal=false;//sprintf_s(szBuf, "MgdCLsNamFail: %x %x", classId, objectId);
	}
    return retVal;
}


int GetClassNameFromClassIdEx(
    _Out_cap_ (noutBufSize) WCHAR outbuf[], 
    int noutBufSize, 
    ClassID classId, 
    ObjectID objectId, 
    bool fExpandSystemString// = false
)
{
    int nResultLen = 0;
    outbuf[0] = 0;
    ModuleID moduleID=0;
    mdTypeDef tdToken;
    ClassID classIdParent;
    ClassID typeArgs[20];
    ULONG32 cNumTypeArgs;
    CorElementType elemType;
    ClassID arrayClassId;
    ULONG arrayRank;
    HRESULT hr = S_OK;
    if (classId == NULL && objectId != NULL)
    {
        // can't call GetClassFromObject: CORPROF_E_NOT_MANAGED_THREAD 
        classId = *(DWORD *)objectId;
//        hr = pCorProfilerInfo->GetClassFromObject(objectId, &classId);
        if (hr != S_OK || classId == NULL)
        {
            hr = E_FAIL;
        }
    }
#if MSDEBUG
//    classId= 1; // force a crash to test exception handling

#endif MSDEBUG
    if ( hr == S_OK && !IsValidClassId(classId))
    {
		_snwprintf_s(outbuf, noutBufSize, _TRUNCATE, L"ClsIdFail: %x %x", classId, objectId);
		nResultLen  = wcsnlen(outbuf,noutBufSize);
        hr = E_FAIL;
    }
    if (hr == S_OK && 
        S_OK == (hr = g_pCorProfilerInfo->IsArrayClass(classId, &elemType, &arrayClassId, &arrayRank)))
    {

        cNumTypeArgs = dimensionof(typeArgs);
        CComPtr<IMetaDataImport> pIMetaDataImport;
        hr = g_pCorProfilerInfo->GetClassIDInfo2(arrayClassId, &moduleID, &tdToken, &classIdParent, cNumTypeArgs, &cNumTypeArgs, typeArgs);
        if (hr == S_OK && moduleID != 0)
        {
            hr = MyGetModuleMetaData( moduleID, ofRead, IID_IMetaDataImport, (LPUNKNOWN *)&pIMetaDataImport );
            if (hr == S_OK && pIMetaDataImport)
            {
                int nDelt = GetNestedClassInfo( outbuf + nResultLen, noutBufSize- nResultLen, tdToken, pIMetaDataImport, cNumTypeArgs, typeArgs);

                if (nDelt > 0)
                {
                    nResultLen+=nDelt;
                    if (wcsncpy_s(outbuf+nResultLen, noutBufSize - nResultLen, L"+",_TRUNCATE) ==0)
					{
						nResultLen += 1;
					}
                }
            }
            //if (elemType == ELEMENT_TYPE_CLASS)
            //{
            //    bstrArrayType = GetClassNameFromClassId(arrayClassId, objectId, /*fExpandSystemString=*/ 0); // recur
            //    //swprintf_s(wszTemp, dimensionof(wszTemp), L"(Azray #dim=%d) ", arrayRank);
            //    //bstrResult.Append(wszTemp);            
            //    bstrResult.Append(bstrArrayType);
            //}
        }
        int nArrayTypeLen = 0;
        WCHAR warrayType[MAXElemTypeToStringLEN];
        if (elemType == ELEMENT_TYPE_CLASS)
        {
            nArrayTypeLen = GetClassNameFromClassId(warrayType, dimensionof(warrayType), arrayClassId, objectId, /*fExpandSystemString=*/ 0); // recur
            //swprintf_s(wszTemp, dimensionof(wszTemp), L"(Azray #dim=%d) ", arrayRank);
            //bstrResult.Append(wszTemp);            
        }
        else
        {
            nArrayTypeLen = ElemTypeToString(warrayType, dimensionof(warrayType),  pIMetaDataImport, elemType);
        }
        for (UINT i = 0 ; i < arrayRank ; i++)
        {
            if (i > 0)
            {
                if (wcsncpy_s(outbuf + nResultLen, noutBufSize - nResultLen, L",",_TRUNCATE) ==0)
				{
					nResultLen+=1;
				}
            }
            wcsncpy_s(outbuf + nResultLen, noutBufSize - nResultLen, warrayType,_TRUNCATE);
            nResultLen = wcsnlen(outbuf, noutBufSize);

        }
        wcsncpy_s(outbuf + nResultLen, noutBufSize - nResultLen, L"[]",_TRUNCATE);
        nResultLen =  wcsnlen(outbuf,noutBufSize);

        //swprintf_s(wszTemp, dimensionof(wszTemp), L"(Array #dim=%d, type= %s)", 
        //    arrayRank, 
        //    ElemTypeToString(pIMetaDataImport, elemType)
        //    );

        //bstrResult.Append(wszTemp);
    }
    else
    { // not an array
        cNumTypeArgs = dimensionof(typeArgs);
        hr = g_pCorProfilerInfo->GetClassIDInfo2(classId, &moduleID, &tdToken, &classIdParent, cNumTypeArgs, &cNumTypeArgs, typeArgs);
        if (hr == S_OK)
        {
            CComPtr<IMetaDataImport>  pIMetaDataImport;
            hr = MyGetModuleMetaData( moduleID, ofRead, IID_IMetaDataImport, (LPUNKNOWN *)&pIMetaDataImport );
            if (hr == S_OK)
            {
                int nDelt = GetNestedClassInfo(outbuf + nResultLen, noutBufSize - nResultLen, tdToken, pIMetaDataImport, cNumTypeArgs, typeArgs);
                if (nDelt>0)
                {
                    nResultLen+=nDelt;
                    if (wcsncpy_s(outbuf + nResultLen, noutBufSize - nResultLen, L".",_TRUNCATE) ==0)
					{
						nResultLen++;
					}
                }
                WCHAR wszTemp[MAXCLASSNAMELEN] = {0};
                mdToken tkExtends = NULL;
                hr = pIMetaDataImport->GetTypeDefProps( tdToken, wszTemp, dimensionof(wszTemp), 0, 0, &tkExtends );
                if (hr == S_OK)
                {
                    wcsncpy_s(outbuf + nResultLen, noutBufSize - nResultLen, wszTemp, noutBufSize-nResultLen -1);
                    nResultLen = wcsnlen(outbuf, noutBufSize);
                    if (cNumTypeArgs==0 && fExpandSystemString && objectId) // now check if system.string
                    {
                        if (fExpandSystemString)
                        {
                            if (g_pCProcessGC->m_SystemStringClassId == 0) // if we didn't get System.String yet (User might have started with TrackClrObj off)
                            {
                                if (wcscmp(wszTemp,L"System.String")==0)
                                {
                                    g_pCProcessGC->m_SystemStringClassId = classId;
                                }
                            }

                            if  (g_pCProcessGC->m_SystemStringClassId == classId)
                            {
                                int exceptionCode = 0;
                                int nLen = GetSystemStringExpansion(objectId, wszTemp, dimensionof(wszTemp), &exceptionCode);
                                if (nLen >= 0 && exceptionCode == 0) // empty string
                                {
                                    if (wcsncpy_s(outbuf + nResultLen, noutBufSize - nResultLen, L" ", _TRUNCATE) ==0)
									{
	                                    nResultLen+= 1;
									}
                                    if (nLen)
                                    { // we're adding a potentially very long string
                                        wcsncpy_s(outbuf + nResultLen, noutBufSize - nResultLen, wszTemp, _TRUNCATE);
                                        nResultLen = wcsnlen(outbuf, noutBufSize);
                                    }
                                }
                                else
                                {
                                    _snwprintf_s(outbuf + nResultLen, noutBufSize - nResultLen, _TRUNCATE , L" MemSpect: string exception code: %08x", exceptionCode);
                                    nResultLen = wcsnlen(outbuf, noutBufSize);
                                }
                            }
                        }
                    }
                }
            }
            if (cNumTypeArgs)
            {
                if (wcsncpy_s(outbuf + nResultLen, noutBufSize - nResultLen, L"<", _TRUNCATE) == 0)
				{
	                nResultLen +=1;
				}
                for (ULONG i = 0 ; i < cNumTypeArgs ; i++)
                {
                    if (i >0)
                    {
                        if (wcsncpy_s(outbuf + nResultLen, noutBufSize - nResultLen, L",", _TRUNCATE) == 0)
						{
							nResultLen +=1;
						}
                    }
                    nResultLen+= GetClassNameFromClassId(outbuf+nResultLen, noutBufSize-nResultLen, typeArgs[i], objectId,/*fExpandSystemStringObjectId=*/ 0); // recur
                }
                if (wcsncpy_s(outbuf + nResultLen, noutBufSize - nResultLen, L">",_TRUNCATE) == 0)
				{
	                nResultLen +=1;
				}
            }
            
        }
    
    }
	if (nResultLen == 0)
	{
        _snwprintf_s(outbuf + nResultLen, noutBufSize - nResultLen, _TRUNCATE , L" MemSpect: err scode: %08x", hr);
        nResultLen = wcsnlen(outbuf, noutBufSize);
	}
    return nResultLen;
}

int GetClassNameFromClassId(
	_Out_cap_(noutBufSize) WCHAR outbuf[],
	int noutBufSize,
	ClassID classId,
	ObjectID objectId,
	bool fExpandSystemString// = false
	)
{
	int nRetval = 0;
	__try
	{
		nRetval = GetClassNameFromClassIdEx(
			outbuf,
			noutBufSize,
			classId,
			objectId,
			fExpandSystemString
			);

	}
	__except (EXCEPTION_EXECUTE_HANDLER)
	{
		unsigned long excode = GetExceptionCode();
		_snwprintf_s(outbuf, noutBufSize, _TRUNCATE, L"Exception getting ClassName 0x%x  Code =0x%x", classId,excode);
	}

	return nRetval;
}

HRESULT DoGetClrDataHelper
(
    CAddressNode *pNode,
    DWORD fExpandSystemString,
    _Out_z_cap_(uicBuf) WCHAR * pszBuf, 
    UINT uicBuf
    )
{
    HRESULT hr = E_FAIL;
    int nResultLen = 0;
    pszBuf[0] = 0;
#define ClassNameOffset 12
    pszBuf[ClassNameOffset/2]=0;
    // sizeof(TrkBlock) == 0x18  ==24
    DWORD *pTrackBlockStruct = (DWORD *)(pNode->m_pv);
    TrkBlock * pTrkBlock = (TrkBlock * )&(pTrackBlockStruct[Offset_AllocToTblk]);
#if MSDEBUG
    VSASSERT(pNode->m_cb == sizeof(TrkBlock) + 6*4,"sizeof TrkBlk ?"); //Left, Parent, Right, Key1(bt), Key2(Id), sizeof(TrkBlock) + 0xafaf0001
    if (pTrkBlock->m_tbSig != TrkBlock_SIGNATURE)
    {
        return hr;
    }
#endif MSDEBUG
        
    BlockType bt = (BlockType )(pTrackBlockStruct[Offset_AllocToTblk-2]);
    DWORD dwObjectId = pTrackBlockStruct[Offset_AllocToTblk-1];
    WCHAR wszAppDomain[500] = {0};
    WCHAR wszAssembly[500] = {0};
    WCHAR wszModule[500] = {0};
    AppDomainID appDomainID = 0;
    AssemblyID assemblyID = 0;
    DWORD dwBaseAddress = 0;
    ModuleID moduleID=0;
    ClassID classId = 0;
    ULONG cchBuf;
    LPDWORD ptr = (LPDWORD) (pszBuf);
    ptr[2]  = 0;    // class size
    if (g_pCorProfilerInfo)
    {  // Appdomain, Assembly, Module, Class
        switch(bt)
        {
            case bt_ClrObject: // for an obj, we'll get the class
                classId = pTrkBlock->ClrObject.m_ClassId;
                //fallthru
            case bt_ClrClass:
                {
                    if (bt == bt_ClrClass)
                    {
                        classId = (ClassID) dwObjectId;
                    }
                    auto res = GetClrClassStats(classId);
                    ptr[0] = res.first;
                    ptr[1] = res.second;
                    GetClassNameFromClassId( pszBuf + ClassNameOffset/2, uicBuf - ClassNameOffset/2, classId, dwObjectId,
                        /*fExpandSystemString=*/bt== bt_ClrObject && fExpandSystemString ? true : false);
                    hr = S_OK;
                }
                break;
            case bt_ClrModule:
                moduleID = (ModuleID) dwObjectId;
                break;
            case bt_ClrAssembly:
                assemblyID = (AssemblyID)dwObjectId;
                break;
            case bt_ClrAppDomain:
                appDomainID = (AppDomainID)dwObjectId;
                break;
        }
        if (moduleID != 0 )
        {
            if (bt == bt_ClrModule) // if not explicitly getting the very long path of a module
            {
                cchBuf = dimensionof(wszModule);
                hr = g_pCorProfilerInfo->GetModuleInfo(moduleID, NULL, cchBuf, &cchBuf, wszModule, &assemblyID); // we don't want base addr
            }
        }
        if (assemblyID != 0)
        {
            cchBuf = dimensionof(wszAssembly);
            hr = g_pCorProfilerInfo->GetAssemblyInfo(assemblyID, cchBuf, &cchBuf, wszAssembly, &appDomainID, &moduleID);
        }
        if (appDomainID != 0)
        {
            cchBuf = dimensionof(wszAppDomain);
            hr = g_pCorProfilerInfo->GetAppDomainInfo(appDomainID, cchBuf, &cchBuf, wszAppDomain, NULL); // we don't want processid
        }
        if (hr== S_OK)
        {
            if (bt == bt_ClrObject || bt == bt_ClrClass) 
            {
            }
            else
            {
                _snwprintf_s(pszBuf+ClassNameOffset/2, uicBuf-ClassNameOffset/2, _TRUNCATE, L"%s|%s,%s", wszAppDomain, wszAssembly, wszModule);
            }
        }
    }
    return hr;
    
}

HRESULT DoGetClrData
(
    CAddressNode *pNode,
    DWORD fExpandSystemString,
    _Out_z_cap_(uicBuf) WCHAR * pszBuf, 
    UINT uicBuf
)
{
    HRESULT hr=E_FAIL;
    __try
    {
        hr = DoGetClrDataHelper(pNode, fExpandSystemString, pszBuf, uicBuf);
    } __except(EXCEPTION_EXECUTE_HANDLER) {
        ;
    }
    return hr;
}

ULONG  DoGetClrSize(BlockType bt, ClassID classId, ObjectID objectId)
{
    ULONG  nSize = 0;
    HRESULT hr = E_FAIL;
    if (g_pCorProfilerInfo)
    {
        switch(bt)
        {
            case bt_ClrClass:
                {
                    hr =  MyGetClassLayout(classId, NULL, NULL, NULL, &nSize);
                }
                break;
            case bt_ClrObject:
                hr =  g_pCorProfilerInfo->GetObjectSize(objectId, &nSize);
                if (hr == S_OK)
                {
                    
                }
                break;
        }
    }
    return nSize;
}

int DoClrObjTrkToggle(DWORD dwParam)
{
    if (g_pCorProfilerInfo)
    {
        if (dwParam == 0 || dwParam ==1)
        {
            MyProfiler::m_dwEventMask = COR_PRF_ENABLE_STACK_SNAPSHOT
                | COR_PRF_ENABLE_OBJECT_ALLOCATED
                | COR_PRF_MONITOR_GC //GarbageCollectionStarted, GarbageCollectionFinished, MovedReferences, SurvivingReferences, ObjectReferences, ObjectsAllocatedByClass, RootReferences, HandleCreated, HandleDestroyed, and FinalizeableObjectQueued callbacks.
    | (dwParam ? (                 COR_PRF_MONITOR_OBJECT_ALLOCATED // ModuleLoad, ModuleUnload, and ModuleAttachedToAssembly callbacks.
    )                
    : 0)
                | COR_PRF_MONITOR_CLASS_LOADS // ClassLoad and ClassUnload 
                | COR_PRF_MONITOR_MODULE_LOADS // ModuleLoad, ModuleUnload, and ModuleAttachedToAssembly callbacks.
                | COR_PRF_MONITOR_ASSEMBLY_LOADS // AssemblyLoad and AssemblyUnload callbacks
                | COR_PRF_MONITOR_APPDOMAIN_LOADS // ModuleLoad, ModuleUnload, and ModuleAttachedToAssembly callbacks.
                | COR_PRF_MONITOR_SUSPENDS //Controls the RuntimeSuspend, RuntimeResume, RuntimeThreadSuspended, and RuntimeThreadResumed callbacks.
                | COR_PRF_MONITOR_THREADS // Controls the ThreadCreated, ThreadDestroyed, ThreadAssignedToOSThread, and ThreadNameChanged callbacks
                ;
            if (g_TrackJit)
            {
                MyProfiler::m_dwEventMask |= COR_PRF_MONITOR_JIT_COMPILATION; // 0x20
            }
            if (g_TrackExcpt)
            {
                MyProfiler::m_dwEventMask |= COR_PRF_MONITOR_EXCEPTIONS; // 0x40
            }
            HRESULT hr = g_pCorProfilerInfo->SetEventMask(MyProfiler::m_dwEventMask);
            if (hr == S_OK)
            {
                if (dwParam == 1 && !g_TrackClrObjects)
                {// if we're toggling ON then there may be CLR objs that we don't have tracked when we were off
                    if (g_pCProcessGC)
                    {
                        g_pCProcessGC->m_fHaveToCatchUpObjectTracking=true;
                    }
                }
                g_TrackClrObjects = dwParam;
            }
        }
        
    }
    return g_TrackClrObjects;
}


CComQIPtr<ICorDebugProcess> g_pCorDebugProcess;

class MyManagedDebugHandler:
    public CComObjectRootEx<CComSingleThreadModel>,
        ICorDebugManagedCallback2
{
    public:
        BEGIN_COM_MAP(MyManagedDebugHandler)
            COM_INTERFACE_ENTRY(ICorDebugManagedCallback2)
        END_COM_MAP()
        MyManagedDebugHandler()
        {
            VSASSERT(1, "MyManagedDebugHandler");
        }
         HRESULT STDMETHODCALLTYPE Breakpoint( 
            /* [in] */ ICorDebugAppDomain *pAppDomain,
            /* [in] */ ICorDebugThread *pThread,
            /* [in] */ ICorDebugBreakpoint *pBreakpoint) {return S_OK;};
        
         HRESULT STDMETHODCALLTYPE StepComplete( 
            /* [in] */ ICorDebugAppDomain *pAppDomain,
            /* [in] */ ICorDebugThread *pThread,
            /* [in] */ ICorDebugStepper *pStepper,
            /* [in] */ CorDebugStepReason reason) {return S_OK;};
        
         HRESULT STDMETHODCALLTYPE Break( 
            /* [in] */ ICorDebugAppDomain *pAppDomain,
            /* [in] */ ICorDebugThread *thread) {return S_OK;};
        
         HRESULT STDMETHODCALLTYPE Exception( 
            /* [in] */ ICorDebugAppDomain *pAppDomain,
            /* [in] */ ICorDebugThread *pThread,
            /* [in] */ BOOL unhandled) {return S_OK;};
        
         HRESULT STDMETHODCALLTYPE EvalComplete( 
            /* [in] */ ICorDebugAppDomain *pAppDomain,
            /* [in] */ ICorDebugThread *pThread,
            /* [in] */ ICorDebugEval *pEval) {return S_OK;};
        
         HRESULT STDMETHODCALLTYPE EvalException( 
            /* [in] */ ICorDebugAppDomain *pAppDomain,
            /* [in] */ ICorDebugThread *pThread,
            /* [in] */ ICorDebugEval *pEval) {return S_OK;};
        
         HRESULT STDMETHODCALLTYPE CreateProcess( 
            /* [in] */ ICorDebugProcess *pProcess) {return S_OK;};
        
         HRESULT STDMETHODCALLTYPE ExitProcess( 
            /* [in] */ ICorDebugProcess *pProcess) {return S_OK;};
        
         HRESULT STDMETHODCALLTYPE CreateThread( 
            /* [in] */ ICorDebugAppDomain *pAppDomain,
            /* [in] */ ICorDebugThread *thread) {return S_OK;};
        
         HRESULT STDMETHODCALLTYPE ExitThread( 
            /* [in] */ ICorDebugAppDomain *pAppDomain,
            /* [in] */ ICorDebugThread *thread) {return S_OK;};
        
         HRESULT STDMETHODCALLTYPE LoadModule( 
            /* [in] */ ICorDebugAppDomain *pAppDomain,
            /* [in] */ ICorDebugModule *pModule) {return S_OK;};
        
         HRESULT STDMETHODCALLTYPE UnloadModule( 
            /* [in] */ ICorDebugAppDomain *pAppDomain,
            /* [in] */ ICorDebugModule *pModule) {return S_OK;};
        
         HRESULT STDMETHODCALLTYPE LoadClass( 
            /* [in] */ ICorDebugAppDomain *pAppDomain,
            /* [in] */ ICorDebugClass *c) {return S_OK;};
        
         HRESULT STDMETHODCALLTYPE UnloadClass( 
            /* [in] */ ICorDebugAppDomain *pAppDomain,
            /* [in] */ ICorDebugClass *c) {return S_OK;};
        
         HRESULT STDMETHODCALLTYPE DebuggerError( 
            /* [in] */ ICorDebugProcess *pProcess,
            /* [in] */ HRESULT errorHR,
            /* [in] */ DWORD errorCode) {return S_OK;};
        
         HRESULT STDMETHODCALLTYPE LogMessage( 
            /* [in] */ ICorDebugAppDomain *pAppDomain,
            /* [in] */ ICorDebugThread *pThread,
            /* [in] */ LONG lLevel,
            /* [in] */ WCHAR *pLogSwitchName,
            /* [in] */ WCHAR *pMessage) {return S_OK;};
        
         HRESULT STDMETHODCALLTYPE LogSwitch( 
            /* [in] */ ICorDebugAppDomain *pAppDomain,
            /* [in] */ ICorDebugThread *pThread,
            /* [in] */ LONG lLevel,
            /* [in] */ ULONG ulReason,
            /* [in] */ WCHAR *pLogSwitchName,
            /* [in] */ WCHAR *pParentName) {return S_OK;};
        
         HRESULT STDMETHODCALLTYPE CreateAppDomain( 
            /* [in] */ ICorDebugProcess *pProcess,
            /* [in] */ ICorDebugAppDomain *pAppDomain) {return S_OK;};
        
         HRESULT STDMETHODCALLTYPE ExitAppDomain( 
            /* [in] */ ICorDebugProcess *pProcess,
            /* [in] */ ICorDebugAppDomain *pAppDomain) {return S_OK;};
        
         HRESULT STDMETHODCALLTYPE LoadAssembly( 
            /* [in] */ ICorDebugAppDomain *pAppDomain,
            /* [in] */ ICorDebugAssembly *pAssembly) {return S_OK;};
        
         HRESULT STDMETHODCALLTYPE UnloadAssembly( 
            /* [in] */ ICorDebugAppDomain *pAppDomain,
            /* [in] */ ICorDebugAssembly *pAssembly) {return S_OK;};
        
         HRESULT STDMETHODCALLTYPE ControlCTrap( 
            /* [in] */ ICorDebugProcess *pProcess) {return S_OK;};
        
         HRESULT STDMETHODCALLTYPE NameChange( 
            /* [in] */ ICorDebugAppDomain *pAppDomain,
            /* [in] */ ICorDebugThread *pThread) {return S_OK;};
        
         HRESULT STDMETHODCALLTYPE UpdateModuleSymbols( 
            /* [in] */ ICorDebugAppDomain *pAppDomain,
            /* [in] */ ICorDebugModule *pModule,
            /* [in] */ IStream *pSymbolStream) {return S_OK;};
        
         HRESULT STDMETHODCALLTYPE EditAndContinueRemap( 
            /* [in] */ ICorDebugAppDomain *pAppDomain,
            /* [in] */ ICorDebugThread *pThread,
            /* [in] */ ICorDebugFunction *pFunction,
            /* [in] */ BOOL fAccurate) {return S_OK;};
        
         HRESULT STDMETHODCALLTYPE BreakpointSetError( 
            /* [in] */ ICorDebugAppDomain *pAppDomain,
            /* [in] */ ICorDebugThread *pThread,
            /* [in] */ ICorDebugBreakpoint *pBreakpoint,
            /* [in] */ DWORD dwError) {return S_OK;};


         HRESULT STDMETHODCALLTYPE FunctionRemapOpportunity( 
            /* [in] */ ICorDebugAppDomain *pAppDomain,
            /* [in] */ ICorDebugThread *pThread,
            /* [in] */ ICorDebugFunction *pOldFunction,
            /* [in] */ ICorDebugFunction *pNewFunction,
            /* [in] */ ULONG32 oldILOffset) {return S_OK;}
        
         HRESULT STDMETHODCALLTYPE CreateConnection( 
            /* [in] */ ICorDebugProcess *pProcess,
            /* [in] */ CONNID dwConnectionId,
            /* [in] */ WCHAR *pConnName) {return S_OK;}
        
         HRESULT STDMETHODCALLTYPE ChangeConnection( 
            /* [in] */ ICorDebugProcess *pProcess,
            /* [in] */ CONNID dwConnectionId) {return S_OK;}
        
         HRESULT STDMETHODCALLTYPE DestroyConnection( 
            /* [in] */ ICorDebugProcess *pProcess,
            /* [in] */ CONNID dwConnectionId) {return S_OK;}
        
         HRESULT STDMETHODCALLTYPE Exception( 
            /* [in] */ ICorDebugAppDomain *pAppDomain,
            /* [in] */ ICorDebugThread *pThread,
            /* [in] */ ICorDebugFrame *pFrame,
            /* [in] */ ULONG32 nOffset,
            /* [in] */ CorDebugExceptionCallbackType dwEventType,
            /* [in] */ DWORD dwFlags) {return S_OK;}
        
         HRESULT STDMETHODCALLTYPE ExceptionUnwind( 
            /* [in] */ ICorDebugAppDomain *pAppDomain,
            /* [in] */ ICorDebugThread *pThread,
            /* [in] */ CorDebugExceptionUnwindCallbackType dwEventType,
            /* [in] */ DWORD dwFlags) {return S_OK;}
        
         HRESULT STDMETHODCALLTYPE FunctionRemapComplete( 
            /* [in] */ ICorDebugAppDomain *pAppDomain,
            /* [in] */ ICorDebugThread *pThread,
            /* [in] */ ICorDebugFunction *pFunction) {return S_OK;}
        
         HRESULT STDMETHODCALLTYPE MDANotification( 
            /* [in] */ ICorDebugController *pController,
            /* [in] */ ICorDebugThread *pThread,
            /* [in] */ ICorDebugMDA *pMDA) {return S_OK;}


};


CLINKAGE DWORD  ENTRYPOINT ReadProcessMemoryHelper(
        __in HANDLE hProc,
        __in LPVOID addr,
        __in int nElems,
        __in int nSizeOfElem,
        __in_ecount(nElems) LPVOID dwBuff
    )
{
    DWORD dwBytesRead = 0;
    if (ReadProcessMemory(hProc, addr, dwBuff, nElems * nSizeOfElem, &dwBytesRead) == 0)
    {
#if MSDEBUG
        VSASSERTF((false,"ReadProcessMemoryHelper failed %x", GetLastError()));
#endif MSDEBUG
    }
    return dwBytesRead;
}

    
#if FREEZETRY
CLINKAGE HRESULT  ENTRYPOINT FreezeThreads(DWORD nProcessId)
{
    HRESULT hr = E_FAIL;
    if (g_pCorDebugProcess == 0)
    {
        HMODULE hMscoree = LoadLibraryA("mscoree.dll");
        VSASSERT(hMscoree ,"could not loadlib mscoree.dll");
        typedef HRESULT (STDAPICALLTYPE *FPCreateCordb)(int iDebuggerVersion, LPCWSTR szDebuggeeVersion, void ** ppCordb);
        FPCreateCordb fpCreateCordb = (FPCreateCordb) GetProcAddress(hMscoree, "CreateDebuggingInterfaceFromVersion");
        VSASSERT(hMscoree ,"GetProcAddress hMscoree");

        CComQIPtr<ICorDebug> pCorDebug;
        hr = fpCreateCordb(CorDebugLatestVersion, L"v4.0.30105", reinterpret_cast<void**>(&pCorDebug));
        if (hr != S_OK)
        {
            hr = fpCreateCordb(CorDebugLatestVersion, L"v4.0.30319", reinterpret_cast<void**>(&pCorDebug));
        }
        VSASSERT(hr== S_OK,"IID_ICLRRuntimeInfo failed");

        hr= pCorDebug->Initialize();
        VSASSERT(hr== S_OK,"m_pCorDebug Initialize failed");

        CComObject<MyManagedDebugHandler>* pHandler;
        hr = CComObject<MyManagedDebugHandler>::CreateInstance(&pHandler);
        VSASSERT(hr== S_OK,"MyManagedDebugHandler>::CreateInstancefailed");
        pHandler->AddRef();
        CComQIPtr<ICorDebugManagedCallback> pCallback;
        hr= pHandler->QueryInterface(_ATL_IIDOF(ICorDebugManagedCallback2), reinterpret_cast<void**>(&pCallback));
        VSASSERT(hr== S_OK,"pHandler->QueryInterface(IID_ICorDebugManagedCallback2 failed");
        
        hr = pCorDebug->SetManagedHandler(pCallback);
        VSASSERT(hr== S_OK,"SetManagedHandler Initialize failed");

        hr = pCorDebug->DebugActiveProcess(nProcessId, false, reinterpret_cast<ICorDebugProcess**>(&g_pCorDebugProcess));
        VSASSERT(hr== S_OK,"pCorDebug->GetProcess failed");
    }
    if (g_pCorDebugProcess)
    {
        hr = g_pCorDebugProcess->Stop(0);
        VSASSERT(hr== S_OK,"g_pCorDebugProcess->Stop failed");
    }
    return hr;
}

CLINKAGE HRESULT ENTRYPOINT UnfreezeThreads()
{
    HRESULT hr = E_FAIL;
    if (g_pCorDebugProcess)
    {
        hr=g_pCorDebugProcess->Continue(0);
        VSASSERT(hr== S_OK,"g_pCorDebugProcess->Continue failed");
    }
    return hr;
}
#endif FREEZETRY
