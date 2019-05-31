#include "Pch.H"
#include "atlsafe.h"
#include "mscoree.h"
#include "vsassert.h"
#include "debugalloc.h"
#include "util.h"
#include "mem.h"
#include <string>

#define IfFailGo(EXPR) IfFailGoto(EXPR, Error)

#define FAILEDHR(HR) FAILED(HR)
#define IfNullGo(EXPR, HR) {if (!(EXPR)) {hr = HR;goto Error;}}
#define IfNullFail(expr)                { if (!(expr)) return (E_FAIL); }

#import <mscorlib.tlb> \
	no_namespace \
	rename("_Module", "_MscorlibModule") \
	rename("ReportEvent", "MscorlibReportEvent") \
	rename("CompilationRelaxations", "MscorlibCompilationRelaxations") \
	rename("CompilationRelaxations_NoStringInterning", "MscorlibCompilationRelaxations_NoStringInterning") \
	rename("Thread", "MscorlibThread") \
	rename("_Mutex", "_MscorlibMutex") \
	rename("_Hash", "_MscorlibHash") \
	exclude("IObjectHandle") \
	rename("value", "Mscorlibvalue")

extern ULONG g_nThreadFreeze; // when this is non-zero we're trying to freeze
extern DWORD g_dwThreadIdMemSpectBgdThreadGCAndCleanupRCW; // the private threadid of MemSpect to do RCWCleanup and GC. Managed code can run
extern int g_CleanUpRCWs;

extern void ProcessGCCritSectRequest();
extern void ProcessGCCritSectRelease();

//"borrowed" from cAppdomainmanager
HRESULT GetAssemblyFromAppDomain(_AppDomain* pAppDomain, LPCWSTR wszAssemblyName, _Deref_out_opt_ _Assembly **ppAssembly)
{
	HRESULT hr = S_OK;
	SAFEARRAY                   *pAssemblyArray = NULL;
	CComSafeArray<IUnknown*>    csaAssemblies;
	size_t                      cchAssemblyName;
	long                        cAssemblies;

	IfFailRet(pAppDomain->raw_GetAssemblies(&pAssemblyArray));

	csaAssemblies.Attach(pAssemblyArray);
	cchAssemblyName = wcslen(wszAssemblyName);
	cAssemblies = csaAssemblies.GetCount();
	hr = E_FAIL;
	for (long i = 0; i < cAssemblies; i++)
	{
		CComQIPtr<_Assembly>        srpqiAssembly;
		srpqiAssembly = csaAssemblies.GetAt(i);
		if (srpqiAssembly == nullptr)
		{
			continue;
		}
		CComBSTR cbstrAssemblyLocation;
		if (S_OK == srpqiAssembly->get_Location(&cbstrAssemblyLocation))
		{
			WCHAR drive[_MAX_DRIVE];
			WCHAR dir[_MAX_DIR];
			WCHAR fname[_MAX_FNAME];
			WCHAR ext[_MAX_EXT];
			_wsplitpath_s(cbstrAssemblyLocation, drive, dir, fname, ext);
			if (_wcsicmp(fname, wszAssemblyName) == 0)
			{
				*ppAssembly = srpqiAssembly.Detach();
				hr = S_OK;
				break;
			}
		}
	}
	return hr;
}



static HRESULT CallStaticMethodFromType(_Type *pType, CComBSTR &cbstrMethod)
{
	CComVariant     cvtEmptyTarget;
	CComVariant     cvtReturnValue;
	if (pType == NULL)
		return E_POINTER;

	HRESULT hr = S_OK;

	IfFailGo(pType->raw_InvokeMember_3(
		cbstrMethod,
		(BindingFlags)(BindingFlags_InvokeMethod | BindingFlags_Public | BindingFlags_Static),
		NULL,
		cvtEmptyTarget,
		NULL,
		&cvtReturnValue));

Error:
	return hr;
}

HRESULT GetTypeFromAssembly(LPCWSTR wszTypeName, _Assembly * srpAssembly, _Type **ppType)
{
	HRESULT hr = S_OK;
	IfFailRet(srpAssembly->raw_GetType_2(CComBSTR(wszTypeName), ppType));
	IfNullFail((*ppType));
	return hr;
}

typedef HRESULT(STDAPICALLTYPE *fpCLRCreateInstance)(
	REFCLSID  rclsid,
	REFIID    riid,
	LPVOID*   ppv
	);

HRESULT StartClrAndGetAppDomain(CComPtr<_AppDomain> &srpDomain)
{
	CComPtr<ICLRMetaHost>	srpMetaHost;
	CComPtr<ICLRRuntimeInfo> srpRuntimeInfo;
	CComPtr<ICorRuntimeHost> srpCorHost;
	HRESULT hr;
	HMODULE hMscoree;

	hMscoree = GetModuleHandleA("mscoree.dll");
	if (hMscoree == NULL)
	{
		hMscoree = LoadLibrary("mscoree.dll"); //windows\system32
	}
	VSASSERT(hMscoree, "could not loadlib mscoree.dll");
	fpCLRCreateInstance pCLRCreateInstance = (fpCLRCreateInstance)GetProcAddress(hMscoree, "CLRCreateInstance");
	VSASSERT(pCLRCreateInstance, "GetProcAddress pCLRCreateInstance hMscoree");
	CComPtr<IEnumUnknown> pRuntimes;

	IfFailRet(pCLRCreateInstance(CLSID_CLRMetaHost, IID_PPV_ARGS(&srpMetaHost)));

	if (S_OK != srpMetaHost->GetRuntime(L"v4.0.30319", IID_PPV_ARGS(&srpRuntimeInfo)))
	{
		IfFailRet(srpMetaHost->EnumerateInstalledRuntimes(&pRuntimes));
		while (true)
		{
			CComPtr<IUnknown> pUnkRuntime;
			if (S_OK != pRuntimes->Next(1, &pUnkRuntime, 0))
			{
				break;
			}
			CComQIPtr<ICLRRuntimeInfo> pRuntime(pUnkRuntime);
			if (pRuntime != nullptr)
			{
				srpRuntimeInfo = pRuntime;
				WCHAR szVersion[_MAX_PATH];
				DWORD cchVersion = _countof(szVersion);
				srpRuntimeInfo->GetVersionString(szVersion, &cchVersion);
				break;
			}
		}
	}
	IfNullFail(srpRuntimeInfo);

	BOOL bStarted = false;
	DWORD dwStartupFlags;
	hr = srpRuntimeInfo->IsStarted(&bStarted, &dwStartupFlags);

	IfFailRet(srpRuntimeInfo->GetInterface(CLSID_CorRuntimeHost, IID_PPV_ARGS(&srpCorHost)));
	if (!bStarted)
	{
		IfFailRet(srpCorHost->Start());
	}

	CComPtr<IUnknown>       srpUnk;
	IfFailRet(srpCorHost->GetDefaultDomain(&srpUnk));
	ASSERT(srpUnk != NULL, "");
	IfNullFail(srpUnk);
	CComQIPtr<_AppDomain> appDomain(srpUnk);
	IfNullFail(appDomain);
	srpDomain = appDomain.Detach();
	IfNullFail(srpDomain);
	return hr;

}

struct GCRCW
{

	CComPtr<_AppDomain>     srpDomain;
	CComPtr<_Assembly>      srpAssembly;
	CComPtr<_Type>          srpGCType;
	CComPtr<_Type>          srpMarshalType;
	CComBSTR bstrCollect;
	CComBSTR bstrWaitForPendingFinalizers;
	CComBSTR bstrCleanupUnusedObjectsInCurrentContext;

	int m_nFailures;
	bool m_fDidShutDown;

	void GCRCWInit()
	{
		m_fDidShutDown = false;
		HRESULT hr;

		m_nFailures = 0;

		IfFailGo(StartClrAndGetAppDomain(srpDomain));

		IfFailGo(GetAssemblyFromAppDomain(srpDomain, L"mscorlib", &srpAssembly));
		IfFailGo(GetTypeFromAssembly(L"System.Runtime.InteropServices.Marshal", srpAssembly, &srpMarshalType));

		IfFailGo(GetTypeFromAssembly(L"System.GC", srpAssembly, &srpGCType));
		bstrCollect.Append(L"Collect");
		bstrWaitForPendingFinalizers.Append(L"WaitForPendingFinalizers");
		bstrCleanupUnusedObjectsInCurrentContext.Append(L"CleanupUnusedObjectsInCurrentContext");
	Error:

		VSASSERTF((hr == S_OK, "error init'g bg thread for rcw cleanup %x", hr));
	}

	HRESULT GarbageCollectCLR()
	{
		HRESULT hr = S_OK;
		if (g_pCDebFreezeAllThreads) // if we're frozen already
		{
			if ((g_CleanUpRCWs & 2) > 0) { //,"GC on background requires flag set"));
				IfFailGo(CallStaticMethodFromType(srpGCType, bstrCollect));
			}
		}
		else
		{
			if (m_nFailures <= 4) // only allow a few failures
			{
				if (g_CleanUpRCWs > 0)
				{
					//*
					hr = CallStaticMethodFromType(srpMarshalType, bstrCleanupUnusedObjectsInCurrentContext);
					if (hr == 0x80131512) // 0x80131512 COR_E_MISSINGMEMBER   happens on csLife.exe
					{
						m_nFailures += 1;
						hr = S_OK;
					}
					if (hr == 0x8007042b) // The process terminated unexpectedly
					{
						m_nFailures += 1;
						hr = S_OK;
					}
				}
				if ((g_CleanUpRCWs & 2) > 0)
				{
					if (g_nThreadFreeze) // we want to freeze
					{
						ProcessGCCritSectRequest();
						auto save_g_nThreadFreeze = g_nThreadFreeze;
						g_nThreadFreeze = 0; // safe to do because MemSpect thread is waitin
						IfFailGo(CallStaticMethodFromType(srpGCType, bstrCollect));
						IfFailGo(CallStaticMethodFromType(srpGCType, bstrWaitForPendingFinalizers));
						IfFailGo(CallStaticMethodFromType(srpGCType, bstrCollect));
						IfFailGo(CallStaticMethodFromType(srpGCType, bstrWaitForPendingFinalizers));

						g_nThreadFreeze = save_g_nThreadFreeze; // now do the freeze
						IfFailGo(CallStaticMethodFromType(srpGCType, bstrCollect));
						ProcessGCCritSectRelease();
					}
					else
					{
						IfFailGo(CallStaticMethodFromType(srpGCType, bstrCollect));
						IfFailGo(CallStaticMethodFromType(srpGCType, bstrWaitForPendingFinalizers));
						IfFailGo(CallStaticMethodFromType(srpGCType, bstrCollect));
					}
				}
			}

			/*/
			//Microsoft.VisualStudio.Platform.AppDomainManager, Version=10.0.0.0, Culture=Neutral, PublicKeyToken=b03f5f7f11d50a3a
			//Microsoft.VisualStudio.Platform.VsRcwCleanupPrivate
			//CleanUpUnReachableRCWs

			IfFailGo(GetAssemblyFromAppDomain(srpDomain, L"Microsoft.VisualStudio.Platform.AppDomainManager, Version=10.0.0.0, Culture=Neutral, PublicKeyToken=b03f5f7f11d50a3a", &srpAssembly));
			ASSERT(srpAssembly != NULL,"");
			IfNullGo(srpAssembly, E_UNEXPECTED);
			IfFailGo(GetTypeFromAssembly(L"Microsoft.VisualStudio.Platform.VsRcwCleanupPrivate", srpAssembly, &srpMarshalType));
			IfFailGo(CallStaticMethodFromType(srpMarshalType, L"CleanUpUnReachableRCWs"));

			//*/

		}
	Error:
#if MSDEBUG
		VSASSERT(hr == S_OK, "error bg thread for rcw cleanup");
#endif MSDEBUG
		return hr;
	}
	void ShutDown()
	{
		VSASSERT(!m_fDidShutDown, "gcrcw shutdown only once?");
		if (!m_fDidShutDown)
		{
			m_fDidShutDown = true;
			srpDomain = 0;
			srpAssembly = 0;
			srpGCType = 0;
			srpMarshalType = 0;

		}
	}

};




HANDLE s_GCBackTheadStartEvent = INVALID_HANDLE_VALUE;
HANDLE s_GCBackTheadDoneEvent = INVALID_HANDLE_VALUE;

static DWORD WINAPI GCBackThreadProc(void *pvThreadObject)
{
	GCRCW *pgcrcw = (GCRCW *)pvThreadObject;
	pgcrcw->GCRCWInit();

	while (!pgcrcw->m_fDidShutDown)
	{
		WaitForSingleObject(s_GCBackTheadStartEvent, INFINITE);
		pgcrcw->GarbageCollectCLR();
		SetEvent(s_GCBackTheadDoneEvent);
	}
	return 0;
}

GCRCW gcrcw;

void GarbageCollectOrCleanupRCWs()
{
	//        GarbageCollectCLR(true, false); // can't call managed code from this thread: must be called from diff thread.
	if (s_GCBackTheadStartEvent == INVALID_HANDLE_VALUE)
	{
		s_GCBackTheadStartEvent = CreateEvent(NULL, //secatt
			FALSE, // bmanualreaset
			FALSE, // initial state
			"s_GCBackTheadStartEvent");

		s_GCBackTheadDoneEvent = CreateEvent(NULL, //secatt
			FALSE, // bmanualreaset
			FALSE, // initial state
			"s_GCBackTheadDoneEvent");

		auto stkSize = 2 * 65536;
		if ((g_CleanUpRCWs & 2) > 0) // immersive GC occurs on this thread
		{
			stkSize *= 4;
		}


		// create a thread that lasts a long time. This thread can be suspended along with all the others when freezing
		HANDLE hThread = CreateThread(NULL,         // no security...
			stkSize,            // stack size
			GCBackThreadProc,   // initial method
			&gcrcw, // parameter to the thread proc
			0,            // run immediately
			&g_dwThreadIdMemSpectBgdThreadGCAndCleanupRCW);

		//if (hThread != INVALID_HANDLE_VALUE)
		//{
		//    WaitForSingleObject(hThread, INFINITE);
		//    CloseHandle(hThread);
		//}

	}

	SetEvent(s_GCBackTheadStartEvent); // tell the thread to do it's thing

	WaitForSingleObject(s_GCBackTheadDoneEvent, INFINITE);

}

void ShutDownRCWCleanupStuff()
{
	if (!gcrcw.m_fDidShutDown)
	{
		gcrcw.ShutDown();
	}
}

/*

VOID RCWCleanupList::CleanupWrappersInCurrentCtxThread(BOOL fWait, BOOL fManualCleanupRequested, BOOL bIgnoreComObjectEagerCleanupSetting)


>	clr.dll!RCWCleanupList::CleanupWrappersInCurrentCtxThread(int fManualCleanupRequested, int bIgnoreComObjectEagerCleanupSetting, int) Line 1627	C++
clr.dll!MarshalNative::CleanupUnusedObjectsInCurrentContext() Line 1347	C++
Microsoft.VisualStudio.Platform.AppDomainManager.ni.dll!6d355c29()	Unknown
[Frames below may be incorrect and/or missing, no symbols loaded for Microsoft.VisualStudio.Platform.AppDomainManager.ni.dll]
Microsoft.VisualStudio.Platform.AppDomainManager.ni.dll!6d35c59a()	Unknown
clr.dll!@COMToCLRDispatchHelper@32() Line 1975	Unknown
clr.dll!COMToCLRWorker(Thread * pThread, ComMethodFrame * pFrame) Line 787	C++
003ca0aa()	Unknown
msenv.dll!CMsoCMHandler::EnvironmentMsgLoop() Line 482	C++
msenv.dll!CMsoCMHandler::FPushMessageLoop(unsigned long uReason) Line 359	C++
msenv.dll!SCM::FPushMessageLoop(SCMI * pscmi, unsigned long uReason, void * pvLoopData) Line 2243	C++
msenv.dll!SCM_MsoCompMgr::FPushMessageLoop(unsigned long dwComponentID, unsigned long uReason, void * pvLoopData) Line 2999	C++
msenv.dll!CMsoComponent::PushMsgLoop(unsigned long msgloop) Line 716	C++
msenv.dll!VStudioMainLogged() Line 985	C++
msenv.dll!VStudioMain(MAINPARAM * pMainParam) Line 1053	C++
devenv.exe!util_CallVsMain(MAINPARAM * pMainParam, int * piRes) Line 1062	C++
devenv.exe!CDevEnvAppId::Run(wchar_t * wszCmdLine, int nCmdShow) Line 680	C++
devenv.exe!WinMain(HINSTANCE__ * hInstance, HINSTANCE__ * __formal, char * lpCmdLine, int nCmdShow) Line 56	C++
devenv.exe!__tmainCRTStartup() Line 529	C


>	clr.dll!RCWCleanupList::CleanupWrappersInCurrentCtxThread(int fManualCleanupRequested, int bIgnoreComObjectEagerCleanupSetting, int) Line 1575	C++
clr.dll!MarshalNative::CleanupUnusedObjectsInCurrentContext() Line 1347	C++
clr.dll!CallDescrWorkerInternal(unsigned long pParams) Line 756	Unknown
clr.dll!CallDescrWorkerWithHandler(CallDescrData * fCriticalCall, int) Line 79	C++
clr.dll!CallDescrWorkerReflectionWrapper(CallDescrData * pCallDescrData, Frame * pFrame)	C++
clr.dll!RuntimeMethodHandle::InvokeMethod(Object * target, PtrArray * objs, bool fConstructor, SignatureNative * pSigUNSAFE) Line 1642	C++
mscorlib.ni.dll!678b66b3()	Unknown
[Frames below may be incorrect and/or missing, no symbols loaded for mscorlib.ni.dll]
mscorlib.ni.dll!678f36cd()	Unknown
mscorlib.ni.dll!679276c1()	Unknown
mscorlib.ni.dll!67f3565d()	Unknown
mscorlib.ni.dll!67eef3ce()	Unknown
clr.dll!@COMToCLRDispatchHelper@32() Line 2021	Unknown
clr.dll!COMToCLRWorker(Thread * pThread, ComMethodFrame * pFrame) Line 787	C++
003ca0aa()	Unknown
MemSpectDll.dll!CallStaticMethodFromType(_Type * pType, const wchar_t * wszMethod) Line 110	C++
MemSpectDll.dll!GCBackThreadProc(void * pvThreadObject) Line 250	C++
kernel32.dll!BaseThreadInitThunk(unsigned long RunProcessInit, long (void *) * StartAddress, void * Argument) Line 65	C
ntdll.dll!__RtlUserThreadStart(long (void *) * StartAddress, void * Argument) Line 3188	C
ntdll.dll!_RtlUserThreadStart(long (void *) * StartAddress, void * Argument) Line 3116	C

*/
struct CompileAndExecuteData
{
	std::string result;
	std::string filename;
	std::string memsSpectBaseDllFileName;
	HANDLE eventCompAndExecDone;
};

static DWORD WINAPI CompileAndExecuteThreadProc(void *pParam)
{
	CompileAndExecuteData *pData = (CompileAndExecuteData *)pParam;
	HRESULT hr;
	CComPtr<_AppDomain>     srpDomain;
	CComPtr<_Assembly> srpAssemblyMsCorlib;
	CComPtr<_Assembly> srpAssemblyMemSpectBase;
	CComPtr<_Type> typeCommon;
	IfFailGo(StartClrAndGetAppDomain(srpDomain));
	pData->result.append("Got appdomain! ");

	IfFailGo(GetAssemblyFromAppDomain(srpDomain, L"mscorlib", &srpAssemblyMsCorlib));
	pData->result.append("Got mscorlib ");

	if (S_OK != GetAssemblyFromAppDomain(srpDomain, L"memspectbase", &srpAssemblyMemSpectBase))
	{
		CComPtr<_Type> srpreflectionAssemblyType;
		IfFailGo(GetTypeFromAssembly(L"System.Reflection.Assembly", srpAssemblyMsCorlib, &srpreflectionAssemblyType));
		pData->result.append("Got refl ");
		{
			CComSafeArray<VARIANT> args;
			args.Add(CComVariant(CComBSTR(pData->memsSpectBaseDllFileName.c_str())));
			CComVariant cvtReturnValue;
			CComVariant cvtTarget;
			IfFailGo(srpreflectionAssemblyType->raw_InvokeMember_3(
				CComBSTR(L"LoadFrom"),
				(BindingFlags)(BindingFlags_InvokeMethod | BindingFlags_Public | BindingFlags_Static),
				NULL,
				cvtTarget,
				args,
				&cvtReturnValue));
		}
		pData->result.append("did loadfrom ");
		IfFailGo(GetAssemblyFromAppDomain(srpDomain, L"memspectbase", &srpAssemblyMemSpectBase));
		pData->result.append("Got memspectbaseAfterLoadFrom ");
	}
	else
	{
		pData->result.append("Got memspectbase ");
	}
	IfFailGo(GetTypeFromAssembly(L"MemSpect.Common", srpAssemblyMemSpectBase, &typeCommon));
	pData->result.append("Got common ");
	{
		CComSafeArray<VARIANT> args;
		args.Add(CComVariant(CComBSTR(pData->filename.c_str())));
		CComVariant cvtReturnValue;
		CComVariant cvtTarget;
		IfFailGo(typeCommon->raw_InvokeMember_3(
			CComBSTR(L"CompileAndExecuteFile"),
			(BindingFlags)(BindingFlags_InvokeMethod | BindingFlags_Public | BindingFlags_Static),
			NULL,
			cvtTarget,
			args,
			&cvtReturnValue));

		USES_CONVERSION;
		pData->result = W2A(cvtReturnValue.bstrVal);
	}

Error:
	SetEvent(pData->eventCompAndExecDone);
	return 0;
}

std::string CompileAndExecute(std::string filename, std::string memsSpectBaseDllFileName)
{
	//    MessageBoxA(0, "Hi", "hi", 0);
	CompileAndExecuteData data{ "", filename, memsSpectBaseDllFileName };
	data.eventCompAndExecDone = CreateEvent(NULL, //secatt
		FALSE, // bmanualreset
		FALSE, // initial state
		"s_CompAndExecDone");
	HANDLE hThread = CreateThread(NULL,         // no security...
		1024 * 1024,            // stack size
		CompileAndExecuteThreadProc,   // initial method
		&data, // parameter to the thread proc
		STACK_SIZE_PARAM_IS_A_RESERVATION,            // run immediately
		/*threadid*/ NULL);
	WaitForSingleObject(data.eventCompAndExecDone, INFINITE);
	return data.result;
}
