#include "pch.h"
#include "vsassert.h"
#include "mem.h"

// these 2 include files from 
#include "myAppxPackaging.h" //"C:\Program Files\Windows Kits\8.0\Include\winrt\AppxPackaging.h"
							// also needs C:\Program Files\Windows Kits\8.0\Include\shared\winapifamily.h
#include "myshObjIdl.h" //CLSID_PackageDebugSettings  "c:\Program Files\Windows Kits\8.0\Include\um\ShObjIdl.h"
//#include "userenv.h"//DeriveAppContainerSidFromAppContainerName 

// "C:\Users\calvinh\Documents\Visual Studio 11\Projects\csGridApp1\csGridApp1\bin\Debug\App.xaml"
// "c:\users\calvinh\documents\visual studio 11\Projects\csGridApp1\csGridApp1\bin\Debug\AppX\csGridApp1.exe"

WCHAR *g_AppContainerNamedObjectPath=L"";
WCHAR *g_packageFullName=L"";
PFNGetPackageFullName g_PFNGetPackageFullName = 0;
PFNGetCurrentPackagePath g_PFNGetCurrentPackagePath = 0;

int g_ImmersiveTID = 0;
BOOL g_isImmersive = 0;

CLINKAGE UINT32 ENTRYPOINT GetPathOfAppContainerNamedObject(
	_In_ LPWSTR pPackageFamilyName,
	_Out_ LPWSTR pAppContainerNamedObjectPath
	)
{
	HRESULT hr = S_OK;
	HMODULE hModuleUserEnv = LoadLibrary("userenv.dll");
	typedef LONG (WINAPI *pfnDeriveAppContainerSidFromAppContainerName)(
			_In_ PCWSTR pszAppContainerName,
			_Outptr_ PSID *ppsidAppContainerSid);
	pfnDeriveAppContainerSidFromAppContainerName fnDeriveAppContainerSidFromAppContainerName;
	fnDeriveAppContainerSidFromAppContainerName =
		(pfnDeriveAppContainerSidFromAppContainerName) GetProcAddress(hModuleUserEnv, "DeriveAppContainerSidFromAppContainerName");
							
	PSID psidAppContainer;
	hr = (*fnDeriveAppContainerSidFromAppContainerName)(pPackageFamilyName, &psidAppContainer);
	DWORD dwLength = MAX_PATH;

	auto pfnGetAppContainerNamedObjectPath = 
		(UINT (WINAPI *)(HANDLE, PSID,ULONG, LPWSTR, PULONG)) 
		GetProcAddress( GetModuleHandle("kernel32.dll"),"GetAppContainerNamedObjectPath");

	hr = (*pfnGetAppContainerNamedObjectPath)(
		0, // token
		psidAppContainer,
		MAX_PATH,
		pAppContainerNamedObjectPath,
		&dwLength);
	{
		VSASSERTF((hr == S_OK || hr == S_FALSE, "GetAppContainerNamedObjectPath hresult %x", hr));
	}

	FreeSid(psidAppContainer);
	FreeLibrary(hModuleUserEnv);
	hr = S_OK;
	return hr;
}

PFNGetPackageFamilyName g_PFNGetPackageFamilyName;



CLINKAGE UINT32 ENTRYPOINT GetPackageFullNameFromProcess(
	_In_ HANDLE hProcess,
	_Inout_ LPWSTR pPackageFullName
	)
{
	HRESULT hr = E_FAIL;
	UINT32 dwPackageFullNameLength= MAX_PATH;
    HMODULE hK32 = GetModuleHandle("kernel32");
	pPackageFullName[0] =0;
	g_PFNGetPackageFullName = (PFNGetPackageFullName)GetProcAddress(hK32, "GetPackageFullName");
	if (g_PFNGetPackageFullName )
	{
		g_PFNGetPackageFullName(hProcess, &dwPackageFullNameLength, pPackageFullName);
		if (pPackageFullName[0])
		{
			hr = S_OK;
		}
	}
	// failure here means not immersive

	return hr;
}

CLINKAGE UINT32 ENTRYPOINT MyGetCurrentPackagePath( //note: no trailing backslash "c:\users\calvinh\documents\visual studio 2012\Projects\csGridApp1\csGridApp1\bin\Debug\AppX"
		_Inout_ UINT32 *packagePathLength,
		_Out_opt_ PWSTR pPackagePath
	)
{
	HRESULT hr = E_FAIL;
    HMODULE hK32 = GetModuleHandle("kernel32");
	pPackagePath[0] =0;
	g_PFNGetCurrentPackagePath = (PFNGetCurrentPackagePath)GetProcAddress(hK32, "GetCurrentPackagePath");
	if (g_PFNGetCurrentPackagePath)
	{
		g_PFNGetCurrentPackagePath(packagePathLength, pPackagePath);
		if (pPackagePath)
		{
			hr = S_OK;
		}
	}

	return hr;
}



CLINKAGE UINT32 ENTRYPOINT GetPackageFamilyNameFromProcess(
	_In_ HANDLE hProcess,
	_Inout_ LPWSTR pPackageFamilyName
	)
{
	HRESULT hr = E_FAIL;
	UINT32 dwPackageFamilyNameLength= MAX_PATH;
    HMODULE hK32 = GetModuleHandle("kernel32");
	g_PFNGetPackageFamilyName =  (PFNGetPackageFamilyName)GetProcAddress(hK32, "GetPackageFamilyName");
	if (g_PFNGetPackageFamilyName )
	{
		g_PFNGetPackageFamilyName(hProcess, &dwPackageFamilyNameLength, pPackageFamilyName);

		hr = S_OK;
	}
	// failure code means not immersive
	return hr;
}




CLINKAGE UINT32 ENTRYPOINT GetPackageNamesFromManifest(
                    _In_ LPWSTR packagePath,
					_In_ UINT nAppIndex, // 0 means 1st one, etc. could be Multiple Apps in a package
					_Inout_ LPWSTR *pPackageName,		//Package Name : 226e5454-7bb8-43ac-b20a-17bb711430b8
					_Inout_ LPWSTR *pPackageFullName,  //Package Full Name : 226e5454-7bb8-43ac-b20a-17bb711430b8_1.0.0.0_neutral__faknarqntgs30
					_Inout_ LPWSTR *pPackageFamilyName,  //Package Family Name : 226e5454-7bb8-43ac-b20a-17bb711430b8_faknarqntgs30
					_Inout_ LPWSTR *pAppUserModelId,	//"226e5454-7bb8-43ac-b20a-17bb711430b8_faknarqntgs30!App"
					_Inout_ LPWSTR pAppContainerNamedObjectPath //AppContainerNamedObjects\S-1-15-2-164615428-3030373648-1861741472-1867675152-3734011038-2714587842-4209093548
					)
{
	HRESULT hr;
	CComPtr<IAppxFactory> pAppxFactory;

	hr = CoCreateInstance(
		__uuidof(AppxFactory),
		NULL, 
		CLSCTX_INPROC_SERVER,
		__uuidof(IAppxFactory),
		(LPVOID *)&pAppxFactory);

	if (SUCCEEDED(hr))
	{
		CComPtr<IStream> inputStream;
		hr = SHCreateStreamOnFileEx(packagePath,
			STGM_READ | STGM_SHARE_DENY_WRITE,
			0, //default file attributes
			FALSE, // do not create new file
			NULL, // no template
			&inputStream);

		if (SUCCEEDED(hr))
		{
			CComPtr<IAppxManifestReader> pAppxManifestReader;
			CComPtr<IAppxPackageReader> pAppxPackageReader;
			CComPtr<IAppxManifestPackageId> pAppxManifestPackageId;
			hr = pAppxFactory->CreateManifestReader(inputStream, &pAppxManifestReader);
			if (SUCCEEDED(hr))
			{
				hr = pAppxManifestReader->GetPackageId(&pAppxManifestPackageId);
				if (SUCCEEDED(hr))
				{

					hr = pAppxManifestPackageId->GetPackageFullName(pPackageFullName);
					if (SUCCEEDED(hr))
					{
						hr = pAppxManifestPackageId->GetName(pPackageName);
						hr = pAppxManifestPackageId->GetPackageFamilyName(pPackageFamilyName);

						hr = GetPathOfAppContainerNamedObject(*pPackageFamilyName, pAppContainerNamedObjectPath);
						//HMODULE hModuleUserEnv = LoadLibrary("userenv.dll");
						//typedef LONG (WINAPI *pfnDeriveAppContainerSidFromAppContainerName)(
						//		_In_ PCWSTR pszAppContainerName,
						//		_Outptr_ PSID *ppsidAppContainerSid);
						//pfnDeriveAppContainerSidFromAppContainerName fnDeriveAppContainerSidFromAppContainerName;
						//fnDeriveAppContainerSidFromAppContainerName =
						//	(pfnDeriveAppContainerSidFromAppContainerName) GetProcAddress(hModuleUserEnv, "DeriveAppContainerSidFromAppContainerName");
						//	
						//PSID psidAppContainer;
						//hr = (*fnDeriveAppContainerSidFromAppContainerName)(*pPackageFamilyName, &psidAppContainer);
						//DWORD dwLength = MAX_PATH;

						//auto pfnGetAppContainerNamedObjectPath = 
						//	(UINT (WINAPI *)(HANDLE, PSID,ULONG, LPWSTR, PULONG)) 
						//	GetProcAddress( GetModuleHandle("kernel32.dll"),"GetAppContainerNamedObjectPath");

						//hr = (*pfnGetAppContainerNamedObjectPath)(
						//	0, // token
						//	psidAppContainer,
						//	MAX_PATH,
						//	pAppContainerNamedObjectPath,
						//	&dwLength);


						//FreeSid(psidAppContainer);
						//FreeLibrary(hModuleUserEnv);

					}
					else
					{
						VSASSERTF((false, "GetPaathOfappContainerNamedObject hresult %x", hr));
					}
					CComPtr<IAppxManifestApplicationsEnumerator> pAppxManifestApplicationsEnumerator;
					hr = pAppxManifestReader->GetApplications(&pAppxManifestApplicationsEnumerator);
					BOOL fHasNext = false;
					if (SUCCEEDED(hr))
					{
						int nAppCount = 0;
						hr = CS_E_PACKAGE_NOTFOUND;
						while ((S_OK ==pAppxManifestApplicationsEnumerator->GetHasCurrent(&fHasNext)) && fHasNext)
						{
							if (nAppCount++ == nAppIndex)
							{
								CComPtr<IAppxManifestApplication> pAppxManifestApplication;
								hr = pAppxManifestApplicationsEnumerator->GetCurrent(&pAppxManifestApplication);
								hr = pAppxManifestApplication->GetAppUserModelId(pAppUserModelId);
								break;
							}
						}
					}
				}
			}
		}
	}
	if (hr != S_OK)
	{
		VSASSERTF((false, "GetPackageNamesFromManifest hresult %x", hr));
	}
	return hr;
}


CLINKAGE UINT32 ENTRYPOINT PackageDebuggingEnabler(
					_In_ LPWSTR pPackageFullName,
					_In_ LPWSTR strCmdLine, 
					_In_ LPWSTR strEnvVars, 
					_In_ int	 nEnable)
{
	HRESULT hr;
	CComPtr<IPackageDebugSettings> pPackageDebugSettings;
	hr = pPackageDebugSettings.CoCreateInstance(__uuidof(PackageDebugSettings));
	if (SUCCEEDED(hr))
	{
		if (nEnable)
		{
			//launches strCmdLine process like so "c:\program files\memspect\memspect.exe" -p 2816 -tid 56

			hr = pPackageDebugSettings->EnableDebugging(pPackageFullName, strCmdLine, strEnvVars);
		}
		else
		{
			hr = pPackageDebugSettings->DisableDebugging(pPackageFullName);
		}
	}
	{
		VSASSERTF((hr == S_OK, "PackageDebuggingEnabler hresult %x", hr));
	}
	return hr;
}

CLINKAGE UINT32 ENTRYPOINT PackageLauncher(
					_In_ LPWSTR pAppUserModelId,
					_Inout_ DWORD *pProcessId
					)
{
	HRESULT hr;
	*pProcessId = 0;
    CComPtr<IApplicationActivationManager> pApplicationActivationManager;
    hr = pApplicationActivationManager.CoCreateInstance(__uuidof(ApplicationActivationManager));

	if (SUCCEEDED(hr))
	{
		/*
enum ACTIVATEOPTIONS
    {
        AO_NONE	= 0,
        AO_DESIGNMODE	= 0x1,
        AO_NOERRORUI	= 0x2,
        AO_NOSPLASHSCREEN	= 0x4
    } 	ACTIVATEOPTIONS;
		*/
		hr = pApplicationActivationManager->ActivateApplication(pAppUserModelId,0, AO_NONE, pProcessId);


	// If process is running as admin, this results in 
	/*
This app can't open
csGridApp1 can't open while Windows Explorer is running with administrator priveleges. Restart Windows Explorer normally and try again
*/

	}
	{
		VSASSERTF((hr == S_OK, "PackageLauncher hresult %x", hr));
	}
	return hr;
}


CLINKAGE UINT32 ENTRYPOINT PackageResumeSuspend(
					_In_ LPWSTR pPackageFullName,
					_In_ int	 nResume)
{
	HRESULT hr;
	CComPtr<IPackageDebugSettings> pPackageDebugSettings;
	hr = pPackageDebugSettings.CoCreateInstance(__uuidof(PackageDebugSettings));
	if (SUCCEEDED(hr))
	{
		if (nResume)
		{
			hr = pPackageDebugSettings->Resume(pPackageFullName);
		}
		else
		{
			hr = pPackageDebugSettings->Suspend(pPackageFullName);
		}
	}
	VSASSERTF((hr == S_OK, "PackageResumeSuspend hresult %x", hr));
	return hr;
}


