//=--------------------------------------------------------------------------=
// Options.Cpp
//=--------------------------------------------------------------------------=
// Options dialog for debugging stuff
//=--------------------------------------------------------------------------=
// Copyright (c) 1997, Microsoft Corporation
//			All Rights Reserved
// Information Contained Herein Is Proprietary and Confidential.
//=--------------------------------------------------------------------------=
//
// Author:  BrianPe, 07/97
//
#include "pch.h"
#include "vsassert.h"
#include "commctrl.h"
#include "dump.h"
#include "main.h"
#include "mem.h"
#include "resource.h"
#include "util.h"
#include "stackwalk.h"
#include <shlobj.h>
//#include <LegacyActivationShimDelayLoad.h>

// VASSERT options environment variables
//static const char _szVsassertOptionsUseDefault[] = "VSASSERT_OPTIONS_DEFAULT";  // if set, the default options will be used (ignore options in vsassert.ini)





//=--------------------------------------------------------------------------=
// Private functions
//

static void _ReadOptions();
static void _UpdateCurrentOptions();


//=----------------------------------------------------------------------=
// Private goodies
//

// default section name
static const TCHAR _szDefaults[] = "Defaults";

// Switches tab
static const TCHAR _szDataProp[] = "TreeView_StyleData";

WCHAR g_szGlobal[20];

//=----------------------------------------------------------------------=
// Buffers we keep to put options data into.
//
TCHAR g_szDllsToAssertOn[MAX_PATH] = ""; //"ole32.dll;oleaut32.dll;shell32.dll"; comma sep dlls to assert when loaded
TCHAR g_JustTheseProcesses[MAX_PATH] = ""; // 


static TCHAR _szSymbolsPath[MAX_SYMBOLS_PATH] = "";

TCHAR g_szProcessExeFullPathName[MAX_PATH];  // user options section, actually exe full file name
TCHAR g_szVSAssertDllFullPathName[MAX_PATH]; //dll full file name of VSAssert.dll;

// we do not apply report leak options immediatelly, so we need a copy of them


static BOOL      s_fSaveCallstack = g_fSaveCallstack;
static int       s_nStackFrames = g_nStackFrames;

static BOOL      s_fEnableGdiLeakCheck = g_fEnableGdiLeakCheck;
static BOOL      s_fDontReportKnownGDILeaks = g_fDontReportKnownGDILeaks;
static BOOL      s_fDontReportSmallGDILeaks = FALSE;
static UINT      s_uicMinGdiHits = 1;
static BOOL     s_fTrackVirtualMem = FALSE;
static BOOL     s_fTrackHeap = FALSE;
static BOOL     s_fMessageBoxOnStart = FALSE;
static int   s_nSharedMemSize = 4096;
static BOOL s_StartChildProcess = true;


bool g_fIsRunDLL = false; // the Assert dialog can be out of proc, using Rundll as the exe
BOOL g_fIsChildProcess = false; // the Assert dialog can be out of proc, using Rundll as the exe, and CLR Profiling can load Vsassert into any managed proc

struct CIntOption
{
    char * szName;
    int  * piValue;
};

struct CStringOption
{
    char * szName;
    char * szValue;
    UINT   uicValue;
};

static const CIntOption  _VsAssertIntOptions[] =
{

    // output tab options
    "Asserts", (int *)&g_dfAssertFlags,
    "DebugPrintf", (int *)&g_dfPrintfFlags,

    // memory tab options
    "fDumpMemoryStats", (int *)&g_fDumpMemStats,
    "DumpMemoryStatsInterval", (int *)&g_nDumpMemStatInterval,
    "LeakBreakpointSkipCount", (int *)&g_ulLeakBreakSkipCount,

    // leak report tab options

    "fSaveCallstacks", (int *)&g_fSaveCallstack,
    "NumberOfStackFrames", (int *)&g_nStackFrames,

    "fReportGDILeaks", (int *)&s_fEnableGdiLeakCheck,
    "fDontReportKnownGDILeaks", (int *)&s_fDontReportKnownGDILeaks,
    "fDontReportSmallGDILeaks", (int *)&s_fDontReportSmallGDILeaks,
    "SmallGDILeakHits", (int *)&s_uicMinGdiHits,

    // detour options
    "fTrackVirtualMem", (int *)&s_fTrackVirtualMem,
    "fTrackHeap", (int *)&s_fTrackHeap,
    "fTrackLoadResource", (int *)&g_fTrackLoadResource,

    "fMessageBoxOnStart", (int *)&s_fMessageBoxOnStart,


    "nSharedMemSize", (int *)&s_nSharedMemSize,
    "NativeOnly", (int *)&g_NativeOnly,
    "TrackClrObjects", (int *)&g_TrackClrObjects,
    "TrackGCStacks", (int *)&g_fTrackGCStacks,
    "DynStackSymRes", (int *)&g_DynStackSymRes,
    "StartChildProcess", (int *)&s_StartChildProcess,
    "TrackCodeMarkers", (int *)&g_TrackCodeMarkers,
    "TrackJit", (int *)&g_TrackJit,
    "TrackExcpt", (int *)&g_TrackExcpt,
	"TrackETW", (int *)&g_TrackETW,
	"TrackArenas", (int *)&g_TrackArenas,
    "TrackingMode", (int *)&g_TrackingMode,
    "StackStorageMode", (int *)&g_StackStorageMode,
    "TrackFileLoadUnload", (int *)&g_TrackFileLoadUnload,
    "EnableAsserts", (int *)&g_fEnableAsserts,
    "CheckGCLostObjects", (int *)&g_CheckGCLostObjects,
    "fHandle4gigStacks", (int *)&g_fHandle4gigStacks,
    "fTrackThreadCreate", (int *)&g_fTrackThreadCreate,
    "fTrackGDI", (int *)&g_fTrackGDI,
    "ThreadDynamic", (int *)&g_ThreadDynamic,
    "fUseGlobalName", (int *)&g_fUseGlobalName,
    "CleanUpRCWs", &g_CleanUpRCWs,
    "ImmersiveSession", (int *)&g_ImmersiveSession,
    "ImmersiveTID", &g_ImmersiveTID,
    "VirtualAllocTopDown", (int *)&g_VirtualAllocTopDown,
    "ShowManagedSourceInStacks", (int *)&g_ShowManagedSourceInStacks,
	"CodeMarkerParameter", (int *)&g_CodeMarkerParameter,

};

static const CIntOption  _VsAssertReadOnlyIntOptions[] =
{
    "fAlwaysEnableFastGetStack", (int *)&g_fEnableFastGetStack,
    "UpdateLoadedModulesTimeout", (int *)&g_dwUpdateLoadedModulesTimeout,
    "NumberOfGDIStackFrames(8 max)", (int *)&g_GdiTrace_nStackFrames,
    "RetailHeaderTrailerSize", (int *)&g_nRetailHeaderTrailerSize
};

static const CStringOption _VsAssertStringOptions[] =
{
    // leak report tab options
    "SymbolsPath", _szSymbolsPath, sizeof(_szSymbolsPath),
    "DllsToAssertOn", g_szDllsToAssertOn, sizeof(g_szDllsToAssertOn),
    "JustTheseProcesses", g_JustTheseProcesses, sizeof(g_JustTheseProcesses),
};




// Private data


//=--------------------------------------------------------------------------=
// DepersistOptions
//=--------------------------------------------------------------------------=
// Called from main to load up the debug options
// return False if we don't want to load for this process
bool DepersistOptions()
{
    char szEnvVar[MAX_PATH];
    char szOptionsFile[MAX_PATH + 50];
    char szOptionsDir[MAX_PATH];
    BOOL fSuitesRun = FALSE;
    BOOL fDefault = FALSE;

    *szEnvVar = 0;
    *szOptionsFile = 0;
    *szOptionsDir = 0;


    *szEnvVar = 0;


    *szEnvVar = 0;


    char szDrive[_MAX_DRIVE];
    char szDir[MAX_PATH];
    char szName[MAX_PATH];

    char szDriveEXE[MAX_PATH];
    char szDirEXE[MAX_PATH];
    char szNameEXE[MAX_PATH];
    WCHAR wszPackageFullName[MAX_PATH];
    char szPackageFullName[MAX_PATH];
    if (0 == GetPackageFullNameFromProcess(GetCurrentProcess(), wszPackageFullName)) // we want to determine if Immersive
    { //like "7582e171-23da-47ac-8327-320e9a641669_1.0.0.0_neutral__faknarqntgs30"
        UINT32 len = dimensionof(wszPackageFullName);
        //now we want to get the immersive package path: , like "C:\Program Files\WindowsApps\ZeptoLabUKLimited.CutTheRope_1.1.0.31_x86__sq9zxnwrk84pj"
        // it's not necessarily the same as the curdir or the same as the main process exe (could be WWAHost.exe for javascript)
        if (MyGetCurrentPackagePath(&len, wszPackageFullName) == ERROR_SUCCESS)
        {//note: need to add trailing backslash (and convert to ansi) "c:\users\calvinh\documents\visual studio 2012\Projects\csGridApp1\csGridApp1\bin\Debug\AppX"
            sprintf_s(szPackageFullName, "%S\\", wszPackageFullName);
            g_isImmersive = true;
        }

    }
    GetModuleFileNameA(g_hinstDll, g_szVSAssertDllFullPathName, sizeof(g_szVSAssertDllFullPathName)); // like c:\MemSpect\MemSpectDll.dll

    GetModuleFileNameA(NULL, g_szProcessExeFullPathName, sizeof(g_szProcessExeFullPathName)); // like C:\PROGRAM FILES\MICROSOFT VISUAL STUDIO 11.0\COMMON7\IDE\devenv.exe
    _splitpath(g_szProcessExeFullPathName, szDriveEXE, szDirEXE, szNameEXE, NULL);
    _strlwr(szNameEXE); //like devenv.exe
    BOOL fDidReadUserOptions = FALSE;
    BOOL fIsMemSpectWatchingMemSpect = false;
#if MSDEBUG
    //	        ::MessageBoxA(0,szNameEXE, szDirEXE,0);  // this is before INI file is read
#endif MSDEBUG
    if (
        (fIsMemSpectWatchingMemSpect = (lstrcmpi(szNameEXE, "MemSpect") == 0)
        )
        ||
        lstrcmpi(szNameEXE, "QTAgent32") == 0 ||
        // C:\PROGRAM FILES\MICROSOFT VISUAL STUDIO 11.0\COMMON7\IDE\COMMONEXTENSIONS\MICROSOFT\TESTWINDOW\vstest.executionengine.exe
        lstrcmpi(szNameEXE, "vstest.executionengine") == 0 ||
        lstrcmpi(szNameEXE, "vstest.executionengine.x86") == 0         // Dev11 unit test engine (on 64 bit OS, adds ".x86.exe"
        )
    {
        g_fIsChildProcess = true; /// the current process is the child process (not the target analysis process) in which we resolve symbols, but don't do mem analysis
    }
    else
    {
        if (lstrcmpi(szNameEXE, "rundll32") == 0) // used when showing Assert
        {// could occur when MemSpectDll.dll is loaded in System32 due to PATH
            g_fIsRunDLL = true;
            fDidReadUserOptions = true;	// dont read options
        }
    }
    if (!fDidReadUserOptions)
    {

        // try to read default options from <vsassert-path>\vsassert.ini
        if (g_isImmersive) // for immersive, assume MemSpectDll.Dll is loaded in c:\windows\System32 so it doesn't have to be signed.
        {
            _splitpath(szPackageFullName, szDrive, szDir, szName, NULL);
        }
        else
        {
            _splitpath(g_szVSAssertDllFullPathName, szDrive, szDir, szName, NULL);
        }
        sprintf(szOptionsFile, "%s%sMemSpect.ini", szDrive, szDir);

        if (0xffffffff != GetFileAttributesA(szOptionsFile)) // if found
        {
            VsSetOptionsFileName(szOptionsFile);
            fDidReadUserOptions = true;
            _ReadOptions();
        }
        else
        {
            VSASSERTF((false, "could not find MemSpect.ini file %s", szOptionsFile)); // need for at least sympath
        }
    }
    if (g_fUseGlobalName || !IsUserInteractive()) // windows service
    {
        wcscpy_s(g_szGlobal, L"Global\\");
    }
    else
    {
        g_szGlobal[0] = L'\0';
    }


    if (!g_isImmersive)
    {
        if (g_JustTheseProcesses && g_JustTheseProcesses[0] && g_JustTheseProcesses[0] != '*')
        {
            char *ptr = strstr(g_JustTheseProcesses, szNameEXE);
            if (ptr > 0 && ptr[-1] == ';' && ptr[strlen(szNameEXE)] == ';')
            {
                g_fIsChildProcess = false; // user explicitly wants to include this process
                if (fIsMemSpectWatchingMemSpect)
                {// caveat emptor
                    // if we're watching ourself, we need to remove us from the list 
                    // so when we launch child proc, avoid infinite proc launch recursion
                    ptr[0] = 0; // null term
                    WritePrivateProfileStringA(_szDefaults, "JustTheseProcesses", g_JustTheseProcesses, szOptionsFile);

                }
            }
            else
            {// all other EXEs: disable 
                g_fIsRunDLL = true;
                //return false; // dllmain will unload us. Can't do this because MemSpect API does PINvoke into this dll
            }
        }
    }




    GetEnvironmentVariableA("VS_SYMBOLS_PATH", _szSymbolsPath, sizeof(_szSymbolsPath));
    _UpdateCurrentOptions();

    // Check if the user has specified that the assert should be routed to a file
    // We still support "VBASSERT" even though "VSASSERT" is our primary choice
    //


    *szEnvVar = 0;


    //if(!g_fCollectLeaks && !fSuitesRun &&
    //	!g_fIsRunDLL // not for the VSAssert out of proc dialog in rundll32.exe
    //	)
    //{
    // clear leak report file
    //char _szBak[MAX_PATH + 25];
    //strcpy(_szBak, g_pszLeakReportFilename);
    //strcat(_szBak, ".bak");
    //MoveFileExA(g_pszLeakReportFilename, _szBak, MOVEFILE_REPLACE_EXISTING);
    //}
    return true;
}

//--------------------------------------------------------------------------


static void _UpdateCurrentOptions()
{
    g_pszSymbolsPath = *_szSymbolsPath ? _szSymbolsPath : NULL;


    if (!g_fIsRunDLL) //if we're not the Assert out of process dialog
    {
        if (!g_fIsChildProcess)
        {
            if (s_fMessageBoxOnStart && IsUserInteractive())
            {
                if (g_isImmersive)
                {
                    Sleep(15000);
                }
                else
                {
                    char szBuf[1000 + MAX_PATH];
                    sprintf_s(szBuf, "MemSpectDll.dll starting in target Process. You can now attach a debugger\r\n%s Pid = %d\r\n Cause a DebugBreak() ?", g_szProcessExeFullPathName, GetCurrentProcessId());
                    if (MessageBoxA(0, szBuf, "", MB_YESNO) == IDYES)
                    {
                        _asm int 3;
                    }
                }

            }
        }
    }
    // adjust symbol path to work around FwcWsp.dll changing our DbgHelp options in Debug version
#if MSDEBUG
    if (_szSymbolsPath && *_szSymbolsPath)
    {
        //  SymbolsPath=SRV*c:\symcache*\\ddrps\symbols*http://symweb
        //  SymbolsPath=SRV*c:\symcache*d:\

        strcpy(_szSymbolsPath, "SRV*c:\\symcache*d:\\");
    }
#endif MSDEBUG

    auto lambdaMarkers = [](char *optionName, CodeMarkerActionEnum action)
    {
        char TempBuf[10000] = "";

        VsGetProfileString(_szDefaults, optionName, TempBuf, (TCHAR *)TempBuf, sizeof(TempBuf));

        char *ptr = TempBuf;
        DWORD markerId;
        while (ptr && *ptr)
        {
            char tempStr[100];
            auto commaPos = strchr(ptr, ',');
            if (commaPos)
            {
                *commaPos = '\0';
            }
            sscanf_s(ptr,"%80s", tempStr,_countof(tempStr)); // removes white space too
            //&& *ptr && (markerId = atoi(ptr))
            if (isdigit(tempStr[0]))
            {
                markerId = atoi(tempStr);
            }
            else
            {
                markerId = GetCodeMarkerIdFromName(tempStr);
            }
            if (markerId > 0)
            {
                if (g_CodeMarkerActions == nullptr)
                {
                    g_CodeMarkerActions = new (DebugAlloc(sizeof(CodeMarkerActionSetWrapper))) CodeMarkerActionSetWrapper(MySTLAlloc<CodeMarkerActionSetWrapper>(InternalHeapToUse));
                }
                auto res = g_CodeMarkerActions->m_pStlType->find(markerId);
                if (res != g_CodeMarkerActions->m_pStlType->end())
                {
                    res->second.action |= action;
                }
                else
                {
                    g_CodeMarkerActions->m_pStlType->insert(
                        pair<DWORD, CodeMarkerAction>(
                        markerId,
                        CodeMarkerAction(action))
                        );
                }
            }
            ptr = commaPos;
            if (ptr)
            {
                ptr++; // advance past ','
            }
        }
    };

    lambdaMarkers("CodeMarkersToCollectStats",CodeMarkerAction_TakeMemStatSnapShot);

    lambdaMarkers("CodeMarkersAtWhichToTakeSnapshot",CodeMarkerAction_TakeMegaSnapshot);

    lambdaMarkers("CodeMarkersAtWhichToFreeze", CodeMarkerAction_Freeze);

    lambdaMarkers("CodeMarkersToSendStatusMsg", CodeMarkerAction_ShowInStatusMessage);

	lambdaMarkers("CodeMarkersAtWhichToCrash", CodeMarkerAction_Crash);

	lambdaMarkers("CodeMarkersAtWhichToHang", CodeMarkerAction_Hang);

	lambdaMarkers("CodeMarkersAtWhichToSleep", CodeMarkerAction_Sleep);

	lambdaMarkers("CodeMarkersAtWhichToDebugBreak", CodeMarkerAction_DebugBreak);

	lambdaMarkers("CodeMarkersAtWhichToRecur", CodeMarkerAction_Recur);


#if MSDEBUGxx
    g_memStats.CodeMarkerId = markerId;
    g_memStats.SeqNo = g_ulGlobalPassCount;
    g_memStatsVec->m_pStlType->push_back(g_memStats);
    g_memStatsVec->m_pStlType->push_back(g_memStats);
    for_each(
        g_memStatsVec->m_pStlType->begin(),
        g_memStatsVec->m_pStlType->end(),
        [&](vector < MemStats, MySTLAlloc<MemStats >>::reference  it) {
            auto x = sizeof(it);

            //		WriteFile(m_hPipeFromChild, &it, sizeof(MemStats), &nBytesWritten, 0); // send the data
    }
    );

#endif MSDEBUG



    g_fEnableGdiLeakCheck = s_fEnableGdiLeakCheck;
    g_fDontReportKnownGDILeaks = (g_fEnableGdiLeakCheck && s_fDontReportKnownGDILeaks);

    if (g_GdiTrace_nStackFrames > MAX_GDI_TRACE_CALLSTACK_FRAMES)
        g_GdiTrace_nStackFrames = MAX_GDI_TRACE_CALLSTACK_FRAMES;
    else if (g_fDontReportKnownGDILeaks && g_GdiTrace_nStackFrames < DEFAULT_GDI_TRACE_CALLSTACK_FRAMES)
        g_GdiTrace_nStackFrames = DEFAULT_GDI_TRACE_CALLSTACK_FRAMES;
    else if (g_GdiTrace_nStackFrames < MIN_GDI_TRACE_CALLSTACK_FRAMES)
        g_GdiTrace_nStackFrames = MIN_GDI_TRACE_CALLSTACK_FRAMES;

    if (g_fSaveCallstack || g_fEnableGdiLeakCheck)
        g_fEnableFastGetStack = TRUE;

    g_StartChildProcess = s_StartChildProcess;

    CreateDefaultHeap(); // do this after reading/setting options

    if (!g_fIsRunDLL) //if we're not the Assert out of process dialog
    {
        if (!g_fIsChildProcess) // if we're not the child process
        {
            if (s_fTrackVirtualMem || s_fTrackHeap)
            {
                g_fTrackVirtualMem = s_fTrackVirtualMem;
                g_fTrackHeap = s_fTrackHeap;
                InitDetours(s_fTrackVirtualMem, s_fTrackHeap, s_nSharedMemSize);
            }
        }
    }
}

//--------------------------------------------------------

static void _ReadOptions()
{
    int i;

    for (i = 0; i < _countof(_VsAssertIntOptions); i++)
    {
        *(_VsAssertIntOptions[i].piValue) = VsGetProfileInt(_szDefaults, _VsAssertIntOptions[i].szName, *(_VsAssertIntOptions[i].piValue));
    }

    for (i = 0; i < _countof(_VsAssertReadOnlyIntOptions); i++)
    {
        *(_VsAssertReadOnlyIntOptions[i].piValue) = VsGetProfileInt(_szDefaults, _VsAssertReadOnlyIntOptions[i].szName, *(_VsAssertReadOnlyIntOptions[i].piValue));
    }

    for (i = 0; i < _countof(_VsAssertStringOptions); i++)
    {
        VsGetProfileString(_szDefaults, _VsAssertStringOptions[i].szName, (char *)_VsAssertStringOptions[i].szValue, _VsAssertStringOptions[i].szValue, _VsAssertStringOptions[i].uicValue);
    }
    if (g_szDllsToAssertOn && g_szDllsToAssertOn[0])
    {
        _strlwr_s(g_szDllsToAssertOn, strlen(g_szDllsToAssertOn) + 1); // +1 sizeof include null
    }
}

