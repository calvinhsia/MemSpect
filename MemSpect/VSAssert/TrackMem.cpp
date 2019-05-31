//=--------------------------------------------------------------------------=
// TrackMem.cpp
//=--------------------------------------------------------------------------=
// Debug leak detection and memory allocation.
//=--------------------------------------------------------------------------=
// Copyright 2010 Microsoft Corporation.  All rights reserved.
// Information contained herein is proprietary and confidential.


/*
code to use Detours to hook into things like VirtualAllocEx, HeapAlloc
*/

#include "pch.h"
#include "vsassert.h"
#include "mem.h"
#include "stackwalk.h"
#include "detours30\detours.h"
#include "aclapi.h"
#include "winternl.h"
#include "psapi.h"
#include "version.h"
#include "GdiTrack.h"
#include "tlhelp32.h"
#include "ScopeGuard.h"
#include "evntrace.h"
#include "evntprov.h"
#include "TrackMem.h"


typedef struct _LDR_DLL_LOADED_NOTIFICATION_DATA {
    ULONG Flags;                    //Reserved.
    PCUNICODE_STRING FullDllName;   //The full path name of the DLL module.
    PCUNICODE_STRING BaseDllName;   //The base file name of the DLL module.
    PVOID DllBase;                  //A pointer to the base address for the DLL in memory.
    ULONG SizeOfImage;              //The size of the DLL image, in bytes.
} LDR_DLL_LOADED_NOTIFICATION_DATA, *PLDR_DLL_LOADED_NOTIFICATION_DATA;

typedef struct _LDR_DLL_UNLOADED_NOTIFICATION_DATA {
    ULONG Flags;                    //Reserved.
    PCUNICODE_STRING FullDllName;   //The full path name of the DLL module.
    PCUNICODE_STRING BaseDllName;   //The base file name of the DLL module.
    PVOID DllBase;                  //A pointer to the base address for the DLL in memory.
    ULONG SizeOfImage;              //The size of the DLL image, in bytes.
} LDR_DLL_UNLOADED_NOTIFICATION_DATA, *PLDR_DLL_UNLOADED_NOTIFICATION_DATA;

typedef union _LDR_DLL_NOTIFICATION_DATA {
    LDR_DLL_LOADED_NOTIFICATION_DATA Loaded;
    LDR_DLL_UNLOADED_NOTIFICATION_DATA Unloaded;
} LDR_DLL_NOTIFICATION_DATA, *PLDR_DLL_NOTIFICATION_DATA;

typedef const LDR_DLL_NOTIFICATION_DATA *PCLDR_DLL_NOTIFICATION_DATA;

VOID CALLBACK LdrDllNotification(
    __in      ULONG NotificationReason,
    __in      PCLDR_DLL_NOTIFICATION_DATA NotificationData,
    __in_opt  PVOID Context
);

typedef
//	_Function_class_(LDR_DLL_NOTIFICATION_FUNCTION)
VOID
NTAPI
LDR_DLL_NOTIFICATION_FUNCTION(
    __in ULONG NotificationReason,
    __in PCLDR_DLL_NOTIFICATION_DATA NotificationData,
    __in_opt PVOID Context
);

typedef LDR_DLL_NOTIFICATION_FUNCTION *PLDR_DLL_NOTIFICATION_FUNCTION;

#pragma warning(push)
#pragma warning(disable:4229)  //warning C4229: anachronism used : modifiers on data are ignored

typedef NTSTATUS(WINAPI *LdrRegisterDllNotificationFunction)(
    ULONG Flags,
    PLDR_DLL_NOTIFICATION_FUNCTION NotificationFunction,
    void* Context,
    void** Cookie
    );

typedef NTSTATUS(WINAPI *LdrUnregisterDllNotificationFunction)(
    void* Cookie
    );

#pragma warning(pop)

#define LDR_DLL_NOTIFICATION_REASON_LOADED (1)
#define LDR_DLL_NOTIFICATION_REASON_UNLOADED (2)



#ifndef STATUS_SUCCESS
#define STATUS_SUCCESS 0
#endif

#undef HeapAlloc
#undef HeapFree
#undef HeapReAlloc
#undef HeapSize


/*<Immersive stuff>*/
extern void ImmersiveTest();




typedef LONG(WINAPI *PFNGetPackageId)(
    _In_ HANDLE               hProcess,
    _Inout_ UINT32 *bufferLength,
    _Out_opt_ BYTE *pBuffer
    );
PFNGetPackageId g_PFNGetPackageId;

typedef BYTE PACKAGE_INFO_REFERENCE[MAX_PATH];

typedef LONG(WINAPI *PFNOpenPackageInfoByFullName)(
    _In_ PCWSTR packageFullName,
    /*_Reserved_*/ const UINT32 reserved,
    _Out_ PACKAGE_INFO_REFERENCE packageInfoReference
    );
PFNOpenPackageInfoByFullName g_PFNOpenPackageInfoByFullName;

/*</Immersive stuff>*/










/*
#define REM /##/
#define STRINGIZE(x,y) x##y
#define HASH_CHAR #
#define HASH(x) STRINGIZE(HASH_CHAR,)x
#define SECTION /##/ ---
#define NAMESPACE(x)
#define END_NAMESPACE(x)
#define ENUM(x)
#define ENUMVALUE(x,y) { x, _T(#y) },
#define END_ENUM(x)
#define CONDITIONAL(x) HASH(ifdef) x
#define END_CONDITIONAL(x) HASH(endif) /##/##x
#define CONDITIONAL2(x,y) HASH(if) (defined x || defined y)
#define END_CONDITIONAL2(x,y) HASH(endif) /##/##x || y
#define CPPONLY HASH(if) 0 /##/ C++ only
#define END_CPPONLY HASH(endif) /##/ C++ only
#define INTERNAL

#include "..\CodeMarkers\CodeMarkerValues.txt"
*/
//#define Codemarkers_IncludeAppEnum 1
#define Codemarkers_IncludeAllMarkers 1


/*
Manually generate CodeMarkersLUT.gh:
tf Edit VSAssert\CodeMarkersLUT.gh
tf Edit VSAssert\CodeMarkerValues.txt
cd VSAssert
copy "\\cpvsbuild\DROPS\dev11\main\raw\current\sources\vscommon\CodeMarkers\CodeMarkerValues.txt"


cl code_markers.cpp /P
copy code_markers.i CodeMarkersLUT.gh

// code_markers.cpp contains 3 lines:
// this will gen the LoookUpTable : CodeMarkerValues.gh
#include "CodeMarkerValuesLUT.h"
#include "CodeMarkerValues.txt"


*/

typedef CMyStlWrap<deque<DWORD, MySTLAlloc<DWORD> > >  DWORDdeque;

struct CodeMarkerStruct
{
    int id;
    char *MarkerName;
    BYTE IsBegin; // is it a Begin?
    BYTE Priority;  // 0 is high priority
    int idMate; // the markerId of the mate: for a Begin, it's the End, and VV
    DWORDdeque *markerStack; // read in from file as 0, used on begin markers: indicates the recursion level of the Begin for the End Marker 
    static int  _cdecl CompareCodeMarkerIds
    (
        const void * id1,
        const void *id2
    )
    {
        CodeMarkerStruct *p1 = (CodeMarkerStruct *)id1;
        CodeMarkerStruct *p2 = (CodeMarkerStruct *)id2;

        if (p1->id < p2->id)
        {
            return -1;
        }
        if (p1->id > p2->id)
        {
            return 1;
        }
        return 0;
    }
    static bool s_fDidSort;
    static void EnsureSorted()
    {
        if (!s_fDidSort)
        {
            qsort(g_MarkerStruct, sizeof(g_MarkerStruct) / sizeof(CodeMarkerStruct), sizeof(CodeMarkerStruct), CompareCodeMarkerIds);
            s_fDidSort = true;
        }
    }
} g_MarkerStruct[] =
{
#include "CodeMarkerslut.gh"
};

bool CodeMarkerStruct::s_fDidSort = false;

MapDwordDwordWrapper *g_CodeMarkerInstanceMap = 0;

CodeMarkerActionSetWrapper *g_CodeMarkerActions = nullptr;

MemStatWrapper *g_memStatsVec = nullptr;
MemStats g_memStats; // this is the single static instance that accumulates stats. 


DWORD g_ThreadCmdsToChildReady = 0;
DWORD g_fTrackingGhosts = 0;
DWORD g_SeqNoperfIdleCodeMarker = 0; // for VS, the seq no that PerfIdle (there's only ever 1) came: else 0
DWORD g_dwThreadIdChildProcess = 0;
DWORD g_dwThreadIdMemSpectBgdThreadGCAndCleanupRCW = 0;

int CProcessGC::s_nGCNumber = 0;
int CProcessGC::s_nJitNumber = 0;
int CProcessGC::s_nExcptNumber = 0;



CLINKAGE  void ENTRYPOINT GetCodeMarkerNameFromId(int  id, const char **res)
{
    *res = "UnknownMarker";
    CodeMarkerStruct::EnsureSorted();
    CodeMarkerStruct *p =
        (CodeMarkerStruct *)
        bsearch(
            &id,
            g_MarkerStruct,
            dimensionof(g_MarkerStruct),
            sizeof(g_MarkerStruct[0]),
            CodeMarkerStruct::CompareCodeMarkerIds);
    if (p)
    {
        *res = p->MarkerName;
    }
}

CLINKAGE  int ENTRYPOINT GetCodeMarkerIdFromName(const char *name) // this is very slow
{
    int nLen = strlen(name);
    CodeMarkerStruct::EnsureSorted();
    CodeMarkerStruct *p = g_MarkerStruct;
    for (int i = 0;
        i < dimensionof(g_MarkerStruct);
        i++, p++)
    {
        if (_strnicmp(name, p->MarkerName, nLen) == 0)
        {
            return p->id;
        }
    }
    return 0;
}

CodeMarkerStruct *GetCodeMarkerStructFromId(int  id)
{
    CodeMarkerStruct::EnsureSorted();
    CodeMarkerStruct *p =
        (CodeMarkerStruct *)
        bsearch(
            &id,
            g_MarkerStruct,
            dimensionof(g_MarkerStruct),
            sizeof(g_MarkerStruct[0]),
            CodeMarkerStruct::CompareCodeMarkerIds);
    return p;
}


struct _CLIENT_ID
{
    DWORD UniqueProcess;
    DWORD UniqueThread;
};


typedef struct _CLIENT_ID CLIENT_ID; ///guessing
typedef long KPRIORITY; ///guessing


typedef struct _THREAD_BASIC_INFORMATION {


    NTSTATUS                ExitStatus;
    PVOID                   TebBaseAddress;
    CLIENT_ID               ClientId;
    KAFFINITY               AffinityMask;
    KPRIORITY               Priority;
    KPRIORITY               BasePriority;



} THREAD_BASIC_INFORMATION, *PTHREAD_BASIC_INFORMATION;


typedef enum _THREAD_INFORMATION_CLASSx {

    ThreadBasicInformation,
    ThreadTimes,
    ThreadPriority,
    ThreadBasePriority,
    ThreadAffinityMask,
    ThreadImpersonationToken,
    ThreadDescriptorTableEntry,
    ThreadEnableAlignmentFaultFixup,
    ThreadEventPair,
    ThreadQuerySetWin32StartAddress,
    ThreadZeroTlsCell,
    ThreadPerformanceCount,
    ThreadAmILastThread,
    ThreadIdealProcessor,
    ThreadPriorityBoost,
    ThreadSetTlsArrayAddress,
    ThreadIsIoPendingxx, //added xx to avoid collision with C:\Program Files (x86)\Microsoft SDKs\Windows\v7.0A\Include\winternl.h
    ThreadHideFromDebugger


} THREAD_INFORMATION_CLASSx, *PTHREAD_INFORMATION_CLASSx;


typedef NTSTATUS(WINAPI *PFNtQueryInformationThread)(
    IN HANDLE               ThreadHandle,
    IN THREAD_INFORMATION_CLASSx ThreadInformationClass,
    OUT PVOID               ThreadInformation,
    IN ULONG                ThreadInformationLength,
    OUT PULONG              ReturnLength OPTIONAL
    );

PFNtQueryInformationThread g_PFNtQueryInformationThread;



//#include "nturtl.h"

//DECLARE_MUTEX( _csMemTrackLock);




CDebFreezeAllThreads * g_pCDebFreezeAllThreads = 0;
ULONG g_nThreadFreeze = 0;


void RemoveObjRefsFromList();

SECURITY_ATTRIBUTES _SECURITY_ATTRIBUTES;
SECURITY_ATTRIBUTES *_pSECURITY_ATTRIBUTES;
SECURITY_ATTRIBUTES *GetSecurityAttributes()
{
    if (_pSECURITY_ATTRIBUTES == nullptr)
    {
        _pSECURITY_ATTRIBUTES = &_SECURITY_ATTRIBUTES;
        _SECURITY_ATTRIBUTES.nLength = sizeof(SECURITY_ATTRIBUTES);
        _SECURITY_ATTRIBUTES.bInheritHandle = TRUE;
        _SECURITY_ATTRIBUTES.lpSecurityDescriptor = NULL;

        PSID pEveryoneSID = NULL, pAdminSID = NULL;
        PACL pACL = NULL;
        PSECURITY_DESCRIPTOR pSD = NULL;
        SID_IDENTIFIER_AUTHORITY SIDAuthWorld = SECURITY_WORLD_SID_AUTHORITY;
        SID_IDENTIFIER_AUTHORITY SIDAuthNT = SECURITY_NT_AUTHORITY;

        // Create a well-known SID for the Everyone group.
        if (!AllocateAndInitializeSid(&SIDAuthWorld, 1,
            SECURITY_WORLD_RID,
            0, 0, 0, 0, 0, 0, 0,
            &pEveryoneSID))
        {
            VSASSERTF((false, "AllocateAndInitializeSid everyone failed %d", GetLastError()));
        }
        // Create a SID for the BUILTIN\Administrators group.
        if (!AllocateAndInitializeSid(&SIDAuthNT, 2,
            SECURITY_BUILTIN_DOMAIN_RID,
            DOMAIN_ALIAS_RID_ADMINS,
            0, 0, 0, 0, 0, 0,
            &pAdminSID))
        {
            VSASSERTF((false, "AllocateAndInitializeSid admin failed %d", GetLastError()));
        }

        EXPLICIT_ACCESS ea[1];
        ZeroMemory(ea, sizeof(ea));
        ea[0].grfAccessPermissions = EVENT_ALL_ACCESS;
        ea[0].grfAccessMode = SET_ACCESS;
        ea[0].grfInheritance = SUB_CONTAINERS_AND_OBJECTS_INHERIT;
        ea[0].Trustee.TrusteeForm = TRUSTEE_IS_SID;
        ea[0].Trustee.TrusteeType = TRUSTEE_IS_WELL_KNOWN_GROUP;
        ea[0].Trustee.ptstrName = (LPSTR)pAdminSID;
        if (SetEntriesInAcl(1, ea, NULL, &pACL) != ERROR_SUCCESS)
        {
            VSASSERTF((false, "SetEntriesInAcl failed %d ", GetLastError()));
        }
        // Initialize a security descriptor.  

        pSD = (PSECURITY_DESCRIPTOR)DebugAlloc(SECURITY_DESCRIPTOR_MIN_LENGTH);
        if (pSD == NULL)
        {
            VSASSERTF((false, "debugAlloc PSECURITY_DESCRIPTOR failed %d", GetLastError()));
        }
        if (!InitializeSecurityDescriptor(pSD, SECURITY_DESCRIPTOR_REVISION))
        {
            VSASSERTF((false, "InitializeSecurityDescriptor failed %d", GetLastError()));
        }
        pACL = NULL; // When an object has no DACL (when the pDacl parameter is NULL), no protection is assigned to the object, and all access requests are granted.
        // 
        // Add the ACL to the security descriptor. 
        if (!SetSecurityDescriptorDacl(pSD,
            TRUE,     // bDaclPresent flag   
            pACL,
            FALSE))   // not a default DACL 
        {
            VSASSERTF((false, "SetSecurityDescriptorDacl failed %d", GetLastError()));
        }
        _SECURITY_ATTRIBUTES.lpSecurityDescriptor = pSD;
    }
    return _pSECURITY_ATTRIBUTES;
}

// CreateChildProcess : class in parent process to instantiate and communicate with a child process
class CreateChildProcess
{
private:
    HANDLE m_hChildProcess;// handle to the child process we create
    HANDLE m_hEventFromChild;        // event: when child process signals, we have a request from child
    HANDLE m_hThreadPipeListener; // thread in parent process to communicate with child
    HANDLE m_hFileMapping;  // shared mem handle
    LPVOID m_pvMappedSection; // shared mem pointer
    DWORD m_cbSharedMem;  // size of shared mem
    WCHAR  m_bstrFullPipeName[MAX_PATH];
public:
    HANDLE m_hEventFromTarget;        // event: when Parent process signals to child a request is available
    HANDLE m_hPipeFromChild;    // handle to the named pipe. The child process (MemSpect) uses this pipe to send cmds to the targ (Devenv)
    HANDLE m_hPipeFromTarget;   // handle to named pipe. The Targ proc (Devenv) uses this to comm with the child (MemSpect).

    CreateChildProcess()
    {

        m_hChildProcess = INVALID_HANDLE_VALUE;
        m_hPipeFromChild = INVALID_HANDLE_VALUE;
        m_hPipeFromTarget = INVALID_HANDLE_VALUE;
        m_hEventFromChild = INVALID_HANDLE_VALUE;
        m_hEventFromTarget = INVALID_HANDLE_VALUE;
        m_hThreadPipeListener = INVALID_HANDLE_VALUE;
        m_pvMappedSection = 0;
        m_cbSharedMem = 0;

    }

    void MakeTheEvents()
    {
        WCHAR szEventFromChildName[900];
        WCHAR szEventFromParentName[900];
        swprintf_s(szEventFromChildName, L"%sMemSpectFromChild%d", g_szGlobal, GetCurrentProcessId()); // like "Event1231" Event names are case sensitive

        m_hEventFromChild = CreateEventW(
            GetSecurityAttributes(),
            FALSE, //bManualReset
            FALSE, //bInitialState
            szEventFromChildName);

        VSASSERTF((m_hEventFromChild != 0, "failed to CreateEvent m_hEventFromChild %d", GetLastError()));

        swprintf_s(szEventFromParentName, L"%sMemSpectFromTarget%d", g_szGlobal, GetCurrentProcessId()); // like "Event1231" Event names are case sensitive
        m_hEventFromTarget = CreateEventW(
            GetSecurityAttributes(),
            FALSE, //bManualReset
            FALSE, //bInitialState
            szEventFromParentName);

        VSASSERTF((m_hEventFromTarget != 0, "failed to CreateEvent m_hEventFromTarget %d", GetLastError()));
    }

    void MakeThePipes()
    {
        if (m_hChildProcess != INVALID_HANDLE_VALUE)
        {
            CloseHandle(m_hChildProcess);
            m_hChildProcess = INVALID_HANDLE_VALUE;
        }

        if (m_hPipeFromChild != INVALID_HANDLE_VALUE)
        {
            CloseHandle(m_hPipeFromChild);
            m_hPipeFromChild = INVALID_HANDLE_VALUE;
        }
        if (m_hPipeFromTarget != INVALID_HANDLE_VALUE)
        {
            CloseHandle(m_hPipeFromTarget);
            m_hPipeFromTarget = INVALID_HANDLE_VALUE;
        }
        WCHAR szPipeName[900];
        //AppContainerNamedObjects\S-1-15-2-2230492263-497787423-4065611893-1016377898-965005438-4034910760-1470024513
        WCHAR prefix[MAX_PATH] = L"";
        //		WCHAR * prefix = L"Sessions\\2\\AppContainerNamedObjects\\S-1-15-2-2230492263-497787423-4065611893-1016377898-965005438-4034910760-1470024513";
        if (g_AppContainerNamedObjectPath[0])
        {
            swprintf_s(prefix, L"Sessions\\%d\\%s", g_ImmersiveSession, g_AppContainerNamedObjectPath);
        }
        //prefix = L"";
        swprintf_s(szPipeName, L"%s\\MemSpectPipe%d", prefix, GetCurrentProcessId()); //make the names unique per our process. Pipe names are case insensitive
        swprintf_s(m_bstrFullPipeName, L"\\\\.\\pipe\\");
        wcscat_s(m_bstrFullPipeName, szPipeName);// the pipe name is "\\.\pipe\MemSpectPipe1234"

        m_hPipeFromChild = CreateNamedPipeW(
            m_bstrFullPipeName,
            PIPE_ACCESS_DUPLEX + FILE_FLAG_FIRST_PIPE_INSTANCE,
            PIPE_TYPE_MESSAGE + PIPE_WAIT,
            1, //PIPE_UNLIMITED_INSTANCES,
            MaxMsgSize,  //nOutBufferSize
            MaxMsgSize,  //nInBufferSize
            100, // nDefaultTimeout
            GetSecurityAttributes()); // lpSecurityAttributes
        if (m_hPipeFromChild == INVALID_HANDLE_VALUE)
        {
            VSASSERTF((m_hPipeFromChild != INVALID_HANDLE_VALUE, "couldn't make m_hPipeFromChild hr = %x", GetLastError()));
        }
        // now prepare name for 2nd pipe: m_hPipeFromTarget, which is created in child process, and Open_Existing from this process.
        wcscat_s(m_bstrFullPipeName, L"two");
        if (false && g_isImmersive)
        {
            m_hPipeFromTarget = CreateNamedPipeW(
                m_bstrFullPipeName,
                PIPE_ACCESS_DUPLEX + FILE_FLAG_FIRST_PIPE_INSTANCE,
                PIPE_TYPE_MESSAGE + PIPE_WAIT,
                1, //PIPE_UNLIMITED_INSTANCES,
                MaxMsgSize,  //nOutBufferSize
                MaxMsgSize,  //nInBufferSize
                100, // nDefaultTimeout
                GetSecurityAttributes()); // lpSecurityAttributes
            //m_hPipeFromTarget = CreateFileW(
            //	m_bstrFullPipeName, 
            //	GENERIC_READ + GENERIC_WRITE,
            //	0,  //no sharing
            //	m_pSecurityAttributes,
            //	OPEN_EXISTING,
            //	0, // default attributes
            //	NULL // TEMPLATE flie
            //	);
        }



    }

    void StartCreateChildProcess(WCHAR * szChildExeFileName, DWORD cbSharedMemSize)
    {
        m_cbSharedMem = cbSharedMemSize;
        WCHAR szFileMappingName[900];


        swprintf_s(szFileMappingName, L"%sMemSpectFileMapping%d", g_szGlobal, GetCurrentProcessId());
        m_hFileMapping = CreateFileMappingW(
            INVALID_HANDLE_VALUE,   // backed by paging file
            GetSecurityAttributes(), //securityattrib
            PAGE_READWRITE,
            0,
            static_cast<DWORD>(m_cbSharedMem), // Truncation in 64-bit
            szFileMappingName);

        VSASSERTF((m_hFileMapping != 0, "failed to CreateFileMapping %d", GetLastError()));

        m_pvMappedSection = MapViewOfFile(m_hFileMapping, FILE_MAP_READ | FILE_MAP_WRITE, 0, 0, 0);
        VSASSERTF((m_pvMappedSection != 0, "failed to MapViewOfFile %d", GetLastError()));
        sprintf_s((TCHAR *)m_pvMappedSection, m_cbSharedMem, _T("Parent process putting stuff in shared mem %d"), 1);

        CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
        MakeTheEvents();

        MakeThePipes();

        StartChildProcess(szChildExeFileName);

        StartBackgroundThreadPipeListener();

        //DWORD res = ::WaitForSingleObject( hThread, INFINITE );
        return;
    }

    void StartChildProcess(WCHAR * szChildExeFileName)
    {
        if (0xffffffff != GetFileAttributesW(szChildExeFileName)) // if found
        {

            if (g_StartChildProcess && IsUserInteractive()) // not from a Windows Service
            {
                STARTUPINFOW StartupInfo = { sizeof(STARTUPINFO) };
                PROCESS_INFORMATION ProcessInformation = {};
                WCHAR achCommandLine[MAX_PATH * 3];
                // like WpfApplication1.exe 2648
                swprintf_s(achCommandLine, L"%s %d", szChildExeFileName, GetCurrentProcessId());
                if (!CreateProcessW(
                    NULL,
                    achCommandLine,
                    NULL,
                    NULL,
                    TRUE,   // inherit handles
                    CREATE_DEFAULT_ERROR_MODE | NORMAL_PRIORITY_CLASS,
                    NULL,
                    NULL,
                    &StartupInfo,
                    &ProcessInformation))
                {
                    VSASSERTF((false, "failed to create process with cmd line %S", achCommandLine));
                    return;
                }
                ::CloseHandle(ProcessInformation.hThread);// We don't need the thread handle
                m_hChildProcess = ProcessInformation.hProcess;
            }
        }
        else
        {
            if (g_StartChildProcess && IsUserInteractive())
            {
                VSASSERTF((false, "Didn't find %S", szChildExeFileName));
            }
        }
    }

    void StartBackgroundThreadPipeListener()
    {
        // Create a background thread in the parent process to process child requests
        m_hThreadPipeListener = CreateThread(NULL,         // no security
            524288,            // default stack size: STACK_SIZE_PARAM_IS_A_RESERVATION
            CreateChildProcess::ThreadProc,   // initial method
            (void *) static_cast<CreateChildProcess *>(this), // parameter to the thread proc
            0 + STACK_SIZE_PARAM_IS_A_RESERVATION,            // run immediately
            &g_dwThreadIdChildProcess);

        VSASSERTF((m_hThreadPipeListener != 0, "failed to CreateThread %d", GetLastError()));
    }
    ~CreateChildProcess()
    {
        //        SendMsgToChild(1, (char *)Quit);
    }
    static const DWORD MaxMsgSize = 1024;  // keep this small: large stuff goes via shared mem


    typedef CMyStlWrap < vector < DWORD, MySTLAlloc<DWORD >>, MySTLAlloc<DWORD>  > DataStream;
    DataStream *m_pDataStream;


    int HandleMsgVerb(char *szProcMsg, DWORD nTotBytesAvail); // return 0 for exit, else # bytes to write back in msg (< MaxMsgSize)
    int HandleMsgVerbHelper(char *szProcMsg, DWORD nTotBytesAvail)
    {
        int retval;
        __try
        {
            retval = HandleMsgVerb(szProcMsg, nTotBytesAvail);
        }
        __except (EXCEPTION_EXECUTE_HANDLER) {
            VSASSERTF((false, "Exception %x Vrb=%d", GetExceptionCode(), *szProcMsg));
        }
        return retval;

    }// return 0 for exit, else # bytes to write back in msg (< MaxMsgSize)


    void ShutDown()
    {
        CloseHandle(m_hChildProcess);
        m_hChildProcess = INVALID_HANDLE_VALUE;
        CloseHandle(m_hThreadPipeListener);
        m_hThreadPipeListener = INVALID_HANDLE_VALUE;
        CloseHandle(m_hEventFromChild);
        m_hEventFromChild = INVALID_HANDLE_VALUE;
        CloseHandle(m_hEventFromTarget);
        m_hEventFromTarget = INVALID_HANDLE_VALUE;
        CloseHandle(m_hPipeFromChild);
        m_hPipeFromChild = INVALID_HANDLE_VALUE;
        CloseHandle(m_hPipeFromTarget);
        m_hPipeFromTarget = INVALID_HANDLE_VALUE;
    }


    static DWORD WINAPI ThreadProc(void *pvThreadObject)
    {
        CreateChildProcess *pCreateChildProcess = static_cast<CreateChildProcess *>(pvThreadObject);
        //        SetThreadName(GetCurrentThreadId(), "VSAssertSrvComm", 0);

        // by the time we've reached here, the DllMain is done, so we can resume an immersive thread
        //CoInitializeEx(0, COINIT_APARTMENTTHREADED); // for immersive
        if (g_packageFullName[0])
        {
            if (g_ImmersiveTID)
            {
                //PackageResumeSuspend(g_packageFullName, /*nResume = */1);
                HANDLE hThreadImmersive = OpenThread(THREAD_SUSPEND_RESUME, false, g_ImmersiveTID); // NULL on failure
                if (hThreadImmersive != 0)
                {
                    ResumeThread(hThreadImmersive); // -1 on failure
                    CloseHandle(hThreadImmersive);
                }
            }
        }
        g_ImmersiveTID = 0;

        BOOL fDone = false;
        int nRetailHeapNamesToUpdate = 0;
        BOOL fDisconnect = false;

        while (!fDone)
        {
            DWORD res = ::WaitForSingleObject(pCreateChildProcess->m_hEventFromChild, 2000); // wait 2 secs for the event
            switch (res)
            {
            case WAIT_OBJECT_0: // the event signalled: we got a request
            {
                //    LockCritSect; can't : DoForceGC deadlock
                CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
                DWORD nTotBytesAvail = 0;
                if (PeekNamedPipe(pCreateChildProcess->m_hPipeFromChild, 0, 0, 0, &nTotBytesAvail, 0))
                {
                    if (nTotBytesAvail != 0)
                    {
                        char szProcMsg[MaxMsgSize + 1];
                        DWORD nRead = 0;
                        //read the msg
                        VSASSERT(nTotBytesAvail < sizeof(szProcMsg), "msg too big?");
                        if (ReadFile(pCreateChildProcess->m_hPipeFromChild, szProcMsg, nTotBytesAvail, &nRead, NULL))
                        {
                            szProcMsg[nTotBytesAvail] = 0; //null terminate
                            int nRes = pCreateChildProcess->HandleMsgVerb(szProcMsg, nTotBytesAvail);
                            if (*szProcMsg == Quit)
                            {
                                fDisconnect = true;
                            }
                        }
                    }
                }
                else
                {
                    DWORD err = GetLastError();
                    if (err != ERROR_BAD_PIPE)
                    {
                        VSASSERTF((false, "PeekNamedPipe failed Err = %x", GetLastError()));
                    }
                }
            }
            break;
            case WAIT_TIMEOUT:
            {
                // test to see if child process still alive. If so, just continue waiting for event
                if (pCreateChildProcess->m_hChildProcess != INVALID_HANDLE_VALUE)
                {
                    if (WaitForSingleObject(pCreateChildProcess->m_hChildProcess, 0) == WAIT_OBJECT_0) // did the proc terminate?
                    {
                        fDisconnect = true;
                    }
                }
                if (!g_nUseChildProcessForSymbols) // if we're resolving in current process, we try to convert hex addr heap names
                {
                    int temp = CHeapSpy::UpdateRetailHeapNames(/*fGetCountOnly*/true);
                    if (nRetailHeapNamesToUpdate > 0 && temp)
                    {
                        CHeapSpy::UpdateRetailHeapNames(/*fGetCountOnly*/false);
                    }
                    else
                    {
                        nRetailHeapNamesToUpdate = temp; //do it next timeout
                    }
                }
            }
            break;
            default:
                fDone = true;
            }
            if (fDisconnect)
            {
                fDisconnect = false;
                if (pCreateChildProcess->m_hChildProcess != INVALID_HANDLE_VALUE)
                {
                    CloseHandle(pCreateChildProcess->m_hChildProcess);
                    pCreateChildProcess->m_hChildProcess = INVALID_HANDLE_VALUE;
                }
                //pCreateChildProcess->MakeThePipes(); // close/reopen pipes
                if (pCreateChildProcess->m_hPipeFromChild != INVALID_HANDLE_VALUE)
                {
                    DWORD nCurInstances = 0;
                    GetNamedPipeHandleState(pCreateChildProcess->m_hPipeFromChild, NULL, &nCurInstances, NULL, NULL, NULL, NULL);
                    DisconnectNamedPipe(pCreateChildProcess->m_hPipeFromChild);
                }
                if (pCreateChildProcess->m_hPipeFromTarget != INVALID_HANDLE_VALUE)
                {
                    CloseHandle(pCreateChildProcess->m_hPipeFromTarget);
                    pCreateChildProcess->m_hPipeFromTarget = INVALID_HANDLE_VALUE;
                }
                // this call will block til another client connects. 
                ConnectNamedPipe(pCreateChildProcess->m_hPipeFromChild, NULL);
            }
        }
        pCreateChildProcess->ShutDown();

        return 0; // end the thread
    }
};

#if MSDEBUG
DWORD g_dwThreadIdToBreakOn = 0;
#endif MSDEBUG



// single instance of TrackMemClass
class TrackMemClass
{
public:
    TrackMemClass()
    {
        m_MemSpectHeap = (CHeapSpy *)VsDebHeapCreate(HEAP_GENERATE_EXCEPTIONS, "__MemSpect");
        VSASSERT(m_MemSpectHeap, "heap create failed for __MemSpect");
        // we don't want to see a callstack frame like this:
        // public\vc\inc\xtree(756) : vsassert.dll!std::_Tree<std::_Tmap_traits<void *,TrkBlock,std::less<void *>,MySTLAlloc<std::pair<void *,TrkBlock> >,0> >::insert<std::pair<void *,TrkBlock> > + 55 bytes
        // so we skip them:
#if MSDEBUG
        m_MemSpectHeap->m_nStackFramesToSkip = 6;  // hide some non-interesting inernal stackframes.
#else
        m_MemSpectHeap->m_nStackFramesToSkip = 1;  // hide some non-interesting inernal stackframes.
#endif
        m_MemSpectHeap->m_nHeaderSize = m_MemSpectHeap->m_nTrailerSize = 0;
        m_ListTrkBlocks = new (DebugAlloc(sizeof(ListTrkBlockWrapper)))
            ListTrkBlockWrapper(less<TreeKey>(), MySTLAlloc<PairAddrBlock>((HANDLE)m_MemSpectHeap));
    }
    ~TrackMemClass()
    {
        ShutDownDetours();
    }

    ListTrkBlocks * GetList()
    {
#if MSDEBUG
        VSASSERT(this && m_ListTrkBlocks, "m_ListTrkBlocks should be non-null");
        if (g_dwThreadIdToBreakOn && GetCurrentThreadId() == g_dwThreadIdToBreakOn)
        {
            _asm {int 3};
        }
#endif MSDEBUG
        return m_ListTrkBlocks->m_pStlType;
    }

    pair<ListTrkBlocks::iterator, bool> InsertIntoList(ListTrkBlocks::value_type val, int nExtraFramesToSkip = 0)
    {
#if MSDEBUGxxx
        // check to see if there are any dupes with the same address
        for (int i = bt_None + 1; i < bt_Max; i++)
        {
            auto found = m_ListTrkBlocks->m_pStlType->find(TreeKey((BlockType)i, val.first.second));
            if (found != m_ListTrkBlocks->m_pStlType->end())
            {
                if (
                    (i == bt_VirtualAlloc && val.first.first == bt_HeapCreate) /*||
                                                                               (i == bt_HeapCreate && val.first.first == bt_VirtualAlloc)*/
                    )
                {
                    VSASSERTF((true, "asdf"));
                }
                else if (
                    (i == bt_VirtualAlloc && val.first.first == bt_MapViewOfFile)
                    )
                {
                    VSASSERTF((true, "asdf"));
                }
                else if (
                    (i == bt_IndirectInfo && val.first.first == bt_MapViewOfFile)
                    )
                {
                    VSASSERTF((true, "asdf"));
                }
                else if (
                    (i == bt_IndirectInfo && val.first.first == bt_VirtualAlloc)
                    )
                {
                    VSASSERTF((true, "asdf"));
                }
                else if (
                    (i == bt_VirtualAlloc && val.first.first == bt_IndirectInfo)
                    )
                {
                    VSASSERTF((true, "asdf"));
                }
                else if (
                    (i == bt_TlsAllocFree && val.first.first == bt_CodeMarker) ||
                    (i == bt_CodeMarker && val.first.first == bt_TlsAllocFree)
                    )
                {
                    VSASSERTF((true, "asdf"));
                }
                else

                {
                    VSASSERTF((false, "duplicate blocktype %d %d %x %x", i, val.first, found->first.second, val.first.second));
                }
            }
        }

#endif MSDEBUG

        m_MemSpectHeap->m_nStackFramesToSkip += nExtraFramesToSkip;     // hide some non-interesting internal stackframes.

        auto res = m_ListTrkBlocks->m_pStlType->insert(val);

        m_MemSpectHeap->m_nStackFramesToSkip -= nExtraFramesToSkip;

        /*
        auto m_AddrNodeInstSetIterator = m_MemSpectHeap->m_AddrSetWrapper->m_pStlType->begin();
        int nCnt = 0;
        while (m_AddrNodeInstSetIterator != m_MemSpectHeap->m_AddrSetWrapper->m_pStlType->end())
        {
        CAddressNode *pn = m_AddrNodeInstSetIterator->second;
        TrkBlock * pTblk = (TrkBlock * )((char *)pn->m_pv+ 20);
        if (pTblk->m_tbSig != TrkBlock_SIGNATURE)
        {
        auto rr = pTblk;
        }
        m_AddrNodeInstSetIterator++;
        nCnt++;
        }
        */
        return res;
    }

    void InitDetours(BOOL fTrackVirtualMem, BOOL fTrackHeap, int nSharedMemSize);

    void ShutDownDetours();

    CHeapSpy * m_MemSpectHeap;

    CreateChildProcess m_CreateChildProcess;

private:


    ListTrkBlockWrapper *m_ListTrkBlocks;
};



TrackMemClass *g_TrackMemClass = NULL;

// returns -1 on failure, else # bytes returned (could be 0)
int SendMsgToChild(ProcMsgVerbs vrb, int nParamBytes, _In_ char *params, _In_count_(outbuf) int outbufSize, _Out_ char *outbuf)
{
    // have to be *very* careful what mem we use
    DWORD nBytesWritten = 0;
    int nRetval = -1;
    if (g_TrackMemClass &&
        g_TrackMemClass->m_CreateChildProcess.m_hPipeFromTarget != INVALID_HANDLE_VALUE)
    {
        if (g_ThreadCmdsToChildReady)
        {
            char SendBuf[2048];
            VSASSERTF((nParamBytes + 1 < sizeof(SendBuf), "Too many params to SendMsgToChild File %s(%d)", __FILE__, __LINE__));
            SendBuf[0] = vrb;
            for (int i = 0; i < nParamBytes; i++)
            {
                SendBuf[i + 1] = params[i];
            }
            SetEvent(g_TrackMemClass->m_CreateChildProcess.m_hEventFromTarget);
            nParamBytes += 1;   // include verb
            if (WriteFile(g_TrackMemClass->m_CreateChildProcess.m_hPipeFromTarget, SendBuf, nParamBytes, &nBytesWritten, 0) == false ||
                nBytesWritten != nParamBytes)
            {
                auto err = GetLastError();
                if (err != 536) // waiting for process to open other end of pipe
                {
                    VSASSERTF((false, "SendMsgToChild WriteFile failed %d File%s(%d)", err, __FILE__, __LINE__));
                }
            }
            else
            {
                nRetval = 0; //indicate success
                if (outbufSize) // if verb has results to return
                {
                    int nResLen = 0;
                    DWORD nBytesRead = 0;
                    if (ReadFile(g_TrackMemClass->m_CreateChildProcess.m_hPipeFromTarget, &nResLen, 4, &nBytesRead, 0) == FALSE ||
                        nBytesRead != 4)
                    {
                        VSASSERTF((false, "SendMsgToChild ReadFile failed %d File%s(%d)", GetLastError(), __FILE__, __LINE__));
                    }
                    else
                    {
                        if (nResLen)
                        {
                            VSASSERTF((nResLen < outbufSize, "SendMsgToChild result size too big %d File%s(%d)", nResLen, __FILE__, __LINE__));

                            if (ReadFile(g_TrackMemClass->m_CreateChildProcess.m_hPipeFromTarget, outbuf, nResLen, &nBytesRead, 0) == FALSE ||
                                nBytesRead != nResLen)
                            {
                                VSASSERTF((false, "SendMsgToChild ReadFile data failed %d File%s(%d)", GetLastError(), __FILE__, __LINE__));
                            }
                            else
                            {
                                outbuf[nResLen] = 0;//nullterm
                                nRetval = nResLen;
                            }
                        }
                    }
                }
            }
        }
    }
    return nRetval;
}

void FreezeProcess(bool fCleanupRCW)
{
    if (g_pCProcessGC)
    {
        DoForceGC(fCleanupRCW,/*fFreezeThreads=*/false); //minimize obj movement when examing obj refs
        g_pCProcessGC->m_GCStats.m_g_dwThreadIdMemSpectBgdThreadGCAndCleanupRCW = g_dwThreadIdMemSpectBgdThreadGCAndCleanupRCW; // so we can pass it back
        g_pCProcessGC->FreezeThreads(fCleanupRCW);
    }
    else
    {
        // not under profiler, like for notepad.exe
        if (g_nThreadFreeze++ == 0)
        {
            if (g_pCDebFreezeAllThreads == 0) // if we haven't frozen yet
            {
                LockCritSect;
                // suspend all threads except ours here
                g_pCDebFreezeAllThreads = new CDebFreezeAllThreads();
            }
        }
    }
}

CAddressNode * GetOrCreateAddressNodeFromObjectId(ObjectID obj, CProcessGC *pCProcessGC = NULL, bool fCreateIfNotFound = false)
{
    CAddressNode *dat = 0;
    if (obj) // RootReferences2 can pass in 0
    {
        if (0 && pCProcessGC) // if we're in the middle of GC, we have to translate for moved objs
        {
            // search for object in moved/surviving map
            //            VSASSERT(false,"ddd");
            auto lowerb = pCProcessGC->m_pRangeMap->m_pStlType->lower_bound(obj);
            if (lowerb != pCProcessGC->m_pRangeMap->m_pStlType->end())
            {
                Range oRange = lowerb->second;
                if (obj < oRange.first)
                {
                    lowerb--;
                    oRange = lowerb->second;
                }
                if (oRange.first <= obj && obj < oRange.first + oRange.second)
                {
                    ObjectID oIdNew = oRange.first + obj - lowerb->first;
                    if (oIdNew != obj)
                    {
                        obj = oIdNew;
                    }
                }
            }

        }
        auto thelist = g_TrackMemClass->GetList();
        auto thekey = TreeKey(bt_ClrObject, (LPVOID)obj);
        auto res = thelist->find(thekey);
        if (res != thelist->end())
        {
            CHeapSpy *pHeapSpy = g_TrackMemClass->m_MemSpectHeap;
            TrkBlock *pTrkBlk = &(*res).second;
            PVOID ptrAlloc = ((DWORD *)pTrkBlk) - Offset_AllocToTblk; // ptr to raw allocation
            dat = pHeapSpy->FindInst(ptrAlloc);
        }
#if MSDEBUG
        if (!dat && !fCreateIfNotFound)
        {
            VSASSERT(dat, "couldn't find addrnode of obj in tree");
        }
#endif MSDEBUG
        if (!dat && fCreateIfNotFound)
        {
            DWORD nSize;
            ClassID classID;
            g_pCorProfilerInfo->GetObjectSize(obj, &nSize);
            g_pCorProfilerInfo->GetClassFromObject(obj, &classID);
            TrkBlock oClrObj(nSize);
            oClrObj.ClrObject.m_ClassId = classID;
            auto resInsert = g_TrackMemClass->InsertIntoList(PairAddrBlock(thekey, oClrObj));
            VSASSERT(resInsert.second == true, "GetOrCreateAddressNodeFromObjectId failed to create if not found");
            TrkBlock *pTrkBlk = &resInsert.first->second;
            PVOID ptrAlloc = ((DWORD *)pTrkBlk) - Offset_AllocToTblk; // ptr to raw allocation
            CHeapSpy *pHeapSpy = g_TrackMemClass->m_MemSpectHeap;
            dat = pHeapSpy->FindInst(ptrAlloc);
            VSASSERT(dat, "couldn't find addrnode of obj in tree after creating if not found");
            if (g_CheckGCLostObjects >= 3)
            {
                VsDebugPrintf("\r\nGetOrCreateAddrNode  %08x ", obj);
            }
        }
    }
    return dat;
}

struct LotsAThreadsData
{
    DWORD nSleepTime;
    DWORD nAlloc;
};

DWORD WINAPI LotsOfThreadsThreadRoutine(void *pdata)
{
    LotsAThreadsData *pLotsAThreadsData = (LotsAThreadsData *)pdata;
    Sleep(pLotsAThreadsData->nSleepTime);
    if (pLotsAThreadsData->nAlloc)
    {
        WCHAR * pstr = (WCHAR *)HeapAlloc(GetProcessHeap(), 0, pLotsAThreadsData->nAlloc);
        swprintf_s(pstr, pLotsAThreadsData->nAlloc / 2, L"LotsAthreads %d SleepTime=%d", GetCurrentThreadId(), pLotsAThreadsData->nSleepTime);
    }
    return 0;
};

int RecurManyLevels(int n)
{
    if (n > 0)
    {
        return RecurManyLevels(n - 1);
    }
    return 0;
}

#define DONOTHINGONMAINTHREAD 0xFFFFFFFF

DWORD g_ExecuteOnMainThreadVerb = DONOTHINGONMAINTHREAD;
DWORD g_ExecuteOnMainThreadP1;
void ExecuteCrashCode(DWORD verb, DWORD p1)
{
    switch (verb)
    {
    case 0:	// crash
    {
        // we want to have a somewhat random crash so we get different watson bucket ids
        int nBufSize = p1;
        if (nBufSize == 0) // constant Watson bucket
        {
            char *ptr = 0;
            *ptr = 0;
        }
        else
        {
            //_asm jmp[eax]
            //	008DBEDC FF 20                jmp         dword ptr[eax]

            auto pMem = DebugAlloc(nBufSize);
            _asm mov ebx, pMem
            _asm mov[ebx], 0ffh
            _asm inc ebx
            _asm mov[ebx], 20h
            _asm mov eax, pMem
            _asm jmp eax

        }
    }
    break;
    case 1:
    {
        //hang
        LockCritSect
            while (1)
            {
                Sleep(100000);
            }
    }
    break;
    case 2:
    {
        Sleep(p1);
    }
    break;
    case 3:
    {
        //stack overflow
        RecurManyLevels(p1);
    }
    break;
    case 4:
        DebugBreak(); // _asm int 3
        break;
    }
}

int CreateChildProcess::HandleMsgVerb(char *szProcMsg, DWORD nTotBytesAvail) // return 0 for exit, else # bytes to write back in msg (< MaxMsgSize)
{
    int nRetval = 0; // return # of bytes in msg, not # bytes written to shared mem!
    char verb = *szProcMsg;
    DWORD nBytesWritten;
    VSASSERT(g_pCProcessGC == 0 || g_pCProcessGC->m_fDoingObjectDump == false, "Cannot send a named pipe messages while doing an object dump");
    switch (verb)
    {
    case GetMemSpectVersion:
    {
        sprintf_s((char *)m_pvMappedSection, m_cbSharedMem, "%d", rup); // update version number in Version.h
#if MSDEBUG
        strcat_s((char *)m_pvMappedSection, m_cbSharedMem, "D");
#endif
        //char szEnvVar[1024];
        //if(GetEnvironmentVariableA("COR_PROFILER_PATH", szEnvVar, sizeof(szEnvVar)))
        //{
        //    char szBuf[1024];
        //    int nLen = sprintf_s(szBuf, sizeof(szBuf),"COR_PROFILER_PATH= %s", szEnvVar);
        //    SendMsgToChild( UpdateStatusMessage,nLen, szBuf);
        //}

        *szProcMsg = VerbDone;
        nRetval = 1;
    }
    break;
    case GetSharedMem:
    {
        DWORD *dwRetval = (DWORD *)(szProcMsg + 1);
        MEMORYSTATUSEX *pmemstat = (MEMORYSTATUSEX *)m_pvMappedSection;
        pmemstat->dwLength = sizeof(MEMORYSTATUSEX);
        GlobalMemoryStatusEx(pmemstat);
        dwRetval[0] = (DWORD)m_hFileMapping;
        dwRetval[1] = m_cbSharedMem;
        dwRetval[2] = (DWORD)g_hDebugHeap; // the handle of our priv heap
        dwRetval[3] = (DWORD)GetProcessHeap(); // the handle of our priv heap
        dwRetval[4] = (DWORD)(g_TrackMemClass->m_MemSpectHeap->m_hHeap); // the handle of our Memspect priv heap
        dwRetval[5] = (DWORD)(g_fHandle4gigStacks);
        dwRetval[6] = g_SeqNoperfIdleCodeMarker; // the seq no that perfIdle occurred
        dwRetval[7] = GetCurrentThreadId();
        dwRetval[8] = (DWORD)m_pvMappedSection;
        dwRetval[9] = (DWORD)Offset_AllocToTblk;
        dwRetval[10] = (DWORD)GetCommandLineW();
        dwRetval[11] = (DWORD)&g_pCDebFreezeAllThreads; // non-null indicates frozen
        dwRetval[12] = (DWORD)&g_ulGlobalPassCount;
        dwRetval[13] = (DWORD)& CHeapSpy::s_OverallCurAlloc; // send the # of overall allocs
        dwRetval[14] = g_dwMainThread; // typically the UI main thread, but could be injected thread
        dwRetval[15] = IsDebuggerPresent();
        dwRetval[16] = g_StackStorageMode == InMemory ? 0 : (DWORD)&MemMap::_stats;
        dwRetval[17] = (DWORD)&g_ThreadCmdsToChildReady;
        dwRetval[18] = (DWORD)&g_fTrackingGhosts;

        nRetval = 1 + 19 * sizeof(DWORD);
        if (m_hPipeFromTarget == INVALID_HANDLE_VALUE)
        {

            if (g_isImmersive)
            {

            }
            else
            {
                m_hPipeFromTarget = CreateFileW(
                    m_bstrFullPipeName,
                    GENERIC_READ + GENERIC_WRITE,
                    0,  //no sharing
                    GetSecurityAttributes(),
                    OPEN_EXISTING,
                    0, // default attributes
                    NULL // TEMPLATE flie
                );

                {
                    // for immersive, we get Access denied: low box can't access pipe created in high box
                    VSASSERTF((m_hPipeFromTarget != INVALID_HANDLE_VALUE, "couldn't make m_hPipeFromTarget hr = %x (5 == Access Denied: are u admin?", GetLastError()));
                    DWORD dwMode = PIPE_READMODE_MESSAGE;
                    auto res = SetNamedPipeHandleState(m_hPipeFromTarget, &dwMode, 0, 0);
                    VSASSERTF((res == TRUE, "SetNamedPipeHandleStatefailed %d File %s(%d)", GetLastError(), __FILE__, __LINE__));
                }

            }
        }
    }
    *szProcMsg = VerbDone;
    break;
    case GetIniFile:
        sprintf_s((char *)m_pvMappedSection, m_cbSharedMem, "%s", VsGetOptionsFileName());
        *szProcMsg = VerbDone;
        nRetval = 1;
        //{
        //    PSAPI_WORKING_SET_INFORMATION wsi;
        //    PVOID ptr = &wsi;
        //    QueryWorkingSet(GetCurrentProcess(),ptr, sizeof(PSAPI_WORKING_SET_INFORMATION));

        //}

        break;
    case GetHeapStats:
    {
        // first DWORD is # of heaps
        DWORD *ptr = (DWORD *)m_pvMappedSection;
        ptr++;// skip heap count, filled in below
        HANDLE hHeap = 0;
        int nHeaps = 0;
        while ((hHeap = VsDebGetNextHeap(hHeap, (LPCSTR *)(ptr + 1), (LPCSTR *)(ptr + 3), ptr + 5)))
        {
            // 0 is heaphandle, 1 is heapname, 2 is heapnamelen, 3 is Filename, 4 is Filenamelen, 5 is LineNo
            ptr[0] = (DWORD)hHeap;
            CHeapSpy *pHeapSpy = (CHeapSpy *)hHeap;

            ptr[2] = strlen(pHeapSpy->m_pszHeapName);
            ptr[4] = pHeapSpy->m_pszFile ? strlen(pHeapSpy->m_pszFile) : 0;
            ptr[6] = pHeapSpy->m_cCurNumAllocs;
            ptr[7] = pHeapSpy->m_cCurNumBytesAllocated;
            ptr[8] = pHeapSpy->m_cNumAllocs;
            ptr[9] = pHeapSpy->m_cNumBytesAllocated;
            ptr[10] = pHeapSpy->m_nHeaderSize;
            ptr[11] = pHeapSpy->m_nTrailerSize;
            nHeaps++;
            ptr += 12;
        }
        *(DWORD *)m_pvMappedSection = nHeaps; // fill in the count
    }

    *szProcMsg = VerbDone;
    nRetval = 1;
    break;

    case ResolveSymbolNoLineInfo:
        VSASSERTF((false, "Unused verb: ResolveSymbolNoLineInfo File %s(%d)", __FILE__, __LINE__));
        // fallthru
    case ResolveSymbol:
    {
        //currently used only for managed sym res
        // this can call into lock stackwalk, so we need to prevent deadlock
        LockCritSect;
        DWORD dwAddrToResolve = (*(DWORD *)(szProcMsg + 1));
        DWORD fIsfunctionid = (*(DWORD *)(szProcMsg + 5));

        bool fWantLineInfo = false;
        if (verb == ResolveSymbol)
        {
            fWantLineInfo = true;
        }
        DWORD *ptr = (DWORD *)m_pvMappedSection;
        ptr[0] = 0; // 0 indicates error, else line len
        if (GetStringFromAddr(dwAddrToResolve, (char *)ptr, m_cbSharedMem - 4, fIsfunctionid, !fWantLineInfo, /*fIsCallingFromChildProcess =*/true))
        {
            //                    ptr[0] = strlen(&ptr[1]);
        }
    }

    *szProcMsg = VerbDone;
    nRetval = 1;
    break;
    case ResolveStackFrames:
    {
        VSASSERTF((false, "Unused verb: ResolveStackframes File %s(%d)", __FILE__, __LINE__));
        // this can call into lock stackwalk, so we need to prevent deadlock
        LockCritSect;
        CAddressNode *pAddressNode = (CAddressNode *)(*(DWORD *)(szProcMsg + 1));
        int nIndex = *(DWORD *)(szProcMsg + 5);
        UINT nLastFrameToGet = *(DWORD *)(szProcMsg + 9);
        char *ptr = (char *)m_pvMappedSection;
        ptr += 4;
        UINT nCnt = 0;
        for (UINT i = nIndex; i < nLastFrameToGet; i++)
        {
            GetStringFromAddr(pAddressNode->m_pdwStackAddr[i], ptr, m_cbSharedMem - (ptr - (char *)m_pvMappedSection));
            ptr += strlen(ptr) + 1; // skip nullterm
            VSASSERT(ptr < (char *)m_pvMappedSection + m_cbSharedMem, "callstack too big for shared mem");
            nCnt += 1;
        }
        ((DWORD *)m_pvMappedSection)[0] = nCnt; // 0 indicates # of strings
    }

    *szProcMsg = VerbDone;
    nRetval = 1;
    break;

    case ForceGC:
    {
        DWORD dwRCWCleanUp = (*(DWORD *)(szProcMsg + 1));

        if (g_pCProcessGC)
        {
            g_pCProcessGC->m_objLookRef = 0;
        }
        HRESULT hr = DoForceGC(/*fDoRCWCleanup=*/ dwRCWCleanUp != 0, /*fFreezeThreads=*/false);
        DWORD *ptr = (DWORD *)m_pvMappedSection;
        ptr[0] = hr; // return hresult from GC
    }
    *szProcMsg = VerbDone;
    nRetval = 1;
    break;
    case GetHeapAllocs:
    {
        VSASSERT(g_pCDebFreezeAllThreads, "Trying to dump heap when not frozen?");
        //3 params: hHeap (CHeapSpy), SeqnoLo, SeqnoHi  (-1 means no seq filter)
        DWORD dwParam = (*(DWORD *)(szProcMsg + 1));
        DWORD filtLo = (*(DWORD *)(szProcMsg + 5));
        DWORD filtHi = (*(DWORD *)(szProcMsg + 9));
        if (filtHi == 0)
        {
            filtHi = g_ulGlobalPassCount;
        }
        CHeapSpy *pHeapSpy = (CHeapSpy *)dwParam;
        DWORD nItemCnt = 0;
        DWORD nMaxItems = 0;
        CAddressNode *pAddressNode, **pDest;
        vector<CAddressNode *> pResults; // uses DebugAlloc
        // Need to chunk data
        DWORD nChunkSize = 2048;
        if (CHeapSpy::IsValidHeap(pHeapSpy))
        {
            UINT i = 0;

            auto lambdaFiltAlloc = [&]() {
                VSASSERT(pAddressNode, "Addrnode not found in memspect heap for GetHeapAllocs");
                if (pAddressNode)
                {
                    if (filtLo == 0xffffffff || (pAddressNode->m_cAlloc >= filtLo && pAddressNode->m_cAlloc <= filtHi))
                    {
                        *pDest++ = pAddressNode;
                        nItemCnt++;
                        if (nItemCnt == nChunkSize)
                        {
                            WriteFile(m_hPipeFromChild, &nItemCnt, 4, &nBytesWritten, 0);
                            WriteFile(m_hPipeFromChild, &pResults.at(0), 4 * nItemCnt, &nBytesWritten, 0);
                            pResults.clear();
                            nItemCnt = 0;
                            pResults.resize(nChunkSize);
                            pDest = &pResults[0];
                        }
                    }
                }
            };
            if (pHeapSpy == g_TrackMemClass->m_MemSpectHeap)
            {
                ListTrkBlocks* thelist = g_TrackMemClass->GetList();
                nMaxItems = thelist->size();
                if (nMaxItems)
                {
                    pResults.resize(min(nChunkSize, nMaxItems));
                    pDest = &pResults[0];
                    for (auto iter = thelist->begin(); iter != thelist->end(); iter++)
                    {
                        TrkBlock *pTrkBlock = &iter->second;
                        PVOID ptrAlloc = ((DWORD *)pTrkBlock) - Offset_AllocToTblk; // ptr to raw allocation
                        pAddressNode = pHeapSpy->FindInst(ptrAlloc);
                        lambdaFiltAlloc();
                    }
                }
            }
            else {
                nMaxItems = pHeapSpy->m_cCurNumAllocs;
                if (nMaxItems)
                {
                    pResults.resize(min(nChunkSize, nMaxItems));
                    pDest = &pResults[0];
                    pAddressNode = pHeapSpy->EnumReset();
                    for (i = 0; i < nMaxItems; i++, pAddressNode = pHeapSpy->EnumNext())
                    {
                        lambdaFiltAlloc();
                    }
                }
            }
            pResults.resize(nItemCnt);
        }
        else
        {
            VSASSERTF((false, "invalid heap for GetHeapAllocs Handle = %08x File %s(%d)", pHeapSpy, __FILE__, __LINE__));
        }
        WriteFile(m_hPipeFromChild, &nItemCnt, 4, &nBytesWritten, 0);
        if (nItemCnt)
        {
            WriteFile(m_hPipeFromChild, &pResults.at(0), 4 * nItemCnt, &nBytesWritten, 0);
            // write terminating zero count
            nItemCnt = 0;
            WriteFile(m_hPipeFromChild, &nItemCnt, 4, &nBytesWritten, 0);
        }
    }
    *szProcMsg = VerbDone;
    nRetval = 1;
    break;

    case GetFirstHeapBlock:
        VSASSERT(g_pCDebFreezeAllThreads, "Trying to dump heap when not frozen?");
        //fall thru
    case GetNextHeapBlocks:
    { // don't use datastream here: too much memory
        VSASSERTF((false, "Unused verb: GetNextHeapBlocks File %s(%d)", __FILE__, __LINE__));
        DWORD dwParam = (*(DWORD *)(szProcMsg + 1));
        UINT nIndex = (*(DWORD *)(szProcMsg + 5));; //index into entire thing (could be larger than sharedmemsize)
        DWORD *ptr = (DWORD *)m_pvMappedSection;
        VSASSERT(verb == GetNextHeapBlocks || nIndex == 0, "GetNext should pass in index");
        ULONG nCnt = 0;  // just this block
        CHeapSpy *pHeapSpy = (CHeapSpy *)dwParam;
        if (CHeapSpy::IsValidHeap(pHeapSpy)) // is it one of ours?
        {
            ULONG nNumFit = (m_cbSharedMem >> 2) - 6; // # allocs fit in shared mem
            CAddressNode *pAddressNode;
            if (nIndex == 0)
            {
                pAddressNode = pHeapSpy->EnumReset();
            }
            else
            {
                pAddressNode = pHeapSpy->EnumNext();
            }
            bool fFirstTime = true;
            for (; pAddressNode && nCnt < nNumFit;)
            {
                if (fFirstTime)
                {
                    fFirstTime = false;
                }
                else
                {
                    pAddressNode = pHeapSpy->EnumNext(); // we want to store nullterm
                }
                if (pAddressNode)
                {
                    if (pHeapSpy == g_TrackMemClass->m_MemSpectHeap)
                    {
                        /// we want just those allocations that are TrkBlocks: todo: 
                        /// instead of iterating the heap, iterate the list
                        if (pAddressNode->m_cb == sizeof(ListTrkBlocks::value_type) + 4 * 4)
                        {
#if MSDEBUG
                            if (((TrkBlock *)(Offset_AllocToTblk * 4 + (char *)(pAddressNode->m_pv)))->m_tbSig == TrkBlock_SIGNATURE)
#endif MSDEBUG
                            {
                                ptr[nCnt++] = (DWORD)pAddressNode;
                            }
                        }
                    }
                    else
                    {
                        ptr[nCnt++] = (DWORD)pAddressNode;
                    }
                }
            }
            VSASSERT(pHeapSpy == g_TrackMemClass->m_MemSpectHeap || // ignore this assert for Memspect: PeekNamedPipe allocates mem. Also, native non-frozen threads could be created/allocate
                (pHeapSpy->m_cCurNumAllocs == 0 || pAddressNode || nIndex + nCnt == pHeapSpy->m_cCurNumAllocs), "# allocs diff from enum");
        }
        DWORD *dwRetval = (DWORD *)(szProcMsg + 1);
        dwRetval[0] = nCnt;
    }
    *szProcMsg = VerbDone;
    nRetval = 1 + 1 * sizeof(DWORD);
    break;
    case DoPivot: //  3 params: hHeap, index, StackFrameAddr to pivot. Returns in shared mem PAllocationStruct: return value = # of items. 
    { // don't use datastream here: too much memory
        DWORD dwParam = (*(DWORD *)(szProcMsg + 1));
        UINT nIndex = (*(DWORD *)(szProcMsg + 5));; //index into entire thing (could be larger than sharedmemsize)
        UINT nTargetPivotAddr = (*(DWORD *)(szProcMsg + 9));  //Target address
        DWORD *ptr = (DWORD *)m_pvMappedSection;
        ULONG nCnt = 0;  // just this block
        CHeapSpy *pHeapSpy = (CHeapSpy *)dwParam;
        if (CHeapSpy::IsValidHeap(pHeapSpy)) // is it one of ours?
        {
            ULONG nNumFit = (m_cbSharedMem >> 2) - 6; // # allocs fit in shared mem
            CAddressNode *pAddressNode;
            if (nIndex == 0)
            {
                pAddressNode = pHeapSpy->EnumReset();
            }
            else
            {
                pAddressNode = pHeapSpy->EnumNext();
            }
            bool fFirstTime = true;
            for (; pAddressNode && nCnt < nNumFit;)
            {
                if (fFirstTime)
                {
                    fFirstTime = false;
                }
                else
                {
                    pAddressNode = pHeapSpy->EnumNext(); // we want to store nullterm
                }
                if (pAddressNode)
                {
                    auto stackArr = pAddressNode->GetStackArray();
                    for (ULONG i = 0; i < pAddressNode->m_uicStackAddr; i++)
                    {
                        if (nTargetPivotAddr == stackArr[i])
                        {
                            ptr[nCnt++] = (DWORD)pAddressNode;
                            break;
                        }
                    }
                }
            }
        }
        DWORD *dwRetval = (DWORD *)(szProcMsg + 1);
        dwRetval[0] = nCnt;
    }
    *szProcMsg = VerbDone;
    nRetval = 1 + 1 * sizeof(DWORD);
    break;
    case GetFirstObjRefs:
    case GetNextObjRefs:
    {
        VSASSERTF((false, "dead code"));
        //accum data in a stream
        DWORD dwParam = (*(DWORD *)(szProcMsg + 1));
        DWORD *ptr = (DWORD *)m_pvMappedSection;
        UINT nIndex = 0; //index into vector
        if (verb == GetFirstObjRefs)
        {
            VSASSERT(m_pDataStream == 0, "datastream should be 0");
            if (m_pDataStream)
            {
                m_pDataStream->freemem(); // prevent leaks
            }
            m_pDataStream = new (DebugAlloc(sizeof(DataStream))) DataStream(MySTLAlloc<DWORD>(InternalHeapToUse));
            // Does GC, which collects lots of info: need to partition into a single DWORD stream with 3 sections
            // 0: Refs To/From me ElemSize = 2 DWORD (REFTOME  or REFBYME, ObjectID )
            // 1: Roots  ElemSize = 3 DWORDS (ObjectID, (USHORT FLAG) << 16 + USHORT KIND, rootId))
            // 2: GCRoot array of arrays.  ElemSize =  DWORD (ObjectID): Section starts DWORD =# of GCRoots, followed by the paths to the root
            //           the paths are each preceeded by the DWORD path len, followed by DWORD ObjectIDs.
            // Each section 0,1,2 starts with a DWORD indicating # of elements in section, followed by the data
            // if not enough room in shared mem, then call back with nIndex =DWORD index into stream.
            // 1st DWORD in shared mem is always # of DWORDS of stream in the shared mem.
            // 2nd DWORD is continuing data flow
            LockCritSect;
            g_pCProcessGC->m_objLookRef = dwParam;
            g_pCProcessGC->m_objLookParam = (*(DWORD *)(szProcMsg + 5));
            DoForceGC(/*fDoRCWCleanup=*/ false,/*fFreezeThreads=*/false);
            // section 0 Refs To/From
            m_pDataStream->m_pStlType->push_back(g_pCProcessGC->m_pvecObjRefs->m_pStlType->size());
            for_each(
                g_pCProcessGC->m_pvecObjRefs->m_pStlType->begin(),
                g_pCProcessGC->m_pvecObjRefs->m_pStlType->end(),
                [this](vecPairIntObjectIdType::reference it)
            {
                m_pDataStream->m_pStlType->push_back(it.first); // nMode == REFTOME or REFFROMME
                m_pDataStream->m_pStlType->push_back((DWORD)GetOrCreateAddressNodeFromObjectId(it.second));
            }
            );
            // section 1 RootObjs
            m_pDataStream->m_pStlType->push_back(g_pCProcessGC->m_pRootRefs->m_pStlType->size());
            for_each(
                g_pCProcessGC->m_pRootRefs->m_pStlType->begin(),
                g_pCProcessGC->m_pRootRefs->m_pStlType->end(),
                [this](CProcessGC::RootRefsType::reference it)
            {
                m_pDataStream->m_pStlType->push_back((DWORD)GetOrCreateAddressNodeFromObjectId(it.first));
                m_pDataStream->m_pStlType->push_back(it.second.first); // flags/kinds
                m_pDataStream->m_pStlType->push_back(it.second.second); //rootid
            }
            );
            // section 2 path to GCRoots
            ULONG nRoots = g_pCProcessGC->m_pResultsPathToGCRoot->m_pStlType->size();
            m_pDataStream->m_pStlType->push_back(nRoots);
            for (ULONG i = 0; i < nRoots; i++)
            {
                vecObjId vecPath = (g_pCProcessGC->m_pResultsPathToGCRoot->m_pStlType->at(i));
                m_pDataStream->m_pStlType->push_back(vecPath.size());
                for_each(
                    vecPath.rbegin(), // need to go backwards, so iterate in reverse
                    vecPath.rend(),
                    [this](vecObjId::reference it)
                {
                    m_pDataStream->m_pStlType->push_back((DWORD)GetOrCreateAddressNodeFromObjectId(it));
                }
                );
            }
            g_pCProcessGC->m_pvecObjRefs->m_pStlType->clear();
            g_pCProcessGC->m_pRootRefs->m_pStlType->clear();
            g_pCProcessGC->m_pResultsPathToGCRoot->m_pStlType->clear();

            g_pCProcessGC->m_objLookRef = 0; // so subsequent GC's work normally

        }
        else
        {
            nIndex = dwParam; // initial index
        }
        ULONG nCnt = 0;
        if (m_pDataStream)
        {
            auto nStreamSize = m_pDataStream->m_pStlType->size();
            for (; nCnt * 4 < m_cbSharedMem - 6 && nIndex < nStreamSize; nCnt++)
            {
                ptr[nCnt] = m_pDataStream->m_pStlType->at(nIndex);
                nIndex++;
            }
            if (nIndex >= nStreamSize)
            {
                m_pDataStream->freemem();
                m_pDataStream = 0;
            }
        }
        DWORD *dwRetval = (DWORD *)(szProcMsg + 1);
        dwRetval[0] = nCnt;
    }
    *szProcMsg = VerbDone;
    nRetval = 1 + 1 * sizeof(DWORD);
    break;

    case ThreadsFreeze:
    {
        auto fCleanupRCW = (int)(*(DWORD *)(szProcMsg + 1)) != 0 ? true : false;
        FreezeProcess(fCleanupRCW);
        DWORD *dwRetval = (DWORD *)(szProcMsg + 1);
        dwRetval[0] = g_ulGlobalPassCount;
    }
    *szProcMsg = VerbDone;
    nRetval = 1 + 1 * sizeof(DWORD);
    break;
    case ThreadsUnFreeze:
    {
        DWORD save_g_ulGlobalPassCount = g_ulGlobalPassCount; // before unfreeze
        if (g_pCDebFreezeAllThreads)
        {
            g_nThreadFreeze = 1; // so only 1 try to unfreeze
        }
        if (g_pCProcessGC)
        {
            g_pCProcessGC->UnfreezeThreads();
        }
        else
        {
            if (g_pCDebFreezeAllThreads)
            {
                if (--g_nThreadFreeze == 0)
                {
                    delete g_pCDebFreezeAllThreads;
                    g_pCDebFreezeAllThreads = NULL;
                }
            }
        }
        DWORD *dwRetval = (DWORD *)(szProcMsg + 1);
        dwRetval[0] = save_g_ulGlobalPassCount;
    }
    *szProcMsg = VerbDone;
    nRetval = 1 + 1 * sizeof(DWORD);
    break;
    case GetClrData:
    {
        CAddressNode *pNode = (CAddressNode *)(*(DWORD *)(szProcMsg + 1));
        DWORD fExpandSystemString = *(DWORD *)(szProcMsg + 5);
        if (g_pCProcessGC)
        {
            g_pCProcessGC->m_CritSect.Request();
        }
        VSASSERT(g_pCDebFreezeAllThreads, "can't get info unless frozen");

        DoGetClrData(pNode, fExpandSystemString, (WCHAR *)m_pvMappedSection, MAXCLASSNAMELENWITHSTRINGContent);
        if (g_pCProcessGC)
        {
            g_pCProcessGC->m_CritSect.Release();
        }
    }
    *szProcMsg = VerbDone;
    nRetval = 1;
    break;
    case GetClassNameFromId:      //given classid, return name in pipe. p0= ClassId, p1 = dwObjectId, p2 = fExpandSystem. If ClassId is null, will get from ObjectId

    {
        ClassID classId = (*(ClassID *)(szProcMsg + 1));
        ObjectID objectId = *(ObjectID *)(szProcMsg + 5);
        bool fExpandSystemString = *(bool *)(szProcMsg + 9);
        VSASSERTF((classId || objectId, "GetClassNameFromId: Must have either classId or objectId (or both)"));

        if (classId == 1) // test comm speed mode
        {
            int nBufLen = *(int *)(szProcMsg + 9); // in wchars
            WCHAR *pBuf = (WCHAR *)m_pvMappedSection;
            //int nBufSize = MAXCLASSNAMELENWITHSTRINGContent;

            //int nBufLen = 0;
            //int nBufSize = MAXCLASSNAMELENWITHSTRINGContent;
            //const WCHAR *src = L"Foobar\r\n";
            //WCHAR *pBuf = (WCHAR *)m_pvMappedSection;
            //int nIter = *(int *)(szProcMsg+9);
            //for (int i = 0 ; i < nIter; i++)
            //{
            //    wcscpy_s(pBuf + nBufLen, nBufSize - nBufLen ,src);
            //    nBufLen += wcslen(src);
            //}
            if (objectId == 0) // shared mem
            {
            }
            else
            {
                //named pipe
                nBufLen *= 2; //convert to byte len
                WriteFile(m_hPipeFromChild, &nBufLen, 4, &nBytesWritten, 0); // send the count
                VSASSERT(nBytesWritten == 4, "byteswritten not 4");

                WriteFile(m_hPipeFromChild, pBuf, nBufLen, &nBytesWritten, 0); // send the string
                VSASSERT(nBytesWritten == nBufLen, "string not written");

                *szProcMsg = VerbDone;
                nRetval = 1;
                break;
            }
        }
        else
        {

            if (g_pCProcessGC)
            {
                g_pCProcessGC->m_CritSect.Request();
            }
            VSASSERT(g_pCDebFreezeAllThreads, "can't get info unless frozen");

            int nLen = GetClassNameFromClassId((WCHAR *)m_pvMappedSection, MAXCLASSNAMELENWITHSTRINGContent, classId, objectId, fExpandSystemString);

            if (g_pCProcessGC)
            {
                g_pCProcessGC->m_CritSect.Release();
            }
        }
    }
    *szProcMsg = VerbDone;
    nRetval = 1;
    break;

    case GetClrSize:
    {
        BlockType bt = (BlockType)((*(DWORD *)(szProcMsg + 1)));
        ClassID classId = (*(DWORD *)(szProcMsg + 5));
        ObjectID objectId = (*(DWORD *)(szProcMsg + 9));
        DWORD *ptr = (DWORD *)m_pvMappedSection;
        ptr[0] = DoGetClrSize(bt, classId, objectId);
    }
    *szProcMsg = VerbDone;
    nRetval = 1;
    break;
    case GetClrClsLayout:  // 1 param: ClassId, returns in pipe: DWORD nClassSize, DWORD nCntMems, (int iOffSet, szFieldName)nCntMems
    {
        VSASSERT(g_pCDebFreezeAllThreads, "can't get info unless frozen");
        DWORD dwData[5];
        ClassID classId = (*(DWORD *)(szProcMsg + 1));
        ModuleID moduleID = 0;
        mdTypeDef tdToken;
        ClassID classIdParent;
        HRESULT hr;
        ULONG cchBuf;
        WCHAR wszTemp[500] = { 0 };
        auto fSuccess = false;
        auto thelist = g_TrackMemClass->GetList();
        VSASSERTF((classId != NULL, "Classid cannot be null %x", classId));

        hr = g_pCorProfilerInfo->GetClassIDInfo2(classId, &moduleID, &tdToken, &classIdParent, 0, 0, 0);

        // 0x80131365 == CORPROF_E_CLASSID_IS_ARRAY
        if (hr == S_OK)
        {
            CComPtr<IMetaDataImport>  pIMetaDataImport;
            hr = MyGetModuleMetaData(moduleID, ofRead, IID_IMetaDataImport, (LPUNKNOWN *)&pIMetaDataImport);
            if (hr == S_OK)
            {
                cchBuf = dimensionof(wszTemp);
                mdToken tkExtends = NULL;
                hr = pIMetaDataImport->GetTypeDefProps(tdToken, wszTemp, cchBuf, 0, 0, &tkExtends);
                if (hr == S_OK)
                {
                    COR_FIELD_OFFSET *poffsetArr;
                    ULONG cFieldOffset;
                    ULONG ulClassSize;
                    WCHAR wszFld[MAXCLASSNAMELEN];
                    WCHAR bufFldtype[MAXElemTypeToStringLEN];
                    PCCOR_SIGNATURE pcor_sig;
                    ModuleID moduleID2;
                    CComPtr<IMetaDataImport> pIMetaDataImport2;
                    hr = MyGetClassLayout(classId, 0, 0, &cFieldOffset, &ulClassSize);

                    if (hr == S_OK)
                    {
                        hr = g_pCorProfilerInfo->GetClassIDInfo2(classId, &moduleID2, 0, 0, 0, 0, 0);
                        VSASSERTF((hr == S_OK, "GetClrClsLayout GetClassIDInfo2 failed"));
                        hr = MyGetModuleMetaData(moduleID2, ofRead, IID_IMetaDataImport, (LPUNKNOWN *)&pIMetaDataImport2);
                        VSASSERTF((hr == S_OK, "GetClrClsLayout GetModuleMetadata failed"));
                    }
                    if (hr == S_OK)
                    {
                        auto szArr = cFieldOffset * sizeof(COR_FIELD_OFFSET);
                        poffsetArr = (COR_FIELD_OFFSET *)alloca(szArr);
                        hr = MyGetClassLayout(classId, poffsetArr, szArr, &cFieldOffset, &ulClassSize);
                        VSASSERTF((hr == S_OK, "assert getclasslayout failed"));
                        if (hr == S_OK)
                        {
                            fSuccess = true;
                            dwData[0] = cFieldOffset;
                            dwData[1] = ulClassSize;
                            dwData[2] = classIdParent;
                            dwData[3] = 0;
                            dwData[4] = 0;

                            TreeKey theKey(bt_ClrClass, (LPVOID)classId);
                            auto res = thelist->find(theKey);
                            if (res != thelist->end())
                            {
                                dwData[3] = res->second.ClrClass.m_nInstance;
                                dwData[4] = res->second.ClrClass.m_nCollected;
                            }

                            WriteFile(m_hPipeFromChild, &dwData, 4 * 5, &nBytesWritten, 0); // send the count, classSize, classIdParent

                            for (UINT i = 0; i < cFieldOffset; i++)
                            {
                                ULONG fldsz = dimensionof(wszFld);
                                ULONG nSigLen = 0;
                                hr = pIMetaDataImport2->GetFieldProps(poffsetArr[i].ridOfField, NULL, wszFld, fldsz, &fldsz, 0, &pcor_sig, &nSigLen, 0, 0, 0);
                                if (hr == S_OK)
                                {
                                    fldsz--; // we want the len without the null term
                                    CorCallingConvention callconv;
                                    pcor_sig += CorSigUncompressData(pcor_sig, (ULONG *)&callconv);
                                    ULONG fieldType;
                                    pcor_sig += CorSigUncompressData(pcor_sig, (ULONG *)&fieldType);

                                    int bufFldTypeLen = ElemTypeToString(bufFldtype, dimensionof(bufFldtype), pIMetaDataImport2, (CorElementType)fieldType, &pcor_sig);

                                    dwData[0] = poffsetArr[i].ulOffset;
                                    dwData[1] = fldsz;
                                    VSASSERTF((dwData[1] > 0, "got bad bstrfld length"));
                                    WriteFile(m_hPipeFromChild, &dwData, 8, &nBytesWritten, 0); // send the offset, strlen
                                    WriteFile(m_hPipeFromChild, wszFld, 2 * fldsz, &nBytesWritten, 0); // send the str

                                    dwData[0] = bufFldTypeLen;
                                    VSASSERTF((dwData[0] > 0, "got bad bstrType length"));
                                    WriteFile(m_hPipeFromChild, &dwData, 4, &nBytesWritten, 0); // send the strlen
                                    WriteFile(m_hPipeFromChild, bufFldtype, 2 * bufFldTypeLen, &nBytesWritten, 0); // send the str

                                }
                                else
                                {
                                    // error
                                    wcscpy_s(wszFld, L"Err getting field props");
                                    fldsz = wcslen(wszFld);
                                    dwData[0] = poffsetArr[i].ulOffset;
                                    dwData[1] = fldsz;
                                    WriteFile(m_hPipeFromChild, &dwData, 8, &nBytesWritten, 0); // send the offset, strlen
                                    WriteFile(m_hPipeFromChild, wszFld, 2 * fldsz, &nBytesWritten, 0); // send the str

                                    dwData[0] = fldsz;
                                    WriteFile(m_hPipeFromChild, &dwData, 4, &nBytesWritten, 0); // send the strlen
                                    WriteFile(m_hPipeFromChild, wszFld, 2 * fldsz, &nBytesWritten, 0); // send the str
                                }
                            }
                        }
                    }
                }
            }
        }
        if (!fSuccess)
        {
            dwData[0] = -1; // indicate failure
            dwData[1] = -1; // indicate failure
            dwData[2] = -1; // indicate failure
            dwData[3] = -1; // indicate failure
            dwData[4] = -1; // indicate failure
            WriteFile(m_hPipeFromChild, &dwData, 4 * 5, &nBytesWritten, 0); // send the count
        }
    }
    *szProcMsg = VerbDone;
    nRetval = 1;
    break;
    case GetGCStatAddr: // 0 params. Returns addr of GC Stat struct, or 0 if not avail
        if (g_pCProcessGC)
        {
            *(DWORD *)m_pvMappedSection = (DWORD)&(g_pCProcessGC->m_GCStats);
        }
        else
        {
            *(DWORD *)m_pvMappedSection = 0;
        }
        *szProcMsg = VerbDone;
        nRetval = 1;
        break;
    case ClrObjTrk:
    {
        DWORD dwParam = (*(DWORD *)(szProcMsg + 1));
        auto OldVal = g_TrackClrObjects;
        DWORD *ptr = (DWORD *)m_pvMappedSection;
        ptr[0] = DoClrObjTrkToggle(dwParam);
        ptr[1] = (DWORD)g_pCProcessGC; //CLR might not even be attached, regardless of setting.
        ptr[2] = g_ulGlobalPassCount;

        if (dwParam == 0 && OldVal && ptr[0] == 0) // did we just turn off obj tracking? if so, let's reset the class counts and remove the objs
        {
            RemoveObjRefsFromList();
        }
        else
        {
            // we're turning ontracking, let's force a gc so we retrack objs
            if (g_pCProcessGC)
            {
                g_pCProcessGC->m_fDidTurnOnTrkClrObjsForRetrack = true;
                DoForceGC(/*fDoRCWCleanup=*/ false,/*fFreezeThreads=*/false);
            }
        }
    }
    *szProcMsg = VerbDone;
    nRetval = 1;
    break;
    case GetProcessHeapHandles:
    {
        DWORD *ptr = (DWORD *)m_pvMappedSection;
        ptr[0] = GetProcessHeaps(m_cbSharedMem / 4 - 1, (PHANDLE)&ptr[1]);
        VSASSERT(ptr[0] < m_cbSharedMem / 4 - 1, "too many heaps in GetProcessHeaps: need more shared mem");
    }
    *szProcMsg = VerbDone;
    nRetval = 1;
    break;
    case DoHeapWalk: // // 3 params: handle, index, fOnlyWantRegions. If index=0, it's first block. Returns # blks in 0, then consecutive PROCESS_HEAP_ENTRY structs
    { // don't use datastream here: too much memory
        HANDLE hHeap = (*(HANDLE *)(szProcMsg + 1));
        UINT nIndex = (*(DWORD *)(szProcMsg + 5));; //index into entire thing (could be larger than sharedmemsize)
        bool fOnlyWantRegions = (*(bool *)(szProcMsg + 9)); // if we only want VirtualAlloc info, not heap blocks
        DWORD *ptr = (DWORD *)m_pvMappedSection;
        ULONG nCnt = 0;  // just this block

        PROCESS_HEAP_ENTRY *pHeapEntryArray = (PROCESS_HEAP_ENTRY *)&ptr[1];
        PROCESS_HEAP_ENTRY HeapEntry;
        ULONG nNumFit = ((m_cbSharedMem - 4) / sizeof(HeapEntry)) - 1; // # allocs fit in shared mem

        if (nIndex == 0)
        {
            // first time:
            ZeroMemory(&HeapEntry, sizeof(HeapEntry));
        }
        else
        {
            auto lastEntryIndex = (nIndex - 1) % nNumFit; //mod
            memcpy(&HeapEntry, &pHeapEntryArray[lastEntryIndex], sizeof(HeapEntry));
        }

        bool fDone = false;
        while (!fDone)
        {
            auto nRes = HeapWalk(hHeap, &HeapEntry);
            if (nRes > 0)
            {
                bool fInclude = true;
                if (fOnlyWantRegions)
                {
                    if (HeapEntry.wFlags & (PROCESS_HEAP_REGION | PROCESS_HEAP_UNCOMMITTED_RANGE))
                    {
                    }
                    else
                    {
                        fInclude = false;
                    }
                }
                if (fInclude)
                {
                    memcpy(&pHeapEntryArray[nCnt++], &HeapEntry, sizeof(HeapEntry));
                    if (nCnt == nNumFit)
                    {
                        fDone = true;
                    }
                }
            }
            else
            {
                // done entire heap
                fDone = true;
            }
        }

        ptr[0] = nCnt;
    }
    *szProcMsg = VerbDone;
    nRetval = 1;
    break;
    case GetStackAddresses:
    {
        VSASSERTF((false, "Unused verb: GetStackAddresses File %s(%d)", __FILE__, __LINE__));
        CAddressNode *pAddressNode = (CAddressNode *)(*(DWORD *)(szProcMsg + 1));
        memcpy(m_pvMappedSection, pAddressNode->m_pdwStackAddr, 4 * pAddressNode->m_uicStackAddr);
    }
    *szProcMsg = VerbDone;
    nRetval = 1;
    break;
    case LotsOfThreads:
    {
        DWORD dwCnt = (*(DWORD *)(szProcMsg + 1));
        LotsAThreadsData *pLotsAThreadsData = (LotsAThreadsData *)(szProcMsg + 5);
        DWORD *ptr = (DWORD *)m_pvMappedSection;
        for (DWORD i = 0; i < dwCnt; i++)
        {
            DWORD dwThreadId = 0;
            HANDLE hThread = CreateThread(NULL,         // no security
                0,            // default stack size
                LotsOfThreadsThreadRoutine,   // initial method
                pLotsAThreadsData, // parameter to the thread proc
                0,            // run immediately
                &dwThreadId);
            ptr[i] = dwThreadId;
            CloseHandle(hThread);

        }
    }
    *szProcMsg = VerbDone;
    nRetval = 1;
    break;
    case GetClrObjDump:  // no params: will write to named pipe all CLR Objects and their references. 
    {
        //VSASSERT(g_pCProcessGC,"Request to get CLR OBJ dump when no CLR");
        if (g_pCProcessGC)
        {
            DoForceGC( /*fDoRCWCleanup =*/0,/*fFreezeThreads=*/false); // do a few so objs to minimize obj movement when we're collecting data
            DoForceGC( /*fDoRCWCleanup =*/0,/*fFreezeThreads=*/false);
            DoForceGC( /*fDoRCWCleanup =*/0,/*fFreezeThreads=*/false);
            g_pCProcessGC->m_fDoingObjectDump = true;
            g_pCProcessGC->m_fDidSignalEndDumpingGCRoots = false;
            DoForceGC( /*fDoRCWCleanup =*/0,/*fFreezeThreads=*/false);
            g_pCProcessGC->m_fDoingObjectDump = false;
            //verbdone sent in GCFinished
        }
        else
        {
            *szProcMsg = VerbDone;
            nRetval = 1;
        }
    }
    break;
    case DoVirtualAlloc:
    case DoVirtualFree:
    {
        DWORD *dwParams = (DWORD *)(szProcMsg + 1);
        DWORD res = 0;
        if (verb == DoVirtualAlloc)
        {
            res = (DWORD)VirtualAlloc((LPVOID)dwParams[0], dwParams[1], dwParams[2], dwParams[3]);
        }
        else
        {
            res = VirtualFree((LPVOID)dwParams[0], dwParams[1], dwParams[2]);
        }
        DWORD *dwRetval = (DWORD *)(szProcMsg + 1);
        dwRetval[0] = res;
    }
    *szProcMsg = VerbDone;
    nRetval = 1 + sizeof(DWORD);
    break;
    case TranslateStackIndex:
    {
        // writepipe an integer count, followed by pairs of index, realaddress
        DWORD dwParam = (*(DWORD *)(szProcMsg + 1));
        auto nCount = 1;
        if (dwParam == 0)  // we want all so far
        {
            nCount = Framewalker::g_StackFrameMapWrapper->m_pStlType->size();
        }
        WriteFile(m_hPipeFromChild, &nCount, 4 * 1, &nBytesWritten, 0);
        if (dwParam == 0)
        {
            // we need to lock the dict or copy it out first
            int nCounter = 0;

            for_each(
                Framewalker::g_StackFrameMapWrapper->m_pStlType->begin(),
                Framewalker::g_StackFrameMapWrapper->m_pStlType->end(),
                [this, &nBytesWritten, &nCounter, nCount](Framewalker::StackFrameMap::reference  it) {
                DWORD temp[2];
                temp[0] = (DWORD)it.first; // addr
                temp[1] = it.second;  // index
                WriteFile(m_hPipeFromChild, &temp, 4 * 2, &nBytesWritten, 0);
                nCounter++;
                if (nCounter == nCount) // can happen if more items are entered into map while we're looping
                {
                    return; // safe to not return all items, but we must honor count already sent down pipe
                }
            }
            );
            //                    VSASSERT(nCounter == nCount,"mismatch on frames sent items retrieved");
        }
        else
        {
            // we only want 1: the one in dwParam
            pair<LPVOID, StackFrameIndex> res = Framewalker::GetRealAddressFromStackFrameIndex(dwParam);
            DWORD temp[2];
            temp[0] = (DWORD)res.first; // addr
            temp[1] = res.second;  // index
            WriteFile(m_hPipeFromChild, &temp, 4 * 2, &nBytesWritten, 0);
        }

    }
    *szProcMsg = VerbDone;
    nRetval = 1;
    break;
    case GetCodeMarkerName:
    {
        DWORD dwParam = (*(DWORD *)(szProcMsg + 1));
        const char * pMarkerName;
        GetCodeMarkerNameFromId(dwParam, &pMarkerName);

        sprintf_s((char *)m_pvMappedSection, m_cbSharedMem, pMarkerName);
    }
    *szProcMsg = VerbDone;
    nRetval = 1;
    break;
    case GetCodeMarkers: //, // 2 params: Lo,Hi seqno. If both 0, gets all
    {
        DWORD dwParamLo = (*(DWORD *)(szProcMsg + 1));
        DWORD dwParamHi = (*(DWORD *)(szProcMsg + 5));
        auto thelist = g_TrackMemClass->GetList();
        CHeapSpy *pHeapSpy = g_TrackMemClass->m_MemSpectHeap;
        auto nodeStart = thelist->lower_bound(TreeKey(bt_CodeMarker, (LPVOID)0));
        auto nodeEnd = thelist->upper_bound(TreeKey(bt_CodeMarker, (LPVOID)0xffffffff)); //find last in range
        vector<CAddressNode *> pResults;
        for (; nodeStart != nodeEnd; nodeStart++)
        {
            TrkBlock *pCodeMarkerTrk = &nodeStart->second;
            PVOID ptrAlloc = ((DWORD *)pCodeMarkerTrk) - Offset_AllocToTblk; // ptr to raw allocation
            CAddressNode *pAddrNode = pHeapSpy->FindInst(ptrAlloc);
            if (pAddrNode->m_cAlloc >= dwParamLo)
            {
                if (dwParamHi == 0 || pAddrNode->m_cAlloc <= dwParamHi)
                {
                    pResults.push_back(pAddrNode);
                }
            }
        }
        DWORD nItems = pResults.size();
        WriteFile(m_hPipeFromChild, &nItems, 4, &nBytesWritten, 0);
        if (nItems)
        {
            WriteFile(m_hPipeFromChild, &pResults.at(0), 4 * nItems, &nBytesWritten, 0);
        }
    }
    *szProcMsg = VerbDone;
    nRetval = 1;
    break;
    case GotCustomCodeMarker:
    {
        //*
        //VSASSERTF((false,"dddebuga"));
        DWORD dwEventType = (*(DWORD *)(szProcMsg + 1));
        DWORD dwDepth = (*(DWORD *)(szProcMsg + 5));
        DWORD dwMarkerId = (*(DWORD *)(szProcMsg + 9));
        DWORD dwstrLen = (*(DWORD *)(szProcMsg + 13));
        DWORD *pszmarkerName = (DWORD *)(szProcMsg + 17);
        WCHAR *wszMarkerName = (WCHAR *)DebugAlloc(dwstrLen * 2 + 2);
        for (DWORD i = 0; i < dwstrLen; i++)
        {
            wszMarkerName[i] = (WCHAR)(pszmarkerName[i]);
        }
        wszMarkerName[dwstrLen] = 0; //nullterm

        //Iteration1/OpenSolution/TestCustomCodeMarkerVSASSERTF((false,"dddd %d %d %d %d %S", dwEventType, dwDepth, dwMarkerId, dwstrLen, wszMarkerName));
        CustomCodeMarker(wszMarkerName, dwEventType, dwDepth, dwMarkerId, 0, 0);
        /*/

        DWORD dwEventType = (* (DWORD *)(szProcMsg+1));
        DWORD dwDepth = (* (DWORD *)(szProcMsg+5));
        DWORD dwMarkerId = (* (DWORD *)(szProcMsg+9));
        DWORD dwstrLen = (* (DWORD *)(szProcMsg+13));
        WCHAR *pszMarkername =(WCHAR *) m_pvMappedSection;
        pszMarkername[dwstrLen] = 0;

        GotCodeMarker(pszMarkername, dwEventType, dwDepth, dwMarkerId,0,0);
        //*/
    }
    *szProcMsg = VerbDone;
    nRetval = 1;
    break;
    case SetCodeMarkerAction: // p0 = action, p1 = # of markers , p2...pN = markerids. If P1=0, means clear all actions
    {
        CodeMarkerActionEnum markerAction = (CodeMarkerActionEnum)(*(DWORD *)(szProcMsg + 1));
        DWORD dwParam1 = (*(DWORD *)(szProcMsg + 5));
        if (dwParam1 == 0) // we want to clear the marker(s) we're waiting for
        {
            if (g_CodeMarkerActions != nullptr)
            {
                g_CodeMarkerActions->freemem();
                g_CodeMarkerActions = nullptr;
                //for_each(
                //    g_CodeMarkerActions->m_pStlType->begin() ,
                //    g_CodeMarkerActions->m_pStlType->end() ,
                //    [](CodeMarkerActionSet::reference it)
                //{
                //    it.second.action &= ~CodeMarkerAction_Freeze;
                //});
            }
        }
        else
        {
            if (g_CodeMarkerActions == nullptr)
            {
                g_CodeMarkerActions = new (DebugAlloc(sizeof(CodeMarkerActionSetWrapper))) CodeMarkerActionSetWrapper(MySTLAlloc<CodeMarkerActionSetWrapper>(InternalHeapToUse));
            }
            for (DWORD i = 0; i < dwParam1; i++)
            {
                DWORD markerId = (*(DWORD *)(szProcMsg + 5 + 4 + i * 4));
                if (markerId != 0)
                {
                    auto res = g_CodeMarkerActions->m_pStlType->find(markerId);
                    if (res == g_CodeMarkerActions->m_pStlType->end())
                    {
                        g_CodeMarkerActions->m_pStlType->insert(
                            pair<DWORD, CodeMarkerAction>(
                                markerId,
                                CodeMarkerAction(markerAction))
                        );
                    }
                    else
                    {
                        res->second.action = markerAction;
                    }
                }
            }
        }
    }
    *szProcMsg = VerbDone;
    nRetval = 1;
    break;

    case AssertStackFrame:
    case AssertSeqNo:// P0: 0 add p1 as SeqNo to assert on. P0:1 remove p1. P0:2 return all current. Param0:2 means clear all
    {
        UINTSetWrapper **ppSet;
        if (verb == AssertSeqNo)
        {
            ppSet = &CHeapSpy::g_AssertOnSeqNo;
        }
        else {
            ppSet = &CHeapSpy::g_AssertOnStackFrame;
        }

        DWORD dwParam0 = (*(DWORD *)(szProcMsg + 1));
        DWORD dwParam1 = (*(DWORD *)(szProcMsg + 5));
        switch (dwParam0)
        {
        case 0: // add param 1 
            if ((*ppSet) == 0)
            {
                (*ppSet) = new (DebugAlloc(sizeof(UINTSetWrapper))) UINTSetWrapper(MySTLAlloc<UINT>(InternalHeapToUse));
            }
            (*ppSet)->m_pStlType->insert(dwParam1); // will not insert dupes
            break;
        case 1: // remove param 1
            if ((*ppSet) != 0)
            {
                (*ppSet)->m_pStlType->erase(dwParam1);
            }
            break;
        case 2: // return all current
            if ((*ppSet))
            {
                auto nData = (*ppSet)->m_pStlType->size();
                WriteFile(m_hPipeFromChild, &nData, 4, &nBytesWritten, 0);
                for (auto iter = (*ppSet)->m_pStlType->begin();
                    iter != (*ppSet)->m_pStlType->end();
                    iter++)
                {
                    nData = *iter;
                    WriteFile(m_hPipeFromChild, &nData, 4, &nBytesWritten, 0);
                }
            }
            else
            {
                auto nData = 0; // send 0 as item count
                WriteFile(m_hPipeFromChild, &nData, 4, &nBytesWritten, 0);
            }
            break;
        case 3: // clearall
            if ((*ppSet))
            {
                (*ppSet)->freemem();
                (*ppSet) = 0;
            }
            break;
        }
    }
    *szProcMsg = VerbDone;
    nRetval = 1;
    break;
    case GetThreadInfo:  // p0: 0 means get all current threadIDs and the SeqNo when created
    {
        VSASSERT(g_pCDebFreezeAllThreads, "can't get threadinfo unless frozen");
        DWORD dwData[4];

        dwData[0] = g_pCDebFreezeAllThreads->m_nThreads + 1; //include current thread (MemSpect debug thread)
        WriteFile(m_hPipeFromChild, &dwData, 4, &nBytesWritten, 0); // send the count
        for (int i = 0; i < g_pCDebFreezeAllThreads->m_nThreads + 1; i++)
        {
            DWORD dwStackBase = 0;
            DWORD dwStackLimit = 0;
            TEB *pteb;
            NT_TIB *ptib;
            if (i == g_pCDebFreezeAllThreads->m_nThreads) // last one
            {
                dwData[0] = GetCurrentThreadId();
                dwData[1] = 0;
                pteb = NtCurrentTeb();
                ptib = (NT_TIB *)pteb->Reserved1;
                dwStackBase = (DWORD)ptib->StackBase;
                dwStackLimit = (DWORD)ptib->StackLimit;
            }
            else
            {
                dwData[0] = g_pCDebFreezeAllThreads->m_rgThreads[i]->GetId();
                dwData[1] = g_pCDebFreezeAllThreads->m_rgThreads[i]->GetGlobalPassCount();
                THREAD_BASIC_INFORMATION basicInfo;
                if (g_PFNtQueryInformationThread)
                {
                    // Get TEB address
                    HANDLE hThread = g_pCDebFreezeAllThreads->m_rgThreads[i]->GetHandle();
                    VSASSERT(hThread != 0, "got null hThread in GetThreadInfo");
                    if (g_PFNtQueryInformationThread(hThread,
                        ThreadBasicInformation, &basicInfo, sizeof(THREAD_BASIC_INFORMATION), NULL) == 0)
                    {
                        pteb = (TEB *)basicInfo.TebBaseAddress;
                        ptib = (NT_TIB *)pteb->Reserved1;
                        dwStackBase = (DWORD)ptib->StackBase;
                        dwStackLimit = (DWORD)ptib->StackLimit;
                    }
                }
            }
            dwData[2] = dwStackBase;
            dwData[3] = dwStackLimit;
            WriteFile(m_hPipeFromChild, &dwData, 4 * 4, &nBytesWritten, 0); // send 4: (Id,SeqNo, stackBase, stackLimit)
        }
    }
    *szProcMsg = VerbDone;
    nRetval = 1;
    break;
    case SetHeapName:
    {
        DWORD *dwParams = (DWORD *)(szProcMsg + 1);
        HANDLE hHeap = (HANDLE)dwParams[0];
        int nHeapNameLen = dwParams[1];
        CHeapSpy *pHeapSpy = CHeapSpy::IsValidHeap((CHeapSpy *)hHeap, /*fCheckHandle =*/false);
        if (pHeapSpy)
        {
            ((LPSTR)m_pvMappedSection)[nHeapNameLen] = 0; //nullterm
            pHeapSpy->m_pszHeapName = pHeapSpy->m_StringList.Add((LPCSTR)m_pvMappedSection);
            pHeapSpy->m_pszFile = "";
            pHeapSpy->m_uLine = 0;
        }
    }
    *szProcMsg = VerbDone;
    nRetval = 1;
    break;

    case GhostAllocSpec:
    {
        // setting them to 0 means turning logging off
        DWORD *dwParams = (DWORD *)(szProcMsg + 1);
        g_fTrackingGhosts = dwParams[0];
    }
    *szProcMsg = VerbDone;
    nRetval = 1;
    break;
    case MyGetModuleFileName:
    {
        // setting them to 0 means turning logging off
        DWORD *dwParams = (DWORD *)(szProcMsg + 1);
        HMODULE addr = (HMODULE)dwParams[0];
        if (GetModuleFileName(addr, (LPSTR)m_pvMappedSection, m_cbSharedMem) == 0)
        {
            int n = GetLastError();

        }
    }
    *szProcMsg = VerbDone;
    nRetval = 1;

    break;
    case MyGetModuleHandle:
        VSASSERTF((false, "MyGetModuleHandle not implemented"));
        break;
    case FreeStackMem:
    {
        VSASSERT(g_pCDebFreezeAllThreads, "Trying to free stack mem heap when not frozen?");
        DWORD dwParam = (*(DWORD *)(szProcMsg + 1));
        ULONG nFramesFreed = 0;
        LockCritSect;
        auto lambdaFreeHeapStacks = [&](CHeapSpy *pHeapSpy) {
            CAddressNode *pAddressNode = pHeapSpy->EnumReset();
            for (UINT i = 0; i < pHeapSpy->m_cCurNumAllocs; i++, pAddressNode = pHeapSpy->EnumNext())
            {
                VSASSERT(pAddressNode, "pAddressNode null in FreeStackMem.");
                if (pAddressNode->m_uicStackAddr)
                {
                    DWORD dwSize = sizeof(CAddressNode);
                    if (g_StackStorageMode != InMemory)
                    {
                        dwSize += sizeof(DWORD);
                    }
                    CAddressNode *poNewNode = (CAddressNode *)DebugAlloc(dwSize);

                    memcpy(poNewNode, pAddressNode, dwSize);
                    nFramesFreed += poNewNode->m_uicStackAddr;
                    pHeapSpy->ReplaceInst(pAddressNode->m_pv, poNewNode);
                    poNewNode->m_uicStackAddr = 0; // truncate stack
                }
            }
        };
        CHeapSpy *pHeapSpy = (CHeapSpy *)dwParam;
        if (pHeapSpy != 0)
        {
            VSASSERTF((CHeapSpy::IsValidHeap(pHeapSpy) != 0, "invalid heap for FreeStackMem %x", dwParam));
            lambdaFreeHeapStacks(pHeapSpy);
        }
        else
        {
            for (pHeapSpy = _pHeapSpyList; pHeapSpy; pHeapSpy = pHeapSpy->m_pNextHeapSpy)
            {
                lambdaFreeHeapStacks(pHeapSpy);
            }
        }
        MemMap::CompactIt();
        DWORD *dwRetval = (DWORD *)(szProcMsg + 1);
        dwRetval[0] = nFramesFreed;
    }
    *szProcMsg = VerbDone;
    nRetval = 1 + sizeof(DWORD);

    break;
    case TrackingMode:
    {
        DWORD dwParam = (*(DWORD *)(szProcMsg + 1));
        LockCritSect;
        if (dwParam == -1)
        {
            // just return current value
        }
        else
        {
            if (g_TrackingMode != dwParam)
            {
                g_TrackingMode = (TrackingModeEnum)dwParam; // so we can set a bpt
            }
        }
        DWORD *dwRetval = (DWORD *)(szProcMsg + 1);
        dwRetval[0] = g_TrackingMode;
    }
    *szProcMsg = VerbDone;
    nRetval = 1 + sizeof(DWORD);
    break;
    case SuspendResumeImmersive:
    {
        DWORD dwParam = (*(DWORD *)(szProcMsg + 1));
    }
    *szProcMsg = VerbDone;
    nRetval = 1;
    break;
    case GetMemStats:
    {
        DWORD dwData[4];
        if (g_memStatsVec == nullptr)
        {
            dwData[0] = 0;
            WriteFile(m_hPipeFromChild, &dwData, 4, &nBytesWritten, 0); // send the count
        }
        else
        {
            dwData[0] = g_memStatsVec->m_pStlType->size();
            WriteFile(m_hPipeFromChild, &dwData, 4, &nBytesWritten, 0); // send the count
            for_each(
                g_memStatsVec->m_pStlType->begin(),
                g_memStatsVec->m_pStlType->end(),
                [&](vector < MemStats, MySTLAlloc<MemStats >>::reference  it) {
                WriteFile(m_hPipeFromChild, &it, sizeof(MemStats), &nBytesWritten, 0); // send the data
            }
            );
        }
    }
    *szProcMsg = VerbDone;
    nRetval = 1;
    break;
    case GetCodeMarkerActions:
    {
        DWORD dwData[4];
        if (g_CodeMarkerActions == nullptr)
        {
            dwData[0] = 0;
            WriteFile(m_hPipeFromChild, &dwData, 4, &nBytesWritten, 0); // send the count
        }
        else
        {
            dwData[0] = g_CodeMarkerActions->m_pStlType->size();
            WriteFile(m_hPipeFromChild, &dwData, 4, &nBytesWritten, 0); // send the count
            for_each(
                g_CodeMarkerActions->m_pStlType->begin(),
                g_CodeMarkerActions->m_pStlType->end(),
                [&](CodeMarkerActionSet::reference  it) {
                dwData[0] = it.first; // marker
                dwData[1] = (DWORD)it.second.action; // action
                WriteFile(m_hPipeFromChild, dwData, 8, &nBytesWritten, 0); // send the data
            }
            );
        }
    }
    *szProcMsg = VerbDone;
    nRetval = 1;
    break;
    case CompileAndExecuteFile:
    {
        char *filename = ((char *)(szProcMsg + 1));
        auto filenameLen = strlen(filename);

        char *memspectBaseDllFileName = ((char *)szProcMsg + 1 + filenameLen + 1); // skip nullterm too
        auto result = CompileAndExecute(std::string(filename), std::string(memspectBaseDllFileName));
        DWORD nBufLen = result.length();
        WriteFile(m_hPipeFromChild, &nBufLen, 4, &nBytesWritten, 0);

        WriteFile(m_hPipeFromChild, result.c_str(), nBufLen, &nBytesWritten, 0);
        *szProcMsg = VerbDone;
    }
    nRetval = 1;
    break;
    case Quit:
    {
        DWORD dwParam = (*(DWORD *)(szProcMsg + 1));
        auto fTerminateBoth = dwParam & 1; // false means leave parent (devenv) alive
        auto fIgnoreFreezeState = dwParam & 2;
        if (!fTerminateBoth)
        {
            if (!fIgnoreFreezeState)
            {
                if (g_nThreadFreeze)
                {
                    g_nThreadFreeze = 1; // force unfreeze
                    verb = ThreadsUnFreeze;
                    HandleMsgVerb(&verb, 0); // recur
                }
            }
        }
        else
        {
            TerminateProcess(GetCurrentProcess(), 0);
        }
    }
    nRetval = 0;
    break;
    case CrashTargetProcess:
    {
        DWORD *dwParams = (DWORD *)(szProcMsg + 1);
        auto subVerb = dwParams[0] & 0xff;
        bool fOnMainThread = (dwParams[0] & 0x100) ? true : false;
        if (fOnMainThread)
        {
            LockCritSect
                g_ExecuteOnMainThreadVerb = subVerb;
            g_ExecuteOnMainThreadP1 = dwParams[1];

        }
        else
        {
            ExecuteCrashCode(subVerb, dwParams[1]);
        }
    }
    break;
    case GetPEBaseAddressFromMem:
    {
        DWORD dwParam = (*(DWORD *)(szProcMsg + 1));
        DWORD peBaseAddress = 0;
        if (dwParam != 0)
        {
            HMODULE hm = (HMODULE)dwParam;
            char buff[_MAX_PATH];
            // we just want to verify there's a module there
            if (GetModuleFileName(hm, buff, sizeof(buff)) > 4) // returns length
            {
                _IMAGE_DOS_HEADER *pdosHeader = (_IMAGE_DOS_HEADER *)hm;
                if (pdosHeader->e_magic == IMAGE_DOS_SIGNATURE)
                {
                    IMAGE_NT_HEADERS *pNTHeader = (IMAGE_NT_HEADERS *)((DWORD)hm + pdosHeader->e_lfanew);
                    peBaseAddress = pNTHeader->OptionalHeader.ImageBase;
                }
            }

        }
        DWORD *dwRetval = (DWORD *)(szProcMsg + 1);
        dwRetval[0] = peBaseAddress;
        nRetval = 1 + sizeof(DWORD);
        *szProcMsg = VerbDone;
    }
    break;
    default:
        nRetval = 1;
        *szProcMsg = UnKnownVerb;
    }
    if (nRetval)
    {
        VSASSERT(nRetval < MaxMsgSize, "msg to return too big");
        nBytesWritten = 0;
        if (WriteFile(m_hPipeFromChild, szProcMsg, nRetval, &nBytesWritten, 0) == FALSE)
        {
            auto err = GetLastError();
            if (err != ERROR_NO_DATA) // The pipe is being closed. 
            {
                VSASSERTF((nRetval == nBytesWritten, "writepipe # bytes mismatch GetLastError=%d   File %s(%d)", GetLastError(), __FILE__, __LINE__));
            }
        }
    }
    return nRetval;
}




HMODULE WINAPI Detoured()
{
    // will get called during a detour attach 
    return 0;
}


NTSTATUS(WINAPI *Real_ZwAllocateVirtualMemory)(
    __in     HANDLE ProcessHandle,
    __inout  PVOID *BaseAddress,
    __in     ULONG_PTR ZeroBits,
    __inout  PSIZE_T RegionSize,
    __in     ULONG AllocationType,
    __in     ULONG Protect
    );

NTSTATUS(WINAPI *Real_ZwFreeVirtualMemory)(
    __in     HANDLE ProcessHandle,
    __inout  PVOID *BaseAddress,
    __inout  PSIZE_T RegionSize,
    __in     ULONG FreeType
    );



LPVOID(WINAPI * Real_VirtualAllocEx)(HANDLE hProcess,
    LPVOID lpAddress,
    SIZE_T dwSize,
    DWORD flAllocationType,
    DWORD flProtect)
    = VirtualAllocEx;

LPVOID(WINAPI * Real_VirtualAlloc)(
    LPVOID lpAddress,
    SIZE_T dwSize,
    DWORD flAllocationType,
    DWORD flProtect)
    = VirtualAlloc;


BOOL(WINAPI * Real_VirtualFreeEx)(HANDLE hProcess,
    LPVOID lpAddress,
    SIZE_T dwSize,
    DWORD dwFreeType)
    = VirtualFreeEx;

BOOL(WINAPI * Real_VirtualFree)(
    LPVOID lpAddress,
    SIZE_T dwSize,
    DWORD dwFreeType)
    = VirtualFree;


LPVOID(WINAPI *Real_MapViewOfFile)(
    __in  HANDLE hFileMappingObject,
    __in  DWORD dwDesiredAccess,
    __in  DWORD dwFileOffsetHigh,
    __in  DWORD dwFileOffsetLow,
    __in  SIZE_T dwNumberOfBytesToMap
    ) = MapViewOfFile;


LPVOID(WINAPI *Real_MapViewOfFileEx)(
    HANDLE hFileMappingObject,
    DWORD dwDesiredAccess,
    DWORD dwFileOffsetHigh,
    DWORD dwFileOffsetLow,
    SIZE_T dwNumberOfBytesToMap,
    LPVOID lpBaseAddress
    ) = MapViewOfFileEx;

BOOL(WINAPI *Real_UnmapViewOfFile)(
    LPCVOID lpBaseAddress
    ) = UnmapViewOfFile;


BOOL(WINAPI *Real_UnmapViewOfFileEx)(
    PVOID lpBaseAddress,
    ULONG unmapFlags
    ) = UnmapViewOfFileEx;


HANDLE(WINAPI * Real_HeapCreate)(
    DWORD flOptions,
    SIZE_T dwInitialSize,
    SIZE_T dwMaximumSize
    ) = HeapCreate;


BOOL(WINAPI * Real_HeapDestroy)(
    HANDLE hHeap
    ) = HeapDestroy;



//\\ddindex2\sources_tfs\Dev10_Main\toolsrc\devdiv\makesku\vslab\linkfile.inc\ntrtl.h

typedef NTSTATUS
(*PRTL_HEAP_COMMIT_ROUTINE)(
    IN PVOID Base,
    IN OUT PVOID *CommitAddress,
    IN OUT PULONG CommitSize
    );


typedef struct _RTL_HEAP_PARAMETERS {
    ULONG Length;
    ULONG SegmentReserve;
    ULONG SegmentCommit;
    ULONG DeCommitFreeBlockThreshold;
    ULONG DeCommitTotalFreeThreshold;
    ULONG MaximumAllocationSize;
    ULONG VirtualMemoryThreshold;
    ULONG InitialCommit;
    ULONG InitialReserve;
    PRTL_HEAP_COMMIT_ROUTINE CommitRoutine;
    ULONG Reserved[2];
} RTL_HEAP_PARAMETERS, *PRTL_HEAP_PARAMETERS;


PVOID(WINAPI * Real_RtlCreateHeap) (
    __in ULONG Flags,
    __in_opt PVOID HeapBase,
    __in_opt SIZE_T ReserveSize,
    __in_opt SIZE_T CommitSize,
    __in_opt PVOID Lock,
    __in_opt PRTL_HEAP_PARAMETERS Parameters
    ) = 0;

PVOID(WINAPI * Real_RtlDestroyHeap) (
    __in __post_invalid PVOID HeapHandle
    ) = 0;






LPVOID(WINAPI * Real_RtlAllocHeap)(
    HANDLE hHeap,
    DWORD dwFlags,
    SIZE_T dwBytes
    ) = 0;


BOOL(WINAPI * Real_RtlFreeHeap)(
    HANDLE hHeap,
    DWORD dwFlags,
    LPVOID lpMem
    ) = 0;


LPVOID(WINAPI *Real_HeapReAlloc)(
    HANDLE hHeap,
    DWORD dwFlags,
    LPVOID lpMem,
    SIZE_T dwBytes
    ) = HeapReAlloc;

BOOL(WINAPI *Real_HeapValidate)(
    HANDLE hHeap,
    DWORD dwFlags,
    LPCVOID lpMem
    ) = HeapValidate;

BOOL(WINAPI *Real_RtlValidateHeap)(
    HANDLE hHeap,
    DWORD dwFlags,
    LPCVOID lpMem
    ) = 0;


SIZE_T(WINAPI *Real_HeapSize)(
    __in  HANDLE hHeap,
    __in  DWORD dwFlags,
    __in  LPCVOID lpMem
    ) = HeapSize;


BOOL(WINAPI * Real_RtlLockHeap)(HANDLE hHeap)
= 0;

BOOL(WINAPI * Real_RtlUnlockHeap)(HANDLE hHeap)
= 0;

SIZE_T(WINAPI *Real_HeapCompact)(
    __in  HANDLE hHeap,
    __in  DWORD dwFlags
    ) = HeapCompact;

BOOL(WINAPI *Real_HeapQueryInformation)(
    __in_opt   HANDLE HeapHandle,
    __in       HEAP_INFORMATION_CLASS HeapInformationClass,
    __out      PVOID HeapInformation,
    __in       SIZE_T HeapInformationLength,
    __out_opt  PSIZE_T ReturnLength
    ) = HeapQueryInformation;

BOOL(WINAPI *Real_HeapSetInformation)(
    __in_opt   HANDLE HeapHandle,
    __in       HEAP_INFORMATION_CLASS HeapInformationClass,
    __in       PVOID HeapInformation,
    __in       SIZE_T HeapInformationLength
    ) = HeapSetInformation;

BOOL(WINAPI *Real_HeapWalk)(
    __in     HANDLE hHeap,
    __inout  LPPROCESS_HEAP_ENTRY lpEntry
    ) = HeapWalk;


HGLOBAL(WINAPI *Real_GlobalAlloc)(
    __in  UINT uFlags,
    __in  SIZE_T dwBytes
    ) = GlobalAlloc;

HLOCAL(WINAPI *Real_LocalAlloc)(
    __in  UINT uFlags,
    __in  SIZE_T dwBytes
    ) = LocalAlloc;

HGLOBAL(WINAPI *Real_GlobalFree)(
    __in  HGLOBAL hMem
    ) = GlobalFree;

HLOCAL(WINAPI *Real_LocalFree)(
    __in  HLOCAL  hMem
    ) = LocalFree;

SIZE_T(WINAPI *Real_LocalSize)(
    __in  HLOCAL  hMem
    ) = LocalSize;

HGLOBAL(WINAPI *Real_GlobalReAlloc)(
    __in  HGLOBAL hMem,
    __in  SIZE_T dwBytes,
    __in  UINT uFlags
    ) = GlobalReAlloc;


HLOCAL(WINAPI *Real_LocalReAlloc)(
    __in  HLOCAL  hMem,
    __in  SIZE_T dwBytes,
    __in  UINT uFlags
    ) = LocalReAlloc;



LONG(WINAPI *Real_RegOpenKeyEx)(
    __in        HKEY hKey,
    __in_opt    LPCTSTR lpSubKey,
    __reserved  DWORD ulOptions,
    __in        REGSAM samDesired,
    __out       PHKEY phkResult
    ) = RegOpenKeyEx;

LONG(WINAPI *Real_RegCreateKeyEx)(
    __in        HKEY hKey,
    __in        LPCTSTR lpSubKey,
    __reserved  DWORD Reserved,
    __in_opt    LPTSTR lpClass,
    __in        DWORD dwOptions,
    __in        REGSAM samDesired,
    __in_opt    LPSECURITY_ATTRIBUTES lpSecurityAttributes,
    __out       PHKEY phkResult,
    __out_opt   LPDWORD lpdwDisposition
    ) = RegCreateKeyEx;

LONG(WINAPI *Real_RegCloseKey)(
    __in  HKEY hKey
    ) = RegCloseKey;

typedef void(WINAPI *PFCODEMARKER)(int, const void *, unsigned long);

void(WINAPI *Real_PerfCodeMarker)(
    int nTimerID,
    _In_opt_bytecount_(cbUserData) const void *pUserData,
    unsigned long cbUserData
    );


DWORD(WINAPI *Real_TlsAlloc)(
    void
    ) = TlsAlloc;


BOOL(WINAPI *Real_TlsFree)(
    DWORD dwTlsIndex
    ) = TlsFree;

HANDLE(WINAPI *Real_CreateThread)(
    __in_opt   LPSECURITY_ATTRIBUTES lpThreadAttributes,
    __in       SIZE_T dwStackSize,
    __in       LPTHREAD_START_ROUTINE lpStartAddress,
    __in_opt   LPVOID lpParameter,
    __in       DWORD dwCreationFlags,
    __out_opt  LPDWORD lpThreadId
    ) = CreateThread;

BOOL(WINAPI *Real_CloseHandle)(
    __in  HANDLE hObject
    ) = CloseHandle;

NTSTATUS(WINAPI *Real_ZwClose)(
    __in  HANDLE hObject
    ) = 0;


ULONG(WINAPI *Real_TraceEvent)(
    __in TRACEHANDLE SessionHandle,
    __in PEVENT_TRACE_HEADER EventTrace
    ) = TraceEvent;

ULONG(WINAPI *Real_EventWrite)(
    _In_ REGHANDLE RegHandle,
    _In_ PCEVENT_DESCRIPTOR EventDescriptor,
    _In_ ULONG UserDataCount,
    _In_reads_opt_(UserDataCount) PEVENT_DATA_DESCRIPTOR UserData
    ) = EventWrite;

ULONG(WINAPI *Real_EventWriteString)(
    _In_ REGHANDLE RegHandle,
    _In_ UCHAR Level,
    _In_ ULONGLONG Keyword,
    _In_ PCWSTR String
    ) = EventWriteString;

#include "winternl.h"

// from sdk\inc\internal\ntmmapi.h
typedef enum _SECTION_INHERIT {
    ViewShare = 1,
    ViewUnmap = 2
} SECTION_INHERIT;



NTSTATUS(WINAPI *Real_ZwCreateFile)(
    __out     PHANDLE FileHandle,
    __in      ACCESS_MASK DesiredAccess,
    __in      POBJECT_ATTRIBUTES ObjectAttributes,
    __out     PIO_STATUS_BLOCK IoStatusBlock,
    __in_opt  PLARGE_INTEGER AllocationSize,
    __in      ULONG FileAttributes,
    __in      ULONG ShareAccess,
    __in      ULONG CreateDisposition,
    __in      ULONG CreateOptions,
    __in_opt  PVOID EaBuffer,
    __in      ULONG EaLength
    ) = 0;

NTSTATUS(WINAPI *Real_ZwOpenFile)(
    __out  PHANDLE FileHandle,
    __in   ACCESS_MASK DesiredAccess,
    __in   POBJECT_ATTRIBUTES ObjectAttributes,
    __out  PIO_STATUS_BLOCK IoStatusBlock,
    __in   ULONG ShareAccess,
    __in   ULONG OpenOptions
    ) = 0;

NTSTATUS(WINAPI *Real_ZwCreateSection)(
    __out     PHANDLE SectionHandle,
    __in      ACCESS_MASK DesiredAccess,
    __in_opt  POBJECT_ATTRIBUTES ObjectAttributes,
    __in_opt  PLARGE_INTEGER MaximumSize,
    __in      ULONG SectionPageProtection,
    __in      ULONG AllocationAttributes,
    __in_opt  HANDLE FileHandle
    ) = 0;


NTSTATUS(WINAPI *Real_ZwMapViewOfSection)(
    __in     HANDLE SectionHandle,
    __in     HANDLE ProcessHandle,
    __inout  PVOID *BaseAddress,
    __in     ULONG_PTR ZeroBits,
    __in     SIZE_T CommitSize,
    __inout  PLARGE_INTEGER SectionOffset,
    __inout  PSIZE_T ViewSize,
    __in     SECTION_INHERIT InheritDisposition,
    __in     ULONG AllocationType,
    __in     ULONG Win32Protect
    ) = 0;

NTSTATUS(WINAPI *Real_ZwOpenSection)(
    __out  PHANDLE SectionHandle,
    __in   ACCESS_MASK DesiredAccess,
    __in   POBJECT_ATTRIBUTES ObjectAttributes
    ) = 0;

NTSTATUS(WINAPI *Real_ZwUnmapViewOfSectionEx)(
    __in      HANDLE ProcessHandle,
    __in      PVOID BaseAddress,
    __in      ULONG unmapflags
    /*
    UnmapFlags - Supplies 0 or MEM_UNMAP_WITH_TRANSIENT_BOOST.

    MEM_UNMAP_WITH_TRANSIENT_BOOST should be used if the
    pages backing this view should be temporarily boosted
    (with automatic short term decay) because another thread
    will access them shortly.
    */
    ) = 0;


HGLOBAL(WINAPI *Real_LoadResource)(
    HMODULE hModule,
    HRSRC hResInfo
    ) = LoadResource;


//#define VMCOMMIT_OFFSET  1 // VirtualAlloc can come Reserve first, then Commit, but the same base address. We'll offset the Commit in this case

NTSTATUS WINAPI Mine_ZwAllocateVirtualMemory(
    __in     HANDLE ProcessHandle,
    __inout  PVOID *BaseAddress,
    __in     ULONG_PTR ZeroBits,
    __inout  PSIZE_T RegionSize,
    __in     ULONG AllocationType,
    __in     ULONG Protect
)
{
    NTSTATUS retval;
    auto UserRequestedBaseAddress = BaseAddress ? *BaseAddress : 0;
    if (g_VirtualAllocTopDown)
    {
        AllocationType |= MEM_TOP_DOWN;
    }

    if (CDisableTrace::CanDetour() && GetCurrentProcess() == ProcessHandle)  // F5 scenario allocs for diff process
    {
        CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
        LockCritSect;

        retval = Real_ZwAllocateVirtualMemory(
            ProcessHandle,
            BaseAddress,
            ZeroBits,
            RegionSize,
            AllocationType,
            Protect);
        if (retval == STATUS_SUCCESS)
        {

            {
                auto thelist = g_TrackMemClass->GetList();
                /*
                VirtualAlloc can be called with BaseAddress=0 (which means a new Alloc) or some existing address within the range of some prior VAlloc'd mem
                */

                PVOID AddrResult = *BaseAddress;
                TreeKey theKey(bt_VirtualAlloc, AddrResult);

                auto res = thelist->find(theKey);
                bool fIsCommitOfPriorVAlloc = false;
                if (res != thelist->end()) // if it exists already, check flags. Could be Commit of reserve. Else we'll delete info 
                {
                    if (res->second.VMAllocTrk.m_dwType == MEM_RESERVE &&
                        AllocationType == MEM_COMMIT)
                    {
                        fIsCommitOfPriorVAlloc = true;
                        res->second.VMAllocTrk.m_dwType |= MEM_COMMIT;
                    }
                    else
                    {
#if MSDEBUG
                        //                    VSASSERT(false,"VirtualAlloc already there");
#endif MSDEBUG
                        thelist->erase(res); // delete it: the callstack saved with it needs refreshing
                    }
                }
                if (!fIsCommitOfPriorVAlloc)
                {
                    // no exact match or exact match was deleted
                    // now see if there's any match in range
                    auto lowerVA = thelist->lower_bound(theKey);
                    auto upperVA = thelist->upper_bound(TreeKey(bt_VirtualAlloc, (void *)((DWORD)(AddrResult)+*RegionSize)));
                    while (lowerVA != upperVA && lowerVA->first.first == bt_VirtualAlloc)
                    {
                        lowerVA = thelist->erase(lowerVA);
                    }


                    TrkBlock  oVMAllocTrk(*RegionSize); // note: lpAddress is the resultof VirtualAlloc, not the param
                    oVMAllocTrk.VMAllocTrk.m_dwType = AllocationType;
                    //            oVMAllocTrk.VMAllocTrk.m_flProtect = Protect;
                    oVMAllocTrk.VMAllocTrk.m_reqAddress = UserRequestedBaseAddress; // the requested starting address

                    auto resInsert = g_TrackMemClass->InsertIntoList(PairAddrBlock(theKey, oVMAllocTrk),
#if MSDEBUG
                        /*nExtraFramesToSkip =*/0
#else
                        /*nExtraFramesToSkip =*/2
#endif MSDEBUG
                    );
                    VSASSERT(resInsert.second == true, "Failed to insert new block in ZwAllocateVirtualMemory");
                }
            }
        }
#if MSDEBUG
        if (*RegionSize >= 0x00160000 && *RegionSize < 0x02000000)
        {
            //        DebugBreak();
            //        VSASSERTF((false,"Got large VA %x %x", *RegionSize, *BaseAddress));

        }
#endif MSDEBUG
    }
    else
    {

#if MSDEBUG
        //VSASSERTF((false,"ZwAllocateVirtualMemory can't detour?"));   can't assert because initialization comes through here
#endif MSDEBUG
        retval = Real_ZwAllocateVirtualMemory(
            ProcessHandle,
            BaseAddress,
            ZeroBits,
            RegionSize,
            AllocationType,
            Protect);
    }
    return retval;
}

NTSTATUS WINAPI Mine_ZwFreeVirtualMemory(
    __in     HANDLE ProcessHandle,
    __inout  PVOID *BaseAddress,
    __inout  PSIZE_T RegionSize,
    __in     ULONG FreeType
)
{
    NTSTATUS retval = STATUS_SUCCESS;
    if ((CDisableTrace::CanDetour()) && GetCurrentProcess() == ProcessHandle)  // F5 scenario allocs for diff process
    {
        CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
        LockCritSect;

        SIZE_T nFreeSize = 0;

        retval = Real_ZwFreeVirtualMemory(
            ProcessHandle,
            BaseAddress,
            RegionSize,
            FreeType);
        /*
        the doc for ZwFreeVirtualMemory http://msdn.microsoft.com/en-us/library/ff566460(VS.85).aspx is wrong
        C:\Users\calvinh\AppData\Local\SourceServer\WIN_MINKERNEL\win7_gdr\minkernel\ntos\rtl\heap.c\1\heap.c
        RtlCreatHeap in Heap.c  has this code:
        ExtraSize = (SIZE_T)RtlpHeapGenerateRandomValue64() % HEAP_RANDOM_REBASE_VALUES;

        //
        //  Rebase within 1 MBytes range
        //

        ExtraSize *= HEAP_MM_GRANULARITY;
        TempRebaseSize = ExtraSize + ReserveSize;

        if (TempRebaseSize < ReserveSize) {

        //
        //  If any overflow occured above and we ended up with a smaller buffer
        //  just allocate space for that buffer
        //

        TempRebaseSize = ReserveSize;
        ExtraSize = 0;
        }

        Status = ZwAllocateVirtualMemory( NtCurrentProcess(),
        (PVOID *)&RebaseTempBuffer,
        0,
        &TempRebaseSize,
        MEM_RESERVE,
        RtlpGetHeapProtection(Flags) );

        if (!NT_SUCCESS( Status )) {

        return NULL;
        }

        Heap = RebaseTempBuffer;
        ReserveSize = TempRebaseSize;

        if (ExtraSize) {

        //
        //  Split the vad and release the lower memory
        //

        Status = RtlpHeapFreeVirtualMemory( NtCurrentProcess(),
        (PVOID *)&RebaseTempBuffer,
        &ExtraSize,
        MEM_RELEASE );



        note how it allocates then calls Free with the ExtraSize, resulting in that size being freed: so a block is allocated, but the top part is freed.

        VAone 0x05600000 0x00050000 0x00002000
        VAins 0x05600000 0x00050000
        VMFre 0x05600000 0x00040000
        VAone 0x05640000 0x00001000 0x00001000
        VAins 0x05640000 0x00001000

        ExtraSize = (SIZE_T)RtlpHeapGenerateRandomValue64() % HEAP_RANDOM_REBASE_VALUES;
        773D0401 E8 B7 FD FF FF       call        RtlpHeapGenerateRandomValue64 (773D01BDh)
        773D0406 83 E0 1F             and         eax,1Fh

        //
        //  Rebase within 1 MBytes range
        //

        ExtraSize *= HEAP_MM_GRANULARITY;
        773D0409 C1 E0 10             shl         eax,10h
        773D040C 89 45 DC             mov         dword ptr [ebp-24h],eax
        TempRebaseSize = ExtraSize + ReserveSize;
        773D040F 8B 4D 10             mov         ecx,dword ptr [ebp+10h]
        773D0412 03 C1                add         eax,ecx
        773D0414 89 45 D8             mov         dword ptr [ebp-28h],eax

        if (TempRebaseSize < ReserveSize) {
        773D0417 3B C1                cmp         eax,ecx
        773D0419 0F 82 B6 D1 03 00    jb          $LN87+2C5h (7740D5D5h)
        }

        Status = ZwAllocateVirtualMemory( NtCurrentProcess(),
        (PVOID *)&RebaseTempBuffer,
        0,
        &TempRebaseSize,
        MEM_RESERVE,
        RtlpGetHeapProtection(Flags) );
        773D041F 81 E3 00 00 04 00    and         ebx,40000h
        773D0425 F7 DB                neg         ebx
        773D0427 1B DB                sbb         ebx,ebx
        773D0429 83 E3 3C             and         ebx,3Ch
        773D042C 83 C3 04             add         ebx,4
        773D042F 53                   push        ebx
        773D0430 68 00 20 00 00       push        2000h
        773D0435 8D 45 D8             lea         eax,[ebp-28h]
        773D0438 50                   push        eax
        773D0439 57                   push        edi
        773D043A 8D 45 D0             lea         eax,[ebp-30h]
        773D043D 50                   push        eax
        773D043E 6A FF                push        0FFFFFFFFh
        773D0440 E8 0B F6 FD FF       call        _ZwAllocateVirtualMemory@24 (773AFA50h)

        if (!NT_SUCCESS( Status )) {
        773D0445 85 C0                test        eax,eax

        return NULL;
        773D0447 0F 8C 3F 80 00 00    jl          $LN87+38Eh (773D848Ch)
        }

        Heap = RebaseTempBuffer;
        773D044D 8B 45 D0             mov         eax,dword ptr [ebp-30h]
        773D0450 89 45 E4             mov         dword ptr [ebp-1Ch],eax
        ReserveSize = TempRebaseSize;
        773D0453 8B 45 D8             mov         eax,dword ptr [ebp-28h]
        773D0456 89 45 10             mov         dword ptr [ebp+10h],eax

        if (ExtraSize) {
        773D0459 39 7D DC             cmp         dword ptr [ebp-24h],edi
        773D045C 74 2B                je          $LN87+335h (773D0489h)

        //
        //  Split the vad and release the lower memory
        //

        Status = RtlpHeapFreeVirtualMemory( NtCurrentProcess(),
        (PVOID *)&RebaseTempBuffer,
        &ExtraSize,
        MEM_RELEASE );
        773D045E 68 00 80 00 00       push        8000h
        773D0463 8D 45 DC             lea         eax,[ebp-24h]
        773D0466 50                   push        eax
        773D0467 8D 45 D0             lea         eax,[ebp-30h]
        773D046A 50                   push        eax
        773D046B 6A FF                push        0FFFFFFFFh
        773D046D E8 0D 3D FF FF       call        RtlpSecMemFreeVirtualMemory (773C417Fh)




        */


        if (retval == STATUS_SUCCESS && FreeType == MEM_RELEASE) // if free succeeded and not decommit
        {
            {
                ListTrkBlocks *thelist = g_TrackMemClass->GetList();
                // now that we did the Real_VirtualFree we need to determine all in the list that are in the range freed
                MEMORY_BASIC_INFORMATION MBI;
                if (sizeof(MBI) == VirtualQueryEx(GetCurrentProcess(), *BaseAddress, &MBI, sizeof(MBI)))
                {
                    nFreeSize = MBI.RegionSize;
                }
#if MSDEBUG
                //                VSASSERTF((nFreeSize >= *RegionSize,"VirtualFree regionsize  != freesize?"));
                VSASSERTF((MBI.State == MEM_FREE, "VirtualFree didn't free?"));
                VSASSERTF((MBI.BaseAddress == *BaseAddress, "VirtualFree virtualquery didn't return right baseaddress"));
#endif MSDEBUG
                // now we know the range to be freed: from BaseAddress to BaseAddress + nFreeSize
                LPVOID pEndRange = (BYTE *)(*BaseAddress) + nFreeSize;

                // first we want to get the node to start erasing from
                auto nodeStart = thelist->lower_bound(TreeKey(bt_VirtualAlloc, *BaseAddress));
                if (nodeStart != thelist->end() && nodeStart->first.first == bt_VirtualAlloc)
                {
                    if (nodeStart->first.second == *BaseAddress) // exact match of region start
                    {
                        if (nodeStart->second.m_dwSize + (LPBYTE)(*BaseAddress) > pEndRange)
                        {
                            // orig VirtAlloc size is > freesize (like HeapCreate: VA 0x02920000 (sz=0x001f0000), VF 0x02920000(sz=0x001e0000), use last 0x10000) so we need to move node to diff address
                            //							VSASSERT(false,"ZwFreeVirtualMemory");
                            TrkBlock *pTrkBlock = &nodeStart->second;
                            TrkBlock oTrkBlkCopy = *pTrkBlock;
                            VSASSERTF((oTrkBlkCopy.m_dwSize > nFreeSize, "VirtualFree dwSize > freesize?"));
                            oTrkBlkCopy.m_dwSize -= nFreeSize;  // new node size is reduced
                            PVOID ptrAlloc = ((DWORD *)pTrkBlock) - Offset_AllocToTblk; // ptr to original raw allocation
                            CAddressNode *pAddrNode = g_TrackMemClass->m_MemSpectHeap->FindInst(ptrAlloc); // get the node info
                            auto nNodeSize = sizeof(CAddressNode) + pAddrNode->m_uicStackAddr * sizeof(DWORD); //prepare to copy
                            if (g_StackStorageMode != InMemory)
                            {
                                nNodeSize = sizeof(CAddressNode) + sizeof(DWORD);
                            }

                            CAddressNode *pnewNode = (CAddressNode *)DebugAlloc(nNodeSize); // this will replace the one in DebugHeap and be freed via DeleteInst
                            memcpy(pnewNode, pAddrNode, nNodeSize);

                            PVOID lpNewAddr = (BYTE *)(nodeStart->first.second) + nFreeSize; // the new addr of the valloc
                            auto resInsrt = g_TrackMemClass->InsertIntoList(PairAddrBlock(TreeKey(bt_VirtualAlloc, lpNewAddr), oTrkBlkCopy));
                            //// because we made a copy of the mapsize offset, we want to set stack size to 0 so it won't be added to freelist
                            pAddrNode->m_uicStackAddr = 0;

                            PVOID ptrNew = (DWORD *)(&resInsrt.first->second) - Offset_AllocToTblk; // ptr to new alloc
                            g_TrackMemClass->m_MemSpectHeap->ReplaceInst(ptrNew, pnewNode);

                            thelist->erase(nodeStart); // now we can erase original node

                            nodeStart = thelist->lower_bound(TreeKey(bt_VirtualAlloc, lpNewAddr)); // recalc nodeStart
                        }
                    }
                    while (nodeStart != thelist->end() && nodeStart->first.first == bt_VirtualAlloc)
                    {
                        if (nodeStart->first.second >= pEndRange)
                        {
                            break;
                        }
                        nodeStart = thelist->erase(nodeStart);
                    }
                }

                //auto fSomethingToDelete = true;
                //if (nodeStart->first.first != bt_VirtualAlloc) // if the one we're Freeing is the last in the set or not in it at all
                //{
                //    nodeStart-- ; // go back 1 to see if we fall in it's range
                //    if (nodeStart->first.first == bt_VirtualAlloc &&
                //        ((BYTE *)*BaseAddress) < (BYTE *)(nodeStart->first.second) + nodeStart->second.m_dwSize)
                //    {
                //        thelist->erase(nodeStart);

                //    }
                //    fSomethingToDelete = false;
                //}
                //// at this point, nodeStart == the alloc or 

                //if (fSomethingToDelete &&  nodeStart != thelist->end()) // if there is one >= lpAddress
                //{
                //    TrkBlock oVMAllocTrk = (*nodeStart).second;
                //    auto theKey = (*nodeStart).first;
                //    if (theKey.second == *BaseAddress) // if it's an exact match
                //    {
                //        // nodeStart is now set to the beginning of the range from which we'll start removing
                //    }
                //    else
                //    {
                //        // not a match: must be > lpAddress. Now see if there's any overlap
                //        if (((BYTE *)*BaseAddress) + nFreeSize >= theKey.second) // overlap, 
                //        {
                //            // we've determined that nodeStart doesn't match, but there's overlap with what we freed, so we still want to delete it
                //        }
                //        else
                //        {
                //            // no overlap, not found. 
                //            fSomethingToDelete = false;
                //        }
                //    }
                //    if (fSomethingToDelete )
                //    {
                //        // now we need to determine the end of the range to free
                //        // loop starting with next one
                //        nodeStart = thelist->erase(nodeStart); // erase 1st one
                //        for (auto nodeEnd = nodeStart  ; nodeEnd != thelist->end() ;  )
                //        {
                //            auto theKey = nodeEnd->first;
                //            if (theKey.first != bt_VirtualAlloc)
                //            {
                //                break;
                //            }
                //            nodeEnd = thelist->erase(nodeEnd);                                
                //            if (theKey.second >= pEndRange) // if it's out of the range
                //            {
                //                break;
                //            }
                //        }
                //    }
                //    
                //}
            }

        }
        else
        {
            // free failed
        }
    }
    else
    {
        //        VSASSERTF((false, " non-det VirtualFree %x %x %x ", BaseAddress, RegionSize, FreeType));
        retval = Real_ZwFreeVirtualMemory(
            ProcessHandle,
            BaseAddress,
            RegionSize,
            FreeType);

    }
    return retval;
}

void CurrentThreadIsBeingDetached()
{
    if (g_TrackMemClass) // are we the target process?
    {
        TEB *pteb;
        NT_TIB *ptib;

        pteb = NtCurrentTeb();
        ptib = (NT_TIB *)pteb->Reserved1;
        DWORD dwStackBase = (DWORD)ptib->StackBase;
        DWORD dwStackLimit = (DWORD)ptib->StackLimit;

        LockCritSect;
        auto thelist = g_TrackMemClass->GetList();
        CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
        // stacks are usually multiple MBIs
        // StackBase is highest addr of stack +1. VirtualQuery for (StackBase -1), the MBI.AllocationBase is the start of stack. Stackbase - MBI.AllocationBase = stacksize
        //
        MEMORY_BASIC_INFORMATION MBI;
        if (sizeof(MBI) == VirtualQueryEx(GetCurrentProcess(), (PVOID)(dwStackBase - 1), &MBI, sizeof(MBI)))
        {
            DWORD dwStackLow = (DWORD)MBI.AllocationBase;
            auto nodeStart = thelist->lower_bound(TreeKey(bt_VirtualAlloc, (PVOID)dwStackLow));
            while (nodeStart != thelist->end() && nodeStart->first.first == bt_VirtualAlloc)
            {
                TrkBlock *pVMAllocTrk = &nodeStart->second;
                if ((BYTE *)(nodeStart->first.second) >= (PVOID)dwStackBase)
                {
                    break;
                }
                nodeStart = thelist->erase(nodeStart);
            }

        }
    }
}


LPVOID WINAPI Mine_MapViewOfFileEx(
    HANDLE hFileMappingObject,
    DWORD dwDesiredAccess,
    DWORD dwFileOffsetHigh,
    DWORD dwFileOffsetLow,
    SIZE_T dwNumberOfBytesToMap,
    LPVOID lpBaseAddress
)
{
    LPVOID lpAddress;

    if (CDisableTrace::CanDetour())
    {
        lpAddress = Real_MapViewOfFileEx(hFileMappingObject, dwDesiredAccess, dwFileOffsetHigh, dwFileOffsetLow, dwNumberOfBytesToMap, lpBaseAddress);
        if (lpAddress)
        {
            CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
            LockCritSect;
            MEMORY_BASIC_INFORMATION MBI;
            SIZE_T dwSize = 0;
            if (sizeof(MBI) == VirtualQueryEx(GetCurrentProcess(), lpAddress, &MBI, sizeof(MBI)))
            {
                dwSize = MBI.RegionSize;
            }
            ListTrkBlocks *thelist = g_TrackMemClass->GetList();
            auto res = thelist->find(TreeKey(bt_MapViewOfFile, lpAddress));
            if (res != thelist->end()) // if it exists already, we'll delete info 
            {
                VSASSERT(false, "MapViewOfFileEx already there. This assert can be safely ignored");
#if MSDEBUG
#endif MSDEBUG
                thelist->erase(res); // delete it: the callstack saved with it needs refreshing
            }
            TrkBlock  oTrkBlock(dwSize);
            oTrkBlock.MapViewOfFileTrk.m_dwDesiredAccess = dwDesiredAccess;
            oTrkBlock.MapViewOfFileTrk.m_dwNumberOfBytesToMap = dwNumberOfBytesToMap;

            auto resPairIB = g_TrackMemClass->InsertIntoList(PairAddrBlock(TreeKey(bt_MapViewOfFile, lpAddress), oTrkBlock), /*nExtraFramesToSkip =*/1);
            VSASSERT(resPairIB.second == true, "failed to insert fo MapViewOfFileEx");
        }
    }
    else
    {
        if (g_StackStorageMode == InMemory)
        {
            VSASSERT(false, "Mine_MapViewOfFileEx can't detour");
        }
        lpAddress = Real_MapViewOfFileEx(hFileMappingObject, dwDesiredAccess, dwFileOffsetHigh, dwFileOffsetLow, dwNumberOfBytesToMap, lpBaseAddress);
    }
    return lpAddress;
}

LPVOID WINAPI Mine_MapViewOfFile(
    __in  HANDLE hFileMappingObject,
    __in  DWORD dwDesiredAccess,
    __in  DWORD dwFileOffsetHigh,
    __in  DWORD dwFileOffsetLow,
    __in  SIZE_T dwNumberOfBytesToMap
)
{
    LPVOID lpAddress = Mine_MapViewOfFileEx(hFileMappingObject, dwDesiredAccess, dwFileOffsetHigh, dwFileOffsetLow, dwNumberOfBytesToMap, /*lpBaseAddress*/ 0);
    return lpAddress;

}


BOOL WINAPI Mine_UnmapViewOfFileEx(
    PVOID lpBaseAddress,
    ULONG UnmapFlags
)
{
    BOOL fResult;
    if (CDisableTrace::CanDetour() && g_TrackMemClass) // not shutting down
    {
        CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
        LockCritSect;
        fResult = Real_UnmapViewOfFileEx(lpBaseAddress, UnmapFlags);
        if (fResult)
        {
            ListTrkBlocks *thelist = g_TrackMemClass->GetList();
            lpBaseAddress = (PVOID)(((DWORD)lpBaseAddress) & (~AllocationGranularity + 1));
            auto res = thelist->find(TreeKey(bt_MapViewOfFile, (LPVOID)lpBaseAddress));
            if (res != thelist->end()) // if it exists already, we'll delete info 
            {
                thelist->erase(res); // delete it: the callstack saved with it needs refreshing
            }
        }
    }
    else
    {
        fResult = Real_UnmapViewOfFileEx(lpBaseAddress, UnmapFlags);
    }
    return fResult;
}


// need both UnmapViewOfFile and UnmapViewOfFileEx because OS versions
BOOL WINAPI Mine_UnmapViewOfFile(
    PVOID lpBaseAddress
)
{
    return Mine_UnmapViewOfFileEx(lpBaseAddress, /*UnmapFlags=*/ 0);
}


//USE_RTL_CREATE_HEAP seems to trigger assert: repeated calls seem to return the same value, triggering assert
//#define USE_RTL_CREATE_HEAP 1 
#if USE_RTL_CREATE_HEAP

PVOID WINAPI  Mine_RtlCreateHeap(
    __in ULONG Flags,
    __in_opt PVOID HeapBase,
    __in_opt SIZE_T ReserveSize,
    __in_opt SIZE_T CommitSize,
    __in_opt PVOID Lock,
    __in_opt PRTL_HEAP_PARAMETERS Parameters
)
{
    PVOID retval;
    retval = Real_RtlCreateHeap(Flags, HeapBase, ReserveSize, CommitSize, Lock, Parameters);
    ULONG_PTR Fp; //frame ptr
    ULONG_PTR ReturnAddress;

    _asm mov Fp, EBP;
    HANDLE hHeapHandle;
    if (CDisableTrace::CanDetour()) // if not call from VsDebHeapCreate
    {
        // we need to lock before we call stackwalk, which locks s_mxsStackWalk to avoid deadlocks
        LockCritSect;
        hHeapHandle = Real_RtlCreateHeap(Flags, HeapBase, ReserveSize, CommitSize, Lock, Parameters);
        if (hHeapHandle)
        {
            //            Fp = *(ULONG *)Fp;
            Fp = *(ULONG *)Fp; // skip over HeapCreate call
            ReturnAddress = *((PULONG_PTR)(Fp + sizeof(ULONG_PTR)));

            char      szBuf[1024] = "_"; // all detoured heaps start with a single "_"

            scope
            {
                CDisableTrace lock; // don't detour when resolving symbols
                if (g_fHandle4gigStacks)
                { // store the stack index
                    Framewalker::StackFrameIndex ndx = Framewalker::GetOrCreateStackFrameIndexFromAddress((LPVOID)ReturnAddress, /*fIsManaged=*/false);
                    makehex((DWORD)ndx , szBuf + 1);
                }
                else
                {
                    makehex(ReturnAddress, szBuf + 1); // can't do any symbol res at all because recursive
                    //            GetStringFromAddr(ReturnAddress, szBuf+1, sizeof(szBuf)-1,/*fNoFileLineInfo=*/true);
                }
            }

            CHeapSpy *pHeapSpy = new CHeapSpy(hHeapHandle, szBuf, __FILE__, __LINE__);
            if (!pHeapSpy) // out of mem?
            {
                Real_RtlDestroyHeap(hHeapHandle);
                SetLastError(ERROR_OUTOFMEMORY);
                hHeapHandle = 0;
            }
            else
            {
                LockCritSect;
                ListTrkBlocks *thelist = g_TrackMemClass->GetList();
                pHeapSpy->m_nStackFramesToSkip = 1; // skip internal non-interesting stack frames
                CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
                auto res = thelist->find(TreeKey(bt_HeapCreate, hHeapHandle));
                if (res != thelist->end()) // if it exists already, we'll delete info 
                {
                    VSASSERT(false, "Mine_HeapCreate: why is heap already logged?");
                    thelist->erase(res); // delete it: the callstack saved with it needs refreshing
                }
                // we'll put the original real heap handle into the log
                TrkBlock oHeapBlk(ReserveSize);
                oHeapBlk.HeapCreateTrk.m_hHeap = hHeapHandle;
                oHeapBlk.HeapCreateTrk.m_pszHeapName = pHeapSpy->m_pszHeapName;
                //                oHeapBlk.HeapCreateTrk.m_flOptions= flOptions;
                //                oHeapBlk.HeapCreateTrk.m_dwInitialSize = dwInitialSize;
                //                oHeapBlk.HeapCreateTrk.m_dwMaximumSize = dwMaximumSize;

                auto resPairIB = g_TrackMemClass->InsertIntoList(PairAddrBlock(TreeKey(bt_HeapCreate, hHeapHandle), oHeapBlk), );
                VSASSERT(resPairIB.second == true, "failed to insert for heapCreate");
                hHeapHandle = (HANDLE)pHeapSpy;
            }
        }
    }
    else
    {
        hHeapHandle = Real_RtlCreateHeap(Flags, HeapBase, ReserveSize, CommitSize, Lock, Parameters);
    }
    retval = hHeapHandle;
    return retval;
}



PVOID WINAPI  Mine_RtlDestroyHeap(
    __in __post_invalid PVOID HeapHandle
)
{
    PVOID retval = 0;
    // regardless of CDisableTrace::CanDetour()
    CHeapSpy *pHeapSpy = CHeapSpy::IsValidHeap((CHeapSpy *)HeapHandle, /*fCheckHandle =*/true);
    if (pHeapSpy != 0 && HeapHandle == pHeapSpy->m_hHeap) // if we're being called on a real heap handle, rather than a wrapped one
    {
        pHeapSpy = NULL;// force going through Real_HeapDestroy
    }

    if (pHeapSpy) // is it one of ours?
    {
        VsDebugHeapDestroyInternal(pHeapSpy, /*Check for leaks*/ true);
    }
    else
    {
        retval = Real_RtlDestroyHeap(HeapHandle);
    }

    // regardless of which way we destroyed, it we need to remove it from the log
    if (retval)
    {
        {
            if (CDisableTrace::CanDetour()) // if we're not shutting down
            {
                LockCritSect;
                ListTrkBlocks *thelist = g_TrackMemClass->GetList();
                auto res = thelist->find(TreeKey(bt_HeapCreate, (LPVOID)HeapHandle));
                if (res != thelist->end()) // if it exists already, we'll delete info 
                {
                    CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
                    thelist->erase(res); // delete it: the callstack saved with it needs refreshing
                }
            }
        }
    }
    return retval;
}

#endif USE_RTL_CREATE_HEAP



CHeapSpy * CreateHeapSpyForNewHeap(HANDLE hHeapHandle, ULONG_PTR ReturnAddress, char prefix)
{
    CHeapSpy *pHeapSpy = 0;
    char      szBuf[1024]; // all detoured heaps start with a single "_"
    szBuf[0] = prefix;

    scope
    {
        CDisableTrace lock; // don't detour when resolving symbols
        if (g_fHandle4gigStacks)
        {
            // store the stack index
            StackFrameIndex ndx = Framewalker::GetOrCreateStackFrameIndexFromAddress((LPVOID)ReturnAddress, /*fIsManaged=*/false);
            makehex((DWORD)ndx, szBuf + 1);
        }
        else
        {
            // we store hex: child proc sym resolver may not be ready yet.
            makehex(ReturnAddress, szBuf + 1);

            ////            GetStringFromAddr(ReturnAddress, szBuf+1, sizeof(szBuf)-1,/*fNoFileLineInfo=*/true);
            //            char  parms[8];
            //            ((DWORD *)parms)[0] = 0; // no file/lineinfo
            //            ((DWORD *)parms)[1] = ReturnAddress ;
            //            
            //            int nLen = SendMsgToChild(ResolveSymbolFromTarg , sizeof(parms), parms, sizeof(szBuf)-1, szBuf+1); 
        }
    }

        /* CreateHeap calls AllocHeap internally: this results in CHeapSpy being created for heap, then CreateHeap tries to create
        it again for the same handle.
        */
    auto ptemp = CHeapSpy::IsValidHeap((CHeapSpy *)hHeapHandle, /*fCheckHandle*/true);
    if (ptemp == 0)
    {
        pHeapSpy = new CHeapSpy(hHeapHandle, szBuf, "", 0);
    }
    else
    {
        // already created. Let's use this instance and the new name (we don't want the name of the HeapAlloc from inside the CreateHeap)
        pHeapSpy = ptemp;
        pHeapSpy->m_pszHeapName = pHeapSpy->m_StringList.Add(szBuf);

    }
    if (false) //strstr(pHeapSpy->m_pszHeapName, "_heap_init"))
    {
        int nLen = sprintf_s(szBuf, sizeof(szBuf), "HeapCreate %s", pHeapSpy->m_pszHeapName);
        SendMsgToChild(UpdateStatusMessage, nLen, szBuf);
    }

    return pHeapSpy;
}

HANDLE WINAPI Mine_HeapCreate(
    DWORD flOptions,
    SIZE_T dwInitialSize,
    SIZE_T dwMaximumSize
)
{
    ULONG_PTR Fp; //frame ptr
    ULONG_PTR ReturnAddress;

    _asm mov Fp, EBP;
    HANDLE hHeapHandle;
    if (CDisableTrace::CanDetour()) // if not call from VsDebHeapCreate
    {
        // we need to lock before we call stackwalk, which locks s_mxsStackWalk to avoid deadlocks
        CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
        LockCritSect;
        hHeapHandle = Real_HeapCreate(flOptions, dwInitialSize, dwMaximumSize);
        if (hHeapHandle)
        {
            ReturnAddress = *((PULONG_PTR)(Fp + sizeof(ULONG_PTR)));

            CHeapSpy *pHeapSpy = CreateHeapSpyForNewHeap(hHeapHandle, ReturnAddress, '_');


            if (!pHeapSpy) // out of mem?
            {
                Real_HeapDestroy(hHeapHandle);
                SetLastError(ERROR_OUTOFMEMORY);
                hHeapHandle = 0;
            }
            else
            {
                ListTrkBlocks *thelist = g_TrackMemClass->GetList();
                pHeapSpy->m_nStackFramesToSkip = 1; // skip 1 level stack frames
                CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
                auto res = thelist->find(TreeKey(bt_HeapCreate, hHeapHandle));
                if (res != thelist->end()) // if it exists already, we'll delete info 
                {
                    VSASSERT(false, "Mine_HeapCreate: why is heap already logged?");
                    thelist->erase(res); // delete it: the callstack saved with it needs refreshing
                }
                // we'll put the original real heap handle into the log
                TrkBlock oHeapBlk(dwMaximumSize);
                oHeapBlk.HeapCreateTrk.m_hHeap = hHeapHandle;
                oHeapBlk.HeapCreateTrk.m_pszHeapName = pHeapSpy->m_pszHeapName;
                //                oHeapBlk.HeapCreateTrk.m_flOptions= flOptions;
                //                oHeapBlk.HeapCreateTrk.m_dwInitialSize = dwInitialSize;
                //                oHeapBlk.HeapCreateTrk.m_dwMaximumSize = dwMaximumSize;


                auto resPairIB = g_TrackMemClass->InsertIntoList(PairAddrBlock(TreeKey(bt_HeapCreate, hHeapHandle), oHeapBlk),
                    /*nExtraFramesToSkip =*/1
                );
                VSASSERT(resPairIB.second == true, "failed to insert for heapCreate");

                hHeapHandle = (HANDLE)pHeapSpy;
            }
        }
    }
    else
    {
        hHeapHandle = Real_HeapCreate(flOptions, dwInitialSize, dwMaximumSize);
    }
    return hHeapHandle;
}


BOOL WINAPI Mine_HeapDestroy(
    HANDLE hHeap
)
{
    BOOL fRetval = true;
    if (CDisableTrace::CanDetour())
    {
        CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
        CHeapSpy *pHeapSpy = CHeapSpy::IsValidHeap((CHeapSpy *)hHeap, /*fCheckHandle =*/true);
        if (pHeapSpy != 0 && hHeap == pHeapSpy->m_hHeap) // if we're being called on a real heap handle, rather than a wrapped one
        {
            // Heaps that were created before Memspect, but didn't alloc anything after, like WinSpool. 
            // also recursively
            //Also MemSpectHeap, CRT heap
            // we must remove it from our list
            if (!CHeapSpy::IsValidHeap((CHeapSpy *)hHeap,/*fCheckHandle=*/ false)) // use the real handle
            {
                delete pHeapSpy;
                //pHeapSpy->~CHeapSpy(); // must do it this way
                //DebugFree(pHeapSpy);
            }
            pHeapSpy = NULL;// force going through Real_HeapDestroy
        }
        if (pHeapSpy) // is it one of ours?
        {
            LockCritSect;
            VsDebugHeapDestroyInternal(pHeapSpy, /*Check for leaks*/ true); // this will cause recur to remove from theList.
        }
        else
        {
            fRetval = Real_HeapDestroy(hHeap);
        }
    }
    else
    {
        // recursive: just destroy
        fRetval = Real_HeapDestroy(hHeap);
    }
    if (fRetval) // if we destroyed the heap we need to remove from our list
    {
        LockCritSect;
        ListTrkBlocks *thelist = g_TrackMemClass->GetList();
        auto res = thelist->find(TreeKey(bt_HeapCreate, (LPVOID)hHeap));
        if (res != thelist->end()) // if it exists already, we'll delete info 
        {
            thelist->erase(res); // delete it
        }
    }
    return fRetval;
}


LPVOID WINAPI  Mine_RtlAllocHeap(
    HANDLE hHeap,
    DWORD dwFlags,
    SIZE_T dwBytes
)
{
    LPVOID lpMem;
    CHeapSpy *pHeapSpy = CHeapSpy::IsValidHeap((CHeapSpy *)hHeap, /*fCheckHandle =*/true); // must use the right heapspy even if TrackingMode=minimal
    bool fCanDetour = pHeapSpy && CDisableTrace::CanDetour();

    if (!pHeapSpy && hHeap != g_hDebugHeap)
    {
        // it's a heap we haven't seen before: created before we got into the process
        CDisableTrace lock; // don't detour when resolving symbols
        LockCritSect;

        ULONG_PTR Fp; //frame ptr
        ULONG_PTR ReturnAddress;
        _asm mov Fp, EBP;
        ReturnAddress = *((PULONG_PTR)(Fp + sizeof(ULONG_PTR)));

        pHeapSpy = CreateHeapSpyForNewHeap(hHeap, ReturnAddress, '~');

        if (!g_nUseChildProcessForSymbols)
        {
            pHeapSpy->m_uLine = ReturnAddress; // use line # for later sym resolution in CHeapSpy::UpdateRetailHeapNames
        }
        pHeapSpy->m_nStackFramesToSkip = 1;
        VSASSERT(pHeapSpy, "out of mem for HeapAlloc");
    }
    if (g_TrackMemClass && pHeapSpy == g_TrackMemClass->m_MemSpectHeap)
    {
        pHeapSpy = NULL; // if it's our memSpect heap: occurs when internal alloc for memspect debug heap growing
        VSASSERT(fCanDetour == false, "memspect heap: can't detour");
    }
    if (fCanDetour)
    {
        LockCritSect;   // this critsect must be around all allocs: an alloc can recur for internal heap structs
        if (g_ExecuteOnMainThreadVerb != DONOTHINGONMAINTHREAD && GetCurrentThreadId() == g_dwMainThread)
        {
            auto temp = g_ExecuteOnMainThreadVerb;
            g_ExecuteOnMainThreadVerb = DONOTHINGONMAINTHREAD;

            ExecuteCrashCode(temp, g_ExecuteOnMainThreadP1);
        }
        //  deadlock can occur if DllMain called for Thread Attach/Detach and they alloc
        if (pHeapSpy)// is it one of ours? If so we MUST use our version of Alloc, so it matches on free
        {
            lpMem = VsDebugAllocInternal(pHeapSpy, dwFlags, dwBytes);
        }
        else
        {
            lpMem = Real_RtlAllocHeap(hHeap, dwFlags, dwBytes);
        }
    }
    else
    {
        if (pHeapSpy)
        {
            lpMem = Real_RtlAllocHeap(pHeapSpy->m_hHeap, dwFlags, dwBytes);
        }
        else
        {
            lpMem = Real_RtlAllocHeap(hHeap, dwFlags, dwBytes);
        }
    }
    return lpMem;
}

BOOL WINAPI Mine_RtlFreeHeap(
    HANDLE hHeap,
    DWORD dwFlags,
    LPVOID lpMem
)
{
    BOOL fRetval = true;
    if (lpMem)
    {
        CHeapSpy *pHeapSpy = CHeapSpy::IsValidHeap((CHeapSpy *)hHeap, /*fCheckHandle =*/true);
        // regardless of CDisableTrace::CanDetour()
        LockCritSect; // this can cause recursion: freeing can cause CoalesceFreeBlocks which can call ZqAllocateVirtualMemory
        if (pHeapSpy) // is it one of ours?
        {
            VsDebugFreeInternalEx(pHeapSpy, lpMem);
        }
        else
        {
            scope
            {
                fRetval = Real_RtlFreeHeap(hHeap, dwFlags, lpMem);
            }
        }
    }

    return fRetval;
}


LPVOID WINAPI Mine_HeapReAlloc(
    HANDLE hHeap,
    DWORD dwFlags,
    LPVOID lpMem,
    SIZE_T dwBytes
)
{
    LPVOID lpNewAddress;
    CHeapSpy *pHeapSpy = CHeapSpy::IsValidHeap((CHeapSpy *)hHeap, /*fCheckHandle =*/true);
    bool fDidit = false;

    if (pHeapSpy)
    {
        if (CDisableTrace::CanDetour())
        {
            CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
            LockCritSect;
            // Perhaps the original Alloc that we're resizing happened before we came into existence.
            CAddressNode * pn = pHeapSpy->FindInst(lpMem);
            if (!pn)
            {
                VSASSERT(pHeapSpy->m_nHeaderSize == 0, "realloc inst not found with headersize");
                hHeap = pHeapSpy->m_hHeap; // use the original handle
                pHeapSpy = NULL;
            }
            else
            {
                if (hHeap == pHeapSpy->m_hHeap) // if we're being called on a real heap handle, rather than a wrapped one, we need to adjust by headersize
                {
                    lpMem = (BYTE *)lpMem + pHeapSpy->m_nHeaderSize;
                }

                lpNewAddress = VsDebugReallocInternal(pHeapSpy, lpMem, dwFlags, dwBytes);
                fDidit = true;
            }
        }
    }
    if (!fDidit)
    {
        LockCritSect;
        lpNewAddress = Real_HeapReAlloc(hHeap, dwFlags, lpMem, dwBytes);
    }

    return lpNewAddress;
}


BOOL WINAPI Mine_RtlValidateHeap(
    __in      HANDLE hHeap,
    __in      DWORD dwFlags,
    __in_opt  LPCVOID lpMem
)
{
    BOOL fRetval = true;
    CHeapSpy *pHeapSpy = CHeapSpy::IsValidHeap((CHeapSpy *)hHeap, /*fCheckHandle =*/true);
    // regardless of CDisableTrace::CanDetour()
    if (pHeapSpy) // is it one of ours?
    {
        pHeapSpy->HeapCheck();
        fRetval = true;
    }
    else
    {
        fRetval = Real_RtlValidateHeap(hHeap, dwFlags, lpMem);
    }
    return fRetval;
}


BOOL WINAPI Mine_HeapValidate(
    __in      HANDLE hHeap,
    __in      DWORD dwFlags,
    __in_opt  LPCVOID lpMem
)
{
    BOOL fRetval = true;
    CHeapSpy *pHeapSpy = CHeapSpy::IsValidHeap((CHeapSpy *)hHeap, /*fCheckHandle =*/true);
    // regardless of CDisableTrace::CanDetour()
    if (pHeapSpy) // is it one of ours?
    {
        pHeapSpy->HeapCheck();
    }
    else
    {
        fRetval = Real_HeapValidate(hHeap, dwFlags, lpMem);
    }
    return fRetval;
}

SIZE_T WINAPI Mine_HeapSize(
    __in  HANDLE hHeap,
    __in  DWORD dwFlags,
    __in  LPCVOID lpMem
)
{
    SIZE_T nRetval;
    CHeapSpy *pHeapSpy = CHeapSpy::IsValidHeap((CHeapSpy *)hHeap, /*fCheckHandle =*/false);// can't lock for real heap handle: deadlock
    // regardless of CDisableTrace::CanDetour()
    if (pHeapSpy) // is it one of ours?
    {
        LockCritSect;
        nRetval = VsDebugSizeInternal(pHeapSpy, (LPVOID)lpMem);
    }
    else
    {
        nRetval = Real_HeapSize(hHeap, dwFlags, lpMem);
    }
    return nRetval;
}

BOOL WINAPI Mine_RtlLockHeap(HANDLE hHeap)
{
    BOOL nRetval = 0;
    CHeapSpy *pHeapSpy = CHeapSpy::IsValidHeap((CHeapSpy *)hHeap, /*fCheckHandle =*/true);
    // regardless of CDisableTrace::CanDetour()
    // can't take critsect when user locks heap for a while
    auto pcs = s_mxsStackWalk.GetCriticalSectionDangerous();
    EnterCriticalSection(pcs);
    if (pHeapSpy) // is it one of ours?
    {
        if (!pHeapSpy->m_dwTidOfHeapLock)
        {
            pHeapSpy->m_dwTidOfHeapLock = GetCurrentThreadId();
            //            s_mxsStackWalk.Request();
        }
        nRetval = Real_RtlLockHeap(pHeapSpy->m_hHeap);
        if (!nRetval) //failed?
        {
            pHeapSpy->m_dwTidOfHeapLock = 0;
            //            s_mxsStackWalk.Release();
        }
    }
    else
    {
        nRetval = Real_RtlLockHeap(hHeap);
    }
    if (!nRetval)
    {
        LeaveCriticalSection(pcs);
    }
    return nRetval;
}

BOOL WINAPI Mine_RtlUnlockHeap(HANDLE hHeap)
{
    BOOL nRetval = 0;
    CHeapSpy *pHeapSpy = CHeapSpy::IsValidHeap((CHeapSpy *)hHeap, /*fCheckHandle =*/true);
    // regardless of CDisableTrace::CanDetour()
    auto pcs = s_mxsStackWalk.GetCriticalSectionDangerous();
    if (pHeapSpy) // is it one of ours?
    {
        nRetval = Real_RtlUnlockHeap(pHeapSpy->m_hHeap);
        if (nRetval) //success?
        {
            if (pHeapSpy->m_dwTidOfHeapLock)
            {
                pHeapSpy->m_dwTidOfHeapLock = 0;
                //                s_mxsStackWalk.Release();
            }
        }
    }
    else
    {
        nRetval = Real_RtlUnlockHeap(hHeap);
    }
    if (nRetval)
    {
        LeaveCriticalSection(pcs);
    }
    return nRetval;
}

SIZE_T WINAPI Mine_HeapCompact(
    __in  HANDLE hHeap,
    __in  DWORD dwFlags
)
{
    SIZE_T nRetval = 0;
    CHeapSpy *pHeapSpy = CHeapSpy::IsValidHeap((CHeapSpy *)hHeap, /*fCheckHandle =*/true);
    // regardless of CDisableTrace::CanDetour()
    if (pHeapSpy) // is it one of ours?
    {
        nRetval = Real_HeapCompact(pHeapSpy->m_hHeap, dwFlags);
    }
    else
    {
        nRetval = Real_HeapCompact(hHeap, dwFlags);
    }
    return nRetval;
}

BOOL WINAPI Mine_HeapQueryInformation(
    __in_opt   HANDLE hHeap,
    __in       HEAP_INFORMATION_CLASS HeapInformationClass,
    __out      PVOID HeapInformation,
    __in       SIZE_T HeapInformationLength,
    __out_opt  PSIZE_T ReturnLength
)
{
    BOOL nRetval = 0;
    CHeapSpy *pHeapSpy = CHeapSpy::IsValidHeap((CHeapSpy *)hHeap, /*fCheckHandle =*/true);
    // regardless of CDisableTrace::CanDetour()
    if (pHeapSpy) // is it one of ours?
    {
        nRetval = Real_HeapQueryInformation(pHeapSpy->m_hHeap, HeapInformationClass, HeapInformation, HeapInformationLength, ReturnLength);
    }
    else
    {
        nRetval = Real_HeapQueryInformation(hHeap, HeapInformationClass, HeapInformation, HeapInformationLength, ReturnLength);
    }
    return nRetval;
}

BOOL WINAPI Mine_HeapSetInformation(
    __in_opt   HANDLE hHeap,
    __in       HEAP_INFORMATION_CLASS HeapInformationClass,
    __in       PVOID HeapInformation,
    __in       SIZE_T HeapInformationLength
)
{
    BOOL nRetval = 0;
    CHeapSpy *pHeapSpy = CHeapSpy::IsValidHeap((CHeapSpy *)hHeap, /*fCheckHandle =*/true);
    // regardless of CDisableTrace::CanDetour()
    if (pHeapSpy) // is it one of ours?
    {
        nRetval = Real_HeapSetInformation(pHeapSpy->m_hHeap, HeapInformationClass, HeapInformation, HeapInformationLength);
    }
    else
    {
        nRetval = Real_HeapSetInformation(hHeap, HeapInformationClass, HeapInformation, HeapInformationLength);
    }
    return nRetval;
}

BOOL WINAPI Mine_HeapWalk(
    __in     HANDLE hHeap,
    __inout  LPPROCESS_HEAP_ENTRY lpEntry
)
{
    BOOL nRetval = 0;
    CHeapSpy *pHeapSpy = CHeapSpy::IsValidHeap((CHeapSpy *)hHeap, /*fCheckHandle =*/true);
    // regardless of CDisableTrace::CanDetour()
    if (pHeapSpy) // is it one of ours?
    {
        nRetval = Real_HeapWalk(pHeapSpy->m_hHeap, lpEntry);
    }
    else
    {
        nRetval = Real_HeapWalk(hHeap, lpEntry);
    }
    return nRetval;
}



HGLOBAL WINAPI Mine_GlobalAlloc(
    __in  UINT uFlags,
    __in  SIZE_T dwBytes
)
{
    LockCritSect;
    return Real_GlobalAlloc(uFlags, dwBytes);
}

HLOCAL WINAPI Mine_LocalAlloc(
    __in  UINT uFlags,
    __in  SIZE_T dwBytes
)
{
    LockCritSect;
    return Real_LocalAlloc(uFlags, dwBytes);
}

HGLOBAL WINAPI Mine_GlobalFree(
    __in  HGLOBAL hMem
)
{
    LockCritSect;
    return Real_GlobalFree(hMem);
}


HLOCAL WINAPI Mine_LocalFree(
    __in  HLOCAL  hMem
)
{
    LockCritSect;
    return Real_LocalFree(hMem);
}

SIZE_T WINAPI Mine_LocalSize(
    __in  HLOCAL  hMem
)
{
    SIZE_T retval;
    BOOL fDidit = false;
    LockCritSect;
    if (_pDefaultHeap->m_nHeaderSize)
    {
        CAddressNode *pAddrNode = _pDefaultHeap->FindInst((char *)hMem - _pDefaultHeap->m_nHeaderSize);
        if (pAddrNode)
        {
            retval = pAddrNode->m_cb;
            fDidit = true;
        }
        //            retval = Real_LocalSize((char *)hMem - _pDefaultHeap->m_nHeaderSize ) - (_pDefaultHeap->m_nHeaderSize + _pDefaultHeap->m_nTrailerSize);
    }
    if (!fDidit)
    {
        retval = Real_LocalSize(hMem);
    }
    return retval;
}


HGLOBAL WINAPI Mine_GlobalReAlloc(
    __in  HGLOBAL hMem,
    __in  SIZE_T dwBytes,
    __in  UINT uFlags
)
{
    LockCritSect;
    return Real_GlobalReAlloc(hMem, dwBytes, uFlags);
}


HLOCAL WINAPI Mine_LocalReAlloc(
    __in  HLOCAL  hMem,
    __in  SIZE_T dwBytes,
    __in  UINT uFlags
)
{
    LockCritSect;
    return Real_LocalReAlloc(hMem, dwBytes, uFlags);
}




LONG WINAPI Mine_RegOpenKeyEx(
    __in        HKEY hKey,
    __in_opt    LPCTSTR lpSubKey,
    __reserved  DWORD ulOptions,
    __in        REGSAM samDesired,
    __out       PHKEY phkResult
)
{
    LONG retval;
    retval = Real_RegOpenKeyEx(hKey, lpSubKey, ulOptions, samDesired, phkResult);
#if 1
    if (CDisableTrace::CanDetour() && retval == ERROR_SUCCESS)
    {
        if (hKey == HKEY_CLASSES_ROOT
            || hKey == HKEY_CURRENT_CONFIG
            || hKey == HKEY_CURRENT_USER
            || hKey == HKEY_LOCAL_MACHINE
            || hKey == HKEY_USERS)
        {
            // these don't have to be closed
        }
        else
        {
            LockCritSect;
            ListTrkBlocks *thelist = g_TrackMemClass->GetList();
            TreeKey theKey(bt_RegKey, (LPVOID)*phkResult);
            auto res = thelist->find(theKey);
            if (res != thelist->end()) // if it exists already, we'll delete info 
            {
                TrkBlock oTrkBlk = (*res).second;
                auto theKey2 = (*res).first;
                VSASSERT(false, "regkey result found again?");
                thelist->erase(res); // delete it
            }
            TrkBlock oTrkBlk(0);
            auto newBlk = PairAddrBlock(theKey, oTrkBlk);
            auto resPairIB = g_TrackMemClass->InsertIntoList(newBlk);
        }
    }
#endif

    return retval;
}

LONG WINAPI Mine_RegCreateKeyEx(
    __in        HKEY hKey,
    __in        LPCTSTR lpSubKey,
    __reserved  DWORD Reserved,
    __in_opt    LPTSTR lpClass,
    __in        DWORD dwOptions,
    __in        REGSAM samDesired,
    __in_opt    LPSECURITY_ATTRIBUTES lpSecurityAttributes,
    __out       PHKEY phkResult,
    __out_opt   LPDWORD lpdwDisposition
)
{
    LONG retval;
    retval = Real_RegCreateKeyEx(hKey, lpSubKey, Reserved, lpClass, dwOptions, samDesired, lpSecurityAttributes, phkResult, lpdwDisposition);
    if (CDisableTrace::CanDetour() && retval == ERROR_SUCCESS)
    {
        LockCritSect;
        ListTrkBlocks *thelist = g_TrackMemClass->GetList();
        TreeKey theKey(bt_RegKey, (LPVOID)*phkResult);
        auto res = thelist->find(theKey);
        if (res != thelist->end()) // if it exists already, we'll delete info 
        {
            TrkBlock oTrkBlk = (*res).second;
            auto theKey2 = (*res).first;
            VSASSERT(false, "regkey result found again?");
            thelist->erase(res); // delete it
        }
        TrkBlock oTrkBlk(0);
        auto newBlk = PairAddrBlock(theKey, oTrkBlk);
        auto resPairIB = g_TrackMemClass->InsertIntoList(newBlk);
    }
    return retval;
}

LONG WINAPI Mine_RegCloseKey(
    __in  HKEY hKey
)
{
    LONG retval;
    retval = Real_RegCloseKey(hKey);
    if (CDisableTrace::CanDetour() && retval == ERROR_SUCCESS)
    {
        LockCritSect;
        ListTrkBlocks *thelist = g_TrackMemClass->GetList();
        auto res = thelist->find(TreeKey(bt_RegKey, (LPVOID)hKey));
        if (res != thelist->end()) // does it exist?
        {
            thelist->erase(res);
        }
        else
        {
            VSASSERT(false, "regclosekey key not found");
        }
    }
    return retval;
}





/*
Shutdown seq:
marker 548  perfExitBegin
marker 18062
marker 7074
marker 7194
marker 9261
marker 9262
marker 7195
marker 18062
marker 7194
marker 9261
marker 9262
marker 7195
marker 7076
marker 7077
marker 7075
marker 7078
marker 7079
marker 549 perfExitEnd

{ 548, _T("perfExitBegin") },
{ 549, _T("perfExitEnd") },
{ 7078, _T("perfVSStopCLRBegin") },
{ 7079, _T("perfVSStopCLREnd") },
{ 7194, _T("perfVSGarbageCollectCLRBegin") },
{ 7195, _T("perfVSGarbageCollectCLREnd") },
*/
int g_CurrentCodeMarkerDepth = 0;

//so external processes that don't use markers can call
// nEventType = 0=none, 1=Start 2=End
// nDepthLevel (is the Markername nested?)
// markerId a real code marker id or a created one (anything above 64k)
CLINKAGE void ENTRYPOINT CustomCodeMarker(
    const WCHAR *MarkerName,
    int nEventType,
    int nDepthLevel,
    DWORD markerId,
    _In_opt_bytecount_(cbUserData) const void *pUserData,
    unsigned long cbUserData)
{
    static ULONG s_CodeMarkerInstance = 1;
    BOOL fFreeze = false;
    BOOL fSendMsg = false;
    //    char *p = GetCodeMarkerNameFromId(nTimerID);
    if (Real_PerfCodeMarker) // we could be running some other process that does not use markers, but that sends a marker to freeze.
    {
        const DWORD perfVSInputDelay = 9445;
        // we don't want to send input delays: the userdata size changed in Dev14 around Oct 7, 2014, but we want to be compatible with both sizes
        // So if we want to work in both Dev14 and Dev12, we don't send it. 
        //  	Microsoft.Internal.Performance.CodeMarkers.dll!MicrosoftVisualStudioPerfTrack::CheckPerfTrackScenario calls ValidatePointerSize which DebugBreaks if mismatch
        //       the code will DebugBreak (if debugger present) else throws, but swallows

        if (markerId != perfVSInputDelay)
        {
//            Real_PerfCodeMarker(markerId, pUserData, cbUserData);
        }
    }
    if (!g_fShuttingDown)
    {
        LockCritSect;
        CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
#define perfIdleMarkerId  502
        //#define perfVSCoEEShutdownCOMBegin 7076
        if (g_CodeMarkerInstanceMap == 0)
        {
            // map between a MarkerId and an Instance Count
            g_CodeMarkerInstanceMap = new (DebugAlloc(sizeof(MapDwordDwordWrapper))) MapDwordDwordWrapper(less<DWORD>(), MySTLAlloc<PairInt>(InternalHeapToUse));
        }
        int cbExtra = 0;
        DWORD codeMarkerInstanceNum = 1;
        // we want to calculate instance #
        auto codeMarkerInstanceRes = g_CodeMarkerInstanceMap->m_pStlType->find(markerId);
        if (codeMarkerInstanceRes == g_CodeMarkerInstanceMap->m_pStlType->end())
        {
            // the first instance is always = 1
            g_CodeMarkerInstanceMap->m_pStlType->insert(PairInt(markerId, codeMarkerInstanceNum));
        }
        else
        {
            codeMarkerInstanceNum = ++codeMarkerInstanceRes->second;
        }

        if (MarkerName == 0) // custom name is 0, so it's a normal codemarker
        {
            CodeMarkerStruct * cmarkerData = GetCodeMarkerStructFromId(markerId);
            if (cmarkerData)
            {
                if (cmarkerData->IsBegin) // if it's a begin marker, we want to track the nesting level so the End can match
                {
                    if (cmarkerData->markerStack == 0)
                    {
                        cmarkerData->markerStack =
                            new (DebugAlloc(sizeof(DWORDdeque))) DWORDdeque(MySTLAlloc<DWORD>(InternalHeapToUse));
                    }
                    cmarkerData->markerStack->m_pStlType->push_back(codeMarkerInstanceNum);
                }
                else
                {
                    if (cmarkerData->idMate) //if it is an End and has a mate, let's attempt to match to calc instance num
                    {
                        auto codeMarkerMatchingBegin = GetCodeMarkerStructFromId(cmarkerData->idMate);
                        if (codeMarkerMatchingBegin)
                        {
                            // perfVBCompilerBackgroundThreadStop has no matching start fired
                            //VSASSERTF((codeMarkerMatchingBegin->markerStack != 0,"no marker stack for end marker? %s", codeMarkerMatchingBegin->MarkerName));

                            //VSASSERTF((!codeMarkerMatchingBegin->markerStack->m_pStlType->empty() ,"marker stack for end marker empty?"));
                            if (codeMarkerMatchingBegin->markerStack && !codeMarkerMatchingBegin->markerStack->m_pStlType->empty())
                            {
                                codeMarkerInstanceNum = codeMarkerMatchingBegin->markerStack->m_pStlType->back();
                                codeMarkerMatchingBegin->markerStack->m_pStlType->pop_back();
                                if (codeMarkerMatchingBegin->markerStack->m_pStlType->empty())
                                {
                                    codeMarkerMatchingBegin->markerStack->freemem();
                                    codeMarkerMatchingBegin->markerStack = NULL;
                                }
                            }
                        }
                    }
                }
                // now calc nesting depth
                VSASSERTF((nDepthLevel == 0, "expected DepthLevel==0 for codemarker"));
                nDepthLevel = g_CurrentCodeMarkerDepth;
                if (cmarkerData->IsBegin)
                {
                    g_CurrentCodeMarkerDepth++;
                }
                else
                {
                    if (cmarkerData->idMate) // the mate for a non-begin is an end
                    {
                        if (g_CurrentCodeMarkerDepth > 0)
                        {
                            g_CurrentCodeMarkerDepth--;
                            nDepthLevel = g_CurrentCodeMarkerDepth; // depth of End goes up 1
                        }
                        else
                        {
#if MSDEBUG
                            VSASSERTF((false, "code marker recursion level negative? %s", cmarkerData->MarkerName));
#endif MSDEBUG
                        }
                    }
                }
            }
        }
        else
        {
            cbExtra = 2 * wcslen(MarkerName);
        }
        ListTrkBlocks *thelist = g_TrackMemClass->GetList();
        auto key = TreeKey(bt_CodeMarker, (LPVOID)(s_CodeMarkerInstance++));

        TrkBlock oCodeMarkerBlk(0);
        oCodeMarkerBlk.CodeMarker.m_CodeMarkerId = markerId;

        auto markertype = IIType_CustomCodeMarker;
        if (pUserData && cbUserData)
        {
            markertype = IIType_CustomCodeMarkerWithUserData;
        }
        oCodeMarkerBlk.CodeMarker.m_pCIndirectInfoCustomCodeMarker = CIndirectInfo::CreateInstance(markertype, cbExtra);
        if (cbExtra)
        {
            memcpy(&oCodeMarkerBlk.CodeMarker.m_pCIndirectInfoCustomCodeMarker->m_pwChar,
                MarkerName,
                cbExtra);
        }
        oCodeMarkerBlk.CodeMarker.m_pCIndirectInfoCustomCodeMarker->IIType_CustomCodeMarker.nMarkerInstance = codeMarkerInstanceNum;
        oCodeMarkerBlk.CodeMarker.m_pCIndirectInfoCustomCodeMarker->IIType_CustomCodeMarker.nDepthLevel = nDepthLevel;
        oCodeMarkerBlk.CodeMarker.m_pCIndirectInfoCustomCodeMarker->IIType_CustomCodeMarker.nEventType = nEventType;
        if (markertype == IIType_CustomCodeMarkerWithUserData)
        {
            oCodeMarkerBlk.CodeMarker.m_pCIndirectInfoCustomCodeMarker->m_IndirectInfoType = IIType_CustomCodeMarkerWithUserData;
            auto tmp = DebugAlloc(cbUserData);
            memcpy(tmp, pUserData, cbUserData);
            oCodeMarkerBlk.CodeMarker.m_pCIndirectInfoCustomCodeMarker->IIType_CustomCodeMarkerWithUserData.pUserData = tmp;
            oCodeMarkerBlk.CodeMarker.m_pCIndirectInfoCustomCodeMarker->IIType_CustomCodeMarkerWithUserData.cbUserData = cbUserData;
        }

        auto resPairIB = g_TrackMemClass->InsertIntoList(PairAddrBlock(key, oCodeMarkerBlk), /*nExtraFramesToSkip =*/1);
        VSASSERT(resPairIB.second == true, "failed to insert for CodeMarker");
        DWORD dwAction = 0;
        if (g_CodeMarkerActions != nullptr) // && wcscmp(MarkerName,L"ETW") != 0)
        {
            auto res = g_CodeMarkerActions->m_pStlType->find(markerId);
            if (res != g_CodeMarkerActions->m_pStlType->end())
            {
                dwAction = res->second.action;
                if (dwAction  & CodeMarkerAction_TakeMemStatSnapShot)
                {
                    // user specified we want to take a snapshot of memstats at this codemarker
                    if (g_memStatsVec == nullptr)
                    {
                        g_memStatsVec = new (DebugAlloc(sizeof(MemStatWrapper))) MemStatWrapper(MySTLAlloc<MemStatWrapper>(InternalHeapToUse));
                    }
                    g_memStats.CodeMarkerId = markerId;
                    g_memStats.SeqNo = g_ulGlobalPassCount;
                    g_memStatsVec->m_pStlType->push_back(g_memStats);
                    fSendMsg = true;
                }
                if (dwAction  & (CodeMarkerAction_Freeze | CodeMarkerAction_TakeMegaSnapshot))
                {
                    fSendMsg = true;
                    fFreeze = true;
                }
                if (dwAction  & CodeMarkerAction_ShowInStatusMessage)
                {
                    fSendMsg = true;
                }
                if (dwAction & CodeMarkerAction_Crash)
                {
                    char *ptr = 0;
                    *ptr = 0;   // this AV may be handled, depending on the stack
                }
                if (dwAction & CodeMarkerAction_Hang)
                {
                    //	LockCritSect;
                    Sleep(g_CodeMarkerParameter);
                }
                if (dwAction & CodeMarkerAction_Sleep)
                {
                    Sleep(g_CodeMarkerParameter);
                }
                if (dwAction & CodeMarkerAction_DebugBreak)
                {
                    DebugBreak();
                }
                if (dwAction & CodeMarkerAction_Recur)
                {
                    RecurManyLevels(g_CodeMarkerParameter);
                }
            }
        }

        auto seqno = g_ulGlobalPassCount;
        if (markerId == perfIdleMarkerId)
        {
            g_SeqNoperfIdleCodeMarker = seqno; // record when VS goes idle
        }
        if (fSendMsg || markerId == perfIdleMarkerId)
        {
            DWORD parms[4];
            parms[0] = markerId;
            parms[1] = seqno;
            parms[2] = dwAction;
            parms[3] = codeMarkerInstanceNum;

            int res = SendMsgToChild(GotCodemarker, sizeof(parms), (char *)parms, 0, 0);
			if (res < 0)
			{
				parms[0] = markerId;
				auto x = g_TrackMemClass->m_CreateChildProcess.m_hPipeFromTarget;
			}
        }
    }
    if (fFreeze)
    {
        FreezeProcess(/*fCleanupRCW=*/false);
        // must be outside all csects
        Sleep(1500);/// allow enough time for child proc to send Freeze message. The freeze will occur during this sleep and suspend current thread
    }
}

// my detoured version of PerfCodeMarker
void WINAPI Mine_PerfCodeMarker(
    int nTimerID,
    _In_opt_bytecount_(cbUserData) const void *pUserData,
    unsigned long cbUserData
)
{
    if (CDisableTrace::CanDetour())
    {
        CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
        CustomCodeMarker(NULL, 0, 0, nTimerID, pUserData, cbUserData);
    }
    if (Real_PerfCodeMarker)
    {
        Real_PerfCodeMarker(nTimerID, pUserData, cbUserData);
    }
}


DWORD  WINAPI Mine_TlsAlloc(
    void
)
{
    DWORD dwTlsIndex = Real_TlsAlloc();
    if (dwTlsIndex != TLS_OUT_OF_INDEXES)
    {
        if (CDisableTrace::CanDetour())
        {
            LockCritSect;
            CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
            ListTrkBlocks *thelist = g_TrackMemClass->GetList();
            auto key = TreeKey(bt_TlsAllocFree, (LPVOID)(dwTlsIndex));

            TrkBlock oTlsBlk(/*size=*/4);
            oTlsBlk.TlsAllocFree.m_TlsIndex = dwTlsIndex;
            auto resPairIB = g_TrackMemClass->InsertIntoList(PairAddrBlock(key, oTlsBlk), /*nExtraFramesToSkip =*/1);
#if MSDEBUG
            VSASSERT(resPairIB.second == true, "failed to insert for TlsAlloc");
#endif MSDEBUG
        }
    }
    return dwTlsIndex;
}



BOOL WINAPI Mine_TlsFree(
    __in DWORD dwTlsIndex
)
{
    BOOL fRetval = Real_TlsFree(dwTlsIndex);
    if (fRetval != 0) //if success
    {
        if (CDisableTrace::CanDetour())
        {
            LockCritSect;
            CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
            ListTrkBlocks *thelist = g_TrackMemClass->GetList();
            auto key = TreeKey(bt_TlsAllocFree, (LPVOID)(dwTlsIndex));
            auto res = thelist->find(key);
            if (res != thelist->end()) // does it exist?
            {
                thelist->erase(res);
            }
            else
            {
                // it's ok not to find it: could have been created before we got here
                //                VSASSERT(false,"tlsfree key not found");
            }
        }
    }

    return fRetval;
}


HANDLE WINAPI Mine_CreateThread(
    __in_opt   LPSECURITY_ATTRIBUTES lpThreadAttributes,
    __in       SIZE_T dwStackSize,
    __in       LPTHREAD_START_ROUTINE lpStartAddress,
    __in_opt   LPVOID lpParameter,
    __in       DWORD dwCreationFlags,
    __out_opt  LPDWORD lpThreadId
)
{
    HANDLE hRetval;
    DWORD dwThreadId;
    if (lpThreadId == 0) // if caller doesn't care about theadid
    {
        lpThreadId = &dwThreadId;
    }

    hRetval = Real_CreateThread(lpThreadAttributes,
        dwStackSize,
        lpStartAddress,
        lpParameter,
        dwCreationFlags,
        lpThreadId);

    if (hRetval)
    {
        if (CDisableTrace::CanDetour())
        {
            LockCritSect;
            CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour

            ListTrkBlocks *thelist = g_TrackMemClass->GetList();
            auto key = TreeKey(bt_ThreadCreate, (LPVOID)(hRetval));

            auto res = thelist->find(key);
            if (res != thelist->end()) // does it exist?
            {
                thelist->erase(res); // erase it
            }

            TrkBlock oTlsBlk(/*size=*/4);
            oTlsBlk.ThreadCreate.m_dwNewThreadId = *lpThreadId;
            oTlsBlk.ThreadCreate.m_ThreadStartRoutine = lpStartAddress;

            auto resPairIB = g_TrackMemClass->InsertIntoList(PairAddrBlock(key, oTlsBlk), /*nExtraFramesToSkip =*/1);
            VSASSERT(resPairIB.second == true, "failed to insert for threadcreate");
        }
    }
    return hRetval;
}

BOOL WINAPI Mine_CloseHandle(
    __in  HANDLE hObject
)
{
    BOOL fRetval;
    fRetval = Real_CloseHandle(hObject);
    if (fRetval != 0) //if success
    {
        if (CDisableTrace::CanDetour())
        {
            LockCritSect;
            CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
            ListTrkBlocks *thelist = g_TrackMemClass->GetList();
            auto key = TreeKey(bt_ThreadCreate, (LPVOID)(hObject));
            auto res = thelist->find(key);
            if (res != thelist->end()) // does it exist?
            {
                thelist->erase(res);
            }
            else
            {
                // it's ok not to find it: could have been created before we got here
                //                VSASSERT(false,"tlsfree key not found");
            }
        }
    }

    return fRetval;
}



void CheckIfWeAssertOnThisGuy(TrkBlock &oTrkBlk)
{
    if (g_szDllsToAssertOn && g_szDllsToAssertOn[0])
    {
        if (oTrkBlk.IndirectInfo.pCIndirectInfo->m_nLen)
        {
            USES_CONVERSION;
            char *lowerfilename = W2A(oTrkBlk.IndirectInfo.pCIndirectInfo->m_pwChar); // it's nulltermed
            char * pBkSlash = strrchr(lowerfilename, '\\');
            char *justFilename = lowerfilename;
            if (pBkSlash)
            {
                justFilename = pBkSlash + 1;
            }
            if (*justFilename)
            {
                _strlwr(justFilename);
                char *ptr = strstr(g_szDllsToAssertOn, justFilename);
                if (ptr)
                {
                    if (ptr[-1] == ';' && ptr[strlen(justFilename)] == ';')
                    {
                        VSASSERTF((false, "DllsToAssertOn: %s %s", g_szDllsToAssertOn, lowerfilename));
                    }
                }
            }
        }

    }
}

static int g_LoaderNotifcationSerNo;
void NTAPI MyLdrNotificationCallback(
    __in      ULONG NotificationReason,
    __in      PCLDR_DLL_NOTIFICATION_DATA NotificationData,
    __in_opt  PVOID Context
)
{
    if (CDisableTrace::CanDetour())
    {
        LockCritSect;
        CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour

        if (Real_PerfCodeMarker == 0 &&
            !g_isImmersive &&
            (g_TrackCodeMarkers & TrackCodeMarkerMode_Normal) &&
            NotificationReason == LDR_DLL_NOTIFICATION_REASON_LOADED /*1. Unloaded == 2*/)
        {
            if (_wcsicmp(L"Microsoft.Internal.Performance.CodeMarkers.Dll", NotificationData->Loaded.BaseDllName->Buffer) == 0 ||
                _wcsicmp(L"Microsoft.VisualStudio.CodeMarkers.Dll", NotificationData->Loaded.BaseDllName->Buffer) == 0)
            {
                HMODULE hCodeMarkerInst = (HMODULE)NotificationData->Loaded.DllBase;
                Real_PerfCodeMarker = (PFCODEMARKER)GetProcAddress(hCodeMarkerInst, "_PerfCodeMarker@12");
                if (Real_PerfCodeMarker != 0)
                {
                    auto res = AddAtomW(L"VSCodeMarkersEnabled");
                    VSASSERTF((res != 0, "AddAtom failed for codemarkers Err = %d File %s(%d)", GetLastError(), __FILE__, __LINE__));
                    VSASSERT(Real_PerfCodeMarker, "Real_PerfCodeMarker _PerfCodeMarker@12 can't be found");
                    DetourTransactionBegin();

                    ATTACH(&(PVOID&)Real_PerfCodeMarker, Mine_PerfCodeMarker);
                    if (DetourTransactionCommit() != 0)
                    {
                        VSASSERTF((false, "DetourTransactionCommit failed for %S",NotificationData->Loaded.FullDllName ));
                    }
                }
                else
                {
                    g_TrackCodeMarkers &= ~TrackCodeMarkerMode_Normal;
                }

            }
        }


        ListTrkBlocks *thelist = g_TrackMemClass->GetList();
        g_LoaderNotifcationSerNo--; // count backward to get unique # and avoid collision with file/section handles
        auto key = TreeKey(bt_IndirectInfo, (LPVOID)(g_LoaderNotifcationSerNo));
        //            VSASSERT(false,"asdfasdfasdf");
        auto res = thelist->find(key);
        if (res != thelist->end()) // does it exist?
        {
            VSASSERT(false, "LdrNotification already exists?");
            thelist->erase(res);    //delete it
        }
        int cbExtra = NotificationReason == LDR_DLL_NOTIFICATION_REASON_LOADED ?
            NotificationData->Loaded.FullDllName->Length :
            NotificationData->Unloaded.FullDllName->Length;

        TrkBlock oTrkBlk(
            NotificationReason == LDR_DLL_NOTIFICATION_REASON_LOADED ? NotificationData->Loaded.SizeOfImage : NotificationData->Unloaded.SizeOfImage
        ); // 
        oTrkBlk.IndirectInfo.pCIndirectInfo = CIndirectInfo::CreateInstance(
            NotificationReason == LDR_DLL_NOTIFICATION_REASON_LOADED ? IIType_FileLoad : IIType_FileUnload,
            cbExtra);
        memcpy(&oTrkBlk.IndirectInfo.pCIndirectInfo->m_pwChar,
            NotificationReason == LDR_DLL_NOTIFICATION_REASON_LOADED ?
            NotificationData->Loaded.FullDllName->Buffer :
            NotificationData->Unloaded.FullDllName->Buffer,
            cbExtra);

        oTrkBlk.IndirectInfo.pCIndirectInfo->IIType_Notify.m_BaseAddress =
            NotificationReason == LDR_DLL_NOTIFICATION_REASON_LOADED ?
            NotificationData->Loaded.DllBase :
            NotificationData->Unloaded.DllBase;


        CheckIfWeAssertOnThisGuy(oTrkBlk);

        auto resPairIB = g_TrackMemClass->InsertIntoList(PairAddrBlock(key, oTrkBlk), /*nExtraFramesToSkip= */ 1);
        VSASSERT(resPairIB.second == true, "failed to insert for MyLdrNotificationCallback");
    }

}

NTSTATUS WINAPI Mine_ZwCreateFile(
    __out     PHANDLE FileHandle,
    __in      ACCESS_MASK DesiredAccess,
    __in      POBJECT_ATTRIBUTES ObjectAttributes,
    __out     PIO_STATUS_BLOCK IoStatusBlock,
    __in_opt  PLARGE_INTEGER AllocationSize,
    __in      ULONG FileAttributes,
    __in      ULONG ShareAccess,
    __in      ULONG CreateDisposition,
    __in      ULONG CreateOptions,
    __in_opt  PVOID EaBuffer,
    __in      ULONG EaLength
)
{
    NTSTATUS retval;

    retval = Real_ZwCreateFile(
        FileHandle,
        DesiredAccess,
        ObjectAttributes,
        IoStatusBlock,
        AllocationSize,
        FileAttributes,
        ShareAccess,
        CreateDisposition,
        CreateOptions,
        EaBuffer,
        EaLength
    );
    if (retval == STATUS_SUCCESS)
    {
        if (CDisableTrace::CanDetour())
        {
            LockCritSect;
            CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
            ListTrkBlocks *thelist = g_TrackMemClass->GetList();
            auto key = TreeKey(bt_IndirectInfo, (LPVOID)(*FileHandle));
            //            VSASSERT(false,"asdfasdfasdf");
            auto res = thelist->find(key);
            if (res != thelist->end()) // does it exist?
            {
                if (ObjectAttributes->ObjectName->Length != res->second.IndirectInfo.pCIndirectInfo->m_nLen)
                {
                    //                    VSASSERTF((false,"ZwCreateFile handle already exists %08x %S", *FileHandle, ObjectAttributes->ObjectName->Buffer));
                }
                thelist->erase(res);    //delete it
            }
            int cbExtra = (ObjectAttributes->ObjectName->Length);
            TrkBlock oTrkBlk(0);
            //oTrkBlk.IndirectInfo.pKey = *FileHandle;
            oTrkBlk.IndirectInfo.pCIndirectInfo = CIndirectInfo::CreateInstance(IIType_CreateFile, cbExtra);
            memcpy(&oTrkBlk.IndirectInfo.pCIndirectInfo->m_pwChar,
                ObjectAttributes->ObjectName->Buffer,
                cbExtra);

            CheckIfWeAssertOnThisGuy(oTrkBlk);

            auto resPairIB = g_TrackMemClass->InsertIntoList(PairAddrBlock(key, oTrkBlk), /*nExtraFramesToSkip =*/1);
            VSASSERT(resPairIB.second == true, "failed to insert for ZwCreateFile");
        }
    }
    return retval;
}

NTSTATUS WINAPI Mine_ZwOpenFile(
    __out  PHANDLE FileHandle,
    __in   ACCESS_MASK DesiredAccess,
    __in   POBJECT_ATTRIBUTES ObjectAttributes,
    __out  PIO_STATUS_BLOCK IoStatusBlock,
    __in   ULONG ShareAccess,
    __in   ULONG OpenOptions
)
{
    NTSTATUS retval;
    retval = Real_ZwOpenFile(FileHandle, DesiredAccess, ObjectAttributes, IoStatusBlock, ShareAccess, OpenOptions);
    if (retval == STATUS_SUCCESS)
    {
        if (CDisableTrace::CanDetour())
        {
            LockCritSect;
            CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
            ListTrkBlocks *thelist = g_TrackMemClass->GetList();
            auto key = TreeKey(bt_IndirectInfo, (LPVOID)(*FileHandle));

            auto res = thelist->find(key);
            if (res != thelist->end()) // does it exist?
            {
                if (ObjectAttributes->ObjectName->Length != res->second.IndirectInfo.pCIndirectInfo->m_nLen)
                {
                    //                    VSASSERTF((false,"ZwOpenFile handle already exists %08x %S", *FileHandle, ObjectAttributes->ObjectName->Buffer));
                }
                thelist->erase(res);    //delete it
            }
            int cbExtra = (ObjectAttributes->ObjectName->Length);
            TrkBlock oTrkBlk(0);
            //oTrkBlk.IndirectInfo.pKey = *FileHandle;
            oTrkBlk.IndirectInfo.pCIndirectInfo = CIndirectInfo::CreateInstance(IIType_OpenFile, cbExtra);
            memcpy(&oTrkBlk.IndirectInfo.pCIndirectInfo->m_pwChar,
                ObjectAttributes->ObjectName->Buffer,
                cbExtra);

            CheckIfWeAssertOnThisGuy(oTrkBlk);
            auto resPairIB = g_TrackMemClass->InsertIntoList(PairAddrBlock(key, oTrkBlk), /*nExtraFramesToSkip =*/1);
            VSASSERT(resPairIB.second == true, "failed to insert for ZwCreateFile");
        }
    }
    return retval;
}

NTSTATUS WINAPI Mine_ZwOpenSection(
    __out  PHANDLE SectionHandle,
    __in   ACCESS_MASK DesiredAccess,
    __in   POBJECT_ATTRIBUTES ObjectAttributes
)
{
    NTSTATUS retval;
    retval = Real_ZwOpenSection(SectionHandle, DesiredAccess, ObjectAttributes);
    if (retval == STATUS_SUCCESS)
    {
        if (CDisableTrace::CanDetour())
        {
            LockCritSect;
            CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
            ListTrkBlocks *thelist = g_TrackMemClass->GetList();
            auto key = TreeKey(bt_IndirectInfo, (LPVOID)(*SectionHandle));

            auto res = thelist->find(key);
            if (res != thelist->end()) // does it exist?
            {
                //                VSASSERTF((false,"OpenSection handle already exists %08x %S", *SectionHandle, ObjectAttributes->ObjectName->Buffer));
                thelist->erase(res);    //delete it
            }
            int cbExtra = (ObjectAttributes->ObjectName->Length);
            TrkBlock oTrkBlk(0);
            //oTrkBlk.IndirectInfo.pKey = *SectionHandle;
            oTrkBlk.IndirectInfo.pCIndirectInfo = CIndirectInfo::CreateInstance(IIType_OpenSection, cbExtra);
            memcpy(&oTrkBlk.IndirectInfo.pCIndirectInfo->m_pwChar,
                ObjectAttributes->ObjectName->Buffer,
                cbExtra);

            CheckIfWeAssertOnThisGuy(oTrkBlk);

            auto resPairIB = g_TrackMemClass->InsertIntoList(PairAddrBlock(key, oTrkBlk), /*nExtraFramesToSkip =*/1);
            VSASSERT(resPairIB.second == true, "failed to insert for ZwOpenSection");
        }
    }
    return retval;
}

NTSTATUS WINAPI Mine_ZwCreateSection(
    __out     PHANDLE SectionHandle,
    __in      ACCESS_MASK DesiredAccess,
    __in_opt  POBJECT_ATTRIBUTES ObjectAttributes,
    __in_opt  PLARGE_INTEGER MaximumSize,
    __in      ULONG SectionPageProtection,
    __in      ULONG AllocationAttributes,
    __in_opt  HANDLE FileHandle
)
{
    NTSTATUS retval;
    retval = Real_ZwCreateSection(SectionHandle, DesiredAccess, ObjectAttributes,
        MaximumSize, SectionPageProtection, AllocationAttributes, FileHandle);
    if (retval == STATUS_SUCCESS)
    {
        if (CDisableTrace::CanDetour())
        {
            LockCritSect;
            CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
            ListTrkBlocks *thelist = g_TrackMemClass->GetList();
            auto key = TreeKey(bt_IndirectInfo, (LPVOID)(*SectionHandle));

            auto res = thelist->find(key);
            if (res != thelist->end()) // does it exist?
            {
                //VSASSERTF((false,"CreateSection handle already exists %08x", *SectionHandle));
                thelist->erase(res);    //delete it
            }
            int cbExtra = 0;
            TrkBlock *pTrkBlkFileHandle = 0;
            if (ObjectAttributes && ObjectAttributes->ObjectName)
            {
                cbExtra = (ObjectAttributes->ObjectName->Length);
            }
            else
            {
                if (FileHandle)
                {
                    auto resFileHandle = thelist->find(TreeKey(bt_IndirectInfo, (LPVOID)(FileHandle)));
                    if (resFileHandle != thelist->end())
                    {
                        pTrkBlkFileHandle = &resFileHandle->second;
                        cbExtra = (pTrkBlkFileHandle->IndirectInfo.pCIndirectInfo->m_nLen);
                    }
                }
            }
            TrkBlock oTrkBlk(0);
            //oTrkBlk.IndirectInfo.pKey = *SectionHandle;
            oTrkBlk.IndirectInfo.pCIndirectInfo = CIndirectInfo::CreateInstance(IIType_CreateSection, cbExtra);
            if (cbExtra)
            {
                if (ObjectAttributes && ObjectAttributes->ObjectName)
                {
                    oTrkBlk.IndirectInfo.pCIndirectInfo->m_nLen = ObjectAttributes->ObjectName->Length;
                    memcpy(&oTrkBlk.IndirectInfo.pCIndirectInfo->m_pwChar,
                        ObjectAttributes->ObjectName->Buffer,
                        cbExtra);
                }
                else
                {
                    oTrkBlk.IndirectInfo.pCIndirectInfo->m_nLen = pTrkBlkFileHandle->IndirectInfo.pCIndirectInfo->m_nLen;
                    memcpy(&oTrkBlk.IndirectInfo.pCIndirectInfo->m_pwChar,
                        pTrkBlkFileHandle->IndirectInfo.pCIndirectInfo->m_pwChar,
                        cbExtra);

                }
                CheckIfWeAssertOnThisGuy(oTrkBlk);
            }

            auto resPairIB = g_TrackMemClass->InsertIntoList(PairAddrBlock(key, oTrkBlk), /*nExtraFramesToSkip =*/1);
            VSASSERT(resPairIB.second == true, "failed to insert for ZwCreateSection");
        }
    }
    return retval;
}
/*
void TryStorage()
{
::MessageBox(0, L"hi", L"", 0);
wchar_t *pszFileName = L"c:\\t.txt";
HRESULT hr;
for (int i = 0; i < 23; i++)
{
CComPtr<IStorage> m_pStorage;
//		DeleteFile(pszFileName);
hr = ::StgOpenStorage(pszFileName, NULL, STGM_TRANSACTED | STGM_READWRITE | STGM_SHARE_EXCLUSIVE, NULL, 0, &m_pStorage);

// if the docfile storage was corrupted, the open will fail, so create the storage from scratch so we can write.
if (FAILED(hr))
hr = ::StgCreateDocfile(pszFileName, STGM_TRANSACTED | STGM_READWRITE | STGM_SHARE_EXCLUSIVE | STGM_CREATE, 0, &m_pStorage);

CComPtr<IStream> pStream;
m_pStorage->CreateStream(L"FOO", STGM_DIRECT | STGM_WRITE | STGM_CREATE | STGM_SHARE_EXCLUSIVE, 0, 0, &pStream);
DWORD dwWritten;
wchar_t buff[1000];
swprintf_s(buff, L"Data %d", i);
pStream->Write(buff, wcslen(buff) * 2, &dwWritten);
pStream = nullptr;
m_pStorage->Commit(0);// STGC_DANGEROUSLYCOMMITMERELYTODISKCACHE);
m_pStorage = nullptr;

}
::MessageBox(0, L"done iter", L"", 0);
}


Mine_ZwCreateFile, 0x000000e8   0x01300618 L"\\??\\c:\\t.txt"
Mine_ZwCreateSection, 0x00f5e674 {0x00000140} FileHandle=0x000000e8
Mine_ZwMapViewOfSection erase (bt_IndirectInfo (15), 0x033b0000)
Mine_ZwMapViewOfSection (bt_IndirectInfo (15), 0x033b0000) SectHand= 0x00000140
Mine_ZwCreateFile, 0x00000168   0x0132c9b0 L"\\??\\C:\\Users\\calvinh\\AppData\\Local\\Temp\\~DF6C20F8E6AED22913.TMP"
Mine_ZwCreateSection, 0x00f5e34c {0x00000184} FileHandle=0x00000168
Mine_ZwMapViewOfSection erase (bt_IndirectInfo (15), 0x03430000)
Mine_ZwMapViewOfSection (bt_IndirectInfo (15), 0x03430000) SectHand= 0x00000184
Mine_ZwClose deleteing 0x00000184 0x010af2e0 {m_IndirectInfoType=IIType_CreateSection (3) IIType_File={m_Unused=0x00000000 } IIType_Section=...}
Mine_ZwClose deleteing 0x00000168 0x010aede0 {m_IndirectInfoType=IIType_CreateFile (2) IIType_File={m_Unused=0x00000000 } IIType_Section=...}
Mine_ZwClose deleteing 0x00000140 0x010ae3f0 {m_IndirectInfoType=IIType_CreateSection (3) IIType_File={m_Unused=0x00000000 } IIType_Section=...}
Mine_ZwClose deleteing 0x000000e8 0x010b33e8 {m_IndirectInfoType=IIType_CreateFile (2) IIType_File={m_Unused=0x00000000 } IIType_Section=...}

*/

NTSTATUS WINAPI Mine_ZwMapViewOfSection(
    __in     HANDLE SectionHandle,
    __in     HANDLE ProcessHandle,
    __inout  PVOID *BaseAddress,
    __in     ULONG_PTR ZeroBits,
    __in     SIZE_T CommitSize,
    __inout  PLARGE_INTEGER SectionOffset,
    __inout  PSIZE_T ViewSize,
    __in     SECTION_INHERIT InheritDisposition,
    __in     ULONG AllocationType,
    __in     ULONG Win32Protect
)
{
    NTSTATUS retval;
    retval = Real_ZwMapViewOfSection(SectionHandle, ProcessHandle, BaseAddress,
        ZeroBits, CommitSize, SectionOffset, ViewSize, InheritDisposition, AllocationType, Win32Protect);
    if (retval == STATUS_SUCCESS && ProcessHandle == GetCurrentProcess())
    {
        if (CDisableTrace::CanDetour())
        {
            LockCritSect;
            CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
            ListTrkBlocks *thelist = g_TrackMemClass->GetList();
            auto key = TreeKey(bt_IndirectInfo, (LPVOID)(*BaseAddress));

            auto res = thelist->find(key);
            if (res != thelist->end()) // does it exist?
            {
                //VSASSERTF((false, "mapViewOfSection already exists %08x", *BaseAddress));
                thelist->erase(res);    //delete it
            }
            // let's look up the section handle in the open sections, to find the filename
            auto keySectionHandle = TreeKey(bt_IndirectInfo, (LPVOID)(SectionHandle));
            res = thelist->find(keySectionHandle);
            TrkBlock *pTrkBlkSectionhandle = 0;
            auto cbExtra = 0;
            if (res != thelist->end())
            {
                pTrkBlkSectionhandle = &res->second;
                cbExtra = (pTrkBlkSectionhandle->IndirectInfo.pCIndirectInfo->m_nLen);
                // can't assert: UnMap doesn't pass section handle
                //				VSASSERTF((pTrkBlkSectionhandle->IndirectInfo.pCIndirectInfo->IIType_Section.m_BaseAddress == 0, "IndirectInfo baseaddress non-zero?"));
                pTrkBlkSectionhandle->IndirectInfo.pCIndirectInfo->IIType_Section.m_BaseAddress = *BaseAddress; // track base addr so when handle is closed, we can free this guy
            }
            TrkBlock oTrkBlk(*ViewSize);
            //            oTrkBlk.IndirectInfo.pKey = *BaseAddress;
            oTrkBlk.IndirectInfo.pCIndirectInfo = CIndirectInfo::CreateInstance(IIType_MapSection, cbExtra);
            oTrkBlk.IndirectInfo.pCIndirectInfo->IIType_MapViewOfSection.m_SectionHandle = SectionHandle;
            if (cbExtra)
            {
                oTrkBlk.IndirectInfo.pCIndirectInfo->m_nLen = cbExtra;
                memcpy(&oTrkBlk.IndirectInfo.pCIndirectInfo->m_pwChar,
                    pTrkBlkSectionhandle->IndirectInfo.pCIndirectInfo->m_pwChar,
                    cbExtra);
                CheckIfWeAssertOnThisGuy(oTrkBlk);
            }

            auto resPairIB = g_TrackMemClass->InsertIntoList(PairAddrBlock(key, oTrkBlk), /*nExtraFramesToSkip =*/2);
            VSASSERT(resPairIB.second == true, "failed to insert for ZwMapViewOfSection");
        }
    }
    return retval;
}

NTSTATUS WINAPI Mine_ZwUnmapViewOfSectionEx(
    __in      HANDLE ProcessHandle,
    __in      PVOID BaseAddress,
    __in      ULONG UnmapFlags
)
{
    NTSTATUS retval;
    retval = Real_ZwUnmapViewOfSectionEx(ProcessHandle, BaseAddress, UnmapFlags);
    if (retval == STATUS_SUCCESS)
    {
        if (CDisableTrace::CanDetour())
        {
            LockCritSect;
            CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
            ListTrkBlocks *thelist = g_TrackMemClass->GetList();
            VSASSERTF((BaseAddress != 0, "UnmapViewOfSection: base address=0"));
            auto key = TreeKey(bt_IndirectInfo, (LPVOID)(BaseAddress));
            auto res = thelist->find(key);
            if (res != thelist->end())
            {
                TrkBlock *pTrkBk = &res->second;

                CIndirectInfo *pCIndirectInfo = pTrkBk->IndirectInfo.pCIndirectInfo;

                delete pCIndirectInfo;
                thelist->erase(res);
            }
        }
    }
    return retval;
}

NTSTATUS WINAPI Mine_ZwClose(
    __in  HANDLE hObject
)
{
    NTSTATUS fRetval;
    fRetval = Real_ZwClose(hObject);
    if (fRetval == STATUS_SUCCESS) // success
    {
        if (CDisableTrace::CanDetour())
        {
            CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
            LockCritSect;
            ListTrkBlocks *thelist = g_TrackMemClass->GetList();
            auto key = TreeKey(bt_ThreadCreate, (LPVOID)(hObject)); // see if it's a close of a thread
            auto res = thelist->find(key);
            if (res != thelist->end()) // does it exist?
            {
                thelist->erase(res);
            }
            else
            {
                // it's ok not to find it: could have been created before we got here
                //                VSASSERT(false,"tlsfree key not found");
            }

            key = TreeKey(bt_IndirectInfo, (LPVOID)(hObject));
            res = thelist->find(key);
            if (res != thelist->end())
            {
                TrkBlock *pTrkBk = &res->second;

                CIndirectInfo *pCIndirectInfo = pTrkBk->IndirectInfo.pCIndirectInfo;
                if (pCIndirectInfo->m_IndirectInfoType == IIType_CreateSection ||
                    pCIndirectInfo->m_IndirectInfoType == IIType_OpenSection)
                {
                    void *baseAddr = pCIndirectInfo->IIType_Section.m_BaseAddress;
                    auto resSect = thelist->find(TreeKey(bt_IndirectInfo, baseAddr));
                    if (resSect != thelist->end())
                    {
                        thelist->erase(resSect);
                    }
                }

                delete pCIndirectInfo;
                thelist->erase(res);
            }

        }
    }

    return fRetval;
}




HGLOBAL WINAPI Mine_LoadResource(
    HMODULE hModule,
    HRSRC hResInfo
)
{
    HGLOBAL hRsrc = Real_LoadResource(hModule, hResInfo);
    if (hRsrc)
    {
        if (CDisableTrace::CanDetour())
        {
            DWORD dwSize = SizeofResource(hModule, hResInfo); // outside critsect to avoid deadlock
            LockCritSect;
            CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour

            ListTrkBlocks *thelist = g_TrackMemClass->GetList();
            auto key = TreeKey(bt_LoadResource, (LPVOID)(hRsrc));
            auto res = thelist->find(key);
            if (res != thelist->end()) // does it exist?
            {
                thelist->erase(res);
            }
            else
            {
                // it's ok not to find it: could have been created before we got here
                //                VSASSERT(false,"tlsfree key not found");
            }

            TrkBlock oTrkBlk(dwSize);
            oTrkBlk.LoadResourceInfo.hResource = hRsrc;
            oTrkBlk.LoadResourceInfo.hResInfo = hResInfo;


            auto resPairIB = g_TrackMemClass->InsertIntoList(PairAddrBlock(key, oTrkBlk), /*nExtraFramesToSkip =*/1);
            VSASSERT(resPairIB.second == true, "failed to insert for LoadResource");
        }
    }
    return hRsrc;
}

#define LOGFILE_PATH L"c:\\users\\calvinh\\documents\\t1.etl"
#define LOGSESSION_NAME L"My Event Trace Session"

// GUID that identifies your trace session.
// Remember to create your own session GUID.


// {87CAD8CB-F1D9-403F-AACE-6DBE22938419}
static const GUID SessionGuid =
{ 0x87cad8cb, 0xf1d9, 0x403f,{ 0xaa, 0xce, 0x6d, 0xbe, 0x22, 0x93, 0x84, 0x19 } };


/*
Microsoft-Windows-Kernel-Disk            {C7BDE69A-E1E0-4177-B6EF-283AD1525271}
Microsoft-Windows-Kernel-EventTracing    {B675EC37-BDB6-4648-BC92-F3FDC74D3CA2}
Microsoft-Windows-Kernel-File            {EDD08927-9CC4-4E65-B970-C2560FB5C289}
Microsoft-Windows-Kernel-General         {A68CA8B7-004F-D7B6-A698-07E2DE0F1F5D}
Microsoft-Windows-Kernel-IO              {ABF1F586-2E50-4BA8-928D-49044E6F0DB7}
Microsoft-Windows-DotNETRuntime          {E13C0D23-CCBC-4E12-931B-D9CC2EEE27E4}
Microsoft-Windows-DotNETRuntimeRundown   {A669021C-C450-4609-A035-5AF59AF4DF18}

Microsoft-VisualStudio-Common            {25c93eda-40a3-596d-950d-998ab963f367}
tracelog -stop "My Event Trace Session"

*/
void StartATraceSession()
{
    ULONG status = ERROR_SUCCESS;
    TRACEHANDLE SessionHandle = 0;
    EVENT_TRACE_PROPERTIES* pSessionProperties = NULL;
    ULONG BufferSize = 0;
    BOOL TraceOn = TRUE;

    // Allocate memory for the session properties. The memory must
    // be large enough to include the log file name and session name,
    // which get appended to the end of the session properties structure.

    BufferSize = sizeof(EVENT_TRACE_PROPERTIES) + sizeof(LOGFILE_PATH) + sizeof(LOGSESSION_NAME);
    pSessionProperties = (EVENT_TRACE_PROPERTIES*)malloc(BufferSize);

    // Set the session properties. You only append the log file name
    // to the properties structure; the StartTrace function appends
    // the session name for you.

    ZeroMemory(pSessionProperties, BufferSize);
    pSessionProperties->Wnode.BufferSize = BufferSize;
    pSessionProperties->Wnode.Flags = WNODE_FLAG_TRACED_GUID;
    pSessionProperties->Wnode.ClientContext = 1; //QPC clock resolution
    pSessionProperties->Wnode.Guid = SessionGuid;
    pSessionProperties->LogFileMode = EVENT_TRACE_FILE_MODE_SEQUENTIAL;
    pSessionProperties->MaximumFileSize = 1;  // 1 MB
    pSessionProperties->LoggerNameOffset = sizeof(EVENT_TRACE_PROPERTIES);
    pSessionProperties->LogFileNameOffset = sizeof(EVENT_TRACE_PROPERTIES) + sizeof(LOGSESSION_NAME);
    memcpy((LPWSTR)((char*)pSessionProperties + pSessionProperties->LogFileNameOffset),
        LOGFILE_PATH,
        sizeof(LOGFILE_PATH)
    );

    // Create the trace session.

    status = StartTraceW((PTRACEHANDLE)&SessionHandle, LOGSESSION_NAME, pSessionProperties);
    if (ERROR_SUCCESS != status)
    {
        wprintf(L"StartTrace() failed with %lu\n", status);
        goto cleanup;
    }

    // Enable the providers that you want to log events to your session.
    GUID ProviderGuid;
    CLSIDFromString(L"{E13C0D23-CCBC-4E12-931B-D9CC2EEE27E4}", &ProviderGuid);

    status = EnableTraceEx2(
        SessionHandle,
        (LPCGUID)&ProviderGuid,
        EVENT_CONTROL_CODE_ENABLE_PROVIDER,
        TRACE_LEVEL_INFORMATION,
        0,
        0,
        0,
        NULL
    );

    if (ERROR_SUCCESS != status)
    {
        wprintf(L"EnableTrace() failed with %lu\n", status);
        TraceOn = FALSE;
        goto cleanup;
    }

    //wprintf(L"Run the provider application. Then hit any key to stop the session.\n");
    //_getch();

cleanup:
    auto x = 2;
    //if (SessionHandle)
    //{
    //	if (TraceOn)
    //	{
    //		status = EnableTraceEx2(
    //			SessionHandle,
    //			(LPCGUID)&ProviderGuid,
    //			EVENT_CONTROL_CODE_DISABLE_PROVIDER,
    //			TRACE_LEVEL_INFORMATION,
    //			0,
    //			0,
    //			0,
    //			NULL
    //		);
    //	}

    //	status = ControlTrace(SessionHandle, LOGSESSION_NAME, pSessionProperties, EVENT_TRACE_CONTROL_STOP);

    //	if (ERROR_SUCCESS != status)
    //	{
    //		wprintf(L"ControlTrace(stop) failed with %lu\n", status);
    //	}
    //}

    //if (pSessionProperties)
    //{
    //	free(pSessionProperties);
    //	pSessionProperties = NULL;
    //}

}

ULONG WINAPI Mine_EventWrite(
    _In_ REGHANDLE RegHandle,
    _In_ PCEVENT_DESCRIPTOR EventDescriptor,
    _In_ ULONG UserDataCount,
    _In_reads_opt_(UserDataCount) PEVENT_DATA_DESCRIPTOR UserData
)
{
    ULONG result = 0;
    if (CDisableTrace::CanDetour())
    {
        LockCritSect;
        CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
        // warning: sprintf versions use memory allocators, which could deadlock
        //WCHAR szBuf[1000];
        //swprintf_s(szBuf, L"Etw ID=%d Op = %d Ch = %d",
        //    EventDescriptor->Id,
        //    EventDescriptor->Opcode,
        //    EventDescriptor->Channel
        //);
        CustomCodeMarker(L"ETW",
            EventDescriptor->Opcode,
            0,
            (INT)EventDescriptor->Id,
            UserData,
            UserDataCount * sizeof(EVENT_DATA_DESCRIPTOR)
        );
    }
    result = Real_EventWrite(RegHandle, EventDescriptor, UserDataCount, UserData);
    return result;
}

ULONG
WINAPI
Mine_EventWriteString(
    _In_ REGHANDLE RegHandle,
    _In_ UCHAR Level,
    _In_ ULONGLONG Keyword,
    _In_ PCWSTR String
)
{
    ULONG result = 0;
    if (CDisableTrace::CanDetour())
    {
        LockCritSect;
        CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
                            //char szBuf[1000];
                            //sprintf_s(szBuf, sizeof(szBuf), "Etw C=%d", UserDataCount);
        CustomCodeMarker(String,
            0,  //nEventType
            0, // nDepthLevel
            0, //MarkerId
            0, //UserData
            0 //UserDataCount
        );
    }
    result = Real_EventWriteString(RegHandle, Level, Keyword, String);
    return result;
}


ULONG WINAPI Mine_TraceEvent(
    __in TRACEHANDLE SessionHandle,
    __in PEVENT_TRACE_HEADER EventTrace
)
{
    ULONG result = 0;
    if (CDisableTrace::CanDetour())
    {
        LockCritSect;
        CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
        CustomCodeMarker(L"TraceEvent", 0, 0, 0, 0, 0);
        
    }
    result = Real_TraceEvent(SessionHandle, EventTrace);
    return result;
}



PCHAR DetRealName(PCHAR psz)
{
    PCHAR pszBeg = psz;
    // Move to end of name.
    while (*psz) {
        psz++;
    }
    // Move back through A-Za-z0-9 names.
    while (psz > pszBeg &&
        ((psz[-1] >= 'A' && psz[-1] <= 'Z') ||
        (psz[-1] >= 'a' && psz[-1] <= 'z') ||
            (psz[-1] >= '0' && psz[-1] <= '9'))) {
        psz--;
    }
    return psz;
}


VOID DetAttach(PVOID *ppbReal, PVOID pbMine, PCHAR psz)
{
    LONG l = DetourAttach(ppbReal, pbMine);
    if (l != 0) {
        char szBuf[1024];
        sprintf_s(szBuf, sizeof(szBuf), "Attach detour Failed %s Errcode=%d. (Most features will not work. if it's PerfCodeMaker: Try disabling in .INI file)", DetRealName(psz), l);
        VSASSERT(false, szBuf);
    }
}

VOID DetDetach(PVOID *ppbReal, PVOID pbMine, PCHAR psz)
{
    LONG l = DetourDetach(ppbReal, pbMine);
    if (l != 0) {
        char szBuf[1024];
        sprintf_s(szBuf, sizeof(szBuf), "Detach Failed %s Errcode=%d", DetRealName(psz), l);
        VSASSERT(false, szBuf);
    }
}




void AddGdiObject(HGDIOBJ a0)
{
    if (a0 != INVALID_HANDLE_VALUE && a0 != 0)
    {
        if (CDisableTrace::CanDetour())
        {
            LockCritSect;
            CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
            ListTrkBlocks *thelist = g_TrackMemClass->GetList();
            auto key = TreeKey(bt_GdiObject, (LPVOID)(a0));
            auto res = thelist->find(key);
            if (res != thelist->end()) // we're adding but it already exists. We'll erase existing
            {
                thelist->erase(res);
            }

            TrkBlock oGdiBlk(/*size=*/0);
            oGdiBlk.GdiObjInfo.hgdiobj = a0;
            oGdiBlk.GdiObjInfo.GdiObjectType = (BYTE)GetObjectType(a0);
            auto resPairIB = g_TrackMemClass->InsertIntoList(PairAddrBlock(key, oGdiBlk), /*nExtraFramesToSkip =*/2);
#if MSDEBUG
            VSASSERT(resPairIB.second == true, "failed to insert for GdiObj");
#endif MSDEBUG
        }
    }
}

void RemoveGdiObject(HGDIOBJ a0)
{
    if (CDisableTrace::CanDetour())
    {
        LockCritSect;
        CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
        ListTrkBlocks *thelist = g_TrackMemClass->GetList();
        auto key = TreeKey(bt_GdiObject, (LPVOID)(a0));
        auto res = thelist->find(key);
        if (res != thelist->end())
        {
            thelist->erase(res);
        }
        else
        {
#if MSDEBUG
            //            VSASSERT(false, "Couldn't remove GDI obj");
#endif MSDEBUG
        }
    }
    else
    {
        VSASSERT(false, "Couldn't Remove because CanDetour=false");
    }
}
HDC __stdcall Mine_BeginPaint(HWND a0, LPPAINTSTRUCT a1)
{
    auto retval = Real_BeginPaint(a0, a1);
    AddGdiObject(retval);
    return retval;
}

HICON __stdcall Mine_CopyIcon(HICON a0)
{
    auto retval = Real_CopyIcon(a0);
    AddGdiObject(retval);
    return retval;
}

HANDLE __stdcall Mine_CopyImage(HANDLE a0, UINT a1, int a2, int a3, UINT a4)
{
    auto retval = Real_CopyImage(a0, a1, a2, a3, a4);
    AddGdiObject(retval);
    return retval;
}

HMETAFILE __stdcall Mine_CopyMetaFileA(HMETAFILE a0, LPCSTR a1)
{
    auto retval = Real_CopyMetaFileA(a0, a1);
    AddGdiObject(retval);
    return retval;
}

HMETAFILE __stdcall Mine_CopyMetaFileW(HMETAFILE a0, LPCWSTR a1)
{
    auto retval = Real_CopyMetaFileW(a0, a1);
    AddGdiObject(retval);
    return retval;
}

HBITMAP __stdcall Mine_CreateBitmap(int a0, int a1, UINT a2, UINT a3, void* a4)
{
    auto retval = Real_CreateBitmap(a0, a1, a2, a3, a4);
    AddGdiObject(retval);
    return retval;
}

HBITMAP __stdcall Mine_CreateBitmapIndirect(BITMAP* a0)
{
    auto retval = Real_CreateBitmapIndirect(a0);
    AddGdiObject(retval);
    return retval;
}
HBRUSH __stdcall Mine_CreateBrushIndirect(struct tagLOGBRUSH* a0)
{
    auto retval = Real_CreateBrushIndirect(a0);
    AddGdiObject(retval);
    return retval;
}

HCOLORSPACE __stdcall Mine_CreateColorSpaceA(struct tagLOGCOLORSPACEA* a0)
{
    auto retval = Real_CreateColorSpaceA(a0);
    AddGdiObject(retval);
    return retval;
}

HCOLORSPACE __stdcall Mine_CreateColorSpaceW(struct tagLOGCOLORSPACEW* a0)
{
    auto retval = Real_CreateColorSpaceW(a0);
    AddGdiObject(retval);
    return retval;
}

HBITMAP __stdcall Mine_CreateCompatibleBitmap(HDC a0, int a1, int a2)
{
    auto retval = Real_CreateCompatibleBitmap(a0, a1, a2);
    AddGdiObject(retval);
    return retval;
}

HDC __stdcall Mine_CreateCompatibleDC(HDC a0)
{
    auto retval = Real_CreateCompatibleDC(a0);
    AddGdiObject(retval);
    return retval;
}

HCURSOR __stdcall Mine_CreateCursor(HINSTANCE a0, int a1, int a2, int a3, int a4, void* a5, void* a6)
{
    auto retval = Real_CreateCursor(a0, a1, a2, a3, a4, a5, a6);
    AddGdiObject(retval);
    return retval;
}


HDC __stdcall Mine_CreateDCA(LPCSTR a0, LPCSTR a1, LPCSTR a2, struct _devicemodeA* a3)
{
    auto retval = Real_CreateDCA(a0, a1, a2, a3);
    AddGdiObject(retval);
    return retval;
}

HDC __stdcall Mine_CreateDCW(LPCWSTR a0, LPCWSTR a1, LPCWSTR a2, struct _devicemodeW* a3)
{
    auto retval = Real_CreateDCW(a0, a1, a2, a3);
    AddGdiObject(retval);
    return retval;
}
HBRUSH __stdcall Mine_CreateDIBPatternBrush(HGLOBAL a0, UINT a1)
{
    auto retval = Real_CreateDIBPatternBrush(a0, a1);
    AddGdiObject(retval);
    return retval;
}

HBRUSH __stdcall Mine_CreateDIBPatternBrushPt(void* a0, UINT a1)
{
    auto retval = Real_CreateDIBPatternBrushPt(a0, a1);
    AddGdiObject(retval);
    return retval;
}

HBITMAP __stdcall Mine_CreateDIBSection(HDC a0, struct tagBITMAPINFO* a1, UINT a2, void** a3, HANDLE a4, DWORD a5)
{
    auto retval = Real_CreateDIBSection(a0, a1, a2, a3, a4, a5);
    AddGdiObject(retval);
    return retval;
}

HBITMAP __stdcall Mine_CreateDIBitmap(HDC a0, BITMAPINFOHEADER* a1, DWORD a2, void* a3, struct tagBITMAPINFO* a4, UINT a5)
{
    auto retval = Real_CreateDIBitmap(a0, a1, a2, a3, a4, a5);
    AddGdiObject(retval);
    return retval;
}

HRGN __stdcall Mine_CreateEllipticRgn(int a0, int a1, int a2, int a3)
{
    auto retval = Real_CreateEllipticRgn(a0, a1, a2, a3);
    AddGdiObject(retval);
    return retval;
}

HRGN __stdcall Mine_CreateEllipticRgnIndirect(RECT* a0)
{
    auto retval = Real_CreateEllipticRgnIndirect(a0);
    AddGdiObject(retval);
    return retval;
}

HDC __stdcall Mine_CreateEnhMetaFileA(HDC a0, LPCSTR a1, RECT* a2, LPCSTR a3)
{
    auto retval = Real_CreateEnhMetaFileA(a0, a1, a2, a3);
    AddGdiObject(retval);
    return retval;
}

HDC __stdcall Mine_CreateEnhMetaFileW(HDC a0, LPCWSTR a1, RECT* a2, LPCWSTR a3)
{
    auto retval = Real_CreateEnhMetaFileW(a0, a1, a2, a3);
    AddGdiObject(retval);
    return retval;
}

HFONT __stdcall Mine_CreateFontA(int a0, int a1, int a2, int a3, int a4, DWORD a5, DWORD a6, DWORD a7, DWORD a8, DWORD a9, DWORD a10, DWORD a11, DWORD a12, LPCSTR a13)
{
    HFONT retval = Real_CreateFontA(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13);
    AddGdiObject(retval);
    return retval;
}

HFONT __stdcall Mine_CreateFontW(int a0, int a1, int a2, int a3, int a4, DWORD a5, DWORD a6, DWORD a7, DWORD a8, DWORD a9, DWORD a10, DWORD a11, DWORD a12, LPCWSTR a13)
{
    HFONT retval = Real_CreateFontW(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13);
    AddGdiObject(retval);
    return retval;
}

HFONT __stdcall Mine_CreateFontIndirectA(LOGFONTA* a0)
{
    HFONT retval = Real_CreateFontIndirectA(a0);
    AddGdiObject(retval);
    return retval;
}

HFONT __stdcall Mine_CreateFontIndirectW(LOGFONTW* a0)
{
    HFONT retval = Real_CreateFontIndirectW(a0);
    AddGdiObject(retval);
    return retval;
}

HFONT __stdcall Mine_CreateFontIndirectExA(ENUMLOGFONTEXDVA* a0)
{
    HFONT retval = Real_CreateFontIndirectExA(a0);
    AddGdiObject(retval);
    return retval;
}

HFONT __stdcall Mine_CreateFontIndirectExW(ENUMLOGFONTEXDVW* a0)
{
    HFONT retval = Real_CreateFontIndirectExW(a0);
    AddGdiObject(retval);
    return retval;
}

HPALETTE __stdcall Mine_CreateHalftonePalette(HDC a0)
{
    HPALETTE retval = Real_CreateHalftonePalette(a0);
    AddGdiObject(retval);
    return retval;
}
HBRUSH __stdcall Mine_CreateHatchBrush(int a0, COLORREF a1)
{
    HBRUSH retval = Real_CreateHatchBrush(a0, a1);
    AddGdiObject(retval);
    return retval;
}

HDC __stdcall Mine_CreateICA(LPCSTR a0, LPCSTR a1, LPCSTR a2, struct _devicemodeA* a3)
{
    HDC retval = Real_CreateICA(a0, a1, a2, a3);
    AddGdiObject(retval);
    return retval;
}

HDC __stdcall Mine_CreateICW(LPCWSTR a0, LPCWSTR a1, LPCWSTR a2, struct _devicemodeW* a3)
{
    HDC retval = Real_CreateICW(a0, a1, a2, a3);
    AddGdiObject(retval);
    return retval;
}

HICON __stdcall Mine_CreateIcon(HINSTANCE a0, int a1, int a2, BYTE a3, BYTE a4, BYTE* a5, BYTE* a6)
{
    HICON retval = Real_CreateIcon(a0, a1, a2, a3, a4, a5, a6);
    AddGdiObject(retval);
    return retval;
}

HICON __stdcall Mine_CreateIconFromResource(PBYTE a0, DWORD a1, BOOL a2, DWORD a3)
{
    HICON retval = Real_CreateIconFromResource(a0, a1, a2, a3);
    AddGdiObject(retval);
    return retval;
}

HICON __stdcall Mine_CreateIconFromResourceEx(PBYTE a0, DWORD a1, BOOL a2, DWORD a3, int a4, int a5, UINT a6)
{
    HICON retval = Real_CreateIconFromResourceEx(a0, a1, a2, a3, a4, a5, a6);
    AddGdiObject(retval);
    return retval;
}

HICON __stdcall Mine_CreateIconIndirect(struct _ICONINFO* a0)
{
    HICON retval = Real_CreateIconIndirect(a0);
    AddGdiObject(retval);
    return retval;
}

HICON __stdcall Mine_DuplicateIcon(HINSTANCE a0, HICON a1)
{
    HICON retval = Real_DuplicateIcon(a0, a1);
    AddGdiObject(retval);
    return retval;
}

HMENU __stdcall Mine_CreateMenu(void)
{
    HMENU retval = Real_CreateMenu();
    AddGdiObject(retval);
    return retval;
}

HDC __stdcall Mine_CreateMetaFileA(LPCSTR a0)
{
    HDC retval = Real_CreateMetaFileA(a0);
    AddGdiObject(retval);
    return retval;
}

HDC __stdcall Mine_CreateMetaFileW(LPCWSTR a0)
{
    HDC retval = Real_CreateMetaFileW(a0);
    AddGdiObject(retval);
    return retval;
}

HPALETTE __stdcall Mine_CreatePalette(LOGPALETTE* a0)
{
    HPALETTE retval = Real_CreatePalette(a0);
    AddGdiObject(retval);
    return retval;
}
HBRUSH __stdcall Mine_CreatePatternBrush(HBITMAP a0)
{
    HBRUSH retval = Real_CreatePatternBrush(a0);
    AddGdiObject(retval);
    return retval;
}

HPEN __stdcall Mine_CreatePen(int a0, int a1, COLORREF a2)
{
    HPEN retval = Real_CreatePen(a0, a1, a2);
    AddGdiObject(retval);
    return retval;
}
HPEN __stdcall Mine_CreatePenIndirect(LOGPEN* a0)
{
    HPEN retval = Real_CreatePenIndirect(a0);
    AddGdiObject(retval);
    return retval;
}

HRGN __stdcall Mine_CreatePolyPolygonRgn(POINT* a0, INT* a1, int a2, int a3)
{
    HRGN retval = Real_CreatePolyPolygonRgn(a0, a1, a2, a3);
    AddGdiObject(retval);
    return retval;
}

HRGN __stdcall Mine_CreatePolygonRgn(POINT* a0, int a1, int a2)
{
    HRGN retval = Real_CreatePolygonRgn(a0, a1, a2);
    AddGdiObject(retval);
    return retval;
}

HMENU __stdcall Mine_CreatePopupMenu(void)
{
    HMENU retval = Real_CreatePopupMenu();
    AddGdiObject(retval);
    return retval;
}

HRGN __stdcall Mine_CreateRectRgn(int a0, int a1, int a2, int a3)
{
    HRGN retval = Real_CreateRectRgn(a0, a1, a2, a3);
    AddGdiObject(retval);
    return retval;
}

HRGN __stdcall Mine_CreateRectRgnIndirect(RECT* a0)
{
    HRGN retval = Real_CreateRectRgnIndirect(a0);
    AddGdiObject(retval);
    return retval;
}

HRGN __stdcall Mine_CreateRoundRectRgn(int a0, int a1, int a2, int a3, int a4, int a5)
{
    HRGN retval = Real_CreateRoundRectRgn(a0, a1, a2, a3, a4, a5);
    AddGdiObject(retval);
    return retval;
}

HBRUSH __stdcall Mine_CreateSolidBrush(COLORREF a0)
{
    HBRUSH retval = Real_CreateSolidBrush(a0);
    AddGdiObject(retval);
    return retval;
}

BOOL __stdcall Mine_DeleteColorSpace(HCOLORSPACE a0)
{
    BOOL retval = Real_DeleteColorSpace(a0);
    if (retval)
        RemoveGdiObject(a0);
    return retval;
}

BOOL __stdcall Mine_DeleteDC(HDC a0)
{
    auto retval = Real_DeleteDC(a0);
    if (retval)
    {
        RemoveGdiObject(a0);
    }
    return retval;
}


BOOL __stdcall Mine_DeleteEnhMetaFile(HENHMETAFILE a0)
{
    BOOL retval = Real_DeleteEnhMetaFile(a0);
    if (retval)
        RemoveGdiObject(a0);
    return retval;
}

BOOL __stdcall Mine_DeleteMetaFile(HMETAFILE a0)
{
    BOOL retval = Real_DeleteMetaFile(a0);
    if (retval)
        RemoveGdiObject(a0);
    return retval;
}

BOOL __stdcall Mine_DeleteObject(HGDIOBJ a0)
{
    auto retval = Real_DeleteObject(a0);
    if (retval) // success
    {
        RemoveGdiObject(a0);
    }
    return retval;
}

BOOL __stdcall Mine_DestroyAcceleratorTable(HACCEL a0)
{
    auto retval = Real_DestroyAcceleratorTable(a0);
    if (retval) // success
    {
        RemoveGdiObject(a0);
    }
    return retval;
}

BOOL __stdcall Mine_DestroyCursor(HCURSOR a0)
{
    auto retval = Real_DestroyCursor(a0);
    if (retval) // success
    {
        RemoveGdiObject(a0);
    }
    return retval;
}
BOOL __stdcall Mine_DestroyIcon(HICON a0)
{
    auto retval = Real_DestroyIcon(a0);
    if (retval) // success
    {
        RemoveGdiObject(a0);
    }
    return retval;
}
BOOL __stdcall Mine_DestroyMenu(HMENU a0)
{
    auto retval = Real_DestroyMenu(a0);
    if (retval) // success
    {
        RemoveGdiObject(a0);
    }
    return retval;
}

BOOL __stdcall Mine_EndPaint(HWND a0, PAINTSTRUCT* a1)
{
    auto retval = Real_EndPaint(a0, a1);
    if (retval) // success
    {
        RemoveGdiObject(a1->hdc);
    }
    return retval;
}

HPEN __stdcall Mine_ExtCreatePen(DWORD a0, DWORD a1, struct tagLOGBRUSH* a2, DWORD a3, DWORD* a4)
{
    auto retval = Real_ExtCreatePen(a0, a1, a2, a3, a4);
    AddGdiObject(retval);
    return retval;
}

HRGN __stdcall Mine_ExtCreateRegion(XFORM* a0, DWORD a1, RGNDATA* a2)
{
    auto retval = Real_ExtCreateRegion(a0, a1, a2);
    AddGdiObject(retval);
    return retval;
}

HICON __stdcall Mine_ExtractAssociatedIconA(HINSTANCE a0, _In_z_ LPSTR a1, _Inout_ LPWORD a2)
{
    auto retval = Real_ExtractAssociatedIconA(a0, a1, a2);
    AddGdiObject(retval);
    return retval;
}

HICON __stdcall Mine_ExtractAssociatedIconW(HINSTANCE a0, _In_z_ LPWSTR a1, _Inout_ LPWORD a2)
{
    auto retval = Real_ExtractAssociatedIconW(a0, a1, a2);
    AddGdiObject(retval);
    return retval;
}

HICON __stdcall Mine_ExtractIconA(HINSTANCE a0, LPCSTR a1, UINT a2)
{
    auto retval = Real_ExtractIconA(a0, a1, a2);
    AddGdiObject(retval);
    return retval;
}

HICON __stdcall Mine_ExtractIconW(HINSTANCE a0, LPCWSTR a1, UINT a2)
{
    auto retval = Real_ExtractIconW(a0, a1, a2);
    AddGdiObject(retval);
    return retval;
}

UINT __stdcall Mine_ExtractIconExA(LPCSTR pszIconFile, int index, HICON* phLarge, HICON* phSmall, UINT uiIcon)
{
    auto retval = Real_ExtractIconExA(pszIconFile, index, phLarge, phSmall, uiIcon);
    for (UINT i = 0; i < uiIcon; i++)
    {
        if (phLarge != nullptr)
        {
            AddGdiObject(phLarge[i]);
        }
        if (phSmall != nullptr)
        {
            AddGdiObject(phSmall[i]);
        }
    }
    return retval;
}

UINT __stdcall Mine_ExtractIconExW(LPCWSTR pszIconFile, int index, HICON* phLarge, HICON* phSmall, UINT uiIcon)
{
    auto retval = Real_ExtractIconExW(pszIconFile, index, phLarge, phSmall, uiIcon);
    for (UINT i = 0; i < uiIcon; i++)
    {
        if (phLarge != nullptr)
        {
            AddGdiObject(phLarge[i]);
        }
        if (phSmall != nullptr)
        {
            AddGdiObject(phSmall[i]);
        }
    }
    return retval;
}

HDC __stdcall Mine_GetDC(HWND a0)
{
    auto retval = Real_GetDC(a0);
    AddGdiObject(retval);
    return retval;
}

HDC __stdcall Mine_GetDCEx(HWND a0, HRGN a1, DWORD a2)
{
    auto retval = Real_GetDCEx(a0, a1, a2);
    AddGdiObject(retval);
    return retval;
}

HENHMETAFILE __stdcall Mine_GetEnhMetaFileA(LPCSTR a0)
{
    auto retval = Real_GetEnhMetaFileA(a0);
    AddGdiObject(retval);
    return retval;
}

HENHMETAFILE __stdcall Mine_GetEnhMetaFileW(LPCWSTR a0)
{
    auto retval = Real_GetEnhMetaFileW(a0);
    AddGdiObject(retval);
    return retval;
}

HMETAFILE __stdcall Mine_GetMetaFileA(LPCSTR a0)
{
    auto retval = Real_GetMetaFileA(a0);
    AddGdiObject(retval);
    return retval;
}

HMETAFILE __stdcall Mine_GetMetaFileW(LPCWSTR a0)
{
    auto retval = Real_GetMetaFileW(a0);
    AddGdiObject(retval);
    return retval;
}

HDC __stdcall Mine_GetWindowDC(HWND a0)
{
    auto retval = Real_GetWindowDC(a0);
    AddGdiObject(retval);
    return retval;
}

HACCEL __stdcall Mine_LoadAcceleratorsA(HINSTANCE a0, LPCSTR a1)
{
    auto retval = Real_LoadAcceleratorsA(a0, a1);
    AddGdiObject(retval);
    return retval;
}

HACCEL __stdcall Mine_LoadAcceleratorsW(HINSTANCE a0, LPCWSTR a1)
{
    auto retval = Real_LoadAcceleratorsW(a0, a1);
    AddGdiObject(retval);
    return retval;
}

HBITMAP __stdcall Mine_LoadBitmapA(HINSTANCE a0, LPCSTR a1)
{
    auto retval = Real_LoadBitmapA(a0, a1);
    AddGdiObject(retval);
    return retval;
}

HBITMAP __stdcall Mine_LoadBitmapW(HINSTANCE a0, LPCWSTR a1)
{
    auto retval = Real_LoadBitmapW(a0, a1);
    AddGdiObject(retval);
    return retval;
}

HCURSOR __stdcall Mine_LoadCursorA(HINSTANCE a0, LPCSTR a1)
{
    auto retval = Real_LoadCursorA(a0, a1);
    AddGdiObject(retval);
    return retval;
}

HCURSOR __stdcall Mine_LoadCursorW(HINSTANCE a0, LPCWSTR a1)
{
    auto retval = Real_LoadCursorW(a0, a1);
    AddGdiObject(retval);
    return retval;
}

HCURSOR __stdcall Mine_LoadCursorFromFileA(LPCSTR a0)
{
    auto retval = Real_LoadCursorFromFileA(a0);
    AddGdiObject(retval);
    return retval;
}

HCURSOR __stdcall Mine_LoadCursorFromFileW(LPCWSTR a0)
{
    auto retval = Real_LoadCursorFromFileW(a0);
    AddGdiObject(retval);
    return retval;
}

HICON __stdcall Mine_LoadIconA(HINSTANCE a0, LPCSTR a1)
{
    auto retval = Real_LoadIconA(a0, a1);
    AddGdiObject(retval);
    return retval;
}

HICON __stdcall Mine_LoadIconW(HINSTANCE a0, LPCWSTR a1)
{
    auto retval = Real_LoadIconW(a0, a1);
    AddGdiObject(retval);
    return retval;
}

HANDLE __stdcall Mine_LoadImageA(HINSTANCE a0, LPCSTR a1, UINT a2, int a3, int a4, UINT a5)
{
    auto retval = Real_LoadImageA(a0, a1, a2, a3, a4, a5);
    AddGdiObject(retval);
    return retval;
}

HANDLE __stdcall Mine_LoadImageW(HINSTANCE a0, LPCWSTR a1, UINT a2, int a3, int a4, UINT a5)
{
    auto retval = Real_LoadImageW(a0, a1, a2, a3, a4, a5);
    AddGdiObject(retval);
    return retval;
}

HMENU __stdcall Mine_LoadMenuA(HINSTANCE a0, LPCSTR a1)
{
    auto retval = Real_LoadMenuA(a0, a1);
    AddGdiObject(retval);
    return retval;
}

HMENU __stdcall Mine_LoadMenuW(HINSTANCE a0, LPCWSTR a1)
{
    auto retval = Real_LoadMenuW(a0, a1);
    AddGdiObject(retval);
    return retval;
}

HMENU __stdcall Mine_LoadMenuIndirectA(MENUTEMPLATEA* a0)
{
    auto retval = Real_LoadMenuIndirectA(a0);
    AddGdiObject(retval);
    return retval;
}

HMENU __stdcall Mine_LoadMenuIndirectW(MENUTEMPLATEW* a0)
{
    auto retval = Real_LoadMenuIndirectW(a0);
    AddGdiObject(retval);
    return retval;
}

int __stdcall Mine_ReleaseDC(HWND a0, HDC a1)
{
    auto retval = Real_ReleaseDC(a0, a1);
    if (retval)
    {
        RemoveGdiObject(a1);
    }
    return retval;
}

HMETAFILE __stdcall Mine_SetMetaFileBitsEx(UINT a0, BYTE* a1)
{
    auto retval = Real_SetMetaFileBitsEx(a0, a1);
    AddGdiObject(retval);
    return retval;
}

HENHMETAFILE __stdcall Mine_SetWinMetaFileBits(UINT a0, BYTE* a1, HDC a2, struct tagMETAFILEPICT* a3)
{
    auto retval = Real_SetWinMetaFileBits(a0, a1, a2, a3);
    AddGdiObject(retval);
    return retval;
}

DWORD_PTR __stdcall Mine_SHGetFileInfoA(LPCSTR a0, DWORD a1, SHFILEINFOA *a2, UINT a3, UINT a4)
{
    auto retval = Real_SHGetFileInfoA(a0, a1, a2, a3, a4);
    if (a2 != nullptr)
    {
        AddGdiObject(a2->hIcon);
    }
    return retval;
}

DWORD_PTR __stdcall Mine_SHGetFileInfoW(LPCWSTR a0, DWORD a1, SHFILEINFOW *a2, UINT a3, UINT a4)
{
    auto retval = Real_SHGetFileInfoW(a0, a1, a2, a3, a4);
    if (a2 != nullptr)
    {
        AddGdiObject(a2->hIcon);
    }
    return retval;
}






HRESULT  InitGdiTrack()
{
    HMODULE hInstGDI = GetModuleHandle("Gdi32.dll");
    CHECKNULL(hInstGDI);
    HMODULE hInstUsr = GetModuleHandle("User32.dll");
    HMODULE hInstShl = LoadLibrary("Shell32.dll");

    DECLATTACH(hInstUsr, BeginPaint);
    DECLATTACH(hInstUsr, CopyIcon);
    DECLATTACH(hInstUsr, CopyImage);
    DECLATTACH(hInstGDI, CopyMetaFileA);
    DECLATTACH(hInstGDI, CopyMetaFileW);
    DECLATTACH(hInstGDI, CreateBitmap);
    DECLATTACH(hInstGDI, CreateBitmapIndirect);

    DECLATTACH(hInstGDI, CreateBrushIndirect);
    DECLATTACH(hInstGDI, CreateColorSpaceA);
    DECLATTACH(hInstGDI, CreateColorSpaceW);
    DECLATTACH(hInstGDI, CreateCompatibleBitmap);
    DECLATTACH(hInstGDI, CreateCompatibleDC);
    DECLATTACH(hInstUsr, CreateCursor);

    DECLATTACH(hInstGDI, CreateDCA);
    DECLATTACH(hInstGDI, CreateDCW);

    DECLATTACH(hInstGDI, CreateDIBPatternBrush);
    DECLATTACH(hInstGDI, CreateDIBPatternBrushPt);
    DECLATTACH(hInstGDI, CreateDIBSection);
    DECLATTACH(hInstGDI, CreateDIBitmap);

    DECLATTACH(hInstGDI, CreateEllipticRgn);
    DECLATTACH(hInstGDI, CreateEllipticRgnIndirect);
    DECLATTACH(hInstGDI, CreateEnhMetaFileA);
    DECLATTACH(hInstGDI, CreateEnhMetaFileW);


    DECLATTACH(hInstGDI, CreateFontA);
    DECLATTACH(hInstGDI, CreateFontW);
    DECLATTACH(hInstGDI, CreateFontIndirectA);
    DECLATTACH(hInstGDI, CreateFontIndirectW);
    DECLATTACH(hInstGDI, CreateFontIndirectExA);
    DECLATTACH(hInstGDI, CreateFontIndirectExW);

    DECLATTACH(hInstGDI, CreateHalftonePalette);
    DECLATTACH(hInstGDI, CreateHatchBrush);
    DECLATTACH(hInstGDI, CreateICA);
    DECLATTACH(hInstGDI, CreateICW);
    DECLATTACH(hInstUsr, CreateIcon);
    DECLATTACH(hInstUsr, CreateIconFromResource);
    DECLATTACH(hInstUsr, CreateIconFromResourceEx);

    DECLATTACH(hInstUsr, CreateIconIndirect);
    DECLATTACH(hInstShl, DuplicateIcon);
    DECLATTACH(hInstUsr, CreateMenu);
    DECLATTACH(hInstGDI, CreateMetaFileA);
    DECLATTACH(hInstGDI, CreateMetaFileW);
    DECLATTACH(hInstGDI, CreatePalette);

    DECLATTACH(hInstGDI, CreatePatternBrush);
    DECLATTACH(hInstGDI, CreatePen);
    DECLATTACH(hInstGDI, CreatePenIndirect);
    DECLATTACH(hInstGDI, CreatePolyPolygonRgn);
    DECLATTACH(hInstGDI, CreatePolygonRgn);
    DECLATTACH(hInstUsr, CreatePopupMenu);
    DECLATTACH(hInstGDI, CreateRectRgn);
    DECLATTACH(hInstGDI, CreateRectRgnIndirect);
    DECLATTACH(hInstGDI, CreateRoundRectRgn);
    DECLATTACH(hInstGDI, CreateSolidBrush);

    DECLATTACH(hInstGDI, DeleteColorSpace);
    DECLATTACH(hInstGDI, DeleteDC);
    DECLATTACH(hInstGDI, DeleteEnhMetaFile);
    DECLATTACH(hInstGDI, DeleteMetaFile);
    DECLATTACH(hInstGDI, DeleteObject);
    DECLATTACH(hInstUsr, DestroyAcceleratorTable);
    DECLATTACH(hInstUsr, DestroyCursor);
    DECLATTACH(hInstUsr, DestroyIcon);
    DECLATTACH(hInstUsr, DestroyMenu);

    DECLATTACH(hInstUsr, EndPaint);
    DECLATTACH(hInstGDI, ExtCreatePen);
    DECLATTACH(hInstGDI, ExtCreateRegion);
    DECLATTACH(hInstShl, ExtractAssociatedIconA);
    DECLATTACH(hInstShl, ExtractAssociatedIconW);
    DECLATTACH(hInstShl, ExtractIconA);
    DECLATTACH(hInstShl, ExtractIconW);
    DECLATTACH(hInstShl, ExtractIconExA);
    DECLATTACH(hInstShl, ExtractIconExW);

    DECLATTACH(hInstUsr, GetDC);
    DECLATTACH(hInstUsr, GetDCEx);
    DECLATTACH(hInstGDI, GetEnhMetaFileA);
    DECLATTACH(hInstGDI, GetEnhMetaFileW);
    DECLATTACH(hInstGDI, GetMetaFileA);
    DECLATTACH(hInstGDI, GetMetaFileW);
    DECLATTACH(hInstUsr, GetWindowDC);

    DECLATTACH(hInstUsr, LoadAcceleratorsA);
    DECLATTACH(hInstUsr, LoadAcceleratorsW);
    DECLATTACH(hInstUsr, LoadBitmapA);
    DECLATTACH(hInstUsr, LoadBitmapW);
    DECLATTACH(hInstUsr, LoadCursorA);
    DECLATTACH(hInstUsr, LoadCursorW);
    DECLATTACH(hInstUsr, LoadCursorFromFileA);
    DECLATTACH(hInstUsr, LoadCursorFromFileW);
    DECLATTACH(hInstUsr, LoadIconA);
    DECLATTACH(hInstUsr, LoadIconW);
    DECLATTACH(hInstUsr, LoadImageA);
    DECLATTACH(hInstUsr, LoadImageW);
    DECLATTACH(hInstUsr, LoadMenuA);
    DECLATTACH(hInstUsr, LoadMenuW);
    DECLATTACH(hInstUsr, LoadMenuIndirectA);
    DECLATTACH(hInstUsr, LoadMenuIndirectW);

    DECLATTACH(hInstUsr, ReleaseDC);
    DECLATTACH(hInstGDI, SetMetaFileBitsEx);
    DECLATTACH(hInstGDI, SetWinMetaFileBits);
    DECLATTACH(hInstShl, SHGetFileInfoA);
    DECLATTACH(hInstShl, SHGetFileInfoW);

    return S_OK;
}



void TryGetKernelBaseAddrs(HMODULE hmodule)
{
    Real_VirtualAllocEx = (LPVOID(WINAPI *)(HANDLE, LPVOID, SIZE_T, DWORD, DWORD))GetProcAddress(hmodule, "VirtualAllocEx");
    Real_VirtualFreeEx = (BOOL(WINAPI *)(HANDLE, LPVOID, SIZE_T, DWORD))GetProcAddress(hmodule, "VirtualFreeEx");

    Real_VirtualAlloc = (LPVOID(WINAPI *)(LPVOID, SIZE_T, DWORD, DWORD))GetProcAddress(hmodule, "VirtualAlloc");
    Real_VirtualFree = (BOOL(WINAPI *)(LPVOID, SIZE_T, DWORD))GetProcAddress(hmodule, "VirtualFree");
}

void InitDetours(BOOL fTrackVirtualMem, BOOL fTrackHeap, int nSharedMemSize)
{
    VSASSERT(g_TrackMemClass == NULL, "InitDetours not reentrant");
    // let's create a private heap to track stuff
    // The name of this heap is used by VBDiagMargin

    // use placement new for private heap
    g_TrackMemClass = new (DebugAlloc(sizeof(TrackMemClass))) TrackMemClass();
    //
    HMODULE hNTDll = GetModuleHandle("ntdll");
    Real_RtlCreateHeap = (PVOID(WINAPI *)(ULONG, PVOID, SIZE_T, SIZE_T, PVOID, PRTL_HEAP_PARAMETERS))GetProcAddress(hNTDll, "RtlCreateHeap");
    Real_RtlDestroyHeap = (PVOID(WINAPI *)(PVOID))GetProcAddress(hNTDll, "RtlDestroyHeap");
    Real_RtlFreeHeap = (BOOL(WINAPI *)(HANDLE, DWORD, LPVOID))GetProcAddress(hNTDll, "RtlFreeHeap");
    Real_RtlAllocHeap = (LPVOID(WINAPI *)(HANDLE, DWORD, SIZE_T))GetProcAddress(hNTDll, "RtlAllocateHeap");
    Real_RtlLockHeap = (BOOL(WINAPI *)(HANDLE))GetProcAddress(hNTDll, "RtlLockHeap");
    Real_RtlUnlockHeap = (BOOL(WINAPI *)(HANDLE))GetProcAddress(hNTDll, "RtlUnlockHeap");
    Real_RtlValidateHeap = (BOOL(WINAPI *)(HANDLE, DWORD, LPCVOID))GetProcAddress(hNTDll, "RtlValidateHeap");

    //    Real_HeapCreate  = (HANDLE (__stdcall *)(DWORD,SIZE_T,SIZE_T))GetProcAddress(hNTDll, "RtlCreateHeap");
    //    Real_HeapDestroy= (BOOL(__stdcall *)(HANDLE))GetProcAddress(hNTDll, "RtlDestroyHeap");
    VSASSERT(Real_HeapCreate &&
        Real_HeapDestroy &&
        Real_RtlCreateHeap &&
        Real_RtlDestroyHeap &&
        Real_RtlLockHeap &&
        Real_RtlUnlockHeap &&
        Real_RtlValidateHeap, "couldn't get Rtl*Heap");



    Real_ZwAllocateVirtualMemory = (NTSTATUS(WINAPI *)(HANDLE, PVOID *, ULONG_PTR, PSIZE_T, ULONG, ULONG))GetProcAddress(hNTDll, "ZwAllocateVirtualMemory");
    Real_ZwFreeVirtualMemory = (NTSTATUS(WINAPI *)(HANDLE, PVOID *, PSIZE_T, ULONG))GetProcAddress(hNTDll, "ZwFreeVirtualMemory");

    VSASSERT(Real_ZwAllocateVirtualMemory && Real_ZwFreeVirtualMemory, "couldn't get ZwAllocateVirtualMemory and Real_ZwFreeVirtualMemory ");

    Real_ZwClose = (NTSTATUS(WINAPI *)(HANDLE))GetProcAddress(hNTDll, "ZwClose");

    Real_ZwCreateFile = (NTSTATUS(WINAPI *)(
        PHANDLE,
        ACCESS_MASK,
        POBJECT_ATTRIBUTES,
        PIO_STATUS_BLOCK,
        PLARGE_INTEGER,
        ULONG,
        ULONG,
        ULONG,
        ULONG,
        PVOID,
        ULONG))GetProcAddress(hNTDll, "ZwCreateFile");

    Real_ZwOpenFile = (NTSTATUS(WINAPI*)(
        PHANDLE,
        ACCESS_MASK,
        POBJECT_ATTRIBUTES,
        PIO_STATUS_BLOCK,
        ULONG,
        ULONG
        ))GetProcAddress(hNTDll, "ZwOpenFile");


    VSASSERT(Real_ZwOpenFile && Real_ZwClose && Real_ZwCreateFile, "Real_ZwOpenFile, Real_ZwClose, Real_ZwCreateFile not found");

    Real_ZwOpenSection = (NTSTATUS(WINAPI *)(
        PHANDLE,
        ACCESS_MASK,
        POBJECT_ATTRIBUTES
        ))GetProcAddress(hNTDll, "ZwOpenSection");

    Real_ZwCreateSection = (NTSTATUS(WINAPI *)(
        PHANDLE,
        ACCESS_MASK,
        POBJECT_ATTRIBUTES,
        PLARGE_INTEGER,
        ULONG,
        ULONG,
        HANDLE
        ))GetProcAddress(hNTDll, "ZwCreateSection");

    Real_ZwMapViewOfSection = (NTSTATUS(WINAPI *)(
        HANDLE,
        HANDLE,
        PVOID *,
        ULONG_PTR,
        SIZE_T,
        PLARGE_INTEGER,
        PSIZE_T,
        SECTION_INHERIT,
        ULONG,
        ULONG
        ))GetProcAddress(hNTDll, "ZwMapViewOfSection");

    Real_ZwUnmapViewOfSectionEx = (NTSTATUS(WINAPI *)(
        HANDLE,
        PVOID,
        ULONG
        ))GetProcAddress(hNTDll, "ZwUnmapViewOfSectionEx");

    VSASSERT(Real_ZwOpenSection && Real_ZwMapViewOfSection && Real_ZwUnmapViewOfSectionEx && Real_ZwCreateSection, "Real_ZwOpenSection && Real_ZwMapViewOfSection &&  Real_ZwUnmapViewOfSection && Real_ZwCreateSection not found");


    HMODULE hKernelBase = GetModuleHandle("kernelbase");
    if (hKernelBase != INVALID_HANDLE_VALUE) // on Win7 kernel32::VirtualAllocEx is a stub which calls (doesn't forward) kernelbase::VirtualAllocEx
    {
        auto pGlobalAlloc = (HGLOBAL(__stdcall *)(UINT, SIZE_T))GetProcAddress(hKernelBase, "GlobalAlloc");
        if (pGlobalAlloc)
        {
            Real_GlobalAlloc = pGlobalAlloc;
        }
        auto pGlobalReAlloc = (HGLOBAL(WINAPI *)(HGLOBAL, SIZE_T, UINT))GetProcAddress(hKernelBase, "GlobalReAlloc");
        if (pGlobalReAlloc)
        {
            Real_GlobalReAlloc = pGlobalReAlloc;
        }
        auto pGlobalFree = (HGLOBAL(WINAPI *)(HGLOBAL))GetProcAddress(hKernelBase, "GlobalFree");
        if (pGlobalAlloc)
        {
            Real_GlobalFree = pGlobalFree;
        }
        auto pLocalAlloc = (HLOCAL(WINAPI *)(UINT, SIZE_T))GetProcAddress(hKernelBase, "LocalAlloc");
        if (pLocalAlloc)
        {
            Real_LocalAlloc = pLocalAlloc;
        }
        auto pLocalReAlloc = (HLOCAL(WINAPI *)(HLOCAL, SIZE_T, UINT))GetProcAddress(hKernelBase, "LocalReAlloc");
        if (pLocalReAlloc)
        {
            Real_LocalReAlloc = pLocalReAlloc;
        }
        auto pLocalFree = (HLOCAL(WINAPI *)(HLOCAL))GetProcAddress(hKernelBase, "LocalFree");
        if (pLocalAlloc)
        {
            Real_LocalFree = pLocalFree;
        }
        auto pLocalSize = (SIZE_T(WINAPI *)(HLOCAL))GetProcAddress(hKernelBase, "LocalSize");
        if (pLocalSize)
        {
            Real_LocalSize = pLocalSize;
        }

        TryGetKernelBaseAddrs(hKernelBase);
    }
    HMODULE hK32 = GetModuleHandle("kernel32");
    if (!(Real_VirtualAllocEx && Real_VirtualFreeEx && Real_VirtualAlloc && Real_VirtualFree))
    {
        TryGetKernelBaseAddrs(hK32);
    }
    VSASSERT(Real_VirtualAllocEx && Real_VirtualFreeEx && Real_VirtualAlloc && Real_VirtualFree, "Real_VirtualAllocEx, Real_VirtualFreeEx not found in kernelbase");


    if (!g_NativeOnly)
    {
        //;Set COR_ENABLE_PROFILING=1
        //;Set COR_PROFILER={01673DDC-46F5-454F-84BC-F2F34564C2AD}
        //;Set COR_PROFILER_PATH=D:\MemSpect\MemSpectDll.dll
        SetEnvironmentVariableA("COR_ENABLE_PROFILING", "1");
        SetEnvironmentVariableA("COR_PROFILER", "{01673DDC-46F5-454F-84BC-F2F34564C2AD}");
        SetEnvironmentVariableA("COR_PROFILER_PATH", g_szVSAssertDllFullPathName);
        SetEnvironmentVariableA("_NO_DEBUG_HEAP", "1");

    }

    if (g_isImmersive)
    {

        WCHAR packageFamilyName[MAX_PATH];
        UINT32 dwPackageFullNameLength = MAX_PATH;
        BYTE  pBuffer[MAX_PATH];
        WCHAR  tempBuffer[MAX_PATH] = L"";
        PACKAGE_INFO_REFERENCE  package_Info_Reference;
        LONG res = g_PFNGetPackageFullName(GetCurrentProcess(), &dwPackageFullNameLength, tempBuffer);

        if (res == 0 && tempBuffer[0])
        {
            int nLen = 2 * wcslen(tempBuffer) + 2;
            g_packageFullName = (WCHAR *)DebugAlloc(nLen);
            memcpy(g_packageFullName, tempBuffer, nLen);

            GetPackageFamilyNameFromProcess(GetCurrentProcess(), packageFamilyName);


            g_PFNGetPackageId = (PFNGetPackageId)GetProcAddress(hK32, "GetPackageId");
            dwPackageFullNameLength = MAX_PATH;
            g_PFNGetPackageId(GetCurrentProcess(), &dwPackageFullNameLength, pBuffer);


            g_PFNOpenPackageInfoByFullName = (PFNOpenPackageInfoByFullName)GetProcAddress(hK32, "OpenPackageInfoByFullName");
            g_PFNOpenPackageInfoByFullName(g_packageFullName, 0, package_Info_Reference);
            HRESULT hr = GetPathOfAppContainerNamedObject(packageFamilyName, tempBuffer);
            VSASSERT(hr == S_OK, "Couldn't get g_AppContainerNamedObjectPath");
            nLen = 2 * wcslen(tempBuffer) + 2;
            g_AppContainerNamedObjectPath = (WCHAR *)DebugAlloc(nLen);
            memcpy(g_AppContainerNamedObjectPath, tempBuffer, nLen);
        }
        else
        {
#ifndef APPMODEL_ERROR_NO_PACKAGE
#define APPMODEL_ERROR_NO_PACKAGE 15700L  // from Win8 Winerror.h
#endif
            VSASSERTF((res == APPMODEL_ERROR_NO_PACKAGE, "unknown retval from GetPackageFullName %x", res));
        }
    }




    //
    g_TrackMemClass->InitDetours(fTrackVirtualMem, fTrackHeap, nSharedMemSize);

    if (g_AppContainerNamedObjectPath[0])
    {
        // we want to resume the suspended app
        //		PackageResumeSuspend(g_packageFullName, /*nResume=*/1); //can't from within DLLMain ?
    }

}

void ShutDownDetours()
{
    if (g_TrackMemClass)
    {
        CDisableTrace lock;
        delete g_TrackMemClass;
        g_TrackMemClass = 0;
    }
    if (CHeapSpy::g_AssertOnSeqNo)
    {
        CHeapSpy::g_AssertOnSeqNo->freemem();
        CHeapSpy::g_AssertOnSeqNo = 0;
    }
    if (CHeapSpy::g_AssertOnStackFrame)
    {
        CHeapSpy::g_AssertOnStackFrame->freemem();
        CHeapSpy::g_AssertOnStackFrame = 0;
    }

    Framewalker::ShutDownFramewalker();
}

// Calls DetourUpdateThread on all the threads of this process except the current thread and
// returns the handles of the threads to close them once the detour transaction is committed.
// DetourUpdateThread suspends the thread and saves the handle to resume it when DetourTransactionCommit
// is called so till then we need to keep the handles alive.
LONG DetourUpdateAllThreads(std::vector<HANDLE> &threadHandles)
{
    // Take a snapshot of all the threads in the system.
    HANDLE hSnapshot = CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, /* th32ProcessID */ 0); // th32ProcessID is ignored when TH32CS_SNAPTHREAD is passed
    if (hSnapshot == INVALID_HANDLE_VALUE)
    {
        return ERROR_INVALID_HANDLE;
    }
    SCOPE_GUARD(CloseHandle(hSnapshot));

    DWORD dwProcessId = GetCurrentProcessId();
    DWORD dwCurrentThreadId = GetCurrentThreadId();

    // Iterate over the threads in the system
    THREADENTRY32 threadEntry = { 0 };
    threadEntry.dwSize = sizeof(THREADENTRY32);
    if (Thread32First(hSnapshot, &threadEntry))
    {
        do
        {
            // If the current thread is owned by this process and it is not the current thread then call DetourUpdateThread
            if (threadEntry.th32OwnerProcessID == dwProcessId && threadEntry.th32ThreadID != dwCurrentThreadId)
            {
                HANDLE threadHandle = OpenThread(THREAD_SUSPEND_RESUME, /* bInheritHandle */ FALSE, threadEntry.th32ThreadID);
                // It is possible that the thread is gone since the snapshot so check if we successfully retrieved the handle.
                if (threadHandle != nullptr)
                {
                    (void)DetourUpdateThread(threadHandle); // We want to continue with the rest of the threads if a particular thread cannot be updated

                    try
                    {
                        // Save the thread handle to close it after the detour transaction is over.
                        threadHandles.push_back(threadHandle);
                    }
                    catch (std::bad_alloc)
                    {
                        return ERROR_OUTOFMEMORY;
                    }
                }
            }

            threadEntry.dwSize = sizeof(THREADENTRY32); // refresh the size field as Thread32Next can overwrite it.
        } while (Thread32Next(hSnapshot, &threadEntry));
    }
    else
    {
        // Cannot access any threads so report the error.
        return GetLastError();
    }

    return NO_ERROR;
}

void TrackMemClass::InitDetours(BOOL fTrackVirtualMem, BOOL fTrackHeap, int nSharedMemSize)
{
    //	StartATraceSession();
    HeapSetInformation(GetProcessHeap(), HeapEnableTerminationOnCorruption, NULL, 0);
    auto hLibNtDll = GetModuleHandleA("ntdll.dll");
    if (hLibNtDll)
    {
        g_PFNtQueryInformationThread = (NTSTATUS(WINAPI *)(HANDLE, THREAD_INFORMATION_CLASSx, PVOID, ULONG, PULONG))   GetProcAddress(hLibNtDll, "NtQueryInformationThread");
}
    // try to read default options from <vsassert-path>\MemSpect.ini
    char szDrive[_MAX_DRIVE];
    char szDir[_MAX_DIR];
    char szName[_MAX_FNAME];
    _splitpath(g_szVSAssertDllFullPathName, szDrive, szDir, szName, NULL);
    {
        DetourTransactionBegin();
        std::vector<HANDLE> threadHandles;
        DetourUpdateAllThreads(threadHandles);
        SCOPE_GUARD(std::for_each(threadHandles.begin(), threadHandles.end(), [](HANDLE h) { CloseHandle(h); }));

        if (fTrackVirtualMem)
        {
            ATTACH(&(PVOID&)Real_ZwAllocateVirtualMemory, Mine_ZwAllocateVirtualMemory);
            ATTACH(&(PVOID&)Real_ZwFreeVirtualMemory, Mine_ZwFreeVirtualMemory);
            /*
            ATTACH(&(PVOID&)Real_VirtualAllocEx, Mine_VirtualAllocEx);
            ATTACH(&(PVOID&)Real_VirtualAlloc, Mine_VirtualAlloc);
            ATTACH(&(PVOID&)Real_VirtualFreeEx, Mine_VirtualFreeEx);
            ATTACH(&(PVOID&)Real_VirtualFree, Mine_VirtualFree);
            */
            ATTACH(&(PVOID&)Real_MapViewOfFile, Mine_MapViewOfFile);
            ATTACH(&(PVOID&)Real_MapViewOfFileEx, Mine_MapViewOfFileEx);
            ATTACH(&(PVOID&)Real_UnmapViewOfFile, Mine_UnmapViewOfFile);
            ATTACH(&(PVOID&)Real_UnmapViewOfFileEx, Mine_UnmapViewOfFileEx);
        }
        if (g_fTrackGDI > 0)
        {
            auto res = InitGdiTrack();
#if MSDEBUG
            VSASSERT(res == S_OK, "Error detouring GDI objs");
#endif MSDEBUG

        }

        if (fTrackHeap)
        {
#if USE_RTL_CREATE_HEAP

            ATTACH(&(PVOID&)Real_RtlCreateHeap, Mine_RtlCreateHeap);
            ATTACH(&(PVOID&)Real_RtlDestroyHeap, Mine_RtlDestroyHeap);
#else
            ATTACH(&(PVOID&)Real_HeapCreate, Mine_HeapCreate);
            ATTACH(&(PVOID&)Real_HeapDestroy, Mine_HeapDestroy);
#endif USE_RTL_CREATE_HEAP


            /*
            ATTACH(&(PVOID&)Real_HeapAlloc, Mine_HeapAlloc);
            ATTACH(&(PVOID&)Real_HeapFree, Mine_HeapFree);
            ATTACH(&(PVOID&)Real_HeapLock, Mine_HeapLock);
            ATTACH(&(PVOID&)Real_HeapUnlock, Mine_HeapUnlock);
            /*/
            ATTACH(&(PVOID&)Real_RtlAllocHeap, Mine_RtlAllocHeap);
            ATTACH(&(PVOID&)Real_RtlFreeHeap, Mine_RtlFreeHeap);
            ATTACH(&(PVOID&)Real_RtlLockHeap, Mine_RtlLockHeap);
            ATTACH(&(PVOID&)Real_RtlUnlockHeap, Mine_RtlUnlockHeap);
            ATTACH(&(PVOID&)Real_RtlValidateHeap, Mine_RtlValidateHeap);
            //*/
            ATTACH(&(PVOID&)Real_HeapReAlloc, Mine_HeapReAlloc);
            ATTACH(&(PVOID&)Real_HeapValidate, Mine_HeapValidate);
            ATTACH(&(PVOID&)Real_HeapSize, Mine_HeapSize);
            ATTACH(&(PVOID&)Real_HeapCompact, Mine_HeapCompact);
            ATTACH(&(PVOID&)Real_HeapQueryInformation, Mine_HeapQueryInformation);
            ATTACH(&(PVOID&)Real_HeapSetInformation, Mine_HeapSetInformation);
            ATTACH(&(PVOID&)Real_HeapWalk, Mine_HeapWalk);

            ATTACH(&(PVOID&)Real_GlobalAlloc, Mine_GlobalAlloc);
            ATTACH(&(PVOID&)Real_GlobalFree, Mine_GlobalFree);
            ATTACH(&(PVOID&)Real_GlobalReAlloc, Mine_GlobalReAlloc);
            ATTACH(&(PVOID&)Real_LocalAlloc, Mine_LocalAlloc);
            ATTACH(&(PVOID&)Real_LocalFree, Mine_LocalFree);
            ATTACH(&(PVOID&)Real_LocalSize, Mine_LocalSize);
            ATTACH(&(PVOID&)Real_LocalReAlloc, Mine_LocalReAlloc);

            ATTACH(&(PVOID&)Real_TlsAlloc, Mine_TlsAlloc);
            ATTACH(&(PVOID&)Real_TlsFree, Mine_TlsFree);

        }

        if (g_fTrackThreadCreate)
        {
            ATTACH(&(PVOID&)Real_CreateThread, Mine_CreateThread);
            //ATTACH(&(PVOID&)Real_CloseHandle, Mine_CloseHandle);

            ATTACH(&(PVOID&)Real_ZwCreateFile, Mine_ZwCreateFile);
            ATTACH(&(PVOID&)Real_ZwOpenFile, Mine_ZwOpenFile);
            ATTACH(&(PVOID&)Real_ZwOpenSection, Mine_ZwOpenSection);
            ATTACH(&(PVOID&)Real_ZwCreateSection, Mine_ZwCreateSection);
            ATTACH(&(PVOID&)Real_ZwMapViewOfSection, Mine_ZwMapViewOfSection);
            ATTACH(&(PVOID&)Real_ZwUnmapViewOfSectionEx, Mine_ZwUnmapViewOfSectionEx);


            ATTACH(&(PVOID&)Real_ZwClose, Mine_ZwClose);

        }
		if (g_TrackETW)
		{
			ATTACH(&(PVOID&)Real_TraceEvent, Mine_TraceEvent);

			ATTACH(&(PVOID&)Real_EventWrite, Mine_EventWrite);

			ATTACH(&(PVOID&)Real_EventWriteString, Mine_EventWriteString);
		}
		HMODULE hCodeMarkerInst = GetModuleHandleA("Microsoft.VisualStudio.CodeMarkers.Dll");
		if (hCodeMarkerInst == nullptr)
		{
			hCodeMarkerInst = GetModuleHandleA("Microsoft.Internal.Performance.CodeMarkers.Dll");
		}
		if (hCodeMarkerInst != nullptr)
		{
			Real_PerfCodeMarker = (PFCODEMARKER)GetProcAddress(hCodeMarkerInst, "_PerfCodeMarker@12");
			if (Real_PerfCodeMarker != 0)
			{
				auto res = AddAtomW(L"VSCodeMarkersEnabled");
				VSASSERTF((res != 0, "AddAtom failed for codemarkers Err = %d File %s(%d)", GetLastError(), __FILE__, __LINE__));
				ATTACH(&(PVOID&)Real_PerfCodeMarker, Mine_PerfCodeMarker);
			}

		}

        if (g_fTrackLoadResource)
        {
            ATTACH(&(PVOID&)Real_LoadResource, Mine_LoadResource);
        }

#if 0
        ATTACH(&(PVOID&)Real_RegCreateKeyEx, Mine_RegCreateKeyEx);
        ATTACH(&(PVOID&)Real_RegOpenKeyEx, Mine_RegOpenKeyEx);
        ATTACH(&(PVOID&)Real_RegCloseKey, Mine_RegCloseKey);
#endif 0

        DetourTransactionCommit();
        }
    if (g_fHandle4gigStacks)
    {
        Framewalker::g_StackFrameMapWrapper = new (DebugAlloc(sizeof(Framewalker::StackFrameMapWrapper)))
            Framewalker::StackFrameMapWrapper(MySTLAlloc<pair<LPVOID, StackFrameIndex > >(InternalHeapToUse));
    }

    // possible race condition/stack ovflo when debugging and not _NO_DEBUG_HEAP: see 
    // "You can develop code faster" http://blogs.msdn.com/b/calvin_hsia/archive/2009/12/16/9938003.aspx
    GetThreadData(/*fCreate=*/ true); // Force TLS value to be set, causing detouring to start working via CDisableTrace

    if (!g_fIsChildProcess) // we don't want the child proc to create a child proc
    {
        // now see if there's an EXE child process to start
        WCHAR szChildExeName[MAX_PATH];

        // create starting EXE filename
        swprintf_s(szChildExeName, L"%S%SMemSpect.Exe", szDrive, szDir);

        m_CreateChildProcess.StartCreateChildProcess(szChildExeName, nSharedMemSize); // we want to call this even if StartChildProcess==0 so  pipes/events are created
        CComBSTR strStart = "MemSpect Initialized";
        BSTR msgStart = strStart.Detach(); // intentional leak
    }
    if (g_TrackFileLoadUnload && hLibNtDll)
    {

        LdrRegisterDllNotificationFunction pRegisterFunc = (LdrRegisterDllNotificationFunction)GetProcAddress(hLibNtDll, "LdrRegisterDllNotification");
        // Failed to find the proc address of LdrRegisterDllNotification
        if (pRegisterFunc != nullptr)
        {
            void * dwCookie;
            NTSTATUS registerStatus = pRegisterFunc(0, MyLdrNotificationCallback, nullptr, &dwCookie);
            // Ensure sucessful registration. 
            if (registerStatus != STATUS_SUCCESS)
            {
                VSASSERT(false, "failed to LdrRegisterDllNotification");
            }
        }
    }


    }


void TrackMemClass::ShutDownDetours()
{
    // we don't need to restore detoured funcs because process is going away, but we do need to destroy our private heap

    if (m_MemSpectHeap)
    {
        if (m_ListTrkBlocks)
        {
            m_ListTrkBlocks->freemem();
        }
        // read the THIS pointer before we free
        HANDLE hHeap = (HANDLE)m_MemSpectHeap;
        // we're freeing the THIS:
        DebugFree(this);

        VsDebHeapDestroy(hHeap, /*fCheckforLeaks*/false);
        // after this point,we have no THIS
    }
}


void TMUpdateLoadedModules()
{
    LockCritSect;
    UpdateLoadedModules();
}

#define g_ftrackClassInstances  1
#if MSDEBUG

CComBSTR GetTrkBlkInfoForObj(ObjectID objectID, char *szDesc, TrkBlock *pTrkBlock)
{
    USES_CONVERSION;
    WCHAR wszTemp[500] = { 0 };
    WCHAR wszClassName[500] = { 0 };
    CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour

    PVOID ptrAlloc = ((DWORD *)pTrkBlock) - Offset_AllocToTblk; // ptr to raw allocation
    CHeapSpy *pHeapSpy = g_TrackMemClass->m_MemSpectHeap;
    CAddressNode *pAddrNode = pHeapSpy->FindInst(ptrAlloc);
    CComBSTR bstrInfo(szDesc);

    GetClassNameFromClassId(wszClassName, dimensionof(wszClassName), pTrkBlock->ClrObject.m_ClassId, objectID);
    swprintf_s(wszTemp, dimensionof(wszTemp), L"ID= %08x %s, Gen=%d, Mov=%d, Surv=%d  SeqNo=%d\r\n",
        objectID,
        wszClassName,
        pTrkBlock->ClrObject.m_nGen,
        pTrkBlock->ClrObject.m_nMoved,
        pTrkBlock->ClrObject.m_nGCsSurvived,
        pAddrNode->m_cAlloc
    );

    bstrInfo.Append(wszTemp);
    if (g_StackStorageMode == InMemory)
    {
        for (UINT i = 0; i < pAddrNode->m_uicStackAddr; i++)
        {
            char szBuff[1000];
            GetStringFromAddr(pAddrNode->m_pdwStackAddr[i], szBuff, sizeof(szBuff));
            bstrInfo.Append(L"\r\n");
            bstrInfo.Append(A2W(szBuff));
        }
    }
    return bstrInfo;
}
#endif MSDEBUG

ListTrkBlocks::iterator CProcessGC::AddOrFindClassId(ClassID classID, DWORD dwSize)
{
    auto clsKey = TreeKey(bt_ClrClass, (LPVOID)classID);
    ListTrkBlocks *thelist = g_TrackMemClass->GetList();
    auto resClass = thelist->find(clsKey);
    if (resClass == thelist->end())
    {
        TrkBlock oTrkBlockCls(dwSize);
        oTrkBlockCls.ClrClass.m_ClassId = classID;
        g_TrackMemClass->InsertIntoList(PairAddrBlock(clsKey, oTrkBlockCls), /*nExtraFramesToSkip =*/1);
        resClass = thelist->find(clsKey);
    }
    return resClass;
}


STDMETHODIMP CProcessGC::TrackObject(BlockType bt, DWORD dwObjectId, DWORD dwSize, DWORD dwAction /*=0*/)
{
    if (!g_TrackMemClass || !g_TrackClrObjects)
    {
        return S_OK;
    }
    LockCritSect;
    ListTrkBlocks *thelist = g_TrackMemClass->GetList();
    CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour

    int nExtraFramesToSkip = 1;
    TrkBlock oTrkBlk(dwSize);
    bool fDidGetSystemString = false;
    switch (bt)
    {
    case bt_ClrObject:
    {
        oTrkBlk.ClrObject.m_ClassId = dwAction;

        auto clsTrkBlk = AddOrFindClassId(dwAction, dwSize);

        (*clsTrkBlk).second.ClrClass.m_nInstance++; // bump instance count
#if MSDEBUG
        nExtraFramesToSkip = 2;
#else
        nExtraFramesToSkip = 2;
#endif
        // if we didn't get System.String classId yet, lets get it:
        if (m_SystemStringClassId == 0)
        {
            WCHAR tempClassName[100]; // only needs to check for "System.String", so doesn't need to be very big
            int nlen = GetClassNameFromClassId(tempClassName, dimensionof(tempClassName), dwAction, dwObjectId, /*fExpandSystemString= */ false);
            if (nlen == 13 && wcscmp(tempClassName, L"System.String") == 0)
            {
                m_SystemStringClassId = dwAction; // got it. it won't change. 
                fDidGetSystemString = true;
            }

        }
        g_memStats.ClrObjs.Alloc.count++;
        g_memStats.ClrObjs.Alloc.size += dwSize;
    }
    break;
    case bt_ClrClass:
    {
        oTrkBlk.ClrClass.m_ClassId = dwObjectId;
        g_memStats.ClrClasses.Alloc.count++;
        g_memStats.ClrClasses.Alloc.size += dwSize;
    }
    break;
    default: // module, assembly,appdomain
        oTrkBlk.ClrLoads.m_data = dwObjectId;
        g_memStats.ClrOther.Alloc.count++;
        g_memStats.ClrOther.Alloc.size += dwSize;
        nExtraFramesToSkip = 2;
        break;
    }
    auto treekey = TreeKey(bt, (LPVOID)dwObjectId);
    auto existalready = thelist->find(treekey);
    if (existalready != thelist->end()) // should not exist already. Else assert
    {
#if MSDEBUG
        if (bt == bt_ClrObject)
        {
            USES_CONVERSION;
            TrkBlock *pTrkBlock = &existalready->second;
            CComBSTR bstrTemp = GetTrkBlkInfoForObj(dwObjectId, "TrackObj: object slot already occupied: replacing:  This assert can be safely ignored\r\n", pTrkBlock);
            VSASSERT(false, W2A(bstrTemp));
        }
        else
        {
            VSASSERTF((false, "TrackObj tree slot already occupied %d  File%s(%d). This assert can be safely ignored", bt, __FILE__, __LINE__));
        }
#endif MSDEBUG
        thelist->erase(existalready);
    }

    auto resPairIB = g_TrackMemClass->InsertIntoList(PairAddrBlock(treekey, oTrkBlk), nExtraFramesToSkip);


    if (fDidGetSystemString)
    {
        // now insert into list so we can accum GC data
        TrkBlock oTrkBlkSystemString(0);//size = 0
        oTrkBlkSystemString.ClrClass.m_ClassId = dwAction;
        g_TrackMemClass->InsertIntoList(PairAddrBlock(TreeKey(bt_ClrClass, (LPVOID)dwAction), oTrkBlkSystemString), nExtraFramesToSkip);
    }

    VSASSERT(resPairIB.second == true, "failed to insert CLR obj or class for TrackObject");
    return S_OK;
}

STDMETHODIMP  CProcessGC::TrackObjectFree(BlockType bt, DWORD dwObjectId) // not bt_ClrObject
{
    HRESULT hr = E_FAIL;
    VSASSERT(bt != bt_ClrObject, "objs are GC'd, not freed");
    if (!g_TrackClrObjects)
    {
        hr = S_OK;
    }
    else
    {
        if (CDisableTrace::CanDetour()) // if we're not shutting down
        {
            CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
            LockCritSect;
            ListTrkBlocks *thelist = g_TrackMemClass->GetList();
            auto res = thelist->find(TreeKey(bt, (LPVOID)dwObjectId));
            if (res != thelist->end()) // if it exists 
            {
                thelist->erase(res); // delete it
                hr = S_OK;
            }
        }
    }
    return hr;
}

STDMETHODIMP CProcessGC::JITCompilationFinished(FunctionID functionID, HRESULT hrStatus, BOOL fIsSafeToBlock)
{
    if (!g_TrackClrObjects)
    {
        return S_OK;
    }
    VSASSERTF((g_TrackJit != 0, "why are we getting JIT call when we're not tracking jit?"));
    // can't call GetFunctionOrClassNameFromFunctionId due to deadlock, so we'll store the functionID
    LockCritSect;
    CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour

    s_nJitNumber++;
    auto keyNew = TreeKey(bt_Jit, (LPVOID)g_pCProcessGC->s_nJitNumber);
    TrkBlock oTrkBlock(0); // 0 size for jit
    oTrkBlock.JitInfo.m_functionId = functionID;
    oTrkBlock.JitInfo.m_fIsSafeToBlock = fIsSafeToBlock;

    auto resInsert = g_TrackMemClass->InsertIntoList(PairAddrBlock(keyNew, oTrkBlock),/*nExtraFramesToSkip =*/ 2);
    VSASSERT(resInsert.second == true, "failed to insert jit obj into tree");
    // now we have the functionID of the caller
    if (g_DynStackSymRes)
    {
        AddToDynStackSymNames(functionID, /*bool fIsFunctionId=*/ true);
    }

    //char szbuf[1000];
    //auto result = GetFunctionOrClassNameFromFunctionId(functionID,  szbuf, sizeof(szbuf), /*fIncludeModuleName =*/true);
    ////OutputDebugString(szbuf);
    ////OutputDebugString("\r\n");
    //VSASSERTF((g_TrackJit != 0,"why are we getting JIT call when we're not tracking jit?"));
    //LockCritSect;
    //CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
    //auto thelist = g_TrackMemClass->GetList();

    //s_nJitNumber++;
    //auto keyNew = TreeKey(bt_Jit, (LPVOID)g_pCProcessGC->s_nJitNumber);
    //TrkBlock oTrkBlock(0); // 0 size for jit
    //CComBSTR bstrJit(szbuf);
    //auto nLen =2* bstrJit.Length();
    //oTrkBlock.JitInfo.pCIndirectInfo = CIndirectInfo::CreateInstance(IIType_Jit, nLen);
    //memcpy(&oTrkBlock.JitInfo.pCIndirectInfo->m_pwChar, bstrJit.m_str, nLen);

    //auto resInsert = g_TrackMemClass->InsertIntoList(PairAddrBlock(keyNew, oTrkBlock));
    //VSASSERT(resInsert.second==true,"failed to insert jit obj into tree");
    return S_OK;
}


STDMETHODIMP CProcessGC::ExceptionThrown(ObjectID objectID)
{
    if (!g_TrackClrObjects)
    {
        return S_OK;
    }
    LockCritSect;
    CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour

    s_nExcptNumber++;
    auto keyNew = TreeKey(bt_Excpt, (LPVOID)s_nExcptNumber);
    ClassID classID;
    ULONG nSize;
    g_pCorProfilerInfo->GetObjectSize(objectID, &nSize);
    g_pCorProfilerInfo->GetClassFromObject(objectID, &classID);
    TrkBlock oTrkBlock(nSize);
    oTrkBlock.ExcptInfo.objectIDExcept = objectID;
    oTrkBlock.ExcptInfo.classIDExcept = classID;

    auto resInsert = g_TrackMemClass->InsertIntoList(PairAddrBlock(keyNew, oTrkBlock),/*nExtraFramesToSkip =*/ 1);
    VSASSERT(resInsert.second == true, "failed to insert Excpt obj into tree");
    return S_OK;
}


STDMETHODIMP CProcessGC::HandleCreated(GCHandleID handleId, ObjectID initialObjectId)
{
    if (!g_TrackClrObjects)
    {
        return S_OK;
}
    LockCritSect;
    CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour

    auto keyNew = TreeKey(bt_GCHnd, (LPVOID)handleId);
    TrkBlock oTrkBlock(4); // 0 size for GCHnd
    oTrkBlock.GCHndInfo.handleId = handleId;
    oTrkBlock.GCHndInfo.initialObjectId = initialObjectId;
    auto resInsert = g_TrackMemClass->InsertIntoList(PairAddrBlock(keyNew, oTrkBlock),
#if MSDEBUG
        /*nExtraFramesToSkip =*/1
#else
        /*nExtraFramesToSkip =*/2
#endif MSDEBUG
    );

#if MSDEBUG
    //    VSASSERT(resInsert.second==true,"failed to insert GCHnd obj into tree");
#endif MSDEBUG

    return S_OK;
}

STDMETHODIMP CProcessGC::HandleDestroyed(GCHandleID handleId)
{
    if (!g_TrackClrObjects)
    {
        return S_OK;
    }
    LockCritSect;
    CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour

    auto thelist = g_TrackMemClass->GetList();
    auto key = TreeKey(bt_GCHnd, (LPVOID)handleId);
    auto res = thelist->find(key);

    if (res == thelist->end())
    {
#if MSDEBUG
        VSASSERTF((g_pCProcessGC->m_fDidTurnOnTrkClrObjsForRetrack, "GCHandleDestroyed not found %x", handleId));
#endif MSDEBUG
    }
    else
    {
        thelist->erase(res);
    }
    return S_OK;
}


CProcessGC::CProcessGC()  // ctor
{
    ZeroMemory(&m_GCStats, sizeof(m_GCStats));
    VSASSERTF((g_pCorProfilerInfo != 0, "g_pCorProfilerInfo null?"));
    m_objLookRef = 0;
    m_SystemStringClassId = 0;
    m_fInGC = 0;
    m_fDoingObjectDump = 0;
    m_fDidSignalEndDumpingGCRoots = false;
    m_fHaveToCatchUpObjectTracking = false;

    m_pRangeMap = new(DebugAlloc(sizeof(MapRange))) MapRange(less<ObjectID>(), MySTLAlloc<PairObjRange>(InternalHeapToUse));

    m_pvecObjRefs = new(DebugAlloc(sizeof(vecPairIntObjectId))) vecPairIntObjectId(MySTLAlloc<PairIntObjId>(InternalHeapToUse));

    m_pRootRefs = new(DebugAlloc(sizeof(RootRefs))) RootRefs(less<ObjectID>(), MySTLAlloc<pair<ObjectID, PairInt> >(InternalHeapToUse));

    m_pRefMap = new (DebugAlloc(sizeof(RefMap))) RefMap(less<ObjectID>(), MySTLAlloc<pair<ObjectID, CObjRef * > >(InternalHeapToUse));

    m_pResultsPathToGCRoot = new (DebugAlloc(sizeof(ResultsPathToGCRoot))) ResultsPathToGCRoot(MySTLAlloc<ObjectID>(InternalHeapToUse));
}

CProcessGC::~CProcessGC()
{
    m_pvecObjRefs->freemem();
    m_pRangeMap->freemem();
    m_pRootRefs->freemem();
    m_pRefMap->freemem();
    m_pResultsPathToGCRoot->freemem();
}

STDMETHODIMP CProcessGC::GarbageCollectionStarted(int cGenerations, BOOL generationCollected[], COR_PRF_GC_REASON reason)
{
    m_fInGC = true;
    m_CritSect.Request();
    g_pCProcessGC->m_pvecObjRefs->m_pStlType->clear();
    g_pCProcessGC->m_pRootRefs->m_pStlType->clear();
    g_pCProcessGC->m_pResultsPathToGCRoot->m_pStlType->clear();

    g_pCProcessGC->m_setObjects.clear();
    g_pCProcessGC->m_GCStats.m_nCollected = 0;
    g_pCProcessGC->m_GCStats.m_nMoved = 0;
    g_pCProcessGC->m_GCStats.m_nSurvived = 0;


    BYTE genCollectedBitMap = 0;
    for (int i = 0; i < cGenerations; i++)
    {
        if (generationCollected[i])
        {
            InterlockedIncrement((long *)&m_GCStats.m_nGenCollected[i]);
            genCollectedBitMap |= (1 << i);
        }
    }
    if (g_fTrackGCStacks)
    {
        LockCritSect;
        CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
        auto thelist = g_TrackMemClass->GetList();
        g_pCProcessGC->s_nGCNumber++;
        auto keyNew = TreeKey(bt_TrackGCStacks, (LPVOID)g_pCProcessGC->s_nGCNumber);
        TrkBlock oTrkBlock(0); // 0 size for gc stack
        oTrkBlock.TrackGCStacksInfo.genCollectedBitMap = genCollectedBitMap;
        oTrkBlock.TrackGCStacksInfo.Reason = reason; //COR_PRF_GC_INDUCED = 1, COR_PRF_GC_OTHER = 0

        auto resInsert = g_TrackMemClass->InsertIntoList(PairAddrBlock(keyNew, oTrkBlock), /*nExtraFramesToSkip =*/2);
        VSASSERT(resInsert.second == true, "failed to insert gcstack obj into tree");
    }

    m_cGenerations = cGenerations;
    memcpy(m_generationCollected, generationCollected, cGenerations * sizeof(BOOL));
    HRESULT hr = g_pCorProfilerInfo->GetGenerationBounds(0, &m_ulNumBounds, 0);

    m_pgeneration_range = (COR_PRF_GC_GENERATION_RANGE  *)DebugAlloc(m_ulNumBounds * sizeof(COR_PRF_GC_GENERATION_RANGE));
    hr = g_pCorProfilerInfo->GetGenerationBounds(m_ulNumBounds, &m_ulNumBounds, m_pgeneration_range);

    if (g_CheckGCLostObjects >= 2)
    {
        if ((g_dfPrintfFlags & DF_OUTPUTFILE) == 0)
        {
            g_dfPrintfFlags |= DF_OUTPUTFILE;
            g_dfEnabled |= DF_OUTPUTFILE;
            g_dfPrintfFlags &= ~DF_OUTPUTDEBUGSTRING; // for perf
        }
        VsDebugPrintf("* GC* GC Start #gen = %d   SeqNo=%d\r\n", cGenerations, g_ulGlobalPassCount);
        for (int i = 0; i < cGenerations; i++)
        {
            VsDebugPrintf("  %d ( %d)", generationCollected[i], m_GCStats.m_nGenCollected[i]);
        }
        VsDebugPrintf("\r\n* GC*Bounds  #%d  \r\n", m_ulNumBounds);
        for (ULONG j = 0; j < m_ulNumBounds; j++)
        {
            VsDebugPrintf("* GC* Bnds %4d  %d  %08x  %08x  %08x\r\n", j,
                m_pgeneration_range[j].generation,
                m_pgeneration_range[j].rangeStart,
                m_pgeneration_range[j].rangeStart + m_pgeneration_range[j].rangeLength,
                m_pgeneration_range[j].rangeLengthReserved
            );
        }
        VsDebugPrintf("\r\n");
    }

    return S_OK;
}


void CProcessGC::PostProcessGC() // happens after CLR has done GC, but before GarbageCollectionFinished is called
{

    if (g_TrackClrObjects)
    {
        CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour

        ListTrkBlocks *thelist = g_TrackMemClass->GetList();
        CHeapSpy *pHeapSpy = g_TrackMemClass->m_MemSpectHeap;
        if (!m_pRangeMap->m_pStlType->empty())
        {

            // we don't want to save callstacks for each Moved object: we're replacing each moved obj stack with the obj creation stack
            BackupValue<BOOL> save_g_fSaveCallstack(&g_fSaveCallstack);
            g_fSaveCallstack = false;

            if (g_CheckGCLostObjects >= 2)
            {
                VsDebugPrintf("PostProcessGC Start: SeqNo=%d\r\n", g_ulGlobalPassCount);
                ObjectID oIdPrior = 0;
                for (auto nn = m_pRangeMap->m_pStlType->begin(); nn != m_pRangeMap->m_pStlType->end(); nn++)
                {
                    auto oId = (*nn).first;
                    auto oRange = (*nn).second;
                    //                    VsDebugPrintf("%08x  %08x   %08x \r\n",oId, oRange.first, oRange.first + oRange.second );
                    VSASSERT(oId >= oIdPrior, "overlap ?");
                    oIdPrior = oId + oRange.second;
                }
            }
            struct CMovedNode
            {
                CMovedNode() :
                    m_TrkBlock(0)
                {
                }
                ObjectID m_oId;
                TrkBlock m_TrkBlock;
                CAddressNode *m_pSavedAddressNode; // save CAddressNode plus the callstack
            };
            /*
            DebugFree(m_pgeneration_range);

            HRESULT hr = g_pCorProfilerInfo->GetGenerationBounds(0, &m_ulNumBounds , 0);

            m_pgeneration_range = (COR_PRF_GC_GENERATION_RANGE  *) DebugAlloc(m_ulNumBounds * sizeof(COR_PRF_GC_GENERATION_RANGE ));
            hr = g_pCorProfilerInfo->GetGenerationBounds(m_ulNumBounds, &m_ulNumBounds , m_pgeneration_range);
            if (g_CheckGCLostObjects>=2)
            {
            VsDebugPrintf("\r\n* GCFinished GC*Bounds  #%d  \r\n", m_ulNumBounds);
            for (ULONG j = 0 ; j < m_ulNumBounds ; j++)
            {
            VsDebugPrintf("* GC* Bnds %4d  %d  %08x  %08x  %08x\r\n", j,
            m_pgeneration_range[j].generation,
            m_pgeneration_range[j].rangeStart,
            m_pgeneration_range[j].rangeStart + m_pgeneration_range[j].rangeLength,
            m_pgeneration_range[j].rangeLengthReserved
            );
            }
            VsDebugPrintf("\r\n");
            }
            */
            //COR_PRF_GC_LARGE_OBJECT_HEAP ==3
            for (int nGen = COR_PRF_GC_LARGE_OBJECT_HEAP; nGen >= 0; nGen--) // for each gen
            {
                if (m_generationCollected[nGen])
                {
                    for (ULONG nBound = 0; nBound < m_ulNumBounds; nBound++)
                    {
                        COR_PRF_GC_GENERATION_RANGE *pGenRange = &m_pgeneration_range[nBound];
                        if (pGenRange->generation == nGen)
                        {
                            ULONG nMoved = 0;
                            vector<CMovedNode, MySTLAlloc<CMovedNode> > MovedNodeList(MySTLAlloc<CMovedNode>(InternalHeapToUse)); // use private debug heap
                            auto oIdlowestInGen = pGenRange->rangeStart;
                            auto oIdHighestInGen = oIdlowestInGen + pGenRange->rangeLength;
                            auto treeNodeIter = thelist->lower_bound(TreeKey(bt_ClrObject, (LPVOID)oIdlowestInGen)); // find first CLR obj in range;
                            auto treeNodeEnd = thelist->upper_bound(TreeKey(bt_ClrObject, (LPVOID)oIdHighestInGen));


                            ObjectID oId;
                            BOOL fDidReachEndOfRanges = false;
                            ObjectID oIdRangeStart;
                            ObjectID oIdRangeEnd;
                            auto rangeCurrent = m_pRangeMap->m_pStlType->lower_bound(oIdlowestInGen);

                            if (rangeCurrent == m_pRangeMap->m_pStlType->end())
                            {
                                fDidReachEndOfRanges = true;
                            }
                            else
                            {
                                Range oRange = rangeCurrent->second;  // not an apple
                                oIdRangeStart = rangeCurrent->first;
                                oIdRangeEnd = oIdRangeStart + oRange.second;
                            }
                            // objs in tree and ranges are ascending: walk both sets in parallel            
                            // we walk every obj in GenBounds: if it falls in a range then it survived or moved. Else collected.
                            while (treeNodeIter != treeNodeEnd)
                            {
                                oId = (ObjectID)treeNodeIter->first.second;
                                while (!fDidReachEndOfRanges && oId >= oIdRangeEnd) // bring the ranges up to date
                                {
                                    if (++rangeCurrent != m_pRangeMap->m_pStlType->end())
                                    {
                                        oIdRangeStart = rangeCurrent->first;
                                        Range oRange = rangeCurrent->second;
                                        oIdRangeEnd = oIdRangeStart + oRange.second;
                                    }
                                    else
                                    {
                                        // we've reached the end of the Moved/Survived ranges. We still have to continue through the end of the gen bound: the rest is collected.
                                        if (g_CheckGCLostObjects >= 2)
                                        {
                                            VsDebugPrintf("GCEOR %08x\r\n", oId);
                                        }
                                        fDidReachEndOfRanges = true;
                                    }
                                }
                                if (!fDidReachEndOfRanges && (oIdRangeStart <= oId && oId < oIdRangeEnd)) // within a range?
                                {
                                    // survived or moved
                                    InterlockedIncrement((long *)&m_GCStats.m_nSurvived);
                                    ObjectID oNewId = rangeCurrent->second.first + oId - oIdRangeStart;
                                    TrkBlock *pTrkBlock = &treeNodeIter->second;
                                    if (oNewId == oId) // did it get moved?
                                    {
                                        // survived
                                        if (g_CheckGCLostObjects >= 3)
                                        {
                                            VsDebugPrintf("GCSur: %08x\r\n", oId);
                                        }
                                        pTrkBlock->ClrObject.m_nGCsSurvived++;
                                        pTrkBlock->ClrObject.m_nGen = nGen;
                                        treeNodeIter++;
                                    }
                                    else
                                    {
                                        // moved: so save stuff, then delete old node normally
                                        PVOID ptrAlloc = ((DWORD *)pTrkBlock) - Offset_AllocToTblk; // ptr to raw allocation
                                        CAddressNode *pAddrNode = pHeapSpy->FindInst(ptrAlloc);
                                        CMovedNode oNewNode;
                                        oNewNode.m_oId = oNewId;
                                        if (g_CheckGCLostObjects >= 3)
                                        {
                                            VsDebugPrintf("GCMov: %08x  New=%08x  %d\r\n", oId, oNewId, pAddrNode->m_cAlloc);
                                        }
                                        oNewNode.m_TrkBlock = *pTrkBlock;

                                        auto nNodeSize = sizeof(CAddressNode);
                                        if (g_StackStorageMode != InMemory)
                                        {
                                            nNodeSize += sizeof(DWORD);
                                        }
                                        else
                                        {
                                            nNodeSize += pAddrNode->m_uicStackAddr * sizeof(DWORD);
                                        }
                                        oNewNode.m_pSavedAddressNode = (CAddressNode *)DebugAlloc(nNodeSize); // this will replace the one in DebugHeap and be freed via DeleteInst
                                        memcpy(oNewNode.m_pSavedAddressNode, pAddrNode, nNodeSize);
                                        oNewNode.m_TrkBlock.ClrObject.m_nMoved = pTrkBlock->ClrObject.m_nMoved + 1;
                                        oNewNode.m_TrkBlock.ClrObject.m_nGen = nGen;
                                        oNewNode.m_TrkBlock.ClrObject.m_nGCsSurvived = pTrkBlock->ClrObject.m_nGCsSurvived + 1;

                                        InterlockedIncrement((long *)&m_GCStats.m_nMoved);
                                        MovedNodeList.push_back(oNewNode);
                                        auto save_g_AssertOnSeqNo = CHeapSpy::g_AssertOnSeqNo; // we need to save/restore this so we don't assert on Move
                                        CHeapSpy::g_AssertOnSeqNo = 0;
                                        if (g_StackStorageMode != InMemory)
                                        {
                                            pAddrNode->m_uicStackAddr = 0;	// we're transferring the stack , so we don't want to add to freelist
                                        }
                                        treeNodeIter = thelist->erase(treeNodeIter);
                                        CHeapSpy::g_AssertOnSeqNo = save_g_AssertOnSeqNo;
                                        nMoved++;
                                    }
                                }
                                else
                                {
                                    // collected
                                    if (g_CheckGCLostObjects >= 3)
                                    {
                                        VsDebugPrintf("GCCol: %08x\r\n", oId);
                                    }
                                    if (g_ftrackClassInstances)
                                    {
                                        TrkBlock *pTrkBlock = &treeNodeIter->second;

                                        auto clsTrkBlk = AddOrFindClassId(pTrkBlock->ClrObject.m_ClassId, pTrkBlock->m_dwSize);
                                        TrkBlock *pTrkBlkClass = &(*clsTrkBlk).second;
                                        pTrkBlkClass->ClrClass.m_nInstance--; // bump instance count
                                        pTrkBlkClass->ClrClass.m_nCollected++; // bump collected count
                                        g_memStats.ClrObjs.Free.count++;
                                        g_memStats.ClrObjs.Free.size += pTrkBlock->m_dwSize;

                                    }
                                    InterlockedIncrement((long *)&m_GCStats.m_nCollected);
                                    treeNodeIter = thelist->erase(treeNodeIter);
                                }
                            }
                            ULONG nMoveIndex = 0;
                            if (g_CheckGCLostObjects >= 2)
                            {
                                VsDebugPrintf("GCMoving: %d objs\r\n", nMoved);
                            }
                            for (auto pNodeToAdd = MovedNodeList.begin(); nMoveIndex < nMoved; pNodeToAdd++, nMoveIndex++)
                            {
                                ObjectID oIdNew = pNodeToAdd->m_oId;
                                auto keyNew = TreeKey(bt_ClrObject, (LPVOID)oIdNew);
                                VSASSERT(g_CheckGCLostObjects == 0 || thelist->find(keyNew) == thelist->end(), "GCMoved: obj moved to already occupied obj");
                                auto resInsert = g_TrackMemClass->InsertIntoList(PairAddrBlock(keyNew, pNodeToAdd->m_TrkBlock));
                                VSASSERT(g_CheckGCLostObjects == 0 || resInsert.second == true, "failed to insert moved obj into tree");
                                TrkBlock *pNewTrkBlk = &(resInsert.first)->second;
                                PVOID ptrAllocNew = ((DWORD *)pNewTrkBlk) - Offset_AllocToTblk; // ptr to raw allocation
                                CAddressNode *pOldAddrNodeCopy = pNodeToAdd->m_pSavedAddressNode;// point to saved copy
                                pHeapSpy->ReplaceInst(ptrAllocNew, pOldAddrNodeCopy);
                            }
                            //                            break;
                        }
                    }
                }
            }
        }
        if (g_CheckGCLostObjects >= 2)
        {
            VsDebugPrintf("PostProcessGC done: SeqNo=%d\r\n", g_ulGlobalPassCount);
        }
    }
}


STDMETHODIMP CProcessGC::GarbageCollectionFinished()
{
    LockCritSect;
    PostProcessGC();
    if (g_TrackClrObjects)
    {
        if (m_fDoingObjectDump)
        {
            DWORD nBytesWritten;
            DWORD verb = VerbDone;
            WriteFile(g_TrackMemClass->m_CreateChildProcess.m_hPipeFromChild, &verb, 1, &nBytesWritten, 0); //yes: 1 byte
            m_fDoingObjectDump = 0;
        }
        if (m_fHaveToCatchUpObjectTracking)
        {
            // user toggled back onobjtracking:
            m_fHaveToCatchUpObjectTracking = 0;
            CHeapSpy *pHeapSpy = g_TrackMemClass->m_MemSpectHeap;
            pHeapSpy->m_nStackFramesToSkip += 3;  // hide some non-interesting inernal stackframes.
            ReTrackObjectsAfterTogglingOnTracking(); // give a nice name so it informs user when shown in callstack
            pHeapSpy->m_nStackFramesToSkip -= 3;  // hide some non-interesting inernal stackframes.
        }
        else
        {
            if (m_objLookRef && m_objLookParam == 0)// user specified an obj on which to GCRoot. Iterate thru all roots finding all paths
            {
                m_pCurPathSoFar = &vecObjId(MySTLAlloc<ObjectID>(InternalHeapToUse)); // This is stack only: can only be ref'd at this frame and below
                m_pCurPathSoFar->resize(1000);
#if GCROOTPATHBREADTHFIRST
                list<CObjRef*, MySTLAlloc<CObjRef*> > queuelist(MySTLAlloc<CObjRef*>(InternalHeapToUse)); // This is stack only: can only be ref'd at this frame and below
                queueObjId thequeue(queuelist);// This is stack only: can only be ref'd at this frame and below
                m_pobjqueue = &thequeue;
                for_each(
                    m_pRootRefs->m_pStlType->begin(),
                    m_pRootRefs->m_pStlType->end(),
                    [this](RootRefsType::reference  it) {
                    ObjectID obj = it.first;
                    auto refMapLookup = m_pRefMap->m_pStlType->find(obj);
                    CObjRef *pObjRef = (*refMapLookup).second;
                    m_pobjqueue->push(pObjRef);
            }
                );
                m_nDepth = 0;
                FindPathsToObj();
#else
                for_each(
                    m_pRootRefs->m_pStlType->begin(),
                    m_pRootRefs->m_pStlType->end(),
                    [this](RootRefsType::reference  it) {
                    m_nDepth = 0;
                    m_objIdFrom = it.first;
                    FindPathsToObj();
                }
                );
#endif GCROOTPATHBREADTHFIRST

                for_each(
                    m_pRefMap->m_pStlType->begin(),
                    m_pRefMap->m_pStlType->end(),
                    [](ObjRefMapStlType::reference  it) {
                    it.second->~CObjRef();
                }
                );
                m_pRefMap->m_pStlType->clear();
        }
    }
        if (g_CheckGCLostObjects)
        {
            // first find if all the objs in our tree are in ObjectReferences 
            ListTrkBlocks *thelist = g_TrackMemClass->GetList();
            auto treeNodeIter = thelist->lower_bound(TreeKey(bt_ClrObject, (LPVOID)0)); // find first CLR obj in range;

            while (1)
            {
                auto bt = treeNodeIter->first.first;
                if (bt != bt_ClrObject)
                {
                    break;
                }
                TrkBlock *pTrkBlock = &(*treeNodeIter).second;
                ObjectID oId = (ObjectID)(*treeNodeIter).first.second;
                auto findit = m_setObjects.find(oId);
#if MSDEBUG
                if (findit == m_setObjects.end())
                {
                    CComBSTR bstrTemp = GetTrkBlkInfoForObj(oId, "object in tree not found in ObjectReferences set\n", pTrkBlock);
                    VSASSERTF((false, "%S", bstrTemp));
                }
#else
                VSASSERT(findit != m_setObjects.end(), "GC Fin: obj in list not in setobjs");
#endif MSDEBUG
                treeNodeIter++;
            }

            // second find if all the objs from ObjectReferences are in our tree
            for (auto it = m_setObjects.begin(); it != m_setObjects.end(); it++)
            {
                ObjectID objectID = *it;
                auto theTreeKey = TreeKey(bt_ClrObject, (LPVOID)objectID);
                auto treeres = thelist->find(theTreeKey);

#if MSDEBUG
                if (treeres == thelist->end()) // if we found an obj in set that isn't in our tree, add it.
                {
                    ULONG  nSize = 0;
                    ClassID classID;
                    g_pCorProfilerInfo->GetObjectSize(objectID, &nSize);
                    g_pCorProfilerInfo->GetClassFromObject(objectID, &classID);
                    TrkBlock oTrkBlock(nSize);
                    oTrkBlock.ClrObject.m_ClassId = classID;
                    oTrkBlock.ClrObject.m_nGCsSurvived = 1000; // head start/flag

                    auto resInsert = g_TrackMemClass->InsertIntoList(PairAddrBlock(theTreeKey, oTrkBlock));
                    VSASSERT(resInsert.second == true, "Failed to insert new block in CLR checkclrlostobjs");

                    CComBSTR bstrInfo = GetTrkBlkInfoForObj(objectID, "GC lost objects\r\n", &oTrkBlock);
                    VSASSERTF((false, "%S", bstrInfo));

                }
#else
                VSASSERT(treeres != thelist->end(), "GC Finished: GC Lost obj not found in our tree"); // if we found an obj in set that isn't in our tree, add it.
#endif MSDEBUG
            }
            m_setObjects.clear();
        }
}
    DebugFree(m_pgeneration_range);
    m_pgeneration_range = 0;
    m_pRangeMap->m_pStlType->clear();
    m_fInGC = false;
    m_CritSect.Release();
    return S_OK;
}


void CProcessGC::FindPathsToObj()  // recursive
{
#if GCROOTPATHBREADTHFIRST
    while (!m_pobjqueue->empty())
    {
        CObjRef *pObjRef = m_pobjqueue->front();
        m_pobjqueue->pop();
        if (!pObjRef->m_fBeenHereAlready)
        {
            ObjectID obj = pObjRef->m_objectId;
            if (obj != m_objLookRef)
            {
                // add the refs from this obj to the queue
                pObjRef->m_fBeenHereAlready = 1;// flag to indicate we've been here;
                auto pcurref = pObjRef->m_pvecObjId->m_pStlType;
                auto nEnd = pcurref->size();
                if (nEnd)
                {
                    auto pbegin = &pcurref->at(0);
                    for (ULONG i = 0; i < nEnd; i++, pbegin++)
                    {
                        auto refMapLookup = m_pRefMap->m_pStlType->find((ObjectID)*pbegin);
                        CObjRef *pObjRefChild = (*refMapLookup).second;
                        if (!pObjRefChild->m_fBeenHereAlready && pObjRefChild->m_pCObjRefParent == 0) // if parent != 0, then already queued
                        {
                            pObjRefChild->m_pCObjRefParent = pObjRef;
                            pObjRefChild->m_nLevel = pObjRef->m_nLevel + 1;
                            m_pobjqueue->push(pObjRefChild);
                        }
                    }
                }
            }
            else
            {
                // bingo
                if (pObjRef->m_nLevel > 0) // 0 indicates it's a GC Root, so we won't add it here
                {
                    vecObjId pathParent(MySTLAlloc<ObjectID>(InternalHeapToUse));
                    pathParent.push_back(obj);
                    CObjRef *pParent = pObjRef->m_pCObjRefParent;
                    while (pParent)
                    {
                        pathParent.push_back(pParent->m_objectId);
                        pParent = pParent->m_pCObjRefParent;
                    }
                    m_pResultsPathToGCRoot->m_pStlType->push_back(pathParent);
                }
            }
        }

    }
#else
    auto refMapLookup = m_pRefMap->m_pStlType->find(m_objIdFrom);
    VSASSERT(refMapLookup != m_pRefMap->m_pStlType->end(), "Obj not found in objrefmap");
    pair<ObjectID, CObjRef *> pr = *refMapLookup;
    CObjRef *pObjRef = pr.second;
    if (!pObjRef->m_fBeenHereAlready) // have we been here before?
    {
        VSASSERT(m_nDepth < 1500, "obj ref search too deep");
        if (m_pCurPathSoFar->size() < m_nDepth + 1)
        {
            if (m_pCurPathSoFar->capacity() < m_nDepth + 1)
            {
                m_pCurPathSoFar->reserve(m_nDepth + 10);
            }
            m_pCurPathSoFar->resize(m_nDepth + 1);
        }
        (*m_pCurPathSoFar)[m_nDepth] = m_objIdFrom;   // indicate where we are;
        if (m_objIdFrom == m_objLookRef) // bingo!
        {
            if (m_nDepth)
            {
                vecObjId pathvec(MySTLAlloc<ObjectID>(InternalHeapToUse));
                for (UINT i = 0; i <= m_nDepth; i++)
                {
                    pathvec.push_back((*m_pCurPathSoFar).at(i));
                }
                m_pResultsPathToGCRoot->m_pStlType->push_back(pathvec);
            }
        }
        else
        {// now iterate thru the cur objs references.
            pObjRef->m_fBeenHereAlready = 1;// flag to indicate we've been here;
            auto pcurref = pObjRef->m_pvecObjId->m_pStlType;
            auto nEnd = pcurref->size();
            if (nEnd)
            {
                auto pbegin = &pcurref->at(0);
                m_nDepth++;
                for (ULONG i = 0; i < nEnd; i++, pbegin++)
                {
                    m_objIdFrom = ((ObjectID)*pbegin);
                    FindPathsToObj(); //recur
                }
                m_nDepth--;
            }
                }
            }
#endif GCROOTPATHBREADTHFIRST
        }

void CProcessGC::ReTrackObjectsAfterTogglingOnTracking()
{
    LockCritSect;
    ListTrkBlocks *thelist = g_TrackMemClass->GetList();

    for_each(
        m_pRefMap->m_pStlType->begin(),
        m_pRefMap->m_pStlType->end(),
        [this, thelist](ObjRefMapStlType::reference  it) {
        ObjectID objectID = it.first;
        auto theTreeKey = TreeKey(bt_ClrObject, (LPVOID)objectID);
        auto treeres = thelist->find(theTreeKey);
        if (treeres == thelist->end())
        {
            ULONG  nSize = 0;
            ClassID classID;
            g_pCorProfilerInfo->GetObjectSize(objectID, &nSize);
            g_pCorProfilerInfo->GetClassFromObject(objectID, &classID);
            TrkBlock oTrkBlock(nSize);
            oTrkBlock.ClrObject.m_ClassId = classID;

#if MSDEBUG
            int nExtraFramesToSkip = 0;
#else
            int nExtraFramesToSkip = -1;
#endif
            auto resInsert = g_TrackMemClass->InsertIntoList(PairAddrBlock(theTreeKey, oTrkBlock), nExtraFramesToSkip);
            VSASSERT(resInsert.second == true, "Failed to insert new block in CLR MovedRef");
            if (g_ftrackClassInstances)
            {
                DWORD dwSize = 0;
                g_pCorProfilerInfo->GetObjectSize(objectID, &dwSize);
                auto clsTrkBlk = AddOrFindClassId(classID, dwSize);// get the class that we just collected

                (*clsTrkBlk).second.ClrClass.m_nInstance++; // bump instance count

            }


        }
        it.second->~CObjRef();
    }
    );
    m_pRefMap->m_pStlType->clear();
}

STDMETHODIMP CProcessGC::MovedReferences(ULONG cmovedObjectIDRanges, ObjectID oldObjectIDRangeStart[], ObjectID newObjectIDRangeStart[], ULONG cObjectIDRangeLength[])
{
    HRESULT hr = S_OK;
    if (g_TrackClrObjects)
    {
        LockCritSect;
        if (g_CheckGCLostObjects >= 2)
        {
            VsDebugPrintf("* GC* Moved: %d\r\n", cmovedObjectIDRanges);
            for (ULONG i = 0; i < cmovedObjectIDRanges; i++)
            {
                VsDebugPrintf("* GC*Mov %4d %08x %08x  %08x  %08x\r\n", i,
                    oldObjectIDRangeStart[i],
                    oldObjectIDRangeStart[i] + cObjectIDRangeLength[i],
                    cObjectIDRangeLength[i],
                    newObjectIDRangeStart ? newObjectIDRangeStart[i] : 0);
            }
            VsDebugPrintf("\r\n");
        }
        //        CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
        for (ULONG i = 0; i < cmovedObjectIDRanges; i++)
        {
            auto resmap = m_pRangeMap->m_pStlType->insert(
                PairObjRange(
                    oldObjectIDRangeStart[i],
                    Range(
                        newObjectIDRangeStart[i],
                        cObjectIDRangeLength[i])
                )
            );
            VSASSERT(g_CheckGCLostObjects == 0 || resmap.second == true, "map insert failed: movedrefs");
        }
    }
    return hr;
}

STDMETHODIMP CProcessGC::SurvivingReferences(ULONG cSurvivingObjectIDRanges, ObjectID objectIDRangeStart[], ULONG cObjectIDRangeLength[])
{
    if (g_TrackClrObjects)
    {
        LockCritSect;
        if (g_CheckGCLostObjects >= 2)
        {
            VsDebugPrintf("* GC* Surved: %d\r\n", cSurvivingObjectIDRanges);
            for (ULONG i = 0; i < cSurvivingObjectIDRanges; i++)
            {
                VsDebugPrintf("* GC*Sur %4d %08x  %08x  %08x\r\n", i,
                    objectIDRangeStart[i],
                    objectIDRangeStart[i] + cObjectIDRangeLength[i],
                    cObjectIDRangeLength[i]);
            }
            VsDebugPrintf("\r\n");
        }
        //        CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
        for (ULONG i = 0; i < cSurvivingObjectIDRanges; i++)
        {
            auto resmap = m_pRangeMap->m_pStlType->insert(
                PairObjRange(
                    objectIDRangeStart[i],
                    Range(
                        objectIDRangeStart[i],
                        cObjectIDRangeLength[i])
                )
            );
            VSASSERT(g_CheckGCLostObjects == 0 || resmap.second == true, "map insert failed: survive");
        }
    }
    return S_OK;
}


STDMETHODIMP CProcessGC::RootReferences2(ULONG cRootRefs, ObjectID rootRefIds[], COR_PRF_GC_ROOT_KIND rootKinds[], COR_PRF_GC_ROOT_FLAGS rootFlags[], UINT_PTR rootIds[])
{
    HRESULT hr = S_OK;
    LockCritSect;

    if (g_pCProcessGC->m_fDoingObjectDump)
    {
        DWORD nBytesWritten;
        //1st, write out the # of rootrefs in this bunch
        WriteFile(g_TrackMemClass->m_CreateChildProcess.m_hPipeFromChild, &cRootRefs, 4 * 1, &nBytesWritten, 0);

        for (ULONG i = 0; i < cRootRefs; i++)
        {
            CAddressNode *pAddrNode = GetOrCreateAddressNodeFromObjectId(rootRefIds[i], /*pCProcessGC=*/this);
            VSASSERT(rootRefIds[i] == 0 || pAddrNode, "RootRef2 addrnode not found in memspect heap"); // we must include 0 because it's include din count that we already sent

            struct GCRoottempdata
            {
                CAddressNode * m_pAddrNode;
                COR_PRF_GC_ROOT_KIND m_rootKinds;
                COR_PRF_GC_ROOT_FLAGS  m_rootFlags;
                UINT m_rootId;
            } GCRoottemp =
            {
                pAddrNode, rootKinds[i], rootFlags[i], rootIds[i]
            };
            WriteFile(g_TrackMemClass->m_CreateChildProcess.m_hPipeFromChild, &GCRoottemp, sizeof(GCRoottempdata), &nBytesWritten, 0);
        }
    }


    if (m_objLookRef && m_objLookParam == 0)
    {
        for (ULONG i = 0; i < cRootRefs; i++)
        {
            if (rootRefIds[i] != 0)
            {
                auto valdata = pair<ULONG, ULONG>(
                    ((rootFlags[i] << 16) + rootKinds[i]),
                    rootIds[i]
                    );

                auto resInsert = m_pRootRefs->m_pStlType->insert(pair<ObjectID, PairInt>(rootRefIds[i], valdata));
            }
        }
    }

    return hr;
}



STDMETHODIMP CProcessGC::ObjectReferences(ObjectID objectID, ClassID classID, ULONG cObjectRefs, ObjectID objectRefIDs[])
{
    HRESULT hr = S_OK;
    LockCritSect;
    if (g_CheckGCLostObjects)
    {
        m_setObjects.insert(objectID);
    }
    //    CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour

    if (g_pCProcessGC->m_fDoingObjectDump)
    {

        DWORD nBytesWritten;
        if (!m_fDidSignalEndDumpingGCRoots) // do only once
        {
            DWORD zero = 0;
            // now write out a 0 to indicate the end of the GCRoots section
            WriteFile(g_TrackMemClass->m_CreateChildProcess.m_hPipeFromChild, &zero, 4 * 1, &nBytesWritten, 0);
            m_fDidSignalEndDumpingGCRoots = true;
        }

        CAddressNode *pAddrNode = GetOrCreateAddressNodeFromObjectId(objectID, /*pCProcessGC=*/this, /*fCreateIfNotFound=*/true);
        if (pAddrNode == 0)
        {
            VSASSERT(pAddrNode, "ObjectReferences addrnode not found in memspect heap");
        }

        struct tempdata
        {
            CAddressNode * m_pAddrNode;
            ULONG m_cObjectRefs;
        } temp =
        {
            pAddrNode, cObjectRefs
        };

        WriteFile(g_TrackMemClass->m_CreateChildProcess.m_hPipeFromChild, &temp, sizeof(tempdata), &nBytesWritten, 0);
        if (cObjectRefs)
        {
            // first we convert the obj refs to a CAddressNode (HeapAllocationStruct)
            CAddressNode**pRefs = (CAddressNode **)DebugAlloc(cObjectRefs * sizeof(CAddressNode **));
            for (ULONG i = 0; i < cObjectRefs; i++)
            {
                pAddrNode = GetOrCreateAddressNodeFromObjectId(objectRefIDs[i], /*pCProcessGC=*/this, /*fCreateIfNotFound*/true);
                if (!pAddrNode)
                {
                    VSASSERTF((false, "addrnode obj ref not found in memspect heap %i %x ", i, objectRefIDs[i]));
                }
                pRefs[i] = pAddrNode;
            }
            WriteFile(g_TrackMemClass->m_CreateChildProcess.m_hPipeFromChild, pRefs, 4 * cObjectRefs, &nBytesWritten, 0);
            DebugFree(pRefs);
        }

    }

    if (m_objLookRef || m_fHaveToCatchUpObjectTracking)
    {
        if (m_objLookParam == 0 || m_fHaveToCatchUpObjectTracking)
        {
            CObjRef *pCObjRef = new (DebugAlloc(sizeof(CObjRef))) CObjRef(
#if GCROOTPATHBREADTHFIRST
                objectID,
#endif GCROOTPATHBREADTHFIRST
                cObjectRefs,
                objectRefIDs);
            auto refmapIns = m_pRefMap->m_pStlType->insert(
                pair<ObjectID, CObjRef *>(objectID, pCObjRef)
            );
        }
        if (!m_fHaveToCatchUpObjectTracking)
        {
            if (objectID == m_objLookRef && (m_objLookParam == 0 || m_objLookParam == REFBYME))
            {
                for (ULONG i = 0; i < cObjectRefs; i++)
                {
                    m_pvecObjRefs->m_pStlType->push_back(PairIntObjId(REFBYME, objectRefIDs[i]));
                }
            }
            else
            {
                if (m_objLookParam == 0 || m_objLookParam == REFTOME)
                {
                    for (ULONG i = 0; i < cObjectRefs; i++)
                    {
                        if (objectRefIDs[i] == m_objLookRef)
                        {
                            m_pvecObjRefs->m_pStlType->push_back(PairIntObjId(REFTOME, objectID));
                            break;
                        }
                    }
                }
            }
        }
    }
    return hr;
}


void ProcessGCCritSectRequest()
{
    g_pCProcessGC->m_CritSect.Request();
}

void ProcessGCCritSectRelease()
{
    g_pCProcessGC->m_CritSect.Release();
}

void CProcessGC::FreezeThreads(int CleanupRCWs /*= 1*/)
{
    if (g_nThreadFreeze++ == 0)
    {
        VSASSERT(g_pCDebFreezeAllThreads == 0, "request to freeze when already frozen?");
        //while (m_fInGC) // hack
        //{
        //    Sleep(100);
        //}

        DoForceGC(/*fDoRCWCleanup=*/ CleanupRCWs,/*fFreezeThreads=*/true);
        // could still fail due to Server GC. Client can test. Assert may not be visible (no desktop)
        g_pCProcessGC->m_CritSect.Request(); // wait for GC done
        g_pCProcessGC->m_CritSect.Release();

    }
}

//LPTOP_LEVEL_EXCEPTION_FILTER g_filtPrior;
//DWORD WINAPI exceptFilt(_EXCEPTION_POINTERS *ExceptionInfo)
//{
//    ::MessageBoxA(0,"","Except",0);
//    return 0;
//}
DWORD WINAPI VecExceptionHandler(_EXCEPTION_POINTERS *pExceptionInfo)
{

    /*
    We collect stacks and stuff as DWORDs. When we want to resolve managed stuff into ClrClass or FuncInfo,
    we call ClrProfiler to help. However, it could have been unloaded. There's no API to validata a DWORD FuncId or ClassId
    So we inject this except handler
    CLRVectoredExceptionHandlerShim terminatees process on STATUS_ACCESS_VIOLATION, so we change it.

    */
    if (pExceptionInfo->ExceptionRecord->ExceptionCode == STATUS_ACCESS_VIOLATION)
    {
        pExceptionInfo->ExceptionRecord->ExceptionCode = STATUS_INVALID_PARAMETER;
    }

    //        ::MessageBoxA(0,"","Except",0);
    return EXCEPTION_EXECUTE_HANDLER;
}


STDMETHODIMP CProcessGC::RuntimeSuspendFinished() // gets called as GC is starting
{
    if (g_nThreadFreeze)
    {
        if (g_pCDebFreezeAllThreads == 0 &&
            (GetCurrentThreadId() == g_dwThreadIdChildProcess ||
                GetCurrentThreadId() == g_dwThreadIdMemSpectBgdThreadGCAndCleanupRCW)
            ) // if we haven't frozen yet and we're on the requesting thread
        {
            LockCritSect;
            // suspend all threads (except ours) here 
            /*
            >	MemSpectDll.dll!CProcessGC::RuntimeSuspendFinished()  Line 5786	C++
            MemSpectDll.dll!MyProfiler::RuntimeSuspendFinished()  Line 597 + 0xb bytes	C++
            clr.dll!EEToProfInterfaceImpl::RuntimeSuspendFinished()  Line 4462 + 0xb bytes	C++
            clr.dll!Thread::SysSuspendForGC(SUSPEND_FOR_GC)  Line 13373	C++
            clr.dll!WKS::GCHeap::SuspendEE(SUSPEND_FOR_GC)  Line 644	C++
            clr.dll!WKS::GCHeap::GarbageCollectGeneration(0x00000002, reason_induced)  Line 26403	C++
            clr.dll!WKS::GCHeap::GarbageCollectTry(0x00000002, 0x00000000)  Line 26210	C++
            clr.dll!WKS::GCHeap::GarbageCollect(0xffffffff, 0x00000000, 0x00000000)  Line 26190	C++
            clr.dll!ProfToEEInterfaceImpl::ForceGC()  Line 4686	C++
            MemSpectDll.dll!DoForceGC(true)  Line 997 + 0x19 bytes	C++
            MemSpectDll.dll!CProcessGC::FreezeThreads()  Line 5778 + 0x7 bytes	C++
            MemSpectDll.dll!CreateChildProcess::HandleMsgVerb(0x0280fa3c, 0x00000005)  Line 1299	C++

            */
            g_pCDebFreezeAllThreads = new (DebugAlloc(sizeof(CDebFreezeAllThreads))) CDebFreezeAllThreads();
            m_hVectoredExceptionHandler = AddVectoredExceptionHandler(1, (PVECTORED_EXCEPTION_HANDLER)&VecExceptionHandler);
        }
    }
    return S_OK;
}

void CProcessGC::UnfreezeThreads()
{
    VSASSERT(g_pCDebFreezeAllThreads, "request to unfreeze when not frozen?");
    BOOL res = RemoveVectoredExceptionHandler(m_hVectoredExceptionHandler);
    VSASSERT(res, "RemoveVectoredExceptionHandler failed");
    if (g_pCDebFreezeAllThreads)
    {
        if (--g_nThreadFreeze == 0)
        {
            LockCritSect;
            g_pCDebFreezeAllThreads->~CDebFreezeAllThreads(); // call dtor to unfreeze
            DebugFree(g_pCDebFreezeAllThreads);
            g_pCDebFreezeAllThreads = NULL;
        }
    }
}

CProcessGC::CObjRef::CObjRef(
#if GCROOTPATHBREADTHFIRST
    ObjectID objid,
#endif GCROOTPATHBREADTHFIRST
    ULONG cObjectRefs,
    ObjectID objectRefIDs[])
{
#if GCROOTPATHBREADTHFIRST
    m_objectId = objid;
    m_pCObjRefParent = NULL;
    m_nLevel = 0;
#endif GCROOTPATHBREADTHFIRST
    m_fBeenHereAlready = false;
    m_pvecObjId = new (DebugAlloc(sizeof(CMyStlWrap<vecObjId>))) CMyStlWrap<vecObjId>(MySTLAlloc<ObjectID>(InternalHeapToUse));
    if (cObjectRefs)
    {
        m_pvecObjId->m_pStlType->resize(cObjectRefs);
        memcpy(&(*(m_pvecObjId->m_pStlType))[0], objectRefIDs, cObjectRefs * sizeof(ObjectID));
    }
}

pair<ULONG, ULONG> GetClrClassStats(ClassID classId)
{
    pair<ULONG, ULONG>  prResult(0, 0);
    LockCritSect;
    ListTrkBlocks *thelist = g_TrackMemClass->GetList();
    auto res = thelist->find(TreeKey(bt_ClrClass, (LPVOID)classId));
    if (res != thelist->end())
    {
        TrkBlock *pTrkBlk = &(*res).second;
        prResult.first = pTrkBlk->ClrClass.m_nInstance;
        prResult.second = pTrkBlk->ClrClass.m_nCollected;
    }

    return prResult;
}

void RemoveObjRefsFromList() // we're toggling off obj tracking, so remove all info
{
    LockCritSect;
    ListTrkBlocks *thelist = g_TrackMemClass->GetList();
    auto treeNodeIter = thelist->lower_bound(TreeKey(bt_ClrObject, (LPVOID)0)); // find first CLR obj in range
    auto nodeEnd = thelist->upper_bound(TreeKey(bt_ClrObject, (LPVOID)0xffffffff)); //find last clr obj in range
    while (treeNodeIter != nodeEnd)
    {
        treeNodeIter = thelist->erase(treeNodeIter);
    }
    treeNodeIter = thelist->lower_bound(TreeKey(bt_ClrClass, (LPVOID)0)); // find first CLR obj in range
    nodeEnd = thelist->upper_bound(TreeKey(bt_ClrClass, (LPVOID)0xffffffff)); //find last clr obj in range
    for (; treeNodeIter != nodeEnd; treeNodeIter++)
    {
        TrkBlock *pTrkBlock = &(*treeNodeIter).second;
        pTrkBlock->ClrClass.m_nInstance = 0;
        pTrkBlock->ClrClass.m_nCollected = 0;
    }


}

