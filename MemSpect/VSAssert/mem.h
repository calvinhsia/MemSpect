//=--------------------------------------------------------------------------=
// Mem.h
//=--------------------------------------------------------------------------=
// Debug leak detection and memory allocation
//=--------------------------------------------------------------------------=
// Copyright 1997 Microsoft Corporation.  All rights reserved.
// Information contained herein is proprietary and confidential.
//
// BrianPe, 4/97.  Uncreatively lifted from heap.c in ruby.
//

#ifndef _INC_MEM_H
#define _INC_MEM_H

#include "Util.H"
#undef new  // we make our own
#include <CorHdr.h>
#include <cor.h>
#include <corprof.h>
#include <cordebug.h>

#include "mystl.h"

#define dimensionof(a) 		(sizeof(a)/sizeof(*(a)))

struct CLeakInfo
{
    DWORD     m_dwSize;
    UINT      m_uicHits;
};

extern CMutex s_mxsStackWalk;
#define LockCritSect  CLock cl(&s_mxsStackWalk);

//=--------------------------------------------------------------------------=
// Globals for this module
//
extern TCHAR g_szProcessExeFullPathName[MAX_PATH]; //exe full file name of process
extern TCHAR g_szVSAssertDllFullPathName[MAX_PATH]; //dll full file name of VSAssert.dll

extern LPTSTR g_pszSymbolsPath;
extern LPTSTR g_pszDbgHelpOutput;
extern int    g_nDumpMemStatInterval;

extern BOOL   g_fDumpMemStats;
extern BOOL   g_fReportLeaks;
extern BOOL   g_fEnableExternalLeaks;
extern ULONG  g_ulLeakBreakSkipCount;

extern BOOL   g_fSaveCallstack;
extern BOOL   g_fIsChildProcess;
extern bool g_fIsRunDLL;

// the following are ignored if g_fSaveCallstack is FALSE

//extern LPSTR  g_pszRememberCallstackForAllocSizes;
//extern BOOL   g_fSaveCallstackOnlyForSize;
//extern ULONG  g_ulSaveCallstackSize;
extern UINT    g_nStackFrames;

extern BOOL   g_fEnableGdiLeakCheck;

extern int    g_GdiTrace_nStackFrames;

extern LPTSTR g_pszDllsToSkipInCallstack;
extern TCHAR g_szDllsToAssertOn[];
extern BOOL   g_fEnableFastGetStack;
extern int g_nRetailHeaderTrailerSize;
extern int g_nUseChildProcessForSymbols;
extern int g_fEnableAsserts;
extern int g_CheckGCLostObjects;
extern int g_fHandle4gigStacks;
extern int g_fTrackLoadResource;
extern int g_fTrackGCStacks;
extern int g_NativeOnly;
extern int g_DynStackSymRes;

extern DWORD  g_dwUpdateLoadedModulesTimeout;
extern BOOL   g_fCollectLeaks;



extern UINT      g_uicGDILeaks;
extern BOOL      g_fDontReportKnownGDILeaks;
extern UINT      g_uicMinGdiHits;

extern ULONG  g_ulGlobalPassCount ;     // shared passcount amoung heap and IMalloc spies
extern BOOL g_fTrackVirtualMem;
extern BOOL g_fTrackHeap;
extern BOOL g_TrackClrObjects ;
extern BOOL g_StartChildProcess;
typedef enum {
    TrackCodeMarkerMode_None = 0,
    TrackCodeMarkerMode_Normal = 1,
    TrackCodeMarkerMode_Custom = 2,
} TrackCodeMarkerMode;
extern int g_TrackCodeMarkers;
extern BOOL g_fTrackThreadCreate;
extern int g_fTrackGDI;
extern int g_ThreadDynamic;
extern BOOL g_fUseGlobalName;
extern BOOL g_ShowManagedSourceInStacks;
extern int g_CleanUpRCWs;
extern int g_ImmersiveSession;
extern int g_ImmersiveTID;
extern BOOL g_isImmersive;
extern BOOL g_VirtualAllocTopDown;
extern int g_CodeMarkerParameter;
extern BOOL g_TrackJit;
extern BOOL g_TrackExcpt;
extern BOOL g_TrackETW;
extern BOOL g_TrackArenas;

typedef enum {
    Minimal,
    Normal
} TrackingModeEnum;
extern TrackingModeEnum g_TrackingMode;
typedef enum {
	InMemory,
	InMapFile,
	InMapFileVerify
} StackStorageModeEnum;
extern StackStorageModeEnum g_StackStorageMode;
extern BOOL g_TrackFileLoadUnload;
extern DWORD g_dwThreadIdChildProcess ; // the private threadid of MemSpect to handle verbs coming in. Strictly native code: no managed allowed to run
extern DWORD g_dwThreadIdMemSpectBgdThreadGCAndCleanupRCW; // the private threadid of MemSpect to do RCWCleanup and GC. Managed code can run
extern DWORD g_fTrackingGhosts;


extern HANDLE           g_hDebugHeap;

const UINT AllocationGranularity = 0x10000; // 64k

#define DEFAULT_CALLSTACK_FRAMES   8
#define MAX_CALLSTACK_FRAMES     1000
#define MIN_CALLSTACK_FRAMES       2

#define DEFAULT_GDI_TRACE_CALLSTACK_FRAMES 4
#define MAX_GDI_TRACE_CALLSTACK_FRAMES  8
#define MIN_GDI_TRACE_CALLSTACK_FRAMES  1

#define MAX_SYMBOLS_PATH   32*MAX_PATH

#define ADDRESS_NODE_SIZE (sizeof(CAddressNode) + sizeof(DWORD_PTR) * (g_nStackFrames - 1))

//=--------------------------------------------------------------------------=
// Called during process attach / detach to enable the default heap
//
BOOL  CreateDefaultHeap();
void  DestroyDefaultHeap();


void makehex(DWORD dwAddress , char *pszBuf);

class CHeapSpy;

extern CHeapSpy  *_pDefaultHeap ;


typedef struct
{
	UINT ulSize; // size of allocation in bytes
	DWORD ulOffset;
} MapFileLocator; // used for freelist


class CAddressNode : public ALLOCATION
  {
  public:
    void  DumpInst();
    void  VerifyHeaderTrailer(CHeapSpy *pHeapSpy);
    void  FillOnFree(CHeapSpy *pHeapSpy);         //Fill memory with a distinctive pattern on free.
	DWORD * GetStackArray();
    // We must override these to use the debug allocators.  Otherwise
    // we will infinite loop.
    //
    PVOID operator new    (size_t cb);
    void  operator delete (PVOID pv);
    UINT          m_uicStackAddr; // # of stack frames
    DWORD     m_pdwStackAddr[0]; // call stack array. If using MemMap, then it's the offset in the map

  };


//=--------------------------------------------------------------------------=
// We also maintain a lookup for filenames.  We do this because we must save
// off the filename in case the DLL it's connected to unloads.  But, we don't
// want to take the hit of allocating a filename for each allocation node.
//
struct STRINGLISTENTRY
  {
  STRINGLISTENTRY *pNext;
  LPCSTR           pszOriginal;
  TCHAR            szCopy[1];
  };

class CStringList
  {
  public:
    LPCSTR Add(LPCSTR psz);
    LPCSTR Find(LPCSTR psz);

    CStringList() : m_pListHead(NULL) {};
    ~CStringList();

  private:
    STRINGLISTENTRY *m_pListHead;
  };


typedef unordered_set<UINT, 
				hash<UINT>,
				equal_to<UINT>,
                MySTLAlloc<UINT> 
                > UINTSet;

typedef CMyStlWrap<UINTSet, MySTLAlloc<UINT>, equal_to<UINT> > UINTSetWrapper;

//=--------------------------------------------------------------------------=
// This tracks actual allocations.  We pass off instances of this a heap handle,
// and we also use it as the tracking mechanism for IMallocSpy
//
class CHeapSpy
  {
  public:
    PVOID  operator new(size_t size);
    void   operator delete(PVOID pv);

    HANDLE      m_hHeap;        //handle to heap that this instance is tracking
    CHeapSpy   *m_pNextHeapSpy; // pointer to next heap spy in chain
    LPCSTR      m_pszHeapName;  // descriptive name for the heap
    LPCSTR      m_pszFile;      // source file where VBCreateHeap was called
    UINT        m_uLine;	// line in source file
//    DECLARE_MUTEX(m_mxsSpy);    // Locking mutex

    CHeapSpy(HANDLE hHeap, 
        LPCSTR pszHeapName, 
        LPCSTR  pszFile,      // the file that created the heap
        UINT    uLine         // and the line where it was allocated
        );
    ~CHeapSpy();

    // These are used for actually manipulating the instance list
    //
    void  AddInst(PVOID pv, SIZE_T dwBytes, UINT ifrStackStart);
    void  ChangeInst(PVOID pvOld, PVOID pvNew, SIZE_T dwBytes, UINT ifrStackStart);
    void  DeleteInst(PVOID pv);
    void ReplaceInst(PVOID pv, CAddressNode *pNewInst);
    // Lookup functions
    //
    CAddressNode *FindInst(PVOID pv);
    BOOL          IsEmpty(UINT_PTR dwInst = INSTANCE_GLOBAL);

    CAddressNode *EnumReset();
    CAddressNode *EnumNext();

    // Diagnostics
    //
    void  AnalyzeInst(PVOID pv);
    void  DumpStatistics();
    void  HeapCheck();

    // Static lookup functions
    //
    static CHeapSpy  *FindCorrectHeap(PVOID pv);
    // Static diagnostics
    //
    static void       CheckAllHeaps();
    static void       DumpAllStatistics();
    static int  UpdateRetailHeapNames(BOOL fGetCountOnly);
    static CHeapSpy *IsValidHeap(CHeapSpy *pHeapSpy, BOOL fCheckHandle = false);
    static BOOL       IsValidHeapPtr(CHeapSpy *pHeapSpy, LPVOID pv);
    static bool     CanDetourHeap(HANDLE hHeap);

    int  m_nHeaderSize;
    int  m_nTrailerSize;



    CStringList   m_StringList;  //list of file names


public:





	typedef unordered_map<LPVOID, CAddressNode *,
									hash<LPVOID>,
									equal_to<LPVOID>,
                                    MySTLAlloc<pair<LPVOID, CAddressNode *>  >
                                     > AddrNodeInstSet;

    typedef CMyStlWrap<AddrNodeInstSet,  
                                    MySTLAlloc<pair<LPVOID, CAddressNode *> >,
									equal_to<LPVOID> > AddrSetWrapper;

    AddrSetWrapper *m_AddrSetWrapper;
    AddrNodeInstSet::iterator m_AddrNodeInstSetIterator;
    // stuff to track overall memory statistics
    //
    ULONG  m_cCurNumAllocs;          // Current number of allocations including dups from SysAlloc*
    SIZE_T m_cCurNumBytesAllocated;  // Current number of bytes allocated.
    SIZE_T m_cNumBytesAllocated;     // Total bytes allocated.
    ULONG  m_cNumAllocs;             // Total number of allocations ever done.
    
    ULONG  m_cStatCurNumAllocs;      // Current number of allocations without dups
    DWORD  m_dwTidOfHeapLock ; // did somebody calll HeapLock?        
    int m_nStackFramesToSkip; // # of stack frames to skip (we don't want to show too many from within VSAssert)

    static volatile LONG s_OverallCurAlloc; // These are overall statistics to since we
    static volatile LONG s_OverallCurBytes; // wouldn't mind the overall high water.


    static UINTSetWrapper *g_AssertOnStackFrame; // assert if any addr in this set is in the current call stack
    static UINTSetWrapper *g_AssertOnSeqNo;  // assert if the seq no we're currently freeing is in the set

  };

extern CHeapSpy  *_pHeapSpyList;

VOID     VsDebugFreeInternalEx            (CHeapSpy *phHeapSpy, PVOID pv);// Ex version assumes already critsect and heapspy valid for perf

template <class ConstructorArg, class SavedValue>
class BackupState
{
public:
    BackupState(const ConstructorArg & constructorArg, const SavedValue & savedValue) :
        m_constructorArg(constructorArg),
        m_savedValue(savedValue),
        m_fRestored(false)
    {
    }

    void Restore()
    {
        if (! m_fRestored)
        {
            DoRestore();
            m_fRestored = true;
        }
    }

    const SavedValue & OldValue()
    {
        return m_savedValue;
    }

    void ReInit()
    {
        VSASSERT( m_fRestored, "No need to reinitialize if restore has not been called");
        m_fRestored = false;
    }

protected:
    virtual void DoRestore() = 0;

    SavedValue m_savedValue;
    ConstructorArg m_constructorArg;
private:
    bool m_fRestored;
};



template <class T>
class BackupValue :
    public BackupState<T *, T>
{
public:
    BackupValue(_In_opt_ T * pMemory)
        : BackupState(pMemory, pMemory ? *pMemory : T())
    {
   }

    BackupValue(_In_opt_ T  *pMemory, T value)
        : BackupState(pMemory, value)
    {
    }

    ~BackupValue()
    {
        Restore();
    }

    void Init(_In_ T* pMemory)
    {
#if MSDEBUG
        VSASSERT(!m_constructorArg, "Double Init");
        VSASSERT(pMemory, "NULL Memory location to backup");
#endif MSDEBUG
        m_constructorArg = pMemory;
        m_savedValue = *pMemory;
    }

protected:
    void DoRestore()
    {
        if (m_constructorArg)
        {
            *m_constructorArg = m_savedValue;
        }
    }

protected:
};


class CSaveHeaderTrailerSize // set sizes to 0, restore in dtor
{
public:
    CSaveHeaderTrailerSize(CHeapSpy *pHeapSpy)
    {
        m_pHeapSpy = pHeapSpy;
        m_HeaderSize = pHeapSpy->m_nHeaderSize;
        m_TrailerSize = pHeapSpy->m_nTrailerSize;
        pHeapSpy->m_nHeaderSize = 0;
        pHeapSpy->m_nTrailerSize = 0;
    }
    ~CSaveHeaderTrailerSize()
    {
        m_pHeapSpy->m_nHeaderSize = m_HeaderSize;
        m_pHeapSpy->m_nTrailerSize = m_TrailerSize;
    }
private:
    int m_HeaderSize;
    int m_TrailerSize;
    CHeapSpy *m_pHeapSpy;
};


  // helper class to modify THREADGLOBAL's  m_ulDisableTrace
  class CDisableTrace
  {
    public:
    CDisableTrace();
    ~CDisableTrace();
    static BOOL CanDetour();

    private:
    THREADGLOBAL *m_ptd;
  };


// helpers for leak reporting

void PrintLeaks(void *pLeaks,
                size_t uicLeaks,
                size_t ElementSize,
                BOOL (__cdecl *IsEqual )(void *, void *),
                void (__cdecl *Print )(void *, UINT uicTimesLeaked, void * pPrintContext),
                void * pPrintContext);

void PrintCallstack(DWORD_PTR * pdwStackAddr, UINT uicStackAddr);

int  CompareCallstack(const DWORD_PTR * pLStackAddr, const UINT uicLStackAddr,
                      const DWORD_PTR * pRStackAddr, const UINT uicRStackAddr);

inline BOOL IsCreated(const char * pszFileName)
{
   return (pszFileName && 0xffffffff != GetFileAttributes(pszFileName));
}


void CurrentThreadIsBeingDetached();

// Note:  These must be a multiple of 8 to keep things aligned properly
//        on risc.
//
// On 64bit systems, they should be multiples of 16 to keep them properly alligned
extern  char *_szHeader;


extern char *_szTrailer;


///////////////////// Detours, so works with Ret binaries and VirtualAlloc


#define INVALID_TLS (DWORD)-1
#define TLS_THREAD_DETACHED ((LPVOID)-2)
extern DWORD            _dwTls ;
   

// we want to start detouring
void InitDetours(BOOL fTrackVirtualMem, BOOL fTrackHeap, int nSharedMemSize);


void ShutDownDetours();

enum IndirectInfoType
{
    IIType_None,
    IIType_OpenFile,
    IIType_CreateFile,
    IIType_CreateSection,
    IIType_OpenSection,
    IIType_MapSection,
    IIType_CustomCodeMarker,
    IIType_CustomCodeMarkerWithUserData,
	IIType_FileLoad,  //LdrDllNotification 
	IIType_FileUnload, //LdrDllNotification 
};

class CIndirectInfo
{
public:
    void  operator delete (PVOID pv)
    {
        DebugFree(pv);
    }
    
    static CIndirectInfo *CreateInstance(IndirectInfoType iType, int cbExtra)
    {
        auto nSize = sizeof(CIndirectInfo ) + cbExtra + 2;  /// add 1*2 for nullterm
        
        CIndirectInfo *ptr = (CIndirectInfo *)DebugAlloc(nSize);
        ZeroMemory(ptr, nSize);
        ptr->m_IndirectInfoType = iType;
        ptr->m_nLen = cbExtra;
        return ptr;
    }
    IndirectInfoType m_IndirectInfoType;
    union
    {
        struct
        {
            LPVOID m_BaseAddress;
        } IIType_Section;

        struct
        {
            // key = PVOID m_BaseAddress;
            HANDLE m_SectionHandle;
        } IIType_MapViewOfSection;

        struct
        {
            DWORD nMarkerInstance;  // for all codemarkers (custom and normal), the instance (0,1,2) of it.
            WORD nEventType; // 0=none, 1=Start 2=End
            WORD nDepthLevel; //
        } IIType_CustomCodeMarker;

		struct
        {
            DWORD nMarkerInstance;  // for all codemarkers (custom and normal), the instance (0,1,2) of it.
            WORD nEventType; // 0=none, 1=Start 2=End
            WORD nDepthLevel; //
            const VOID * pUserData;
            unsigned int cbUserData;
        } IIType_CustomCodeMarkerWithUserData;

        struct
        {
            LPVOID m_BaseAddress;
        } IIType_Notify;


	
	};
    int m_nLen; // for mapped sections, we copy the filename from the Sectionhandle allocation, if found
    WCHAR m_pwChar[];
};

typedef enum
{
    bt_None = 0,
    bt_VirtualAlloc,
    bt_MapViewOfFile,
    bt_HeapCreate,
    bt_HeapAlloc, // unused, but need to keep for compat
    bt_RegKey,
    bt_CreateFile,
    bt_ClrObject,
    bt_ClrClass,
    bt_ClrModule,
    bt_ClrAssembly,
    bt_ClrAppDomain,
    bt_CodeMarker,
    bt_TlsAllocFree,
    bt_ThreadCreate,
    bt_IndirectInfo, // some types have string info, like filename.
    bt_LoadResource,
    bt_TrackGCStacks,
    bt_Jit,
    bt_GCHnd,
    bt_Excpt, // ExceptionsThrown
    bt_GdiObject,
    bt_Max // unused
} BlockType;


HRESULT DoGetClrData(
    CAddressNode *pNode,
    DWORD fExpandSystemString,
    _Out_z_cap_(uicBuf) WCHAR * pszBuf, 
    UINT uicBuf
    );

ULONG  DoGetClrSize(BlockType bt, ClassID classId, ObjectID objectId);
// returns len in #elems of buf
int ElemTypeToString(
    _Out_z_cap_(nbufSize) WCHAR buf[], 
    int nbufSize, 
    CComPtr<IMetaDataImport> pIMetaDataImport,  
    CorElementType elemType, 
    PCCOR_SIGNATURE *pSig=0
    );

struct TrkBlock;

typedef pair<BlockType, LPVOID> TreeKey; // BlockType, Address

typedef pair<TreeKey, TrkBlock> PairAddrBlock;



typedef map<TreeKey, TrkBlock, less<TreeKey>, MySTLAlloc<PairAddrBlock > > ListTrkBlocks;
typedef CMyStlWrap<ListTrkBlocks, MySTLAlloc<PairAddrBlock>, less<TreeKey> > ListTrkBlockWrapper;


typedef vector<ObjectID, MySTLAlloc<ObjectID> > vecObjId;

typedef pair<ObjectID, ULONG> Range;

typedef pair<ObjectID, Range> PairObjRange;

typedef map<ObjectID, Range, less<ObjectID>, MySTLAlloc<PairObjRange> > MapRangeType;

typedef CMyStlWrap<MapRangeType, MySTLAlloc<PairObjRange>, less<ObjectID > > MapRange;


typedef pair<ULONG, ObjectID> PairIntObjId;

typedef pair<ULONG, vecObjId> PairIntObjvec;

typedef pair<ULONG, ULONG> PairInt;

typedef CMyStlWrap< vecObjId >  vecObjIdWrap;

typedef vector<PairInt, MySTLAlloc<ObjectID> > vecPairIntObjectIdType;

typedef CMyStlWrap< vecPairIntObjectIdType >  vecPairIntObjectId;

typedef map<DWORD, DWORD, less<DWORD>, MySTLAlloc<PairInt> > MapDWordDword;
typedef CMyStlWrap<MapDWordDword, MySTLAlloc<PairInt>, less<DWORD> > MapDwordDwordWrapper;



class CSymName
{
public:
    typedef enum {
        SymNameFlag_OK = 0x1,
        SymNameFlag_Error = 0x2,
    } SymNameFlag;
    void operator delete (PVOID pv)
    {
        DebugFree(pv);
    }
    static CSymName *CreateInstance(WCHAR *pszBuf, SymNameFlag flag = SymNameFlag_OK)
    {
        auto nLen = wcslen(pszBuf);
        auto nSize = sizeof(CSymName) + 2*nLen + 1;  // add 1 nullterm
        CSymName *ptr = (CSymName *)DebugAlloc(nSize);
        wcscpy(ptr->m_str, pszBuf);
		VSASSERT(nLen < 65536, "Sym Name too long?");
        ptr->m_nLen = (SHORT)nLen;
        ptr->m_dwSymFlags = flag;
        return ptr;
    }
    int GetSymName(_Out_cap_(uicBuf) WCHAR * pszBuf, UINT uicBuf)
    {
        int retval = 0;
        auto nchars = min(m_nLen, (int)(uicBuf -1));
        wcsncpy_s(pszBuf, uicBuf, m_str, nchars );
        pszBuf[nchars] = 0; //nullterm
        if (m_dwSymFlags == CSymName::SymNameFlag_OK)
        {
            retval = nchars;
        }
        else
        {
            retval = 0;
        }
        
        return retval;
    }
private:
    SHORT m_dwSymFlags;
    SHORT m_nLen;
    WCHAR m_str[];
};

typedef map<FunctionID, CSymName *, less<FunctionID>, MySTLAlloc<pair<FunctionID, CSymName * > > > MapSymNames;

typedef CMyStlWrap<MapSymNames, MySTLAlloc<pair<UINT_PTR, CSymName * > >, less<UINT_PTR> > MapDynStackSymNamesWrapper;

extern MapDynStackSymNamesWrapper *g_MapDynStackSymNamesWrapper;

bool AddToDynStackSymNames(UINT_PTR ip, bool fIsFunctionId);


// </STL stuff>


#define TrkBlock_SIGNATURE  0x6b6c4254   // TBlk


struct TrkBlock
{
    TrkBlock(SIZE_T dwSize) // ctor
    {
        ZeroMemory(this, sizeof(*this));  
        m_dwSize = dwSize;
#if MSDEBUG
        m_tbSig = TrkBlock_SIGNATURE; // so we can tell by inspection that this is one of ours
#endif MSDEBUG
    }
#if MSDEBUG
    DWORD m_tbSig;
#endif MSDEBUG
    SIZE_T m_dwSize;
    union
    {
        struct 
        {
            LPVOID m_reqAddress;// for Free, this is the BOOL result, for Alloc, user requested starting address
            // m_dwType for Alloc: MEM_COMMIT(0x1000) | MEM_RESERVE(0x2000) | MEM_RESET(0x80000)    
            // m_dwType for Free:|    MEM_DECOMMIT(0x4000) MEM_RELEASE(0x8000)
            DWORD m_dwType; 
//            DWORD m_flProtect;
        } VMAllocTrk;
        struct
        {
            DWORD m_dwDesiredAccess;
            DWORD m_dwNumberOfBytesToMap;
            
        } MapViewOfFileTrk;

        struct
        {
            HANDLE m_hHeap;
            LPCSTR m_pszHeapName;
//            DWORD m_flOptions;
//            DWORD m_dwInitialSize;
//            DWORD m_dwMaximumSize;
        } HeapCreateTrk;

        struct
        {
            HANDLE m_hHeap;
            DWORD m_dwFlags;            
        } HeapAllocTrk;

        struct
        {
            HKEY m_hRegKey;
        } RegKeyTrk;

        struct 
        {
            DWORD m_data; // could be Object, Class,Assembly,Module,AppDomain
        } ClrLoads;

        struct 
        {
            DWORD m_ClassId;
            USHORT m_nInstance;
            USHORT m_nCollected;
        } ClrClass;
        
        struct
        {
            DWORD m_ClassId;
            BYTE m_nMoved;      
            BYTE m_nGen; // the generation : Gen0, Gen1
            USHORT m_nGCsSurvived;
        } ClrObject;

        struct
        {
            DWORD m_CodeMarkerId;
            CIndirectInfo *m_pCIndirectInfoCustomCodeMarker; // used only for custom code markers
        } CodeMarker;
        
        struct
        {
            DWORD m_TlsIndex;
        } TlsAllocFree;

        struct
        {
            DWORD m_dwNewThreadId;
            PVOID m_ThreadStartRoutine;
        } ThreadCreate;

        struct
        {
            //LPVOID pKey; // could be HANDLE or BaseAddress
            CIndirectInfo *pCIndirectInfo;
        } IndirectInfo;

        struct 
        {
            FunctionID m_functionId;
            DWORD m_fIsSafeToBlock;
        } JitInfo;

        struct
        {
            HGLOBAL hResource;
            HRSRC hResInfo;
        } LoadResourceInfo;

        struct
        {
            BYTE genCollectedBitMap;
            BYTE Reason;
        } TrackGCStacksInfo;

        struct
        {
            GCHandleID handleId;
            ObjectID initialObjectId;
        } GCHndInfo;

        struct 
        {
            ObjectID objectIDExcept;
            ClassID classIDExcept;
        } ExcptInfo;

        struct
        {
            HGDIOBJ hgdiobj;
            BYTE GdiObjectType;

        } GdiObjInfo;
    };
    
};


#define CHECK_GC_LOST_OBJECTS 1

//c:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\include\xtree
//  class _Tree_nod struct _Node
// Dev10 : Microsoft (R) C/C++ Optimizing Compiler Version 16.00.30319.01 for x86
// Dev11 : Microsoft (R) C/C++ Optimizing Compiler Version 17.00.50206 for x86
#if _MSC_VER <1700
	#define Offset_AllocToTblk 5 // LEFT, PARENT, RIGHT, KEY1 (bt), KEY2(LPVOID)
#else
	#define Offset_AllocToTblk 6 // LEFT, PARENT, RIGHT, _Color, _IsNil, KEY1 (bt), KEY2(LPVOID)
#endif 


#define MANAGED_STACK_FLAG 0x80000000
#define InternalHeapToUse 0 //g_TrackMemClass->m_MemSpectHeap  // 0 means DebugAlloc g_TrackMemClass->m_MemSpectHeap

HRESULT DoForceGC(int fDoRCWCleanup, bool fFreezeThreads);

#define MAXCLASSNAMELEN 900
#define MAXCLASSNAMELENWITHSTRINGContent 2500
#define MAXElemTypeToStringLEN 600

int GetClassNameFromClassId(
    _Out_cap_ (noutBufSize) WCHAR outbuf[], 
    int noutBufSize, 
    ClassID classId, 
    ObjectID objectId, 
    bool fExpandSystemString = false
    );

#define REFBYME 1
#define REFTOME 2

class CProcessGC
{
    public:
    CProcessGC();
    ~CProcessGC();

    STDMETHODIMP GarbageCollectionStarted( int cGenerations, BOOL generationCollected[], COR_PRF_GC_REASON reason);

    void PostProcessGC(); // happens after CLR has done GC, but before GarbageCollectionFinished is called
    STDMETHODIMP GarbageCollectionFinished();

    STDMETHODIMP MovedReferences ( ULONG cmovedObjectIDRanges, ObjectID oldObjectIDRangeStart[], ObjectID newObjectIDRangeStart[], ULONG cObjectIDRangeLength[]);
    STDMETHODIMP SurvivingReferences ( ULONG cSurvivingObjectIDRanges, ObjectID objectIDRangeStart[], ULONG cObjectIDRangeLength[]);

    STDMETHODIMP ObjectReferences ( ObjectID objectID, ClassID classID, ULONG cObjectRefs, ObjectID objectRefIDs[]);
    STDMETHODIMP RootReferences2 ( ULONG cRootRefs, ObjectID rootRefIds[], COR_PRF_GC_ROOT_KIND rootKinds[], COR_PRF_GC_ROOT_FLAGS rootFlags[], UINT_PTR rootIds[]);

    STDMETHODIMP TrackObject(BlockType bt, DWORD dwObjectId, DWORD dwSize, DWORD dwExtraInfo = 0);
    STDMETHODIMP TrackObjectFree(BlockType bt, DWORD dwObjectId);
    STDMETHODIMP JITCompilationFinished( FunctionID functionID, HRESULT hrStatus, BOOL fIsSafeToBlock);
    STDMETHODIMP ExceptionThrown(ObjectID objectID);
    STDMETHODIMP HandleCreated ( GCHandleID handleId, ObjectID initialObjectId);
    STDMETHODIMP HandleDestroyed( GCHandleID handleId);

    ListTrkBlocks::iterator AddOrFindClassId(ClassID classID, DWORD dwSize);


    void FindPathsToObj();  // recursive: use no params to reduce stack depth
    ULONG m_nDepth;
    ObjectID m_objIdFrom;
    ClassID m_SystemStringClassId;

    vecObjId *m_pCurPathSoFar;
    void ReTrackObjectsAfterTogglingOnTracking();

    typedef vector<vecObjId,MySTLAlloc<ObjectID>> ResultsType; // vector of vectors
    typedef CMyStlWrap<ResultsType ,MySTLAlloc<ObjectID>  > ResultsPathToGCRoot;
    ResultsPathToGCRoot  *m_pResultsPathToGCRoot;  //vector of vectors
    

    void FreezeThreads( int CleanupRCWs = 1);
    STDMETHODIMP RuntimeSuspendFinished();
    void UnfreezeThreads();
    
    COR_PRF_GC_GENERATION_RANGE *m_pgeneration_range;
    BOOL m_generationCollected[COR_PRF_GC_LARGE_OBJECT_HEAP+1];
    ULONG m_ulNumBounds;
    int m_cGenerations;
    bool m_fInGC;
    bool m_fDoingObjectDump;
    bool m_fDidSignalEndDumpingGCRoots;
    bool m_fDidTurnOnTrkClrObjsForRetrack;

    HANDLE m_hVectoredExceptionHandler; 

    CMutex m_CritSect;
    ObjectID m_objLookRef; // obj for which to look for references 
    DWORD m_objLookParam; // 0 means all info (Refs(FromMe,ToMe), ObjRoots, RootPaths). 1 = FromMe, 2= ToMe
    bool m_fHaveToCatchUpObjectTracking;
    MapRange *m_pRangeMap;

    vecPairIntObjectId *m_pvecObjRefs; // refs to/from a single object

    typedef multimap<ObjectID, 
                                                    PairInt, 
                                                    less<ObjectID>, 
                                                    MySTLAlloc<pair<ObjectID, PairInt> > 
                                    > RootRefsType;
    
    typedef CMyStlWrap<RootRefsType,  
                                    MySTLAlloc<pair<ObjectID, PairInt> >,
                                    less<ObjectID> > RootRefs;

    RootRefs *m_pRootRefs;

#define GCROOTPATHBREADTHFIRST 1

    struct CObjRef
    {
        CObjRef(
#if GCROOTPATHBREADTHFIRST
            ObjectID objid,
#endif GCROOTPATHBREADTHFIRST
            ULONG cObjectRefs, 
            ObjectID objectRefIDs[]);
        ~CObjRef()
        {
            m_pvecObjId->freemem();
            DebugFree(this); // placement new
        }
#if GCROOTPATHBREADTHFIRST
        CObjRef *m_pCObjRefParent;
        ObjectID m_objectId;
        int m_nLevel;
#endif GCROOTPATHBREADTHFIRST

        CMyStlWrap<vecObjId> *m_pvecObjId;
        bool m_fBeenHereAlready;
    };

    typedef map<ObjectID, 
                                                    CObjRef *,
                                                    less<ObjectID>, 
                                                    MySTLAlloc<pair<ObjectID, CObjRef *> > 
                                     > ObjRefMapStlType;
    typedef CMyStlWrap<ObjRefMapStlType,  
                                    MySTLAlloc<pair<ObjectID, CObjRef *> >,
                                    less<ObjectID> > RefMap;

    RefMap *m_pRefMap;

#if GCROOTPATHBREADTHFIRST
    typedef queue<CObjRef*, list<CObjRef*, MySTLAlloc<CObjRef*>> > queueObjId;
    queueObjId *m_pobjqueue; // used for breadth first
#endif GCROOTPATHBREADTHFIRST

    struct 
    {
        volatile ULONG m_nGenCollected[COR_PRF_GC_LARGE_OBJECT_HEAP+1]; // 0 = # gen0 , 1= #gen1 etc
        volatile ULONG  m_nMoved;
        volatile ULONG m_nSurvived;
        volatile ULONG m_nCollected;
        DWORD m_g_dwThreadIdMemSpectBgdThreadGCAndCleanupRCW; // it's here because it isn't set til RCWs collected
    } m_GCStats;
    static int s_nGCNumber; // 0,1,2...
    static int s_nJitNumber; // 0,1,2...
    static int s_nExcptNumber; // 0,1,2...

    set<ObjectID> m_setObjects;
};

extern CProcessGC *g_pCProcessGC ;
extern void GarbageCollectOrCleanupRCWs();
extern void ShutDownRCWCleanupStuff();
std::string CompileAndExecute(std::string filename, std::string memsSpectBaseDllFileName);


std::pair<ULONG, ULONG> GetClrClassStats(ClassID classId);


int DoClrObjTrkToggle(DWORD dwParam);

void TMUpdateLoadedModules();


extern HANDLE (WINAPI * Real_HeapCreate)(
                    DWORD flOptions,
                    SIZE_T dwInitialSize,
                    SIZE_T dwMaximumSize
                ) ;


extern BOOL (WINAPI * Real_HeapDestroy)(
            HANDLE hHeap
        );


extern LPVOID (WINAPI * Real_HeapAlloc)(
            HANDLE hHeap,
            DWORD dwFlags,
            SIZE_T dwBytes
        );

extern BOOL (WINAPI * Real_HeapFree)(
        HANDLE hHeap,
        DWORD dwFlags,
        LPVOID lpMem
    );

extern LPVOID (WINAPI * Real_RtlAllocHeap)(
            HANDLE hHeap,
            DWORD dwFlags,
            SIZE_T dwBytes
        );

extern BOOL (WINAPI * Real_RtlFreeHeap)(
        HANDLE hHeap,
        DWORD dwFlags,
        LPVOID lpMem
    );

extern LPVOID (WINAPI *Real_HeapReAlloc)(
    HANDLE hHeap,
    DWORD dwFlags,
    LPVOID lpMem,
    SIZE_T dwBytes
);

extern BOOL (WINAPI *Real_HeapValidate)(
        HANDLE hHeap,
        DWORD dwFlags,
        LPCVOID lpMem
);

extern SIZE_T (WINAPI *Real_HeapSize)(
  __in  HANDLE hHeap,
  __in  DWORD dwFlags,
  __in  LPCVOID lpMem
);



typedef DWORD StackFrameIndex;

struct Framewalker{
    ULONG *BackTrace;
    DWORD nFrameIndex;
    UINT FramesToSkip;
    ULONG_PTR StackStart;
    ULONG_PTR StackEnd;



	typedef unordered_map<LPVOID, StackFrameIndex,
						hash<LPVOID>,
						equal_to<LPVOID>,
						MySTLAlloc<pair<const LPVOID, StackFrameIndex>  >
                                     > StackFrameMap;

    typedef CMyStlWrap<StackFrameMap,  
                                    MySTLAlloc<pair<LPVOID, StackFrameIndex> >,
									equal_to<LPVOID> > StackFrameMapWrapper;


    static StackFrameMapWrapper *g_StackFrameMapWrapper;

    static StackFrameIndex g_StackframeIndex;
    
    HRESULT Framewalker::WalkSomeNativeFrames(ULONG Fp);

    static StackFrameIndex GetOrCreateStackFrameIndexFromAddress(LPVOID pAddress, bool fIsManaged);

    static pair<LPVOID, StackFrameIndex> GetRealAddressFromStackFrameIndex(StackFrameIndex ndx);
    static void ShutDownFramewalker();
};

class MemMap // for mapping MemSpect internal storage to PageFile
{
	struct MappingData 
	{
		HANDLE _hFileMapping;
		LPVOID _mapBase;
		DWORD _ulBaseOffset;
		UINT _currentMappedViewSize;
		DWORD _dwFileSize; // total size of underlying mapped object 
	};

	typedef CMyStlWrap<vector<MapFileLocator, MySTLAlloc<MapFileLocator>>> vecMapFileLocator;
	typedef unordered_map<DWORD,
					vecMapFileLocator *, 
					hash<DWORD>,
					equal_to<DWORD>,
					MySTLAlloc<pair<DWORD, vecMapFileLocator *>>
				> freeListHashMap;
	typedef CMyStlWrap<freeListHashMap,
			MySTLAlloc<pair<DWORD, vecMapFileLocator *>>,
			equal_to<DWORD>
			> freeList;
	static freeList *g_FreeLists;
private:

	static HANDLE _hFileHandle; // for Paging File, INVALID_HANDLE_VALUE

	static void Resize(DWORD newSize);
	static void UnmapView(LPVOID dwAddr);
	static DWORD _uiViewSize;
	static DWORD _initialSize;
	static MappingData _currentMapping;
	static const DWORD nRoundVal = 1;// << 2; // power of 2 >=4
	static MappingData CreateAMapping(DWORD newSize);

	static DWORD Roundit(DWORD dwSize)
	{
		auto res = dwSize;
		auto remainder =res %nRoundVal;
		if (remainder !=0)
		{
			res += nRoundVal - remainder;
		}
		return res;
	}
public:
	static void CompactIt();
	static LPVOID MapView(DWORD dwOffset, int numBytesToMap, MappingData *mappingData = nullptr);
	static void VerifyStuff();
	static void AddStack(CAddressNode *pn);
	static void RemoveStack(CAddressNode *pn);
	struct stats
	{
		DWORD nFileMaps;
		DWORD nCompactions;
		DWORD ulOffsetFreeMem;
		DWORD nCurAlloc;
		DWORD ulMemSize;
		DWORD nMapViews;
		DWORD nFreeListSize;
	};
	static stats _stats;

};


SECURITY_ATTRIBUTES *GetSecurityAttributes();

WCHAR g_szGlobal[]; // either null or "Global\"


    
    typedef enum ProcMsgVerbs : byte
    {
        GetMemSpectVersion = 0, // no params, returns a String
        GetSharedMem = 1, // no params, returns 9 DWORDs: hMappedFile, SharedMemSize, g_hDebugHeap, GetProcessHeap(), MemSpectHeap, g_fHandle4gigStacks, SeqNoPerfIdle,Tid(memspect),Offset_AllocToTblk  Puts MEMORYSTATUSEX in shared mem
        GetIniFile = 2,  // no params, returns a String: fullpath to ini file
        GetHeapStats = 3,  //noparams,  0 is heaphandle, 1 is heapname, 2 is heapnamelen, 3 is Filename, 4 is Filenamelen, 5 is LineNo
        GetHeapAllocs = 4, // 3 params: hHeap (CHeapSpy), SeqnoLo, SeqnoHi  (-1 means no seq filter)
        GetFirstHeapBlock = 5, // 2 params: hHeap, nIndex (starts at 0), returns Num Dword ptrs to Blks (0 indicates no more). fills shared mem with array of ptrs.
        GetNextHeapBlocks = 6, // ditto
        ResolveSymbol = 7,  // 2 param: address, fIsFunctionId. string is in shared mem, like "f:\dd\wpf\src\graphics\core\common\display.cpp(889) : wpfgfx_v0400.dll!CDisplaySet::Init + 20 bytes"
        ResolveSymbolNoLineInfo = 8, // 1 param: address. Will not include line no info like "wpfgfx_v0400.dll!CDisplaySet::Init + 20 bytes"
        ResolveStackFrames = 9, // 3 params: address of CAllocation heap block, nCallStackIndex, nCallStackMax, returns ptr[0] = nCnt of strings, then consecutive null term'd strings for each stack frame
        ForceGC = 10,    // 1 param: 0 means don't call RCWCleanup, 1 means call it.
        GetFirstObjRefs = 11, // 2 param: 1 = the objectId 2 = thing to get:(0 = all, 1 = refFROMME, 2 = refTOME) . Will do a GC. 
        GetNextObjRefs = 12, // 2 param: initial index into array if not all obtained in GetFirstObjRefs. (2nd param ignored)
        ThreadsFreeze = 13,// 1 params: cleanup RCW ? Returns 1 DWORD: SeqNo of freeze
        ThreadsUnFreeze = 14, // 0 params: Returns 1 DWORD: SeqNo of freeze
        GetClrData = 15, // 2 params:Blk, fIncludeSystem.String. could be Assembly, Module, Appdomain, ClassID, returns 3 ints, string (Class/Module/Appdomain name) in shared mem
        GetClassNameFromId = 16, //given classid, return name in pipe. p0= ClassId, p1 = dwObjectId, p2 = fExpandSystem. If ClassId is null, will get from ObjectId
        GetClrSize = 17, // 3 params:BlkType,   ClassID 3rd param is objectId or NUll
        GetClrClsLayout = 18, // 1 param: ClassId, returns in pipe: DWORD nClassSize, DWORD nCntMems, (int iOffSet, szFieldName)nCntMems
        GetGCStatAddr = 19,// 0 params. Returns addr of GC Stat struct, or 0 if not avail
        ClrObjTrk = 20, // 1 param: 0, 1, or 2 . 2 means just return current value. 0, 1 to turn on/off CLR Obj Tracking (returns 3 DWORDS in shared mem: cur setting, g_pCProcessGC, and SeqNo)
        GetCodeMarkerName = 21, // 1 param: code marker id, returns string of marker in shared mem
        GetCodeMarkers = 22, // 2 params: Lo,Hi seqno. If both 0, gets all. For CodeMarkerMerge
        GotCustomCodeMarker = 23, // 5 params int EventType, int DepthLevel, int MarkerId, int LenStr, WCHAR strScenarioname
        GetProcessHeapHandles = 24,    // no param: cnt, then all process heap handles in sharedmem
        DoHeapWalk = 25, // 3 params: handle, index, fOnlyWantRegions. If index=0, it's first block. Returns # blks in 0, then consecutive PROCESS_HEAP_ENTRY structs
        GetStackAddresses = 26, // 1 param: heapallocationblk. Puts in shared mem DWORD nCnt of addresses (should match m_uicStackAddr) followed by nCnt DWORDS of the call stack frame addresses
        DoPivot = 27, //  3 params: hHeap, index, StackFrameAddr to pivot. Returns in shared mem PAllocationStruct: return value = # of items. 
        LotsOfThreads = 28, // Create lots of threads nThreads, nSleep, nLeak
        GetClrObjDump = 29, // no params: will write to named pipe all CLR Objects and their references. 
        DoVirtualAlloc = 30, // params of VirtualAlloc
        DoVirtualFree = 31, // params of VirtualFree
        TranslateStackIndex = 32,// only needed if fHandle4gigStacks: 1 param: 0 means get all stack index addresses, non-zero means get just that one. Written to pipe
        AssertSeqNo = 33,    // P0: 0 add p1 as SeqNo to assert on. P0:1 remove p1. P0:2 return all current. Param0:2 means clear all
        AssertStackFrame = 34,    // P0: 0 add p1 as frame to assert on. P0:1 remove p1. P0:2 return all current. Param0:2 means clear all
        GetThreadInfo = 35,  // no params; returns 4 DWORDS for each thread: (Id, SeqNo, stackBase, stackLimit)
        SetHeapName = 36, // Symbol resolved in child process: 2 params: Handle, nBytes, bytestream
        GhostAllocSpec = 37, // specify 0 to turn ghost off, 1 to turn on
        MyGetModuleFileName = 38, //
        MyGetModuleHandle = 39, // GetModuleHandle of a dll specified in pipe
        FreeStackMem = 40,   // 1 param: hHeap (CHeapSpy): free stack mem for specified heap
        TrackingMode = 41,   // 1 param: -1 means get the current tracking mode. else sets TrackingMode to the param
        SuspendResumeImmersive = 42,	// NOP unless Immersive 2 params: P0=1 means Resume, 0 means Suspend   p1 = TID to resume
        GetMemStats = 43, // Get the memstats list that accumulated via CustomCodeMarker
        GetCodeMarkerActions = 44, // Get the actions recorded, if any
		CompileAndExecuteFile = 45, // 1 param: full path filename to execute. Will compile and execute C# code

        // these are from parent to child (using pipe
        GotCodemarker = 100, // 4 DWORDS: codemarker ID and seqno, Extrainfo, InstanceNum
        SetCodeMarkerAction = 101,    // p0 = action, p1 = # of markers , p2...pN = markerids. If P1=0, means clear all actions
        ResolveSymbolFromTarg = 102, // p0: sym to resolve. p1 = fIncludeFileAndLineno returns: len prefixed str of sym name in pipeFromTarg
        UpdateStatusMessage = 103,    // p0: string to display in status
        HeapLeak = 104,       // When HeapDestroy, AllocationStructs of any leftovers p0: HeapHandle, p1: CAddressNode *. Repeat for multiple leaks
        GhostAllocLog = 105,    // when an alloc meeting AllocLocSpec criteria, send it's info
        GhostAllocFree = 106,    // when a logged alloc gets freed, we'll send a msg telling 
        LoadResourceEvent = 107,   // when a resource is loaded: sends hModule, hResInfo, call stack
        CrashTargetProcess = 108,  // p0 = 0 means crash, p0=1 means hang, p1 = Int 3 (DebugBreak()). For crash, p1 = 0 means constant crash, p1>0 means unique int for crash. Force an AV in target process to test crash scenarios
		GetPEBaseAddressFromMem = 109,  // p0 = hModule as IntPtr : get the PE Base Address as set by ASLR


        //these are same in both child-parent and parent-child direction        
        NoOp = 124,   // no op: for perf measurements
        UnKnownVerb =  125,
        VerbDone = 126, // no params: indicates the prior cmd optionally put data in shared mem and it's ready, or the command is done
        Quit = 127 //',1 param: if from child (MemSpect.exe) to parent(Devenv), then Bit0=0 means quit, but don't terminate child, bit0=1 means terminate parent & child. Bit1=0 means unfreeze first if frozen. Bit1=1 means ignore Frozen status
        
    } ProcMsgVerbs;

int SendMsgToChild(ProcMsgVerbs vrb, 
                int nParamBytes, 
                _In_ char *params, 
                _In_count_(outbuf) int outbufSize = 0, 
                _Out_ char *outbuf = 0
                ) ;

CLINKAGE void ENTRYPOINT CustomCodeMarker(
                    const WCHAR *MarkerName, 
                    int nEventType, 
                    int nDepthLevel, 
                    DWORD markerId, 
                    _In_opt_bytecount_(cbUserData) const void *pUserData, 
                    unsigned long cbUserData);



/* <MemStat Stuff> */
//#pragma pack(show)
#pragma pack(push, mypack, 4)
//#pragma pack(show)
struct AllocData
{
	DWORD count;
	LONGLONG size; // bytes
};

struct MemStatDetail // size with 4 byte pack is 2 * 12 bytes
{
	AllocData Alloc;
	AllocData Free;
};

struct MemStats
{
	ULONG SeqNo; // seq no at which snapped
	DWORD CodeMarkerId; // codemarker which triggered snap
	MemStatDetail HeapAllocs;
	MemStatDetail ClrObjs;
	MemStatDetail ClrClasses;
	MemStatDetail ClrOther; //module, assembly,appdomain
	MemStatDetail VirtualAlloc;
};
#pragma pack(pop, mypack)

typedef enum _CodeMarkerActionEnum // any changes here, make sure Nativeimports.vb matches
{
	CodeMarkerAction_TakeMemStatSnapShot = 0x0001,
	CodeMarkerAction_TakeMegaSnapshot = 0x0002,
	CodeMarkerAction_Freeze = 0x0004, // freeze all but MemSpect threads
	CodeMarkerAction_ShowInStatusMessage = 0x0008,
	CodeMarkerAction_Crash = 0x0010,
	CodeMarkerAction_Hang = 0x0020, // doesn't freeze threads, but causes deadlock for specified duration (CodeMarkerParameter)
	CodeMarkerAction_Sleep = 0x0040, // sleep for specified duration (CodeMarkerParameter)
	CodeMarkerAction_DebugBreak = 0x0080,
	CodeMarkerAction_Recur = 0x0100, // recur for specified levels (CodeMarkerParameter)
} CodeMarkerActionEnum;

struct CodeMarkerAction
{
	CodeMarkerAction(DWORD dw)
	{
		action = dw;
	}
	DWORD action;
};
extern MemStats g_memStats;// this is the single static instance that accumulates stats. 

typedef unordered_map<DWORD, CodeMarkerAction, 
	hash<DWORD>,
	equal_to<DWORD>,
	MySTLAlloc<pair<DWORD, CodeMarkerAction>>> CodeMarkerActionSet;

typedef CMyStlWrap<CodeMarkerActionSet, MySTLAlloc<pair<DWORD, CodeMarkerAction > >, equal_to<DWORD> > CodeMarkerActionSetWrapper;

extern CodeMarkerActionSetWrapper *g_CodeMarkerActions;

// when desired, a snap of the g_memStats is taken and added to a vector
typedef CMyStlWrap < vector < MemStats, MySTLAlloc<MemStats >>, MySTLAlloc<MemStats>  > MemStatWrapper;
extern MemStatWrapper *g_memStatsVec; // assert if any addr in this set is in the current call stack


/* </MemStat Stuff> */


HRESULT STDMETHODCALLTYPE MyGetModuleMetaData( 
/* [in] */ ModuleID moduleId,
/* [in] */ DWORD dwOpenFlags,
/* [in] */ REFIID riid,
/* [out] */ IUnknown **ppOut);

HRESULT STDMETHODCALLTYPE MyGetClassLayout( 
    /* [in] */ ClassID classID,
    /* [out][in] */ COR_FIELD_OFFSET rFieldOffset[  ],
    /* [in] */ ULONG cFieldOffset,
    /* [out] */ ULONG *pcFieldOffset,
    /* [out] */ ULONG *pulClassSize);


// Immersive stuff
extern WCHAR *g_AppContainerNamedObjectPath;
extern WCHAR *g_packageFullName;

typedef LONG (WINAPI *PFNGetPackageFamilyName )(
			_In_ HANDLE               hProcess,
			_Inout_ UINT32 *packageFamilyNameLength,
			_Out_opt_ PWSTR packageFamilyName
);

typedef LONG (WINAPI *PFNGetPackageFullName )(
			_In_ HANDLE               hProcess,
			_Inout_ UINT32 *packageFullNameLength,
			_Out_opt_ PWSTR packageFullName
);


CLINKAGE UINT32 ENTRYPOINT GetPackageFullNameFromProcess(
	_In_ HANDLE hProcess,
	_Inout_ LPWSTR pPackageFullName
	);

extern PFNGetPackageFullName g_PFNGetPackageFullName;

typedef LONG (WINAPI *PFNGetCurrentPackagePath)(
			_Inout_ UINT32 *packagePathLength,
			_Out_opt_ PWSTR pPackagePath
);

CLINKAGE UINT32 ENTRYPOINT MyGetCurrentPackagePath(
			_Inout_ UINT32 *packagePathLength,
			_Out_opt_ PWSTR pPackagePath
	);

CLINKAGE UINT32 ENTRYPOINT GetPackageFamilyNameFromProcess(
	_In_ HANDLE hProcess,
	_Inout_ LPWSTR pPackageFamilyName
	);


CLINKAGE UINT32 ENTRYPOINT GetPathOfAppContainerNamedObject(
	_In_ LPWSTR pPackageFamilyName,
	_Out_ LPWSTR pAppContainerNamedObjectPath
	);

CLINKAGE UINT32 ENTRYPOINT PackageResumeSuspend(
					_In_ LPWSTR pPackageFullName,
					_In_ int	 nResume);

CLINKAGE  int ENTRYPOINT GetCodeMarkerIdFromName(const char *name); // this is very slow


#endif // _INC_MEM_H

