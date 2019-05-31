#include "pch.h"
#include "vsassert.h"
#include "mem.h"

typedef enum {
    ArenaBlkType_Header=0x101,
    ArenaBlkType_Alloc=0x102
} ArenaBlkType;

class ArenaAllocData; // fwd declare
class ArenaHdrInfo; // fwd declare



typedef map<PVOID, ArenaAllocData *, less<PVOID>, MySTLAlloc<pair<PVOID, ArenaAllocData *> > > ArenaAllocMap;

typedef CMyStlWrap<ArenaAllocMap,  
                                MySTLAlloc<pair<PVOID, ArenaAllocData *> >,
                                less<PVOID> > WrappedArenaAllocMap;




class ArenaCommonInfo
{
public:
    ArenaBlkType m_ArenaBlkType;
    size_t m_dwUserData;
};

DWORD g_dwArenaId = 0; // increment for each header
// one of these per Arena ( nrlsAllocator) (which can have many allocations)
class ArenaHdrInfo : public ArenaCommonInfo
{

public:
    DWORD m_CntEverAlloc;
    ULONG m_SizeEverAlloc;
    DWORD m_CntCurLive;
    DWORD m_SizeCurLive;
    ArenaAllocData *m_pMostRecentAllocation; // ptr to the last block allocated (most recent. 0 if none)
    BOOL m_fDidHaveAFree; // if there were any individual Free, then the linklist for FreeToMark is broken
    DWORD m_dwArenaId;
    WrappedArenaAllocMap *m_pWrappedArenaAllocMap;
    DWORD m_NameLen;
    char m_szArenaName[];
};



// one of these per allocation
class ArenaAllocData : public ArenaCommonInfo
{
public:
    PVOID m_pData; // Ptr to actual allocation 
    ArenaHdrInfo *m_ArenaHdrInfo; /// ptr to Arena containing this alloc
    size_t m_dwAllocSize;
    ArenaAllocData *m_pPriorAllocation; // ptr to prior one (0 if none): backwards link list
};

static CHeapSpy *g_pCHeapSpyArenas = NULL;

#undef HeapAlloc
#undef HeapFree
#undef HeapCreate

#define LockCritSect  CLock cl(&s_mxsStackWalk);


UINTSetWrapper *g_ArenaHeaderSet = 0;; // set of arena headers




#if MSDEBUGxx
    void VerifyIntegrity(ArenaHdrInfo *pArenaHdrInfo)
    {
        int nCnt = 0;
        auto pData = pArenaHdrInfo->m_pMostRecentAllocation;
        while (pData)
        {
            //VSASSERTF((pData->m_pPriorAllocation == 0 || pData->m_pPriorAllocation->m_pNextAllocation == pData ,"Verify prior->next != me"));
            //pData= pData->m_pPriorAllocation;
            nCnt++;
        }
        VSASSERTF((nCnt == pArenaHdrInfo->m_CntCurLive,"Verify cnt != cntCurLive  %  %d", nCnt, pArenaHdrInfo->m_CntCurLive));

    }
#else
#define VerifyIntegrity(a)
#endif MSDEBUG

/*
note:
Must set JustTheseProcesses in MemSpect.ini to include your process name
JustTheseProcesses = ;devenv;vswinexpress;cppclient;

or
You can use the MemSpect launcher to do so (and it will automatically put the “processname” in the JustTheseProcesses
Just point “Program” to link.exe and add the params in the “Args”, then hit “Start”


if you get stackoverflows like this while debugging:
MemSpectDll.dll!Mine_RtlFreeHeap(void * hHeap, unsigned long dwFlags, void * lpMem) Line 3887	C++
ntdll.dll!RtlDebugFreeHeap(void * HeapHandle, unsigned long Flags, void * BaseAddress) Line 832	C
ntdll.dll!RtlpFreeHeap(_HEAP * Heap, unsigned long Flags, _HEAP_ENTRY * BusyBlock, void * BaseAddress) Line 6823	C
ntdll.dll!RtlFreeHeap(void * HeapHandle, unsigned long Flags, void * BaseAddress) Line 2105	C
MemSpectDll.dll!VsDebugFreeInternalEx(CHeapSpy * pHeapSpy, void * pv) Line 1087	C++
MemSpectDll.dll!Mine_RtlFreeHeap(void * hHeap, unsigned long dwFlags, void * lpMem) Line 3887	C++
ntdll.dll!RtlDebugFreeHeap(void * HeapHandle, unsigned long Flags, voijustt * BaseAddress) Line 832	C
it's because the debugger causes the debug heap to be used.
To disable debug heap:
setx _no_debug_heap 1
then restart VS

*/

/*
Sample client code from native:

struct Chain
{
	Chain *pNext;
};
Chain *g_pChain = 0;
void MakeADeepStack(int nLevel)
{
	if (nLevel > 0)
	{
		MakeADeepStack(nLevel - 1);
	}
	else
	{
		Chain *ptr = (Chain *) HeapAlloc(GetProcessHeap(), 0, sizeof(Chain));
		if (ptr)
		{
			ptr->pNext = g_pChain;
			g_pChain = ptr;
		}
	}
}

typedef PVOID(WINAPI *PArenaCreated)(HANDLE hHeap, WCHAR * ArenaName, DWORD dwUserData);
typedef BOOL(WINAPI *PArenaAllocation)(DWORD hArena, DWORD dwSize, PVOID addressAlloc, DWORD dwUserDefinedType);
typedef BOOL(WINAPI * PArenaFree)(DWORD hArena, PVOID pAddressToFree);

typedef BOOL(WINAPI *PArenaRelease)(DWORD hArena, PVOID Mark);
typedef BOOL(WINAPI *PArenaDestroy)(DWORD hArena);
typedef void(WINAPI *PCustomCodeMarker)(
	WCHAR *MarkerName,
	int nEventType,
	int nDepthLevel,
	DWORD markerId,
	_In_opt_bytecount_(cbUserData) const void *pUserData,
	unsigned long cbUserData
	);

int APIENTRY _tWinMain(_In_ HINSTANCE hInstance,
	_In_opt_ HINSTANCE hPrevInstance,
	_In_ LPTSTR    lpCmdLine,
	_In_ int       nCmdShow)
{
	UNREFERENCED_PARAMETER(hPrevInstance);
	UNREFERENCED_PARAMETER(lpCmdLine);
	if (IsDebuggerPresent())
	{
		MessageBoxA(0, "danger: set no_debug_heap=1", 0, 0);
	}
	else
	{
		MessageBoxA(0, "hey you can attach debugger if you want", 0, 0);
	}

	MSG msg;
	HACCEL hAccelTable;
	auto hlib = LoadLibrary(L"c:\\MemSpect\\MemSpectDll.dll");
	PArenaCreated pArenaCreated = (PArenaCreated) GetProcAddress(hlib, "_ArenaCreated@12");
	PArenaAllocation pArenaAllocation = (PArenaAllocation) GetProcAddress(hlib, "_ArenaAllocation@16");
	PArenaFree pArenaFree = (PArenaFree) GetProcAddress(hlib, "_ArenaFree@8");
	PArenaRelease pArenaRelease = (PArenaRelease) GetProcAddress(hlib, "_ArenaRelease@8");
	PArenaDestroy pArenaDestroy = (PArenaDestroy) GetProcAddress(hlib, "_ArenaDestroy@4");
	PCustomCodeMarker pCustomCodeMarker = (PCustomCodeMarker) GetProcAddress(hlib, "_CustomCodeMarker@24");

	PVOID hArena = pArenaCreated(0, L"MyArena", 10);
	pArenaAllocation((DWORD) hArena, 10, 0, 0);
	int nIter = 1;
	int nMax = 1000000*2;
//	nMax = 10000;
//	nMax = 2;
	for (int fragment = 0; fragment < nIter; fragment++)
	{
		for (int i = 0; i < nMax ; i++)
		{
			pCustomCodeMarker(L"foo", 0, 0, 1, 0, 0);
	//		MakeADeepStack(1);
		}
		if (fragment + 1 < nIter)
		{
			//while (g_pChain)
			//{
			//	Chain*tmp = g_pChain->pNext;
			//	HeapFree(GetProcessHeap(), 0, tmp);
			//	g_pChain = tmp;

			//}
		}
	}




See also: C:\MemSpect\Misc\NorlsAllocator.cpp

For managed, sample client code, see Arenaclient.Xaml.vb
*/


//todo: HEAP_ZERO_MEMORY flag on Arena or alloc

// hHeap parameter is ignored
CLINKAGE PVOID ENTRYPOINT ArenaCreated(HANDLE hHeap, WCHAR * ArenaName, DWORD dwUserData)
{
    if (!g_TrackArenas)
    {
        return 0;
    }
    LockCritSect;
    if (g_pCHeapSpyArenas == NULL)
    {
#if MSDEBUG
//        MessageBoxA(0,"dbg","",0);
#endif MSDEBUG
		// create a heap to store arena info. This will be detoured.
        hHeap = HeapCreate(0,0,0);
        g_pCHeapSpyArenas = CHeapSpy::IsValidHeap((CHeapSpy *) hHeap, /*fCheckHandle*/true);
        VSASSERTF((g_pCHeapSpyArenas != 0,"err getting g_pCHeapSpyArenas (Check JustTheseProcesses in INI file)"));
        g_pCHeapSpyArenas->m_pszHeapName = g_pCHeapSpyArenas->m_StringList.Add("_Arenas");
        g_pCHeapSpyArenas->m_nStackFramesToSkip = 2;
        g_ArenaHeaderSet = new (DebugAlloc(sizeof(UINTSetWrapper))) UINTSetWrapper(MySTLAlloc<UINT>(InternalHeapToUse));
    }
    else
    {
        hHeap = g_pCHeapSpyArenas->m_hHeap;
    }
    int nNameLen = 0;
    char *szArenaName=0;
    if (ArenaName != 0)
    {
        USES_CONVERSION;
        szArenaName=W2A(ArenaName);

        nNameLen = strlen(szArenaName);
    }
    // the HeapAlloc is detoured to track stacks:
    ArenaHdrInfo *pArenaHdrInfo = (ArenaHdrInfo *)HeapAlloc(hHeap, HEAP_ZERO_MEMORY, sizeof(ArenaHdrInfo) + nNameLen + 1);
    pArenaHdrInfo->m_ArenaBlkType = ArenaBlkType_Header;
    pArenaHdrInfo->m_dwUserData = dwUserData;
    pArenaHdrInfo->m_pMostRecentAllocation = NULL;
    pArenaHdrInfo->m_dwArenaId = InterlockedIncrement(&g_dwArenaId);
    if (nNameLen)
    {
        strcpy(pArenaHdrInfo->m_szArenaName, szArenaName);
        pArenaHdrInfo->m_NameLen = nNameLen;
        pArenaHdrInfo->m_szArenaName[nNameLen] = 0;
    }
    g_ArenaHeaderSet->m_pStlType->insert((DWORD)pArenaHdrInfo);
    VerifyIntegrity(pArenaHdrInfo);
    return pArenaHdrInfo;
}

CLINKAGE BOOL ENTRYPOINT ArenaAllocation(DWORD hArena, DWORD dwSize, PVOID addressAlloc, DWORD dwUserDefinedType)
{
    if (!g_TrackArenas)
    {
        return true;
    }
    LockCritSect;
    ArenaHdrInfo *pArenaHdrInfo = (ArenaHdrInfo *)hArena;
#if MSDEBUG
    VSASSERT(hArena != 0,"err ArenaAllocation: hArena is null?");
    VSASSERT(g_pCHeapSpyArenas != 0,"err ArenaAlloc g_pCHeapSpyArenas is null?");
    VSASSERTF((g_ArenaHeaderSet->m_pStlType->find(hArena) != g_ArenaHeaderSet->m_pStlType->end(),"ArenaAllocation hArena not found %x", hArena));
    VSASSERT(pArenaHdrInfo->m_ArenaBlkType == ArenaBlkType_Header,"ArenaAllocation: blkType invalid");
#endif MSDEBUG
    if (pArenaHdrInfo->m_pWrappedArenaAllocMap == 0)
    {
        pArenaHdrInfo->m_pWrappedArenaAllocMap = new (DebugAlloc(sizeof(WrappedArenaAllocMap))) 
                    WrappedArenaAllocMap(less<PVOID>(),MySTLAlloc<pair<PVOID, ArenaAllocData *> >(InternalHeapToUse));
    }
    ArenaAllocData *pArenaAllocData = (ArenaAllocData *)HeapAlloc(g_pCHeapSpyArenas->m_hHeap, 0, sizeof(ArenaAllocData));

    pArenaAllocData->m_ArenaHdrInfo = pArenaHdrInfo;  // ptr to container
    pArenaAllocData->m_ArenaBlkType = ArenaBlkType_Alloc;
    pArenaAllocData->m_pData = addressAlloc;
    pArenaAllocData->m_dwAllocSize= dwSize;
    pArenaAllocData->m_dwUserData = dwUserDefinedType;
    pArenaHdrInfo->m_CntEverAlloc +=1;
    pArenaHdrInfo->m_CntCurLive +=1;
    pArenaHdrInfo->m_SizeEverAlloc += dwSize;
    pArenaHdrInfo->m_SizeCurLive += dwSize;

    pArenaAllocData->m_pPriorAllocation = pArenaHdrInfo->m_pMostRecentAllocation;

    pArenaHdrInfo->m_pMostRecentAllocation = pArenaAllocData;

    pArenaHdrInfo->m_pWrappedArenaAllocMap->m_pStlType->insert(pair<PVOID, ArenaAllocData *>(addressAlloc, pArenaAllocData));

    VerifyIntegrity(pArenaHdrInfo);

//    memset(addressAlloc, 0xAF, dwSize);

    return TRUE;
}


// free a single alloc in arena (JScript does this and maintains a freelist)
CLINKAGE BOOL ENTRYPOINT ArenaFree(DWORD hArena, PVOID pAddressToFree)
{
    if (!g_TrackArenas)
    {
        return true;
    }
    LockCritSect;
    ArenaHdrInfo *pArenaHdrInfo = (ArenaHdrInfo *)hArena;
#if MSDEBUG
    VSASSERTF((hArena != 0,"err ArenaFree: hArena is null?"));
    VSASSERTF((g_pCHeapSpyArenas != 0,"err ArenaFree g_pCHeapSpyArenas is null?"));
    VSASSERTF((g_ArenaHeaderSet->m_pStlType->find(hArena) != g_ArenaHeaderSet->m_pStlType->end(),"ArenaAllocation hArena not found %x", hArena));
    VSASSERTF((pArenaHdrInfo->m_ArenaBlkType == ArenaBlkType_Header,"ArenaFree: blkType invalid"));
#endif MSDEBUG
    BOOL res = false;

    auto resFind = pArenaHdrInfo->m_pWrappedArenaAllocMap->m_pStlType->find(pAddressToFree);
    if (resFind != pArenaHdrInfo->m_pWrappedArenaAllocMap->m_pStlType->end())
    {
        ArenaAllocData *pFreedData = resFind->second;
        pArenaHdrInfo->m_CntCurLive -=1;
        pArenaHdrInfo->m_SizeCurLive -= pFreedData->m_dwAllocSize;

        // cant memset because the page might be freed.
//        memset(ptrData->m_pData, 0xAF, ptrData->m_dwAllocSize); // fill mem that's been freed

        if (pArenaHdrInfo->m_pMostRecentAllocation != pFreedData) // if we're not freeing the most recent, we're breaking the linklist
        {
            pArenaHdrInfo->m_fDidHaveAFree = true; // linklist is broken, so no freetomark allowed
        }
        else
        {
            pArenaHdrInfo->m_pMostRecentAllocation = pFreedData->m_pPriorAllocation;
        }
        auto resHfree= HeapFree(g_pCHeapSpyArenas->m_hHeap, 0, pFreedData);
        VSASSERT(resHfree,"ArenaFree: failed to heapFree");
        auto resFree = pArenaHdrInfo->m_pWrappedArenaAllocMap->m_pStlType->erase(resFind);
//        VSASSERT(pArenaHdrInfo->m_CntCurLive==0 || resFree != pArenaHdrInfo->m_pWrappedArenaAllocMap->m_pStlType->end(),"free failed in ArenaFree");

        res = true;
    }
    else
    {
#if MSDEBUG
//		VSASSERTF((false,"ArenaFree: addr not found to free %x %s  (safe to ignore this assert)", pAddressToFree, pArenaHdrInfo->m_szArenaName));
#endif MSDEBUG
    }
    VerifyIntegrity(pArenaHdrInfo);

    return res;
}




CLINKAGE BOOL ENTRYPOINT ArenaRelease(DWORD hArena, PVOID Mark)
{
    if (!g_TrackArenas)
    {
        return true;
    }
    LockCritSect;
    ArenaHdrInfo *pArenaHdrInfo = (ArenaHdrInfo *)hArena;
#if MSDEBUG
    VSASSERTF((hArena != 0,"err ArenaRelease: hArena is null?"));
    VSASSERTF((g_pCHeapSpyArenas != 0,"err ArenaRelease g_pCHeapSpyArenas is null?"));
    VSASSERTF((g_ArenaHeaderSet->m_pStlType->find(hArena) != g_ArenaHeaderSet->m_pStlType->end(),"ArenaAllocation hArena not found %x", hArena));
    VSASSERTF((pArenaHdrInfo->m_ArenaBlkType == ArenaBlkType_Header,"ArenaRelease: blkType invalid"));
#endif MSDEBUG
    if (g_ArenaHeaderSet->m_pStlType->find(hArena) == g_ArenaHeaderSet->m_pStlType->end())
    {
        return false;
    }
    BOOL res = true;

    if (pArenaHdrInfo->m_fDidHaveAFree) // did we have a brokenlink
    {
        VSASSERT(Mark == 0 ,"Arena had a free and is now freeing to mark?");
        //we had a broken link, we want to free them all
        for_each (pArenaHdrInfo->m_pWrappedArenaAllocMap->m_pStlType->begin(),
                  pArenaHdrInfo->m_pWrappedArenaAllocMap->m_pStlType->end(),
                  [&](ArenaAllocMap::reference it) {
                      ArenaAllocData *pFreedData = it.second;
                      pArenaHdrInfo->m_CntCurLive --;
                      pArenaHdrInfo->m_SizeCurLive -= pFreedData->m_dwAllocSize;
                      auto resHfree= HeapFree(g_pCHeapSpyArenas->m_hHeap, 0, pFreedData);
                      VSASSERT(resHfree,"ArenaFree: failed to heapFree");
                    }
                  );
        pArenaHdrInfo->m_pWrappedArenaAllocMap->m_pStlType->clear();

    }
    else
    {

        BOOL fMark = false;
        while (pArenaHdrInfo->m_pMostRecentAllocation && ! fMark)
        {
            if (pArenaHdrInfo->m_pMostRecentAllocation->m_pData == Mark)
            {
                fMark = true;
            }
            if (ArenaFree(hArena, pArenaHdrInfo->m_pMostRecentAllocation->m_pData) == false)
            {
                VSASSERTF((false,"error releasing arena %s", pArenaHdrInfo->m_szArenaName));
                res = false;
                break;
            }
    //        auto temp = pArenaHdrInfo->m_pMostRecentAllocation->m_pPriorAllocation;
    //        pArenaHdrInfo->m_CntCurLive -=1;
    //        pArenaHdrInfo->m_SizeCurLive -= pArenaHdrInfo->m_pMostRecentAllocation->m_dwAllocSize;
    //        // cant memset because the page might be freed.
    ////        memset(pArenaHdrInfo->m_pMostRecentAllocation->m_pData, 0xAF, pArenaHdrInfo->m_pMostRecentAllocation->m_dwAllocSize); // fill the user free'd mem
    //
    //        HeapFree(g_pCHeapSpyArenas->m_hHeap, 0, pArenaHdrInfo->m_pMostRecentAllocation);
    //
    //        pArenaHdrInfo->m_pMostRecentAllocation = temp;
        }
    }
    VerifyIntegrity(pArenaHdrInfo);
    return res;
}


CLINKAGE BOOL ENTRYPOINT ArenaDestroy(DWORD hArena)
{
    if (!g_TrackArenas)
    {
        return true;
    }
    LockCritSect;
    ArenaHdrInfo *pArenaHdrInfo = (ArenaHdrInfo *)hArena;
#if MSDEBUG
    VSASSERTF((hArena != 0,"err ArenaDestroy: hArena is null?"));
    VSASSERTF((g_ArenaHeaderSet->m_pStlType->find(hArena) != g_ArenaHeaderSet->m_pStlType->end(),"ArenaAllocation hArena not found %x", hArena));
    VSASSERTF((pArenaHdrInfo->m_ArenaBlkType == ArenaBlkType_Header,"ArenaDestroy: blkType invalid"));
#endif MSDEBUG
    BOOL res = ArenaRelease(hArena, NULL);
    if (pArenaHdrInfo->m_pWrappedArenaAllocMap)
    {
        VSASSERT(pArenaHdrInfo->m_pWrappedArenaAllocMap->m_pStlType->size() == 0,"Count should be 0 in arena destroy");
        pArenaHdrInfo->m_pWrappedArenaAllocMap->freemem();
    }
    auto findres = g_ArenaHeaderSet->m_pStlType->find(hArena);
    if  (findres != g_ArenaHeaderSet->m_pStlType->end())
    {
        g_ArenaHeaderSet->m_pStlType->erase(findres);
    }
    VSASSERTF((res!=0,"Err freeing while destroying arena"));
    VSASSERTF((pArenaHdrInfo->m_ArenaBlkType == ArenaBlkType_Header,"ArenaDestroy: blkType invalid"));
    HeapFree(g_pCHeapSpyArenas->m_hHeap, 0, pArenaHdrInfo);
    return res;
}

