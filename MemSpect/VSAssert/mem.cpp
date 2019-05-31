//=--------------------------------------------------------------------------=
// Mem.cpp
//=--------------------------------------------------------------------------=
// Debug leak detection and memory allocation.
//=--------------------------------------------------------------------------=
// Copyright 1997 Microsoft Corporation.  All rights reserved.
// Information contained herein is proprietary and confidential.
//
// BrianPe, 4/97.  Uncreatively lifted from heap.c in ruby.
//

#include "pch.h"
#include "vsassert.h"
#include "dump.h"
#include "util.h"
#include "main.h"
#include "stdlib.h"
#include "mem.h"
#include "stackwalk.h"

#undef LOCK_MUTEX

struct CDumpLeakContext
{
	CLeakInfo     m_MinLeak;
	CLeakInfo     m_Total;
	CLeakInfo     m_Reported;
	CHeapSpy     *m_pHeapSpy;
	LPCSTR        pszNoFile;  // to identify external leaks
	LPCSTR        m_pszHeapName;
};


#define Compare(a, b) ((a) > (b) ? 1 : (a) == (b) ? 0 : -1)


// callback functions for PrintLeaks
void ReportLeaks(void * pElement, UINT uicTimesLeaked, void * pContext);

//////////////////////////////////////////////////////////////////////////////
//                        Debugging memory leaks
//////////////////////////////////////////////////////////////////////////////
//
// To debug a memory leak you need to figure out where and when the allocation
// was made.  There are two ways to do this:
//
// If it is a leak that uses VsAssert for it's debug allocation, the leak
// assert may show the file and line number of the allocation.  If this is
// the case, why have you even read this far?  The bug is staring you in
// the face.
//
// If the file/line number says something intuitive like "External Component"
// then this is an IMalloc leak from a component that didn't use VsOleAlloc.
// Look at the value for nAlloc in the assert dialog.
// Then, put a passcount breakpoint on the code below
// and set the passcount to skip nAlloc-1 times.  When the breakpoint
// gets hit verify that the bytecount is correct -- sometimes nAlloc is
// different if you're running in the debugger or if you didn't exactally
// reproduce the leak scenario the same way.
//
// Once you hit the breakpoint you should be able to look down the callstack
// to see where the allocation was made.  Sometimes OLE will mess up the stack.
// In that case just step into OLE code (you are running NT, aren't you?) and out
// you will land smack dab on the line that leaked.
//
// Note: if you are trying to track down a BSTR leak, you should know that by
// default, OLEAUT caches BSTR pointers and reuses them.  This can throw off
// the location of a memory leak.  To work around this, make sure you are running
// the debug version of OLEAUT (this comes down with a debug vssetup), and then
// set the following environment variable:
//
//      set OANOCACHE=1
//
// This will disable BSTR caching.
//
void PutBreakPointHere(void * pvAddress, SIZE_T nByteCount, ULONG  nAlloc, LPCSTR szFile, ULONG uLine)
{
	pvAddress = pvAddress;  nAlloc = nAlloc;  nByteCount = nByteCount;
	szFile = szFile;
	uLine = uLine;
	int PutBreakPointOnThisLine = 1;  ////// <--- set breakpoint here. //////

	// And we can also automagically int3 if the user wants us to
	//
	if (nAlloc == g_ulLeakBreakSkipCount)
		Int3;
}

//=--------------------------------------------------------------------------=
// Switches

// Enables periodic heap checking.  The check interval is set by
// _cHeapCheckInterval below -- you must set it through the compiler
// or debugger.
//
DEFINE_SWITCH(_fHeapCheck, "VsAssert", "Enable heap checking");

// Allows you to trace allocations.  This is very handy when NT Int3's due
// to a memory overwrite somewhere.
//
// NOTE:  This emits a ton of data.  Ensure that you are logging to a file
//        before turning this on or you will hate yourself as you wait
//        for msdev or the IPane to catch up.
//
DEFINE_SWITCH(_fTraceAllocations, "VsAssert", "Trace all allocations");

// Disables code to fill memory with a standard pattern on free.
//
DEFINE_SWITCH(_fNoFillOnFree, "VsAssert", "Disable filling memory on free");

// Asserts if we ever try to Realloc() for zero bytes, which it technically
// correct but a potential source of memory corruption errors
//
DEFINE_SWITCH(_fAssertRealloc0, "VsAssert", "Assert on Realloc(..., 0) calls");

// Since we implement the memory stuff, we should undef the macros here
//
#undef SysAllocString
#undef SysAllocStringByteLen
#undef SysAllocStringLen
#undef malloc
#undef realloc
#undef free
#undef HeapAlloc
#undef HeapReAlloc
#undef HeapFree

//=--------------------------------------------------------------------------=
// Public variables
//


LPTSTR	  g_pszSymbolsPath = NULL;

BOOL  g_fDumpMemStats = FALSE;

BOOL  g_fDontReportKnownLeaks = FALSE;

BOOL  g_fSaveCallstack = TRUE; // CLR moved objs turns this off
UINT   g_nStackFrames = DEFAULT_CALLSTACK_FRAMES;  // # of stack frames to get for each allocation

BOOL  g_fCollectLeaks = FALSE;



//LPSTR g_pszRememberCallstackForAllocSizes = NULL;
//BOOL  g_fSaveCallstackOnlyForSize = FALSE;
//ULONG g_ulSaveCallstackSize    = 0;       // specific byte count to save for

int   g_nDumpMemStatInterval = 100;     // interval for dumping stats
ULONG g_ulLeakBreakSkipCount = 0;       // nAlloc to int3 on
short g_sDumpLines = 8;       // # of lines to spew

BOOL  g_fEnableGdiLeakCheck = FALSE;
BOOL  g_fDontReportKnownGDILeaks = FALSE;
UINT  g_uicMinGdiHits = 0;
int   g_GdiTrace_nStackFrames = DEFAULT_GDI_TRACE_CALLSTACK_FRAMES;

BOOL  g_fEnableFastGetStack = FALSE;
int g_nRetailHeaderTrailerSize = 0;
int g_nUseChildProcessForSymbols = 1; // 1 means use child process (not target proc) to resolve native symbols.  0 means use target process
int g_fEnableAsserts = 1;
int g_CheckGCLostObjects = 0;
int g_fHandle4gigStacks = 0;
int g_fTrackLoadResource = 1; // track LoadResource
int g_fTrackGCStacks = 0;
int g_NativeOnly = 0;

DWORD g_dwUpdateLoadedModulesTimeout = 10000;

//--------------------------------------



UINT      g_uicGDILeaks = 0;


#define DUMP_BYTES_PER_LINE    16      // # of bytes to spew per line
#define MAX_DUMP_BLOCKS        5      // limit number of dumped memory blocks per one leak, i.e. the same callstack


//=--------------------------------------------------------------------------=
// Private variables
//

CHeapSpy  *_pDefaultHeap = NULL;       // default heap
CHeapSpy  *_pHeapSpyList = NULL;       // linked list of HeapSpy instances
ULONG      g_ulGlobalPassCount = 1;     // shared passcount amoung heap and IMalloc spies. Init to non-zero 1

BOOL g_fTrackVirtualMem = false;
BOOL g_fTrackHeap = false;
BOOL g_TrackClrObjects = false;
BOOL g_StartChildProcess = true;
int g_TrackCodeMarkers = TrackCodeMarkerMode_Normal | TrackCodeMarkerMode_Custom; // Bitflag: 0 means none, 1 means normal code markers, 2 means custom, 3 meas both
BOOL g_fTrackThreadCreate = true;
int g_fTrackGDI = 0;
int g_ThreadDynamic = true;
BOOL g_fUseGlobalName = false;
BOOL g_ShowManagedSourceInStacks = false;
int g_CleanUpRCWs = 1;
int g_ImmersiveSession = 2;
BOOL g_TrackJit = false;
BOOL g_TrackExcpt = true;
BOOL g_TrackETW = false;
BOOL g_TrackArenas = true;
TrackingModeEnum g_TrackingMode = Normal;
StackStorageModeEnum g_StackStorageMode = InMemory;
BOOL g_TrackFileLoadUnload = 0;
BOOL g_DynStackSymRes = false;
BOOL g_VirtualAllocTopDown = false;
int g_CodeMarkerParameter = 10000;

static UINT       _cHeapCheckInterval = 100;  // # of allocations between heap checks.  Setting
// this too low can really slow down the app
static LONG       _lIgnoreIMalloc = 0;        // Nonzero to ignore upcoming IMalloc allocations
static CHAR       _szStackBuf[4096];          // we use this for printing the stack
static DWORD_PTR  *_rgIgnoreList;             // list of external leak sizes to ignore




// Note:  These must be a multiple of 8 to keep things aligned properly
//        on risc.
//
// On 64bit systems, they should be multiples of 16 to keep them properly alligned

#ifdef _WIN64
char *_szHeader  = "__HEADHEADHEAD__";	// beginning of block signature
#else
char *_szHeader = "MEMSPECT";	// beginning of block signature
#endif


char *_szTrailer = "memspect";	// end of block signature


#define SZBADTRAILER "Heap block trailer trashed."
#define SZBADHEADER  "Heap block header trashed."

DECLARE_MUTEX(_mxsHeapSpy);       // The mutex to lock when we need to access globals

static CAddressNode *s_AddressNodeTemp = NULL;




//=--------------------------------------------------------------------------=
// IMallocSpy file/line info
//
static const CHAR               _szNoFile[] = "External Component";
static BOOL                     _fMallocSpyDestroyed = FALSE;

#define MEMCMP(PV1, PV2, CB)	memcmp((PV1), (PV2), (CB))
#define MEMCPY(PV1, PV2, CB)	memcpy((PV1), (PV2), (CB))
#define MEMSET(PV,  VAL, CB)	memset((PV),  (VAL), (CB))
#define MEMMOVE(PV1, PV2, CB)	memmove((PV1), (PV2), (CB))

//=--------------------------------------------------------------------------=
// Private functions
//
void     _DumpMemoryBlock(PVOID pv, SIZE_T cb, CHeapSpy *pHeapSpy);
IMalloc *_GetInternalMalloc();
static BOOL _IsIgnoreThisLeak(CAddressNode *pn, UINT_PTR dwInst, LPCSTR pszNoFile);

#if MEMSPECT

PVOID __cdecl operator new(size_t size)
{
	return DebugAlloc(size);
}
PVOID __cdecl operator new(size_t size, _In_z_ LPSTR pszFile, UINT uLine)
{
	return DebugAlloc(size);
}
void  __cdecl operator delete(PVOID pv)
{
	DebugFree(pv);
}
#else

PVOID __cdecl operator new(size_t size)
{
	return VsDebAlloc(0, size);
}
PVOID __cdecl operator new(size_t size, _In_z_ LPSTR pszFile, UINT uLine)
{
	return VsDebugAllocInternal(DEFAULT_HEAP, 0, size, pszFile, uLine, INSTANCE_GLOBAL, NULL);
}
void  __cdecl operator delete(PVOID pv)
{
	VsDebFree(pv);
}
#endif MEMSPECT

//
//
// Public functions, but public only to this DLL
//
//

//static
void MemMap::Resize(DWORD newSize)
{
	if (g_FreeLists == nullptr)
	{
		g_FreeLists = new (DebugAlloc(sizeof(freeList))) freeList(MySTLAlloc<pair<DWORD, vecMapFileLocator *> >(InternalHeapToUse));
		_stats.ulOffsetFreeMem = 0;
	}
	if (newSize > _stats.ulMemSize)
	{
		//if (newSize >= 0x80000000)
		//{
		//	VSASSERTF((false, "too many stacks  %x", newSize));
		//}

		MappingData newMappingData = CreateAMapping(newSize);

		if (_currentMapping._hFileMapping != 0) // prior data?
		{
			int nBlocks = _stats.ulMemSize / AllocationGranularity;
			for (int i = 0; i < nBlocks; i++)
			{
				auto oldAddr = MapView(i * AllocationGranularity, AllocationGranularity, &_currentMapping);
				auto newAddr = MapView(i * AllocationGranularity, AllocationGranularity, &newMappingData);
				memcpy(newAddr, oldAddr, AllocationGranularity);
			}
			UnmapView(_currentMapping._mapBase);
			CloseHandle(_currentMapping._hFileMapping);
			_currentMapping = newMappingData;
		}
		else
		{
			_currentMapping = newMappingData;
		}
		_stats.ulMemSize = newSize;
	}
}

//static
MemMap::MappingData MemMap::CreateAMapping(DWORD newSize)
{
	MappingData mapData = { 0 };
	char szBuf[100];
	sprintf_s(szBuf, sizeof(szBuf), "%SMemSpect%d_%d", g_szGlobal, GetCurrentProcessId(), ++_stats.nFileMaps);
	mapData._hFileMapping = CreateFileMappingA(
		_hFileHandle,
		GetSecurityAttributes(), // security
		PAGE_READWRITE,
		0, // sizeHigh
		newSize, // sizeLow
		szBuf//name;
		);
	auto err = GetLastError();
	if (mapData._hFileMapping == 0)
	{
		VSASSERTF((false, "Can't create file mapping %x", err));
	}
	mapData._dwFileSize = newSize;
	return mapData;
}

//static
LPVOID MemMap::MapView(DWORD ulOffset, int numBytesToMap, MappingData *mappingData /*= nullptr*/)
{
	LPVOID mappedAddress = nullptr;
	auto newBaseOffset = (ulOffset / AllocationGranularity) * AllocationGranularity;
	auto leftover = ulOffset - newBaseOffset;
	LPVOID preferredAddress = nullptr;
	auto desiredSize = _uiViewSize;
	if (mappingData == nullptr)
	{
		mappingData = &_currentMapping;
	}

	if (mappingData->_mapBase != nullptr)
	{
		// if what we want to map is already mapped (from the current mapping data)
		bool fFits = ulOffset >= mappingData->_ulBaseOffset &&
			ulOffset + numBytesToMap < mappingData->_ulBaseOffset + mappingData->_currentMappedViewSize;
		if (fFits && leftover + numBytesToMap < mappingData->_currentMappedViewSize)
		{
			mappedAddress = (LPVOID) ((DWORD) (mappingData->_mapBase) + ((int) (newBaseOffset - mappingData->_ulBaseOffset + leftover)));
		}
		else
		{
			preferredAddress = mappingData->_mapBase;
			UnmapView(mappingData->_mapBase);
			VSASSERT(numBytesToMap <= AllocationGranularity, "#bytes to map should be AllocationGran");
			if (leftover + numBytesToMap > desiredSize)
			{
				desiredSize += AllocationGranularity;
			}
			if (newBaseOffset + desiredSize >= mappingData->_dwFileSize)
			{
				desiredSize = (mappingData->_dwFileSize - newBaseOffset);
			}
		}
	}
	if (mappedAddress == nullptr)
	{
		_stats.nMapViews++;
		// try at preferred address
		mappedAddress = MapViewOfFileEx(
			mappingData->_hFileMapping,
			FILE_MAP_READ | FILE_MAP_WRITE,
			0,
			newBaseOffset,
			desiredSize,
			preferredAddress
			);
		if (mappedAddress == nullptr)
		{
			if (preferredAddress != nullptr)
			{
				// try again at any address
				mappedAddress = MapViewOfFileEx(
					mappingData->_hFileMapping,
					FILE_MAP_READ | FILE_MAP_WRITE,
					0,
					newBaseOffset,
					desiredSize,
					nullptr
					);
			}
			if (mappedAddress == nullptr)
			{
				auto err = GetLastError();

				VSASSERTF((0, "Can't MapView %d %d %d", ulOffset, numBytesToMap, err));
			}
		}
		mappingData->_mapBase = mappedAddress;
		mappingData->_currentMappedViewSize = desiredSize;
		mappingData->_ulBaseOffset = newBaseOffset;
		mappedAddress = (LPVOID) ((DWORD) mappedAddress + (int) leftover);
	}
	return mappedAddress;

}
//static
void MemMap::UnmapView(LPVOID dwAddr)
{
	auto res = UnmapViewOfFile(dwAddr);
	VSASSERT(res, "unmapView failed");
}
//static 
void MemMap::VerifyStuff()
{
	if (g_StackStorageMode == InMapFileVerify)
	{
		DWORD dwfreeSize = 0;

		//	freeList *notherMap = new (DebugAlloc(sizeof(freeList))) freeList(less<DWORD>(), MySTLAlloc<pair<DWORD,vecMHandle *> >(InternalHeapToUse ));

		unordered_map<DWORD, MapFileLocator> mapTest;
		DWORD dwFirstFreeOffset = 0;
		VSASSERT(_stats.ulOffsetFreeMem >= _stats.nFreeListSize, "How can offset be smaller than freelist?");
		if (g_FreeLists != nullptr)
		{
			for_each(g_FreeLists->m_pStlType->begin(),
				g_FreeLists->m_pStlType->end(),
				[&](freeListHashMap::reference it)
			{
				auto freelist = it.second;
				dwfreeSize += it.first * it.second->m_pStlType->size();
				for_each(
					freelist->m_pStlType->begin(),
					freelist->m_pStlType->end(),
					[&](MapFileLocator &mh)
				{
					if (dwFirstFreeOffset == 0)
					{
						dwFirstFreeOffset = mh.ulOffset;
					}
					auto res = mapTest.insert(pair<DWORD, MapFileLocator>(mh.ulOffset, mh));
					VSASSERT(mh.ulSize > 0, "freelist entry 0 size?");
					if (!res.second)
					{
						VSASSERT(res.second, "didn't insert free");
					}
					//						notherMap->m_pStlType->insert(pair<DWORD,mHandle >(it2._Locator.ulOffset, it2));
				}
				);

			});
		}
		VSASSERT(dwfreeSize == _stats.nFreeListSize, "freesize wrong");


		DWORD nAllocs = 0;
		DWORD dwTotSize = 0;
		for (CHeapSpy * pHeap = _pHeapSpyList; pHeap; pHeap = pHeap->m_pNextHeapSpy)
		{
			if (pHeap->m_AddrSetWrapper != nullptr)
			{
				for_each(pHeap->m_AddrSetWrapper->m_pStlType->begin(),
					pHeap->m_AddrSetWrapper->m_pStlType->end(),
					[&](CHeapSpy::AddrNodeInstSet::reference it)
				{
					CAddressNode *pn = it.second;
					if (pn->m_uicStackAddr)
					{
						dwTotSize += pn->m_uicStackAddr * 4;
#if MSDEBUG
						dwTotSize += 8;
#endif MSDEBUG
						nAllocs++;
						MapFileLocator mh = { pn->m_uicStackAddr * 4, pn->m_pdwStackAddr[0] };
						auto res = mapTest.insert(pair<DWORD, MapFileLocator>(pn->m_pdwStackAddr[0], mh));
						if (!res.second)
						{
							VSASSERTF((res.second, "didn't insert alloc %d %d %d",
								pn->m_pdwStackAddr[0],
								_stats.nFreeListSize,
								dwFirstFreeOffset
								));
						}
					}
				}
				);
			}
		}


		VSASSERT(_stats.ulOffsetFreeMem >= dwTotSize, "How can offset be smaller than dwTotSize?");



		VSASSERT(dwTotSize == _stats.nCurAlloc, "totsize curallocs?");

		VSASSERT(_stats.nCurAlloc <= _stats.ulOffsetFreeMem, "How can CurAllocs be bigger than offset ?");

		VSASSERT(dwTotSize + dwfreeSize == _stats.ulOffsetFreeMem, "totsize +freesize== offset?");
	}
}

//static 
void MemMap::CompactIt()
{
	DWORD dwfreeSize = _stats.nFreeListSize;
	// we don't want to thrash as we get close to growing mem required, so only compact if size changes
	// if the potential gain is worth it: if the freesize is > cursize, then we can avoid a growth doubling
	if (dwfreeSize > _stats.ulMemSize/2)
	{
		VerifyStuff();
#if MSDEBUG
		// first, calculate the tot size of all stacks
		DWORD dwTotSize = 0;
		DWORD nAllocs = 0;
		for (CHeapSpy * pHeap = _pHeapSpyList; pHeap; pHeap = pHeap->m_pNextHeapSpy)
		{
			if (pHeap->m_AddrSetWrapper != nullptr)
			{
				for_each(pHeap->m_AddrSetWrapper->m_pStlType->begin(),
					pHeap->m_AddrSetWrapper->m_pStlType->end(),
					[&](CHeapSpy::AddrNodeInstSet::reference it)
				{
					CAddressNode *pn = it.second;
					VSASSERT(pn->m_uicStackAddr >= 0 && pn->m_uicStackAddr < 500, "bad stack");
					if (pn->m_uicStackAddr>0)
					{
						dwTotSize += pn->m_uicStackAddr * 4;
						dwTotSize += 8;
					}
					nAllocs++;
				}
				);
			}
		}
		VSASSERT(dwTotSize + dwfreeSize == _stats.ulOffsetFreeMem, "compact: totsize = freesize < offset");

		VSASSERT(dwTotSize == _stats.nCurAlloc, "dwTotSize  == _stats.nCurAlloc");
#endif MSDEBUG

		auto newSize = _stats.nCurAlloc;
		//		VSASSERT(dwTotSize + dwfreeSize <= _stats.ulOffsetFreeMem ,"compact: totsize = freesize < offset");
		DWORD pow2 = AllocationGranularity;
		while (pow2 < newSize)
		{
			pow2 <<= 1;
		}
		newSize = pow2; // could be same as old size
		if (newSize < _initialSize)
		{
			newSize = _initialSize;
		}
		_stats.nCompactions++;
		MappingData tempMappingData = CreateAMapping(newSize);
		DWORD ulOffsetNew = 0;
		// now we want to iterate over all current stacks
		for (CHeapSpy * pHeap = _pHeapSpyList; pHeap; pHeap = pHeap->m_pNextHeapSpy)
		{
			if (pHeap->m_AddrSetWrapper != nullptr)
			{
				for_each(pHeap->m_AddrSetWrapper->m_pStlType->begin(),
					pHeap->m_AddrSetWrapper->m_pStlType->end(),
					[&](CHeapSpy::AddrNodeInstSet::reference it)
				{
					if (it.second->m_uicStackAddr > 0)
					{
						DWORD dwSize = it.second->m_uicStackAddr * 4;
#if MSDEBUG
						dwSize += 8;
#endif MSDEBUG
						auto ulOffset = it.second->m_pdwStackAddr[0];
						auto oldAddr = MapView(ulOffset, dwSize, &_currentMapping);
						auto newAddr = MapView(ulOffsetNew, dwSize, &tempMappingData);
						VSASSERTF((ulOffsetNew + dwSize < newSize, "compact: newsize not big enough"));
						memcpy(newAddr, oldAddr, dwSize);
						it.second->m_pdwStackAddr[0] = ulOffsetNew;
						ulOffsetNew += dwSize;
					}
				}
				);
			}
		}
		UnmapView(_currentMapping._mapBase);
		CloseHandle(_currentMapping._hFileMapping);
		_currentMapping = tempMappingData;
		_stats.ulMemSize = newSize;

		_stats.ulOffsetFreeMem = ulOffsetNew;

		for_each(g_FreeLists->m_pStlType->begin(),
			g_FreeLists->m_pStlType->end(),
			[&](freeListHashMap::reference it)
		{
			it.second->m_pStlType->clear();
			//	it.second->freemem();
		});
		g_FreeLists->m_pStlType->clear();
		_stats.nFreeListSize = 0;
		VerifyStuff();
	}
}
//static
void MemMap::AddStack(CAddressNode *pn)
{
	MapFileLocator mh;
	auto nItems = pn->m_uicStackAddr;
	if (nItems > 0)
	{
		//	VSASSERT(pn->m_uicStackAddr != 27,"add 27");

		CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
		VSASSERT(!CDisableTrace::CanDetour(), "MemMap::RemmvoeStack CanDetour?");
		if (g_FreeLists == nullptr) // initialization
		{
			Resize(_initialSize);
		}
		auto dataSize = Roundit(nItems * 4);
#if MSDEBUG
		dataSize += 8;
#endif MSDEBUG
		auto res = g_FreeLists->m_pStlType->find(dataSize);
		int vecSize;
		if (res != g_FreeLists->m_pStlType->end() &&
			(vecSize = res->second->m_pStlType->size()) > 0) // found a freelist of desired size && not empty list
		{
			mh = res->second->m_pStlType->at(vecSize - 1);
			res->second->m_pStlType->pop_back();
			_stats.nFreeListSize -= dataSize;
		}
		else
		{
			if (_stats.ulOffsetFreeMem + dataSize >= _stats.ulMemSize)
			{
				CompactIt();
				if (_stats.ulOffsetFreeMem + dataSize >= _stats.ulMemSize)
				{
					// gotta grow
					auto newSize = _stats.ulMemSize * 2;
					if (_stats.ulMemSize >= 0x80000000)
					{
						newSize = _stats.ulMemSize + 0x4000000; // 67 meg
					}
					Resize(newSize);
				}
			}
			mh.ulOffset = _stats.ulOffsetFreeMem;
			mh.ulSize = (WORD) dataSize;
			_stats.ulOffsetFreeMem += dataSize;
		}
		_stats.nCurAlloc += dataSize;
		DWORD * addr = (DWORD *) MapView(mh.ulOffset, mh.ulSize, &_currentMapping);

		//		VSASSERTF((addr >= (DWORD)MemMap::_currentMapping._mapBase && addr < (DWORD)MemMap::_currentMapping._mapBase + MemMap::_currentMapping._currentMappedViewSize ,"memmap not in range"));
#if MSDEBUG
		memcpy(&addr[1], pn->m_pdwStackAddr, nItems * 4);
		addr[0] = 0x1234;
		addr[pn->m_uicStackAddr + 1] = 0x4321;
		//static int nn = 0;
		//for (UINT i = 0 ; i <pn->m_uicStackAddr ; i++)
		//{
		//	//			addr[i+1] = ++nn;
		//}
#else
		memcpy(addr, pn->m_pdwStackAddr, nItems * 4);
#endif MSDEBUG

		pn->m_pdwStackAddr[0] = mh.ulOffset;
		if (g_StackStorageMode == InMapFileVerify)
		{
			VSASSERT(_stats.nCurAlloc + _stats.nFreeListSize == _stats.ulOffsetFreeMem, "_stats.nCurAlloc + _stats.nFreeListSize == _stats.ulOffsetFreeMem?");

			VSASSERT(_stats.nFreeListSize < _stats.ulOffsetFreeMem, "freelist size > offsetfreemem?");
		}
	}
	else
	{
		pn->m_pdwStackAddr[0] = 0;
	}

}

//static
void MemMap::RemoveStack(CAddressNode *pn)
{
	DWORD dwSize = pn->m_uicStackAddr * 4;
	//	VSASSERT(pn->m_uicStackAddr != 27,"rmv 27");
	if (dwSize != 0)
	{
		CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
		//		VSASSERT(!CDisableTrace::CanDetour(),"MemMap::RemmvoeStack CanDetour?");
		auto freeSize = Roundit(dwSize);
#if MSDEBUG
		freeSize += 8;
#endif MSDEBUG
		auto res = g_FreeLists->m_pStlType->find(freeSize);
		vecMapFileLocator *freelist;
		if (res == g_FreeLists->m_pStlType->end())
		{
			freelist = new (DebugAlloc(sizeof(vecMapFileLocator))) vecMapFileLocator(MySTLAlloc<vecMapFileLocator>(InternalHeapToUse));
			g_FreeLists->m_pStlType->insert(pair<DWORD, vecMapFileLocator *>(freeSize, freelist));
		}
		else
		{
			freelist = res->second;
		}
		MapFileLocator mh = { freeSize, pn->m_pdwStackAddr[0] };
		freelist->m_pStlType->push_back(mh);
		_stats.nFreeListSize += freeSize;
		VSASSERT(_stats.nFreeListSize <= _stats.ulOffsetFreeMem, "freelist size > offsetfreemem?");
		_stats.nCurAlloc -= freeSize;
	}
}


MemMap::freeList *MemMap::g_FreeLists = NULL;
HANDLE MemMap::_hFileHandle = INVALID_HANDLE_VALUE; // INVALID_HANDLE_VALUE = System paging file  
MemMap::stats MemMap::_stats = { 0 };
MemMap::MappingData MemMap::_currentMapping = { 0 };
DWORD MemMap::_uiViewSize = 2 * AllocationGranularity;
DWORD MemMap::_initialSize =
#if MSDEBUG
AllocationGranularity * 2;
#else
AllocationGranularity *  16 * 256;// AllocationGranularity * 16  == 1 Megabyte (1,048,576)
#endif



//=--------------------------------------------------------------------------=
// CreateDefaultHeap
//=--------------------------------------------------------------------------=
// Ok, we're not really creating the default heap, because we're using the
// process heap.  But, we wrap it in a spy instance.
//
BOOL CreateDefaultHeap()
{
	ASSERT(!_pDefaultHeap, "DebugHeap: Default heap already created");
	_pDefaultHeap = new CHeapSpy(GetProcessHeap(), "__Process Heap", "", 0);
	_pDefaultHeap->m_nStackFramesToSkip = 1;

	return (_pDefaultHeap != NULL);
}


//=--------------------------------------------------------------------------=
// DestroyDefaultHeap
//=--------------------------------------------------------------------------=
// Removes the default heap.  We don't actually destroy the heap, just the
// spy instance.
//
VOID DestroyDefaultHeap()
{
	//ShutDownDetours();

	if (_pDefaultHeap)
	{
		DebugFree(s_AddressNodeTemp);
		_pDefaultHeap->HeapCheck();
		delete _pDefaultHeap;
		_pDefaultHeap = NULL;
	}
}


//
//
// VS memory functions -    These are the public functions used to
//                          create allocate and generally putz around
//                          with the heap and IMalloc stuff
//


//=--------------------------------------------------------------------------=
// VsDebugAllocInternal
//=--------------------------------------------------------------------------=
// Allocs some memory against one of our heaps
//
PVOID VsDebugAllocInternal
(
HANDLE  hheap,    // heap to alloc against.  Can be DEFAULT_HEAP.
DWORD   dwFlags,  // alloc flags
SIZE_T  cb
)
{
	LPVOID    lpvRet;
	SIZE_T    dwBytes;
	CHeapSpy *pHeapSpy;

	CLock clStack(&s_mxsStackWalk);

	// special case for the default heap
	//
	if (DEFAULT_HEAP == hheap)
	{
		pHeapSpy = _pDefaultHeap;
		ASSERT(pHeapSpy, "DebugHeap: No default heap");
	}
	else
		pHeapSpy = (CHeapSpy *) hheap;

	if (!CHeapSpy::IsValidHeap(pHeapSpy))
		return NULL;


	// increase size to make space for header and trailer signatures
	//
	dwBytes = cb + pHeapSpy->m_nHeaderSize + pHeapSpy->m_nTrailerSize;
	if (Real_RtlAllocHeap == 0)
	{// occurs before detouring and we're init'ing
		lpvRet = HeapAlloc(pHeapSpy->m_hHeap, dwFlags, dwBytes);
	}
	else
	{
		//        CSaveHeaderTrailerSize saveHeaderTrailer(pHeapSpy);
		CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour
		lpvRet = Real_RtlAllocHeap(pHeapSpy->m_hHeap, dwFlags, dwBytes);
		if (lpvRet)
		{
			if (!(dwFlags & HEAP_ZERO_MEMORY)) // custom initialization of contents
			{
				memset(lpvRet, 0xAF, dwBytes);
			}

			pHeapSpy->AddInst(lpvRet, cb, pHeapSpy->m_nStackFramesToSkip);

			//advance pointer past header signature.
			//
			lpvRet = (LPVOID) ((char *) lpvRet + pHeapSpy->m_nHeaderSize);
		}
	}


	return lpvRet;
}


//=--------------------------------------------------------------------------=
// VsDebugReallocInternal
//=--------------------------------------------------------------------------=
// debug version of HeapRealloc
//
PVOID VsDebugReallocInternal
(
HANDLE  hheap,    // heap to alloc against, or DEFAULT_HEAP for the process heap
PVOID   pv,       // old ptr
DWORD   dwFlags,  // alloc flags
SIZE_T  cb
)
{
	LPVOID        lpvRet;
	SIZE_T        dwBytes;
	CHeapSpy     *pHeapSpy;
	CAddressNode *pn;

	CLock clStack(&s_mxsStackWalk);

	//This is not, technically, an error since the various realloc functions normally accept
	//zero byte requests. The problem is that, in that case, the realloc function frees the
	//pointer and returns NULL. VS code consistently treats a NULL return as an out of memory
	//error and does very bad things (typically, it continues to use the deallocated memory).
	//
	//So ... don't use realloc to request zero bytes. Instead, treat that as a special case
	//and call free. That way there is no confusion over what a NULL return means.
#ifdef _DEBUG
	if (FSWITCH(_fAssertRealloc0))
	{
		ASSERT(cb, "Realloc call requesting 0 bytes");
	}
#endif

	// If NULL is passed to VsDebugReallocInternal it should behave like VsDebugAllocInternal
	// This follows the same convention as the CRT.
	if (NULL == pv)
		return (VsDebugAllocInternal(hheap, dwFlags, cb));

	// special case for the default heap
	//
	if (DEFAULT_HEAP == hheap)
	{
		pHeapSpy = _pDefaultHeap;
		ASSERT(pHeapSpy, "No default heap");
	}
	else
		pHeapSpy = (CHeapSpy *) hheap;

	if (!CHeapSpy::IsValidHeap(pHeapSpy))
	{
		return NULL;
	}

	// move pointer to beginning of header
	//
	pv = (LPVOID) ((char *) pv - pHeapSpy->m_nHeaderSize);

	pn = pHeapSpy->FindInst(pv);
	if (!pn)
	{
		FAIL("DebugHeap: could not find pv in the given heap.  See debug output for more info.");
		pHeapSpy->AnalyzeInst(pv);
		return NULL;
	}

	//    pn->VerifyHeaderTrailer(pHeapSpy);

	SIZE_T origSize = pn->m_cb;

	// increase size to make space for header and trailer signatures
	//
	dwBytes = cb + pHeapSpy->m_nHeaderSize + pHeapSpy->m_nTrailerSize;

	if (Real_HeapReAlloc == 0)
	{
		lpvRet = HeapReAlloc(pHeapSpy->m_hHeap, dwFlags, pv, dwBytes);
	}
	else
	{
		//        CSaveHeaderTrailerSize saveHeaderTrailer(pHeapSpy);
		lpvRet = Real_HeapReAlloc(pHeapSpy->m_hHeap, dwFlags, pv, dwBytes);
	}

	if (lpvRet)
	{
		// If the allocation grew then we must initialize the old trailer and
		// any bytes padded on by the system in the previous allocation to
		// either 0x0 or to 0xAF.
		//
		if (cb > origSize)
		{
			SIZE_T cbOffset;
			BYTE  byte = (dwFlags & HEAP_ZERO_MEMORY) ? 0x00 : 0xAF;

			// get the byte offset of trailer in the old allocation
			//
			cbOffset = origSize + pHeapSpy->m_nHeaderSize;
			memset((char *) lpvRet + cbOffset, byte, dwBytes - cbOffset);
		}
		// sometimes the OS will call Alloc/Free to satisfy Realloc, inwhich case we don't have to changeinst
		if (pHeapSpy->FindInst(pv)) // if old is still there
		{
			pHeapSpy->ChangeInst(pv, lpvRet, cb, 0);
		}
		else
		{
			CAddressNode *pnNew = pHeapSpy->FindInst(lpvRet);
			if (!pnNew)
			{
				pHeapSpy->AddInst(lpvRet, cb, pHeapSpy->m_nStackFramesToSkip);
			}
			else
			{
				pnNew->m_cb = cb;   // set size to requested, without header/trailer
			}
			memcpy((char *) lpvRet + dwBytes - pHeapSpy->m_nTrailerSize, _szTrailer, pHeapSpy->m_nTrailerSize);

		}
		lpvRet = (LPVOID) ((char *) lpvRet + pHeapSpy->m_nHeaderSize);
	}

	return lpvRet;
}




//=--------------------------------------------------------------------------=
// VsDebugFreeInternal
//=--------------------------------------------------------------------------=
// Debug version of HeapFree
//
VOID VsDebugFreeInternal
(
HANDLE  hheap,  // heap to free from
PVOID   pv      // thingie to free
)
{
	CHeapSpy *pHeapSpy;

	CLock clStack(&s_mxsStackWalk);
	// special case for the default heap
	//
	if (DEFAULT_HEAP == hheap)
	{
		pHeapSpy = _pDefaultHeap;
		if (!g_fIsChildProcess)
		{
			if (!g_fShuttingDown)
			{
				ASSERT(pHeapSpy, "No default heap");
			}
		}
	}
	else
		pHeapSpy = (CHeapSpy *) hheap;

	// retail HeapFree accepts a NULL pointer, so we should to
	//
	if (!pv) {
		return;
	}

	if (!CHeapSpy::IsValidHeap(pHeapSpy))
	{
		return;
	}
	VsDebugFreeInternalEx(pHeapSpy, pv);
}

void VsDebugFreeInternalEx(CHeapSpy *pHeapSpy, PVOID pv)
{
	BOOL      fRet = FALSE;
	pv = (LPVOID) ((char *) pv - pHeapSpy->m_nHeaderSize);


	if (Real_RtlFreeHeap == 0)
	{
		fRet = HeapFree(pHeapSpy->m_hHeap, 0, pv);
	}
	else
	{
		fRet = Real_RtlFreeHeap(pHeapSpy->m_hHeap, 0, pv);
	}

	CAddressNode * pn;
	// WinNT will set free memory to 0xEEFEEEFE which is "îþîþ"
	pn = pHeapSpy->FindInst(pv);

	if (pn)
	{

		ASSERT(fRet, "DebugHeap: pv was found to be allocated in the heap "
			"passed in but HeapFree() failed.  Maybe the pointer "
			"was already freed.");

		if (fRet)
		{
			pHeapSpy->DeleteInst(pv);  //checks header and trailer
		}

	}

}


//=--------------------------------------------------------------------------=
// VsDebugHeapCreateInternal
//=--------------------------------------------------------------------------=
// Debug heap creation function
//
HANDLE VsDebugHeapCreateInternal
(
DWORD  dwFlags,  // flags to create heap with
LPCSTR pszName,
LPCSTR pszFile,  // file that created the heap
UINT   uLine     // line where heap create call was made
)
{
	CHeapSpy *pHeapSpy = NULL;
	HANDLE    hHeap;

	if (Real_HeapCreate == 0)
	{
		hHeap = HeapCreate(dwFlags, 0, 0);
	}
	else
	{
		hHeap = Real_HeapCreate(dwFlags, 0, 0);
	}
	CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour

	if (hHeap != NULL)
	{
		pHeapSpy = new CHeapSpy(hHeap, pszName, pszFile, uLine);
#ifdef _WIN64
		pHeapSpy->m_nHeaderSize = 16;
#else
		pHeapSpy->m_nHeaderSize = 8;
#endif
		pHeapSpy->m_nTrailerSize = 8;

	}

	if (pHeapSpy == NULL && hHeap != NULL)
		HeapDestroy(hHeap);

	return (HANDLE) pHeapSpy;
}


//=--------------------------------------------------------------------------=
// VsDebugHeapDestroyInternal
//=--------------------------------------------------------------------------=
// Debug heap destroy function.  We support NOT checking for leaks, since
// it is possible that the user wants to destroy the heap, allocations and
// all.  You should be careful here not to defeat the purpose of the
// debug allocators.
//
VOID VsDebugHeapDestroyInternal
(
HANDLE  hheap,          // heap to destroy
BOOL    fCheckForLeaks  // do you want to check for leaks?
)
{

	CHeapSpy *pHeapSpy = (CHeapSpy *) hheap;

	if (!CHeapSpy::IsValidHeap(pHeapSpy))
	{
		return;
	}

	pHeapSpy->HeapCheck();

	bool fLeaksFound = false;


	// if there were leaks, don't destroy the heap so it'll be around for debugging
	if (!fLeaksFound)
		HeapDestroy(pHeapSpy->m_hHeap);

	delete pHeapSpy;
}


//=--------------------------------------------------------------------------=
// VsDebugSizeInternal
//=--------------------------------------------------------------------------=
// returns the size of the given allocation.
//
// NOTE:  Do not depend on the size this returns.  The retail HeapSize
//        function may round this up to the nearest page.  We can't, since
//        that may lead to inadvertently trashing the trailer.
//
SIZE_T VsDebugSizeInternal
(
HANDLE hheap,
PVOID  pv
)
{
	CAddressNode  *pn;
	CHeapSpy      *pHeapSpy;

	// special case for the default heap
	//
	if (DEFAULT_HEAP == hheap)
	{
		pHeapSpy = _pDefaultHeap;
		ASSERT(pHeapSpy, "No default heap");
	}
	else
		pHeapSpy = (CHeapSpy *) hheap;


	pv = (LPVOID) ((char *) pv - pHeapSpy->m_nHeaderSize);
	pn = pHeapSpy->FindInst(pv);
	SIZE_T nSize;
	if (!pn)
	{
		if (pHeapSpy->m_nHeaderSize) // only fail for CHK heaps
		{
			FAIL("DebugHeap: could not find pv in the given heap.  See debug output for more info.");
			pHeapSpy->AnalyzeInst(pv);
			return 0;
		}
		else
		{
			nSize = Real_HeapSize(pHeapSpy->m_hHeap, 0, pv); // bugbug dwflags? 
		}
	}
	else
	{
		nSize = pn->m_cb;
	}
	return nSize;
}


//=--------------------------------------------------------------------------=
// VsDebValidateHeaps
//=--------------------------------------------------------------------------=
// Verifies that all internal heaps are correct and happy
//
CLINKAGE VOID ENTRYPOINT VsDebValidateHeaps()
{
	CHeapSpy::CheckAllHeaps();
}


//=--------------------------------------------------------------------------=
// VsDebIsValidHeap
//=--------------------------------------------------------------------------=
// Validates the given heap handle as good.  Asserts if it's not
//
CLINKAGE BOOL ENTRYPOINT VsDebIsValidHeap
(
HANDLE hHeap
)
{
	CHeapSpy *pHeapSpy = (CHeapSpy *) hHeap;

	return CHeapSpy::IsValidHeap(pHeapSpy) ? true : false;
}


//=--------------------------------------------------------------------------=
// VsDebIsValidHeapPtr
//=--------------------------------------------------------------------------=
// Validates the given pointer in the given heap handle as good.  Asserts if
// it's not
//
CLINKAGE BOOL ENTRYPOINT VsDebIsValidHeapPtr
(
HANDLE hHeap,
PVOID  pv
)
{
	CHeapSpy *pHeapSpy = (CHeapSpy *) hHeap;

	return CHeapSpy::IsValidHeapPtr(pHeapSpy, pv);
}


//=--------------------------------------------------------------------------=
// VsDebDumpMemStats
//=--------------------------------------------------------------------------=
// Dumps a list of memory statistics for all heaps to the debug window
//
CLINKAGE VOID ENTRYPOINT VsDebDumpMemStats()
{
	CHeapSpy::DumpAllStatistics();
}

//=--------------------------------------------------------------------------=
// VsDebGetNextHeap
//=--------------------------------------------------------------------------=
// will return the next CHeapSpy instance. If passed in 0, then will get first instance
// When no more instances or if passed in invalid data, will return NULL
HANDLE VsDebGetNextHeap
(
__in HANDLE hHeap,
__out LPCSTR *pszHeapName,
__out LPCSTR *pszFile,
__out ULONG * pnLineNo)
{
	CHeapSpy* hRetval = NULL;

	CHeapSpy *pWalk = _pHeapSpyList;
	if (hHeap == NULL) // user passed in NULL, just return the first instance, if any
	{
		hRetval = pWalk;
	}
	else
	{
		// user passed in an instance: let's find it and return the Next, if any
		while (pWalk && pWalk != hHeap)
		{
			pWalk = pWalk->m_pNextHeapSpy;
		}
		if (pWalk)
		{
			hRetval = pWalk->m_pNextHeapSpy;
		}
	}
	if (hRetval)
	{
		*pszHeapName = hRetval->m_pszHeapName;
		*pszFile = hRetval->m_pszFile;
		*pnLineNo = hRetval->m_uLine;
	}

	return (HANDLE) hRetval;
}



// very useful: each allocation is tagged with 1,2,3.... Thus they can be sorted chronologically for leak detection
CLINKAGE BOOL ENTRYPOINT VsDebGetHeapStats(
	__in HANDLE hHeap, // pass in 0 or invalid for Global stats
	__out ULONG * nCurNumAllocs,
	__out ULONG * nCurBytes,
	__out ULONG * nTotNumAllocs,
	__out ULONG * nTotBytes,
	__out ULONG * pnGlobalPassCount
	)
{
	BOOL fGotit = false;
	CHeapSpy *pHeapSpy = (CHeapSpy *) hHeap;
	if (CHeapSpy::IsValidHeap(pHeapSpy))
	{
		*nCurNumAllocs = pHeapSpy->m_cCurNumAllocs;
		*nCurBytes = pHeapSpy->m_cCurNumBytesAllocated;
		*nTotBytes = pHeapSpy->m_cNumBytesAllocated;
		*nTotNumAllocs = pHeapSpy->m_cNumAllocs;
	}
	else
	{
		*nCurNumAllocs = CHeapSpy::s_OverallCurAlloc;
		*nCurBytes = CHeapSpy::s_OverallCurBytes;
	}
	*pnGlobalPassCount = g_ulGlobalPassCount;


	return fGotit;
}

//=--------------------------------------------------------------------------=
// VsDebCheckLeaks
//=--------------------------------------------------------------------------=
// Checks for leaks against the given heap
//
CLINKAGE VOID ENTRYPOINT VsDebCheckLeaks
(
HANDLE hheap,   // heap to check against
UINT_PTR dwInst   // instance to check, or 0 for all instances
)
{
	CHeapSpy      *pHeapSpy;

	// special case for the default heap
	//
	if (DEFAULT_HEAP == hheap)
	{
		pHeapSpy = _pDefaultHeap;
		ASSERT(pHeapSpy, "No default heap");
	}
	else
		pHeapSpy = (CHeapSpy *) hheap;

}


//=--------------------------------------------------------------------------=
// VsDebGetFirstBlock
//=--------------------------------------------------------------------------=
// Returns the first block allocated from the given heap.  Used to traverse
// the entire instance chain.
//
CLINKAGE PALLOCATION ENTRYPOINT VsDebGetFirstBlock
(
HANDLE hheap
)
{
	CHeapSpy      *pHeapSpy;

	// special case for the default heap
	//
	if (DEFAULT_HEAP == hheap)
	{
		pHeapSpy = _pDefaultHeap;
		ASSERT(pHeapSpy, "No default heap");
	}
	else
		pHeapSpy = (CHeapSpy *) hheap;

	return (ALLOCATION *) pHeapSpy->EnumReset();
}


//=--------------------------------------------------------------------------=
// VsDebGetNextBlock
//=--------------------------------------------------------------------------=
// Returns the next block allocated from the given heap.  Used to traverse
// the entire instance chain.
//
CLINKAGE PALLOCATION ENTRYPOINT VsDebGetNextBlock
(
HANDLE hheap
)
{
	CHeapSpy      *pHeapSpy;

	// special case for the default heap
	//
	if (DEFAULT_HEAP == hheap)
	{
		pHeapSpy = _pDefaultHeap;
		ASSERT(pHeapSpy, "No default heap");
	}
	else
		pHeapSpy = (CHeapSpy *) hheap;

	return (ALLOCATION *) pHeapSpy->EnumNext();
}


LPOLESTR GetKnownLeaksFileName(WCHAR *outbuf, int cchBuf)
{
	*outbuf = 0;
	//    GetModuleFileNameW(NULL, outbuf, cchBuf);
	/// GetModuleFileNameW causes deadlock: it's already in g_szProcessExeFullPathName as Ascii
	// change name
	CComBSTR bstrTemp(g_szProcessExeFullPathName);
	wcscpy(outbuf, bstrTemp.m_str);
	WCHAR * p = wcsrchr(outbuf, L'.');
	if (!p)
	{
		p = wcsrchr(outbuf, L'\\');
		if (p)
			p++;
		else
			p = outbuf;
	}

	wcscpy(p, L"_leaks.xml");

	return outbuf;

}

//=--------------------------------------------------------------------------=
// VsIgnoreAllocsInternal
//=--------------------------------------------------------------------------=
// Marks the next series of external allocations as ignorable
//
CLINKAGE void ENTRYPOINT VsIgnoreAllocsInternal
(
BOOL fIgnore
)
{
	//if(fIgnore)
	//    InterlockedIncrement(&_lIgnoreIMalloc);
	//else
	//    InterlockedDecrement(&_lIgnoreIMalloc);

	//ASSERT(_lIgnoreIMalloc >= 0, "Over/underflow ignoring leaks");
}


//
//
// CAddressNode   - this is a simple class used to store details about a
//                  given allocation.  
//=--------------------------------------------------------------------------=
// CAddressNode::operator new
//=--------------------------------------------------------------------------=
// we overload this so we don't get into a cycle.
//


PVOID CAddressNode::operator new(size_t cb)
{
	VSASSERTF((false, "CAddrssNode op new should not be called"));
	return DebugAlloc(ADDRESS_NODE_SIZE);
}


//=--------------------------------------------------------------------------=
// CAddressNode::operator delete
//=--------------------------------------------------------------------------=
void CAddressNode::operator delete(PVOID pv)
{
	VSASSERTF((false, "CAddrssNode op delete should not be called"));
	DebugFree(pv);
}


//=--------------------------------------------------------------------------=
// CAddressNode::DumpInst
//=--------------------------------------------------------------------------=
// Dump instance information out to the debug window
//
VOID CAddressNode::DumpInst()
{
#if !MEMSPECT

	DEBUGPRINTF("%s(%u) Address=0x%lx  nAlloc=%ld  Bytes=%ld\r\n",m_pszFile,
		m_uLine, (ULONG_PTR) m_pv, (ULONG) m_cAlloc, (ULONG) m_cb);
#endif !MEMSPECT
}


//=--------------------------------------------------------------------------=
// CAddressNode::VerifyHeaderTrailer
//=--------------------------------------------------------------------------=
// Inspect allocation for header and trailer signature overwrites
//
void CAddressNode::VerifyHeaderTrailer(CHeapSpy *pHeapSpy)
{
	//Verify the header

	if (memcmp((char *) m_pv, _szHeader, pHeapSpy->m_nHeaderSize) != 0)
	{
		FAIL(SZBADHEADER);
		DEBUGPRINTF(SZBADHEADER);
		DEBUGPRINTF("\r\n");
		DumpInst();
	}

	//Verify the trailer
	if (memcmp((char *) m_pv + m_cb + pHeapSpy->m_nHeaderSize, _szTrailer, pHeapSpy->m_nTrailerSize) != 0)
	{
		FAIL(SZBADTRAILER);
		DEBUGPRINTF(SZBADTRAILER);
		DEBUGPRINTF("\r\n");
		DumpInst();
	}
}

DWORD* CAddressNode::GetStackArray()
{
	if (g_StackStorageMode == InMemory)
	{
		return m_pdwStackAddr;
	}
	return (DWORD *) MemMap::MapView(m_pdwStackAddr[0], m_uicStackAddr * 4);

}

//=--------------------------------------------------------------------------=
// CAddressNode::FillOnFree
//=--------------------------------------------------------------------------=
// Fill the about to be free'd memory
//
void CAddressNode::FillOnFree(CHeapSpy *pHeapSpy)
{
	memset((char *) m_pv + pHeapSpy->m_nHeaderSize, 0xbf, m_cb);
}

//
//
// CStringList  - class to handle a list of file names
//
//


//=--------------------------------------------------------------------------=
// CStringList::~CStringList
//=--------------------------------------------------------------------------=
// death becomes us
//
CStringList::~CStringList()
{
	while (m_pListHead)
	{
		STRINGLISTENTRY *pPrev = m_pListHead;
		m_pListHead = m_pListHead->pNext;
		DebugFree(pPrev);
	}
}


//=--------------------------------------------------------------------------=
// CStringList::Add
//=--------------------------------------------------------------------------=
// Adds the given string to the list.  Doesn't add it if it's already there
//
LPCSTR CStringList::Add
(
LPCSTR  psz
)
{
	if (!psz)
		return NULL;

	if (LPCSTR pszFound = Find(psz))
		return pszFound;

	STRINGLISTENTRY *pNew = (STRINGLISTENTRY *) DebugAlloc(1 + sizeof(STRINGLISTENTRY) +
		sizeof(TCHAR) * lstrlen(psz));

	if (!pNew)
	{
		FAIL("OOM allocating debug string list entry");
		return NULL;
	}

	pNew->pNext = m_pListHead;
	m_pListHead = pNew;

	pNew->pszOriginal = psz;
	strcpy_s(pNew->szCopy, lstrlen(psz) + 1, psz);

	return (LPCSTR) pNew->szCopy;
}


//=--------------------------------------------------------------------------=
// CStringList::Find
//=--------------------------------------------------------------------------=
// Returns our version of the given string.  Our version is a direct copy,
// which is needed because the requested string may have been unloaded.
//
LPCSTR CStringList::Find
(
LPCSTR pszSeek
)
{
	for (STRINGLISTENTRY *pEntry = m_pListHead; pEntry; pEntry = pEntry->pNext)
	{
		if (pEntry->pszOriginal == pszSeek)
		{
			return (LPCSTR) pEntry->szCopy;
		}
	}

	return NULL;
}


//
//
// CHeapSpy   -   class to handle heap leak tracking
//
//

// overall memory allocation statistics
//
volatile LONG CHeapSpy::s_OverallCurAlloc = 0;
volatile LONG CHeapSpy::s_OverallCurBytes = 0;

UINTSetWrapper *CHeapSpy::g_AssertOnStackFrame = 0; // assert if any addr in this set is in the current call stack
UINTSetWrapper *CHeapSpy::g_AssertOnSeqNo = 0;  // assert if the seq no we're currently freeing is in the set


//=--------------------------------------------------------------------------=
// CHeapSpy::operator new
//=--------------------------------------------------------------------------=
// again, we must overload this to prevent a cycle.
//
PVOID CHeapSpy::operator new(size_t size)
{
	return DebugAlloc(size);
}


//=--------------------------------------------------------------------------=
// CHeapSpy::operator delete
//=--------------------------------------------------------------------------=
//
void CHeapSpy::operator delete(PVOID pv)
{
	DebugFree(pv);
}


//=--------------------------------------------------------------------------=
// CHeapSpy::CHeapSpy
//=--------------------------------------------------------------------------=
//
CHeapSpy::CHeapSpy
(
HANDLE  hHeap,        // the real heap
LPCSTR  pszHeapName,
LPCSTR  pszFile,      // the file that created the heap
UINT    uLine         // and the line where it was allocated
)
: m_hHeap(hHeap),
m_uLine(0),
m_cCurNumAllocs(0),
m_cCurNumBytesAllocated(0),
m_cNumAllocs(0),
m_cNumBytesAllocated(0),
m_cStatCurNumAllocs(0),
m_nStackFramesToSkip(0),
m_AddrSetWrapper(0),
m_dwTidOfHeapLock(0),
m_nHeaderSize(0),
m_nTrailerSize(0)
{

	if (s_AddressNodeTemp == NULL) // do we need to init our static?
	{
		// we add 4 to ADDRESS_NODE_SIZE because debug _Alloca does local var stack checking
		s_AddressNodeTemp = (CAddressNode *) DebugAlloc(4 + ADDRESS_NODE_SIZE); // this is never freeed
	}

	// Because we may be called from DLLs that can be unloaded at any time,
	// we can't just store the static string here - we must allocate (bummer)
	//
	m_pszFile = m_StringList.Add(pszFile);

	m_pszHeapName = m_StringList.Add(pszHeapName);

	ASSERT(m_pszFile && m_pszHeapName, "OOM creating heap");
	m_nHeaderSize = g_nRetailHeaderTrailerSize;
	m_nTrailerSize = g_nRetailHeaderTrailerSize;
	/*
	// add new one at the beginning of the list
	this->m_pNextHeapSpy = _pHeapSpyList;
	_pHeapSpyList = this;

	/*/
	m_pNextHeapSpy = NULL;
	// and link it in at the end: earlier heaps are used more: ProcessHeap and MemSpect
	//
	auto next = _pHeapSpyList;
	while (next && next->m_pNextHeapSpy)
	{
		next = next->m_pNextHeapSpy;
	}
	if (next == NULL)
	{
		_pHeapSpyList = this;
	}
	else
	{
		VSASSERTF((next->m_pNextHeapSpy == NULL, "next ptr should be null"));
		next->m_pNextHeapSpy = this;
	}

	//*/
	VSASSERTF((IsValidHeap(this) != NULL, "got invalid heap ? "));


}


//=--------------------------------------------------------------------------=
// CHeapSpy::~CHeapSpy
//=--------------------------------------------------------------------------=
//
CHeapSpy::~CHeapSpy()
{

	CHeapSpy *pPrev = NULL;
	CHeapSpy *pSpy;

	for (pSpy = _pHeapSpyList;
		pSpy && pSpy != this;
		pPrev = pSpy, pSpy = pSpy->m_pNextHeapSpy);

	// should never happen, but we want to protect ourselves anyway
	//
	if (!pSpy)
	{
		FAIL("DebugHeap: heapspy pointer not found in spy list");
		return;
	}

	if (pPrev)
		pPrev->m_pNextHeapSpy = pSpy->m_pNextHeapSpy;
	else
		_pHeapSpyList = pSpy->m_pNextHeapSpy;

	if (m_AddrSetWrapper)
	{
		m_AddrSetWrapper->freemem();
	}
}


//=--------------------------------------------------------------------------=
// CHeapSpy::AddInst
//=--------------------------------------------------------------------------=
// A heap allocation occured.  We save it off so we can track it.
//
void CHeapSpy::AddInst
(
PVOID  pv,       // Pointer to the beginning of the block (HEADHEAD)
SIZE_T dwBytes,  // count of REQUESTED bytes (cbTotal - HEADER - TRAILER)
UINT   ifrStackStart // how many stack frames to skip if we are getting a callstack for the allocation
)
{

	CDisableTrace lock; // we don't want to recur into ourselves when doing stuff internally. If we alloc mem here, we don't want to detour

	CAddressNode * pn = s_AddressNodeTemp; // first a local copy (so not variable size)

	pn->m_cAlloc = InterlockedIncrement((LONG *) &g_ulGlobalPassCount);

	pn->m_pv = pv;                   //memory address of allocation
	pn->m_cb = dwBytes;              //bytes requested to be allocated
	pn->m_dwTid = GetCurrentThreadId(); //tid of this thread
	pn->m_uicStackAddr = 0;

	if (g_fSaveCallstack && g_TrackingMode == Normal) // we don't save stacks for CLR Moved objs
	{
		pn->m_uicStackAddr = VsCaptureStackBackTrace(ifrStackStart + 1, pn->m_pdwStackAddr);

		if (CHeapSpy::g_AssertOnStackFrame)
		{
			auto end = CHeapSpy::g_AssertOnStackFrame->m_pStlType->end();
			for (UINT i = 0; i < pn->m_uicStackAddr; i++)
			{
				auto res = CHeapSpy::g_AssertOnStackFrame->m_pStlType->find(pn->m_pdwStackAddr[i]);
				if (res != end)
				{
					auto save_g_fEnableAsserts = g_fEnableAsserts;
					g_fEnableAsserts = 1;
					VSASSERTF((false, "AssertOnStackFrame Addr = %x", pn->m_pdwStackAddr[i]));
					g_fEnableAsserts = save_g_fEnableAsserts;
					break;
				}
			}
		}
	}

	if (m_AddrSetWrapper == NULL)
	{
		m_AddrSetWrapper = new (DebugAlloc(sizeof(AddrSetWrapper))) AddrSetWrapper(MySTLAlloc<pair<LPVOID, CAddressNode * > >(InternalHeapToUse));
	}

	// now copy the local temp version to a heap one of variable size
	DWORD nNodeSize = sizeof(CAddressNode) +pn->m_uicStackAddr* sizeof(DWORD);
	if (g_StackStorageMode != InMemory)
	{
		nNodeSize = sizeof(CAddressNode) +sizeof(DWORD);
		MemMap::AddStack(pn);
	}

	CAddressNode *pCAddressNode = (CAddressNode *) DebugAlloc(nNodeSize);
	memcpy(pCAddressNode, pn, nNodeSize);
#if MSDEBUG
	// for some reason on winblue 
	VSASSERT(m_AddrSetWrapper->m_pStlType->find(pv) == m_AddrSetWrapper->m_pStlType->end(), "Adding node to addrset already exists?");
#endif MSDEBUG

	auto addrSetIns = m_AddrSetWrapper->m_pStlType->insert(pair<LPVOID, CAddressNode *>(pv, pCAddressNode));
	if (addrSetIns.second == false) // didn't insert
	{
#if MSDEBUG
		VSASSERT(addrSetIns.second == true, "AddrSetWrapper insert failed");
#endif MSDEBUG
	}
	else
	{
		g_memStats.HeapAllocs.Alloc.count++;
		g_memStats.HeapAllocs.Alloc.size+= dwBytes;
		++m_cCurNumAllocs;

		// Copy header and trailer signatures.
		// and update our statistics

		// this should not include instances added by detoured Mine_SysAlloc* functions via 
		// AddSysAllocInst (to work with oleaut32 BSTR caching turned on) , because they are actually duplicates of the ones added by PostAlloc 
		// via AddInst, which are marked to ignore.
		{
			//memcpy((char *)pv, _szHeader, m_nHeaderSize);
			//memcpy((char *)pv + m_nHeaderSize + dwBytes, _szTrailer, m_nTrailerSize);

			++m_cStatCurNumAllocs;
			++m_cNumAllocs;

			m_cCurNumBytesAllocated += dwBytes;
			m_cNumBytesAllocated += dwBytes;


			InterlockedIncrement(&s_OverallCurAlloc);
			InterlockedExchangeAdd(&s_OverallCurBytes, dwBytes);

			if (g_fDumpMemStats && (m_cNumAllocs % g_nDumpMemStatInterval) == 0)
			{
				FAIL("Undone: DumpHeapStats()");
			}
		}
	}
}

//=--------------------------------------------------------------------------=
// CHeapSpy::ChangeInst
//=--------------------------------------------------------------------------=
// Change the allocation information to reflect a realloc.
//
void CHeapSpy::ChangeInst
(
PVOID  pvOld,
PVOID  pvNew,
SIZE_T dwBytes,
UINT ifrStackStart
)
{
	if (FSWITCH(_fTraceAllocations))
	{
		CAddressNode *pn = FindInst(pvOld);
		ASSERT(pn != NULL, "Failure tracking reallocation: Old pointer does not exist");
		DEBUGPRINTF("Realloc (old): ");
		pn->DumpInst();
	}


	DeleteInst(pvOld);
	AddInst(pvNew, dwBytes, ifrStackStart + 1); // +1 to skip this function

	if (FSWITCH(_fTraceAllocations))
	{
		CAddressNode *pn = FindInst(pvNew);
		ASSERT(pn != NULL, "Failure tracking reallocation: newly created pointer does not exist");
		DEBUGPRINTF("Realloc (new): ");
		if (pn)
			pn->DumpInst();
	}
}

void ProcessGhostFree(CHeapSpy *pHeapSpy, CAddressNode *pn)
{
	char  parms[12];
	((DWORD *)parms)[0] = (DWORD)pHeapSpy;
	((DWORD *)parms)[1] = (DWORD)pn;
	((DWORD *)parms)[2] = (DWORD)g_ulGlobalPassCount;
	// we send a msg, can be async
	char outbuf[4];
	int nLen = SendMsgToChild(GhostAllocFree, sizeof(parms), parms, sizeof(outbuf), outbuf);

}

//=--------------------------------------------------------------------------=
// CHeapSpy::DeleteInst
//=--------------------------------------------------------------------------=
// A heap allocation got free or was reallocated so remove the
// information from the instance table and check for memory trashing.
//
void CHeapSpy::DeleteInst(PVOID pv)
{

	VSASSERT(m_AddrSetWrapper, "where'd our data go? DeleteInst");
	auto addrsetFind = m_AddrSetWrapper->m_pStlType->find(pv);
	VSASSERT(addrsetFind != m_AddrSetWrapper->m_pStlType->end(), "delete inst couldn't find addr in addrset");

	CAddressNode *pn = addrsetFind->second;

	if (g_AssertOnSeqNo)
	{
		auto res = g_AssertOnSeqNo->m_pStlType->find(pn->m_cAlloc);
		if (res != g_AssertOnSeqNo->m_pStlType->end())
		{
			auto save_g_fEnableAsserts = g_fEnableAsserts;
			g_fEnableAsserts = 1;
			VSASSERTF((false, "Seq No %d is being freed (size = %d)", pn->m_cAlloc, pn->m_cb));
			g_fEnableAsserts = save_g_fEnableAsserts;
		}
	}

	if (g_fTrackingGhosts) // send msg to child proc
	{
		ProcessGhostFree(this, pn);
	}
	g_memStats.HeapAllocs.Free.count++;
	g_memStats.HeapAllocs.Free.size += pn->m_cb;
	--m_cCurNumAllocs;
	{
		--m_cStatCurNumAllocs;
		m_cCurNumBytesAllocated -= pn->m_cb;
		InterlockedDecrement(&s_OverallCurAlloc);
		InterlockedExchangeAdd(&s_OverallCurBytes, -(LONG) (pn->m_cb));
	}

	auto addrSetErase = m_AddrSetWrapper->m_pStlType->erase(pv);
	if (g_StackStorageMode != InMemory)
	{
		MemMap::RemoveStack(pn);
	}
	DebugFree(pn);
	VSASSERT(addrSetErase == 1, "addrset erase failed");
	if (m_AddrSetWrapper->m_pStlType->empty()) //last one out turn off the lights
	{
		m_AddrSetWrapper->freemem();
		m_AddrSetWrapper = 0;
	}
}

void  CHeapSpy::ReplaceInst(PVOID pv, CAddressNode *pCAddressNode)
{
	// we want to replace the debugalloc info for an allocated block with new info that's already been calc'd (including callstack)
	// we don't want to call ChangeInst, which deletes/recreates the debugalloc info

	VSASSERT(m_AddrSetWrapper, "where'd our data go? ReplaceInst");
	auto addrsetFind = m_AddrSetWrapper->m_pStlType->find(pv);
	VSASSERT(addrsetFind != m_AddrSetWrapper->m_pStlType->end(), "delete inst couldn't find addr in addrset");
	CAddressNode * pn = addrsetFind->second;

	if (g_fTrackingGhosts) // send msg to child proc
	{
		ProcessGhostFree(this, pn);
	}

	pCAddressNode->m_pv = pn->m_pv; // preserve ptr to allocation

	if (g_StackStorageMode != InMemory)
	{
		MemMap::RemoveStack(pn);
	}
	DebugFree(pn); // free replaced CAddressNode

	auto addrSetErase = m_AddrSetWrapper->m_pStlType->erase(pv);
	VSASSERT(addrSetErase == 1, "addrset erase failed in ReplaceInst");

	auto addrSetIns = m_AddrSetWrapper->m_pStlType->insert(pair<LPVOID, CAddressNode *>(pv, pCAddressNode));
	VSASSERT(addrSetIns.second == true, "addrset insert ReplaceInst failed");
}

//=--------------------------------------------------------------------------=
// CHeapSpy::FindInst
//=--------------------------------------------------------------------------=
// Give a pointer to an allocation, return a pointer to the debug allocation
// information.
//
CAddressNode *CHeapSpy::FindInst
(
PVOID pv
)
{
	CAddressNode * pn = NULL;

	if (m_AddrSetWrapper)
	{
		auto addrsetFind = m_AddrSetWrapper->m_pStlType->find(pv);
		if (addrsetFind != m_AddrSetWrapper->m_pStlType->end())
		{
			pn = addrsetFind->second;
		}
	}
	return pn;
}


//=--------------------------------------------------------------------------=
// CHeapSpy::IsEmpty
//=--------------------------------------------------------------------------=
// Returns TRUE if the Instance table is empty (no leaks)
//
BOOL CHeapSpy::IsEmpty
(
UINT_PTR dwInst // instance ID to check, or 0 for all
)
{
	CAddressNode *pn = EnumReset();

	if (!pn)
		return TRUE;

	// Cache this for speed
	//
	LPCSTR pszNoFile = m_StringList.Find(_szNoFile);

	for (pn = EnumReset(); pn; pn = EnumNext())
	{
		if (_IsIgnoreThisLeak(pn, dwInst, pszNoFile))
			continue;

		return FALSE;
	}

	return TRUE;
}


//=--------------------------------------------------------------------------=
// CHeapSpy::AnalyzeInst
//=--------------------------------------------------------------------------=
// Given a pointer try determine if it is a valid Read and Write pointer
// and what heap it was allocated in if any.
//
VOID CHeapSpy::AnalyzeInst
(
LPVOID pv
)
{
	CHeapSpy * pHeapSpy;
	CAddressNode * pn = NULL;

	pHeapSpy = FindCorrectHeap(pv);

	// if we found the instance to be in a different heap then output that info.
	//
	if (pHeapSpy && pHeapSpy != this)
	{
		// we thought pv was in m_pszHeapName but its really in szCorrectHeap
		//
		LPCSTR szCorrectHeap = pHeapSpy->m_pszHeapName;
		DEBUGPRINTF("AnalyzeInst found that pv=%lX is in '%s' not '%s'\r\n",
			pv, szCorrectHeap, m_pszHeapName);
	}
	else
	{
		// Either we've already got the correct heap or the pointer does not point to any
		// known heap allocations (pHeapListWalk == NULL).   Here we check if it
		// points to readable or writable memory and if it is in the addressable
		// range for the given OS.
		//
		BOOL fBadAddress;

		// note that on Win95 memory above 0xC0000000 is read/writable but not usable for heaps!
		//
		if (TRUE)
			// UNDONE (v-mattlp 10/22/98): need correct values for Win64 here
			fBadAddress = ((INT_PTR) pv <= 0x0000FFFF) || ((INT_PTR) pv >= 0x7FFF0000);
		else
			fBadAddress = ((INT_PTR) pv <= 0x003FFFFF) || ((INT_PTR) pv >= 0xC0000000);

		if (pHeapSpy == this)
		{
			ASSERT(!fBadAddress, "AnalyzeInst - pv found in a known heap but it's a bad pointer.");

			// here we assume that the Win32 heap API failed.  Why else would AnalyzeInst have been
			// called if we have the correct hHeap?
			//
			DEBUGPRINTF("AnalyzeInst found that pointer pv=0x%lX was used with the correct VSHeap API\r\n" \
				"but the Win32 heap API failed.  This means that the HeapSpy debug tracker\r\n" \
				"is out of sync with the underlying heap.\r\n", pv);
		}
		else  // pointer does not point to a known allocation
		{
			ASSERT(!pHeapSpy, "AnalyzeInst:  Pointer doesn't point to a known allocation");

			// Report what we know about the memory address
			if (fBadAddress)
				DEBUGPRINTF("AnalyzeInst found that pointer pv=0x%lX is outside the addressable\n\r"
				"range for this operating system.\n\r", pv);
			else
				DEBUGPRINTF("AnalyzeInst found that pointer pv=0x%lX is in the addressable\n\r"
				"range for this operating system and points to readable/writable\n\r"
				"memory.  However it does not point to a known allocation.  Perhaps\n\r"
				"the allocation was already freed.\n\r", pv);
		}
	}

	if (pHeapSpy)
	{
		pn = pHeapSpy->FindInst(pv);
		if (pn)
			pn->DumpInst();
	}
}



//=--------------------------------------------------------------------------=
// CHeapSpy::DumpStatistics
//=--------------------------------------------------------------------------=
// dumps allocation statistics for this heap to the debug window
//
void CHeapSpy::DumpStatistics()
{
	char szFormat[] = "\t%s\t:%lu";

	DEBUGPRINTF("\r\nStaticstics for heap '%s':", m_pszHeapName);


	DEBUGPRINTF(szFormat, "Current allocations    ", m_cStatCurNumAllocs);
	DEBUGPRINTF(szFormat, "Current bytes allocated", m_cCurNumBytesAllocated);
	DEBUGPRINTF(szFormat, "Total allocations      ", m_cNumAllocs);
	DEBUGPRINTF(szFormat, "Total bytes allocated  ", m_cNumBytesAllocated);
}


//=--------------------------------------------------------------------------=
// CHeapSpy::HeapCheck
//=--------------------------------------------------------------------------=
// Inspect all of the allocations for header and trailer signature overwrites
//
void CHeapSpy::HeapCheck()
{
	if (!FSWITCH(_fHeapCheck))
		return;

	if ((m_cNumAllocs % _cHeapCheckInterval) != 0)
		return;

	// Not all spies have heaps (IMalloc, for example)
	//
	if (m_hHeap)
	{
		ASSERT(HeapValidate(m_hHeap, 0, NULL), "Internal heap error");
	}

	CAddressNode * pn = EnumReset();
	while (pn)
	{
		pn->VerifyHeaderTrailer(this);
		pn = EnumNext();
	}
}


//
//
// Private CHeapSpy goo
//
//


//=--------------------------------------------------------------------------=
// CHeapSpy::EnumReset
//=--------------------------------------------------------------------------=
// Reset the enumerator and return the first node.  NULL if empty.
//
CAddressNode * CHeapSpy::EnumReset()
{
	CAddressNode *pn = NULL;
	if (m_AddrSetWrapper)
	{
		m_AddrNodeInstSetIterator = m_AddrSetWrapper->m_pStlType->begin();
		if (m_AddrNodeInstSetIterator != m_AddrSetWrapper->m_pStlType->end())
		{
			pn = m_AddrNodeInstSetIterator->second;
			m_AddrNodeInstSetIterator++;
		}
	}
	return pn;
}


//=--------------------------------------------------------------------------=
// CHeapSpy::EnumNext
//=--------------------------------------------------------------------------=
// Return the next node in the enumeration.  m_pnEnumNode points to the last
// node returned.  It is NULL if no more left.
//
CAddressNode * CHeapSpy::EnumNext()
{
	CAddressNode *pn = NULL;
	if (m_AddrSetWrapper)
	{
		AddrNodeInstSet::iterator end = m_AddrSetWrapper->m_pStlType->end();

		if (m_AddrNodeInstSetIterator != end)
		{
			pn = m_AddrNodeInstSetIterator->second;
			m_AddrNodeInstSetIterator++;
		}
	}
	return pn;
}


//
//
// Static CHeapSpy goo
//
//


//=--------------------------------------------------------------------------=
// CHeapSpy::FindCorrectHeap
//=--------------------------------------------------------------------------=
// Given a pv return the heap that the allocation was made in.   This
// operation is linear with the number of heaps so it can be slow.
// pv should point to the beginning of the allocation.  The primary
// heap is always the first heap in the list.
//
CHeapSpy *CHeapSpy::FindCorrectHeap
(
PVOID pv
)
{

	CAddressNode * pn;
	CHeapSpy *pHeapListWalk = _pHeapSpyList;
	// first determine if it points to memory in a heap that we know about
	pn = pHeapListWalk->FindInst(pv);
	while (pn == NULL)
	{
		pHeapListWalk = pHeapListWalk->m_pNextHeapSpy;
		if (!pHeapListWalk)
			break;
		pn = pHeapListWalk->FindInst(pv);
	}
	return pHeapListWalk;
}


//=--------------------------------------------------------------------------=
// CHeapSpy::CheckAllHeaps
//=--------------------------------------------------------------------------=
//
void CHeapSpy::CheckAllHeaps()
{

	CHeapSpy * pHeapSpyCur;

	for (pHeapSpyCur = _pHeapSpyList;
		pHeapSpyCur;
		pHeapSpyCur = pHeapSpyCur->m_pNextHeapSpy)
		pHeapSpyCur->HeapCheck();

	// Now we check ALL process heaps (which will include the ones we have
	// already checked).  This will check any heaps in this process.  This
	// will include the heaps used by IMalloc!
	//
	DWORD cHeaps = GetProcessHeaps(0, NULL);
	if (cHeaps)
	{
		HANDLE *phHeap = (HANDLE *) DebugAlloc(cHeaps * sizeof(HANDLE));
		if (phHeap)
		{
			DWORD dw = GetProcessHeaps(cHeaps, phHeap);
			ASSERT(dw == cHeaps, "No heaps to check");

			while (dw > 0)
			{
				dw -= 1;
				ASSERT(HeapValidate(phHeap[dw], 0, NULL), "External heap error");
			}

			DebugFree(phHeap);
		}
	}
}



//=--------------------------------------------------------------------------=
// CHeapSpy::DumpAllStatistics
//=--------------------------------------------------------------------------=
// Dumps statistics for all heaps we have
//
void CHeapSpy::DumpAllStatistics()
{

	CHeapSpy *pWalk = _pHeapSpyList;

	while (pWalk)
	{
		pWalk->DumpStatistics();
		pWalk = pWalk->m_pNextHeapSpy;
	}
}


//static
int CHeapSpy::UpdateRetailHeapNames(BOOL fGetCountOnly)
{
	VSASSERT(false, "Dead code");
	int nCount = 0;
	CHeapSpy *pHeapSpy;
	for (pHeapSpy = _pHeapSpyList; pHeapSpy; pHeapSpy = pHeapSpy->m_pNextHeapSpy)
	{
		if (pHeapSpy->m_pszHeapName[1] == '0' &&
			pHeapSpy->m_pszHeapName[2] == 'x')
		{
			nCount++;
			if (!fGetCountOnly)
			{
				DWORD retAddr = pHeapSpy->m_uLine; // overloaded
				pHeapSpy->m_uLine = 0;
				char buf[1024];
				buf[0] = pHeapSpy->m_pszHeapName[0]; // preserve 1st char
				if (GetStringFromAddr(retAddr, buf + 1, sizeof(buf) -1, true)) // no line # info
				{
					pHeapSpy->m_pszHeapName = pHeapSpy->m_StringList.Add(buf);
				}
			}

		}
	}
	return nCount;
}

//=--------------------------------------------------------------------------=
// CHeapSpy::IsValidHeap
//=--------------------------------------------------------------------------=
// Check if hHeap is in the heap list.
//
//static
CHeapSpy * CHeapSpy::IsValidHeap
(
CHeapSpy *pHeapSpy,
BOOL fCheckHandle /*= false*/ // if true, check to see if the real handle is in our list
)
{

	CHeapSpy *pWalk = _pHeapSpyList;
	while (pWalk && pWalk != pHeapSpy)
	{
		if (fCheckHandle)
		{
			if (pWalk->m_hHeap == (HANDLE) pHeapSpy)
			{
				break;
			}
		}
		pWalk = pWalk->m_pNextHeapSpy;
	}
	return pWalk;
}


//=--------------------------------------------------------------------------=
// CHeapSpy::IsValidHeapPtr
//=--------------------------------------------------------------------------=
// Check if pv points to a known heap allocation, that is a pVoid returned
// from VBAlloc, VBReAlloc, VBHeapAlloc, or VBHeapReAlloc.
// If pHeapSpy is NULL then we search for the correct heap.   The primary
// heap is always searched first so it is reasonably fast.
//
BOOL CHeapSpy::IsValidHeapPtr
(
CHeapSpy *pHeapSpy,
PVOID     pv
)
{
	BOOL fRet;
	char * szCorrectHeap = NULL;
	CHeapSpy *pCorrectHeap;

	pv = (BYTE *) pv - pHeapSpy->m_nHeaderSize; //need to account for the header
	// If this pointer was allocated with New then need to account for stored CHeapSpy *
	// we take a guess by checking if we are pointing to "HEAD" header.
	if (memcmp((char *) pv, _szHeader, pHeapSpy->m_nHeaderSize) != 0)
	{
		CHeapSpy *pHeapSpyFromAlloc = *(CHeapSpy **) ((BYTE *) pv + pHeapSpy->m_nHeaderSize - sizeof(CHeapSpy **));
		ASSERT(!pHeapSpy || pHeapSpyFromAlloc == pHeapSpy, "IsValidHeapPtr: passed in heap doesn't match one in allocation header");

		pv = (BYTE *) pv - sizeof(CHeapSpy **);

		// now we should be pointing to the beginning of the actual allocation
		ASSERT(memcmp((char *) pv, _szHeader, pHeapSpy->m_nHeaderSize) == 0, "Header not found");
	}

	if (pHeapSpy)
	{
		fRet = (pHeapSpy->FindInst(pv) != NULL);
		if (!fRet)
		{
			pCorrectHeap = FindCorrectHeap(pv);
			if (pCorrectHeap)
				DEBUGPRINTF("IsValidHeapPtr failed - %s is the wrong heap.  The correct heap is %s\n\r", pHeapSpy->m_pszHeapName, pCorrectHeap->m_pszHeapName);
			else
				DEBUGPRINTF("IsValidHeapPtr failed - Ptr %lX does not point to any known heap allocation.\n\r", pv);
		}
	}
	else
	{
		fRet = (pHeapSpy->FindInst(pv) != NULL);
		if (!fRet)
			DEBUGPRINTF("IsValidHeapPtr failed - Ptr %lX does not point to any known heap allocation.\n\r", pv);
	}
	return fRet;
}






//=--------------------------------------------------------------------------=
// _IsIgnoreThisLeak
//=--------------------------------------------------------------------------=
// Returns TRUE if leak should be ignored.
//
static BOOL _IsIgnoreThisLeak(CAddressNode *pn, UINT_PTR dwInst, LPCSTR pszNoFile)
{
	BOOL fIgnoreThisLeak = FALSE;
#if !MEMSPECT

	if(pn->m_fIgnorable)
	{
		fIgnoreThisLeak = TRUE;
	}
	else if(!g_fEnableExternalLeaks && pn->m_pszFile == pszNoFile)
	{
		fIgnoreThisLeak = TRUE;
	}
	else if(dwInst && pn->m_dwInst == dwInst)
	{
		fIgnoreThisLeak = TRUE;
	}
	else if(!pn->m_cb)
		fIgnoreThisLeak = TRUE;
#endif !MEMSPECT
	return fIgnoreThisLeak;
}
//=--------------------------------------------------------------------------=
// EOF

CDisableTrace::CDisableTrace()
{
	// the threaddata may not be initialized yet: check for that. 
	// If so, we're in the middle of initing it, so  no detour
	m_ptd = GetThreadData(/*fcreate=*/g_ThreadDynamic ? true : false);

	if (m_ptd)
	{
		m_ptd->m_ulDisableTrace++;
	}
}


//static
BOOL CDisableTrace::CanDetour()
{
	BOOL  fCanDetour = true;
	//*
	// the threaddata may not be initialized yet: check for that. 
	// If so, we're in the middle of initing it, so  no detour
	THREADGLOBAL *ptd;
	if (_dwTls == INVALID_TLS)
	{
		return false;
	}
	//if (g_TrackingMode == 0) // minimal tracking
	//{
	//    return false;
	//}
	ptd = GetThreadData(/*fcreate=*/g_ThreadDynamic ? true : false);

	if (ptd == NULL)
	{
		return false;
	}

	if (ptd->m_ulDisableTrace > 0)
	{
		fCanDetour = false;
	}
	/*/
	fCanDetour = CDisableTrace::CanDetour();
	//*/

	// while shutting down, we might be trying to print a leak report, which resolves symbols, which allocates...
	if (g_fShuttingDown)
	{
		fCanDetour = false;
	}
	return fCanDetour;

}

CDisableTrace::~CDisableTrace()
{
	if (m_ptd)
	{
		if (m_ptd->m_ulDisableTrace)
		{
			m_ptd->m_ulDisableTrace--;
		}
	}
}



//---------------------------------------------------------


