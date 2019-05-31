// ==++==
//
//   Copyright (c) Microsoft Corporation.  All rights reserved.
//
// ==--==
// ===========================================================================
// File: alloc.cpp
//
// Handles the memory allocation for the compiler
//
// There are three memory allocators:
//
// NRHEAP - "no-release" heap; incrementally allocates from pages
//          allocated from the PAGEHEAP.
// ===========================================================================

#include "stdafx.h"

/*
 * We have special allocation rules in the compiler, where everything
 * should be allocated out of a heap associated with the compiler
 * instance we are in. Prevent people from using new/delete or
 * malloc/free.
 *<EMAIL>
 *************** REMOVED 7/6/99 [randyki] *****************</EMAIL>
 *
 * This is a great idea, but in fact the above rules are not 100% true.
 * There are some allocations which "outlive" compiler instances (the
 * compiler has been made multi-instance since the rules were made)
 * such as allocations from the name table, and the name table itself
 * (among other COM objects whose references are provided to parties
 * outside the compiler, such as source modules, source data objects,
 * etc.)
 *
 * These allocations are now made from the module's default heap, using
 * VSMEM's allocation tracking mechanism for standard leak detection,
 * etc.
 
PVOID __cdecl operator new(size_t size)
      {
        VSFAIL("Never use the global operator new in the compiler");
        return 0;
      }
PVOID __cdecl operator new(size_t size, PCSTR pszFile, UINT uLine)
      {
        VSFAIL("Never use the global operator new in the compiler");
        return 0;
      }
void  __cdecl operator delete(PVOID pv)
      {
        VSFAIL("Never use the global operator delete in the compiler");
      }
 */

VSDEFINE_SWITCH(gfNRHeapValidation, "C# IntelliSense", "Enable NR Heap validation (slow)");

/*
 ************************** NRHEAP methods ******************************
 */

/* The "no-release" heap works by incrementally allocating from a page
 * of memory that was gotten from the page allocator. An allocation operation
 * is just an increment of the current pointer, and a range check to make
 * sure that we haven't gone beyond the end of the page. If we do go beyond
 * the end of the page, we just allocate a new page. All the pages
 * are linked together.
 */

//copy c:\VSPro\binaries\x86ret\bin\i386\vc7\vcpackages\cslangsvc.* "c:\Program Files (x86)\Microsoft Visual Studio 11.0\VC#\VCSPackages"
//copy c:\VSPro\binaries\x86ret\bin\i386\vspkgs\msvbide.* "c:\Program Files (x86)\Microsoft Visual Studio 11.0\Common7\IDE"
typedef DWORD (__stdcall *MemSpectArenaCreated)(DWORD unused, WCHAR *wArenaName, DWORD dwUserData);
typedef BOOL (__stdcall *MemSpectArenaAllocation)(DWORD hArena, DWORD dwSize, PVOID addrAlloc, DWORD dwUserData);
typedef BOOL (__stdcall *MemSpectArenaRelease)(DWORD hArena, PVOID mark);
typedef BOOL (__stdcall *MemSpectArenaDestroy)(DWORD hArena);

static bool g_MemSpectLoadedTest = false; // true if we've tested for MemSpect loaded. Only want to do this once

static bool g_MemSpectTrackingNRLS = false; // true if we're tracking NRLS via MemSpect

MemSpectArenaCreated g_MemSpectArenaCreated;

MemSpectArenaAllocation g_MemSpectArenaAllocation;

MemSpectArenaRelease g_MemSpectArenaRelease;

MemSpectArenaDestroy g_MemSpectArenaDestroy;



NRHEAP::NRHEAP(PAGEHEAP& pageHeap) : m_heapPage(pageHeap)
{
    Init(L"", ProtectedEntityFlags::Other);
}

NRHEAP::NRHEAP(PAGEHEAP& pageHeap, const CStringW& heapName) : m_heapPage(pageHeap)
{
    Init(heapName, ProtectedEntityFlags::Other);
}

NRHEAP::NRHEAP(PAGEHEAP& pageHeap, const CStringW& heapName, ProtectedEntityFlagsEnum entity) : m_heapPage(pageHeap)
{
    Init(heapName, entity);
}

void NRHEAP::Init(const CStringW& heapName, ProtectedEntityFlagsEnum entity)
{
    nextFree = limitFree = NULL;
    pageList = pageLast = NULL;
    allowReadOnlyDirectives = false;
    anyPageMarkedReadOnly = false;
    inAllowingWrite = false;
    this->entity = entity;
    
    m_heapName = heapName;
    
    if (!g_MemSpectLoadedTest)
    {
        g_MemSpectLoadedTest = true;
        HMODULE hMemSpectDll = GetModuleHandle("MemSpectDll.dll");
        if (hMemSpectDll)
        {
            g_MemSpectArenaCreated = (MemSpectArenaCreated)GetProcAddress(hMemSpectDll, "_ArenaCreated@12");
            g_MemSpectArenaAllocation = (MemSpectArenaAllocation)GetProcAddress(hMemSpectDll, "_ArenaAllocation@16");
            g_MemSpectArenaRelease= (MemSpectArenaRelease)GetProcAddress(hMemSpectDll, "_ArenaRelease@8");
            g_MemSpectArenaDestroy= (MemSpectArenaDestroy)GetProcAddress(hMemSpectDll, "_ArenaDestroy@4");

            if (g_MemSpectArenaCreated &&
                g_MemSpectArenaAllocation &&
                g_MemSpectArenaRelease &&
                g_MemSpectArenaDestroy)
            {
                g_MemSpectTrackingNRLS = true;
            }
            
        }
    }
    if (g_MemSpectTrackingNRLS)
    {
        m_hMemSpectArenaHandle = g_MemSpectArenaCreated(0,(WCHAR *)(const WCHAR *)heapName, entity);
    }
}

NRHEAP::~NRHEAP()
{
    FreeHeap();
    if (g_MemSpectTrackingNRLS)
    {
        g_MemSpectArenaDestroy(m_hMemSpectArenaHandle);
    }
}

/*
 * Allocate zeroed memory.
 */
void * NRHEAP::_AllocZero(size_t sz
#ifdef DEBUG
                          , PCSTR pszFile, UINT iLine    // Again, these aren't used
#endif
                         )
{
    void * p = _Alloc(sz
#ifdef DEBUG
                      , pszFile, iLine
#endif
                     );
    memset(p, 0, sz);  // zero the memory.
    return p;
}

/*
 * Allocate (duplicate) a wide char string.
 */
PWSTR NRHEAP::AllocStr(PCWSTR str)
{
    if (str == NULL)
        return NULL;

    size_t str_len = wcslen(str);

    // integer overflow
    if (!(str_len < str_len + 1))
    {
        ASSERT(str_len < str_len + 1);
        return NULL;
    }

    PWSTR strNew = (PWSTR) Alloc((str_len + 1) * sizeof(WCHAR));
    HRESULT hr;
    hr = StringCchCopyW (strNew, str_len + 1, str);
    ASSERT (SUCCEEDED (hr));
    return strNew;
}


void NRHEAP::ForbidWriteInternal(void * p, size_t sz)
{
    ASSERT (IsAddressInHeap (p));
    PageProtect::ForbidWrite (entity, p, sz);
}

void NRHEAP::AllowWriteInternal(void * p, size_t sz)
{
    ASSERT (IsAddressInHeap (p));
    PageProtect::AllowWrite (entity, p, sz);
}

void NRHEAP::SetPageWriteStatus(NRPAGE * page, bool writeable)
{
    SetPageRegionWriteStatus(page, page, writeable);
}

void NRHEAP::SetPageRegionWriteStatus(NRPAGE * first, NRPAGE * last, bool writeable)
{
    size_t end = last->limitAvail - ((BYTE*)first);
    if (writeable)
    {
        AllowWriteInternal(first, end);
    }
    else
    {
        anyPageMarkedReadOnly = true;
        ForbidWriteInternal(first, end);
    }
}

bool NRHEAP::IsAddressInCurrentPage(const void * addr) const
{
    if (!pageList)
        return false;
    return (addr >= pageLast && addr < pageLast->limitAvail);
}

void NRHEAP::SetAllPagesWriteStatus(bool writeable)
{
    if (!PageProtect::IsEntityProtected(this->entity))
    {
        // if the entity type for this heap is not protected,
        // short-circuit before iterating pages
        return ;
    }

    for (NRPAGE * first = pageList; first; first = first->next)
    {
        NRPAGE * end = first;
        PAGEHEAP::PAGEARENA * arena = NULL;
        for (NRPAGE * last = end->next; last; last = last->next)
        {
            if (end->limitAvail == (BYTE*)last)
            {
                if (arena == NULL)
                {
                    arena = m_heapPage.FindArena(first);
                }
                if (arena->OwnsPage(last))
                {
                    end = last;
                }
                else
                {
                    break;
                }
            }
            else
            {
                break;
            }
        }
        SetPageRegionWriteStatus(first, end, writeable);
        first = end;
    }
}

void NRHEAP::MakeCurrentPageReadOnlyInternal()
{
    if (pageLast && allowReadOnlyDirectives)
    {
        SetPageWriteStatus(pageLast, false);
    }
}


void NRHEAP::MakeCurrentPageWriteableInternal()
{
    if (anyPageMarkedReadOnly && pageLast)
    {
        SetPageWriteStatus(pageLast, true);
    }
}


void NRHEAP::MakeAllHeapWriteableInternal()
{
    SetAllPagesWriteStatus(true);
    anyPageMarkedReadOnly = false;
}


void NRHEAP::MakeAllHeapReadOnlyInternal()
{
    if (!allowReadOnlyDirectives)
        return ;
    SetAllPagesWriteStatus(false);
}

/*
 * Allocate a new block of memory. This should be VERY FAST and VERY SMALL. 
 */
void * NRHEAP::_Alloc(size_t sz
#ifdef DEBUG
                      , PCSTR pszFile, UINT iLine        // NOTE:  These aren't used here, since NRHEAP's aren't leak-checked (obviously)...
#endif
                     )
{
    void * p = nextFree;
    if (sz == 0 && p == NULL)
        sz = 1;

#ifdef DEBUG

    if ((nextFree += DebugSize(sz)) > limitFree)
        return AllocMore(DebugSize(sz));
    else
    {
        MakeCurrentPageWriteableInternal();
        // fill in with non-zero values so user doesn't expect zeroed memory.
        return DebugFill(p, sz);
    }
#else
    if ((nextFree += RoundUpAllocSize(sz)) > limitFree)
        return AllocMore(sz);
    else
    {
        MakeCurrentPageWriteableInternal();
        if (g_MemSpectTrackingNRLS)
        {
            g_MemSpectArenaAllocation(m_hMemSpectArenaHandle, sz, p, 0);
        }
        return p;
    }
#endif //DEBUG
}


/*
 * An allocation request has overflowed the current page.
 * allocate a new page and try again.
 */
void * NRHEAP::AllocMore(size_t sz)
{
    sz = RoundUpAllocSize(sz);   // round to appropriate byte boundary.
    NRPAGE * newPage = NewPage(sz);
    ASSERT(newPage->next == NULL);

    // Set all the memory in this page as available for alloc.
    nextFree = newPage->firstAvail;
    limitFree = newPage->limitAvail;

    // Link the page in to the list.
    if (pageLast)
    {
        MakeCurrentPageWriteableInternal();
        pageLast->next = newPage;
        MakeCurrentPageReadOnlyInternal();
        pageLast = newPage;
    }
    else
    {
        // First page.
        pageList = pageLast = newPage;
    }

    // We must now be able to allocate the memory.
    return Alloc(sz);
}

/*
 * Allocate a new page of memory, with enough size
 * to handle a block of size sz.
 */
NRPAGE * NRHEAP::NewPage(size_t sz)
{
    size_t pageSize = PAGEHEAP::pageSize; // Page size.
    size_t allocSize;
    NRPAGE * newPage;

    allocSize = RoundUp(sz + sizeof(NRPAGE), pageSize);

    // Allocate the new page.
    newPage = (NRPAGE *) m_heapPage.AllocPages(allocSize);
    // EDMAURER 'AllocPages' throws if it cannot fulfill the request.
    ASSERT (newPage);
    // Initialize the new page.
    newPage->next = NULL;
    newPage->firstAvail = (BYTE *)newPage + RoundUpAllocSize(sizeof(NRPAGE));
    newPage->limitAvail = ((BYTE *)newPage) + allocSize;
    newPage->m_pDestructibleObjects = new CAtlArray<IHasDestructor*>;

    // We should have enough room in the new page!
    ASSERT(newPage->limitAvail > newPage->firstAvail);
    ASSERT((unsigned)(newPage->limitAvail - newPage->firstAvail) >= sz);

    return newPage;
}

/*
 * Return a mark of the current allocation state, so you
 * can free all memory allocated subsequent.
 */
void NRHEAP::Mark(NRMARK * mark)
{
    // Record current page and location.
    mark->page = pageLast;
    mark->nextFree = nextFree;
}

CStringW NRHEAP::GetDebugIdentifier() const
{
    return m_heapName;
}

size_t NRHEAP::CalcCommittedSize () const
{
    size_t iCommit = 0;
    for (NRPAGE *p = pageList; p; p = p->next)
        iCommit += p->limitAvail - (BYTE *)p;

    return iCommit;
}

/*
 * Free all memory allocated after the mark.
 */
void NRHEAP::Free(NRMARK * mark)
{
    if (mark->page == NULL)
    {
        // Mark was before anything was allocated.
        FreeHeap();
    }
    else
    {
        if (g_MemSpectTrackingNRLS)
        {
            g_MemSpectArenaRelease(m_hMemSpectArenaHandle, mark->nextFree);
        }
        NRPAGE * page, *nextPage;

#ifdef DEBUG

        page = pageList;
        while (page != pageLast)
        {
            if (page == mark->page)
                break;
            page = page->next;
        }
        ASSERT(mark->page == page); // we should have found the page in the list
#endif

        ValidateHeap();

        // Free all pages after the new last one.
        for (page = mark->page->next; page != NULL; page = nextPage)
        {
            nextPage = page->next;

            DestroyDestructibleObjectsOnPage(page);

            m_heapPage.FreePages(entity, page, (BYTE *)page->limitAvail - (BYTE *)page);
        }

        DestroyDestructibleObjectsOnPageAfterAddress(mark->page, mark->nextFree);

        // Reset the last page and location.
        pageLast = mark->page;
        if (anyPageMarkedReadOnly)
        {
            SetPageWriteStatus(pageLast, true);
        }
        pageLast->next = NULL;
        nextFree = mark->nextFree;
        limitFree = pageLast->limitAvail;

#ifdef DEBUG
        // Free rest of page with junk
        memset(nextFree, 0xEE, limitFree - nextFree);
#endif //DEBUG

        MakeCurrentPageReadOnlyInternal();
    }
}

/*
 * Free the entire heap.
 */
void NRHEAP::FreeHeap()
{
    NRPAGE * page, *nextPage;

    ValidateHeap();

    // Free all the pages.
    for (page = pageList; page != NULL; page = nextPage)
    {
        nextPage = page->next;

        DestroyDestructibleObjectsOnPage(page);

        m_heapPage.FreePages(entity, page, (BYTE *)page->limitAvail - (BYTE *)page);
    }

    // Reset the allocator.
    nextFree = limitFree = NULL;
    pageList = pageLast = NULL;
    if (g_MemSpectTrackingNRLS)
    {
        g_MemSpectArenaRelease(m_hMemSpectArenaHandle, 0); // 0 means freeall
    }
}

#ifdef DEBUG 
/*
 * For DEBUG allocations, add information to an allocation to validate 
 * no buffer overruns.
 */
void * NRHEAP::DebugFill(void * p, size_t sz)
{
    size_t sizeUser = sz;                   // user space
    // integer overflow
    if (!(sizeUser < sizeUser + 1))
    {
        ASSERT(sizeUser < sizeUser + 1);
        return NULL;
    }

    if (p == NULL)
        return NULL;

    * (size_t *) p = sizeUser;  // record user size at beginning of block for traversal.
    p = (BYTE*)p + RoundUpAllocSize(sizeof(size_t));

    // Put sentinal bytes in.
    for (size_t i = sizeUser; i < RoundUpAllocSize(sizeUser + 1); ++i)
        * ((BYTE *)p + i) = DEBUGSENTINAL;

    return p;
}
#endif //DEBUG


/*
 * Validate that all the sentinal bytes in the heap 
 * are still intact.
 */
void NRHEAP::ValidateHeap()
{
#ifdef DEBUG
    if (!VSFSWITCH(gfNRHeapValidation))
        return ;

    NRPAGE * page;

    for (page = pageList; page != NULL; page = page->next)
    {
        // Validate this page.
        BYTE * p = page->firstAvail;
        for (;;)
        {
            if (p >= page->limitAvail)
                break;
            size_t sizeUser = *(size_t *)p;
#pragma warning (suppress : 4310) // cast truncates constant value
            if (sizeUser == (size_t)I64(0xCCCCCCCCCCCCCCCC) || sizeUser == (size_t)I64(0xEEEEEEEEEEEEEEEE))
                break;   // at the end of this page.

            // integer overflow
            if (!(sizeUser < sizeUser + 1))
            {
                ASSERT(sizeUser < sizeUser + 1);
                break;
            }

            p = (BYTE*)p + RoundUpAllocSize(sizeof(size_t));
            for (size_t i = sizeUser; i < RoundUpAllocSize(sizeUser + 1); ++i)
            {
                ASSERT(p[i] == DEBUGSENTINAL);
            }
            p += RoundUpAllocSize(sizeUser + 1);
        }
    }
#endif //DEBUG
}

void NRHEAP::DestroyDestructibleObjectsOnPage(NRPAGE* pPage)
{
    // Call the destructor on objects that have one.
    for (CAtlArrayEnum<IHasDestructor*> e(*(pPage->m_pDestructibleObjects)); e.MoveNext(); )
    {
        e.Current()->~IHasDestructor();
        e.RemoveCurrent();
    }

    // Destroy the array of destructible objects itself
    delete pPage->m_pDestructibleObjects;
}

void NRHEAP::DestroyDestructibleObjectsOnPageAfterAddress(NRPAGE* pPage, void* pAddress)
{
    // Call the destructor on objects that have one.
    for (CAtlArrayEnum<IHasDestructor*> e(*(pPage->m_pDestructibleObjects)); e.MoveNext(); )
    {
        if (e.Current() >= pAddress)
        {
            e.Current()->~IHasDestructor();
            e.RemoveCurrent();
        }
    }
}
