// ==++==
//
//   Copyright (c) Microsoft Corporation.  All rights reserved.
//
// ==--==
// ===========================================================================
// File: alloc.h
//
// ===========================================================================

#ifndef __alloc_h__
#define __alloc_h__

#include "MemHeap.h"
#include "PageHeap.h"
#include "locks.h"
#include "iheapallocator.h"
#include "PageProtect.h"
#include "IHasDestructor.h"







/* No-release allocator. Allocates very fast, cannot
* be released in general, except in a LIFO fashion
* to a particular mark.
*/

// Holds a page of memory being used by the NRHEAP.
struct NRPAGE
{
    NRPAGE * next;      // next page in use.
    BYTE * firstAvail;  // first available byte for allocation.
    BYTE * limitAvail;  // limit for allocation.
    CAtlArray<IHasDestructor*>* m_pDestructibleObjects;
};

class NRMARK
{
private:
    friend class NRHEAP;

    NRPAGE * page;      // page.
    BYTE * nextFree;    // first free location within the page.
};

class NRHEAP
{
public:

    class AllowingWrite
    {
    private:
        NRHEAP * heap;
        bool alreadyInAllowingWrite;
    public:
        AllowingWrite(NRHEAP * heap)
        {
            this->heap = heap;
            if (heap)
            {
                alreadyInAllowingWrite = heap->inAllowingWrite;
                heap->inAllowingWrite = true;
            }
            else
            {
                alreadyInAllowingWrite = false;
            }
        }
        ~AllowingWrite()
        {
            if (!alreadyInAllowingWrite && heap)
            {
                heap->inAllowingWrite = false;
            }
        }
        bool OwnsHeapWriteability()
        {
            return !alreadyInAllowingWrite;
        }
    };

#if defined(__GNUC__) && (__GNUC__ < 3)
    friend class NRHEAP::AllowingWrite;
#endif

    NRHEAP(PAGEHEAP& heap);
    NRHEAP(PAGEHEAP& heap, const CStringW& heapName);
    NRHEAP(PAGEHEAP& heap, const CStringW& heapName, ProtectedEntityFlagsEnum entity);

    ~NRHEAP();

    PAGEHEAP& GetPageHeap()
    {
        return m_heapPage;
    }

    void ForbidWrite(void * p, size_t sz)
    {
        VerifyHeapEntity();
        ForbidWriteInternal(p, sz);
    }
    void AllowWrite(void * p, size_t sz)
    {
        VerifyHeapEntity();
        AllowWriteInternal(p, sz);
    }

    void DisallowReadOnlyDirectives()
    {
        VerifyHeapEntity();
        DisallowReadOnlyDirectivesInternal();
    }
    void AllowReadOnlyDirectives()
    {
        VerifyHeapEntity();
        AllowReadOnlyDirectivesInternal();
    }
    bool AllowingReadOnlyDirectives()
    {
        VerifyHeapEntity();
        return AllowingReadOnlyDirectivesInternal();
    }
    void MakeCurrentPageReadOnly()
    {
        VerifyHeapEntity();
        MakeCurrentPageReadOnlyInternal();
    } // Calling Alloc makes it writeable until the next call to MakeReadOnly
    void MakeCurrentPageWriteable()
    {
        VerifyHeapEntity();
        MakeCurrentPageWriteableInternal();
    } // Calling Alloc makes it writeable until the next call to MakeReadOnly
    void MakeAllHeapReadOnly()
    {
        VerifyHeapEntity();
        MakeAllHeapReadOnlyInternal();
    }
    void MakeAllHeapWriteable()
    {
        VerifyHeapEntity();
        MakeAllHeapWriteableInternal();
    }

    bool IsAddressInCurrentPage (const void * addr) const;
    bool IsAddressInHeap (const void * addr) const
    {
        for (NRPAGE * page = pageList; page; page = page->next)
        {
            if (addr >= page && addr < page->limitAvail)
                return true;
        }
        return false;
    }

#ifdef DEBUG
    void * DebugFill(void * p, size_t sz);
    void * _Alloc(size_t sz, PCSTR pszFile, UINT iLine);
    void * _AllocZero(size_t sz, PCSTR pszFile, UINT iLine);
#else
    void * _Alloc(size_t sz);
    void * _AllocZero(size_t sz);
#endif

    void ValidateHeap();
    PWSTR AllocStr(PCWSTR str);
    void Mark(NRMARK * mark);
    void Free(NRMARK * mark);
    void FreeHeap();
    size_t CalcCommittedSize () const;
    CStringW GetDebugIdentifier() const;

    template <class T>
    T* NewDestructibleObject()
    {
        VcsAssertNoThrow();

#ifdef DEBUG
        T* pDestructibleObj = static_cast<T*>(_Alloc(sizeof(T), __FILE__, __LINE__));
#else
        T* pDestructibleObj = static_cast<T*>(_Alloc(sizeof(T)));
#endif

        if (pDestructibleObj == NULL)
        {
            return NULL;
        }

        // Undefine "new" to the actual C++ new operator
#pragma push_macro("new")
#undef new
    #pragma warning(push)
    #pragma warning(disable:4291)
        new(pDestructibleObj) T;    // vtable init
    #pragma warning(pop)
#pragma pop_macro("new")

        pageLast->m_pDestructibleObjects->Add(pDestructibleObj);

        return pDestructibleObj;
    }

private:
    void Init(const CStringW& heapName, ProtectedEntityFlagsEnum entity);
    void VerifyHeapEntity()
    {
        ASSERT(entity && L"Heap entity must be set before manipulating heap writeability");
    }
    void ForbidWriteInternal(void * p, size_t sz);
    void AllowWriteInternal(void * p, size_t sz);

    void DisallowReadOnlyDirectivesInternal()
    {
        allowReadOnlyDirectives = false;
    }
    void AllowReadOnlyDirectivesInternal()
    {
        allowReadOnlyDirectives = true;
    }
    bool AllowingReadOnlyDirectivesInternal()
    {
        return allowReadOnlyDirectives;
    }
    void MakeCurrentPageReadOnlyInternal(); // Calling Alloc makes it writeable until the next call to MakeReadOnly
    void MakeCurrentPageWriteableInternal(); // Calling Alloc makes it writeable until the next call to MakeReadOnly
    void MakeAllHeapReadOnlyInternal();
    void MakeAllHeapWriteableInternal();

    void SetPageWriteStatus(NRPAGE * page, bool writeable);
    void SetPageRegionWriteStatus(NRPAGE * first, NRPAGE * last, bool writeable);
    void SetAllPagesWriteStatus(bool writeable);

    BYTE * nextFree;                // location of free area
    BYTE * limitFree;               // just beyond end of free area in this page.

    NRPAGE * pageList;              // list of pages used by this allocator.
    NRPAGE * pageLast;              // last page in the list.

    // The source of pages for this heap.
    PAGEHEAP& m_heapPage;
    
    DWORD m_hMemSpectArenaHandle;
    
    void * AllocMore(size_t sz);
    NRPAGE * NewPage(size_t sz);

    ProtectedEntityFlagsEnum entity;
    CStringW m_heapName;
    bool allowReadOnlyDirectives;
    bool anyPageMarkedReadOnly;
    bool inAllowingWrite;

    void DestroyDestructibleObjectsOnPage(NRPAGE* pPage);
    void DestroyDestructibleObjectsOnPageAfterAddress(NRPAGE* pPage, void* pAddress);

#ifdef DEBUG
    inline size_t DebugSize (size_t sz) const
    {
        // integer overflow
        if (!(sz < sz + 1 + sizeof(size_t) + sizeof(void*) + sizeof(void*)))
        {
            ASSERT(sz < sz + 1 + sizeof(size_t) + sizeof(void*) + sizeof(void*));
            return 0;
        }
        return RoundUpAllocSize(sizeof(size_t)) + RoundUpAllocSize(sz + 1); // int at beginning for size, plus 1 (or more) bytes of sentinel.
    }
    static const BYTE DEBUGSENTINAL = 0xAE;  // put at end of block to detect overrun.
#endif //DEBUG
};

class NRHeapMarker
{
public:
    NRHeapMarker(NRHEAP * h)
    {
        heap = h;
        heap->Mark(&mark);
    }
    ~NRHeapMarker()
    {
        heap->Free(&mark);
    }
private:
    NRHEAP * heap;
    NRMARK mark;
};

class NRHeapWriteMaker
{
public:
    NRHeapWriteMaker(NRHEAP * heap) : allowingWrite(heap)
    {
        this->heap = heap;
        if (heap)
        {
            heap->MakeAllHeapWriteable();
        }
    }
    ~NRHeapWriteMaker()
    {
        if (heap && allowingWrite.OwnsHeapWriteability())
        {
            heap->AllowReadOnlyDirectives();
            heap->MakeAllHeapReadOnly();
        }
    }
private:
    NRHEAP * heap;
    NRHEAP::AllowingWrite allowingWrite;
};

class NRHeapWriteAllower
{
public:
    NRHeapWriteAllower(NRHEAP * heap) : allowingWrite(heap)
    {
        this->heap = heap;
        if (heap)
        {
            heap->DisallowReadOnlyDirectives();
        }
    }
    ~NRHeapWriteAllower()
    {
        if (heap && allowingWrite.OwnsHeapWriteability())
        {
            heap->AllowReadOnlyDirectives();
            heap->MakeAllHeapReadOnly();
        }
    }
private:
    NRHEAP * heap;
    NRHEAP::AllowingWrite allowingWrite;
};

class NRHEAPWRAPPER :
            public IHeapAllocator
{
    NRHEAP *heap;

public:
    NRHEAPWRAPPER() :
            heap(NULL)
    {}

    NRHEAPWRAPPER(NRHEAP *h) :
            heap(h)
    {}

    ~NRHEAPWRAPPER()
    {}

    NRHEAP * Init(NRHEAP *h)
    {
        heap = h;
        return h;
    }
    void Delete()
    {
        if (heap)
            delete heap;
    }
    NRHEAP *operator->() const
    {
        return heap;
    }
    operator NRHEAP*() const
    {
        return heap;
    }

    void * Allocate(size_t sz)
    {
        return heap->Alloc(sz);
    }
};

#endif // __alloc_h__
