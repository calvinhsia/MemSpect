#if MSDEBUG
#define _ITERATOR_DEBUG_LEVEL 1
#else
#define _ITERATOR_DEBUG_LEVEL 0
#endif MSDEBUG
//#define _SILENCE_STDEXT_HASH_DEPRECATION_WARNINGS
#include "map"
#include "set"
#include "list"
#include "vector"
#include "queue"
#include "algorithm"
#include "unordered_map"
#include "unordered_set"
#include "memory"
// STL functions for VSAssert (calvinh 03/2010)
using namespace std;
using namespace stdext;


// <STL stuff>




// <MySTLAlloc>
// an allocator that uses a particular heap: either a VSAssert style wrapped heap or the VSAssert private debug heap
#ifdef false
template <class T>
class MySTLAlloc
{
public:
    typedef T          value_type;
    typedef size_t     size_type;
    typedef ptrdiff_t  difference_type;

    typedef T*         pointer;
    typedef const T*   const_pointer;

    typedef T&         reference;
    typedef const T&   const_reference;

    MySTLAlloc(HANDLE hHeap)
    {
        m_hHeap = hHeap;
    }


    MySTLAlloc(const MySTLAlloc &obj) // copy constructor
    {
        m_hHeap = obj.m_hHeap;
    }
    HANDLE m_hHeap; // a heap to use to allocate our stuff. If 0, use VSAssert private debug heap
private:
    void operator =(const MySTLAlloc &);
public:

    template <class _Other>
    MySTLAlloc(const MySTLAlloc<_Other> &other)
    {
        m_hHeap = other.m_hHeap;
    }

    ~MySTLAlloc()
    {
    }


    template <class U>
    struct rebind
    {
        typedef MySTLAlloc<U> other;
    };


    pointer
        address(reference r) const
    {
        return &r;
    }

    const_pointer
        address(const_reference r) const
    {
        return &r;
    }

    pointer
        allocate(size_type n, const void* /*hint*/ = 0)
    {
        pointer p;
        unsigned nSize = n * sizeof(T);

        if (m_hHeap)
        {
            p = (pointer)VsDebugAllocInternal(m_hHeap, 0, nSize);
        }
        else
        {
            p = (pointer)DebugAlloc(nSize);
        }
        if (p == 0)
        {
            VSASSERTF((false, "MyStlAlloc failed to allocate: out of memmory allocating %d (%x).\n Try reducing stack size limit. For 32 bit proc, try http://blogs.msdn.com/b/calvin_hsia/archive/2010/09/27/10068359.aspx ", nSize, nSize));
        }
        return p;
    }

    void
        deallocate(pointer p, size_type /*n*/)
    {
        if (m_hHeap)
        {
            VsDebugFreeInternal(m_hHeap, p);
        }
        else
        {
            DebugFree(p);
        }
    }

    void
        construct(pointer p, const T& val)
    {
        new (p) T(val);
    }

    void
        destroy(pointer p)
    {
        p->~T();
    }

    size_type
        max_size() const
    {
        return ULONG_MAX / sizeof(T);
    }

};


template <class T>
bool
operator==(const MySTLAlloc<T>& left, const MySTLAlloc<T>& right)
{
    if (left.m_hHeap == right.m_hHeap)
    {
        return true;
    }
    return false;
}

struct TrkBlock; // forward declare

template <class T>
bool
operator!=(const MySTLAlloc<T>& left, const MySTLAlloc<T>& right)
{
    if (left.m_hHeap != right.m_hHeap)
    {
        return true;
    }
    return false;
}
#endif //false

template <class T>
struct MySTLAlloc
{
	typedef T value_type;
	MySTLAlloc(HANDLE hHeap) 
	{
		m_hHeap = hHeap;
	}
	// A converting copy constructor:
	template<class U> MySTLAlloc(const MySTLAlloc<U>& other) 
	{
		m_hHeap = other.m_hHeap;
	}
	template<class U> bool operator==(const MySTLAlloc<U>&) const 
	{
		return true;
	}
	template<class U> bool operator!=(const MySTLAlloc<U>&) const 
	{
		return false;
	}
	T* allocate(const size_t n) const
	{
		if (n == 0)
		{
			return nullptr;
		}
		if (n > static_cast<size_t>(-1) / sizeof(T))
		{
			throw std::bad_array_new_length();
		}
		unsigned nSize = n * sizeof(T);
		void *pv;
        if (m_hHeap)
        {
            pv = VsDebugAllocInternal(m_hHeap, 0, nSize);
        }
        else
        {
            pv = DebugAlloc(nSize);
        }
        if (pv == 0)
        {
            VSASSERTF((false, "MyStlAlloc failed to allocate: out of memmory allocating %d (%x).\n Try reducing stack size limit. For 32 bit proc, try http://blogs.msdn.com/b/calvin_hsia/archive/2010/09/27/10068359.aspx ", nSize, nSize));
        }
		return static_cast<T*>(pv);
	}
	void deallocate(T* const p, size_t) const 
	{
		if (m_hHeap)
		{
			VsDebugFreeInternal(m_hHeap, p);
		}
		else
		{
			DebugFree(p);
		}
	}
	HANDLE m_hHeap; // a heap to use to allocate our stuff. If 0, use VSAssert private debug heap
};


/*
    CMyStlWrap wraps various STL containers to use various allocators, like the debug heap or a private heap
    */
template <class StlType, class Alloc = MySTLAlloc<StlType>, class Traits = less<ULONG>>
class CMyStlWrap
{
    //    typedef CMyStlWrap<StlType, Alloc, Traits > _me ;
public:
    CMyStlWrap(Alloc allocatorinstance)
    {
        m_pStlTypeMem = DebugAlloc(sizeof(StlType));
        m_pStlType = new(m_pStlTypeMem) StlType(allocatorinstance);
    }
    CMyStlWrap(Traits traits, Alloc allocatorinstance)
    {
        m_pStlTypeMem = DebugAlloc(sizeof(StlType));
        m_pStlType = new(m_pStlTypeMem) StlType(traits, allocatorinstance);
    }
    void freemem() // must call this when done. No destructor
    {
        m_pStlType->clear();
        m_pStlType->~StlType();  // must invoke dtor manually
        DebugFree(m_pStlTypeMem);
        DebugFree(this); // placement new
    }
    StlType *m_pStlType;
    StlType *operator->(void)
    {
        asdf anybody home ?
            return m_pStlType;
    }
    //_me operator&()
    //{
    //    return *this;
    //}
private:
    LPVOID m_pStlTypeMem; // pointer to vector
};


