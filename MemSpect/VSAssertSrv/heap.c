/*++    Copyright (c) 1989  Microsoft Corporation    Module Name:        heap.c    Abstract:        This module implements a heap allocator.    Author:        Steve Wood (stevewo) 20-Sep-1989 (Adapted from URTL\alloc.c)        Adrian Marinescu (adrmarin) - added hash for large blocks, anti-fragmentation          policies, new front-end heap (LFH), lower overhead on 64 bit platforms,          security mitigations.        Adrian Marinescu (2004) - Redesign the segments and committ/decommitt management:              - Remove the exponential trend in reserving the memory for segments                  (to reduce the VA pressure on large server applications, eliminate                  scalability issues for large segments)              - Fix the old fundamental design problem when the heap can corrupt                itself at low memory condition when fails to allocate memory for UCRs.              - Constant-time allocation: eliminate the need to walk last committed                region of a segment              - Simplify the code by removing special tests for last entries and                updating a single entry cache              - Enable reducing the overhead to 8 bytes for 64bits platforms on                default heap              - Remove the perf counters used to sample the time per ops (since                the new design guarantees constant time cost and these are obsolete)              - Secure additional list removal operations              - More cleanup, reduce complexity and code paths (like RtlpAllocateHeapSlowly,                RtlpFreeHeapSlowly)    Revision History:    --*/    
#include <ntos.h>  
#include <ntrtl.h>  
#include <nturtl.h>  
#include "ntrtlp.h"  
#include "heap.h"  
#include "heappriv.h"  
#include "ntetw.h"  
#include "wmiguid.h"  
  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
ULONG RtlpDisableHeapLookaside = 0;  
BOOLEAN RtlpOptimizeForFootprint = FALSE;  
SIZE_T RtlpLargestLfhBlock = HEAP_DEFAULT_LARGEST_LFH_BLOCK;  
  
#endif  
  
  
#if defined(ALLOC_DATA_PRAGMA) && defined(NTOS_KERNEL_RUNTIME)  
#pragma const_seg("PAGECONST")  
#endif  
const UCHAR CheckHeapFillPattern[ CHECK_HEAP_TAIL_SIZE ] = {  
    CHECK_HEAP_TAIL_FILL,  
    CHECK_HEAP_TAIL_FILL,  
    CHECK_HEAP_TAIL_FILL,  
    CHECK_HEAP_TAIL_FILL,  
    CHECK_HEAP_TAIL_FILL,  
    CHECK_HEAP_TAIL_FILL,  
    CHECK_HEAP_TAIL_FILL,  
#ifdef _WIN64  
    CHECK_HEAP_TAIL_FILL,  
    CHECK_HEAP_TAIL_FILL,  
    CHECK_HEAP_TAIL_FILL,  
    CHECK_HEAP_TAIL_FILL,  
    CHECK_HEAP_TAIL_FILL,  
    CHECK_HEAP_TAIL_FILL,  
    CHECK_HEAP_TAIL_FILL,  
    CHECK_HEAP_TAIL_FILL,  
#endif  
    CHECK_HEAP_TAIL_FILL  
};  
  
  
//  //  Local procedure prototypes  //    
VOID  
FASTCALL  
RtlpInsertUCRBlock (  
    __in PHEAP Heap,  
    __inout PHEAP_UCR_DESCRIPTOR UCRBlock  
    );  
  
VOID  
FASTCALL  
RtlpRemoveUCRBlock (  
    __in PHEAP Heap,  
    __inout PHEAP_UCR_DESCRIPTOR UCRBlock  
    );  
  
PHEAP_UCR_DESCRIPTOR  
FASTCALL  
RtlpSearchUCRBlock (  
    __in PHEAP_SEGMENT Segment,  
    __in PVOID EndAddress  
    );  
  
VOID  
RtlpCreateUCREntry (  
    __inout PHEAP Heap,  
    __inout PHEAP_SEGMENT Segment,  
    __in PHEAP_UCR_DESCRIPTOR NewUCRBlock,  
    __in SIZE_T Size,  
    __in PHEAP_FREE_ENTRY LastKnownEntry,  
    __out PSIZE_T FreeSize  
    );  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
VOID  
FASTCALL  
RtlpUpdateUCRIndexRemove (  
    __in PHEAP Heap,  
    __inout PHEAP_UCR_DESCRIPTOR UCREntry  
    );  
  
VOID  
FASTCALL  
RtlpUpdateUCRIndexInsert (  
    __in PHEAP Heap,  
    __inout PHEAP_UCR_DESCRIPTOR UCREntry  
    );  
  
#else  
  
#define RtlpUpdateUCRIndexRemove(H,U)  
#define RtlpUpdateUCRIndexInsert(H,U)  
  
#endif // NTOS_KERNEL_RUNTIME    
VOID  
RtlpInitializeUCRIndex(  
    __in PHEAP Heap  
    );  
  
PLIST_ENTRY  
FASTCALL  
RtlpFindUCREntry(  
    __in PHEAP Heap,  
    __in SIZE_T Size  
    );  
  
NTSTATUS  
RtlpDestroyHeapSegment (  
    __in PHEAP_SEGMENT Segment  
    );  
  
PHEAP_FREE_ENTRY  
RtlpExtendHeap (  
    __in PHEAP Heap,  
    __in SIZE_T AllocationSize  
    );  
  
PVOID  
FASTCALL  
RtlpAllocateHeap (  
    __in PHEAP Heap,  
    __in ULONG Flags,  
    __in SIZE_T Size,  
    __in SIZE_T AllocationSize,  
    __in_opt PHEAP_LOOKUP_ENTRY LookupEntry,  
    __out PULONG TracingSource  
    );  
  
LOGICAL  
FASTCALL  
RtlpFreeHeap (  
    __in PHEAP Heap,  
    __in ULONG Flags,  
    __in_opt PHEAP_ENTRY BusyBlock,  
    __in PVOID BaseAddress  
    );  
  
#if defined(ALLOC_PRAGMA) && defined(NTOS_KERNEL_RUNTIME)  
  
#pragma alloc_text(PAGE, RtlCreateHeap)  
#pragma alloc_text(PAGE, RtlDestroyHeap)  
#pragma alloc_text(PAGE, RtlZeroHeap)  
#pragma alloc_text(PAGE, RtlpDestroyHeapSegment)  
  
#endif // ALLOC_PRAGMA    
//  //  Public functions implementation  //    
  
_Must_inspect_result_  
PVOID  
RtlCreateHeap (  
    __in ULONG Flags,  
    __in_opt PVOID HeapBase,  
    __in_opt SIZE_T ReserveSize,  
    __in_opt SIZE_T CommitSize,  
    __in_opt PVOID Lock,  
    __in_opt PRTL_HEAP_PARAMETERS Parameters  
    )  
  
/*++    Routine Description:        This routine initializes a heap.    Arguments:        Flags - Specifies optional attributes of the heap.            Valid Flags Values:            HEAP_NO_SERIALIZE - if set, then allocations and deallocations on                           this heap are NOT synchronized by these routines.            HEAP_GROWABLE - if set, then the heap is a "sparse" heap where                          memory is committed only as necessary instead of                          being preallocated.        HeapBase - if not NULL, this specifies the base address for memory          to use as the heap.  If NULL, memory is allocated by these routines.        ReserveSize - if not zero, this specifies the amount of virtual address          space to reserve for the heap.        CommitSize - if not zero, this specifies the amount of virtual address          space to commit for the heap.  Must be less than ReserveSize.  If          zero, then defaults to one page.        Lock - if not NULL, this parameter points to the resource lock to          use.  Only valid if HEAP_NO_SERIALIZE is NOT set.        Parameters - optional heap parameters.    Return Value:        PVOID - a pointer to be used in accessing the created heap.    --*/    
{  
    ULONG_PTR ArraySize;  
    PVOID CommittedBase;  
    PLIST_ENTRY FreeListHead;  
    SIZE_T FrontEndArraySize;  
    ULONG GlobalFlag = RtlGetNtGlobalFlags();  
    PHEAP Heap = NULL;  
    static ULONG_PTR HighestUserAddress = 0;  
    PHEAP_LIST_LOOKUP ListLookup;  
    SIZE_T LookupSize;  
    MEMORY_BASIC_INFORMATION MemoryInformation;  
    SIZE_T MaximumHeapBlockSize;  
    PVOID NextHeapHeaderAddress;  
    ULONG n;  
    SIZE_T PaddingSize;  
    PHEAP_SEGMENT Segment = NULL;  
    ULONG SegmentFlags = 0;  
    ULONG SizeOfHeapHeader;  
    NTSTATUS Status;  
    SYSTEM_BASIC_INFORMATION SystemInformation;  
    RTL_HEAP_PARAMETERS TempParameters;  
    PVOID UnCommittedBase;  
    ULONG InitialCountOfUnusedUnCommittedRanges;  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
    PPEB Peb;  
  
#endif // NTOS_KERNEL_RUNTIME    
    RTL_PAGED_CODE();  
  
    //      //  Check if we should be using the page heap code.  If not then turn      //  off any of the page heap flags before going on      //    
#ifdef DEBUG_PAGE_HEAP  
  
    if ( RtlpDebugPageHeap && ( HeapBase == NULL ) && ( Lock == NULL )) {  
  
        PVOID PageHeap;  
  
        PageHeap = RtlpDebugPageHeapCreate(  
  
            Flags,  
            HeapBase,  
            ReserveSize,  
            CommitSize,  
            Lock,  
            Parameters );  
  
        if (PageHeap != NULL) {  
  
            return PageHeap;  
        }  
  
        //          // A `-1' value signals a recursive call from page heap          // manager. We set this to null and continue creating          // a normal heap. This small hack is required so that we          // minimize the dependencies between the normal and the page          // heap manager.          //    
        if ((SIZE_T)Parameters == (SIZE_T)-1) {  
  
            Parameters = NULL;  
  
        } else {  
  
            //              // If we get here it means page heap create returned null due to              // a real error (out of memory or fault injection) and we have              // to fail the call.              //    
            return NULL;  
        }  
    }  
  
    Flags &= ~( HEAP_PROTECTION_ENABLED |  
                HEAP_BREAK_WHEN_OUT_OF_VM |  
                HEAP_NO_ALIGNMENT );  
  
#endif // DEBUG_PAGE_HEAP    
    //      //  If the caller does not want to skip heap validiation checks then we      //  need to validate the rest of the flags but simply masking out only      //  those flags that want on a create heap call      //    
    if (!(Flags & HEAP_SKIP_VALIDATION_CHECKS)) {  
  
        HEAP_ERROR(HEAP_ERROR_LEVEL_VALIDATION_FAILURE,  
                   !(Flags & ~HEAP_CREATE_VALID_MASK),  
                   (ULONG_PTR) Flags);  
  
        if (Flags & ~HEAP_CREATE_VALID_MASK) {  
  
            Flags &= HEAP_CREATE_VALID_MASK;  
        }  
    }  
  
    //      //  The maximum heap block size is really 0x7f000 which is 0x80000 minus a      //  page.  Maximum block size is 0xfe00 and granularity shift is 3.      //    
    MaximumHeapBlockSize = HEAP_MAXIMUM_BLOCK_SIZE << HEAP_GRANULARITY_SHIFT;  
  
    //      //  Assume we're going to be successful until we're shown otherwise      //    
    Status = STATUS_SUCCESS;  
  
    //      //  This part of the routine builds up local variable containing all the      //  parameters used to initialize the heap.  First thing we do is zero      //  it out.      //    
    RtlZeroMemory( &TempParameters, sizeof( TempParameters ) );  
  
    //      //  If our caller supplied the optional heap parameters then we'll      //  make sure the size is good and copy over them over to our      //  local copy      //    
    if (ARGUMENT_PRESENT( Parameters )) {  
  
        try {  
  
            if (Parameters->Length == sizeof( *Parameters )) {  
  
                RtlCopyMemory( &TempParameters, Parameters, sizeof( *Parameters ) );  
            }  
  
        } except( RtlpHeapExceptionFilter(GetExceptionCode()) ) {  
  
            Status = GetExceptionCode();  
        }  
  
        if (!NT_SUCCESS( Status )) {  
  
            return NULL;  
        }  
    }  
  
    //      //  Set the parameter block to the local copy      //    
    Parameters = &TempParameters;  
  
    //      //  If nt global flags tells us to always do tail or free checking      //  or to disable coalescing then force those bits set in the user      //  specified flags      //    
    if (GlobalFlag & FLG_HEAP_ENABLE_TAIL_CHECK) {  
  
        Flags |= HEAP_TAIL_CHECKING_ENABLED;  
    }  
  
    if (GlobalFlag & FLG_HEAP_ENABLE_FREE_CHECK) {  
  
        Flags |= HEAP_FREE_CHECKING_ENABLED;  
    }  
  
    if (GlobalFlag & FLG_HEAP_DISABLE_COALESCING) {  
  
        Flags |= HEAP_DISABLE_COALESCE_ON_FREE;  
    }  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
    //      //  In the non kernel case we also check if we should      //  validate parameters, validate all, or do stack backtraces      //    
    Peb = NtCurrentPeb();  
  
    if (GlobalFlag & FLG_HEAP_VALIDATE_PARAMETERS) {  
  
        Flags |= HEAP_VALIDATE_PARAMETERS_ENABLED;  
    }  
  
    if (GlobalFlag & FLG_HEAP_VALIDATE_ALL) {  
  
        Flags |= HEAP_VALIDATE_ALL_ENABLED;  
    }  
  
    if (GlobalFlag & FLG_USER_STACK_TRACE_DB) {  
  
        Flags |= HEAP_CAPTURE_STACK_BACKTRACES;  
    }  
  
    //      //  Also in the non kernel case the PEB will have some state      //  variables that we need to set if the user hasn't specified      //  otherwise      //    
    if (Parameters->SegmentReserve == 0) {  
  
        Parameters->SegmentReserve = Peb->HeapSegmentReserve;  
    }  
  
    if (Parameters->SegmentCommit == 0) {  
  
        Parameters->SegmentCommit = Peb->HeapSegmentCommit;  
    }  
  
    if (Parameters->DeCommitFreeBlockThreshold == 0) {  
  
        Parameters->DeCommitFreeBlockThreshold = Peb->HeapDeCommitFreeBlockThreshold;  
    }  
  
    if (Parameters->DeCommitTotalFreeThreshold == 0) {  
  
        Parameters->DeCommitTotalFreeThreshold = Peb->HeapDeCommitTotalFreeThreshold;  
    }  
  
#else // NTOS_KERNEL_RUNTIME    
    //      //  In the kernel case Mm has some global variables that we set      //  into the paramters if the user hasn't specified otherwise      //    
    if (Parameters->SegmentReserve == 0) {  
  
        Parameters->SegmentReserve = MmHeapSegmentReserve;  
    }  
  
    if (Parameters->SegmentCommit == 0) {  
  
        Parameters->SegmentCommit = MmHeapSegmentCommit;  
    }  
  
    if (Parameters->DeCommitFreeBlockThreshold == 0) {  
  
        Parameters->DeCommitFreeBlockThreshold = MmHeapDeCommitFreeBlockThreshold;  
    }  
  
    if (Parameters->DeCommitTotalFreeThreshold == 0) {  
  
        Parameters->DeCommitTotalFreeThreshold = MmHeapDeCommitTotalFreeThreshold;  
    }  
  
#endif // NTOS_KERNEL_RUNTIME    
    //      //  Get the highest user address      //    
    if (HighestUserAddress == 0) {  
  
        if (!NT_SUCCESS(ZwQuerySystemInformation(SystemBasicInformation,  
                                                 &SystemInformation,  
                                                 sizeof(SystemInformation),  
                                                 NULL))) {  
            return NULL;  
        }  
  
        HighestUserAddress = SystemInformation.MaximumUserModeAddress;  
    }  
  
    //      //  If the user hasn't said what the largest allocation size is then      //  we should compute it as the difference between the highest and lowest      //  address less one page      //    
    if (Parameters->MaximumAllocationSize == 0) {  
  
        Parameters->MaximumAllocationSize = (HighestUserAddress -  
                                             (ULONG_PTR)MM_LOWEST_USER_ADDRESS -  
                                             PAGE_SIZE );  
    }  
  
    //      //  Set the virtual memory threshold to be non zero and not more than the      //  maximum heap block size of 0x7f000.  If the user specified one that is      //  too large we automatically and silently drop it down.      //    
    if ((Parameters->VirtualMemoryThreshold == 0) ||  
        (Parameters->VirtualMemoryThreshold > MaximumHeapBlockSize)) {  
  
        Parameters->VirtualMemoryThreshold = MaximumHeapBlockSize;  
    }  
  
    //      //  The default commit size is MINIMUM_HEAP_COMMIT and the default      //  reserve size is 64 pages.      //    
    if (!ARGUMENT_PRESENT( CommitSize )) {  
  
        CommitSize = MINIMUM_HEAP_COMMIT;  
  
        if (!ARGUMENT_PRESENT( ReserveSize )) {  
  
            ReserveSize = 64 * CommitSize;  
  
        } else {  
  
            ReserveSize = ROUND_UP_TO_POWER2( ReserveSize,  
                                              MINIMUM_HEAP_COMMIT );  
        }  
  
    } else {  
  
        //          //  The heap actually uses space that is reserved and commited          //  to store internal data structures (the LOCK,          //  the HEAP_PSEUDO_TAG, etc.). These structures can be larger than          //  4K especially on a 64-bit build. So, make sure the commit          //  is at least 8K in length.          //    
        CommitSize = ROUND_UP_TO_POWER2(CommitSize, MINIMUM_HEAP_COMMIT);  
  
        if (!ARGUMENT_PRESENT( ReserveSize )) {  
  
            ReserveSize = ROUND_UP_TO_POWER2( CommitSize, 16 * PAGE_SIZE );  
  
        } else {  
  
            ReserveSize = ROUND_UP_TO_POWER2( ReserveSize,  
                                              MINIMUM_HEAP_COMMIT );  
  
            //              //  If the CommitSize is larger than the ReservedSize, adjust              //  it to the ReserveSize. Reserved size is already rounded up to              //  MINIMUM_HEAP_COMMIT.              //    
            if ( CommitSize > ReserveSize ) {  
  
                CommitSize = ReserveSize;  
            }  
        }  
    }  
  
    //      // If this heap is a growable heap and no heap base was specified, use one      // reserved page in its initial segment as a guard page. Expand the      // reserved size to accommodate this page if necessary.      //    
    if (((Flags & HEAP_GROWABLE) != 0) && (HeapBase == NULL)) {  
        PaddingSize = PAGE_SIZE;  
        SegmentFlags |= HEAP_SEGMENT_PADDING_PRESENT;  
        if ((ReserveSize - PAGE_SIZE) < CommitSize) {  
            ReserveSize += PaddingSize;  
  
            //              // Round the reserve size up to the memory manager's allocation              // granularity to avoid wasting space.              //    
            ReserveSize = ROUND_UP_TO_POWER2(ReserveSize, HEAP_MM_GRANULARITY);  
        }  
  
    } else {  
        PaddingSize = 0;  
    }  
  
    if ((CommitSize == 0) || (ReserveSize == 0)) {  
        return NULL;  
    }  
  
    HEAPASSERT(ReserveSize >= CommitSize);  
    HEAPASSERT(CommitSize >= PAGE_SIZE);  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
    //      //  In the non kernel case check if we are creating a debug heap      //  the test checks that skip validation checks is false.      //    
    if (DEBUG_HEAP( Flags )) {  
  
        return RtlDebugCreateHeap( Flags,  
                                   HeapBase,  
                                   ReserveSize,  
                                   CommitSize,  
                                   Lock,  
                                   Parameters );  
    }  
  
#endif // NTOS_KERNEL_RUNTIME    
    //      //  Compute the size of the heap which will be the      //  heap struct itself and if we are to serialize with      //  out own lock then add room for the lock.  If the      //  user did not supply the lock then set the lock      //  variable to -1.      //    
    SizeOfHeapHeader = sizeof( HEAP );  
  
    if (!(Flags & HEAP_NO_SERIALIZE)) {  
  
        if (ARGUMENT_PRESENT( Lock )) {  
  
            Flags |= HEAP_LOCK_USER_ALLOCATED;  
  
        } else {  
  
            SizeOfHeapHeader += sizeof( HEAP_LOCK );  
            Lock = (PHEAP_LOCK)-1;  
        }  
  
    } else if (ARGUMENT_PRESENT( Lock )) {  
  
        //          //  In this error case the call said not to serialize but also fed us          //  a lock          //    
        return NULL;  
    }  
  
    //      //  See if caller allocate the space for the heap.      //    
    if (ARGUMENT_PRESENT( HeapBase )) {  
  
        //          //  The call specified a heap base now check if there is          //  a caller supplied commit routine          //    
        if (Parameters->CommitRoutine != NULL) {  
  
            //              //  The caller specified a commit routine so the caller              //  also needs to have given us certain parameters and make              //  sure the heap is not growable.  Otherwise it is an error              //    
            if ((Parameters->InitialCommit == 0) ||  
                (Parameters->InitialReserve == 0) ||  
                (Parameters->InitialCommit > Parameters->InitialReserve) ||  
                (Flags & HEAP_GROWABLE)) {  
  
                return NULL;  
            }  
  
            //              //  Set the commited base and the uncommited base to the              //  proper pointers within the heap.              //    
            CommittedBase = HeapBase;  
            UnCommittedBase = (PCHAR)CommittedBase + Parameters->InitialCommit;  
            ReserveSize = Parameters->InitialReserve;  
  
            //              //  Zero out a page of the heap where our first part goes              //    
            RtlZeroMemory( CommittedBase, Parameters->InitialCommit );  
  
        } else {  
  
            //              //  The user gave us space but not commit routine              //  So query the base to get its size              //    
            Status = ZwQueryVirtualMemory( NtCurrentProcess(),  
                                           HeapBase,  
                                           MemoryBasicInformation,  
                                           &MemoryInformation,  
                                           sizeof( MemoryInformation ),  
                                           NULL );  
  
            if (!NT_SUCCESS( Status )) {  
  
                return NULL;  
            }  
  
            //              //  Make sure the user gave us a base address for this block              //  and that the memory is not free              //    
            if (MemoryInformation.BaseAddress != HeapBase) {  
  
                return NULL;  
            }  
  
            if (MemoryInformation.State == MEM_FREE) {  
  
                return NULL;  
            }  
  
            //              //  Set our commit base to the start of the range              //    
            CommittedBase = MemoryInformation.BaseAddress;  
  
            //              //  If the memory is committed then              //  we can zero out a page worth              //    
            if (MemoryInformation.State == MEM_COMMIT) {  
  
                RtlZeroMemory( CommittedBase, PAGE_SIZE );  
  
                //                  //  Set the commit size and uncommitted base according                  //  to the start of the vm                  //    
                CommitSize = MemoryInformation.RegionSize;  
                UnCommittedBase = (PCHAR)CommittedBase + CommitSize;  
  
                //                  //  Find out the uncommited base is reserved and if so                  //  the update the reserve size accordingly.                  //    
                Status = ZwQueryVirtualMemory( NtCurrentProcess(),  
                                               UnCommittedBase,  
                                               MemoryBasicInformation,  
                                               &MemoryInformation,  
                                               sizeof( MemoryInformation ),  
                                               NULL );  
  
                ReserveSize = CommitSize;  
  
                if ((NT_SUCCESS( Status )) &&  
                    (MemoryInformation.State == MEM_RESERVE)) {  
  
                    ReserveSize += MemoryInformation.RegionSize;  
                }  
  
            } else {  
  
                //                  //  The memory the user gave us is not committed so dummy                  //  up these small numbers                  //    
                ReserveSize = MemoryInformation.RegionSize;  
                CommitSize = MINIMUM_HEAP_COMMIT;  
                UnCommittedBase = CommittedBase;  
            }  
        }  
  
        //          //  This user gave us a base and we've just taken care of the committed          //  bookkeeping.  So mark this segment as user supplied and set the          //  heap          //    
        SegmentFlags |= HEAP_SEGMENT_USER_ALLOCATED;  
        Heap = (PHEAP)HeapBase;  
  
    } else {  
  
        PVOID RebaseTempBuffer = NULL;  
        SIZE_T TempRebaseSize;  
        SIZE_T ExtraSize = 0;  
  
#if DBG  
        SIZE_T RequestedReservedSize = ReserveSize;  
#endif  
  
        //          //  The user did not specify a heap base so we have to allocate the          //  vm here.  First make sure the user did not give us a commit routine          //    
        if (Parameters->CommitRoutine != NULL) {  
  
            return NULL;  
        }  
  
        ExtraSize = (SIZE_T)RtlpHeapGenerateRandomValue64() % HEAP_RANDOM_REBASE_VALUES;  
  
        //          //  Rebase within 1 MBytes range          //    
        ExtraSize *= HEAP_MM_GRANULARITY;  
        TempRebaseSize = ExtraSize + ReserveSize;  
  
        if (TempRebaseSize < ReserveSize) {  
  
            //              //  If any overflow occured above and we ended up with a smaller buffer              //  just allocate space for that buffer              //    
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
  
            //              //  Split the vad and release the lower memory              //    
            Status = RtlpHeapFreeVirtualMemory( NtCurrentProcess(),  
                                                (PVOID *)&RebaseTempBuffer,  
                                                &ExtraSize,  
                                                MEM_RELEASE );  
  
            NT_VERIFY(NT_SUCCESS(Status));  
  
            Heap = (PHEAP)((ULONG_PTR)RebaseTempBuffer + ExtraSize);  
            ReserveSize = TempRebaseSize - ExtraSize;  
        }  
  
        HEAPASSERT( ReserveSize >= RequestedReservedSize );  
  
        //          //  Set the committed and uncommitted base to be the same the following          //  code will actually commit the page for us          //    
        CommittedBase = Heap;  
        UnCommittedBase = Heap;  
    }  
  
    //      //  At this point we have a heap pointer, committed base, uncommitted base,      //  segment flags, commit size, and reserve size.  If the committed and      //  uncommited base are the same then we need to commit the amount      //  specified by the commit size      //    
    if (CommittedBase == UnCommittedBase) {  
  
        Status = ZwAllocateVirtualMemory( NtCurrentProcess(),  
                                          (PVOID *)&CommittedBase,  
                                          0,  
                                          &CommitSize,  
                                          MEM_COMMIT,  
                                          RtlpGetHeapProtection(Flags) );  
  
        //          //  In the non successful case we need to back out any vm reservation          //  we did earlier          //    
        if (!NT_SUCCESS( Status )) {  
  
            if (!ARGUMENT_PRESENT(HeapBase)) {  
  
                //                  //  Return the reserved virtual address space.                  //    
                Status = RtlpHeapFreeVirtualMemory(NtCurrentProcess(),  
                                                   (PVOID *)&Heap,  
                                                   &ReserveSize,  
                                                   MEM_RELEASE );  
  
                NT_VERIFY(NT_SUCCESS(Status));  
            }  
  
            return NULL;  
        }  
  
        if (IS_HEAP_LOGGING_ENABLED()) {  
            RtlpLogHeapCommit( Heap,  
                               CommittedBase,  
                               CommitSize,  
                               HEAP_LOG_CREATE_HEAP);  
  
        }  
  
        //          //  The new uncommitted base is not adjusted above what we just          //  committed          //    
        UnCommittedBase = (PVOID)((PCHAR)UnCommittedBase + CommitSize);  
    }  
  
    //      //  At this point we have memory for the start of the heap committed and      //  ready to be initialized.  So now we need initialize the heap      //    
    //      //  Calculate the end of the heap header and make room for 8 uncommitted      //  range structures.  Once we have the room for them then chain them      //  together and null terminate the chain      //    
    NextHeapHeaderAddress = Heap + 1;  
  
    //      //  Check if tagging is enabled in global flags.      //      //  If tagging is enabled then make room for 129 pseudo tag heap entry.      //  Which is one more than the number of free lists.  Also point the heap      //  header to this array of pseudo tags entries.      //    
    if (IS_HEAP_TAGGING_ENABLED()) {  
  
        Heap->PseudoTagEntries = (PHEAP_PSEUDO_TAG_ENTRY)ROUND_UP_TO_POWER2( NextHeapHeaderAddress,  
                                                                             sizeof( QUAD ) );  
  
        SizeOfHeapHeader += HEAP_NUMBER_OF_PSEUDO_TAG * sizeof( HEAP_PSEUDO_TAG_ENTRY );  
  
        //          //  Update the next address with the number of pseudotags          //  (The math is right here because  Heap->PseudoTagEntries is of          //  type PHEAP_PSEUDO_TAG_ENTRY)          //    
        NextHeapHeaderAddress = Heap->PseudoTagEntries + HEAP_NUMBER_OF_PSEUDO_TAG;  
  
        Flags |= HEAP_TAGGING_ENABLED;  
    }  
  
    //      //  Round the size of the heap header to the next 8 byte boundary      //    
    SizeOfHeapHeader = (ULONG) ROUND_UP_TO_POWER2( SizeOfHeapHeader,  
                                                   HEAP_GRANULARITY );  
  
    //      //  If the sizeof the heap header is larger than the native      //  page size, you have a problem. Further, if the CommitSize passed      //  in was smaller than the SizeOfHeapHeader, you may not even make it      //  this far before death...      //    
    //      //  Fill in the heap header fields      //    
    Heap->Entry.Size = (USHORT)(SizeOfHeapHeader >> HEAP_GRANULARITY_SHIFT);  
    Heap->Entry.Flags = HEAP_ENTRY_BUSY;  
    Heap->Entry.UnusedBytes = HEAP_BLOCK_STATE_NT_METADATA_BLOCK;  
  
    Heap->Signature = HEAP_SIGNATURE;  
  
    Heap->Flags = Flags & ~HEAP_SKIP_VALIDATION_CHECKS;  
    Heap->Interceptor = 0;  
  
    RtlZeroMemory(&Heap->Counters, sizeof(Heap->Counters));  
  
    RtlpCreateHeapEncoding( Heap );  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
    Heap->Counters.HeapPollInterval = HEAP_POLL_INTERVAL;  
    if (Heap->Flags & HEAP_CAPTURE_STACK_BACKTRACES) {  
  
        Heap->Interceptor = RtlpGetHeapInterceptorIndex(RtlpStackTraceDatabaseLogPrefix);  
  
        //          //  Clear the free checking to preserve the stack information on the          //  free blocks          //    
        Heap->Flags &= ~HEAP_FREE_CHECKING_ENABLED;  
    }  
  
#endif  
  
    Heap->ForceFlags = (Flags & (HEAP_NO_SERIALIZE |  
                                 HEAP_GENERATE_EXCEPTIONS |  
                                 HEAP_ZERO_MEMORY |  
                                 HEAP_REALLOC_IN_PLACE_ONLY |  
                                 HEAP_VALIDATE_PARAMETERS_ENABLED |  
                                 HEAP_VALIDATE_ALL_ENABLED |  
                                 HEAP_TAIL_CHECKING_ENABLED |  
                                 HEAP_CREATE_ALIGN_16 |  
                                 HEAP_FREE_CHECKING_ENABLED));  
  
    Heap->HeaderValidateLength = (USHORT)((PCHAR)NextHeapHeaderAddress - (PCHAR)Heap);  
    Heap->HeaderValidateCopy = NULL;  
  
    //      //  Initialize the free list to be all empty      //    
    InitializeListHead( &Heap->FreeLists );  
  
    //      //  Make it so that there a no big block allocations      //    
    InitializeListHead( &Heap->VirtualAllocdBlocks );  
    InitializeListHead( &Heap->SegmentList );  
    InitializeListHead( &Heap->UCRList );  
  
    //      //  Initialize the critical section that controls access to      //  the free list.  If the lock variable is -1 then the caller      //  did not supply a lock so we need to make room for one      //  and initialize it.      //    
    if (Lock == (PHEAP_LOCK)-1) {  
  
        Lock = (PHEAP_LOCK)NextHeapHeaderAddress;  
  
        Status = RtlInitializeLockRoutine( Lock );  
  
        if (!NT_SUCCESS( Status )) {  
  
            if (!ARGUMENT_PRESENT(HeapBase)) {  
  
                Status = RtlpHeapFreeVirtualMemory( NtCurrentProcess(),  
                                                    (PVOID *)&Heap,  
                                                    &ReserveSize,  
                                                    MEM_RELEASE );  
  
                NT_VERIFY(NT_SUCCESS(Status));  
            }  
  
            return NULL;  
        }  
  
        NextHeapHeaderAddress = (PHEAP_LOCK)Lock + 1;  
    }  
  
    Heap->LockVariable = Lock;  
  
    //      //  Disable the lock tests until we finish the heap creation      //  all these operations are done w/o helding the heap lock      //    
    Heap->CompatibilityFlags |= HEAP_COMPAT_DISABLE_LOCKING_TEST;  
  
    //      //  Create the free list index      //    
    ArraySize = HEAP_MAXIMUM_FREELISTS;  
  
    //      //  Max index needs to be multiple of 32. Adjust it in case it is not.      //    
    ArraySize = ROUND_UP_TO_POWER2( ArraySize, 32 );  
  
    //      // Determine the size of the list lookup structure.      //    
    LookupSize = sizeof(HEAP_LIST_LOOKUP) +  
                 (ArraySize * sizeof(PLIST_ENTRY)) +  
                 ArraySize / 8;  
  
    LookupSize = ROUND_UP_TO_POWER2(LookupSize, HEAP_GRANULARITY);  
  
    ListLookup = (PHEAP_LIST_LOOKUP)NextHeapHeaderAddress;  
  
    //      //  Point to the next available address, and adjust the size of the metadata      //  that needs to be preallocated      //    
    NextHeapHeaderAddress = (PVOID) ((ULONG_PTR)NextHeapHeaderAddress + LookupSize);  
    SizeOfHeapHeader += (ULONG)LookupSize;  
  
    //      //  Initialize the first segment for the heap      //    
    if (!RtlpInitializeHeapSegment( Heap,  
                                    (PHEAP_SEGMENT)Heap,  
                                    SizeOfHeapHeader,  
                                    0,  
                                    SegmentFlags,  
                                    CommittedBase,  
                                    UnCommittedBase,  
                                    (PCHAR)CommittedBase + ReserveSize - PaddingSize)) {  
  
        if (!(Heap->Flags & HEAP_LOCK_USER_ALLOCATED)) {  
            RtlDeleteLockRoutine( Lock );  
        }  
  
        if (!ARGUMENT_PRESENT(HeapBase)) {  
            Status = RtlpHeapFreeVirtualMemory(NtCurrentProcess(),  
                                               (PVOID *)&Heap,  
                                               &ReserveSize,  
                                               MEM_RELEASE );  
  
            NT_VERIFY(NT_SUCCESS(Status));  
        }  
  
        return NULL;  
    }  
  
    if (ARGUMENT_PRESENT( HeapBase )) {  
  
        //          //  The memory has not been committed by this code          //  We need to zero out the ListLookup          //    
        RtlZeroMemory(ListLookup, ArraySize);  
    }  
  
    //      //  Initialize the list index      //    
    ListLookup->ArraySize = (ULONG)ArraySize;  
  
    //      //  The ListEntries will be immediately after the index structure      //    
    ListLookup->ListsInUseUlong = (PULONG)(ListLookup + 1);  
    ListLookup->ListHead = &Heap->FreeLists;  
  
    //      //  The bitmap is placed after the array with hints to      //  free blocks      //    
    ListLookup->ListHints = (PLIST_ENTRY *)((PCHAR)ListLookup->ListsInUseUlong + (ArraySize / 8));  
  
    RtlpPopulateListIndex(Heap, ListLookup);  
  
    //      //  Fill in additional heap entry fields      //    
    Heap->ProcessHeapsListIndex = 0;  
    Heap->SegmentReserve = Parameters->SegmentReserve;  
    Heap->SegmentCommit = Parameters->SegmentCommit;  
    Heap->DeCommitFreeBlockThreshold = Parameters->DeCommitFreeBlockThreshold >> HEAP_GRANULARITY_SHIFT;  
    Heap->DeCommitTotalFreeThreshold = Parameters->DeCommitTotalFreeThreshold >> HEAP_GRANULARITY_SHIFT;  
    Heap->MaximumAllocationSize = Parameters->MaximumAllocationSize;  
  
    Heap->VirtualMemoryThreshold = (ULONG) (ROUND_UP_TO_POWER2( Parameters->VirtualMemoryThreshold,  
                                                       HEAP_GRANULARITY ) >> HEAP_GRANULARITY_SHIFT);  
  
    //      //  Set the encoded committ routine, if applicable      //    
    RtlpSetCommitRoutine(Heap, Parameters->CommitRoutine);  
  
    Heap->TuningParameters.CommittThresholdShift = 4;  //  alow 25% extra space      Heap->TuningParameters.MaxPreCommittThreshold = (HEAP_MAXIMUM_BLOCK_SIZE * 2) << HEAP_GRANULARITY_SHIFT;  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
    //      //  Set the flags for compatibility mode      //    
    if (RtlpDisableHeapLookaside & HEAP_COMPAT_DISABLE_LOOKASIDES) {  
  
        Heap->CompatibilityFlags = HEAP_COMPAT_INVALID_PTR_ACCESS;  
    }  
  
#endif // NTOS_KERNEL_RUNTIME    
    //      //  We either align the heap at 16 or 8 byte boundaries.  The AlignRound      //  and AlignMask are used to bring allocation sizes up to the next      //  boundary.  The align round includes the heap header and the optional      //  check tail size      //    
    if (Flags & HEAP_CREATE_ALIGN_16) {  
  
        Heap->AlignRound = 15 + sizeof( HEAP_ENTRY );  
        Heap->AlignMask = ~((ULONG_PTR)15);  
  
    } else {  
  
        Heap->AlignRound = HEAP_GRANULARITY - 1 + sizeof( HEAP_ENTRY );  
        Heap->AlignMask = ~((ULONG_PTR)HEAP_GRANULARITY - 1);  
    }  
  
    if (Heap->Flags & HEAP_TAIL_CHECKING_ENABLED) {  
  
        Heap->AlignRound += CHECK_HEAP_TAIL_SIZE;  
    }  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
    //      //  Initialize the heap lookaside lists.  This is only for the user mode      //  heap and the heap contains a pointer to the lookaside list array.      //  The array is sized the same as the dedicated free list.  First we      //  allocate space for the lookaside list and then we initialize each      //  lookaside list.      //      //  But the caller asked for no serialize or asked for non growable      //  heap then we won't enable the lookaside lists.      //    
    Heap->FrontEndHeap = NULL;  
    Heap->FrontHeapLockCount = 0;  
    Heap->FrontEndHeapType = 0;  
    Heap->RequestedFrontEndHeapType = 0;  
    Heap->UCRIndex = NULL;  
  
    if (((Flags & HEAP_NO_SERIALIZE) == 0) &&  
        ((Flags & HEAP_GROWABLE) != 0) &&  
        ((RtlpDisableHeapLookaside & HEAP_COMPAT_DISABLE_LOOKASIDES) == 0)) {  
  
        FrontEndArraySize = HEAP_MAXIMUM_FREELISTS * sizeof(HEAP_INDEX_USAGE_DATA);  
        FrontEndArraySize = ROUND_UP_TO_POWER2(FrontEndArraySize, HEAP_GRANULARITY);  
        Heap->FrontEndHeapUsageData =  
            RtlAllocateHeap(Heap,  
                            HEAP_ZERO_MEMORY | HEAP_NO_INTERCEPTORS | HEAP_NO_CACHE_BLOCK,  
                            FrontEndArraySize);  
  
        if (Heap->FrontEndHeapUsageData != NULL) {  
            ((PHEAP_ENTRY)Heap->FrontEndHeapUsageData - 1)->UnusedBytes =  
                HEAP_BLOCK_STATE_NT_METADATA_BLOCK;  
  
            Heap->FrontEndHeapMaximumIndex = HEAP_MAXIMUM_FREELISTS;  
  
        } else {  
            if (!(Heap->Flags & HEAP_LOCK_USER_ALLOCATED)) {  
                RtlDeleteLockRoutine(Lock);  
            }  
  
            if (!ARGUMENT_PRESENT(HeapBase)) {  
                Status = RtlpHeapFreeVirtualMemory(NtCurrentProcess(),  
                                                   (PVOID *)&Heap,  
                                                   &ReserveSize,  
                                                   MEM_RELEASE);  
  
                NT_VERIFY(NT_SUCCESS(Status));  
            }  
  
            return NULL;  
        }  
    }  
  
    //      //  In the non kernel case we need to add this heap to the processes heap      //  list      //    
    RtlpAddHeapToProcessList(Heap);  
  
#endif // NTOS_KERNEL_RUNTIME    
    if (IS_HEAP_LOGGING_ENABLED()) {  
        RtlpLogHeapCreateEvent(Heap,  
                               Flags,  
                               ReserveSize,  
                               CommitSize,  
                               (HANDLE)HEAP_LOGGER_ID);  
    }  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
    if (IS_HEAPSUMMARY_LOGGING_ENABLED()) {  
        RtlpLogHeapCreateEvent(Heap,  
                               Flags,  
                               ReserveSize,  
                               CommitSize,  
                               (HANDLE)HEAPSUMMARY_LOGGER_ID);  
    }  
  
#endif // NTOS_KERNEL_RUNTIME    
    if (IS_HEAP_RANGE_LOGGING_ENABLED()) {  
  
        RtlpHeapLogRangeCreate(Heap, ReserveSize, Flags);  
    }  
  
    Heap->CompatibilityFlags &= ~HEAP_COMPAT_DISABLE_LOCKING_TEST;  
  
    //      //  And return the fully initialized heap to our caller      //    
    return (PVOID)Heap;  
}  
  
PVOID  
RtlDestroyHeap (  
    __in __post_invalid PVOID HeapHandle  
    )  
  
/*++    Routine Description:        This routine is the opposite of Rtl Create Heap.  It tears down an      existing heap structure.    Arguments:        HeapHandle - Supplies a pointer to the heap being destroyed    Return Value:        PVOID - Returns null if the heap was destroyed completely and a          pointer back to the heap if for some reason the heap could          not be destroyed.    --*/    
{  
    PHEAP Heap = (PHEAP)HeapHandle;  
    PLIST_ENTRY Head;  
    PVOID LowFragmentationHeap;  
    PLIST_ENTRY Next;  
    SIZE_T RegionSize;  
    PHEAP_SEGMENT Segment;  
    UCHAR SegmentIndex;  
    NTSTATUS Status;  
    ULONG Interceptor;  
    PHEAP_VIRTUAL_ALLOC_ENTRY VirtualEntry;  
    PVOID VirtualEntryReservedBase;  
  
    //      //  Validate that HeapAddress points to a HEAP structure.      //    
    RTL_PAGED_CODE();  
  
    if (HeapHandle == NULL) {  
  
        HEAP_ERROR( HEAP_ERROR_LEVEL_VALIDATION_FAILURE,  
                    (HeapHandle != NULL),  
                    HeapHandle);  
  
        return NULL;  
    }  
  
    //      //  Check if this is the debug version of heap using page allocation      //  with guard pages      //    
    IF_DEBUG_PAGE_HEAP_THEN_RETURN( HeapHandle,  
                                    RtlpDebugPageHeapDestroy( HeapHandle ));  
  
    if (Interceptor = Heap->Interceptor) {  
  
        (*RtlpGetInterceptRoutine(Interceptor))( Heap,  
                                                 HEAP_INTERCEPT_DESTROY,  
                                                 RtlpGetInterceptorContextValue(Interceptor),  
                                                 NULL );  
    }  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
    //      //  In the non kernel case check if this is the debug version of heap      //  and of so then call the debug version to do the teardown      //    
    if (DEBUG_HEAP( Heap->Flags )) {  
  
        if (!RtlDebugDestroyHeap( HeapHandle )) {  
  
            return HeapHandle;  
        }  
    }  
  
    //      //  We are not allowed to destroy the process heap      //    
    if (HeapHandle == NtCurrentPeb()->ProcessHeap) {  
  
        return HeapHandle;  
    }  
  
    //      // The heap is ready to be destroyed. Remove it from the heap list      // before operating on it. This allows other threads (such as      // process reflection threads) to operate on the heap list without      // having any structures deleted out from under them (so long as they      // hold the heap list lock).      //    
    RtlpRemoveHeapFromProcessList( Heap );  
    if (LowFragmentationHeap = RtlpGetLowFragHeap(Heap)) {  
        RtlpDestroyLowFragHeap(LowFragmentationHeap);  
    }  
  
#endif // NTOS_KERNEL_RUNTIME    
    //      //  For every big allocation we remove it from the list and free the      //  vm      //    
    Head = &Heap->VirtualAllocdBlocks;  
    Next = Head->Flink;  
  
    while (Head != Next) {  
  
        VirtualEntry = (PHEAP_VIRTUAL_ALLOC_ENTRY)  
            CONTAINING_RECORD( Next, HEAP_VIRTUAL_ALLOC_ENTRY, Entry );  
  
        Next = Next->Flink;  
        RegionSize = 0;  
        VirtualEntryReservedBase = RtlpGetVirtualBlockReservedBase(VirtualEntry);  
  
        Status = RtlpHeapFreeVirtualMemory( NtCurrentProcess(),  
                                            &VirtualEntryReservedBase,  
                                            &RegionSize,  
                                            MEM_RELEASE );  
  
        NT_VERIFY(NT_SUCCESS(Status));  
  
        if (IS_HEAP_RANGE_LOGGING_ENABLED()) {  
  
            RtlpHeapLogRangeRelease(Heap, VirtualEntryReservedBase, RegionSize);  
        }  
    }  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
    //      //  In the non kernel case we need to destroy any heap tags we have setup      //  and remove this heap from the process heap list      //    
    RtlpDestroyTags( Heap );  
  
#endif // NTOS_KERNEL_RUNTIME    
    //      //  If the heap is serialized, delete the critical section created      //  by RtlCreateHeap.      //    
    if (!(Heap->Flags & HEAP_NO_SERIALIZE)) {  
  
        if (!(Heap->Flags & HEAP_LOCK_USER_ALLOCATED)) {  
  
            (VOID)RtlDeleteLockRoutine( Heap->LockVariable );  
        }  
  
        Heap->LockVariable = NULL;  
    }  
  
    //      //  For every segment in the heap we call a worker routine to      //  destroy the segment. The deletion must go backwards, so the segment      //  that contains the heap structure goes away last      //    
    do {  
  
        Segment = CONTAINING_RECORD( Heap->SegmentList.Blink, HEAP_SEGMENT, SegmentListEntry );  
  
        //          //  Deleting the segment will remove the segment from the list          //    
        RtlpDestroyHeapSegment( Segment );  
  
    } while ((PVOID)Segment != (PVOID)Heap);  
  
    if (IS_HEAP_LOGGING_ENABLED()) {  
        RtlpLogHeapDestroyEvent(HeapHandle, (HANDLE)HEAP_LOGGER_ID);  
    }  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
    if (IS_HEAPSUMMARY_LOGGING_ENABLED()) {  
        RtlpLogHeapDestroyEvent(HeapHandle, (HANDLE)HEAPSUMMARY_LOGGER_ID);  
    }  
  
#endif // NTOS_KERNEL_RUNTIME    
    if (IS_HEAP_RANGE_LOGGING_ENABLED()) {  
  
        RtlpHeapLogRangeDestroy(Heap);  
    }  
  
    //      //  And we return to our caller      //    
    return NULL;  
}  
  
_Must_inspect_result_  
__bcount_opt(Size) __allocator  
PVOID  
RtlAllocateHeap (  
    __in PVOID HeapHandle,  
    __in_opt ULONG Flags,  
    __in SIZE_T Size  
    )  
  
/*++    Routine Description:        This routine allocates a memory of the specified size from the specified      heap.    Arguments:        HeapHandle - Supplies a pointer to an initialized heap structure        Flags - Specifies the set of flags to use to control the allocation        Size - Specifies the size, in bytes, of the allocation    Return Value:        PVOID - returns a pointer to the newly allocated block    --*/    
{  
    SIZE_T AllocationIndex;  
    SIZE_T AllocationSize = 0;  
    SIZE_T ExtraSize;  
    PHEAP Heap = (PHEAP)HeapHandle;  
    ULONG Interceptor = 0;  
    PVOID InterceptorData;  
    PHEAP_LOOKUP_ENTRY LookupEntry = NULL;  
    PVOID ReturnValue = NULL;  
    ULONG TracingSource = 0;  
  
    //      // Set any flags that the heap enforces for all allocations.      //    
    Flags |= Heap->ForceFlags;  
  
    if (Size > MAXINT_PTR) {  
        TracingSource = MEMORY_FROM_INVALID;  
        ReturnValue = NULL;  
        goto DoExit;  
    }  
  
    if (Interceptor = Heap->Interceptor) {  
  
        if (!(Flags & (HEAP_NO_INTERCEPTORS | HEAP_NEED_EXTRA_FLAGS))  
              &&  
            !IS_DEBUG_PAGE_HEAP_HANDLE(HeapHandle)) {  
  
            ExtraSize = 0;  
  
            if (!NT_SUCCESS((*RtlpGetInterceptRoutine(Interceptor))( Heap,  
                                                                     HEAP_INTERCEPT_PRE_ALLOC,  
                                                                     RtlpGetInterceptorContextValue(Interceptor),  
                                                                     &ExtraSize ))) {  
  
                Interceptor = 0;  
                ReturnValue = NULL;  
                goto raiseandexit;  
            }  
  
            ExtraSize = ROUND_UP_TO_POWER2(ExtraSize, sizeof(HEAP_ENTRY));  
  
            //              //  Create space for the extended header entry              //    
            ExtraSize += sizeof(HEAP_ENTRY);  
            Size += ExtraSize;  
  
        } else {  
  
            Interceptor = 0;  
        }  
    }  
  
    //      //  Check for special features that force us to call the slow, do-everything      //  version.  We do everything slow for any of the following flags.      //      //    HEAP_SLOW_FLAGS defined as           0x6f030f60      //      //      HEAP_DEBUG_FLAGS, defined as       0x69020000 (heappriv.h)      //      //        HEAP_VALIDATE_PARAMETERS_ENABLED 0x40000000 (heap.h)      //      //        HEAP_VALIDATE_ALL_ENABLED        0x20000000 (heap.h)      //      //        HEAP_CAPTURE_STACK_BACKTRACES    0x08000000 (heap.h)      //      //        HEAP_CREATE_ENABLE_TRACING       0x00020000 (ntrtl.h winnt obsolete)      //      //        HEAP_FLAG_PAGE_ALLOCS            0x01000000 (heappage.h)      //      //      HEAP_SETTABLE_USER_FLAGS           0x00000E00 (ntrtl.h)      //      //      HEAP_NEED_EXTRA_FLAGS              0x0f000100 (heap.h)      //      //      HEAP_CREATE_ALIGN_16               0x00010000 (ntrtl.h winnt obsolete)      //      //      HEAP_FREE_CHECKING_ENABLED         0x00000040 (ntrtl.h winnt)      //      //      HEAP_TAIL_CHECKING_ENABLED         0x00000020 (ntrtl.h winnt )      //      //  We also do everything slow if the size is greater than max long      //    
    AllocationSize = ROUND_UP_TO_POWER2(((Size ? Size : 1) -  
                                         HEAP_UNUSED_PRIVATE_DATA_SIZE + HEAP_GRANULARITY),  
                                         HEAP_GRANULARITY);  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
    //      // Compute the allocation index for the current request. This accounts      // for size-0 requests, rounding them up to size 1 first. The result      // is the size, in heap units, needed to hold the requested size,      // including the size of a heap header for the final allocation.      //    
    AllocationIndex = AllocationSize >> HEAP_GRANULARITY_SHIFT;  
  
    //      // If none of the slow flags are set, this is not an internal allocation,      // and the heap is serializable, see if this allocation request should be      // forwarded to the front-end heap.      //    
    if (!(Flags & (HEAP_SLOW_FLAGS | HEAP_NO_CACHE_BLOCK | HEAP_NO_SERIALIZE))) {  
        if ((Size <= HEAP_LARGEST_LFH_BLOCK) &&  
            (RtlpIsFrontEndEnabledForIndex(Heap, AllocationIndex)) != FALSE) {  
  
            HEAPASSERT(Heap->FrontEndHeapUsageData[AllocationIndex] ==  
                       RtlpGetLFHContext(RtlpGetLowFragHeap(Heap), (Size ? Size : 1)));  
  
            TracingSource = MEMORY_FROM_LOWFRAG;  
            ReturnValue = RtlpLowFragHeapAllocFromContext(Heap->FrontEndHeap,  
                                                          Heap->FrontEndHeapUsageData[AllocationIndex],  
                                                          Size,  
                                                          Flags);  
  
            if (ReturnValue != NULL) {  
  
                HEAPASSERT( RtlpIsLFHBlock( (PHEAP_ENTRY)ReturnValue - 1) );  
  
                goto DoExit;  
            }  
        }  
  
        //          // Allocating from the low-fragmentation heap failed. Attempt to          // reference the free block lookup entry for this allocation index.          //    
        LookupEntry = RtlpGetLookupEntry( Heap->BlocksIndex, AllocationIndex );  
    }  
  
#endif  
  
    //      // Attempt to allocate from the back-end heap.      //    
    ReturnValue = RtlpAllocateHeap( Heap,  
                                    Flags | HEAP_NO_INTERCEPTORS,  
                                    Size,  
                                    AllocationSize,  
                                    LookupEntry,  
                                    &TracingSource );  
  
DoExit:  
  
    if (ReturnValue) {  
        if (Interceptor) {  
            InterceptorData = ReturnValue;  
            Size -= ExtraSize;  
            ReturnValue = RtlpSetupExtendedBlock( Heap,  
                                                  Flags,  
                                                  ReturnValue,  
                                                  Size,  
                                                  (USHORT)ExtraSize,  
                                                  Interceptor );  
  
            if (!NT_SUCCESS((*RtlpGetInterceptRoutine(Interceptor))( Heap,  
                                                                     HEAP_INTERCEPT_POST_ALLOC,  
                                                                     RtlpGetInterceptorContextValue(Interceptor),  
                                                                     InterceptorData))) {  
  
                RtlFreeHeap(Heap, 0, ReturnValue);  
                ReturnValue = NULL;  
                goto raiseandexit;  
            }  
  
        }  
  
        HEAPASSERT((Size == 0) || (RtlSizeHeap(Heap, 0, ReturnValue) == Size));  
  
        //          // Blocks that we are returning to the user should be committed. Ensure          // that non-LFH, non-internal blocks have the DECOMMIT flag cleared. This          // assert must contain all the conditions capable of voluntarily          // setting this flag.          //    
        if (ReturnValue && (IS_DEBUG_PAGE_HEAP_HANDLE(Heap) == FALSE)) {  
            HEAPASSERT((RtlpIsLFHBlock(RtlpGetHeapEntry(ReturnValue)))  
                            ||  
                       (Flags & HEAP_NO_CACHE_BLOCK)  
                            ||  
                       (!RtlpIsInternalFlagSet(Heap, RtlpGetHeapEntry(ReturnValue))));  
        }  
  
    } else {  
  
raiseandexit:  
  
        SET_LAST_STATUS(STATUS_NO_MEMORY);  
  
        //          //  This is the error return.          //    
        if (Flags & HEAP_GENERATE_EXCEPTIONS) {  
  
            EXCEPTION_RECORD ExceptionRecord;  
  
            //              //  Construct an exception record.              //    
            ExceptionRecord.ExceptionCode = STATUS_NO_MEMORY;  
            ExceptionRecord.ExceptionRecord = (PEXCEPTION_RECORD)NULL;  
            ExceptionRecord.NumberParameters = 1;  
            ExceptionRecord.ExceptionFlags = 0;  
            ExceptionRecord.ExceptionInformation[ 0 ] = (AllocationSize == 0) ? Size : AllocationSize;  
            ExceptionRecord.ExceptionAddress = (PVOID)(ULONG_PTR) RtlRaiseException;  
  
            RtlRaiseException( &ExceptionRecord );  
        }  
    }  
  
    //      // If heap logging is enabled, log the allocation. Three conditions      // must be met:      //      //  1. Heap logging must be enabled.      //  2. The heap handle must be a normal heap handle. Otherwise, checking      //     to see if an allocation is marked as internal will AV.      //  3. This function must be called with debug options disabled. When      //     debug flags are set in the heap, this ensures that only the      //     innermost call to this function (i.e., the one with all the debug      //     checks bypassed) logs an event. This is needed to prevent double      //     events from being logged under light pageheap and certain gflags      //     options.      //    
    if (IS_HEAP_LOGGING_ENABLED() &&  
        (IS_DEBUG_PAGE_HEAP_HANDLE(Heap) == FALSE) &&  
        (DEBUG_HEAP(Flags) == FALSE) &&  
        TracingSource != MEMORY_FROM_INVALID) {  
  
        if ((ReturnValue == 0) ||  
            (RtlpIsLFHBlock(RtlpGetHeapEntry(ReturnValue))) ||  
            (!RtlpIsInternalFlagSet(Heap, RtlpGetHeapEntry(ReturnValue)))) {  
  
            RtlpLogHeapAllocateEvent(Heap,  
                                     ReturnValue,  
                                     Size,  
                                     TracingSource);  
        }  
    }  
  
    return ReturnValue;  
}  
  
__success(return != 0)  
LOGICAL  
RtlFreeHeap (  
    __in PVOID HeapHandle,  
    __in_opt ULONG Flags,  
    __in __post_invalid  PVOID BaseAddress  
    )  
  
/*++    Routine Description:        This routine returns a previously allocated block back to its heap    Arguments:        HeapHandle - Supplies a pointer to the owning heap structure        Flags - Specifies the set of flags to use in the deallocation        BaseAddress - Supplies a pointer to the block being freed    Return Value:        LOGICAL - TRUE if the block was properly freed and FALSE otherwise    --*/    
{  
    PHEAP_ENTRY BusyBlock = NULL;  
    PHEAP Heap = (PHEAP)HeapHandle;  
    ULONG Interceptor = 0;  
  
    //      //  First check if the address we're given is null and if so then      //  there is really nothing to do so just return success      //    
    if (BaseAddress == NULL) {  
  
        return TRUE;  
    }  
  
    if (!IS_DEBUG_PAGE_HEAP_HANDLE(HeapHandle)) {  
  
        //          //  Get the pointer pointer to the start of the block          //    
        BusyBlock = RtlpProbeUserBuffer( Heap, BaseAddress );  
  
        if ( BusyBlock == NULL) {  
  
            SET_LAST_STATUS( STATUS_INVALID_PARAMETER );  
  
            return FALSE;  
        }  
  
        if (RtlpHasExtendedHeader(BaseAddress)) {  
  
             if (RtlpValidateExtendedEntry(Heap, BusyBlock, BaseAddress) == FALSE) {  
                SET_LAST_STATUS(STATUS_INVALID_PARAMETER);  
                return FALSE;  
             }  
  
             Interceptor = ((PHEAP_ENTRY)BaseAddress - 1)->InterceptorValue;  
  
  
             //               //  If we have an interceptor defined, call the appropriate routine               //    
             if (!(Flags & (HEAP_NO_INTERCEPTORS | HEAP_NEED_EXTRA_FLAGS))) {  
  
                 if (!NT_SUCCESS((*RtlpGetInterceptRoutine(Interceptor))( Heap,  
                                                                          HEAP_INTERCEPT_PRE_FREE,  
                                                                          RtlpGetInterceptorContextValue(Interceptor),  
                                                                          RtlpGetExtendedData(BaseAddress)))) {  
  
                     SET_LAST_STATUS( STATUS_INVALID_PARAMETER );  
  
                     return FALSE;  
                 }  
             }  
        }  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
        //          //  For the user mode heap, check whether this is an LFH          //  block and free it.          //    
        if (RtlpIsLFHBlock(BusyBlock)) {  
            RtlpLowFragHeapFree(Heap, BusyBlock);  
  
            if (IS_HEAP_LOGGING_ENABLED()) {  
                RtlpLogHeapFreeEvent( Heap, BaseAddress, MEMORY_FROM_LOWFRAG );  
            }  
  
            return TRUE;  
        }  
  
#endif // NTOS_KERNEL_RUNTIME    
    }  
  
    return RtlpFreeHeap(Heap, Flags | HEAP_NO_INTERCEPTORS, BusyBlock, BaseAddress);  
}  
  
  
SIZE_T  
RtlSizeHeap (  
    __in PVOID HeapHandle,  
    __in_opt ULONG Flags,  
    __in PVOID BaseAddress  
    )  
  
/*++    Routine Description:        This routine returns the size, in bytes, of the indicated block      of heap storage.  The size only includes the number of bytes the      original caller used to allocate the block and not any unused      bytes at the end of the block.    Arguments:        HeapHandle - Supplies a pointer to the heap that owns the block          being queried        Flags - Supplies a set of flags used to allocate the block        BaseAddress - Supplies the address of the block being queried    Return Value:        SIZE_T - returns the size, in bytes, of the queried block, or -1          if the block is not in use.    --*/    
{  
    PHEAP Heap = (PHEAP)HeapHandle;  
    PHEAP_ENTRY BusyBlock;  
    SIZE_T BusySize;  
  
    //      //  Compliment the input flags with those enforced by the heap      //    
    Flags |= Heap->ForceFlags;  
  
    //      //  Check if this is the nonkernel debug version of heap      //    
#if !defined(NTOS_KERNEL_RUNTIME)  
  
    if (DEBUG_HEAP( Flags )) {  
  
        return RtlDebugSizeHeap( HeapHandle, Flags, BaseAddress );  
    }  
  
#endif // NTOS_KERNEL_RUNTIME    
    //      //  No lock is required since nothing is modified and nothing      //  outside the busy block is read.  Backup to get a pointer      //  to the heap entry      //    
  
    BusyBlock = RtlpProbeUserBuffer( Heap, BaseAddress );  
  
    if ( BusyBlock == NULL) {  
        SET_LAST_STATUS( STATUS_INVALID_PARAMETER );  
        return (SIZE_T)-1;  
    }  
  
    //      //  If the block is not in use then the answer is -1 and      //  we'll set the error status for the user mode thread      //    
    if (RtlpIsVirtualBlock( BusyBlock )) {  
  
        //          //  Otherwise if the block is from our large allocation then          //  we'll get the result from that routine          //    
        BusySize = RtlpGetSizeOfBigBlock( Heap, BusyBlock );  
  
    } else {  
  
        //          //  Otherwise the block must be one that we can handle so          //  calculate its block size and then subtract what's not being          //  used by the caller.          //          //  Note: This includes the heap entry header in its calculation.          //    
        BusySize = (((SIZE_T)RtlpGetAllocationUnits(Heap, BusyBlock)) << HEAP_GRANULARITY_SHIFT) -  
                    RtlpGetUnusedBytes(Heap, BusyBlock);  
    }  
  
    //      //  And return to our caller      //    
    return BusySize;  
}  
  
NTSTATUS  
RtlZeroHeap (  
    __in PVOID HeapHandle,  
    __in_opt ULONG Flags  
    )  
  
/*++    Routine Description:        This routine zero's (or fills) in all the free blocks in a heap.      It does not touch big allocations.    Arguments:        HeapHandle - Supplies a pointer to the heap being zeroed        Flags - Supplies a set of heap flags to compliment those already          set in the heap    Return Value:        NTSTATUS - An appropriate status code    --*/    
{  
    PHEAP_ENTRY CurrentBlock;  
    PHEAP_ENTRY EncodedBlock = NULL;  
    PHEAP_FREE_ENTRY FreeBlock;  
    PHEAP Heap = (PHEAP)HeapHandle;  
    BOOLEAN LockAcquired = FALSE;  
    PHEAP_SEGMENT Segment;  
    ULONG SegmentIndex;  
    SIZE_T Size;  
    NTSTATUS Status;  
    PHEAP_UCR_DESCRIPTOR UCRHeader;  
  
    //      //  Compliment the input flags with those enforced by the heap      //    
    Flags |= Heap->ForceFlags;  
  
    //      //  Check if this is the nonkernel debug version of heap      //    
#if !defined(NTOS_KERNEL_RUNTIME)  
  
    if (DEBUG_HEAP( Flags )) {  
  
        return RtlDebugZeroHeap( HeapHandle, Flags );  
    }  
  
#endif // NTOS_KERNEL_RUNTIME    
    //      //  Unless something happens otherwise we'll assume that we'll      //  be successful      //    
    Status = STATUS_SUCCESS;  
  
    try {  
  
        //          //  Lock the heap          //    
        if (!(Flags & HEAP_NO_SERIALIZE)) {  
  
            RtlAcquireLockRoutine( Heap->LockVariable );  
  
            LockAcquired = TRUE;  
        }  
  
        try {  
  
            PLIST_ENTRY Head, Next;  
  
            Head = &Heap->SegmentList;  
            Next = Head->Flink;  
  
            //              //  Zero fill all the free blocks in all the segments              //    
            for (Next = Head->Flink; Next != Head; Next = Next->Flink ) {  
  
                Segment = CONTAINING_RECORD( Next, HEAP_SEGMENT, SegmentListEntry );  
  
                CurrentBlock = Segment->FirstEntry;  
  
                //                  //  With the current segment we'll zoom through the                  //  blocks until we reach the end                  //    
                while (CurrentBlock < Segment->LastValidEntry) {  
  
                    if (EncodedBlock) {  
  
                        RtlpPackHeapEntry( Heap, EncodedBlock );  
                    }  
  
                    RtlpUnpackHeapEntry( Heap, CurrentBlock );  
                    EncodedBlock = CurrentBlock;  
  
                    Size = CurrentBlock->Size << HEAP_GRANULARITY_SHIFT;  
  
                    //                      //  If the block is not in use then we'll either zero                      //  it or fill it.                      //    
                    if (!RtlpIsBlockBusySafe( CurrentBlock )) {  
  
                        FreeBlock = (PHEAP_FREE_ENTRY)CurrentBlock;  
  
                        if ( FreeBlock->Flags & HEAP_ENTRY_DECOMMITTED ) {  
  
                            if (RtlpFastRemoveFreeBlock(Heap, FreeBlock)) {  
  
                                //                                  //  Succesfully removed the block. This guarantees the block                                  //  has been committed in place. We can insert it back in the list                                  //  and continue walking the heap                                  //    
                                RtlpFastInsertFreeBlockDirect(Heap, FreeBlock, FreeBlock->Size);  
                            }  
  
                            //                              //  We either comitted a block or converted to a true UCR.                              //  The entry should be encoded at this point, retry again from the top                              //  preserving the FreeBlock pointer                              //    
                            EncodedBlock = NULL;  
  
                            continue;  
                        }  
  
                        if ((Heap->Flags & HEAP_FREE_CHECKING_ENABLED) &&  
                            (CurrentBlock->Flags & HEAP_ENTRY_FILL_PATTERN)) {  
  
                            RtlFillMemoryUlong( FreeBlock + 1,  
                                                Size - sizeof( *FreeBlock ),  
                                                FREE_HEAP_FILL );  
  
                        } else {  
  
                            RtlFillMemoryUlong( FreeBlock + 1,  
                                                Size - sizeof( *FreeBlock ),  
                                                0 );  
                        }  
                    }  
  
                    //                      //  If the following entry is uncommited then we need to                      //  skip over it.  This code strongly implies that the                      //  uncommitted range list is in perfect sync with the                      //  blocks in the segement                      //    
                    if ( RtlpIsUCRBlock( CurrentBlock ) ) {  
  
                        UCRHeader = (PHEAP_UCR_DESCRIPTOR)(CurrentBlock + 1);  
  
                        CurrentBlock = (PHEAP_ENTRY)  
                            (((PCHAR)(UCRHeader + 1)) + UCRHeader->Size);  
  
                    } else {  
  
                        //                          //  Otherwise the next block exists so advance to it                          //    
                        CurrentBlock += CurrentBlock->Size;  
                    }  
                }  
            }  
  
        } except( RtlpHeapExceptionFilter(GetExceptionCode()) ) {  
  
            Status = GetExceptionCode();  
        }  
  
    } finally {  
  
        //          //  Unlock the heap          //    
        if (EncodedBlock) {  
  
            RtlpPackHeapEntry( Heap, EncodedBlock );  
        }  
  
        if (LockAcquired) {  
  
            RtlReleaseLockRoutine( Heap->LockVariable );  
        }  
    }  
  
    return Status;  
}  
  
//  //  Private support procedures.  //    
VOID  
FASTCALL  
RtlpUpdateHeapRates (  
    __inout PHEAP Heap,  
    __in ULONG Counter  
    )  
{  
    switch (Counter) {  
  
        case HEAP_COUNTER_LOCK_COLLISION: {  
  
            Heap->Counters.LockAcquires += 1;  
            Heap->Counters.LockCollisions += 1;  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
            if (Heap->FrontEndHeapType != HEAP_FRONT_LOWFRAGHEAP) {  
  
                if (Heap->Counters.LockAcquires < Heap->Counters.LockCollisions) {  
  
                    //                      //  If the counter overflows, reset the counters                      //    
                    Heap->Counters.LockAcquires = 0;  
                    Heap->Counters.LockCollisions = 0;  
                }  
  
                if (Heap->Counters.LockCollisions >= Heap->Counters.LockAcquires >> HEAP_TUNING_COLLISION_THRESHOLD_SHIFT) {  
  
                    Heap->CompatibilityFlags |= HEAP_MAINTENANCE_ENABLE_LFH;  
                }  
            }  
#endif  
        }  
        break;  
  
        case HEAP_COUNTER_COMMIT:  
            Heap->Counters.CommitRate += 1;  
        break;  
  
        case HEAP_COUNTER_DECOMMIT:  
            Heap->Counters.DecommittRate += 1;  
        break;  
    }  
}  
  
  
VOID  
FASTCALL  
RtlpInsertUCRBlock (  
    __in PHEAP Heap,  
    __inout PHEAP_UCR_DESCRIPTOR UCRBlock  
    )  
  
/*++    Routine Description:        The function inserts a UCR descriptor in the UCR list  in sorted order, according with the UCR size. The UCRs at the end  of the segment (with size == 0) are not inserted in the list    Arguments:        Heap - Supplies the heap handle        UCRBlock - Supplies the UCR descriptor, completely initialized    Return Value:        None.    --*/    
{  
    //      //  Insert only the non-empty UCRs, in the list ordered by size      //    
    if (UCRBlock->Size) {  
  
        PLIST_ENTRY Next;  
  
        Next = RtlpFindUCREntry( Heap, UCRBlock->Size );  
  
        RtlpSafeInsertTailList( Next, &UCRBlock->ListEntry );  
  
        RtlpUpdateUCRIndexInsert( Heap, UCRBlock );  
    }  
  
    RtlpSafeInsertHeadList( &(RtlpGetSegment( Heap, ((PHEAP_ENTRY)UCRBlock - 1))->UCRSegmentList),  
                            &UCRBlock->SegmentEntry );  
}  
  
VOID  
FASTCALL  
RtlpRemoveUCRBlock (  
    __in PHEAP Heap,  
    __inout PHEAP_UCR_DESCRIPTOR UCRBlock  
    )  
  
/*++    Routine Description:        Removes a non-empty UCR descriptor from the heap's list    Arguments:        UCRBlock - Supplies the UCR to be removed from the list    Return Value:      None.    --*/    
{  
    //      //  Only non-empty UCRs are inserted to the list      //    
    RtlpHeapRemoveEntryList( &UCRBlock->SegmentEntry );  
  
    if (UCRBlock->Size) {  
  
        RtlpUpdateUCRIndexRemove( Heap, UCRBlock );  
        RtlpHeapRemoveEntryList( &UCRBlock->ListEntry );  
    }  
}  
  
  
PHEAP_UCR_DESCRIPTOR  
FASTCALL  
RtlpSearchUCRBlock (  
    __in PHEAP_SEGMENT Segment,  
    __in PVOID EndAddress  
    )  
  
/*++    Routine Description:        The routine searches the UCR list for the latest UCR descriptor  that ends at the given address.    Arguments:        Heap - Supplies the current heap handle        EndAddress - Supplies the address where the UCR should end.    Return Value:        Return the UCR descriptor (or NULL if no UCR matches the address)    --*/    
{  
    PLIST_ENTRY Head, Next;  
  
    Head = &Segment->UCRSegmentList;  
    Next = Head->Flink;  
  
    while (Head != Next) {  
  
        PHEAP_UCR_DESCRIPTOR TmpUCRBlock;  
  
        TmpUCRBlock = CONTAINING_RECORD( Next, HEAP_UCR_DESCRIPTOR, SegmentEntry );  
  
        if (((PCHAR)TmpUCRBlock->Address + TmpUCRBlock->Size) == (PCHAR)EndAddress) {  
  
            return TmpUCRBlock;  
  
        } else {  
  
            Next = Next->Flink;  
        }  
    }  
  
    return NULL;  
}  
  
VOID  
RtlpCreateUCREntry (  
    __inout PHEAP Heap,  
    __inout PHEAP_SEGMENT Segment,  
    __in PHEAP_UCR_DESCRIPTOR NewUCRBlock,  
    __in SIZE_T Size,  
    __in PHEAP_FREE_ENTRY LastKnownEntry,  
    __out PSIZE_T FreeSize  
    )  
  
/*++    Routine Description:        The function creates a UCR descriptor structure right before      a new uncommitted range, or the end of the segment. The UCR descriptor      will keep the base address and the size of the uncommitted range that      follows this descriptor. The size can only be 0 if the UCR block is      at the end of the segment.    Arguments:        Heap - Supplies the heap handle        Segment - Supplies the current segment containing the new UCR        UncommittedRange - Supplies the base address of the uncommitted range        Size - Supplies the size of the uncommitted range        LastKnownEntry - Supplies the pointer to the last valid free block entry          before the base address of the UCR        FreeSize - receives the size of the free block resulted from creating the UCR    Return Value:        None.    Assumptions:        Entire space from  LastKnownEntry to UncommittedRange is committed when      this function is called. The size of the LastKnownEntry must be valid    --*/    
{  
    PHEAP_ENTRY Entry;  
    PVOID UncommittedRange;  
  
    //      //  Initialize the UCR descriptor      //    
    UncommittedRange = (PVOID)(NewUCRBlock + 1);  
    NewUCRBlock->Address = UncommittedRange;  
    NewUCRBlock->Size = Size;  
  
    //      //  Setup properly the leading block entry      //    
    Entry = (PHEAP_ENTRY)NewUCRBlock - 1;  
#pragma prefast(suppress:__WARNING_BUFFER_UNDERFLOW, "A HEAP_ENTRY structure always preceeds an UCR block")  
    Entry->Flags = HEAP_ENTRY_BUSY;  
    Entry->Size = (sizeof(HEAP_UCR_DESCRIPTOR) + sizeof(HEAP_ENTRY)) >> HEAP_GRANULARITY_SHIFT;  
    Entry->UnusedBytes = 0;  
  
    RtlpSetUCRBlock( Entry );  
    RtlpSetSmallTagIndex( Heap, Entry, 0 );  
    RtlpSetBlockOffset( Entry, Segment );  
  
    HEAP_ERROR( HEAP_ERROR_LEVEL_CRITICAL,  
                ((PHEAP_ENTRY)LastKnownEntry <= Entry),  
                LastKnownEntry );  
  
    if ((PHEAP_ENTRY)LastKnownEntry != Entry) {  
  
        //          //  There is no free block here. The existing space is used for UCR          //    
        RtlpSetBlockPreviousSize(Heap, Entry, 0);  
    }  
  
    RtlpPackHeapEntry( Heap, Entry );  
  
    //      // Add the UCR block to the UCR block list and the UCR index table.      // This call must check to see whether this table exists.      //    
    RtlpInsertUCRBlock( Heap, NewUCRBlock );  
  
    //      //  Update the segment and heap counters affected by creation of a new UCR      //    
    Segment->NumberOfUnCommittedRanges += 1;  
    Segment->NumberOfUnCommittedPages += (ULONG)(NewUCRBlock->Size / PAGE_SIZE);  
    Heap->Counters.TotalMemoryCommitted -=  NewUCRBlock->Size;  
    Heap->Counters.TotalUCRs += 1;  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
    //      // If the UCR index table has not been created yet, enough UCRs exist,      // and the heap is eligible for UCR tracking, allocate and initialize      // the table.      //    
    if ((Heap->Counters.TotalUCRs > HEAP_UCR_INDEX_THRESHOLD) &&  
        (Heap->UCRIndex == NULL) &&  
        ((Heap->Flags & HEAP_NO_SERIALIZE) == 0) &&  
        ((Heap->Flags & HEAP_GROWABLE) != 0) &&  
        ((RtlpDisableHeapLookaside & HEAP_COMPAT_DISABLE_LOOKASIDES) == 0)) {  
  
        Heap->CompatibilityFlags |= HEAP_MAINTENANCE_EXTEND_UCR_INDEX;  
    }  
  
#endif  
  
    if (NewUCRBlock->Size >= (HEAP_MAXIMUM_BLOCK_SIZE << HEAP_GRANULARITY_SHIFT)) {  
        Heap->Counters.TotalMemoryLargeUCR += NewUCRBlock->Size;  
    }  
  
    *FreeSize = (SIZE_T)(Entry - (PHEAP_ENTRY)LastKnownEntry);  
}  
  
PHEAP_FREE_ENTRY  
RtlpFindAndCommitPages (  
    __inout PHEAP Heap,  
    __inout PSIZE_T Size  
    )  
  
/*++    Routine Description:        This function searches the supplied segment for an uncommitted range that      satisfies the specified size.  It commits the range and returns a heap entry      for the range.    Arguments:        Heap - Supplies the heap being manipulated        Segment - Supplies the segment being searched        Size - Supplies the size of what we need to look for, on return it contains          the size of what we're just found and committed.        AddressWanted - Optionally gives an address where we would like the pages          based.  If supplied the entry must start at this address    Return Value:        PHEAP_FREE_ENTRY - Returns a pointer to the newly committed range that          satisfies the given size requirement, or NULL if we could not find          something large enough and/or based at the address wanted.    --*/    
{  
    ULONG_PTR Address;  
    PHEAP_ENTRY FirstEntry;  
    SIZE_T Length;  
    PLIST_ENTRY Next;  
    PHEAP_ENTRY NextEntry;  
    PHEAP_SEGMENT Segment;  
    NTSTATUS Status;  
    PHEAP_UCR_DESCRIPTOR UCRBlock;  
  
    Next = RtlpFindUCREntry( Heap, *Size );  
  
    if (Next != &Heap->UCRList) {  
  
        UCRBlock = CONTAINING_RECORD( Next, HEAP_UCR_DESCRIPTOR, ListEntry );  
  
        HEAP_ERROR( HEAP_ERROR_LEVEL_CRITICAL,  
                    (UCRBlock->Size >= *Size),  
                    UCRBlock );  
  
        Segment = RtlpGetSegment( Heap, ((PHEAP_ENTRY)UCRBlock - 1) );  
  
        Address = (ULONG_PTR)UCRBlock->Address;  
  
        //          //  We found an uncommitted range large enough to          //  satisfy the request.          //    
        if (RtlpGetCommitRoutine(Heap) != NULL) {  
  
            Status = (RtlpGetCommitRoutine(Heap))( Heap,  
                                                   (PVOID *)&Address,  
                                                   Size );  
  
        } else {  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
            //              //  If we have a small uncommited range left, Adjust the size to              //  take that block too              //    
            if (( (UCRBlock->Size - (*Size)) <=  
                        (((SIZE_T)Heap->DeCommitFreeBlockThreshold) << HEAP_GRANULARITY_SHIFT) )  
                    &&  
                (UCRBlock->Size < (((SIZE_T)Heap->VirtualMemoryThreshold) << HEAP_GRANULARITY_SHIFT)) ) {  
  
                *Size = UCRBlock->Size;  
            }  
#endif //  NTOS_KERNEL_RUNTIME    
#ifdef _WIN64  
            //              // This is for Wow64 processes. This is needed to return PAGE_SIZE aligned              // aligned sizes.              //    
            *Size = ROUND_UP_TO_POWER2 (*Size, PAGE_SIZE);  
#endif  
  
            Status = ZwAllocateVirtualMemory( NtCurrentProcess(),  
                                              (PVOID *)&Address,  
                                              0,  
                                              Size,  
                                              MEM_COMMIT,  
                                              RtlpGetHeapProtection(Heap->Flags) );  
  
            RtlpUpdateHeapRates( Heap, HEAP_COUNTER_COMMIT );  
        }  
  
        if (!NT_SUCCESS( Status )) {  
  
            Heap->Counters.CommitFailures += 1;  
  
            return NULL;  
        }  
  
        if (IS_HEAP_LOGGING_ENABLED()) {  
            RtlpLogHeapCommit( Heap,  
                               (PVOID)Address,  
                               *Size,  
                               HEAP_LOG_FIND_AND_COMMIT_PAGES);  
  
        }  
  
        //          //  At this point we have some committed memory, with Address and Size          //  giving us the necessary details          //          //  Update the number of uncommitted pages in the segment and if necessary          //  mark down the largest uncommitted range          //    
        FirstEntry = (PHEAP_ENTRY)UCRBlock - 1;  
  
        RtlpUnpackHeapEntry( Heap, FirstEntry );  
  
        FirstEntry->Flags = 0;  
        FirstEntry->UnusedBytes = 0;  
  
        RtlpRemoveUCRBlock( Heap, UCRBlock );  
  
        Segment->NumberOfUnCommittedRanges -= 1;  
        Segment->NumberOfUnCommittedPages -= (ULONG)(UCRBlock->Size / PAGE_SIZE);  
        Heap->Counters.TotalMemoryCommitted +=  UCRBlock->Size;  
        Heap->Counters.CommittOps += 1;  
  
        Heap->Counters.TotalUCRs -= 1;  
  
        if (UCRBlock->Size >= (HEAP_MAXIMUM_BLOCK_SIZE << HEAP_GRANULARITY_SHIFT)) {  
  
            Heap->Counters.TotalMemoryLargeUCR -=  UCRBlock->Size;  
        }  
  
        if ((UCRBlock->Size > (*Size))  
                ||  
            (((PCHAR)UCRBlock->Address + UCRBlock->Size) == (PCHAR)Segment->LastValidEntry)) {  
  
            //              //  We need to create an UCR with the updated size if either              //  there is uncommitted space left in the existing UCR, or              //  this UCR is the last in the segment (and the size will be 0)              //    
            RtlpCreateUCREntry( Heap,  
                                Segment,  
                                (PHEAP_UCR_DESCRIPTOR)((PCHAR)UCRBlock->Address + (*Size)) - 1,  
                                UCRBlock->Size - (*Size),  
                                (PHEAP_FREE_ENTRY)FirstEntry,  
                                Size  
                              );  
  
            *Size = (*Size) << HEAP_GRANULARITY_SHIFT;  
  
        } else {  
  
            //              //  Since the UCR descriptor goes away              //  the free size needs to include the size of that structure too              //    
            *Size += ((SIZE_T)FirstEntry->Size) << HEAP_GRANULARITY_SHIFT;  
  
            //              //  This UCR becomes empty, and it is not the last              //  UCR in a segment.              //    
        }  
  
        RtlpSetSmallTagIndex( Heap, FirstEntry, 0 );  
        RtlpSetBlockOffset( FirstEntry, Segment );  
  
        //          //  Update the fields in the first entry, and optional          //  following entry.          //    
        if (IS_HEAP_LOGGING_ENABLED()) {  
  
                RtlpLogHeapExtendEvent( Heap,  
                                        (PVOID)FirstEntry,  
                                        *Size,  
                                        (Heap->TotalFreeSize) << HEAP_GRANULARITY_SHIFT,  
                                        (HANDLE)HEAP_LOGGER_ID );  
        }  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
        if (IS_HEAPSUMMARY_LOGGING_ENABLED()) {  
                RtlpLogHeapExtendEvent( Heap,  
                                       (PVOID)FirstEntry,  
                                       *Size,  
                                       (Heap->TotalFreeSize) << HEAP_GRANULARITY_SHIFT,  
                                       (HANDLE)HEAPSUMMARY_LOGGER_ID);  
        }  
  
#endif  
  
        //          //  And return the heap entry to our caller          //    
        return (PHEAP_FREE_ENTRY)FirstEntry;  
    }  
  
    //      //  At this point we did not find an uncommitted range entry that satisfied      //  our requirements either because of size and/or address.  So return null      //  to tell the user we didn't find anything.      //    
    return NULL;  
}  
  
  
//  //  Declared in heappriv.h  //    
BOOLEAN  
RtlpInitializeHeapSegment (  
    __inout PHEAP Heap,  
    __out PHEAP_SEGMENT Segment,  
    __in_opt SIZE_T SizeOfMetadata,  
    __in UCHAR SegmentIndex,  
    __in ULONG Flags,  
    __in PVOID BaseAddress,  
    __in PVOID UnCommittedAddress,  
    __in PVOID CommitLimitAddress  
    )  
  
/*++    Routine Description:        This routines initializes the internal structures for a heap segment.      The caller supplies the heap and the memory for the segment being      initialized    Arguments:        Heap - Supplies the address of the heap owning this segment        Segment - Supplies a pointer to the segment being initialized        SegmentIndex - unused        Flags - Supplies flags controlling the initialization of the segment          Valid flags are:                HEAP_SEGMENT_USER_ALLOCATED        BaseAddress - Supplies the base address for the segment        UnCommittedAddress - Supplies the address where the uncommited range starts        CommitLimitAddress - Supplies the top address available to the segment    Return Value:        BOOLEAN - TRUE if the initialization is successful and FALSE otherwise    --*/    
{  
    SIZE_T CommitSize;  
    PHEAP_ENTRY FirstEntry;  
    ULONG GlobalFlag = RtlGetNtGlobalFlags();  
    PVOID MinimumCommitted;  
    ULONG NumberOfCommittedPages;  
    ULONG NumberOfPages;  
    ULONG NumberOfUnCommittedPages;  
    USHORT PreviousSize;  
    USHORT Size;  
    NTSTATUS Status;  
  
  
  
    //      //  Compute the total number of pages possible in this segment      //    
    NumberOfPages = (ULONG) (((PCHAR)CommitLimitAddress - (PCHAR)BaseAddress) / PAGE_SIZE);  
  
    //      //  First entry points to the first possible segment entry after      //  the segment header      //    
    FirstEntry = (PHEAP_ENTRY)( (PCHAR)Segment + ROUND_UP_TO_POWER2( SizeOfMetadata,  
                                                                     HEAP_GRANULARITY ) );  
  
    PreviousSize = 0;  
  
    //      //  Compute the index size of the segment header      //    
    Size = (USHORT)(((PCHAR)FirstEntry - (PCHAR)Segment) >> HEAP_GRANULARITY_SHIFT);  
  
    //      //  Make sure we have space for the UCR descriptor      //    
    MinimumCommitted = (PVOID)((PCHAR)(FirstEntry + 1) + (sizeof(HEAP_UCR_DESCRIPTOR) + sizeof(HEAP_ENTRY)));  
  
    //      //  If the first available heap entry is not committed and      //  it is beyond the heap limit then we cannot initialize      //    
    if ( MinimumCommitted >= UnCommittedAddress) {  
  
        if (MinimumCommitted >= CommitLimitAddress) {  
  
            return FALSE;  
        }  
  
        //          //  Enough of the segment has not been committed so we          //  will commit enough now to handle the first entry          //    
        CommitSize = (PCHAR)(FirstEntry + 1) - (PCHAR)UnCommittedAddress;  
  
#ifdef _WIN64  
        //          // This is for Wow64 processes. This is needed to return PAGE_SIZE aligned          // aligned sizes.          //    
        CommitSize = ROUND_UP_TO_POWER2 (CommitSize, PAGE_SIZE);  
#endif  
  
  
        Status = ZwAllocateVirtualMemory( NtCurrentProcess(),  
                                          (PVOID *)&UnCommittedAddress,  
                                          0,  
                                          &CommitSize,  
                                          MEM_COMMIT,  
                                          RtlpGetHeapProtection(Heap->Flags) );  
  
        if (!NT_SUCCESS( Status )) {  
  
            Heap->Counters.CommitFailures += 1;  
            return FALSE;  
        }  
  
        if (IS_HEAP_LOGGING_ENABLED()) {  
            RtlpLogHeapCommit( Heap,  
                               UnCommittedAddress,  
                               CommitSize,  
                               HEAP_LOG_INITIALIZE_SEGMENT);  
  
        }  
  
        //          //  Because we had to commit some memory we need to adjust          //  the uncommited address          //    
        UnCommittedAddress = (PVOID)((PCHAR)UnCommittedAddress + CommitSize);  
    }  
  
    //      //  At this point we know there is enough memory committed to handle the      //  segment header and one heap entry      //      //  Now compute the number of uncommited pages and the number of committed      //  pages      //    
  
    NumberOfUnCommittedPages = (ULONG)(((PCHAR)CommitLimitAddress - (PCHAR)UnCommittedAddress) / PAGE_SIZE);  
    NumberOfCommittedPages = NumberOfPages - NumberOfUnCommittedPages;  
  
    //      //  Initialize the heap segment heap entry.  We      //  calculated earlier if there was a previous entry      //    
    Segment->Entry.Size = Size;  
    Segment->Entry.Flags = HEAP_ENTRY_BUSY;  
    Segment->Entry.SmallTagIndex = 0;  
    Segment->Entry.UnusedBytes = HEAP_BLOCK_STATE_NT_METADATA_BLOCK;  
    RtlpSetBlockPreviousSize(Heap, &Segment->Entry, PreviousSize);  
  
#if !NTOS_KERNEL_RUNTIME  
  
    //      //  In the non kernel case see if we need to capture the callers stack      //  backtrace      //    
    if (GlobalFlag & FLG_USER_STACK_TRACE_DB) {  
  
        Segment->SegmentAllocatorBackTraceIndex = (USHORT)RtlLogStackBackTrace();  
    }  
  
#endif // !NTOS_KERNEL_RUNTIME    
    //      //  Now initializes the heap segment      //    
    Segment->SegmentSignature = HEAP_SEGMENT_SIGNATURE;  
    Segment->SegmentFlags = Flags;  
    Segment->Heap = Heap;  
    Segment->BaseAddress = BaseAddress;  
    Segment->FirstEntry = FirstEntry;  
    Segment->LastValidEntry = (PHEAP_ENTRY)((PCHAR)BaseAddress + (NumberOfPages * PAGE_SIZE));  
    Segment->NumberOfPages = NumberOfPages;  
    Segment->NumberOfUnCommittedPages = 0;  
  
    Heap->Counters.TotalMemoryCommitted +=  NumberOfPages * PAGE_SIZE;  
    Heap->Counters.TotalMemoryReserved += NumberOfPages * PAGE_SIZE;  
  
  
    InitializeListHead( &Segment->UCRSegmentList );  
    RtlpSetBlockOffset(&Segment->Entry, Segment);  
  
    //      //  Initialize the first free heap entry after the heap segment header and      //  put it in the free list.  This first entry will be for whatever is left      //  of the committed range      //    
    RtlpSetBlockPreviousSize(Heap, FirstEntry, Segment->Entry.Size);  
    RtlpSetBlockOffset(FirstEntry, Segment);  
  
    //      //  If there are uncommitted pages then we need to insert them      //  into the uncommitted ranges list. We create the UCR descriptor anyway      //  since any segment need to end with this structure      //    
    RtlpCreateUCREntry( Heap,  
                        Segment,  
                        (PHEAP_UCR_DESCRIPTOR)UnCommittedAddress - 1,  
                        NumberOfUnCommittedPages * PAGE_SIZE,  
                        (PHEAP_FREE_ENTRY)FirstEntry,  
                        &CommitSize  
                      );  
  
    RtlpPackHeapEntry(Heap, &Segment->Entry);  
  
    if (CommitSize) {  
  
        RtlpInsertFreeBlock( Heap, (PHEAP_FREE_ENTRY)FirstEntry, CommitSize );  
    }  
  
    //      //  Have the containing heap point to this segment via the specified index      //    
    RtlpSafeInsertTailList( &Heap->SegmentList, &Segment->SegmentListEntry);  
    Heap->Counters.TotalSegments += 1;  
  
    //      //  And return to our caller      //    
    return TRUE;  
}  
  
NTSTATUS  
RtlpDestroyHeapSegment (  
    __in PHEAP_SEGMENT Segment  
    )  
  
/*++    Routine Description:        This routine removes an existing heap segment.  After the call it      is as if the segment never existed    Arguments:        Segment - Supplies a pointer to the heap segment being destroyed    Return Value:        NTSTATUS - An appropriate status value    --*/    
{  
    PVOID BaseAddress;  
    SIZE_T BytesToFree;  
    PHEAP Heap;  
    NTSTATUS Status;  
  
    RTL_PAGED_CODE();  
  
    //      //  We actually only have work to do if the segment is not      //  user allocated.  If the segment is user allocated then      //  we'll assume knows how to get rid of the memory      //    
    if (!(Segment->SegmentFlags & HEAP_SEGMENT_USER_ALLOCATED)) {  
  
        RtlpHeapRemoveEntryList( &Segment->SegmentListEntry );  
  
        Heap = Segment->Heap;  
        BaseAddress = Segment->BaseAddress;  
        BytesToFree = 0;  
  
        //          //  Free all the virtual memory for the segment and return          //  to our caller.          //    
        Status = RtlpHeapFreeVirtualMemory( NtCurrentProcess(),  
                                            (PVOID *)&BaseAddress,  
                                            &BytesToFree,  
                                            MEM_RELEASE );  
  
        NT_VERIFY(NT_SUCCESS(Status));  
  
        if (IS_HEAP_RANGE_LOGGING_ENABLED()) {  
  
            RtlpHeapLogRangeRelease(Heap, BaseAddress, BytesToFree);  
        }  
  
        return Status;  
  
    } else {  
  
        //          //  User allocated segments are a noop          //    
        return STATUS_SUCCESS;  
    }  
}  
  
  
PHEAP_FREE_ENTRY  
RtlpExtendHeap (  
    __in PHEAP Heap,  
    __in SIZE_T AllocationSize  
    )  
  
/*++    Routine Description:        This routine is used to extend the amount of committed memory in a heap    Arguments:        Heap - Supplies the heap being modified        AllocationSize - Supplies the size, in bytes, that we need to extend the          heap    Return Value:        PHEAP_FREE_ENTRY - Returns a pointer to the newly created heap entry          of the specified size, or NULL if we weren't able to extend the heap    --*/    
{  
    SIZE_T CommitSize;  
    PHEAP_FREE_ENTRY FreeBlock;  
    SIZE_T FreeSize;  
    PLIST_ENTRY Head;  
    PLIST_ENTRY Next;  
    ULONG NumberOfPages;  
    SIZE_T ReserveSize;  
    PHEAP_SEGMENT Segment;  
    NTSTATUS Status;  
  
    //      //  Compute the number of pages need to hold this extension      //  And then compute the real free, still in bytes, based on      //  the page count      //    
    NumberOfPages = (ULONG) ((AllocationSize + PAGE_SIZE - 1) / PAGE_SIZE);  
    FreeSize = NumberOfPages * PAGE_SIZE;  
    FreeBlock = NULL;  
  
    FreeBlock = RtlpFindAndCommitPages( Heap,  
                                        &FreeSize);  
  
    //      //  If we were successful the we will coalesce it with adjacent      //  free blocks and put it in the free list then return the      //  the free block      //    
    if (FreeBlock != NULL) {  
  
        //          //  RtlpCoalesceFreeBlocks needs the free size in heap units.          //  We'll shift with the granularity before calling the coalesce.          //    
        FreeSize = FreeSize >> HEAP_GRANULARITY_SHIFT;  
  
        FreeBlock = RtlpCoalesceFreeBlocks( Heap, FreeBlock, &FreeSize, FALSE );  
  
        RtlpInsertFreeBlock( Heap, FreeBlock, FreeSize );  
  
        RtlpUnpackHeapEntry( Heap, (PHEAP_ENTRY)FreeBlock );  
  
        HEAPASSERT(FreeBlock->Size >= (AllocationSize >> HEAP_GRANULARITY_SHIFT));  
  
        return FreeBlock;  
    }  
  
    //      //  At this point we weren't able to get the memory from an existing      //  heap segment so now check if we found an unused segment index      //  and if we're allowed to grow the heap.      //    
    if ( Heap->Flags & HEAP_GROWABLE ) {  
  
        Segment = NULL;  
  
        //          //  Calculate a reserve size for the new segment, we might          //  need to fudge it up if the allocation size we're going for          //  right now is already beyond the default reserve size          //    
        if ((AllocationSize + (2 * PAGE_SIZE)) > Heap->SegmentReserve) {  
  
            ReserveSize = AllocationSize + (2 * PAGE_SIZE);  
  
        } else {  
  
            ReserveSize = Heap->SegmentReserve;  
        }  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
        if ((RtlpGetLowFragHeap(Heap) == NULL)  
                &&  
            (ReserveSize >= (HEAP_MAX_SEGMENT_SIZE / 4))) {  
  
            Heap->CompatibilityFlags |= HEAP_MAINTENANCE_ENABLE_LFH;  
        }  
#endif  
  
        ReserveSize = ROUND_UP_TO_POWER2(ReserveSize, HEAP_MM_GRANULARITY);  
  
        //          //  Limit the size of the segments to the maximum value addressable          //  from a segment offset          //    
        if (ReserveSize >= HEAP_MAX_SEGMENT_SIZE) {  
            ReserveSize = HEAP_MAX_SEGMENT_SIZE;  
        }  
  
        //          //  Try and reserve some vm          //    
        Status = ZwAllocateVirtualMemory( NtCurrentProcess(),  
                                          (PVOID *)&Segment,  
                                          0,  
                                          &ReserveSize,  
                                          MEM_RESERVE,  
                                          RtlpGetHeapProtection(Heap->Flags) );  
  
        //          //  If we get back status no memory then we should trim back the          //  request to something reasonable and try again.  We'll half          //  the amount until we it either succeeds or until we reach          //  the allocation size.  In the latter case we are really          //  out of memory.          //    
        while ((!NT_SUCCESS( Status )) && (ReserveSize != (AllocationSize + (2 * PAGE_SIZE)))) {  
  
            //              //  Reduce the reserved size by 50%              //    
            ReserveSize = ReserveSize / 2;  
  
            if ( ReserveSize < (AllocationSize + (2 * PAGE_SIZE)) ) {  
  
                ReserveSize = (AllocationSize + (2 * PAGE_SIZE));  
            }  
  
            Status = ZwAllocateVirtualMemory( NtCurrentProcess(),  
                                              (PVOID *)&Segment,  
                                              0,  
                                              &ReserveSize,  
                                              MEM_RESERVE,  
                                              RtlpGetHeapProtection(Heap->Flags) );  
        }  
  
        if (NT_SUCCESS( Status )) {  
  
            //              //  Adjust the heap state information              //    
            Heap->SegmentReserve += ReserveSize;  
  
            //              //  Compute the commit size to be either the default, or if              //  that's not big enough then make it big enough to handle              //  this current request              //    
            if ((AllocationSize + PAGE_SIZE) > Heap->SegmentCommit) {  
  
                CommitSize = AllocationSize + PAGE_SIZE;  
  
            } else {  
  
                CommitSize = Heap->SegmentCommit;  
            }  
  
#ifdef _WIN64  
            //              // This is for Wow64 processes. This is needed to return PAGE_SIZE aligned              // aligned sizes.              //    
            CommitSize = ROUND_UP_TO_POWER2( CommitSize, PAGE_SIZE );  
#endif  
  
            HEAPASSERT(CommitSize <= (ReserveSize - PAGE_SIZE));  
  
            //              //  Try and commit the memory              //    
            Status = ZwAllocateVirtualMemory( NtCurrentProcess(),  
                                              (PVOID *)&Segment,  
                                              0,  
                                              &CommitSize,  
                                              MEM_COMMIT,  
                                              RtlpGetHeapProtection(Heap->Flags) );  
  
            //              //  If the commit is successful but we were not able to              //  initialize the heap segment then still make the status              //  and error value              //    
            if (NT_SUCCESS( Status ) &&  
                !RtlpInitializeHeapSegment( Heap,  
                                            Segment,  
                                            sizeof(*Segment),  
                                            0,  
                                            HEAP_SEGMENT_PADDING_PRESENT,  
                                            Segment,  
                                            (PCHAR)Segment + CommitSize,  
                                            (PCHAR)Segment + ReserveSize - PAGE_SIZE)) {  
  
                Status = STATUS_NO_MEMORY;  
            }  
  
            if (IS_HEAP_LOGGING_ENABLED()) {  
                RtlpLogHeapCommit( Heap,  
                                   Segment,  
                                   CommitSize,  
                                   HEAP_LOG_EXTEND_HEAP);  
  
            }  
  
            //              //  If we've been successful so far then we're done and we              //  can return the first entry in the segment to our caller              //    
            if (NT_SUCCESS( Status )) {  
                if (IS_HEAP_LOGGING_ENABLED()) {  
  
                    RtlpLogHeapExtendEvent( Heap,  
                                            (PVOID)Segment->FirstEntry,  
                                            CommitSize,  
                                            (Heap->TotalFreeSize) << HEAP_GRANULARITY_SHIFT,  
                                            (HANDLE)HEAP_LOGGER_ID);  
                }  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
                if (IS_HEAPSUMMARY_LOGGING_ENABLED()) {  
  
                RtlpLogHeapExtendEvent( Heap,  
                                       (PVOID)Segment->FirstEntry,  
                                       CommitSize,  
                                       (Heap->TotalFreeSize) << HEAP_GRANULARITY_SHIFT,  
                                       (HANDLE)HEAPSUMMARY_LOGGER_ID );  
                }  
  
#endif //NTOS_KERNEL_RUNTIME    
                if (IS_HEAP_RANGE_LOGGING_ENABLED()) {  
  
                    RtlpHeapLogRangeReserve( Heap, Segment, ReserveSize );  
                }  
  
                RtlpUnpackHeapEntry( Heap, Segment->FirstEntry );  
  
                HEAPASSERT(((PHEAP_FREE_ENTRY)Segment->FirstEntry)->Size >= (AllocationSize >> HEAP_GRANULARITY_SHIFT));  
  
                return (PHEAP_FREE_ENTRY)Segment->FirstEntry;  
            }  
  
            //              //  Otherwise either the commit or heap segment initialization failed              //  so we'll release the memory which will also decommit it if necessary              //    
            Status = RtlpHeapFreeVirtualMemory( NtCurrentProcess(),  
                                                (PVOID *)&Segment,  
                                                &ReserveSize,  
                                                MEM_RELEASE );  
  
            NT_VERIFY(NT_SUCCESS(Status));  
  
        } else {  
            Heap->Counters.CommitFailures += 1;  
        }  
    }  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
    //      //  In the non kernel case we disabled coalescing on free then what we'll      //  do as a last resort is coalesce the heap and see if a block comes out      //  that we can use      //    
    if (Heap->Flags & HEAP_DISABLE_COALESCE_ON_FREE) {  
  
        FreeBlock = RtlpCoalesceHeap( Heap );  
  
        if ((FreeBlock != NULL) && (FreeBlock->Size >= AllocationSize)) {  
  
            return FreeBlock;  
        }  
    }  
  
#endif // NTOS_KERNEL_RUNTIME    
    //      //  Either the heap cannot grow or we out of resources of some type      //  so we're going to return null      //    
    return NULL;  
}  
  
  
//  //  Declared in heappriv.h  //    
PHEAP_FREE_ENTRY  
RtlpCoalesceFreeBlocks (  
    __in PHEAP Heap,  
    __in PHEAP_FREE_ENTRY FreeBlock,  
    __inout PSIZE_T FreeSize,  
    __in BOOLEAN RemoveFromFreeList  
    )  
  
/*++    Routine Description:        This routine coalesces the free block together.    Arguments:        Heap - Supplies a pointer to the heap being manipulated        FreeBlock - Supplies a pointer to the free block that we want coalesced        FreeSize - Supplies the size, in heap units, of the free block.  On return it          contains the size, in bytes, of the of the newly coalesced free block        RemoveFromFreeList - Indicates if the input free block is already on a          free list and needs to be removed to before coalescing    Return Value:        PHEAP_FREE_ENTRY - returns a pointer to the newly coalesced free block    --*/    
{  
    PHEAP_FREE_ENTRY FreeBlock1;  
    PHEAP_FREE_ENTRY NextFreeBlock;  
  
    //      //  Point to the preceding block      //    
    FreeBlock1 = (PHEAP_FREE_ENTRY)((PHEAP_ENTRY)FreeBlock - RtlpGetBlockPreviousSize( Heap, (PHEAP_ENTRY)FreeBlock ));  
  
    //      //  Check if there is a preceding block, and if it is free, and the two sizes      //  put together will still fit on a free lists.      //    
    if (FreeBlock1 != FreeBlock) {  
  
        if (!RtlpIsBusyFlagSet( Heap, (PHEAP_ENTRY)FreeBlock1 )) {  
  
            RtlpUnpackHeapEntry( Heap, (PHEAP_ENTRY)FreeBlock1 );  
  
            //              //  Check if we need to remove the input block from the free list              //    
            if (RemoveFromFreeList) {  
  
                if (!RtlpRemoveFreeBlock( Heap, FreeBlock )) {  
  
                    //                      //  The caller needs to make sure the block was committed                      //  before calling this function                      //    
                    HEAPASSERT(FALSE);  
                }  
  
                //                  //  We're removed so we don't have to do it again                  //    
                RemoveFromFreeList = FALSE;  
            }  
  
            //              //  We are going to merge ourselves with the preceding block              //    
            HEAPASSERT(RtlpGetBlockPreviousSize( Heap, (PHEAP_ENTRY)FreeBlock ) == FreeBlock1->Size);  
  
            //              //  Remove the preceding block from its free list              //    
            if (RtlpRemoveFreeBlock( Heap, FreeBlock1 )) {  
  
                //                  //  Clear the flags for the free block                  //    
                FreeBlock1->Flags = 0;  
                FreeBlock1->UnusedBytes = 0;  
  
                //                  //  Point to the preceding block, and adjust the sizes for the                  //  new free block.  It is the total of both blocks.                  //    
                FreeBlock = FreeBlock1;  
  
                *FreeSize += FreeBlock1->Size;  
  
                FreeBlock->Size = (USHORT)*FreeSize;  
  
                RtlpSetBlockPreviousSize( Heap,  
                                          ((PHEAP_ENTRY)FreeBlock + *FreeSize),  
                                          (USHORT)*FreeSize );  
            }  
        }  
    }  
  
    //      //  There is a following block so now get a pointer to it      //  and check if it is free and if putting the two blocks together      //  still fits on a free list      //    
    NextFreeBlock = (PHEAP_FREE_ENTRY)((PHEAP_ENTRY)FreeBlock + *FreeSize);  
  
    HEAPASSERT(NextFreeBlock != FreeBlock);  
  
    if (!RtlpValidateCheckSum( Heap, (PHEAP_ENTRY)NextFreeBlock )) {  
  
        RtlpLogHeapFailure( heap_failure_entry_corruption,  
                            Heap,  
                            NextFreeBlock,  
                            NULL,  
                            NULL,  
                            NULL );  
    }  
  
retrycoalesce:  
  
    if (!RtlpIsBusyFlagSet( Heap, (PHEAP_ENTRY)NextFreeBlock )) {  
  
        //          //  We are going to merge ourselves with the following block          //    
        RtlpUnpackHeapEntry( Heap, (PHEAP_ENTRY)NextFreeBlock );  
  
        //          //  Check if we need to remove the input block from the free list          //    
        if (RemoveFromFreeList) {  
  
            if (!RtlpRemoveFreeBlock( Heap, FreeBlock )) {  
  
                //                  //  The caller needs to make sure the block was committed                  //  before calling this function                  //    
                HEAPASSERT(FALSE);  
            }  
  
            //              //  We're removed so we don't have to do it again              //  if the next block is getting converted and retry coalescing              //    
            RemoveFromFreeList = FALSE;  
        }  
  
        //          //  Remove the following block from its free list          //    
        if (RtlpRemoveFreeBlock( Heap, NextFreeBlock )) {  
  
            //              //  Clear the free flags for the block              //    
            FreeBlock->Flags = 0;  
            FreeBlock->UnusedBytes = 0;  
  
            //              //  Adjust the size for the newly combined block              //    
            *FreeSize += NextFreeBlock->Size;  
  
            FreeBlock->Size = (USHORT)*FreeSize;  
  
            RtlpSetBlockPreviousSize( Heap,  
                                      ((PHEAP_ENTRY)FreeBlock + *FreeSize),  
                                      (USHORT)*FreeSize );  
        } else {  
  
            goto retrycoalesce;  
  
        }  
    }  
  
    //      //  And return the free block to our caller      //    
    return FreeBlock;  
}  
  
VOID  
RtlpDeCommitFreeBlock (  
    __inout PHEAP Heap,  
    __out PHEAP_FREE_ENTRY FreeBlock,  
    __in SIZE_T FreeSize,  
    __in BOOLEAN ForceDecommit  
    )  
  
/*++    Routine Description:        This routine takes a free block and decommits it.  This is usually called      because the block is beyond the decommit threshold    Arguments:        Heap - Supplies a pointer to the heap being manipulated        FreeBlock - Supplies a pointer to the block being decommitted        FreeSize - Supplies the size, in heap units, of the free block being decommitted    Return Value:        None.    --*/    
{  
    ULONG_PTR DeCommitAddress;  
    ULONG_PTR DeCommitSize;  
    SIZE_T FreeBlockSize;  
    PHEAP_ENTRY TrailingBlock;  
    PHEAP_FREE_ENTRY LeadingFreeBlock;  
    PHEAP_FREE_ENTRY ListFreeBlock;  
    SIZE_T ListFreeSize;  
    PHEAP_SEGMENT Segment;  
    NTSTATUS Status;  
    PVOID TrailingDeCommitAddress = NULL;  
    SIZE_T TrailingDeCommitSize;  
    BOOLEAN TrailingUCR = FALSE;  
    PHEAP_UCR_DESCRIPTOR UCRBlock;  
  
    //      //  If the heap has a user specified decommit routine then we won't really      //  decommit anything instead we'll call a worker routine to chop it up      //  into pieces that will fit on the free lists      //    
    if (RtlpGetCommitRoutine(Heap) != NULL) {  
  
        RtlpInsertFreeBlock( Heap, FreeBlock, FreeSize );  
  
        return;  
    }  
  
    if (!ForceDecommit) {  
  
        if (FreeSize < Heap->DeCommitFreeBlockThreshold) {  
  
            RtlpInsertFreeBlock( Heap, FreeBlock, FreeSize );  
  
            return;  
        }  
  
        if (((Heap->TotalFreeSize + FreeSize) < Heap->DeCommitTotalFreeThreshold)  
                ||  
             ((Heap->TotalFreeSize + FreeSize) <  
                (Heap->Counters.TotalMemoryCommitted >>  
                  (Heap->TuningParameters.CommittThresholdShift + HEAP_GRANULARITY_SHIFT)))) {  
  
            RtlpInsertFreeBlock( Heap, FreeBlock, FreeSize );  
  
            return;  
        }  
  
        LeadingFreeBlock = RtlpCoalesceFreeBlocks(Heap, FreeBlock, &FreeSize, FALSE);  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
        //          //  Check to see if there is now enough free memory in the heap to          //  justify collecting interior pages. This is the case if there is          //  a significant gap between the amount of committed space and the          //  amount of "busy" space and the amount of busy space does not          //  seem to be declining. These conditions aim to maximize the number          //  of blocks collected at one time, since collection is a somewhat          //  expensive operation.          //    
        if ((FreeSize > (PAGE_SIZE >> HEAP_GRANULARITY_SHIFT))  
                &&  
            (FreeSize <= HEAP_MAXIMUM_BLOCK_SIZE)) {  
  
            SIZE_T TotalMemoryAllocated;  
  
            //              //  Only blocks on the free list will be cleaned up, so ensure              //  that the current block is also eligible.              //    
            RtlpInsertFreeBlock(Heap, LeadingFreeBlock, FreeSize);  
            TotalMemoryAllocated = Heap->Counters.TotalMemoryCommitted - (Heap->TotalFreeSize << HEAP_GRANULARITY_SHIFT);  
            if ((TotalMemoryAllocated <  
                  (Heap->Counters.HighWatermarkSize - (Heap->Counters.HighWatermarkSize >> HEAP_WATERMARK_SHIFT)))  
                    &&  
                (TotalMemoryAllocated >  
                    (Heap->Counters.LastPolledSize - (Heap->Counters.LastPolledSize >> HEAP_LAST_POLLED_SHIFT)))) {  
  
                    RtlpCollectFreeBlocks(Heap);  
                    Heap->Counters.HighWatermarkSize = Heap->Counters.LastPolledSize = TotalMemoryAllocated;  
            }  
  
            return;  
        }  
  
#endif  //  NTOS_KERNEL_RUNTIME    
    } else {  
  
        SIZE_T InBlockDecommitSize;  
        PVOID TempBaseAddress;  
  
        //          // Do not attempt to coalesce with adjacent blocks. Verify that the          // caller-provided free size is at least related to the leading free          // block's size. Note that if this block was just coalesced, it may          // be that the free size is larger than the value that can be held          // in the block's header; do not assert in this case if the bottom          // bits of both values match.          //    
        HEAPASSERT( (USHORT)FreeSize == (USHORT)FreeBlock->Size );  
  
        if (FreeBlock->Flags & HEAP_ENTRY_DECOMMITTED) {  
  
            Heap->Counters.InBlockDeccommits -= 1;  
  
            if ( RtlpGetFreeBlockInsidePageBoundaries( Heap,  
                                                       FreeBlock,  
                                                       &TempBaseAddress,  
                                                       &InBlockDecommitSize ) ) {  
  
                Heap->Counters.InBlockDeccomitSize -= InBlockDecommitSize;  
  
            } else {  
  
                HEAPASSERT( FALSE );  
            }  
        }  
  
        LeadingFreeBlock = FreeBlock;  
    }  
  
    //      //  Get a pointer to the owning segment      //    
    Segment = RtlpGetSegment(Heap, (PHEAP_ENTRY)FreeBlock);  
  
    TrailingBlock =  (PHEAP_ENTRY)LeadingFreeBlock + FreeSize;  
  
    if ( RtlpIsUCRBlock( TrailingBlock ) ) {  
  
        //          //  Coalesce with the trailing UCR block, if any          //    
        UCRBlock = (PHEAP_UCR_DESCRIPTOR)(TrailingBlock + 1);  
        RtlpRemoveUCRBlock( Heap, UCRBlock );  
  
        TrailingDeCommitAddress = UCRBlock->Address;  
        TrailingDeCommitSize = UCRBlock->Size;  
  
        Segment->NumberOfUnCommittedRanges -= 1;  
        Segment->NumberOfUnCommittedPages -= (ULONG)(UCRBlock->Size / PAGE_SIZE);  
        Heap->Counters.TotalMemoryCommitted +=  UCRBlock->Size;  
        Heap->Counters.TotalUCRs -= 1;  
  
        if (UCRBlock->Size >= (HEAP_MAXIMUM_BLOCK_SIZE << HEAP_GRANULARITY_SHIFT)) {  
  
            Heap->Counters.TotalMemoryLargeUCR -=  UCRBlock->Size;  
        }  
  
        FreeSize += sizeof(HEAP_UCR_DESCRIPTOR) + sizeof(HEAP_ENTRY) + (UCRBlock->Size >> HEAP_GRANULARITY_SHIFT);  
        TrailingUCR = TRUE;  
    } else {  
  
        //          //  Safety valve for logging functions          //    
        TrailingDeCommitSize = 0;  
    }  
  
    //      //  We have something to decommit at this point      //    
    if (RtlpGetBlockPreviousSize(Heap, (PHEAP_ENTRY)LeadingFreeBlock) == 0) {  
  
        PHEAP_FREE_ENTRY FreeEntry;  
  
        //          //  We have to coalesce with an existing UCR          //  Note, this cannot be the beginning of the segment, since that will always have a          //  block busy (the segment descriptor)          //    
        DeCommitAddress = (ULONG_PTR)LeadingFreeBlock;  
  
        UCRBlock = RtlpSearchUCRBlock( Segment, LeadingFreeBlock );  
  
        if (ForceDecommit && (UCRBlock == NULL)) {  
  
            //              //  This is a block that failed to be committed inside and              //  is forced now to a real UCR. If this happens while an extend              //  heap call, we might have just committed the UCR before this block              //  and we are about to return a coalesced allocation. There is              //  no previous UCR in that scenario that we need to combine with.              //    
            goto DoDecommit;  
        }  
  
        HEAP_ERROR( HEAP_ERROR_LEVEL_CRITICAL,  
                    (UCRBlock != NULL),  
                    LeadingFreeBlock );  
  
        if (!TrailingUCR) {  
  
            DeCommitSize = ROUND_DOWN_TO_POWER2( (PUCHAR)LeadingFreeBlock + FreeSize * sizeof(HEAP_ENTRY) - sizeof(HEAP_FREE_ENTRY),  
                                                 PAGE_SIZE);  
  
        } else {  
  
            DeCommitSize = ROUND_DOWN_TO_POWER2( (PUCHAR)LeadingFreeBlock + FreeSize * sizeof(HEAP_ENTRY),  
                                                 PAGE_SIZE);  
        }  
  
        DeCommitSize = DeCommitSize - DeCommitAddress;  
  
        if (DeCommitSize == 0) {  
  
            HEAP_ERROR( HEAP_ERROR_LEVEL_CRITICAL,  
                        (!TrailingUCR),  
                        LeadingFreeBlock );  
  
            RtlpInsertFreeBlock( Heap, LeadingFreeBlock, FreeSize );  
  
            return;  
        }  
  
        //          //  Decommit the memory          //    
#pragma prefast(suppress:__WARNING_KERNELUNRELEASEDVADS, "We do want decommitting the memory, not releasing the VAD")  
        Status = RtlpHeapFreeVirtualMemory( NtCurrentProcess(),  
                                            (PVOID *)&DeCommitAddress,  
                                            &DeCommitSize,  
                                            MEM_DECOMMIT );  
  
        if (!NT_SUCCESS(Status)) {  
  
            RtlpUpdateHeapRates( Heap, HEAP_COUNTER_DECOMMIT );  
  
            if (TrailingUCR) {  
  
                //                  //  we failed to decommit the memory. Restore the trailing UCR                  //  and insert the block to the free list                  //    
                RtlpCreateUCREntry( Heap,  
                                    Segment,  
                                    (PHEAP_UCR_DESCRIPTOR)TrailingDeCommitAddress - 1,  
                                    TrailingDeCommitSize,  
                                    LeadingFreeBlock,  
                                    &FreeSize  
                                  );  
            }  
  
            RtlpInsertFreeBlock( Heap, LeadingFreeBlock, FreeSize );  
            return;  
        }  
  
        if (IS_HEAP_LOGGING_ENABLED()) {  
            RtlpLogHeapDecommit( Heap,  
                                 (PVOID)DeCommitAddress,  
                                 DeCommitSize,  
                                 HEAP_LOG_DECOMMIT_FREE_BLOCK);  
  
        }  
  
        Heap->Counters.DeCommitOps += 1;  
  
        //          //  UCRBlock cannot be NULL. OK to reference it w/o test          //  Update the size of the previous uncommitted range          //    
        if (UCRBlock->Size >= (HEAP_MAXIMUM_BLOCK_SIZE << HEAP_GRANULARITY_SHIFT)) {  
  
            Heap->Counters.TotalMemoryLargeUCR -=  UCRBlock->Size;  
        }  
  
        //          //  Insert the UCR in the appropriate order in the list,          //  according to the new size          //    
        RtlpRemoveUCRBlock( Heap, UCRBlock );  
  
        UCRBlock->Size += DeCommitSize;  
  
        RtlpInsertUCRBlock( Heap, UCRBlock );  
  
        Segment->NumberOfUnCommittedPages += (ULONG)(DeCommitSize / PAGE_SIZE);  
        Heap->Counters.TotalMemoryCommitted -=  DeCommitSize;  
  
        if (UCRBlock->Size >= (HEAP_MAXIMUM_BLOCK_SIZE << HEAP_GRANULARITY_SHIFT)) {  
  
            Heap->Counters.TotalMemoryLargeUCR +=  UCRBlock->Size;  
        }  
  
        if (!TrailingUCR) {  
  
            FreeEntry = (PHEAP_FREE_ENTRY)((PCHAR)DeCommitAddress + DeCommitSize);  
  
#pragma prefast(suppress:__WARNING_INCORRECT_ANNOTATION, "When (!TrailingUCR) -> DeCommitSize = ROUND_DOWN_TO_POWER2( (PUCHAR)LeadingFreeBlock + FreeSize * sizeof(HEAP_ENTRY) - sizeof(HEAP_FREE_ENTRY), PAGE_SIZE);, so we should have space for a heap entry at this location. This may be due to bug Esp:674.")  
            RtlpSetBlockPreviousSize(Heap, (PHEAP_ENTRY)FreeEntry, 0);  
  
            if ((PCHAR)((PHEAP_ENTRY)LeadingFreeBlock + FreeSize) != ((PCHAR)DeCommitAddress + DeCommitSize)) {  
  
                //                  //  Create the new free block right after the uncommitted range                  //    
                FreeEntry->UnusedBytes = 0;  
                FreeEntry->Flags = 0;  
                FreeEntry->Size = (USHORT)(((FreeSize << HEAP_GRANULARITY_SHIFT) -  
                                            DeCommitSize) >> HEAP_GRANULARITY_SHIFT );  
  
                HEAP_ERROR( HEAP_ERROR_LEVEL_CRITICAL,  
                            ((LONG)FreeEntry->Size > 1),  
                            FreeEntry );  
  
                RtlpSetSmallTagIndex(Heap, (PHEAP_ENTRY)FreeEntry, 0);  
                RtlpSetBlockOffset( (PHEAP_ENTRY)FreeEntry, Segment );  
                RtlpInsertFreeBlock( Heap, FreeEntry, FreeEntry->Size );  
  
            } else {  
  
                RtlpPackHeapEntry(Heap, (PHEAP_ENTRY)FreeEntry);  
            }  
        }  
  
        if (IS_HEAP_LOGGING_ENABLED()) {  
  
            RtlpLogHeapContractEvent( Heap,  
                                      (PVOID)DeCommitAddress,  
                                      DeCommitSize,  
                                      (Heap->TotalFreeSize) << HEAP_GRANULARITY_SHIFT,  
                                      TrailingUCR,  
                                      TrailingDeCommitSize,  
                                      (HANDLE)HEAP_LOGGER_ID );  
        }  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
        if (IS_HEAPSUMMARY_LOGGING_ENABLED()) {  
  
            RtlpLogHeapContractEvent( Heap,  
                                      (PVOID)DeCommitAddress,  
                                      DeCommitSize,  
                                      (Heap->TotalFreeSize) << HEAP_GRANULARITY_SHIFT,  
                                      TrailingUCR,  
                                      TrailingDeCommitSize,  
                                      (HANDLE)HEAPSUMMARY_LOGGER_ID );  
        }  
  
#endif //!defined(NTOS_KERNEL_RUNTIME)    
        return;  
    }  
  
DoDecommit:  
  
    //      //  Make sure the block we are trying to decommit start on the next full      //  page boundary.  The leading free size is the size of whatever it takes      //  to round up the free block to the next page specified in units of      //  heap entries.      //    
    DeCommitAddress = ROUND_UP_TO_POWER2( (PCHAR)LeadingFreeBlock + sizeof(HEAP_ENTRY) + sizeof(HEAP_UCR_DESCRIPTOR), PAGE_SIZE );  
  
    if ( (PCHAR)DeCommitAddress ==  ((PCHAR)LeadingFreeBlock + 2 * sizeof(HEAP_ENTRY) + sizeof(HEAP_UCR_DESCRIPTOR))) {  
  
        //          //  If we take the space for UCR block there are only sizeof(HEAP_ENTRY) bytes left for          //  the free block (so no space for free list entry.          //  Round up to the next page          //    
        DeCommitAddress += PAGE_SIZE;  
    }  
  
    if (!TrailingUCR) {  
  
        DeCommitSize = ROUND_DOWN_TO_POWER2( (PUCHAR)LeadingFreeBlock + FreeSize * sizeof(HEAP_ENTRY) - sizeof(HEAP_FREE_ENTRY),  
                                             PAGE_SIZE);  
  
    } else {  
  
        DeCommitSize = ROUND_DOWN_TO_POWER2( (PUCHAR)LeadingFreeBlock + FreeSize * sizeof(HEAP_ENTRY),  
                                             PAGE_SIZE);  
    }  
  
    if (DeCommitSize >= DeCommitAddress) {  
  
        DeCommitSize = DeCommitSize - DeCommitAddress;  
  
    } else {  
  
        HEAP_ERROR( HEAP_ERROR_LEVEL_CRITICAL,  
                    (!TrailingUCR),  
                    LeadingFreeBlock );  
  
        RtlpInsertFreeBlock( Heap, LeadingFreeBlock, FreeSize );  
  
        return;  
    }  
  
  
    if (!ForceDecommit) {  
  
        if ((!(RtlpIsUCRBlock( TrailingBlock )))  
                &&  
             ( (DeCommitSize == 0)  
                    ||  
               (DeCommitSize < Heap->DeCommitFreeBlockThreshold))) {  
  
            RtlpInsertFreeBlock( Heap, LeadingFreeBlock, FreeSize );  
  
            return;  
        }  
    }  
  
    if (DeCommitSize) {  
  
        //          //  We have something to decommit at this point          //    
        Heap->Counters.DeCommitOps += 1;  
  
#pragma prefast(suppress:__WARNING_KERNELUNRELEASEDVADS, "We do want decommitting the memory, not releasing the VAD")  
        Status = RtlpHeapFreeVirtualMemory( NtCurrentProcess(),  
                                            (PVOID *)&DeCommitAddress,  
                                            &DeCommitSize,  
                                            MEM_DECOMMIT );  
  
        if (!NT_SUCCESS(Status)) {  
  
            RtlpUpdateHeapRates( Heap, HEAP_COUNTER_DECOMMIT );  
  
            if (TrailingUCR) {  
  
                //                  //  we failed to decommit the memory. Restore the trailing UCR                  //  and insert the block to the free list                  //    
                RtlpCreateUCREntry( Heap,  
                                    Segment,  
                                    (PHEAP_UCR_DESCRIPTOR)TrailingDeCommitAddress - 1,  
                                    TrailingDeCommitSize,  
                                    LeadingFreeBlock,  
                                    &FreeSize  
                                  );  
            }  
  
            RtlpInsertFreeBlock( Heap, LeadingFreeBlock, FreeSize );  
  
            return;  
        }  
  
        if (IS_HEAP_LOGGING_ENABLED()) {  
            RtlpLogHeapDecommit( Heap,  
                                 (PVOID)DeCommitAddress,  
                                 DeCommitSize,  
                                 HEAP_LOG_DECOMMIT_FREE_BLOCK2);  
        }  
    }  
  
    if ( !TrailingUCR ) {  
        PHEAP_FREE_ENTRY FreeEntry;  
  
        //          //  Create the new free block right after the uncommitted range          //    
        FreeEntry = (PHEAP_FREE_ENTRY)((PCHAR)DeCommitAddress + DeCommitSize);  
  
#pragma prefast(suppress:__WARNING_INCORRECT_ANNOTATION, "When (!TrailingUCR) -> DeCommitSize = ROUND_DOWN_TO_POWER2( (PUCHAR)LeadingFreeBlock + FreeSize * sizeof(HEAP_ENTRY) - sizeof(HEAP_FREE_ENTRY), PAGE_SIZE);, so we should have space for a heap entry at this location. This may be due to bug Esp:674.")  
        RtlpSetBlockPreviousSize(Heap, (PHEAP_ENTRY)FreeEntry, 0);  
  
        if ((PCHAR)((PHEAP_ENTRY)LeadingFreeBlock + FreeSize) != ((PCHAR)DeCommitAddress + DeCommitSize)) {  
  
            FreeEntry->UnusedBytes = 0;  
            FreeEntry->Flags = 0;  
            FreeEntry->Size = (USHORT)(((FreeSize << HEAP_GRANULARITY_SHIFT) -  
                                        ((ULONG_PTR)DeCommitAddress - (ULONG_PTR)LeadingFreeBlock) -  
                                        DeCommitSize) >> HEAP_GRANULARITY_SHIFT );  
  
            HEAP_ERROR ( HEAP_ERROR_LEVEL_CRITICAL,  
                         (LONG)FreeEntry->Size > 1,  
                         FreeEntry );  
  
            RtlpSetSmallTagIndex( Heap, (PHEAP_ENTRY)FreeEntry, 0 );  
            RtlpSetBlockOffset( (PHEAP_ENTRY)FreeEntry, Segment );  
            RtlpInsertFreeBlock( Heap, FreeEntry, FreeEntry->Size );  
  
        } else {  
  
            // DbgBreakPoint();              RtlpPackHeapEntry(Heap, (PHEAP_ENTRY)FreeEntry);  
  
        }  
    }  
  
    RtlpCreateUCREntry( Heap,  
                        Segment,  
                        (PHEAP_UCR_DESCRIPTOR)DeCommitAddress - 1,  
                        DeCommitSize,  
                        LeadingFreeBlock,  
                        &FreeBlockSize  
                      );  
  
    RtlpInsertFreeBlock( Heap, LeadingFreeBlock, FreeBlockSize );  
  
    if (IS_HEAP_LOGGING_ENABLED()) {  
        RtlpLogHeapContractEvent( Heap,  
                                (PVOID)DeCommitAddress,  
                                DeCommitSize,  
                                (Heap->TotalFreeSize) << HEAP_GRANULARITY_SHIFT,  
                                FALSE,  
                                0,  
                                (HANDLE)HEAP_LOGGER_ID );  
    }  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
    if (IS_HEAPSUMMARY_LOGGING_ENABLED()) {  
  
        RtlpLogHeapContractEvent( Heap,  
                                 (PVOID)DeCommitAddress,  
                                 DeCommitSize,  
                                 (Heap->TotalFreeSize) << HEAP_GRANULARITY_SHIFT,  
                                 FALSE,  
                                 0,  
                                 (HANDLE)HEAPSUMMARY_LOGGER_ID );  
    }  
  
#endif  
  
}  
  
  
//  //  Declared in heappriv.h  //    
VOID  
RtlpInsertFreeBlock (  
    __inout PHEAP Heap,  
    __inout PHEAP_FREE_ENTRY FreeBlock,  
    __in SIZE_T FreeSize  
    )  
  
/*++    Routine Description:        This routines take a piece of committed memory and adds to the      the appropriate free lists for the heap.  If necessary this      routine will divide up the free block to sizes that fit      on the free list      Arguments:        Heap - Supplies a pointer to the owning heap        FreeBlock - Supplies a pointer to the block being freed        FreeSize - Supplies the size, in bytes, of the block being freed    Return Value:        None.    --*/    
{  
    UCHAR Flags;  
    USHORT PreviousSize;  
    PHEAP_SEGMENT Segment;  
    USHORT Size;  
  
    if (FreeSize == 0) {  
        return;  
    }  
  
    //      //  Get the size of the previous block, the index of the segment      //  containing this block, and the flags specific to the block      //    
    PreviousSize = RtlpGetBlockPreviousSize(Heap, (PHEAP_ENTRY)FreeBlock);  
  
    //      //  Blocks preceded by uncommitted regions normally need page-aligned      //  block headers. However, in low-memory conditions, it is possible      //  for a block that has just been converted to a UCR to be passed to      //  this function. The block's previous size field is not necessarily      //  valid in this case. Do not assert if the interior decommit flag is      //  set, since that signals this case and all will be well after the      //  insertion of this block is completed.      //    
    if (PreviousSize == 0) {  
  
        HEAP_ERROR ( HEAP_ERROR_LEVEL_CRITICAL,  
                     ((FreeBlock->Flags & HEAP_ENTRY_DECOMMITTED)  
                        ||  
                     (ROUND_UP_TO_POWER2(FreeBlock, PAGE_SIZE) == (ULONG_PTR)FreeBlock)),  
                     FreeBlock );  
    }  
  
    Segment = RtlpGetSegment( Heap, (PHEAP_ENTRY)FreeBlock );;  
  
    Flags = FreeBlock->Flags;  
  
    //      //  Now, while there is still something left to add to the free list      //  we'll process the information      //    
    while (FreeSize != 0) {  
  
        //          //  If the size is too big for our free lists then we'll          //  chop it down.          //    
        if (FreeSize > (ULONG)HEAP_MAXIMUM_BLOCK_SIZE) {  
  
            Size = HEAP_MAXIMUM_BLOCK_SIZE;  
  
            //              //  This little adjustment is so that we don't have a remainder              //  that is too small to be useful on the next iteration              //  through the loop              //    
            if (FreeSize == ((ULONG)HEAP_MAXIMUM_BLOCK_SIZE + 1)) {  
  
                Size -= 16;  
            }  
  
            //              //  Guarantee that Last entry does not get set in this              //  block.              //    
            FreeBlock->Flags = 0;  
  
        } else {  
  
            Size = (USHORT)FreeSize;  
  
            //              //  This could propagate the last entry flag              //    
            FreeBlock->Flags = Flags;  
        }  
  
        //          //  Update the block sizes and then insert this          //  block into a free list          //    
        RtlpSetBlockPreviousSize(Heap, (PHEAP_ENTRY)FreeBlock, PreviousSize);  
  
        RtlpSetBlockOffset((PHEAP_ENTRY)FreeBlock, Segment);  
  
        FreeBlock->Size = Size;  
        FreeBlock->UnusedBytes = 0;  
        FreeBlock->Flags &= ~HEAP_ENTRY_DECOMMITTED;  
        RtlpSetSmallTagIndex( Heap, (PHEAP_ENTRY)FreeBlock, 0 );  
  
        RtlpInsertFreeBlockDirect( Heap, FreeBlock, Size );  
  
        //          //  Note the size of what we just freed, and then update          //  our state information for the next time through the          //  loop          //    
        PreviousSize = Size;  
  
        FreeSize -= Size;  
  
        FreeBlock = (PHEAP_FREE_ENTRY)((PHEAP_ENTRY)FreeBlock + Size);  
  
        //          //  Check if we're done with the free block based on the          //  segment information, otherwise go back up and check size          //  Note that is means that we can get called with a very          //  large size and still work.          //    
        if ((PHEAP_ENTRY)FreeBlock >= Segment->LastValidEntry) {  
  
            return;  
        }  
    }  
  
    //      //  Update the previous of the last block      //    
    RtlpSetBlockPreviousSize( Heap,  
                              (PHEAP_ENTRY)FreeBlock,  
                              PreviousSize );  
  
    if (PreviousSize == 0) {  
  
        HEAP_ERROR ( HEAP_ERROR_LEVEL_CRITICAL,  
                     ROUND_UP_TO_POWER2(FreeBlock, PAGE_SIZE) == (ULONG_PTR)FreeBlock,  
                     FreeBlock );  
    }  
  
    //      //  And return to our caller      //    
    return;  
}  
  
PHEAP_ENTRY_EXTRA  
RtlpGetExtraStuffPointer (  
    __in PHEAP_ENTRY BusyBlock  
    )  
  
/*++    Routine Description:        This routine calculates where the extra stuff record will be given      the busy block and returns a pointer to it.  The caller must have      already checked that the entry extra field is present    Arguments:        Heap - Supplies the heap handle        BusyBlock - Supplies the busy block whose extra stuff we are seeking    Return Value:        PHEAP_ENTRY_EXTRA - returns a pointer to the extra stuff record.    --*/    
{  
    ULONG AllocationIndex;  
  
    //      //  On big blocks the extra stuff is automatically part of the      //  block      //    
    if (RtlpIsVirtualBlock( BusyBlock )) {  
  
        PHEAP_VIRTUAL_ALLOC_ENTRY VirtualAllocBlock;  
  
        VirtualAllocBlock = CONTAINING_RECORD( BusyBlock, HEAP_VIRTUAL_ALLOC_ENTRY, BusyBlock );  
  
        return &VirtualAllocBlock->ExtraStuff;  
  
    } else {  
  
        //          //  On non big blocks the extra stuff follows immediately after          //  the allocation itself.          //          //  We do some funny math here because the busy block          //  stride is 8 bytes we know we can stride it by its          //  index minus one to get to the end of the allocation          //    
        AllocationIndex = BusyBlock->Size;  
  
        return (PHEAP_ENTRY_EXTRA)(BusyBlock + AllocationIndex - 1);  
    }  
}  
  
SIZE_T  
RtlpGetSizeOfBigBlock (  
    __in PHEAP Heap,  
    __in PHEAP_ENTRY BusyBlock  
    )  
  
/*++    Routine Description:        This routine returns the size, in bytes, of the big allocation block    Arguments:        BusyBlock - Supplies a pointer to the block being queried    Return Value:        SIZE_T - Returns the size, in bytes, that was allocated to the big          block    --*/    
{  
    PHEAP_VIRTUAL_ALLOC_ENTRY VirtualAllocBlock;  
  
    //      //  Get a pointer to the block header itself      //    
    VirtualAllocBlock = CONTAINING_RECORD( BusyBlock, HEAP_VIRTUAL_ALLOC_ENTRY, BusyBlock );  
  
    //      //  The size allocated to the block is actually the difference between the      //  commit size stored in the virtual alloc block and the size stored in      //  in the block.      //    
#pragma prefast(suppress:__WARNING_BUFFER_UNDERFLOW, "A HEAP_VIRTUAL_ALLOC_ENTRY structure always preceeds a virtual block")  
    return VirtualAllocBlock->CommitSize - RtlpQueryBlockAllocationUnits(Heap, BusyBlock);  
}  
  
BOOLEAN  
RtlpCheckBusyBlockTail (  
    __in PHEAP Heap,  
    __inout PHEAP_ENTRY BusyBlock  
    )  
  
/*++    Routine Description:        This routine checks to see if the bytes beyond the user specified      allocation have been modified.  It does this by checking for a tail      fill pattern    Arguments:        BusyBlock - Supplies the heap block being queried    Return Value:        BOOLEAN - TRUE if the tail is still okay and FALSE otherwise    --*/    
{  
    SIZE_T cbEqual;  
    SIZE_T Size;  
    PCHAR Tail;  
  
    if (RtlpIsMetadataBlock( BusyBlock )) {  
  
        //          //  Do not check the signature for the heap metadata since          //  the unused bytes are overloaded to not reflect the actual size          //    
        return TRUE;  
    }  
  
    //      //  Compute the user allocated size of the input heap block      //    
    if (RtlpIsVirtualBlock( BusyBlock )) {  
  
        //          //  Otherwise if the block is from our large allocation then          //  we'll get the result from that routine          //    
        Size = RtlpGetSizeOfBigBlock( Heap, BusyBlock );  
  
    } else {  
  
        //          //  Otherwise the block must be one that we can handle so          //  calculate its block size and then subtract what's not being          //  used by the caller.          //          //  Note: This includes the heap entry header in its calculation.          //    
        Size = (((SIZE_T)RtlpGetAllocationUnits(Heap, BusyBlock)) << HEAP_GRANULARITY_SHIFT) -  
               RtlpGetUnusedBytes(Heap, BusyBlock);  
    }  
  
    Size += RtlpGetExtendedDataSize(BusyBlock + 1);  
  
    //      //  Compute a pointer to the tail of the input block.  This would      //  be the space right after the user allocated portion      //    
    Tail = (PCHAR)(BusyBlock + 1) + Size;  
  
    //      //  Check if the tail fill pattern is still there      //    
    cbEqual = RtlCompareMemory( Tail,  
                                CheckHeapFillPattern,  
                                CHECK_HEAP_TAIL_SIZE );  
  
    //      //  If the number we get back isn't equal to the tail size then      //  someone modified the block beyond its user specified allocation      //  size      //    
    if (cbEqual != CHECK_HEAP_TAIL_SIZE) {  
  
        //          //  Do some debug printing          //    
        HeapDebugPrint(( "Heap block at %p modified at %p past requested size of %Ix\n",  
                         BusyBlock,  
                         Tail + cbEqual,  
                         Size ));  
  
        HeapDebugBreak( BusyBlock );  
  
        //          //  And tell our caller there was an error          //    
        return FALSE;  
  
    } else {  
  
        //          //  And return to our caller that the tail is fine          //    
        return TRUE;  
    }  
}  
  
ULONG  
RtlpHeapExceptionFilter (  
    __in NTSTATUS ExceptionCode  
    )  
  
/*++    Routine Description:        This routine is the exception filter used by heap operations.    Arguments:        ExceptionCode - exception code        ExceptionRecord - structure with pointers to .exr and .cxr    Return Value:        EXCEPTION_CONTINUE_SEARCH for deadlock and stack overflow exception        EXCEPTION_EXECUTE_HANDLER otherwise    --*/    
{  
    if ((ExceptionCode == STATUS_STACK_OVERFLOW)  
            ||  
        (ExceptionCode == STATUS_POSSIBLE_DEADLOCK)  
            ||  
        (ExceptionCode == STATUS_NO_MEMORY)) {  
  
        return EXCEPTION_CONTINUE_SEARCH;  
    }  
  
    return EXCEPTION_EXECUTE_HANDLER;  
}  
  
VOID  
RtlpCreateHeapEncoding (  
    __inout PHEAP Heap  
    )  
  
/*++        Routine Description:            The function sets up the encoding structure for the block metadata encoding.        Arguments:            Heap - Supplies the heap handle        Return Value:            None.    --*/    
{  
    if (((Heap->CompatibilityFlags & HEAP_COMPAT_DISABLE_ENCODING) == 0)  
            &&  
        ((Heap->Flags & HEAP_TAGGING_ENABLED) == 0)) {  
  
        Heap->Encoding.Code1 = 0;  
  
        //          //  Force the HEAP_ENTRY_IN_CONSISTENT_STATE flag to be set          //    
        Heap->Encoding.Flags |= HEAP_ENTRY_IN_CONSISTENT_STATE;  
        Heap->EncodeFlagMask = Heap->Encoding.Code1;  
  
        //          //  ISSUE: Choose some random numbers at release. Leave the header not actually          //  encoded in the first version to make simpler debugging the issues          //    
#if !defined(NTOS_KERNEL_RUNTIME)  
  
        Heap->Encoding.Code1 |= (ULONG)RtlpHeapGenerateRandomValue64();  
        Heap->Encoding.Code2 = (USHORT)RtlpHeapGenerateRandomValue64();  
  
#else  
  
        //          // N.B.  Marked as user domain due to the fact that the win32k desktop          //       heap overlays these allocations into the user mode address          //       space (unfortunately).          //    
        Heap->Encoding.Code1 |= ExGenRandom(ExGenRandomDomainUserVisible);  
        Heap->Encoding.Code2 = (USHORT)ExGenRandom(ExGenRandomDomainUserVisible);  
  
#endif  
  
        //          //  Code 3 and 4 must be 0 (flags and segment offset are assumed uncoded.          //    
        Heap->Encoding.Code3 = 0;  
        Heap->Encoding.Code4 = 0;  
    }  
}  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
VOID  
RtlpPerformHeapMaintenance(  
    __in PHEAP Heap  
    )  
{  
    if (Heap->CompatibilityFlags & HEAP_MAINTENANCE_ENABLE_LFH) {  
  
        Heap->CompatibilityFlags &= ~HEAP_MAINTENANCE_ENABLE_LFH;  
  
        if (!(RtlpDisableHeapLookaside & HEAP_COMPAT_DISABLE_LOOKASIDES)) {  
  
            RtlpActivateLowFragmentationHeap( Heap );  
        }  
    }  
  
    //      // Create the UCR index table if it does not yet exist and its flag is      // set. Note that the flag must be cleared before the operation to      // prevent it from recursing infinitely, since the table extension will      // also allocate memory and perform heap maintenance. The flag must also      // be cleared afterward to ensure that the table is created only once.      //    
    if (Heap->CompatibilityFlags & HEAP_MAINTENANCE_EXTEND_UCR_INDEX) {  
        Heap->CompatibilityFlags &= ~HEAP_MAINTENANCE_EXTEND_UCR_INDEX;  
        RtlpInitializeUCRIndex(Heap);  
        Heap->CompatibilityFlags &= ~HEAP_MAINTENANCE_EXTEND_UCR_INDEX;  
    }  
}  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
VOID  
RtlpUpdateHeapWatermarks (  
    __in PHEAP Heap  
    )  
  
/*++    Routine Description:        This routine polls a heap's usage data to allow the heap manager to      determine the relationship between its current commit and current      allocation size. It also examines the ratio of interior decommits to      mainline heap operations and decreases the interior decommit rate if      this ratio is too high.        The behavior of this function is controlled by a series of defined      constants that can be tweaked to adjust the policy. See the definitions      of these constants for more information.        To ensure correctness and scalability, this function should only be      called by threads that own the heap lock.    Arguments:        Heap - Supplies a pointer to the heap structure to survey.    Return Value:        None.    --*/    
{  
    ULONG DecommitShift;  
    SIZE_T TotalMemoryAllocated;  
  
    //      // Interior decommit is not a valid operation for page heaps.      // If this heap is not a page heap, decide whether any of the      // heap's statistics or controls need to be updated.      //    
    if (!IS_DEBUG_PAGE_HEAP_HANDLE((PVOID)Heap)) {  
        Heap->Counters.PollIntervalCounter += 1;  
  
        //          // If enough mainline heap operations (allocates or frees) have          // occurred, approximate the current allocated size in the heap          // and update the appropriate watermarks.          //    
        if (Heap->Counters.PollIntervalCounter > Heap->Counters.HeapPollInterval) {  
            Heap->Counters.PollIntervalCounter = 0;  
            TotalMemoryAllocated =  
                Heap->Counters.TotalMemoryCommitted - (Heap->TotalFreeSize << HEAP_GRANULARITY_SHIFT);  
  
            if (TotalMemoryAllocated > Heap->Counters.HighWatermarkSize) {  
                Heap->Counters.HighWatermarkSize = TotalMemoryAllocated;  
            }  
  
            Heap->Counters.LastPolledSize = TotalMemoryAllocated;  
        }  
  
        //          // Decide if the interior decommit rate should be reduced. Because          // the heap watermark statistics are only updated when the heap poll          // counter reaches the desired interval, and because those statistics          // are reset each time an interior decommit occurs, the decommit rate          // can be artificially reduced by increasing the number of operations          // that must occur between watermark updates.          //    
        Heap->Counters.AllocAndFreeOps += 1;  
        if (Heap->Counters.AllocAndFreeOps >= HEAP_DECOMMIT_RATE_INTERVAL) {  
  
            //              // The polling interval should increase (thus decreasing the              // decommit rate) if the ratio of decommits to mainline heap              // operations is too high. Experiments show that the threshold              // ratio should be much higher for heaps that either lack an              // LFH or are not using the LFH to handle most of their              // allocations. Check for these conditions and choose the              // appropriate ratio factor.              //    
            if ((Heap->FrontEndHeapType == HEAP_FRONT_LOWFRAGHEAP) &&  
                (Heap->Counters.AllocationIndicesActive > HEAP_MINIMUM_INDICES_ACTIVE)) {  
  
                DecommitShift = HEAP_LFH_DECOMMIT_SHIFT;  
  
            } else {  
                DecommitShift = HEAP_NO_LFH_DECOMMIT_SHIFT;  
            }  
  
            //              // If the ratio is too high, double the watermark polling              // interval.              //    
            if (Heap->Counters.DecommitsSinceLastCheck >  
                    (ULONG)(HEAP_DECOMMIT_RATE_INTERVAL >> DecommitShift)) {  
  
                if (Heap->Counters.HeapPollInterval < HEAP_MAX_POLL_INTERVAL) {  
                    Heap->Counters.HeapPollInterval *= 2;  
                }  
            }  
  
            Heap->Counters.DecommitsSinceLastCheck = 0;  
            Heap->Counters.AllocAndFreeOps = 0;  
        }  
    }  
}  
  
#endif  
  
BOOLEAN  
FASTCALL  
RtlpGetFreeBlockInsidePageBoundaries (  
    __in PHEAP Heap,  
    __in PHEAP_FREE_ENTRY FreeBlock,  
    __deref_out PVOID *BaseAddress,  
    __out PSIZE_T Size  
    )  
  
/*++        Routine Description:            The function determines the potential space that may be      decommitted / committed inside a free block.        Arguments:            Heap - supplies the heap handle            FreeBlock - Supplies the free block structure            BaseAddres - Receives the base address inside the free block              that could be decommitted            Size - Receives the number of bytes that can be decommitted                 inside the free block        Return Value:            TRUE if a non-empty range is available for committ / decommitt          operations, FALSE otherwise    --*/    
{  
    PVOID EndAddress;  
    PHEAP_FREE_ENTRY_EXTRA FreeExtra;  
    PHEAP_ENTRY HeapEntry = (PHEAP_ENTRY)FreeBlock;  
  
    //      //  Make sure in chk builds that the entry is decoded,      //  and we own the heap lock      //    
    HEAPASSERT( RtlpIsHeapLocked(Heap) );  
    HEAPASSERT( !(HeapEntry->Code1 & Heap->EncodeFlagMask) );  
  
    //      //  Determine the closer page boundary from the free block      //    
    //      //  On low memory condition, the decommitted free block might have to be      //  converted to an UCR. Make sure we have enough space to succeed the conversion      //    
    *BaseAddress = (PVOID)ROUND_UP_TO_POWER2( (PCHAR)FreeBlock + sizeof(HEAP_ENTRY) + sizeof(HEAP_UCR_DESCRIPTOR), PAGE_SIZE );  
  
    if ( (PCHAR)*BaseAddress == ((PCHAR)FreeBlock + 2 * sizeof(HEAP_ENTRY) + sizeof(HEAP_UCR_DESCRIPTOR))) {  
  
        //          //  If we take the space for UCR block there are only sizeof(HEAP_ENTRY) bytes left for          //  the free block (so no space for free list entry.          //  Round up to the next page          //    
        *BaseAddress = (PVOID)((ULONG_PTR)(*BaseAddress) + PAGE_SIZE);  
    }  
  
    //      //  Determine the end of the free block. Take in account the possible extra information      //    
    EndAddress = (PVOID)ROUND_DOWN_TO_POWER2( (PUCHAR)HeapEntry + (ULONG)HeapEntry->Size * sizeof(HEAP_ENTRY) - sizeof(HEAP_FREE_ENTRY),  
                                              PAGE_SIZE);  
  
    //      //  If we have an unempty range, proceed with decommitting the block      //    
    if (EndAddress > (*BaseAddress)) {  
  
        *Size = (SIZE_T)EndAddress - (SIZE_T)(*BaseAddress);  
  
        return TRUE;  
    }  
  
    return FALSE;  
}  
  
VOID  
FASTCALL  
RtlpDecommitBlock (  
    __in PHEAP Heap,  
    __inout PHEAP_FREE_ENTRY FreeBlock  
    )  
  
/*++        Routine Description:        The function decommits the unused space inside of a free block.        Arguments:            Heap - Supplies the heap owning the free block            FreeBlock - Supplies the free block to be decommitted        Return Value:            None.    --*/    
{  
    PVOID BaseAddress;  
    SIZE_T Size = FreeBlock->Size;  
    NTSTATUS Status;  
  
    FreeBlock->Flags &= ~(HEAP_ENTRY_FILL_PATTERN |  
                     HEAP_ENTRY_EXTRA_PRESENT |  
                     HEAP_ENTRY_BUSY);  
  
    FreeBlock->UnusedBytes = 0;  
  
    if (Heap->Flags & HEAP_FREE_CHECKING_ENABLED) {  
  
        RtlFillMemoryUlong( (PCHAR)(FreeBlock + 1),  
                            (Size << HEAP_GRANULARITY_SHIFT) -  
                                sizeof( *FreeBlock ),  
                            FREE_HEAP_FILL );  
  
        FreeBlock->Flags |= HEAP_ENTRY_FILL_PATTERN;  
    }  
  
    //      //  Never decommitt a block for heaps which have provided committ routine      //    
    if (RtlpGetCommitRoutine(Heap) == NULL) {  
  
        HEAPASSERT(!(FreeBlock->Flags & HEAP_ENTRY_DECOMMITTED));  
  
        //          //  Check whether there is a non-empty range inside the blocks or not          //    
        if ( RtlpGetFreeBlockInsidePageBoundaries( Heap,  
                                                   FreeBlock,  
                                                   &BaseAddress,  
                                                   &Size ) ) {  
  
            //              //  We found a valid range inside the block. Go ahead and decommit              //  the content of the free block              //    
            Status = RtlpHeapFreeVirtualMemory( NtCurrentProcess(),  
                                                (PVOID *)&BaseAddress,  
                                                &Size,  
                                                MEM_DECOMMIT );  
  
            if (NT_SUCCESS(Status)) {  
  
                //                  //  We cannot preserve the fill pattern since the memory is getting decommitted                  //  If an application is touching this memory will AV actually                  //    
                if (IS_HEAP_LOGGING_ENABLED()) {  
                    RtlpLogHeapDecommit( Heap,  
                                         BaseAddress,  
                                         Size,  
                                         HEAP_LOG_DECOMMIT_BLOCK);  
  
                }  
  
                RtlpUpdateHeapRates( Heap, HEAP_COUNTER_DECOMMIT );  
                Heap->Counters.InBlockDeccommits += 1;  
                Heap->Counters.InBlockDeccomitSize += Size;  
                Heap->Counters.DecommitsSinceLastCheck += 1;  
  
                if (IS_HEAP_LOGGING_ENABLED()) {  
                    RtlpLogHeapContractEvent( Heap,  
                                              (PVOID)BaseAddress,  
                                              Size,  
                                              (Heap->TotalFreeSize) << HEAP_GRANULARITY_SHIFT,  
                                              FALSE,  
                                              0,  
                                              (HANDLE)HEAP_LOGGER_ID);  
                }  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
                if (IS_HEAPSUMMARY_LOGGING_ENABLED()) {  
                    RtlpLogHeapContractEvent( Heap,  
                                             (PVOID)BaseAddress,  
                                             Size,  
                                             (Heap->TotalFreeSize) << HEAP_GRANULARITY_SHIFT,  
                                             FALSE,  
                                             0,  
                                             (HANDLE)HEAPSUMMARY_LOGGER_ID);  
                }  
  
#endif  
  
                FreeBlock->Flags &= ~(HEAP_ENTRY_FILL_PATTERN | HEAP_ENTRY_SETTABLE_FLAGS);  
                FreeBlock->Flags |= HEAP_ENTRY_DECOMMITTED;  
  
            } else {  
  
                //                  //  This should never fail. Assert on chk builds                  //    
                HeapDebugPrint(( "RtlpHeapFreeVirtualMemory failed %lx for heap %p (base %p, size %Ix)\n",  
                                 Status,  
                                 Heap,  
                                 BaseAddress,  
                                 Size));  
  
                //  HEAPASSERT( NT_SUCCESS(Status) );              }  
        }  
    }  
  
    RtlpPackHeapEntry(Heap, (PHEAP_ENTRY)FreeBlock);  
    RtlpValidateHeapLists(Heap);  
}  
  
BOOLEAN  
FASTCALL  
RtlpCommitBlock (  
    __in PHEAP Heap,  
    __inout PHEAP_FREE_ENTRY FreeBlock  
    )  
  
/*++        Routine Description:            The function commits entirely the space inside      of a free block, previously decommitted with RtlpDecommitBlock        Arguments:            Heap - Supplies the heap owning the free block            FreeBlock - Supplies the free block to be committed        Return Value:            Returns TRUE if the memory is successfuly committed, FALSE otherwise      NOTE that on failure case, the caller needs to convert this free block to a true UCR      (which is always guaranteed to succeed since the appropriate space for conversion      has been reserved)    --*/    
{  
    PVOID BaseAddress;  
    BOOLEAN InteriorPagePresent;  
    SIZE_T Size;  
    NTSTATUS Status;  
  
    HEAPASSERT(FreeBlock->Flags & HEAP_ENTRY_DECOMMITTED);  
  
    InteriorPagePresent =  RtlpGetFreeBlockInsidePageBoundaries( Heap,  
                                                                 FreeBlock,  
                                                                 &BaseAddress,  
                                                                 &Size );  
  
    HEAPASSERT(InteriorPagePresent);  
  
    Status = RtlpAllocateVirtualMemoryHeap( NtCurrentProcess(),  
                                            (PVOID *)&BaseAddress,  
                                            0,  
                                            &Size,  
                                            MEM_COMMIT,  
                                            RtlpGetHeapProtection(Heap->Flags) );  
  
    if (NT_SUCCESS(Status)) {  
  
        if (IS_HEAP_LOGGING_ENABLED()) {  
            RtlpLogHeapCommit( Heap,  
                               BaseAddress,  
                               Size,  
                               HEAP_LOG_COMMIT_BLOCK);  
  
        }  
  
        Heap->Counters.InBlockDeccommits -= 1;  
        Heap->Counters.InBlockDeccomitSize -= Size;  
  
        if (IS_HEAP_LOGGING_ENABLED()) {  
            RtlpLogHeapExtendEvent( Heap,  
                                    (PVOID)BaseAddress,  
                                    Size,  
                                    (Heap->TotalFreeSize) << HEAP_GRANULARITY_SHIFT,  
                                    (HANDLE)HEAP_LOGGER_ID );  
        }  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
        if (IS_HEAPSUMMARY_LOGGING_ENABLED()) {  
            RtlpLogHeapExtendEvent( Heap,  
                                       (PVOID)BaseAddress,  
                                       Size,  
                                       (Heap->TotalFreeSize) << HEAP_GRANULARITY_SHIFT,  
                                       (HANDLE)HEAPSUMMARY_LOGGER_ID );  
        }  
  
#endif // NTOS_KERNEL_RUNTIME    
        RtlpUpdateHeapRates( Heap, HEAP_COUNTER_COMMIT );  
  
        //          //  Clear the decommitt flag for the current block, and any additional information          //  we might have changed while handling the internal free block header          //    
        if (FreeBlock->Flags & HEAP_ENTRY_FILL_PATTERN) {  
  
            RtlFillMemoryUlong( BaseAddress,  
                                Size,  
                                FREE_HEAP_FILL );  
  
        }  
  
        FreeBlock->Flags &= ~(HEAP_ENTRY_DECOMMITTED | HEAP_ENTRY_SETTABLE_FLAGS);  
  
    } else {  
  
        Heap->Counters.InBlockCommitFailures += 1;  
  
        HeapDebugPrint(( "ZwAllocateVirtualMemory failed %lx for heap %p (base %p, size %p)\n",  
                         Status,  
                         Heap,  
                         BaseAddress,  
                         Size));  
  
        return FALSE;  
    }  
  
  
    return TRUE;  
}  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
VOID  
FASTCALL  
RtlpCollectFreeBlocks (  
    __in PHEAP Heap  
    )  
  
/*++        Routine Description:            Walks the free list for a given heap and decommits the interiors          of those free blocks with committed interior regions that are at          least a page in size.        Arguments:            Heap - Supplies a pointer to the heap to be trimmed.        Return Value:            None.    --*/    
{  
    PHEAP_FREE_ENTRY Entry;  
    PLIST_ENTRY Head;  
    PLIST_ENTRY Next;  
  
    Head = &Heap->FreeLists;  
    Next = Head->Blink;  
  
    while (Next != Head) {  
        Entry = CONTAINING_RECORD( Next, HEAP_FREE_ENTRY, FreeList );  
  
        //          //  All the entries on the free list are encoded and must          //  be decoded before being used.          //    
        RtlpUnpackHeapEntry(Heap, (PHEAP_ENTRY)Entry);  
  
        //          // Only entries that are greater than a page in size and already          // committed have any chance of being decommitted.          //    
        if (Entry->Size <= (PAGE_SIZE >> HEAP_GRANULARITY_SHIFT)) {  
            RtlpPackHeapEntry(Heap, (PHEAP_ENTRY)Entry);  
            break;  
        }  
  
        //          // N.B The decommit operation packs the entry being decommitted.          //    
        if ((Entry->Flags & HEAP_ENTRY_DECOMMITTED) == 0) {  
            RtlpDecommitBlock(Heap, Entry);  
        } else {  
            RtlpPackHeapEntry(Heap, (PHEAP_ENTRY)Entry);  
        }  
  
        Next = Next->Blink;  
    }  
}  
  
NTSTATUS  
NTAPI  
RtlpFlushHeapsCallback (  
    PHEAP Heap,  
    PVOID Parameter  
    )  
  
/*++    Routine Description:        Flushes the LFH caches and decommits interior regions for the given heap.    Arguments:        Heap - Supplies a heap to flush.        Parameter - Unused.    Return Value:        None.    --*/    
{  
  
    PVOID LowFragmentationHeap;  
  
    UNREFERENCED_PARAMETER(Parameter);  
  
    if (((Heap->Flags & HEAP_NO_SERIALIZE) == 0) &&  
        (RtlTryAcquireLockRoutine(Heap->LockVariable) != FALSE)) {  
  
        LowFragmentationHeap = RtlpGetLowFragHeap(Heap);  
        if (LowFragmentationHeap != NULL) {  
            RtlpLowFragHeapFlushCaches(LowFragmentationHeap);  
        }  
  
        RtlpCollectFreeBlocks(Heap);  
        RtlReleaseLockRoutine(Heap->LockVariable);  
    }  
  
    return STATUS_SUCCESS;  
}  
  
VOID  
RtlFlushHeaps (  
    VOID  
    )  
  
/*++    Routine Description:        Flushes the LFH caches and decommits interior regions for all heaps.    Arguments:        None.    Return Value:        None.    --*/    
{  
    NT_VERIFY(NT_SUCCESS(RtlEnumProcessHeaps(RtlpFlushHeapsCallback, NULL)));  
  
    return;  
}  
  
#endif  
  
#ifdef _HEAP_DEBUG  
  
UCHAR  
FASTCALL  
RtlpTestCommitFreeBlock (  
    __in PHEAP Heap,  
    __inout PHEAP_FREE_ENTRY FreeBlock  
    )  
  
/*++        Routine Description:            Debug routine to validate that a free block is entirely committed.        Arguments:            Heap - supplies the heap owning the free block            FreeBlock - Supplies the block to be tested        Return Value:            Arbitrary value    --*/    
{  
    PUCHAR BaseAddress, EndAddress;  
    UCHAR Result = 0;  
  
    BaseAddress = (PUCHAR)FreeBlock;  
    EndAddress = (PUCHAR)((PHEAP_ENTRY)FreeBlock + FreeBlock->Size);  
  
    while (BaseAddress < EndAddress) {  
  
        //          //  Dummy operation to detect invalid read memory access          //    
        Result ^= *BaseAddress;  
        BaseAddress += PAGE_SIZE;  
    }  
  
    return Result;  
}  
  
#endif  //  _HEAP_DEBUG    
#endif // NTOS_KERNEL_RUNTIME    
BOOLEAN  
FASTCALL  
RtlpCreateSplitBlock (  
    __in PHEAP Heap,  
    __in PHEAP_SEGMENT Segment,  
    __out PHEAP_FREE_ENTRY SplitBlock,  
    __in UCHAR FreeFlags,  
    __in BOOLEAN PatternCheck,  
    __in USHORT PreviousBlockSize,  
    __in SIZE_T FreeSize  
    )  
  
/*++        Routine Description:            The function splits initialize a new block as results      from splitting a bigger block in two chunks. This function      is called from both alloc and realloc procedures.        Arguments:            Heap - Supplies the heap handle            Segment - Supplies the segment containing the block            SplitBlock - Supplies the original heap entry that needs to get splitted            FreeFlags - Supplies the flags for the next free block            PatternCheck - TRUE is the pattern of the next block needs                         to be tested (debug)            PreviousBlockSize - Supplies the size of the previous block            FreeSize - Supplies the size (in allocation units) of the block should be split        Return Value:            None.    --*/    
{  
    PHEAP_FREE_ENTRY SplitBlock2;  
    LOGICAL CommitFailure = FALSE;  
  
    //      //  Reset the flags that we copied from the original free list      //  header, and set it other size fields.      //    
    SplitBlock->Flags = FreeFlags;  
    SplitBlock->UnusedBytes = 0;  
    RtlpSetBlockPreviousSize(Heap, (PHEAP_ENTRY)SplitBlock, PreviousBlockSize);  
    RtlpSetBlockOffset( (PHEAP_ENTRY)SplitBlock, Segment );  
    RtlpSetSmallTagIndex(Heap, SplitBlock, 0);  
    SplitBlock->Size = (USHORT)FreeSize;  
  
    //      //  We need to check the following block      //  and if it is busy then update its previous size      //  before inserting our new free block into the      //  free list      //    
retrysplit:  
  
    SplitBlock2 = (PHEAP_FREE_ENTRY)((PHEAP_ENTRY)SplitBlock + FreeSize);  
  
    if (RtlpIsBusyFlagSet(Heap, (PHEAP_ENTRY)SplitBlock2)) {  
  
        //          //  The next block is busy. Update the previous size,          //  insert the current block to the free lists and we are done          //    
        RtlpSetBlockPreviousSize( Heap,  
                                  (PHEAP_ENTRY)SplitBlock2,  
                                  (USHORT)FreeSize );  
  
        if (!PatternCheck) {  
  
            RtlpFastInsertFreeBlockDirect( Heap, SplitBlock, (USHORT)FreeSize );  
  
        } else {  
  
            RtlpInsertFreeBlockDirect( Heap, SplitBlock, (USHORT)FreeSize );  
        }  
  
    } else {  
  
        //          //  The following block is free so we'll merge          //  these to blocks. by first merging the flags          //    
        RtlpUnpackHeapEntry(Heap, (PHEAP_ENTRY)SplitBlock2);  
  
        //          //  Removing the second block from its free list          //    
        if (!RtlpFastRemoveFreeBlock( Heap, SplitBlock2 )) {  
  
            if (!CommitFailure) {  
  
                //                  //  This might be a low memory condition and the                  //  uncommitted block has been converted to a UCR                  //    
                CommitFailure = TRUE;  
  
                goto retrysplit;  
            }  
  
            return FALSE;  
        }  
  
        //          //  Ensure that the block does not accidentally have its decommit          //  flag set, which will cause bad flags to propagate to the          //  coalesced block          //    
        HEAPASSERT(!(SplitBlock2->Flags & HEAP_ENTRY_DECOMMITTED));  
  
        if (PatternCheck) {  
  
            RtlpCheckFreePattern( SplitBlock2 );  
        }  
  
        SplitBlock->Flags = SplitBlock2->Flags;  
  
        //          //  Updating the free total number of free bytes          //  in the heap and updating the size of the new          //  free block          //    
        FreeSize += SplitBlock2->Size;  
  
        //          //  If the new free block is still less than the          //  maximum heap block size then we'll simply          //  insert it back in the free list          //    
        if (FreeSize <= HEAP_MAXIMUM_BLOCK_SIZE) {  
  
            SplitBlock->Size = (USHORT)FreeSize;  
  
            //              //  Again check if the new following block              //  exists and if so then update is previous              //  size              //    
            RtlpSetBlockPreviousSize( Heap,  
                                      (PHEAP_ENTRY)SplitBlock + FreeSize,  
                                      (USHORT)FreeSize );  
  
            //              //  Insert the new free block into the free              //  list and update the free heap size              //    
            if (!PatternCheck) {  
  
                RtlpFastInsertFreeBlockDirect( Heap, SplitBlock, (USHORT)FreeSize );  
  
            } else {  
  
                RtlpInsertFreeBlockDirect( Heap, SplitBlock, (USHORT)FreeSize );  
            }  
  
        } else {  
  
            //              //  The new free block is pretty large so we              //  need to call a private routine to do the              //  insert              //    
            RtlpInsertFreeBlock( Heap, SplitBlock, FreeSize );  
        }  
    }  
  
    return TRUE;  
}  
  
PVOID  
FASTCALL  
RtlpSetupExtendedBlock (  
    __in PHEAP Heap,  
    __in ULONG Flags,  
    __in_bcount(Size+ExtraSize) PVOID BaseAddress,  
    __in SIZE_T Size,  
    __in USHORT ExtraSize,  
    __in ULONG Interceptor  
    )  
  
/*++        Routine Description:            This procedure initialize the block header for a prefix entry      A prefix entry is following additional data as required by some interceptors      and is placed right before the user allocation.        Arguments:            Heap - Supplies the heap            Flags - Supplies the operation flags            BaseAddress - Supplies the base address of the allocation            Size - Supplies the actual size of the request            ExtraSize - Supplies the size of the prefix block        Return Value:    --*/    
{  
    PHEAP_ENTRY BusyBlock = (PHEAP_ENTRY)BaseAddress - 1;  
    PHEAP_ENTRY ExtendedEntry;  
    BOOLEAN LockAcquired;  
    SIZE_T UnusedBytes;  
  
    HEAPASSERT(RtlpIsBlockBusy(BusyBlock));  
  
    LockAcquired = FALSE;  
  
    //      //  Locate the position of the extended entry      //    
    ExtendedEntry = (PHEAP_ENTRY)((PUCHAR)BaseAddress + ExtraSize - sizeof(HEAP_ENTRY));  
  
#pragma prefast(suppress:__WARNING_BUFFER_UNDERFLOW, "A BLOCK_ENTRY always preceeds the base address")  
    if (RtlpIsVirtualBlock( BusyBlock )) {  
  
        //          //  The virtual block has the unused bytes coded differently          //  than the other blocks. The field is protected by the          //  heap lock, if the heap is serializable          //    
        if (!((Flags| Heap->ForceFlags) & HEAP_NO_SERIALIZE)) {  
            RtlAcquireLockRoutine( Heap->LockVariable );  
            LockAcquired = TRUE;  
        }  
  
        RtlpUnpackHeapEntry(Heap, BusyBlock);  
  
        BusyBlock->Size += (USHORT)ExtraSize;  
        UnusedBytes = BusyBlock->Size;  
  
        //          //  The segment offset is used to point to the extended block          //  Note that the virtual block has already a type set, so the          //  same field cannot be overloaded          //    
        BusyBlock->SegmentOffset = (UCHAR)(ExtraSize >> HEAP_GRANULARITY_SHIFT);  
  
        RtlpPackHeapEntry(Heap, BusyBlock);  
  
        //          //  Set the pointer from the extended block back to the original allocation          //    
        ExtendedEntry->SegmentOffset = (UCHAR)(ExtraSize >> HEAP_GRANULARITY_SHIFT);  
  
    } else {  
  
        //          //  For regular blocks we do not any locking to query the          //  size.          //    
        UnusedBytes = RtlpGetUnusedBytes(Heap, BusyBlock);  
  
        ExtendedEntry->EntryOffset = (UCHAR)(ExtraSize >> HEAP_GRANULARITY_SHIFT);  
        BusyBlock->UnusedBytes = ExtendedEntry->SegmentOffset |  
                                 ((BusyBlock->UnusedBytes & HEAP_LFH_BIT) | HEAP_EXTENDED_INFO);  
  
    }  
  
    //      //  Mark the extended block header appropriately and save the actual previous size      //    
    ExtendedEntry->ExtendedBlockSignature = HEAP_BLOCK_STATE_EXTENDED_BLOCK;  
    ExtendedEntry->UnusedBytesLength = (USHORT)(UnusedBytes + ExtraSize);  
    ExtendedEntry->InterceptorValue = Interceptor;  
  
    if (LockAcquired != FALSE) {  
        RtlReleaseLockRoutine(Heap->LockVariable);  
    }  
  
#pragma prefast(suppress:__WARNING_FAILING_TO_RELEASE, "The heap lock will always be released if it's acquired unless there's a process-killing corruption, in which case the critsec state is irrelevant.")  
    return (ExtendedEntry + 1);  
}  
  
PVOID  
FASTCALL  
RtlpAllocateHeap (  
    __in PHEAP Heap,  
    __in ULONG Flags,  
    __in SIZE_T Size,  
    __in SIZE_T AllocationSize,  
    __in_opt PHEAP_LOOKUP_ENTRY LookupEntry,  
    __out PULONG TracingSource  
    )  
  
/*++    Routine Description:        This routine allocates a block of the specified size from the specified      heap.    Arguments:        Heap - Supplies the address of the heap from which to allocate.        Flags - Supplies flags that control the operation.        Size - Supplies the user-supplied requested size for this allocation.        AllocationSize - Supplies the requested size for this allocation as          a multiple of the heap granularity. This value also accounts for          size-0 requests.        LookupEntry - Supplies an optional pointer to the free list lookup entry          for blocks of this size.        TracingSource - Supplies a pointer to a buffer that receives information          about the path this routine took to allocate the output block.    Return Value:        The address of the user-writable portion of a newly allocated block,      or NULL if no block could be allocated.    --*/    
{  
    SIZE_T AllocationIndex;  
    PHEAP_ENTRY BusyBlock;  
    SIZE_T BlockSize = 0;  
    SIZE_T ContextIndex;  
    UCHAR EntryFlags;  
    PHEAP_ENTRY_EXTRA ExtraStuff;  
    LOGICAL FastAlloc = TRUE;  
    PHEAP_FREE_ENTRY FreeBlock;  
    UCHAR FreeFlags;  
    PULONG FreeListsInUse;  
    ULONG FreeListsInUseUlong;  
    PLIST_ENTRY FreeListHead;  
    SIZE_T FreeSize;  
    PVOID FrontEndHeap = NULL;  
    ULONG InUseIndex;  
    BOOLEAN LockAcquired = FALSE;  
    PLIST_ENTRY Next;  
    PVOID ReturnValue = NULL;  
    NTSTATUS Status;  
    PHEAP_INDEX_USAGE_DATA UsageData;  
    PHEAP_VIRTUAL_ALLOC_ENTRY VirtualAllocBlock = NULL;  
    SIZE_T VirtualAllocPadding;  
    PVOID VirtualAllocReserved;  
    SIZE_T VirtualBlockPaddedSize;  
    LOGICAL ContentionPresent = FALSE;  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
    ContextIndex = AllocationSize >> HEAP_GRANULARITY_SHIFT;  
  
#endif  
  
    if ((Flags & HEAP_SLOW_FLAGS) || (Size >= 0x80000000)) {  
  
        FastAlloc = FALSE;  
        *TracingSource = MEMORY_FROM_SLOWPATH;  
  
        //          //  If the size is greater than maxlong then say we can't allocate that          //  much and return the error to our caller          //    
        if (Size > MAXINT_PTR) {  
            return NULL;  
        }  
  
        //          //  In the non kernel case check if we should be using the debug version          //  of heap allocation          //    
#if !defined(NTOS_KERNEL_RUNTIME)  
  
        if (DEBUG_HEAP( Flags )) {  
            return RtlDebugAllocateHeap( Heap, Flags, Size );  
        }  
  
#endif  
  
        //          //  Round up the requested size to the allocation granularity.  Note          //  that if the request is for zero bytes we will still allocate memory,          //          //      Allocation size will be either 16, 24, 32, ...          //      Allocation index will be 2, 3, 4, ...          //    
        AllocationSize = RtlpRoundAllocationSize(Heap, Size);  
  
        //          //  Generate the flags needed for this heap entry.  Mark it busy and add          //  any user settable bits.  Also if the input flag indicates any entry          //  extra fields and we have a tag to use then make room for the extra          //  fields in the heap entry          //    
        EntryFlags = (UCHAR)(HEAP_ENTRY_BUSY | ((Flags & HEAP_SETTABLE_USER_FLAGS) >> 4));  
  
        if ((Flags & HEAP_NEED_EXTRA_FLAGS) || (Heap->PseudoTagEntries != NULL)) {  
  
            EntryFlags |= HEAP_ENTRY_EXTRA_PRESENT;  
            AllocationSize += sizeof( HEAP_ENTRY_EXTRA );  
        }  
  
        AllocationIndex = AllocationSize >>  HEAP_GRANULARITY_SHIFT;  
  
    } else {  
        EntryFlags = HEAP_ENTRY_BUSY;  
        AllocationIndex = AllocationSize >> HEAP_GRANULARITY_SHIFT;  
  
        //          //  The smallest block can have the index 2          //  because we need to hold the free list entry          //  when the block is released.          //    
        if (AllocationIndex < 2) {  
            AllocationSize += HEAP_GRANULARITY;  
            AllocationIndex = 2;  
        }  
  
        *TracingSource = MEMORY_FROM_MAINPATH;  
    }  
  
    if ((Flags & HEAP_NO_CACHE_BLOCK) &&  
        (IS_HEAP_TAGGING_ENABLED() == FALSE)) {  
  
        EntryFlags |= HEAP_ENTRY_INTERNAL;  
    }  
  
    try {  
  
        //          //  Check if we need to serialize our access to the heap          //    
        if (!(Flags & HEAP_NO_SERIALIZE)) {  
  
            //              //  Attempt to lock the heap              //    
            if (!RtlpAcquireLockWithContentionCheck(Heap, &ContentionPresent)) {  
  
                SET_LAST_STATUS( STATUS_POSSIBLE_DEADLOCK );  
                leave;  
            }  
  
            LockAcquired = TRUE;  
            RtlpCheckForHeapMaintenance(Heap);  
  
        }  
  
        if (AllocationIndex <= Heap->VirtualMemoryThreshold) {  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
            //              // If this is not an internal allocation, update the usage data              // that is used to decide when the LFH is enabled for a given              // allocation index.              //              // If the caller-provided context array index does not have a              // corresponding slot in the array, check to see if the user              // has already explicitly requested an LFH for this heap. If              // so, enable the LFH right away to ensure the context array              // index will be valid for subsequent allocations of this size.              //              // If the context array index is valid and the requested size              // can be handled by the front-end heap, update the usage data              // accordingly.              //    
            if ((Flags & HEAP_NO_CACHE_BLOCK) == 0) {  
                if (ContextIndex >= Heap->FrontEndHeapMaximumIndex) {  
                    if ((Size <= HEAP_LARGEST_LFH_BLOCK) &&  
                        (RtlpGetLowFragHeap(Heap) == NULL) &&  
                        (Heap->RequestedFrontEndHeapType == HEAP_FRONT_LOWFRAGHEAP)) {  
  
                        Heap->CompatibilityFlags |= HEAP_MAINTENANCE_ENABLE_LFH;  
                    }  
  
                } else if ((Size <= HEAP_LARGEST_LFH_BLOCK) &&  
                           (RtlpIsFrontEndEnabledForIndex(Heap, ContextIndex) == FALSE)) {  
  
                    UsageData = &Heap->FrontEndHeapUsageData[ContextIndex];  
                    *UsageData += HEAP_BLOCK_USAGE_INCREMENT | HEAP_BLOCK_OPERATION_INCREMENT;  
  
                    //                      // If:                      // (a) Contention is present in this heap, or                      // (b) The total number of active blocks of this size                      //     exceeds a given threshold, or                      // (c) The total number of blocks of this size that have                      //     ever been allocated exceeds a given threshold,                      // then enable the front-end heap for the current                      // allocation index.                      //    
                    if ((ContentionPresent != FALSE) ||  
                        ((*UsageData & HEAP_BLOCK_USAGE_MASK) > HEAP_BLOCK_USAGE_THRESHOLD) ||  
                        (*UsageData > HEAP_BLOCK_OPERATION_THRESHOLD)) {  
  
                        HEAP_INDEX_USAGE_DATA TempUsage;  
  
                        TempUsage = RtlpGetLFHContext( RtlpGetLowFragHeap(Heap),  
                                                       (Size ? Size : 1) );  
  
                        //                          // If the frontend heap context value is valid, store                          // it in the context array. Passing it back to the                          // frontend later will allocate a block large enough                          // to hold requests of the specified size. Mark the                          // frontend as active for this allocation index.                          //                          // If getting the frontend context value failed, see                          // if a frontend heap even exists yet and create one                          // if it does not.                          //    
                        if (TempUsage != HEAP_INVALID_BUCKET_INDEX) {  
                            *UsageData = TempUsage;  
  
                            //                              // Enable the frontend. This write must be volatile                              // to ensure that all previous initialization steps                              // have completed before other threads observe that                              // the relevant bit is set.                              //    
                            RtlpEnableFrontEndForIndex(Heap, ContextIndex);  
                            Heap->Counters.AllocationIndicesActive += 1;  
  
                        } else {  
                            if ((ContentionPresent != FALSE) ||  
                                (*UsageData > HEAP_BLOCK_USAGE_THRESHOLD) ) {  
  
                                if (RtlpGetLowFragHeap(Heap) == NULL) {  
                                    Heap->CompatibilityFlags |= HEAP_MAINTENANCE_ENABLE_LFH;  
                                }  
                            }  
                        }  
                    }  
                }  
            }  
  
            if ((LookupEntry != NULL) && (LookupEntry->Hint != NULL)) {  
                FreeBlock = CONTAINING_RECORD( LookupEntry->Hint, HEAP_FREE_ENTRY, FreeList );  
                RtlpUnpackHeapEntry( Heap, (PHEAP_ENTRY)FreeBlock );  
  
                HEAPASSERT( FreeBlock->Size >= AllocationIndex );  
  
                if (RtlpFastRemoveFreeBlock( Heap, FreeBlock )) {  
  
                    goto SplitFreeBlock;  
                }  
  
                //                  //  Removal failed, maybe due to low memory condition or corruption                  //    
                SET_LAST_STATUS( STATUS_NO_MEMORY );  
                leave;  
            }  
#endif  
            //              //  The following code cycles through the [0] free list until              //  it finds a block that satisfies the request.  The list              //  is sorted so the search is can be terminated early on success              //    
            FreeListHead = &Heap->FreeLists;  
  
            //              //  We can use the index to find the block very quick              //    
            Next = RtlpFindEntry( Heap, AllocationIndex );  
  
            if ( FreeListHead != Next ) {  
  
                FreeBlock = CONTAINING_RECORD( Next, HEAP_FREE_ENTRY, FreeList );  
  
                RtlpUnpackHeapEntry( Heap, (PHEAP_ENTRY)FreeBlock );  
  
                if ( FreeBlock->Size >= AllocationIndex ) {  
  
                    //                      //  We've found something that we can use so now remove                      //  it from the free list and go to where we treat splitting                      //  a free block.  Note that the block we found here might                      //  actually be the exact size we need and that is why                      //  in the split free block case we have to consider having                      //  nothing free after the split                      //    
                    if (!RtlpFastRemoveFreeBlock( Heap, FreeBlock )) {  
  
                        //                          //  Low memory condition. The previous block                          //  has been converted to an UCR or the entry is corrupted                          //    
                        leave;  
                    }  
  
                    goto SplitFreeBlock;  
  
                } else {  
  
                    RtlpPackHeapEntry( Heap, (PHEAP_ENTRY)FreeBlock );  
                }  
            }  
  
            //              //  The [0] list is either empty or everything is too small              //  so now extend the heap which should get us something less              //  than or equal to the virtual memory threshold              //    
            FreeBlock = RtlpExtendHeap( Heap, AllocationSize );  
  
            //              //  And provided we got something we'll treat it just like the previous              //  split free block cases              //    
            if (FreeBlock != NULL) {  
  
                if (!RtlpFastRemoveFreeBlock( Heap, FreeBlock )) {  
  
                    //                      //  Low memory condition. The previous block                      //  has been converted to an UCR. Exit the function.                      //  Note that in the low memory case, the entry is                      //  properly restored. In the corruption case the entry is left intact                      //    
                    leave;  
                }  
  
SplitFreeBlock:  
  
                //                  //  Save the blocks flags and decrement the amount of                  //  free space left in the heap                  //    
                FreeFlags = FreeBlock->Flags;  
  
                if (!FastAlloc) {  
  
                    RtlpCheckFreePattern( FreeBlock );  
                }  
  
                //                  //  Mark the block busy                  //    
                BusyBlock = (PHEAP_ENTRY)FreeBlock;  
                if (RtlpIsBlockBusySafe(BusyBlock) != FALSE) {  
                    RtlpLogHeapFailure(heap_failure_entry_corruption,  
                                       Heap,  
                                       BusyBlock,  
                                       NULL,  
                                       NULL,  
                                       NULL);  
  
                    leave;  
                }  
  
                BusyBlock->Flags = EntryFlags;  
  
                //                  //  Compute the size (i.e., index) of the amount from this block                  //  that we don't need and can return to the free list                  //    
                HEAPASSERT( BusyBlock->Size >= AllocationIndex );  
  
                FreeSize = BusyBlock->Size - AllocationIndex;  
  
                //                  //  Finish setting up the rest of the new busy block                  //    
                BusyBlock->Size = (USHORT)AllocationIndex;  
                RtlpSetUnusedBytes( Heap,  
                                    BusyBlock,  
                                    0,  
                                    AllocationIndex,  
                                    (AllocationSize - Size) );  
  
                RtlpSetSmallTagIndex( Heap, BusyBlock, 0 );  
  
                //                  //  Now if the size that we are going to free up is not zero                  //  then lets get to work and to the split.                  //    
                if (FreeSize != 0) {  
  
                    //                      //  But first we won't ever bother doing a split that only                      //  gives us 8 bytes back.  So if free size is one then just                      //  bump up the size of the new busy block                      //    
                    if (FreeSize == 1) {  
  
                        BusyBlock->Size += 1;  
                        RtlpSetUnusedBytes( Heap,  
                                            BusyBlock,  
                                            0,  
                                            AllocationIndex + 1,  
                                            AllocationSize + sizeof( HEAP_ENTRY ) - Size );  
  
                    } else {  
  
                        if (!RtlpCreateSplitBlock( Heap,  
                                                   RtlpGetSegment(Heap, BusyBlock),  
                                                   (PHEAP_FREE_ENTRY)(BusyBlock + AllocationIndex),  
                                                   FreeFlags,  
                                                   !FastAlloc,  
                                                   (USHORT)AllocationIndex,  
                                                   FreeSize )) {  
  
                            //                              //  Possible corruption caused this failure.                              //    
                            SET_LAST_STATUS( STATUS_DATA_OVERRUN );  
                            leave;  
                        }  
  
                        //                          //  Now that free flags made it back into a free block                          //  we can zero out what we saved.                          //    
                        FreeFlags = 0;  
                    }  
                }  
  
                //                  //  Return the address of the user portion of the allocated block.                  //  This is the byte following the header.                  //    
                ReturnValue = BusyBlock + 1;  
                BlockSize = BusyBlock->Size << HEAP_GRANULARITY_SHIFT;  
  
                //                  // If the unused bytes were too large for the UnusedBytes field                  // then decrement the BlockSize so the value is not zero'd if the                  // HEAP_ZERO_MEMORY flag was passed.                  //    
                if (RtlpHasUnusedBytesOverflow(BusyBlock)) {  
                    BlockSize = BlockSize - sizeof(SIZE_T);  
                }  
                NT_ASSERT((SIZE_T)(BusyBlock + BusyBlock->Size) + HEAP_UNUSED_PRIVATE_DATA_SIZE >=  
                          (SIZE_T)((PCHAR)ReturnValue + BlockSize) + HEAP_UNUSED_PRIVATE_DATA_SIZE - sizeof(HEAP_ENTRY));  
  
                if (FastAlloc) {  
  
                    //                      //  Release the lock before the zero memory call                      //    
                    RtlpPackHeapEntry( Heap, (PHEAP_ENTRY)BusyBlock );  
  
                    if (LockAcquired) {  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
                        RtlpUpdateHeapWatermarks(Heap);  
  
#endif  
  
                        RtlReleaseLockRoutine( Heap->LockVariable );  
  
                        LockAcquired = FALSE;  
                    }  
  
                    //                      //  If the flags indicate that we should zero memory then do it now                      //    
                    if (Flags & HEAP_ZERO_MEMORY) {  
                        RtlpHeapZeroMemory(ReturnValue,  BlockSize, Size);  
                    }  
  
                } else {  
  
  
                    //                      //  If the flags indicate that we should zero memory then do it now                      //  and fix up the unused bytes field which we zero out.                      //    
                    if (Flags & HEAP_ZERO_MEMORY) {  
  
                        RtlpHeapZeroMemory(ReturnValue,  BlockSize, Size);  
  
                    //                      //  Otherwise if the flags indicate that we should fill heap then                      //  do it now.                      //    
                    } else if (Heap->Flags & HEAP_FREE_CHECKING_ENABLED) {  
  
                        RtlFillMemoryUlong( (PCHAR)ReturnValue,  
                                Size & ~0x3,  
                                ALLOC_HEAP_FILL );  
                    }  
  
                    //                      //  If the flags indicate that we should do tail checking then copy                      //  the fill pattern right after the heap block.                      //    
                    if (Heap->Flags & HEAP_TAIL_CHECKING_ENABLED) {  
  
                        RtlFillMemory( (PCHAR)ReturnValue + Size,  
                                       CHECK_HEAP_TAIL_SIZE,  
                                       CHECK_HEAP_TAIL_FILL );  
  
                        BusyBlock->Flags |= HEAP_ENTRY_FILL_PATTERN;  
  
                    }  
  
                    RtlpSetSmallTagIndex( Heap, BusyBlock, 0 );  
  
                    //                      //  If the flags indicate that there is an extra block persent then                      //  we'll fill it in                      //    
                    if (BusyBlock->Flags & HEAP_ENTRY_EXTRA_PRESENT) {  
  
                        ExtraStuff = RtlpGetExtraStuffPointer( BusyBlock );  
  
                        RtlZeroMemory( ExtraStuff, sizeof( *ExtraStuff ));  
  
  
        #if !defined(NTOS_KERNEL_RUNTIME)  
  
                        if (Heap->Flags & HEAP_CAPTURE_STACK_BACKTRACES) {  
  
                            ExtraStuff->AllocatorBackTraceIndex = (USHORT)RtlLogStackBackTrace();  
                        }  
  
                        //                          //  In the non kernel case the tagging goes in either the extra                          //  stuff of the busy block small tag index                          //    
                        if (IS_HEAP_TAGGING_ENABLED()) {  
  
                            ExtraStuff->TagIndex = RtlpUpdateTagEntry( Heap,  
                                                                       (USHORT)((Flags & HEAP_TAG_MASK) >> HEAP_TAG_SHIFT),  
                                                                       0,  
                                                                       BusyBlock->Size,  
                                                                       AllocationAction );  
                        }  
  
                    } else if (IS_HEAP_TAGGING_ENABLED()) {  
  
                        RtlpSetSmallTagIndex( Heap,  
                                              BusyBlock,  
                                              (UCHAR)RtlpUpdateTagEntry( Heap,  
                                              (USHORT)((Flags & HEAP_SMALL_TAG_MASK) >> HEAP_TAG_SHIFT),  
                                              0,  
                                              BusyBlock->Size,  
                                              AllocationAction ));  
  
        #endif // NTOS_KERNEL_RUNTIME    
                    }  
  
                    RtlpPackHeapEntry( Heap, (PHEAP_ENTRY)BusyBlock );  
  
                }  
  
                //                  //  And return the allocated block to our caller                  //    
                leave;  
            }  
  
            //              //  We weren't able to extend the heap so we must be out of memory              //    
            Status = STATUS_NO_MEMORY;  
  
            //              //  At this point the allocation is way too big for any of the free lists              //  and we can only satisfy this request if the heap is growable              //    
        } else if (Heap->Flags & HEAP_GROWABLE) {  
  
            //              //  Compute how much memory we will need for this allocation which              //  will include the allocation size plus a header, and then go              //  get the committed memory              //    
            AllocationSize += FIELD_OFFSET( HEAP_VIRTUAL_ALLOC_ENTRY, BusyBlock );  
            AllocationSize += HEAP_UNUSED_PRIVATE_DATA_SIZE;  
  
            //              // Pad out the virtual allocation with up to 15 preceding pages of              // padding and one trailing page of padding.              //    
            VirtualAllocPadding =  
                ((SIZE_T)RtlpHeapGenerateRandomValue32() % HEAP_VIRTUAL_ALLOC_REBASE_VALUES) *  
                    PAGE_SIZE;  
  
            //              // Reserve some memory for the virtual block.              //    
            VirtualAllocReserved = NULL;  
            VirtualBlockPaddedSize = VirtualAllocPadding + AllocationSize + PAGE_SIZE;  
            Status = ZwAllocateVirtualMemory( NtCurrentProcess(),  
                                              (PVOID *)&VirtualAllocReserved,  
                                              0,  
                                              &VirtualBlockPaddedSize,  
                                              MEM_RESERVE,  
                                              RtlpGetHeapProtection(Heap->Flags) );  
  
            if (!NT_SUCCESS(Status)) {  
                leave;  
            }  
  
            //              // Commit the necessary pages.              //    
            VirtualAllocBlock = (PHEAP_VIRTUAL_ALLOC_ENTRY)  
                ((PUCHAR)VirtualAllocReserved + VirtualAllocPadding);  
  
            Status = ZwAllocateVirtualMemory(NtCurrentProcess(),  
                                             (PVOID *)&VirtualAllocBlock,  
                                             0,  
                                             &AllocationSize,  
                                             MEM_COMMIT,  
                                             RtlpGetHeapProtection(Heap->Flags));  
  
            if (NT_SUCCESS(Status)) {  
  
                //                  //  Just committed, already zero.  Fill in the new block                  //  and insert it in the list of big allocation                  //    
                VirtualAllocBlock->BusyBlock.Size = (USHORT)(AllocationSize - Size);  
                VirtualAllocBlock->BusyBlock.Flags = EntryFlags | HEAP_ENTRY_EXTRA_PRESENT;  
                VirtualAllocBlock->CommitSize = AllocationSize;  
                VirtualAllocBlock->ReserveSize = VirtualBlockPaddedSize;  
                RtlpSetVirtualBlock(&VirtualAllocBlock->BusyBlock);  
                Heap->Counters.TotalSizeInVirtualBlocks += AllocationSize;  
  
                if (IS_HEAP_LOGGING_ENABLED()) {  
                    RtlpLogHeapCommit( Heap,  
                                       VirtualAllocBlock,  
                                       AllocationSize,  
                                       HEAP_LOG_ALLOCATE_HEAP);  
  
                }  
  
                if (IS_HEAP_LOGGING_ENABLED()) {  
                    RtlpLogHeapExtendEvent( Heap,  
                                            (PVOID)VirtualAllocBlock,  
                                            AllocationSize,  
                                            (Heap->TotalFreeSize) << HEAP_GRANULARITY_SHIFT,  
                                            (HANDLE)HEAP_LOGGER_ID );  
                }  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
                if (IS_HEAPSUMMARY_LOGGING_ENABLED()) {  
                    RtlpLogHeapExtendEvent( Heap,  
                                           (PVOID)VirtualAllocBlock,  
                                           AllocationSize,  
                                           (Heap->TotalFreeSize) << HEAP_GRANULARITY_SHIFT,  
                                           (HANDLE)HEAPSUMMARY_LOGGER_ID );  
                }  
  
                if (Heap->Flags & HEAP_CAPTURE_STACK_BACKTRACES) {  
  
                    VirtualAllocBlock->ExtraStuff.AllocatorBackTraceIndex = (USHORT)RtlLogStackBackTrace();  
                }  
  
                //                  //  In the non kernel case see if we need to add heap tagging                  //    
                if (IS_HEAP_TAGGING_ENABLED()) {  
  
                    VirtualAllocBlock->ExtraStuff.TagIndex =  
                        RtlpUpdateTagEntry( Heap,  
                                (USHORT)((Flags & HEAP_SMALL_TAG_MASK) >> HEAP_TAG_SHIFT),  
                                0,  
                                VirtualAllocBlock->CommitSize >> HEAP_GRANULARITY_SHIFT,  
                                VirtualAllocationAction );  
                }  
  
#endif // NTOS_KERNEL_RUNTIME    
                RtlpPackHeapEntry( Heap, &VirtualAllocBlock->BusyBlock );  
  
                RtlpSafeInsertTailList( &Heap->VirtualAllocdBlocks, (PLIST_ENTRY)VirtualAllocBlock );  
  
                //                  //  Return the address of the user portion of the allocated block.                  //  This is the byte following the header.                  //    
                ReturnValue = (PHEAP_ENTRY)(VirtualAllocBlock + 1);  
  
                leave;  
  
            } else {  
  
                VirtualAllocPadding = 0;  
                Status = RtlpHeapFreeVirtualMemory(NtCurrentProcess(),  
                                                   &VirtualAllocReserved,  
                                                   &VirtualAllocPadding,  
                                                   MEM_RELEASE);  
  
                NT_VERIFY(NT_SUCCESS(Status));  
  
                VirtualAllocBlock = NULL;  
  
                Heap->Counters.CommitFailures += 1;  
            }  
  
        } else {  
  
            Status = STATUS_BUFFER_TOO_SMALL;  
        }  
  
        ReturnValue = NULL;  
  
    } finally {  
  
        if (LockAcquired) {  
  
            HEAPASSERT( (ReturnValue == NULL) ||  
                        RtlpValidateEntry( Heap,  
                           RtlpGetHeapEntry(ReturnValue),  
                           TRUE));  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
            if ((ReturnValue != NULL) && (VirtualAllocBlock == NULL)) {  
                RtlpUpdateHeapWatermarks(Heap);  
            }  
  
#endif  
  
            RtlReleaseLockRoutine( Heap->LockVariable );  
        }  
    }  
  
    if ((IS_HEAP_RANGE_LOGGING_ENABLED()) &&  
          (ReturnValue != NULL) &&  
          (VirtualAllocBlock != NULL)) {  
  
        RtlpHeapLogRangeReserve( Heap,  
                                 RtlpGetVirtualBlockReservedBase(VirtualAllocBlock),  
                                 VirtualAllocBlock->ReserveSize);  
    }  
  
    return ReturnValue;  
}  
  
LOGICAL  
FASTCALL  
RtlpFreeHeap (  
    __in PHEAP Heap,  
    __in ULONG Flags,  
    __in_opt PHEAP_ENTRY BusyBlock,  
    __in PVOID BaseAddress  
    )  
  
/*++    Routine Description:        This routine returns a previously allocated block back to its heap    Arguments:        HeapHandle - Supplies a pointer to the owning heap structure        Flags - Specifies the set of flags to use in the deallocation        BusyBlock - Supplies a block being freed    Return Value:        LOGICAL - TRUE if the block was properly freed and FALSE otherwise    --*/    
{  
    SIZE_T BlockSize;  
    PHEAP_ENTRY_EXTRA ExtraStuff;  
    BOOLEAN FastFree = TRUE;  
    SIZE_T FreeSize;  
    PVOID FrontHeap = NULL;  
    BOOLEAN LockAcquired = FALSE;  
    PHEAP_LOOKUP_ENTRY LookupEntry = NULL;  
    LOGICAL ReturnValue = TRUE;  
    NTSTATUS Status;  
    ULONG TraceSource;  
    PHEAP_VIRTUAL_ALLOC_ENTRY VirtualAllocBlock = NULL;  
    SIZE_T VirtualAllocCommitted;  
    PVOID VirtualAllocReservedBase = NULL;  
    BOOLEAN IsInternalBlock = FALSE;  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
    USHORT TagIndex = 0;  
  
#endif // NTOS_KERNEL_RUNTIME    
    //      // Verify that the address being freed does not correspond to the      // heap handle itself.      //    
    if ((PVOID)Heap == BusyBlock) {  
        RtlpLogHeapFailure(heap_failure_invalid_argument,  
                           Heap,  
                           BusyBlock,  
                           NULL,  
                           NULL,  
                           NULL);  
  
        return FALSE;  
    }  
  
    //      //  Complement the input flags with those enforced by the heap      //    
    Flags |= Heap->ForceFlags;  
  
    //      //  Now check if we should go the slow route      //    
    if (Flags & HEAP_SLOW_FLAGS) {  
  
        FastFree = FALSE;  
        TraceSource = MEMORY_FROM_SLOWPATH;  
  
        #if !defined(NTOS_KERNEL_RUNTIME)  
  
            //              //  In the non kernel case see if we should be calling the debug version to              //  free the heap              //    
            if (DEBUG_HEAP( Flags )) {  
  
                return RtlDebugFreeHeap( Heap, Flags, BaseAddress );  
            }  
  
        #endif // NTOS_KERNEL_RUNTIME    
    } else {  
  
        TraceSource = MEMORY_FROM_MAINPATH;  
    }  
  
    try {  
  
        //          //  Check if we need to lock the heap          //    
        if (!(Flags & HEAP_NO_SERIALIZE)) {  
  
            //              //  Attempt to lock the heap              //    
            if (!RtlpAcquireLockWithContentionCheck(Heap, NULL)) {  
  
                SET_LAST_STATUS( STATUS_POSSIBLE_DEADLOCK );  
                ReturnValue = FALSE;  
                BusyBlock = NULL;  
                leave;  
            }  
  
            LockAcquired = TRUE;  
  
            RtlpUnpackHeapEntry( Heap, BusyBlock );  
  
            //              //  Find the appropriate lookup entry for the block size              //  Note that the Size can be actually different than the one requested              //  since in some rare cases the heap coalesce a busy block with 8 bytes              //  if next to an uncommitted range. Same would happen if a realloc occurs              //    
            LookupEntry = RtlpGetLookupEntry( Heap->BlocksIndex,  
                                              BusyBlock->Size );  
  
        } else {  
  
            RtlpUnpackHeapEntry( Heap, BusyBlock );  
        }  
  
        if (BusyBlock->Flags & HEAP_ENTRY_INTERNAL) {  
  
            IsInternalBlock = TRUE;  
  
            //              //  Clear the flag overlapping flag with free blocks              //    
            BusyBlock->Flags &= ~HEAP_ENTRY_INTERNAL;  
        }  
  
        //          //  Check if this is not a virtual block allocation meaning          //  that we it is part of the heap free list structure and not          //  one huge allocation that we got from vm          //    
        if ( !RtlpIsVirtualBlock( BusyBlock ) ) {  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
            //              // Decrement the count of active blocks for this allocation              // index.              //              // N.B. These conditions must remain in this order, since the              //      third test can only execute safely if the second is              //      true, and the second can only execute safely if the              //      first is true.              //    
            if ((BusyBlock->Size < Heap->FrontEndHeapMaximumIndex) &&  
                (RtlpIsFrontEndEnabledForIndex(Heap, BusyBlock->Size) == FALSE) &&  
                ((Heap->FrontEndHeapUsageData[BusyBlock->Size] > HEAP_BLOCK_USAGE_INCREMENT))) {  
  
                Heap->FrontEndHeapUsageData[BusyBlock->Size] -= HEAP_BLOCK_USAGE_INCREMENT;  
            }  
  
            if (!FastFree) {  
  
                //                  //  First in the non kernel case remove any tagging we might                  //  have been using.  Note that the will either be in                  //  the heap header, or in the extra block if present                  //    
                if (IS_HEAP_TAGGING_ENABLED()) {  
  
                    if (BusyBlock->Flags & HEAP_ENTRY_EXTRA_PRESENT) {  
  
                        ExtraStuff = (PHEAP_ENTRY_EXTRA)(BusyBlock + BusyBlock->Size - 1);  
  
                        TagIndex = RtlpUpdateTagEntry( Heap,  
                                                       ExtraStuff->TagIndex,  
                                                       BusyBlock->Size,  
                                                       0,  
                                                       FreeAction );  
  
                    } else {  
  
                        TagIndex = RtlpUpdateTagEntry( Heap,  
                                                       RtlpGetSmallTagIndex( Heap, BusyBlock),  
                                                       BusyBlock->Size,  
                                                       0,  
                                                       FreeAction );  
                    }  
                }  
            }  
  
#endif // NTOS_KERNEL_RUNTIME    
            //              //  This block is not a big allocation so we need to              //  to get its size, and coalesce the blocks note that              //  the user mode heap does this conditionally on a heap              //  flag.  The coalesce function returns the newly formed              //  free block and the new size.              //    
            BlockSize = FreeSize = BusyBlock->Size;  
  
    #ifdef NTOS_KERNEL_RUNTIME  
  
            BusyBlock = (PHEAP_ENTRY)RtlpCoalesceFreeBlocks( Heap,  
                                                             (PHEAP_FREE_ENTRY)BusyBlock,  
                                                             &FreeSize,  
                                                             FALSE );  
  
    #else // NTOS_KERNEL_RUNTIME    
            if (!(Heap->Flags & HEAP_DISABLE_COALESCE_ON_FREE)) {  
  
                BusyBlock = (PHEAP_ENTRY)RtlpCoalesceFreeBlocks( Heap,  
                                                                 (PHEAP_FREE_ENTRY)BusyBlock,  
                                                                 &FreeSize,  
                                                                 FALSE );  
            }  
  
    #endif // NTOS_KERNEL_RUNTIME    
            //              //  Check for a small allocation that can go on a freelist              //  first, these should never trigger a decommit.              //    
            HEAPASSERT(HEAP_MAXIMUM_FREELISTS < Heap->DeCommitFreeBlockThreshold);  
  
            //              //  If the allocation fits on a free list then insert it on              //  the appropriate free list.  If the block is not the last              //  entry then make sure that the next block knows our correct              //  size, and update the heap free space counter.              //    
            if ( (FreeSize < Heap->DeCommitFreeBlockThreshold)  
                    ||  
                 ((Heap->TotalFreeSize + FreeSize) < Heap->DeCommitTotalFreeThreshold)) {  
  
    #if !defined(NTOS_KERNEL_RUNTIME)  
  
                //                  //  If the block is larger than 1 page, and has uncommited ranges around                  //  force the decommit to reduce the VA fragmentation                  //    
                if (((Heap->TotalFreeSize + FreeSize) > Heap->DeCommitTotalFreeThreshold)  
                        &&  
                    (FreeSize >= (PAGE_SIZE >> HEAP_GRANULARITY_SHIFT))  
                        &&  
                    ((RtlpGetBlockPreviousSize(Heap, BusyBlock) == 0))) {  
  
                    //                      //  Decommit the block right away                      //    
                    RtlpDeCommitFreeBlock( Heap, (PHEAP_FREE_ENTRY)BusyBlock, FreeSize, FALSE );  
                    TagIndex = 0;  
  
                } else  
  
    #endif  //NTOS_KERNEL_RUNTIME    
                    if (FreeSize <= (ULONG)HEAP_MAXIMUM_BLOCK_SIZE) {  
  
                        //                          //  Insert the block to the free lists.                          //  Do fill the block with the pattern in the debug version                          //    
                        if (FastFree) {  
  
                            RtlpFastInsertFreeBlockDirect( Heap,  
                                                           (PHEAP_FREE_ENTRY)BusyBlock,  
                                                           (USHORT)FreeSize );  
                        } else {  
  
                            RtlpInsertFreeBlockDirect( Heap,  
                                                       (PHEAP_FREE_ENTRY)BusyBlock,  
                                                       (USHORT)FreeSize );  
                        }  
  
                     } else {  
  
                         //                           //  The block is too big to go on a free list in its                           //  entirety but we don't want to decommit anything so                           //  simply call a worker routine to hack up the block                           //  into pieces that will fit on the free lists.                           //    
                         RtlpInsertFreeBlock( Heap, (PHEAP_FREE_ENTRY)BusyBlock, FreeSize );  
                     }  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
                    //                      //  In the non kernel case see if the there was tag and if                      //  so then update the entry to show that it's been freed                      //    
                    if (TagIndex != 0) {  
  
                        PHEAP_FREE_ENTRY_EXTRA FreeExtra;  
  
                        RtlpUnpackHeapEntry(Heap, BusyBlock);  
                        BusyBlock->Flags |= HEAP_ENTRY_EXTRA_PRESENT;  
                        FreeExtra = (PHEAP_FREE_ENTRY_EXTRA)(BusyBlock + BusyBlock->Size) - 1;  
                        RtlpPackHeapEntry(Heap, BusyBlock);  
  
                        FreeExtra->TagIndex = TagIndex;  
                        FreeExtra->FreeBackTraceIndex = 0;  
  
                        if (Heap->Flags & HEAP_CAPTURE_STACK_BACKTRACES) {  
  
                            FreeExtra->FreeBackTraceIndex = (USHORT)RtlLogStackBackTrace();  
                        }  
                    }  
  
#endif // NTOS_KERNEL_RUNTIME    
            } else {  
  
                //                  //  Otherwise the block is to big for any lists and we should decommit                  //  the block                  //    
                RtlpDeCommitFreeBlock( Heap, (PHEAP_FREE_ENTRY)BusyBlock, FreeSize, FALSE );  
            }  
  
            BusyBlock = NULL;  
  
        } else {  
  
            //              //  This is a big virtual block allocation.  To free it we only have to              //  remove it from the heaps list of virtual allocated blocks, unlock              //  the heap, and return the block to vm              //    
            VirtualAllocBlock = CONTAINING_RECORD( BusyBlock, HEAP_VIRTUAL_ALLOC_ENTRY, BusyBlock );  
            VirtualAllocCommitted = VirtualAllocBlock->CommitSize;  
            VirtualAllocReservedBase = RtlpGetVirtualBlockReservedBase(VirtualAllocBlock);  
            Heap->Counters.TotalSizeInVirtualBlocks -= VirtualAllocCommitted;  
  
            RtlpHeapRemoveEntryList( &VirtualAllocBlock->Entry );  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
            //              //  In the non kernel case see if we need to free the tag              //    
            if ((!FastFree) && IS_HEAP_TAGGING_ENABLED()) {  
  
                RtlpUpdateTagEntry( Heap,  
                                    VirtualAllocBlock->ExtraStuff.TagIndex,  
                                    VirtualAllocBlock->CommitSize >> HEAP_GRANULARITY_SHIFT,  
                                    0,  
                                    VirtualFreeAction );  
            }  
  
#endif // NTOS_KERNEL_RUNTIME    
            //              //  Release lock here as there is no reason to hold it across              //  the system call.              //    
            BusyBlock = NULL;  
  
            if (LockAcquired) {  
  
                RtlReleaseLockRoutine( Heap->LockVariable );  
                LockAcquired = FALSE;  
            }  
  
            FreeSize = 0;  
  
            Status = RtlpHeapFreeVirtualMemory( NtCurrentProcess(),  
                                                &VirtualAllocReservedBase,  
                                                &FreeSize,  
                                                MEM_RELEASE );  
  
            NT_VERIFY(NT_SUCCESS(Status));  
  
            if (IS_HEAP_LOGGING_ENABLED()) {  
                RtlpLogHeapContractEvent( Heap,  
                                          (PVOID)VirtualAllocBlock,  
                                          VirtualAllocCommitted,  
                                          (Heap->TotalFreeSize) << HEAP_GRANULARITY_SHIFT,  
                                          FALSE,  
                                          0,  
                                          (HANDLE)HEAP_LOGGER_ID);  
            }  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
            if (IS_HEAPSUMMARY_LOGGING_ENABLED()) {  
  
                RtlpLogHeapContractEvent( Heap,  
                                          (PVOID)VirtualAllocBlock,  
                                          VirtualAllocCommitted,  
                                          (Heap->TotalFreeSize) << HEAP_GRANULARITY_SHIFT,  
                                          FALSE,  
                                          0,  
                                          (HANDLE)HEAPSUMMARY_LOGGER_ID );  
            }  
  
#endif  
  
            //              // FreeSize is in bytes, but we need it in heap units              // to change the allocation counter later              //    
            BlockSize = FreeSize >> HEAP_GRANULARITY_SHIFT;  
        }  
  
    } finally {  
  
        if (BusyBlock != NULL) {  
            RtlpPackHeapEntry(Heap, BusyBlock);  
        }  
  
        if (LockAcquired) {  
  
#if !defined(NTOS_KERNEL_RUNTIME)  
  
            RtlpUpdateHeapWatermarks(Heap);  
  
#endif  
  
            RtlReleaseLockRoutine( Heap->LockVariable );  
        }  
    }  
  
    //      //  The block was freed successfully so return success to our      //  caller      //      if (IS_HEAP_LOGGING_ENABLED()) {  
  
        //          // Log successful frees of non-internal blocks.          //    
        if ((IsInternalBlock == FALSE) && (ReturnValue != FALSE)) {  
            RtlpLogHeapFreeEvent( Heap, BaseAddress, TraceSource );  
        }  
    }  
  
    if ((IS_HEAP_RANGE_LOGGING_ENABLED()) && (VirtualAllocBlock != NULL)) {  
        RtlpHeapLogRangeRelease(Heap, VirtualAllocReservedBase, FreeSize);  
    }  
  
    return ReturnValue;  
}  
  
ULONG  
RtlpHeapGenerateRandomValue32 (  
    )  
  
{  
  
    static ULONG Cookie = 0;  
  
    if (Cookie == 0){  
        NTSTATUS Status;  
        Status = NtQueryInformationProcess (NtCurrentProcess (),  
                                            ProcessCookie,  
                                            &Cookie,  
                                            sizeof (Cookie),  
                                            NULL);  
  
        //          // If querying the process cookie fails, use the current          // tick count instead.          //    
        if (!NT_SUCCESS(Status)) {  
            Cookie = NtGetTickCount();  
        }  
    }  
  
    return RtlRandomEx(&Cookie);  
}  
  
ULONGLONG  
RtlpHeapGenerateRandomValue64(  
    )  
  
/*++    Routine Description:        This routine returns a pseudorandom value.    Arguments:        None.    Return Value:        Returns a pseudorandom 64-bit value.    --*/    
{  
    ULONGLONG ReturnValue;  
  
    ReturnValue = ((ULONGLONG)RtlpHeapGenerateRandomValue32()) << 32;  
    ReturnValue |= RtlpHeapGenerateRandomValue32();  
  
    return ReturnValue;  
}  
  
#ifdef _HEAP_DEBUG  
  
//  //  Fault injection code for internal mechanism testing on low memory  //    
volatile LONG RtlpHeapFaultRate = 0;  
  
NTSTATUS  
_RtlpAllocateVirtualMemoryHeap (  
    __in HANDLE ProcessHandle,  
    __inout PVOID *BaseAddress,  
    __in ULONG_PTR ZeroBits,  
    __inout PSIZE_T RegionSize,  
    __in ULONG AllocationType,  
    __in ULONG Protect  
    )  
{  
    static LONG FaultSeed = 0;  
    LONG FaultRate;  
  
    FaultRate = ReadNoFence(&RtlpHeapFaultRate);  
    if ((FaultRate == 0)  
            ||  
        (RtlRandom(&FaultSeed) % FaultRate) != 0) {  
  
        return NtAllocateVirtualMemory( ProcessHandle,  
                                        BaseAddress,  
                                        ZeroBits,  
                                        RegionSize,  
                                        AllocationType,  
                                        Protect );  
    }  
  
    return STATUS_INSUFFICIENT_RESOURCES;  
}  
  
#endif  // _HEAP_DEBUG    
View on WinOwn 
 More on reSearch | Other resources on the KM network KMEmail | KMBug WinSE | KMBug W8 | Windows SE Bot | KM on Codebox.         
        
KM Network 