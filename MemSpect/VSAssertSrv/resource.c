/*++    Copyright (c) 1989-1993  Microsoft Corporation    
Module Name:        resource.c    
Abstract:        This module implements the executive functions to acquire and release      a shared resource.    
Author:        Mark Lucovsky       (markl)     04-Aug-1989    
Environment:        These routines are statically linked in the caller's executable and      are callable only from user mode.  They make use of Nt system      services.    
Revision History:        Neill Clift (NeillC)        Fixed basic deadlocks in delayed criticalsection event initialization      Fixed Enter/Leave to not raise exception in low memory      Added anti-convoy logic          
    The critical section LockCount can be described by this tuple:        (WC=wc,W=w,L=l)        
    WC: Count of threads waiting for the lock      
    W: Waiter woken bit. Signifies that a single thread has been woken to acquire the lock      
    L: Lock bit, This is the actual lock bit        Lots of code assumes that the LockCount is a variable that is -1 when unlocked. To      
            maintain this all the bits are inverted. If L=1 for example the critical section is unlocked.      If no waiters are present wc=-1 etc.      We describe the general transition is this format.        
            Uncontended acquire:        (WC=wc,W=w,L=1) ---- EnterCriticalSection ----> (Wc=wc,W=w,L=0)        
            Uncontended release:        (WC=-1,W=w,L=0) ---- LeaveCriticalSection ----> (WC=-1,W=w,L=1)        
            Contended acquire:        (WC=wc,W=w,L=0) ---- EnterCriticalSection ----> (Wc=wc+1,W=w,L=0)        
            Contended release (waiter already woken):        In this case the waiter woken bit says that there is a thread      that has been woken (W=0) that is to acquire this lock. We don't      have to wake another becuase of this.        
                (WC=wc,W=0,L=0) ---- LeaveCriticalSection ----> (WC=wc,W=0,L=1)        
            Contended release (waiter not woken):        In this case we have threads waiting and nobody woken to attempt      to obtain the critical section. We wake a thread and set the waker      woken bit.        
                    (WC=wc,W=1,L=0) ---- LeaveCriticalSection ----> (WC=wc-1,W=0,L=1) wc != -1 and we wake a waiter          
            Waiter woken acquire:        In this case this thread is the woken waiter entering the critical section.      If we obtain it we must clear the waker woken bit.        
                (WC=wc,W=0,L=1) ---- Woken waiter acquires ----> (Wc=wc,W=1,L=0)        
                Waiter woken re-wait:        In this case this thread is the woken waiter is waiting again as it failed      to acquire the critical section.        (WC=wc,W=0,L=0) ---- Woken waiter sleeps again ----> (Wc=wc+1,W=1,L=0)      --*/    
  
#include <ntos.h>  
#include <ntrtl.h>  
#include <nturtl.h>  
#include "ldrp.h"  
#include <heap.h>  
#include <avrf.h>  
#include <stktrace.h>  
#include <wow64t.h>  
#include "ntetw.h"  
#include "ntdllp.h"  
#include <ntrtlp.h>  
  
#pragma optimize ("t", on)  
  
NTSTATUS  
RtlpEnterCriticalSectionContended(  
    IN PRTL_CRITICAL_SECTION CriticalSection  
    );  
  
HANDLE  
RtlpCreateDeferredCriticalSectionEvent(  
    IN PRTL_CRITICAL_SECTION CriticalSection  
    );  
  
VOID  
RtlpPossibleDeadlock (  
    IN PVOID ResourceOrCritSect  
    );  
  
VOID  
RtlpNotOwnerCriticalSection (  
    IN PRTL_CRITICAL_SECTION CriticalSection  
    );  
  
VOID  
RtlpUnWaitCriticalSection (  
    IN PRTL_CRITICAL_SECTION CriticalSection  
    );  
  
VOID  
RtlpAddDebugInfoToCriticalSection (  
    __in PRTL_CRITICAL_SECTION CriticalSection  
    );  
  
//  // Define the desired access for semaphores.  //    
#define DESIRED_EVENT_ACCESS \  
                (EVENT_QUERY_STATE | EVENT_MODIFY_STATE | SYNCHRONIZE)  
  
#define DESIRED_SEMAPHORE_ACCESS \  
                (SEMAPHORE_QUERY_STATE | SEMAPHORE_MODIFY_STATE | SYNCHRONIZE)  
  
VOID RtlDumpResource( IN PRTL_RESOURCE Resource );  
  
//  // A keyed event handle == NULL automatically defaults to the critical  // section out of memory event.  //    
HANDLE GlobalKeyedEventHandle=KEYEDEVENT_DEFAULT_HANDLE;  
BOOLEAN RtlpForceCSDebugInfoCreation;  
  
#define RTLP_KEYEDEVENT_ALIAS ((HANDLE)-1)  
#define RtlpIsKeyedEvent(xxHandle) (xxHandle == RTLP_KEYEDEVENT_ALIAS)  
//#define RTLP_USE_GLOBAL_KEYED_EVENT 1    
#define MAX_SLIST_DEPTH 10  
  
#define CS_LOCK_BIT          0x1  
#define CS_LOCK_BIT_V        0x0  
#define CS_LOCK_WAITER_WOKEN 0x2  
#define CS_LOCK_WAITER_INC   0x4  
  
#define CS_RETURN            0x0  
#define CS_RETURN_EXIT       0x1  
#define CS_RETURN_WOKEN      0x2  
  
#define CS_LOCK_INIT          -1  
#define CS_LOCK_UNCONTENDED   (CS_LOCK_INIT & ~(CS_LOCK_BIT))  
#define CS_INVALID_DEBUG_INFO ((PRTL_CRITICAL_SECTION_DEBUG)-1)  
  
#define MAX_SPIN_COUNT          0x00FFFFFF  
  
#define RTLP_CRITICAL_SECTION_ALL_FLAGS                 ~RTL_CRITICAL_SECTION_FLAG_RESERVED  
#define RTLP_CRITICAL_SECTION_FORCE_FLAGS \  
    (RTL_CRITICAL_SECTION_FLAG_NO_DEBUG_INFO | RTL_CRITICAL_SECTION_FLAG_RESOURCE_TYPE)  
#define RTLP_CRITICAL_SECTION_MINIMUM_DYNAMIC_SPINS         100L  
#define RTLP_CRITICAL_SECTION_MAXIMUM_DYNAMIC_SPINS         2000L  
  
#define SPINCOUNT_MASK(SpinCount)   ((SpinCount) & MAX_SPIN_COUNT)  
#define FLAGS_MASK(SpinCount)       ((SpinCount) & RTL_CRITICAL_SECTION_ALL_FLAG_BITS)  
  
BOOLEAN  
ProtectHandle (  
    IN HANDLE hObject  
    )  
{  
    NTSTATUS Status;  
    OBJECT_HANDLE_FLAG_INFORMATION HandleInfo;  
  
    Status = NtQueryObject (hObject,  
                            ObjectHandleFlagInformation,  
                            &HandleInfo,  
                            sizeof (HandleInfo),  
                            NULL);  
  
    if (NT_SUCCESS(Status)) {  
  
        HandleInfo.ProtectFromClose = TRUE;  
  
        Status = NtSetInformationObject (hObject,  
                                         ObjectHandleFlagInformation,  
                                         &HandleInfo,  
                                         sizeof (HandleInfo));  
        if (NT_SUCCESS( Status )) {  
            return TRUE;  
        }  
    }  
  
    return FALSE;  
}  
  
  
BOOLEAN  
UnProtectHandle (  
    IN HANDLE hObject  
    )  
{  
    NTSTATUS Status;  
    OBJECT_HANDLE_FLAG_INFORMATION HandleInfo;  
  
    Status = NtQueryObject (hObject,  
                            ObjectHandleFlagInformation,  
                            &HandleInfo,  
                            sizeof (HandleInfo),  
                            NULL);  
  
    if (NT_SUCCESS(Status)) {  
  
        HandleInfo.ProtectFromClose = FALSE;  
  
        Status = NtSetInformationObject (hObject,  
                                         ObjectHandleFlagInformation,  
                                         &HandleInfo,  
                                         sizeof (HandleInfo));  
        if (NT_SUCCESS(Status)) {  
            return TRUE;  
        }  
    }  
  
    return FALSE;  
}  
  
SLIST_HEADER RtlCriticalSectionDebugSList;  
RTL_CRITICAL_SECTION_DEBUG RtlpStaticDebugInfo[8];  
PRTL_CRITICAL_SECTION_DEBUG RtlpStaticDebugInfoEnd;  
  
PVOID  
RtlpAllocateDebugInfo (  
    VOID  
    )  
  
/*++    Routine Description:        This routine allocates a critsec debug info structure. It prefers to      allocate from the static debug info array, but falls back on the      process heap if the array is completely full.    Arguments:        None.    Return Value:        The address of a new debug info structure.    --*/    
{  
    PVOID p;  
  
    p = RtlInterlockedPopEntrySList(&RtlCriticalSectionDebugSList);  
  
    if (p == NULL) {  
        PVOID ProcessHeap = NtCurrentPeb()->ProcessHeap;  
  
        NT_ASSERT (ProcessHeap != NULL);  
  
        if (ProcessHeap != NULL) {  
            p = RtlAllocateHeap (ProcessHeap,  
                                 0,  
                                 sizeof(RTL_CRITICAL_SECTION_DEBUG));  
  
            if (p == NULL) {  
                KdPrint(("NTDLL: Unable to allocate debug information from heap\n"));  
            }  
        }  
    }  
  
    return p;  
}  
  
C_ASSERT (sizeof (RTL_CRITICAL_SECTION_DEBUG) == sizeof (RTL_RESOURCE_DEBUG));  
  
VOID  
RtlpFreeDebugInfo (  
    IN PRTL_CRITICAL_SECTION_DEBUG DebugInfo  
    )  
  
/*++    Routine Description:        This routine releases an unused debug info structure, either to the process      heap (if it was dynamically allocated) or to the S-list of static debug      info blocks (if it comes from the corresponding array).    Arguments:        DebugInfo - Supplies a pointer to the debug info structure to free.    Return Value:        None.    --*/    
{  
  
    PVOID Trace;  
  
    NT_ASSERT (RtlpStaticDebugInfoEnd != NULL);  
    NT_ASSERT (DebugInfo != CS_INVALID_DEBUG_INFO);  
  
    //      // Free the associated backtrace, if one exists.      //    
    Trace = RtlpGetStackTraceAddressEx(DebugInfo->CreatorBackTraceIndex,  
                                       DebugInfo->CreatorBackTraceIndexHigh);  
  
    if (Trace != NULL) {  
        RtlReleaseStackTrace(Trace);  
    }  
  
    //      // If this debug info was statically allocated, push it to the S-list      // of free static debug info structures. Otherwise, free it back to      // the heap.      //      // N.B. FUTURE - The low-fragmentation heap caches heap-allocated debug info      //      structures in a way that may make the first clause of this check      //      unnecessary.      //    
    if ((RtlQueryDepthSList(&RtlCriticalSectionDebugSList) < MAX_SLIST_DEPTH) ||  
        ((DebugInfo >= RtlpStaticDebugInfo) &&  
         (DebugInfo < RtlpStaticDebugInfoEnd))) {  
  
        RtlInterlockedPushEntrySList(&RtlCriticalSectionDebugSList,  
                                     (PSLIST_ENTRY)DebugInfo);  
  
    } else {  
        RtlFreeHeap(NtCurrentPeb()->ProcessHeap, 0, DebugInfo);  
    }  
  
    return;  
}  
  
NTSTATUS  
RtlpInitDeferredCriticalSection (  
    VOID  
    )  
  
/*++    Routine Description:        This routine creates the initial S-list of static debug info blocks      and populates it with a few entries. NTDLL routines can then use      these info blocks before the process heap is created.    Arguments:        None.    Return Value:        NTSTATUS.    --*/    
{  
    ULONG Size;  
    PRTL_CRITICAL_SECTION_DEBUG p;  
  
    RtlFailedCriticalDebugAllocations = 0;  
  
    //      // Initialize the S-list header and get pointers to the first and final      // entries in the static array.      //    
    RtlInitializeSListHead(&RtlCriticalSectionDebugSList);  
    Size = sizeof(RtlpStaticDebugInfo) / sizeof(RtlpStaticDebugInfo[0]);  
    RtlpStaticDebugInfoEnd = RtlpStaticDebugInfo + Size;  
  
    //      // Chain all of the debug info blocks together. The list is not circular,      // so ignore the very last entry (it should point to NULL).      //    
    for (p = RtlpStaticDebugInfo; p <= RtlpStaticDebugInfo + Size - 2; p++) {  
        ((PSLIST_ENTRY)p)->Next = (PSLIST_ENTRY)(p + 1);  
    }  
  
    ((PSLIST_ENTRY)p)->Next = NULL;  
  
    //      // Push the entire list as a set.      //    
    InterlockedPushListSList(&RtlCriticalSectionDebugSList,  
                             (PSLIST_ENTRY)&RtlpStaticDebugInfo[0],  
                             (PSLIST_ENTRY)&RtlpStaticDebugInfo[Size - 1],  
                             Size);  
  
    return STATUS_SUCCESS;  
}  
  
  
LOGICAL  
RtlIsCriticalSectionLocked (  
    IN PRTL_CRITICAL_SECTION CriticalSection  
    )  
  
/*++    Routine Description:        This routine checks to see whether a critical section is presently locked.    Arguments:        CriticalSection - Supplies a pointer to the critsec to check.    Return Value:        TRUE if the critsec is locked, FALSE otherwise.    --*/  {  
  
    if ((CriticalSection->LockCount & CS_LOCK_BIT) != 0) {  
        return FALSE;  
  
    } else {  
        return TRUE;  
    }  
}  
  
LOGICAL  
RtlIsCriticalSectionLockedByThread (  
    IN PRTL_CRITICAL_SECTION CriticalSection  
    )  
  
/*++    Routine Description:        This routine checks to see if a critical section is owned by the current      thread.    Arguments:        CriticalSection - Supplies a pointer to the critsec to check.    Return Value:        TRUE if the critsec is locked by the current thread, FALSE otherwise.    --*/    
{  
  
    PTEB Teb;  
  
    Teb = NtCurrentTeb();  
    if (CriticalSection->OwningThread == Teb->ClientId.UniqueThread) {  
        return TRUE;  
  
    } else {  
        return FALSE;  
    }  
}  
  
ULONG  
RtlGetCriticalSectionRecursionCount (  
    IN PRTL_CRITICAL_SECTION CriticalSection  
    )  
  
/*++    Routine Description:        This routine returns the number of times the current thread has      successfully acquired a specified critical section.    Arguments:        CriticalSection - Supplies a pointer to the critsec to check.    Return Value:        If the current thread owns the critical section, the number of times it      has acquired the critsec, including recursive acquires. Otherwise, 0.    --*/    
{  
  
    if (CriticalSection->OwningThread == NtCurrentTeb()->ClientId.UniqueThread) {  
        return CriticalSection->RecursionCount;  
  
    } else {  
        return 0;  
    }  
}  
  
VOID  
RtlpWaitForCriticalSection (  
    IN PRTL_CRITICAL_SECTION CriticalSection  
    )  
{  
    UNREFERENCED_PARAMETER(CriticalSection);  
    return;  
}  
  
BOOLEAN  
RtlpWaitCouldDeadlock (  
    VOID  
    )  
  
/*++    Routine Description:        This routine determines whether waiting on a resource could cause      deadlock by checking to see if a shutdown that could terminate the      resource's owning thread is in progress.    Arguments:        None.    Return Value:        TRUE if it is potentially unsafe to wait, FALSE otherwise.    --*/    
{  
  
#if defined(_WIN64)  
  
    PPEB_LDR_DATA32 PebLdr32;  
  
#endif  
  
    if (PebLdr.ShutdownInProgress != FALSE) {  
        return TRUE;  
    }  
  
#if defined(_WIN64)  
  
    else if (UseWOW64 != FALSE) {  
        PebLdr32 = (PPEB_LDR_DATA32)(ULONG_PTR) NtCurrentPeb32()->Ldr;  
        if ((PebLdr32 != NULL) && (PebLdr32->ShutdownInProgress != FALSE)) {  
            return TRUE;  
        }  
    }  
  
#endif  
  
    return FALSE;  
}  
  
LONG  
RtlpWaitOnCriticalSection (  
    IN PRTL_CRITICAL_SECTION CriticalSection,  
    IN LONG Increment  
    )  
  
/*++    Routine Description:        This routine attempts to wait on a critical section's event. Callers      to this routine attempt to atomically increment the number of waiters      for the critsec. If a thread succeeds in doing so, it blocks on the      underlying event and waits to be woken.    Arguments:        CriticalSection - Supplies the critical section on which to wait.        Increment - Supplies the value that should be subtracted from the lock          count in the critical section when attempting to add this thread          to the count of waiters.    Return Value:        CS_RETURN_EXIT if the critsec is the loader lock and the process is      exiting.        CS_RETURN if the lock appears to be acquirable without waiting.        CS_RETURN_WOKEN if the current thread waited and was woken.    --*/    
{  
    NTSTATUS st;  
    ULONG TimeoutCount = 0;  
    PLARGE_INTEGER TimeoutTime;  
    LOGICAL CsIsLoaderLock;  
    HANDLE LockSemaphore;  
    PTEB Teb;  
    LONG OldValue, NewValue;  
    LONG OldContention, NewContention;  
  
    //      // If the target critsec is the loader lock, mark this thread      // accordingly.      //    
    CsIsLoaderLock = (CriticalSection == &LdrpLoaderLock);  
    Teb = NtCurrentTeb();  
    if (CsIsLoaderLock != FALSE) {  
        Teb->WaitingOnLoaderLock = TRUE;  
    }  
  
    //      // If the process is exiting, do not block on the critical section.      // This prevents hangs when critsec-owning threads are terminated      // before relinquishing control of their resources.      //    
    if (RtlpWaitCouldDeadlock() != FALSE) {  
        NtTerminateProcess(NtCurrentProcess(), STATUS_THREAD_IS_TERMINATING);  
    }  
  
    if (RtlpTimeoutDisable) {  
        TimeoutTime = NULL;  
    } else {  
        TimeoutTime = &RtlpTimeout;  
    }  
  
    //      // Get the underlying semaphore object for this critsec. If no object      // exists, create one.      //    
    LockSemaphore = CriticalSection->LockSemaphore;  
    if (LockSemaphore == NULL) {  
        LockSemaphore = RtlpCreateDeferredCriticalSectionEvent(CriticalSection);  
    }  
  
    if ((CriticalSection->DebugInfo == CS_INVALID_DEBUG_INFO) &&  
        ((CriticalSection->SpinCount & RTL_CRITICAL_SECTION_FLAG_NO_DEBUG_INFO) == 0)) {  
  
        RtlpAddDebugInfoToCriticalSection(CriticalSection);  
    }  
  
    NT_ASSERT (Increment == CS_LOCK_WAITER_INC || Increment == CS_LOCK_WAITER_WOKEN);  
  
    //      // Attempt to add the current thread to the waiter count. There are      // two possibilities:      //      // 1. This thread has never waited before. The increment should be      //    CS_LOCK_WAITER_INC.      // 2. This thread previously waited and was woken but was unable to      //    acquire the critsec. The waiter woken bit should be cleared      //    in this case, since this thread is the only one that can validly      //    set the bit.      //      // Continue to attempt this operation until it succeeds.      //    
    while (1) {  
        OldValue = ReadNoFence(&CriticalSection->LockCount);  
  
        NT_ASSERT ((Increment == CS_LOCK_WAITER_INC) ||  
                   ((OldValue & CS_LOCK_WAITER_WOKEN) == 0));  
  
        //          // Return right away if the lock does not appear to be held.          //    
        if ((OldValue & CS_LOCK_BIT) != 0) {  
            return CS_RETURN;  
        }  
  
        NewValue = OldValue - Increment;  
  
        NT_ASSERT ((Increment == CS_LOCK_WAITER_INC) ||  
                   ((NewValue & CS_LOCK_WAITER_WOKEN) != 0));  
        NT_ASSERT ((NewValue & CS_LOCK_BIT) == 0);  
        NT_ASSERT ((OldValue & CS_LOCK_BIT) == 0);  
  
        //          // No fence is required for this interlock, as the only fencing          // requirement is that it must occur before this thread waits in          // the kernel.  The kernel wait primitives already have a full barrier.          //    
        if (InterlockedCompareExchangeNoFence(&CriticalSection->LockCount,  
                                              NewValue,  
                                              OldValue) == OldValue) {  
  
            break;  
        }  
    }  
  
    //      // This thread must now wait on the semaphore. Increment the      // contention counter for this critsec.      //    
    NewContention = OldContention = 0;  
    if (CriticalSection->DebugInfo != CS_INVALID_DEBUG_INFO) {  
        CriticalSection->DebugInfo->ContentionCount += 1;  
    }  
  
    //      // Wait until this thread is successfully woken from blocking on the      // semaphore.      //    
    do {  
        if (IS_CRITSEC_LOGGING_ENABLED()){  
  
            ETW_CRITSEC_EVENT_COLLISION CritsecCollisionEvent;  
  
            CritsecCollisionEvent.Header.Packet.HookId = PERFINFO_LOG_TYPE_CRITSEC_COLLISION;  
  
            CritsecCollisionEvent.Address = (PVOID)CriticalSection;  
            CritsecCollisionEvent.SpinCount = (ULONG)SPINCOUNT_MASK(CriticalSection->SpinCount);  
            CritsecCollisionEvent.LockCount = CriticalSection->LockCount;  
            CritsecCollisionEvent.OwningThread = (PVOID)CriticalSection->OwningThread;  
  
            NtTraceEvent((HANDLE)(ULONG_PTR)CRITSEC_LOGGER_ID,  
                         ETW_USER_EVENT_WITH_STACKWALK(2),  
                         sizeof(ETW_CRITSEC_EVENT_COLLISION) - sizeof(SYSTEM_TRACE_HEADER),  
                         &CritsecCollisionEvent);  
  
        }  
  
        if (!RtlpIsKeyedEvent(LockSemaphore)) {  
            st = NtWaitForSingleObject(LockSemaphore,  
                                       FALSE,  
                                       TimeoutTime);  
        } else {  
            st = NtWaitForKeyedEvent(GlobalKeyedEventHandle,  
                                     CriticalSection,  
                                     FALSE,  
                                     TimeoutTime);  
        }  
  
        //          // The wait can time out if timeouts are enabled. When this occurs,          // apply some heuristics to decide if the timeout indicates a possible          // deadlock.          //    
        if (st == STATUS_TIMEOUT) {  
            DbgPrintEx(DPFLTR_DEFAULT_ID,  
                       DPFLTR_WARNING_LEVEL,  
                       "RTL: Enter Critical Section Timeout (%I64u secs) %d\n",  
                       TimeoutTime->QuadPart / (-10000000),  
                       TimeoutCount);  
  
            DbgPrintEx(DPFLTR_DEFAULT_ID,  
                       DPFLTR_ERROR_LEVEL,  
                       "RTL: Pid.Tid %x.%x, owner tid %x Critical Section %p - ContentionCount == %lu\n",  
                       Teb->ClientId.UniqueProcess,  
                       Teb->ClientId.UniqueThread,  
                       CriticalSection->OwningThread,  
                       CriticalSection,  
                       (CriticalSection->DebugInfo != CS_INVALID_DEBUG_INFO ?  
                            CriticalSection->DebugInfo->ContentionCount : 0));  
  
            TimeoutCount += 1;  
            if (CriticalSection->DebugInfo != CS_INVALID_DEBUG_INFO) {  
#if DBG  
                NewContention = CriticalSection->DebugInfo->EntryCount;  
#else  
                NewContention = CriticalSection->DebugInfo->ContentionCount;  
#endif  
            }  
  
            //              // If more than two timeouts have occurred and no additional              // threads appear to want to contend for the lock, report a              // possible deadlock. Do not, however, do this in the case of              // the loader lock, for which hold times might be rather long.              //    
            if ((TimeoutCount > 2) && (CriticalSection != &LdrpLoaderLock) &&  
                (NewContention == OldContention)) {  
  
                RtlpPossibleDeadlock((PVOID)CriticalSection);  
            }  
  
            OldContention = NewContention;  
            DbgPrintEx(DPFLTR_DEFAULT_ID,  
                       DPFLTR_ERROR_LEVEL,  
                       "RTL: Re-Waiting\n");  
  
        } else {  
            if (NT_SUCCESS(st)) {  
                if (CsIsLoaderLock) {  
                    Teb->WaitingOnLoaderLock = 0;  
                    LdrpLogEvent(PERFINFO_LOG_TYPE_LDR_LOCK_ACQUIRE_WAIT);  
                }  
  
                return CS_RETURN_WOKEN;  
            }  
  
            RtlRaiseStatus(st);  
        }  
  
    } while (TRUE);  
}  
  
LOGICAL  
RtlTryEnterCriticalSection (  
    IN PRTL_CRITICAL_SECTION CriticalSection  
    )  
  
/*++    Routine Description:        This routine attempts to enter a critical section and returns immediately      regardless of whether the critsec was acquired.    Arguments:        CriticalSection - Supplies a pointer to the critsec to acquire.    Return Value:        TRUE if the critsec was acquired, FALSE otherwise.    --*/    
{  
  
    PTEB Teb;  
  
#if DBG  
    PRTL_CRITICAL_SECTION_DEBUG DebugInfo;  
#endif  
  
    Teb = NtCurrentTeb();  
  
    //      // Attempt to acquire the lock directly by clearing the lock bit, which      // also ensures that the first operation on the desired cache line is a      // write.      //    
    if (InterlockedBitTestAndResetAcquire(&CriticalSection->LockCount,  
                                          CS_LOCK_BIT_V)) {  
  
        NT_ASSERT((CriticalSection->LockCount & CS_LOCK_BIT) == 0);  
        NT_ASSERT(CriticalSection->OwningThread == NULL);  
  
        CriticalSection->OwningThread = Teb->ClientId.UniqueThread;  
        CriticalSection->RecursionCount = 1;  
  
#if DBG  
        Teb->CountOfOwnedCriticalSections += 1;  
        DebugInfo = CriticalSection->DebugInfo;  
        if (DebugInfo != CS_INVALID_DEBUG_INFO) {  
            DebugInfo->EntryCount += 1;  
        }  
#endif  
  
        return TRUE;  
    }  
  
    //      // The critical section is presently owned, but it can be reacquired      // recursively.      //    
    if (CriticalSection->OwningThread == Teb->ClientId.UniqueThread) {  
  
        NT_ASSERT((CriticalSection->LockCount & CS_LOCK_BIT) == 0);  
        NT_ASSERT(CriticalSection->RecursionCount > 0);  
  
#if DBG  
        DebugInfo = CriticalSection->DebugInfo;  
        if (DebugInfo != CS_INVALID_DEBUG_INFO) {  
            DebugInfo->EntryCount += 1;  
        }  
#endif  
  
        CriticalSection->RecursionCount += 1;  
        return TRUE;  
    }  
  
    return FALSE;  
}  
  
NTSTATUS  
RtlEnterCriticalSection (  
    IN PRTL_CRITICAL_SECTION CriticalSection  
    )  
  
/*++    Routine Description:        This function enters a critical section.    Arguments:        CriticalSection - Supplies a pointer to the critsec to enter.    Return Value:        NTSTATUS.    --*/    
{  
  
    PTEB Teb;  
  
#if DBG  
    PRTL_CRITICAL_SECTION_DEBUG DebugInfo;  
#endif  
  
    Teb = NtCurrentTeb();  
  
    //      // Attempt to clear the critsec's lock bit. The first write to the      // critsec's cache line is a write. If the lock transitions from      // unacquired to acquired, this thread is the owner, so update the      // recursion count and return TRUE.      //    
    if (InterlockedBitTestAndResetAcquire(&CriticalSection->LockCount, CS_LOCK_BIT_V)) {  
  
        NT_ASSERT((CriticalSection->LockCount & CS_LOCK_BIT) == 0);  
        NT_ASSERT(CriticalSection->OwningThread == NULL);  
  
        CriticalSection->OwningThread = Teb->ClientId.UniqueThread;  
        CriticalSection->RecursionCount = 1;  
  
#if DBG  
        Teb->CountOfOwnedCriticalSections += 1;  
        DebugInfo = CriticalSection->DebugInfo;  
        if (DebugInfo != CS_INVALID_DEBUG_INFO) {  
            DebugInfo->EntryCount += 1;  
        }  
#endif  
  
        return STATUS_SUCCESS;  
    }  
  
    //      // The critical section is presently owned, but it can be reacquired      // recursively.      //    
    if (CriticalSection->OwningThread == Teb->ClientId.UniqueThread) {  
  
        NT_ASSERT((CriticalSection->LockCount & CS_LOCK_BIT) == 0);  
        NT_ASSERT(CriticalSection->RecursionCount > 0);  
  
#if DBG  
        DebugInfo = CriticalSection->DebugInfo;  
        if (DebugInfo != CS_INVALID_DEBUG_INFO) {  
            DebugInfo->EntryCount += 1;  
        }  
#endif  
  
        CriticalSection->RecursionCount += 1;  
  
        return STATUS_SUCCESS;  
    }  
  
    //      // This critical section could not be acquired in one shot, so enter      // the contended path.      //    
    return RtlpEnterCriticalSectionContended (CriticalSection);  
}  
  
DECLSPEC_NOINLINE  
NTSTATUS  
RtlpEnterCriticalSectionContended (  
    IN PRTL_CRITICAL_SECTION CriticalSection  
    )  
  
/*++    Routine Description:        This routine enters a critical section, waiting on the critical section      if this is required. It is marked as a no-inline routine to keep its      various initialization instructions off of the fast, uncontended path      through critical section entry.    Arguments:        CriticalSection - Supplies a pointer to the critsec to enter.    Return Value:        NTSTATUS.    --*/    
{  
  
    LONG BitsToChange;  
    ULONG_PTR Flags;  
    LONG NewValue;  
    LONG OldValue;  
    ULONG_PTR NewSpinCount;  
    ULONG_PTR SpinCount;  
    PTEB Teb;  
    LONG WaitInc;  
    LONG WaitRet;  
  
#if DBG  
    PRTL_CRITICAL_SECTION_DEBUG DebugInfo;  
#endif  
  
    Teb = NtCurrentTeb();  
  
    //      // The critsec could not be acquired in one go. If this is a statically-      // allocated critsec, add it to the process critsec list if it is missing.      // (The extra time required here is not critical, since the current thread      // will have to spin and/or wait anyway.)      //    
    if ((CriticalSection->SpinCount & RTL_CRITICAL_SECTION_FLAG_STATIC_INIT) &&  
        (CriticalSection->DebugInfo != CS_INVALID_DEBUG_INFO) &&  
        (CriticalSection->DebugInfo->ProcessLocksList.Flink == NULL)) {  
  
        NewSpinCount = CriticalSection->SpinCount;  
        if ((NewSpinCount & RTL_CRITICAL_SECTION_FLAG_DYNAMIC_SPIN) &&  
            (SPINCOUNT_MASK(NewSpinCount) == 0)) {  
  
            NewSpinCount |= RTLP_CRITICAL_SECTION_MAXIMUM_DYNAMIC_SPINS;  
        }  
  
        if (NtCurrentPeb()->NumberOfProcessors == 1) {  
            NewSpinCount = FLAGS_MASK(NewSpinCount);  
        }  
  
        RtlAcquireSRWLockExclusive(&RtlCriticalSectionLock);  
        if (CriticalSection->DebugInfo->ProcessLocksList.Flink == NULL) {  
            CriticalSection->SpinCount = NewSpinCount;  
            InsertTailList(&RtlCriticalSectionList, &CriticalSection->DebugInfo->ProcessLocksList);  
        }  
  
        RtlReleaseSRWLockExclusive(&RtlCriticalSectionLock);  
    }  
  
    //      // This thread has never waited before, so it must leave the waiter-woken      // bit alone.      //    
    BitsToChange = CS_LOCK_BIT;  
    WaitInc = CS_LOCK_WAITER_INC;  
    NewSpinCount = CriticalSection->SpinCount;  
    Flags = FLAGS_MASK(NewSpinCount);  
    NewSpinCount = SPINCOUNT_MASK(NewSpinCount);  
    while (1) {  
  
        NT_ASSERT((BitsToChange == CS_LOCK_BIT) ||  
                  (BitsToChange == (CS_LOCK_BIT|CS_LOCK_WAITER_WOKEN)));  
  
        NT_ASSERT((WaitInc == CS_LOCK_WAITER_INC) || (WaitInc == CS_LOCK_WAITER_WOKEN));  
  
        //          // If the critical section contains a spin count, capture it and          // begin spinning.          //    
        SpinCount = NewSpinCount;  
  
        //          // At each spin, capture the present value of the lock count and          // try to acquire the lock by modifying the appropriate bits.          //    
        while (SpinCount > 0) {  
            OldValue = ReadNoFence(&CriticalSection->LockCount);  
            while ((OldValue & CS_LOCK_BIT) != 0) {  
  
                NT_ASSERT((BitsToChange == CS_LOCK_BIT) ||  
                          ((OldValue & CS_LOCK_WAITER_WOKEN) == 0));  
  
                NewValue = OldValue ^ BitsToChange;  
  
                NT_ASSERT((NewValue & CS_LOCK_BIT) == 0);  
  
                //                  // Attempt to clear the lock bit (and set the waiter-woken                  // bit if necessary). If this succeeds, this thread is the                  // current lock owner. If dynamic spin is in use when this                  // occurs, increase the spin count, since there is now                  // additional evidence that hold times are short enough to                  // make spinning worthwhile.                  //    
                if ((NewValue = InterlockedCompareExchangeAcquire(&CriticalSection->LockCount,  
                                                                  NewValue,  
                                                                  OldValue)) == OldValue) {  
  
                    if ((Flags & RTL_CRITICAL_SECTION_FLAG_DYNAMIC_SPIN) &&  
                        (NewSpinCount < RTLP_CRITICAL_SECTION_MAXIMUM_DYNAMIC_SPINS)) {  
  
                        CriticalSection->SpinCount = (NewSpinCount + 1) | Flags;  
                    }  
  
exit_and_set_owner:  
  
                    NT_ASSERT((CriticalSection->LockCount & CS_LOCK_BIT) == 0);  
                    NT_ASSERT(CriticalSection->OwningThread == NULL);  
  
                    CriticalSection->OwningThread = Teb->ClientId.UniqueThread;  
                    CriticalSection->RecursionCount = 1;  
  
#if DBG  
                    Teb->CountOfOwnedCriticalSections += 1;  
                    DebugInfo = CriticalSection->DebugInfo;  
                    if (DebugInfo != CS_INVALID_DEBUG_INFO) {  
                        DebugInfo->EntryCount += 1;  
                    }  
#endif  
  
                    return STATUS_SUCCESS;  
                }  
  
                OldValue = NewValue;  
            }  
  
            YieldProcessor ();  
            SpinCount--;  
        }  
  
        //          // Attempt to clear the lock bit and acquire the lock as long          // as the lock seems to be acquirable. If the lock cannot be          // acquired, block.          //    
        OldValue = ReadNoFence(&CriticalSection->LockCount);  
        while ((OldValue & CS_LOCK_BIT) != 0) {  
  
            NT_ASSERT((BitsToChange == CS_LOCK_BIT) ||  
                      ((OldValue & CS_LOCK_WAITER_WOKEN) == 0));  
  
            NewValue = OldValue ^ BitsToChange;  
            if ((NewValue = InterlockedCompareExchangeAcquire(&CriticalSection->LockCount,  
                                                              NewValue,  
                                                              OldValue)) == OldValue) {  
  
                if ((Flags & RTL_CRITICAL_SECTION_FLAG_DYNAMIC_SPIN) &&  
                    (NewSpinCount < RTLP_CRITICAL_SECTION_MAXIMUM_DYNAMIC_SPINS)) {  
  
                    CriticalSection->SpinCount = (NewSpinCount + 1) | Flags;  
                }  
  
                goto exit_and_set_owner;  
            }  
  
            OldValue = NewValue;  
        }  
  
        //          // Spinning was ineffective, so if dynamic spins are enabled,          // decrement the spin count to reduce the amount of time wasted          // spinning.          //    
        if ((Flags & RTL_CRITICAL_SECTION_FLAG_DYNAMIC_SPIN) &&  
            (NewSpinCount > RTLP_CRITICAL_SECTION_MINIMUM_DYNAMIC_SPINS)) {  
  
            NewSpinCount--;  
        }  
  
        //          // Try to wait for the critical section. If the critsec was dropped          // before the wait began, repeat the previous steps using the same          // values, since this thread was never technically woken.          //          // If, however, this thread waited and was woken, it is responsible          // for ensuring that the waiter-woken state of the critsec is properly          // maintained. Change the values used to manipulate the critsec          // accordingly.          //    
        WaitRet = RtlpWaitOnCriticalSection(CriticalSection, WaitInc);  
        if (WaitRet == CS_RETURN_EXIT) {  
            goto exit_and_set_owner;  
  
        } else if (WaitRet == CS_RETURN_WOKEN) {  
  
            NT_ASSERT((CriticalSection->LockCount & CS_LOCK_WAITER_WOKEN) == 0);  
  
            BitsToChange = CS_LOCK_BIT | CS_LOCK_WAITER_WOKEN;  
            WaitInc = CS_LOCK_WAITER_WOKEN;  
  
            NT_ASSERT(CS_LOCK_WAITER_WOKEN * 2 == CS_LOCK_WAITER_INC);  
        }  
    }  
}  
  
NTSTATUS  
RtlLeaveCriticalSection(  
    IN PRTL_CRITICAL_SECTION CriticalSection  
    )  
  
/*++    Routine Description:        This function leaves a critical section.    Arguments:        CriticalSection - Supplies a pointer to the critsec to drop.    Return Value:       NTSTATUS. This routine raises an exception if an error occured.    --*/    
{  
  
    LONG Delta;  
    LONG NewValue;  
    LONG OldValue;  
    LONG TopValue;  
  
#if DBG  
  
    PTEB Teb;  
  
#endif  
  
#if DBG  
  
    Teb = NtCurrentTeb();  
  
    NT_ASSERT ((CriticalSection->LockCount&CS_LOCK_BIT) == 0);  
    NT_ASSERT (CriticalSection->OwningThread == Teb->ClientId.UniqueThread);  
  
    if (CriticalSection->OwningThread != Teb->ClientId.UniqueThread) {  
        RtlpNotOwnerCriticalSection (CriticalSection);  
        return STATUS_INVALID_OWNER;  
    }  
  
#endif  
  
    NT_ASSERT (CriticalSection->RecursionCount > 0);  
  
    //      // If the releasing thread has acquired this critsec more than once,      // return immediately instead of updating the lock state.      //    
    if (--CriticalSection->RecursionCount != 0) {  
        return STATUS_SUCCESS;  
    }  
  
    CriticalSection->OwningThread = NULL;  
  
#if DBG  
  
    NT_ASSERT (Teb->CountOfOwnedCriticalSections > 0);  
    Teb->CountOfOwnedCriticalSections--;  
  
#endif  
  
    //      // Most releases will be uncontended, so try an uncontended release first.      // If this works, there is no work left to do, so exit.      //    
    OldValue = InterlockedCompareExchangeRelease(&CriticalSection->LockCount,  
                                                 CS_LOCK_INIT,  
                                                 CS_LOCK_UNCONTENDED);  
  
    if (OldValue == CS_LOCK_UNCONTENDED) {  
        return STATUS_SUCCESS;  
    }  
  
    //      // If the release failed because the lock was not held, raise an error.      //    
    if ((OldValue & CS_LOCK_BIT) != 0) {  
        RtlpNotOwnerCriticalSection (CriticalSection);  
    }  
  
    //      // The waiter-woken bit is used to decide which thread gets to wake a      // waiter. If another thread owns the bit, simply try to drop the lock      // bit and leave. If no thread owns the bit, try to acquire it while      // dropping the lock bit and decrementing the waiter count.      //    
    while (TRUE) {  
        if ((OldValue & CS_LOCK_WAITER_WOKEN) == 0) {  
            Delta = CS_LOCK_BIT;  
  
        } else {  
            Delta = CS_LOCK_BIT - CS_LOCK_WAITER_WOKEN + CS_LOCK_WAITER_INC;  
        }  
  
        //          // No fence is required on this operation since the lock has already          // been released with appropriate fencing. The manipulation of the lock          // count must be before an attempt to unwait a thread. The barriers          // inherent in the unwait operation provide the correct ordering.          //    
        NewValue = OldValue + Delta;  
        TopValue = InterlockedCompareExchangeNoFence(&CriticalSection->LockCount,  
                                                     NewValue,  
                                                     OldValue);  
  
        if (TopValue == OldValue) {  
            if ((Delta & CS_LOCK_WAITER_WOKEN) != 0) {  
                RtlpUnWaitCriticalSection (CriticalSection);  
            }  
  
            break;  
        }  
  
        OldValue = TopValue;  
    }  
  
    return STATUS_SUCCESS;  
}  
  
VOID  
RtlInitializeResource (  
    IN PRTL_RESOURCE Resource  
    )  
  
/*++    Routine Description:        This routine initializes a run-time resource.    Arguments:        Resource - Supplies the resource to initialize.    Return Value:        None.    --*/    
{  
    PRTL_RESOURCE_DEBUG DebugInfo;  
    HANDLE ExclusiveSemaphore;  
    HANDLE SharedSemaphore;  
    ULONG StackTraceIndex;  
    NTSTATUS Status;  
  
    DebugInfo = RtlpAllocateDebugInfo();  
    if (DebugInfo == NULL) {  
        RtlRaiseStatus(STATUS_NO_MEMORY);  
    }  
  
    DebugInfo->ContentionCount = 0;  
    StackTraceIndex = RtlLogStackBackTraceEx(1);  
    DebugInfo->CreatorBackTraceIndex = (USHORT)StackTraceIndex;  
    DebugInfo->CreatorBackTraceIndexHigh = (USHORT)(StackTraceIndex >> 16);  
    Status = NtCreateSemaphore(&SharedSemaphore,  
                               DESIRED_SEMAPHORE_ACCESS,  
                               NULL,  
                               0,  
                               MAXLONG);  
  
    if (!NT_SUCCESS(Status)) {  
        RtlpFreeDebugInfo(DebugInfo);  
        RtlRaiseStatus(Status);  
    }  
  
    Status = NtCreateSemaphore(&ExclusiveSemaphore,  
                               DESIRED_SEMAPHORE_ACCESS,  
                               NULL,  
                               0,  
                               MAXLONG);  
  
    if (!NT_SUCCESS(Status)) {  
  
        NT_VERIFY(  
            NT_SUCCESS(  
                NtClose(SharedSemaphore)  
                ));  
  
        RtlpFreeDebugInfo(DebugInfo);  
        RtlRaiseStatus(Status);  
    }  
  
    Resource->SharedSemaphore = SharedSemaphore;  
    WriteULongRaw(&Resource->NumberOfWaitingShared, 0);  
    Resource->ExclusiveSemaphore = ExclusiveSemaphore;  
    WriteULongRaw(&Resource->NumberOfWaitingExclusive, 0);  
    WriteRaw(&Resource->NumberOfActive, 0);  
    Resource->ExclusiveOwnerThread = NULL;  
    Resource->Flags = 0;  
    Resource->DebugInfo = DebugInfo;  
  
    //      // Initialize the critical section.      //      // N.B. The resource must be fully initialized before it is inserted      //      into the global critical section list.      //      // N.B. A thread may read the lock type value before it is set below,      //      causing the entry to be read as a critical section rather than      //      a resource.      //    
    NT_VERIFY(  
        NT_SUCCESS(  
            RtlInitializeCriticalSectionEx(&Resource->CriticalSection,  
                                           0,  
                                           RTL_CRITICAL_SECTION_FLAG_RESOURCE_TYPE)  
            ));  
  
    RtlpAddDebugInfoToCriticalSection(&Resource->CriticalSection);  
    if (Resource->CriticalSection.DebugInfo != CS_INVALID_DEBUG_INFO) {  
        Resource->CriticalSection.DebugInfo->Type = RTL_RESOURCE_TYPE;  
    }  
  
    return;  
}  
  
VOID  
RtlpPossibleDeadlock (  
    IN PVOID ResourceOrCritSect  
    )  
  
/*++    Routine Description:        This routine reports a possible deadlock, raising an exception if      appropriate.    Arguments:        ResourceOrCritSect - Supplies a pointer to the object that could not          be acquired in a timely manner and that may be abandoned.    Return Value:        None.    --*/    
{  
    EXCEPTION_RECORD ExceptionRecord;  
    PRTLP_UNHANDLED_EXCEPTION_FILTER Filter;  
  
    Filter = RtlDecodePointer(RtlpUnhandledExceptionFilter);  
  
    if (Filter == NULL) {  
        Filter = (PRTLP_UNHANDLED_EXCEPTION_FILTER)RtlUnhandledExceptionFilter;  
    }  
  
    __try {  
  
        ExceptionRecord.ExceptionCode = STATUS_POSSIBLE_DEADLOCK;  
        ExceptionRecord.ExceptionFlags = 0;  
        ExceptionRecord.ExceptionRecord = NULL;  
        ExceptionRecord.ExceptionAddress = (PVOID)(ULONG_PTR) RtlRaiseException;  
        ExceptionRecord.NumberParameters = 1;  
        ExceptionRecord.ExceptionInformation[0] = (ULONG_PTR)ResourceOrCritSect;  
        RtlRaiseException (&ExceptionRecord);  
  
    } __except (Filter(GetExceptionInformation())) {  
        NOTHING;  
    }  
  
    return;  
}  
  
LONG  
RtlpNonNegativeDecrement (  
    IN PLONG NumberOfWaiting  
    )  
  
/*++    Routine Description:        This routine implements a bounded down counter. It decrements the      supplied value, but never to a value less than 0.    Arguments:        NumberOfWaiting - Supplies a pointer to the value to decrement.    Return Value:        The previous value of the counter.    --*/    
{  
    LONG OldValue, NewValue;  
  
    OldValue = ReadNoFence(NumberOfWaiting);  
    while (1) {  
        NT_ASSERT (OldValue >= 0);  
        if (OldValue > 0) {  
            NewValue = OldValue - 1;  
            if ((NewValue = InterlockedCompareExchange (NumberOfWaiting,  
                                                        NewValue,  
                                                        OldValue)) == OldValue) {  
                return OldValue;  
            }  
        } else {  
            return OldValue;  
        }  
  
        OldValue = NewValue;  
    }  
}  
  
BOOLEAN  
RtlAcquireResourceShared (  
    IN PRTL_RESOURCE Resource,  
    IN BOOLEAN Wait  
    )  
  
/*++    Routine Description:        The routine acquires the resource for shared access.  Upon return from      the procedure the resource is acquired for shared access.    Arguments:        Resource - Supplies the resource to acquire        Wait - Indicates if the call is allowed to wait for the resource          to become available or must return immediately    Return Value:        BOOLEAN - TRUE if the resource is acquired and FALSE otherwise    --*/    
{  
  
    LONG NewValue;  
    LONG OldValue;  
    NTSTATUS Status;  
    ULONG TimeoutCount;  
    PLARGE_INTEGER TimeoutTime;  
  
    OldValue = ReadNoFence(&Resource->NumberOfActive);  
  
    if ((OldValue < 0) &&  
        (Resource->ExclusiveOwnerThread ==  
            NtCurrentTeb()->ClientId.UniqueThread)) {  
  
        OldValue = InterlockedDecrement (&Resource->NumberOfActive);  
        NT_ASSERT (OldValue < -1);  
        return TRUE;  
    }  
  
    while (1) {  
  
        //          // The user mode resource package starves exclusive acquires. This enables it to handle          // recursive acquires without tracking multiple owners.          //    
        if (OldValue >= 0) {  
            NewValue = OldValue + 1;  
            if ((NewValue = InterlockedCompareExchange (&Resource->NumberOfActive,  
                                                        NewValue,  
                                                        OldValue)) == OldValue) {  
                return TRUE;  
            }  
  
            OldValue = NewValue;  
  
        } else {  
            //              //  Check if we are allowed to wait or must return immediately, and              //  indicate that we didn't acquire the resource              //    
            if (!Wait) {  
                return FALSE;  
            }  
  
            Resource->DebugInfo->ContentionCount += 1;  
  
            //              // Increment the number of waiting shared acquires. Once we increment              // this value we have to stand ready to wait on the semaphore.              //    
            OldValue = InterlockedIncrement ((PLONG)&Resource->NumberOfWaitingShared);  
            NT_ASSERT (OldValue > 0);  
  
            OldValue = ReadNoFence(&Resource->NumberOfActive);  
  
            if (OldValue < 0 ||  
                RtlpNonNegativeDecrement ((PLONG)&Resource->NumberOfWaitingShared) == 0) {  
  
                TimeoutCount = 0;  
  
rewait:  
                if (Resource->Flags & RTL_RESOURCE_FLAG_LONG_TERM) {  
                    TimeoutTime = NULL;  
                } else {  
                    TimeoutTime = &RtlpTimeout;  
                }  
  
                Status = NtWaitForSingleObject (Resource->SharedSemaphore,  
                                                FALSE,  
                                                TimeoutTime);  
  
                if (Status == STATUS_TIMEOUT) {  
  
                    DbgPrintEx(DPFLTR_DEFAULT_ID,  
                               DPFLTR_ERROR_LEVEL,  
                               "RTL: Acquire Shared Sem Timeout %d(%I64u secs)\n",  
                               TimeoutCount,  
                               TimeoutTime->QuadPart / (-10000000));  
  
                    DbgPrintEx(DPFLTR_DEFAULT_ID,  
                               DPFLTR_ERROR_LEVEL,  
                               "RTL: Resource at %p\n",  
                               Resource);  
  
                    TimeoutCount += 1;  
                    if (TimeoutCount > 2) {  
  
                        //                          // If the image is a Win32 image, then raise an exception                          // and try to get to the uae popup                          //    
                        RtlpPossibleDeadlock ((PVOID) Resource);  
                    }  
  
                    DbgPrintEx(DPFLTR_DEFAULT_ID,  
                               DPFLTR_ERROR_LEVEL,  
                               "RTL: Re-Waiting\n");  
                    goto rewait;  
                }  
  
                if (!NT_SUCCESS(Status)) {  
                    RtlRaiseStatus(Status);  
                }  
                OldValue = ReadNoFence(&Resource->NumberOfActive);  
            }  
        }  
    }  
}  
  
  
BOOLEAN  
RtlAcquireResourceExclusive (  
    IN PRTL_RESOURCE Resource,  
    IN BOOLEAN Wait  
    )  
  
/*++    Routine Description:        The routine acquires the resource for exclusive access.  Upon return from      the procedure the resource is acquired for exclusive access.    Arguments:        Resource - Supplies the resource to acquire        Wait - Indicates if the call is allowed to wait for the resource          to become available or must return immediately    Return Value:        BOOLEAN - TRUE if the resource is acquired and FALSE otherwise    --*/    
{  
    NTSTATUS Status;  
    ULONG TimeoutCount;  
    PLARGE_INTEGER TimeoutTime;  
    LONG OldValue, NewValue;  
  
    OldValue = ReadNoFence(&Resource->NumberOfActive);  
  
    if (OldValue < 0 && Resource->ExclusiveOwnerThread == NtCurrentTeb()->ClientId.UniqueThread) {  
        OldValue = InterlockedDecrement (&Resource->NumberOfActive);  
        NT_ASSERT (OldValue < -1);  
        return TRUE;  
    }  
  
    while (1) {  
        if (OldValue == 0) {  
            NewValue = OldValue - 1;  
            if ((NewValue = InterlockedCompareExchange (&Resource->NumberOfActive,  
                                                        NewValue,  
                                                        OldValue)) == OldValue) {  
                Resource->ExclusiveOwnerThread = NtCurrentTeb()->ClientId.UniqueThread;  
                return TRUE;  
            }  
  
            OldValue = NewValue;  
        } else {  
            //              //  Check if we are allowed to wait or must return immediately, and              //  indicate that we didn't acquire the resource              //    
            if (!Wait) {  
                return FALSE;  
            }  
  
            Resource->DebugInfo->ContentionCount += 1;  
  
            //              // Increment the number of waiting shared acquires. Once we increment              // this value we have to stand ready to wait on the semaphore.              //    
            OldValue = InterlockedIncrement ((PLONG)&Resource->NumberOfWaitingExclusive);  
            NT_ASSERT (OldValue > 0);  
  
            OldValue = ReadNoFence(&Resource->NumberOfActive);  
  
            if (OldValue != 0 ||  
                RtlpNonNegativeDecrement ((PLONG)&Resource->NumberOfWaitingExclusive) == 0) {  
  
                TimeoutCount = 0;  
  
rewait:  
                if (Resource->Flags & RTL_RESOURCE_FLAG_LONG_TERM) {  
                    TimeoutTime = NULL;  
                } else {  
                    TimeoutTime = &RtlpTimeout;  
                }  
  
                Status = NtWaitForSingleObject (Resource->ExclusiveSemaphore,  
                                                FALSE,  
                                                TimeoutTime);  
  
                if (Status == STATUS_TIMEOUT) {  
  
                    DbgPrintEx(DPFLTR_DEFAULT_ID,  
                               DPFLTR_ERROR_LEVEL,  
                               "RTL: Acquire Exclusive Sem Timeout %d (%I64u secs)\n",  
                               TimeoutCount,  
                               TimeoutTime->QuadPart / (-10000000));  
  
                    DbgPrintEx(DPFLTR_DEFAULT_ID,  
                               DPFLTR_ERROR_LEVEL,  
                               "RTL: Resource at %p\n",  
                               Resource);  
  
                    TimeoutCount += 1;  
  
                    if (TimeoutCount > 2) {  
  
                        //                          // If the image is a Win32 image, then raise an exception                          // and try to get to the uae popup                          //    
                        RtlpPossibleDeadlock ((PVOID) Resource);  
                    }  
                    DbgPrintEx(DPFLTR_DEFAULT_ID,  
                               DPFLTR_ERROR_LEVEL,  
                               "RTL: Re-Waiting\n");  
                    goto rewait;  
                }  
  
                if (!NT_SUCCESS(Status)) {  
                    RtlRaiseStatus(Status);  
                }  
                OldValue = ReadNoFence(&Resource->NumberOfActive);  
            }  
  
        }  
    }  
}  
  
  
VOID  
RtlReleaseResource (  
    IN PRTL_RESOURCE Resource  
    )  
  
/*++    Routine Description:        This routine release the input resource.  The resource can have been      acquired for either shared or exclusive access.    Arguments:        Resource - Supplies the resource to release    Return Value:        None.    --*/    
{  
    NTSTATUS Status;  
    LONG PreviousCount;  
    LONG OldValue;  
  
    OldValue = ReadNoFence(&Resource->NumberOfActive);  
  
    NT_ASSERT (OldValue != 0);  
  
    //      // If its acquired exclusive then increment the count.      //      if (OldValue < 0) {  
//        NT_ASSERT (Resource->ExclusiveOwnerThread == NtCurrentTeb()->ClientId.UniqueThread);    
        if (OldValue == -1) {  
            Resource->ExclusiveOwnerThread = NULL;  
        }  
  
        OldValue = InterlockedIncrement (&Resource->NumberOfActive);  
        NT_ASSERT (OldValue <= 0);  
        if (OldValue != 0) {  
            return;  
        }  
  
        if (ReadULongNoFence(&Resource->NumberOfWaitingShared) != 0) {  
            OldValue = InterlockedExchange ((PLONG)&Resource->NumberOfWaitingShared, 0);  
            if (OldValue != 0) {  
                Status = NtReleaseSemaphore (Resource->SharedSemaphore,  
                                             OldValue,  
                                             &PreviousCount);  
                if (!NT_SUCCESS (Status)) {  
                    RtlRaiseStatus (Status);  
                }  
            }  
        }  
  
        if (RtlpNonNegativeDecrement ((PLONG)&Resource->NumberOfWaitingExclusive) != 0) {  
            Status = NtReleaseSemaphore (Resource->ExclusiveSemaphore,  
                                         1,  
                                         &PreviousCount);  
  
            if (!NT_SUCCESS(Status)) {  
                RtlRaiseStatus(Status);  
            }  
        }  
  
    } else {  
        OldValue = InterlockedDecrement (&Resource->NumberOfActive);  
        NT_ASSERT (OldValue >= 0);  
        if (OldValue != 0) {  
            return;  
        }  
  
        if (RtlpNonNegativeDecrement ((PLONG)&Resource->NumberOfWaitingExclusive) != 0) {  
            Status = NtReleaseSemaphore (Resource->ExclusiveSemaphore,  
                                         1,  
                                         &PreviousCount);  
  
            if (!NT_SUCCESS(Status)) {  
                RtlRaiseStatus(Status);  
            }  
        }  
    }  
}  
  
  
VOID  
RtlConvertSharedToExclusive (  
    IN PRTL_RESOURCE Resource  
    )  
  
/*++    Routine Description:        This routine converts a resource acquired for shared access into      one acquired for exclusive access.  Upon return from the procedure      the resource is acquired for exclusive access    Arguments:        Resource - Supplies the resource to acquire for shared access, it          must already be acquired for shared access    Return Value:        None    --*/    
{  
    HANDLE Thread;  
    LONG OldValue, NewValue;  
  
    Thread = NtCurrentTeb()->ClientId.UniqueThread;  
  
    OldValue = ReadNoFence(&Resource->NumberOfActive);  
  
    NT_ASSERT (OldValue != 0);  
  
    //      // See if its already exclusively owned      //      if (OldValue < 0) {  
        NT_ASSERT (Resource->ExclusiveOwnerThread == Thread);  
        return;  
    }  
  
    while (1) {  
        if (OldValue == 1) {  
            NewValue = -1;  
            if ((NewValue = InterlockedCompareExchange (&Resource->NumberOfActive,  
                                                        NewValue,  
                                                        OldValue)) == OldValue) {  
                Resource->ExclusiveOwnerThread = NtCurrentTeb()->ClientId.UniqueThread;  
                return;  
            }  
            OldValue = NewValue;  
        } else {  
            RtlReleaseResource (Resource);  
            RtlAcquireResourceExclusive (Resource, TRUE);  
  
            return;  
        }  
    }  
}  
  
  
VOID  
RtlConvertExclusiveToShared (  
    IN PRTL_RESOURCE Resource  
    )  
  
/*++    Routine Description:        This routine converts a resource acquired for exclusive access into      one acquired for shared access.  Upon return from the procedure      the resource is acquired for shared access    Arguments:        Resource - Supplies the resource to acquire for shared access, it          must already be acquired for exclusive access    Return Value:        None    --*/    
{  
    LONG PreviousCount;  
    NTSTATUS Status;  
    LONG OldValue;  
  
    OldValue = ReadNoFence(&Resource->NumberOfActive);  
  
    NT_ASSERT (OldValue == -1);  
    NT_ASSERT (Resource->ExclusiveOwnerThread == NtCurrentTeb()->ClientId.UniqueThread);  
  
    Resource->ExclusiveOwnerThread = NULL;  
    OldValue = InterlockedExchange (&Resource->NumberOfActive, 1);  
  
    NT_ASSERT (OldValue == -1);  
  
    if (ReadULongNoFence(&Resource->NumberOfWaitingShared) != 0) {  
        OldValue = InterlockedExchange ((PLONG)&Resource->NumberOfWaitingShared, 0);  
        if (OldValue != 0) {  
            Status = NtReleaseSemaphore (Resource->SharedSemaphore,  
                                         OldValue,  
                                         &PreviousCount);  
            if (!NT_SUCCESS (Status)) {  
                RtlRaiseStatus (Status);  
            }  
        }  
    }  
  
    return;  
}  
  
  
VOID  
RtlDeleteResource (  
    IN PRTL_RESOURCE Resource  
    )  
  
/*++    Routine Description:        This routine deletes (i.e., uninitializes) the input resource variable      Arguments:        Resource - Supplies the resource variable being deleted    Return Value:        None    --*/    
{  
    RtlDeleteCriticalSection (&Resource->CriticalSection);  
  
    NtClose (Resource->SharedSemaphore);  
    NtClose (Resource->ExclusiveSemaphore);  
  
    RtlpFreeDebugInfo (Resource->DebugInfo);  
    RtlZeroMemory (Resource, sizeof(*Resource));  
  
    return;  
}  
  
  
VOID  
RtlDumpResource (  
    IN PRTL_RESOURCE Resource  
    )  
  
{  
    DbgPrint("Resource @ %lx\n", Resource);  
  
    DbgPrint(" NumberOfWaitingShared = %lx\n", ReadULongNoFence(&Resource->NumberOfWaitingShared));  
    DbgPrint(" NumberOfWaitingExclusive = %lx\n", ReadULongNoFence(&Resource->NumberOfWaitingExclusive));  
  
    DbgPrint(" NumberOfActive = %lx\n", ReadNoFence(&Resource->NumberOfActive));  
  
    return;  
}  
  
  
NTSTATUS  
RtlInitializeCriticalSection (  
    IN PRTL_CRITICAL_SECTION CriticalSection  
    )  
  
/*++    Routine Description:        This routine initializes a critical section with a default spin count of 0.    Arguments:        CriticalSection - Supplies the address of the critsec to initialize.    Return Value:        NTSTATUS.    --*/    
{  
    return RtlInitializeCriticalSectionAndSpinCount(CriticalSection, 0);  
}  
  
VOID  
RtlUpdateClonedCriticalSection (  
    __inout PRTL_CRITICAL_SECTION CriticalSection  
    )  
  
/*++    Routine Description:        A cloned thread invokes this routine to update a critical section      known to have been previously held by the thread that requested a clone.        Once this routine completes, the clone thread may successfully leave      the critical section.    Arguments:        CriticalSection - Supplies the resource variable being reinitialized.    Return Value:        None.    --*/    
{  
    CriticalSection->OwningThread = NtCurrentTeb ()->ClientId.UniqueThread;  
    CriticalSection->LockCount = (ULONG)-1 & (~CS_LOCK_BIT);  
    CriticalSection->RecursionCount = 1;  
    CriticalSection->LockSemaphore = NULL;  
}  
  
VOID  
RtlEnableEarlyCriticalSectionEventCreation (  
    VOID  
    )  
  
/*++    Routine Description:        This routine marks the PEB of the calling process so critical section events      are created at critical section creation time rather than at contention time.      This allows critical processes not to have to worry about error paths later      on at the expense of extra pool consumed.    Arguments:        None    Return Value:        None    --*/    
{  
    NtCurrentPeb()->NtGlobalFlag |= FLG_CRITSEC_EVENT_CREATION;  
}  
  
FORCEINLINE  
VOID  
RtlpAddDebugInfoToLockList (  
    __in PRTL_CRITICAL_SECTION_DEBUG DebugInfo  
    )  
  
/*++    Routine Description:        This routine adds a debug info structure to the process's lock list.    Arguments:        DebugInfo - Supplies the address of the structure to add to the list.    Return Value:        None.    --*/    
{  
  
    RtlAcquireSRWLockExclusive(&RtlCriticalSectionLock);  
    InsertTailList(&RtlCriticalSectionList, &DebugInfo->ProcessLocksList);  
    RtlReleaseSRWLockExclusive(&RtlCriticalSectionLock);  
}  
  
FORCEINLINE  
VOID  
RtlpInitCriticalSectionDebugInfo (  
    __in PRTL_CRITICAL_SECTION CriticalSection,  
    __out PRTL_CRITICAL_SECTION_DEBUG DebugInfo  
    )  
  
/*++    Routine Description:        This routine initializes and associates the passed debug info section      with the indicated critical section.    Arguments:        CriticalSection - Supplies the critical section to initialize.        DebugInfo - Supplies the debug info to initialize.    Return Value:        None.    --*/    
{  
  
    ULONG StackTraceIndex;  
  
    DebugInfo->Type = RTL_CRITSECT_TYPE;  
    if ((CriticalSection->SpinCount & RTL_CRITICAL_SECTION_FLAG_RESOURCE_TYPE) != 0) {  
        DebugInfo->Type = RTL_RESOURCE_TYPE;  
    }  
  
    DebugInfo->ContentionCount = 0;  
    DebugInfo->EntryCount = 0;  
    DebugInfo->CriticalSection = CriticalSection;  
    DebugInfo->Flags = 0;  
  
    //      // Try to get a stack trace. If no trace database was created      // then the log() function is a no op.      //    
    StackTraceIndex = RtlLogStackBackTraceEx(2);  
    DebugInfo->CreatorBackTraceIndex = (USHORT)StackTraceIndex;  
    DebugInfo->CreatorBackTraceIndexHigh = (USHORT)(StackTraceIndex >> 16);  
  
    return;  
}  
  
NTSTATUS  
RtlInitializeCriticalSectionEx (  
    IN PRTL_CRITICAL_SECTION CriticalSection,  
    ULONG SpinCount,  
    ULONG Flags  
    )  
  
/*++    Routine Description:        This routine initializes a critical section.    Arguments:        CriticalSection - Supplies the address of the critsec to initialize.        SpinCount - Supplies the spin count to use. This is ignored on uniprocessor          systems.        Flags - Supplies flag values that control the properties of the critsec.    Return Value:        NTSTATUS.    --*/    
{  
  
    BOOLEAN AllocateDebugInfo;  
  
    //      // Validate the spin count and flags.      //    
    if (((Flags & ~RTLP_CRITICAL_SECTION_ALL_FLAGS) != 0) ||  
        (((Flags & RTL_CRITICAL_SECTION_FLAG_NO_DEBUG_INFO) != 0) &&  
         ((Flags & RTL_CRITICAL_SECTION_FLAG_FORCE_DEBUG_INFO) != 0))) {  
  
        return STATUS_INVALID_PARAMETER_3;  
    }  
  
    if (SpinCount & RTL_CRITICAL_SECTION_ALL_FLAG_BITS) {  
        return STATUS_INVALID_PARAMETER_2;  
    }  
  
    //      // If this is a statically-initialized critical section, it must already      // have a valid debug info structure with the appropriate flags.      //    
    if (Flags & RTL_CRITICAL_SECTION_FLAG_STATIC_INIT) {  
        NT_ASSERT(CriticalSection->DebugInfo != NULL);  
        NT_ASSERT(CriticalSection->DebugInfo != CS_INVALID_DEBUG_INFO);  
        NT_ASSERT(CriticalSection->DebugInfo->Flags & RTL_CRITICAL_SECTION_DEBUG_FLAG_STATIC_INIT);  
  
        return STATUS_SUCCESS;  
    }  
  
    //      // Initialize the lock fields, the count indicates how many are waiting      // to enter or are in the critical section, LockSemaphore is the object      // to wait on when entering the critical section.  SpinLock is used      // for the add interlock instruction. Recursion count is the number of      // times the critical section has been recursively entered.      //    
    CriticalSection->LockCount = CS_LOCK_INIT;  
    CriticalSection->RecursionCount = 0;  
    CriticalSection->OwningThread = 0;  
    CriticalSection->LockSemaphore = 0;  
  
    //      // Set up the spin count on multiprocessor machines.      //    
    if ( NtCurrentPeb()->NumberOfProcessors > 1 ) {  
        if ((Flags & RTL_CRITICAL_SECTION_FLAG_DYNAMIC_SPIN) || (SpinCount == 0)) {  
            CriticalSection->SpinCount =  
                (RTL_CRITICAL_SECTION_FLAG_DYNAMIC_SPIN | RTLP_CRITICAL_SECTION_MAXIMUM_DYNAMIC_SPINS);  
  
        } else {  
            CriticalSection->SpinCount = SpinCount & MAX_SPIN_COUNT;  
        }  
  
    } else {  
        CriticalSection->SpinCount = 0;  
    }  
  
    //      // Create a debug info structure. If this value is set to FALSE, the      
    // creation of a debug info structure is deferred to the first time      
    // a thread must block when acquiring the critsec.      
    //      
    // This must be done with care to ensure that all relevant parties      
    // (e.g. the process heap) have access to a critical section debug      
    // info structure at the appropriate times.      //    
    CriticalSection->SpinCount |= Flags & RTLP_CRITICAL_SECTION_FORCE_FLAGS;  
  
#if DBG  
  
    if ((CriticalSection->SpinCount & RTL_CRITICAL_SECTION_FLAG_NO_DEBUG_INFO) == 0) {  
        AllocateDebugInfo = TRUE;  
  
    } else {  
        AllocateDebugInfo = FALSE;  
    }  
  
#else  
  
    if (((Flags & RTL_CRITICAL_SECTION_FLAG_FORCE_DEBUG_INFO) != 0) ||  
        (RtlpForceCSDebugInfoCreation != FALSE)) {  
  
        AllocateDebugInfo = TRUE;  
  
    } else {  
        AllocateDebugInfo = FALSE;  
    }  
  
#endif  
  
    //      // Initialize the critsec's debug info field if necessary. Note that      
    // the debug info pointer must be set to the invalid value here even      
    // if a debug info needs to be allocated, since the allocating routine      
    // assumes that critsecs to which a debug info is added have invalid      
    // debug info pointers.      //    
    CriticalSection->DebugInfo = CS_INVALID_DEBUG_INFO;  
    if (AllocateDebugInfo != FALSE) {  
        RtlpAddDebugInfoToCriticalSection(CriticalSection);  
  
        //          
        // If allocating the debug info at this point failed, do not try to          
        // allocate it later. This provides behavioral continuity with old          
        // implementations of this code, which would always try to allocate          
        // debug info structures up front and would never retry those          
        // allocations if they failed.          //    
        if (CriticalSection->DebugInfo == CS_INVALID_DEBUG_INFO) {  
            CriticalSection->SpinCount |=  
                RTL_CRITICAL_SECTION_FLAG_NO_DEBUG_INFO;  
        }  
    }  
  
    if (IS_CRITSEC_LOGGING_ENABLED()) {  
  
        ETW_CRITSEC_EVENT_INIT CritsecInitEvent;  
  
        CritsecInitEvent.Header.Packet.HookId = PERFINFO_LOG_TYPE_CRITSEC_INITIALIZE;  
  
        CritsecInitEvent.Address = (PVOID)CriticalSection;  
        CritsecInitEvent.SpinCount = (PVOID)CriticalSection->SpinCount;  
  
        NtTraceEvent((HANDLE)(ULONG_PTR)CRITSEC_LOGGER_ID,  
                     ETW_USER_EVENT_WITH_STACKWALK(1),  
                     sizeof(ETW_CRITSEC_EVENT_INIT) - sizeof(SYSTEM_TRACE_HEADER),  
                     &CritsecInitEvent);  
  
    }  
  
    return STATUS_SUCCESS;  
}  
  
  
NTSTATUS  
RtlInitializeCriticalSectionAndSpinCount (  
    IN PRTL_CRITICAL_SECTION CriticalSection,  
    ULONG SpinCount  
    )  
  
/*++    Routine Description:        This routine initializes a critical section with a given spin count.    Arguments:        CriticalSection - Supplies the critsec to initialize.        SpinCount - Supplies the spin count to use on multiprocessor machines.    Return Value:        NTSTATUS.    --*/  {  
    ULONG PrivateSpinCount = SpinCount & (~RTL_CRITICAL_SECTION_ALL_FLAG_BITS);  
  
    return RtlInitializeCriticalSectionEx(CriticalSection, PrivateSpinCount, 0);  
}  
  
ULONG  
RtlSetCriticalSectionSpinCount (  
    IN PRTL_CRITICAL_SECTION CriticalSection,  
    ULONG SpinCount  
    )  
  
/*++    Routine Description:        This routine sets the spin count of the provided critical section.    Arguments:        CriticalSection - Supplies the critsec to modify.        SpinCount - Supplies the spin count to use on multiprocessor machines.    Return Value:        The previous spin count.    --*/    
{  
    ULONG OldSpinCount;  
    ULONG NewSpinCount;  
  
    OldSpinCount = (ULONG)CriticalSection->SpinCount;  
    NewSpinCount = FLAGS_MASK(OldSpinCount) | (SpinCount & MAX_SPIN_COUNT);  
    if (NtCurrentPeb()->NumberOfProcessors == 1) {  
        NewSpinCount = FLAGS_MASK(NewSpinCount);  
    }  
  
    CriticalSection->SpinCount = NewSpinCount;  
    return SPINCOUNT_MASK(OldSpinCount);  
}  
  
VOID  
RtlpAddDebugInfoToCriticalSection (  
    __in PRTL_CRITICAL_SECTION CriticalSection  
    )  
  
/*++    Routine Description:        This routine adds a debug info structure to a critical section.    Arguments:        CriticalSection - Supplies a pointer to the critical section to which a          debug info block should be added.    Return Value:        None.    --*/    
{  
  
    PRTL_CRITICAL_SECTION_DEBUG DebugInfo;  
  
    NT_ASSERT((CriticalSection->SpinCount &  
                RTL_CRITICAL_SECTION_FLAG_NO_DEBUG_INFO) == 0);  
  
    DebugInfo = (PRTL_CRITICAL_SECTION_DEBUG)RtlpAllocateDebugInfo();  
    if (DebugInfo == NULL) {  
        InterlockedIncrement((PLONG)&RtlFailedCriticalDebugAllocations);  
        return;  
    }  
  
    //      // Ensure that the debug info structure is properly initialized before      // it is assigned to the current critsec and added to the process's lock      // list, since the list can theoretically be traversed at any time.      //    
    RtlpInitCriticalSectionDebugInfo(CriticalSection, DebugInfo);  
  
    //      // Attempt to change the critsec's debug info address from the invalid      // info value to the address of the allocated structure. If this succeeds,      // add this critical section to the process's critsec list and exit.      //    
    if (InterlockedCompareExchangePointer(  
                &((PVOID)CriticalSection->DebugInfo),  
                DebugInfo,  
                CS_INVALID_DEBUG_INFO) == CS_INVALID_DEBUG_INFO) {  
  
        RtlpAddDebugInfoToLockList(CriticalSection->DebugInfo);  
        return;  
    }  
  
    //      // Another thread set the debug info pointer for the current critsec.      //    
    RtlpFreeDebugInfo(DebugInfo);  
}  
  
HANDLE  
RtlpCreateDeferredCriticalSectionEvent (  
    IN PRTL_CRITICAL_SECTION CriticalSection  
    )  
  
/*++    Routine Description:        This routine checks to see if the initialization of a particular critical      section is complete yet and completes it if it is not.    Arguments:        CriticalSection - Supplies the critical section to initialize.    Return Value:        A HANDLE value which refers to the wait event or a keyed event.    --*/    
{  
  
    HANDLE Handle;  
    HANDLE OldHandle;  
    NTSTATUS Status;  
  
    //      // Attempt to create a semaphore event. If this fails due to a lack      // of resources, use the global keyed event instead.      //    
#if defined(RTLP_USE_GLOBAL_KEYED_EVENT)  
  
    Status = STATUS_INSUFFICIENT_RESOURCES;  
  
#else  
  
    Status = NtCreateEvent (&Handle,  
                            DESIRED_EVENT_ACCESS,  
                            NULL,  
                            SynchronizationEvent,  
                            FALSE);  
  
#endif  
  
    if (!NT_SUCCESS(Status)) {  
        Handle = RTLP_KEYEDEVENT_ALIAS;  
    }  
  
    //      // No fence is required for the publication of the lock event handle, since      // the only other globally observable memory accesses which need to be      // ordered with respect to this are event creation (ensured by the kernel)      // and the subsequent wait on the event (which also has barriers in the      // kernel).      //    
    OldHandle = InterlockedCompareExchangePointerNoFence(&CriticalSection->LockSemaphore,  
                                                         Handle,  
                                                         NULL);  
  
    if (OldHandle != NULL) {  
        if (Handle != RTLP_KEYEDEVENT_ALIAS) {  
            NT_VERIFY(NT_SUCCESS(  
                    NtClose(Handle)  
                    ));  
        }  
  
        Handle = OldHandle;  
  
    } else if (Handle != RTLP_KEYEDEVENT_ALIAS) {  
  
#if DBG  
        ProtectHandle(Handle);  
#endif  
  
    }  
  
    return Handle;  
}  
  
  
NTSTATUS  
RtlDeleteCriticalSection (  
    IN PRTL_CRITICAL_SECTION CriticalSection  
    )  
  
/*++    Routine Description:        This routine deletes a critical section and releases any associated      memory.    Arguments:        CriticalSection - Supplies the critsec to delete.    Return Value:        NTSTATUS.    --*/    
{  
  
    PRTL_CRITICAL_SECTION_DEBUG DebugInfo;  
    LOGICAL FreeDebugInfo;  
    HANDLE LockSemaphore;  
    NTSTATUS Status;  
  
    LockSemaphore = CriticalSection->LockSemaphore;  
  
    if (LockSemaphore != NULL && !RtlpIsKeyedEvent(LockSemaphore)) {  
  
#if DBG  
        UnProtectHandle(LockSemaphore);  
#endif  
  
        Status = NtClose(LockSemaphore);  
  
    } else {  
        Status = STATUS_SUCCESS;  
    }  
  
    //      // Free the debug info structure if it exists, releasing any associated      // stack trace information.      //      // N.B. This code ignores potential races between deleting a critsec      //      and creating a deferred debug info structure, since any operation      //      that can do the latter (e.g., waiting on the critsec) cannot      //      validly occur while the critsec is being deleted.      //    
    DebugInfo = CriticalSection->DebugInfo;  
    if ((DebugInfo != NULL) && (DebugInfo != CS_INVALID_DEBUG_INFO)) {  
        FreeDebugInfo =  
            ((CriticalSection->SpinCount & RTL_CRITICAL_SECTION_FLAG_STATIC_INIT) == 0);  
  
        RtlAcquireSRWLockExclusive(&RtlCriticalSectionLock);  
        __try {  
            if (DebugInfo->ProcessLocksList.Flink != NULL) {  
                RtlSafeRemoveEntryList(&DebugInfo->ProcessLocksList);  
            }  
  
        } __finally {  
            RtlReleaseSRWLockExclusive(&RtlCriticalSectionLock);  
        }  
  
        RtlZeroMemory(DebugInfo, sizeof (*DebugInfo));  
  
        if (FreeDebugInfo) {  
            RtlpFreeDebugInfo(DebugInfo);  
        }  
    }  
  
    RtlZeroMemory (CriticalSection,  
                   sizeof(RTL_CRITICAL_SECTION));  
  
    //      // Attempt to force future accesses to this memory to raise an access violation.      //    
    CriticalSection->DebugInfo = NULL;  
  
    return Status;  
}  
  
VOID  
RtlpUnWaitCriticalSection (  
    IN PRTL_CRITICAL_SECTION CriticalSection  
    )  
  
/*++    Routine Description:        This routine wakes a single waiter from a critical section.    Arguments:        CriticalSection - Supplies the critsec to wait.    Return Value:        None.    --*/    
{  
    NTSTATUS st;  
    HANDLE LockSemaphore;  
  
    LockSemaphore = CriticalSection->LockSemaphore;  
  
    if (LockSemaphore == NULL) {  
        LockSemaphore = RtlpCreateDeferredCriticalSectionEvent (CriticalSection);  
    }  
  
    if (!RtlpIsKeyedEvent (LockSemaphore)) {  
        st = NtSetEvent(LockSemaphore, NULL);  
  
    } else {  
        st = NtReleaseKeyedEvent(GlobalKeyedEventHandle,  
                                 CriticalSection,  
                                 FALSE,  
                                 0);  
    }  
  
    if (!NT_SUCCESS (st)) {  
        RtlRaiseStatus(st);  
    }  
}  
  
  
VOID  
RtlpNotOwnerCriticalSection (  
    IN PRTL_CRITICAL_SECTION CriticalSection  
    )  
  
/*++    Routine Description:        This routine is used to raise errors when a thread attempts an operation      on a critsec that is illegal because the thread does not own the critsec.    Arguments:        CriticalSection - Supplies the address of the critsec in question.    Return Value:        None.    --*/    
{  
    LOGICAL CsIsLoaderLock;  
    PRTLP_UNHANDLED_EXCEPTION_FILTER Filter;  
    PPEB_LDR_DATA Ldr;  
    ULONG Deadlock;  
  
    //      // Do not raise an error if the process is exiting anyway.      //    
    CsIsLoaderLock = (CriticalSection == &LdrpLoaderLock);  
    Ldr = NtCurrentPeb()->Ldr;  
  
    Deadlock = Ldr->ShutdownInProgress &&  
        (!CsIsLoaderLock || Ldr->ShutdownThreadId == NtCurrentTeb()->ClientId.UniqueThread);  
  
#if defined(_WIN64)  
    if (!Deadlock && UseWOW64) {  
        PPEB_LDR_DATA32 Ldr32 = (PPEB_LDR_DATA32)(UlongToPtr(NtCurrentPeb32()->Ldr));  
  
        Deadlock = Ldr32 && Ldr32->ShutdownInProgress;  
    }  
#endif  
  
    if (Deadlock) {  
        return;  
    }  
  
    if (NtCurrentPeb()->BeingDebugged) {  
        DbgPrintEx(DPFLTR_DEFAULT_ID,  
                   DPFLTR_ERROR_LEVEL,  
                   "NTDLL: Calling thread (%X) not owner of CritSect: %p  Owner ThreadId: %X\n",  
                   NtCurrentTeb()->ClientId.UniqueThread,  
                   CriticalSection,  
                   CriticalSection->OwningThread );  
        DbgBreakPoint();  
    }  
  
    Filter = RtlDecodePointer(RtlpUnhandledExceptionFilter);  
  
    __try {  
        RtlRaiseStatus (STATUS_RESOURCE_NOT_OWNED);  
    } __except (Filter ? Filter(GetExceptionInformation()) : EXCEPTION_CONTINUE_SEARCH) {  
        NOTHING;  
    }  
  
}  
  
  
/////////////////////////////////////////////////////////////////////  /////////////////////////////////////////// Critical section verifier  /////////////////////////////////////////////////////////////////////    
//  // This variable enables the critical section verifier (abandoned locks,  // terminatethread() while holding locks, etc.).  //    
BOOLEAN RtlpCriticalSectionVerifier = FALSE;  
  
//  // Settable from debugger to avoid a flurry of similar failures.  //    
BOOLEAN RtlpCsVerifyDoNotBreak = FALSE;  
  
  
VOID  
RtlCheckHeldCriticalSections (  
    __in     HANDLE hThread,  
    __in_opt PRTL_CRITICAL_SECTION const *LocksHeld  
    )  
/*++    Routine Description:        This routine is called to ensure that the given thread does not      hold any locks other than the ones in a specified list of      known-held locks.    Arguments:        hThread     -- the thread to check        LocksHeld   -- Locks known to be held by the thread    Return Value:        None.    --*/    
{  
    NTSTATUS Status;  
    THREAD_BASIC_INFORMATION ThreadInfo;  
    PLIST_ENTRY Entry;  
    PRTL_CRITICAL_SECTION_DEBUG DebugInfo;  
    RTL_CRITICAL_SECTION_DEBUG ExtraDebugInfoCopy = {0}; // initialized because of W4 warning      PRTL_CRITICAL_SECTION CriticalSection;  
    RTL_CRITICAL_SECTION CritSectCopy = {0}; // initialized because of W4 warning      PRTL_CRITICAL_SECTION const *LockHeld;  
    BOOLEAN Copied;  
    ULONG CountOfOwnedCriticalSections = 0;  
    HANDLE ThreadId;  
    THREAD_TEB_INFORMATION TebInfo;  
  
    //      // We do not check anything if critical section verifier is not on.      //    
    if ((RtlpCriticalSectionVerifier == FALSE) ||  
        (RtlpCsVerifyDoNotBreak != FALSE)) {  
  
        goto Done;  
    }  
  
    //      // We do not do anything if we are shutting down the process.      //    
    if (LdrpShutdownInProgress) {  
        goto Done;  
    }  
  
    //      // This function is typically called with NtCurrentThread() as the      // first parameter. Reference the current thread's TEB directly      // in that case.      //    
    if (hThread == NtCurrentThread()) {  
        if (NtCurrentTeb()->CountOfOwnedCriticalSections == 0) {  
            //              // The app verifier keeps this TEB counter updated on all              // platforms instead of just x86 chk without the verifier,              // at least for critical sections that were not entered from ntdll.              //    
            goto Done;  
        }  
  
        ThreadId = NtCurrentTeb()->ClientId.UniqueThread;  
    }  
    else {  
        //          // Query the value of TEB's CountOfOwnedCriticalSections field.          //    
        TebInfo.TebInformation = &CountOfOwnedCriticalSections;  
        TebInfo.BytesToRead = sizeof (CountOfOwnedCriticalSections);  
  
#if defined (BUILD_WOW6432)  
        TebInfo.TebOffset = FIELD_OFFSET (TEB32, CountOfOwnedCriticalSections);  
#else  
        TebInfo.TebOffset = FIELD_OFFSET (TEB, CountOfOwnedCriticalSections);  
#endif  
  
        Status = NtQueryInformationThread (hThread,  
                                           ThreadTebInformation,  
                                           &TebInfo,  
                                           sizeof(TebInfo),  
                                           NULL);  
  
        if (!NT_SUCCESS (Status)) {  
            //              // Bail out if the thread died already or if we are out of memory.              //    
            goto Done;  
        }  
  
        if (CountOfOwnedCriticalSections == 0) {  
            //              // The app verifier keeps this TEB counter updated on all              // platforms instead of just x86 chk without the verifier,              // at least for critical sections that were not entered from ntdll.              //    
            goto Done;  
        }  
  
        //          // Query the thread ID.          //    
        Status = NtQueryInformationThread (hThread,  
                                           ThreadBasicInformation,  
                                           &ThreadInfo,  
                                           sizeof(ThreadInfo),  
                                           NULL);  
  
        if (! NT_SUCCESS(Status)) {  
            //              // Bail out if the thread died already or if we are out of memory.              //    
            goto Done;  
        }  
  
        ThreadId = ThreadInfo.ClientId.UniqueThread;  
        ASSERT (ThreadInfo.ClientId.UniqueProcess == NtCurrentTeb()->ClientId.UniqueProcess);  
    }  
  
    //      // Iterate the global list of critical sections      //    
    RtlAcquireSRWLockShared(&RtlCriticalSectionLock);  
    __try {  
        for (Entry = RtlCriticalSectionList.Flink;  
             Entry != &RtlCriticalSectionList;  
             Entry = Entry->Flink) {  
  
            DebugInfo = CONTAINING_RECORD(Entry,  
                                          RTL_CRITICAL_SECTION_DEBUG,  
                                          ProcessLocksList);  
  
            CriticalSection = DebugInfo->CriticalSection;  
            if (LocksHeld) {  
  
                //                  // We have a list of okay-to-hold critical sections;                  // scan through it, looking for this critical section.                  // If we find it, we'll skip it and continue walking                  // ProcessLocksList.                  //    
                for (LockHeld = LocksHeld;  
                     *LockHeld;  
                     LockHeld++) {  
  
                    if (CriticalSection == *LockHeld) {  
  
                        //                          // We've found this critical section in the                          // LocksHeld array.                          //    
                        break;  
                    }  
                }  
  
                if (*LockHeld) {  
  
                    //                      // Our caller expected the thread to be holding                      // this critical section; skip it, and continue                      // walking through ProcessLocksList.                      //    
                    continue;  
                }  
            }  
  
            Copied = TRUE;  
            __try {  
                CritSectCopy = *CriticalSection;  
  
            } __except (EXCEPTION_EXECUTE_HANDLER) {  
                Copied = FALSE;  
            }  
  
            if (Copied == FALSE) {  
  
                //                  // Exception while reading the contents of the critsect.                  // The critsect has probably been decommitted without a call to                  // RtlDeleteCriticalSection.                  //                  // You might think the entry could be deleted from the list,                  // but it can't... there may be another RTL_CRITICAL_SECTION out                  // there that is truly allocated, and whose DebugInfo pointer                  // points at this DebugInfo.  In that case, when that critsect                  // is deleted, the RtlCriticalSectionList is corrupted.                  //    
                VERIFIER_STOP (APPLICATION_VERIFIER_LOCK_IN_FREED_MEMORY,  
                                "undeleted critical section in freed memory",  
                                CriticalSection, "Critical section address",  
                                DebugInfo, "Critical section debug info address",  
                                RtlpGetStackTraceAddressEx (DebugInfo->CreatorBackTraceIndex,  
                                                            DebugInfo->CreatorBackTraceIndexHigh),  
                                "Initialization stack trace. Use dps to dump it if non-NULL.",  
                                NULL, "" );  
            }  
            else if(CritSectCopy.DebugInfo != DebugInfo) {  
  
                //                  // Successfully read the critical section structure but                  // the current debug info field of this critical section                  // doesn't point to the current DebugInfo - it was probably                  // initialized more than one time or simply corrupted.                  //                  // Try to make a copy of the DebugInfo currently pointed                  // by our critical section. This might fail if the critical                  // section is corrupted.                  //    
                Copied = TRUE;  
                __try {  
                    ExtraDebugInfoCopy = *(CritSectCopy.DebugInfo);  
  
                } __except (EXCEPTION_EXECUTE_HANDLER) {  
                    Copied = FALSE;  
                }  
  
                if (Copied == FALSE) {  
  
                    //                      // Exception while reading the contents of the debug info.                      // The current critical section structure is corrupted.                      //    
                    VERIFIER_STOP (APPLICATION_VERIFIER_LOCK_CORRUPTED,  
                                   "corrupted critical section",  
                                   CriticalSection,  
                                   "Critical section address",  
                                   CritSectCopy.DebugInfo,  
                                   "Invalid debug info address of this critical section",  
                                   DebugInfo,  
                                   "Address of the debug info found in the active list.",  
                                   RtlpGetStackTraceAddressEx (DebugInfo->CreatorBackTraceIndex,  
                                                               DebugInfo->CreatorBackTraceIndexHigh),  
                                   "Initialization stack trace. Use dps to dump it if non-NULL." );  
                }  
                else {  
  
                    //                      // Successfully read this second debug info                      // of the same critical section.                      //    
                    VERIFIER_STOP (APPLICATION_VERIFIER_LOCK_DOUBLE_INITIALIZE,  
                                   "double initialized or corrupted critical section",  
                                   CriticalSection,  
                                   "Critical section address.",  
                                   DebugInfo,  
                                   "Address of the debug info found in the active list.",  
                                   RtlpGetStackTraceAddressEx (DebugInfo->CreatorBackTraceIndex,  
                                                               DebugInfo->CreatorBackTraceIndexHigh),  
                                   "First initialization stack trace. Use dps to dump it if non-NULL.",  
                                   RtlpGetStackTraceAddressEx (ExtraDebugInfoCopy.CreatorBackTraceIndex,  
                                                               ExtraDebugInfoCopy.CreatorBackTraceIndexHigh),  
                                   "Second initialization stack trace. Use dps to dump it if non-NULL.");  
                }  
            }  
            else if (CritSectCopy.OwningThread == ThreadId) {  
  
                //                  // The thread has a critical section locked. Since this API is called                  // whenever the thread is in a context that does not allow this                  // we will have to report the issue (e.g. thread exits or is                  // terminated, a thread pool work item finished, etc.).                  //    
                VERIFIER_STOP (APPLICATION_VERIFIER_EXIT_THREAD_OWNS_LOCK,  
                               "Thread is in a state in which it cannot own a critical section",  
                               ThreadId, "Thread identifier",  
                               CriticalSection, "Critical section address",  
                               DebugInfo, "Critical section debug info address",  
                               RtlpGetStackTraceAddressEx (DebugInfo->CreatorBackTraceIndex,  
                                                           DebugInfo->CreatorBackTraceIndexHigh),  
                               "Initialization stack trace. Use dps to dump it if non-NULL." );  
            }  
        }  
  
    } __finally {  
  
        //          // Release the CS list lock.          //    
        RtlReleaseSRWLockShared(&RtlCriticalSectionLock);  
    }  
  
Done:  
  
    NOTHING;  
}  
  
  
VOID  
RtlCheckForOrphanedCriticalSections (  
    __in HANDLE hThread  
    )  
/*++    Routine Description:        This routine is called from kernel32's ExitThread, TerminateThread      and SuspendThread in an effort to track calls that kill threads while      they own critical sections.    Arguments:        hThread     -- thread to be killed    Return Value:        None.    --*/  {  
    // The work is performed by RtlCheckHeldCriticalSections, which we      // call with the following okay-to-be-held critical section.    
    //      // Staring from build 5206, we try to check loaderlock too.      //    
    PRTL_CRITICAL_SECTION const LocksHeld[] = {  
//        &LdrpLoaderLock,          NULL  
    };  
  
    RtlCheckHeldCriticalSections(hThread, LocksHeld);  
}  
  
HANDLE  
RtlQueryCriticalSectionOwner (  
    __in HANDLE EventHandle  
    )  
  
/*++    Routine Description:        This routine finds the owner for a critical section given an event      handle (i.e. a LockSemaphore).    Arguments:         EventHandle -- LockSemaphore possibly backing up a critcial          section    Return Value:        ID of the owning thread or NULL    --*/    
{  
    PRTL_CRITICAL_SECTION CriticalSection;  
    PRTL_CRITICAL_SECTION_DEBUG DebugInfo;  
    PLIST_ENTRY Head, Next, Follower;  
    BOOLEAN UpdateFollower = FALSE;  
    HANDLE OwnerThread = NULL;  
  
    if (EventHandle == NULL) {  
        return NULL;  
    }  
  
    if (RtlpIsKeyedEvent(EventHandle)) {  
        return NULL;  
    }  
  
    Head = &RtlCriticalSectionList;  
    if (!RtlTryAcquireSRWLockShared(&RtlCriticalSectionLock)) {  
        return NULL;  
    }  
  
    __try {  
        Follower = Next = Head->Flink;  
        while (Next != Head) {  
            DebugInfo = CONTAINING_RECORD(Next,  
                                          RTL_CRITICAL_SECTION_DEBUG,  
                                          ProcessLocksList);  
  
            if (DebugInfo->Type == RTL_CRITSECT_TYPE) {  
                CriticalSection = DebugInfo->CriticalSection;  
                __try {  
  
                    //                      // N.B. Because the memory backing a critical section                      // object can be deleted without the critical section                      // debug information structure being freed, it is not                      // safe to access the critical section object without                      // the use of __try/__except.                      //                      // N.B. This is behavior is especially common in the                      //      case of static critical section objects.                      //    
                    if (CriticalSection->LockSemaphore == EventHandle) {  
                        OwnerThread = CriticalSection->OwningThread;  
                        break;  
                    }  
  
                } __except (EXCEPTION_EXECUTE_HANDLER) {  
  
                    //                      // N.B. If a debugger is attached, then break into the                      // debugger. This is done in order to catch corruption                      // under stress.                      //    
                    if (RtlIsAnyDebuggerPresent() != FALSE) {  
                        DbgBreakPoint();  
                    }  
                }  
            }  
  
            Next = Next->Flink;  
            if (Next == Follower) {  
                break;  
            }  
  
            if (UpdateFollower) {  
                Follower = Follower->Flink;  
            }  
  
            UpdateFollower = (UpdateFollower == FALSE) ? TRUE : FALSE;  
        }  
  
    } __finally {  
        RtlReleaseSRWLockShared(&RtlCriticalSectionLock);  
    }  
  
    return OwnerThread;  
}  
  
ULONG  
RtlGetCriticalSectionContentionCount (  
    __in PRTL_CRITICAL_SECTION CriticalSection  
    )  
  
/*++    Routine Description:        This routine returns the contention count of a critical section,      if debug information is available.    Arguments:        CriticalSection - Supplies the critical section structure to be queried    Return Value:        The values of the ContentionCount if available, or 0 if the DebugInfo      has not been initialized.    --*/    
{  
  
    if (CriticalSection->DebugInfo != CS_INVALID_DEBUG_INFO) {  
  
        return CriticalSection->DebugInfo->ContentionCount;  
    }  
  
    return 0;  
}  
  
