

#include "Pch.H"
#include "vsassert.h"

#if DEBUG


struct REFASSOC {
    IUnknown *m_punk;
    void *m_pvOwner;
    unsigned m_PassCount;
};

#define REFASSOC_MAX_INITIAL    1000

REFASSOC g_rgRefassocInitialBuffer[REFASSOC_MAX_INITIAL];
unsigned g_cRefassoc;
unsigned g_cRefassocAllocated = REFASSOC_MAX_INITIAL;
REFASSOC *g_rgRefassoc = g_rgRefassocInitialBuffer;

BOOL g_fTracking = TRUE;

unsigned g_PassCount = 0;


// Transfer ownership of punk from pvFrom to pvTo.
//
CLINKAGE VOID ENTRYPOINT TrackerTransfer(IUnknown *punk, void *pvFrom, void *pvTo)
{     
    unsigned iRefassoc;

    g_PassCount++;    
    if (!g_fTracking) // Put breakpoint here.
    {
        return;
    }          

    if (pvFrom) 
    {
        // Start from the most recently added

        iRefassoc = g_cRefassoc; 

        // If we assert here, it means somebody tried to release
        // something that isn't in our table.        
        ASSERT(iRefassoc > 0, "");

        while (iRefassoc > 0) 
        {
            iRefassoc--;

            if (g_rgRefassoc[iRefassoc].m_punk == punk && 
                g_rgRefassoc[iRefassoc].m_pvOwner == pvFrom) 
            {
                break;
            }

            // If we assert here, it means somebody tried to release
            // something that isn't in our table.        
            ASSERT(iRefassoc > 0, "");
        }
    }
    else 
    {
        iRefassoc = g_cRefassoc;       

        if (iRefassoc >= g_cRefassocAllocated) {
            REFASSOC *rgRefassocNew;
            ASSERT(iRefassoc == g_cRefassocAllocated, "");

            // we need a new buffer.

            rgRefassocNew = (REFASSOC *) VsDebAlloc(0, g_cRefassocAllocated * 2 * sizeof(REFASSOC));
            if (!rgRefassocNew) 
            {
                ASSERT(FALSE, "can't get memory to do tracking");
                g_fTracking = FALSE;
                return;
            }

            memcpy(rgRefassocNew, 
                g_rgRefassoc, g_cRefassocAllocated * sizeof(REFASSOC));

            if (g_rgRefassoc != g_rgRefassocInitialBuffer) {
                VsDebFree(g_rgRefassoc);
            }

            g_rgRefassoc = rgRefassocNew;
            g_cRefassocAllocated *= 2;
        }

        g_rgRefassoc[iRefassoc].m_punk = punk;
        g_rgRefassoc[iRefassoc].m_pvOwner = NULL;

        g_cRefassoc++;
    }

    ASSERT(g_rgRefassoc[iRefassoc].m_punk == punk, "");
    ASSERT(g_rgRefassoc[iRefassoc].m_pvOwner == pvFrom, "");
    ASSERT(iRefassoc < g_cRefassoc, "");

    if (pvTo) 
    {
        g_rgRefassoc[iRefassoc].m_pvOwner = pvTo;
        g_rgRefassoc[iRefassoc].m_PassCount = g_PassCount;
    }        
    else {
        memmove(g_rgRefassoc + iRefassoc, g_rgRefassoc + iRefassoc + 1,
            sizeof(g_rgRefassoc[0]) * (g_cRefassoc - iRefassoc - 1) );

        g_cRefassoc--;        
    }      

}

// Call this at the end to make sure everything was released.
CLINKAGE VOID ENTRYPOINT TrackerAssertEverythingReleased()
{     
    UINT PassCount;

    ASSERT(g_fTracking, "");

    if (g_cRefassoc > 0) {
        PassCount = g_rgRefassoc[g_cRefassoc - 1].m_PassCount;
        ASSERT(FALSE, "TRACKER: something wasn't released.  Enter debugger and look at instructions in tracker.cpp to find the leak.");

        // To find the problem, do the following:
        // 
        //   1. Write down the value of PassCount.
        //   2. Place breakpoint where instructed at the top of TrackerTransfer.
        //   3. Restart.
        //   4. When you break on TrackerTransfer, set g_PassCount to the 
        //          value you wrote down.  Continue.
        //   5. When you break on the DebugBreak() line, look at the callstack 
        //      and you'll see who did the allocation.

    }

    if (g_rgRefassoc != g_rgRefassocInitialBuffer) 
    {
        VsDebFree(g_rgRefassoc);
    }
}

#endif

