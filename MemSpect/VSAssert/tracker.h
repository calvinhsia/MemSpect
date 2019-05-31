/*
  Tracker Macros
  ==============

    These macros can be used to record AddRef and Release on COM objects so 
    reference counting errors can be pinpointed.

    How these macros work:
    =====================

    Whenever a reference is added by calling AddRef on a COM object, there is 
    usually a variable somewhere that holds a pointer to the object.  This
    variable "owns" the reference.  When this variable is cleared (or goes out 
    of scope) the reference should be released.  If there is a reference leak, it 
    means that one of these variables didn't make its call to Release.

    What these macros do in the debug build is that whenever anybody calls
    AddRef, and entry is added to a table.  The entry stores the COM IUnkown
    pointer, the address of the variable that holds this pointer, and a pass count.
    Whenever anybody calls Release, we search the table for an entry where the COM
    IUnknown pointer matches and the address of the variable matches, and remove it.
    (If we don't find it we assert.  This means somebody is over-releasing.)

    Then, at shutdown, we make sure the table is empty.  If it is not empty, then
    we assert.  With the PassCount number, you can restart and break at the 
    offending AddRef.  I.e., you can break at exactly the point where somebody
    AddRefs an object but never calls Release.


    How to use:
    ===========

    Here is some sample code which uses these macros.

    IFoo *g_pFoo;

    void PlayWithObjects()
    {
      IFoo *pFoo = NULL;
      IFoo *pFoo2 = NULL;
      
      GetFooFromSomewhere(&pFoo);
      TRACKER_RECEIVE(pFoo);

      pFoo2 = pFoo;
      TRACKER_ADDREF(pFoo2);

      // assign it to our global variable.  This global
      // is going to hold it for a long time, so we don't
      // release it at the end of this function.  Somebody somewhere
      // else is going to release it later.
      g_pFoo = pFoo;
      TRACKER_ADDREF(g_pFoo);

      // use pFoo and pFoo2 for a while

      TRACKER_RELEASE(pFoo);
      TRACKER_RELEASE(pFoo2);
    }
    
    Whenever you are given an object (because you called some function that
    returns it to you) and you now hold a reference to it, call TRACKER_RECEIVE.
    (This does nothing in the non-debug build.)  This will make sure you 
    remember to call release later.

    Whenever you need to addref (because you copy the pointer into another
    variable, for example) use TRACKER_ADDREF.

    Whenever you need to release (when your variables are going out of scope),
    call TRACKER_RELEASE.

    Other notes: if you're giving out an object to your caller, you will need
    to use TRACKER_GIVE_AWAY.  If you're transferring ownership from one variable
    to another, use TRACKER_ASSIGN.

*/

#ifndef TRACKER_H_INCLUDED
#define TRACKER_H_INCLUDED
/*


#include "ole2.h"


#ifdef DEBUG

CLINKAGE void ENTRYPOINT  TrackerTransfer(IUnknown *punk, void *pvFrom, void *pvTo);
CLINKAGE void ENTRYPOINT  TrackerAssertEverythingReleased();

#endif

// Same as TRACKER_RELEASE, except first checks for NULL
#define TRACKER_CHECK_RELEASE(punk) \
  if (punk) {                       \
    TRACKER_RELEASE(punk);          \
  }                                 

// Same as TRACKER_RELEASE, except first checks for NULL 
#define TRACKER_CHECK_RELEASEZ(punk)   \
  {                                    \
    if (punk) {                        \
      TRACKER_RELEASE(punk);           \
    }                                  \
    punk = NULL;                       \
  }

#if DEBUG

template <class PI> inline void ENTRYPOINT TRACKER_ADDREF(PI &punk)
  {
  punk->AddRef();      
  TrackerTransfer(punk, NULL, &(punk)); 
  }
template <class PI> inline void ENTRYPOINT TRACKER_RELEASE(PI &punk)
  {
  punk->Release();      
  TrackerTransfer((punk), &(punk), NULL);
  punk = NULL;
  }


template <class PI> inline void ENTRYPOINT TRACKER_RECEIVE(PI &punk) 
  {
  TrackerTransfer((punk), NULL, &(punk)); 
  }

template <class PI> inline void ENTRYPOINT TRACKER_GIVE_AWAY(PI &punk) 
  {
  TrackerTransfer((punk), &(punk), NULL); 
  }

template <class PI> inline void ENTRYPOINT TRACKER_ASSIGN(PI &punkDest, PI &punkSrc) 
  {
  (punkDest) = (punkSrc); 
  TrackerTransfer((punkSrc), &(punkSrc), &(punkDest)); 
  }

#define TRACKER_ASSERT_EVERYTHING_RELEASED() \
    TrackerAssertEverythingReleased()

#else                                                                            

#define TRACKER_ADDREF(punk)       (punk)->AddRef()

#define TRACKER_RELEASE(punk)      (punk)->Release()

#define TRACKER_RECEIVE(punk)       

#define TRACKER_GIVE_AWAY(punk) 

#define TRACKER_ASSIGN(punkDest, punkSrc)   { (punkDest) = (punkSrc); }

#define TRACKER_ASSERT_EVERYTHING_RELEASED()

#endif

*/

#endif
