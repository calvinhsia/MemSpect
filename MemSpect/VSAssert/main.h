//=--------------------------------------------------------------------------=
// Main.H
//=--------------------------------------------------------------------------=
// DLL Main code for VS assertions.  Also holds global debugging stuff
//=--------------------------------------------------------------------------=
// Copyright (c) 1997, Microsoft Corporation
//			All Rights Reserved
// Information Contained Herein Is Proprietary and Confidential.
//=--------------------------------------------------------------------------=

#ifndef _INC_MAIN_H
#define _INC_MAIN_H

//=--------------------------------------------------------------------------=
// Handy macros
//
#define DLLEXPORT _declspec(dllexport)
#define scope
#define ARRAY_SIZE(x) (sizeof(x) / sizeof(*x))

#define IfFailGoto(x, label)          if(FAILED(hr = (x))) goto label
#define IfFailGo(x)                   IfFailGoto(x, Error)
#define IfFailRet(x)                  if(FAILED(hr = (x))) return hr
#define IfFalseGoto(x, hres, label)   if(!(x)) { hr = hres; goto label;}
#define IfFalseGo(x, hres)            IfFalseGoto(x, hres, Error)
#define IfFalseRet(x, hres)           if(!(x)) return hres

#define UNUSED(x) (x==x)

//=--------------------------------------------------------------------------=
// Global thingies the DLL would like to have around
//
extern int	  g_dfAssertFlags;      // how to output asserts
extern int	  g_dfPrintfFlags;      // how to output DebugPrintf
extern int	  g_dfEnabled;		// bitmask specifying which flags are valid
extern DWORD	  g_dwDebugThreadId;    // ID of the debugging thread, if it's running
extern HINSTANCE  g_hinstDll;		// The DLL instance handle
extern BOOL       g_fShuttingDown;      // TRUE if we're shutting down
extern DWORD           g_dwMainThread;

//=--------------------------------------------------------------------------=
// Debug memory allocation
//
#include "debugalloc.h"


#pragma warning(4:4702) // unreachable code

#endif // _INC_MAIN_H
