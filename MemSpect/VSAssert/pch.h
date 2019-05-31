//=--------------------------------------------------------------------------=
// Pch.H
//=--------------------------------------------------------------------------=
// Precompiled header for VsAssert.Dll
//=--------------------------------------------------------------------------=
// Copyright (c) 1997, Microsoft Corporation
//			All Rights Reserved
// Information Contained Herein Is Proprietary and Confidential.
//=--------------------------------------------------------------------------=

#define STRICT
#define DEBUG 1
#define _CRT_STDIO_LEGACY_WIDE_SPECIFIERS
#include <windows.h>
#include <malloc.h>
#include <stdio.h>

#include "vsassertatl.h"

#include "metahost.h"
#include <CorHdr.h>
#include <cor.h>
#include <corprof.h>
#include "StackWalk.h"

// We just use our own defaults
#define VSASSERT_SET_DEFAULTS 1

#if !defined(NUMBER_OF)
#define NUMBER_OF(x) (sizeof(x) / sizeof((x)[0]))
#endif // !defined(NUMBER_OF)
