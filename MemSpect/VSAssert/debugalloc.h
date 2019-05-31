// debug alloc functions
#pragma once 

PVOID DebugAlloc(SIZE_T cb);
PVOID DebugRealloc(PVOID pv, SIZE_T cb);
VOID  DebugFree(PVOID pv);
INT_PTR DebugSize(PVOID pv);
