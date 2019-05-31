
#include <stdlib.h>
#include "debugalloc.h"

#define malloc(size)          DebugAlloc((size))
#define realloc(pv, size)     DebugRealloc((pv), (size))
#define free(pv)              DebugFree((pv))

#include <atlcoll.h>

#undef malloc
#undef realloc
#undef free