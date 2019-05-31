#pragma once

VOID DetAttach(PVOID *ppbReal, PVOID pbMine, PCHAR psz);
VOID DetDetach(PVOID *ppbReal, PVOID pbMine, PCHAR psz);

#define ATTACH(x,y)   DetAttach((PVOID*) x, (PVOID) y,#x)
#define DETACH(x,y)   DetDetach(x,y,#x)

#define CHECKNULL(p) \
    if (!p) \
    { \
        DetourTransactionAbort(); \
        return E_FAIL; \
    }

#define DECLATTACH(h, p)  CHECKNULL((Real_##p = reinterpret_cast<decltype(Real_##p)>(GetProcAddress(h, #p)))) \
    ATTACH(&Real_##p, Mine_##p);
