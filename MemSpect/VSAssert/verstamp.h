#ifndef _VERSION_H_INCL
#include "version.h"                   /* SLM maintained version file */
#endif

#ifndef _WIN32_WCE
#if defined(_WIN32) || defined(WIN32)
#include <winver.h>
#else   /* !WIN32 */
#include <ver.h>
#endif  /* !WIN32 */
#else	/* !_WIN32_WCE */
#include "winbase.h"
#endif


#ifndef rpt
#define rpt 0
#endif

#undef rmmpad
#define rmmpad

#if     (rup < 10)
#define ruppad "000"
#elif   (rup < 100)
#define ruppad "00"
#elif   (rup < 1000)
#define ruppad "0"
#else
#define ruppad
#endif

#define rptpad

#define VERSION_STR1(a,b,c,d)       #a "." rmmpad #b "." ruppad #c "." rptpad #d

#define VERSION_STR2(a,b,c,d)       VERSION_STR1(a,b,c,d)
#define VER_PRODUCTVERSION_STR      VERSION_STR2(rmj,rmm,rup,rpt)
#define VER_PRODUCTVERSION          rmj,rmm,rup,rpt

/*--------------------------------------------------------------*/
/* the following section defines values used in the version     */
/* data structure for all files, and which do not change.       */
/*--------------------------------------------------------------*/

// DEBUG flag is set for debug build, not set for retail build
#if defined(DEBUG) || defined(_DEBUG)
#define VER_DEBUG                   VS_FF_DEBUG
#else
#define VER_DEBUG                   0
#endif

#define VER_PRIVATEBUILD            0
#define VER_PRERELEASE              0

#define VER_FILEFLAGSMASK           VS_FFI_FILEFLAGSMASK
#if defined(_WIN32) || defined(WIN32)
#define VER_FILEOS                  VOS__WINDOWS32
#else
#define VER_FILEOS                  VOS_DOS_WINDOWS16
#endif
#define VER_FILEFLAGS               (VER_PRIVATEBUILD|VER_PRERELEASE|VER_DEBUG)

#define VER_COMPANYNAME_STR         "Microsoft Corporation"
