#if 0
// If you are reading this, then you're probably trying to fix a bug in preprocessing
// the multiple-personality code marker values file.
// This is for CodeMarkerSink's look-up-table which turns the enum into a C++ struct
// that can be used to look-up a name from a code marker value.
// Contact pharring for details on how this works
#endif
#define REM /##/
#define STRINGIZE(x,y) x##y
#define HASH_CHAR #
#define HASH(x) STRINGIZE(HASH_CHAR,)x
#define SECTION /##/ ---
#define NAMESPACE(x)
#define END_NAMESPACE(x)
#define ENUM(x)
#define ENUMVALUE(x,y) { x, _T(#y) },
#define ENUMVALUEEX(x,y,z) { x, _T(#y) },
#define END_ENUM(x)
#define CONDITIONAL(x) HASH(ifdef) x
#define END_CONDITIONAL(x) HASH(endif) /##/##x
#define CONDITIONAL2(x,y) HASH(if) (defined x || defined y)
#define END_CONDITIONAL2(x,y) HASH(endif) /##/##x || y
#define CPPONLY HASH(if) 0 /##/ C++ only
#define END_CPPONLY HASH(endif) /##/ C++ only
#define INTERNAL
#define DOCCOMMENT(x)