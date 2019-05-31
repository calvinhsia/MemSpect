
#include <windows.h>
#include "dbghlper.h"
#include "vcexcept.h"

EXTERN_C BOOL WINAPI SetThreadName( DWORD dwThread, LPCSTR szThreadName, DWORD dwFlags )
{
	EXCEPTION_VISUALCPP_DEBUG_INFO info;

	info.dwType = EXCEPTION_DEBUGGER_NAME_THREAD;
	info.SetName.szName = szThreadName;
	info.SetName.dwThreadID = dwThread;
	info.SetName.dwFlags = dwFlags;

	__try
	{
		RaiseException( EXCEPTION_VISUALCPP_DEBUGGER, 0, sizeof(info)/sizeof(INT_PTR), (INT_PTR *)&info );
	}
	__except(EXCEPTION_CONTINUE_EXECUTION)
	{
	}

	return TRUE;
}
