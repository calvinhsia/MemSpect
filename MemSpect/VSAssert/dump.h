//=--------------------------------------------------------------------------=
// Dump.H
//=--------------------------------------------------------------------------=
// Stack dumping functions
//=--------------------------------------------------------------------------=
// Copyright (c) 1997, Microsoft Corporation
//			All Rights Reserved
// Information Contained Herein Is Proprietary and Confidential.
//=--------------------------------------------------------------------------=

#ifndef _INC_DUMP_H
#define _INC_DUMP_H

// writes current thread stack to the stream
HRESULT WINAPI DebDumpStack(IStream *pStream, UINT ifrStart = 0, BOOL fWriteEOS = TRUE);

HRESULT WINAPI DebDumpStackResolveSymbols
(
 IStream   * pStream,
 DWORD_PTR * pdwAddr,
 UINT        uicAddr,
 BOOL        fWriteEOS
);


#endif // _INC_DUMP_H