Imports System.Runtime.InteropServices

Namespace MemSpect

    Public Module StackMapFile
        Friend _hFileMapping As IntPtr
        Friend _ulMemSize As UInteger
        Friend Structure MappingData
            Friend _MapBase As IntPtr
            Friend _MapOffset As UInt32
            Friend _MapViewSize As Integer
        End Structure
        Friend _CurrentMapping As MappingData
        Friend _didInitialize As Boolean
        Public Sub Cleanup()
            If _CurrentMapping._MapBase <> IntPtr.Zero Then
                Dim res = UnmapViewOfFile(_CurrentMapping._MapBase)
                Debug.Assert(res, "couldn't unmap view")
                _CurrentMapping._MapBase = IntPtr.Zero
            End If
            If _hFileMapping <> IntPtr.Zero Then
                NativeImports.CloseHandle(_hFileMapping)
                _hFileMapping = IntPtr.Zero
            End If
        End Sub
        Public Sub InitStackMapping()
            If Not _didInitialize Then
                _didInitialize = True
                AddHandler ProcComm.TargetUnfrozen, Sub()
                                                        ' when we unfreeze, compaction might occur, causing rename of the page file
                                                        ' so we need to unmap/free
                                                        Cleanup()
                                                    End Sub
            End If
            If _hFileMapping = IntPtr.Zero Then
                Dim stats = StackMemMapStats.GetMemMapStats()
                _ulMemSize = stats.ulMemSize
                Dim mapName = String.Format("{0}MemSpect{1}_{2}", _strGlobal, _TargetProcessId, stats.nFileMaps)
                _hFileMapping = NativeImports.OpenFileMapping(FILE_MAP_READ, bInheritHandle:=0, lpName:=mapName)
                If _hFileMapping = IntPtr.Zero Then
                    'hMap = New IntPtr(Marshal.GetLastWin32Error())
                    Dim lErr = Marshal.GetLastWin32Error()
                    UpdateStatusMsg("error OpenFileMapping " + GetErrorMessageFromWin32LastError(lErr))
                End If
            End If
        End Sub

        Public Function MapStackView(ulOffset As UInt32, numBytesToMap As Integer) As IntPtr
            InitStackMapping()
            Dim mappedAddress = IntPtr.Zero
            Dim newBaseOffset = CUInt((ulOffset \ AllocationGranularity) * AllocationGranularity)
            Dim leftover = ulOffset - newBaseOffset
            Dim preferredAddress = IntPtr.Zero
            Dim desiredSize = AllocationGranularity * 2
            If (desiredSize > _ulMemSize) Then
                desiredSize = CInt(_ulMemSize)
            End If
            If _CurrentMapping._MapBase <> IntPtr.Zero Then
                Dim fFits = ulOffset >= _CurrentMapping._MapOffset AndAlso
                    ulOffset + numBytesToMap < _CurrentMapping._MapOffset + _CurrentMapping._MapViewSize
                If fFits AndAlso leftover + numBytesToMap < _CurrentMapping._MapViewSize Then
                    mappedAddress = _CurrentMapping._MapBase + CInt(newBaseOffset - _CurrentMapping._MapOffset + leftover)
                Else
                    preferredAddress = _CurrentMapping._MapBase
                    Dim res = UnmapViewOfFile(_CurrentMapping._MapBase)
                    Debug.Assert(res, "Unmapview didn't work")
                    If (leftover + desiredSize > AllocationGranularity) Then
                        desiredSize += AllocationGranularity
                    End If
                    If newBaseOffset + desiredSize >= _ulMemSize Then
                        desiredSize = CInt(_ulMemSize - newBaseOffset)
                    End If
                End If
            End If
            If mappedAddress = IntPtr.Zero Then
                '/// try at preferred address
                mappedAddress = MapViewOfFileEx(
                    _hFileMapping,
                    FILE_MAP_READ,
                    0,
                    newBaseOffset,
                    CUInt(desiredSize),
                    preferredAddress
                    )
                If (mappedAddress = IntPtr.Zero) Then

                    If (preferredAddress <> IntPtr.Zero) Then

                        '// try again at any address
                        mappedAddress = MapViewOfFileEx(
                            _hFileMapping,
                            FILE_MAP_READ,
                            0,
                            newBaseOffset,
                           CUInt(desiredSize),
                            IntPtr.Zero
                            )
                    End If
                End If
                If (mappedAddress = IntPtr.Zero) Then
                    Dim err = Marshal.GetLastWin32Error()
                    Dim h = Marshal.GetHRForLastWin32Error()
                    ' if this throws, check sizes of mappings: can't exceed mapped obj size
                    Throw New InvalidOperationException(String.Format("Can't MapView {0:n0} {1} hr = {2:x8} MemSz={3} newBase={4} Desird={5}",
                                                                      ulOffset,
                                                                      numBytesToMap,
                                                                      h,
                                                                      _ulMemSize,
                                                                      newBaseOffset,
                                                                      desiredSize))
                End If

                _CurrentMapping._MapBase = mappedAddress
                _CurrentMapping._MapViewSize = desiredSize
                _CurrentMapping._MapOffset = newBaseOffset
                mappedAddress = mappedAddress + CInt(leftover)
            End If
            Return mappedAddress
        End Function

        Friend Function GetStackMapOffset(hctr As HeapAllocationContainer) As UInteger
            Dim nOffsetOfMapOffset = _OffsetCallStackFrames ' 1st entry is offset in to mapped file
            Dim tmp As Integer
            If ReadProcessMemoryDword(_hProcessTarget, hctr.HeapBlockPtr.MyAdd(nOffsetOfMapOffset), tmp, IntPtr.Size, Nothing) = 0 Then
                UpdateStatusMsg("getcallstackaddr readprocmem failed", fAssert:=True)
            End If
            Return CUInt(tmp)
        End Function

        Public Function GetCallStackAddressFromMap(hctr As HeapAllocationContainer, nIndex As Integer) As IntPtr
            Dim res = IntPtr.Zero
            If nIndex >= 0 AndAlso nIndex < hctr.AllocationStruct.m_uicStackAddr Then
                Dim off = GetStackMapOffset(hctr) + CUInt(nIndex * 4)
                Dim addr = MapStackView(off, 4)
#If DEBUG Then
                addr += 4
#End If
                res = Marshal.ReadIntPtr(addr)
            End If
            Return res
        End Function

        Public Function GetCallStackAddressToArrayFromMap(hctr As HeapAllocationContainer, Optional ByVal nMaxStackFrames As Integer = 0) As IntPtr()
            Dim res() As IntPtr = Nothing
            If hctr.AllocationStruct.m_uicStackAddr > 0 Then
                If nMaxStackFrames > 0 Then
                    nMaxStackFrames = Math.Min(hctr.AllocationStruct.m_uicStackAddr, nMaxStackFrames)
                Else
                    nMaxStackFrames = hctr.AllocationStruct.m_uicStackAddr
                End If
                ReDim res(nMaxStackFrames - 1)
                Dim offset = GetStackMapOffset(hctr) ' offset into page file
                Dim addrStacks = MapStackView(offset, nMaxStackFrames * IntPtr.Size)
#If DEBUG Then
                Dim tst = Marshal.ReadIntPtr(addrStacks)
                Debug.Assert(tst.ToInt32 = &H1234, String.Format("Beginning of stack not right {0:x0}", tst.ToInt32))

                tst = Marshal.ReadIntPtr(addrStacks + nMaxStackFrames * 4 + 4)
                Debug.Assert(tst.ToInt32 = &H4321, String.Format("Beginning of stack not right {0:x0}", tst.ToInt32))
                addrStacks += 4
#End If
                Dim nBytes = ReadProcessMemoryAsIntPtrArray(GetCurrentProcess(), addrStacks, nMaxStackFrames, IntPtr.Size, res)
                Debug.Assert(nBytes = IntPtr.Size * nMaxStackFrames)
                'For i = 0 To nMaxStackFrames - 1
                '    res(i) = Marshal.ReadIntPtr(addrStacks + i * 4)
                'Next
            End If
            Return res
        End Function


        <StructLayout(LayoutKind.Sequential)>
        Public Structure StackMemMapStats
            Public nFileMaps As UInteger
            Public nCompactions As UInteger
            Public ulOffsetFreeMem As UInteger
            Public nCurAlloc As UInteger
            Public ulMemSize As UInteger
            Public nMapViews As UInteger
            Public nFreeListSize As UInteger
            Public Overrides Function ToString() As String
                'Dim addr = MapStackView(0, 8)
                'Dim dat = Marshal.ReadInt32(addr)
                Return String.Format("NFileM {0} Compact{1} Offset {2:n0} CurAlloc{3:n0} MemSize{4:n0} nMapVws {5:n0} FreeSize{6:n0}",
                                     nFileMaps,
                                     nCompactions,
                                     ulOffsetFreeMem,
                                     nCurAlloc,
                                     ulMemSize,
                                     nMapViews,
                                     nFreeListSize
                                     )
            End Function

            Public Shared Function GetMemMapStats() As StackMemMapStats
                Dim procblk As New StackMemMapStats
                If IsUsingStackMemMap Then
                    Dim dwBytesRead = 0
                    If ReadProcessMemoryMemMapStruct(_hProcessTarget, _AddressOfStackMemMapStats, procblk, Marshal.SizeOf(GetType(StackMemMapStats)), dwBytesRead) = 0 Then
                        UpdateStatusMsg("reading procmem for ReadHeaps failed", fAssert:=True)
                    End If
                End If
                Return procblk
            End Function

        End Structure


    End Module

End Namespace
