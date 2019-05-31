Imports System.IO
Imports System.Runtime.InteropServices
' minidumps from watson: http://mswikis/autobug/Pages/Dumps%20and%20Dump%20Types.aspx

Namespace MemSpect

    Public NotInheritable Class MiniDumpReader
        Implements IDisposable
        Friend _hFileHandlMiniDump As IntPtr
        Friend _minidumpFileSize As ULong
        Friend _strmPtr As New IntPtr ' raw from minidump
        Friend _strmSize As UInteger
        Friend Structure MappingData
            Friend _hFileMapping As IntPtr
            Friend _addrFileMapping As IntPtr
            Friend _mapOffset As Long
            Friend _mappedSize As UInt32
        End Structure
        Friend _mappingDataCurrent As MappingData

        Public _MemoryDictionary As New SortedList(Of MemDictEntry, UInteger)
        Public _ModuleDictionary As Dictionary(Of IntPtr, ModuleData)

        Public Shared ReadOnly Property Singleton As MiniDumpReader 'singleton per offline snap :)
            Get
                If _offlineSnapshot Is Nothing AndAlso _ConnectionMode = MemSpectMode.MiniDumpOnly Then
                    _offlineSnapshot = New OfflineMegaSnapshot(_MiniDumpFileName)
                End If
                If _offlineSnapshot IsNot Nothing AndAlso _offlineSnapshot._InstanceMiniDumpReader Is Nothing Then
                    If String.IsNullOrEmpty(_offlineSnapshot._DataFilePath) OrElse _ConnectionMode = MemSpectMode.OnLine Then
                        Throw New InvalidOperationException("must be offline for reading minidumps")
                    End If
                    Dim minidumpfile = String.Empty
                    If _ConnectionMode <> MemSpectMode.MiniDumpOnly Then
                        Dim dmpFileName = "minidmp.dmp"
                        If Not String.IsNullOrEmpty(_MiniDumpFileName) Then
                            dmpFileName = _MiniDumpFileName
                        End If
                        minidumpfile = Path.Combine(_offlineSnapshot._DataFilePath, dmpFileName)
                    Else
                        minidumpfile = _MiniDumpFileName
                    End If
                    _offlineSnapshot._InstanceMiniDumpReader = New MiniDumpReader(minidumpfile)
                End If
                Return _offlineSnapshot._InstanceMiniDumpReader
            End Get
        End Property

        <DebuggerDisplay("{ToString()}")>
        Public Structure MemDictEntry
            Implements IComparable

            Dim StartAddr As IntPtr ' the real mem addr in the minidump target process
            Dim nSize As UInteger ' size of the mem for the dict entry 

            Public Function CompareTo(ByVal obj As Object) As Integer Implements System.IComparable.CompareTo
                Dim res = 0
                Dim other = CType(obj, MemDictEntry)
                ' check non-overlap cases
                Dim mestart = Me.StartAddr.ToInt64 And UInteger.MaxValue
                Dim otherstart = other.StartAddr.ToInt64 And UInteger.MaxValue
                If mestart + Me.nSize < otherstart Then
                    Return -1
                End If
                If mestart > otherstart + other.nSize Then
                    Return 1
                End If
                ' check overlap cases
                If mestart >= otherstart Then
                    If mestart + Me.nSize < otherstart + other.nSize Then
                        Return 0
                    End If
                    Return 1
                Else
                    Return -1
                End If
                If otherstart + other.nSize < mestart Then
                    Return 1
                End If

                If mestart + Me.nSize > otherstart Then
                    res = -1
                ElseIf Me.StartAddr = other.StartAddr Then
                    res = 0
                ElseIf mestart > otherstart Then
                    res = 1
                End If
                Return res
            End Function
            Public Overrides Function ToString() As String
                Return String.Format("Addr={0:x8} Size={1:x8}", StartAddr.ToInt32, nSize)
            End Function
        End Structure

        Public Sub New(ByVal minidumpFilename As String)
            _MiniDumpFileName = minidumpFilename
            _minidumpFileSize = CULng((New FileInfo(minidumpFilename)).Length)
            _hFileHandlMiniDump = CreateFile(
                minidumpFilename,
                EFileAccess.GENERIC_READ,
                EFileShare.FILE_SHARE_READ,
                lpSecurityAttributes:=IntPtr.Zero,
                dwCreationDisposition:=ECreationDisposition.OPEN_EXISTING,
                dwFlagsAndAttributes:=EFileAttributes.FILE_ATTRIBUTE_READONLY,
                hTemplateFile:=IntPtr.Zero)
            If _hFileHandlMiniDump <> IntPtr.Zero AndAlso _hFileHandlMiniDump.ToInt32 <> &HFFFFFFFF Then
                _mappingDataCurrent._hFileMapping = CreateFileMapping(_hFileHandlMiniDump, 0, AllocationProtect.PAGE_READONLY, 0, 0, Nothing)
            Else
                Throw New InvalidOperationException("could not open minidump file " + minidumpFilename + " " + GetErrorMessageFromWin32LastError(Marshal.GetLastWin32Error))
            End If
            Dim x = Me.GetMinidumpSystemInfo()
            If x.processorArchitecture <> ProcessorArchitecture.PROCESSOR_ARCHITECTURE_INTEL Then
                Throw New InvalidOperationException("Dump Processor Architecture = " + x.processorArchitecture.ToString)
            End If

        End Sub

        Public Function ReadStreamType(ByVal strmType As MINIDUMP_STREAM_TYPE) As MINIDUMP_LOCATION_DESCRIPTOR
            Dim initloc As New MINIDUMP_LOCATION_DESCRIPTOR With {.Rva = 0, .DataSize = AllocationGranularity}
            If MapStream(initloc) = IntPtr.Zero Then ' BOF
                Throw New Win32Exception(Marshal.GetLastWin32Error, "MapViewOfFile failed")
            End If
            Dim dirPtr As IntPtr ' to  MINIDUMP_DIRECTORY
            Dim dir As MINIDUMP_DIRECTORY
            _strmPtr = IntPtr.Zero
            _strmSize = 0
            dirPtr = IntPtr.Zero
            If MiniDumpReadDumpStream(
                                 _mappingDataCurrent._addrFileMapping,
                                strmType,
                                dirPtr,
                                _strmPtr,
                               _strmSize) Then
                dir = CType(Marshal.PtrToStructure(dirPtr, GetType(MINIDUMP_DIRECTORY)), MINIDUMP_DIRECTORY)
            Else
                dir.Location.Rva = 0
                dir.Location.DataSize = 0
            End If
            Return dir.Location
        End Function
        ' map a stream location into mem, returning a ptr to mem
        Public Function MapStream(ByVal loc As MINIDUMP_LOCATION_DESCRIPTOR) As IntPtr
            Dim intptrRetval = IntPtr.Zero
            Dim newbaseOffset = (CLng(loc.Rva) \ AllocationGranularity) * AllocationGranularity

            Dim mapViewSize = CUInt(AllocationGranularity * 4)
            Dim nLeftover = loc.Rva - newbaseOffset
            If newbaseOffset > UInteger.MaxValue Then
                Throw New InvalidOperationException("newbase out of range")
            End If
            Dim preferredAddress = _mappingDataCurrent._addrFileMapping
            Dim fFits = loc.Rva >= _mappingDataCurrent._mapOffset AndAlso
                loc.Rva + loc.DataSize < _mappingDataCurrent._mapOffset + _mappingDataCurrent._mappedSize
            If Not fFits Then
                If preferredAddress <> IntPtr.Zero Then
                    Dim res = UnmapViewOfFile(preferredAddress)
                    Debug.Assert(res, "unmapview failed?")
                End If
                Dim hiPart As UInteger = CUInt(newbaseOffset >> 32) And UInteger.MaxValue
                Dim loPart = CUInt(newbaseOffset)
                If loc.DataSize + nLeftover > mapViewSize Then
                    mapViewSize = CUInt(loc.DataSize + nLeftover)
                End If
                If newbaseOffset + mapViewSize >= _minidumpFileSize Then
                    mapViewSize = Math.Min(CUInt(loc.DataSize + nLeftover), CUInt(_minidumpFileSize))
                End If
                _mappingDataCurrent._addrFileMapping = MapViewOfFileEx(_mappingDataCurrent._hFileMapping, FILE_MAP_READ, hiPart, loPart, mapViewSize, preferredAddress)
                If _mappingDataCurrent._addrFileMapping = IntPtr.Zero Then
                    If preferredAddress <> IntPtr.Zero Then
                        preferredAddress = IntPtr.Zero
                        _mappingDataCurrent._addrFileMapping = MapViewOfFileEx(_mappingDataCurrent._hFileMapping, FILE_MAP_READ, hiPart, loPart, mapViewSize, preferredAddress)
                    End If
                    If _mappingDataCurrent._addrFileMapping = IntPtr.Zero Then
                        Dim lerr = Marshal.GetLastWin32Error
                        Throw New Win32Exception(lerr, String.Format("MapViewOfFile failed {0:x8}  {1:x8} LastErr={2:x8}",
                                                                          newbaseOffset, loc.DataSize, lerr))
                    End If
                End If
                _mappingDataCurrent._mapOffset = newbaseOffset
                _mappingDataCurrent._mappedSize = mapViewSize

            End If
            intptrRetval = _mappingDataCurrent._addrFileMapping + CInt(newbaseOffset - _mappingDataCurrent._mapOffset + nLeftover) ' must fit in 32 bits!
            Return intptrRetval
        End Function

        Public Function GetMinidumpSystemInfo() As NativeImports.MINIDUMP_SYSTEM_INFO
            Dim res As NativeImports.MINIDUMP_SYSTEM_INFO
            Dim locStream = ReadStreamType(MINIDUMP_STREAM_TYPE.SystemInfoStream)
            If locStream.Rva <> 0 AndAlso locStream.DataSize <> 0 Then
                Dim sysInfoStrm = MapStream(locStream)
                res = CType(Marshal.PtrToStructure(sysInfoStrm, GetType(MINIDUMP_SYSTEM_INFO)), MINIDUMP_SYSTEM_INFO)
            End If
            Return res
        End Function

        Public Sub MakeMemoryDictionary()
            If _MemoryDictionary.Count = 0 Then
                UpdateStatusMsg("Reading minidump mem data to dict")

                ' 2 types of stream: Memory64 (used for _MINIDUMP_TYPE.MiniDumpWithFullMemory) and Memory: 
                Dim memliststreamDesc = ReadStreamType(MINIDUMP_STREAM_TYPE.Memory64ListStream)
                If memliststreamDesc.Rva <> 0 AndAlso memliststreamDesc.DataSize <> 0 Then
                    Dim memliststream = MapStream(memliststreamDesc)
                    Dim memList = CType(Marshal.PtrToStructure(memliststream,
                                               GetType(MINIDUMP_MEMORY64_LIST)), MINIDUMP_MEMORY64_LIST)

                    Dim nDescriptorSize = CUInt(Marshal.SizeOf(GetType(MINIDUMP_MEMORY_DESCRIPTOR64))) '16

                    Dim locrva As New MINIDUMP_LOCATION_DESCRIPTOR With {
                        .Rva = CUInt(memliststreamDesc.Rva + CInt(Marshal.SizeOf(GetType(MINIDUMP_MEMORY64_LIST)))),
                        .DataSize = nDescriptorSize
                    }
                    Dim posSoFar As ULong = 0
                    For i = 0 To memList.NumberOfMemoryRanges - 1
                        Dim ptr = MapStream(locrva)
                        Dim memrange = CType(Marshal.PtrToStructure(ptr,
                                    GetType(MINIDUMP_MEMORY_DESCRIPTOR64)), MINIDUMP_MEMORY_DESCRIPTOR64)
                        If memrange.DataSize > UInteger.MaxValue Then
                            Throw New InvalidOperationException("mem size too big")
                        End If

                        Dim iptr = LongToIntPtr(memrange.StartOfMemoryRange)
                        Dim dentry As New MemDictEntry With {
                            .nSize = CUInt(memrange.DataSize),
                            .StartAddr = iptr
                        }
                        _MemoryDictionary.Add(dentry, CUInt(posSoFar + memList.BaseRva))
                        posSoFar += memrange.DataSize
                        locrva.Rva += nDescriptorSize
                    Next
                End If
                ' now the non-64 version (slightly different, not just structs, but no baseRva)
                memliststreamDesc = Me.ReadStreamType(MINIDUMP_STREAM_TYPE.MemoryListStream)
                If memliststreamDesc.Rva <> 0 AndAlso memliststreamDesc.DataSize <> 0 Then
                    Dim memliststream = MapStream(memliststreamDesc)
                    Dim memList = CType(Marshal.PtrToStructure(memliststream,
                                               GetType(MINIDUMP_MEMORY_LIST)), MINIDUMP_MEMORY_LIST)

                    Dim nDescriptorSize = CUInt(Marshal.SizeOf(GetType(MINIDUMP_MEMORY_DESCRIPTOR)))

                    Dim locrva As New MINIDUMP_LOCATION_DESCRIPTOR With {
                        .Rva = CUInt(memliststreamDesc.Rva + CInt(Marshal.SizeOf(GetType(MINIDUMP_MEMORY_LIST)))),
                        .DataSize = nDescriptorSize
                    }
                    For i = 0 To memList.NumberOfMemoryRanges - 1
                        Dim ptr = MapStream(locrva)
                        Dim memrange = CType(Marshal.PtrToStructure(ptr,
                                    GetType(MINIDUMP_MEMORY_DESCRIPTOR)), MINIDUMP_MEMORY_DESCRIPTOR)

                        Dim iptr = New IntPtr(CInt(memrange.StartOfMemoryRange))
                        Dim dentry As New MemDictEntry With {
                            .nSize = CUInt(memrange.MemoryLocDesc.DataSize),
                            .StartAddr = iptr
                        }
                        _MemoryDictionary.Add(dentry, CUInt(memrange.MemoryLocDesc.Rva))
                        locrva.Rva += nDescriptorSize
                    Next
                End If
                If _MemoryDictionary.Count = 0 Then
                    Throw New InvalidOperationException("can't read any memory outof minidump")
                End If
                UpdateStatusMsg("MemDictCreated")
            End If

        End Sub

        Public Function ReadMemoryDictionary(ByVal addr As IntPtr, ByVal nsize As Integer) As Byte()

            If _MemoryDictionary.Count = 0 Then
                MakeMemoryDictionary()
                If _MemoryDictionary.Count = 0 Then
                    Throw New InvalidOperationException("can't make mem dict from minidump")
                End If
            End If
            Dim result(0) As Byte
            result = Nothing
            Dim intres = -1
            Dim targ As New MemDictEntry With {.StartAddr = CType(addr, IntPtr)}

            Dim findres = FindNearest(Of MemDictEntry)(
                            CType(_MemoryDictionary.Keys, IList(Of MiniDumpReader.MemDictEntry)),
                            targ,
                            Nothing,
                            Nothing,
                            Nothing)

            If findres(0) >= 0 AndAlso findres(1) >= 0 Then ' bingo: got a near one. 
                intres = findres(0)

                Dim rva = _MemoryDictionary.Values(intres)
                Dim dictentry = _MemoryDictionary.Keys(intres)
                ' now check to see if the addr is in range of request

                Dim diff = (addr.ToInt64 And UInteger.MaxValue) -
                                                    (dictentry.StartAddr.ToInt64 And UInteger.MaxValue)

                '                Dim resultAdd = dictentry.StartAddr.MyAdd(dictentry.nSize).MyAdd(diff).ToInt64
                If diff >= 0 AndAlso diff < dictentry.nSize Then

                    Dim rvaDescript = CUInt(CULng(rva) + diff)


                    Dim mld = New MINIDUMP_LOCATION_DESCRIPTOR With {
                                        .Rva = rvaDescript,
                                        .DataSize = Math.Min(dictentry.nSize, CUInt(nsize))
                                    }

                    Dim ptrRawMem = MapStream(mld)
                    Dim blk As New ProcMemBlockByte
                    Dim dwBytesRead = 0
                    If ReadProcessMemoryByte(_MemSpectProcessHandle,
                                             ptrRawMem,
                                             blk,
                                             Math.Min(Marshal.SizeOf(blk), nsize), dwBytesRead) <> 0 Then
                        ReDim Preserve blk.data(nsize - 1)
                        result = blk.data
                    End If
                End If
            End If
            Return result
        End Function

        Private _VirtAllocs As SortedList(Of IntPtr, MEMORY_BASIC_INFORMATION)
        Public Function GetVirtualAllocs() As SortedList(Of IntPtr, MEMORY_BASIC_INFORMATION)
            If _VirtAllocs Is Nothing Then
                _VirtAllocs = New SortedList(Of IntPtr, MEMORY_BASIC_INFORMATION)(_CompareIntPtr)
                Dim strmDescriptor = Me.ReadStreamType(MINIDUMP_STREAM_TYPE.MemoryInfoListStream)
                If strmDescriptor.Rva = 0 AndAlso strmDescriptor.DataSize = 0 Then
                    Throw New InvalidOperationException("minidump has no " +
                                                        MINIDUMP_STREAM_TYPE.MemoryInfoListStream.ToString +
                                                        " it needs _MINIDUMP_TYPE.MiniDumpWithFullMemoryInfo flag" + vbCrLf +
                                                        "  MemSpect Launcher ->CreateMinidump or" + vbCrLf +
                                                        "  ProcessExplorer->Right-click on process->CreateDump->CreateFullDump.")
                End If
                Dim vmstream = MapStream(strmDescriptor)
                Dim vmemlist = CType(Marshal.PtrToStructure(vmstream, GetType(MINIDUMP_MEMORY_INFO_LIST)), MINIDUMP_MEMORY_INFO_LIST)
                Dim nDescSize = CUInt(Marshal.SizeOf(GetType(MINIDUMP_MEMORY_INFO)))
                Dim locrva As New MINIDUMP_LOCATION_DESCRIPTOR With {
                    .Rva = CUInt(strmDescriptor.Rva + CInt(Marshal.SizeOf(GetType(MINIDUMP_MEMORY_INFO_LIST)))),
                    .DataSize = nDescSize}
                For i = 0 To vmemlist.NumberOfEntries - 1
                    Dim ptr = MapStream(locrva)
                    Dim vmInfo = CType(Marshal.PtrToStructure(ptr,
                                GetType(MINIDUMP_MEMORY_INFO)), MINIDUMP_MEMORY_INFO)
                    If vmInfo.RegionSize > UInteger.MaxValue Then
                        Exit For
                    End If

                    Dim mbi As New MEMORY_BASIC_INFORMATION With {
                        .AllocationBase = vmInfo.AllocationBase.LongToIntPtr,
                        .BaseAddress = vmInfo.BaseAddress.LongToIntPtr,
                        .RegionSize = CUInt(vmInfo.RegionSize),
                        .lType = CType(vmInfo.Type, AllocationType),
                        .State = CType(vmInfo.State, AllocationState),
                        .Protect = CType(vmInfo.Protect, AllocationProtect),
                        .AllocationProtect = CType(vmInfo.AllocationProtect, AllocationProtect)
                    }
                    _VirtAllocs.Add(mbi.BaseAddress, mbi)
                    locrva.Rva += nDescSize
                Next
            End If
            Return _VirtAllocs
        End Function

        Private _didLoadModules As Boolean = False ' singleton

        Public Sub LoadModules()
            ' we want to load modules into the symbol resolver so pdbs can be loaded, and source files found
            FillModuleDictionary()
            If Not _didLoadModules Then
                For Each itm In _ModuleDictionary

                    VsLoadModuleInfo(CType(_MemSpectProcessHandle, IntPtr),
                                     itm.Value.ModuleName,
                                     itm.Value.minidump_Module.BaseOfImage.LongToIntPtr,
                                     CInt(itm.Value.minidump_Module.SizeOfImage)
                                     )

                Next
                _didLoadModules = True
            End If
        End Sub

        Public Structure ModuleData
            Dim ModuleName As String
            Dim minidump_Module As MINIDUMP_MODULE
        End Structure
        <System.Runtime.ExceptionServices.HandleProcessCorruptedStateExceptions()>
        Private Sub FillModuleDictionary()
            If _ModuleDictionary Is Nothing Then
                Try

                    If _ModuleDictionary Is Nothing Then
                        _ModuleDictionary = New Dictionary(Of IntPtr, ModuleData)
                        Dim strmDescriptor = Me.ReadStreamType(MINIDUMP_STREAM_TYPE.ModuleListStream)
                        Dim modulestream = MapStream(strmDescriptor)
                        Dim modulelist = CType(Marshal.PtrToStructure(modulestream, GetType(MINIDUMP_MODULE_LIST)), MINIDUMP_MODULE_LIST)
                        Dim nDescSize = CUInt(Marshal.SizeOf(GetType(MINIDUMP_MODULE)) - 4)
                        Dim locrva As New MINIDUMP_LOCATION_DESCRIPTOR With {
                            .Rva = CUInt(strmDescriptor.Rva + CInt(Marshal.SizeOf(GetType(MINIDUMP_MODULE_LIST)))),
                            .DataSize = CUInt(nDescSize + 4)}
                        For i = 0 To modulelist.NumberOfModules - 1
                            Dim ptr = MapStream(locrva)
                            Dim moduleInfo = CType(Marshal.PtrToStructure(ptr,
                                        GetType(MINIDUMP_MODULE)), MINIDUMP_MODULE)

                            Dim modulename = GetNameFromRva(moduleInfo.ModuleNameRva)

                            If moduleInfo.BaseOfImage > UInteger.MaxValue Then
                                Dim r = 2 ' C:\Windows\System32\ntdll.dll on 64 bit shows with humongo base addr
                            Else
                                Dim moduleKey = moduleInfo.BaseOfImage.LongToIntPtr

                                If Not _ModuleDictionary.ContainsKey(moduleKey) Then
                                    _ModuleDictionary.Add(moduleKey, New ModuleData With {
                                                          .ModuleName = modulename,
                                                          .minidump_Module = moduleInfo
                                                      })

                                End If

                            End If
                            locrva.Rva += nDescSize
                        Next
                    End If
                Catch ex As Exception
                    Common.MemSpectExceptionHandler(ex)
                End Try

            End If

        End Sub

        <System.Runtime.ExceptionServices.HandleProcessCorruptedStateExceptions()>
        Function GetModuleName(ByVal addr As IntPtr) As String
            Dim modname = ""
            FillModuleDictionary()
            Dim md As ModuleData = Nothing
            If _ModuleDictionary.TryGetValue(addr, md) Then ' failure returns nothing, not ""
                modname = md.ModuleName
            End If
            Return modname
        End Function

        Public Function GetThreadInfoFromMinidump() As List(Of MINIDUMP_THREAD)
            Dim threads As New List(Of MINIDUMP_THREAD)
            Dim locStream = ReadStreamType(MINIDUMP_STREAM_TYPE.ThreadListStream)
            Dim thrdstrm = MapStream(locStream)
            Dim thrdList = CType(Marshal.PtrToStructure(thrdstrm, GetType(MINIDUMP_THREAD_LIST)), MINIDUMP_THREAD_LIST)
            Dim nDescSize = CUInt(Marshal.SizeOf(GetType(MINIDUMP_THREAD)))

            Dim locrva As New MINIDUMP_LOCATION_DESCRIPTOR With {
                .Rva = CUInt(locStream.Rva + CInt(Marshal.SizeOf(GetType(MINIDUMP_THREAD_LIST)))),
                .DataSize = CUInt(nDescSize)}
            For i = 0 To thrdList.NumberOfThreads - 1
                Dim ptr = MapStream(locrva)
                Dim thrdInfo = CType(Marshal.PtrToStructure(ptr,
                            GetType(MINIDUMP_THREAD)), MINIDUMP_THREAD)

                threads.Add(thrdInfo)
                locrva.Rva += nDescSize
            Next
            Return threads
        End Function

        Friend Function GetNameFromRva(ByVal rva As UInteger) As String
            Dim retstr = ""
            If rva <> 0 Then
                Dim locname = MapStream(New MINIDUMP_LOCATION_DESCRIPTOR With {
                                           .Rva = rva,
                                           .DataSize = 600
                                       }
                                   )
                retstr = Marshal.PtrToStringBSTR(locname.MyAdd(4)) ' skip len
            End If
            Return retstr
        End Function

        Public ReadOnly Property IsValid As Boolean
            Get
                If _hFileHandlMiniDump = IntPtr.Zero Then
                    Return False
                End If
                If _mappingDataCurrent._hFileMapping = IntPtr.Zero Then
                    Return False
                End If
                Return True
            End Get
        End Property

        Private disposedValue As Boolean ' To detect redundant calls
        ' IDisposable
        Friend Sub Dispose(ByVal disposing As Boolean)
            If Not Me.disposedValue Then
                If disposing Then
                    ' TODO: dispose managed state (managed objects).
                End If
                If _mappingDataCurrent._addrFileMapping <> IntPtr.Zero Then
                    UnmapViewOfFile(_mappingDataCurrent._addrFileMapping)
                    _mappingDataCurrent._addrFileMapping = IntPtr.Zero
                End If
                If _mappingDataCurrent._hFileMapping <> IntPtr.Zero Then
                    CloseHandle(_mappingDataCurrent._hFileMapping)
                    _mappingDataCurrent._hFileMapping = IntPtr.Zero
                End If
                If _hFileHandlMiniDump <> IntPtr.Zero Then
                    CloseHandle(_hFileHandlMiniDump)
                End If
                _hFileHandlMiniDump = IntPtr.Zero
                '                    _hFile.Dispose()
                ' TODO: free unmanaged resources (unmanaged objects) and override Finalize() below.
                ' TODO: set large fields to null.
            End If
            Me.disposedValue = True
        End Sub

        ' TODO: override Finalize() only if Dispose(ByVal disposing As Boolean) above has code to free unmanaged resources.
        Protected Overrides Sub Finalize()
            ' Do not change this code.  Put cleanup code in Dispose(ByVal disposing As Boolean) above.
            Dispose(False)
            MyBase.Finalize()
        End Sub

        ' This code added by Visual Basic to correctly implement the disposable pattern.
        Public Sub Dispose() Implements IDisposable.Dispose
            ' Do not change this code.  Put cleanup code in Dispose(ByVal disposing As Boolean) above.
            Dispose(True)
            GC.SuppressFinalize(Me)
        End Sub


        Friend Shared Sub Clear() ' needed for test scenarios
            If _offlineSnapshot IsNot Nothing AndAlso _offlineSnapshot._InstanceMiniDumpReader IsNot Nothing Then
                _offlineSnapshot._InstanceMiniDumpReader._didLoadModules = False
                _offlineSnapshot._InstanceMiniDumpReader._MemoryDictionary.Clear()
                _offlineSnapshot._InstanceMiniDumpReader._ModuleDictionary = Nothing
                _offlineSnapshot._InstanceMiniDumpReader = Nothing
            End If
        End Sub


    End Class
End Namespace
