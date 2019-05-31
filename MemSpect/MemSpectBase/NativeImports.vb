Imports System.Runtime.InteropServices
Imports FastSerialization0
Imports System.Text

Namespace MemSpect

    Public Module NativeImports
        <Flags>
        Public Enum CodeMarkerActionEnum
            CodeMarkerAction_TakeMemStatSnapShot = 1
            CodeMarkerAction_TakeMegaSnapShot = 2
            CodeMarkerAction_Freeze = 4
            CodeMarkerAction_ShowInStatusMessage = 8
            CodeMarkerAction_Crash = &H10
            CodeMarkerAction_Hang = &H20
            CodeMarkerAction_Sleep = &H40
            CodeMarkerAction_DebugBreak = &H80
            CodeMarkerAction_Recur = &H100

        End Enum

        Public Const MAX_PATH As Integer = 260

        Public Const FILE_MAP_READ As Int32 = &H4
        Public Const FILE_MAP_WRITE As Int32 = &H2
        Public Const INVALID_HANDLE_VALUE = -1
        Public Enum EFileShare
            FILE_SHARE_NONE = &H0
            FILE_SHARE_READ = &H1
            FILE_SHARE_WRITE = &H2
            FILE_SHARE_DELETE = &H4
        End Enum

        Public Enum EFileAccess As System.Int32
            ''
            ''  The following are masks for the predefined standard access types
            ''

            DELETE = &H10000
            READ_CONTROL = &H20000
            WRITE_DAC = &H40000
            WRITE_OWNER = &H80000
            SYNCHRONIZE = &H100000

            STANDARD_RIGHTS_REQUIRED = &HF0000
            STANDARD_RIGHTS_READ = READ_CONTROL
            STANDARD_RIGHTS_WRITE = READ_CONTROL
            STANDARD_RIGHTS_EXECUTE = READ_CONTROL
            STANDARD_RIGHTS_ALL = &H1F0000
            SPECIFIC_RIGHTS_ALL = &HFFFF

            ''
            '' AccessSystemAcl access type
            ''
            ACCESS_SYSTEM_SECURITY = &H1000000
            ''
            '' MaximumAllowed access type
            ''
            MAXIMUM_ALLOWED = &H2000000

            GENERIC_READ = &H80000000
            GENERIC_WRITE = &H40000000
            GENERIC_EXECUTE = &H20000000
            GENERIC_ALL = &H10000000
        End Enum

        Public Enum ECreationDisposition
            ''' <summary>
            ''' Creates a new file, only if it does not already exist.
            ''' If the specified file exists, the function fails and the last-error code is set to ERROR_FILE_EXISTS (80).
            ''' If the specified file does not exist and is a valid path to a writable location, a new file is created.
            ''' </summary>
            CREATE_NEW = 1

            ''' <summary>
            ''' Creates a new file, always.
            ''' If the specified file exists and is writable, the function overwrites the file, the function succeeds, and last-error code is set to ERROR_ALREADY_EXISTS (183).
            ''' If the specified file does not exist and is a valid path, a new file is created, the function succeeds, and the last-error code is set to zero.
            ''' For more information, see the Remarks section of this topic.
            ''' </summary>
            CREATE_ALWAYS = 2

            ''' <summary>
            ''' Opens a file or device, only if it exists.
            ''' If the specified file or device does not exist, the function fails and the last-error code is set to ERROR_FILE_NOT_FOUND (2).
            ''' For more information about devices, see the Remarks section.
            ''' </summary>
            OPEN_EXISTING = 3

            ''' <summary>
            ''' Opens a file, always.
            ''' If the specified file exists, the function succeeds and the last-error code is set to ERROR_ALREADY_EXISTS (183).
            ''' If the specified file does not exist and is a valid path to a writable location, the function creates a file and the last-error code is set to zero.
            ''' </summary>
            OPEN_ALWAYS = 4

            ''' <summary>
            ''' Opens a file and truncates it so that its size is zero bytes, only if it exists.
            ''' If the specified file does not exist, the function fails and the last-error code is set to ERROR_FILE_NOT_FOUND (2).
            ''' The calling process must open the file with the GENERIC_WRITE bit set as part of the dwDesiredAccess parameter.
            ''' </summary>
            TRUNCATE_EXISTING = 5
        End Enum

        Public Enum EFileAttributes
            FILE_ATTRIBUTE_READONLY = &H1
            FILE_ATTRIBUTE_HIDDEN = &H2
            FILE_ATTRIBUTE_SYSTEM = &H4
            FILE_ATTRIBUTE_DIRECTORY = &H10
            FILE_ATTRIBUTE_ARCHIVE = &H20
            FILE_ATTRIBUTE_DEVICE = &H40
            FILE_ATTRIBUTE_NORMAL = &H80
            FILE_ATTRIBUTE_TEMPORARY = &H100
            FILE_ATTRIBUTE_SPARSE_FILE = &H200
            FILE_ATTRIBUTE_REPARSE_POINT = &H400
            FILE_ATTRIBUTE_COMPRESSED = &H800
            FILE_ATTRIBUTE_OFFLINE = &H1000
            FILE_ATTRIBUTE_NOT_CONTENT_INDEXED = &H2000
            FILE_ATTRIBUTE_ENCRYPTED = &H4000
            FILE_ATTRIBUTE_VIRTUAL = &H10000

            'This parameter can also contain combinations of flags (FILE_FLAG_*) 
            FILE_FLAG_BACKUP_SEMANTICS = &H2000000
            FILE_FLAG_DELETE_ON_CLOSE = &H4000000
            FILE_FLAG_NO_BUFFERING = &H2000000
            FILE_FLAG_OPEN_NO_RECALL = &H100000
            FILE_FLAG_OPEN_REPARSE_POINT = &H200000
            FILE_FLAG_OVERLAPPED = &H40000000
            FILE_FLAG_POSIX_SEMANTICS = &H100000
            FILE_FLAG_RANDOM_ACCESS = &H10000000
            FILE_FLAG_SEQUENTIAL_SCAN = &H8000000
            FILE_FLAG_WRITE_THROUGH = &H80000000
        End Enum

        <DllImport("kernel32.dll", SetLastError:=True, CharSet:=System.Runtime.InteropServices.CharSet.Auto)>
        Public Function CreateFile(ByVal lpFileName As String,
            ByVal dwDesiredAccess As EFileAccess,
            ByVal dwShareMode As EFileShare,
            ByVal lpSecurityAttributes As IntPtr,
            ByVal dwCreationDisposition As ECreationDisposition,
            ByVal dwFlagsAndAttributes As EFileAttributes,
            ByVal hTemplateFile As IntPtr) As IntPtr
        End Function

        <DllImport("kernel32.dll", SetLastError:=True)>
        Public Function CloseHandle(ByVal hFile As IntPtr) As Boolean

        End Function

        <DllImport("kernel32.dll", SetLastError:=True)>
        Public Function GetCurrentProcess() As IntPtr

        End Function

        <DllImport("kernel32.dll", SetLastError:=True)>
        Public Function GetCurrentThreadId() As IntPtr

        End Function

        <DllImport("kernel32.dll", SetLastError:=True)>
        Public Function IsDebuggerPresent() As Integer

        End Function


        <DllImport("Kernel32", CharSet:=CharSet.Auto, SetLastError:=True)>
        Public Function CreateFileMapping(
                                         ByVal hFile As IntPtr,
                                         ByVal lpAttributes As UInt32,
                                         ByVal flProtect As UInt32,
                                         ByVal dwMaxSizeHi As UInt32,
                                         ByVal dwMaxSizeLow As UInt32,
                                         ByVal lpName As String
                                         ) As IntPtr
        End Function

        'Public Declare Function MapViewOfFile Lib "kernel32" (ByVal hFileMappingObject As IntPtr,
        '                                               ByVal dwDesiredAccess As UInt32,
        '                                               ByVal dwFileOffsetHigh As UInt32,
        '                                               ByVal dwFileOffsetLow As UInt32,
        '                                               ByVal dwNumberOfBytesToMap As UInt32) As IntPtr

        <DllImport("kernel32.dll", SetLastError:=True, CharSet:=CharSet.Auto)>
        Public Function MapViewOfFile(ByVal hFileMappingObject As IntPtr,
                                               ByVal dwDesiredAccess As UInt32,
                                               ByVal dwFileOffsetHigh As UInt32,
                                               ByVal dwFileOffsetLow As UInt32,
                                               ByVal dwNumberOfBytesToMap As UInt32) As IntPtr

        End Function
        <DllImport("kernel32.dll", SetLastError:=True, CharSet:=CharSet.Auto)>
        Public Function MapViewOfFileEx(ByVal hFileMappingObject As IntPtr,
                                               ByVal dwDesiredAccess As UInt32,
                                               ByVal dwFileOffsetHigh As UInt32,
                                               ByVal dwFileOffsetLow As UInt32,
                                               ByVal dwNumberOfBytesToMap As UInt32,
                                               ByVal PreferredAddress As IntPtr
                                               ) As IntPtr

        End Function

        <DllImport("Kernel32.dll", EntryPoint:="FormatMessageW", SetLastError:=True, CharSet:=CharSet.Unicode, CallingConvention:=CallingConvention.StdCall)>
        Public Function FormatMessage(
                                     ByVal dwFlags As Integer,
                                     ByRef lpSource As IntPtr,
                                     ByVal dwMessageId As Integer,
                                     ByVal dwLanguageId As Integer,
                                     ByVal lpBuffer As Text.StringBuilder,
                                     ByVal nSize As Integer,
                                     ByRef Arguments As IntPtr
                                      ) As Integer
        End Function

        Public Const FORMAT_MESSAGE_ALLOCATE_BUFFER = &H100
        Public Const FORMAT_MESSAGE_IGNORE_INSERTS = &H200
        Public Const FORMAT_MESSAGE_FROM_SYSTEM = &H1000
        Public Const FORMAT_MESSAGE_ARGUMENT_ARRAY = &H2000
        Public Const FORMAT_MESSAGE_FROM_HMODULE = &H800
        Public Const FORMAT_MESSAGE_FROM_STRING = &H400


        <DllImport("kernel32.dll", SetLastError:=False, CharSet:=CharSet.Auto)>
        Public Function UnmapViewOfFile(
              ByVal lpBaseAddress As IntPtr
            ) As Boolean
        End Function


        <DllImport("kernel32.dll", SetLastError:=False, CharSet:=CharSet.Auto)>
        Public Function OpenFileMapping(
                         ByVal dwDesiredAccess As UInt32,
                         ByVal bInheritHandle As UInt32,
                         ByVal lpName As String) As IntPtr
        End Function

        Public Const BlockSize As Integer = 4096

        <StructLayout(LayoutKind.Sequential)>
        Structure ProcMemBlock
            <MarshalAs(UnmanagedType.ByValArray, SizeConst:=BlockSize)>
            Dim data() As Char
        End Structure

        <StructLayout(LayoutKind.Sequential)>
        Structure ProcMemBlockByte
            <MarshalAs(UnmanagedType.ByValArray, SizeConst:=BlockSize)>
            Dim data() As Byte
        End Structure

        <StructLayout(LayoutKind.Sequential)>
        Structure ProcMemIntPtr
            <MarshalAs(UnmanagedType.ByValArray, SizeConst:=BlockSize)>
            Dim data() As IntPtr
        End Structure

        <Flags()>
        Public Enum PROCESS_HEAP_ENTRY_WFLAGS As UShort
            PROCESS_HEAP_ENTRY_NONE = 0
            PROCESS_HEAP_ENTRY_BUSY = &H4
            PROCESS_HEAP_ENTRY_DDESHARE = &H20
            PROCESS_HEAP_ENTRY_MOVEABLE = &H10
            PROCESS_HEAP_REGION = &H1
            PROCESS_HEAP_UNCOMMITTED_RANGE = &H2
        End Enum

        <StructLayoutAttribute(LayoutKind.Sequential)>
        Public Structure STRUCT_BLOCK
            Implements IFastSerializable, IFastSerializableVersion 'structs can't inherit from classes (SerializableObject), so we explicitly implement these

            Public hMem As IntPtr
            Public dwReserved1 As Integer
            Public dwReserved2 As Integer
            Public dwReserved3 As Integer

            Public Sub FromStream(ByVal deserializer As Deserializer, ByVal versionFromStream As Integer) Implements IFastSerializable.FromStream
                hMem = CType(deserializer.ReadInt(), IntPtr)
                dwReserved1 = deserializer.ReadInt()
                dwReserved2 = deserializer.ReadInt()
                dwReserved3 = deserializer.ReadInt()
            End Sub

            Public Sub ToStream(ByVal serializer As Serializer) Implements IFastSerializable.ToStream
                serializer.Write(hMem.ToInt32())
                serializer.Write(dwReserved1)
                serializer.Write(dwReserved2)
                serializer.Write(dwReserved3)
            End Sub

            Public ReadOnly Property MinimumVersion As Integer Implements IFastSerializableVersion.MinimumVersion
                Get
                    Return SerializableObject.CurrentMinimumVersion
                End Get
            End Property

            Public ReadOnly Property Version As Integer Implements IFastSerializableVersion.Version
                Get
                    Return SerializableObject.CurrentSerializationVersion
                End Get
            End Property
        End Structure

        <StructLayoutAttribute(LayoutKind.Sequential)>
        Public Structure STRUCT_REGION
            Implements IFastSerializable, IFastSerializableVersion 'structs can't inherit from classes (SerializableObject), so we explicitly implement these

            Public dwCommittedSize As Integer
            Public dwUnCommittedSize As Integer
            Public lpFirstBlock As IntPtr
            Public lpLastBlock As IntPtr

            Public Sub FromStream(ByVal deserializer As Deserializer, ByVal versionFromStream As Integer) Implements IFastSerializable.FromStream
                deserializer.Read(dwCommittedSize)
                deserializer.Read(dwUnCommittedSize)
                lpFirstBlock = CType(deserializer.ReadInt(), IntPtr)
                lpLastBlock = CType(deserializer.ReadInt(), IntPtr)
            End Sub

            Public Sub ToStream(ByVal serializer As Serializer) Implements IFastSerializable.ToStream
                serializer.Write(dwCommittedSize)
                serializer.Write(dwUnCommittedSize)
                serializer.Write(lpFirstBlock.ToInt32())
                serializer.Write(lpLastBlock.ToInt32())
            End Sub

            Public ReadOnly Property MinimumVersion As Integer Implements IFastSerializableVersion.MinimumVersion
                Get
                    Return SerializableObject.CurrentMinimumVersion
                End Get
            End Property

            Public ReadOnly Property Version As Integer Implements IFastSerializableVersion.Version
                Get
                    Return SerializableObject.CurrentSerializationVersion
                End Get
            End Property
        End Structure

        <StructLayoutAttribute(LayoutKind.Explicit)>
        Public Structure UNION_BLOCK
            Implements IFastSerializable, IFastSerializableVersion 'structs can't inherit from classes (SerializableObject), so we explicitly implement these

            <FieldOffset(0)>
            Public Block As STRUCT_BLOCK

            <FieldOffset(0)>
            Public Region As STRUCT_REGION

            Public Sub FromStream(ByVal deserializer As Deserializer, ByVal versionFromStream As Integer) Implements IFastSerializable.FromStream
                deserializer.Read(Block)
                'deserializer.Read(Region)
            End Sub

            Public Sub ToStream(ByVal serializer As Serializer) Implements IFastSerializable.ToStream
                serializer.Write(Block)
                'serializer.Write(Region)
            End Sub

            Public ReadOnly Property MinimumVersion As Integer Implements IFastSerializableVersion.MinimumVersion
                Get
                    Return SerializableObject.CurrentMinimumVersion
                End Get
            End Property

            Public ReadOnly Property Version As Integer Implements IFastSerializableVersion.Version
                Get
                    Return SerializableObject.CurrentSerializationVersion
                End Get
            End Property
        End Structure

        Public Enum ArenaBlkType ' must match blocktype in ArenaHandler
            Header = &H101
            Alloc = &H102
        End Enum

        <StructLayout(LayoutKind.Sequential, CharSet:=CharSet.Ansi)>
        Public Structure ArenaHeader
            Public blkType As ArenaBlkType
            Public dwUserData As IntPtr
            Public CntEverAlloc As Integer
            Public SizeEverAlloc As Integer
            Public CntCurLive As Integer
            Public SizeCurLive As Integer
            Public LastAllocation As IntPtr
            Public fDidHaveAFree As Boolean
            Public ArenaId As Integer
            Public ArenaAllocMap As IntPtr
            Public NameLen As Integer
            Public ArenaNamePtr As IntPtr
            Public ReadOnly Property ArenaName As String
                Get
                    Dim name = String.Empty
                    If NameLen > 0 Then
                        name = ReadProcessMemAsString(ArenaNamePtr, nMaxLen:=NameLen)
                    End If
                    Return name
                End Get
            End Property

            Public Overrides Function ToString() As String
                Return String.Format("ArenaID = {0} Name={1} User = {2} CntEverAlloc = {3} SizeEverAlloc = {4} CntCurLive = {5}  SizeCurLive = {6} LastAlloc = {7}",
                                     ArenaId,
                                     ArenaName,
                                     dwUserData,
                                     CntEverAlloc,
                                     SizeEverAlloc,
                                     CntCurLive,
                                     SizeCurLive,
                                     LastAllocation)
            End Function

            Public Shared Function FromProcMemIntPtr(ByVal pHdrInfo As IntPtr) As ArenaHeader
                Dim pProcMemIntPtr As New ProcMemIntPtr
                Dim arenaHdrSize = Marshal.SizeOf(GetType(ArenaHeader))
                If _ConnectionMode = MemSpectMode.Offline Then
                    pProcMemIntPtr.data = CType(Array.CreateInstance(GetType(IntPtr), arenaHdrSize), IntPtr())
                End If
                If ReadProcessMemoryDwordsEx(__hProcessTarget, pHdrInfo, pProcMemIntPtr, arenaHdrSize, 0) = 0 Then
                End If
                Dim ahdr = New ArenaHeader
                ahdr.blkType = CType(pProcMemIntPtr.data(0), ArenaBlkType)
                ahdr.dwUserData = pProcMemIntPtr.data(1)
                ahdr.CntEverAlloc = pProcMemIntPtr.data(2).ToInt32
                ahdr.SizeEverAlloc = pProcMemIntPtr.data(3).ToInt32
                ahdr.CntCurLive = CInt(pProcMemIntPtr.data(4))
                ahdr.SizeCurLive = CInt(pProcMemIntPtr.data(5))
                ahdr.LastAllocation = pProcMemIntPtr.data(6)
                ahdr.fDidHaveAFree = CBool(pProcMemIntPtr.data(7))
                ahdr.ArenaId = CInt(pProcMemIntPtr.data(8))
                ahdr.ArenaAllocMap = pProcMemIntPtr.data(9)
                ahdr.NameLen = CInt(pProcMemIntPtr.data(10))
                ahdr.ArenaNamePtr = pHdrInfo.MyAdd(11 * 4)
                Return ahdr
            End Function
        End Structure


        <StructLayout(LayoutKind.Sequential, CharSet:=CharSet.Auto)>
        Public Class MEMORYSTATUSEX

            ''' <summary>
            ''' Initializes a new instance of the <see cref="T:MEMORYSTATUSEX" /> class.
            ''' </summary>
            Public Sub New()
                Me.dwLength = CType(Marshal.SizeOf(GetType(MEMORYSTATUSEX)), UInt32)
            End Sub
            ' Fields
            ''' <summary>
            ''' Size of the structure, in bytes. You must set this member before calling GlobalMemoryStatusEx.
            ''' </summary>
            Public dwLength As UInt32
            ''' <summary>
            ''' Number between 0 and 100 that specifies the approximate percentage of physical memory that is in use (0 indicates no memory use and 100 indicates full memory use).
            ''' </summary>
            Public dwMemoryLoad As UInt32
            ''' <summary>
            ''' Total size of physical memory, in bytes.
            ''' </summary>
            Public ullTotalPhys As UInt64
            ''' <summary>
            ''' Size of physical memory available, in bytes.
            ''' </summary>
            Public ullAvailPhys As UInt64
            ''' <summary>
            ''' Size of the committed memory limit, in bytes. This is physical memory plus the size of the page file, minus a small overhead.
            ''' </summary>
            Public ullTotalPageFile As UInt64
            ''' <summary>
            ''' Size of available memory to commit, in bytes. The limit is ullTotalPageFile.
            ''' </summary>
            Public ullAvailPageFile As UInt64
            ''' <summary>
            ''' Total size of the user mode portion of the virtual address space of the calling process, in bytes.
            ''' </summary>
            Public ullTotalVirtual As UInt64
            ''' <summary>
            ''' Size of unreserved and uncommitted memory in the user mode portion of the virtual address space of the calling process, in bytes.
            ''' </summary>
            Public ullAvailVirtual As UInt64
            ''' <summary>
            ''' Size of unreserved and uncommitted memory in the extended portion of the virtual address space of the calling process, in bytes.
            ''' </summary>
            Public ullAvailExtendedVirtual As UInt64
            Public Overrides Function ToString() As String
                Dim str = String.Format(
                    "Load={0}% TotPhys={1:x16} AvlPhy={2:x16} TotPgFl={3:x16} AvlPgFl={4:x16} TotVirt={5:x16} AvlVirt={6:x16}",
                    dwMemoryLoad,
                    ullTotalPhys, ullAvailPhys,
                    ullTotalPageFile, ullAvailPageFile,
                    ullTotalVirtual, ullAvailVirtual
                    )
                Return str
            End Function
        End Class



        <StructLayout(LayoutKind.Sequential)>
        Public Structure PROCESS_HEAP_ENTRY
            Implements IFastSerializable, IFastSerializableVersion 'structs can't inherit from classes (SerializableObject), so we explicitly implement these

            Public lpData As IntPtr
            Public cbData As Integer
            Public cbOverhead As Byte
            Public iRegionIndex As Byte
            Public wFlags As PROCESS_HEAP_ENTRY_WFLAGS
            Public UnionBlock As UNION_BLOCK

            Public Sub FromStream(ByVal deserializer As Deserializer, ByVal versionFromStream As Integer) Implements IFastSerializable.FromStream
                lpData = CType(deserializer.ReadInt(), IntPtr)
                deserializer.Read(cbData)
                deserializer.Read(cbOverhead)
                deserializer.Read(iRegionIndex)

                Dim temp As Short = 0
                deserializer.Read(temp)
                wFlags = CType(temp, PROCESS_HEAP_ENTRY_WFLAGS)

                deserializer.Read(UnionBlock)

            End Sub

            Public Sub ToStream(ByVal serializer As Serializer) Implements IFastSerializable.ToStream
                serializer.Write(lpData.ToInt32())
                serializer.Write(cbData)
                serializer.Write(cbOverhead)
                serializer.Write(iRegionIndex)
                serializer.Write(CShort(wFlags))
                serializer.Write(UnionBlock)
            End Sub

            Public ReadOnly Property MinimumVersion As Integer Implements IFastSerializableVersion.MinimumVersion
                Get
                    Return SerializableObject.CurrentMinimumVersion
                End Get
            End Property

            Public ReadOnly Property Version As Integer Implements IFastSerializableVersion.Version
                Get
                    Return SerializableObject.CurrentSerializationVersion
                End Get
            End Property
        End Structure

        Public Enum BinaryType
            SCS_32BIT_BINARY = 0
            SCS_64BIT_BINARY = 6

        End Enum

        <DllImport("kernel32.dll", SetLastError:=True)>
        Public Function GetBinaryType(ByVal lpApplicationName As String,
                                      ByRef lpBinaryType As Integer
                                     ) As Boolean

        End Function


        <DllImport("kernel32.dll", SetLastError:=True, EntryPoint:="ReadProcessMemory")>
        Public Function ReadProcessMemory(
               ByVal hProcess As IntPtr,
               ByVal lpBaseAddress As IntPtr,
               ByRef lpBuffer As ProcMemBlock,
               ByVal dwByteSize As Integer,
               ByRef lpNumberOfBytesRead As Integer
         ) As Integer
        End Function

        <DllImport("kernel32.dll", SetLastError:=True, EntryPoint:="WriteProcessMemory")>
        Public Function WriteProcessMemory(
               ByVal hProcess As IntPtr,
               ByVal lpBaseAddress As IntPtr,
               ByVal lpBuffer As Byte(),
               ByVal dwByteSize As Integer,
               ByRef lpNumberOfBytesWritten As Integer
         ) As Integer
        End Function

        <DllImport("kernel32.dll", SetLastError:=True, EntryPoint:="ReadProcessMemory")>
        Public Function ReadProcessMemoryByte(
               ByVal hProcess As IntPtr,
               ByVal lpBaseAddress As IntPtr,
               ByRef lpBuffer As ProcMemBlockByte,
               ByVal dwByteSize As Integer,
               ByRef lpNumberOfBytesRead As Integer
         ) As Integer
        End Function

        <DllImport("kernel32.dll", SetLastError:=True, EntryPoint:="ReadProcessMemory")>
        Public Function ReadProcessMemoryHeapAllocationStruct(
               ByVal hProcess As IntPtr,
               ByVal lpBaseAddress As IntPtr,
               ByRef lpBuffer As HeapAllocationStruct,
               ByVal dwByteSize As Integer,
               ByRef lpNumberOfBytesRead As Integer
         ) As Integer
        End Function

        <DllImport("kernel32.dll", SetLastError:=True, EntryPoint:="ReadProcessMemory")>
        Public Function ReadProcessMemoryTrackBlockStruct(
               ByVal hProcess As IntPtr,
               ByVal lpBaseAddress As IntPtr,
               ByRef lpBuffer As TrackBlockStruct,
               ByVal dwByteSize As Integer,
               ByRef lpNumberOfBytesRead As Integer
         ) As Integer
        End Function


        <DllImport("kernel32.dll", SetLastError:=True, EntryPoint:="ReadProcessMemory")>
        Public Function ReadProcessMemoryDword(
               ByVal hProcess As IntPtr,
               ByVal lpBaseAddress As IntPtr,
               ByRef lpBuffer As Integer,
               ByVal dwByteSize As Integer,
               ByRef lpNumberOfBytesRead As Integer
         ) As Integer
        End Function

        <DllImport("kernel32.dll", SetLastError:=True, EntryPoint:="ReadProcessMemory")>
        Public Function ReadProcessMemoryDwords(
               ByVal hProcess As IntPtr,
               ByVal lpBaseAddress As IntPtr,
               ByRef lpBuffer As ProcMemIntPtr,
               ByVal dwByteSize As Integer,
               ByRef lpNumberOfBytesRead As Integer
         ) As Integer
        End Function

        <DllImport("kernel32.dll", SetLastError:=True, EntryPoint:="ReadProcessMemory")>
        Public Function ReadProcessMemoryMemMapStruct(
               ByVal hProcess As IntPtr,
               ByVal lpBaseAddress As IntPtr,
               ByRef lpBuffer As StackMemMapStats,
               ByVal dwByteSize As Integer,
               ByRef lpNumberOfBytesRead As Integer
         ) As Integer
        End Function

        <DllImport("kernel32.dll", SetLastError:=True, EntryPoint:="ReadProcessMemory")>
        Public Function ReadProcessMemoryGCStatStruct(
               ByVal hProcess As IntPtr,
               ByVal lpBaseAddress As IntPtr,
               ByRef lpBuffer As GCStats,
               ByVal dwByteSize As Integer,
               ByRef lpNumberOfBytesRead As Integer
         ) As Integer
        End Function

        <DllImport("kernel32.dll", SetLastError:=True, EntryPoint:="ReadProcessMemory")>
        Public Function ReadProcessMemorySYMBOL_INFO(
               ByVal hProcess As IntPtr,
               ByVal lpBaseAddress As IntPtr,
               ByRef lpBuffer As SYMBOL_INFO,
               ByVal dwByteSize As Integer,
               ByRef lpNumberOfBytesRead As Integer
         ) As Integer
        End Function



        <DllImport("psapi", SetLastError:=True)>
        Public Function GetModuleFileNameEx(ByVal hProcess As IntPtr, ByVal hModule As IntPtr, ByVal lpFileName As Text.StringBuilder, ByVal nSize As Integer) As Integer
        End Function

        <DllImport("kernel32", SetLastError:=True)>
        Public Function GetModuleFileName(ByVal hModule As IntPtr, ByVal lpFileName As Text.StringBuilder, ByVal nSize As Integer) As Integer

        End Function

        <DllImport("kernel32.dll", SetLastError:=True, ExactSpelling:=True)>
        Public Function VirtualAllocEx(ByVal hProcess As IntPtr, ByVal lpAddress As IntPtr,
             ByVal dwSize As UInteger, ByVal flAllocationType As AllocationType,
             ByVal flProtect As AllocationProtect) As IntPtr
        End Function

        <DllImport("kernel32.dll", SetLastError:=True, ExactSpelling:=True)>
        Public Function VirtualFree(ByVal lpAddress As IntPtr,
             ByVal dwSize As UInteger, ByVal dwFreeType As FreeType) As Boolean
        End Function

        <DllImport("kernel32.dll", SetLastError:=True, ExactSpelling:=True)>
        Public Function VirtualFreeEx(ByVal hProcess As IntPtr, ByVal lpAddress As IntPtr,
             ByVal dwSize As UInteger, ByVal dwFreeType As FreeType) As Boolean
        End Function

        <DllImport("kernel32", SetLastError:=True)>
        Public Function VirtualQueryEx(
                                    ByVal hProcess As IntPtr,
                                    ByVal lpAddress As IntPtr,
                                    ByRef mbi As MEMORY_BASIC_INFORMATION,
                                    ByVal dwLength As UInteger) As UInteger
        End Function

        'http://www.microsoft.com/msj/archive/S2058.aspx 
        <DllImport("psapi", SetLastError:=True)>
        Public Function QueryWorkingSet(
                                    ByVal hProcess As IntPtr,
                                    ByVal pv As IntPtr,
                                    ByVal dwLength As Integer) As UInteger
        End Function
        Public Const ERROR_BAD_LENGTH = 24

        <Flags()>
        Public Enum WorkingSetFlags
            NotAccessed
            [ReadOnly]
            Executable  '2
            ExecutableReadOnly '3
            ReadWrite
            CopyOnWrite
            ExecutableReadWrite ' 6
            ExecutableCopyOnWrite
            NotAccessed2 '8
            MAXVAL
        End Enum


        <Flags>
        Public Enum GCRootFlags
            Pinning = 1
            WeakRef = 2
            Interior = 4
            RefCounted = 8
        End Enum

        Public Enum GCRootKind
            Other = 0
            Stack = 1
            Finalizer = 2
            Handle = 3
        End Enum

        <DllImport("user32.dll", SetLastError:=True, CharSet:=CharSet.Auto)>
        Public Function SendMessage(ByVal hWnd As IntPtr, ByVal Msg As UInteger, ByVal wParam As IntPtr, ByVal lParam As IntPtr) As IntPtr
        End Function

        <DllImport("user32.dll", SetLastError:=True, CharSet:=CharSet.Auto)>
        Public Function ShowWindow(ByVal hwnd As IntPtr, ByVal nCmdShow As Int32) As Boolean
        End Function
        Public Const SW_HIDE As Integer = 0
        Public Const SW_MAXIMIZE As Integer = 3
        Public Const SW_MINIMIZE As Integer = 6
        Public Const SW_RESTORE As Integer = 9

        <DllImport(MemSpectDllName, EntryPoint:="_VsDebGetHeapStats@24")>
        Public Function VsDebGetHeapStats(
            <[In]()> ByVal nHeap As IntPtr,
            <[Out]()> ByRef nCurNumAllocs As Integer,
            <[Out]()> ByRef nCurBytes As Integer,
            <[Out]()> ByRef nTotNumAllocs As Integer,
            <[Out]()> ByRef nTotBytes As UInteger,
            <[Out]()> ByRef pnGlobalPassCount As Integer
        ) As UInteger
        End Function

        ' To minimize the # of elems marsheled, we'll use the SizeParamIndex. 
        ' However, that's an elem count, not a byte count that ReadProcessMemory uses
        <DllImport(MemSpectDllName, EntryPoint:="_ReadProcessMemoryHelper@20")>
        Public Function ReadProcessMemoryAsIntPtrArray(
                                                  ByVal hProcess As IntPtr,
                                                  ByVal dwAddress As IntPtr,
                                                  ByVal nNumElems As Integer,
                                                  ByVal nElemSize As Integer,
                                                  <MarshalAs(UnmanagedType.LPArray, SizeParamIndex:=2)> ByVal lpBuffer As IntPtr()
                                                  ) As Integer
        End Function

        <DllImport(MemSpectDllName, EntryPoint:="_ReadProcessMemoryHelper@20")>
        Public Function ReadProcessMemoryAsByteArray(
                                                  ByVal hProcess As IntPtr,
                                                  ByVal dwAddress As IntPtr,
                                                  ByVal nNumElems As Integer,
                                                  ByVal nElemSize As Integer,
                                                  <MarshalAs(UnmanagedType.LPArray, SizeParamIndex:=2)> ByVal lpBuffer As Byte()
                                                  ) As Integer
        End Function


        <DllImport(MemSpectDllName, EntryPoint:="_VsResolveSymbols@12")>
        Public Function VsResolveSymbols(
                                        ByVal dwAddress As IntPtr,
                                        ByVal pszBuf As Text.StringBuilder,
                                        ByVal uicBuf As Integer) As Boolean
        End Function

        <DllImport(MemSpectDllName, EntryPoint:="_VsAssertWriteMiniDumpEx@16", SetLastError:=True)>
        Public Function VsAssertWriteMiniDumpEx(
                                        ByVal hProcess As IntPtr,
                                        ByVal processID As Integer,
                                        ByVal szMiniDumpFullFileName As Text.StringBuilder,
                                        ByVal dumpType As Integer) As Boolean
        End Function

        <DllImport(MemSpectDllName, EntryPoint:="_CRC32@16")>
        Public Function CRC32(
                            ByVal crcStart As UInteger,
                            ByVal pszBuf As IntPtr,
                            ByVal bufOffset As Integer,
                            ByVal len As Integer
                            ) As UInteger
        End Function

        <DllImport(MemSpectDllName, SetLastError:=True, CharSet:=CharSet.Unicode)>
        Public Function GetPackageNamesFromManifest(
                            ByVal packagePath As String,
                            nAppIndex As Integer,
                            <MarshalAs(UnmanagedType.LPWStr)> ByRef PackageName As String,
                            <MarshalAs(UnmanagedType.LPWStr)> ByRef PackageFullName As String,
                            <MarshalAs(UnmanagedType.LPWStr)> ByRef PackageFamilyName As String,
                            <MarshalAs(UnmanagedType.LPWStr)> ByRef AppUserModelId As String,
                            AppContainerNamedObjectPath As Text.StringBuilder
                            ) As Integer
        End Function

        <DllImport(MemSpectDllName, SetLastError:=True, CharSet:=CharSet.Unicode)>
        Public Function GetPackageFamilyNameFromProcess(
                            ByVal hProcess As IntPtr,
                            PackageFamilyName As Text.StringBuilder
                            ) As Integer
        End Function

        <DllImport(MemSpectDllName, SetLastError:=True, CharSet:=CharSet.Unicode)>
        Public Function GetPackageFullNameFromProcess(
                            ByVal hProcess As IntPtr,
                            PackageFullName As Text.StringBuilder
                            ) As Integer
        End Function


        <DllImport(MemSpectDllName, SetLastError:=True, CharSet:=CharSet.Unicode)>
        Public Function GetPathOfAppContainerNamedObject(
                            ByVal packageFamilyName As String,
                            AppContainerNamedObjectPath As Text.StringBuilder
                            ) As Integer
        End Function

        <DllImport(MemSpectDllName, SetLastError:=True, CharSet:=CharSet.Unicode)>
        Public Function PackageDebuggingEnabler(
                            ByVal pPackageFullName As String,
                            strCmdLine As String,
                            strEnvVars As String,
                            nEnable As Integer
                            ) As Integer
        End Function


        <DllImport(MemSpectDllName, SetLastError:=True, CharSet:=CharSet.Unicode)>
        Public Function PackageLauncher(
                            ByVal AppUserModelId As String,
                            ByRef ProcessId As Integer
                            ) As Integer
        End Function

        <DllImport(MemSpectDllName, SetLastError:=True, CharSet:=CharSet.Unicode)>
        Public Function PackageResumeSuspend(
                            ByVal PackageFullName As String,
                            ByVal nResume As Integer
                            ) As Integer
        End Function


        <DllImport("dbghelp.dll", SetLastError:=True)>
        Public Function SymEnumerateModules64(
                <[In]()> ByVal hProcess As IntPtr,
                <[In]()> ByVal SymEnumerateModules64CallBack As SymEnumerateModulesProc64,
                <[In]()> ByVal UserContext As IntPtr
              ) As Boolean
        End Function

        Public Delegate Function SymEnumerateModulesProc64(
                <[In]()> ByVal ModuleName As String,
                <[In]()> ByVal BaseOfDll As Int64,
                <[In]()> ByVal UserContext As IntPtr
              ) As Boolean

        <DllImport("dbghelp.dll", SetLastError:=True)>
        Public Function SymEnumSymbols(
                <[In]()> ByVal hProcess As IntPtr,
                <[In]()> ByVal BaseOfDll As Int64,
                <[In]()> ByVal Mask As String,
                <[In]()> ByVal Callback As SymEnumSymbolsProc,
                <[In]()> ByVal UserContext As IntPtr
              ) As Boolean
            'mask="*!*" does all syms in all modules (baseofdll = 0)
        End Function

        Public Delegate Function SymEnumSymbolsProc(
            <[In]()> ByVal pSymInfo As IntPtr,
            <[In]()> ByVal SymSize As Integer,
            <[In]()> ByVal UserContext As IntPtr
          ) As Boolean

        <StructLayout(LayoutKind.Sequential)>
        Public Structure SYMBOL_INFO
            Public SizeOfStruct As Integer
            Public TypeIndex As Integer
            Public Reserved1 As Int64
            Public Reserved2 As Int64
            Public Index As Integer
            Public Size As Integer
            Public ModBase As Int64
            Public Flags As SYMFlags
            Public Value As Int64
            Public Address As Int64
            Public Register As Integer
            Public Scope As Integer
            Public Tag As SYMTag
            Public NameLen As Integer
            Public MaxNameLen As Integer
            'public   Name[1]
        End Structure


        <Flags()>
        Public Enum SYMFlags
            VALUEPRESENT = &H1
            REGISTER = &H8
            REGREL = &H10
            FRAMEREL = &H20
            PARAMETER = &H40
            LOCAL = &H80
            CONSTANT = &H100
            EXPORT = &H200
            FORWARDER = &H400
            [FUNCTION] = &H800
            VIRTUAL = &H1000
            THUNK = &H2000
            TLSREL = &H4000
            SLOT = &H8000
            ILREL = &H10000
            METADATA = &H20000
            CLR_TOKEN = &H40000
            NULL = &H80000
            '// this resets SymNext/Prev to the beginning
            '// of the module passed in the address field

            RESET = &H80000000


        End Enum

        Public Enum SYMTag
            TagNull
            TagExe
            TagCompiland
            TagCompilandDetails
            TagCompilandEnv
            TagFunction
            TagBlock
            TagData
            TagAnnotation
            TagLabel
            TagPublicSymbol
            TagUDT
            TagEnum
            TagFunctionType
            TagPointerType
            TagArrayType
            TagBaseType
            TagTypedef
            TagBaseClass
            TagFriend
            TagFunctionArgType
            TagFuncDebugStart
            TagFuncDebugEnd
            TagUsingNamespace
            TagVTableShape
            TagVTable
            TagCustom
            TagThunk
            TagCustomType
            TagManagedType
            TagDimension
        End Enum

        <DllImport("dbghelp.dll", SetLastError:=True)>
        Public Function SymLoadModuleEx(
                <[In]()> ByVal hProcess As IntPtr,
                <[In]()> ByVal hFile As IntPtr,
                <[In]()> ByVal ImageName As String,
                <[In]()> ByVal ModuleName As String,
                <[In]()> ByVal BaseOfDll As Int64,
                <[In]()> ByVal DllSize As Integer,
                <[In]()> ByVal Data As IntPtr,
                <[In]()> ByVal Flags As Integer
              ) As Int64
        End Function

        <DllImport("dbghelp.dll", SetLastError:=True)>
        Public Function SymUnloadModule64(
                <[In]()> ByVal hProcess As IntPtr,
                <[In]()> ByVal BaseOfDll As Int64
              ) As Boolean
        End Function

        <DllImport("dbghelp.dll", SetLastError:=True)>
        Public Function SymGetModuleInfo64(
                <[In]()> ByVal hProcess As IntPtr,
                <[In]()> ByVal dwAddr As Int64,
                ByRef ModuleInfo As IMAGEHLP_MODULE64
              ) As Boolean
        End Function

        <StructLayout(LayoutKind.Sequential)>
        Public Structure IMAGEHLP_MODULE64
            Dim SizeOfStruct As Integer
            Dim BaseOfImage As Int64
            Dim ImageSize As Integer
            Dim TimeDateStamp As Integer
            Dim CheckSum As Integer
            Dim NumSyms As Integer
            Dim SymType As SymType
            <MarshalAs(UnmanagedType.ByValTStr, SizeConst:=32)>
            Dim ModuleName As String
            <MarshalAs(UnmanagedType.ByValTStr, SizeConst:=256)>
            Dim ImageName As String
            <MarshalAs(UnmanagedType.ByValTStr, SizeConst:=256)>
            Dim LoadedImageName As String
            <MarshalAs(UnmanagedType.ByValTStr, SizeConst:=256)>
            Dim LoadedPDBName As String
            Dim CVSig As Integer
            <MarshalAs(UnmanagedType.ByValTStr, SizeConst:=MAX_PATH * 3)>
            Dim CVData As String
            Dim PdbSig As Integer
            Dim PdbSig70 As Guid
            Dim PdbAge As Integer
            Dim PdbUnmatched As Boolean
            Dim DbgUnmatched As Boolean
            Dim LineNumbers As Boolean
            Dim GlobalSymbols As Boolean
            Dim TypeInfo As SymType
            Dim SourceIndexed As Boolean
            Dim Publics As Boolean
            Dim MachineType As Integer
            Dim Reserved As Integer
        End Structure

        Public Enum SymType
            SymNone = 0
            SymCoff
            SymCv
            SymPdb
            SymExport
            SymDeferred
            SymSym
            SymDia
            NumSymTypes
        End Enum

        <DllImport("dbghelp.dll", SetLastError:=True)>
        Public Function SymSetSearchPath(
                <[In]()> ByVal hProcess As IntPtr,
                <[In]()> ByVal SearchPath As String
                ) As Boolean
        End Function
        <Flags()>
        Public Enum SymOptions
            SYMOPT_CASE_INSENSITIVE = &H1
            SYMOPT_UNDNAME = &H2
            SYMOPT_DEFERRED_LOADS = &H4
            SYMOPT_NO_CPP = &H8
            SYMOPT_LOAD_LINES = &H10
            SYMOPT_OMAP_FIND_NEAREST = &H20
            SYMOPT_LOAD_ANYTHING = &H40
            SYMOPT_IGNORE_CVREC = &H80
            SYMOPT_NO_UNQUALIFIED_LOADS = &H100
            SYMOPT_FAIL_CRITICAL_ERRORS = &H200
            SYMOPT_EXACT_SYMBOLS = &H400
            SYMOPT_ALLOW_ABSOLUTE_SYMBOLS = &H800
            SYMOPT_IGNORE_NT_SYMPATH = &H1000
            SYMOPT_INCLUDE_32BIT_MODULES = &H2000
            SYMOPT_PUBLICS_ONLY = &H4000
            SYMOPT_NO_PUBLICS = &H8000
            SYMOPT_AUTO_PUBLICS = &H10000
            SYMOPT_NO_IMAGE_SEARCH = &H20000
            SYMOPT_SECURE = &H40000
            SYMOPT_NO_PROMPTS = &H80000
            SYMOPT_OVERWRITE = &H100000
            SYMOPT_IGNORE_IMAGEDIR = &H200000
            SYMOPT_FLAT_DIRECTORY = &H400000
            SYMOPT_FAVOR_COMPRESSED = &H800000
            SYMOPT_ALLOW_ZERO_ADDRESS = &H1000000
            SYMOPT_DISABLE_SYMSRV_AUTODETECT = &H2000000

            SYMOPT_DEBUG = &H80000000

        End Enum

        <DllImport("dbghelp.dll", SetLastError:=True)>
        Public Function SymGetSearchPath(
                    ByVal hProcess As IntPtr,
                    ByVal SearchPath As StringBuilder,
                    ByVal CChh As Integer
                ) As Boolean
        End Function

        <DllImport("dbghelp.dll", SetLastError:=True)>
        Public Function SymGetOptions(
                ) As SymOptions
        End Function


        <DllImport("dbghelp.dll", SetLastError:=True)>
        Public Function UnDecorateSymbolName(
                ByVal DecoratedName As String,
                ByVal UnDecoratedName As Text.StringBuilder,
                ByVal dwLength As Integer,
                ByVal dwFlags As Integer
              ) As Integer

        End Function


        <StructLayout(LayoutKind.Sequential)>
        Public Structure IMAGEHLP_DEFERRED_SYMBOL_LOAD64
            Dim SizeOfStruct As Integer
            Dim BaseOfImage As Int64
            Dim CheckSum As Integer
            Dim TimeDateStamp As Integer
            <MarshalAs(UnmanagedType.ByValTStr, SizeConst:=MAX_PATH)>
            Dim FileName As String
            Dim Reparse As Boolean
            Dim hFile As IntPtr
            Dim flags As Integer
        End Structure

        <StructLayout(LayoutKind.Sequential)>
        Public Structure IMAGEHLP_CBA_EVENT
            Dim Severity As Integer
            Dim code As Integer
            Dim desc As IntPtr
            Dim obj As IntPtr
        End Structure


        <Flags()>
        Public Enum SymActionCode
            CBA_DEFERRED_SYMBOL_LOAD_START = &H1
            CBA_DEFERRED_SYMBOL_LOAD_COMPLETE = &H2
            CBA_DEFERRED_SYMBOL_LOAD_FAILURE = &H3
            CBA_SYMBOLS_UNLOADED = &H4
            CBA_DUPLICATE_SYMBOL = &H5
            CBA_READ_MEMORY = &H6
            CBA_DEFERRED_SYMBOL_LOAD_CANCEL = &H7
            CBA_SET_OPTIONS = &H8
            CBA_EVENT = &H10
            CBA_DEFERRED_SYMBOL_LOAD_PARTIAL = &H20
            CBA_DEBUG_INFO = &H10000000
            CBA_SRCSRV_INFO = &H20000000
            CBA_SRCSRV_EVENT = &H40000000
        End Enum

        Public Delegate Function SymRegisterCallbackProc64(
                <[In]()> ByVal hProcess As IntPtr,
                <[In]()> ByVal ActionCode As SymActionCode,
                <[In]()> ByVal CallbackData As Int64,
                <[In]()> ByVal UserContext As Int64
              ) As Boolean

        <DllImport("dbghelp.dll", SetLastError:=True)>
        Public Function SymRegisterCallback64(
                    ByVal hProcess As IntPtr,
                    ByVal CallbackFunction As SymRegisterCallbackProc64,
                    ByVal UserContext As Int64
              ) As Integer

        End Function

        <Flags()>
        Public Enum _MINIDUMP_TYPE
            MiniDumpNormal = &H0
            MiniDumpWithDataSegs = &H1
            MiniDumpWithFullMemory = &H2
            MiniDumpWithHandleData = &H4
            MiniDumpFilterMemory = &H8
            MiniDumpScanMemory = &H10
            MiniDumpWithUnloadedModules = &H20
            MiniDumpWithIndirectlyReferencedMemory = &H40
            MiniDumpFilterModulePaths = &H80
            MiniDumpWithProcessThreadData = &H100
            MiniDumpWithPrivateReadWriteMemory = &H200
            MiniDumpWithoutOptionalData = &H400
            MiniDumpWithFullMemoryInfo = &H800
            MiniDumpWithThreadInfo = &H1000
            MiniDumpWithCodeSegs = &H2000
            MiniDumpWithoutAuxiliaryState = &H4000
            MiniDumpWithFullAuxiliaryState = &H8000
            MiniDumpWithPrivateWriteCopyMemory = &H10000
            MiniDumpIgnoreInaccessibleMemory = &H20000
            MiniDumpWithTokenInformation = &H40000
        End Enum


        Public Enum MINIDUMP_STREAM_TYPE
            UnusedStream = 0
            ReservedStream0 = 1
            ReservedStream1 = 2
            ThreadListStream = 3
            ModuleListStream = 4
            MemoryListStream = 5
            ExceptionStream = 6
            SystemInfoStream = 7
            ThreadExListStream = 8
            Memory64ListStream = 9
            CommentStreamA = 10
            CommentStreamW = 11
            HandleDataStream = 12
            FunctionTableStream = 13
            UnloadedModuleListStream = 14
            MiscInfoStream = 15
            MemoryInfoListStream = 16 '  like VirtualQuery
            ThreadInfoListStream = 17
            HandleOperationListStream = 18
            LastReservedStream = &HFFFF
        End Enum

        <DllImport("dbghelp.dll", SetLastError:=True)>
        Public Function MiniDumpReadDumpStream(
                        ByVal BaseOfDump As IntPtr,
                        ByVal StreamNumber As MINIDUMP_STREAM_TYPE,
                        ByRef DirPtr As IntPtr,
                        ByRef StreamPointer As IntPtr,
                        ByRef StreamSize As UInteger
                ) As Boolean

        End Function

        <DllImport("dbghelp.dll", SetLastError:=True)>
        Public Function SymSetHomeDirectory(
                ByVal hProcess As IntPtr,
                ByVal dir As String
              ) As Integer

        End Function


        <StructLayout(LayoutKind.Sequential)>
        <DebuggerDisplay("{ToString()}")>
        Public Structure MINIDUMP_LOCATION_DESCRIPTOR
            Dim DataSize As UInteger
            Dim Rva As UInteger ' relative byte offset
            Public Overrides Function ToString() As String
                Return String.Format("Off={0:x8} Sz={1:x8}", Rva, DataSize)
            End Function
        End Structure

        <StructLayout(LayoutKind.Sequential)>
        <DebuggerDisplay("{ToString()}")>
        Public Structure MINIDUMP_DIRECTORY
            Dim StreamType As MINIDUMP_STREAM_TYPE
            Dim Location As MINIDUMP_LOCATION_DESCRIPTOR
            Public Overrides Function ToString() As String
                Return String.Format("{0} {1}", StreamType, Location)
            End Function
        End Structure

        Public Enum ProcessorArchitecture As UShort ' : ushort
            PROCESSOR_ARCHITECTURE_INTEL = 0
            PROCESSOR_ARCHITECTURE_MIPS = 1
            PROCESSOR_ARCHITECTURE_ALPHA = 2
            PROCESSOR_ARCHITECTURE_PPC = 3
            PROCESSOR_ARCHITECTURE_SHX = 4
            PROCESSOR_ARCHITECTURE_ARM = 5
            PROCESSOR_ARCHITECTURE_IA64 = 6
            PROCESSOR_ARCHITECTURE_ALPHA64 = 7
            PROCESSOR_ARCHITECTURE_MSIL = 8
            PROCESSOR_ARCHITECTURE_AMD64 = 9
            PROCESSOR_ARCHITECTURE_IA32_ON_WIN64 = 10
        End Enum

        <StructLayout(LayoutKind.Sequential)>
        Public Structure MINIDUMP_SYSTEM_INFO
            Dim processorArchitecture As ProcessorArchitecture
            Dim processorLevel As UShort
            Dim processorRevision As UShort
            Dim NumberOfProcessors As Byte
            Dim ProductType As Byte
            Dim MajorVersion As UInteger
            Dim MinorVersion As UInteger
            Dim BuildNumber As UInteger
            'This enum is the same value as System.PlatformId.
            Dim PlatformId As System.PlatformID
            '// RVA to a CSDVersion string in the string table.
            '// This would be a string like "Service Pack 1".
            Dim CSDVersionRva As UInteger
            'Remaining fields are not imported.
            Public Overrides Function ToString() As String
                Return String.Format("Arch = {0} Proclevel = {1} ProcRev = {2} NumProc = {3} ProdType={4} VerMaj = {5} VerMin = {6}, BuildNum= {7}, Platid = {8}",
                                     processorArchitecture.ToString,
                                     processorLevel,
                                     processorRevision,
                                     NumberOfProcessors,
                                     ProductType,
                                     MajorVersion,
                                     MinorVersion,
                                     BuildNumber,
                                     PlatformId
                                     )
            End Function
        End Structure



        <StructLayout(LayoutKind.Sequential)>
        Public Structure MINIDUMP_MEMORY_DESCRIPTOR
            Dim StartOfMemoryRange As Int64 ' 64
            Dim MemoryLocDesc As MINIDUMP_LOCATION_DESCRIPTOR
        End Structure

        <StructLayout(LayoutKind.Sequential)>
        Public Structure MINIDUMP_MEMORY_DESCRIPTOR64
            Dim StartOfMemoryRange As Int64
            Dim DataSize As ULong
            'MINIDUMP_MEMORY_DESCRIPTOR64 is used for full-memory minidumps where all of the raw memory is sequential 
            '   at the end of the minidump. There is no need for individual relative virtual addresses (RVAs), 
            '   because the RVA is the base RVA plus the sum of the preceding data blocks
        End Structure

        <StructLayout(LayoutKind.Sequential)>
        Public Structure MINIDUMP_MEMORY_LIST
            Dim NumberOfMemoryRanges As UInteger
            'array of MINIDUMP_MEMORY_DESCRIPTOR
        End Structure

        <StructLayout(LayoutKind.Sequential)>
        Public Structure MINIDUMP_MEMORY64_LIST
            Dim NumberOfMemoryRanges As ULong
            Dim BaseRva As Int64 'Note that BaseRva is the overall base RVA for the memory list. 
            'To locate the data for a particular descriptor, start at BaseRva and increment 
            '   by the size of a descriptor until you reach the descriptor.
            'MINIDUMP_MEMORY_DESCRIPTOR64 MemoryRanges [0];
        End Structure


        <StructLayout(LayoutKind.Sequential)>
        Public Structure MINIDUMP_MEMORY_INFO_LIST
            Dim SizeOfHeader As UInteger
            Dim SizeOfEntry As UInteger
            Dim NumberOfEntries As ULong
        End Structure

        <StructLayout(LayoutKind.Sequential)>
        Public Structure MINIDUMP_MEMORY_INFO
            Dim BaseAddress As Long
            Dim AllocationBase As Long
            Dim AllocationProtect As UInteger
            Dim __alignment1 As UInteger
            Dim RegionSize As Long
            Dim State As UInteger
            Dim Protect As UInteger
            Dim Type As UInteger
            Dim __alignment2 As UInteger
        End Structure

        <StructLayout(LayoutKind.Sequential)>
        Public Structure MINIDUMP_MODULE_LIST
            Dim NumberOfModules As UInteger
            'MINIDUMP_MODULE Modules[];
        End Structure

        <StructLayout(LayoutKind.Sequential)>
        Public Structure MINIDUMP_MODULE
            Dim BaseOfImage As Long
            Dim SizeOfImage As UInteger
            Dim CheckSum As UInteger
            Dim TimeDateStamp As UInteger
            Dim ModuleNameRva As UInteger
            Dim VersionInfo As VS_FIXEDFILEINFO
            Dim CvRecord As MINIDUMP_LOCATION_DESCRIPTOR
            Dim MiscRecord As MINIDUMP_LOCATION_DESCRIPTOR
            Dim Reserved0 As Long
            Dim Reserved1 As Long
        End Structure

        <StructLayout(LayoutKind.Sequential)>
        Public Structure MINIDUMP_THREAD_LIST
            Dim NumberOfThreads As Integer
            '  MINIDUMP_THREAD Threads[];
        End Structure

        <StructLayout(LayoutKind.Sequential)>
        Public Structure MINIDUMP_THREAD
            Dim ThreadId As Integer
            Dim SuspendCount As Integer
            Dim PriorityClass As Integer
            Dim Priority As Integer
            Dim Teb As Long
            Dim Stack As MINIDUMP_MEMORY_DESCRIPTOR
            Dim ThreadContext As MINIDUMP_LOCATION_DESCRIPTOR
        End Structure

        <StructLayout(LayoutKind.Sequential)>
        Public Structure MINIDUMP_HANDLE_DATA_STREAM
            Dim SizeOfHeader As Integer
            Dim SizeOfDescriptor As Integer
            Dim NumberOfDescriptors As Integer
            Dim Reserved As Integer
        End Structure

        <StructLayout(LayoutKind.Sequential)>
        Public Structure MINIDUMP_HANDLE_DESCRIPTOR
            Dim Handle As Long
            Dim TypeNameRva As UInteger ' obj type
            Dim ObjectNameRva As UInteger ' obj name
            Dim Attributes As Integer
            Dim GrantedAccess As Integer
            Dim HandleCount As Integer
            Dim PointerCount As Integer
        End Structure

        <StructLayout(LayoutKind.Sequential)>
        Public Structure MINIDUMP_HANDLE_DESCRIPTOR_2
            Dim Handle As Long
            Dim TypeNameRva As UInteger
            Dim ObjectNameRva As UInteger
            Dim Attributes As Integer
            Dim GrantedAccess As Integer
            Dim HandleCount As Integer
            Dim PointerCount As Integer
            Dim ObjectInfoRva As Integer
            Dim Reserved0 As Integer
        End Structure

        <StructLayout(LayoutKind.Sequential)>
        Public Structure VS_FIXEDFILEINFO
            Dim dwSignature As UInteger
            Dim dwStrucVersion As UInteger
            Dim dwFileVersionMS As UInteger
            Dim dwFileVersionLS As UInteger
            Dim dwProductVersionMS As UInteger
            Dim dwProductVersionLS As UInteger
            Dim dwFileFlagsMask As UInteger
            Dim dwFileFlags As UInteger
            Dim dwFileOS As UInteger
            Dim dwFileType As UInteger
            Dim dwFileSubtype As UInteger
            Dim dwFileDateMS As UInteger
            Dim dwFileDateLS As UInteger
        End Structure



        <DllImport(MemSpectDllName, EntryPoint:="_VsDebIsValidHeap@4")>
        Public Function VsDebIsValidHeap(ByVal nHeap As IntPtr) As Integer
        End Function

        <DllImport(MemSpectDllName, EntryPoint:="_VsDebGetNextHeap@16")>
        Public Function VsDebGetNextHeap(
            <[In]()> ByVal nHeap As IntPtr,
            <[Out]()> ByRef pszHeapName As IntPtr,
            <[Out]()> ByRef pszFileName As IntPtr,
            <[Out]()> ByRef nLineNo As Integer
            ) As IntPtr
        End Function

        <DllImport(MemSpectDllName, EntryPoint:="_VsDebGetFirstBlock@4")>
        Public Function VsDebGetFirstBlock(ByVal nHeap As IntPtr) As IntPtr
        End Function


        <DllImport(MemSpectDllName, EntryPoint:="_VsDebGetNextBlock@4")>
        Public Function VsDebGetNextBlock(ByVal nHeap As IntPtr) As IntPtr
        End Function

        <DllImport(MemSpectDllName, EntryPoint:="_VsResolveSymbolEx@20")>
        Public Function VsResolveSymbolEx(
                                          ByVal hProcess As IntPtr,
                                          ByVal dwAddress As IntPtr,
                                          ByVal buf As StringBuilder,
                                          ByVal bufSize As Integer,
                                          <MarshalAs(UnmanagedType.Bool)> ByVal fNoFileLineInfo As Boolean
                                          ) As Integer
        End Function

        <DllImport(MemSpectDllName, EntryPoint:="_VsSetSymbolCallbackFunction@4")>
        Public Function VsSetSymbolCallbackFunction(ByVal callback As SymRegisterCallbackProc64
                                          ) As Boolean
        End Function


        <DllImport(MemSpectDllName, EntryPoint:="_VsClearSymbols@0")>
        Public Function VsClearSymbols() As IntPtr
        End Function

        <DllImport(MemSpectDllName, EntryPoint:="_VsSymGetSourceFile@20")>
        Public Function VsSymGetSourceFile(
                                          ByVal hProcess As IntPtr,
                                          ByVal dwAddress As IntPtr,
                                          ByVal filename As String,
                                          ByVal fullSourcePath As StringBuilder,
                                          ByVal bufSize As Integer
                                          ) As Boolean
        End Function


        <DllImport(MemSpectDllName,
                        CharSet:=CharSet.Ansi,
            EntryPoint:="_GetCodeMarkerNameFromId@8")>
        Public Sub GetCodeMarkerNameFromId(
                                                id As Integer,
                                                ByRef name As IntPtr
                                               )

        End Sub

        <DllImport(MemSpectDllName,
                        CharSet:=CharSet.Ansi,
            EntryPoint:="_GetCodeMarkerIdFromName@4")>
        Public Function GetCodeMarkerIdFromName(
                                                name As String
                                               ) As Integer

        End Function


        <DllImport(MemSpectDllName)>
        Public Function VsLoadModuleInfo(ByVal hProcess As IntPtr,
                                ByVal pszModuleName As String,
                                ByVal dwBaseAddress As IntPtr,
                                ByVal nModuleSize As Integer) As Boolean

        End Function



        <StructLayout(LayoutKind.Sequential)>
        Structure MEMORY_BASIC_INFORMATION
            Dim BaseAddress As IntPtr
            Dim AllocationBase As IntPtr
            Dim AllocationProtect As AllocationProtect ' when initially allocated
            Dim RegionSize As UInteger
            Dim State As AllocationState
            Dim Protect As AllocationProtect ' current
            Dim lType As AllocationType
            Public ReadOnly Property EndAddress As IntPtr
                Get
                    Return BaseAddress.MyAdd(RegionSize - 1)
                End Get
            End Property
            Public Overrides Function ToString() As String
                Return String.Format("BaseAdress={0:x8} AllocationBase={1:x8} EndAddr={2:x8} Size= {3:x8}({3:n0}) Protect={4} State={5} Type={6}",
                                     BaseAddress.ToInt32,
                                     AllocationBase.ToInt32,
                                     EndAddress.ToInt32,
                                     RegionSize,
                                     Protect,
                                     State,
                                     lType
                                     )
            End Function
        End Structure

        <Flags()>
        Enum AllocationProtect As UInteger
            PAGE_EXECUTE = &H10
            PAGE_EXECUTE_READ = &H20
            PAGE_EXECUTE_READWRITE = &H40
            PAGE_EXECUTE_WRITECOPY = &H80
            PAGE_NOACCESS = &H1
            PAGE_READONLY = &H2
            PAGE_READWRITE = &H4
            PAGE_WRITECOPY = &H8
            PAGE_GUARD = &H100
            PAGE_NOCACHE = &H200
            PAGE_WRITECOMBINE = &H400
        End Enum

        <Flags()>
        Enum AllocationType As UInteger
            MEM_IMAGE = &H1000000
            MEM_MAPPED = &H40000
            MEM_PRIVATE = &H20000
            MEM_TOP_DOWN = &H100000 ' Allocates memory at the highest possible address. 
            MEM_WRITE_WATCH = &H200000
        End Enum

        <Flags()>
        Enum AllocationState As UInteger
            MEM_COMMIT = &H1000
            MEM_FREE = &H10000
            MEM_RESERVE = &H2000
            MEM_RESET = &H80000
            MEM_WRITE_WATCH = &H200000
            MEM_PHYSICAL = &H400000
            MEM_LARGE_PAGES = &H20000000
        End Enum

        <Flags()>
        Enum FreeType
            MEM_DECOMMIT = &H4000
            MEM_RELEASE = &H8000
        End Enum

        <DllImport("Kernel32.dll", SetLastError:=True, CallingConvention:=CallingConvention.Winapi)>
        Public Function IsWow64Process(
        ByVal hProcess As IntPtr,
        <MarshalAs(UnmanagedType.Bool)> ByRef wow64Process As Boolean) As <MarshalAs(UnmanagedType.Bool)> Boolean
        End Function

        <DllImport("Kernel32.dll", SetLastError:=True, CallingConvention:=CallingConvention.Winapi)>
        Public Function OpenProcess(
            ByVal dwDesiredAccess As Process_Flags,
            ByVal bInheritHandle As Integer,
            ByVal dwProcessId As Integer) As IntPtr
        End Function


        Public Const MOVEFILE_DELAY_UNTIL_REBOOT = 4
        <DllImport("kernel32",
              PreserveSig:=True,
              CharSet:=CharSet.Auto,
              EntryPoint:="MoveFileEx",
              BestFitMapping:=False,
              ThrowOnUnmappableChar:=True,
              SetLastError:=True)>
        Public Function MoveFileEx(
             ByVal lpExistingFileName As String,
             ByVal lpNewFileName As String,
             ByVal dwFlags As Integer) As <MarshalAs(UnmanagedType.Bool)> Boolean
        End Function



        <Flags()>
        Enum Process_Flags
            PROCESS_TERMINATE = &H1
            PROCESS_CREATE_THREAD = &H2
            PROCESS_SET_SESSIONID = &H4
            PROCESS_VM_OPERATION = &H8
            PROCESS_VM_READ = &H10
            PROCESS_VM_WRITE = &H20
            PROCESS_DUP_HANDLE = &H40
            PROCESS_CREATE_PROCESS = &H80
            PROCESS_SET_QUOTA = &H100
            PROCESS_SET_INFORMATION = &H200
            PROCESS_QUERY_INFORMATION = &H400
            PROCESS_SUSPEND_RESUME = &H800
            PROCESS_QUERY_LIMITED_INFORMATION = &H1000
        End Enum

        <DllImport("psapi.dll", SetLastError:=True)>
        Public Function GetMappedFileName(
                                                 ByVal hProcess As IntPtr,
                                                 ByVal lpv As IntPtr,
                                                 ByVal lpFileName As Text.StringBuilder,
                                                 ByVal nSize As Integer) As Integer

        End Function

        <DllImport("kernel32.dll", SetLastError:=False, CharSet:=CharSet.Auto)>
        Public Function GetPrivateProfileInt(
                         ByVal lpAppName As String,
                         ByVal lpKeyName As String,
                         ByVal lpDefault As Integer,
                         ByVal lpFileName As String) As Integer
        End Function

        <DllImport("kernel32.dll", SetLastError:=True, CharSet:=CharSet.Auto)>
        Public Function GetPrivateProfileString(
                         ByVal lpAppName As String,
                         ByVal lpKeyName As String,
                         ByVal lpDefault As String,
                         ByVal lpReturnedString As Text.StringBuilder,
                         ByVal nSize As Integer,
                         ByVal lpFileName As String) As Integer
        End Function

        <DllImport("kernel32.dll", SetLastError:=True, CharSet:=CharSet.Auto)>
        Public Function WritePrivateProfileString(
                         ByVal lpAppName As String,
                         ByVal lpKeyName As String,
                         ByVal lpString As String,
                         ByVal lpFileName As String) As Integer ' used for testing to alter ini file
        End Function


        Public Declare Function GetProcessHeaps Lib "kernel32" (ByVal NumberOfHeaps As Integer, ByVal ProcessHeaps As IntPtr()) As Integer

        <DllImport("kernel32.dll", CharSet:=CharSet.Ansi)>
        Public Function GetProcAddress(ByVal hProcess As IntPtr, ByVal lpProcName As String) As IntPtr

        End Function

        <DllImport("kernel32.dll", CharSet:=CharSet.Auto)>
        Public Function GetModuleHandle(ByVal lpModuleName As String) As IntPtr

        End Function

        <DllImport("kernel32.dll")>
        Public Function CreateRemoteThread(
                                          ByVal hProcess As IntPtr,
                                          ByVal lpSecurityAttributes As IntPtr,
                                          ByVal dwStackSize As UInteger,
                                          ByVal lpStartAddress As IntPtr,
                                          ByVal lpParameter As IntPtr,
                                          ByVal dwCreationFlags As UInteger,
                                          ByRef lpThreadId As IntPtr
                                          ) As IntPtr

        End Function

        'c:\Program Files (x86)\Microsoft SDKs\Windows\v7.0A\Include\WinUser.h
        Public Function IS_INTRESOURCE(ByVal lp As IntPtr) As Boolean
            Dim fIsIntres = False
            If lp.ToInt32 >= 0 AndAlso lp.ToInt32 < &H10000 Then
                fIsIntres = True
            End If
            Return fIsIntres
        End Function

        Public Enum ResTypes
            RT_NONE
            RT_CURSOR
            RT_BITMAP
            RT_ICON
            RT_MENU
            RT_DIALOG
            RT_STRING
            RT_FONTDIR
            RT_FONT
            RT_ACCELERATOR
            RT_RCDATA
            RT_MESSAGETABLE

            RT_GROUP_CURSOR = 12
            RT_GROUP_ICON = 14
            RT_VERSION = 16
            RT_DLGINCLUDE = 17
        End Enum


        'c:\Program Files (x86)\Microsoft SDKs\Windows\v7.0A\Include\WinUser.h
        Public Const IMAGE_BITMAP = 0
        Public Const IMAGE_ICON = 1
        Public Const IMAGE_CURSOR = 2

        Public Const IMAGE_ENHMETAFILE = 3

        Public Const LR_DEFAULTCOLOR = &H0
        Public Const LR_MONOCHROME = &H1
        Public Const LR_COLOR = &H2
        Public Const LR_COPYRETURNORG = &H4
        Public Const LR_COPYDELETEORG = &H8
        Public Const LR_LOADFROMFILE = &H10
        Public Const LR_LOADTRANSPARENT = &H20
        Public Const LR_DEFAULTSIZE = &H40
        Public Const LR_VGACOLOR = &H80
        Public Const LR_LOADMAP3DCOLORS = &H1000
        Public Const LR_CREATEDIBSECTION = &H2000
        Public Const LR_COPYFROMRESOURCE = &H4000
        Public Const LR_SHARED = &H8000

        Public Const DI_MASK = &H1
        Public Const DI_IMAGE = &H2
        Public Const DI_NORMAL = &H3
        Public Const DI_COMPAT = &H4
        Public Const DI_DEFAULTSIZE = &H8
        Public Const DI_NOMIRROR = &H10


        'C:\Program Files (x86)\Microsoft SDKs\Windows\v7.0A\include\WinBase.h
        Public Delegate Function ENUMRESTYPEPROC(ByVal hModule As IntPtr, ByVal lpType As IntPtr, ByVal lParam As IntPtr) As Boolean

        <DllImport("kernel32.dll",
            CallingConvention:=CallingConvention.Winapi,
            CharSet:=CharSet.Auto,
            PreserveSig:=True)>
        Public Function EnumResourceTypes(
                                <[In]()> ByVal hModule As IntPtr,
                                <[In]()> ByVal EnumResTypeProc As ENUMRESTYPEPROC,
                                <[In]()> ByVal lParam As IntPtr
                                ) As Integer

        End Function


        Public Const RESOURCE_ENUM_LN = (&H1)
        Public Const RESOURCE_ENUM_MUI = (&H2)
        Public Const RESOURCE_ENUM_MUI_SYSTEM = (&H4)
        Public Const RESOURCE_ENUM_VALIDATE = (&H8)
        Public Const RESOURCE_ENUM_MODULE_EXACT = (&H10)

        <DllImport("kernel32.dll",
            CallingConvention:=CallingConvention.Winapi,
            CharSet:=CharSet.Auto,
            PreserveSig:=True)>
        Public Function EnumResourceTypesEx(
                                <[In]()> ByVal hModule As IntPtr,
                                <[In]()> ByVal EnumResTypeProc As ENUMRESTYPEPROC,
                                <[In]()> ByVal lParam As IntPtr,
                                <[In]()> ByVal dwFlags As Integer,
                                <[In]()> ByVal LangId As Integer
                                ) As Integer

        End Function


        Public Delegate Function ENUMRESNAMEPROC(
                                                ByVal hModule As IntPtr,
                                                ByVal lpType As IntPtr,
                                                ByVal lpName As IntPtr,
                                                ByVal lParam As IntPtr
                                                ) As Boolean

        <DllImport("kernel32.dll",
            CallingConvention:=CallingConvention.Winapi,
            CharSet:=CharSet.Auto,
            PreserveSig:=True)>
        Public Function EnumResourceNames(
                                <[In]()> ByVal hModule As IntPtr,
                                <[In]()> ByVal lpType As IntPtr,
                                <[In]()> ByVal EnumResNamesProc As ENUMRESNAMEPROC,
                                <[In]()> ByVal lParam As IntPtr
                                ) As Integer

        End Function

        Public Delegate Function ENUMRESLANGSPROC(
                                                ByVal hModule As IntPtr,
                                                ByVal lpType As IntPtr,
                                                ByVal lpName As IntPtr,
                                                ByVal wLangId As UInt16,
                                                ByVal lParam As IntPtr
                                                ) As Boolean

        <DllImport("kernel32.dll",
            CallingConvention:=CallingConvention.Winapi,
            CharSet:=CharSet.Auto,
            PreserveSig:=True)>
        Public Function EnumResourceLanguages(
                                <[In]()> ByVal hModule As IntPtr,
                                <[In]()> ByVal lpType As IntPtr,
                                <[In]()> ByVal lpName As IntPtr,
                                <[In]()> ByVal EnumResLangsProc As ENUMRESLANGSPROC,
                                <[In]()> ByVal lParam As IntPtr
                                ) As Integer

        End Function

        <DllImport("kernel32.dll",
            CallingConvention:=CallingConvention.Winapi,
            CharSet:=CharSet.Auto,
            PreserveSig:=True)>
        Public Function FindResourceEx(
                                <[In]()> ByVal hModule As IntPtr,
                                <[In]()> ByVal lpType As IntPtr,
                                <[In]()> ByVal lpName As IntPtr,
                                <[In]()> ByVal wLang As UInt16
                                ) As IntPtr

        End Function

        <DllImport("kernel32.dll",
            CallingConvention:=CallingConvention.Winapi,
            CharSet:=CharSet.Auto,
            PreserveSig:=True)>
        Public Function LoadResource(
                                <[In]()> ByVal hModule As IntPtr,
                                <[In]()> ByVal hResInfo As IntPtr
                                ) As IntPtr

        End Function


        <DllImport("kernel32.dll",
            CallingConvention:=CallingConvention.Winapi,
            CharSet:=CharSet.Auto,
            PreserveSig:=True)>
        Public Function SizeofResource(
                                <[In]()> ByVal hModule As IntPtr,
                                <[In]()> ByVal hResInfo As IntPtr
                                ) As Integer

        End Function

        <DllImport("kernel32.dll",
            CallingConvention:=CallingConvention.Winapi,
            CharSet:=CharSet.Auto,
            EntryPoint:="LoadLibrary",
            PreserveSig:=True)>
        Public Function LoadLibrary(<[In]()> ByVal DllPath As String) As IntPtr

        End Function

        Public Const LOAD_LIBRARY_AS_IMAGE_RESOURCE As UInteger = &H20

        <DllImport("kernel32.dll",
            CallingConvention:=CallingConvention.Winapi,
            CharSet:=CharSet.Auto,
            PreserveSig:=True)>
        Public Function LoadLibraryEx(<[In]()> ByVal DllPath As String, <[In]()> ByVal hFile As IntPtr, <[In]()> ByVal dwFlags As UInteger) As IntPtr

        End Function

        <DllImport("kernel32.dll",
            CallingConvention:=CallingConvention.Winapi,
            EntryPoint:="FreeLibrary",
            PreserveSig:=True,
            SetLastError:=True)>
        Public Function FreeLibrary(<[In]()> ByVal Handle As IntPtr) As Integer

        End Function

        '        typedef struct _NT_TIB {
        '    struct _EXCEPTION_REGISTRATION_RECORD *ExceptionList;
        '    PVOID StackBase;
        '    PVOID StackLimit;
        '    PVOID SubSystemTib;
        '#if defined(_MSC_EXTENSIONS)
        '    union {
        '        PVOID FiberData;
        '        DWORD Version;
        '    };
        '#Else
        '    PVOID FiberData;
        '#End If
        '    PVOID ArbitraryUserPointer;
        '    struct _NT_TIB *Self;
        '} NT_TIB;
        <StructLayout(LayoutKind.Sequential)>
        Public Structure NT_TIB
            Dim ExceptionList As IntPtr
            Dim StackBase As IntPtr
            Dim StackLimit As IntPtr
            Dim SubSystemTib As IntPtr
            Dim FiberData As IntPtr
            Dim ArbitraryUserPointer As IntPtr
            Dim Self As IntPtr
        End Structure


        'typedef struct _TEB {
        '    BYTE Reserved1[1952];
        '    PVOID Reserved2[412];
        '    PVOID TlsSlots[64];
        '    BYTE Reserved3[8];
        '    PVOID Reserved4[26];
        '    PVOID ReservedForOle;  // Windows 2000 only
        '    PVOID Reserved5[4];
        '    PVOID TlsExpansionSlots;
        '} TEB, *PTEB;


        <StructLayout(LayoutKind.Sequential)>
        Public Structure TEB
            <MarshalAs(UnmanagedType.ByValArray, SizeConst:=1952)>
            Dim reserved1() As Byte
            <MarshalAs(UnmanagedType.ByValArray, SizeConst:=412)>
            Dim reserved2() As IntPtr
            <MarshalAs(UnmanagedType.ByValArray, SizeConst:=64)>
            Dim TlsSlots() As IntPtr
            <MarshalAs(UnmanagedType.ByValArray, SizeConst:=8)>
            Dim reserved3() As Byte
            <MarshalAs(UnmanagedType.ByValArray, SizeConst:=26)>
            Dim reserved4() As IntPtr
            Dim ReservedForOle As IntPtr
            <MarshalAs(UnmanagedType.ByValArray, SizeConst:=4)>
            Dim reserved5() As IntPtr
            Dim TlsExpansionSlots As IntPtr
        End Structure


        ' want to have a struct that can map memory or file
        Public IMAGE_DOS_SIGNATURE As UShort = &H5A4D
        <StructLayout(LayoutKind.Sequential)>
        Public Structure _IMAGE_DOS_HEADER
            Public e_magic As UShort
            Public e_cblp As UShort
            Public e_cp As UShort
            Public e_crlc As UShort
            Public e_cparhdr As UShort
            Public e_minalloc As UShort
            Public e_maxalloc As UShort
            Public e_ss As UShort
            Public e_sp As UShort
            Public e_csum As UShort
            Public e_ip As UShort
            Public e_cs As UShort
            Public e_lfarlc As UShort
            Public e_ovno As UShort
            <MarshalAs(UnmanagedType.ByValArray, SizeConst:=4)>
            Public e_res() As UShort
            Public e_oemid As UShort
            Public e_oeminfo As UShort
            <MarshalAs(UnmanagedType.ByValArray, SizeConst:=10)>
            Public e_res2() As UShort
            Public e_lfanew As Integer
            Public Sub New(br As IO.BinaryReader)
                Me.e_magic = br.ReadUInt16
                Me.e_cblp = br.ReadUInt16()
                Me.e_cp = br.ReadUInt16()
                Me.e_crlc = br.ReadUInt16()
                Me.e_cparhdr = br.ReadUInt16()
                Me.e_minalloc = br.ReadUInt16()
                Me.e_maxalloc = br.ReadUInt16()
                Me.e_ss = br.ReadUInt16()
                Me.e_sp = br.ReadUInt16()
                Me.e_csum = br.ReadUInt16()
                Me.e_ip = br.ReadUInt16()
                Me.e_cs = br.ReadUInt16()
                Me.e_lfarlc = br.ReadUInt16()
                Me.e_ovno = br.ReadUInt16()
                ReDim e_res(3)
                For i = 0 To 3
                    Me.e_res(i) = br.ReadUInt16()
                Next
                Me.e_oemid = br.ReadUInt16()
                Me.e_oeminfo = br.ReadUInt16()
                ReDim Me.e_res2(9)
                For i = 0 To 9
                    Me.e_res2(i) = br.ReadUInt16()
                Next
                Me.e_lfanew = br.ReadUInt16()
            End Sub
        End Structure

        Public Structure IMAGE_FILE_HEADER
            Public Machine As UShort
            Public NumberOfSections As UShort
            Public TimeDateStamp As UInteger
            Public PointerToSymbolTable As UInteger
            Public NumberOfSymbols As Integer
            Public SizeOfOptionalHeader As UShort
            Public Characteristics As UShort
            Public Sub New(br As IO.BinaryReader)
                Me.Machine = br.ReadUInt16
                Me.NumberOfSections = br.ReadUInt16
                Me.TimeDateStamp = br.ReadUInt32
                Me.PointerToSymbolTable = br.ReadUInt32
                Me.NumberOfSymbols = br.ReadInt32
                Me.SizeOfOptionalHeader = br.ReadUInt16
                Me.Characteristics = br.ReadUInt16
            End Sub
        End Structure
        Public Structure IMAGE_OPTIONAL_HEADER32
            Public Magic As UShort
            Public MajorLinkerVersion As Byte
            Public MinorLinkerVersion As Byte
            Public SizeOfCode As Integer
            Public SizeOfInitializedData As Integer
            Public SizeOfUninitializedData As Integer
            Public AddressOfEntryPoint As Integer
            Public BaseOfCode As Integer
            Public BaseOfData As Integer
            Public ImageBase As Integer
            Public SectionAlignment As Integer
            Public FileAlignment As Integer
            Public MajorOperatingSystemVersion As UShort
            Public MinorOperatingSystemVersion As UShort
            Public MajorImageVersion As UShort
            Public MinorImageVersion As UShort
            Public MajorSubsystemVersion As UShort
            Public MinorSubsystemVersion As UShort
            Public Win32VersionValue As Integer
            Public SizeOfImage As Integer
            Public SizeOfHeaders As Integer
            Public CheckSum As Integer
            Public Subsystem As UShort
            Public DllCharacteristics As UShort
            Public SizeOfStackReserve As Integer
            Public SizeOfStackCommit As Integer
            Public SizeOfHeapReserve As Integer
            Public SizeOfHeapCommit As Integer
            Public LoaderFlags As Integer
            Public NumberOfRvaAndSizes As Integer
            <MarshalAs(UnmanagedType.ByValArray, SizeConst:=IMAGE_NUMBEROF_DIRECTORY_ENTRIES)>
            Public DataDirectory() As IMAGE_DATA_DIRECTORY

            Public Sub New(br As IO.BinaryReader)
                Me.Magic = br.ReadUInt16
                Me.MajorLinkerVersion = br.ReadByte
                Me.MinorLinkerVersion = br.ReadByte
                Me.SizeOfCode = br.ReadInt32
                Me.SizeOfInitializedData = br.ReadInt32
                Me.SizeOfUninitializedData = br.ReadInt32
                Me.AddressOfEntryPoint = br.ReadInt32
                Me.BaseOfCode = br.ReadInt32
                Me.BaseOfData = br.ReadInt32
                Me.ImageBase = br.ReadInt32
                Me.SectionAlignment = br.ReadInt32
                Me.FileAlignment = br.ReadInt32
                Me.MajorOperatingSystemVersion = br.ReadUInt16
                Me.MinorOperatingSystemVersion = br.ReadUInt16
                Me.MajorImageVersion = br.ReadUInt16
                Me.MinorImageVersion = br.ReadUInt16
                Me.MajorSubsystemVersion = br.ReadUInt16
                Me.MinorSubsystemVersion = br.ReadUInt16
                Me.Win32VersionValue = br.ReadInt32
                Me.SizeOfImage = br.ReadInt32
                Me.SizeOfHeaders = br.ReadInt32
                Me.CheckSum = br.ReadInt32
                Me.Subsystem = br.ReadUInt16
                Me.DllCharacteristics = br.ReadUInt16
                Me.SizeOfStackReserve = br.ReadInt32
                Me.SizeOfStackCommit = br.ReadInt32
                Me.SizeOfHeapReserve = br.ReadInt32
                Me.SizeOfHeapCommit = br.ReadInt32
                Me.LoaderFlags = br.ReadInt32
                Me.NumberOfRvaAndSizes = br.ReadInt32
                ReDim DataDirectory(IMAGE_NUMBEROF_DIRECTORY_ENTRIES)
                For i = 0 To IMAGE_NUMBEROF_DIRECTORY_ENTRIES - 1
                    Me.DataDirectory(i) = New IMAGE_DATA_DIRECTORY(br)
                Next
            End Sub
        End Structure
        Public Enum IMAGE_DIRECTORY_ENTRY_ENUM
            IMAGE_DIRECTORY_ENTRY_EXPORT = 0 '  // Export Directory
            IMAGE_DIRECTORY_ENTRY_IMPORT = 1 '  // Import Directory
            IMAGE_DIRECTORY_ENTRY_RESOURCE = 2 '  // Resource Directory
            IMAGE_DIRECTORY_ENTRY_EXCEPTION = 3 '  // Exception Directory
            IMAGE_DIRECTORY_ENTRY_SECURITY = 4 '  // Security Directory
            IMAGE_DIRECTORY_ENTRY_BASERELOC = 5 '  // Base Relocation Table
            IMAGE_DIRECTORY_ENTRY_DEBUG = 6 '  // Debug Directory
            IMAGE_DIRECTORY_ENTRY_ARCHITECTURE = 7 '  // Architecture Specific Data
            IMAGE_DIRECTORY_ENTRY_GLOBALPTR = 8 '  // RVA of GP
            IMAGE_DIRECTORY_ENTRY_TLS = 9 '  // TLS Directory
            IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG = 10 '  // Load Configuration Directory
            IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT = 11 '  // Bound Import Directory in headers
            IMAGE_DIRECTORY_ENTRY_IAT = 12 '  // Import Address Table
            IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT = 13 '  // Delay Load Import Descriptors
            IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR = 14 '  // COM Runtime descriptor
        End Enum

        Public Const IMAGE_NUMBEROF_DIRECTORY_ENTRIES As Integer = 14
        Public Structure IMAGE_DATA_DIRECTORY
            Public VirtualAddress As Integer
            Public Size As Integer
            Sub New(br As IO.BinaryReader)
                Me.VirtualAddress = br.ReadInt32
                Me.Size = br.ReadInt32
            End Sub
        End Structure

        Public Structure _IMAGE_NT_HEADERS
            Public Signature As Integer
            Public FileHeader As IMAGE_FILE_HEADER
            Public OptionalHeader As IMAGE_OPTIONAL_HEADER32

        End Structure


        <StructLayout(LayoutKind.Sequential, Pack:=4)>
        Public Structure AllocData
            Public cnt As Integer
            Public size As Int64
        End Structure

        <StructLayout(LayoutKind.Sequential, Pack:=4)>
        Public Structure MemStatDetail
            Public Alloc As AllocData
            Public Free As AllocData
            Public Overrides Function ToString() As String
                Return String.Format("(Alloc {0:n0}/{1:n0} Free {2:n0}/{3:n0})", Alloc.cnt, Alloc.size, Free.cnt, Free.size)
            End Function
        End Structure

        <StructLayout(LayoutKind.Sequential, Pack:=4)>
        Public Structure MemStats
            Public SeqNo As Int32
            Public CodeMarkerId As Int32
            Public HeapAllocs As MemStatDetail
            Public ClrObjs As MemStatDetail
            Public ClrClasses As MemStatDetail
            Public ClrOther As MemStatDetail '//Module, assembly,appdomain 
            Public VirtualAlloc As MemStatDetail

            Public Shared Function FromByteArray(res() As Byte) As MemStats
                Dim mstat As MemStats
                mstat.SeqNo = BitConverter.ToInt32(res, 0)
                mstat.CodeMarkerId = BitConverter.ToInt32(res, 4)
                Dim nOff = 8
                mstat.HeapAllocs.Alloc.cnt = BitConverter.ToInt32(res, nOff)
                mstat.HeapAllocs.Alloc.size = BitConverter.ToInt64(res, nOff + 4)
                mstat.HeapAllocs.Free.cnt = BitConverter.ToInt32(res, nOff + 12)
                mstat.HeapAllocs.Free.size = BitConverter.ToInt64(res, nOff + 16)
                nOff += 24
                mstat.ClrObjs.Alloc.cnt = BitConverter.ToInt32(res, nOff)
                mstat.ClrObjs.Alloc.size = BitConverter.ToInt64(res, nOff + 4)
                mstat.ClrObjs.Free.cnt = BitConverter.ToInt32(res, nOff + 12)
                mstat.ClrObjs.Free.size = BitConverter.ToInt64(res, nOff + 16)
                nOff += 24
                mstat.ClrClasses.Alloc.cnt = BitConverter.ToInt32(res, nOff)
                mstat.ClrClasses.Alloc.size = BitConverter.ToInt64(res, nOff + 4)
                mstat.ClrClasses.Free.cnt = BitConverter.ToInt32(res, nOff + 12)
                mstat.ClrClasses.Free.size = BitConverter.ToInt64(res, nOff + 16)
                nOff += 24
                mstat.ClrOther.Alloc.cnt = BitConverter.ToInt32(res, nOff)
                mstat.ClrOther.Alloc.size = BitConverter.ToInt64(res, nOff + 4)
                mstat.ClrOther.Free.cnt = BitConverter.ToInt32(res, nOff + 12)
                mstat.ClrOther.Free.size = BitConverter.ToInt64(res, nOff + 16)
                nOff += 24
                mstat.VirtualAlloc.Alloc.cnt = BitConverter.ToInt32(res, nOff)
                mstat.VirtualAlloc.Alloc.size = BitConverter.ToInt64(res, nOff + 4)
                mstat.VirtualAlloc.Free.cnt = BitConverter.ToInt32(res, nOff + 12)
                mstat.VirtualAlloc.Free.size = BitConverter.ToInt64(res, nOff + 16)

                Return mstat
            End Function
        End Structure


    End Module
End Namespace