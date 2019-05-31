Imports MemSpect
Imports System.IO
Imports System.Runtime.InteropServices

<TestClass()>
<CLSCompliant(False)>
Public Class ImageTests
    Inherits TestBase

    <ClassInitialize()>
    Public Shared Sub ClassInit(ByVal ctx As TestContext)
        BaseClassInitialize(ctx)
    End Sub
    <ClassCleanup()>
    Public Shared Sub ClassCleanup()
        BaseClassCleanup()
    End Sub

    <TestInitialize()>
    Sub TestInit()
        BaseTestInit()
    End Sub
    <TestCleanup()>
    Public Sub TestCleanup()
        BaseTestCleanup()
    End Sub

    <TestMethod>
    <Ignore>
    Public Sub TestReadPEHeader()
        Try
            InitTestMethodAndSetBaseLine()
            For ii = 1 To 1
                Dim filename = Path.Combine(_TestContext.TestDeploymentDir, MemSpectDllName)
                _VBAssert.OutputText("Starting PE header read test")
                If Not File.Exists(filename) Then
                    Throw New FileNotFoundException(filename)
                End If
                Using strm As New StreamReader(filename)
                    Using br As New BinaryReader(strm.BaseStream)

                        Dim dosHeader = New _IMAGE_DOS_HEADER(br)
                        _VBAssert.OutputText(String.Format("Got addr {0:x8}", dosHeader.e_lfanew))

                        br.BaseStream.Seek(dosHeader.e_lfanew + 4, SeekOrigin.Begin) ' skip over IMAGE_NT_HEADERS.Signature
                        Dim fileHeader = New IMAGE_FILE_HEADER(br)
                        _VBAssert.OutputText(String.Format("FileHeader Machine = {0:x8} #Secs = {1} ", fileHeader.Machine, fileHeader.NumberOfSections))
                        Dim optHeader = New IMAGE_OPTIONAL_HEADER32(br)
                        _VBAssert.OutputText(String.Format("OptHeader {0:x8} LinkVer = {1}.{2} ImageBase = {3:x8}",
                                                           optHeader.Magic,
                                                           optHeader.MajorLinkerVersion,
                                                           optHeader.MinorLinkerVersion,
                                                           optHeader.ImageBase
                                                           ))
                        For i = 0 To IMAGE_NUMBEROF_DIRECTORY_ENTRIES - 1
                            _VBAssert.OutputText(String.Format(" {0,-50} {1:x0} {2}",
                                                               CType(i, IMAGE_DIRECTORY_ENTRY_ENUM),
                                                               optHeader.DataDirectory(i).VirtualAddress,
                                                               optHeader.DataDirectory(i).Size))
                        Next
                    End Using
                End Using


                _VBAssert.OutputText("Now for Kernel32.dll in memory")
                Dim krnl32addr = GetModuleHandle("kernel32.dll")
                _VBAssert.OutputText("Got kernel32 addr = " + krnl32addr.ToInt32.ToString("x8"), fAddToBaseline:=False)
                Dim dosHeaderk32 = CType(Marshal.PtrToStructure(krnl32addr, GetType(_IMAGE_DOS_HEADER)), _IMAGE_DOS_HEADER)
                Dim addrFileHeader = krnl32addr.ToInt32 + dosHeaderk32.e_lfanew + IntPtr.Size ' skip over IMAGE_NT_HEADERS.Signature
                _VBAssert.OutputText(String.Format("K32 Got addr {0:x8} FileHdr={1:x8}", dosHeaderk32.e_lfanew, addrFileHeader))
                Dim fileHdrk32 = CType(Marshal.PtrToStructure(New IntPtr(addrFileHeader), GetType(IMAGE_FILE_HEADER)), IMAGE_FILE_HEADER)
                _VBAssert.OutputText(String.Format("Got k32 file header Machine = {0:x8} #sects = {1}", fileHdrk32.Machine, fileHdrk32.NumberOfSections))
                Dim opthdr32addr = New IntPtr(addrFileHeader + Marshal.SizeOf(fileHdrk32))
                _VBAssert.OutputText(String.Format("Addr opt hdr = {0:x8}", opthdr32addr.ToInt32))
                Dim opthdrk32 = CType(Marshal.PtrToStructure(opthdr32addr, GetType(IMAGE_OPTIONAL_HEADER32)), IMAGE_OPTIONAL_HEADER32)
                _VBAssert.OutputText(String.Format("k32OptHeader {0:x8} LinkVer = {1}.{2} ImageBase = {3:x8}",
                                                   opthdrk32.Magic,
                                                   opthdrk32.MajorLinkerVersion,
                                                   opthdrk32.MinorLinkerVersion,
                                                   opthdrk32.ImageBase
                                                   ))

                _VBAssert.OutputText(String.Format("Calling func {0:x8}", Images.GetDllBaseAddressFromMem(krnl32addr).ToInt32))
            Next
        Catch ex As Exception
            HandleTestException(ex)
        End Try


    End Sub
End Class
