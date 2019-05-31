Imports MemSpect
Imports System.IO.Compression


<TestClass()>
<CLSCompliant(False)>
Public Class ZipTests
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
        AddHandler MemSpect.Common.StatusMessageEvent, AddressOf TheStatusHandler
    End Sub
    <TestCleanup()>
    Public Sub TestCleanup()
        BaseTestCleanup()
    End Sub
    <TestMethod>
    Public Sub ZipTest()
        InitTestMethodAndSetBaseLine()
        Try
            
            Dim snapName = "c:\MemSpect\Snaps\CSLife"
            'snapName = "c:\a1"
            'snapName = "\\vspfs2\results\stress\MemSpectApex\calvinh\t.zip"
            '            snapName = "c:\t2"
            'snapName = "\\VSPERFRELFUN2\MemSpectApex\calvinh\Snap__VSConsumption2014-03-10_18-35_perfVSCloseSolutionBegin5_0_0\SnapSerializedData.zip"
            'snapName = "\\VSPERFRELFUN2\MemSpectApex\calvinh\Snap__VSConsumption2014-03-10_18-35_perfVSCloseSolutionBegin5_0_0"
            'OfflineMegaSnapshot.LoadMegaSnapshot(fWaitTilDone:=True, snapName:=snapName)

            LoadOffLineSnap(snapName, fWaitTilClrLoadDone:=True, fClearFilterToo:=True)


            'Using fstr = New IO.FileStream(IO.Path.Combine(snapName, Common.SnapDataZipName), IO.FileMode.Open)
            '    Using za = New ZipArchive(fstr, ZipArchiveMode.Read)
            '        For Each entry In za.Entries
            '            If entry.FullName = "Filter.mhd" Then

            '                Dim strm = entry.Open()
            '                'Dim snap = New OfflineMegaSnapshot(snapName)
            '                Using deserializer = OfflineMegaSnapshot.GetDeserializerForPath(IO.Path.Combine(snapName, entry.FullName), Common._offlineSnapshot, strm)
            '                    Dim x = CType(deserializer.GetEntryObject(), GlobalFilter)
            '                    '                     MemSpect 131125 UI Dbg exception: System.NotSupportedException: This operation is not supported.
            '                    'at System.IO.Compression.DeflateStream.get_Length()
            '                    'at FastSerialization0.IOStreamStreamReader.Fill(Int32 minimum) in c:\MemSpect\FastSerialization\StreamReaderWriter.cs:line 495
            '                    'at FastSerialization0.MemoryStreamReader.ReadInt32() in c:\MemSpect\FastSerialization\StreamReaderWriter.cs:line 46
            '                    'at FastSerialization0.Deserializer..ctor(IStreamReader reader, String streamName) in c:\MemSpect\FastSerialization\FastSerialization.cs:line 808
            '                    'at MemSpect.Common.OfflineMegaSnapshot.GetDeserializerForPath(String fullPath, OfflineMegaSnapshot offlineSnapshot, Stream strm) in c:\memspect\MemSpectBase\Common.vb:line 2169
            '                    'at Test.ZipTests.ZipTest() in c:\memspect\Test\ZipTests.vb:line 50

            '                End Using
            '                strm.Close()

            '            End If

            '        Next

            '    End Using

            'End Using

            'Dim zipPack = IO.Compression.ZipFile.Open(IO.Path.Combine(snapName, Common.SnapDataZipName), ZipArchiveMode.Read)
            'For Each entry In zipPack.Entries
            '    _VBAssert.OutputText(entry.FullName)
            '    '                _VBAssert.OutputText("Stream len = " + str.Length.ToString)

            'Next

            'Common.OfflineMegaSnapshot.LoadMegaSnapshot(fWaitTilDone:=True, snapName:=snapName)
            Dim procheapAllocs = _HeapList.Where(Function(h) h.GetHeapName = ProcessHeapName).Single.TakeMemSnapshot(fEnableFilter:=False).Allocs
            For Each hctr In procheapAllocs
                Dim c = hctr.GetStringContent

            Next


            _VBAssert.OutputText("Loaded # heaps = " + _HeapList.Count.ToString)
        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub

    <TestMethod>
    Public Sub SnapLoad()
        InitTestMethodAndSetBaseLine()
        Try
            Dim snapName = "c:\MemSpect\Snaps\CSLife"
            'snapName="c:\t.zip"
            Common.OfflineMegaSnapshot.LoadMegaSnapshot(fWaitTilDone:=True, snapName:=snapName)
            _VBAssert.OutputText("Loaded # heaps = " + _HeapList.Count.ToString)
        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub

End Class
