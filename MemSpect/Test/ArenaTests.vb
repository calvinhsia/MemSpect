Imports MemSpect


<TestClass()>
<CLSCompliant(False)>
Public Class ArenaTests
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
        RemoveHandler ProcComm.TargetFrozen, AddressOf ArenaClientTargetFrozenEventHandler
        RemoveHandler ProcComm.TargetUnfrozen, AddressOf ArenaClientTargetUnfrozenEventHandler
        BaseTestCleanup()
    End Sub
    Private _didaddTargetFrozenEvent As Boolean
    Public ReadOnly Property ArenaClientExePath As String
        Get
            Return IO.Path.Combine(memspectInstallDir, "Test\ArenaClient.exe")
        End Get
    End Property
    ''' <summary>
    ''' Starts process and waits til frozen
    ''' </summary>
    ''' <param name="plauncher"></param>
    ''' <remarks></remarks>
    Private Sub StartArenaClient(Optional ByVal plauncher As ProcessLauncher = Nothing)
        _ProcessLauncher = plauncher

        If _ProcessLauncher Is Nothing Then
            _ProcessLauncher = New ProcessLauncher() With {
                ._AdditionalCmdLineParams = "",
                .fMessageBoxOnStart = 0,
                .CodeMarkersAtWhichToFreeze = "1",
                .TrackETW = 0,
                .StartChildProcess = 0
            }
        End If
        Dim pname = ArenaClientExePath
        CommonUI.InitializeCommonUI()
        Dim hproc As Process = _ProcessLauncher.LaunchTargProc(pname, fWithDll:=True, wrkdir:=TestContext.DeploymentDirectory) '' dev11 testdll doesn't run in deploydir
        _VBAssert.OutputText("MemSpect vers = " + _MemSpectVersion, fAddToBaseline:=False)
        If Not _didaddTargetFrozenEvent Then
            _didaddTargetFrozenEvent = True
            AddHandler TargetFrozen, AddressOf ArenaClientTargetFrozenEventHandler
        End If


        Dim nCnt = 0
#If DEBUG Then
        Dim nLimit = 50
#Else
        Dim nLimit = 50
#End If
        While Not ProcComm._isTargetFrozen
            'Await System.Threading.Tasks.Task.Run(Sub() System.Threading.Thread.Sleep(1000))
            System.Threading.Thread.Sleep(1000)
            nCnt += 1
            If nCnt = nLimit Then
                'Exit While
                Throw New Exception("Arena Client timeout: ")
            End If
        End While
        _VBAssert.OutputText("Target frozen in StartArenaClient")

        '        FrozenTarget = True
        ReadHeaps()
    End Sub

    Private Sub ArenaTestHelper(ByVal fIsOffline As Boolean)
        If Not ProcComm._isTargetFrozen Then
            _VBAssert.OutputText("Target not frozen in ArenaTestHelper")
        End If
        For Each hp In __HeapList
            _VBAssert.OutputText(hp.HeapName, fAddToBaseline:=False)
        Next
        Dim hpArena = (From hp In _HeapList Where hp.IsArenaHeap).FirstOrDefault
        If hpArena Is Nothing Then
            Throw New Exception("Arena heap not found")
        End If
        Dim snap = hpArena.TakeMemSnapshot
        Dim ctrls = ShowSnapshot(hpArena, snap)
        Dim bmem As BrowseMem = CType(ctrls.SurfaceDetails.Children(0), BrowseMem)
        bmem._TabControl.SelectedIndex = 1 ' select Details tab
        bmem.OnTabItemDetailsGotFocus(bmem._TabItemDetails, New Windows.RoutedEventArgs)
        Dim nItems = bmem._DetailBrowse._BrowseList.Items.Count
        _VBAssert.OutputText("# items = " + nItems.ToString)
        Dim fDidArHdr = False
        Dim fDidArAlloc = False
        For iOutputType = 0 To 1
            Dim nCnt = 0
            For Each itm In bmem._DetailBrowse._BrowseList.Items
                If iOutputType = 0 Then
                    Dim hctr = HeapAllocationContainer.CreateFrom(itm)
                    If fIsOffline Then
                        ' we can test more when offline, because addresses, threadids are const in snap
                        Dim fDoIt = False
                        Select Case hctr.GetArenaBlockType
                            Case ArenaBlkType.Alloc
                                If Not fDidArAlloc Then
                                    fDidArAlloc = True
                                    fDoIt = True
                                    _VBAssert.OutputText("Arena Allocation:")
                                End If
                            Case ArenaBlkType.Header
                                If Not fDidArHdr Then
                                    fDidArHdr = True
                                    _VBAssert.OutputText("Arena Header:")
                                    fDoIt = True
                                End If
                        End Select
                        If fDoIt Then
                            Dim res = GetStacksAndDump(hctr)
                            _VBAssert.OutputText(res.Key, fAddToBaseline:=True, cSplitChar:=CChar(vbCr))
                            _VBAssert.OutputText(res.Value, fAddToBaseline:=True, cSplitChar:=CChar(vbCr))
                        End If
                    End If
                    Dim strData = String.Format("{0,4} {1,-10} {2,-10} {3:x8} {4,10} {5,10} {6,10} {7,10} {8} {9}",
                                                hctr.GetArenaHeaderInfo.ArenaId,
                                                hctr.GetArenaHeaderInfo.ArenaName,
                                                hctr.GetArenaBlockType,
                                                hctr.GetArenaUserDefinedType.ToInt32,
                                                hctr.GetArenaHeaderInfo.CntEverAlloc,
                                                hctr.GetArenaHeaderInfo.SizeEverAlloc,
                                                hctr.GetArenaHeaderInfo.CntCurLive,
                                                hctr.GetArenaHeaderInfo.SizeCurLive,
                                                hctr.GetArenaAllocSize,
                                                hctr.GetStringContent
                                                )

                    _VBAssert.OutputText(strData)

                Else
                    _VBAssert.OutputText((itm.ToString), fAddToBaseline:=fIsOffline)
                End If
                nCnt += 1
                If nCnt = 100 Then
                    Exit For
                End If
            Next
        Next
    End Sub

    <TestMethod()>
    <Description("Test Arena allocator interface")>
    Public Sub ArenaTest()

        ' "c:\Program Files (x86)\Microsoft Visual Studio 10.0\Common7\IDE\MSTest.exe" /noisolation "/testcontainer:d:\ChuckJ\VSTests\DirectAuthorTests\bin\DirectAuthorTests.dll" /test:DirectAuthorTests.CompletionTests.BasicCompletions

        Try
            InitTestMethodAndSetBaseLine()
            _VBAssert.OutputText("Testing Arena (NoRelease) Allocator")

            StartArenaClient()

            ArenaTestHelper(fIsOffline:=False)

        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub

    <TestMethod()>
    <Description("Test Arena allocator interface")>
    Public Sub ArenaOffline()

        ' "c:\Program Files (x86)\Microsoft Visual Studio 10.0\Common7\IDE\MSTest.exe" /noisolation "/testcontainer:d:\ChuckJ\VSTests\DirectAuthorTests\bin\DirectAuthorTests.dll" /test:DirectAuthorTests.CompletionTests.BasicCompletions

        Try
            InitTestMethodAndSetBaseLine()
            _VBAssert.OutputText("Testing Arena (NoRelease) Allocator offline")

            LoadOffLineSnap(IO.Path.Combine(memspectInstallDir, "Snaps\ArenaClient"))

            ArenaTestHelper(fIsOffline:=True)

            _VBAssert.OutputText("Dupes")
            Dim dupeSurface = ShowDuplicateAllocations(From a In _HeapList Where a.IsArenaHeap, Nothing, "Arena dupes")
            DoTestDuplicatesHelper(dupeSurface, "Arena Dupes")


        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub

    <TestMethod()>
    <Description("Test Arena lotsfree")>
    <Ignore>
    Public Sub ArenaLotsFree()

        ' "c:\Program Files (x86)\Microsoft Visual Studio 10.0\Common7\IDE\MSTest.exe" /noisolation "/testcontainer:d:\ChuckJ\VSTests\DirectAuthorTests\bin\DirectAuthorTests.dll" /test:DirectAuthorTests.CompletionTests.BasicCompletions

        Try
            InitTestMethodAndSetBaseLine()
            Dim pLauncher = New ProcessLauncher() With {
                ._AdditionalCmdLineParams = "lotsfree",
                .fMessageBoxOnStart = 0,
                .CodeMarkersAtWhichToFreeze = "1"
            }

            StartArenaClient(pLauncher)

            ArenaTestHelper(fIsOffline:=False)



        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub


    <TestMethod()>
    <Description("Test Tracking Mode")>
    Public Sub ArenaTrackingMode()

        ' "c:\Program Files (x86)\Microsoft Visual Studio 10.0\Common7\IDE\MSTest.exe" /noisolation "/testcontainer:d:\ChuckJ\VSTests\DirectAuthorTests\bin\DirectAuthorTests.dll" /test:DirectAuthorTests.CompletionTests.BasicCompletions

        Try
            InitTestMethodAndSetBaseLine()
            _VBAssert.OutputText("Testing Tracking Mode")
            Dim launcher = New ProcessLauncher() With {
                ._AdditionalCmdLineParams = "TrackingMode",
                .fMessageBoxOnStart = 0,
                .TrackingMode = 0,
                .CodeMarkersAtWhichToFreeze = "1"
            }

            StartArenaClient(launcher)
            Dim hpSnap = ProcessHeapSnap
            Dim lamSnapNumStacklessAllocs = Function(strDesc As String) As Integer
                                                _VBAssert.OutputText(strDesc)
                                                Dim n = 0
                                                For Each hctr In hpSnap.Allocs
                                                    If hctr.AllocationStruct.m_uicStackAddr > 0 Then
                                                        n += 1
                                                    End If
                                                Next
                                                Return n
                                            End Function
            Dim nStacklessAllocs = lamSnapNumStacklessAllocs.Invoke("1st freeze")
            _VBAssert.OutputText("# of allocs = " + hpSnap.Allocs.Count.ToString, fAddToBaseline:=False)
            _VBAssert.IsTrue(hpSnap.Allocs.Count > 2500, "# allocs expected to be > 2500")
            _VBAssert.OutputText("# of allocs With stack = " + nStacklessAllocs.ToString, fAddToBaseline:=False)
            _VBAssert.IsTrue(nStacklessAllocs = 0, "# nStacklessAllocs =00")
            AddHandler TargetUnfrozen, AddressOf ArenaClientTargetUnfrozenEventHandler

            _VBAssert.OutputText("Setting track mode normal" + GetGlobalPassCount.ToString, fAddToBaseline:=False)
            SetTrackingMode(TrackingModeEnum.Normal)
            _VBAssert.OutputText("Unfreeze target" + GetGlobalPassCount.ToString, fAddToBaseline:=False)
            FrozenTarget = False

            _VBAssert.OutputText("sleep a bit" + GetGlobalPassCount.ToString, fAddToBaseline:=False)
            Threading.Thread.Sleep(3000)

            _VBAssert.OutputText("freeze target" + GetGlobalPassCount.ToString + " " + Threading.Thread.CurrentThread.ManagedThreadId.ToString, fAddToBaseline:=False)
            FrozenTarget = True

            hpSnap = ProcessHeap.TakeMemSnapshot
            nStacklessAllocs = lamSnapNumStacklessAllocs.Invoke("2nd Freeze")
            _VBAssert.OutputText("# of allocs = " + hpSnap.Allocs.Count.ToString, fAddToBaseline:=False)
            _VBAssert.IsTrue(hpSnap.Allocs.Count > 3000, "# allocs expected to be > 3000")
            _VBAssert.OutputText("# of allocs With stack = " + nStacklessAllocs.ToString, fAddToBaseline:=False)
            _VBAssert.IsTrue(nStacklessAllocs >= 15, "# nStacklessAllocs >=15    Got " + nStacklessAllocs.ToString)

            Dim ctrls = ShowSnapshot(ProcessHeap, hpSnap)
            Dim bmem As BrowseMem = CType(ctrls.SurfaceDetails.Children(0), BrowseMem)
            Dim tv = bmem._tvPanel._tv
            _VBAssert.IsTrue(tv.Items.Count >= 2, "Should be >=2 kids: bmem._tvPanel._tv.Items.Count " + bmem._tvPanel._tv.Items.Count.ToString)
            _VBAssert.IsTrue(tv.Items(0).ToString.Contains("NoStackCollected"), "tv.Item should have UnknownNodeFrame = " + tv.Items(0).ToString)
            Dim itm2 = CType(tv.Items(1), TVStackAggPanel.StackAggTreeView.StackAggTViewItem)
            _VBAssert.IsTrue(itm2.Items.Count >= 2, "itm2.Items.Count >=2  Got " + itm2.Items.Count.ToString)

            'For Each itm As TVStackAggPanel.StackAggTreeView.StackAggTViewItem In bmem._tvPanel._tv.Items
            '    DumpTVItemChildren("TrackMem children", itm)
            'Next



        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub

    <TestMethod()>
    <Description("Test CustomMarker")>
    Public Sub ArenaCustomMarker()

        Try
            InitTestMethodAndSetBaseLine()
            _VBAssert.OutputText("Testing CustomMarker")
            Dim nIters = 2
            'codemarker 1 = WORDPERF
            Dim launcher = New ProcessLauncher() With {
                ._AdditionalCmdLineParams = "CustomMarker " + nIters.ToString,
                .fMessageBoxOnStart = 0,
                .TrackClrObjects = 1,
                .TrackingMode = 1,
                .CodeMarkersAtWhichToFreeze = "1",
                .CodeMarkersToCollectStats = "100002,100003,100004,100005"
            }
            StartArenaClient(launcher)
            _VBAssert.OutputText("ArenaClientStarted")
            For Each markerAlloc In MemSpectSnap.Allocs.Where(Function(hctr) hctr.TBlkBlockType = BlockTypes.CodeMarker)
                _VBAssert.OutputText(String.Format("Got Marker {0}", CleanLineForBaseline(markerAlloc.GetCodeMarkerName)))
            Next


            For iter = 1 To nIters
                _VBAssert.OutputText("Unfreeze target Iter=" + iter.ToString)
                FrozenTarget = False

                Threading.Thread.Sleep(3000) ' the UnFrozen event shows up while sleeping in here. This sleep needs to be longer than the one in GotCodeMarker
                _VBAssert.OutputText("slept a bit")
                FireCustomCodeMarkerEvent("Test1234", CodeMarkerEventType.Start, dwDepthLevel:=33, dwMarkerId:=100000)

                FireCustomCodeMarkerEvent("OpenSolution/Initialize Control Form.Delete File. Add Winform", CodeMarkerEventType.Start, dwDepthLevel:=33, dwMarkerId:=100002)
                FireCustomCodeMarkerEvent("OpenSolution/Initialize Control Form.Delete File. Add Winform", CodeMarkerEventType.[End], dwDepthLevel:=33, dwMarkerId:=100003)

                FireCustomCodeMarkerEvent("Show toolbox. Rebuild. Close All Documents", CodeMarkerEventType.Start, dwDepthLevel:=33, dwMarkerId:=100004)
                FireCustomCodeMarkerEvent("Show toolbox. Rebuild. Close All Documents", CodeMarkerEventType.[End], dwDepthLevel:=33, dwMarkerId:=100005)

                _VBAssert.OutputText("freeze target")
                FrozenTarget = True

                For Each markerAlloc In MemSpectSnap.Allocs.Where(Function(hctr) hctr.TBlkBlockType = BlockTypes.CodeMarker)
                    Dim nm = markerAlloc.GetCodeMarkerName(fIncludeInstanceNum:=True)
                    _VBAssert.OutputText(String.Format("After Got Marker {0}",
                                                       CleanLineForBaseline(nm)),
                                                   cSplitChar:=CChar(vbCr))
                Next

            Next



            Dim cmarkers = MemSpectSnap.Allocs.Where(Function(hctr) hctr.TBlkBlockType = BlockTypes.CodeMarker)

            _VBAssert.OutputText("Done all iters Marker count = " + cmarkers.Count.ToString)


            MemSpectWin._btnLastSelected = TrkType.CodeMarker

            Dim ctrls = ShowSnapshot(MemSpectSnap.SpyHeap, New MemSnapshot With {.Allocs = cmarkers.ToList}, "Markers")


            Dim mspectWin = CType(ctrls.SurfaceDetails.Children(0), MemSpectWin)
            Dim bmem = mspectWin._bmem

            bmem._TabControl.SelectedIndex = 1 ' select Details tab
            bmem.OnTabItemDetailsGotFocus(bmem._TabItemDetails, New Windows.RoutedEventArgs)


            _VBAssert.OutputText("CodeMarker snap")

            For Each markeritem In bmem._DetailBrowse._BrowseList.Items
                _VBAssert.OutputText(String.Format("{0}", markeritem.ToString), fAddToBaseline:=False)
            Next

            'For Each hctr In cmarkers
            '    If hctr.GetCodeMarkerName(fIncludeInstanceNum:=False) = "WORDPERF" Then ' = 1
            '        _VBAssert.OutputText(hctr.ToString + " Stack = " + hctr.AllocationStruct.m_uicStackAddr.ToString)
            '        Dim stk = hctr.GetCallStackAsString
            '        _VBAssert.OutputText("   " + stk)
            '        For Each line In stk
            '        Next

            '    End If
            'Next
            _VBAssert.OutputText("GetMemStats")
            Dim mm = GetMemStats()
            For Each mstat In mm
                _VBAssert.OutputText(String.Format("Seqno={0},CMarker={1}  Hp={2}, ClrObj= {3} ClrCls={4} ClrOther={5} VirtAlloc= {6}",
                                                   mstat.SeqNo,
                                                   mstat.CodeMarkerId,
                                                   mstat.HeapAllocs.ToString(),
                                                   mstat.ClrObjs.ToString(),
                                                   mstat.ClrClasses.ToString(),
                                                   mstat.ClrOther.ToString(),
                                                   mstat.VirtualAlloc.ToString()
                                                   ), fAddToBaseline:=False)
            Next

        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub


    <TestMethod()>
    <TestCategory("Arena")>
    <TestCategory("Inject")>
    <Description("Arena inject thread")>
    Public Sub ArenaInject()
        Try
            ' we want to start ArenaClient without MemSpect, then we want to inject a thread

            InitTestMethodAndSetBaseLine()
            _VBAssert.OutputText("Arena test CreateRemoteThread injection")

            Dim args = "ArenaInject "
            If Debugger.IsAttached Then
                args += "1000000"
            Else
                Dim additionalDelay = ""
                args += additionalDelay + "9000"
            End If
            Dim wrkdir = TestContext.DeploymentDirectory ' D:\Memspect\VSAssertSrv\TestResults\calvinh_CALVINH9 2010-08-20 10_10_45\Out

            _ProcessLauncher = New ProcessLauncher() With {
                ._AdditionalCmdLineParams = args,
                ._nmsecsToWaitTilStart = 3000,
                .StartChildProcess = 1,
                .fMessageBoxOnStart = 0
            }
            Dim hproc = _ProcessLauncher.LaunchTargProc(ArenaClientExePath, fWithDll:=False, fDoInitcomm:=False, wrkdir:=TestContext.DeploymentDirectory)
            Dim memspDllPath = IO.Path.Combine(wrkdir, MemSpectDllName)

            Dim result = ProcessLauncher.InjectIntoProcess(hproc, memspDllPath, fDebug:=False)
            System.Threading.Thread.Sleep(15000)
            _VBAssert.IsTrue(String.IsNullOrEmpty(result), "InjectIntoProcess failed " + result)
            Dim q = From a In Process.GetProcesses
                    Where a.ProcessName.ToLower.Equals("memspect")
                    Order By a.StartTime Descending
                    Select a.MainWindowTitle, a.ProcessName


            If (q.Count > 1) Then
                For Each p In q
                    _VBAssert.OutputText(String.Format("  {0}", p.ProcessName))
                Next

            End If
            _VBAssert.IsTrue(q.Count = 1, "didn't find 1 instance of memspect process: found " + q.Count.ToString)
            _VBAssert.OutputText("MemSpect process found " + q.FirstOrDefault.ProcessName + " " + q.FirstOrDefault.MainWindowTitle, fAddToBaseline:=False)

            _VBAssert.IsTrue(q.FirstOrDefault.ProcessName = "MemSpect", "Didn't find MemSpect.exe process")
            _VBAssert.IsTrue(q.FirstOrDefault.MainWindowTitle.Contains("ArenaClient"), "Didn't find 'ArenaClient' in MemSpect.exe title " + q.FirstOrDefault.MainWindowTitle)
            _VBAssert.IsTrue(q.FirstOrDefault.MainWindowTitle.Contains(hproc.Id.ToString), "Didn't find 'ArenaClient PID' in MemSpect.exe title " + q.FirstOrDefault.MainWindowTitle)

            '           hProc.WaitForExit()
        Catch ex As Exception
            HandleTestException(ex)
        End Try
        '        _ProcessLauncher = Nothing
    End Sub


    <TestMethod()>
    <Description("Test Arena class names like ConcurDict")>
    <Ignore()>
    Public Sub ArenaConcurDict()

        ' "c:\Program Files (x86)\Microsoft Visual Studio 10.0\Common7\IDE\MSTest.exe" /noisolation "/testcontainer:d:\ChuckJ\VSTests\DirectAuthorTests\bin\DirectAuthorTests.dll" /test:DirectAuthorTests.CompletionTests.BasicCompletions

        Try
            InitTestMethodAndSetBaseLine()
            Dim pLauncher = New ProcessLauncher() With {
                ._AdditionalCmdLineParams = "concurdict",
                .fMessageBoxOnStart = 0,
                .TrackClrObjects = 1,
                .CodeMarkersAtWhichToFreeze = "1"
            }
            StartArenaClient(pLauncher)
            _VBAssert.OutputText("Concurrent dictionary")
            Dim q = From alloc In MemSpectSnap.Allocs
                    Where alloc.TBlkBlockType = BlockTypes.ClrObject AndAlso
                    alloc.GetSize = 36 AndAlso
                    (alloc.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False).
                     Contains("System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.String>"))
                    Order By alloc.AllocationStruct.SeqNo
            ' ConccurrentDictionary is size 36 on both Dev10 and Dev11.
            MemSpectWin._btnLastSelected = TrkType.ClrObjects
            Dim ctrls = ShowSubSnapShot(q.ToList, "Concurrent")
            Dim mspectWin As MemSpectWin = CType(ctrls.SurfaceDetails.Children(0), MemSpectWin)
            Dim bmem = mspectWin._bmem
            bmem._TabControl.SelectedIndex = 1 ' select Details tab
            bmem.OnTabItemDetailsGotFocus(bmem._TabItemDetails, New Windows.RoutedEventArgs)
            Dim nItems = bmem._DetailBrowse._BrowseList.Items.Count
            _VBAssert.OutputText("# items = " + nItems.ToString)
            Dim ncnt = 0
            ClrClassInfo.g_DictClassLayouts.Clear()
            For Each itm In bmem._DetailBrowse._BrowseList.Items
                ncnt += 1
                Dim hctr = HeapAllocationContainer.CreateFrom(itm)
                _VBAssert.OutputText("Item #" + ncnt.ToString)
                _VBAssert.OutputText(hctr.ToString, fAddToBaseline:=True, cSplitChar:=CChar(vbCr))
                _VBAssert.OutputText(hctr.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True), fAddToBaseline:=True, cSplitChar:=CChar(vbCr))
                _VBAssert.OutputText(hctr.GetClrClassMemberLayout, fAddToBaseline:=True, cSplitChar:=CChar(vbCr))
                Dim res = GetStacksAndDump(hctr)
                _VBAssert.OutputText(res.Value, fAddToBaseline:=True, cSplitChar:=CChar(vbCr))
                _VBAssert.OutputText("")
                _VBAssert.OutputText("")

                Dim ctrlObjRef = TVObjRefPanel.CreateObjectReferenceDataSurface(hctr, hctr.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True))
                Dim otvobjrefPanel = CType(ctrlObjRef.SurfaceDetails.Children(0), TVObjRefPanel)
                Dim tvObjref = CType(otvobjrefPanel.Children(0), TVObjRefPanel.TVObjRef)
                For Each item In tvObjref.Items
                    Dim tvitem = CType(item, MemSpect.TVObjRefPanel.TVObjRef.TVObjRefTVItem)
                    If item.ToString.Contains("RefFromMe") Then
                        tvitem.IsExpanded = True
                        _VBAssert.OutputText(tvitem.ToString)
                        GetTVItemInfo(CType(item, MemSpect.TVObjRefPanel.TVObjRef.TVObjRefTVItem), 0, fExpandIfNecessary:=False)
                    End If
                Next

            Next

        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub

    <TestMethod()>
    <Description("Test Arena class names with long membernames")>
    Public Sub ArenaLongMember()
        Try
            InitTestMethodAndSetBaseLine()
            Dim pLauncher = New ProcessLauncher() With {
                ._AdditionalCmdLineParams = "longmember",
                .fMessageBoxOnStart = 0,
                .TrackClrObjects = 1,
                .CodeMarkersAtWhichToFreeze = "1"
            }
            StartArenaClient(pLauncher)
            '            System.Threading.Thread.Sleep(15000) 'sleep so can attach debugger

            _VBAssert.OutputText("LongMemberName")

            Dim q = From alloc In MemSpectSnap.Allocs
                    Where alloc.TBlkBlockType = BlockTypes.ClrObject AndAlso
                    (alloc.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False).
                     Contains("ClassWithLongName"))
                    Order By alloc.AllocationStruct.SeqNo

            For Each Typ In q
                _VBAssert.OutputText("Got results " + Typ.ToString, fAddToBaseline:=False)
                Dim layout = Typ.GetClrClassMemberLayout()
                Dim res = System.Text.RegularExpressions.Regex.Replace(layout, "0x[0-9A-Fa-f]{8}", "0x00000000")

                _VBAssert.OutputText("Layout = " + res, fAddToBaseline:=True, cSplitChar:=CChar(vbCr))
            Next
        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub



    <TestMethod()>
    <Description("Test Arena class names like nullable")>
    <Ignore()>
    Public Sub ArenaNullable()

        ' "c:\Program Files (x86)\Microsoft Visual Studio 10.0\Common7\IDE\MSTest.exe" /noisolation "/testcontainer:d:\ChuckJ\VSTests\DirectAuthorTests\bin\DirectAuthorTests.dll" /test:DirectAuthorTests.CompletionTests.BasicCompletions

        Try
            InitTestMethodAndSetBaseLine()
            Dim pLauncher = New ProcessLauncher() With {
                ._AdditionalCmdLineParams = "nullable",
                .fMessageBoxOnStart = 0,
                .TrackClrObjects = 1,
                .CodeMarkersAtWhichToFreeze = "1"
            }
            StartArenaClient(pLauncher)

            Dim q = From alloc In MemSpectSnap.Allocs
                    Where alloc.TBlkBlockType = BlockTypes.ClrObject AndAlso
                    (alloc.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True).Contains("NullableContClass"))
                 Order By alloc.AllocationStruct.SeqNo

            MemSpectWin._btnLastSelected = TrkType.ClrObjects
            Dim ctrls = ShowSubSnapShot(q.ToList, "Nullable")
            Dim mspectWin As MemSpectWin = CType(ctrls.SurfaceDetails.Children(0), MemSpectWin)
            Dim bmem = mspectWin._bmem
            bmem._TabControl.SelectedIndex = 1 ' select Details tab
            bmem.OnTabItemDetailsGotFocus(bmem._TabItemDetails, New Windows.RoutedEventArgs)
            Dim nItems = bmem._DetailBrowse._BrowseList.Items.Count
            _VBAssert.OutputText("# items = " + nItems.ToString)
            Dim ncnt = 0
            ClrClassInfo.g_DictClassLayouts.Clear()
            For Each itm In bmem._DetailBrowse._BrowseList.Items
                ncnt += 1
                Dim hctr = HeapAllocationContainer.CreateFrom(itm)
                Dim res = GetStacksAndDump(hctr)
                _VBAssert.OutputText("Item #" + ncnt.ToString)
                CleanLineForBaseline(hctr.GetCallStackAsString)
                _VBAssert.OutputText(res.Key, fAddToBaseline:=True, cSplitChar:=CChar(vbCr))
                _VBAssert.OutputText(res.Value, fAddToBaseline:=True, cSplitChar:=CChar(vbCr))
                _VBAssert.OutputText("")
                _VBAssert.OutputText("")

                Dim ctrlObjRef = TVObjRefPanel.CreateObjectReferenceDataSurface(hctr, hctr.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True))
                Dim otvobjrefPanel = CType(ctrlObjRef.SurfaceDetails.Children(0), TVObjRefPanel)
                Dim tvObjref = CType(otvobjrefPanel.Children(0), TVObjRefPanel.TVObjRef)
                For Each item In tvObjref.Items
                    Dim tvitem = CType(item, MemSpect.TVObjRefPanel.TVObjRef.TVObjRefTVItem)
                    If item.ToString.Contains("RefFromMe") Then
                        tvitem.IsExpanded = True
                        _VBAssert.OutputText(tvitem.ToString)
                        GetTVItemInfo(CType(item, MemSpect.TVObjRefPanel.TVObjRef.TVObjRefTVItem), 0, fExpandIfNecessary:=False)
                    End If
                Next

            Next

        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub

    Private Sub ArenaClientTargetFrozenEventHandler()
        _VBAssert.OutputText("Got TargetFrozen Event" + GetGlobalPassCount.ToString +
                             " " + Threading.Thread.CurrentThread.ManagedThreadId.ToString, fAddToBaseline:=False)
    End Sub

    Private Sub ArenaClientTargetUnfrozenEventHandler()
        _VBAssert.OutputText("Got TargetUnFrozen Event" + GetGlobalPassCount.ToString, fAddToBaseline:=False)
    End Sub



End Class
