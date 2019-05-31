Imports System.Text
Imports MemSpect
Imports System.Windows.Controls
Imports System.Runtime.InteropServices
Imports System.Windows
Imports MemSpect.Images
Imports System.Reflection
Imports System.IO
Imports System.Windows.Input
Imports System.Text.RegularExpressions
Imports System.Windows.Threading

'One of the background threads threw exception:System.Runtime.InteropServices.InvalidComObjectException: COM object that has been separated from its underlying RCW cannot be used. at 
' Tools->Options->Test Tools->Test Execution->Performance->Keep Test execution engine running between test runs
<CLSCompliant(False)>
Public Class TestBase
    Protected _nmSecsDelayBetweenUIDisplay As Integer = 0 'msecs
    Protected _threadIdOfTest As Integer
    Friend _VBAssert As VBAssert
    Friend _TestContext As TestContext
    Friend _ProcessLauncher As ProcessLauncher

    Private Function GetTrackClrObjectSetting() As Boolean
        _VBAssert.IsNotNull(_ProcessLauncher, "how do we have no processlauncher?")
        If _ProcessLauncher IsNot Nothing OrElse Not _ProcessLauncher._DidInitComm Then
            '            _VBAssert.IsTrue(False, "Didn't init comm with process")
            Return False
        End If
        If _ConnectionMode = MemSpectMode.Offline Then
            Return False
        End If
        SendMsg(ProcMsgVerb.ClrObjTrk, fSendEndMsgSync:=True, dwords:={CUInt(3)}) ' get cur clr obj trk setting into shared mem byte 0
        Dim n = Marshal.ReadInt32(_SharedMemAddr, 0)
        Return n <> 0

    End Function

    Protected Sub TheStatusHandler(ByVal o As Object, ByVal e As StatusMessageEventArgs)
        If e.MsgType <> StatusMessageType.StatusBarEntry Then
            Try
                _VBAssert.OutputText(String.Format("[{0:hh\:mm\:ss\.fff t}] {1}", DateTime.Now, e.Message), fAddToBaseline:=False)
            Catch ex As Exception

            End Try
        End If

    End Sub

    Friend Property TrackClrObjects As Boolean
        Get
            Return GetTrackClrObjectSetting()
        End Get
        Set(ByVal newvalue As Boolean)
            Dim fCurval = GetTrackClrObjectSetting()
            If fCurval Then
                If newvalue = True Then

                Else
                    SendMsg(ProcMsgVerb.ClrObjTrk, fSendEndMsgSync:=True, dwords:={CUInt(0)}) ' turn off clr obj trk
                    _VBAssert.IsTrue(fCurval <> GetTrackClrObjectSetting(), "couldn't set ClrObjTrackSetting off")
                End If
            Else
                If newvalue = True Then
                    SendMsg(ProcMsgVerb.ClrObjTrk, fSendEndMsgSync:=True, dwords:={CUInt(1)}) ' turn on clr obj trk
                    _VBAssert.IsTrue(fCurval <> GetTrackClrObjectSetting(), "couldn't set ClrObjTrackSetting on")
                Else

                End If
            End If
        End Set
    End Property

    Property TestContext As TestContext
        Get
            Return _TestContext
        End Get
        Set(ByVal value As TestContext)
            _TestContext = value
        End Set
    End Property

    Protected _memSpectSnap As MemSnapshot
    Protected ReadOnly Property MemSpectSnap As MemSnapshot
        Get
            If _memSpectSnap Is Nothing Then
                Dim MemSpectheap = (From hp In _HeapList Where hp.IsMemSpectHeap).First
                _memSpectSnap = MemSpectheap.TakeMemSnapshot
            End If
            Return _memSpectSnap
        End Get
    End Property
    Private _ProcHeap As CSpyHeap
    Protected ReadOnly Property ProcessHeap As CSpyHeap
        Get
            If _ProcHeap Is Nothing Then
                _ProcHeap = (From hp In _HeapList Where hp.HeapName = ProcessHeapName).First
            End If
            Return _ProcHeap
        End Get
    End Property
    Protected _ProcHeapSnap As MemSnapshot
    Protected ReadOnly Property ProcessHeapSnap As MemSnapshot
        Get
            If _ProcHeapSnap Is Nothing Then
                _ProcHeapSnap = ProcessHeap.TakeMemSnapshot
            End If
            Return _ProcHeapSnap
        End Get
    End Property

    Sub New()
        _IsFrozen = False
        _threadIdOfTest = System.Threading.Thread.CurrentThread.ManagedThreadId
    End Sub

    Private _IsFrozen As Boolean
    Protected Property FrozenTarget As Boolean
        Get
            If _ConnectionMode = MemSpectMode.Offline Then
                Return True
            End If
            Return _IsFrozen
        End Get
        Set(ByVal value As Boolean)
            If _ConnectionMode = MemSpectMode.OnLine Then
                Assert.IsTrue(_ProcessLauncher._DidInitComm, "can't freeze: didn't init comm")
                If value Then
                    If Not ProcComm._isTargetFrozen Then
                        _VBAssert.OutputText("Freezing target. SeqNo = " + GetGlobalPassCount.ToString, fAddToBaseline:=False)
                        ProcComm.FreezeTarget()
                        _IsFrozen = True
                    End If
                Else
                    If ProcComm._isTargetFrozen Then
                        ProcComm.UnFreezeTarget()
                        _memSpectSnap = Nothing
                        _ProcHeapSnap = Nothing
                        _IsFrozen = False
                    End If
                End If
            End If
        End Set
    End Property

    Shared Sub BaseClassInitialize(ByVal ctx As TestContext)
        Dim wrkdir = ctx.DeploymentDirectory ' D:\Memspect\VSAssertSrv\TestResults\calvinh_CALVINH9 2010-08-20 10_10_45\Out
        'Dim asmdir = Path.GetDirectoryName(Reflection.Assembly.GetExecutingAssembly.Location)

        DeployMemspectFiles(memspectInstallDir, wrkdir, fIncludeAll:=True)
        ' force the deployed version to be loaded even though the curdir might be c:\memspect\Test\bin\debug
        ' note: on Dev10, this loads dbghelp.dll again from the deploy dir, which causes confusion on globals

        ' Dev10: ==>Actual  : "Runtime version 4.0.30319.269"
        ' Dev11: ==>Actual  : "Runtime version 4.0.30319.17929"
        If Not System.Environment.Version.ToString.EndsWith("30319.269") Then
            ' http://blogs.msdn.com/b/calvin_hsia/archive/2008/10/28/9020745.aspx
            Using x = New DynamicDllLoader(Path.Combine(wrkdir, "dbghelp.dll"), fUnload:=False)
                '    VsClearSymbols() ' call the export to load it from the desired location
            End Using
            Using x = New DynamicDllLoader(Path.Combine(wrkdir, MemSpectDllName))
                VsClearSymbols() ' call the export to load it from the desired location
            End Using

        End If



    End Sub

    Shared Sub BaseClassCleanup()
    End Sub

    Protected Sub BaseTestInitStatics()
        _ProcessLauncher = Nothing
        _IsUnderTest = True
        _ShowUI = False
        _GlobalFilter.ClearFilter()
        _memSpectSnap = Nothing
        _ProcHeapSnap = Nothing
        _ProcHeap = Nothing
        HeapAllocationContainer.__CodeMarkerSnap = Nothing
        GCData._ClrObjRefDict = Nothing
        _CodeMarkerNameDict.Clear()
        ProcComm._isCommInitialized = False
    End Sub

    Protected _TestStartTime As DateTime
    Protected Sub BaseTestInit()
        _VBAssert = New VBAssert(Me)
        BaseThreadCheck(Me)
        BaseTestInitStatics()
        _nmSecsDelayBetweenUIDisplay = 0
        _TestStartTime = DateTime.Now
        '        g_TestContext.BeginTimer(g_TestContext.TestName)
        'g_TestContext.AddResultFile("d:\t.txt")
        _VBAssert.OutputText("Test start " + _TestStartTime.ToString + " " + TestContext.TestName, fAddToBaseline:=False)
        _VBAssert.OutputText("Note: lines starting with " + VBAssert.g_TestCommentPrefix + " are comments and not used for comparison to determine failure", fAddToBaseline:=False)
    End Sub

    ''' <summary>
    ''' 
    ''' </summary>
    ''' <param name="baselineName">defaults to testname</param>
    ''' <remarks></remarks>
    Protected Sub InitTestMethodAndSetBaseLine(Optional ByVal baselineName As String = "")
        Common.Initialize()
        If baselineName = "" Then
            baselineName = TestContext.TestName
        End If
        Dim baselineFile = IO.Path.Combine(memspectInstallDir, "Test\Baselines\" + baselineName + ".txt")
        _VBAssert.SetBaselineFile(baselineFile)
    End Sub

    Protected Sub BaseTestCleanup()
        BaseThreadCheck(Me)
        RemoveHandler StatusMessageEvent, AddressOf TheStatusHandler
        CLRObjRefsWrapper.WaitTilLoaded() ' make sure loaded before next test runs
        If _ConnectionMode = MemSpectMode.OnLine Then
            Try
                If _ProcessLauncher IsNot Nothing Then
                    TrackClrObjects = False ' shuts down faster
                End If
            Catch ex As Exception
                _VBAssert.OutputText("BaseTestCleanup exception turning off TrackClrObj " + ex.Message)
            End Try
        End If
        If _ChildWindows IsNot Nothing Then
            _ChildWindows.Clear()
        End If
        If DataWindowMain._DataWindowMain IsNot Nothing Then
            DataWindowMain._DataWindowMain.Close()
        End If
        'http://blogs.msdn.com/b/mpuleio/archive/2008/03/19/weird-com-exceptions-in-scsf-cab-unit-tests.aspx
        'System.Windows.Threading.Dispatcher.CurrentDispatcher.InvokeShutdown()
        If _ProcessLauncher IsNot Nothing Then
            If _ConnectionMode = MemSpectMode.OnLine Then
                If TrackClrObjects Then
                    TrackClrObjects = False
                End If
            End If
            Dim res = _ProcessLauncher.ShutDownTargetProcess()
            If Not String.IsNullOrEmpty(res) Then
                _VBAssert.OutputText("Process Shutdown: " + res)
            End If
            _ProcessLauncher = Nothing
        End If
        GC.Collect()
        Common.Initialize()

        Dim testEndTime = DateTime.Now
        Dim dura = testEndTime - _TestStartTime
        _VBAssert.OutputText(String.Format("Test end {0} {1} #Secs = {2:n2}", TestContext.TestName, testEndTime.ToString, dura.TotalSeconds), fAddToBaseline:=False)
        _VBAssert.OnTestFinished()
        '       g_TestContext.EndTimer(g_TestContext.TestName)
        CloseCommunications()
    End Sub

    Protected Sub HandleTestException(ByVal ex As Exception)
        Dim str = "MemSpect " + _MemSpectVersion + " exception: " + ex.ToString
        _VBAssert.OutputText(str)
        Assert.Fail(str)
    End Sub

    Private _thread As System.Threading.Thread
    Protected Sub BaseThreadCheck(ByVal tst As TestBase)
        If _thread Is Nothing Then
            _thread = System.Threading.Thread.CurrentThread
        Else
            _VBAssert.IsTrue(_threadIdOfTest = System.Threading.Thread.CurrentThread.ManagedThreadId, "called on wrong thread ")
            _VBAssert.IsTrue(_thread.ManagedThreadId = System.Threading.Thread.CurrentThread.ManagedThreadId, "called on wrong thread ")
        End If

    End Sub

    Protected Sub CloseCommunications()
        ProcComm.CloseComm()
        If _ProcessLauncher IsNot Nothing Then
            _ProcessLauncher._DidInitComm = False
        End If
    End Sub

    Protected Function WaitTilCodeMakerperfIdle() As Integer
        Dim nSeqnoIdle = 0
        Dim ntimes = 0
        Do
            ' this gets perfIdle no matter when it occured
            'nSeqnoIdle = Common._PerfIdleSeqNo  ' this gets perfIdle only if we're alive when it happened
            If nSeqnoIdle = 0 Then
                Dim resultByteArray = SendMsg(ProcMsgVerb.GetSharedMem, fSendEndMsgSync:=True)
                nSeqnoIdle = BitConverter.ToInt32(resultByteArray, 6 * 4 + 1)
            End If
            If nSeqnoIdle > 0 Then
                '     System.Threading.Thread.Sleep(3000 * If(_ProcessLauncher.targProcIsDebug, 5, 1)) ' too soon: wait a bit more
                Exit Do
            End If

            System.Threading.Thread.Sleep(1000)
            ntimes += 1
            If ntimes = 120 Then
                _VBAssert.OutputText("No perfidle found. Other codemarkers found:")
                FrozenTarget = True
                ReadHeaps()
                Dim hp = (From heap In _HeapList Where heap.GetHeapName = MemSpectHeapName).First
                For Each hctr In hp.TakeMemSnapshot.Allocs.Where(Function(h) h.TBlk.BlockType = BlockTypes.CodeMarker)
                    _VBAssert.OutputText(hctr.ToString)
                Next

                For Each hctr In hp.TakeMemSnapshot.Allocs.Where(Function(h) h.TBlk.BlockType = BlockTypes.IndirectInfo)
                    _VBAssert.OutputText(hctr.ToString)
                Next
                FrozenTarget = False
                ProcComm.CloseComm()
                Throw New InvalidOperationException("perfidle didn't occur after " + ntimes.ToString + " seconds. ProcComm._isTargetFrozen = " + ProcComm._isTargetFrozen.ToString)
            End If
        Loop
        Return nSeqnoIdle
    End Function


    Protected Sub StartVS()
        CommonUI.InitializeCommonUI()
        '// sign up for MemSpect status events early
        AddHandler MemSpect.Common.StatusMessageEvent, AddressOf TheStatusHandler

        _ProcessLauncher.LaunchTargProc(ProcessLauncher.GetDevEnvFullPath, fWithDll:=True, wrkdir:=TestContext.DeploymentDirectory)

        _VBAssert.OutputText("MemSpect vers = " + _MemSpectVersion, fAddToBaseline:=False)
        VBDiagMarginBase.ShowSymbolPath()

        If Not _ProcessLauncher.FreezeAtStartup = 1 Then
            WaitTilCodeMakerperfIdle()
        End If

        'If _ProcessLauncher.TrackClrObjects > 0 Then ' slower
        '    System.Threading.Thread.Sleep(8000 * If(_ProcessLauncher.targProcIsDebug, 3, 1))
        'End If
        Debug.Assert(_ProcessLauncher._pidTargetProcess <> 0, "didn't get pid")
        'If nSeqNoToStart > 0 Then
        '    Dim nDelta = Integer.MaxValue
        '    Dim nOld = 0
        '    Do While True
        '        Dim n = GetGlobalPassCount()
        '        If n - nOld < 6000 Then ' delta in 5 secs < 6000
        '            System.Threading.Thread.Sleep(5000)
        '            Exit Do
        '        End If
        '        nOld = n
        '        'If n >= nSeqNoToStart Then
        '        '    Exit Do
        '        'End If
        '        System.Threading.Thread.Sleep(5000)
        '    Loop
        'End If
        FrozenTarget = True
        _didBulkGetStackIndex = True
        BulkReadStackIndexes()
        FrozenTarget = True
        ReadHeaps()
    End Sub




    Protected Sub LoadOffLineSnap(ByVal SnapName As String,
                                  Optional ByVal fWaitTilClrLoadDone As Boolean = True,
                                  Optional ByVal fClearFilterToo As Boolean = True
                                                                                  )
        _nmSecsDelayBetweenUIDisplay = 0
        Dim strresult = CommonUI.InitCommWithUI({System.Environment.GetCommandLineArgs(0), "/o", SnapName})
        If fWaitTilClrLoadDone Then
            CLRObjRefsWrapper.WaitTilLoaded()
        End If
        If Not String.IsNullOrEmpty(strresult) Then
            _VBAssert.OutputText("Could not load offline snap " + SnapName + " " + strresult)
        End If
        If fClearFilterToo Then
            _GlobalFilter.ClearFilter()
        End If
        FrozenTarget = True
    End Sub


    Protected Sub DoTestWastedMemory(ByVal nMatchCnt As Integer, ByVal nItemsExpected As Integer, Optional ByVal fDumpIt As Boolean = False)
        Dim ctrls = DoShowAllocationsWithWaste("test")
        Dim bmem As BrowseMem = CType(ctrls.SurfaceDetails.Children(0), BrowseMem)
        bmem._TabControl.SelectedIndex = 1 ' select Details tab
        bmem.OnTabItemDetailsGotFocus(bmem._TabItemDetails, New Windows.RoutedEventArgs)
        Dim nItems = bmem._DetailBrowse._BrowseList.Items.Count

        _VBAssert.OutputText("WastedMemory test MatchCnt = " +
                             _nToMatchForUnused.ToString +
                             " # items = " +
                             nItems.ToString, fAddToBaseline:=False)
        _VBAssert.IsTrue(nMatchCnt <= _nToMatchForUnused, "Expected MatchCnt failed: Expected  " + nMatchCnt.ToString + " actual: " + _nToMatchForUnused.ToString)

        _VBAssert.IsTrue(nItemsExpected <= nItems, "Expected MatchCnt failed: Expected  " + nItemsExpected.ToString + " actual: " + nItems.ToString)
        If fDumpIt Then
            For Each itm In bmem._DetailBrowse._BrowseList.Items
                _VBAssert.OutputText(itm.ToString)
            Next
        End If

    End Sub

    Private Sub ModFoldRecur(ByVal col As ItemCollection)
        Dim lst = New SortedSet(Of String) ' sort them
        For Each itm As ModFoldPanel.ModFoldItem In col
            lst.Add(itm._NodeName)
            If itm._NodeName = "USER32.dll" Then
                ModFoldRecur(itm.Items)
            End If
        Next
        For Each itm In lst
            Dim fAddTobaseline = True
            If itm.Contains("DRAWMENU") OrElse
                itm.ToLower = MemSpectDllName.ToLower OrElse
                itm.IndexOf("msvcr", StringComparison.OrdinalIgnoreCase) >= 0 OrElse
                itm.IndexOf("oleaut32", StringComparison.OrdinalIgnoreCase) >= 0 OrElse
                itm.Contains("LoadStringW") Then
                'USER32.dll!__fnINLPUAHDRAWMENU + 46 bytes
                'USER32.dll!__fnINLPUAHDRAWMENUITEM + 68 bytes
                fAddTobaseline = False
            End If
            _VBAssert.OutputText("ModFold " + itm, fAddTobaseline)
        Next
    End Sub

    Protected Sub DoTestModuleFold()
        _VBAssert.OutputText("TestModuleFold")
        Dim ctrls = ShowModFold()
        Dim panelModelFold = CType(ctrls.SurfaceDetails.Children(0), ModFoldPanel)
        Dim tvmodFold = ModFoldPanel._tvsym
        ModFoldRecur(tvmodFold.Items)
    End Sub

    Protected Sub DoTestMappedFiles(ByVal sFilter As String)
        _VBAssert.OutputText(String.Format("MappedFiles Filter={0}", sFilter))
        Dim q = From alloc In MemSpectSnap.Allocs
                Where alloc.TBlkBlockType = BlockTypes.MapFile
                Select Stack = alloc.GetCallStackAsString,
                       Size = alloc.GetSize,
                       FileName = alloc.GetMappedFilenameFromAllocation()
                       Where FileName.Contains(sFilter)
                       Order By FileName, Stack

        _VBAssert.OutputText("Got # MappedFiles items = " + q.Count.ToString)
        For Each item In q
            'Mapped file 24576 \Device\HarddiskVolume3\Windows\Registration\R00000000000a.clb
            'Pivot Size=24576 MapFile \Device\HarddiskVolume3\Windows\Registration\R00000000000a.clb MemSpect

            Dim fname = item.FileName
            If Not String.IsNullOrEmpty(fname) Then
                '{ Address = 0x00000000, File = R00000000000c.clb, Path = C:\Windows\Registration, SeqNo = XXXX, Instance = , SizeOnDisk = 23700, VMSize = 24576, VMWaste = 40960, ImageType = Native, code = 0, data = 0, rsrc = 0, reloc = 0, Sections = , ImageBasemem = 00000000, ImageBasePE = 00000000, Relocated = 0, _HeapAllocationContainer = R00000000000c.clb, C:\Windows\Registration, DiskSize=23700, Sections =  }
                fname = RegularExpressions.Regex.Replace(fname, "R00000000000[0-9a-z].clb", "R00000000000b.clb") '
                If fname.Contains("\Device\HarddiskVolume3\Windows\Registration\R") Then
                    fname = "\Device\HarddiskVolume3\Windows\Registration\RXXXXXXXXX.clb"
                End If
                CleanAndSortStacksForBaseline(String.Format("Mapped file {0} {1}", item.Size, fname), {item.Stack})

            End If
        Next
    End Sub

    Protected Sub DoTestHeapCreates(Optional ByVal strIncludes As String() = Nothing)
        Dim q = From alloc In MemSpectSnap.Allocs
                Where alloc.TBlkBlockType = BlockTypes.HeapCreate
                Select Stack = alloc.GetCallStackAsString,
                       HeapName = alloc.GetHeapNameForHeapCreate
                       Order By HeapName, Stack

        For Each item In q
            Dim fInclude = True
            If strIncludes IsNot Nothing Then
                fInclude = False
                For Each tryinclude In strIncludes
                    If item.HeapName.Contains(tryinclude) Then
                        fInclude = True
                        Exit For
                    End If
                Next
            End If
            If fInclude Then
                CleanAndSortStacksForBaseline(String.Format("HeapCreate {0}", item.HeapName), {item.Stack})
            End If
        Next
        _VBAssert.OutputText("", fAddToBaseline:=False)
    End Sub

    Protected Sub DoTestProcessHeap(ByVal strIncludes As String())
        Dim q = From alloc In ProcessHeapSnap.Allocs
                Select Stack = alloc.GetCallStackAsString
                Select Stack,
                incl = (Function() As String
                            For Each strin In strIncludes
                                If Stack.Contains(strin) Then
                                    Return strin
                                End If
                            Next
                            Return ""
                        End Function).Invoke
                Where incl <> ""
                Order By incl, Stack

        For Each item In q
            CleanAndSortStacksForBaseline("Process Heap", {item.Stack})
        Next
        _VBAssert.OutputText("", fAddToBaseline:=False)
    End Sub

    Protected Sub DoTestVirtualMemory(Optional ByVal strIncludes As String() = Nothing, Optional ByVal strExcludes As String() = Nothing)
        If _ConnectionMode = MemSpectMode.Offline Then
            Return
        End If
        Dim vmStats = VirtualMem.VMStatistics.GetVirtualMemorystats()
        Dim vmsnapshot = New SortedList(Of IntPtr, MEMORY_BASIC_INFORMATION)(GetVirtAllocs, _CompareIntPtr)

        Dim q = From a In vmsnapshot.Values
                Select
                        Filename = GetFileNameFromMBI(a),
                        AllocationProtect = CType(a.AllocationProtect, AllocationProtect).ToString(),
                        State = CType(a.State, AllocationState).ToString(),
                        Type = CType(a.lType, AllocationType).ToString,
                        _mbi = a
                        Where Not String.IsNullOrEmpty(Filename)
                        Order By Filename

        '        _VBAssert.OutputCurrentText("Virtual Memory mapped files")
        For Each item In q
            Dim fname = item.Filename
            Dim fInclude = True
            If strIncludes IsNot Nothing Then
                fInclude = False
                For Each tryinclude In strIncludes
                    If item.Filename.Contains(tryinclude) Then
                        fInclude = True
                        Exit For
                    End If
                Next
            End If
            If fInclude Then
                '=File=D:\Memspect\VSAssertSrv\TestResults\calvinh_CALVINH9 2010-08-22 23_32_08\Out\Vsassert.dll AllocationProt=PAGE_EXECUTE_WRITECOPY State=MEM_COMMIT Type=MEM_IMAGE
                If fname.ToLower.StartsWith("c") Or fname.ToLower.StartsWith("d") Then
                    If (MemSpectDllName.ToLower + " detoured.dll microsoft.internal.performance.codemarkers.dll ").Contains(IO.Path.GetFileName(item.Filename.ToLower)) Then
                        fname = IO.Path.GetFileName(fname)
                    End If


                End If
                If strExcludes IsNot Nothing Then
                    Array.ForEach(strExcludes, Sub(str As String)
                                                   If fname.ToLower.Contains(str) Then
                                                       fInclude = False
                                                   End If
                                               End Sub)
                End If
                If fname.ToLower.Contains("marta.dll") OrElse fname.ToLower.Contains("wldap32") OrElse fname.ToLower.EndsWith(".mui") Then
                    fInclude = False
                End If
                If fInclude Then
                    _VBAssert.OutputText(String.Format("VMMappedFile={0} AllocationProt={1} State={2} Type={3}", fname, item.AllocationProtect.ToString, item.State.ToString, item.Type.ToString))
                End If
            End If
        Next
        _VBAssert.OutputText("", fAddToBaseline:=False)
    End Sub

    Protected Sub DoTestVMUI(ByVal fManagedToo As Boolean)
        Dim vm = New VirtualMem
        Dim ctrls = vm.ShowVirtualAllocs(New VirtualMem.VMDisplayOptions With {.fShowDetail = True})
        If _ConnectionMode <> MemSpectMode.OnLine AndAlso _ConnectionMode <> MemSpectMode.Existing Then
            Dim spHeaderchildren = CType(ctrls.SurfaceHeader.Children(0), StackPanel).Children


            For Each child In spHeaderchildren
                If TryCast(child, Browse) IsNot Nothing Then
                    Dim brChild = CType(child, Browse)
                    For Each itm In brChild._BrowseList.Items
                        _VBAssert.OutputText(itm.ToString)
                    Next

                End If

            Next
            _VBAssert.OutputText("Free Fragmentation")
            Dim ctrlsFrag = vm.DoShowFreeFragmentation(vm._VMDataDict)
            Dim brFrag = CType(ctrlsFrag.SurfaceDetails.Children(0), Browse)
            For Each itm In brFrag._BrowseList.Items
                _VBAssert.OutputText(itm.ToString)
            Next


        End If
        Dim brVM = CType(ctrls.SurfaceDetails.Children(0), Browse)
        Dim nHeaps = 0
        Dim nFiles = 0
        Dim nStacks = 0
        Dim nManaged = 0
        Dim fDidGetMemSpectStack = False
        Dim fDidGetMainUIThread = False
        _VBAssert.IsTrue(brVM._BrowseList.Items.Count > 220, "Expected > 220 items in VM UI: got " + brVM._BrowseList.Items.Count.ToString)
        For Each itm In brVM._BrowseList.Items
            'VirtualMem._LastTipObj = Nothing
            'Dim eventArg = New RoutedEventArgs(ListViewItem.MouseMoveEvent, brVM._BrowseList)
            'VirtualMem.On_VirtmemMouseMove(brVM._BrowseList, eventArg)
            '_VBAssert.IsTrue(VirtualMem._LastTipObj IsNot Nothing, "no tooltip found for " + itm.ToString)
            Dim datDesc = ComponentModel.TypeDescriptor.GetProperties(itm)("Data")
            Dim dat = CStr(datDesc.GetValue(itm))
            If dat.StartsWith("Heap ") Then
                nHeaps += 1
            ElseIf dat.StartsWith("Stack") Then
                nStacks += 1
                If dat.Contains("MemSpect") Then
                    fDidGetMemSpectStack = True
                ElseIf dat.Contains("Main UI thread") Then
                    fDidGetMainUIThread = True
                End If
            ElseIf dat.StartsWith("Managed") Then
                nManaged += 1
            End If

        Next
        If _ConnectionMode <> MemSpectMode.MiniDumpOnly AndAlso _ConnectionMode <> MemSpectMode.Existing Then
            _VBAssert.IsTrue(nHeaps > 16, "VM Expected nHeaps >16" + " got " + nHeaps.ToString)
            _VBAssert.IsTrue(nStacks >= 6, "VM Expected nStacks >= 6" + " got " + nStacks.ToString)
            If fManagedToo Then
                _VBAssert.IsTrue(nManaged >= 2, "VM Expected nManaged >= 2" + " got " + nManaged.ToString)
            End If
        End If
        If _ConnectionMode = MemSpectMode.OnLine Then
            _VBAssert.IsTrue(fDidGetMemSpectStack, "didn't find MemSpectStack")
        End If

        If _ConnectionMode = MemSpectMode.OnLine OrElse _ConnectionMode = MemSpectMode.Existing Then
            '            _VBAssert.IsTrue(fDidGetMainUIThread, "fDidGetMainUIThread is false?")
        Else
            Dim blistColors = vm._browColors

            For Each colorItem In blistColors._BrowseList.Items
                Dim filt = CStr(ComponentModel.TypeDescriptor.GetProperties(colorItem)("Filter").GetValue(colorItem))
                blistColors._BrowseList.SelectedItem = colorItem
                _VBAssert.OutputText(colorItem.ToString + " #items = " + vm._browVM._BrowseList.Items.Count.ToString)
            Next


        End If

    End Sub

    Protected Sub DoContextMenuItems(ByVal cmn As ContextMenu, ByVal strkind As String, ByVal fTitle As String)
        Dim fDoCtxMenuItems = True
        For Each itm As MenuItem In cmn.Items
            _VBAssert.OutputText(strkind + " ctxm = " + fTitle + " " + itm.Header.ToString)
            Dim cmItemName = itm.Header.ToString.ToLower

            If cmItemName.Contains("notepad") OrElse
                cmItemName.Contains("xcel") Then
                fDoCtxMenuItems = False
            End If
            If cmItemName.Contains("duplicates") AndAlso fTitle.ToLower.Contains("threadinfo") Then
                fDoCtxMenuItems = False
            End If
            If fDoCtxMenuItems Then
                Dim curwin = DataWindowMain._TabControl.SelectedIndex
                'itm.RaiseEvent(New RoutedEventArgs(MenuItem.ClickEvent, itm))
                InvokeContextMenu(cmn, itm.Header.ToString)
                WaitForOtherThreads(_nmSecsDelayBetweenUIDisplay)
                DataWindowMain._TabControl.SelectedIndex = curwin
            End If
        Next
        _GlobalFilter.ClearFilter()  ' in case ctx item changes seqno
    End Sub

    Protected Sub DoSpotCheckHeaps(ByVal actOnSnap As Action(Of MemSnapshot),
                                   ByVal fDoAllocs As Boolean,
                                   Optional ByVal fDumpOut As Boolean = False,
                                   Optional ByVal fDoCtxMenuItems As Boolean = False)
        If _ConnectionMode = MemSpectMode.OnLine Then
            If ProcComm._pipestreamToTarget Is Nothing OrElse Not ProcComm._pipestreamToTarget.IsConnected Then
                _VBAssert.IsTrue(ProcComm._pipestreamToTarget IsNot Nothing AndAlso ProcComm._pipestreamToTarget.IsConnected, "pipe not connected")
            End If
        End If
        _VBAssert.OutputText("DoSpotCheckHeaps", fAddToBaseline:=False)
        FrozenTarget = True
        ReadHeaps()

        'Dim curdir = IO.Directory.GetCurrentDirectory
        'MsgBox("Curdir=" + curdir) ' C:\Users\calvinh\Documents\Visual Studio 2010\Projects\csMemSpectClient\TestResults\calvinh_CALVINH9 2010-08-16 10_57_36\Out
        'MsgBox("asmdir=" + Reflection.Assembly.GetExecutingAssembly.Location)'D:\Memspect\Test\bin\Debug
        Dim lamProcBrowseMem = Sub(bmem As BrowseMem, fTitle As String)
                                   _VBAssert.OutputText("BMem " + fTitle, fAddToBaseline:=False)

                                   WaitForOtherThreads(_nmSecsDelayBetweenUIDisplay)
                                   If bmem._tvPanel._tv.Items.Count > 0 Then
                                       CType(bmem._tvPanel._tv.Items(bmem._tvPanel._tv.Items.Count - 1), TVStackAggPanel.StackAggTreeView.StackAggTViewItem).IsSelected = True
                                       Dim ctxMenuTV = bmem._tvPanel._tv.ContextMenu
                                       If fDumpOut Then
                                           DoContextMenuItems(ctxMenuTV, "StackAgg", fTitle)
                                       End If
                                   End If

                                   bmem._TabControl.SelectedIndex = 1 ' select Details tab
                                   bmem.OnTabItemDetailsGotFocus(bmem._TabItemDetails, New Windows.RoutedEventArgs)
                                   '                bmem._TabControl.SelectedItem = bmem._TabItem2
                                   WaitForOtherThreads(_nmSecsDelayBetweenUIDisplay)
                                   If bmem._DetailBrowse IsNot Nothing Then
                                       Dim itemCnt = bmem._DetailBrowse._BrowseList.Items.Count
                                       If fDoAllocs AndAlso itemCnt > 0 Then
                                           For Each itm In bmem._DetailBrowse._BrowseList.Items
                                               If fDumpOut Then
                                                   _VBAssert.OutputText(itm.ToString)
                                               End If
                                               Dim hctr = HeapAllocationContainer.CreateFrom(itm)
                                               If hctr Is Nothing Then ' like heap density report
                                                   Exit For
                                               End If
                                               If hctr.SpyHeapPtr.IsMemSpectHeap AndAlso
                                                   hctr.TBlkBlockType = BlockTypes.VirtualAlloc AndAlso
                                                   (CType(hctr.TBlk.UnionData2, AllocationState) And AllocationState.MEM_COMMIT) <> AllocationState.MEM_COMMIT Then
                                                   ' if it's only reserved, don't try to read it
                                               Else
                                                   If fDumpOut Then
                                                       Dim prStacksAndDump = GetStacksAndDump(hctr, nMaxDumpSize:=1024 * 1)
                                                       Dim sframes = prStacksAndDump.Key
                                                       _VBAssert.OutputText(sframes, cSplitChar:=CChar(vbCr))
                                                       Dim strAddrDump = prStacksAndDump.Value
                                                       _VBAssert.OutputText(strAddrDump, cSplitChar:=CChar(vbCr))
                                                   End If
                                               End If
                                           Next
                                           bmem._DetailBrowse._BrowseList.SelectedIndex = 1
                                           Dim ctxMenu = bmem._DetailBrowse._BrowseList.ContextMenu
                                           If ctxMenu IsNot Nothing Then
                                               ' select an item before invoking mnu
                                               If Not fTitle.ToLower.Contains("threadinfo") Then
                                                   bmem._DetailBrowse._BrowseList.SelectedIndex = 0
                                                   If fDumpOut Then
                                                       DoContextMenuItems(ctxMenu, "Bmem", fTitle)
                                                   End If
                                               End If
                                           End If
                                       End If
                                   End If
                               End Sub


        Dim h As HeapReportUI
        Dim ctrlsHeapRep As DataSurface
        If Common._ConnectionMode = MemSpectMode.Offline Then
            h = New HeapReportUI()
            Dim tempH = Common._offlineSnapshot.heapReport
            h._totHeapData = tempH._totHeapData
            h._ProcessHeapHandles = tempH._ProcessHeapHandles
            h._ProcessHeapData = tempH._ProcessHeapData
            ctrlsHeapRep = h.ShowResults()

        Else
            h = New HeapReportUI
            ctrlsHeapRep = h.ShowResults()
        End If
        Dim bmemheapRpt = CType(ctrlsHeapRep.SurfaceDetails.Children(0), Browse)

        _VBAssert.IsTrue(bmemheapRpt._BrowseList.Items.Count > 0, "heap report has no items")

        _VBAssert.OutputText("", fAddToBaseline:=False)
        WaitForOtherThreads(_nmSecsDelayBetweenUIDisplay)
        Dim hp = (From heap In _HeapList Where heap.GetHeapName = ProcessHeapName).First
        Dim hrpt = New HeapReportUI
        Dim hwalkmapCtrls = hrpt.DoHeapWalkMapReport(hp.GetRealOSHeapHandle, "title")
        lamProcBrowseMem.Invoke(CType(hwalkmapCtrls.SurfaceDetails.Children(0), BrowseMem), "HeapWalkMap")
        WaitForOtherThreads(_nmSecsDelayBetweenUIDisplay)

        DoTestProcessHeap({
               "dwmapi.dll!McGenEventRegister"
               }) 'EtwEventRegister
        Try
            Dim nHeaps = _HeapList.Count
            For Each hp In _HeapList
                Dim snap = hp.TakeMemSnapshot
                actOnSnap(snap)
                If snap.Allocs.Count > 0 Then
                    Dim ctrls = ShowSnapshot(hp, snap)
                    Dim nCritSects = 0
                    If snap.SpyHeap.GetHeapName = ProcessHeapName Then
                        lamProcBrowseMem.Invoke(CType(ctrls.SurfaceDetails.Children(0), BrowseMem), ProcessHeapName)
                    End If

                    WaitForOtherThreads(_nmSecsDelayBetweenUIDisplay)
                    If hp.IsMemSpectHeap Then
                        Dim amemspectwin = CType(ctrls.SurfaceDetails.Children(0), MemSpectWin)

                        Array.ForEach(amemspectwin._btns, Sub(btn As RadioButton)
                                                              If btn IsNot amemspectwin._btnAll Then
                                                                  btn.IsChecked = True
                                                                  lamProcBrowseMem.Invoke(amemspectwin._bmem, hp.HeapName + " " + btn.Tag.ToString)
                                                              End If
                                                          End Sub)
                    Else
                        '                        lamProcBrowseMem.Invoke(CType(ctrls.SurfaceDetails.Children(0), BrowseMem))
                    End If
                End If
                If DataWindowMain._TabControl.Items.Count > 10 Then 'else out of mem
                    If DataWindowMain._DataWindowMain IsNot Nothing Then
                        '          DataWindowMain._DataWindowMain.Close()
                    End If
                End If
            Next
            'WaitForOtherThreads(5)
        Catch ex As Exception

        End Try
    End Sub


    ''' <summary>
    ''' sorts stacks and cleans for output to baseline
    ''' </summary>
    Protected Sub CleanAndSortStacksForBaseline(ByVal desc As String, ByVal strlist As IEnumerable(Of String))
        _VBAssert.OutputText(desc)

        For Each stk In From s In strlist Order By s
            Dim fDidMemSpectStack = False
            For Each line In stk.Split({CChar(vbCr), CChar(vbLf)}, StringSplitOptions.RemoveEmptyEntries)
                line = CleanLineForBaseline(line)
                If line.StartsWith("d:\memspect") Then
                    If fDidMemSpectStack Then ' don't want 2 in a row for DEBUG bits
                        Continue For
                    End If
                    fDidMemSpectStack = True
                End If
                _VBAssert.OutputText(line, fAddToBaseline:=True, cSplitChar:=CChar(vbCrLf))
                'If line <> "d:\w7rtm\base\win32\client\thread.c(65) : KERNEL32.dll!BaseThreadInitThunk + 14 bytes" Then 'debug/retail
                'End If
            Next
        Next
        _VBAssert.OutputText("", fAddToBaseline:=False)
    End Sub
    ''' <summary>
    ''' sorts stacks and cleans for output to baseline
    ''' </summary>
    Protected Sub CleanStacksForBaseline(ByVal desc As String, ByVal strlist As IEnumerable(Of String))
        _VBAssert.OutputText(desc)
        For Each stk In strlist
            For Each line In stk.Split({CChar(vbCr), CChar(vbLf)}, StringSplitOptions.RemoveEmptyEntries)
                line = CleanLineForBaseline(line)
                If line <> "d:\w7rtm\base\win32\client\thread.c(65) : KERNEL32.dll!BaseThreadInitThunk + 14 bytes" Then 'debug/retail
                    _VBAssert.OutputText(line, fAddToBaseline:=True, cSplitChar:=CChar(vbCrLf))
                End If
            Next
        Next
        _VBAssert.OutputText("", fAddToBaseline:=False)
    End Sub

    Protected Function CleanLineForBaseline(ByVal line As String, Optional ByVal fIsCallStack As Boolean = True) As String
        Dim res = ""
        '=                       1 6,192 d:\memspect\vsassert\trackmem.cpp(1945) : Vsassert.dll!Mine_RtlAllocHeap + 685 bytes
        If fIsCallStack Then
            Dim ndx = line.ToLower.IndexOf(Path.GetFileNameWithoutExtension(MemSpectDllName).ToLower)
            If ndx >= 0 Then
                line = "d:\memspect\ MemSpect"
            End If

            If line.Contains("win7_gdr") Then
                line = line.Replace("win7_gdr", "w7rtm")
            End If

        End If
        res = RegularExpressions.Regex.Replace(line, "0x[0-9A-Fa-f]{8}", "0x00000000")

        res = RegularExpressions.Regex.Replace(res, "Address = [0-9A-Fa-f]{8}", "Address = 0x00000000")

        res = RegularExpressions.Regex.Replace(res, "HGlobal = [0-9A-Fa-f]{8}", "HGlobal = 0x00000000")

        res = RegularExpressions.Regex.Replace(res, "System.String [0-9]+ : [0-9]+", "System.String 00 : 000") ' csLife puts Gen: cnt as ints in window title

        res = RegularExpressions.Regex.Replace(res, "SeqNo\s*=\s*[0-9]+(,[0-9]+)*", "SeqNo=000,000")

        res = RegularExpressions.Regex.Replace(res, "HeapHandle = [0-9A-Fa-f]{8}", "HeapHandle = 0x00000000")

        res = RegularExpressions.Regex.Replace(res, "Thread\s*=\s*[0-9]+", "Thread=0000")

        res = RegularExpressions.Regex.Replace(res, "GCMoved=[0-9]{1}", "GCMoved=X")

        res = RegularExpressions.Regex.Replace(res, "GCSurvived=[0-9]{1}", "GCSurvived=X")

        res = RegularExpressions.Regex.Replace(res,
                                               "(?i)([A-Z])(:\\memspect)",
                                                Function(match) As String
                                                    Dim retval = "d"
                                                    If Char.IsUpper(match.Value(0)) Then
                                                        retval = "D"
                                                    End If
                                                    retval += match.Groups(2).Value
                                                    Return retval
                                                End Function
                                        ) ' "C:\MemSpect"=> "D:\MemSpect", matching case for drive and folder


        'Dim x = New RegularExpressions.Regex("0x[0-9A-Z]", RegularExpressions.RegexOptions.IgnoreCase And RegularExpressions.RegexOptions.Multiline)
        Return res
    End Function

    Protected _strTVItemsToInclude As String()
    Protected _TvItemMaxDepth As Integer = 10
    Protected Sub GetTVItemInfo(ByVal tvitem As TVObjRefPanel.TVObjRef.TVObjRefTVItem, ByVal nDepth As Integer, Optional ByVal fExpandIfNecessary As Boolean = True)
        Dim str = tvitem.ToString
        Dim fInclude = False
        If _strTVItemsToInclude IsNot Nothing Then

            For Each strItem In _strTVItemsToInclude
                If str.Contains(strItem) Then
                    fInclude = True
                    Exit For
                End If
            Next
        End If
        str = CleanLineForBaseline(str)
        Dim sIndent = ""
        For i = 0 To nDepth - 1
            sIndent += " "
        Next
        If fInclude Then
            _VBAssert.OutputText(sIndent + String.Format("{0,-3} {1}", nDepth, str), fAddToBaseline:=fInclude)
        Else
            _VBAssert.OutputText(sIndent + String.Format(" {0,-3} {1}", nDepth, str), fAddToBaseline:=fInclude)
        End If
        'If str.Contains("TRACKMOUSEEVENT") Then
        '    Return
        'End If
        'If str.Contains("ButtonInternal.ButtonStandardAdapter") Then
        '    Return
        'End If
        'If str.Contains("System.EventHandler") Then
        '    Return
        'End If
        'If str.Contains("System.String") Then ' life Generaion/count changes
        '    If tvitem._tvParentItem.ToString.Contains("Life.Form1") Then
        '        str = "System.String  Generation/count"
        '    End If
        'End If
        If nDepth < _TvItemMaxDepth AndAlso fExpandIfNecessary Then
            If Not tvitem._DidtryGettingChildren Then
                If TVObjRefPanel._fCombineFromMeToMe Then
                    If tvitem._nodeType <> NodeType.RefToParent Then
                        tvitem.on_Expand(tvitem, New RoutedEventArgs) ' attempt to expand
                    End If
                Else
                    tvitem.on_Expand(tvitem, New RoutedEventArgs) ' attempt to expand
                End If
            End If
        End If
        For Each item In tvitem.Items ' From anItem In tvitem.Items Order By anItem.ToString ' sort for consistency

            Dim tvobjrefitem = TryCast(item, TVObjRefPanel.TVObjRef.TVObjRefTVItem)
            If tvobjrefitem Is Nothing Then 'dummy tag
                _VBAssert.IsTrue(CStr(CType(item, TreeViewItem).Tag) = "dummy", "dummy treeview tag expected")
            Else
                GetTVItemInfo(tvobjrefitem, nDepth + 1, fExpandIfNecessary) ' recur
            End If
        Next
    End Sub

    Protected Sub DoTestDuplicatesHelper(ByVal dupeSurface As DataSurface, ByVal strDesc As String)
        _VBAssert.OutputText(strDesc, fAddToBaseline:=False)
        Dim bmemDupes = CType(dupeSurface.SurfaceDetails.Children(0), BrowseMem)
        bmemDupes._TabControl.SelectedIndex = 1 ' select Details tab
        bmemDupes.OnTabItemDetailsGotFocus(bmemDupes._TabItemDetails, New Windows.RoutedEventArgs)
        Dim cnt = bmemDupes._DetailBrowse._BrowseList.Items.Count
        For Each dupe In bmemDupes._DetailBrowse._BrowseList.Items
            Dim hctr = HeapAllocationContainer.CreateFrom(dupe)
            If hctr.GetSize > 0 Then
                Dim dupeID = CType(ComponentModel.TypeDescriptor.GetProperties(dupe)("DupeID").GetValue(dupe), Integer)
                Dim dupeIndex = CType(ComponentModel.TypeDescriptor.GetProperties(dupe)("DupeIndex").GetValue(dupe), Integer)
                Dim heap = CType(ComponentModel.TypeDescriptor.GetProperties(dupe)("Heap").GetValue(dupe), String)
                If _ConnectionMode = MemSpectMode.Offline Then
                    _VBAssert.OutputText(dupe.ToString)
                Else
                    _VBAssert.OutputText(String.Format("Dupe #={0} ndx={1} Sz={2} Hp={3}", dupeID, dupeIndex, hctr.GetSize.ToString, heap))
                End If
            End If
        Next

    End Sub

    Protected Function DoTestDuplicates(Optional ByVal inputHeapList As IEnumerable(Of CSpyHeap) = Nothing) As DataSurface
        Dim qHp = inputHeapList

        If inputHeapList Is Nothing Then
            qHp = From a In _HeapList
        End If

        Dim dupeSurface = ShowDuplicateAllocations(qHp, Nothing, "")

        DoTestDuplicatesHelper(dupeSurface, "Duplicates in all heaps")

        Dim qAllocs = From a In ProcessHeapSnap.Allocs

        dupeSurface = ShowDuplicateAllocations(Nothing, qAllocs, "")
        DoTestDuplicatesHelper(dupeSurface, "Duplicates in __Process Heap")
        Return dupeSurface
    End Function

    Protected Sub DoTestStackAgg(ByVal tvPanel As TVStackAggPanel)
        _VBAssert.OutputText("StackAgg panel")
        Dim tv = tvPanel._tv
        For Each item As TVStackAggPanel.StackAggTreeView.StackAggTViewItem In tv.Items
            DoStackAggNode(item, 0)
        Next

        _VBAssert.OutputText("StackAgg panel sort by name")
        tvPanel.on_SortNameSizeCnt(tvPanel._btnSortName, New Windows.RoutedEventArgs)
        For Each item As TVStackAggPanel.StackAggTreeView.StackAggTViewItem In tv.Items
            DoStackAggNode(item, 0)
        Next


    End Sub

    Private Sub DoStackAggNode(ByVal node As TVStackAggPanel.StackAggTreeView.StackAggTViewItem, ByVal nDepth As Integer)
        Dim sIndent = ""
        For i = 0 To nDepth - 1
            sIndent += " "
        Next
        Dim str = CleanLineForBaseline(node.ToString)
        Dim fAddToBaseline = True
        If str.Contains("d:\memspect\ MemSpect") Then
            fAddToBaseline = False ' debug/retail
        End If
        _VBAssert.OutputText(sIndent + str, fAddToBaseline:=fAddToBaseline)
        If Not node.IsExpanded Then
            node.IsExpanded = True
        End If
        For Each child In node.Items
            DoStackAggNode(CType(child, TVStackAggPanel.StackAggTreeView.StackAggTViewItem), nDepth + 1)
        Next

    End Sub

    Protected Sub DoTestThreads()
        Dim ctrls = UIBase.DoShowThreads
        Dim b = CType(ctrls.SurfaceDetails.Children(0), Browse)
        _VBAssert.OutputText("DoShowThreads: # threads = " + b._BrowseList.Items.Count.ToString, fAddToBaseline:=False)
        _VBAssert.OutputText("DoShowThreads: # threads > 15 " + If(b._BrowseList.Items.Count > 15, "Yes", "No"))

    End Sub

    Protected Sub DoTestPivot(ByVal strFrame As String)

#If False Then
        "MSCTF.dll!ActivateAssembly + 133 bytes"
        Address = 0x00230000  Size = 4,096  SeqNo = 1,182 Thread = 14776 Heap = __MemSpect  BlockType = VirtualAlloc  ReqAddress = 00230000  MemType = 00001000
Call Stack:
d:\memspect\vsassert\trackmem.cpp(1400) : vsassert.dll!Mine_ZwAllocateVirtualMemory + 333 bytes

#End If
        Dim q = From alloc In MemSpectSnap.Allocs
                Where alloc.TBlkBlockType = BlockTypes.VirtualAlloc AndAlso
                    alloc.GetSize >= 65536
                    Select Stack = alloc.GetCallStackAsString, alloc
                    Where Stack.Contains(strFrame)
                    Order By Stack

        _VBAssert.OutputText("Pivot addr = " + strFrame + " #Stacks = " + q.Count.ToString)

        If q.Count = 0 Then
            _VBAssert.OutputText("No pivot item found for " + strFrame)
        Else

            For Each item In q
                CleanAndSortStacksForBaseline("Pivot " + strFrame, {item.Stack})
            Next
            Dim nDepth = 0
            Dim stklines = q.First.Stack.Split({CChar(vbCr)})
            While Not stklines(nDepth).Contains(strFrame)
                nDepth += 1
            End While
            Dim ctrls = BrowseMem.Pivot(q.First.alloc, CType(nDepth, IntPtr), "Pivot " + strFrame)
            Dim bmem As BrowseMem = CType(ctrls.SurfaceDetails.Children(0), BrowseMem)
            DoTestStackAgg(bmem._tvPanel)


            bmem._TabControl.SelectedIndex = 1 ' select Details tab
            bmem.OnTabItemDetailsGotFocus(bmem._TabItemDetails, New Windows.RoutedEventArgs)
            For Each itm In bmem._DetailBrowse._BrowseList.Items
                Dim hctr = HeapAllocationContainer.CreateFrom(itm)
                If hctr IsNot Nothing Then
                    Dim blkType = hctr.TBlkBlockType
                    If ComponentModel.TypeDescriptor.GetProperties(itm)("Data") IsNot Nothing Then
                        Dim strData = String.Empty
                        If blkType <> BlockTypes.None Then
                            strData = CStr(ComponentModel.TypeDescriptor.GetProperties(itm)("Data").GetValue(itm))
                        End If
                        Dim heapName As String = CStr(ComponentModel.TypeDescriptor.GetProperties(itm)("Heap").GetValue(itm))

                        _VBAssert.OutputText(String.Format("Pivot Size={0} {1} {2} {3}",
                                                           hctr.GetSize,
                                                           blkType.ToString,
                                                           CleanLineForBaseline(strData),
                                                           heapName))

                    End If
                    'CleanStacksForBaseline("Pivot", {hctr.GetCallStack})
                End If
            Next
        End If

    End Sub

    Protected Sub DoTestSymbolDictionary(ByVal nMustBeGreaterThan As Integer)
        Dim symdictctrls = UIBase.ShowSymbolDictionary
        Dim b As Browse = CType(symdictctrls.SurfaceDetails.Children(0), Browse)
        _VBAssert.OutputText("Symbol dictionary # items = " + b._BrowseList.Items.Count.ToString, fAddToBaseline:=False)
        _VBAssert.OutputText("Symbol dictionary # items must be greater than " + nMustBeGreaterThan.ToString + "  " + If(b._BrowseList.Items.Count > nMustBeGreaterThan, "true", "false"))
    End Sub

    Protected Sub ProcessAddressDump(ByVal dump As String)
        Dim nBaseAddr = 0
        For Each line In dump.Split({CChar(vbCr), CChar(vbLf)})
            If line.Length > 10 Then
                _VBAssert.OutputText(nBaseAddr.ToString("x8") + " " + line.Substring(10))
                nBaseAddr += 16
            End If
        Next

    End Sub

    Protected Sub DoTestAddressTip(ByVal spTip As StackPanel, Optional ByVal nChildOffset As Integer = 0)
        'GetAddressToolTip
        If spTip Is Nothing Then
            _VBAssert.OutputText("No tip " + ((New StackTrace).GetFrames(0).GetMethod.Name))
        Else
            Dim tbxStack = CType(spTip.Children(0 + nChildOffset), TextBlock)
            Dim sStack = GetTextFromTextBlock(tbxStack)
            _VBAssert.OutputText(sStack, cSplitChar:=CChar(vbCr), fAddToBaseline:=False)
            Dim tbxDump = CType(spTip.Children(2 + nChildOffset), TextBox)
            _VBAssert.OutputText(tbxDump.Text, cSplitChar:=CChar(vbCr), fAddToBaseline:=False)
            _VBAssert.IsTrue(sStack.Length > 200, "stack len < 200 chars? " + tbxStack.Text.Length.ToString)
            _VBAssert.IsTrue(tbxDump.Text.Length > 200, "dump len < 200 chars? " + tbxDump.Text.Length.ToString)
            _VBAssert.OutputText(((New StackTrace).GetFrames(0).GetMethod.Name) + " Success")

            'CleanStacksForBaseline("Address tip ", {tbxStack.Text})
            'ProcessAddressDump(tbxDump.Text)

        End If
    End Sub

    Protected Sub DoTestUnusedMembers(ByVal allocs As IEnumerable(Of HeapAllocationContainer), ByVal strDesc As String)
        _VBAssert.OutputText("UnusedMembers: " + strDesc + " # allocs = " + allocs.Count.ToString)

        Dim ctrlsUnused = ShowUnusedMembers(allocs, "test")

        Dim brUnused = CType(ctrlsUnused.SurfaceDetails.Children(0), Browse)
        _VBAssert.OutputText("Unused item cnt = " + brUnused._BrowseList.Items.Count.ToString)

        For Each itm In brUnused._BrowseList.Items
            _VBAssert.OutputText(itm.ToString)
        Next

    End Sub

    Protected Sub DoTestGCStackInfo()

        Dim gcAllocs = From halloc In MemSpectSnap.Allocs
           Where halloc.TBlkBlockType = BlockTypes.TrackGCStacks
           Select halloc
        _VBAssert.IsTrue(gcAllocs.Count > 1, "Didn't get GCStacks cnt > 1. Got  " + gcAllocs.Count.ToString)

    End Sub


    Protected Sub DoClassLayout(ByVal halloc As HeapAllocationContainer,
                                ByVal fOutput As Boolean,
                                Optional ByVal fCleanLineForBaseline As Boolean = True)
        Dim res = GetStacksAndDump(halloc)
        If fOutput Then
            For Each line In res.Key.Split({CChar(vbCr), CChar(vbLf)}, StringSplitOptions.RemoveEmptyEntries)
                Dim fAddToBaseline = True
                If line.Contains("Address=") OrElse line.StartsWith("GCGen =") Then
                    fAddToBaseline = False
                End If
                If line.StartsWith("Call Stack:") Then
                    Exit For
                End If
                If fAddToBaseline Then
                    Dim linetooutput = line
                    If fCleanLineForBaseline Then
                        linetooutput = CleanLineForBaseline(line)
                    End If
                    _VBAssert.OutputText(linetooutput, fAddToBaseline)
                End If
            Next

        End If

    End Sub

    Protected Sub DoTestImages()

        Dim vm = New VirtualMem
        vm.ShowVirtualAllocs(New VirtualMem.VMDisplayOptions With {.fShowDetail = True})
        Dim browVM = vm._browVM

        Dim imgui = New ImageUi(vm)
        vm._browVM._BrowseList.Items.Filter = Nothing ' no filter for images
        vm._imageAnalyzer = Images.ImageAnalyzer.CreateImageAnalyzer()

        Dim ctrlsImages = imgui.DoShowImageData(
            vm._imageAnalyzer,
            vm._imageAnalyzer._lstImageSymbolNames,
            vm._imageAnalyzer._ImageResourcesDict,
            vm._imageAnalyzer._lstImageData)

        ' query list of lists for duplicate rsrcs
        Dim qr = From dictEntryListOfLists In vm._imageAnalyzer._ImageResourcesDict
                 Where dictEntryListOfLists.Value.Count > 1
                 Select
                     dictEntryListOfLists.Value,
                     dictEntryListOfLists.Key

        'now query list of lists
        Dim qdupes = From dictEntry In qr
                     Select
                        dictEntry.Key,
                        dictEntry.Value.Count,
                        _ResList = dictEntry.Value
                        Order By Key

        For Each dupe In qdupes
            _VBAssert.OutputText(String.Format("Dupe Cnt = {0}, Key = {1:x8}", dupe.Count, dupe.Key), fAddToBaseline:=False)

            For Each rsrcData In From dup In dupe._ResList Order By dup.ToString
                Dim strdat = rsrcData.ToString
                Dim fAddToBase = True
                If strdat.IndexOf("path=Global", StringComparison.OrdinalIgnoreCase) >= 0 OrElse
                    strdat.IndexOf("path= Imgtype", StringComparison.OrdinalIgnoreCase) >= 0 Then
                    fAddToBase = False
                End If

                _VBAssert.OutputText(String.Format("{0:x8} {1}", dupe.Key, strdat), fAddToBaseline:=fAddToBase)

            Next

        Next


        Dim bmemImgs = CType(ctrlsImages.SurfaceDetails.Children(0), BrowseMem)
        bmemImgs.OnTabItemDetailsGotFocus(bmemImgs._TabItemDetails, New RoutedEventArgs)

        Dim excludelist = {
            "msctf.dll",
            "memspect",
            "cryptbase.dll",
            "msvcp100.dll",
            "msvcr100.dll",
            "msvcp110.dll",
            "msvcr110",
            "msvcp120.dll",
            "msvcr120",
            "sspicli.dll",
            "mscorlib.ni.dll",
            "detoured.dll",
            "microsoft.internal.performance",
            "theme"
        }
        '{ Address = 0x00000000, File = Theme2036706413, Path = \Windows, SeqNo = XXXX, Instance = , SizeOnDisk = 0, LrgstConsecZeros = XXXX, VMSize = 983040, VMWaste = 0, ImageType = Native, RTVersion = , OptimizationInfo = None, OptPartialNGen = , LangIDs = , code = 0, data = 0, rsrc = 0, reloc = 0, Sections = , ImageBasePE = 00000000, DynamicBase = 0, Relocated = 0, _HeapAllocationContainer = Theme2036706413, \Windows, DiskSize=0, Sections =  }

        Dim lamInclude = Function(line As String) As Boolean
                             Dim fInclude = True
                             If line.Contains("ASqmManifest") OrElse line.Contains("Cor_Private") OrElse line.Contains("Cor_Public") Then
                                 fInclude = False
                             Else
                                 For Each excl In excludelist
                                     If line.IndexOf(excl, StringComparison.OrdinalIgnoreCase) >= 0 Then
                                         fInclude = False
                                         Exit For
                                     End If
                                 Next
                             End If
                             Return fInclude
                         End Function

        Dim lamShowImageList = Sub(Imgitems As ItemCollection)
                                   For Each itm In Imgitems
                                       Dim line = itm.ToString
                                       If lamInclude(line) Then
                                           line = RegularExpressions.Regex.Replace(line, "ImageBaseMem = [0-9A-Fa-f]{8}", "ImageBasemem = 00000000")
                                           line = RegularExpressions.Regex.Replace(line, "ImageBasePEMem = [0-9A-Fa-f]{8}", "ImageBasemem = 00000000")
                                           line = RegularExpressions.Regex.Replace(line, "R00000000000[0-9a-z].clb", "R00000000000b.clb") '
                                           line = RegularExpressions.Regex.Replace(line, "SeqNo = [0-9]+", "SeqNo = XXXX")
                                           line = RegularExpressions.Regex.Replace(line, "LrgstConsecZeros = [-?0-9]+", "LrgstConsecZeros = XXXX")

                                           _VBAssert.OutputText(CleanLineForBaseline(line))

                                       End If
                                   Next

                               End Sub

        lamShowImageList.Invoke(bmemImgs._DetailBrowse._BrowseList.Items)

        _VBAssert.OutputText("Dumping Image subsnap")
        Dim lstImgs = From a In vm._imageAnalyzer._lstImageData.Values
                      Take 5


        Dim bmemSubSnap = imgui.ShowImageAllocs(lstImgs.ToList)

        bmemSubSnap.OnTabItemDetailsGotFocus(bmemSubSnap._TabItemDetails, New RoutedEventArgs)
        lamShowImageList.Invoke(bmemSubSnap._DetailBrowse._BrowseList.Items)

        _VBAssert.OutputText("Resources")
        bmemImgs._TabControl.SelectedIndex = 1 ' details

        Dim tabRResources =
            CType((
                    From titem In bmemImgs._TabControl.Items
                    Where CStr(CType(titem, TabItem).Tag) = "Resources"
                    ).FirstOrDefault, 
                TabItem)
        tabRResources.RaiseEvent(New Windows.RoutedEventArgs With {.RoutedEvent = TabItem.GotFocusEvent})
        Dim browResources = CType(tabRResources.Content, Browse)
        Dim lamShowItems = Sub(br As Browse)
                               Dim nCnt = 0
                               For Each itm In br._BrowseList.Items
                                   nCnt += 1
                                   Dim line = itm.ToString
                                   If lamInclude(line) Then
                                       line = RegularExpressions.Regex.Replace(line, "HGlobal = [0-9A-Fa-f]{8}", "HGlobal = 0x00000000")
                                       If line.IndexOf(MemSpectDllName, StringComparison.OrdinalIgnoreCase) < 0 Then
                                           If line.IndexOf("dwmapi.dll", StringComparison.OrdinalIgnoreCase) < 0 Then
                                               _VBAssert.OutputText(line, fAddToBaseline:=True)
                                           End If
                                       End If
                                   End If
                               Next
                               _VBAssert.OutputText("Resource count = " + br._BrowseList.Items.Count.ToString, fAddToBaseline:=False)
                               _VBAssert.IsTrue(br._BrowseList.Items.Count > 70, "Expected > 70 resource items, got " + br._BrowseList.Items.Count.ToString)
                               For Each ctxm As MenuItem In br._BrowseList.ContextMenu.Items
                                   _VBAssert.OutputText("Context menu item " + ctxm.Header.ToString)
                               Next
                           End Sub

        lamShowItems.Invoke(browResources)

        Dim lvitm = browResources._BrowseList.Items(1)
        Dim rdata = CType(
                ComponentModel.
                TypeDescriptor.
                GetProperties(
                    lvitm
                  )("_ResourceData").
                  GetValue(lvitm), 
    ResourceData)

        _VBAssert.OutputText("Resource bin dump")
        Dim str = ResourceData.GetMemoryDumpOfResourceAsString(rdata)
        _VBAssert.OutputText(str, cSplitChar:=CChar(vbCr))

        _VBAssert.OutputText("now check res dupes")
        Dim tabDupeResources =
                CType((
                    From titem In bmemImgs._TabControl.Items
                    Where CStr(CType(titem, TabItem).Tag) = "DupeResources"
                    ).FirstOrDefault, 
                TabItem)
        tabDupeResources.RaiseEvent(New RoutedEventArgs With {.RoutedEvent = TabItem.GotFocusEvent})

        Dim bmemDupes = CType(tabDupeResources.Content, Browse)

        _VBAssert.OutputText("Dupe res count =" + bmemDupes._BrowseList.Items.Count.ToString)

        For i = 1 To 10
            browResources._BrowseList.SelectedItems.Add(browResources._BrowseList.Items(i))
            '            CType(bmemResources._BrowseList.Items(i), ListViewItem).IsSelected = True
        Next

        imgui.ShowDupeSubset(browResources)

        Dim tabDupeSubset =
                CType((
                    From titem In bmemImgs._TabControl.Items
                    Where CStr(CType(titem, TabItem).Tag) = "DupeResourcesSubSnap"
                    ).FirstOrDefault, 
                TabItem)
        tabDupeSubset.RaiseEvent(New RoutedEventArgs With {.RoutedEvent = TabItem.GotFocusEvent})
        Dim bmemSubset = CType(tabDupeSubset.Content, Browse)
        lamShowItems.Invoke(bmemSubset)

        _VBAssert.OutputText("ImageSymbolNames")
        Dim tabSymNames =
                CType((
                    From titem In bmemImgs._TabControl.Items
                    Where CStr(CType(titem, TabItem).Tag) = "ImageSymbolNames"
                    ).FirstOrDefault, 
                TabItem)

        tabSymNames.RaiseEvent(New RoutedEventArgs With {.RoutedEvent = TabItem.GotFocusEvent})
        Dim bmemImageSymbolNames = CType(tabSymNames.Content, Browse)
        For Each itm In bmemImageSymbolNames._BrowseList.Items
            Dim symname = CType(ComponentModel.TypeDescriptor.GetProperties(itm)("Symbol").GetValue(itm), String)
            If symname.ToLower = "a" Then ' out of memory if we do all
                Dim lstFiles = CType(ComponentModel.TypeDescriptor.GetProperties(itm)("Files").GetValue(itm), List(Of ImageSymboldata))
                Dim fAddTobaseline = True
                For Each File In lstFiles
                    If File.imagedat.Filename = MemSpectDllName Then
                        fAddTobaseline = False
                        Exit For
                    End If
                Next
                If fAddTobaseline Then
                    _VBAssert.OutputText(itm.ToString)
                    For Each File In lstFiles
                        _VBAssert.OutputText("  " + File.imagedat.Filename)
                    Next
                End If
            End If
        Next

    End Sub

    Public Sub DumpTVItemChildren(ByVal strDesc As String, ByVal tvitem As MyTreeViewItem, Optional fShowSelected As Boolean = False)
        If Not String.IsNullOrEmpty(strDesc) Then
            _VBAssert.OutputText(strDesc)
        End If
        Dim sb As New StringBuilder
        tvitem.DumpChildren(sb, 0, fShowSelected)
        _VBAssert.OutputText(sb.ToString, cSplitChar:=CChar(vbCr))
    End Sub

    Public Sub DoCompileTest(Optional nIter As Integer = 5)
        Dim fIncludeMemSpectBaseRef = String.Empty
        'fIncludeMemSpectBaseRef = "//"
        'nIter = 1
        For iter = 1 To nIter
            UpdateStatusMsg(String.Format("Doing iter {0}", iter))
            Dim txt =
    <xml>
// can add the fullpath to an assembly for reference like so:
//Ref: System.dll
//Ref: System.linq.dll
//Ref: System.core.dll
<%= fIncludeMemSpectBaseRef %>//Ref: MemSpectBase.dll
<%= fIncludeMemSpectBaseRef %>using MemSpect;
using System;

namespace DoesntMatter
{
    public class SomeClass
    {
        public static string DoMain(string[] args)
        {
            AppDomain.CurrentDomain.AssemblyResolve += (or, er) =>
            {
                System.Reflection.Assembly assembly = null;
                var filename = System.IO.Path.Combine(@"c:\MemSpect\", er.Name);
                if (System.IO.File.Exists(filename))
                {
                    assembly = System.Reflection.Assembly.LoadFrom(filename);
                }
                return assembly;
            };
<%= fIncludeMemSpectBaseRef %>            Common.UpdateStatusMsg("Executing in dynamically generated code: In Main<%= iter %>");
            var x = <%= iter %>;
            var y = 100/x;
            var res = string.Format("Did Main <%= iter %> in thread {0} IntPtr.Size = {1}", System.Threading.Thread.CurrentThread.ManagedThreadId, IntPtr.Size);

            return res;
        }

    }
}

</xml>.Value.Replace(vbLf, vbCr + vbLf)
            Dim codeFileName = WriteOutputToTempFile(txt)
            Dim res = CompileAndExecuteFileInTarget(codeFileName)
            UpdateStatusMsg(res)
            '            Dim res = CompileAndExecuteFile(codeFileName)
        Next

    End Sub


End Class

<TestClass()>
<CLSCompliant(False)>
Public Class DevEnv
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
    '#Critsects=  2165

    <TestMethod(),
    Description("DevEnv")>
    Public Sub VSGhost()
        Try
            InitTestMethodAndSetBaseLine()
            _ProcessLauncher = New ProcessLauncher() With {
                ._pidTargetProcess = 0,
                ._nmsecsToWaitTilStart = 5000,
                .FreezeAtStartup = 1
            }
            StartVS()

            _GhostData.StartTracking()

            FrozenTarget = False ' 
            System.Threading.Thread.Sleep(5000)  ' let proc continue execution

            _GhostData.StopTracking()

            FrozenTarget = True
            Dim ctrls = UIBase.DoShowGhostAllocs()
            Dim bmem As BrowseMem = CType(ctrls.SurfaceDetails.Children(0), BrowseMem)
            bmem._TabControl.SelectedIndex = 1 ' select Details tab
            bmem.OnTabItemDetailsGotFocus(bmem._TabItemDetails, New Windows.RoutedEventArgs)
            Dim items = bmem._DetailBrowse._BrowseList.Items
            _VBAssert.OutputText("Ghost # items = " + items.Count.ToString, fAddToBaseline:=False)
            _VBAssert.OutputText("Ghost # items  > 10 = " + If(items.Count > 10, True, False).ToString)
            For Each lineitem In items
                Dim hctr = HeapAllocationContainer.CreateFrom(lineitem)
                _VBAssert.OutputText("Ghost " + hctr.ToString + " " + hctr.SpyHeapPtr.HeapName + " WhenFreed =" + hctr.GhostAlloc.SeqNoWhenFreed.ToString, fAddToBaseline:=False)
            Next
            FrozenTarget = False

            _ProcessLauncher.ShutDownTargetProcess()

        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub

    <TestMethod(),
    Description("DevEnv")>
    Public Sub VSNormal()
        Try

            InitTestMethodAndSetBaseLine("VS")
            _ProcessLauncher = New ProcessLauncher() With {
                ._pidTargetProcess = 0,
                ._nmsecsToWaitTilStart = 5000
            }
            VSStartScenario()
            Dim vallocs = GetVirtAllocs().Values
            Dim lastmbi = vallocs(vallocs.Count - 1)
            Dim lastaddr = lastmbi.BaseAddress.ToInt32.ToString("x8")

            '==>VSNormal IsTrue failed LastVirtualAlloc not 'fffe0000'. Got 7ffe1000
            Dim IsrunningUnderWow64 = False

            If IsWow64Process(GetCurrentProcess(), IsrunningUnderWow64) AndAlso IsrunningUnderWow64 Then
                _VBAssert.IsTrue(lastaddr = "fffe0000", "LastVirtualAlloc not 'fffe0000'. Got " + lastaddr)

            End If

            _VBAssert.OutputText("Last Virtual Alloc = " + lastmbi.BaseAddress.ToInt32.ToString("x8"), fAddToBaseline:=False)



        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub



    <TestMethod(),
    TestCategory("codemarker"),
    TestCategory("online"),
    Description("test code marker")>
    Public Sub VSMarkerEvents()
        Try
            InitTestMethodAndSetBaseLine()
            AddHandler StatusMessageEvent, AddressOf TheStatusHandler
            _ProcessLauncher = New ProcessLauncher() With {
                ._pidTargetProcess = 0,
                .TrackClrObjects = 0,
                .FreezeAtStartup = 0,
                .fMessageBoxOnStart = 0
            }
            Dim perfVSPrimeEditorBegin = 7191
            Dim perfVSPrimeEditorEnd = 7192
            Dim perfVSPrimeCLRNotScheduled = 7097
            Dim perfVSPrimeEditorNotScheduled = 7193
            Dim perfVSLoadUIBegin = 7007
            Dim perfVSLoadUIEnd = 7008

            _ProcessLauncher.LaunchTargProc(ProcessLauncher.GetDevEnvFullPath, fWithDll:=True, wrkdir:=TestContext.DeploymentDirectory)

            For i = 1 To 3
                ' timing: between unfreeze and WaitForCodeMarker, not enough time between Prime events..
                _VBAssert.OutputText("WaitForCodeMarkers Iter " + i.ToString)
                Dim res = WaitForCodeMarker(
                    nMsecsTimeout:=50000,
                    fFreezeWhenHit:=True,
                    MarkerIds:={
                        _PerfIdleCodeMarker, perfVSLoadUIBegin, perfVSLoadUIEnd
                        }
                    )
                If res = 0 Then
                    _VBAssert.OutputText("didn't get WaitForCodeMarker: timedout")
                    _VBAssert.IsTrue(Not ProcComm._isTargetFrozen, "should not be frozen")
                Else
                    Select Case res
                        Case perfVSPrimeEditorBegin
                            _VBAssert.OutputText("WaitForCodeMarker got  perfVSPrimeEditorBegin", fAddToBaseline:=False)
                        Case perfVSPrimeEditorEnd
                            _VBAssert.OutputText("WaitForCodeMarker got  perfVSPrimeEditorEnd", fAddToBaseline:=False)
                        Case _PerfIdleCodeMarker
                            _VBAssert.OutputText("Got perf PerfIdleCodeMarker ")
                        Case perfVSPrimeEditorNotScheduled
                            _VBAssert.OutputText("WaitForCodeMarker got  perfVSPrimeEditorNotScheduled", fAddToBaseline:=False)
                        Case perfVSPrimeCLRNotScheduled
                            _VBAssert.OutputText("WaitForCodeMarker got  perfVSPrimeCLRNotScheduled", fAddToBaseline:=False)
                        Case perfVSLoadUIBegin
                            _VBAssert.OutputText("Got perf perfVSLoadUIBegin ")
                        Case perfVSLoadUIEnd
                            _VBAssert.OutputText("Got perf perfVSLoadUIEnd ")
                        Case Else
                            _VBAssert.OutputText("WaitForCodeMarker got something weird " + res.ToString)

                    End Select
                    _VBAssert.IsTrue(ProcComm._isTargetFrozen, "should be frozen")
                    '                    System.Threading.Thread.Sleep(2000)
                    ProcComm.UnFreezeTarget()

                End If


            Next

        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub

    <TestMethod()>
    <TestCategory("offline")>
    <Description("test concurrentdict count")>
    Sub VSConcurrentDict()
        Try
            InitTestMethodAndSetBaseLine()
            LoadOffLineSnap(IO.Path.Combine(memspectInstallDir, "Snaps\VSStart"), fWaitTilClrLoadDone:=True)
            _VBAssert.OutputText("Loaded vs snap")

            _VBAssert.OutputText("Concurrent dictionary in VS Offline")
            Dim q = From alloc In MemSpectSnap.Allocs
                    Where alloc.TBlkBlockType = BlockTypes.ClrObject AndAlso
                    alloc.GetSize = 36 AndAlso
                    (alloc.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False).
                     Contains("System.Collections.Concurrent.ConcurrentDictionary`2<"))
                    Order By alloc.AllocationStruct.SeqNo


            ' ConcurrentDictionary is size 36 on both Dev10 and Dev11.
            MemSpectWin._btnLastSelected = TrkType.ClrObjects
            Dim ctrls = ShowSubSnapShot(q.ToList, "Concurrent")
            Dim mspectWin As MemSpectWin = CType(ctrls.SurfaceDetails.Children(0), MemSpectWin)
            Dim bmem = mspectWin._bmem
            bmem._TabControl.SelectedIndex = 1 ' select Details tab
            bmem.OnTabItemDetailsGotFocus(bmem._TabItemDetails, New Windows.RoutedEventArgs)
            Dim nItems = bmem._DetailBrowse._BrowseList.Items.Count
            _VBAssert.OutputText("# items = " + nItems.ToString)
            Dim ncnt = 0
            For Each itm In bmem._DetailBrowse._BrowseList.Items
                ncnt += 1
                Dim hctr = HeapAllocationContainer.CreateFrom(itm)
                _VBAssert.OutputText("Item #" + ncnt.ToString)
                _VBAssert.OutputText(hctr.ToString, fAddToBaseline:=True, cSplitChar:=CChar(vbCr))
                _VBAssert.OutputText(hctr.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True), fAddToBaseline:=True, cSplitChar:=CChar(vbCr))
                _VBAssert.OutputText(hctr.GetClrClassMemberLayout(fIncludeClassesWithNoMembers:=True), fAddToBaseline:=True, cSplitChar:=CChar(vbCr))
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

            _VBAssert.OutputText("Looking at loaded resources (in VS because Notepad and Life have 0")
            Dim loadedres = MemSpectSnap.Allocs.Where(Function(h) h.TBlkBlockType = BlockTypes.LoadResource).OrderBy(Function(h) h.AllocationStruct.SeqNo)

            For Each res In loadedres
                _VBAssert.OutputText(res.ToString)
            Next

        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub


    <TestMethod(),
    TestCategory("codemarker"),
    Description("test code marker")>
    Public Sub VSCodeMarker()
        Try
            InitTestMethodAndSetBaseLine()
            '; 9261 perfVSLocalRegistryCreateInstanceBegin
            '; 9262 perfVSLocalRegistryVsLoaderCoCreateInstance

            _ProcessLauncher = New ProcessLauncher() With {
                .TrackClrObjects = 0,
                ._nmsecsToWaitTilStart = 5000,
                .fMessageBoxOnStart = 0,
                .CodeMarkersToCollectStats = "9261,9262"
            }

            StartVS()

            _VBAssert.OutputText("CodeMarkerActions")
            Dim lstActions = GetCodeMarkerActions()
            For Each act In lstActions
                _VBAssert.OutputText(act.ToString)
            Next

            Dim stats = GetMemStats()
            For Each stat In stats
                _VBAssert.OutputText(String.Format("{0:n0} {1} {2} Heap={3} Clrobjs={4}",
                                                   stat.SeqNo,
                                                   stat.CodeMarkerId,
                                                   GetCodeMarkerNameRaw(stat.CodeMarkerId),
                                                   stat.HeapAllocs.ToString,
                                                   stat.ClrObjs.ToString
                                                   ),
                                     fAddToBaseline:=False
                                                   )

            Next
            _VBAssert.IsTrue(stats.Count > 5, "didn't get enough memstats")

            _VBAssert.OutputText("Clearing marker wait events")
            SendMsg(ProcMsgVerb.SetCodeMarkerAction, fSendEndMsgSync:=True, dwords:={0, 0}) ' clear marker wait event
            lstActions = GetCodeMarkerActions()
            _VBAssert.IsTrue(lstActions.Count = 0, "Should have 0 actions, got " + lstActions.Count.ToString)

            _VBAssert.OutputText("Sending 3 markers: 1,2,3")
            SendMsg(ProcMsgVerb.SetCodeMarkerAction, fSendEndMsgSync:=True, dwords:={CInt(CodeMarkerActionEnum.CodeMarkerAction_Freeze Or CodeMarkerActionEnum.CodeMarkerAction_ShowInStatusMessage), 3, 1, 2, 3})
            lstActions = GetCodeMarkerActions()
            For Each act In lstActions
                _VBAssert.OutputText(act.ToString)
            Next


        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub



    <TestMethod(),
    TestCategory("codemarker"),
    TestCategory("offline"),
    Description("test code marker")>
    Public Sub VSCodeMarkerOffline()
        Try
            InitTestMethodAndSetBaseLine()



            Dim x = HeapAllocationContainer.dictCodeMarkerGuidLookup
            For Each itm In x
                _VBAssert.OutputText(itm.Key.ToString + " " + itm.Value)
            Next
            LoadOffLineSnap(IO.Path.Combine(memspectInstallDir, "Snaps\VSAssertSrvSnap"), fWaitTilClrLoadDone:=False)
            _VBAssert.OutputText("Loaded vs snap")

            Dim name = String.Empty
            Dim nAddr = IntPtr.Zero
            GetCodeMarkerNameFromId(Common._PerfIdleCodeMarker, nAddr)
            If nAddr <> IntPtr.Zero Then
                name = Marshal.PtrToStringAnsi(nAddr)
            End If
            _VBAssert.OutputText("Idle perf marker = " + name.ToString)

            Dim limit = 20
            Dim skip = 1000
            For Each alloc In ProcessHeapSnap.Allocs
                If skip > 0 Then
                    skip -= 1
                Else
                    Dim h = alloc.NearestCodeMarker
                    _VBAssert.OutputText("for this marker")
                    _VBAssert.OutputText(h + " " + alloc.ToString)
                    Dim t = alloc.GetCodeMarkerTree
                    _VBAssert.OutputText(t, cSplitChar:=CChar(vbCrLf))
                    _VBAssert.OutputText("")
                    limit -= 1
                    If limit = 0 Then
                        Exit For
                    End If

                End If
            Next



        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub


    <TestMethod(),
    Description("DevEnv 4 gig stacks")>
    Public Sub VS4gigStacks()
        Try
            InitTestMethodAndSetBaseLine("VS")
            _ProcessLauncher = New ProcessLauncher() With {
                .fHandle4gigStacks = 1,
                ._nmsecsToWaitTilStart = 5000
            } ' just all the defaults
            VSStartScenario()
        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub


    <StructLayout(LayoutKind.Sequential)>
    Structure ProcMemInt32
        <MarshalAs(UnmanagedType.ByValArray, SizeConst:=200)>
        Dim data() As Int32
    End Structure

    <DllImport("kernel32.dll", SetLastError:=True, EntryPoint:="ReadProcessMemory")>
    Public Shared Function ReadProcessMemoryDwordsTest(
               ByVal hProcess As IntPtr,
               ByVal lpBaseAddress As IntPtr,
               ByRef lpBuffer As ProcMemInt32,
               ByVal dwByteSize As Integer,
               ByRef lpNumberOfBytesRead As Integer
         ) As Integer
    End Function


    Function GetStackArray(ByVal alloc As HeapAllocationContainer) As IntPtr()
        Dim addr = alloc.HeapBlockPtr.MyAdd(_OffsetCallStackFrames)
        'Dim numBytesToRead = 4 * alloc.AllocationStruct.m_uicStackAddr
        Dim arr(alloc.AllocationStruct.m_uicStackAddr - 1) As IntPtr
        ReadProcessMemoryAsIntPtrArray(_hProcessTarget,
                                addr,
                                alloc.AllocationStruct.m_uicStackAddr,
                                IntPtr.Size,
                                arr)

        ' Dim buf As New ProcMemInt32
        'Dim res = ReadProcessMemoryDwordsTest(_hProcessTarget,
        '                            addr,
        '                            buf,
        '                            numBytesToRead,
        '                            0)
        'If res = 0 Then
        '    Throw New InvalidOperationException("null readprocmem")
        'End If
        Return arr
    End Function

    <TestMethod(),
    Description("DevEnv track clrobj")>
    <Ignore()>
    Public Sub VSPerf()
        Try
            InitTestMethodAndSetBaseLine()
            _ProcessLauncher = New ProcessLauncher() With {
                .TrackClrObjects = 0
            }
            StartVS()
            Dim nTot As Long = 0
            Dim start = DateTime.Now
            Dim nIter = 10 ' 10 => 4.5 secs
            For i = 1 To nIter
                For Each alloc In ProcessHeapSnap.Allocs
                    Dim arr = GetStackArray(alloc)
                    'Dim arr = alloc.GetCallStackAddressestoArray
                    nTot += arr.Length
                Next
            Next
            Dim endtime = DateTime.Now
            Dim nAvg = nTot / ProcessHeapSnap.Allocs.Count / nIter
            _VBAssert.OutputText(String.Format("Time ={0} # stks = {1}   Avg  ={2}",
                                               (endtime - start),
                                               ProcessHeapSnap.Allocs.Count,
                                               nAvg))

        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub


    <TestMethod(),
    Description("perf")>
    <Ignore()>
    Public Sub VSOpenVBFile()
        Try
            InitTestMethodAndSetBaseLine()
            _ProcessLauncher = New ProcessLauncher() With {
                .TrackClrObjects = 1,
                ._AdditionalCmdLineParams = """" + IO.Path.Combine(memspectInstallDir, "VSAssertSrv\My Project\MyExtensions\MyWpfExtension.vb") + """"
            }
            StartVS()
            Dim TargetClassName = "Microsoft.VisualStudio.Text.Implementation.TextBuffer"
            Dim q = From alloc In MemSpectSnap.Allocs
                    Where alloc.TBlkBlockType = BlockTypes.ClrObject AndAlso
                    alloc.GetSize = 124
                    Select
                    alloc,
                    ClassName = alloc.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True)
                    Where ClassName = TargetClassName
                    Select Stack = alloc.GetCallStackAsString,
                    alloc,
                    ClassName



            For Each targetAlloc In q
                CleanAndSortStacksForBaseline("Callstack for " + TargetClassName, {targetAlloc.Stack})
                Dim hctr = targetAlloc.alloc
                Dim ctrls = TVObjRefPanel.CreateObjectReferenceDataSurface(hctr, targetAlloc.ClassName)
                Dim otvobjrefPanel = CType(ctrls.SurfaceDetails.Children(0), TVObjRefPanel)
                Dim tvObjref = CType(otvobjrefPanel.Children(0), TVObjRefPanel.TVObjRef)

                _VBAssert.OutputText("ObjRefs for " + TargetClassName)
                _TvItemMaxDepth = 7
                For Each item In tvObjref.Items
                    If item.ToString.Contains("RefFromMe") Then
                        GetTVItemInfo(CType(item, MemSpect.TVObjRefPanel.TVObjRef.TVObjRefTVItem), 0)
                    End If
                Next
            Next
        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub




    <TestMethod(),
    Description("DevEnv track clrobj")>
    Public Sub VSTrackClrObj()
        Try
            InitTestMethodAndSetBaseLine()
            _ProcessLauncher = New ProcessLauncher() With {
                .TrackClrObjects = 1,
                ._nmsecsToWaitTilStart = 5000
            }
            VSStartScenario(fDoVirtualMem:=False) ' don't do vm: depends on timing for VSFileHandler
            Dim q = From alloc In ProcessHeapSnap.Allocs

            Dim snapProcHeap = ProcessHeapSnap
            Dim ctrls = ShowSnapshot(ProcessHeap, snapProcHeap)
            Dim bmem = CType(ctrls.SurfaceDetails.Children(0), BrowseMem)
            bmem._TabControl.SelectedIndex = 1 ' select Details tab
            bmem.OnTabItemDetailsGotFocus(bmem._TabItemDetails, New Windows.RoutedEventArgs)
            Dim itemCntNocodeMarkes = bmem._DetailBrowse._BrowseList.Items.Count
            _VBAssert.OutputText("Proc heap no merged code markers = " + itemCntNocodeMarkes.ToString, fAddToBaseline:=False)
            bmem._MergeCodemarkers = True

            bmem.RemoveBrowseDetailsAndRefresh()

            Dim itemcntWithCodeMarkers = bmem._DetailBrowse._BrowseList.Items.Count
            _VBAssert.OutputText("Proc heap with merged code markers = " + itemcntWithCodeMarkers.ToString, fAddToBaseline:=False)
            _VBAssert.IsTrue(itemCntNocodeMarkes + 400 < itemcntWithCodeMarkers, "merged code markers not working")

            DoTestSymbolDictionary(nMustBeGreaterThan:=1100)

        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub

    <TestMethod()>
    <Ignore()>
    Public Sub VSKnownIssue()
        Try
            InitTestMethodAndSetBaseLine()
            LoadOffLineSnap(IO.Path.Combine(memspectInstallDir, "Snaps\AGEndToEnd_3_1"), fWaitTilClrLoadDone:=True, fClearFilterToo:=False)
            _VBAssert.OutputText("Loaded AG snap")
            ReadHeaps()
            _VBAssert.OutputText("Got globalfilter seqnos " + vbCrLf + _GlobalFilter._LeakMultipleRawText, cSplitChar:=CChar(vbCr))
            Dim ctrls = DataWindowMain.MakeNewDatasurface("Filter", "Filter", nMaxHeaderHeight:=1)
            Dim ofilt = New Filter(_GlobalFilter, Nothing)
            ctrls.SurfaceDetails.Children.Add(ofilt)
            ofilt.rbtnKnownIssuesExclude.IsChecked = True

            ofilt.btnApply_Click(Me, New RoutedEventArgs)

            _VBAssert.OutputText("")
            _VBAssert.OutputText("Filtered Heap list cnt = " + ofilt.browFilteredHeapList._BrowseList.Items.Count.ToString)
            For Each itm In ofilt.browFilteredHeapList._BrowseList.Items
                _VBAssert.OutputText(itm.ToString, cSplitChar:=CChar(vbCr))
            Next
            ofilt.browFilteredHeapList._BrowseList.SelectedItems.Add(ofilt.browFilteredHeapList._BrowseList.Items(0))

            MemSpectWin._btnLastSelected = TrkType.ClrObjects
            InvokeContextMenu(ofilt.browFilteredHeapList._BrowseList.ContextMenu, "_Snapshot")
            Dim ctrlsSnap = DataWindowMain.GetDataSurface

            Dim bmemSubSnp = CType(ctrlsSnap.SurfaceDetails.Children(0), MemSpectWin)


            bmemSubSnp._bmem._TabControl.SelectedIndex = 1 ' select Details tab
            bmemSubSnp._bmem.OnTabItemDetailsGotFocus(bmemSubSnp._bmem._TabItemDetails, New Windows.RoutedEventArgs)
            Dim br = bmemSubSnp._bmem._DetailBrowse._BrowseList
            Dim nCnt = br.Items.Count
            _VBAssert.OutputText("# detail items= " + nCnt.ToString)
            For Each britem In br.Items
                _VBAssert.OutputText(britem.ToString)
            Next


        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub

    <TestMethod(),
    Description("DevEnv track clrobj")>
    Public Sub VSGenerics()
        Try
            InitTestMethodAndSetBaseLine()
            LoadOffLineSnap(IO.Path.Combine(memspectInstallDir, "Snaps\VSStart"), fWaitTilClrLoadDone:=True)
            _VBAssert.OutputText("Loaded vs snap")
            ReadHeaps()
            Dim cname = "AssemblyListCatalog"

            Dim q = From alloc In MemSpectSnap.Allocs
                    Where alloc.TBlkBlockType = BlockTypes.ClrObject
                    Let clname = alloc.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False)
                    Where clname.Contains(cname) OrElse clname.Contains("CachedAssemblyCatalog")
                    Order By alloc.TBlkBlockType Descending, alloc.AllocationStruct.SeqNo, clname
                    Select alloc

            _VBAssert.OutputText("# " + cname + " = " + q.Count.ToString)
            For Each alloc In q
                DoClassLayout(alloc, fOutput:=True)
            Next

        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub


    <TestMethod(),
    Description("DevEnv track clrobj")>
    <Ignore()>
    Public Sub VSLostClrObj()
        Try
            InitTestMethodAndSetBaseLine("VS")
            _ProcessLauncher = New ProcessLauncher() With {
                .TrackClrObjects = 1,
                .CheckGCLostObjects = 1
            }
            VSStartScenario()
            SendMsg(ProcMsgVerb.ClrObjTrk, fSendEndMsgSync:=True, dwords:={CUInt(0)}) ' turn off clr obj trk
        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub

    Private Sub VSStartScenario(Optional ByVal fDoVirtualMem As Boolean = True)
        StartVS()

        DoTestMappedFiles("Microsoft.VisualStudio.Shell.Interop.10.0.dll")

        'test SymGetSourceFile
        '=76729463 d:\w7rtm\com\ole32\com\catalog\catalog.cxx(2198) : ole32.dll!CComCatalog::GetClassInfoInternal + 658 bytes
        '=753a417d d:\w7rtm\base\win32\client\thread.c(65) : kernel32.dll!BaseThreadInitThunk + 14 bytes

        'For Each item In StackFrameDictionary
        '    If item.Value.Contains("f:\dd") OrElse item.Value.Contains("thread.c") Then
        '        Dim nParen = item.Value.IndexOf("(")
        '        If nParen >= 0 Then
        '            Dim fname = item.Value.Substring(0, nParen)
        '            _VBAssert.OutputText(item.Key.ToInt32.ToString("x8") + " " + item.Value + " " + fname)
        '            Dim sb As New StringBuilder(290)
        '            If VsSymGetSourceFile(_hProcessTarget, item.Key, fname, sb, sb.Capacity) Then
        '                _VBAssert.OutputText("Bingo! got source " + sb.ToString)
        '            Else
        '                _VBAssert.OutputText("no source ")
        '            End If

        '        End If
        '    End If
        'Next

        DoTestHeapCreates({"_clr.dll!DebuggerHeap::Init",
                       "_msenv.dll!InitFMain"
                      })

        If fDoVirtualMem Then
            DoTestVirtualMemory({"devenv.exe"})
        End If
        DoCodeMarkers()

        DoTestThreads()
        '        DoProcessHeap({"ole32.dll!CoInitializeEx"})

        'DoSpotCheckHeaps(AddressOf EnumHeapsCallback, fDoAllocs:=True)

        'FrozenTarget = False
        'System.Threading.Thread.Sleep(2000)
        'If False Then
        '    Dim dte = _ProcessLauncher.GetVSDTE()
        '    dte.ExecuteCommand("File.OpenProject ""C:\Users\calvinh\Documents\Visual Studio 2010\Projects\Heap\Heap.vbproj""")
        '    WaitForOtherThreads(45) 'wait 
        '    Dim noldCnt = GetGlobalPassCount()
        '    Dim ndelta = Integer.MaxValue
        '    Do While ndelta > 5000 ' wait til delta < 5000 for 5 seconds
        '        WaitForOtherThreads(5)
        '        ndelta = GetGlobalPassCount() - noldCnt
        '    Loop


        'End If
        '    EnumHeaps(AddressOf EnumHeapsCallback, fDoAllocs:=True)
    End Sub

    Private Sub DoCodeMarkers()
        Dim q = From alloc In MemSpectSnap.Allocs
                Where alloc.TBlkBlockType = BlockTypes.CodeMarker
                Select Stack = alloc.GetCallStackAsString,
                       Marker = alloc.GetCodeMarkerName,
                       MarkerId = alloc.TBlk.UnionData1.ToString()
                Where Marker.ToLower.Contains("splash") OrElse Marker.ToLower.Contains("perfidle")
                Order By MarkerId

        For Each item In q
            CleanAndSortStacksForBaseline("Code marker Id = " + item.MarkerId + " " + item.Marker, {item.Stack})
        Next

    End Sub

    Private Sub EnumHeapsCallback(ByVal snap As MemSnapshot)
        If snap.Allocs.Count > 0 Then
            If snap.SpyHeap.IsMemSpectHeap Then

            Else
                If snap.SpyHeap.GetHeapName = ProcessHeapName Then
                    For Each alloc In snap.Allocs

                    Next
                Else

                End If
            End If
        End If
    End Sub


End Class

<TestClass()>
<CLSCompliant(False)>
Public Class Offline
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
        BaseThreadCheck(Me)
        InitTestMethodAndSetBaseLine()
        _nmSecsDelayBetweenUIDisplay = 0
        Dim SnapName = IO.Path.Combine(memspectInstallDir, "Snaps\Npad")
        LoadOffLineSnap(SnapName)
    End Sub

    <TestCleanup()>
    Sub TestCleanup()
        BaseTestCleanup()
    End Sub

    <TestMethod()>
    <Description("UI features offline")>
    Public Sub NotepadUI()
        Try

            Dim allocs = ProcessHeapSnap.Allocs.Where(Function(h) h.GetSize = 72)

            DoTestUnusedMembers(allocs, "Proc heap where size=72")

            Dim tvitm As TVStackAggPanel.StackAggTreeView.StackAggTViewItem = Nothing

            _VBAssert.OutputText("TreeSearch testing: searching for LdrpRunInitializeRoutines")
            _VBAssert.IsTrue(CBool(GetPrivateProfileInt(ProfileStringSection, "WhenDist", 1, _iniFileName)), "When dist should be on")
            Dim ctrls = ShowSnapshot(ProcessHeap, ProcessHeapSnap)
            Dim bmem = CType(ctrls.SurfaceDetails.Children(0), BrowseMem)
            Dim tv = bmem._tvPanel._tv

            bmem._tvPanel.SearchTreeItems("LdrpRunInitializeRoutines", fMakeSubSnapshot:=False)
            tvitm = CType(tv.Items(0), TVStackAggPanel.StackAggTreeView.StackAggTViewItem) ' root
            DumpTVItemChildren(String.Empty, tvitm, fShowSelected:=True)

            bmem._tvPanel.SearchTreeItems("spincount", fMakeSubSnapshot:=True)

            ctrls = DataWindowMain.GetDataSurface
            bmem = CType(ctrls.SurfaceDetails.Children(0), BrowseMem)
            bmem.OnTabItemDetailsGotFocus(bmem._TabItemDetails, New RoutedEventArgs)
            _VBAssert.OutputText("Found spincount as subsnap #=" + bmem._DetailBrowse._BrowseList.Items.Count.ToString)

            _VBAssert.OutputText("BMem testing")
            ctrls = ShowSnapshot(ProcessHeap, ProcessHeapSnap)
            bmem = CType(ctrls.SurfaceDetails.Children(0), BrowseMem)
            bmem._TabControl.SelectedIndex = 1 ' details

            bmem.OnTabItemDetailsGotFocus(bmem._TabItemDetails, New RoutedEventArgs)
            Dim br = bmem._DetailBrowse._BrowseList
            tv = bmem._tvPanel._tv
            Dim nCnt = br.Items.Count
            _VBAssert.OutputText("# detail items= " + nCnt.ToString)
            For i = 0 To nCnt Step 50
                Dim detItem = br.Items(i)
                Dim hctr = HeapAllocationContainer.CreateFrom(detItem)
                _VBAssert.OutputText("Nav: det->tv = " + hctr.ToString)
                If hctr IsNot Nothing Then
                    bmem._tvPanel._tv.ExpandToItem(hctr)
                    tvitm = CType(tv.SelectedItem, TVStackAggPanel.StackAggTreeView.StackAggTViewItem)
                    DumpTVItemChildren("Selected TVItem=" + tvitm.ToString, CType(tv.Items(0), MyTreeViewItem), fShowSelected:=True)
                End If
            Next
            _VBAssert.OutputText("Now we try going detail to tv inverted")
            bmem._tvPanel._chkInvertStack.RaiseEvent(New RoutedEventArgs With {.RoutedEvent = CheckBox.CheckedEvent})
            For i = 0 To nCnt Step 50
                Dim detItem = br.Items(i)
                Dim hctr = HeapAllocationContainer.CreateFrom(detItem)
                _VBAssert.OutputText("Nav: det->tv = " + hctr.ToString)
                If hctr IsNot Nothing Then
                    bmem._tvPanel._tv.ExpandToItem(hctr)
                    tvitm = CType(tv.SelectedItem, TVStackAggPanel.StackAggTreeView.StackAggTViewItem)
                    DumpTVItemChildren("Selected TVItem=" + tvitm.ToString, CType(tv.Items(0), MyTreeViewItem), fShowSelected:=True)
                End If
            Next



            _VBAssert.OutputText("Now we try going from tv->Detail")
            ctrls = ShowSnapshot(ProcessHeap, ProcessHeapSnap) ' get a new bmem
            bmem = CType(ctrls.SurfaceDetails.Children(0), BrowseMem)
            tv = bmem._tvPanel._tv
            tvitm = CType(tv.Items(0), TVStackAggPanel.StackAggTreeView.StackAggTViewItem)
            ' Child # 3, then child # 0, etc
            For Each num In {0, 0, 0, 0, 0}
                _VBAssert.OutputText("tvitem target = " + tvitm.ToString)
                tvitm = CType(tvitm.Items(num), TVStackAggPanel.StackAggTreeView.StackAggTViewItem)
                tvitm.IsExpanded = True
            Next
            tvitm.IsSelected = True

            If SymSetHomeDirectory(CType(Process.GetCurrentProcess.Id, IntPtr), IO.Path.Combine(memspectInstallDir, "src")) = 0 Then
                _VBAssert.OutputText("failed setting sym home dir")
            End If
            _VBAssert.OutputText("Trying to view source code file " + tvitm._memNode._baseName, fAddToBaseline:=False)
            Dim srccode = ViewSourceCodeFile(tvitm._memNode._baseName)
            ' this failed twicee due to reboot: reboot again fixed. Sometimes reboot twice after windows update
            If TypeOf (DataWindowMain.GetDataSurface.SurfaceDetails.Children(0)) Is SimpleEd Then

                Dim simped = CType(DataWindowMain.GetDataSurface.SurfaceDetails.Children(0), SimpleEd)
                _VBAssert.OutputText("OpenSourceCodeFile " + srccode, fAddToBaseline:=False)
                _VBAssert.OutputText("OpenSourceCodeFile " + IO.Path.GetFileName(srccode))
                _VBAssert.OutputText("Source code view # lines =" + simped._br._BrowseList.Items.Count.ToString +
                                     " LineNo=" + simped._nLineNo.ToString)
            Else
                ' _VBAssert.IsTrue(False, "ViewSourceCodeFile failed. (Reboot can make succeed)")
            End If

            _VBAssert.OutputText("Now navigating to = " + tvitm.ToString)

            InvokeContextMenu(tv.ContextMenu, "_Goto Detail View")
            br = bmem._DetailBrowse._BrowseList

            For Each detitm In br.SelectedItems
                _VBAssert.OutputText("Selected items = " + detitm.ToString)
            Next
            For i = 3 To 5
                br.SelectedItems.Add(br.Items(i))

            Next
            Dim listfilt = CType(CType(bmem._DetailBrowse.Children(0), DockPanel).Children(0), Browse.ListFilter)
            Dim listfiltTxtBox = CType(CType(listfilt.Children(0), Border).Child, TextBox)
            _VBAssert.OutputText("Selected item totals = " + listfiltTxtBox.Text)

            DoSpotCheckHeaps(Sub()

                             End Sub, fDoAllocs:=True, fDumpOut:=True)

        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub


    <TestMethod()>
    <Description("was same baseline as online. Now separate")>
    Public Sub NotepadOffline()
        Try

            'd:\w7rtm\minkernel\etw\ntdll\regguidsapi.c(457) : ntdll.dll!EtwpCreateRegGuidsContext + 36 bytes
            'Dim filename = ViewSourceCodeFile("d:\w7rtm\minkernel\etw\ntdll\regguidsapi.c(457) : ntdll.dll!EtwpCreateRegGuidsContext + 36 bytes")
            '_VBAssert.OutputText("OpenSourceCodeFile = " + filename, fAddToBaseline:=False)


            DoSpotCheckHeaps(Sub(snap As MemSnapshot)

                             End Sub, fDoAllocs:=True)



            '        DoMappedFiles("")
            DoTestHeapCreates({"_clr.dll!DebuggerHeap::Init",
                           "_mscoree.dll!_heap_init",
                           "_MSVCR100.dll!_heap_init"
                          })


            DoTestVirtualMemory(strExcludes:={"msvcr100.dll", "msvcp100.dll"}) ' exclude because debug/retail

            DoTestDuplicates()


        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub

    <TestMethod>
    Public Sub NotepadHeapRef()
        Try
            For Each alloc In ProcessHeapSnap.Allocs
                Dim refs = GetReferences(alloc.GetAddr, ProcessHeapSnap.Allocs)
                _VBAssert.OutputText(String.Format("{0:x8}", alloc.GetAddr.ToInt32))
                For Each ref In refs
                    _VBAssert.OutputText(String.Format("  {0}", ref.ToString))
                Next
            Next
        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub

    <TestMethod()>
    Public Sub NotepadOfflineInteract()
        Try
            _ShowUI = True



#If DEBUG Then
            _nmSecsDelayBetweenUIDisplay = 500
#Else
            _nmSecsDelayBetweenUIDisplay = 100
#End If
            DoSpotCheckHeaps(Sub(snap As MemSnapshot)

                             End Sub, fDoAllocs:=True, fDumpOut:=True, fDoCtxMenuItems:=True)
        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub

    <TestMethod()>
    Public Sub NPadOffWhenDist()
        Try
            _ShowUI = True
            Dim ctrls = ShowSnapshot(ProcessHeap, ProcessHeapSnap)
            Dim bmem = CType(ctrls.SurfaceDetails.Children(0), BrowseMem)
            Dim tabctrl = CType(bmem.Children(0), MyTabControl)
            Dim tbitem = CType(tabctrl.Items(0), TabItem)
            Dim tvpanel = CType(tbitem.Content, TVStackAggPanel)
            tvpanel._WhenDistPanel._chkEnableWhen.IsChecked = True

            Dim lamVerifyOutput = Sub(strDesc As String)
                                      _VBAssert.OutputText(strDesc)
                                      _VBAssert.OutputText(String.Format("IsEnabled = {0} Min= {1:n0}  Max= {2:n0}",
                                                                         tvpanel._WhenDistPanel._chkEnableWhen.IsChecked,
                                                                         tvpanel._WhenDistPanel._WhenDistTxtBoxMin.GetSeqNo,
                                                                         tvpanel._WhenDistPanel._WhenDistTxtBoxMax.GetSeqNo)
                                                                     )
                                      DumpTVItemChildren("WhenDist initial children", CType(tvpanel._tv.Items(0), TVStackAggPanel.StackAggTreeView.StackAggTViewItem))
                                      WaitForOtherThreads(2000)
                                  End Sub

            lamVerifyOutput("Initial children")

            WaitForOtherThreads(500)
            _VBAssert.OutputText("Now set focus on txtbox for Min in panel and send '123'")
            tvpanel._WhenDistPanel._WhenDistTxtBoxMin.Focus()
            WaitForOtherThreads(500)

            Keyboard.FocusedElement.RaiseEvent(
                New TextCompositionEventArgs(
                    InputManager.Current.PrimaryKeyboardDevice,
                    New TextComposition(InputManager.Current, Keyboard.FocusedElement, "123")
                    ) With {.RoutedEvent = TextCompositionManager.TextInputEvent}
                )


            '          Dim text = "Hello"
            '          Dim target = Keyboard.FocusedElement
            '          Dim routedEvent = TextCompositionManager.TextInputEvent

            'target.RaiseEvent( 
            '  new new TextCompositionEventArgs( 
            '    InputManager.Current.PrimaryKeyboardDevice, 
            '    new TextComposition(InputManager.Current, target, text)) 
            '  { RoutedEvent = routedEvent } 
            '); 


            '            System.Threading.Thread.Sleep(7000)
            'Keyboard.FocusedElement.RaiseEvent(
            '    New KeyEventArgs(
            '        Windows.Input.Keyboard.PrimaryDevice,
            '        PresentationSource.FromVisual(CType(Keyboard.FocusedElement, Media.Visual)),
            '        timestamp:=0,
            '        Key:=Key.D1) With {.RoutedEvent = Keyboard.KeyDownEvent})

            'whendist._WhenDistTxtBoxMin.RaiseEvent(
            '    New KeyEventArgs(
            '        Windows.Input.Keyboard.PrimaryDevice,
            '        PresentationSource.FromVisual(CType(Keyboard.FocusedElement, Media.Visual)),
            '        timestamp:=0,
            '        Key:=Key.D1) With {.RoutedEvent = Keyboard.KeyUpEvent})




            tvpanel._WhenDistPanel._btnApplyWhenMinMax.RaiseEvent(New RoutedEventArgs() With {.RoutedEvent = Button.ClickEvent})

            lamVerifyOutput("After typing '123'")

            Dim tvitem = CType(tvpanel._tv.Items(0), TVStackAggPanel.StackAggTreeView.StackAggTViewItem)

            _VBAssert.OutputText("now try to select the whendist contents for first item")

            _VBAssert.IsNull(tvitem._WhenDistTextControl._txtBox, "textbox should be null initially")


            'InputManager.Current.ProcessInput(New MouseButtonEventArgs(
            '        InputManager.Current.PrimaryMouseDevice,
            '        0,
            '        MouseButton.Left) With {
            '           .RoutedEvent = TextBlock.MouseDownEvent
            '        }
            ')
            tvitem._WhenDistTextControl._txtBlk.RaiseEvent(
                New MouseButtonEventArgs(
                    InputManager.Current.PrimaryMouseDevice,
                    0,
                    MouseButton.Left) With {
                       .RoutedEvent = TextBlock.MouseDownEvent
                    }
                )
            _VBAssert.IsNotNull(tvitem._WhenDistTextControl._txtBox, "textbox should be created")

            tvitem._WhenDistTextControl._txtBox.Select(2, 3)


            InvokeContextMenu(tvitem._WhenDistTextControl._txtBox.ContextMenu, "Set _Range to bucket Selection")

            lamVerifyOutput("After range sel")

            _VBAssert.OutputText(" now test swap back from textbox to textblock")
            tvitem = CType(tvpanel._tv.Items(0), TVStackAggPanel.StackAggTreeView.StackAggTViewItem)
            ' now raise event so becomes textbox
            tvitem._WhenDistTextControl._txtBlk.RaiseEvent(
                New MouseButtonEventArgs(
                    InputManager.Current.PrimaryMouseDevice,
                    0,
                    MouseButton.Left) With {.RoutedEvent = TextBlock.MouseDownEvent}
                )
            'now select next item which should change textbox => textblock
            _VBAssert.IsNotNull(tvitem._WhenDistTextControl._txtBox, "textbox not created?")

            Dim tvitemChild = CType(tvitem.Items(0), TVStackAggPanel.StackAggTreeView.StackAggTViewItem)
            tvitemChild.IsSelected = True
            _VBAssert.IsNull(tvitem._WhenDistTextControl._txtBox, "textbox should be gone?")



            _VBAssert.OutputText(" now test subsnap")
            tvitem = CType(tvpanel._tv.Items(0), TVStackAggPanel.StackAggTreeView.StackAggTViewItem)

            tvitem._WhenDistTextControl._txtBlk.RaiseEvent(
                New MouseButtonEventArgs(
                    InputManager.Current.PrimaryMouseDevice,
                    0,
                    MouseButton.Left) With {.RoutedEvent = TextBlock.MouseDownEvent}
                )

            tvitem._WhenDistTextControl._txtBox.Select(2, 3)

            InvokeContextMenu(tvitem._WhenDistTextControl._txtBox.ContextMenu, "_SubSnap Current items")

            Dim ctrlsSnap = DataWindowMain.GetDataSurface
            Dim bmemSubSnp = CType(ctrlsSnap.SurfaceDetails.Children(0), BrowseMem)
            tabctrl = CType(bmemSubSnp.Children(0), MyTabControl)
            tbitem = CType(tabctrl.Items(0), TabItem)
            tvpanel = CType(tbitem.Content, TVStackAggPanel)

            lamVerifyOutput("Subsnap contents = ")

            tvpanel._WhenDistPanel._chkEnableWhen.IsChecked = False
            lamVerifyOutput("Disable WhenDist")
            tvpanel._WhenDistPanel._chkEnableWhen.IsChecked = True

        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub

End Class

<TestClass()>
<CLSCompliant(False)>
Public Class Notepad
    Inherits TestBase
    <ClassInitialize()>
    Public Shared Sub ClassInit(ByVal ctx As TestContext)
        BaseClassInitialize(ctx)
    End Sub

    <TestInitialize()>
    Public Sub TestInit()
        BaseTestInit()
        _nmSecsDelayBetweenUIDisplay = 0
    End Sub

    <TestCleanup()>
    Sub TestCleanup()
        BaseTestCleanup()
    End Sub


    <TestMethod()>
    <Ignore()>
    Public Sub TestPerf()
        InitTestMethodAndSetBaseLine("TestPerf")
        _ProcessLauncher = New ProcessLauncher() With {
            .StartChildProcess = 0
        }
        StartNotepad()

        ReadHeaps()

        FrozenTarget = True




        Dim allocs = ProcessHeapSnap.Allocs

        Dim st = DateTime.Now



        For iter = 0 To 1000
            Dim tempfilename = IO.Path.ChangeExtension(IO.Path.GetTempFileName, "mhd")

            Dim dd = New FastSerialization0.IOStreamStreamWriter(tempfilename)
            For i = 0 To 100000
                dd.Write("test")
            Next
            'Dim tempfilename = IO.Path.ChangeExtension(IO.Path.GetTempFileName, "mhd")

            'Dim s = New FastSerialization0.Serializer(tempfilename, ProcessHeapSnap)
            's.Close()
            '_VBAssert.OutputText(tempfilename)

            'Dim deser = Common.OfflineMegaSnapshot.GetDeserializerForPath(tempfilename)
            'Common._offlineSnapshot = New Common.OfflineMegaSnapshot

            'Dim snap = CType(deser.GetEntryObject, MemSnapshot)
            'For Each x In snap.Allocs
            '    _VBAssert.OutputText(x.ToString)
            'Next
        Next


        'For iter = 0 To 1000
        '    For Each alloc In allocs
        '        Dim y = alloc.GetCallStackAddressestoArray
        '    Next

        'Next
        Dim endtime = DateTime.Now - st
        _VBAssert.OutputText(endtime.ToString)


    End Sub

    <TestMethod()>
    <Ignore>
    Public Sub NoteTestInterProcComm()
        Try
            BaseThreadCheck(Me)
            InitTestMethodAndSetBaseLine()
            _ProcessLauncher = New ProcessLauncher()
            StartNotepad()
            SendMsg(ProcMsgVerb.GetIniFile, fSendEndMsgSync:=True)
            Dim tmp = ReadSharedMemAsString()
            _VBAssert.OutputText(tmp)
            Dim dataRead = String.Empty
            Dim nStrDatalen = 250 ' in wchars
            Dim dataToRead = New String("A"c, nStrDatalen)
            Dim encoder = New UnicodeEncoding
            Dim barray = encoder.GetBytes(dataToRead)
            _VBAssert.OutputText("arr size=" + barray.Length.ToString)
            Dim nbwrtten = 0
            If WriteProcessMemory(_hProcessTarget, _memSpectSharedMemAddr, barray, barray.Length, nbwrtten) = 0 Then
                Dim strResult = GetErrorMessageFromWin32LastError(System.Runtime.InteropServices.Marshal.GetLastWin32Error)

                _VBAssert.OutputText("didn't writeprocmem " + strResult)
            End If
            _VBAssert.OutputText("# bytes written = " + nbwrtten.ToString)
            For nloop = 1 To 10
                Dim el(3) As Double
                For nMode = 0 To 2 ' 0 = none, 1= shared mem, 2 = pipe
                    Dim dtStart = DateTime.Now
                    For iter = 0 To 100000 ' 100000
                        Select Case nMode
                            Case 0
                                SendMsg(ProcMsgVerb.GetClassNameFromId, fSendEndMsgSync:=True, dwords:={1, 0, nStrDatalen}) 'send 1 to indicate test comm mode
                            Case 1
                                SendMsg(ProcMsgVerb.GetClassNameFromId, fSendEndMsgSync:=True, dwords:={1, 0, nStrDatalen}) 'send 1 to indicate test comm mode
                                dataRead = ReadSharedMemAsString(fAnsi:=False)
                                _VBAssert.IsTrue(dataRead.StartsWith("AAA"))
                            Case 2
                                SendMsg(ProcMsgVerb.GetClassNameFromId, fSendEndMsgSync:=False, dwords:={1, 1, nStrDatalen}) 'send 1 to indicate test comm mode
                                Dim res = GetMsg(4)
                                Dim nLen = BitConverter.ToInt32(res, 0)
                                res = GetMsg(nLen)
                                dataRead = encoder.GetString(res)

                                EndMsgSync()
                                _VBAssert.IsTrue(dataRead.StartsWith("AAA"))

                        End Select

                    Next

                    Dim elapsed = (DateTime.Now - dtStart).TotalSeconds
                    el(nMode) = elapsed
                Next
                _VBAssert.OutputText(String.Format("{0,10} {1,10} {2,10}", el(0), el(1), el(2)))

            Next


        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub


    '#Critsects=  71
    <TestMethod()>
    Public Sub NotePad()
        Try
            BaseThreadCheck(Me)
            InitTestMethodAndSetBaseLine("Notepad")
            _ProcessLauncher = New ProcessLauncher() With {
                .TrackClrObjects = 0
            }
            '_ProcessLauncher._fMessageBoxOnStart = 1
            DoNotepadstuff()

            DoTestVMUI(fManagedToo:=False)


        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub

    <TestMethod()>
    <Ignore()>
    Public Sub NotePad4GigStacks()
        Try
            BaseThreadCheck(Me)
            InitTestMethodAndSetBaseLine("Notepad")
            _ProcessLauncher = New ProcessLauncher() With {
                .fHandle4gigStacks = 1,
                ._nmsecsToWaitTilStart = 5000
            }
            DoNotepadstuff()
        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub

    <TestMethod()>
    <Ignore()>
    Public Sub AttachExistProcess()
        Dim hProc As Process = Nothing
        Try
            InitTestMethodAndSetBaseLine()
            CommonUI.InitializeCommonUI()
            Dim pathNotepad = GetNotepadPath()
            hProc = Process.Start(pathNotepad)
            _VBAssert.OutputText("Runtime version " + System.Environment.Version.ToString(), fAddToBaseline:=False)
            ' Dev10: ==>Actual  : "Runtime version 4.0.30319.269"
            ' Dev11: ==>Actual  : "Runtime version 4.0.30319.17929"

            System.Threading.Thread.Sleep(1000)
            Dim strResult = ProcComm.InitComm(
                {System.Environment.GetCommandLineArgs(0),
                 "/x",
                 CStr(hProc.Id)
                }
             )
            If Not String.IsNullOrEmpty(strResult) Then
                _VBAssert.OutputText("Could not init comm " + pathNotepad + " " + strResult)
            End If
            DoTestVMUI(fManagedToo:=False)

            _VBAssert.OutputText("Now testing symbols")
            _VBAssert.IsFalse(VBDiagMarginBase._DidShowSymbolPath)
            Dim sympath = VBDiagMarginBase.ShowSymbolPath()
            _VBAssert.OutputText("Sym path = " + sympath, fAddToBaseline:=False)
            _VBAssert.IsTrue(Not String.IsNullOrEmpty(sympath), "Error getting sympath: make sure QTAgent is fresh (either kill process, or Tools->Options->TestTools->TestExecution->KeepTestExecutionEngine running betweenruns")

            _VBAssert.IsTrue(VBDiagMarginBase._DidShowSymbolPath)
            Dim symCtrls = SymbolFileUI.CreateSymbolFileDataSurface

            Dim symFiles As SymbolFileUI = Nothing

            Dim tabctrl As MyTabControl = Nothing
            Dim tabSymFiles As TabItem
            Dim brSymFiles As Browse = Nothing
            Dim lamRefresh = Sub()
                                 tabctrl = CType(symCtrls.SurfaceDetails.Children(0), MyTabControl)
                                 symFiles = CType(symCtrls.SurfaceHeader.Tag, SymbolFileUI)
                                 tabSymFiles = CType(tabctrl.Items(0), TabItem)
                                 brSymFiles = CType(tabSymFiles.Content, Browse)
                             End Sub
            lamRefresh.Invoke()
            For Each itm In brSymFiles._BrowseList.Items
                _VBAssert.OutputText(itm.ToString, fAddToBaseline:=False)
            Next
            _VBAssert.IsTrue(brSymFiles._BrowseList.Items.Count = 1, "Symfiles list should have 1 item " + brSymFiles._BrowseList.Items.Count.ToString)

            _VBAssert.OutputText("Now Refreshing")
            symFiles._btnRefreshsym.RaiseEvent(New Windows.RoutedEventArgs With {.RoutedEvent = Button.ClickEvent})
            lamRefresh.Invoke()
            _VBAssert.IsTrue(brSymFiles._BrowseList.Items.Count = 1, "Symfiles list should have 1 item" + brSymFiles._BrowseList.Items.Count.ToString)

            _VBAssert.OutputText("Now loading all symfiles")
            symFiles._chkIncludeAllModules.IsChecked = True
            symFiles._btnRefreshsym.RaiseEvent(New Windows.RoutedEventArgs With {.RoutedEvent = Button.ClickEvent})
            lamRefresh.Invoke()
            _VBAssert.IsTrue(brSymFiles._BrowseList.Items.Count >= 25, "Symfiles list should have >=25 " + brSymFiles._BrowseList.Items.Count.ToString)

            For Each itm In brSymFiles._BrowseList.Items
                _VBAssert.OutputText(itm.ToString, fAddToBaseline:=False)
            Next
            _VBAssert.IsTrue(symFiles._SymFilesToUnload.Count >= 25, "# files to unload should be >=25 " + " Actual=" + symFiles._SymFilesToUnload.Count.ToString)


            _VBAssert.OutputText("Now loading symbols from symfiles")
            Dim tbSyms As TabItem = CType(tabctrl.Items(1), TabItem)
            tbSyms.Focus()
            _VBAssert.IsNotNull(tbSyms.Content, "focus didn't populate content")

            Dim brSymAll As Browse = CType(tbSyms.Content, Browse)
            Dim ncnt = 100
            For Each itm In brSymAll._BrowseList.Items
                _VBAssert.OutputText(itm.ToString, fAddToBaseline:=False)
                ncnt -= 1
                If ncnt = 0 Then
                    Exit For
                End If
            Next

        Catch ex As Exception
            HandleTestException(ex)
            Assert.IsTrue(False, "Exception " + ex.Message)
        Finally
            Dim ncnt = 15
            hProc.CloseMainWindow()
            While Not hProc.HasExited
                '                hProc.Close()
                hProc.WaitForExit(1000)
                ncnt -= 1
                If ncnt = 0 Then
                    hProc.Kill()
                    Exit While
                End If
            End While
        End Try


    End Sub

    <TestMethod()>
    <Description("GDI tracking")>
    Public Sub NPadGDI()
        Try
            BaseThreadCheck(Me)
            InitTestMethodAndSetBaseLine()
            _VBAssert.OutputText("GDI")
            _ProcessLauncher = New ProcessLauncher() With {
                .StartChildProcess = 0,
                .fTrackGDI = 1
            }
            StartNotepad()
            FrozenTarget = True
            Dim q = From alloc In MemSpectSnap.Allocs Where
                    (alloc.TBlkBlockType = BlockTypes.GdiObjs)

            MemSpectWin._btnLastSelected = TrkType.GdiObjs
            Dim ctrls = ShowSubSnapShot(q.ToList, "SubSnapShot button", tblkType:=TrkType.GdiObjs)
            Dim mspectWin = CType(ctrls.SurfaceDetails.Children(0), MemSpectWin)
            Dim bmem = mspectWin._bmem

            bmem._TabControl.SelectedIndex = 1 ' select Details tab
            bmem.OnTabItemDetailsGotFocus(bmem._TabItemDetails, New Windows.RoutedEventArgs)

            For Each itm In bmem._DetailBrowse._BrowseList.Items
                _VBAssert.OutputText(CleanLineForBaseline(itm.ToString))
            Next


        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub

    <TestMethod()>
    Public Sub NotePadEatMem()
        Try
            BaseThreadCheck(Me)
            InitTestMethodAndSetBaseLine()
            _ProcessLauncher = New ProcessLauncher() With {
                .StartChildProcess = 0
            }
            StartNotepad()


            FrozenTarget = True
            _VBAssert.OutputText("MemEaterAPI, init # items  = " + MemoryEater._EatenAllocations.Count.ToString)
            MemoryEater.EatSomeMemory(10, False, False, True, False)

            _VBAssert.OutputText("MemEaterAPI, After eating # items  = " + MemoryEater._EatenAllocations.Count.ToString, fAddToBaseline:=False)
            _VBAssert.OutputText("MemoryEater._EatenAllocations.Count > 180 " + If(MemoryEater._EatenAllocations.Count > 180, "true", "false"))

            MemoryEater.FreeAll()
            _VBAssert.OutputText("MemEaterAPI, After Free # items  = " + MemoryEater._EatenAllocations.Count.ToString)

            Dim x = New MemoryEaterUI
            Dim brow = CType(x._ctrls.SurfaceDetails.Children(0), Browse)
            Dim btnEat = CType(x._ctrls.SurfaceHeader.Children(0), Button)
            Dim btnFree = CType(x._ctrls.SurfaceHeader.Children(1), Button)

            _VBAssert.OutputText("MemEaterUI, Init # items  = " + brow._BrowseList.Items.Count.ToString)
            btnEat.RaiseEvent(New Windows.RoutedEventArgs With {.RoutedEvent = Button.ClickEvent})
            brow = CType(x._ctrls.SurfaceDetails.Children(0), Browse) ' new instance after click
            _VBAssert.OutputText("MemEaterUI, after eating big # items  = " + brow._BrowseList.Items.Count.ToString, fAddToBaseline:=False)
            _VBAssert.OutputText("brow._BrowseList.Items.Count > 14 " + If(brow._BrowseList.Items.Count >= 14, "true", "false"))

            btnFree.RaiseEvent(New Windows.RoutedEventArgs With {.RoutedEvent = Button.ClickEvent})
            brow = CType(x._ctrls.SurfaceDetails.Children(0), Browse) ' new instance after click
            _VBAssert.OutputText("MemEaterUI, after Clicking free # items  = " + brow._BrowseList.Items.Count.ToString)

        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub

    <TestMethod()>
    <Description("use memspect.exe to launch notepad.exe with cmd line arg for file name")>
    Public Sub NotePadCmdLineArgs()
        Try
            InitTestMethodAndSetBaseLine()
            Dim wrkdir = Me._TestContext.DeploymentDirectory ' D:\Memspect\VSAssertSrv\TestResults\calvinh_CALVINH9 2010-08-20 10_10_45\Out
            ' need to use a unique temp file name
            Dim tempfilename = IO.Path.ChangeExtension(IO.Path.GetTempFileName, "txt")
            Dim inifilename = IO.Path.Combine(wrkdir, MemSpectIniName)
            NativeImports.WritePrivateProfileString(ProfileStringSection, "StartChildProcess", "1", inifilename)
            Dim npadPath = GetNotepadPath()
            ProcessLauncher.AddProcessToJustThisProcesses(npadPath, inifilename)
            IO.File.WriteAllText(tempfilename, "test file" + vbCrLf + IO.File.ReadAllText(inifilename))
            Dim memspectExeFile = IO.Path.Combine(wrkdir, "memspect.exe")
            Dim memSpectProc = System.Diagnostics.Process.Start(memspectExeFile, npadPath + " " + tempfilename)
            System.Threading.Thread.Sleep(3000)


            Dim npadProc As Process = (From proc In System.Diagnostics.Process.GetProcessesByName("notepad")
                                       Where proc.MainWindowTitle.IndexOf(IO.Path.GetFileName(tempfilename), StringComparison.OrdinalIgnoreCase) >= 0).FirstOrDefault

            _VBAssert.IsNotNull(npadProc, "notepad not launched")
            If npadProc IsNot Nothing Then
                _VBAssert.OutputText("bingo got temp filename")
            End If
            'For Each p In q
            '    If p.MainWindowTitle.IndexOf(IO.Path.GetFileName(tempfilename), StringComparison.OrdinalIgnoreCase) >= 0 Then
            '        _VBAssert.OutputText("bingo got temp filename")
            '        npadProc = p
            '        Exit For
            '    End If
            'Next
            ' now search for memspect proc with the notepad pid
            Dim retrycount = 20
            Dim msProc As Process = Nothing
            Do While retrycount > 0
                Dim q = From proc In System.Diagnostics.Process.GetProcessesByName("memspect")
                        Where proc.MainWindowTitle.Contains(npadProc.Id.ToString)
                If q.Count = 1 Then
                    msProc = q.First
                    _VBAssert.OutputText("got memspect process that matched our instance of notepad")
                    Exit Do
                End If
                System.Threading.Thread.Sleep(1000)

                retrycount -= 1
            Loop
            Assert.IsNotNull(msProc, "Memspect process not found")

            npadProc.CloseMainWindow()
            System.Threading.Thread.Sleep(3000)
            If msProc.HasExited Then
                _VBAssert.OutputText("Memspect proc closed properly after npad closed")

            End If



            '            _VBAssert.OutputText("After np launch, # instances = " + q.Count.ToString)


            ' now ensure that notepad and memspect processes have started


            '            _VBAssert.OutputText("After np kill, # instances = " + lamGetProc.Invoke("notepad.exe").ToString)

        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub


    <TestMethod()>
    Public Sub NotePadAddrDump()
        Try
            BaseThreadCheck(Me)
            InitTestMethodAndSetBaseLine("NotepadAddrDump")
            _ProcessLauncher = New ProcessLauncher() With {
                ._nmsecsToWaitTilStart = 3000,
                ._WhenDist = 0
            }
            StartNotepad()
            FrozenTarget = True
            ReadHeaps()

            DoTestPivot("KERNELBASE.dll!LoadLibraryExW")

            DoTestDuplicates()

            DoStringAddrDump()

            DoTestAssertSeqNoOrStackFrame(ProcMsgVerb.AssertSeqNo)

            DoTestAssertSeqNoOrStackFrame(ProcMsgVerb.AssertStackFrame)

            DoTestWastedMemory(nMatchCnt:=17, nItemsExpected:=22)

            DoTestModuleFold()


            _VBAssert.OutputText("WorkingSet ")

            Dim ctrlsWS = VirtualMem.ShowWorkingSetInfo()

            Dim browWS = CType(ctrlsWS.SurfaceDetails.Children(0), Browse)
            _VBAssert.IsTrue(browWS._BrowseList.Items.Count > 1800, "Workingset expected 1800, got " + browWS._BrowseList.Items.Count.ToString)



        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub

    Private Sub DoStringAddrDump()
        'Address = 0x00579210  Size = 4,524  SeqNo = 417 Thread = 16004 Heap = _Process Heap HeapHandle= 00530000
        '       Cannot open the %% file.  Make sure a disk is in the drive you specified.
        Dim qString = From alloc In ProcessHeapSnap.Allocs
                      Where alloc.GetSize = 4524
                      Select
                    Stack = alloc.GetCallStackAsString,
                    AddrDump = GetMemoryDump(alloc.GetAddr, alloc.GetSize, nMaxDumpSize:=100000)

        For Each item In qString
            CleanAndSortStacksForBaseline("Notepad string stack", {item.Stack})
            _VBAssert.OutputText("Notepad string dump")
            Dim dump = item.AddrDump
            ProcessAddressDump(dump)
        Next
    End Sub

    <TestMethod()>
    <Ignore()>
    Public Sub NotePadHeaderTrailer()
        Try
            BaseThreadCheck(Me)
            InitTestMethodAndSetBaseLine()
            ' we have to deal with allocations made in a heap before we're loaded, then freed
            ' after we've wrapped them
            _ProcessLauncher = New ProcessLauncher() With {
                .fMessageBoxOnStart = 1,
                .RetailHeaderTrailerSize = 0
            }
            StartNotepad()
            FrozenTarget = True
            ReadHeaps()
        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub


    <TestMethod()>
    Public Sub NotePadCompile()
        Try
            BaseThreadCheck(Me)
            InitTestMethodAndSetBaseLine()
            'AddHandler StatusMessageEvent, Sub(o, e)
            '                                   _VBAssert.OutputText(e.Message, cSplitChar:=CChar(vbCr))
            '                               End Sub
            AddHandler StatusMessageEvent, AddressOf TheStatusHandler
            _VBAssert.OutputText("Start Compile")
            _ProcessLauncher = New ProcessLauncher() With {
                .TrackClrObjects = 1,
                ._nmsecsToWaitTilStart = 500
            }
            StartNotepad(fFreeze:=False)
            DoCompileTest()
        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub



    <TestMethod()>
    <Ignore>
    Public Sub NotePadPipes()
        Try
            BaseThreadCheck(Me)
            InitTestMethodAndSetBaseLine()
            _ProcessLauncher = New ProcessLauncher() With {
                ._nmsecsToWaitTilStart = 3000
            }
            AddHandler Common.StatusMessageEvent, Sub(o, e)
                                                      _VBAssert.OutputText("StatMsg " + e.Message, fAddToBaseline:=False)
                                                  End Sub
            Dim npProcess = StartNotepad()
            FrozenTarget = False
            AddHandler npProcess.Exited, Sub(o, e)
                                             UpdateStatusMsg("notepad exited event")
                                         End Sub
            'Dim dwords = {1, 2, 3}

            'Dim arrSize = dwords.Count * IntPtr.Size
            'Dim barray As Byte() = CType(Array.CreateInstance(GetType(Byte), arrSize), Byte())
            'For i = 0 To dwords.Count - 1 ' little endian first
            '    Dim bytes = BitConverter.GetBytes(dwords(i))
            '    For j = 0 To 3
            '        barray(4 * i + j) = bytes(j)
            '    Next
            'Next
            'Dim tmr = New Timers.Timer(2000)

            'AddHandler tmr.Elapsed, Sub(o, e)
            '                            Try
            '                                _VBAssert.OutputText("Timer fired")
            '                                'ProcComm._pipestreamToTarget.Write(barray, 0, arrSize)
            '                                SendMsg(ProcMsgVerb.ForceGC, fSendEndMsgSync:=True, dwords:={1})

            '                            Catch ex As Exception
            '                                _VBAssert.OutputText(ex.Message)
            '                            End Try
            '                        End Sub
            'tmr.Start()
            SendMsg(ProcMsgVerb.ForceGC, fSendEndMsgSync:=True, dwords:={1})
            _VBAssert.OutputText("Killing np")
            npProcess.CloseMainWindow()
            _ProcessLauncher._pidTargetProcess = 0
            System.Threading.Thread.Sleep(3000)
        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub



    <TestMethod()>
    Public Sub NotePadConnect()
        Try
            BaseThreadCheck(Me)
            InitTestMethodAndSetBaseLine()
            _ProcessLauncher = New ProcessLauncher() With {
                ._nmsecsToWaitTilStart = 3000
            }
            Dim npProcess = StartNotepad()
            ReadHeaps()
            FrozenTarget = False
            _VBAssert.OutputText("Disconnecting Notepad ")
            CloseCommunications()
            System.Threading.Thread.Sleep(5000)
            Dim strResult = InitComm({System.Environment.GetCommandLineArgs(0), _ProcessLauncher._pidTargetProcess.ToString})
            _ProcessLauncher._DidInitComm = True
            If Not String.IsNullOrEmpty(strResult) Then
                _VBAssert.OutputText("Could not init comm PID=" + _ProcessLauncher._pidTargetProcess.ToString + " " + strResult)
            End If

            _VBAssert.OutputText("Reconnecting to Notepad via PID")
            FrozenTarget = True
            ReadHeaps()
            DoTestHeapCreates({"_clr.dll!DebuggerHeap::Init",
                           "_mscoree.dll!_heap_init",
                           "_MSVCR100.dll!_heap_init"
                          })

            DoTestVirtualMemory(strExcludes:={"msvcr100.dll", "msvcp100.dll", "msvcp120.dll", "windows\registration", MemSpectDllName.ToLower}) ' exclude because debug/retail
            _VBAssert.OutputText("Taking offline snap")
            Dim newsnapfolder = Path.Combine(_TestContext.DeploymentDirectory, "SnapNotepadConnect")
            Dim snapfolder = Common.OfflineMegaSnapshot.CreateMegaSnapshot(newsnapfolder)
            _VBAssert.OutputText("Done offline snap. folder =" + snapfolder, fAddToBaseline:=False)

            CloseCommunications()
            System.Threading.Thread.Sleep(3000)
            _VBAssert.OutputText("Loading offline snap " + snapfolder, fAddToBaseline:=False)
            Dim strres = InitComm({System.Environment.GetCommandLineArgs(0), "/o", snapfolder})
            If Not String.IsNullOrEmpty(strres) Then
                _VBAssert.OutputText("Couldn't load offline snap " + snapfolder + " " + strres)
            Else
                DoNotepadstuff(fStartNotepadProcess:=False, fDoTestDupes:=False)
            End If
            DoTestWastedMemory(nMatchCnt:=17, nItemsExpected:=23)
            CloseCommunications()

        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub

    '<DeploymentItem("MemSpectMTATest.exe")>
    <TestMethod()>
    Public Sub NotepadMultiThreadConnect()
        Try

            'start notepad
            InitTestMethodAndSetBaseLine()
            _ProcessLauncher = New ProcessLauncher()

            StartNotepad()

            _VBAssert.OutputText(" Started notepad, closing communication")
            ProcComm.CloseComm()

            Dim psi As New ProcessStartInfo() With {.RedirectStandardError = True, .RedirectStandardOutput = True, .UseShellExecute = False}
            Dim destFile = Path.Combine(_TestContext.TestDeploymentDir, "MemSpectMTATest.exe")
            File.Copy(IO.Path.Combine(memspectInstallDir, "test\MemspectMTATest.exe"), destFile)
            psi.FileName = destFile
            psi.Arguments = _ProcessLauncher._pidTargetProcess.ToString()

            Dim proc = New Process()

            proc.StartInfo = psi

            proc.Start()
            While True
                Dim line = proc.StandardOutput.ReadLine
                If line = Nothing Then
                    Exit While
                End If
                _VBAssert.OutputText("Gotaline " + line, fAddToBaseline:=False)
            End While
            proc.WaitForExit()
            _VBAssert.OutputText("Process exit code= " + proc.ExitCode.ToString)
            '        _VBAssert.IsTrue(proc.ExitCode = 0, "Test failed: exit code non-zero")
        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub

    Friend Shared Function GetNotepadPath() As String
        Dim windir = System.Environment.GetEnvironmentVariable("windir") ' like "c:\windows"
        Dim pname = windir + "\syswow64\notepad.exe"
        If Not IO.File.Exists(pname) Then
            pname = windir + "\system32\notepad.exe"
        End If
        Return pname
    End Function

    Private Function StartNotepad(Optional fFreeze As Boolean = False) As Process
        _VBAssert.OutputText("Starting Notepad", fAddToBaseline:=False)
        Dim pname = GetNotepadPath()
        CommonUI.InitializeCommonUI()
#If DEBUG Then
        ' _ProcessLauncher._nmsecsToWaitTilStart = 3000
#End If
        Dim proc = _ProcessLauncher.LaunchTargProc(pname, fWithDll:=True, wrkdir:=TestContext.DeploymentDirectory)
        If Not String.IsNullOrEmpty(_ProcessLauncher.LauncherErrorMessage) Then
            _VBAssert.OutputText("Process LauncherErrorMessage " + _ProcessLauncher.LauncherErrorMessage)
        End If
        If _ProcessLauncher.targProcIsDebug Then
            System.Threading.Thread.Sleep(3500)
        End If
        System.Threading.Thread.Sleep(1000)
        _VBAssert.OutputText("MemSpect vers = " + _MemSpectVersion, fAddToBaseline:=False)
        If fFreeze Then
            FrozenTarget = True
        End If
        Return proc
    End Function


    Private Sub DoNotepadstuff(Optional ByVal fStartNotepadProcess As Boolean = True,
                               Optional ByVal fDoTestDupes As Boolean = True)
        If fStartNotepadProcess Then
            StartNotepad()
        End If
        DoSpotCheckHeaps(Sub(hp As MemSnapshot)

                         End Sub, fDoAllocs:=True)

        '        DoMappedFiles("")
        DoTestHeapCreates({"_clr.dll!DebuggerHeap::Init",
                       "_mscoree.dll!_heap_init",
                       "_MSVCR100.dll!_heap_init"
                      })
        'DoTestPivot("MSCTF.dll!ActivateAssembly + 133 bytes") can't do: ret & deb stks differ for offline
        If fDoTestDupes Then
            DoTestDuplicates()
        End If

    End Sub

    Private Sub DoTestAssertSeqNoOrStackFrame(ByVal vrb As ProcMsgVerb)
        Dim vrbIndex = If(vrb = ProcMsgVerb.AssertSeqNo, 0, 1)
        For i = 1 To 100
            SendMsg(vrb, fSendEndMsgSync:=True, dwords:={0, i})
        Next
        Dim lamGetCount = Function() As Integer
                              Dim ctrls = UIBase.ShowAssertLists()
                              Dim sp As StackPanel = CType(ctrls.SurfaceDetails.Children(0), StackPanel)
                              Dim b As Browse = CType(sp.Children(vrbIndex), Browse)
                              Dim ncnt = b._BrowseList.Items.Count
                              Return ncnt
                          End Function

        _VBAssert.OutputText(vrb.ToString + " added 100 items, got back " + lamGetCount.Invoke.ToString)

        SendMsg(vrb, fSendEndMsgSync:=True, dwords:={1, 10}) ' 1 = remove 1
        _VBAssert.OutputText(vrb.ToString + " removed 1, got back " + lamGetCount.Invoke.ToString)

        SendMsg(vrb, fSendEndMsgSync:=True, dwords:={1, 10}) ' 1 = remove the same 1 again: should be noop
        _VBAssert.OutputText(vrb.ToString + " removed same one, got back " + lamGetCount.Invoke.ToString)


        SendMsg(vrb, fSendEndMsgSync:=True, dwords:={3})  ' clear them
        _VBAssert.OutputText(vrb.ToString + " Cleared them all, got back " + lamGetCount.Invoke.ToString)
    End Sub

    <TestMethod()>
    <Ignore()>
    Public Sub SymbolLoading()
        Try

            'seems like must kill qtagent32.exe before running this test
            InitTestMethodAndSetBaseLine()
            _ProcessLauncher = New ProcessLauncher() With {
                .TrackClrObjects = 0
            }
            VsClearSymbols()
            StartNotepad()
            FrozenTarget = True
            ReadHeaps()
            ShowSnapshot(ProcessHeap, ProcessHeapSnap) ' get some syms resolved

            Dim sbSearchpath = New StringBuilder(1500)
            SymGetSearchPath(_hProcessTarget, sbSearchpath, sbSearchpath.Capacity)
            _VBAssert.OutputText("Current Sym Path len= " + sbSearchpath.Length.ToString)
            _VBAssert.OutputText("Current Sym Path = " + sbSearchpath.ToString)
            Assert.IsTrue(sbSearchpath.Length > 0, "sym search path empty?")
            '            SymSetSearchPath(_hProcessTarget, "d:\MemSpect\;" + sbSearchpath.ToString)

            'SymGetSearchPath(_hProcessTarget, sbSearchpath, sbSearchpath.Capacity)

            '            System.Threading.Thread.Sleep(2000)
            _VBAssert.OutputText("Testing Symbol loading " + GetGlobalPassCount.ToString + " " + _HeapList.Count.ToString + " hproc=" + _hProcessTarget.ToInt32.ToString, fAddToBaseline:=False)

            Dim symFiles = New SymbolFiles(fIncludeAllModules:=True)



            Dim symenum = New SymbolEnumerator(_hProcessTarget, strMask:="Kernel*!*")
            For Each itm In symenum._symlist
                Dim modName = symFiles._SymFileData(itm.Value(0).syminfo.ModBase).ModInfo.ModuleName
                Dim itemno = 0
                For Each vv In itm.Value
                    itemno += 1
                    _VBAssert.OutputText(String.Format("{0:x8} Itm#={1} {2} {3} {4}", itm.Key, itemno, modName, vv.syminfo.Tag, vv.symName))
                Next
            Next

            For Each hp In _HeapList
                _VBAssert.OutputText(hp.GetHeapName)
            Next

            Dim stat = SymbolFiles.CheckSymbolFileStatus
            _VBAssert.OutputText("AggSymLoadstat = " + stat)

            Dim ctrlsSym = SymbolFileUI.CreateSymbolFileDataSurface()

            _VBAssert.OutputText("SymFileDetails = ")
            Dim br As Browse = CType(ctrlsSym.SurfaceDetails.Children(0), Browse)
            For Each itm In br._BrowseList.Items
                _VBAssert.OutputText(itm.ToString, fAddToBaseline:=False)
                '_VBAssert.OutputText(String.Format("{0} {1} {2} {3} {4}",
                '                                   itm.Key,
                '                                   itm.Value.BaseOfDll,
                '                                   itm.Value.ModInfo.ImageName,
                '                                   itm.Value.ModInfo.LoadedPDBName,
                '                                   itm.Value.ModInfo.NumSyms
                '                                   ))
            Next




        Catch ex As Exception
            HandleTestException(ex)
        Finally
            VsClearSymbols()

        End Try
    End Sub





End Class








<TestClass()>
<CLSCompliant(False)>
Public Class Life
    Inherits TestBase

    <ClassInitialize()>
    Public Shared Sub ClassInit(ByVal ctx As TestContext)
        BaseClassInitialize(ctx)
    End Sub

    <TestInitialize()>
    Public Sub TestInit()
        BaseTestInit()
        _nmSecsDelayBetweenUIDisplay = 0
    End Sub

    <TestCleanup()>
    Sub TestCleanup()

        BaseTestCleanup()

    End Sub

    '#Critsects=  509

    <TestMethod()>
    Public Sub Life()
        Try
            BaseThreadCheck(Me)
            InitTestMethodAndSetBaseLine("Life")
            _ProcessLauncher = New ProcessLauncher() With {
                .fMessageBoxOnStart = 0,
                .fTrackThreadCreate = 1,
                .TrackClrObjects = 1,
                ._nmsecsToWaitTilStart = 3000
            }
            '_ProcessLauncher._fMessageBoxOnStart = 1
            DoLifeStuff()
        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub


    <TestMethod()>
    Public Sub LifeClasses()
        Try
            BaseThreadCheck(Me)
            InitTestMethodAndSetBaseLine()
            _VBAssert.OutputText("Start LifeClasses online part")
            _ProcessLauncher = New ProcessLauncher() With {
                .fMessageBoxOnStart = 0,
                .fTrackThreadCreate = 1,
                .TrackClrObjects = 1,
                ._nmsecsToWaitTilStart = 3000
            }
            Dim snap = StartLife()


            'Dim xx = ClrClassInfo.GetClassNameFromClassOrObjectId(New IntPtr(&H1000))
            '_VBAssert.OutputText("Expected Exception " + xx)



            DoTestGCStackInfo()


            Dim allocs = From halloc In MemSpectSnap.Allocs
                   Where halloc.TBlkBlockType = BlockTypes.ClrObject
                   Let cname = halloc.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True)
                   Where cname.IndexOf("Generic") >= 0
                   Select halloc

            _VBAssert.OutputText("# of items Online = " + allocs.Count().ToString)

            For Each halloc In allocs
                DoClassLayout(halloc, fOutput:=True)
            Next

            allocs = From halloc In MemSpectSnap.Allocs
                   Where halloc.TBlkBlockType = BlockTypes.ClrClass
                   Let cname = halloc.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True)
                   Where cname.IndexOf("Form1", StringComparison.OrdinalIgnoreCase) >= 0
                   Select halloc

            _VBAssert.OutputText("# of ClrClass items Online = " + allocs.Count().ToString)

            For Each halloc In allocs
                DoClassLayout(halloc, fOutput:=True)
            Next



            CloseCommunications()
            _ProcessLauncher.ShutDownTargetProcess()
            BaseTestInitStatics()

            Common.Initialize()

            LoadOffLineSnap(IO.Path.Combine(memspectInstallDir, "Snaps\csLife"))

            allocs = From halloc In MemSpectSnap.Allocs
                   Where halloc.TBlkBlockType = BlockTypes.ClrObject
                   Let cname = halloc.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True)
                   Where cname.IndexOf("button", StringComparison.OrdinalIgnoreCase) >= 0 OrElse
                    cname.IndexOf("form1", StringComparison.OrdinalIgnoreCase) >= 0 OrElse
                    cname.IndexOf("Eventhandler", StringComparison.OrdinalIgnoreCase) >= 0
                    Select halloc

            _VBAssert.OutputText("# of items offline = " + allocs.Count().ToString)

            For Each halloc In allocs
                DoClassLayout(halloc, fOutput:=True)
            Next


        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub

    <TestMethod()>
    <Description("Compare VM file names with MemSpectWin->files")>
    Public Sub LifeVMFiles()
        InitTestMethodAndSetBaseLine()
        If False Then
            _ProcessLauncher = New ProcessLauncher() With {
                .fMessageBoxOnStart = 0,
                .fTrackThreadCreate = 1,
                .TrackClrObjects = 1,
                ._nmsecsToWaitTilStart = 3000
            }
            Dim snap = StartLife()

            ReadHeaps()

            FrozenTarget = True
        End If

        LoadOffLineSnap(IO.Path.Combine(memspectInstallDir, "Snaps\csLife"))

        Dim vallocs = GetVirtAllocs()

        Dim qva = From a In vallocs.Values
                Where (a.lType Or (AllocationType.MEM_IMAGE + AllocationType.MEM_MAPPED)) > 0
                Select
                        Filename = GetFileNameFromMBI(a),
                        _mbi = a
                        Order By Filename
        Dim last = String.Empty
        For Each f In qva
            If f.Filename <> last Then
                _VBAssert.OutputText(f._mbi.BaseAddress.ToInt32.ToString("x8") + " " + f.Filename)
                last = f.Filename
            End If
        Next
        _VBAssert.OutputText("Now from MemSpectWin")


        Dim q = From alloc In MemSpectSnap.Allocs Where
                (alloc.TBlkBlockType = BlockTypes.MapFile Or alloc.GetIndirectInfoType = IndirectInfoType.IIType_MapSection)
                Select
                    fname = alloc.GetMappedOrIndirectFileName,
                    alloc
                    Order By fname



        For Each itm In q
            _VBAssert.OutputText(itm.alloc.GetAddr.ToInt32.ToString("x8") + " " + itm.fname)
        Next

        'Dim ctrls = ShowSubSnapShot(q.ToList, "test")

        'Dim mspectWin = CType(ctrls.SurfaceDetails.Children(0), MemSpectWin)
        'Dim bmem = mspectWin._bmem

        'bmem._TabControl.SelectedIndex = 1 ' select Details tab
        'bmem.OnTabItemDetailsGotFocus(bmem._TabItemDetails, New Windows.RoutedEventArgs)



    End Sub


    <TestMethod()>
    Public Sub LifeDynStackSym()
        Try
            BaseThreadCheck(Me)
            InitTestMethodAndSetBaseLine()
            Dim ProcessLauncher = New ProcessLauncher() With {
                .fHandle4gigStacks = 1,
                ._DynStackSymRes = 1,
                .fMessageBoxOnStart = 0,
                .TrackClrObjects = 1
            }
            Dim snap = StartLife(ProcessLauncher)
            'we want a mixed stack 
            Dim alloc = MemSpectSnap.Allocs.
                Where(Function(h) h.TBlkBlockType = BlockTypes.ClrObject AndAlso
                          h.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False).Contains("System.Windows.Forms.CreateParams")).FirstOrDefault
            _VBAssert.OutputText(alloc.ToString, fAddToBaseline:=False)
            CleanAndSortStacksForBaseline("Life.form1 stack using DynStackSym", alloc.GetCallStackAsStringArray)

        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub

    <TestMethod()>
    Public Sub LifeWriteMem()
        Try
            BaseThreadCheck(Me)
            InitTestMethodAndSetBaseLine()
            Dim ProcessLauncher = New ProcessLauncher() With {
                .TrackClrObjects = 1
            }
            Dim mssnap = StartLife(ProcessLauncher)
            Dim alloc = mssnap.Allocs.
                Where(Function(h) h.TBlkBlockType = BlockTypes.ClrObject AndAlso
                          h.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True) = "System.String button1").FirstOrDefault
            _VBAssert.OutputText("Looking for button1")
            _VBAssert.OutputText(alloc.ToString, fAddToBaseline:=False)

            Dim lamShowMem = Sub(desc As String, expectedStr As String)
                                 _VBAssert.OutputText(desc)
                                 Dim dmp = GetStacksAndDump(alloc).Value.Trim
                                 _VBAssert.OutputText(dmp, cSplitChar:=CChar(vbCr), fAddToBaseline:=False)
                                 _VBAssert.IsTrue(dmp.EndsWith(expectedStr), "Expected end:" + expectedStr + "  Got: " + dmp)
                             End Sub

            lamShowMem.Invoke("Original", "b u t t o n 1")
            alloc.WriteHeapAllocationStructMemory()
            lamShowMem.Invoke("After Write", "M e m S p e c t")

        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub


    <TestMethod()>
    <Ignore()>
    Public Sub Life4GigStacks()
        Try
            BaseThreadCheck(Me)
            InitTestMethodAndSetBaseLine("Life")
            _ProcessLauncher = New ProcessLauncher() With {
                .fHandle4gigStacks = 1,
                .TrackClrObjects = 1
            }
            DoLifeStuff()
        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub

    <TestMethod()>
    Public Sub LifeClrLostObj()
        Try
            BaseThreadCheck(Me)
            InitTestMethodAndSetBaseLine("Life")
            _ProcessLauncher = New ProcessLauncher() With {
                .CheckGCLostObjects = 1,
                .TrackClrObjects = 1,
                ._nmsecsToWaitTilStart = 3000
            }
            DoLifeStuff()
        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub
    <TestMethod()>
    <Ignore>
    Public Sub LifeCompile()
        Try
            BaseThreadCheck(Me)
            InitTestMethodAndSetBaseLine()
            'AddHandler StatusMessageEvent, Sub(o, e)
            '                                   _VBAssert.OutputText(e.Message, cSplitChar:=CChar(vbCr))
            '                               End Sub
            AddHandler StatusMessageEvent, AddressOf TheStatusHandler
            _VBAssert.OutputText("Start Compile")
            _ProcessLauncher = New ProcessLauncher() With {
                .TrackClrObjects = 1,
                ._nmsecsToWaitTilStart = 3000
            }
            StartLife(fFreeze:=False)
            DoCompileTest()


        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub

    <TestMethod()>
    Public Sub LifeSnap()
        Try
            BaseThreadCheck(Me)
            InitTestMethodAndSetBaseLine()
            For i = 1 To 1
                _ProcessLauncher = New ProcessLauncher() With {
                    .CheckGCLostObjects = 1,
                    .TrackClrObjects = 1,
                    .TrackFileLoadUnload = 1
                }
                Dim snap = StartLife()
                'StartVS()
                _VBAssert.OutputText("setting LeakMultipleRawText ")
                _GlobalFilter.LeakMultipleRawText = "1,2" + vbCrLf + "3,4"
                _VBAssert.OutputText("Taking offline snap " + i.ToString)
                Dim newsnapfolder = Path.Combine(_TestContext.DeploymentDirectory, "SnapLife" + i.ToString)
                Dim snapfolder = Common.OfflineMegaSnapshot.CreateMegaSnapshot(newsnapfolder)
                _VBAssert.OutputText("Done offline snap. folder =" + snapfolder, fAddToBaseline:=False)
                ProcComm.UnFreezeTarget()

                Common.Initialize()

                CloseCommunications()
                _ProcessLauncher.ShutDownTargetProcess()

                _VBAssert.OutputText("Loading offline snap " + snapfolder, fAddToBaseline:=False)
                Dim strres = InitComm({System.Environment.GetCommandLineArgs(0), "/o", snapfolder})
                If Not String.IsNullOrEmpty(strres) Then
                    _VBAssert.OutputText("Couldn't load offline snap " + snapfolder + " " + strres)
                Else
                    CLRObjRefsWrapper.WaitTilLoaded()
                    _VBAssert.OutputText("Offline snap loaded")
                    _VBAssert.OutputText("LeakMultipleRawText =" + _GlobalFilter.LeakMultipleRawText, cSplitChar:=CChar(vbCr))
                    Dim newsnap = MemSpectSnap
                    Dim q = From a In newsnap.Allocs
                            Where a.TBlkBlockType = BlockTypes.ClrObject
                            Let clsname = a.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False)
                            Where clsname = "System.String"
                            Select a

                    _VBAssert.OutputText("# of System.String = " + q.Count().ToString, fAddToBaseline:=False)
                    _VBAssert.IsTrue(q.Count > 800, "expected # strings > 800")

                    Dim form1strAlloc = (From a In q Where a.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True).Contains("Form1")).Single
                    _VBAssert.OutputText(form1strAlloc.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True))

                    CleanAndSortStacksForBaseline("Form1 stack:", form1strAlloc.GetCallStackAsStringArray)

                    _VBAssert.OutputText("Now individual stack frames")
                    For frame = 0 To form1strAlloc.AllocationStruct.m_uicStackAddr - 1
                        Dim tmp = form1strAlloc.GetCallStackAddr(frame)
                        Dim sym = ResolveAddressToSymbol(tmp)
                        _VBAssert.OutputText(CleanLineForBaseline(sym))
                    Next


                    Dim qstr = From alloc In q
                               Let clsName = alloc.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True)
                               Where clsName.Contains("&Reset") OrElse clsName.Contains("button1")
                               Order By clsName

                    For Each alloc In qstr
                        _VBAssert.OutputText(alloc.clsName, fAddToBaseline:=True)
                    Next

                    _VBAssert.OutputText("FileLoadNotifications")

                    Dim nFileLoad = 0

                    For Each alloc In From a In newsnap.Allocs
                                        Where a.TBlkBlockType = BlockTypes.IndirectInfo AndAlso
                                        a.GetIndirectInfoType = IndirectInfoType.IIType_FileLoad

                        nFileLoad += 1
                        _VBAssert.OutputText(alloc.ToString, fAddToBaseline:=False)
                    Next
                    _VBAssert.IsTrue(nFileLoad > 10, "Didn't get 10 file loads, got " + nFileLoad.ToString)
                End If

                Common.Initialize()
                CloseCommunications()

                '                Directory.Delete(snapfolder, recursive:=True)
                'Dim lamDelFiles = Sub(fldr As String)
                '                      For Each File In IO.Directory.GetFiles(fldr)
                '                          Dim x = New FileInfo(File)
                '                          x.Attributes = FileAttributes.Normal
                '                          IO.File.Delete(File)
                '                      Next
                '                      IO.Directory.Delete(fldr)
                '                  End Sub
                'lamDelFiles.Invoke(Path.Combine(snapfolder, "MemSpect"))
                'lamDelFiles.Invoke(snapfolder)
            Next

        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub

    <TestMethod()>
    Public Sub LifeStackStore()
        Try
            BaseThreadCheck(Me)
            InitTestMethodAndSetBaseLine()
            For i = 1 To 1
                _ProcessLauncher = New ProcessLauncher() With {
                    .CheckGCLostObjects = 1,
                    .TrackClrObjects = 1,
                    .TrackFileLoadUnload = 1,
                    .StackStorageMode = 1
                }
                Dim snap = StartLife()
                _VBAssert.OutputText("Taking offline snap " + i.ToString)
                Dim newsnapfolder = Path.Combine(_TestContext.DeploymentDirectory, "SnapStackStore" + i.ToString)
                Dim snapfolder = Common.OfflineMegaSnapshot.CreateMegaSnapshot(newsnapfolder)
                _VBAssert.OutputText("Done offline snap. folder =" + snapfolder, fAddToBaseline:=False)
                ProcComm.UnFreezeTarget()

                Common.Initialize()

                CloseCommunications()
                _ProcessLauncher.ShutDownTargetProcess()

                _VBAssert.OutputText("Loading offline snap " + snapfolder, fAddToBaseline:=False)
                Dim strres = InitComm({System.Environment.GetCommandLineArgs(0), "/o", snapfolder})
                If Not String.IsNullOrEmpty(strres) Then
                    _VBAssert.OutputText("Couldn't load offline snap " + snapfolder + " " + strres)
                Else
                    CLRObjRefsWrapper.WaitTilLoaded()
                    _VBAssert.OutputText("Offline snap loaded")
                    Dim newsnap = MemSpectSnap
                    Dim q = From a In newsnap.Allocs
                            Where a.TBlkBlockType = BlockTypes.ClrObject
                            Let clsname = a.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False)
                            Where clsname = "Life.Form1"
                            Select a

                    Dim form1strAlloc = (From a In q Where a.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True).Contains("Form1")).Single
                    _VBAssert.OutputText(form1strAlloc.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True))

                    CleanAndSortStacksForBaseline("Form1 stack:", form1strAlloc.GetCallStackAsStringArray)

                    _VBAssert.OutputText("Now individual stack frames")
                    For frame = 0 To form1strAlloc.AllocationStruct.m_uicStackAddr - 1
                        Dim tmp = form1strAlloc.GetCallStackAddr(frame)
                        Dim sym = ResolveAddressToSymbol(tmp)
                        _VBAssert.OutputText(CleanLineForBaseline(sym))
                    Next

                End If

                Common.Initialize()
                CloseCommunications()

                '                Directory.Delete(snapfolder, recursive:=True)
                'Dim lamDelFiles = Sub(fldr As String)
                '                      For Each File In IO.Directory.GetFiles(fldr)
                '                          Dim x = New FileInfo(File)
                '                          x.Attributes = FileAttributes.Normal
                '                          IO.File.Delete(File)
                '                      Next
                '                      IO.Directory.Delete(fldr)
                '                  End Sub
                'lamDelFiles.Invoke(Path.Combine(snapfolder, "MemSpect"))
                'lamDelFiles.Invoke(snapfolder)
            Next

        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub



    <TestMethod()>
    Public Sub LifeGCRoot()
        Try
            BaseThreadCheck(Me)
            InitTestMethodAndSetBaseLine()
            '_ProcessLauncher = New ProcessLauncher() With {
            '        .CheckGCLostObjects = 1,
            '        .TrackClrObjects = 1
            '    }
            'Dim snap = StartLife()
            LoadOffLineSnap(IO.Path.Combine(memspectInstallDir, "Snaps\csLife"))
            _VBAssert.OutputText("Loaded Snaps\csLife")
            For Each itm In From a In GCData.GetGCRootInfo(fIncludeDupes:=False) Order By a
                _VBAssert.OutputText("GCRoot " + itm.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False) + " " + itm.ToString + " " + itm.GetGCRootExtraInfo)
            Next

            Dim targObjStrs = {"System.Windows.Forms.TextBox", "Life.Form1"}
            For Each targStr In targObjStrs
                Dim targObjStr = targStr
                _VBAssert.OutputText("")
                Dim oTarg = (From a In MemSpectSnap.Allocs
                             Where a.TBlkBlockType = BlockTypes.ClrObject
                          Where a.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False) = targObjStr).First
                _VBAssert.OutputText("Getting GCRoots For " + targObjStr + " = " + oTarg.ToString, fAddToBaseline:=True)

                Dim reslist = oTarg.GetGCRootPaths(Sub(s As String)
                                                       _VBAssert.OutputText(s)
                                                   End Sub)
                For Each lst In reslist
                    _VBAssert.OutputText("GotRoots: " + targObjStr)
                    For i = 1 To lst.Count
                        Dim itm = lst(i - 1)
                        _VBAssert.OutputText("  " + i.ToString + " " + itm.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False) + " " + itm.ToString)
                    Next
                Next
                _VBAssert.OutputText(" ")
                _VBAssert.OutputText(" ")



                'Dim ctrls = TVObjRefPanel.CreateObjectReferenceData(oLife, targObj)
                'Dim otvobjrefPanel = CType(ctrls.SurfaceDetails.Children(0), TVObjRefPanel)
                'Dim tvObjref = CType(otvobjrefPanel.Children(0), TVObjRefPanel.TVObjRef)


                'Dim orefdata = TVObjRefPanel.TVObjRef.GetObjectRefData(oTarg, brtypeRequest:=TVObjRefPanel.branchtype.PathToGCRoot)

                'For Each lstAlloc In orefdata.Skip(2)
                '    _VBAssert.OutputText("GCRootPath " + targObjStr)
                '    For Each alloc In lstAlloc
                '        _VBAssert.OutputText("  " + alloc.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False) + " " + alloc.ToString)

                '    Next
                'Next


            Next

        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub

    <TestMethod()>
    <Description("Export offline snap to PerfView")>
    Public Sub LifePerfViewExport()
        Try
            BaseThreadCheck(Me)
            InitTestMethodAndSetBaseLine()

            LoadOffLineSnap(IO.Path.Combine(memspectInstallDir, "Snaps\csLife"), fClearFilterToo:=True)
            _VBAssert.OutputText("Exporting offline snap to PerfView")
            Dim tmpxmlfile = IO.Path.ChangeExtension(IO.Path.GetTempFileName, "xml")
            PerfViewExport.PerfViewExportFile(hCtrList:=Nothing, outputFileName:=tmpxmlfile)
            For Each line In IO.File.ReadAllLines(tmpxmlfile)
                _VBAssert.OutputText(line)
            Next


        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub


    <TestMethod()>
    Public Sub LifeImages()
        Try
            BaseThreadCheck(Me)
            InitTestMethodAndSetBaseLine()
            _ProcessLauncher = New ProcessLauncher() With {
                .TrackClrObjects = 1,
                ._nmsecsToWaitTilStart = 3000
            }
            StartLife()

            DoTestImages()


        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub

    <TestMethod()>
    Public Sub LifeImagesOffLine()
        Try
            BaseThreadCheck(Me)
            InitTestMethodAndSetBaseLine()
            _ShowUI = True ' want to test valueconverters too
            LoadOffLineSnap(IO.Path.Combine(memspectInstallDir, "Snaps\csLife"))

            DoTestImages()


        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub

    Public ReadOnly Property GetLifeExeName As String
        Get
            Return IO.Path.Combine(memspectInstallDir, "test\cslife.exe")
        End Get
    End Property

    Public _hProcLife As Process


    Private Function StartLife(Optional ByVal pLauncher As ProcessLauncher = Nothing, Optional fFreeze As Boolean = True) As MemSnapshot
        If pLauncher IsNot Nothing Then
            _ProcessLauncher = pLauncher
        End If
        '        _ProcessLauncher.ShowSymbolLoadStatus = 5
        Debug.Assert(_ProcessLauncher IsNot Nothing, "no process launcher?")
        Dim pname = GetLifeExeName
        CommonUI.InitializeCommonUI()
        AddHandler MemSpect.Common.StatusMessageEvent, AddressOf TheStatusHandler

        _hProcLife = _ProcessLauncher.LaunchTargProc(pname, fWithDll:=True, wrkdir:=TestContext.DeploymentDirectory)
        _VBAssert.OutputText("MemSpect vers = " + _MemSpectVersion, fAddToBaseline:=False)
        If _ProcessLauncher.targProcIsDebug Then
            System.Threading.Thread.Sleep(2000)
        End If
        System.Threading.Thread.Sleep(1000)
        If fFreeze Then
            FrozenTarget = True
            _VBAssert.OutputText("Ini file = " + _iniFileName, fAddToBaseline:=False)
            VBDiagMarginBase.ShowSymbolPath()
            ReadHeaps()
            Return MemSpectSnap
        End If
        Return Nothing
    End Function

    Private Sub DoTestClassNameExpansion(ByVal snap As MemSnapshot)

        'test string expansion
        Dim qString = From alloc In snap.Allocs
                      Where alloc.TBlkBlockType = BlockTypes.ClrObject
                      Let strName = alloc.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True)
                      Where strName.StartsWith("System.String")
                      Order By strName
                      Select strName, alloc

        _VBAssert.IsTrue(qString.Count > 900, "Expected > 900 strings. Got " + qString.Count.ToString)

        _VBAssert.IsTrue(qString.Where(Function(obj) As Boolean
                                           If obj.strName.ToLower = "system.string cslife.exe" Then
                                               Return True
                                           End If
                                           Return False
                                       End Function).Count() = 1, "Couldn't find stirng 'System.String csLife.exe'")
        For Each clsname In qString
            If Not (clsname.strName.Contains(vbCr) OrElse clsname.strName.Contains(vbLf) OrElse clsname.strName.StartsWith("System.String GDI+Atom")) Then
                'System.String 6 : 7889   Generation, cnt
                If Not RegularExpressions.Regex.IsMatch(clsname.strName, "System.String [0-9]+ : [0-9]+") Then
                    Dim str = clsname.strName
                    Dim ndxp = str.IndexOf("d:\memspect\test", StringComparison.OrdinalIgnoreCase)
                    If ndxp > 0 Then
                        str = "System.String " + str.Substring(ndxp).ToLower
                    End If
                    _VBAssert.OutputText(str)

                End If
            End If


        Next

        Dim qArray = From alloc In snap.Allocs
                     Where alloc.TBlkBlockType = BlockTypes.ClrObject
                     Let classname = alloc.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True)
                     Where classname.EndsWith(")") AndAlso classname.Contains("(Count=")
                     Order By classname
                     Select classname, alloc

        _VBAssert.OutputText("Array name and size")

        For Each arrayObj In qArray
            '==>Expected: "System.Object[](Count=1020)"
            '==>Actual  : "System.Object[](Count=10)"
            ' we're asynch interrupting Life, so could be either
            Dim fAdd = Not arrayObj.classname.Contains("System.Object[](Count=10")
            _VBAssert.OutputText(arrayObj.classname, fAddToBaseline:=fAdd)
        Next

    End Sub

    Private Sub DoTestObjRefs(ByVal fDoCombineChildren As Boolean)

        Dim q = From alloc In MemSpectSnap.Allocs Where
        alloc.TBlkBlockType = BlockTypes.ClrObject
        Where alloc.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False) = "System.Windows.Forms.TextBox"
        Select alloc

        _VBAssert.IsTrue(q.Count = 1, "Should be 1 textbox")
        If q.Count <> 1 Then
            Return
        End If
        Dim hctr = q.First

        Dim classname = hctr.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True)
        classname = String.Format("{0} 0x{1:x8} ({2:n0})", classname, hctr.GetAddr.ToInt32, hctr.GetSize)

        _ShowUI = True ' for tooltip showing
        Dim ctrls = TVObjRefPanel.CreateObjectReferenceDataSurface(hctr, classname)
        Dim otvobjrefPanel = CType(ctrls.SurfaceDetails.Children(0), TVObjRefPanel)
        Dim tvObjref = CType(otvobjrefPanel.Children(0), TVObjRefPanel.TVObjRef)
        ' tvObjref.AddGCRootPaths(hctr)

        _VBAssert.OutputText("ObjRefs for System.Windows.Forms.TextBox")

        _strTVItemsToInclude = {
    "System.Collections.Queue",
    "ComponentModel",
    "ListEntry",
    "WndProc",
    "ControlNativeWindow",
    "SolidBrush",
    "Life.Form1",
    "System.Windows.Forms.TextBox"
    }

        Dispatcher.CurrentDispatcher.Invoke(DispatcherPriority.Render, Function() Nothing)
        Dim nLoopTimes = 1
        If fDoCombineChildren Then
            nLoopTimes = 2
        End If
        For nloop = 1 To nLoopTimes
            If nloop = 2 Then
                TVObjRefPanel._fCombineFromMeToMe = True
            End If
            Dim fDidGetTip = False
            For Each item In tvObjref.Items
                Dim tvitem = CType(item, MemSpect.TVObjRefPanel.TVObjRef.TVObjRefTVItem)
                If Not fDidGetTip Then
                    fDidGetTip = True
                    _VBAssert.OutputText("TVItem tip Before = " + If(tvitem.ToolTip Is Nothing, "null", tvitem.ToolTip.ToString))
                    tvObjref.RaiseEvent(New MouseEventArgs(
                                        Mouse.PrimaryDevice, 0) With
                                        {
                                            .RoutedEvent = TreeViewItem.MouseMoveEvent,
                                            .Source = CType(tvitem.Header, StackPanel).Children(0)}
                                        )
                    _VBAssert.IsNotNull(tvObjref._LastTipObj, "no tooltip obj " + ((New StackTrace).GetFrames(0).GetMethod.Name))

                    _VBAssert.IsNotNull(tvObjref._LastTipObj.ToolTip, "no tooltip " + ((New StackTrace).GetFrames(0).GetMethod.Name))

                    _VBAssert.OutputText("TVItem tip After = " + CType(tvObjref._LastTipObj.ToolTip, ToolTip).Content.ToString)
                    Dim spttipcontent = CType(CType(tvObjref._LastTipObj.ToolTip, ToolTip).Content, StackPanel)
                    Dim tipText = CType(spttipcontent.Children(0), TextBlock).Text
                    For Each line In tipText.Split({CChar(vbCr), CChar(vbLf)}, StringSplitOptions.RemoveEmptyEntries)
                        _VBAssert.OutputText(CleanLineForBaseline(line))
                    Next

                    DoTestAddressTip(spttipcontent, 1) '1 extra child at beginning
                End If
                GetTVItemInfo(tvitem, 0)
            Next
        Next
        TVObjRefPanel._fCombineFromMeToMe = False

        Dim gcRootsDataSurface = TVObjRefPanel.ShowGCRoots()
        Dim qGCRoots = From alloc In GCData.GetGCRootInfo
                       Where alloc.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False) = "Life.Form1"
                       Select
                       Stack = alloc.GetCallStackAsString,
                       RootClassName = alloc.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True),
                       RootKind = alloc.GetGCRootKind
                       Where RootKind <> "Stack"

        _VBAssert.OutputText("GCRoots for Life.Form1")
        For Each root In qGCRoots
            '            If root.RootClassName <> "Life.Form1" Then
            CleanAndSortStacksForBaseline("GCRoot " + root.RootKind + " " + CleanLineForBaseline(root.RootClassName), {root.Stack})
            '           End If
        Next

        '            DoTestPivot("KERNELBASE.dll!LoadLibraryExW + 376 bytes")

        '            DoTestDuplicates(nSizeGreaterThan:=60)
        _VBAssert.OutputText("GetCLRObjectRefs")
        Dim reflist = GCData.GetCLRObjectRefDict()
        _VBAssert.OutputText("GetCLRObjectRefs reflist count=" + reflist.Count.ToString, fAddToBaseline:=False)
        _VBAssert.IsTrue(reflist.Count > 2000, "count sohuld be > 2000")
        _VBAssert.OutputText("GetCLRObjectRefs gcrootlist count=" + GCData.GetGCRootInfo.Count.ToString, fAddToBaseline:=False)
        _VBAssert.IsTrue(GCData.GetGCRootInfo.Count > 60, "count sohuld be > 60")


    End Sub

    Private Sub DoLifeStuff()
        Dim snap = StartLife()

        Dim q = From alloc In snap.Allocs Where
                alloc.TBlkBlockType = BlockTypes.ClrObject
                Where alloc.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False) = "System.Windows.Forms.Button"
                Select alloc.GetCallStackAsString

        Dim ctrls = ShowSubSnapShot(snap.Allocs, "test")
        Dim mspectWin = CType(ctrls.SurfaceDetails.Children(0), MemSpectWin)
        Dim bmem = mspectWin._bmem

        bmem._TabControl.SelectedIndex = 1 ' select Details tab
        bmem.OnTabItemDetailsGotFocus(bmem._TabItemDetails, New Windows.RoutedEventArgs)



        CleanAndSortStacksForBaseline("Stacks for System.Windows.Forms.Button", q)

        DoTestClassNameExpansion(snap)

        DoTestMappedFiles("")

        DoTestHeapCreates({
"_clr.dll!DebuggerHeap::Init+25bytes",
"_clr.dll!DllPreInit::DllPreInit+20bytes",
"_gdiplus.dll!InternalGdiplusStartup+38bytes",
"_MSVCR100_CLR0400.dll!_heap_init+15bytes"
                      }
            )
        DoTestVirtualMemory({"cslife.exe", "user32.dll"})

    End Sub

    <TestMethod()>
    <Description("find refs to System.Windows.Forms.TextBox")>
    Public Sub LifeObjRefs()
        Try
            InitTestMethodAndSetBaseLine("LifeObjRefs")
            _ProcessLauncher = New ProcessLauncher() With {
                .CheckGCLostObjects = 1,
                .TrackClrObjects = 1,
                ._nmsecsToWaitTilStart = 3000
            }
            Dim snap = StartLife()

            Dim ctrls = ShowSubSnapShot(snap.Allocs, "test")
            Dim mspectWin = CType(ctrls.SurfaceDetails.Children(0), MemSpectWin)
            Dim mswinTypes = mspectWin.GetType
            Dim cntTot = 0
            Dim cntAll = 0
            For Each typ In mswinTypes.GetMethods(BindingFlags.Instance Or BindingFlags.NonPublic) ' friend members
                If typ.Name.StartsWith("get__btn") Then
                    Dim btnMeth = CType(typ, MethodInfo)
                    Dim btnName = typ.Name.Replace("get__btn", "")
                    Dim btn = CType(btnMeth.Invoke(mspectWin, Nothing), RadioButton)
                    '                    btn.RaiseEvent(New RoutedEventArgs With {.RoutedEvent = RadioButton.CheckedEvent})
                    btn.IsChecked = True
                    Dim tv = mspectWin._bmem._tvPanel._tv
                    Dim cntCat = 0
                    For Each itm As TVStackAggPanel.StackAggTreeView.StackAggTViewItem In tv.Items
                        cntCat += itm._memNode._Cnt
                    Next
                    _VBAssert.OutputText("" + btnName + " " + cntCat.ToString, fAddToBaseline:=False)
                    If btnName = "All" Then
                        cntAll = cntCat
                    Else
                        cntTot += cntCat
                    End If
                    Dim bmem = mspectWin._bmem
                    Select Case btnName
                        Case "All"
                        Case "Files"
                            _VBAssert.IsTrue(cntCat > 22, btnName + " # >22" + " Actual = " + cntCat.ToString)

                        Case "VirtualAllocs"
                            _VBAssert.IsTrue(cntCat > 50, btnName + " # >50" + " Actual = " + cntCat.ToString)
                        Case "HeapCreates"
                            _VBAssert.IsTrue(cntCat >= 4, btnName + " # >=4" + " Actual = " + cntCat.ToString)
                        Case "GCStackInfo"
                            _VBAssert.IsTrue(cntCat > 1, btnName + " # > 1" + " Actual = " + cntCat.ToString)
                        Case "ClrLoads"
                            _VBAssert.IsTrue(cntCat >= 10, btnName + " # >=10" + " Actual = " + cntCat.ToString)
                        Case "ClrClasses"
                            bmem._TabControl.SelectedIndex = 1 ' select Details tab
                            bmem.OnTabItemDetailsGotFocus(bmem._TabItemDetails, New Windows.RoutedEventArgs)
                            For Each itm In bmem._DetailBrowse._BrowseList.Items
                                Dim hctr = HeapAllocationContainer.CreateFrom(itm)
                                Dim clsName = hctr.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False)
                                If "System.String Life.Form1 System.Collections.Hashtable".Contains(clsName) Then
                                    Dim cl = hctr.GetClassLayoutFromClassIdForHCtr()
                                    Dim fAddToBaseline = True
                                    If clsName = "System.String" Then
                                        fAddToBaseline = False
                                        _VBAssert.IsTrue(cl.classNumInstances > 900, clsName + " # inst>900" + " Actual = " + cl.classNumInstances.ToString)
                                        _VBAssert.IsTrue(cl.classNumCollected > 260, clsName + " # coll>260" + " Actual = " + cl.classNumCollected.ToString)

                                    End If



                                    _VBAssert.OutputText(
                                        String.Format(
                                            "# {0} instances = {1} # coll = {2}",
                                            clsName,
                                            cl.classNumInstances.ToString,
                                            cl.classNumCollected.ToString), fAddToBaseline)
                                End If
                            Next

                        Case "ClrObjects"
                            _VBAssert.IsTrue(cntCat > 2200, btnName + " # >2200" + " Actual = " + cntCat.ToString)
                        Case "CodeMarker"
                            _VBAssert.IsTrue(cntCat = 1, btnName + " # =1" + " Actual = " + cntCat.ToString)
                        Case "ThreadInfo"
                            _VBAssert.IsTrue(cntCat >= 8, btnName + " # >8" + " Actual = " + cntCat.ToString)
                        Case "LoadResourceInfo"
                            _VBAssert.IsTrue(cntCat = 0, btnName + " # =0" + " Actual = " + cntCat.ToString)
                        Case "ClrJit"
                            _VBAssert.IsTrue(cntCat > 9, btnName + " # >9" + " Actual = " + cntCat.ToString)
                        Case "ClrGCHnd"
                            _VBAssert.IsTrue(cntCat > 60, btnName + " # >60" + " Actual = " + cntCat.ToString)
                        Case "ClrExcpt"
                            _VBAssert.IsTrue(cntCat >= 0, btnName + " # >=0" + " Actual = " + cntCat.ToString)
                        Case "GdiObjs"
                        Case Else
                            _VBAssert.OutputText("unknown button " + btnName)
                    End Select
                End If
            Next
            _VBAssert.IsTrue(cntTot = cntAll, "cntAll <> cntTot? " + cntTot.ToString + " " + cntAll.ToString)

            Dim clsNameForUnused = "System.Configuration.FactoryRecord"
            Dim allocs = From a In MemSpectSnap.Allocs.Where(Function(a) a.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False) = clsNameForUnused)

            DoTestUnusedMembers(allocs, clsNameForUnused)

            DoTestObjRefs(fDoCombineChildren:=True)

            _VBAssert.OutputText("Now test FreeStackMem")

            Dim nFreed = FreeStackMemory(ProcessHeap)

            _VBAssert.OutputText(
                String.Format("'{0}' # freed stackframes = {1} ({2} bytes)", ProcessHeap.HeapName, nFreed, nFreed * 4),
                fAddToBaseline:=False)
            _VBAssert.IsTrue(nFreed > 50000, "# freed > 50000")


        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub

    <TestMethod()>
    <Description("find refs to System.Windows.Forms.TextBox")>
    Public Sub LifeObjSubSum()
        Try
            InitTestMethodAndSetBaseLine()
            LoadOffLineSnap(IO.Path.Combine(memspectInstallDir, "Snaps\csLife"))

            Dim cName = "System.Windows.Forms.TextBox"

            Dim hctrTextbox = (From alloc In MemSpectSnap.Allocs
                            Where alloc.TBlkBlockType = BlockTypes.ClrObject
                            Where alloc.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False) = cName
                            ).First
            _VBAssert.OutputText(hctrTextbox.ToString)
            Dim ctrls = TVObjRefPanel.CreateObjectReferenceDataSurface(hctrTextbox, cName)
            Dim otvobjrefPanel = CType(ctrls.SurfaceDetails.Children(0), TVObjRefPanel)
            Dim tvObjref = CType(otvobjrefPanel.Children(0), TVObjRefPanel.TVObjRef)

            Dim firstItem = tvObjref.Items(0)
            _VBAssert.OutputText(firstItem.ToString)

            CType(tvObjref.Items(0), TVObjRefPanel.TVObjRef.TVObjRefTVItem).IsSelected = True

            InvokeContextMenu(tvObjref.ContextMenu, "_SubSnapshot all objs only ref'd by this")
            Dim ctrlsSnap = DataWindowMain.GetDataSurface
            Dim mswin = CType(ctrlsSnap.SurfaceDetails.Children(0), MemSpectWin)
            Dim bmemSubSnp = mswin._bmem
            bmemSubSnp._TabItemDetails.Focus()
            _VBAssert.OutputText("Got subsnap #items = " + bmemSubSnp._DetailBrowse._BrowseList.Items.Count.ToString)
            _VBAssert.OutputText("# nodes visited = " + HeapAllocationContainer._nNodesVisited.ToString)
            For Each itm In bmemSubSnp._DetailBrowse._BrowseList.Items
                _VBAssert.OutputText(itm.ToString)
            Next

            _VBAssert.OutputText(String.Format("Getting objs reachable from {0}", hctrTextbox))

            InvokeContextMenu(tvObjref.ContextMenu, "_SubSnapshot Children recursively")
            Dim ctrlsChildren = DataWindowMain.GetDataSurface
            Dim bmemSubSnpChildren = CType(ctrlsChildren.SurfaceDetails.Children(0), BrowseMem)
            bmemSubSnpChildren._TabItemDetails.Focus()
            bmemSubSnpChildren.OnTabItemDetailsGotFocus(bmemSubSnpChildren._DetailBrowse, New RoutedEventArgs)
            _VBAssert.OutputText(CType(ctrlsChildren.SurfaceHeader.Children(0), TextBlock).Text)
            _VBAssert.OutputText("Got SubSnapChildren # items = " + bmemSubSnpChildren._DetailBrowse._BrowseList.Items.Count.ToString)
            For Each itm In bmemSubSnpChildren._DetailBrowse._BrowseList.Items
                _VBAssert.OutputText(itm.ToString)
            Next
        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub



    Private Sub TestExpandAll()
        _VBAssert.OutputText("test Expand all summary", fAddToBaseline:=False)
        Dim msAllocs = MemSpectSnap.Allocs

        Dim q = (From alloc In msAllocs
                Where alloc.TBlkBlockType = BlockTypes.ClrObject AndAlso
                        alloc.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False) = "System.Windows.Forms.Button")
        Dim lisq = q.ToList
        _VBAssert.OutputText("# buttons found in list = " + lisq.Count.ToString)

        MemSpectWin._btnLastSelected = TrkType.ClrObjects
        Dim ctrls = ShowSubSnapShot(q.ToList, "SubSnapShot button")
        Dim mspectWin = CType(ctrls.SurfaceDetails.Children(0), MemSpectWin)
        Dim bmem = mspectWin._bmem

        bmem._TabControl.SelectedIndex = 1 ' select Details tab
        bmem.OnTabItemDetailsGotFocus(bmem._TabItemDetails, New Windows.RoutedEventArgs)
        _VBAssert.OutputText("# buttons found in snap = " + bmem._DetailBrowse._BrowseList.Items.Count.ToString)
        bmem._DetailBrowse._BrowseList.SelectedIndex = 0 ' select 1st
        InvokeContextMenu(bmem._DetailBrowse._BrowseList.ContextMenu, "_References")

        ctrls = DataWindowMain.GetDataSurface
        Dim tvobjrefPanel = CType(ctrls.SurfaceDetails.Children(0), TVObjRefPanel)
        Dim tvObjRef = CType(tvobjrefPanel.Children(0), TVObjRefPanel.TVObjRef)
        CType(tvObjRef.Items(0), TVObjRefPanel.TVObjRef.TVObjRefTVItem).IsSelected = True
        InvokeContextMenu(tvObjRef.ContextMenu, "_Expand SubTree")
        Dim sumry = tvobjrefPanel.TVObjRef._TotalsForExpandAll
        _VBAssert.OutputText(String.Format("button children: cnt = {0:n0} Size = {1:n0}", sumry.TotCnt, sumry.TotSize))
        For Each itm In tvObjRef.Items
            '_VBAssert.OutputText(itm.ToString)
            GetTVItemInfo(CType(itm, TVObjRefPanel.TVObjRef.TVObjRefTVItem), 0, fExpandIfNecessary:=False)
        Next

    End Sub

    Private Function GetTestNameFromstack() As String
        Dim ret = String.Empty
        Dim s = New StackTrace()
        Dim curasm = Assembly.GetExecutingAssembly.GetName.Name ' MemSpect.Test
        ret = curasm
        ret += vbCrLf
        'Dim q = From f In s.GetFrames() Take While f
        Dim priorFr As StackFrame = Nothing
        For Each fr In s.GetFrames
            If Not fr.GetMethod.Module.Name.StartsWith(curasm) Then
                Exit For
            End If
            ret += String.Format("{0} {1}" + vbCrLf, fr.GetMethod().Module.Name, fr.ToString)
            priorFr = fr
        Next
        ret += vbCrLf
        ret += s.ToString()

        ret = priorFr.GetMethod.Name
        Return ret
    End Function

    <TestMethod()>
    Sub LifeOffline()
        Try
            InitTestMethodAndSetBaseLine()
            '_VBAssert.OutputText(GetTestNameFromstack)

            LoadOffLineSnap(IO.Path.Combine(memspectInstallDir, "Snaps\csLife"), fClearFilterToo:=False)
            _VBAssert.OutputText("Got globalfilter seqnos " + _GlobalFilter._LeakMultipleRawText, cSplitChar:=CChar(vbCr))
            _GlobalFilter.ClearFilter()

            DoTestWastedMemory(nMatchCnt:=17, nItemsExpected:=22, fDumpIt:=True)


            Dim snap = ProcessHeap.TakeMemSnapshot(fEnableFilter:=True)
            _VBAssert.OutputText("testing mem dump")

            Dim smallAllocs = snap.Allocs.Where(Function(h) h.GetSize Mod 4 <> 0).OrderBy(Function(h) h.GetSize)
            Dim nPriorSize = 0
            For Each alloc In smallAllocs '.Where(Function(h) h.GetSize = 6)
                If nPriorSize <> alloc.GetSize Then
                    Dim dmp = GetMemoryDump(alloc.GetAddr, alloc.GetSize)
                    _VBAssert.OutputText("Dump of size " + alloc.GetSize.ToString + " " + "SeqNo=" + alloc.AllocationStruct.SeqNo.ToString)
                    _VBAssert.OutputText(dmp, cSplitChar:=CChar(vbCr))
                    nPriorSize = alloc.GetSize
                End If
            Next

            _VBAssert.OutputText("testing FilterUI")
            _VBAssert.OutputText("UnFiltered snap cnt = " + snap.Allocs.Count.ToString)
            _GlobalFilter.SrchString = "ClassLoader::RunMain"
            snap = ProcessHeap.TakeMemSnapshot(fEnableFilter:=True)
            _VBAssert.OutputText("Filtered snap " + _GlobalFilter.ToString + " cnt = " + snap.Allocs.Count.ToString)
            _GlobalFilter.ClearFilter()
            _GlobalFilter.SeqNoLo = 1000
            _GlobalFilter.SeqNoHi = 2000
            snap = ProcessHeap.TakeMemSnapshot(fEnableFilter:=True)
            _VBAssert.OutputText("Filtered snap " + _GlobalFilter.ToString + " cnt = " + snap.Allocs.Count.ToString)


            _GlobalFilter.ClearFilter()
            Dim clsToUse = "System.RuntimeType"
            _VBAssert.OutputText("Now test unused and MemberDistribution for " + clsToUse)
            Dim allocs = From a In MemSpectSnap.Allocs.Where(Function(a) a.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False) = clsToUse)

            DoTestUnusedMembers(allocs, clsToUse)

            For offset = 0 To allocs(0).GetSize Step 4
                Dim ctrlsMemDist = ShowMemberDistribution(allocs, offset, "testMemDist")
                Dim brMemDist = CType(ctrlsMemDist.SurfaceDetails.Children(0), Browse)
                _VBAssert.OutputText("MemDist item Offset = " + offset.ToString + " cnt = " + brMemDist._BrowseList.Items.Count.ToString)

                For Each itm In brMemDist._BrowseList.Items
                    _VBAssert.OutputText(itm.ToString)
                Next
            Next



            _VBAssert.OutputText("ClrClass Instance/collections")
            Dim q = From alloc In MemSpectSnap.Allocs Where
                alloc.TBlkBlockType = BlockTypes.ClrClass

            MemSpectWin._btnLastSelected = TrkType.ClrClasses
            Dim ctrls = ShowSubSnapShot(q.ToList, "SubSnapShot ClrClasses")
            Dim mspectWin = CType(ctrls.SurfaceDetails.Children(0), MemSpectWin)

            Dim bmem = mspectWin._bmem

            bmem._TabItemDetails.Focus()
            '            bmem._TabItemDetails.RaiseEvent(New RoutedEventArgs With {.RoutedEvent = TabItem.GotFocusEvent})

            For Each itm In bmem._DetailBrowse._BrowseList.Items
                _VBAssert.OutputText(itm.ToString)
            Next

            _VBAssert.OutputText("Thread Distribution")


            Dim tbthreadDist = CType((
                    From titem In bmem._TabControl.Items
                    Where CStr(CType(titem, TabItem).Tag) = "Thread Distribution"
                    ).FirstOrDefault, 
                TabItem)

            tbthreadDist.RaiseEvent(New Windows.RoutedEventArgs With {.RoutedEvent = TabItem.GotFocusEvent})
            Dim brThrd = CType(tbthreadDist.Content, Browse)

            For i = 1 To 1
                brThrd._BrowseList.SelectedItems.Add(brThrd._BrowseList.Items(i))
            Next


            For Each itm In brThrd._BrowseList.Items
                _VBAssert.OutputText(itm.ToString)
            Next

            _VBAssert.OutputText("ThreadDist subsnap")
            MemSpectWin._btnLastSelected = TrkType.ClrClasses
            InvokeContextMenu(brThrd._BrowseList.ContextMenu, "_SubSnapshot")
            Dim ctrlsThrdSubSnap = DataWindowMain.GetDataSurface
            Dim bmemThreadSubSnp = CType(ctrlsThrdSubSnap.SurfaceDetails.Children(0), BrowseMem)
            bmemThreadSubSnp._TabItemDetails.Focus()
            bmemThreadSubSnp.OnTabItemDetailsGotFocus(bmemThreadSubSnp._DetailBrowse, New RoutedEventArgs)

            For Each itm In bmemThreadSubSnp._DetailBrowse._BrowseList.Items
                _VBAssert.OutputText(itm.ToString)
            Next



            _VBAssert.OutputText("Class Layout")
            q = From alloc In MemSpectSnap.Allocs
              Where alloc.TBlkBlockType = BlockTypes.ClrObject AndAlso alloc.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True).Contains("Form1")

            DoClassLayout(q.First, fOutput:=True, fCleanLineForBaseline:=False)


            DoTestObjRefs(fDoCombineChildren:=False)

            DoTestClassNameExpansion(MemSpectSnap)

            DoTestVMUI(fManagedToo:=True)
            '_ShowUI = True
            '_nmSecsDelayBetweenUIDisplay = 4000
            TestExpandAll()


            Dim ctrlsProcHeap = ShowSubSnapShot(ProcessHeapSnap.Allocs, "Proc Heap")
            _VBAssert.OutputText(" Size dist of proc heap")
            Dim psnapBrowMem = CType(ctrlsProcHeap.SurfaceDetails.Children(0), BrowseMem)
            Dim tbsizeDist = CType((From titem In psnapBrowMem._TabControl.Items
                                    Where CStr(CType(titem, TabItem).Tag) = "Size Distribution").FirstOrDefault, TabItem)

            tbsizeDist.RaiseEvent(New Windows.RoutedEventArgs With {.RoutedEvent = TabItem.GotFocusEvent})
            Dim brSizeDist = CType(tbsizeDist.Content, Browse)
            For Each itm In brSizeDist._BrowseList.Items
                _VBAssert.OutputText(itm.ToString)
            Next

            _VBAssert.OutputText("Size Dist subsnap")
            brSizeDist._BrowseList.SelectedItems.Add(brSizeDist._BrowseList.Items(0))
            InvokeContextMenu(brSizeDist._BrowseList.ContextMenu, "_SubSnapshot")
            Dim ctrlsSizeSubSnap = DataWindowMain.GetDataSurface
            Dim bmemSizeSubSnp = CType(ctrlsSizeSubSnap.SurfaceDetails.Children(0), BrowseMem)
            bmemSizeSubSnp._TabItemDetails.Focus()
            bmemSizeSubSnp.OnTabItemDetailsGotFocus(bmemSizeSubSnp._TabItemDetails, New RoutedEventArgs)

            For Each itm In bmemSizeSubSnp._DetailBrowse._BrowseList.Items
                _VBAssert.OutputText(itm.ToString)
            Next


            Dim dupeSurface = DoTestDuplicates()
            Dim bmemDupes = CType(dupeSurface.SurfaceDetails.Children(0), BrowseMem)
            bmemDupes._TabControl.SelectedIndex = 1 ' select Details tab
            For Each itm In bmemDupes._DetailBrowse._BrowseList.Items
                _VBAssert.OutputText(itm.ToString)
            Next


            WaitForOtherThreads(_nmSecsDelayBetweenUIDisplay)
        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub

    <TestMethod()>
    Sub LifeOffMSWin()
        Try
            InitTestMethodAndSetBaseLine()
            '_VBAssert.OutputText(GetTestNameFromstack)

            LoadOffLineSnap(IO.Path.Combine(memspectInstallDir, "Snaps\csLife"), fClearFilterToo:=False)
            _VBAssert.OutputText("Got globalfilter seqnos " + _GlobalFilter._LeakMultipleRawText, cSplitChar:=CChar(vbCr))
            _GlobalFilter.ClearFilter()
            For Each blktype In [Enum].GetValues(GetType(BlockTypes))
                Dim tk = BlockTypeToTrackType(CType(blktype, BlockTypes))
                If tk <> TrkType.All Then
                    MemSpectWin._btnLastSelected = CType(tk, TrkType)
                    _VBAssert.OutputText("Doing " + tk.ToString)
                    Dim ctrls = ShowSubSnapShot(MemSpectSnap.Allocs.ToList, "SubSnapShot " + tk.ToString)
                    Dim mspectWin = CType(ctrls.SurfaceDetails.Children(0), MemSpectWin)
                    Dim bmem = mspectWin._bmem
                    bmem._TabItemDetails.Focus()
                    bmem.OnTabItemDetailsGotFocus(bmem._TabItemDetails, New RoutedEventArgs)
                    Dim nCnt = 0
                    For Each itm In bmem._DetailBrowse._BrowseList.Items
                        _VBAssert.OutputText(itm.ToString)
                        nCnt += 1
                        If nCnt = 5 Then
                            Exit For
                        End If
                    Next
                End If
            Next

        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub



    <TestMethod()>
    Sub LifeFilter()
        Try
            InitTestMethodAndSetBaseLine()
            '_VBAssert.OutputText(GetTestNameFromstack)
            MemSpectWin._btnLastSelected = TrkType.ClrObjects


            LoadOffLineSnap(IO.Path.Combine(memspectInstallDir, "Snaps\csLife"), fClearFilterToo:=True)

            For Each srchStringToUse In {"button;textbox"}
                _VBAssert.OutputText("Using srchString " + srchStringToUse)
                _GlobalFilter.SrchString = srchStringToUse
                Dim ofilt = New Filter(_GlobalFilter, Nothing)
                ofilt.btnApply_Click(Nothing, Nothing)


                _VBAssert.OutputText("Filtered Types found " + ofilt.browTypesFoundRes._BrowseList.Items.Count.ToString)

                For Each itm In ofilt.browTypesFoundRes._BrowseList.Items
                    _VBAssert.OutputText(itm.ToString)
                Next
                _VBAssert.OutputText("Filtered Types subsnap")
                Dim x = ofilt.browTypesFoundRes._BrowseList.Items(1)
                ofilt.browTypesFoundRes._BrowseList.SelectedItems.Add(x)

                InvokeContextMenu(ofilt.browTypesFoundRes._BrowseList.ContextMenu, "_SubSnapshot")
                Dim details = DataWindowMain.GetDataSurface.SurfaceDetails.Children(0)
                _VBAssert.OutputText("details " + details.GetType().Name)
                Dim mspectWin = CType(details, BrowseMem)
                Dim bmem = mspectWin
                bmem._TabControl.SelectedIndex = 1 ' select Details tab
                bmem.OnTabItemDetailsGotFocus(bmem._TabItemDetails, New Windows.RoutedEventArgs)
                For Each itm In bmem._DetailBrowse._BrowseList.Items
                    _VBAssert.OutputText(itm.ToString)
                Next



                _VBAssert.OutputText("SnapFoundStrs")
                ofilt.btnShowFoundStrsInBrowMem.RaiseEvent(New RoutedEventArgs() With {.RoutedEvent = Button.ClickEvent})
                Dim browFoundStrs = CType(DataWindowMain.GetDataSurface.SurfaceDetails.Children(0), Browse)
                For Each itm In browFoundStrs._BrowseList.Items
                    _VBAssert.OutputText(itm.ToString())
                Next

                _VBAssert.OutputText("SnapFoundStrs subsnap")
                browFoundStrs._BrowseList.SelectedItems.Add(browFoundStrs._BrowseList.Items(1))

                InvokeContextMenu(browFoundStrs._BrowseList.ContextMenu, "_SubSnapshot")
                Dim bmemFoundStrSubSnap = CType(DataWindowMain.GetDataSurface.SurfaceDetails.Children(0), BrowseMem)
                bmemFoundStrSubSnap._TabControl.SelectedIndex = 1 ' select Details tab
                bmemFoundStrSubSnap.OnTabItemDetailsGotFocus(bmemFoundStrSubSnap._TabItemDetails, New Windows.RoutedEventArgs)
                For Each itm In bmemFoundStrSubSnap._DetailBrowse._BrowseList.Items
                    _VBAssert.OutputText(itm.ToString)
                Next


                _VBAssert.OutputText("SnapAllFiltered")
                ofilt.btnSubSnapAll.RaiseEvent(New RoutedEventArgs() With {.RoutedEvent = Button.ClickEvent})
                Dim bmemAllFilteredAllocs = CType(DataWindowMain.GetDataSurface.SurfaceDetails.Children(0), BrowseMem)
                bmemAllFilteredAllocs._TabControl.SelectedIndex = 1 ' select Details tab
                bmemAllFilteredAllocs.OnTabItemDetailsGotFocus(bmemAllFilteredAllocs._TabItemDetails, New Windows.RoutedEventArgs)
                Dim nItems = bmemAllFilteredAllocs._DetailBrowse._BrowseList.Items.Count
                For Each itm In bmemAllFilteredAllocs._DetailBrowse._BrowseList.Items
                    _VBAssert.OutputText(itm.ToString, cSplitChar:=CChar(vbCr))
                Next

                _VBAssert.OutputText("Filtered Heap list cnt = " + ofilt.browFilteredHeapList._BrowseList.Items.Count.ToString)
                For Each itm In ofilt.browFilteredHeapList._BrowseList.Items
                    _VBAssert.OutputText(itm.ToString, cSplitChar:=CChar(vbCr))
                Next

            Next

        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub

    <TestMethod()>
    <Ignore>
    Sub LifeFilterCB()
        Try
            InitTestMethodAndSetBaseLine()
            '_VBAssert.OutputText(GetTestNameFromstack)

            LoadOffLineSnap(IO.Path.Combine(memspectInstallDir, "Snaps\cb"), fClearFilterToo:=True)
            _GlobalFilter.SrchString = "toolbox"
            Dim ofilt = New Filter(_GlobalFilter, Nothing)
            ofilt.btnApply_Click(Nothing, Nothing)


            _VBAssert.OutputText("Filtered Types found")
            For Each itm In ofilt.browTypesFoundRes._BrowseList.Items
                _VBAssert.OutputText(itm.ToString)
            Next

        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub

    <TestMethod()>
    Sub LifeSelf()
        Try
            InitTestMethodAndSetBaseLine()
            _VBAssert.OutputText("Launching MemSpect to analyze mem use of another instance of MemSpect looking at snaps\CSLife ")
            Dim snapPath = Path.Combine(memspectInstallDir, "Snaps\csLife")
            Dim plaunch = New ProcessLauncher With
                          {
                              .TrackClrObjects = 1,
                              ._AdditionalCmdLineParams = "-o " + snapPath,
                              .StartChildProcess = 1,
                              ._nmsecsToWaitTilStart = 2000
                        }
            Dim targproc = Path.Combine(TestContext.DeploymentDirectory, "MemSpect.exe")
            If Not File.Exists(targproc) Then
                Throw New FileNotFoundException(targproc)
            End If
            Dim procMemspect = plaunch.LaunchTargProc(
                targproc,
                fWithDll:=True,
                fDoInitcomm:=False,
                wrkdir:=TestContext.DeploymentDirectory
                )
            _VBAssert.OutputText(" Starting MemSpect hasexited= " + If(procMemspect Is Nothing, "nothing", ""))
            System.Threading.Thread.Sleep(25000)
            Dim procs = Process.GetProcessesByName("memspect").OrderBy(Function(p) p.MainWindowTitle)

            For Each proc In procs.OrderBy(Function(p) p.MainWindowTitle)
                _VBAssert.OutputText("MemSpect Process found title = " + proc.MainWindowTitle, fAddToBaseline:=False)
            Next

            _VBAssert.IsTrue(procs.Count = 2, "didn't find 2 procs")

            _VBAssert.IsTrue(procs(0).MainWindowTitle.StartsWith("MemSpect csLife"))
            _VBAssert.IsTrue(procs(1).MainWindowTitle.StartsWith("MemSpect MemSpect.exe"))

            System.Threading.Thread.Sleep(3000)
            procs(0).CloseMainWindow()
            procs(1).WaitForExit(30000)
            _VBAssert.IsTrue(procs(1).HasExited, "analysis process didn't exit (reboot might help)")
            '_VBAssert.OutputText(GetTestNameFromstack)
        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub


    <TestMethod()>
    Sub LifeCmdLineSnap()
        Try
            InitTestMethodAndSetBaseLine()
            _ProcessLauncher = New ProcessLauncher() With {
                .TrackClrObjects = 1,
                ._nmsecsToWaitTilStart = 3000
            }
            StartLife()

            Dim hprocLife = _hProcLife

            CloseCommunications()

            _VBAssert.OutputText("Life started PID = " + hprocLife.Id.ToString, fAddToBaseline:=False)
            System.Threading.Thread.Sleep(4000)


            Dim wrkdir = TestContext.DeploymentDirectory ' D:\Memspect\VSAssertSrv\TestResults\calvinh_CALVINH9 2010-08-20 10_10_45\Out

            Dim snapFolderName = IO.Path.Combine(wrkdir, "LifeCmdLineSnap")
            _VBAssert.OutputText("Temp folder for snap = " + snapFolderName, fAddToBaseline:=False)
            If Directory.Exists(snapFolderName) Then
                Directory.Delete(snapFolderName, recursive:=True)
            End If
            Dim snapDir = Directory.CreateDirectory(snapFolderName)

            'now start an instance of MemSpect to take a command line snapshot
            Dim MemSpectExe = IO.Path.Combine(wrkdir, "MemSpect.exe")
            Dim args = String.Format("/c {0} ""{1}""", hprocLife.Id, snapFolderName)
            Dim hProcMemSpectExe = Process.Start(MemSpectExe, args)

            _VBAssert.OutputText("Snapshot created ")
            hProcMemSpectExe.WaitForExit(1000 * 60 * 5) '  5 min
            _VBAssert.IsTrue(File.Exists(Path.Combine(snapFolderName, "notes.txt")))
            _VBAssert.IsTrue(File.Exists(Path.Combine(snapFolderName, "minidump.dmp")))
            _VBAssert.IsTrue(File.Exists(Path.Combine(snapFolderName, DumpedFilesMHDFileName)))

            For Each snapFile In snapDir.GetFiles()
                _VBAssert.OutputText(snapFile.Name, fAddToBaseline:=False)
            Next

            hprocLife.CloseMainWindow()


        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub


    <TestMethod()>
    <Ignore()>
    Sub LifeInteract()
        Try
            InitTestMethodAndSetBaseLine()
            LoadOffLineSnap(IO.Path.Combine(memspectInstallDir, "Snaps\csLife"))
            _ShowUI = True
#If DEBUG Then
            _nmSecsDelayBetweenUIDisplay = 500
#Else
            _nmSecsDelayBetweenUIDisplay = 100
#End If
            DoSpotCheckHeaps(Sub(snap As MemSnapshot)

                             End Sub, fDoAllocs:=True, fDumpOut:=True, fDoCtxMenuItems:=True)


        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub


End Class
' 
' 

<TestClass(),
CLSCompliant(False)>
Public Class MiniDump
    Inherits TestBase
    <ClassInitialize()>
    Public Shared Sub ClassInit(ByVal ctx As TestContext)
        BaseClassInitialize(ctx)
    End Sub

    <TestInitialize()>
    Public Sub TestInit()
        BaseTestInit()
        _nmSecsDelayBetweenUIDisplay = 0
    End Sub
    <TestCleanup()>
    Sub TestCleanup()
        BaseTestCleanup()
    End Sub

    <TestMethod()>
    Sub MiniDumpMemDict()
        Try
            InitTestMethodAndSetBaseLine()
            _ConnectionMode = MemSpectMode.Offline
            Dim OfflineDir = IO.Path.Combine(memspectInstallDir, "Snaps\NPad")
            'Dim OfflineDir = IO.Path.Combine(memspectInstallDir, "Snaps\VSStart")
            Dim miniDumpfile = IO.Path.Combine(OfflineDir, "minidump.dmp")
            _VBAssert.OutputText("Minidump read " + miniDumpfile.ToLower, fAddToBaseline:=False)
            Using rdr = New MiniDumpReader(miniDumpfile)
                Dim newentry1 = New MiniDumpReader.MemDictEntry With {
                    .StartAddr = CType(100, IntPtr),
                    .nSize = 10
                }
                rdr._MemoryDictionary.Add(newentry1, &H1000)
                Dim newentry2 = New MiniDumpReader.MemDictEntry With {
                    .StartAddr = CType(1000, IntPtr),
                    .nSize = 20
                }
                rdr._MemoryDictionary.Add(newentry2, &H2000)

                _VBAssert.OutputText(String.Format("Init value Start {0}  Size {1}", newentry1.StartAddr, newentry1.nSize))
                _VBAssert.OutputText(String.Format("Init value Start {0}  Size {1}", newentry2.StartAddr, newentry2.nSize))
                For i = 98 To 112
                    For nlen = 9 To 12
                        Dim bytesRead = 0
                        Dim res = rdr.ReadMemoryDictionary(CType(i, IntPtr), nlen)
                        If res IsNot Nothing Then
                            bytesRead = res.Length
                        End If
                        _VBAssert.OutputText(String.Format("readmemdict {0,3} {1,2} {2,3} ", i, nlen, bytesRead))

                    Next
                Next
            End Using
        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub

    <TestMethod()>
    <System.Runtime.ExceptionServices.HandleProcessCorruptedStateExceptions()>
    <Ignore()>
    Public Sub MiniDumpVirt()
        Try
            InitTestMethodAndSetBaseLine()
            Dim OfflineDir = IO.Path.Combine(memspectInstallDir, "Snaps\VSStart")
            Dim strResult = ProcComm.InitComm({System.Environment.GetCommandLineArgs(0), "/o", OfflineDir})
            If Not String.IsNullOrEmpty(strResult) Then
                _VBAssert.OutputText("Could not init comm " + OfflineDir + " " + strResult)
            End If
            Dim miniDumpfile = IO.Path.Combine(OfflineDir, "minidump.dmp")
            _VBAssert.OutputText("Minidump read " + miniDumpfile.ToLower)
            'Dim vms = MiniDumpReader.Singleton.GetVirtualAllocs
            'For Each vm In vms

            'Next
            Using rdr = New MiniDumpReader(miniDumpfile)
                Dim locStream = rdr.ReadStreamType(MINIDUMP_STREAM_TYPE.MemoryInfoListStream)
                Dim vmstream = rdr.MapStream(locStream)
                Dim vmemlist = CType(Marshal.PtrToStructure(vmstream, GetType(MINIDUMP_MEMORY_INFO_LIST)), MINIDUMP_MEMORY_INFO_LIST)
                Dim nDescSize = CUInt(Marshal.SizeOf(GetType(MINIDUMP_MEMORY_INFO)))
                Dim locrva As New MINIDUMP_LOCATION_DESCRIPTOR With {
                    .Rva = CUInt(locStream.Rva + CInt(Marshal.SizeOf(GetType(MINIDUMP_MEMORY_INFO_LIST)))),
                    .DataSize = nDescSize}
                For i = 0 To vmemlist.NumberOfEntries - 1
                    Dim ptr = rdr.MapStream(locrva)
                    Dim vmInfo = CType(Marshal.PtrToStructure(ptr,
                                GetType(MINIDUMP_MEMORY_INFO)), MINIDUMP_MEMORY_INFO)

                    _VBAssert.OutputText(String.Format("{0,4}  {1:x8}  {2:x8} {3:x8} {4:x8} {5:x8} {6:x8}",
                                                       i,
                                                       vmInfo.AllocationBase,
                                                       vmInfo.BaseAddress,
                                                       vmInfo.RegionSize,
                                                       vmInfo.Protect,
                                                       vmInfo.State,
                                                       vmInfo.Type
                                                       )
                                                   )
                    Dim mbi As New MEMORY_BASIC_INFORMATION With {
                        .AllocationBase = CType(vmInfo.AllocationBase, IntPtr),
                        .BaseAddress = CType(vmInfo.BaseAddress, IntPtr),
                        .RegionSize = CUInt(vmInfo.RegionSize),
                        .lType = CType(vmInfo.Type, AllocationType),
                        .State = CType(vmInfo.State, AllocationState),
                        .AllocationProtect = CType(vmInfo.Protect, AllocationProtect)
                    }

                    locrva.Rva += nDescSize
                Next

            End Using
        Catch ex As Exception
            HandleTestException(ex)

        End Try
    End Sub

    <TestMethod()>
    <Description("Read Minidump file only")>
    <System.Runtime.ExceptionServices.HandleProcessCorruptedStateExceptions()>
    Public Sub MiniDumpVM()
        Try
            InitTestMethodAndSetBaseLine()
            CommonUI.InitializeCommonUI()
            Dim MinidumpFile = IO.Path.Combine(memspectInstallDir, "Snaps\csLife\Minidump.dmp")
            Dim strResult = ProcComm.InitComm({System.Environment.GetCommandLineArgs(0), "/m", MinidumpFile})
            If Not String.IsNullOrEmpty(strResult) Then
                _VBAssert.OutputText("Could not init comm Minidumpfile=" + MinidumpFile + " " + strResult)
            End If
            _VBAssert.IsTrue(_ConnectionMode = MemSpectMode.MiniDumpOnly, "connmode should be minidump. Currently =" + _ConnectionMode.ToString)
            _VBAssert.OutputText("Minidump read " + CleanLineForBaseline(MinidumpFile.ToLower))

            DoTestVMUI(fManagedToo:=True)
        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub

    <TestMethod()>
    <Description("Create a Minidump file only")>
    <Ignore>
    <System.Runtime.ExceptionServices.HandleProcessCorruptedStateExceptions()>
    Public Sub MiniDumpCreateMD()
        Try
            InitTestMethodAndSetBaseLine()
            Dim pathNotepad = Notepad.GetNotepadPath
            Dim wrkdir = TestContext.DeploymentDirectory
            Dim inifile = IO.Path.Combine(wrkdir, MemSpectIniName)
            NativeImports.WritePrivateProfileString(ProfileStringSection, "JustTheseProcesses", "foo", inifile)

            '_ProcessLauncher = New ProcessLauncher(ProcessLauncher.WriteIniFile.WriteAnything) With {
            '    ._nmsecsToWaitTilStart = 2000
            '}

            '_ProcessLauncher.LaunchTargProc(pathNotepad, fWithDll:=False, wrkdir:=TestContext.DeploymentDirectory)


            _VBAssert.OutputText("Starting Notepad")
            Dim procNP = Process.Start(pathNotepad)
            System.Threading.Thread.Sleep(3000)
            _VBAssert.OutputText("Creating minidump")
            Dim strResult = InitCommWithUI({System.Environment.GetCommandLineArgs(0), "/d", procNP.Id.ToString})
            If Not String.IsNullOrEmpty(strResult) Then
                _VBAssert.OutputText("Got failure " + strResult)
            End If
            _VBAssert.OutputText("Minidump created " + _MiniDumpFileName)
            Dim vm = GetVirtAllocs()
            _VBAssert.OutputText("Got # virtallocs = " + vm.Count.ToString())
            For Each m In vm.Values
                _VBAssert.OutputText(m.ToString())
            Next

            _VBAssert.OutputText("Closing Notepad")


            'procNP.Kill()

        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub



    <Ignore()>
    <TestMethod()>
    <System.Runtime.ExceptionServices.HandleProcessCorruptedStateExceptions()>
    Public Sub MiniDumpSolo()
        Try
            InitTestMethodAndSetBaseLine()
            Dim MinidumpFile = IO.Path.Combine(memspectInstallDir, "snaps\VsStart\minidump.dmp")
            Dim strResult = ProcComm.InitComm({System.Environment.GetCommandLineArgs(0), "/m", MinidumpFile})
            If Not String.IsNullOrEmpty(strResult) Then
                _VBAssert.OutputText("Could not init comm Minidumpfile=" + MinidumpFile + " " + strResult)
            End If

            _VBAssert.OutputText("Minidump read " + MinidumpFile.ToLower)
            Using rdr = New MiniDumpReader(MinidumpFile)
                For Each nStreamType As MINIDUMP_STREAM_TYPE In [Enum].GetValues(GetType(MINIDUMP_STREAM_TYPE))
                    Dim locStream = rdr.ReadStreamType(nStreamType)
                    If locStream.Rva <> 0 OrElse locStream.DataSize <> 0 Then
                        _VBAssert.OutputText(nStreamType.ToString + "(" + CInt(nStreamType).ToString + ") _addrFileMapping=" + rdr._mappingDataCurrent._addrFileMapping.ToInt32.ToString("x8"), fAddToBaseline:=False)
                        _VBAssert.OutputText(String.Format("    StrmPtr {0:x8}  Size {1:x8}",
                                                                   rdr._strmPtr.ToInt32,
                                                                   rdr._strmSize
                                                                   ), fAddToBaseline:=False)
                        _VBAssert.OutputText(String.Format("    DirLoc DataSize {0:x8}  RVA {1:x8} ",
                                                          locStream.DataSize,
                                                          locStream.Rva))
                    End If
                    Select Case nStreamType
                        Case MINIDUMP_STREAM_TYPE.ModuleListStream
                            Dim modStream = rdr.MapStream(locStream)
                            Dim moduleList = CType(Marshal.PtrToStructure(modStream, GetType(MINIDUMP_MODULE_LIST)), MINIDUMP_MODULE_LIST)
                            Dim nDescSize = CUInt(Marshal.SizeOf(GetType(MINIDUMP_MODULE)) - 4)
                            Dim locrva As New MINIDUMP_LOCATION_DESCRIPTOR With {
                                .Rva = CUInt(locStream.Rva + CInt(Marshal.SizeOf(GetType(MINIDUMP_MODULE_LIST)))),
                                .DataSize = CUInt(nDescSize + 4)}
                            For i = 0 To moduleList.NumberOfModules - 1
                                Dim ptr = rdr.MapStream(locrva)
                                Dim moduleInfo = CType(Marshal.PtrToStructure(ptr,
                                            GetType(MINIDUMP_MODULE)), MINIDUMP_MODULE)

                                Dim modulename = rdr.GetNameFromRva(moduleInfo.ModuleNameRva)

                                If moduleInfo.BaseOfImage > UInteger.MaxValue Then
                                    Dim r = 2 ' C:\Windows\System32\ntdll.dll on 64 bit shows with humongo base addr
                                Else
                                    Dim moduleKey = moduleInfo.BaseOfImage.LongToIntPtr


                                End If
                                locrva.Rva += nDescSize
                            Next

                        Case MINIDUMP_STREAM_TYPE.HandleDataStream
                            If locStream.Rva <> 0 OrElse locStream.DataSize <> 0 Then
                                Dim handleliststream = rdr.MapStream(locStream)
                                Dim handlelist = CType(Marshal.PtrToStructure(handleliststream,
                                                                              GetType(MINIDUMP_HANDLE_DATA_STREAM)), 
                                                                              MINIDUMP_HANDLE_DATA_STREAM)
                                Dim locrva As New MINIDUMP_LOCATION_DESCRIPTOR With {
                                    .Rva = CUInt(locStream.Rva + CInt(Marshal.SizeOf(GetType(MINIDUMP_HANDLE_DATA_STREAM)))),
                                    .DataSize = CUInt(handlelist.SizeOfDescriptor)
                                }
                                Select Case handlelist.SizeOfDescriptor
                                    Case Marshal.SizeOf(GetType(MINIDUMP_HANDLE_DESCRIPTOR))
                                        _VBAssert.OutputText("MINIDUMP_HANDLE_DESCRIPTOR # =" + handlelist.NumberOfDescriptors.ToString)

                                    Case Marshal.SizeOf(GetType(MINIDUMP_HANDLE_DESCRIPTOR_2))
                                        _VBAssert.OutputText("MINIDUMP_HANDLE_DESCRIPTOR_2 # = " + handlelist.NumberOfDescriptors.ToString)
                                        Dim hTypes As New Dictionary(Of String, Integer)
                                        For i = 0 To Math.Min(handlelist.NumberOfDescriptors - 1, 100000)
                                            Dim ptr = rdr.MapStream(locrva)
                                            Dim handledesc = CType(Marshal.PtrToStructure(ptr,
                                                        GetType(MINIDUMP_HANDLE_DESCRIPTOR_2)), MINIDUMP_HANDLE_DESCRIPTOR_2)

                                            Dim objname = rdr.GetNameFromRva(handledesc.ObjectNameRva)

                                            Dim typename = rdr.GetNameFromRva(handledesc.TypeNameRva)
                                            Dim objinfo = rdr.GetNameFromRva(handledesc.ObjectNameRva)
                                            If Not hTypes.ContainsKey(typename) Then
                                                hTypes.Add(typename, 1)
                                            Else
                                                hTypes(typename) += 1
                                            End If
                                            _VBAssert.OutputText(String.Format(
                                                                 "{0} h = {1:x8} cnt={2} Type={3} Objname={4} ObjInfo={5}",
                                                                 i,
                                                                 handledesc.Handle,
                                                                 handledesc.HandleCount,
                                                                 typename,
                                                                 objname,
                                                                 objinfo
                                                                 ))

                                            locrva.Rva = CUInt(locrva.Rva + handlelist.SizeOfDescriptor)
                                        Next
                                        For Each itm In hTypes
                                            _VBAssert.OutputText(String.Format("{0,-10} {1} ", itm.Key, itm.Value))
                                        Next
                                    Case Else
                                        _VBAssert.OutputText("handle list descriptor size unknown")
                                End Select

                            End If

                        Case MINIDUMP_STREAM_TYPE.MemoryListStream
                            If locStream.Rva <> 0 OrElse locStream.DataSize <> 0 Then
                                Dim memliststream = rdr.MapStream(locStream)
                                Dim memList = CType(Marshal.PtrToStructure(memliststream,
                                                           GetType(MINIDUMP_MEMORY_LIST)), MINIDUMP_MEMORY_LIST)

                                Dim nDescriptorSize = CUInt(Marshal.SizeOf(GetType(MINIDUMP_MEMORY_DESCRIPTOR)))

                                Dim locrva As New MINIDUMP_LOCATION_DESCRIPTOR With {
                                    .Rva = CUInt(locStream.Rva + CInt(Marshal.SizeOf(GetType(MINIDUMP_MEMORY_LIST)))),
                                    .DataSize = nDescriptorSize
                                }
                                _VBAssert.OutputText(String.Format("NumberOfMemoryRanges = {0} ",
                                                                   memList.NumberOfMemoryRanges
                                                                   ))
                                Dim _MemoryDictionary As New SortedList(Of MiniDumpReader.MemDictEntry, UInteger)
                                For i = 0 To Math.Min(memList.NumberOfMemoryRanges - 1, 50000)
                                    Dim ptr = rdr.MapStream(locrva)
                                    Dim memrange = CType(Marshal.PtrToStructure(ptr,
                                                GetType(MINIDUMP_MEMORY_DESCRIPTOR)), MINIDUMP_MEMORY_DESCRIPTOR)

                                    Dim iptr = New IntPtr(CInt(memrange.StartOfMemoryRange))
                                    Dim dentry As New MiniDumpReader.MemDictEntry With {
                                        .nSize = CUInt(memrange.MemoryLocDesc.DataSize),
                                        .StartAddr = iptr
                                    }
                                    _VBAssert.OutputText(String.Format("{0} Size = {1:x8}  Start = {2:x16} Rva {3:x8}",
                                                                       i,
                                                                       memrange.MemoryLocDesc.DataSize,
                                                                       memrange.StartOfMemoryRange,
                                                                       memrange.MemoryLocDesc.Rva
                                                                       ))


                                    _MemoryDictionary.Add(dentry, CUInt(memrange.MemoryLocDesc.Rva))
                                    locrva.Rva += nDescriptorSize
                                Next
                                For Each item In _MemoryDictionary
                                    _VBAssert.OutputText(String.Format("{0:x8} {1:x8} {2:x8}",
                                                                        item.Key.StartAddr.ToInt32,
                                                                        item.Key.nSize,
                                                                        item.Value
                                                                        ))

                                    If item.Key.StartAddr.ToInt32 < &H110000 Then
                                        Dim ptrRawMem = rdr.MapStream(
                                            New MINIDUMP_LOCATION_DESCRIPTOR With {
                                                .Rva = item.Value,
                                                .DataSize = item.Key.nSize
                                            }
                                        )
                                        GetMemBytes(ptrRawMem, 10)

                                    End If
                                Next

                            End If

                    End Select
                Next

            End Using
        Catch ex As Exception
            HandleTestException(ex)

        End Try
    End Sub
    <TestMethod>
    Public Sub MinidumpSystemInfo()
        Try
            InitTestMethodAndSetBaseLine()
            Dim OfflineDir = IO.Path.Combine(memspectInstallDir, "Snaps\VSStart")
            Dim miniDumpfile = IO.Path.Combine(OfflineDir, "minidump.dmp")
            _ConnectionMode = MemSpectMode.MiniDumpOnly
            _MiniDumpFileName = miniDumpfile
            Dim systeminfo = MiniDumpReader.Singleton.GetMinidumpSystemInfo()
            _VBAssert.OutputText("System Info")
            _VBAssert.OutputText(systeminfo.ToString)
        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub

    <TestMethod()>
    <System.Runtime.ExceptionServices.HandleProcessCorruptedStateExceptions()>
    Public Sub MiniDumpStreams()
        Try
            InitTestMethodAndSetBaseLine()
            'Dim OfflineDir = IO.Path.Combine(memspectInstallDir, "Snaps\NPad")
            Dim OfflineDir = IO.Path.Combine(memspectInstallDir, "Snaps\VSStart")
            Dim strResult = ProcComm.InitComm({System.Environment.GetCommandLineArgs(0), "/o", OfflineDir})
            If Not String.IsNullOrEmpty(strResult) Then
                _VBAssert.OutputText("Could not load offline" + OfflineDir + " " + strResult)
            End If

            Dim miniDumpfile = IO.Path.Combine(OfflineDir, "minidump.dmp")
            _VBAssert.OutputText("Minidump read " + miniDumpfile.ToLower, fAddToBaseline:=False)
            Using rdr = New MiniDumpReader(miniDumpfile)
                If rdr._mappingDataCurrent._hFileMapping = IntPtr.Zero Then
                    _VBAssert.OutputText("file mapping failed")
                Else
                    _VBAssert.OutputText("file mapping succeeded")
                End If
                If Not rdr.IsValid Then
                    _VBAssert.OutputText("couldn't open minidumpfile " + miniDumpfile)
                Else
                    _VBAssert.OutputText("Opened minidump _hFileMapping=" + rdr._mappingDataCurrent._hFileMapping.ToInt32.ToString("x8"), fAddToBaseline:=False)
                End If

                For Each nStreamType As MINIDUMP_STREAM_TYPE In [Enum].GetValues(GetType(MINIDUMP_STREAM_TYPE))
                    Dim locStream = rdr.ReadStreamType(nStreamType)
                    If locStream.Rva <> 0 OrElse locStream.DataSize <> 0 Then
#If 1 Then
                        _VBAssert.OutputText(nStreamType.ToString + "(" + CInt(nStreamType).ToString + ") _addrFileMapping=" + rdr._mappingDataCurrent._addrFileMapping.ToInt32.ToString("x8"), fAddToBaseline:=False)
                        _VBAssert.OutputText(String.Format("    StrmPtr {0:x8}  Size {1:x8}",
                                                                   rdr._strmPtr.ToInt32,
                                                                   rdr._strmSize
                                                                   ), fAddToBaseline:=False)
                        _VBAssert.OutputText(String.Format("    DirLoc DataSize {0:x8}  RVA {1:x8} ",
                                                          locStream.DataSize,
                                                          locStream.Rva))
                        Select Case nStreamType
                            Case MINIDUMP_STREAM_TYPE.SystemInfoStream
                                Dim sysInfoStrm = rdr.MapStream(locStream)
                                Dim sysinfo = CType(Marshal.PtrToStructure(sysInfoStrm, GetType(MINIDUMP_SYSTEM_INFO)), MINIDUMP_SYSTEM_INFO)
                                _VBAssert.OutputText(sysinfo.ToString)
                            Case MINIDUMP_STREAM_TYPE.ThreadListStream
                                Dim thrdstrm = rdr.MapStream(locStream)
                                Dim thrdList = CType(Marshal.PtrToStructure(thrdstrm, GetType(MINIDUMP_THREAD_LIST)), MINIDUMP_THREAD_LIST)
                                Dim nDescSize = CUInt(Marshal.SizeOf(GetType(MINIDUMP_THREAD)))
                                _VBAssert.OutputText("# of threads = " + thrdList.NumberOfThreads.ToString + "  descsize =" + nDescSize.ToString)

                                Dim locrva As New MINIDUMP_LOCATION_DESCRIPTOR With {
                                    .Rva = CUInt(locStream.Rva + CInt(Marshal.SizeOf(GetType(MINIDUMP_THREAD_LIST)))),
                                    .DataSize = CUInt(nDescSize)}
                                For i = 0 To thrdList.NumberOfThreads - 1
                                    Dim ptr = rdr.MapStream(locrva)
                                    Dim thrdInfo = CType(Marshal.PtrToStructure(ptr,
                                                GetType(MINIDUMP_THREAD)), MINIDUMP_THREAD)

                                    _VBAssert.OutputText(String.Format("{0} {1} {2} {3:x8}",
                                                                       i,
                                                                       thrdInfo.ThreadId,
                                                                       thrdInfo.Priority,
                                                                       locrva.Rva
                                                                       ))

                                    locrva.Rva += nDescSize
                                Next

                            Case MINIDUMP_STREAM_TYPE.ModuleListStream
                                Dim modulestrm = rdr.MapStream(locStream)
                                Dim ModuleList = CType(Marshal.PtrToStructure(modulestrm, GetType(MINIDUMP_MODULE_LIST)), MINIDUMP_MODULE_LIST)
                                Dim nDescSize = CUInt(Marshal.SizeOf(GetType(MINIDUMP_MODULE)) - 4) ' -4 because of MINIDUMP_MODULE Modules[]
                                _VBAssert.OutputText("# of modules = " + ModuleList.NumberOfModules.ToString + "  descsize =" + nDescSize.ToString)

                                Dim locrva As New MINIDUMP_LOCATION_DESCRIPTOR With {
                                    .Rva = CUInt(locStream.Rva + CInt(Marshal.SizeOf(GetType(MINIDUMP_MODULE_LIST)))),
                                    .DataSize = CUInt(nDescSize + 4)}
                                For i = 0 To ModuleList.NumberOfModules - 1
                                    Dim ptr = rdr.MapStream(locrva)
                                    Dim modInfo = CType(Marshal.PtrToStructure(ptr,
                                                GetType(MINIDUMP_MODULE)), MINIDUMP_MODULE)

                                    Dim locname = rdr.MapStream(New MINIDUMP_LOCATION_DESCRIPTOR With {
                                                               .Rva = modInfo.ModuleNameRva,
                                                               .DataSize = 600
                                                           }
                                                       )
                                    Dim name = Marshal.PtrToStringBSTR(locname.MyAdd(4)) ' skip len

                                    _VBAssert.OutputText(String.Format("{0} {1:x8} Size = {2:x8} TStamp={3:x8} Rva = {4:x16} {5}",
                                                                       i,
                                                                       modInfo.BaseOfImage,
                                                                       modInfo.SizeOfImage,
                                                                       modInfo.TimeDateStamp,
                                                                       locrva.Rva,
                                                                       name
                                                                       ))


                                    locrva.Rva += nDescSize
                                Next
                            Case MINIDUMP_STREAM_TYPE.Memory64ListStream
                                Dim memliststream = rdr.MapStream(locStream)
                                If memliststream = IntPtr.Zero Then
                                    _VBAssert.OutputText("error mapping stream")
                                    Return
                                End If
                                Dim MemList = CType(Marshal.PtrToStructure(memliststream,
                                                                           GetType(MINIDUMP_MEMORY64_LIST)), MINIDUMP_MEMORY64_LIST)
                                _VBAssert.OutputText(String.Format("NumberOfMemoryRanges = {0} BaseRVA = {1:x8}",
                                                                   MemList.NumberOfMemoryRanges,
                                                                   MemList.BaseRva
                                                                   ))

                                Dim nTotalMemSize As ULong
                                Dim posSoFar As ULong = 0
                                Dim nDescSize = CUInt(Marshal.SizeOf(GetType(MINIDUMP_MEMORY_DESCRIPTOR64))) '16
                                Dim locrva As New MINIDUMP_LOCATION_DESCRIPTOR With {
                                    .Rva = CUInt(locStream.Rva + CInt(Marshal.SizeOf(GetType(MINIDUMP_MEMORY64_LIST)))),
                                    .DataSize = nDescSize}
                                For i = 0 To MemList.NumberOfMemoryRanges - 1
                                    Dim ptr = rdr.MapStream(locrva)
                                    Dim memrange = CType(Marshal.PtrToStructure(ptr,
                                                GetType(MINIDUMP_MEMORY_DESCRIPTOR64)), MINIDUMP_MEMORY_DESCRIPTOR64)
                                    _VBAssert.OutputText(String.Format("{0} Size = {1:x8}  Start = {2:x16} {3:x8}",
                                                                       i,
                                                                       memrange.DataSize,
                                                                       memrange.StartOfMemoryRange,
                                                                       locrva.Rva
                                                                       ))

                                    _VBAssert.OutputText("posSoFar = " + posSoFar.ToString("x8"))
                                    'If i > 2133 Then
                                    '    Dim ptrRawMem = rdr.MapStream(
                                    '        New MINIDUMP_LOCATION_DESCRIPTOR With {
                                    '            .Rva = CUInt(posSoFar + MemList.BaseRva),
                                    '            .DataSize = CUInt(memrange.DataSize)
                                    '        }
                                    '    )
                                    '    GetMemBytes(ptrRawMem, 10)
                                    'End If
                                    posSoFar += memrange.DataSize
                                    locrva.Rva += nDescSize
                                    nTotalMemSize += memrange.DataSize
                                Next
                                _VBAssert.OutputText("Total mem size = " + nTotalMemSize.ToString("n0"))
                                rdr.MakeMemoryDictionary()
                                _VBAssert.OutputText("Dictionary # entries=" + rdr._MemoryDictionary.Count.ToString)
                                For i = 0 To rdr._MemoryDictionary.Count - 1
                                    _VBAssert.OutputText(String.Format("{0} {1} Rva={2:x8}",
                                                                       i,
                                                                       rdr._MemoryDictionary.Keys(i).ToString,
                                                                       rdr._MemoryDictionary.Values(i)))
                                Next
                                For Each raddr In {&H20000, &H20000 + 8, &HFFFDF000}
                                    _VBAssert.OutputText("Readmemdict " + raddr.ToString("x8"))
                                    Dim rr = rdr.ReadMemoryDictionary(New IntPtr(raddr), 200)
                                    '                                Dim rr = rdr.ReadMemoryDictionary(New IntPtr(&HFFDF0000), 200)
                                    _VBAssert.OutputText("Mem lookup len = " + rr.Length.ToString)
                                    If rr.Length > 0 Then
                                        ShowBytes(rr, 5)
                                    End If

                                Next
                            Case MINIDUMP_STREAM_TYPE.MemoryInfoListStream
                                Dim minfostrm = rdr.MapStream(locStream)
                                Dim MemInfoList = CType(Marshal.PtrToStructure(minfostrm, GetType(MINIDUMP_MEMORY_INFO_LIST)), MINIDUMP_MEMORY_INFO_LIST)
                                _VBAssert.OutputText(String.Format("NumberOfEntries {0} HeaderSize {1}   Entry Size {2}",
                                                                   MemInfoList.NumberOfEntries,
                                                                   MemInfoList.SizeOfHeader,
                                                                   MemInfoList.SizeOfEntry
                                                                   ))
                        End Select

#End If
                    End If

                Next
                _VBAssert.OutputText("Before close, reader should be valid. Actual = " + rdr.IsValid.ToString)
            End Using

            '            _VBAssert.OutputText("After close reader valid should be false. Actual = " + rdr.IsValid.ToString)


        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub


    Private Sub GetMemBytes(ByVal ptrRawMem As IntPtr, ByVal nLines As Integer)
        Dim blk As New ProcMemBlockByte
        Dim dwBytesRead = 0
        '                                        _VBAssert.OutputText("ptrRawMem - " + ptrRawMem.ToInt32.ToString("x8") + " SoFar=" + posSoFar.ToString("x8"))
        If ReadProcessMemoryByte(Process.GetCurrentProcess.Handle,
                                 ptrRawMem,
                                 blk,
                                 Marshal.SizeOf(blk), dwBytesRead) <> 0 Then
            _VBAssert.OutputText("# bytes read=" + dwBytesRead.ToString)
            ShowBytes(blk.data, nLines)
        End If
    End Sub

    Private Sub ShowBytes(ByVal data() As Byte, ByVal nLines As Integer)
        For ii = 0 To nLines - 1
            Dim str As New Text.StringBuilder
            For jj = 0 To 16 - 1
                If ii * 16 + jj >= data.Length Then
                    Exit For
                End If
                Dim b = data(ii * 16 + jj)
                str.Append(" " + b.ToString("x2"))
            Next
            _VBAssert.OutputText(String.Format("{0,3}  {1}", ii, str.ToString))
        Next

    End Sub
End Class

<TestClass()>
<CLSCompliant(False)>
Public Class TestMisc
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

    <TestMethod()>
    Public Sub TestMemLayout()
        InitTestMethodAndSetBaseLine()
        Try

            _ShowUI = True
            LoadOffLineSnap(IO.Path.Combine(memspectInstallDir, "Snaps\csLife"))
            ReadHeaps()
            Dim hh = _HeapList.Where(Function(h) h.GetHeapName = MemSpectHeapName).Single

            Dim snapAllocs = hh.TakeMemSnapshot(fEnableFilter:=False).Allocs.
                Where(Function(h) h.TBlkBlockType = BlockTypes.ClrObject)

            MemSpectWin._btnLastSelected = TrkType.ClrObjects
            Dim ctrls = ShowSubSnapShot(snapAllocs.ToList, "SubSnapShot ClrObjs")
            DataWindowMain._DataWindowMain.Width = 1000
            DataWindowMain._DataWindowMain.Height = 800
            Dim mspectWin = CType(ctrls.SurfaceDetails.Children(0), MemSpectWin)
            Dim bmem = mspectWin._bmem
            Dim tbMemLayout = CType((
                    From titem In bmem._TabControl.Items
                    Where CType(titem, TabItem).Tag.ToString = "MemoryLayout"
                    ).Single, 
                TabItem)
            'instantiate tab
            tbMemLayout.RaiseEvent(New Windows.RoutedEventArgs With {.RoutedEvent = TabItem.GotFocusEvent})

            'create a drawingcontext which outputs the drawing output to test log
            Dim testDrawingContext = New MyDrawingContextTest() With {
                ._vbassert = _VBAssert
            }

            WaitForOtherThreads(1000)
            If Not tbMemLayout.Focus() Then ' force to focus, so will render
                _VBAssert.OutputText("Couldn't focus mem layout tab")
            End If


            Dim ctrMemRegionGraphContainer = CType(tbMemLayout.Content, MemoryRegionGraphContainer)

            ctrMemRegionGraphContainer._RegionGraph._MyDrawingContext = testDrawingContext
            _VBAssert.OutputText("Got mem layout " + ctrMemRegionGraphContainer.ToString)
            _VBAssert.OutputText(String.Format("{0} {1}", DataWindowMain._DataWindowMain.Width, DataWindowMain._DataWindowMain.Height))
            _VBAssert.OutputText(String.Format("{0} {1}", tbMemLayout.Width, tbMemLayout.Height))
            'ctrMemLayout.Width = 800
            'ctrMemLayout.Height = 600
            _VBAssert.OutputText(String.Format("memlayout {0} {1}", ctrMemRegionGraphContainer.Width, ctrMemRegionGraphContainer.Height))
            _VBAssert.OutputText(String.Format("memlayoutc {0} {1}", ctrMemRegionGraphContainer._RegionGraph.Width, ctrMemRegionGraphContainer._RegionGraph.Height))

            'DataWindowMain._DataWindowMain.Topmost = True ' this makes it hard to debug without dual mon
            'Window.GetWindow(tbMemLayout).Topmost = True

            WaitForOtherThreads(3000) '  allow rendering

            _VBAssert.OutputText(String.Format("memlayout {0} {1}", ctrMemRegionGraphContainer.Width, ctrMemRegionGraphContainer.Height))
            _VBAssert.OutputText(String.Format("memlayoutc {0} {1}", ctrMemRegionGraphContainer._RegionGraph.Width, ctrMemRegionGraphContainer._RegionGraph.Height))
            'For Each itm In testDrawingContext.lst
            '    _VBAssert.OutputText(String.Format("got Rect {0} {1} {2}", itm.rect.ToString(), itm.brush.ToString, itm.pen.ToString))
            'Next

            _VBAssert.OutputText("Sending LeftMousewheel")
            Dim ev = New MouseWheelEventArgs(Mouse.PrimaryDevice, timestamp:=0, delta:=1) With
                                {
                                    .RoutedEvent = UIElement.PreviewMouseWheelEvent,
                                    .Source = ctrMemRegionGraphContainer
                                }

            ctrMemRegionGraphContainer.RaiseEvent(ev)
            _VBAssert.IsTrue(ev.Handled, "mouse wheel event wasn't handled")
            'Dim ev = New MouseButtonEventArgs(Mouse.PrimaryDevice, 0, MouseButton.Left) With
            '                    {
            '                        .RoutedEvent = UIElement.MouseLeftButtonUpEvent,
            '                        .Source = ctrMemRegionGraphContainer
            '                    }

            'ctrMemRegionGraphContainer.RaiseEvent(ev)




            WaitForOtherThreads(3000) '  allow rendering

            _VBAssert.OutputText("VirtualMemory graph")
            Dim vm = New VirtualMem
            vm.ShowVirtualAllocs()

            Dim memGraphContainer = vm.ShowVMGraphLayout()
            WaitForOtherThreads(3000) '  allow rendering

            _VBAssert.OutputText("# of regions = " + memGraphContainer._RegionGraph._regions.Count.ToString)
            For Each reg In memGraphContainer._RegionGraph._regions
                _VBAssert.OutputText(reg.ToString)
            Next


        Catch ex As Exception
            HandleTestException(ex)
        End Try


    End Sub

    Public Class MyDrawingContextTest
        Implements IDrawRectangle

        Friend _vbassert As VBAssert
        Friend _drawingContext As Media.DrawingContext

        Public Sub DrawRectangle(ByVal brush As System.Windows.Media.Brush, ByVal pen As System.Windows.Media.Pen, ByVal rectangle As System.Windows.Rect) Implements MemSpect.IDrawRectangle.DrawRectangle
            If _drawingContext IsNot Nothing Then
                _drawingContext.DrawRectangle(brush, pen, rectangle)
            End If
            If _vbassert IsNot Nothing Then
                _vbassert.OutputText(String.Format("Rect {0} {1} {2}", rectangle.ToString(), brush.ToString, If(pen IsNot Nothing, pen.ToString, String.Empty)))
            End If
        End Sub

        Public Sub SetDrawingContext(ByVal context As System.Windows.Media.DrawingContext) Implements MemSpect.IDrawRectangle.SetDrawingContext
            _drawingContext = context
        End Sub
    End Class
    <TestMethod()>
    <Ignore()>
    Sub TestIntPtr()

        ' these values throw on dev11, but not dev10
        'Dim myaddarg1 = &H7FFF0000
        'Dim myaddArg2 = &H7FFBD000

        Try
            Dim start = &H7FFFFFF0
            Dim startIPtr = New IntPtr(start)
            For i = 1 To 100
                Dim res = startIPtr.MyAdd(i)


            Next

        Catch ex As Exception
            Debug.Assert(False)
            HandleTestException(ex)
        End Try
    End Sub

    <TestMethod()>
    <Ignore()>
    Public Sub TestRegEx()
        InitTestMethodAndSetBaseLine()
        _VBAssert.OutputText("regex")
        Dim frame = "c:\enlist\win8\inetcore\jscript\lib\common\datastructures\immutablelist.h(261) : jscript9.dll!regex::ImmutableList<Js::BufferBuilder *>::Reverse + 28 bytes"

        _VBAssert.OutputText("regex Init val = " + frame)
        Dim frameName = SymbolStripFileName(frame, fStripBytesToo:=True)

        _VBAssert.OutputText("regex " + frameName)
        Dim m = Regex.Match(frameName, "(\S*![^+]*[^ +])")
        If m.Success Then
            _VBAssert.OutputText("got match " + m.Groups(1).Value)


        End If
        _VBAssert.OutputText("done")

    End Sub

    <TestMethod()>
    <Ignore()>
    Sub TestCrc()
        Dim testbuflen = 20000
        Dim buf(testbuflen - 1) As Byte

        Dim ptr = Marshal.AllocCoTaskMem(testbuflen)
        Marshal.Copy(buf, 0, ptr, testbuflen)
        Dim crc = CRC32(0, ptr, 0, testbuflen)


        For i = 0 To testbuflen - 1
            buf(i) = CByte(i Mod 255)
        Next
        Dim crc2 = CRC32(0, ptr, 0, testbuflen)
        Marshal.FreeCoTaskMem(ptr)

        _VBAssert.IsTrue(crc <> crc2, "CRC of 0 is same as non-zero?")

    End Sub

End Class

<TestClass()>
<CLSCompliant(False)>
Public Class Resources
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

    <TestMethod()>
    <Ignore()>
    Sub TestAppDomain()

        Try

            For Each strSym In {
            "47   1B          I_RpcBindingInqDynamicEndpoint (forwarded to RPCRT4.I_RpcBindingInqDynamicEndpointW)",
            "1309  51C 00041210 _wtof = _wtof",
            "131   82 0000304C ?Detach@CurrentScheduler@Concurrency@@SAXXZ = ?Detach@CurrentScheduler@Concurrency@@SAXXZ (public: static void __cdecl Concurrency::CurrentScheduler::Detach(void))",
            "1310  51D 00041168 _wtof_l = _wtof_l",
            "1319  526 0003BCFC _wutime64 = _wutime64",
            "132   83 00019914 ?DisableTracing@Concurrency@@YAJXZ = ?DisableTracing@Concurrency@@YAJXZ (long __cdecl Concurrency::DisableTracing(void))",
            "1311  51E 00023404 _wtoi = _wtoi"
            }
                Dim match = RegularExpressions.Regex.Match(strSym, "[0-9]+")
                Dim ord = strSym.Substring(0, match.Length)
                strSym = strSym.Substring(match.Length)

                match = Regex.Match(strSym, "\s+[0-9A-Fa-f]+")
                Dim hint = strSym.Substring(0, match.Length).Trim
                strSym = strSym.Substring(match.Length)

                Dim rva = "" ' may not be present
                match = Regex.Match(strSym, "\s+")
                If match.Length > 4 Then
                    ' no rva
                Else
                    match = Regex.Match(strSym, "\s+[0-9A-Fa-f]{8}")
                    rva = strSym.Substring(0, match.Length)
                    strSym = strSym.Substring(match.Length)
                End If
                Dim splitstr = strSym.Trim.Split()
                Dim undecName = splitstr(0)
                Dim symNameStrB = New StringBuilder(1000)
                Dim symbolName = UnDecorateSymbolName(splitstr(0), symNameStrB, 1000, 0)
                Debug.WriteLine(String.Format("Name = {0} Unde = {1}", symNameStrB, undecName))




            Next











            Dim sb As New StringBuilder(290)
            ' we will always get line no info. so we can go to source code
            '            Dim initdbghelp = VsResolveSymbolEx(GetCurrentProcess, IntPtr.Zero, sb, sb.Capacity, fNoFileLineInfo:=False)

            Dim sym = "?I_ScGetCurrentGroupStateW@@YGKPAUSC_HANDLE__@@PAGPAK@Z (unsigned long __stdcall I_ScGetCurrentGroupStateW(struct SC_HANDLE__ *,unsigned short *,unsigned long *))"
            sym = "_AddAccessAllowedObjectAceStub@28"
            sym = "?is_signed@?$numeric_limits@D@std@@2_NB = ?is_signed@?$numeric_limits@C@std@@2_NB (public: static bool const std::numeric_limits<signed char>::is_signed)"
            sym = "?is_signed@?$numeric_limits@D@std@@2_NB"
            sym = "?I_ScGetCurrentGroupStateW@@YGKPAUSC_HANDLE__@@PAGPAK@Z"
            sym = "?MyFunc@@YAHD@Z"
            Dim symName As New StringBuilder(1000)
            Dim reslt = UnDecorateSymbolName(sym, symName, 1000, 0)
            Debug.Assert(symName.ToString = "int __cdecl MyFunc(char)", "undecorate name failed")
            sym = "?I_ScGetCurrentGroupStateW@@YGKPAUSC_HANDLE__@@PAGPAK@Z"
            symName = New StringBuilder(1000)
            reslt = UnDecorateSymbolName(sym, symName, 1000, 0)
            Debug.Assert(symName.ToString = "unsigned long __stdcall I_ScGetCurrentGroupStateW(struct SC_HANDLE__ *,unsigned short *,unsigned long *)", "undecorate name failed")


            sym = "??0?$basic_iostream@DU?$char_traits@D@std@@@std@@QEAA@$$QEAV01@@Z"
            symName = New StringBuilder(1000)
            reslt = UnDecorateSymbolName(sym, symName, 1000, 0)



            Dim x = New CompareIntPtr
            Dim ip1 = New IntPtr(1)
            Dim ip2 = New IntPtr(2)
            Dim res = x.Compare(ip1, ip2)
            Dim lst = New SortedList(Of IntPtr, Integer)(x)
            lst.Add(New IntPtr(-3), lst.Count)
            lst.Add(New IntPtr(-1), lst.Count)
            lst.Add(New IntPtr(0), lst.Count)
            lst.Add(New IntPtr(1), lst.Count)
            For Each itm In lst
                Debug.WriteLine(itm.Value)
            Next



            Dim appdomainSetup = New AppDomainSetup With {
                .ApplicationBase = IO.Path.GetDirectoryName(Assembly.GetExecutingAssembly.Location)
            }
            Dim testDomain = AppDomain.CreateDomain("testdomain", Nothing, appdomainSetup)

            Dim thisasm = "MemSpect" 'not MemSpect.Test ! Assembly.GetExecutingAssembly.FullName
            Dim appdLoader = CType(testDomain.CreateInstanceAndUnwrap(
                thisasm,
                "MemSpect.AssemblyLoadInTempAppDomain"
                ), AssemblyLoadInTempAppDomain)

            Dim asmToUse = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly.Location), "MemSpect.exe")
            Dim bytesAsm() = File.ReadAllBytes(asmToUse)

            Dim loadres = appdLoader.LoadAssembly(bytesAsm, asmToUse, Images.UseSeparateAppDomain)
            If Not String.IsNullOrEmpty(loadres) Then
                _VBAssert.OutputText("failed to load asm " + loadres)
            Else
                Dim nms = appdLoader.GetResourceNames()
                _VBAssert.IsTrue(nms.Count > 1, "got no resources " + nms.Count.ToString)
                _VBAssert.OutputText(String.Join(vbCrLf, nms))
            End If
            AppDomain.Unload(testDomain)
            _VBAssert.OutputText("Done " + _TestContext.TestName)
        Catch ex As Exception
            _VBAssert.IsTrue(False, ex.Message)
        End Try

    End Sub

End Class
