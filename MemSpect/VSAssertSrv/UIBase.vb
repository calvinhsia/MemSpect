Imports System.Windows.Threading
Imports System.Windows
Imports System.Windows.Controls
Imports System.Windows.Input
Imports MemSpect
Imports System.Windows.Media
Imports System.IO
Imports System.Linq
Imports System.Runtime.InteropServices
Imports System.ComponentModel

Namespace MemSpect
    Public MustInherit Class VBDiagMarginBase
        Inherits DockPanel
        Friend Shared _windOwner As Window ' for IsRemote, has window, else nothing
        'see http://blogs.msdn.com/calvin_hsia/archive/2009/09/28/9900562.aspx
        Friend Shared _logFile As String = ""    ' Excel can read/create graph easily

        Friend Const MaxWidthMargin As Integer = 400
        Friend Shared WithEvents _timerAutoGc As DispatcherTimer
        Friend Shared _nAutoGCSecs As Integer
        Friend Shared _nGCLoopCount As Integer = 1
        Friend Shared _nCleanUpRCWs As Integer = 1
        Friend Shared _AvailVMMemRed As Integer = 0
        Friend Shared _LargestFreeVMSizeRed As Integer = 0
        Public Shared _FontSize As Integer = 8
        Public Shared _FontName As String = "Arial"
        Public Shared _Foreground As String = "Blue"
        Public Shared _Background As String = "White"

        Friend Shared _OutputToDebugWindow As Integer = 0

        Friend Shared _PriorMaxSeqNo As UInteger = 0
        Friend Shared _PriorAllocCount As Integer = 0 ' # items allocated


        Friend WithEvents _btnGC As Button
        Friend WithEvents _btnMenu As Button
        Friend WithEvents _btnNrlsMem As Button
        Friend WithEvents _btnStrPool As Button
        Friend WithEvents _txtAutoGCSecs As TextBox
        Friend WithEvents _btnVirtMem As Button
        Public WithEvents _chkClrObjects As CheckBox
        Public WithEvents _chkFreeze As CheckBox

        Friend _txtmemStat As TextBlock
        '        Friend _txtNrlsMem As TextBlock
        Friend _txtGCStat As TextBlock


        Friend _ContentType As String = ""

        Friend _txtVirtmemStat As TextBlock
        Friend _FilterUI As FilterUI
        Friend _txtMaxSeqNo As TextBlock
        Friend _spDebugHeaps As DockPanel

        Friend _isDisposed As Boolean = False
        Friend _oFilterTab As TabItem

        Protected Sub New()
            MakeCtxMenuMain()

        End Sub

        Friend Sub MakeCtxMenuMain()
            Me.ContextMenu = New ContextMenu

            Me.ContextMenu.AddMnuItem("About MemSpect", "About MemSpect", AddressOf on_CtxMenuMain)

            Dim itm = Me.ContextMenu.AddMnuItem("_Always On Top",
                                      "Keep main window Always On Top",
                                      AddressOf on_CtxMenuMain)

            ' not minidump or existing modes
            Dim fIsOnOrOffline = _ConnectionMode = MemSpectMode.OnLine OrElse _ConnectionMode = MemSpectMode.Offline
            If fIsOnOrOffline Then
                Me.ContextMenu.AddMnuItem("_HeapDensityReport",
                                          "Detailed report of heap density: heapallocs vs VM",
                                          AddressOf on_CtxMenuMain)
                Me.ContextMenu.AddMnuItem("_Filter...",
                                          "Filter by SeqNo or Thread",
                                          AddressOf on_CtxMenuMain)
                Me.ContextMenu.AddMnuItem("_Clear Filter",
                                          "Reset filter",
                                          AddressOf on_CtxMenuMain)
                Me.ContextMenu.AddMnuItem("_ObjectDump",
                                          "Get the entire CLR Object graph and GCRoots",
                                          AddressOf on_CtxMenuMain)

                Me.ContextMenu.AddMnuItem("Show _Threads",
                                          "Show thread info, including live and dead threads",
                                          AddressOf on_CtxMenuMain)
                Me.ContextMenu.AddMnuItem("Show Allocations with _Waste",
                                          "search for all allocations that have wasted space (consecutive 175(0xAF) or 0x00) (respects filter)",
                                          AddressOf on_CtxMenuMain)

                Me.ContextMenu.AddMnuItem("Module Fold",
                                          "get all stacksymbols, aggregate per module. Takes ~ 30 secs",
                                          AddressOf on_CtxMenuMain)

                Me.ContextMenu.AddMnuItem("Duplicates",
                                          "Show duplicates on all heaps",
                                          AddressOf on_CtxMenuMain)

                Me.ContextMenu.AddMnuItem("Export to Perf_View",
                                          "Export all stacks of all allocs to view in PerfView \\clrmain\tools\PerfView.exe. Respects Filter. Must have extension '.perfview.xml'",
                                          AddressOf on_CtxMenuMain)

                Me.ContextMenu.AddMnuItem("Show _KnownIssues",
                                          "Known Leak issues",
                                          AddressOf on_CtxMenuMain)

                Me.ContextMenu.AddMnuItem("Show _MemStats",
                                          "Show accumulated memory statistics (gathered on each specified code marker. see 'CodeMarkersToCollectStats'",
                                          AddressOf on_CtxMenuMain)

                If _ConnectionMode <> MemSpectMode.MiniDumpOnly Then
                    Me.ContextMenu.AddMnuItem("_SymbolDictionary",
                                              "Show the current StackFrameDictionary symbols. All stack frames symbols resolved so far. For offline, it's all stack frames",
                                              AddressOf on_CtxMenuMain)
                End If
                If _ConnectionMode = MemSpectMode.OnLine Then
                    Me.ContextMenu.AddMnuItem("S_ymbolFile (PDB) Load data",
                                         "Show details about symbol files (PDBs) loaded for symbol resolution. Also shows all symbols in all PDBs (Native only)",
                                         AddressOf on_CtxMenuMain)
                    Dim curmode = GetTrackingMode()
                    Dim newmode = CType(1 - curmode, TrackingModeEnum)

                    Dim mnuItmTrackingMode = Me.ContextMenu.AddMnuItem("Change Trac_king Mode to " + newmode.ToString,
                                         "Minimal means use less memory (will also free all prior callstack memory). SetLo changes Minimal->Normal.",
                                         AddressOf on_CtxMenuMain)
                    AddHandler TrackingModeChanged, Sub(fnew As TrackingModeEnum)
                                                        Select Case fnew
                                                            Case TrackingModeEnum.Minimal
                                                                mnuItmTrackingMode.Header = "Change Trac_king Mode to Normal"
                                                            Case TrackingModeEnum.Normal
                                                                mnuItmTrackingMode.Header = "Change Trac_king Mode to Minimal"
                                                        End Select
                                                    End Sub


                    Me.ContextMenu.AddMnuItem("Crash Target _Process",
                                         "Sometimes we want to intentionally AV: testing error scenarios. Will cause constant Watson bucket. MemSpect API allows random buckets",
                                         AddressOf on_CtxMenuMain)

                    Me.ContextMenu.AddMnuItem("Hang Target Process",
                                         "Sometimes we want to intentionally hang: testing error scenarios (Thread.Sleep(big))",
                                         AddressOf on_CtxMenuMain)
                    Dim fileNameCodeToExec = Path.Combine(Path.GetDirectoryName(_iniFileName),
                                                "ExecCode.cs")

                    Me.ContextMenu.AddMnuItem("E_xecute Code In Target Process",
                                        String.Format("Compile and Execute C# code in '{0}' on a new thread. Allows you to add references, run managed code in target. If frozen, can deadlock!",
                                                       fileNameCodeToExec),
                                         AddressOf on_CtxMenuMain)

                Else
                    Me.ContextMenu.AddMnuItem("Show offline snapshot info",
                                         "At the time of snapshot, various info about the machine is captured",
                                         AddressOf on_CtxMenuMain)
                End If
                If _IsCalvinHBuild Then
                    Dim mitemDiff = Me.ContextMenu.AddMnuItem("Diff Sna_ps",
                                              "Show differences between currently loaded offline snap and another",
                                              AddressOf on_CtxMenuMain
                                              )
                    If _ConnectionMode <> MemSpectMode.Offline Then
                        mitemDiff.IsEnabled = False
                    End If
                End If

            Else

            End If

            Me.ContextMenu.AddMnuItem("_Edit MemSpect.ini File",
                                      "The Config file for this tool " + _iniFileName,
                                      AddressOf on_CtxMenuMain)
            If _IsCalvinHBuild Then
                Me.ContextMenu.AddMnuItem("_DebugInfo",
                                          "ClassNameCache, other debug info (more for offline). Only for _IsCalvinhBuild",
                                          AddressOf on_CtxMenuMain)

            End If



            Me.ContextMenu.AddMnuItem("Analyze ima_ges in a folder",
                                      "Show optimization, resources, duplicates for files in a particular folder",
                                      AddressOf on_CtxMenuMain)



            Me.ContextMenu.AddMnuItem("_Reset UI/Memory",
                                                 "Reset managed instance cnts, iteration counter. Also reread/reprocess INI file options, refresh heap list" +
                                                 vbCrLf + " Clear symbol memory, unload symbol files, (reset code marker dictionary for offline)",
                                                 AddressOf on_CtxMenuMain)

            If Common._ConnectionMode = MemSpectMode.OnLine Then


                Me.ContextMenu.AddMnuItem("Show _Assert Lists",
                                                     "show the seq no's or stackframes on which we'll break",
                                                     AddressOf on_CtxMenuMain)

                itm = Me.ContextMenu.AddMnuItem("Track_Ghost",
                                                  "Normally, the tag (consisting of callstack, SeqNo, Thread) is discarded when memory is freed." + vbCrLf +
                                                  "Ghost will NOT discard the tag info. Thus you can catch certain kinds of inefficiencies," + vbCrLf +
                                                  "things like high memory users what causes lots of GCs. Turns off autorefresh." + vbCrLf +
                                                  "Toggle on/off around scenario. see TrackGhost in INI file",
                                                  AddressOf on_CtxMenuMain)


                itm.IsChecked = _GhostData.IsTracking


                Me.ContextMenu.AddMnuItem("_HeapLeaks",
                                     "Show leftovers after HeapDestroy occurs",
                                     AddressOf on_CtxMenuMain)



                Me.ContextMenu.AddMnuItem("_MemoryEater",
                                                     "eat/free lots of memory in target proc for testing",
                                                     AddressOf on_CtxMenuMain)
                Me.ContextMenu.AddMnuItem("Create Offline Mega _Snapshot",
                                                     "Make an entire MemSpect dump of process for later investigation (Must resolve all symbols: can take 12 mins or more)",
                                                     AddressOf on_CtxMenuMain)
                Me.ContextMenu.AddMnuItem("_LotsAThreads", "create lots of threads for testing",
                                                     AddressOf on_CtxMenuMain)


            Else
                Me.ContextMenu.AddMnuItem("Show _Memory Dictionary", "dump out memory and module dictionaries for minidump or offline snap",
                                                     AddressOf on_CtxMenuMain)

            End If

            Me.ContextMenu.AddMnuItem("_Quit", "If you just close the MemSpect UI window, the parent window will not be terminated. Choose Quit to terminate both.",
                                                 AddressOf on_CtxMenuMain)


        End Sub

        Friend Sub on_CtxMenuMain(ByVal sender As Object, ByVal e As RoutedEventArgs)
            Dim mitem = CType(e.OriginalSource, MenuItem)
            Dim vrb = mitem.Header.ToString
            Select Case vrb
                Case "About MemSpect"
                    Dim ctrls = DataWindowMain.MakeNewDatasurface("About", "About MemSpect", nMaxHeaderHeight:=1)
                    ctrls.SurfaceHeader.Children.Add(New TextBlock With {.Text = "About MemSpect"})
                    Dim abtCtrl = New About()
                    ctrls.SurfaceDetails.Children.Add(abtCtrl)


                Case "_Always On Top"
                    Window.GetWindow(Me).Topmost = Not Window.GetWindow(Me).Topmost
                    mitem.IsChecked = Window.GetWindow(Me).Topmost

                Case "_HeapDensityReport"
                    Dim h As HeapReportUI
                    If Common._ConnectionMode = MemSpectMode.Offline Then
                        If _offlineSnapshot.heapReport Is Nothing Then
                            Throw New InvalidOperationException("Heap report was not saved in snapshot: see 'IncludeHeapReport' ini setting")
                        End If
                        h = New HeapReportUI
                        h.ShowResultsForOffline(_offlineSnapshot.heapReport)
                    Else
                        h = New HeapReportUI
                        h.ShowResults()
                        'HeapReport._ProcessHeapData.Clear()
                    End If
                Case "_Filter..."
                    _VBDiagMarginBase._chkFreeze.IsChecked = True
                    If _oFilterTab Is Nothing Then
                        Dim ctrls = DataWindowMain.MakeNewDatasurface("Filter", "Filter", nMaxHeaderHeight:=1)
                        ctrls.SurfaceDetails.Children.Add(New Filter(_GlobalFilter, Me))
                        _oFilterTab = ctrls.TopSurface
                        'Dim oFilter = New Filter(_GlobalFilter, Me)
                        '_oFilterWin = MyWindow.CreateFrom(oFilter, WinType:="Dlog", fCloseOnUnFreeze:=True)
                        '_oFilterWin.Owner = _windowMain
                        AddHandler _oFilterTab.Unloaded, Sub()
                                                             _oFilterTab = Nothing
                                                         End Sub
                    Else
                        _oFilterTab.Focus()
                    End If
                    '                    _oFilterWin.Show()
                Case "_Clear Filter"
                    _GlobalFilter.ClearFilter()
                    Me._FilterUI.RefreshFilterTextbox()
                    UpdateStatusMsg("Filter Cleared")
                Case "_Edit MemSpect.ini File"
                    If Not String.IsNullOrEmpty(_iniFileName) Then
                        If File.Exists(_iniFileName) Then
                            Process.Start(_iniFileName)
                        Else
                            MessageBox.Show("file not found " + _iniFileName)
                        End If
                    End If

                Case "_SymbolDictionary"
                    UIBase.ShowSymbolDictionary()
                Case "S_ymbolFile (PDB) Load data"
                    SymbolFileUI.CreateSymbolFileDataSurface()
                Case "Change Trac_king Mode to Minimal"
                    SetTrackingMode(TrackingModeEnum.Minimal)
                    Dim nFreed = FreeStackMemory(Nothing)
                    UpdateStatusMsg(String.Format("# frames Freed={0:n0} ({1:n0} bytes)", nFreed, nFreed * 4))

                Case "Change Trac_king Mode to Normal"
                    SetTrackingMode(TrackingModeEnum.Normal)
                Case "Crash Target _Process"
                    UpdateStatusMsg("Crashing target process")
                    SendMsg(ProcMsgVerb.CrashTargetProcess, {CUInt(0), CUInt(1), CUInt(0)}, fSendEndMsgSync:=False)
                    End
                Case "Hang Target Process"
                    UpdateStatusMsg("Hanging target process")
                    SendMsg(ProcMsgVerb.CrashTargetProcess, {CUInt(1), CUInt(0), CUInt(0)}, fSendEndMsgSync:=False)
                    End
                Case "E_xecute Code In Target Process"
                    Dim fileName = Path.Combine(Path.GetDirectoryName(_iniFileName),
                                                "ExecCode.cs")
                    If Not File.Exists(fileName) Then
                        Throw New FileNotFoundException(fileName)
                    End If
                    UpdateStatusMsg(String.Format("Compiling and Executing {0}", fileName))
                    Dim res = CompileAndExecuteFileInTarget(fileName)
                    UpdateStatusMsg(res)
                Case "Show offline snapshot info"
                    Dim notes = Path.Combine(_offlineSnapshot._DataFilePath, "Notes.txt")
                    Process.Start(notes)
                Case "Diff Sna_ps"
                    Dim flderBrowse = New Forms.FolderBrowserDialog
                    'http://stackoverflow.com/questions/15368771/show-detailed-folder-browser-from-a-propertygrid
                    flderBrowse.Description = "Folder from which a snapshot is compared to current"
                    flderBrowse.SelectedPath = Common._offlineSnapshot._DataFilePath
                    UpdateStatusMsgDbg("Set path " + flderBrowse.SelectedPath)
                    flderBrowse.ShowNewFolderButton = False
                    If flderBrowse.ShowDialog() = Forms.DialogResult.OK AndAlso Not String.IsNullOrEmpty(flderBrowse.SelectedPath) Then
                        CLRObjRefsWrapper.WaitTilLoaded() ' for offline/online
                        SnapDifferUI.ShowSnapDiff(flderBrowse.SelectedPath)
                    End If

                Case "_Reset UI/Memory"

                    Me.ResetUi(_chkClrObjects.IsChecked, _chkFreeze.IsChecked)
                    HeapAllocationContainer._dictCodeMarkerGuidLookup = Nothing
                    If _ConnectionMode = MemSpectMode.OnLine Then
                        StackFrameDictionary.Clear()
                        GCData.ClearData()
                        ClrClassInfo.g_DictClassLayouts.Clear()
                        HeapAllocationContainer._dictCodeMarkerGuidLookup = Nothing
                    End If
                    If _ConnectionMode = MemSpectMode.Offline Then
                        Common._CodeMarkerNameDict.Clear()
                    End If
                    KnownIssues.ClearKnownIssues()
                    VsClearSymbols()
                    GC.Collect()
                Case "Show _Assert Lists"
                    UIBase.ShowAssertLists()
                    'Case "DontLogFreesOnMainThread"
                    '    SendMsg(ProcMsgVerb.SetMemFreeLogging, {CUInt(1)})
                Case "Track_Ghost"
                    If Not _GhostData.IsTracking Then
                        _GhostData.StartTracking()
                    Else
                        _GhostData.StopTracking()
                        'CollectGarbageAndGetStats(nTimes:=1, fDoGC:=False) ' just want to update stats: no gc cuz deadlock
                        FreezeTargetThreads()
                        UIBase.DoShowGhostAllocs()

                    End If
                    MakeCtxMenuMain() ' refresh checkmarks

                Case "_ObjectDump"
                    UIBase.ShowObjectDump()
                Case "_DebugInfo"
                    Dim tabctrl = New MyTabControl

                    Dim titem = tabctrl.AddTabItem("LayoutDet",
                                       "",
                                       Sub(sender2 As Object, e2 As RoutedEventArgs)
                                           Dim thetabItem = CType(sender2, TabItem)
                                           Dim qLayoutDet = From a In ClrClassInfo.g_DictClassLayouts
                                                            Let layoutDet = a.Value
                                                            From det In layoutDet.dictFieldInfo
                                                            Select
                                                               ClsId = a.Key.ToInt32.ToString("x8"),
                                                               a.Value.className,
                                                               det.Key,
                                                               det.Value.FldName,
                                                               det.Value.FldType

                                           thetabItem.Content = New Browse(qLayoutDet, fAllowBrowFilter:=True)
                                       End Sub
                    )
                    titem.RaiseEvent(New RoutedEventArgs With {.RoutedEvent = TabItem.GotFocusEvent})


                    tabctrl.AddTabItem("Layouts",
                                       "",
                                        Sub(sender2 As Object, e2 As RoutedEventArgs)
                                            Dim thetabItem = CType(sender2, TabItem)
                                            Dim qLayouts = From a In ClrClassInfo.g_DictClassLayouts
                                                           Let layoutDet = a.Value
                                                           Let ClsIDParentRaw = If(layoutDet IsNot Nothing, a.Value.classIdParent, IntPtr.Zero)
                                                           Select
                                                                ClsId = a.Key.ToInt32.ToString("x8"),
                                                                ClassName = ClrClassInfo.GetClassNameFromClassOrObjectId(a.Key),
                                                                ClsIdParent = ClsIDParentRaw.ToInt32.ToString("x8"),
                                                                ParentClass = If(ClsIDParentRaw = IntPtr.Zero, "", ClrClassInfo.GetClassNameFromClassOrObjectId(ClsIDParentRaw)),
                                                                clsSize = If(layoutDet IsNot Nothing, a.Value.classSize, 0),
                                                                clsFldCnt = If(layoutDet IsNot Nothing, a.Value.dictFieldInfo.Count, 0)


                                            thetabItem.Content = New Browse(qLayouts, fAllowBrowFilter:=True)
                                        End Sub
                    )


                    If _ConnectionMode = MemSpectMode.Offline Then


                        tabctrl.AddTabItem("System.String",
                                           "",
                                            Sub(sender2 As Object, e2 As RoutedEventArgs)
                                                Dim thetabItem = CType(sender2, TabItem)

                                                Dim qSysStringDict = From a In Common._offlineSnapshot._systemStringDictionary
                                                                     Select
                                                                     ObjId = a.Key.ToInt32.ToString("x8"),
                                                                     str = a.Value


                                                thetabItem.Content = New Browse(qSysStringDict, fAllowBrowFilter:=True)
                                            End Sub
                        )


                        tabctrl.AddTabItem("MappedFiles",
                                           "",
                                            Sub(sender2 As Object, e2 As RoutedEventArgs)
                                                Dim thetabItem = CType(sender2, TabItem)

                                                Dim qMappedFiles = From a In Common._offlineSnapshot._mappedFilesDictionary
                                                                   Select
                                                                   Addr = a.Key.ToInt32.ToString("x8"),
                                                                   a.Value


                                                thetabItem.Content = New Browse(qMappedFiles, fAllowBrowFilter:=True)
                                            End Sub
                        )


                        tabctrl.AddTabItem("HeapHands",
                                           "",
                                            Sub(sender2 As Object, e2 As RoutedEventArgs)
                                                Dim thetabItem = CType(sender2, TabItem)

                                                Dim qheapHands = From a In Common._offlineSnapshot._realHeapHandleDictionary
                                                                 Select
                                                                 Addr = a.Key.ToInt32.ToString("x8"),
                                                                 Handle = a.Value.ToInt32.ToString("x8")


                                                thetabItem.Content = New Browse(qheapHands, fAllowBrowFilter:=True)
                                            End Sub
                        )


                        tabctrl.AddTabItem("ClrLoad",
                                           "",
                                            Sub(sender2 As Object, e2 As RoutedEventArgs)
                                                Dim thetabItem = CType(sender2, TabItem)

                                                Dim qClrLoad = From a In Common._offlineSnapshot._clrLoadDictionary
                                                               Select
                                                               Addr = a.Key.ToInt32.ToString("x8"),
                                                               a.Value


                                                thetabItem.Content = New Browse(qClrLoad, fAllowBrowFilter:=True)
                                            End Sub
                        )

                    End If


                    Dim ctrls = DataWindowMain.MakeNewDatasurface("Debug", "", 45)
                    Dim spHoriz = New StackPanel With {.Orientation = Controls.Orientation.Horizontal}
                    Dim btnResetLayouts = New Button With {
                        .Content = "_Reset class Layouts, class names, StackFrameDictionary",
                        .ToolTip = "Resets only if online"
                    }
                    AddHandler btnResetLayouts.Click, Sub()
                                                          If _ConnectionMode = MemSpectMode.OnLine Then
                                                              ClrClassInfo.g_DictClassLayouts.Clear()
                                                              StackFrameDictionary.Clear()
                                                              HeapAllocationContainer._dictCodeMarkerGuidLookup = Nothing
                                                              GCData.ClearData()
                                                          End If
                                                      End Sub
                    spHoriz.Children.Add(btnResetLayouts)
                    ctrls.SurfaceHeader.Children.Add(spHoriz)

                    ctrls.SurfaceDetails.Children.Add(tabctrl)

                Case "Show _Threads"
                    UIBase.DoShowThreads()
                Case "Show Allocations with _Waste"
                    DoShowAllocationsWithWaste("Allocs with Waste in all heaps")
                Case "Module Fold"
                    ShowModFold()
                Case "Duplicates"
                    ShowDuplicateAllocations(_HeapList, Nothing, "All heaps")
                Case "Export to Perf_View"
                    PerfViewExport.Export(hctrList:=Nothing, outputFileName:=String.Empty)
                Case "Show _KnownIssues"
                    ShowKnownIssues()
                Case "Show _MemStats"
                    ShowMemStats()
                Case "Analyze ima_ges in a folder"
                    Dim imgui = New ImageUi(vm:=Nothing)
                    imgui.ShowOptDataForFolderHelper(String.Empty, fShowBrowserDialog:=True)
                Case "_HeapLeaks"
                    Dim qfunc = Function(theheapAllocs As List(Of HeapAllocationContainer), thebmem As BrowseMem) As IEnumerable
                                    Dim qDetails As IEnumerable
                                    qDetails = From a In theheapAllocs
                                               Select
                                                   Address = a.GetAddr.ToString("x8"),
                                                   a.AllocationStruct.SeqNo,
                                                   Size = a.GetSize,
                                                   a.AllocationStruct.Thread,
                                                   Heap = a.SpyHeapPtr.HeapName,
                                                   StringContent = a.GetStringContent,
                                                   _HeapAllocationContainer = a
                                               Order By SeqNo

                                    thebmem.ColumnTips = {TIP_ADDRESS, TIP_SEQNO, TIP_SIZE, TIP_THREAD, TIP_STRING_CONTENT}
                                    thebmem.InitialSortOrder = New BrowseInitialSortOrder With {.ColumnNo = 2, .direction = ListSortDirection.Ascending}
                                    Return qDetails
                                End Function
                    ShowSubSnapShot(_HeapLeaks, "Heap leftovers after HeapDestroy called", qfunc)
                Case "_MemoryEater"
                    Dim x = New MemoryEaterUI
                Case "Create Offline Mega _Snapshot"
                    ProcComm.FreezeTarget()

                    Me.UpdateLayout()
                    Dim flderBrowse = New Forms.FolderBrowserDialog
                    flderBrowse.Description = "Folder into which files will be created"
                    flderBrowse.SelectedPath = Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)
                    If flderBrowse.ShowDialog() = Forms.DialogResult.OK AndAlso Not String.IsNullOrEmpty(flderBrowse.SelectedPath) Then
                        Dim act = New CreateMegaSnapActivity
                        Dim inargs = New Dictionary(Of String, Object) From {{"InSnapFolder", flderBrowse.SelectedPath}}
                        Dim wfapp = New WorkflowApplication(act, inargs)
                        '                        wfapp.SynchronizationContext = System.Threading.SynchronizationContext.Current
                        wfapp.Run()
                    End If
                Case "_LotsAThreads"
                    Dim nThreads = 10
                    Dim nLeak = 100
                    Dim nSleep = 100
                    SendMsg(ProcMsgVerb.LotsOfThreads, fSendEndMsgSync:=True, dwords:={nThreads, nSleep, nLeak})
                    For i = 0 To nThreads - 1
                        Dim nThreadId = Marshal.ReadInt32(_SharedMemAddr, i * 4)
                        UpdateStatusMsg(String.Format("Thread # {0} Id = {1} IntentionalLeakBytes{2}", i, nThreadId, nLeak))

                    Next
                Case "Show _Memory Dictionary"
                    MiniDumpReader.Singleton.MakeMemoryDictionary()

                    Dim qMemDict = From a In MiniDumpReader.Singleton._MemoryDictionary
                                   Select
                                   Address = a.Key.StartAddr.ToInt32.ToString("x8"),
                                   EndAddress = a.Key.StartAddr.MyAdd(a.Key.nSize - 1).ToInt32.ToString("x8"),
                                   SizeHex = a.Key.nSize.ToString("x8"),
                                   Size10 = a.Key.nSize,
                                   FileOffsetHex = a.Value.ToString("x8"),
                                   FileOffset10 = a.Value

                    Dim ctrls = DataWindowMain.MakeNewDatasurface("Mem Dict", "", nMaxHeaderHeight:=30)
                    ctrls.SurfaceHeader.Children.Add(
                        New TextBlock With {
                            .Text = "Memory and Module dictionaries from minidump"
                        })

                    Dim brMemDict = New Browse(qMemDict,
                                        fAllowBrowFilter:=True,
                                        ColWidths:={
                                            WIDTH_ADDRESS,
                                            WIDTH_ADDRESS,
                                            WIDTH_ADDRESS,
                                            WIDTH_ADDRESS + 20,
                                            WIDTH_ADDRESS,
                                            WIDTH_ADDRESS + 20
                                                    })
                    ctrls.SurfaceDetails.Children.Add(brMemDict)


                    MiniDumpReader.Singleton.LoadModules()

                    Dim qModuleDict = From a In MiniDumpReader.Singleton._ModuleDictionary
                                      Select
                                      Address = a.Key.ToInt32.ToString("x8"),
                                      Size = a.Value.minidump_Module.SizeOfImage.ToString("n0"),
                                      a.Value.ModuleName,
                                      a.Value.minidump_Module.TimeDateStamp
                                      Order By ModuleName


                    Dim brModuleDict = New Browse(qModuleDict,
                                                  fAllowBrowFilter:=True,
                                        ColWidths:={
                                            WIDTH_ADDRESS,
                                            WIDTH_ADDRESS + 10,
                                            500,
                                            WIDTH_ADDRESS,
                                            WIDTH_ADDRESS + 20,
                                            WIDTH_ADDRESS,
                                            WIDTH_ADDRESS + 20
                                                    })


                    ctrls.SurfaceDetails.Children.Add(brModuleDict)
                Case "_Quit"
                    _windowMain.SaveSettings()
                    If DataWindowMain._DataWindowMain IsNot Nothing Then
                        DataWindowMain._DataWindowMain.Close()
                    End If
                    If _ConnectionMode = MemSpectMode.OnLine Then
                        SendMsg(ProcMsgVerb.Quit, fSendEndMsgSync:=False, dwords:={1}) ' terminate parent process (which will terminate UI proc too)
                    End If
                    End 'end program
                Case Else
                    MessageBox.Show("ClicKed: " + mitem.Header.ToString)
            End Select

        End Sub
        Sub ResetUi(ByVal chkClrObjVal As Boolean?, ByVal chkFreezeVal As Boolean?)
            MakeCtxMenuMain()
            Me.Children.Clear()
            If _txtStatBar IsNot Nothing AndAlso _txtStatBar.Parent IsNot Nothing Then
                CType(_txtStatBar.Parent, Panel).Children.Clear()
            End If
            If _txtStatus IsNot Nothing AndAlso _txtStatus.Parent IsNot Nothing Then
                CType(_txtStatus.Parent, Panel).Children.Clear()
            End If
            'If Not _IsClientOutOfProcess And _ConnectionMode = MemSpectMode.OnLine Then
            '    If GetPrivateProfileInt(ProfileStringSection, "fTrackVirtualMem", 0, _iniFileName) > 0 Or
            '        GetPrivateProfileInt(ProfileStringSection, "fTrackHeap", 0, _iniFileName) > 0 Then
            '        If GetModuleHandle("vsassert") = IntPtr.Zero Then ' if vsassert isn't loaded yet and we want to track stuff (could be all retail)
            '            ' call an entry point to cause DLL to load
            '            Try
            '                VsDebIsValidHeap(New IntPtr(0))
            '            Catch
            '                Dim cFullName = IO.Path.Combine(IO.Path.Combine(IO.Directory.GetCurrentDirectory(), "x86"), "vsassert.dll")
            '                Using ldr = New DynamicDllLoader(cFullName)
            '                    VsDebIsValidHeap(New IntPtr(0))
            '                End Using

            '            End Try
            '        End If

            '    End If
            'End If


            _nAutoGCSecs = GetPrivateProfileInt(ProfileStringSection, "AutoGCSeconds", 0, _iniFileName)
            _ShowAddressInCallStacks = GetPrivateProfileInt(ProfileStringSection, "ShowAddressInCallStacks", 0, _iniFileName)

            _btnGC = New Button With {
                .Content = "_GC",
                .ToolTip = <xml>
Do a Garbage Collection. GC's are automatic by MemSpect when freezing the target.
Marshal.CleanupUnusedObjectsInCurrentContext is also called, so no Manual GC is ever required.
MemSpect version = <%= _MemSpectVersion %>
CalvinH, BenBrad, VanceM. 10/01/09
Shamelessly implemented 
by copying/pasting 
from a multitude of sources
see http://calvinh6/MemSpect

                    </xml>.Value
            }

            _nGCLoopCount = GetPrivateProfileInt(ProfileStringSection, "GCLoopCount", 1, _iniFileName)
            _nCleanUpRCWs = GetPrivateProfileInt(ProfileStringSection, "CleanUpRCWs", 1, _iniFileName)
            '_nGCWaitForOtherThreads = 1000 * GetPrivateProfileInt(ProfileStringSection, "GCWaitForOtherThreads ", 1000, _iniFileName)

            Dim sb = New Text.StringBuilder(256)
            GetPrivateProfileString(ProfileStringSection, "FontName", "Arial", sb, sb.Capacity, _iniFileName)
            _FontName = sb.ToString

            GetPrivateProfileString(ProfileStringSection, "FontSize", "8", sb, sb.Capacity, _iniFileName)
            _FontSize = CInt(sb.ToString)

            GetPrivateProfileString(ProfileStringSection, "Foreground", "Blue", sb, sb.Capacity, _iniFileName)
            _Foreground = sb.ToString

            GetPrivateProfileString(ProfileStringSection, "Background", "White", sb, sb.Capacity, _iniFileName)
            _Background = sb.ToString


            _btnMenu = New Button With {
                .Content = "_Menu",
                .ToolTip = "Show context menu" + vbCrLf +
                        "ContentType = " + _ContentType + vbCrLf +
                        "Ini file = " + _iniFileName + vbCrLf +
                        "Internal Private Heap Handle= " + ProcComm._hDebugHeap.ToString("x8") + vbCrLf +
                        "ProcessHeap = " + ProcComm._hProcessHeap.ToString("x8") + vbCrLf +
                        "MemSpectHeap = " + ProcComm._hMemSpectHeap.ToString("x8") + vbCrLf +
                        "_NT_SYMBOL_PATH=" + System.Environment.GetEnvironmentVariable("_NT_SYMBOL_PATH") + vbCrLf +
                        "4gig stack = " + _fHandle4gigStacks.ToString
                        }

            'LogFileFolder=C:\users\Calvinh\Documents\
            sb = New Text.StringBuilder(256)
            GetPrivateProfileString(ProfileStringSection, "LogFileFolder", "", sb, sb.Capacity, _iniFileName)
            Dim LogFileFolder = ""
            If sb.Length = 0 Then
                LogFileFolder = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) 'C:\users\Calvinh\Documents\
            Else
                LogFileFolder = sb.ToString
            End If
            If LogFileFolder.ToLower = "none" Then
                _logFile = ""
            Else
                Try
                    Dim MainModname = ""
                    If _IsClientOutOfProcess Then
                        MainModname = IO.Path.GetFileNameWithoutExtension(_TargetProc.MainModule.FileName)
                    ElseIf _ConnectionMode = MemSpectMode.Offline Then
                        MainModname = "OfflineMode " + IO.Path.GetFileNameWithoutExtension(_offlineSnapshot._DataFilePath)
                    Else
                        sb.Clear()
                        GetModuleFileName(IntPtr.Zero, sb, sb.Capacity)
                        MainModname = IO.Path.GetFileNameWithoutExtension(sb.ToString)
                    End If
                    _logFile = IO.Path.Combine(LogFileFolder, MainModname + "Log" + If(_IsClientOutOfProcess, "R", "") + ".csv")

                Catch ex As Exception
                    ' a 32 bit process cannot access a 64...

                End Try
            End If

            _OutputToDebugWindow = GetPrivateProfileInt(ProfileStringSection, "OutputToDebugWindow", 0, _iniFileName)


            _AvailVMMemRed = GetPrivateProfileInt(ProfileStringSection, "AvailVMMemRed", 65536, _iniFileName)
            _LargestFreeVMSizeRed = GetPrivateProfileInt(ProfileStringSection, "LargestFreeVMSizeRed", 512000, _iniFileName)

            Dim nCnt = 0 ' 0 is norls, 1 is heap
            Dim nTotMem = 0

            '_txtNrlsMem = New TextBlock With {
            '    .MaxWidth = MaxWidthMargin,
            '    .HorizontalAlignment = HorizontalAlignment.Left,
            '    .Text = String.Format("NRLS Tot = {0:n0}, Cnt={1:n0}", nTotMem, nCnt),
            '    .ToolTip = "NoRelease Allocator Build CHK vb\language\shared with NRLSTrack on, rebuild VSPackage"
            '}

            _txtGCStat = New TextBlock With {
                .MaxWidth = MaxWidthMargin,
                .HorizontalAlignment = HorizontalAlignment.Left,
                .Background = Brushes.Khaki,
                .FontSize = 10,
                .ToolTip = "Garbage Collection stats: 4 generations: Large, 2, 1,0.  Srv= Survived, Mov=Moved, Col = Collected" + vbCrLf +
                    "An obj may survive a GC due to it's generation not being collected. The true indication of an object's age is its Seqno"
            }

            _txtmemStat = New TextBlock With {
                .HorizontalAlignment = HorizontalAlignment.Left,
                .MaxWidth = MaxWidthMargin,
                .FontSize = 10
            }
            _txtVirtmemStat = New TextBlock With {
                .HorizontalAlignment = HorizontalAlignment.Left,
                .ToolTip = "VirtMem stat" + _logFile + vbCrLf + .Text,
                .MaxWidth = MaxWidthMargin,
                .FontSize = 10
            }
            _FilterUI = New FilterUI

            _txtMaxSeqNo = New TextBlock With {
                .HorizontalAlignment = HorizontalAlignment.Left,
                .MaxWidth = MaxWidthMargin,
                .Background = Brushes.LightGreen,
                .FontSize = 10,
                .ToolTip = "Max Alloc # (pass count). Allows chronological ordering for leak detection. DeltaAlloc  = change in # Allocated items (? of total # of allocations tracked by MemSpect. Negative means a net decrease)" + vbCrLf +
                "Delta  (ΔSeq) = # allocations since last refresh (some may have been freed)"
            }

            _btnVirtMem = New Button With {
                .Content = "_VirtualMem",
                .ToolTip = "See Virtual memory layout, images loaded" + vbCrLf +
                        "Takes a few seconds. see my blog http://blogs.msdn.com/calvin_hsia/archive/2009/10/19/9909326.aspx"
            }
            _chkClrObjects = New CheckBox With {
                .Content = "_ClrObject",
                .IsChecked = If(chkClrObjVal.HasValue, chkClrObjVal, False),
                .ToolTip = "Track Managed Objects in the CLR: will be slower" + vbCrLf +
                "You can turn this on later when you care about callstacks for managed obj creation" + vbCrLf +
                "Mixed mode callstack collections are unaffected by this setting." + vbCrLf +
                "Toggling this off/on resets all class instance & collected counts, so you can use this in small scenarios to see GC churn"
            }
            _chkFreeze = New CheckBox With {
                .Content = "_Freeze",
                .IsChecked = If(chkFreezeVal.HasValue, chkFreezeVal, ProcComm._isTargetFrozen),
                .ToolTip = "Freeze or unfreeze most threads in parent process. Beware deadlocks" + vbCrLf +
                "Taking snapshots automatically freezes"
            }

            Dim spControls1 As New StackPanel With {.Orientation = Orientation.Horizontal}
            _btnNrlsMem = New Button With {
                .Content = "NRLSMem",
                .ToolTip = "NoRelaseAllocator Memory Report(CHK VB only: uses testhook)" + vbCrLf +
                        "Build CHK vb\language\shared with NRLSTrack on, rebuild VSPackage"
            }

            _btnStrPool = New Button With {
                .Content = "StrPool",
                .ToolTip = "VB StringPool(CHK VB only: uses testhook)" + vbCrLf +
                        "Build CHK VSPackage. Inproc MEF Only"
            }
            Dim gr = New Grid()
            gr.RowDefinitions.Add(New RowDefinition() With {.Height = CType((New GridLengthConverter()).ConvertFromString("Auto"), GridLength)})
            Dim spTop = New StackPanel With {.Orientation = Controls.Orientation.Vertical}
            spTop.Children.Add(spControls1) ' add the ctrl stackpanel to the margin
            Dim dpTop = New DockPanel
            dpTop.Children.Add(spTop)
            gr.Children.Add(dpTop)
            gr.RowDefinitions.Add(New RowDefinition() With {.Height = CType((New GridLengthConverter()).ConvertFromString("*"), GridLength)})
            Grid.SetRow(dpTop, 0)
            Dim dpHeaps = New DockPanel
            gr.RowDefinitions.Add(New RowDefinition() With {.Height = CType((New GridLengthConverter()).ConvertFromString("5"), GridLength)})
            Grid.SetRow(dpHeaps, 1)
            gr.Children.Add(dpHeaps)
            Dim grSplitter = New GridSplitter() With {.HorizontalAlignment = Windows.HorizontalAlignment.Stretch, .Background = Brushes.AliceBlue, .ToolTip = "Drag me around"}
            Grid.SetRow(grSplitter, 2)
            gr.Children.Add(grSplitter)
            gr.RowDefinitions.Add(New RowDefinition() With {.Height = CType((New GridLengthConverter()).ConvertFromString("*"), GridLength)})
            Dim dpStatTxt = New DockPanel
            Grid.SetRow(dpStatTxt, 3)
            gr.Children.Add(dpStatTxt)
            gr.RowDefinitions.Add(New RowDefinition() With {.Height = CType((New GridLengthConverter()).ConvertFromString("14"), GridLength)})
            Dim dpStatBar = New DockPanel
            Grid.SetRow(dpStatBar, 4)
            gr.Children.Add(dpStatBar)
            Me.Children.Add(gr)
            AddHandler dpHeaps.SizeChanged, Sub()
                                                If _spDebugHeaps IsNot Nothing AndAlso _spDebugHeaps.Children.Count = 1 AndAlso dpHeaps.ActualHeight > 10 Then
                                                    Dim child = _spDebugHeaps.Children(0)
                                                    If TryCast(child, Browse) IsNot Nothing Then
                                                        CType(child, Browse).Height = dpHeaps.ActualHeight - 10
                                                    End If
                                                End If
                                            End Sub
            AddHandler dpStatTxt.SizeChanged, Sub()
                                                  If _txtStatus IsNot Nothing AndAlso dpStatTxt.ActualHeight > 5 Then
                                                      _txtStatus.Height = dpStatTxt.ActualHeight - 5
                                                  End If
                                              End Sub
            If Common._ConnectionMode <> MemSpectMode.OnLine Then

                spControls1.Children.Add(_btnMenu)

                spTop.Children.Add(New TextBlock With {.Text = "MemSpect version " + _MemSpectUICurrentVersion(fIncludeTimeStamp:=True)})
                spTop.Children.Add(_FilterUI)

                _spDebugHeaps = New DockPanel
                If Not _IsLoadingOfflineSnapshot Then
                    RefreshHeapList()
                End If

                spControls1.Children.Add(_btnVirtMem)
                dpHeaps.Children.Add(_spDebugHeaps)
                dpStatTxt.Children.Add(_txtStatus)
                dpStatBar.Children.Add(_txtStatBar)

            Else

                spControls1.Children.Add(_btnGC)
                spControls1.Children.Add(_btnMenu)
                spControls1.Children.Add(_btnVirtMem)
                If _IsClientOutOfProcess Then
                    SendMsg(ProcMsgVerb.ClrObjTrk, fSendEndMsgSync:=True, dwords:={CUInt(2)}) ' read current setting
                    Dim sResult = Marshal.ReadInt32(_SharedMemAddr)
                    If sResult = 1 Then
                        _fChkClrObjRecursive = True ' prevent recursion, race condition: timer fires can calls GC, causing RCW thread to start when CLR not ready
                        _chkClrObjects.IsChecked = True
                        _fChkClrObjRecursive = False
                    End If
                    spControls1.Children.Add(_chkClrObjects)
                    spControls1.Children.Add(_chkFreeze)
                Else
                    spControls1.Children.Add(_btnNrlsMem)
                    spControls1.Children.Add(_btnStrPool)
                End If
                spControls1.Children.Add(New Label With {
                                         .Content = "Refresh:",
                                         .ToolTip = "Refresh (seconds) can do a GC or update stats (colored managed object list is dynamically updated) ."
                                     })

                _txtAutoGCSecs = New TextBox With {
                    .Text = _nAutoGCSecs.ToString,
                    .ToolTip = "= # seconds to wait for autogc. Set to 0 for no AutoGC"
                }
                OntxtAutoGCSecsLostFocus()
                spControls1.Children.Add(_txtAutoGCSecs)
                spTop.Children.Add(_txtGCStat)

                spTop.Children.Add(_txtmemStat) ' add the txt stackpanel to the margin


                spTop.Children.Add(_txtVirtmemStat) ' add the virtmem stat to the margin
                spTop.Children.Add(_txtMaxSeqNo)
                spTop.Children.Add(New TextBlock With {
                                .Text = "MemSpect Ver " + _MemSpectUICurrentVersion(fIncludeTimeStamp:=True),
                                .Foreground = Brushes.DarkCyan,
                                .FontSize = 8
                            })
                spTop.Children.Add(_FilterUI)

                _spDebugHeaps = New DockPanel
                RefreshHeapList()
                dpHeaps.Children.Add(_spDebugHeaps)
                dpStatTxt.Children.Add(_txtStatus)
                dpStatBar.Children.Add(_txtStatBar)

                CollectGarbageAndGetStats(0) ' get init mem stats with no gc
                _nIterations = 0

                AddHandler ProcComm.TargetFrozen, AddressOf OnTargetFrozenStateChanged
                AddHandler ProcComm.TargetUnfrozen, AddressOf OnTargetFrozenStateChanged
            End If

        End Sub

        Private Sub OnTargetFrozenStateChanged()
            _chkFreeze.Dispatcher.Invoke(Sub()
                                             _chkFreeze.IsChecked = ProcComm._isTargetFrozen
                                         End Sub)
        End Sub

        Friend Shared _DidShowSymbolPath As Boolean = False

        Friend Shared Function ShowSymbolPath() As String
            Dim symbolpath = String.Empty
            If Not _DidShowSymbolPath Then
                _DidShowSymbolPath = True ' show sympath after we've init'd syms by resolve syms when getting heap names
                If _ConnectionMode = MemSpectMode.Existing OrElse _ConnectionMode = MemSpectMode.OnLine Then

                    ' or _isUnderTest so StartLife gets sympath
                    If _ConnectionMode = MemSpectMode.Existing OrElse _IsUnderTest Then ' need to init symserv by calling it to resolve inptr.zero
                        Dim sb = New Text.StringBuilder(100)
                        Dim res = VsResolveSymbolEx(_hProcessTarget, IntPtr.Zero, sb, sb.Capacity, fNoFileLineInfo:=False)

                    End If

                    symbolpath = SymbolFiles.CheckSymbolFileStatus(fCheckSymCache:=True)
                    UpdateStatusMsg("SymPath = " + symbolpath)
#If DEBUG Then
                    UpdateStatusMsg("DebugSymPath: hard coded to D: see options.cpp") ' see options.cpp    // adjust symbol path to work around FwcWsp.dll changing our DbgHelp options in Debug version
#End If

                End If
            End If
            Return symbolpath
        End Function

        Public Sub RefreshHeapList(Optional ByVal fClearFirst As Boolean = False)
            If _spDebugHeaps Is Nothing Then
                Return ' on init: if gc button pushed before heaplist gen'd
            End If
            _spDebugHeaps.Children.Clear()
            _HeapList.Clear()
            If ReadHeaps(fClearFirst) Then
                If _ConnectionMode = MemSpectMode.OnLine Then
                    ShowSymbolPath()
                End If
                Dim q = From a In _HeapList
                        Where a.CurTotBytes > 0
                        Select Heap = a.HeapName,
                        CurSize = a.CurTotBytes,
                        nLive = a.CurNumAllocs,
                        _Heap = a

                Dim initSortOrder = New BrowseInitialSortOrder With {.ColumnNo = 1, .direction = ComponentModel.ListSortDirection.Ascending}
                Dim coltips = {"HeapName is obtained from resolving symbol of caller to HeapCreate." + vbCrLf + "There can be multiple with the same name. Heaps with 0 allocations are omitted for space reasons. " +
                                        vbCrLf + "Use HeapDensity report to see all heaps" +
                                        vbCrLf + "MemSpect's private heap is " + MemSpectHeapName + " in which VirtualAllocs, ClrObjs, etc. are tracked",
                             "Current size of heap (since MemSpect started in process: some heap allocations in heaps created before MemSpect before are not included: mainly msvcrt and ProcessHeap)" + vbCrLf +
                             "The " + MemSpectHeapName + " size is misleading, so set to 1",
                            "# of live allocations" + vbCrLf +
                            "The total of this column is the total # of callstacks currently stored by MemSpect"
                            }
                Dim brow = New Browse(q,
                                      ColWidths:=New Integer() {If(_IsClientOutOfProcess, 155, 120), 90, 90, 65},
                                      fAllowHeaderClickSort:=True,
                                      InitialSortOrder:=initSortOrder,
                                      ColTips:=coltips
                                      )
                AddHandler brow._BrowseList.MouseDoubleClick, AddressOf OnHeapDblClick
                AddHandler brow._BrowseList.MouseMove, AddressOf OnHeapMouseMove
                Dim cm = brow._BrowseList.ContextMenu
                Dim lamctxmenu = Sub(sender As Object, e As RoutedEventArgs)
                                     Using New DataWindowMain.SetCursorWait

                                         Dim mitem = CType(e.OriginalSource, MenuItem)
                                         Dim selItem = brow._BrowseList.SelectedItem
                                         Dim vrb = mitem.Header.ToString
                                         Dim tdesc = ComponentModel.TypeDescriptor.GetProperties(selItem)("_Heap")
                                         Dim tdval As Object = Nothing
                                         If tdesc IsNot Nothing Then
                                             tdval = tdesc.GetValue(selItem)
                                         End If
                                         If tdval IsNot Nothing Then
                                             Dim hp = CType(tdval, CSpyHeap)
                                             Select Case vrb
                                                 Case "_Snapshot"
                                                     FreezeTarget()
                                                     Dim act = New ShowSnapActivity
                                                     Dim inargs = New Dictionary(Of String, Object) From {{"InHeap", hp}}
                                                     Dim wfapp = New WorkflowApplication(act, inargs)
                                                     wfapp.SynchronizationContext = System.Threading.SynchronizationContext.Current
                                                     wfapp.OnUnhandledException = Function(workflowEx As WorkflowApplicationUnhandledExceptionEventArgs) As UnhandledExceptionAction
                                                                                      Dim res As UnhandledExceptionAction = UnhandledExceptionAction.Abort
                                                                                      UpdateStatusMsg(workflowEx.UnhandledException.ToString())
                                                                                      Return res
                                                                                  End Function
                                                     wfapp.Run()
                                                     'Dim bg = New BackgroundWorker
                                                     'Dim snap As MemSnapshot
                                                     'AddHandler bg.DoWork, Sub()
                                                     '                          DataWindowMain.SetCursor(Cursors.Arrow)
                                                     '                          snap = hp.TakeMemSnapshot(fEnableFilter:=True)
                                                     '                      End Sub
                                                     'AddHandler bg.RunWorkerCompleted, Sub()
                                                     '                                      ShowSnapshot(hp, snap)
                                                     '                                      DataWindowMain.SetCursor(Cursors.Wait)
                                                     '                                  End Sub
                                                     'bg.RunWorkerAsync()
                                                 Case "Find Lea_ks"
                                                     CommonUI.SearchForLeaksInAllocs(hp.TakeMemSnapshot().Allocs)
                                                 Case "_Duplicates"
                                                     Try
                                                         Dim winName = ""
                                                         Dim snap As MemSnapshot = Nothing
                                                         If brow._BrowseList.SelectedItems.Count > 0 Then
                                                             Dim lstAllocs As New List(Of HeapAllocationContainer)
                                                             Dim qhps = From itm In brow._BrowseList.SelectedItems
                                                                        Select
                                                                        typdesc = ComponentModel.TypeDescriptor.GetProperties(itm)("_Heap"),
                                                                        itm
                                                                        Select heap = CType(tdesc.GetValue(itm), CSpyHeap)




                                                             'For Each itm In Me._menu._itemsctl.SelectedItems
                                                             '    tdesc = ComponentModel.TypeDescriptor.GetProperties(itm)("_Heap")
                                                             '    hp = CType(tdesc.GetValue(itm), CSpyHeap)
                                                             '    heaps.Add(hp)
                                                             '    winName += " " + hp.GetHeapName
                                                             '    snap = hp.TakeMemSnapshot
                                                             '    Dim qhp As IEnumerable(Of HeapAllocationContainer)
                                                             '    If hp.IsMemSpectHeap Then
                                                             '        qhp = From a In snap.Allocs
                                                             '                 Where a.TblkBlockType = BlockTypes.ClrObject OrElse a.TblkBlockType = BlockTypes.HeapAlloc

                                                             '    Else
                                                             '        qhp = From a In snap.Allocs

                                                             '    End If
                                                             'Next
                                                             ShowDuplicateAllocations(qhps, Nothing, winName)
                                                         Else
                                                             MessageBox.Show("no heap(s) selected")
                                                             Return
                                                         End If

                                                     Catch ex As Exception
                                                         CommonUI.MemSpectExceptionHandler(ex)
                                                     End Try
                                                 Case "Export to Perf_View"
                                                     PerfViewExport.Export(hp.TakeMemSnapshot(fEnableFilter:=True).Allocs, outputFileName:=String.Empty)

                                                 Case "_HeapWalkMap"
                                                     Dim hrpt = New HeapReportUI
                                                     hrpt.DoHeapWalkMapReport(hp.GetRealOSHeapHandle, selItem.ToString)
                                                 Case "_FreeStackMemory"
                                                     Dim nFreed = FreeStackMemory(hp)
                                                     UpdateStatusMsg(String.Format("'{0}' # frames Freed={1:n0} ({2:n0} bytes)", hp.HeapName, nFreed, nFreed * 4))

                                                 Case Else
                                                     MessageBox.Show("ClicKed: " + mitem.Header.ToString)
                                             End Select
                                         End If
                                     End Using
                                 End Sub
                'reverse order 
                If _ConnectionMode = MemSpectMode.OnLine Then
                    cm.AddMnuItem("_FreeStackMemory",
                                  "Free all stacks so far accumulated in this heap, to free memory", CType(lamctxmenu, RoutedEventHandler), 0)
                End If

                cm.AddMnuItem("_HeapWalkMap", "Map heap mem to VM", CType(lamctxmenu, RoutedEventHandler), 0)

                cm.AddMnuItem("Export to Perf_View", "Export to an XML file that PerfView (\\clrmain\tools\PerfView.exe) can read.  Must have extension '.perfview.xml'. Respects Filter", CType(lamctxmenu, RoutedEventHandler), 0)

                cm.AddMnuItem("_Duplicates",
                              "Find all duplicated memory in heaps (multiselect too: freeze first!) (Blocks identical in size and content)",
                              CType(lamctxmenu, RoutedEventHandler), 0)
                cm.AddMnuItem("Find Lea_ks",
                              <xml> From BenBrad:
The Find Leaks dialog can take more than 2 sequence ranges.  The way I use it is to record the low,high sequence numbers for an action multiple times (say 4), then provide them all to the dialog.  It will then show you matching allocations and what range they occurred in, ie:
 
    low1,high1
    low2,high2
    low3,high3
    low4,high4
 
Usually, I don't collect sequence numbers for the 1st time an action is performed in the IDE because of one-time initializations (image loads, cache creations, etc).
 
If you click the Exact match checkbox, it will only show groups of allocations that have exact callstack matches and at least one allocation in every range. 
 
If you use the % option, it will show you groups of allocations that have at least that percentage of frames matching in any range, so you may find some ranges have 0.
                              </xml>.Value,
                              CType(lamctxmenu, RoutedEventHandler), 0)
                cm.AddMnuItem("_Snapshot",
                              "Show a snapshot of this heap. Will freeze if thawed",
                              CType(lamctxmenu, RoutedEventHandler), 0)

                _spDebugHeaps.Children.Add(brow)
            Else
                _spDebugHeaps.Children.Add(New Label With {.Content = "Online and Offline modes show heaplist", .ToolTip = ""})
                UpdateStatusMsg("ConnMode = " + _ConnectionMode.ToString)
                ShowSymbolPath()
            End If
        End Sub

        Friend Shared Sub FreezeTargetThreads()
            If _ConnectionMode = MemSpectMode.OnLine Then
                If _VBDiagMarginBase IsNot Nothing Then
                    If Not _VBDiagMarginBase._chkFreeze.IsChecked Then
                        _VBDiagMarginBase._chkFreeze.IsChecked = True
                    End If
                End If
            End If
        End Sub

        Friend Shared Function IsTargetFrozen() As Boolean
            Return ProcComm._isTargetFrozen

        End Function

        'Protected Overrides Sub OnGotKeyboardFocus(ByVal e As System.Windows.Input.KeyboardFocusChangedEventArgs)
        '    '            MyBase.OnGotKeyboardFocus(e)
        '    System.Windows.Input.Keyboard.Focus(_wpfTextViewHost.TextView.VisualElement)

        'End Sub


        Sub OntxtAutoGCSecsLostFocus() Handles _txtAutoGCSecs.LostFocus
            Try
                _nAutoGCSecs = If(_txtAutoGCSecs.Text.Length > 0, CInt(_txtAutoGCSecs.Text), 0)
                If _nAutoGCSecs > 0 Then
                    _timerAutoGc = New DispatcherTimer
                    _timerAutoGc.Interval = TimeSpan.FromSeconds(_nAutoGCSecs)
                    _timerAutoGc.Start()
                Else
                    _timerAutoGc = Nothing
                End If
                _VBDiagMarginBase._txtAutoGCSecs.Text = _nAutoGCSecs.ToString
                UpdateStatusMsg("Refresh Rate set to " + _nAutoGCSecs.ToString + " @SeqNo=" + GetGlobalPassCount.ToString("n0"))
            Catch ex As Exception

            End Try
        End Sub

        Sub OnbtnVirtMemClick(ByVal sender As Object, ByVal e As RoutedEventArgs) Handles _btnVirtMem.Click
            If _IsLoadingOfflineSnapshot Then
                MessageBox.Show("Loading snapshot: please wait")
            Else
                Try
                    Dim vm = New VirtualMem
                    vm.ShowVirtualAllocs()

                Catch ex As Exception
                    CommonUI.MemSpectExceptionHandler(ex)
                End Try
            End If
        End Sub

        Private _fChkClrObjRecursive As Boolean = False
        Sub OnchkClrObjects() Handles _chkClrObjects.Checked, _chkClrObjects.Unchecked
            If Not _fChkClrObjRecursive Then
                _fChkClrObjRecursive = True
                Dim newVal = 0
                If _chkClrObjects.IsChecked Then
                    newVal = 1
                End If
                If _ConnectionMode = MemSpectMode.OnLine Then
                    SendMsg(ProcMsgVerb.ClrObjTrk, fSendEndMsgSync:=True, dwords:={newVal}) ' read current setting
                    Dim sResult = Marshal.ReadInt32(_SharedMemAddr)
                    Dim seqno = Marshal.ReadInt32(_SharedMemAddr + 2 * IntPtr.Size)
                    If sResult = 1 Then
                        _chkClrObjects.IsChecked = True
                    Else
                        _chkClrObjects.IsChecked = False
                    End If
                    OnbtnGCClick()

                    UpdateStatusMsg("ChkClrObj " + If(sResult = 1, "On", "Off") + " @SeqNo=" + seqno.ToString("n0"))

                    _fChkClrObjRecursive = False

                End If
            End If
        End Sub

        Sub OnchkFreeze() Handles _chkFreeze.Checked, _chkFreeze.Unchecked
            If _ConnectionMode = MemSpectMode.OnLine Then
                If _chkFreeze.IsChecked Then ' if checked, then freeze
                    '_chkClrObjects.IsEnabled = False ' ensure can't toggle while frozen
                    If Not ProcComm._isTargetFrozen Then ' if not frozen, then we'll freeze
                        'before freezing, refresh heap list so we don't get stale handles
                        RefreshHeapList(fClearFirst:=True)
                        ProcComm.FreezeTarget(fRaiseEvent:=False) ' don't raise event
                        'OnbtnGCClick()
                    End If
                Else
                    If ProcComm._isTargetFrozen Then ' unfreeze
                        If _GhostData.IsTracking Then
                            _GhostData.StopTracking()
                            _GhostData.Clear()
                            _VBDiagMarginBase.MakeCtxMenuMain()
                        End If
                        DataWindowMain.CloseWindows()
                        GCData.ClearData()
                        _MemBasicInfoList.Clear()
                        WorkingSetInfo._WorkingSetDict = Nothing
                        If _ConnectionMode = MemSpectMode.OnLine Then
                            ProcComm.UnFreezeTarget()
                        End If
                    End If
                End If

            End If
        End Sub



        Sub OnMenuButtonClick() Handles _btnMenu.Click
            If _IsLoadingOfflineSnapshot Then
                MessageBox.Show("Loading snapshot: please wait")
            Else
                Me.ContextMenu.IsOpen = True
            End If
        End Sub

        Sub OnbtnGCClick() Handles _btnGC.Click
            Dim nTimes = Math.Max(1, _nGCLoopCount)
            For i = 1 To nTimes
                GC.Collect() ' collect in MemSpect process, not target.
            Next
            CollectGarbageAndGetStats(nTimes)
            GCData.ClearData()
        End Sub

        'Public MustOverride Function GetNorlsAllocStatusReport(ByRef nCnt As Integer, ByRef nTotMem As Integer) As String

        Friend Shared _nIterations As Integer
        Declare Function GetGuiResources Lib "user32" (ByVal hHandle As IntPtr, ByVal uiFlags As Integer) As Integer
        Private Shared _strMemStatHeader As String = ""
        Private Shared _GCFailCount As Integer = 0
        Friend Shared Sub CollectGarbageAndGetStats(ByVal nTimes As Integer, Optional fDoGC As Boolean = True)
            If ProcComm.TargProcHasExited Then
                End
            End If
            If ProcComm._isTargetFrozen Then
                Return ' must not recur while frozen
            End If

            If Common._ConnectionMode <> MemSpectMode.OnLine Then
                Return
            End If
            If Common._IsInMiddleOfMessaging <> ProcMsgVerb.UnKnownVerb Then ' we're in the middle of something like resolve heap names
                Return
            End If
            If _GhostData.IsTracking Then ' when tracking ghosts, don't want to cause deadlock
                Return
            End If
            Try
                If fDoGC AndAlso _VBDiagMarginBase._chkClrObjects.IsChecked Then
                    If _GCFailCount < 2 Then
                        For i = 1 To nTimes
                            If _IsClientOutOfProcess Then
                                SendMsg(ProcMsgVerb.ForceGC, fSendEndMsgSync:=True, dwords:={_nCleanUpRCWs})
                                Dim sResult = Marshal.ReadInt32(_SharedMemAddr) ' reads mem shared by parent/child
                                If sResult <> 0 Then
                                    _GCFailCount += 1
                                    UpdateStatusMsg("GC result = " + sResult.ToString("x8"))
                                    Exit For
                                End If
                            Else
                                Debug.Assert(False, "dead code")
                                'If _nCleanUpRCWs > 0 Then
                                '    Marshal.CleanupUnusedObjectsInCurrentContext()
                                'End If
                                'WaitForOtherThreads(_nGCWaitForOtherThreads)
                                'GC.Collect(GC.MaxGeneration)
                                'GC.WaitForPendingFinalizers()
                                'GC.Collect()
                            End If
                        Next
                    End If
                End If
                Dim devenv = _TargetProc
                Dim sFmt = "{0,5},[{1,14}],{2,10},{3,5},{4,5},{5,5},{6,5}"
                Try

                    Dim nCntNorls = 0 ' 0 is norls, 1 is heap
                    Dim nTotMemNorls = 1 ' 0 means get string report. 1 means just get the totals

                    Dim sOutput As String
                    Dim logToFile = Not String.IsNullOrEmpty(_logFile)
                    Dim logExists = logToFile AndAlso File.Exists(_logFile)

                    Using writer = If(logToFile,
                                      If(logExists, File.AppendText(_logFile), File.CreateText(_logFile)),
                                      Nothing
                                      )
                        _nIterations += 1
                        If _nIterations = 1 OrElse Not logExists Then ' write column headers?
                            _strMemStatHeader = String.Format(sFmt, "Iter", "When", "Priv Mb", "Hndle", "GDI", "User", "Thrds")

                            If nTotMemNorls > 10 Then
                                _strMemStatHeader += ", Norls"
                            End If
                            _strMemStatHeader += ", MaxAllocCtr"
                            If logToFile Then
                                writer.WriteLine(_strMemStatHeader)
                            End If
                        End If

                        sOutput = String.Format(sFmt, _nIterations,
                            DateTime.Now.ToLongTimeString,
                            (devenv.PrivateMemorySize64 / 1000000.0).ToString("f6"),
                            devenv.HandleCount,
                            GetGuiResources(devenv.Handle, 0),
                            GetGuiResources(devenv.Handle, 1),
                            devenv.Threads.Count.ToString
                            )


                        If logToFile Then
                            writer.WriteLine(sOutput)
                            writer.Close()
                        End If
                        If _OutputToDebugWindow > 0 Then
                            Debug.WriteLine(sOutput)
                        End If
                    End Using

                    Dim vmStats = VirtualMem.VMStatistics.GetVirtualMemorystats()
                    Dim thrsh = GetPrivateProfileInt(ProfileStringSection, "VMFreezeThreshold", 0, _iniFileName) * 2 ^ 20
                    If thrsh > 0 Then
                        If vmStats.nTotMEM_COMMIT + vmStats.nTotMEM_RESERVE >= thrsh Then
                            UpdateStatusMsg("Freezing due to thrsh =" + thrsh.ToString("n0") + ". TotCom + Res =  " + (vmStats.nTotMEM_COMMIT + vmStats.nTotMEM_RESERVE).ToString("n0"))
                            VBDiagMarginBase.FreezeTargetThreads()
                        End If
                    End If


                    Dim isRed = False
                    If vmStats.nLargestFree < _LargestFreeVMSizeRed OrElse vmStats.nTotMEM_Free < _AvailVMMemRed Then
                        isRed = True
                    End If

                    Dim MaxSeqNoCurrent = GetGlobalPassCount()
                    Dim DeltaSeqNo = MaxSeqNoCurrent - _PriorMaxSeqNo
                    _PriorMaxSeqNo = MaxSeqNoCurrent

                    Debug.Assert(_AddressOfnCurAllocs <> IntPtr.Zero, "_AddressOfnDeletedAllocs zero?")
                    Dim curAllocCount = GetNumCurAllocs()
                    Dim deltaAllocCount = curAllocCount - _PriorAllocCount
                    _PriorAllocCount = curAllocCount


                    _VBDiagMarginBase._txtmemStat.Text = sOutput
                    _VBDiagMarginBase._txtmemStat.ToolTip = _strMemStatHeader + vbCrLf +
                        sOutput + If(String.IsNullOrEmpty(_logFile), String.Empty, vbCrLf +
                        "  Click to start Excel with log to graph. Logfile: " + _logFile)
                    _VBDiagMarginBase._txtVirtmemStat.Text = vmStats.ToString
                    _VBDiagMarginBase._txtVirtmemStat.ToolTip = vmStats.ToolTip
                    _VBDiagMarginBase._FilterUI.RefreshFilterTextbox()
                    _VBDiagMarginBase._txtMaxSeqNo.Text = String.Format("MaxSeqNo= {0:n0}   ΔAlloc={1:n0}  ΔSeq={2:n0}",
                                                                        MaxSeqNoCurrent,
                                                                        deltaAllocCount,
                                                                        DeltaSeqNo
                                                                        )
                    If _IsClientOutOfProcess Then
                        Dim gcStatData = GCStats.GetGCStats()
                        _VBDiagMarginBase._txtGCStat.Text = gcStatData.ToString
                    Else
                        ' inst._txtNrlsMem.Text = String.Format("NorleaseAllocator Count = {0:n0}, Total={1:n0}", nCntNorls, nTotMemNorls)
                    End If

                    If isRed Then
                        _VBDiagMarginBase._txtVirtmemStat.Background = Brushes.Red
                    Else
                        _VBDiagMarginBase._txtVirtmemStat.Background = Brushes.White
                    End If
#If DEBUG Then
                    If IsUsingStackMemMap Then
                        Dim stats = StackMemMapStats.GetMemMapStats().ToString()
                        UpdateStatusMsg(stats)
                    End If
#End If
                Catch ex As Exception
                    Try
                        If _IsCalvinHBuild Then
                            UpdateStatusMsg(ex.ToString)
                        End If
                        _VBDiagMarginBase._txtmemStat.Text = ex.ToString

                    Catch ex2 As Exception

                    End Try
                End Try

                _VBDiagMarginBase.RefreshHeapList()

            Catch ex As Exception
                ' don't want to show msg box if things like pipe broken due to parent proc dying
                '            MessageBox.Show(ex.Message + " " + If(String.IsNullOrEmpty(ex.StackTrace), "", ex.StackTrace))

                UpdateStatusMsg("Ex while GC: " + ex.ToString)
                'If _windOwner IsNot Nothing Then
                '    _windOwner.Close()
                'End If
            End Try
            If TargProcHasExited() Then
                End
            End If
        End Sub


        Shared Sub OnTimerTick() Handles _timerAutoGc.Tick
            Try

                If _nAutoGCSecs > 0 Then
                    CollectGarbageAndGetStats(1)
                End If
            Catch ex As Exception
                UpdateStatusMsg("Exception collectGarbage " + ex.ToString)
            End Try
        End Sub

        Public Function ShowMemStats() As DataSurface
            Dim ctrls = DataWindowMain.MakeNewDatasurface("MemStats", "Memory Statistics", nMaxHeaderHeight:=40)
            Dim lstMemStats = GetMemStats()
            Dim q = From a In lstMemStats
                    Select
                        a.SeqNo,
                        a.CodeMarkerId,
                        Marker = GetCodeMarkerNameRaw(a.CodeMarkerId),
                        HeapAllocCnt = a.HeapAllocs.Alloc.cnt,
                        HeapAllocSize = a.HeapAllocs.Alloc.size,
                        HeapFreeCnt = a.HeapAllocs.Free.cnt,
                        HeapFreeSize = a.HeapAllocs.Free.size,
                        ClrObjAllocCnt = a.ClrObjs.Alloc.cnt,
                        ClrObjAllocSize = a.ClrObjs.Alloc.size,
                        ClrObjFreeCnt = a.ClrObjs.Free.cnt,
                        ClrObjFreeSize = a.ClrObjs.Free.size,
                        ClrClassAllocCnt = a.ClrClasses.Alloc.cnt,
                        ClrClassAllocSize = a.ClrClasses.Alloc.size,
                        ClrClassFreeCnt = a.ClrClasses.Free.cnt,
                        ClrClassFreeSize = a.ClrClasses.Free.size,
                        ClrOtherAllocCnt = a.ClrOther.Alloc.cnt,
                        ClrOtherAllocSize = a.ClrOther.Alloc.size,
                        ClrOtherFreeCnt = a.ClrOther.Free.cnt,
                        ClrOtherFreeSize = a.ClrOther.Free.size
            'VirtualAllocCnt = a.VirtualAlloc.Alloc.cnt,
            'VirtualAllocSize = a.VirtualAlloc.Alloc.size,
            'VirtualAllocFreeCnt = a.VirtualAlloc.Free.cnt,
            'VirtualAllocFreeSize = a.VirtualAlloc.Free.size

            Dim br = New Browse(q)
            ctrls.SurfaceHeader.Children.Add(New TextBlock With {.Text = "MemStats"})
            ctrls.SurfaceDetails.Children.Add(br)

            Return ctrls
        End Function

        Public Const MemSpectApexXmlFileName As String = "MemSpectApexKnownIssues.xml"
        Public Shared Function ShowKnownIssues() As DataSurface
            Dim ctrls = DataWindowMain.MakeNewDatasurface("KnownIssues", "Known Leak Issues", nMaxHeaderHeight:=40)
            Dim issues = KnownIssues.Known_Issues
            Dim q = From a In issues
                    Select a.IssueName,
                           a.IssueDescription,
                           a.BugId,
                           a.FailTestWhenFound,
                           Created = a.Created.ToString("yyyy-MM-dd"),
                           a.Scenario,
                           a.EstimatedSize,
                           Predicate = "hover to see",
                           _KnownIssue = a


            Dim br = New Browse(q,
                                fAllowBrowFilter:=True,
                                ColWidths:={400, 300, 70, 60, 100}
                                )
            br._BrowseList.AddHandler(MouseMoveEvent,
                                      New RoutedEventHandler(
                                          Sub(o As Object, e As RoutedEventArgs)
                                              Try
                                                  Dim lv = TryCast(o, Browse.BrowseList)
                                                  If lv IsNot Nothing Then
                                                      Dim tb = TryCast(e.OriginalSource, TextBlock)
                                                      If tb IsNot Nothing Then
                                                          If tb.DataContext IsNot Nothing Then
                                                              If tb.Name = "Predicate" Then
                                                                  Dim kntdesc = TypeDescriptor.GetProperties(tb.DataContext)("_KnownIssue")
                                                                  Dim knissue = CType(kntdesc.GetValue(tb.DataContext), KnownIssue)
                                                                  Dim ttipobj = New ToolTip With {
                                                                      .PlacementTarget = tb,
                                                                      .Placement = Primitives.PlacementMode.Bottom
                                                                  }
                                                                  tb.ToolTip = ttipobj
                                                                  ttipobj.Content = New TextBlock With {
                                                                      .Text = knissue.IssueName +
                                                                      vbCrLf +
                                                                      knissue.PredicateString
                                                                  }
                                                                  'ttipobj.IsOpen = True
                                                              End If
                                                          Else
                                                              UpdateStatusMsgDbg("null dc")
                                                          End If
                                                      Else
                                                          UpdateStatusMsgDbg("null tb " + e.OriginalSource.ToString)
                                                      End If
                                                  Else
                                                      UpdateStatusMsgDbg("null lv " + o.GetType.ToString)
                                                  End If
                                              Catch ex As Exception
                                                  UpdateStatusMsgDbg(ex.ToString)
                                              End Try
                                          End Sub
                                        )
                                    )
            ctrls.SurfaceHeader.Children.Add(New Label With {.Content = "Known Leak Issues " + br._BrowseList.Items.Count.ToString})
            ctrls.SurfaceDetails.Children.Add(br)
            Return ctrls
        End Function

    End Class

    Public Class UIBase

        Public Shared Function DoShowThreads() As DataSurface
            Dim thrdlist = GetLiveThreads(fIncludeDeadThreads:=True)

            Dim ctrls = DataWindowMain.MakeNewDatasurface("ThreadInfo", "ThreadInfo", nMaxHeaderHeight:=40)
            ctrls.SurfaceHeader.Children.Add(New Label With {.Content = "Live & dead threads"})

            Dim q = From a In thrdlist.Values
                    Select
                    a.ThreadId,
                    a.SeqNoStart,
                    a.nTotAllocs,
                    a.nTotSize,
                    StackSize = a.MiniDumpThreadInfo.Stack.MemoryLocDesc.DataSize.ToString("x8"),
                    StackMemSize = (a.StackBase.ToInt32 - a.StackLimit.ToInt32).ToString("x8"),
                    StackBase = a.StackBase.ToString("x8"),
                    StackLimit = a.StackLimit.ToString("x8"),
                    ThreadProcAddr = a.GetThreadProcAddress().ToInt32.ToString("x8"),
                    a.ThreadProc,
                    a.Note,
                    _threadinfo = a
                    Order By SeqNoStart

            Dim b = New Browse(q,
                               fAllowBrowFilter:=True,
                               ColWidths:={80, 80, 80, 80, 80, 80, 80, 80, 80},
                               ColTips:={TIP_THREAD,
                                         TIP_SEQNO,
                                         "Total # of live allocations from this thread",
                                         "Total # bytes live allocations from this thread"
                                        },
                               arrColumnsToTotal:={"nTotAllocs", "nTotSize", "StackSize" + HexColumnSpecifier, "StackMemSize" + HexColumnSpecifier})
            ctrls.SurfaceDetails.Children.Add(b)
            Dim miSubSnap = New MenuItem With {.Header = "_SubSnapshot"}
            b._BrowseList.ContextMenu.Items.Insert(0, miSubSnap)
            AddHandler miSubSnap.Click, Sub()
                                            Dim selitem = b._BrowseList.SelectedItem
                                            If selitem IsNot Nothing Then
                                                Dim tinfodesc = ComponentModel.TypeDescriptor.GetProperties(selitem)("_threadinfo")
                                                If tinfodesc IsNot Nothing Then
                                                    Dim tinfo = CType(tinfodesc.GetValue(selitem), ThreadInfo)
                                                    ShowSubSnapShot(tinfo.allocs, "SubSnapShot Thread " + tinfo.ThreadId.ToString)
                                                End If

                                            End If

                                        End Sub

            Return ctrls
        End Function


        Public Shared Function ShowObjectDump() As DataSurface
            Dim display As Boolean = False
            If Common._ConnectionMode = MemSpectMode.Offline And Not Common._offlineSnapshot Is Nothing Then
                display = Common._offlineSnapshot.ContainsClrData
            End If

            If _VBDiagMarginBase._chkClrObjects.IsChecked Or display Then
                VBDiagMarginBase.FreezeTargetThreads()
                Dim refList = GCData.GetCLRObjectRefDict()

                Dim ctrlsGCRoots = DataWindowMain.MakeNewDatasurface("GC_Roots", "CLR Object roots from obj dump", nMaxHeaderHeight:=40)
                Dim qfuncGCRoots = Function(theheapAllocs As List(Of HeapAllocationContainer), thebmem As BrowseMem) As IEnumerable
                                       'theheapallocs is nothing
                                       Dim qGCRoots = From a In GCData.GetGCRootInfo
                                                      Select
                                                           Address = a.GetAddr.ToInt32.ToString("x8"),
                                                           a.AllocationStruct.SeqNo,
                                                           Size = a.GetSize,
                                                           a.AllocationStruct.Thread,
                                                           RootKind = a.GetGCRootKind,
                                                           RootFlags = a.GetGCRootFlags,
                                                           RootId = a.TBlk.Left.ToString("x8"),
                                                           ClassName = a.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False),
                                                           _HeapAllocationContainer = a
                                                      Order By SeqNo

                                       thebmem.InitialSortOrder = New BrowseInitialSortOrder With {.ColumnNo = 2, .direction = ListSortDirection.Ascending}
                                       thebmem._arrColumnsToTotal = {"Size"}
                                       Return qGCRoots
                                   End Function

                ctrlsGCRoots.SurfaceDetails.Children.Add(
                        New BrowseMem(qfuncGCRoots, Nothing, {WIDTH_ADDRESS, WIDTH_SEQNO, 65, 65, 85, 105, 65, 500})
                    )

                Dim ctrls = DataWindowMain.MakeNewDatasurface("ObjDump", "CLR Object Dump", nMaxHeaderHeight:=40)
                Dim qfunc2 = Function(theheapAllocs As List(Of HeapAllocationContainer), thebmem As BrowseMem) As IEnumerable
                                 'theheapallocs is nothing
                                 Dim q = From clrObj In refList.Values
                                         Let hctr = clrObj.hctnr
                                         Select
                                             Address = hctr.TBlk.Address.ToString("x8"),
                                             hctr.AllocationStruct.SeqNo,
                                             hctr.TBlk.Size,
                                             hctr.AllocationStruct.Thread,
                                             Gen = hctr.GetGen,
                                             MovedCnt = hctr.GetMovedCnt,
                                             SurvivedCnt = hctr.GetSurvivedCnt,
                                             NumRefsFromMe = clrObj.Refs.Count,
                                             Classid = hctr.TBlk.UnionData1.ToString("x8"),
                                             ClassName = hctr.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True),
                                             _HeapAllocationContainer = hctr,
                                             _ClrObjDump = clrObj
                                         Order By SeqNo

                                 thebmem.InitialSortOrder = New BrowseInitialSortOrder With {.ColumnNo = 2, .direction = ListSortDirection.Ascending}
                                 thebmem.ColumnTips = {TIP_ADDRESS, TIP_SEQNO, TIP_SIZE, TIP_THREAD,
                                                     "Gen3 = large heap",
                                                     "# times moved in GC",
                                                     "# times survived GC",
                                                     "# references this object has to other objects",
                                                     ""
                                                    }
                                 thebmem._arrColumnsToTotal = {"Size", "MovedCnt", "SurvivedCnt", "NumRefsFromMe"}

                                 Return q
                             End Function
                Dim ColWidths = {WIDTH_ADDRESS, WIDTH_SEQNO, 60, 60, 60, 60, 60, 60, 65, 500}
                Dim bmem = New BrowseMem(qfunc2, Nothing, ColWidths:=ColWidths,
                                         fAllowBrowStringFilter:=True, tblkType:=TrkType.ClrObjects)
                ctrls.SurfaceDetails.Children.Add(CType(bmem, UIElement))
                Return ctrls
            Else
                MessageBox.Show("Need to turn on CLR Object tracking option")
            End If
            Return Nothing
        End Function

        Private Shared _SeqNoAtLastSymResolve As UInteger

        Public Shared Function ShowSymbolDictionary() As DataSurface
            VBDiagMarginBase.FreezeTargetThreads()
            Dim ctrls = DataWindowMain.MakeNewDatasurface("SymbolDictionary", "SymbolDictionary", nMaxHeaderHeight:=45)
            Dim btnBulkRead As New Button With {.Content = "_BulkReadStackIndex", .ToolTip = "(Only for 4gigstack option) read the index to address table from target proc" +
                vbCrLf + "the Stackindexdict (which never shrinks) can be larger than all symbols in current allocations, because of Frees"
                }
            Dim btnResolveSym As New Button With {.Content = "_ResolveSymbols", .ToolTip = "resolve all unresolved symbols. For 32bit stacks, will walk all recorded callstacks"}

            Dim btnLookup As New Button With {.Content = "_Lookup", .ToolTip = "from a set of hex index values, one per line (starting with 0x), resolve the symbols"}

            Dim txtLookup As New TextBox With {.ToolTip = "paste values in here for lookup", .AcceptsReturn = True, .VerticalScrollBarVisibility = ScrollBarVisibility.Auto}
            If Not _fHandle4gigStacks Then
                btnBulkRead.IsEnabled = False
            End If
            Dim lamRefresh = Sub()
                                 ctrls.SurfaceHeader.Children.Clear()
                                 ctrls.SurfaceDetails.Children.Clear()
                                 Dim q As IEnumerable
                                 Dim ColWidths = {70, 70, 800}
                                 Dim sExtra = ""
                                 If _fHandle4gigStacks Then
                                     q = From lk In Common._stackIndexDict
                                         Group Join a In StackFrameDictionary On lk.Key Equals a.Key Into g = Group
                                         From sym In g.DefaultIfEmpty
                                         Select StackIndex = lk.Key.ToInt32.ToString("x8"),
                                         Addr = lk.Value.ToInt32.ToString("x8"),
                                         Name = SymbolStripFileName(sym.Value),
                                         Symbol = sym.Value

                                     sExtra += String.Format("    # of items in Index = {0:n0}  # in SymDict = {1:n0}", _stackIndexDict.Count, StackFrameDictionary.Count)

                                 Else
                                     q = From a In StackFrameDictionary
                                         Select Addr = a.Key.ToInt32.ToString("x8"),
                                           Name = SymbolStripFileName(a.Value),
                                           Symbol = a.Value

                                     ColWidths = {70, 800, 600}
                                 End If
                                 Dim nItems = Aggregate item In q Into Count()
                                 Dim tb As New TextBlock With {
                                     .Text = "# items = " + nItems.ToString("n0") + sExtra,
                                     .ToolTip = "As more snapshots are taken, more symbols need to be resolved"
                                 }
                                 Dim b = New Browse(q, ColWidths:=ColWidths, fAllowBrowFilter:=True)
                                 ctrls.SurfaceHeader.Children.Add(btnBulkRead)
                                 ctrls.SurfaceHeader.Children.Add(btnResolveSym)
                                 ctrls.SurfaceHeader.Children.Add(btnLookup)
                                 ctrls.SurfaceHeader.Children.Add(txtLookup)
                                 ctrls.SurfaceHeader.Children.Add(tb)
                                 ctrls.SurfaceDetails.Children.Add(b)

                                 b._BrowseList.ContextMenu.AddMnuItem("_View Source Code",
                                                                      "Open source code",
                                                                      Sub()
                                                                          Dim itmcnt = b._BrowseList.SelectedItems.Count
                                                                          If itmcnt = 1 Then
                                                                              Dim selitem = b._BrowseList.SelectedItems(0)
                                                                              Dim symtdesc = ComponentModel.TypeDescriptor.GetProperties(selitem)("Symbol")
                                                                              Dim sym = CStr(symtdesc.GetValue(selitem))
                                                                              ViewSourceCodeFile(sym)
                                                                          End If
                                                                      End Sub, InsertPos:=0)

                             End Sub
            AddHandler btnBulkRead.Click, Sub()
                                              BulkReadStackIndexes()
                                              lamRefresh.Invoke()
                                          End Sub
            AddHandler btnResolveSym.Click, Sub()
                                                Using New DataWindowMain.SetCursorWait
                                                    If _fHandle4gigStacks Then
                                                        For Each ndx In _stackIndexDict.Keys
                                                            If Not StackFrameDictionary.ContainsKey(ndx) Then
                                                                ResolveAddressToSymbol(ndx)
                                                            End If
                                                        Next
                                                    Else
                                                        For Each hp In _HeapList
                                                            Dim snap = hp.TakeMemSnapshot
                                                            For Each alloc In snap.Allocs
                                                                If alloc.AllocationStruct.SeqNo >= _SeqNoAtLastSymResolve Then
                                                                    Dim z = alloc.GetCallStackAsStringArray
                                                                End If
                                                            Next
                                                        Next
                                                    End If
                                                    _SeqNoAtLastSymResolve = GetGlobalPassCount()
                                                    lamRefresh.Invoke()
                                                End Using
                                            End Sub
            AddHandler btnLookup.Click, Sub()
                                            Dim sb As New Text.StringBuilder
                                            Try
                                                For Each strLine In txtLookup.Text.Split(CChar(vbCrLf))
                                                    strLine = Trim(strLine).Replace(vbLf, "").Replace(vbCr, "")
                                                    If strLine.StartsWith("0x") Then
                                                        strLine = strLine.Substring(2)
                                                        If strLine.Length > 3 Then
                                                            Dim val = Int32.Parse(strLine, Globalization.NumberStyles.AllowHexSpecifier)
                                                            Dim str = ResolveAddressToSymbol(New IntPtr(val))
                                                            sb.AppendLine(str)
                                                        End If
                                                    End If
                                                Next
                                            Catch ex As Exception
                                                CommonUI.MemSpectExceptionHandler(ex)
                                            End Try
                                            txtLookup.Text = sb.ToString
                                        End Sub



            lamRefresh.Invoke()
            Return ctrls
        End Function

        Public Shared Function ShowAssertLists() As DataSurface
            VBDiagMarginBase.FreezeTargetThreads()
            Dim ctrls = DataWindowMain.MakeNewDatasurface("AssertList", "Seqno and Stackframes on which to Assert", nMaxHeaderHeight:=50)
            SendMsg(ProcMsgVerb.AssertSeqNo, fSendEndMsgSync:=False, dwords:={2}) ' 2 return all current
            Dim res = GetMsg(4)
            Dim nItems = BitConverter.ToInt32(res, 0)
            Dim listSeqNos As New List(Of Integer)
            For i = 0 To nItems - 1
                res = GetMsg(4)
                listSeqNos.Add(BitConverter.ToInt32(res, 0))
            Next
            EndMsgSync()
            Dim q = From a In listSeqNos
                    Select SeqNo = a,
                    _needMorethan1col = a
                    Order By SeqNo

            Dim brow = New Browse(q)

            SendMsg(ProcMsgVerb.AssertStackFrame, fSendEndMsgSync:=False, dwords:={2}) ' 2 returns all
            res = GetMsg(4)
            nItems = BitConverter.ToInt32(res, 0)
            Dim listFrames As New Dictionary(Of IntPtr, String)
            For i = 0 To nItems - 1
                res = GetMsg(4)
                Dim addr = IntPtr.op_Explicit(BitConverter.ToInt32(res, 0))
                Dim frame = ResolveAddressToSymbol(addr)
                listFrames.Add(addr, frame)
            Next
            EndMsgSync()
            Dim q2 = From fr In listFrames
                     Select Address = fr.Key.ToInt32.ToString("x8"),
                        Frame = fr.Value
                     Order By Address

            Dim brow2 = New Browse(q2, ColWidths:={WIDTH_ADDRESS, 800})
            Dim sp As New StackPanel With {.Orientation = Orientation.Horizontal}
            sp.Children.Add(brow)
            sp.Children.Add(brow2)
            ctrls.SurfaceDetails.Children.Add(sp)

            Return ctrls
        End Function

        Public Shared Function DoShowGhostAllocs() As DataSurface
            Dim ctrls = DataWindowMain.MakeNewDatasurface("Ghosts", "Ghost allocations (could have been freed)", nMaxHeaderHeight:=50)
            Dim maxSeqno = GetGlobalPassCount()
            Dim btnMissingSeqNos = New Button With {
            .Content = "Missing SeqNos",
            .ToolTip = "Show missing seqnos in notepad",
            .Width = 100,
            .Height = 40
            }
            ctrls.SurfaceHeader.Children.Add(btnMissingSeqNos)
            AddHandler btnMissingSeqNos.Click, Sub()
                                                   Dim setSeqNos As New SortedSet(Of UInteger)
                                                   For Each GhostAlloc In _GhostData.GhostList.Values
                                                       Dim seqNo = GhostAlloc.hctr.AllocationStruct.SeqNo
                                                       setSeqNos.Add(seqNo)
                                                   Next
                                                   For Each hp In _HeapList
                                                       For Each alloc In hp.TakeMemSnapshot(fEnableFilter:=False).Allocs
                                                           Dim seqNo = alloc.AllocationStruct.SeqNo
                                                           setSeqNos.Add(seqNo)
                                                       Next
                                                   Next
                                                   Dim missing As New List(Of KeyValuePair(Of UInteger, Integer))

                                                   Dim numMissingInThisRun = 0
                                                   Dim missingSeqno As UInteger = 0
                                                   For i As UInteger = 1 To maxSeqno
                                                       If setSeqNos.Contains(i) Then
                                                           If numMissingInThisRun <> 0 Then
                                                               missing.Add(New KeyValuePair(Of UInteger, Integer)(missingSeqno, numMissingInThisRun))
                                                           End If
                                                           numMissingInThisRun = 0
                                                       Else
                                                           If numMissingInThisRun = 0 Then ' first one in this seriese?
                                                               missingSeqno = i
                                                           End If
                                                           numMissingInThisRun += 1
                                                       End If
                                                   Next
                                                   Dim ctrlsMissing = DataWindowMain.MakeNewDatasurface("MissingSeqNos", "Seq Nos that are not found in all ghosts and all heaps", nMaxHeaderHeight:=50)
                                                   Dim qMissing = From a In missing
                                                                  Select
                                                                     SeqNo = a.Key,
                                                                     NumMissing = a.Value

                                                   Dim brMissing = New Browse(qMissing,
                                                                              arrColumnsToTotal:={"NumMissing"},
                                                                              fAllowBrowFilter:=True,
                                                                              ColTips:={"Missing SeqNo", "# of seqnos missing starting from the Missing Seqno"})
                                                   ctrlsMissing.SurfaceDetails.Children.Add(brMissing)
                                               End Sub
            ctrls.SurfaceHeader.Children.Add(
                New TextBlock With {
                    .Text = _GhostData.ToString() + " MaxSeqNo = " + maxSeqno.ToString("n0")
                })

            Dim qfunc = New BrowQueryDelegate(
                        Function(lst As List(Of HeapAllocationContainer), b As BrowseMem) As IEnumerable
                            Dim q = From alloc In _GhostData.GhostList.Values
                                    Select
                                    Address = alloc.hctr.AllocationStruct.Address.ToString("x8"),
                                    SeqNo = alloc.hctr.AllocationStruct.SeqNo,
                                    Size = alloc.hctr.GetSize,
                                    Thread = alloc.hctr.AllocationStruct.Thread,
                                    WhenFreed = alloc.SeqNoWhenFreed,
                                    Heap = alloc.Heap.GetHeapName,
                                    Data = alloc.hctr.GetDisplayData,
                                    _HeapAllocationContainer = alloc.hctr

                            b._ColWidths = {WIDTH_ADDRESS, WIDTH_SEQNO, 65, 65, 65, 200, 600}
                            Return q
                        End Function
            )

            Dim lstHctr = From a In _GhostData.GhostList.Values
                          Select a.hctr

            Dim bmem = New BrowseMem(qfunc, lstHctr.ToList, fAllowBrowStringFilter:=True)

            ctrls.SurfaceDetails.Children.Add(CType(bmem, UIElement))
            Return ctrls

        End Function

    End Class
End Namespace
