Imports MemSpect
Imports System.Windows.Controls
Imports MemSpect.SnapDiffer

<TestClass>
<CLSCompliant(False)>
Public Class TestSnapCompare
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

    <TestMethod(),
    Description("Load 2 snaps")>
    <Ignore>
    Public Sub CompSnaps()
        Try
            InitTestMethodAndSetBaseLine()
            _VBAssert.OutputText("Comparing Snapshots!!")
            _VBAssert.OutputText(GetCodeMarkerIdFromName("perfVSLoadUIBegin").ToString() + " GetCodeMarkerIdFromName lookup")
            Dim snapNames = {
                "c:\MemSpect\Snaps\MarkerSnap_perfVSCloseSolutionBegin__VSConsumption_0_1",
                "c:\MemSpect\Snaps\MarkerSnap_perfVSCloseSolutionBegin__VSConsumption_0_2"
            }
            'snapNames = {
            '    "\\VSPERFRELFUN2\MemSpectApex\calvinh\MarkerSnap_perfVSCloseSolutionBegin__VSConsumption_0_20",
            '    "\\VSPERFRELFUN2\MemSpectApex\calvinh\MarkerSnap_perfVSCloseSolutionBegin__VSConsumption_0_21"
            '}
            snapNames = {
                "c:\MemSpect\Snaps\CSLife1",
                "c:\MemSpect\Snaps\CSLife2"
            }
            LoadOffLineSnap(snapNames(0), fWaitTilClrLoadDone:=True)
            Dim snapnum = LoadTheDiff(snapNames(1))
            Dim snapHelper = MemNodeDiff._SnapShotHelperDict(snapnum)
            Dim opts = OptionsForDiff.ProcessHeap
            snapHelper.FillData(opts)
            Dim res = snapHelper.AddTheKids(
                {MemNodeDiff._SnapShotHelperDict(0)._lstAllocs,
                 MemNodeDiff._SnapShotHelperDict(snapnum)._lstAllocs},
             nLevel:=0)

            _VBAssert.OutputText(String.Format("Cnt={0} {1}   Size = {2} {3}",
                                               res._arrLstAllocs(0).Count,
                                               res._arrLstAllocs(1).Count,
                                               res._totSize(0),
                                               res._totSize(1)))
            For snapnum = 0 To 1
                _VBAssert.OutputText("OffSnap " + snapnum.ToString)
                For Each alloc In res._arrLstAllocs(snapnum).OrderByDescending(Function(h) h.hctr.GetSize)

                    _VBAssert.OutputText(String.Format("{0}, {1}",
                                                       snapnum,
                                                       alloc.ToString()
                                                       ))
                    CleanStacksForBaseline("stack sss", alloc.hctr.GetCallStackAsStringArray)

                Next
            Next

        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub



    <TestMethod(),
    Description("Load 2 snaps")>
    <Ignore>
    Public Sub CompSnapUI()
        Try
            InitTestMethodAndSetBaseLine()
            _VBAssert.OutputText("Comparing Snapshots")

            Dim snapNames = {
                "c:\MemSpect\Snaps\MarkerSnap_perfVSCloseSolutionBegin__VSConsumption_0_1",
                "c:\MemSpect\Snaps\MarkerSnap_perfVSCloseSolutionBegin__VSConsumption_0_2"
            }
            'snapNames = {
            '    "\\VSPERFRELFUN2\MemSpectApex\calvinh\MarkerSnap_perfVSCloseSolutionBegin__VSConsumption_0_20",
            '    "\\VSPERFRELFUN2\MemSpectApex\calvinh\MarkerSnap_perfVSCloseSolutionBegin__VSConsumption_0_21"
            '}
            snapNames = {
                "c:\MemSpect\Snaps\CSLife1",
                "c:\MemSpect\Snaps\CSLife2"
            }
            LoadOffLineSnap(snapNames(0), fWaitTilClrLoadDone:=True)


            Dim ctrls = SnapDifferUI.ShowSnapDiff(snapNames(1))
            Dim diffp = CType(ctrls.SurfaceDetails.Children(0), SnapDifferUI.DiffPanel)
            Dim tv = CType(diffp.Children(0), SnapDifferUI.SnapDiffTreeView)

            Dim lamRecur As Action(Of ItemsControl, Integer)
            lamRecur = Sub(listItems, depth)

                           For Each itm As SnapDifferUI.SnapDiffTVItem In listItems.Items
                               _VBAssert.OutputText(
                                   String.Format("{0} {1} Cnt={2} {3} {4}",
                                   New String(" "c, depth),
                                   If(itm.IsExpanded, "+", " "),
                                   If(itm._fIsDummy, 0, itm._mnode._lstAllocs.Count),
                                   If(itm.HasDummyChild, "HasDummyChild", String.Empty),
                                   itm.ToString
                                   ))
                               If itm.HasDummyChild Then
                                   itm.IsExpanded = True
                               End If
                               For Each child As SnapDifferUI.SnapDiffTVItem In listItems.Items
                                   If Not child._fIsDummy AndAlso child._mnode._cntArr(0) + child._mnode._cntArr(1) > 1 Then
                                       lamRecur(child, depth + 1)

                                   End If
                               Next
                           Next

                       End Sub
            lamRecur(tv, 0)

        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub






    '<TestMethod(),
    'Description("Load 2 snaps")>
    '<Ignore>
    'Public Sub CompSnapsOld()
    '    Try
    '        InitTestMethodAndSetBaseLine()
    '        _VBAssert.OutputText("Comparing Snapshots")

    '        Dim lam = Sub(snap As offlineSnapHelper, allocType As BlockTypes)
    '                      Common._offlineSnapshot = snap._offlineSnap
    '                      _VBAssert.OutputText(snap._snapPath)

    '                      Dim heapName = String.Empty

    '                      Select Case allocType
    '                          Case BlockTypes.None
    '                              heapName = Common.ProcessHeapName
    '                          Case Else
    '                              heapName = Common.MemSpectHeapName
    '                      End Select
    '                      Dim heapSnapshot = snap.GetHeapSnapshot(heapName)
    '                      Dim allocs = heapSnapshot.
    '                          Allocs.
    '                          Where(Function(h) h.TBlkBlockType = allocType).
    '                          OrderBy(Function(h) h.AllocationStruct.SeqNo).Take(10000)

    '                      For Each alloc In allocs
    '                          snap._lstAllocs.Add(New AllocForDiff(snap._offlineSnap, alloc))
    '                          '_VBAssert.OutputText(alloc.ToString)
    '                          '                                      _VBAssert.OutputText("# " + snap._lstAllocs.Count.ToString)
    '                          'Dim txt = CleanLineForBaseline(alloc.ToString)
    '                          'txt = Text.RegularExpressions.Regex.Replace(txt, "Thread\s*=\s*[0-9]+", "Thread=0000")
    '                          '_VBAssert.OutputText(txt, fAddToBaseline:=False)
    '                          'CleanStacksForBaseline("stack sss", alloc.GetCallStackAsStringArray)

    '                          '                              _VBAssert.OutputText(alloc.GetCallStackAsString)

    '                      Next
    '                  End Sub

    '        Dim recurLam As Func(Of HeapAllocationContainer, Boolean) ' recursive lambda
    '        Dim nlev = 0
    '        recurLam = Function(x As HeapAllocationContainer) As Boolean
    '                       If nlev < 10 Then
    '                           nlev += 1
    '                           recurLam(x)
    '                       Else
    '                           _VBAssert.OutputText("got 10")
    '                       End If
    '                       Return True
    '                   End Function

    '        recurLam(Nothing)


    '        Dim snap1 = New offlineSnapHelper("\\VSPERFRELFUN2\MemSpectApex\calvinh\MarkerSnap_perfVSCloseSolutionBegin__VSConsumption_0_1")
    '        lam(snap1, BlockTypes.None)
    '        '            Return

    '        Dim snap2 = New offlineSnapHelper("\\VSPERFRELFUN2\MemSpectApex\calvinh\MarkerSnap_perfVSCloseSolutionBegin__VSConsumption_0_2")
    '        lam(snap2, BlockTypes.None)

    '        Dim max = Math.Max(snap1._lstAllocs.Count, snap2._lstAllocs.Count)
    '        _VBAssert.OutputText("Comparing " + max.ToString)
    '        Dim diffList = New List(Of Tuple(Of HeapAllocationContainer, HeapAllocationContainer))
    '        For i = 0 To max - 1
    '            Dim alloc0 As AllocForDiff = Nothing
    '            Dim alloc1 As AllocForDiff = Nothing

    '            If i < snap1._lstAllocs.Count Then
    '                alloc0 = snap1._lstAllocs(i)
    '            End If
    '            If i < snap2._lstAllocs.Count Then
    '                alloc1 = snap2._lstAllocs(i)
    '            End If
    '            Dim fdiff = False
    '            If alloc0 IsNot Nothing AndAlso alloc1 IsNot Nothing Then
    '                Common._offlineSnapshot = snap1._offlineSnap
    '                Dim stk1 = alloc0.hctr.GetCallStackAsString
    '                Common._offlineSnapshot = snap2._offlineSnap
    '                Dim stk2 = alloc1.hctr.GetCallStackAsString
    '                stk1 = Text.RegularExpressions.Regex.Replace(stk1, "!0x[0-9A-Fa-f]{8}", "0x00000000")
    '                stk2 = Text.RegularExpressions.Regex.Replace(stk2, "!0x[0-9A-Fa-f]{8}", "0x00000000")
    '                If stk1 <> stk2 Then
    '                    fdiff = True
    '                Else
    '                    If alloc0.hctr.GetSize <> alloc1.hctr.GetSize Then
    '                        fdiff = True
    '                    End If
    '                End If
    '            End If
    '            If fdiff Then
    '                diffList.Add(New Tuple(Of HeapAllocationContainer, HeapAllocationContainer)(alloc0.hctr, alloc1.hctr))
    '            End If



    '        Next
    '        _VBAssert.OutputText("# diffs = " + diffList.Count.ToString)
    '        For Each diff In diffList
    '            _VBAssert.OutputText(diff.Item1.ToString)
    '            _VBAssert.OutputText(diff.Item2.ToString)
    '            _VBAssert.OutputText(String.Empty)

    '        Next
    '        '_VBAssert.OutputText("Now for 2")
    '        'Dim snap2 = New offlineSnap("\\VSPERFRELFUN2\MemSpectApex\calvinh\MarkerSnap_perfVSCloseSolutionBegin__VSConsumption_0_2", _VBAssert)
    '        'lam(snap2)




    '    Catch ex As Exception
    '        HandleTestException(ex)
    '    End Try
    'End Sub


End Class
