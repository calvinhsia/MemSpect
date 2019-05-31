Imports MemSpect

<TestClass()>
<CLSCompliant(False)>
Public Class LeakTests
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

    Sub theHandler(ByVal o As Object, ByVal e As StatusMessageEventArgs)
        _VBAssert.OutputText(e.Message)
    End Sub
    Sub addTheHandler()
        AddHandler Common.StatusMessageEvent, AddressOf theHandler
    End Sub
    Sub removeTheHandler()
        RemoveHandler Common.StatusMessageEvent, AddressOf theHandler

    End Sub



    <TestMethod()>
    <Ignore>
    Sub CheckGCRoots()
        InitTestMethodAndSetBaseLine()
        Try


            Dim SnapName = IO.Path.Combine(memspectInstallDir, "Snaps\NavToRoslyn_7")
            SnapName = IO.Path.Combine(memspectInstallDir, "Snaps\NavTo_7")
            'SnapName = IO.Path.Combine(memspectInstallDir, "Snaps\cslife")
            UpdateStatusMsg("Loading " + SnapName)
            LoadOffLineSnap(SnapName, fWaitTilClrLoadDone:=True)

            '            Dim allocs = _HeapList.Where(Function(hp) hp.GetHeapName = MemSpectHeapName).Single.TakeMemSnapshot(fEnableFilter:=False).Allocs.Where(Function(h) h.TblkBlockType = BlockTypes.ClrObject)
            Dim allocs = _HeapList.Where(Function(hp) hp.GetHeapName = MemSpectHeapName).
                Single.
                GetLeaksFromLeakMultipleToList(7).
                Where(Function(h) h.TBlkBlockType = BlockTypes.ClrObject)

            For Each alloc In allocs
                If alloc.GetAddr.ToInt32 = &H33F19908 Then
                    Dim nPath = 0
                    Dim gcpathLists = alloc.GetGCRootPaths(Sub(str)
                                                               _VBAssert.OutputText(str)
                                                           End Sub) ' lk.GetObjectRefData(NodeType.PathFromGCRoot) '.Where(Function(lst) Not lst(0).GetGCRootExtraInfo.Contains("WeakRef"))

                    For Each gcpathList In gcpathLists
                        _VBAssert.OutputText(String.Format("PathFromGCRoot {0} PathLen ={1} {2}", nPath, gcpathList.Count, alloc.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True)))
                        nPath += 1
                        For Each refobj In gcpathList
                            _VBAssert.OutputText(String.Format("  {0} {1}", refobj.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True), refobj.GetGCRootExtraInfo))
                        Next

                    Next

                End If



            Next
            'Dim lk = lks.Where(
            '    Function(l) l.TblkBlockType = BlockTypes.ClrObject AndAlso
            '        l.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False) = "Microsoft.VisualStudio.PlatformUI.NavigateToWindow").
            '    Skip(1).
            '    Take(1).Single
            '_VBAssert.OutputText(lk.ToString + lk.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False))

            'Dim gcpathLists = GetGCRootPaths(lk, Sub(str)
            '                                         ' _VBAssert.OutputText(str)
            '                                     End Sub) ' lk.GetObjectRefData(NodeType.PathFromGCRoot) '.Where(Function(lst) Not lst(0).GetGCRootExtraInfo.Contains("WeakRef"))
            'Dim nPath = 0
            'For Each gcpathList In gcpathLists
            '    _VBAssert.OutputText(String.Format("PathFromGCRoot {0} PathLen ={1}", nPath, gcpathList.Count))
            '    For Each refobj In gcpathList
            '        _VBAssert.OutputText(String.Format("  {0} {1}", refobj.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False), refobj.GetGCRootExtraInfo))
            '    Next

            'Next


        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub


    <TestMethod()>
    <Ignore>
    Sub TestTest()
        InitTestMethodAndSetBaseLine()
        Try
            addTheHandler()
            UpdateStatusMsg("1 one")
            '        addh()
            UpdateStatusMsg("1 two")

        Catch ex As Exception
            HandleTestException(ex)
        End Try
        removeTheHandler()
        removeTheHandler()
        removeTheHandler()
    End Sub
    <TestMethod()>
    <Ignore>
    Sub TestTest2()
        InitTestMethodAndSetBaseLine()
        Try
            addTheHandler()
            UpdateStatusMsg("2 one")
            '       addh()
            UpdateStatusMsg("2 two")

        Catch ex As Exception
            HandleTestException(ex)
        End Try
        removeTheHandler()
        removeTheHandler()
        removeTheHandler()
    End Sub



    <TestMethod()>
    <Ignore>
    Sub NavToObjRef()
        InitTestMethodAndSetBaseLine()
        Try

            AddHandler Common.StatusMessageEvent, Sub(o, e)
                                                      _VBAssert.OutputText(e.Message)
                                                  End Sub
            Dim SnapName = IO.Path.Combine(memspectInstallDir, "Snaps\NavToRoslyn_7")
            SnapName = IO.Path.Combine(memspectInstallDir, "Snaps\NavTo_7")
            LoadOffLineSnap(SnapName, fWaitTilClrLoadDone:=True)
            _GlobalFilter.LeakMultiple = 7
            _VBAssert.OutputText(
                String.Format(
                    "Looking for leaks {0} {1:n0} - {2:n0}",
                    CleanLineForBaseline(SnapName),
                    _GlobalFilter.SeqNoLo,
                    _GlobalFilter.SeqNoHi
                    )
                )

            Dim lks = _HeapList.Where(Function(hp) hp.GetHeapName = MemSpectHeapName).Single.GetLeaksFromLeakMultipleToList(7)
            Dim lk = lks.Where(
                Function(l) l.TBlkBlockType = BlockTypes.ClrObject AndAlso
                    l.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False) = "Microsoft.VisualStudio.PlatformUI.NavigateToWindow").
                Take(1).Single
            _VBAssert.OutputText(lk.ToString + lk.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False))

            Dim gcpathLists = lk.GetGCRootPaths(Sub(str)
                                                    _VBAssert.OutputText(str)
                                                End Sub) ' lk.GetObjectRefData(NodeType.PathFromGCRoot) '.Where(Function(lst) Not lst(0).GetGCRootExtraInfo.Contains("WeakRef"))
            'Dim gcpathLists = GetGCRootPaths(lk, Sub(str)
            '                                         _VBAssert.OutputText(str)
            '                                     End Sub) ' lk.GetObjectRefData(NodeType.PathFromGCRoot) '.Where(Function(lst) Not lst(0).GetGCRootExtraInfo.Contains("WeakRef"))
            Dim nPath = 0
            For Each gcpathList In gcpathLists
                _VBAssert.OutputText(String.Format("PathFromGCRoot {0} PathLen ={1}", nPath, gcpathList.Count))
                For Each refobj In gcpathList
                    _VBAssert.OutputText(String.Format("  {0:x8} {1} {2}", refobj.GetAddr.ToInt32, refobj.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False), refobj.GetGCRootExtraInfo))
                Next

            Next


        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub

    <TestMethod()>
    <Ignore>
    Sub Layout()
        InitTestMethodAndSetBaseLine()
        Try
#If False Then
Address    SeqNo       Size     Thread   Gen      Moved    Srviv    SyncBlk  Classid    ClassName                                                                                                                              NumItems    NearestCodeMarker
047ba018   9643796     36       4828     2        2        4        00000000 5f6c02d0   Microsoft.VisualStudio.CSharp.Services.Language.Features.Outlining.CSharpHiddenRegionTag                                               0           
047ba03c   9643815     36       4828     2        2        4        00000000 5f6c02d0   Microsoft.VisualStudio.CSharp.Services.Language.Features.Outlining.CSharpHiddenRegionTag                                               0           
047ba060   9643821     36       4828     2        2        4        00000000 5f6c02d0   Microsoft.VisualStudio.CSharp.Services.Language.Features.Outlining.CSharpHiddenRegionTag                                               0           
047ba084   9643827     36       4828     2        2        4        00000000 5f6c02d0   Microsoft.VisualStudio.CSharp.Services.Language.Features.Outlining.CSharpHiddenRegionTag                                               0           

#End If

            Dim SnapName = IO.Path.Combine(memspectInstallDir, "Snaps\VSCSld7")
            UpdateStatusMsg("Loading " + SnapName)
            LoadOffLineSnap(SnapName, fWaitTilClrLoadDone:=True)
            Dim alloc = _HeapList.Where(Function(hp) hp.GetHeapName = MemSpectHeapName).
                Single.
                TakeMemSnapshot(fEnableFilter:=False).
                Allocs.
                Where(Function(h) h.TBlkBlockType = BlockTypes.ClrObject AndAlso
                          h.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False) = "Microsoft.VisualStudio.CSharp.Services.Language.Features.Outlining.CSharpHiddenRegionTag"
                          ).
                FirstOrDefault
            Dim layout = alloc.GetClrClassMemberLayout()
            _VBAssert.OutputText(layout, cSplitChar:=CChar(vbCr))
            Dim dmp = GetMemoryDump(alloc.GetAddr().MyAdd(-4), alloc.GetSize())
            _VBAssert.OutputText(dmp, cSplitChar:=CChar(vbCr))
        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub

    <TestMethod()>
    Sub LeakMultiple()
        InitTestMethodAndSetBaseLine()
        Try
            Dim lstLeaks As New List(Of HeapAllocationContainer)

            '4169690,4905057 
            '4905057,5640424 
            '5640424,6375791 
            '6375791,7111158 
            '7111158,7846525 
            '7846525,8581892 
            '8581892,9317259 

            Dim SnapName = IO.Path.Combine(memspectInstallDir, "Snaps\VSCSld7") ' System.Drawing.dll!System.Drawing.Internal.SystemColorTracker.Add
            'Dim SnapName = IO.Path.Combine(memspectInstallDir, "Snaps\VSVBBld7") ' System.Drawing.dll!System.Drawing.Internal.SystemColorTracker.Add
            LoadOffLineSnap(SnapName, fWaitTilClrLoadDone:=True, fClearFilterToo:=False)
            _VBAssert.OutputText(
                String.Format(
                    "Looking for leaks {0} {1:n0} - {2:n0}",
                    CleanLineForBaseline(SnapName).ToLower,
                    _GlobalFilter.SeqNoLo,
                    _GlobalFilter.SeqNoHi
                    )
                )

            _VBAssert.OutputText("now test range feature")
            Dim seqnoList(7 - 1, 2 - 1) As UInteger
            Dim seqnoStart = _GlobalFilter.SeqNoLo
            Dim seqnoEnd = _GlobalFilter.SeqNoHi
            Dim nDelta = CUInt((seqnoEnd - seqnoStart) / 7)
            _VBAssert.OutputText(String.Format("Range feature Start={0} End={1}  Delta= {2}", seqnoStart, seqnoEnd, nDelta))
            For i = 0 To 7 - 1
                'Dim bucketNo As Integer = nMult * (alloc.AllocationStruct.SeqNo - _GlobalFilter.SeqNoLo) \ (nDelta)

                Dim iterstart = seqnoStart + CUInt(i * nDelta)
                seqnoList(i, 0) = iterstart
                seqnoList(i, 1) = iterstart + nDelta
                _VBAssert.OutputText(String.Format("Range feature Iter={0} Start = {1} End={2} ", i, iterstart, iterstart + nDelta))
            Next
            For Each hp In _HeapList
                Dim lst = hp.GetLeaksFromLeakMultipleToList(7, seqnoList)
                If lst.Count > 0 Then
                    lstLeaks.AddRange(lst)
                End If
            Next
            Dim nItem = 0
            For Each itm In lstLeaks.OrderBy(Function(a) a.AllocationStruct.SeqNo)
                nItem += 1
                _VBAssert.OutputText(String.Format(" itm # {0} {1} {2} {3}", nItem, itm.ToString, itm.SpyHeapPtr.HeapName, itm.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True)))
            Next


            lstLeaks.Clear()

            _VBAssert.OutputText("now test non-range feature")
            For Each hp In _HeapList
                Dim dict = hp.GetLeaksFromLeakMultiple(7)
                lstLeaks.AddRange(dict.Values)
            Next
            nItem = 0
            For Each itm In lstLeaks.OrderBy(Function(a) a.AllocationStruct.SeqNo)
                nItem += 1
                _VBAssert.OutputText(String.Format(" itm # {0} {1} {2} {3}", nItem, itm.ToString, itm.SpyHeapPtr.HeapName, itm.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True)))
            Next

        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub
    <TestMethod()>
    <Ignore>
    Sub LeakMultipleUndo()
        InitTestMethodAndSetBaseLine()
        Try
            Dim SnapName = IO.Path.Combine(memspectInstallDir, "Snaps\Snap_TypeTextCSNoUndo_13_0")

            LoadOffLineSnap(SnapName, fWaitTilClrLoadDone:=False, fClearFilterToo:=False)
            _VBAssert.OutputText(
                String.Format(
                    "Leaks {0} {1:n0} - {2:n0}",
                    CleanLineForBaseline(SnapName),
                    _GlobalFilter.SeqNoLo,
                    _GlobalFilter.SeqNoHi
                    )
                )
            For i = 0 To _GlobalFilter.LeakMultiple - 1
                _VBAssert.OutputText(String.Format("Iter {0} Lo={1} Hi= {2}", i, _GlobalFilter._LeakMultipleSeqnos(i, 0), _GlobalFilter._LeakMultipleSeqnos(i, 1)))
            Next
            Dim lkMultipleToUse = _GlobalFilter.LeakMultiple
            _VBAssert.OutputText("LeakMulti=" + lkMultipleToUse.ToString)
            For Each hp In _HeapList
                If hp.GetHeapName = MemSpectHeapName Then
                    '_GlobalFilter.KnownIssues = ShowKnownIssuesEnum.Exclude
                    Dim lks = hp.GetLeaksFromLeakMultipleToList(lkMultipleToUse, _GlobalFilter._LeakMultipleSeqnos)
                    For Each lk In lks.OrderBy(Function(a) a.AllocationStruct.SeqNo) '.Where(
                        'Function(h) h.AllocationStruct.SeqNo >=
                        '    _GlobalFilter._LeakMultipleSeqnos(0, 0) AndAlso
                        '    h.AllocationStruct.SeqNo <= _GlobalFilter._LeakMultipleSeqnos(0, 1)
                        '    )

                        _VBAssert.OutputText("Leak: " + lk.GetKnownIssue + " :" + lk.ToString + " " + lk.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True))

                    Next
                Else
                    'If hp.GetHeapName = ProcessHeapName Or True Then
                    '    Dim lks = hp.GetLeaksFromLeakMultiple(lkMultipleToUse, _GlobalFilter._LeakMultipleSeqnos)
                    '    For Each lk In lks.Where(
                    '        Function(h) h.AllocationStruct.SeqNo >=
                    '            _GlobalFilter._LeakMultipleSeqnos(0, 0) AndAlso
                    '            h.AllocationStruct.SeqNo <= _GlobalFilter._LeakMultipleSeqnos(0, 1)
                    '            )
                    '        _VBAssert.OutputText("Leak: " + lk.KnownIssue + " " + lk.ToString + " " + lk.GetStringContent())

                    '    Next

                    'End If

                End If
            Next

            '"Microsoft.VisualStudio.Editor.Implementation.Undo.OleParentUndoUnit"
        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub

    <TestMethod()>
    <Ignore>
    Sub LeakMultipleTemp()
        InitTestMethodAndSetBaseLine()
        Try

            Dim SnapName = IO.Path.Combine(memspectInstallDir, "Snaps\VBType_47")

            LoadOffLineSnap(SnapName, fWaitTilClrLoadDone:=False, fClearFilterToo:=False)
            _VBAssert.OutputText(
                String.Format(
                    "leaks {0} {1:n0} - {2:n0}",
                    CleanLineForBaseline(SnapName),
                    _GlobalFilter.SeqNoLo,
                    _GlobalFilter.SeqNoHi
                    )
                )
            For i = 0 To _GlobalFilter.LeakMultiple - 1
                _VBAssert.OutputText(String.Format("Iter {0} Lo={1} Hi= {2}", i, _GlobalFilter._LeakMultipleSeqnos(i, 0), _GlobalFilter._LeakMultipleSeqnos(i, 1)))
            Next
            Dim lkMultipleToUse = _GlobalFilter.LeakMultiple
            _VBAssert.OutputText("LeakMulti=" + lkMultipleToUse.ToString)
            For Each hp In _HeapList
                If hp.GetHeapName = MemSpectHeapName Then
                    _GlobalFilter.KnownIssues = ShowKnownIssuesEnum.Exclude
                    Dim lks = hp.GetLeaksFromLeakMultipleToList(lkMultipleToUse, _GlobalFilter._LeakMultipleSeqnos)
                    For Each lk In lks.OrderBy(Function(a) a.AllocationStruct.SeqNo) '.Where(
                        'Function(h) h.AllocationStruct.SeqNo >=
                        '    _GlobalFilter._LeakMultipleSeqnos(0, 0) AndAlso
                        '    h.AllocationStruct.SeqNo <= _GlobalFilter._LeakMultipleSeqnos(0, 1)
                        '    )

                        _VBAssert.OutputText("Leak: " + lk.GetKnownIssue + " :" + lk.ToString + " " + lk.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True))

                    Next
                Else
                    'If hp.GetHeapName = ProcessHeapName Or True Then
                    '    Dim lks = hp.GetLeaksFromLeakMultiple(lkMultipleToUse, _GlobalFilter._LeakMultipleSeqnos)
                    '    For Each lk In lks.Where(
                    '        Function(h) h.AllocationStruct.SeqNo >=
                    '            _GlobalFilter._LeakMultipleSeqnos(0, 0) AndAlso
                    '            h.AllocationStruct.SeqNo <= _GlobalFilter._LeakMultipleSeqnos(0, 1)
                    '            )
                    '        _VBAssert.OutputText("Leak: " + lk.KnownIssue + " " + lk.ToString + " " + lk.GetStringContent())

                    '    Next

                    'End If

                End If
            Next


        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub

End Class
