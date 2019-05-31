Namespace MemSpect

    Public Class SnapDiffer
        <Flags>
        Public Enum OptionsForDiff
            None = 0
            ProcessHeap = 1
            AllOtherNativeHeaps = 2
            ManagedObjects = 4
            VirtualAllocs = 8
            Files = 16
        End Enum


        Public Shared Function LoadTheDiff(snapName As String,
                                           Optional markerStr0 As String = "",
                                           Optional markerStr1 As String = "") As Integer

            Dim snaps(1) As offlineSnapHelper
            Dim nSnapIndexToUse = 0 ' for main snap, 0, for others, just a unique #
            For i = 0 To 1
                If i = 0 Then
                    If MemNodeDiff._SnapShotHelperDict.Count = 1 Then ' already have item 0
                        Debug.Assert(MemNodeDiff._SnapShotHelperDict(0)._offlineSnapSave Is _offlineSnapshot, "dict 0 doesn't match?")
                        Continue For
                    End If
                End If
                If i = 1 Then
                    nSnapIndexToUse = MemNodeDiff._SnapShotHelperDict.Select(Function(e) e.Key).Max + 1
                    Debug.Assert(Not MemNodeDiff._SnapShotHelperDict.ContainsKey(nSnapIndexToUse))
                End If

                snaps(i) = New offlineSnapHelper(nSnapIndexToUse, markerStr0, markerStr1)
                If i = 0 Then
                    snaps(i).InitFirst()
                Else
                    UpdateStatusMsg("Loading " + snapName)
                    snaps(i).InitSecond(snapName)
                End If
                MemNodeDiff._SnapShotHelperDict(nSnapIndexToUse) = snaps(i)
            Next
            Return nSnapIndexToUse
        End Function



        Friend Class offlineSnapHelper
            Implements IDisposable
            Public _snapPath As String
            Public _offlineFilter As New GlobalFilter
            Dim _filesInSnap As String()
            Friend _offlineSnapSave As OfflineMegaSnapshot
            Friend _lstAllocs As New List(Of AllocForDiff)
            Friend _nSnapIndexToUse As Integer
            Dim markerId(1) As Integer

            Sub New(nSnapIndexToUse As Integer, markerStr0 As String, markerStr1 As String)
                _nSnapIndexToUse = nSnapIndexToUse
                If Not String.IsNullOrEmpty(markerStr0) Then
                    markerId(0) = GetCodeMarkerIdFromName(markerStr0)
                End If
                If Not String.IsNullOrEmpty(markerStr1) Then
                    markerId(1) = GetCodeMarkerIdFromName(markerStr1)
                End If
            End Sub

            Public Sub InitFirst()
                _offlineSnapSave = Common._offlineSnapshot
                SetFilter()
            End Sub

            Sub SetFilter()
                If markerId(0) > 0 OrElse markerId(1) > 0 Then
                    Dim markers = GetHeapSnapshot(MemSpectHeapName).Allocs.Where(Function(h) h.TBlkBlockType = BlockTypes.CodeMarker)
                    If markerId(0) > 0 Then
                        _offlineFilter.SeqNoLo = markers.Where(Function(h) h.TBlk.UnionData1 = markerId(0)).Single.AllocationStruct.SeqNo
                    End If
                    If markerId(1) > 0 Then
                        _offlineFilter.SeqNoHi = markers.Where(Function(h) h.TBlk.UnionData1 = markerId(1)).Single.AllocationStruct.SeqNo
                    End If
                End If
                UpdateStatusMsg(String.Format("Filter for {0} = {1}", _nSnapIndexToUse, _offlineFilter.ToString))

            End Sub

            Public Sub InitSecond(snapPath As String)
                _snapPath = snapPath
                Dim loadedsnap = New OfflineMegaSnapshot(snapPath)
                Common._offlineSnapshot = loadedsnap
                _filesInSnap = Common._offlineSnapshot.GetFilesInSnapshot()
                Using deserializer = OfflineMegaSnapshot.GetDeserializerForPath(IO.Path.Combine(snapPath, "StackFrameDictionary.mhd"), Common._offlineSnapshot)
                    Dim stackWrapper As StackDictionaryWrapper = CType(deserializer.GetEntryObject(), StackDictionaryWrapper)
                End Using
                _offlineSnapSave = Common._offlineSnapshot
                SetFilter()
            End Sub

            Public Function GetHeapSnapshot(desiredHeapName As String) As MemSnapshot
                Dim snap As MemSnapshot = Nothing
                Common._offlineSnapshot = _offlineSnapSave
                If _offlineSnapSave._snapshots IsNot Nothing AndAlso _offlineSnapSave._snapshots.Count > 0 Then
                    snap = _offlineSnapSave._snapshots.Where(Function(s) s.SpyHeap.HeapName.StartsWith(desiredHeapName)).Single
                Else
                    For Each file In _filesInSnap
                        If Common._offlineSnapshot.OffLineFileIsHeapName(file) Then
                            If file.StartsWith(desiredHeapName) Then
                                Dim fullfilename = IO.Path.Combine(_snapPath, file)
                                '_VBAssert.OutputText("Got heap " + fullfilename)
                                Using deserializer = OfflineMegaSnapshot.GetDeserializerForPath(fullfilename, Common._offlineSnapshot)
                                    snap = CType(deserializer.GetEntryObject(), Common.MemSnapshot)
                                    '_VBAssert.OutputText(snap.Allocs.Count.ToString)
                                    '_VBAssert.OutputText(snap.SpyHeap.HeapName)
                                    Exit For
                                End Using
                            End If
                        End If
                    Next
                End If

                Return snap
            End Function

            Public Function GetAllocsFromHeapSnapshot(desiredheapName As String) As IEnumerable(Of HeapAllocationContainer)
                Dim snap = GetHeapSnapshot(desiredheapName)
                Dim allocs = snap.Allocs.Where(Function(h) _offlineFilter.FilterFunction(h))
                Return allocs
            End Function

            Public Sub FillData(DiffOptions As OptionsForDiff)
                Common._offlineSnapshot = _offlineSnapSave
                If _nSnapIndexToUse > 0 Then
                    MemNodeDiff._SnapShotHelperDict(0).FillData(DiffOptions) ' recur
                End If
                Dim allocs As IEnumerable(Of HeapAllocationContainer) = New List(Of HeapAllocationContainer)
                Dim heapName = String.Empty
                If (DiffOptions And OptionsForDiff.ProcessHeap) > 0 Then
                    heapName = Common.ProcessHeapName
                    allocs = allocs.Union(GetAllocsFromHeapSnapshot(heapName))
                End If

                If (DiffOptions And OptionsForDiff.AllOtherNativeHeaps) > 0 Then
                    If _nSnapIndexToUse = 0 Then
                        For Each hp In _HeapList.Where(Function(heap) heap.HeapName <> MemSpectHeapName AndAlso heap.HeapName <> ProcessHeapName)
                            allocs = allocs.Union(hp.TakeMemSnapshot(fEnableFilter:=False).Allocs.Where(Function(h) _offlineFilter.FilterFunction(h)))
                        Next
                    Else
                        For Each file In _filesInSnap.Where(Function(heapFileName) Not heapFileName.StartsWith(MemSpectHeapName) AndAlso Not heapFileName.StartsWith(ProcessHeapName))
                            If _offlineSnapshot.OffLineFileIsHeapName(file) Then
                                allocs = allocs.Union(GetAllocsFromHeapSnapshot(file))
                            End If
                        Next
                    End If
                End If

                If (DiffOptions >= OptionsForDiff.ManagedObjects) Then
                    heapName = Common.MemSpectHeapName
                    Dim heapSnapshot = GetHeapSnapshot(heapName)
                    allocs = allocs.Union(GetAllocsFromHeapSnapshot(heapName).
                        Where(Function(h)
                                  If (DiffOptions And OptionsForDiff.ManagedObjects) > 0 Then
                                      If h.TBlkBlockType = BlockTypes.ClrObject Then
                                          Return True
                                      End If
                                  End If
                                  If (DiffOptions And OptionsForDiff.VirtualAllocs) > 0 Then
                                      If h.TBlkBlockType = BlockTypes.VirtualAlloc Then
                                          Return True
                                      End If
                                  End If
                                  If (DiffOptions And OptionsForDiff.Files) > 0 Then
                                      If h.TBlkBlockType = BlockTypes.MapFile Then
                                          Return True
                                      End If
                                  End If
                                  Return False
                              End Function))
                End If
                _lstAllocs.Clear()
                For Each alloc In allocs.OrderByDescending(Function(h) h.GetSize)
                    _lstAllocs.Add(New AllocForDiff(_nSnapIndexToUse, alloc))
                Next
            End Sub

            Friend Function AddTheKids(lstAllocs As List(Of AllocForDiff)(),
                                      nLevel As Integer,
                                      Optional memnodeResult As MemNodeDiff = Nothing
                                      ) As MemNodeDiff

                If memnodeResult Is Nothing Then
                    memnodeResult = New MemNodeDiff
                End If
                Dim sdict = New Dictionary(Of String, MemNodeDiff)
                Dim maxNumAllocs = Math.Max(lstAllocs(0).Count, lstAllocs(1).Count)

                For nAlloc = 0 To maxNumAllocs - 1
                    Dim alloc(1) As AllocForDiff
                    For snapNum = 0 To 1
                        If nAlloc < lstAllocs(snapNum).Count Then
                            alloc(snapNum) = lstAllocs(snapNum)(nAlloc)
                            Dim hctr = alloc(snapNum).hctr
                            Dim ndxframe = hctr.AllocationStruct.m_uicStackAddr - nLevel - 1
                            If ndxframe >= 0 Then
                                Dim addrFrame = alloc(snapNum)._stack(ndxframe)
                                Dim name = ResolveAddressToSymbol(addrFrame, fStripFileName:=False, fStripBytesToo:=False, fNormalizeHexAddress:=True)
                                Dim mnode As MemNodeDiff = Nothing
                                If Not sdict.TryGetValue(name, mnode) Then
                                    mnode = New MemNodeDiff With {._frameName = name}
                                    sdict(name) = mnode
                                End If
                                mnode.AddOne(snapNum, alloc(snapNum))
                            End If
                        End If
                    Next
                Next
                If sdict.Values.Count = 0 Then
                    For snapnum = 0 To 1
                        For Each alloc In lstAllocs(snapnum)
                            memnodeResult.AddOne(snapnum, alloc)
                        Next
                    Next
                Else
                    For Each mnode In sdict.Values
                        If mnode._totSize(0) <> mnode._totSize(1) OrElse
                            mnode._arrLstAllocs(0).Count <> mnode._arrLstAllocs(1).Count Then
                            '                _VBAssert.OutputText(String.Format("rec {0}", mnode))

                            AddTheKids(mnode._arrLstAllocs, nLevel + 1, memnodeResult) ' recur

                        Else
                            ' thrown away because they match in both snaps
                            '_VBAssert.OutputText(String.Format("   {0} {1} {2}",
                            '                                   New String(" "c, nLevel * 2),
                            '                                   nLevel,
                            '                                   mnode))

                        End If
                    Next
                End If
                Return memnodeResult
            End Function

            Private disposedValue As Boolean ' To detect redundant calls

            ' IDisposable
            Protected Overridable Sub Dispose(disposing As Boolean)
                If Not Me.disposedValue Then
                    If disposing Then
                        If _offlineSnapSave IsNot Nothing Then
                            _offlineSnapshot = _offlineSnapSave
                            OfflineMegaSnapshot.CloseZipPackage()
                            _offlineSnapshot = MemNodeDiff._SnapShotHelperDict(0)._offlineSnapSave
                        End If
                    End If
                End If
                Me.disposedValue = True
            End Sub

            Public Sub Dispose() Implements IDisposable.Dispose
                Dispose(True)
                GC.SuppressFinalize(Me)
            End Sub

        End Class
#If False Then
/// Clone all fields from an instance of base class TSrc into derived class TDst
public static TDst Clone<TSrc, TDst>(TSrc source, TDst target)
    where TDst : TSrc
{
    var bf = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.FlattenHierarchy;
    foreach (FieldInfo fis in source.GetType().GetFields(bf))
        fis.SetValue(target, fis.GetValue(source));
    return target;
}

/// Create a new instance of a derived class, cloning all fields from type TSrc
public static TDst Clone<TSrc, TDst>(TSrc source)
    where TDst : TSrc, new()
{
    return Clone(source, new TDst());
}
#End If

        'Public Shared Function Clone(Of TSrc As Class, TDst As TSrc)(source As TSrc, target As TDst) As TDst
        '    Dim bf = Reflection.BindingFlags.Instance Or Reflection.BindingFlags.Public Or Reflection.BindingFlags.NonPublic Or Reflection.BindingFlags.FlattenHierarchy
        '    For Each fis In source.GetType().GetFields(bf)
        '        fis.SetValue(target, fis.GetValue(source))
        '    Next
        '    Return target
        'End Function
        Public Shared Function Clone(Of TSrc, TDst As {TSrc, New})(source As TSrc) As TDst
            '            Return Clone(source, New TDst)
            Dim target = New TDst
            Dim bf = Reflection.BindingFlags.Instance Or Reflection.BindingFlags.Public Or Reflection.BindingFlags.NonPublic Or Reflection.BindingFlags.FlattenHierarchy
            For Each fis In source.GetType().GetFields(bf)
                fis.SetValue(target, fis.GetValue(source))
            Next
            Return target
        End Function
        'Public Class AllocForDiffTry
        '    Inherits HeapAllocationContainer
        '    Private _OfflineMegaSnapshot As OfflineMegaSnapshot
        '    Public Sub New()

        '    End Sub
        '    Public Overrides Sub ReInit()
        '        Common._offlineSnapshot = _OfflineMegaSnapshot
        '    End Sub

        'End Class
        Public Class MemNodeDiff
            ' dict that contains lookup from offline snap index to offlinesnap helper
            ' 1st entry (index 0) contains the main offline snap
            Friend Shared _SnapShotHelperDict As New Dictionary(Of Integer, offlineSnapHelper)
            Sub New()
                If _ConnectionMode <> MemSpectMode.Offline Then
                    Throw New InvalidOperationException("Must be offline snapshot mode")
                End If
                _arrLstAllocs(0) = New List(Of AllocForDiff)
                _arrLstAllocs(1) = New List(Of AllocForDiff)
            End Sub
            Public Sub AddOne(snapNum As Integer, alloc As AllocForDiff)
                _arrLstAllocs(snapNum).Add(alloc)
                _totSize(snapNum) += alloc.hctr.GetSize()
            End Sub
            ''' <summary>
            ''' 
            ''' </summary>
            ''' <param name="nIndexToClear">0 means clear all</param>
            ''' <remarks></remarks>
            Public Shared Sub ClearDict(nIndexToClear As Integer)
                If nIndexToClear > 0 Then
                    Debug.Assert(_SnapShotHelperDict.ContainsKey(nIndexToClear))
                    Dim offlineSnapHelper = _SnapShotHelperDict(nIndexToClear)
                    offlineSnapHelper.Dispose()
                    _SnapShotHelperDict.Remove(nIndexToClear)
                Else
                    For Each itm In _SnapShotHelperDict
                        If itm.Key <> 0 Then
                            itm.Value.Dispose()
                        End If
                    Next
                    _SnapShotHelperDict.Clear()
                End If
            End Sub

            Public _totSize(1) As Integer
            Public _arrLstAllocs(1) As List(Of AllocForDiff)
            Public _frameName As String
            Public _memNodeChildren As List(Of MemNodeDiff)
            Public Overrides Function ToString() As String
                Return String.Format("c0={0} c1={1} s0={2} s1={3} {4} {5} {6}",
                                     _arrLstAllocs(0).Count,
                                     _arrLstAllocs(1).Count,
                                     _totSize(0),
                                     _totSize(1),
                                     _frameName,
                                     If(_arrLstAllocs(0).Count = 0, String.Empty, _arrLstAllocs(0)(0).hctr.ToString),
                                     If(_arrLstAllocs(1).Count = 0, String.Empty, _arrLstAllocs(1)(0).hctr.ToString)
                                     )
            End Function
        End Class

        Public Class AllocForDiff
            Public _nOffLineSnapshotIndex As Integer ' 0 or 1
            Public _stack As IntPtr()
            Private _hctr As HeapAllocationContainer
            Public Sub New(snapNum As Integer, hctr As HeapAllocationContainer)
                _nOffLineSnapshotIndex = snapNum
                _hctr = hctr
                _stack = hctr.GetCallStackAddressestoArray
            End Sub
            Public ReadOnly Property hctr As HeapAllocationContainer
                Get
                    Debug.Assert(SnapDiffer.MemNodeDiff._SnapShotHelperDict(1) IsNot Nothing, "snapshot num nothing?")
                    Common._offlineSnapshot = SnapDiffer.MemNodeDiff._SnapShotHelperDict(_nOffLineSnapshotIndex)._offlineSnapSave
                    Return _hctr
                End Get
            End Property
            Public Overrides Function ToString() As String
                Return _hctr.ToString()
            End Function
        End Class

    End Class


End Namespace
