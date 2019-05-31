Imports MemSpect.SnapDiffer
Imports System.Text

Namespace MemSpect
    Public Class SnapDifferUI
        Public Shared Function ShowSnapDiff(snapFolderPath As String) As DataSurface
            '            Dim snapName = "c:\MemSpect\Snaps\MarkerSnap_perfVSCloseSolutionBegin__VSConsumption_0_2"
            '            snapName = "c:\MemSpect\Snaps\CSLife2"
            Dim ctrls = DataWindowMain.MakeNewDatasurface("SDiff", "Snap Difference with " + snapFolderPath, nMaxHeaderHeight:=100)
            Dim spvert = New StackPanel With {.Orientation = Orientation.Vertical}
            spvert.Children.Add(
                New TextBlock With {
                    .Text =
                    String.Format("Showing Snap Diff {0} {1}",
                                  _offlineSnapshot._DataFilePath,
                                  snapFolderPath
                                  )
                })

            Dim spTypes = New StackPanel With {.Orientation = Orientation.Horizontal}
            Dim chkProcessHeap = New CheckBox With {.Content = "_Process Heap", .IsChecked = True}
            Dim chkAllOtherNative = New CheckBox With {.Content = "_All Other Natve", .IsChecked = False}
            Dim chkManaged = New CheckBox With {.Content = "_Managed Objects", .IsChecked = True}
            Dim chkVirtualAllocs = New CheckBox With {.Content = "_VirtualAllocs", .IsChecked = False}
            Dim chkFiles = New CheckBox With {.Content = "_Files", .IsChecked = False}

            spTypes.Children.Add(chkProcessHeap)
            spTypes.Children.Add(chkAllOtherNative)
            spTypes.Children.Add(chkManaged)
            spTypes.Children.Add(chkVirtualAllocs)
            spTypes.Children.Add(chkFiles)

            Dim btnDoit = New Button With {.Content = "_Doit", .Height = 20}
            spTypes.Children.Add(btnDoit)
            spvert.Children.Add(spTypes)
            ctrls.SurfaceHeader.Children.Add(spvert)
            Dim dpanel As DiffPanel = Nothing
            Dim marker1 = "perfStartCLREnd"
            Dim marker2 = "perfVSInitUIThread"
            marker1 = String.Empty
            marker2 = String.Empty
            Dim snapnum = LoadTheDiff(snapFolderPath, marker1, marker2)

            AddHandler btnDoit.Click, Sub(o, e)
                                          Try
                                              Dim opts = OptionsForDiff.None
                                              If chkProcessHeap.IsChecked Then
                                                  opts = opts Or OptionsForDiff.ProcessHeap
                                              End If
                                              If chkAllOtherNative.IsChecked Then
                                                  opts = opts Or OptionsForDiff.AllOtherNativeHeaps
                                              End If
                                              If chkManaged.IsChecked Then
                                                  opts = opts Or OptionsForDiff.ManagedObjects
                                              End If
                                              If chkVirtualAllocs.IsChecked Then
                                                  opts = opts Or OptionsForDiff.VirtualAllocs
                                              End If
                                              If chkFiles.IsChecked Then
                                                  opts = opts Or OptionsForDiff.Files
                                              End If
                                              UpdateStatusMsg("Calculating Diff for " + snapFolderPath)
                                              dpanel = New DiffPanel(snapnum, opts)
                                              ctrls.SurfaceDetails.Children.Clear()
                                              ctrls.SurfaceDetails.Children.Add(dpanel)
                                              'AddHandler ctrls.TopSurface.IsKeyboardFocusedChanged, Sub(o, e)
                                              '                                                          UpdateStatusMsg("IsKeyboardFocusedChanged" + o.ToString + ":" + e.ToString)
                                              '                                                      End Sub
                                              _offlineSnapshot = MemNodeDiff._SnapShotHelperDict(0)._offlineSnapSave
                                              UpdateStatusMsg("Done showing Snap diff for " + snapFolderPath)
                                          Catch ex As Exception
                                              MemSpectExceptionHandler(ex)
                                          End Try

                                      End Sub
            AddHandler ctrls.TopSurface.Unloaded, Sub(o, e)
                                                      UpdateStatusMsg("Unloaded " + o.ToString + ":" + e.ToString)
                                                      If dpanel IsNot Nothing AndAlso dpanel._offlineSnapHelper IsNot Nothing Then
                                                          MemNodeDiff.ClearDict(dpanel._offlineSnapHelper._nSnapIndexToUse)
                                                      End If
                                                  End Sub
            If _IsUnderTest Then
                btnDoit.RaiseEvent(New RoutedEventArgs With {.RoutedEvent = Button.ClickEvent})
            End If
            Return ctrls
        End Function


        Public Class DiffPanel
            Inherits DockPanel
            Friend _offlineSnapHelper As offlineSnapHelper
            Dim _tv As SnapDiffTreeView
            Sub New(snapnum As Integer, opts As OptionsForDiff)

                _offlineSnapHelper = MemNodeDiff._SnapShotHelperDict(snapnum)
                _offlineSnapHelper.FillData(opts)

                Dim res = _offlineSnapHelper.AddTheKids(
                    {MemNodeDiff._SnapShotHelperDict(0)._lstAllocs,
                     MemNodeDiff._SnapShotHelperDict(snapnum)._lstAllocs},
                 nLevel:=0)
                Dim _tv = New SnapDiffTreeView(res)
                Me.Children.Add(_tv)
            End Sub
        End Class

        Public Class SnapDiffTreeView
            Inherits MyTreeViewBase

            Sub New(res As MemNodeDiff)
                AddTheChildrenForDiff(res._arrLstAllocs(0).Union(res._arrLstAllocs(1)), Me, 0)
                If Me.Items.Count > 0 Then
                    CType(Me.Items(0), SnapDiffTVItem).IsExpanded = True
                End If
                Me.ContextMenu.AddMnuItem(
                    "Show allocs in notepad",
                    "dump to notepad",
                    Sub()
                        Dim itm = CType(Me.SelectedItem, SnapDifferUI.SnapDiffTVItem)
                        Dim sb = New StringBuilder
                        Dim mn = itm._mnode
                        For Each alloc In mn._lstAllocs
                            sb.AppendLine(String.Format("{0} {1}", alloc._nOffLineSnapshotIndex, alloc.hctr.ToString))
                            sb.AppendLine(alloc.hctr.GetCallStackAsString)
                        Next
                        WriteOutputToTempFile(sb.ToString)

                    End Sub,
                    InsertPos:=0)
            End Sub

            Friend Sub AddTheChildrenForDiff(lstAllocs As IEnumerable(Of AllocForDiff), listCtrl As ItemsControl, nLevel As Integer)
                Dim sdict = New Dictionary(Of String, MemNodeDiffForUi)
                For Each itm In lstAllocs
                    Dim hctr = itm.hctr ' calls property get, which changes the _offlinesnap shot appropriately
                    Dim ndxframe = hctr.AllocationStruct.m_uicStackAddr - nLevel - 1
                    If ndxframe >= 0 Then
                        Dim addrFrame = itm._stack(ndxframe)
                        Dim name = ResolveAddressToSymbol(addrFrame, fStripFileName:=True, fStripBytesToo:=True, fNormalizeHexAddress:=True)
                        Dim mnode As MemNodeDiffForUi = Nothing
                        If Not sdict.TryGetValue(name, mnode) Then
                            mnode = New MemNodeDiffForUi With {._frameName = name}
                            sdict(name) = mnode
                        End If
                        Dim oneOrZero = If(itm._nOffLineSnapshotIndex = 0, 0, 1)
                        mnode._cntArr(oneOrZero) += 1
                        mnode._totalArr(oneOrZero) += hctr.GetSize
                        mnode._lstAllocs.Add(itm)
                    End If

                Next
                For Each itm In sdict.Values.OrderByDescending(Function(mn) Math.Abs(mn._totalArr(1) - mn._totalArr(0)))
                    Dim newitem = New SnapDiffTVItem(itm, Me, fIsDummy:=False, nLevel:=nLevel)
                    listCtrl.Items.Add(newitem)
                Next
            End Sub
        End Class

        Public Class MemNodeDiffForUi
            Public _lstAllocs As New List(Of AllocForDiff)
            Public _cntArr(1) As Integer
            Public _totalArr(1) As Integer

            Public _frameName As String
        End Class

        Public Class SnapDiffTVItem
            Inherits MyTreeViewItem
            Friend _fIsDummy As Boolean
            Friend _mnode As MemNodeDiffForUi
            Friend _tvm As SnapDiffTreeView
            Friend _nLevel As Integer
            Sub New(mnode As MemNodeDiffForUi, tvm As SnapDiffTreeView, fIsDummy As Boolean, nLevel As Integer)
                _fIsDummy = fIsDummy
                _mnode = mnode
                _tvm = tvm
                _nLevel = nLevel
                If Not fIsDummy Then
                    Dim sp = New StackPanel With {.Orientation = Orientation.Horizontal}
                    Dim strText = String.Format("Delta={0:n0} Cnt0={1:n0} Cnt1={2:n0} Tot0={3:n0}, Tot1={4:n0} {5}",
                                                mnode._totalArr(1) - mnode._totalArr(0),
                                                _mnode._cntArr(0),
                                                _mnode._cntArr(1),
                                                _mnode._totalArr(0),
                                                _mnode._totalArr(1),
                                                mnode._frameName)
                    sp.Children.Add(New TextBlock() With {.Text = strText})
                    Me.ItemContainerStyle = tvm.ItemContainerStyle
                    Me.Header = sp
                    If mnode._lstAllocs.Count > 0 Then
                        Dim tviDummy = New SnapDiffTVItem(Nothing, tvm, fIsDummy:=True, nLevel:=nLevel + 1)
                        Me.Items.Add(tviDummy)
                    End If
                End If
            End Sub
            Sub onExpand(o As Object, e As RoutedEventArgs) Handles Me.Expanded
                If HasDummyChild() Then
                    Me.Items.Clear() ' remove dummy node
                    _tvm.AddTheChildrenForDiff(_mnode._lstAllocs, Me, nLevel:=_nLevel + 1)
                    _offlineSnapshot = MemNodeDiff._SnapShotHelperDict(0)._offlineSnapSave
                End If
                e.Handled = True
            End Sub
            Function HasDummyChild() As Boolean
                Dim res = False
                If Me.Items.Count = 1 Then
                    If CType(Me.Items(0), SnapDiffTVItem)._fIsDummy Then
                        res = True
                    End If
                End If
                Return res
            End Function
            Public Overrides Function ToString() As String
                If _fIsDummy Then
                    Return "DUMMY"
                End If
                Return CType(CType(Me.Header, StackPanel).Children(0), TextBlock).Text
            End Function
        End Class
    End Class


End Namespace
