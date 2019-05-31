Imports System.ComponentModel
Imports System.Runtime.InteropServices


Namespace MemSpect

    Public Class TVObjRefPanel
        Inherits DockPanel
        Friend Shared _fCombineFromMeToMe As Boolean = False

        Public Shared Function CreateObjectReferenceDataSurface(ByVal hCtr As HeapAllocationContainer, ByVal cName As String) As DataSurface
            If hCtr.TBlkBlockType <> BlockTypes.ClrObject Then
                Return Nothing
            End If
            Using New DataWindowMain.SetCursorWait

                Dim ctrls = DataWindowMain.MakeNewDatasurface("ObjRef", "", nMaxHeaderHeight:=80)
                Dim spHoriz As New StackPanel With {.Orientation = Orientation.Horizontal}
                Dim spVert As New StackPanel With {.Orientation = Orientation.Vertical}
                spVert.Children.Add(New TextBlock With {
                                                 .Text = cName + vbCrLf + hCtr.ToString,
                                                 .ToolTip =
    <xml>
This References view for a ChosenObj shows 3 kinds of branches: the objects that ChosenObj refers to (RefFromMe “->”), the objects that refer to ChosenObj (RefToMe”&lt;-“) 
and the (possibly many) PathsToGCRoots (Root->obj1->obj2->....ChosenObj.
Child Nodes of the RefFromMe branch will always show RefFromMe data (as indicated by arrows "->" and "&lt;-"). Similarly for RefToMe. 
Data is obtained dynamically for perf reasons: the calculation to see if a node really has children isn't done until you actually select it. 
Use ContextMenu->GCRoots to see the roots.
</xml>.Value
                                             })

                Dim btnShowGCRoots = New Button With {
                    .Content = "Show _GCRoots",
                    .ToolTip = "Show the current GC Root list.",
                    .Height = 50
                    }
                AddHandler btnShowGCRoots.Click, Sub()
                                                     Try
                                                         ShowGCRoots()
                                                     Catch ex As Exception
                                                         MemSpectExceptionHandler(ex)
                                                     End Try

                                                 End Sub

                spHoriz.Children.Add(btnShowGCRoots)

                Dim chkCombineFromMeToMe = New CheckBox With {
                    .Content = "_CombineFromTo",
                    .ToolTip = "Each node has a combination of From and To nodes as children " +
                      vbCrLf + "If unchecked (uncombined), obj RefsFromMe are in 1st branch, RefsToMe are in 2nd, and the 3rd is PathsFromGCRoot" +
                      vbCrLf + "     Every node in the 1st branch is referenced by its parent" +
                      vbCrLf + "     and every node in the 2nd branch is a reference to its parent" +
                      vbCrLf + "If Checked, the 1st branch is the root obj, and the 2nd is PathsFromGCRoot" +
                      vbCrLf + "    Every node in the 1st branch can have 2 different children: RefsFromMe and RefsToMe" +
                      vbCrLf + "Either choice, some child nodes are omitted if they're already shown in their ancestry",
                    .IsChecked = _fCombineFromMeToMe
                }
                Dim lamChkHandler = New RoutedEventHandler(Sub(sender As Object, e As RoutedEventArgs)
                                                               _fCombineFromMeToMe = chkCombineFromMeToMe.IsChecked.GetValueOrDefault
                                                               DataWindowMain.RemoveCurrentTab()
                                                               Dim xx = CreateObjectReferenceDataSurface(hCtr, cName) ' recur
                                                           End Sub)

                chkCombineFromMeToMe.AddHandler(CheckBox.CheckedEvent, lamChkHandler)
                chkCombineFromMeToMe.AddHandler(CheckBox.UncheckedEvent, lamChkHandler)

                spHoriz.Children.Add(chkCombineFromMeToMe)
                spHoriz.Children.Add(New TextBlock With {.Text = "  "}) 'spacer
                spVert.Children.Add(New TextBlock() With {.Text = "Explore the reference graph of an object. Expand nodes, Right click on the object for more options"})
                Dim spRef1 = New StackPanel() With {.Orientation = Orientation.Horizontal}
                spVert.Children.Add(spRef1)
                spRef1.Children.Add(New TextBlock() With {.Text = "->", .Background = Brushes.Salmon})
                spRef1.Children.Add(New TextBlock() With {.Text = " arrow shows objects referenced by an object"})

                Dim spRef2 = New StackPanel() With {.Orientation = Orientation.Horizontal}
                spVert.Children.Add(spRef2)
                spRef2.Children.Add(New TextBlock() With {.Text = "<-", .Background = Brushes.PaleGreen})
                spRef2.Children.Add(New TextBlock() With {.Text = " arrow shows references to object"})

                spHoriz.Children.Add(spVert)

                ctrls.SurfaceHeader.Children.Add(spHoriz)
                ctrls.TopSurface.ToolTip = String.Format("References {0:x8} {1}", hCtr.TBlk.Address.ToInt32, cName)
                Dim panel = New TVObjRefPanel(hCtr, cName, ctrls)

                Dim sph As New StackPanel With {.Orientation = Orientation.Horizontal}
                sph.Children.Add(New TextBlock With {.Text = " Expand all total: "})
                sph.Children.Add(panel._txtDataExpandAllTotals)
                spVert.Children.Add(sph)

                ctrls.SurfaceDetails.Children.Add(panel)
                Return ctrls
            End Using
        End Function


        Friend _txtDataExpandAllTotals As New TextBlock
        Friend _tvObjRef As TVObjRef

        Private Sub New(ByVal hctr As HeapAllocationContainer, ByVal cname As String, ByVal ctrls As DataSurface)
            _tvObjRef = New TVObjRef(hctr, cname, Me)
            Me.Children.Add(_tvObjRef)
        End Sub

        Friend Shared Function ShowGCRoots() As DataSurface
            Using New DataWindowMain.SetCursorWait
                Dim ctrls = DataWindowMain.MakeNewDatasurface("GCRoots", "", nMaxHeaderHeight:=140)
                Dim sp As New StackPanel With {.Orientation = Orientation.Vertical}
                ctrls.SurfaceHeader.Children.Add(sp)
                sp.Children.Add(New TextBlock With {
                    .Text = "# GCRoots =" + GCData.GetGCRootInfo.Count.ToString + " Some can show more than once if flags, kind,rootid are different"
                })
                Dim spSum As New StackPanel With {.Orientation = Orientation.Horizontal}
                sp.Children.Add(spSum)
                Dim qRootKind = From a In GCData.GetGCRootInfo Group By RootKind = a.GetGCRootKind Into Count(), TotSize = Sum(a.GetSize)
                                Select RootKind, Count, TotSize

                spSum.Children.Add(New Browse(qRootKind))

                Dim qRootFlags = From a In GCData.GetGCRootInfo Group By RootFlags = a.GetGCRootFlags Into Count(), TotSize = Sum(a.GetSize)
                                 Select RootFlags, Count, TotSize

                spSum.Children.Add(New Browse(qRootFlags))
                Dim qfunc = Function(theheapAllocs As List(Of HeapAllocationContainer), bmem As BrowseMem) As IEnumerable
                                Dim q = From a In theheapAllocs
                                        Select
                                            Address = a.TBlk.Address.ToInt32.ToString("x8"),
                                            a.AllocationStruct.SeqNo,
                                            a.TBlk.Size,
                                            a.AllocationStruct.Thread,
                                            RootKind = a.GetGCRootKind,
                                            RootFlags = a.GetGCRootFlags,
                                            RootId = a.TBlk.Left.ToString("x8"),
                                            ClassName = a.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False),
                                            _HeapAllocationContainer = a

                                Return q
                            End Function

                'we set tblkType:= TrkType.ClrObjects so objref ctx menu for gcroots
                ctrls.SurfaceDetails.Children.Add(New BrowseMem(
                                                  qfunc,
                                                  GCData.GetGCRootInfo,
                                                  tblkType:=TrkType.ClrObjects,
                                                  ColWidths:={WIDTH_ADDRESS, WIDTH_SEQNO, 65, 65, 85, 105, 65, 500},
                                                  fAllowBrowStringFilter:=True))
                Return ctrls
            End Using

        End Function


        Public Class TVObjRef
            Inherits MyTreeViewBase
            Public _hctr As HeapAllocationContainer
            Friend Structure TotalsForExpandAll
                Friend hctrRoot As HeapAllocationContainer
                Friend TotCnt As Long
                Friend TotSize As Long
            End Structure
            Private _tvObjRefPanel As TVObjRefPanel
            Private Shared _IsInExpandAll As Boolean = False
            Friend Shared _TotalsForExpandAll As TotalsForExpandAll ' accum cnt,size
            Sub New(ByVal hctnr As HeapAllocationContainer, ByVal cname As String, ByVal tvObjRefPanel As TVObjRefPanel)
                _hctr = hctnr
                _tvObjRefPanel = tvObjRefPanel

                Me.ContextMenu.AddMnuItem(
                    "_SubSnapshot all objs only ref'd by this",
                    "Snap this item and referenced child items, recursively." + vbCrLf +
                    " All child items that can ONLY be reached by this item" + vbCrLf +
                    "Warning: can take along time, depending on graph",
                    AddressOf on_CtxMenuObjRef, 0)

                Me.ContextMenu.AddMnuItem(
                    "_SubSnapshot Children non-recursively",
                    "Snap just the child items (useful for collections).",
                    AddressOf on_CtxMenuObjRef, 0)

                Me.ContextMenu.AddMnuItem(
                    "_SubSnapshot Children recursively",
                    "Snap child items and their children, etc",
                    AddressOf on_CtxMenuObjRef, 0)


                Me.ContextMenu.AddMnuItem("Show stacks in _Notepad", "Dump this item", AddressOf on_CtxMenuObjRef, 0)

                Me.ContextMenu.AddMnuItem("_References", "References To/From this obj", AddressOf on_CtxMenuObjRef, 1)

                If _fCombineFromMeToMe Then
                    Me.Items.Add(New TVObjRefTVItem(hctnr, Me, NodeType.RootObject, NodeType.RootObject.ToString + " " + cname, String.Empty))
                Else
                    Me.Items.Add(New TVObjRefTVItem(hctnr, Me, NodeType.RefFromParent, "RefFromMe " + cname, String.Empty))

                    Me.Items.Add(New TVObjRefTVItem(hctnr, Me, NodeType.RefToParent, "RefToMe " + cname, String.Empty))

                End If

                Dim tvItemgcRootBranch = New TVObjRefTVItem(
                                       hctnr,
                                       Me,
                                       NodeType.PathFromGCRoot,
                                       " Paths from GCRoots to " + cname,
                                       "")
                Me.Items.Add(tvItemgcRootBranch)

                CType(Me.Items(0), TVObjRefTVItem).IsExpanded = True ' expand 1st branch (Root or FromMe)
                If Not _fCombineFromMeToMe Then
                    CType(Me.Items(1), TVObjRefTVItem).IsExpanded = True ' expand 2nd branch (ToMe)
                End If
            End Sub


            Private _datawindowmainCursor As DataWindowMain.SetCursorWait
            Protected Overrides Sub OnExpandAllStart(ByVal selItem As MyTreeViewItem)
                _IsInExpandAll = True
                _datawindowmainCursor = New DataWindowMain.SetCursorWait
                MyBase.OnExpandAllStart(selItem)
                Dim tvitem = CType(selItem, TVObjRefTVItem)
                TVObjRef._TotalsForExpandAll = New TotalsForExpandAll With {.hctrRoot = tvitem._HeapAllocationContainer}
            End Sub

            Protected Overrides Sub OnExpandAllEnd()
                _IsInExpandAll = False
                _datawindowmainCursor.Dispose()
                _datawindowmainCursor = Nothing
                MyBase.OnExpandAllEnd()
                _tvObjRefPanel._txtDataExpandAllTotals.Text = String.Format(" for ""{0}""  Cnt= {1:n0} Size= {2:n0}",
                                                             Me.SelectedItem.ToString,
                                                             TVObjRef._TotalsForExpandAll.TotCnt,
                                                             TVObjRef._TotalsForExpandAll.TotSize)
            End Sub
            Structure qStruct
                Public hctr As HeapAllocationContainer
                Public Depth As Integer
            End Structure
            Private Sub on_CtxMenuObjRef(ByVal sender As Object, ByVal e As RoutedEventArgs)
                Dim tvitm = TryCast(Me.SelectedItem, TVObjRefTVItem)
                Dim mitem = TryCast(e.OriginalSource, MenuItem)
                Dim vrb = mitem.Header.ToString
                Select Case vrb
                    Case "_SubSnapshot all objs only ref'd by this"
                        ShowObjRefGraphSnapshot(tvitm)
                    Case "_SubSnapshot Children recursively"

                        Dim hashAlreadyDone = New HashSet(Of IntPtr)
                        Dim que As New Queue(Of qStruct)
                        que.Enqueue(New qStruct With {.hctr = tvitm._HeapAllocationContainer, .Depth = 0})
                        Dim lstAllReferred = New List(Of qStruct)
                        Dim nMaxQLen = 0
                        Dim nSizeTotal As ULong
                        Dim nTotalObjsExamined = 0
                        Do While que.Count > 0
                            Dim curQItem = que.Dequeue
                            Dim obj = curQItem.hctr
                            nTotalObjsExamined += 1
                            Dim orefs = obj.GetObjectRefData(NodeType.RefFromParent)(0)
                            For Each oref In orefs
                                If Not hashAlreadyDone.Contains(oref.GetAddr) Then
                                    hashAlreadyDone.Add(oref.GetAddr)
                                    If (oref.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False) = "System.WeakReference") Then
                                    Else
                                        Dim newitem = New qStruct With {.hctr = oref, .Depth = curQItem.Depth + 1}
                                        que.Enqueue(newitem)
                                        lstAllReferred.Add(newitem)
                                        nSizeTotal += CULng(oref.GetSize())
                                    End If
                                End If
                            Next
                            If que.Count > nMaxQLen Then
                                nMaxQLen = que.Count
                            End If
                        Loop
                        Dim ctrls = DataWindowMain.MakeNewDatasurface(
                            "Descendents",
                            String.Format("Descendent reference tree of {0}", tvitm._HeapAllocationContainer),
                            nMaxHeaderHeight:=40)
                        Dim bqdel = Function(lst As List(Of HeapAllocationContainer), thebmem As BrowseMem) As IEnumerable
                                        Dim qDetails As IEnumerable
                                        qDetails = From o In lstAllReferred
                                                   Let a = o.hctr
                                                   Select
                                                Address = a.GetAddr.ToString("x8"),
                                                a.AllocationStruct.SeqNo,
                                                Size = a.GetSize,
                                                a.AllocationStruct.Thread,
                                                o.Depth,
                                                ClassName = a.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True),
                                                _HeapAllocationContainer = a
                                                   Order By SeqNo

                                        thebmem._ColWidths = {WIDTH_ADDRESS, WIDTH_SEQNO, 60, 60, 60}
                                        thebmem.ColumnTips = {TIP_ADDRESS, TIP_SEQNO, TIP_SIZE, TIP_THREAD, "depth from root (# of reference levels)"}
                                        thebmem._arrColumnsToTotal = {"Size"}
                                        '                                                             thebmem.InitialSortOrder = New BrowseInitialSortOrder With {.ColumnNo = 2, .direction = ListSortDirection.Ascending}
                                        Return qDetails

                                    End Function

                        Dim br = New BrowseMem(bqdel, Nothing)
                        ctrls.SurfaceHeader.Children.Add(New TextBlock() With {
                                .Text = String.Format("All referenced children of {0} Total size = {1:n0}", tvitm._HeapAllocationContainer, nSizeTotal)})
                        ctrls.SurfaceDetails.Children.Add(br)

                    Case "_SubSnapshot Children non-recursively"
                        Dim refs = tvitm._HeapAllocationContainer.GetObjectRefData(NodeType.RefFromParent)
                        ShowSubSnapShot(refs(0), "NonRecursive children of " + tvitm.ToString)

                    Case "Show stacks in _Notepad"
                        BrowseMem.ShowStacksInNotepad(Nothing, tvitm.Tag.ToString, tvitm._HeapAllocationContainer)
                    Case "_References"
                        Dim classname = tvitm._HeapAllocationContainer.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True)
                        classname = String.Format("{0} 0x{1:x8} ({2:n0})", classname, tvitm._HeapAllocationContainer.GetAddr.ToInt32, tvitm._HeapAllocationContainer.GetSize)
                        TVObjRefPanel.CreateObjectReferenceDataSurface(tvitm._HeapAllocationContainer, classname)
                    Case Else
                        MessageBox.Show("ObjRef menu ClicKed: " + mitem.Header.ToString)
                End Select
            End Sub

            Friend Sub on_MouseMove(ByVal sender As Object, ByVal e As MouseEventArgs) Handles Me.MouseMove
                Try
                    Dim tb = TryCast(e.OriginalSource, TextBlock)
                    If tb IsNot Nothing AndAlso tb.IsLoaded AndAlso (tb.Text = "->" OrElse tb.Text = "<-" OrElse tb.Text = "--") Then ' not loaded if no UI: under test
                        Dim tvitem = GetAncestor(Of TVObjRefTVItem)(tb)

                        If _LastTipObj IsNot Nothing AndAlso _LastTipObj.ToolTip IsNot Nothing Then
                            If tb Is _LastTipObj Then
                                Return ' same obj, don't recalc
                            End If
                            ClearPriorTVToolTipIfAny()
                        End If
                        Dim sptip = GetAddressToolTip(tvitem._HeapAllocationContainer)
                        'Dim tiphdr = String.Empty

                        'Select Case tvitem._nodeType
                        '    Case NodeType.RootObject
                        '        tiphdr = "Root"
                        '    Case NodeType.RefFromParent
                        '        tiphdr = "This entire branch is descending the Obj hierarchy: the child nodes are what each parent node obj references. " +
                        '            vbCrLf + "Some child nodes are not shown if they also occur in the ancestry to prevent circular relationships"
                        '    Case NodeType.RefToParent
                        '        tiphdr = "This entire branch is ascending the Obj hierarchy: the child nodes are objs that reference each parent node" +
                        '            vbCrLf + "Some child nodes are not shown if they also occur in the ancestry to prevent circular relationships"
                        '    Case NodeType.PathFromGCRoot
                        '        tiphdr = "Direct path from the obj to a GCRoot"
                        'End Select
                        'tiphdr += vbCrLf + tvitem.ToString
                        sptip.Children.Insert(0, New TextBlock With {.Text = tvitem.ToString})
                        Dim ttipObj = New ToolTip With {
                            .PlacementTarget = tb,
                            .Placement = Controls.Primitives.PlacementMode.Bottom,
                            .Content = sptip
                        }
                        tb.ToolTip = ttipObj
                        ttipObj.IsOpen = True
                        _LastTipObj = tb
                    Else
                        ClearPriorTVToolTipIfAny()
                    End If
                Catch ex As Exception
                    CommonUI.MemSpectExceptionHandler(ex)
                End Try
            End Sub

            <DebuggerDisplay("{ToString()}")>
            Public Class TVObjRefTVItem
                Inherits MyTreeViewItem
                Implements IComparable

                Public _HeapAllocationContainer As HeapAllocationContainer
                Friend _nodeType As NodeType
                Friend _tvParentItem As ItemsControl '  TVObjRefTVItem or TVObjRef
                Friend _DidtryGettingChildren As Boolean
                Sub New(ByVal hCntr As HeapAllocationContainer,
                        ByVal tvParentItem As ItemsControl,
                        ByVal btype As NodeType,
                        Optional ByVal strNodeName As String = "",
                        Optional ByVal extraInfo As String = "",
                        Optional ByVal fNeverExpand As Boolean = False)

                    If fNeverExpand Then ' when user is navigating tree and finds circ ref, we don't want child expandable 
                        '_DidtryGettingChildren = True
                    End If
                    _tvParentItem = tvParentItem
                    _HeapAllocationContainer = hCntr
                    _nodeType = btype
                    Me.ItemContainerStyle = tvParentItem.ItemContainerStyle
                    Dim sysStr = ""
                    Dim fIsSystemString = False
                    Dim fIsSystemStringArray = False
                    Dim memberNameOfParent As String = String.Empty ' membername of Parent's member: e.g. Form.Control = Button
                    Dim memberNameToParent = String.Empty ' member name of current item referencing "parent". e.g. Button.containiner = Form
                    Dim strSuffix = String.Empty
                    If strNodeName = String.Empty Then
                        Dim hctrParent As HeapAllocationContainer = Nothing
                        Dim parentItem = TryCast(tvParentItem, TVObjRef)
                        If parentItem Is Nothing Then
                            hctrParent = TryCast(tvParentItem, TVObjRefTVItem)._HeapAllocationContainer
                        Else
                            hctrParent = parentItem._hctr
                        End If
                        If btype = NodeType.RefFromParent OrElse btype = NodeType.PathFromGCRoot Then
                            Dim dictMems = hctrParent.GetClrClassMembersToDict
                            If dictMems.ContainsKey(hCntr.GetAddr) Then
                                Dim lst = dictMems(hCntr.GetAddr)
                                Dim smemName = lst.First
                                lst.Remove(smemName)
                                memberNameOfParent = smemName + " = "
                            End If
                        Else ' for RefToMe, gcroot   <-
                            'memberName = "foo = "
                        End If
                        strNodeName = hCntr.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True)
                        If strNodeName.StartsWith("System.String") Then
                            fIsSystemString = True
                            sysStr = strNodeName.Substring(13)
                            If sysStr.StartsWith("[]") Then
                                fIsSystemStringArray = True
                            End If
                            strNodeName = strNodeName.Substring(0, 13)
                        End If
                        If btype = NodeType.RefToParent Then
                            ' here we're constructing a node that has a reference to the parent, so we want the member name of the current 
                            ' item that is the parent ref. "parentItem" refers to the child item
                            Dim dictMems = hCntr.GetClrClassMembersToDict ' get the members of the current object
                            If dictMems.ContainsKey(hctrParent.GetAddr) Then
                                memberNameToParent = "." + dictMems(hctrParent.GetAddr).FirstOrDefault
                            End If
                        End If
                        strSuffix = String.Format(" 0x{0:x8} ({1})", hCntr.TBlk.Address.ToInt32, hCntr.TBlk.Size)
                    End If
                    If Not String.IsNullOrEmpty(extraInfo) Then
                        strNodeName += " " + extraInfo
                    End If
                    Dim sp As New StackPanel With {.Orientation = Orientation.Horizontal}
                    Dim arrow = String.Empty
                    Dim bgcolor = Brushes.PaleGreen
                    Select Case _nodeType
                        Case NodeType.RootObject
                            bgcolor = Brushes.PowderBlue
                            arrow = "--"
                        Case NodeType.RefFromParent, NodeType.PathFromGCRoot
                            arrow = "->"
                            If _nodeType = NodeType.RefFromParent Then
                                bgcolor = Brushes.LightSalmon
                            Else
                                bgcolor = Brushes.Khaki
                            End If
                        Case NodeType.RefToParent
                            arrow = "<-"
                    End Select
                    sp.Children.Add(New TextBlock With {
                                    .Text = arrow,
                                    .Background = bgcolor
                                   }
                            )

                    If Not String.IsNullOrEmpty(memberNameOfParent) Then
                        sp.Children.Add(New TextBlock With {
                                        .Text = memberNameOfParent,
                                        .Foreground = Brushes.DarkCyan
                                        }
                                    )
                    End If

                    sp.Children.Add(New TextBlock With {
                                    .Text = strNodeName
                                    }
                                )
                    If Not String.IsNullOrEmpty(memberNameToParent) Then
                        sp.Children.Add(New TextBlock With {
                                        .Text = memberNameToParent,
                                        .Foreground = Brushes.DarkCyan
                                        }
                                    )

                    End If

                    If Not String.IsNullOrEmpty(strSuffix) Then
                        sp.Children.Add(New TextBlock With {
                                        .Text = strSuffix
                                        }
                                    )

                    End If

                    If Not String.IsNullOrEmpty(sysStr) Then
                        sp.Children.Add(New TextBlock With {
                                        .Text = " " + sysStr,
                                        .FontWeight = FontWeights.Bold
                                        }
                                    )
                    End If
                    Me.Header = sp
                    Me.Tag = strNodeName
                    If _nodeType = NodeType.RefFromParent OrElse
                        _nodeType = NodeType.RefToParent OrElse
                        _nodeType = NodeType.RootObject OrElse
                        (_nodeType = NodeType.PathFromGCRoot AndAlso TryCast(tvParentItem, TVObjRef) IsNot Nothing) Then

                        If fIsSystemStringArray OrElse
                            Not fIsSystemString OrElse
                            _nodeType = NodeType.RefToParent Then ' system.string can't reference anything
                            Me.Items.Add(New TreeViewItem With {.Tag = "dummy"})
                        End If
                    End If
                End Sub

                Friend Sub on_Expand(ByVal o As Object, ByVal e As RoutedEventArgs) Handles Me.Expanded
                    If _IsInExpandAll Then ' for ExpandAll, only expand descending subtrees
                        If Me._nodeType = NodeType.RefToParent Then
                            Return
                        End If
                    End If
                    If Not _DidtryGettingChildren Then '1st time?                       ' expand 1 level
                        Dim tempList As New SortedSet(Of TVObjRefTVItem) 'store them so we can sort before adding
                        _DidtryGettingChildren = True
                        If Me.Items.Count = 1 Then ' is it a dummy?
                            Dim possibleDum = Me.Items(0)
                            Dim lamInclude = Function(hctr As HeapAllocationContainer) As Boolean
                                                 If _IsInExpandAll Then
                                                     ' if we're doing an expand all, and the thing we're expanding is older than the curitem, then don't expand
                                                     If _TotalsForExpandAll.hctrRoot.AllocationStruct.SeqNo > hctr.AllocationStruct.SeqNo Then
                                                         Return False
                                                     End If
                                                 End If
                                                 Return True
                                             End Function
                            Dim lamExpand = Sub()
                                                If CStr(CType(possibleDum, TreeViewItem).Tag) = "dummy" Then
                                                    Me.Items.Clear() ' get rid of dummy
                                                    If _nodeType <> NodeType.PathFromGCRoot Then
                                                        Dim dictParentStuff = _HeapAllocationContainer.GetClrClassMembersToDict
                                                        Dim lstTypesToExpand As NodeType()
                                                        If _fCombineFromMeToMe Then
                                                            lstTypesToExpand = {NodeType.RefFromParent, NodeType.RefToParent}
                                                        Else
                                                            lstTypesToExpand = {_nodeType}
                                                        End If
                                                        For Each nodeTyp As NodeType In lstTypesToExpand
                                                            Dim lsts = _HeapAllocationContainer.GetObjectRefData(nodeTyp)
                                                            If lsts.Count = 0 Then
                                                                Continue For
                                                            End If
                                                            Dim hctrs = lsts(0)
                                                            tempList.Clear()
                                                            For Each h In hctrs '.Distinct(New HeapAllocationContainerComparer) 'distinct children
                                                                If h = Me._HeapAllocationContainer Then
                                                                    Continue For
                                                                End If
                                                                If Not lamInclude.Invoke(h) Then
                                                                    Continue For
                                                                End If
                                                                Dim testparentItem = Me._tvParentItem

                                                                Do While testparentItem IsNot Nothing 'AndAlso testparentItem._HeapAllocationContainer IsNot Nothing
                                                                    Dim tryObjRef = TryCast(testparentItem, TVObjRef)
                                                                    If tryObjRef IsNot Nothing Then
                                                                        Exit Do ' got to top without match
                                                                    Else
                                                                        Dim tryTVItemRef = CType(testparentItem, TVObjRefTVItem)
                                                                        If tryTVItemRef._HeapAllocationContainer = h Then
                                                                            ' when user is navigating tree and finds circ ref, we don't want child expandable 
                                                                            'tempList.Add(New TVObjRefTVItem(h, Me, nodeTyp, fNeverExpand:=True))
                                                                            Continue For ' don't add: circ ref
                                                                        End If
                                                                        testparentItem = tryTVItemRef._tvParentItem
                                                                    End If
                                                                Loop
                                                                Dim newitem = New TVObjRefTVItem(h, Me, nodeTyp)
                                                                tempList.Add(newitem)
                                                            Next
                                                            For Each sitem In tempList
                                                                Me.Items.Add(sitem)
                                                            Next
                                                        Next
                                                    Else
                                                        AddGCRootPaths(_HeapAllocationContainer, Me)
                                                    End If
                                                End If

                                            End Sub
                            If _IsInExpandAll Then
                                Using New DataWindowMain.SetCursorWait
                                    lamExpand.Invoke()
                                End Using
                            Else
                                lamExpand.Invoke()
                            End If
                        End If
                    End If
                End Sub

                Friend Sub AddGCRootPaths(ByVal hctnr As HeapAllocationContainer, ByVal tvItemgcRootBranch As TVObjRefTVItem)
                    Dim bt = NodeType.PathFromGCRoot
                    Dim heapAllocLists = hctnr.GetObjectRefData(bt)
                    CType(tvItemgcRootBranch.Header, StackPanel).Children.Add(New TextBlock With {.Text = " (# Paths = " + heapAllocLists.Count.ToString + ")"})
                    For i = 0 To heapAllocLists.Count - 1

                        Dim tvItemgcRoot = New TVObjRefTVItem(
                                           hctnr,
                                           tvItemgcRootBranch,
                                           bt,
                                           bt.ToString + " (PathLen = " + heapAllocLists(i).Count.ToString + ") ",
                                           "")
                        tvItemgcRoot.Items.Clear() ' remove the dummy item: we're adding kids here
                        tvItemgcRootBranch.Items.Add(tvItemgcRoot)
                        Dim parntitem = tvItemgcRootBranch
                        For Each hcntrRef In heapAllocLists(i)
                            Dim itmchild =
                                    New TVObjRefTVItem(
                                          hcntrRef,
                                          parntitem,
                                          bt,
                                          strNodeName:=String.Empty,
                                          extraInfo:=hcntrRef.GetGCRootExtraInfo
                                          )
                            tvItemgcRoot.Items.Add(itmchild)
                            parntitem = itmchild
                            '    tvitemgcroot = itmchild
                        Next
                    Next
                    tvItemgcRootBranch.ExpandAll()
                End Sub

                Protected Overrides Sub OnExpandAll()
                    MyBase.OnExpandAll()
                    TVObjRef._TotalsForExpandAll.TotCnt += 1
                    TVObjRef._TotalsForExpandAll.TotSize += _HeapAllocationContainer.GetSize
                End Sub

                Public Overrides Function ToString() As String
                    Dim sp = CType(Me.Header, StackPanel)
                    Dim sResult = String.Empty
                    For Each tb In sp.Children
                        sResult += CType(tb, TextBlock).Text
                    Next
                    Return sResult
                End Function

                Public Function CompareTo(ByVal obj As Object) As Integer Implements System.IComparable.CompareTo
                    Dim n = Me.ToString.CompareTo(obj.ToString)
                    Return n
                End Function
            End Class


            Public Function ShowObjRefGraphSnapshot(ByVal tvitmRoot As TVObjRefTVItem) As DataSurface

                Dim _ObjGraph = tvitmRoot._HeapAllocationContainer.GetObjectsOnlyRefdByThis

                Dim lstSnap = New List(Of HeapAllocationContainer)
                lstSnap.AddRange((From itm In _ObjGraph Where itm.fIsInGraph Select itm.objdata.hctnr))

                Dim qfunc = Function(theheapAllocs As List(Of HeapAllocationContainer), thebmem As BrowseMem) As IEnumerable
                                Dim qDetails As IEnumerable
                                qDetails = From a In _ObjGraph
                                           Where a.fIsInGraph
                                           Let hctr = a.objdata.hctnr
                                           Select
                                               Address = hctr.TBlk.Address.ToString("x8"),
                                               hctr.AllocationStruct.SeqNo,
                                               hctr.TBlk.Size,
                                               hctr.AllocationStruct.Thread,
                                               ChildRecursionLevel = a.nLevel,
                                               Gen = hctr.GetGen,
                                               Moved = hctr.GetMovedCnt,
                                               Srviv = hctr.TBlk.UnionData2 >> 16,
                                               Classid = hctr.GetClassId.ToInt32.ToString("x8"),
                                               ClassName = hctr.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True),
                                               _HeapAllocationContainer = hctr
                                           Order By ChildRecursionLevel

                                thebmem.ColumnTips = {TIP_ADDRESS, TIP_SEQNO, TIP_SIZE, TIP_THREAD, "# of levels of indirection from " + tvitmRoot.ToString}
                                thebmem._ColWidths = {WIDTH_ADDRESS, WIDTH_SEQNO, 60, 60, 60, 60, 60, 60, 65, 900}
                                thebmem.InitialSortOrder = New BrowseInitialSortOrder With {.ColumnNo = 5, .direction = ComponentModel.ListSortDirection.Ascending}
                                thebmem._arrColumnsToTotal = {"Size", "Moved", "Srviv"}
                                Return qDetails
                            End Function
                Dim ctrls = ShowSubSnapShot(lstSnap, "Children SubSnap " + tvitmRoot.ToString, qfunc, tblkType:=TrkType.ClrObjects)
                Return ctrls
            End Function

        End Class

    End Class


End Namespace
