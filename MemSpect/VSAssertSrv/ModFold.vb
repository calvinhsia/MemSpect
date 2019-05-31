Namespace MemSpect
    Module ModFold
        Public Function ShowModFold() As DataSurface
            Dim ctrls = DataWindowMain.MakeNewDatasurface("ModFold", "Module Fold", nMaxHeaderHeight:=45)
            Try
                ctrls.SurfaceHeader.Children.Add(New Label With {.Content = "Module Fold " + _GlobalFilter.ToString})

                ctrls.SurfaceDetails.Children.Add(New ModFoldPanel)

            Catch ex As Exception
                CommonUI.MemSpectExceptionHandler(ex)
            End Try
            Return ctrls
        End Function
    End Module

    Public Class ModFoldPanel
        Inherits SortableTVPanel
        Private Shared _ndxsym As Integer
        Friend Shared _dict As SortedList(Of String, ModFoldData)
        Friend Shared _sortorder As TVSortOrder = TVSortOrder.Size
        Friend Shared _tvsym As ModFoldTV
        Public Class ModFoldData
            Public allocs As New SortedDictionary(Of Long, HeapAllocationContainer)
        End Class

        Sub New()
            _dict = New SortedList(Of String, ModFoldData)
            VBDiagMarginBase.FreezeTargetThreads()
            Me._srchPanel.Visibility = Windows.Visibility.Hidden
            ReadHeaps(fClearFirst:=True)
            For Each hp In _HeapList
                Dim snap = hp.TakeMemSnapshot
                For Each alloc In snap.Allocs
                    If Not hp.IsMemSpectHeap OrElse alloc.TBlkBlockType <> BlockTypes.IndirectInfo Then ' don't include mapped file twice

                        Dim callstk = alloc.GetCallStackAsStringArray ' so syms get resolved
                        Dim priorFrameModule = ""
                        For i = callstk.Count - 1 To 0 Step -1
                            Dim FrameWithNoFileName = SymbolStripFileName(callstk(i))
                            If FrameWithNoFileName.Length > 0 Then
                                Dim newModule = GetSymbolModuleName(FrameWithNoFileName)
                                If newModule <> priorFrameModule Then
                                    Dim dat As ModFoldData = Nothing
                                    If Not _dict.TryGetValue(FrameWithNoFileName, dat) Then
                                        dat = New ModFoldData
                                        _dict.Add(FrameWithNoFileName, dat)
                                    End If
                                    If Not dat.allocs.ContainsKey(alloc.GetAddr.ToInt64) Then
                                        dat.allocs.Add(alloc.GetAddr.ToInt64, alloc)
                                    End If
                                    priorFrameModule = newModule
                                End If
                            End If
                        Next ' callstack
                    End If
                Next ' alloc
            Next ' heap
            _tvsym = New ModFoldTV
            Me.Children.Add(_tvsym)
        End Sub

        Friend Overrides Function SearchTreeItems(ByVal srchstring As String, ByVal fMakeSubSnapshot As Boolean) As Boolean
            Return MyBase.SearchTreeItems(srchstring, fMakeSubSnapshot)
        End Function

        Friend Overrides Sub on_SortNameSizeCnt(ByVal sender As Object, ByVal e As System.Windows.RoutedEventArgs)
            DataWindowMain._DataWindowMain.Cursor = Cursors.Wait
            Me.Children.Remove(_tvsym)
            Dim btnVal = CType(sender, RadioButton).Content.ToString.Replace("Sort", "").Trim
            _sortorder = CType([Enum].Parse(GetType(TVSortOrder), btnVal), TVSortOrder)
            _tvsym = New ModFoldTV
            Me.Children.Add(_tvsym)
            DataWindowMain._DataWindowMain.Cursor = Cursors.Arrow

        End Sub

        Shared Function AddKids(ByVal symbase As String,
                                ByVal ctrl As ItemsControl, ByVal nDepth As Integer) As Tuple(Of Integer, Long)
            Dim nTotCnt = 0
            Dim nTotSize As Long = 0
            Dim curitems As New List(Of ModFoldItem)

            Do While _ndxsym < _dict.Keys.Count
                Dim symbol = _dict.Keys(_ndxsym)
                If symbol.Length > symbase.Length AndAlso symbase = symbol.Substring(0, symbase.Length) Then
                    Dim bnds = GetSymbolBound(symbol, symbase.Length + 1)
                    Dim newitem = New ModFoldItem(ctrl)
                    curitems.Add(newitem)
                    If symbol = bnds Then 'leaf?
                        Dim agg = Aggregate a In _dict.Values(_ndxsym).allocs Into cnt = Count(), nSum = Sum(a.Value.GetSize)
                        newitem._ndxDict = _ndxsym
                        newitem.nCnt = agg.cnt
                        newitem.nSize = agg.nSum
                        newitem.SetFullName(symbol)
                        nTotCnt += agg.cnt
                        nTotSize += agg.nSum
                        _ndxsym += 1

                    Else
                        Dim Res = AddKids(bnds, newitem, nDepth + 1)
                        newitem.nCnt = Res.Item1
                        newitem.nSize = Res.Item2
                        nTotCnt += Res.Item1
                        nTotSize += Res.Item2
                        newitem.SetFullName(bnds)
                    End If
                Else
                    Exit Do
                End If
            Loop
            Select Case _sortorder
                Case TVSortOrder.Name
                    For Each itm In From a In curitems
                        ctrl.Items.Add(itm)
                    Next
                Case TVSortOrder.Count
                    For Each itm In From a In curitems Order By a.nCnt Descending
                        ctrl.Items.Add(itm)
                    Next
                Case TVSortOrder.Size
                    For Each itm In From a In curitems Order By a.nSize Descending
                        ctrl.Items.Add(itm)
                    Next
            End Select
            Return New Tuple(Of Integer, Long)(nTotCnt, nTotSize)
        End Function

        Friend Shared sepchars As Char() = {"."c, "!"c, ":"c, "<"c}

        Public Shared Function GetSymbolBound(ByVal sym As String, ByVal startpos As Integer) As String
            If startpos <= 1 Then
                Return GetSymbolModuleName(sym)
            End If
            Dim fDone = False

            Do While Not fDone
                Dim posSep = sym.IndexOfAny(sepchars, startpos)
                If posSep >= 0 Then
                    If sym(posSep) = "." AndAlso sym.Length > posSep + 3 Then
                        If "dll exe".Contains(sym.Substring(posSep + 1, 3).ToLower) Then
                            startpos = posSep + 4
                            Continue Do
                        End If
                    End If
                    If sym(posSep) = ":" AndAlso sym.Length > posSep + 1 Then
                        If sym(posSep + 1) = ":" Then  ' 2 consec ":"
                            posSep += 1
                        End If
                    End If
                    'Vsassert.dll!MySTLAlloc<std::_Tree_nod<std::_Tmap_traits<std::pair<unsigned long,void *>,TrkBlock,std::less<std::pair<unsigned long,void *> >,MySTLAlloc<std::pair<std::pair<unsigned long,void *>,TrkBlock> >,0> >::_Node>::allocate + 28 bytes
                    If sym(posSep) = "<" Then
                        posSep += 1
                        Dim nNest = 1
                        While posSep < sym.Length AndAlso nNest > 0
                            Select Case sym(posSep)
                                Case "<"c
                                    nNest += 1
                                Case ">"c
                                    nNest -= 1
                            End Select
                            posSep += 1
                        End While
                    End If
                    sym = sym.Substring(0, posSep)
                End If
                fDone = True
            Loop
            Return sym
        End Function

        Public Shared Function GetSymbolModuleName(ByVal sym As String) As String ' already stripped filename
            Dim pos = sym.IndexOf("!")
            If pos > 5 Then
                sym = sym.Substring(0, pos)
            End If
            Return sym
        End Function

        Class ModFoldTV
            Inherits MyTreeViewBase
            Sub New()
                _ndxsym = 0
                Me.ContextMenu.AddMnuItem("_SubSnapshot", "Show details in subsnap", AddressOf on_CtxMenuModFold)
                AddKids("", Me, 0)

            End Sub

            Private Sub on_CtxMenuModFold(ByVal sender As Object, ByVal e As RoutedEventArgs)
                Dim mitem = TryCast(e.OriginalSource, MenuItem)
                Dim vrb = mitem.Header.ToString
                Select Case vrb
                    Case "_SubSnapshot"
                        Dim itm = CType(Me.SelectedItem, ModFoldItem)
                        Dim lst As New List(Of HeapAllocationContainer)
                        getAllocsRecur(lst, itm)
                        Dim qfunc = Function(theheapAllocs As List(Of HeapAllocationContainer), thebmem As BrowseMem) As IEnumerable
                                        ' theheapAllocs is nothing
                                        Dim q = From hctr In lst
                                                Select
                                                    Address = hctr.GetAddr.ToString("x8"),
                                                    hctr.AllocationStruct.SeqNo,
                                                    Size = hctr.GetSize,
                                                    hctr.AllocationStruct.Thread,
                                                    Type = hctr.TBlkBlockType.ToString,
                                                    Data = hctr.GetDisplayData,
                                                    Heap = hctr.SpyHeapPtr.GetHeapName,
                                                    _HeapAllocationContainer = hctr

                                        Return q
                                    End Function

                        Dim ctrls = DataWindowMain.MakeNewDatasurface("SubSnap", itm.Header.ToString, nMaxHeaderHeight:=40)
                        ctrls.SurfaceHeader.Children.Add(New TextBlock With {
                                                         .Text = itm.ToString
                                                     }
                                                 )

                        ctrls.SurfaceDetails.Children.Add(New BrowseMem(
                                                          qfunc,
                                                          Nothing,
                                                          fAllowBrowStringFilter:=True,
                                                          ColWidths:={WIDTH_ADDRESS, WIDTH_SEQNO, 65, 65, 65, 300, 300}
                                                          )
                                                      )


                End Select
            End Sub

            Private Sub getAllocsRecur(ByVal lst As List(Of HeapAllocationContainer), ByVal itm As ModFoldItem)
                If itm.Items.Count > 0 Then
                    For Each symitem As ModFoldItem In itm.Items
                        If symitem._ndxDict <> -1 Then
                            lst.AddRange(_dict.Values(symitem._ndxDict).allocs.Values)
                        Else
                            getAllocsRecur(lst, symitem)
                        End If
                    Next
                Else
                    lst.AddRange(_dict.Values(itm._ndxDict).allocs.Values) ' leaf
                End If
            End Sub

        End Class

        <DebuggerDisplay("{tostring}")>
        Class ModFoldItem
            Inherits MyTreeViewItem
            Public Property nCnt As Integer
            Public Property nSize As Long
            Public Property _ndxDict As Integer = -1 ' -1 indicates no data avail (interior node)
            Friend _NodeName As String
            Sub New(ByVal parent As ItemsControl)
                Me.ItemContainerStyle = parent.ItemContainerStyle
            End Sub
            Public Sub SetFullName(ByVal NodeName As String)
                Dim spHeader As New StackPanel With {.Orientation = Orientation.Horizontal}
                spHeader.Children.Add(New TextBlock With {
                  .Text = nCnt.ToString("n0") + " "
                      }
                  )
                spHeader.Children.Add(New TextBlock With {
                  .Text = nSize.ToString("n0") + " ",
                  .Foreground = Brushes.DarkCyan
                      }
                  )
                spHeader.Children.Add(New TextBlock With {
                  .Text = NodeName
                      }
                  )
                _NodeName = NodeName

                Me.Header = spHeader
                If Me._ndxDict <> -1 Then
                    Me.ToolTip = _dict.Keys(_ndxDict)
                End If
            End Sub

            Sub on_Selected(ByVal o As Object, ByVal e As RoutedEventArgs) Handles Me.Selected
                Me.SelectedItemChanged(o, e, ModFoldPanel._tvsym._SelectedItems)
            End Sub

            Public Overrides Function ToString() As String
                Return String.Format("{0:n0}   {1:n0} {2}", Me.nCnt, Me.nSize, Me._NodeName)
            End Function
        End Class

    End Class

End Namespace