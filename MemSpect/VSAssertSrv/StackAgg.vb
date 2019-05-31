Namespace MemSpect
    Public Class TVStackAggPanel
        Inherits SortableTVPanel
        Public ReadOnly Property nTotal As Integer
            Get
                Return _tv._nTotal
            End Get
        End Property
        Public ReadOnly Property NTotalSize As UInteger
            Get
                Return _tv._nTotalSize
            End Get
        End Property
        Friend _tv As StackAggTreeView
        Friend _qDetails As Collections.IEnumerable
        Private _bmem As BrowseMem ' can be nothing for tests
        Sub New(ByVal qDetails As Collections.IEnumerable, ByVal bmem As BrowseMem, Optional ByVal nPivotRootAddress As Integer = 0)
            _qDetails = qDetails
            _bmem = bmem
            _tv = New StackAggTreeView(qDetails, Me, bmem, New IntPtr(nPivotRootAddress))
            Me.Children.Add(_tv)
        End Sub

        'Private lastSearchString As String = String.Empty
        'Private detailsEnumerator As IEnumerator = Nothing

        Friend Overrides Function SearchTreeItems(ByVal srchstring As String, ByVal fMakeSubSnapshot As Boolean) As Boolean
            If srchstring.Equals(String.Empty) Then
                Return False
            End If
            Browse.g_LastStringFilterContent = srchstring
            Dim fPositive = True
            Dim srchText = srchstring
            If srchstring.StartsWith("!") Then
                srchText = srchText.Substring(1)
                fPositive = False
            End If
            srchText = srchText.ToLower
            'If (srchstring.Equals(lastSearchString)) Then
            '    'do continuing search from where we left off
            'Else
            '    lastSearchString = srchstring
            '    detailsEnumerator = _qDetails.GetEnumerator()
            'End If

            Dim lstAllocs As New List(Of HeapAllocationContainer)
            'Dim rangePartitioner = System.Collections.Concurrent.Partitioner.Create(_qDetails)

            'todo: this is for incremental searching.
            'todo: expandtoitem() has an issue where there may be two nodes at the same level with the same name, but expandtoitem() only searches one of them.  
            '      need to fix that and then incremental search should work.
            'While True
            '    If detailsEnumerator.MoveNext() Then
            '        Dim tdesc = ComponentModel.TypeDescriptor.GetProperties(detailsEnumerator.Current)("_HeapAllocationContainer")
            '        Dim tmp = tdesc.GetValue(detailsEnumerator.Current)
            '        If tmp Is Nothing Then
            '            Continue While
            '        End If

            '        Dim hctr = CType(tmp, HeapAllocationContainer)
            '        For Each frm In From f In hctr.ResolveCallStackFrames
            '                        Where f.ToLower.Contains(srchstring)

            '            If Not fMakeSubSnapshot Then
            '                Me._tv.ExpandToItem(hctr, frmToSelect:=frm)
            '                Return True
            '            Else
            '                lstAllocs.Add(hctr)
            '            End If
            '            Exit For
            '        Next

            '    Else
            '        Exit While
            '    End If

            'End While
            For Each itm In _qDetails
                Dim hctr = HeapAllocationContainer.CreateFrom(itm)
                If hctr Is Nothing Then
                    Continue For
                End If

                Dim strGotIt = String.Empty
                Dim fResult = hctr.GetCallStackAsStringArray.Any(Function(o) As Boolean
                                                                     If o.IndexOf(srchText, StringComparison.OrdinalIgnoreCase) >= 0 Then
                                                                         strGotIt = o
                                                                         Return True
                                                                     End If
                                                                     Return False
                                                                 End Function)
                If Not fPositive Then
                    fResult = Not fResult
                End If
                If fResult Then
                    If Not fMakeSubSnapshot Then
                        Me._tv.ExpandToItem(hctr, frmToSelect:=strGotIt)
                        Return True
                    Else
                        lstAllocs.Add(hctr)
                    End If
                End If

                'Dim res = hctr.GetCallStackAsStringArray.Where(Function(o) As Boolean

                '                                                   If fPositive Then

                '                                                   Else

                '                                                   End If
                '                                                   Return True
                '                                               End Function)

                'For Each frm As String In hctr.GetCallStackAsStringArray.Where(Function(o) o.ToLower.Contains(
                '                Where lam.Invoke()
                '    'For Each frm As String In From f In hctr.GetCallStackAsStringArray
                '    '                Where lam.Invoke()

                '    If Not fMakeSubSnapshot Then
                '        Me._tv.ExpandToItem(hctr, frmToSelect:=frm)
                '        Return True
                '    Else
                '        lstAllocs.Add(hctr)
                '    End If
                '    Exit For
                'Next

            Next
            If fMakeSubSnapshot Then
                ShowSubSnapShot(lstAllocs, "SubSnapShot Search results: """ + srchstring + """")
                Return True
            End If
            Return False
        End Function

        Friend Overrides Sub on_SortNameSizeCnt(ByVal sender As Object, ByVal e As System.Windows.RoutedEventArgs)
            Using New DataWindowMain.SetCursorWait
                Dim oldval = _tv
                Me.Children.Remove(_tv)
                Dim btnVal = CType(sender, RadioButton).Content.ToString.Replace("Sort", "").Trim
                Dim sorder = CType([Enum].Parse(GetType(TVSortOrder), btnVal), TVSortOrder)
                Select Case sorder
                    Case TVSortOrder.Count
                        Me._btnSortCnt.IsChecked = True
                    Case TVSortOrder.Name
                        Me._btnSortName.IsChecked = True
                    Case TVSortOrder.Size
                        Me._btnSortSize.IsChecked = True

                End Select
                If oldval IsNot Nothing Then
                    _tv = New StackAggTreeView(_qDetails, Me, _bmem)
                    Me.Children.Add(_tv)
                End If
            End Using
        End Sub

        'MyTreeView takes a query as input, aggregates stacks. 
        ' if you pass in a starting Addr, then the stacks are inverted, rooted at that Addr 
        Public Class StackAggTreeView
            Inherits MyTreeViewBase

            Public _nRootAddress As IntPtr ' if 0, then normal stack starting at alloca. 
            '                               Else, it's a const addr from which we root an inverted stack
            Private _TVStackAggPanel As TVStackAggPanel
            Public _nTotalSize As UInteger = 0
            Public _nTotal As Integer = 0

            Friend _qdetails As IEnumerable
            Friend _bmem As BrowseMem 'Bmem = nothing means don't do UI: used for RecurdownMultiples

            Friend _isInverted As Boolean = False ' when false, fastest varying stack level is at root of treeview (leading to MemSpect). When true, root is ThreadStart
            Public ReadOnly Property IsDoingUI As Boolean
                Get
                    Return If(_bmem Is Nothing, False, True)
                End Get
            End Property
            'Private Shared _tdesc As ComponentModel.PropertyDescriptor ' = ComponentModel.TypeDescriptor.GetProperties(lstSrc(0))("_HeapAllocationContainer")
            ''' <summary>
            ''' Bmem = nothing means don't do UI: used for RecurdownMultiples
            ''' </summary>
            Sub New(ByVal qDetails As Collections.IEnumerable, ByVal TVstkPanel As TVStackAggPanel, ByVal bmem As BrowseMem, Optional ByVal nRootAddress As IntPtr? = Nothing)
                _bmem = bmem
                _TVStackAggPanel = TVstkPanel
                _qdetails = qDetails
                _nRootAddress = If(nRootAddress.HasValue, nRootAddress.Value, IntPtr.Zero)

                If IsDoingUI Then

                    Me.ContextMenu.AddMnuItem("_SubSnapshot", "create new view of this snapshot from this stack level or multiselection", AddressOf on_ctxMenuItemStkAgg, 0)

                    Me.ContextMenu.AddMnuItem("_View Source Code", "open the source code using source server (only for native code)", AddressOf on_ctxMenuItemStkAgg, 1)

                    Me.ContextMenu.AddMnuItem("_Goto Detail View", "Switch to Detail tab and select up to 1st 5 items on this stack frame", AddressOf on_ctxMenuItemStkAgg, 2)

                    Me.ContextMenu.AddMnuItem("_Waste", "Show wasted allocations from this frame", AddressOf on_ctxMenuItemStkAgg, 3)

                    Me.ContextMenu.AddMnuItem("_References", "References from and to this object. Managed and Native. Max 10 results", AddressOf on_ctxMenuItemStkAgg, 4)

                    Me.ContextMenu.AddMnuItem("_Unused Members", "Treat these allocations as a class, show unused members from this frame, Show class member value distribution", AddressOf on_ctxMenuItemStkAgg, 5)

                    Me.ContextMenu.AddMnuItem("Show stacks in _Notepad", "Dump 1st 20 child items or multiselection", AddressOf on_ctxMenuItemStkAgg, 6)

                    Me.ContextMenu.AddMnuItem("Show stacks & _Mem dump in Notepad", "Dump 1st 20 child items or multiselection", AddressOf on_ctxMenuItemStkAgg, 7)

                    Me.ContextMenu.AddMnuItem("Export to _PerfView", "Export to an XML file that PerfView (\\clrmain\tools\PerfView.exe) can read.  Must have extension '.perfview.xml'", AddressOf on_ctxMenuItemStkAgg, 8)


                    Dim itm = Me.ContextMenu.AddMnuItem("_Assert for this StackFrame", "Whenever this stack frame appears while running, break", AddressOf on_ctxMenuItemStkAgg, 9)
                    itm.IsEnabled = _ConnectionMode = MemSpectMode.OnLine
                    Me.ContextMenu.AddMnuItem("_Pivot", "Show all stacks that start exactly at this address. See also Aggregate Callstacks->String Search->Subsnap results",
                                              AddressOf on_ctxMenuItemStkAgg, 10)
                    Me.ContextMenu.AddMnuItem("_Duplicates", "Show all dupes from this frame", AddressOf on_ctxMenuItemStkAgg, 11)

                    Me.ContextMenu.AddMnuItem("Find lea_ks", "Find leaks in allocations", AddressOf on_ctxMenuItemStkAgg, 12)
                End If

                Dim lamWhenEnabledChanged = Sub(o As Object, e As RoutedEventArgs)
                                                Using New DataWindowMain.SetCursorWait
                                                    Me._isInverted = _TVStackAggPanel._chkInvertStack.IsChecked.Value
                                                    SortableTVPanel.g_InitInvertStack = Me._isInverted ' persist per session
                                                    Me.Items.Clear()
                                                    Dim fcalcMinmax = True
                                                    If TryCast(o, Button) IsNot Nothing AndAlso CStr(CType(o, Button).Content) = "_Apply" Then
                                                        _TVStackAggPanel._WhenDistPanel._SeqNoMin = _TVStackAggPanel._WhenDistPanel._WhenDistTxtBoxMin.GetSeqNo
                                                        _TVStackAggPanel._WhenDistPanel._SeqNoMax = _TVStackAggPanel._WhenDistPanel._WhenDistTxtBoxMax.GetSeqNo
                                                        fcalcMinmax = False
                                                    End If

                                                    Dim res = Me.AddTheChildren(qDetails, nDepth:=0, listCtrl:=Me, fRecalcMinMax:=fcalcMinmax)
                                                    If _TVStackAggPanel._WhenDistPanel._WhenDistEnabled Then
                                                        _TVStackAggPanel._WhenDistPanel.SetRange(_TVStackAggPanel._WhenDistPanel._SeqNoMin, _TVStackAggPanel._WhenDistPanel._SeqNoMax)
                                                    End If
                                                    _nTotal = res.Item1
                                                    _nTotalSize = res.Item2
                                                    For Each itm1stLEvel As StackAggTViewItem In Me.Items
                                                        itm1stLEvel.IsExpanded = True ' we can expand each 1st level item, but it's slow
                                                    Next
                                                    Dim lst = Me.Items
                                                    Do While lst.Count = 1
                                                        CType(lst(0), StackAggTViewItem).IsExpanded = True
                                                        Exit Do
                                                        lst = CType(lst(0), StackAggTViewItem).Items
                                                    Loop
                                                End Using
                                            End Sub
                AddHandler _TVStackAggPanel._WhenDistPanel._chkEnableWhen.Checked, lamWhenEnabledChanged
                AddHandler _TVStackAggPanel._WhenDistPanel._chkEnableWhen.Unchecked, lamWhenEnabledChanged

                AddHandler _TVStackAggPanel._WhenDistPanel._btnApplyWhenMinMax.Click, lamWhenEnabledChanged

                AddHandler _TVStackAggPanel._chkInvertStack.Checked, lamWhenEnabledChanged
                AddHandler _TVStackAggPanel._chkInvertStack.Unchecked, lamWhenEnabledChanged


                lamWhenEnabledChanged.Invoke(Nothing, Nothing)
            End Sub

            Friend Sub on_ctxMenuItemStkAgg(ByVal sender As Object, ByVal e As RoutedEventArgs)
                Debug.Assert(IsDoingUI, "Need BMem for ctx menu")
                Dim mitem = TryCast(e.OriginalSource, MenuItem)
                Dim vrb = mitem.Header.ToString
                Dim tvitm = TryCast(Me.SelectedItem, StackAggTViewItem)
                Select Case vrb
                    Case "_SubSnapshot"
                        Dim allocs = New List(Of HeapAllocationContainer)
                        Dim enumSource As New List(Of HeapAllocationContainer)
                        If Me._SelectedItems.Count > 0 Then
                            For Each n In Me._SelectedItems
                                Dim tvitem = CType(n, StackAggTViewItem)
                                enumSource.AddRange(tvitem._memNode._hctrList)
                            Next
                        Else
                            enumSource = tvitm._memNode._hctrList
                        End If
                        ShowSubSnapShot(enumSource, "SubSnapShot " + If(enumSource.Count = 1, tvitm.ToString, "MultiSelect #items =" + enumSource.Count.ToString() + " "))
                    Case "Find lea_ks"
                        Dim dwTargetAddr As IntPtr = IntPtr.Zero

                        If Me._nRootAddress = IntPtr.Zero Then 'normal stack
                            dwTargetAddr = tvitm._memNode._hctrList(0).GetCallStackAddr(tvitm._nDepth - 1)
                        Else ' upside down
                            dwTargetAddr = tvitm._memNode._addrCallStkAtThisDepth
                        End If

                        CommonUI.SearchForLeaksInAllocs(Common.GetAllocsForPivot(dwTargetAddr))
                    Case "_View Source Code"

                        OpenSourceFilesRecur(tvitm, fRecur:=False)
                        '                                    OpenSourceCodeFile(tvitm._memNode._baseName)
                    Case "_Goto Detail View"
                        'have to instantiate details tab content first:
                        _bmem.OnTabItemDetailsGotFocus(_bmem._TabItemDetails, New RoutedEventArgs)
                        Dim blist = _bmem._DetailBrowse._BrowseList
                        blist.SelectedItems.Clear()
                        Dim detitms = blist.Items
                        Dim defview = CollectionViewSource.GetDefaultView(blist.ItemsSource)
                        Dim srcColl = defview.SourceCollection
                        Dim fDidScroll = False
                        For Each alloc In tvitm._memNode._hctrList.Take(5)
                            For Each detitm In srcColl
                                Dim tmp = HeapAllocationContainer.CreateFrom(detitm)
                                If tmp IsNot Nothing AndAlso tmp.AllocationStruct.SeqNo = alloc.AllocationStruct.SeqNo Then
                                    blist.SelectedItems.Add(detitm)
                                    If Not fDidScroll Then
                                        fDidScroll = True
                                        blist.ScrollIntoView(detitm)
                                    End If

                                End If
                            Next
                        Next
                        _bmem._TabItemDetails.Focus()
                    Case "_Waste"

                        DoShowAllocationsWithWaste("Wasted allocations " + tvitm.ToString, tvitm._memNode._hctrList)
                    Case "_References"
                        For Each hc In tvitm._memNode._hctrList.Take(10)
                            If (hc.TBlk.BlockType = BlockTypes.ClrObject) Then
                                TVObjRefPanel.CreateObjectReferenceDataSurface(hc, hc.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False) + " " + hc.ToString)
                            Else
                                Dim results = GetReferences(hc.GetAddr, hc.SpyHeapPtr.TakeMemSnapshot(fEnableFilter:=False).Allocs)
                                ShowSubSnapShot(results, String.Format("References to {0:x8}", hc.GetAddr.ToInt32))
                            End If
                        Next

                    Case "Show stacks in _Notepad", "Show stacks & _Mem dump in Notepad"
                        Dim fIncludeMemdump = vrb = "Show stacks & _Mem dump in Notepad"
                        Dim sbuilder = New Text.StringBuilder
                        Dim nCnt = 0
                        Dim enumSource As New List(Of HeapAllocationContainer)
                        If Me._SelectedItems.Count > 0 Then
                            sbuilder.AppendLine("Stack Dump Selected Items")
                            For Each n In Me._SelectedItems
                                Dim tvitem = CType(n, StackAggTViewItem)
                                enumSource.AddRange(tvitem._memNode._hctrList)
                            Next
                        Else
                            sbuilder.AppendLine("Stack Dump " + tvitm.Header.ToString)
                            enumSource = tvitm._memNode._hctrList
                        End If
                        sbuilder.AppendLine()
                        For Each node In enumSource
                            Dim prStacksAndDump = GetStacksAndDump(node, nMaxDumpSize:=If(fIncludeMemdump, 65536, 0))
                            Dim sframes = prStacksAndDump.Key
                            sbuilder.Append(sframes)
                            sbuilder.AppendLine()
                            sbuilder.AppendLine()
                            If fIncludeMemdump Then
                                Dim strAddrDump = prStacksAndDump.Value
                                sbuilder.Append(strAddrDump)
                                sbuilder.AppendLine()
                                sbuilder.AppendLine()
                            End If
                            nCnt += 1
                            If nCnt = 20 Then
                                Exit For
                            End If
                        Next
                        WriteOutputToTempFile(sbuilder.ToString)
                    Case "Export to _PerfView"
                        PerfViewExport.Export(tvitm._memNode._hctrList, outputFileName:=String.Empty)
                    Case "_Assert for this StackFrame"
                        If _ConnectionMode = MemSpectMode.OnLine Then
                            Dim addr = tvitm._memNode._addrCallStkAtThisDepth
                            SendMsg(ProcMsgVerb.AssertStackFrame, fSendEndMsgSync:=True, dwords:={0, addr.ToInt32}) ' add
                        End If
                    Case "_Pivot"
                        Dim listToPivot As List(Of HeapAllocationContainer) = Nothing
                        Dim pivdesc = """" + tvitm.ToString + """"
                        If _bmem.IsPivotLimited Then
                            listToPivot = New List(Of HeapAllocationContainer)
                            For Each itm In _qdetails
                                Dim hctr = HeapAllocationContainer.CreateFrom(itm)
                                If hctr Is Nothing Then
                                    Continue For
                                End If
                                If hctr.AllocationStruct.Address <> IntPtr.Zero Then
                                    Dim stkframes = hctr.GetCallStackAddressestoArray
                                    If stkframes.Contains(tvitm._memNode._addrCallStkAtThisDepth) Then
                                        listToPivot.Add(hctr)
                                    End If
                                End If
                            Next
                            pivdesc = "Limited src " + pivdesc
                        End If

                        If Me._nRootAddress = IntPtr.Zero Then 'was normal stack: do normal pivot
                            pivdesc = "Pivot " + pivdesc
                            BrowseMem.Pivot(
                                tvitm._memNode._hctrList(0),
                                New IntPtr(tvitm._nDepth),
                                pivdesc,
                                listToPivot)
                        Else ' upside down
                            'Dim depthToUse = tvitm._nDepth - tvitm._memNode._nDepth - tvitm._nDepth
                            pivdesc = "Pivot upside down " + pivdesc
                            BrowseMem.Pivot(
                                Nothing,
                                tvitm._memNode._addrCallStkAtThisDepth,
                                pivdesc,
                                listToPivot)
                        End If
                    Case "_DumpChildren"
                        Dim sb As New Text.StringBuilder
                        sb.AppendLine("Children of """ + tvitm.ToString + """")
                        tvitm.DumpChildren(sb, 0)
                        WriteOutputToTempFile(sb.ToString)
                    Case "_Expand SubTree"
                        tvitm.ExpandAll()
                    Case "_Duplicates"
                        ShowDuplicateAllocations(Nothing, tvitm._memNode._hctrList, "Duplicates from " + tvitm.ToString)
                    Case "_Unused Members"
                        ShowUnusedMembers(tvitm._memNode._hctrList, "Unused Members from " + tvitm.ToString)
                    Case Else
                        MessageBox.Show("ClicKed: " + vrb)
                End Select
            End Sub

            Private Function AddTheChildren(
                                          ByVal lstSrc As IEnumerable,
                                          ByVal nDepth As Integer,
                                          ByVal listCtrl As ItemsControl,
                                          ByVal fRecalcMinMax As Boolean) As Tuple(Of Integer, UInteger)
                Dim sdict As New Dictionary(Of IntPtr, MemNode) ' stackaddr, MemNode (framename,list(of HeapAllocationContainer))
                Dim memNode As MemNode = Nothing
                Dim nRootAddress = _nRootAddress ' are we upside down?
                Dim nTotSize As UInteger = 0 ' virtallocs can accum > 2gig
                Dim nTotCnt As Integer = 0
                Dim saveShowStringContent = Common._ExpandStringContents
                Common._ExpandStringContents = False ' perf: don't need stringcontent
                Dim pblk As New ProcMemIntPtr
                Try
                    Dim _tdesc As ComponentModel.PropertyDescriptor = Nothing
                    For Each itm In lstSrc
                        Dim hctr = TryCast(itm, HeapAllocationContainer)
                        If hctr Is Nothing Then

                            'Dim tryhctr = itm.GetType.GetMember("_HeapAllocationContainer")
                            'If tryhctr.Length < 1 Then
                            '    Continue For
                            'End If
                            'hctr = CType(CType(tryhctr.GetValue(0), Reflection.PropertyInfo).GetValue(itm, Nothing), HeapAllocationContainer)
                            'If hctr Is Nothing Then
                            '    Continue For
                            'End If
                            If _tdesc Is Nothing Then
                                _tdesc = ComponentModel.TypeDescriptor.GetProperties(itm)("_HeapAllocationContainer")
                                If _tdesc Is Nothing Then
                                    Throw New InvalidOperationException("'_HeapAllocationContainer' was not defined as a field")
                                End If
                            End If
                            Dim tmp = _tdesc.GetValue(itm) ' if _tdesc is null, the "_HeapAllocationContainer" was not defined as a field
                            If tmp Is Nothing Then
                                Continue For
                            End If
                            hctr = CType(tmp, HeapAllocationContainer)
                        End If
                        If IsDoingUI Then
                            If fRecalcMinMax Then
                                If hctr.AllocationStruct.SeqNo > _TVStackAggPanel._WhenDistPanel._SeqNoMax Then
                                    _TVStackAggPanel._WhenDistPanel._SeqNoMax = hctr.AllocationStruct.SeqNo
                                End If
                                If hctr.AllocationStruct.SeqNo < _TVStackAggPanel._WhenDistPanel._SeqNoMin Then
                                    _TVStackAggPanel._WhenDistPanel._SeqNoMin = hctr.AllocationStruct.SeqNo
                                End If
                            Else
                                If _TVStackAggPanel._WhenDistPanel._WhenDistEnabled Then
                                    If hctr.AllocationStruct.SeqNo < _TVStackAggPanel._WhenDistPanel._SeqNoMin OrElse hctr.AllocationStruct.SeqNo > _TVStackAggPanel._WhenDistPanel._SeqNoMax Then
                                        Continue For
                                    End If
                                End If
                            End If
                        End If
                        Dim addrCallStkAtThisDepth As IntPtr
                        Dim baseNodeName = String.Empty
                        Dim nupside = 0
                        Dim stklevel = nDepth
                        If _isInverted Then
                            stklevel = hctr.AllocationStruct.m_uicStackAddr - nDepth - 1
                            If stklevel < 0 Then
                                Continue For
                            End If
                        End If
                        If nRootAddress <> IntPtr.Zero Then '  if we're doing an upsidedown stack (pivot)
                            If _ConnectionMode = MemSpectMode.Offline AndAlso hctr.HeapBlockPtr = IntPtr.Zero Then
                                pblk.data = _offlineSnapshot._allocationStacks.Item(hctr.AllocationStruct.Address)
                            Else
                                pblk.data = hctr.GetCallStackAddressestoArray ' works for IsUsingStackMemMap too
                            End If
                            For i = 0 To hctr.AllocationStruct.m_uicStackAddr - 1
                                If pblk.data(i) = nRootAddress Then
                                    nupside = i - nDepth
                                    If nupside < 0 Then
                                        Exit For
                                    End If
                                    addrCallStkAtThisDepth = hctr.GetCallStackAddr(nupside)
                                    Exit For
                                End If
                            Next
                        Else
                            addrCallStkAtThisDepth = hctr.GetCallStackAddr(stklevel)
                        End If
                        If addrCallStkAtThisDepth = IntPtr.Zero Then ' none found at this depth
                            If nDepth > 0 OrElse nRootAddress <> IntPtr.Zero Then 'no stk frame for this alloc at all (TrackingMode=Minimal), then we'll add it as unknown
                                Continue For ' next alloc
                            End If
                        End If
                        Dim nodeMemSize As UInteger = CUInt(hctr.GetSize)

                        nTotCnt += 1
                        nTotSize += nodeMemSize
                        If sdict.TryGetValue(addrCallStkAtThisDepth, memNode) Then
                            memNode._nTotSize += nodeMemSize
                            memNode._hctrList.Add(hctr)
                        Else
                            If nRootAddress <> IntPtr.Zero Then
                                stklevel = nupside
                            End If

                            If addrCallStkAtThisDepth = IntPtr.Zero Then
                                baseNodeName = "NoStackCollected(see Trackingmode)"
                            Else
                                baseNodeName = ResolveAddressToSymbol(addrCallStkAtThisDepth)
                            End If

                            memNode = New MemNode(addrCallStkAtThisDepth, hctr, baseNodeName)
                            memNode._nTotSize = nodeMemSize
                            sdict.Add(addrCallStkAtThisDepth, memNode)
                        End If
                    Next

                    If IsDoingUI Then

                        If fRecalcMinMax AndAlso nTotCnt > 0 AndAlso nTotSize = 0 Then ' like codemarkers and GC
                            If _TVStackAggPanel.GetSortOrder = TVSortOrder.Size Then
                                _TVStackAggPanel._btnSortCnt.IsChecked = True
                                _TVStackAggPanel._btnSortSize.IsEnabled = False
                            End If
                        End If
                    End If

                    Dim mylam = Function(mnode As MemNode) As Boolean
                                    Dim tvi = New StackAggTViewItem(nDepth + 1, mnode, Me)
                                    listCtrl.Items.Add(tvi)
                                    Return True
                                End Function
                    Dim sorder = TVSortOrder.Size
                    sorder = _TVStackAggPanel.GetSortOrder
                    Select Case sorder
                        Case TVSortOrder.Name
                            Dim lamSortAddr = Function(nameStackFrame As String) As String
                                                  'f:\dd\env\msenv\core\init.cpp(3582) : msenv.dll!OnMainWindowCreated + 893 bytes
                                                  'graphics\shared\util\utillib\memutils.cxx(216) : wpfgfx_v0400.dll!WPF::ProcessHeapImpl::Alloc + 28 bytes
                                                  'Microsoft.VisualStudio.Platform.WindowManagement.dll!Microsoft.VisualStudio.Platform.WindowManagement.WindowFrame.get_IsAutoVisibleTool
                                                  Dim res = nameStackFrame
                                                  Dim nSrcLineIndex = nameStackFrame.IndexOf(") :") ' if there's a source line
                                                  If nSrcLineIndex > 0 Then ' strip it off: next is DLL name
                                                      res = nameStackFrame.Substring(nSrcLineIndex + 3).Trim
                                                  End If
                                                  'nChar = nameStackFrame.IndexOf("!")
                                                  'If nChar > 0 Then
                                                  '    Return nameStackFrame.Substring(nChar + 1).Trim
                                                  'End If
                                                  Return res
                                              End Function
                            For Each itm In From a In sdict Order By lamSortAddr.Invoke(a.Value._baseName.ToLower)
                                If Not mylam(itm.Value) Then
                                    Exit For
                                End If
                            Next
                        Case TVSortOrder.Count
                            For Each itm In From a In sdict Order By a.Value._Cnt Descending
                                If Not mylam(itm.Value) Then
                                    Exit For
                                End If
                            Next
                        Case TVSortOrder.Size
                            For Each itm In From a In sdict Order By a.Value._nTotSize Descending, a.Value._baseName
                                If Not mylam(itm.Value) Then
                                    Exit For
                                End If
                            Next
                    End Select
                Catch ex As Exception
                    CommonUI.MemSpectExceptionHandler(ex)
                Finally
                    Common._ExpandStringContents = saveShowStringContent
                End Try
                Return New Tuple(Of Integer, UInteger)(nTotCnt, nTotSize)

            End Function

#If False Then
            recursion on mousemove
   at MemSpect.TVStackAggPanel.StackAggTreeView.on_MouseMove(Object sender, MouseEventArgs e)
   at System.Windows.Input.MouseEventArgs.InvokeEventHandler(Delegate genericHandler, Object genericTarget)
   at System.Windows.RoutedEventArgs.InvokeHandler(Delegate handler, Object target)
   at System.Windows.RoutedEventHandlerInfo.InvokeHandler(Object target, RoutedEventArgs routedEventArgs)
   at System.Windows.EventRoute.InvokeHandlersImpl(Object source, RoutedEventArgs args, Boolean reRaised)
   at System.Windows.UIElement.RaiseEventImpl(DependencyObject sender, RoutedEventArgs args)
   at System.Windows.UIElement.RaiseTrustedEvent(RoutedEventArgs args)
   at System.Windows.UIElement.RaiseEvent(RoutedEventArgs args, Boolean trusted)
   at System.Windows.Input.InputManager.ProcessStagingArea()
   at System.Windows.Input.InputManager.ProcessInput(InputEventArgs input)
   at System.Windows.Input.InputProviderSite.ReportInput(InputReport inputReport)
   at System.Windows.Interop.HwndMouseInputProvider.ReportInput(IntPtr hwnd, InputMode mode, Int32 timestamp, RawMouseActions actions, Int32 x, Int32 y, Int32 wheel)
   at System.Windows.Interop.HwndMouseInputProvider.FilterMessage(IntPtr hwnd, WindowMessage msg, IntPtr wParam, IntPtr lParam, Boolean& handled)
   at System.Windows.Interop.HwndSource.InputFilterMessage(IntPtr hwnd, Int32 msg, IntPtr wParam, IntPtr lParam, Boolean& handled)
   at MS.Win32.HwndWrapper.WndProc(IntPtr hwnd, Int32 msg, IntPtr wParam, IntPtr lParam, Boolean& handled)
   at MS.Win32.HwndSubclass.DispatcherCallbackOperation(Object o)
   at System.Windows.Threading.ExceptionWrapper.InternalRealCall(Delegate callback, Object args, Int32 numArgs)
   at MS.Internal.Threading.ExceptionFilterHelper.TryCatchWhen(Object source, Delegate method, Object args, Int32 numArgs, Delegate catchHandler)
   at System.Windows.Threading.Dispatcher.LegacyInvokeImpl(DispatcherPriority priority, TimeSpan timeout, Delegate method, Object args, Int32 numArgs)
   at MS.Win32.HwndSubclass.SubclassWndProc(IntPtr hwnd, Int32 msg, IntPtr wParam, IntPtr lParam)
   at MS.Win32.UnsafeNativeMethods.DispatchMessage(MSG& msg)
   at System.Windows.Threading.Dispatcher.PushFrameImpl(DispatcherFrame frame)
   at System.Windows.Threading.Dispatcher.PushFrame(DispatcherFrame frame)
   at System.Windows.Threading.DispatcherOperation.Wait(TimeSpan timeout)
   at System.Windows.Threading.Dispatcher.InvokeImpl(DispatcherOperation operation, CancellationToken cancellationToken, TimeSpan timeout)
   at System.Windows.Threading.Dispatcher.LegacyInvokeImpl(DispatcherPriority priority, TimeSpan timeout, Delegate method, Object args, Int32 numArgs)
   at MemSpect.CommonUI.CommonUIStatusMsgEventHandler(Object sender, StatusMessageEventArgs e)
   at MemSpect.Common.StatusMessageEventEventHandler.Invoke(Object sender, StatusMessageEventArgs e)
   at MemSpect.Common.UpdateStatusMsg(String msg, Boolean fAssert, StatusMessageType msgType, StatusMessagePriority msgPriority)
   at MemSpect.GCData.GetCLRObjectRefDict()
   at MemSpect.Common.HeapAllocationContainer.GetObjectRefData(NodeType brtypeRequest)
   at MemSpect.Common.HeapAllocationContainer.GetClrClassMemberLayout()
   at MemSpect.Common.HeapAllocationContainer.GetDisplayData(Boolean fVerbose)
   at MemSpect.Common.GetStacksAndDump(Object cntext, Int32 nMaxDumpSize)
   at MemSpect.CommonUI.GetAddressToolTip(Object dataContext)
   at MemSpect.TVStackAggPanel.StackAggTreeView.on_MouseMove(Object sender, MouseEventArgs e)

#End If
            Private _IsInMouseMove As Boolean
            Friend Sub on_MouseMove(ByVal sender As Object, ByVal e As MouseEventArgs) Handles Me.MouseMove
                If Not _IsInMouseMove Then
                    _IsInMouseMove = True
                    Try
                        Dim tb = TryCast(e.OriginalSource, TextBlock)
                        If tb IsNot Nothing AndAlso tb.Tag IsNot Nothing Then
                            Dim tvitem = GetAncestor(Of StackAggTViewItem)(tb)
                            If _LastTipObj IsNot Nothing AndAlso _LastTipObj.ToolTip IsNot Nothing Then
                                If tb Is _LastTipObj Then
                                    Return ' same obj, don't recalc
                                End If
                                ClearPriorTVToolTipIfAny()
                            End If
                            Dim sptip = GetAddressToolTip(tvitem._memNode._hctrList.FirstOrDefault)
                            sptip.Children.Insert(0, New TextBlock With {.Text = tvitem.ToString})
                            sptip.Children.Insert(1, New TextBlock With {.Text =
                                                  String.Format("# of items = {0:n0}  Size = {1:n0}  Stack for 1st item:", tvitem._memNode._Cnt, tvitem._memNode._nTotSize)})
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
                    Finally
                        _IsInMouseMove = False
                    End Try
                End If

            End Sub


            Public Class MemNode
                Inherits MemNodeBase
                Public _nTotSize As UInteger
                Sub New(ByVal addrCallStkAtThisDepth As IntPtr, ByVal hctr As HeapAllocationContainer, ByVal baseName As String)
                    MyBase.New(addrCallStkAtThisDepth, hctr, baseName)
                End Sub
            End Class

            <DebuggerDisplay("{ToString()}")>
            Public Class StackAggTViewItem
                Inherits MyTreeViewItem
                '                Implements IMyTreeViewItem

                Public _nDepth As Integer
                Public _tvm As StackAggTreeView
                Public _memNode As MemNode
                Public _WhenDistTextControl As WhenDistTextBoxControl
                Sub New(ByVal nDepth As Integer,
                        ByVal mNode As MemNode,
                        ByVal tvm As StackAggTreeView
                        )
                    _nDepth = nDepth
                    _tvm = tvm
                    _memNode = mNode
                    If _tvm.IsDoingUI Then

                        Me.ItemContainerStyle = tvm.ItemContainerStyle
                        Dim spheader As New StackPanel With {.Orientation = Orientation.Horizontal}
                        If Me._tvm._TVStackAggPanel._WhenDistPanel._WhenDistEnabled Then
                            _WhenDistTextControl = New WhenDistTextBoxControl(_memNode._hctrList, _tvm._TVStackAggPanel._WhenDistPanel)
                            spheader.Children.Add(_WhenDistTextControl._txtBlk)
                        End If

                        spheader.Children.Add(New TextBlock With {
                                              .Text = mNode._Cnt.ToString("n0") + " "
                                          }
                                      )

                        spheader.Children.Add(New TextBlock With {
                                              .Text = mNode._nTotSize.ToString("n0") + " ",
                                              .Foreground = Brushes.DarkCyan,
                                              .Tag = 1
                                          }
                                      ) ' tag indicates put tooltip here
                        spheader.Children.Add(New TextBlock With {
                                              .Text = mNode._baseName
                                          }
                                      )

                        'spheader.Children.Add(New TextBlock With {
                        '                      .Text = Me.FontFamily.ToString + Me.FontSize.ToString
                        '                      }
                        '                  )
                        Me.Header = spheader
                    End If

                    If mNode._Cnt > 0 Then ' if this item has children, add a dummy so the "+" appears
                        Me.Items.Add(True)
                    End If
                End Sub

                Function HasDummyChild() As Boolean
                    Dim res = False
                    If Me.Items.Count = 1 Then
                        If TryCast(Me.Items(0), StackAggTViewItem) Is Nothing Then
                            res = True
                        End If
                    End If
                    Return res
                End Function
                Sub OnExpand(ByVal o As Object, ByVal e As RoutedEventArgs) Handles Me.Expanded
                    _tvm.ClearPriorTVToolTipIfAny()
                    If HasDummyChild() Then
                        If Not _tvm.IsDoingUI Then
                            Me.Items.Clear() ' remove dummy node
                            _tvm.AddTheChildren(_memNode._hctrList, _nDepth, listCtrl:=Me, fRecalcMinMax:=False)
                        Else
                            Using New DataWindowMain.SetCursorWait
                                Me.Items.Clear() ' remove dummy node
                                _tvm.AddTheChildren(_memNode._hctrList, _nDepth, listCtrl:=Me, fRecalcMinMax:=False)
                            End Using
                        End If
                    End If
                    e.Handled = True
                End Sub

                Sub on_Selected(ByVal o As Object, ByVal e As RoutedEventArgs) Handles Me.Selected
                    If HasDummyChild() Then
                        Me.OnExpand(o, e)
                    End If
                    If _tvm.IsDoingUI Then
                        Me.SelectedItemChanged(o, e, _tvm._SelectedItems)
                    End If
                    e.Handled = True
                End Sub

                Public Overrides Function ToString() As String
                    Dim whenstr = If(_WhenDistTextControl Is Nothing, String.Empty, _WhenDistTextControl._txtBlk.Text + " ")
                    Dim retval = whenstr
                    retval += String.Format("{0} {1:n0} {2}", _memNode._Cnt, _memNode._nTotSize, _memNode._baseName)
                    Return retval
                End Function


            End Class ' StackAggTViewItem

            ''' <summary>
            ''' from root, expand path to a particular HeapAllocationContainer
            ''' </summary>
            ''' <param name="hctr"></param>
            ''' <param name="frmToSelect"></param>
            ''' <remarks></remarks>
            Friend Sub ExpandToItem(ByVal hctr As HeapAllocationContainer, Optional ByVal frmToSelect As String = "")
                If _nRootAddress <> IntPtr.Zero Then
                    UpdateStatusMsg("ExpandToItem doesn't work for pivots")
                Else
                    Dim stkAddrs As System.Collections.Generic.IEnumerable(Of IntPtr)
                    stkAddrs = hctr.GetCallStackAddressestoArray
                    If Me._isInverted Then
                        stkAddrs = stkAddrs.Reverse()
                    End If
                    Dim curItems = Me.Items
                    Dim bingo As StackAggTViewItem = Nothing
                    For Each FrameAddr In stkAddrs
                        Dim frm = StackFrameDictionary(FrameAddr)
                        For Each itm As StackAggTViewItem In curItems
                            If itm._memNode._baseName = frm Then
                                bingo = itm
                                itm.IsExpanded = True
                                itm.IsSelected = True
                                itm.BringIntoView()
                                itm.Focus()

                                ' itm.SetValue(MyTreeViewItem._IsMySelectedProperty, True)
                                Exit For
                            End If
                        Next
                        If frmToSelect = frm Then
                            Exit For
                        End If
                        If bingo Is Nothing Then
                            Exit For
                        End If
                        curItems = bingo.Items

                    Next
                End If
            End Sub
        End Class ' MyTreeView
    End Class 'TVPanel

    Public MustInherit Class SortableTVPanel
        Inherits DockPanel
        Friend _srchPanel As TreeSearchPanel
        Friend _WhenDistPanel As WhenDistPanel
        Friend WithEvents _btnSortName As RadioButton
        Friend WithEvents _btnSortSize As RadioButton
        Friend WithEvents _btnSortCnt As RadioButton
        Friend WithEvents _chkInvertStack As CheckBox
        Public Shared g_InitInvertStack As Boolean = False
        Protected Sub New()
            Dim dpbtnSP As New StackPanel With {.Orientation = Orientation.Horizontal}

            _btnSortName = New RadioButton With {
                .Content = "Sort Name   ",
                .ToolTip = "Sorts a node's immediate children by module name (everything after the ':')"
                }
            _btnSortSize = New RadioButton With {
                .Content = "Sort Size  ",
                .ToolTip = "Sorts a node's immediate children by Size of allocation",
                .IsChecked = True
                }
            _btnSortCnt = New RadioButton With {
                .Content = "Sort Count",
                .ToolTip = "Sorts a node's immediate children by number of allocations"
                }
            _chkInvertStack = New CheckBox With {
                .Content = "Invert Stacks",
                .ToolTip = "When checked, the root node will be the first stack frame for the thread, with descendents being subsequent frames" + vbCrLf +
                "When unchecked, the tree root will be an intercepted call entry into MemSpect, with the subsequent descendent frames",
                .IsChecked = g_InitInvertStack,
                .Margin = New Thickness(20, 0, 0, 0)
            }

            dpbtnSP.Children.Add(_btnSortName)
            dpbtnSP.Children.Add(_btnSortSize)
            dpbtnSP.Children.Add(_btnSortCnt)
            dpbtnSP.Children.Add(_chkInvertStack)

            Dim bord As New Border With {
                .Height = 25
            }
            Dim dpHeader As New DockPanel
            dpHeader.Children.Add(dpbtnSP)

            _WhenDistPanel = New WhenDistPanel(Me)
            dpHeader.Children.Add(_WhenDistPanel)

            _srchPanel = New TreeSearchPanel(Me)
            dpHeader.Children.Add(_srchPanel)

            '            Me.ToolTip = "Each Treeview Item shows Allocation Cnt followed by cum size, then name"
            bord.Child = dpHeader
            DockPanel.SetDock(bord, Dock.Top)
            Me.Children.Add(bord)
        End Sub
        Friend Function GetSortOrder() As TVSortOrder
            Dim retSortOrder = TVSortOrder.Size
            If _btnSortCnt.IsChecked Then
                retSortOrder = TVSortOrder.Count
            ElseIf _btnSortName.IsChecked Then
                retSortOrder = TVSortOrder.Name
            End If
            Return retSortOrder
        End Function

        Friend MustOverride Sub on_SortNameSizeCnt(ByVal sender As Object, ByVal e As RoutedEventArgs) Handles _
                _btnSortCnt.Checked,
                _btnSortSize.Checked,
                _btnSortName.Checked


        Friend Overridable Function SearchTreeItems(ByVal srchstring As String, ByVal fMakeSubSnapshot As Boolean) As Boolean
            Return False
        End Function


        Friend Class TreeSearchPanel
            Inherits DockPanel

            Sub New(ByVal tv As SortableTVPanel)
                Me.HorizontalAlignment = Windows.HorizontalAlignment.Right
                Dim txtSearch As New TextBox With {
                    .Width = 200,
                    .Text = Browse.g_LastStringFilterContent,
                    .ToolTip = "search call stacks for string. '!' first means NOT"
                }
                Dim btnSearch As New Button With {.Content = "TreeSearch",
                                                  .ToolTip = "Search tree items"
                                                 }

                Dim spSearch As New StackPanel With {.Orientation = Orientation.Horizontal}
                spSearch.Children.Add(New Label With {
                                      .Content = "String search"
                                  })

                Dim spResultType As New StackPanel With {.Orientation = Orientation.Vertical}
                Dim rbtnFindFirst = New RadioButton With {
                    .Content = "Find an occurrence",
                    .ToolTip = "find the first occurrence of search string in the treeview, regardless of order",
                    .IsChecked = True,
                    .FontSize = 8
                }
                Dim rbtnMakeSubSnapshot = New RadioButton With {
                    .Content = "SubSnap results",
                    .ToolTip = "Find all occurrences, make a subsnapshot",
                    .FontSize = 8
                }
                spResultType.Children.Add(rbtnFindFirst)
                spResultType.Children.Add(rbtnMakeSubSnapshot)

                spSearch.Children.Add(txtSearch)
                spSearch.Children.Add(spResultType)
                spSearch.Children.Add(btnSearch)
                Me.Children.Add(spSearch)

                Dim lamSearch = Sub()
                                    DataWindowMain._DataWindowMain.Cursor = Cursors.Wait
                                    If Not tv.SearchTreeItems(txtSearch.Text.Trim, fMakeSubSnapshot:=rbtnMakeSubSnapshot.IsChecked.GetValueOrDefault) Then
                                        UpdateStatusMsg(txtSearch.Text.Trim + " not found")
                                    End If
                                    DataWindowMain._DataWindowMain.Cursor = Cursors.Arrow
                                End Sub

                AddHandler txtSearch.KeyUp, Sub(sender As Object, e As KeyEventArgs)
                                                If e.Key = Key.Enter Then
                                                    lamSearch.Invoke()
                                                End If
                                            End Sub

                AddHandler btnSearch.Click, Sub()
                                                lamSearch.Invoke()
                                            End Sub

            End Sub

        End Class

    End Class


End Namespace
