Imports System.ComponentModel

Namespace MemSpect
    Public Enum MemRegionType
        Unused ' unused memory
        Used
        Pinned
        CustomColor
    End Enum

    Public Class MemRegion
        Friend start As ULong
        Friend Length As ULong
        Friend RegionType As MemRegionType
        Friend SpaceBetween As ULong
        Friend lst As New List(Of IMemoryBlock)
        Friend brush As Brush ' can be null: ColorFromType
        Public Function ColorFromType() As Brush
            Dim retBrush As Brush = Brushes.White
            If brush IsNot Nothing Then
                retBrush = brush
            Else
                Select Case RegionType
                    Case MemRegionType.Unused
                        retBrush = Brushes.Blue
                    Case MemRegionType.Used
                        retBrush = Brushes.Green
                    Case MemRegionType.Pinned
                        retBrush = Brushes.Red
                End Select
            End If
            Return retBrush
        End Function

        Public Function GetTip(ByVal bgw As BackgroundWorker) As String
            Dim retval = String.Format("Addr Range {0:x8}-{1:x8}  Size={2:n0}", start, start + Length, Length)
            retval += vbCrLf + If(RegionType = MemRegionType.Pinned, "Pinned ", String.Empty)
            Dim numToShowInTip = 20
            For Each itm In lst.Take(numToShowInTip)
                If bgw IsNot Nothing AndAlso bgw.CancellationPending Then
                    Exit For
                End If
                If TypeOf (itm) Is VMData Then
                    Dim vmd = CType(itm, VMData)
                    If vmd.bgColorClass.colorBrush.Color = Brushes.White.Color Then
                        retval += vbCrLf + "Unused"
                    Else
                        retval += vbCrLf + String.Format("{0:x8} {1:x8} {2}", vmd.mbi.BaseAddress.ToInt32, vmd.mbi.AllocationBase.ToInt32, vmd.strData)
                    End If
                    If lst.Count = 1 Then
                        Dim stk = VirtualMem.GetCallStackForMBI(vmd.mbi)
                        If Not String.IsNullOrEmpty(stk) Then
                            retval += vbCrLf + stk
                        End If
                    End If
                Else
                    If TypeOf (itm) Is HeapAllocationContainer Then
                        Dim hctr = CType(itm, HeapAllocationContainer)
                        If lst.Count = 1 Then
                            Dim stksAndDump = GetStacksAndDump(hctr)
                            retval += vbCrLf + stksAndDump.Key
                        Else
                            retval += vbCrLf + itm.ToString
                            If hctr.TBlkBlockType = BlockTypes.ClrObject Then
                                retval += " " + hctr.GetClassNameFromHeapCtr(False)
                            Else
                            End If
                        End If
                    End If
                End If
            Next
            If lst.Count > numToShowInTip Then
                retval += vbCrLf + "..."
            End If
            Return retval
        End Function

        Public Overrides Function ToString() As String
            Dim cname = String.Empty
            If lst.Count > 0 Then
                Dim first = lst(0)
                If first.GetType Is GetType(HeapAllocationContainer) Then
                    cname = CType(first, HeapAllocationContainer).GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False)
                End If
            End If
            Return String.Format("{0:x8} {1,8} {2,8} {3,8} {4} {5} {6}", start, Length, SpaceBetween, lst.Count, If(brush Is Nothing, "", brush.ToString), RegionType, cname)
        End Function
    End Class

    Public Class MemoryRegionGraphContainer
        Inherits ContentControl
        Friend _RegionGraph As MemoryRegionGraph
        Friend _dp As New DockPanel
        Friend _sp As StackPanel
        Friend _txtStat As TextBlock
        Friend _chkDispData As CheckBox
        Public Shared Function CreateMemoryRegionGraphContainerFromHctr(
                                                                       ByVal allocs As IEnumerable(Of IMemoryBlock),
                                                                       ByVal _trkType As TrkType
                                                                       ) As MemoryRegionGraphContainer
            Dim srtedPinnned = New SortedDictionary(Of ULong, HeapAllocationContainer)()
            ' first we see if we're dealing with CLR, and if so, see if there are pinned regions
            If _trkType = TrkType.ClrObjects Then
                For Each rt In GCData.GetGCRootInfo(fIncludeDupes:=False).Where(Function(h) h.GetGCRootFlags.Contains("Pinning"))
                    srtedPinnned.Add(rt.GetAddr.MyToULong, rt)
                Next
                ' now find any overlapped and their pinned members
                Dim overlapped = srtedPinnned.Values.Where(Function(h) h.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False) = "System.Threading.OverlappedData")

                For Each alloc In overlapped.ToList ' tolist so can modify in loop
                    ' now get the ref'd user object
                    Dim orefDataLst = alloc.GetObjectRefData(NodeType.RefFromParent)
                    Dim clsLayouts = ClrClassInfo.GetClassLayoutsFromClassId(alloc.GetClassId)
                    For Each layout In clsLayouts

                        For Each mem In layout.dictFieldInfo
                            If mem.Value.FldName = "m_userObject" Then 'm_isArray
                                Dim offset = mem.Key
                                Dim offsetAddr = alloc.GetAddr.MyAdd(offset)
                                Dim dwValueLong As Long = ReadProcessMemoryDWORDEx(offsetAddr)

                                Dim dwValue As Integer = Convert.ToInt32(dwValueLong)
                                If dwValue <> 0 Then
                                    Dim memUserObj = orefDataLst(0).Where(Function(h) h.GetAddr.ToInt32 = dwValue).Single
                                    Dim kk = memUserObj.GetAddr.MyToULong
                                    If Not srtedPinnned.ContainsKey(kk) Then
                                        srtedPinnned.Add(memUserObj.GetAddr.MyToULong, memUserObj)
                                    End If
                                End If
                            End If
                        Next
                    Next
                Next
            End If


            ' now that we have a pinned lookup, create the regions
            Dim retval = CreateRegionGraphContainerFromMemoryBlocks(allocs, minSpaceBetween:=4, srtedPinned:=srtedPinnned)
            Return retval
        End Function

        Public Shared Function CreateRegionGraphContainerFromMemoryBlocks(
                                                         ByVal allocs As IEnumerable(Of IMemoryBlock),
                                                         ByVal minSpaceBetween As Integer,
                                                         ByVal srtedPinned As SortedDictionary(Of ULong, HeapAllocationContainer)
                                                         ) As MemoryRegionGraphContainer
            Dim regions = New SortedList(Of ULong, MemRegion)()
            Dim nextallocGuess As ULong = 0
            Dim priorIsPinned = False
            Dim priorBrush As SolidColorBrush = Nothing
            Dim curRange As MemRegion = Nothing
            Dim fNeedNewRange = True
            Dim lstAllocs = allocs.ToList.OrderBy(Function(h) h.Address)
            For Each alloc In lstAllocs

                Dim curaddr = alloc.Address
                Dim curSize = alloc.Size
                Dim curBrush As SolidColorBrush = Nothing
                If TypeOf (alloc) Is VMData Then
                    curBrush = CType(alloc, VMData).bgColorClass.colorBrush
                    If priorBrush IsNot Nothing Then
                        If curBrush IsNot Nothing Then
                            If curBrush.Color <> priorBrush.Color Then
                                fNeedNewRange = True
                            End If
                        End If
                    End If
                End If

                priorBrush = curBrush ' could be null

                Dim curIsPinned = False
                If srtedPinned IsNot Nothing AndAlso srtedPinned.ContainsKey(curaddr) Then
                    curIsPinned = True
                End If
                If curIsPinned Xor priorIsPinned Then
                    fNeedNewRange = True ' change in pinning 
                End If

                Dim spaceBetween = curaddr - nextallocGuess ' sometimes 2 bytes, which is treated as adjacent
                If spaceBetween >= minSpaceBetween Then
                    fNeedNewRange = True
                End If
                If fNeedNewRange Then
                    If curRange IsNot Nothing Then ' if there's a prior one, see if we need an empty one for space
                        If spaceBetween >= 4 Then
                            '                            regions.Add(nextallocGuess, New MemRegion With {.start = nextallocGuess, .length = spaceBetween, .regionType = MemRegionType.Unused})
                        End If
                    End If
                    curRange = New MemRegion With {
                        .start = curaddr,
                        .Length = curSize,
                        .brush = curBrush,
                        .SpaceBetween = spaceBetween,
                        .RegionType = MemRegionType.Used}
                    regions.Add(curaddr, curRange)
                    fNeedNewRange = False
                Else ' falls in currange
                    curRange.Length += curSize + spaceBetween
                End If
                If curIsPinned Then
                    curRange.RegionType = MemRegionType.Pinned
                End If
                priorIsPinned = curIsPinned
                curRange.lst.Add(alloc)
                nextallocGuess = curaddr + alloc.Size
            Next
            Dim x = New MemoryRegionGraphContainer(regions, startAddr:=0, endAddr:=0)
            Return x
        End Function

        Public Sub New(ByVal regions As SortedList(Of ULong, MemRegion), ByVal startAddr As ULong, ByVal endAddr As ULong)
            _RegionGraph = New MemoryRegionGraph(regions, startAddr, endAddr, Me)
            _sp = New StackPanel With {.Orientation = Orientation.Horizontal}
            _chkDispData = New CheckBox With {
                .Content = "Show Data ",
                .ToolTip = "Show raw data underlying graph. Mouse wheel to zoom in/out. Zooming is binary: zoom into top or bottom half. Zoom out goes to prior zoom in zoom stack" + vbCrLf +
                "'I' and 'O' zoom in/out as well (shift to indicate bottom half for zoom in). Ensure control has focus for keystrokes."
            }
            _sp.Children.Add(_chkDispData)
            Dim chkDispDataHandler = New RoutedEventHandler(
                    Sub(o As Object, e As RoutedEventArgs)
                        _RegionGraph.ClosePriorTipIfAny()
                        _dp.Children.Clear()
                        _dp.Children.Add(_sp)
                        If _chkDispData.IsChecked Then
                            Dim q = From a In regions.Values
                                    Select Addr = a.start.ToString("x8"),
                                    a.Length,
                                    NumAllocs = a.lst.Count,
                                    a.SpaceBetween,
                                    RegionType = a.RegionType.ToString,
                                    Region = a.ToString

                            Dim br = New Browse(q,
                                                fAllowBrowFilter:=True,
                                                ColWidths:={WIDTH_ADDRESS, WIDTH_ADDRESS, 100, 100, 100, 900, 900},
                                                ColTips:={"Address of region", "Size of region in bytes", "# Allocs in the region", "Space between regions"},
                                                InitialSortOrder:=New BrowseInitialSortOrder() With {.ColumnNo = 1, .direction = ComponentModel.ListSortDirection.Ascending},
                                                arrColumnsToTotal:={"Length", "SpaceBetween", "NumAllocs"}
                                                )
                            _dp.Children.Add(br)
                        Else
                            _dp.Children.Add(_RegionGraph)
                        End If
                    End Sub)
            _chkDispData.AddHandler(CheckBox.CheckedEvent, chkDispDataHandler)
            _chkDispData.AddHandler(CheckBox.UncheckedEvent, chkDispDataHandler)

            _txtStat = New TextBlock With {.Text = "# regions =  " + regions.Count.ToString}
            _sp.Children.Add(_txtStat)

            DockPanel.SetDock(_sp, Dock.Top)
            _dp.Children.Add(_sp)
            _dp.Children.Add(_RegionGraph)
            Me.Content = _dp
        End Sub

        Sub mousebutton(ByVal o As Object, ByVal e As MouseEventArgs) Handles Me.MouseLeftButtonUp, Me.MouseRightButtonUp
            Me.Focus()
        End Sub

        Sub MemoryRegionCntr_SizeChanged(ByVal o As Object, ByVal e As SizeChangedEventArgs) Handles Me.SizeChanged
            _RegionGraph.RaiseEvent(e)
        End Sub
        Protected Overrides Sub OnMouseWheel(ByVal e As MouseWheelEventArgs)
            MyBase.OnMouseWheel(e)
        End Sub

        Sub on_mousewheelevent(ByVal o As Object, ByVal e As MouseWheelEventArgs) Handles Me.MouseWheel

        End Sub
        Sub keystroke(ByVal o As Object, ByVal e As KeyEventArgs) Handles Me.PreviewKeyUp
            'UpdateStatusMsg(e.Key.ToString + " " + e.KeyboardDevice.Modifiers.ToString)
            If Not _chkDispData.IsChecked Then
                Select Case e.Key
                    Case Key.Up, Key.I ' in/out  or up/down
                        If e.KeyboardDevice.Modifiers = ModifierKeys.None Then
                            If TryZoomIn(fFirstHalf:=True) Then
                                e.Handled = True
                            End If
                        Else
                            If TryZoomIn(fFirstHalf:=False) Then
                                e.Handled = True
                            End If
                        End If
                    Case Key.Down, Key.O
                        If TryZoomOut() Then

                        End If
                        e.Handled = True
                End Select
            End If
        End Sub

        Function TryZoomIn(ByVal fFirstHalf As Boolean) As Boolean
            Dim fDoit = False
            If _RegionGraph._zoom.Count < 20 Then
                Dim newStart As ULong
                Dim newEnd As ULong
                Dim midway = _RegionGraph._AddrStart + CULng(_RegionGraph._mapAddrRangeLen / 2)
                If fFirstHalf Then
                    newStart = _RegionGraph._AddrStart
                    newEnd = midway
                Else
                    newStart = midway
                    newEnd = _RegionGraph._AddrEnd
                End If
                _RegionGraph._zoom.Insert(0, New Tuple(Of ULong, ULong)(_RegionGraph._AddrStart, _RegionGraph._AddrEnd))
                fDoit = True
                ZoomHelper(newStart, newEnd)
            End If
            Return fDoit
        End Function

        Function TryZoomOut() As Boolean
            Dim fDoit = False
            If _RegionGraph._zoom.Count > 0 Then
                Dim newStart = _RegionGraph._zoom(0).Item1
                Dim newEnd = _RegionGraph._zoom(0).Item2
                _RegionGraph._zoom.RemoveAt(0)
                fDoit = True
                ZoomHelper(newStart, newEnd)
            End If
            Return fDoit
        End Function

        Sub ZoomHelper(ByVal newStart As ULong, ByVal newEnd As ULong)
            _RegionGraph.ClosePriorTipIfAny()
            Dim bgw = _RegionGraph._bgw ' read it once
            If bgw IsNot Nothing Then
                bgw.CancelAsync()
            End If
            _dp.Children.Remove(_RegionGraph)
            Dim w = _RegionGraph.Width
            Dim h = _RegionGraph.Height
            Dim zoom = _RegionGraph._zoom
            Dim savedrawingContext = _RegionGraph._MyDrawingContext ' for tests
            _RegionGraph = New MemoryRegionGraph(_RegionGraph._regions, newStart, newEnd, Me)
            _RegionGraph._bgw = bgw
            _RegionGraph.Width = w
            _RegionGraph.Height = h
            _RegionGraph._zoom = zoom
            _RegionGraph._MyDrawingContext = savedrawingContext

            _dp.Children.Add(_RegionGraph)
            _RegionGraph.Redraw()
        End Sub

        Sub on_previewmousewheelevent(ByVal o As Object, ByVal e As MouseWheelEventArgs) Handles Me.PreviewMouseWheel
            If Not _chkDispData.IsChecked Then
                Dim bgw = _RegionGraph._bgw
                If bgw Is Nothing OrElse Not bgw.IsBusy Then
                    If e.Delta > 0 Then
                        Dim pos = e.GetPosition(Me)
                        If _IsUnderTest Then
                            pos = New Point(1, 1)
                        End If
                        Dim addrClicked = _RegionGraph.ConvertPointToAddr(pos)
                        Dim fFirstHalf = True
                        Dim midway = _RegionGraph._AddrStart + CULng(_RegionGraph._mapAddrRangeLen / 2)

                        If addrClicked > midway Then
                            fFirstHalf = False
                        End If
                        If TryZoomIn(fFirstHalf) Then
                            e.Handled = True
                        End If
                    Else
                        If TryZoomOut() Then
                            e.Handled = True
                        End If
                    End If
                End If
            End If
        End Sub

        Public Sub RefreshRegionStatus()
            _txtStat.Text = String.Format("    AddrStart={0:x8} AddrEnd={1:x8} Length={2:x8} #Regions={3}   ZoomLevel= 2^{4}",
                             _RegionGraph._AddrStart,
                             _RegionGraph._AddrEnd,
                             _RegionGraph._AddrEnd - _RegionGraph._AddrStart,
                             _RegionGraph._regions.Count,
                             _RegionGraph._zoom.Count
                             )
        End Sub

    End Class

    Public Class MemoryRegionGraph
        Inherits FrameworkElement
        Private _penBorder As New Pen(Brushes.Black, 1)

        Friend _regions As SortedList(Of ULong, MemRegion)
        Friend _AddrStart As ULong
        Friend _AddrEnd As ULong
        Friend _mapAddrRangeLen As ULong
        Friend _mapDispLen As ULong
        Private _MapOffset As Point
        Private _MapSize As Size ' 
        Friend _MemoryRegionContainer As MemoryRegionGraphContainer

        Public _zoom As New List(Of Tuple(Of ULong, ULong)) ' start, end pairs

        Public Sub New(ByVal regions As SortedList(Of ULong, MemRegion), ByVal startAddr As ULong, ByVal endAddr As ULong, ByVal memoryRegionContainer As MemoryRegionGraphContainer)
            _regions = regions
            _MemoryRegionContainer = memoryRegionContainer

            If _regions.Count > 0 Then
                If startAddr = 0 Then
                    _AddrStart = _regions.First.Key
                Else
                    _AddrStart = startAddr
                End If
                If endAddr = 0 Then
                    _AddrEnd = _regions.Last.Key
                Else
                    _AddrEnd = endAddr
                End If
            End If
            '            Me.Margin = New Windows.Thickness(2, 2, 2, 2)
        End Sub
        Public Function ConvertAddrToPoint(ByVal addr As ULong) As Point
            Dim relpos = (addr - _AddrStart) / _mapAddrRangeLen
            Debug.Assert(relpos >= 0 AndAlso relpos < 1, "relpos between 0 and 1? " + relpos.ToString)
            Dim ptPos = CInt(relpos * _mapDispLen)
            Dim y = ptPos \ CInt(_MapSize.Width)
            Dim x = ptPos - y * CInt(_MapSize.Width)
            Dim pt = New Point(x, y)
            Return pt
        End Function

        Public Function ConvertPointToAddr(ByVal Pt As Point) As ULong
            Dim ptPos = Pt.Y * _MapSize.Width + Pt.X
            Dim relpos = ptPos / _mapDispLen
            Debug.Assert(relpos >= 0 AndAlso relpos < 1, "relpos should be between 0 and 1? " + relpos.ToString)

            Dim val = CULng(relpos * _mapAddrRangeLen) + _AddrStart
            Return val
        End Function

        Friend _MyDrawingContext As IDrawRectangle
        Friend Sub OnRenderhelper()
            If _regions.Count > 0 Then
                If Width > 3 AndAlso Height > 40 AndAlso _mapDispLen > 0 Then ' _mapDispLen set on SizeChanged
                    Dim rectBorder = New Rect(_MapOffset, _MapSize)
                    _MyDrawingContext.DrawRectangle(Brushes.White, _penBorder, rectBorder)

                    Dim res = FindNearest(Of ULong)(_regions.Keys, _AddrStart, Nothing, Nothing, Nothing)
                    Dim curRegNum = 0
                    If res(0) >= 0 AndAlso res(1) >= 0 Then
                        curRegNum = res(0)
                    Else
                        Debug.Assert(False, "how we get here")
                    End If

                    While curRegNum < _regions.Values.Count
                        Dim curReg = _regions.Values(curRegNum)
                        Dim curAddr = curReg.start
                        Dim endreg = curReg.start + curReg.Length
                        If endreg < _AddrEnd Then

                            If endreg >= _AddrStart Then
                                If curReg.RegionType <> MemRegionType.Unused Then
                                    Dim curBrush = curReg.ColorFromType

                                    Dim pt0 = ConvertAddrToPoint(Math.Max(curAddr, _AddrStart)) ' max because region could be huge and start before curAddr
                                    Dim pt1 = ConvertAddrToPoint(Math.Min(curAddr + curReg.Length, _AddrEnd))
                                    Dim rect As Rect
                                    Debug.Assert(Not (pt0.X < 0 OrElse pt1.X < 0 OrElse pt0.Y < 0 OrElse pt1.Y < 0), "invalid points")

                                    If pt0.Y = pt1.Y Then ' fits on single line

                                        Debug.Assert(pt1.X - pt0.X >= 0, "bad pointsx")
                                        rect = New Rect(pt0.X, pt0.Y, pt1.X - pt0.X, 1)
                                        _MyDrawingContext.DrawRectangle(curBrush, pen:=Nothing, rectangle:=rect)
                                    Else
                                        ' draw the partial to end of line
                                        rect = New Rect(pt0.X, pt0.Y, _MapSize.Width - pt0.X, 1)
                                        _MyDrawingContext.DrawRectangle(curBrush, pen:=Nothing, rectangle:=rect)
                                        ' draw the middle block of contiguous whole lines
                                        Debug.Assert(pt1.Y - pt0.Y >= 0, "bad points")
                                        rect = New Rect(_MapOffset.X, pt0.Y + 1, _MapSize.Width, pt1.Y - pt0.Y)
                                        _MyDrawingContext.DrawRectangle(curBrush, pen:=Nothing, rectangle:=rect)
                                        ' draw the last partial line
                                        rect = New Rect(_MapOffset.X, pt1.Y, pt1.X, 1)
                                        _MyDrawingContext.DrawRectangle(curBrush, pen:=Nothing, rectangle:=rect)

                                    End If
                                    'curReg.rect = rect
                                End If
                            End If
                        End If
                        curRegNum += 1
                    End While
                End If
            End If

        End Sub
        Protected Overrides Sub OnRender(ByVal drawingContext As System.Windows.Media.DrawingContext)
            MyBase.OnRender(drawingContext)
            Try
                If _MyDrawingContext Is Nothing Then
                    Dim mydrawingc = New MyDrawingContext(drawingContext)
                    _MyDrawingContext = mydrawingc
                Else
                    _MyDrawingContext.SetDrawingContext(drawingContext)
                End If
                OnRenderhelper()

            Catch ex As Exception

            End Try
        End Sub

        Public Sub Redraw()
            _MapOffset = New Point(2, 2)
            _MapSize = New Size(Width - _MapOffset.X * 4, Height - _MapOffset.Y * 14)
            _mapDispLen = CULng(_MapSize.Width * _MapSize.Height)
            _mapAddrRangeLen = _AddrEnd - _AddrStart
            Me.InvalidateVisual()
            _MemoryRegionContainer.RefreshRegionStatus()
            ClosePriorTipIfAny() ' async could have drawn one
        End Sub
        Private Sub MyControl_SizeChanged(ByVal o As Object, ByVal e As SizeChangedEventArgs) Handles Me.SizeChanged
            If e.NewSize.Width > 10 Then
                Width = e.NewSize.Width
            End If
            If e.NewSize.Height > 10 Then
                Height = e.NewSize.Height
            End If
            Redraw()
        End Sub
        Private Sub MyControl_OnMouseBtns(ByVal o As Object, ByVal e As MouseButtonEventArgs) Handles Me.MouseLeftButtonUp, Me.MouseRightButtonUp
        End Sub


        Friend _priortip As ToolTip = Nothing
        Public Sub ClosePriorTipIfAny()
            If _priortip IsNot Nothing Then
                If _priortip.IsOpen Then
                    _priortip.InvalidateVisual()
                    _priortip.IsOpen = False
                End If
                _priortip = Nothing
            End If
        End Sub
        Friend _bgw As BackgroundWorker = New BackgroundWorker With {.WorkerSupportsCancellation = True}
        Private Sub MyControl_OnMouseMove(ByVal o As Object, ByVal e As MouseEventArgs) Handles Me.MouseMove
            'UpdateStatusMsg(pos.ToString())
            Try
                Dim pos = e.GetPosition(Me)
                If pos.Y >= _MapSize.Height Then
                    ClosePriorTipIfAny()
                    Return
                End If
                If pos.X >= _MapSize.Width Then
                    ClosePriorTipIfAny()
                    Return
                End If
                If _bgw Is Nothing Then
                Else
                    If Not _bgw.CancellationPending Then
                        If _bgw.IsBusy Then
                            _bgw.CancelAsync()
                        End If
                    End If
                End If

                AddHandler _bgw.DoWork, Sub(od As Object, ed As DoWorkEventArgs)
                                            Dim addr = ConvertPointToAddr(pos)

                                            Dim res = FindNearest(Of ULong)(_regions.Keys, addr, Nothing, Nothing, Nothing)
                                            Dim curRegNum = 0
                                            Dim tiptext = String.Empty
                                            If res(0) >= 0 AndAlso res(1) >= 0 Then
                                                Dim region = _regions.Values(res(0))
                                                If addr < region.start + region.Length Then
                                                    tiptext = region.GetTip(_bgw)
                                                End If
                                            End If
                                            If String.IsNullOrEmpty(tiptext) Then
                                                tiptext = "Unused " + addr.ToString("x8")
                                            End If
                                            If Not _bgw.CancellationPending Then
                                                ed.Result = tiptext
                                            Else
                                                ed.Cancel = True
                                            End If

                                        End Sub

                AddHandler _bgw.RunWorkerCompleted, Sub(oc As Object, ec As RunWorkerCompletedEventArgs)
                                                        If ec.Cancelled OrElse ec.Result Is Nothing OrElse ec.Error IsNot Nothing Then
                                                        Else
                                                            Dim tiptext = CStr(ec.Result)
                                                            Dim tp = New ToolTip() With {
                                                                .Content = New MonospacefontTextBlockForToolTip With {.Text = tiptext}
                                                            }
                                                            ClosePriorTipIfAny()
                                                            _priortip = tp
                                                            'Me.ToolTip = tp
                                                            tp.IsOpen = True

                                                        End If

                                                    End Sub
                _bgw.RunWorkerAsync()

                'Dim addr = ConvertPointToAddr(pos)

                'Dim res = FindNearest(Of ULong)(_regions.Keys, addr, Nothing, Nothing, Nothing)
                'Dim curRegNum = 0
                'Dim tiptext = "Unused " + addr.ToString("x8")
                'If res(0) >= 0 AndAlso res(1) >= 0 Then
                '    Dim region = _regions.Values(res(0))
                '    If addr < region.start + region.Length Then
                '        tiptext = region.GetTip
                '    End If
                'End If
                'Dim tp = New ToolTip() With {
                '    .Content = New MonospacefontTextBlock With {.Text = tiptext},
                '    .Background = Brushes.LightYellow
                '}
                'ClosePriorTipIfAny()
                '_priortip = tp
                'Me.ToolTip = tp
                'tp.IsOpen = True

            Catch ex As Exception

            End Try

        End Sub
    End Class
    Public Interface IDrawRectangle
        Sub DrawRectangle(ByVal brush As Brush, ByVal pen As Pen, ByVal rectangle As Rect)
        Sub SetDrawingContext(ByVal context As DrawingContext)
    End Interface

    Public Class MyDrawingContext
        Implements IDrawRectangle


        Friend _drawingContext As DrawingContext
        Public Sub New(ByVal drawingContext As DrawingContext)
            _drawingContext = drawingContext
        End Sub

        Public Sub DrawRectangle(ByVal brush As System.Windows.Media.Brush, ByVal pen As System.Windows.Media.Pen, ByVal rectangle As System.Windows.Rect) Implements IDrawRectangle.DrawRectangle
            _drawingContext.DrawRectangle(brush, pen, rectangle)
        End Sub

        Public Sub SetDrawingContext(ByVal context As System.Windows.Media.DrawingContext) Implements IDrawRectangle.SetDrawingContext
            _drawingContext = context
        End Sub
    End Class
End Namespace
