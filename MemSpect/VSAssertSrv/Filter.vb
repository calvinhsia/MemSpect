
Imports System
Imports System.Diagnostics
Imports System.Windows
Imports System.Windows.Automation
Imports System.Windows.Controls
Imports System.Windows.Controls.Primitives
Imports System.Windows.Data
Imports System.Windows.Documents
Imports System.Windows.Ink
Imports System.Windows.Input
Imports System.Windows.Markup
Imports System.Windows.Media
Imports System.Windows.Media.Animation
Imports System.Windows.Media.Effects
Imports System.Windows.Media.Imaging
Imports System.Windows.Media.Media3D
Imports System.Windows.Media.TextFormatting
Imports System.Windows.Navigation
Imports System.Windows.Shapes
Imports System.Windows.Shell
Imports System.Linq
Imports System.Xml.Linq
Imports System.ComponentModel
Imports System.Threading
Imports System.Threading.Tasks

Namespace MemSpect
    Public Class Filter
        Inherits DockPanel
        Friend WithEvents Grid1 As System.Windows.Controls.Grid

        Friend WithEvents SeqNoHiTextBox As TxtBoxNumeric
        Friend WithEvents SeqNoLoTextBox As TxtBoxNumeric
        Friend WithEvents ThreadTextBox As System.Windows.Controls.TextBox
        Friend WithEvents SrchStrTextBox As System.Windows.Controls.TextBox
        Friend WithEvents LeakMultipleTextBox As System.Windows.Controls.TextBox
        Friend WithEvents rbtnKnownIssuesNone As System.Windows.Controls.RadioButton
        Friend WithEvents rbtnKnownIssuesInclude As System.Windows.Controls.RadioButton
        Friend WithEvents rbtnKnownIssuesExclude As System.Windows.Controls.RadioButton
        Friend WithEvents btnApply As System.Windows.Controls.Button
        Friend WithEvents btnClear As System.Windows.Controls.Button

        Friend ctrlFiltResults As StackPanel
        Friend btnSubSnapAll As Button
        Friend browTypesFoundRes As Browse
        Friend btnShowFoundStrsInBrowMem As Button
        Friend browFilteredHeapList As Browse
        Public _FilterData As GlobalFilter
        Private _VBDiagMarginBase As VBDiagMarginBase
        Sub New(ByVal filterData As GlobalFilter, ByVal Base As VBDiagMarginBase)
            _FilterData = filterData
            _VBDiagMarginBase = Base
            '            Me.Title = "Filter " + GetWindowTitle()
            'Me.Height = 900
            'Me.Width = 600
            Dim xaml =
        <Grid
            xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
            xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
            xmlns:l="clr-namespace:MemSpect;assembly=MemSpect"
            >
            <StackPanel Orientation="Vertical">
                <StackPanel Orientation="Vertical">
                    <Grid HorizontalAlignment="Left" Margin="20,0,0,0" Name="Grid1" VerticalAlignment="Top">
                        <Grid.RowDefinitions>
                            <RowDefinition Height="Auto"/>
                            <RowDefinition Height="Auto"/>
                            <RowDefinition Height="Auto"/>
                            <RowDefinition Height="Auto"/>
                            <RowDefinition Height="Auto"/>
                            <RowDefinition Height="Auto"/>
                        </Grid.RowDefinitions>
                        <Grid.ColumnDefinitions>
                            <ColumnDefinition Width="Auto"/>
                            <ColumnDefinition Width="Auto"/>
                            <ColumnDefinition Width="50"/>
                            <ColumnDefinition Width="Auto"/>
                            <ColumnDefinition Width="50"/>
                            <ColumnDefinition Width="Auto"/>
                            <ColumnDefinition Width="Auto"/>
                        </Grid.ColumnDefinitions>
                        <StackPanel Orientation="Vertical" Grid.Column="0" Grid.Row="0">
                            <StackPanel Orientation="Horizontal">
                                <Label Content="Seq No _Lo:" Width="100" HorizontalAlignment="Left" Margin="3" Target="{Binding ElementName=SeqNoLoTextBox}"/>
                                <l:TxtBoxNumeric Height="23" HorizontalAlignment="Left" Margin="3" Name="SeqNoLoTextBox"
                                    Text="{Binding Path=SeqNoLo, Mode=TwoWay, ValidatesOnExceptions=true, NotifyOnValidationError=true, Converter={l:NumericalConverter}}" Width="140"/>
                            </StackPanel>
                            <StackPanel Orientation="Horizontal">
                                <Label Content="Seq No _Hi:" Width="100" HorizontalAlignment="Left" Margin="3" Target="{Binding ElementName=SeqNoHiTextBox}"/>
                                <l:TxtBoxNumeric Height="23" HorizontalAlignment="Left" Margin="3" Name="SeqNoHiTextBox"
                                    Text="{Binding Path=SeqNoHi, Mode=TwoWay, ValidatesOnExceptions=true, NotifyOnValidationError=true, Converter={l:NumericalConverter}}" Width="140"/>
                            </StackPanel>
                            <StackPanel Orientation="Horizontal">
                                <Label Content="Thread:" Width="100" HorizontalAlignment="Left" Margin="3"/>
                                <TextBox Grid.Column="1" HorizontalAlignment="Left" Margin="3" Name="ThreadTextBox" Text="{Binding Path=Thread, Mode=TwoWay, ValidatesOnExceptions=true, NotifyOnValidationError=true}" Width="140"/>
                            </StackPanel>
                            <StackPanel Orientation="Horizontal">
                                <Label Content="StringSearch:" Width="100" HorizontalAlignment="Left" Margin="3"
                                    ToolTip="Searches all callstacks, stringcontent, class names. Can be expensive. Case insensitive. Use '!' prefix for all results that do NOT contain the string. Use ';' to separate multiple OR'd values"/>
                                <TextBox HorizontalAlignment="Left" Margin="3" Name="SrchStrTextBox" Text="{Binding Path=SrchString, Mode=TwoWay, ValidatesOnExceptions=true, NotifyOnValidationError=true}" Width="140"/>
                            </StackPanel>
                        </StackPanel>

                        <StackPanel Orientation="Vertical" Grid.Column="3" Grid.Row="0">
                            <Label Content="Leak _Multiple:" HorizontalAlignment="Left" Margin="3" Target="{Binding ElementName=LeakMultipleTextBox}"
                                ToolTip="Repeat scenario N times exactly, find multiples in filter range (See also RtClick on heap->FindLeaks).&#xa;A single integer (# times)&#xa; or N Pairs of ints, 1 comma separated pair per line"/>
                            <TextBox
                                Height="150"
                                HorizontalAlignment="Left"
                                Margin="3"
                                Name="LeakMultipleTextBox"
                                VerticalScrollBarVisibility="Auto"
                                AcceptsReturn="True"
                                Text="{Binding Path=LeakMultipleRawText , Mode=TwoWay, ValidatesOnExceptions=true, NotifyOnValidationError=true}"
                                Width="170"/>

                        </StackPanel>
                        <StackPanel Orientation="Vertical" Grid.Column="6" Grid.Row="0">
                            <Label Content="KnownIssues:" HorizontalAlignment="Left" Margin="3"
                                ToolTip="Known leak issues from XML. .&#xa;
See also Menu->ShowKnownIssues"/>
                            <RadioButton Name="rbtnKnownIssuesNone" Content="No Action" ToolTip="no special action taken with Known Issues"></RadioButton>
                            <RadioButton Name="rbtnKnownIssuesInclude" Content="_Include known issues in filtered results and as a column in Details" ToolTip="Include Known Issues in filter and show them in a column in snapshots, which can be sorted, etc."></RadioButton>
                            <RadioButton Name="rbtnKnownIssuesExclude" Content="E_xclude known issues in filtered results"
                                ToolTip="Exclude Known Issues in snapshots. Takes longer when filtering: caution: with large # of stacks takes much longer"></RadioButton>
                            <StackPanel Orientation="Vertical" Margin="20,20,0,0" Grid.Row="4" Grid.Column="5">
                                <Button Content="_Clear Filter" Height="23" Margin="0,0,12,0" Name="btnClear" Width="75" HorizontalAlignment="Left" ToolTip="Clear filter. Content written to status for reference"/>
                                <Button Content="_Apply Filter" Height="23" Margin="0,20,12,0" Name="btnApply" Width="75" HorizontalAlignment="Left"/>
                            </StackPanel>
                        </StackPanel>
                    </Grid>

                </StackPanel>
                <StackPanel Orientation="Vertical" Grid.Row="1" Name="ctrlFiltResults" Width="1000" HorizontalAlignment="Left"></StackPanel>

            </StackPanel>
        </Grid>

            Dim lst = New List(Of GlobalFilter)
            lst.Add(_FilterData)

            Dim GlobalFilterDataViewSource As New System.Windows.Data.CollectionViewSource
            GlobalFilterDataViewSource.Source = lst

            Dim oGrid = CType(System.Windows.Markup.XamlReader.Load(xaml.CreateReader), Grid)
            oGrid.DataContext = GlobalFilterDataViewSource

            ctrlFiltResults = CType(oGrid.FindName("ctrlFiltResults"), StackPanel)
            SeqNoHiTextBox = CType(oGrid.FindName("SeqNoHiTextBox"), TxtBoxNumeric)
            SeqNoLoTextBox = CType(oGrid.FindName("SeqNoLoTextBox"), TxtBoxNumeric)
            ThreadTextBox = CType(oGrid.FindName("ThreadTextBox"), TextBox)
            SrchStrTextBox = CType(oGrid.FindName("SrchStrTextBox"), TextBox)
            rbtnKnownIssuesNone = CType(oGrid.FindName("rbtnKnownIssuesNone"), RadioButton)
            rbtnKnownIssuesInclude = CType(oGrid.FindName("rbtnKnownIssuesInclude"), RadioButton)
            rbtnKnownIssuesExclude = CType(oGrid.FindName("rbtnKnownIssuesExclude"), RadioButton)
            rbtnKnownIssuesNone.IsChecked = True
            LeakMultipleTextBox = CType(oGrid.FindName("LeakMultipleTextBox"), TextBox)
            LeakMultipleTextBox.ToolTip = <xml>
Put in a single integer, indicating the number of iterations, or multiple lines of pairs of comma separated Start/End pairs : e.g.:                                              
4169690,4905057 
4905057,5640424 
5640424,6375791 
6375791,7111158 
7111158,7846525 
7846525,8581892 
8581892,9317259 

                                          </xml>.Value
            LeakMultipleTextBox.Text = _FilterData.LeakMultipleRawText
            btnClear = CType(oGrid.FindName("btnClear"), Button)
            btnApply = CType(oGrid.FindName("btnApply"), Button)
            Me.Children.Add(oGrid)
        End Sub

        Sub on_TxtboxEntry(ByVal sender As Object, ByVal e As RoutedEventArgs) Handles SeqNoHiTextBox.GotFocus,
                                             SeqNoLoTextBox.GotFocus,
                                             ThreadTextBox.GotFocus,
                                             LeakMultipleTextBox.GotFocus
            Dim tb = CType(sender, TextBox)
            tb.SelectAll()
        End Sub

        Sub on_btnClear() Handles btnClear.Click
            If Not String.IsNullOrEmpty(_GlobalFilter.LeakMultipleRawText) Then
                UpdateStatusMsg("Filter clear leak multiples:" + vbCrLf + _GlobalFilter.LeakMultipleRawText)
            End If
            ClearValues()
            _GlobalFilter.ClearFilter()
            _VBDiagMarginBase._FilterUI.RefreshFilterTextbox()
            ctrlFiltResults.Children.Clear()
            UpdateStatusMsg("Filter Cleared")
        End Sub

        Private Sub btnKnownIssues_Click() Handles rbtnKnownIssuesExclude.Checked,
                                                rbtnKnownIssuesInclude.Checked,
                                                rbtnKnownIssuesNone.Checked
            UpdateStatusMsg("Clearing known issues so they will be reread")
            KnownIssues.ClearKnownIssues()
        End Sub
        Private Class foundStats
            Public strFound As String
            Public findType As GlobalFilter.FilterFindType
            Public lst As New List(Of HeapAllocationContainer)
            Public TotalSize As ULong
        End Class

        Friend Sub btnApply_Click(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles btnApply.Click
            Try
                'http://social.msdn.microsoft.com/Forums/vstudio/en-US/e75a6449-1f40-463c-a251-12d317350bf2/textbox-focus-databinding-and-hotkeys?forum=wpf
                Dim ctr = GetAncestor(Of DockPanel)(CType(sender, FrameworkElement))
                Dim bindings = BindingOperations.GetSourceUpdatingBindings(ctr)
                For Each binding In bindings
                    binding.UpdateSource()
                Next
                'Dim focused = Keyboard.FocusedElement
                'If focused IsNot Nothing Then
                '    If TryCast(focused, TextBox) IsNot Nothing Then
                '        Dim bex = CType(focused, TextBox).GetBindingExpression(TextBox.TextProperty)
                '        bex.UpdateSource()
                '    End If
                'End If
                'Me.DialogResult = False
                Dim nTotHCtrs = 0
                Me.Cursor = Cursors.Wait
                _GlobalFilter.LeakMultipleRawText = LeakMultipleTextBox.Text
                SeqNoLoTextBox.Text = _GlobalFilter.SeqNoLo.ToString("n0")
                SeqNoHiTextBox.Text = _GlobalFilter.SeqNoHi.ToString("n0")

                Dim swatch = Stopwatch.StartNew()

                _GlobalFilter = _FilterData
                _GlobalFilter.SrchString = _GlobalFilter.SrchString.ToUpper
                If rbtnKnownIssuesExclude.IsChecked Then
                    _GlobalFilter.KnownIssues = ShowKnownIssuesEnum.Exclude
                Else
                    If rbtnKnownIssuesInclude.IsChecked Then
                        _GlobalFilter.KnownIssues = ShowKnownIssuesEnum.Include
                    Else
                        _GlobalFilter.KnownIssues = ShowKnownIssuesEnum.NoAction
                    End If
                End If
                UpdateStatusMsg("Filtering " + _GlobalFilter.ToString(fLongVersion:=True))
                If _VBDiagMarginBase IsNot Nothing Then
                    _VBDiagMarginBase._FilterUI.RefreshFilterTextbox()
                End If
                Dim lstHeaps = New List(Of Tuple(Of CSpyHeap, MemSnapshot))
                Dim snap As MemSnapshot
                Dim dictStringsFound = New Dictionary(Of String, foundStats)
                Dim dictTypesFound = New Dictionary(Of String, foundStats)

                _GlobalFilter._deleFilterGotString = Sub(foundstr As String, hctr As HeapAllocationContainer, findType As GlobalFilter.FilterFindType)
                                                         Debug.Assert(Not String.IsNullOrEmpty(foundstr), "got null filter string ?")
                                                         Dim stats As foundStats = Nothing
                                                         If Not dictStringsFound.TryGetValue(foundstr, stats) Then
                                                             stats = New foundStats With {.strFound = foundstr, .findType = findType}
                                                             dictStringsFound(foundstr) = stats
                                                         End If
                                                         stats.lst.Add(hctr)
                                                         stats.TotalSize += CULng(hctr.GetSize)

                                                         Dim foundType = "NativeHeap"
                                                         If hctr.IsMemSpectHeap Then
                                                             foundType = hctr.TBlkBlockType.ToString
                                                         End If
                                                         If Not dictTypesFound.TryGetValue(foundType, stats) Then
                                                             stats = New foundStats With {.strFound = foundType}
                                                             dictTypesFound(foundType) = stats
                                                         End If
                                                         stats.lst.Add(hctr)
                                                         stats.TotalSize += CULng(hctr.GetSize)
                                                     End Sub

                _GlobalFilter._dictFrames = New Dictionary(Of IntPtr, Boolean)

                ctrlFiltResults.Children.Clear()

                If _ConnectionMode = MemSpectMode.Offline Then
                    MiniDumpReader.Singleton.MakeMemoryDictionary()
                End If
                Dim lamDoIt As Action(Of Object) = Sub(o As Object)
                                                       Dim hp = CType(o, CSpyHeap)
                                                       UpdateStatusMsg(String.Format("Filtering {0}", hp.HeapName), msgType:=StatusMessageType.StatusBarEntry, msgPriority:=StatusMessagePriority.Low)
                                                       If _GlobalFilter.LeakMultiple > 0 Then
                                                           Dim lstAllocs = hp.GetLeaksFromLeakMultipleToList(_GlobalFilter.LeakMultiple, _GlobalFilter._LeakMultipleSeqnos).
                                                               OrderBy(Function(h) h.AllocationStruct.SeqNo).ToList

                                                           snap = MemSnapshot.CreateMemSnapshotFromListAllocs(hp, lstAllocs, fEnableFilter:=_GlobalFilter.IsFilterMoreThanOnlySeqno)
                                                       Else
                                                           snap = hp.TakeMemSnapshot(fEnableFilter:=True)
                                                       End If
                                                       If snap.nCnt > 0 Then
                                                           nTotHCtrs += snap.Allocs.Count
                                                           lstHeaps.Add(New Tuple(Of CSpyHeap, MemSnapshot)(hp, snap))
                                                       End If

                                                   End Sub

                'Dim taskMemSpectHeap = New Task(lamDoIt, _HeapList.Where(Function(h) h.HeapName = MemSpectHeapName).SingleOrDefault())
                'taskMemSpectHeap.Start()
                'Dim taskProcessHeap = New Task(lamDoIt, _HeapList.Where(Function(h) h.HeapName = ProcessHeapName).SingleOrDefault())
                'taskProcessHeap.Start()

                'For Each hp In _HeapList.Where(Function(h) h.HeapName <> MemSpectHeapName AndAlso h.HeapName <> ProcessHeapName)
                '    lamDoIt.Invoke(hp)
                'Next
                'taskMemSpectHeap.Wait()
                'taskProcessHeap.Wait()

                'Parallel.ForEach(_HeapList, Sub(hp)

                '                            End Sub)

                For Each hp In _HeapList
                    lamDoIt.Invoke(hp)
                Next

                _GlobalFilter._deleFilterGotString = Nothing
                Dim spHoriz = New StackPanel With
                              {
                                  .Orientation = Orientation.Horizontal,
                                  .MaxHeight = 350
                              }
                ctrlFiltResults.Children.Add(spHoriz)
                If Not String.IsNullOrEmpty(_GlobalFilter.SrchString) Then
                    Dim bordTypesFound = New Border With {.BorderBrush = Brushes.Azure, .BorderThickness = New Windows.Thickness(4)}
                    Dim qTypesFoundRes = From fstats In dictTypesFound.Values
                                      Order By fstats.strFound
                                      Select
                                        FoundType = fstats.strFound,
                                        fstats.TotalSize,
                                        fstats.lst.Count,
                                        _fstats = fstats


                    browTypesFoundRes = New Browse(qTypesFoundRes,
                                                   ColWidths:={200, 90, 90},
                                                   fAllowBrowFilter:=True,
                                                   arrColumnsToTotal:={"TotalSize", "Count"}
                                                   )

                    bordTypesFound.Child = browTypesFoundRes
                    spHoriz.Children.Add(bordTypesFound)
                    browTypesFoundRes._BrowseList.ContextMenu.
                        AddMnuItem(
                                "_SubSnapshot",
                                "Create a new snapshot from the FoundString selected items",
                                Sub()
                                    Dim items = browTypesFoundRes._BrowseList.SelectedItems
                                    If items Is Nothing OrElse items.Count < 1 Then
                                        items = browTypesFoundRes._BrowseList.Items
                                    End If
                                    Dim qAllocs = From itm In items
                                                    Let tdescitem = ComponentModel.TypeDescriptor.GetProperties(itm)("_fstats"),
                                                    fstats = CType(tdescitem.GetValue(itm), foundStats)
                                                    From hctr In fstats.lst
                                                    Select hctr

                                    Dim subsnapTitle = "SubSnapFoundStringTypes " + _GlobalFilter.ToString()
                                    Dim tblkType = TrkType.All

                                    If items.Count = 1 Then
                                        subsnapTitle += " " + items(0).ToString()
                                        If subsnapTitle.Contains("FoundType = ClrObject") Then
                                            tblkType = TrkType.ClrObjects
                                        End If
                                    End If
                                    Dim msnap = MemSnapshot.CreateMemSnapshotFromListAllocs(hp:=Nothing, srcAllocs:=qAllocs.ToList, fEnableFilter:=False)

                                    ShowSnapshot(Nothing, msnap, subsnapTitle, AddressOf BrowQueryFuncForAnyType, tblkType)

                                End Sub, 0)


                    btnShowFoundStrsInBrowMem = New Button() With
                        {
                            .Content = "Show found strings",
                            .ToolTip = "Show the strings found and their associated allocations in a new page",
                            .Width = 200,
                            .HorizontalAlignment = Windows.HorizontalAlignment.Left
                        }
                    AddHandler btnShowFoundStrsInBrowMem.Click, Sub()
                                                                    Try
                                                                        Dim qStringres = From fstats In dictStringsFound.Values
                                                                                          Order By fstats.TotalSize Descending
                                                                                          Select
                                                                                            FindType = fstats.findType.ToString.Substring(4),
                                                                                            FoundString = fstats.strFound,
                                                                                            fstats.lst.Count,
                                                                                            fstats.TotalSize,
                                                                                            _fstats = fstats
                                                                        Dim browStringResults = New Browse(qStringres,
                                                                                                       ColWidths:={110, 1100, 90, 90},
                                                                                                       fAllowBrowFilter:=True,
                                                                                                       arrColumnsToTotal:={"TotalSize", "Count"}
                                                                                                       )
                                                                        Dim subsnapTitle = String.Format("FoundStrings '{0}'", _GlobalFilter.ToString())
                                                                        browStringResults._BrowseList.ContextMenu.AddMnuItem(
                                                                            "_SubSnapshot",
                                                                            "Create a new snapshot from the selected items",
                                                                            Sub()
                                                                                Dim items = browStringResults._BrowseList.SelectedItems
                                                                                If items Is Nothing OrElse items.Count < 1 Then
                                                                                    items = browStringResults._BrowseList.Items
                                                                                End If
                                                                                Dim qAllocs = From itm In items
                                                                                                Let tdescitem = ComponentModel.TypeDescriptor.GetProperties(itm)("_fstats"),
                                                                                                fstats = CType(tdescitem.GetValue(itm), foundStats)
                                                                                                From hctr In fstats.lst
                                                                                                Select hctr

                                                                                If items.Count = 1 Then
                                                                                    subsnapTitle += " " + items(0).ToString()
                                                                                End If
                                                                                Dim msnap = New MemSnapshot With {.Allocs = qAllocs.ToList}

                                                                                ShowSnapshot(Nothing, msnap, subsnapTitle, AddressOf BrowQueryFuncForAnyType)

                                                                            End Sub, 0)
                                                                        Dim ctrls = DataWindowMain.MakeNewDatasurface(
                                                                            "FoundStrs",
                                                                           subsnapTitle,
                                                                           nMaxHeaderHeight:=50)
                                                                        ctrls.SurfaceHeader.Children.Add(
                                                                            New TextBlock With {
                                                                                .Text = subsnapTitle
                                                                            }
                                                                        )
                                                                        ctrls.SurfaceDetails.Children.Add(browStringResults)
                                                                    Catch ex As Exception
                                                                        MemSpectExceptionHandler(ex)

                                                                    End Try
                                                                End Sub
                    ctrlFiltResults.Children.Add(btnShowFoundStrsInBrowMem)
                End If

                btnSubSnapAll = New Button() With
                    {
                        .Content = "_SubSnap All Results",
                        .ToolTip = "Show all allocations together in a single page, combining e.g. native and managed allocations",
                        .Width = 200,
                        .HorizontalAlignment = Windows.HorizontalAlignment.Left
                    }
                AddHandler btnSubSnapAll.Click, Sub()
                                                    Try
                                                        Dim lstAll = New List(Of HeapAllocationContainer)
                                                        lstHeaps.ForEach(Sub(Tup As Tuple(Of CSpyHeap, MemSnapshot))
                                                                             lstAll.AddRange(Tup.Item2.Allocs)
                                                                         End Sub)
                                                        Dim mSnap = New MemSnapshot() With {.Allocs = lstAll}

                                                        Dim x = ShowSnapshot(Nothing, mSnap, "FilterRes " + _GlobalFilter.ToString(), AddressOf BrowQueryFuncForAnyType)

                                                    Catch ex As Exception
                                                        MemSpectExceptionHandler(ex)
                                                    End Try

                                                End Sub
                ctrlFiltResults.Children.Add(btnSubSnapAll)

                Dim qSnapList = From tp In lstHeaps
                    Select Heap = tp.Item1.HeapName,
                    FilteredSize = tp.Item2.nTotMem,
                    FilteredCount = tp.Item2.nCnt,
                    CurSize = tp.Item1.CurTotBytes,
                    nLive = tp.Item1.CurNumAllocs,
                    _Heap = tp.Item1,
                    _Snap = tp.Item2
                Dim bordSnapList = New Border With {.BorderBrush = Brushes.Azure, .BorderThickness = New Windows.Thickness(4)}
                browFilteredHeapList = New Browse(qSnapList,
                                      ColWidths:=New Integer() {If(_IsClientOutOfProcess, 180, 140), 100, 90, 90},
                                      fAllowBrowFilter:=True,
                                      ColTips:={"HeapName",
                                                "Filtered Size",
                                                "# filtered items",
                                                "Current size in bytes of heap(unfiltered)",
                                                "Current number of live items in heap(unfiltered)"
                                               },
                                      fAllowHeaderClickSort:=True,
                                      arrColumnsToTotal:={"FilteredSize", "FilteredCount", "CurSize", "nLive"}
                                      )

                browFilteredHeapList._BrowseList.ContextMenu.AddMnuItem("_Snapshot", "Show a snapshot of the heap",
                                                        Sub()
                                                            browFilteredHeapList._BrowseList.RaiseEvent(
                                                                New MouseButtonEventArgs(
                                                                    InputManager.Current.PrimaryMouseDevice,
                                                                    0,
                                                                    MouseButton.Left) With {
                                                                       .RoutedEvent = ListView.MouseDoubleClickEvent
                                                                    }
                                                                )
                                                            'Dim itm = brow._BrowseList.SelectedItem
                                                            'If itm IsNot Nothing Then
                                                            '    Dim tdesc = ComponentModel.TypeDescriptor.GetProperties(itm)("_Heap")
                                                            '    Dim tdval As Object = Nothing
                                                            '    If tdesc IsNot Nothing Then
                                                            '        tdval = tdesc.GetValue(itm)
                                                            '        If tdval IsNot Nothing Then

                                                            '        End If
                                                            '    End If

                                                            'End If
                                                        End Sub, 0
                )

                AddHandler browFilteredHeapList._BrowseList.MouseDoubleClick, AddressOf OnHeapSnapDblClick
                AddHandler browFilteredHeapList._BrowseList.MouseMove, AddressOf OnHeapMouseMove
                bordSnapList.Child = browFilteredHeapList
                ctrlFiltResults.Children.Add(New TextBlock With {.Text = _GlobalFilter.ToString(fLongVersion:=True)})
                Dim txtRes = String.Format("Found # Filtered allocs={0:n0} in {1:n3} secs ", nTotHCtrs, swatch.ElapsedMilliseconds / 1000.0)
                UpdateStatusMsg(txtRes)
                ctrlFiltResults.Children.Add(New TextBlock With {.Text = txtRes})
                If _GlobalFilter.LeakMultiple > 0 AndAlso _GlobalFilter.LeakMultiple < 20 Then
                    If _GlobalFilter._LeakMultipleSeqnos Is Nothing Then
                        Dim nBucketWidth = (_GlobalFilter.SeqNoHi - _GlobalFilter.SeqNoLo) \ _GlobalFilter.LeakMultiple
                        Dim buckstarts(_GlobalFilter.LeakMultiple - 1) As UInteger
                        For i = 0 To _GlobalFilter.LeakMultiple - 1
                            buckstarts(i) = _GlobalFilter.SeqNoLo + CUInt(i * nBucketWidth)
                        Next
                        Dim strBucks = String.Join(", ", buckstarts)
                        ctrlFiltResults.Children.Add(New TextBox With {
                                                     .Text = String.Format("BucketSize= {0}   Buckets = {1}", nBucketWidth, strBucks),
                                                     .IsReadOnly = True
                                                 })

                    End If
                End If
                spHoriz.Children.Insert(0, bordSnapList)
                Me.Cursor = Cursors.Arrow
                'Me.DialogResult = True
                'Me.Close()

            Catch ex As Exception
                CommonUI.MemSpectExceptionHandler(ex)
            End Try
        End Sub

        Public Function BrowQueryFuncForAnyType(theheapAllocs As List(Of HeapAllocationContainer), thebmem As BrowseMem) As IEnumerable
            Dim qDetails As IEnumerable
            qDetails = From hctr In theheapAllocs
                Select
                    Address = hctr.GetAddr.ToString("x8"),
                    hctr.AllocationStruct.SeqNo,
                    Size = hctr.GetSize,
                    hctr.AllocationStruct.Thread,
                    BlkType = hctr.GetBlockTypeName,
                    Data = hctr.GetDisplayData,
                    ClassName = hctr.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True),
                    _HeapAllocationContainer = hctr
                Order By SeqNo

            thebmem._ColWidths = {WIDTH_ADDRESS, WIDTH_SEQNO, 60, 60, 80, 900}
            thebmem.ColumnTips = {TIP_ADDRESS, TIP_SEQNO, TIP_SIZE, TIP_THREAD, TIP_STRING_CONTENT}
            thebmem._arrColumnsToTotal = {"Size"}
            thebmem.InitialSortOrder = New BrowseInitialSortOrder With {.ColumnNo = 2, .direction = ListSortDirection.Ascending}
            Return qDetails
        End Function

        Sub OnHeapSnapDblClick(ByVal sender As Object, ByVal e As RoutedEventArgs)
            Dim lv As Browse.BrowseList = Nothing
            Try
                lv = TryCast(sender, Browse.BrowseList)
                If IsNothing(lv) Then
                    Return
                End If
                Dim dt = GetListViewItemDataContextFromObj(e.OriginalSource)
                If IsNothing(dt) Then
                    dt = lv.SelectedItem
                    If IsNothing(dt) Then
                        Return
                    End If
                End If
                lv.Cursor = Cursors.Wait
                Dim tdesc = ComponentModel.TypeDescriptor.GetProperties(dt)("_Heap")
                Dim heap = CType(tdesc.GetValue(dt), CSpyHeap)
                Dim tdescSnap = ComponentModel.TypeDescriptor.GetProperties(dt)("_Snap")
                Dim Snap = CType(tdescSnap.GetValue(dt), MemSnapshot)

                ShowSnapshot(heap, Snap)
            Catch ex As Exception
                CommonUI.MemSpectExceptionHandler(ex)
            Finally
                If lv IsNot Nothing Then
                    lv.Cursor = Cursors.Arrow
                End If
            End Try

        End Sub

        Private Sub ClearValues()
            SeqNoLoTextBox.Text = "0"
            SeqNoHiTextBox.Text = "0"
            ThreadTextBox.Text = "0"
            SrchStrTextBox.Text = String.Empty
            rbtnKnownIssuesNone.IsChecked = True
            LeakMultipleTextBox.Text = String.Empty
        End Sub

    End Class

    Public Class FilterUI
        Inherits DockPanel
        Public WithEvents _btnSeqNoLo As New Button With {
            .Content = "Set_Lo",
            .ToolTip = "Set low value for SeqNo for filter. Also, if TrackingMode =0 (Minimal), sets it to 1 (Normal). Red background indicates TrackingMode=0 (Minimal)" + vbCrLf +
            " Also fires customcodemarker 100000 when not frozen to trigger MemStat collection"
            }
        Public WithEvents _btnSeqNoHi As New Button With {
            .Content = "Set_Hi",
            .ToolTip = "Set high value for SeqNo for filter. Also, if trackingMode=0 (Minimal) in INI file, sets it to 0 (Minimal)"
            }
        Public _txtCurFilter As TextBlock

        Sub New()
            Dim sp As New StackPanel With {.Orientation = Orientation.Horizontal}
            If _ConnectionMode = MemSpectMode.OnLine Then
                Dim curmode = GetTrackingMode()
                AddHandler TrackingModeChanged, Sub(e As TrackingModeEnum)
                                                    Select Case e
                                                        Case TrackingModeEnum.Minimal
                                                            _btnSeqNoLo.Background = Brushes.Red
                                                        Case TrackingModeEnum.Normal
                                                            _btnSeqNoLo.Background = Brushes.LightGray
                                                    End Select

                                                End Sub
                SetTrackingMode(curmode)
                sp.Children.Add(_btnSeqNoLo)
                sp.Children.Add(_btnSeqNoHi)
            End If
            _txtCurFilter = New TextBlock With {
                            .Text = _GlobalFilter.ToString
                        }
            sp.Children.Add(_txtCurFilter)
            Me.Children.Add(sp)
            AddHandler GlobalFilter.GlobalFilterChanged, Sub()
                                                             Try
                                                                 RefreshFilterTextbox()
                                                             Catch ex As Exception

                                                             End Try
                                                         End Sub

        End Sub
        Public Sub RefreshFilterTextbox()
            _txtCurFilter.Dispatcher.Invoke(Sub()
                                                _txtCurFilter.Text = _GlobalFilter.ToString
                                            End Sub)
        End Sub
        Sub On_btnLo() Handles _btnSeqNoLo.Click
            Dim curTrackingMode = GetTrackingMode()
            If curTrackingMode = TrackingModeEnum.Minimal Then ' if we're not tracking any stacks
                SetTrackingMode(TrackingModeEnum.Normal) ' track them
                UpdateStatusMsgDbg("TrackingMode Minimal-> Normal")
            End If
            _GlobalFilter.SeqNoLo = GetGlobalPassCount()
            If Not ProcComm._isTargetFrozen Then
                FireCustomCodeMarkerEvent("SetLo", CodeMarkerEventType.None, 0, 100000)
            End If

            UpdateStatusMsg("SetLo " + _GlobalFilter.SeqNoLo.ToString)
            _GlobalFilter.SeqNoHi = 0
            RefreshFilterTextbox()
        End Sub

        Sub On_btnHi() Handles _btnSeqNoHi.Click
            _GlobalFilter.SeqNoHi = GetGlobalPassCount()
            UpdateStatusMsg("SetHi " + _GlobalFilter.SeqNoHi.ToString)
            If GetTrackingMode(fFromInitialSettings:=True) = TrackingModeEnum.Minimal Then ' if user wants minimal
                SetTrackingMode(TrackingModeEnum.Minimal)
            End If
            RefreshFilterTextbox()
        End Sub
    End Class
End Namespace
