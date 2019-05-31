
Namespace MemSpect
    ' 1 instance of this on every treeviewitem: lots of instances, so use TextBlock for perf, unless user clicks->then TextBox to allow selection
    Public Class WhenDistTextBoxControl
        Friend _hctrList As List(Of HeapAllocationContainer)
        Friend _whenDistPanel As WhenDistPanel
        Friend _txtBox As TextBox
        Friend WithEvents _txtBlk As New TextBlock

        Public Shared NumBuckets As Long = 32

        Public Sub New(ByVal hctrList As List(Of HeapAllocationContainer), ByVal WDPanel As WhenDistPanel)
            _hctrList = hctrList
            _whenDistPanel = WDPanel
            Me._txtBlk.Text = WhenDist()
            Me._txtBlk.Background = Brushes.LightSalmon
            Me._txtBlk.FontFamily = FontFamilyCourierNew
            Me._txtBlk.FontSize = 10
        End Sub

        Private Sub onTxtblk_mousedown(ByVal s As Object, ByVal e As MouseButtonEventArgs) Handles _txtBlk.MouseDown
            Try

                If Me._txtBox Is Nothing AndAlso e.RightButton = MouseButtonState.Released Then
                    ' user clicked on the textblock: we want to swap in a TextBox
                    ' but first we need to select the item
                    Dim sp = CType(VisualTreeHelper.GetParent(Me._txtBlk), StackPanel)
                    Dim tvitem = CType(sp.Parent, TVStackAggPanel.StackAggTreeView.StackAggTViewItem)
                    tvitem.IsSelected = True

                    Me._txtBox = New TextBox With {
                            .Text = Me._txtBlk.Text,
                            .IsReadOnly = True,
                            .Background = Me._txtBlk.Background,
                            .FontFamily = Me._txtBlk.FontFamily,
                            .FontSize = Me._txtBlk.FontSize
                    }

                    sp.Children.Remove(Me._txtBlk)
                    sp.Children.Insert(0, Me._txtBox)
                    Me._txtBox.ContextMenu = New ContextMenu
                    Me._txtBox.ContextMenu.AddMnuItem("_SubSnap Current items",
                                            "create a subsnapshot of the current allocations selected time range",
                                            Sub(o As Object, e2 As RoutedEventArgs)
                                                Dim lst = _hctrList
                                                If Me._txtBox.SelectionLength > 0 Then
                                                    Dim rng = 1 + CDbl((_whenDistPanel._SeqNoMax - _whenDistPanel._SeqNoMin))  ' avoid div 0
                                                    lst = New List(Of HeapAllocationContainer)
                                                    Dim srclist = CType(_whenDistPanel._SortableTVPanel, TVStackAggPanel)._qDetails
                                                    For Each itm In srclist
                                                        Dim hctr = HeapAllocationContainer.CreateFrom(itm)
                                                        If hctr Is Nothing Then
                                                            Continue For
                                                        End If
                                                        If hctr.AllocationStruct.SeqNo >= _whenDistPanel._SeqNoMin Then
                                                            Dim bckt = CInt(
                                                                Math.Truncate(
                                                                    NumBuckets *
                                                                        (hctr.AllocationStruct.SeqNo - _whenDistPanel._SeqNoMin) / rng)
                                                                )
                                                            If bckt >= Me._txtBox.SelectionStart AndAlso
                                                                    bckt <= Me._txtBox.SelectionStart + Me._txtBox.SelectionLength Then
                                                                lst.Add(hctr)
                                                            End If
                                                        End If
                                                    Next
                                                End If
                                                ShowSubSnapShot(lst, "SubSnap from timerange " + Me._txtBox.Text + Me._txtBox.SelectedText)
                                            End Sub)

                    Me._txtBox.ContextMenu.AddMnuItem("Set _Range to bucket Selection",
                                            "change the min and max values to those selected",
                                            Sub(o As Object, e2 As RoutedEventArgs)
                                                If Me._txtBox.SelectionLength > 0 Then
                                                    Dim bktsize = _whenDistPanel.BucketSize
                                                    _whenDistPanel.SetRange(
                                                        _whenDistPanel._SeqNoMin + CUInt(Me._txtBox.SelectionStart * bktsize),
                                                        _whenDistPanel._SeqNoMin + CUInt((Me._txtBox.SelectionStart + Me._txtBox.SelectionLength) * bktsize))
                                                    _whenDistPanel._btnApplyWhenMinMax.RaiseEvent(New RoutedEventArgs(Button.ClickEvent, _whenDistPanel._btnApplyWhenMinMax))

                                                End If
                                            End Sub)


                    tvitem.AddHandler(TreeViewItem.UnselectedEvent, New RoutedEventHandler(AddressOf onselchanged))

                    Me._txtBox.Focus()
                    e.Handled = True
                Else

                End If
            Catch ex As Exception
                MemSpectExceptionHandler(ex)
            End Try
        End Sub

        Private Sub onselchanged(ByVal o As Object, ByVal e As RoutedEventArgs)
            Try
                If Me._txtBox IsNot Nothing Then
                    Dim sp = CType(VisualTreeHelper.GetParent(Me._txtBox), StackPanel)
                    sp.Children.Remove(Me._txtBox)
                    sp.Children.Insert(0, Me._txtBlk)
                    Me._txtBox = Nothing

                End If
            Catch ex As Exception
                MemSpectExceptionHandler(ex)
            End Try

        End Sub

        Private Function GetBuckets() As Integer()
            Dim buckets(CInt(NumBuckets)) As Integer
            Dim rng = 1 + CDbl((_whenDistPanel._SeqNoMax - _whenDistPanel._SeqNoMin)) ' avoid div 0
            For Each hctr In _hctrList
                Dim bckt = CInt(Math.Truncate(
                        NumBuckets * (hctr.AllocationStruct.SeqNo - _whenDistPanel._SeqNoMin) / rng)
                    )
                If bckt >= 0 AndAlso bckt < buckets.Length Then
                    buckets(bckt) += 1
                End If
            Next
            Return buckets
        End Function

        Public Function WhenDist() As String
            Dim strWhen = String.Empty
            Dim buckets = GetBuckets()
            For i = 0 To CInt(NumBuckets) - 1
                Dim n = buckets(i)
                If n > 0 Then
                    If n > 9 Then
                        strWhen += Chr(Math.Min(n - 9, 26) + 64)
                    Else
                        strWhen += Chr(n + 48)
                    End If
                Else
                    strWhen += "_"
                End If
            Next
            Return strWhen
        End Function

        Public Overrides Function ToString() As String
            Return WhenDist()
        End Function
    End Class

    Public Class WhenDistPanel
        Inherits DockPanel
        Public _WhenDistEnabled As Boolean
        Public _WhenDistTxtBoxMin As TxtBoxNumeric 'not textblock,  so user can select contents
        Public _WhenDistTxtBoxMax As TxtBoxNumeric 'not textblock,  so user can select contents
        Public _btnApplyWhenMinMax As Button
        Public _SortableTVPanel As SortableTVPanel
        Public _SeqNoMin As UInteger
        Public _SeqNoMax As UInteger
        Public WithEvents _chkEnableWhen As CheckBox
        Sub New(ByVal tv As SortableTVPanel)
            _SortableTVPanel = tv
            _SeqNoMin = Integer.MaxValue
            Dim sp As New StackPanel With {.Orientation = Orientation.Horizontal}

            WhenDistTextBoxControl.NumBuckets = GetPrivateProfileInt(ProfileStringSection, "WhenDistNumBuckets", 32, _iniFileName)
            _WhenDistEnabled = CBool(GetPrivateProfileInt(ProfileStringSection, "WhenDist", 1, _iniFileName))

            sp.Children.Add(New TextBlock With {.Text = "   "}) ' spacer

            _chkEnableWhen = New CheckBox With {
                .Content = "_When",
                .ToolTip = "Show the distribution of allocations in 32 buckets. 37 possible values: '_', 0-9, A-Z. Anything > 9 will show as 'Z'" +
                    vbCrLf + "(slower when enabled) ",
                .IsChecked = _WhenDistEnabled
                }

            _WhenDistTxtBoxMin = New TxtBoxNumeric With {
                .Margin = New Thickness(1)
            }
            _WhenDistTxtBoxMax = New TxtBoxNumeric With {
                .Margin = New Thickness(1, 0, 2, 0)
            }

            sp.Children.Add(_chkEnableWhen)

            Dim lamWhenMinMaxChaged = Sub(sender As Object, e As RoutedEventArgs)
                                          'add your own event handler
                                      End Sub
            Dim lamMinMaxKeyUp = Sub(sender As Object, e As KeyEventArgs)
                                     If e.Key = Key.Enter Then
                                         _btnApplyWhenMinMax.RaiseEvent(New RoutedEventArgs(Button.ClickEvent, _btnApplyWhenMinMax))
                                         lamWhenMinMaxChaged.Invoke(Nothing, Nothing)
                                     End If
                                 End Sub
            _btnApplyWhenMinMax = New Button With {
                .Content = "_Apply",
                .Margin = New Thickness(1)
            }

            AddHandler _btnApplyWhenMinMax.Click, lamWhenMinMaxChaged


            sp.Children.Add(New Label With {.Content = "Min"})
            sp.Children.Add(_WhenDistTxtBoxMin)
            sp.Children.Add(New Label With {.Content = "Max"})
            sp.Children.Add(_WhenDistTxtBoxMax)
            sp.Children.Add(_btnApplyWhenMinMax)
            Dim bord = New Border With {
                .Child = sp,
                .BorderThickness = New Thickness(1),
                .BorderBrush = Brushes.LightGray,
                .Margin = New Thickness(10, 0, 10, 0)
            }
            Me.Children.Add(bord)
        End Sub

        Public ReadOnly Property BucketSize As Integer
            Get
                Dim bcketsize = 0
                If _SeqNoMax > _SeqNoMin Then
                    Dim tmp = 1 + CDbl((_SeqNoMax - _SeqNoMin)) / WhenDistTextBoxControl.NumBuckets
                    bcketsize = CInt(tmp)
                End If
                Return bcketsize
            End Get
        End Property

        Public Sub SetRange(ByVal seqmin As UInteger, ByVal seqmax As UInteger)
            _WhenDistTxtBoxMin.SetText(seqmin.ToString("n0"))
            _WhenDistTxtBoxMax.SetText(seqmax.ToString("n0"))
            _btnApplyWhenMinMax.ToolTip = String.Format("{0} {1} {2}", seqmin, seqmax, BucketSize)
        End Sub

        Sub chkEnabledChanged() Handles _chkEnableWhen.Checked, _chkEnableWhen.Unchecked
            WritePrivateProfileString(ProfileStringSection,
                          "WhenDist",
                          If(_chkEnableWhen.IsChecked, "1", "0"),
                          _iniFileName)
            _WhenDistEnabled = _chkEnableWhen.IsChecked.Value
            If Not _WhenDistEnabled Then
                _WhenDistTxtBoxMin.Text = ""
                _WhenDistTxtBoxMax.Text = ""
            End If
        End Sub
    End Class
End Namespace
