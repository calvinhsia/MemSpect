Imports System.IO
Imports System.IO.Pipes
Imports System.Windows.Controls
Imports System.Windows.Documents
Imports System.Windows.Input
Imports System.Windows.Threading
Imports System.Threading
Imports System.Text
Imports System.Windows.Media
Imports System.Runtime.InteropServices
Imports System.Linq

Imports FastSerialization0
Imports System.ComponentModel

<Assembly: Runtime.CompilerServices.InternalsVisibleTo("MemSpect.Test, PublicKey=0024000004800000940000000602000000240000525341310004000001000100fd51ffc305f02c69a29bf791b1c7c5794e6a717c2db35b64dbf800529443f8a960ef67b9362c662b1ebe0c1452c11bfe29c828e22a668e07a1b9679d935c5472922d64a8924e6bf91e0c2737777647da1eb1a7667a14736aa56f67f6db68d6c8a928bf36b6ee155d053cadf5b25c7f36b435ead06b35bab33948a0dfee77fd99")> 
Namespace MemSpect

    Public Class MyTabControl
        Inherits TabControl
        Protected Friend _tabitems As New List(Of TabItem)
        ''' <summary>
        ''' add a new tab to the browmem
        ''' </summary>
        ''' <param name="strItem"></param>
        ''' <param name="strToolTip"></param>
        ''' <param name="delGotFocus">called when got focus and content is empty</param>
        Friend Function AddTabItem(ByVal strItem As String, ByVal strToolTip As String, ByVal delGotFocus As Action(Of Object, RoutedEventArgs)) As TabItem
            Dim tbitem = New TabItem With {.Tag = strItem}

            _tabitems.Add(tbitem)
            tbitem.Header = New TextBlock With {
                .Text = strItem,
                .FontWeight = FontWeights.Bold,
                .ToolTip = strToolTip
                }

            Dim lamInvokeDel = Sub(sender As Object, e As RoutedEventArgs)
                                   Try
                                       If tbitem.Content Is Nothing Then
                                           Using New DataWindowMain.SetCursorWait
                                               delGotFocus.Invoke(tbitem, New RoutedEventArgs)
                                           End Using
                                       End If
                                   Catch ex As Exception
                                       CommonUI.MemSpectExceptionHandler(ex)
                                   End Try
                               End Sub

            If delGotFocus IsNot Nothing Then
                If _tabitems.Count = 1 Then ' 1st one we need to render
                    lamInvokeDel.Invoke(tbitem, New RoutedEventArgs)
                Else
                    AddHandler tbitem.GotFocus, lamInvokeDel

                End If
            End If

            Me.Items.Add(tbitem)
            Return tbitem
        End Function

    End Class

    Public Class MyTreeViewBase
        Inherits TreeView
        Friend _SelectedItems As New List(Of MyTreeViewItem)

        Sub New()
            Dim XAMLtvItemStyle = _
<Style
    xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
    xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
    xmlns:l="clr-namespace:MemSpect;assembly=MemSpect"
    TargetType="{x:Type TypeName=TreeViewItem}">
    <Setter Property="FontFamily" Value=<%= VBDiagMarginBase._FontName %>/>
    <Setter Property="FontSize" Value=<%= VBDiagMarginBase._FontSize.ToString + "pt" %>/>
    <Setter Property="Foreground" Value=<%= VBDiagMarginBase._Foreground %>/>
    <Setter Property="Background" Value=<%= VBDiagMarginBase._Background %>/>
    <Style.Triggers>
        <!--
        <Trigger Property="IsMySelected" Value="True">
            <Setter Property="Foreground" Value="White"/>
            <Setter Property="Background" Value="DarkKhaki"/>
        </Trigger>
        -->
        <!--
            <Trigger Property="IsMouseOver" Value="True">
                <Setter Property="Foreground" Value="DarkRed"/>
                <Setter Property="Background" Value="LightGray"/>
            </Trigger>
        -->
    </Style.Triggers>
</Style>
            Dim styyle = CType(Windows.Markup.XamlReader.Load(XAMLtvItemStyle.CreateReader), Windows.Style)
            'If SystemParameters.HighContrast Then
            '    ' for some reason, adding setters is different from removing them: Exception: white is invalid for panel
            '    styyle.Setters.RemoveAt(styyle.Setters.Count - 1)
            '    styyle.Setters.RemoveAt(styyle.Setters.Count - 1)
            'End If

            Me.ItemContainerStyle = styyle

            Me.ContextMenu = New ContextMenu

            Me.ContextMenu.AddMnuItem("_DumpChildren To Notepad", "Dump expanded children to notepad", AddressOf on_ctxMenuItemBase)

            Me.ContextMenu.AddMnuItem("_Expand SubTree", "Expand tree branch from here: warning: try small branches first!", AddressOf on_ctxMenuItemBase)

        End Sub

        Friend Sub on_ctxMenuItemBase(ByVal sender As Object, ByVal e As RoutedEventArgs)
            Dim mitem = TryCast(e.OriginalSource, MenuItem)
            Dim vrb = mitem.Header.ToString
            Select Case vrb
                Case "_Expand SubTree"
                    OnExpandAllStart(CType(Me.SelectedItem, MyTreeViewItem))
                    CType(Me.SelectedItem, MyTreeViewItem).ExpandAll()
                    OnExpandAllEnd()
                Case "_DumpChildren To Notepad"
                    Dim sb As New Text.StringBuilder
                    sb.AppendLine("Children of """ + Me.SelectedItem.ToString + """")
                    CType(Me.SelectedItem, MyTreeViewItem).DumpChildren(sb, 0)
                    WriteOutputToTempFile(sb.ToString)
                Case Else
                    MsgBox("Base menu Chose " + vrb)
            End Select
        End Sub

        Protected Overridable Sub OnExpandAllStart(ByVal selItem As MyTreeViewItem)

        End Sub

        Protected Overridable Sub OnExpandAllEnd()

        End Sub

        Sub on_rtmouse(ByVal sender As Object, ByVal e As MouseEventArgs) Handles Me.PreviewMouseRightButtonDown
            ' we want to select the item you rt click on so context menu knows which item is selected
            Dim tv = TryCast(sender, MyTreeViewBase)

            If tv IsNot Nothing Then
                Dim pt = e.GetPosition(tv)
                Dim elem = tv.InputHitTest(pt)
                elem = CType(GetAncestor(Of MyTreeViewItem)(CType(elem, DependencyObject)), IInputElement)
                If elem IsNot Nothing Then
                    elem.Focus()
                End If
            End If
        End Sub

        Protected Friend _LastTipObj As FrameworkElement ' the control (like Textbox) that has the .ToolTip property
        Public Sub ClearPriorTVToolTipIfAny()
            If _LastTipObj IsNot Nothing Then
                If _LastTipObj.ToolTip IsNot Nothing Then
                    Dim lastTip = CType(_LastTipObj.ToolTip, ToolTip)
                    lastTip.IsOpen = False
                    _LastTipObj.ToolTip = Nothing
                End If
                _LastTipObj = Nothing
            End If
        End Sub

        Sub On_MouseLeave() Handles Me.MouseLeave
            Me.ClearPriorTVToolTipIfAny()
        End Sub

    End Class

    Public Class MyTreeViewItem
        Inherits TreeViewItem
        Friend Shared _IsMySelectedProperty As DependencyProperty = DependencyProperty.Register("IsMySelected", GetType(Boolean), GetType(MyTreeViewItem))
        Private Shared _IsInSelectedItemChanged As Boolean
        Protected Sub SelectedItemChanged(ByVal o As Object, ByVal e As RoutedEventArgs, ByVal SelectedList As List(Of MyTreeViewItem))
            If _IsInSelectedItemChanged Then
                Return
            End If
            _IsInSelectedItemChanged = True
            Dim wasMySelected = CBool(Me.GetValue(_IsMySelectedProperty))
            Dim lamUnSelectAll = Sub()
                                     'UpdateStatusMsgDbg("LamClearAll")
                                     SelectedList.ForEach(Sub(f As MyTreeViewItem)
                                                              f.SetValue(_IsMySelectedProperty, False)
                                                              f.IsSelected = False
                                                          End Sub)
                                     SelectedList.Clear()

                                 End Sub

            If Mouse.RightButton <> MouseButtonState.Pressed Then
                ' gets called when Me.IsSelected changes false -> true or true -> false, contrary to xml docs 
                If Me.IsSelected Then
                    'UpdateStatusMsgDbg("me.isselected # sel = " + SelectedList.Count().ToString + " " + Me.ToString)
                    Dim lastone = SelectedList.FirstOrDefault

                    Dim fShift = Keyboard.IsKeyDown(Key.LeftShift) OrElse Keyboard.IsKeyDown(Key.RightShift)
                    Dim fIsControl = Keyboard.IsKeyDown(Key.LeftCtrl) OrElse Keyboard.IsKeyDown(Key.RightCtrl)
                    If Not fIsControl OrElse fShift Then ' clear selection on select if not controlkey or if fShift
                        lamUnSelectAll.Invoke()
                    End If
                    If fShift Then ' they're currently all unselected
                        '                        UpdateStatusMsgDbg("Shift")
                        Dim parnt = TryCast(Me.Parent, ItemsControl)
                        'If p IsNot parnt Then
                        '    UpdateStatusMsgDbg("not parent")
                        'End If
                        If lastone IsNot Nothing AndAlso parnt IsNot Nothing Then
                            Dim start = parnt.Items.IndexOf(lastone)
                            Dim theend = parnt.Items.IndexOf(Me)
                            If start >= 0 AndAlso theend >= 0 Then
                                If start > theend Then
                                    Dim tmp = start
                                    start = theend
                                    theend = tmp
                                End If
                                For i = start To theend ' inclusive
                                    Dim t = CType(parnt.Items(i), MyTreeViewItem)
                                    If Not SelectedList.Contains(t) OrElse Not t.IsSelected Then
                                        t.SetValue(_IsMySelectedProperty, True)
                                        t.IsSelected = True
                                        SelectedList.Add(t)
                                    End If
                                Next
                            End If
                        End If
                    Else
                        If wasMySelected Then
                            Me.SetValue(_IsMySelectedProperty, False)
                            SelectedList.Remove(Me)
                        Else
                            Me.SetValue(_IsMySelectedProperty, True)
                            SelectedList.Add(Me)
                        End If

                    End If
                End If

                'UpdateStatusMsgDbg("# sel = " + SelectedList.Count().ToString + " " + Me.ToString)
            Else ' rt-click context menu: unselect multi
                'UpdateStatusMsgDbg("rt-click context menu # sel = " + SelectedList.Count().ToString + " " + Me.ToString)
                lamUnSelectAll.Invoke()
                Me.SetValue(_IsMySelectedProperty, True)
                SelectedList.Add(Me)
            End If
            'Dim ndx = 0
            'For Each it In SelectedList
            '    UpdateStatusMsgDbg(String.Format("---{0} {1}", ndx, it))
            '    ndx += 1
            'Next
            _IsInSelectedItemChanged = False
        End Sub

        Protected Friend Sub DumpChildren(ByVal sb As StringBuilder, ByVal nDepth As Integer, Optional fShowSelected As Boolean = False)
            Dim sIndent = String.Empty
            For i = 0 To nDepth - 1
                sIndent += " "
            Next
            sb.AppendLine(sIndent + If(fShowSelected AndAlso Me.IsSelected, "*SELECTED*", String.Empty) + Me.ToString)

            If Me.IsExpanded Then
                For Each item As MyTreeViewItem In Me.Items
                    item.DumpChildren(sb, nDepth + 1, fShowSelected) ' recur
                Next
            End If
        End Sub

        Protected Overridable Sub OnExpandAll()

        End Sub

        Protected Friend Sub ExpandAll()
            OnExpandAll()
            Me.IsExpanded = True
            For Each itm In Me.Items
                Dim tryc = TryCast(itm, MyTreeViewItem)
                If tryc IsNot Nothing Then
                    tryc.ExpandAll()
                Else
                    Dim bpt = 2
                End If
            Next
        End Sub

    End Class

    Public Enum TVSortOrder
        Name
        Size
        Count
    End Enum

    ''' <summary>
    ''' for seqno, large integers with Thousands separator
    ''' </summary>
    ''' <remarks></remarks>
    Public Class TxtBoxNumeric
        Inherits TextBox
        Public Sub New()
            Width = WIDTH_SEQNO
            Dim bding = New Binding With {
                .Source = Me,
                .Converter = New NumericalConverter,
                .UpdateSourceTrigger = UpdateSourceTrigger.LostFocus,
                .Mode = BindingMode.TwoWay,
                .Path = New PropertyPath(TextBox.TextProperty)
            }
            Me.SetBinding(TextBox.TextProperty, bding)
        End Sub
        Public Sub SetText(ByVal newText As String) ' so not recursive
            'Dim b = Me.GetBindingExpression(TextBox.TextProperty)
            'Me.SetBinding(TextBox.TextProperty, b)
            Me.Text = newText
        End Sub
        Public Function GetSeqNo() As UInteger
            Dim retInt As UInteger = 0
            If UInteger.TryParse(Me.Text.Trim, Globalization.NumberStyles.AllowThousands, Globalization.CultureInfo.CurrentCulture, retInt) Then

            End If
            Return retInt
        End Function
    End Class

    Public Module CommonUI

        Public _windowMain As MainWindow ' will be out of proc parent main window: for in proc, is nothing
        Friend _DidAddStutusEventHandler As Boolean = False
        Friend Sub InitializeCommonUI() ' need to init, esp for auto tests which use same instance of assembly
            Common.Initialize()

            Images.AppDomainLoaderClear()

            _txtStatus = New TextBox With {
                    .AcceptsReturn = True,
                    .AcceptsTab = True,
                    .IsReadOnly = True,
                    .IsUndoEnabled = False,
                    .VerticalScrollBarVisibility = ScrollBarVisibility.Auto,
                    .Text = String.Empty
               }

            _txtStatBar = New TextBlock With {.Text = "statbar",
                                              .Background = Brushes.LightGray
                                             }
            If Not _DidAddStutusEventHandler Then
                _DidAddStutusEventHandler = True
                AddHandler StatusMessageEvent, AddressOf CommonUIStatusMsgEventHandler
            End If
            '_txtStatus.DataContext = Common._statusHistory
            _ChildWindows = New List(Of ChildWindows)

            _FindLeaksWindow = Nothing
            TVObjRefPanel._fCombineFromMeToMe = False
            DataWindowMain._DataWindowMain = Nothing
            DataWindowMain._DataSurfaceNumber = 0
            Common._ExpandStringContents = True
            MemSpectWin._btnLastSelected = TrkType.HeapCreates ' choose a small one so faster by default
            SortableTVPanel.g_InitInvertStack = False
            MemoryEater._EatenAllocations.Clear()

            Common.OfflineMegaSnapshot.LoadSnapshotSub = AddressOf CommonUI.OfflineMegaSnapshotUI.LoadMegaSnapshot

        End Sub

        Public Structure ChildWindows
            Public m_CloseOnUnFreeze As Boolean
            Public m_Window As Window
        End Structure

        Private _FindLeaksWindow As FindLeaksWindow
        Private Property FindLeaksWindow As FindLeaksWindow
            Get
                If _FindLeaksWindow Is Nothing Then
                    _FindLeaksWindow = New FindLeaksWindow
                End If
                Return _FindLeaksWindow
            End Get
            Set(ByVal value As FindLeaksWindow)
                _FindLeaksWindow = value
            End Set
        End Property

        Public _VBDiagMarginBase As VBDiagMarginBase
        Public _ChildWindows As List(Of ChildWindows)
        Public Const WIDTH_ADDRESS As Integer = 72 ' width in Browses
        Public Const WIDTH_GROUP As Integer = 65

        Public Const WIDTH_SEQNO As Integer = 80 ' width in Browses

        Public Const TIP_ADDRESS As String = "Address. Hover over address to get callstack info/mem dump in a tip. Dbl-click to get tip in NotePad"
        Public Const TIP_GROUP As String = "ID # for custom allocation grouping, such as leaks."
        Public TIP_SEQNO As New TextBlock With {
            .Text = "Sequence number for chronological ordering "
        }
        Public TIP_THREAD As New TextBlock With {
            .Text = "The thread on which the allocation occurred. 'M' indicates the thread on which MemSpectDll.Dll was injected (usually UI thread)"
            }
        Public TIP_SIZE As New TextBlock With {
            .Text = "the size of the allocation"
            }
        Public TIP_STRING_CONTENT As New TextBlock With {
            .Text = "an attempt to show various string formats, such as unicode/bstr/ascii/ATL::CSimpleString"
            }


        Public Class MyWindow
            Inherits Window
            Private _WinType As String
            Sub New(ByVal sTitle As String, Optional ByVal WinType As String = "View", Optional ByVal fCloseOnUnFreeze As Boolean = True)
                Me.Title = sTitle
                Me._WinType = WinType
                '                Me.Owner = _windowMain
                '               Me.ShowInTaskbar = False
                Select Case WinType
                    Case "View"
                        Me.Left = My.Settings.ViewWindowLoc.X
                        Me.Top = My.Settings.ViewWindowLoc.Y
                        Me.Width = My.Settings.ViewWindowSize.Width
                        Me.Height = My.Settings.ViewWindowSize.Height
                        Me.WindowState = Windows.WindowState.Normal ' set to normal first (so it goes to correct monitor, then maximize)
                        If CType(My.Settings.ViewWindowState, WindowState) = Windows.WindowState.Maximized Then
                            Dim tmr As DispatcherTimer = Nothing
                            tmr = New DispatcherTimer(
                                      TimeSpan.FromMilliseconds(10),
                                      DispatcherPriority.Background,
                                      Sub()
                                          If Me IsNot Nothing Then
                                              Me.WindowState = Windows.WindowState.Maximized
                                          End If
                                          tmr.Stop()
                                      End Sub,
                                      Me.Dispatcher)
                            tmr.Start()
                        End If
                        If Me.Height > System.Windows.SystemParameters.VirtualScreenHeight Then
                            Me.Height = System.Windows.SystemParameters.VirtualScreenHeight
                        End If
                        If Me.Width > System.Windows.SystemParameters.VirtualScreenWidth Then
                            Me.Width = System.Windows.SystemParameters.VirtualScreenWidth
                        End If
                        If Me.Top + Me.Height / 2 > System.Windows.SystemParameters.VirtualScreenHeight Then
                            Me.Top = System.Windows.SystemParameters.VirtualScreenHeight - Me.Height
                        End If
                        If Me.Left + Me.Width / 2 > System.Windows.SystemParameters.VirtualScreenWidth Then
                            Me.Left = System.Windows.SystemParameters.VirtualScreenWidth - Me.Width
                        End If
                        If Me.Top < 0 Then
                            Me.Top = 0
                        End If
                        If Me.Left < 0 Then
                            Me.Left = 0
                        End If
                    Case "Dlog"
                        Me.Left = My.Settings.DlogWindowLoc.X
                        Me.Top = My.Settings.DlogWindowLoc.Y
                        Me.Width = My.Settings.DlogWindowSize.Width
                        Me.Height = My.Settings.DlogWindowSize.Height
                    Case Else
                        Me.Left = My.Settings.ElseWindowLoc.X
                        Me.Top = My.Settings.ElseWindowLoc.Y
                        Me.Width = My.Settings.ElseWindowSize.Width
                        Me.Height = My.Settings.ElseWindowSize.Height
                End Select
                If _ChildWindows IsNot Nothing Then
                    _ChildWindows.Add(New ChildWindows With {.m_Window = Me, .m_CloseOnUnFreeze = fCloseOnUnFreeze})
                End If
            End Sub
            Public Shared Function CreateFrom(ByVal oWin As Window, Optional ByVal WinType As String = "View", Optional ByVal fCloseOnUnFreeze As Boolean = True) As MyWindow
                Dim oWinRet As New MyWindow(oWin.Title, WinType, fCloseOnUnFreeze)
                Dim tmp = oWin.Content
                oWin.Content = Nothing
                oWinRet.Content = tmp
                Return oWinRet
            End Function

            Sub On_Closed() Handles Me.Closed
                Select Case _WinType
                    Case "View"
                        My.Settings.ViewWindowLoc = New System.Drawing.Point(CInt(Me.Left), CInt(Me.Top))
                        My.Settings.ViewWindowSize = New System.Drawing.Size(CInt(Me.Width), CInt(Me.Height))
                        If Me.WindowState <> Windows.WindowState.Minimized Then
                            My.Settings.ViewWindowState = CInt(Me.WindowState)
                        End If
                    Case "Dlog"
                        My.Settings.DlogWindowLoc = New System.Drawing.Point(CInt(Me.Left), CInt(Me.Top))
                        My.Settings.DlogWindowSize = New System.Drawing.Size(CInt(Me.Width), CInt(Me.Height))
                    Case Else
                        My.Settings.ElseWindowLoc = New System.Drawing.Point(CInt(Me.Left), CInt(Me.Top))
                        My.Settings.ElseWindowSize = New System.Drawing.Size(CInt(Me.Width), CInt(Me.Height))
                End Select
                My.Settings.Save()
                If Not _IsShuttingDown Then
                    '                _ChildWindows.Remove(Me)
                End If
            End Sub

            Protected Overrides Sub OnMouseWheel(ByVal e As MouseWheelEventArgs)
                MyBase.OnMouseWheel(e)
                'MsgBox("onmousewh")
            End Sub

            Sub on_mouswheel(ByVal o As Object, ByVal e As MouseWheelEventArgs) Handles Me.MouseWheel
                'MsgBox("on_mouse")
            End Sub
            Sub on_previewmousewheelevent(ByVal o As Object, ByVal e As MouseWheelEventArgs) Handles Me.PreviewMouseWheel
                '    MsgBox("onprev")
            End Sub
        End Class

        Public MustInherit Class MyActivity
            Inherits NativeActivity
            Protected Sub New()
                DataWindowMain.SetCursor(Cursors.AppStarting)
            End Sub
        End Class



        Public Class ShowSnapActivity
            Inherits MyActivity
            Property InHeap As InArgument(Of CSpyHeap)
            Protected Overrides Sub Execute(ByVal context As System.Activities.NativeActivityContext)
                Dim hp As CSpyHeap = InHeap.Get(context)
                DataWindowMain.SetCursor(Cursors.Wait)
                ShowSnapshot(hp, hp.TakeMemSnapshot(fEnableFilter:=True))
                DataWindowMain.SetCursor(Cursors.Arrow)
            End Sub
        End Class


        Public Class CreateMegaSnapActivity
            Inherits MyActivity
            Property InSnapFolder As InArgument(Of String)
            Protected Overrides Sub Execute(ByVal context As System.Activities.NativeActivityContext)
                Dim SnapFolder As String = InSnapFolder.Get(context)
                DataWindowMain.SetCursor(Cursors.Wait)
                UpdateStatusMsg("Start OfflineSnap " + SnapFolder)
                Common.OfflineMegaSnapshot.CreateMegaSnapshot(SnapFolder)
                DataWindowMain.SetCursor(Cursors.Arrow)
            End Sub
        End Class

        Public Class DataSurface
            Public Property TopSurface As TabItem
            Public Property SurfaceHeader As DockPanel
            Public Property SurfaceDetails As DockPanel

            'Sets the text displayed in a tab for the DataSurface
            Public Sub SetTabHeaderText(ByVal newText As String)
                Dim headerSP = CType(TopSurface.Header, StackPanel)
                CType(headerSP.Children(0), TextBlock).Text = newText
            End Sub

            'Gets the text displayed in a tab for the DataSurface
            Public Function GetTabHeaderText() As String
                Dim headerSP = CType(TopSurface.Header, StackPanel)
                Return CType(headerSP.Children(0), TextBlock).Text
            End Function
        End Class


        Public Function GetMemSpectHyperLink(Optional fontSize? As Double = Nothing) As StackPanel
            Dim lblsp = CreateHyperLinkControl(
                String.Format("Ver. {0}. For latest, see ", _MemSpectUICurrentVersion(fIncludeTimeStamp:=True)),
                "http://calvinh6/MemSpect",
                fontSize
            )
            Return lblsp
        End Function
        Public Function CreateHyperLinkControl(lbl As String, uri As String, Optional fontSize? As Double = Nothing) As StackPanel
            Dim lnktblk = New TextBlock With {
                .Text = lbl
            }
            Dim mylink = New MyHyperlink(uri) With {.Margin = New Windows.Thickness(0, -5, 0, 0)}
            If fontSize.HasValue Then
                lnktblk.FontSize = fontSize.Value
                mylink.FontSize = fontSize.Value
            End If
            Dim lblsp = New StackPanel With {.Orientation = Orientation.Horizontal}
            lblsp.Children.Add(lnktblk)
            lblsp.Children.Add(mylink)
            Return lblsp

        End Function

        Public Class DataWindowMain
            Inherits Window
            Public Shared WithEvents _DataWindowMain As Window ' the single window into which we'll put our data as tabs
            Public Shared _DataSurfaceNumber As Integer
            Friend Shared WithEvents _TabControl As TabControl
            Public Shared Function MakeNewDatasurface(
                                              ByVal strShortName As String,
                                              ByVal strToolTip As String,
                                              ByVal nMaxHeaderHeight As Integer
                                              ) As DataSurface
                If _DataWindowMain Is Nothing Then
                    _DataWindowMain = New MyWindow(GetWindowTitle(), "View", fCloseOnUnFreeze:=True)

                    _TabControl = New TabControl

                    Dim dp As New DockPanel
                    dp.Children.Add(_TabControl)
                    _DataWindowMain.Content = dp

                    If Application.Current IsNot Nothing Then
                        Dim win = Application.Current.MainWindow
                        If win IsNot Nothing Then
                            Dim mainCtrls = TryCast(win.Content, MainWindow.VBDiag)
                            If mainCtrls IsNot Nothing Then
                                win.Content = Nothing
                                win.Visibility = Windows.Visibility.Hidden
                                Dim ctrlsMain = DataWindowMain.MakeNewDatasurface("Main", "Main menu and heaplist", nMaxHeaderHeight:=0) ' recur
                                ctrlsMain.TopSurface.Content = mainCtrls
                                AddHandler _DataWindowMain.Closed, Sub()
                                                                       Try
                                                                           If _ConnectionMode = MemSpectMode.OnLine Then
                                                                               win.Content = mainCtrls
                                                                               win.Visibility = Windows.Visibility.Visible
                                                                           Else
                                                                               win.Close()
                                                                           End If
                                                                       Catch ex As Exception
                                                                           'Cannot set visibility or call Show, ShowDialog, or WindowInteropHelper.EnsureHandle after a wWindow has closed
                                                                       End Try
                                                                   End Sub
                            End If
                        End If
                    End If
                End If
                If _ShowUI Then
                    AddHandler _DataWindowMain.ContentRendered, Sub()
                                                                    If _DataWindowMain IsNot Nothing Then ' user could have clicked Close before rendered,
                                                                        _DataWindowMain.Topmost = False
                                                                    End If
                                                                End Sub
                    AddHandler _DataWindowMain.Initialized, Sub()
                                                                _DataWindowMain.Topmost = True
                                                            End Sub
                    _DataWindowMain.Show()
                    _DataWindowMain.Activate()
                End If
                Dim xaml = _
        <TabItem
            xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
            xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
            >
            <Grid>
                <Grid.RowDefinitions>
                    <RowDefinition Height=<%= nMaxHeaderHeight.ToString %>/>
                    <RowDefinition Height="Auto"/>
                    <RowDefinition Height="*"/>
                </Grid.RowDefinitions>
                <GridSplitter
                    ResizeDirection="Rows"
                    Grid.Row="1"
                    Width="Auto"
                    Height="3"
                    HorizontalAlignment="Stretch"
                    VerticalAlignment="Stretch"
                    Margin="0"/>

                <DockPanel Grid.Row="0" Name="UserControlDP">
                </DockPanel>
                <DockPanel Name="ctrlBrowseMem" Grid.Row="2"/>
            </Grid>
        </TabItem>



                Dim tbitem = CType(System.Windows.Markup.XamlReader.Load(xaml.CreateReader), TabItem)
                tbitem.Tag = "Top" ' flag to distinguish our tabitems from internal ones

                _DataSurfaceNumber += 1

                'Creates the content control for the tab that shows the DataSurface name and a close button.
                Dim tbHeaderPanel = New StackPanel With {
                    .Orientation = System.Windows.Controls.Orientation.Horizontal
                }
                Dim tbHeader = New TextBlock With {
                    .Text = strShortName + " #" + _DataSurfaceNumber.ToString
                }
                Dim tbHeaderCloseButton = New Button With {
                    .Content = New TextBlock With {.Text = "r",
                                                    .TextAlignment = System.Windows.TextAlignment.Center,
                                                    .VerticalAlignment = System.Windows.VerticalAlignment.Center,
                                                    .HorizontalAlignment = System.Windows.HorizontalAlignment.Center,
                                                   .IsEnabled = False
                                                  },
                    .VerticalAlignment = System.Windows.VerticalAlignment.Center,
                    .HorizontalAlignment = System.Windows.HorizontalAlignment.Center,
                    .FontFamily = New FontFamily("Marlett"),
                    .BorderThickness = New Thickness(0),
                    .Background = Brushes.Transparent,
                    .Width = 15,
                    .Height = 15,
                    .Margin = New Thickness(5, 2, 2, 2)
                }
                Dim closeButtonStyle = New Style With {
                    .TargetType = GetType(Button)
                    }
                Dim trigger = New Trigger With {.Property = IsMouseOverProperty, .Value = True}
                trigger.Setters.Add(New Setter With {.Property = BackgroundProperty, .Value = Brushes.Pink})

                closeButtonStyle.Triggers.Add(trigger)

                tbHeaderCloseButton.Style = closeButtonStyle

                'handles the close of a DataSurface
                AddHandler tbHeaderCloseButton.Click, Sub(o As Object, e As RoutedEventArgs)
                                                          ' RtClick Tabitem 
                                                          Dim r = GetAncestor(Of TabItem)(CType(e.OriginalSource, DependencyObject))
                                                          If r IsNot Nothing Then
                                                              If r.Tag IsNot Nothing AndAlso r.Tag.GetType = GetType(String) AndAlso CStr(r.Tag) = "Top" Then
                                                                  _TabControl.Items.Remove(r)
                                                                  If _TabControl.Items.Count = 0 Then
                                                                      _DataWindowMain.Close()
                                                                  End If
                                                              End If
                                                          End If
                                                      End Sub

                tbHeaderPanel.Children.Add(tbHeader)
                tbHeaderPanel.Children.Add(tbHeaderCloseButton)

                If _TabControl.SelectedItem IsNot Nothing Then
                    Dim srcTabItem = CType(_TabControl.SelectedItem, TabItem)
                    Dim headerPanel = CType(srcTabItem.Header, StackPanel)
                    Dim headertb = CType(headerPanel.Children(0), TextBlock)
                    Dim headerTxt = headertb.Text
                    strToolTip = If(String.IsNullOrEmpty(strToolTip), String.Empty, strToolTip + vbCrLf) + String.Format("(prior tab: '{0}')", headerTxt)
                End If
                If Not String.IsNullOrEmpty(strToolTip) Then
                    tbHeader.ToolTip = strToolTip
                End If

                tbitem.Header = tbHeaderPanel
                _TabControl.Items.Add(tbitem)
                _TabControl.SelectedItem = tbitem
                Dim ds = GetDataSurface()
                Return ds
            End Function
            ''' <summary>
            ''' 
            ''' </summary>
            ''' <param name="nSurface"> Optional. defaults to -1= means most recent</param>
            ''' <returns></returns>
            ''' <remarks></remarks>
            Friend Shared Function GetDataSurface(Optional ByVal nSurface As Integer = -1) As DataSurface
                If nSurface = -1 Then
                    nSurface = _TabControl.Items.Count - 1
                End If
                Dim tbitem = CType(_TabControl.Items(nSurface), TabItem)
                Dim userctrlDP = CType(tbitem.FindName("UserControlDP"), DockPanel)
                'todo: potential bug?  is it possible to add a data surface that has a different XAML layout, thus doesn't contain ctrlBrowseMem?
                Dim ctrlData = CType(tbitem.FindName("ctrlBrowseMem"), DockPanel)
                Dim ds = New DataSurface With {
                    .TopSurface = tbitem,
                    .SurfaceHeader = userctrlDP,
                    .SurfaceDetails = ctrlData
                    }
                'tbitem.RenderTransform = New ScaleTransform(1, 1)
                'AddHandler tbitem.MouseWheel, Sub(sender As Object, e As MouseWheelEventArgs)
                '                                  Dim tr = CType(tbitem.RenderTransform, ScaleTransform)
                '                                  If e.Delta > 0 Then
                '                                      tr.ScaleX *= 1.1
                '                                      tr.ScaleY *= 1.1
                '                                  Else
                '                                      tr.ScaleX /= 1.1
                '                                      tr.ScaleY /= 1.1
                '                                  End If
                '                                  e.Handled = True

                '                              End Sub
                'AddHandler tbitem.Unloaded, Sub(o, e)
                '                                UpdateStatusMsg("Unloaded " + o.ToString + ":" + e.ToString)
                '                            End Sub
                Return ds
            End Function

            Public Class SetCursorWait
                Implements IDisposable

#Region "IDisposable Support"
                Private disposedValue As Boolean ' To detect redundant calls
                Friend wndProgBar As ProgBarWindow
                Public Sub New(Optional ByVal fUseProgBar As Boolean = False)
                    DataWindowMain.SetCursor(Cursors.Wait)
                    If fUseProgBar Then
                        If wndProgBar Is Nothing Then
                            If Not _IsUnderTest Then
                                wndProgBar = New ProgBarWindow("MemSpect", String.Empty, _windowMain, fCanCancel:=False, fIsIndeterminate:=True)
                            End If
                        End If

                    End If
                End Sub
                ' IDisposable
                Protected Overridable Sub Dispose(ByVal disposing As Boolean)
                    If Not Me.disposedValue Then
                        If disposing Then
                            DataWindowMain.SetCursor(Cursors.Arrow)
                            If wndProgBar IsNot Nothing Then
                                wndProgBar.Close()
                                wndProgBar = Nothing
                            End If
                        End If
                    End If
                    Me.disposedValue = True
                End Sub

                ' This code added by Visual Basic to correctly implement the disposable pattern.
                Public Sub Dispose() Implements IDisposable.Dispose
                    ' Do not change this code.  Put cleanup code in Dispose(ByVal disposing As Boolean) above.
                    Dispose(True)
                    GC.SuppressFinalize(Me)
                End Sub
#End Region

            End Class
            Shared Sub SetCursor(ByVal inputcursor As Cursor, Optional ByVal uiObj As FrameworkElement = Nothing, Optional ByVal fForce As Boolean = True)
                ' need to allow Cursors.AppStarting
                'UpdateStatusMsgDbg("SetCurs " + inputcursor.ToString + " F=" + fForce.ToString)
                Dim curCursor As Cursor = Nothing
                If fForce Then
                    curCursor = Cursors.Arrow
                Else
                    curCursor = If(_windowMain Is Nothing, Cursors.Arrow, _windowMain.Cursor)
                End If
                If curCursor Is Cursors.AppStarting AndAlso inputcursor Is Cursors.Arrow Then
                    ' do nothing: caller finished, but bgd hasn't
                Else
                    If _DataWindowMain IsNot Nothing Then
                        _DataWindowMain.Dispatcher.Invoke(Sub()
                                                              _DataWindowMain.Cursor = inputcursor
                                                          End Sub)
                    End If

                    If _windowMain IsNot Nothing Then
                        _windowMain.Dispatcher.Invoke(Sub()
                                                          _windowMain.Cursor = inputcursor
                                                      End Sub)
                    End If
                    If uiObj IsNot Nothing Then
                        uiObj.Dispatcher.Invoke(Sub()
                                                    uiObj.Cursor = inputcursor
                                                End Sub)
                    End If
                End If
            End Sub

            Shared Sub On_Key(ByVal o As Object, ByVal e As KeyEventArgs) Handles _TabControl.KeyUp
                If e.Key = Key.F4 AndAlso e.KeyboardDevice.Modifiers = ModifierKeys.Control AndAlso _TabControl.SelectedItem IsNot Nothing Then
                    RemoveCurrentTab()
                End If

            End Sub
            Friend Shared Sub RemoveCurrentTab()
                _TabControl.Items.Remove(_TabControl.SelectedItem)
                If _TabControl.Items.Count = 0 Then
                    _DataWindowMain.Close()
                End If
            End Sub

            Friend Shared Sub On_Close() Handles _DataWindowMain.Closed
                If _VBDiagMarginBase IsNot Nothing Then
                    _VBDiagMarginBase._oFilterTab = Nothing
                End If
                If _ChildWindows IsNot Nothing Then
                    _ChildWindows.Clear()
                End If
                _TabControl = Nothing
                _DataWindowMain = Nothing
            End Sub

            Friend Shared Sub RemoveTab(ByVal oFilterTab As TabItem)
                _TabControl.Items.Remove(oFilterTab)
                If _TabControl.Items.Count = 0 Then
                    _DataWindowMain.Close()
                End If
            End Sub

            Shared Sub CloseWindows()
                If _DataWindowMain IsNot Nothing Then
                    _DataWindowMain.Close()
                    _DataWindowMain = Nothing
                End If
            End Sub

        End Class

        Public Class MyHyperlink
            Inherits Label
            Public Sub New(ByVal txt As String, Optional ByVal uri As Uri = Nothing)
                If uri Is Nothing Then
                    uri = New Uri(txt)
                End If
                VerticalAlignment = Windows.VerticalAlignment.Top
                Dim hLink = New Hyperlink With {.NavigateUri = uri}
                hLink.Inlines.Add(txt)
                AddHandler hLink.RequestNavigate,
                    Sub(oo As Object, ee As System.Windows.Navigation.RequestNavigateEventArgs)
                        Process.Start(New ProcessStartInfo(ee.Uri.AbsoluteUri))
                    End Sub

                Me.Content = hLink

            End Sub
        End Class

        Public Sub SearchForLeaksInAllocs(ByVal allocs As IEnumerable(Of HeapAllocationContainer))
            If Not FindLeaksWindow.IsLoaded Then
                FindLeaksWindow = New FindLeaksWindow(FindLeaksWindow.LeakDetector.DetectionParameters)
            End If

            FindLeaksWindow.SearchForLeaksInAllocs(allocs)
        End Sub

        Public Function ShowSubSnapShot(ByVal lst As List(Of HeapAllocationContainer),
                                        ByVal subsnapName As String,
                                        Optional ByVal queryFunc As BrowQueryDelegate = Nothing,
                                        Optional ByVal tblkType As TrkType = TrkType.All,
                                        Optional ByVal fEnableFilter As Boolean = False) As DataSurface
            Dim ctrls As DataSurface = Nothing

            If lst.Count > 0 Then
                Dim imagedata = TryCast(lst(0), Images.Imagedata)
                If imagedata IsNot Nothing Then
                    Dim lstImages = New List(Of Images.Imagedata)
                    For Each itm As Images.Imagedata In lst
                        lstImages.Add(itm)
                    Next
                    Dim imgui = New ImageUi(vm:=Nothing)
                    Dim bmem = imgui.ShowImageAllocs(lstImages)
                    ctrls = DataWindowMain.MakeNewDatasurface("ImgSub", subsnapName, nMaxHeaderHeight:=50)
                    ctrls.SurfaceHeader.Children.Add(
                        New TextBlock With {
                            .Text = subsnapName
                        }
                    )

                    ctrls.SurfaceDetails.Children.Add(bmem)
                    Return ctrls
                End If
            End If

            Dim newSnap = MemSnapshot.CreateMemSnapshotFromListAllocs(hp:=Nothing, srcAllocs:=lst, fEnableFilter:=fEnableFilter)

            If newSnap.SpyHeap IsNot Nothing Then
                subsnapName = subsnapName + " " + newSnap.SpyHeap.HeapName
            End If

            ctrls = ShowSnapshot(newSnap.SpyHeap, newSnap, subsnapName, queryFunc, tblkType:=tblkType)
            Return ctrls
        End Function

        Public Function ShowSnapshot(ByVal heap As CSpyHeap,
                                     ByVal snap As MemSnapshot,
                                     Optional ByVal title As String = "",
                                     Optional ByVal queryFunc As BrowQueryDelegate = Nothing,
                                     Optional ByVal tblkType As TrkType = TrkType.All) As DataSurface

            Dim fDoMemSpectWin = heap IsNot Nothing AndAlso heap.IsMemSpectHeap AndAlso queryFunc Is Nothing
            If tblkType <> TrkType.All Then
                fDoMemSpectWin = True
            End If
            Dim strtip = title
            If String.IsNullOrEmpty(title) Then
                strtip = String.Format("   Snapshot Heap={0} Created = {1} # items ={2}  TotMem= {3:n0} ",
                                                       heap.HeapName,
                                                       snap.Timestamp.ToString,
                                                       snap.nCnt,
                                                       snap.nTotMem)
            End If

            Dim tabName = If(title.StartsWith("Sub"), "Sub", String.Empty) + "Snap" + If(fDoMemSpectWin, "M", String.Empty)
            Dim ctrls = DataWindowMain.MakeNewDatasurface(tabName, strtip, nMaxHeaderHeight:=40)
            Try
                UpdateStatusMsg("ShowSnapshot " + tabName, msgType:=StatusMessageType.StatusBarEntry)
                '                Dim ctrls = CreateWindowAndControls(nMaxHeight:=40)
                Dim sp As New StackPanel With {.Orientation = Orientation.Vertical}
                ctrls.SurfaceHeader.Children.Add(sp)
                sp.Children.Add(New TextBlock With {
                    .Text = If(title = String.Empty, snap.Timestamp.ToString + " " + heap.GetHeapStatistics + " " + _GlobalFilter.ToString, title)
                })
                Dim ctrlTxtTotal = New TextBlock
                sp.Children.Add(ctrlTxtTotal)

                If fDoMemSpectWin Then
                    'Dim nCnt = 0
                    'For Each alloc In From aa In snap.Allocs Where aa.TblkBlockType = BlockTypes.ClrObject

                    '    Dim dw = ReadProcessMemoryDWORDEx(alloc.GetAddr.MyAdd(alloc.GetSize - 4))
                    '    If dw = 0 Then
                    '        nCnt += 1
                    '    End If
                    'Next
                    'Debug.Assert(False, String.Format("# allocs={0}   # Last0={1}", snap.Allocs.Count, nCnt))

                    Dim ovmTrkWin = New MemSpectWin(snap, ctrlTxtTotal)
                    ctrls.SurfaceDetails.Children.Add(ovmTrkWin)
                Else
                    Dim colwidths = {WIDTH_ADDRESS, WIDTH_SEQNO, 60, 60, 900}
                    Dim qfunc = queryFunc
                    If qfunc Is Nothing Then
                        If heap IsNot Nothing AndAlso heap.IsArenaHeap Then
                            qfunc = Function(theheapAllocs As List(Of HeapAllocationContainer), thebmem As BrowseMem) As IEnumerable
                                        Dim qDetails As IEnumerable
                                        qDetails = From a In theheapAllocs
                                                   Let ainfo = a.GetArenaAllocationInfo(a)
                                                   Let ahdr = ainfo.ArenaHeaderInfo
                                            Select
                                                Address = ainfo.ArenaAllocAddress.ToString("x8"),
                                                a.AllocationStruct.SeqNo,
                                                Size = ainfo.ArenaAllocSize,
                                                a.AllocationStruct.Thread,
                                                ahdr.ArenaId,
                                                ahdr.ArenaName,
                                                ArenaBlockType = ainfo.ArenaBlockType.ToString,
                                                UserData = ainfo.ArenaAllocUserDefinedData.ToInt32.ToString("x8"),
                                                ahdr.CntEverAlloc,
                                                ahdr.SizeEverAlloc,
                                                ahdr.CntCurLive,
                                                ahdr.SizeCurLive,
                                                StringContent = a.GetStringContent,
                                                _HeapAllocationContainer = a
                                            Order By SeqNo

                                        'qDetails = From a In theheapAllocs
                                        '           Select
                                        '        Address = a.GetAddr.ToString("x8"),
                                        '        a.AllocationStruct.SeqNo,
                                        '        Size = 10,
                                        '        a.AllocationStruct.Thread,
                                        '        _HeapAllocationContainer = a
                                        '    Order By SeqNo


                                        thebmem._ColWidths = {WIDTH_ADDRESS, WIDTH_SEQNO, 60, 60, 60, 120, 80, 80, 80, 80, 80, 80, 900}
                                        thebmem.ColumnTips = {TIP_ADDRESS, TIP_SEQNO,
                                                              "Header size for a  header, Allocation size for an allocation",
                                                              TIP_THREAD,
                                                              "Arena Header ID: 1,2,3",
                                                              "Arena Name as specified in call to ArenaCreate",
                                                              "BlockType is Header (for creation of an arena) or Allocation (for individual allocations from an arena)",
                                                              "User data as specified in call to ArenaCreate",
                                                              "Count of total # of allocations ever made in this arena",
                                                              "Cnt Size of total # of allocations ever made in this arena",
                                                              "Current # of live allocations in this arena",
                                                              "Size of current live allocations in this arena",
                                                              TIP_STRING_CONTENT}
                                        thebmem._arrColumnsToTotal = {"Size", "CntEverAlloc", "SizeEverAlloc", "CntCurLive", "SizeCurLive"}
                                        thebmem.InitialSortOrder = New BrowseInitialSortOrder With {.ColumnNo = 2, .direction = ListSortDirection.Ascending}
                                        Return qDetails
                                    End Function
                        Else
                            qfunc = Function(theheapAllocs As List(Of HeapAllocationContainer), thebmem As BrowseMem) As IEnumerable
                                        Dim qDetails As IEnumerable
                                        qDetails = From a In theheapAllocs
                                            Select
                                                Address = a.GetAddr.ToString("x8"),
                                                a.AllocationStruct.SeqNo,
                                                Size = a.GetSize,
                                                a.AllocationStruct.Thread,
                                                StringContent = a.GetStringContent,
                                                _HeapAllocationContainer = a
                                            Order By SeqNo

                                        thebmem._ColWidths = {WIDTH_ADDRESS, WIDTH_SEQNO, 60, 60, 900}
                                        thebmem.ColumnTips = {TIP_ADDRESS, TIP_SEQNO, TIP_SIZE, TIP_THREAD, TIP_STRING_CONTENT}
                                        thebmem._arrColumnsToTotal = {"Size"}
                                        thebmem.InitialSortOrder = New BrowseInitialSortOrder With {.ColumnNo = 2, .direction = ListSortDirection.Ascending}
                                        Return qDetails
                                    End Function
                        End If
                    End If
                    Dim bmem = New BrowseMem(qfunc, snap.Allocs, fAllowBrowStringFilter:=True, colwidths:=colwidths, tblkType:=tblkType)
                    ctrls.SurfaceDetails.Children.Add(bmem)
                    ctrlTxtTotal.Text = String.Format("Total Cnt= {0:n0} Size = {1:n0}", bmem.nTotal, bmem.nTotalSize)
                End If
                SymbolFiles.CheckSymbolFileStatus()

            Catch ex As Exception
                MemSpectExceptionHandler(ex)
            End Try
            Return ctrls
        End Function

        'Dynamically adds the GroupID column to a given data surface for the desired groupID identifier.
        Public Sub ShowGroupIDForDataSurface(ByVal dataSurface As DataSurface, ByVal groupIDIndex As Integer)
            For Each child As Object In dataSurface.SurfaceDetails.Children
                If child.GetType() Is GetType(BrowseMem) Then
                    CType(child, BrowseMem).AddColumnForProperty("_HeapAllocationContainer.GroupIDs[" + groupIDIndex.ToString() + "]", WIDTH_GROUP, "GroupID", TIP_GROUP)
                ElseIf child.GetType() Is GetType(MemSpectWin) Then
                    CType(child, MemSpectWin).AddColumnForProperty("_HeapAllocationContainer.GroupIDs[" + groupIDIndex.ToString() + "]", WIDTH_GROUP, "GroupID", TIP_GROUP)
                End If
            Next
        End Sub


        Public Function GetListViewItemDataContextFromObj(ByVal obj As Object) As Object
            Dim dt As Object = Nothing
            Dim depobj = TryCast(obj, DependencyObject)
            If depobj IsNot Nothing Then
                Dim lvi = GetAncestor(Of ListViewItem)(depobj)
                If lvi IsNot Nothing Then
                    dt = lvi.DataContext
                End If
            End If
            Return dt
        End Function

        Sub OnHeapDblClick(ByVal sender As Object, ByVal e As RoutedEventArgs)
            Dim lv As Browse.BrowseList = Nothing
            Try
                Dim dt = GetListViewItemDataContextFromObj(e.OriginalSource)
                If IsNothing(dt) Then
                    Return
                End If
                lv = TryCast(sender, Browse.BrowseList)
                If IsNothing(lv) Then
                    Return
                End If
                InvokeContextMenu(lv.ContextMenu, "_Snapshot")
                'Return
                'lv.Cursor = Cursors.Wait
                'Dim tdesc = ComponentModel.TypeDescriptor.GetProperties(dt)("_Heap")
                'Dim heap = CType(tdesc.GetValue(dt), CSpyHeap)
                'Dim snap = heap.TakeMemSnapshot()
                'ShowSnapshot(heap, snap)
            Catch ex As Exception
                MemSpectExceptionHandler(ex)
            Finally
                If lv IsNot Nothing Then
                    lv.Cursor = Cursors.Arrow
                End If
            End Try
        End Sub

        Public Function ShowUnusedMembers(ByVal hCtrList As IEnumerable(Of HeapAllocationContainer), ByVal strDesc As String) As DataSurface
            Dim ctrls As DataSurface = Nothing
            Using w As New ProgBarWindow(strDesc, String.Empty, DataWindowMain._DataWindowMain, fCanCancel:=True, fIsIndeterminate:=False)

                If hCtrList.Count > 0 AndAlso hCtrList(0).IsMemSpectHeap AndAlso hCtrList(0).TBlkBlockType <> BlockTypes.ClrObject Then
                    strDesc += " Can't show unused members for " + hCtrList(0).TBlkBlockType.ToString
                    hCtrList = New List(Of HeapAllocationContainer)
                End If
                Dim orderedAllocs = From a In hCtrList
                                     Order By a.GetSize Descending

                Dim lrgst = orderedAllocs.FirstOrDefault
                Dim smallest = orderedAllocs.LastOrDefault
                Dim nUniqueClassIds = 0
                Dim ClassId = IntPtr.Zero

                Dim q As IEnumerable
                Dim strHelp = String.Empty
                If hCtrList.Count = 0 OrElse lrgst.GetSize > &H100000 Then
                    q = orderedAllocs
                    If hCtrList.Count = 0 Then
                    Else
                        strHelp = "Too big: largest alloc > 1M"
                    End If
                Else
                    'For each offset, we need a list of allocs with an unused member 0 at that offset
                    Dim offsetsdict = New SortedDictionary(Of Integer, Integer) ' offset, count of used
                    w._ProgBar.Maximum = lrgst.GetSize
                    Dim lastOffset = lrgst.GetSize
                    If lrgst.IsMemSpectHeap AndAlso lrgst.IsTrkBlkType(TrkType.ClrObjects) Then
                        lastOffset -= IntPtr.Size 'size of clro includes syncblk header at offset -4
                    End If
                    Dim stepSize = IntPtr.Size ' default to 4 bytes. If we have a class layout, we'll use the member size form the class layout
                    Dim iOffset = 0
                    Dim clsLayouts = ClrClassInfo.GetClassLayoutsFromClassId(ClassId) 'get a null list to start: ClassId == IntPtr.Zero
                    Dim clsIdParent = IntPtr.Zero
                    Dim clsSize = 0
                    Dim curMemberSize = IntPtr.Size

                    While iOffset < lastOffset
                        w.UpdateText(String.Format("processing {0} of {1}", iOffset, lrgst.GetSize() - 1))
                        w._ProgBar.Value = iOffset
                        For Each alloc In orderedAllocs
                            If alloc.IsMemSpectHeap AndAlso (alloc.TBlkBlockType = BlockTypes.CodeMarker OrElse
                                alloc.TBlkBlockType = BlockTypes.TlsAllocFree) Then
                            Else
                                If iOffset = 0 Then ' see if we have exactly 1 ClassID
                                    If alloc.IsMemSpectHeap AndAlso alloc.TBlkBlockType = BlockTypes.ClrObject Then
                                        If alloc.TBlk.UnionData1 <> ClassId.ToInt32 Then
                                            If ClassId = IntPtr.Zero Then ' first time thru
                                                ClassId = New IntPtr(alloc.TBlk.UnionData1)
                                                nUniqueClassIds = 1
                                                clsLayouts = ClrClassInfo.GetClassLayoutsFromClassId(ClassId)
                                                clsIdParent = IntPtr.Zero
                                                If clsLayouts.Count > 0 Then
                                                    clsIdParent = clsLayouts(clsLayouts.Count - 1).classIdParent
                                                    clsSize = clsLayouts(clsLayouts.Count - 1).classSize
                                                End If
                                            Else
                                                nUniqueClassIds += 1
                                                ClassId = IntPtr.Zero
                                                stepSize = IntPtr.Size
                                            End If
                                        End If
                                    End If
                                    Continue For ' no need to read the classid to see if it's unused
                                ElseIf iOffset > alloc.GetSize Then
                                    Exit For
                                End If
                                Dim dword = ReadProcessMemoryDWORDEx(alloc.GetAddr.MyAdd(iOffset))
                                If curMemberSize < IntPtr.Size Then
                                    dword = CInt((Math.Pow(256, curMemberSize) - 1)) And dword
                                End If
                                Dim valTouse = 0
                                If dword = 0 Then ' unused?
                                    valTouse = 1
                                Else
                                    Dim curoffset = iOffset + IntPtr.Size ' skip over 1st DWord
                                    While curoffset < curMemberSize
                                        dword = ReadProcessMemoryDWORDEx(alloc.GetAddr.MyAdd(curoffset))
                                        If dword = 0 Then ' unused?
                                            valTouse = 1
                                            Exit While
                                        End If
                                    End While
                                End If
                                If offsetsdict.ContainsKey(iOffset) Then
                                    offsetsdict(iOffset) += valTouse
                                Else
                                    offsetsdict(iOffset) = valTouse
                                End If
                            End If
                        Next
                        stepSize = curMemberSize
                        iOffset += stepSize
                        Dim tup = ClrClassInfo.GetMemberInformation(ClassId, iOffset)
                        If tup IsNot Nothing Then
                            curMemberSize = tup.Item2
                        End If
                    End While

                    'if the value = the count, then it's 100%. If = count/2, then %50
                    q = From anOffsetEntry In offsetsdict
                        Let fldInfo = ClrClassInfo.GetClsLayoutFromOffset(clsLayouts, anOffsetEntry.Key)
                            Select
                                Offset = anOffsetEntry.Key,
                                Name = fldInfo.FldName,
                                Type = fldInfo.FldType,
                                CountUnused = anOffsetEntry.Value,
                                PctUnused = CInt(anOffsetEntry.Value * 100.0 / hCtrList.Count)
                            Order By Offset

                    Dim agg = Aggregate alloc In offsetsdict Where alloc.Value = hCtrList.Count Into Count()

                    strHelp = String.Format(
                        "Largest alloc = {0:n0} Smallest alloc = {1:n0}  # Allocs = {2:n0} # Members with 100% empty {3:n0} Potential savings={4:n0} bytes",
                                lrgst.GetSize,
                                smallest.GetSize,
                                hCtrList.Count,
                                agg,
                                agg * IntPtr.Size * hCtrList.Count
                                )
                    strHelp += String.Format(vbCrLf + "Class Size = {0}  ParentId = {1:x8} {2}",
                                             clsSize,
                                             clsIdParent.ToInt32,
                                             ClrClassInfo.GetClassNameFromClassOrObjectId(clsIdParent, fExpandSystemStringOrArray:=False)
                                             )


                End If

                Dim br = New Browse(q, fAllowBrowFilter:=True,
                                    ColWidths:={70, 700, 700},
                                    ColTips:={"Offset in base 10",
                                              "Member Name (Managed code)",
                                              "Member Type",
                                              "Count of members with zero values (Unused)",
                                              "Percent of members with zero (Unused)"
                                             }
                                    )
                ctrls = DataWindowMain.MakeNewDatasurface("Unused", strDesc, nMaxHeaderHeight:=65)

                Dim sp As New StackPanel With {.Orientation = Orientation.Vertical}
                sp.Children.Add(New TextBlock With {.Text = strDesc})
                sp.Children.Add(New TextBlock With {.Text = "Each offset with a 0 member"})
                sp.Children.Add(New TextBlock With {.Text = strHelp})

                ctrls.SurfaceHeader.Children.Add(sp)

                ctrls.SurfaceDetails.Children.Add(br)

                br._BrowseList.ContextMenu.AddMnuItem("Show member value distribution",
                                                      "For each member at a particular offset, show the range of values. Works best with member size = 4 bytes",
                                                      Sub(sender As Object, e As RoutedEventArgs)
                                                          Dim itm = br._BrowseList.SelectedItem
                                                          If itm IsNot Nothing Then
                                                              Dim tdesc = TypeDescriptor.GetProperties(itm)("Offset")
                                                              Dim offset = CInt(tdesc.GetValue(itm))
                                                              ShowMemberDistribution(
                                                                  orderedAllocs,
                                                                  offset,
                                                                  String.Format("Member distribution TotCnt={0} {1}", orderedAllocs.Count, itm))
                                                          End If
                                                          UpdateStatusMsg("foo" + br._BrowseList.SelectedItem.ToString)
                                                      End Sub,
                                                      InsertPos:=0
                )
            End Using

            Return ctrls

        End Function

        Public Function ShowMemberDistribution(
                                              ByVal hCtrList As IEnumerable(Of HeapAllocationContainer),
                                              ByVal nOffset As Integer,
                                              ByVal strDesc As String) As DataSurface
            Dim ctrls As DataSurface = Nothing

            Dim memSize = 0
            Using w As New ProgBarWindow(strDesc, String.Empty, DataWindowMain._DataWindowMain, fCanCancel:=True, fIsIndeterminate:=False)
                Dim dictMemValues = New Dictionary(Of Integer, Integer) ' value, count
                Dim q As IEnumerable
                For Each alloc In hCtrList
                    If memSize = 0 Then
                        Dim clsId = alloc.GetClassId
                        If clsId <> IntPtr.Zero Then
                            Dim tup = ClrClassInfo.GetMemberInformation(clsId, nOffset)
                            If tup IsNot Nothing Then
                                memSize = tup.Item2
                            Else
                                memSize = IntPtr.Size
                            End If
                        Else
                            memSize = IntPtr.Size
                        End If
                    End If
                    If nOffset - IntPtr.Size < alloc.GetSize Then
                        Dim dword = ReadProcessMemoryDWORDEx(alloc.GetAddr.MyAdd(nOffset))
                        If memSize <> IntPtr.Size Then
                            dword = CInt((Math.Pow(256, memSize) - 1)) And dword
                        End If
                        If Not dictMemValues.ContainsKey(dword) Then
                            dictMemValues(dword) = 1
                        Else
                            dictMemValues(dword) += 1
                        End If
                    End If
                Next

                q = From a In dictMemValues
                      Order By a.Key
                      Select Value = a.Key.ToString("x8"),
                      Count = a.Value


                Dim br = New Browse(q, fAllowBrowFilter:=True,
                                    ColWidths:={70, 70},
                                    ColTips:={"Value in hex", "Count"}
                                    )
                ctrls = DataWindowMain.MakeNewDatasurface("MemberDist", strDesc, nMaxHeaderHeight:=65)

                Dim sp As New StackPanel With {.Orientation = Orientation.Vertical}
                sp.Children.Add(New TextBlock With {.Text = strDesc})

                ctrls.SurfaceHeader.Children.Add(sp)

                ctrls.SurfaceDetails.Children.Add(br)
            End Using

            Return ctrls

        End Function


        Public Function ShowDuplicateAllocations(ByVal inputHeapList As IEnumerable(Of CSpyHeap),
                                                 ByVal inputhCtrList As IEnumerable(Of HeapAllocationContainer),
                                                 ByVal winName As String) As DataSurface
            Dim allocsUnsorted As New List(Of HeapAllocationContainer)
            ' Input:either a list of heaps or a list of allocations
            If inputhCtrList Is Nothing Then
                For Each hp In inputHeapList
                    Dim snap = hp.TakeMemSnapshot(fEnableFilter:=False)
                    Dim qhp As IEnumerable(Of HeapAllocationContainer)
                    If hp.IsMemSpectHeap Then
                        qhp = From a In snap.Allocs
                                 Where a.TBlkBlockType = BlockTypes.ClrObject OrElse a.TBlkBlockType = BlockTypes.HeapAlloc

                    Else
                        qhp = From a In snap.Allocs
                    End If
                    allocsUnsorted.AddRange(qhp)
                Next
                winName += " " + _GlobalFilter.ToString
            Else
                allocsUnsorted.AddRange(inputhCtrList)
            End If
            If allocsUnsorted.Count = 0 Then
                Throw New InvalidOperationException("No allocations in which to find duplicates")
            End If
            Dim allocs = From a In allocsUnsorted
                         Order By a Descending

            Dim PriorAlloc As HeapAllocationContainer = Nothing
            Dim nDupeID = 0
            Dim nDupeIndex = 0
            Dim firstalloc = allocs(0)
            Dim fIsforClrObjects = firstalloc.IsMemSpectHeap AndAlso firstalloc.TBlkBlockType = BlockTypes.ClrObject
            Dim dupeList = New List(Of DupeContainer)
            For Each alloc In allocs
                If PriorAlloc IsNot Nothing Then
                    Dim nRes = PriorAlloc.CompareTo(alloc)
                    If nRes = 0 Then
                        If nDupeIndex = 0 Then
                            nDupeIndex += 1
                            nDupeID += 1
                            dupeList.Add(New DupeContainer With {.Alloc = PriorAlloc, .DupeID = nDupeID, .DupeIndex = nDupeIndex})
                        End If
                        nDupeIndex += 1
                        dupeList.Add(New DupeContainer With {.Alloc = alloc, .DupeID = nDupeID, .DupeIndex = nDupeIndex})
                    Else
                        nDupeIndex = 0
                    End If
                End If
                PriorAlloc = alloc
            Next
            nDupeID = 0
            Dim GrpSize As Long = 0
            Dim grpTot = 0
            For Each dupe In From a In dupeList Order By a.DupeID Ascending, a.DupeIndex Descending
                If dupe.DupeID <> nDupeID Then
                    GrpSize = dupe.Alloc.GetSize * dupe.DupeIndex ' index of largest one
                    grpTot = dupe.DupeIndex
                    nDupeID = dupe.DupeID
                End If
                dupe.DupeTotalForID = GrpSize
                dupe.DupeCntForID = grpTot
            Next
            ''now we need to assign new IDs in new order
            nDupeID = 0
            Dim nNewId = 0
            For Each dupe In From a In dupeList Order By a.DupeTotalForID Descending

                If dupe.DupeID <> nDupeID Then
                    nNewId += 1
                    nDupeID = dupe.DupeID
                End If
                dupe.DupeID = nNewId
            Next
            Dim fDidAddDupeSizeDist = False

            Dim qfunc = Function(theheapAllocs As List(Of HeapAllocationContainer), thebmem As BrowseMem) As IEnumerable
                            ' theheapAllocs is nothing
                            Dim q = From dupe In dupeList
                                    Let hctr = dupe.Alloc
                                    Select
                                        Address = hctr.GetAddr.ToString("x8"),
                                        hctr.AllocationStruct.SeqNo,
                                        Size = hctr.GetSize,
                                        hctr.AllocationStruct.Thread,
                                        Type = hctr.TBlkBlockType.ToString,
                                        Data = hctr.GetDisplayData(HeapAllocationContainer.GetDisplayDataEnum.DisplayShortWithStringOrArrayExpansion),
                                        dupe.DupeID,
                                        dupe.DupeIndex,
                                        DupeData = String.Format("{0} of {1}", dupe.DupeIndex, dupe.DupeCntForID),
                                        DupeTot = dupe.DupeTotalForID,
                                        Heap = hctr.SpyHeapPtr.GetHeapName,
                                        _HeapAllocationContainer = hctr
                                    Order By DupeTot Descending

                            thebmem._arrColumnsToTotal = {"Size"}
                            thebmem.ColumnTips = {TIP_ADDRESS, TIP_SEQNO, TIP_SIZE, TIP_THREAD,
                                                "Type of allocation like managed (none=native)",
                                                "Data (like stringcontent)",
                                                "Dupe ID (All with the same id are dupes",
                                                "Index within a group ID",
                                                "Index Of Total Group Count",
                                                "Total size for the group ID"
                                                 }

                            If Not fDidAddDupeSizeDist Then
                                fDidAddDupeSizeDist = True
                                Dim tabDupeDist = thebmem.AddTabItemForBrowMem("DupeSizeDist",
                                                              "Distribution of duplicates by size",
                                                                       Sub(sndr As Object, ejit As RoutedEventArgs)
                                                                           Dim thetabItem = CType(sndr, TabItem)
                                                                           Dim qDupeDist = From dupe In dupeList
                                                                                           Select
                                                                                             dupe
                                                                                             Group By dupe.DupeID,
                                                                                                Data = dupe.Alloc.GetDisplayData(nMode:=HeapAllocationContainer.GetDisplayDataEnum.DisplayShortWithStringOrArrayExpansion),
                                                                                                DupeTot = dupe.DupeTotalForID
                                                                                             Into Count()
                                                                                             Select DupeID, Count, DupeTot, Data
                                                                                             Order By DupeTot Descending


                                                                           Dim br = New Browse(qDupeDist,
                                                                                                InitialSortOrder:=New BrowseInitialSortOrder With {.ColumnNo = 2, .direction = ComponentModel.ListSortDirection.Descending},
                                                                                                ColWidths:={65, 80, 80, 900},
                                                                                                ColTips:={"DupeID", "Dupe Size Count", "Total size for the group ID"},
                                                                                                arrColumnsToTotal:={"Count", "DupeTot"}
                                                                                                )

                                                                           br._BrowseList.ContextMenu.AddMnuItem(
                                                                               "_SubSnapshot",
                                                                               "Create a new snapshot from the selected items",
                                                                               Sub()
                                                                                   Dim items = br._BrowseList.SelectedItems
                                                                                   If items Is Nothing OrElse items.Count < 1 Then
                                                                                       items = br._BrowseList.Items
                                                                                   End If
                                                                                   Dim qAllocs = From itm In items
                                                                                                   Let tdescitem = ComponentModel.TypeDescriptor.GetProperties(itm)("DupeID"),
                                                                                                   DupeID = CType(tdescitem.GetValue(itm), Long)
                                                                                                   From dupe In dupeList
                                                                                                   Where dupe.DupeID = DupeID
                                                                                                   Select dupe.Alloc


                                                                                   ShowSubSnapShot(qAllocs.ToList, "SubSnapSizeDist")

                                                                               End Sub, 0)

                                                                           thetabItem.Content = br

                                                                       End Sub)

                            End If
                            Return q
                        End Function

            Dim ctrls = DataWindowMain.MakeNewDatasurface("Dupe", "Duplicates (Blocks identical in size and content, regardless of call stack) " + winName, nMaxHeaderHeight:=40)
            ctrls.SurfaceHeader.Children.Add(New TextBlock With {.Text = "Duplicates " + winName})

            Dim tblkType = TrkType.All
            If fIsforClrObjects Then
                tblkType = TrkType.ClrObjects
            End If
            Dim bmem = New BrowseMem(
                                        qfunc,
                                        Nothing,
                                        tblkType:=tblkType,
                                        fAllowBrowStringFilter:=True,
                                        ColWidths:={WIDTH_ADDRESS, WIDTH_SEQNO, 65, 65, 65, 600, 65, 65, 85, 120, 300}
                                    )
            bmem.DynamicBackgroundConverter = New DupeBackGroundConverter
            ctrls.SurfaceDetails.Children.Add(bmem)
            Return ctrls
        End Function


        Public Class DupeBackGroundConverter
            Implements IValueConverter

            Public Function Convert(ByVal value As Object, ByVal targetType As System.Type, ByVal parameter As Object, ByVal culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.Convert
                Dim lvitem = CType(value, ListViewItem)
                Dim result = CType(ComponentModel.TypeDescriptor.GetProperties(lvitem.DataContext)("DupeID").GetValue(lvitem.DataContext), Integer)
                If result Mod 2 = 0 Then
                    Return Brushes.Khaki
                End If
                Return Brushes.White
            End Function

            Public Function ConvertBack(ByVal value As Object, ByVal targetType As System.Type, ByVal parameter As Object, ByVal culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.ConvertBack
                Throw New NotImplementedException()
            End Function
        End Class
        Public Sub OpenSourceFilesRecur(ByVal itm As TVStackAggPanel.StackAggTreeView.StackAggTViewItem, Optional ByVal fRecur As Boolean = False)
            If itm._memNode IsNot Nothing Then
                ViewSourceCodeFile(itm._memNode._baseName)
            End If
            If fRecur Then
                If Not itm.IsExpanded Then
                    itm.IsExpanded = True
                End If
                For Each child As TVStackAggPanel.StackAggTreeView.StackAggTViewItem In itm.Items
                    OpenSourceFilesRecur(child, fRecur)
                Next
            End If
        End Sub

        Public Function ViewSourceCodeFile(ByVal strFrame As String) As String
            Dim strLocalFileName = String.Empty
            Dim pos = strFrame.IndexOf("(")
            If pos > 0 Then
                Dim rparen = strFrame.IndexOf(")")
                Dim origfilename = strFrame.Substring(0, pos)
                Dim lineno = 0
                Dim nn = Integer.TryParse(strFrame.Substring(pos + 1, rparen - pos - 1), lineno)
                Dim addr = IntPtr.Zero
                For Each itm In StackFrameDictionary ' backward search
                    If itm.Value = strFrame Then
                        addr = itm.Key
                        Exit For
                    End If
                Next
                If addr <> IntPtr.Zero Then
                    Dim sbLocalFileName As New StringBuilder(520)
                    Dim hProcToUseForSymbolResolution = -1

                    If _ConnectionMode = MemSpectMode.Offline Then
                        MiniDumpReader.Singleton.LoadModules()
                        hProcToUseForSymbolResolution = CInt(_MemSpectProcessHandle)
                    Else
                        hProcToUseForSymbolResolution = CInt(_hProcessTarget)
                    End If
                    'tf.exe view /version:1305500 /noprompt "$/Dev10/Releases/RTMRel/vb/Language/VsPackage/Interop/SyntaxTreeContainer.cpp" /server:http://vstfdevdiv.redmond.corp.microsoft.com:8080/ /console 
                    'http://msdn.microsoft.com/en-us/library/ff560066(VS.85).aspx  by setting the DBGHELP_HOMEDIR environment variable. If this subdirectory does not already exist, it will be created.
                    If VsSymGetSourceFile(CType(hProcToUseForSymbolResolution, IntPtr), addr, origfilename, sbLocalFileName, sbLocalFileName.Capacity) Then
                        '                    _VBAssert.OutputText("Bingo! got source " + sb.ToString)
                        strLocalFileName = sbLocalFileName.ToString

                        Dim ctrls = DataWindowMain.MakeNewDatasurface("Src", origfilename, nMaxHeaderHeight:=45)

                        'Dim txtboxFileContent = New TextBox With {
                        '        .AcceptsReturn = True,
                        '        .AcceptsTab = True,
                        '        .VerticalScrollBarVisibility = ScrollBarVisibility.Auto
                        '    }
                        'txtboxFileContent.AppendText(txt)
                        'txtboxFileContent.ScrollToLine(lineno)
                        'ctrls.SurfaceDetails.Children.Add(txtboxFileContent)
                        Dim simpEd = New SimpleEd(sbLocalFileName.ToString, lineno)
                        ctrls.SurfaceDetails.Children.Add(simpEd)


                        Dim btnNotepad = New Button With {
                            .Content = "_NotePad",
                            .ToolTip = "Open with Notepad"
                        }
                        ctrls.SurfaceHeader.Children.Add(btnNotepad)
                        AddHandler btnNotepad.Click, Sub()
                                                         Process.Start("notepad", sbLocalFileName.ToString)
                                                     End Sub


                        Dim btnDefault = New Button With {
                            .Content = "Open with _default",
                            .ToolTip = "Open with whatever is registered for the file extension, like .CPP->VS"
                        }
                        ctrls.SurfaceHeader.Children.Add(btnDefault)
                        AddHandler btnDefault.Click, Sub()
                                                         Process.Start(sbLocalFileName.ToString)
                                                     End Sub

                        'Dim btnGotoLine = New Button With {
                        '    .Content = "_GotoLine " + lineno.ToString,
                        '    .ToolTip = "Do whatever is registered for the file extension, like .CPP->VS"
                        '}
                        'ctrls.SurfaceHeader.Children.Add(btnGotoLine)
                        'AddHandler btnGotoLine.Click, Sub()
                        '                                  '                                                          txtboxFileContent.ScrollToLine(Math.Max(1, lineno - 5))
                        '                                  simpEd.on_Loaded()
                        '                              End Sub


                        Dim sp As New StackPanel With {
                                            .Orientation = Orientation.Vertical,
                                            .LayoutTransform = New ScaleTransform(0.6, 0.6)
                                                      }
                        sp.Children.Add(New TextBlock With {
                                                         .Text = strFrame + " "
                                                     })
                        sp.Children.Add(New TextBlock With {
                                                         .Text = origfilename + "  Line " + lineno.ToString + " "
                                                     })
                        sp.Children.Add(New TextBox With {
                                                         .Text = sbLocalFileName.ToString,
                                                        .IsReadOnly = True
                                                     })
                        ctrls.SurfaceHeader.Children.Add(sp)
                        'AddHandler txtboxFileContent.Loaded, Sub()
                        '                                         txtboxFileContent.ScrollToLine(Math.Max(1, lineno - 5))
                        '                                     End Sub

                    Else
                        UpdateStatusMsg("Src not found:" + origfilename)
                    End If
                End If

            End If
            Return strLocalFileName
        End Function

        Public Class SimpleEd
            Inherits DockPanel
            Friend _nLineNo As Integer
            Friend _br As Browse
            Sub New(ByVal LocalFileName As String, ByVal nLineNo As Integer)
                _nLineNo = nLineNo
                Dim lines = IO.File.ReadAllLines(LocalFileName)
                Dim ndx = 0
                Dim q = From line In lines
                        Select lineno = Function() As Integer
                                            ndx += 1
                                            Return ndx
                                        End Function.Invoke,
                                        line = line.Replace(vbTab, "    ")


                _br = New Browse(q, ColWidths:={60, 1000}, fAllowHeaderClickSort:=False)
                Dim britem = _br._BrowseList.Items(_nLineNo)
                _br._BrowseList.ScrollIntoView(britem)
                Me.Children.Add(_br)

            End Sub
            'Sub on_LostFocus(ByVal sender As Object, ByVal e As RoutedEventArgs) Handles _txtContent.LostFocus
            '    e.Handled = True ' so highlight selection not hidden
            'End Sub
            'Sub on_Loaded() Handles _txtContent.Loaded
            '    Try
            '        'Me._txtContent.SelectionStart = 0
            '        'Me._txtContent.SelectionLength = 3000
            '        '            MsgBox("Cartindex=" + _txtContent.CaretIndex.ToString + "selected txt = " + Me._txtContent.SelectedText)
            '        Dim nstart = Me._txtContent.GetCharacterIndexFromLineIndex(Math.Max(0, _nLineNo - 4))
            '        Dim nStartSel = _dictLines(Math.Max(0, _nLineNo - 4))
            '        Dim nEndSel = _dictLines(Math.Min(_nLineNo + 4, _nMaxLines))

            '        _txtContent.Select(nStartSel, nEndSel)
            '        '                    Me._txtContent.ScrollToLine(_nLineNo)

            '    Catch ex As Exception
            '        MemSpectExceptionHandler(ex)
            '    End Try

            'End Sub
        End Class

        <System.Runtime.ExceptionServices.HandleProcessCorruptedStateExceptions()>
        Public Function DoShowAllocationsWithWaste(
                                                  ByVal sTitle As String,
                                                  Optional ByVal srcAllocs As IEnumerable(Of HeapAllocationContainer) = Nothing
                                                ) As DataSurface

            VBDiagMarginBase.FreezeTargetThreads()
            Dim ctrls = DataWindowMain.MakeNewDatasurface("Waste", sTitle, nMaxHeaderHeight:=45)

            Try
                Dim tbktype = TrkType.All
                If srcAllocs IsNot Nothing AndAlso srcAllocs.Count > 0 Then
                    If srcAllocs(0).TBlkBlockType <> BlockTypes.None Then
                        _byteToMatch = 0     ' clr objects: use 0, rather than 0xAF as default
                        tbktype = TrkType.ClrObjects
                    Else
                        _byteToMatch = 175 ' &af
                    End If
                End If
                Dim txtMatchCount As New TextBox With {
                    .ToolTip = "HeapAllocs called without HEAP_ZERO_MEMORY are filled with 0xAF",
                    .Text = _nToMatchForUnused.ToString,
                    .Width = 60,
                    .MaxHeight = 30,
                    .VerticalAlignment = VerticalAlignment.Top
                }
                Dim txtByteMatch As New TextBox With {
                    .ToolTip = "HeapAllocs called without HEAP_ZERO_MEMORY are filled with 175 (0xAF). Or you can choose 0",
                    .Text = _byteToMatch.ToString,
                    .Width = 60,
                    .MaxHeight = 30,
                    .VerticalAlignment = VerticalAlignment.Top
                }
                Dim btnRefresh As New Button With {
                    .Content = "_Refresh",
                    .MaxWidth = 100,
                    .HorizontalAlignment = Windows.HorizontalAlignment.Left,
                    .MaxHeight = 30,
                    .VerticalAlignment = VerticalAlignment.Top
                }

                Dim lamRefresh = Sub()
                                     DataWindowMain._DataWindowMain.Cursor = Cursors.Wait

                                     ctrls.SurfaceDetails.Children.Clear()
                                     ' wastecnt, nBiggestRunLength, totalMatchCnt
                                     Dim allocsWithWaste = New List(Of Tuple(Of HeapAllocationContainer, Tuple(Of Integer, Integer, Integer)))
                                     Dim lamWasteInAlloc = Function(alloc As HeapAllocationContainer) As Integer
                                                               Dim nWastedTotal = 0 ' sum of all runs > nMatch
                                                               Dim nTotSize = alloc.GetSize
                                                               Dim isCommited = True
                                                               If alloc.IsMemSpectHeap Then
                                                                   Select Case alloc.TBlkBlockType
                                                                       Case BlockTypes.ClrObject
                                                                           nTotSize -= IntPtr.Size ' negative offset -4 for syncblock, so dec size by 4 and ignore syncblock of next obj
                                                                       Case BlockTypes.VirtualAlloc
                                                                           Dim allocState = CType(alloc.TBlk.UnionData2, AllocationState)
                                                                           If (allocState And AllocationState.MEM_COMMIT) = 0 Then ' if not commit, nothing to compare
                                                                               isCommited = False
                                                                           End If
                                                                   End Select
                                                               End If
                                                               Dim nTotMatches = 0
                                                               If nTotSize >= _nToMatchForUnused Then
                                                                   Dim blk As New ProcMemBlockByte
                                                                   Dim nBlkSize = Marshal.SizeOf(blk)
                                                                   Dim nOffset = 0
                                                                   Dim nBytesRead = 0
                                                                   Dim nBiggestUnused = 0
                                                                   Dim nMatchesSoFar = 0 ' look for consecutive matches
                                                                   Dim fPriorWasMatch = False
                                                                   While nTotSize > 0
                                                                       Dim nBytesToReadThisTime = Math.Min(nTotSize, nBlkSize)
                                                                       Dim addrToRead = alloc.GetAddr.MyAdd(nOffset)
                                                                       Select Case _ConnectionMode
                                                                           Case MemSpectMode.Offline
                                                                               blk.data = MiniDumpReader.Singleton.ReadMemoryDictionary(addrToRead, nBytesToReadThisTime)
                                                                               If blk.data IsNot Nothing Then
                                                                                   nBytesRead = blk.data.Length
                                                                               Else
                                                                                   nBytesRead = 0
                                                                               End If
                                                                           Case MemSpectMode.OnLine
                                                                               ReDim blk.data(nBytesToReadThisTime - 1)
                                                                               nBytesRead = ReadProcessMemoryAsByteArray(
                                                                                   _hProcessTarget,
                                                                                   addrToRead,
                                                                                   nBytesToReadThisTime,
                                                                                   1,
                                                                                   blk.data)
                                                                               If nBytesRead <= 0 Then
                                                                                   Throw New InvalidOperationException(
                                                                                       "error reading process memory " +
                                                                                       addrToRead.ToInt32.ToString("x8") + " " +
                                                                                       nBytesToReadThisTime.ToString())
                                                                               End If
                                                                               'If ReadProcessMemoryByte(_hProcessTarget,
                                                                               '                        addrToRead,
                                                                               '                        blk,
                                                                               '                        nBytesToReadThisTime,
                                                                               '                        nBytesRead
                                                                               '                        ) = 0 Then
                                                                               '    Exit While
                                                                               'End If
                                                                       End Select
                                                                       If Not isCommited Then ' we want to make RESERVE look like wasted too
                                                                           nBytesRead = nBytesToReadThisTime
                                                                           ReDim blk.data(nBytesToReadThisTime)
                                                                       End If
                                                                       For i = 0 To nBytesRead - 1
                                                                           If blk.data(i) = _byteToMatch Then
                                                                               nTotMatches += 1
                                                                               nMatchesSoFar += 1
                                                                               If nMatchesSoFar > nBiggestUnused Then
                                                                                   nBiggestUnused = nMatchesSoFar
                                                                               End If
                                                                               fPriorWasMatch = True
                                                                           Else
                                                                               If nMatchesSoFar >= _nToMatchForUnused Then
                                                                                   nWastedTotal += nMatchesSoFar
                                                                                   '                Exit While
                                                                               End If
                                                                               fPriorWasMatch = False
                                                                               nMatchesSoFar = 0
                                                                           End If
                                                                       Next
                                                                       nOffset += nBytesToReadThisTime
                                                                       nTotSize -= nBytesToReadThisTime
                                                                   End While
                                                                   If nMatchesSoFar >= _nToMatchForUnused Then
                                                                       nWastedTotal += nMatchesSoFar
                                                                   End If
                                                                   If nWastedTotal > 0 Then
                                                                       allocsWithWaste.Add(New Tuple(Of HeapAllocationContainer, Tuple(Of Integer, Integer, Integer))(alloc,
                                                                                                                                                             New Tuple(Of Integer, Integer, Integer)(nWastedTotal, nBiggestUnused, nTotMatches)))
                                                                   End If
                                                               End If
                                                               Return nWastedTotal
                                                           End Function
                                     Dim nTotWaste As Long = 0
                                     Dim nTotCumSize As Long = 0
                                     If srcAllocs Is Nothing Then
                                         For Each hp In _HeapList
                                             Dim snap = hp.TakeMemSnapshot(fEnableFilter:=True)
                                             If hp.IsMemSpectHeap Then
                                                 For Each alloc In From a In snap.Allocs.Where(
                                                                   Function(h) h.TBlkBlockType = BlockTypes.ClrObject)
                                                               Where a.GetSize > _nToMatchForUnused
                                                     nTotWaste += lamWasteInAlloc.Invoke(alloc)
                                                     nTotCumSize += alloc.GetSize
                                                 Next ' alloc
                                             Else
                                                 For Each alloc In From a In snap.Allocs Where a.GetSize > _nToMatchForUnused
                                                     nTotWaste += lamWasteInAlloc.Invoke(alloc)
                                                     nTotCumSize += alloc.GetSize
                                                 Next ' alloc
                                             End If
                                         Next ' heap
                                     Else
                                         For Each alloc In srcAllocs
                                             nTotWaste += lamWasteInAlloc.Invoke(alloc)
                                             nTotCumSize += alloc.GetSize
                                         Next
                                     End If

                                     ctrls.SurfaceHeader.Children.Clear()
                                     ctrls.SurfaceHeader.LayoutTransform = New ScaleTransform(0.8, 0.8)
                                     ctrls.SurfaceHeader.Children.Add(New Label With {.Content = "# of consecutive bytes of 175(0xAF) to match"})
                                     ctrls.SurfaceHeader.Children.Add(txtMatchCount)
                                     ctrls.SurfaceHeader.Children.Add(txtByteMatch)
                                     ctrls.SurfaceHeader.Children.Add(btnRefresh)
                                     Dim spVert As New StackPanel With {.Orientation = Orientation.Vertical}

                                     spVert.Children.Add(New Label With {.Content = sTitle})
                                     spVert.Children.Add(New TextBlock With {.Text = _GlobalFilter.ToString})
                                     spVert.Children.Add(New TextBlock With {
                                                                      .Text = String.Format("Waste bytes = {0:n0} TotBytes = {1:n0}  PctWaste={2:n0} ",
                                                                                           nTotWaste,
                                                                                           nTotCumSize,
                                                                                           If(nTotCumSize = 0, 0, CInt(nTotWaste * 100.0 / nTotCumSize))
                                                                  )
                                                                  })

                                     ctrls.SurfaceHeader.Children.Add(spVert)

                                     Dim qfunc = Function(theheapAllocs As List(Of HeapAllocationContainer), thebmem As BrowseMem) As IEnumerable
                                                     Dim q = From tup In allocsWithWaste
                                                                         Select hctr = tup.Item1,
                                                                            Waste = tup.Item2.Item1,
                                                                            BiggestUnused = tup.Item2.Item2,
                                                                            TotMatches = tup.Item2.Item3
                                                                         Select
                                                                             Address = hctr.GetAddr.ToString("x8"),
                                                                             hctr.AllocationStruct.SeqNo,
                                                                             Size = hctr.GetSize,
                                                                             hctr.AllocationStruct.Thread,
                                                                             WasteCnt = Waste,
                                                                             WastePct = CInt(100.0 * Waste / hctr.GetSize),
                                                                             BiggestUnused,
                                                                             BiggestUnusedPct = CInt(100.0 * BiggestUnused / hctr.GetSize),
                                                                             TotMatches,
                                                                             TotMatchPct = CInt(100.0 * TotMatches / hctr.GetSize),
                                                                             Heap = hctr.SpyHeapPtr.HeapName,
                                                                             StringContent = hctr.GetStringContent,
                                                                             _HeapAllocationContainer = hctr

                                                     thebmem.ColumnTips = {TIP_ADDRESS, TIP_SEQNO, TIP_SIZE, TIP_THREAD,
                                                                         "Sum of byte runs wasted > " + _nToMatchForUnused.ToString,
                                                                         "Percent of size",
                                                                           "Biggest Unused",
                                                                           "Biggest Unused %",
                                                                           "Sum of bytes matching",
                                                                           "% of bytes matching"
                                                                          }

                                                     thebmem._arrColumnsToTotal = {"Size", "WasteCnt", "BiggestUnused", "TotMatches"}
                                                     Return q
                                                 End Function

                                     Dim b = New BrowseMem(qfunc,
                                                           Nothing,
                                                           ColWidths:={WIDTH_ADDRESS, WIDTH_SEQNO, 65, 65, 65, 65, 65, 65, 65, 65, 210, 500},
                                                           fAllowBrowStringFilter:=True,
                                                           tblkType:=tbktype
                                                           )
                                     Dim sp As New StackPanel With {.Orientation = Orientation.Horizontal}
                                     Dim txt As New TextBlock
                                     ctrls.SurfaceDetails.Children.Add(b)
                                     DataWindowMain._DataWindowMain.Cursor = Cursors.Arrow

                                 End Sub
                AddHandler btnRefresh.Click, Sub()
                                                 _nToMatchForUnused = CInt(txtMatchCount.Text)
                                                 _byteToMatch = CInt(txtByteMatch.Text)
                                                 lamRefresh.Invoke()
                                             End Sub
                lamRefresh.Invoke()

            Catch ex As Exception
                MemSpectExceptionHandler(ex)
            End Try
            Return ctrls
        End Function


        Sub OnHeapMouseMove(ByVal sender As Object, ByVal e As RoutedEventArgs)
            Dim lv As Browse.BrowseList
            Try
                lv = TryCast(sender, Browse.BrowseList)
                Dim tb = TryCast(e.OriginalSource, TextBlock)
                If tb IsNot Nothing Then
                    Dim tdesc = ComponentModel.TypeDescriptor.GetProperties(tb.DataContext)("_Heap")
                    Dim heap = CType(tdesc.GetValue(tb.DataContext), CSpyHeap)
                    Dim lvi = GetAncestor(Of ListViewItem)(tb)
                    If lvi IsNot Nothing Then
                        '                    UpdateStatusMsg("lvi " + lvi.ToString)
                        If lvi.ToolTip Is Nothing Then
                            lvi.ToolTip = heap.ToString
                        End If
                    End If
                Else
                    '                lv.ToolTip = <xml>
                    'DblClick a native heap to see native memory use. 
                    'You can do this multiple times to create multiple snapshots
                    '__VMTrack is a special heap tracking Virtual Memory
                    '</xml>.Value
                End If
            Catch ex As Exception
                UpdateStatusMsg("Err " + ex.Message, fAssert:=True)
                MemSpectExceptionHandler(ex)
            End Try
        End Sub

        Public FontFamilyCourierNew As New FontFamily("Courier New")
        Public Class MonospacefontTextBlockForToolTip
            Inherits TextBlock
            Sub New()
                Me.FontFamily = FontFamilyCourierNew
                Me.FontSize = 8
                If Not SystemParameters.HighContrast Then
                    Me.Background = Brushes.LightYellow
                End If
            End Sub
        End Class

        Public _scaleX As Double = 1
        Public _scaleY As Double = 1

        Public Function GetTextFromTextBlock(tblock As TextBlock) As String
            Dim sb = New Text.StringBuilder
            For Each lin In tblock.Inlines
                If TryCast(lin, Documents.Run) IsNot Nothing Then
                    sb.Append(CType(lin, Documents.Run).Text)
                ElseIf TryCast(lin, Documents.Bold) IsNot Nothing Then
                    Dim bold = CType(lin, Documents.Bold)
                    sb.Append(CType(bold.Inlines(0), Documents.Run).Text)
                ElseIf TryCast(lin, Documents.Underline) IsNot Nothing Then
                    Dim underline = CType(lin, Documents.Underline)
                    sb.Append(CType(underline.Inlines(0), Documents.Run).Text)
                End If
            Next
            Return sb.ToString
        End Function

        Public Function GetAddressToolTip(ByVal dataContext As Object) As StackPanel
            Dim tblockStacks = New TextBlock
            Dim strSoFar = New StringBuilder
            Dim cback = New Action(Of String, Boolean)(Sub(Str, isbold)
                                                           If isbold Then
                                                               If strSoFar.Length > 0 Then
                                                                   tblockStacks.Inlines.Add(strSoFar.ToString()) ' add prior non-bold
                                                               End If
                                                               strSoFar.Clear()
                                                               Dim boldrun = New Run(Str)
                                                               boldrun.Foreground = Brushes.DarkBlue
                                                               tblockStacks.Inlines.Add(New Bold(boldrun))
                                                           Else
                                                               strSoFar.Append(Str)
                                                           End If
                                                       End Sub)
            ' handle any non-bold leftover

            Dim prStacksAndDump = GetStacksAndDump(dataContext, CallBack:=cback)
            If strSoFar.Length > 0 Then
                tblockStacks.Inlines.Add(strSoFar.ToString)
            End If

            Dim sframes = prStacksAndDump.Key
            Dim strAddrDump = prStacksAndDump.Value

            Dim fontsz = VBDiagMarginBase._FontSize + 1
            If sframes.Length > 40 Then
                fontsz = VBDiagMarginBase._FontSize + 1
            End If
            '            '            sframes = """adfasdf<Bold>text that is bold </Bold> not bold"""
            '            Dim XAMLStacks = _
            '<TextBlock
            '    xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
            '    xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
            '    xmlns:l="clr-namespace:MemSpect;assembly=MemSpect"
            '    FontSize=<%= fontsz %>
            '    Text=<%= sframes %>
            '    >
            '</TextBlock>
            '            'UpdateStatusMsg(sframes, msgType:=StatusMessageType.LogEntry)
            '            'UpdateStatusMsg(XAMLStacks.ToString(), msgType:=StatusMessageType.LogEntry)
            '            Dim tbxStacks = CType(Markup.XamlReader.Load(XAMLStacks.CreateReader()), TextBlock)
            '            tbxStacks.Inlines.Add("normal text")
            'tblockStacks.Inlines.Clear()
            'tblockStacks.Inlines.Add("normal text")

            'tblockStacks.Inlines.Add(New Bold(New Run("some bold text")))
            'tblockStacks.Inlines.Add("normal text2")

            'Dim tbxStacks = New TextBox With {
            '    .Text = sframes,
            '    .FontSize = fontsz,
            '    .FontWeight = FontWeights.Bold,
            '    .BorderThickness = New Windows.Thickness(0)
            '}

            Dim tbxDump = New TextBox With {
                .Text = strAddrDump,
                .BorderThickness = New Windows.Thickness(0),
                .FontFamily = FontFamilyCourierNew,
                .FontSize = 8
            }
            If Not SystemParameters.HighContrast Then
                tblockStacks.Background = Brushes.LightYellow
                tbxDump.Background = Brushes.LightYellow
            End If
            Dim sp = New StackPanel With {.Orientation = Orientation.Vertical}
            sp.Children.Add(tblockStacks)
            sp.Children.Add(New TextBlock With {.Text = "Dbl-Click address to see entire block & CallStack"})
            sp.Children.Add(tbxDump)
            Return sp
        End Function


        Public Function GetAncestor(Of T As DependencyObject)(ByVal element As DependencyObject, Optional ByVal fGoUpOneFirst As Boolean = False) As T
            Try
                Dim lamGetParent = Function(elem As DependencyObject) As DependencyObject
                                       If elem Is Nothing Then
                                           Return elem
                                       End If
                                       Dim contentelement = TryCast(elem, ContentElement)
                                       If contentelement IsNot Nothing Then
                                           Dim p = ContentOperations.GetParent(contentelement)
                                           If p IsNot Nothing Then
                                               Return p
                                           End If
                                           Dim fce = TryCast(contentelement, FrameworkContentElement)
                                           If fce IsNot Nothing Then
                                               Return fce.Parent
                                           End If
                                           Return Nothing
                                       End If
                                       Return VisualTreeHelper.GetParent(elem)
                                   End Function
                If fGoUpOneFirst Then
                    element = lamGetParent(element)
                End If
                While element IsNot Nothing
                    If TryCast(element, T) IsNot Nothing Then
                        Exit While
                    End If
                    element = lamGetParent(element)
                End While

            Catch ex As Exception
                MemSpectExceptionHandler(ex)
                Return Nothing
            End Try
            Return CType(element, T)
        End Function

        <Conditional("Debug")>
        Public Sub UpdateStatusMsgDbg(ByVal newStat As String)
            UpdateStatusMsg(newStat)
        End Sub


        Public Class HeapReportUI
            Inherits Common.HeapReport
            Public Sub New()
                MyBase.New()
            End Sub

            Public Sub New(ByVal fOnlyWantRegions As Boolean)
                MyBase.New(fOnlyWantRegions)
            End Sub

            Public Function DoHeapWalkMapReport(ByVal hHandle As IntPtr, ByVal strTitle As String) As DataSurface
                Dim hData As HeapData
                hData = GetHeapWalkData(hHandle)

                Dim hpHeapSpy = hData.heapSpy
                Dim allocs As List(Of HeapAllocationContainer)
                Dim snap As MemSnapshot
                If hpHeapSpy Is Nothing Then
                    Dim hpMemSpect = (From h In _HeapList Where h.IsMemSpectHeap).First
                    snap = hpMemSpect.TakeMemSnapshot(fEnableFilter:=False)
                    Dim tmpAllocs = From a In snap.Allocs
                                    Where a.TBlkBlockType = BlockTypes.HeapAlloc AndAlso a.TBlk.UnionData1 = hHandle.ToInt32

                    allocs = tmpAllocs.ToList

                Else
                    snap = hpHeapSpy.TakeMemSnapshot(fEnableFilter:=False)
                    allocs = snap.Allocs
                End If
                Dim qfunc = Function(theheapAllocs As List(Of HeapAllocationContainer), thebmem As BrowseMem) As IEnumerable
                                Dim q = From heapentry In hData.pelist
                                        Group Join alloc In allocs On alloc.AllocationStruct.Address Equals
                                        heapentry.lpData Into r = Group
                                        From alloc In r.DefaultIfEmpty
                                        Select
                                        Address = heapentry.lpData.ToInt32.ToString("x8"),
                                        SeqNo = If(alloc Is Nothing, CUInt(0), alloc.AllocationStruct.SeqNo),
                                        Size = If(alloc Is Nothing, heapentry.cbData, alloc.AllocationStruct.Size),
                                        Thread = If(alloc Is Nothing, String.Empty, alloc.AllocationStruct.Thread),
                                        cbData = heapentry.cbData,
                                        cbDataHex = heapentry.cbData.ToString("x8"),
                                        Flags = heapentry.wFlags.ToString.Substring(13),
                                        Ovhead = heapentry.cbOverhead.ToString(),
                                        RegionNdx = CInt(heapentry.iRegionIndex),
                                        CmtSize = heapentry.UnionBlock.Region.dwCommittedSize.ToString("n0"),
                                        UnCmtSize = heapentry.UnionBlock.Region.dwUnCommittedSize.ToString("n0"),
                                        pFirst = heapentry.UnionBlock.Region.lpFirstBlock.ToInt32.ToString("x8"),
                                        pLast = heapentry.UnionBlock.Region.lpLastBlock.ToInt32.ToString("x8"),
                                        _HeapAllocationContainer = alloc
                                        Order By Address

                                thebmem.ColumnTips = {TIP_ADDRESS, TIP_SEQNO, TIP_SIZE, TIP_THREAD,
                                                    "PROCESS_HEAP_ENTRY.cbData",
                                                    "PROCESS_HEAP_ENTRY.cbDatain hex",
                                                    "PROCESS_HEAP_ENTRY.wFlags",
                                                    "PROCESS_HEAP_ENTRY.cbOverhead",
                                                    "PROCESS_HEAP_ENTRY.iRegionIndex",
                                                    "PROCESS_HEAP_ENTRY.Region.dwCommittedSize",
                                                    "PROCESS_HEAP_ENTRY.Region.dwUnCommittedSize",
                                                    "PROCESS_HEAP_ENTRY.Region.lpFirstBlock",
                                                    "PROCESS_HEAP_ENTRY.Region.lpLastBlock"
                                                    }
                                thebmem.InitialSortOrder = New BrowseInitialSortOrder With {.ColumnNo = 1, .direction = ListSortDirection.Ascending}

                                thebmem._arrColumnsToTotal = {"Size", "cbData", "cbDataHex" + HexColumnSpecifier, "Ovhead", "UnCmtSize"}

                                Return q
                            End Function
                Dim ctrls = DataWindowMain.MakeNewDatasurface("HeapMap", "HeapWalkMap " + If(hpHeapSpy Is Nothing, String.Empty, hpHeapSpy.GetHeapName) + " Handle=" + hHandle.ToInt32.ToString("x8"), nMaxHeaderHeight:=40)
                Dim sp As New StackPanel With {.Orientation = Orientation.Vertical}
                ctrls.SurfaceHeader.Children.Add(sp)
                sp.Children.Add(New TextBlock With {
                    .Text = strTitle + vbCrLf + hData.ToString
                })

                sp.Children.Add(New TextBlock With {
                    .Text = snap.Timestamp.ToString + " " + If(hpHeapSpy Is Nothing, String.Empty, hpHeapSpy.GetHeapStatistics)
                })


                Dim cwidths = {WIDTH_ADDRESS, WIDTH_SEQNO, 65, 65, 65, 74, 130, 65, 74, 74, 74, 74}
                ctrls.SurfaceDetails.Children.Add(New BrowseMem(qfunc, snap.Allocs, ColWidths:=cwidths, fAllowBrowStringFilter:=True))
                'If Common._ConnectionMode = MemSpectMode.OnLine Then
                '    HeapReport._ProcessHeapData.Clear()
                'End If
                Return ctrls
            End Function

            Friend Function ShowResultsForOffline(ByVal report As Common.HeapReport) As DataSurface 'called from on_CtxMenuMain for offline
                _totHeapData = report._totHeapData
                Me._ProcessHeapData = report._ProcessHeapData
                Me._ProcessHeapHandles = report._ProcessHeapHandles
                Return ShowResults()
            End Function

            Friend Function ShowResults() As DataSurface ' called from on_CtxMenuMain for online

                Dim ctrls = DataWindowMain.MakeNewDatasurface("HeapDensity",
                                                              "Heap Density SeqNo=" + GetGlobalPassCount.ToString("n0") + " " + DateTime.Now.ToString,
                                                              nMaxHeaderHeight:=45)
                Dim sp As New StackPanel With {.Orientation = Orientation.Vertical}
                ctrls.SurfaceHeader.Children.Add(sp)
                sp.Children.Add(New TextBlock With {
                    .Text = "Heap Density: HeapWalk API is called. # Heaps=" + _ProcessHeapData.Count.ToString + " " + _totHeapData.ToString
                })
                sp.Children.Add(New TextBlock With {
                    .Text = String.Format("MemSpect heap handle={0:x8}  Memspect Debug Heap Handle={1:x8} Process Heap Handle= {2:x8}   ",
                                          ProcComm._hMemSpectHeap.ToInt32,
                                          ProcComm._hDebugHeap.ToInt32,
                                          ProcComm._hProcessHeap.ToInt32)
                })

                Dim q = From a In _ProcessHeapData
                        Order By a.hHeap.ToInt32
                        Select
                            Handle = a.hHeap.ToString("x8"),
                            HeapHandleSpy = If(a.heapSpy IsNot Nothing, a.heapSpy.HeapHandleSpy.ToInt32.ToString("x8"), String.Empty),
                            Heap = a.GetHeapNameForHeapData,
                            a.nTotSize,
                            a.nTotAllocs,
                            a.nComSize,
                            a.nUnComSize,
                            VMSize = a.nComSize + a.nUnComSize,
                            TotEverAlloc = If(a.heapSpy IsNot Nothing, a.heapSpy.TotMemAllocated, CUInt(0)),
                            Gaps = a.nGaps,
                            PctFull = If(a.nComSize = 0, 0, CInt(100.0 * (a.nTotSize / (a.nComSize + a.nUnComSize)))),
                            PctFrag = If(a.nTotAllocs = 0, 0, CInt(100.0 * ((a.nGaps - 1) / a.nTotAllocs))),
                            _HeapHandle = a.hHeap


                Dim ctips = {"Heap Handle",
                             "CHeapSpy ptr",
                             "HeapName is obtained from resolving symbol of caller to HeapCreate. There can be multiple with the same name",
                             "HeapWalk API results sum(size) for PROCESS_HEAP_ENTRY_WFLAGS.PROCESS_HEAP_ENTRY_BUSY",
                             "HeapWalk API results count() for PROCESS_HEAP_ENTRY_WFLAGS.PROCESS_HEAP_ENTRY_BUSY",
                             "HeapWalk API results Sum of dwCommittedSize for PROCESS_HEAP_ENTRY_WFLAGS.PROCESS_HEAP_ENTRY_REGION",
                             "HeapWalk API results Sum of dwUnCommittedSize for PROCESS_HEAP_ENTRY_WFLAGS.PROCESS_HEAP_ENTRY_REGION",
                             "Sum of nComSize + nUnComSize",
                             "Sum of all allocations intercepted by MemSpect. Not decremented when freed. Some heaps had allocations before MemSpect was injected",
                             "Gaps: Allocations sorted by address: count of distances > ExpectedGap. " +
                             vbCrLf + "ExpectedGap is 12 for MemSpect intercepted heaps, 28 for others",
                             "100 * nTotSize / (nComSize + nUnComSize)",
                             "100 * nGaps / nTotAllocs"
                             }
                Dim b = New Browse(q,
                                  fAllowHeaderClickSort:=True,
                                  ColWidths:={70, 70, 250, 90, 90, 90, 90, 90, 90, 90, 90, 50, 50, 50},
                                  ColTips:=ctips,
                                  arrColumnsToTotal:={"nTotSize", "nTotAllocs", "nComSize", "nUnComSize", "VMSize", "TotEverAlloc", "Gaps"})
                Dim lamCtxmenu = Sub(sender As Object, e As RoutedEventArgs)
                                     Dim mitem = CType(e.OriginalSource, MenuItem)
                                     Dim selItem = b._BrowseList.SelectedItem
                                     Dim tdesc = ComponentModel.TypeDescriptor.GetProperties(selItem)("_HeapHandle")
                                     Dim hSelected = CType(tdesc.GetValue(selItem), IntPtr)
                                     Dim vrb = mitem.Header.ToString
                                     Select Case vrb
                                         Case "_HeapWalkMap"
                                             DoHeapWalkMapReport(hSelected, selItem.ToString)
                                     End Select
                                 End Sub

                Dim cm = b._BrowseList.ContextMenu
                cm.AddMnuItem("_HeapWalkMap", "Create a heap map report", lamCtxmenu, 0)

                ctrls.SurfaceDetails.Children.Add(b)
                If _ConnectionMode = MemSpectMode.OnLine Then
                    _ProcessHeapData.Clear()
                    _ProcessHeapHandles.Clear()
                End If
                Return ctrls
            End Function

        End Class


        ''' <summary>
        ''' Adds ctx menu items to a ctxmenu. Wraps with exception handling and wait cursor
        ''' works for any ctx menu, like treeview or listview
        ''' </summary>
        ''' <param name="mnu"></param>
        ''' <param name="itmName"></param>
        ''' <param name="tip"></param>
        ''' <param name="hndler"></param>
        ''' <param name="InsertPos"></param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <System.Runtime.CompilerServices.Extension()>
        Public Function AddMnuItem(ByVal mnu As ContextMenu,
                                       ByVal itmName As String,
                                       ByVal tip As String,
                                       ByVal hndler As RoutedEventHandler,
                                       Optional ByVal InsertPos As Integer = -1) As MenuItem
            Dim mitem = New MenuItem With {
                 .Header = itmName,
                 .ToolTip = tip
             }

            Dim lamHandler = Sub(sender As Object, e As RoutedEventArgs)
                                 Try
                                     DataWindowMain.SetCursor(Cursors.Wait)
                                     hndler.Invoke(sender, e)
                                 Catch ex As Exception
                                     MemSpectExceptionHandler(ex)
                                 End Try
                                 DataWindowMain.SetCursor(Cursors.Arrow, fForce:=False)  ' allow Cursors.AppStarted
                             End Sub

            AddHandler mitem.Click, lamHandler
            If InsertPos = -1 Then
                mnu.Items.Add(mitem)
            Else
                mnu.Items.Insert(InsertPos, mitem)
            End If
            Return mitem
        End Function
        ''' <summary>
        ''' dumps output to temp file, shows in associated app like notepad
        ''' returns filename
        ''' </summary>
        ''' <param name="strToOutput"></param>
        ''' <param name="fExt"></param>
        Public Function WriteOutputToTempFile(ByVal strToOutput As String, Optional ByVal fExt As String = "txt", Optional ByVal fShowInNotepad As Boolean = True) As String
            Dim tmpFilename = IO.Path.GetTempFileName
            Dim fileOut = Path.ChangeExtension(tmpFilename, fExt) ' final file name
            Try
                If fExt = "csv" Then
                    File.WriteAllText(tmpFilename, strToOutput)
                Else
                    File.WriteAllText(tmpFilename, strToOutput, New UnicodeEncoding(bigEndian:=False, byteOrderMark:=True))
                End If
                File.Move(tmpFilename, fileOut) ' rename
                MoveFileEx(tmpFilename, Nothing, MOVEFILE_DELAY_UNTIL_REBOOT) ' delete on next reboot (only as admin or local system)
                'HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Session Manager\PendingFileRenameOperations
            Catch ex As Exception
                UpdateStatusMsg("exception saving temp file " + ex.ToString)
            End Try

            If _ShowUI AndAlso fShowInNotepad Then
                If fExt = "txt" Then
                    Process.Start("notepad", fileOut) ' intentionally use notepad, rather than "ini" file start due to shell cotask "leak"
                Else
                    If MessageBox.Show("Data written to " + fileOut + ".  Start associated app? (Excel or notepad)", String.Empty, MessageBoxButton.YesNo, MessageBoxImage.Question) = MessageBoxResult.Yes Then
                        Process.Start(fileOut)
                    Else
                        If MessageBox.Show("Copy filename to clipboard ?" + fileOut, String.Empty, MessageBoxButton.YesNo, MessageBoxImage.Question) = MessageBoxResult.Yes Then
                            System.Windows.Clipboard.SetText(fileOut)
                        End If
                    End If
                End If
            End If
            Return fileOut
        End Function

        Public Sub WaitForOtherThreads(ByVal nmSecs As Integer)
            If nmSecs > 0 Then
                Dim wtch = DateTime.Now
                Do Until (DateTime.Now - wtch).Duration > TimeSpan.FromMilliseconds(nmSecs)
                    Dispatcher.CurrentDispatcher.Invoke(
                        DispatcherPriority.Background,
                        Function()
                            System.Threading.Thread.Sleep(100)
                            Return 0
                        End Function)
                Loop
            End If
        End Sub

        Public Function MemSpectExceptionHandler(ByVal ex As Exception, Optional ByVal strDesc As String = " ") As MessageBoxResult
            Dim str = Common.MemSpectExceptionHandler(ex, strDesc)

            UpdateStatusMsg(str)

            If _IsUnderTest Then
                '                Throw ex
            End If
            Dim res = MessageBox.Show(str, "MemSpect Exception " + Process.GetCurrentProcess.Id.ToString, MessageBoxButton.OK)
            'If res = MessageBoxResult.Cancel Then
            '    DataWindowMain.On_Close()
            '    End
            'End If
            Return res
        End Function

        Public Sub InvokeContextMenu(ByVal cMenu As ContextMenu, ByVal menuItemToInvoke As String)
            Dim mitem = CType((From itm In cMenu.Items
                        Where CType(itm, MenuItem).Header.ToString = menuItemToInvoke).First, MenuItem)

            mitem.RaiseEvent(New RoutedEventArgs(MenuItem.ClickEvent, mitem))
        End Sub



        Public _txtStatus As TextBox
        Public _txtStatBar As TextBlock

        Public Class OfflineMegaSnapshotUI
            Public Class MegaSnapshotLoaderAct
                Inherits MyActivity
                Property snapFullPathProp As InArgument(Of String)
                Protected Overrides Sub Execute(ByVal context As System.Activities.NativeActivityContext)
                    Try
                        Dim snapFullPath = snapFullPathProp.Get(context)
                        Common.OfflineMegaSnapshot.LoadMegaSnapshot(fWaitTilDone:=True, snapName:=snapFullPath)

                    Catch ex As Exception
                        MemSpectExceptionHandler(ex, "Exception while loading snapshot")
                    End Try
                End Sub
            End Class

            Public Shared Event LoadMegaSnapshotCompletedEvent()

            Public Shared Sub LoadMegaSnapshot(ByVal fWaitTilDone As Boolean, snapFullPathName As String)
                If fWaitTilDone Then
                    Common.OfflineMegaSnapshot.LoadMegaSnapshot(fWaitTilDone:=True, snapName:=snapFullPathName)
                    ReadHeaps()
                Else
                    Debug.Assert(Not _IsLoadingOfflineSnapshot, "Loading snap again?")
                    _IsLoadingOfflineSnapshot = True
                    Dim inArgs = New Dictionary(Of String, Object) From {{"snapFullPathProp", snapFullPathName}}

                    Dim wfapp = New WorkflowApplication(New MegaSnapshotLoaderAct, inArgs)
                    UpdateStatusMsg("Starting Snap load '" + snapFullPathName + "'...")
                    'wfapp.SynchronizationContext = System.Threading.SynchronizationContext.Current
                    DataWindowMain.SetCursor(Cursors.AppStarting)
                    wfapp.Completed = Sub()
                                          _IsLoadingOfflineSnapshot = False
                                          CommonUI._windowMain.Dispatcher.BeginInvoke(Sub()
                                                                                          DataWindowMain.SetCursor(Cursors.Arrow, fForce:=True)
                                                                                          RaiseEvent LoadMegaSnapshotCompletedEvent()
                                                                                      End Sub)
                                      End Sub
                    wfapp.Run()
                End If
            End Sub

        End Class

        Public Function InitCommWithUI(ByVal args() As String) As String
            Dim retVal = String.Empty
            CommonUI.InitializeCommonUI()

            If args.Count < 2 Then ' user didn't specify any args, so we'll put up the launcher UI
                GetArgumentsFromLauncherUI(args)
            End If

            If Not ProcComm.AreArgumentsValid(args) Then
                retVal = GetValidArgumentsFromUI(args)

                If Not String.IsNullOrEmpty(retVal) Then
                    Return retVal
                End If
            End If

            retVal = ProcComm.InitComm(args)

            If Common._ConnectionMode = MemSpectMode.QuitMemspect Then
                End
            End If
            If Not String.IsNullOrEmpty(retVal) AndAlso _txtStatus IsNot Nothing Then
                retVal += vbCrLf + "StatusLog:" + vbCrLf + _txtStatus.Text
            End If
            Return retVal
        End Function

        Function GetValidArgumentsFromUI(ByRef args() As String) As String
            If args(1).Length > 1 AndAlso (args(1).StartsWith("/") Or args(1).StartsWith("-") Or Not Char.IsDigit(args(1)(1))) Then
                If args(1).Length > 1 Then
                    If "/-".IndexOf(args(1)(0)) >= 0 Then
                        Select Case Char.ToLower(args(1)(1))
                            Case "m"c
                                Array.Resize(args, 3)
                                Dim dlg = New System.Windows.Forms.OpenFileDialog
                                dlg.DefaultExt = "dmp"
                                If dlg.ShowDialog <> Forms.DialogResult.OK OrElse Not File.Exists(dlg.FileName) Then
                                    Return GetHelpText()
                                End If
                                args(2) = dlg.FileName


                                Return String.Empty
                            Case "o"c
                                Array.Resize(args, 3)
                                Dim dlg = New System.Windows.Forms.FolderBrowserDialog
                                dlg.SelectedPath = IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly.Location)
                                If dlg.ShowDialog() <> Forms.DialogResult.OK Then
                                    Return GetHelpText()
                                End If
                                args(2) = dlg.SelectedPath
                                Common.OfflineMegaSnapshot.LoadSnapshotSub = AddressOf CommonUI.OfflineMegaSnapshotUI.LoadMegaSnapshot

                                Return String.Empty
                            Case "p"c
                                args(1) = CStr(ShowProcessChooser("Choose a process that was launched under Memspect"))
                            Case "d"c
                                If args.Length < 3 Then
                                    Array.Resize(args, 3)
                                    args(2) = CStr(ShowProcessChooser("Choose a process for which to create a MiniDump", f32bitOnly:=False))
                                End If
                                _TargetProcessId = CInt(args(2))
                                If _TargetProcessId <> 0 Then
                                    _TargetProc = Process.GetProcessById(_TargetProcessId)
                                    _hProcessTarget = _TargetProc.Handle

                                    _ConnectionMode = MemSpectMode.OnLine ' pretend we're online while making minidump
                                    Dim minidumpFile = Path.ChangeExtension(Path.Combine(Path.GetTempPath, _TargetProc.ProcessName), ".dmp")

                                    Dim res = Common.OfflineMegaSnapshot.CreateMiniDumpFile(minidumpFile)
                                    If res.Contains("fail") Then
                                        Return res + " Err creating Minidump"
                                    End If

                                    args(1) = "/m" ' change mode to minidump
                                    args(2) = minidumpFile
                                    _ConnectionMode = MemSpectMode.MiniDumpOnly ' now we're in minidump mode
                                    _IsClientOutOfProcess = False

                                    '_OfflineDataFilePath = IO.Path.GetDirectoryName(args(2))
                                    _MiniDumpFileName = minidumpFile

                                    If ProcessType(_TargetProc) <> "32" Then
                                        Return "Minidump created: " + minidumpFile + vbCrLf +
                                            "Can't view 32 bit process " + _TargetProc.ProcessName
                                    End If

                                End If

                                Return String.Empty
                            Case "x"c
                                Array.Resize(args, 3)
                                args(2) = CStr(ShowProcessChooser("Choose a process for which to examine VM"))
                                Return String.Empty
                            Case "c"c
                                Array.Resize(args, 4)
                                'need 2 arguments, the PID and the directory to dump the snapshot.
                                'arguments can come in either order. if either is omitted, UI is shown
                                'to get the argument.
                                'IE:
                                '   Memspect /c 1234 c:\OfflineSnapshot
                                '   or
                                '   Memspect /c c:\OfflineSnapshot 1234

                                args(2) = CStr(ShowProcessChooser("Choose a process that was launched under Memspect"))

                                Dim dlg = New System.Windows.Forms.FolderBrowserDialog
                                dlg.SelectedPath = IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly.Location)
                                If dlg.ShowDialog() <> Forms.DialogResult.OK Then
                                    Return GetHelpText()
                                End If
                                args(3) = dlg.SelectedPath
                                Return String.Empty
                            Case "?"c
                                Return GetHelpText()
                        End Select
                    Else
                        If String.IsNullOrEmpty(args(1)) Then
                            args(1) = ProcessLauncher.GetDevEnvFullPath
                        Else
                            'args(1) = ProcessLauncher.GetDevEnvFullPath
                            'if they passed an invalid exe, we could let them choose the correct one.
                            'Else
                            '    Dim dlg = New System.Windows.Forms.OpenFileDialog
                            '    dlg.DefaultExt = "exe"
                            '    If dlg.ShowDialog <> Forms.DialogResult.OK OrElse Not File.Exists(dlg.FileName) Then
                            '        Return GetHelpText()
                            '    End If
                            '    args(1) = dlg.FileName
                        End If
                        Return String.Empty

                    End If
                End If 'args(1).Length > 1 
                Dim tryint = 0
                If Not Integer.TryParse(args(1), tryint) Then
                    Return GetHelpText()
                End If

            Else
                args(1) = CStr(ShowProcessChooser("Choose a process that was launched under Memspect"))
            End If

            Return String.Empty
        End Function

        Friend Sub GetArgumentsFromLauncherUI(ByRef args() As String)

            Dim LauncherWindow = New LauncherUi
            If LauncherWindow.ShowDialog = True Then
                Dim fSpecial = False
                If Keyboard.IsKeyDown(Key.LeftCtrl) Then
                    fSpecial = True
                End If
                If LauncherWindow.rbtnDump.IsChecked Or LauncherWindow.rbtnExist.IsChecked Then ' attach to existing or do a minidump of a process
                    Dim f32bitOnly = LauncherWindow.rbtnExist.IsChecked.GetValueOrDefault
                    Dim targpid = ShowProcessChooser("Choose a process for which to create a minidump and show VirtualMemory", f32bitOnly)
                    If targpid = 0 Then
                        ReDim Preserve args(2)
                        args(1) = "/?" ' show help
                    Else
                        ReDim Preserve args(3)
                        If LauncherWindow.rbtnDump.IsChecked Then
                            args(1) = "/d"
                        Else
                            args(1) = "/x"
                        End If
                        args(2) = CStr(targpid)
                    End If

                    Return
                End If

                If LauncherWindow.rbtnInject.IsChecked Then
                    Dim targpid = ShowProcessChooser("Choose a 32 bit process for which to Inject " + MemSpectDllName)
                    If targpid = 0 Then
                        End ' end program when no proc chosen
                    Else

                        Dim hProc = Process.GetProcessById(targpid)
                        Dim wrkdir = IO.Path.GetDirectoryName(Reflection.Assembly.GetExecutingAssembly.Location)

                        Dim msppath = IO.Path.Combine(wrkdir, MemSpectDllName)

                        Dim res = ProcessLauncher.InjectIntoProcess(hProc, msppath, fDebug:=False)
                        If Not String.IsNullOrEmpty(res) Then
                            MsgBox(res)
                            End ' end program! 
                        End If
                        System.Threading.Thread.Sleep(5000)
                        ReDim Preserve args(2)
                        args(1) = targpid.ToString ' tell MemSpect.exe to attach to pid
                        Return
                    End If
                End If

                If LauncherWindow.rbtnLaunch.IsChecked Then
                    Dim targfullpath = LauncherWindow.cboFile.Text
                    Dim pl = New ProcessLauncher(WriteIniFile:=ProcessLauncher.WriteIniFile.WriteAnything) With {
                        .TrackClrObjects = If(LauncherWindow.chkTrackClrObjects.IsChecked.GetValueOrDefault, 1, 0),
                        .StartChildProcess = 1,
                        ._nmsecsToWaitTilStart = 3000,
                        .fMessageBoxOnStart = If(fSpecial, 1, 0)
                    }
                    If LauncherWindow.IsImmersive Then
                        Dim mspectDllPathInOS = String.Empty
                        If Environment.Is64BitOperatingSystem Then
                            mspectDllPathInOS = Path.Combine(Environment.GetEnvironmentVariable("SystemRoot"), "SysWow64") '  "c:\windows"
                        Else
                            mspectDllPathInOS = Path.Combine(Environment.GetEnvironmentVariable("SystemRoot"), "System32")
                        End If
                        mspectDllPathInOS = Path.Combine(mspectDllPathInOS, "MemSpect", MemSpectDllName)
                        If Not File.Exists(mspectDllPathInOS) Then
                            MsgBox("Please copy file as admin: File not found " + mspectDllPathInOS)
                            End
                        End If
                        Dim wrkdir = IO.Path.GetDirectoryName(Reflection.Assembly.GetExecutingAssembly.Location)

                        Dim msppath = IO.Path.Combine(wrkdir, MemSpectDllName)
                        If (New FileInfo(msppath).Length <> (New FileInfo(mspectDllPathInOS).Length)) Then
                            MsgBox("Version wrong: " + mspectDllPathInOS + " " + msppath)
                            End
                        End If


                        _ImmersivePackageInfo = New ImmersivePackageInfoClass(targfullpath)
                        If String.IsNullOrEmpty(_ImmersivePackageInfo.strPackageFullName) Then
                            MsgBox("Could not get ImmersivePackageInfo from " + targfullpath + vbCrLf +
                                   GetErrorMessageFromHResult(_ImmersivePackageInfo._hr)
                                   )
                            End ' end program!
                        End If
                        pl.IsImmersiveApp = True
                        pl.StartChildProcess = 0 ' immersive can't show ui

                        pl.LaunchTargProc(targfullpath, fWithDll:=False)
                        Dim procid = pl.ImmersiveProcessId
                        If procid = 0 Then
                            MsgBox(targfullpath + vbCrLf + "could not launch", MsgBoxStyle.Exclamation)
                            End ' end program!
                        End If

                        ReDim Preserve args(2)
                        args(1) = procid.ToString ' tell MemSpect.exe to attach to pid
                        Return

                    Else

                        If LauncherWindow.chkDisablePerfWatson.IsEnabled AndAlso LauncherWindow.chkDisablePerfWatson.IsChecked Then
                            'HKEY_CURRENT_USER\Software\Microsoft\VisualStudio\11.0\General /v MaximumResponsivenessDelay /t REG_DWORD /d 3600000000
                            Dim regkey = LauncherWindow.GetPerfWatsonkey(targfullpath)
                            If String.IsNullOrEmpty(regkey) Then
                                Throw New Exception(
                                    String.Format("Registry not found to modify PerfWatson registry key, Please start/close {0} first",
                                                  Path.GetFileName(targfullpath)))
                            End If
                            Microsoft.Win32.Registry.SetValue(
                                regkey,
                                "MaximumResponsivenessDelay",
                                1600000000,
                                Microsoft.Win32.RegistryValueKind.DWord
                                )
                            If True OrElse regkey.Contains("12.0") Then ' any version
                                'reg add HKEY_CURRENT_USER\Software\Microsoft\VisualStudio\12.0 /v EnableResponsiveness /t REG_DWORD /d 0
                                Dim respKey = regkey.Replace("\General", "")
                                Microsoft.Win32.Registry.SetValue(
                                respKey,
                                "EnableResponsiveness",
                                0,
                                Microsoft.Win32.RegistryValueKind.DWord
                                )
                            End If
                        End If
                    End If

                    If String.IsNullOrEmpty(targfullpath) Then
                        targfullpath = ProcessLauncher.GetDevEnvFullPath
                    End If

                    If Path.GetFileNameWithoutExtension(targfullpath).ToLowerInvariant = "devenv" AndAlso Not String.IsNullOrEmpty(ProcessLauncher._VsVersionstring) Then
                        Try
                            '
                            ' Reg add "HKLM\SOFTWARE\Wow6432Node\Microsoft\VisualStudio\14.0\Performance" /v DisablePerfWatson  /t REG_DWORD /d 1 /f
                            ' Reg add "HKLM\SOFTWARE\Wow6432Node\Microsoft\VisualStudio\14.0\Performance" /t REG_SZ /d "Microsoft.Internal.Performance.CodeMarkers.dll" /f
                            Dim codemerkerdllname = "Microsoft.Internal.Performance.CodeMarkers.dll"
                            Dim keyname = String.Format(
                                              "SOFTWARE\Microsoft\VisualStudio\{0}\Performance",
                                              ProcessLauncher._VsVersionstring)
                            Dim alreadyreg = False
                            Using key = Microsoft.Win32.Registry.LocalMachine.OpenSubKey(keyname)
                                If key IsNot Nothing Then
                                    Dim val = key.GetValue(String.Empty)
                                    If val IsNot Nothing And TypeOf (val) Is String Then
                                        If Not String.IsNullOrEmpty(CStr(val)) Then
                                            If (CStr(val).ToLower = codemerkerdllname.ToLower) Then
                                                alreadyreg = True
                                            End If
                                        End If
                                    End If
                                End If
                            End Using
                            If Not alreadyreg Then
                                If Not IsRunningAsAdmin() Then
                                    If MsgBox("You're not running as admin: codemarkers aren't and can't be registered. Proceed without registration?", MsgBoxStyle.YesNo) = MsgBoxResult.No Then
                                        End
                                    End If
                                Else
                                    Using key = Microsoft.Win32.Registry.LocalMachine.CreateSubKey(keyname)
                                        key.SetValue(String.Empty, "Microsoft.Internal.Performance.CodeMarkers.dll", Microsoft.Win32.RegistryValueKind.String)
                                    End Using
                                End If
                            End If
                        Catch ex As Exception

                        End Try
                    End If

                    If Not String.IsNullOrEmpty(LauncherWindow.cboArgs.Text) Then
                        pl._AdditionalCmdLineParams = LauncherWindow.cboArgs.Text
                    End If

                    If targfullpath.ToLower.EndsWith("exe") Then
                        pl.LaunchTargProc(targfullpath, fWithDll:=True, fDoInitcomm:=False)
                        End ' end program!
                    End If


                    ReDim Preserve args(2)
                    If IO.Path.GetFileName(targfullpath).ToLower = DumpedFilesMHDFileName.ToLower Then
                        args(1) = "/o"
                        args(2) = IO.Path.GetDirectoryName(targfullpath)
                    Else
                        args(1) = "/m"
                        args(2) = targfullpath
                    End If
                Else
                    ReDim Preserve args(1)
                    args(1) = "/p"
                End If
            Else
                End
            End If
        End Sub

        Friend Function ShowProcessChooser(ByVal strTitle As String, Optional ByVal f32bitOnly As Boolean = True) As Integer
            Dim selectedPid As Integer = 0
            Dim qProcs = From proc In System.Diagnostics.Process.GetProcesses _
                      Select proc.Id,
                             proc.ProcessName,
                             proc.MainWindowTitle,
                             proc.WorkingSet64,
                             proc.PrivateMemorySize64,
                             Is64 = ProcessType(proc),
                             _proc = proc
                      Order By ProcessName


            Dim w = New MyWindow(strTitle, WinType:="ProcessChooser", fCloseOnUnFreeze:=False)
            Dim b = New Browse(qProcs)
            w.Content = b
            b._BrowseList.ContextMenu = Nothing ' else invokes default item

            AddHandler b._BrowseList.MouseDoubleClick, Sub(o As Object, e As MouseButtonEventArgs)
                                                           Dim lv = TryCast(o, ListView)
                                                           If lv IsNot Nothing Then
                                                               If e IsNot Nothing Then
                                                                   Dim pt = e.GetPosition(lv)
                                                                   Dim elem = lv.InputHitTest(pt)
                                                                   If elem IsNot Nothing Then
                                                                       elem = GetAncestor(Of ListViewItem)(CType(elem, DependencyObject))
                                                                       If elem IsNot Nothing Then
                                                                           w.Close()
                                                                       End If
                                                                   End If
                                                               End If
                                                           End If
                                                       End Sub
            AddHandler b._BrowseList.KeyUp, Sub(o As Object, e As KeyEventArgs)
                                                If e.Key = Key.Return Then
                                                    w.Close()
                                                End If
                                            End Sub
            AddHandler w.Closed,
                Sub(o As Object, e As EventArgs)
                    Dim dt = b._BrowseList.SelectedItem
                    If dt IsNot Nothing Then
                        Dim tdesc = ComponentModel.TypeDescriptor.GetProperties(dt)("_proc")
                        Dim hProc = CType(tdesc.GetValue(dt), Process)
                        Dim is64 = CStr(TypeDescriptor.GetProperties(dt)("Is64").GetValue(dt))
                        If is64 = "32" OrElse Not f32bitOnly Then
                            selectedPid = hProc.Id
                            w.Close()
                        Else
                            If is64 = "Access is denied" Then
                                Dim strmsg = String.Format("Access denied: do you want to try anyway? (Running As Admin = {0}", IsRunningAsAdmin)
                                If MessageBox.Show(strmsg,
                                                   strTitle,
                                                   MessageBoxButton.OKCancel,
                                                   MessageBoxImage.Question) = MessageBoxResult.OK Then
                                    selectedPid = hProc.Id
                                    w.Close()
                                End If
                            Else
                                MessageBox.Show("Please choose a 32 bit process", strTitle, MessageBoxButton.OK, MessageBoxImage.Information)
                            End If
                        End If
                    End If
                End Sub
            w.ShowDialog()
            Return selectedPid
        End Function

        Private Sub CommonUIStatusMsgEventHandler(ByVal sender As Object, ByVal e As StatusMessageEventArgs)
            Dim msg = String.Format("{0:T} {1}", e.MsgTimeStamp, e.Message) 'like "11:52:37 PM"

            If System.Diagnostics.Debugger.IsAttached Then
                Trace.WriteLine(msg)
            End If
            If Not _IsUnderTest Then
                If e.MsgType <> StatusMessageType.StatusBarEntry Then
                    _txtStatus.Dispatcher.BeginInvoke(Sub(p1 As String)
                                                          _txtStatus.AppendText(p1 + vbCrLf)
                                                          _txtStatus.CaretIndex = _txtStatus.Text.Length
                                                          _txtStatus.ScrollToEnd()
                                                      End Sub, {msg})
                End If
            End If
            If Not _IsUnderTest Then ' 

                If _txtStatBar.Dispatcher.CheckAccess Then
                    _txtStatBar.Text = msg
                Else

                    _txtStatBar.Dispatcher.BeginInvoke(DispatcherPriority.Render,
                                                       Sub(oTextBlock As TextBlock)
                                                           oTextBlock.Text = msg
                                                           'Dispatcher.CurrentDispatcher.Invoke(DispatcherPriority.Render, Function() Nothing)
                                                       End Sub, _txtStatBar)
                End If
                If e.MsgPriority = StatusMessagePriority.Normal Then
                    Try
                        If _txtStatBar.Dispatcher.CheckAccess Then
                            ' now we want to invoke a nothing func with low pri on the UI thread so the statbar updates immediately
                            _txtStatBar.Dispatcher.Invoke(DispatcherPriority.Render, Function() Nothing)
                            '                                                   System.Threading.Thread.Sleep(1000)

                        End If
                    Catch ex As Exception


                    End Try
                End If
            End If
        End Sub



    End Module




End Namespace