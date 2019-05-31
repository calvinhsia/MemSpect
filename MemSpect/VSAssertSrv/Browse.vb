Imports System.Windows.Controls
Imports System.Windows.Threading
Imports System.Windows.Data
Imports System.Windows.Documents
Imports System.Windows.Input
Imports System.Windows
Imports System.IO
Imports System.Windows.Media
Imports System.Collections.ObjectModel
Imports System.Runtime.CompilerServices
Imports System.Linq
Imports System.Xml.Linq
Imports System.Runtime.InteropServices

Imports System.Windows.Data.Binding
Imports System.ComponentModel


Namespace MemSpect

    ' see http://blogs.msdn.com/calvin_hsia/archive/2007/12/06/6684376.aspx

    Public Class BrowseInitialSortOrder
        ''' <summary>
        ''' 1 based col no
        ''' </summary>
        ''' <remarks></remarks>
        Public ColumnNo As Integer ' 1 based
        Public direction As ComponentModel.ListSortDirection
    End Class

    Public Class Browse
        Inherits DockPanel
        Public _dpInner As DockPanel ' panel above BrowseList
        Public _lfPanel As DockPanel = Nothing
        Public WithEvents _BrowseList As BrowseList
        Sub New(
               ByVal Query As IEnumerable,
               Optional ByVal fAllowHeaderClickSort As Boolean = True,
               Optional ByVal ColWidths() As Integer = Nothing,
               Optional ByVal fAllowBrowFilter As Boolean = False,
               Optional ByVal InitialSortOrder As BrowseInitialSortOrder = Nothing,
               Optional ByVal ColTips() As Object = Nothing,
               Optional ByVal arrColumnsToTotal() As String = Nothing
            )
            _dpInner = New DockPanel
            Me.Children.Add(_dpInner)
            _BrowseList = New BrowseList(Query,
                                         fAllowHeaderClickSort,
                                         ColWidths,
                                         InitialSortOrder,
                                         ColTips)
            If fAllowBrowFilter OrElse arrColumnsToTotal IsNot Nothing Then
                _lfPanel = New ListFilter(_BrowseList)
                _dpInner.Children.Add(_lfPanel)
                DockPanel.SetDock(_lfPanel, Dock.Top)
            End If

            _dpInner.Children.Add(_BrowseList)

            SetDock(_dpInner, Dock.Top)
            If fAllowBrowFilter OrElse arrColumnsToTotal IsNot Nothing Then

                If arrColumnsToTotal Is Nothing Then
                    arrColumnsToTotal = {""} ' at least show how many items are selected
                End If
                Dim BrowseTotals = New TextBox With {
                        .Text = String.Empty,
                        .ToolTip = "Selected items and totals show here",
                        .Background = Brushes.Turquoise,
                        .IsReadOnly = True
                    }
                Dim bord = New Border With {.Height = 25}
                bord.Child = BrowseTotals
                _lfPanel.Children.Insert(0, bord)

                AddHandler _BrowseList.SelectionChanged,
                    Sub()
                        Try
                            Dim colNdx = 0
                            If _BrowseList.SelectedItems.Count > 1 Then
                                Dim totArray(arrColumnsToTotal.Length - 1) As Long
                                For Each itm In _BrowseList.SelectedItems
                                    Dim tdescProps = TypeDescriptor.GetProperties(itm)
                                    colNdx = 0
                                    For Each colname In arrColumnsToTotal
                                        If Not String.IsNullOrEmpty(colname) Then
                                            If colname.EndsWith(HexColumnSpecifier) Then
                                                Dim val = CStr(tdescProps(colname.Replace(HexColumnSpecifier, "")).GetValue(itm))
                                                Dim valh = Long.Parse(val, Globalization.NumberStyles.AllowHexSpecifier)
                                                totArray(colNdx) += CLng(valh)
                                            Else
                                                Dim val = tdescProps(colname).GetValue(itm)
                                                totArray(colNdx) += CLng(val)
                                            End If
                                            colNdx += 1
                                        End If
                                    Next
                                Next
                                Dim totTxt = ""
                                colNdx = 0
                                For Each colname In arrColumnsToTotal
                                    If Not String.IsNullOrEmpty(colname) Then
                                        If colname.EndsWith(HexColumnSpecifier) Then
                                            totTxt += String.Format("   {0} = {1:x8}", colname.Replace(HexColumnSpecifier, ""), totArray(colNdx))
                                        Else

                                            totTxt += String.Format("   {0} = {1:n0}", colname, totArray(colNdx))
                                        End If
                                        colNdx += 1
                                    End If
                                Next
                                BrowseTotals.Text = String.Format("# Sel Items = {0} {1}  ", _BrowseList.SelectedItems.Count, totTxt)
                            Else
                                BrowseTotals.Text = String.Empty
                            End If

                        Catch ex As Exception
                            CommonUI.MemSpectExceptionHandler(ex, "Input = " + String.Join(",", arrColumnsToTotal))
                        End Try

                    End Sub
            End If

        End Sub

        Public Sub SetDynamicBackground(ByVal converter As IValueConverter, ByVal fAdditive As Boolean)
            Dim style As Windows.Style
            If fAdditive Then
                style = New Windows.Style(GetType(ListViewItem), _BrowseList.ItemContainerStyle)
            Else
                style = New Windows.Style(GetType(ListViewItem))
            End If

            Dim bgSetter = New Setter(ListViewItem.BackgroundProperty, Brushes.White)
            Dim bnd = New Binding With {.Converter = converter,
                                        .Mode = BindingMode.OneTime,
                                        .RelativeSource = New RelativeSource(RelativeSourceMode.Self)
                                       }

            bgSetter.Value = bnd
            style.Setters.Add(bgSetter)

            Dim mrgSetter = New Setter(ListViewItem.MarginProperty, New Thickness(0))
            style.Setters.Add(mrgSetter)

            style.Setters.Add(New Setter(ListViewItem.BorderThicknessProperty, New Thickness(0)))

            Me._BrowseList.ItemContainerStyle = style

        End Sub

        Friend Shared g_LastStringFilterContent As String = String.Empty
        Friend Class ListFilter
            Inherits DockPanel
            Private _browList As BrowseList
            Private WithEvents _txtFilter As New TextBox With {.Width = 200, .Text = g_LastStringFilterContent, .ToolTip = "this defaults to the same value from any other instance for convenience"}
            Private WithEvents _btnApply As New Button With {.Content = "Apply", .ToolTip = "Apply the text filter to all string fields (including hex values). Case insensitive "}
            Private _txtStatus As New TextBlock
            Sub New(ByVal browList As BrowseList)
                _browList = browList
                Dim spFilt As New StackPanel With {.Orientation = Orientation.Horizontal, .HorizontalAlignment = Windows.HorizontalAlignment.Right}
                spFilt.Children.Add(_txtStatus)
                spFilt.Children.Add(New Label With {
                                    .Content = "StringFilter",
                                    .ToolTip = " Case insenSitive search of character fields (not stacks) containing text. A filter works on current (possibly filtered) set" + vbCrLf +
                                            "If it starts with '!', then it's negated"
                                    }
                                )
                spFilt.Children.Add(_txtFilter)
                spFilt.Children.Add(_btnApply)
                Me.Children.Add(spFilt)
                RefreshFilterStat()
            End Sub
            Sub RefreshFilterStat()
                _txtStatus.Text = String.Format("# items = {0:n0}  ", _browList.Items.Count)
            End Sub
            Sub on_txtFilterKey(ByVal sender As Object, ByVal e As KeyEventArgs) Handles _txtFilter.KeyUp
                If e.Key = Key.Enter Then
                    on_btnApply_click()
                End If
            End Sub

            Sub on_btnApply_click() Handles _btnApply.Click
                _browList.Cursor = Cursors.Wait
                Dim filttext = _txtFilter.Text.Trim.ToLower
                Dim fIsPositive = True
                If filttext.StartsWith("!") Then
                    filttext = filttext.Substring(1)
                    fIsPositive = False
                End If
                If String.IsNullOrEmpty(filttext) Then
                    _browList.Items.Filter = Nothing
                Else
                    g_LastStringFilterContent = filttext
                    _browList.Items.Filter = Function(itm As Object) As Boolean
                                                 Dim fIsMatch = False
                                                 Dim r = ComponentModel.TypeDescriptor.GetProperties(itm)

                                                 ' loop through all properties of this item
                                                 For Each prop As ComponentModel.PropertyDescriptor In r
                                                     Dim ww = TryCast(prop.GetValue(itm), String)
                                                     If ww IsNot Nothing Then ' for string types
                                                         If ww.ToLower.Contains(filttext) Then
                                                             fIsMatch = True
                                                             Exit For
                                                         End If
                                                     End If
                                                 Next
                                                 If Not fIsPositive Then
                                                     fIsMatch = Not fIsMatch
                                                 End If
                                                 Return fIsMatch
                                             End Function

                End If
                RefreshFilterStat()
                _browList.Cursor = Cursors.Arrow
            End Sub
        End Class

        Public Class BrowseList
            Inherits ListView

            Friend _query As IEnumerable
            Friend _InitialBListSortOrder As BrowseInitialSortOrder

            Sub New(
                   ByVal Query As IEnumerable,
                   Optional ByVal fAllowHeaderClickSort As Boolean = True,
                   Optional ByVal ColWidths() As Integer = Nothing,
                   Optional ByVal InitialSortOrder As BrowseInitialSortOrder = Nothing,
                   Optional ByVal ColTips() As Object = Nothing
                                                     )
                Try
                    _query = Query
                    If InitialSortOrder Is Nothing Then
                        _InitialBListSortOrder = New BrowseInitialSortOrder With {.ColumnNo = -1, .direction = ComponentModel.ListSortDirection.Ascending}
                    Else
                        _InitialBListSortOrder = InitialSortOrder
                    End If
                    Debug.Assert(VirtualizingStackPanel.GetIsVirtualizing(Me), "ListView is already virtualizing")
                    '                    VirtualizingStackPanel.SetVirtualizationMode(Me, VirtualizationMode.Recycling)
                    Debug.Assert(VirtualizingStackPanel.GetVirtualizationMode(Me) = VirtualizationMode.Standard, "not VirtualizationMode.Standard")

                    Dim nColIndex = 0
                    Dim gv As New GridView
                    Me.ContextMenu = New ContextMenu

                    Me.ContextMenu.AddMnuItem("Copy",
                                              "Copy all or selected items to clipboard",
                                              AddressOf on_CtxMenuBrow)

                    Me.ContextMenu.AddMnuItem("Export to E_xcel",
                                              "create a temp file of all or selected items in CSV format that Excel can read",
                                              AddressOf on_CtxMenuBrow)

                    Me.ContextMenu.AddMnuItem("Export to _Notepad",
                                              "create a temp file of all or selected items in TXT format",
                                              AddressOf on_CtxMenuBrow
                                  )

                    Me.View = gv
                    Me.ItemsSource = Query
                    If fAllowHeaderClickSort Then
                        Me.AddHandler(GridViewColumnHeader.ClickEvent, New RoutedEventHandler(AddressOf HandleHeaderClick))
                    End If
                    If Query.GetType.GetInterface(GetType(IEnumerable(Of )).FullName).GetGenericArguments(0).Name = "XElement" Then ' It's XML
                        Dim Elem1 = CType(Query, IEnumerable(Of XElement))(0).Elements ' Thanks Avner!
                        For Each Item In Elem1
                            Dim gvc As New GridViewColumn
                            Dim hdr = New GridViewColumnHeader With {.Content = Item.Name.LocalName}
                            gvc.Header = hdr
                            If ColTips IsNot Nothing AndAlso nColIndex <= ColTips.Length Then
                                hdr.ToolTip = ColTips(nColIndex)
                            End If
                            gv.Columns.Add(gvc)
                            Dim bind As New Binding("Element[" + Item.Name.LocalName + "].Value")
                            gvc.DisplayMemberBinding = bind
                            If ColWidths IsNot Nothing AndAlso nColIndex <= ColWidths.Length Then
                                gvc.Width = ColWidths(nColIndex)
                            Else
                                gvc.Width = 180
                            End If
                            nColIndex += 1
                        Next
                    Else ' it's some anonymous type like "VB$AnonymousType_1`3". Let's use reflection to get the column names
                        Dim colWidthMult = VBDiagMarginBase._FontSize / 10
                        For Each mem In From mbr In _
                                Query.GetType().GetInterface(GetType(IEnumerable(Of )).FullName) _
                                .GetGenericArguments()(0).GetMembers _
                                Where mbr.MemberType = Reflection.MemberTypes.Property

                            If mem.Name.StartsWith("_") Then
                                Continue For
                            End If

                            Dim datatype = CType(mem, Reflection.PropertyInfo)
                            Dim coltype = datatype.PropertyType.Name
                            Select Case coltype
                                Case "Int32", "String", "Int64", "UInteger", "UInt32", "Double", "Boolean", "UInt64"
                                    Dim gvc As New GridViewColumn
                                    Dim hdr = New GridViewColumnHeader With {.Content = mem.Name, .Tag = mem.Name}
                                    If ColTips IsNot Nothing AndAlso nColIndex < ColTips.Length Then
                                        hdr.ToolTip = ColTips(nColIndex)
                                    Else
                                        hdr.ToolTip = IIf(fAllowHeaderClickSort, "Click on header to sort", String.Empty)
                                    End If
                                    gvc.Header = hdr
                                    gv.Columns.Add(gvc)
                                    If gv.Columns.Count = _InitialBListSortOrder.ColumnNo Then
                                        _LastHeaderClicked = CType(gvc.Header, GridViewColumnHeader)
                                        _LastSortDir = _InitialBListSortOrder.direction
                                        SetHeaderTemplate(gvc, _LastSortDir)
                                    End If
                                    'Text=<%= "{Binding Path=" + mem.Name + "}" %>

                                    Dim tbTextString As String
                                    'alas http://social.msdn.microsoft.com/Forums/en-US/wpf/thread/e99e37ca-ee4d-429f-afbd-29a6b874d270/
                                    If coltype <> "String" AndAlso coltype <> "Boolean" Then ' want numeric converter
                                        tbTextString = "{Binding Path=" + mem.Name + ",Converter={""l:NumericalConverter""}}"
                                    Else
                                        tbTextString = "{Binding Path=" + mem.Name + "}"
                                    End If
                                    Dim XAMLdt = _
                                    <DataTemplate
                                        xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
                                        xmlns:l="clr-namespace:MemSpect;assembly=MemSpect"
                                        >
                                        <TextBlock
                                            Name=<%= mem.Name %>
                                            Text=<%= tbTextString %>
                                            >
                                        </TextBlock>
                                    </DataTemplate>
                                    'FontFamily = <%= VBDiagMarginBase._FontName %>
                                    'FontSize = <%= VBDiagMarginBase._FontSize.ToString + "pt" %>

                                    Dim dt = CType(System.Windows.Markup.XamlReader.Load(XAMLdt.CreateReader), DataTemplate)
                                    gvc.CellTemplate = dt

                                    If ColWidths IsNot Nothing AndAlso nColIndex < ColWidths.Length Then
                                        gvc.Width = ColWidths(nColIndex) * colWidthMult
                                    Else
                                        If coltype <> "String" Then
                                            If coltype = "Int64" Then
                                                gvc.Width = 100 * colWidthMult
                                            Else
                                                gvc.Width = 80 * colWidthMult
                                            End If
                                            'gvc.Width=Double.NaN ' auto
                                        Else
                                            '                                gvc.DisplayMemberBinding = New Binding(mem.Name)
                                            gvc.Width = 180 * colWidthMult
                                        End If
                                    End If
                                    nColIndex += 1

                            End Select
                        Next
                    End If

                    Dim XAMLlbStyle = _
            <Style
                xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
                xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
                TargetType="ListViewItem">
                <Setter Property="Margin" Value="0"/>
                <Setter Property="FontFamily" Value=<%= VBDiagMarginBase._FontName %>/>
                <Setter Property="FontSize" Value=<%= VBDiagMarginBase._FontSize.ToString + "pt" %>/>
                <Setter Property="Foreground" Value=<%= VBDiagMarginBase._Foreground %>/>
                <Setter Property="Background" Value=<%= VBDiagMarginBase._Background %>/>
                <!--

                    <Style.Triggers>
                    <Trigger Property="IsSelected" Value="True">
                        <Setter Property="Foreground" Value="DarkRed"/>
                        <Setter Property="Background" Value="LightGray"/>
                    </Trigger>
                    <Trigger Property="IsMouseOver" Value="True">
                        <Setter Property="Foreground" Value="DarkRed"/>
                    </Trigger>
                </Style.Triggers>
-->
            </Style>
                    Dim styyle = CType(Windows.Markup.XamlReader.Load(XAMLlbStyle.CreateReader), Windows.Style)
                    'If SystemParameters.HighContrast Then
                    '    styyle.Setters.RemoveAt(styyle.Setters.Count - 1)
                    '    styyle.Setters.RemoveAt(styyle.Setters.Count - 1)
                    '    'styyle.Setters.Add(New Setter(ListViewItem.BackgroundProperty, VBDiagMarginBase._Background))
                    '    'styyle.Setters.Add(New Setter(ListViewItem.ForegroundProperty, VBDiagMarginBase._Foreground))
                    'End If
                    Me.ItemContainerStyle = styyle
                Catch ex As Exception
                    CommonUI.MemSpectExceptionHandler(ex)
                End Try
            End Sub

            Friend Class MyValueConverter
                Implements IValueConverter

                Private Const MaxWidth As Integer = 170
                Public Function Convert(ByVal value As Object, ByVal targetType As System.Type, ByVal parameter As Object, ByVal culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.Convert
                    If Not value Is Nothing Then
                        Dim type As Type = value.GetType()

                        If type Is GetType(String) Then
                            Dim str = value.ToString().Trim()
                            Dim index As Integer = str.IndexOfAny(New Char() {CChar("\r"), CChar("\n")})
                            Dim lengthLimit = MyValueConverter.MaxWidth

                            If index >= 0 Then
                                lengthLimit = index - 1
                            End If
                            If index >= 0 Or (str.Length > lengthLimit) Then
                                value = str.Substring(0, lengthLimit)
                            Else
                                value = str
                            End If
                        ElseIf type Is GetType(Int32) Then
                            value = CInt(value).ToString("n0")
                        ElseIf type Is GetType(Int64) Then
                            value = CType(value, Int64).ToString("n0")
                        ElseIf type Is GetType(UInt64) Then
                            value = CType(value, UInt64).ToString("n0")
                        Else
                            value = value.ToString()
                        End If
                    End If
                    Return value
                End Function

                Public Function ConvertBack(ByVal value As Object, ByVal targetType As System.Type, ByVal parameter As Object, ByVal culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.ConvertBack
                    Throw New NotImplementedException()
                End Function
            End Class

            Public Function DumpListToString(ByVal fCSV As Boolean) As Text.StringBuilder
                Dim sb = New Text.StringBuilder
                Dim gridview = CType(Me.View, GridView)
                Dim IsFirst = True

                'Dim defview = Me.View
                Dim defview = CollectionViewSource.GetDefaultView(Me.ItemsSource)
                Dim colNames = New List(Of String)
                Dim priorPadding = 0
                Dim widths(gridview.Columns.Count - 1) As Integer
                Dim colndx = 0
                For Each col In gridview.Columns
                    If IsFirst Then
                        IsFirst = False
                    Else
                        If fCSV Then
                            sb.Append(",")
                        Else
                            For i = 1 To priorPadding
                                sb.Append(" ")
                            Next
                        End If
                    End If
                    Dim txt = CType(col.Header, GridViewColumnHeader).Content.ToString
                    colNames.Add(txt)
                    widths(colndx) = CInt(col.ActualWidth / 6) ' record for later use
                    priorPadding = widths(colndx) - txt.Length
                    sb.Append(txt)
                    colndx += 1
                Next
                sb.AppendLine()
                Dim srcColl = defview.SourceCollection
                If Me.SelectedItems.Count > 0 Then ' if only 1 (or more) item is sel, then do only 1 item to notepad, excel
                    If Me.SelectedItems.Count = Me.Items.Count Then
                        srcColl = Me.Items 'if all are selected, output in item order
                    Else
                        srcColl = Me.SelectedItems
                    End If
                Else
                    srcColl = Me.Items
                End If
                For Each row In srcColl
                    IsFirst = True
                    priorPadding = 0
                    colndx = 0
                    For Each colName In colNames
                        If IsFirst Then
                            IsFirst = False
                        Else
                            If fCSV Then
                                sb.Append(",")
                            Else
                                For i = 1 To priorPadding
                                    sb.Append(" ")
                                Next
                            End If
                        End If
                        Dim typeDescProp = ComponentModel.TypeDescriptor.GetProperties(row)(colName)
                        If typeDescProp IsNot Nothing Then
                            Dim val = typeDescProp.GetValue(row)
                            Dim strval = String.Empty
                            If val IsNot Nothing Then
                                If typeDescProp.PropertyType.Name = "List`1" Then
                                    Dim tryenum = TryCast(val, IEnumerable)
                                    If tryenum IsNot Nothing Then
                                        Dim strList = String.Empty
                                        Dim fFirst = True
                                        For Each itm In tryenum
                                            If fFirst Then
                                                fFirst = False
                                            Else
                                                strList += "|"
                                            End If
                                            strList += itm.ToString
                                        Next
                                        'Dim q = (From a In tryenum).ToList
                                        strval = strList.ToString.PadRight(widths(colndx))
                                    End If
                                End If
                                If String.IsNullOrEmpty(strval) Then
                                    strval = val.ToString.Replace(vbCr, " ").Replace(vbLf, " ").Replace("""", " ")
                                End If
                                If fCSV Then
                                    Dim fIsString = val.GetType.ToString = GetType(String).ToString
                                    'Dim intval = 0
                                    'If fIsString AndAlso strval.Length > 0 AndAlso Char.IsDigit(strval(0)) AndAlso Not Integer.TryParse(strval, intval) Then
                                    '    strval = "0x" + strval ' so Excel doesn't interpret as sci notation 02e214  = 2 X 10^214. But also don't want 0x before every integer (sigh)
                                    'End If
                                    If strval.Contains(",") Then 'OrElse fIsString Then 
                                        strval = """" + strval + """"
                                    End If
                                End If
                            End If
                            sb.Append(strval)
                            priorPadding = widths(colndx) - strval.Length

                        End If
                        colndx += 1
                    Next
                    sb.AppendLine()
                Next

                Return sb
            End Function

            Private _LastSortDir As System.ComponentModel.ListSortDirection = ComponentModel.ListSortDirection.Ascending
            Private _LastHeaderClicked As GridViewColumnHeader = Nothing
            Friend Sub HandleHeaderClick(ByVal sender As Object, ByVal e As RoutedEventArgs)
                If e.OriginalSource.GetType Is GetType(GridViewColumnHeader) Then
                    Dim gvh = CType(e.OriginalSource, GridViewColumnHeader)
                    Dim dir As System.ComponentModel.ListSortDirection = ComponentModel.ListSortDirection.Ascending
                    If Not gvh Is Nothing AndAlso Not gvh.Column Is Nothing Then
                        DataWindowMain.SetCursor(Cursors.AppStarting, uiObj:=Me)
                        If _LastHeaderClicked IsNot Nothing Then
                            _LastHeaderClicked.Column.HeaderTemplate = Nothing
                        End If
                        If gvh Is _LastHeaderClicked Then
                            If _LastSortDir = ComponentModel.ListSortDirection.Ascending Then
                                dir = ComponentModel.ListSortDirection.Descending
                            End If
                        End If
                        SetHeaderTemplate(gvh.Column, dir)
                        _LastHeaderClicked = gvh
                        _LastSortDir = dir

                        'Me.Items.Refresh()
                        Dim act = New MySortActivity
                        Dim inargs = New Dictionary(Of String, Object) From {{"brow", Me}}
                        Dim wfapp = New WorkflowApplication(act, inargs)
                        wfapp.SynchronizationContext = System.Threading.SynchronizationContext.Current
                        wfapp.Completed = Sub()
                                              DataWindowMain.SetCursor(Cursors.Arrow, fForce:=True, uiObj:=Me)
                                          End Sub
                        wfapp.Run()
                    End If
                End If
            End Sub

            Private Class MySortActivity
                Inherits MyActivity
                Property brow As InArgument(Of BrowseList)
                Protected Overrides Sub Execute(ByVal context As System.Activities.NativeActivityContext)
                    Dim bl As BrowseList = brow.Get(context)
                    bl.Items.SortDescriptions.Clear()
                    Dim gh = CType(bl._LastHeaderClicked.Column.Header, GridViewColumnHeader)
                    Dim sd = New System.ComponentModel.SortDescription(gh.Tag.ToString, bl._LastSortDir)
                    bl.Items.SortDescriptions.Add(sd)
                    bl.Items.Refresh()

                End Sub
            End Class

            Private Sub SetHeaderTemplate(ByVal gvc As GridViewColumn, ByVal dir As ComponentModel.ListSortDirection)
                gvc.HeaderTemplate = CType(Windows.Markup.XamlReader.Load(
    <DataTemplate
        xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
        xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
        >
        <DockPanel>
            <TextBlock HorizontalAlignment="Center"
                Text="{Binding}"/>
            <Path
                Fill="DarkGray"
                Data=<%= If(dir = ComponentModel.ListSortDirection.Ascending,
                         "M 4,4 L 12,4 L 8,2",
                         "M 4,2 L 8,4 L 12,2") %>/>
        </DockPanel>
    </DataTemplate>.CreateReader), DataTemplate)
            End Sub

            Sub on_Key(ByVal sender As Object, ByVal e As KeyEventArgs) Handles Me.PreviewKeyDown
                If e.Key = Key.Enter Then
                    If Me.SelectedItems IsNot Nothing Then
                        If Me.ContextMenu IsNot Nothing AndAlso Me.ContextMenu.Items IsNot Nothing Then
                            If Me.ContextMenu.Items.Count > 0 Then
                                e.Handled = True
                                Dim mnuitm = TryCast(Me.ContextMenu.Items(0), MenuItem)
                                If mnuitm IsNot Nothing Then
                                    Dim args As New RoutedEventArgs With {
                                        .RoutedEvent = MenuItem.ClickEvent,
                                        .Source = mnuitm
                                    }
                                    mnuitm.RaiseEvent(args)
                                End If
                            End If
                        End If
                    End If
                End If
            End Sub

            Private Sub on_CtxMenuBrow(ByVal sender As Object, ByVal e As RoutedEventArgs)
                Dim mitem = TryCast(e.OriginalSource, MenuItem)
                Dim vrb = mitem.Header.ToString
                Dim fCSV = vrb = "Export to E_xcel"

                Dim sb = DumpListToString(fCSV)
                Select Case vrb
                    Case "Export to _Notepad"
                        WriteOutputToTempFile(sb.ToString)
                    Case "Copy"
                        System.Windows.Clipboard.SetData(DataFormats.Text, sb.ToString)
                    Case "Export to E_xcel"
                        'Dim fname = Path.Combine(System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), "MemSpect") + If(fCSV, ".csv", ".txt")
                        'IO.File.WriteAllText(fname, sb.ToString)
                        'If MessageBox.Show("Data written to " + fname + ".  Start it via associated app? (Excel or Notepad)", "", MessageBoxButton.YesNo, MessageBoxImage.Question) = MessageBoxResult.Yes Then
                        '    Process.Start(fname)
                        'End If
                        WriteOutputToTempFile(sb.ToString, "csv")
                End Select
            End Sub

        End Class

    End Class

    <ValueConversion(GetType(Integer), GetType(String))>
    <ValueConversion(GetType(UInteger), GetType(String))>
    <ValueConversion(GetType(Int64), GetType(String))>
    <ValueConversion(GetType(Double), GetType(String))>
    Public Class NumericalConverter
        Implements System.Windows.Data.IValueConverter

        Public Function Convert(ByVal value As Object, ByVal targetType As System.Type, ByVal parameter As Object, ByVal culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.Convert
            Dim strRes = ""
            Select Case value.GetType
                Case GetType(Integer)
                    Dim val = CInt(value)
                    If val > 9999 Then
                        strRes = val.ToString("N0")
                    Else
                        strRes = val.ToString()
                    End If
                Case GetType(UInteger)
                    Dim val = CType(value, UInteger)
                    If val > 9999 Then
                        strRes = val.ToString("N0")
                    Else
                        strRes = val.ToString()
                    End If
                Case GetType(Int64) ' Long
                    Dim val = CType(value, Int64)
                    If val > 9999 Then
                        strRes = CType(value, Int64).ToString("N0")
                    Else
                        strRes = CType(value, Int64).ToString()
                    End If
                Case GetType(UInt64) ' Long
                    Dim val = CType(value, UInt64)
                    If val > 9999 Then
                        strRes = CType(value, UInt64).ToString("N0")
                    Else
                        strRes = CType(value, UInt64).ToString()
                    End If
                Case GetType(Double)
                    strRes = String.Format("{0,12:n3}", value)
                Case GetType(String)
                    Dim intTemp = 0
                    If Integer.TryParse(CStr(value), intTemp) Then
                        strRes = intTemp.ToString("n0")
                    Else
                        strRes = CStr(value)
                    End If
            End Select
            Return strRes
        End Function

        Public Function ConvertBack(ByVal value As Object, ByVal targetType As System.Type, ByVal parameter As Object, ByVal culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.ConvertBack
            Dim retval As Object = 0
            Dim tmpint = 0
            Select Case targetType
                Case GetType(Integer)
                    If Integer.TryParse(CStr(value).Trim,
                                        Globalization.NumberStyles.AllowThousands,
                                        Globalization.CultureInfo.CurrentCulture,
                                        tmpint) Then
                        retval = CInt(value.ToString)
                    End If
                Case GetType(UInteger)
                    retval = CUInt(value.ToString)
                Case GetType(Int64)
                    retval = CType(value, Int64)
                Case GetType(Double)
                    Throw New NotImplementedException
                Case GetType(String)
                    Dim tmp = CStr(value).Replace(",", "")
                    If Integer.TryParse(tmp, tmpint) Then
                        retval = tmpint
                    End If
            End Select
            Return retval
        End Function
    End Class

    Public Delegate Function BrowQueryDelegate(ByVal lst As List(Of HeapAllocationContainer),
                                              ByVal bmem As BrowseMem) As IEnumerable


    Public Class BrowseMem
        Inherits DockPanel
        Friend WithEvents _DetailBrowse As Browse ' null: deferred initialization
        Friend _qDetails As IEnumerable
        Friend _queryFunction As BrowQueryDelegate ' Func(Of List(Of HeapAllocationContainer), BrowseMem, IEnumerable)
        Public _ColWidths As Integer()
        Public _arrColumnsToTotal() As String
        Friend _TabControl As MyTabControl
        Public ReadOnly Property _TabItemStackAgg As TabItem ' stackaggview
            Get
                Return _TabControl._tabitems(0)
            End Get
        End Property

        Public ReadOnly Property _TabItemDetails As TabItem ' browse details
            Get
                Return _TabControl._tabitems(1)
            End Get
        End Property
        Private _trkType As TrkType
        Friend _tvPanel As TVStackAggPanel
        Private _fAllowFilter As Boolean
        Friend _allocs As List(Of HeapAllocationContainer)
        Friend _MergeCodemarkers As Boolean = False

        Public Property DynamicBackgroundConverter As IValueConverter

        Private _CustomColumnFactories As New Dictionary(Of String, CustomColumnFactory)() 'Stores custom columns to add to the view in addition to the ones defined by the query

        Public Property IsPivotLimited As Boolean ' for images, so we pivot only within current collection

        Public ReadOnly Property nTotal As Integer
            Get
                Return _tvPanel.nTotal
            End Get
        End Property
        Public ReadOnly Property nTotalSize As UInteger
            Get
                Return _tvPanel.NTotalSize
            End Get
        End Property
        Property InitialSortOrder As New BrowseInitialSortOrder With
            {.ColumnNo = -1,
             .direction = ComponentModel.ListSortDirection.Ascending}
        Property ColumnTips As Object() ' could be UIElement or string

        Sub New(ByVal queryFunc As BrowQueryDelegate,
                ByVal allocs As List(Of HeapAllocationContainer),
                Optional ByVal ColWidths As Integer() = Nothing,
                Optional ByVal tblkType As TrkType = TrkType.All,
                Optional ByVal fAllowBrowStringFilter As Boolean = False,
                Optional ByVal nPivotAddress As Integer = 0
                )
            _allocs = allocs
            _ColWidths = ColWidths
            _trkType = tblkType
            _fAllowFilter = fAllowBrowStringFilter
            _queryFunction = queryFunc

            _TabControl = New MyTabControl

            AddTabItemForBrowMem("Aggregate Call Stacks",
                                   "Currently allocations, aggregated by call stack with total counts, sizes",
                                   Nothing
                                   )
            AddTabItemForBrowMem("Details",
                                   "Each individual Currently Allocated Block, which you can hover over (the address) to show call stack or sort by various columns",
                                   AddressOf OnTabItemDetailsGotFocus
                                   )

            _qDetails = _queryFunction.Invoke(allocs, Me)

            If allocs IsNot Nothing Then
                AddTabItemForBrowMem("Thread Distribution",
                           "Distribution of allocations per thread",
                           Sub(sender As Object, e As RoutedEventArgs)
                               Dim thetabItem = CType(sender, TabItem)

                               Dim LiveThreadsDict = GetLiveThreads()

                               Dim qThrdSum = From a In allocs
                                              Where a.IsTrkBlkType(_trkType)
                                              Group By a.AllocationStruct.ThreadId
                                              Into Sum(a.GetSize), Count()
                                              Select ThreadId,
                                              Sum,
                                              Count,
                                              Live = If(LiveThreadsDict.ContainsKey(ThreadId), "", "Dead"),
                                              Note = If(LiveThreadsDict.ContainsKey(ThreadId), LiveThreadsDict(ThreadId).Note, "")
                                              Order By Sum Descending

                               Dim br = New Browse(qThrdSum,
                                                   fAllowBrowFilter:=True,
                                                   InitialSortOrder:=New BrowseInitialSortOrder With {.ColumnNo = 2},
                                                   ColTips:={"Thread ID",
                                                             "Sum of live allocations per thread",
                                                             "Count of live allocations per thread",
                                                             "IsThread dead (exited) or alive?",
                                                             "Minidump (for offline), or Main UI thrd or MemSpect priv thrd"
                                                            },
                                                        arrColumnsToTotal:={"Sum", "Count"}
                                                   )

                               Dim brctxmnu = br._BrowseList.ContextMenu
                               Dim mitem = brctxmnu.AddMnuItem("_SubSnapshot",
                                             "Create a new snapshot from the selected threads",
                                             Sub()
                                                 Dim items = br._BrowseList.SelectedItems
                                                 If items Is Nothing OrElse items.Count < 1 Then
                                                     items = br._BrowseList.Items
                                                 End If
                                                 Dim qAllocs = From itm In items
                                                               Let tdescitem = ComponentModel.TypeDescriptor.GetProperties(itm)("ThreadId"),
                                                                 Thread = CType(tdescitem.GetValue(itm), String)
                                                               From alloc In allocs
                                                               Where alloc.AllocationStruct.ThreadId.ToString = Thread
                                                               Select alloc


                                                 ShowSubSnapShot(qAllocs.ToList, "SubSnapThreads", _queryFunction)
                                             End Sub, 0)

                               thetabItem.Content = br

                           End Sub)

                AddTabItemForBrowMem("Size Distribution",
                                     "Distribution of allocations for each different sized alloc",
                                     Sub(sender As Object, e As RoutedEventArgs)
                                         Dim thetabItem = CType(sender, TabItem)
                                         Dim qSizeDist = From a In allocs
                                                         Where a.IsTrkBlkType(_trkType)
                                                         Group By a.GetSize Into Count()
                                                         Select
                                                            Size = GetSize,
                                                            Count
                                                         Order By Size


                                         Dim br = New Browse(qSizeDist,
                                                             fAllowBrowFilter:=True,
                                                             InitialSortOrder:=New BrowseInitialSortOrder With {.ColumnNo = 1},
                                                             ColTips:={"Size of allocation in bytes", "# of allocs"},
                                                             arrColumnsToTotal:={"Size", "Count"}
                                                             )

                                         Dim brctxmnu = br._BrowseList.ContextMenu
                                         Dim mitem = brctxmnu.AddMnuItem("_SubSnapshot",
                                                       "Create a new snapshot from the selected size distribution items",
                                                       Sub()
                                                           Dim items = br._BrowseList.SelectedItems
                                                           If items Is Nothing OrElse items.Count < 1 Then
                                                               items = br._BrowseList.Items
                                                           End If
                                                           Dim qAllocs = From itm In items
                                                                           Let tdescitem = ComponentModel.TypeDescriptor.GetProperties(itm)("Size"),
                                                                           Size = CType(tdescitem.GetValue(itm), Integer)
                                                                           From alloc In allocs
                                                                           Where alloc.GetSize = Size
                                                                           Select alloc


                                                           ShowSubSnapShot(qAllocs.ToList, "SubSnapSizeDist", _queryFunction)
                                                       End Sub, 0)


                                         thetabItem.Content = br
                                     End Sub)

                AddTabItemForBrowMem("MemoryLayout", "Shows a graph of the allocations sorted by Address",
                                     Sub(sender As Object, e As RoutedEventArgs)
                                         Dim thetabItem = CType(sender, TabItem)
                                         Try
                                             Dim allocsToUse = allocs.Where(Function(h) h.TBlkBlockType = _trkType).ToList

                                             Dim x = MemoryRegionGraphContainer.CreateMemoryRegionGraphContainerFromHctr(
                                                 allocsToUse,
                                                 _trkType)
                                             thetabItem.Content = x
                                             AddHandler thetabItem.LostFocus, Sub() x._RegionGraph.ClosePriorTipIfAny()
                                             AddHandler thetabItem.GotFocus, Sub() x._RegionGraph.ClosePriorTipIfAny()
                                             AddHandler thetabItem.IsVisibleChanged, Sub() x._RegionGraph.ClosePriorTipIfAny()
                                             AddHandler thetabItem.MouseLeave, Sub() x._RegionGraph.ClosePriorTipIfAny()
                                         Catch ex As Exception
                                             If _IsUnderTest Then
                                                 MsgBox(ex.ToString)
                                             End If
                                             thetabItem.Content = ex.ToString
                                         End Try

                                     End Sub)

            End If


            _tvPanel = New TVStackAggPanel(_qDetails, Me, nPivotRootAddress:=nPivotAddress)
            _TabItemStackAgg.Content = _tvPanel
            Me.Children.Add(_TabControl)

            If _GlobalFilter.KnownIssues = ShowKnownIssuesEnum.Include Then
                Dim x = KnownIssues.Known_Issues.Count
                If x = 0 Then
                    UpdateStatusMsg("not adding known Issues: none found")
                Else
                    Me.AddColumnForProperty("_HeapAllocationContainer.KnownIssue", 300, "KnownIssue",
                                            "The Known leak Issue prior from xml ")
                End If
            End If

            If _CodeMarkerMode <> CodeMarkerType.None Then
                Me.AddColumnForProperty("_HeapAllocationContainer.NearestCodeMarker", 500, "NearestCodeMarker",
                                        "The NearestCodeMarker prior to the current Seqno. DblClick to open in NotePad")
            End If
        End Sub

        ''' <summary>
        ''' add a new tab to the browmem
        ''' </summary>
        ''' <param name="strItem"></param>
        ''' <param name="strToolTip"></param>
        ''' <param name="delGotFocus">called when got focus and content is empty</param>
        Friend Function AddTabItemForBrowMem(ByVal strItem As String, ByVal strToolTip As String, ByVal delGotFocus As Action(Of Object, RoutedEventArgs)) As TabItem
            Dim tbitem = _TabControl.AddTabItem(strItem, strToolTip, delGotFocus)

            AddHandler tbitem.LostFocus, Sub() ClearPriorToolTipIfAny()
            AddHandler tbitem.GotFocus, Sub() ClearPriorToolTipIfAny()
            AddHandler tbitem.IsVisibleChanged, Sub() ClearPriorToolTipIfAny()
            Return tbitem
        End Function

        Friend Sub RemoveBrowseDetailsAndRefresh()
            If _DetailBrowse IsNot Nothing Then
                _TabItemDetails.Content = Nothing
                _DetailBrowse = Nothing
                OnTabItemDetailsGotFocus(_TabItemDetails, New RoutedEventArgs)
            End If
        End Sub

        'Creates a new column factory for the specified property name.  If it doesn't exist, an empty column will be created.
        Public Sub AddColumnForProperty(ByVal propertyName As String, ByVal columnWidth As Integer, ByVal columnHeaderText As String, ByVal columnHeaderTip As String)
            Dim columnFactory As New CustomColumnFactory(propertyName, columnWidth, columnHeaderText, columnHeaderTip)

            Me.AddCustomColumnFactory(columnFactory)
        End Sub

        'Adds an existing column factory to the collection.  If _DetailBrowse doesn't exist, then add it directly to the view.
        Public Sub AddCustomColumnFactory(ByVal customColumnFactory As CustomColumnFactory)
            If Not _CustomColumnFactories.ContainsKey(customColumnFactory.PropertyName) Then
                _CustomColumnFactories.Add(customColumnFactory.PropertyName, customColumnFactory)
            End If

            If Not _DetailBrowse Is Nothing Then
                CType(_DetailBrowse._BrowseList.View, GridView).Columns.Add(customColumnFactory.CreateColumn())
            End If
        End Sub


        ''' <summary>
        ''' Simple custom column factory for BrowseList
        ''' </summary>
        ''' <remarks>This will create a custom column that is bound to the property specified by PropertyName.  If the property doesn't exist 
        '''          in a specified item, it just has an empty entry.
        ''' 
        '''          Creating a new column every time is necessary since columns may be needed for multiple BrowseMem elements and the same column can't be reused.
        ''' </remarks>
        Public Class CustomColumnFactory
            Public Property PropertyName As String
            Public Property ColumnWidth As Integer
            Public Property ColumnHeaderText As String
            Public Property ColumnHeaderTip As String

            Public Sub New(ByVal propertyName As String, ByVal columnWidth As Integer, ByVal columnHeaderText As String, ByVal columnHeaderTip As String)
                Me.PropertyName = propertyName '"_HeapAllocationContainer.NearestCodeMarker"
                Me.ColumnWidth = columnWidth
                Me.ColumnHeaderText = columnHeaderText
                Me.ColumnHeaderTip = columnHeaderTip
            End Sub

            'Generates a new column based on the specified properties.
            Public Function CreateColumn() As GridViewColumn
                Dim newColumn As New GridViewColumn()

                Dim dataTemplate As New DataTemplate()
                newColumn.CellTemplate = dataTemplate

                Dim stackPanelFactory As New FrameworkElementFactory(GetType(StackPanel))
                stackPanelFactory.SetValue(StackPanel.OrientationProperty, Orientation.Horizontal)

                Dim textBlockFactory As New FrameworkElementFactory(GetType(TextBlock))
                Dim textBinder = New Binding(PropertyName) With {.Converter = New Browse.BrowseList.MyValueConverter()}

                'Dim toolTipBinder As New Binding(bindingName)
                Dim columnHeader As New GridViewColumnHeader() With {.Content = PropertyName, .Tag = PropertyName}

                textBlockFactory.SetBinding(TextBlock.TextProperty, textBinder)
                textBlockFactory.SetValue(TextBlock.NameProperty, ColumnHeaderText)

                stackPanelFactory.AppendChild(textBlockFactory)
                'textBlockFactory.SetBinding(TextBlock.ToolTipProperty, toolTipBinder)

                dataTemplate.VisualTree = stackPanelFactory

                newColumn.Header = columnHeader

                newColumn.Width = ColumnWidth
                CType(newColumn.Header, GridViewColumnHeader).Content = ColumnHeaderText
                CType(newColumn.Header, GridViewColumnHeader).ToolTip = New ToolTip() With {.Content = ColumnHeaderTip}

                Return newColumn
            End Function

        End Class

        Public Event OnTabItemDetailsCreatedEvent(ByVal sender As Object, ByVal e As RoutedEventArgs)

        Friend Sub OnTabItemDetailsGotFocus(ByVal sender As Object, ByVal e As RoutedEventArgs)
            Dim thetabItem = CType(sender, TabItem)
            If _DetailBrowse Is Nothing Then '1st time through or refresh
                Dim setAllocs = _allocs
                If _MergeCodemarkers AndAlso _trkType <> TrkType.CodeMarker AndAlso _allocs IsNot Nothing Then
                    setAllocs = New List(Of HeapAllocationContainer)

                    setAllocs.AddRange(_allocs) ' don't want to modify original
                    If _allocs.Count = 0 OrElse Not _allocs(0).IsMemSpectHeap Then ' don't add allocs from MemSpectHeap: they're already included in _allocs
                        setAllocs.AddRange(GetCodeMarkersToMerge)
                    End If
                End If


                _qDetails = _queryFunction.Invoke(setAllocs, Me)

                If _ColWidths Is Nothing Then
                    _ColWidths = {100, 100}
                End If
                _DetailBrowse = New Browse(_qDetails,
                                        fAllowHeaderClickSort:=True,
                                        ColWidths:=_ColWidths,
                                        fAllowBrowFilter:=_fAllowFilter,
                                        InitialSortOrder:=InitialSortOrder,
                                        ColTips:=ColumnTips,
                                        arrColumnsToTotal:=_arrColumnsToTotal
                                        )

                If DynamicBackgroundConverter IsNot Nothing Then
                    _DetailBrowse.SetDynamicBackground(DynamicBackgroundConverter, fAdditive:=False)
                End If

                If _GlobalFilter.KnownIssues = ShowKnownIssuesEnum.Include Then
                    Dim bordKnownIssues = New Border() With {
                        .Margin = New Thickness(1)
                    }
                    Dim btnSubSnapKnownIssues = New Button With {
                        .Content = " SubSnap non-_Known Issues",
                        .Margin = New Thickness(10, 0, 10, 0),
                        .HorizontalAlignment = Windows.HorizontalAlignment.Right,
                        .ToolTip = "SubSnapshot non-Known Issues."
                    }
                    bordKnownIssues.Child = btnSubSnapKnownIssues
                    _DetailBrowse._lfPanel.Children.Add(bordKnownIssues)
                    AddHandler btnSubSnapKnownIssues.Click,
                        Sub(obj, echk)
                            Dim items = _DetailBrowse._BrowseList.Items
                            Dim qAllocs = From item In items
                                          Select
                                            item
                                            Select hctr = HeapAllocationContainer.CreateFrom(item)
                                            Where hctr IsNot Nothing AndAlso
                                            String.IsNullOrEmpty(hctr.GetKnownIssue)

                            ShowSubSnapShot(qAllocs.ToList, "SubSnapNotKnown")
                        End Sub
                End If


                For Each columnFactory In _CustomColumnFactories.Values
                    'AddHandler CType(column.Header, GridViewColumnHeader).Click, New RoutedEventHandler(AddressOf _DetailBrowse._BrowseList.HandleHeaderClick)
                    Dim newcol = columnFactory.CreateColumn
                    CType(_DetailBrowse._BrowseList.View, GridView).Columns.Add(newcol)
                Next
                '_CustomColumnsToAdd.Clear()

                Dim ctm = _DetailBrowse._BrowseList.ContextMenu
                Dim mnidx = 0
                Dim mnuItem As MenuItem
                mnuItem = ctm.AddMnuItem("_SubSnapshot",
                               "Create a new snapshot from the selected items",
                               AddressOf on_CtxMnuBrowMemDetails, mnidx)
                mnidx += 1
                mnuItem = ctm.AddMnuItem("_Show stacks in Notepad",
                               "Dump selected items",
                               AddressOf on_CtxMnuBrowMemDetails, mnidx)
                mnidx += 1

                mnuItem = ctm.AddMnuItem("Show stacks and _Memory in Notepad",
                               "Dump selected items, including memory",
                               AddressOf on_CtxMnuBrowMemDetails, mnidx)
                mnidx += 1

                mnuItem = ctm.AddMnuItem("_References",
                                                           "References. For Managed objects: from and to this (these) object(s). For Native allocations, searches other allocations in same heap (unfiltered) that contain the address of the current allocation (potentially slow)",
                                                           AddressOf on_CtxMnuBrowMemDetails, mnidx)
                mnidx += 1
                If _trkType = TrkType.ClrObjects Then
                    mnuItem = ctm.AddMnuItem("Show _GC Roots",
                                   "Show all objects that are GC Roots",
                                   AddressOf on_CtxMnuBrowMemDetails, mnidx)
                    mnidx += 1
                Else
                    'If _trkType = TrkType.
                End If

                mnuItem = ctm.AddMnuItem("_GoTo in StackView",
                               "Open this item in the aggregate stack view, expanding all nodes. Doesn't work for pivots",
                               AddressOf on_CtxMnuBrowMemDetails, mnidx)
                mnidx += 1

                mnuItem = ctm.AddMnuItem("_Duplicates",
                               "Find dupes on all or selected items in this view",
                               AddressOf on_CtxMnuBrowMemDetails, mnidx)
                mnidx += 1

                mnuItem = ctm.AddMnuItem("_Waste",
                               "Find wasted allocations on all or selected items in this view",
                               AddressOf on_CtxMnuBrowMemDetails, mnidx)
                mnidx += 1

                mnuItem = ctm.AddMnuItem("_Unused Members",
                               "Treat selected allocations as a class, show unused members of each instance, Show class member value distribution",
                               AddressOf on_CtxMnuBrowMemDetails, mnidx)
                mnidx += 1

                mnuItem = ctm.AddMnuItem("Set_Lo Seq no",
                               "Record the seq no for Lo filter for next snapshot",
                               AddressOf on_CtxMnuBrowMemDetails, mnidx)
                mnidx += 1

                mnuItem = ctm.AddMnuItem("Set_Hi Seq no",
                               "Record the seq no for Hi filter for next snapshot",
                               AddressOf on_CtxMnuBrowMemDetails, mnidx)
                mnidx += 1

                mnuItem = ctm.AddMnuItem("_Assert On Seqno Free",
                               "Target process will assert when any of selected seqnos are freed",
                               AddressOf on_CtxMnuBrowMemDetails, mnidx)
                If _ConnectionMode <> MemSpectMode.OnLine Then
                    mnuItem.IsEnabled = False
                End If
                mnidx += 1

                mnuItem = ctm.AddMnuItem("Fill Memory",
                               "Fill memory with 'MemSpect' for items selected. Will double null terminate. " + vbCrLf +
                               "CLRObjs will not overwrite ClsId. System.String length is not overwritten",
                               AddressOf on_CtxMnuBrowMemDetails, mnidx)
                If _ConnectionMode <> MemSpectMode.OnLine Then
                    mnuItem.IsEnabled = False
                End If
                mnidx += 1

                mnuItem = ctm.AddMnuItem("_Merge Code Markers",
                                             "Add or remove the code markers into this data",
                                             AddressOf on_CtxMnuBrowMemDetails, mnidx)
                mnidx += 1

                If Me._MergeCodemarkers Then
                    mnuItem.IsChecked = True
                Else
                    mnuItem.IsChecked = False
                End If
                mnuItem = ctm.AddMnuItem("Show String content",
                                                  "try to show mem content as string for managed (System.String),native",
                                                  AddressOf on_CtxMnuBrowMemDetails, mnidx)
                mnidx += 1

                mnuItem.IsChecked = Common._ExpandStringContents



                _TabItemDetails.Content = _DetailBrowse
                _DetailBrowse._BrowseList.AddHandler(MouseMoveEvent, New RoutedEventHandler(Sub(o As Object, e2 As RoutedEventArgs)
                                                                                                On_MouseMove(o, e2)
                                                                                            End Sub))
                _DetailBrowse._BrowseList.AddHandler(Control.MouseDoubleClickEvent, New RoutedEventHandler(AddressOf On_DetailsDblClick))


                RaiseEvent OnTabItemDetailsCreatedEvent(thetabItem, e)

                ' because the tip disappears if you mouse directly over it we need to forward events
                _DetailBrowse._BrowseList.AddHandler(PreviewMouseWheelEvent,
                                                     New RoutedEventHandler(Sub(o As Object, e2 As RoutedEventArgs)
                                                                                If _LastTipObj IsNot Nothing Then
                                                                                    Dim lastTip = CType(_LastTipObj.ToolTip, ToolTip)
                                                                                    If lastTip.IsOpen AndAlso lastTip.Content IsNot Nothing Then
                                                                                        If TryCast(lastTip.Content, FrameworkElement) IsNot Nothing Then
                                                                                            Dim tipContent = CType(lastTip.Content, FrameworkElement)
                                                                                            tipContent.RaiseEvent(e2)
                                                                                        End If
                                                                                    End If

                                                                                End If
                                                                            End Sub
                ))

            End If
        End Sub

        'Sub ontabiTemVisibility() Handles _TabItem2.IsVisibleChanged,
        '        _TabItem1.IsVisibleChanged,
        '        _TabItem1.GotFocus,
        '        _TabItem2.LostFocus,
        '        _TabItem3.GotFocus,
        '        _TabItem3.LostFocus
        '    Me.ClearPriorToolTipIfAny()
        'End Sub
        ''' <summary>
        ''' returns filename
        ''' </summary>
        ''' <param name="itms">SelectedItems if from BrowMem, </param>
        ''' <param name="strTitle"></param>
        ''' <param name="hctr">if from StackAggView</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Shared Function ShowStacksInNotepad(
                                             ByVal itms As IList,
                                             ByVal strTitle As String,
                                             Optional ByVal hctr As HeapAllocationContainer = Nothing,
                                             Optional ByVal nMaxDumpSize As Integer = 65536
                                             ) As String
            ' called from Brow with SelectedItems, or from StackAggTree w/ hCtr
            Dim sbuilder = New Text.StringBuilder
            Dim mylam = Sub(itm As Object)
                            If Not String.IsNullOrEmpty(strTitle) Then
                                sbuilder.AppendLine(strTitle)
                                sbuilder.AppendLine()
                            End If

                            Dim prStacksAndDump = GetStacksAndDump(itm, nMaxDumpSize)
                            Dim sframes = prStacksAndDump.Key
                            Dim strAddrDump = prStacksAndDump.Value
                            sbuilder.Append(sframes)
                            sbuilder.AppendLine()
                            sbuilder.Append(strAddrDump)
                        End Sub
            If itms IsNot Nothing Then
                strTitle = ""
                For Each itm In itms
                    mylam.Invoke(itm)
                Next
            Else
                Debug.Assert(hctr IsNot Nothing)
                mylam.Invoke(hctr)
            End If
            Dim filename = WriteOutputToTempFile(sbuilder.ToString)
            Return filename
        End Function

        Private Sub on_CtxMnuBrowMemDetails(ByVal sender As Object, ByVal e As RoutedEventArgs)
            Dim selItem = Me._DetailBrowse._BrowseList.SelectedItem ' could be null
            Dim hctrSelected = HeapAllocationContainer.CreateFrom(selItem)
            If hctrSelected Is Nothing Then
                Return
            End If
            Dim mitem = CType(e.OriginalSource, MenuItem)
            Dim vrb = mitem.Header.ToString
            Select Case vrb
                Case "_Show stacks in Notepad", "Show stacks and _Memory in Notepad"
                    Dim items = Me._DetailBrowse._BrowseList.SelectedItems
                    Dim mdumpToo = vrb = "Show stacks and _Memory in Notepad"
                    ShowStacksInNotepad(If(items.Count > 0, items, Nothing), selItem.ToString, nMaxDumpSize:=If(mdumpToo, -1, 0)) ' some dumps are VERY big. 
                    'Case "_Pivot"
                    '    If hctrSelected.TblkBlockType <> BlockTypes.CodeMarker Then
                    '        Dim sframes = hctrSelected.ResolveCallStackFrames
                    '        Dim q = From frm In sframes Select _frame = frm, Frame = frm
                    '        Dim bSel = New Browse(q, delDblClick:=AddressOf On_DblClickFrame, ColWidths:={800})
                    '        _oWinSelectFrame = New MyWindow("Choose a stack frame about which to pivot " + selItem.ToString)
                    '        _oWinSelectFrame.Content = bSel
                    '        Dim w = GetAncestor(Of Window)(_menu._itemsctl)
                    '        _oWinSelectFrame.Owner = GetAncestor(Of Window)(_menu._itemsctl)
                    '        _oWinSelectFrame.ShowDialog()
                    '        If bSel._BrowseList.SelectedIndex >= 0 Then
                    '            Dim item = bSel._BrowseList.SelectedItem.ToString
                    '            BrowseMem.Pivot(hctrSelected, New IntPtr(1 + bSel._BrowseList.SelectedIndex), "Pivot " + selItem.ToString)
                    '        End If
                    '    End If
                Case "_GoTo in StackView"

                    '                                Dim r = Me._menu._bmem._TabItem1.Focus()
                    Me._TabControl.SelectedIndex = 0
                    If hctrSelected IsNot Nothing Then
                        Me._tvPanel._tv.ExpandToItem(hctrSelected)
                    End If

                Case "_Duplicates", "_Waste", "_SubSnapshot", "_Unused Members"
                    Dim items = Me._DetailBrowse._BrowseList.SelectedItems
                    If items Is Nothing OrElse items.Count < 1 Then
                        items = Me._DetailBrowse._BrowseList.Items
                    End If
                    Dim qAllocs = From item In items
                                  Select
                                    item
                                    Select hctr = HeapAllocationContainer.CreateFrom(item)
                                    Where hctr IsNot Nothing

                    If vrb = "_Duplicates" Then
                        ShowDuplicateAllocations(Nothing, qAllocs, " Source #= " + items.Count.ToString)
                    ElseIf vrb = "_Waste" Then
                        DoShowAllocationsWithWaste("Wasted items Source # = " + items.Count.ToString, qAllocs)
                    ElseIf vrb = "_Unused Members" Then
                        ShowUnusedMembers(qAllocs, "Unused ")
                    Else
                        ShowSubSnapShot(qAllocs.ToList, "SubSnapShot")
                    End If

                Case "_References"
                    For Each item In Me._DetailBrowse._BrowseList.SelectedItems
                        Dim hctr = HeapAllocationContainer.CreateFrom(item)
                        If hctr.TBlkBlockType = BlockTypes.ClrObject Then
                            Dim classname = hctr.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True)
                            classname = String.Format("{0} 0x{1:x8} ({2:n0})", classname, hctr.TBlk.Address.ToInt32, hctr.TBlk.Size)
                            TVObjRefPanel.CreateObjectReferenceDataSurface(hctr, classname)
                        Else
                            Dim results = GetReferences(hctr.GetAddr, hctr.SpyHeapPtr.TakeMemSnapshot(fEnableFilter:=False).Allocs)
                            ShowSubSnapShot(results, String.Format("References to {0:x8}", hctr.GetAddr.ToInt32))
                        End If
                    Next

                Case "Show _GC Roots"
                    TVObjRefPanel.ShowGCRoots()
                Case "Set_Lo Seq no", "Set_Hi Seq no"
                    Dim tdescSeqNo = ComponentModel.TypeDescriptor.GetProperties(selItem)("SeqNo")
                    If tdescSeqNo IsNot Nothing Then
                        Dim seqnoSelected = CType(tdescSeqNo.GetValue(selItem), UInteger)
                        If vrb = "Set_Lo Seq no" Then
                            _GlobalFilter.SeqNoLo = seqnoSelected
                        Else
                            _GlobalFilter.SeqNoHi = seqnoSelected
                        End If
                        _GlobalFilter.RefreshFilterDisplay()
                    End If
                Case "_Assert On Seqno Free"
                    If _ConnectionMode = MemSpectMode.OnLine Then
                        For Each item In Me._DetailBrowse._BrowseList.SelectedItems
                            Dim tdescSeqNo = ComponentModel.TypeDescriptor.GetProperties(item)("SeqNo")
                            If tdescSeqNo IsNot Nothing Then
                                Dim seqnoSelected = CType(tdescSeqNo.GetValue(item), Integer)
                                ProcComm.SendMsg(ProcMsgVerb.AssertSeqNo, fSendEndMsgSync:=True, dwords:={0, seqnoSelected})
                            End If
                        Next
                    End If
                Case "Fill Memory"
                    If _ConnectionMode = MemSpectMode.OnLine OrElse _ConnectionMode = MemSpectMode.Existing Then
                        Dim bEncode As New Text.UnicodeEncoding
                        For Each item In Me._DetailBrowse._BrowseList.SelectedItems
                            Dim hctr = HeapAllocationContainer.CreateFrom(item)
                            If hctr IsNot Nothing AndAlso hctr.TBlkBlockType <> BlockTypes.CodeMarker Then
                                hctr.WriteHeapAllocationStructMemory()
                                'Dim nIndex = 0
                                'Dim nLeftToWrite = hctr.GetSize
                                'While nLeftToWrite > 0
                                '    Dim bArray = bEncode.GetBytes(String.Format("MemSpect{0,4}", nIndex))
                                '    Dim bWritten = 0
                                '    Dim nbytesThisTime = Math.Min(bArray.Length, nLeftToWrite)
                                '    WriteProcessMemory(_hProcessTarget, hctr.GetAddr.MyAdd(nIndex * bArray.Length), bArray, nbytesThisTime, bWritten)
                                '    nLeftToWrite -= nbytesThisTime
                                '    nIndex += 1
                                'End While
                            End If
                        Next
                    End If

                Case "_Merge Code Markers"
                    Me._MergeCodemarkers = Not Me._MergeCodemarkers
                    Me.RemoveBrowseDetailsAndRefresh()
                Case "Show String content"
                    Common._ExpandStringContents = Not Common._ExpandStringContents
                    Me.RemoveBrowseDetailsAndRefresh()
                Case Else
                    MessageBox.Show("BMem ClicKed: " + mitem.Header.ToString)
            End Select

        End Sub

        Sub On_MouseLeave() Handles Me.MouseLeave
            Me.ClearPriorToolTipIfAny()
        End Sub


        Sub On_DetailsDblClick(ByVal sender As Object, ByVal e As RoutedEventArgs)
            Dim lv = TryCast(sender, Browse.BrowseList)
            Try
                If lv IsNot Nothing Then
                    Dim selItem = lv.SelectedItem
                    Dim tb = TryCast(e.OriginalSource, TextBlock)
                    If tb IsNot Nothing Then
                        Select Case tb.Name
                            Case "Address"
                                ClearPriorToolTipIfAny()
                                lv.Cursor = Cursors.Wait
                                Dim heapContainer = HeapAllocationContainer.CreateFrom(tb.DataContext)
                                ShowStacksInNotepad(lv.SelectedItems, "")
                            Case "NearestCodeMarker"
                                Dim txt = tb.Name + vbCrLf +
                                    tb.DataContext.ToString + vbCrLf +
                                    HeapAllocationContainer.CreateFrom(tb.DataContext).GetCodeMarkerTree
                                WriteOutputToTempFile(txt)
                            Case "StringContent"
                                Dim hctr = HeapAllocationContainer.CreateFrom(tb.DataContext)
                                Dim txt = hctr.ToString + vbCrLf +
                                    hctr.GetStringContent(fGetMemDumpToo:=True)
                                WriteOutputToTempFile(txt)
                        End Select
                    End If
                End If
            Catch ex As Exception
                CommonUI.MemSpectExceptionHandler(ex)
            Finally
                lv.Cursor = Cursors.Arrow
            End Try
        End Sub

        Private _LastTipObj As FrameworkElement ' the control (like Textbox) that has the .ToolTip property
        Public Sub ClearPriorToolTipIfAny()
            If _LastTipObj IsNot Nothing Then
                If _LastTipObj.ToolTip IsNot Nothing Then
                    Dim lastTip = CType(_LastTipObj.ToolTip, ToolTip)
                    lastTip.IsOpen = False
                    '_LastTipObj.ToolTip = Nothing
                End If
                _LastTipObj = Nothing
            End If
        End Sub

        Sub On_MouseMove(ByVal sender As Object, ByVal e As RoutedEventArgs)
            Dim lv As Browse.BrowseList = Nothing
            Try
                lv = TryCast(sender, Browse.BrowseList)
                If lv IsNot Nothing Then
                    Dim tb = TryCast(e.OriginalSource, TextBlock)
                    If tb IsNot Nothing Then
                        '                    Dim o = lv.ItemContainerGenerator.ContainerFromItem(tb)
                        If _LastTipObj IsNot Nothing Then
                            Dim lastTip = CType(_LastTipObj.ToolTip, ToolTip)
                            If tb Is _LastTipObj Then ' over same obj: don't create a new tip
                                Return
                            Else
                                ' different object: close the tip
                                lastTip.IsOpen = False
                            End If
                        End If
                        tb.ToolTip = Nothing
                        ClearPriorToolTipIfAny()
                        Dim dt = tb.DataContext
                        If dt IsNot Nothing Then
                            Dim ttipObj = New ToolTip
                            ttipObj.PlacementTarget = tb
                            ttipObj.Placement = Controls.Primitives.PlacementMode.Bottom
                            Select Case tb.Name
                                Case "Address"
                                    lv.Cursor = Cursors.Wait

                                    ttipObj.LayoutTransform = New ScaleTransform(_scaleX, _scaleY)
                                    AddHandler ttipObj.PreviewMouseWheel, Sub(o As Object, e2 As MouseWheelEventArgs)
                                                                              If Keyboard.Modifiers = Windows.Input.ModifierKeys.Control Then
                                                                                  Dim tv = CType(o, FrameworkElement)
                                                                                  Dim tr = CType(tv.LayoutTransform, ScaleTransform)
                                                                                  If e2.Delta > 0 Then
                                                                                      tr.ScaleX *= 1.1
                                                                                      tr.ScaleY *= 1.1
                                                                                  Else
                                                                                      tr.ScaleX /= 1.1
                                                                                      tr.ScaleY /= 1.1
                                                                                  End If
                                                                                  _scaleX = tr.ScaleX
                                                                                  _scaleY = tr.ScaleY
                                                                                  e2.Handled = True
                                                                              End If

                                                                          End Sub


                                    ttipObj.Content = GetAddressToolTip(dt)
                                    lv.Cursor = Cursors.Arrow
                                Case "StringContent"
                                    Dim hctr = HeapAllocationContainer.CreateFrom(dt)

                                    ttipObj.Content = New MonospacefontTextBlockForToolTip With {
                                        .Text = hctr.ToString + vbCrLf +
                                            hctr.GetStringContent(fGetMemDumpToo:=True)
                                        }
                                Case "NearestCodeMarker"
                                    ttipObj.Content = New MonospacefontTextBlockForToolTip With {
                                        .Text = HeapAllocationContainer.CreateFrom(dt).GetCodeMarkerTree
                                    }
                                Case "CodeMarkerUserData"
                                    Dim strtip = String.Empty
                                    Dim hctr = HeapAllocationContainer.CreateFrom(dt)
                                    Dim cmdata = hctr.GetCodeMarkerData
                                    If cmdata.MarkerUserDataLen > 0 Then
                                        Dim memdmp = GetMemoryDump(cmdata.MarkerUserDataAddr, cmdata.MarkerUserDataLen)
                                        strtip = cmdata.MarkerUserData + vbCrLf + memdmp
                                    Else
                                        strtip = "No User data"
                                    End If

                                    ttipObj.Content = New MonospacefontTextBlockForToolTip With {
                                        .Text = strtip
                                    }
                                Case "SeqNo"
                                    ttipObj.Content = New MonospacefontTextBlockForToolTip With {
                                        .Text = "Allocation sequence number, (1,2,3...) for chonology" + vbCrLf + tb.Text
                                    }
                                Case Else
                                    Dim strTip = tb.Text
                                    Dim hctr = HeapAllocationContainer.CreateFrom(dt)
                                    If hctr IsNot Nothing Then
                                        strTip = "(Hover over Address column to see call stack. Double click to see tip in notepad)" + vbCrLf +
                                            tb.Text
                                    End If
                                    ttipObj.Content = New MonospacefontTextBlockForToolTip With {
                                        .Text = strTip
                                        }
                            End Select
                            tb.ToolTip = ttipObj
                            _LastTipObj = tb
                            ToolTipService.SetShowDuration(tb, 10000)
                            Dim tt As New DispatcherTimer With {
                                .Interval = New TimeSpan(0, 0, 0, 0, 600)
                            }
                            AddHandler tt.Tick, AddressOf openttip
                            tt.Start()
                            ToolTipService.SetBetweenShowDelay(tb, 500)
                        End If
                    End If
                End If

            Catch ex As Exception

            Finally
                If lv IsNot Nothing Then
                    lv.Cursor = Cursors.Arrow
                End If
            End Try

        End Sub

        'Sub On_MouseRightButtonDown(ByVal sender As Object, ByVal e As RoutedEventArgs) Handles _DetailBrowse.MouseRightButtonDown
        '    ClearPriorToolTipIfAny()
        '    If e.OriginalSource.GetType Is GetType(TextBlock) AndAlso CType(e.OriginalSource, TextBlock).Text = "Address" Then
        '        'Dim sfd = New Microsoft.Win32.SaveFileDialog ' this causes lots of Shell IMalloc activity
        '        Dim fname = System.IO.Path.Combine(System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), "Stacks.Log")
        '        If MessageBox.Show("Write Addresses/stacks to " + fname + "?", "DumpStacks (takes a while)", MessageBoxButton.YesNo, MessageBoxImage.Question) = MessageBoxResult.Yes Then
        '            Me.Cursor = Cursors.Wait
        '            DumpAddressesAndStacksToLog(fname)
        '            Process.Start(fname)
        '            Me.Cursor = Cursors.Arrow
        '        End If
        '    End If

        'End Sub

        'Sub DumpAddressesAndStacksToLog(ByVal outFilename As String)
        '    If File.Exists(outFilename) Then
        '        File.Delete(outFilename)
        '    End If
        '    Dim StackFilters As List(Of String) = Nothing
        '    Dim StackFilterFileName = System.IO.Path.Combine(System.IO.Path.GetDirectoryName(_iniFileName), "StackFilters.txt")
        '    If File.Exists(StackFilterFileName) Then
        '        StackFilters = New List(Of String)
        '        StackFilters.AddRange(File.ReadAllLines(StackFilterFileName))
        '        If StackFilters.Count = 0 Then
        '            StackFilters = Nothing
        '        End If
        '    End If

        '    Using writer = If(File.Exists(outFilename), File.AppendText(outFilename), File.CreateText(outFilename))
        '        Dim defview = CollectionViewSource.GetDefaultView(_DetailBrowse._BrowseList.ItemsSource)
        '        Dim srcColl = defview.SourceCollection

        '        For Each row In srcColl
        '            Dim prStacksAndDump = GetStacksAndDump(row, nMaxDumpSize:=1024, stkFilters:=StackFilters)
        '            Dim strAddrDump = prStacksAndDump.Value
        '            If Not String.IsNullOrEmpty(strAddrDump) Then ' did it get filtered?
        '                writer.WriteLine(prStacksAndDump.Key)
        '                writer.WriteLine()
        '                writer.WriteLine(strAddrDump)
        '            End If

        '        Next
        '    End Using
        'End Sub

        Public Sub openttip(ByVal sender As Object, ByVal e As EventArgs)
            Dim tmr = CType(sender, DispatcherTimer)
            If _LastTipObj IsNot Nothing Then
                If _LastTipObj.ToolTip IsNot Nothing Then
                    CType(_LastTipObj.ToolTip, ToolTip).IsOpen = True
                    RemoveHandler tmr.Tick, AddressOf openttip
                End If
            End If
            tmr.Stop()

        End Sub


        Public Shared Function Pivot(
                                    ByVal hctrTarget As HeapAllocationContainer,
                                    ByVal nDepthOrTargetAddress As IntPtr,
                                    ByVal strTitle As String,
                                    Optional ByVal listToPivot As List(Of HeapAllocationContainer) = Nothing
                                    ) As DataSurface
            Dim ctrls As DataSurface = Nothing
            If hctrTarget Is Nothing AndAlso nDepthOrTargetAddress.ToInt32 < 2 Then 'OrElse _tvm._nStartAddr <> 0 Then
                Throw New InvalidOperationException("can't pivot so low") ' or pivot a pivot")
            Else
                Dim dwTargetAddr As IntPtr
                If hctrTarget Is Nothing Then ' pivoting a pivot
                    dwTargetAddr = nDepthOrTargetAddress
                Else
                    dwTargetAddr = hctrTarget.GetCallStackAddr(nDepthOrTargetAddress.ToInt32() - 1)
                End If
                Dim dwPivotAddress As IntPtr
                dwPivotAddress = dwTargetAddr
                Dim heapAllocs As List(Of HeapAllocationContainer)
                If listToPivot IsNot Nothing Then
                    heapAllocs = listToPivot
                Else
                    heapAllocs = Common.GetAllocsForPivot(dwTargetAddr).ToList()
                End If
                ctrls = ShowPivot(heapAllocs, strTitle, dwPivotAddress)
            End If
            Return ctrls
        End Function

        Friend Shared Function ShowPivot(ByVal heapAllocs As List(Of HeapAllocationContainer),
                                         ByVal strTitle As String,
                                         ByVal dwPivotAddress As IntPtr) As DataSurface
            Dim ctrls = DataWindowMain.MakeNewDatasurface("Pivot", strTitle, nMaxHeaderHeight:=40)

            Dim sp As New StackPanel With {.Orientation = Orientation.Vertical}
            ctrls.SurfaceHeader.Children.Add(sp)
            Dim agg = Aggregate a In heapAllocs Into cnt = Count(), nSum = Sum(a.GetSize)


            sp.Children.Add(New TextBlock With {
                .Text = strTitle + "   Cnt=" + agg.cnt.ToString("n0") + " Size=" + agg.nSum.ToString("n0")
            })
            sp.Children.Add(New TextBlock With {
                .Text = _GlobalFilter.ToString
            })
            'Dim spSum As New StackPanel With {.Orientation = Orientation.Horizontal}
            'sp.Children.Add(spSum)
            'Dim qHeapSum = From hctr In heapAllocs Group By Heap = hctr.SpyHeapPtr.GetHeapName Into Count(), Sum(hctr.GetSize)
            '          Select Heap, Count, Size = Sum

            'spSum.Children.Add(New Browse(qHeapSum))
            Dim qfunc = Function(theheapAllocs As List(Of HeapAllocationContainer), thebmem As BrowseMem) As IEnumerable
                            'theheapallocs isnull
                            Dim q = From hctr In heapAllocs
                                    Select
                                        Address = hctr.GetAddr.ToString("x8"),
                                        hctr.AllocationStruct.SeqNo,
                                        Size = hctr.GetSize,
                                        hctr.AllocationStruct.Thread,
                                        BlkType = hctr.GetBlockTypeName,
                                        Data =hctr.GetDisplayData,
                                        Heap = If(hctr.SpyHeapPtr Is Nothing, String.Empty, hctr.SpyHeapPtr.GetHeapName),
                                        _HeapAllocationContainer = hctr
                                    Order By Size Descending, BlkType

                            Return q
                        End Function
            Dim bmem = New BrowseMem(qfunc,
                                     heapAllocs,
                                     nPivotAddress:=dwPivotAddress.ToInt32,
                                     fAllowBrowStringFilter:=True,
                                     ColWidths:={WIDTH_ADDRESS, WIDTH_SEQNO, 65, 65, 80, 300, 100})
            ctrls.SurfaceDetails.Children.Add(bmem)
            bmem._arrColumnsToTotal = {"Size"}

            Dim cm = bmem._tvPanel._tv.ContextMenu
            cm.AddMnuItem("Dump to _XML", "Dump the stack tree into an XML file.", Sub(sender As Object, e As RoutedEventArgs)
                                                                                       Dim selectedFrameNode = TryCast(bmem._tvPanel._tv.SelectedItem, TVStackAggPanel.StackAggTreeView.StackAggTViewItem)
                                                                                       If selectedFrameNode Is Nothing Then
                                                                                           MessageBox.Show("Please first select a node.", "Error", MessageBoxButton.OK, MessageBoxImage.Error)
                                                                                       Else
                                                                                           Dim dumpToXmlWindow = New DumpToXmlWindow(selectedFrameNode)
                                                                                           dumpToXmlWindow.ShowDialog()
                                                                                       End If
                                                                                   End Sub)
            Return ctrls
        End Function
    End Class

End Namespace
