'ManagedObjViewer.vb
Imports Microsoft.VisualStudio.Utilities
Imports System.ComponentModel.Composition
Imports Microsoft.VisualStudio.Text.Editor
Imports System.Windows.Controls
Imports System.Windows.Threading
Imports System.Windows.Data
Imports System.Windows
Imports System.IO
Imports System.Windows.Media
Imports System.Collections.ObjectModel
Imports System.Runtime.CompilerServices
Imports System.Linq
Imports Microsoft.VisualStudio.Text
Imports Microsoft.VisualBasic.TestHooks
Imports System.Xml.Linq
Imports System.Runtime.InteropServices
Imports Microsoft.VisualStudio.Text.Projection

Namespace Microsoft.VisualBasic.Editor


    Public Class ManagedObjViewer
        'Inherits ui
        Private _BucketList As ReadOnlyObservableCollection(Of VBObjectTracker.ObjectBucket)
        Sub New(ByVal BucketList As ReadOnlyObservableCollection(Of VBObjectTracker.ObjectBucket), ByVal spVBD As StackPanel)
            _BucketList = BucketList
            Dim qManObj = From Ob In BucketList
                         Select Ob.BucketName, Ob.LivingReferences, Ob.DeadReferences

            'spVBD.Children.Add(New Browse(qManObj) With {
            '                 .Background = Brushes.LightSalmon,
            '                 .ToolTip = "Class Name,   Live Count,    Released Count. DblClick to see IPropertyCollection"
            '             })
            Dim xaml = _
                    <ListView Name="MyPanel"
                        xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
                        xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
                        Background="LightSalmon"
                        ToolTip="Class Name,   Live Count,    Released Count"
                        >
                    </ListView>

            Dim lv = CType(System.Windows.Markup.XamlReader.Load(xaml.CreateReader), ListView)

            'see http://blogs.msdn.com/calvin_hsia/archive/2007/11/12/6146537.aspx
            Dim dt As New DataTemplate
            Dim factsp = New FrameworkElementFactory(GetType(StackPanel))
            factsp.SetValue(StackPanel.OrientationProperty, Orientation.Horizontal)
            dt.VisualTree = factsp

            Dim factTB = New FrameworkElementFactory(GetType(TextBlock))
            factTB.SetBinding(TextBlock.TextProperty, New Binding("BucketName") With {.Mode = BindingMode.OneWay})
            factTB.SetValue(TextBlock.WidthProperty, 180.0)
            factsp.AppendChild(factTB) ' add the textblock to the listbox row stackpanel


            factTB = New FrameworkElementFactory(GetType(TextBlock))
            factTB.SetBinding(TextBlock.TextProperty, New Binding("LivingReferences.Count") With {.Mode = BindingMode.OneWay})
            factTB.SetValue(TextBlock.WidthProperty, 32.0)
            factsp.AppendChild(factTB) ' add the textblock to the listbox row stackpanel

            factTB = New FrameworkElementFactory(GetType(TextBlock))
            factTB.SetBinding(TextBlock.TextProperty, New Binding("DeadReferences.Count") With {.Mode = BindingMode.OneWay})
            factTB.SetValue(TextBlock.WidthProperty, 32.0)
            factsp.AppendChild(factTB) ' add the textblock to the listbox row stackpanel

            lv.ItemsSource = BucketList

            lv.ItemTemplate = dt
            'AddHandler lb.MouseMoveEvent, AddressOf OnMouseMove
            lv.AddHandler(ListView.MouseDoubleClickEvent, New RoutedEventHandler(AddressOf OnMouseDblClick))
            lv.AddHandler(ListView.MouseMoveEvent, New RoutedEventHandler(AddressOf On_MouseMove))
            spVBD.Children.Add(lv)
            AddHandler lv.MouseLeave, Sub()
                                          ClearPriorToolTipIfAny()
                                      End Sub

        End Sub

        Private _LastTipObj As FrameworkElement
        Sub ClearPriorToolTipIfAny()
            If _LastTipObj IsNot Nothing Then
                Dim lastTip = CType(_LastTipObj.ToolTip, ToolTip)
                lastTip.IsOpen = False
                _LastTipObj = Nothing
            End If
        End Sub

        Sub On_MouseMove(ByVal sender As Object, ByVal e As RoutedEventArgs)
            Try
                Dim lv = TryCast(sender, ListView)
                If lv IsNot Nothing Then
                    Dim tb = TryCast(e.OriginalSource, TextBlock)
                    If tb IsNot Nothing Then
                        Dim o = lv.ItemContainerGenerator.ContainerFromItem(tb)
                        If _LastTipObj IsNot Nothing Then
                            Dim lastTip = CType(_LastTipObj.ToolTip, ToolTip)
                            If tb Is _LastTipObj Then ' over same obj: don't create a new tip
                                Return
                            Else
                                ' different object: close the tip
                                lastTip.IsOpen = False
                            End If
                        End If

                        Dim dt = tb.DataContext
                        If dt IsNot Nothing Then
                            Dim ttipObj = New ToolTip
                            ttipObj.PlacementTarget = tb
                            ttipObj.Placement = Controls.Primitives.PlacementMode.Bottom
                            If Char.IsLetter(tb.Text(0)) Then
                                Dim sb As New Text.StringBuilder
                                Dim className = tb.Text
                                sb.AppendLine("BucketName = " + className + " DblClick to see details for IPropertyOwner.")
                                sb.AppendLine("To track more managed objects in list, copy CHK bits. See VBDiagMargin.Docx")
                                Dim q = From bkt In _BucketList Where bkt.BucketName = className

                                If tb.Text.StartsWith("VB") Then
                                    className = tb.Text.Substring(2)
                                End If

                                If q.Count > 0 Then
                                    Dim objbucket = q.First
                                    If objbucket.LivingReferences.Count > 1 AndAlso className = "WPF Text Views" Then
                                        Dim nCnt = 0
                                        For Each wkref In objbucket.LivingReferences
                                            nCnt += 1
                                            If wkref.IsAlive Then
                                                Dim wpfView = CType(wkref.Target, IWpfTextView)
                                                Dim snap = wpfView.TextBuffer.CurrentSnapshot

                                                Dim txt = If(snap.LineCount > 0, snap.Lines(0).GetText, "")
                                                sb.AppendLine(nCnt.ToString + ": " + wpfView.TextBuffer.ContentType.TypeName + " # of Lines = " + snap.LineCount.ToString + " Line 1: " + txt)
                                            Else
                                                sb.AppendLine("Dead ref")
                                            End If
                                        Next
                                    Else
                                        For Each wkref In objbucket.LivingReferences
                                            If wkref.IsAlive Then
                                                Dim testobj = wkref.Target
                                                Dim plist = GetPropertyList(testobj)
                                                If plist IsNot Nothing Then

                                                    sb.AppendLine("is IPropertyOwner")
                                                    For Each p In plist
                                                        sb.AppendLine("Key= " + p.Key.ToString + "         Val=" + p.Value.ToString)
                                                    Next

                                                End If
                                                sb.AppendLine(testobj.GetType.FullName)
                                            End If
                                        Next
                                    End If
                                End If
                                ttipObj.Content = sb.ToString

                            Else
                                Return
                            End If

                            tb.ToolTip = ttipObj
                            _LastTipObj = tb
                            ToolTipService.SetShowDuration(tb, 10000)
                            _tmrToolTip = New DispatcherTimer With {
                                .Interval = New TimeSpan(0, 0, 0, 0, 600)
                            }
                            AddHandler _tmrToolTip.Tick, AddressOf openttip
                            _tmrToolTip.Start()
                            ToolTipService.SetBetweenShowDelay(tb, 500)
                        End If
                    End If
                End If

            Catch ex As Exception

            End Try

        End Sub
        Private _tmrToolTip As DispatcherTimer
        Public Shared Function GetPropertyList(ByVal obj As Object) As ReadOnlyCollection(Of KeyValuePair(Of Object, Object))
            Dim retval As ReadOnlyCollection(Of KeyValuePair(Of Object, Object)) = Nothing
            Dim typ = obj.GetType
            If typ.GetInterface(GetType(IPropertyOwner).Name) IsNot Nothing Then
                Dim pinfo = typ.GetProperty("Properties")
                Dim props = pinfo.GetValue(obj, Nothing)

                retval = CType(props, PropertyCollection).PropertyList
            End If
            Return retval
        End Function

        Public Sub openttip(ByVal sender As Object, ByVal e As EventArgs)
            If _LastTipObj IsNot Nothing Then
                If _LastTipObj.ToolTip IsNot Nothing Then
                    Dim tip = CType(_LastTipObj.ToolTip, ToolTip)
                    If tip.IsOpen = False Then
                        tip.IsOpen = True
                        _tmrToolTip.Interval = New TimeSpan(0, 0, 0, 5000)
                        '                        RemoveHandler CType(sender, DispatcherTimer).Tick, AddressOf openttip
                    Else
                        _tmrToolTip = Nothing
                        ClearPriorToolTipIfAny()
                    End If
                End If
            End If
        End Sub

        Public Sub OnMouseDownTip(ByVal sender As Object, ByVal e As EventArgs)
            ClearPriorToolTipIfAny()
            If _LastTipObj IsNot Nothing Then
                If _LastTipObj.ToolTip IsNot Nothing Then
                    CType(_LastTipObj.ToolTip, ToolTip).IsOpen = True
                    RemoveHandler CType(sender, DispatcherTimer).Tick, AddressOf openttip
                End If
            End If
        End Sub


        Sub OnMouseDblClick(ByVal sender As Object, ByVal e As RoutedEventArgs)
            Try
                Dim lv = TryCast(sender, ListView)
                If lv IsNot Nothing Then
                    Dim tb = TryCast(e.OriginalSource, TextBlock)
                    If tb IsNot Nothing Then
                        ClearPriorToolTipIfAny()
                        Dim o = lv.ItemContainerGenerator.ContainerFromItem(tb)
                        Dim dt = tb.DataContext
                        If dt IsNot Nothing Then
                            If Char.IsLetter(tb.Text(0)) Then
                                Dim className = tb.Text
                                Dim q = From bkt In _BucketList Where bkt.BucketName = className

                                'If tb.Text.StartsWith("VB") Then
                                '    className = tb.Text.Substring(2)
                                'End If

                                If q.Count > 0 Then
                                    Dim objbucket = q.First
                                    For Each wkref In objbucket.LivingReferences
                                        If wkref.IsAlive Then
                                            Dim testobj = wkref.Target
                                            Dim typ = wkref.Target.GetType
                                            If typ.GetInterface(GetType(IPropertyOwner).Name) IsNot Nothing Then
                                                Dim pinfo = typ.GetProperty("Properties")
                                                Dim properties = pinfo.GetValue(testobj, Nothing)

                                                Dim pcoll = CType(properties, PropertyCollection).PropertyList
                                                Dim oWin = New PropertyBagBrowser(pcoll, className, testobj)

                                            End If
                                        End If
                                    Next
                                End If
                            End If
                        End If
                    End If
                End If

            Catch ex As Exception

            End Try


        End Sub



    End Class

    Public Class PropertyBagBrowser
        Inherits Window
        Private _pcoll As ReadOnlyCollection(Of KeyValuePair(Of Object, Object))
        Sub New(ByVal pcoll As ReadOnlyCollection(Of KeyValuePair(Of Object, Object)), ByVal className As String, ByVal testobj As Object)
            _pcoll = pcoll
            Dim wpfView = TryCast(testobj, IWpfTextView)
            Me.Title = "PropertyBag Browser " + className
            If wpfView IsNot Nothing Then
                Dim snap = wpfView.TextBuffer.CurrentSnapshot

                Dim txt = If(snap.LineCount > 0, snap.Lines(0).GetText, "")
                Me.Title += " " + wpfView.TextBuffer.ContentType.TypeName + " # of Lines = " + snap.LineCount.ToString + " Line 1: " + txt

            End If
            Dim q = From p In pcoll
                    Select Key = p.Key.ToString,
                            Value = p.Value.ToString,
                            HasProps = If(ManagedObjViewer.GetPropertyList(p.Value) Is Nothing, " ", "Y")
            Dim b = New Browse(q, delDblClick:=AddressOf OnDblClickPropBag)
            Me.Content = b
            Me.Show()
        End Sub

        Sub OnDblClickPropBag(ByVal sender As Object, ByVal e As RoutedEventArgs)
            Try
                Dim lv = TryCast(sender, ListView)
                If lv IsNot Nothing Then
                    Dim tb = TryCast(e.OriginalSource, TextBlock)
                    If tb IsNot Nothing Then
                        Dim o = lv.ItemContainerGenerator.ContainerFromItem(tb)
                        Dim dt = tb.DataContext
                        If dt IsNot Nothing Then
                            If tb.Name = "Key" Then
                                Dim q = From a In _pcoll Where tb.Text = a.Key.ToString

                                If q.Count > 0 Then
                                    Dim obj = q.First.Value
                                    Dim owin = New PropertyBrowser(obj)
                                End If
                            End If
                        End If
                    End If
                End If

            Catch ex As Exception

            End Try
        End Sub

        Public Function DoesObjHaveAnyPropsThatOwnProps(ByVal obj As Object) As Boolean
            Dim retval = False
            Dim props = obj.GetType.GetProperties
            For Each prop In props

            Next

            Return retval
        End Function
    End Class

    Public Class PropertyBrowser
        Inherits Window
        Private _props As New List(Of KeyValuePair(Of String, String))
        Sub New(ByVal obj As Object)
            Me.Title = "Property Browser " + obj.GetType.FullName
            Dim props = obj.GetType.GetProperties

            For Each prop In props
                Dim val = prop.GetValue(obj, Nothing)
                If val IsNot Nothing Then
                    _props.Add(New KeyValuePair(Of String, String)(prop.Name, val.ToString))
                End If

            Next
            Dim q = New Browse(_props)
            Me.Content = q
            Me.Show()

        End Sub

    End Class
End Namespace