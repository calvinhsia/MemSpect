Imports System.Text

Imports MemSpect

Public Class LeakSummaryControl
    Implements IDisposable

    Dim _leakDetector As LeakDetector
    Private _browse As MemSpect.Browse.BrowseList

    Public Sub New(ByVal leakDetector As LeakDetector)
        ' This call is required by the designer.
        InitializeComponent()

        _leakDetector = leakDetector

        ' Add any initialization after the InitializeComponent() call.
        Dim summaryItems = From group In _leakDetector.LeakGroups
                           Select
                               group.LeakID,
                               group.LeakSize,
                               AllocationCount = group.LeakGroup.Count(),
                               _group = group
                           Order By LeakSize Descending

        Dim browse = New MemSpect.Browse(summaryItems)
        _browse = browse._BrowseList
        AddHandler _browse.MouseDoubleClick, AddressOf ShowLeakContainerSubSnapshot
        AddHandler _browse.SelectionChanged, AddressOf LeakSelectionChanged

        browse.HorizontalAlignment = Windows.HorizontalAlignment.Stretch
        browse.VerticalAlignment = Windows.VerticalAlignment.Stretch

        Me.LeakSummaryDockPanel.Children.Add(browse)

        'Me.UpdateLayout()
    End Sub

    Public Sub ShowLeakContainerSubSnapshot(ByVal sender As Object, ByVal e As MouseButtonEventArgs)

        Dim tdescitem = ComponentModel.TypeDescriptor.GetProperties(_browse.SelectedItem)("_group")
        Dim selectedLeak = CType(tdescitem.GetValue(_browse.SelectedItem), LeakDetector.LeakSummaryContainer)

        Dim dataSurface = ShowSubSnapShot(selectedLeak.LeakGroup.ToList(), "LeakGroup" + selectedLeak.LeakID.ToString() + " ")

        CommonUI.ShowGroupIDForDataSurface(dataSurface, _leakDetector.LeakDetectorID)

    End Sub

    Public Sub LeakSelectionChanged(ByVal sender As Object, ByVal e As SelectionChangedEventArgs)
        If e.AddedItems.Count <> 1 Then
            LeakSummaryTextBox.Text = String.Empty
            Return
        End If

        Dim tdescitem = ComponentModel.TypeDescriptor.GetProperties(e.AddedItems(0))("_group")
        Dim selectedLeak = CType(tdescitem.GetValue(e.AddedItems(0)), LeakDetector.LeakSummaryContainer)

        LeakSummaryTextBox.Text = selectedLeak.LeakSummary
    End Sub

#Region "IDisposable Support"
    Private disposedValue As Boolean ' To detect redundant calls

    ' IDisposable
    Protected Overridable Sub Dispose(ByVal disposing As Boolean)
        If Not Me.disposedValue Then
            If disposing Then
                RemoveHandler _browse.MouseDoubleClick, AddressOf ShowLeakContainerSubSnapshot
                RemoveHandler _browse.SelectionChanged, AddressOf LeakSelectionChanged
            End If

            _browse = Nothing
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

    Private Sub SaveButton_Click(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles SaveButton.Click
        Dim output As New StringBuilder()

        Dim orderedGroups = From group In _leakDetector.LeakGroups
                            Select group
                            Order By group.LeakSize Descending

        For Each group In orderedGroups
            output.AppendLine(group.LeakSummary)
            output.AppendLine()
        Next

        WriteOutputToTempFile(output.ToString())
    End Sub
End Class

