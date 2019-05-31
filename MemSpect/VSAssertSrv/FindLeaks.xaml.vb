Imports System
Imports System.IO

Imports MemSpect
Imports System.ComponentModel

'MultiFilter: Currently used for defining ranges to search for leaks but might be merged with the GlobalFilter functionality to allow filtering with multiple ranges
Public Class FindLeaksWindow
    Private _filterRanges As List(Of Tuple(Of Int32, Int32)) = New List(Of Tuple(Of Int32, Int32))()
    'Private _heapID As String
    Private _allocs As IEnumerable(Of HeapAllocationContainer)
    Private _minMatchThresholdPct As Integer = 85
    Private _leakDetector As New LeakDetector()

    Public WithEvents _workerThread As BackgroundWorker

    Public Sub New()
        Me.Initialize()
    End Sub

    Public Sub New(ByVal leakDetectionParams As LeakDetector.LeakDetectorParameters)
        Me.Initialize()

        If Not leakDetectionParams Is Nothing Then
            Me.traditionalLeakCheckBox.IsChecked = leakDetectionParams.FindTraditionalLeaksOnly
            Me.minThresholdTextBox.Text = CStr(leakDetectionParams.MatchThreshold * 100)

            Me.MultiFilterTextBox.Text = ""
            For Each range In leakDetectionParams.FilterRanges
                Me.MultiFilterTextBox.Text += range.Item1.ToString() + "," + range.Item2.ToString() + vbCrLf
            Next
        End If

    End Sub

    Public Property LeakDetector As LeakDetector
        Get
            Return _leakDetector
        End Get
        Private Set(ByVal value As LeakDetector)
            _leakDetector = value
        End Set
    End Property

    Private Sub Initialize()
        ' This call is required by the designer.
        InitializeComponent()

        _workerThread = New BackgroundWorker()
        _workerThread.WorkerReportsProgress = True
        _workerThread.WorkerSupportsCancellation = True

        ' Add any initialization after the InitializeComponent() call.
        minThresholdTextBox.Text = "85"
    End Sub

    'Public Sub ShowLeaksForHeap(ByVal heapID As String)
    '    _heapID = heapID
    '    _allocs = Nothing
    '    Me.Show()
    'End Sub

    Public Sub SearchForLeaksInAllocs(ByVal allocs As IEnumerable(Of HeapAllocationContainer))
        '_heapID = String.Empty
        _allocs = allocs
        Me.Show()
    End Sub

    Private Sub Okay_Click(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles Okay.Click
        'if worker is busy and this button was clicked, it means the user canceled the search, so call cancelasync.
        If _workerThread.IsBusy And CStr(Okay.Content) = "Cancel" Then
            _workerThread.CancelAsync()
            Return
        ElseIf _workerThread.IsBusy Then
            MsgBox("Cannot execute two searches at once.  Please cancel currently executing search...")
            Return
        End If

        If Not traditionalLeakCheckBox.IsChecked Then
            Dim tempThreshold As Integer
            If Not Integer.TryParse(minThresholdTextBox.Text, tempThreshold) Then
                MsgBox("Minimum match threshold must be between 0 and 100")
                Return
            End If

            If tempThreshold <= 0 Or tempThreshold > 100 Then
                MsgBox("Minimum match threshold must be between 0 and 100")
                Return
            End If
            _minMatchThresholdPct = tempThreshold
        End If

        Dim filters As String = MultiFilterTextBox.Text

        Dim filterRanges As List(Of Tuple(Of Int32, Int32)) = New List(Of Tuple(Of Int32, Int32))()

        Dim reader As New StringReader(filters)
        Dim line As String = reader.ReadLine()
        While Not String.IsNullOrEmpty(line)
            Dim range As Tuple(Of Int32, Int32) = Nothing
            If TryParseLine(line, range) Then
                filterRanges.Add(range)
            End If
            line = reader.ReadLine()
        End While
        If filterRanges.Count <= 1 Then
            MsgBox("Please enter at least two sequence number ranges.")
            Return
        End If

        Dim leakDetector As New LeakDetector()

        'validate ranges
        If Not leakDetector.ValidateLeakRanges(filterRanges) Then
            'ValidateLeakRanges() will log detailed information for invalid ranges
            'Common.MemspectLog.LogWarning("Invalid ranges set")
            Return
        End If

        'If Not String.IsNullOrEmpty(_heapID) Then
        '    Dim heapTarget As CSpyHeap = (From heap In Common._HeapList
        '                              Where heap.HeapName = _heapID
        '                              Select heap).First()
        '    _allocs = heapTarget.TakeMemSnapshot(False).Allocs
        'End If

        statusBar.Content = "Starting to find leaks..."

        'start find leaks on worker thread
        Dim leakParams As New LeakDetector.LeakDetectorParameters()
        leakParams.FilterRanges = filterRanges
        leakParams.Allocs = _allocs
        leakParams.MatchThreshold = CDbl(_minMatchThresholdPct / 100)
        leakParams.FindTraditionalLeaksOnly = CBool(traditionalLeakCheckBox.IsChecked)

        Okay.Content = "Cancel"
        _workerThread.RunWorkerAsync(leakParams)
    End Sub

    Private Sub worker_DoWork(ByVal sender As Object, ByVal e As DoWorkEventArgs) Handles _workerThread.DoWork
        Dim leakDetector As New LeakDetector()

        leakDetector.FindLeaksAsync(CType(e.Argument, LeakDetector.LeakDetectorParameters),
                                    CType(sender, BackgroundWorker),
                                    e)

        e.Result = leakDetector
    End Sub

    Private Sub worker_ProgressChanged(ByVal sender As Object, ByVal e As ProgressChangedEventArgs) Handles _workerThread.ProgressChanged
        statusBar.Content = e.UserState
    End Sub

    Private Sub worker_RunWorkerCompleted(ByVal sender As Object, ByVal e As RunWorkerCompletedEventArgs) Handles _workerThread.RunWorkerCompleted
        If e.Error IsNot Nothing Then
            UpdateStatusMsg(e.Error.Message, msgType:=StatusMessageType.AlertMsgBox)
        ElseIf e.Cancelled Then
            UpdateStatusMsg("Find leaks canceled.")
            statusBar.Content = "Canceled"
        Else
            _leakDetector = CType(e.Result, LeakDetector)

            Dim summary = ShowLeakSummary()

            Dim win As CommonUI.DataSurface = Me.ShowLeaks(_leakDetector.Leaks)
            If win Is Nothing Then
                statusBar.Content = "No leaks found"
            Else
                win.SetTabHeaderText(win.GetTabHeaderText().Replace("Snap", "LeakedSnaps"))
                statusBar.Content = ""
                CommonUI.DataWindowMain._TabControl.SelectedItem = summary.TopSurface
                Me.Hide()
            End If
        End If

        Okay.Content = "Okay"
    End Sub

    Private Function ShowLeaks(ByVal leaks As IEnumerable(Of HeapAllocationContainer)) As CommonUI.DataSurface
        If leaks Is Nothing Then
            UpdateStatusMsg("Leak collection is Nothing when calling ShowLeaks()", msgType:=StatusMessageType.AlertMsgBox)
            Return Nothing
        End If

        Dim heapCount As Integer = (From leak In leaks
                                   Select leak.SpyHeapPtr).Distinct().Count()

        Dim heapTarget = New CSpyHeap()
        If heapCount = 1 Then
            heapTarget = leaks(0).SpyHeapPtr
        End If

        Dim leakSnapShot = MemSnapshot.CreateMemSnapshotFromListAllocs(heapTarget, leaks.ToList, fEnableFilter:=False)

        Dim dataSurface = CommonUI.ShowSnapshot(heapTarget, leakSnapShot, "Leaks", Nothing)
        CommonUI.ShowGroupIDForDataSurface(dataSurface, _leakDetector.LeakDetectorID)

        Return dataSurface
    End Function

    Private Function TryParseLine(ByVal line As String, ByRef range As Tuple(Of Int32, Int32)) As Boolean
        If String.IsNullOrEmpty(line) Then
            Return False
        End If

        Dim splitIndex As Integer = line.IndexOf(",")
        If splitIndex <= 0 Then
            Return False
        End If

        Dim nRangeStart As Int32 = -1
        Dim nRangeEnd As Int32 = -1

        If Not Int32.TryParse(line.Substring(0, splitIndex).Trim(), nRangeStart) Then
            Return False
        End If

        If Not Int32.TryParse(line.Substring(splitIndex + 1).Trim(), nRangeEnd) Then
            Return False
        End If

        range = New Tuple(Of Int32, Int32)(nRangeStart, nRangeEnd)

        Return True
    End Function

    Private Sub reqExactMatchCheckBox_Checked(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles traditionalLeakCheckBox.Checked, traditionalLeakCheckBox.Unchecked
        If traditionalLeakCheckBox.IsChecked Then
            minThresholdTextBox.IsEnabled = False
            _minMatchThresholdPct = 100
        Else
            minThresholdTextBox.IsEnabled = True
        End If
    End Sub

    Private Function ShowLeakSummary() As CommonUI.DataSurface
        Dim ctrls = DataWindowMain.MakeNewDatasurface("LeakSummary", "", nMaxHeaderHeight:=40)
        'Dim sp As New StackPanel With {.Orientation = Orientation.Vertical}
        'ctrls.SurfaceDetails.Children.Add(sp)

        'Dim leaksBySize = From group In _leakDetector.LeakGroups
        '                      Order By group.LeakSize Descending

        'Dim summary As New System.Text.StringBuilder()
        'For Each group In leaksBySize
        '    summary.AppendLine(String.Format("Leak {0} has a size of {1} from {2} objects.", group.LeakID, group.LeakSize, group.LeakGroup.Count()))
        'Next

        Dim totalLeakSize = (From leakGroup In _leakDetector.LeakGroups
                             Select leakGroup.LeakSize).Sum()

        Dim totalLeakCount = (From leakGroup In _leakDetector.LeakGroups
                              Select leakGroup.LeakGroup.Count()).Sum()

        Dim totalLeakSummary As String = String.Format("Total Leak Count: {0}   Total Leak Size: {1}    Total Leaked Objects: {2}",
                                                       _leakDetector.LeakGroups.Count,
                                                       totalLeakSize,
                                                       totalLeakCount)

        ctrls.SurfaceHeader.Children.Add(New TextBlock() With {.Text = totalLeakSummary})

        Dim summaryItems = From group In _leakDetector.LeakGroups
                           Select
                               group.LeakID,
                               group.LeakSize,
                               AllocationCount = group.LeakGroup.Count()
                           Order By LeakSize Descending

        ctrls.SurfaceDetails.Children.Add(New LeakSummaryControl(_leakDetector))
        'sp.Children.Add(New LeakSummaryControl(_leakDetector.LeakGroups))

        Return ctrls
    End Function


End Class