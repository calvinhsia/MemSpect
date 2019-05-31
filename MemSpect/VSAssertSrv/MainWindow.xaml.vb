Imports System.Runtime.InteropServices
Imports System.Windows.Threading
Imports System.Threading
Imports MemSpect

Class MainWindow

    Private WithEvents _timer As New DispatcherTimer With {.Interval = TimeSpan.FromSeconds(2)}
    Public _spHeaps As New StackPanel With {.Orientation = Orientation.Vertical}
    Private _vbDiag As VBDiag
    Sub New()
        Try
            InitializeComponent()
            Dim pathCurExeAsm = Reflection.Assembly.GetExecutingAssembly.Location ' "\\calvinh6\public\test\MemSpect.exe"
            If pathCurExeAsm.StartsWith("\\") Then
                MessageBox.Show("Can't run MemSpect from a network share " + pathCurExeAsm, "MemSpect " + _MemSpectUICurrentVersion())
                Environment.Exit(1)
            End If
            Dim pathMemspect = Reflection.Assembly.GetEntryAssembly().Location
            Try
                IO.Directory.SetCurrentDirectory(IO.Path.GetDirectoryName(pathMemspect)) ' set path so we can find components (like codemarkers)
            Catch ex As Exception
                'eat exceptions: could be immersive?
            End Try

            ' common cause of failure here: didn't deploy Fastserialization.dll
            'MessageBox.Show("VSAssert child process. attach a debugger. Pid =  " + Diagnostics.Process.GetCurrentProcess.Id.ToString)
            _IsClientOutOfProcess = True ' indicate out of proc

            _windowMain = Me
            _vbDiag = New VBDiag(Me)
            Dim args = System.Environment.GetCommandLineArgs
            Dim connResult = CommonUI.InitCommWithUI(args)
            If Not String.IsNullOrEmpty(connResult) Then
                Dim oErrWin = New Window() With {.Title = "MemSpect " + _MemSpectUICurrentVersion()}
                Dim sp = New StackPanel With {.Orientation = Orientation.Vertical}
                oErrWin.Content = sp
                Dim tb = New TextBox With {.Text = connResult}
                Dim btnClose = New Button With {.Content = "_OK", .Width = 100, .HorizontalAlignment = Windows.HorizontalAlignment.Left}
                AddHandler btnClose.Click, Sub()
                                               End ' end the prog
                                           End Sub
                sp.Children.Add(tb)
                sp.Children.Add(btnClose)
                oErrWin.ShowDialog()
                'MessageBox.Show(connResult, "MemSpect " + _MemSpectUICurrentVersion())
                End ' end the prog
            End If


            Me.Left = My.Settings.MainWindowLoc.X
            Me.Top = My.Settings.MainWindowLoc.Y
            Me.Width = My.Settings.MainWindowSize.Width
            Me.Height = My.Settings.MainWindowSize.Height
            'Me.RenderTransform = New ScaleTransform(1, 1)
            'AddHandler Me.MouseWheel, _
            '       Sub(sender As Object, e As MouseWheelEventArgs)
            '           Dim tr = CType(Me.RenderTransform, ScaleTransform)
            '           If e.Delta > 0 Then
            '               tr.ScaleX *= 1.1
            '               tr.ScaleY *= 1.1
            '           Else
            '               tr.ScaleX /= 1.1
            '               tr.ScaleY /= 1.1
            '           End If
            '           e.Handled = True
            '       End Sub


        Catch ex As Exception
            CommonUI.MemSpectExceptionHandler(ex)
        End Try
    End Sub



    Public Class VBDiag
        Inherits VBDiagMarginBase
        Sub New(ByVal windOwner As Window)
            MyBase.New()
            _windOwner = windOwner
            _VBDiagMarginBase = Me
        End Sub

    End Class

    Public _MutexSingleton As Mutex
    Private Sub Window_Loaded(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles MyBase.Loaded
        Try
            Dim mutexName = Reflection.Assembly.GetExecutingAssembly.GetName.Name
            Dim fCreatedNewMainWindow = False
            _MutexSingleton = New Mutex(initiallyOwned:=True, Name:=mutexName, createdNew:=fCreatedNewMainWindow)
            '"c:\Users\calvinh\AppData\Local\Microsoft_Corp\MemSpect.exe_Url_1ybtwiu2ed4n0yytnejwte3yyaqircvc\1.0.0.0\user.config"
            '            _spHeaps.Children.Add(_txtStatus)

            If _IsLoadingOfflineSnapshot Then
                AddHandler OfflineMegaSnapshotUI.LoadMegaSnapshotCompletedEvent,
                    Sub()
                        _vbDiag.ResetUi(chkClrObjVal:=Nothing, chkFreezeVal:=Nothing)
                        UpdateStatusMsg("SnapLoad done")
                        _VBDiagMarginBase._FilterUI.RefreshFilterTextbox()
                        If _GlobalFilter.SeqNoLo <> 0 Then
                            UpdateStatusMsg("SetLo = " + _GlobalFilter.SeqNoLo.ToString)
                            UpdateStatusMsg("SetHi = " + _GlobalFilter.SeqNoHi.ToString)
                        End If
                        Me.Content = _vbDiag
                    End Sub
                'While _IsLoadingOfflineSnapshot
                '    WaitForOtherThreads(1000)
                'End While
            End If
            If _ConnectionMode = MemSpectMode.Offline OrElse _ConnectionMode = MemSpectMode.MiniDumpOnly OrElse _ConnectionMode = MemSpectMode.Existing Then
                _iniFileName = IO.Path.Combine(IO.Path.GetDirectoryName(Reflection.Assembly.GetEntryAssembly.Location), MemSpectIniName)
            End If

            If Not fCreatedNewMainWindow Then
                Top += 50
                Left += 50
            End If
            _vbDiag.ResetUi(chkClrObjVal:=Nothing, chkFreezeVal:=Nothing)

            If _ConnectionMode = MemSpectMode.Existing OrElse _ConnectionMode = MemSpectMode.MiniDumpOnly Then
                _vbDiag._btnVirtMem.RaiseEvent(New RoutedEventArgs With {.RoutedEvent = Button.ClickEvent})
            End If
            Me.Content = _vbDiag
            Me.Title = GetWindowTitle()
            If _ConnectionMode = MemSpectMode.OnLine Then
                AddHandler TrackingModeChanged, Sub(newMode As TrackingModeEnum)
                                                    Dim seqno = GetGlobalPassCount
                                                    Select Case newMode
                                                        Case TrackingModeEnum.Minimal
                                                            UpdateStatusMsg("TrackingMode Normal->Minimal Seqno=" + seqno.ToString)
                                                        Case TrackingModeEnum.Normal
                                                            UpdateStatusMsg("TrackingMode Minimal->Normal Seqno=" + seqno.ToString)
                                                    End Select
                                                End Sub
                _timer.Start()
            End If
            _delParentDone = New ParentDone(AddressOf DoForceClose)

            GhostEnableStartTrackingGhost()

        Catch ex As Exception
            CommonUI.MemSpectExceptionHandler(ex)
            Me.Close() ' might as well shut down the app for this kind of exception
        End Try
    End Sub

    Private _IsClosed As Boolean = False
    Sub OnTimerTick() Handles _timer.Tick
        Try
            If _pipestreamToTarget Is Nothing OrElse Not _pipestreamToTarget.IsConnected Then
                DoForceClose()
                Return
            End If
            '            SendMsg(ProcMsgVerb.GetHeapStats)
            '            Dim ptr = New IntPtr(_SharedMemAddr)
            '            Dim numHeaps = Marshal.ReadInt32(ptr)
            '            ptr += 4
            '            _ulGlobalPassCount = Marshal.ReadInt32(ptr)
            '            UpdateStatusMsg("NumHeaps = " + numHeaps.ToString + " passcount=" + _ulGlobalPassCount.ToString)
            '            ptr += 4
            '            _debugHeaps.Clear()
            '            _vbDiag.RefreshDebugHeapList()
            '            _spHeaps.Children.Clear()
            '            Dim q = From a In _debugHeaps
            '                    Select Heap = a.HeapName,
            '                    CurSize = a.CurTotBytes.ToString("n0"),
            '                    nLive = a.CurNumAllocs.ToString("n0"),
            '                    TotAlloc = a.TotNumAllocs.ToString("n0"),
            '                    _Heap = a
            '            Dim bord = New Border With {.BorderBrush = Brushes.Azure, .BorderThickness = New Windows.Thickness(4)}

            '            Dim brow = New Browse(q,
            '                                  delDblClick:=AddressOf OnHeapDblClick,
            '                                  ColWidths:={190, 70, 60, 60},
            '                                  fAllowHeaderClickSort:=True) With {
            '                                    .ToolTip =
            '<xml>
            'DblClick a native heap to see native memory use. 
            'You can do this multiple times to create multiple snapshots
            '</xml>.Value
            '                                }

            '            bord.Child = brow
            '            _spHeaps.Children.Add(bord)
            '            _spHeaps.Children.Add(_txtStatus)


        Catch ex As IO.IOException ' a named pipe IO op failed
            DoForceClose()
        Catch ex As ArgumentException
            If Not ex.Message.StartsWith("Process with an Id") Then
                CommonUI.MemSpectExceptionHandler(ex)
                DoForceClose()
            End If
        Catch ex As Exception
            If Not ex.Message.Contains("has exited.") Then
                CommonUI.MemSpectExceptionHandler(ex)
            End If
            DoForceClose()
        End Try
    End Sub

    Sub DoForceClose()
        SaveSettings()
        VBDiagMarginBase._timerAutoGc = Nothing
        If Not _IsClosed Then
            _IsClosed = True
            _timer = Nothing
            ProcComm.CloseComm()
            UpdateStatusMsg("Disconnected")
            Me.Close()
        End If
    End Sub

    Private _fDidSaveSettings As Boolean
    Sub SaveSettings()
        If Not _fDidSaveSettings Then
            _fDidSaveSettings = True
            My.Settings.MainWindowLoc = New System.Drawing.Point(CInt(Me.Left), CInt(Me.Top))
            My.Settings.MainWindowSize = New System.Drawing.Size(CInt(Me.Width), CInt(Me.Height))
            My.Settings.Save()
        End If
    End Sub

    Sub On_Closed() Handles Me.Closed
        MemoryEater.FreeAll()  ' free any test allocations
        _timer = Nothing
        _IsShuttingDown = True
        'If VBDiagMarginBase._instanceList(0)._chkClrObjects.IsChecked Then
        '    VBDiagMarginBase._instanceList(0)._chkClrObjects.IsChecked = False ' we don't need to track CLR objects any more
        'End If
        Try
            _VBDiagMarginBase._chkFreeze.IsChecked = False ' when turning off the lights, thaw parent 
            DataWindowMain.CloseWindows()
            If Not _IsClosed Then
                Try
                    If _ConnectionMode = MemSpectMode.OnLine Then
                        SendMsg(ProcMsgVerb.Quit, fSendEndMsgSync:=False, dwords:={0}) ' quit but leave parent running
                    End If
                Catch ex As Exception

                End Try
            End If
            SaveSettings()
            If _ConnectionMode <> MemSpectMode.Offline Then
                ProcComm.CloseComm()
            End If

        Catch ex As Exception

        End Try
        End ' end the process
    End Sub
End Class

