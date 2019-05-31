Imports MemSpect
Imports System.Runtime.InteropServices
Imports System.Linq

Public Class LauncherUi

    Public _procidImmersive As Integer

    Private Sub LauncherUi_Initialized(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Initialized
        Me.Left = My.Settings.LauncherWindowLoc.X
        Me.Top = My.Settings.LauncherWindowLoc.Y
    End Sub

    Public ReadOnly Property IsImmersive As Boolean
        Get
            Dim fres = False
            Dim targ = cboFile.Text
            If IO.File.Exists(targ) Then
                If IO.Path.GetExtension(targ).ToLower.EndsWith("xml") Then
                    Return True
                End If
            End If
            Return fres
        End Get
    End Property


    Private Sub OnLoad() Handles MyBase.Loaded

        Me.chkDisablePerfWatson.ToolTip =
<xml>As a convenenience, this will ensure Visual Studio PerfWatson is disabled via registry (see INI file)
The ChkBox is enabled for all ONLY because tooltips don't show for disabled controls
You can also do this manually:

reg add HKEY_CURRENT_USER\Software\Microsoft\VisualStudio\11.0\General /v MaximumResponsivenessDelay /t REG_DWORD /d 1600000000
Works for devenv.exe==> HKEY_CURRENT_USER\Software\Microsoft\VisualStudio\11.0\General
and foobar.exe ==> HKEY_CURRENT_USER\Software\Microsoft\foobar\11.0\General   (where foobar is like VSWinExpress)
If not disabled, PerfWatson will detect something on the UI thread went > 2 seconds and then it will suspend threads and try to allocate memory. 
If a suspended thread owns a critical section, then deadlock results.
</xml>.Value


        If Not String.IsNullOrEmpty(My.Settings.LauncherBtn) Then
            Select Case CInt(My.Settings.LauncherBtn)
                Case 1
                    Me.rbtnLaunch.IsChecked = True
                Case 2
                    Me.rbtnInject.IsChecked = True
                Case 3
                    Me.rbtnAttach.IsChecked = True
                Case 4
                    Me.rbtnDump.IsChecked = True
            End Select
        End If
        ' read ini file for settings. _iniFileName is not init'd yet: it's obtained from the target process
        Dim inifile = IO.Path.Combine(IO.Path.GetDirectoryName(System.Reflection.Assembly.GetEntryAssembly.Location), MemSpectIniName)
        Dim initTrackClrObjects = GetPrivateProfileInt(ProfileStringSection, "TrackClrObjects", 0, inifile)
        Me.chkTrackClrObjects.IsChecked = initTrackClrObjects > 0
        Dim lblsp = GetMemSpectHyperLink(fontSize:=10)
        lblsp.Children.Add(New TextBlock With {.Text = " Admin= " + IsRunningAsAdmin.ToString})
        Me.lblVersion.Content = lblsp

        Dim filenameDevEnv = ProcessLauncher.GetDevEnvFullPath
        Me.cboFile.Items.Clear()
        If My.Settings.LauncherMRU Is Nothing OrElse My.Settings.LauncherMRU.Count = 0 Then
            Me.cboFile.Items.Add(New ComboBoxItem With {.Content = filenameDevEnv})
        Else
            Dim fGotDevenv = (From strMru In My.Settings.LauncherMRU
                             Where String.Equals(filenameDevEnv, CStr(strMru), StringComparison.OrdinalIgnoreCase)).Any()

            If Not fGotDevenv Then
                Me.cboFile.Items.Add(New ComboBoxItem With {.Content = filenameDevEnv})
            End If
            For Each itm In My.Settings.LauncherMRU
                Me.cboFile.Items.Add(New ComboBoxItem With {.Content = itm})
            Next
        End If
        Me.cboArgs.Items.Clear()
        If My.Settings.LauncherArgs IsNot Nothing AndAlso My.Settings.LauncherArgs.Count > 0 Then
            For Each itm In My.Settings.LauncherArgs
                Me.cboArgs.Items.Add(New ComboBoxItem With {.Content = itm})
            Next
            Me.cboArgs.Text = My.Settings.LauncherArgs(0)
        End If

        Me.cboFile.SelectedIndex = 0
        Me.cboFile.Focus()

        Dim lamHandler = Sub(sender As Object, e As StatusMessageEventArgs)
                             txtLaunchStatus.Text = e.Message
                         End Sub

        AddHandler StatusMessageEvent, lamHandler
        AddHandler Me.Closed, Sub()
                                  RemoveHandler StatusMessageEvent, lamHandler
                              End Sub

    End Sub

    Private Sub OnClose() Handles Me.Closed
        My.Settings.LauncherWindowLoc = New System.Drawing.Point(CInt(Me.Left), CInt(Me.Top))
        My.Settings.Save()
    End Sub

    Private Sub btnStart_Click(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles btnStart.Click
        Dim bintype = 0

        Me.DialogResult = True
        If Me.rbtnLaunch.IsChecked Then
            If Not IO.File.Exists(Me.cboFile.Text) Then
                MsgBox("File not found """ + Me.cboFile.Text + """")
                Return
            End If
            If GetBinaryType(Me.cboFile.Text, bintype) AndAlso bintype <> BinaryType.SCS_32BIT_BINARY Then
                ' COMIMAGE_FLAGS_32BITREQUIRED
                MsgBox(Me.cboFile.Text + " not a 32 bit binary")
            End If
            My.Settings.LauncherBtn = "1"
        Else
            My.Settings.LauncherBtn = "1" ' always 1
        End If
        Dim col As New Specialized.StringCollection
        If Me.cboFile.Items.Count > 0 AndAlso Me.cboFile.SelectedIndex >= 0 Then
            Me.cboFile.Text = CStr(CType(Me.cboFile.Items(Me.cboFile.SelectedIndex), ComboBoxItem).Content) ' so caller can read text
        Else
            Me.cboFile.Text = ProcessLauncher.GetDevEnvFullPath
        End If
        col.Add(Me.cboFile.Text)

        For Each itm As ComboBoxItem In Me.cboFile.Items
            If CStr(itm.Content) <> Me.cboFile.Text Then
                col.Add(CStr(itm.Content))
            End If
        Next
        My.Settings.LauncherMRU = col

        col = New Specialized.StringCollection

        Me.cboArgs.Items.Insert(0, New ComboBoxItem With {.Content = Me.cboArgs.Text})

        For Each itm As ComboBoxItem In Me.cboArgs.Items
            If Not col.Contains(CStr(itm.Content)) Then
                col.Add(CStr(itm.Content))
            End If
        Next

        My.Settings.LauncherArgs = col

        My.Settings.Save()
        Me.Close()
    End Sub

    Private Sub cboFile_PreviewKeyDown(ByVal sender As Object, ByVal e As KeyEventArgs) Handles cboFile.PreviewKeyDown, cboArgs.PreviewKeyDown
        Dim cbo = CType(sender, ComboBox)
        If cbo.IsDropDownOpen Then
            If e.Key = Key.Delete Then
                If cbo.Items.Count > 0 AndAlso cbo.SelectedIndex >= 0 Then
                    cbo.Items.RemoveAt(cbo.SelectedIndex)
                    If cbo.Items.Count > 0 Then
                        cbo.SelectedIndex = 0
                    End If
                End If
            End If
        End If
    End Sub
    ' Given an EXE name like "devenv.exe", get the matching reg name, like "VisualStudio"
    'targfullpath =  C:\Program Files\Microsoft Visual Studio 10.0\Common7\IDE\devenv.exe
    Public Function GetPerfWatsonkey(ByVal targfullpath As String) As String
        Dim regKey = String.Empty
        ' 5:12:07.1319934 PM	devenv.exe	16708	RegQueryValue	HKCU\Software\Microsoft\VisualStudio\12.0\General\MaximumResponsivenessDelay	SUCCESS	Type: REG_DWORD, Length: 4, Data: 1600000000
        'reg add c /v MaximumResponsivenessDelay /t REG_DWORD /d 1600000000
        Dim fname = IO.Path.GetFileNameWithoutExtension(targfullpath).ToLower
        Dim regInner = String.Empty
        Select Case fname
            Case "devenv"
                If Not targfullpath.Contains("10") Then
                    regInner = "VisualStudio"
                End If
            Case Else ' like "VSWinExpress"
                regInner = fname
        End Select
        ' for VS 10.0, perfwatson didn't exist
        Dim ver = "12.0"
        Dim strVS = "\Microsoft Visual Studio "
        Dim pos = targfullpath.IndexOf(strVS)
        If pos > 0 Then
            ver = targfullpath.Substring(pos + strVS.Length, 4)
            'HKEY_CURRENT_USER\Software\Microsoft\VisualStudio\10.0\General
            Dim regkTry = String.Format("HKEY_CURRENT_USER\Software\Microsoft\{0}\{1}\General", regInner, ver)

            Dim val = Microsoft.Win32.Registry.GetValue(regkTry, "AllowDeferredSaveProjects", 33)
            If val IsNot Nothing Then
                regKey = regkTry
            End If
        End If
        Return regKey

    End Function

    Private Sub cboFile_() Handles cboFile.SelectionChanged
        'C:\Program Files (x86)\Microsoft Visual Studio 11.0\Common7\IDE\devenv.exe
        _procidImmersive = 0
        _ImmersivePackageInfo = Nothing
        Me.tbxImmersiveInfo.Text = String.Empty

        If Me.cboFile.SelectedItem IsNot Nothing AndAlso
                    Not String.IsNullOrEmpty(GetPerfWatsonkey(CType(Me.cboFile.SelectedItem, ComboBoxItem).Content.ToString)) Then
            Me.chkDisablePerfWatson.IsChecked = True
        Else
            Me.chkDisablePerfWatson.IsChecked = False
        End If
    End Sub

    Private Sub btnCancel_Click(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles btnCancel.Click
        Me.DialogResult = False
        Me.Close()
    End Sub

    Private Sub btnHelp_Click(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles btnHelp.Click
        MsgBox(GetHelpText())
    End Sub

    Private Sub btnBrowse_Click(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles btnBrowse.Click
        Dim dlg = New System.Windows.Forms.OpenFileDialog
        dlg.InitialDirectory = IO.Path.GetDirectoryName(Reflection.Assembly.GetExecutingAssembly.Location)
        dlg.Title = "Choose a process to launch or '" + DumpedFilesMHDFileName + "' in a MemSpect snapshot to load it."
        'dlg.Filter = "Executable|*.exe|Manifest(Immersive)|*.xml|dump or snap (*.*dmp,DumpedFiles.mhd)|*.mdmp;*.hdmp;*.?dmp;DumpedFiles.mhd|All Files | *.*"
        dlg.Filter = "Executable or Manifest or Dump or Snap|*.exe;*.xml;*.dmp;" + DumpedFilesMHDFileName + "|All Files | *.*"
        If dlg.ShowDialog = Forms.DialogResult.OK AndAlso IO.File.Exists(dlg.FileName) Then
            Dim arr As New List(Of String)
            For Each itm As ComboBoxItem In cboFile.Items
                arr.Add(CStr(itm.Content))
            Next

            Me.cboFile.Items.Clear()
            Me.cboFile.Items.Add(New ComboBoxItem With {.Content = dlg.FileName})
            For Each itm In arr
                If CStr(itm) <> dlg.FileName Then
                    Me.cboFile.Items.Add(New ComboBoxItem With {.Content = itm})
                End If
            Next
            Me.cboFile.SelectedIndex = 0
        End If
    End Sub

    Private Sub btnSettings_Click(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles btnSettings.Click
        Dim pathCurExeAsm = Reflection.Assembly.GetExecutingAssembly.Location ' "\\calvinh6\public\test\MemSpect.exe"
        Dim inifile = IO.Path.ChangeExtension(pathCurExeAsm, "ini")
        Process.Start(inifile)
    End Sub


    Public Function EnsureImmersive() As Boolean
        Dim fretVal = False
        If IsImmersive Then
            _ImmersivePackageInfo = New ImmersivePackageInfoClass(cboFile.Text) ' a new one every time 
            tbxImmersiveInfo.Text = String.Format("FullName {0}" + vbCrLf +
                                                  "AppUserModelId {1}" + vbCrLf +
                                                  "Name {2}" + vbCrLf +
                                                  "Fam Name {3}" + vbCrLf +
                                                  "AppContainer Name {4}" + vbCrLf,
                                                  _ImmersivePackageInfo.strPackageFullName,
                                                  _ImmersivePackageInfo.strAppUserModelId,
                                                  _ImmersivePackageInfo.strPackageName,
                                                  _ImmersivePackageInfo.strPackageFamilyName,
                                                  _ImmersivePackageInfo.strpAppContainerNamedObjectPath)
            fretVal = True
        Else
            UpdateStatusMsg("Program must be valid immersive manifest xml file")
        End If
        Return fretVal
    End Function

    Private Sub btnImmersiveLaunch_Click(sender As Object, e As RoutedEventArgs) Handles btnImmersiveLaunch.Click
        If EnsureImmersive() Then
            Dim hr = PackageLauncher(_ImmersivePackageInfo.strAppUserModelId, _procidImmersive)
            If hr <> 0 Then
                UpdateStatusMsg(_ImmersivePackageInfo.strPackageFullName + vbCrLf + GetErrorMessageFromHResult(hr))
            Else
                UpdateStatusMsg("Launch PID= " + _procidImmersive.ToString)
            End If
        End If
    End Sub

    Private Sub btnImmersiveTerm_Click(sender As Object, e As RoutedEventArgs) Handles btnImmersiveTerm.Click
        If _procidImmersive <> 0 Then
            Dim proc = Process.GetProcessById(_procidImmersive)
            If proc Is Nothing Then
                UpdateStatusMsg("Process " + _procidImmersive.ToString + " not found")
            Else
                proc.Kill()
                UpdateStatusMsg("Process Kill invoked " + _procidImmersive.ToString)
            End If
            _procidImmersive = 0
        Else
            UpdateStatusMsg("no MemSpect launched process found")
        End If
    End Sub


    Private Sub btnEnableDebug_Click(sender As Object, e As RoutedEventArgs) Handles btnEnableDebug.Click
        If EnsureImmersive() Then
            Dim hr = ProcessLauncher.DeployMemSpectAndEnableDebug(_ImmersivePackageInfo.strManifestPath)
            If hr <> 0 Then
                UpdateStatusMsg(_ImmersivePackageInfo.strPackageFullName + vbCrLf + GetErrorMessageFromHResult(hr))
            Else
                UpdateStatusMsg("Enabled debug for " + _ImmersivePackageInfo.strPackageFullName)
            End If
        End If
    End Sub

    Private Sub btnDisableDebug_Click(sender As Object, e As RoutedEventArgs) Handles btnDisableDebug.Click
        If EnsureImmersive() Then
            Dim hr = PackageDebuggingEnabler(_ImmersivePackageInfo.strPackageFullName, "", "", nEnable:=0)
            If hr <> 0 Then
                UpdateStatusMsg(_ImmersivePackageInfo.strPackageFullName + vbCrLf + GetErrorMessageFromHResult(hr))
            Else
                UpdateStatusMsg("Disabled debug for " + _ImmersivePackageInfo.strPackageFullName)
            End If
        End If

    End Sub

    Private Sub btnSuspend_Click(sender As Object, e As RoutedEventArgs) Handles btnSuspend.Click
        If EnsureImmersive() Then
            Dim hr = PackageResumeSuspend(_ImmersivePackageInfo.strPackageFullName, nResume:=0)
            If hr <> 0 Then
                UpdateStatusMsg(_ImmersivePackageInfo.strPackageFullName + vbCrLf + GetErrorMessageFromHResult(hr))
            Else
                UpdateStatusMsg("Package suspend for " + _ImmersivePackageInfo.strPackageFullName)
            End If
        End If
    End Sub

    Private Sub btnResume_Click(sender As Object, e As RoutedEventArgs) Handles btnResume.Click
        If EnsureImmersive() Then
            Dim hr = PackageResumeSuspend(_ImmersivePackageInfo.strPackageFullName, nResume:=1)
            If hr <> 0 Then
                UpdateStatusMsg(_ImmersivePackageInfo.strPackageFullName + vbCrLf + GetErrorMessageFromHResult(hr))
            Else
                UpdateStatusMsg("Package resume")
            End If
        End If
    End Sub

End Class
