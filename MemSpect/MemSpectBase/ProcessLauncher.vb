Namespace MemSpect
    Public Class ProcessLauncher

        Private _WriteIniFile As WriteIniFile

        Property StartChildProcess As Integer = 0
        Property fReportLeaks As Integer = 0
        Property NativeOnly As Integer = 0
        Property TrackingMode As Integer = 1 ' default to Maximal tracking for testing
        Property TrackClrObjects As Integer = 0 '  setting in INI file, but also can be dyna changed
        Property EatLowMem As Integer = 0
        Property fHandle4gigStacks As Integer = 0
        Property RetailHeaderTrailerSize As Integer = 0
        Property nSharedMemSize As Integer = 65536
        Property NumberOfStackFrames As Integer = 50
        Property FreezeAtStartup As Integer = 0
        Property SymbolsPath As String = "SRV*c:\symcache*\\ddrps\symbols*http://symweb"
        Property ShowSymbolLoadStatus As Integer = 0
        Property StackStorageMode As Integer = 0

        Property TrackFileLoadUnload As Integer = 1

        Property ShowAddressInCallStacks As Integer = 0

        Property CheckGCLostObjects As Integer = 0
        Property CodeMarkersAtWhichToFreeze As String = String.Empty
        Property CodeMarkersToCollectStats As String = String.Empty
        Property CodeMarkersToSendStatusMsg As String = "perfIdle"
        Property CodeMarkersAtWhichToTakeSnapshot As String = String.Empty
        Property CodeMarkersAtWhichToCrash As String = String.Empty
        Property CodeMarkersAtWhichToHang As String = String.Empty
        Property CodeMarkersAtWhichToSleep As String = String.Empty
        Property CodeMarkersAtWhichToDebugBreak As String = String.Empty
        Property CodeMarkersAtWhichToRecur As String = String.Empty

        Property CodeMarkerParameter As Integer = 10000


        Property TrackCodeMarkers As Integer = 3 'Note to Prism and stress users: turn this off because it stores callstacks for events, which are never freed
        Property fMessageBoxOnStart As Integer = 0
        Property fTrackThreadCreate As Integer = 1 'Note to Prism and stress users: turn this off because it stores callstacks for events, which are never freed
        Property _VirtualAllocTopDown As Integer = 0
        Property _DynStackSymRes As Integer = 0
        Property _WhenDist As Integer = 1
        Property fTrackLoadResource As Integer = 1 'Note to Prism and stress users: turn this off because it stores callstacks for events, which are never freed
        Property fTrackGDI As Integer = 0
        Property TrackGCStacks As Integer = 1 'Note to Prism and stress users: turn this off because it stores callstacks for events, which are never freed
        Property TrackJIT As Integer = 1 'Note to Prism and stress users: turn this off because it stores callstacks for events, which are never freed
        Property TrackExcpt As Integer = 1 'Note to Prism and stress users: turn this off because it stores callstacks for events, which are never freed

        Property TrackETW As Integer = 0 'Note to Prism and stress users: turn this off because it stores callstacks for events, which are never freed

        Property TrackArenas As Integer = 1

        Property IncludeHeapReport As Integer = 1 ' 

        Property EnableAsserts As Integer = 1 ' turn this off for unattended mode


        Property _pidTargetProcess As Integer = 0
        Property _DidInitComm As Boolean = False
        Property _nmsecsToWaitTilStart As Integer = 1000
        Property _AdditionalCmdLineParams As String = ""
        Property IsImmersiveApp As Boolean = False
        Property ImmersiveProcessId As Integer = 0
        Property LauncherErrorMessage As String = String.Empty
        Private _targProcIsDebug As Boolean
        ReadOnly Property targProcIsDebug As Boolean
            Get
                Return _targProcIsDebug
            End Get
        End Property

        ''' <summary>
        '''  for devenv, will be "14.0" or "12.0". For Dev15, can be empty
        ''' </summary>
        Public Shared _VsVersionstring As String = String.Empty

        Public Enum WriteIniFile
            WriteNothing ' don't write anything at all
            ' WriteMinimal ' only TrackClrObjects
            WriteAnything
        End Enum
        Sub New(Optional ByVal WriteIniFile As WriteIniFile = ProcessLauncher.WriteIniFile.WriteAnything)
            _WriteIniFile = WriteIniFile ' for tests, we'll modify INI file. Else respect existing one.
        End Sub

        Public Sub WriteTheIniFile(ByVal inifile As String, ByVal procName As String, Optional writeAll As Boolean = False)
            If String.IsNullOrEmpty(inifile) Then
                Throw New ArgumentException("Inifile is null")
            End If
            If _WriteIniFile <> WriteIniFile.WriteNothing Then
                If NativeImports.WritePrivateProfileString(ProfileStringSection, "TrackClrObjects", TrackClrObjects.ToString, inifile) = 0 Then
                    Dim lerr = System.Runtime.InteropServices.Marshal.GetLastWin32Error
                    UpdateStatusMsg("Could not write to INI file " + inifile + " Are you running as admin? " +
                                    GetErrorMessageFromWin32LastError(lerr),
                                    msgType:=StatusMessageType.AlertMsgBox)
                    Environment.Exit(1)

                End If

                AddProcessToJustThisProcesses(procName, inifile)


            End If
            If _WriteIniFile = WriteIniFile.WriteAnything Then
                NativeImports.WritePrivateProfileString(ProfileStringSection, "StartChildProcess", StartChildProcess.ToString, inifile)
                NativeImports.WritePrivateProfileString(ProfileStringSection, "nSharedMemSize", nSharedMemSize.ToString, inifile)
                NativeImports.WritePrivateProfileString(ProfileStringSection, "fMessageBoxOnStart", fMessageBoxOnStart.ToString, inifile) ' this has to be not _IsUnderTest
                If _IsUnderTest OrElse writeAll Then ' be really careful about writing these: it will write default values
                    NativeImports.WritePrivateProfileString(ProfileStringSection, "NumberOfStackFrames", NumberOfStackFrames.ToString, inifile)

                    NativeImports.WritePrivateProfileString(ProfileStringSection, "DllsToAssertOn", "", inifile)

                    NativeImports.WritePrivateProfileString(ProfileStringSection, "TrackingMode", TrackingMode.ToString, inifile)

                    NativeImports.WritePrivateProfileString(ProfileStringSection, "EatLowMem ", EatLowMem.ToString, inifile)
                    NativeImports.WritePrivateProfileString(ProfileStringSection, "fHandle4gigStacks", fHandle4gigStacks.ToString, inifile)
                    NativeImports.WritePrivateProfileString(ProfileStringSection, "FreezeAtStartup", FreezeAtStartup.ToString, inifile)
                    NativeImports.WritePrivateProfileString(ProfileStringSection, "NativeOnly", NativeOnly.ToString, inifile)

                    NativeImports.WritePrivateProfileString(ProfileStringSection, "ShowAddressInCallStacks", ShowAddressInCallStacks.ToString, inifile)

                    NativeImports.WritePrivateProfileString(ProfileStringSection, "SymbolsPath", SymbolsPath, inifile)

                    NativeImports.WritePrivateProfileString(ProfileStringSection, "ShowSymbolLoadStatus", ShowSymbolLoadStatus.ToString, inifile)

                    NativeImports.WritePrivateProfileString(ProfileStringSection, "StackStorageMode", StackStorageMode.ToString, inifile)

                    NativeImports.WritePrivateProfileString(ProfileStringSection, "TrackFileLoadUnload", TrackFileLoadUnload.ToString, inifile)


                    NativeImports.WritePrivateProfileString(ProfileStringSection, "CodeMarkersAtWhichToFreeze", CodeMarkersAtWhichToFreeze, inifile)

                    NativeImports.WritePrivateProfileString(ProfileStringSection, "CodeMarkersToCollectStats", CodeMarkersToCollectStats, inifile)

                    NativeImports.WritePrivateProfileString(ProfileStringSection, "CodeMarkersToSendStatusMsg", CodeMarkersToSendStatusMsg, inifile)

                    NativeImports.WritePrivateProfileString(ProfileStringSection, "CodeMarkersAtWhichToTakeSnapshot", CodeMarkersAtWhichToTakeSnapshot, inifile)

                    NativeImports.WritePrivateProfileString(ProfileStringSection, "CodeMarkersAtWhichToCrash", CodeMarkersAtWhichToCrash, inifile)

                    NativeImports.WritePrivateProfileString(ProfileStringSection, "CodeMarkersAtWhichToHang", CodeMarkersAtWhichToHang, inifile)

                    NativeImports.WritePrivateProfileString(ProfileStringSection, "CodeMarkersAtWhichToSleep", CodeMarkersAtWhichToSleep, inifile)

                    NativeImports.WritePrivateProfileString(ProfileStringSection, "CodeMarkersAtWhichToDebugBreak", CodeMarkersAtWhichToDebugBreak, inifile)

                    NativeImports.WritePrivateProfileString(ProfileStringSection, "CodeMarkersAtWhichToRecur", CodeMarkersAtWhichToRecur, inifile)

                    NativeImports.WritePrivateProfileString(ProfileStringSection, "CodeMarkerParameter", CodeMarkerParameter.ToString, inifile)

                    NativeImports.WritePrivateProfileString(ProfileStringSection, "TrackCodeMarkers", TrackCodeMarkers.ToString, inifile)

                    NativeImports.WritePrivateProfileString(ProfileStringSection, "CheckGCLostObjects", CheckGCLostObjects.ToString, inifile)
                    NativeImports.WritePrivateProfileString(ProfileStringSection, "fTrackThreadCreate", fTrackThreadCreate.ToString, inifile)
                    NativeImports.WritePrivateProfileString(ProfileStringSection, "fTrackLoadResource", fTrackLoadResource.ToString, inifile)
                    NativeImports.WritePrivateProfileString(ProfileStringSection, "TrackGCStacks", TrackGCStacks.ToString, inifile)
                    NativeImports.WritePrivateProfileString(ProfileStringSection, "fTrackGDI", fTrackGDI.ToString, inifile)
                    NativeImports.WritePrivateProfileString(ProfileStringSection, "TrackJIT", TrackJIT.ToString, inifile)
                    NativeImports.WritePrivateProfileString(ProfileStringSection, "TrackExcpt", TrackExcpt.ToString, inifile)
                    NativeImports.WritePrivateProfileString(ProfileStringSection, "TrackArenas", TrackArenas.ToString, inifile)

                    NativeImports.WritePrivateProfileString(ProfileStringSection, "IncludeHeapReport", IncludeHeapReport.ToString, inifile)

                    NativeImports.WritePrivateProfileString(ProfileStringSection, "EnableAsserts", EnableAsserts.ToString, inifile)


                    NativeImports.WritePrivateProfileString(ProfileStringSection, "VirtualAllocTopDown", _VirtualAllocTopDown.ToString, inifile)

                    NativeImports.WritePrivateProfileString(ProfileStringSection, "DynStackSymRes", _DynStackSymRes.ToString, inifile)

                    NativeImports.WritePrivateProfileString(ProfileStringSection, "WhenDist", _WhenDist.ToString, inifile)

                    NativeImports.WritePrivateProfileString(ProfileStringSection, "RetailHeaderTrailerSize", RetailHeaderTrailerSize.ToString, inifile)
                End If
            End If
        End Sub

        Public Shared Function DeployMemSpectAndEnableDebug(ByVal strManifestPath As String) As Integer
            Dim dirPackage = IO.Path.GetDirectoryName(strManifestPath)

            Dim mspectExeInstallDirFullName = Reflection.Assembly.GetEntryAssembly().Location '<MemSpectInstalldir>\MemSpect.exe'
            Dim srcdir = IO.Path.GetDirectoryName(mspectExeInstallDirFullName) '<MemSpectInstalldir>
            ' we need to communicate the package path to the IPackageDebug::EnableDebugging debugger (MemSpect.exe) so it can write the ThreadID to the deployed MemSpect.Ini
            Dim inifileMemSpectInstallDir = IO.Path.Combine(srcdir, MemSpectIniName)
            WritePrivateProfileString(ProfileStringSection, "MEMSPECT_MANIFEST_PATH", _ImmersivePackageInfo.strManifestPath, inifileMemSpectInstallDir)

            WritePrivateProfileString(ProfileStringSection, "EnableAsserts", "0", inifileMemSpectInstallDir)
            Dim n = GetPrivateProfileInt(ProfileStringSection, "CleanUpRCWs", 0, inifileMemSpectInstallDir)
            WritePrivateProfileString(ProfileStringSection, "CleanUpRCWs", (n Or 2).ToString, inifileMemSpectInstallDir) ' make sure GC occurs in background thread

            ' put DLL where immersive can access them: Always, so Debug/Retail works
            DeployMemspectFiles(srcdir, dirPackage, fIncludeAll:=True, fOnlyIfNewer:=False) ' 

            '//launches strCmdLine process like so "c:\memspect\memspect.exe" -p 2816 -tid 56
            Dim strEnvVars = String.Empty ' = "MEMSPECT_MANIFEST_PATH=" + _ImmersivePackageInfo.strManifestPath
            Dim hr = PackageDebuggingEnabler(_ImmersivePackageInfo.strPackageFullName, mspectExeInstallDirFullName, strEnvVars, nEnable:=1)
            Return hr
        End Function

        Public Function LaunchTargProc(
                                      ByVal procName As String,
                                      ByVal fWithDll As Boolean,
                                      Optional ByVal fDoInitcomm As Boolean = True,
                                      Optional ByVal wrkdir As String = ""
                                                                  ) As Process
            _DidInitComm = False

            If String.IsNullOrEmpty(wrkdir) Then ' test assembly in Dev11 is not deploydir
                wrkdir = IO.Path.GetDirectoryName(Reflection.Assembly.GetExecutingAssembly.Location) ' D:\Memspect\VSAssertSrv\TestResults\calvinh_CALVINH9 2010-08-20 10_10_45\Out
                Debug.Assert(Not _IsUnderTest, "LaunchTargProc: must pass in test context deploy dir")
            Else
                Debug.Assert(_IsUnderTest, "LaunchTargProc: pass in test context deploy dir")
            End If
            Debug.Assert(IO.Directory.Exists(wrkdir), "Wrkdir not found " + wrkdir)
            Dim inifile = IO.Path.Combine(wrkdir, MemSpectIniName)
            Debug.Assert(IO.File.Exists(inifile), "inifile not found " + inifile)
            If String.IsNullOrEmpty(procName) Then
                procName = GetDevEnvFullPath()
            End If
            ' Check HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Image File Execution Options\devenv.exe
            ' "IFEO" and warn user about AppVerifier, GFlags, RascalPro JIT debugging, etc.
            Dim justfname = System.IO.Path.GetFileName(procName)
            Dim ImageFileOptionsKey = "SOFTWARE\Microsoft\Windows NT\CurrentVersion\Image File Execution Options\" + justfname
            Using key = Microsoft.Win32.Registry.LocalMachine.OpenSubKey(ImageFileOptionsKey)
                If key IsNot Nothing Then
                    UpdateStatusMsg("Warning: found key 'HKLM'\" + ImageFileOptionsKey + "'" + vbCrLf +
                                    "This can be detrimental to MemSpect: could be due to AppVerifier, GFlags, RascalPro JIT debugging, etc",
                                    msgType:=StatusMessageType.AlertMsgBox)
                End If
            End Using


            WriteTheIniFile(inifile, procName)
            If IsImmersiveApp Then
                Dim hr = ProcessLauncher.DeployMemSpectAndEnableDebug(_ImmersivePackageInfo.strManifestPath)
                If hr <> 0 Then
                    MsgBox("LaunchDeploy:" + procName + vbCrLf + GetErrorMessageFromHResult(hr), MsgBoxStyle.Exclamation)
                End If
                Dim procIdImm = 0
                hr = PackageLauncher(_ImmersivePackageInfo.strAppUserModelId, procIdImm)
                If hr <> 0 Then
                    MsgBox("Launch: " + procName + vbCrLf + GetErrorMessageFromHResult(hr).ToString, MsgBoxStyle.Exclamation)
                End If
                ImmersiveProcessId = procIdImm
                Return Nothing
            End If
            If Not IO.File.Exists(procName) Then
                Throw New IO.FileNotFoundException(procName)
            End If
            Dim stinfo As New ProcessStartInfo
#If False Then
Set COR_ENABLE_PROFILING=1
Set COR_PROFILER={01673DDC-46F5-454F-84BC-F2F34564C2AD}

rem for drops with any CHK bits, put this version of VSAssert and use this: 
rem 	         Set COR_PROFILER_PATH=C:\Program Files\Microsoft Visual Studio 10.0\Common7\IDE\Vsassert.dll
rem Set COR_PROFILER_PATH=%~dp0Vsassert.dll
Set COR_PROFILER_PATH=d:\memspect\Vsassert.dll

#End If
            Dim hProc As Process = Nothing
            Dim procId = 0
            If fWithDll Then
                Dim targDllName = IO.Path.Combine(wrkdir, MemSpectDllName)
                'Dim strProfPath = Environment.GetEnvironmentVariable("COR_PROFILER_PATH")
                'If Not String.IsNullOrEmpty(strProfPath) Then
                '    If String.Compare(IO.Path.GetDirectoryName(targDllName), IO.Path.GetDirectoryName(strProfPath), StringComparison.OrdinalIgnoreCase) <> 0 Then
                '        Throw New Exception("COR_PROFILER_PATH already set incompatibly " + vbCrLf +
                '                            "COR_PROFILER_PATH= " + strProfPath + vbCrLf +
                '                            "new path= " + targDllName)

                '    End If
                'End If
                'stinfo.EnvironmentVariables.AddIfDoesntExist("COR_ENABLE_PROFILING", "1")
                'stinfo.EnvironmentVariables.AddIfDoesntExist("COR_PROFILER", "{01673DDC-46F5-454F-84BC-F2F34564C2AD}")
                'stinfo.EnvironmentVariables.AddIfDoesntExist("COR_PROFILER_PATH", targDllName)
                '            stinfo.EnvironmentVariables.Item("_NT_SYMBOL_PATH") = "srv*c:\symcache*d:\" ' remove network for perf
                stinfo.EnvironmentVariables.AddIfDoesntExist("OANOCACHE", "1")
                stinfo.UseShellExecute = False ' needed to start with env vars

                '     Dim targfullpath = "D:\dd\MQProVSL\src\ddsuites\src\vs\vb\msvb7\UnitTests\Editor\Binaries\Harness.exe /t"
                Dim targcmdlineargs = "/n /D:""" + IO.Path.Combine(wrkdir, MemSpectDllName) + """ " + procName
                stinfo.FileName = IO.Path.Combine(wrkdir, "withdll.exe")
                stinfo.Arguments = targcmdlineargs
                If _AdditionalCmdLineParams.Length > 0 Then
                    stinfo.Arguments += " " + Trim(_AdditionalCmdLineParams)
                End If
                hProc = Process.Start(stinfo) ' hproc of "withdll.exe"
                If Not fDoInitcomm Then
                    Return Nothing
                End If

                System.Threading.Thread.Sleep(_nmsecsToWaitTilStart)

                '                ShowWindow(hProc.MainWindowHandle, SW_MINIMIZE)  ' withdll
                'hproc.WaitForInputIdle() ' only for gui. Withdll has no GUI and is not the desired procid
                procId = hProc.Id ' this is procid of Withdll
                ' look for it amongst possibly multiple instances.
                Dim q2 = From a In Process.GetProcesses
                         Where a.ProcessName.ToLower = IO.Path.GetFileNameWithoutExtension(procName).ToLower
                         Order By a.StartTime Descending

                If q2.Count() = 0 Then
                    UpdateStatusMsg("After waiting " + _nmsecsToWaitTilStart.ToString + " ProcessLauncher Procstart found 0 instances of " + procName, msgType:=StatusMessageType.AlertMsgBox)
                    Return Nothing
                End If
                If q2.Count > 1 Then
                    For Each pr In q2
                        Dim res = DoInitComm(pr.Id)
                        If String.IsNullOrEmpty(res) Then
                            procId = pr.Id ' bingo
                            'SendMsg(ProcMsgVerb.Quit) ' disconnect so others can connect
                            Exit For
                        Else
                            UpdateStatusMsg("found instance of " + procName + " but couldn't connect: " + res)
                            ' wrong process
                        End If
                    Next
                Else
                    procId = q2.First.Id
                End If
                hProc = Process.GetProcessById(procId) ' hproc of target (notepad or devenv)
            Else ' not withdll
                stinfo.FileName = procName
                stinfo.Arguments = _AdditionalCmdLineParams.Trim
                hProc = Process.Start(stinfo)
                System.Threading.Thread.Sleep(_nmsecsToWaitTilStart)
                'hproc.WaitForInputIdle() ' only for gui. Withdll has no GUI and is not the desired procid
                procId = hProc.Id
            End If

            'ShowWindow(hProc.MainWindowHandle, SW_MINIMIZE)
            _pidTargetProcess = procId
            Debug.WriteLine("Launched proc " + procName + " Pid = " + _pidTargetProcess.ToString)
            If fDoInitcomm Then
                If Not _DidInitComm Then
                    Dim strRes = DoInitComm(procId)
                    LauncherErrorMessage = strRes
                    If Not String.IsNullOrEmpty(strRes) Then
                        hProc = Nothing ' _test._VBAssert.IsTrue(False, "can't init comm " + strRes)
                    End If
                End If
            End If
            Return hProc
        End Function

        Private Function DoInitComm(ByVal pid As Integer) As String
            If fMessageBoxOnStart > 0 Then
                UpdateStatusMsg("DoInitComm fMessageBoxOnStart " + System.Diagnostics.Process.GetCurrentProcess.ProcessName + " " +
                                            System.Diagnostics.Process.GetCurrentProcess.Id.ToString,
                                            msgType:=StatusMessageType.AlertMsgBox)
            End If
            Dim res = ProcComm.InitComm({System.Environment.GetCommandLineArgs(0), CStr(pid)})
            If String.IsNullOrEmpty(res) Then
                _DidInitComm = True
                If _MemSpectVersion.Contains("D") Then
                    _targProcIsDebug = True
                End If
            End If
            Return res
        End Function
        'Friend Sub TestLaunch()
        '    Dim fIs64bitOS = Not String.IsNullOrEmpty(My.Application.GetEnvironmentVariable("programfiles(x86)"))
        '    Dim windir = My.Application.GetEnvironmentVariable("windir")
        '    If fIs64bitOS Then
        '        windir += "\syswow64\"
        '    Else
        '        windir += "\system32\"
        '    End If
        '    Dim targfullpath = ""
        '    Dim DevEnvPath = IO.Path.Combine(My.Application.GetEnvironmentVariable("programfiles"), "Microsoft Visual Studio 10.0\Common7\IDE") + "\"

        '    '        targfullpath = IO.Path.Combine(windir, "notepad.exe")
        '    targfullpath = IO.Path.Combine(DevEnvPath, "devenv.exe")
        '    Dim hProc = LaunchTargProc(targfullpath, fWithDll:=True)
        '    If hProc Is Nothing Then
        '        MsgBox("launch failed " + targfullpath)
        '        Return
        '    End If
        '    Dim pid = hProc.Id
        '    Dim fDidit = False
        '    While Not fDidit ' keep looping: ROT takes a while to update
        '        Dim prot As ComTypes.IRunningObjectTable = Nothing
        '        GetRunningObjectTable(0, prot)
        '        Dim enumMoniker As ComTypes.IEnumMoniker = Nothing
        '        prot.EnumRunning(enumMoniker)
        '        enumMoniker.Reset()
        '        Dim nFetched As IntPtr = IntPtr.Zero
        '        Dim monikers As ComTypes.IMoniker() = {Nothing}
        '        While enumMoniker.Next(1, monikers, nFetched) = 0
        '            Dim moniker = monikers(0)
        '            Dim pbc As ComTypes.IBindCtx = Nothing
        '            CreateBindCtx(0, pbc)
        '            If pbc IsNot Nothing Then
        '                Dim sb = ""
        '                moniker.GetDisplayName(pbc, Nothing, sb)
        '                Debug.WriteLine("GotMoniker " + sb)
        '                Dim runningobj As Object = Nothing
        '                prot.GetObject(moniker, runningobj)
        '                If TypeOf (runningobj) Is EnvDTE._DTE Then
        '                    Dim dte = CType(runningobj, EnvDTE._DTE)

        '                    Debug.Write("IsDTE " + dte.FullName)
        '                    If sb.EndsWith(pid.ToString) Then
        '                        Debug.WriteLine("Sending Exit " + pid.ToString)
        '                        dte.ExecuteCommand("exit")
        '                        fDidit = True
        '                        Exit While
        '                    End If
        '                End If
        '                Debug.WriteLine(sb)
        '            End If
        '        End While
        '    End While
        'End Sub

        'Friend Function GetVSDTE() As EnvDTE.DTE
        '    If True Then
        '        Return Nothing
        '    End If
        '    Dim dte As EnvDTE.DTE = Nothing
        '    'http://social.msdn.microsoft.com/forums/en-US/vbinterop/thread/561f4f11-0ea8-4c1d-b344-6b22bf9d2640/
        '    ' ClassInit, TestMethod called on diff threads.
        '    Dim fGotIt = False
        '    Dim nLoopCnt = 0
        '    While Not fGotIt ' keep looping: ROT takes a while to update
        '        Dim prot As ComTypes.IRunningObjectTable = Nothing
        '        GetRunningObjectTable(0, prot)
        '        Dim enumMoniker As ComTypes.IEnumMoniker = Nothing
        '        prot.EnumRunning(enumMoniker)
        '        enumMoniker.Reset()
        '        Dim nFetched As IntPtr = IntPtr.Zero
        '        Dim monikers As ComTypes.IMoniker() = {Nothing}
        '        While enumMoniker.Next(1, monikers, nFetched) = 0
        '            Dim moniker = monikers(0)
        '            Dim pbc As ComTypes.IBindCtx = Nothing
        '            CreateBindCtx(0, pbc)
        '            If pbc IsNot Nothing Then
        '                Dim sb = ""
        '                moniker.GetDisplayName(pbc, Nothing, sb)
        '                '                    Debug.WriteLine("GotMoniker " + sb)
        '                Dim runningobj As Object = Nothing
        '                prot.GetObject(moniker, runningobj)
        '                If TypeOf (runningobj) Is EnvDTE._DTE Then
        '                    dte = CType(runningobj, EnvDTE._DTE)
        '                    '                            Debug.Write("IsDTE " + dte.FullName)
        '                    If sb.EndsWith(_pidTargetProcess.ToString) Then
        '                        Debug.WriteLine("Got DTE " + _pidTargetProcess.ToString)
        '                        'dte.ExecuteCommand("File.OpenProject ""C:\Users\calvinh\Documents\Visual Studio 2010\Projects\Heap\Heap.vbproj""")
        '                        'dte.ExecuteCommand("exit")
        '                        fGotIt = True
        '                        Exit While
        '                    End If
        '                End If
        '                '                        Debug.WriteLine(sb)
        '            End If
        '        End While
        '        VBDiagMarginBase.WaitForOtherThreads(5)
        '        nLoopCnt += 1
        '        Debug.Assert(nLoopCnt < 10, "can't get devenv dte")
        '    End While
        '    Return dte
        'End Function
        Public Shared Function GetDevEnvFullPath() As String
            Dim WillowInstallEnvVar = "DefaultLowImpactVSPath" ' DefaultLowImpactVSPath=D:\Program Files (x86)\Microsoft Visual Studio 15.0\Common7\IDE\devenv.exe

            Dim willowVal = System.Environment.GetEnvironmentVariable(WillowInstallEnvVar)
            If Not String.IsNullOrEmpty(willowVal) Then
                Return willowVal
            End If
            Dim lstSkus = {"devenv.exe", "vswinexpress.exe"}
            For Each verstring In {"16.0", "15.0", "14.0", "12.0", "11.0", "10.0"}
                For Each exename In lstSkus
                    Dim filename = IO.Path.Combine(IO.Path.Combine(
                                             System.Environment.GetEnvironmentVariable("programfiles"),
                                             String.Format("Microsoft Visual Studio {0}\Common7\IDE", verstring)),
                                            exename)
                    If IO.File.Exists(filename) Then
                        _VsVersionstring = verstring
                        Return filename
                    End If
                Next
            Next
            Dim vsdir = IO.Path.Combine(System.Environment.GetEnvironmentVariable("programfiles"), "Microsoft Visual Studio")
            Dim lstDevenv = New List(Of System.IO.FileInfo)
            If IO.Directory.Exists(vsdir) Then ' we have "C:\Program Files (x86)\Microsoft Visual Studio\"
                '"C:\Program Files (x86)\Microsoft Visual Studio\2017\Enterprise\Common7\IDE\devenv.exe"
                '"C:\Program Files (x86)\Microsoft Visual Studio\VS15Preview\Common7\IDE"
                ' we want to search for  Common7/IDE/Devenv.exe but we need to do it efficiently
                ' so search 2 levels down max
                Dim lamRecurDir As Action(Of String, Integer) = Nothing
                lamRecurDir = Sub(pathIn As String, nLevel As Integer)
                                  If nLevel < 2 Then
                                      Dim vsInstalls = IO.Directory.GetDirectories(pathIn)
                                      For Each vsInStall In vsInstalls
                                          If IO.Directory.Exists(IO.Path.Combine(vsInStall, "Common7")) Then
                                              For Each exename In lstSkus
                                                  Dim filename = IO.Path.Combine(vsInStall, "Common7\IDE", exename)
                                                  If IO.File.Exists(filename) Then
                                                      lstDevenv.Add(New System.IO.FileInfo(filename))
                                                  End If
                                              Next
                                          Else
                                              lamRecurDir(vsInStall, nLevel + 1)
                                          End If
                                      Next
                                  End If
                              End Sub
                lamRecurDir(vsdir, 0)
                If lstDevenv.Count > 0 Then
                    Dim newest = (From devenv In lstDevenv Order By devenv.CreationTime Descending).FirstOrDefault
                    If newest IsNot Nothing Then
                        Return newest.FullName
                    End If
                End If
            End If
            Return String.Empty
        End Function

        Friend Function ShutDownTargetProcess() As String
            Dim retval = ""
            If _pidTargetProcess <> 0 Then
                If _ConnectionMode = MemSpectMode.OnLine Then
                    SendMsg(ProcMsgVerb.Quit, fSendEndMsgSync:=False, dwords:={0}) 'detach memspect (will unfreeze if nec), leave parent proc running
                End If

                Dim hProc = Process.GetProcessById(_pidTargetProcess) ' hproc of target (notepad or devenv)
                Dim ncnt = 15
                hProc.CloseMainWindow()
                While Not hProc.HasExited
                    '                hProc.Close()
                    hProc.WaitForExit(5000 * If(_targProcIsDebug, 4, 1))
                    ncnt -= 1
                    If ncnt = 0 Then
                        hProc.Kill()
                        retval = "Process had to be killed"
                        Exit While
                    End If
                End While
                _pidTargetProcess = 0
                'Dim dte = _ProcessLaunch.GetVSDTE()
                'If dte IsNot Nothing Then
                '    dte.ExecuteCommand("exit")
                '    dte.Quit()

                'End If

            End If
            Return retval
        End Function

        Public Shared Function InjectIntoProcess(ByVal hProc As Process, ByVal memspDllPathToTry As String, ByVal fDebug As Boolean) As String
            Dim strResult = String.Empty
            Try
                Dim memspDllPath = memspDllPathToTry

                Dim fIsImmersive = False
                Dim PackageFamilyName = New Text.StringBuilder(MAX_PATH)
                Dim hr = GetPackageFamilyNameFromProcess(hProc.Handle, PackageFamilyName)
                If hr = 0 AndAlso PackageFamilyName.Length > 0 Then
                    fIsImmersive = True
                    ' can't copy it ourselves because must run with UAC on
                    memspDllPath = Environment.ExpandEnvironmentVariables("%windir%\system32\MemSpect\" + MemSpectDllName)
                End If
                If Not IO.File.Exists(memspDllPath) Then
                    Throw New InvalidOperationException("File not found " + memspDllPath +
                                                        If(fIsImmersive,
                                                           vbCrLf + " (on 64 bit OS, must be in 'C:\Windows\SysWow64\MemSpect\MemSpectDll.dll') ",
                                                           String.Empty))
                End If

                If Not fIsImmersive Then ' immersive will have memspect in system32 to avoid signing, but ini file in manifest dir
                    ' verify INI settings are valid
                    Dim inifile = IO.Path.Combine(IO.Path.GetDirectoryName(memspDllPath), MemSpectIniName)
                    If Not _IsUnderTest Then ' test sets to 1, but launcher doesn't
                        Dim startchildproc = GetPrivateProfileInt(ProfileStringSection, "StartChildProcess", 0, inifile)
                        If startchildproc <> 0 Then
                            MsgBox("Warning: StartChildProcess is set to 1 in " + inifile + vbCrLf +
                                   " This will cause 2 instances of MemSpect to try to attach. Just dismiss the error about 'System.UnauthorizedAccessException: Access to the path is denied' while creating NamedPipeServerStreamToChild")
                        End If

                    End If
                    AddProcessToJustThisProcesses(hProc.ProcessName, inifile)

                End If

                ' for a given reboot, Loadlib addr is const even in ASLR world
                Dim hKernel32 = GetModuleHandle("kernel32.dll")
                Debug.Assert(hKernel32 <> IntPtr.Zero, "could not get kernel32.dll")
                Dim pAddrLoadLibrary = GetProcAddress(hKernel32, "LoadLibraryA")
                Debug.Assert(pAddrLoadLibrary <> IntPtr.Zero, "could not get pLoadLibrary")

                ' size is rounded to next alloc granularity: 4k for page commit, 64k chunk of VM. 
                Dim addrPathString = VirtualAllocEx(hProc.Handle,
                                           IntPtr.Zero,
                                           256,
                                           CType(AllocationState.MEM_COMMIT Or AllocationState.MEM_RESERVE, AllocationType),
                                           AllocationProtect.PAGE_READWRITE)
                If addrPathString = IntPtr.Zero Then
                    strResult = GetErrorMessageFromWin32LastError(System.Runtime.InteropServices.Marshal.GetLastWin32Error)
                    Return " VirtualAllocEx failed: " + strResult
                End If

                Dim bencoder = New Text.ASCIIEncoding
                Dim bytArr = bencoder.GetBytes(memspDllPath)
                'Init'd to zero, so don't need nullterm
                Dim resw = WriteProcessMemory(hProc.Handle, addrPathString, bytArr, memspDllPath.Length, 0)
                If resw = 0 Then
                    strResult = GetErrorMessageFromWin32LastError(System.Runtime.InteropServices.Marshal.GetLastWin32Error)
                    Return " WriteProcessMemory failed: " + strResult
                End If
#If DEBUG Then
                Dim pathRead = GetStringFromRemoteMem(addrPathString, memspDllPath.Length, hProc.Handle)
                Debug.Assert(memspDllPath = pathRead, "Path written not same as read")
                '                System.Threading.Thread.Sleep(16000) ' wait to attach debugger
                'MsgBox("about to createremote")
#End If
                Dim hThread = CreateRemoteThread(hProc.Handle, IntPtr.Zero, 256000, pAddrLoadLibrary, addrPathString, 0, IntPtr.Zero)
                If hThread = IntPtr.Zero Then
                    strResult = GetErrorMessageFromWin32LastError(System.Runtime.InteropServices.Marshal.GetLastWin32Error)
                    Return " CreateRemoteThread failed: " + strResult
                End If
                Debug.Assert(hThread <> IntPtr.Zero, "CreateRemoteThread returned 0")
                'MsgBox("AddrPathstr= " + addrPathString.ToString("x8") + " " + memspDllPath)
                System.Threading.Thread.Sleep(3000) ' wait for the remote thread to load/init memspect.exe as UI
                If fDebug Then
                    System.Threading.Thread.Sleep(30000)
                End If
#If DEBUG Then
                System.Threading.Thread.Sleep(6000) ' wait for the remote thread to load/init memspect.exe as UI
#End If
                If fDebug Then
                    System.Threading.Thread.Sleep(15000) ' wait for the remote thread to load/init memspect.exe as UI
                End If
                If Not hProc.HasExited Then
                    Dim res = VirtualFreeEx(hProc.Handle, addrPathString, 0, FreeType.MEM_RELEASE)
                    If Not res Then
                        Dim lerr = System.Runtime.InteropServices.Marshal.GetLastWin32Error
                        Dim errmsg = GetErrorMessageFromWin32LastError(lerr)
                        Debug.Assert(res, "could not free vm " + errmsg)
                    End If
                End If
            Catch ex As Exception
                strResult = ex.ToString
            End Try

            Return strResult
        End Function

        Public Shared Sub AddProcessToJustThisProcesses(ByVal procName As String, ByVal inifile As String)
            Dim sb = New Text.StringBuilder(500)
            NativeImports.GetPrivateProfileString(ProfileStringSection, "JustTheseProcesses", String.Empty, sb, sb.Capacity, inifile)
            Dim JustThese = sb.ToString.ToLower
            If JustThese = "*" Then ' all processes are enabled with "*"
                Return
            End If
            Dim justfname = IO.Path.GetFileNameWithoutExtension(procName).ToLower
            Dim ndx = JustThese.IndexOf(justfname)
            If ndx > 0 AndAlso JustThese(ndx - 1) = ";" AndAlso
                JustThese.Length > ndx + justfname.Length AndAlso
                JustThese(ndx + justfname.Length) = ";" AndAlso
                JustThese.Substring(ndx, justfname.Length) = justfname Then
            Else
                If JustThese.Length = 0 Then
                    JustThese = ";" + justfname + ";"
                Else
                    JustThese += justfname + ";"
                End If
                NativeImports.WritePrivateProfileString(ProfileStringSection, "JustTheseProcesses", JustThese, inifile)
            End If
        End Sub


    End Class

End Namespace