Imports System.Runtime.InteropServices
Imports MemSpect
Imports System.IO

Friend Module TstUtil
    Public ReadOnly Property memspectInstallDir As String
        Get
            ' D:\Memspect\VSAssertSrv\TestResults\calvinh_CALVINH9 2010-08-20 10_10_45\Out
            Dim wrkdir = Path.GetDirectoryName(Reflection.Assembly.GetExecutingAssembly.Location)
            If wrkdir.ToLower.StartsWith(Path.GetTempPath.ToLower) Then ' some builds of dev11 (Jan/Feb 2011)
                Return "c:\memspect"
            End If
            Dim ndx = wrkdir.IndexOf("VSAssertSrv", 0, StringComparison.OrdinalIgnoreCase) ' dev10
            Dim installdir = ""
            If ndx < 0 Then
                ndx = wrkdir.LastIndexOf("test", StringComparison.OrdinalIgnoreCase) ' dev11
            End If
            If ndx >= 1 Then
                installdir = wrkdir.Substring(0, ndx - 1)    '"d:\MemSpect"
            End If
            Return installdir
        End Get
    End Property






    <DllImport("ole32.dll")>
    Public Sub GetRunningObjectTable(ByVal reserved As Integer,
                              ByRef pRunningObjectTable As ComTypes.IRunningObjectTable)
    End Sub

    <DllImport("ole32.dll")>
    Public Sub CreateBindCtx(ByVal reserved As Integer, ByRef pBindCtx As ComTypes.IBindCtx)

    End Sub


#If False Then

    Friend Class MyTestContext
        Inherits TestContext
        Private m_Properties As New Dictionary(Of String, Object)
        Private m_Log As IO.StringWriter

        Public Overrides Sub AddResultFile(ByVal fileName As String)

        End Sub

        Public Overrides Sub BeginTimer(ByVal timerName As String)

        End Sub

        Public Overrides ReadOnly Property DataConnection As System.Data.Common.DbConnection
            Get
                Throw New NotImplementedException((New StackTrace).GetFrame(0).GetMethod.Name)
            End Get
        End Property

        Public Overrides ReadOnly Property DataRow As System.Data.DataRow
            Get
                Throw New NotImplementedException((New StackTrace).GetFrame(0).GetMethod.Name)
            End Get
        End Property

        Public Overrides Sub EndTimer(ByVal timerName As String)

        End Sub

        Public Overrides ReadOnly Property Properties As System.Collections.IDictionary
            Get
                Return m_Properties
            End Get
        End Property

        Public Overrides Sub WriteLine(ByVal format As String, ByVal ParamArray args() As Object)

        End Sub
    End Class

#End If













    ''' <summary>
    ''' todo: add a way to allow failed asserts to be logged and let the tests continue 
    ''' </summary>
    ''' <remarks></remarks>
    Public Class VBAssert


        ''' <summary>
        ''' By Default we will log output to a file in the test directory.  This significantly helps with debugging
        ''' during a large suite update
        ''' </summary>
        ''' <remarks></remarks>
        Public _fLogOutputToFile As Boolean = True
        ''' <summary>
        ''' used to indicate a test baseline comment. If it starts with this prefix, then an Assert.Equal is not executed
        ''' </summary>
        ''' <remarks></remarks>
        Public Const g_TestCommentPrefix As String = "==>" '  

        Public Property _IsCalculatingBaseLine As Boolean
        Public Property _IsCalculatingBaseLineWasToggledTrue As Boolean ' indicate we toggled this true due to first failure, so subsequent failures will be shown
        Public Property _SuppressAsserts As Boolean
        Public Property _fOutputToOutputWindow As Boolean

        Private m_AssertCount As Integer
        Friend Shared _TestRunStreamWriter As StreamWriter
        Private __BaselineStreamReader As StreamReader
        Private ReadOnly Property _BaselineStreamReader As StreamReader
            Get
                If __BaselineStreamReader Is Nothing Then
                    If String.IsNullOrEmpty(_BaseLineFile) Then
                        _test._VBAssert.IsTrue(False, "null baseline file")
                    End If
                    __BaselineStreamReader = New StreamReader(_BaseLineFile)
                End If
                Return __BaselineStreamReader
            End Get
        End Property
        Private _BaselineStreamLeftover As String = String.Empty
        Private Function GetExpectedString() As String

            Dim strRetval = String.Empty
            '_ExpectedArray = File.ReadAllText(_BaseLineFile).Replace(vbCrLf, vbLf).Split(CChar(vbLf)).Where(
            '    Function(x) Not x.StartsWith(g_TestCommentPrefix)).ToArray

            If String.IsNullOrEmpty(_BaselineStreamLeftover) Then
                Dim nextline = String.Empty
                Do While True
                    If _BaselineStreamReader.EndOfStream Then
                        Return Nothing
                    End If
                    nextline = _BaselineStreamReader.ReadLine.Replace(vbCrLf, vbLf)
                    If Not nextline.StartsWith(g_TestCommentPrefix) Then
                        Exit Do
                    End If
                Loop
                _BaselineStreamLeftover = nextline
                Dim ndxlf = _BaselineStreamLeftover.IndexOf(vbLf)
                If ndxlf >= 0 Then
                    strRetval = _BaselineStreamLeftover.Substring(0, ndxlf)
                    _BaselineStreamLeftover = _BaselineStreamLeftover.Substring(ndxlf + 1)
                Else
                    strRetval = _BaselineStreamLeftover
                    _BaselineStreamLeftover = String.Empty
                End If
            End If
            Return strRetval
        End Function

        Private _nExpectedArrayIndex As Integer
        Private _fHasUsedUpAllExpectedArrayEntries As Boolean
        Public _BaseLineFile As String ' full path
        Friend _test As TestBase
        Private _nFailures As Integer

        Sub New(ByVal test As TestBase)
            _test = test
            Try
                Dim fsTestLog = New FileStream(tempResultsFile, FileMode.Create, FileAccess.Write, FileShare.None)
                If _TestRunStreamWriter IsNot Nothing Then
                    _TestRunStreamWriter.Close()
                End If
                _TestRunStreamWriter = New StreamWriter(fsTestLog)
            Catch ex As Exception

            End Try
            'test.TestContext.BeginTimer(test.TestContext.TestName)
            If Debugger.IsAttached Then
                _fOutputToOutputWindow = True
            End If
        End Sub

        Public ReadOnly Property tempResultsFile As String
            Get
                Return Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.Desktop), "MemSpectLog.txt")
                'Return Path.Combine(_test.TestContext.TestDeploymentDir, "CurrentTestOutput.txt")
            End Get
        End Property
        Public ReadOnly Property outputResultsfile As String
            Get
                If String.IsNullOrEmpty(_BaseLineFile) Then
                    _test._VBAssert.IsTrue(False, "null baseline file")
                End If
                Return Path.Combine(_test.TestContext.TestRunDirectory, Path.GetFileName(_BaseLineFile))
            End Get
        End Property

        Public Sub SetBaselineFile(ByVal baseFile As String)
            _BaseLineFile = baseFile
            'If File.Exists(baseFile) Then
            '    ' poor man's way to know which test is currently running
            '    Dim dst = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.Desktop), "log.txt")
            '    Try
            '        File.Copy(baseFile, dst, overwrite:=True)
            '    Catch ex As Exception

            '    End Try

            'End If
            If Not File.Exists(baseFile) Then
                _IsCalculatingBaseLine = True
                _fOutputToOutputWindow = True
            End If
        End Sub

        Public Sub OnTestFinished()
            'If Not _IsCalculatingBaseLine Then
            '    If _BaseLineFile IsNot Nothing Then
            '        If _nExpectedArrayIndex <> _ExpectedArray.Length Then
            '            'If _nExpectedArrayIndex = _ExpectedArray.Length - 1 AndAlso
            '            '    _ExpectedArray(_nExpectedArrayIndex).Trim.Length = 0 Then
            '            '    OutputText("") ' accomodate final empty line
            '            'End If
            '            IsTrue(_fHasUsedUpAllExpectedArrayEntries, String.Format("Not all expected values in baseline matched Expected={0} Actual ={1}",
            '                                                                     _nExpectedArrayIndex.ToString,
            '                                                                     _ExpectedArray.Length.ToString
            '                                                                     ))
            '        End If

            '    End If
            'End If
            '_test.TestContext.EndTimer(_test.TestContext.TestName)
            VsClearSymbols() ' free client symbols for more ram
            If Not _IsCalculatingBaseLine Then
                If Not _BaselineStreamReader.EndOfStream Then
                    If Not _IsCalculatingBaseLine Then
                        Dim leftOver = 0
                        While Not _BaselineStreamReader.EndOfStream AndAlso GetExpectedString() IsNot Nothing
                            leftOver += 1
                        End While
                        If leftOver > 0 Then
                            OutputText(String.Format("Expected {0} lines   Actual {1} lines", _nExpectedArrayIndex + leftOver, _nExpectedArrayIndex), fAddToBaseline:=True)
                        End If
                    End If
                End If

            End If
            OutputText("# of asserts = " + g_AssertCount.ToString, fAddToBaseline:=False)
            If File.Exists(_BaseLineFile) Then
                'D:\Memspect\VSAssertSrv\TestResults\calvinh_CALVINH9 2010-08-25 14_33_29
                Dim diffArgs = _BaseLineFile + " """ + outputResultsfile + """"

                '//"HKEY_CURRENT_USER\Software\Microsoft\VisualStudio\11.0\TeamFoundation\SourceControl\DiffTools\.*\Compare" /v "Command" /d "c:\bin\bc2.exe" /f
                '// 1st try dev10, then dev11
                Dim cmpCommand = CStr(Microsoft.Win32.Registry.GetValue("HKEY_CURRENT_USER\Software\Microsoft\VisualStudio\12.0\TeamFoundation\SourceControl\DiffTools\.*\Compare", "Command", ""))
                If (String.IsNullOrEmpty(cmpCommand)) Then
                    cmpCommand = CStr(Microsoft.Win32.Registry.GetValue("HKEY_CURRENT_USER\Software\Microsoft\VisualStudio\14.0\TeamFoundation\SourceControl\DiffTools\.*\Compare", "Command", "windiff.exe"))
                End If

                OutputText("Compare cmd = " + cmpCommand + " " + diffArgs, fAddToBaseline:=False) ' write out the cmd so easy to execute later
                _TestRunStreamWriter.Close()
                If File.Exists(outputResultsfile) Then
                    File.Delete(outputResultsfile)
                End If
                File.Copy(tempResultsFile, outputResultsfile)
                If _nFailures > 0 Then
                    Try
                        'Dim procinfo = New ProcessStartInfo
                        'procinfo.FileName = "cmd.exe"
                        'procinfo.Arguments = "/c start c:\bin\bc2.exe " + diffArgs

                        'Dim hprocDiff = Process.Start(procinfo)
                        'hprocDiff.WaitForExit(3000)
                        Dim hprocDiff = Process.Start(cmpCommand + " ", diffArgs)
                    Catch ex As Exception
                        Debug.WriteLine("Err starting compare " + ex.Message)
                    End Try
                    Assert.IsTrue(False, "Test failures " + _nFailures.ToString)
                End If

                If _IsCalculatingBaseLine Then ' MsgBox("Update baseline?", MsgBoxStyle.OkCancel) = MsgBoxResult.Ok Then
                    File.Delete(_BaseLineFile)
                    File.Copy(tempResultsFile, _BaseLineFile)
                End If
            Else
                _TestRunStreamWriter.Close()
                If Not String.IsNullOrEmpty(_BaseLineFile) Then
                    File.Copy(tempResultsFile, _BaseLineFile)
                End If
            End If
            _TestRunStreamWriter = Nothing

        End Sub
        ''' <summary>
        ''' Outputs to Debug.Writeline and text file only if g_fCalcBaseline is true
        ''' </summary>
        ''' <param name="strOutput"></param>
        ''' <remarks> need to work around a bug where clicking on a test result run (pass or fail) in Test Results window causes
        ''' msgbox "Error creating window handle"
        ''' http://sharepoint/sites/spdhost/Shared%20Documents/Dev%20(includes%20costs,%20architecture%20docs)/Dev%20issues/Opening%20test%20result%20from%20Test%20Results%20window%20causes%20msgbox(Error%20creating%20window%20handle).htm
        ''' </remarks>
        Public Sub OutputText(ByVal strOutput As String,
                                            Optional ByVal fAddToBaseline As Boolean = True,
                                            Optional ByVal cSplitChar As Char = " "c)
            '    strOutput = strOutput.Trim
            ' if you want to split a string, cSplitChar =CChar(vbCr)
            If cSplitChar <> " "c AndAlso strOutput.IndexOf(cSplitChar) >= 0 Then ' allow multiple lines to be split
                Dim split = strOutput.Split(CChar(cSplitChar))
                For i = 0 To split.Length - 1
                    Dim ln = split(i)
                    Dim newtext = ln.Replace(vbLf, "")
                    'If fAddExtraCR AndAlso i < split.Length - 1 Then
                    '    newtext += vbCr
                    'End If
                    OutputText(newtext, fAddToBaseline, " "c)  ' recur
                Next
                Return
            End If
            If Not fAddToBaseline Then
                strOutput = g_TestCommentPrefix + strOutput
            End If
            _test.TestContext.WriteLine("{0}", strOutput)
            If _fOutputToOutputWindow Then 'Or g_fCalcBaseline Then
                Trace.WriteLine(strOutput)
            End If

            _TestRunStreamWriter.WriteLine(strOutput) ' some strings don't have data prefix and are added anyway

            'If g_fLogOutputToFile Then
            '    g_TestRunOutputStringBuilder.AppendLine(strOutput)
            '    '           If Not Diagnostics.Debugger.IsAttached Then ' debugger attached: use output window
            '    '            Microsoft.VisualBasic.FileIO.FileSystem.WriteAllText(g_TestRunOutputFile, strOutput + vbCrLf, True)
            '    '                If(m_IsSuites And False, System.Text.Encoding.ASCII, System.Text.Encoding.Unicode)) ' appends to file in "D:\dd\SPDHost\src\vb\Language\MSVBIDETest\TestResults\Test _2008-03-06 09_09_38_\Out"
            '    '            End If
            'End If

            If fAddToBaseline Then
                If Not _IsCalculatingBaseLine Then
                    If _BaseLineFile IsNot Nothing Then
                        If Not _BaselineStreamReader.EndOfStream Then
                            Dim expected = GetExpectedString()
                            If expected Is Nothing Then
                                OutputFailureString("Reached end of baseline, but more text: " + strOutput)
                            Else
                                _nExpectedArrayIndex += 1
                                AreEqual(expected, strOutput, "ExpectedIndex = " + _nExpectedArrayIndex.ToString, True)
                            End If
                        Else
                            _test._VBAssert.IsTrue(False, "we have more text than orig baseline")
                        End If

                    End If
                Else
                    _test._VBAssert.g_AssertCount += 1 ' increment because we would have asserted
                End If
            End If
        End Sub

        Public Sub AreEqual(Of T)(ByVal Expected As T, ByVal actual As T, Optional ByVal msg As String = "", Optional ByVal fStripCrLf As Boolean = False)
            g_AssertCount += 1
            If Not _IsCalculatingBaseLine Then
                If Not Expected.Equals(actual) Then
                    ' for suites, 0D 0D 0A 22 0D 0A 
                    Dim fIsGood = False
                    If fStripCrLf Then
                        If Expected.ToString.Replace(vbCr, "").Replace(vbLf, "") = actual.ToString.Replace(vbCr, "").Replace(vbLf, "") Then
                            fIsGood = True
                        End If
                    End If
                    If Not fIsGood Then
                        OutputFailureString(_test.TestContext.TestName + " " + ((New StackTrace).GetFrames(0).GetMethod.Name) + " failed " + msg)
                        OutputFailureString("Expected: """ + Expected.ToString + """")
                        OutputFailureString("Actual  : """ + actual.ToString + """")

                        If ShouldWeAssert() Then
                            Assert.AreEqual(Expected, actual, msg)
                        End If
                    End If
                End If
            End If
        End Sub
        Public Sub IsNotNull(Of T)(ByVal val As T, Optional ByVal msg As String = "")
            g_AssertCount += 1
            If Not _IsCalculatingBaseLine Then
                If val Is Nothing Then
                    If Not _SuppressAsserts Then
                        OutputFailureString(_test.TestContext.TestName + " " + ((New StackTrace).GetFrames(0).GetMethod.Name) + " failed " + msg)
                    End If

                    If ShouldWeAssert() Then
                        Assert.IsNotNull(val, msg)
                    End If
                End If

            End If
        End Sub
        Public Sub IsNull(Of T)(ByVal val As T, Optional ByVal msg As String = "")
            g_AssertCount += 1
            If Not _IsCalculatingBaseLine Then
                If val IsNot Nothing Then
                    If Not _SuppressAsserts Then
                        OutputFailureString(_test.TestContext.TestName + " " + ((New StackTrace).GetFrames(0).GetMethod.Name) + " failed " + msg)
                        OutputFailureString("Val = " + val.ToString)
                    End If
                    If ShouldWeAssert() Then
                        Assert.IsNull(val, msg)
                    End If
                End If
            End If
        End Sub
        Public Sub IsTrue(ByVal val As Boolean, Optional ByVal msg As String = "")
            g_AssertCount += 1
            If Not _IsCalculatingBaseLine Then
                If Not val Then
                    If Not _SuppressAsserts Then
                        OutputFailureString(_test.TestContext.TestName + " " + ((New StackTrace).GetFrames(0).GetMethod.Name) + " failed " + msg)
                        OutputFailureString("Val = " + val.ToString)
                    End If
                    If ShouldWeAssert() Then
                        Assert.IsTrue(val, msg)
                    End If
                End If
            Else
                If _IsCalculatingBaseLineWasToggledTrue Then
                    Assert.IsTrue(val, msg)
                End If
            End If
        End Sub
        Public Sub IsFalse(ByVal val As Boolean, Optional ByVal msg As String = "")
            g_AssertCount += 1
            If Not _IsCalculatingBaseLine Then
                If val Then
                    If Not _SuppressAsserts Then
                        OutputFailureString(_test.TestContext.TestName + " " + ((New StackTrace).GetFrames(0).GetMethod.Name) + " failed " + msg)
                        OutputFailureString("Val = " + val.ToString)
                    End If
                    If ShouldWeAssert() Then
                        Assert.IsFalse(val, msg)
                    End If
                End If
            Else
                If _IsCalculatingBaseLineWasToggledTrue Then
                    Assert.IsFalse(val, msg)
                End If
            End If
        End Sub

        Public Sub IsInstanceOfType(ByVal obj As Object, ByVal expectedType As Type, Optional ByVal msg As String = "")
            g_AssertCount += 1
            If Not _IsCalculatingBaseLine Then
                If Not expectedType.IsInstanceOfType(obj) Then
                    If Not _SuppressAsserts Then
                        OutputFailureString(_test.TestContext.TestName + " " + ((New StackTrace).GetFrames(0).GetMethod.Name) + " failed " + msg)
                        OutputFailureString("Assert IsInstanceOfType failed " + msg + " " + obj.ToString + expectedType.ToString)
                    End If
                    If ShouldWeAssert() Then
                        Assert.IsInstanceOfType(obj, expectedType, msg)
                    End If
                End If
            Else
                If _IsCalculatingBaseLineWasToggledTrue Then
                    Assert.IsInstanceOfType(obj, expectedType, msg)
                End If
            End If
        End Sub

        Public Property g_AssertCount() As Integer
            Get
                Return m_AssertCount
            End Get
            Set(ByVal value As Integer)
                m_AssertCount = value
            End Set
        End Property

        Public Sub OutputFailureString(ByVal str As String)
            _nFailures += 1
            OutputText(str, fAddToBaseline:=False) ' making fAddToBaseline true causes infinite recursion
            If _BaseLineFile Is Nothing Then ' when test has no baseline at all and we don't want one
                Assert.IsTrue(False, str)
            End If

        End Sub

        Public Function ShouldWeAssert() As Boolean
            Dim retval = True
            Dim y = "This line will be hit just before any Assert.Condition fires"
            If _SuppressAsserts Then
                retval = False
            Else
                _nFailures += 1
                If _IsCalculatingBaseLine Then
                    retval = False
                Else
                    _IsCalculatingBaseLine = True ' we got 1st failure: let's indicate we're just collecting failures
                    _IsCalculatingBaseLineWasToggledTrue = True ' indicate so
                    retval = False
                End If
            End If
            Return retval   ' change this to false and we won't assert
        End Function


    End Class

End Module
