Imports MemSpect
Imports System.IO
Imports System.Xml.Serialization
Imports System.CodeDom.Compiler
Imports System.Reflection
Imports System.Text

<TestClass()>
<CLSCompliant(False)>
Public Class KnownIssuesTests
    Inherits TestBase
    <ClassInitialize()>
    Public Shared Sub ClassInit(ByVal ctx As TestContext)
        BaseClassInitialize(ctx)
    End Sub
    <ClassCleanup()>
    Public Shared Sub ClassCleanup()
        BaseClassCleanup()
    End Sub

    <TestInitialize()>
    Sub TestInit()
        BaseTestInit()
    End Sub
    <TestCleanup()>
    Public Sub TestCleanup()
        BaseTestCleanup()
    End Sub

    <TestMethod()>
    <Ignore>
    Sub KnownIssuesCompile()
        InitTestMethodAndSetBaseLine()
        Try
            Dim xmlKnownIssuesFile = Path.Combine(memspectInstallDir, "MemSpectBase", "MemSpectApexKnownIssues.xml")
            _VBAssert.OutputText("XML File " + xmlKnownIssuesFile, fAddToBaseline:=False)
            Dim knownIssues = MemSpect.KnownIssues.CreateKnownIssues({xmlKnownIssuesFile})
            _VBAssert.OutputText("KnownIssues Assembly = " + MemSpect.KnownIssues._KnownIssuesAssembly, fAddToBaseline:=False)
            _VBAssert.OutputText("SourceFile compiled = ")
            _VBAssert.OutputText(MemSpect.KnownIssues._KnownIssuesCSharpSourceFile, cSplitChar:=CChar(vbLf))
        Catch ex As Exception
            _VBAssert.OutputText(MemSpect.KnownIssues._KnownIssuesCSharpSourceFile, cSplitChar:=CChar(vbLf))
            HandleTestException(ex)
        End Try
    End Sub


    <TestMethod()>
    <Ignore>
    Sub KnownIssues()
        InitTestMethodAndSetBaseLine()
        Try
            Dim xmlKnownIssuesFile = Path.Combine(memspectInstallDir, "MemSpectBase", "MemSpectApexKnownIssues.xml")
            _VBAssert.OutputText("XML File " + xmlKnownIssuesFile, fAddToBaseline:=False)
            Dim knownIssues = MemSpect.KnownIssues.CreateKnownIssues({xmlKnownIssuesFile})
            _VBAssert.OutputText(MemSpect.KnownIssues._KnownIssuesAssembly, fAddToBaseline:=False)
            _VBAssert.OutputText("get list from asm")
            Dim lstKnownIssues = MemSpect.KnownIssues.CreateKnownIssuesFromAssembly(MemSpect.KnownIssues._KnownIssuesAssembly)
            For Each issue In lstKnownIssues
                _VBAssert.OutputText(issue.ToString())
                _VBAssert.IsNotNull(issue.predicate, "Predicate null for " + issue.ToString)
            Next
            _VBAssert.OutputText("got list from asm")

        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub

    <TestMethod()>
    <Ignore>
    Sub ShowKnownIssues()
        InitTestMethodAndSetBaseLine()
        Try
            _VBAssert.OutputText("ShowKnownIssues")
            AddHandler StatusMessageEvent, Sub(o, e)
                                               _VBAssert.OutputText(e.Message)
                                           End Sub
            Dim x = VBDiagMarginBase.ShowKnownIssues
            Dim br = CType(x.SurfaceDetails.Children(0), Browse)
            _VBAssert.OutputText("# items = " + br._BrowseList.Items.Count.ToString)
        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub

    <TestMethod>
    <Ignore>
    Sub CompileVerb()
        InitTestMethodAndSetBaseLine()
        Try
            AddHandler StatusMessageEvent, Sub(o, e)
                                               _VBAssert.OutputText(e.Message, cSplitChar:=CChar(vbCr))
                                           End Sub
            Dim asmMemSpectBase = GetType(HeapAllocationContainer).Assembly
            For i = 0 To 1
                UpdateStatusMsg(String.Format("Iter {0}", i))
                Dim txt =
    <xml>
// can add the fullpath to an assembly for reference like so:
//Ref: System.dll
//Ref: System.linq.dll
//Ref: System.core.dll
//Ref: MemSpectBase.dll
//Ref: C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\IDE\PrivateAssemblies\Microsoft.VisualStudio.Telemetry.dll
//Ref: C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\IDE\PrivateAssemblies\Microsoft.ApplicationInsights.dll
//Ref: C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\IDE\PrivateAssemblies\Microsoft.ApplicationInsights.UniversalTelemetryChannel.dll
//Ref: C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\IDE\PrivateAssemblies\Microsoft.ApplicationInsights.PersistenceChannel.dll
//Ref: C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\Microsoft.CSharp.dll
//Ref: C:\Windows\Microsoft.NET\assembly\GAC_MSIL\Microsoft.VisualStudio\v4.0_14.0.0.0__b03f5f7f11d50a3a\microsoft.visualstudio.dll
using System;
using MemSpect;
using Microsoft.VisualStudio.Telemetry;

namespace DoesntMatter
{
    public class SomeClass
    {
        public static string DoMain(string[] args)
        {
            var res = new System.Text.StringBuilder();
            res.AppendLine("Executing in dynamically generated code: In Main");
   
            Common.UpdateStatusMsg(res.ToString()); 
            TelemetrySession tsession = null;
            tsession = TelemetryService.DefaultSession;
            tsession.SetSharedProperty("myprop","myval: "+DateTime.Now.ToString());
            var resProp = tsession.GetSharedProperty("myprop");
            res.AppendFormat("Shared Property {0}\r\n", resProp);
            tsession.PostEvent("test event");
            res.AppendFormat("Posted test event\r\n");
            try
            {
                var x = <%= i %>;
                var y = 100/x;
            }
            catch (Exception ex)
            {
                res.AppendFormat("Exception {0}\r\n", ex);
            }
            tsession.Dispose();
            res.AppendFormat("did main<%= i %>\r\n");
            return res.ToString();
        }

    }
}

</xml>.Value.Replace(vbLf, vbCr + vbLf)
                'UpdateStatusMsg(txt)
                'If File.Exists("c:\T.txt") Then
                '    File.Delete("c:\T.txt")
                'End If
                'File.WriteAllText("c:\t.txt", txt)
                Dim codeFileName = WriteOutputToTempFile(txt)
                Dim res = CompileAndExecuteFile(codeFileName)
                UpdateStatusMsg(String.Format("Exec result : {0}", res))
            Next
        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub

End Class
