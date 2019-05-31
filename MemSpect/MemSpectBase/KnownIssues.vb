Imports System.Xml.Serialization
Imports System.IO
Imports System.Reflection
Imports System.Text
Imports System.CodeDom.Compiler
Imports System.ComponentModel

Namespace MemSpect

    <XmlRoot(IsNullable:=False)>
    Public Class KnownIssues
        <XmlArray>
        Public Issues As KnownIssue()
        Public Shared _KnownIssuesAssembly As String
        Public Shared _KnownIssuesCSharpSourceFile As String
        Public Shared _KnownIssues As List(Of KnownIssue)
        Public Shared _nPredicatesCalled As Integer

        Public Shared _PropertyBag As New Dictionary(Of String, Object)
        ''' <summary>
        ''' dict of currently found leaks
        ''' </summary>
        Public Shared _DictCurLeaks As New Dictionary(Of IntPtr, HeapAllocationContainer) ' hctr addr to hctr
        ''' <summary>
        ''' cache being built of known issues
        ''' </summary>
        Public Shared _DictKnownIssuesCache As New Dictionary(Of IntPtr, KnownIssue) ' hctr addr to it's known issue

        ''' <summary>
        ''' will create list if not found
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Shared ReadOnly Property Known_Issues As List(Of KnownIssue)
            Get
                If _KnownIssues Is Nothing Then
                    _KnownIssues = CreateKnownIssues(lstFiles:={})
                    'Dim kTestIssue = _KnownIssues.Where(Function(iss) iss.IssueName = "TestIssue").First
                    'kTestIssue.predicate = Function(hctr, stk) As Boolean
                    '                           If hctr.IsMemSpectHeap AndAlso hctr.IsTrkBlkType(TrkType.ClrObjects) Then
                    '                               If Not KnownIssues._PropertyBag.ContainsKey("FirstTime") Then
                    '                                   Common.UpdateStatusMsg("FirstTime KnownIssue predicate processing ClrObj " + hctr.ToString)
                    '                                   KnownIssues._PropertyBag("FirstTime") = 1
                    '                                   If KnownIssues._DictCurLeaks IsNot Nothing Then
                    '                                       Dim knownUndo = KnownIssues._KnownIssues.Where(Function(k) k.IssueName = "UndoTransaction").FirstOrDefault()
                    '                                       Dim que = New Queue(Of HeapAllocationContainer)
                    '                                       ' for all leaked obj found
                    '                                       For Each lkItem In KnownIssues._DictCurLeaks.Values
                    '                                           'if we haven't seem this one before
                    '                                           If Not KnownIssues._DictKnownIssuesCache.ContainsKey(lkItem.GetAddr) Then
                    '                                               Dim className = lkItem.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False)
                    '                                               'is it undo?
                    '                                               If className = "Microsoft.VisualStudio.Editor.Implementation.Undo.OleParentUndoUnit" Then
                    '                                                   Common.UpdateStatusMsg("Got UndoUnit " + hctr.ToString)
                    '                                                   ' add all the things it refs to a queue if its in the leak list
                    '                                                   que.Enqueue(lkItem)
                    '                                               End If
                    '                                           End If
                    '                                       Next
                    '                                       While que.Count > 0
                    '                                           Dim lkItem = que.Dequeue
                    '                                           Dim objrefs = lkItem.GetObjectRefData(NodeType.RefFromParent)(0)
                    '                                           For Each obj In objrefs
                    '                                               ' if it's in the list of leaks
                    '                                               If KnownIssues._DictCurLeaks.ContainsKey(obj.GetAddr) Then
                    '                                                   If Not KnownIssues._DictKnownIssuesCache.ContainsKey(obj.GetAddr) Then
                    '                                                       ' Common.UpdateStatusMsg("Got UndoUnit2 " + obj.ToString())
                    '                                                       que.Enqueue(obj)
                    '                                                       ' and indicate the kind of leak id "undo"
                    '                                                       KnownIssues._DictKnownIssuesCache(obj.GetAddr) = knownUndo
                    '                                                   End If
                    '                                               End If
                    '                                           Next

                    '                                       End While

                    '                                   End If

                    '                               End If
                    '                           End If
                    '                           'if (hctr.IsMemSpectHeap &&
                    '                           '    hctr.TBlk.BlockType == Common.BlockTypes.ClrObject)
                    '                           '{
                    '                           '                      If (!KnownIssues._PropertyBag.ContainsKey("FirstTime")) Then
                    '                           '    {
                    '                           '        Common.UpdateStatusMsg("FirstTime KnownIssue predicate processing ClrObj");
                    '                           '        KnownIssues._PropertyBag["FirstTime"] = 1;
                    '                           '        if (KnownIssues._DictCurLeaks != null)
                    '                           '        {
                    '                           '            var knownUndo = KnownIssues._KnownIssues.Where(k => k.IssueName == "UndoTransaction").FirstOrDefault();
                    '                           '            if (knownUndo != null)
                    '                           '            {
                    '                           '                foreach (var lkItem in KnownIssues._DictCurLeaks.Values)
                    '                           '                {
                    '                           '                                  If (!KnownIssues._DictKnownIssuesCache.ContainsKey(lkItem.GetAddr)) Then
                    '                           '                    {
                    '                           '                        var classname = lkItem.GetClassNameFromHeapCtr(fExpandSystemStringOrArray: false);
                    '                           '                        if (classname == "Microsoft.VisualStudio.Editor.Implementation.Undo.OleParentUndoUnit")
                    '                           '                        {
                    '                           '                            //Common.UpdateStatusMsg("Got UndoUnit");
                    '                           '                            Action<Common.HeapAllocationContainer> act = null; // recursive lambda
                    '                           '                            act = (ctr) =>
                    '                           '                            {
                    '                           '                                var objrefs = ctr.GetObjectRefData(Common.NodeType.RefFromParent)[0];
                    '                           '                                foreach (var obj in objrefs)
                    '                           '                                {
                    '                           '                                          If (KnownIssues._DictCurLeaks.ContainsKey(obj.GetAddr)) Then
                    '                           '                                    {
                    '                           '                                        //Common.UpdateStatusMsg("Got UndoUnit " + obj.ToString());
                    '                           '                                        KnownIssues._DictKnownIssuesCache[obj.GetAddr] = knownUndo;
                    '                           '                                        act(obj);
                    '                           '                                    }
                    '                           '                                }
                    '                           '                            };
                    '                           '                            act(lkItem);
                    '                           '                        }
                    '                           '                    }
                    '                           '                }
                    '                           '            }
                    '                           '        }
                    '                           '    }
                    '                           '}
                    '                           'return false;

                    '                           Return False
                    '                       End Function

                    UpdateStatusMsg("Init known issues # =" + _KnownIssues.Count.ToString)
                End If
                Return _KnownIssues
            End Get
        End Property
        Public Shared Sub ClearKnownIssues()
            _KnownIssues = Nothing
            _PropertyBag.Clear()
            _DictCurLeaks.Clear()
            _DictKnownIssuesCache.Clear()
            _nPredicatesCalled = 0
        End Sub

        Public Const MemSpectApexXmlFileName As String = "MemSpectApexKnownIssues.xml"
        Private Const KnownIssuesGeneratorClassName As String = "KnownIssuesGenerator"

        ''' <summary>
        ''' return NULL if not a known issue
        ''' </summary>
        Public Shared Function GetKnownIssue(
                                            hctr As HeapAllocationContainer,
                                            Optional dictLeaks As Dictionary(Of IntPtr, HeapAllocationContainer) = Nothing
                                          ) As KnownIssue
            'GCData.GetCLRObjectRefDict()
            'Dim f = Known_Issues
            Dim retval As KnownIssue = Nothing
            If _DictKnownIssuesCache.TryGetValue(hctr.GetAddr, retval) Then
            Else
                If dictLeaks IsNot Nothing Then
                    _DictCurLeaks = dictLeaks
                End If
                Dim stk = hctr.GetCallStackAsString
                For Each issue In Known_Issues '.Where(Function(iss) iss.IssueName = "TestIssue")
                    Try
                        _nPredicatesCalled += 1
                        If issue.predicate.Invoke(hctr, stk) Then
                            retval = issue
                            Exit For
                        End If
                    Catch ex As Exception
                        UpdateStatusMsg("Exception processing knownIssue " + issue.IssueName + " " + ex.ToString)
                    End Try
                Next
                _DictKnownIssuesCache(hctr.GetAddr) = retval '' nothing if not found
            End If
            Return retval
        End Function

        ''' <summary>
        ''' Takes a generated assembly and creates a list of known issues
        ''' ready for consumption (compiled predicates)
        ''' Pass in name of assembly
        ''' Assembly has fullpaths of original XML files embedded
        ''' If xml files not found, will look in curdir or snapshot path _OfflineDataFilePath
        ''' </summary>
        ''' <param name="strAsm">full path to generated assembly</param>
        ''' <returns>list of known issues</returns>
        ''' <remarks></remarks>
        Public Shared Function CreateKnownIssuesFromAssembly(strAsm As String) As List(Of KnownIssue)
            Dim lstKnownIssues = New List(Of KnownIssue)
            If Not File.Exists(strAsm) Then
                UpdateStatusMsg("Could not find " + strAsm)
                strAsm = Path.Combine(
                    Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
                    Path.GetFileName(strAsm))
                UpdateStatusMsg("Looking in " + strAsm)
            End If
            If File.Exists(strAsm) Then
                Dim asm = Assembly.LoadFrom(strAsm)
                Dim myClassType = asm.GetExportedTypes().Where(Function(t) t.Name = KnownIssuesGeneratorClassName).Single()
                Dim fld = myClassType.GetField("_listXmlFiles")
                Dim myClassInstance = Activator.CreateInstance(myClassType)

                Dim lstXmlFiles = CType(fld.GetValue(myClassInstance), List(Of String))
                Dim files = ProcessXmlFiles(lstXmlFiles, lstKnownIssues)

                Dim meth = myClassType.GetMethod("CompileIssues")
                Dim result = CType(meth.Invoke(myClassInstance, {lstKnownIssues}), List(Of KnownIssue))
                lstKnownIssues = result
            Else
                UpdateStatusMsg("Could not find " + strAsm)
            End If
            Return lstKnownIssues
        End Function

        Private Shared Function ProcessXmlFiles(lstFiles As IEnumerable(Of String), lstKnownIssues As List(Of KnownIssue)) As String
            Dim strBuilderFiles = New StringBuilder()
            For Each fname In lstFiles
                ' like @"C:\MemSpect\MemSpectBase\MemSpectApexKnownIssues.xml",
                ' beware: paths could change (different machine)
                strBuilderFiles.AppendLine(String.Format("@""{0}"",", fname))  'trailing extra comma ok
                UpdateStatusMsg("KnownIssuesXML: " + fname)
                If Not File.Exists(fname) Then
                    UpdateStatusMsg("Ignoring: (File not found) " + fname)
                Else
                    Using fstream = New FileStream(fname, FileMode.Open, FileAccess.Read)
                        Dim xmlSerializer = New XmlSerializer(GetType(KnownIssues))
                        Dim f = CType(xmlSerializer.Deserialize(fstream), KnownIssues)
                        For Each issue In f.Issues
                            issue.SampleCallStack = String.Empty ' no point in storing this guy
                            If Not lstKnownIssues.Contains(issue) Then
                                lstKnownIssues.Add(issue)
                            Else
                                'UpdateStatusMsg("Issue already added " + issue.ToString)
                            End If
                        Next
                    End Using

                End If
            Next
            Return strBuilderFiles.ToString
        End Function
        ''' <summary>
        ''' </summary>
        ''' <param name="lstFiles">list of input files</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Shared Function CreateKnownIssues(lstFiles As IEnumerable(Of String)) As List(Of KnownIssue)
            Dim lstFilesToUse = New List(Of String)(lstFiles)
            Dim lstKnownIssues As New List(Of KnownIssue)
            '                    //see  http://blogs.msdn.com/b/calvin_hsia/archive/2013/02/27/10398012.aspx
            ' add curdir first: dupes are weeded out
            Dim xmlFile = Path.Combine(
               Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly.Location),
               MemSpectApexXmlFileName
                )
            lstFilesToUse.Add(xmlFile)


            If lstFiles.Count = 0 Then
                ' look in curdir first: takes precedence
                If _offlineSnapshot IsNot Nothing AndAlso Not String.IsNullOrEmpty(_offlineSnapshot._DataFilePath) Then
                    Debug.Assert(_ConnectionMode = MemSpectMode.Offline)
                    Dim pathOffline = Path.Combine(_offlineSnapshot._DataFilePath, "MemSpect")
                    If Directory.Exists(pathOffline) Then
                        For Each additionalKnownIssueFile In Directory.GetFiles(
                            pathOffline, "MemSpectApexKnown*.xml")
                            lstFilesToUse.Add(additionalKnownIssueFile)
                        Next
                    End If
                End If

            Else

            End If
            Dim strFiles = ProcessXmlFiles(lstFilesToUse, lstKnownIssues)

            Dim strBuilderPreds = New StringBuilder()
            For Each issue In lstKnownIssues ' .Take(2)
                Dim strPred = issue.PredicateString.Trim()
                If (String.IsNullOrEmpty(strPred)) Then
                    strPred = "return false;"
                End If

                Dim strTemp = String.Format(
                    "lstResult.Add((Common.HeapAllocationContainer hctr, string stk) =>{{ {0} }} );",
                    strPred)
                strBuilderPreds.AppendLine(String.Format("//{0}", issue.ToString()))
                strBuilderPreds.AppendLine(strTemp)
                strBuilderPreds.AppendLine()
            Next
            _KnownIssuesCSharpSourceFile =
<xml>
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using MemSpect;

namespace DoesntMatter
{
  public class <%= KnownIssuesGeneratorClassName %>
  {
    public static List&lt;string&gt; _listXmlFiles = new List&lt;string&gt;()
    {
<%= strFiles.ToString() %>
    };
    public List&lt;KnownIssue&gt; CompileIssues( List&lt;KnownIssue&gt; lstIssues)
    {
        var lstCompiled = CompileIssuePredicates();
        int nCnt = 0;
        foreach (var issue in lstIssues)
        {
            issue.predicate = lstCompiled[nCnt++];
        }
        return lstIssues;
    }

    public List&lt;Func&lt;Common.HeapAllocationContainer, string, bool&gt;&gt; CompileIssuePredicates()
    {
        var lstResult = new List&lt;Func&lt;Common.HeapAllocationContainer,string,bool&gt;&gt;();
<%= strBuilderPreds.ToString %>
        return lstResult;
    }
  }
}
</xml>.Value '.Replace(vbLf, vbCrLf)
            Dim cdprovider = CodeDomProvider.CreateProvider("C#")
            Dim compileParms = New CompilerParameters()
            compileParms.ReferencedAssemblies.Add("System.dll")
            compileParms.ReferencedAssemblies.Add("System.linq.dll")
            compileParms.ReferencedAssemblies.Add("System.core.dll")
            Dim memspectBaseasm = Assembly.GetAssembly(GetType(HeapAllocationContainer)).Location
            compileParms.ReferencedAssemblies.Add(memspectBaseasm)
            compileParms.ReferencedAssemblies.Add(Assembly.GetAssembly(GetType(FastSerialization0.Serializer)).Location)
            compileParms.GenerateInMemory = False
            _KnownIssuesAssembly = Path.Combine(
                     Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
                     "MemSpectApexKnownIssues.dll")
            If (File.Exists(_KnownIssuesAssembly)) Then
                Try
                    File.Delete(_KnownIssuesAssembly)
                Catch ex As Exception
                    UpdateStatusMsg("Warning: Generating InMemory assembly because Error deleting " + _KnownIssuesAssembly + " " + ex.ToString)
                    compileParms.GenerateInMemory = True
                    _KnownIssuesAssembly = String.Empty
                End Try
            End If
            compileParms.OutputAssembly = _KnownIssuesAssembly

#If DEBUG Then
                    compileParms.IncludeDebugInformation = true
#End If '//DEBUG

            Dim resCompile = cdprovider.CompileAssemblyFromSource(
                compileParms,
                _KnownIssuesCSharpSourceFile)
            If (resCompile.Errors.HasErrors) Then
                Throw New InvalidOperationException(
                    String.Format("Compile error: # Errs={0}  {1}",
                    resCompile.Errors.Count.ToString(),
                    resCompile.Errors(0).ToString())
                    )

            Else
                Dim myClassType = resCompile.CompiledAssembly.GetExportedTypes().Where(Function(t) t.Name = KnownIssuesGeneratorClassName).Single()
                Dim meth = myClassType.GetMethod("CompileIssues")
                Dim myClassInstance = Activator.CreateInstance(myClassType)
                Dim result = CType(meth.Invoke(myClassInstance, {lstKnownIssues}), List(Of KnownIssue))
            End If
            Return lstKnownIssues
        End Function

    End Class

    '// to gen xsd
    '// xsd "C:\Users\calvinh\Documents\Visual Studio 12\Projects\ApexTest\MemSpectApex\bin\Debug\MemSpectApex.dll" /type:KnownIssues
    '// replace minoccurs with "1"
    '// to gen xml from xsd Menu-XML-Use schema, use intellisense, hit tab to trigger snippet input
    Public Class KnownIssue
        <XmlAttribute>
        Public IssueName As String
        <XmlAttribute>
        <DefaultValue("")>
        Public BugId As String
        <XmlAttribute>
        Public FailTestWhenFound As Boolean
        <XmlAttribute(DataType:="date")>
        Public Created As DateTime ' ; //"2013-02-01"
        Public IssueDescription As String
        Public Scenario As String '//; // how it's encountered
        Public EstimatedSize As String
        Public PredicateString As String
        Public SampleCallStack As String

        <XmlIgnore>
        Public predicate As Func(Of HeapAllocationContainer, String, Boolean)

        Public Overrides Function ToString() As String
            Return String.Format("{0} {1} BugId={2}",
                IssueName,
                IssueDescription,
                BugId
                )
        End Function

        Public Overrides Function GetHashCode() As Integer
            Return String.Concat(IssueName, IssueDescription, BugId).GetHashCode()
        End Function

        Public Overrides Function Equals(obj As Object) As Boolean
            If (ReferenceEquals(Me, obj)) Then
                Return True
            End If
            Dim other = TryCast(obj, KnownIssue)
            If (other IsNot Nothing AndAlso
                String.Equals(IssueName + IssueDescription + BugId, other.IssueName + other.IssueDescription + other.BugId)) Then
                Return True
            End If

            Return False
        End Function
    End Class

End Namespace