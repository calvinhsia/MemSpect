Imports Stacks
Imports System.Text.RegularExpressions
Imports System.Xml

Namespace MemSpect
    Public Class PerfViewExport
        Inherits InternStackSource

        Friend Shared Sub Export(ByVal hctrList As List(Of HeapAllocationContainer), Optional ByVal outputFileName As String = "")
            ProcComm.FreezeTarget()
            If String.IsNullOrEmpty(outputFileName) Then
                Dim saveFileDialog = New Windows.Forms.SaveFileDialog With {
                    .DefaultExt = "xml",
                    .FileName = "t.perfview.xml",
                    .Title = "Save PerfView output to "
                }
                Dim flder = IO.Path.GetTempPath
                If Not String.IsNullOrEmpty(My.Settings.PerfViewSaveDir) Then
                    flder = My.Settings.PerfViewSaveDir
                End If
                saveFileDialog.InitialDirectory = flder
                If saveFileDialog.ShowDialog <> Forms.DialogResult.OK Then
                    Return
                End If
                My.Settings.PerfViewSaveDir = IO.Path.GetDirectoryName(saveFileDialog.FileName)
                My.Settings.Save()
                outputFileName = saveFileDialog.FileName
            End If
            Dim inargs = New Dictionary(Of String, Object) From
                {
                    {"outputFileArg", outputFileName},
                    {"inputHctrList", hctrList}
                }
            Dim wfapp = New WorkflowApplication(New PerfViewExportActivity(), inargs)
            wfapp.Run()
            wfapp.OnUnhandledException = Function(ex As WorkflowApplicationUnhandledExceptionEventArgs) As UnhandledExceptionAction
                                             UpdateStatusMsg("Exception: " + ex.ToString())
                                             Return UnhandledExceptionAction.Abort
                                         End Function

            wfapp.Completed = Sub()
                                  UpdateStatusMsg("PerfViewData Exported to " + outputFileName)
                                  DataWindowMain.SetCursor(Cursors.Arrow, fForce:=True)
                              End Sub
        End Sub

        Private Class PerfViewExportActivity
            Inherits MyActivity
            Property outputFileArg As InArgument(Of String)
            Property inputHctrList As InArgument(Of List(Of HeapAllocationContainer))
            Protected Overrides Sub Execute(ByVal context As System.Activities.NativeActivityContext)
                Dim outputFileName = outputFileArg.Get(context)
                Dim hctrList = inputHctrList.Get(context) ' could be null
                PerfViewExportFile(hctrList, outputFileName)
            End Sub
        End Class

        Public Shared Sub PerfViewExportFile(ByVal hCtrList As List(Of HeapAllocationContainer), ByVal outputFileName As String)
            Dim source = New PerfViewExport(hCtrList)
            Dim settings = New XmlWriterSettings With {.Indent = True, .IndentChars = " "}
            Using writer = XmlWriter.Create(outputFileName, settings)
                writer.WriteStartElement("StackWindow")
                XmlStackSourceWriter.WriteStacks(writer, source)
                writer.WriteEndElement()
            End Using

        End Sub

        Private m_emptyModuleIdx As StackSourceModuleIndex
        Friend m_dataDir As String

        Friend Sub New(ByVal hctrList As List(Of HeapAllocationContainer))
            m_emptyModuleIdx = ModuleIntern("")
            Dim stackFrameCache = New Dictionary(Of IntPtr, StackSourceFrameIndex)()

            Dim lamProcessHCtr = Sub(alloc As HeapAllocationContainer)
                                     Dim sample = New StackSourceSample(Me)
                                     sample.Metric = alloc.GetSize
                                     sample.TimeRelMSec = alloc.AllocationStruct.SeqNo
                                     Dim stackIdx = StackSourceCallStackIndex.Invalid
                                     Dim stack = alloc.GetCallStackAddressestoArray
                                     For i = stack.Length - 1 To 0 Step -1
                                         Dim frmAddr = stack(i)
                                         Dim frameIdx As StackSourceFrameIndex
                                         If Not stackFrameCache.TryGetValue(frmAddr, frameIdx) Then
                                             Dim frameName = ResolveAddressToSymbol(frmAddr, fStripFileName:=True, fStripBytesToo:=True)
                                             Dim m = Regex.Match(frameName, "(\S*![^+]*[^ +])")
                                             If m.Success Then
                                                 frameName = m.Groups(1).Value
                                             End If
                                             '                            // Ignore infrastructure parts of the stack.  
                                             'If (frameName.StartsWith("MemSpectDll", StringComparison.OrdinalIgnoreCase)) Then
                                             '    Exit For
                                             'End If
                                             frameIdx = FrameIntern(frameName, m_emptyModuleIdx)
                                             stackFrameCache(frmAddr) = frameIdx
                                         End If
                                         stackIdx = CallStackIntern(frameIdx, stackIdx)
                                     Next ' stack frame

                                     sample.StackIndex = stackIdx
                                     sample.SampleIndex = CType(m_samples.Count, StackSourceSampleIndex)
                                     m_samples.Add(sample)

                                 End Sub
            If hctrList IsNot Nothing Then
                For Each alloc In hctrList
                    lamProcessHCtr.Invoke(alloc)
                Next
            Else
                For Each hp In _HeapList
                    Dim snap = hp.TakeMemSnapshot(fEnableFilter:=True)
                    For Each alloc In snap.Allocs
                        lamProcessHCtr.Invoke(alloc)
                    Next ' allocation
                Next ' heap

            End If
        End Sub
    End Class

End Namespace
