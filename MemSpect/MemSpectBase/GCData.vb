Imports System.Runtime.InteropServices

Namespace MemSpect

    Public Class GCData
        Friend Shared _GCRootDict As New Dictionary(Of IntPtr, List(Of HeapAllocationContainer))
        ' section 2: the objects and their refs
        Friend Const nSizeOfClrObjDump = 8 ' objectid, nRefs

        Friend Shared _ClrObjRefDict As Dictionary(Of IntPtr, ClrObjDump)

        Public Shared Function AddGCRoot(ByVal hctrGCRoot As HeapAllocationContainer) As Boolean
            Dim lst As List(Of HeapAllocationContainer) = Nothing
            If Not _GCRootDict.TryGetValue(hctrGCRoot.AllocationStruct.Address, lst) Then
                lst = New List(Of HeapAllocationContainer)
                _GCRootDict(hctrGCRoot.AllocationStruct.Address) = lst
            End If
            lst.Add(hctrGCRoot)
            Return True
        End Function

        ' could return nothing
        ''' <summary>
        ''' Return either a list of GCRoots for a given hctr or all theGCRoots
        ''' could return nothing
        ''' </summary>
        Public Shared Function GetGCRootInfo(
                                     Optional ByVal hctr As HeapAllocationContainer = Nothing,
                                     Optional ByVal fIncludeDupes As Boolean = True) As List(Of HeapAllocationContainer)
            Dim lst As List(Of HeapAllocationContainer) = Nothing

            CLRObjRefsWrapper.WaitTilLoaded() ' for offline/online
            If _ClrObjRefDict Is Nothing Then
                Debug.Assert(_ConnectionMode <> MemSpectMode.Offline, "_ClrObjRefDict not filled?")
                If _ConnectionMode = MemSpectMode.OnLine Then
                    GetCLRObjectRefDict()
                End If
            End If
            Debug.Assert(_ClrObjRefDict IsNot Nothing, "Didn't get ClrObjRefDict")
            If hctr Is Nothing Then
                lst = New List(Of HeapAllocationContainer)
                For Each lstGCRoots In _GCRootDict.Values
                    If fIncludeDupes Then
                        lst.AddRange(lstGCRoots)
                    Else
                        lst.Add(lstGCRoots(0))
                    End If
                Next
            Else
                If Not _GCRootDict.TryGetValue(hctr.AllocationStruct.Address, lst) Then
                    'return null lst
                End If
            End If
            Return lst
        End Function

        Public Shared Function GetGCRootInfoFromAddr(ByVal addr As IntPtr) As HeapAllocationContainer
            Dim h As HeapAllocationContainer = Nothing
            Dim lsts As List(Of HeapAllocationContainer) = Nothing
            If _GCRootDict.TryGetValue(addr, lsts) Then
                h = lsts(0)
            End If
            Return h
        End Function

        Public Shared Sub ClearData()
            If _GCRootDict.Count > 0 Then
                UpdateStatusMsg("Clearing CLR Obj Dump")
                _GCRootDict.Clear()
            End If
            If _ClrObjRefDict IsNot Nothing Then
                _ClrObjRefDict = Nothing
            End If
        End Sub


        '' Get the entire CLR Obj graph: Dict<Object, objs reffed by it>
        '' works on/offline
        Public Shared Function GetCLRObjectRefDict() As Dictionary(Of IntPtr, ClrObjDump)
            CLRObjRefsWrapper.WaitTilLoaded() ' for offline/online
            If _ClrObjRefDict Is Nothing Then
                Debug.Assert(_ConnectionMode <> MemSpectMode.Offline, "_ClrObjRefDict not filled ?")
                If _ConnectionMode = MemSpectMode.OnLine Then
                    _ClrObjRefDict = New Dictionary(Of IntPtr, ClrObjDump)
                    Dim hpMemSpect = (From h In _HeapList Where h.IsMemSpectHeap).First
                    Try
                        UpdateStatusMsg("Getting CLR Obj Dump")

                        SendMsg(ProcMsgVerb.GetClrObjDump, fSendEndMsgSync:=False, dwords:={0})
                        ' first section is (possibly many bunches of) GCRoots. Each starts with a count(N) followed by N instances of GCRootData
                        ' If N==0, that's the end of the GCRoot section.
                        ' 2nd section is pairs of ObjectId, nRefs
                        Dim fGCRootsFinished = False
                        Dim nSizeofGCRootData = 4 * 4
                        Do While Not fGCRootsFinished
                            Dim result = GetMsg(4)
                            If result.Length = 4 Then
                                Dim nGCRootsThisBatch = BitConverter.ToInt32(result, 0)
                                If nGCRootsThisBatch = 0 Then
                                    fGCRootsFinished = True
                                    Exit Do
                                End If
                                For i = 0 To nGCRootsThisBatch - 1
                                    result = GetMsg(nSizeofGCRootData)
                                    If result.Length <> nSizeofGCRootData Then
                                        UpdateStatusMsg("gcroot batch error", fAssert:=True)
                                    End If
                                    Dim hblkAddr = New IntPtr(BitConverter.ToInt32(result, 0 * 4))
                                    If (hblkAddr <> IntPtr.Zero) Then 'rootreferences2 passes in0
                                        Dim hctnr = MakeHeapAllocationContainerFromAllocationStructAddr(hblkAddr, hpMemSpect)
                                        If hctnr IsNot Nothing Then
                                            Dim GCRootKind = BitConverter.ToInt32(result, 1 * 4)
                                            Dim GCRootFlags = BitConverter.ToInt32(result, 2 * 4)
                                            Dim RootId = New IntPtr(BitConverter.ToInt32(result, 3 * 4))
                                            'overload right/left:  make tblk struct same as GCRoots elsewhere
                                            hctnr._tblkContainer._Tblk.Right = New IntPtr((GCRootFlags << 16) + GCRootKind)
                                            hctnr._tblkContainer._Tblk.Left = RootId
                                            hctnr._tblkContainer._Tblk.Parent = CType(TBLK_SIGNATURE, IntPtr)
                                            AddGCRoot(hctnr)
                                        End If
                                    End If
                                Next
                            Else
                                fGCRootsFinished = True
                            End If
                        Loop

                        If _GCRootDict.Count > 0 Then ' if we got some gcroots, then we know there are some objs to get too

                            'note: can't do anything that causes interproc comm during GetObjDump
                            Dim aloc As New HeapAllocationStruct

                            Do While True
                                Dim result = GetMsg(nSizeOfClrObjDump)
                                If result.Length <> nSizeOfClrObjDump Then
                                    If result.Length = 1 AndAlso result(0) = ProcMsgVerb.VerbDone Then
                                        Exit Do '  finished
                                    End If
                                    UpdateStatusMsg("obj dump data err", fAssert:=True)
                                    Exit Do
                                End If
                                Dim hblkAddr = New IntPtr(BitConverter.ToInt32(result, 0 * 4))
                                If hblkAddr <> IntPtr.Zero Then
                                    Dim hctnr = MakeHeapAllocationContainerFromAllocationStructAddr(hblkAddr, hpMemSpect)
                                    Dim clrObjDmp As New ClrObjDump With
                                        {
                                            .hctnr = hctnr
                                        }
                                    Dim nRefs = BitConverter.ToInt32(result, 1 * 4)
                                    If nRefs > 0 Then
                                        result = GetMsg(nRefs * 4)
                                        Dim dwBytesRead = 0
                                        For i = 0 To result.Length - 1 Step 4
                                            Dim ref = New IntPtr(BitConverter.ToInt32(result, i))
                                            If ReadProcessMemoryHeapAllocationStruct(_hProcessTarget, ref, aloc, Marshal.SizeOf(aloc), dwBytesRead) = 0 Then
                                                UpdateStatusMsg(String.Format("ReadProcMem for obj dump err {0} {1:x8}", i, ref.ToInt32), fAssert:=True)
                                            Else
                                                clrObjDmp.Refs.Add(aloc.Address)
                                            End If
                                        Next
                                    End If
                                    _ClrObjRefDict.Add(clrObjDmp.hctnr.AllocationStruct.Address, clrObjDmp)
                                End If
                            Loop
                        End If
                        UpdateStatusMsg("Done gettng CLR Obj Dump")
                    Catch ex As Exception
                        Common.MemSpectExceptionHandler(ex)
                    Finally
                        _IsInMiddleOfMessaging = ProcMsgVerb.UnKnownVerb
                    End Try
                End If
            End If
            Debug.Assert(_ClrObjRefDict IsNot Nothing, "Didn't get ClrObjRefDict")
            Return _ClrObjRefDict
        End Function




    End Class

End Namespace
