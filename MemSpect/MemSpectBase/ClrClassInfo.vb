Imports System.Collections.ObjectModel
Imports FastSerialization0
Imports System.Runtime.InteropServices

Namespace MemSpect

    Public Class ClrClassInfo
        Inherits SerializableObject

        ' class name and layout stored here keyed by ClsId
        Private Shared _g_DictClassLayouts As New Dictionary(Of IntPtr, ClassLayoutDetail) ' key ClsId

#If DEBUG Then
        Private Shared _IsInGetClassLayouts As Boolean = False
#End If
        Public Shared Property g_DictClassLayouts As Dictionary(Of IntPtr, ClassLayoutDetail) ' key ClsId
            Get
                Return _g_DictClassLayouts
            End Get
            Set(ByVal value As Dictionary(Of IntPtr, ClassLayoutDetail))
                _g_DictClassLayouts = value
            End Set
        End Property
        '        Public Shared g_DictClassLayouts2 As New MyDictClassLayout


        'Public Enum InheritsFromTypes
        '    EventHandler ' System.EventHandler
        'End Enum

        'Friend Shared InheritsFromTypeDict As New Dictionary(Of String, IntPtr) ' these never change once a process starts

        'Public Shared Function InheritsFrom(ByVal clsId As IntPtr, ByVal typ As InheritsFromTypes) As Boolean
        '    Dim fInheritsFrom = False
        '    Dim clsIdBase = IntPtr.Zero

        '    Dim clsinfo = _g_DictClassLayouts(clsId)


        '    If InheritsFromTypeDict.ContainsKey(typ.ToString) Then
        '        clsIdBase = InheritsFromTypeDict(typ.ToString)
        '    Else
        '        clsIdBase = (From a In _g_DictClassLayouts.Values
        '                     Select a.classId
        '                   ).First

        '    End If
        '    Return fInheritsFrom
        'End Function

        Public Shared Sub GetAllClassInfos(Optional ByVal memspectHeapSnap As MemSnapshot = Nothing)
            If _ConnectionMode = MemSpectMode.OnLine Then
                UpdateStatusMsg(" -Collecting CLR Class Info ")
                If memspectHeapSnap Is Nothing Then
                    memspectHeapSnap = (From hp In _HeapList Where hp.IsMemSpectHeap).First.TakeMemSnapshot(fEnableFilter:=False)
                End If
                ' we want to iterate all classes to make sure we have all layouts in dict
                For Each alloc In memspectHeapSnap.Allocs.Where(Function(o) o.TBlkBlockType = BlockTypes.ClrObject)
                    Dim layouts = ClrClassInfo.GetClassLayoutsFromClassId(CType(alloc.TBlk.UnionData1, IntPtr))
                    For Each layout In layouts
                        Dim cname = ClrClassInfo.GetClassNameFromClassOrObjectId(layout.classId, fExpandSystemStringOrArray:=False)
                        For Each layoutdet In layout.dictFieldInfo
                            If "Class".Contains(layoutdet.Value.FldType) AndAlso layoutdet.Key > 0 AndAlso layout.classSize > 4 Then
                                Dim offsetAddr = alloc.GetAddr.MyAdd(layoutdet.Key)
                                Dim dwValue = ReadProcessMemoryDWORDEx(offsetAddr)
                                If dwValue <> 0 Then
                                    Dim clsname = ClrClassInfo.GetClassNameFromClassOrObjectId(IntPtr.Zero, CType(dwValue, IntPtr), fExpandSystemStringOrArray:=False)
                                End If
                            End If
                        Next
                    Next
                Next
            End If
        End Sub

        ''' <summary>
        ''' Given a classid get the class name. If we only have an objectid, set classid to null and it will be retrieved
        ''' </summary>
        ''' <param name="clsId">can be zero</param>
        ''' <param name="ObjectId">defaults to zero. Must be valid when clsid =0
        ''' <paramref name="fExpandSystemStringOrArray"> </paramref> </param>
        ''' <param name="fExpandSystemStringOrArray">include array/string expansion in name</param>
        Public Shared Function GetClassNameFromClassOrObjectId(ByVal clsId As IntPtr,
                                                Optional ByVal ObjectId As IntPtr = Nothing,
                                                Optional ByVal fExpandSystemStringOrArray As Boolean = False,
                                                Optional ByVal fAllowInvalidData As Boolean = False) As String
            Dim sRetval = String.Empty
#If DEBUG Then ' need conditional because not defined in retail
            Debug.Assert(Not _IsInGetClassLayouts, "GetClassNameFromClassId while getting layouts?")
#End If
            If clsId = IntPtr.Zero Then
                If ObjectId = IntPtr.Zero Then
                    Return sRetval
                End If
                clsId = CType(ReadProcessMemoryDWORDEx(ObjectId), IntPtr)
                If Not fAllowInvalidData Then
                    Debug.Assert(clsId <> IntPtr.Zero, "zeroc clsid from objid?")
                End If
                If clsId = IntPtr.Zero Then
                    Return sRetval
                End If
            End If
            Dim clsLayoutDet As ClassLayoutDetail = Nothing
            Dim fDidExpandString = False
            If ClrClassInfo.g_DictClassLayouts.TryGetValue(clsId, clsLayoutDet) Then
                sRetval = clsLayoutDet.className
                If fExpandSystemStringOrArray And sRetval = "System.String" Then
                    Dim stringContents As String = String.Empty
                    If _ConnectionMode = MemSpectMode.OnLine Then
                        SendMsg(ProcMsgVerb.GetClassNameFromId, fSendEndMsgSync:=True, dwords:={clsId.ToInt32, ObjectId.ToInt32, 1})
                        sRetval = ReadSharedMemAsString(fAnsi:=False)
                        fDidExpandString = True
                    Else
                        If Common._offlineSnapshot._systemStringDictionary.TryGetValue(ObjectId, stringContents) Then
                            sRetval += " " + stringContents
                            fDidExpandString = True
                        End If
                    End If
                End If
            Else
                If _ConnectionMode = MemSpectMode.OnLine Then ' could be offline
                    Dim fAddNewClassInfo = False
                    Debug.Assert(Not fExpandSystemStringOrArray OrElse ObjectId <> IntPtr.Zero, "if we want to expand, need instance")
                    If Not fExpandSystemStringOrArray Then
                        'only store class name, not expanded array or string
                        'the Address is the ObjectId for String Expansion
                        'get the basic class name
                        SendMsg(ProcMsgVerb.GetClassNameFromId, fSendEndMsgSync:=True, dwords:={clsId.ToInt32, ObjectId.ToInt32, 0})
                        sRetval = ReadSharedMemAsString(fAnsi:=False)
                        fAddNewClassInfo = True
                    Else
                        SendMsg(ProcMsgVerb.GetClassNameFromId, fSendEndMsgSync:=True, dwords:={clsId.ToInt32, ObjectId.ToInt32, 1})
                        sRetval = ReadSharedMemAsString(fAnsi:=False)
                        If sRetval.StartsWith("System.String") Then
                            fDidExpandString = True
                        Else
                            fAddNewClassInfo = True
                        End If
                    End If
                    If fAddNewClassInfo Then
                        ' cache the basic class name
                        clsLayoutDet = ClassLayoutDetail.CreateClassLayoutDetail(sRetval, clsId)
                    End If
                End If
            End If
            If fExpandSystemStringOrArray AndAlso Not fDidExpandString AndAlso ObjectId <> IntPtr.Zero Then
                Try

                    Dim nEntries = 0

                    Dim lamEventHandler = Sub()
                                              Dim objIdTarg = CType(ReadProcessMemoryDWORDEx(ObjectId.MyAdd(4)), IntPtr)
                                              Dim ehTarg = String.Empty
                                              If objIdTarg <> IntPtr.Zero Then
                                                  If ObjectId <> objIdTarg Then ' Eventhandler refers to self
                                                      ehTarg = ClrClassInfo.GetClassNameFromClassOrObjectId(clsId:=IntPtr.Zero, ObjectId:=objIdTarg, fExpandSystemStringOrArray:=True)
                                                  Else
                                                      ehTarg = "<self>"
                                                  End If
                                              End If
                                              sRetval += String.Format("(Target={0} 0x{1:x8})", ehTarg, objIdTarg.ToInt32) ' so can sort by target

                                          End Sub
                    If sRetval.StartsWith("System.") AndAlso Not sRetval.EndsWith("]") Then ' we don't want array of collections
                        Dim systype = sRetval.Substring(7)
                        If Not clsLayoutDet._fDidTryGettingClassLayouts Then
                            ClrClassInfo.GetClassLayoutsFromClassId(clsLayoutDet.classId)
                        End If
                        If systype.StartsWith("Collections.") Then
                            Dim colType = sRetval.Substring(19)
                            If colType = "HashTable" Then
                                Dim offset_count = 28
                                Select Case clsLayoutDet.classSize
                                    Case 52
                                        offset_count = 24
                                End Select
                                nEntries = ReadProcessMemoryDWORDEx(ObjectId.MyAdd(offset_count))
                                sRetval += "(Count=" + nEntries.ToString + ")"
                            ElseIf colType = "ArrayList" Then
                                nEntries = ReadProcessMemoryDWORDEx(ObjectId.MyAdd(12))
                                sRetval += "(Count=" + nEntries.ToString + ")"
                            ElseIf colType.StartsWith("Generic.") Then
                                If Not colType.EndsWith("]") Then ' we don't want the array, just the base type
                                    Dim genType = sRetval.Substring(27)
                                    Try
                                        If genType.StartsWith("Dictionary`2") Then '     System.Collections.Generic.Dictionary`2<System.String,System.TimeZoneInfo>

                                            'we want to disting between the Dictionary itself and the ValueCollection in the Dictionary (size 12)
                                            'Dictionary`2<System.Int32,Microsoft.VisualStudio.TeamFoundation.WorkItemTracking.UIWorkItem>.ValueCollection<System.Int32,Microsoft.VisualStudio.TeamFoundation.WorkItemTracking.UIWorkItem>
                                            If Not genType.Contains("ValueCollection") AndAlso Not genType.Contains("KeyCollection") Then
                                                ' count(32) - freeCount (44)
                                                Dim offset_count = 0 '= clsLayoutDet.GetOffsetOfField("count")
                                                Dim offset_freecount = 0 '= clsLayoutDet.GetOffsetOfField("freeCount")
                                                Select Case clsLayoutDet.classSize
                                                    Case 52
                                                        offset_count = 32
                                                        offset_freecount = 44
                                                    Case 48
                                                        offset_count = 28
                                                        offset_freecount = 40

                                                End Select
                                                nEntries = ReadProcessMemoryDWORDEx(ObjectId.MyAdd(offset_count)) - ReadProcessMemoryDWORDEx(ObjectId.MyAdd(offset_freecount))
                                                sRetval += "(Count=" + nEntries.ToString + ")"
                                            End If
                                        ElseIf genType.StartsWith("List`1") OrElse genType.StartsWith("Stack`1") Then
                                            ' _size = 12
                                            nEntries = ReadProcessMemoryDWORDEx(ObjectId.MyAdd(12))
                                            sRetval += "(Count=" + nEntries.ToString + ")"
                                        ElseIf genType.StartsWith("HashSet`1") OrElse genType.StartsWith("Queue`1") OrElse genType.StartsWith("TreeSet`1") Then
                                            'm_count = 20
                                            nEntries = ReadProcessMemoryDWORDEx(ObjectId.MyAdd(20))
                                            sRetval += "(Count=" + nEntries.ToString + ")"
                                        Else
                                            Dim r = 4
                                        End If

                                    Catch ex As Exception
                                        Debug.Assert(False, sRetval + vbCrLf + ex.ToString)
                                        'swallow any exceptions
                                    End Try
                                End If
                            ElseIf colType.StartsWith("Concurrent.ConcurrentDictionary") AndAlso
                                Not colType.Contains(">+System.Collections.Concurrent.ConcurrentDictionary`2<") AndAlso
                                Not colType.Contains(">.Node<") Then
                                ' distinguish between dict and table:
                                '    System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.String>
                                '    System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.String>+System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.String>.Node<System.String,System.String>[](Count=31)(CMOD_REQD) 0x032ec7e8
                                Dim layouts = GetClassLayoutsFromClassId(clsId)
                                Debug.Assert(layouts.Count = 2, "layout for concurdict should have no inheritance " + layouts.Count.ToString) ' system.object only
                                If layouts.Count = 2 Then
                                    Dim offset_m_countPerLock = 0
                                    Dim objid_count = ObjectId
                                    Debug.Assert(objid_count <> IntPtr.Zero, "objid 0 for concurdict?")
                                    Dim res = layouts(1).dictFieldInfo(4)
                                    If res.FldName = "m_tables" Then ' dev11 concurdict 

#If False Then
{ Address = 048310dc, SeqNo = 235746, Size = 36, Thread = 4924M, Gen = 2, Moved = 4, Srviv = 19, Classid = 72fc1498, ClassName = System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.Object>, NumItems = 0, _HeapAllocationContainer = Address=0x048310dc, SeqNo=235,746, Size=36, BlkType=ClrObject Thread=4924M }  Heap = __MemSpect
ClassId = 0x72fc1498 System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.Object>
GCGen = 2 GCMoved=4 GCSurvived =19 
   4 System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.Object>.m_tables   System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.Object>.Tables<System.String,System.Object>(CMOD_REQD) 0x048312e8
   8 System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.Object>.m_comparer   System.Collections.Generic.GenericEqualityComparer`1<System.String>(System.Collections.Generic.IEqualityComparer`1<arg # 0 9>) 0x047b42f4
  12 System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.Object>.m_serializationArray   Array 0x00000000
  16 System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.Object>.m_budget   I4 0x00000001
  20 System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.Object>.m_serializationConcurrencyLevel   I4 0x00000000
  24 System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.Object>.m_serializationCapacity   I4 0x00000000
  28 System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.Object>.m_growLockArray   Boolean 0x00000001

048310dc : 72fc1498 048312e8 047b42f4 00000000 00000001 00000000 00000000 00000001   98 14 fc 72 e8 12 83 04 f4 42 7b 04 00 00 00 00 01 00 00 00 00 00 00 00 00 00 00 00 01 00 00 00      r     B{                     
048310fc : 00000000                                                                  00 00 00 00                                                                                           

{ Address = 048312e8, SeqNo = 235768, Size = 20, Thread = 4924M, Gen = 2, Moved = 4, Srviv = 19, Classid = 73399ab8, ClassName = System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.Object>.Tables<System.String,System.Object>, NumItems = 0, _HeapAllocationContainer = Address=0x048312e8, SeqNo=235,768, Size=20, BlkType=ClrObject Thread=4924M }  Heap = __MemSpect
ClassId = 0x73399ab8 System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.Object>.Tables<System.String,System.Object>
GCGen = 2 GCMoved=4 GCSurvived =19 
   4 System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.Object>.Tables<System.String,System.Object>.m_buckets   System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.Object>+System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.Object>.Node<System.String,System.Object>[](Count=31)(Array) 0x0483125c
   8 System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.Object>.Tables<System.String,System.Object>.m_locks   System.Object[](Count=16)(Array) 0x04831100
  12 System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.Object>.Tables<System.String,System.Object>.m_countPerLock   I4[](Count=16)(CMOD_REQD) 0x04831210

048312e8 : 73399ab8 0483125c 04831100 04831210 00000000                              b8 9a 39 73 5c 12 83 04 00 11 83 04 10 12 83 04 00 00 00 00                                         9s\              


#End If
                                        offset_m_countPerLock = 12
                                        objid_count = New IntPtr(ReadProcessMemoryDWORDEx(ObjectId.MyAdd(4))) ' 048312e8 in the example
                                    Else
                                        'dev10

#If False Then
   4 System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.String>.m_buckets   System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.String>+System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.String>.Node<System.String,System.String>[](Count=31)(CMOD_REQD) 0x032ec7e8
   8 System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.String>.m_locks   System.Object[](Count=16)(Array) 0x032ec68c
  12 System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.String>.m_countPerLock   I4[](Count=16)(CMOD_REQD) 0x032ec79c
  16 System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.String>.m_comparer   System.Collections.Generic.GenericEqualityComparer`1<System.String>(System.Collections.Generic.IEqualityComparer`1<arg # 0 a>) 0x03253118
  20 System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.String>.m_serializationArray   Array 0x00000000
  24 System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.String>.m_serializationConcurrencyLevel   I4 0x00000000
  28 System.Collections.Concurrent.ConcurrentDictionary`2<System.String,System.String>.m_serializationCapacity   I4 0x00000000

#End If
                                        Debug.Assert(res.FldName = "m_buckets")
                                        offset_m_countPerLock = 12 ' both happen to be offset 12
                                    End If
                                    Dim addr_m_countPerLock = New IntPtr(ReadProcessMemoryDWORDEx(objid_count.MyAdd(offset_m_countPerLock)))

                                    ' now we have the address of the m_countPerLock int array
                                    Dim sizeArray = ReadProcessMemoryDWORDEx(addr_m_countPerLock.MyAdd(4))
                                    Dim nCount = 0
                                    For i = 0 To sizeArray - 1
                                        nCount += ReadProcessMemoryDWORDEx(addr_m_countPerLock.MyAdd(8 + i * IntPtr.Size)) ' skip clsid and count
                                    Next
                                    sRetval += String.Format("(Count={0})", nCount)
                                End If
                            End If
                            'ElseIf systype = "RuntimeType" Then
                            '    Dim rtHandle = ObjectId.MyAdd(16)
                            '    Dim rttype = String.Empty
                            '    Dim rtaddr = IntPtr.Zero
                            '    If rtHandle <> IntPtr.Zero Then
                            '        rtaddr = ReadProcessMemoryDWORDEx(rtHandle)
                            '        If rtaddr <> IntPtr.Zero Then
                            '            rttype = ResolveAddressToSymbol(rtaddr)
                            '        End If
                            '    End If
                            '    sRetval += String.Format("Type= {0:x8} {1}", rtaddr.ToInt32, rttype)
                        ElseIf systype = ("WeakReference") OrElse systype = "Windows.Controls.Image.WeakBitmapSourceEvents" Then
                            Dim wrHand = CType(ReadProcessMemoryDWORDEx(ObjectId.MyAdd(4)), IntPtr)
                            Dim wrTargObjId = GetGCHandleTargetObjectId(wrHand)
                            Dim wrTarg = GetGCHandleTargetObj(wrHand)
                            sRetval += String.Format("(Target={0} 0x{1:x8})", wrTarg, wrTargObjId.ToInt32) ' so can sort by target
                            '                            sRetval += String.Format(" m_handle= {0:x8} handlePtr= {1:x8} {2:x8}  {3}", wrHand, wrHandPtr.ToInt32, wrHandPtrPtr.ToInt32, wrTarg)
                        ElseIf systype.StartsWith("EventHandler") Then
                            lamEventHandler.Invoke()
                        ElseIf systype.StartsWith("Windows.RoutedEventHandler") Then
                            lamEventHandler.Invoke()
                        End If
                    End If

                    If fExpandSystemStringOrArray AndAlso sRetval.EndsWith("]") Then


                        'Dim strDims = sRetval.Substring(12)
                        'Dim nDigs = 1
                        'If Char.IsDigit(strDims(1)) Then
                        '    nDigs = 2
                        'End If
                        'Dim nDims = CInt(strDims.Substring(0, nDigs))
                        Dim nDims = 1
                        For i = 1 To nDims
                            Dim nElemsPerDim = 0
                            If nDims = 1 Then
                                nElemsPerDim = ReadProcessMemoryDWORDEx(ObjectId.MyAdd(4))
                            Else
                                nElemsPerDim = ReadProcessMemoryDWORDEx(ObjectId.MyAdd(4 + 4 * i))
                            End If

                            sRetval += String.Format("(Count={0})", nElemsPerDim)
                        Next
                    End If
                Catch ex As Exception
                    Debug.Assert(False, "Exception while expanding string, type, or array" + vbCrLf + ex.ToString)
                End Try
            End If
            Return sRetval
        End Function

        Public Shared Function GetGCHandleTargetObj(ByVal gcHand As IntPtr) As String
            Dim targ = String.Empty
            If gcHand <> IntPtr.Zero Then
                Dim ObjectId = GetGCHandleTargetObjectId(gcHand)
                If ObjectId <> IntPtr.Zero Then
                    targ = ClrClassInfo.GetClassNameFromClassOrObjectId(clsId:=IntPtr.Zero, ObjectId:=ObjectId, fExpandSystemStringOrArray:=True)
                End If
            End If
            Return targ
        End Function

        Public Shared Function GetGCHandleTargetObjectId(ByVal gcHand As IntPtr) As IntPtr
            Dim targ = IntPtr.Zero
            If gcHand <> IntPtr.Zero Then
                targ = CType(ReadProcessMemoryDWORDEx(CType(gcHand, IntPtr)), IntPtr)
            End If
            Return targ
        End Function

        ''' <summary>
        ''' Gets classlayout of class, including inherited into a list recursively
        ''' </summary>
        Public Shared Function GetClassLayoutsFromClassId(
                 ByVal clsId As IntPtr,
                 Optional ByVal lstRes As List(Of ClassLayoutDetail) = Nothing
                 ) As List(Of ClassLayoutDetail)
            If lstRes Is Nothing Then
                lstRes = New List(Of ClassLayoutDetail)
            End If
            Dim clsidParent = IntPtr.Zero
            Debug.Assert(_ConnectionMode = MemSpectMode.Offline OrElse _ConnectionMode = MemSpectMode.OnLine, "works in offline or online")
            Dim layoutDetails As ClassLayoutDetail = Nothing
            If clsId <> IntPtr.Zero Then
                Dim fNeedToGet = False
                If ClrClassInfo.g_DictClassLayouts.ContainsKey(clsId) Then ' could exist, but only contain classname: no details
                    layoutDetails = ClrClassInfo.g_DictClassLayouts(clsId)
                    If layoutDetails IsNot Nothing Then
                        If Not layoutDetails._fDidTryGettingClassLayouts AndAlso _ConnectionMode = MemSpectMode.OnLine Then
                            fNeedToGet = True
                        Else
                            lstRes.Insert(0, layoutDetails) ' put at head of list
                            clsidParent = layoutDetails.classIdParent ' set parent so we can recur
                        End If
                    Else
                        Debug.Assert(False, "null layoutdet?")
                        lstRes.Add(New ClassLayoutDetail) ' empty
                    End If
                Else
                    fNeedToGet = True
                End If
                If fNeedToGet Then
                    Debug.Assert(_ConnectionMode = MemSpectMode.OnLine, "offline cl layout in dict")
                    Dim fNeedName = False
#If DEBUG Then
                    Debug.Assert(_IsInGetClassLayouts = False, "recursive GetClassLayoutFromClassId?")
                    _IsInGetClassLayouts = True
#End If
                    Try
                        SendMsg(ProcMsgVerb.GetClrClsLayout, fSendEndMsgSync:=False, dwords:={clsId.ToInt32})
                        Dim res = GetMsg(4 * 5) ' get # of items, class size, parentclassid
                        Debug.Assert(res.Length = 20, "didn't getmsg right size: got " + res.Length.ToString)
                        Dim nItems = BitConverter.ToInt32(res, 0)
                        If nItems >= 0 Then '<0 is error
                            Dim clsSize = BitConverter.ToInt32(res, 4)
                            clsidParent = New IntPtr(BitConverter.ToInt32(res, 8))
                            Dim nInstances = BitConverter.ToInt32(res, 12)
                            Dim nCollected = BitConverter.ToInt32(res, 16)
                            If layoutDetails Is Nothing Then
                                layoutDetails = New ClassLayoutDetail With {.classId = clsId}
                                fNeedName = True ' can't get name while in msg sync
                            End If
                            layoutDetails.classIdParent = clsidParent
                            layoutDetails.classSize = clsSize
                            layoutDetails.classNumInstances = nInstances
                            layoutDetails.classNumCollected = nCollected
                            lstRes.Insert(0, layoutDetails) ' reverse order of inheritance
                            For i = 0 To nItems - 1
                                res = GetMsg(8)
                                Dim iOff = BitConverter.ToInt32(res, 0)
                                Dim fldNameLenInBytes = BitConverter.ToInt32(res, 4)
                                Dim fldName = String.Empty
                                For j = 0 To fldNameLenInBytes - 1
                                    res = GetMsg(2)
                                    Dim ch = BitConverter.ToChar(res, 0)
                                    fldName += ch
                                Next
                                res = GetMsg(4)
                                Dim fldType = String.Empty
                                Dim fldTypeLenInBytes = BitConverter.ToInt32(res, 0)
                                For j = 0 To fldTypeLenInBytes - 1
                                    res = GetMsg(2)
                                    Dim ch = BitConverter.ToChar(res, 0)
                                    fldType += ch
                                Next
                                layoutDetails.dictFieldInfo(iOff) = New ClsLayoutFieldInfo With {.FldName = fldName, .FldType = fldType}

                            Next
                        End If
                    Catch ex As Exception
                        Debug.Assert(False, ex.ToString)
                        Common.MemSpectExceptionHandler(ex)
                    Finally
                        EndMsgSync()
                    End Try
#If DEBUG Then
                    _IsInGetClassLayouts = False
#End If
                    If layoutDetails Is Nothing Then
                        layoutDetails = New ClassLayoutDetail With {.classId = clsId, ._fDidTryGettingClassLayouts = True}
                        fNeedName = True
                    End If
                    If fNeedName Then
                        layoutDetails.className = GetClassNameFromClassOrObjectId(layoutDetails.classId)
                    End If
                    ClrClassInfo.g_DictClassLayouts(clsId) = layoutDetails
                    layoutDetails._fDidTryGettingClassLayouts = True
                End If
                If clsidParent <> IntPtr.Zero Then
                    GetClassLayoutsFromClassId(clsidParent, lstRes) ' recur
                End If
            End If
            Return lstRes
        End Function

        Public Shared Function GetClsLayoutFromOffset(clsLayOuts As List(Of ClassLayoutDetail), iOffset As Integer) As ClsLayoutFieldInfo
            Dim emptyClsLayout = New ClsLayoutFieldInfo With {.FldName = String.Empty, .FldType = String.Empty}
            Dim retval = emptyClsLayout
            For Each clsLayout In clsLayOuts
                If clsLayout.dictFieldInfo.ContainsKey(iOffset) Then
                    retval = clsLayout.dictFieldInfo(iOffset)
                    Exit For
                End If
            Next
            Return retval
        End Function

        ''' <summary>
        ''' Get the size of the member at the offset
        ''' alg: find the current offset member
        ''' subtract it from the offset of the next member
        ''' </summary>
        Public Shared Function GetMemberInformation(classId As IntPtr, offset As Integer) As Tuple(Of ClsLayoutFieldInfo, Integer)

            Dim res = GetAllMemberInformation(classId)
            Dim tup As Tuple(Of ClsLayoutFieldInfo, Integer) = Nothing
            If res.TryGetValue(offset, tup) Then
            End If
            Return tup
        End Function

        ''' <summary>
        ''' if clsid == intptr.zero, returns empty list
        ''' </summary>
        Public Shared Function GetAllMemberInformation(clsId As IntPtr) As SortedDictionary(Of Integer, Tuple(Of ClsLayoutFieldInfo, Integer))
            Dim result = New SortedDictionary(Of Integer, Tuple(Of ClsLayoutFieldInfo, Integer))
            Dim clsLayouts = ClrClassInfo.GetClassLayoutsFromClassId(clsId)
            Dim clsSize = 0
            If clsLayouts.Count > 0 Then
                clsSize = clsLayouts(clsLayouts.Count - 1).classSize
            End If
            Dim lstOffsets = New List(Of Integer)
            For Each clsLayout In clsLayouts
                For Each entry In clsLayout.dictFieldInfo
                    '                    result.Add(entry.Key, New Tuple(Of ClsLayoutFieldInfo, Integer)(entry.Value, 0)) ' key = offset, value = (name,type), size
                    lstOffsets.Add(entry.Key)
#If DEBUG Then
'                    UpdateStatusMsg(String.Format("Adding {0} {1}", entry.Key, entry.Value.ToString))
#End If
                Next
            Next
            ' now calc & plug in the sizes into the tuple
            Dim ndxOffset = 0
            Dim size = 0
            For Each clsLayout In clsLayouts
                For Each entry In clsLayout.dictFieldInfo
                    ndxOffset += 1
                    If ndxOffset < lstOffsets.Count Then
                        size = lstOffsets(ndxOffset) - entry.Key ' size is next offset - cur offset
                    Else
                        size = clsSize - entry.Key - IntPtr.Size ' minus clsid
                    End If
                    result.Add(entry.Key, New Tuple(Of ClsLayoutFieldInfo, Integer)(entry.Value, size))
#If DEBUG Then
'                    UpdateStatusMsg(String.Format("set size {0} = {1}", entry.Key, size))
#End If

                Next
            Next
            Return result
        End Function

        Public Overrides Sub FromStream(ByVal deserializer As Deserializer, ByVal versionFromStream As Integer)

            Dim nCnt = deserializer.ReadInt
            For i = 0 To nCnt - 1
                Dim clsid = New IntPtr(deserializer.ReadInt)
                Dim val = deserializer.ReadObject
                If ClrClassInfo.g_DictClassLayouts.ContainsKey(clsid) Then
                    Dim tmp = ClrClassInfo.g_DictClassLayouts(clsid)
                    ClrClassInfo.g_DictClassLayouts.Remove(clsid)
                End If
                ClrClassInfo.g_DictClassLayouts.Add(clsid, CType(val, ClassLayoutDetail))
            Next
        End Sub

        Public Overrides Sub ToStream(ByVal serializer As Serializer)
            serializer.Write(ClrClassInfo.g_DictClassLayouts.Count)
            For Each entry In ClrClassInfo.g_DictClassLayouts
                serializer.Write(entry.Key.ToInt32)
                serializer.WritePrivate(entry.Value)
            Next
        End Sub

    End Class

    'one per class. Some classes have no instances (inherit)
    Public Class ClassLayoutDetail
        Inherits SerializableObject

        Public classId As IntPtr
        Public className As String = String.Empty ' base name with no expanded sys.string, array info
        Public classIdParent As IntPtr
        Public classSize As Integer
        Public classNumInstances As Integer ' for offline, this is it. For online, can be refreshed from TrkBlk
        Public classNumCollected As Integer ' for offline, this is it. For online, can be refreshed from TrkBlk
        Public _fDidTryGettingClassLayouts As Boolean ' some classes have none (like no members, but inherits). For online only

        Public dictFieldInfo As New SortedDictionary(Of Integer, ClsLayoutFieldInfo) ' key = offset, 

        Public Shared Function CreateClassLayoutDetail(ByVal clsName As String, ByVal clsId As IntPtr) As ClassLayoutDetail
            Dim clsLayoutDet = New ClassLayoutDetail With {.className = clsName, .classId = clsId}
            ClrClassInfo.g_DictClassLayouts(clsId) = clsLayoutDet ' add to cache
            Return clsLayoutDet
        End Function

        Public Function GetOffsetOfField(ByVal fld As String) As Integer
            Dim nOffset = 0
            For Each itm In dictFieldInfo
                If itm.Value.FldName = fld Then
                    nOffset = itm.Key
                    Exit For
                End If
            Next
            Return nOffset
        End Function
        Public Overrides Sub FromStream(ByVal deserializer As Deserializer, ByVal versionFromStream As Integer)
            classId = CType(deserializer.ReadInt, IntPtr)
            className = deserializer.ReadString
            classIdParent = CType(deserializer.ReadInt, IntPtr)
            classSize = deserializer.ReadInt
            classNumInstances = deserializer.ReadInt
            classNumCollected = deserializer.ReadInt
            _fDidTryGettingClassLayouts = True ' we're offline
            Dim nCnt = deserializer.ReadInt
            For i = 0 To nCnt - 1
                Dim key = deserializer.ReadInt
                Dim val = deserializer.ReadObject
                dictFieldInfo.Add(key, CType(val, ClsLayoutFieldInfo))
            Next
        End Sub

        Public Overrides Sub ToStream(ByVal serializer As Serializer)
            serializer.Write(classId.ToInt32)
            serializer.Write(className)
            serializer.Write(classIdParent.ToInt32)
            serializer.Write(classSize)
            serializer.Write(classNumInstances)
            serializer.Write(classNumCollected)
            serializer.Write(dictFieldInfo.Count)
            For Each itm In dictFieldInfo
                serializer.Write(itm.Key)
                serializer.WritePrivate(itm.Value)
            Next
        End Sub
        Public Overrides Function ToString() As String
            Return String.Format("{0:x8} P={1:x8} # = {2} {3}", classId.ToInt32, classIdParent.ToInt32, dictFieldInfo.Count, className)
        End Function
    End Class

    Public Class ClsLayoutFieldInfo
        Inherits SerializableObject

        Public FldName As String
        Public FldType As String

        Public Overrides Sub FromStream(ByVal deserializer As Deserializer, ByVal versionFromStream As Integer)
            FldName = deserializer.ReadString
            FldType = deserializer.ReadString
        End Sub

        Public Overrides Sub ToStream(ByVal serializer As Serializer)
            serializer.Write(FldName)
            serializer.Write(FldType)
        End Sub

        Public Overrides Function ToString() As String
            Return String.Format("{0} {1}", FldName, FldType)
        End Function
    End Class


End Namespace
