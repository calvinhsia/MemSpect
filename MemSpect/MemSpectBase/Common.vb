Imports System.IO
Imports System.IO.Pipes
Imports System.Windows.Threading
Imports System.Threading
Imports System.Text
Imports System.Runtime.InteropServices
Imports System.Linq

Imports FastSerialization0
Imports System.ComponentModel
Imports System.Collections.ObjectModel
Imports System.Security.Principal
Imports System.Net
Imports System.Threading.Tasks
Imports System.CodeDom.Compiler

<Assembly: Runtime.CompilerServices.InternalsVisibleTo("MemSpect.Test, PublicKey=0024000004800000940000000602000000240000525341310004000001000100fd51ffc305f02c69a29bf791b1c7c5794e6a717c2db35b64dbf800529443f8a960ef67b9362c662b1ebe0c1452c11bfe29c828e22a668e07a1b9679d935c5472922d64a8924e6bf91e0c2737777647da1eb1a7667a14736aa56f67f6db68d6c8a928bf36b6ee155d053cadf5b25c7f36b435ead06b35bab33948a0dfee77fd99")>
<Assembly: Runtime.CompilerServices.InternalsVisibleTo("MemSpect, PublicKey=0024000004800000940000000602000000240000525341310004000001000100fd51ffc305f02c69a29bf791b1c7c5794e6a717c2db35b64dbf800529443f8a960ef67b9362c662b1ebe0c1452c11bfe29c828e22a668e07a1b9679d935c5472922d64a8924e6bf91e0c2737777647da1eb1a7667a14736aa56f67f6db68d6c8a928bf36b6ee155d053cadf5b25c7f36b435ead06b35bab33948a0dfee77fd99")>

'This file contains stuff common between the inproc version (VBDiagMargin) and the out of proc version (MemSpect.exe)
Namespace MemSpect

    Public Module Common

        Private _isInitialized As Boolean = False

        Public _iniFileName As String = String.Empty ' full path to ini file
        Public _MemSpectVersion As String = __MemSpectUICurrentVersion ' as read from target process
        Public Const __MemSpectUICurrentVersion As String = "161109" ' update in version.h too
        Public ReadOnly Property _IsCalvinHBuild As Boolean
            Get
                Return Environment.GetEnvironmentVariable("computername").ToLower.StartsWith("calvinh")
            End Get
        End Property


        Public Function _MemSpectUICurrentVersion(Optional ByVal fIncludeTimeStamp As Boolean = False, Optional ByVal fIncludeDebugRetail As Boolean = True) As String

            Dim DebugRetail = String.Empty
#If DEBUG Then
            If fIncludeDebugRetail Then
                DebugRetail = " UI Dbg"
            End If
#Else
#End If
            If fIncludeTimeStamp Then
                If _IsCalvinHBuild = True Then
                    Dim instdir = IO.Path.GetDirectoryName(Reflection.Assembly.GetExecutingAssembly.Location)
                    Dim mostRecent = DateTime.Now.AddDays(-100000)
                    For Each fname In {"MemSpectBase.dll",
                                       "MemSpect.exe",
                                       "MemSpectDll.dll"
                                       }
                        Dim pname = IO.Path.Combine(instdir, fname)
                        Dim finfo = New FileInfo(pname)
                        If finfo.LastWriteTime > mostRecent Then
                            mostRecent = finfo.LastWriteTime
                        End If
                    Next
                    DebugRetail += " " + mostRecent.ToString
                End If

            End If
            Return __MemSpectUICurrentVersion + DebugRetail

        End Function

        Public Function GetGlobalPassCount() As UInteger
            Dim res As UInteger = 0
            'offline and online

            Debug.Assert(_IsUnderTest OrElse _ConnectionMode = MemSpectMode.MiniDumpOnly OrElse _ConnectionMode = MemSpectMode.Offline OrElse _AddressOfulGlobalPassCount <> IntPtr.Zero, "_AddressOfulGlobalPassCount is zero?")
            If _AddressOfulGlobalPassCount <> IntPtr.Zero Then
                res = IntegerToUInteger(ReadProcessMemoryDWORDEx(_AddressOfulGlobalPassCount))
            End If
            Return res

        End Function
        Public Function GetNumCurAllocs() As Integer
            Dim res = 0
            If _ConnectionMode = MemSpectMode.OnLine Then
                Debug.Assert(_AddressOfnCurAllocs <> IntPtr.Zero, "_AddressOfnCurAllocs is zero?")
                res = ReadProcessMemoryDWORDEx(_AddressOfnCurAllocs)
            End If
            Return res
        End Function

        Friend _Addressof_g_pCDebFreezeAllThreads As IntPtr ' address in target process of freeze state class. Non-zero means frozen.
        Friend _AddressOfulGlobalPassCount As IntPtr ' address in target process of GlobalPassCount (current seqno)
        Friend _AddressOfnCurAllocs As IntPtr '  address in target process of # of items currently allocated
        Public _AddressOfStackMemMapStats As IntPtr ' address in target of Mem Map statistics
        ''' <summary>
        ''' for online, use stack storage in paging file
        ''' for offline, stacks are not in minidump, so are in _allocationStacks
        ''' </summary>
        Public ReadOnly Property IsUsingStackMemMap() As Boolean
            Get
                If _AddressOfStackMemMapStats = IntPtr.Zero Then
                    Return False
                End If
                Return True
            End Get
        End Property

        Public _IsClientOutOfProcess As Boolean  ' this will be out of proc or inproc. for Offline, it's false
        Public Enum MemSpectMode
            UnInitialized
            OnLine
            Offline ' using mega snapshot
            Existing     ' existing process with no injected DLLs, no detours. Can just examine VM or WriteProcessMem
            MiniDumpOnly
            QuitMemspect
        End Enum

        Public Enum ShowKnownIssuesEnum
            NoAction
            ''' <summary>
            ''' include issues in a snapshot, show them in a column
            ''' </summary>
            ''' <remarks></remarks>
            Include
            ''' <summary>
            ''' do not include known issues in a snapshot
            ''' </summary>
            ''' <remarks></remarks>
            Exclude
        End Enum

        Public _ConnectionMode As MemSpectMode
        Public _pathCommandlineSnapshot As String 'non-null means path to write snapshot to for command line snapshot only mode (Prism)

        Public _IsLoadingOfflineSnapshot As Boolean ' true means it's currently loading
        Public _MiniDumpFileName As String '= "minidump.dmp" ' only for Minidump
        Public Const AllocationGranularity As Integer = &H10000 ' 64k
        Public Const _PerfIdleCodeMarker As Integer = 502
        Public Const HexColumnSpecifier = "16" ' added to end of columns to total to indicate add hex, 
        Public Const MemSpectHeapName As String = "__MemSpect"
        Public Const MemSpectDllName As String = "MemSpectDll.dll"
        Public Const MemSpectIniName As String = "MemSpect.ini" ' default
        Public Const CodeMarkerGuidLookupFilename As String = "CodeMarkerGuidLookup.txt" ' in memspect deploy dir, so user can edit
        Public Const DumpedFilesMHDFileName = "DumpedFiles.mhd"
        Public Const SnapDataZipName = "SnapSerializedData.zip"
        Public Const ProcessHeapName As String = "__Process Heap"
        Public Const ClassNameOffset As Integer = 12
        Public Const IndirectInfoLenOffset As Integer = 20 ' see union in CIndirectInfo
        Public Const IndirectInfoBufferOffset As Integer = 24

        Public Const MANAGED_STACK_FLAG As Integer = &H80000000
        Public _strGlobal As String = String.Empty
        Public _fHandle4gigStacks As Boolean
        Public _memSpectThreadId As Integer = -1 ' the threadid of the memspect thread in the target process. Init to -1 for offline
        Public _memSpectSharedMemAddr As IntPtr ' the addr in targ proc for shared mem
        Public _addressOf_g_ThreadCmdsToChildReady As IntPtr
        Public _addressOf_g_fTrackingGhosts As IntPtr
        Public _MainThread As Integer ' the thread in the target process that did LoadLib of MemSpectDll.Dll.Usually UI thread, but can be injected thread
        Public _Offset_AllocToTblk As Integer ' 5 for Dev10, 6 for Dev11: layout of tree_nod changed

        Public _IsUnderTest As Boolean ' flag set by test code or non-Memspect API code
        Public _ShowUI As Boolean = True ' flag set by test code. 

        Public _TargetProcessId As Integer ' used for out of proc parent proc
        Public _TargetProc As Process 'if _IsRemote, then ParentProcess.Handle, else CurrentProcess.Handle
        Friend __hProcessTarget As IntPtr ' private for 
        Public _SharedMemAddr As IntPtr ' addr in UI proc of mem shared with target proc
        Public _SharedMemSize As Integer
        Public _PerfIdleSeqNo As Integer ' seqno at which VS perfIdle codemarker fired
        Public _UseChildProcessForSymbols As Boolean ' true if we resolve symbols in this process. False if in remote (target)
        Public _GCStatsAddr As IntPtr
        Public _ExpandStringContents As Boolean = True
        Public _ShowAddressInCallStacks As Integer = 0
        Public _CodeMarkerMode As CodeMarkerType = CodeMarkerType.CustomMarker Or CodeMarkerType.NormalMarker
        Public _offlineSnapshot As OfflineMegaSnapshot = Nothing

        Friend _MegaSnapShotSymbolDict As SortedSet(Of IntPtr) ' if non-null, indicates we're taking a megasnap and postponing symres for batching

        Public _CompareIntPtr As New CompareIntPtr

        Public _OffsetCallStackFrames As UInteger = CUInt(Marshal.SizeOf(GetType(HeapAllocationStruct)))
        ''' <summary>
        ''' from stackframeindex to realaddress. fManaged is high bit of index.
        ''' </summary>
        ''' <remarks></remarks>
        Public _stackIndexDict As Dictionary(Of IntPtr, IntPtr)
        Public _didBulkGetStackIndex As Boolean ' we only want to do this if frozen
        Public _MemSpectProcessHandle As IntPtr = GetCurrentProcess() ' faster to cache handle
        Public _HeapLeaks As List(Of HeapAllocationContainer) ' detected leaks from HeapDestroys with leftovers

        Public _GhostData As New GhostData ' single instance

        Public MemSpectLog As New List(Of String)


        Public _ImmersivePackageInfo As ImmersivePackageInfoClass


        Public ReadOnly Property IsInitialized As Boolean
            Get
                Return _isInitialized
            End Get
        End Property

        Public Function GetErrorMessageFromWin32LastError(ByVal errnum As Integer) As String
            Dim errmsg = String.Empty
            If errnum <> 0 Then
                Dim sb = New StringBuilder(256)
                Dim res = FormatMessage(
                    FORMAT_MESSAGE_IGNORE_INSERTS +
                    FORMAT_MESSAGE_FROM_SYSTEM +
                    FORMAT_MESSAGE_ARGUMENT_ARRAY,
                    Nothing,
                    errnum,
                    0,
                    sb,
                    sb.Capacity,
                    IntPtr.Zero)

                errmsg = sb.ToString.Replace(vbCr, String.Empty).Replace(vbLf, String.Empty).Trim(New Char() {" "c, CChar(vbCr), CChar(vbLf)}) ' strip off space, crlf if any            
            End If
            Return errmsg
        End Function

        Public Function GetErrorMessageFromHResult(ByVal hr As Integer) As String
            Dim ex = Marshal.GetExceptionForHR(hr)
            Dim res = String.Empty
            If ex Is Nothing Then
                res = "HRESULT Error code + " + hr.ToString("x8")
            Else
                res = ex.ToString
            End If
            Return res
        End Function


        Public Class Win32Exception
            Inherits Exception
            ' we don't want to complicate with too many constructors calling/changing Marshal.GetLastWin32Error
            Private _msg As String
            Public Sub New(ByVal errnum As Integer, ByVal szDesc As String)
                Me._msg = (szDesc + " " + GetErrorMessageFromWin32LastError(errnum))
            End Sub
            Public Overrides ReadOnly Property Message As String
                Get
                    Return Me._msg
                End Get
            End Property
        End Class

        Public Class GhostData ' single instance
            Public Property SeqNoTurnedOn As UInteger
            Public Property SeqNoTurnedOff As UInteger
            Public Property IsTracking As Boolean
            Public GhostList As New Dictionary(Of UInteger, AllocCtrGhost) ' added when TrackGhost is used
            Public Overrides Function ToString() As String
                Return String.Format("Ghost SeqNoTurnedOn={0:n0} SeqNoTurnedOff={1:n0} # Items={2:n0}", SeqNoTurnedOn, SeqNoTurnedOff, GhostList.Count)
            End Function

            Public Sub Clear()
                SeqNoTurnedOff = 0
                SeqNoTurnedOn = 0
                GhostList.Clear()
            End Sub

            Public Sub StartTracking()
                Clear()
                IsTracking = True
                SeqNoTurnedOn = GetGlobalPassCount()
                UpdateStatusMsg("Tracking ghost allocations @SeqNo=" + SeqNoTurnedOn.ToString("n0"))
                'start tracking allocs where trkLo <= size <= trkHi
                SendMsg(ProcMsgVerb.GhostAllocSpec, fSendEndMsgSync:=True, dwords:={1})
            End Sub

            Public Sub StopTracking()

                ' gotta watch for race condition
                WriteProcessMemory(_hProcessTarget, _addressOf_g_fTrackingGhosts, New Byte() {0, 0, 0, 0}, 4, Nothing) ' indicate we're not tracking ghosts
                Thread.Sleep(1000)
                '                SendMsg(ProcMsgVerb.GhostAllocSpec, fSendEndMsgSync:=True, dwords:={0}) ' turn off logging
                IsTracking = False

                SeqNoTurnedOff = GetGlobalPassCount()
                UpdateStatusMsg("ghost allocation tracking stopped @SeqNo=" + SeqNoTurnedOff.ToString("n0"))
            End Sub
        End Class

        Public Class AllocCtrGhost 'info about an allocation that could already be freed.
            Public Data As String ' could be managed obj name
            Public callstkArray As IntPtr()
            Public SeqNoWhenFreed As Integer ' >0 if freed.
            Public hctr As HeapAllocationContainer
            Public Heap As CSpyHeap

            Public ReadOnly Property callStkstring As String
                Get
                    Dim sb As New StringBuilder
                    For Each addr In callstkArray
                        sb.AppendLine(ResolveAddressToSymbol(addr))
                    Next
                    Return sb.ToString
                End Get
            End Property
        End Class


        Public Event StatusMessageEvent(ByVal sender As Object, ByVal e As StatusMessageEventArgs)
        Public Enum StatusMessageType
            LogEntry ' shows in log and status bar
            StatusBarEntry ' shows on status bar only
            'Warning
            AlertMsgBox ' shows MsgBox
            CalvinHBuild ' for CalvinH debugging
        End Enum
        Public Enum StatusMessagePriority
            Normal
            Low   ' will not block on UI
        End Enum
        Public Class StatusMessageEventArgs
            Inherits EventArgs
            Public Property Message As String
            Public Property MsgType As StatusMessageType
            Public Property MsgPriority As StatusMessagePriority
            Public Property MsgTimeStamp As DateTime
            Public Overrides Function ToString() As String
                Return Message
            End Function
        End Class

        Public Sub UpdateStatusMsg(ByVal msg As String,
                                   Optional ByVal fAssert As Boolean = False,
                                   Optional ByVal msgType As StatusMessageType = StatusMessageType.LogEntry,
                                   Optional ByVal msgPriority As StatusMessagePriority = StatusMessagePriority.Normal)
            'Thread.CurrentThread.CurrentCulture = New System.Globalization.CultureInfo("ja-JP") ' ja-JP

            ' msg = DateTime.Now.ToLongTimeString.Substring(0, 8).Trim + " " + msg ' skip date
            If msgType = StatusMessageType.CalvinHBuild AndAlso Not _IsCalvinHBuild Then
                Return
            End If
            Dim msgTimestamp = DateTime.Now
            If String.IsNullOrEmpty(_pathCommandlineSnapshot) Then ' we're in command line snap mode: don't update stat bar
                RaiseEvent StatusMessageEvent(Nothing,
                                              New StatusMessageEventArgs With {
                                                  .Message = msg,
                                                  .MsgType = msgType,
                                                  .MsgPriority = msgPriority,
                                                  .MsgTimeStamp = msgTimestamp
                                                }
                                              )

            End If
            msg = String.Format("{0:T} {1}", msgTimestamp, msg) 'like "11:52:37 PM"
            MemSpectLog.Add(msg) ' add to our collection so we can dump to offline snapshot
            If msgType = StatusMessageType.AlertMsgBox Then
                MsgBox(msg)
            End If
            If fAssert Then ' in debug versions, we may want to assert to attach, but in retail, we may not want any msgbox
                Debug.Assert(False, msg)
            End If
        End Sub


        Public Sub Initialize() ' need to init, esp for auto tests which use same instance of assembly
            'If _isInitialized Then
            '    UpdateStatusMsg("Common.Initialize() called twice without calling Cleanup() first.", msgType:=StatusMessageType.AlertMsgBox)
            '    Return
            'End If
            OfflineMegaSnapshot.CloseZipPackage()
            CLRObjRefsWrapper.ClrObjRefFileName = Nothing
            GCData.ClearData()
            _MemSpectVersion = _MemSpectUICurrentVersion() ' default DLL version to UI version in case of offline load
            _MiniDumpFileName = "minidump.dmp"
            _iniFileName = IO.Path.Combine(IO.Directory.GetCurrentDirectory, MemSpectIniName)
            _fHandle4gigStacks = False
            _offlineSnapshot = Nothing
            _AddressOfnCurAllocs = IntPtr.Zero
            _AddressOfulGlobalPassCount = IntPtr.Zero
            _IsClientOutOfProcess = True
            _ConnectionMode = MemSpectMode.OnLine
            _TargetProcessId = 0
            _TargetProc = Nothing
            __hProcessTarget = IntPtr.Zero
            _SharedMemAddr = IntPtr.Zero
            _SharedMemSize = 0
            _UseChildProcessForSymbols = True
            _GCStatsAddr = IntPtr.Zero
            _stackIndexDict = New Dictionary(Of IntPtr, IntPtr)
            StackFrameDictionary.Clear()
            _didBulkGetStackIndex = False
            __HeapList.Clear()
            _memSpectSharedMemAddr = IntPtr.Zero
            _MemBasicInfoList.Clear()
            WorkingSetInfo._WorkingSetDict = Nothing
            ClrClassInfo.g_DictClassLayouts.Clear()
            MiniDumpReader.Clear() ' test scenario
            _VirtualAllocOrFileCalls = Nothing
            _FileLoadNotifications = Nothing
            _AddressOfStackMemMapStats = IntPtr.Zero
            _HeapLeaks = New List(Of HeapAllocationContainer)
            _GhostData = New GhostData
            Dim hmemspect = NativeImports.GetModuleHandle(MemSpectDllName)
            If hmemspect <> IntPtr.Zero Then ' if memspectdll is loaded (for resolving symbols)
                VsClearSymbols() ' remove symbols from prior run for different process: test scenario
            End If
            ' when calling VsClearSymbols it loads MemSpectDll.dll very early
            HeapAllocationContainer._dictCodeMarkerGuidLookup = Nothing
            HeapAllocationContainer.__CodeMarkerSnap = Nothing
            KnownIssues.ClearKnownIssues()
            StackMapFile.Cleanup()
            _offlineSnapshot = Nothing
            SnapDiffer.MemNodeDiff.ClearDict(0) ' 0 means clear all
            _isInitialized = True
        End Sub


        Public Enum CodeMarkerEventType
            None
            Start
            [End]
        End Enum

        <Flags()>
        Public Enum CodeMarkerType
            None
            NormalMarker
            CustomMarker
        End Enum

        'During a live MemSpect run (perhaps via Prism or Apex) a test can call to say a particular named test scenario was reached, like "OpenSolution"
        ' or "Typing"
        'dwMarkerId is a code marker Id associated with the marker/event which can be used for sorting purposes. Something like the sequential integers starting at 100000 
        '  also used in conjunction with CodeMarkersAtWhichToFreeze
        ' dwLevel is nesting level, like 1,2,3
        ' eventType
        ' 
        Public Function FireCustomCodeMarkerEvent(ByVal strMarkerName As String,
                                                  ByVal eventType As CodeMarkerEventType,
                                                  ByVal dwDepthLevel As Integer,
                                                  ByVal dwMarkerId As Integer) As Boolean

            Dim bencode = New Text.ASCIIEncoding
            Dim barray = bencode.GetBytes(strMarkerName)

            Dim intArray(4 + barray.Length) As Integer ' include nullterm
            intArray(0) = CInt(eventType)
            intArray(1) = dwDepthLevel
            intArray(2) = dwMarkerId
            intArray(3) = strMarkerName.Length
            For i = 0 To barray.Length - 1
                intArray(i + 4) = barray(i)
            Next
            intArray(4 + barray.Length) = 0    ' nullterm
            Dim res = SendMsg(ProcMsgVerb.GotCustomCodeMarker, fSendEndMsgSync:=True, dwords:=intArray)


            'Dim bencode = New Text.UnicodeEncoding
            'Dim barray = bencode.GetBytes(strMarkerName)
            'WriteProcessMemory(_MemSpectProcessHandle, _SharedMemAddr, barray, barray.Length, Nothing)

            'Dim intArray(4) As Integer
            'intArray(0) = CInt(eventType)
            'intArray(1) = dwDepthLevel
            'intArray(2) = dwMarkerId
            'intArray(3) = strMarkerName.Length

            'SendMsg(ProcMsgVerb.GotCustomCodeMarker, intArray)

            Return If(res Is Nothing, False, True)
        End Function


        'GotCodemarker
        'Returns marker received. Returns 0 on non-received, timeout. 
        Public Function WaitForCodeMarker(ByVal nMsecsTimeout As Integer, ByVal fFreezeWhenHit As Boolean, ByVal MarkerIds As Integer()) As Integer
            Dim retInt = 0
            ' prepend array with fFreezeWhenHit, length
            Dim tempArr(MarkerIds.Length + 1) As Integer
            tempArr(0) = If(fFreezeWhenHit, 1, 0)
            tempArr(1) = MarkerIds.Length
            For i = 0 To MarkerIds.Length - 1
                tempArr(i + 2) = MarkerIds(i)
            Next
            Dim ret = SendMsg(ProcMsgVerb.SetCodeMarkerAction, fSendEndMsgSync:=True, dwords:=tempArr)

            Dim ev = New ManualResetEventSlim(False)
            Dim evhandler = Sub(e As ProcComm.CodeMarkerEventArgs)
                                retInt = e.ReceivedMarkerId
                                If fFreezeWhenHit Then
                                    ProcComm.FreezeTarget()
                                End If
                                ev.Set()
                            End Sub
            AddHandler ProcComm.ReceivedCodeMarkerEvent, evhandler

            If ev.Wait(nMsecsTimeout) Then
                Debug.Assert(retInt <> 0, "received event should set retInt")
            Else
                Debug.Assert(retInt = 0, "waitforcodemarker timeout retint should be 0")
                '                SendMsg(ProcMsgVerb.CodeMarkerWaitEvent, fSendEndMsgSync:=True, dwords:={0, 0}) ' clear marker wait event
            End If
            '            RemoveHandler ProcComm.ReceivedCodeMarkerEvent, evhandler
            Return retInt
        End Function



        Public Function MemSpectExceptionHandler(ByVal ex As Exception, Optional ByVal strDesc As String = " ") As String
            Dim procid = Process.GetCurrentProcess.Id
            Dim procName = IO.Path.GetFileNameWithoutExtension(Process.GetCurrentProcess().MainModule.ModuleName) '"MemSpect" or "AutoWatson"
            Dim str = procName + "(Ver= " + _MemSpectVersion +
                ")(Pid=" + procid.ToString + " Thread " + System.Threading.Thread.CurrentThread.ManagedThreadId.ToString + " " + strDesc.Trim() +
                " Exception: " + ex.ToString ' .Message + " " + strDesc + If(String.IsNullOrEmpty(ex.StackTrace), "", ex.StackTrace)
            System.Console.WriteLine(str)
            UpdateStatusMsg(str)
            If _IsUnderTest Then
                Throw ex
                '                MsgBox(str.ToString)
            End If
            Debug.Assert(False, str)
            Return str
        End Function

        ''' <summary>
        ''' the process handle of the target: like devenv.exe
        ''' </summary>
        Public Property _hProcessTarget As IntPtr ' if _IsRemote, then ParentProcess.Handle, else CurrentProcess.Handle
            Get
                If _ConnectionMode = MemSpectMode.Offline Then
                    Dim strErr = "ProcessTarget request in offline mode "
                    Dim trace = Function(fr As StackFrame()) As String
                                    Dim sb As New Text.StringBuilder
                                    For Each f In fr
                                        sb.Append(f.GetMethod.Name + vbCrLf)
                                    Next
                                    Return sb.ToString
                                End Function.Invoke((New System.Diagnostics.StackTrace).GetFrames)
                    Dim str = "MemSpect " + _MemSpectVersion + strErr + trace
                    'Dim res = MessageBox.Show(str)
                    Debug.Assert(False, str)
                    Throw New InvalidOperationException(str)
                End If
                Debug.Assert(__hProcessTarget <> IntPtr.Zero, "somebody retrieving null handle to targ process")
                Return __hProcessTarget
            End Get
            Set(ByVal value As IntPtr)
                __hProcessTarget = value
            End Set
        End Property


        Public Class CompareIntPtr
            Inherits Comparer(Of IntPtr)
            Public Overrides Function Compare(ByVal x As System.IntPtr, ByVal y As System.IntPtr) As Integer
                If x = y Then
                    Return 0
                End If
                Dim x1 = (x.ToInt64)
                Dim y1 = (y.ToInt64)
                If x1 >= 0 Then
                    If y1 >= 0 Then
                        If x1 < y1 Then
                            Return -1
                        End If
                        Return 1
                    End If
                    Return -1
                Else
                    If y1 < 0 Then
                        If x1 < y1 Then
                            Return -1
                        End If
                        Return 1
                    End If
                    Return 1
                End If

            End Function

        End Class

        Public _MemBasicInfoList As New SortedList(Of IntPtr, MEMORY_BASIC_INFORMATION)(_CompareIntPtr)
        Public Function GetVirtAllocs() As SortedList(Of IntPtr, MEMORY_BASIC_INFORMATION)
            Dim memList As SortedList(Of IntPtr, MEMORY_BASIC_INFORMATION)

            If _ConnectionMode = MemSpectMode.Offline OrElse _ConnectionMode = MemSpectMode.MiniDumpOnly Then
                memList = MiniDumpReader.Singleton.GetVirtualAllocs
            Else
                If ProcComm._isTargetFrozen Or _ConnectionMode = MemSpectMode.Existing Then ' if using existing proc that's life, we need a stable snapshot of VM
                    memList = _MemBasicInfoList
                Else
                    memList = New SortedList(Of IntPtr, MEMORY_BASIC_INFORMATION)(_CompareIntPtr)
                End If
                If memList.Count = 0 Then
                    Dim mbi As New MEMORY_BASIC_INFORMATION
                    Dim mbiSize = Marshal.SizeOf(mbi)
                    Dim lpMem = IntPtr.Zero ' start at 0
                    Do While VirtualQueryEx(_hProcessTarget, lpMem, mbi, CUInt(mbiSize)) = mbiSize
                        If memList.Count > 0 AndAlso mbi.BaseAddress = IntPtr.Zero Then
                            Exit Do ' looped around address space?
                        End If
                        If mbi.RegionSize = UInteger.MaxValue Then ' &HFFFFFFFF Then
                            Exit Do
                        End If
                        memList.Add(lpMem, mbi)
                        Debug.Assert(mbi.BaseAddress = lpMem, "base addr")
                        'Debug.WriteLine(lpMem.ToInt32.ToString("x8") + " " + mbi.RegionSize.ToString("x8"))
                        lpMem = lpMem.MyAdd(mbi.RegionSize)
                    Loop
                End If
            End If

            Return memList

        End Function


        Private _clrLow As Long
        Private _clrEnd As Long
        Private _HaveTriedToGetClrAddressAlready As Boolean
        Public Function IsAddressInCLRDll(ByVal addr As IntPtr) As Boolean
            Dim fIsInClrDLL = False
            If Not _HaveTriedToGetClrAddressAlready Then
                _HaveTriedToGetClrAddressAlready = True
                If _clrLow = 0 Then
                    If _ConnectionMode = MemSpectMode.OnLine Then
                        Dim bencode = New Text.ASCIIEncoding
                        Dim barray = bencode.GetBytes("clr.dll")
                        Dim intarr(barray.Length) As Integer
                        For i = 0 To barray.Length - 1
                            intarr(i) = barray(i)
                        Next
                        intarr(barray.Length) = 0  ' nullterm
                        '                        SendMsg(ProcMsgVerb.MyGetModuleHandle, intarr)
                        Dim lpMem = GetModuleHandle("clr.dll")
                        Dim addrStart = lpMem
                        If lpMem <> IntPtr.Zero Then
                            Dim mbi As MEMORY_BASIC_INFORMATION
                            Dim mbiPrior As MEMORY_BASIC_INFORMATION
                            Dim mbiSize = Marshal.SizeOf(mbi)
                            Do While VirtualQueryEx(_hProcessTarget, lpMem, mbi, CUInt(mbiSize)) = mbiSize
                                If mbi.AllocationBase <> addrStart Then
                                    Exit Do
                                End If
                                mbiPrior = mbi
                                lpMem = lpMem.MyAdd(mbi.RegionSize)
                            Loop
                            _clrLow = addrStart.ToInt64
                            _clrEnd = mbiPrior.AllocationBase.MyAdd(-1).ToInt64
                        End If
                    ElseIf _ConnectionMode = MemSpectMode.Offline Then
                        For Each entry In MiniDumpReader.Singleton._ModuleDictionary.Values
                            Dim filename = entry.ModuleName
                            If Path.GetFileName(entry.ModuleName).ToLower = "clr.dll" Then
                                _clrLow = entry.minidump_Module.BaseOfImage
                                ' to first approx, clr.dll image size is same as VMSize
                                _clrEnd = _clrLow + entry.minidump_Module.SizeOfImage
                                Exit For
                            End If
                        Next

                    End If

                End If
            End If
            If _clrLow <> 0 Then
                Dim testaddr = addr.ToInt64
                If testaddr > _clrLow AndAlso testaddr < _clrEnd Then
                    fIsInClrDLL = True
                End If

            End If
            Return fIsInClrDLL
        End Function


        <System.Runtime.CompilerServices.Extension()>
        Function GetConsecutiveZeros(ByVal this As FileInfo) As Integer
            Dim nZerosMax = 0
            Dim nRetries = 0
            Dim tmpname = String.Empty
            While nRetries < 2
                Dim strm As Stream = Nothing
                Try
                    Dim nChunkSize = 65536
                    Dim buf(nChunkSize) As Byte
                    If nRetries = 0 Then
                        strm = this.Open(FileMode.Open, FileAccess.Read)
                    Else
                        tmpname = Path.ChangeExtension(Path.GetTempFileName, "txt")
                        File.Copy(this.FullName, tmpname)
                        Dim finfoTmp = New FileInfo(tmpname)
                        finfoTmp.Attributes = FileAttributes.Archive
                        strm = File.Open(tmpname, FileMode.Open, FileAccess.Read)
                    End If
                    nZerosMax = 0
                    Using strm
                        Dim nZerosCur = 0
                        While strm.Position < strm.Length
                            nChunkSize = strm.Read(buf, 0, nChunkSize)
                            For i = 0 To nChunkSize - 1
                                If buf(i) = 0 Then
                                    nZerosCur += 1
                                    If nZerosCur > nZerosMax Then
                                        nZerosMax = nZerosCur
                                    End If
                                Else
                                    nZerosCur = 0
                                End If
                            Next
                        End While
                        strm.Close()
                    End Using

                    'Using binreader = New BinaryReader(strm)
                    '    Dim nZerosCur = 0
                    '    While strm.Position < strm.Length

                    '        Dim b = binreader.ReadByte
                    '        If b = 0 Then
                    '            nZerosCur += 1
                    '        Else
                    '            If nZerosCur > nZerosMax Then
                    '                nZerosMax = nZerosCur
                    '            End If
                    '            nZerosCur = 0
                    '        End If
                    '    End While
                    'End Using
                    nRetries = 2 ' break out of while
                Catch ex As Exception
                    nZerosMax = -1 ' indicate error
                    nRetries += 1
                Finally
                    If strm IsNot Nothing Then
                        strm.Close()
                    End If
                End Try
            End While
            If Not String.IsNullOrEmpty(tmpname) AndAlso File.Exists(tmpname) Then
                File.Delete(tmpname)
            End If

            Return nZerosMax
        End Function

        <System.Runtime.CompilerServices.Extension()>
        Public Function MyAdd(ByVal this As IntPtr, ByVal addend As Long) As IntPtr
            ' for some reason, switching from release/debug causes this extension method to clash with
            ' the Shared method IntPtr.Add
            Dim iptrRes = IntPtr.Zero
            Try
                Dim num = this.ToInt64 + addend
                ' Dev10 had a different behavior (see Bug #170425: CInt() behavior change [throws OverflowException in Dev11; didn't in Dev10]
                If num <= Integer.MaxValue Then ' normal range
                    iptrRes = New IntPtr(num)
                Else
                    If num > UInteger.MaxValue Then
                        Throw New InvalidOperationException("Integer overflow: is this a 32 bit process?")
                    End If
                    ' we have a # > Integer.MaxValue, like &h80000000 = 2147483648
                    ' we want to convert to the right neg long to make an intptr
                    Dim tmp = Integer.MinValue + (num - Integer.MaxValue - 1)
                    iptrRes = New IntPtr(tmp)
                End If
            Catch ex As Exception
                Throw New InvalidOperationException(String.Format("MyAdd args = {0:x8} {1:x8} {2}", this.ToInt32, addend, ex.ToString))
            End Try
            Return iptrRes
        End Function

        <System.Runtime.CompilerServices.Extension()>
        Public Function MyToULong(ByVal this As IntPtr) As ULong
            ' for some reason, switching from release/debug causes this extension method to clash with
            ' the Shared method IntPtr.Add
            Dim res = this.ToInt64
            If res < 0 Then
                res += UInteger.MaxValue + 1
            End If
            Debug.Assert(res >= 0, "overflow converting " + this.ToInt32.ToString("x8"))
            Return CULng(res)
        End Function

        <System.Runtime.CompilerServices.Extension()>
        Public Function Increment(ByVal this As IntPtr) As IntPtr
            Return this.MyAdd(1)
        End Function


        <System.Runtime.CompilerServices.Extension()>
        Public Function LongToIntPtr(ByVal this As Long) As IntPtr
            Return IntPtr.Zero.MyAdd(this)
        End Function


        Public _GlobalFilter As New GlobalFilter

        Public Const ProfileStringSection As String = "Defaults"

        Public Const TBLK_SIGNATURE As Integer = &H6B6C4254 'TBlk
        Public _IsShuttingDown As Boolean = False
        Public _IsInMiddleOfMessaging As ProcMsgVerb = ProcMsgVerb.UnKnownVerb ' if in the middle of messaging, then don't let background thread process msgs

        Public Function CheckIsDebuggingAndMsgBox() As Boolean
            Dim sbDebug = New Text.StringBuilder(256)
            GetPrivateProfileString(ProfileStringSection, "fMessageBoxOnStart", String.Empty, sbDebug, sbDebug.Capacity, _iniFileName)
            Dim fIsDebugging = False
            If sbDebug.Length > 0 AndAlso CInt(sbDebug.ToString) > 0 Then
                UpdateStatusMsg(String.Format("MemSpect process starting. Attach a debugger {0}({1})",
                                              Process.GetCurrentProcess.MainModule.FileName,
                                              Process.GetCurrentProcess.Id),
                                          msgType:=StatusMessageType.AlertMsgBox)
                System.Threading.Thread.Sleep(5000)

                'MessageBox.Show(String.Format("MemSpect process starting. Attach a debugger {0}({1})",
                '                              Process.GetCurrentProcess.MainModule.FileName,
                '                              Process.GetCurrentProcess.Id))
                fIsDebugging = True
            End If
            Return fIsDebugging
        End Function


        Friend __HeapList As New SortedSet(Of CSpyHeap)(New CSpyHeap.CHeapCompare)

        Public ReadOnly Property _HeapList As SortedSet(Of CSpyHeap) '(New CSpyHeap.CHeapCompare) 'New SortedDictionary(Of String,CSpyHeap) '
            Get
                If _ConnectionMode = MemSpectMode.Existing OrElse _ConnectionMode = MemSpectMode.MiniDumpOnly Then
                Else
                    If __HeapList.Count = 0 Then
                        Dim res = ReadHeaps()
                        Debug.Assert(res, "readheaps failed")
                    End If
                End If
                Return __HeapList
                '    Publicz _HeapList As New SortedSet(Of CSpyHeap)(New CSpyHeap.CHeapCompare) 'New SortedDictionary(Of String,CSpyHeap) '

            End Get
        End Property

        Public Enum ProcMsgVerb
            GetMemSpectVersion = 0 ', // no params, returns a String
            GetSharedMem = 1 ', // no params, returns 9 DWORDs: hMappedFile, SharedMemSize, g_hDebugHeap, GetProcessHeap(), MemSpectHeap, g_fHandle4gigStacks, SeqNoPerfIdle,Tid(memspect),Offset_AllocToTblk  Puts MEMORYSTATUSEX in shared mem
            GetIniFile = 2 ',  // no params, returns a String: fullpath to ini file
            GetHeapStats = 3 ', // 
            GetHeapAllocs = 4 ' , // 3 params: hHeap (CHeapSpy), SeqnoLo, SeqnoHi  (-1 means no seq filter)
            GetFirstHeapBlock = 5 ', // 1 param: hHeap, 0 is Num Dword ptrs to Blks (0 indicates no more), array of ptrs. Max 1
            GetNextHeapBlocks = 6 ', // 1 param: hHeap, 0 is Num Dword ptrs to Blks (0 indicates no more), array of ptrs
            ResolveSymbol = 7 ',  // 2 param: address, fIsFunctionId. string is in shared mem, like "f:\dd\wpf\src\graphics\core\common\display.cpp(889) : wpfgfx_v0400.dll!CDisplaySet::Init + 20 bytes"
            ResolveSymbolNoLineInfo = 8 ', // 1 param: address. Will not include line no info like "wpfgfx_v0400.dll!CDisplaySet::Init + 20 bytes"
            ResolveStackFrames = 9 ', // 1 param: address of CAllocation heap block, returns ptr[0] = nCnt of strings, then consecutive null term'd strings for each stack frame
            ForceGC = 10 ',    // 1 param
            GetFirstObjRefs = 11 ', // 2 param: 1 = the objectId 2 = thing to get:(0 = all, 1 = refFROMME, 2 = refTOME) . Will do a GC. 
            GetNextObjRefs = 12 ', // 2 param: initial index into array if not all obtained in GetFirstObjRefs. (2nd param ignored)
            ThreadsFreeze = 13 ',// 1 params: cleanup RCW ? Returns 1 DWORD: SeqNo of freeze
            ThreadsUnFreeze = 14 ', // 0 params: Returns 1 DWORD: SeqNo of freeze, Use ProcComm.UnfreezeTarget() as it tracks frozen state
            GetClrData = 15 ', // 2 params:Blk, fIncludeSystem.String. could be Assembly, Module, Appdomain, ClassID, returns 3 ints, string (Class/Module/Appdomain name) in shared mem
            GetClassNameFromId = 16  '//given classid, return name in pipe. p0= ClassId, p1 = dwObjectId, p2 = fExpandSystem. If ClassId is null, will get from ObjectId
            GetClrSize = 17 ', // 3 params:BlkType,   ClassID 3rd param is objectId or NUll
            GetClrClsLayout = 18 '// 1 param: ClassId, returns in pipe: DWORD nClassSize, DWORD nCntMems, (int iOffSet, szFieldName)nCntMems
            GetGCStatAddr = 19 ' ,// 0 params. Returns addr of GC Stat struct, or 0 if not avail
            ClrObjTrk = 20 ', // // 1 param: 0, 1, or 2 . 2 means just return current value. 0, 1 to turn on/off CLR Obj Tracking (returns 3 DWORDS in shared mem: cur setting, g_pCProcessGC, and SeqNo)
            GetCodeMarkerName = 21 '// 1 param: code marker id, returns string of marker in shared mem
            GetCodeMarkers = 22 ', // 2 params: Lo,Hi seqno. If both 0, gets all. For CodeMarkerMerge
            GotCustomCodeMarker = 23 ', // 5 params int EventType, int DepthLevel, int MarkerId, int LenStr, WCHAR strScenarioname
            GetProcessHeapHandles = 24 ',    // no param: cnt, then all process heap handles in sharedmem
            DoHeapWalk = 25 ', // 3 params: handle, index, fOnlyWantRegions. If index=0, it's first block. Returns # blks in 0, then consecutive PROCESS_HEAP_ENTRY structs
            GetStackAddresses = 26 ', // 1 param: heapallocationblk. Puts in shared mem DWORD nCnt of addresses (should match m_uicStackAddr) followed by nCnt DWORDS of the call stack frame addresses
            DoPivot = 27 ' , //  3 params: hHeap, index, StackFrameAddr to pivot. Returns in shared mem PAllocationStruct: return value = # of items. 
            LotsOfThreads = 28 ', // Create lots of threads nThreads, nSleep, nLeak
            GetClrObjDump = 29 ', // no params: will write to named pipe all CLR Objects and their references
            DoVirtualAlloc = 30 ', // params of VirtualAlloc
            DoVirtualFree = 31 ', // params of VirtualFree
            TranslateStackIndex = 32 ',// only needed if fHandle4gigStacks: 1 param: 0 means get all stack index addresses, non-zero means get just that one. Written to pipe
            AssertSeqNo = 33 ',    // Param0: 0 means add param 1 as SeqNo to assert on.  Param0:1 means return all current SeqNos. Param0:2 means clear all
            AssertStackFrame = 34 ',    // Param0: 0 means add param 1 as StackFrame to assert on.  Param0:1 means return all current StackFrames. Param0:2 means clear all
            GetThreadInfo = 35 '  // no params; returns 4 DWORDS for each thread: (Id, SeqNo, stackBase, stackLimit)
            SetHeapName = 36 ', // Symbol resolved in child process: 2 params: Handle, nBytes, bytestream
            GhostAllocSpec = 37 ', // specify size range, obj name for special logging
            MyGetModuleFileName = 38 ', // 
            MyGetModuleHandle = 39 ', // GetModuleHandle of a dll specified in pipe
            FreeStackMem = 40 ',   // 1 param: hHeap (CHeapSpy): free stack mem for specified heap
            TrackingMode = 41 '   // 1 param: -1 means get the current tracking mode. else sets TrackingMode to the param
            SuspendResumeImmersive = 42 ',	// NOP unless Immersive 2 params: P0=1 means Resume, 0 means Suspend   p1 = TID to resume
            GetMemStats = 43 ', // Get the memstats list that accumulated via CustomCodeMarker
            GetCodeMarkerActions = 44 ', // Get the actions recorded, if any
            CompileAndExecuteFile = 45 ', // 1 param: full path filename to execute. Will compile and execute C# code
            '        // these are from parent to child
            GotCodemarker = 100 ', // 4 DWORDS: codemarker ID and seqno, Extrainfo, InstanceNum
            SetCodeMarkerAction = 101 ',    // p0 = action, p1 = # of markers , p2...pN = markerids. If P1=0, means clear all actions
            ResolveSymbolFromTarg = 102 ', // p0: sym to resolve. p1 = fIncludeFileAndLineno returns: len prefixed str of sym name in pipeFromTarg
            UpdateStatusMessage = 103 ',    // p0: string to display in status
            HeapLeak = 104 ',       // When HeapDestroy, AllocationStructs of any leftovers p0: HeapHandle, p1: CAddressNode *. Repeat for multiple leaks
            GhostAllocLog = 105 ',    // when an alloc meeting AllocLocSpec criteria, send it's info
            GhostAllocFree = 106 ',    // when a logged alloc gets freed, we'll send a msg telling 
            LoadResourceEvent = 107 ' // when a resource is loaded: sends hModule, hResInfo, call stack
            CrashTargetProcess = 108 '  // p0 = 0 means crash, p0=1 means hang, p1 = Int 3 (DebugBreak()). For crash, p1 = 0 means constant crash, p1>0 means unique int for crash. Force an AV in target process to test crash scenarios
            GetPEBaseAddressFromMem = 109 ' // p0 = hModule as IntPtr : get the PE Base Address as set by ASLR
            '//these are same in both child-parent and parent-child direction        
            NoOp = 124 ',   // no op: for perf measurements
            UnKnownVerb = 125 ',
            VerbDone = 126 '// no params: indicates the prior cmd put data in shared mem and it's ready
            Quit = 127 ',1 param: if from child (MemSpect.exe) to parent(Devenv), then Bit0=0 means quit, but don't terminate child, bit0=1 means terminate parent & child. Bit1=0 means unfreeze first if frozen. Bit1=1 means ignore Frozen status        
        End Enum

        ''' <summary>
        ''' 
        ''' </summary>
        ''' <param name="cntext"></param>
        ''' <param name="nMaxDumpSize">Optional. 0 means no dump. -1 means Max as spec'd by context (Could be VERY big). Default 64K.  </param>
        Public Function GetStacksAndDump(
                                 ByVal cntext As Object,
                                 Optional ByVal nMaxDumpSize As Integer = 65536,
                                 Optional CallBack As Action(Of String, Boolean) = Nothing
                                    ) As KeyValuePair(Of String, String)

            Dim Address As IntPtr
            Dim Size As Integer
            Dim BlockType = BlockTypes.None
            Dim fDoAddrDump = True
            Dim fIsArena = False ' either Header or Alloc
            Dim fIsArenaAlloc = False ' just alloc
            Dim fIsHeapWalkMap = False
            Dim sbuilder = New Text.StringBuilder()

            Dim AddString = Sub(str As String, isBold As Boolean)
                                sbuilder.Append(str)
                                If CallBack IsNot Nothing Then
                                    CallBack(str, isBold)
                                End If
                            End Sub

            Dim heapContainer = TryCast(cntext, HeapAllocationContainer)
            Dim lamGetAddrAndSizeFromContext = Sub()
                                                   Dim AddressDesc = ComponentModel.TypeDescriptor.GetProperties(cntext)("Address")
                                                   If AddressDesc IsNot Nothing Then
                                                       Dim rr = Integer.Parse(CStr(AddressDesc.GetValue(cntext)), System.Globalization.NumberStyles.AllowHexSpecifier)
                                                       Address = New IntPtr(rr)
                                                   End If
                                                   Dim sizeDesc = ComponentModel.TypeDescriptor.GetProperties(cntext)("Size")
                                                   If sizeDesc IsNot Nothing Then
                                                       Size = CInt(sizeDesc.GetValue(cntext))
                                                   End If

                                               End Sub
            If heapContainer Is Nothing Then
                heapContainer = HeapAllocationContainer.CreateFrom(cntext)
                'heapContainer could still be nothing
                If heapContainer Is Nothing Then
                    lamGetAddrAndSizeFromContext()
                End If
                ' we got a hctr, but it could be a heapwalk map
                Dim cbDataDesc = ComponentModel.TypeDescriptor.GetProperties(cntext)("cbData")
                If cbDataDesc IsNot Nothing Then
                    fIsHeapWalkMap = True
                End If
                AddString(cntext.ToString() + " ", False) ' cntxt includes a _HeapAllocationContainer already
            Else
                AddString(heapContainer.ToString() + " ", False)
            End If

            If heapContainer IsNot Nothing Then
                Address = heapContainer.GetAddr
                Size = heapContainer.GetSize
                BlockType = heapContainer.TBlkBlockType
                If heapContainer.SpyHeapPtr IsNot Nothing Then ' can be nothing for Images
                    AddString(" Heap = " + heapContainer.SpyHeapPtr.HeapName, False)
                End If
            End If

            If BlockType = BlockTypes.None Then
                If heapContainer IsNot Nothing AndAlso heapContainer.SpyHeapPtr IsNot Nothing Then
                    If heapContainer.SpyHeapPtr.IsArenaHeap Then
                        fIsArena = True
                        If heapContainer.GetArenaBlockType = ArenaBlkType.Alloc Then
                            fIsArenaAlloc = True
                        End If
                        AddString(String.Format(vbCrLf + " ArenaHeader {0} ",
                                                      heapContainer.GetArenaHeaderInfo.ToString
                                                      ), False)

                    End If
                    AddString(" HeapHandle= " + heapContainer.SpyHeapPtr.GetRealOSHeapHandle.ToString("x8"), False)
                End If
            Else
                Dim tmp = heapContainer.GetDisplayData(nMode:=HeapAllocationContainer.GetDisplayDataEnum.DisplayToolTip, CallBack:=CallBack)
                If Not String.IsNullOrEmpty(tmp) Then
                    AddString(vbCrLf + tmp, False)
                End If
                Select Case heapContainer.TBlkBlockType
                    Case BlockTypes.TlsAllocFree, BlockTypes.ClrExcpt, BlockTypes.ThreadCreate
                        fDoAddrDump = False
                    Case BlockTypes.IndirectInfo
                        Select Case heapContainer.GetIndirectInfoType
                            Case IndirectInfoType.IIType_FileLoad, IndirectInfoType.IIType_FileUnload
                                fDoAddrDump = False
                        End Select
                End Select
            End If
            If heapContainer IsNot Nothing AndAlso _GlobalFilter.KnownIssues = ShowKnownIssuesEnum.Include Then
                Dim issue = heapContainer.GetKnownIssue
                If Not String.IsNullOrEmpty(issue) Then
                    AddString(vbCrLf + String.Format("Known Issue {0}", KnownIssues.GetKnownIssue(heapContainer).ToString), False)
                End If
            End If
            AddString(vbCrLf, False)

            If heapContainer IsNot Nothing Then
                If heapContainer.IsGhostAlloc Then
                    Dim nearestMarker = heapContainer.NearestCodeMarker
                    If Not String.IsNullOrEmpty(nearestMarker) Then
                        AddString("NearestCodemarker = " + nearestMarker, False)
                    End If

                    If heapContainer.GhostAlloc.SeqNoWhenFreed > 0 Then ' if it's been freed
                        AddString("GHOSTED! This alloc was freed at SeqNo= " + heapContainer.GhostAlloc.SeqNoWhenFreed.ToString("n0"), False)
                        fDoAddrDump = False
                    Else
                        AddString("GHOSTED! This alloc was ghost tracked and not freed yet", False)
                    End If
                    AddString(heapContainer.GhostAlloc.callStkstring, False)
                Else
                    Dim stk = String.Empty
                    If heapContainer.AllocationStruct.m_uicStackAddr = 0 Then
                        If _FileLoadNotifications IsNot Nothing AndAlso _FileLoadNotifications.ContainsKey(heapContainer.GetAddr) Then
                            stk = _FileLoadNotifications(heapContainer.GetAddr).GetCallStackAsString
                        End If
                    Else
                        stk = heapContainer.GetCallStackAsString
                    End If
                    AddString(stk, False)
                End If
            End If
            'If stkFilters IsNot Nothing Then
            '    If (From filter In stkFilters Where sframes.ToLower.Contains(filter)).Count() > 0 Then
            '        fDoAddrDump = False
            '    End If
            'End If

            Dim strAddrDump = String.Empty
            If fDoAddrDump And nMaxDumpSize <> 0 Then  ' getting dump is expensive
                If heapContainer Is Nothing OrElse (Address <> heapContainer.AllocationStruct.Address) Then
                    If heapContainer IsNot Nothing And BlockType = BlockTypes.ClrObject Then
                        Address = Address.MyAdd(-4) ' show syncSblock too
                    End If
                    If heapContainer IsNot Nothing AndAlso BlockType = BlockTypes.CodeMarker Then
                        Dim cmdata = heapContainer.GetCodeMarkerData
                        If cmdata.MarkerUserDataLen > 0 Then
                            strAddrDump = "MarkerData" + vbCrLf + GetMemoryDump(cmdata.MarkerUserDataAddr, cmdata.MarkerUserDataLen)
                        End If
                    Else
                        strAddrDump = GetMemoryDump(Address, Size, nMaxDumpSize:=nMaxDumpSize)
                    End If
                Else
                    Address = heapContainer.AllocationStruct.Address
                    If BlockType = BlockTypes.ClrObject Then
                        Address = Address.MyAdd(-4)
                    End If
                    strAddrDump = GetMemoryDump(
                    Address,
                    heapContainer.AllocationStruct.Size,
                    nMaxDumpSize:=nMaxDumpSize
                    )
                End If
                If fIsArenaAlloc Then ' additional mem dump for arenaalloc
                    Dim arenaDump = GetMemoryDump(heapContainer.AllocationStruct.Address, heapContainer.AllocationStruct.Size)
                    strAddrDump += vbCrLf + "ArenaAlloc dump" + vbCrLf + arenaDump
                End If
                If fIsHeapWalkMap Then
                    If heapContainer IsNot Nothing AndAlso heapContainer.IsMemSpectHeap Then
                        lamGetAddrAndSizeFromContext()
                        Dim HeapWalkDump = GetMemoryDump(Address, Size)
                        strAddrDump += vbCrLf + "MemSpect Metadata" + vbCrLf + HeapWalkDump
                    End If
                End If
            End If
            Return New KeyValuePair(Of String, String)(sbuilder.ToString, strAddrDump)
        End Function


        'see http://msdn.microsoft.com/en-us/magazine/dd419661.aspx
        <System.Runtime.ExceptionServices.HandleProcessCorruptedStateExceptions()>
        Public Function GetMemoryDump(ByVal nAddress As IntPtr,
                                             ByVal nSize As Integer,
                                             Optional ByVal nMaxDumpSize As Integer = 1024,
                                             Optional ByVal fUseCurrentProcess As Boolean = False
                                              ) As String

            Dim sbAddrDump As New Text.StringBuilder
            Try
                Dim nBytes = nSize ' ((nSize + 3) \ 4) * 4 'int div round
                Dim numBytesTruncated = 0
                If nMaxDumpSize > 0 Then
                    numBytesTruncated = nBytes - nMaxDumpSize
                    nBytes = Math.Min(nMaxDumpSize, nBytes)
                End If
                Dim innerCnt = 0
                Dim strBytes = String.Empty
                Dim strChars = String.Empty
                Dim ptr = nAddress
                Dim blk As New ProcMemBlockByte
                Dim blkCnt = 0
                Dim blksize = Marshal.SizeOf(blk)
                Dim nExitearly = 0
                Dim dwBytesRead = 0
                For i = 0 To nBytes - 1 Step 4
                    Dim offsetInBlk = i Mod blksize
                    If nExitearly > 0 Then
                        If i > nExitearly Then
                            Exit For
                        End If
                    End If
                    If innerCnt Mod 8 = 0 Then
                        If i > 0 Then
                            sbAddrDump.AppendLine("  " + strBytes + "  " + strChars)
                            strBytes = String.Empty
                            strChars = String.Empty
                        Else
                            sbAddrDump.AppendLine()
                        End If
                        Dim addrToUse = If(fUseCurrentProcess, IntPtr.Zero, nAddress) ' like for embedded resource dump
                        sbAddrDump.Append(String.Format("{0:x8} : ", addrToUse.MyAdd(i).ToInt32))
                    End If
                    innerCnt += 1
                    Dim dword As Integer = 0
                    If _IsClientOutOfProcess OrElse _ConnectionMode <> MemSpectMode.OnLine OrElse fUseCurrentProcess Then
                        If i Mod blksize = 0 Then
                            Dim nBytesToReadThisTime = (nBytes - blkCnt * blksize)
                            If nBytesToReadThisTime > blksize Then
                                nBytesToReadThisTime = blksize
                            End If
                            Dim addrtoread = nAddress.MyAdd(blkCnt * blksize)
                            If (_ConnectionMode = MemSpectMode.Offline OrElse _ConnectionMode = MemSpectMode.MiniDumpOnly) AndAlso
                                        Not fUseCurrentProcess Then
                                Dim bytes = MiniDumpReader.Singleton.ReadMemoryDictionary(addrtoread, nBytesToReadThisTime)
                                If bytes IsNot Nothing Then
                                    blk.data = bytes
                                    dwBytesRead = blk.data.Length
                                Else
                                    dwBytesRead = 0
                                End If
                                If dwBytesRead < 4 Then
                                    'dwBytesRead = 0
                                End If
                                nExitearly = i + dwBytesRead
                            Else
                                Dim hProcessToUse As IntPtr
                                If fUseCurrentProcess Then
                                    hProcessToUse = GetCurrentProcess() ' like dumping resources
                                Else
                                    hProcessToUse = _hProcessTarget
                                End If
                                If ReadProcessMemoryByte(hProcessToUse, nAddress.MyAdd(blkCnt * blksize), blk, nBytesToReadThisTime, dwBytesRead) = 0 Then
                                    Dim lerr = Marshal.GetLastWin32Error
                                    If lerr = 299 Then ' only partial readprocmem 
                                        nExitearly = i + dwBytesRead
                                    Else
                                        Dim tempstr = "Dump readmem failed " +
                                                        nAddress.ToInt32.ToString("x8") +
                                                        " LastErr= " + lerr.ToString + " " + GetErrorMessageFromWin32LastError(lerr)
                                        UpdateStatusMsg(tempstr, fAssert:=True)
                                        sbAddrDump.AppendLine(tempstr)
                                        Return sbAddrDump.ToString

                                    End If
                                End If
                            End If
                            blkCnt += 1

                        End If
                        If dwBytesRead - offsetInBlk >= 4 Then
                            dword = BitConverter.ToInt32(blk.data, offsetInBlk)
                        Else
                            For b = 0 To dwBytesRead - offsetInBlk - 1
                                dword = CInt(dword + (blk.data(offsetInBlk + b) * 256 ^ b))
                            Next
                        End If
                    Else ' current process (like embedded resource)
                        dword = Marshal.ReadIntPtr(ptr.MyAdd(i)).ToInt32
                        Throw New InvalidOperationException("Can't get here ???")
                    End If
                    Dim bytesToProcThisTime = dwBytesRead - offsetInBlk
                    If bytesToProcThisTime > 0 Then
                        toBytes(dword, strBytes, strChars, If(bytesToProcThisTime > 4, 4, bytesToProcThisTime))
                        If bytesToProcThisTime >= 3 Then
                            sbAddrDump.Append(String.Format("{0:x8} ", dword)) ' for narrower display, just show bytes/chars, not dwords
                        Else
                            Select Case bytesToProcThisTime
                                Case 1
                                    sbAddrDump.Append(String.Format("{0:x2} ", dword))
                                Case 2
                                    sbAddrDump.Append(String.Format("{0:x4} ", dword))
                            End Select
                        End If
                    End If
                Next
                ' some leftovers
                Dim startLeftover = innerCnt
                Do While innerCnt Mod 8 > 0
                    sbAddrDump.Append("         ")
                    innerCnt += 1
                Loop
                sbAddrDump.Append("  " + strBytes + "  ")
                innerCnt = startLeftover
                Do While innerCnt Mod 8 > 0
                    sbAddrDump.Append("            ") ' 4 * 3 = 12
                    innerCnt += 1
                Loop
                sbAddrDump.AppendLine(strChars)

                Do While innerCnt Mod 8 > 0
                    sbAddrDump.Append("         ")
                    innerCnt += 1
                Loop
                If numBytesTruncated > 0 Then
                    sbAddrDump.AppendLine(String.Format(
                                          "<...TotSize={0:n0}, Displayed={1:n0} Truncated={2:n0} bytes>",
                                          nSize,
                                          nSize - numBytesTruncated,
                                          numBytesTruncated))
                End If
            Catch ex As Exception
                sbAddrDump.Append("Exception when reading " + nAddress.ToInt32.ToString + " " + ex.Message)
                'MemSpectExceptionHandler(ex)
            End Try

            Return sbAddrDump.ToString
        End Function

        Private shifts As Integer() = {24, 16, 8, 0}
        Public Sub toBytes(ByVal num As Integer, ByRef strBytes As String, ByRef strChars As String, ByVal nLimit As Integer)
            For i = 3 To 4 - nLimit Step -1
                Dim abyte = CByte((num >> shifts(i) And &HFF))
                strBytes += String.Format("{0:x2} ", abyte)
                Dim thechar = " "
                If abyte > 15 AndAlso abyte < 127 Then
                    thechar = Chr(abyte).ToString
                End If
                strChars += thechar
            Next
        End Sub




        Public Function GetStringFromRemoteMem(ByVal addr As IntPtr, ByVal nStrLen As Integer, ByVal hProc As IntPtr) As String
            Dim dwBytesRead As Integer
            Dim strRetval = String.Empty
            If nStrLen = 0 Then ' deal with nullterm strings: read 1 char at a time til 0
                Dim blk As New ProcMemBlockByte
                Do
                    If ReadProcessMemoryByte(hProc, addr, blk, 1, dwBytesRead) <> 0 Then
                        If blk.data(0) = 0 Then
                            Exit Do
                        End If
                        Dim thechar = BitConverter.ToChar(blk.data, 0)
                        strRetval += thechar
                    Else
                        Dim err = Marshal.GetLastWin32Error
                        UpdateStatusMsg("fail GetStringFromRemoteMem " + addr.ToString + " " + nStrLen.ToString + " " + err.ToString + " " + GetErrorMessageFromWin32LastError(err), fAssert:=True)
                        Exit Do
                    End If
                    addr = addr.Increment()
                Loop

            Else
                Dim blk As New ProcMemBlock
                If ReadProcessMemory(hProc, addr, blk, nStrLen, dwBytesRead) <> 0 Then
                    strRetval = New String(blk.data, 0, nStrLen)
                Else
                    Dim err = Marshal.GetLastWin32Error
                    UpdateStatusMsg("fail " + addr.ToString + " " + nStrLen.ToString + " " + err.ToString + " " + GetErrorMessageFromWin32LastError(err), fAssert:=True)
                End If

            End If
            Return strRetval
        End Function

        Public Class ThreadInfo
            Friend _ThreadId As Integer
            Public ReadOnly Property ThreadId As String
                Get
                    Dim retval = _ThreadId.ToString
                    If _ThreadId = _MainThread Then
                        retval += "M"
                    End If
                    Return retval
                End Get
            End Property

            Public SeqNoStart As Integer ' 0 indicates tid gone
            Public nTotAllocs As Integer
            Public nTotSize As Integer
            Public Note As String = String.Empty
            Public MiniDumpThreadInfo As MINIDUMP_THREAD
            Public StackBase As IntPtr
            Public StackLimit As IntPtr
            Private _ThreadProc As String = String.Empty
            Public Function GetThreadProcAddress() As IntPtr
                Dim res = IntPtr.Zero
                If Not Common._fHandle4gigStacks AndAlso StackBase <> IntPtr.Zero Then
                    If _ThreadId <> _memSpectThreadId Then
                        Dim ndx = 1
                        While ndx < 1024
                            Dim dword = ReadProcessMemoryDWORDEx(StackBase.MyAdd(-ndx * 4))
                            ndx += 1
                            If dword <> 0 Then ' read next dword
                                dword = ReadProcessMemoryDWORDEx(StackBase.MyAdd(-ndx * 4))
                                res = CType(dword, IntPtr)
                                _ThreadProc = ResolveAddressToSymbol(CType(dword, IntPtr), fStripFileName:=True, fStripBytesToo:=True)
                                Exit While
                            End If
                        End While
                    End If
                End If
                Return res
            End Function
            Public ReadOnly Property ThreadProc As String
                Get
                    If Not Common._fHandle4gigStacks AndAlso StackBase <> IntPtr.Zero AndAlso String.IsNullOrEmpty(_ThreadProc) Then
                        If _ThreadId <> _memSpectThreadId Then
                            Dim addr = GetThreadProcAddress()
                            _ThreadProc = ResolveAddressToSymbol(addr, fStripFileName:=True, fStripBytesToo:=True)
                        End If
                    End If
                    Return _ThreadProc
                End Get
            End Property
            Public allocs As New List(Of HeapAllocationContainer)
            Friend Sub GetStackInfoFromTeb() ' only for offline
                Dim teb = CInt(MiniDumpThreadInfo.Teb)
                If teb <> 0 Then
                    Dim blkintptr = New ProcMemIntPtr
                    Dim tibSize = Marshal.SizeOf(GetType(NT_TIB))
                    blkintptr.data = CType(Array.CreateInstance(GetType(IntPtr), tibSize), IntPtr())
                    Dim dwBytesRead = 0
                    Dim res2 = ReadProcessMemoryDwordsEx(__hProcessTarget, New IntPtr(teb), blkintptr, tibSize, dwBytesRead)
                    Debug.Assert(dwBytesRead = tibSize, "error reading tib")

                    StackBase = New IntPtr(blkintptr.data(1).ToInt32)
                    StackLimit = New IntPtr(blkintptr.data(2).ToInt32)
                End If
            End Sub


            Public Overrides Function ToString() As String
                Return String.Format("Tid={0,5} Proc={1} Base={2:x8} Lim = {3:x8} Siz={4:n0} {5}", ThreadId, ThreadProc, StackBase.ToInt32, StackLimit.ToInt32, StackBase.ToInt32 - StackLimit.ToInt32, Note)
            End Function
        End Class


        Public Function GetMemStats() As List(Of MemStats)
            Dim lstMemStats = New List(Of MemStats)
            If _ConnectionMode = MemSpectMode.Offline Then
            Else
                If _ConnectionMode = MemSpectMode.OnLine Then
                    ProcComm.FreezeTarget()
                    SendMsg(ProcMsgVerb.GetMemStats, fSendEndMsgSync:=False, dwords:={0})
                    Dim res = GetMsg(4)
                    Dim nItems = BitConverter.ToInt32(res, 0)
                    For i = 0 To nItems - 1
                        res = GetMsg(Marshal.SizeOf(GetType(MemStats)))
                        Dim x = MemStats.FromByteArray(res)
                        lstMemStats.Add(x)
                    Next
                    EndMsgSync()
                End If
            End If
            Return lstMemStats
        End Function

        Public Class MarkerAction
            Public MarkerId As Integer
            Public Markername As String
            Public action As CodeMarkerActionEnum
            Public Overrides Function ToString() As String
                Return String.Format("{0}({1})={2}", Markername, MarkerId, action.ToString())
            End Function
        End Class

        Public Function GetCodeMarkerActions() As List(Of MarkerAction)
            Dim lstActions = New List(Of MarkerAction)
            If _ConnectionMode = MemSpectMode.OnLine Then
                SendMsg(ProcMsgVerb.GetCodeMarkerActions, fSendEndMsgSync:=False, dwords:={0})
                Dim res = GetMsg(4)
                Dim nItems = BitConverter.ToInt32(res, 0)
                For i = 0 To nItems - 1
                    res = GetMsg(8)
                    Dim markerID = BitConverter.ToInt32(res, 0)
                    Dim action = CType(BitConverter.ToInt32(res, 4), CodeMarkerActionEnum)
                    Dim markerName = GetCodeMarkerNameRaw(markerID, fuseOffline:=True)
                    lstActions.Add(New MarkerAction() With {.MarkerId = markerID, .Markername = markerName, .action = action})
                Next
                EndMsgSync()

            End If
            Return lstActions
        End Function

        ''' <summary>
        ''' 
        ''' </summary>
        ''' <param name="fIncludeDeadThreads"> also gets thread total stats</param>
        Public Function GetLiveThreads(Optional fIncludeDeadThreads As Boolean = False) As Dictionary(Of Integer, ThreadInfo) ' threadid, ThreadInfo
            Dim thrdlist As New Dictionary(Of Integer, ThreadInfo) 'key = thread id
            If _ConnectionMode = MemSpectMode.Offline Then
                Dim threads = MiniDumpReader.Singleton.GetThreadInfoFromMinidump
                For Each thr As MINIDUMP_THREAD In threads
                    Dim tinfo = New ThreadInfo With {
                                 ._ThreadId = thr.ThreadId,
                                 .MiniDumpThreadInfo = thr,
                                 .Note = If(thr.ThreadId = _MainThread, "Main ", String.Empty) + "from minidump"
                             }
                    tinfo.GetStackInfoFromTeb()
                    thrdlist.Add(thr.ThreadId, tinfo)
                Next
            ElseIf _ConnectionMode = MemSpectMode.OnLine Then
                ProcComm.FreezeTarget()
                SendMsg(ProcMsgVerb.GetThreadInfo, fSendEndMsgSync:=False, dwords:={0})
                Dim res = GetMsg(4) ' get # of threads
                Dim nItems = BitConverter.ToInt32(res, 0)
                For i = 0 To nItems - 1
                    res = GetMsg(4 * 4) '// no params; returns 4 DWORDS for each thread: (Id, SeqNo, stackBase, stackLimit)
                    Dim tid = BitConverter.ToInt32(res, 0)
                    Dim seqno = BitConverter.ToInt32(res, 4)
                    Dim stackbase = BitConverter.ToInt32(res, 8)
                    Dim stackLimit = BitConverter.ToInt32(res, 12)
                    Dim tinfo = New ThreadInfo With {
                                 ._ThreadId = tid,
                                 .SeqNoStart = seqno,
                                 .StackBase = New IntPtr(stackbase),
                                 .StackLimit = New IntPtr(stackLimit),
                                 .MiniDumpThreadInfo = New MINIDUMP_THREAD
                                 }
                    If seqno = 1 Then
                        tinfo.Note = "Main UI thread"
                    End If
                    If tid = _memSpectThreadId Then
                        tinfo.Note = "MemSpect private thread"
                    End If
                    thrdlist.Add(tid, tinfo)
                Next
                EndMsgSync()
            End If
            If fIncludeDeadThreads Then
                ' now get dead threads
                For Each hp In _HeapList
                    Dim snap = hp.TakeMemSnapshot(fEnableFilter:=False)
                    For Each alloc In snap.Allocs
                        Dim tinfo As ThreadInfo = Nothing
                        Dim tid = alloc.AllocationStruct.ThreadId
                        If tid = 0 Then
                            Throw New Exception("tid=0")
                        End If
                        If Not thrdlist.TryGetValue(tid, tinfo) Then
                            tinfo = New ThreadInfo With {._ThreadId = tid, .Note = "Dead thread"}
                            thrdlist.Add(tid, tinfo)
                        End If
                        tinfo.allocs.Add(alloc)
                        tinfo.nTotAllocs += 1
                        tinfo.nTotSize += alloc.GetSize
                        'tinfo.allocs.Add(alloc)
                    Next
                Next

            End If
            Return thrdlist
        End Function


        'Class for taking and loading snapshots of most of the MemSpect data. 
        'Create snapshot will simply write out the data to disk and all the dictionaries below should remain empty.
        'Load snapshot will populate the dictionaries below and some of the other properties in Common with the snapshot data.
        Public Class OfflineMegaSnapshot
            Public _snapshots As New List(Of Common.MemSnapshot)
            Public _DataFilePath As String = String.Empty ' when creating a snap, empty. Else full path to folder of snap
            Friend _zipPackageToRead As Packaging.Package
            Friend _InstanceMiniDumpReader As MiniDumpReader ' the singleton. Only test code should access this
            Friend _StackFrameDictionary As Dictionary(Of IntPtr, String)
            ' everything else is a heap
            Public SnapFileNames As String() = {
                "ClrClassInfo",
                "CLRObjRefs",
                "Filter",
                "HeapReport",
                "StackFrameDictionary",
                "WorkingSetInfo"
            }

            Sub New(DataFilePath As String)
                _DataFilePath = DataFilePath
            End Sub

            Public Function OffLineFileIsHeapName(strFullPathName As String) As Boolean
                Dim fIsHeap = True
                For Each strName In SnapFileNames
                    If Path.GetFileNameWithoutExtension(strFullPathName).IndexOf(strName, StringComparison.OrdinalIgnoreCase) >= 0 Then
                        fIsHeap = False
                        Exit For
                    End If
                Next
                Return fIsHeap
            End Function

            Public ReadOnly Property ContainsClrData As Boolean
                Get
                    Dim ret = False
                    If GCData._ClrObjRefDict IsNot Nothing Then
                        ret = GCData.GetCLRObjectRefDict.Count > 0
                    End If
                    Return ret
                End Get
            End Property

            Private _ClrClassInfoWrapper As New ClrClassInfo

            Private _WorkingSetList As SortedList(Of IntPtr, WorkingSetInfo)
            Public ReadOnly Property WorkingSetList As SortedList(Of IntPtr, WorkingSetInfo)
                Get
                    If _WorkingSetList Is Nothing Then
                        'Dim deser = GetDeserializerForPath(IO.Path.Combine(_OfflineDataFilePath, "WorkingSetInfo.mhd"))
                        'Me._WorkingSetInfoWrapper = CType(deser.GetEntryObject(), WorkingSetInfoWrapper)
                        'Debug.Assert(Me._WorkingSetDict IsNot Nothing, "err deserializing WorkingSetInfo")
                        _WorkingSetList = New SortedList(Of IntPtr, WorkingSetInfo)(_CompareIntPtr)
                    End If
                    Return _WorkingSetList
                End Get
            End Property

            Private _heapReport As Common.HeapReport
            Public ReadOnly Property heapReport As Common.HeapReport
                Get
                    If _heapReport Is Nothing Then
                        Dim hrptPath = IO.Path.Combine(_DataFilePath, "HeapReport.mhd")
                        If IO.File.Exists(hrptPath) Then

                            Using des = GetDeserializerForPath(hrptPath, Me)
                                Me._heapReport = CType(des.GetEntryObject(), Common.HeapReport)
                            End Using
                        End If
                    End If
                    Return _heapReport
                End Get
            End Property

            Public _realHeapHandleDictionary As New Dictionary(Of IntPtr, IntPtr)()
            Public _systemStringDictionary As New Dictionary(Of IntPtr, String)()
            'Public _codemarkerDictionary As New Dictionary(Of Integer, String)() ' markerid, name
            Public _mappedFilesDictionary As New Dictionary(Of IntPtr, String)()
            Public _heapCreateDictionary As New Dictionary(Of IntPtr, String)()
            Public _clrLoadDictionary As New Dictionary(Of IntPtr, String)()

            Public _allocationStacks As IDictionary(Of IntPtr, IntPtr())

            Public Shared Sub CloseZipPackage()
                If _offlineSnapshot IsNot Nothing AndAlso _offlineSnapshot._zipPackageToRead IsNot Nothing Then
                    UpdateStatusMsg("Closing Zip Package", msgType:=StatusMessageType.CalvinHBuild)
                    _offlineSnapshot._zipPackageToRead.Close()
                    _offlineSnapshot._zipPackageToRead = Nothing
                End If
            End Sub

            Public Shared LoadSnapshotSub As Action(Of Boolean, String) = Nothing

            ''' <summary>
            ''' Create an entire dump into a folder. Threads should be frozen. Respects ClrObjDump setting.
            ''' returns the folder name
            ''' </summary>
            ''' <param name="newDumpFolder">
            ''' Folder into which a folder called "MemSpect8_3_2010 11_15_35AM" is created.
            ''' (pass in null or "", defaults to same folder as MemSpect.exe)
            ''' The only way to freeze is for client process to Freeze/Unfreeze. 
            ''' (Detach (procMsgvrb==Quit) automatically unfreezes)
            ''' Client should track the frozen state.
            ''' MemSpect.exe tracks via the Checkbox.
            ''' to call this from UI, call CreateMegaSnapshotFromUI
            ''' </param>
            Public Shared Function CreateMegaSnapshot(
                                                     ByVal newDumpFolder As String,
                                                     Optional ByVal fUseBackgroundThreads As Boolean = False,
                                                     Optional fCopyMemSpectToo As Boolean = True) As String
                ProcComm.FreezeTarget()
                _MegaSnapShotSymbolDict = New SortedSet(Of IntPtr)(_CompareIntPtr) ' batch sym res for perf

                'before we take a snap, we want to make sure we resolve thead syms
                Dim thrds = GetLiveThreads()
                For Each thd In thrds
                    Dim frame = thd.Value.ThreadProc ' force sym resolution 
                Next

                Dim NewMegaSnap = New OfflineMegaSnapshot(String.Empty)
                If String.IsNullOrEmpty(newDumpFolder) Then
                    newDumpFolder = Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)
                    newDumpFolder = Path.Combine(newDumpFolder, DateTime.Now.ToString.Replace(":", "_").Replace("/", "_").Replace(" ", "_"))
                End If
                If Not IO.Directory.Exists(newDumpFolder) Then
                    System.IO.Directory.CreateDirectory(newDumpFolder)
                End If
                UpdateStatusMsg("Start creating MegaSnapshot " + newDumpFolder + " this could take ->10 mins")
                Dim swatch = New Stopwatch
                swatch.Start()

                UpdateStatusMsg(" MaxSeqNo= " + GetGlobalPassCount.ToString("n0"))

                Dim zipPackageToCreate = Packaging.Package.Open(Path.Combine(newDumpFolder, SnapDataZipName), FileMode.Create)

                Dim minidmpEv = New ManualResetEventSlim(False)
                Dim snapEv = New ManualResetEventSlim(False)
                Dim lamMiniDump = Sub()
                                      ' write Mini to Mega dir
                                      Dim statSuffix = String.Empty
                                      If _IsCalvinHBuild Then
                                          statSuffix += " Tid=" + GetCurrentThreadId().ToString
                                      End If
                                      Dim mdumpFilename = Path.Combine(newDumpFolder, "minidump.dmp")
                                      UpdateStatusMsg(" -Writing minidump " + mdumpFilename + statSuffix)
                                      Dim stat = CreateMiniDumpFile(mdumpFilename)
                                      UpdateStatusMsg(" --" + stat + statSuffix)
                                      Dim mscordacwksSrc = Path.Combine(
                                          Path.GetDirectoryName(GetType(Integer).Assembly.Location),
                                          "MSCorDacWks.dll"
                                          )
                                      ' copy MSCorDacWks too so if the drop disappears, can use VsDbg.dll in WinDbg
                                      If File.Exists(mscordacwksSrc) Then
                                          File.Copy(mscordacwksSrc,
                                                    Path.Combine(newDumpFolder,
                                                                 Path.GetFileName(mscordacwksSrc)
                                                                 ))
                                      End If
                                      If fCopyMemSpectToo Then
                                          ' write the entire mspect install to the folder to avoid version issues
                                          Dim mspectInstalldir = Path.GetDirectoryName(_iniFileName)
                                          Dim msDir = Path.Combine(newDumpFolder, "MemSpect")
                                          IO.Directory.CreateDirectory(msDir)
                                          For Each msfile In IO.Directory.GetFiles(mspectInstalldir)
                                              If Not Path.GetExtension(msfile).ToLower.EndsWith("etl") Then
                                                  Try
                                                      Dim destFile = Path.Combine(msDir, Path.GetFileName(msfile))
                                                      IO.File.Copy(msfile, destFile)
                                                      Dim finfo = New FileInfo(destFile)
                                                      If finfo.IsReadOnly Then ' turn off readonly in case where copy/overwrite snapshots
                                                          finfo.IsReadOnly = False
                                                      End If

                                                  Catch ex As Exception
                                                      UpdateStatusMsg("warning: " + msfile + " " + ex.ToString)
                                                  End Try
                                              End If
                                          Next
                                      End If
                                      minidmpEv.Set()

                                  End Sub
                Dim lamSnapShot = Sub()
                                      Dim statSuffix = String.Empty
                                      If _IsCalvinHBuild Then
                                          statSuffix += " Tid=" + GetCurrentThreadId().ToString
                                      End If
                                      UpdateStatusMsg(" -Processing heaps...." + statSuffix)


                                      Dim dataFiles As New List(Of String)()
                                      Common.ReadHeaps(fClearFirst:=True)

                                      'lambda to update file list and serialize an object
                                      Dim EasySerializerLambda = Sub(fileName As String, data As IFastSerializable)
                                                                     Try
                                                                         GC.Collect()
                                                                         UpdateStatusMsg(" -Writing " + fileName)
                                                                         Dim serlizer As Serializer = Nothing
                                                                         If fileName = "HeapReport.mhd" Then
                                                                             ' lazily loaded, so we'll not zip it to avoid race conditions
                                                                             fileName = Path.Combine(newDumpFolder, fileName)
                                                                             serlizer = New Serializer(fileName, data)
                                                                         Else
                                                                             Dim uri = New Uri(fileName, UriKind.Relative)

                                                                             Dim uriFixed = Packaging.PackUriHelper.CreatePartUri(uri) ' prepends "/"

                                                                             Dim part = zipPackageToCreate.CreatePart(
                                                                                 uriFixed,
                                                                                 System.Net.Mime.MediaTypeNames.Application.Zip,
                                                                                 Packaging.CompressionOption.Normal)

                                                                             Dim swriter = New IOStreamStreamWriter(part.GetStream())
                                                                             serlizer = New Serializer(swriter, data)
                                                                         End If
                                                                         serlizer.Close()
                                                                         dataFiles.Add(fileName)
                                                                     Catch ex As Exception
                                                                         UpdateStatusMsg("Failed to serialize data: " + ex.ToString(), fAssert:=True)
                                                                         MemSpectExceptionHandler(ex)
                                                                     End Try

                                                                 End Sub

                                      'serialize the HeapReport information
                                      GC.Collect()

                                      'Dim imanal = Images.ImageAnalyzer.AnalyzeImages(Nothing)
                                      'IO.File.Copy(imanal._StacksFileName, Path.Combine(newDumpFolder, "ImageLoadCallStacks.txt"))
                                      'EasySerializerLambda.Invoke("Images.mhd", imanal)

                                      'serialize each heap
                                      Dim heapCount As Integer = 0
                                      Dim memspectHeapSnap As MemSnapshot = Nothing
                                      For Each heap As CSpyHeap In Common._HeapList
                                          'ignore the filter during snapshot creation.  otherwise, people could take a snapshot with the filter on thinking
                                          'they are getting a full dump but only end up with the filtered data.  If this is needed in the future, it would
                                          'be easy to add an .ini file option.
                                          Dim snapshot As Common.MemSnapshot = heap.TakeMemSnapshot(fEnableFilter:=False)
                                          If heap.IsMemSpectHeap Then
                                              memspectHeapSnap = snapshot
                                          End If
                                          ' add heaphandle because name is not unique
                                          Dim hname = heap.GetHeapName + heap.GetRealOSHeapHandle.ToString("x8") + ".mhd"
                                          '"_pdm.dll!ATLCComObject<CEnumDispatchExObject>CreateInstace _ 79 bytes10c70000.mhd"
                                          hname = New String(hname.Where(Function(x) Not Path.GetInvalidFileNameChars.Contains(x)).ToArray)
                                          '                                          Dim hname = heap.GetHeapName.Replace(":", String.Empty).Replace("+", "_").Replace("*", String.Empty) + heap.GetRealOSHeapHandle.ToString("x8") + ".mhd"
                                          EasySerializerLambda.Invoke(hname, snapshot)

                                          heapCount += 1
                                      Next

                                      ClrClassInfo.GetAllClassInfos(memspectHeapSnap)

                                      EasySerializerLambda.Invoke("ClrClassInfo.mhd", NewMegaSnap._ClrClassInfoWrapper)


                                      EasySerializerLambda.Invoke("Filter.mhd", _GlobalFilter)

                                      Dim aworkingSetInfoWrapper As New WorkingSetInfoWrapper

                                      EasySerializerLambda.Invoke("WorkingSetInfo.mhd", aworkingSetInfoWrapper)

                                      'VsClearSymbols() ' release native resources.... NO: we don't want to release because user might continue on with current MemSpect session
                                      'if we are tracking clr objects, serialize that information.
                                      SendMsg(ProcMsgVerb.ClrObjTrk, fSendEndMsgSync:=True, dwords:={2}) ' read current setting
                                      Dim sResult = Marshal.ReadInt32(_SharedMemAddr)
                                      Dim pCProcessGC = Marshal.ReadIntPtr(_SharedMemAddr, 4) ' non-zero indicates CLR attached

                                      If sResult = 1 AndAlso pCProcessGC <> IntPtr.Zero Then
                                          Dim reflist = GCData.GetCLRObjectRefDict()

                                          'The Serializer object doesn't support writing collections as the entryobject in a file, so these wrappers
                                          'are used to create a serializable object that contains just the collection.  This could probably be generalized
                                          'at some point in the future.
                                          Dim CLRObjectRefsWrapper As New CLRObjRefsWrapper()
                                          EasySerializerLambda.Invoke("CLRObjRefs.mhd", CLRObjectRefsWrapper)
                                      Else
                                          'no tracking so don't read refs.
                                      End If
                                      Dim nSyms = _MegaSnapShotSymbolDict.Count
                                      UpdateStatusMsg(String.Format(" -Resolving {0} Symbols", nSyms))
                                      For Each addr In _MegaSnapShotSymbolDict ' resolve them in addr order for perf
                                          nSyms -= 1
                                          Dim sym = ResolveAddressToSymbol(addr)
                                          If nSyms Mod 2500 = 0 Then
                                              UpdateStatusMsg(String.Format(" -{0} syms to resolve ({1:x8}={2})", nSyms, addr.ToInt32, sym))
                                          End If
                                      Next
                                      _MegaSnapShotSymbolDict = Nothing
                                      'Serialize the call stack frame dictionary
                                      Dim StackFrameDictionaryWrapper As New StackDictionaryWrapper()
                                      EasySerializerLambda.Invoke("StackFrameDictionary.mhd", StackFrameDictionaryWrapper)

                                      Try
                                          If GetPrivateProfileInt(ProfileStringSection, "IncludeHeapReport", 1, _iniFileName) = 1 Then
                                              Dim hprpt = New Common.HeapReport
                                              EasySerializerLambda.Invoke("HeapReport.mhd", hprpt)
                                          Else
                                              UpdateStatusMsg("Skipping heapreport due to 'IncludeHeapReport' setting in INI file")
                                          End If
                                      Catch ex As Exception
                                          MemSpectExceptionHandler(ex, "Exception while serializing HeapReport: skipping")

                                      End Try

                                      'writes a master list of all the files we serialized
                                      Dim sw As New StreamWriter(Path.Combine(newDumpFolder, DumpedFilesMHDFileName))
                                      For Each file As String In dataFiles
                                          sw.WriteLine(file)
                                      Next
                                      sw.Close()

                                      File.Copy(_iniFileName, Path.Combine(newDumpFolder, Path.GetFileName(_iniFileName)))

                                      snapEv.Set()

                                  End Sub

                If fUseBackgroundThreads Then
                    ThreadPool.QueueUserWorkItem(Sub(parm As Object)
                                                     lamMiniDump.Invoke()
                                                 End Sub, minidmpEv)

                    ThreadPool.QueueUserWorkItem(Sub(parm As Object)
                                                     lamSnapShot.Invoke()
                                                 End Sub, snapEv)

                    minidmpEv.Wait()

                    GC.Collect()

                    snapEv.Wait()
                Else
                    lamMiniDump.Invoke()

                    GC.Collect()

                    lamSnapShot.Invoke()

                End If
                zipPackageToCreate.Close()
                NewMegaSnap.ClearAll()
                GC.Collect()
                File.WriteAllText(Path.Combine(newDumpFolder, "Notes.txt"), CreateMegaSnapNotes(), Encoding.Unicode)
                Dim elapsed = swatch.Elapsed
                UpdateStatusMsg("Done creating MegaSnapshot in " + elapsed.ToString("hh\:mm\:ss\.ff") + " " + newDumpFolder)
                Return newDumpFolder
            End Function

            Public Shared Function CreateMiniDumpFile(ByVal mdumpFullPathFilename As String) As String
                Dim res = String.Empty
                If VsAssertWriteMiniDumpEx(
                    _hProcessTarget,
                    _TargetProc.Id,
                    New StringBuilder(mdumpFullPathFilename),
                    _MINIDUMP_TYPE.MiniDumpWithFullMemory +
                    _MINIDUMP_TYPE.MiniDumpWithFullMemoryInfo +
                    _MINIDUMP_TYPE.MiniDumpWithHandleData
                    ) Then
                    res = ("done writing Minidump " + mdumpFullPathFilename)
                Else
                    Dim lerr = Marshal.GetLastWin32Error
                    res = ("failed minidump creation Lasterr= " + lerr.ToString("x8") + " " + GetErrorMessageFromWin32LastError(lerr))
                End If
                Return res
            End Function

            Public Shared Function CreateMegaSnapNotes() As String
                Dim sb As New StringBuilder()
                sb.AppendLine("MemSpect Snapshot notes")
                'Dim sb As New StringBuilder(_txtStatus.Text)
                ' dump out heap summary:quick way to spot regressions by comparing Prism runs without loading snap
                Dim frmt = "Summary {0,-65} {1,20} {2,20}"
                sb.AppendLine(String.Format(frmt,
                                            "Heap List", "CurSize", "nLive"))
                'Summary Heap List                                                                      CurSize                nLive
                'Summary __MemSpect                                                                      129720                 3243
                'Summary __Process Heap                                                                  315395                 2551
                'Summary _clr.dll!DebuggerHeap::Init + 25 bytes                                            1704                    3
                'Summary _clr.dll!DllPreInit::DllPreInit + 20 bytes                                       28373                   68
                'Summary _comctl32.dll!_heap_init + 27 bytes                                               7262                    6
                'Summary _gdiplus.dll!InternalGdiplusStartup + 38 bytes                                 1055776                 2113
                'Summary _mscoreei.dll!_heap_init + 15 bytes                                              11950                   68
                'Summary _mscoreei.dll!CreateProcessExecutableHeap + 15 bytes                                 0                    0
                'Summary _MSVCR100_CLR0400.dll!_heap_init + 15 bytes                                      12678                   73
                'Summary _USP10.dll!UspInitMemory + 66 bytes                                              54818                   19
                'Summary _uxtheme.dll!ThemeHeapStaticInitialize + 30 bytes                                 3944                  155
                'Summary ~MemSpectDll.dll!_heap_alloc_base + 83 bytes                                      3408                    6
                'Summary ~MSCOREE.DLL!_calloc_impl + 196 bytes                                                0                    0
                'Summary ~msvcrt.dll!_calloc_impl + 310 bytes                                             15176                   16


                For Each hp In _HeapList
                    sb.AppendLine(String.Format(frmt,
                                                hp.HeapName,
                                                hp.CurTotBytes,
                                                hp.CurNumAllocs
                                                ))
                Next

                sb.AppendLine()
                If Not String.IsNullOrEmpty(_GlobalFilter.LeakMultipleRawText) AndAlso _GlobalFilter.LeakMultipleRawText.Contains(vbCr) Then
                    sb.AppendLine("<SeqNos>")
                    For Each lin In _GlobalFilter.LeakMultipleRawText.Split({CChar(vbCr), CChar(vbLf)})
                        lin = lin.Replace(vbCr, "").Replace(vbLf, "")
                        If Not String.IsNullOrEmpty(lin) Then
                            sb.AppendLine(lin)
                        End If
                    Next
                    sb.AppendLine("</SeqNos>")
                End If

                sb.AppendLine()
                sb.AppendLine("Status")

                For Each status In MemSpectLog
                    sb.AppendLine(status)
                Next

                sb.AppendLine()
                Dim sysdir = System.Environment.GetEnvironmentVariable("systemroot") ' c:\windows
                Dim systeminfoFile = IO.Path.Combine(sysdir, "system32\systeminfo.exe")
                If IO.File.Exists(systeminfoFile) Then
                    sb.AppendLine("Systeminfo:")
                    Dim psinfo = New ProcessStartInfo With {
                                          .FileName = systeminfoFile,
                                          .UseShellExecute = False,
                                          .RedirectStandardOutput = True
                                      }
                    Dim p = Process.Start(psinfo)
                    Dim sr = p.StandardOutput
                    Dim strResult = sr.ReadToEnd
                    sb.Append(strResult)
                End If

                sb.AppendLine()

                sb.AppendLine(String.Format("CLR version {0}", System.Environment.Version.ToString))
                sb.AppendLine()

                sb.AppendLine("Running Processes")
                For Each proc In System.Diagnostics.Process.GetProcesses.OrderBy(Function(p As Process) p.ProcessName)
                    Try

                        sb.AppendLine(String.Format("{0,6} {1,-20} {2,12:n0} {3,4} {4,25} {5,-50} {6,-50}",
                                                    proc.Id,
                                                    proc.ProcessName,
                                                    proc.WorkingSet64,
                                                    proc.Threads.Count,
                                                    proc.StartTime,
                                                    proc.MainWindowTitle,
                                                    proc.MainModule.FileName
                                                    ))
                    Catch ex As Exception
                        sb.AppendLine(proc.ProcessName + " " + ex.Message)

                    End Try

                Next
                ' now determine if this was a VS instance, and get the VS Sqm InstanceId
                Dim vsVer = String.Empty
                For Each possibleVer In {"15.0", "14.0", "12.0"}
                    Using regkey = Microsoft.Win32.Registry.LocalMachine.OpenSubKey("Software\Microsoft\VisualStudio\" + possibleVer)
                        If regkey IsNot Nothing Then
                            vsVer = possibleVer
                            Exit For
                        End If
                    End Using
                Next
                If Not String.IsNullOrEmpty(vsVer) Then
                    Dim sqmKey = String.Format("Software\Microsoft\VSCommon\{0}\SQM\PIDs\{1}", vsVer, _TargetProcessId)
                    Using regkey = Microsoft.Win32.Registry.CurrentUser.OpenSubKey(sqmKey)
                        If regkey IsNot Nothing Then
                            Dim vsInstanceId = CStr(regkey.GetValue("VSInstanceId"))
                            If Not String.IsNullOrEmpty(vsInstanceId) Then
                                sb.AppendLine()
                                sb.AppendLine("VSInstanceId DATAID_VS_InstanceId from SQM regkey " + sqmKey + " = " + vsInstanceId)
                                sb.AppendLine()
                            End If
                        End If
                    End Using

                End If

                sb.AppendLine()
                sb.AppendLine("Env vars:")
                For Each envvar As String In From env In System.Environment.GetEnvironmentVariables.Keys Order By env

                    sb.AppendLine(envvar + "=" + System.Environment.GetEnvironmentVariable(envvar))
                Next


                '                sb.AppendLine("_NT_SYMBOL_PATH = " + System.Environment.GetEnvironmentVariable("_NT_SYMBOL_PATH"))
                sb.AppendLine()
                sb.AppendLine()
                sb.AppendLine("Contents of " + _iniFileName)
                sb.Append(IO.File.ReadAllText(_iniFileName))
                Return sb.ToString
            End Function

            ''' <summary>
            ''' loads a MemSpect snapshot
            ''' </summary>
            ''' <param name="fWaitTilDone">synchronously execute</param>
            ''' <param name="snapName">fullpath to folder (not dumpedfiles.mhd)</param>
            Public Shared Sub LoadMegaSnapshot(ByVal fWaitTilDone As Boolean, ByVal snapName As String)

                If Not String.IsNullOrEmpty(snapName) Then
                    If (snapName.ToLower.EndsWith(".mhd")) Then
                        snapName = Path.GetDirectoryName(snapName)
                    End If
                End If
                _ConnectionMode = MemSpectMode.Offline
                _IsClientOutOfProcess = False

                If OfflineMegaSnapshot.LoadSnapshotSub Is Nothing Or fWaitTilDone Then
                    OfflineMegaSnapshot.LoadMegaSnapshotEx(snapName)
                    ReadHeaps()
                Else
                    OfflineMegaSnapshot.LoadSnapshotSub.Invoke(fWaitTilDone, snapName)
                End If
            End Sub
            Public Function GetFilesInSnapshot() As String()
                If _zipPackageToRead Is Nothing Then
                    Dim snapDatazipFilePath = Path.Combine(_DataFilePath, SnapDataZipName)

                    If File.Exists(snapDatazipFilePath) Then
                        _zipPackageToRead = Packaging.Package.Open(snapDatazipFilePath, FileMode.Open, FileAccess.Read, FileShare.Read)
                        UpdateStatusMsg("Opening zip package " + _DataFilePath, msgType:=StatusMessageType.CalvinHBuild)
                    End If
                End If
                Dim files = ReadLinesOfFile(Path.Combine(_DataFilePath, DumpedFilesMHDFileName))
                Return files
            End Function

            Private Shared Sub LoadMegaSnapshotEx(dataFilePath As String)
                Try
                    Dim swatch = New Stopwatch
                    swatch.Start()
                    Dim LoadedMegaSnap = New OfflineMegaSnapshot(dataFilePath)

                    LoadedMegaSnap.ClearAll()

                    Dim fStackStorageMode = GetPrivateProfileInt(ProfileStringSection, "StackStorageMode", 0, _iniFileName)
                    If fStackStorageMode = 0 Then
                        LoadedMegaSnap._allocationStacks = New Dictionary(Of IntPtr, IntPtr())()
                    Else
                        LoadedMegaSnap._allocationStacks = New MapFileDict.MapFileDict(Of IntPtr, IntPtr())(
                        ulInitialSize:=AllocationGranularity * 16,
                        uiViewSize:=AllocationGranularity * 4
                        )
                    End If

                    Common._offlineSnapshot = LoadedMegaSnap

                    'read in all the files contained in this dump
                    Dim files = LoadedMegaSnap.GetFilesInSnapshot()

                    'load each file as it occurs
                    For Each file As String In files
                        UpdateStatusMsg("Loading " + file)
                        Dim fullPath As String = Path.Combine(LoadedMegaSnap._DataFilePath, file)
                        If (file.EndsWith("HeapReport.mhd", StringComparison.OrdinalIgnoreCase)) Then
                            Continue For  '' on demand
                        End If
                        Try
                            Using deserializer As Deserializer = OfflineMegaSnapshot.GetDeserializerForPath(fullPath, LoadedMegaSnap)
                                If (file.EndsWith("StackFrameDictionary.mhd", StringComparison.OrdinalIgnoreCase)) Then
                                    Dim stackWrapper As StackDictionaryWrapper = CType(deserializer.GetEntryObject(), StackDictionaryWrapper)
                                ElseIf (file.EndsWith("filter.mhd", StringComparison.OrdinalIgnoreCase)) Then
                                    _GlobalFilter = CType(deserializer.GetEntryObject, GlobalFilter)
                                    _GlobalFilter.RefreshFilterDisplay()
                                ElseIf (file.EndsWith("WorkingSetInfo.mhd", StringComparison.OrdinalIgnoreCase)) Then
                                    Dim aworkingSetInfoWrapper = CType(deserializer.GetEntryObject, WorkingSetInfoWrapper)
                                    LoadedMegaSnap._WorkingSetList = aworkingSetInfoWrapper.WorkingSetList
                                ElseIf (file.EndsWith("ClrClassInfo.mhd", StringComparison.OrdinalIgnoreCase)) Then
                                    Dim aClassLayoutsWrapper = CType(deserializer.GetEntryObject, ClrClassInfo)


                                ElseIf (file.EndsWith("CLRObjRefs.mhd", StringComparison.OrdinalIgnoreCase)) Then
                                    'Dim clrWrapper As CLRObjRefsWrapper = CType(deserializer.GetEntryObject(), CLRObjRefsWrapper)

                                    CLRObjRefsWrapper.ClrObjRefFileName = IO.Path.Combine(LoadedMegaSnap._DataFilePath, file)
                                    'no longer need to do this since we are in offline mode.  the one feature in UIBase that keys off
                                    'this being set now uses the OfflineMegaSnapshot.ContainsClrData property to determine if clr data
                                    'is available.
                                    'If VBDiagMarginBase._instanceList.Count > 0 Then
                                    '    Dim margb = VBDiagMarginBase._instanceList(0)
                                    '    margb._chkClrObjects.Dispatcher.Invoke(Sub()
                                    '                                               margb._chkClrObjects.IsChecked = True
                                    '                                           End Sub)
                                    'End If
                                Else
                                    Dim snapshot As Common.MemSnapshot = CType(deserializer.GetEntryObject(), Common.MemSnapshot)
                                    LoadedMegaSnap._snapshots.Add(snapshot)
                                End If
                            End Using

                        Catch ex As Exception
                            UpdateStatusMsg("Exception loading " + file + vbCrLf + ex.ToString)
                        End Try
                    Next
                    ' now parse Notes.txt for SeqNos
                    Dim lines = LoadedMegaSnap.ReadLinesOfFile(IO.Path.Combine(LoadedMegaSnap._DataFilePath, "notes.txt"))
                    Dim fInSeqNos = False
                    Dim txtSeqnos = String.Empty
                    Dim numSeqNoSets = 0
                    For Each line In lines
                        If Not fInSeqNos Then
                            If line.StartsWith("<SeqNos>") Then
                                If numSeqNoSets > 0 Then
                                    UpdateStatusMsg("Warning: got > 1 set of <SeqNos> in snapshot notes", fAssert:=False, msgType:=StatusMessageType.AlertMsgBox)
                                End If
                                fInSeqNos = True
                                numSeqNoSets += 1
                            End If
                        Else
                            If line.StartsWith("</SeqNos>") Then
                                fInSeqNos = False
                                'Exit For ' continue to see if there's another set
                            Else
                                line = Trim(line)
                                If Not String.IsNullOrEmpty(line) Then
                                    txtSeqnos += line + vbCrLf
                                End If
                            End If
                        End If
                    Next
                    If Not String.IsNullOrEmpty(txtSeqnos) Then
                        _GlobalFilter.LeakMultipleRawText = txtSeqnos
                    End If
                    UpdateStatusMsg("# stacks loaded = " + HeapAllocationContainer._NumInstances.ToString("n0"))
                    Dim elapsed = swatch.Elapsed
                    If Not String.IsNullOrEmpty(CLRObjRefsWrapper.ClrObjRefFileName) Then
                        ' put thread stuff here so no thread contention on zip package
                        ThreadPool.QueueUserWorkItem(Sub(parm As Object)
                                                         CLRObjRefsWrapper.LoadFromOffLineSnapshot()
                                                     End Sub)
                        UpdateStatusMsg(
                            String.Format("Snap Loaded (ClrObjref Load pending) in  " + elapsed.ToString("hh\:mm\:ss\.ff"))
                            )
                    Else
                        OfflineMegaSnapshot.CloseZipPackage()
                        UpdateStatusMsg(
                            String.Format("Snap Loaded in " + elapsed.ToString("hh\:mm\:ss\.ff"))
                            )
                    End If

                Catch ex As Exception
                    MemSpectExceptionHandler(ex)
                End Try
            End Sub

            Private Sub ClearAll()
                If _WorkingSetList IsNot Nothing Then
                    _WorkingSetList = Nothing
                End If
                _snapshots.Clear()

                GCData.ClearData()

                _realHeapHandleDictionary.Clear()
                _systemStringDictionary.Clear()
                _mappedFilesDictionary.Clear()
                _heapCreateDictionary.Clear()
                _clrLoadDictionary.Clear()
                ClrClassInfo.g_DictClassLayouts.Clear()

                _allocationStacks = Nothing
                CLRObjRefsWrapper.ClrObjRefFileName = Nothing
            End Sub

            Friend Function ReadLinesOfFile(ByVal fname As String) As String()
                Dim res As String()
                If True OrElse _zipPackageToRead Is Nothing Then
                    res = IO.File.ReadAllLines(fname)
                Else
                    Dim dumpedFilesPart = _zipPackageToRead.GetParts().Where(Function(p) CompareUriWithFileName(p.Uri, fname)).Single()
                    'Dim x = New StreamWriter(Path.GetTempFileName)
                    'zzz.GetStream().w()
                    Dim lst As New List(Of String)
                    Using sr As New StreamReader(dumpedFilesPart.GetStream)
                        While True
                            Dim lin = sr.ReadLine()
                            If lin Is Nothing Then
                                Exit While
                            End If
                            lst.Add(lin)
                        End While
                    End Using
                    res = lst.ToArray
                End If
                Return res
            End Function

            Friend Shared Function CompareUriWithFileName(ByVal uri As Uri, ByVal fname As String) As Boolean
                Dim fAreEqual = False
                Dim justfname = Path.GetFileName(fname).ToLower
                Dim tmpUri = uri.OriginalString.Replace("%20", " ")
                fAreEqual = tmpUri.ToLower.Contains(justfname)
                Return fAreEqual
            End Function

            Private _SnapShotVersion As String = String.Empty
            Friend Function GetMegaSnapShotVersion(Optional ByVal path As String = "") As String
                If String.IsNullOrEmpty(_SnapShotVersion) Then
                    Try
                        Debug.Assert(Not String.IsNullOrEmpty(path), "why is path null for GetSnapVersion?")
                        '9/27/2010 3:30:31 PM MemSpect Version = 100928
                        Dim notesVerLines = ReadLinesOfFile(System.IO.Path.Combine(_DataFilePath, "notes.txt"))
                        Dim notesVerLine = (From lin In notesVerLines Where lin.Contains("MemSpect Version") OrElse
                                            lin.Contains("MemSpect UI Ver") OrElse
                                            lin.Contains("MemSpect DLL Ver")
                                            ).First
                        Dim verPos = notesVerLine.IndexOf("=")
                        _SnapShotVersion = notesVerLine.Substring(verPos + 1).Trim

                    Catch ex As Exception
                        ' for any exception, we'll just use an old vers
                        UpdateStatusMsg("Excpt getting snapShotVersion: " + ex.ToString)
                        _SnapShotVersion = "100928"
                    End Try
                End If
                Return _SnapShotVersion
            End Function

            Friend Shared Function GetDeserializerForPath(ByVal fullPath As String, ByVal offlineSnapshot As OfflineMegaSnapshot, Optional strm As Stream = Nothing) As Deserializer
                If strm Is Nothing Then
                    If offlineSnapshot._zipPackageToRead Is Nothing Then
                        If Not File.Exists(fullPath) Then
                            fullPath = fullPath.Replace(" ", "%20")
                        End If
                        If Not File.Exists(fullPath) Then
                            Throw New FileNotFoundException(fullPath)
                        End If
                    Else
                        Dim part = offlineSnapshot._zipPackageToRead.GetParts.Where(Function(p) CompareUriWithFileName(p.Uri, fullPath)).SingleOrDefault
                        If part IsNot Nothing Then
                            strm = part.GetStream
                        End If
                    End If
                    If strm Is Nothing Then
                        strm = New FileStream(fullPath, FileMode.Open, FileAccess.Read) ' allow readonly access
                    End If

                End If

                'centralized place to create deserializers for the snapshots.  a factory has to be registered for each type if it
                'is to be deserialized, so we do it all once here.
                Dim strmreader = New IOStreamStreamReader(strm)
                Dim deserializer As New Deserializer(strmreader, fullPath)
                Dim ver = "120502"
                If offlineSnapshot IsNot Nothing Then
                    ver = offlineSnapshot.GetMegaSnapShotVersion(fullPath)
                End If

                Dim arrDeserTypes = {
                            GetType(MemSnapshot),
                            GetType(ClrClassInfo),
                            GetType(ClassLayoutDetail),
                            GetType(ClsLayoutFieldInfo),
                            GetType(WorkingSetInfoWrapper),
                            GetType(WorkingSetInfo),
                            GetType(CSpyHeap),
                            GetType(HeapAllocationContainer),
                            GetType(HeapAllocationStruct),
                            GetType(TrackBlockStruct),
                            GetType(HeapAllocationContainer.nrlsData),
                            GetType(HeapAllocationContainer.Location),
                            GetType(ClrObjDump),
                            GetType(HeapReport),
                            GetType(HeapReport.HeapData),
                            GetType(NativeImports.PROCESS_HEAP_ENTRY),
                            GetType(NativeImports.UNION_BLOCK),
                            GetType(NativeImports.STRUCT_BLOCK),
                            GetType(NativeImports.STRUCT_REGION),
                            GetType(StackDictionaryWrapper),
                            GetType(CLRObjRefsWrapper),
                            GetType(GlobalFilter)
                          }

                Array.ForEach(arrDeserTypes, Sub(typeToDeserialize As Type)
                                                 Dim deserializeFunc As Func(Of IFastSerializable) =
                                                     Function() CType(System.Activator.CreateInstance(typeToDeserialize), IFastSerializable)
                                                 'Dim deserializeFuncs = Expressions.Expression.Lambda(Of IFastSerializable)(Expressions.Expression.[New](typeToDeserialize)).Compile
                                                 If ver < "101012" Then
                                                     Dim str = typeToDeserialize.FullName.Replace("MemSpect", "Microsoft.VisualBasic.Editor") ' due to namespace change
                                                     deserializer.RegisterFactory(str, deserializeFunc)
                                                 Else
                                                     deserializer.RegisterFactory(typeToDeserialize, deserializeFunc)
                                                 End If

                                             End Sub)

                Return deserializer
            End Function

            Public Function TryGetStackFrame(ByVal frameAddr As IntPtr, ByRef stackFrame As String) As Boolean
                If StackFrameDictionary.ContainsKey(frameAddr) Then
                    stackFrame = StackFrameDictionary(frameAddr)
                    Return True
                Else
                    stackFrame = String.Empty
                    Return False
                End If
            End Function

            Public Function TryGetMappedFileForAllocation(ByVal allocAddr As IntPtr, ByRef fileName As String) As Boolean
                If Common._offlineSnapshot._mappedFilesDictionary.ContainsKey(allocAddr) Then
                    fileName = Common._offlineSnapshot._mappedFilesDictionary(allocAddr)
                    Return True
                Else
                    fileName = String.Empty
                    Return False
                End If
            End Function


            Public Function TryGetSnapshotForHeapSpyPtr(ByVal heapSpyPtr As IntPtr, ByRef snapshot As Common.MemSnapshot) As Boolean
                Dim snaps = Me._snapshots.Where(Function(item As MemSnapshot)
                                                    If item.SpyHeap.HeapHandleSpy = heapSpyPtr Then
                                                        Return True
                                                    End If
                                                    Return False
                                                End Function)
                If snaps.Count() = 1 Then
                    snapshot = snaps.First()
                    Return True
                ElseIf snaps.Count() > 1 Then
                    UpdateStatusMsg("Found more than one heap for HeapSpyPtr " + heapSpyPtr.ToString("x8") + ".  Using First()", fAssert:=True)
                    snapshot = snaps.First()
                    Return True
                Else
                    snapshot = Nothing
                End If

                Return False
            End Function

        End Class


        Public Class WorkingSetInfoWrapper
            Inherits SerializableObject
            Friend WorkingSetList As SortedList(Of IntPtr, WorkingSetInfo)

            Public Overrides Sub FromStream(ByVal deserializer As Deserializer, ByVal versionFromStream As Integer)
                Dim cnt = deserializer.ReadInt
                WorkingSetList = New SortedList(Of IntPtr, WorkingSetInfo)(_CompareIntPtr)
                For i = 1 To cnt
                    Dim key = New IntPtr(deserializer.ReadInt)
                    Dim wsinfo = CType(deserializer.ReadObject(), WorkingSetInfo)
                    WorkingSetList.Add(key, wsinfo)
                Next
            End Sub

            Public Overrides Sub ToStream(ByVal serializer As Serializer)
                Dim dict = WorkingSetInfo.GetWorkingSetDict()
                serializer.Write(dict.Count)
                For Each itm In dict
                    serializer.Write(itm.Key.ToInt32)
                    serializer.WritePrivate(itm.Value)
                Next
            End Sub
        End Class

        Friend _stackFrameDictOnLine As New Dictionary(Of IntPtr, String)
        Public Property StackFrameDictionary As Dictionary(Of IntPtr, String)
            Get
                If _ConnectionMode = MemSpectMode.OnLine Then
                    Return _stackFrameDictOnLine
                End If
                Return _offlineSnapshot._StackFrameDictionary
            End Get
            Set(value As Dictionary(Of IntPtr, String))
                If _ConnectionMode = MemSpectMode.OnLine Then
                    _stackFrameDictOnLine = value
                Else
                    _offlineSnapshot._StackFrameDictionary = value
                End If
            End Set
        End Property

        'wrapper classes used to make serializing this data easier
        Public Class StackDictionaryWrapper
            Inherits SerializableObject

            Public Overrides ReadOnly Property Version As Integer
                Get
                    Return SerializableObject.CurrentSerializationVersion + 1 ' 4
                End Get
            End Property

            Public Overrides Sub FromStream(ByVal deserializer As Deserializer, ByVal versionFromStream As Integer)
                Dim size = deserializer.ReadInt()
                '                UpdateStatusMsg("StackFrameDict size=" + size.ToString)
                StackFrameDictionary = New Dictionary(Of IntPtr, String)(size)
                For i As Integer = 0 To size - 1
                    Dim addr As IntPtr = CType(deserializer.ReadInt(), IntPtr)
                    Dim frame As String = deserializer.ReadString()
                    StackFrameDictionary.Add(addr, frame)
                Next
                If versionFromStream = SerializableObject.CurrentSerializationVersion + 1 Then
                    Dim cnt = CType(deserializer.ReadInt, Integer)
                    For i As Integer = 0 To cnt - 1
                        Dim key = CType(deserializer.ReadInt, IntPtr)
                        Dim val = CType(deserializer.ReadInt, IntPtr)
                        _stackIndexDict(key) = val
                    Next
                End If
            End Sub

            Public Overrides Sub ToStream(ByVal serializer As Serializer)
                '               UpdateStatusMsg("StackFrameDict size=" + StackFrameDictionary.Count.ToString)
                serializer.Write(StackFrameDictionary.Count)
                For Each entry In StackFrameDictionary
                    serializer.Write(entry.Key.ToInt32())
                    serializer.Write(entry.Value)
                Next
                serializer.Write(_stackIndexDict.Count)
                For Each entry In _stackIndexDict
                    serializer.Write(entry.Key.ToInt32)
                    serializer.Write(entry.Value.ToInt32)
                Next
            End Sub
        End Class

        Public Class CLRObjRefsWrapper
            Inherits SerializableObject
            ' for offline,we want to delay loading because it takes so long
            Private Shared evDoneLoading As New ManualResetEventSlim(False)
            Private Shared _ClrObjRefFileName As String
            Private Shared _exceptList As List(Of Exception)
            Friend Shared _DictOfClrObjects As Dictionary(Of IntPtr, HeapAllocationContainer) ' AllocationStruct.address to clrobj
            Public Shared Property ClrObjRefFileName As String
                Get
                    Return _ClrObjRefFileName
                End Get
                Set(ByVal value As String)
                    _ClrObjRefFileName = value
                    evDoneLoading.Reset()
                End Set
            End Property
            Public Shared Sub WaitTilLoaded()
                If _ConnectionMode = MemSpectMode.Offline AndAlso Not String.IsNullOrEmpty(ClrObjRefFileName) Then
                    If Not evDoneLoading.IsSet Then
                        UpdateStatusMsg("waiting for ClrObjRefData Loaded")
                        evDoneLoading.Wait()
                        UpdateStatusMsg("--done waiting for ClrObjRefData")
                    End If
                    If _exceptList.Count > 0 Then
                        Dim ex = _exceptList(0)
                        _exceptList.Clear() ' prevent multiple throws
                        Throw ex
                    End If
                End If
            End Sub
            Friend Shared Sub LoadFromOffLineSnapshot()
                Try
                    _exceptList = New List(Of Exception)
                    UpdateStatusMsg("Starting CLRObjRefData load in background", msgPriority:=StatusMessagePriority.Low)
                    Debug.Assert(Not String.IsNullOrEmpty(ClrObjRefFileName))
                    _DictOfClrObjects = New Dictionary(Of IntPtr, HeapAllocationContainer)
                    Dim clrobjects = _offlineSnapshot.
                        _snapshots.
                        Where(Function(hp As MemSnapshot) hp.SpyHeap.IsMemSpectHeap).
                        First.
                        Allocs.Where(Function(h) h.TBlkBlockType = BlockTypes.ClrObject)

                    For Each obj In clrobjects
                        _DictOfClrObjects(obj.AllocationStruct.Address) = obj
                    Next


                    Using deserializerObjRef As Deserializer = OfflineMegaSnapshot.GetDeserializerForPath(ClrObjRefFileName, Common._offlineSnapshot)
                        Dim clrWrapper As CLRObjRefsWrapper = CType(deserializerObjRef.GetEntryObject(), CLRObjRefsWrapper)
                    End Using
                Catch ex As Exception
                    _exceptList.Add(ex)
                End Try
                _DictOfClrObjects = Nothing

                OfflineMegaSnapshot.CloseZipPackage()

                UpdateStatusMsg("--Done Background loading CLRObjRefData", msgPriority:=StatusMessagePriority.Low)
                evDoneLoading.Set()
            End Sub
            Public Overrides Sub FromStream(ByVal deserializer As Deserializer, ByVal versionFromStream As Integer)
                Dim size As Integer = deserializer.ReadInt()
                GCData._ClrObjRefDict = New Dictionary(Of IntPtr, ClrObjDump)(size)
                For i As Integer = 0 To size - 1
                    Dim entry As ClrObjDump = CType(deserializer.ReadObject(), ClrObjDump)
                    GCData._ClrObjRefDict.Add(entry.hctnr.AllocationStruct.Address, entry)
                Next

                size = deserializer.ReadInt()
                For i As Integer = 0 To size - 1
                    Dim entry As HeapAllocationContainer = CType(deserializer.ReadObject(), HeapAllocationContainer)
                    GCData.AddGCRoot(entry)
                Next
            End Sub

            Public Overrides Sub ToStream(ByVal serializer As Serializer)

                Dim CLRObjectRefs = GCData.GetCLRObjectRefDict.Values

                serializer.WritePrivateCollection(CLRObjectRefs, "CLRObjectRefs")
                serializer.WritePrivateCollection(GCData.GetGCRootInfo, "GCRootList")
            End Sub
        End Class

        'abstract class for serializing objects with Vance's FastSerializer
        Public MustInherit Class SerializableObject
            Implements IFastSerializable, IFastSerializableVersion

            Public Const CurrentSerializationVersion As Integer = 3
            Public Const CurrentMinimumVersion As Integer = 3

            'Complex types need to implement these methods so that we know how to read and write their contents.
            'As long as the order of reading matches the order of writing, data can be serialized in any way.
            'This allows for conditional reading/writing, for example based on the BlockType of a HeapAllocationContainer.
            Public MustOverride Sub FromStream(ByVal deserializer As Deserializer, ByVal versionFromStream As Integer) Implements IFastSerializable.FromStream
            Public MustOverride Sub ToStream(ByVal serializer As Serializer) Implements IFastSerializable.ToStream

            Public Overridable ReadOnly Property MinimumVersion As Integer Implements IFastSerializableVersion.MinimumVersion
                Get
                    Return SerializableObject.CurrentMinimumVersion
                End Get
            End Property

            Public Overridable ReadOnly Property Version As Integer Implements IFastSerializableVersion.Version
                Get
                    Return SerializableObject.CurrentSerializationVersion
                End Get
            End Property
        End Class

        <DebuggerDisplay("{ToString()}")>
        Public Class CSpyHeap
            Inherits SerializableObject
            Implements IComparable

            Public Sub New()

            End Sub

            Public Property HeapHandleSpy As IntPtr ' not the OS handle: it's the CHeapSpy *
            Private _OsHeapHandle As IntPtr
            Public Property GetRealOSHeapHandle() As IntPtr
                Get
                    If _OsHeapHandle = IntPtr.Zero Then
                        If Common._ConnectionMode = MemSpectMode.Offline Then
                            If Common._offlineSnapshot._realHeapHandleDictionary.ContainsKey(Me.HeapHandleSpy) Then
                                _OsHeapHandle = Common._offlineSnapshot._realHeapHandleDictionary(Me.HeapHandleSpy)
                            End If
                        Else

                            Dim hHandle As Integer
                            If IsMemSpectHeap Then
                                Return ProcComm._hMemSpectHeap
                            Else
                                If ReadProcessMemoryDword(_hProcessTarget, HeapHandleSpy, hHandle, 4, Nothing) = 0 Then
                                    UpdateStatusMsg("getHeapHandle readprocmem failed", fAssert:=True)
                                End If
                            End If
                            _OsHeapHandle = New IntPtr(hHandle)
                        End If
                    End If
                    Return _OsHeapHandle
                End Get
                Set(ByVal value As IntPtr)
                    _OsHeapHandle = value
                End Set
            End Property

            Public Property HeapName As String = String.Empty
            Public Property nLive As Integer
            Public Property CurNumAllocs As Integer
            Public Property CurTotBytes As Integer
            Public Property TotMemAllocated As UInteger ' # Total allocated including freed
            Public Property TotNumAllocs As Integer ' Byte total alloc including freed
            Public Property HeapFileName As String ' sourcefile that defines the heap
            Public Property HeapFileLineNo As Integer
            Public Property IsMemSpectHeap As Boolean ' Memspect special heap
            Public Property nHeaderSize As Integer
            Public Property nTrailerSize As Integer

            Private _IsArenaHeap As Boolean?
            Private _fDidAddArenaHeapHandler As Boolean = False
            ' store headers in a dict for perf
            Private _ArenaHeaderDict As Dictionary(Of IntPtr, ArenaHeader)
            Public Function GetArenaHdrInfo(ByVal hptr As IntPtr) As ArenaHeader
                Dim hdr As ArenaHeader
                If _ArenaHeaderDict.ContainsKey(hptr) Then
                    hdr = _ArenaHeaderDict(hptr)
                Else
                    hdr = ArenaHeader.FromProcMemIntPtr(hptr)
                    _ArenaHeaderDict(hptr) = hdr
                End If
                Return hdr
            End Function

            Public ReadOnly Property IsArenaHeap As Boolean
                Get
                    If Not _IsArenaHeap.HasValue Then
                        _IsArenaHeap = False
                        If Not String.IsNullOrEmpty(HeapName) AndAlso
                                HeapName.StartsWith("_Arenas") Then '_MemSpectDll.dll!ArenaCreated + 27 bytes
                            _IsArenaHeap = True
                            _ArenaHeaderDict = New Dictionary(Of IntPtr, ArenaHeader)
                            If Not _fDidAddArenaHeapHandler Then
                                _fDidAddArenaHeapHandler = True
                                AddHandler TargetUnfrozen, Sub()
                                                               _IsArenaHeap = Nothing
                                                               _ArenaHeaderDict = Nothing
                                                           End Sub
                            End If
                        End If

                    End If
                    Return _IsArenaHeap.Value
                End Get
            End Property

            Public Function CompareTo(ByVal obj As Object) As Integer Implements System.IComparable.CompareTo
                Dim cc = CType(obj, CSpyHeap)
                Dim scomparer = New CaseInsensitiveComparer
                Dim nRes = scomparer.Compare(cc.HeapName, Me.HeapName)
                If nRes = 0 Then
                    If cc.HeapHandleSpy.ToInt32 < Me.HeapHandleSpy.ToInt32 Then
                        nRes = -1
                    Else
                        nRes = 1
                    End If
                End If
                Return nRes
            End Function

            Public Function GetPivot(ByVal dwTargetAddr As IntPtr) As List(Of HeapAllocationContainer)
                Dim heapAllocs = New List(Of HeapAllocationContainer)

                If Common._ConnectionMode = MemSpectMode.Offline Then
                    Dim snap As Common.MemSnapshot = Nothing
                    If Common._offlineSnapshot.TryGetSnapshotForHeapSpyPtr(Me.HeapHandleSpy, snap) Then
                        For Each alloc As HeapAllocationContainer In snap.Allocs
                            If True Then 'Not IsMemSpectHeap OrElse alloc.TblkBlockType <> BlockTypes.IndirectInfo Then
                                Dim stackAddrs = alloc.GetCallStackAddressestoArray
                                If stackAddrs.Contains(CType(dwTargetAddr, IntPtr)) Then
                                    heapAllocs.Add(alloc)
                                End If
                            End If
                        Next
                    End If

                Else
                    Dim blksize = CInt(Marshal.SizeOf(GetType(ProcMemIntPtr)) / Marshal.SizeOf(GetType(Integer)))
                    Dim nIndex As Integer = 0 ' index into returned data
                    Do While True ' loop for all allocations
                        '// 3 params: hHeap, index, StackFrameAddr to pivot. Returns in shared mem PAllocationStruct: return value = # of items. 
                        Dim resArr = SendMsg(ProcMsgVerb.DoPivot, fSendEndMsgSync:=True, dwords:={HeapHandleSpy.ToInt32, nIndex, dwTargetAddr.ToInt32})
                        Dim nAllocs = BitConverter.ToInt32(resArr, 1) '# of allocations that fit in shared mem
                        If nAllocs = 0 Then
                            Exit Do
                        End If
                        Dim nLoopCnt = 0
                        Do While nAllocs > 0 ' loop for sizeof ProcMemBlockDwords chunk
                            Dim nToDoThisLoop = nAllocs
                            If nToDoThisLoop > blksize Then
                                nToDoThisLoop = blksize
                            End If
                            Dim dwBytesRead = 0
                            Dim blocks = CType(Marshal.PtrToStructure(_SharedMemAddr.MyAdd(nLoopCnt * 4 * blksize),
                                                                      GetType(ProcMemIntPtr)
                                                                      ), 
                                                ProcMemIntPtr)
                            For i = 0 To nToDoThisLoop - 1
                                Dim hblkAddr = blocks.data(i)
                                If hblkAddr = IntPtr.Zero Then
                                    Exit For
                                End If
                                Dim hctnr = MakeHeapAllocationContainerFromAllocationStructAddr(hblkAddr, Me)
                                If hctnr IsNot Nothing Then
                                    If True Then ' Not IsMemSpectHeap OrElse hctnr.TblkBlockType <> BlockTypes.IndirectInfo Then
                                        If True Then '  _GlobalFilter.FilterFunction(hctnr) Then
                                            heapAllocs.Add(hctnr)
                                        End If
                                    End If
                                End If
                            Next
                            nAllocs -= nToDoThisLoop
                            nIndex += nToDoThisLoop
                            nLoopCnt += 1
                        Loop   ' done with blocks chunk
                    Loop
                End If
                Return heapAllocs
            End Function

            ''' <summary>
            ''' given just a handle of PHeapSpy (not the OS heap handle), get or make a CSpyHeap 
            ''' </summary>
            Public Shared Function GetOrMakeSpyHeapFromHandle(ByVal hHeap As IntPtr) As CSpyHeap
                Dim pSpyHeap = _HeapList.Where(Function(hp As CSpyHeap) hp.HeapHandleSpy = hHeap).FirstOrDefault
                If pSpyHeap Is Nothing Then ' the heap was created after last time we read heaps
                    Dim dwords(4) As Integer
                    Dim memblk As New ProcMemIntPtr
                    ' the CHeapSpy raw struct
                    If ReadProcessMemoryDwords(_hProcessTarget, hHeap, memblk, 4 * 4, Nothing) = 0 Then
                        UpdateStatusMsg("reading procmem for ReadHeaps failed", fAssert:=True)
                    End If
                    Dim theHeapName = GetStringFromRemoteMem(memblk.data(2), 0, _hProcessTarget)
                    'theHeapName = HeapNameConvertFromRaw(cSpyheapptr, theHeapName)
                    pSpyHeap = New CSpyHeap With {
                        .HeapName = theHeapName,
                        ._OsHeapHandle = memblk.data(0)
                    }
                End If
                Return pSpyHeap
            End Function

            Public Function TakeMemSnapshot(Optional ByVal fEnableFilter As Boolean = True) As MemSnapshot
                Dim rawHeapAllocs = New List(Of HeapAllocationContainer) ' raw
                Dim nCnt = 0
                UpdateStatusMsg("Taking mem snapshot " + HeapName, msgType:=StatusMessageType.StatusBarEntry, msgPriority:=StatusMessagePriority.Low)
                Dim numBTypes = [Enum].GetNames(GetType(BlockTypes)).Count
                Dim aCnt(numBTypes) As Integer
                Dim aSize(numBTypes) As Integer
                Dim retvalSnapShot As MemSnapshot

                If _ConnectionMode = MemSpectMode.Offline Then
                    Dim snapshots = Common._offlineSnapshot._snapshots.Where(Function(item As MemSnapshot) As Boolean
                                                                                 If item.SpyHeap.HeapHandleSpy = Me.HeapHandleSpy Then
                                                                                     Return True
                                                                                 End If
                                                                                 Return False

                                                                             End Function)
                    If snapshots.Count() = 0 Then
                        UpdateStatusMsg("ERROR: Failed to find snapshot for " + Me.HeapName + ".", fAssert:=True)
                        Return MemSnapshot.CreateMemSnapshotFromListAllocs(hp:=Me, srcAllocs:=New List(Of HeapAllocationContainer), fEnableFilter:=fEnableFilter)
                    ElseIf snapshots.Count() > 1 Then
                        UpdateStatusMsg("WARNING: Found more than one snapshot. Using first snapshot found.", fAssert:=True)
                    End If
                    Dim tempSnap As Common.MemSnapshot = snapshots.First()

                    If Not fEnableFilter Then
                        retvalSnapShot = tempSnap
                    Else
                        retvalSnapShot = MemSnapshot.CreateMemSnapshotFromListAllocs(Me, tempSnap.Allocs, fEnableFilter)
                    End If
                Else
                    ProcComm.FreezeTarget()
                    Debug.Assert(_IsClientOutOfProcess)
                    Dim seqfiltLo = -1
                    Dim seqfiltHi = -1
                    If fEnableFilter Then
                        seqfiltLo = UIntegerToInteger(_GlobalFilter.SeqNoLo)
                        seqfiltHi = UIntegerToInteger(_GlobalFilter.SeqNoHi) ' if 0, will be max
                    End If
                    SendMsg(ProcMsgVerb.GetHeapAllocs, fSendEndMsgSync:=False, dwords:={HeapHandleSpy.ToInt32, seqfiltLo, seqfiltHi})

                    If fEnableFilter Then
                        If Not _GlobalFilter.IsFilterMoreThanOnlySeqno Then
                            fEnableFilter = False ' if we're only filtering on seqno, then we've already filtered. Perf for the common filter case
                        End If
                    End If

                    ' stream: while TRUE
                    '   a DWORD indicating nItems
                    '   if none, exit while
                    '   then nItem DWORD addresses, 
                    ' end while
                    While True
                        Dim res = GetMsg(4)
                        If res.Length <> 4 Then
                            Throw New InvalidOperationException("Communications not working correctly")
                        End If

                        Dim nAllocs = BitConverter.ToInt32(res, 0)
                        If nAllocs = 0 Then
                            Exit While
                        End If
                        ' for perf, let's read from the pipe in chunks

                        Dim nChunkSize = -1
                        Dim nChunkOffset = nChunkSize

                        While nAllocs > 0
                            If nChunkOffset = nChunkSize Then
                                nChunkOffset = 0
                                nChunkSize = CInt(IntPtr.Size * Math.Min(2048, nAllocs))
                                res = GetMsg(nChunkSize)
                            End If
                            Dim paddr = IntPtr.op_Explicit(BitConverter.ToInt32(res, nChunkOffset))
                            If paddr <> IntPtr.Zero Then
                                Dim hctr = MakeHeapAllocationContainerFromAllocationStructAddr(paddr, Me)
                                If hctr IsNot Nothing Then
                                    rawHeapAllocs.Add(hctr)
                                End If
                            End If

                            nChunkOffset += IntPtr.Size
                            nAllocs -= 1
                        End While

                    End While



                    'For i = 0 To nAllocs - 1
                    '    res = GetMsg(4)
                    '    Dim paddr = IntPtr.op_Explicit(BitConverter.ToInt32(res, 0))
                    '    If paddr <> IntPtr.Zero Then
                    '        Dim hctr = MakeHeapAllocationContainerFromAllocationStructAddr(paddr, Me)
                    '        If hctr IsNot Nothing Then
                    '            rawHeapAllocs.Add(hctr)
                    '        End If
                    '    End If
                    'Next
                    EndMsgSync()
                    retvalSnapShot = MemSnapshot.CreateMemSnapshotFromListAllocs(Me, rawHeapAllocs, fEnableFilter)
                End If
                UpdateStatusMsg("Done mem snapshot " + HeapName, msgType:=StatusMessageType.StatusBarEntry)
                Return retvalSnapShot
            End Function

            Function GetHeapStatistics() As String
                Dim retval = New Text.StringBuilder
                retval.Append(String.Format("Heap '{0}'", HeapName))
                retval.Append(String.Format("CurNumAllocs = {0:n0} ", CurNumAllocs))
                retval.Append(String.Format("CurNumByteAllocated = {0:n0} ", CurTotBytes))
                retval.Append(String.Format("NumByteAllocated = {0:n0} ", TotMemAllocated))
                retval.Append(String.Format("NumAllocs = {0:n0} ", TotNumAllocs))
                Return retval.ToString
            End Function

            Public Function GetHeapName() As String
                Dim str = String.Empty
                If IsMemSpectHeap Then
                    str = MemSpectHeapName
                Else
                    str = HeapName
                End If

                Return str
            End Function

            Public Overrides Function ToString() As String
                Dim strHeapname = String.Empty
                If Common._ConnectionMode = MemSpectMode.Offline Then
                    If Common._offlineSnapshot._realHeapHandleDictionary.ContainsKey(Me.HeapHandleSpy) Then
                        strHeapname = HeapName + " Handle = " + Common._offlineSnapshot._realHeapHandleDictionary(Me.HeapHandleSpy).ToString("x8")
                    End If
                Else
                    strHeapname = HeapName + " Handle = " + GetRealOSHeapHandle.ToInt32.ToString("x8")
                End If
                If IsMemSpectHeap Then
                    strHeapname += " Contains callstack details for Managed Objects, Classes, Files/VirtualAllocs, heapCreates, Codemarkers" + vbCrLf +
                        "This is a MemSpect created private heap: it's a real Win32 heap, but the only things in it are those that MemSpect puts in it" + vbCrLf +
                        "This way, the same mechanism to track/show callstacks and allocations can be used for Win32 Heaps and VirtualAllocs, CLR Objs, etc." + vbCrLf +
                        "CurSize shows as 0 because getting a meaningful Size from this heap is problematic: " + vbCrLf +
                        "  should the size of a VirtualAlloc call with MEM_RESET or MEM_COMMIT of an already Alloc'd region be counted the same as a ClrObj size?" + vbCrLf +
                        String.Empty
                End If
                Return strHeapname
            End Function

            Public Class CHeapCompare
                Implements IComparer(Of CSpyHeap)

                Public Function Compare(ByVal x As CSpyHeap, ByVal y As CSpyHeap) As Integer Implements System.Collections.Generic.IComparer(Of CSpyHeap).Compare
                    Dim dd = New CaseInsensitiveComparer
                    Dim nRes = dd.Compare(x.HeapName, y.HeapName)
                    If nRes = 0 Then
                        If x.HeapHandleSpy.ToInt32 < y.HeapHandleSpy.ToInt32 Then
                            nRes = -1
                        Else
                            nRes = 1
                        End If
                    End If
                    Return nRes
                End Function
            End Class

            Public Overrides Sub FromStream(ByVal deserializer As Deserializer, ByVal versionFromStream As Integer)
                Dim temp As Integer = 0
                deserializer.Read(temp)
                HeapHandleSpy = CType(temp, IntPtr)

                deserializer.Read(HeapName)
                deserializer.Read(nLive)
                deserializer.Read(CurNumAllocs)
                deserializer.Read(CurTotBytes)
                Dim tmp = 0
                deserializer.Read(tmp)
                Dim barr = BitConverter.GetBytes(tmp) ' xxx next time we rev megasnapshot
                TotMemAllocated = BitConverter.ToUInt32(barr, 0)
                deserializer.Read(TotNumAllocs)
                deserializer.Read(HeapFileName)
                deserializer.Read(HeapFileLineNo)
                deserializer.Read(IsMemSpectHeap)
                Dim IsNrlsAlloc = False
                deserializer.Read(IsNrlsAlloc)
                deserializer.Read(nHeaderSize)
                deserializer.Read(nTrailerSize)

                Dim realHeapHandle As IntPtr = CType(deserializer.ReadInt(), IntPtr)
                If Not Common._offlineSnapshot._realHeapHandleDictionary.ContainsKey(Me.HeapHandleSpy) Then
                    Common._offlineSnapshot._realHeapHandleDictionary.Add(Me.HeapHandleSpy, realHeapHandle)
                End If
            End Sub

            Public Overrides Sub ToStream(ByVal serializer As Serializer)
                serializer.Write(HeapHandleSpy.ToInt32())
                serializer.Write(HeapName)
                serializer.Write(nLive)
                serializer.Write(CurNumAllocs)
                serializer.Write(CurTotBytes)

                Dim barr = BitConverter.GetBytes(TotMemAllocated)
                Dim tmp = BitConverter.ToInt32(barr, 0)
                serializer.Write(tmp)

                serializer.Write(TotNumAllocs)
                serializer.Write(HeapFileName)
                serializer.Write(HeapFileLineNo)
                serializer.Write(IsMemSpectHeap)
                serializer.Write(False) ' isnrlsalloc
                serializer.Write(nHeaderSize)
                serializer.Write(nTrailerSize)

                serializer.Write(Me.GetRealOSHeapHandle().ToInt32())
            End Sub

            Public Shared _dictLeaks As Dictionary(Of IntPtr, HeapAllocationContainer)
            Friend Shared _seqnoRangesAsSortedList As SortedList(Of UInteger, Tuple(Of UInteger, UInteger)) = Nothing
            Friend Shared _nMultiple As Integer
            Friend Shared _nMultipleWithThreshold As Double
            Friend Shared _LeakMultiplePercentThreshold As Integer = 50 ' 50% for non_seqno pairs
            Friend Shared _Queue As Queue(Of queuedata)
            Friend Shared _LeakMultipleIterationTolerance As Integer = 20 ' +=25% of iterations '' currently unused.

            Friend Class queuedata
                Public _allocsToUse As IEnumerable(Of HeapAllocationContainer)
                Public _nDepth As Integer
                Public Overrides Function ToString() As String
                    Return String.Format("{0} {1}", _allocsToUse.Count, _nDepth)
                End Function
            End Class

            Public Function GetLeaksFromLeakMultipleToList(
                                ByVal nMultiple As Integer,
                                Optional ByVal arrSeqNoRanges As UInteger(,) = Nothing) As List(Of HeapAllocationContainer)
                Dim dict = GetLeaksFromLeakMultiple(nMultiple, arrSeqNoRanges)
                Dim lst = New List(Of HeapAllocationContainer)
                lst.AddRange(dict.Values)
                Return lst
            End Function


            Public Function GetLeaksFromLeakMultiple(
                                            ByVal nMultiple As Integer,
                                            Optional ByVal arrSeqNoRanges As UInteger(,) = Nothing) As Dictionary(Of IntPtr, HeapAllocationContainer)
                _nMultiple = nMultiple
                If nMultiple < 2 Then
                    Throw New ArgumentOutOfRangeException("nMultple must be greater than 1. nMultiple = " + nMultiple.ToString)
                End If
                _seqnoRangesAsSortedList = Nothing
                If _GlobalFilter.SeqNoLo = 0 OrElse _GlobalFilter.SeqNoHi = 0 Then
                    If arrSeqNoRanges Is Nothing Then
                        Throw New InvalidOperationException("Filter SeqNo lo and hi need to be set")
                    End If
                End If
                If arrSeqNoRanges IsNot Nothing Then
                    _LeakMultiplePercentThreshold = GetPrivateProfileInt(ProfileStringSection, "LeakMultiplePercentThreshold", 75, _iniFileName)
                    _GlobalFilter.SeqNoLo = arrSeqNoRanges(0, 0)
                    _GlobalFilter.SeqNoHi = arrSeqNoRanges(arrSeqNoRanges.Length \ 2 - 1, 1)
                End If
                _LeakMultipleIterationTolerance = GetPrivateProfileInt(ProfileStringSection, "LeakMultipleIterationTolerance", 25, _iniFileName)
                Dim snap = TakeMemSnapshot(fEnableFilter:=True)
                Dim allocsToUse As IEnumerable(Of HeapAllocationContainer) = snap.Allocs
                If IsMemSpectHeap Then
                    allocsToUse = snap.Allocs.Where(Function(o) o.IsLeakableType)
                    'allocsToUse = snap.Allocs.Where(Function(o) o.TBlk.BlockType = BlockTypes.ClrObject)
                    'allocsToUse = snap.Allocs.Where(
                    '    Function(o) o.TBlk.BlockType =
                    '        BlockTypes.ClrObject AndAlso
                    '        o.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False) = "Microsoft.VisualStudio.Editor.Implementation.Undo.OleParentUndoUnit"
                    '        )

                End If

                If arrSeqNoRanges IsNot Nothing Then
                    _seqnoRangesAsSortedList = New SortedList(Of UInteger, Tuple(Of UInteger, UInteger))
                    For i = 0 To nMultiple - 1
                        _seqnoRangesAsSortedList.Add(arrSeqNoRanges(i, 0),
                                New Tuple(Of UInteger, UInteger)(arrSeqNoRanges(i, 0), arrSeqNoRanges(i, 1)))
                    Next
                End If
                _dictLeaks = New Dictionary(Of IntPtr, HeapAllocationContainer)
                _Queue = New Queue(Of queuedata)
                _Queue.Enqueue(New queuedata() With {._allocsToUse = allocsToUse, ._nDepth = 0})
                _nMultipleWithThreshold = _nMultiple * _LeakMultiplePercentThreshold / 100
                Dim nMaxQCnt = 0
                Dim nTimes = 0
                While (_Queue.Count > 0)
                    If _Queue.Count > nMaxQCnt Then
                        nMaxQCnt = _Queue.Count
                    End If
                    nTimes += 1
                    Dim qd = _Queue.Dequeue
                    RecurDownMultiples(qd._allocsToUse, qd._nDepth)
                End While
                If nTimes > 1 Then
                    UpdateStatusMsg(String.Format("RecurDownMultiples #Times={0} MaxQCnt={1} {2}", nTimes, nMaxQCnt, GetHeapName))
                    UpdateStatusMsg(String.Format("Found {0} leaks per iteration", CInt(_dictLeaks.Count / _nMultiple)))
                    ' we want to calc known issues with either setting to set the cache, so user experiences the same set with either setting
                    If _GlobalFilter.KnownIssues = ShowKnownIssuesEnum.Exclude OrElse _GlobalFilter.KnownIssues = ShowKnownIssuesEnum.Include Then
                        Dim origCnt = _dictLeaks.Count
                        Dim lstDel = New List(Of IntPtr)
                        For Each entry In _dictLeaks
                            If _GlobalFilter.KnownIssues = ShowKnownIssuesEnum.Exclude Then
                                ' delete the items that are known issues
                                If (Not String.IsNullOrEmpty(entry.Value.GetKnownIssue(dictLeaks:=_dictLeaks))) Then
                                    lstDel.Add(entry.Key)
                                End If
                            End If
                        Next
                        If _GlobalFilter.KnownIssues = ShowKnownIssuesEnum.Exclude Then
                            For Each item In lstDel
                                _dictLeaks.Remove(item)
                            Next
                            '_dictLeaks = _lstLeaks.Where(Function(h) String.IsNullOrEmpty(h.KnownIssue(lstLeaks:=_lstLeaks))).ToList
                            UpdateStatusMsg(String.Format("Filtered out {0} known issues per iteration", CInt((origCnt - _dictLeaks.Count) / _nMultiple)))
                        End If
                    End If
                End If
                Return _dictLeaks
            End Function

            Friend Sub RecurDownMultiples(
                                  ByVal hctrLst As IEnumerable(Of HeapAllocationContainer),
                                  ByVal nDepth As Integer
                                  )
                'we might have something like this (with nMult=7):
                '   141 -> not multiple of 7, so we expand children and recur for each child, which might look like this
                '          71 ' not multiple, so expand/recur
                '          70 ' yes is multiple
                '               Are each of these the same stack depth? No, means expand/recur
                '               yes: is each stack identical? No, means expand/recur
                '                    yes: bucketize: even distribution ? No, means expand/recur

                Dim sdict As New Dictionary(Of IntPtr, MemNodeBase)
                ' 1st, for each alloc, group them by call stack addr at spec'd depth
                For Each hctr In hctrLst
                    Dim addrCallStkAtThisDepth As IntPtr = hctr.GetCallStackAddr(nDepth)
                    If addrCallStkAtThisDepth <> IntPtr.Zero Then ' beyond stack?
                        Dim memNode As MemNodeBase = Nothing
                        If sdict.TryGetValue(addrCallStkAtThisDepth, memNode) Then
                            memNode._hctrList.Add(hctr)
                        Else
                            Dim baseNodeName = ResolveAddressToSymbol(addrCallStkAtThisDepth)
                            If String.IsNullOrEmpty(baseNodeName) Then
                                baseNodeName = "NoStackCollected(see Trackingmode)"
                            End If
                            memNode = New MemNodeBase(addrCallStkAtThisDepth, hctr, baseNodeName)
                            sdict.Add(addrCallStkAtThisDepth, memNode)
                        End If
                    End If
                Next
                'UpdateStatusMsg(String.Format("Num = {0} Cnt = {1}", hctrLst.Count, sdict.Count), msgType:=StatusMessageType.CalvinHBuild)
                ' for each different call stack addr at this depth
                For Each item In sdict.Values
                    Dim isEvenDist = IsEvenDistribution(item._hctrList)
                    If isEvenDist Then
                        'UpdateStatusMsg(String.Format("  {0}", item.ToString), msgType:=StatusMessageType.CalvinHBuild)
                        Dim fDone = nDepth + 1 = item._hctrList(0).AllocationStruct.m_uicStackAddr
                        If Not fDone Then
                            If item._hctrList.Count <= _nMultiple Then ' no tolerance: once we have < 2 *_nMultiple we don't want to dig any further: stacks might diverge but that's alright
                                fDone = True
                            End If
                        End If
                        If Not fDone Then
                            _Queue.Enqueue(New queuedata() With {._allocsToUse = item._hctrList, ._nDepth = nDepth + 1})
                        Else
                            For Each hctr In item._hctrList
                                _dictLeaks(hctr.GetAddr) = hctr
                            Next
                        End If
                    End If
                Next
            End Sub

            Friend Function IsEvenDistribution(ByVal hctrList As List(Of HeapAllocationContainer)) As Boolean
                Dim fIsEvenDistribution = False
                If hctrList.Count > _nMultipleWithThreshold Then
                    Dim Buckets(_nMultiple - 1) As Integer ' now check if distribution across timeline is even
                    Dim bucketNo = -1 ' <0 means don't add to bucket array
                    Dim nBucksWithOccurences = 0
                    For Each alloc In hctrList
                        If _seqnoRangesAsSortedList Is Nothing Then
                            bucketNo = CInt(CLng(_nMultiple) *
                                (alloc.AllocationStruct.SeqNo - _GlobalFilter.SeqNoLo) \
                                (_GlobalFilter.SeqNoHi - _GlobalFilter.SeqNoLo))
                        Else
                            Dim curSeq = alloc.AllocationStruct.SeqNo
                            Dim res = FindNearest(Of UInteger)(_seqnoRangesAsSortedList.Keys, curSeq, Nothing, Nothing, Nothing)
                            bucketNo = res(0)
                            If bucketNo >= 0 Then
                                Dim interval = _seqnoRangesAsSortedList.Values(bucketNo)
                                Dim endedge = interval.Item2
                                If bucketNo + 1 < _nMultiple Then
                                    ' some allocations occur after end of iteration, but before start of next iter so use the start of next iter
                                    endedge = _seqnoRangesAsSortedList.Values(bucketNo + 1).Item1
                                End If
                                If interval.Item1 <= curSeq AndAlso curSeq <= endedge Then
                                Else
                                    bucketNo = -1
                                End If
                            End If
                        End If
                        If bucketNo >= 0 AndAlso bucketNo < _nMultiple Then
                            If Buckets(bucketNo) = 0 Then ' first one in bucket?
                                nBucksWithOccurences += 1
                                If nBucksWithOccurences >= _nMultipleWithThreshold Then
                                    fIsEvenDistribution = True
                                    Exit For
                                End If
                            End If
                            Buckets(bucketNo) += 1
                        End If
                    Next
                End If
                Return fIsEvenDistribution
            End Function
        End Class

        Public Class MemNodeBase
            Public _addrCallStkAtThisDepth As IntPtr
            Public ReadOnly Property _Cnt As Integer
                Get
                    Return _hctrList.Count
                End Get
            End Property
            Public _hctrList As New List(Of HeapAllocationContainer)
            Public _baseName As String
            Sub New(ByVal addrCallStkAtThisDepth As IntPtr, ByVal hctr As HeapAllocationContainer, ByVal basename As String)
                _addrCallStkAtThisDepth = addrCallStkAtThisDepth
                _baseName = basename
                _hctrList.Add(hctr)
            End Sub
            Public Overrides Function ToString() As String
                Return String.Format("{0:x8}, {1} {2}", _addrCallStkAtThisDepth.ToInt32(), _hctrList.Count, _baseName)
            End Function
        End Class

        ''' <summary>
        ''' Given the address of a CAddressNode, create a HeapAllocationContainer
        ''' </summary>
        Public Function MakeHeapAllocationContainerFromAllocationStructAddr(ByVal hblkAddr As IntPtr, ByVal hp As CSpyHeap) As HeapAllocationContainer
            Dim dwBytesRead = 0
            Dim hctnr = New HeapAllocationContainer With {
                    .HeapBlockPtr = CType(hblkAddr, IntPtr),
                    .SpyHeapPtr = hp
                }

            If ReadProcessMemoryHeapAllocationStruct(_hProcessTarget, hblkAddr, hctnr.AllocationStruct, Marshal.SizeOf(hctnr.AllocationStruct), dwBytesRead) <> 0 Then
                If hp.IsMemSpectHeap Then
                    hctnr._tblkContainer = New TrackBlockContainer(TrackBlockStruct.GetTrackBlockFromAddress(hctnr.AllocationStruct.Address))
#If DEBUG Then
                    If hctnr.TBlk.Signature <> TBLK_SIGNATURE Then
                        UpdateStatusMsg("got tblk with bad signature " + hctnr.AllocationStruct.Address.ToString("x8"), fAssert:=True)
                        hctnr._tblkContainer = Nothing
                    End If
#End If
                End If
            Else
                UpdateStatusMsg("failed to readproc mem " + hctnr.ToString() + vbCrLf + hblkAddr.ToString("x8"), fAssert:=True)
                hctnr = Nothing
            End If
            Return hctnr
        End Function


        ''' <summary>
        ''' An obj and the things it refs
        ''' </summary>
        Public Class ClrObjDump
            Inherits SerializableObject

            Public hctnr As HeapAllocationContainer ' main object: 
            Public Refs As New HashSet(Of IntPtr) ' list of HeapAllocationContainers of objects referenced by main object

            Public Overrides Sub FromStream(ByVal deserializer As Deserializer, ByVal versionFromStream As Integer)
                'we'll read the hctr into a temp, then look it up in the offline snapshot MemSpect heap (should already be there)
                ' that way we save some memory
                Dim hctrTemp As HeapAllocationContainer = Nothing
                deserializer.Read(hctrTemp)
                If CLRObjRefsWrapper._DictOfClrObjects.ContainsKey(hctrTemp.AllocationStruct.Address) Then
                    hctnr = CLRObjRefsWrapper._DictOfClrObjects(hctrTemp.AllocationStruct.Address) ' assign from heap
                Else
                    hctnr = hctrTemp
                End If


                Dim temp As Integer
                deserializer.Read(temp) ' used to be classid, but now 0
                Dim ClassId = CType(temp, IntPtr)

                deserializer.Read(temp)
                For i As Integer = 0 To temp - 1
                    Refs.Add(CType(deserializer.ReadInt(), IntPtr))
                Next
            End Sub

            Public Overrides Sub ToStream(ByVal serializer As Serializer)
                serializer.WriteDefered(hctnr)
                serializer.Write(0) ' ClassId.ToInt32())
                serializer.Write(Refs.Count)
                For i As Integer = 0 To Refs.Count - 1
                    serializer.Write(Refs(i).ToInt32())
                Next
            End Sub

        End Class

        Public Function GetWindowTitle() As String
            Dim sTitle = "MemSpect"
            Select Case _ConnectionMode
                Case MemSpectMode.OnLine
                    Try
                        Dim targProc = Process.GetProcessById(_TargetProcessId)
                        sTitle += String.Format(" {0}({1}) {2}",
                                            targProc.MainModule.ModuleName,
                                            _TargetProcessId, targProc.MainWindowTitle)

                    Catch ex As Exception
                        'if shutting down

                    End Try
                Case MemSpectMode.Offline
                    If _offlineSnapshot IsNot Nothing Then
                        sTitle += " " + IO.Path.GetFileNameWithoutExtension(_offlineSnapshot._DataFilePath)
                    End If
                Case MemSpectMode.MiniDumpOnly
                    If _offlineSnapshot IsNot Nothing Then
                        sTitle += " " + Path.Combine(_offlineSnapshot._DataFilePath, _MiniDumpFileName)
                    End If
                Case MemSpectMode.Existing
                    sTitle += " Existing Pid " + _TargetProc.Id.ToString + " " + _TargetProc.ProcessName + ":" + _TargetProc.MainWindowTitle
            End Select
            Return sTitle
        End Function



        ''' <summary>
        ''' try to interpret some mem as a string for display. Works offline & online
        ''' </summary>
        ''' <param name="dwaddr"></param>
        ''' <param name="fDerefFirst"></param>
        ''' <param name="nMaxLen">in bytes</param>
        Public Function ReadProcessMemAsString(ByVal dwaddr As IntPtr,
                                               Optional ByVal fDerefFirst As Boolean = False,
                                               Optional ByVal nMaxLen As Integer = -1) As String
            Dim str = String.Empty
            '            Debug.WriteLine("ReadProcMemAsStr " + dwaddr.ToInt32.ToString("x8") + " " + nMaxLen.ToString)
            If dwaddr <> IntPtr.Zero AndAlso (nMaxLen = -1 OrElse nMaxLen > 4) Then
                Dim dwBytesRead = 0

                If fDerefFirst Then
                    If _ConnectionMode = MemSpectMode.Offline Then
                        Throw New InvalidOperationException("can't deref if offline")
                    End If
                    Dim tmp As Integer
                    If ReadProcessMemoryDword(_hProcessTarget, dwaddr, tmp, 4, dwBytesRead) = 0 Then
                        UpdateStatusMsg("ReadProcessMemAsString deref readprocmem failed", fAssert:=False)
                    End If
                    dwaddr = New IntPtr(tmp)
                End If
                Dim blk As New ProcMemBlockByte
                Dim minToRead = 520 ' at least maxpath
                If nMaxLen > 0 Then
                    minToRead = Math.Min(minToRead, nMaxLen)
                End If
                If _ConnectionMode = MemSpectMode.Offline Then
                    blk.data = MiniDumpReader.Singleton.ReadMemoryDictionary(dwaddr, minToRead) ' at least maxpath
                    If blk.data Is Nothing Then
                        Return str
                    End If
                    If nMaxLen = -1 Then
                        nMaxLen = blk.data.Length - 1
                    Else
                        nMaxLen = Math.Min(blk.data.Length, nMaxLen)
                    End If
                Else
                    ReDim blk.data(minToRead - 1)
                    dwBytesRead = ReadProcessMemoryAsByteArray(_hProcessTarget,
                                                    dwaddr,
                                                    minToRead,
                                                    1,
                                                    blk.data)
                    If dwBytesRead = 0 Then

                        'End If
                        'If ReadProcessMemoryByte(_hProcessTarget, dwaddr, blk, minToRead, dwBytesRead) = 0 Then
                        Dim lerr = Marshal.GetLastWin32Error
                        Dim errmsg = GetErrorMessageFromWin32LastError(lerr)
                        Dim sReturn = (String.Format("ReadProcessMemAsString error LastErr={0} Msg={1} addr={2:x8} nBytes={3} #read={4}",
                                                       lerr,
                                                       errmsg,
                                                       dwaddr.ToInt32,
                                                       minToRead,
                                                       dwBytesRead
                                                       ))
                        Return sReturn
                    End If
                    If nMaxLen = -1 Then
                        nMaxLen = dwBytesRead
                    Else
                        nMaxLen = Math.Min(dwBytesRead, nMaxLen)
                    End If
                End If
                Dim nLen = 0
                Dim nOffset = 0 ' offset into mem
                If nMaxLen > 4 Then
                    '46295a98 : 2e1a0001 4654454e 656d6172 6b726f77 7265562c 6e6f6973 2e34763d 54000130   01 00 1a 2e 4e 45 54 46 72 61 6d 65 77 6f 72 6b 2c 56 65 72 73 69 6f 6e 3d 76 34 2e 30 01 00 54     .NETFramework,Version=v4.0  T
                    '46295ab8 : 7246140e 77656d61 446b726f 6c707369 614e7961 2e10656d 2054454e 6d617246   0e 14 46 72 61 6d 65 77 6f 72 6b 44 69 73 70 6c 61 79 4e 61 6d 65 10 2e 4e 45 54 20 46 72 61 6d    FrameworkDisplayName.NET Fram
                    '46295ad8 : 726f7765 0034206b                                                         65 77 6f 72 6b 20 34 00                                                                           ework 4 
                    Dim nAscii = 0
                    Dim nToTest = nMaxLen
                    Dim nTrailingZeros = 0
                    Dim nConsecAscii = 0
                    Dim nMaxConsecAscii = 0
                    For chindx = 0 To nToTest - 1
                        Dim tst = blk.data(chindx)
                        If tst = 0 Then
                            nTrailingZeros += 1
                        Else
                            nTrailingZeros = 0
                        End If
                        If IsAsciiChar(tst) Then
                            nAscii += 1
                            nConsecAscii += 1
                            If nConsecAscii > nMaxConsecAscii Then
                                nMaxConsecAscii = nConsecAscii
                            End If
                        Else
                            nConsecAscii = 0
                        End If
                    Next
                    nToTest -= nTrailingZeros
                    If nToTest > 3 Then
                        If nMaxConsecAscii > 3 AndAlso nAscii * 2 > nToTest Then ' > half
                            Dim enc As New ASCIIEncoding
                            str = enc.GetString(blk.data, 0, nMaxLen)
                            Return str
                        End If
                    End If
                End If
                For nLen = 0 To nMaxLen - 2 Step 2
                    If blk.data(nLen) = 0 AndAlso blk.data(nLen + 1) = 0 Then
                        If nLen = 2 AndAlso nMaxLen > 10 Then ' some strings have a length preceding, like BSTR
                            '040ce000 : 00000048 00690056 00750073 006c0061 00530020 00750074 00690064 0020006f   48 00 00 00 56 00 69 00 73 00 75 00 61 00 6c 00 20 00 53 00 74 00 75 00 64 00 69 00 6f 00 20 00   H   V i s u a l   S t u d i o   
                            '040ce020 : 00650054 006d0061 00460020 0075006f 0064006e 00740061 006f0069 0020006e   54 00 65 00 61 00 6d 00 20 00 46 00 6f 00 75 00 6e 00 64 00 61 00 74 00 69 00 6f 00 6e 00 20 00   T e a m   F o u n d a t i o n   
                            '040ce040 : 00650053 00760072 00720065 afaf0000                                       53 00 65 00 72 00 76 00 65 00 72 00 00 00 af af                                                   S e r v e r     

                            nOffset = nLen + 2
                        Else
                            If nLen > 3 AndAlso nLen < 20 AndAlso nMaxLen > 30 AndAlso blk.data(3) <> 0 Then ' some strings are like ATL::CStringT
                                '22ef0430 : 54bf6004 0000000d 0000000f 00000001 00790053 00740073 006d0065 004f002e   04 60 bf 54 0d 00 00 00 0f 00 00 00 01 00 00 00 53 00 79 00 73 00 74 00 65 00 6d 00 2e 00 4f 00    ` T            S y s t e m . O 
                                '22ef0450 : 006a0062 00630065 00000074 afafafaf                                       62 00 6a 00 65 00 63 00 74 00 00 00 af af af af                                                   b j e c t       
                                nOffset = nLen + 2
                            Else
                                Exit For
                            End If
                        End If
                    End If
                Next
                Dim bEncode As New UnicodeEncoding
                'Dim decoder = bEncode.GetDecoder
                'Dim charsRead(nlen) As Char
                'decoder.GetChars(blk.data, 0, nlen, charsRead, 0)
                str = bEncode.GetString(blk.data, nOffset, nLen - nOffset)
            End If
            Return str
        End Function

        Public Function IsAsciiChar(ByVal b As Byte) As Boolean
            If b >= 32 AndAlso b < 127 Then
                Return True
            End If
            Return False
        End Function

        Public Function GetCodeMarkersToMerge() As List(Of HeapAllocationContainer)
            Dim cmlist As New List(Of HeapAllocationContainer)
            _CodeMarkerMode = CType(GetPrivateProfileInt(ProfileStringSection, "TrackCodeMarkers", 0, _iniFileName), CodeMarkerType)
            If _CodeMarkerMode <> CodeMarkerType.None Then
                '9446, _T("perfVSPerfWatsonUnResponsive") },
                '9447, _T("perfVSPerfWatsonUnResponsiveBegin")
                Dim lamFiltMarker = Function(hctr As HeapAllocationContainer) As Boolean
                                        '{ 9442, _T("perfVSIdleBegin") },
                                        '{ 9443, _T("perfVSIdleEnd") },
                                        '{ 9444, _T("perfVSIdleDelay") },
                                        '{ 9445, _T("perfVSInputDelay") },
                                        '{ 9446, _T("perfVSPerfWatsonUnResponsive") },
                                        '{ 9447, _T("perfVSPerfWatsonUnResponsiveBegin") },
                                        '{ 9448, _T("perfVSPerfWatsonUnResponsiveEnd") },
                                        'If hctr.TBlk.UnionData1 >= 9444 AndAlso hctr.TBlk.UnionData1 <= 9448 Then
                                        '    Return False
                                        'End If
                                        Dim fInclude = False
                                        If hctr IsNot Nothing Then
                                            If _CodeMarkerMode < 3 Then
                                                If _CodeMarkerMode = 2 Then 'custom
                                                    fInclude = hctr.GetCodeMarkerType = CodeMarkerType.CustomMarker
                                                Else
                                                    fInclude = hctr.GetCodeMarkerType = CodeMarkerType.NormalMarker
                                                End If
                                            Else
                                                fInclude = True
                                            End If
                                        End If
                                        Return fInclude
                                    End Function
                If _ConnectionMode = MemSpectMode.Offline Then
                    Dim z = _offlineSnapshot._snapshots.Where(Function(hp As MemSnapshot)
                                                                  If hp.SpyHeap.IsMemSpectHeap Then
                                                                      Return True
                                                                  End If
                                                                  Return False
                                                              End Function)
                    Dim mspectHeapSnap = z.First
                    cmlist.AddRange(mspectHeapSnap.Allocs.Where(Function(a As HeapAllocationContainer) As Boolean
                                                                    Dim fInclude = False
                                                                    If a.TBlkBlockType = BlockTypes.CodeMarker Then
                                                                        If lamFiltMarker.Invoke(a) Then
                                                                            fInclude = True
                                                                        End If
                                                                    End If
                                                                    Return fInclude
                                                                End Function))

                Else
                    'Dim mspectHeapSnap = (From hp In _HeapList Where hp.IsMemSpectHeap).First.TakeMemSnapshot

                    Dim msHeap = (From hp In _HeapList Where hp.IsMemSpectHeap).First
                    SendMsg(ProcMsgVerb.GetCodeMarkers, fSendEndMsgSync:=False, dwords:={0, 0})
                    Dim res = GetMsg(4)
                    Dim nItems = BitConverter.ToInt32(res, 0)
                    For i = 0 To nItems - 1
                        res = GetMsg(4)
                        Dim addr = IntPtr.op_Explicit(BitConverter.ToInt32(res, 0))
                        Dim hctr = MakeHeapAllocationContainerFromAllocationStructAddr(addr, msHeap)
                        If lamFiltMarker.Invoke(hctr) Then
                            cmlist.Add(hctr)
                        End If
                    Next
                    EndMsgSync()
                End If
            End If
            If cmlist.Count = 0 Then
                _CodeMarkerMode = CodeMarkerType.None ' so doesn't show when not relevant (like NotePad.exe)
            End If
            Return cmlist
        End Function

        Public Class HeapReport
            Inherits SerializableObject
            Public _ProcessHeapHandles As New List(Of IntPtr) ' heaps caught by caught HeapAlloc or HeapCreate calls
            Public _ProcessHeapData As New List(Of HeapData)
            Private _nPriorVal As IntPtr
            Public _totHeapData As HeapData ' use an instance to accum totals

            Public Class HeapData
                Inherits SerializableObject

                Public hHeap As IntPtr ' the handle for the heap
                Public heapSpy As CSpyHeap ' if we have it
                Public nTotAllocs As Int64
                Public nTotSize As Int64 ' as reported by HeapWalk
                Public nComSize As Int64
                Public nUnComSize As Int64
                Public nGaps As Integer ' # of gaps->fragmentation
                Public pelist As New List(Of PROCESS_HEAP_ENTRY)
                Public Function GetHeapNameForHeapData() As String
                    Dim heapName = String.Empty
                    If heapSpy Is Nothing Then
                        heapName = hHeap.ToString("x8") + " "
                        Select Case hHeap
                            Case ProcComm._hMemSpectHeap
                                heapName += "MemSpect heapxxxx"
                            Case ProcComm._hDebugHeap
                                heapName = MemSpectHeapName + " debug heap " + heapName
                            Case Else

                        End Select
                    Else
                        heapName = heapSpy.HeapName
                    End If
                    Return heapName
                End Function
                Public Overrides Function ToString() As String
                    Dim str = String.Format("Total Cnt= {0:n0} Size= {1:n0} Commit={2:n0} Uncom = {3:n0} Gaps={4:n0} ",
                                         nTotAllocs, nTotSize, nComSize, nUnComSize, nGaps)
                    If hHeap <> IntPtr.Zero Then
                        str += "Handle = " + hHeap.ToInt32.ToString("x8") + " "
                    End If
                    Return str
                End Function

                Public Overrides ReadOnly Property Version As Integer
                    Get
                        Return SerializableObject.CurrentSerializationVersion + 1 ' 4
                    End Get
                End Property

                Public Overrides Sub FromStream(ByVal deserializer As Deserializer, ByVal versionFromStream As Integer)
                    hHeap = CType(deserializer.ReadInt(), IntPtr)
                    deserializer.Read(heapSpy)
                    If versionFromStream = SerializableObject.CurrentMinimumVersion + 1 Then ' newer
                        deserializer.Read(nTotAllocs)
                        deserializer.Read(nTotSize)
                        deserializer.Read(nComSize)
                        deserializer.Read(nUnComSize)
                    Else
                        Dim tmpInt As Integer
                        deserializer.Read(tmpInt)
                        nTotAllocs = tmpInt
                        deserializer.Read(tmpInt)
                        nTotSize = tmpInt
                        deserializer.Read(tmpInt)
                        nComSize = tmpInt
                        deserializer.Read(tmpInt)
                        nUnComSize = tmpInt
                    End If
                    deserializer.Read(nGaps)

                    Dim size As Integer = deserializer.ReadInt()
                    pelist = New List(Of PROCESS_HEAP_ENTRY)(size)
                    For i As Integer = 0 To size - 1
                        pelist.Add(CType(deserializer.ReadObject(), PROCESS_HEAP_ENTRY))
                    Next

                End Sub

                Public Overrides Sub ToStream(ByVal serializer As Serializer)
                    '' xxx next time we rev megasnapshot: add Memspect private heap handle name
                    serializer.Write(hHeap.ToInt32())
                    serializer.WriteDefered(heapSpy)
                    serializer.Write(nTotAllocs)
                    serializer.Write(nTotSize)
                    serializer.Write(nComSize)
                    serializer.Write(nUnComSize)
                    serializer.Write(nGaps)
                    serializer.WritePrivateCollection(pelist)
                End Sub
            End Class

            Public Function GetProcessHeapHandles() As List(Of IntPtr)
                ProcComm.FreezeTarget()
                Dim lstHandles = New List(Of IntPtr)
                If _ConnectionMode = MemSpectMode.OnLine Then
                    SendMsg(ProcMsgVerb.GetProcessHeapHandles, fSendEndMsgSync:=True)
                    Dim ptr = _SharedMemAddr
                    Dim numHeapHandles = Marshal.ReadInt32(ptr)
                    For i = 0 To numHeapHandles - 1
                        Dim handleHeapFound = Marshal.ReadIntPtr(ptr, (i + 1) * 4)
                        lstHandles.Add(handleHeapFound)
                    Next
                End If
                Return lstHandles
            End Function

            Public Function GetHeapWalkData(ByVal hHeap As IntPtr, Optional ByVal fOnlyWantRegions As Boolean = False) As HeapData
                If Common._ConnectionMode = MemSpectMode.Offline Then
                    Dim hrpt = Common._offlineSnapshot.heapReport ' get it to force load
                    If _offlineSnapshot.heapReport Is Nothing Then
                        Throw New InvalidOperationException("Heap report was not saved in snapshot: see 'IncludeHeapReport' ini setting")
                    End If

                    Dim data = hrpt._ProcessHeapData.Where(Function(item As HeapData)
                                                               Return item.hHeap = hHeap
                                                           End Function)
                    If data.Count() = 0 Then
                        UpdateStatusMsg("ERROR: Failed to find heapdata hHeap " + hHeap.ToString("x8") + ".", fAssert:=True)
                        Return New HeapData()
                    ElseIf data.Count() > 1 Then
                        UpdateStatusMsg("WARNING: Found more than one heapdata for " + hHeap.ToString("x8") + ".  Using first one found.", fAssert:=True)
                    End If
                    Return data.First()
                End If

                ProcComm.FreezeTarget()

                'If hHeap = ProcComm._hDebugHeap Then
                '    Return Nothing
                'End If
                Dim hSpy As CSpyHeap = Nothing
                Dim fIsMemSpectHeap = False
                Dim nExpectedGap = 28 ' gap between allocations: if gap > nExpectedGap then real gap
                For Each spyheap In _HeapList
                    If spyheap.GetRealOSHeapHandle = hHeap Then
                        hSpy = spyheap
                        If hSpy.IsMemSpectHeap Then
                            fIsMemSpectHeap = True
                            nExpectedGap = 8 ' non-CHeapSpy wrapped detoured HeapAllocs
                        End If
                        nExpectedGap = hSpy.nHeaderSize + hSpy.nTrailerSize + 12
                        Exit For
                    End If
                Next

                Dim hd As New HeapData With {
                    .hHeap = hHeap,
                    .heapSpy = hSpy
                    }
                _nPriorVal = IntPtr.Zero
                Dim nIndex As Integer = 0 ' index into data
                Dim sz = Marshal.SizeOf(GetType(PROCESS_HEAP_ENTRY))
                Dim blksize = CInt(sz / Marshal.SizeOf(GetType(Integer)))
                Do While True
                    Dim res = SendMsg(ProcMsgVerb.DoHeapWalk, fSendEndMsgSync:=True, dwords:={hd.hHeap.ToInt32, nIndex, If(fOnlyWantRegions, 1, 0)})
                    If res Is Nothing Then
                        Throw New InvalidOperationException("Communication broken")
                    End If
                    Dim nItems = Marshal.ReadInt32(_SharedMemAddr) ' # of allocations that fit in shared mem
                    If nItems = 0 Then
                        Exit Do
                    End If
                    hd.pelist.Capacity += nItems
                    For i = 0 To nItems - 1
                        Dim pEntry = CType(Marshal.PtrToStructure(_SharedMemAddr.MyAdd(4 + i * 4 * blksize),
                                                                  GetType(PROCESS_HEAP_ENTRY)), 
                                                PROCESS_HEAP_ENTRY)
                        If (pEntry.wFlags And PROCESS_HEAP_ENTRY_WFLAGS.PROCESS_HEAP_ENTRY_BUSY) > 0 Then
                            Debug.Assert(Not fOnlyWantRegions, "got heap busy even though fOnlyWantRegions")
                            hd.nTotAllocs += 1
                            hd.nTotSize += pEntry.cbData
                            If _nPriorVal.MyAdd(nExpectedGap).ToInt64 < pEntry.lpData.ToInt64 Then
                                hd.nGaps += 1
                            End If
                            _nPriorVal = pEntry.lpData.MyAdd(pEntry.cbData)

                        ElseIf (pEntry.wFlags And PROCESS_HEAP_ENTRY_WFLAGS.PROCESS_HEAP_REGION) > 0 Then
                            hd.nComSize += pEntry.UnionBlock.Region.dwCommittedSize
                            hd.nUnComSize += pEntry.UnionBlock.Region.dwUnCommittedSize
                        ElseIf (pEntry.wFlags And PROCESS_HEAP_ENTRY_WFLAGS.PROCESS_HEAP_UNCOMMITTED_RANGE) > 0 Then
                            hd.nUnComSize += pEntry.UnionBlock.Region.dwUnCommittedSize
                        End If
                        hd.pelist.Add(pEntry)
                    Next
                    nIndex += nItems
                Loop

                Return hd
            End Function
            Sub New()
                Me.New(fOnlyWantRegions:=False)
            End Sub
            Sub New(ByVal fOnlyWantRegions As Boolean)
                If Common._ConnectionMode = MemSpectMode.Offline Then
                    Return
                End If

                Try
                    UpdateStatusMsg(" -Calculating Heap Report")
                    ProcComm.FreezeTarget()
                    _ProcessHeapHandles = GetProcessHeapHandles()
                    _ProcessHeapData.Clear()
                    _totHeapData = New HeapData
                    For Each hHandle In _ProcessHeapHandles
                        Dim hd = GetHeapWalkData(hHandle, fOnlyWantRegions)
                        _ProcessHeapData.Add(hd)
                        _totHeapData.nTotAllocs += hd.nTotAllocs
                        _totHeapData.nTotSize += hd.nTotSize
                        _totHeapData.nUnComSize += hd.nUnComSize
                        _totHeapData.nComSize += hd.nComSize
                        _totHeapData.nGaps += hd.nGaps
                    Next

                Catch ex As Exception
                    MemSpectExceptionHandler(ex)
                End Try
            End Sub

            Public Overrides Sub FromStream(ByVal deserializer As Deserializer, ByVal versionFromStream As Integer)
                Dim size As Integer = deserializer.ReadInt()
                _ProcessHeapHandles = New List(Of IntPtr)(size)
                For i As Integer = 0 To size - 1
                    _ProcessHeapHandles.Add(CType(deserializer.ReadInt(), IntPtr))
                Next

                size = deserializer.ReadInt()
                _ProcessHeapData = New List(Of HeapData)(size)
                For i As Integer = 0 To size - 1
                    _ProcessHeapData.Add(CType(deserializer.ReadObject(), HeapData))
                Next

                _nPriorVal = CType(deserializer.ReadInt(), IntPtr)
                deserializer.Read(_totHeapData)
            End Sub

            Public Overrides Sub ToStream(ByVal serializer As Serializer)
                serializer.Write(_ProcessHeapHandles.Count)
                For i As Integer = 0 To _ProcessHeapHandles.Count - 1
                    serializer.Write(_ProcessHeapHandles(i).ToInt32())
                Next

                serializer.WritePrivateCollection(_ProcessHeapData)

                serializer.Write(_nPriorVal.ToInt32())
                serializer.Write(_totHeapData)
            End Sub
        End Class

        Public Function HeapNameConvertFromRaw(ByVal heapHandle As IntPtr, ByVal theHeapName As String) As String
            If _UseChildProcessForSymbols Then
                'targ proc doesn't resolve syms, so we do it here and send it back so asserts can show syms
                If theHeapName.Length >= 11 Then 'targ sends hex addr "~0x12345678"
                    If "_~".IndexOf(theHeapName(0)) >= 0 AndAlso theHeapName.Substring(1, 2) = "0x" Then
                        Dim str = Int32.Parse(theHeapName.Substring(3, 8), Globalization.NumberStyles.AllowHexSpecifier)
                        Dim dwAddr = New IntPtr(str)
                        If (dwAddr.ToInt32 And MANAGED_STACK_FLAG) = 0 Then ' if the stack is in upper memory, it will ovflo to look like managed
                            theHeapName = theHeapName(0) + ResolveAddressToSymbol(dwAddr, fStripFileName:=True).Replace("[", String.Empty).Replace("]", String.Empty).Replace(" ", String.Empty)

                        End If
                        ''now send name to targ proc
                        'Dim bencode = New Text.ASCIIEncoding
                        'Dim barray = bencode.GetBytes(theHeapName)
                        'WriteProcessMemory(_MemSpectProcessHandle, _SharedMemAddr, barray, barray.Length, Nothing)

                        'SendMsg(ProcMsgVerb.SetHeapName, {heapHandle.ToInt32, theHeapName.Length})
                    End If
                End If
            End If

            Return theHeapName
        End Function

        Public Function ReadHeaps(Optional ByVal fClearFirst As Boolean = False) As Boolean ' called from RefreshHeapList
            If _ConnectionMode = MemSpectMode.Existing OrElse _ConnectionMode = MemSpectMode.MiniDumpOnly Then
                Return False
            End If
            Try
                If _ConnectionMode = MemSpectMode.Offline Then
                    __HeapList.Clear()
                    If Common._offlineSnapshot Is Nothing Then
                        '                        OfflineMegaSnapshot.LoadMegaSnapshot(fWaitTilDone:=_IsUnderTest)
                    End If
                    If _offlineSnapshot IsNot Nothing Then
                        For i As Integer = 0 To Common._offlineSnapshot._snapshots.Count - 1
                            __HeapList.Add(Common._offlineSnapshot._snapshots(i).SpyHeap)
                        Next
                    End If
                ElseIf _IsClientOutOfProcess Then
                    If fClearFirst Then ' there might be some stale heaps
                        __HeapList.Clear()
                    End If
                    SendMsg(ProcMsgVerb.GetHeapStats, fSendEndMsgSync:=True)
                    Dim ptr = _SharedMemAddr
                    Dim numHeaps = Marshal.ReadInt32(ptr)
                    ptr = ptr.MyAdd(4)
                    Dim heapdatsize = 12 ' 12 dwords per heap
                    Dim dwords(numHeaps * heapdatsize) As Integer
                    Dim memblk As New ProcMemIntPtr

                    'we make a copy of shared mem so we can reuse when resolving heapnames and sending name back to targ proc
                    If ReadProcessMemoryDwords(_MemSpectProcessHandle, ptr, memblk, numHeaps * heapdatsize * 4, Nothing) = 0 Then
                        UpdateStatusMsg("reading procmem for ReadHeaps failed", fAssert:=True)
                    End If
                    Dim hdata = memblk.data
                    Dim tempLiveHeapList As New SortedSet(Of CSpyHeap)

                    For i = 0 To numHeaps - 1
                        '     // 0 is heaphandle, 1 is heapname, 2 is heapnamelen, 3 is Filename, 4 is Filenamelen, 5 is LineNo
                        Dim hIndex = i * heapdatsize
                        Dim cSpyheapptr = hdata(hIndex)
                        Dim theHeapName = GetStringFromRemoteMem(hdata(hIndex + 1), hdata(hIndex + 2).ToInt32, _hProcessTarget)
                        theHeapName = HeapNameConvertFromRaw(cSpyheapptr, theHeapName)
                        Dim pSpyHeap As CSpyHeap
                        Dim hpSubset = __HeapList.Where(Function(hp As CSpyHeap) As Boolean
                                                            If hp.HeapName = theHeapName Then
                                                                If hp.HeapHandleSpy = cSpyheapptr Then
                                                                    Return True
                                                                End If
                                                            End If
                                                            Return False
                                                        End Function)
                        If hpSubset.Count = 1 Then
                            pSpyHeap = hpSubset.First
                        Else
                            pSpyHeap = New CSpyHeap With {.HeapName = theHeapName, .HeapHandleSpy = cSpyheapptr}
                            __HeapList.Add(pSpyHeap)
                            With pSpyHeap
                                .HeapName = theHeapName
                                .HeapFileName = String.Empty
                                .IsMemSpectHeap = .HeapName = MemSpectHeapName
                                .HeapFileLineNo = hdata(hIndex + 5).ToInt32
                                .nHeaderSize = hdata(hIndex + 10).ToInt32
                                .nTrailerSize = hdata(hIndex + 11).ToInt32
                            End With
                        End If
                        With pSpyHeap
                            .CurNumAllocs = hdata(hIndex + 6).ToInt32
                            .CurTotBytes = hdata(hIndex + 7).ToInt32
                            .TotNumAllocs = hdata(hIndex + 8).ToInt32
                            Dim tmp = hdata(hIndex + 9).ToInt32
                            Dim barr = BitConverter.GetBytes(tmp)
                            .TotMemAllocated = BitConverter.ToUInt32(barr, 0)

                        End With
                        tempLiveHeapList.Add(pSpyHeap) ' track the ones we have: those missing are deleted
                    Next
                    __HeapList.RemoveWhere(Function(hp As CSpyHeap) As Boolean
                                               If tempLiveHeapList.Contains(hp) Then
                                                   Return False
                                               End If
                                               Return True
                                           End Function)
                Else
                    Debug.Assert(False, "dead code")
                End If
            Catch ex As Exception
                MemSpectExceptionHandler(ex)
            End Try
            Return __HeapList.Count > 0
        End Function


        Public Class MyDict(Of Tkey, Tvalue As New) ' dict that allows missing values to be 0
            Inherits Dictionary(Of Tkey, Tvalue)
            Default Overloads Property item(ByVal k As Tkey) As Tvalue
                Get
                    Dim val As Tvalue = Nothing
                    If Not TryGetValue(k, val) Then
                        val = New Tvalue
                    End If
                    Return val
                End Get
                Set(ByVal value As Tvalue)

                End Set
            End Property
        End Class

        Public Class MyTuple(Of Tval1 As New, Tval2 As New)
            Inherits Tuple(Of Tval1, Tval2)
            Sub New()
                MyBase.New(New Tval1, New Tval2)
            End Sub
            Sub New(ByVal p1 As Tval1, ByVal p2 As Tval2)
                MyBase.New(p1, p2)
            End Sub
        End Class

        Public Function IntToUInt(ByVal val As Integer) As UInteger
            If val < 0 Then
                IntToUInt = CUInt(CLng(val) + 4294967296.0#)
            Else
                IntToUInt = CUInt(val)
            End If
        End Function

        Public Class MemSnapshot
            Inherits SerializableObject

            Property SpyHeap As CSpyHeap
            Property Allocs As List(Of HeapAllocationContainer)
            Property nCnt As Integer
            Property nTotMem As Long
            Property Timestamp As Date
            Public aBlockTypeCnts() As Integer
            Public aBlockTypeSizes() As Long

            Public Shared Function CreateMemSnapshotFromListAllocs(
                                                                  ByVal hp As CSpyHeap,
                                                                  ByVal srcAllocs As List(Of HeapAllocationContainer),
                                                                  Optional ByVal fEnableFilter As Boolean = True
                                                                  ) As MemSnapshot
                Dim retSnapshot = New MemSnapshot() With {
                    .SpyHeap = hp,
                    .Allocs = New List(Of HeapAllocationContainer)
                }
                With retSnapshot
                    For Each alloc As HeapAllocationContainer In srcAllocs
                        If Not fEnableFilter OrElse _GlobalFilter.FilterFunction(alloc) Then
                            If .SpyHeap Is Nothing Then
                                .SpyHeap = alloc.SpyHeapPtr
                                hp = .SpyHeap
                            End If
                            If hp.IsMemSpectHeap Then
                                .aBlockTypeCnts(alloc.TBlkBlockType) += 1
                                .aBlockTypeSizes(alloc.TBlkBlockType) += alloc.TBlk.Size
                            End If
                            .Allocs.Add(alloc)
                            .nCnt += 1
                            .nTotMem += alloc.GetSize
                        End If
                    Next
                End With
                Return retSnapshot
            End Function

            Sub New()
                Timestamp = DateTime.Now
                Dim numBTypes = [Enum].GetNames(GetType(BlockTypes)).Count
                Array.Resize(aBlockTypeCnts, numBTypes)
                Array.Resize(aBlockTypeSizes, numBTypes)
            End Sub
            Public Overrides ReadOnly Property Version As Integer
                Get
                    Return SerializableObject.CurrentSerializationVersion + 1 ' 4
                End Get
            End Property
            Public Overrides Sub FromStream(ByVal deserializer As Deserializer, ByVal versionFromStream As Integer)
                deserializer.Read(SpyHeap)

                Dim size As Integer = 0
                deserializer.Read(size)
                Dim allocs As New List(Of HeapAllocationContainer)()
                For i As Integer = 0 To size - 1
                    allocs.Add(CType(deserializer.ReadObject(), HeapAllocationContainer))
                Next
                Me.Allocs = allocs

                deserializer.Read(nCnt)
                If versionFromStream = SerializableObject.CurrentSerializationVersion Then
                    Dim tmp As Integer = 0
                    deserializer.Read(tmp)
                    nTotMem = tmp
                Else
                    If versionFromStream = SerializableObject.CurrentSerializationVersion + 1 Then
                        deserializer.Read(nTotMem)
                    Else
                        Debug.Assert(False, "Wrong version for deserializer " + versionFromStream.ToString)
                    End If
                End If
                Dim timestampString = String.Empty
                deserializer.Read(timestampString)
                Try
                    Timestamp = DateTime.Parse(timestampString)
                Catch ex As Exception
                    ' india has diff format
                End Try

                size = 0
                deserializer.Read(size)
                Dim objs As New List(Of Integer)()
                For i As Integer = 0 To size - 1
                    Dim temp = 0
                    deserializer.Read(temp)
                    objs.Add(temp)
                Next

                aBlockTypeCnts = objs.ToArray()

                deserializer.Read(size)

                Dim objsLong As New List(Of Long)()
                If versionFromStream = SerializableObject.CurrentSerializationVersion Then
                    For i As Integer = 0 To size - 1
                        Dim temp = 0
                        deserializer.Read(temp)
                        objsLong.Add(temp)
                    Next
                Else
                    If versionFromStream = SerializableObject.CurrentSerializationVersion + 1 Then
                        For i As Integer = 0 To size - 1
                            Dim temp = 0L ' long
                            deserializer.Read(temp)
                            objsLong.Add(temp)
                        Next
                    End If
                End If

                aBlockTypeSizes = objsLong.ToArray()
            End Sub

            Public Overrides Sub ToStream(ByVal serializer As Serializer)
                serializer.WriteDefered(SpyHeap)
                serializer.WritePrivateCollection(Allocs)
                serializer.Write(nCnt)
                serializer.Write(nTotMem)
                serializer.Write(Timestamp.ToString())

                Dim size As Integer = aBlockTypeCnts.Length
                serializer.Write(size)
                For i As Integer = 0 To size - 1
                    serializer.Write(aBlockTypeCnts(i))
                Next

                size = aBlockTypeSizes.Length
                serializer.Write(size)
                For i As Integer = 0 To size - 1
                    serializer.Write(aBlockTypeSizes(i))
                Next
            End Sub

            Public Overrides Function ToString() As String
                Return String.Format("Snap: # allocs = {0:n0} {1}", nCnt, If(SpyHeap Is Nothing, String.Empty, SpyHeap.HeapName))
            End Function
        End Class

        Public Enum IndirectInfoType
            IIType_None
            IIType_OpenFile
            IIType_CreateFile
            IIType_CreateSection
            IIType_OpenSection
            IIType_MapSection
            IIType_CustomCodeMarker
            IIType_CustomCodeMarkerWithUserData
            IIType_FileLoad ',  //LdrDllNotification 
            IIType_FileUnload ', //LdrDllNotification 
        End Enum

        ' note: must add at end to maintain compat with prior snapshots
        Public Enum BlockTypes ' must match BlockType in vsassert. Used for __Memspect pseudoheap
            None
            VirtualAlloc
            MapFile
            HeapCreate
            HeapAlloc
            RegKey
            CreateFile
            ClrObject
            ClrClass
            ClrModule
            ClrAssembly
            ClrAppDomain
            CodeMarker
            TlsAllocFree
            ThreadCreate
            IndirectInfo ' like createfile, opensection
            LoadResource
            TrackGCStacks
            ClrJit
            ClrGCHnd
            ClrExcpt
            GdiObjs
        End Enum
        Public Enum GdiObjType
            PEN = 1
            BRUSH = 2
            DC = 3
            METADC = 4
            PAL = 5
            FONT = 6
            BITMAP = 7
            REGION = 8
            METAFILE = 9
            MEMDC = 10
            EXTPEN = 11
            ENHMETADC = 12
            ENHMETAFILE = 13
            COLORSPACE = 14
        End Enum


        ' note: must add at end to maintain compat with prior snapshots
        Public Enum TrkType ' some BlockTypes are merged into TrkTypes, which are shown in snapshot
            All
            VirtualAllocs
            MappedFiles
            HeapCreates
            CodeMarker
            ClrLoads ' combines Module,assembly,appdomain
            ClrClasses
            ClrObjects
            ThreadInfo
            Indirect
            LoadResource
            TrackGCStacks
            ClrJit
            ClrGCHnd
            ClrExcpt
            GdiObjs
        End Enum

        <StructLayout(LayoutKind.Sequential)>
        Public Structure GCStats
            Public Gen0 As Integer
            Public Gen1 As Integer
            Public Gen2 As Integer
            Public Gen3 As Integer
            Public Moved As Integer
            Public Survived As Integer
            Public Collected As Integer
            Public g_MemSpectBgdThreadCleanupRCW As Integer

            Public Shared Function GetGCStats() As GCStats
                Dim gcStatData As New GCStats
                If _IsClientOutOfProcess AndAlso _ConnectionMode = MemSpectMode.OnLine Then
                    If _GCStatsAddr = IntPtr.Zero Then
                        SendMsg(ProcMsgVerb.GetGCStatAddr, fSendEndMsgSync:=True, dwords:={CUInt(0)})
                        _GCStatsAddr = Marshal.ReadIntPtr(_SharedMemAddr)
                    End If
                    If _GCStatsAddr <> IntPtr.Zero Then
                        If ReadProcessMemoryGCStatStruct(_hProcessTarget, _GCStatsAddr, gcStatData, Marshal.SizeOf(GetType(GCStats)), Nothing) = 0 Then
                            UpdateStatusMsg("readmem gc failed", fAssert:=True)
                        End If
                    End If
                End If
                Return gcStatData
            End Function
            Public Overrides Function ToString() As String
                Return String.Format("GC: {0} {1} {2} {3}  ΔSrv={4:n0} ΔMov={5:n0}  ΔCol={6:n0}",
                                          Gen3,
                                          Gen2,
                                          Gen1,
                                          Gen0,
                                          Survived,
                                          Moved,
                                          Collected
                                          )
            End Function
        End Structure
        Friend _CodeMarkerNameDict As New Dictionary(Of Integer, String) ' these are const, so never need to flush
        ''' <summary>
        ''' Get name without instance information
        ''' useOffline so doesn't do sendmsg: avoid deadlock
        Public Function GetCodeMarkerNameRaw(CodeMarkerId As Integer, Optional fuseOffline As Boolean = False) As String
            Dim codeMarkerName = String.Empty
            If Not _CodeMarkerNameDict.TryGetValue(CodeMarkerId, codeMarkerName) Then
                If Common._ConnectionMode = MemSpectMode.Offline OrElse fuseOffline Then
                    Dim nAddr = IntPtr.Zero
                    GetCodeMarkerNameFromId(CodeMarkerId, nAddr)
                    If nAddr <> IntPtr.Zero Then
                        codeMarkerName = Marshal.PtrToStringAnsi(nAddr)
                    End If
                    'Common._offlineSnapshot.TryGetCodemarkerForAllocation(CodeMarkerId, codeMarkerName)
                Else
                    SendMsg(ProcMsgVerb.GetCodeMarkerName, fSendEndMsgSync:=True, dwords:={CodeMarkerId})
                    codeMarkerName = ReadSharedMemAsString()
                End If

                _CodeMarkerNameDict.Add(CodeMarkerId, codeMarkerName) ' these are const, so no need to clear
            End If
            Return codeMarkerName
        End Function

        <StructLayout(LayoutKind.Sequential)>
        Public Structure HeapAllocationStruct ' one of these for every allocation
            Implements IFastSerializable, IFastSerializableVersion 'structs can't inherit from classes (SerializableObject), so we explicitly implement these

            Public Address As IntPtr ' addr of the allocation
            Public Size As Integer ' size of the allocation
            Public SeqNo As UInteger ' chrono
            Public ThreadId As Integer
            Public ReadOnly Property Thread As String
                Get
                    Dim retVal = ThreadId.ToString
                    If ThreadId = _MainThread Then
                        retVal += "M"
                    End If
                    Return retVal
                End Get
            End Property
            Public m_uicStackAddr As Integer ' count of stack addr found

            Public ReadOnly Property Version As Integer Implements IFastSerializableVersion.Version
                Get
                    Return SerializableObject.CurrentSerializationVersion
                End Get
            End Property

            Public ReadOnly Property MinimumVersion As Integer Implements IFastSerializableVersion.MinimumVersion
                Get
                    Return SerializableObject.CurrentMinimumVersion
                End Get
            End Property

            Public Sub ToStream(ByVal serializer As Serializer) Implements IFastSerializable.ToStream
                serializer.Write(Address.ToInt32())
                serializer.Write(Size)
                serializer.Write(UIntegerToInteger(SeqNo)) ' force to integer: UINT (4 bytes) maps to LONG (8 bytes)
                serializer.Write(ThreadId)
                serializer.Write(m_uicStackAddr)
            End Sub

            Public Sub FromStream(ByVal deserializer As Deserializer, ByVal versionFromStream As Integer) Implements IFastSerializable.FromStream
                Dim tempValue As Integer

                deserializer.Read(tempValue)
                Address = CType(tempValue, IntPtr)
                deserializer.Read(Size)
                Dim seqnoAsInt = 0
                deserializer.Read(seqnoAsInt)
                Me.SeqNo = IntegerToUInteger(seqnoAsInt)
                deserializer.Read(ThreadId)
                deserializer.Read(m_uicStackAddr)
            End Sub
        End Structure

        Public Class TrackBlockContainer
            Public _Tblk As TrackBlockStruct
            Public Sub New(ByVal trackBlockStruct As TrackBlockStruct)
                _Tblk = trackBlockStruct
            End Sub
        End Class

        Public Function BlockTypeToTrackType(ByVal blkType As BlockTypes) As TrkType
            Dim ttype = TrkType.All
            Select Case blkType
                Case BlockTypes.MapFile, BlockTypes.IndirectInfo
                    ttype = TrkType.Indirect
                Case BlockTypes.VirtualAlloc
                    ttype = TrkType.VirtualAllocs
                Case BlockTypes.HeapCreate
                    ttype = TrkType.HeapCreates
                Case BlockTypes.GdiObjs
                    ttype = TrkType.GdiObjs
                Case BlockTypes.TrackGCStacks
                    ttype = TrkType.TrackGCStacks
                Case BlockTypes.ClrAssembly, BlockTypes.ClrAppDomain
                    ttype = TrkType.ClrLoads
                Case BlockTypes.ClrJit
                    ttype = TrkType.ClrJit
                Case BlockTypes.ClrGCHnd
                    ttype = TrkType.ClrGCHnd
                Case BlockTypes.ClrExcpt
                    ttype = TrkType.ClrExcpt
                Case BlockTypes.ClrClass
                    ttype = TrkType.ClrClasses
                Case BlockTypes.ClrObject
                    ttype = TrkType.ClrObjects
                Case BlockTypes.CodeMarker
                    ttype = TrkType.CodeMarker
                Case BlockTypes.TlsAllocFree, BlockTypes.ThreadCreate
                    ttype = TrkType.ThreadInfo
                Case BlockTypes.LoadResource
                    ttype = TrkType.LoadResource
            End Select
            Return ttype
        End Function


        <StructLayout(LayoutKind.Sequential)>
        Public Structure TrackBlockStruct ' one of these for every _MemSpect heap (hooking VirtualAlloc,etc)
            Implements IFastSerializable, IFastSerializableVersion 'structs can't inherit from classes (SerializableObject), so we explicitly implement these

            'Public Head1 As IntPtr 'Head
            'Public Head2 As IntPtr 'Head
            Public Left As IntPtr
            Public Parent As IntPtr
            Public Right As IntPtr
            ' these 2 (_Color,_IsNil) were moved before _MyVal in dev11: see //c:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\include\xtree  class _Tree_nod struct _Node
            Public Color_IsNil As Integer
            Public BlockType As BlockTypes ' key word 1
            Public Address As IntPtr 'key word 2
#If DEBUG Then
            Public Signature As Integer ' #define TRACKEDBLOCK_SIGNATURE  0x6b6c4254   // TBlk
#End If
            Public Size As Integer
            Public UnionData1 As Integer
            Public UnionData2 As Integer  'param passed into VirtualAlloc

            Public Shared Function GetTrackBlockFromAddress(ByVal dwAddress As IntPtr) As TrackBlockStruct
                Dim TBlk = New TrackBlockStruct
                Dim dwBytesRead = 0
                If _Offset_AllocToTblk = 5 Then ' dev10 layout is different: doesn't have Color and IsNil before MyVal, so we have to shift
                    If ReadProcessMemoryTrackBlockStruct(_hProcessTarget,
                                                         dwAddress,
                                                         TBlk,
                                                         Marshal.SizeOf(TBlk) - 4, dwBytesRead) = 0 Then
                        UpdateStatusMsg("ReadTrackBlockStruct5 failed " + dwAddress.ToString("x8"), fAssert:=True)
                    End If
                    TBlk.UnionData2 = TBlk.UnionData1
                    TBlk.UnionData1 = TBlk.Size
#If DEBUG Then
                    TBlk.Size = TBlk.Signature
                    TBlk.Signature = TBlk.Address.ToInt32
#Else
                    TBlk.Size = CInt(TBlk.Address)
#End If
                    TBlk.Address = CType(TBlk.BlockType, IntPtr)
                    TBlk.BlockType = CType(TBlk.Color_IsNil, BlockTypes)

                Else
                    If ReadProcessMemoryTrackBlockStruct(_hProcessTarget,
                                                         dwAddress,
                                                         TBlk,
                                                         Marshal.SizeOf(TBlk), dwBytesRead) = 0 Then
                        UpdateStatusMsg("ReadTrackBlockStruct6 failed " + dwAddress.ToString("x8"), fAssert:=True)
                    End If

                End If
                Debug.Assert(TBlk.IsValidTblk, "got tblk wtih invalid sig")
                Return TBlk
            End Function


            Public ReadOnly Property Version As Integer Implements IFastSerializableVersion.Version
                Get
                    Return SerializableObject.CurrentSerializationVersion
                End Get
            End Property

            Public ReadOnly Property MinimumVersion As Integer Implements IFastSerializableVersion.MinimumVersion
                Get
                    Return SerializableObject.CurrentMinimumVersion
                End Get
            End Property

            Public Function IsValidTblk() As Boolean
#If DEBUG Then
                If BlockType = BlockTypes.None OrElse Signature = TBLK_SIGNATURE Then
                    Return True
                End If
                Return False
#Else
                Return True
#End If
            End Function

            Public Function IsTrkBlkType(ByVal ttype As TrkType) As Boolean
                Dim fIsTrkBlkType = False
                Debug.Assert(IsValidTblk, "must be valid tblk type: invalid sig")
                Select Case ttype
                    Case TrkType.All
                        'If BlockType <> BlockTypes.IndirectInfo Then
                        '    fIsTrkBlkType = True
                        'End If
                        fIsTrkBlkType = True
                    Case TrkType.VirtualAllocs
                        If BlockType = BlockTypes.VirtualAlloc Then
                            fIsTrkBlkType = True
                        End If
                    Case TrkType.MappedFiles
                        Debug.Assert(False, "Mapped files now go to indirectinfo")

                    Case TrkType.HeapCreates
                        If BlockType = BlockTypes.HeapCreate Then
                            fIsTrkBlkType = True
                        End If
                    Case TrkType.GdiObjs
                        If BlockType = BlockTypes.GdiObjs Then
                            fIsTrkBlkType = True
                        End If
                    Case TrkType.CodeMarker
                        If BlockType = BlockTypes.CodeMarker Then
                            fIsTrkBlkType = True
                        End If
                    Case TrkType.ClrLoads ' combines Module,assembly,appdomain
                        If BlockType > BlockTypes.ClrClass AndAlso BlockType <= BlockTypes.ClrAppDomain Then
                            fIsTrkBlkType = True
                        End If

                    Case TrkType.ClrClasses
                        If BlockType = BlockTypes.ClrClass Then
                            fIsTrkBlkType = True
                        End If
                    Case TrkType.ClrObjects
                        If BlockType = BlockTypes.ClrObject Then
                            fIsTrkBlkType = True
                        End If
                    Case TrkType.ThreadInfo
                        If BlockType = BlockTypes.ThreadCreate OrElse BlockType = BlockTypes.TlsAllocFree Then
                            fIsTrkBlkType = True
                        End If
                    Case TrkType.Indirect
                        If BlockType = BlockTypes.IndirectInfo OrElse BlockType = BlockTypes.MapFile Then
                            fIsTrkBlkType = True
                        End If
                    Case TrkType.LoadResource
                        If BlockType = BlockTypes.LoadResource Then
                            fIsTrkBlkType = True
                        End If

                    Case TrkType.TrackGCStacks
                        If BlockType = BlockTypes.TrackGCStacks Then
                            fIsTrkBlkType = True
                        End If
                    Case TrkType.ClrJit
                        If BlockType = BlockTypes.ClrJit Then
                            fIsTrkBlkType = True
                        End If
                    Case TrkType.ClrGCHnd
                        If BlockType = BlockTypes.ClrGCHnd Then
                            fIsTrkBlkType = True
                        End If
                    Case TrkType.ClrExcpt
                        If BlockType = BlockTypes.ClrExcpt Then
                            fIsTrkBlkType = True
                        End If
                    Case Else
                        Debug.Assert(False, "Unknwon trktype")
                End Select
                Return fIsTrkBlkType
            End Function

            Public Sub ToStream(ByVal serializer As Serializer) Implements IFastSerializable.ToStream
                serializer.Write(Left.ToInt32())
                serializer.Write(Parent.ToInt32())
                serializer.Write(Right.ToInt32())
                serializer.Write([Enum].GetName(GetType(BlockTypes), BlockType))
                serializer.Write(Address.ToInt32())
                serializer.Write(TBLK_SIGNATURE)
                serializer.Write(Size)
                serializer.Write(UnionData1)
                serializer.Write(UnionData2)
            End Sub

            Public Sub FromStream(ByVal deserializer As Deserializer, ByVal versionFromStream As Integer) Implements IFastSerializable.FromStream
                Dim tempVal As Integer

                deserializer.Read(tempVal)
                Left = CType(tempVal, IntPtr)

                deserializer.Read(tempVal)
                Parent = CType(tempVal, IntPtr)

                deserializer.Read(tempVal)
                Right = CType(tempVal, IntPtr)

                Dim tempString As String = String.Empty
                deserializer.Read(tempString)
                BlockType = CType([Enum].Parse(GetType(BlockTypes), tempString), BlockTypes)

                deserializer.Read(tempVal)
                Address = CType(tempVal, IntPtr)
#If DEBUG Then
                deserializer.Read(Signature)
#Else
                deserializer.Read(tempVal)
#End If
                deserializer.Read(Size)
                deserializer.Read(UnionData1)
                deserializer.Read(UnionData2)
            End Sub
        End Structure

        Public Enum VBBilKind
            SYM_Uninitialized
            SYM_VoidType
            SYM_PointerType
            SYM_NamedType
            SYM_GenericBadNamedRoot
            SYM_Hash
            SYM_Alias
            SYM_CCContainer
            SYM_Class
            SYM_MethodImpl
            SYM_SyntheticMethod
            SYM_MethodDecl
            SYM_EventDecl
            SYM_DllDeclare
            SYM_Param
            SYM_ParamWithValue
            SYM_Variable
            SYM_VariableWithValue
            SYM_VariableWithArraySizes
            SYM_CCConstant
            SYM_StaticLocalBackingField
            SYM_Expression
            SYM_Implements
            SYM_Interface
            SYM_HandlesList
            SYM_ImplementsList
            SYM_Namespace
            SYM_NamespaceRing
            SYM_XmlName
            SYM_XmlNamespaceDeclaration
            SYM_XmlNamespace
            SYM_Property
            SYM_ArrayType
            SYM_ArrayLiteralType
            SYM_ApplAttr
            SYM_UserDefinedOperator
            SYM_GenericParam
            SYM_GenericConstraint
            SYM_GenericTypeConstraint
            SYM_GenericNonTypeConstraint
            SYM_GenericBinding
            SYM_GenericTypeBinding
            SYM_TypeForwarder
            SYM_ExtensionCallLookupResult
            SYM_LiftedOperatorMethod
        End Enum

        Public Interface IMemoryBlock
            Function Address() As ULong
            Function Size() As ULong
        End Interface

        <DebuggerDisplay("{ToString()}")>
        Public Class HeapAllocationContainer ' one of these for every allocation through VSAssert (chk & ret)
            Inherits SerializableObject
            Implements IComparable(Of HeapAllocationContainer)
            Implements IMemoryBlock

            Public Shared _NumInstances As Integer
            Public AllocationStruct As HeapAllocationStruct
            Public HeapBlockPtr As IntPtr ' addr of the HeapAllocationStruct. older snaps didn't save, so getting stack requires storing stack in snapshot, rather than getting from dump
            Public SpyHeapPtr As CSpyHeap

            Friend _tblkContainer As TrackBlockContainer
            Public Sub New()
                _NumInstances += 1
            End Sub

            Public ReadOnly Property TBlkBlockType As BlockTypes 'so we don't creating a null tblkcjust to query the type
                Get
                    Dim retval = BlockTypes.None
                    If _tblkContainer IsNot Nothing Then
                        retval = _tblkContainer._Tblk.BlockType
                    End If
                    Return retval
                End Get
            End Property
            Public Function IsLeakableType() As Boolean
                Dim fInclude = True
                Select Case TBlkBlockType
                    Case BlockTypes.CodeMarker, BlockTypes.TrackGCStacks
                        fInclude = False
                    Case BlockTypes.IndirectInfo
                        Select Case GetIndirectInfoType()
                            Case IndirectInfoType.IIType_FileLoad, IndirectInfoType.IIType_FileUnload
                                fInclude = False
                        End Select
                End Select
                Return fInclude
            End Function


            Public ReadOnly Property TBlk As TrackBlockStruct ' only valid for IsMemSpectHeap
                Get
                    If _tblkContainer Is Nothing Then
                        _tblkContainer = New TrackBlockContainer(Nothing)
                    End If
                    Return _tblkContainer._Tblk
                End Get
            End Property

            Public Function IsTrkBlkType(ByVal ttype As TrkType) As Boolean
                Dim fRetval = False
                If ttype = TrkType.All Then
                    fRetval = True
                Else
                    If _tblkContainer IsNot Nothing Then
                        fRetval = _tblkContainer._Tblk.IsTrkBlkType(ttype)
                    End If
                End If
                Return fRetval
            End Function

            Public GhostAlloc As AllocCtrGhost ' if it's a ghost alloc, else null
            ''' <summary>
            ''' When tracking ghosts, we track the alloc in GhostData. if it's freed, then IsGhostAlloc = true. 
            ''' </summary>
            Public ReadOnly Property IsGhostAlloc As Boolean
                Get
                    Dim fIsGhost = False
                    If GhostAlloc IsNot Nothing Then
                        fIsGhost = True
                    End If
                    Return fIsGhost
                End Get
            End Property

            '#Region "GroupID"
            '            ''' <summary>
            '            ''' GroupID functionality
            '            ''' </summary>
            '            ''' <remarks>Allocations can be grouped in a variety of ways.  Since the same allocation can possibly be grouped in multiple ways, we need to keep track of
            '            ''' each groupID by some identifier.  When the GroupID is displayed, the displayer needs to pass in the desired identifier to get the correct GroupID.  This allows for
            '            ''' the same allocation to be included in several different groups at the same time but always show the correct GroupID for a given view.</remarks>
            '            Private __groupID As Dictionary(Of Integer, Integer)
            '            Private ReadOnly Property _groupID As Dictionary(Of Integer, Integer)
            '                Get
            '                    If __groupID Is Nothing Then
            '                        __groupID = New Dictionary(Of Integer, Integer)
            '                    End If
            '                    Return __groupID
            '                End Get
            '            End Property

            '            Public Sub AddGroupID(ByVal identifier As Integer, ByVal groupID As Integer)
            '                If _groupID.ContainsKey(identifier) Then
            '                    _groupID(identifier) = groupID
            '                Else
            '                    _groupID.Add(identifier, groupID)
            '                End If
            '            End Sub

            '            Public Function GetGroupID(ByVal identifier As Integer) As Integer
            '                If _groupID.ContainsKey(identifier) Then
            '                    Return _groupID(identifier)
            '                End If

            '                Return -1
            '            End Function

            '            Public ReadOnly Property GroupIDs As Dictionary(Of Integer, Integer)
            '                Get
            '                    Return _groupID
            '                End Get
            '            End Property
            '#End Region


            Public ReadOnly Property IsMemSpectHeap As Boolean
                Get
                    Dim fIsMemspectHeap = False
                    If SpyHeapPtr IsNot Nothing Then
                        If SpyHeapPtr.IsMemSpectHeap Then
                            fIsMemspectHeap = True
                        End If
                    End If
                    Return fIsMemspectHeap
                End Get
            End Property

            Public ReadOnly Property GetSize As Integer
                Get
                    If IsMemSpectHeap Then
                        Return TBlk.Size
                    Else
                        If SpyHeapPtr IsNot Nothing AndAlso SpyHeapPtr.IsArenaHeap Then
                            Return GetArenaAllocSize
                        Else
                            Return AllocationStruct.Size
                        End If
                    End If
                End Get
            End Property

            Public ReadOnly Property GetAddr As IntPtr
                Get
                    If IsMemSpectHeap Then
                        Return TBlk.Address
                    Else
                        If SpyHeapPtr IsNot Nothing Then
                            If SpyHeapPtr.IsArenaHeap Then
                                Return GetArenaAllocAddr
                            End If
                        End If
                        Return AllocationStruct.Address
                    End If
                End Get
            End Property

            Public Class ArenaAllocationInfo

                Private _hCtr As HeapAllocationContainer
                Dim blkint As New ProcMemIntPtr

                Sub New(ByVal hCtr As HeapAllocationContainer)
                    _hCtr = hCtr
                    Debug.Assert(hCtr.SpyHeapPtr.IsArenaHeap)
                    Dim byteSizeArena = IntPtr.Size * 5
                    blkint.data = CType(Array.CreateInstance(GetType(IntPtr), NativeImports.BlockSize), IntPtr())
                    Dim nBytesRead = 0
                    ReadProcessMemoryDwordsEx(If(_ConnectionMode = MemSpectMode.Offline, IntPtr.Zero, _hProcessTarget),
                                              _hCtr.AllocationStruct.Address, blkint, byteSizeArena, nBytesRead)
                    Debug.Assert(nBytesRead = byteSizeArena)
                End Sub

                Public ReadOnly Property ArenaBlockType As ArenaBlkType
                    Get
                        Dim dwBlkType = blkint.data(0).ToInt32 ' 1st member is blk type
                        Dim res = CType(dwBlkType, ArenaBlkType)
                        Return res
                    End Get
                End Property

                Public ReadOnly Property ArenaAllocAddress As IntPtr
                    Get
                        If ArenaBlockType = ArenaBlkType.Alloc Then
                            Dim dwPtr = blkint.data(2) ' skip blktype,usertype
                            Return dwPtr
                        Else
                            Return _hCtr.AllocationStruct.Address
                        End If
                    End Get
                End Property

                Public ReadOnly Property ArenaAllocSize As Integer
                    Get
                        Dim res = 0
                        If ArenaBlockType = ArenaBlkType.Alloc Then
                            res = blkint.data(4).ToInt32
                        Else
                            res = _hCtr.AllocationStruct.Size
                        End If
                        Return res
                    End Get
                End Property

                Public ReadOnly Property ArenaAllocUserDefinedData As IntPtr
                    Get
                        Return blkint.data(1)
                    End Get
                End Property

                Public ReadOnly Property ArenaHeaderInfo As ArenaHeader
                    Get
                        Dim dwHdrPtr As IntPtr
                        Select Case ArenaBlockType
                            Case ArenaBlkType.Header
                                dwHdrPtr = _hCtr.AllocationStruct.Address
                            Case ArenaBlkType.Alloc
                                dwHdrPtr = blkint.data(3) ' 1st member is blk type, 2nd is userdef, 3rd is pData
                        End Select
                        Dim res = _hCtr.SpyHeapPtr.GetArenaHdrInfo(dwHdrPtr)
                        Return res
                    End Get
                End Property
            End Class

            Public Function GetArenaAllocationInfo(ByVal hCtr As HeapAllocationContainer) As ArenaAllocationInfo
                Dim res = New ArenaAllocationInfo(hCtr)
                Return res
            End Function

            Public ReadOnly Property GetArenaAllocAddr As IntPtr
                Get
                    If SpyHeapPtr.IsArenaHeap AndAlso GetArenaBlockType = ArenaBlkType.Alloc Then
                        Dim dwNrlsAddrPtr = ReadProcessMemoryDWORDEx(AllocationStruct.Address.MyAdd(4 * 2)) ' skip blktype,usertype
                        Return CType(dwNrlsAddrPtr, IntPtr)
                    Else
                        Return AllocationStruct.Address
                    End If
                End Get
            End Property

            Public ReadOnly Property GetArenaUserDefinedType As IntPtr
                Get
                    If SpyHeapPtr.IsArenaHeap Then
                        Dim dwNrlsUserType = ReadProcessMemoryDWORDEx(AllocationStruct.Address.MyAdd(4 * 1)) ' skip blktype
                        Return CType(dwNrlsUserType, IntPtr)
                    Else
                        Return IntPtr.Zero
                    End If
                End Get
            End Property

            Public ReadOnly Property GetArenaBlockType As ArenaBlkType
                Get
                    Dim res = ArenaBlkType.Alloc
                    If SpyHeapPtr.IsArenaHeap Then
                        Dim dwBlkType = ReadProcessMemoryDWORDEx(AllocationStruct.Address) ' 1st member is blk type
                        res = CType(dwBlkType, ArenaBlkType)
                    End If
                    Return res
                End Get
            End Property

            Public ReadOnly Property GetArenaAllocSize As Integer
                Get
                    Dim res = 0
                    If SpyHeapPtr.IsArenaHeap Then
                        If GetArenaBlockType = ArenaBlkType.Alloc Then
                            res = ReadProcessMemoryDWORDEx(AllocationStruct.Address.MyAdd(4 * 4))
                        Else
                            Debug.Assert(GetArenaBlockType = ArenaBlkType.Header)
                            res = AllocationStruct.Size
                        End If
                    End If
                    Return res
                End Get
            End Property

            'gets header info for both header and alloc types
            Public ReadOnly Property GetArenaHeaderInfo As ArenaHeader
                Get
                    Dim res As ArenaHeader = Nothing
                    If SpyHeapPtr.IsArenaHeap Then
                        Dim dwHdrPtr As IntPtr
                        Select Case GetArenaBlockType
                            Case ArenaBlkType.Header
                                dwHdrPtr = AllocationStruct.Address
                            Case ArenaBlkType.Alloc
                                dwHdrPtr = CType(ReadProcessMemoryDWORDEx(AllocationStruct.Address.MyAdd(4 * 3)), IntPtr) ' 1st member is blk type, 2nd is userdef, 3rd is pData
                        End Select
                        res = SpyHeapPtr.GetArenaHdrInfo(dwHdrPtr)
                    End If
                    Return res
                End Get
            End Property

            Public ReadOnly Property GetClassId As IntPtr
                Get
                    Dim classid = IntPtr.Zero
                    If IsMemSpectHeap Then
                        If TBlkBlockType = BlockTypes.ClrObject Then
                            classid = New IntPtr(TBlk.UnionData1)
                        ElseIf TBlkBlockType = BlockTypes.ClrClass Then
                            classid = Me.GetAddr
                        End If
                    End If
                    Return classid
                End Get
            End Property

            Public ReadOnly Property GetMovedCnt As Integer
                Get
                    Dim movedcnt As Integer
                    If IsMemSpectHeap Then
                        If TBlkBlockType = BlockTypes.ClrObject Then
                            movedcnt = &HFF And TBlk.UnionData2
                        End If
                    End If
                    Return movedcnt
                End Get
            End Property

            Public ReadOnly Property GetSurvivedCnt As Integer
                Get
                    Dim survivedcnt As Integer
                    If IsMemSpectHeap Then
                        If TBlkBlockType = BlockTypes.ClrObject Then
                            survivedcnt = TBlk.UnionData2 >> 16
                        End If
                    End If
                    Return survivedcnt
                End Get
            End Property
            ' get the gen of the object
            Public ReadOnly Property GetGen As Integer
                Get
                    Dim gen = 0
                    If IsMemSpectHeap Then
                        If TBlkBlockType = BlockTypes.ClrObject Then
                            gen = (TBlk.UnionData2 And &HFF00) >> 8
                        End If
                    End If
                    Return gen
                End Get
            End Property
            ' the reason for the gc
            Public ReadOnly Property GetGCReason As String
                Get
                    Dim reason = String.Empty
                    If IsMemSpectHeap Then
                        If TBlkBlockType = BlockTypes.TrackGCStacks Then
                            Dim nreason = (TBlk.UnionData1 And &HFF00) >> 8
                            Select Case nreason
                                Case 0
                                    reason = "Other"
                                Case 1
                                    reason = "Induced"
                                Case Else
                                    reason = "# " + nreason.ToString
                            End Select
                        End If
                    End If
                    Return reason
                End Get
            End Property
            ' get the gens being collected for this gc
            Public ReadOnly Property GetGCGens As String
                Get
                    Dim gcGens = String.Empty
                    If IsMemSpectHeap Then
                        If TBlkBlockType = BlockTypes.TrackGCStacks Then
                            Dim tmp = TBlk.UnionData1 And &HF
                            If (tmp And 8) <> 0 Then
                                gcGens += "3"
                            End If
                            If (tmp And 4) <> 0 Then
                                gcGens += "2"
                            End If
                            If (tmp And 2) <> 0 Then
                                gcGens += "1"
                            End If
                            If (tmp And 1) <> 0 Then
                                gcGens += "0"
                            End If
                        End If
                    End If
                    Return gcGens
                End Get
            End Property

            Public ReadOnly Property GetNumInstances As Integer
                Get
                    Dim Instcnt As Integer
                    If IsMemSpectHeap Then
                        If TBlkBlockType = BlockTypes.ClrClass Then
                            Instcnt = &HFFFF And TBlk.UnionData2
                        End If
                    End If
                    Return Instcnt
                End Get
            End Property

            Public ReadOnly Property GetNumCollected As Integer
                Get
                    Dim collectCnt As Integer
                    If IsMemSpectHeap Then
                        If TBlkBlockType = BlockTypes.ClrClass Then
                            collectCnt = TBlk.UnionData2 >> 16
                        End If
                    End If
                    Return collectCnt
                End Get
            End Property

            Public Function GetObjectRefData(ByVal brtypeRequest As NodeType) As List(Of List(Of HeapAllocationContainer))

                Dim retlist As New List(Of List(Of HeapAllocationContainer))
                'refs from is easy, find clrobjdump of heapcontainer and return refs

                If (brtypeRequest And NodeType.RefFromParent) = NodeType.RefFromParent Then
                    Dim objClrObjDump As ClrObjDump = Nothing
                    If GCData.GetCLRObjectRefDict.TryGetValue(Me.AllocationStruct.Address, objClrObjDump) Then
                        Dim frommelst = New List(Of HeapAllocationContainer)
                        For Each hAddr In objClrObjDump.Refs
                            Dim refObjDump = GCData.GetCLRObjectRefDict(hAddr)
                            frommelst.Add(refObjDump.hctnr)
                        Next
                        'Dim fromme = From item In GCData.GetCLRObjectRefDict.Values
                        '             Where objClrObjDump.Refs.Contains(item.hctnr.AllocationStruct.Address)
                        '             Select item.hctnr
                        'Dim frommelst = New List(Of HeapAllocationContainer)(fromme)
                        retlist.Add(frommelst)
                    Else
                        Dim errmsg = String.Format(
                            "GetCLRObjectRefDict value not found. (Possibly because obj was collected in subseq GC) Count =  {0} Addr={1:x8}",
                            GCData.GetCLRObjectRefDict.Count, Me.AllocationStruct.Address.ToInt32) +
                             " " + (New StackTrace).ToString
                        If _IsCalvinHBuild Then
                            UpdateStatusMsg("CalvinHBuild " + errmsg)
                        Else
                            Debug.Assert(False, errmsg)
                        End If
                    End If
                End If

                If (brtypeRequest And NodeType.RefToParent) = NodeType.RefToParent Then
                    'refs to, go through all clrobjdumps and return those that have a ref to refCtr
                    Dim tome = From item In GCData.GetCLRObjectRefDict.Values
                               Where item.Refs.Contains(Me.AllocationStruct.Address)
                               Select item.hctnr

                    Dim tomelst = New List(Of HeapAllocationContainer)(tome)
                    retlist.Add(tomelst)
                End If
                If (brtypeRequest And NodeType.PathFromGCRoot) = NodeType.PathFromGCRoot Then
                    Dim pgcr = Me.GetGCRootPaths()
                    retlist.AddRange(pgcr)
                End If
                Return retlist
            End Function

            Public Function GetClassLayoutFromClassIdForHCtr() As ClassLayoutDetail
                Dim cLayout As ClassLayoutDetail = Nothing
                Debug.Assert(Me.TBlkBlockType = BlockTypes.ClrClass)
                Dim clsid = Me.GetAddr
                Dim res = ClrClassInfo.GetClassLayoutsFromClassId(clsid)
                If res.Count > 0 Then
                    cLayout = res(res.Count - 1)
                Else
                    cLayout = New ClassLayoutDetail With {.classId = clsid}
                End If
                If _ConnectionMode = MemSpectMode.OnLine Then ' get updated instance counts when online
                    cLayout.classNumInstances = Me.GetNumInstances
                    cLayout.classNumCollected = Me.GetNumCollected
                End If
                Return cLayout
            End Function

            Public Function GetJitInfo() As String
                Dim jitinfo = String.Empty
                If TBlkBlockType = BlockTypes.ClrJit Then
                    Debug.Assert((TBlk.UnionData1 And MANAGED_STACK_FLAG) = 0, "jit functionId > MANAGED_STACK_FLAG")
                    Dim addr = CType(TBlk.UnionData1 Or MANAGED_STACK_FLAG, IntPtr)
                    jitinfo = ResolveAddressToSymbol(addr, fIsFunctionId:=True)
                End If
                Return jitinfo
            End Function

            Public Function GetJitMethodName() As String
                Dim methname = String.Empty
                If TBlkBlockType = BlockTypes.ClrJit Then
                    Dim inf = GetJitInfo()
                    Dim ndx = inf.LastIndexOf(".")
                    If ndx > 0 Then
                        methname = inf.Substring(ndx + 1)
                    End If
                End If
                Return methname
            End Function

            Public Function GetJitInfoAsmName() As String
                Dim fname = String.Empty
                Dim sym = GetJitInfo()
                Dim posBang = sym.IndexOf("!")
                If posBang > 0 Then
                    fname = sym.Substring(0, posBang)
                End If
                Return fname

            End Function

            Public Function GetExcptObjId() As IntPtr
                Dim excptObj = IntPtr.Zero
                If TBlkBlockType = BlockTypes.ClrExcpt Then
                    excptObj = CType(TBlk.UnionData1, IntPtr)
                End If
                Return excptObj
            End Function

            Public Function GetExcptClassId() As IntPtr
                Dim cls = IntPtr.Zero
                If TBlkBlockType = BlockTypes.ClrExcpt Then
                    cls = CType(TBlk.UnionData2, IntPtr)
                End If
                Return cls
            End Function

            Public Function GetGCHndInfoHandle() As IntPtr
                Dim gchnd = IntPtr.Zero
                If TBlkBlockType = BlockTypes.ClrGCHnd Then
                    gchnd = CType(TBlk.UnionData1, IntPtr)
                End If
                Return gchnd
            End Function

            Public Function GetGCHndInfoInitialObjectId() As IntPtr
                Dim gcdat = IntPtr.Zero
                If TBlkBlockType = BlockTypes.ClrGCHnd Then
                    gcdat = CType(TBlk.UnionData2, IntPtr)
                End If
                Return gcdat
            End Function

            'needs to be fast
            Public Function GetCallStackAddr(ByVal nIndex As Integer) As IntPtr
                Dim nRetval = IntPtr.Zero
                If nIndex < Me.AllocationStruct.m_uicStackAddr Then
                    If _ConnectionMode = MemSpectMode.Offline AndAlso
                        _offlineSnapshot._allocationStacks IsNot Nothing AndAlso
                        _offlineSnapshot._allocationStacks.Count > 0 Then ' older snaps didn't save heapblockptr. Also if stackstorage is in pagefile

                        Dim stk() As IntPtr = Nothing
                        If Common._offlineSnapshot._allocationStacks.TryGetValue(Me.AllocationStruct.Address, stk) Then
                            'Dim stk = _offlineSnapshot._allocationStacks(Me.AllocationStruct.Address)
                            If nIndex >= stk.Length Then
                                UpdateStatusMsg("getcallstackaddr failed " + nIndex.ToString + " " + stk.Length.ToString, fAssert:=True)
                            Else
                                nRetval = stk(nIndex)
                            End If
                        Else
                            UpdateStatusMsg("getcallstackaddr offline read failed.", fAssert:=True)
                        End If
                    Else
                        If IsGhostAlloc Then
                            nRetval = Me.GhostAlloc.callstkArray(nIndex)
                        Else
                            ' for offline too
                            If Not IsUsingStackMemMap Then
                                Dim nOffset = _OffsetCallStackFrames + nIndex * IntPtr.Size
                                Dim tmp = ReadProcessMemoryDWORDEx(Me.HeapBlockPtr.MyAdd(nOffset))
                                nRetval = New IntPtr(tmp)
                            Else
                                nRetval = StackMapFile.GetCallStackAddressFromMap(Me, nIndex)
                            End If
                        End If
                    End If
#If DEBUG Then
                    Dim sym = ResolveAddressToSymbol(nRetval)
#End If
                End If
                Return nRetval
            End Function

            ''' <summary>
            ''' works offline too
            ''' </summary>
            ''' <returns></returns>
            ''' <remarks></remarks>
            Public Function GetCallStackAddressestoArray(Optional ByVal nMaxStackFrames As Integer = 0) As IntPtr()
                '                Dim addrs(Math.Max(0, AllocationStruct.m_uicStackAddr - 1)) As IntPtr
                If AllocationStruct.m_uicStackAddr = 0 Then
                    Return New IntPtr() {IntPtr.Zero}
                End If
                If IsGhostAlloc Then
                    Return Me.GhostAlloc.callstkArray
                End If
                Dim res() As IntPtr = Nothing
                If AllocationStruct.m_uicStackAddr > 0 Then
                    If _ConnectionMode = MemSpectMode.Offline AndAlso _offlineSnapshot._allocationStacks IsNot Nothing AndAlso _offlineSnapshot._allocationStacks.Count > 0 Then
                        res = _offlineSnapshot._allocationStacks(Me.AllocationStruct.Address)
                    Else
                        If IsUsingStackMemMap Then
                            res = StackMapFile.GetCallStackAddressToArrayFromMap(Me, nMaxStackFrames)
                        Else
                            Dim addr = Me.HeapBlockPtr.MyAdd(_OffsetCallStackFrames)
                            Dim nBytesRead = 0
                            Dim nframestoget = AllocationStruct.m_uicStackAddr
                            If nMaxStackFrames > 0 Then
                                nframestoget = Math.Min(nframestoget, nMaxStackFrames)
                            End If
                            If _ConnectionMode = MemSpectMode.Offline Then
                                Dim blkintptr = New ProcMemIntPtr
                                Dim byteSizeStack = IntPtr.Size * nframestoget
                                blkintptr.data = CType(Array.CreateInstance(GetType(IntPtr), byteSizeStack), IntPtr())
                                Dim res2 = ReadProcessMemoryDwordsEx(IntPtr.Zero, addr, blkintptr, byteSizeStack, nBytesRead)
                                Debug.Assert(nBytesRead = byteSizeStack, "# bytes read wrong for GetCallStackAddressestoArray")
                                res = blkintptr.data
                            Else
                                ReDim res(AllocationStruct.m_uicStackAddr - 1)
                                nBytesRead = ReadProcessMemoryAsIntPtrArray(_hProcessTarget,
                                                        addr,
                                                        nframestoget,
                                                        IntPtr.Size,
                                                        res)
                                Debug.Assert(nBytesRead = nframestoget * IntPtr.Size, "# bytes read don't match stack")
                            End If
                        End If
                    End If
                End If
                Return res
            End Function

            Public Function GetGCRootExtraInfo() As String
                Dim extrainfo = String.Empty
                If Me.TBlk.Parent.ToInt32 = TBLK_SIGNATURE Then
                    Dim kind = GetGCRootKind()
                    Dim flags = GetGCRootFlags()
                    extrainfo = String.Format(" RootKind = {0} RootFlags = ""{1}"" RootId 0x{2:x8}", kind, flags, Me.TBlk.Left.ToInt32)
                End If
                Return extrainfo
            End Function

            Public Function GetGCRootKind() As String
                Dim dwkind = Me.TBlk.Right.ToInt32 And &HFFFF

                Dim kind = CType(dwkind, GCRootKind).ToString
                'Select Case dwkind
                '    Case 0
                '        kind = "Other"
                '    Case 1
                '        kind = "Stack"
                '    Case 2
                '        kind = "Finalizer"
                '    Case 3
                '        kind = "Handle"
                'End Select
                Return kind
            End Function

            Public Function GetGCRootFlagsAsInt() As Integer
                Dim dwflags = Me.TBlk.Right.ToInt32 >> 16
                Return dwflags
            End Function

            Public Function GetGCRootFlags() As String
                Dim flags = String.Empty
                Dim dwflags = Me.TBlk.Right.ToInt32 >> 16
                If dwflags <> 0 Then
                    flags = CType(dwflags, GCRootFlags).ToString ' like "Pinning, WeakRef"
                End If
                Return flags
            End Function

            ''' <summary>
            ''' works for ghost allocs too
            ''' </summary>
            Public Function GetClassNameFromHeapCtr(ByVal fExpandSystemStringOrArray As Boolean) As String
                If Not Common._ExpandStringContents Then
                    fExpandSystemStringOrArray = False
                End If
                If IsGhostAlloc Then
                    fExpandSystemStringOrArray = False
                End If

                Dim sRetval = String.Empty
                If Me.TBlkBlockType = BlockTypes.CodeMarker Then ' merged markers
                    Return Me.GetBlockTypeName
                End If

                If Me.IsMemSpectHeap Then
                    Dim bt = Me.TBlkBlockType
                    If Common._ConnectionMode = MemSpectMode.Offline Then
                        Select Case bt
                            Case BlockTypes.ClrClass, BlockTypes.ClrObject
                                sRetval = ClrClassInfo.GetClassNameFromClassOrObjectId(Me.GetClassId, Me.GetAddr, fExpandSystemStringOrArray)
                            Case BlockTypes.ClrExcpt
                                sRetval = ClrClassInfo.GetClassNameFromClassOrObjectId(Me.GetExcptClassId)
                        End Select
                    Else
                        Select Case bt
                            Case BlockTypes.ClrClass
                                sRetval = ClrClassInfo.GetClassNameFromClassOrObjectId(New IntPtr(Me.TBlk.UnionData1), fExpandSystemStringOrArray:=False)
                            Case BlockTypes.ClrObject
                                sRetval = ClrClassInfo.GetClassNameFromClassOrObjectId(New IntPtr(Me.TBlk.UnionData1), Me.GetAddr, fExpandSystemStringOrArray)
                                If IsGhostAlloc AndAlso Me.GhostAlloc.SeqNoWhenFreed > 0 Then
                                    sRetval += " Ghost freed at " + Me.GhostAlloc.SeqNoWhenFreed.ToString("n0")
                                End If
                            Case BlockTypes.ClrExcpt
                                sRetval = ClrClassInfo.GetClassNameFromClassOrObjectId(Me.GetExcptClassId)
                        End Select
                    End If 'online/offline
                End If 'ismemspectheap
                Return sRetval
            End Function

            Public Function GetGenericOrArrayCount() As Integer
                Dim nCnt = 0
                Dim cname = GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True)
                If cname.EndsWith(")") AndAlso
                    cname.Contains("Count=") Then
                    Dim ndx = cname.IndexOf("Count=")
                    Dim substr = cname.Substring(ndx + 6).Replace(")", String.Empty)
                    If Integer.TryParse(substr, nCnt) Then

                    End If
                End If
                Return nCnt
            End Function

            Public Function GetClrSyncBlockInfo() As Integer
                Dim syncblk = 0
                If TBlkBlockType = BlockTypes.ClrObject Then
                    syncblk = ReadProcessMemoryDWORDEx(GetAddr.MyAdd(-4))
                End If
                Return syncblk
            End Function

            Public Overrides ReadOnly Property Version As Integer
                Get
                    Return SerializableObject.CurrentSerializationVersion + 2 ' 5
                End Get
            End Property

            Public Overrides Sub ToStream(ByVal serializer As Serializer)
                'write out data common to all HeapAllocationContainers
                serializer.WriteDefered(Me.SpyHeapPtr) ' write spyheap first so we can tell what heap we're in
                serializer.WritePrivate(Me.AllocationStruct)
                If IsUsingStackMemMap Then
                    serializer.Write(CInt(0)) ' stacks are in page file, so won't be in minidump, so must be put in _allocationStacks
                Else
                    serializer.Write(Me.HeapBlockPtr.ToInt32) ' stacks are in minidump
                End If
                If IsMemSpectHeap Then
                    Debug.Assert(TBlkBlockType <> BlockTypes.None, "Memspect heap with no blocktype?")
                    serializer.WritePrivate(Me.TBlk)
                Else
                    Debug.Assert(TBlkBlockType = BlockTypes.None, "non-Memspect heap with blocktype?")

                End If
                'Dim nrlsData As New nrlsData
                'serializer.Write(nrlsData)

                'writes out the addresses for the callstack frames.  These will be used later to look up the actual frames from the
                'callstack frame dictionary
                Dim stackAddrs As IntPtr() = GetCallStackAddressestoArray()
                If IsUsingStackMemMap Then
                    serializer.Write(stackAddrs.Length)
                End If
                For i As Integer = 0 To stackAddrs.Length - 1
                    Dim addr = stackAddrs(i)
                    If Not _MegaSnapShotSymbolDict.Contains(addr) Then
                        _MegaSnapShotSymbolDict.Add(addr)
                    End If
                    'Dim symFrame = Common.ResolveAddressToSymbol(addr) ' resolving causes symname to be cached
                    If IsUsingStackMemMap Then
                        serializer.Write(stackAddrs(i).ToInt32())
                    End If
                Next

                'some allocations also contain special information, so switch over the blocktype and store the relevant information.
                'During deserialization, once the BlockType is known, we read in any special information and store it in the correct place.
                Select Case Me.TBlkBlockType
                    Case BlockTypes.HeapCreate
                        Dim heapName = HeapNameConvertFromRaw(New IntPtr(Me.TBlk.UnionData1), GetStringFromRemoteMem(New IntPtr(Me.TBlk.UnionData2), 0, _hProcessTarget))
                        serializer.Write(heapName)

                    Case BlockTypes.ClrModule, BlockTypes.ClrAssembly, BlockTypes.ClrAppDomain
                        SendMsg(ProcMsgVerb.GetClrData, fSendEndMsgSync:=True, dwords:={Me.HeapBlockPtr.ToInt32, 0})
                        Dim className = ReadSharedMemAsString(Common.ClassNameOffset, fAnsi:=False)
                        serializer.Write(className)

                    Case BlockTypes.ClrClass
                        SendMsg(ProcMsgVerb.GetClrData, fSendEndMsgSync:=True, dwords:={Me.HeapBlockPtr.ToInt32, 1})
                        Dim nInst = Marshal.ReadInt32(_SharedMemAddr, 0)
                        Dim nColl = Marshal.ReadInt32(_SharedMemAddr, 4)
                        Dim nClassSize = Marshal.ReadInt32(_SharedMemAddr, 8)
                        Dim sClass = ReadSharedMemAsString(ClassNameOffset, fAnsi:=False)

                        serializer.Write(nInst)
                        serializer.Write(nColl)
                        serializer.Write(nClassSize)
                        serializer.Write(sClass)

                    Case BlockTypes.ClrObject
                        Dim className As String = Me.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False) ' don't want expansion for raw class dict
                        serializer.Write(className)

                        'System.String objects carry all or part of the actual string in the classname.  If we find strings,
                        'we need to split up the classname and actual string and store them separately so lookups and caches work correctly.
                        If className = ("System.String") Then
                            Try
                                Dim classNamewithString = Me.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True)
                                Dim splitIndex = classNamewithString.IndexOf(" ")
                                If splitIndex < 0 Then
                                    serializer.Write(String.Empty)
                                Else
                                    Dim stringContents = classNamewithString.Substring(splitIndex + 1)
                                    serializer.Write(stringContents)
                                End If

                            Catch ex As Exception
                                UpdateStatusMsg("Exception encountered while writing System.String info.", fAssert:=True)
                                MemSpectExceptionHandler(ex)
                            End Try
                        End If

                    Case Common.BlockTypes.MapFile
                        Dim mfName As String = Me.GetMappedFilenameFromAllocation()
                        serializer.Write(mfName)

                    Case Common.BlockTypes.CodeMarker
                        Dim cmName As String = GetCodeMarkerName(fIncludeInstanceNum:=False)
                        serializer.Write(cmName)

                    Case BlockTypes.ClrJit
                        Dim jitName = GetJitInfo() ' causes sym res to get name into stackframe dict
                    Case BlockTypes.ClrGCHnd

                    Case BlockTypes.ThreadCreate
                        Dim x = GetBlockTypeName() ' force symbol resolution
                    Case Else

                End Select
            End Sub

            Public Overrides Sub FromStream(ByVal deserializer As Deserializer, ByVal versionFromStream As Integer)
                Dim fMustReadStacksFromStream = False
                If versionFromStream = CurrentSerializationVersion + 2 Then ' newer
                    deserializer.Read(Me.SpyHeapPtr) ' so we can tell if IsMemSpectHeap
                    deserializer.Read(Me.AllocationStruct)
                    Dim tmp = CInt(0)
                    deserializer.Read(tmp)
                    If tmp = 0 Then
                        fMustReadStacksFromStream = True ' stacks were stored in page file, so can't be read from minidump, so we need to store in _allocationStacks
                    Else
                        Me.HeapBlockPtr = New IntPtr(tmp)
                    End If
                    If IsMemSpectHeap Then
                        Dim tblkc = New TrackBlockContainer(Nothing)
                        deserializer.Read(tblkc._Tblk)
                        _tblkContainer = tblkc
                    End If
                Else
                    If versionFromStream = CurrentSerializationVersion + 1 Then ' newer
                        deserializer.Read(Me.SpyHeapPtr) ' so we can tell if IsMemSpectHeap
                        deserializer.Read(Me.AllocationStruct)
                        If IsMemSpectHeap Then
                            Dim tblkc = New TrackBlockContainer(Nothing)
                            deserializer.Read(tblkc._Tblk)
                            _tblkContainer = tblkc
                        End If
                    Else ' older
                        deserializer.Read(Me.AllocationStruct)
                        Dim tblkc = New TrackBlockContainer(Nothing)
                        deserializer.Read(tblkc._Tblk)
                        _tblkContainer = tblkc
                        Dim nrlsData As New nrlsData
                        deserializer.Read(nrlsData)
                        deserializer.Read(Me.SpyHeapPtr)
                    End If
                    fMustReadStacksFromStream = True ' older versions stored stack in main mem, so it's in minidump
                End If

                If fMustReadStacksFromStream Then
                    Dim stackSize As Integer = 0
                    deserializer.Read(stackSize)
                    Dim stacks(stackSize - 1) As IntPtr
                    For i As Integer = 0 To stackSize - 1
                        stacks(i) = CType(deserializer.ReadInt(), IntPtr)
                    Next
                    SyncLock (Common._offlineSnapshot._allocationStacks)
                        If Not Common._offlineSnapshot._allocationStacks.ContainsKey(Me.AllocationStruct.Address) Then
                            Common._offlineSnapshot._allocationStacks.Add(Me.AllocationStruct.Address, stacks)
                        End If
                    End SyncLock

                End If

                Select Case Me.TBlkBlockType
                    Case BlockTypes.HeapCreate
                        Dim heapName As String = String.Empty
                        deserializer.Read(heapName)
                        If Not Common._offlineSnapshot._heapCreateDictionary.ContainsKey(Me.AllocationStruct.Address) Then
                            Common._offlineSnapshot._heapCreateDictionary.Add(Me.AllocationStruct.Address, heapName)
                        End If

                        Exit Select

                    Case BlockTypes.ClrModule, BlockTypes.ClrAssembly, BlockTypes.ClrAppDomain
                        Dim className = String.Empty
                        deserializer.Read(className)
                        If Not Common._offlineSnapshot._clrLoadDictionary.ContainsKey(Me.AllocationStruct.Address) Then
                            Common._offlineSnapshot._clrLoadDictionary.Add(Me.AllocationStruct.Address, className)
                        End If

                        Exit Select

                    Case BlockTypes.ClrClass
                        Dim nInst As Integer = 0
                        Dim nColl As Integer = 0
                        Dim nClassSize As Integer = 0
                        Dim sClass As String = String.Empty

                        deserializer.Read(nInst)
                        deserializer.Read(nColl)
                        deserializer.Read(nClassSize)
                        deserializer.Read(sClass)
                        Dim classLayoutDet As ClassLayoutDetail = Nothing
                        If ClrClassInfo.g_DictClassLayouts.TryGetValue(Me.GetAddr, classLayoutDet) Then
                        Else
                            classLayoutDet = New ClassLayoutDetail With {.classId = Me.GetAddr}
                            ClrClassInfo.g_DictClassLayouts(Me.GetAddr) = classLayoutDet
                        End If
                        classLayoutDet.classNumInstances = nInst
                        classLayoutDet.classNumCollected = nColl
                        classLayoutDet.classSize = nClassSize
                        classLayoutDet.className = sClass

                        Exit Select

                    Case BlockTypes.ClrObject
                        Dim className As String = String.Empty
                        deserializer.Read(className)
                        Dim classLayoutDet As ClassLayoutDetail = Nothing
                        If ClrClassInfo.g_DictClassLayouts.TryGetValue(Me.GetClassId, classLayoutDet) Then
                        Else
                            classLayoutDet = New ClassLayoutDetail With {.classId = Me.GetClassId}
                            ClrClassInfo.g_DictClassLayouts(Me.GetClassId) = classLayoutDet
                        End If
                        classLayoutDet.className = className

                        If className = "System.String" Then
                            Dim stringContents As String = String.Empty
                            deserializer.Read(stringContents)
                            If Not Common._offlineSnapshot._systemStringDictionary.ContainsKey(Me.GetAddr) Then
                                Common._offlineSnapshot._systemStringDictionary.Add(Me.GetAddr, stringContents)
                            End If
                        End If
                        Exit Select

                    Case Common.BlockTypes.MapFile
                        Dim mfName As String = String.Empty
                        deserializer.Read(mfName)
                        If Not Common._offlineSnapshot._mappedFilesDictionary.ContainsKey(Me.GetAddr) Then
                            Common._offlineSnapshot._mappedFilesDictionary.Add(Me.GetAddr, mfName)
                        End If
                        Exit Select

                    Case Common.BlockTypes.CodeMarker
                        Dim cmName As String = String.Empty
                        deserializer.Read(cmName)
                        'If Not Common._offlineSnapshot._codemarkerDictionary.ContainsKey(Me.TBlk.UnionData1) Then
                        '    Common._offlineSnapshot._codemarkerDictionary.Add(Me.TBlk.UnionData1, cmName)
                        'End If
                        Exit Select
                    Case BlockTypes.None
                        _tblkContainer = Nothing ' todo: next version don't even read/write these
                    Case BlockTypes.ClrJit
                    Case BlockTypes.ClrGCHnd
                    Case BlockTypes.ClrExcpt
                    Case BlockTypes.None
                    Case BlockTypes.TlsAllocFree
                    Case BlockTypes.ThreadCreate
                    Case BlockTypes.VirtualAlloc
                    Case BlockTypes.HeapAlloc
                    Case BlockTypes.IndirectInfo
                    Case BlockTypes.LoadResource
                    Case BlockTypes.TrackGCStacks
                    Case BlockTypes.GdiObjs
                    Case Else
                        Throw New NotImplementedException("tblk type fromstream " + TBlkBlockType.ToString)
                End Select

            End Sub

            Public Class Location
                Inherits SerializableObject

                Public Sub New()

                End Sub
                Public m_lBegLine As Integer
                Public m_lEndLine As Integer
                Public m_lBegColumn As Integer
                Public m_lEndColumn As Integer
                Public Overrides Function ToString() As String
                    Return String.Format("({0},{1})-({2},{3})", m_lBegLine, m_lBegColumn, m_lEndLine, m_lEndColumn)
                End Function

                Public Overrides Sub ToStream(ByVal serializer As Serializer)
                    serializer.Write(Me.m_lBegLine)
                    serializer.Write(Me.m_lEndLine)
                    serializer.Write(Me.m_lBegColumn)
                    serializer.Write(Me.m_lEndColumn)
                End Sub

                Public Overrides Sub FromStream(ByVal deserializer As Deserializer, ByVal versionFromStream As Integer)
                    deserializer.Read(Me.m_lBegLine)
                    deserializer.Read(Me.m_lEndLine)
                    deserializer.Read(Me.m_lBegColumn)
                    deserializer.Read(Me.m_lEndColumn)
                End Sub
            End Class


            Public Class nrlsData
                Inherits SerializableObject

                Public Sub New()

                End Sub

                Public NrlsSymName As String = String.Empty
                Public NrlsSymFile As String = String.Empty
                Public NrlsSymProj As String = String.Empty
                Public NrlsSymContainer As String = String.Empty
                Public NrlsBorn As Integer
                Public NrlsLastRef As Integer
                Public NrlsCallCount As Integer
                Public NrlsPctZero As Integer

                Public _Location As Location
                Public ReadOnly Property GetLocation As String
                    Get
                        Dim str = String.Empty
                        If _Location IsNot Nothing Then
                            str = _Location.ToString
                        End If
                        Return str
                    End Get
                End Property

                Public Overrides Sub ToStream(ByVal serializer As Serializer)
                    serializer.Write(Me.NrlsSymName)
                    serializer.Write(Me.NrlsSymFile)
                    serializer.Write(Me.NrlsSymProj)
                    serializer.Write(Me.NrlsSymContainer)
                    serializer.Write(Me.NrlsBorn)
                    serializer.Write(Me.NrlsLastRef)
                    serializer.Write(Me.NrlsCallCount)
                    serializer.Write(Me.NrlsPctZero)
                    serializer.Write(Me._Location)
                End Sub

                Public Overrides Sub FromStream(ByVal deserializer As Deserializer, ByVal versionFromStream As Integer)
                    deserializer.Read(Me.NrlsSymName)
                    deserializer.Read(Me.NrlsSymFile)
                    deserializer.Read(Me.NrlsSymProj)
                    deserializer.Read(Me.NrlsSymContainer)
                    deserializer.Read(Me.NrlsBorn)
                    deserializer.Read(Me.NrlsLastRef)
                    deserializer.Read(Me.NrlsCallCount)
                    deserializer.Read(Me.NrlsPctZero)
                    deserializer.Read(Me._Location)

                End Sub
            End Class

            <DebuggerDisplay("{ToString()}")>
            Public Class CodeMarkerData
                Public Property MarkerName As String ' the raw name only
                Public ReadOnly Property MarkerNameEx As String ' indented, with instance and depth
                    Get
                        Return String.Format(
                            "{0}{1}{2} Depth={3} SeqNo={4:n0} {5}",
                            IIf(MarkerDepth >= 0, New String(CChar(" "), MarkerDepth), "NegMarkerDepth" + MarkerDepth.ToString()),
                            MarkerName,
                            MarkerInstance,
                            MarkerDepth,
                            _hctr.AllocationStruct.SeqNo,
                            MarkerUserData)
                    End Get
                End Property
                Public Property MarkerDepth As Integer
                Public Property MarkerInstance As Integer
                Public Property MarkerUserData As String
                Public Property MarkerUserDataAddr As IntPtr
                Public Property MarkerUserDataLen As Integer
                Friend _hctr As HeapAllocationContainer
                Public Overrides Function ToString() As String
                    Return MarkerNameEx
                End Function
            End Class
            ' guid (or string) to string. guid includes braces
            Friend Shared _dictCodeMarkerGuidLookup As Dictionary(Of String, String)
            Public Shared ReadOnly Property dictCodeMarkerGuidLookup As Dictionary(Of String, String)
                Get
                    If _dictCodeMarkerGuidLookup Is Nothing Then
                        Dim testPath = CodeMarkerGuidLookupFilename
                        If Not File.Exists(testPath) Then
                            Dim dir = Reflection.Assembly.GetAssembly(GetType(Common)).Location
                            testPath = Path.Combine(dir, CodeMarkerGuidLookupFilename)
                        End If
                        If File.Exists(testPath) Then
                            Dim lines = File.ReadAllLines(CodeMarkerGuidLookupFilename)
                            _dictCodeMarkerGuidLookup = New Dictionary(Of String, String)
                            For Each line In lines.Where(Function(o) o.StartsWith("{") AndAlso o.Length > 37 AndAlso o(37) = "}")
                                Dim guidstr = line.Substring(0, 38)
                                Dim guid = New Guid(guidstr)
                                Dim val = line.Substring(38).Trim
                                If Not String.IsNullOrEmpty(val) Then
                                    '"B" includes Braces {}
                                    _dictCodeMarkerGuidLookup(guid.ToString("B")) = line.Substring(39)
                                End If
                            Next
                        End If
                    End If
                    Return _dictCodeMarkerGuidLookup
                End Get
            End Property

            Public Function GetCodeMarkerData() As CodeMarkerData
                Dim codeMarkerName As String = String.Empty
                Dim CustomMarkerNameLen = 0
                Dim nDepthLevel = 0
                Dim dwMarkerInstance = 0
                Dim mrkerUserData = String.Empty
                Dim dwAddrUserData = IntPtr.Zero
                Dim dwUserDataLen = 0
                If TBlk.UnionData2 <> 0 Then ' the indirectinfo can't be put in a dict
                    ' for CodeMarker,UnionDat1 is the marker id and UnionData2 is IndirectInfo
                    Dim blkInt As ProcMemIntPtr
                    Dim dwBytesRead = 0
                    blkInt.data = CType(Array.CreateInstance(GetType(IntPtr), NativeImports.BlockSize), IntPtr())

                    ReadProcessMemoryDwordsEx(
                        __hProcessTarget,
                        CType(TBlk.UnionData2, IntPtr),
                        blkInt,
                        IntPtr.Size * 6,
                        dwBytesRead
                        )
                    Dim iitype = CType(blkInt.data(0).ToInt32, IndirectInfoType)
                    Debug.Assert(iitype = IndirectInfoType.IIType_CustomCodeMarker OrElse iitype = IndirectInfoType.IIType_CustomCodeMarkerWithUserData, "Got invalid iitype " + iitype.ToString)
                    dwMarkerInstance = blkInt.data(1).ToInt32
                    Dim dw = blkInt.data(2).ToInt32
                    nDepthLevel = dw >> 16
                    Dim nEventType = CType(dw And &HFFFF, CodeMarkerEventType)
                    Dim nameOffset = 20
                    If iitype = IndirectInfoType.IIType_CustomCodeMarkerWithUserData Then
                        dwAddrUserData = blkInt.data(3)

                        dwUserDataLen = blkInt.data(4).ToInt32
                        Select Case dwUserDataLen
                            Case 4
                                mrkerUserData = ReadProcessMemoryDWORDEx(dwAddrUserData).ToString("x8")
                            Case 8
                                mrkerUserData = String.Format("{0} {1}",
                                                              ReadProcessMemoryDWORDEx(dwAddrUserData).ToString("x8"),
                                                              ReadProcessMemoryDWORDEx(dwAddrUserData.MyAdd(4)).ToString("x8")
                                                              )
                            Case 16
                                Dim blkint2 As ProcMemIntPtr
                                blkint2.data = CType(Array.CreateInstance(GetType(IntPtr), NativeImports.BlockSize), IntPtr())
                                ReadProcessMemoryDwordsEx(
                                    __hProcessTarget,
                                    dwAddrUserData,
                                    blkint2,
                                    IntPtr.Size * 4,
                                    dwBytesRead
                                    )
                                Dim byts(15) As Byte
                                For i = 0 To 3
                                    Dim wrd = blkint2.data(i).ToInt32
                                    '                                    Dim wrd = ReadProcessMemoryDWORDEx(dwAddrUserData.MyAdd(i * 4))
                                    Dim tmpByts = BitConverter.GetBytes(wrd)
                                    byts(i * 4 + 0) = tmpByts(0)
                                    byts(i * 4 + 1) = tmpByts(1)
                                    byts(i * 4 + 2) = tmpByts(2)
                                    byts(i * 4 + 3) = tmpByts(3)
                                Next
                                Dim guidMarker = New Guid(byts)
                                If dictCodeMarkerGuidLookup IsNot Nothing AndAlso Not dictCodeMarkerGuidLookup.TryGetValue(guidMarker.ToString("B"), mrkerUserData) Then
                                    mrkerUserData = guidMarker.ToString() + " " + guidMarker.ToString
                                End If

                            Case Else
                                mrkerUserData = ReadProcessMemAsString(dwAddrUserData, nMaxLen:=dwUserDataLen)
                        End Select
                    End If
                    CustomMarkerNameLen = blkInt.data(5).ToInt32
                    If CustomMarkerNameLen > 0 Then
                        codeMarkerName = ReadProcessMemAsString(CType(TBlk.UnionData2, IntPtr).MyAdd(nameOffset + 4), nMaxLen:=CustomMarkerNameLen + 1) ' include nullterm
                        If nEventType <> CodeMarkerEventType.None Then
                            codeMarkerName += nEventType.ToString
                        End If
                    End If
                    'codeMarkerName += " EvType= " + nEventType.ToString + " Depth= " + nDepthLevel.ToString
                End If
                If CustomMarkerNameLen = 0 Then ' is it a standard (non-custom) code marker?
                    codeMarkerName = GetCodeMarkerNameRaw(Me.TBlk.UnionData1)
                End If
                Dim res = New CodeMarkerData With {
                    .MarkerName = codeMarkerName,
                    .MarkerInstance = dwMarkerInstance,
                    .MarkerDepth = nDepthLevel,
                    .MarkerUserData = mrkerUserData,
                    .MarkerUserDataAddr = dwAddrUserData,
                    .MarkerUserDataLen = dwUserDataLen,
                    ._hctr = Me
                    }
                Return res
            End Function

            Public Function GetCodeMarkerName(Optional ByVal fIncludeInstanceNum As Boolean = False) As String
                Dim markerData = GetCodeMarkerData()
                If fIncludeInstanceNum Then
                    Return markerData.MarkerNameEx
                Else
                    Return markerData.MarkerName
                End If
            End Function

            Public Function GetCodeMarkerType() As CodeMarkerType
                Dim markerType = CodeMarkerType.None
                If TBlkBlockType = BlockTypes.CodeMarker Then
                    markerType = CodeMarkerType.NormalMarker
                    If TBlk.UnionData1 <> 0 Then
                        Dim customMarkerNameLen = ReadProcessMemoryDWORDEx(CType(TBlk.UnionData2, IntPtr).MyAdd(20))
                        If customMarkerNameLen > 0 Then
                            markerType = CodeMarkerType.CustomMarker
                        End If
                    End If
                End If
                Return markerType
            End Function

            Public Function GetCallStackAsString() As String
                Dim symnames = GetCallStackAsStringArray()
                Dim sb As New Text.StringBuilder
                If symnames.Count > 0 Then
                    sb.AppendLine("Call Stack:")
                    For Each frame In symnames
                        sb.AppendLine(frame)
                    Next
                Else
                    sb.Append("no stackframes logged. Check TrackingMode (_NT_SYMBOL_PATH can help resolve symbols)")
                End If
                Return sb.ToString
            End Function

            Public Function GetCallStackAsStringArray(
                                        Optional ByVal nIndex As Integer = 0,
                                        Optional ByVal nNumToTryToGet As Integer = 0) As String()
                ' note: address could be of Heap block or of target of VirtualAlloc (__VMTrack)
                Dim frameNames As New List(Of String)
                Try
                    If Me.AllocationStruct.m_uicStackAddr <> 0 Then

                        Dim nLastFrameToget As Integer = Me.AllocationStruct.m_uicStackAddr - 1
                        If nNumToTryToGet > 0 Then
                            nLastFrameToget = Math.Min(nIndex + nNumToTryToGet - 1, nLastFrameToget)
                        End If
                        If nLastFrameToget > 1000 Then
                            Throw New InvalidDataException("stack frame count too big")
                        End If
                        ' we want to get frame # nIndex to nLastFrameToGet
                        If nLastFrameToget <= Me.AllocationStruct.m_uicStackAddr Then
                            Dim stackAddrs = GetCallStackAddressestoArray()
                            nLastFrameToget = Math.Min(nLastFrameToget, stackAddrs.Length - 1)

                            For i = nIndex To nLastFrameToget
                                Dim addrToresolve = stackAddrs(i)
                                Dim frame = ResolveAddressToSymbol(addrToresolve)
                                If _ShowAddressInCallStacks <> 0 Then  ' both the hex address and the stack index will show before the frame, like "b73a979d 00000100 " + frame
                                    frame = addrToresolve.ToInt32.ToString("x8") + " " + frame
                                    If _fHandle4gigStacks Then
                                        Dim dwRealAddress = IntPtr.Zero
                                        If _stackIndexDict.TryGetValue(addrToresolve, dwRealAddress) Then
                                            frame = dwRealAddress.ToInt32.ToString("x8") + " " + frame
                                        End If
                                    End If
                                End If
                                frameNames.Add(frame)
                            Next
                        End If
                    End If
                Catch ex As Exception
                    frameNames.Add("Err getting sframe " + ex.ToString)
                End Try
                Return frameNames.ToArray
            End Function

            Public Shared Operator =(ByVal p1 As HeapAllocationContainer, ByVal p2 As HeapAllocationContainer) As Boolean
                If p1.AllocationStruct.Address = p2.AllocationStruct.Address Then
                    Return True
                End If
                Return False
            End Operator

            Public Shared Operator <>(ByVal p1 As HeapAllocationContainer, ByVal p2 As HeapAllocationContainer) As Boolean
                If p1.AllocationStruct.Address <> p2.AllocationStruct.Address Then
                    Return True
                End If
                Return False
            End Operator

            Public Function CompareTo(ByVal other As HeapAllocationContainer) As Integer Implements IComparable(Of HeapAllocationContainer).CompareTo
                Dim nSize = Me.GetSize
                If nSize < other.GetSize Then
                    Return -1
                End If
                If nSize > other.GetSize Then
                    Return 1
                End If
                If TBlkBlockType = BlockTypes.ClrObject Then
                    nSize -= IntPtr.Size 'size of clro includes syncblk header at offset -4
                End If
                Dim addr1 = Me.GetAddr
                addr1 += Me.SpyHeapPtr.nHeaderSize
                Dim addr2 = other.GetAddr
                addr2 += other.SpyHeapPtr.nHeaderSize
                If addr1 = addr2 Then
                    Return 0
                End If
                Dim nBytes = CInt(nSize / 4) * 4 ' round
                Dim ptr1 = addr1
                Dim blk1 As New ProcMemBlockByte
                Dim ptr2 = addr2
                Dim blk2 As New ProcMemBlockByte
                Dim blkCnt = 0
                Dim blksize = CInt(Marshal.SizeOf(blk1) / 4) ' probability of dupe is low, so read less mem

                For i = 0 To nBytes - 1
                    If i Mod blksize = 0 Then
                        Dim nBytesToReadThisTime = (nBytes - blkCnt * blksize)
                        If nBytesToReadThisTime > blksize Then
                            nBytesToReadThisTime = blksize
                        End If
                        Dim addr1toread = addr1.MyAdd(blkCnt * blksize)
                        Dim addr2toread = addr2.MyAdd(blkCnt * blksize)

                        If _ConnectionMode = MemSpectMode.Offline Then
                            blk1.data = MiniDumpReader.Singleton.ReadMemoryDictionary(addr1toread, nBytesToReadThisTime)
                            blk2.data = MiniDumpReader.Singleton.ReadMemoryDictionary(addr2toread, nBytesToReadThisTime)
                            If blk1.data Is Nothing OrElse blk2.data Is Nothing Then
                                Return 0
                            End If
                        Else
                            ReDim blk1.data(nBytesToReadThisTime - 1)
                            ReDim blk2.data(nBytesToReadThisTime - 1)
                            Dim dwBytesRead = ReadProcessMemoryAsByteArray(_hProcessTarget,
                                                                           addr1toread,
                                                                           nBytesToReadThisTime,
                                                                           1,
                                                                           blk1.data)
                            If dwBytesRead <= 0 Then
                                'If ReadProcessMemoryByte(_hProcessTarget, addr1toread, blk1, nBytesToReadThisTime, dwBytesRead) = 0 Then
                                Dim lerr = Marshal.GetLastWin32Error
                                UpdateStatusMsg("readmem failed1 " + addr1.ToString("x8") + " LastErr= " + lerr.ToString + " " + GetErrorMessageFromWin32LastError(lerr), fAssert:=True)
                            End If
                            dwBytesRead = ReadProcessMemoryAsByteArray(_hProcessTarget,
                                                                        addr2toread,
                                                                        nBytesToReadThisTime,
                                                                        1,
                                                                        blk2.data)
                            If dwBytesRead <= 0 Then
                                'If ReadProcessMemoryByte(_hProcessTarget, addr2toread, blk2, nBytesToReadThisTime, dwBytesRead) = 0 Then
                                Dim lerr = Marshal.GetLastWin32Error
                                UpdateStatusMsg("readmem failed2 " + addr1.ToString("x8") + " LastErr= " + lerr.ToString + " " + GetErrorMessageFromWin32LastError(lerr), fAssert:=True)
                            End If

                        End If
                        blkCnt += 1
                    End If
                    Dim b1 = blk1.data(i Mod blksize)
                    Dim b2 = blk2.data(i Mod blksize)
                    Dim res = b1.CompareTo(b2)
                    If res <> 0 Then
                        Return res
                    End If
                Next
                Return 0 ' dup!
            End Function

            Public Overrides Function ToString() As String
                Dim str = String.Empty
                str = String.Format("Address=0x{0:x8}, SeqNo={1:n0}, Size={2:n0}, BlkType={3} Thread={4} {5}",
                                    GetAddr.ToInt32,
                                    AllocationStruct.SeqNo,
                                    GetSize,
                                    TBlkBlockType.ToString,
                                    AllocationStruct.Thread,
                                    GetDisplayData(nMode:=GetDisplayDataEnum.DisplayShort)).TrimEnd
                '#If DEBUG Then
                'If TBlkBlockType = BlockTypes.ClrObject Then
                '    str = GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False) + " " + str
                'End If
                '#End If
                Return str
            End Function

            Public Function GetHeapNameForHeapCreate() As String
                If TBlkBlockType = BlockTypes.CodeMarker Then ' merged markers
                    Return Me.GetBlockTypeName
                End If
                If Common._ConnectionMode = MemSpectMode.Offline Then
                    If Common._offlineSnapshot._heapCreateDictionary.ContainsKey(Me.AllocationStruct.Address) Then
                        Return Common._offlineSnapshot._heapCreateDictionary(Me.AllocationStruct.Address)
                    Else
                        Return "Failed to find heap create name in snapshot."
                    End If
                Else
                    Return HeapNameConvertFromRaw(New IntPtr(Me.TBlk.UnionData1), GetStringFromRemoteMem(New IntPtr(Me.TBlk.UnionData2), 0, _hProcessTarget))
                End If
            End Function

            Public Function GetBlockTypeName() As String
                Dim res = String.Empty
                res = Me.TBlkBlockType.ToString()
                Select Case Me.TBlkBlockType
                    Case BlockTypes.None
                        res = "NativeHeap"
                    Case BlockTypes.CodeMarker
                        res = "CM: " + TBlk.UnionData1.ToString() + " " + GetCodeMarkerName()
                    Case BlockTypes.ThreadCreate
                        res = String.Format("NewId {0,5} ThrdStart:{1}", TBlk.UnionData1, ResolveAddressToSymbol(CType(TBlk.UnionData2, IntPtr)))
                    Case BlockTypes.TlsAllocFree
                        res = " TlsIndex = " + TBlk.UnionData1.ToString("x8")
                    Case BlockTypes.TrackGCStacks
                        res = " GC Reason=" + GetGCReason
                    Case BlockTypes.ClrJit
                        res = " JIT = " + GetJitInfo()
                    Case BlockTypes.ClrGCHnd
                        res = String.Format(" GCHandle = {0:x8} InitObjId = {1:x8}", GetGCHndInfoHandle, GetGCHndInfoInitialObjectId)
                    Case BlockTypes.GdiObjs
                        res = String.Format("GDIObjType = {0}", CType(TBlk.UnionData2, GdiObjType))
                End Select
                Return res
            End Function

            Public Function GetIndirectInfo() As String
                Dim str = String.Empty
                If TBlkBlockType = BlockTypes.IndirectInfo Then
                    Dim pblk As New ProcMemBlockByte
                    Dim typ = GetIndirectInfoType()
                    Select Case typ
                        Case IndirectInfoType.IIType_CreateFile,
                            IndirectInfoType.IIType_OpenFile,
                            IndirectInfoType.IIType_OpenSection,
                            IndirectInfoType.IIType_CreateSection
                            str = "Handle = " + TBlk.Address.ToInt32.ToString("x8") + " "
                        Case IndirectInfoType.IIType_MapSection

                            Dim dw = ReadProcessMemoryDWORDEx(CType(TBlk.UnionData1, IntPtr).MyAdd(4))
                            str += "SectionHandle = " + dw.ToString("x8") + " "
                    End Select
                    str += TBlk.Address.ToInt32.ToString("x8")
                    str += " " + TBlk.UnionData1.ToString("x8")
                    str += " Len= " + ReadProcessMemoryDWORDEx(CType(TBlk.UnionData1, IntPtr).MyAdd(IndirectInfoLenOffset)).ToString + " "
                    str += ReadProcessMemAsString(CType(TBlk.UnionData1, IntPtr).MyAdd(IndirectInfoBufferOffset))
                    'If ReadProcessMemoryByte(_hProcessTarget, CType(TBlk.UnionData1, IntPtr), pblk, 4, 0) <> 0 Then
                    '    Dim strlen = BitConverter.ToInt32(pblk.data, 0)
                    '    If ReadProcessMemoryByte(_hProcessTarget, CType(TBlk.UnionData1, IntPtr).Add(4), pblk, strlen, 0) <> 0 Then

                    '    End If

                    'End If
                End If
                Return str
            End Function

            Public Function GetMappedOrIndirectFileName() As String
                Dim str = String.Empty
                If TBlkBlockType = BlockTypes.IndirectInfo Then
                    Dim nLen = ReadProcessMemoryDWORDEx(CType(TBlk.UnionData1, IntPtr).MyAdd(IndirectInfoLenOffset))
                    If nLen > 0 Then
                        str += ReadProcessMemAsString(CType(TBlk.UnionData1, IntPtr).MyAdd(IndirectInfoBufferOffset), nMaxLen:=nLen)
                    End If
                    If str.StartsWith("\??\") Then
                        str = str.Substring(4)
                    End If
                End If
                If String.IsNullOrEmpty(str) Then
                    str = GetMappedFilenameFromAllocation(fDoVirtualQuery:=False)
                End If
                Return str
            End Function

            Public Function GetIndirectInfoType() As IndirectInfoType
                ' tblk.Address is the key: could be a handle or base addr depending on type
                Dim typ = IndirectInfoType.IIType_None
                If TBlkBlockType = BlockTypes.IndirectInfo Then
                    Dim tmp = ReadProcessMemoryDWORDEx(CType(TBlk.UnionData1, IntPtr))
                    typ = CType(tmp, IndirectInfoType)
                End If
                Return typ
            End Function

            Public Function GetMapRequestNumberOfBytes() As Integer
                Dim res = 0
                If TBlkBlockType = BlockTypes.MapFile Then
                    res = TBlk.UnionData2
                End If
                Return res
            End Function
            Public Function GetMapRequestDesiredAccess() As Integer
                Dim res = 0
                If TBlkBlockType = BlockTypes.MapFile Then
                    res = TBlk.UnionData1
                End If
                Return res
            End Function

            Public Shared Function CreateFrom(ByVal dt As Object) As HeapAllocationContainer
                Dim hctr As HeapAllocationContainer = Nothing
                If dt IsNot Nothing Then
                    hctr = TryCast(dt, HeapAllocationContainer)
                    If hctr Is Nothing Then
                        Dim tdesc = TypeDescriptor.GetProperties(dt)("_HeapAllocationContainer")
                        If tdesc IsNot Nothing Then
                            hctr = CType(tdesc.GetValue(dt), HeapAllocationContainer)
                        End If
                    End If
                End If
                Return hctr
            End Function

            Friend Shared __CodeMarkerSnap As List(Of HeapAllocationContainer)
            Friend Shared ReadOnly Property _CodeMarkerSnap As List(Of HeapAllocationContainer)
                Get
                    If __CodeMarkerSnap Is Nothing Then
                        __CodeMarkerSnap = GetCodeMarkersToMerge() ' could be empty list
                        If _ConnectionMode = MemSpectMode.OnLine Then
                            If Not _fDidSubscribeUnfrozenCodeMarkerSnap Then
                                _fDidSubscribeUnfrozenCodeMarkerSnap = True
                                AddHandler ProcComm.TargetUnfrozen,
                                    Sub()
                                        __CodeMarkerSnap = Nothing
                                    End Sub
                            End If
                        End If

                    End If
                    Return __CodeMarkerSnap
                End Get
            End Property

            Private Shared _fDidSubscribeUnfrozenCodeMarkerSnap As Boolean = False
            Private Shared _CodeMarkerComparer As New CodeMarkerSeqnoComparer
            Public ReadOnly Property NearestCodeMarker() As String
                Get
                    Dim strCodeMarker = String.Empty
                    Dim hctrCodeMarker = GetNearestCodeMarkerContainer(0)

                    If hctrCodeMarker IsNot Nothing Then
                        strCodeMarker = hctrCodeMarker.GetCodeMarkerName(fIncludeInstanceNum:=True) +
                                " " + hctrCodeMarker.AllocationStruct.SeqNo.ToString("n0")
                    End If
                    Return strCodeMarker
                End Get
            End Property

            Public ReadOnly Property KnownIssue As String ' can't use optional params due to memberbinding
                Get
                    Dim issue = String.Empty
                    Dim known = KnownIssues.GetKnownIssue(Me)
                    If known IsNot Nothing Then
                        issue = known.IssueName
                    End If
                    Return issue
                End Get
            End Property

            Public Function GetKnownIssue(Optional ByVal dictLeaks As Dictionary(Of IntPtr, HeapAllocationContainer) = Nothing) As String
                Dim issue = String.Empty
                Dim known = KnownIssues.GetKnownIssue(Me, dictLeaks)
                If known IsNot Nothing Then
                    issue = known.IssueName
                End If
                Return issue
            End Function

            Private Function GetNearestCodeMarkerContainer(ByRef ndx As Integer) As HeapAllocationContainer
                Dim hctrMarker As HeapAllocationContainer = Nothing
                If _CodeMarkerSnap IsNot Nothing Then
                    Dim findRes = FindNearest(Of HeapAllocationContainer)(_CodeMarkerSnap, Me, Nothing, Nothing, _CodeMarkerComparer)
                    If findRes(0) >= 0 Then
                        ndx = findRes(0)
                        hctrMarker = _CodeMarkerSnap.Item(ndx)

                    End If
                End If
                Return hctrMarker
            End Function

            Public Function GetCodeMarkerTree() As String
                Dim retval = String.Empty
                Dim ndx = 0
                Dim hctrNearestMarker = GetNearestCodeMarkerContainer(ndx)
                If hctrNearestMarker IsNot Nothing Then
                    Dim stackMarkers As New Stack(Of String)

                    Dim currentMarkerDat = hctrNearestMarker.GetCodeMarkerData()
                    stackMarkers.Push(currentMarkerDat.MarkerNameEx)
                    Dim curdepth = currentMarkerDat.MarkerDepth
                    Do While curdepth >= 0 AndAlso ndx > 0
                        ndx -= 1
                        currentMarkerDat = _CodeMarkerSnap(ndx).GetCodeMarkerData
                        If currentMarkerDat.MarkerDepth < curdepth Then ' we've popped a level
                            ' we want the first marker at the level we just left (down a level)
                            Dim cmrk = _CodeMarkerSnap(ndx + 1).GetCodeMarkerData
                            Dim txtLevel = cmrk.MarkerNameEx
                            stackMarkers.Push(txtLevel)
                            curdepth -= 1
                        End If
                    Loop
                    Dim sb As New StringBuilder
                    While stackMarkers.Count > 0
                        sb.AppendLine(stackMarkers.Pop)
                    End While
                    retval = sb.ToString
                End If
                Return retval
            End Function

            Public Class CodeMarkerSeqnoComparer
                Inherits Comparer(Of HeapAllocationContainer)

                Public Overrides Function Compare(ByVal x As HeapAllocationContainer, ByVal y As HeapAllocationContainer) As Integer
                    Return x.AllocationStruct.SeqNo.CompareTo(y.AllocationStruct.SeqNo)
                End Function
            End Class
            ''' <summary>
            ''' used for details display: maxlen GetSize or 500 for perf
            ''' </summary>
            Public Function GetStringContent(Optional ByVal fGetMemDumpToo As Boolean = False) As String
                Dim str = String.Empty
                Try
                    If SpyHeapPtr IsNot Nothing Then

                        If IsMemSpectHeap AndAlso Me.TBlkBlockType = BlockTypes.CodeMarker Then
                            str = GetBlockTypeName()
                        Else
                            If Common._ExpandStringContents Then
                                If TBlkBlockType = BlockTypes.None Then ' only for normal heaps: not for clr objs
                                    If IsGhostAlloc AndAlso Me.GhostAlloc.SeqNoWhenFreed > 0 Then
                                        str = "Ghost"
                                    Else
                                        Dim addrToUse As IntPtr
                                        Dim sizeToUse As Integer
                                        If SpyHeapPtr.IsArenaHeap Then
                                            If GetArenaBlockType = ArenaBlkType.Alloc Then
                                                addrToUse = GetArenaAllocAddr
                                                sizeToUse = GetArenaAllocSize
                                            Else
                                                sizeToUse = 0 ' no string content for arena header
                                            End If
                                        Else
                                            addrToUse = GetAddr
                                            sizeToUse = GetSize
                                        End If
                                        If sizeToUse > 0 Then
                                            str = ReadProcessMemAsString(addrToUse + SpyHeapPtr.nHeaderSize,
                                                                         nMaxLen:=Math.Min(sizeToUse, 500)
                                                                         )

                                            str = str.Replace(vbCr, String.Empty).Replace(vbLf, String.Empty)
                                            'the ^ is the not operator. It tells the regex to find everything that doesn't match, 
                                            '  instead of everything that does match. The \u####-\u#### says which characters match.\u0000-\u007F is the equivilent of the first 255 characters in utf-8 or unicode, which are always the ascii characters. So you match every non ascii character (because of the not) and do a replace on everything that matches
                                            str = System.Text.RegularExpressions.Regex.Replace(str, "[^\u0000-\u007F]", String.Empty)
                                            If fGetMemDumpToo Then
                                                If TBlkBlockType = BlockTypes.ClrObject Then
                                                    addrToUse = addrToUse.MyAdd(-4) ' show syncSblock too
                                                End If
                                                str += vbCrLf +
                                                    GetMemoryDump(addrToUse, Math.Min(sizeToUse, 500))
                                            End If
                                        End If
                                    End If
                                End If
                            End If
                        End If
                    End If

                Catch ex As Exception
                    Debug.Assert(False, ex.ToString)
                End Try
                Return str
            End Function

            Public Function GetMappedFilenameFromAllocation(Optional ByVal fDoVirtualQuery As Boolean = True) As String
                If TBlkBlockType = BlockTypes.CodeMarker Then ' merged markers
                    Return Me.GetBlockTypeName
                End If
                Dim mappedFile = String.Empty
                If Common._ConnectionMode = MemSpectMode.Offline Then
                    Common._offlineSnapshot.TryGetMappedFileForAllocation(Me.GetAddr, mappedFile)
                    If String.IsNullOrEmpty(mappedFile) Then
                        mappedFile = MiniDumpReader.Singleton.GetModuleName(Me.GetAddr)
                    End If
                    Return mappedFile
                End If
                If Not IsGhostAlloc Then
                    Dim sbFilename As New Text.StringBuilder(300)
                    Try
                        If GetMappedFileName(
                            _hProcessTarget,
                            Me.GetAddr,
                            sbFilename,
                            sbFilename.Capacity) > 0 Then ' returns # of bytes
                            'success
                            mappedFile = sbFilename.ToString
                        End If
                    Catch ex As Exception
                        Debug.Assert(False, "Ex GetMappedFilenameFromAllocation " + ex.ToString)
                    End Try

                    mappedFile = ConvertDeviceHardDiskVolume(mappedFile)
                    If fDoVirtualQuery Then
                        If String.IsNullOrEmpty(mappedFile) Then
                            Dim mbi = New MEMORY_BASIC_INFORMATION
                            VirtualQueryEx(_hProcessTarget, Me.GetAddr, mbi, CUInt(Marshal.SizeOf(mbi)))
                            mappedFile = GetFileNameFromMBI(mbi)
                        End If

                    End If
                End If
                Return mappedFile
            End Function

            ''' <summary>
            ''' gets members in offset order
            ''' </summary>
            ''' <returns></returns>
            ''' <remarks></remarks>
            Public Function GetClrClassMemberLayout(Optional fIncludeClassesWithNoMembers As Boolean = True,
                                                    Optional CallBack As Action(Of String, Boolean) = Nothing
                                                    ) As String
                Dim sbuilder = New Text.StringBuilder()

                Dim AddString = Sub(str As String, isBold As Boolean)
                                    If CallBack IsNot Nothing Then
                                        CallBack(str, isBold)
                                    Else
                                        sbuilder.Append(str)
                                    End If
                                End Sub

                Dim clsLayouts = ClrClassInfo.GetClassLayoutsFromClassId(Me.GetClassId)
                Dim orefDataLst As List(Of List(Of HeapAllocationContainer)) = Nothing
                If Me.TBlkBlockType = BlockTypes.ClrObject Then
                    orefDataLst = GetObjectRefData(NodeType.RefFromParent)
                End If
                If clsLayouts.Count > 0 Then
                    Dim layoutIndex = 0
                    Dim valuetypeOffset = 0
                    For Each clsLayout In clsLayouts
                        Dim baseName = ClrClassInfo.GetClassNameFromClassOrObjectId(clsLayout.classId)
                        If layoutIndex = 1 AndAlso baseName = "System.ValueType" Then
                            valuetypeOffset = IntPtr.Size
                        End If
                        layoutIndex += 1
                        Dim memberSize = 0
                        If fIncludeClassesWithNoMembers AndAlso clsLayout.dictFieldInfo.Count = 0 Then
                            AddString(vbCrLf + "     " + baseName + " (no members)", False)
                        End If
                        For i = 0 To clsLayout.dictFieldInfo.Count - 1
                            Dim boldValue = False
                            Dim offset = clsLayout.dictFieldInfo.Keys(i)
                            Dim entry = clsLayout.dictFieldInfo(offset)
                            Dim fType = entry.FldType
                            If i < clsLayout.dictFieldInfo.Count - 1 Then
                                memberSize = clsLayout.dictFieldInfo.Keys(i + 1) - offset
                            Else ' last one
                                memberSize = clsLayout.classSize - 4 - offset ' sizeof(clsid) + sizeof(flags)
                            End If
                            If fType.StartsWith("System.Nullable`1") Then
                                Dim fHasValue = False
                                Dim strNullable = String.Empty
                                Dim strBoolNullable = String.Empty '  bool is in same dword
                                For iNullableOffset = 0 To memberSize - 4 Step 4
                                    Dim offsetAddr = Me.GetAddr.MyAdd(offset + iNullableOffset)
                                    Dim dwValueLong As Long = ReadProcessMemoryDWORDEx(offsetAddr)

                                    Dim dwValue As Integer = Convert.ToInt32(dwValueLong)
                                    If iNullableOffset = 0 Then ' 1st one is "HasValue"
                                        If (dwValue And &HFF) <> 0 Then
                                            fHasValue = True
                                            If memberSize = 4 AndAlso fType = "System.Nullable`1<Boolean>" Then
                                                Dim boolVal = If((dwValue And &HFF00) > 0, True, False)
                                                strBoolNullable = String.Format(" Value = {0}", boolVal)
                                            End If
                                        End If
                                        dwValue = (dwValue And &HFF)
                                        strNullable = "HasValue"
                                    Else
                                        strNullable = "NullableMember(" + (iNullableOffset / 4).ToString + ")"
                                        If dwValue <> 0 Then
                                            'Dim strNullableMem = ClrClassInfo.GetClassNameFromClassOrObjectId(clsId:=IntPtr.Zero,
                                            '                                             ObjectId:=CType(dwValue, IntPtr),
                                            '                                             fExpandSystemStringOrArray:=True,
                                            '                                             fAllowInvalidData:=True)

                                            'strNullable += strNullableMem
                                        End If
                                    End If
                                    AddString(vbCrLf + String.Format("{0,4} {1}.{2}   {3} {4} 0x{5:x0}{6}",
                                                                           offset + iNullableOffset,
                                                                           baseName,
                                                                           entry.FldName,
                                                                           fType,
                                                                           strNullable,
                                                                           dwValue,
                                                                           strBoolNullable
                                                                           ), False)
                                    If Not fHasValue Then
                                        Exit For
                                    End If
                                Next
                            Else
                                If memberSize < 4 Then 'class padding
                                    Select Case fType.ToLower
                                        Case "boolean", "sbyte", "byte", "char"
                                            memberSize = 1
                                        Case "short", "ushort"
                                            memberSize = 2
                                    End Select
                                End If
                                If Me.TBlkBlockType = BlockTypes.ClrObject Then ' not a class, but an instance, so it has values
                                    Dim hexPrefix = "0x"
                                    Dim strValuefmt = String.Format(":x{0}", memberSize * 2) ' for 4, "x:8"
                                    Dim clsinfoMember = String.Empty
                                    Dim offsetAddr = Me.GetAddr.MyAdd(offset)
                                    Dim dwValueLong As Long = ReadProcessMemoryDWORDEx(offsetAddr + valuetypeOffset)

                                    Dim dwValue As Integer = Convert.ToInt32(dwValueLong)
                                    If memberSize < 4 Then ' don't show padding
                                        Select Case memberSize
                                            Case 1
                                                dwValue = Convert.ToInt32(dwValueLong And &HFF)
                                            Case 2
                                                dwValue = Convert.ToInt32(dwValueLong And &HFFFF)
                                        End Select
                                    Else
                                        If memberSize = 4 Then
                                            If dwValue <> 0 Then ' show refs
                                                If offset = 0 Then
                                                    ' this actually happens
                                                    fType += String.Format("offset {0} {1} {2}", offset, entry.FldName, entry.FldType)
                                                Else
                                                    ' we don't want to call ClrClassInfo.GetClassNameFromClassOrObjectId for arbitrary values 
                                                    '  because no way to tell if it's a valid classid, cause crash in target proc
                                                    Select Case fType
                                                        Case "Class" ', "String"
                                                            fType += " " + ClrClassInfo.GetClassNameFromClassOrObjectId(IntPtr.Zero, New IntPtr(dwValue), fExpandSystemStringOrArray:=True)
                                                        Case "String"
                                                            fType = ClrClassInfo.GetClassNameFromClassOrObjectId(IntPtr.Zero, New IntPtr(dwValue), fExpandSystemStringOrArray:=True)
                                                            If fType.Length > "System.String ".Length Then
                                                                boldValue = True
                                                            End If
                                                        Case "Object"
                                                            fType += " " + ClrClassInfo.GetClassNameFromClassOrObjectId(IntPtr.Zero, New IntPtr(dwValue), fExpandSystemStringOrArray:=True)
                                                        Case Else
                                                            If fType.StartsWith("System.EventHandler") Then
                                                                fType += " " + ClrClassInfo.GetClassNameFromClassOrObjectId(IntPtr.Zero, New IntPtr(dwValue), fExpandSystemStringOrArray:=True)
                                                            End If
                                                    End Select
                                                End If
                                                If orefDataLst.Count = 1 Then
                                                    Dim mems = orefDataLst(0).Where(Function(h) h.GetAddr.ToInt32 = dwValue)
                                                    If mems.Count = 1 Then
                                                        clsinfoMember = mems(0).GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True)
                                                        If clsinfoMember = fType Then
                                                            clsinfoMember = String.Empty
                                                        Else
                                                            'clsinfoMember = String.Format("({0})({1})", clsinfoMember, fType)
                                                            fType = String.Format("{0}({1})", clsinfoMember, fType)
                                                            clsinfoMember = String.Empty
                                                        End If
                                                    End If
                                                End If
                                            End If
                                        Else ' > 4
                                            Dim nSize = memberSize - 4
                                            Do While nSize >= 0
                                                clsinfoMember += String.Format(" 0x{0:x8}", dwValue)
                                                offsetAddr = offsetAddr.MyAdd(4)
                                                dwValueLong = ReadProcessMemoryDWORDEx(offsetAddr)
                                                dwValue = Convert.ToInt32(dwValueLong)
                                                nSize -= 4
                                            Loop
                                            AddString(vbCrLf + String.Format("{0,4} {1}.{2}   {3} {4}",
                                                                                   offset,
                                                                                   baseName,
                                                                                   entry.FldName,
                                                                                   fType,
                                                                                   clsinfoMember
                                                                                   ), False)
                                        End If
                                    End If
                                    If memberSize <= 4 Then
                                        If boldValue Then
                                            AddString(vbCrLf + String.Format("{0,4} {1}.{2}   System.String ",
                                                                               offset,
                                                                               baseName,
                                                                               entry.FldName
                                                                               ), False)
                                            AddString(fType.Substring("System.String ".Length), True)

                                            AddString(String.Format(" {0}{1" + strValuefmt + "}{2}",
                                                                               hexPrefix,
                                                                               dwValue,
                                                                               clsinfoMember
                                                                               ), False)
                                        Else
                                            AddString(vbCrLf + String.Format("{0,4} {1}.{2}   {3} {4}{5" + strValuefmt + "}{6}",
                                                                               offset,
                                                                               baseName,
                                                                               entry.FldName,
                                                                               fType,
                                                                               hexPrefix,
                                                                               dwValue,
                                                                               clsinfoMember
                                                                               ), False)
                                        End If

                                    End If
                                Else
                                    'class
                                    AddString(vbCrLf + String.Format("{0,4} {1}.{2}   {3}",
                                                                           offset,
                                                                           baseName,
                                                                           entry.FldName,
                                                                           fType
                                                                           ), False)

                                End If



                            End If
                        Next
                    Next
                End If

                Return sbuilder.ToString
            End Function

            ''' <summary>
            ''' from tblk.clrobj, get class instance values into dict Key=ObjectId, value =list of membernames that ref the list
            ''' multiple members can ref same objid
            ''' </summary>
            Public Function GetClrClassMembersToDict() As Dictionary(Of IntPtr, List(Of String))
                Dim dictResult = New Dictionary(Of IntPtr, List(Of String))
                Dim clsLayouts = ClrClassInfo.GetClassLayoutsFromClassId(Me.GetClassId)
                Dim layoutIndex = 0
                Dim valueTypeoffset = 0
                For Each clsLayout In clsLayouts
                    If layoutIndex = 1 AndAlso clsLayout.className = "System.ValueType" Then
                        valueTypeoffset = IntPtr.Size
                    End If
                    layoutIndex += 1
                    For Each entry In clsLayout.dictFieldInfo

                        Dim offsetAddr = Me.GetAddr.MyAdd(entry.Key + valueTypeoffset)
                        Dim dwValue = ReadProcessMemoryDWORDEx(offsetAddr)
                        Dim fType = entry.Value.FldType
                        If dwValue <> 0 Then
                            Dim ObjId = New IntPtr(dwValue)
                            If Not dictResult.ContainsKey(ObjId) Then
                                dictResult.Add(ObjId, New List(Of String) From {entry.Value.FldName})
                            Else
                                dictResult(ObjId).Add(entry.Value.FldName)
                            End If
                        End If
                    Next
                Next
                Return dictResult
            End Function


            Public Function Address() As ULong Implements IMemoryBlock.Address
                Return GetAddr.MyToULong
            End Function

            Public Function Size() As ULong Implements IMemoryBlock.Size
                Return CULng(GetSize)
            End Function

            Sub WriteHeapAllocationStructMemory()
                Debug.Assert(_ConnectionMode = MemSpectMode.OnLine OrElse _ConnectionMode = MemSpectMode.Existing, "Writemem requires online")
                Dim startaddr = Me.GetAddr
                Dim len = Me.GetSize
                If Me.TBlkBlockType = BlockTypes.VirtualAlloc Then
                    Dim mbi = GetMBIForAddress(startaddr)
                    If (mbi.State And AllocationState.MEM_COMMIT) = 0 Then
                        Throw New InvalidOperationException("Can't write to non-committed memory")
                    End If
                End If
                If Me.TBlkBlockType = BlockTypes.ClrObject Then
                    startaddr = startaddr.MyAdd(IntPtr.Size) ' skip classid
                    len -= IntPtr.Size
                    Dim clname = GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False)
                    If clname = "System.String" Then
                        startaddr = startaddr.MyAdd(IntPtr.Size) ' skip strlen
                        len -= IntPtr.Size
                    End If
                End If
                Dim bEncode As New Text.UnicodeEncoding
                Dim nOffset = 0
                Dim bWritten = 0
                Dim nIndex = 0
                Dim buflen = len ' save orig len
                len -= 2  ' save room for final 2 zero bytes
                While len > 0
                    Dim bArray = bEncode.GetBytes(String.Format("MemSpect{0,4}", nIndex))
                    WriteProcessMemory(_hProcessTarget, startaddr.MyAdd(nOffset), bArray, Math.Min(bArray.Length, len), bWritten)
                    len -= bWritten
                    nOffset += bWritten
                    nIndex += 1
                End While
                If buflen > 0 Then
                    ' write final 2 zero bytes
                    WriteProcessMemory(_hProcessTarget, startaddr.MyAdd(nOffset), {0, 0}, Math.Min(buflen, 2), bWritten)
                End If
            End Sub

            Public Function GetGCRootPaths(Optional ByVal delOut As Action(Of String) = Nothing) As List(Of List(Of HeapAllocationContainer))
                Dim retlist As New List(Of List(Of HeapAllocationContainer))
#If True Then

                Dim lamShowInfo = Sub(str As String)
                                      If delOut IsNot Nothing Then
                                          'delOut.Invoke(str)
                                      End If
                                  End Sub
#End If
                ' now find paths from gcroot to obj
                Dim graphEx = New Dictionary(Of IntPtr, ClrObjDumpEx)
                For Each entry In GCData.GetCLRObjectRefDict
                    graphEx(entry.Key) = New ClrObjDumpEx With {.clrObjDat = entry.Value}
                Next
                Dim objQueue As New Queue(Of ClrObjDumpEx)
                For Each gcrt In GCData.GetGCRootInfo(fIncludeDupes:=False)
                    If (gcrt.GetGCRootFlagsAsInt And (GCRootFlags.Pinning Or GCRootFlags.WeakRef)) = 0 Then
                        Dim entry = graphEx(gcrt.AllocationStruct.Address)
                        entry.clrObjDat.hctnr = gcrt
                        If Not entry.fQueuedAlready Then
                            entry.fQueuedAlready = True
                            objQueue.Enqueue(entry)
                        End If
                    End If
                Next
                Dim numGCRootsLeft = objQueue.Count
                Dim TargetId = Me.AllocationStruct.Address
                Dim nVisited = 0
                Dim nMaxGCRootPathLenSoFar = 100000
                Do While objQueue.Count > 0
                    nVisited += 1
                    Dim curQueuedObj = objQueue.Dequeue
                    'lamAssert.Invoke(curQueuedObj, "Dequeue")
                    Dim curId = curQueuedObj.clrObjDat.hctnr.AllocationStruct.Address
                    If curId <> TargetId Then
                        Dim curQueuedObjInGraph = graphEx(curId)
                        If Not curQueuedObjInGraph.fQueuedChildrenAlready Then
                            curQueuedObjInGraph.fQueuedChildrenAlready = True
                            For Each objptr In From oref In curQueuedObj.clrObjDat.Refs Distinct
                                Dim childFromGraph = graphEx(objptr)
                                If Not childFromGraph.fQueuedChildrenAlready Then 'AndAlso child.ParentObj Is Nothing Then
                                    If True Then ' GCData.GetGCRootInfo(childFromGraph.clrObjDat.hctnr.AllocationStruct.Address) Is Nothing Then   ' don't add gcroots
                                        Dim childCopy = childFromGraph.CloneIt
                                        Dim fAddit = True
                                        If curQueuedObj.PathToRoot IsNot Nothing Then
                                            If curQueuedObj.PathToRoot.Count > CInt(1.2 * nMaxGCRootPathLenSoFar) Then ' if it's 20% longer than found so far, then don't bother
                                                fAddit = False
                                            Else
                                                childCopy.PathToRoot.AddRange(curQueuedObj.PathToRoot)
                                            End If
                                        End If
                                        If fAddit Then
                                            childCopy.PathToRoot.Add(curQueuedObj.clrObjDat.hctnr)
                                            'lamAssert.Invoke(childFromGraph, "cloned")
                                            childCopy.nLevel = curQueuedObj.nLevel + 1
#If DEBUG Then
                                            lamShowInfo.Invoke("   Eq " + childCopy.ToString)
#End If
                                            ' child.fBeenHereAlready = True
                                            objQueue.Enqueue(childCopy)

                                        End If
                                    End If
                                End If
                            Next
                        End If
                    Else 'bingo
                        Dim lstGCRootPath = New List(Of HeapAllocationContainer)
                        If curQueuedObj.PathToRoot IsNot Nothing Then
                            lstGCRootPath.AddRange(curQueuedObj.PathToRoot)
                        End If
                        lstGCRootPath.Add(curQueuedObj.clrObjDat.hctnr)
                        If lstGCRootPath.Count < nMaxGCRootPathLenSoFar Then
                            nMaxGCRootPathLenSoFar = lstGCRootPath.Count
                        End If
#If DEBUG Then
                        lamShowInfo.Invoke("!!! " + lstGCRootPath.Count.ToString)
#End If
                        retlist.Add(lstGCRootPath)
                    End If
                    If numGCRootsLeft > 0 Then
                        numGCRootsLeft -= 1
                    End If
                Loop
                lamShowInfo.Invoke("Visited " + nVisited.ToString + " nodes.")
                Return retlist
            End Function


            Public Class ObjRefGraph
                Public objdata As ClrObjDump ' An obj and the things it refs
                Public fHaveVisited As Boolean
                Public fIsInGraph As Boolean
                Public nLevel As Integer
                Public Overrides Function ToString() As String
                    Return String.Format("{0} {1} {2}", If(fHaveVisited, "Vis", "") + If(fIsInGraph, "!", ""), nLevel, objdata.hctnr.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False))
                End Function
            End Class

            Public Shared _nNodesVisited As Integer ' used by tests

            Public Class ObjRefGraphCollection
                Inherits System.Collections.ObjectModel.KeyedCollection(Of IntPtr, ObjRefGraph)

                Protected Overrides Function GetKeyForItem(ByVal item As ObjRefGraph) As System.IntPtr
                    Return item.objdata.hctnr.AllocationStruct.Address
                End Function
            End Class

            Public Function GetObjectsOnlyRefdByThis() As ObjRefGraphCollection
                Dim clrObjRefDict = GCData.GetCLRObjectRefDict() 'Get the entire CLR Obj graph: Dict<Object, objs reffed by it>
                ' We want to "pick up" the part of the object graph starting from the selected node (Root)
                '   and hold it high, dangling (IsInGraph) those objects that can only be reached from the Root.
                ' Alg: we want to set all nodes IsInGraph=true that 
                '    have all paths to GCRoot include a node with IsInGraph
                '     So for each node, enumerate it's PathToGCRoot and travel them til we get to a IsInGraph or end

                '1st we make a writeable copy
                Dim _ObjGraph = New ObjRefGraphCollection
                For Each ref In clrObjRefDict.Values ' for each Pair<Obj,ItsRefs>
                    _ObjGraph.Add(New ObjRefGraph With {.objdata = ref})
                Next
                _nNodesVisited = 0
                ' then we add the root item to a queue
                Dim objgraphRootEntry = _ObjGraph(Me.AllocationStruct.Address)
                objgraphRootEntry.fIsInGraph = True ' indicate the root is included: lets propagate this flag to "dangle" the refed objs
                Dim Queue = New Queue(Of ObjRefGraph)
                Queue.Enqueue(objgraphRootEntry)
                While Queue.Count > 0
                    Dim node = Queue.Dequeue
                    _nNodesVisited += 1
                    If Not node.fHaveVisited Then ' can happen if a parent refs the child twice
                        node.fHaveVisited = True
                        ' now get all paths to GCRoot. If we get to IsInGraph
                        If Not node.fIsInGraph Then
                            Dim gcRootsForThisNode = node.objdata.hctnr.GetGCRootPaths()
                            Dim fCandidateInclude = True
                            For Each gcRootPath In gcRootsForThisNode
                                Dim fThisPathReachedInclude = False
                                Dim pathLen = gcRootPath.Count
                                For i = 2 To pathLen ' the 1st is the node itself, which we know isn't in the graph (yet), so we start a the 2nd
                                    Dim obj = gcRootPath(pathLen - i) ' have to go backwards from the obj to the gcroot
                                    If _ObjGraph(obj.AllocationStruct.Address).fIsInGraph Then
                                        fThisPathReachedInclude = True
                                        Exit For
                                    End If
                                Next
                                If Not fThisPathReachedInclude Then ' some path reached a GCRoot without reaching IsInGraph
                                    fCandidateInclude = False
                                    Exit For
                                End If
                            Next
                            If fCandidateInclude Then ' if it made it so far it must be included
                                node.fIsInGraph = True
                            End If
                        End If
                        If node.fIsInGraph Then ' only add child nodes for those included
                            ' now get the children that haven't been visited
                            Dim qRef = From item In node.objdata.Refs
                                       Let o = _ObjGraph(item)
                                       Where Not o.fHaveVisited
                                       Select o
                            'now we queue the childen
                            For Each itm In qRef
                                itm.nLevel = node.nLevel + 1
                                Queue.Enqueue(itm)
                            Next
                        End If
                    End If
                End While

                Return _ObjGraph
            End Function

            Public Enum GetDisplayDataEnum
                DisplayShort
                DisplayShortWithStringOrArrayExpansion ' expand strings
                DisplayToolTip ' expand strings and show layout
            End Enum

            Private Shared _IsInGetDisplayData As Boolean = False
            Public Function GetDisplayData(Optional ByVal nMode As GetDisplayDataEnum = GetDisplayDataEnum.DisplayShort,
                                           Optional CallBack As Action(Of String, Boolean) = Nothing
                                           ) As String

                Dim sbuilder = New Text.StringBuilder()
                If Not _IsInGetDisplayData Then ' prevent recursion (while debugging, ToString)
                    Try
                        _IsInGetDisplayData = True

                        Dim AddString = Sub(str As String, isBold As Boolean)
                                            If CallBack IsNot Nothing Then
                                                CallBack(str, isBold)
                                            Else
                                                sbuilder.Append(str)
                                            End If
                                        End Sub
                        If IsGhostAlloc Then
                            AddString("Ghost ", False)
                        End If
                        Select Case TBlkBlockType
                            Case BlockTypes.None
                                AddString(GetStringContent(), False)
                            Case BlockTypes.MapFile
                                Dim tmp = GetMappedFilenameFromAllocation()
                                If Not String.IsNullOrEmpty(tmp) Then
                                    AddString("FileName = " + tmp, False)
                                End If
                                AddString(String.Format("#bytes Req={0:n0} DesiredAcces={1:x8}", GetMapRequestNumberOfBytes, GetMapRequestDesiredAccess), False)
                            Case BlockTypes.VirtualAlloc
                                AddString("ReqAddress = " + TBlk.UnionData1.ToString("x8") +
                                    "  MemType = " + CType(TBlk.UnionData2, AllocationState).ToString, False)
                                '                    strExtraInfo = "  Protect = " + tblk.UnionData3.ToString("x8")
                            Case BlockTypes.HeapCreate
                                AddString("HeapHandle = " + TBlk.UnionData1.ToString("x8") + " " + SpyHeapPtr.HeapName, False)
                            Case BlockTypes.HeapAlloc
                                AddString("HeapHandle = " + TBlk.UnionData1.ToString("x8"), False)
                            Case BlockTypes.ClrAppDomain, BlockTypes.ClrAssembly, BlockTypes.ClrModule
                                If _ConnectionMode = MemSpectMode.OnLine Then
                                    SendMsg(ProcMsgVerb.GetClrData, fSendEndMsgSync:=True, dwords:={HeapBlockPtr.ToInt32, 1})
                                    AddString(vbCrLf + ReadSharedMemAsString(ClassNameOffset, fAnsi:=False), False)
                                Else
                                    If _ConnectionMode = MemSpectMode.Offline Then
                                        If _offlineSnapshot._clrLoadDictionary.ContainsKey(AllocationStruct.Address) Then
                                            AddString(_offlineSnapshot._clrLoadDictionary(AllocationStruct.Address), False)
                                        Else
                                            AddString("Failed to find data in snapshot.", False)
                                        End If
                                    End If
                                End If
                            Case BlockTypes.ClrClass
                                If _ConnectionMode = MemSpectMode.OnLine Then
                                    SendMsg(ProcMsgVerb.GetClrData, fSendEndMsgSync:=True, dwords:={HeapBlockPtr.ToInt32, 0})
                                    Dim nInst = Marshal.ReadInt32(_SharedMemAddr, 0)
                                    Dim nColl = Marshal.ReadInt32(_SharedMemAddr, 4)
                                    AddString(String.Format("# instances = {0:n0} # collected = {1:n0}", nInst, nColl) +
                                            vbCrLf + ReadSharedMemAsString(ClassNameOffset, fAnsi:=False), False)
                                Else
                                    AddString(GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=(nMode <> GetDisplayDataEnum.DisplayShort)), False)
                                End If
                                If nMode <> GetDisplayDataEnum.DisplayShort Then
                                    If Not IsGhostAlloc Then
                                        AddString(GetClrClassMemberLayout(fIncludeClassesWithNoMembers:=True), False)
                                    End If
                                End If
                            Case BlockTypes.ClrObject
                                Dim clsName = GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=(nMode <> GetDisplayDataEnum.DisplayShort))
                                If nMode = GetDisplayDataEnum.DisplayShort Then
                                    AddString(clsName, False)
                                Else
                                    'If ReadProcessMemoryDword(_hProc, CType(Address, IntPtr), RealAddress, 4, Nothing) = 0 Then
                                    '   '    UpdateStatusMsg("readprocmem failed " + Address.ToString("x8"))
                                    'End If
                                    If nMode = GetDisplayDataEnum.DisplayToolTip Then
                                        If Not clsName.StartsWith("System.String") Then
                                            AddString(" ClassId = 0x" + TBlk.UnionData1.ToString("x8") + " " + clsName, False)
                                        End If
                                        Dim MovedCnt = GetMovedCnt
                                        Dim SurvivedCnt = GetSurvivedCnt
                                        Dim gen = GetGen
                                        AddString(
                                                " GCGen=" + gen.ToString +
                                                " GCMoved=" + MovedCnt.ToString() +
                                                " GCSurvived=" + SurvivedCnt.ToString(), False)
                                        Dim gcrootExtra = GetGCRootExtraInfo()
                                        If String.IsNullOrEmpty(gcrootExtra) Then
                                            Dim gcrts As List(Of HeapAllocationContainer) = Nothing
                                            If GCData._GCRootDict.TryGetValue(Me.AllocationStruct.Address, gcrts) Then
                                                gcrootExtra = gcrts(0).GetGCRootExtraInfo()
                                                AddString(" " + gcrootExtra, False)
                                            End If
                                        End If
                                        If Not IsGhostAlloc AndAlso Not clsName.StartsWith("System.String") Then
                                            AddString(GetClrClassMemberLayout(CallBack:=CallBack), False)
                                        End If
                                    Else
                                        AddString(clsName, False)
                                    End If
                                End If
                            Case BlockTypes.CodeMarker, BlockTypes.ThreadCreate, BlockTypes.TlsAllocFree, BlockTypes.TrackGCStacks, BlockTypes.ClrJit, BlockTypes.ClrGCHnd, BlockTypes.GdiObjs
                                AddString(GetBlockTypeName(), False)
                            Case BlockTypes.IndirectInfo, BlockTypes.MapFile
                                Dim indType = GetIndirectInfoType()
                                AddString(String.Format("IType={0} File = {1}",
                                                             If(indType.ToString.Length < 7, indType.ToString,
                                                               indType.ToString.Substring(7)),
                                                              GetMappedOrIndirectFileName), False)
                                If indType = IndirectInfoType.IIType_FileLoad OrElse indType = IndirectInfoType.IIType_FileUnload Then
                                    AddString(" " + GetExtraDisplayData(), False)
                                End If
                            Case BlockTypes.ClrExcpt
                                AddString(String.Format("Exception ObjId= {0:x8} ClsId={1:x8} {2}",
                                                              GetExcptObjId.ToInt32,
                                                              GetExcptClassId.ToInt32,
                                                              GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False)), False)
                            Case BlockTypes.LoadResource
                                Dim mbi = GetMBIForAddress(New IntPtr(Me.TBlk.UnionData1))
                                AddString(String.Format("{0}", GetFileNameFromMBI(mbi)), False)
                        End Select

                    Finally
                        _IsInGetDisplayData = False
                    End Try
                End If
                Return sbuilder.ToString
            End Function

            Public Function GetExtraDisplayData() As String
                Dim res = String.Empty
                Select Case TBlkBlockType
                    Case BlockTypes.IndirectInfo
                        Select Case GetIndirectInfoType()
                            Case IndirectInfoType.IIType_FileLoad, IndirectInfoType.IIType_FileUnload
                                Dim tmp = ReadProcessMemoryDWORDEx(CType(TBlk.UnionData1, IntPtr).MyAdd(4))
                                res = String.Format("{0:x8}", tmp)
                        End Select
                End Select
                Return res
            End Function

        End Class ' HeapAllocationContainer

        Public Class HeapAllocationContainerComparer
            Implements IEqualityComparer(Of HeapAllocationContainer)

            Public Function Equals1(ByVal x As HeapAllocationContainer, ByVal y As HeapAllocationContainer) As Boolean Implements System.Collections.Generic.IEqualityComparer(Of HeapAllocationContainer).Equals
                If x.AllocationStruct.Address = y.AllocationStruct.Address Then
                    Return True
                End If
                Return False

            End Function

            Public Function GetHashCode1(ByVal obj As HeapAllocationContainer) As Integer Implements System.Collections.Generic.IEqualityComparer(Of HeapAllocationContainer).GetHashCode
                Return obj.AllocationStruct.Address.ToInt32
            End Function
        End Class

        ''' <summary>
        ''' for the specified value, find all entries in the srcList that contain the value. Handy for finding native object references
        ''' </summary>
        Public Function GetReferences(addrTarget As IntPtr, srclist As List(Of HeapAllocationContainer)) As List(Of HeapAllocationContainer)
            Dim resultReferences = New List(Of HeapAllocationContainer)
            Dim addrToFind = addrTarget.ToInt32
            Dim targFirstByte = addrToFind And 255
            Dim blk As New ProcMemBlockByte
            For Each alloc In srclist
                If alloc.TBlk.BlockType <> BlockTypes.VirtualAlloc Or (CType(alloc.TBlk.UnionData2, AllocationState) And AllocationState.MEM_COMMIT) = AllocationState.MEM_COMMIT Then
                    Dim blklen = alloc.GetSize
                    If _ConnectionMode = MemSpectMode.Offline Then
                        blk.data = MiniDumpReader.Singleton.ReadMemoryDictionary(alloc.GetAddr, blklen) ' at least maxpath
                        If blk.data Is Nothing Then
                            Continue For
                        End If
                    Else
                        ReDim blk.data(blklen)
                        Dim dwBytesRead = ReadProcessMemoryAsByteArray(_hProcessTarget,
                                                        alloc.GetAddr,
                                                        blklen,
                                                        1,
                                                        blk.data)

                    End If
                    If blk.data.Length > IntPtr.Size Then
                        For i = 0 To blklen - 1 - IntPtr.Size
                            If blk.data(i) = targFirstByte Then
                                Dim val = BitConverter.ToInt32(blk.data, i)
                                If val = addrToFind Then
                                    resultReferences.Add(alloc)
                                    Exit For
                                End If
                            End If
                        Next
                    End If
                End If
            Next
            Return resultReferences
        End Function


        <Flags()>
        Public Enum NodeType
            RootObject
            RefFromParent = &H1
            RefToParent = &H2
            PathFromGCRoot = &H4
        End Enum

        Friend Class ClrObjDumpEx
            Friend clrObjDat As ClrObjDump
            Friend nLevel As Integer
            Friend fQueuedAlready As Boolean
            Friend fQueuedChildrenAlready As Boolean
            Friend PathToRoot As List(Of HeapAllocationContainer)

            Friend Function CloneIt() As ClrObjDumpEx
                Dim copy = CType(Me.MemberwiseClone, ClrObjDumpEx)
                copy.PathToRoot = New List(Of HeapAllocationContainer)
                Return copy
            End Function

            Public Overrides Function ToString() As String
                Dim pq = String.Empty
                If PathToRoot IsNot Nothing Then
                    For i = 0 To PathToRoot.Count - 1
                        'pq += String.Format("{0} {1:x8} ", PathToRoot(i).GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False), PathToRoot(i).GetAddr.ToInt32)
                        pq += String.Format("{0:x8} ", PathToRoot(i).GetAddr.ToInt32)
                    Next
                End If
                Return String.Format("{0} {1:x8} Been={2} Lev={3} Qalr={4} ID={5:x8} Par={6}",
                                     clrObjDat.hctnr.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False),
                                     clrObjDat.hctnr.GetAddr.ToInt32,
                                     fQueuedChildrenAlready,
                                     nLevel,
                                     fQueuedAlready,
                                     clrObjDat.hctnr.AllocationStruct.Address.ToInt32,
                                     pq
                                     )
            End Function
        End Class


        Public Const strHardDiskVolume = "\device\harddiskvolume"

        '\Device\HarddiskVolume3\Windows\System32\locale.nls
        Public Function ConvertDeviceHardDiskVolume(ByVal strFileName As String) As String
            If strFileName.StartsWith("\??\") Then
                strFileName = strFileName.Substring(4)
            End If
            Dim nVolNdx = strFileName.IndexOf(strHardDiskVolume, StringComparison.OrdinalIgnoreCase)
            If nVolNdx >= 0 Then
                Dim tryname = strFileName.Substring(nVolNdx + strHardDiskVolume.Length + 1)
                For Each letr In {"C:", "D:", "E:"}
                    If IO.File.Exists(letr + tryname) Then
                        strFileName = letr + tryname
                        Exit For
                    End If
                Next
            End If
            Return strFileName

        End Function

        Public Function GetMBIForAddress(
                                               ByVal targAddr As IntPtr,
                                              Optional ByVal virtualallocs As SortedList(Of IntPtr, MEMORY_BASIC_INFORMATION) = Nothing
                                               ) As MEMORY_BASIC_INFORMATION
            If virtualallocs Is Nothing Then
                virtualallocs = GetVirtAllocs()
            End If
            Dim mbi As New MEMORY_BASIC_INFORMATION
            Dim findres = FindNearest(Of IntPtr)(virtualallocs.Keys, targAddr, Nothing, Nothing, _CompareIntPtr)

            If findres(0) >= 0 AndAlso findres(1) >= 0 Then ' bingo
                mbi = virtualallocs.Values(findres(0))

            End If
            'Dim res = From avirta In virtualallocs.Values
            '          Where avirta.BaseAddress.ToInt64 <= targAddr AndAlso
            '            targAddr < avirta.BaseAddress.ToInt64 + avirta.RegionSize

            'If res.Count = 1 Then
            '    mbi = res.First
            'End If
            Return mbi
        End Function

        Public _VirtualAllocOrFileCalls As List(Of HeapAllocationContainer) ' the calls to VA so we can show callstacks in tips
        Public _FileLoadNotifications As Dictionary(Of IntPtr, HeapAllocationContainer)
        Public Function GetFileNameFromMBI(ByVal mbi As MEMORY_BASIC_INFORMATION) As String
            Dim resultname = String.Empty
            If mbi.BaseAddress.ToInt32 = &H5CB0000 Then
                resultname = ""
            End If
            Try
                Select Case mbi.lType
                    Case AllocationType.MEM_IMAGE
                        If mbi.AllocationBase <> IntPtr.Zero Then
                            If _ConnectionMode = MemSpectMode.Offline OrElse _ConnectionMode = MemSpectMode.MiniDumpOnly Then
                                resultname = MiniDumpReader.Singleton.GetModuleName(mbi.AllocationBase)
                            Else
                                Dim sbFilename As New Text.StringBuilder(300)
                                ' should be AllocationBase: the base of the VirtualAlloc for this file
                                If GetModuleFileNameEx(_hProcessTarget, mbi.AllocationBase, sbFilename, sbFilename.Capacity) > 0 Then
                                    resultname = sbFilename.ToString
                                Else
                                    Dim err = Marshal.GetLastWin32Error
                                    If _FileLoadNotifications IsNot Nothing AndAlso _FileLoadNotifications.ContainsKey(mbi.AllocationBase) Then
                                        resultname = _FileLoadNotifications(mbi.AllocationBase).GetMappedOrIndirectFileName
                                    Else
                                        'If err <> 0 Then
                                        '    resultname = "GetLastError: " + err.ToString + " " + GetErrorMessageFromWin32LastError(err) + " "
                                        'End If
                                    End If
                                End If
                            End If
                        End If
                    Case AllocationType.MEM_MAPPED
                        If mbi.AllocationBase <> IntPtr.Zero Then
                            If _ConnectionMode = MemSpectMode.Offline Then
                                ' for offline, there are some mapped files that occurred before Memspect started in terget process
                                '  that didn't get serialized
                                Common._offlineSnapshot.TryGetMappedFileForAllocation(mbi.AllocationBase, resultname)
                            ElseIf _ConnectionMode = MemSpectMode.OnLine Then
                                Dim sbFilename As New Text.StringBuilder(300)
                                Try
                                    If GetMappedFileName(
                                       _hProcessTarget,
                                        mbi.AllocationBase,
                                        sbFilename,
                                        sbFilename.Capacity) > 0 Then

                                        resultname = sbFilename.ToString
                                    Else
                                        'Dim err = Marshal.GetLastWin32Error
                                        'If err <> 0 Then
                                        '    resultname = "GetLastError: " + err.ToString
                                        'End If
                                    End If
                                Catch ex As Exception

                                End Try
                            End If
                        End If
                    Case Else

                End Select
                If String.IsNullOrEmpty(resultname) Then
                    If _VirtualAllocOrFileCalls IsNot Nothing Then
                        For Each alloc In _VirtualAllocOrFileCalls
                            If alloc.TBlkBlockType = BlockTypes.MapFile OrElse alloc.TBlkBlockType = BlockTypes.IndirectInfo Then
                                If alloc.GetAddr = mbi.BaseAddress OrElse alloc.GetAddr = mbi.AllocationBase Then
                                    If alloc.TBlkBlockType = BlockTypes.IndirectInfo Then
                                        resultname = alloc.GetMappedOrIndirectFileName
                                        Exit For
                                    ElseIf alloc.TBlkBlockType = BlockTypes.MapFile Then
                                        resultname = alloc.GetMappedFilenameFromAllocation(fDoVirtualQuery:=False) ' avoid recursion
                                    End If
                                End If
                            End If
                        Next
                    End If
                End If
                resultname = ConvertDeviceHardDiskVolume(resultname)

            Catch ex As Exception
                Common.MemSpectExceptionHandler(ex)
            End Try
            Return resultname
        End Function

        ''' <summary>
        ''' Frees call stack memory tracked for a heap (or all heaps)
        ''' </summary>
        ''' <param name="hp">for a particular heap. Nothing means do all heaps. Freezes & unfreezes target, if not already frozen. </param>
        ''' <returns>returns # of stack frames freed</returns>
        ''' <remarks></remarks>
        Public Function FreeStackMemory(ByVal hp As CSpyHeap) As Integer
            Dim nFreed = 0
            If _ConnectionMode = MemSpectMode.OnLine Then
                Dim fDidFreeze = False
                If Not ProcComm._isTargetFrozen Then
                    ProcComm.FreezeTarget()
                    fDidFreeze = True
                End If
                Dim res = SendMsg(ProcMsgVerb.FreeStackMem, fSendEndMsgSync:=True, dwords:={If(hp Is Nothing, 0, hp.HeapHandleSpy.ToInt32)})
                nFreed += BitConverter.ToInt32(res, 1) '# frames freed
                If fDidFreeze Then
                    ProcComm.UnFreezeTarget()
                End If
            End If
            Return nFreed
        End Function

        Public Enum TrackingModeEnum
            Minimal
            Normal
        End Enum

        Public Event TrackingModeChanged(ByVal NewMode As TrackingModeEnum)

        Public Function SetTrackingMode(ByVal newmode As TrackingModeEnum) As TrackingModeEnum
            Dim res = SendMsg(ProcMsgVerb.TrackingMode, fSendEndMsgSync:=True, dwords:={newmode}) ' returns VerbDone + DWORD
            RaiseEvent TrackingModeChanged(newmode)
            Dim mode = CType(BitConverter.ToInt32(res, 1), TrackingModeEnum)
            Return mode
        End Function

        Public Function GetTrackingMode(Optional ByVal fFromInitialSettings As Boolean = False) As TrackingModeEnum
            Dim mode = TrackingModeEnum.Normal
            If fFromInitialSettings Then
                Dim initTrackingMode = GetPrivateProfileInt(ProfileStringSection, "TrackingMode", 0, _iniFileName)
                Return CType(initTrackingMode, TrackingModeEnum)
            End If
            Dim res = SendMsg(ProcMsgVerb.TrackingMode, fSendEndMsgSync:=True, dwords:={-1}) ' returns VerbDone + DWORD
            If res Is Nothing Then ' when disconnected
                mode = GetTrackingMode(fFromInitialSettings:=True) ' recur
            Else
                mode = CType(BitConverter.ToInt32(res, 1), TrackingModeEnum)
            End If
            Return mode
        End Function


        Public Class DupeContainer
            Public Alloc As HeapAllocationContainer
            Public DupeID As Integer ' ID # of dupe group. Ensure matches with DupeResData
            Public DupeIndex As Integer  ' index within group
            Public DupeCntForID As Integer ' # of items in this groupID
            Public DupeTotalForID As Long
            Public Sub New()

            End Sub
            Public Sub New(ByVal dupe As DupeContainer, ByVal newid As Integer)
                Alloc = dupe.Alloc
                DupeID = newid
                DupeIndex = dupe.DupeIndex
                DupeTotalForID = dupe.DupeTotalForID
            End Sub

            Public Overrides Function ToString() As String
                Return String.Format("Sz {0} ID {1} Ndx {2} Tot {3}", Alloc.GetSize(), DupeID, DupeIndex, DupeTotalForID)
            End Function
        End Class

        Private _ShowSymbolLoadStatus As Integer
        Private _fDidRegSymCallBack As Boolean = False
        Private _nSymFilesLoaded As Integer
        Private _delSymLoad As SymRegisterCallbackProc64 ' this must be a static delegate so it doesn't get GCd
        Private Function SymCallback(ByVal hProcess As IntPtr, ByVal ActionCode As SymActionCode, ByVal CallBackData As Int64, ByVal UserContext As Int64) As Boolean
            Dim retVal = False
            Try

                Dim strMsg = String.Empty
                Select Case ActionCode
                    Case SymActionCode.CBA_DEFERRED_SYMBOL_LOAD_CANCEL
                    Case SymActionCode.CBA_DEBUG_INFO
                        If _ShowSymbolLoadStatus >= 5 Then
                            strMsg = "SymDbgInfo:" + Marshal.PtrToStringAnsi(New IntPtr(CInt(CallBackData)))
                        End If
                    Case SymActionCode.CBA_DEFERRED_SYMBOL_LOAD_START, SymActionCode.CBA_DEFERRED_SYMBOL_LOAD_COMPLETE
                        If ActionCode = SymActionCode.CBA_DEFERRED_SYMBOL_LOAD_START AndAlso _ShowSymbolLoadStatus > 1 OrElse
                            ActionCode = SymActionCode.CBA_DEFERRED_SYMBOL_LOAD_COMPLETE AndAlso _ShowSymbolLoadStatus > 0 Then
                            Dim pData = CType(Marshal.PtrToStructure(
                                                       New IntPtr(CInt(CallBackData)),
                                                       GetType(IMAGEHLP_DEFERRED_SYMBOL_LOAD64)
                                                    ),
                                IMAGEHLP_DEFERRED_SYMBOL_LOAD64)
                            If ActionCode = SymActionCode.CBA_DEFERRED_SYMBOL_LOAD_COMPLETE Then
                                _nSymFilesLoaded += 1
                            End If
                            Dim fname = String.Empty
                            If Not String.IsNullOrEmpty(pData.FileName) Then
                                fname = pData.FileName
                            End If
                            If _ShowSymbolLoadStatus = 1 Then
                                strMsg = String.Format("SymLoad#{0} {1}", _nSymFilesLoaded, IO.Path.GetFileName(pData.FileName))
                            Else
                                strMsg = String.Format("SymLoad#{0} {1} {2}", _nSymFilesLoaded, ActionCode.ToString, pData.FileName)
                            End If

                        End If
                    Case SymActionCode.CBA_SET_OPTIONS
                        If False Then ' (_IsCalvinHBuild OrElse _ShowSymbolLoadStatus > 1) AndAlso Not _IsUnderTest Then
                            MsgBox("Somebody is setting SymSetOptions, changing them from under MemSpect. Attach a debugger to see. Or Dismiss this msgbox")
                        Else
                            strMsg = "Somebody is setting SymSetOptions, changing them from under MemSpect."
                        End If
                    Case SymActionCode.CBA_EVENT
                        If _ShowSymbolLoadStatus >= 5 Then
                            Dim pEventData = CType(Marshal.PtrToStructure(
                                    New IntPtr(CInt(CallBackData)),
                                    GetType(IMAGEHLP_CBA_EVENT)
                                    ),
                                IMAGEHLP_CBA_EVENT)
                            Dim strDesc = Marshal.PtrToStringAnsi(pEventData.desc)
                            strMsg = String.Format("Sym:EV{0} Cod={1} Sev={2}", strDesc, pEventData.code, pEventData.Severity)
                        End If
                    Case SymActionCode.CBA_READ_MEMORY
                        If _ShowSymbolLoadStatus = 6 Then
                            strMsg = String.Format("Sym:{0} {1} ", ActionCode, CallBackData)
                        End If
                        retVal = True
                    Case Else
                        If _ShowSymbolLoadStatus > 0 Then
                            strMsg = String.Format("Sym:{0} {1} ", ActionCode, CallBackData)
                        End If
                End Select
                If Not String.IsNullOrEmpty(strMsg) Then
                    UpdateStatusMsg(strMsg)
                End If
            Catch ex As Exception
                MemSpectExceptionHandler(ex)
            End Try
            Return retVal
        End Function
        ''' <summary>
        ''' convert address (from callstack or heapname) to symbol name (using cache)
        ''' if 4gigStacks, then all addresses are really a key into a map(of address,fmanaged) in the target process.
        ''' </summary>
        ''' <param name="dwAddress">could be the real address or an index (_fHandle4gigStacks)</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Function ResolveAddressToSymbol(
                                              ByVal dwAddress As IntPtr,
                                              Optional ByVal fIsFunctionId As Boolean = False,
                                              Optional ByVal fStripFileName As Boolean = False,
                                              Optional ByVal fStripBytesToo As Boolean = False,
                                              Optional fNormalizeHexAddress As Boolean = False
                                              ) As String
            Dim sym = String.Empty
            If Not StackFrameDictionary.TryGetValue(dwAddress, sym) Then
                If _ConnectionMode = MemSpectMode.OnLine Then
                    Dim fManaged = (dwAddress.ToInt32 And MANAGED_STACK_FLAG) <> 0

                    Dim lamResolve = Function(dwTheRealaddress As IntPtr) As String
                                         If fManaged Then ' managed resolved in target proc
                                             SendMsg(
                                                 ProcMsgVerb.ResolveSymbol,
                                                 fSendEndMsgSync:=True,
                                                 dwords:={dwTheRealaddress.ToInt32, If(fIsFunctionId, 1, 0)}
                                                 )
                                             sym = ReadSharedMemAsString()
                                         Else
                                             If Not _fDidRegSymCallBack Then
                                                 _fDidRegSymCallBack = True
                                                 _ShowSymbolLoadStatus = GetPrivateProfileInt(ProfileStringSection, "ShowSymbolLoadStatus", 0, _iniFileName)
                                                 If _ShowSymbolLoadStatus <> 0 Then
                                                     _delSymLoad = AddressOf SymCallback
                                                     ' chicken or egg?
                                                     ' native code calls SymInitialize, and we can't call SetCallback til after SymInitialize
                                                     VsSetSymbolCallbackFunction(_delSymLoad)
                                                     'If 0 = SymRegisterCallback64(_hProcessTarget, _delSymLoad, 0) Then
                                                     '    Dim lerr = Marshal.GetLastWin32Error
                                                     '    UpdateStatusMsg("Err registering SymCallback " + GetErrorMessageFromWin32LastError(lerr))
                                                     '    _fDidRegSymCallBack = False
                                                     'End If
                                                 End If
                                             End If
                                             Dim sb As New StringBuilder(900)
                                             ' we will always get line no info. so we can go to source code
                                             Dim res = VsResolveSymbolEx(_hProcessTarget, dwTheRealaddress, sb, sb.Capacity, fNoFileLineInfo:=False)
                                             Debug.Assert(res <> 0, "err calling VsResolveSymbolEx")
                                             sym += sb.ToString
                                         End If
                                         Return sym
                                     End Function

                    If _fHandle4gigStacks Then
                        ' everywhere in child process, addresses on stack and for heap name resolution are really an index ORed with MANAGED_STACK_FLAG.
                        ' we need a translation dictionary from index to pair<address,fmanaged>
                        Dim dwRealAddress = IntPtr.Zero
                        If Not _stackIndexDict.TryGetValue(dwAddress, dwRealAddress) Then ' if it's not there
                            Dim tmp = dwAddress
                            If Not _didBulkGetStackIndex Then
                                'If VBDiagMarginBase.IsTargetFrozen Then
                                If _isTargetFrozen Then
                                    _didBulkGetStackIndex = True
                                    BulkReadStackIndexes()
                                End If
                                If Not _stackIndexDict.TryGetValue(dwAddress, dwRealAddress) Then ' if it's still not there
                                    'we'll fall through to getting one
                                End If
                            End If
                            If dwRealAddress = IntPtr.Zero Then ' if still not found
                                Try
                                    Debug.Assert((tmp.ToInt32 And Not MANAGED_STACK_FLAG) >= 0 AndAlso (tmp.ToInt32 And Not MANAGED_STACK_FLAG) < 100000, "4gig stackindex out of bounds?")
                                    SendMsg(ProcMsgVerb.TranslateStackIndex, fSendEndMsgSync:=False, dwords:={tmp.ToInt32}) ' read 1 item in Address->Index map
                                    Dim res = GetMsg(4)
                                    Dim nIndexesSoFar = BitConverter.ToInt32(res, 0) 'will be 1
                                    Debug.Assert(nIndexesSoFar = 1, "nIndexesSoFar should be = 1")
                                    For i = 0 To nIndexesSoFar - 1
                                        res = GetMsg(8) ' index, real address
                                        Dim addr = IntPtr.op_Explicit(BitConverter.ToInt32(res, 0))
                                        Dim ndx = IntPtr.op_Explicit(BitConverter.ToInt32(res, 4)) ' like 1,1,2,3 conditionally OR'ed with MANAGED_STACK_FLAG
                                        If ndx = dwAddress Then
                                            Debug.Assert(dwRealAddress = IntPtr.Zero, ">1 real stack addr for index?")
                                            dwRealAddress = addr
                                        End If
                                        If Not _stackIndexDict.ContainsKey(ndx) Then ' could have been there when added for heapname resolution
                                            _stackIndexDict.Add(ndx, addr)
                                        End If
                                    Next
                                Catch ex As Exception
                                    MemSpectExceptionHandler(ex)
                                Finally
                                    EndMsgSync()
                                End Try
                                '    Debug.Assert(dwRealAddress <> IntPtr.Zero, "could not translate stackindex to realaddress")
                            End If

                        End If
                        If dwRealAddress = IntPtr.Zero Then
                            sym = dwAddress.ToInt32.ToString("x8") + " could not translate stackindex to realaddress "
                        Else
                            sym = lamResolve.Invoke(dwRealAddress)
                        End If
                    Else
                        sym = lamResolve.Invoke(dwAddress)
                    End If
                    StackFrameDictionary.Add(dwAddress, sym)
                Else
                    ' Debug.Assert(False, "trying to resolve symbol when offline")
                    Debug.Assert(Not Common._fHandle4gigStacks, "4 gig stack symbol resolve?")
                    sym = String.Empty
                End If

            End If
            If fStripFileName Then
                sym = SymbolStripFileName(sym, fStripBytesToo:=fStripBytesToo)
            End If
            'If _ShowAddressInCallStacks <> 0 Then
            '    sym = ((Not MANAGED_STACK_FLAG) And dwAddress.ToInt32).ToString("x8") + " " + sym
            'End If
            If fNormalizeHexAddress Then
                '!0x2F2BE4F0 SymErr = 0x7e
                'Microsoft.Alm.CodeAnalysis.Shared.ni.dll!0x5298D1DF SymErr = 0x1e7
                If sym.Contains("!0x") Then
                    sym = Text.RegularExpressions.Regex.Replace(sym, "!0x[0-9A-Fa-f]{8}", "0x00000000")
                End If
            End If
            Return sym
        End Function
        ''' <summary>
        ''' Can be called multiple times to get the stackindexes from target process
        ''' </summary>
        Public Sub BulkReadStackIndexes()
            Try
                If Common._fHandle4gigStacks Then
                    SendMsg(ProcMsgVerb.TranslateStackIndex, fSendEndMsgSync:=False, dwords:={0}) ' read all entries in Address->Index map
                    Dim res = GetMsg(4)
                    Dim nIndexesSoFar = BitConverter.ToInt32(res, 0)
                    For i = 0 To nIndexesSoFar - 1
                        res = GetMsg(8) ' index, real address
                        Dim addr = IntPtr.op_Explicit(BitConverter.ToInt32(res, 0))
                        Dim ndx = IntPtr.op_Explicit(BitConverter.ToInt32(res, 4)) ' like 1,1,2,3 conditionally OR'ed with MANAGED_STACK_FLAG
                        If Not _stackIndexDict.ContainsKey(ndx) Then ' could have been there when added for heapname resolution
                            _stackIndexDict.Add(ndx, addr)
                        End If
                    Next
                    EndMsgSync()
                End If
            Catch ex As Exception
                MemSpectExceptionHandler(ex)
            Finally
            End Try
        End Sub


        Public Class WorkingSetInfo
            Inherits SerializableObject
            Public VirtPage As IntPtr
            Public Protection As Integer
            Public ShareCount As Integer
            Public IsShared As Integer

            Public Overrides Sub ToStream(ByVal serializer As Serializer)
                serializer.Write(VirtPage.ToInt32)
                serializer.Write(Protection)
                serializer.Write(ShareCount)
                serializer.Write(IsShared)
            End Sub

            Public Overrides Sub FromStream(ByVal deserializer As Deserializer, ByVal versionFromStream As Integer)
                VirtPage = New IntPtr(deserializer.ReadInt)
                Protection = deserializer.ReadInt
                ShareCount = deserializer.ReadInt
                IsShared = deserializer.ReadInt

            End Sub
            Public Shared _WorkingSetDict As SortedList(Of IntPtr, WorkingSetInfo)
            Public Shared Function GetWorkingSetDict() As SortedList(Of IntPtr, WorkingSetInfo)
                If _WorkingSetDict IsNot Nothing Then
                    Return _WorkingSetDict
                End If
                Try
                    _WorkingSetDict = New SortedList(Of IntPtr, WorkingSetInfo)(_CompareIntPtr)
                    If _ConnectionMode = MemSpectMode.OnLine OrElse _ConnectionMode = MemSpectMode.Existing Then
                        Dim nSize = 8
                        Dim addrPSAPI_WORKING_SET_INFORMATION = Marshal.AllocCoTaskMem(nSize) 'PSAPI_WORKING_SET_INFORMATION
                        Marshal.WriteInt32(addrPSAPI_WORKING_SET_INFORMATION, 0)
                        Marshal.WriteInt32(addrPSAPI_WORKING_SET_INFORMATION, 4, 0)
                        Dim res = QueryWorkingSet(_hProcessTarget, addrPSAPI_WORKING_SET_INFORMATION, 8)
                        If Marshal.GetLastWin32Error = ERROR_BAD_LENGTH Then
                            Dim nDwords = Marshal.ReadInt32(addrPSAPI_WORKING_SET_INFORMATION)
                            Marshal.FreeCoTaskMem(addrPSAPI_WORKING_SET_INFORMATION)
                            nSize = 4 + nDwords * 14 + 8 ' # entries followed by array of DWORDS PSAPI_WORKING_SET_INFORMATION + 8 for reading LONGs
                            addrPSAPI_WORKING_SET_INFORMATION = Marshal.AllocCoTaskMem(nSize)
                            res = QueryWorkingSet(_hProcessTarget, addrPSAPI_WORKING_SET_INFORMATION, nSize)
                            If res <> 0 Then
                                '                                Debug.Assert(Marshal.ReadInt32(addrPSAPI_WORKING_SET_INFORMATION) = nDwords, "QueryWorkingSet mismatch on 2nd call")
                                For i = 0 To nDwords - 1
                                    Dim entry = Marshal.ReadInt32(addrPSAPI_WORKING_SET_INFORMATION, 4 + i * 4) ' overlapped 
                                    Dim temp = CLng(entry And (&HFFFFF000))
                                    Dim temp2 = CInt((temp) And &HFFFFFFFF)
                                    Dim virtPage = New IntPtr(temp2)
                                    Dim flags = entry And &HFFF
                                    Dim protection = CInt(flags And &H1F) ' 31
                                    Dim sharecount = CInt((flags And &H30) / 2 ^ 5) ' next 3 bits
                                    Dim fshared = flags And &H40 'next 1 bit

                                    'Dim str = String.Format("{0:x8}  {1:x8}, {2:x8} {3:x8}", virtPage.ToInt32, protection, sharecount, fshared)
                                    'Debug.WriteLine(str)
                                    Dim wsInfo = New WorkingSetInfo With {
                                                   .VirtPage = virtPage,
                                                   .Protection = protection,
                                                   .ShareCount = sharecount,
                                                   .IsShared = If(fshared = 0, 0, 1)
                                               }

                                    If _WorkingSetDict.ContainsKey(virtPage) Then
                                        '              CommonUI.UpdateStatusMsgDbg("Dupe key " + wsInfo.ToString)
                                    Else
                                        _WorkingSetDict.Add(virtPage, wsInfo)
                                    End If
                                Next
                            End If
                        End If
                        Marshal.FreeCoTaskMem(addrPSAPI_WORKING_SET_INFORMATION)
                    ElseIf _ConnectionMode = MemSpectMode.Offline Then
                        _WorkingSetDict = Common._offlineSnapshot.WorkingSetList
                    End If
                Catch ex As Exception
                    Common.MemSpectExceptionHandler(ex)
                End Try
                Return _WorkingSetDict
            End Function
            Public Overrides Function ToString() As String
                Return String.Format("{0:x8}  {1:x8}, {2:x8} {3:x8}", VirtPage.ToInt32, Protection, ShareCount, IsShared)
            End Function
        End Class

        '   /// Binary search for 1st item >= key
        '    /// Returns -1 for empty list
        '    /// Returns list.count if key > all items
        '    /// </summary>
        '    /// <typeparam name="T"></typeparam>
        '    /// <param name="sortedList"></param>
        '    /// <param name="key"></param>
        '    public static int FindIndexOfFirstGTorEQTo<T>(this IList<T> sortedList, T key) where T : IComparable<T>
        '    {
        '        int right = 0;
        '        if (sortedList.Count == 0) //empty list
        '        {
        '            right = -1;
        '        }
        '        else
        '        {
        '            right = sortedList.Count - 1;
        '            int left = 0;
        '            while (right > left)
        '            {
        '                var ndx = (left + right) / 2;
        '                var elem = sortedList[ndx];
        '                if (elem.CompareTo(key) >= 0)
        '                {
        '                    right = ndx;
        '                }
        '                else
        '                {
        '                    left = ndx + 1;
        '                }
        '            }
        '        }
        '        if (right >= 0) // see if we're beyond the list?
        '        {
        '            if (sortedList[right].CompareTo(key) < 0)
        '            {
        '                right = sortedList.Count;
        '            }

        '        }
        '        return right;
        '    }
        '    /// <summary>
        '    /// Clear event subscribers. Useful in test scenarios where multiple tests run and perhaps fail to unscubscribe
        '    /// Doesn't work for static events
        '    /// </summary>
        '    /// <param name="obj"></param>
        '    /// <param name="eventName"></param>
        '    /// <returns></returns>
        '    public static bool ClearEventSubscribers(this object obj, string eventName)
        '    {
        '        bool fDidClearEvents = false;
        '        if (obj != null)
        '        {
        '            var type = obj.GetType();
        '            while (type != null)
        '            {
        '                var field = type.GetField(eventName, System.Reflection.BindingFlags.Static | System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic);
        '                if (field != null)
        '                {
        '                    if (field.FieldType == typeof(MulticastDelegate) || field.FieldType.IsSubclassOf(typeof(MulticastDelegate)))
        '                    {
        '                        field.SetValue(obj, null);
        '                        fDidClearEvents = true;
        '                        break;
        '                    }
        '                }
        '                type = type.BaseType;
        '            }
        '        }
        '        return fDidClearEvents;
        '    }
        '}



        Public Function FindNearest(Of T)(
                                         ByVal sortedList As IList(Of T),
                                         ByVal value As T,
                                         ByVal pMaxLeft As Integer?,
                                         ByVal pMaxRight As Integer?,
                                         ByVal pComparer As Comparer(Of T)
                                         ) As Integer()
            Const left = 0
            Const right = 1
            Const notfound = -1
            Dim maxRight = pMaxRight.GetValueOrDefault(sortedList.Count - 1)
            Dim maxLeft = pMaxLeft.GetValueOrDefault(0)
            Dim partition = {maxLeft, maxRight}
            Dim nearest = {maxLeft, maxRight}
            If pComparer Is Nothing Then
                pComparer = Comparer(Of T).Default
            End If
            Do While partition(right) >= partition(left)
                Dim split = partition(left) + (partition(right) - partition(left)) \ 2
                Dim order = pComparer.Compare(value, sortedList(split))
                Select Case order
                    Case -1
                        nearest(right) = split
                        partition(right) = split - 1
                    Case 1
                        nearest(left) = split
                        partition(left) = split + 1
                    Case 0
                        nearest = {split, split}
                        Return nearest
                End Select
            Loop
            If partition(right) < maxLeft Then
                nearest = {notfound, maxLeft}
            ElseIf partition(left) > maxRight Then
                nearest = {maxRight, notfound}
            End If
            Return nearest
        End Function

        <System.Runtime.CompilerServices.Extension()>
        Sub AddIfDoesntExist(ByVal coll As System.Collections.Specialized.StringDictionary, ByVal key As String, ByVal newval As String)
            If Not coll.ContainsKey(key) Then
                coll.Add(key, newval)
            End If
        End Sub

        <System.Runtime.CompilerServices.Extension()>
        Public Sub WritePrivateCollection(Of T)(ByVal ser As Serializer, ByVal coll As IEnumerable(Of T), Optional ByVal name As String = "")
            ''faster than Serializer.WriteCollection: call when it's a private collection and no dupes with any other obj
            ser.Write(coll.Count)
            For Each item In coll
                ser.WritePrivate(CType(item, IFastSerializable))
            Next
        End Sub


        Public Function ReadProcessMemoryDwordsEx(
               ByVal hProcess As IntPtr,
               ByVal lpBaseAddress As IntPtr,
               ByRef lpBuffer As ProcMemIntPtr,
               ByVal dwSize As Integer,
               ByRef lpNumberOfBytesRead As Integer
                                                 ) As Integer
            Dim res As Integer = 0
            If _ConnectionMode = MemSpectMode.Offline Then
                Dim barray = MiniDumpReader.Singleton.ReadMemoryDictionary(lpBaseAddress, dwSize)
                If barray IsNot Nothing Then
                    Dim lenToUse = Math.Min(barray.Length, dwSize)
                    For i = 0 To CInt(lenToUse / 4) - 1
                        lpBuffer.data(i) = CType(BitConverter.ToInt32(barray, i * 4), IntPtr)
                    Next
                    lpNumberOfBytesRead = barray.Length
                    res = 1
                End If
            Else
                res = ReadProcessMemoryDwords(hProcess, lpBaseAddress, lpBuffer, dwSize, lpNumberOfBytesRead)
            End If
            If res = 0 Then
                UpdateStatusMsg("err reading mem ReadProcessMemoryDwords " + lpBaseAddress.ToInt32.ToString("x8"), fAssert:=True)
            End If
            Return res
        End Function
        ''' <summary>
        ''' read target mem, works offline or online
        ''' </summary>
        Public Function ReadProcessMemoryDWORDEx(ByVal addr As IntPtr) As Integer
            Dim res As Integer = 0
            If _ConnectionMode = MemSpectMode.Offline Then
                Dim barray = MiniDumpReader.Singleton.ReadMemoryDictionary(addr, 4)
                If barray Is Nothing OrElse barray.Length <> 4 Then
                    Debug.Assert(False, ("OfflineReadMem fail " + addr.ToInt32.ToString("x8")))
                Else
                    res = BitConverter.ToInt32(barray, 0)
                End If
            Else
                ReadProcessMemoryDword(_hProcessTarget, addr, res, 4, 0)
            End If
            Return res
        End Function
        Public Function IntegerToUInteger(val As Integer) As UInteger
            Dim bytes = BitConverter.GetBytes(val)
            Dim y = BitConverter.ToUInt32(bytes, startIndex:=0)
            Return y
        End Function
        Public Function UIntegerToInteger(val As UInteger) As Integer
            Dim bytes = BitConverter.GetBytes(val)
            Dim y = BitConverter.ToInt32(bytes, startIndex:=0)
            Return y
        End Function

        Public Function SymbolStripFileName(ByVal sym As String, Optional ByVal fStripBytesToo As Boolean = False) As String
            Dim res = sym
            If Not String.IsNullOrEmpty(sym) Then
                Dim pos = sym.LastIndexOf(" : ") ' allow "::" as class/method sep
                If pos >= 0 Then
                    res = sym.Substring(pos + 3)
                End If
                If fStripBytesToo Then ' remove the " + 39 bytes"
                    Dim mtch = System.Text.RegularExpressions.Regex.Match(res, " \+ [0-9]+ bytes")
                    If mtch.Success Then
                        res = res.Substring(0, mtch.Index)
                    End If
                End If
            End If
            Return res
        End Function

        Public _nToMatchForUnused As Integer = 17
        Public _byteToMatch As Integer = 175 '&hAF must match VsDebugAllocInternal

        Public Class MemoryEater
            Public Shared _EatenAllocations As New Dictionary(Of IntPtr, MEMORY_BASIC_INFORMATION)
            Public Shared Sub EatSomeMemory(ByVal eatSize As Integer,
                                     ByVal f2GigLimit As Boolean,
                                     ByVal fTopDown As Boolean,
                                     ByVal fEatAll As Boolean,
                                     ByVal fCommit As Boolean)
                Try
                    If eatSize <= 0 Then
                        Throw New ArgumentException("nChunkSizeInBytes " + eatSize.ToString)
                    End If
                    Dim dwFlags = CType(
                           If(fTopDown, AllocationType.MEM_TOP_DOWN, CUInt(0)) Or
                           If(fCommit, AllocationState.MEM_COMMIT, CUInt(0)) Or
                           AllocationState.MEM_RESERVE,
                        AllocationType)
                    Dim nLoops = 0
                    Do
                        Dim resstr = SendMsg(ProcMsgVerb.DoVirtualAlloc, fSendEndMsgSync:=True, dwords:={0, eatSize, CInt(dwFlags), CInt(AllocationProtect.PAGE_READWRITE)})
                        Dim addrVirtAlloc = New IntPtr(BitConverter.ToInt32(resstr, 1))
                        'Dim res = VirtualAllocEx(_hProc, IntPtr.Zero, CUInt(tstSize), dwFlags, AllocationProtect.PAGE_READWRITE) ' OS bug: TOP_DOWN not respected by VirtualAllocEx

                        If addrVirtAlloc <> IntPtr.Zero Then
                            nLoops += 1
                            If fCommit Then
                                Dim bEncode As New Text.UnicodeEncoding
                                Dim nOffset = 0
                                Dim bWritten = 0
                                Do
                                    Dim bArray = bEncode.GetBytes("MemSpect was here! " + (addrVirtAlloc + nOffset).ToString("x8") + " " + DateTime.Now.ToString)
                                    WriteProcessMemory(_hProcessTarget, addrVirtAlloc.MyAdd(nOffset), bArray, bArray.Length, bWritten)
                                    If bWritten <> bArray.Length Then
                                        UpdateStatusMsg("Error writing committed VM")
                                    End If
                                    nOffset += 512
                                    If nOffset + 100 >= eatSize Then
                                        bWritten = 0
                                    End If
                                Loop While bWritten > 0
                            End If

                            _EatenAllocations.Add(addrVirtAlloc,
                                                New MEMORY_BASIC_INFORMATION With {
                                                  .AllocationBase = CType(addrVirtAlloc, IntPtr),
                                                  .BaseAddress = .AllocationBase,
                                                  .RegionSize = CUInt(eatSize),
                                                  .State = If(fCommit, AllocationState.MEM_COMMIT, AllocationState.MEM_RESERVE)
                                              }
                                          )
                        Else
                            UpdateStatusMsg("Mem eater done eating: out of mem")
                            Exit Do
                        End If
                        If Not fEatAll Then
                            Exit Do
                        End If
                        Dim fStop = False
                        If f2GigLimit Then
                            If fTopDown Then
                                ' growing from HIGH down to 0x80000000
                                If addrVirtAlloc.ToInt32 > 0 Then
                                    fStop = True
                                End If
                            Else
                                ' growing from 0 to 0x80000000
                                If addrVirtAlloc.ToInt32 < 0 Then
                                    fStop = True
                                End If
                            End If
                            'because we stopped *past* the limit we need to free the last one
                            If fStop Then
                                _EatenAllocations.Remove(addrVirtAlloc)
                                nLoops -= 1
                                SendMsg(ProcMsgVerb.DoVirtualFree, fSendEndMsgSync:=True, dwords:={addrVirtAlloc.ToInt32, 0, FreeType.MEM_RELEASE})
                            End If
                        End If
                        If fStop Then
                            Exit Do
                        End If
                    Loop
                    UpdateStatusMsg("MemEater ate " + nLoops.ToString + " allocs of " + eatSize.ToString("n0"))
                Catch ex As Exception
                    Common.MemSpectExceptionHandler(ex)
                End Try
            End Sub

            Public Shared Sub FreeAll()
                For Each alloc In _EatenAllocations.Values
                    SendMsg(ProcMsgVerb.DoVirtualFree, fSendEndMsgSync:=True, dwords:={CInt(alloc.BaseAddress), 0, FreeType.MEM_RELEASE})
                Next
                _EatenAllocations.Clear()
            End Sub
        End Class

        'Automatic leak detection support by callstack matching.
#Region "LeakDetection"


        ''' <summary>
        ''' LeakDetector class
        ''' </summary>
        ''' <remarks>This class wraps the results of a single leak detection operation.  It contains the allocations to search and the ranges to search over, and other configuration parameters
        ''' The result of a search is a collection of LeakSummaryContainers, one for each detected leak.  These can be accessed by the LeakGroups property.</remarks>
        Public Class LeakDetector
            Public Class LeakDetectorParameters
                Public FilterRanges As New List(Of System.Tuple(Of Integer, Integer))()
                Public Allocs As IEnumerable(Of HeapAllocationContainer) = Nothing
                Public MatchThreshold As Double = 0.85
                Public AllocationTypeFilter As BlockTypes = BlockTypes.None
                Public FindTraditionalLeaksOnly As Boolean = False
            End Class

            Private _processedSoFar As Integer = 0
            Private _totalToProcess As Integer = 0
            Private _leaks As IEnumerable(Of HeapAllocationContainer)
            Private _detectionParams As New LeakDetectorParameters()
            Private _leakGroups As New List(Of LeakSummaryContainer)()
            Private _detectorID As Integer

            Public Sub New()
                Static _uniqueDetectorID As Integer = 0
                _detectorID = _uniqueDetectorID
                _uniqueDetectorID += 1
            End Sub

            Public ReadOnly Property LeakDetectorID As Integer
                Get
                    Return Me._detectorID
                End Get
            End Property

            Public Property DetectionParameters As LeakDetectorParameters
                Get
                    Return _detectionParams
                End Get
                Private Set(ByVal value As LeakDetectorParameters)
                    _detectionParams = value
                End Set
            End Property

            Public Property ProcessedSoFar As Integer
                Get
                    Return _processedSoFar
                End Get
                Private Set(ByVal value As Integer)
                    _processedSoFar = value
                End Set
            End Property

            Public Property TotalToProcess As Integer
                Get
                    Return _totalToProcess
                End Get
                Private Set(ByVal value As Integer)
                    _totalToProcess = value
                End Set
            End Property

            Public Property Leaks As IEnumerable(Of HeapAllocationContainer)
                Get
                    Return _leaks
                End Get
                Private Set(ByVal value As IEnumerable(Of HeapAllocationContainer))
                    _leaks = value
                End Set
            End Property

            Public Property LeakGroups As List(Of LeakSummaryContainer)
                Get
                    Return _leakGroups
                End Get
                Private Set(ByVal value As List(Of LeakSummaryContainer))
                    _leakGroups = value
                End Set
            End Property

            'Does some basic verification that the ranges provided are properly defined.
            Public Function ValidateLeakRanges(ByVal filterRanges As List(Of System.Tuple(Of Integer, Integer))) As Boolean

                For Each range In filterRanges
                    If range.Item1 < 0 Then
                        UpdateStatusMsg(String.Format(
                                        "Invalid range definition for {0},{1}.  Lower bounds is less than 0.  Sequence numbers must be non-negative.",
                                        range.Item1.ToString(),
                                        range.Item2.ToString()),
                                    msgType:=StatusMessageType.AlertMsgBox)
                        Return False
                    End If

                    If range.Item2 < 0 Then
                        UpdateStatusMsg(String.Format(
                                        "Invalid range definition for {0},{1}.  Upper bounds is less than 0.  Sequence numbers must be non-negative.",
                                        range.Item1.ToString(),
                                        range.Item2.ToString()),
                                    msgType:=StatusMessageType.AlertMsgBox)
                        Return False
                    End If

                    If range.Item1 > range.Item2 Then
                        UpdateStatusMsg(String.Format(
                                        "Invalid range definition for {0},{1}.  Specified lower bounds is greater than upper bounds.",
                                        range.Item1.ToString(),
                                        range.Item2.ToString()),
                                    msgType:=StatusMessageType.AlertMsgBox)
                        Return False
                    End If

                    If range.Item1 = range.Item2 Then
                        UpdateStatusMsg(String.Format(
                                        "Range definition for {0},{1} is only a single allocation.",
                                        range.Item1.ToString(),
                                        range.Item2.ToString()),
                                    msgType:=StatusMessageType.AlertMsgBox)
                    End If
                Next

                For i As Integer = 0 To filterRanges.Count - 1
                    Dim currRange = filterRanges(i)
                    For j As Integer = i + 1 To filterRanges.Count - 1
                        Dim tempRange = filterRanges(j)
                        If (currRange.Item1 > tempRange.Item1 And currRange.Item1 < tempRange.Item2) Or
                            (currRange.Item2 > tempRange.Item1 And currRange.Item2 < tempRange.Item2) Then

                            UpdateStatusMsg(String.Format("Overlapping ranges detected for {0},{1} and {2},{3}.  This may result in invalid results.",
                                                                 currRange.Item1.ToString(),
                                                                 currRange.Item2.ToString(),
                                                                 tempRange.Item1.ToString(),
                                                                 tempRange.Item2.ToString()),
                                                             msgType:=StatusMessageType.AlertMsgBox)

                        End If
                    Next
                Next

                Return True
            End Function

            'Called by a background worker to do the searching asynchronously
            Public Sub FindLeaksAsync(ByVal leakParams As LeakDetectorParameters, ByVal worker As BackgroundWorker, ByVal e As DoWorkEventArgs)
                Me.DetectionParameters = leakParams
                PrivateFindLeaks(leakParams, worker, e)
            End Sub

            'Synchronous search
            Public Sub FindLeaks(ByVal leakParams As LeakDetectorParameters)
                Me.DetectionParameters = leakParams
                PrivateFindLeaks(leakParams, Nothing, Nothing)
            End Sub

            'The bulk of the detection is done here.
            'The basic algorithm is as follows:
            '   1. Partition the allocations into the defined ranges.
            '   2. For each allocation in the first range
            '       2.1 Iteration over each range and find all the matching allocations in that range.
            '       2.2 Any matched allocations are removed from the range to avoid being recompared.
            '       2.3 Group all the matching allocations into a LeakSummaryContainer
            '   3. Continue until the first range is empty
            Private Function PrivateFindLeaks(ByVal leakParams As LeakDetectorParameters,
                                       ByVal worker As BackgroundWorker,
                                       ByVal e As DoWorkEventArgs) As IEnumerable(Of HeapAllocationContainer)

                Dim filterRanges As List(Of System.Tuple(Of Integer, Integer)) = leakParams.FilterRanges
                Dim allocs As IEnumerable(Of HeapAllocationContainer) = leakParams.Allocs
                Dim matchThreshold As Double = leakParams.MatchThreshold
                Dim allocationType As BlockTypes = leakParams.AllocationTypeFilter
                Dim traditionalLeaksOnly As Boolean = leakParams.FindTraditionalLeaksOnly

                Me.Leaks = Nothing

                If (filterRanges.Count < 2) Then
                    UpdateStatusMsg("Need at least 2 ranges to search for leaks.")
                    Return Me.Leaks
                End If

                If allocs.Count < 3 Then
                    UpdateStatusMsg("Heap contains insufficient allocations to contain leaks.")

                    Return Me.Leaks
                End If

                'Dim baselineRange As List(Of HeapAllocationContainer) = New List(Of HeapAllocationContainer)()
                Dim searchRanges As List(Of List(Of HeapAllocationContainer)) = New List(Of List(Of HeapAllocationContainer))()

                'partition the allocations up into the individual ranges.
                'todo: we don't need to do this.  We can just search for all matching allocations in the entire set, and then later split the allocations up by range.
                '      this might be faster and more efficient.
                For i As Integer = 0 To filterRanges.Count - 1
                    If Not worker Is Nothing Then

                        If worker.CancellationPending Then
                            e.Cancel = True
                            Return Leaks
                        End If

                        worker.ReportProgress(CInt(i * 100 / filterRanges.Count), String.Format("Collecting allocations for range {0}", (i + 1).ToString()))
                    End If

                    Dim tempIndex = i
                    Dim tempRange = allocs.Where(Function(alloc As HeapAllocationContainer) As Boolean
                                                     Dim validAlloc = alloc.AllocationStruct.SeqNo >= filterRanges(tempIndex).Item1 And
                                                                      alloc.AllocationStruct.SeqNo < filterRanges(tempIndex).Item2 And
                                                                      alloc.IsLeakableType

                                                     If Not allocationType = BlockTypes.None Then
                                                         validAlloc = validAlloc And alloc.TBlkBlockType = allocationType
                                                     End If

                                                     Return validAlloc
                                                 End Function)
                    searchRanges.Add(tempRange.ToList())
                Next

                'consider: we can search for either exact matches, or approximate matches.  think about whether the UI can/should be different for the two, and if so, what would it look like
                '      ie: it might be nice to highlight the mismatched frames in approximate matches
                Dim foundMatches As New List(Of IEnumerable(Of HeapAllocationContainer))()
                'Dim matchIDDictionary As New Dictionary(Of IntPtr(), Integer)()
                Dim iterCount = 0
                Dim total As Integer = searchRanges(0).Count
                Dim progressUpdateInterval As Integer = CInt(searchRanges(0).Count / 10)
                Me.ProcessedSoFar = 0
                Me.TotalToProcess = searchRanges(0).Count
                Dim currentLeakID As Integer = 0

                'for a given allocation in the first range
                '   find all allocations in each range with that allocation
                '   tag them with ID and remove them
                While searchRanges(0).Count > 0
                    iterCount += 1

                    'check worker state for cancelation and send update if needed.
                    If Not worker Is Nothing Then
                        If worker.CancellationPending Then
                            e.Cancel = True
                            Return Leaks
                        End If

                        If iterCount Mod 5 = 0 Then
                            worker.ReportProgress(CInt(iterCount / total), String.Format("Processed {0} / {1}", ProcessedSoFar, TotalToProcess))
                        End If
                    End If


                    Dim currentAlloc As HeapAllocationContainer = searchRanges(0)(0)
                    Dim matchArray As IEnumerable(Of HeapAllocationContainer)() = CType(Array.CreateInstance(GetType(IEnumerable(Of HeapAllocationContainer)), filterRanges.Count), IEnumerable(Of HeapAllocationContainer)())

                    'find matches in every range.  save them and remove them from the ranges being searched.
                    For i As Integer = 0 To searchRanges.Count - 1
                        Dim tempMatches As IEnumerable(Of HeapAllocationContainer) = FindAllMatchesInRange(searchRanges(i), currentAlloc, matchThreshold)
                        matchArray.SetValue(tempMatches, i)

                        For Each tempMatch In tempMatches
                            searchRanges(i).Remove(tempMatch)
                        Next
                    Next

                    Me.ProcessedSoFar += matchArray(0).Count()

                    'depending on what kind of leak we're looking for, we need to check if an allocation occurred in every range or just some of them.
                    Dim foundLeaks As Boolean
                    If traditionalLeaksOnly Then
                        'traditional leaks require allocations to occur at least once in each range
                        foundLeaks = True
                        For Each matchset In matchArray
                            foundLeaks = foundLeaks And matchset.Count() > 0
                        Next
                    Else
                        'fuzzy leaks need only for there to be allocations found in some other range than the first one.
                        foundLeaks = False
                        For i As Integer = 1 To matchArray.Length - 1
                            foundLeaks = foundLeaks Or matchArray(i).Count() > 0
                            If foundLeaks Then
                                Exit For
                            End If
                        Next
                    End If

                    'if we found a leak, tag it with a GroupID and create the new LeakSummaryContainer
                    If foundLeaks Then
                        Dim results As IEnumerable(Of HeapAllocationContainer) = From match In matchArray
                                                                                 From matchAlloc In match
                                                                                 Select matchAlloc

                        'For Each result In results
                        '    result.AddGroupID(_detectorID, currentLeakID)
                        'Next

                        foundMatches.Add(results)

                        Dim leakContainer = New LeakSummaryContainer(results, currentLeakID)
                        For i As Integer = 0 To matchArray.Length - 1
                            leakContainer.LeakRangeCounts.Add(matchArray(i).Count())
                        Next
                        Me.LeakGroups.Add(leakContainer)

                        currentLeakID += 1
                    End If
                End While

                'Group all the leaking allocations together and return.
                Me.Leaks = From matches In foundMatches
                           From match In matches
                           Where Not match Is Nothing
                           Select match

                Return Me.Leaks
            End Function

            'returns a collection of allocations who match the provided allocation by at least the specified threshold.
            Private Function FindAllMatchesInRange(ByVal range As List(Of HeapAllocationContainer),
                                                   ByVal alloc As HeapAllocationContainer,
                                                   ByVal threshold As Double) As IEnumerable(Of HeapAllocationContainer)

                Dim tempMatches = From a In range
                                  Where StacksMatch(alloc.GetCallStackAddressestoArray(), a.GetCallStackAddressestoArray(), threshold)
                                  Select a

                Return CType(tempMatches.ToArray().Clone(), IEnumerable(Of HeapAllocationContainer))
            End Function

            'compares the stacks of two allocations to see if they match by at least the specified threshold.
            Private Function StacksMatch(ByVal baseStack As IntPtr(), ByVal tempStack As IntPtr(), ByVal threshold As Double) As Boolean
                'consider: change address comparisons to unique IDs and use shorts?

                'if our threshold is 100%, then every frame has to match.  As soon as one doesn't match, return false
                If threshold = 1.0 Then
                    If baseStack.Length <> tempStack.Length Then
                        Return False
                    End If

                    For i As Integer = 0 To baseStack.Length - 1
                        If baseStack(i) <> tempStack(i) Then
                            Return False
                        End If
                    Next

                    Return True
                Else
                    Dim minFramesToMatch As Integer = Convert.ToInt32(baseStack.Length * threshold)
                    Dim maxMismatchedFrames As Integer = baseStack.Length - minFramesToMatch


                    'consider: couple of different wants to do this.  note, that if stacks get truncated due to the limit set in the .ini, this might be harder/ineffective
                    ' - start at the top and require that the stacks match all the way down up to the desired accuracyThreshold
                    ' - just require that overall there is a match rate over the accuracyThreshold
                    ' - start at both sides and match in to the middle. this could not work due to truncated callstacks
                    Dim matchedFrameCount = 0
                    Dim mismatchedFrameCount = 0
                    Dim tempStackIndex = 0
                    For i As Integer = 0 To Math.Min(baseStack.Length - 1, tempStack.Length - 1)
                        If tempStackIndex >= tempStack.Length Then
                            Exit For
                        End If

                        ''if we have enough frames, return true
                        If matchedFrameCount >= minFramesToMatch Then
                            Return True
                        End If

                        'if we've missed more than we're allowed to miss, return false
                        If mismatchedFrameCount > maxMismatchedFrames Then
                            Return False
                        End If

                        'if the current two frames match, increment counters and continue
                        If baseStack(i) = tempStack(tempStackIndex) Then
                            matchedFrameCount += 1
                            tempStackIndex += 1
                            Continue For
                        End If

                        'the current frames don't match, so keep incrementing the pointer for the target callstack to see if we eventually get a match.
                        'If we do, increment counters and set the tempStackIndex to point to this frame and keep going
                        Dim mismatchedFrame As Boolean = True
                        For j As Integer = tempStackIndex + 1 To Math.Min(tempStackIndex + maxMismatchedFrames + 1, tempStack.Length - 1)
                            If baseStack(i) = tempStack(j) Then
                                matchedFrameCount += 1
                                tempStackIndex = j
                                mismatchedFrame = False
                                Exit For
                            End If
                        Next

                        'if we never found a matching frame, increment mismatched count and continue
                        If mismatchedFrame Then
                            mismatchedFrameCount += 1
                        End If

                        'move the target callstack frame pointer up by one.
                        tempStackIndex += 1
                    Next

                    'if we have enough frames, return true
                    If matchedFrameCount >= minFramesToMatch Then
                        Return True
                    End If

                End If

                Return False
            End Function

            'Collection of allocations that are an exact or potential leak
            Public Class LeakSummaryContainer
                Private _leakSize As Integer = -1
                Private _leakID As Integer = -1
                Private _leakGroup As IEnumerable(Of HeapAllocationContainer)
                Private _leakRangeCounts As New List(Of Integer)()

                Public Sub New(ByVal leaks As IEnumerable(Of HeapAllocationContainer), ByVal leakID As Integer)
                    Me._leakGroup = leaks
                    Me._leakID = leakID
                End Sub

                Public Property LeakRangeCounts As List(Of Integer)
                    Get
                        Return _leakRangeCounts
                    End Get
                    Private Set(ByVal value As List(Of Integer))

                    End Set
                End Property

                Public Property LeakGroup As IEnumerable(Of HeapAllocationContainer)
                    Get
                        Return _leakGroup
                    End Get
                    Private Set(ByVal value As IEnumerable(Of HeapAllocationContainer))
                        _leakGroup = value
                    End Set
                End Property

                Public ReadOnly Property LeakSize As Integer
                    Get
                        If _leakSize = -1 Then
                            _leakSize = (From alloc In LeakGroup
                                         Select alloc.GetSize).Sum()
                        End If
                        Return _leakSize
                    End Get
                End Property

                Public ReadOnly Property LeakID As Integer
                    Get
                        Return _leakID
                    End Get
                End Property

                Public ReadOnly Property LeakSummary As String
                    Get
                        Return Me.ComputeLeakSummary()
                    End Get
                End Property

                Private Function ComputeLeakSummary() As String
                    Dim types = From allocs In Me.LeakGroup
                                Select allocs.TBlkBlockType
                                Distinct

                    Dim clrObjTypes As IEnumerable(Of String) = Nothing
                    If types.Contains(BlockTypes.ClrObject) Then
                        clrObjTypes = From alloc In Me.LeakGroup
                                      Where alloc.TBlkBlockType = BlockTypes.ClrObject
                                      Select alloc.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True)
                                      Distinct
                    End If

                    Dim summaryText As New StringBuilder()
                    summaryText.AppendLine(String.Format("LeakID: {0}   LeakSize: {1}   LeakedObjects: {2}", Me.LeakID, Me.LeakSize, Me.LeakGroup.Count()))
                    summaryText.AppendLine()

                    summaryText.AppendLine("Allocation types: ")

                    Dim printClrObjTypes As Boolean = False
                    For Each allocType In types
                        Dim typeStr As String = [Enum].GetName(GetType(BlockTypes), allocType)
                        If typeStr.Equals("None") Then
                            typeStr = "HeapAlloc"
                        ElseIf typeStr.Equals([Enum].GetName(GetType(BlockTypes), BlockTypes.ClrObject)) Then
                            typeStr += "(types listed after callstack)"
                            printClrObjTypes = True
                        End If
                        summaryText.AppendLine("    " + typeStr + " ")
                    Next

                    summaryText.AppendLine()

                    summaryText.AppendLine("Allocation range distributions:")
                    For i As Integer = 0 To Me.LeakRangeCounts.Count - 1
                        summaryText.AppendLine("    " + i.ToString() + ": " + Me.LeakRangeCounts(i).ToString() + " ")
                    Next

                    summaryText.AppendLine()
                    summaryText.AppendLine()

                    summaryText.AppendLine(GetStacksAndDump(Me.LeakGroup(0), nMaxDumpSize:=0).Key)

                    summaryText.AppendLine()


                    If printClrObjTypes Then
                        summaryText.AppendLine("Clr Object Types")
                        For Each clrObjType In clrObjTypes
                            summaryText.AppendLine("    " + clrObjType)
                        Next
                        summaryText.AppendLine()
                    End If

                    Return summaryText.ToString()
                End Function

            End Class

        End Class
#End Region

        'Common function getting allocations based on a stackframe address pivot.
        Public Function GetAllocsForPivot(ByVal dwTargetAddr As IntPtr) As IEnumerable(Of HeapAllocationContainer)
            Dim heapAllocs = New List(Of HeapAllocationContainer)
            For Each heap In _HeapList
                Dim res = heap.GetPivot(dwTargetAddr)
                heapAllocs.AddRange(res)
            Next

            Return heapAllocs
        End Function

        ''' <summary>
        ''' process start and wait til done
        ''' </summary>
        ''' <param name="strCmd"></param>
        ''' <remarks></remarks>
        Public Sub StartProcess(ByVal strCmd As String, Optional ByVal args As String = Nothing)
            Dim pstartinfo = New ProcessStartInfo(strCmd)
            If args IsNot Nothing Then
                pstartinfo.Arguments = args
            End If
            Dim hproc = Process.Start(pstartinfo)

            Dim ncnt = 0
            Do While Not hproc.HasExited AndAlso ncnt < 360  '  secs
                System.Threading.Thread.Sleep(1000) ' 1 sec
                hproc.Refresh()
                ncnt += 1
            Loop
            If Not hproc.HasExited Then
                Throw New InvalidOperationException("waiting for process to finish" + strCmd)
            End If
        End Sub

        Public Sub DeployMemspectFiles(
                                      ByVal srcFolder As String,
                                      ByVal targFolder As String,
                                      Optional ByVal fIncludeAll As Boolean = False,
                                      Optional ByVal fOnlyIfNewer As Boolean = True)
            ' copy memspect files to wrkdir
#If False Then
        these are auto deployed
                    "MemSpect.Exe",
            "MemSpect.pdb",
            "FastSerialization.dll",
            "FastSerialization.pdb",

#End If
            Dim memspectFiles As New List(Of String) From {
                MemSpectDllName,
                IO.Path.ChangeExtension(MemSpectDllName, "pdb"),
                MemSpectIniName,
                CodeMarkerGuidLookupFilename,
                "dbghelp.dll",
                "symsrv.dll",
                "srcsrv.dll",
                "srcsrv.ini",
                "ibcmerge.exe",
                "MapFileDict.dll",
                "MapFileDict.pdb",
                "withdll.exe",
                "msvcr120.dll",
                "msvcp120.dll",
                "MemSpectBase.dll",
                "MemSpectBase.pdb",
                "TraceEvent.dll"
            }
            If fIncludeAll Then
                memspectFiles.Add("MemSpect.exe")
                memspectFiles.Add("MemSpect.pdb")
                memspectFiles.Add("FastSerialization.dll")
                memspectFiles.Add("FastSerialization.pdb")
            End If
            '            "srcsrv.dll",

            memspectFiles.ForEach(Sub(file As String)
                                      Dim srcFullname = IO.Path.Combine(srcFolder, file)
                                      Dim trgFullName = IO.Path.Combine(targFolder, file)
                                      CopyFileIfNewer(srcFullname, trgFullName, fOnlyIfNewer)
                                  End Sub)
        End Sub

        Public Function CopyFileIfNewer(ByVal srcFullname As String, ByVal trgFullname As String, Optional ByVal fOnlyIfNewer As Boolean = False) As Boolean
            Dim fCopy = False
            If IO.File.Exists(trgFullname) Then
                Dim finfoSrc = New IO.FileInfo(srcFullname)
                Dim finfoTrg = New IO.FileInfo(trgFullname)
                If fOnlyIfNewer Then ' check timestamp
                    If finfoTrg.LastWriteTime < finfoSrc.LastWriteTime Then
                        fCopy = True
                    End If
                Else
                    fCopy = True
                End If
                If fCopy Then
                    finfoTrg.Attributes = FileAttributes.Normal  ' in case readonly
                    Try
                        IO.File.Delete(trgFullname) ' delete old
                    Catch ex As Exception
                        ' dev11 requires all to be deployed cuz test.dll runs in bin\debug, rather than deployment folder, so ignore exception
                        fCopy = False
                    End Try
                End If
            Else
                fCopy = True
            End If
            If fCopy Then
                IO.File.Copy(srcFullname, trgFullname)
            End If
            Return fCopy
        End Function

        Public Function IsRunningAsAdmin() As Boolean
            Dim fIsAdmin = False
            Dim ident = System.Security.Principal.WindowsIdentity.GetCurrent
            Dim princ = New System.Security.Principal.WindowsPrincipal(ident)
            fIsAdmin = princ.IsInRole(System.Security.Principal.WindowsBuiltInRole.Administrator)
            Return fIsAdmin
        End Function

        Public Function GetPrivateProfileStringEx(ByVal keyName As String, ByVal strDefault As String) As String
            Dim sb = New StringBuilder(512)
            Dim retval = strDefault
            If GetPrivateProfileString(ProfileStringSection, keyName, strDefault, sb, sb.Capacity, _iniFileName) > 0 Then
                retval = sb.ToString
            End If
            Return retval
        End Function

        Private _DidAddAppDomainResolve As Boolean = False
        ''' <summary>
        ''' will compile and execute file in current process. 
        ''' </summary>
        ''' <param name="filename"></param>
        ''' <returns></returns>
        Public Function CompileAndExecuteFile(filename As String) As String
            Dim strResult = String.Empty
            Try

                Dim cdProvider = CodeDomProvider.CreateProvider("C#")
                Dim compileParams = New CompilerParameters
                Dim refLines = File.ReadAllLines(filename).Where(Function(l) l.StartsWith("//Ref:"))
#If False Then
            Dim asmMemSpectBase = GetType(HeapAllocationContainer).Assembly
            Dim i = 0
            Dim txt =
<xml>
// can add the fullpath to an assembly for reference like so:
//Ref: System.dll
//Ref: System.linq.dll
//Ref: System.core.dll
//Ref: <%= asmMemSpectBase.Location %>
using MemSpect;

namespace DoesntMatter
{
    public class SomeClass
    {
        public static string DoMain(string[] args)
        {
            Common.UpdateStatusMsg("Executing in dynamically generated code: In Main<%= i %>");
            var x = <%= i %>;
            var y = 100/x;
            return "did main<%= i %>";
        }

    }
}
</xml>.Value.Replace(vbLf, vbCr + vbLf)

#End If
                Dim lstRefDirs = New HashSet(Of String)
                For Each lin In refLines
                    Dim refAsm = lin.Replace("//Ref:", String.Empty).Trim
                    If refAsm.StartsWith("""") AndAlso refAsm.EndsWith("""") Then
                        refAsm = refAsm.Replace("""", "")
                    End If
                    If refAsm.StartsWith("MemSpectBase", StringComparison.InvariantCultureIgnoreCase) Then
                        refAsm = GetType(Common).Assembly.Location
                    End If
                    Dim dir = Path.GetDirectoryName(refAsm)
                    If Not String.IsNullOrEmpty(dir) Then
                        If Not File.Exists(refAsm) Then
                            UpdateStatusMsg(String.Format("Couldn't find {0}", refAsm))
                        End If
                        lstRefDirs.Add(dir)
                        'UpdateStatusMsg(String.Format("added dir '{0}', len = {1}", dir, lstRefDirs.Count))
                    End If
                    Dim Res = compileParams.ReferencedAssemblies.Add(refAsm)
                    '               UpdateStatusMsg(String.Format("Adding ref to {0}", refAsm))
                Next
#If DEBUG Then
                '                compileParams.IncludeDebugInformation = True '// unauthorized exception To write pdb ?

#End If
                'compileParams.ReferencedAssemblies.Add("c:\MemSpect\MapFileDict.dll")
                'compileParams.ReferencedAssemblies.Add("c:\MemSpect\FastSerialization.dll")
                'compileParams.ReferencedAssemblies.Add("c:\MemSpect\TraceEvent.dll")
                compileParams.GenerateInMemory = True ' in memory cannot be unloaded
                Dim resCompile = cdProvider.CompileAssemblyFromFile(
                    compileParams,
                    filename
                )
                If resCompile.Errors.HasErrors Then
                    For Each Errr As CompilerError In resCompile.Errors
                        'UpdateStatusMsg(String.Format("{0}", Errr.ToString))
                        strResult += "CompileError: " + Errr.ToString() + vbCrLf
                    Next
                Else
                    '                UpdateStatusMsg(String.Format("No Compile Errors {0}", resCompile.CompiledAssembly.Location))
                    Dim asm = resCompile.CompiledAssembly ' loads asm
                    Dim typ = asm.GetExportedTypes()
                    Dim foundIt = False
                    For Each clas In typ
                        For Each meth In clas.GetMethods
                            If meth.Name = "DoMain" AndAlso meth.IsStatic Then
                                foundIt = True
                                Dim fname = String.Empty
                                If Not _DidAddAppDomainResolve Then
                                    AddHandler AppDomain.CurrentDomain.AssemblyResolve,
                                    Function(o, e) As System.Reflection.Assembly
                                        Dim asmR As System.Reflection.Assembly = Nothing
                                        Dim requestName = e.Name.Substring(0, e.Name.IndexOf(","))
                                        For Each refDir In lstRefDirs
                                            For Each ext In {".dll", ".exe"}
                                                fname = Path.Combine(
                                                   refDir,
                                                     requestName) + ext
                                                If File.Exists(fname) Then
                                                    asmR = System.Reflection.Assembly.LoadFrom(fname)
                                                    If asmR IsNot Nothing Then
                                                        Exit For
                                                    End If
                                                End If
                                            Next
                                            If asmR IsNot Nothing Then
                                                Exit For
                                            End If
                                        Next
                                        If asmR Is Nothing Then
                                            UpdateStatusMsg(String.Format("AsmResolve event: Couldn't find {0}", e.Name))
                                        Else
                                            UpdateStatusMsg(String.Format("AsmResolve event: {0}  {1}", e.Name, fname))

                                        End If
                                        Return asmR
                                    End Function
                                End If
                                Dim res = meth.Invoke(Nothing, New Object() {New String() {}})
                                If TryCast(res, String) IsNot Nothing Then
                                    strResult = CStr(res)
                                End If
                                Exit For
                            End If
                        Next
                    Next
                    If Not foundIt Then
                        strResult += String.Format("Couldn't find method DoMain in {0}", filename)
                    End If
                End If
            Catch ex As Exception
                strResult += "Execution Exception: " + ex.ToString()
            End Try
            Return strResult
        End Function
        ''' <summary>
        ''' will compile and exeecute a file in target process
        ''' </summary>
        ''' <param name="codeFilename"></param>
        ''' <returns></returns>
        Public Function CompileAndExecuteFileInTarget(codeFilename As String) As String
            Dim strResult = String.Empty
            Debug.Assert(_ConnectionMode = MemSpectMode.OnLine)
            Dim memspectBase = GetType(Common).Assembly.Location
            SendMsg(ProcMsgVerb.CompileAndExecuteFile,
                    fSendEndMsgSync:=False,
                    stringArray:=New String() {codeFilename, memspectBase})
            Dim res = GetMsg(4)
            If res IsNot Nothing Then
                Dim len = BitConverter.ToInt32(res, 0)
                res = GetMsg(len)
                Dim enc = New ASCIIEncoding
                strResult = enc.GetString(res)
                EndMsgSync()
            Else
                strResult = "Got res = nothing"
            End If
            Return strResult
        End Function
    End Module 'common




    ' http://blogs.msdn.com/calvin_hsia/archive/2008/10/28/9020745.aspx
    Public Class DynamicDllLoader
        Implements IDisposable
        Private _HandleDll As IntPtr
        Private _fUnload As Boolean = False
        Sub New(ByVal FullPath As String, Optional ByVal fChangeDir As Boolean = False, Optional ByVal fUnload As Boolean = True)
            _fUnload = fUnload
            'must change folder to loc of MSVBIDE so VSAssert and MSVBIDEui can be found
            Dim OrigCurdir = System.IO.Directory.GetCurrentDirectory
            If fChangeDir Then
                System.IO.Directory.SetCurrentDirectory(Path.GetDirectoryName(FullPath))
            End If
            Try
                If IO.File.Exists(FullPath) Then
                    _HandleDll = LoadLibrary(FullPath)
                    If _HandleDll = IntPtr.Zero Then
                        Throw New FileNotFoundException("couldn't load " + FullPath)
                    End If
                Else
                    Throw New FileNotFoundException(FullPath)
                End If
            Catch ex As Exception
            Finally
                If fChangeDir Then
                    Directory.SetCurrentDirectory(OrigCurdir)
                End If
            End Try
        End Sub

        Sub UnLoadDll()
            If _HandleDll <> IntPtr.Zero Then
                If _fUnload Then
                    FreeLibrary(_HandleDll)
                    _HandleDll = IntPtr.Zero
                End If
            End If
        End Sub

        Private disposedValue As Boolean = False    ' To detect redundant calls

        ' IDisposable
        Protected Overridable Sub Dispose(ByVal disposing As Boolean)
            If Not Me.disposedValue Then
                UnLoadDll()
            End If
            Me.disposedValue = True
        End Sub

#Region " IDisposable Support "
        ' This code added by Visual Basic to correctly implement the disposable pattern.
        Public Sub Dispose() Implements IDisposable.Dispose
            ' Do not change this code.  Put cleanup code in Dispose(ByVal disposing As Boolean) above.
            Dispose(True)
            GC.SuppressFinalize(Me)
        End Sub
#End Region

    End Class

    Public Module ProcComm
        Public _pipestreamToTarget As NamedPipeClientStream ' pipe to send cmds to target proc (send to DevEnv)
        Public _pipestreamToChild As NamedPipeServerStream ' pipe to receive cmds from target
        Public _eventSndMsgFromChild As EventWaitHandle ' indicate a msg from child to parent is ready
        Public _eventSndMsgFromParent As EventWaitHandle ' indicate a msg from parent to child is ready
        Public _hDebugHeap As IntPtr ' the handle of the priv Debug heap that VSAssert uses: holds stack traces and stuff
        Public _hProcessHeap As IntPtr ' the handle of the process heap in the target process
        Public _hMemSpectHeap As IntPtr ' the handle of the MemSpect Heap 
        Public _MEMORYSTATUSEX As MEMORYSTATUSEX ' from the target process
#If DEBUG Then
        Public _NamedPipeTimeout As Integer = 12000 ' msecs
#Else
        Public _NamedPipeTimeout As Integer = 3000 ' msecs
#End If

        Private _byteEncoding As New UTF8Encoding
        Private _decoder As Decoder = _byteEncoding.GetDecoder

        Public _isCommInitialized As Boolean = False

        Public ReadOnly Property _isTargetFrozen As Boolean
            Get
                Dim isfrozen = True

                If _ConnectionMode = MemSpectMode.OnLine Then
                    If _Addressof_g_pCDebFreezeAllThreads <> IntPtr.Zero Then
                        Dim val = ReadProcessMemoryDWORDEx(_Addressof_g_pCDebFreezeAllThreads)
                        If val = 0 Then
                            isfrozen = False
                        End If
                    End If
                End If
                Return isfrozen
            End Get
        End Property

        Public Event TargetFrozen()
        Public Event TargetUnfrozen()


        Function GetHelpText() As String
            Dim hlpstr =
    <xml>
MemSpect.exe is the UI part of the MemSpect tool http://calvinh6/MemSpect
If 1st param is "/"
    "m" means the 2nd param is the path to a Minidump (like a Watson dump)
            Example: MemSpect /m mydump.dmp
            if 2nd param is omitted, will OpenFileDialog
    "o" means the 2nd param is the path to a MegaSnapshot offline dump. 
            Example: MemSpect /o MemSpectDump0
        if 2nd param is omitted, will BrowseforFolder

    "p" means browse for process to which to attach (it must have been started with the MemSpect env vars)

    "x" means attach to eXisting process which was not started under MemSpect (no intercepted calls)
        2nd param is pid

    "d" means create a MiniDump of an existing process which was not started under MemSpect
        2nd param is pid

    "c" means create an offline snapshot using a process id and output folder
            Example: MemSpect /c 1234 c:\OfflineSnapshot
        Parameters can be specified in any order or skipped.  Any skipped parameters will display UI to obtain them.


Else
    if 1st param starts with integer, then it's the PID of the process that was started under Memspect
    else it's a file name. If it ends in "mhd" or if appending DumpedFiles.Mhd and it exists, opens the
    file as an offline snapshot
End If 


</xml>


            '            MessageBox.Show(hlpstr.Value)
            Return hlpstr.Value
        End Function

        ''' <summary>
        ''' returns '32', '64', or 'Exception message', like "Access Denied"
        ''' </summary>
        Public Function ProcessType(ByVal proc As Process) As String
            Dim is32 = False
            If IntPtr.Size = 4 Then ' 64 bit=8, 32=4
                is32 = True
            End If
            Dim retProcType = "64"
            Try
                Dim IsrunningUnderWow64 = False
                Try
                    ' if machine is 32 bit, then all procs are 32 bit
                    If IsWow64Process(GetCurrentProcess(), IsrunningUnderWow64) AndAlso IsrunningUnderWow64 Then
                        ' CurOs = "64"
                        If IsWow64Process(proc.Handle, IsrunningUnderWow64) AndAlso IsrunningUnderWow64 Then
                            retProcType = "32"
                        Else
                            retProcType = "64"

                        End If
                    Else
                        retProcType = "32"
                    End If
                Catch ex As Exception
                    retProcType = ex.Message ' default to 32 so 
                End Try

            Catch ex As Exception
                retProcType = ex.Message
            End Try
            Return retProcType
        End Function

        Public Function AreArgumentsValid(ByVal args() As String) As Boolean
            Dim retVal As String = String.Empty
            Return ProcessArguments(args, retVal, True)
        End Function

        Private Function AreImmersiveArgs(ByVal args() As String) As Boolean
            '// Immersive launches strCmdLine process like so "c:\program files\memspect\memspect.exe" -p 2816 -tid 56
            'len=5,  0 = EXE, 1 = "-p", 3 - "-tid"
            If args.Length = 5 AndAlso args(1) = "-p" AndAlso args(3) = "-tid" Then
                Return True
            End If
            Return False
        End Function

        Private Function ProcessArguments(ByVal args() As String, ByRef resultString As String, Optional ByVal validateOnly As Boolean = False) As Boolean
            resultString = String.Empty

            If args.Length < 2 Then
                Return False
            End If
            If AreImmersiveArgs(args) Then
                Return True
            End If
            If args(1).StartsWith("/") Or args(1).StartsWith("-") Or Not Char.IsDigit(args(1)(0)) Then
                If args(1).Length > 1 Then
                    If "/-".IndexOf(args(1)(0)) >= 0 Then
                        Select Case Char.ToLower(args(1)(1))
                            Case "m"c
                                If args.Count > 2 Then
                                    If Not File.Exists(args(2)) Then
                                        Return False
                                    End If
                                Else
                                    Return False
                                End If

                                If Not validateOnly Then
                                    _ConnectionMode = MemSpectMode.MiniDumpOnly
                                    _IsClientOutOfProcess = False

                                    _MiniDumpFileName = args(2)
                                    If Not _IsUnderTest Then
                                        If Not Path.GetExtension(_MiniDumpFileName).ToLower().StartsWith(".mhd") AndAlso
                                            File.Exists(Path.Combine(_MiniDumpFileName, DumpedFilesMHDFileName)) Then
                                            If MsgBox("Are you sure you want to open a " + _MiniDumpFileName + "? The same folder contains " + DumpedFilesMHDFileName + " which has much more info like stacks",
                                                   MsgBoxStyle.YesNo Or MsgBoxStyle.Question, "MemSpect") <> MsgBoxResult.Yes Then
                                                Return False
                                            End If
                                        End If
                                    End If
                                    UpdateStatusMsg("Minidump " + args(2))
                                End If

                                Return True
                            Case "o"c
                                Dim pathtry = args(2)
                                If args.Count > 2 Then
                                    If args(2).ToLower.EndsWith(".zip") Then

                                    Else
                                        If Not Directory.Exists(args(2)) Then
                                            pathtry = IO.Path.GetDirectoryName(args(2))
                                            If Not Directory.Exists(pathtry) Then
                                                MsgBox("Path not found: " + pathtry)
                                                Return False
                                            End If
                                            Dim dmpdFiles = Path.Combine(pathtry, DumpedFilesMHDFileName)
                                            If Not File.Exists(dmpdFiles) Then
                                                MsgBox("File not found: " + dmpdFiles + " " + args(2))
                                                Return False
                                            End If
                                        End If

                                    End If
                                Else
                                    Return False
                                End If

                                If Not validateOnly Then
                                    Dim fileinfo = New FileInfo(pathtry) ' make a full path
                                    pathtry = fileinfo.FullName
                                    Common.OfflineMegaSnapshot.LoadMegaSnapshot(fWaitTilDone:=_IsUnderTest, snapName:=pathtry)
                                End If

                                Return True
                            Case "p"c
                                '/p launches the chooser, so it can't be used here since it is UI-specific
                                Return False
                            Case "x"c
                                If args.Length > 2 Then
                                    _ConnectionMode = MemSpectMode.Existing
                                    _TargetProcessId = CInt(args(2))
                                    If _TargetProcessId <> 0 Then
                                        _TargetProc = Process.GetProcessById(_TargetProcessId)
                                        _hProcessTarget = _TargetProc.Handle
                                        Return True
                                    End If
                                End If
                                Return False
                            Case "d"c ' create minidump
                                Return False
                            Case "c"c
                                'need 2 arguments, the PID and the directory to dump the snapshot.
                                'arguments can come in either order. if either is omitted, UI is shown
                                'to get the argument.
                                'IE:
                                '   Memspect /c 1234 c:\OfflineSnapshot
                                '   or
                                '   Memspect /c c:\OfflineSnapshot 1234
                                Dim tempPid = 0

                                If args.Count < 4 Then
                                    Return False
                                End If

                                Dim lamCmdLineSnap = Function(pidArg As Integer, dirArg As Integer) As Boolean
                                                         If Integer.TryParse(args(pidArg), tempPid) And Directory.Exists(args(dirArg)) Then
                                                             If Not validateOnly Then
                                                                 _pathCommandlineSnapshot = args(dirArg)
                                                                 args(1) = args(pidArg)
                                                                 'connection mode online so normal communication occurs.
                                                                 _ConnectionMode = MemSpectMode.OnLine
                                                             End If

                                                             Return True
                                                         End If
                                                         Return False
                                                     End Function
                                If lamCmdLineSnap.Invoke(2, 3) Then
                                    Return True
                                End If
                                If lamCmdLineSnap.Invoke(3, 2) Then
                                    Return True
                                End If
                                MsgBox("Could not take snapshot: make sure folder exists and pipe is open")
                                Environment.Exit(1)

                                Return False
                        End Select
                    Else
                        ' user passed in a fullpath of exe to exec
                        Dim targfullpath = args(1)
                        Dim fIsDumpFile = False
                        Dim fFileExists = False
                        If File.Exists(targfullpath) Then
                            fFileExists = True
                        Else
                            Dim trypathFile = Path.Combine(targfullpath, DumpedFilesMHDFileName)
                            If File.Exists(trypathFile) Then
                                targfullpath = trypathFile
                                fFileExists = True
                                fIsDumpFile = True
                            End If
                        End If


                        If fFileExists Then
                            If Path.GetExtension(targfullpath).ToLower.EndsWith("mhd") Then
                                fIsDumpFile = True
                            End If
                        End If
                        If validateOnly Then
                            Return fFileExists
                        Else
                            targfullpath = New FileInfo(targfullpath).FullName ' full rooted path
                            ' user passed in a fullpath of exe to exec
                            If fIsDumpFile Then
                                Common.OfflineMegaSnapshot.LoadMegaSnapshot(fWaitTilDone:=_IsUnderTest, snapName:=targfullpath)
                                Return True
                            End If
                            Dim pl = New ProcessLauncher(WriteIniFile:=ProcessLauncher.WriteIniFile.WriteNothing) With {
                                ._nmsecsToWaitTilStart = 3000
                            }
                            Dim inifile = IO.Path.Combine(IO.Path.GetDirectoryName(Reflection.Assembly.GetEntryAssembly.Location), MemSpectIniName)
                            ProcessLauncher.AddProcessToJustThisProcesses(
                                targfullpath,
                                inifile
                            )
                            WritePrivateProfileString(ProfileStringSection, "StartChildProcess", "1", inifile)

                            For i = 2 To args.Length - 1
                                Dim strarg = args(i)
                                If strarg.Contains(" ") Then
                                    strarg = """" + strarg + """"
                                End If
                                pl._AdditionalCmdLineParams += If(i = 2, String.Empty, " ") + strarg
                            Next
                            If targfullpath.ToLower.EndsWith("exe") Then
                                pl.LaunchTargProc(targfullpath, fWithDll:=True, fDoInitcomm:=False)
                                Common._ConnectionMode = MemSpectMode.QuitMemspect 'end program!
                            Else
                                Return False
                            End If

                            Return True
                        End If

                    End If
                End If 'args(1).Length > 1 

            Else
                'argument is just the pid
                'no need to set anything here since InitComm assumes args(1) has the pid
                Dim tempPid = 0
                If Integer.TryParse(args(1), tempPid) And tempPid <> 0 Then
                    Return True
                Else
                    resultString = GetHelpText()
                    Return False
                End If
            End If

            Return False
        End Function

        Public Sub ConsoleStatusMessageEventhandler(ByVal sender As Object, ByVal e As StatusMessageEventArgs)
            Dim defaultColor = System.Console.ForegroundColor

            Select Case e.MsgType
                Case StatusMessageType.LogEntry
                    System.Console.WriteLine("[Memspect Entry] " + e.Message)
                    'Case StatusMessageType.Warning
                    '    System.Console.ForegroundColor = ConsoleColor.Yellow
                    '    System.Console.WriteLine("[Memspect Warning] " + e.Message)
                    '    System.Console.ForegroundColor = defaultColor
                    'Case StatusMessageType.Failure
                    '    System.Console.ForegroundColor = ConsoleColor.Red
                    '    System.Console.WriteLine("Memspect Failure] " + e.Message)
                    '    System.Console.ForegroundColor = defaultColor
                Case StatusMessageType.AlertMsgBox
                    System.Console.ForegroundColor = ConsoleColor.Blue
                    System.Console.WriteLine("[Memspect Alert] " + e.Message)
                    System.Console.ForegroundColor = defaultColor
            End Select

        End Sub

        ''' <summary>
        ''' Tries to initiate communication between current process and target process(the 1 which we want to track mem)
        ''' returns empty string on success or string of failure message
        ''' </summary>
        ''' <param name="args"></param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Function InitComm(ByVal args() As String) As String
            Dim pid As Integer = 0

            AddHandler StatusMessageEvent, AddressOf ConsoleStatusMessageEventhandler
            UpdateStatusMsg(String.Format("MemSpect API connecting {0} CmdLine= {1}",
                                          DateTime.Now.ToString("F"),
                                          Environment.CommandLine)
                                      ) ' F: Sunday, June 15, 2008 9:15:07 PM 
            Dim retval = String.Empty

            If Not Common.IsInitialized Then
                Common.Initialize() ' initialize common, not communication
            End If
            _IsInMiddleOfMessaging = ProcMsgVerb.UnKnownVerb


            If AreImmersiveArgs(args) Then
                ' Immersive apps have very limited access rights
                ' First, User launches MemSpect.exe (Process #1, p#1), which shows a launcherUI control window
                ' User points LaunchUI to an app manifest
                ' Then user hits Start. p#1 then enables debugging (which copies MemSpect to a dir that's accessible) and 
                '       launches the immersive P#2 in suspend mode and MemSpect.exe again as the debugger (p#3) with args like
                '               "c:\memspect\memspect.exe" -p 2816 -tid 56
                ' We now want p#3 to Inject MemSpectDll into the immersive process 

                ' We have the original MemSpect install dir: We need to find the package dir, write the TID to it's ini file
                ' so we write the fullpathManifest to the original MemSpect.ini file
                pid = Integer.Parse(args(2))
                Dim tid = Integer.Parse(args(4))
                Dim hProc = Process.GetProcessById(pid)
                Dim mspectExeName = Reflection.Assembly.GetEntryAssembly().Location ' MemSpect.exe' in <MemSpectInstalldir>
                Dim strBldr = New Text.StringBuilder(MAX_PATH)
                Dim inifileInstallDir = IO.Path.Combine(IO.Path.GetDirectoryName(mspectExeName), MemSpectIniName)

                Dim fDebuggingMode = If(GetPrivateProfileInt(ProfileStringSection, "fMessageBoxOnStart", 0, inifileInstallDir) = 0, False, True)
                If fDebuggingMode Then
                    MsgBox("Immersive MemSpect Debugging mode. Attach a debugger to ProcessId = " + Process.GetCurrentProcess.Id.ToString)
                End If

                GetPrivateProfileString(ProfileStringSection, "MEMSPECT_MANIFEST_PATH", String.Empty, strBldr, strBldr.Capacity, inifileInstallDir)
                Dim manifestpath = strBldr.ToString
                If String.IsNullOrEmpty(manifestpath) Then
                    Throw New Exception("Error: got a null MEMSPECT_MANIFEST_PATH from ini file " + inifileInstallDir)
                End If

                Dim dirPackage = IO.Path.GetDirectoryName(manifestpath)

                Dim inifilePackDir = IO.Path.Combine(dirPackage, MemSpectIniName)
                WritePrivateProfileString(ProfileStringSection, "ImmersiveTID", tid.ToString, inifilePackDir)

                ': for immersive, "JustTheseProcesses" is ignored //WritePrivateProfileString(ProfileStringSection, "JustTheseProcesses", "*", inifilePackDir) ' enable all 

                Dim msppath = IO.Path.Combine(dirPackage, MemSpectDllName)

                If fDebuggingMode Then
                    MsgBox("About to inject thread Tid=" + tid.ToString + " " + msppath)
                End If
                retval = ProcessLauncher.InjectIntoProcess(hProc, msppath, fDebuggingMode)
                If Not String.IsNullOrEmpty(retval) Then
                    MsgBox(retval)
                End If
                _ConnectionMode = MemSpectMode.QuitMemspect ' terminate this process
                Return retval
            End If


            If Not ProcessArguments(args, retval, validateOnly:=False) Then
                Return GetHelpText()
            End If


            If _ConnectionMode = MemSpectMode.MiniDumpOnly Or
               _ConnectionMode = MemSpectMode.Offline Or
               _ConnectionMode = MemSpectMode.QuitMemspect Then
                SendTelemetry(String.Empty)
                Return retval
            End If

            If _ConnectionMode = MemSpectMode.Existing Then
                Return retval
            End If

            If retval = GetHelpText() Then
                Return retval
            End If

            pid = CInt(args(1))

            '_ConnectionMode is set to OnLine when we call Common.Initialize() and doesn't need to be set again.  If it is,
            'then it will potentially override connection modes previously set based on commandline arguments.
            '_ConnectionMode = MemSpectMode.OnLine

            _IsClientOutOfProcess = True
            _TargetProcessId = pid
            Try
                Dim IsImmersiveApp = False
                Dim immersivePrefixPipes = String.Empty ' pipes
                Dim immersivePrefixEvents = String.Empty ' events/shared mem
                Dim hProc = Process.GetProcessById(pid)
                Dim PackageFamilyName = New Text.StringBuilder(MAX_PATH)
                Dim AppContainerNamedObjectPath = New Text.StringBuilder(MAX_PATH)
                Dim hr = GetPackageFamilyNameFromProcess(hProc.Handle, PackageFamilyName)
                If hr = 0 AndAlso Not String.IsNullOrEmpty(PackageFamilyName.ToString) Then
                    hr = GetPathOfAppContainerNamedObject(PackageFamilyName.ToString, AppContainerNamedObjectPath)
                    If hr <> 0 Then
                        Throw New Exception("couldn't get AppContainerNamedObjectPath for PackageFamilyName= " + PackageFamilyName.ToString)
                    End If
                    IsImmersiveApp = True
                    Dim SessionNo = GetPrivateProfileInt(ProfileStringSection, "ImmersiveSession", 2, _iniFileName)
                    'Sessions\2\AppContainerNamedObjects\S-1-15-2-2230492263-497787423-4065611893-1016377898-965005438-4034910760-1470024513\MemSpect
                    immersivePrefixPipes = String.Format("Sessions\{0}\{1}\", SessionNo, AppContainerNamedObjectPath)
                    immersivePrefixEvents = immersivePrefixPipes.Replace("Sessions\", "Session\")
                    ' note the prefix is "Session" and not "Sessions" for events
                End If

                Dim lamConnect = Function(desc As String, fThrowOnError As Boolean, act As Action) As String
                                     Dim errConnect = String.Empty
                                     Try
                                         UpdateStatusMsg(desc)
                                         act()
                                     Catch ex As Exception
                                         errConnect = "Error " + desc + vbCrLf + ex.ToString
                                         UpdateStatusMsg(errConnect)
                                         If fThrowOnError Then
                                             Throw New InvalidOperationException(errConnect)
                                         End If
                                     End Try
                                     Return errConnect
                                 End Function

                Dim pipenameToTarget = immersivePrefixPipes + "MemSpectPipe" + _TargetProcessId.ToString

                lamConnect("Creating NamedPipeClientStream '" + pipenameToTarget + "'",
                           fThrowOnError:=True,
                           act:=Sub()
                                    _pipestreamToTarget = New NamedPipeClientStream(".", pipenameToTarget,
                                                                         PipeDirection.InOut,
                                                                         PipeOptions.None)

                                End Sub
                           )


                Dim myPipeSecurity = New PipeSecurity()
                Dim usersPipeAccessRule = New PipeAccessRule(
                    New SecurityIdentifier(WellKnownSidType.BuiltinUsersSid, Nothing),
                    PipeAccessRights.ReadWrite,
                    System.Security.AccessControl.AccessControlType.Allow) ' // allow local users connect
                myPipeSecurity.AddAccessRule(usersPipeAccessRule)


                ' this one requirerd if we start PerfWatson2.exe as user
                Dim usersPipeAccessRuleWorld = New PipeAccessRule(
                    New SecurityIdentifier(WellKnownSidType.WorldSid, Nothing),
                    PipeAccessRights.ReadWrite,
                    System.Security.AccessControl.AccessControlType.Allow) '; // allow local users connect
                myPipeSecurity.AddAccessRule(usersPipeAccessRuleWorld) ';
                Dim pipenameToChild = pipenameToTarget + "two"



                lamConnect("Creating NamedPipeServerStreamToChild +'" + pipenameToChild + "'",
                           fThrowOnError:=True,
                           act:=Sub()
                                    _pipestreamToChild = New NamedPipeServerStream(pipenameToChild,
                                                                                 PipeDirection.InOut,
                                                                                 maxNumberOfServerInstances:=1,
                                                                                 transmissionMode:=PipeTransmissionMode.Message,
                                                                                  options:=PipeOptions.None,
                                                                                  inBufferSize:=1024,
                                                                                  outBufferSize:=1024,
                                                                                  pipeSecurity:=myPipeSecurity
                                                                                  )
                                End Sub
                           )

                Dim eventName = immersivePrefixEvents + "MemSpectFromChild" + _TargetProcessId.ToString

                Dim tryConnectFailMsg = lamConnect("Connecting to FromChild Event '" + eventName + "'",
                                               fThrowOnError:=String.IsNullOrEmpty(immersivePrefixEvents),
                                               act:=Sub() _eventSndMsgFromChild = ManualResetEvent.OpenExisting(eventName))
                If Not String.IsNullOrEmpty(tryConnectFailMsg) Then
                    If String.IsNullOrEmpty(immersivePrefixEvents) Then
                        _strGlobal = "Global\" ' services need this
                        UpdateStatusMsg("Setting _strGlobal = " + _strGlobal)
                        Dim retryMsg = "Retry: Couldn't connect to '" + eventName + "'"
                        eventName = _strGlobal + eventName
                        retryMsg += ", so now trying to connect to '" + eventName + "'"
                        tryConnectFailMsg = lamConnect(retryMsg,
                                                   fThrowOnError:=True,
                                                   act:=Sub() _eventSndMsgFromChild = ManualResetEvent.OpenExisting(eventName))
                    Else
                        Throw New InvalidOperationException(tryConnectFailMsg)
                    End If
                End If

                Dim eventnameMsgFromTarget = _strGlobal + immersivePrefixEvents + "MemSpectFromTarget" + _TargetProcessId.ToString
                lamConnect("Connecting to FromTarget Event '" + eventnameMsgFromTarget + "'",
                           fThrowOnError:=True,
                           act:=Sub() _eventSndMsgFromParent = ManualResetEvent.OpenExisting(eventnameMsgFromTarget))

                lamConnect("Connecting to ToTarget pipe '" + pipenameToTarget + "'",
                           fThrowOnError:=True,
                           act:=Sub() _pipestreamToTarget.Connect(timeout:=10000))
                If Not _pipestreamToTarget.IsConnected Then
                    Throw New InvalidOperationException("Connection to pipe " + pipenameToTarget + " named pipe timed out.")
                End If

                Dim resultStr = SendMsg(ProcMsgVerb.GetSharedMem, fSendEndMsgSync:=True) ' this will cause pipe to child to be created
                If resultStr.Length <> 1 + 4 * 19 Then
                    UpdateStatusMsg("getSharedmemlen wrong " +
                                    resultStr.Length.ToString,
                                    fAssert:=True,
                                    msgType:=StatusMessageType.AlertMsgBox,
                                    msgPriority:=StatusMessagePriority.Normal)
                End If

                _TargetProc = Process.GetProcessById(_TargetProcessId)
                _hProcessTarget = _TargetProc.Handle

                Dim hFileMapping = New IntPtr(BitConverter.ToInt32(resultStr, 1))
                _SharedMemSize = BitConverter.ToInt32(resultStr, 5)

                hFileMapping = OpenFileMapping(FILE_MAP_READ Or FILE_MAP_WRITE, 0, _strGlobal + immersivePrefixEvents + "MemSpectFileMapping" + _TargetProcessId.ToString)
                Debug.Assert(hFileMapping <> IntPtr.Zero, "got null filemapping")
                _SharedMemAddr = MapViewOfFile(CType(hFileMapping, IntPtr), FILE_MAP_READ Or FILE_MAP_WRITE, 0, 0, 0)
                Debug.Assert(_SharedMemAddr <> IntPtr.Zero, "got null _SharedMemAddr ")

                _hDebugHeap = New IntPtr(BitConverter.ToInt32(resultStr, 1 + 2 * 4))
                _hProcessHeap = New IntPtr(BitConverter.ToInt32(resultStr, 1 + 3 * 4))
                _hMemSpectHeap = New IntPtr(BitConverter.ToInt32(resultStr, 1 + 4 * 4))
                _fHandle4gigStacks = CBool(BitConverter.ToInt32(resultStr, 1 + 5 * 4))
                ' 6th = g_SeqNoperfIdleCodeMarker 
                _memSpectThreadId = BitConverter.ToInt32(resultStr, 1 + 7 * 4)
                _memSpectSharedMemAddr = New IntPtr(BitConverter.ToInt32(resultStr, 1 + 8 * 4))
                _Offset_AllocToTblk = BitConverter.ToInt32(resultStr, 1 + 9 * 4)
                'UpdateStatusMsg("memspectthread=" + _memSpectThreadId.ToString + " strlen = " + resultStr.Length.ToString)

                Dim targCmdLineAddr = New IntPtr(BitConverter.ToInt32(resultStr, 1 + 10 * 4))
                Dim targCmdLine = ReadProcessMemAsString(targCmdLineAddr)
                UpdateStatusMsg(String.Format("Target CmdLine(Addr={0:x8})={1}", targCmdLineAddr.ToInt32, targCmdLine))
                _MEMORYSTATUSEX = New MEMORYSTATUSEX
                Marshal.PtrToStructure(_SharedMemAddr, _MEMORYSTATUSEX)

                '000000007ffe0000 2 gig user
                '0000000080000000 or greater =  large addr aware 
                UpdateStatusMsg(String.Format("TotVM={0} Admin={1} ComputerName='{2}' UserName='{3}'",
                                _MEMORYSTATUSEX.ullTotalVirtual.ToString("x16"),
                                IsRunningAsAdmin().ToString,
                                Environment.GetEnvironmentVariable("Computername"),
                                Environment.GetEnvironmentVariable("UserName")
                                ))

                SendTelemetry(String.Format("&T={0}&v={1}",
                                            Path.GetFileNameWithoutExtension(_TargetProc.MainModule.FileName).ToLower,
                                            WebUtility.UrlEncode(_TargetProc.MainModule.FileVersionInfo.FileVersion)
                                            )
                                        )
                UpdateStatusMsg(String.Format("PID={0} '{1}' FileVer='{2}' ProdVer='{3}'",
                   _TargetProcessId,
                   Path.GetFileName(_TargetProc.MainModule.FileName),
                    _TargetProc.MainModule.FileVersionInfo.FileVersion,
                    _TargetProc.MainModule.FileVersionInfo.ProductVersion
                    )) ' PID=1140 'devenv.exe' FileVer='11.0.50727.1 built by: RTMREL' ProdVer='11.0.50727.1'

                _Addressof_g_pCDebFreezeAllThreads = CType(BitConverter.ToInt32(resultStr, 1 + 11 * 4), IntPtr) ' we can now (re-)connect to a frozen proc

                If _isTargetFrozen Then
                    UpdateStatusMsg("Connecting to frozen target ")
                End If

                If IsImmersiveApp Then
                    UpdateStatusMsg("PackageFamilyName= " + PackageFamilyName.ToString)
                    UpdateStatusMsg("AppContainerNamedObjectPath= " + AppContainerNamedObjectPath.ToString)
                End If

                _AddressOfulGlobalPassCount = CType((BitConverter.ToInt32(resultStr, 1 + 12 * 4)), IntPtr)

                _AddressOfnCurAllocs = CType((BitConverter.ToInt32(resultStr, 1 + 13 * 4)), IntPtr)


                _MainThread = BitConverter.ToInt32(resultStr, 1 + 14 * 4)

                Dim fIsDebuggerPresentInTarget = BitConverter.ToInt32(resultStr, 1 + 15 * 4)

                If fIsDebuggerPresentInTarget > 0 Then
                    UpdateStatusMsg("DebuggerPresent in target")
                End If
                If IsDebuggerPresent() > 0 Then
                    UpdateStatusMsg("DebuggerPresent in child")
                End If

                _AddressOfStackMemMapStats = New IntPtr(BitConverter.ToInt32(resultStr, 1 + 16 * 4))
                If _AddressOfStackMemMapStats <> IntPtr.Zero Then
                    UpdateStatusMsg("Using mapped files for stack storage (StackStorageMode=1)")
                End If

                _addressOf_g_ThreadCmdsToChildReady = New IntPtr(BitConverter.ToInt32(resultStr, 1 + 17 * 4))

                _addressOf_g_fTrackingGhosts = New IntPtr(BitConverter.ToInt32(resultStr, 1 + 18 * 4))


                SendMsg(ProcMsgVerb.GetMemSpectVersion, fSendEndMsgSync:=True)
                _MemSpectVersion = ReadSharedMemAsString()
                Dim DebugRetail = String.Empty
                Dim fVersionMatch = True
#If DEBUG Then
                DebugRetail = " UI Dbg"
                If Not _MemSpectVersion.Contains("D") Then
                    fVersionMatch = False
                End If
#Else
                If _MemSpectVersion.Contains("D") Then
                    fVersionMatch = False
                End If
#End If
                If fVersionMatch Then
                    fVersionMatch = _MemSpectVersion.Contains(_MemSpectUICurrentVersion(fIncludeDebugRetail:=False))
                End If

                UpdateStatusMsg("MemSpect DLL Version = " + _MemSpectVersion + "  ")
                If Not fVersionMatch Then
                    UpdateStatusMsg("Memspect Version Mismatch(Dbg must match too): Memspect.EXE version is " +
                                    _MemSpectUICurrentVersion() + DebugRetail +
                                    ". Version received From Target =" + _MemSpectVersion,
                                    msgType:=StatusMessageType.AlertMsgBox)
                    'MessageBox.Show("Memspect Version Mismatch:Memspect.EXE version is" + _MemSpectUICurrentVersion + ". Version received From Target =" + _MemSpectVersion)
                End If

                SendMsg(ProcMsgVerb.GetIniFile, fSendEndMsgSync:=True)

                _iniFileName = ReadSharedMemAsString()
                Debug.Assert(Not String.IsNullOrEmpty(_iniFileName), "got null ini file?")
                CheckIsDebuggingAndMsgBox()
                ' show INI file: some environments (like test, MemSpectApex or Immersive), there are multiple locations.
                UpdateStatusMsg("MemSpect config file " + _iniFileName)


                targCmdLine = ReadProcessMemAsString(targCmdLineAddr)


                _ShowAddressInCallStacks = GetPrivateProfileInt(ProfileStringSection, "ShowAddressInCallStacks", 0, _iniFileName)

                Dim fHandleCommandsToChild = GetPrivateProfileInt(ProfileStringSection, "fHandleCommandsToChild", 1, _iniFileName)
                If fHandleCommandsToChild > 0 AndAlso Not IsImmersiveApp Then ' workaround for immersive: pipe created in highbox can't be opened in lowbox: access denied.
                    ' the threadroutine gets called to resolve symbols. 
                    ' _hProcessTarget must be set first, else race condition
                    hThread = New Thread(AddressOf ThreadRoutineHandleCmdsToChild) ' start thread srv routine for pipe
                    hThread.Start()
                    lamConnect("Connecting to ToChild pipe '" + pipenameToChild + "'",
                               fThrowOnError:=True,
                               act:=Sub() _pipestreamToChild.WaitForConnection())
                    If Not _pipestreamToChild.IsConnected Then
                        Throw New InvalidOperationException("PipeStreamToChild could not connect " + pipenameToChild)
                    End If
                    '_pipestreamToChild.WriteTimeout = 4000 ' timeouts are not supported on this stream
                    '_pipestreamToChild.ReadTimeout = 4000

                End If


                'Dim COR_ENABLE_PROFILING = Environment.GetEnvironmentVariable("COR_ENABLE_PROFILING")
                'Dim COR_PROFILER = Environment.GetEnvironmentVariable("COR_PROFILER")
                'Dim COR_PROFILER_PATH = Environment.GetEnvironmentVariable("COR_PROFILER_PATH") ' occurs when prism
                '' these will be NULL if not exist
                'If Not File.Exists(COR_PROFILER_PATH) OrElse
                '    String.IsNullOrEmpty(COR_ENABLE_PROFILING) OrElse
                '    COR_ENABLE_PROFILING <> "1" OrElse
                '    String.IsNullOrEmpty(COR_PROFILER) OrElse
                '    COR_PROFILER <> "{01673DDC-46F5-454F-84BC-F2F34564C2AD}" Then

                '    UpdateStatusMsg("Error: MemSpectDll.dll COR_PROFILER_PATH not found or enabled= " + COR_PROFILER_PATH)
                '    UpdateStatusMsg("Are you running under Prism? (check system env vars for COR_PROFILER_PATH)")
                'End If

                Dim initEatLowMem = GetPrivateProfileInt(ProfileStringSection, "EatLowMem", 0, _iniFileName)
                If initEatLowMem > 0 Then ' if user wants to eat lots of mem
                    UpdateStatusMsg(String.Format(
                                    "EatLowMem option will VM Reserve multiple {0} byte chunks of memory @SeqNo={1:n0}",
                                    initEatLowMem,
                                    GetGlobalPassCount)
                                )

                    Dim fHandle4gigStacks = GetPrivateProfileInt(ProfileStringSection, "fHandle4gigStacks", 0, _iniFileName)
                    If fHandle4gigStacks = 0 Then
                        UpdateStatusMsg(String.Format("fHandle4gigStacks is not on!!!!!!!"))
                    End If

                    Common.MemoryEater.EatSomeMemory(initEatLowMem, f2GigLimit:=True, fTopDown:=False, fEatAll:=True, fCommit:=False)
                    UpdateStatusMsg(String.Format("Memory eater ate {0}*{1} = {2:n0} bytes of memory @Seqno={3:n0}",
                                                              MemoryEater._EatenAllocations.Count,
                                                              initEatLowMem,
                                                              MemoryEater._EatenAllocations.Count * initEatLowMem,
                                                              GetGlobalPassCount
                                                              )
                                                          )
                End If
                Dim sbTemp As New StringBuilder(250)
                GetPrivateProfileString(ProfileStringSection, "UseChildProcessForSymbols", "1", sbTemp, sbTemp.Capacity, _iniFileName)
                _UseChildProcessForSymbols = If(CInt(sbTemp.ToString) > 0, True, False)

                GetPrivateProfileString(ProfileStringSection, "CleanUpRCWs", "1", sbTemp, sbTemp.Capacity, _iniFileName)
                UpdateStatusMsg("CleanUpRCWs = " + sbTemp.ToString)

                _TargetProc.EnableRaisingEvents = True

                _disp = Dispatcher.CurrentDispatcher

                AddHandler _TargetProc.Exited, AddressOf OnParentProcessDone

                If GetPrivateProfileInt(ProfileStringSection, "FreezeAtStartup", 0, _iniFileName) = 1 Then
                    ProcComm.FreezeTarget()
                End If
                'Try
                '    _TargetProc.WaitForInputIdle()
                'Catch ex As Exception
                'End Try
            Catch ex As Exception
                Return "Error connecting to MemSpect target process (Pid= " + pid.ToString + ")" + vbCrLf +
                   "Possibly because " + MemSpectDllName + " not injected into target" + vbCrLf +
                   "see http://calvinh6/MemSpect FAQ page for more info" + vbCrLf +
                    ex.ToString()
            End Try

            'if we are doing a commandline snapshot, freeze the threads, create the snapshot, unfreeze and then quit.
            If Not String.IsNullOrEmpty(_pathCommandlineSnapshot) Then
                ProcComm.FreezeTarget()
                Common.OfflineMegaSnapshot.CreateMegaSnapshot(_pathCommandlineSnapshot)
                ProcComm.UnFreezeTarget()
                ProcComm.SendMsg(ProcMsgVerb.Quit, fSendEndMsgSync:=False, dwords:={0}) ' quit but leave parent running
                'switch connection mode to quit memspect
                _ConnectionMode = MemSpectMode.QuitMemspect
            Else
                Dim actions = GetCodeMarkerActions()
                For Each action In actions
                    UpdateStatusMsg(" CodeMarkerAction " + action.ToString)
                Next
                _isCommInitialized = True
                FireCustomCodeMarkerEvent("MemSpectConnected", CodeMarkerEventType.None, dwDepthLevel:=0, dwMarkerId:=100001)
            End If
            Return String.Empty 'success
        End Function

        Public Sub CloseComm(Optional ByVal fIgnoreFreezeState As Boolean = False)
            If Not _isCommInitialized Then
                Return
            End If

            RemoveHandler StatusMessageEvent, AddressOf ConsoleStatusMessageEventhandler
            If Not TargProcHasExited() Then
                Try
                    If _GhostData.IsTracking Then
                        _GhostData.StopTracking()
                    End If
                    If _ConnectionMode = MemSpectMode.OnLine AndAlso _pipestreamToTarget IsNot Nothing AndAlso _pipestreamToTarget.IsConnected Then
                        Dim arg = If(fIgnoreFreezeState, 2, 0)
                        SendMsg(ProcMsgVerb.Quit, fSendEndMsgSync:=False, dwords:={arg}) ' quit but leave parent proc running
                    End If
                Catch ex As Exception

                End Try
            End If

            threadRoutineDone = True
            If Not hThread Is Nothing Then
                hThread.Join() ' wait for thread routine to finish
            End If

            If _eventSndMsgFromParent IsNot Nothing Then
                _eventSndMsgFromParent.Close()
            End If
            If _eventSndMsgFromChild IsNot Nothing Then
                _eventSndMsgFromChild.Close()
            End If
            If _pipestreamToTarget IsNot Nothing Then
                _pipestreamToTarget.Close()
                _pipestreamToTarget.Dispose()
                _pipestreamToTarget = Nothing
            End If
            If _pipestreamToChild IsNot Nothing Then
                _pipestreamToChild.Close()
                _pipestreamToChild.Dispose()
                _pipestreamToChild = Nothing
            End If

            _isCommInitialized = False
        End Sub

        Friend hThread As System.Threading.Thread
        Friend threadRoutineDone As Boolean = False
        Friend _noopCnt As Integer = 0

        Public Event ReceivedCodeMarkerEvent(ByVal e As CodeMarkerEventArgs) ' from GotCodemarker
        Public Class CodeMarkerEventArgs
            Inherits EventArgs
            Public Property ReceivedMarkerId As Integer
            Public Property SeqNo As Integer

            Public Property dwAction As CodeMarkerActionEnum
            Public Property nInstanceNum As Integer ' the instance, starting with 1 of this marker occurrence

            Public Property Handled As Boolean

        End Class


        Public Function IsGhostEnabledAtStart() As Boolean
            Dim trackingGhost = GetPrivateProfileInt(ProfileStringSection, "TrackGhost", 0, _iniFileName)
            Return trackingGhost <> 0
        End Function
        ' needs to be done after all interproc commmunication to avoid deadlocks
        Public Function GhostEnableStartTrackingGhost() As Boolean
            Dim trackingGhost = IsGhostEnabledAtStart()
            If trackingGhost Then
                _GhostData.StartTracking()
                'WriteProcessMemory(_hProcessTarget, _addressOf_g_fTrackingGhosts, New Byte() {1, 0, 0, 0}, 4, Nothing) ' indicate we're tracking ghosts
            End If
            Return trackingGhost
        End Function

        Friend Sub ThreadRoutineHandleCmdsToChild(ByVal o As Object) ' handles cmds from target proc to child
            WriteProcessMemory(_hProcessTarget, _addressOf_g_ThreadCmdsToChildReady, New Byte() {1, 0, 0, 0}, 4, Nothing) ' indicate we're ready for listening

            threadRoutineDone = False
            Do While Not threadRoutineDone
                Dim res = _eventSndMsgFromParent.WaitOne(500)
                If res Then
                    Try
                        Dim bArray = GetMsg(nMaxSize:=100, strmToUse:=_pipestreamToChild)
                        If bArray.Length > 0 Then
                            Dim vrb = CType(bArray(0), ProcMsgVerb)
                            Select Case vrb
                                Case ProcMsgVerb.ResolveSymbolFromTarg
                                    Debug.WriteLine(vrb.ToString)
                                    Dim fincludeLineNo = BitConverter.ToInt32(bArray, 1)
                                    Dim addr = BitConverter.ToInt32(bArray, 5)
                                    Dim sym = ResolveAddressToSymbol(CType(addr, IntPtr), fStripFileName:=fincludeLineNo = 0)
                                    'Dim sym = "foobar"
                                    Dim bytes = (New Text.ASCIIEncoding).GetBytes(sym)
                                    Dim buf_Len = BitConverter.GetBytes(sym.Length)
                                    _pipestreamToChild.Write(buf_Len, 0, 4)
                                    _pipestreamToChild.Write(bytes, 0, sym.Length)
                                    'Debug.WriteLine(vrb.ToString + " " + addr.ToString("x8") + " " + sym)
                                    'UpdateStatusMsg("SymRes " + sym)
                                Case ProcMsgVerb.GotCodemarker
                                    Dim nCodeMarkerId = BitConverter.ToInt32(bArray, 1)
                                    Dim cmSeqNo = BitConverter.ToInt32(bArray, 5)
                                    Dim dwExtraInfo = BitConverter.ToInt32(bArray, 9)
                                    Dim nInstanceNum = BitConverter.ToInt32(bArray, 13)
                                    Dim evargs = New CodeMarkerEventArgs With {
                                            .ReceivedMarkerId = nCodeMarkerId,
                                            .SeqNo = cmSeqNo,
                                            .dwAction = CType(dwExtraInfo, CodeMarkerActionEnum),
                                            .nInstanceNum = nInstanceNum,
                                            .Handled = False
                                        }
                                    UpdateStatusMsg("GotCodemarker from parent " + nCodeMarkerId.ToString)
                                    RaiseEvent ReceivedCodeMarkerEvent(evargs)
                                    If Not evargs.Handled Then
                                        If nCodeMarkerId = _PerfIdleCodeMarker Then
                                            _PerfIdleSeqNo = cmSeqNo
                                            UpdateStatusMsg("PerfIdle " + _PerfIdleSeqNo.ToString("n0"), msgPriority:=StatusMessagePriority.Low)
                                        End If
                                        If (dwExtraInfo And CodeMarkerActionEnum.CodeMarkerAction_TakeMemStatSnapShot) > 0 Then


                                        End If
                                        If (dwExtraInfo And CodeMarkerActionEnum.CodeMarkerAction_ShowInStatusMessage) <> 0 Then
                                            UpdateStatusMsg("CodeMarker:" +
                                                            nCodeMarkerId.ToString + " " +
                                                            GetCodeMarkerNameRaw(nCodeMarkerId, fuseOffline:=True) +
                                                            " SeqNo=" +
                                                            cmSeqNo.ToString("n0"), msgPriority:=StatusMessagePriority.Low)

                                        End If

                                        If (dwExtraInfo And CodeMarkerActionEnum.CodeMarkerAction_Freeze) <> 0 Then
                                            ' need to disable timer
                                            ProcComm.FreezeTarget(nCleanupRCWs:=0) ' don't cleanup rcw: deadlock: also VS has already called it
                                            UpdateStatusMsg("CodeMarkersAtWhichToFreeze:" +
                                                            GetCodeMarkerNameRaw(nCodeMarkerId, fuseOffline:=True) +
                                                            " SeqNo=" +
                                                            cmSeqNo.ToString("n0"), msgPriority:=StatusMessagePriority.Low)
                                        End If

                                        If (dwExtraInfo And CodeMarkerActionEnum.CodeMarkerAction_TakeMegaSnapShot) <> 0 Then
                                            ProcComm.FreezeTarget(nCleanupRCWs:=0) ' don't cleanup rcw: deadlock: also VS has already called it
                                            UpdateStatusMsg("CodeMarkersAtWhichToTakeSnapshot:" +
                                                            GetCodeMarkerNameRaw(nCodeMarkerId, fuseOffline:=True) +
                                                            " SeqNo=" +
                                                            cmSeqNo.ToString("n0"), msgPriority:=StatusMessagePriority.Low)

                                            Dim destfolder = String.Empty
                                            Dim nCnt = 0
                                            While String.IsNullOrEmpty(destfolder) OrElse Directory.Exists(destfolder)
                                                nCnt += 1
                                                destfolder = IO.Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.MyDocuments),
                                                                                "MemSpect",
                                                                                "Snap" + nCnt.ToString)
                                            End While
                                            Common.OfflineMegaSnapshot.CreateMegaSnapshot(destfolder)
                                            ProcComm.UnFreezeTarget()
                                        End If

                                    End If
                                Case ProcMsgVerb.UpdateStatusMessage
                                    Dim str = Encoding.ASCII.GetString(bArray, 1, bArray.Length - 1)
                                    UpdateStatusMsg(str, msgPriority:=StatusMessagePriority.Low)
                                Case ProcMsgVerb.NoOp
                                    _noopCnt += 1
                                Case ProcMsgVerb.HeapLeak
                                    Dim handleHeap = New IntPtr(BitConverter.ToInt32(bArray, 1))
                                    Dim pSpyHeap = CSpyHeap.GetOrMakeSpyHeapFromHandle(handleHeap)
                                    Dim hblkAddr = New IntPtr(BitConverter.ToInt32(bArray, 5))
                                    Dim hctr = MakeHeapAllocationContainerFromAllocationStructAddr(hblkAddr, pSpyHeap)
                                    If hctr IsNot Nothing Then
                                        UpdateStatusMsg("Heap Leak '" + hctr.SpyHeapPtr.HeapName + "' " + hctr.GetSize.ToString, msgPriority:=StatusMessagePriority.Low)
                                        _HeapLeaks.Add(hctr)
                                    End If

                                    Dim bResult(4) As Byte
                                    _pipestreamToChild.Write(bResult, 0, 4) ' return a value, so target can continue

                                Case ProcMsgVerb.GhostAllocFree
                                    Dim handleHeap = New IntPtr(BitConverter.ToInt32(bArray, 1))
                                    Dim pSpyHeap = CSpyHeap.GetOrMakeSpyHeapFromHandle(handleHeap)
                                    Dim hblkAddr = New IntPtr(BitConverter.ToInt32(bArray, 1 + 1 * 4))
                                    Dim seqnoNow = BitConverter.ToInt32(bArray, 1 + 2 * 4)
                                    ' this hctr can get invalid very quickly: the targ proc is still running and can free it
                                    Dim hctr = MakeHeapAllocationContainerFromAllocationStructAddr(hblkAddr, pSpyHeap)
                                    If hctr IsNot Nothing Then
                                        ' now we have partial info: we need to extract stuff like callstacks because
                                        ' it could be freed immediately
                                        ' can't resolve symbols while targ proc is running: deadlock
                                        ' so store stack addrs to array
                                        Dim seqnoBeingFreed = hctr.AllocationStruct.SeqNo
                                        Dim ghostCtr = New AllocCtrGhost With {
                                                .hctr = hctr,
                                                .callstkArray = hctr.GetCallStackAddressestoArray,
                                                .Heap = pSpyHeap,
                                                .SeqNoWhenFreed = seqnoNow
                                                }
                                        hctr.GhostAlloc = ghostCtr
                                        Debug.Assert(Not _GhostData.GhostList.ContainsKey(seqnoBeingFreed), "Ghost list already had hctr?" + hctr.ToString())
                                        _GhostData.GhostList(hctr.AllocationStruct.SeqNo) = ghostCtr

                                    End If
                                    Dim bResult(4) As Byte
                                    _pipestreamToChild.Write(bResult, 0, 4) ' return a value, so target can continue

                                Case ProcMsgVerb.Quit
                                    threadRoutineDone = True
                                    MsgBox("got msg from parent: quitting!")
                                Case Else
                                    UpdateStatusMsg("Got unknown verb" + vrb.ToString, msgPriority:=StatusMessagePriority.Low)
                            End Select
                        Else
                            Debug.Assert(False, "got no msg from child")
                        End If

                    Catch ex As Exception
                        Debug.WriteLine(ex.Message)
                    End Try
                Else
                    'timed out
                    If _IsShuttingDown Then
                        threadRoutineDone = True
                    End If
                End If
            Loop
            If __hProcessTarget <> IntPtr.Zero Then
                WriteProcessMemory(_hProcessTarget, _addressOf_g_ThreadCmdsToChildReady, New Byte() {0, 0, 0, 0}, 4, Nothing) ' indicate we're not listening
            End If

        End Sub
        ''' <summary>
        ''' Send a verb to the target proc. Waits for a reply if fGetRetval is true.
        ''' Important sync note: most calls are synchronous
        ''' </summary>
        ''' <param name="bVerb">the verb to use</param>
        ''' <param name="dwords">int array of params</param>
        ''' <returns>byte array. If verb returns 1 DWORD, array is size 5: 0 = VerbDone, 1-4 = DWORD</returns>
        Public Function SendMsg(
                               ByVal bVerb As ProcMsgVerb,
                               Optional ByVal dwords() As Integer = Nothing,
                               Optional ByVal fSendEndMsgSync As Boolean = True,
                               Optional ByVal stringArray() As String = Nothing
                               ) As Byte()
            Debug.Assert(Common._ConnectionMode = MemSpectMode.OnLine)
            If _pipestreamToTarget Is Nothing OrElse Not _pipestreamToTarget.IsConnected Then ' shutting down...
                '                Throw New InvalidOperationException("Communication error")
                Return Nothing
            End If
            Debug.Assert(_IsInMiddleOfMessaging = ProcMsgVerb.UnKnownVerb OrElse _IsInMiddleOfMessaging = ProcMsgVerb.Quit, "SendMsg(" + bVerb.ToString + ")  while in middle of messaging: " + _IsInMiddleOfMessaging.ToString)
            Dim barrayTemp As Byte()
            If stringArray IsNot Nothing Then
                Dim arrSize = 1 ' 1 for the verb
                Array.ForEach(stringArray, Sub(s) arrSize += s.Length + 1) '+  nullterm
                barrayTemp = CType(Array.CreateInstance(GetType(Byte), arrSize), Byte())
                barrayTemp(0) = CByte(bVerb)
                Dim ndx = 1
                Dim enc = New ASCIIEncoding()
                Array.ForEach(stringArray, Sub(s)
                                               Dim bt = enc.GetBytes(s)
                                               If bt.Length > Byte.MaxValue Then
                                                   Throw New InvalidOperationException(String.Format("String parameter too long {0}", s))
                                               End If
                                               Array.Copy(bt, sourceIndex:=0, destinationArray:=barrayTemp, destinationIndex:=ndx, length:=bt.Length)
                                               ndx += bt.Length + 1 ' nullterm
                                           End Sub)

            Else
                If dwords Is Nothing Then
                    dwords = {0}
                End If
                Dim arrSize = 1 + dwords.Count * IntPtr.Size
                barrayTemp = CType(Array.CreateInstance(GetType(Byte), arrSize), Byte())
                barrayTemp(0) = CByte(bVerb)
                For i = 0 To dwords.Count - 1 ' little endian first
                    Dim bytes = BitConverter.GetBytes(dwords(i))
                    For j = 0 To 3
                        barrayTemp(1 + 4 * i + j) = bytes(j)
                    Next
                Next
            End If
            If barrayTemp.Length >= 1024 Then
                Throw New InvalidOperationException("MaxMsgSize exceeded " + barrayTemp.Length.ToString + " see MaxMsgSize in trackmem.cpp")
            End If
            Dim evWrite = New ManualResetEventSlim
            Dim exceptAsync As Exception = Nothing
            _pipestreamToTarget.BeginWrite(barrayTemp, 0, barrayTemp.Length, Sub(ar As IAsyncResult)
                                                                                 Try
                                                                                     _pipestreamToTarget.EndWrite(ar)
                                                                                     evWrite.Set()
                                                                                 Catch ex As Exception
                                                                                     exceptAsync = ex
                                                                                     UpdateStatusMsg("Exception while async write pipe " + ex.ToString)
                                                                                 End Try
                                                                             End Sub, Nothing)

            '            _pipestreamToTarget.Write(barray, 0, barray.Length)
            WaitForPipeOp(evWrite, "Write")

            'If Not evWrite.Wait(_NamedPipeTimeout) Then
            '    Throw New InvalidOperationException("Named pipe write timeout" + If(exceptAsync Is Nothing, String.Empty, exceptAsync.ToString()))
            'End If
            'If exceptAsync IsNot Nothing Then
            '    Throw New InvalidOperationException("Exception during Aync Write " + exceptAsync.ToString())
            'End If
            _eventSndMsgFromChild.Set()
            'If barray(0) = CByte(ProcMsgVerb.GetMemSpectVersion) Then
            '    'Dim rr = _pipestream.IsMessageComplete
            '    Dim m1 = GetMsg()
            '    Dim ntoread = 1
            '    Dim bytecntrecd = BitConverter.ToInt32(m1, 1)

            '    Dim barrRead(100) As Byte
            '    For i = 0 To bytecntrecd
            '        Dim nBytesRead = _pipestream.Read(barrRead, 0, ntoread)
            '    Next



            'End If
            Dim retval As Byte() = Nothing
            'Select Case bVerb
            '    Case ProcMsgVerb.GetClrObjDump,
            '        ProcMsgVerb.Quit,
            '        ProcMsgVerb.TranslateStackIndex,
            '        ProcMsgVerb.GetCodeMarkers,
            '        ProcMsgVerb.GetHeapAllocs,
            '        ProcMsgVerb.GetThreadInfo,
            '        ProcMsgVerb.GetClrClsLayout,
            '        ProcMsgVerb.AssertSeqNo,
            '        ProcMsgVerb.AssertStackFrame
            '        ' these above don't use synchronous

            '        Debug.Assert(bVerb = ProcMsgVerb.Quit OrElse _IsInMiddleOfMessaging = ProcMsgVerb.UnKnownVerb, "SendMsg(" + bVerb.ToString + ") while we are in middle of messaging " + _IsInMiddleOfMessaging.ToString)
            '        If fSendEndMsgSync Then
            '            retval = EndMsgSync()
            '        Else
            '            _IsInMiddleOfMessaging = bVerb ' record verb we're in middle of
            '        End If
            '    Case Else ' normal synchronous verbs will wait for VerbDone
            '        retval = EndMsgSync()
            'End Select
            Debug.Assert(bVerb = ProcMsgVerb.Quit OrElse _IsInMiddleOfMessaging = ProcMsgVerb.UnKnownVerb, "SendMsg(" + bVerb.ToString + ") while we are in middle of messaging " + _IsInMiddleOfMessaging.ToString)
            If fSendEndMsgSync Then
                retval = EndMsgSync()
            Else
                _IsInMiddleOfMessaging = bVerb ' record verb we're in middle of
            End If
            If bVerb = ProcMsgVerb.Quit Then
                _pipestreamToTarget.Close()
                _pipestreamToTarget.Dispose()
                _pipestreamToTarget = Nothing
                CloseComm()
            End If
            Return retval
        End Function

        Public Function EndMsgSync() As Byte()
            Dim retval = GetMsg(100) ' calling GetMsg will wait til server done, synchronizing.
            If retval.Length < 1 OrElse retval(0) <> ProcMsgVerb.VerbDone Then
                Debug.Assert(False, "verb should return Ready " + retval.Length.ToString)
            End If
            _IsInMiddleOfMessaging = ProcMsgVerb.UnKnownVerb
            Return retval
        End Function

        Public Function GetMsg(ByVal nMaxSize As Integer, Optional ByVal strmToUse As PipeStream = Nothing) As Byte()
            If Common._ConnectionMode = MemSpectMode.Offline Then
                Throw New InvalidOperationException("online feature not available offline")
            End If
            If strmToUse Is Nothing Then
                strmToUse = _pipestreamToTarget
            End If
            If Not strmToUse.IsConnected Then
                Throw New InvalidOperationException("Stream is not connected")
            End If
            Dim barrRead(nMaxSize - 1) As Byte
            Dim nBytesRead = 0
            Dim evread = New ManualResetEventSlim
            Dim exceptAsync As Exception = Nothing

            Dim ar = strmToUse.BeginRead(barrRead, 0, nMaxSize, Sub(arcallback As IAsyncResult)
                                                                    Try
                                                                        nBytesRead = strmToUse.EndRead(arcallback)
                                                                        evread.Set()
                                                                    Catch ex As Exception
                                                                        exceptAsync = ex
                                                                        UpdateStatusMsg("Ex while beginread stream " + ex.ToString)
                                                                    End Try
                                                                End Sub, Nothing)

            WaitForPipeOp(evread, "Read")
            '            nBytesRead = strmToUse.Read(barrRead, 0, nMaxSize)
            ReDim Preserve barrRead(nBytesRead - 1)
            Return barrRead
        End Function

        Public Function ReadSharedMemAsString(Optional ByVal nOffset As Integer = 0, Optional ByVal fAnsi As Boolean = True) As String
            Debug.Assert(Common._ConnectionMode = MemSpectMode.OnLine)
            Dim sResult = String.Empty
            If (fAnsi) Then
                sResult = Marshal.PtrToStringAnsi(_SharedMemAddr.MyAdd(nOffset)).TrimEnd ' reads mem shared by parent/child
            Else
                sResult = Marshal.PtrToStringUni(_SharedMemAddr.MyAdd(nOffset)).TrimEnd ' reads mem shared by parent/child
            End If
            Return sResult
        End Function

        Private _disp As Dispatcher
        Public Delegate Sub ParentDone()
        Public _delParentDone As ParentDone

        Sub OnParentProcessDone(ByVal sender As Object, ByVal e As EventArgs)
            '_IsShuttingDown = True
            '        Messagebox.show("the parent closed")
            If _delParentDone IsNot Nothing Then
                _disp.Invoke(_delParentDone) ' needs to run on main thread
            End If
        End Sub


        Public Sub FreezeTarget(Optional ByVal nCleanupRCWs As Integer = 1, Optional ByVal fRaiseEvent As Boolean = True)
            If _isTargetFrozen Then
                Return
            End If
            Dim nseqno = 0
            If _ConnectionMode = MemSpectMode.OnLine Then
                Dim res = SendMsg(ProcMsgVerb.ThreadsFreeze, fSendEndMsgSync:=True, dwords:={nCleanupRCWs})
                If Not _isTargetFrozen Then
                    UpdateStatusMsg("Couldn't freeze target: is it using Sever GC (and not Workstation GC) ?", msgType:=StatusMessageType.AlertMsgBox)
                    Return
                End If
                nseqno = BitConverter.ToInt32(res, 1)
            End If
            If fRaiseEvent Then
                RaiseEvent TargetFrozen() ' checkbox change to ischecked.
            End If
            If _ConnectionMode = MemSpectMode.OnLine Then
                UpdateStatusMsg("Freeze Target SeqNo= " + nseqno.ToString("n0"))
                If IsUsingStackMemMap Then
                    Dim stats = StackMemMapStats.GetMemMapStats().ToString()
                    UpdateStatusMsg(stats)
                End If

            End If
        End Sub

        Public Sub UnFreezeTarget()
            If Not _isTargetFrozen Then
                Return
            End If
            Dim nseqno = 0
            Dim res = SendMsg(ProcMsgVerb.ThreadsUnFreeze, fSendEndMsgSync:=True, dwords:={CUInt(1)})
            nseqno = BitConverter.ToInt32(res, 1)
            HeapAllocationContainer._NumInstances = 0
            RaiseEvent TargetUnfrozen()
            If _ConnectionMode = MemSpectMode.OnLine Then
                UpdateStatusMsg("UnFreeze Target Seqno= " + nseqno.ToString("n0"))
            End If
        End Sub

        Friend Function TargProcHasExited() As Boolean
            Dim fTargHasExited = False
            If _TargetProcessId = 0 Then
                fTargHasExited = True
            Else
                If _TargetProc IsNot Nothing Then
                    If _TargetProc.HasExited Then
                        fTargHasExited = True
                    End If
                Else
                    If _TargetProc IsNot Nothing AndAlso _TargetProc.HasExited Then
                        fTargHasExited = True
                    Else
                        Dim procT As Process = Nothing
                        Try
                            procT = Process.GetProcessById(_TargetProcessId)
                        Catch ex As Exception
                            fTargHasExited = True
                        End Try
                        If procT Is Nothing OrElse procT.HasExited Then
                            fTargHasExited = True
                        End If
                    End If
                End If
            End If
            Return fTargHasExited
        End Function

        Private Sub WaitForPipeOp(ByVal evPipe As ManualResetEventSlim, ByVal desc As String)
            While Not evPipe.IsSet
                If Not evPipe.Wait(_NamedPipeTimeout) Then
#If DEBUG Then
                    UpdateStatusMsg("Named Pipe " + desc + " Timeout: check proc still alive")
#End If
                    If TargProcHasExited() Then
                        _IsShuttingDown = True
                        If _pipestreamToChild IsNot Nothing Then
                            _pipestreamToChild.Close()
                            _pipestreamToChild = Nothing ' streams are useless now
                        End If
                        If _pipestreamToTarget IsNot Nothing Then
                            _pipestreamToTarget.Close()
                            _pipestreamToTarget = Nothing
                        End If
                        UpdateStatusMsg("Named pipe " + desc + " timeout and proc " + _TargetProcessId.ToString() + " exited")
                        CloseComm() 'timer tick will fire, tick routine will check for close comm and call DoForceClose
                        Throw New InvalidOperationException("Target process has exited")
                    End If
                End If
            End While
        End Sub

        Async Function SendTelemetryAsync(msg As String) As Task(Of String) ' like z=44
            Dim result = String.Empty
            Try
                Dim exe = Process.GetCurrentProcess().ProcessName.ToLowerInvariant() ' like "windbg"
                If Not exe.StartsWith("vstest.executionengine") And Not exe.StartsWith("cslife") And Not exe.StartsWith("notepad") And Not exe.StartsWith("arenaclient") And Environment.GetEnvironmentVariable("username").ToLower() <> "calvinh" Then
                    Dim url = "http://calvinh6/MSpect.asp?"
                    If Not String.IsNullOrEmpty(msg) Then
                        '                msg = WebUtility.UrlEncode(msg)
                        url += msg
                    End If
                    url += String.Format("&CMode={0}", _ConnectionMode)
                    url += String.Format("&p={0}", exe)
                    Dim wReq As HttpWebRequest = CType(WebRequest.Create(url), HttpWebRequest)
                    wReq.UseDefaultCredentials = True
                    Using resp = Await wReq.GetResponseAsync()
                        Using rstream = resp.GetResponseStream()
                            Dim sr = New StreamReader(rstream, Encoding.ASCII)
                            result = sr.ReadToEnd
                        End Using
                    End Using

                End If
            Catch ex As Exception

            End Try
            Return result
        End Function

        Async Sub SendTelemetry(msg As String)
            Dim result = Await SendTelemetryAsync(msg)

        End Sub

    End Module

    Public Class ImmersivePackageInfoClass
        Public strManifestPath As String
        Public strPackageFullName As String        'Package Full Name : 226e5454-7bb8-43ac-b20a-17bb711430b8_1.0.0.0_neutral__faknarqntgs30
        Public strPackageName As String            'Package Name : 226e5454-7bb8-43ac-b20a-17bb711430b8
        Public strPackageFamilyName As String      'Package Family Name : 226e5454-7bb8-43ac-b20a-17bb711430b8_faknarqntgs30
        Public strAppUserModelId As String         '"226e5454-7bb8-43ac-b20a-17bb711430b8_faknarqntgs30!App"
        Public strpAppContainerNamedObjectPath As String 'AppContainerNamedObjects\S-1-15-2-164615428-3030373648-1861741472-1867675152-3734011038-2714587842-4209093548
        Public _hr As Integer
        Public Sub New(ByVal manifestPath As String) '"c:\users\calvinh\documents\visual studio 11\Projects\csGridApp1\csGridApp1\bin\Debug\AppX\AppxManifest.xml"
            strManifestPath = manifestPath
            strPackageFullName = String.Empty
            strPackageName = String.Empty
            strPackageFamilyName = String.Empty
            strAppUserModelId = String.Empty
            Dim strbldpAppContainerNamedObjectPath As New Text.StringBuilder(MAX_PATH)
            _hr = GetPackageNamesFromManifest(manifestPath,
                                                 0,
                                                 strPackageName,
                                                 strPackageFullName,
                                                 strPackageFamilyName,
                                                 strAppUserModelId,
                                                 strbldpAppContainerNamedObjectPath)
            If _hr <> 0 Then
                UpdateStatusMsg(manifestPath + vbCrLf + GetErrorMessageFromHResult(_hr))
                '        End ' end program!
            End If
            strpAppContainerNamedObjectPath = strbldpAppContainerNamedObjectPath.ToString
        End Sub

    End Class


    Public Class GlobalFilter
        Inherits SerializableObject

        Public Property Thread As Integer
        Public Property SeqNoLo As UInteger
        Public Property SeqNoHi As UInteger ' 0 means disabled
        Public Property SrchString As String
        Public Property LeakMultiple As Integer
        Public Property KnownIssues As ShowKnownIssuesEnum
        Public _LeakMultipleRawText As String = String.Empty
        Public _LeakMultipleSeqnos(,) As UInteger
        Public Property LeakMultipleRawText As String ' will be either an integer or a set of N comma separated pairs, 1 per line
            Get
                Return _LeakMultipleRawText
            End Get
            Set(ByVal value As String)
                Try
                    _LeakMultipleSeqnos = Nothing
                    LeakMultiple = 0
                    _LeakMultipleRawText = value.Trim()
                    If Not String.IsNullOrEmpty(_LeakMultipleRawText) Then
                        Dim lines = _LeakMultipleRawText.Split({CChar(vbCr), CChar(vbLf)}, StringSplitOptions.RemoveEmptyEntries)
                        If lines.Count = 1 Then
                            LeakMultiple = Integer.Parse(value)
                        Else
                            Dim testpairs(lines.Count - 1, 1) As UInteger
                            For i = 0 To lines.Count - 1
                                Dim pair = lines(i).Split(","c)
                                If pair.Count <> 2 Then
                                    Throw New Exception("# pairs is wrong on line " + (i + 1).ToString)
                                End If
                                testpairs(i, 0) = UInteger.Parse(pair(0))
                                testpairs(i, 1) = UInteger.Parse(pair(1))

                            Next
                            LeakMultiple = lines.Count
                            _LeakMultipleSeqnos = testpairs
                            SeqNoLo = _LeakMultipleSeqnos(0, 0)
                            SeqNoHi = _LeakMultipleSeqnos(LeakMultiple - 1, 1)
                        End If
                        UpdateStatusMsg("LeakMultiple count set=" + LeakMultiple.ToString)
                    End If
                Catch ex As Exception
                    MsgBox(ex.ToString())
                End Try
            End Set
        End Property
        Public Const BaseFilterString As String = "Filter = "
        Sub New()
            SrchString = String.Empty
        End Sub
        ' potential Size > or < value
        ' modulename
        'Public Event FilterEvent(ByVal sender As Object, ByVal e As FilterEventArgs)
        'Public Class FilterEventArgs
        '    Inherits EventArgs
        '    Public Property FoundStr As String ' could be stackframe, class name, or string content
        '    Public Property hCtr As HeapAllocationContainer
        '    Public Overrides Function ToString() As String
        '        Return FoundStr
        '    End Function
        'End Class
        ' easier to use delegate than events: event lifetime mgmt, creating event args
        Public Enum FilterFindType
            FromSeqno
            FromStackFrame
            FromStringContent
            FromClassName
            FromClassField
        End Enum
        Delegate Sub FiltergotString(ByVal foundstr As String, ByVal hctr As HeapAllocationContainer, ByVal findType As FilterFindType)
        Public _deleFilterGotString As FiltergotString ' can only be one
        Public _dictFrames As Dictionary(Of IntPtr, Boolean)
        Public FilterFunction As Func(Of HeapAllocationContainer, Boolean) =
            Function(hctr As HeapAllocationContainer) As Boolean
                If Thread > 0 Then
                    If hctr.AllocationStruct.ThreadId <> _GlobalFilter.Thread Then
                        Return False
                    End If
                End If
                If SeqNoLo > 0 Then
                    If hctr.AllocationStruct.SeqNo < SeqNoLo Then
                        Return False
                    End If
                End If
                If SeqNoHi > 0 Then
                    If hctr.AllocationStruct.SeqNo > SeqNoHi Then
                        Return False
                    End If
                End If
                'If Not hctr.IsLeakableType Then ' we want code markers too
                '    '                    Return False
                'End If
                Dim fResult = True
                If Not String.IsNullOrEmpty(SrchString) Then
                    Dim fPositive = True
                    Dim strsToSearch = SrchString.Split(";"c)
                    For Each srchStrToUse In strsToSearch
                        If Not String.IsNullOrEmpty(srchStrToUse) Then
                            Dim strToSearch = srchStrToUse
                            If strToSearch.StartsWith("!") Then
                                fPositive = False
                                strToSearch = strToSearch.Substring(1)
                                If strToSearch.Length = 0 Then
                                    Return True
                                End If
                            End If
                            Dim stackFrames = hctr.GetCallStackAddressestoArray
                            Dim frName = String.Empty
                            fResult = False
                            If _dictFrames IsNot Nothing Then
                                Array.ForEach(stackFrames, Sub(frameAddr)
                                                               Dim gotit = False
                                                               If Not _dictFrames.TryGetValue(frameAddr, gotit) Then
                                                                   frName = ResolveAddressToSymbol(frameAddr)
                                                                   If frName.IndexOf(strToSearch, StringComparison.OrdinalIgnoreCase) >= 0 Then
                                                                       gotit = True
                                                                   End If
                                                                   _dictFrames(frameAddr) = gotit
                                                               Else
                                                                   If gotit Then
                                                                       frName = ResolveAddressToSymbol(frameAddr)
                                                                   End If
                                                               End If
                                                               If gotit Then
                                                                   If Not fResult Then ' call just once regardless of # of stack frames positive
                                                                       If _deleFilterGotString IsNot Nothing AndAlso (gotit AndAlso fPositive OrElse Not gotit AndAlso Not fPositive) Then
                                                                           _deleFilterGotString(frName, hctr, FilterFindType.FromStackFrame)
                                                                       End If
                                                                   End If
                                                                   fResult = True ' indicate that this alloc is included
                                                               End If
                                                           End Sub)
                            Else
                                Dim frames = hctr.GetCallStackAsStringArray
                                fResult = frames.Any(
                                    Function(frame)
                                        Dim gotit = frame.IndexOf(strToSearch, StringComparison.OrdinalIgnoreCase) >= 0
                                        If _deleFilterGotString IsNot Nothing AndAlso (gotit AndAlso fPositive OrElse Not gotit AndAlso Not fPositive) Then
                                            _deleFilterGotString(frame, hctr, FilterFindType.FromStackFrame)
                                        End If
                                        Return gotit
                                    End Function) ' any found
                            End If
                            If _deleFilterGotString IsNot Nothing AndAlso Not fResult Then ' if we already added via stack frame, we don't need to check class name or string content
                                Dim tmpstr = hctr.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True)
                                Dim findtype = FilterFindType.FromClassName
                                If tmpstr.IndexOf(strToSearch, StringComparison.OrdinalIgnoreCase) >= 0 Then
                                    fResult = True
                                End If
                                If Not fResult Then
                                    Dim layouts = ClrClassInfo.GetClassLayoutsFromClassId(hctr.GetClassId)
                                    For Each layout In layouts
                                        If Not fResult Then
                                            For Each fld In layout.dictFieldInfo.Values
                                                If fld.FldName.IndexOf(strToSearch, StringComparison.OrdinalIgnoreCase) >= 0 Then
                                                    fResult = True
                                                    tmpstr = fld.FldName
                                                    findtype = FilterFindType.FromClassField
                                                    Exit For
                                                End If
                                            Next
                                        End If
                                    Next
                                End If
                                If fResult AndAlso fPositive OrElse Not fResult AndAlso Not fPositive Then
                                    _deleFilterGotString(tmpstr, hctr, findtype)
                                End If
                                If Not fResult Then ' don't want to double count
                                    If Not hctr.IsMemSpectHeap Then
                                        tmpstr = hctr.GetStringContent()
                                        If tmpstr.IndexOf(strToSearch, StringComparison.OrdinalIgnoreCase) >= 0 Then
                                            fResult = True
                                        End If
                                        If fResult AndAlso fPositive OrElse Not fResult AndAlso Not fPositive Then
                                            _deleFilterGotString(tmpstr, hctr, FilterFindType.FromStringContent)
                                        End If
                                    End If
                                End If
                            End If
                            If Not fPositive Then
                                fResult = Not fResult
                            End If
                            If fResult Then
                                Exit For ' early exit: we found 1 result
                            End If

                        End If
                    Next
                End If
                Return fResult
            End Function

        Public ReadOnly Property IsFilterMoreThanOnlySeqno As Boolean
            Get
                Dim fIsFilterMoreThanOnlySeqno = False
                If Not String.IsNullOrEmpty(SrchString) OrElse Thread > 0 Then
                    fIsFilterMoreThanOnlySeqno = True
                End If
                Return fIsFilterMoreThanOnlySeqno
            End Get
        End Property
        Public Sub ClearFilter()
            Thread = 0
            SeqNoHi = 0
            SeqNoLo = 0
            SrchString = String.Empty
            LeakMultiple = 0
            _LeakMultipleRawText = String.Empty
            _dictFrames = Nothing
            KnownIssues = ShowKnownIssuesEnum.NoAction ' takes a long time to execute, so leave it off when unfiltered
        End Sub
        '' occurs when filter changed, 
        Public Shared Event GlobalFilterChanged(ByVal sender As Object, ByVal e As EventArgs)
        Public Sub RefreshFilterDisplay()
            RaiseEvent GlobalFilterChanged(Me, New EventArgs())
            'For Each inst In VBDiagMarginBase._instanceList
            '    inst._FilterUI.RefreshFilterTextbox()
            'Next
        End Sub
        Public Overloads Function ToString(ByVal fLongVersion As Boolean) As String
            Dim str = Me.ToString()
            If fLongVersion Then
                str += " Leak Multiple = " + LeakMultiple.ToString
            End If
            Return str
        End Function
        Public Overrides Function ToString() As String
            Dim str = BaseFilterString
            If SeqNoLo > 0 Then
                If SeqNoHi > 0 Then
                    str += "SeqNo between " + SeqNoLo.ToString("n0") + ", " + SeqNoHi.ToString("n0")
                Else
                    str += "SeqNo >=" + SeqNoLo.ToString("n0")
                End If
            Else
                If SeqNoHi > 0 Then
                    str += "Seqno <=" + SeqNoHi.ToString("n0")
                End If
            End If
            If Not String.IsNullOrEmpty(SrchString) Then
                str += " Srch='" + SrchString + "'"
            End If
            If Thread > 0 Then
                str += " Thread = " + _Thread.ToString
            End If
            If KnownIssues <> ShowKnownIssuesEnum.NoAction Then
                str += " " + KnownIssues.ToString
            End If
            Return str
        End Function

        Function IsEmpty() As Boolean
            If ToString() = BaseFilterString Then
                Return True
            End If
            Return False
        End Function
        Public Overrides ReadOnly Property Version As Integer
            Get
                Return SerializableObject.CurrentSerializationVersion + 2
            End Get
        End Property
        Public Overrides Sub FromStream(ByVal deserializer As Deserializer, ByVal versionFromStream As Integer)
            SeqNoLo = IntegerToUInteger(deserializer.ReadInt)
            SeqNoHi = IntegerToUInteger(deserializer.ReadInt)
            Thread = deserializer.ReadInt
            SrchString = deserializer.ReadString

            If versionFromStream > SerializableObject.CurrentSerializationVersion Then
                ' now read in some other global variables: include cur SeqNo in snapshot
                _memSpectThreadId = deserializer.ReadInt
                _memSpectSharedMemAddr = New IntPtr(deserializer.ReadInt)
                _TargetProcessId = deserializer.ReadInt
                _MainThread = deserializer.ReadInt
                If versionFromStream > SerializableObject.CurrentMinimumVersion + 1 Then
                    ProcComm._hMemSpectHeap = New IntPtr(deserializer.ReadInt)
                    ProcComm._hProcessHeap = New IntPtr(deserializer.ReadInt)
                    ProcComm._hDebugHeap = New IntPtr(deserializer.ReadInt)
                    _SharedMemAddr = New IntPtr(deserializer.ReadInt)
                    _GCStatsAddr = New IntPtr(deserializer.ReadInt)
                    _AddressOfStackMemMapStats = New IntPtr(deserializer.ReadInt)
                    _AddressOfnCurAllocs = New IntPtr(deserializer.ReadInt)
                    _AddressOfulGlobalPassCount = New IntPtr(deserializer.ReadInt)
                    _fHandle4gigStacks = If(deserializer.ReadInt() = 1, True, False)
                    Dim nFiles = deserializer.ReadInt
                    For i = 0 To nFiles - 1
                        Dim naddr = New IntPtr(deserializer.ReadInt)
                        Dim fname = deserializer.ReadString()
                        Common._offlineSnapshot._mappedFilesDictionary(naddr) = fname
                    Next
                End If
            End If
        End Sub

        Public Overrides Sub ToStream(ByVal serializer As Serializer)
            serializer.Write(UIntegerToInteger(SeqNoLo))
            serializer.Write(UIntegerToInteger(SeqNoHi))
            serializer.Write(Thread)
            serializer.Write(SrchString)

            If Me.Version > SerializableObject.CurrentSerializationVersion Then
                'now add some other global variables
                serializer.Write(_memSpectThreadId)
                serializer.Write(_memSpectSharedMemAddr.ToInt32)
                serializer.Write(_TargetProcessId)
                serializer.Write(_MainThread)
                If Me.Version > SerializableObject.CurrentSerializationVersion + 1 Then
                    ' now some more globals
                    serializer.Write(ProcComm._hMemSpectHeap.ToInt32)
                    serializer.Write(ProcComm._hProcessHeap.ToInt32)
                    serializer.Write(ProcComm._hDebugHeap.ToInt32)
                    serializer.Write(_SharedMemAddr.ToInt32)
                    serializer.Write(_GCStatsAddr.ToInt32)
                    serializer.Write(_AddressOfStackMemMapStats.ToInt32)
                    serializer.Write(_AddressOfnCurAllocs.ToInt32)
                    serializer.Write(_AddressOfulGlobalPassCount.ToInt32)
                    serializer.Write(If(_fHandle4gigStacks, 1, 0))

                    Dim dictFNames = New Dictionary(Of IntPtr, String)
                    Dim virtAllocs = GetVirtAllocs()
                    For Each virtAlloc In virtAllocs
                        If (virtAlloc.Value.lType And (AllocationType.MEM_MAPPED + AllocationType.MEM_IMAGE)) > 0 Then
                            Dim fname = GetFileNameFromMBI(virtAlloc.Value)
                            If Not String.IsNullOrEmpty(fname) Then
                                dictFNames(virtAlloc.Value.AllocationBase) = fname
                            End If
                        End If
                    Next
                    serializer.Write(dictFNames.Count)
                    For Each entry In dictFNames
                        serializer.Write(entry.Key.ToInt32)
                        serializer.Write(entry.Value)
                    Next

                End If
            End If

        End Sub
    End Class

End Namespace
