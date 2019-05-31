
Imports System.Windows.Controls
Imports System.Windows.Threading
Imports System.Windows.Data
Imports System.Windows
Imports System.Windows.Input
Imports System.IO
Imports System.Windows.Media
Imports System.Collections.ObjectModel
Imports System.Runtime.CompilerServices
Imports System.Linq
Imports System.Xml.Linq
Imports System.Runtime.InteropServices
Imports MemSpect.Images

Namespace MemSpect
    Public Enum VMColorType
        All ' none
        Stack
        Heap
        Managed
        Image
        Mapped
        [Private]
        Free
        Fragment ' < 64k
    End Enum
    Friend Class VMFragInfo
        Public colrType As VMColorType
        Public nBlks As Integer
        Public nSize As UInteger
    End Class

    Friend Class VMColorClass
        Public colrType As VMColorType
        Public colorBrush As SolidColorBrush
        Public TotalSize As ULong
        Public Committed As ULong
        Public WorkingSet As ULong
        Public BlkCnt As Integer

        ' used to color both VM and legend at top
        Public Shared Function GetVMColorType(ByVal dc As Object) As VMColorClass
            'color chart:  http://docs.mql4.com/constants/colors
            Dim strData = String.Empty
            Dim vmdat As VMData = Nothing
            If TryCast(dc, VMData) IsNot Nothing Then 'when calculating VMData
                vmdat = CType(dc, VMData)
                strData = vmdat.strData
            ElseIf TryCast(dc, String) IsNot Nothing Then 'when color for legend at top
                strData = CStr(dc)
            Else

                If TryCast(dc, ListViewItem) IsNot Nothing Then
                    dc = CType(dc, ListViewItem).DataContext
                End If

                Dim tdesc = System.ComponentModel.TypeDescriptor.GetProperties(dc)("Data")
                If tdesc Is Nothing Then
                    tdesc = ComponentModel.TypeDescriptor.GetProperties(dc)("Filter")
                End If
                strData = CStr(tdesc.GetValue(dc))
            End If

            Dim mbi As MEMORY_BASIC_INFORMATION
            Dim fHasMBI = False
            Dim lamGetMBI = Function() As Boolean
                                If fHasMBI Then
                                    Return True
                                End If
                                If vmdat IsNot Nothing Then
                                    mbi = vmdat.mbi
                                    fHasMBI = True
                                Else
                                    Dim mbitdesc = ComponentModel.TypeDescriptor.GetProperties(dc)("_mbi")
                                    If mbitdesc IsNot Nothing Then
                                        mbi = CType(mbitdesc.GetValue(dc), MEMORY_BASIC_INFORMATION)
                                        fHasMBI = True
                                    End If
                                End If
                                Return fHasMBI
                            End Function
            Dim result = New VMColorClass With {.colrType = VMColorType.All, .colorBrush = Brushes.LightYellow}
            '            Dim dc = itm.DataContext
            '            Dim listView As ListView = TryCast(ItemsControl.ItemsControlFromItemContainer(item), ListView)

            If strData.StartsWith("Stack") Then
                result.colrType = VMColorType.Stack
                If True Then
                    If strData.Contains("MemSpect") Then
                        result.colorBrush = Brushes.OrangeRed
                    ElseIf strData.Contains("Main UI") Then
                        result.colorBrush = Brushes.Orange
                    Else
                        result.colorBrush = Brushes.Orange
                    End If
                End If
            ElseIf strData.StartsWith("Heap") Then
                result.colrType = VMColorType.Heap
                If strData.Contains("MemSpect") Then
                    result.colorBrush = Brushes.Tomato
                Else
                    result.colorBrush = Brushes.Salmon
                End If
            ElseIf strData.StartsWith("Managed") Then
                result.colrType = VMColorType.Managed
                result.colorBrush = Brushes.YellowGreen
            Else
                Dim fIsImage = False
                Dim fIsMapped = False
                Dim fIsFree = False
                Dim fIsPrivate = False
                Select Case strData
                    Case "Image"
                        fIsImage = True
                    Case "Mapped"
                        fIsMapped = True
                    Case "Free", VMColorType.Fragment.ToString
                        fIsFree = True
                    Case "Private"
                        fIsPrivate = True
                    Case Else
                        If lamGetMBI.Invoke Then
                            If mbi.State = AllocationState.MEM_FREE Then
                                fIsFree = True
                            Else
                                If mbi.lType = AllocationType.MEM_IMAGE Then
                                    fIsImage = True
                                ElseIf mbi.lType = AllocationType.MEM_MAPPED Then
                                    fIsMapped = True
                                ElseIf mbi.lType = AllocationType.MEM_PRIVATE Then
                                    fIsPrivate = True
                                End If
                            End If
                        End If
                End Select

                If fIsFree Then
                    result.colrType = VMColorType.Free
                    If (lamGetMBI.Invoke AndAlso mbi.RegionSize < 65536) OrElse strData = VMColorType.Fragment.ToString Then
                        result.colrType = VMColorType.Fragment
                        result.colorBrush = Brushes.Silver
                    Else
                        result.colorBrush = Brushes.White
                    End If
                ElseIf fIsImage Then
                    result.colrType = VMColorType.Image
                    result.colorBrush = Brushes.Violet
                ElseIf fIsMapped Then
                    result.colrType = VMColorType.Mapped
                    result.colorBrush = Brushes.PaleTurquoise

                    If lamGetMBI.Invoke Then
                        If mbi.BaseAddress = _memSpectSharedMemAddr Then
                            result.colorBrush = Brushes.CornflowerBlue
                        End If
                    End If
                ElseIf fIsPrivate Then
                    result.colrType = VMColorType.Private
                    If strData.StartsWith("MemSpect") Then
                        result.colorBrush = Brushes.Gold
                    Else
                        result.colorBrush = Brushes.Yellow
                    End If
                End If
            End If
            Return result
        End Function
        Public Overrides Function ToString() As String
            Return Me.colrType.ToString + " " + Me.colorBrush.ToString
        End Function
    End Class

    Public NotInheritable Class VMBackgroundConverter
        Implements IValueConverter

        Public Function Convert(ByVal value As Object, ByVal targetType As System.Type, ByVal parameter As Object, ByVal culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.Convert
            Dim lvitem = CType(value, ListViewItem)
            Dim result = CType(ComponentModel.TypeDescriptor.GetProperties(lvitem.DataContext)("_bgcolor").GetValue(lvitem.DataContext), VMColorClass)
            'Dim colrstruct = VMColorClass.GetVMColorType(lvitem, fDetail:=True)
            'Dim result = colrstruct.colorBrush
            Return result.colorBrush

        End Function

        Public Function ConvertBack(ByVal value As Object, ByVal targetType As System.Type, ByVal parameter As Object, ByVal culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.ConvertBack
            Throw New NotImplementedException()
        End Function
    End Class

    Friend Class VMData
        Implements IMemoryBlock

        Friend mbi As MEMORY_BASIC_INFORMATION
        Friend WorkingSet As Integer
        Friend ShareCount As Integer
        Friend IsShared As Integer
        Friend strData As String = String.Empty
        Friend bgColorClass As VMColorClass

        Public Function Address() As ULong Implements Common.IMemoryBlock.Address
            Return mbi.BaseAddress.MyToULong
        End Function

        Public Function Size() As ULong Implements Common.IMemoryBlock.Size
            Return CULng(mbi.RegionSize)
        End Function
    End Class

    'see http://blogs.msdn.com/calvin_hsia/archive/2009/10/19/9909326.aspx
    Public Class VirtualMem

        Friend Shared sumrySize As Double = 0.7
        Friend Shared nMaxHeader As Integer = 140

        '        Private _snapshot As List(Of MEMORY_BASIC_INFORMATION)

        Private Shared vmStats As String

        'Private WithEvents _btnNewVMAllocs As Button
        'Private WithEvents _btnFreedVMAllocs As Button
        Friend _browColors As Browse
        Friend _browVM As Browse
        Friend _ctrlsVM As DataSurface
        Friend _imageAnalyzer As ImageAnalyzer
        Friend _VMDataDict As SortedList(Of IntPtr, VMData)
        Friend Class VMDisplayOptions
            Friend fShowDetail As Boolean
            ' add waste params too

        End Class
        Public Sub New()
            _MemBasicInfoList.Clear() ' start with clean slate
        End Sub

        Friend Shared Function ShowWorkingSetInfo() As DataSurface
            Dim ctrls As DataSurface = Nothing
            Dim wsList = WorkingSetInfo.GetWorkingSetDict
            Try
                ctrls = DataWindowMain.MakeNewDatasurface("WorkingSet", "Working set info (only for online)", nMaxHeaderHeight:=50)

                Dim q = From d In wsList.Values
                        Let mbi = GetMBIForAddress(d.VirtPage)
                        Select
                        Address = d.VirtPage.ToInt32.ToString("x8"),
                        AllocationBase = mbi.AllocationBase.ToInt32.ToString("x8"),
                        Data = GetFileNameFromMBI(mbi),
                        Protection = If(d.Protection < WorkingSetFlags.MAXVAL, CType(d.Protection, WorkingSetFlags).ToString, d.Protection.ToString),
                        d.ShareCount,
                        d.IsShared,
                        _mbi = mbi
                        Order By Address


                Dim br = New Browse(q,
                                    fAllowBrowFilter:=True,
                                    InitialSortOrder:=New BrowseInitialSortOrder With {.ColumnNo = 1},
                                    arrColumnsToTotal:={""})
                AddHandler br._BrowseList.MouseMove, AddressOf On_VirtmemMouseMove
                Dim ncnt = q.Count
                ctrls.SurfaceHeader.Children.Add(
                    New TextBlock With {
                        .Text = String.Format("Working set: # pages ={0:n0}, Total Working Set = {1:n0}  ({2:n0}K)", ncnt, ncnt * 4096, ncnt * 4)
                    }
                )
                ctrls.SurfaceDetails.Children.Add(br)
                'For Each itm In retVal.Values
                '    Debug.WriteLine(itm.ToString)
                'Next
            Catch ex As Exception
                CommonUI.MemSpectExceptionHandler(ex)
            End Try
            Return ctrls
        End Function

        Friend Function ShowVirtualAllocs(Optional ByVal displayOptions As VMDisplayOptions = Nothing) As DataSurface
            If displayOptions Is Nothing Then
                displayOptions = New VMDisplayOptions
            End If
            VBDiagMarginBase.FreezeTargetThreads()

            'gather stats and virtualqueryex the entire process address space
            Dim vmStats = VMStatistics.GetVirtualMemorystats()

            Dim dictHeapStuff = New Dictionary(Of IntPtr, String)
            Dim heapDataList As List(Of HeapReport.HeapData) = Nothing
            If _ConnectionMode = MemSpectMode.Offline Then
                Dim hrptOffline = Common._offlineSnapshot.heapReport ' force offline snap to load
                If _offlineSnapshot.heapReport Is Nothing Then
                    UpdateStatusMsg("VM display not including Heap report: it was not saved in snapshot: see 'IncludeHeapReport' ini setting")
                Else
                    heapDataList = hrptOffline._ProcessHeapData
                End If
            ElseIf _ConnectionMode = MemSpectMode.OnLine Then
                Dim hrpt = New HeapReportUI(fOnlyWantRegions:=True)
                heapDataList = hrpt._ProcessHeapData
            Else
                heapDataList = New List(Of HeapReport.HeapData)
            End If

            Dim heapTotalCommit As Long
            Dim heapTotalReserved As Long
            If heapDataList IsNot Nothing Then
                For Each hpdata In heapDataList
                    For Each peentry In hpdata.pelist
                        If (peentry.wFlags And PROCESS_HEAP_ENTRY_WFLAGS.PROCESS_HEAP_ENTRY_DDESHARE) > 0 Then
                            Dim r = 2
                        End If
                        If peentry.cbData > 0 AndAlso (peentry.wFlags And
                            (PROCESS_HEAP_ENTRY_WFLAGS.PROCESS_HEAP_REGION Or
                            PROCESS_HEAP_ENTRY_WFLAGS.PROCESS_HEAP_UNCOMMITTED_RANGE Or
                            PROCESS_HEAP_ENTRY_WFLAGS.PROCESS_HEAP_ENTRY_DDESHARE
                            )) > 0 Then ' DDEShare: CSmartStringSet =~ 2Megs
                            If peentry.wFlags = PROCESS_HEAP_ENTRY_WFLAGS.PROCESS_HEAP_REGION Then
                                heapTotalCommit += peentry.UnionBlock.Region.dwCommittedSize
                            Else
                                heapTotalReserved += peentry.cbData
                            End If
#If DEBUG Then
                        If _ConnectionMode = MemSpectMode.OnLine Then
                            Dim mbi As New MEMORY_BASIC_INFORMATION
                            Dim mbiSize = Marshal.SizeOf(mbi)
                            VirtualQueryEx(_hProcessTarget, peentry.lpData, mbi, CUInt(mbiSize))
                            Debug.Assert(mbi.BaseAddress = peentry.lpData, "base addr")

                        End If
#End If
                            Dim addrToUse = peentry.lpData
                            If (peentry.wFlags And PROCESS_HEAP_ENTRY_WFLAGS.PROCESS_HEAP_ENTRY_DDESHARE) > 0 Then
                                ' we want to round to page boundary
                                Dim temp = peentry.lpData.ToInt32
                                Dim leftover = temp And &HFFFF ' 655355
                                addrToUse = New IntPtr(temp - leftover)
                            End If
                            If dictHeapStuff.ContainsKey(addrToUse) Then
                                dictHeapStuff(addrToUse) = "Heap " + hpdata.GetHeapNameForHeapData
                            Else
                                dictHeapStuff.Add(addrToUse, "Heap " + hpdata.GetHeapNameForHeapData)
                            End If
                        End If
                    Next
                Next

            End If

            Dim lamGetStrDataFromMBI = Function(mbi As MEMORY_BASIC_INFORMATION) As String
                                           Dim strResult = String.Empty
                                           If dictHeapStuff.ContainsKey(mbi.AllocationBase) Then
                                               strResult = dictHeapStuff(mbi.AllocationBase)
                                           Else
                                               strResult = GetFileNameFromMBI(mbi)
                                           End If
                                           Return strResult
                                       End Function



            Dim virtualAllocCalls As List(Of HeapAllocationContainer)
            If _ConnectionMode = MemSpectMode.OnLine Or _ConnectionMode = MemSpectMode.Offline Then
                Dim mspectheap = (From hp In _HeapList Where hp.IsMemSpectHeap).First
                Dim mspectSnap = mspectheap.TakeMemSnapshot(fEnableFilter:=False) ' no filter 
                _VirtualAllocOrFileCalls = (From alloc In mspectSnap.Allocs
                                   Where (alloc.TBlkBlockType = BlockTypes.VirtualAlloc OrElse
                                          alloc.TBlkBlockType = BlockTypes.MapFile OrElse
                                          alloc.TBlkBlockType = BlockTypes.HeapCreate OrElse
                                          alloc.TBlkBlockType = BlockTypes.IndirectInfo)
                                   Order By (alloc.TBlk.Address.ToInt64 And Integer.MaxValue)
                                   ).ToList
                virtualAllocCalls = (From alloc In mspectSnap.Allocs
                                      Where alloc.TBlkBlockType = BlockTypes.VirtualAlloc
                                      Order By alloc.GetAddr.ToInt64
                                      ).ToList
                _FileLoadNotifications = New Dictionary(Of IntPtr, HeapAllocationContainer)
                For Each fileLoadNotificationCtr In From alloc In mspectSnap.Allocs
                                    Where alloc.TBlk.BlockType = BlockTypes.IndirectInfo AndAlso alloc.GetIndirectInfoType = IndirectInfoType.IIType_FileLoad
                                    Order By alloc.AllocationStruct.SeqNo

                    ' order by age so we get the latest, in case an image is loaded/unloaded multiple times, the latest load will have the current addr

                    Dim dispdata = fileLoadNotificationCtr.GetExtraDisplayData
                    Dim addr = Integer.Parse(dispdata, Globalization.NumberStyles.AllowHexSpecifier)
                    Dim intp = New IntPtr(addr)

                    _FileLoadNotifications(New IntPtr(addr)) = fileLoadNotificationCtr
                Next


            Else
                virtualAllocCalls = New List(Of HeapAllocationContainer)

            End If


            'Dim colorClassArray = CType(Array.CreateInstance(GetType(VMColorClass), [Enum].GetValues(GetType(VMColorType)).Length), VMColorClass())
            Dim colorClassArray = New List(Of VMColorClass)([Enum].GetValues(GetType(VMColorType)).Length)

            Dim arrFragFreeVM = CType(
                Array.CreateInstance(
                    GetType(VMFragInfo),
                    [Enum].GetValues(GetType(VMColorType)).Length), 
                    VMFragInfo()
                    )

            For Each clr As VMColorType In [Enum].GetValues(GetType(VMColorType))
                colorClassArray.Add(New VMColorClass With {.colrType = clr})
                arrFragFreeVM(clr) = New VMFragInfo With {.colrType = clr}
            Next

            _VMDataDict = New SortedList(Of IntPtr, VMData)(_CompareIntPtr)
            'Dim VMFrag = Aggregate alloc In _MemBasicInfoList
            '             Where alloc.RegionSize < 65536 AndAlso alloc.State = AllocationState.MEM_FREE
            '             Into Count(), Sum(alloc.RegionSize), Max(alloc.RegionSize)

            'Dim VMLargest = Aggregate alloc In _MemBasicInfoList
            '                Where alloc.State = AllocationState.MEM_FREE AndAlso alloc.AllocationBase.ToInt32 < 0
            '                Into Max(alloc.RegionSize)

            'aggregate throws for empty



            Dim wsPages = WorkingSetInfo.GetWorkingSetDict.Values
            Dim wsIndex = 0
            Dim wsPageCount = WorkingSetInfo.GetWorkingSetDict.Count
            Dim VMLargest2G As UInteger ' largest < 2g
            Dim VMLargest As UInteger ' largest

            Dim VMFragSum As UInteger
            Dim VMFragCnt As Integer
            ' do a parallel walk of mbi and allocsForClr, which are both sorted by addr
            Dim VirtAllocNdx = 0
            Dim VirtAllocNdxLast = virtualAllocCalls.Count - 1
            For Each mbi In GetVirtAllocs().Values
                Dim wsForMBI = 0
                If wsPages IsNot Nothing AndAlso wsIndex < wsPageCount Then
                    While _CompareIntPtr.Compare(wsPages(wsIndex).VirtPage, mbi.BaseAddress) < 0
                        wsIndex += 1
                        If wsIndex >= wsPageCount Then
                            Exit While
                        End If
                    End While
                    Dim endreg = mbi.BaseAddress.MyAdd(mbi.RegionSize)
                    If wsIndex < wsPageCount Then
                        While _CompareIntPtr.Compare(wsPages(wsIndex).VirtPage, endreg) < 0
                            wsForMBI += 4096
                            wsIndex += 1
                            If wsIndex >= wsPageCount Then
                                Exit While
                            End If
                        End While
                    End If
                End If

                Dim dat = New VMData With {
                    .mbi = mbi,
                    .WorkingSet = wsForMBI
                    }
                dat.strData = lamGetStrDataFromMBI.Invoke(mbi)
                _VMDataDict.Add(mbi.BaseAddress, dat)

                If mbi.BaseAddress.ToInt32 <> 0 Then
                    If mbi.State = AllocationState.MEM_FREE Then
                        If mbi.RegionSize < 65536 Then
                            VMFragSum += mbi.RegionSize
                            VMFragCnt += 1
                        End If
                        If (mbi.BaseAddress.ToInt32 And &H80000000) = 0 Then
                            If mbi.RegionSize > VMLargest2G Then
                                VMLargest2G = mbi.RegionSize
                            End If
                        End If
                        If mbi.RegionSize > VMLargest Then
                            VMLargest = mbi.RegionSize
                        End If
                    Else
                        If mbi.BaseAddress = _memSpectSharedMemAddr Then
                            dat.strData = "MemSpect Mapped Shared Memory"
                        Else
                            'see if it's been eaten
                            If Common.MemoryEater._EatenAllocations.ContainsKey(mbi.BaseAddress) Then
                                dat.strData = "MemSpect MemoryEater"
                            Else
                                ' see if it's managed

                                If VirtAllocNdxLast > 0 Then
                                    Dim mbiLow = mbi.AllocationBase.ToInt64 ' go the full range from AllocationBase to end of current MBI
                                    Dim mbiHigh = mbi.BaseAddress.ToInt64 + mbi.RegionSize
                                    Dim curVirtAllocAddr As Long
                                    While VirtAllocNdx < VirtAllocNdxLast
                                        curVirtAllocAddr = virtualAllocCalls(VirtAllocNdx).TBlk.Address.ToInt64
                                        If curVirtAllocAddr >= mbiLow Then
                                            If curVirtAllocAddr < mbiHigh Then
                                                Dim allocCouldMatch = virtualAllocCalls(VirtAllocNdx)
                                                If curVirtAllocAddr < mbiHigh Then

                                                    'd:\memspect\vsassert\mystl.h(103) : Vsassert.dll!MySTLAlloc<std::_Tree_nod<std::_Tmap_traits<std::pair<unsigned long,void *>,TrkBlock,std::less<std::pair<unsigned long,void *> >,MySTLAlloc<std::pair<std::pair<unsigned long,void *>,TrkBlock> >,0> >::_Node>::allocate + 28 bytes
                                                    'd:\w7rtm\minkernel\kernelbase\mmsup.c(354) : KERNELBASE.dll!VirtualAllocEx + 68 bytes
                                                    'd:\w7rtm\minkernel\kernelbase\mmsup.c(336) : KERNELBASE.dll!VirtualAlloc + 24 bytes
                                                    'f:\dd\ndp\clr\src\vm\hosting.cpp(313) : clr.dll!EEVirtualAlloc + 147 bytes

                                                    If dat.strData.Length > 0 Then
                                                        ' already filled with heap info
                                                        '                                                        CommonUI.UpdateStatusMsgDbg("calcman " + mbiLow.ToString("x8") + " " + allocCouldMatch.TblkBlockType.ToString + " " + allocCouldMatch.ToString + " " + dat.strData)
                                                    Else
                                                        Dim fGotAllocator = False
                                                        Dim nStackLevel = 3 ' 3rd on stack is clr.dll. On Dev11, it's 4
                                                        While Not fGotAllocator
                                                            Dim addrCouldBeClr = allocCouldMatch.GetCallStackAddr(nStackLevel)
                                                            Dim sym = ResolveAddressToSymbol(addrCouldBeClr, fStripFileName:=True, fStripBytesToo:=True).ToLower
                                                            If sym.StartsWith("clr.dll") OrElse sym.StartsWith("clrjit.dll") OrElse sym.StartsWith("mscorwks.dll") Then
                                                                dat.strData += "Managed "
                                                                fGotAllocator = True
                                                            End If
                                                            nStackLevel += 1
                                                            If nStackLevel = 6 Then ' safety exit
                                                                Exit While
                                                            End If


                                                        End While
                                                        '                            Debug.Assert(dat.strData.Length = 0, "calc managed: strdata already used " + dat.strData)

                                                        Dim descFrame = String.Join("/",
                                                                                    From frame In allocCouldMatch.GetCallStackAddressestoArray(nMaxStackFrames:=9)
                                                                                    Skip 4
                                                                                    Take 4
                                                                                    Select
                                                                                    ResolveAddressToSymbol(frame, fStripFileName:=True, fStripBytesToo:=True)
                                                                                    )

                                                        'Dim descFrame = ResolveAddressToSymbol(allocCouldMatch.GetCallStackAddr(5), fStripFileName:=True, fStripBytesToo:=True) + "/" +
                                                        '               ResolveAddressToSymbol(allocCouldMatch.GetCallStackAddr(6), fStripFileName:=True, fStripBytesToo:=True) + "/" +
                                                        '               ResolveAddressToSymbol(allocCouldMatch.GetCallStackAddr(7), fStripFileName:=True, fStripBytesToo:=True)
                                                        dat.strData += String.Format(descFrame)
                                                    End If
                                                End If

                                            End If
                                            Exit While
                                        End If
                                        VirtAllocNdx += 1
                                    End While
                                End If

                            End If

                        End If
                    End If
                End If
            Next 'done iterating all MBIs

            Dim threadlist = GetLiveThreads() 'live in snapshot or really live
            Dim nVMTotForAllStks As ULong = 0
            Dim nVMWastedAllStks As ULong = 0
            Dim nVMGuardAllStks As ULong = 0
            Dim nVMUsedForAllStks As ULong = 0

            For Each thrd In threadlist.Values
                Dim mbiStackLow = GetMBIForAddress(thrd.StackBase.MyAdd(-1), GetVirtAllocs)
                Dim vmdat1 = _VMDataDict(mbiStackLow.AllocationBase)
                Dim ndx1 = _VMDataDict.IndexOfKey(mbiStackLow.AllocationBase)

                Dim vmdatIter = vmdat1
                Dim nCntSegs = 0
                Dim nVMTotForThisStk As ULong = 0
                Dim nVMWastedForThisStk As ULong = 0
                Dim nVMGuardForThisStk As ULong = 0
                Dim nVMUsedForThisStk As ULong = thrd.StackBase.MyToULong - thrd.StackLimit.MyToULong
                Do While vmdat1.mbi.AllocationBase = mbiStackLow.AllocationBase
                    nCntSegs += 1
                    If (vmdat1.mbi.Protect And AllocationProtect.PAGE_GUARD) > 0 Then
                        nVMGuardForThisStk += vmdat1.mbi.RegionSize
                    End If
                    nVMTotForThisStk += vmdat1.mbi.RegionSize
                    vmdat1 = _VMDataDict.Values(ndx1 + nCntSegs)
                Loop
                nVMWastedForThisStk = nVMTotForThisStk - nVMUsedForThisStk

                nVMTotForAllStks += nVMTotForThisStk
                nVMWastedAllStks += nVMWastedForThisStk
                nVMGuardAllStks += nVMGuardForThisStk
                nVMUsedForAllStks += nVMUsedForThisStk
                For i = 0 To nCntSegs - 1
                    Dim vmdatSeg = _VMDataDict.Values(ndx1 + i)
                    Dim strThreadData = String.Format("Stack Tid={0} Seg# {1}", thrd.ThreadId, nCntSegs - i - 1)
                    'If Not String.IsNullOrEmpty(vmdatSeg.strData) Then
                    '    ' these are from clr.dll!Thread::CLRSetThreadStackGuarantee
                    '    '                        CommonUI.UpdateStatusMsgDbg("thread strdata already occupied " + vmdatSeg.strData + " " + strThreadData)
                    'End If
                    vmdatSeg.strData = strThreadData
                    If True OrElse i = nCntSegs - 1 Then
                        vmdatSeg.strData += String.Format(" VMTot={0:n0} Used={1:n0} VMWaste={2:n0} VMGuard={3:n0} {4}",
                                                          nVMTotForThisStk,
                                                          nVMUsedForThisStk,
                                                          nVMWastedForThisStk,
                                                          nVMGuardForThisStk,
                                                          thrd.Note + " " + thrd.ThreadProc
                                                          )
                    End If
                Next
            Next

            Dim gcstatdat = GCStats.GetGCStats

            Dim memspectrcwthread = gcstatdat.g_MemSpectBgdThreadCleanupRCW

            Dim ThrdStackSummary = String.Format("#ThrdStacks={0} VMTot={1:n0} VMUsed={2:n0} Waste={3:n0} Guard={4:n0} MemSpectRCW={5:n0}",
                                                 threadlist.Count.ToString,
                                                 nVMTotForAllStks,
                                                 nVMUsedForAllStks,
                                                 nVMWastedAllStks,
                                                 nVMGuardAllStks,
                                                 memspectrcwthread)


            ' now that we've gathered data, iterate through to get totals & colors
            Dim priorValue As VMData = Nothing
            For Each vmdat In _VMDataDict.Values
                Dim colordat = VMColorClass.GetVMColorType(vmdat)
                If colordat.colrType = VMColorType.Mapped AndAlso String.IsNullOrEmpty(vmdat.strData) Then
                    vmdat.strData = "Mapped File"
                End If
                Dim thiscolor = colordat.colrType
                vmdat.bgColorClass = colordat
                Dim arrEntry = colorClassArray(CInt(thiscolor))
                arrEntry.BlkCnt += 1
                arrEntry.TotalSize += vmdat.mbi.RegionSize
                arrEntry.WorkingSet = CULng(arrEntry.WorkingSet + vmdat.WorkingSet)
                If vmdat.mbi.State = AllocationState.MEM_COMMIT Then
                    arrEntry.Committed += vmdat.mbi.RegionSize
                ElseIf vmdat.mbi.State = AllocationState.MEM_FREE Then
                    If vmdat.mbi.RegionSize < 65536 Then
                        If priorValue IsNot Nothing Then
                            arrFragFreeVM(priorValue.bgColorClass.colrType).nBlks += 1
                            arrFragFreeVM(priorValue.bgColorClass.colrType).nSize += vmdat.mbi.RegionSize
                            arrFragFreeVM(VMColorType.All).nBlks += 1
                            arrFragFreeVM(VMColorType.All).nSize += vmdat.mbi.RegionSize
                            vmdat.strData = "Unusable FreeFrag " + priorValue.bgColorClass.colrType.ToString
                        End If
                    End If
                End If
                priorValue = vmdat
            Next
            For i = 1 To [Enum].GetValues(GetType(VMColorType)).Length - 1
                colorClassArray(0).TotalSize += colorClassArray(i).TotalSize
                colorClassArray(0).Committed += colorClassArray(i).Committed
                colorClassArray(0).BlkCnt += colorClassArray(i).BlkCnt
                colorClassArray(0).WorkingSet += colorClassArray(i).WorkingSet
            Next


            _ctrlsVM = DataWindowMain.MakeNewDatasurface(
                "VM",
                "Virtual Memory Map: the Entire address space mapped." +
                "32 bit OS the max is 2G (7FFFFFFF) (or if LargeAddress aware, 0xbffffffff)" +
                "64 bit OS, the max is 4G (fffe0000)" +
                "You can create multiple VM snaps (especially useful when attached to an Existing process (with no MemSpect injected code in it)",
                nMaxHeaderHeight:=nMaxHeader)
            If _ConnectionMode <> MemSpectMode.OnLine Then
                _MEMORYSTATUSEX = New MEMORYSTATUSEX
            Else
                SendMsg(ProcMsgVerb.GetSharedMem, fSendEndMsgSync:=True)
                Marshal.PtrToStructure(_SharedMemAddr, _MEMORYSTATUSEX)
            End If
            Dim spHoriz = New StackPanel With {.Orientation = Orientation.Horizontal}

            Dim spBtns As New StackPanel With {.Orientation = Orientation.Vertical}
            spHoriz.Children.Add(spBtns)

            Dim btnMemLayout = New Button With {
                .Content = "_Graph memory layout",
                .ToolTip = "Show a graph of address space layout with regions"
            }
            spBtns.Children.Add(btnMemLayout)
            AddHandler btnMemLayout.Click, Sub()
                                               ShowVMGraphLayout()
                                           End Sub

            Dim btnAnalyze = New Button With {
                .Content = "_Analyze Images",
                .ToolTip = "Show Image data: Collect callstacks of DLL load, embedded resources duplicates, Imported DLLs in the image, embedded data." + vbCrLf +
                                            "Shows Optimization data. Takes a minute or 2"
            }
            spBtns.Children.Add(btnAnalyze)
            AddHandler btnAnalyze.Click, Sub()
                                             Dim x = New ImageUi(Me)
                                             _browVM._BrowseList.Items.Filter = Nothing ' no filter for images
                                             _imageAnalyzer = Images.ImageAnalyzer.CreateImageAnalyzer()
                                             x.DoShowImageData(
                                                 _imageAnalyzer,
                                                 _imageAnalyzer._lstImageSymbolNames,
                                                 _imageAnalyzer._ImageResourcesDict,
                                                 _imageAnalyzer._lstImageData)
                                         End Sub


            Dim btnVMFrag = New Button With {
                .Content = "Free VM _Fragmentation",
                .ToolTip = "Show the free fragment along with the associated fragmenter. A VirtualAlloc has 64K granularity for a 32 bit process"
                }
            spBtns.Children.Add(btnVMFrag)
            AddHandler btnVMFrag.Click, Sub()
                                            DoShowFreeFragmentation(_VMDataDict)
                                        End Sub
            Dim btnWorkSet = New Button With {
                .Content = "_WorkingSet",
                .ToolTip = "Show the WorkingSet page details"
            }
            spBtns.Children.Add(btnWorkSet)
            AddHandler btnWorkSet.Click, Sub()
                                             ShowWorkingSetInfo()
                                         End Sub



            Dim btnWasteDetails = New Button With {
                .Content = "_Details such as waste",
                .ToolTip = "Calculate empty (zeros) memory blocks: takes a few seconds longer to analyze. " +
                        vbCrLf + "Adds a few columns to the normal VM view, such as zero blocks"
                }
            spBtns.Children.Add(btnWasteDetails)
            AddHandler btnWasteDetails.Click, Sub()
                                                  Dim opts = New VMDisplayOptions
                                                  opts.fShowDetail = True
                                                  Using New DataWindowMain.SetCursorWait
                                                      DataWindowMain._TabControl.Items.Remove(DataWindowMain._TabControl.SelectedItem)
                                                      Dim vm = New VirtualMem
                                                      vm.ShowVirtualAllocs(opts)  'recur
                                                  End Using
                                              End Sub


            Dim qColorList = From filtcolor In colorClassArray
                      Select Filter = filtcolor.colrType.ToString,
                      filtcolor.TotalSize,
                      filtcolor.Committed,
                      filtcolor.WorkingSet,
                      WSetPct = If(filtcolor.TotalSize = 0, 0, CInt(filtcolor.WorkingSet / CDbl(filtcolor.TotalSize) * 100)),
                      Count = filtcolor.BlkCnt,
                      _bgcolor = VMColorClass.GetVMColorType(filtcolor.colrType.ToString)


            _browColors = New Browse(qColorList, ColWidths:={55, 95, 90, 90, 60, 60},
                                       fAllowHeaderClickSort:=False,
                                       colTips:={"Click on an item to filter the VM view. Fragment means Free<64k",
                                                 "Total Size of region type",
                                                 "Memory Committed of region type",
                                                 "Working Set of region type",
                                                 "Working Set % of Total of region type",
                                                 "# of blocks in this region type"
                                                })
            _browColors.LayoutTransform = New ScaleTransform(0.75, 0.75)
            _browColors.SetDynamicBackground(New VMBackgroundConverter, fAdditive:=False)
            spHoriz.Children.Add(_browColors)


            Dim vaAggState = From va In GetVirtAllocs().Values
                             Group By va.State Into Sum(va.RegionSize), Count()
                             Select State = State.ToString,
                                     Count,
                                     Sum
                             Order By Sum Descending

            Dim bAggState = New Browse(vaAggState, ColWidths:={80, 50, 90})
            bAggState.MaxHeight = nMaxHeader
            bAggState._BrowseList.LayoutTransform = New ScaleTransform(sumrySize, sumrySize)

            spHoriz.Children.Add(bAggState)

            Dim qFragFree = From va In arrFragFreeVM
                            Where va.colrType <> VMColorType.Free AndAlso va.colrType <> VMColorType.Fragment
                            Select
                            FragType = va.colrType.ToString + If(va.colrType <> VMColorType.Free, " Frag", ""),
                            Count = va.nBlks,
                            Sum = va.nSize.ToString("n0")

            Dim bFragFree = New Browse(qFragFree,
                                       ColWidths:={80, 50, 90},
                                       colTips:={"VM Free blocks unusable due to Fragmentation. ""All"" is the sum"}
                                       )
            bFragFree.MaxHeight = nMaxHeader
            bFragFree._BrowseList.LayoutTransform = New ScaleTransform(sumrySize, sumrySize)
            bFragFree._BrowseList.ContextMenu.AddMnuItem(
                "_Show Free VM Fragmentation",
                "show the free fragment along with the associated fragmenter. A VirtualAlloc has 64K granularity",
                Sub()
                    DoShowFreeFragmentation(_VMDataDict)
                End Sub,
                0
                )
            spHoriz.Children.Add(bFragFree)


            Dim vaAggType = From va In GetVirtAllocs().Values
                            Group By va.lType Into Sum(va.RegionSize), Count()
                            Select Type = lType.ToString,
                                Count,
                                Sum
                            Order By Sum Descending

            Dim bAggType = New Browse(vaAggType, ColWidths:={80, 50, 90})

            bAggType._BrowseList.LayoutTransform = New ScaleTransform(sumrySize, sumrySize)
            bAggType.MaxHeight = nMaxHeader

            spHoriz.Children.Add(bAggType)


            Dim vaaggProt = From va In GetVirtAllocs().Values
                            Group By va.Protect Into Sum(va.RegionSize), Count()
                            Select Protect = Protect.ToString,
                            Count,
                            Sum
                            Order By Sum Descending

            Dim baggProt = New Browse(vaaggProt, ColWidths:={170, 50, 90})
            baggProt._BrowseList.LayoutTransform = New ScaleTransform(sumrySize, sumrySize)
            baggProt.MaxHeight = nMaxHeader

            spHoriz.Children.Add(baggProt)

            Dim spVert = New StackPanel With {.Orientation = Orientation.Vertical}


            spVert.Children.Add(New TextBlock With {
                                             .Text = String.Format("Seqno ={0:n0} {1}", GetGlobalPassCount, vmStats),
                                             .FontFamily = FontFamilyCourierNew,
                                             .FontSize = 9
                                         })
            If _ConnectionMode = MemSpectMode.OnLine Then
                spVert.Children.Add(New TextBlock With {
                                                 .Text = _MEMORYSTATUSEX.ToString,
                                                 .FontFamily = FontFamilyCourierNew,
                                                 .FontSize = 9
                                             })

            End If

            Dim heapstats = String.Format("Heap Total = {0:n0} Commited = {1:n0}, Resrv = {2:n0}",
                                                         heapTotalCommit + heapTotalReserved,
                                                         heapTotalCommit,
                                                         heapTotalReserved)
            spVert.Children.Add(New Label With {
                                 .Content = "Lrgst Avail Free <2g = " + VMLargest2G.ToString("n0") +
                                 " Lrgst Avail " + VMLargest.ToString("n0") +
                                 "     " + heapstats,
                                .ToolTip = "All MEM_FREE blocks < 65536 ",
                                .FontSize = 9
                             })


            spVert.Children.Add(New Label With {
                                 .Content = ThrdStackSummary,
                                 .ToolTip = "Thread Stack VM use",
                                 .FontSize = 9
                             })

            spVert.Children.Add(New Label With {
                                .Content = String.Format("Working Set = {0:n0} bytes  ({1:n0}K)", wsPageCount * 4096, wsPageCount * 4),
                                 .ToolTip = String.Format("Working set in bytes"),
                                 .FontSize = 9
                            }
                        )


            spHoriz.Children.Add(spVert)

            _ctrlsVM.SurfaceHeader.Children.Add(spHoriz)

            _browVM = MakeVMBrowse(_VMDataDict, displayOptions)


            AddHandler _browColors._BrowseList.SelectionChanged, Sub(sender As Object, e As RoutedEventArgs)
                                                                     Dim brlist = CType(sender, Browse.BrowseList)
                                                                     Dim curfilter As VMColorType = VMColorType.All
                                                                     Dim selitems = brlist.SelectedItems
                                                                     For Each itm In selitems
                                                                         Dim filt = CStr(ComponentModel.TypeDescriptor.GetProperties(itm)("Filter").GetValue(itm))
                                                                         curfilter = CType([Enum].Parse(GetType(VMColorType), filt), VMColorType)
                                                                         Exit For ' only 1st selected item
                                                                     Next
                                                                     '                                                                     UpdateStatusMsgDbg("filt " + curfilter.ToString)
                                                                     If curfilter = VMColorType.All Then
                                                                         _browVM._BrowseList.Items.Filter = Nothing
                                                                     Else
                                                                         _browVM._BrowseList.Items.Filter = Function(itm As Object) As Boolean
                                                                                                                Dim fIsMatch = False

                                                                                                                Dim result = CType(ComponentModel.TypeDescriptor.GetProperties(itm)("_bgcolor").GetValue(itm), VMColorClass)
                                                                                                                If result.colrType = curfilter Then
                                                                                                                    fIsMatch = True
                                                                                                                End If
                                                                                                                'Dim res = VMColorClass.GetVMColorType(itm)
                                                                                                                'If res.colrType = curfilter Then
                                                                                                                '    fIsMatch = True
                                                                                                                'End If
                                                                                                                Return fIsMatch
                                                                                                            End Function

                                                                     End If

                                                                 End Sub



            _ctrlsVM.SurfaceDetails.Children.Add(_browVM)

            AddHandler _ctrlsVM.SurfaceDetails.MouseLeave, Sub() ClearPriorToolTipIfAny()



            Return _ctrlsVM
        End Function

        Friend Shared Function CountEmptyVMBytes(
                                                ByVal mbi As MEMORY_BASIC_INFORMATION,
                                                Optional ByVal addrStart As IntPtr = Nothing,
                                                Optional ByVal nSize As UInteger = 0
                                    ) As Tuple(Of Integer, Integer, Integer)
            Dim res = New Tuple(Of Integer, Integer, Integer)(0, 0, 0) ' 
            If addrStart = IntPtr.Zero Then
                addrStart = mbi.BaseAddress
                nSize = mbi.RegionSize
            End If
            If (mbi.State And AllocationState.MEM_COMMIT) > 0 Then
                Dim blk As New ProcMemBlockByte
                ' read backwards from end, counting consecutive 0 bytes
                Dim dwBytesRead As Integer
                Dim nNumZerosTrailing = 0
                Dim nTotNumZeros = 0
                Dim nNumZerosBiggestSoFar = 0
                Dim nNumZerosCurrentBlock = 0
                Dim nBlks = CInt(nSize / BlockSize)
                Dim fWithinTrailingRegion = True
                Dim fWithinZeroBlock = True
                Dim nCntIsProbablyDemandZeroPage = 0
                For blkNdx = nBlks - 1 To 0 Step -1
                    Dim addr = addrStart.MyAdd(blkNdx * BlockSize)
                    If _ConnectionMode = MemSpectMode.OnLine OrElse _ConnectionMode = MemSpectMode.Existing Then
                        If ReadProcessMemoryByte(_hProcessTarget, addr, blk, BlockSize, dwBytesRead) = 0 Then
                            Dim err = Marshal.GetLastWin32Error ' 299 = Partial ReadProcMem
                            If err <> 299 Then
                                UpdateStatusMsg("fail readRemoteMem " + addr.ToString + " " + BlockSize.ToString + " " + err.ToString + " " + GetErrorMessageFromWin32LastError(err), fAssert:=False)
                                Exit For
                            Else

                                nCntIsProbablyDemandZeroPage += 1
                            End If
                        End If
                        'Debug.Assert(dwBytesRead = BlockSize)
                    Else
                        Try
                            blk.data = MiniDumpReader.Singleton.ReadMemoryDictionary(addr, BlockSize)
                            If blk.data IsNot Nothing Then
                                dwBytesRead = blk.data.Length
                            Else
                                dwBytesRead = 0
                            End If
                        Catch ex As Exception
                            Exit For
                        End Try
                    End If
                    For datNdx = dwBytesRead - 1 To 0 Step -1
                        If blk.data(datNdx) = 0 Then
                            nTotNumZeros += 1
                            If fWithinTrailingRegion Then
                                nNumZerosTrailing += 1
                            End If
                            nNumZerosCurrentBlock += 1
                        Else
                            If nNumZerosCurrentBlock > nNumZerosBiggestSoFar Then
                                nNumZerosBiggestSoFar = nNumZerosCurrentBlock
                            End If
                            nNumZerosCurrentBlock = 0
                            fWithinZeroBlock = False
                            fWithinTrailingRegion = False

                        End If
                    Next ' next byte
                Next ' next block
                If nCntIsProbablyDemandZeroPage = nBlks Then
                    nNumZerosTrailing = CInt(nSize)
                    nNumZerosBiggestSoFar = nNumZerosTrailing
                    nTotNumZeros = nNumZerosTrailing
                End If
                If nNumZerosCurrentBlock > nNumZerosBiggestSoFar Then ' never got a non-zero
                    nNumZerosBiggestSoFar = nNumZerosCurrentBlock
                End If
                res = New Tuple(Of Integer, Integer, Integer)(nNumZerosTrailing, nNumZerosBiggestSoFar, nTotNumZeros)
            ElseIf (mbi.State And AllocationState.MEM_RESERVE) > 0 Then
                res = New Tuple(Of Integer, Integer, Integer)(0, 0, 0)
#If DEBUGxxx Then
            ElseIf (mbi.State And AllocationState.MEM_FREE) > 0 Then
                Debug.Assert(False, "got free MBI for count empty?")
#End If
            End If
            Return res

        End Function

        'Sub OnbtnNewVmAllocsClick(ByVal sender As Object, ByVal e As RoutedEventArgs) Handles _btnNewVMAllocs.Click
        '    '            ShowVMAllocDiff(fNewAllocs:=True)
        'End Sub

        'Sub OnbtnFreedVMAllocsClick(ByVal sender As Object, ByVal e As RoutedEventArgs) Handles _btnFreedVMAllocs.Click
        '    '           ShowVMAllocDiff(fNewAllocs:=False)
        'End Sub

        'Sub ShowVMAllocDiff(ByVal fNewAllocs As Boolean)
        '    Try
        '        Dim NewSnap = GetVirtAllocs()

        '        Dim memsnap1 As List(Of MEMORY_BASIC_INFORMATION)
        '        Dim memsnap2 As List(Of MEMORY_BASIC_INFORMATION)

        '        If fNewAllocs Then
        '            'new allocations: all in new which aren't in old
        '            memsnap1 = NewSnap
        '            memsnap2 = _snapshot
        '        Else
        '            ' freed allocations: all in old which aren't in new
        '            memsnap1 = _snapshot
        '            memsnap2 = NewSnap

        '        End If

        '        Dim qDetails = From a In memsnap1
        '                       Where Not (From heapTmp In memsnap2
        '                                  Select heapTmp.BaseAddress
        '                                  ).Contains(a.BaseAddress)
        '            Select BaseAddress = a.BaseAddress.ToInt32.ToString("x8"),
        '                    AllocationBase = a.AllocationBase.ToInt32.ToString("x8"),
        '                    RegionSize = a.RegionSize.ToString("x8"),
        '                    Filename = GetFileNameFromMBI(a),
        '                    AllocationProtect = CType(a.AllocationProtect, AllocationProtect).ToString(),
        '                    State = CType(a.State, AllocationState).ToString(),
        '                    Type = CType(a.lType, AllocationType).ToString


        '        Dim oWin = New MyWindow(String.Format(
        '                "Snapshot Diff TotDiffCnt = {0:n0} ", qDetails.Count))

        '        oWin.Content = New Browse(
        '            qDetails,
        '            delMouseMove:=AddressOf On_VirtmemMouseMove,
        '            delDblClick:=AddressOf On_VirtMemDblClick
        '            )
        '        oWin.Show()
        '        _snapshot = NewSnap
        '    Catch ex As Exception
        '        MemSpectExceptionHandler(ex)
        '    End Try
        'End Sub
        Public Class VMStatistics
            Private nAddr As UInteger
            Public nTotMEM_COMMIT As UInteger
            Public nCommitAllocs As Integer
            Public nTotMEM_RESERVE As UInteger
            Public nReserveAllocs As Integer
            Public nTotMEM_Free As UInteger
            Public nFree As Integer
            Public nAllocs As Integer
            Public nLargestFree As UInteger
            Public Shared Function GetVirtualMemorystats() As VMStatistics
                Dim res = New VMStatistics
                Dim vmallocs = GetVirtAllocs().Values
                For Each vAlloc In vmallocs
                    res.nAddr += vAlloc.RegionSize
                    Select Case vAlloc.State
                        Case AllocationState.MEM_COMMIT
                            res.nCommitAllocs += 1
                            res.nTotMEM_COMMIT += vAlloc.RegionSize
                        Case AllocationState.MEM_RESERVE
                            res.nTotMEM_RESERVE += vAlloc.RegionSize
                            res.nReserveAllocs += 1
                        Case AllocationState.MEM_FREE
                            res.nFree += 1
                            res.nTotMEM_Free += vAlloc.RegionSize
                            If vAlloc.RegionSize > res.nLargestFree Then
                                res.nLargestFree = vAlloc.RegionSize
                            End If
                    End Select
                Next
                Dim pctFull = CInt(100.0 * ((res.nTotMEM_COMMIT + res.nTotMEM_RESERVE) / res.nAddr))
                Dim pctFrag = CInt(100.0 * res.nFree / res.nCommitAllocs)

                'retval = String.Format("Commit = {0:n0}({1:n0}) Res = {2:n0}({3:n0}) Free= {4:n0}({5:n0}) %Full ={6} %Frag={7} TotVM={8:n0} LrgstFree={9:n0}",
                '                            res.nTotMEM_COMMIT, res.nCommit,
                '                            res.nTotMEM_RESERVE, res.nReserve,
                '                            res.nTotMEM_Free, res.nFree,
                '                            pctFull,
                '                            pctFrag,
                '                            res.nAddr,
                '                            res.nLargestFree
                '                            )
                Return res
            End Function

            Public ReadOnly Property ToolTip As String
                Get
                    Dim strToolTip = String.Format("Commit={0:n0} Reserved={1:n0} nBlks={2:n0}", nTotMEM_COMMIT, nTotMEM_RESERVE, nAllocs)
                    Return strToolTip
                End Get
            End Property

            Public Overrides Function ToString() As String
                Dim retval = String.Format("Commit = {0:n0}({1:n0}) Res = {2:n0}({3:n0}) Free= {4:n0}({5:n0}) %Full ={6} %Frag={7} TotVM={8:n0} LrgstFree={9:n0}",
                                            nTotMEM_COMMIT, nCommitAllocs,
                                            nTotMEM_RESERVE, nReserveAllocs,
                                            nTotMEM_Free, nFree,
                                            CInt(100.0 * ((nTotMEM_COMMIT + nTotMEM_RESERVE) / nAddr)),
                                            CInt(100.0 * nFree / nCommitAllocs),
                                            nAddr,
                                            nLargestFree
                                            )
                Return retval 'String.Format("Commit = {%0:n0} Reser{%1:n0} {%2:n0}", Commit, Reserved, Free)
            End Function
        End Class

        'Shared Function GetVirtualMemorystats(ByRef strToolTip As String, ByRef nTotMEM_FREE As UInteger, ByRef nLargestFree As UInteger) As String
        '    Dim retval = ""
        '    Dim vmallocs = GetVirtAllocs()
        '    Dim nAddr As UInteger = 0
        '    Dim nTotMEM_COMMIT As UInteger = 0
        '    Dim nTotMEM_RESERVE As UInteger = 0
        '    Dim nFree = 0
        '    Dim nCommit = 0
        '    Dim nReserve = 0
        '    For Each a In vmallocs
        '        nAddr += a.RegionSize
        '        Select Case a.State
        '            Case AllocationState.MEM_COMMIT
        '                nCommit += 1
        '                nTotMEM_COMMIT += a.RegionSize
        '            Case AllocationState.MEM_RESERVE
        '                nTotMEM_RESERVE += a.RegionSize
        '                nReserve += 1
        '            Case AllocationState.MEM_FREE
        '                nFree += 1
        '                nTotMEM_FREE += a.RegionSize
        '                If a.RegionSize > nLargestFree Then
        '                    nLargestFree = a.RegionSize
        '                End If
        '        End Select
        '    Next
        '    Dim pctFull = CInt(100.0 * ((nTotMEM_COMMIT + nTotMEM_RESERVE) / nAddr))
        '    Dim pctFrag = CInt(100.0 * nFree / nCommit)
        '    strToolTip = String.Format("Commit={0:n0} Reserved={1:n0} nBlks={2:n0}", nTotMEM_COMMIT, nTotMEM_RESERVE, vmallocs.Count)
        '    '            retval = String.Format("VM: LrgstFree={0:n0} TotFree={1:n0}", nLargestFree, nTotMEM_FREE)

        '    retval = String.Format("Commit = {0:n0}({1:n0}) Res = {2:n0}({3:n0}) Free= {4:n0}({5:n0}) %Full ={6} %Frag={7} TotVM={8:n0} LrgstFree={9:n0}",
        '                                nTotMEM_COMMIT, nCommit,
        '                                nTotMEM_RESERVE, nReserve,
        '                                nTotMEM_FREE, nFree,
        '                                pctFull,
        '                                pctFrag,
        '                                nAddr,
        '                                nLargestFree
        '                                )
        '    Dim thrsh = GetPrivateProfileInt(ProfileStringSection, "VMFreezeThreshold", 0, _iniFileName) * 2 ^ 20
        '    If thrsh > 0 Then
        '        If nTotMEM_COMMIT + nTotMEM_RESERVE >= thrsh Then
        '            CommonUI.UpdateStatusMsg("Freezing due to thrsh =" + thrsh.ToString("n0") + ". TotCom + Res =  " + (nTotMEM_COMMIT + nTotMEM_RESERVE).ToString("n0"))
        '            VBDiagMarginBase.FreezeTargetThreads()
        '        End If
        '    End If

        '    Return retval
        'End Function



        Friend Shared Sub On_VirtMemDblClick(ByVal sender As Object, ByVal e As RoutedEventArgs)
            Dim lv As Browse.BrowseList = Nothing
            Try
                lv = TryCast(sender, Browse.BrowseList)
                If lv IsNot Nothing Then
                    lv.Cursor = Cursors.Wait
                    Dim tb = TryCast(e.OriginalSource, TextBlock)
                    If tb IsNot Nothing Then
                        Select Case tb.Name
                            Case "AllocationBase", "BaseAddress"
                                ClearPriorToolTipIfAny()
                                ' can have hctr, from MemSpecctWin of VirtualAlloc calls
                                Dim tdescHctr = ComponentModel.TypeDescriptor.GetProperties(tb.DataContext)("_HeapAllocationContainer")
                                If tdescHctr IsNot Nothing Then
                                    Dim hctr = CType(tdescHctr.GetValue(tb.DataContext), HeapAllocationContainer)
                                    BrowseMem.ShowStacksInNotepad(Nothing, hctr.ToString, hctr) ' some allocs are too big: take forever. On Dblclick, limit to 64k. On Context menu, get all
                                Else
                                    Dim tdesc = ComponentModel.TypeDescriptor.GetProperties(tb.DataContext)("_mbi")
                                    Dim mbi = CType(tdesc.GetValue(tb.DataContext), MEMORY_BASIC_INFORMATION)
                                    Dim itm = lv.SelectedItem

                                    DumpMBIToNotepad(mbi, If(itm IsNot Nothing, itm.ToString, ""))

                                End If
                        End Select
                    End If
                End If
            Catch ex As Exception
                CommonUI.MemSpectExceptionHandler(ex)
            Finally
                If lv IsNot Nothing Then
                    lv.Cursor = Cursors.Arrow
                End If
            End Try
        End Sub


        Shared Function DumpMBIToNotepad(ByVal mbi As MEMORY_BASIC_INFORMATION, ByVal strTitle As String, Optional ByVal nMaxDumpSize As Integer = 1024) As String
            Dim addr = mbi.BaseAddress
            Dim stk = strTitle + vbCrLf + GetCallStackForMBI(mbi)
            If Not String.IsNullOrEmpty(stk) Then
                stk += vbCrLf
            End If

            Dim strAddrDump = GetMemoryDump(
                    addr,
                    CInt(mbi.RegionSize),
                    nMaxDumpSize
                    )

            Dim filename = WriteOutputToTempFile(String.Format("{0} VMem dump Address = {1:x8}, size = {2}, size = {3}",
                                        stk,
                                        addr.ToInt32,
                                        mbi.RegionSize.ToString("x8"),
                                        mbi.RegionSize
                                        ) + vbCrLf +
                                    strAddrDump)

            Return filename
        End Function

        Friend Shared _LastTipObj As FrameworkElement

        Shared Sub ClearPriorToolTipIfAny()
            If _LastTipObj IsNot Nothing Then
                Dim lastTip = CType(_LastTipObj.ToolTip, ToolTip)
                lastTip.IsOpen = False
                _LastTipObj = Nothing
            End If
        End Sub

        Friend Shared Function GetCallStackForMBI(ByVal mbi As MEMORY_BASIC_INFORMATION, Optional ByRef hctr As HeapAllocationContainer = Nothing) As String
            Dim stk = ""
            ' Sometimes there are more than 1 callstacks for an addr,
            ' e.g.  VirtualAlloc->MemMappedFile
            Dim lamIsInRange = Function(addr As IntPtr, alloc As HeapAllocationContainer) As Boolean
                                   Dim startaddr As ULong = addr.MyToULong
                                   Dim endaddr = startaddr + mbi.RegionSize - 1
                                   Dim resIsInRange = False
                                   If startaddr >= (alloc.GetAddr.MyToULong) Then
                                       If endaddr <= (alloc.GetAddr.MyToULong + mbi.RegionSize) Then
                                           resIsInRange = True
                                       End If
                                   End If
                                   Return resIsInRange
                               End Function

            Dim hctrRef = hctr ' can't use byref in lambda
            Dim lamTry = Function(addr As IntPtr)
                             If _FileLoadNotifications IsNot Nothing AndAlso _FileLoadNotifications.ContainsKey(addr) Then
                                 Dim hctrFileLoad = _FileLoadNotifications(addr)
                                 stk = hctrFileLoad.ToString + vbCrLf + hctrFileLoad.GetCallStackAsString
                             Else
                                 If _VirtualAllocOrFileCalls IsNot Nothing Then

                                     For i = 0 To _VirtualAllocOrFileCalls.Count - 1 ' these are sorted by addr
                                         Dim alloc = _VirtualAllocOrFileCalls(i)

                                         If lamIsInRange.Invoke(addr, alloc) Then
                                             Dim res = GetStacksAndDump(alloc, nMaxDumpSize:=0)
                                             stk += res.Key
                                             If hctrRef IsNot Nothing Then
                                                 hctrRef = alloc ' arbitrary:return 1st one found
                                             End If
                                             Dim nCnt = 1
                                             Do While i + nCnt < _VirtualAllocOrFileCalls.Count
                                                 Dim alloc2nd = _VirtualAllocOrFileCalls(i + nCnt)
                                                 If lamIsInRange.Invoke(addr, alloc2nd) Then
                                                     If nCnt = 1 Then
                                                         stk = "Multiple Stacks!!! Stack # " + nCnt.ToString + vbCrLf + stk ' prepend
                                                     End If
                                                     stk += vbCrLf + vbCrLf + "Multiple Stacks!!! Stack # " + (nCnt + 1).ToString + vbCrLf
                                                     res = GetStacksAndDump(alloc2nd, nMaxDumpSize:=0)
                                                     stk += res.Key
                                                 End If
                                                 nCnt += 1
                                             Loop
                                             Exit For

                                         End If
                                     Next
                                 End If
                             End If

                             Return stk
                         End Function

            stk = lamTry.Invoke(mbi.BaseAddress)
            If String.IsNullOrEmpty(stk) Then
                If mbi.BaseAddress <> mbi.AllocationBase Then
                    If mbi.AllocationBase <> IntPtr.Zero Then
                        stk = lamTry.Invoke(mbi.AllocationBase)
                    End If
                End If
            End If
            hctr = hctrRef
            Return stk
        End Function

        Friend Shared Sub On_VirtmemMouseMove(ByVal sender As Object, ByVal e As RoutedEventArgs)
            Try
                Dim lv = TryCast(sender, Browse.BrowseList)
                If lv IsNot Nothing Then
                    Dim tb = TryCast(e.OriginalSource, TextBlock)
                    If tb IsNot Nothing Then
                        '                    Dim o = lv.ItemContainerGenerator.ContainerFromItem(tb)
                        If _LastTipObj IsNot Nothing Then
                            Dim lastTip = CType(_LastTipObj.ToolTip, ToolTip)
                            If tb Is _LastTipObj Then ' over same obj: don't create a new tip
                                Return
                            Else
                                ' different object: close the tip
                                lastTip.IsOpen = False
                            End If
                        End If
                        tb.ToolTip = Nothing
                        ClearPriorToolTipIfAny()
                        Dim ttipObj = New ToolTip
                        ttipObj.PlacementTarget = tb
                        ttipObj.Placement = Controls.Primitives.PlacementMode.Bottom
                        Select Case tb.Name
                            Case "AllocationBase", "BaseAddress"
                                Dim tdesc = ComponentModel.TypeDescriptor.GetProperties(tb.DataContext)("_mbi")
                                Dim mbi = CType(tdesc.GetValue(tb.DataContext), MEMORY_BASIC_INFORMATION)

                                Dim baddr = If(tb.Name = "AllocationBase", mbi.AllocationBase, mbi.BaseAddress)
                                If CType(mbi.State, AllocationState) <> AllocationState.MEM_FREE Then
                                    Dim strAddrDump = GetMemoryDump(
                                        baddr,
                                        CInt(mbi.RegionSize))

                                    Dim tbxDump = New TextBox With {
                                        .Text = strAddrDump,
                                        .BorderThickness = New Windows.Thickness(0),
                                        .FontFamily = FontFamilyCourierNew,
                                        .FontSize = 8
                                    }
                                    If Not SystemParameters.HighContrast Then
                                        tbxDump.Background = Brushes.LightYellow
                                    End If
                                    Dim sp = New StackPanel With {.Orientation = Orientation.Vertical}
                                    sp.Children.Add(New Label With {.Content = tb.DataContext.ToString})
                                    Dim stk = GetCallStackForMBI(mbi)
                                    If Not String.IsNullOrEmpty(stk) Then
                                        sp.Children.Add(New TextBlock With {.Text = stk})
                                    End If
                                    If CType(mbi.lType, AllocationType) = AllocationType.MEM_IMAGE Then
                                        Dim sbFilename As New Text.StringBuilder(300)
                                        If _ConnectionMode = MemSpectMode.OnLine Then
                                            If GetModuleFileNameEx(_hProcessTarget, mbi.AllocationBase, sbFilename, sbFilename.Capacity) = 0 Then
                                                Dim err = Marshal.GetLastWin32Error
                                                sbFilename.Clear()
                                            End If
                                        End If
                                        sp.Children.Add(New TextBlock With {.Text = "Filename = " + sbFilename.ToString})
                                    End If
                                    sp.Children.Add(tbxDump)
                                    ttipObj.Content = sp
                                Else
                                    ttipObj.Content = baddr.ToInt32.ToString("x8") + " Free memory. Red if unusable due to fragmentation (RegionSize < 65536 and not coalescable with adjacent free blocks)"
                                End If
                            Case "Where", "Filename"
                                ttipObj.Content = tb.Text
                            Case Else
                                Return
                        End Select
                        ToolTipService.SetShowDuration(tb, 100000)

                        ttipObj.IsOpen = True
                        tb.ToolTip = ttipObj
                        _LastTipObj = tb
                    End If

                End If

            Catch ex As Exception

            End Try

        End Sub

        Friend Function DoShowFreeFragmentation(ByVal VMDataDict As SortedList(Of IntPtr, VMData)) As DataSurface
            Dim ctrlsFrag As DataSurface = Nothing
            Try
                ClearPriorToolTipIfAny()
                Dim priFree = From vmdat In VMDataDict.Values
                              Where vmdat.bgColorClass.colrType = VMColorType.Fragment

                Dim lst = New SortedList(Of IntPtr, VMData)(_CompareIntPtr)

                For Each freefrag In priFree
                    If Not lst.ContainsKey(freefrag.mbi.BaseAddress) Then
                        lst.Add(freefrag.mbi.BaseAddress, freefrag)
                    End If
                    Dim base = New IntPtr(freefrag.mbi.BaseAddress.ToInt64 - freefrag.mbi.BaseAddress.ToInt64 Mod 65536)
                    If VMDataDict.ContainsKey(base) Then
                        If Not lst.ContainsKey(base) Then ' can occur with small free within big blocks
                            lst.Add(base, VMDataDict(base))

                        End If
                    Else
                        Dim mbiprior = GetMBIForAddress(base - 1)
                        If Not lst.ContainsKey(mbiprior.BaseAddress) Then
                            lst.Add(mbiprior.BaseAddress, VMDataDict(mbiprior.BaseAddress))
                        End If
                    End If

                Next

                ctrlsFrag = DataWindowMain.MakeNewDatasurface("Frag", "Fragmentation detail", nMaxHeaderHeight:=20)
                ctrlsFrag.SurfaceHeader.Children.Add(
                    New TextBlock With {
                        .Text = "Each fragment (Free VM region < 64k) along with its preceding block which caused the leftover"
                    }
                )

                Dim br = MakeVMBrowse(lst, New VMDisplayOptions)

                ctrlsFrag.SurfaceDetails.Children.Add(br)
                AddHandler ctrlsFrag.SurfaceDetails.MouseLeave, Sub() ClearPriorToolTipIfAny()


            Catch ex As Exception
                CommonUI.MemSpectExceptionHandler(ex)
            End Try
            Return ctrlsFrag
        End Function

        Private Function MakeVMBrowse(ByVal VMDataDict As SortedList(Of IntPtr, VMData), ByVal DisplayOptions As VMDisplayOptions) As Browse
            Dim q As IEnumerable
            Dim ColumnsToTotal() As String = Nothing
            If DisplayOptions.fShowDetail Then

                q = From avmData In VMDataDict.Values
                        Let anMBI = avmData.mbi
                        Let empty = CountEmptyVMBytes(anMBI)
                        Select BaseAddress = anMBI.BaseAddress.ToInt32.ToString("x8"),
                                AllocationBase = anMBI.AllocationBase.ToInt32.ToString("x8"),
                                RegionSize = anMBI.RegionSize.ToString("x8"),
                                RegionEnd = anMBI.EndAddress.ToInt32.ToString("x8"),
                                Size10 = anMBI.RegionSize,
                                Num4K = CInt(anMBI.RegionSize / 4096),
                                Num64K = anMBI.RegionSize / 65536.0,
                                WSet = avmData.WorkingSet,
                                WSetPct = (CInt(100.0 * avmData.WorkingSet / anMBI.RegionSize)),
                                Data = avmData.strData,
                                Protect = CType(anMBI.Protect, AllocationProtect).ToString().Replace("PAGE_", ""),
                                State = CType(anMBI.State, AllocationState).ToString().Replace("MEM_", ""),
                                Type = CType(anMBI.lType, AllocationType).ToString,
                                TrailingZeros = empty.Item1,
                                TrailingZerosPct = CInt(empty.Item1 * 100.0 / anMBI.RegionSize),
                                BiggestZeroBlock = empty.Item2,
                                BiggestZeroPct = CInt(empty.Item2 * 100.0 / anMBI.RegionSize),
                                TotalZeros = empty.Item3,
                                TotZerosPct = CInt(empty.Item3 * 100.0 / anMBI.RegionSize),
                                _mbi = anMBI,
                                _bgcolor = avmData.bgColorClass
                            Order By BaseAddress

                ColumnsToTotal = {"RegionSize" + HexColumnSpecifier, "Size10", "Num4K", "WSet", "TrailingZeros", "BiggestZeroBlock", "TotalZeros"}
            Else
                q = From avmData In VMDataDict.Values
                        Let anMBI = avmData.mbi
                        Select BaseAddress = anMBI.BaseAddress.ToInt32.ToString("x8"),
                                AllocationBase = anMBI.AllocationBase.ToInt32.ToString("x8"),
                                RegionSize = anMBI.RegionSize.ToString("x8"),
                                RegionEnd = anMBI.EndAddress.ToInt32.ToString("x8"),
                                Size10 = anMBI.RegionSize,
                                Num4K = CInt(anMBI.RegionSize / 4096),
                                Num64K = anMBI.RegionSize / 65536.0,
                                WSet = avmData.WorkingSet,
                                WSetPct = (CInt(100.0 * avmData.WorkingSet / anMBI.RegionSize)),
                                Data = avmData.strData,
                                Protect = CType(anMBI.Protect, AllocationProtect).ToString().Replace("PAGE_", ""),
                                State = CType(anMBI.State, AllocationState).ToString().Replace("MEM_", ""),
                                Type = CType(anMBI.lType, AllocationType).ToString,
                                _mbi = anMBI,
                                _bgcolor = avmData.bgColorClass
                            Order By BaseAddress

                ColumnsToTotal = {"RegionSize" + HexColumnSpecifier, "Size10", "Num4K", "WSet"}

            End If
            Dim colTips = {"A pointer to the base address of the region of pages.",
               "A pointer to the base address of a range of pages allocated by the VirtualAlloc function. The page pointed to by the BaseAddress member is contained within this allocation range.",
               "The size of the region beginning at the base address in which all pages have identical attributes, in bytes.",
               "End of region",
               "The size of the region base 10",
               "Pages = RegionSize / 4K",
               "Granularity (Size / 65536)",
               "WorkingSet in bytes (from QueryWorkingSet)",
               "WorkingSet as % of region",
               "Information about the allocation, such as heap name or file name. GetLastError6= accessdenied (possibly LoadLibraryEx(...LOAD_LIBRARY_AS_IMAGE_RESOURCE",
               "The current memory protection option 'PAGE_*'. PAGE_NOACCESS means MEM_RELEASEd but may still be commited",
               "Allocation State, Free, Reserved, Committed  'MEM_*'  MEM_COMMIT=0x1000, MEM_RESERVE=0x2000, MEM_FREE=0x10000",
               "Allocation Type, such as Image, Mapped, or Private",
               "For Committed mem, # of trailing zero bytes",
               "Percent trailing zeros",
               "For Committed mem, # of zero bytes in biggest chunk",
               "Percent biggest chunk zeros",
               "Total # of 0",
               "Total # of 0 as Pct regionsize",
               "Obtained from GetModuleFileNameEx for MEM_IMAGE, or from GetMappedFileName or MappedFiles/Files from MemSpect heap"
               }

            Dim cwidths = New Integer() {70, 70, 70, 70, 90, 70, 70, 70, 70, 570, 120, 90, 90, 90, 80, 80, 80, 80, 80}


            Dim browVM = New Browse(
                    q,
                    ColWidths:=cwidths,
                    fAllowBrowFilter:=True,
                    InitialSortOrder:=New BrowseInitialSortOrder With {
                        .ColumnNo = 1,
                        .direction = ComponentModel.ListSortDirection.Ascending
                        },
                    colTips:=colTips,
                    arrColumnsToTotal:=ColumnsToTotal
                    )

            browVM.SetDynamicBackground(New VMBackgroundConverter, fAdditive:=False)

            AddHandler browVM._BrowseList.MouseDoubleClick, AddressOf On_VirtMemDblClick
            AddHandler browVM._BrowseList.MouseMove, AddressOf On_VirtmemMouseMove

            browVM._BrowseList.ContextMenu.AddMnuItem("Show stacks & _Memory dump in Notepad",
                                      "entire memory & callstack if available for all selected items",
                                      Sub()
                                          Dim itms = browVM._BrowseList.SelectedItems
                                          For Each itm In itms
                                              If itm IsNot Nothing Then
                                                  Dim mbi = CType(System.ComponentModel.TypeDescriptor.GetProperties(itm)("_mbi").GetValue(itm), MEMORY_BASIC_INFORMATION)
                                                  DumpMBIToNotepad(mbi, itm.ToString, nMaxDumpSize:=0)
                                              End If
                                          Next

                                      End Sub, InsertPos:=0)

            browVM._BrowseList.ContextMenu.AddMnuItem("_Dump All selected memory in Notepad",
                                      "show entire memory as a dump for all selected items, even if not contiguous. Can be very big",
                                      Sub()
                                          Dim itms = browVM._BrowseList.SelectedItems
                                          Dim lst = New SortedList(Of IntPtr, Tuple(Of MEMORY_BASIC_INFORMATION, Object))(_CompareIntPtr)
                                          ' first we sort by addr
                                          Dim totLen As ULong = 0
                                          For Each itm In itms
                                              If itm IsNot Nothing Then
                                                  Dim mbi = CType(System.ComponentModel.TypeDescriptor.GetProperties(itm)("_mbi").GetValue(itm), MEMORY_BASIC_INFORMATION)
                                                  lst(mbi.BaseAddress) = New Tuple(Of MEMORY_BASIC_INFORMATION, Object)(mbi, itm)
                                                  totLen += mbi.RegionSize
                                              End If
                                          Next
                                          Dim sb = New Text.StringBuilder
                                          sb.AppendFormat("Mem Dump Start Addr = {0:x8} End = {1:x8}   Size = {2:x8} ({2:n0})  #regions = {3}",
                                                          lst.First.Value.Item1.BaseAddress.ToInt32,
                                                          lst.Last.Value.Item1.EndAddress.ToInt32,
                                                          totLen,
                                                          lst.Count)
                                          Dim iternum = 0
                                          For Each itm In lst
                                              iternum += 1
                                              sb.AppendFormat(vbCrLf + "Region {0}  {1}" + vbCrLf,
                                                              iternum,
                                                              itm.Value.Item2.ToString)
                                              Dim strdump = GetMemoryDump(itm.Value.Item1.BaseAddress, CInt(itm.Value.Item1.RegionSize), nMaxDumpSize:=0)
                                              sb.Append(strdump)
                                          Next
                                          WriteOutputToTempFile(sb.ToString)

                                      End Sub, InsertPos:=0)
            'browVM._BrowseList.ContextMenu.AddMnuItem("_String Search",
            '              "Search selected items for a string, even if not contiguous. Can take a long time",
            '              Sub()
            '                  Dim itms = browVM._BrowseList.SelectedItems
            '                  Dim lst = New SortedList(Of IntPtr, Tuple(Of MEMORY_BASIC_INFORMATION, Object))(_CompareIntPtr)
            '                  ' first we sort by addr
            '                  Dim totLen As ULong = 0
            '                  For Each itm In itms
            '                      If itm IsNot Nothing Then
            '                          Dim mbi = CType(System.ComponentModel.TypeDescriptor.GetProperties(itm)("_mbi").GetValue(itm), MEMORY_BASIC_INFORMATION)
            '                          lst(mbi.BaseAddress) = New Tuple(Of MEMORY_BASIC_INFORMATION, Object)(mbi, itm)
            '                          totLen += mbi.RegionSize
            '                      End If
            '                  Next
            '                  Dim sb = New Text.StringBuilder
            '                  sb.AppendFormat("Mem Dump Start Addr = {0:x8} End = {1:x8}   Size = {2:x8} ({2:n0})  #regions = {3}",
            '                                  lst.First.Value.Item1.BaseAddress.ToInt32,
            '                                  lst.Last.Value.Item1.EndAddress.ToInt32,
            '                                  totLen,
            '                                  lst.Count)
            '                  Dim iternum = 0
            '                  For Each itm In lst
            '                      iternum += 1
            '                      sb.AppendFormat(vbCrLf + "Region {0}  {1}" + vbCrLf,
            '                                      iternum,
            '                                      itm.Value.Item2.ToString)
            '                      Dim strdump = GetMemoryDump(itm.Value.Item1.BaseAddress, CInt(itm.Value.Item1.RegionSize), nMaxDumpSize:=0)
            '                      sb.Append(strdump)
            '                  Next
            '                  WriteOutputToTempFile(sb.ToString)

            '              End Sub, InsertPos:=0)


            Return browVM
        End Function

        Friend Function ShowVMGraphLayout() As MemoryRegionGraphContainer
            Dim regions = New SortedList(Of ULong, MemRegion)()
            For Each itm In _VMDataDict.Values
                Dim colorType = VMColorClass.GetVMColorType(itm)


                Dim newreg = New MemRegion With {
                            .start = itm.Address,
                            .Length = itm.Size,
                            .brush = colorType.colorBrush,
                            .RegionType = MemRegionType.CustomColor
                            }
                newreg.lst.Add(itm)
                regions.Add(itm.Address, newreg)
            Next
            Dim x = MemoryRegionGraphContainer.CreateRegionGraphContainerFromMemoryBlocks(_VMDataDict.Values, minSpaceBetween:=1, srtedPinned:=Nothing)


            Dim ctrlsLayout = DataWindowMain.MakeNewDatasurface("VM Layout", "Graph of memory layout", nMaxHeaderHeight:=20)
            ctrlsLayout.SurfaceHeader.Children.Add(
                New TextBlock With {
                    .Text = "VM Memory Layout"
                }
            )
            ctrlsLayout.SurfaceDetails.Children.Add(x)
            AddHandler x.LostFocus, Sub() x._RegionGraph.ClosePriorTipIfAny()
            AddHandler x.GotFocus, Sub() x._RegionGraph.ClosePriorTipIfAny()
            AddHandler x.IsVisibleChanged, Sub() x._RegionGraph.ClosePriorTipIfAny()
            AddHandler ctrlsLayout.SurfaceDetails.MouseLeave, Sub() x._RegionGraph.ClosePriorTipIfAny()

            Return x
        End Function


    End Class

    Public Class MemoryEaterUI

        Friend _ctrls As DataSurface
        Friend _browMemEater As Browse

        Sub New()
            _ctrls = DataWindowMain.MakeNewDatasurface("MemEater", "eat lots of memory", nMaxHeaderHeight:=30)
            Dim btnEat As New Button With {.Content = "_Eat", .ToolTip = "VirtualAlloc repeatedly"}
            Dim btnFree As New Button With {.Content = "_Free All", .ToolTip = "Free all memory eater allocations (also occurs on Memspect shutdown"}
            Dim chkTopDown As New CheckBox With {.IsChecked = False, .Content = "_TopDown   ", .ToolTip = "Allocate MEM_TOP_DOWN or bottom up"}
            Dim chkCommit As New CheckBox With {.IsChecked = False, .Content = "_Commit   ", .ToolTip = "Allocate COMMIT or just RESERVE. Commit is more expensive"}
            Dim chkLimit2g As New CheckBox With {.IsChecked = True, .Content = "_Limit to 2G   ", .ToolTip = "Only eat up to 0x80000000. If TOP_DOWN, eat above, else below"}
            Dim chkEatAll As New CheckBox With {.IsChecked = True, .Content = "Eat_All   ", .ToolTip = "Continually eat specified size til full"}
            Dim lblSize As New Label With {.Content = "Bite Size in bytes (granularity)"}
            Dim txtSize As New TxtBoxNumeric With {.Text = "1048576", .ToolTip = "# of bytes to eat (64K = 65536,1M =1,048,576)", .Width = 180, .HorizontalAlignment = HorizontalAlignment.Left}
            Dim tbStatus As New TextBlock
            AddHandler btnEat.Click, Sub()
                                         DataWindowMain._DataWindowMain.Cursor = Cursors.Wait
                                         Common.MemoryEater.EatSomeMemory(CInt(txtSize.Text),
                                                       chkLimit2g.IsChecked.GetValueOrDefault,
                                                       chkTopDown.IsChecked.GetValueOrDefault,
                                                       chkEatAll.IsChecked.GetValueOrDefault,
                                                       chkCommit.IsChecked.GetValueOrDefault)
                                         RefreshList(_ctrls, tbStatus)
                                         DataWindowMain._DataWindowMain.Cursor = Cursors.Arrow

                                     End Sub
            AddHandler btnFree.Click, Sub()
                                          VirtualMem.ClearPriorToolTipIfAny()
                                          Common.MemoryEater.FreeAll()
                                          RefreshList(_ctrls, tbStatus)
                                      End Sub
            _ctrls.SurfaceHeader.Children.Add(btnEat)
            _ctrls.SurfaceHeader.Children.Add(btnFree)
            _ctrls.SurfaceHeader.Children.Add(chkTopDown)
            _ctrls.SurfaceHeader.Children.Add(chkLimit2g)
            _ctrls.SurfaceHeader.Children.Add(chkCommit)
            _ctrls.SurfaceHeader.Children.Add(chkEatAll)
            _ctrls.SurfaceHeader.Children.Add(lblSize)
            _ctrls.SurfaceHeader.Children.Add(txtSize)
            _ctrls.SurfaceHeader.Children.Add(tbStatus)

            RefreshList(_ctrls, tbStatus)
            'bcdedit /set IncreaseUserVA 3072
            'bcdedit /deletevalue IncreaseUserVA 
            'Dim tstSize = 1024 * 1024 * 100
            'Dim resstr = SendMsg(ProcMsgVerb.DoVirtualAlloc, {0, tstSize, CType(AllocationType.MEM_TOP_DOWN Or AllocationState.MEM_RESERVE, AllocationType), AllocationProtect.PAGE_READWRITE})
            'Dim res = BitConverter.ToInt32(resstr, 1)
            'UpdateStatusMsg("VAlloced " + tstSize.ToString("n0") + " res = " + res.ToString("x8"))
            'Dim rr = VirtualAllocEx(_hProc, IntPtr.Zero, 1024 * 1024 * 500, CType(AllocationType.MEM_TOP_DOWN Or AllocationState.MEM_RESERVE, AllocationType), AllocationProtect.PAGE_READWRITE)
            'UpdateStatusMsg("VAllocEx " + rr.ToString("x8"))
            'VirtualFree(_hProc, rr, 100, AllocationState.MEM_FREE)

        End Sub



        Private Sub RefreshList(ByVal ctrls As DataSurface, ByVal tbstatus As TextBlock)
            ctrls.SurfaceDetails.Children.Clear()

            Dim q = From a In Common.MemoryEater._EatenAllocations.Values
                    Select BaseAddress = a.BaseAddress.ToInt32.ToString("x8"),
                            AllocationBase = a.AllocationBase.ToInt32.ToString("x8"),
                            RegionSize = a.RegionSize.ToString("x8"),
                            Size10 = a.RegionSize.ToString("n0"),
                            AllocationProtect = CType(a.AllocationProtect, AllocationProtect).ToString(),
                            State = CType(a.State, AllocationState).ToString(),
                            Type = CType(a.lType, AllocationType).ToString,
                            _mbi = a
                            Order By BaseAddress

            _browMemEater = New Browse(
                        q,
                        ColWidths:=New Integer() {WIDTH_ADDRESS, WIDTH_ADDRESS, WIDTH_ADDRESS, 80, 80, 150, 80, 80},
                        fAllowBrowFilter:=True,
                        InitialSortOrder:=New BrowseInitialSortOrder With {.ColumnNo = 1, .direction = ComponentModel.ListSortDirection.Ascending},
                        arrColumnsToTotal:={"RegionSize" + HexColumnSpecifier, "Size10"}
                    )

            AddHandler _browMemEater._BrowseList.MouseDoubleClick, AddressOf VirtualMem.On_VirtMemDblClick
            AddHandler _browMemEater._BrowseList.MouseMove, AddressOf VirtualMem.On_VirtmemMouseMove
            AddHandler _browMemEater._BrowseList.MouseLeave, Sub() VirtualMem.ClearPriorToolTipIfAny()

            ctrls.SurfaceDetails.Children.Add(_browMemEater)

            tbstatus.Text = String.Format("# Eaten = " +
                                          Common.MemoryEater._EatenAllocations.Count.ToString + "  Tot Megs =  " +
                                          (Common.MemoryEater._EatenAllocations.Values.Sum(Function(m As MEMORY_BASIC_INFORMATION) As UInteger
                                                                                               Return CUInt(m.RegionSize / 1024 / 1024)
                                                                                           End Function).ToString("n0")))
        End Sub

    End Class

End Namespace
