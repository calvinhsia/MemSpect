Imports System.Runtime.InteropServices

Namespace MemSpect

    Public Class MemSpectWin
        Inherits DockPanel
        Friend _snap As MemSnapshot
        Friend WithEvents _btnAll As RadioButton
        Friend WithEvents _btnFiles As RadioButton
        Friend WithEvents _btnVirtualAllocs As RadioButton
        Friend WithEvents _btnHeapCreates As RadioButton
        Friend WithEvents _btnGdiObjs As RadioButton
        Friend WithEvents _btnGCStackInfo As RadioButton
        Friend WithEvents _btnClrLoads As RadioButton
        Friend WithEvents _btnClrJit As RadioButton
        Friend WithEvents _btnClrGCHnd As RadioButton
        Friend WithEvents _btnClrExcpt As RadioButton
        Friend WithEvents _btnClrClasses As RadioButton
        Friend WithEvents _btnClrObjects As RadioButton
        Friend WithEvents _btnCodeMarker As RadioButton
        Friend WithEvents _btnThreadInfo As RadioButton
        Friend WithEvents _btnLoadResourceInfo As RadioButton
        Friend _btns As RadioButton()
        Friend Shared _btnLastSelected As TrkType = TrkType.HeapCreates ' choose a small one so faster by default
        Friend _bmem As BrowseMem
        Private _ctrlTxtTotal As TextBlock
        Private _CustomColumnFactories As New Dictionary(Of String, BrowseMem.CustomColumnFactory)() 'Used to save the custom column factories that should be displayed for this window.

        Sub New(ByVal snap As MemSnapshot, ByVal ctrlTxtTotal As TextBlock)
            _snap = snap
            _ctrlTxtTotal = ctrlTxtTotal
            MakeTrkUi()
            OnBtnSel(_btnLastSelected, Nothing)
        End Sub

        'Adds custom columns to the MemSpect win's BrowseMem control
        Public Sub AddColumnForProperty(ByVal propertyName As String, ByVal columnWidth As Integer, ByVal columnHeaderText As String, ByVal columnHeaderTip As String)
            Dim customColumnFactory As New BrowseMem.CustomColumnFactory(propertyName, columnWidth, columnHeaderText, columnHeaderTip)

            If Not _CustomColumnFactories.ContainsKey(customColumnFactory.PropertyName) Then
                _CustomColumnFactories.Add(customColumnFactory.PropertyName, customColumnFactory)
            End If

            If Not _bmem Is Nothing Then
                _bmem.AddCustomColumnFactory(customColumnFactory)
            End If
        End Sub

        Private Sub MakeTrkUi()

            Dim totcnt = 0
            Dim totsize As Long = 0
            ' if there's only 1 type of item, let's make that type the default to show
            Dim numDiffItems = 0
            Dim blkTypeNo = -1
            For i = 0 To _snap.aBlockTypeCnts.Length - 1
                If _snap.aBlockTypeCnts(i) > 0 Then
                    numDiffItems += 1
                    blkTypeNo = i
                End If
                totcnt += _snap.aBlockTypeCnts(i)
                totsize += _snap.aBlockTypeSizes(i)
            Next
            If numDiffItems = 1 Then ' exactly 1
                _btnLastSelected = BlockTypeToTrackType(CType(blkTypeNo, BlockTypes))
            End If


            Dim lambtnContent = Function(btnLabel As String) As UIElement
                                    Dim sp = CType(Windows.Markup.XamlReader.Load(
                                                        <StackPanel
                                                            xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
                                                            xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
                                                            >
                                                            <TextBlock><Bold><%= btnLabel %></Bold></TextBlock>
                                                            <TextBlock FontSize="8">Cnt= <%= totcnt.ToString("n0") %></TextBlock>
                                                            <TextBlock FontSize="8">Size=<%= totsize.ToString("n0") %></TextBlock>
                                                        </StackPanel>.CreateReader
                                                        ), StackPanel)

                                    Dim rr = New Paragraph With {.FontSize = 8}
                                    sp.Orientation = Orientation.Vertical
                                    Return sp
                                End Function
            _btnAll = New RadioButton With {
                .Content = lambtnContent("All"),
                .Tag = TrkType.All,
                .ToolTip = "All types",
                .IsChecked = _btnLastSelected = TrkType.All
                }

            totcnt = _snap.aBlockTypeCnts(BlockTypes.IndirectInfo) + _snap.aBlockTypeCnts(BlockTypes.MapFile)
            totsize = _snap.aBlockTypeSizes(BlockTypes.IndirectInfo) + _snap.aBlockTypeSizes(BlockTypes.MapFile)
            _btnFiles = New RadioButton With {
                .Content = lambtnContent("Files"),
                .Tag = TrkType.Indirect,
                .ToolTip = "Files, mapped files, sections. Detoured CreateFile, OpenFile,CreateSection, OpenSection,MapViewOfSection " + vbCrLf +
                        "memory mapped files: detoured MapViewOfFile and MapViewOfFileEx calls",
                .IsChecked = _btnLastSelected = TrkType.Indirect
            }

            totcnt = _snap.aBlockTypeCnts(BlockTypes.VirtualAlloc)
            totsize = _snap.aBlockTypeSizes(BlockTypes.VirtualAlloc)
            _btnVirtualAllocs = New RadioButton With {
                .Content = lambtnContent("VirtAlloc"),
                .Tag = TrkType.VirtualAllocs,
                .ToolTip = "calls to VirtualAlloc: note make sure you understand VirtualAlloc: A program can call VA multiple times to Commit/free/reset memory",
                .IsChecked = _btnLastSelected = TrkType.VirtualAllocs
                }

            totcnt = _snap.aBlockTypeCnts(BlockTypes.HeapCreate)
            totsize = _snap.aBlockTypeSizes(BlockTypes.HeapCreate)
            _btnHeapCreates = New RadioButton With {
                .Content = lambtnContent("HeapCreates"),
                .Tag = TrkType.HeapCreates,
                .ToolTip = "Calls to HeapCreate",
                .IsChecked = _btnLastSelected = TrkType.HeapCreates
                }

            totcnt = _snap.aBlockTypeCnts(BlockTypes.GdiObjs)
            totsize = 0
            _btnGdiObjs = New RadioButton With {
                .Content = lambtnContent("GDI Objects"),
                .Tag = TrkType.GdiObjs,
                .ToolTip = "GDI handles (objects). Enable with fTrackGDI in MemSpect.INI file",
                .IsChecked = _btnLastSelected = TrkType.GdiObjs
                }

            totcnt = _snap.aBlockTypeCnts(BlockTypes.TrackGCStacks)
            totsize = 0


            _btnGCStackInfo = New RadioButton With {
                .Content = lambtnContent("GCStacks"),
                .Tag = TrkType.TrackGCStacks,
                .ToolTip = "Stacks for GCs: enabled by TrackGCStacks in INI file." + _iniFileName,
                .IsChecked = _btnLastSelected = TrkType.TrackGCStacks
                }

            totcnt = _snap.aBlockTypeCnts(BlockTypes.ClrAssembly) + _snap.aBlockTypeCnts(BlockTypes.ClrAppDomain)
            totsize = _snap.aBlockTypeSizes(BlockTypes.ClrAssembly) + _snap.aBlockTypeSizes(BlockTypes.ClrAppDomain)
            _btnClrLoads = New RadioButton With {
                .Content = lambtnContent("ClrLoads"),
                .Tag = TrkType.ClrLoads,
                .ToolTip = "AppDomain, Assembly, Module Loads",
                .IsChecked = _btnLastSelected = TrkType.ClrLoads
                }

            totcnt = _snap.aBlockTypeCnts(BlockTypes.ClrJit)
            totsize = _snap.aBlockTypeSizes(BlockTypes.ClrJit)
            _btnClrJit = New RadioButton With {
                .Content = lambtnContent("ClrJit"),
                .Tag = TrkType.ClrJit,
                .ToolTip = "Just in time Compile. Controlled by TrackJIT in " + _iniFileName,
                .IsChecked = _btnLastSelected = TrkType.ClrJit
                }

            totcnt = _snap.aBlockTypeCnts(BlockTypes.ClrGCHnd)
            totsize = _snap.aBlockTypeSizes(BlockTypes.ClrGCHnd)
            _btnClrGCHnd = New RadioButton With {
                .Content = lambtnContent("ClrGCHnd"),
                .Tag = TrkType.ClrGCHnd,
                .ToolTip = "GC Handles",
                .IsChecked = _btnLastSelected = TrkType.ClrGCHnd
                }

            totcnt = _snap.aBlockTypeCnts(BlockTypes.ClrExcpt)
            totsize = _snap.aBlockTypeSizes(BlockTypes.ClrExcpt)
            _btnClrExcpt = New RadioButton With {
                .Content = lambtnContent("ClrExcpt"),
                .Tag = TrkType.ClrExcpt,
                .ToolTip = "CLR Exceptions. Controlled by TrackExcpt in " + _iniFileName,
                .IsChecked = _btnLastSelected = TrkType.ClrExcpt
                }


            totcnt = _snap.aBlockTypeCnts(BlockTypes.ClrClass)
            totsize = _snap.aBlockTypeSizes(BlockTypes.ClrClass)
            _btnClrClasses = New RadioButton With {
                .Content = lambtnContent("ClrClasses"),
                .Tag = TrkType.ClrClasses,
                .ToolTip = "Class Loads, Instance & Collection counts",
                .IsChecked = _btnLastSelected = TrkType.ClrClasses
                }

            totcnt = _snap.aBlockTypeCnts(BlockTypes.ClrObject)
            totsize = _snap.aBlockTypeSizes(BlockTypes.ClrObject)
            _btnClrObjects = New RadioButton With {
                .Content = lambtnContent("ClrObjs"),
                .Tag = TrkType.ClrObjects,
                .ToolTip = "AppDomain, Assembly, Module and Class Loads",
                .IsChecked = _btnLastSelected = TrkType.ClrObjects
                }

            totcnt = _snap.aBlockTypeCnts(BlockTypes.CodeMarker)
            totsize = 0
            _btnCodeMarker = New RadioButton With {
                .Content = lambtnContent("CodeMarkers"),
                .Tag = TrkType.CodeMarker,
                .ToolTip = "CodeMarkers." + vbCrLf + "The address column is really just another series of seq no",
                .IsChecked = _btnLastSelected = TrkType.CodeMarker
                }

            totcnt = _snap.aBlockTypeCnts(BlockTypes.TlsAllocFree) + _snap.aBlockTypeCnts(BlockTypes.ThreadCreate)
            totsize = 4 * totcnt ' arbitrary

            _btnThreadInfo = New RadioButton With {
                .Content = lambtnContent("ThrdInfo"),
                .Tag = TrkType.ThreadInfo,
                .ToolTip = "ThreadCreates and ThreadLocalStorage Allocations and Frees: Calls to TlsAlloc and TlsFree." + vbCrLf + " The Address column is the TlsIndex" + vbCrLf +
                    "Controlled by fTrackThreadCreate in INI file " + _iniFileName,
                .IsChecked = _btnLastSelected = TrkType.ThreadInfo
                }

            totcnt = _snap.aBlockTypeCnts(BlockTypes.LoadResource)
            totsize = _snap.aBlockTypeSizes(BlockTypes.LoadResource)
            _btnLoadResourceInfo = New RadioButton With {
                .Content = lambtnContent("Rsrc"),
                .Tag = TrkType.LoadResource,
                .ToolTip = "LoadResource calls (native resource access). Controlled by fTrackLoadResource in INI file " + _iniFileName,
                .IsChecked = _btnLastSelected = TrkType.LoadResource
                }
            _btns = {_btnAll,
                     _btnFiles,
                     _btnVirtualAllocs,
                     _btnHeapCreates,
                     _btnGdiObjs,
                     _btnGCStackInfo,
                     _btnClrLoads,
                     _btnClrJit,
                     _btnClrGCHnd,
                     _btnClrExcpt,
                     _btnClrClasses,
                     _btnClrObjects,
                     _btnCodeMarker,
                     _btnThreadInfo,
                     _btnLoadResourceInfo
            }

            Me.Children.Clear()
            Dim dpRadioBtns As New DockPanel
            dpRadioBtns.Children.Clear()
            dpRadioBtns.Children.Add(_btnAll)
            dpRadioBtns.Children.Add(_btnFiles)
            dpRadioBtns.Children.Add(_btnVirtualAllocs)
            dpRadioBtns.Children.Add(_btnHeapCreates)
            dpRadioBtns.Children.Add(_btnGdiObjs)
            dpRadioBtns.Children.Add(_btnGCStackInfo)
            dpRadioBtns.Children.Add(_btnClrLoads)
            dpRadioBtns.Children.Add(_btnClrJit)
            dpRadioBtns.Children.Add(_btnClrGCHnd)
            dpRadioBtns.Children.Add(_btnClrExcpt)
            dpRadioBtns.Children.Add(_btnClrClasses)
            dpRadioBtns.Children.Add(_btnClrObjects)
            dpRadioBtns.Children.Add(_btnCodeMarker)
            dpRadioBtns.Children.Add(_btnThreadInfo)
            dpRadioBtns.Children.Add(_btnLoadResourceInfo)

            Dim bord As New Border With {
                .Height = 45,
                .BorderThickness = New Windows.Thickness(2),
                .HorizontalAlignment = Windows.HorizontalAlignment.Center
            }
            bord.Child = dpRadioBtns
            Me.Children.Add(bord)
            DockPanel.SetDock(bord, Dock.Top)
        End Sub

        Sub OnBtnSel(ByVal sender As Object, ByVal e As EventArgs) Handles _btnAll.Checked,
                    _btnFiles.Checked,
                    _btnVirtualAllocs.Checked,
                    _btnHeapCreates.Checked,
                    _btnGdiObjs.Checked,
                    _btnGCStackInfo.Checked,
                    _btnClrLoads.Checked,
                    _btnClrJit.Checked,
                    _btnClrExcpt.Checked,
                    _btnClrGCHnd.Checked,
                    _btnClrClasses.Checked,
                    _btnClrObjects.Checked,
                    _btnCodeMarker.Checked,
                    _btnThreadInfo.Checked,
                    _btnLoadResourceInfo.Checked
            Try

                Dim ttype As TrkType
                If sender.GetType.Name = "RadioButton" Then
                    Dim rb = CType(sender, RadioButton)
                    ttype = CType(rb.Tag, TrkType)
                Else
                    ttype = CType(sender, TrkType)
                End If
                _btnLastSelected = ttype
                Dim ColWidths = {100}

                ' the allocs have already been filtered, if filtering is on
                ' if we're merging codemarkers
                Dim fIncludeCMarkerLam = Function(a As HeapAllocationContainer) As Boolean
                                             If _bmem IsNot Nothing Then
                                                 If Me._bmem._MergeCodemarkers AndAlso a.TBlkBlockType = BlockTypes.CodeMarker Then
                                                     Return True ' we're merging, so additional allocs
                                                 End If
                                             End If
                                             Return False
                                         End Function

                Dim qfunc = Function(theheapAllocs As List(Of HeapAllocationContainer), thebmem As BrowseMem) As IEnumerable
                                Dim qDetails As IEnumerable = Nothing

                                Select Case ttype
                                    Case TrkType.All
                                        qDetails = From hCtr In theheapAllocs
                                        Where hCtr.IsTrkBlkType(ttype)
                                        Select
                                            Address = hCtr.TBlk.Address.ToString("x8"),
                                            hCtr.AllocationStruct.SeqNo,
                                            hCtr.TBlk.Size,
                                            hCtr.AllocationStruct.Thread,
                                            BlockType = hCtr.GetBlockTypeName,
                                            _HeapAllocationContainer = hCtr
                                        Order By SeqNo

                                        thebmem._ColWidths = {WIDTH_ADDRESS}
                                        thebmem._arrColumnsToTotal = {"Size"}

                                    Case TrkType.Indirect, TrkType.MappedFiles
                                        qDetails = From hCtr In theheapAllocs
                                        Where hCtr.IsTrkBlkType(ttype) OrElse
                                             fIncludeCMarkerLam(hCtr)
                                        Select
                                            Address = hCtr.GetAddr.ToInt32.ToString("x8"),
                                            hCtr.AllocationStruct.SeqNo,
                                            Size = hCtr.GetSize,
                                            hCtr.AllocationStruct.Thread,
                                            BlockType = hCtr.GetBlockTypeName,
                                            InfoType = If(hCtr.TBlkBlockType = BlockTypes.IndirectInfo, hCtr.GetIndirectInfoType.ToString.Substring(7), hCtr.TBlkBlockType.ToString),
                                            BaseAddr = hCtr.GetExtraDisplayData,
                                            FileName = hCtr.GetMappedOrIndirectFileName,
                                            RequestSize = hCtr.GetMapRequestNumberOfBytes,
                                            DesiredAccess = hCtr.GetMapRequestDesiredAccess.ToString("x8"),
                                            _HeapAllocationContainer = hCtr
                                                   Order By SeqNo

                                        thebmem.ColumnTips = {TIP_ADDRESS, TIP_SEQNO, "Size taken in address space for Mapped Sections",
                                                                  TIP_THREAD,
                                                                  "MemSpect BlockType",
                                                                  "Type of call",
                                                                  "for FileLoad/Unload, the base address",
                                                                  "Filename obtained from looking up handle from OpenFile/CreateFile/OpenSection call"
                                                                 }
                                        thebmem._ColWidths = {WIDTH_ADDRESS, WIDTH_SEQNO, 70, 65, 75, 75, WIDTH_ADDRESS, 700}
                                        thebmem._arrColumnsToTotal = {"Size"}

                                    Case TrkType.VirtualAllocs
                                        Dim virtualallocs = GetVirtAllocs()   ' works with offline too

                                        qDetails = From hCtr In theheapAllocs
                                            Where hCtr.IsTrkBlkType(ttype) OrElse
                                                 fIncludeCMarkerLam(hCtr)
                                            Let targAddr = hCtr.GetAddr
                                            Let mbi = GetMBIForAddress(targAddr, virtualallocs)
                                            Let SizeToUse = CLng(Math.Min(hCtr.GetSize, mbi.RegionSize))
                                            Let empty = VirtualMem.CountEmptyVMBytes(mbi, hCtr.GetAddr, CUInt(SizeToUse))
                                            Select
                                                Address = hCtr.GetAddr.ToString("x8"),
                                                hCtr.AllocationStruct.SeqNo,
                                                hCtr.TBlk.Size,
                                                hCtr.AllocationStruct.Thread,
                                                ReqAddress = hCtr.TBlk.UnionData1.ToString("x8"),
                                                AllocationBase = mbi.AllocationBase.ToInt32.ToString("x8"),
                                                RegionSize = mbi.RegionSize.ToString("x8"),
                                                TrailingZeros = empty.Item1,
                                                TrailingZerosPct = CInt(empty.Item1 * 100.0 / SizeToUse),
                                                BiggestZeroBlock = empty.Item2,
                                                BiggestZeroPct = CInt(empty.Item2 * 100.0 / SizeToUse),
                                                TotalZeros = empty.Item3,
                                                TotZerosPct = CInt(empty.Item3 * 100.0 / SizeToUse),
                                                FileName = GetFileNameFromMBI(mbi),
                                                ReqMemType = CType(hCtr.TBlk.UnionData2, AllocationState).ToString.Replace("MEM_", String.Empty),
                                                CurMemType = mbi.State.ToString.Replace("MEM_", String.Empty),
                                                Data = hCtr.GetBlockTypeName,
                                                _HeapAllocationContainer = hCtr
                                        Order By SeqNo

                                        thebmem.ColumnTips = {"The Address. Since VirtualAlloc can be called 2 different times (with the same resulting Address): 1st to reserve, then to commit" +
                                           vbCrLf + "the callstack is of the commit. The MemType indicates both",
                                                              TIP_SEQNO, TIP_SIZE, TIP_THREAD,
                                                              "User requested base address for VirtualAlloc call",
                                                              "A pointer to the base address of a range of pages allocated by the VirtualAlloc function. The page pointed to by the BaseAddress member is contained within this allocation range.",
                                                              "The size of the region beginning at the base address in which all pages have identical attributes, in bytes.",
                                                               "For Committed mem, # of trailing zero bytes",
                                                               "Percent trailing zeros",
                                                               "For Committed mem, # of zero bytes in biggest chunk",
                                                               "Percent biggest chunk zeros",
                                                               "Total # of 0",
                                                               "Total # of 0 as Pct regionsize",
                                                               "Obtained from GetModuleFileNameEx for MEM_IMAGE, or from GetMappedFileName or MappedFiles/Files from MemSpect heap",
                                                             "type at time of request: Reserved, Committed or both",
                                                             "Current type: Reserved, Committed, Free. If COMMIT, shows row in yellow, FREE shows as white.",
                                                             "Try merging code markers"
                                                             }
                                        thebmem._ColWidths = {WIDTH_ADDRESS, WIDTH_SEQNO, 70, 65, 75, 70, 70, 70, 70, 70, 70, 70, 70, 70, 110, 110, 80, 400}
                                        thebmem._arrColumnsToTotal = {"Size", "RegionSize" + HexColumnSpecifier, "TrailingZeros", "BiggestZeroBlock", "TotalZeros"}

                                        thebmem.DynamicBackgroundConverter = New VirtAllocColorizer
                                    Case TrkType.MappedFiles
                                        Debug.Assert(False, "Mapped files now go to indirectinfo")
                                    Case TrkType.HeapCreates
                                        qDetails = From hCtr In theheapAllocs
                                        Where hCtr.IsTrkBlkType(ttype) OrElse
                                             fIncludeCMarkerLam(hCtr)
                                        Select
                                            Address = hCtr.TBlk.Address.ToString("x8"),
                                            hCtr.AllocationStruct.SeqNo,
                                            hCtr.TBlk.Size,
                                            hCtr.AllocationStruct.Thread,
                                            HeapName = hCtr.GetHeapNameForHeapCreate,
                                            HeapHandle = hCtr.TBlk.UnionData1.ToString("x8"),
                                            BlockType = hCtr.GetBlockTypeName,
                                        _HeapAllocationContainer = hCtr
                                        Order By SeqNo

                                        thebmem.ColumnTips = {TIP_ADDRESS, TIP_SEQNO, TIP_SIZE, TIP_THREAD,
                                                              "HeapName is obtained from resolving symbol of caller to HeapCreate. There can be multiple with the same name",
                                                              "Handle is the unique handle per heap"}

                                        thebmem._ColWidths = {WIDTH_ADDRESS, WIDTH_SEQNO, 65, 40, 500}

                                        thebmem._arrColumnsToTotal = Nothing
                                    Case TrkType.GdiObjs
                                        qDetails = From hctr In theheapAllocs
                                                   Where hctr.IsTrkBlkType(ttype)
                                                   Select
                                                   Address = hctr.TBlk.UnionData1.ToString("x8"),
                                                   hctr.AllocationStruct.SeqNo,
                                                   hctr.AllocationStruct.Thread,
                                                   GdiObjType = CType(hctr.TBlk.UnionData2, GdiObjType).ToString(),
                                                    _HeapAllocationContainer = hctr
                                                   Order By SeqNo
                                        thebmem.ColumnTips = {"Address is the GDI Handle. Hover over Address for stack trace tooltip", TIP_SEQNO, TIP_THREAD}
                                        thebmem._ColWidths = {WIDTH_ADDRESS, WIDTH_SEQNO, 65, 100}


                                    Case TrkType.TrackGCStacks
                                        qDetails = From hCtr In theheapAllocs
                                        Where hCtr.IsTrkBlkType(ttype)
                                        Select
                                            Address = hCtr.TBlk.Address.ToString("x8"),
                                            hCtr.AllocationStruct.SeqNo,
                                            hCtr.AllocationStruct.Thread,
                                            Reason = hCtr.GetGCReason,
                                            GCGens = hCtr.GetGCGens,
                                            _HeapAllocationContainer = hCtr
                                        Order By SeqNo

                                        thebmem._ColWidths = {WIDTH_ADDRESS, WIDTH_SEQNO, 60, 70}
                                        thebmem.ColumnTips = {"Garbage Collection ID",
                                                              TIP_SEQNO,
                                                              TIP_THREAD,
                                                              "Reason GC was done. Induced means intentional GC.Collect call.",
                                                              "Gen of collection"}
                                        thebmem._arrColumnsToTotal = Nothing

                                    Case TrkType.ClrLoads
                                        qDetails = From hCtr In theheapAllocs
                                        Where hCtr.IsTrkBlkType(ttype) OrElse
                                            fIncludeCMarkerLam(hCtr)
                                        Select
                                            Address = hCtr.TBlk.Address.ToString("x8"),
                                            hCtr.AllocationStruct.SeqNo,
                                            Size = Function() As Integer
                                                       'SendMsg(ProcMsgVerb.GetClrSize, {hCtr.TblkBlockType, hCtr.TBlk.UnionData2, hCtr.TBlk.UnionData1})
                                                       'Return Marshal.ReadInt32(_SharedMemAddr)
                                                       Return 0 ' getting size is expensive: requires Managed thread in target exe
                                                   End Function.Invoke(),
                                            hCtr.AllocationStruct.Thread,
                                            BlockType = hCtr.GetBlockTypeName,
                                            Data = hCtr.GetDisplayData,
                                            _HeapAllocationContainer = hCtr
                                        Order By SeqNo

                                        thebmem.ColumnTips = {TIP_ADDRESS, TIP_SEQNO, TIP_SIZE, TIP_THREAD,
                                                              "AssemblyLoad, ModuleLoad, AppDomainCreate",
                                                              "Name of Assembly/Module/AppDomain"
                                                             }

                                        thebmem._ColWidths = {WIDTH_ADDRESS, WIDTH_SEQNO, 60, 60, 60, 800}
                                    Case TrkType.ClrJit
                                        qDetails = From hCtr In theheapAllocs
                                        Where hCtr.IsTrkBlkType(ttype) OrElse
                                            fIncludeCMarkerLam(hCtr)
                                        Select
                                            Address = CType(hCtr.TBlk.UnionData1 And Not MANAGED_STACK_FLAG, IntPtr).ToString("x8"),
                                            hCtr.AllocationStruct.SeqNo,
                                            hCtr.AllocationStruct.Thread,
                                            JitSeqNo = hCtr.TBlk.Address.ToInt32.ToString("x8"),
                                            IsSafeToBlock = hCtr.TBlk.UnionData2,
                                            JitInfo = hCtr.GetJitInfo,
                                            Method = hCtr.GetJitMethodName,
                                            BlockType = hCtr.GetBlockTypeName,
                                            _HeapAllocationContainer = hCtr
                                        Order By SeqNo

                                        thebmem.ColumnTips = {TIP_ADDRESS, TIP_SEQNO, TIP_THREAD,
                                                              "Jit Sequence # 1,2,3...",
                                                              "IsSafeToBlock",
                                                              "JIT compiled function name",
                                                              "JIT compiled method name"
                                                             }

                                        thebmem._ColWidths = {WIDTH_ADDRESS, WIDTH_SEQNO, 65, WIDTH_ADDRESS, 60, 900, 200}

                                        Dim tabJitDist = thebmem.AddTabItemForBrowMem("Assembly Distribution",
                                                                      "Distribution of JITted funtions per assembly",
                                                                      Sub(sndr As Object, ejit As RoutedEventArgs)
                                                                          Dim thetabItem = CType(sndr, TabItem)
                                                                          Dim bmem = CType(thetabItem.Tag, BrowseMem)
                                                                          Dim qJitDist = From hctr In theheapAllocs
                                                                                         Where hctr.IsTrkBlkType(ttype)
                                                                                         Select
                                                                                            FileName = hctr.GetJitInfoAsmName,
                                                                                            hctr
                                                                                            Group By FileName
                                                                                            Into Count()
                                                                                            Select FileName, Count
                                                                                            Order By Count Descending


                                                                          Dim br = New Browse(qJitDist,
                                                                                               InitialSortOrder:=New BrowseInitialSortOrder With {.ColumnNo = 2, .direction = ComponentModel.ListSortDirection.Descending},
                                                                                               ColWidths:={800, 60},
                                                                                               ColTips:={"Assembly", "Jit Count"},
                                                                                               arrColumnsToTotal:={"Count"}
                                                                                               )

                                                                          br._BrowseList.ContextMenu.AddMnuItem(
                                                                              "_SubSnapshot",
                                                                              "Create a new snapshot from the selected items",
                                                                              Sub()
                                                                                  Dim items = br._BrowseList.SelectedItems
                                                                                  Dim desc = "JIT Assembly distribution"
                                                                                  If items Is Nothing OrElse items.Count < 1 Then
                                                                                      items = br._BrowseList.Items
                                                                                  End If
                                                                                  If items.Count = 1 Then
                                                                                      desc += " " + items(0).ToString
                                                                                  End If
                                                                                  Dim qAllocs = From itm In items
                                                                                                  Let tdescitem = ComponentModel.TypeDescriptor.GetProperties(itm)("FileName"),
                                                                                                  FileName = CType(tdescitem.GetValue(itm), String)
                                                                                                  From alloc In theheapAllocs
                                                                                                  Where alloc.IsTrkBlkType(ttype)
                                                                                                  Where alloc.GetJitInfoAsmName = FileName
                                                                                                  Select alloc


                                                                                  ShowSubSnapShot(qAllocs.ToList, desc)

                                                                              End Sub, 0)

                                                                          thetabItem.Content = br

                                                                      End Sub)
                                        tabJitDist.Tag = thebmem

                                    Case TrkType.ClrGCHnd
                                        qDetails = From hctr In theheapAllocs
                                                   Where hctr.IsTrkBlkType(ttype) OrElse
                                                   fIncludeCMarkerLam(hctr)
                                                   Select
                                                        Address = hctr.TBlk.Address.ToString("x8"),
                                                        hctr.AllocationStruct.SeqNo,
                                                        hctr.AllocationStruct.Thread,
                                                        GCHandle = hctr.GetGCHndInfoHandle.ToInt32.ToString("x8"),
                                                        InitialObjId = hctr.GetGCHndInfoInitialObjectId.ToInt32.ToString("x8"),
                                                        CurObjId = ClrClassInfo.GetGCHandleTargetObjectId(hctr.GetGCHndInfoHandle).ToInt32.ToString("x8"),
                                                        ClassName = ClrClassInfo.GetGCHandleTargetObj(hctr.GetGCHndInfoHandle),
                                                        _HeapAllocationContainer = hctr
                                                    Order By SeqNo

                                        '           ClassName = ClrClassInfo.GetClassNameFromClassOrObjectId(IntPtr.Zero, hctr.GetGCHndInfoInitialObjectId, fExpandSystemStringOrArray:=True),

                                        thebmem.ColumnTips = {TIP_ADDRESS, TIP_SEQNO, TIP_THREAD,
                                                              "GCHandle",
                                                              "Initial ObjectId",
                                                              "Class Name of InitialObjectId"
                                                             }

                                        thebmem._ColWidths = {WIDTH_ADDRESS, WIDTH_SEQNO, 60, 70, 70, 70, 900}

                                    Case TrkType.ClrExcpt
                                        qDetails = From hCtr In theheapAllocs
                                        Where hCtr.IsTrkBlkType(ttype) OrElse
                                            fIncludeCMarkerLam(hCtr)
                                        Select
                                            Address = hCtr.TBlk.Address.ToString("x8"),
                                            hCtr.AllocationStruct.SeqNo,
                                            hCtr.AllocationStruct.Thread,
                                            ExceptionObjId = hCtr.GetExcptObjId,
                                            ExceptionClassId = hCtr.GetExcptClassId,
                                            ExceptClassName = hCtr.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True),
                                            BlockType = hCtr.GetBlockTypeName,
                                            _HeapAllocationContainer = hCtr
                                        Order By SeqNo

                                        thebmem.ColumnTips = {TIP_ADDRESS, TIP_SEQNO, TIP_THREAD,
                                                              "Exception ObjectID",
                                                              "Exception ClassID",
                                                              "Exception Class Name"
                                                             }

                                        thebmem._ColWidths = {WIDTH_ADDRESS, WIDTH_SEQNO, 65, 375, 75, 600}

                                    Case TrkType.ClrClasses
                                        qDetails = From hCtr In theheapAllocs
                                            Where hCtr.IsTrkBlkType(ttype) OrElse
                                                 fIncludeCMarkerLam(hCtr)
                                            Let classData = hCtr.GetClassLayoutFromClassIdForHCtr()
                                            Select
                                                Address = hCtr.TBlk.Address.ToString("x8"),
                                                hCtr.AllocationStruct.SeqNo,
                                                hCtr.AllocationStruct.Thread,
                                                Instances = classData.classNumInstances,
                                                Collected = classData.classNumCollected,
                                                ClassSize = classData.classSize,
                                                TotalSize = classData.classSize * classData.classNumInstances,
                                                ClassName = classData.className,
                                                ParentClass = ClrClassInfo.GetClassNameFromClassOrObjectId(classData.classIdParent),
                                                ClsIDParent = classData.classIdParent.ToInt32.ToString("x8"),
                                                _HeapAllocationContainer = hCtr
                                            Order By SeqNo

                                        thebmem.ColumnTips = {TIP_ADDRESS, TIP_SEQNO, TIP_SIZE, TIP_THREAD,
                                                              "current # of live instances of this class",
                                                              "# of instances of this class which were collected",
                                                              "# Instances * ClassSize"}
                                        thebmem._ColWidths = {WIDTH_ADDRESS, WIDTH_SEQNO, 60, 65, 65, 65, 70, 600, 600, WIDTH_ADDRESS}
                                        thebmem._arrColumnsToTotal = {"Instances", "Collected", "TotalSize"}

                                    Case TrkType.ClrObjects

                                        qDetails = From hCtr In theheapAllocs
                                                    Where hCtr.IsTrkBlkType(ttype) OrElse
                                                         fIncludeCMarkerLam(hCtr)
                                                    Select
                                                        Address = hCtr.TBlk.Address.ToString("x8"),
                                                        hCtr.AllocationStruct.SeqNo,
                                                        hCtr.TBlk.Size,
                                                        hCtr.AllocationStruct.Thread,
                                                        Gen = hCtr.GetGen,
                                                        Moved = hCtr.GetMovedCnt,
                                                        Srviv = hCtr.TBlk.UnionData2 >> 16,
                                                        Classid = hCtr.TBlk.UnionData1.ToString("x8"),
                                                        ClassName = hCtr.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True),
                                                        NumItems = hCtr.GetGenericOrArrayCount,
                                                        _HeapAllocationContainer = hCtr
                                                    Order By SeqNo

                                        thebmem.ColumnTips = {TIP_ADDRESS, TIP_SEQNO, TIP_SIZE, TIP_THREAD,
                                                              "CLR Generation. Gen 3 is LargeObjectHeap",
                                                              "# times this object moved in GC",
                                                              "# times this object survived a GC",
                                                              "Class ID of object",
                                                              "Class Name. For System.String, the (partial) string contents are displayed",
                                                              "Number of items in a generic collection or array (# in array of a collection could be larger than it's collection due to growth algortihm)"
                                                             }

                                        thebmem._ColWidths = {WIDTH_ADDRESS, WIDTH_SEQNO, 60, 60, 60, 60, 60, WIDTH_ADDRESS, 900}

                                        thebmem._arrColumnsToTotal = {"Size", "NumItems", "Moved", "Srviv"}
                                        Dim objsum = From tab In thebmem._TabControl.Items
                                                    Let tbitem = CType(tab, TabItem)
                                                    Where TryCast(tbitem.Tag, String) Is Nothing AndAlso
                                                    TryCast(tbitem.Tag, BrowseMem) IsNot Nothing


                                        If objsum.Count = 0 Then
                                            Dim clrTabItem = thebmem.AddTabItemForBrowMem(
                                                "CLR Obj Summary",
                                                "Currently allocated Blocks grouped by Class Name",
                                                AddressOf OnClrObjSummaryGotFocus)

                                            clrTabItem.Tag = thebmem
                                        End If
                                    Case TrkType.CodeMarker
                                        qDetails = From hCtr In theheapAllocs
                                        Where hCtr.IsTrkBlkType(ttype)
                                        Let cmData = hCtr.GetCodeMarkerData
                                        Select
                                            Address = hCtr.TBlk.Address.ToString("x8"),
                                            hCtr.AllocationStruct.SeqNo,
                                            hCtr.AllocationStruct.Thread,
                                            Marker = cmData.MarkerNameEx,
                                            UserDataAddr = cmData.MarkerUserDataAddr.ToInt32.ToString("x8"),
                                            cmData.MarkerUserDataLen,
                                            CodeMarkerUserData = cmData.MarkerUserData,
                                            MarkerInstance = cmData.MarkerInstance,
                                            Depth = cmData.MarkerDepth,
                                            MarkerId = hCtr.TBlk.UnionData1,
                                            MarkerRawName = cmData.MarkerName,
                                            _HeapAllocationContainer = hCtr
                                        Order By SeqNo

                                        thebmem._ColWidths = {WIDTH_ADDRESS, WIDTH_SEQNO, 60, 500, WIDTH_ADDRESS, 65, 300, 65, 40}
                                        thebmem.ColumnTips = {TIP_ADDRESS, TIP_SEQNO, TIP_THREAD,
                                                              "Code marker name from Microsoft.Internal.Performance.CodeMarkers.dll",
                                                              "Some codemarkers have VOID *UserData. This is copied by MemSpect",
                                                              "The size of the UserData",
                                                              "The userdata. CodeMarkerGuidLookup.txt translates things like GUIDs to values",
                                                              "The instance of this marker (1,2,3...). End markers have Instance # = the Begin",
                                                              "The nesting depth of the marker (Begin/End pairs)"
                                                             }
                                        thebmem._arrColumnsToTotal = Nothing

                                        Dim aggCmItem = From tab In thebmem._TabControl.Items
                                                  Let tbitem = CType(tab, TabItem)
                                                  Where TryCast(tbitem.Tag, String) IsNot Nothing AndAlso
                                                  CStr(tbitem.Tag) = "CodeMarker"

                                        If aggCmItem.Count = 0 Then

                                            Dim aggTabItem = thebmem.AddTabItemForBrowMem(
                                                "CodeMarker Aggregate",
                                                "CodeMarkers aggregated",
                                                Sub(sender2 As Object, e2 As RoutedEventArgs)
                                                    Dim thetabItem = CType(sender2, TabItem)
                                                    Dim qAggCodeMarkers = From hctr In theheapAllocs
                                                                          Where hctr.IsTrkBlkType(ttype)
                                                                          Let MarkerTemp = hctr.GetCodeMarkerName(fIncludeInstanceNum:=False)
                                                                          Let Marker = IIf(MarkerTemp = "UnknownMarker", MarkerTemp + hctr.TBlk.UnionData1.ToString(), MarkerTemp).ToString()
                                                                          Group By Marker Into Count()
                                                                          Order By Count Descending
                                                                          Select Marker, Count

                                                    Dim brcmAgg = New Browse(qAggCodeMarkers,
                                                                             ColWidths:={500, 80},
                                                                             arrColumnsToTotal:={"Count"}
                                                                             )
                                                    brcmAgg._BrowseList.ContextMenu.AddMnuItem(
                                                        "_SubSnapshot",
                                                        "Create a new snapshot from the selected items",
                                                        Sub()
                                                            Dim items = brcmAgg._BrowseList.SelectedItems
                                                            If items Is Nothing OrElse items.Count < 1 Then
                                                                items = brcmAgg._BrowseList.Items
                                                            End If
                                                            Dim qAllocs = From itm In items
                                                                            Let tdescitem = ComponentModel.TypeDescriptor.GetProperties(itm)("Marker"),
                                                                            Marker = CType(tdescitem.GetValue(itm), String)
                                                                            From alloc In theheapAllocs
                                                                            Where alloc.IsTrkBlkType(ttype)
                                                                            Where alloc.GetCodeMarkerName = Marker
                                                                            Select alloc


                                                            ShowSubSnapShot(qAllocs.ToList, "SubSnapCodeMarkers")

                                                        End Sub, 0)




                                                    thetabItem.Content = brcmAgg

                                                End Sub)
                                            aggTabItem.Tag = "CodeMarker"
                                        End If

                                    Case TrkType.ThreadInfo
                                        qDetails = From hCtr In theheapAllocs
                                        Where hCtr.IsTrkBlkType(ttype) OrElse
                                             fIncludeCMarkerLam(hCtr)
                                        Select
                                            Address = hCtr.TBlk.UnionData1.ToString("x8"),
                                            hCtr.AllocationStruct.SeqNo,
                                            hCtr.AllocationStruct.Thread,
                                            Data = hCtr.GetBlockTypeName,
                                            _HeapAllocationContainer = hCtr
                                        Order By SeqNo

                                        thebmem.ColumnTips = {"For Tls, this is just a 1,2,3. For Threads, this is the new thread's OS Handle",
                                                              TIP_SEQNO, TIP_THREAD,
                                                              "for thread, the new thread id and the address of the Thread Start routine"}
                                        thebmem._ColWidths = {WIDTH_ADDRESS, WIDTH_SEQNO, 60, 800}

                                        thebmem._arrColumnsToTotal = Nothing

                                    Case TrkType.LoadResource
                                        qDetails = From hCtr In theheapAllocs
                                                   Where hCtr.IsTrkBlkType(ttype) OrElse
                                                       fIncludeCMarkerLam(hCtr)
                                                Let mbi = GetMBIForAddress(New IntPtr(hCtr.TBlk.UnionData1))
                                                Select
                                                    Address = hCtr.TBlk.Address.ToString("x8"),
                                                    hCtr.AllocationStruct.SeqNo,
                                                    hCtr.TBlk.Size,
                                                    hCtr.AllocationStruct.Thread,
                                                    hResInfo = hCtr.TBlk.UnionData2.ToString("x8"),
                                                    Data = hCtr.GetDisplayData(HeapAllocationContainer.GetDisplayDataEnum.DisplayShort),
                                                    _HeapAllocationContainer = hCtr
                                                Order By SeqNo

                                        thebmem.ColumnTips = {TIP_ADDRESS, TIP_SEQNO, TIP_SIZE, TIP_THREAD,
                                                              "HResInfo: parameter to LoadResource (typically found by FindResource)",
                                                              "File from which the resource is loaded."}

                                        thebmem._ColWidths = {WIDTH_ADDRESS, WIDTH_SEQNO, 60, 60, 70, 500}

                                        thebmem._arrColumnsToTotal = {"Size"}


                                End Select
                                thebmem.InitialSortOrder = New BrowseInitialSortOrder With {.ColumnNo = 2, .direction = ComponentModel.ListSortDirection.Ascending}

                                Return qDetails
                            End Function

                Me.Cursor = Cursors.Wait
                If _bmem IsNot Nothing Then
                    _bmem.ClearPriorToolTipIfAny()
                    Me.Children.Remove(_bmem)
                End If
                _bmem = New BrowseMem(qfunc, _snap.Allocs, ColWidths, ttype, fAllowBrowStringFilter:=True)

                'Everytime we create a new BrowseMem, we need to tell it what custom columns we want displayed.
                For Each columnFactory In _CustomColumnFactories.Values
                    _bmem.AddCustomColumnFactory(columnFactory)
                Next

                _ctrlTxtTotal.Text = String.Format("Total Cnt= {0:n0} Size = {1:n0}", _bmem.nTotal, _bmem.nTotalSize)
                Me.Children.Add(_bmem)
                Me.Cursor = Cursors.Arrow
            Catch ex As Exception
                MemSpectExceptionHandler(ex)
            End Try

        End Sub

        Sub OnClrObjSummaryGotFocus(ByVal sender As Object, ByVal e As RoutedEventArgs)
            Dim thetabItem = CType(sender, TabItem)
            Dim bmem = CType(thetabItem.Tag, BrowseMem)

            Dim qObjSum = From hCtr In bmem._allocs
                Where hCtr.TBlkBlockType = BlockTypes.ClrObject
                Select
                    hCtr.TBlk.Size,
                    ClassName = hCtr.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=False),
                    Classid = hCtr.TBlk.UnionData1.ToString("x8")
                    Group By ClassName, Classid
                    Into Cnt = Count(), Size = Sum(Size)
                    Select
                    ClassName,
                    Classid,
                    Count = Cnt,
                    TotalSize = Size
                    Order By ClassName


            Dim coltips = {"Class Name: System.String is not expanded to include string contents",
                           "Class ID",
                           "# of live instances",
                           "# of live instances X size of object"}

            Dim ClrObjSummaryBrowse = New Browse(qObjSum,
                                              fAllowHeaderClickSort:=True,
                                              ColWidths:={1000, 80, 80},
                                              fAllowBrowFilter:=True,
                                              coltips:=coltips,
                                              arrColumnsToTotal:={"Count", "TotalSize"})
            thetabItem.Content = ClrObjSummaryBrowse

            Dim cm = ClrObjSummaryBrowse._BrowseList.ContextMenu

            Dim mitem = cm.AddMnuItem("Show _GC Roots",
                                  "Show all objects that are GC Roots",
                                  AddressOf On_ClrObjSumCtxMenu, 0)

            mitem = cm.AddMnuItem("_References",
                                      "At most 1 Reference from and to this object",
                                      AddressOf On_ClrObjSumCtxMenu, 0)
            mitem.Tag = ClrObjSummaryBrowse

            mitem = cm.AddMnuItem("SubSnapshot _Generic Collections only",
                          "SubSnap of all items that start with ""System.Collections.Generic.""",
                          AddressOf On_ClrObjSumCtxMenu, 0)

            mitem.Tag = ClrObjSummaryBrowse

            mitem = cm.AddMnuItem("_Unused Members",
                                      "Show unused members of this class, Show class member value distribution",
                                      AddressOf On_ClrObjSumCtxMenu, 0)
            mitem.Tag = ClrObjSummaryBrowse

            mitem = cm.AddMnuItem("_SubSnapshot",
                          "SubSnap of just the selected item(s)",
                          AddressOf On_ClrObjSumCtxMenu, 0)

            mitem.Tag = ClrObjSummaryBrowse


        End Sub

        Sub On_ClrObjSumCtxMenu(ByVal sender As Object, ByVal e As RoutedEventArgs)
            Dim mitem = CType(e.OriginalSource, MenuItem)
            Dim vrb = mitem.Header.ToString
            Dim ClrObjSummaryBrowse = CType(mitem.Tag, Browse)
            Select Case vrb
                Case "_SubSnapshot", "_Unused Members", "_References" '                        Classid = hCtr.TBlk.UnionData1.ToString("x8")
                    Dim classIds = New SortedSet(Of Integer)
                    Dim subsetTitle = vrb.Substring(1) + " "
                    For Each item In ClrObjSummaryBrowse._BrowseList.SelectedItems
                        Dim tdescitem = ComponentModel.TypeDescriptor.GetProperties(item)("Classid")
                        Dim classIdHex = CStr(tdescitem.GetValue(item))
                        Dim classId = Integer.Parse(classIdHex, System.Globalization.NumberStyles.AllowHexSpecifier)
                        classIds.Add(classId)
                        subsetTitle += CStr(ComponentModel.TypeDescriptor.GetProperties(item)("ClassName").GetValue(item)) + " "
                    Next

                    Dim q = From alloc In _bmem._allocs
                            Where classIds.Contains(alloc.TBlk.UnionData1)
                            Where alloc.TBlkBlockType = BlockTypes.ClrObject

                    Select Case vrb
                        Case "_Unused Members"
                            ShowUnusedMembers(q, subsetTitle)
                        Case "_References"
                            For Each hctr In From h In q Take 1 ' limit to only 1
                                TVObjRefPanel.CreateObjectReferenceDataSurface(hctr, subsetTitle + " " + hctr.ToString)
                            Next
                        Case "_SubSnapshot"
                            ShowSubSnapShot(q.ToList, subsetTitle)
                    End Select
                Case "Show _GC Roots"
                    TVObjRefPanel.ShowGCRoots()
                Case "SubSnapshot _Generic Collections only"
                    Dim generics = New List(Of HeapAllocationContainer)
                    For Each hctr In _bmem._allocs
                        Dim cName = hctr.GetClassNameFromHeapCtr(fExpandSystemStringOrArray:=True)
                        If cName.StartsWith("System.") AndAlso
                            cName.Contains("Generic") AndAlso
                            cName.Contains("Count=") Then
                            generics.Add(hctr)
                        End If

                    Next
                    ShowSubSnapShot(generics, "Generic SubSnapshot")
            End Select

        End Sub

    End Class
    Public Class VirtAllocColorizer
        Implements IValueConverter


        Public Function Convert(ByVal value As Object, ByVal targetType As System.Type, ByVal parameter As Object, ByVal culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.Convert
            Dim lvitem = CType(value, ListViewItem)
            Dim result = CType(ComponentModel.TypeDescriptor.GetProperties(lvitem.DataContext)("CurMemType").GetValue(lvitem.DataContext), String)
            If result.Contains("COMMIT") Then
                Return Brushes.Yellow
            ElseIf Not result.Contains("FREE") Then
                Return Brushes.YellowGreen
            End If
            Return Brushes.White
        End Function

        Public Function ConvertBack(ByVal value As Object, ByVal targetType As System.Type, ByVal parameter As Object, ByVal culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.ConvertBack
            Throw New NotImplementedException
        End Function
    End Class
End Namespace