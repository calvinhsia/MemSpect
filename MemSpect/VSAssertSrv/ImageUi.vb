Imports System.Runtime.InteropServices
Imports MemSpect.Images
Imports System.Reflection

Namespace MemSpect

    Public Class ImageUi
        Private _vm As VirtualMem
        Friend _bmemImages As BrowseMem
        Friend _TabControl As MyTabControl
        Public Sub New(ByVal vm As VirtualMem)
            _vm = vm
        End Sub
        Public Class ImageDynamicBackground
            Implements IValueConverter

            Public Function Convert(ByVal value As Object, ByVal targetType As System.Type, ByVal parameter As Object, ByVal culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.Convert
                Dim lvitem = CType(value, ListViewItem)
                Dim result = Brushes.White
                Dim ImData = CType(ComponentModel.TypeDescriptor.GetProperties(lvitem.DataContext)("ImageType").GetValue(lvitem.DataContext), String)

                Select Case ImData
                    Case Images.ImageTypeEnum.Managed.ToString
                        result = Brushes.LightGreen
                    Case Images.ImageTypeEnum.Mixed.ToString
                        result = Brushes.Yellow
                    Case Images.ImageTypeEnum.Resource.ToString
                        result = Brushes.LightPink
                End Select
                Return result

            End Function

            Public Function ConvertBack(ByVal value As Object, ByVal targetType As System.Type, ByVal parameter As Object, ByVal culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.ConvertBack
                Throw New NotImplementedException
            End Function
        End Class

        Friend Function ShowImageAllocs(ByVal lstAllocs As IEnumerable(Of Imagedata)) As BrowseMem
            Dim bmemImages As BrowseMem
            Dim virtallocs = GetVirtAllocs()
            Dim getRTVersion = Function(img As Imagedata) As String
                                   Dim rtVer = String.Empty
                                   If img.ImageType = ImageTypeEnum.Mixed OrElse img.ImageType = ImageTypeEnum.Managed Then
                                       Try
                                           Dim asm = Assembly.ReflectionOnlyLoadFrom(img.FullPathFileName)
                                           rtVer = asm.ImageRuntimeVersion
                                       Catch ex As Exception
                                           'Exception: System.IO.FileLoadException: API restriction: The assembly 'file:///C:\Windows\assembly\NativeImages_v4.0.30319_32\CustomMarshalers\c938644b4da3891f19a116f38c368f1b\CustomMarshalers.ni.dll' has already loaded from a different location. It cannot be loaded from a new location within the same appdomain. 
                                           ' ILDasm can open a file even if ReflectionOnlyLoadFrom can't
                                           UpdateStatusMsg("Exception getting version from " + img.FullPathFileName + " " + ex.ToString())
                                       End Try
                                   End If
                                   Return rtVer
                               End Function
            Dim qfuncImages = Function(theheapAllocs As List(Of HeapAllocationContainer), thebmem As BrowseMem) As IEnumerable
                                  Dim q = From img As Imagedata In lstAllocs
                                          Let mbi = GetMBIForAddress(img.ImageBaseMem, virtallocs)
                                          Select
                                                  Address = img.ImageBaseMem.ToString("x8"),
                                                  File = img.Filename,
                                                  Path = img.FilePath,
                                                  img.AllocationStruct.SeqNo,
                                                  img.Instance,
                                                  img.SizeOnDisk,
                                                  img.LrgstConsecZeros,
                                                  img.VMSize,
                                                  VMWaste = (65536 - (img.VMSize Mod 65536)) Mod 65536,
                                                  ImageType = img.ImageType.ToString,
                                                  RTVersion = getRTVersion(img),
                                                  img.OptimizationInfo,
                                                  img.OptPartialNGen,
                                                  img.LangIDs,
                                                  img.code,
                                                  img.data,
                                                  img.rsrc,
                                                  img.reloc,
                                                  ImageBasePE = img.ImageBasePE.ToString("x8"),
                                                  ImageBasePEMem = img.ImageBasePEMem.ToInt32.ToString("x8"),
                                                  img.Relocated,
                                                  img.Sections,
                                                  img.DynamicBase,
                                                  _HeapAllocationContainer = img


                                  thebmem.ColumnTips = {"A pointer to the base address of the image load.",
                                                 "Filename with no path",
                                                 "Path to file with no filename",
                                                 TIP_SEQNO,
                                                 "Instance #: 2 means it's loaded twice",
                                                 "Size of file on disk",
                                                 "Largest block of consecutive zeros",
                                                 "Virtual Memory size",
                                                 "Fragment of VM leftover (65536 - (img.VMSize Mod 65536)) Mod 65536",
                                                 "Managed or Native or Mixed or Resource",
                                                 "For managed, version # (Assembly.ImageRuntimeVersion)",
                                                 "Optimization info (like BBT (native) or IBC (mgd). Right-click->Show Optimazation Details for more info)",
                                                 "PartialNGen flag",
                                                 "LanguageIDs found in resources",
                                                 "size of .Code section in DLL",
                                                 "size of .Data section",
                                                 "size of .Rsrc section",
                                                 "Size of .Reloc section",
                                                 "Imagebase in PE Header of file from DumpBin",
                                                 "Imagebase in PE Header in memory (read from PEHeader of file in memory. Different because of ASLR)",
                                                 "1 if Address != ImageBasePEMem && code >0 (must have code). ASLR makes most relocated",
                                                 "Number of Secions",
                                                 "1 = Dynamic base (ASLR)"
                                                 }

                                  thebmem._ColWidths = {70, 270, 570, 70, 60, 100, 70, 70, 70, 120, 120, 120, 120, 90, 90, 80, 80, 80, 80, 80}
                                  thebmem._arrColumnsToTotal = {"SizeOnDisk", "VMSize", "VMWaste", "code", "data", "rsrc", "reloc"}

                                  Return q
                              End Function

            Dim lstHeapAlloc = New List(Of HeapAllocationContainer)
            lstHeapAlloc.AddRange(lstAllocs)

            bmemImages = New BrowseMem(qfuncImages, lstHeapAlloc, fAllowBrowStringFilter:=True)
            bmemImages.IsPivotLimited = True
            bmemImages.DynamicBackgroundConverter = New ImageDynamicBackground

            AddHandler bmemImages.OnTabItemDetailsCreatedEvent, Sub(sender As Object, e As RoutedEventArgs)
                                                                    AddMenuItemsForBrowImages(bmemImages._DetailBrowse)
                                                                End Sub
            Return bmemImages
        End Function

        Private Sub AddMenuItemsForBrowImages(ByVal brImages As Browse)
            brImages._BrowseList.ContextMenu.Tag = brImages
            brImages._BrowseList.ContextMenu.AddMnuItem(
                "Show Optimi_zation Details",
                "For managed dlls with IBC opt info, shows details in XML of image optimization (including sceanrios, methods, blocks)" + vbCrLf +
                "IBC info is embedded as a resource in a Managed DLL" + vbCrLf +
                "See resources tab, ResType='IBC', ResName='PROFILE_DATA'",
                AddressOf ShowOptimizationDetails
                )

            brImages._BrowseList.ContextMenu.AddMnuItem(
                "Analyze all ima_ges in a Folder",
                "Show optimization, resources, duplicates for files in a particular folder",
                AddressOf ShowOptDataForFolder
                )

        End Sub


        Friend Sub ShowOptimizationDetails(ByVal sender As Object, ByVal e As RoutedEventArgs)
            Try
                Dim _browimages As Browse = CType(CType(CType(sender, MenuItem).Parent, ContextMenu).Tag, Browse)
                For Each itm In _browimages._BrowseList.SelectedItems
                    Dim imgd = CType(ComponentModel.TypeDescriptor.GetProperties(itm)("_HeapAllocationContainer").GetValue(itm), Imagedata)

                    Dim xmlfileOut = IO.Path.Combine(IO.Path.GetTempPath, IO.Path.ChangeExtension(imgd.Filename, "xml"))
                    Dim cmdArgs = String.Format("-v3 -f -dxml {0} -mi ""{1}""  ", xmlfileOut, imgd.FullPathFileName)
                    Dim ibcMergePath = IO.Path.Combine(IO.Path.GetDirectoryName(Reflection.Assembly.GetExecutingAssembly.Location), "ibcmerge.exe")
                    If Not IO.File.Exists(ibcMergePath) Then
                        Throw New IO.FileNotFoundException(ibcMergePath)
                    End If
                    StartProcess(ibcMergePath, cmdArgs)
                    If Not IO.File.Exists(xmlfileOut) Then
                        Throw New InvalidOperationException("No Optimization data found for " + imgd.FullPathFileName)
                    End If
                    UpdateStatusMsg("Starting ImgDetails " + xmlfileOut)
                    Process.Start(xmlfileOut)

                Next

            Catch ex As Exception
                CommonUI.MemSpectExceptionHandler(ex)
            End Try
        End Sub

        Private Sub ShowOptDataForFolder(ByVal sender As Object, ByVal e As RoutedEventArgs)
            Try
                Dim _browimages As Browse = CType(CType(CType(sender, MenuItem).Parent, ContextMenu).Tag, Browse)
                Dim itm = _browimages._BrowseList.SelectedItems(0)
                Dim imgd = CType(ComponentModel.TypeDescriptor.GetProperties(itm)("_HeapAllocationContainer").GetValue(itm), Imagedata)
                ShowOptDataForFolderHelper(IO.Path.GetDirectoryName(imgd.FullPathFileName), fShowBrowserDialog:=True)
            Catch ex As Exception
                CommonUI.MemSpectExceptionHandler(ex)
            End Try
        End Sub

        Friend Shared _LastOptDataFolderPath As String

        Friend Function ShowOptDataForFolderHelper(ByVal DefaultFilePath As String, ByVal fShowBrowserDialog As Boolean) As DataSurface
            Dim ctrls As DataSurface = Nothing
            Try
                If String.IsNullOrEmpty(DefaultFilePath) Then
                    DefaultFilePath = _LastOptDataFolderPath
                End If
                If String.IsNullOrEmpty(DefaultFilePath) Then
                    DefaultFilePath = IO.Path.GetDirectoryName(Reflection.Assembly.GetExecutingAssembly.Location)
                End If
                Dim FilePath = String.Empty
                If fShowBrowserDialog Then
                    Dim flderBrowse = New Forms.FolderBrowserDialog

                    flderBrowse.Description = "Folder from which files will be examined for optimization"
                    flderBrowse.SelectedPath = DefaultFilePath
                    If flderBrowse.ShowDialog() = Forms.DialogResult.OK AndAlso Not String.IsNullOrEmpty(flderBrowse.SelectedPath) Then
                        FilePath = flderBrowse.SelectedPath
                        _LastOptDataFolderPath = FilePath
                    End If
                Else
                    FilePath = DefaultFilePath
                End If
                If Not String.IsNullOrEmpty(FilePath) Then
                    Dim files = {"*.exe", "*.dll"}.SelectMany(Function(filt) IO.Directory.GetFiles(FilePath, filt, IO.SearchOption.TopDirectoryOnly))
                    Dim listFileData As New SortedList(Of String, Imagedata)
                    If files.Count = 0 Then
                        Throw New IO.FileNotFoundException("No *.exe| *.dll files in " + FilePath)
                    End If
                    For Each file In files
                        Dim key = IO.Path.GetFileName(file).ToLower
                        If listFileData.ContainsKey(key) Then
                            UpdateStatusMsg("File already found " + file)
                        Else
                            Dim ndx = listFileData.Count + 1
                            listFileData.Add(key,
                                             New Imagedata With {
                                                 .FullPathFileName = file,
                                                 .ResultIndex = ndx,
                                                 .SizeOnDisk = CUInt((New IO.FileInfo(file)).Length),
                                                 .VMSize = .SizeOnDisk
                                                 }
                                             )

                        End If
                    Next


                    Dim imgui = New ImageUi(vm:=Nothing)
                    ctrls = imgui.DoShowImageData(analyzer:=Nothing,
                                          ImageResourcesDict:=Nothing,
                                          lstImageSymbolNames:=Nothing,
                                          listImage:=listFileData,
                                          title:="FolderOpt",
                                          tip:="Optimization info for *.dll, *.exe files in " + FilePath
                                          )

                End If
            Catch ex As Exception
                CommonUI.MemSpectExceptionHandler(ex)
            End Try
            Return ctrls
        End Function

        Public Class ImageSymbolNameConverterDlls
            Implements IValueConverter

            Public Function Convert(ByVal value As Object, ByVal targetType As System.Type, ByVal parameter As Object, ByVal culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.Convert
                Dim dd = CType(value, List(Of ImageSymboldata))
                Dim q = From a In dd
                        Select a.imagedat.Filename,
                        Path = a.imagedat.FullPathFileName
                'Dim br = New Browse.BrowseList(q)
                Return q
            End Function

            Public Function ConvertBack(ByVal value As Object, ByVal targetType As System.Type, ByVal parameter As Object, ByVal culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.ConvertBack
                Throw New NotImplementedException
            End Function
        End Class

        Public Class ImageSymbolNameConverterMembers
            Implements IValueConverter

            Public Function Convert(ByVal value As Object, ByVal targetType As System.Type, ByVal parameter As Object, ByVal culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.Convert
                Dim dd = CType(value, List(Of ImageSymboldata))

                Dim q = From a In dd
                        Select Mems = a.Members
                        From m In Mems
                        Select m.Name, m.MemberType

                'Dim q = From a In dd
                '        Select a.Members,
                '        a.MemberType
                ''Dim br = New Browse.BrowseList(q)
                'Return q
                Return q
            End Function

            Public Function ConvertBack(ByVal value As Object, ByVal targetType As System.Type, ByVal parameter As Object, ByVal culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.ConvertBack
                Throw New NotImplementedException
            End Function
        End Class

        Friend Shared Function DoShowImageExports(ByVal analyser As ImageAnalyzer) As Browse
            Dim browExports As Browse = Nothing

            Return browExports
        End Function

        'Friend Shared Function DoShowImageSymbolNameDupes(ByVal analyzer As ImageAnalyzer) As Browse
        '    Dim browSymDupes As Browse = Nothing



        '    Dim q = From a In analyzer._lstImageSymbolNames
        '            Where a.Value.Count > 2
        '            Select a.Key

        '    browSymDupes = New Browse(q)

        '    Return browSymDupes
        'End Function

        Friend Shared Function DoShowImageSymbolNames(ByVal lstImageSymbolNames As SortedList(Of String, List(Of ImageSymboldata))) As Browse
            Dim browSymbolNames As Browse = Nothing
            Dim q = From a In lstImageSymbolNames
                    Select Symbol = a.Key,
                            Files = a.Value,
                            ImageType = a.Value.FirstOrDefault.imagedat.ImageType.ToString,
                            Members = a.Value,
                            ImageCount = a.Value.Count

            browSymbolNames = New Browse(q, fAllowBrowFilter:=True)

            Dim lamAddCol = Sub(colName As String, valconv As IValueConverter)
                                Dim lvFactory = New FrameworkElementFactory(GetType(ListView))
                                lvFactory.SetBinding(
                                    ListView.ItemsSourceProperty,
                                    New Binding(colName) With {
                                        .Mode = BindingMode.OneTime,
                                        .Converter = valconv
                                        }
                                    )
                                lvFactory.SetValue(ListView.MaxHeightProperty, 90.0)
                                Dim ctxmenu = New ContextMenu
                                ctxmenu.AddMnuItem("Dump to file", colName, Sub(sender As Object, e As RoutedEventArgs)
                                                                                Dim mitem = CType(e.OriginalSource, MenuItem)
                                                                                Dim itmSel = browSymbolNames._BrowseList.SelectedItem
                                                                                If itmSel Is Nothing Then
                                                                                    Throw New InvalidOperationException("Please Select an item in the main list first")
                                                                                End If
                                                                                Dim td = ComponentModel.TypeDescriptor.GetProperties(itmSel)(colName).GetValue(itmSel)
                                                                                Dim tenum = TryCast(td, IEnumerable)
                                                                                If tenum IsNot Nothing Then
                                                                                    Dim sb = New Text.StringBuilder
                                                                                    sb.AppendLine(itmSel.ToString)
                                                                                    Dim val = CType(valconv.Convert(tenum, GetType(IEnumerable), Nothing, Nothing), IEnumerable)
                                                                                    For Each itm In val
                                                                                        sb.AppendLine(itm.ToString)
                                                                                    Next
                                                                                    WriteOutputToTempFile(sb.ToString)
                                                                                End If
                                                                            End Sub)

                                lvFactory.SetValue(ListView.ContextMenuProperty, ctxmenu)
                                Dim newColDLLs = New GridViewColumn With
                                             {
                                                 .Header = New GridViewColumnHeader With {.Content = colName},
                                                 .CellTemplate = New DataTemplate With {
                                                     .VisualTree = lvFactory
                                                 }
                                             }


                                CType(browSymbolNames._BrowseList.View, GridView).Columns.Add(newColDLLs)

                            End Sub

            lamAddCol.Invoke("Files", New ImageSymbolNameConverterDlls)
            lamAddCol.Invoke("Members", New ImageSymbolNameConverterMembers)

            browSymbolNames.SetDynamicBackground(New ImageDynamicBackground, fAdditive:=False)
            browSymbolNames._BrowseList.AddHandler(
                ListView.PreviewMouseRightButtonDownEvent,
                New MouseButtonEventHandler(
                Sub(sendr As Object, ee As MouseEventArgs)
                    ' we want to select the item you rt click on so context menu knows which item is selected
                    Dim lv = TryCast(sendr, ListView)

                    If lv IsNot Nothing Then
                        Dim pt = ee.GetPosition(lv)
                        Dim elem = lv.InputHitTest(pt)
                        elem = CType(GetAncestor(Of ListViewItem)(CType(elem, DependencyObject)), IInputElement)
                        If elem IsNot Nothing Then ' inner listview
                            elem = CType(GetAncestor(Of ListViewItem)(CType(elem, DependencyObject), fGoUpOneFirst:=True), IInputElement)
                            If elem IsNot Nothing Then
                                Dim lvitem = CType(elem, ListViewItem)
                                browSymbolNames._BrowseList.SelectedItems.Clear()
                                browSymbolNames._BrowseList.SelectedItems.Add(lvitem.DataContext)

                                Debug.Assert(browSymbolNames._BrowseList.SelectedItems.Count = 1, "# items not 1?")
                            End If
                        End If
                    End If

                End Sub)
            )

            Return browSymbolNames
        End Function

        Friend Function DoShowImageData(ByVal analyzer As ImageAnalyzer,
                                        ByVal lstImageSymbolNames As SortedList(Of String, List(Of ImageSymboldata)),
                                        ByVal ImageResourcesDict As SortedDictionary(Of UInteger, List(Of ResourceData)),
                                        ByVal listImage As SortedList(Of String, Imagedata),
                                        Optional ByVal title As String = "Images",
                                        Optional ByVal tip As String = "Image Analysis (loaded files and their resources)"
                                        ) As DataSurface

            Dim ctrlsImages As DataSurface = Nothing

            Try
                Using New DataWindowMain.SetCursorWait
                    ctrlsImages = DataWindowMain.MakeNewDatasurface(title, tip, nMaxHeaderHeight:=65)
                    If analyzer Is Nothing Then
                        Dim sumry As New Imagedata ' to hold smmary info
                        ImageResourcesDict = New SortedDictionary(Of UInteger, List(Of ResourceData))
                        lstImageSymbolNames = New SortedList(Of String, List(Of ImageSymboldata))
                        ImageAnalyzer.ProcessImageDatList(listImage, sumry, ImageResourcesDict, lstImageSymbolNames)

                    End If

                    Dim qImageTypeSumry = From a In listImage.Values
                                          Group By a.ImageType Into Sum(a.VMSize), Count()
                                          Select ImageType = ImageType.ToString, Count, Sum
                                          Order By Sum Descending

                    Dim bImageSumry = New Browse(qImageTypeSumry)
                    bImageSumry._BrowseList.LayoutTransform = New ScaleTransform(VirtualMem.sumrySize, VirtualMem.sumrySize)
                    bImageSumry.MaxHeight = VirtualMem.nMaxHeader
                    bImageSumry.SetDynamicBackground(New ImageDynamicBackground, fAdditive:=False)

                    ctrlsImages.SurfaceHeader.Children.Add(bImageSumry)

                    If analyzer Is Nothing Then

                        Dim q = From a In listImage
                                Order By a.Key
                                Let img = a.Value
                                Select
                                    File = a.Key,
                                    Path = img.FilePath,
                                    img.SizeOnDisk,
                                    ImageType = img.ImageType.ToString,
                                    img.OptimizationInfo,
                                    img.OptPartialNGen,
                                    img.LangIDs,
                                    img.code,
                                    img.data,
                                    img.rsrc,
                                    img.reloc,
                                    img.Sections,
                                    _HeapAllocationContainer = img

                        Dim browFiles = New Browse(q, fAllowBrowFilter:=True)

                        AddMenuItemsForBrowImages(browFiles)

                        ctrlsImages.SurfaceHeader.Children.Add(New TextBlock With {
                                                         .Text = tip
                                                     }
                                                 )

                        browFiles.SetDynamicBackground(New ImageDynamicBackground, fAdditive:=False)
                        _TabControl = New MyTabControl
                        ctrlsImages.SurfaceDetails.Children.Add(_TabControl)
                        _TabControl.Items.Add(New TabItem With
                                              {
                                                  .Header = "Images",
                                                  .Content = browFiles,
                                                  .ToolTip = tip
                                                  }
                                              )

                    Else

                        Dim btnShowStacks = New Button With {
                            .Content = "_CallStacks",
                            .MaxHeight = 45,
                            .MaxWidth = 120,
                            .ToolTip = "Show images, their imports/exports, and load callstacks in a TXT file"}
                        AddHandler btnShowStacks.Click, Sub()
                                                            Process.Start(analyzer._StacksFileName)
                                                        End Sub
                        ctrlsImages.SurfaceHeader.Children.Add(btnShowStacks)

                        ctrlsImages.SurfaceHeader.Children.Add(New TextBlock With {.Text = analyzer._totSumry})
                        _bmemImages = ShowImageAllocs(listImage.Values)

                        ctrlsImages.SurfaceDetails.Children.Add(_bmemImages)
                        _TabControl = _bmemImages._TabControl
                    End If

                    _TabControl.AddTabItem("Resources",
                                           "embedded managed and native resources",
                                           Sub(sender As Object, e As RoutedEventArgs)
                                               Dim tabResources = CType(sender, TabItem)
                                               Dim listAllResouces = New List(Of ResourceData)
                                               For Each lstRsrcs In ImageResourcesDict.Values ' regardless of CRC
                                                   listAllResouces.AddRange(lstRsrcs)
                                               Next
                                               Dim qResources = From resD In listAllResouces
                                                              Let imgD = resD.ImgData
                                                              Select
                                                              imgD.Filename,
                                                              HGlobal = resD.HGlobal.ToInt32.ToString("x8"),
                                                              resD.ResSize,
                                                              resD.ResName,
                                                              resD.ResType,
                                                              resD.langId,
                                                              ImageType = imgD.ImageType.ToString,
                                                              imgD.FilePath,
                                                              _HeapAllocationContainer = imgD,
                                                              _ResourceData = resD

                                               Dim brResources = New Browse(
                                                                 qResources,
                                                                 fAllowBrowFilter:=True,
                                                                 arrColumnsToTotal:={"ResSize"}
                                                                 )
                                               tabResources.Content = brResources
                                               brResources.SetDynamicBackground(New ImageDynamicBackground, fAdditive:=False)
                                               AddDupeContextMenuItemAndEventHandlers(brResources)


                                           End Sub
                    )



                    GetDupeImageResources(ImageResourcesDict, fIsSubSnap:=False)

                    _TabControl.AddTabItem("ImageSymbolNames",
                                           "Names from assemblies",
                                           Sub(sender As Object, e As RoutedEventArgs)
                                               Dim tabImageSymbolNames = CType(sender, TabItem)
                                               tabImageSymbolNames.Content = DoShowImageSymbolNames(lstImageSymbolNames)
                                           End Sub
                    )

                End Using

            Catch ex As Exception
                CommonUI.MemSpectExceptionHandler(ex)
            End Try
            Return ctrlsImages
        End Function

        Private _lastTTObj As TextBlock = Nothing
        Private Sub ClearPriorTipIfany()
            If _lastTTObj IsNot Nothing AndAlso _lastTTObj.ToolTip IsNot Nothing Then
                Dim tip = CType(_lastTTObj.ToolTip, RsrcTip)
                tip.IsOpen = False
                _lastTTObj.ToolTip = Nothing
                _lastTTObj = Nothing
                tip.Dispose()
            End If
        End Sub
        Private Shared _LastFileSaveAsFolder As String
        Private Sub AddDupeContextMenuItemAndEventHandlers(ByVal br As Browse)
            br._BrowseList.ContextMenu.AddMnuItem(
                "Dump resrc binary bytes to _output file",
                "Save As to a file",
                Sub(sender As Object, e As RoutedEventArgs)
                    Dim mnItem = CType(sender, MenuItem)
                    Dim dctxt = br._BrowseList.SelectedItem
                    Dim rdata = CType(
                                    ComponentModel.
                                    TypeDescriptor.
                                    GetProperties(
                                        dctxt
                                      )("_ResourceData").
                                      GetValue(dctxt), 
                        ResourceData)

                    Dim defaultext = ".bin"
                    Select Case rdata.ResType
                        Case "RT_ICON"
                            defaultext = ".ico"
                        Case "RT_BITMAP"
                            defaultext = ".bmp"
                        Case "RT_CURSOR"
                            defaultext = ".cur"
                        Case "RT_STRING"
                            defaultext = ".txt"
                    End Select
                    Dim saveFileDialog = New Windows.Forms.SaveFileDialog
                    saveFileDialog.DefaultExt = defaultext
                    saveFileDialog.FileName = rdata.ResName
                    saveFileDialog.Title = "Save resource to "
                    If String.IsNullOrEmpty(_LastFileSaveAsFolder) Then
                        _LastFileSaveAsFolder = IO.Path.GetTempPath
                    End If
                    saveFileDialog.InitialDirectory = _LastFileSaveAsFolder
                    If saveFileDialog.ShowDialog = Forms.DialogResult.OK Then
                        Dim filename = saveFileDialog.FileName
                        _LastFileSaveAsFolder = IO.Path.GetDirectoryName(filename)
                        Dim strm = saveFileDialog.OpenFile
                        Dim arrData(rdata.ResSize - 1) As Byte
                        If ResourceData.OpenResource(rdata) <> IntPtr.Zero Then
                            Marshal.Copy(rdata._intptrResource, arrData, 0, rdata.ResSize)
                            strm.Write(arrData, 0, rdata.ResSize)
                            strm.Close()
                            ResourceData.CloseResource(rdata)
                            MessageBox.Show("Saved to file " + filename)
                            Process.Start(filename)
                        Else
                            MessageBox.Show("Resource didn't open ")
                        End If
                    End If
                End Sub,
                InsertPos:=0
                )

            br._BrowseList.ContextMenu.AddMnuItem(
                    "Show _Dupes of (multi-)selected Images",
                    "Multiselect some images and show dupes",
                    Sub() ShowDupeSubset(br),
                    InsertPos:=0
                )
            Dim style = br._BrowseList.ItemContainerStyle
            Dim ResNameMouseHandler = New MouseEventHandler(AddressOf MouseEventHdlr)


            style.Setters.Add(New EventSetter With {
                                .Event = ListViewItem.MouseMoveEvent,
                                .Handler = ResNameMouseHandler
                             }
                    )
            style.Setters.Add(New EventSetter With {
                                .Event = ListViewItem.MouseLeaveEvent,
                                .Handler = ResNameMouseHandler
                             }
                    )

            style.Setters.Add(New EventSetter With {
                                .Event = ListViewItem.MouseDoubleClickEvent,
                                .Handler = New MouseButtonEventHandler(Sub(sender As Object, e As RoutedEventArgs)
                                                                           ResNameMouseHandler(sender, CType(e, MouseEventArgs))
                                                                       End Sub)
                             }
                    )
        End Sub

        Friend Sub MouseEventHdlr(ByVal sender As Object, ByVal e As System.Windows.Input.MouseEventArgs)
            Try
                Dim lvItem = CType(sender, ListViewItem)
                Dim tb = TryCast(e.OriginalSource, TextBlock)
                If e.RoutedEvent Is ListViewItem.MouseLeaveEvent Then
                    ClearPriorTipIfany()
                    Return
                End If
                If tb IsNot Nothing Then
                    If tb.Name = "ResName" OrElse tb.Name = "ResType" Then
                        ClearPriorTipIfany()
                        Dim rdata = CType(
                                        ComponentModel.
                                        TypeDescriptor.
                                        GetProperties(
                                            lvItem.DataContext
                                          )("_ResourceData").
                                          GetValue(lvItem.DataContext), 
                            ResourceData)

                        If e.RoutedEvent Is ListViewItem.MouseDoubleClickEvent Then
                            Dim strAddrDump = ResourceData.GetMemoryDumpOfResourceAsString(rdata)
                            WriteOutputToTempFile(lvItem.ToString + vbCrLf + strAddrDump)
                            Return
                        End If

                        Dim ttipObj = New RsrcTip(rdata, tb)
                        'ToolTipService.SetShowDuration(tb, 4000)
                        tb.ToolTip = ttipObj
                        ttipObj.IsOpen = True
                        _lastTTObj = tb

                    End If

                End If
            Catch ex As Exception
                CommonUI.MemSpectExceptionHandler(ex)
            End Try

        End Sub

        Friend Sub ShowDupeSubset(ByVal br As Browse)
            If br._BrowseList.SelectedItems.Count > 0 Then
                Dim dictFilterImgs = New SortedDictionary(Of String, Imagedata) ' fullpath

                For Each itm In br._BrowseList.SelectedItems
                    Dim imgd = CType(ComponentModel.TypeDescriptor.GetProperties(itm)("_HeapAllocationContainer").GetValue(itm), Imagedata)
                    Dim dictKey = imgd.FullPathFileName.ToLower
                    If Not dictFilterImgs.ContainsKey(dictKey) Then
                        dictFilterImgs(dictKey) = imgd
                    End If
                Next
                Dim tiptxt = String.Join(vbCrLf, dictFilterImgs.Values)
                Dim dict = New SortedDictionary(Of UInteger, List(Of ResourceData))
                Dim lstImageSymbolNames As New SortedList(Of String, List(Of ImageSymboldata))

                For Each img In dictFilterImgs.Values
                    Images.ImageResourceClass.CreateImageResources(img, dict, lstImageSymbolNames)
                Next
                GetDupeImageResources(dict, fIsSubSnap:=True, tiptxtAdditional:=tiptxt)
            Else
                GetDupeImageResources(_vm._imageAnalyzer._ImageResourcesDict, fIsSubSnap:=True)
            End If
        End Sub

        Friend Sub GetDupeImageResources(
                                             ByVal imgRsrcCRCDict As SortedDictionary(Of UInteger, List(Of ResourceData)),
                                             ByVal fIsSubSnap As Boolean,
                                             Optional ByVal tiptxtAdditional As String = ""
                                             )
            Dim tiptext = "duplicate embedded Image resources such as icons, bitmaps, strings"
            If tiptxtAdditional <> "" Then
                tiptext = tiptext + vbCrLf + tiptxtAdditional
            End If

            Dim qr = From dictEntryListOfLists In imgRsrcCRCDict
                     Where dictEntryListOfLists.Value.Count > 1
                     Select
                         dictEntryListOfLists.Value,
                         dictEntryListOfLists.Key

            'now query list of lists
            Dim qdupes = From dictEntry In qr
                         Select
                            dictEntry.Key,
                            dictEntry.Value.Count,
                            _ResList = dictEntry.Value
                            Order By _ResList(0).ResSize Descending

            Dim DupeResList As New List(Of DupeResData)
            Dim dupNumber = 1
            Dim duperesSize As Long = 0

            For Each dupe In qdupes
                '                        _VBAssert.OutputText(String.Format("Dupe Cnt = {0}, Key = {1:x8}", dupe.Count, dupe.Key), fAddToBaseline:=False)
                Dim dupNdx = 1
                For Each rsrcData In From dup In dupe._ResList
                                     Order By dup.ToString

                    DupeResList.Add(New DupeResData With {
                                    .CRC = dupe.Key,
                                    .ResData = rsrcData,
                                    .DupeIndex = dupNdx,
                                    .DupeID = dupNumber
                                }
                            )
                    duperesSize += rsrcData.ResSize
                    dupNdx += 1

                Next
                dupNumber += 1
            Next
            tiptext = String.Format("Sum(ResSize column)= {0:n0}", duperesSize) +
                vbCrLf + tiptext

            Dim qDupeRes = From resdupes In DupeResList
                           Let imgd = resdupes.ResData.ImgData
                           Let resD = resdupes.ResData
                           Select
                           CRC = resdupes.CRC.ToString("x8"),
                           resdupes.DupeID,
                           resdupes.DupeIndex,
                           HGlobal = resD.HGlobal.ToInt32.ToString("x8"),
                           imgd.Filename,
                           resD.ResSize,
                           resD.ResName,
                           resD.ResType,
                           resD.langId,
                           ImageType = imgd.ImageType.ToString,
                           imgd.FilePath,
                           _HeapAllocationContainer = imgd,
                           _ResourceData = resD

            Dim colWidths = {70, 70, 60, 80, 300, 90, 300, 170, 70, 90, 470, 70, 70, 70, 70, 70, 70, 70, 70, 70}
            Dim columnTips = {"Cyclic Redundancy check used to find dupes",
                              "DupeId like 1,2,3: one for each distinct duplicate",
                              "DupeIndex, like 1,2,3: one for each dupe within a DupeId",
                              "HGlobal: the returned result of FindResource (native only)",
                              "FileName",
                              "Resource Size in bytes",
                              "Resource Name. Hover to get memory dump in tooltip. (FoldedDupe) means the dupe was mapped to a duplicate saving memory (link option /CVTRes:FoldDups) for native only"
                             }

            Dim browDupeRes = New Browse(
                              qDupeRes,
                              fAllowBrowFilter:=True,
                              colWidths:=colWidths,
                              ColTips:=ColumnTips,
                              InitialSortOrder:=New BrowseInitialSortOrder() With {
                                  .ColumnNo = 5,
                                  .direction = ComponentModel.ListSortDirection.Descending
                              },
                              arrColumnsToTotal:={"ResSize"}
                          )
            browDupeRes.SetDynamicBackground(New DupeBackGroundConverter, fAdditive:=False)

            _TabControl.AddTabItem("DupeResources" + If(fIsSubSnap, "SubSnap", ""),
                                   tiptext,
                                   Sub(sender As Object, e As RoutedEventArgs)
                                       Dim tabRsrcDupe = CType(sender, TabItem)
                                       tabRsrcDupe.Content = browDupeRes
                                       AddDupeContextMenuItemAndEventHandlers(browDupeRes)

                                   End Sub
            )
        End Sub

        Friend Structure DupeResData
            Friend DupeID As Integer ' ID # of dupe group. Ensure matches iwth DupeContainer
            Friend DupeIndex As Integer
            Friend CRC As UInteger
            Friend ResData As ResourceData
        End Structure

        Friend Class RsrcTip
            Inherits ToolTip
            Implements IDisposable
            Private _rsrcData As ResourceData
            Sub New(ByVal rsrcData As ResourceData, ByVal tb As TextBlock)
                _rsrcData = rsrcData
                Dim strAddrDump = ResourceData.GetMemoryDumpOfResourceAsString(rsrcData, fLeaveOpened:=True)

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
                If "RT_ICON  RT_BITMAP  RT_CURSOR".Contains(rsrcData.ResType) Then
                    'Dim hwndHost = MyHwndHost.CreateUIElem(rsrcData)
                    'sp.Children.Add(hwndHost)
                End If
                sp.Children.Add(tbxDump)

                Me.PlacementTarget = tb
                Me.Placement = Controls.Primitives.PlacementMode.Bottom
                Me.Content = sp
            End Sub


#Region "IDisposable Support"
            Private disposedValue As Boolean ' To detect redundant calls

            ' IDisposable
            Protected Overridable Sub Dispose(ByVal disposing As Boolean)
                If Not Me.disposedValue Then
                    If disposing Then
                        ResourceData.CloseResource(_rsrcData)
                        ' TODO: dispose managed state (managed objects).
                    End If

                    ' TODO: free unmanaged resources (unmanaged objects) and override Finalize() below.
                    ' TODO: set large fields to null.
                End If
                Me.disposedValue = True
            End Sub

            ' TODO: override Finalize() only if Dispose(ByVal disposing As Boolean) above has code to free unmanaged resources.
            'Protected Overrides Sub Finalize()
            '    ' Do not change this code.  Put cleanup code in Dispose(ByVal disposing As Boolean) above.
            '    Dispose(False)
            '    MyBase.Finalize()
            'End Sub

            ' This code added by Visual Basic to correctly implement the disposable pattern.
            Public Sub Dispose() Implements IDisposable.Dispose
                ' Do not change this code.  Put cleanup code in Dispose(ByVal disposing As Boolean) above.
                Dispose(True)
                GC.SuppressFinalize(Me)
            End Sub
#End Region

        End Class

    End Class


    'http://blogs.msdn.com/b/calvin_hsia/archive/2008/11/28/9155196.aspx


    'Class Window1
    '    Inherits Window
    '    Sub Load() Handles MyBase.Loaded

    '        Me.Width = 600
    '        Me.Height = 600
    '        Dim popupNoTransparency = CreatePopup(False, "NoTransP", "AliceBlue", 0)
    '        Dim popupAllowTransparency = CreatePopup(True, "AllowTransp", "AliceBlue", 300)


    '        Dim popupTransparencyWithTransBack = CreatePopup(True, "popupTransparencyWithTransBack", "Transparent", 600)

    '        Me.Content = MyHwndHost.CreateUIElem("Main Window", "Bisque")


    '    End Sub
    '    Function CreatePopup(ByVal AllowTransparency As Boolean, ByVal Name As String, ByVal BackGround As String, ByVal Horiz As Integer) As Primitives.Popup
    '        Dim popup = New Primitives.Popup
    '        popup.AllowsTransparency = AllowTransparency
    '        popup.Child = MyHwndHost.CreateUIElem(Name, BackGround)
    '        popup.Placement = Primitives.PlacementMode.AbsolutePoint
    '        popup.HorizontalOffset = Horiz
    '        popup.IsOpen = True
    '        Return popup
    '    End Function
    'End Class

    Public Class MyHwndHost
        Inherits System.Windows.Interop.HwndHost

        Public Shared Function CreateUIElem(ByVal rsrcData As ResourceData) As UIElement
            Dim xaml = _
            <StackPanel
                xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
                xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
                Name="HwndTest" Width="350" Height="450" Orientation="Vertical">
                <UserControl Name="MyBorder"></UserControl>
                <Button>Bottom Button</Button>
                <TextBlock Name="Another">some text</TextBlock>
            </StackPanel>
            Dim UIElem = CType(System.Windows.Markup.XamlReader.Load(xaml.CreateReader), StackPanel)

            Dim Border = CType(UIElem.FindName("MyBorder"), UserControl)
            Dim oMyHwndHost = New MyHwndHost(rsrcData)
            Border.Content = oMyHwndHost

            Return UIElem
        End Function
        Private _rsrcData As ResourceData
        Private _hIcon As IntPtr
        Private Sub New(ByVal rsrcData As ResourceData)
            Debug.Assert(rsrcData.IsOpen(), "resource should be open for tip")
            _rsrcData = rsrcData
            Dim imgType = IntPtr.Zero
            Select Case rsrcData.ResType
                Case "RT_ICON"
                    imgType = New IntPtr(IMAGE_ICON)
                Case "RT_BITMAP"
                    imgType = New IntPtr(IMAGE_BITMAP)
                Case "RT_CURSOR"
                    imgType = New IntPtr(IMAGE_CURSOR)
            End Select
            _hIcon = LoadImage(IntPtr.Zero, rsrcData.lpName, CUInt(imgType.ToInt32), 0, 0, LR_DEFAULTSIZE)
            'Debug.Assert(_hIcon <> IntPtr.Zero, "didn't load image")
        End Sub

        Protected Overrides Function BuildWindowCore(ByVal hwndParent As System.Runtime.InteropServices.HandleRef) As System.Runtime.InteropServices.HandleRef
            Dim hwndMain As IntPtr = CreateWindowEx(0, "static", "", WindowStyles.WS_CHILD Or WindowStyles.WS_HSCROLL Or WindowStyles.WS_VSCROLL, _
                                         0, 0, 200, 300, hwndParent.Handle, IntPtr.Zero, IntPtr.Zero, IntPtr.Zero)
            Return New HandleRef(Me, hwndMain)
        End Function

        Protected Overrides Sub DestroyWindowCore(ByVal hwnd As System.Runtime.InteropServices.HandleRef)
            DestroyWindow(hwnd.Handle)
        End Sub
        Protected Overrides Function WndProc(ByVal hwnd As System.IntPtr, ByVal msg As Integer, ByVal wParam As System.IntPtr, ByVal lParam As System.IntPtr, ByRef handled As Boolean) As System.IntPtr
            Select Case msg
                Case WM_.WM_PAINT
                    Dim ps As New PAINTSTRUCT
                    BeginPaint(hwnd, ps)
                    Dim hDC = ps.hdc

                    Dim r = New RECT
                    GetClientRect(hwnd, r)
                    r.Right = CInt(r.Right / 2) ' create a rect of left half

                    Dim hbrGreen = CreateSolidBrush(CType(&HFF00, IntPtr))
                    Dim hbrBlue = CreateSolidBrush(CType(&HFF0000, IntPtr))
                    FillRect(hDC, r, hbrGreen)
                    r.Left += r.Right - r.Left
                    r.Right *= 2    ' create a rect of right half
                    FillRect(hDC, r, hbrBlue)
                    If _hIcon <> IntPtr.Zero Then
                        DrawIconEx(hDC, 0, 0, _hIcon, 0, 0, 0, IntPtr.Zero, DI_DEFAULTSIZE)
                    End If

                    EndPaint(hwnd, ps)
                    DeleteObject(hbrGreen)
                    DeleteObject(hbrBlue)
                    handled = True
                    Return IntPtr.Zero
            End Select
            Return MyBase.WndProc(hwnd, msg, wParam, lParam, handled)
        End Function
#Region "WIN32 Defs"
        Public Enum COLOR
            COLOR_SCROLLBAR = 0
            COLOR_BACKGROUND = 1
            COLOR_DESKTOP = 1
            COLOR_ACTIVECAPTION = 2
            COLOR_INACTIVECAPTION = 3
            COLOR_MENU = 4
            COLOR_WINDOW = 5
            COLOR_WINDOWFRAME = 6
            COLOR_MENUTEXT = 7
            COLOR_WINDOWTEXT = 8
            COLOR_CAPTIONTEXT = 9
            COLOR_ACTIVEBORDER = 10
            COLOR_INACTIVEBORDER = 11
            COLOR_APPWORKSPACE = 12
            COLOR_HIGHLIGHT = 13
            COLOR_HIGHLIGHTTEXT = 14
            COLOR_BTNFACE = 15
            COLOR_3DFACE = 15
            COLOR_BTNSHADOW = 16
            COLOR_3DSHADOW = 16
            COLOR_GRAYTEXT = 17
            COLOR_BTNTEXT = 18
            COLOR_INACTIVECAPTIONTEXT = 19
            COLOR_BTNHIGHLIGHT = 20
            COLOR_3DHIGHLIGHT = 20
            COLOR_3DHILIGHT = 20
            COLOR_BTNHILIGHT = 20
            COLOR_3DDKSHADOW = 21
            COLOR_3DLIGHT = 22
            COLOR_INFOTEXT = 23
            COLOR_INFOBK = 24
        End Enum
        <StructLayout(LayoutKind.Sequential)> _
        Public Structure RECT
            Public Left As Integer
            Public Top As Integer
            Public Right As Integer
            Public Bottom As Integer

            Public Function ToRect() As System.Windows.Rect
                Return New System.Windows.Rect(Left, Top, Right - Left, Bottom - Top)
            End Function
        End Structure

        <DllImport("user32.dll", CharSet:=CharSet.Auto)> _
        Private Shared Function GetClientRect(ByVal hWnd As System.IntPtr, _
       ByRef lpRECT As RECT) As Integer
        End Function



        Enum WM_
            WM_PAINT = &HF
        End Enum
        <Flags()> _
        Enum WindowStyles
            WS_OVERLAPPED = &H0
            WS_POPUP = &H80000000
            WS_CHILD = &H40000000
            WS_MINIMIZE = &H20000000
            WS_VISIBLE = &H10000000
            WS_DISABLED = &H8000000
            WS_CLIPSIBLINGS = &H4000000
            WS_CLIPCHILDREN = &H2000000
            WS_MAXIMIZE = &H1000000
            WS_BORDER = &H800000
            WS_DLGFRAME = &H400000
            WS_VSCROLL = &H200000
            WS_HSCROLL = &H100000
            WS_SYSMENU = &H80000
            WS_THICKFRAME = &H40000
            WS_GROUP = &H20000
            WS_TABSTOP = &H10000
            WS_MINIMIZEBOX = &H20000
            WS_MAXIMIZEBOX = &H10000
            WS_CAPTION = WS_BORDER Or WS_DLGFRAME
            WS_TILED = WS_OVERLAPPED
            WS_ICONIC = WS_MINIMIZE
            WS_SIZEBOX = WS_THICKFRAME
            WS_TILEDWINDOW = WS_OVERLAPPEDWINDOW
            WS_OVERLAPPEDWINDOW = WS_OVERLAPPED Or WS_CAPTION Or WS_SYSMENU Or WS_THICKFRAME Or WS_MINIMIZEBOX Or WS_MAXIMIZEBOX
            WS_POPUPWINDOW = WS_POPUP Or WS_BORDER Or WS_SYSMENU
            WS_CHILDWINDOW = WS_CHILD
        End Enum
        <DllImport("user32.dll", SetLastError:=True, CharSet:=CharSet.Auto)> _
        Private Shared Function ShowWindow(ByVal hwnd As IntPtr, ByVal nCmdShow As Int32) As Boolean
        End Function
        <DllImport("user32.dll")> _
        Public Shared Function UpdateWindow( _
     ByVal hWnd As IntPtr) As <MarshalAs(UnmanagedType.Bool)> Boolean
        End Function
        <DllImport("user32.dll", CharSet:=CharSet.Auto)> _
        Private Shared Function CreateWindowEx( _
         ByVal dwExStyle As UInteger, _
         ByVal lpClassName As String, _
         ByVal lpWindowName As String, _
         ByVal dwStyle As WindowStyles, _
         ByVal x As Integer, _
         ByVal y As Integer, _
         ByVal nWidth As Integer, _
         ByVal nHeight As Integer, _
         ByVal hWndParent As IntPtr, _
         ByVal hMenut As IntPtr, _
         ByVal hInstancet As IntPtr, _
         ByVal lpParamt As IntPtr) As IntPtr
        End Function
#If 0 Then
hwndMain = CreateWindowEx( 
    0,                      // no extended styles           
    "MainWClass",           // class name                   
    "Main Window",          // window name                  
    WS_OVERLAPPEDWINDOW |   // overlapped window            
             WS_HSCROLL |   // horizontal scroll bar        
             WS_VSCROLL,    // vertical scroll bar          
    CW_USEDEFAULT,          // default horizontal position  
    CW_USEDEFAULT,          // default vertical position    
    CW_USEDEFAULT,          // default width                
    CW_USEDEFAULT,          // default height               
    (HWND) NULL,            // no parent or owner window    
    (HMENU) NULL,           // class menu used              
    hinstance,              // instance handle              
    NULL);                  // no window creation data      

#End If
        <DllImport("user32.dll", SetLastError:=True, CharSet:=CharSet.Auto)> _
        Private Shared Function DestroyWindow(ByVal hwnd As IntPtr) As Boolean
        End Function

        Const CW_USEDEFAULT As Int32 = &H80000000

        Enum Show_Window
            SW_HIDE = 0
            SW_SHOWNORMAL = 1
            SW_NORMAL = 1
            SW_SHOWMINIMIZED = 2
            SW_SHOWMAXIMIZED = 3
            SW_MAXIMIZE = 3
            SW_SHOWNOACTIVATE = 4
            SW_SHOW = 5
            SW_MINIMIZE = 6
            SW_SHOWMINNOACTIVE = 7
            SW_SHOWNA = 8
            SW_RESTORE = 9
            SW_SHOWDEFAULT = 10
            SW_FORCEMINIMIZE = 11
            SW_MAX = 11
        End Enum
        <StructLayout(LayoutKind.Sequential, Pack:=4)> _
        Public Structure PAINTSTRUCT
            Public hdc As IntPtr
            Public fErase As Integer
            Public rcPaint As RECT
            Public fRestore As Integer
            Public fIncUpdate As Integer
            <MarshalAs(UnmanagedType.ByValArray, SizeConst:=32)> _
            Public rgbReserved As Byte()
        End Structure
        <DllImport("user32.dll")> _
        Public Shared Function BeginPaint( _
     ByVal hWnd As IntPtr, ByRef lpPaint As PAINTSTRUCT) As IntPtr
        End Function
        <DllImport("user32.dll")> _
        Public Shared Function EndPaint( _
     ByVal hWnd As IntPtr, ByRef lpPaint As PAINTSTRUCT) As IntPtr
        End Function
        <DllImport("user32.dll")> _
        Public Shared Function FillRect(ByVal hDC As IntPtr, ByRef lpRect As RECT, ByVal hBR As IntPtr) As IntPtr
        End Function
        <DllImport("gdi32.dll")> _
        Public Shared Function CreateSolidBrush(ByVal crColor As IntPtr) As IntPtr
        End Function

        <DllImport("gdi32.dll")> _
        Public Shared Function DeleteObject(ByVal hObject As IntPtr) As IntPtr
        End Function

        Public Declare Function DrawIconEx Lib "user32" (
                                                        ByVal hdc As IntPtr,
                                                        ByVal xLeft As Integer,
                                                        ByVal yTop As Integer,
                                                        ByVal hIcon As IntPtr,
                                                        ByVal cxWidth As Integer,
                                                        ByVal cyHeight As Integer,
                                                        ByVal istepIfAniCur As Integer,
                                                        ByVal hbrFlickerFreeDraw As IntPtr,
                                                        ByVal diFlags As Integer) As Boolean


        <DllImport("user32.dll", SetLastError:=True)> _
        Private Shared Function LoadImage(
                                        ByVal hInst As IntPtr,
                                        ByVal lpszName As IntPtr,
                                        ByVal uType As UInt32,
                                        ByVal cxDesired As Integer,
                                        ByVal cyDesired As Integer,
                                        ByVal fuLoad As UInt32) As IntPtr
        End Function

#End Region

    End Class





End Namespace
