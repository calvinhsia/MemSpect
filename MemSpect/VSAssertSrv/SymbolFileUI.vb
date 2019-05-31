Imports System.Runtime.InteropServices


Namespace MemSpect
    'Symbol Paths:  http://msdn.microsoft.com/en-us/library/ms680689%28v=VS.85%29.aspx
    Public Class SymbolFileUI
        Inherits SymbolFiles


        Public _TabCtrlSymbols As MyTabControl


        Friend _chkIncludeAllModules As CheckBox
        Friend _btnRefreshsym As Button

        Public Shared Function CreateSymbolFileDataSurface() As DataSurface
            VBDiagMarginBase.FreezeTargetThreads() ' so new modules aren't loaded behind us
            Dim symFiles = New SymbolFileUI(fIncludeAllModules:=False)
            Dim ctrls = symFiles.ShowSymbolFiledata
            ctrls.SurfaceHeader.Tag = symFiles ' pass to test code
            Return ctrls
        End Function


        Private Sub New(ByVal fIncludeAllModules As Boolean)
            MyBase.New(fIncludeAllModules)
        End Sub


        Private Function ShowSymbolFileDataHelper(ByVal ctrls As DataSurface, ByVal fIncludeAllModules As Boolean) As DataSurface

            Dim spH = New StackPanel With {.Orientation = Orientation.Horizontal}

            spH.Children.Add(New TextBlock With
                                             {
                                                 .Text = String.Format("Symbol files loaded at SeqNo = {0} {1}", GetGlobalPassCount, DateTime.Now)
                                             })


            Dim qsumry = From entry In _SymFileData.Values
                    Group By entry.ModInfo.SymType Into Count()
                    Select
                        SymType = SymType.ToString,
                        Count
                    Order By Count

            Dim brsumry = New Browse(qsumry)

            spH.Children.Add(brsumry)

            Dim spBtns = New StackPanel With {.Orientation = Orientation.Vertical}
            spH.Children.Add(spBtns)

            _chkIncludeAllModules = New CheckBox With {
                .Content = "Include _all loaded modules",
                .ToolTip = "will enumerate loaded modules and load debug info (can be a Huge amount of memory!).  (Native only)" + vbCrLf +
                    "MemSpect doesn't load PDBs for managed code, so " + vbCrLf +
                    " Else uses just symbols resolved so far",
                .IsChecked = fIncludeAllModules
                }

            spBtns.Children.Add(_chkIncludeAllModules)

            _btnRefreshsym = New Button With {
                             .Content = "_Refresh"
                         }
            AddHandler _btnRefreshsym.Click, Sub()
                                                 ctrls.SurfaceHeader.Children.Clear()
                                                 ctrls.SurfaceDetails.Children.Clear()
                                                 _SymFileData.Clear()
                                                 fIncludeAllModules = _chkIncludeAllModules.IsChecked.GetValueOrDefault
                                                 LoadSymInfo(fIncludeAllModules)
                                                 ShowSymbolFileDataHelper(ctrls, fIncludeAllModules)
                                             End Sub
            spBtns.Children.Add(_btnRefreshsym)

            Dim txtLookupSymbol As New TextBox With {
                .Margin = New Thickness(20, 0, 0, 0),
                .Text = String.Empty,
                .ToolTip = "like '*!*reg*' for any module containing a symbol like *reg*. Case Sensitive. '*!*' does all modules. See SymEnumSymbols in MSDN for more samples"
                }

            spBtns.Children.Add(txtLookupSymbol)
            Dim btnLookupSymbol As New Button With {
                .Margin = New Thickness(20, 0, 0, 0),
                .Content = "LookupSymbol"
                }

            spBtns.Children.Add(btnLookupSymbol)
            AddHandler btnLookupSymbol.Click, Sub()
                                                  Dim txtSym = txtLookupSymbol.Text.Trim
                                                  If Not String.IsNullOrEmpty(txtLookupSymbol.Text.Trim) Then
                                                      UpdateStatusMsg("SymbolLookup " + txtSym)
                                                      Dim symenum = New SymbolEnumerator(_hProcessTarget, strMask:=txtSym)
                                                      '                Dim modName = symFiles._SymFileData(itm.Value(0).syminfo.ModBase).ModInfo.ModuleName


                                                      Dim qsym = From itm In symenum._symlist
                                                                 Let ModBase = itm.Value(0).syminfo.ModBase
                                                                 Let ModName = CStr(IIf(_SymFileData.ContainsKey(ModBase), _SymFileData(ModBase).ModInfo.ModuleName, "ModNotFound"))
                                                                 Select
                                                                 ModName,
                                                                 Addr = itm.Key.ToString("x8"),
                                                                 Cnt = itm.Value.Count,
                                                                 Size = itm.Value(0).syminfo.Size,
                                                                 SymTag = itm.Value(0).syminfo.Tag.ToString,
                                                                 Scope = itm.Value(0).syminfo.Scope,
                                                                 Register = itm.Value(0).syminfo.Register,
                                                                 Flags = itm.Value(0).syminfo.Flags.ToString,
                                                                 Data = ReadProcessMemoryDWORDEx(itm.Key.LongToIntPtr).ToString("x8"),
                                                                 SymName = itm.Value(0).symName

                                                      Dim txtDesc = "Loaded symbol file data for '" + txtSym + "'"
                                                      Dim ctrlsSym = DataWindowMain.MakeNewDatasurface("SymLookup", txtDesc, nMaxHeaderHeight:=85)
                                                      ctrlsSym.SurfaceHeader.Children.Add(New TextBlock() With {.Text = txtDesc})
                                                      Dim brsym = New Browse(qsym,
                                                                             fAllowBrowFilter:=True,
                                                                             ColWidths:={300, 80, 80, 80, 80, 80, 80, 80, 80, 990}
                                                                             )

                                                      ctrlsSym.SurfaceDetails.Children.Add(brsym)


                                                  End If
                                              End Sub

            Dim btnDupeSymbols As New Button With {
                .Margin = New Thickness(20, 0, 0, 0),
                .Content = "_Duplicate Symbols"
                }

            AddHandler btnDupeSymbols.Click, Sub()
                                                 Dim symenum = New SymbolEnumerator(_hProcessTarget, strMask:="*!*")
                                                 'Dim dupes = From itm In symenum._symlist.GroupBy(Function(a) a.Value(0).symName).Where(Function(g) g.Count > 1).Select(Function(g) g.Key)
                                                 Dim dupes = From itm In symenum._symlist
                                                             Group By itm.Value(0).symName Into Count()
                                                             Where Count > 1
                                                             Select symName, Count

                                                 Dim txtDesc = "Duplicate symbols"
                                                 Dim ctrlsSym = DataWindowMain.MakeNewDatasurface("SymDupes", txtDesc, nMaxHeaderHeight:=85)
                                                 ctrlsSym.SurfaceHeader.Children.Add(New TextBlock() With {.Text = txtDesc})
                                                 Dim brsym = New Browse(dupes,
                                                                        fAllowBrowFilter:=True
                                                                        )

                                                 ctrlsSym.SurfaceDetails.Children.Add(brsym)




                                             End Sub
            spBtns.Children.Add(btnDupeSymbols)

            ctrls.SurfaceHeader.Children.Add(spH)


#If False Then
typedef struct _IMAGEHLP_MODULE64 {
  DWORD    SizeOfStruct;
  DWORD64  BaseOfImage;
  DWORD    ImageSize;
  DWORD    TimeDateStamp;
  DWORD    CheckSum;
  DWORD    NumSyms;
  SYM_TYPE SymType;
  TCHAR    ModuleName[32];
  TCHAR    ImageName[256];
  TCHAR    LoadedImageName[256];
  TCHAR    LoadedPdbName[256];
  DWORD    CVSig;
  TCHAR    CVData[MAX_PATH*3];
  DWORD    PdbSig;
  GUID     PdbSig70;
  DWORD    PdbAge;
  BOOL     PdbUnmatched;
  BOOL     DbgUnmatched;
  BOOL     LineNumbers;
  BOOL     GlobalSymbols;
  BOOL     TypeInfo;
  BOOL     SourceIndexed;
  BOOL     Publics;
} IMAGEHLP_MODULE64, *PIMAGEHLP_MODULE64;

#End If
            _TabCtrlSymbols = New MyTabControl
            _TabCtrlSymbols.AddTabItem("SymFiles",
                                       "Symbol files loaded",
                                       Sub(sender As Object, e As RoutedEventArgs)
                                           Dim thetabItem = CType(sender, TabItem)

                                           Dim q = From itm In _SymFileData
                                                   Let imgdat = itm.Value.ModInfo
                                                   Select
                                                    ModName = imgdat.ModuleName,
                                                        BaseOfDll = CInt(itm.Value.BaseOfDll).ToString("x8"),
                                                        SymType = imgdat.SymType.ToString,
                                                        imgdat.LoadedPDBName,
                                                        imgdat.ImageSize,
                                                        imgdat.TimeDateStamp,
                                                        imgdat.CheckSum,
                                                        imgdat.NumSyms,
                                                        imgdat.LoadedImageName,
                                                        imgdat.CVSig,
                                                        imgdat.CVData,
                                                        imgdat.PdbSig,
                                                        Guid = imgdat.PdbSig70.ToString,
                                                        imgdat.PdbAge,
                                                        imgdat.PdbUnmatched,
                                                        imgdat.DbgUnmatched,
                                                        imgdat.LineNumbers,
                                                        imgdat.GlobalSymbols,
                                                        imgdat.TypeInfo,
                                                        imgdat.SourceIndexed,
                                                        imgdat.Publics,
                                                        _imgDat = imgdat


                                           Dim br = New Browse(q, fAllowBrowFilter:=True)
                                           thetabItem.Content = br
                                           br._BrowseList.
                                               ContextMenu.
                                               AddMnuItem(
                                                   "Show All _Symbols for Module",
                                                   "Show symbols from PDB for this module",
                                                   Sub()
                                                       Dim items = br._BrowseList.SelectedItems
                                                       If items Is Nothing OrElse items.Count < 1 Then
                                                           items = br._BrowseList.Items
                                                       End If
                                                       If items IsNot Nothing AndAlso items.Count = 1 Then
                                                           Dim item = items(0)
                                                           Dim tdesc = ComponentModel.TypeDescriptor.GetProperties(item)("ModName")
                                                           Dim modname = CStr(tdesc.GetValue(item))

                                                           Dim symenum = New SymbolEnumerator(_hProcessTarget, strMask:=modname + "!*")

                                                           Dim qsym = From itm In symenum._symlist
                                                                      Select
                                                                      Addr = itm.Key.ToString("x8"),
                                                                      Cnt = itm.Value.Count,
                                                                      Size = itm.Value(0).syminfo.Size,
                                                                      SymTag = itm.Value(0).syminfo.Tag.ToString,
                                                                      Scope = itm.Value(0).syminfo.Scope,
                                                                      Register = itm.Value(0).syminfo.Register,
                                                                      Flags = itm.Value(0).syminfo.Flags.ToString,
                                                                      Data = ReadProcessMemoryDWORDEx(itm.Key.LongToIntPtr).ToString("x8"),
                                                                      SymName = itm.Value(0).symName

                                                           Dim txtDesc = "Loaded symbol file data for '" + modname + "'"
                                                           Dim ctrlsSym = DataWindowMain.MakeNewDatasurface("SymFile", txtDesc, nMaxHeaderHeight:=85)
                                                           ctrlsSym.SurfaceHeader.Children.Add(New TextBlock() With {.Text = txtDesc})
                                                           Dim brsym = New Browse(qsym,
                                                                                  fAllowBrowFilter:=True,
                                                                                  ColWidths:={80, 80, 80, 80, 80, 80, 80, 80, 990}
                                                                                  )

                                                           ctrlsSym.SurfaceDetails.Children.Add(brsym)

                                                       End If



                                                   End Sub)
                                       End Sub)

            '_TabCtrlSymbols.AddTabItem("All Symbols",
            '                           "All symbols loaded",
            '                           Sub(sender As Object, e As RoutedEventArgs)
            '                               Dim thetabItem = CType(sender, TabItem)
            '                               Dim symenum = New SymbolEnumerator(_hProcessSymbolFiles)
            '                               Dim q = From a In symenum._symlist
            '                                       Let firstsyminfo = a.Value(0)
            '                                       Select
            '                                       Address = a.Key.ToString("x8"),
            '                                       NumSymsAtSameAddr = a.Value.Count,
            '                                       ModuleName = If(_SymFileData.ContainsKey(firstsyminfo.syminfo.ModBase),
            '                                                     _SymFileData(firstsyminfo.syminfo.ModBase).ModInfo.ModuleName, "not loaded"),
            '                                       Tag = firstsyminfo.syminfo.Tag.ToString,
            '                                       Flags = firstsyminfo.syminfo.Flags.ToString(),
            '                                       firstsyminfo.symName

            '                               Dim brsyms = New Browse(q,
            '                                                       fAllowBrowFilter:=True,
            '                                                       ColWidths:={WIDTH_ADDRESS, 60, 300, 80, 90, 400},
            '                                                       ColTips:={"Address of symbol",
            '                                                                 "Some addresses have more than 1 symbol"}
            '                                                       )

            '                               thetabItem.Content = brsyms


            '                           End Sub)


            ctrls.SurfaceDetails.Children.Add(_TabCtrlSymbols)


            Return ctrls
        End Function

        Public Function ShowSymbolFiledata() As DataSurface
            Dim ctrls = DataWindowMain.MakeNewDatasurface("SymFiles", "Loaded symbol file data", nMaxHeaderHeight:=85)
            ShowSymbolFileDataHelper(ctrls, fIncludeAllModules:=False)
            Return ctrls

        End Function


    End Class


End Namespace