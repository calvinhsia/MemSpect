Imports System.Runtime.InteropServices


Namespace MemSpect
    'Symbol Paths:  http://msdn.microsoft.com/en-us/library/ms680689%28v=VS.85%29.aspx
    Public Class SymbolFiles
        Implements IDisposable

        Public Class SymbolFileData
            Public BaseOfDll As Int64
            Public ModInfo As IMAGEHLP_MODULE64
        End Class
        Public _SymFileData As New Dictionary(Of Int64, SymbolFileData) ' module baseaddr

        Friend _SymFilesToUnload As New List(Of Int64)

        Protected Friend _hProcessSymbolFiles As IntPtr '' the hProcess we're using for sym res

        Public Shared Function CheckSymbolFileStatus(Optional ByVal fCheckSymCache As Boolean = False) As String
            Dim res = String.Empty
            Dim symbolpath = String.Empty
            ' check symbolpath for writable cache
            Dim sbtemp = New Text.StringBuilder(2000)

            'GetPrivateProfileString(ProfileStringSection, "SymbolsPath", "", sbTemp, sbTemp.Capacity, _iniFileName)
            'If sbTemp.Length > 0 Then
            '    Dim sympath = Path.GetDirectoryName(Reflection.Assembly.GetExecutingAssembly.Location)
            '    sympath += "\;" + sbtemp.ToString()
            '    VSSetSymSearchPath(_hProcessTarget, sympath)
            '    '                            SymSetSearchPath(_hProcessTarget, sympath)
            'End If
            If _ConnectionMode <> MemSpectMode.Offline Then
                If SymGetSearchPath(_hProcessTarget, sbtemp, sbtemp.Capacity) Then
                    symbolpath = sbtemp.ToString
                    If fCheckSymCache Then
                        'SRV*c:\symcache*\\ddrps\symbols*http://symweb
                        'D:\MemSpect\;SRV*c:\symcache*\\ddrps\symbols*http://symweb
                        Dim ndxstar = symbolpath.IndexOf("*")
                        Dim cache = String.Empty
                        If ndxstar > 0 Then
                            Try
                                Dim ndxEndStar = symbolpath.IndexOf("*", ndxstar + 1)
                                cache = symbolpath.Substring(ndxstar + 1, ndxEndStar - ndxstar - 1)
                                If Not String.IsNullOrEmpty(cache) Then
                                    Dim testfile = IO.Path.Combine(cache, "t.txt")
                                    IO.File.WriteAllText(testfile, "testing read/write of cache location")
                                    IO.File.Delete(testfile)
                                End If

                            Catch ex As Exception
                                UpdateStatusMsg("Error writing symbol cache " + cache + " " + ex.Message)
                            End Try

                        End If

                    End If
                    'UpdateStatusMsg("SymOpt=" + SymGetOptions.ToString())
                Else
                    UpdateStatusMsg("error getting SymPath hr =" + Marshal.GetLastWin32Error().ToString)
                End If

            End If
            If _ConnectionMode = MemSpectMode.OnLine Then


                Dim symFiles = New SymbolFiles(fIncludeAllModules:=False)

                If symFiles._SymFileData.Count > 0 Then
                    Dim numPDBs = (From ent In symFiles._SymFileData.Values
                                  Where ent.ModInfo.SymType = SymType.SymPdb).Count

                    Dim pctPDB = 100 * numPDBs / symFiles._SymFileData.Count
                    If pctPDB < 40 Then
                        Dim q = From ent In symFiles._SymFileData.Values
                                Group By ent.ModInfo.SymType Into Count()
                                Select SymType, Count


                        For Each itm In q
                            res += String.Format(" {0} {1}", itm.SymType, itm.Count)
                        Next
                        UpdateStatusMsg(String.Format("SymLoadSmry: Warning: {0} PDB files, {1} non-pdb files loaded", numPDBs, symFiles._SymFileData.Count - numPDBs))
                        UpdateStatusMsg(String.Format("SymLoadSmry: {0}", res))

                        UpdateStatusMsg("SymLoadSmry: check SymbolPath in " + MemSpectIniName + " or SET _NT_SYMBOL_PATH")
                        UpdateStatusMsg("SymOptions: " + SymGetOptions.ToString())

                    End If

                Else
                    UpdateStatusMsg("SymLoadSmry: no symbols loaded: check SymbolPath in " + MemSpectIniName + " or SET _NT_SYMBOL_PATH")
                End If
            End If
            Return symbolpath
        End Function

        Public Sub New(ByVal fIncludeAllModules As Boolean)
            _hProcessSymbolFiles = _hProcessTarget
            LoadSymInfo(fIncludeAllModules)
        End Sub

        Protected Friend Sub LoadSymInfo(ByVal fIncludeAllModules As Boolean)
            If fIncludeAllModules Then
                Dim lastfilenameBase = IntPtr.Zero ' multiple MBIs for a single file
                For Each mbi In GetVirtAllocs().Values

                    If lastfilenameBase <> mbi.AllocationBase Then
                        lastfilenameBase = mbi.AllocationBase
                        Dim FullFilename = GetFileNameFromMBI(mbi)
                        If Not String.IsNullOrEmpty(FullFilename) AndAlso
                                Not FullFilename.StartsWith("GetLastError") Then

                            Dim resSymLoadModule = SymLoadModuleEx(_hProcessSymbolFiles,
                                           hFile:=IntPtr.Zero,
                                           ImageName:=FullFilename,
                                           ModuleName:="",
                                           BaseOfDll:=CLng(mbi.AllocationBase),
                                           DllSize:=0,
                                           Data:=IntPtr.Zero,
                                           Flags:=0
                                           )

                            If resSymLoadModule > 0 Then ' success
                                UpdateStatusMsg("Loaded symbol info for " + FullFilename)
                                _SymFilesToUnload.Add(resSymLoadModule)
                            Else
                                Dim err = Marshal.GetLastWin32Error
                                If err = 0 Then ' ERROR_SUCCESS: already loaded
                                    UpdateStatusMsg("symbol info already loaded for " + FullFilename)
                                Else
                                    UpdateStatusMsg("Error finding symbol info for " + FullFilename)
                                End If

                            End If
                        End If

                    End If

                Next
            End If
            Dim res = SymEnumerateModules64(_hProcessSymbolFiles, AddressOf SymEnumerateModulesProc64, IntPtr.Zero)

        End Sub

        Public Function SymEnumerateModulesProc64(
            ByVal ModuleName As String,
            ByVal BaseOfDll As Int64,
            ByVal UserContext As IntPtr
          ) As Boolean
            '          _VBAssert.OutputText((String.Format("  SymEnumerateModulesProc64 {0} {1:x8}", ModuleName, BaseOfDll)))
            Dim moduleinfo As New IMAGEHLP_MODULE64

            moduleinfo.SizeOfStruct = Marshal.SizeOf(GetType(IMAGEHLP_MODULE64))

            If SymGetModuleInfo64(_hProcessSymbolFiles, BaseOfDll, moduleinfo) Then
                _SymFileData.Add(BaseOfDll, New SymbolFileData With {
                                 .BaseOfDll = BaseOfDll,
                                 .ModInfo = moduleinfo
                                 })
                '                _VBAssert.OutputText(String.Format("SymGetModuleInfo64 {0} {1} {2}", moduleinfo.ImageName, moduleinfo.LoadedImageName, moduleinfo.LoadedPDBName))
            Else

                Dim lerr = Marshal.GetLastWin32Error
                _SymFileData.Add(BaseOfDll, New SymbolFileData With {
                                 .BaseOfDll = BaseOfDll,
                                 .ModInfo = New IMAGEHLP_MODULE64 With {
                                     .LoadedPDBName = String.Format("Error getting sym info {0:x8} {1}", lerr, GetErrorMessageFromWin32LastError(lerr))
                                     }
                                 })
                '               _VBAssert.OutputText(String.Format("SymGetModuleInfo64 failled err= {0:x8}", lerr))
            End If

            Return True
        End Function



#Region "IDisposable Support"
        Private disposedValue As Boolean ' To detect redundant calls

        ' IDisposable
        Protected Overridable Sub Dispose(ByVal disposing As Boolean)
            If Not Me.disposedValue Then
                If disposing Then
                    ' TODO: dispose managed state (managed objects).
                End If

                ' TODO: free unmanaged resources (unmanaged objects) and override Finalize() below.
                If _SymFilesToUnload.Count > 0 Then
                    UpdateStatusMsg("Dispose SymbolFile cnt=" + _SymFilesToUnload.Count.ToString, msgType:=StatusMessageType.LogEntry)
                    For Each itm In _SymFilesToUnload
                        Dim unloadres = SymUnloadModule64(_hProcessSymbolFiles, itm)
                        If unloadres = False Then
                            UpdateStatusMsg("error unloading debug info for " + itm.ToString("x8"), msgType:=StatusMessageType.LogEntry)
                        End If

                    Next

                End If
                ' TODO: set large fields to null.
            End If
            Me.disposedValue = True
        End Sub

        ' TODO: override Finalize() only if Dispose(ByVal disposing As Boolean) above has code to free unmanaged resources.
        Protected Overrides Sub Finalize()
            ' Do not change this code.  Put cleanup code in Dispose(ByVal disposing As Boolean) above.
            Dispose(False)
            MyBase.Finalize()
        End Sub

        ' This code added by Visual Basic to correctly implement the disposable pattern.
        Public Sub Dispose() Implements IDisposable.Dispose
            ' Do not change this code.  Put cleanup code in Dispose(ByVal disposing As Boolean) above.
            Dispose(True)
            GC.SuppressFinalize(Me)
        End Sub
#End Region

    End Class

    Public Class SymbolEnumerator
        Public Class SymbolInfo
            Public syminfo As SYMBOL_INFO
            Public symName As String
        End Class
        Public _symlist As New Dictionary(Of Int64, List(Of SymbolInfo))

        Public Sub New(ByVal hProc As IntPtr, Optional strMask As String = "*!*")
            SymEnumSymbols(hProc, 0, strMask, AddressOf SymEnumSymCallBack, IntPtr.Zero)
        End Sub

        Private Function SymEnumSymCallBack(ByVal psyminfo As IntPtr, ByVal symsize As Integer, ByVal usercontext As IntPtr) As Boolean
            Dim syminfo As SYMBOL_INFO
            Dim dwReadBytes = 0
            Dim curp = GetCurrentProcess()
            If ReadProcessMemorySYMBOL_INFO(curp, psyminfo, syminfo, Marshal.SizeOf(GetType(SYMBOL_INFO)), dwReadBytes) = 0 Then
                UpdateStatusMsg("error reading syminfo")
            End If
            '            Marshal.PtrToStructure(psyminfo, syminfo)
            Dim symname = Marshal.PtrToStringAnsi(psyminfo + Marshal.SizeOf(GetType(SYMBOL_INFO)) - 4)
            Dim symdata = New SymbolInfo With {.symName = symname, .syminfo = syminfo}
            Dim existinginfo As List(Of SymbolInfo) = Nothing
            If _symlist.TryGetValue(syminfo.Address, existinginfo) Then
                existinginfo.Add(symdata)
            Else
                _symlist.Add(syminfo.Address, New List(Of SymbolInfo) From {symdata})

            End If
            Return True  ' keep enumerating
        End Function

    End Class

End Namespace