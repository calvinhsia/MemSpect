Imports System.IO
Imports System.Runtime.InteropServices
Imports System.Reflection
Imports System.Text.RegularExpressions
Imports System.Text
Imports FastSerialization0

Namespace MemSpect

    'http://msdn.microsoft.com/en-us/library/3c4f1xde.aspx
    Public Class AssemblyLoadInTempAppDomain
        Inherits MarshalByRefObject
        Friend _assembly As Assembly
        Public Function LoadAssembly(ByVal bytes() As Byte, ByVal asmFullPathName As String, ByVal fUseSeparateDomain As Boolean) As String
            Dim retval = String.Empty
            Try

                If fUseSeparateDomain Then
                    _assembly = AppDomain.CurrentDomain.Load(bytes)
                Else
                    _assembly = System.Reflection.Assembly.LoadFile(asmFullPathName)
                End If

            Catch ex As Exception
                '                Debug.Assert(False, "temp domain except: " + asmFullPathName + vbCrLf + ex.Message)
                retval = ex.Message
            End Try
            Return retval
        End Function

        Public Function GetResourceNames() As String()
            Dim nms() As String
            Debug.Assert(_assembly IsNot Nothing, "temp domain asm is null?")
            nms = _assembly.GetManifestResourceNames
            Return nms
        End Function

        'caller MUST free intptr
        Public Function LoadResourceInfo(ByVal resName As String) As Tuple(Of IntPtr, Integer) ' addr, size
            Dim result As New Tuple(Of IntPtr, Integer)(IntPtr.Zero, 0)
            Debug.Assert(_assembly IsNot Nothing, "temp domain asm is null?")
            Try
                Dim rinfo = _assembly.GetManifestResourceInfo(resName)
                Dim resSize = 0
                Dim pRes As IntPtr
                '               Dim CRC As UInteger = 0
                Dim resLoc = rinfo.ResourceLocation
                If (resLoc And Reflection.ResourceLocation.Embedded) > 0 Then
                    Using strm = _assembly.GetManifestResourceStream(resName)
                        resSize = CInt(strm.Length)
                        Dim arrData(resSize - 1) As Byte
                        strm.Read(arrData, 0, resSize)

                        pRes = Marshal.AllocCoTaskMem(resSize)
                        Marshal.Copy(arrData, 0, pRes, resSize)
                        '                        CRC = CRC32(0, pRes, 0, resSize)
                        '                      Marshal.FreeCoTaskMem(pRes)
                    End Using
                    result = New Tuple(Of IntPtr, Integer)(pRes, resSize)
                End If

            Catch ex As Exception

            End Try

            Return result
        End Function

    End Class

    Public Class Images
        Private Shared _appDomainTemp As AppDomain = Nothing
        Friend Shared _appDomainLoader As AssemblyLoadInTempAppDomain
        Friend Const UseSeparateAppDomain As Boolean = False
        Public Const ManagedResourceTypeString As String = "Mgd"
        Public Const FoldedDupeString As String = " (FoldedDupe)"

        Friend Shared Function CreateAppDomainLoader() As AssemblyLoadInTempAppDomain
            Debug.Assert(_appDomainLoader Is Nothing, "_appDomainLoader  should be nothing?")
            Dim appdomainSetup = New AppDomainSetup With {
                .ApplicationBase = IO.Path.GetDirectoryName(Assembly.GetExecutingAssembly.Location)
            }
            Dim thisasm = Assembly.GetExecutingAssembly.FullName

            If UseSeparateAppDomain Then
                Debug.Assert(_appDomainTemp Is Nothing, "_appDomainTemp should be nothing?")
                _appDomainTemp = AppDomain.CreateDomain("temp", Nothing, appdomainSetup)
                _appDomainLoader = CType(_appDomainTemp.CreateInstanceAndUnwrap(
                    thisasm,
                    "MemSpect.AssemblyLoadInTempAppDomain"
                    ), AssemblyLoadInTempAppDomain)
            Else
                _appDomainTemp = Nothing
                _appDomainLoader = New AssemblyLoadInTempAppDomain

            End If

            Return _appDomainLoader
        End Function

        Friend Shared Function AppDomainLoaderClear() As Boolean
            If _appDomainLoader IsNot Nothing Then
                If UseSeparateAppDomain Then
                    AppDomain.Unload(_appDomainTemp)
                End If
                _appDomainTemp = Nothing
                _appDomainLoader = Nothing
            End If
            Return True
        End Function

        Public Class ImageSymboldata ' one per managed type
            Public imagedat As Imagedata
            Public Members As List(Of MemberInfo)
            Public Overrides Function ToString() As String
                Return imagedat.Filename
            End Function
        End Class

        ''' <summary>
        ''' Given an address of a module in memory of target process, read the PE header specified ImageBase
        ''' (which could have been modified by ASLR)
        ''' </summary>
        ''' <param name="hModule"></param>
        ''' <returns></returns>
        Public Shared Function GetDllBaseAddressFromMem(hModule As IntPtr, Optional fUseCurrentProcess As Boolean = True) As IntPtr
            Dim res = IntPtr.Zero
            If hModule <> IntPtr.Zero Then
                Try
                    If fUseCurrentProcess Then
                        Dim sig = Marshal.ReadInt16(hModule)
                        If sig = IMAGE_DOS_SIGNATURE Then
                            Dim dosHeader = CType(Marshal.PtrToStructure(hModule, GetType(_IMAGE_DOS_HEADER)), _IMAGE_DOS_HEADER)
                            Dim addrFileHeader = hModule.ToInt32 + dosHeader.e_lfanew + IntPtr.Size ' skip over IMAGE_NT_HEADERS.Signature
                            Dim fileHdr = CType(Marshal.PtrToStructure(New IntPtr(addrFileHeader), GetType(IMAGE_FILE_HEADER)), IMAGE_FILE_HEADER)
                            Dim opthdraddr = New IntPtr(addrFileHeader + Marshal.SizeOf(fileHdr))
                            Dim opthdr = CType(Marshal.PtrToStructure(opthdraddr, GetType(IMAGE_OPTIONAL_HEADER32)), IMAGE_OPTIONAL_HEADER32)
                            res = New IntPtr(opthdr.ImageBase)
                        End If
                    Else
                        If _ConnectionMode = MemSpectMode.OnLine Then
                            Dim resmsg = SendMsg(ProcMsgVerb.GetPEBaseAddressFromMem, dwords:={hModule.ToInt32})
                            res = New IntPtr(BitConverter.ToInt32(resmsg, 1))
                        End If
                        'Dim sig = ReadProcessMemoryDWORDEx(hModule) And &HFFFF
                        'If sig = IMAGE_DOS_SIGNATURE Then

                        'End If
                    End If
                Catch ex As Exception
                    UpdateStatusMsg("Exception getting dllbaseaddr from mem " + ex.Message)
                End Try
            End If
            Return res
        End Function

        Public Class Imagedata
            Inherits HeapAllocationContainer ' which inherits from SerializableObject
            Friend FullPathFileName As String
            Public ReadOnly Property Filename As String 'file.exe (no path)
                Get
                    Return IO.Path.GetFileName(FullPathFileName)
                End Get
            End Property
            Public ReadOnly Property FilePath As String ' no filename
                Get
                    Return IO.Path.GetDirectoryName(FullPathFileName)
                End Get
            End Property
            Public mbi As MEMORY_BASIC_INFORMATION
            Public callstk As String
            Public ImageBaseMem As IntPtr ' base address in memory            
            Public ImageBasePE As ULong ' base address from PE header in file
            Private _ImageBasePEMem As IntPtr?
            Public ReadOnly Property ImageBasePEMem As IntPtr ' base address from PE header in memory
                Get
                    Dim res = IntPtr.Zero
                    If _ImageBasePEMem.HasValue Then
                        res = _ImageBasePEMem.Value
                    Else
                        _ImageBasePEMem = Images.GetDllBaseAddressFromMem(ImageBaseMem, fUseCurrentProcess:=False)
                        res = _ImageBasePEMem.Value
                    End If
                    Return res
                End Get
            End Property


            Public Relocated As Integer ' >0 only if the dll contains code
            Public DynamicBase As Integer
            Public VMSize As UInteger
            Public SizeOnDisk As UInteger
            Public LrgstConsecZeros As Integer
            Public LangIDs As String ' resource lang ids
            Public rsrc As UInteger
            Public code As UInteger
            Public reloc As UInteger
            Public Instance As String ' " 1", " 2" for multi instace
            Public OptimizationInfo As String ' optimization info, like BBT Signature or (Partial) NGen
            Public OptPartialNGen As String ' like "<PartialNGen>true</PartialNGen>"
            Public data As UInteger ' (readonly) uninit
            Public Sections As String
            Public ResultIndex As Integer ' used to match batch file results. 0 indicates none
            Public ImageType As ImageTypeEnum
            Public DllImports As New SortedSet(Of String)
            Public DllExports As New SortedSet(Of String)
            Sub New()
                Sections = String.Empty
                Instance = String.Empty
                callstk = String.Empty
                ImageType = ImageTypeEnum.Native
                OptimizationInfo = "None"
            End Sub
            Friend Sub ProcessExport(ByVal strSym As String, ByVal lstImageSymbolNames As SortedList(Of String, List(Of ImageSymboldata)))
                If Not String.IsNullOrEmpty(strSym) Then
                    If _IsUnderTest Then
                        If Me.Filename.ToLower = MemSpectDllName Then
                            Return
                        End If
                    End If
                    If strSym.Contains("[NONAME]") Then
                        Return
                    End If
                    Dim match = RegularExpressions.Regex.Match(strSym, "[0-9]+")
                    Dim ord = strSym.Substring(0, match.Length)
                    strSym = strSym.Substring(match.Length)

                    match = Regex.Match(strSym, "\s+[0-9A-Fa-f]+")
                    Dim hint = strSym.Substring(0, match.Length).Trim
                    strSym = strSym.Substring(match.Length)

                    Dim rva = "" ' may not be present
                    match = Regex.Match(strSym, "\s+")
                    If match.Length > 4 Then
                        ' no rva
                    Else
                        match = Regex.Match(strSym, "\s+[0-9A-Fa-f]{8}")
                        rva = strSym.Substring(0, match.Length)
                        strSym = strSym.Substring(match.Length)
                    End If
                    Dim splitstr = strSym.Trim.Split()
                    Dim undecName = splitstr(0)
                    Dim symNameStrB = New StringBuilder(1000)
                    Dim res = UnDecorateSymbolName(splitstr(0), symNameStrB, 1000, 0)
                    If res > 0 Then
                        Dim symName = symNameStrB.ToString
                        'Debug.WriteLine(String.Format("Name = {0} ", symName))
                        'If symNameStrB.ToString <> undecName Then
                        '    Debug.WriteLine("UNDECORATED= ", undecName)
                        'End If

                        DllExports.Add(symNameStrB.ToString)
                        Dim symData As List(Of ImageSymboldata) = Nothing
                        If Not lstImageSymbolNames.TryGetValue(symName, symData) Then
                            symData = New List(Of ImageSymboldata)
                            lstImageSymbolNames.Add(symName, symData)
                        End If
                        Dim newSymImageData = New ImageSymboldata With {.imagedat = Me}
                        symData.Add(newSymImageData)
                        newSymImageData.Members = New List(Of MemberInfo)
                    Else
                        Dim lerr = Marshal.GetLastWin32Error
                        Debug.Assert(False, "can' undecorate " + lerr.ToString("x0") + " " + GetErrorMessageFromWin32LastError(lerr) + " " + strSym)

                    End If


                End If
            End Sub

            Friend Function GetSummarySects() As String
                Return String.Format("DiskSize ={0,10:n0} VMSize ={1,10:n0}  Code={2,10:n0} Data={3,10:n0} Rsrc={4,10:n0} ReLoc={5,10:n0} DynamicBase={6} WasRelocated={7}", SizeOnDisk, VMSize, code, data, rsrc, reloc, DynamicBase, Relocated)
            End Function

            Public Overrides Function ToString() As String
                Return String.Format("{0}, {1}, DiskSize={2}, Sections = {3}", Filename, FilePath, SizeOnDisk, Sections)
            End Function

            Public Overrides Sub ToStream(ByVal serializer As Serializer)
                MyBase.ToStream(serializer)
                serializer.Write(FullPathFileName)
                serializer.Write(Filename)
                serializer.Write(FilePath)
            End Sub

            Public Overrides Sub FromStream(ByVal deserializer As Deserializer, ByVal versionFromStream As Integer)
                MyBase.FromStream(deserializer, versionFromStream)
            End Sub
        End Class

        Public Enum ImageTypeEnum
            Native
            Managed
            Mixed
            Resource
        End Enum

        Public Class ResourceData
            Public HGlobal As IntPtr
            Public ResSize As Integer
            Public ResType As String
            Public lpType As IntPtr
            Public ResName As String
            Public lpName As IntPtr
            Public ImgData As Imagedata
            Public langId As UInteger
            Public _intptrResource As IntPtr
            Public Overrides Function ToString() As String
                Return String.Format(
                    "{0} Size={1} Type={2} Name={3} Lang={4} Path={5} ImgType={6}",
                    ImgData.Filename,
                    ResSize,
                    ResType,
                    ResName,
                    langId,
                    ImgData.FilePath,
                    ImgData.ImageType
                    )
            End Function

            ''' <summary>
            ''' Openers must call CloseResource explicitly
            ''' </summary>
            ''' <param name="rsrcData"></param>
            ''' <returns></returns>
            ''' <remarks></remarks>
            Public Shared Function OpenResource(ByVal rsrcData As ResourceData) As IntPtr
                If rsrcData._intptrResource = IntPtr.Zero Then  ' if already opened, just return it. Else reload
                    Try

                        If rsrcData.ResType <> ManagedResourceTypeString Then
                            ' could still be managed, with native rsrc like RT_VERSION
                            Dim hModule = LoadLibraryEx(rsrcData.ImgData.FullPathFileName, IntPtr.Zero, LOAD_LIBRARY_AS_IMAGE_RESOURCE)
                            If hModule <> IntPtr.Zero Then
                                Dim hRscInfo = FindResourceEx(hModule, rsrcData.lpType, rsrcData.lpName, CUShort(rsrcData.langId))
                                If hRscInfo = IntPtr.Zero Then
                                    Return IntPtr.Zero
                                Else

                                End If
                                Debug.Assert(CInt(SizeofResource(hModule, hRscInfo)) = rsrcData.ResSize, "resource size mismatch" + vbCrLf +
                                             SizeofResource(hModule, hRscInfo).ToString + " " + rsrcData.ResSize.ToString + " " +
                                             rsrcData.ToString)
                                Dim hg = LoadResource(hModule, hRscInfo)
                                rsrcData._intptrResource = Marshal.AllocCoTaskMem(rsrcData.ResSize) ' make copy so can freelib
                                Dim arrData(rsrcData.ResSize - 1) As Byte
                                Marshal.Copy(hg, arrData, 0, rsrcData.ResSize)
                                Marshal.Copy(arrData, 0, rsrcData._intptrResource, rsrcData.ResSize)
                                FreeLibrary(hModule)
                            End If
                        Else
                            'http://forums.asp.net/t/1016475.aspx

#If False Then
AppDomainSetup domainSetup = new AppDomainSetup (); 
ApplicationTrust domainAppTrust = new ApplicationTrust (); 

// Trust 
domainAppTrust.ApplicationIdentity = new ApplicationIdentity (AssemblyAccess.AssemblyProduct); 
domainAppTrust.IsApplicationTrustedToRun = true; 

// Configure 
domainSetup.ApplicationName = AssemblyAccess.AssemblyProduct; 
domainSetup.ApplicationBase = EffectiveProjectDirectory; 
domainSetup.PrivateBinPath = EffectiveProjectDirectory; 
domainSetup.LoaderOptimization = LoaderOptimization.MultiDomain; 
domainSetup.CachePath = Path.GetTempPath (); 
domainSetup.DisallowApplicationBaseProbing = false; 
domainSetup.DisallowBindingRedirects = false; 
domainSetup.DisallowCodeDownload = false; 
domainSetup.DisallowPublisherPolicy = false; 

try 
{ 
        // Load a new app domain for typeMgr 
        typeMgrDomain = AppDomain.CreateDomain ("TypeManager Domain", null, domainSetup); 

        // Create the TypeManager type and get the object here 
        typeMgr = (TypeManager) typeMgrDomain.CreateInstanceAndUnwrap (Assembly.GetExecutingAssembly ().FullName, "CE.JSS.Compiler.TypeManager"); 

        // Do something with typeMgr 
} 
catch 
{ 
        typeMgr = null; 
        if (typeMgrDomain != null) 
        { 
                AppDomain.Unload (typeMgrDomain); 
                typeMgrDomain = null; 
        } 
}
#End If
                            'Dim domainSetup = New AppDomainSetup
                            'Dim domainAppTrust = New Security.Policy.ApplicationTrust With {
                            '    .ApplicationIdentity = New ApplicationIdentity("memspectAsmImages"),
                            '    .IsApplicationTrustedToRun = True
                            '}

                            'With domainSetup
                            '    .ApplicationName = "memspectAsmImages"
                            '    .LoaderOptimization = LoaderOptimization.NotSpecified
                            '    .CachePath = IO.Path.GetTempPath
                            '    .DisallowApplicationBaseProbing = True
                            '    .DisallowBindingRedirects = True
                            '    .DisallowCodeDownload = True
                            '    .DisallowPublisherPolicy = True

                            'End With
                            'Dim typeMgrDomain = AppDomain.CreateDomain("TypeManager Domain", Nothing, domainSetup)
                            'Debug.Assert(_appDomainTemp Is Nothing, "")
                            '_appDomainTemp = AppDomain.CreateDomain("temp")
                            Dim bytesAsm() = File.ReadAllBytes(rsrcData.ImgData.FullPathFileName)
                            CreateAppDomainLoader()
                            If String.IsNullOrEmpty(_appDomainLoader.LoadAssembly(bytesAsm, rsrcData.ImgData.FullPathFileName, UseSeparateAppDomain)) Then
                                Dim r = _appDomainLoader.LoadResourceInfo(rsrcData.ResName)
                                rsrcData._intptrResource = r.Item1
                            End If
                            'Dim asm = _appDomainTemp.Load(bytesAsm)
                            ''Dim asm = Reflection.Assembly.LoadFile(rsrcData.ImgData.FullPathFileName)
                            'Dim resInfo = asm.GetManifestResourceInfo(rsrcData.ResName)
                            'Dim resLoc = resInfo.ResourceLocation
                            'If (resLoc And Reflection.ResourceLocation.Embedded) > 0 Then
                            '    Using strm = asm.GetManifestResourceStream(rsrcData.ResName)
                            '        Debug.Assert(CInt(strm.Length) = rsrcData.ResSize, "resource size mismatch" + vbCrLf +
                            '                     strm.Length.ToString + " " + rsrcData.ResSize.ToString + " " +
                            '                     rsrcData.ToString)
                            '        Dim arrData(rsrcData.ResSize - 1) As Byte
                            '        strm.Read(arrData, 0, rsrcData.ResSize)

                            '        rsrcData._intptrResource = Marshal.AllocCoTaskMem(rsrcData.ResSize)
                            '        Marshal.Copy(arrData, 0, rsrcData._intptrResource, rsrcData.ResSize)

                            '    End Using


                            'End If

                        End If
                    Catch ex As Exception

                    End Try
                    'If _appDomainTemp IsNot Nothing Then
                    '    AppDomain.Unload(_appDomainTemp)
                    '    _appDomainTemp = Nothing
                    'End If
                End If

                Return rsrcData._intptrResource
            End Function

            Public Shared Function CloseResource(ByVal rsrcData As ResourceData) As Boolean
                Dim fDidClose = False
                If rsrcData._intptrResource <> IntPtr.Zero Then
                    Marshal.FreeCoTaskMem(rsrcData._intptrResource)
                    rsrcData._intptrResource = IntPtr.Zero
                    AppDomainLoaderClear()
                    fDidClose = True
                End If
                Return fDidClose
            End Function

            Public Shared Function GetMemoryDumpOfResourceAsString(
                                                          ByVal rsrcData As ResourceData,
                                                          Optional ByVal fLeaveOpened As Boolean = False
                                                                                                   ) As String
                Dim res = rsrcData.ToString + vbCrLf
                Try
                    Dim crc As UInteger = 0
                    Dim resSize = rsrcData.ResSize
                    If OpenResource(rsrcData) <> IntPtr.Zero Then
                        crc = CRC32(0, rsrcData._intptrResource, 0, resSize)
                        res += "CRC=" + crc.ToString("x8")
                        res += GetMemoryDump(rsrcData._intptrResource, rsrcData.ResSize, fUseCurrentProcess:=True, nMaxDumpSize:=resSize)
                    Else
                        res += "Failed to open resource"
                    End If

                Catch ex As Exception

                End Try
                If Not fLeaveOpened Then
                    CloseResource(rsrcData)
                End If
                Return res
            End Function

            Public Function IsOpen() As Boolean
                Return _intptrResource <> IntPtr.Zero
            End Function

        End Class

        'iterate through virtualallocs to get loaded image names, then process them
        Public Class ImageAnalyzer
            Inherits SerializableObject

            Friend _ImageResourcesDict As New SortedDictionary(Of UInteger, List(Of ResourceData)) ' key=CRC
            Friend _lstImageSymbolNames As New SortedList(Of String, List(Of ImageSymboldata))
            Friend _lstImageData As New SortedList(Of String, Imagedata) ' key is JustFileName.tolower
            Friend _StacksFileName As String = String.Empty
            Friend _totSumry As String = String.Empty

            Friend Shared Function CreateImageAnalyzer() As ImageAnalyzer
                Return New ImageAnalyzer()
            End Function


            Public Overrides Sub ToStream(ByVal serializer As Serializer)
                serializer.Write(_lstImageData.Count)
                For Each itm In _lstImageData
                    serializer.Write(itm.Key)
                    serializer.Write(itm.Value)
                Next
            End Sub

            Public Overrides Sub FromStream(ByVal deserializer As Deserializer, ByVal versionFromStream As Integer)
                Dim cnt = deserializer.ReadInt()
                For i = 1 To cnt
                    _lstImageData.Add(deserializer.ReadString, CType(deserializer.ReadObject, Imagedata))
                Next
            End Sub
            Private Sub New()

                Try

                    Dim summaryImageData = New Imagedata ' we'll accumulate totals here
                    Dim ndxBatResult = 1
                    For Each mbi In GetVirtAllocs().Values
                        Dim FullFilename = GetFileNameFromMBI(mbi)
                        'For Each itm In browVM._BrowseList.Items
                        '    Dim mbi = CType(System.ComponentModel.TypeDescriptor.GetProperties(itm)("_mbi").GetValue(itm), MEMORY_BASIC_INFORMATION)

                        '    Dim FullFilename = CStr(System.ComponentModel.TypeDescriptor.GetProperties(itm)("Data").GetValue(itm))
                        If Not String.IsNullOrEmpty(FullFilename) AndAlso
                                Not FullFilename.StartsWith("GetLastError") Then

                            Dim JustFileName = IO.Path.GetFileName(FullFilename) ' no path, just file.ext
                            Dim dictKey = JustFileName.ToLower
                            Dim fullpath = IO.Path.GetDirectoryName(FullFilename) ' just path, no file
                            Dim ImageDat As Imagedata
                            Dim fIsNew = True
                            ' have we seen this file already. Is it unique by path and BaseAddr?
                            Dim appendage = String.Empty
                            Do
                                If _lstImageData.ContainsKey(dictKey + appendage) Then
                                    ImageDat = _lstImageData(dictKey + appendage)
                                    If String.Compare(ImageDat.FilePath, fullpath, ignoreCase:=True) = 0 AndAlso
                                        ImageDat.ImageBaseMem = mbi.AllocationBase Then ' Same one?
                                        _lstImageData(dictKey + appendage).VMSize += mbi.RegionSize
                                        summaryImageData.VMSize += mbi.RegionSize
                                        fIsNew = False
                                        Exit Do
                                    End If
                                    ' it's a different load of the same DLL base name: distinguish via Appendage and add new
                                    If appendage = String.Empty Then
                                        appendage = "2" ' 1st has none, 2nd has "2"
                                    Else
                                        appendage = CStr(CInt(appendage) + 1)
                                    End If
                                Else
                                    dictKey += appendage ' not there: add new one with appendage
                                    Exit Do
                                End If
                            Loop

                            If fIsNew Then
                                '                                UpdateStatusMsgDbg(_dictImage.Count.ToString + " " + JustFileName + appendage)
                                ImageDat = New Imagedata With {
                                    .FullPathFileName = FullFilename,
                                    .VMSize = mbi.RegionSize,
                                    .ImageBaseMem = mbi.AllocationBase,
                                    .mbi = mbi
                                }
                                _lstImageData(dictKey) = ImageDat
                                If IO.File.Exists(FullFilename) Then
                                    Dim finfo = New FileInfo(FullFilename)
                                    ImageDat.SizeOnDisk = CUInt(finfo.Length)
                                    ImageDat.LrgstConsecZeros = finfo.GetConsecutiveZeros

                                    If ImageDat.VMSize = 0 Then ' if we're doing a dislay of files on disk, not in mem
                                        ImageDat.VMSize = ImageDat.SizeOnDisk
                                    End If
                                    summaryImageData.SizeOnDisk += ImageDat.SizeOnDisk
                                    ImageDat.ResultIndex = ndxBatResult
                                    ndxBatResult += 1
                                End If
                                Dim hctr = New HeapAllocationContainer
                                ImageDat.callstk = VirtualMem.GetCallStackForMBI(ImageDat.mbi, hctr)
                                ImageDat.AllocationStruct = hctr.AllocationStruct
                                If hctr.AllocationStruct.Address = IntPtr.Zero Then
                                    ImageDat.AllocationStruct.Address = ImageDat.mbi.AllocationBase ' so we get mem dump tooltip
                                    ImageDat.AllocationStruct.Size = CInt(ImageDat.mbi.RegionSize)
                                End If

                                ImageDat.HeapBlockPtr = hctr.HeapBlockPtr
                                ImageDat.SpyHeapPtr = hctr.SpyHeapPtr
                                ImageDat._tblkContainer = New TrackBlockContainer(hctr.TBlk)

                                '_lstImageData(itm.Key) = ImageDat


                                ImageDat.Instance = If(String.IsNullOrEmpty(appendage), "", String.Format("{0,2}", CInt(appendage)))

                                summaryImageData.VMSize += mbi.RegionSize


                            End If

                        End If
                        '                    Dim fExitEarly = _IsUnderTest
                        '#If DEBUG Then
                        '                    fExitEarly = True
                        '#End If
                        '                    If fExitEarly Then
                        '                        If dictImage.Count > 20 Then
                        '                            Exit For
                        '                        End If
                        '                    End If
                    Next
                    Dim resTuple = ProcessImageDatList(_lstImageData, summaryImageData, _ImageResourcesDict, _lstImageSymbolNames)
                    Dim sbCallStackSmry = resTuple.Item1
                    _totSumry = String.Format(" Total Cnt = {0} # stks = {1}  {2}", _lstImageData.Count, resTuple.Item2, summaryImageData.GetSummarySects)
                    sbCallStackSmry.AppendLine(_totSumry)
                    _StacksFileName = WriteOutputToTempFile(sbCallStackSmry.ToString, fShowInNotepad:=False)

                Catch ex As Exception
                    Common.MemSpectExceptionHandler(ex)
                Finally
                    AppDomainLoaderClear()
                End Try
            End Sub

            Friend Shared Function ProcessImageDatList(
                                                 ByVal lstImageData As SortedList(Of String, Imagedata),
                                                 ByVal summaryImageData As Imagedata,
                                                 ByVal ImageResourcesDict As SortedDictionary(Of UInteger, List(Of ResourceData)),
                                                 ByVal lstImageSymbolNames As SortedList(Of String, List(Of ImageSymboldata))
                                                 ) As Tuple(Of StringBuilder, Integer)
                Dim tempImageResultsFileNameBase = IO.Path.Combine(IO.Path.GetTempPath, "ImageInfo") '"1,2,3,4... "+".txt"

                '"C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\bin\vcvars32.bat"
                Dim devenvPath = IO.Path.GetDirectoryName(ProcessLauncher.GetDevEnvFullPath)
                If String.IsNullOrEmpty(devenvPath) Then
                    Throw New InvalidOperationException("Could not find installation of Visual Studio")
                End If
                Dim vcVarsPath = IO.Path.Combine(devenvPath, "..\..\VC\Bin\VCVars32.bat")
                If Not File.Exists(vcVarsPath) Then
                    '"C:\Program Files (x86)\Microsoft Visual Studio\VS15Preview\Common7\IDE\VisualCpp\Auxiliary\Build\vcvars32.bat"
                    vcVarsPath = IO.Path.Combine(devenvPath, "VisualCpp\Auxiliary\Build\vcvars32.bat")
                End If
                If Not File.Exists(vcVarsPath) Then
                    UpdateStatusMsgDbg(vcVarsPath)
                End If

                Dim strLinkBatFileContents = New Text.StringBuilder()
                strLinkBatFileContents.AppendLine(String.Format("call ""{0}"" ", vcVarsPath)) ' "call ""C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\bin\vcvars32.bat"

                For Each itm In lstImageData
                    Dim ImageDat = itm.Value
                    Dim FullFilename = ImageDat.FullPathFileName
                    Dim ndxBatResult = ImageDat.ResultIndex
                    Dim resultFileName = tempImageResultsFileNameBase + ndxBatResult.ToString + ".txt"
                    If IO.File.Exists(resultFileName) Then
                        IO.File.Delete(resultFileName)
                    End If
                    strLinkBatFileContents.AppendLine(String.Format("link /dump /headers /clrheader /exports /imports ""{0}"" > ""{1}"" ", FullFilename, resultFileName))
                Next
                'Exec bat file which generates lots of temp files
                '1st line is call to VCVars
                ' subseq lines are like  link /dump > tempfile.txt
                Dim tempBatchFilename = IO.Path.Combine(IO.Path.GetTempPath, "GetImageInfo.bat")
                File.WriteAllText(tempBatchFilename, strLinkBatFileContents.ToString)
                StartProcess(tempBatchFilename)
                Dim resTuple = ProcessLinkBatResultFiles(tempImageResultsFileNameBase, summaryImageData, lstImageData, ImageResourcesDict, lstImageSymbolNames)
                Return resTuple
            End Function

            Private Shared Function ProcessLinkBatResultFiles(
                                  ByVal tempImageResultsFileNameBase As String,
                                  ByVal summaryImageData As Imagedata,
                                  ByVal lstImageData As SortedList(Of String, Imagedata),
                                  ByVal ImageResourcesDict As SortedDictionary(Of UInteger, List(Of ResourceData)),
                                  ByVal lstImageSymbolNames As SortedList(Of String, List(Of ImageSymboldata))
                                  ) As Tuple(Of StringBuilder, Integer)

                Dim sbCallStackSmry = New Text.StringBuilder("Call Stacks for all images sorted by image name" + vbCrLf)
                ' now process the output of the bat file to determine if image is managed, native, etc.
                For Each ImageDat In lstImageData.Values
                    If ImageDat.ResultIndex > 0 Then
                        Dim resultFileName = tempImageResultsFileNameBase + ImageDat.ResultIndex.ToString + ".txt"
                        ParseLinkOutput(resultFileName, ImageDat, summaryImageData, lstImageSymbolNames)
                    End If

                    If ImageDat.ImageType = ImageTypeEnum.Native AndAlso ImageDat.rsrc > 0 AndAlso
                            ImageDat.code = 0 AndAlso ImageDat.data = 0 AndAlso ImageDat.reloc = 0 Then
                        ImageDat.ImageType = ImageTypeEnum.Resource
                    End If

                Next ' each image
                Try
                    CollectManagedOptimizationInfo(lstImageData)
                Catch ex As Exception
                    UpdateStatusMsg(ex.ToString)
                End Try

                ' now accum call stacks and analyze image resources
                Dim nStks = 0
                'Debug.Assert(_appDomainLoader Is Nothing, "_appDomainLoader  should be nothing")
                If _appDomainLoader IsNot Nothing Then
                    AppDomainLoaderClear()
                End If
                CreateAppDomainLoader()
                For Each itm In lstImageData
                    ' if the actual load addr (ImageBaseMem) and the BaseAddr as spec'd in the file (ImageBasePE) are diff
                    ' or if it diffs with the NTHeader value of the loaded DLL in memory (ImageBasePEMem)
                    If itm.Value.ImageBaseMem.ToInt64 <> itm.Value.ImageBasePE OrElse
                        itm.Value.ImageBaseMem.ToInt64 <> itm.Value.ImageBasePEMem.ToInt64 Then
                        If itm.Value.code > 0 Then ' we only care about relocations if there is code
                            itm.Value.Relocated += 1
                            summaryImageData.Relocated += 1
                        End If
                    End If
                    If String.IsNullOrEmpty(itm.Value.Instance) Then
                        Dim imrsclass = ImageResourceClass.CreateImageResources(itm.Value, ImageResourcesDict, lstImageSymbolNames)
                    End If

                    Dim dat = itm.Value
                    sbCallStackSmry.AppendLine(String.Format("{0,-25} {1}", dat.Filename, dat.GetSummarySects))
                    sbCallStackSmry.AppendLine(String.Format("    ImageBaseMem={0:x8} ImagebaseDisk={1:x8}  FullName={2} Sections = {3}",
                                                dat.ImageBaseMem.ToInt32,
                                                dat.ImageBasePE,
                                                dat.FullPathFileName,
                                                dat.Sections))

                    For Each imp In dat.DllImports
                        sbCallStackSmry.AppendLine("      Imports: " + imp)
                    Next

                    For Each exp In dat.DllExports
                        sbCallStackSmry.AppendLine("      Exports: " + exp)
                    Next

                    If dat.callstk.Length > 0 Then
                        nStks += 1
                        For Each stackFrameLine In dat.callstk.Split(CChar(vbCr))
                            sbCallStackSmry.AppendLine("        " + stackFrameLine.Replace(CChar(vbLf), ""))
                        Next
                    End If
                    sbCallStackSmry.AppendLine()
                    sbCallStackSmry.AppendLine()
                Next
                Return New Tuple(Of StringBuilder, Integer)(sbCallStackSmry, nStks)
            End Function

            Private Shared Sub CollectManagedOptimizationInfo(ByVal lstImageData As SortedList(Of String, Imagedata))
                Dim strIBCMergeBatFileContents = New Text.StringBuilder()
                Dim sbIBCMergeBatfileContents As New StringBuilder
                Dim ndxBatResult = 1
                Dim tempImageResultsFileNameBase = IO.Path.Combine(IO.Path.GetTempPath, "IBCMergeInfo") '"1,2,3,4... "+".txt"
                Dim msFolder = IO.Path.GetDirectoryName(Reflection.Assembly.GetExecutingAssembly.Location)

                For Each itm In From img In lstImageData.Values
                                Where img.ResultIndex > 0
                                Where img.ImageType = ImageTypeEnum.Managed OrElse img.ImageType = ImageTypeEnum.Mixed

                    Dim resultFileName = tempImageResultsFileNameBase + ndxBatResult.ToString + ".txt"
                    If IO.File.Exists(resultFileName) Then
                        IO.File.Delete(resultFileName)
                    End If
                    Dim tmpXMLfilename = IO.Path.ChangeExtension(resultFileName, "xml")
                    sbIBCMergeBatfileContents.AppendLine(String.Format("""{0}\ibcmerge.exe"" -v3 -f -dxml {1} -mi ""{2}"" > ""{3}"" ", msFolder, tmpXMLfilename, itm.FullPathFileName, resultFileName))
                    sbIBCMergeBatfileContents.AppendLine(String.Format("type {0} | find ""PartialNGen"" >> ""{1}"" ", tmpXMLfilename, resultFileName))
                    ndxBatResult += 1
                Next
                If sbIBCMergeBatfileContents.Length > 0 Then
                    Dim tempBatchFilename = IO.Path.Combine(IO.Path.GetTempPath, "RunIBCMerge.bat")
                    File.WriteAllText(tempBatchFilename, sbIBCMergeBatfileContents.ToString)
                    StartProcess(tempBatchFilename)
                End If

                Dim offsetFilename = "Reading IBC resource from file: ".Length
                For i = 1 To ndxBatResult - 1
                    Dim resultFileName = tempImageResultsFileNameBase + i.ToString + ".txt"
                    Dim lineno = 1
                    Dim filename = String.Empty
                    Dim fIsOpt = False
                    For Each strLine In File.ReadAllLines(resultFileName)
                        If lineno = 1 Then
                            'Reading IBC resource from file: C:\Windows\assembly\NativeImages_v4.0.30319_32\mscorlib\74353039393f68f4c068cc37f759e5be\mscorlib.ni.dll

                            Dim fullFilename = strLine.Substring(offsetFilename)
                            filename = Path.GetFileName(fullFilename).ToLower
                            If Not lstImageData.ContainsKey(filename) Then
                                Debug.Assert(False, "lstimage data doesn't contain file " + filename + " " + strLine)
                                Exit For
                            End If
                            Dim optinfo = String.Empty
                            If lstImageData(filename).OptimizationInfo = "None" Then
                                optinfo = "IBC "
                            Else
                                optinfo = "IBCDupe "
                            End If
                            lstImageData(filename).OptimizationInfo = optinfo
                        ElseIf lineno = 2 Then
                            If strLine.StartsWith("Error: Can't find an IBC resource") Then
                                If filename.EndsWith(".ni.dll") Then
                                    lstImageData(filename).OptimizationInfo = "No IBC resource for .ni.dll"
                                Else
                                    lstImageData(filename).OptimizationInfo = "No IBC resource"
                                End If
                            Else
                                If strLine.Contains("PE Resource size") Then
                                    lstImageData(filename).OptimizationInfo += strLine
                                    fIsOpt = True
                                End If
                            End If
                        Else
                            If fIsOpt Then
                                If Not String.IsNullOrWhiteSpace(strLine) Then
                                    If Not strLine.StartsWith("Xml output file:") Then
                                        lstImageData(filename).OptimizationInfo += " " + strLine
                                        If strLine.Contains("PartialNGen") Then
                                            Dim strxml = "<PartialNGen>true</PartialNGen>"
                                            Dim trueOrFalse = String.Empty
                                            Using textRdr = New System.IO.StringReader(strxml)
                                                Dim xr = Xml.XmlNodeReader.Create(textRdr)
                                                While xr.Read
                                                    trueOrFalse = xr.ReadElementContentAsString '
                                                End While
                                            End Using
                                            lstImageData(filename).OptPartialNGen = trueOrFalse
                                        End If
                                    End If
                                End If
                            End If
                        End If
                        lineno += 1
                    Next

                Next
                'Reading IBC resource from file: C:\Windows\assembly\NativeImages_v4.0.30319_32\mscorlib\74353039393f68f4c068cc37f759e5be\mscorlib.ni.dll
                'Error: Can't find an IBC resource in C:\Windows\assembly\NativeImages_v4.0.30319_32\mscorlib\74353039393f68f4c068cc37f759e5be\mscorlib.ni.dll, Error = 1813

            End Sub

            Private Shared Sub ParseLinkOutput(
                                              ByVal resultFileName As String,
                                              ByVal ImageDat As Imagedata,
                                              ByVal summaryImageData As Imagedata,
                                              ByVal lstImageSymbolNames As SortedList(Of String, List(Of ImageSymboldata))
                                              )
                Dim results = IO.File.ReadAllText(resultFileName)
                Dim fInOptionalHeader = False
                Dim fInClrHeader = False
                Dim fInDebugDirectories = False
                Dim finImports = False
                Dim finExports = False
                Dim fGotOrdinal = False
                Dim fInSummary = False
                Dim nEndCurrentSectionIndentLevel = -1 ' once the indent gets back to this, then we've ended the section. -1 indicates no current section
                For Each line In IO.File.ReadAllLines(resultFileName)
                    Dim nCurrentIndentLevel = Aggregate ch In line Take While ch = " " Into Count()

                    If line.Trim.Length > 0 AndAlso nEndCurrentSectionIndentLevel >= nCurrentIndentLevel Then 'end of current section
                        fInOptionalHeader = False
                        fInClrHeader = False
                        finImports = False
                        fInDebugDirectories = False
                        finExports = False
                        fInSummary = False
                        fGotOrdinal = False
                        nEndCurrentSectionIndentLevel = -1 ' no section
                    Else
                        If fInOptionalHeader Then
                            If line.Contains("image base") Then
                                Dim trynum = ULong.Parse(line.Trim.Split(CChar(" "))(0), Globalization.NumberStyles.AllowHexSpecifier)
                                'Dump of file C:\Windows\System32\apphelp.dll
                                '     7FF738B0000 image base (000007FF738B0000 to 000007FF73906FFF)
                                ImageDat.ImageBasePE = trynum
                            ElseIf line.Contains("Dynamic base") Then
                                ImageDat.DynamicBase = 1
                                summaryImageData.DynamicBase += 1
                            End If
                        ElseIf fInClrHeader Then
                            If line.Contains("Native Entry Point") Then
                                'C:\Windows\Microsoft.Net\assembly\GAC_32\Microsoft.VisualStudio.FileDiscovery\v4.0_10.0.0.0__b03f5f7f11d50a3a\Microsoft.VisualStudio.FileDiscovery.dll
                                ImageDat.ImageType = ImageTypeEnum.Mixed
                            End If
                        ElseIf fInDebugDirectories Then
                            'Trace.WriteLine("Looking for BBT Signature...");
                            '// We are looking for a line that looks like:
                            '// 449AFD82 (   A)        4 0073CA04   73BE04    BB030EA6
                            Dim regex = New Regex("[0-9A-F]+ +\(   A\) +[0-9A-F]+ +[0-9A-F]+ +[0-9A-F]+ +([0-9A-F]{4})")
                            Dim match = regex.Match(line)
                            If match.Success Then
                                Dim signature = match.Groups(1).Value
                                If (signature = "BB01" OrElse signature = "BB02" OrElse signature = "BB03" OrElse signature = "B100") Then
                                    ImageDat.OptimizationInfo = signature
                                End If
                            End If

                        ElseIf finImports Then
                            If nCurrentIndentLevel = 4 Then ' dlls start 
                                ImageDat.DllImports.Add(line.Trim())
                            End If
                        ElseIf finExports Then
                            If Not fGotOrdinal Then
                                If line.Contains("ordinal hint RVA      name") Then
                                    fGotOrdinal = True
                                End If
                            Else
                                If fGotOrdinal Then
                                    ImageDat.ProcessExport(line.Trim, lstImageSymbolNames)
                                End If
                            End If
                        ElseIf fInSummary Then
                            If line.Trim.Length > 0 Then

                                Dim lineParts = line.Trim.Split(CChar(" "))

                                Dim sectSize = UInteger.Parse(lineParts(0), Globalization.NumberStyles.AllowHexSpecifier)
                                Dim sectType = lineParts(1).Trim.ToLower
                                ' http://kishorekumar.net/pecoff_v8.1.htm

                                ' http://msdn.microsoft.com/en-us/library/ms809762.aspx Peering Inside the PE: A Tour of the Win32 Portable Executable File Format

                                Select Case sectType
                                    Case ".text" ' code
                                        ImageDat.code += sectSize
                                        summaryImageData.code += sectSize
                                    Case ".bss", ".rdata", ".data", ".sdata", ".srdata", ".sxdata", ".sbss" ' (readonly) initialized
                                        ImageDat.data += sectSize
                                        summaryImageData.data += sectSize
                                    Case ".bss" ' uninit data
                                        ImageDat.data += sectSize
                                        summaryImageData.data += sectSize
                                    Case ".rsrc" ' resources
                                        ImageDat.rsrc += sectSize
                                        summaryImageData.rsrc += sectSize
                                    Case ".reloc" ' relocation
                                        ImageDat.reloc += sectSize
                                        summaryImageData.reloc += sectSize
                                End Select

                                ImageDat.Sections += String.Format("   {0}={1:n0}", sectType, sectSize)
                            End If
                        End If
                    End If
                    ' not in any section: see if we're starting one
                    If line.Trim.Length > 0 Then
                        If line.StartsWith("OPTIONAL HEADER VALUES") Then
                            fInOptionalHeader = True
                            nEndCurrentSectionIndentLevel = 0

                        ElseIf line.Contains("  clr Header:") Then
                            fInClrHeader = True
                            ImageDat.ImageType = ImageTypeEnum.Managed
                            nEndCurrentSectionIndentLevel = 2 ' when we get back to 3, it's end of this part
                        ElseIf line.StartsWith("  Debug Directories") Then
                            fInDebugDirectories = True
                            nEndCurrentSectionIndentLevel = 2 ' when we get back to 3, it's end of this part
                        ElseIf line.StartsWith("  Section contains the following imports:") Then
                            finImports = True
                            nEndCurrentSectionIndentLevel = 2 ' when we get back to 3, it's end of this part
                        ElseIf line.StartsWith("  Section contains the following exports") Then
                            finExports = True
                            nEndCurrentSectionIndentLevel = 2 ' when we get back to 3, it's end of this part
                        ElseIf line.StartsWith("  Summary") Then
                            fInSummary = True
                            nEndCurrentSectionIndentLevel = 2
                        End If
                    End If
                Next ' line in dumpbin
            End Sub


        End Class

        Friend Class ImageResourceClass


            Friend Shared Function CreateImageResources(ByVal ImageDat As Imagedata,
                                                   ByVal imgRsrcCRCDict As SortedDictionary(Of UInteger, List(Of ResourceData)),
                                                   ByVal lstImageSymbolNames As SortedList(Of String, List(Of ImageSymboldata))
                                                   ) As ImageResourceClass
                Dim strLangIds = String.Empty
                Dim result As ImageResourceClass = Nothing
                Try
                    result = New ImageResourceClass(ImageDat, imgRsrcCRCDict, lstImageSymbolNames)
                    strLangIds = String.Join(",", result._langIdList)
                Catch ex As Exception
                    UpdateStatusMsg(ex.Message)
                End Try
                ImageDat.LangIDs = strLangIds
                Return result
            End Function

            Private _langIdList As New List(Of Integer)
            Private _CurrentResImageData As Imagedata ' so callbacks can access
            Private _imgRsrcCRCDict As SortedDictionary(Of UInteger, List(Of ResourceData)) ' can be null. key = CRC

            Private Sub New(
                           ByVal ImageDat As Imagedata,
                           ByVal imgRsrcCRCDict As SortedDictionary(Of UInteger, List(Of ResourceData)),
                           ByVal lstImageSymbolNames As SortedList(Of String, List(Of ImageSymboldata))
                           )
                _imgRsrcCRCDict = imgRsrcCRCDict
                Dim hModule As IntPtr
                Try
                    _CurrentResImageData = ImageDat
                    hModule = LoadLibraryEx(ImageDat.FullPathFileName, IntPtr.Zero, LOAD_LIBRARY_AS_IMAGE_RESOURCE)
                    If hModule <> IntPtr.Zero Then
                        EnumResourceTypesEx(hModule, AddressOf EnumResourceTypeCallback, IntPtr.Zero, RESOURCE_ENUM_LN, 0) ' don't search MUI: only LanguageNeutral Only
                    End If
                    If ImageDat.ImageType = ImageTypeEnum.Managed OrElse
                        ImageDat.ImageType = ImageTypeEnum.Mixed Then

                        Dim bytesAsm() = File.ReadAllBytes(ImageDat.FullPathFileName)
                        If _appDomainLoader Is Nothing Then
                            CreateAppDomainLoader()
                        End If
                        If String.IsNullOrEmpty(_appDomainLoader.LoadAssembly(bytesAsm, ImageDat.FullPathFileName, UseSeparateAppDomain)) Then
                            If String.IsNullOrEmpty(ImageDat.Instance) Then ' don't do multiply loaded instances
                                Dim types As Type() = Nothing
                                Try
                                    types = _appDomainLoader._assembly.GetExportedTypes
                                Catch ex As Exception
                                    UpdateStatusMsg("Can't get exported types for " + ImageDat.FullPathFileName + vbCrLf +
                                                    ex.ToString
                                                    )

                                End Try
                                If types IsNot Nothing Then
                                    For Each typ In types
                                        Dim symName = typ.FullName
                                        Dim symData As List(Of ImageSymboldata) = Nothing
                                        If Not lstImageSymbolNames.TryGetValue(symName, symData) Then
                                            symData = New List(Of ImageSymboldata)
                                            lstImageSymbolNames.Add(symName, symData)
                                        End If
                                        Dim newSymImageData = New ImageSymboldata With {.imagedat = ImageDat}
                                        symData.Add(newSymImageData)
                                        newSymImageData.Members = typ.GetMembers.ToList

                                    Next

                                End If

                            End If

                            Dim resrcNames = _appDomainLoader.GetResourceNames

                            ''Dim asm = System.Reflection.Assembly.LoadFile(ImageDat.FullPathFileName)
                            'Dim asm = _appDomainTemp.Load(bytesAsm)

                            'Dim nms = asm.GetManifestResourceNames
                            '                        Debug.WriteLine(ImageDat.Filename + " " + nms.Count.ToString)
                            For Each nm In resrcNames

                                Dim resstuff = _appDomainLoader.LoadResourceInfo(nm)
                                Dim resAddr = resstuff.Item1
                                Dim resSize = resstuff.Item2
                                Dim crc As UInteger = 0

                                If resAddr <> IntPtr.Zero Then
                                    crc = CRC32(0, resAddr, 0, resSize)
                                    Marshal.FreeCoTaskMem(resAddr)
                                    ' hGlobal is set to 0: for managed resource, it's actually a locally allocated buffer
                                    Dim rdata = New ResourceData With {
                                        .HGlobal = IntPtr.Zero,
                                        .ResName = nm,
                                        .ResSize = resSize,
                                        .ResType = ManagedResourceTypeString,
                                        .langId = 0,
                                        .ImgData = _CurrentResImageData
                                    }
                                    ProcessResData(crc, rdata) ' managed

                                Else
                                    ' Debug.Assert(False, "got null resource " + nm + " " + ImageDat.Filename)
                                End If
                                'Dim resInfo = asm.GetManifestResourceInfo(nm)
                                'Dim resLoc = resInfo.ResourceLocation
                                'Dim resSize = 0
                                'If (resLoc And Reflection.ResourceLocation.Embedded) > 0 Then
                                '    Using strm = asm.GetManifestResourceStream(nm)
                                '        resSize = CInt(strm.Length)
                                '        Dim arrData(resSize - 1) As Byte
                                '        strm.Read(arrData, 0, resSize)

                                '        Dim pRes = Marshal.AllocCoTaskMem(resSize)
                                '        Marshal.Copy(arrData, 0, pRes, resSize)
                                '        CRC = CRC32(0, pRes, 0, resSize)
                                '        Marshal.FreeCoTaskMem(pRes)
                                '    End Using
                                '    Dim rdata = New ResourceData With {
                                '        .ResName = nm,
                                '        .ResSize = resSize,
                                '        .ResType = "Mgd",
                                '        .langId = 0,
                                '        .ImgData = _CurrentResImageData
                                '    }
                                '    ProcessResData(CRC, rdata) ' managed
                                'End If
                            Next

                        End If
                    End If
                Catch ex As Exception
                    Common.MemSpectExceptionHandler(ex)
                Finally
                    If hModule <> IntPtr.Zero Then
                        FreeLibrary(hModule)
                    End If
                    'If _appDomainTemp IsNot Nothing Then
                    '    AppDomain.Unload(_appDomainTemp)
                    '    _appDomainTemp = Nothing
                    'End If
                End Try
            End Sub


            <Conditional("DEBUG")>
            Sub WriteStatus(ByVal msg As String)
                Debug.WriteLine(msg)
            End Sub

            Function EnumResourceTypeCallback(ByVal hModule As IntPtr, ByVal lpType As IntPtr, ByVal lParam As IntPtr) As Boolean
#If DEBUGxx Then
                Dim strRestType = ResToString(lpType, fIsResType:=True)
                WriteStatus("Res type= " + strRestType)
#End If
                EnumResourceNames(hModule, lpType, AddressOf EnumResourceNameCallback, lParam)
                Return True
            End Function

            Function EnumResourceNameCallback(ByVal hModule As IntPtr, ByVal lpType As IntPtr, ByVal lpName As IntPtr, ByVal lParam As IntPtr) As Boolean
#If DEBUGxx Then
                Dim resType = "Res Type = " + lpType.ToInt32.ToString("x8") + " "
                resType += ResToString(lpName)
                WriteStatus(resType + " Res Name = " + lpName.ToInt32.ToString("x8"))
#End If
                EnumResourceLanguages(hModule, lpType, lpName, AddressOf EnumResourceLanguagesCallBack, lParam)
                Return True
            End Function

            Private Sub ProcessResData(ByVal CRC As UInteger, ByVal rData As ResourceData)
                If _imgRsrcCRCDict IsNot Nothing Then
                    Dim lstRes As List(Of ResourceData) = Nothing

                    If _imgRsrcCRCDict.TryGetValue(CRC, lstRes) Then
#If DEBUG Then
                        For Each rd In lstRes
                            If rd.langId = rData.langId AndAlso
                                rd.ResName = rData.ResName AndAlso
                                rd.ResType = rData.ResType AndAlso
                                rd.ImgData.Filename.ToLower = rData.ImgData.Filename.ToLower Then
                                Debug.WriteLine("Image has same signature? " + rd.ToString + " " + rData.ToString)
                                Debug.Assert(False, "Image has same signature? " + rd.ToString + " " + rData.ToString)
                            End If

                        Next
#End If
                        ' now see if this is a folded dupe via hGlobal
                        For Each res In lstRes
                            If res.HGlobal <> IntPtr.Zero AndAlso rData.HGlobal = res.HGlobal Then ' not managed
                                If Not rData.ResName.EndsWith(FoldedDupeString) Then
                                    rData.ResName += FoldedDupeString
                                End If
                            End If
                        Next
                        lstRes.Add(rData)
                    Else
                        _imgRsrcCRCDict(CRC) = New List(Of ResourceData) From {rData}
                    End If

                End If
            End Sub

            Function EnumResourceLanguagesCallBack(ByVal hModule As IntPtr, ByVal lpType As IntPtr, ByVal lpName As IntPtr, ByVal wLangId As UInt16, ByVal lParam As IntPtr) As Boolean
                If Not _langIdList.Contains(wLangId) Then
                    _langIdList.Add(wLangId)
                End If

                Dim hRscInfo = FindResourceEx(hModule, lpType, lpName, wLangId)
                Dim sizeRes = SizeofResource(hModule, hRscInfo)
                Dim hG = LoadResource(hModule, hRscInfo)
                Dim crc = CRC32(0, hG, 0, sizeRes)
                Dim rdata = New ResourceData With {
                    .HGlobal = hG,
                    .ResName = ResToString(lpName),
                    .lpName = lpName,
                    .ResSize = sizeRes,
                    .ResType = ResToString(lpType, fIsResType:=True),
                    .lpType = lpType,
                    .langId = wLangId,
                    .ImgData = _CurrentResImageData
                }
                ProcessResData(crc, rdata)

                'Select Case rt
                '    'Case ResTypes.RT_BITMAP
                '    '    Dim bytes(size) As Byte
                '    '    Marshal.Copy(hG, bytes, 0, size)
                '    '    Using mstream = New System.IO.MemoryStream(bytes)
                '    '        'http://support.microsoft.com/kb/124947/EN-US 'Retrieving Palette Information from a Bitmap Resource

                '    '        IO.File.WriteAllBytes("d:\t.bmp", bytes)
                '    '        Dim bb = New BitmapImage
                '    '        bb.BeginInit()
                '    '        bb.StreamSource = mstream
                '    '        bb.EndInit()

                '    '        Dim bm = BmpBitmapDecoder.Create(mstream, BitmapCreateOptions.None, BitmapCacheOption.OnLoad)

                '    '        Dim bmp = System.Drawing.Bitmap.FromStream(mstream, False, False)
                '    '        mstream.Position = 0
                '    '        Dim wpfbmp = BitmapFrame.Create(mstream, BitmapCreateOptions.None, BitmapCacheOption.OnLoad)
                '    '    End Using

                'End Select



                Return True
            End Function

            Private Function ResToString(ByVal lpType As IntPtr, Optional ByVal fIsResType As Boolean = False) As String
                Dim rt = String.Empty
                Try
                    If IS_INTRESOURCE(lpType) Then
                        If fIsResType Then
                            rt = CType(lpType.ToInt32, ResTypes).ToString
                        Else
                            rt = lpType.ToInt32.ToString
                        End If
                    Else
                        rt = Marshal.PtrToStringAuto(lpType)
                    End If

                Catch ex As Exception

                End Try
                Return rt
            End Function

        End Class ' ImageResources

        Public Class CRC
            Shared Function GetCRC(ByVal bufptr As IntPtr, ByVal len As Integer) As UInteger
                Dim crc = CRC32(0, bufptr, 0, len)
                Return crc
            End Function
        End Class

#If False Then

// Common/CRC.cs

namespace SevenZip
{
	class CRC
	{
		public static readonly uint[] Table;

		static CRC()
		{
			Table = new uint[256];
			const uint kPoly = 0xEDB88320;
			for (uint i = 0; i < 256; i++)
			{
				uint r = i;
				for (int j = 0; j < 8; j++)
					if ((r & 1) != 0)
						r = (r >> 1) ^ kPoly;
					else
						r >>= 1;
				Table[i] = r;
			}
		}

		uint _value = 0xFFFFFFFF;

		public void Init() { _value = 0xFFFFFFFF; }

		public void UpdateByte(byte b)
		{
			_value = Table[(((byte)(_value)) ^ b)] ^ (_value >> 8);
		}

		public void Update(byte[] data, uint offset, uint size)
		{
			for (uint i = 0; i < size; i++)
				_value = Table[(((byte)(_value)) ^ data[offset + i])] ^ (_value >> 8);
		}

		public uint GetDigest() { return _value ^ 0xFFFFFFFF; }

		static uint CalculateDigest(byte[] data, uint offset, uint size)
		{
			CRC crc = new CRC();
			// crc.Init();
			crc.Update(data, offset, size);
			return crc.GetDigest();
		}

		static bool VerifyDigest(uint digest, byte[] data, uint offset, uint size)
		{
			return (CalculateDigest(data, offset, size) == digest);
		}
	}
}

#End If


    End Class ' Images


End Namespace