Imports System.Runtime.InteropServices
Imports System.Windows.Threading

Class MainWindow
    Public _IsShuttingDown As Boolean
    Public Shared statstring As String = "staticstringThis is a static stringThis is a static stringThis is a static stringThis is a static stringThis is a static string"
    Private Sub Window_Loaded(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles MyBase.Loaded
        Try
            Width = 1000
            Left = 200
            Height = 600
            Background = Brushes.LightBlue
            Dim sp = New StackPanel With {.Orientation = Orientation.Vertical}
            Me.Content = sp
            Dim txtStatus = New TextBox With {
                .MaxLines = 25,
                .AcceptsReturn = True,
                .AcceptsTab = True,
                .IsReadOnly = True,
                .VerticalScrollBarVisibility = ScrollBarVisibility.Auto,
                .Text = String.Empty
            }
            sp.Children.Add(txtStatus)
            Dim lamAddStat = Sub(str As String)
                                 '     str = String.Format("{0} {1}", DateTime.Now.ToLongTimeString, str)
                                 txtStatus.AppendText(str + vbCrLf)
                                 txtStatus.CaretIndex = txtStatus.Text.Length
                                 txtStatus.ScrollToEnd()
                                 Me.Dispatcher.Invoke(System.Windows.Threading.DispatcherPriority.Render, Function() Nothing)
                             End Sub
            Dim fBig = False
            ' There can be multiple Arenas (NoReleaseAllocators)

            Dim nArenas = 6
            Dim SizeArena = 1024 * 1024
            Dim numAllocsPerArena = 8
            Dim hp = HeapCreate(0, 0, 0)

            Dim lamDoFreeze = Sub(strDesc As String)
                                  lamAddStat("Sending Freeze code marker: " + strDesc)
                                  If g_fIsLoggingToMemSpect Then
                                      CustomCodeMarker(Nothing, 0, 0, 1, 0, 0) ' send the code marker '1' = CodeMarkersAtWhichToFreeze
                                  End If
                              End Sub
            Dim args = Environment.GetCommandLineArgs
            If GetModuleHandle(MemSpectDllName) = 0 Then
                lamAddStat.Invoke("Not running under " + MemSpectDllName)
            Else
                g_fIsLoggingToMemSpect = True
                lamAddStat.Invoke("Running under " + MemSpectDllName)
            End If
            Dim bEncode As New Text.UnicodeEncoding
            Dim bWritten = 0
            Dim lamStoreBytes = Function(arna As Arena, strmsg As String) As IntPtr
                                    Dim bArray = bEncode.GetBytes(strmsg + Chr(0) + Chr(0))
                                    Dim sizeAlloc = bArray.Length
                                    Dim ptr = arna.Alloc(sizeAlloc + 12) ' empty bytes at end
                                    WriteProcessMemory(GetCurrentProcess, ptr, bArray, sizeAlloc, bWritten)
                                    Return ptr
                                End Function

            If args.Length > 1 Then '1st is full path exe
                Select Case args(1).ToLower
                    Case "longmember"
                        Background = Brushes.HotPink
                        Me.Title = args(1)
                        Dim x = New ClassWithLongName(Of verylongClassNameverylongClassNameverylongClassNameverylongClassNameverylongClassNameverylongClassNameverylongClassName(Of String), 
                                                      verylongClassNameverylongClassNameverylongClassNameverylongClassNameverylongClassNameverylongClassNameverylongClassName(Of String))

                        lamDoFreeze.Invoke("Freezing for LongMemberName")
                        Return

                    Case "custommarker"
                        Background = Brushes.LightCoral
                        Me.Title = args(1)
                        If g_fIsLoggingToMemSpect Then
                            Dim niters = CInt(args(2))
                            lamAddStat("custommarker #iters = " + niters.ToString)
                            Dim arrData(niters) As ProcessStartInfo
                            For iter = 1 To niters
                                CustomCodeMarker("Iteration1/OpenSolution/TypingPinvokeMarker", 22, 33, 100001, 0, 0) ' call PInvoke API for codemarker
                                arrData(iter) = New ProcessStartInfo
                                lamDoFreeze.Invoke("Freezing for CustomMarker Iter=" + iter.ToString)
                                lamAddStat("Done iter = " + iter.ToString)

                            Next

                            Return
                        End If
                    Case "arenainject"
                        Dim fIsDebugging = False
                        Dim nSleep = CInt(args(2))
                        Me.Title = args(1) + args(2)
                        Me.Width = 400
                        Me.Left += 400
                        'If args.Length > 2 AndAlso args(2) = "debug" Then
                        '    fIsDebugging = True
                        'Else

                        'End If
                        AddHandler Me.Closing, Sub()
                                                   _IsShuttingDown = True
                                               End Sub

                        WaitForOtherThreads(nSleep)

                        If Not fIsDebugging Then
                            '                            Me.Close()
                        End If
                        Return
                    Case "nullable"
                        Me.Title = args(1)
                        Me.Width = 100
                        Me.Left += 400
                        DoNullable(Me)
                        lamDoFreeze.Invoke("nullable freeze")

                        Return
                    Case "concurdict"
                        Me.Title = args(1)
                        Me.Width = 100
                        Me.Left += 400
                        _cd = New System.Collections.Concurrent.ConcurrentDictionary(Of String, String)
                        _cd("one") = "1"
                        _cd("two") = "2"
                        _cd("three") = "3"
                        lamDoFreeze.Invoke("concurdict freeze")

                        Return

                    Case "trackingmode"
                        Background = Brushes.LightGreen
                        Me.Title = args(1)
                        lamDoFreeze.Invoke("Tracking mode send 1st freeze codemarker") 'freeze once
                        Dim ar = New Arena(hp, "TMode", 10000)
                        lamDoFreeze.Invoke("Tracking mode send 2nd freeze codemarker") ' freeze again
                        Return
                    Case "lotsfree"
                        Dim allocSize = 30
                        Dim nAllocs = 400000
                        Dim nSizeArena = allocSize * nAllocs + 1000000
                        Dim allocArray(nAllocs) As IntPtr
                        Dim ar = New Arena(hp, "lotsfree", nSizeArena)
                        For i = 0 To nAllocs - 1
                            allocArray(i) = lamStoreBytes.Invoke(ar, "LF" + i.ToString)
                        Next
                        lamAddStat("Done allocating")
                        For i = 0 To nAllocs - 2
                            'For i = nAllocs - 2 To 0 Step -1
                            ar.Free(allocArray(i))
                        Next
                        '                        ar.Destroy()
                        lamAddStat("Done freeing")
                        lamDoFreeze.Invoke("lotsfree")
                        Return
                    Case "big"
                        fBig = True
                        SizeArena *= 100
                        numAllocsPerArena *= 100000
                    Case "msgbox"
                        MessageBox.Show("ArenaClient msgbox: attach debugger Id=" + Process.GetCurrentProcess.Id.ToString)
                    Case "donothing"
                        Me.Content = "donothing"
                        Return
                End Select
            Else
                'lamAddStat.Invoke("Loading MemSpectDll.Dll")
                'LoadLibrary("d:\MemSpect\MemSpectDll.dll")

            End If

            Dim Arenas(nArenas) As Arena
            Dim ArenaNoName As Arena = Nothing
            AddHandler Me.Closing, Sub()
                                       Try
                                           lamAddStat("Closing")
                                           For i = 0 To nArenas - 1
                                               If Arenas(i) IsNot Nothing Then
                                                   Arenas(i).Destroy()
                                               End If
                                           Next
                                           If ArenaNoName IsNot Nothing Then
                                               ArenaNoName.Dispose()
                                           End If
                                           lamAddStat("desroying arenahp")
                                           HeapDestroy(hp)
                                           _IsShuttingDown = True
                                       Catch ex As Exception
                                           Me.Content = ex.ToString

                                       End Try
                                   End Sub
            lamAddStat.Invoke(String.Format("Creating {0} Arenas of size {1}", nArenas, SizeArena))

            For ArenaNdx = 1 To nArenas
                If g_fIsLoggingToMemSpect Then
                    CustomCodeMarker(Nothing, 0, 0, 2, 0, 0)
                End If
                Dim nameArena = "Arena" + ArenaNdx.ToString
                Dim curArena = New Arena(hp, nameArena, SizeArena)
                Arenas(ArenaNdx) = curArena
                Dim allocs(numAllocsPerArena) As IntPtr
                Dim mark = IntPtr.Zero
                ' write some data to the arena
                For j = 0 To numAllocsPerArena - 1
                    If j = 2 Then
                        mark = curArena.Mark
                    End If
                    Dim strmsg = String.Format("ArenaFillData! {0} {1} {2}{3}{4}", nameArena, j, "SomeNRLSData", Chr(0), Chr(0))
                    allocs(j) = lamStoreBytes.Invoke(curArena, strmsg)
                Next
                If Not fBig Then
                    '                    curArena.FreeToMark(mark)
                    ' read the data back to verify
                    For j = 0 To numAllocsPerArena - 1
                        Dim str = Marshal.PtrToStringAuto(allocs(j))
                        lamAddStat.Invoke(String.Format("Read = {0} {1}", str, j))
                    Next
                End If
                If ArenaNdx = 1 Then
                    curArena.Release(mark)
                    lamStoreBytes(curArena, "Added after RelToMark")
                End If
                If ArenaNdx = 2 Then
                    curArena.Dispose()
                    Arenas(ArenaNdx) = Nothing
                End If
                If ArenaNdx = 3 Then
                    curArena.FreeAll()
                End If
                If ArenaNdx = 4 Then
                    lamAddStat.Invoke(String.Format("Freeing {0} {1}", curArena.Name, Marshal.PtrToStringAuto(allocs(2))))
                    curArena.Free(allocs(2))
                End If
                If ArenaNdx = 5 Then
                    lamStoreBytes.Invoke(curArena, "Duplicate data")
                    lamStoreBytes.Invoke(curArena, "Duplicate data")
                End If
            Next
            ArenaNoName = New Arena(hp, Nothing, 1024)
            lamStoreBytes(ArenaNoName, "This is stored in arena with no name")

            lamAddStat("Done")
            Me.Title = "ArenaClientDone"

            If g_fIsLoggingToMemSpect Then
                System.Threading.Thread.Sleep(1000)
                'MsgBox("attach debugger: about to fire codemarker at which to freeze")
                lamDoFreeze.Invoke("Freezing")
            End If
            '            MsgBox("ll")
            'ArenaNoName.Destroy()
            'For i = 1 To nArenas
            '    If Arenas(i) IsNot Nothing Then
            '        Arenas(i).Destroy()
            '        Arenas(i) = Nothing
            '    End If
            'Next
            'lamAddStat("all disposed")
        Catch ex As Exception
            Me.Content = ex.ToString
        End Try
    End Sub

    Class ClassWithLongName(Of T, k)
        Dim name As T
        Dim id As k
        Dim dict As New List(Of Dictionary(Of verylongClassNameverylongClassNameverylongClassNameverylongClassNameverylongClassNameverylongClassNameverylongClassName(Of String), 
                                   List(Of Dictionary(Of verylongClassNameverylongClassNameverylongClassNameverylongClassNameverylongClassNameverylongClassNameverylongClassName(Of String), 
                                              verylongClassNameverylongClassNameverylongClassNameverylongClassNameverylongClassNameverylongClassNameverylongClassName(Of String))
                                          )
                                      )
        )

    End Class
    Class verylongClassNameverylongClassNameverylongClassNameverylongClassNameverylongClassNameverylongClassNameverylongClassName(Of T)
        Dim longmem As List(Of T)
    End Class

    Public Class Arena
        Implements IDisposable

        Private _hHeap As IntPtr
        Private _ArenaBasePtr As IntPtr ' same as Arenabase ptr alloc'd from heap
        Private _hMemSpectArena As IntPtr ' used to comm with MemSpect
        Private _CurFree As IntPtr
        Private _Size As Integer
        Public Property Name
        Private ReadOnly Property _EndPtr As IntPtr
            Get
                Return MyAdd(_ArenaBasePtr, _Size)
            End Get
        End Property

        Public Sub New(ByVal hHeap As IntPtr, ByVal ArenaName As String, ByVal SizeArena As Integer)
            _hHeap = hHeap
            _Size = SizeArena
            Name = ArenaName
            _ArenaBasePtr = HeapAlloc(hHeap, 0, SizeArena)
            _CurFree = _ArenaBasePtr
            If g_fIsLoggingToMemSpect Then
                _hMemSpectArena = ArenaCreated(hHeap, ArenaName, dwUserData:=&H10101010)
            End If
            Verify()
        End Sub

        Public Function Alloc(ByVal size As Integer) As IntPtr
            Verify()
            Dim nextFree = MyAdd(_CurFree, size)
            If nextFree.ToInt64 >= _EndPtr.ToInt64 Then
                Throw New OutOfMemoryException()
            End If
            Dim ptrAlloc = _CurFree
            _CurFree = nextFree
            If g_fIsLoggingToMemSpect Then
                If Not ArenaAllocation(_hMemSpectArena, size, ptrAlloc, dwUserDefinedType:=2) Then
                    Throw New InvalidOperationException("ArenaAlloc?")
                End If
            End If
            Return ptrAlloc
        End Function

        Public Function Mark() As IntPtr
            Verify()
            Return _CurFree
        End Function

        Public Sub Release(ByVal mark As IntPtr)
            Verify()
            If mark = IntPtr.Zero Then ' free all, but don't destroy arena
                _CurFree = _ArenaBasePtr
            Else
                _CurFree = mark
            End If
            If g_fIsLoggingToMemSpect Then
                If Not ArenaRelease(_hMemSpectArena, mark) Then
                    Throw New Exception("ArenaRelease?")
                End If
            End If
        End Sub

        Public Function ReSize(ByVal ptr As IntPtr, ByVal cbSizeOld As Integer, ByVal cbSizeNew As Integer) As IntPtr
            Verify()
            Dim ptrNew = Alloc(cbSizeNew)
            Return ptrNew
        End Function

        ''' <summary>
        ''' free a single prior arena alloc (JS does this to add it to a freelist)
        ''' </summary>
        Public Function Free(ByVal ptr As IntPtr) As Boolean
            If g_fIsLoggingToMemSpect Then
                If Not ArenaFree(_hMemSpectArena, ptr) Then
                    Throw New Exception("Free?")
                End If
            End If
            Return True
        End Function

        Public Sub FreeAll()
            Release(IntPtr.Zero)
        End Sub

        Public Sub Destroy()
            If _hMemSpectArena <> IntPtr.Zero Then
                Verify()
                If g_fIsLoggingToMemSpect Then
                    If Not ArenaDestroy(_hMemSpectArena) Then
                        Throw New InvalidOperationException("Arena destroy?")
                    End If
                End If
                HeapFree(_hHeap, 0, _ArenaBasePtr)
                _hMemSpectArena = IntPtr.Zero
            End If
        End Sub

        Public Sub Verify()
            If _ArenaBasePtr = IntPtr.Zero Then
                Throw New InvalidOperationException("Arena base Ptr invalid?")
            End If
            If g_fIsLoggingToMemSpect Then
                If _hMemSpectArena = IntPtr.Zero Then
                    Throw New InvalidOperationException("_hMemSpectArenaPtr invalid?")
                End If
            End If
        End Sub

#Region "IDisposable Support"
        Private disposedValue As Boolean ' To detect redundant calls

        ' IDisposable
        Protected Overridable Sub Dispose(ByVal disposing As Boolean)
            If Not Me.disposedValue Then
                If disposing Then
                    Destroy()
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

    Public Sub WaitForOtherThreads(ByVal nmSecs As Integer)
        If nmSecs > 0 And Not _IsShuttingDown Then
            Dim wtch = DateTime.Now
            Do Until (DateTime.Now - wtch).Duration > TimeSpan.FromMilliseconds(nmSecs)
                If _IsShuttingDown Then
                    Exit Do
                End If
                Dispatcher.CurrentDispatcher.Invoke(
                    DispatcherPriority.Background,
                    Function()
                        System.Threading.Thread.Sleep(100)
                        Return 0
                    End Function)
            Loop
        End If
    End Sub
    Friend _cd As Concurrent.ConcurrentDictionary(Of String, String)
    Friend _nc As NullableContClass
    Private Sub DoNullable(ByVal mwindow As MainWindow)
        _nc = New NullableContClass(mwindow)
    End Sub
    Public Class NullableContClass
        Public _Nullable1 As fooStruct?
        Public _Nullable2 As Nullable(Of fooStruct)
        Public _NullableIntNoValue As Integer?
        Public _NullableIntValue As Integer?
        
        Public Sub New(ByVal mwindow As MainWindow)
            _Nullable2 = New fooStruct With {
                .memInt1 = &H100,
                .memint2 = &H200,
                .ref1 = mwindow,
                .refString = "NullableReferencedString"
            }
            _NullableIntValue = 31
        End Sub

    End Class

    Public Structure fooStruct
        Dim memInt1 As Integer
        Dim memInt2 As Integer
        Dim ref1 As MainWindow
        Dim refString As String
    End Structure
End Class


Public Module SharedStuff
    Public Const MemSpectDllName As String = "MemSpectDll.dll"
    Public g_fIsLoggingToMemSpect As Boolean
    ''' <summary>
    ''' Arena (NoRelease) Allocator tracking
    ''' 
    ''' </summary>
    ''' <param name="hHeap">A HeapHandle from CreateHeap (can be null)</param>
    ''' <param name="dwUserData">anything you want for viewing in MemSpect</param>
    ''' <param name="ArenaName">Any meaningful name to use for this Arena. 0 defaults to "Arena"</param>
    ''' <returns>a handle for the other MemSpect heap functions</returns>
    ''' <remarks></remarks>
    <DllImport(MemSpectDllName, entrypoint:="_ArenaCreated@12", CharSet:=CharSet.Auto)>
    Public Function ArenaCreated(
                               ByVal hHeap As IntPtr,
                               ByVal ArenaName As String,
                               ByVal dwUserData As IntPtr
                               ) As IntPtr
    End Function

    ''' <summary>
    ''' After an ArenaAllocation, call ArenaAlloc to track memory
    ''' </summary>
    ''' <param name="hArena">The value returned from ArenaCreate</param>
    ''' <param name="addrAlloc">The address of the allocation</param>
    ''' <param name="dwSize">The size of the allocation</param>
    ''' <returns>true on success</returns>
    ''' <remarks></remarks>
    <DllImport(MemSpectDllName, entrypoint:="_ArenaAllocation@16")>
    Public Function ArenaAllocation(ByVal hArena As IntPtr,
                                        ByVal addrAlloc As IntPtr,
                                        ByVal dwSize As Integer,
                                        ByVal dwUserDefinedType As IntPtr
                                        ) As Boolean
    End Function

    ''' <summary>
    ''' When freeing a single allocation(JScript does this)
    ''' </summary>
    ''' <param name="hArena"></param>
    ''' <param name="addrAlloc">must be from prior call to ArenaAllocation</param>
    ''' <returns>true on success</returns>
    ''' <remarks></remarks>
    <DllImport(MemSpectDllName, entrypoint:="_ArenaFree@8")>
    Public Function ArenaFree(
                                        ByVal hArena As IntPtr,
                                        ByVal addrAlloc As IntPtr) As Boolean
    End Function


    <DllImport(MemSpectDllName, entrypoint:="_ArenaMark@4")>
    Public Function ArenaMark(
                                        ByVal hArena As IntPtr) As IntPtr
    End Function


    ''' <summary>
    ''' When Release to a particular Mark
    ''' </summary>
    ''' <param name="hArena"></param>
    ''' <param name="mark">null indicates free all</param>
    ''' <returns>true on success</returns>
    ''' <remarks></remarks>
    <DllImport(MemSpectDllName, entrypoint:="_ArenaRelease@8")>
    Public Function ArenaRelease(
                                        ByVal hArena As IntPtr,
                                        ByVal mark As IntPtr) As Boolean
    End Function

    ''' <summary>
    ''' 
    ''' </summary>
    ''' <param name="hArena">The value returned from ArenaCreate</param>
    ''' <returns>true on success</returns>
    ''' <remarks></remarks>
    <DllImport(MemSpectDllName, entrypoint:="_ArenaDestroy@4")>
    Public Function ArenaDestroy(ByVal hArena As IntPtr) As Boolean
    End Function

    ''' <summary>
    ''' Call this with some markerId, which will be tracked by MemSpect
    ''' If markerId matches the CodeMarkersAtWhichToFreeze in ini file, will cause a freeze
    ''' </summary>
    ''' <param name="markerId">just a DWORD, like perfVSCloseSolution  =7066</param>
    ''' <remarks></remarks>
    <DllImport(MemSpectDllName, entrypoint:="_CustomCodeMarker@24", CharSet:=CharSet.Auto)>
    Public Sub CustomCodeMarker(ByVal strMarkername As String,
                             ByVal nEventType As Integer,
                             ByVal nDepthLevel As Integer,
                             ByVal markerId As Integer,
                             ByVal pUserData As IntPtr,
                             ByVal cbUserData As Integer)
    End Sub


    <DllImport("kernel32.dll", CharSet:=CharSet.Auto)>
    Public Function GetModuleHandle(ByVal lpModuleName As String) As IntPtr

    End Function

    <DllImport("kernel32.dll", _
        CallingConvention:=CallingConvention.Winapi, _
        CharSet:=CharSet.Auto, _
        EntryPoint:="LoadLibrary", _
        PreserveSig:=True)> _
    Public Function LoadLibrary(<[In]()> ByVal DllPath As String) As Integer

    End Function

    <DllImport("kernel32.dll", SetLastError:=True, entrypoint:="ReadProcessMemory")> _
    Public Function ReadProcessMemoryDword( _
               ByVal hProcess As IntPtr, _
               ByVal lpBaseAddress As IntPtr, _
               ByRef lpBuffer As Integer, _
               ByVal dwByteSize As Integer, _
               ByRef lpNumberOfBytesRead As Integer _
         ) As Integer
    End Function

    <DllImport("kernel32.dll", SetLastError:=True, entrypoint:="WriteProcessMemory")> _
    Public Function WriteProcessMemory( _
               ByVal hProcess As IntPtr, _
               ByVal lpBaseAddress As IntPtr, _
               ByVal lpBuffer As Byte(), _
               ByVal dwByteSize As Integer, _
               ByRef lpNumberOfBytesWritten As Integer _
         ) As Integer
    End Function

    <DllImport("kernel32.dll", SetLastError:=True)> _
    Public Function GetCurrentProcess() As IntPtr

    End Function

    'see http://blogs.msdn.com/b/calvin_hsia/archive/2010/05/30/10017430.aspx
    <DllImport("kernel32.dll", SetLastError:=True)> _
    Public Function HeapCreate(
        ByVal flOptions As UInteger,
        ByVal dwInitialSize As UIntPtr,
        ByVal dwMaximumSize As UIntPtr
        ) As IntPtr
    End Function
    <DllImport("kernel32.dll", SetLastError:=True)>
    Public Function HeapAlloc(
        ByVal hHeap As IntPtr,
        ByVal dwFlags As UInteger,
        ByVal dwSize As UIntPtr
        ) As IntPtr
    End Function
    <DllImport("kernel32.dll", SetLastError:=True)>
    Public Function HeapFree(
        ByVal hHeap As IntPtr,
        ByVal dwFlags As UInteger,
        ByVal lpMem As IntPtr
        ) As Boolean
    End Function
    <DllImport("kernel32.dll", SetLastError:=True)>
    Public Function HeapDestroy(
        ByVal hHeap As IntPtr
        ) As Boolean
    End Function


    <System.Runtime.CompilerServices.Extension()>
    Function MyAdd(ByVal this As IntPtr, ByVal addend As Long) As IntPtr
        ' for some reason, switching from release/debug causes this extension method to clash with
        ' the  method IntPtr.Add
        Dim iptrRes = IntPtr.Zero
        Try
            Dim num = this.ToInt64 + addend
            ' Dev10 had a different behavior (see Bug #170425: CInt() behavior change [throws OverflowException in Dev11; didn't in Dev10]
            If num <= Integer.MaxValue Then ' normal range
                iptrRes = New IntPtr(num)
            Else
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

End Module