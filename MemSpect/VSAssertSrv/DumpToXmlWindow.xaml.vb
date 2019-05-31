Imports System.Collections.Specialized
Imports MemSpect
Imports MemSpect.Common
Imports Microsoft.Win32
Imports System.Text.RegularExpressions
Imports System.Xml
Imports System.IO
Imports System.Security
Imports System.Threading
Imports System.Linq
Imports System.Windows.Threading
Imports System.ComponentModel

Public Class DumpToXmlWindow

    Private _selectedFrameNode As TVStackAggPanel.StackAggTreeView.StackAggTViewItem
    Private _xmlWriter As XmlWriter
    Private _shouldStop As Boolean
    Private _dumpToXmlThread As Thread
    Private _numberOfContainerToBeProcessed As Integer
    Private _numberOfProcessedContainer As Integer
    Private _progressBarTimer As DispatcherTimer
    Private _isWindowClosed As Boolean
    Private _selectedBlockTypes As Boolean()

    Private _addressToSymbol As Dictionary(Of IntPtr, String)
    Private _symbolToAddress As Dictionary(Of String, IntPtr)
    Private _addressToAddress As Dictionary(Of IntPtr, IntPtr)

    Friend Sub New(selectedFrameNode As TVStackAggPanel.StackAggTreeView.StackAggTViewItem)
        ' This call is required by the designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.
        _selectedFrameNode = selectedFrameNode

        If Not My.Settings.XmlMru Is Nothing AndAlso My.Settings.XmlMru.Count > 0 Then
            _filePathCombo.ItemsSource = My.Settings.XmlMru
            _filePathCombo.SelectedIndex = 0
        End If

        ' Add block types to the listbox that controls the blocks to be included in XML dump.
        Dim blockTypesArray = [Enum].GetValues(GetType(BlockTypes))
        Array.Sort(blockTypesArray, New BlockTypesComparer())
        _blockTypesListBox.ItemsSource = blockTypesArray

        ' By default None and ClrObject are selected
        _blockTypesListBox.SelectedItems.Add(BlockTypes.None)
        _blockTypesListBox.SelectedItems.Add(BlockTypes.ClrObject)
    End Sub

    ''' <summary>
    ''' Dumps the selected node and all of its descendants (whether they're expanded or not into an XML file that user picks.
    ''' </summary>
    Private Sub DumpDecendantsToXml(depth As UInteger)
        Dim containerCount = _selectedFrameNode._memNode._hctrList.Count
        Dim selectedItemDepth = _selectedFrameNode._nDepth - 1 ' as depth index is 1 based
        Dim pivotFrame = _selectedFrameNode._tvm._nRootAddress

        Dim frameIndexAndContainerList = New List(Of FrameIndexContainerPair)(containerCount)

        _numberOfContainerToBeProcessed = containerCount * 2 ' one set for initial processing and one set for depth first walking
        Debug.WriteLine("Total {0}", _numberOfContainerToBeProcessed)

        For Each container In _selectedFrameNode._memNode._hctrList
            CheckIfThreadCancelled()

            ' Ignore blocks that are not selected for XML dump
            If Not _selectedBlockTypes(CInt(container.TBlkBlockType)) Then
                _numberOfContainerToBeProcessed -= 2 ' Excluding this container for further processing and depth first walking
                Continue For
            End If

            Dim callstack = container.GetCallStackAddressestoArray()

            ' Find the address of the selected frame
            Dim frameIndex = -1
            Dim childFrame = IntPtr.Zero
            For i = 0 To container.AllocationStruct.m_uicStackAddr - 1
                CheckIfThreadCancelled()

                If callstack(i) = pivotFrame Then
                    frameIndex = i - selectedItemDepth
                    Exit For
                End If
            Next

            If frameIndex < 0 Then
                ' Frame is not found so continue with next alloc
                _numberOfContainerToBeProcessed -= 2 ' Excluding this container for further processing and depth first walking
                Continue For
            End If

            frameIndexAndContainerList.Add(New FrameIndexContainerPair(frameIndex, container))
            _numberOfProcessedContainer += 1
        Next

        _addressToAddress = New Dictionary(Of IntPtr, IntPtr)
        _addressToSymbol = New Dictionary(Of IntPtr, String)
        _symbolToAddress = New Dictionary(Of String, IntPtr)

        DumpDecendantsToXmlRecursively(frameIndexAndContainerList, depth)
    End Sub

    Private Function GetSymbol(container As HeapAllocationContainer, frameIndex As Integer) As String
        Dim address = container.GetCallStackAddr(frameIndex)
        Dim symbol As String = Nothing
        If _addressToSymbol.TryGetValue(address, symbol) Then
            Return symbol
        Else
            Dim firstAddress As IntPtr
            If _addressToAddress.TryGetValue(address, firstAddress) Then
                Return _addressToSymbol(firstAddress)
            Else
                symbol = "unknown!unknown"
                Dim symbols = container.GetCallStackAsStringArray(frameIndex, 1)
                If symbols.Length > 0 AndAlso Not symbols(0) Is Nothing Then
                    symbol = StripSymbolName(symbols(0))
                End If

                If _symbolToAddress.TryGetValue(symbol, firstAddress) Then
                    _addressToAddress(address) = firstAddress
                Else
                    _symbolToAddress(symbol) = address
                    _addressToSymbol(address) = symbol
                End If

                Return symbol
            End If
        End If
    End Function

    Private Sub DumpDecendantsToXmlRecursively(frameIndexContainerPairList As List(Of FrameIndexContainerPair), depth As UInteger)

        If frameIndexContainerPairList.Count = 0 Then
            Return
        End If

        Dim allocationCount = frameIndexContainerPairList.Count
        Dim totalAllocationSize = frameIndexContainerPairList.Sum(Function(fiac As FrameIndexContainerPair) fiac.Container.GetSize())
        Dim symbol = GetSymbol(frameIndexContainerPairList(0).Container, frameIndexContainerPairList(0).Index)

        'Debug.WriteLine("Count: {0} Total: {1} Name: {2}", allocationCount, totalAllocationSize, GetMethodName(frameName))

        _xmlWriter.WriteStartElement("StackNode")
        _xmlWriter.WriteAttributeString("Symbol", symbol)
        _xmlWriter.WriteAttributeString("Cost", totalAllocationSize.ToString())

        If depth > 1 Then
            If frameIndexContainerPairList.Count = 1 Then
                ' Optimize the single container case

                Dim frameIndexContainerPair = frameIndexContainerPairList(0)

                ' Write all the frames at once without recursion
                If frameIndexContainerPair.Index > 0 Then
                    Dim depthCutOff = 0
                    If frameIndexContainerPair.Index + 1 > depth Then
                        depthCutOff = frameIndexContainerPair.Index - CInt(depth) + 1
                    End If

                    For descendantFrameIndex = frameIndexContainerPair.Index - 1 To depthCutOff Step -1
                        CheckIfThreadCancelled()

                        Dim descendantSymbol = GetSymbol(frameIndexContainerPair.Container, descendantFrameIndex)

                        'Debug.WriteLine("Count: 1 Total: {0} Name: {1}", container.GetSize, GetMethodName(descendantFrameName))

                        _xmlWriter.WriteStartElement("StackNode")
                        _xmlWriter.WriteAttributeString("Symbol", descendantSymbol)
                        _xmlWriter.WriteAttributeString("Cost", frameIndexContainerPair.Container.GetSize().ToString())
                    Next

                    ' Close all the cascaded XML elements opened above For loop
                    For i = frameIndexContainerPair.Index - 1 To depthCutOff Step -1
                        CheckIfThreadCancelled()
                        _xmlWriter.WriteEndElement()
                    Next
                End If

                _numberOfProcessedContainer += 1
                Debug.WriteLine("Current {0}", _numberOfProcessedContainer)
            Else ' frameIndexAndContainerList.Count > 1 
                ' Create a dictionary from child frames to their containers
                Dim childFrameToContainers = New HybridDictionary(1)

                ' Find the containers for child frames of the selected frame
                For Each frameIndexContainerPair In frameIndexContainerPairList
                    CheckIfThreadCancelled()

                    Dim childFrameIndex = frameIndexContainerPair.Index - 1
                    Dim container = frameIndexContainerPair.Container

                    If childFrameIndex < 0 Then
                        ' We're already at the bottom of this container's stack so there is no child frame left here.
                        _numberOfProcessedContainer += 1
                        Debug.WriteLine("Current {0}", _numberOfProcessedContainer)
                        Continue For
                    End If

                    Dim childSymbol = GetSymbol(container, childFrameIndex)

                    Dim childFrameIndexAndContainerList = CType(childFrameToContainers(childSymbol), List(Of FrameIndexContainerPair))
                    If childFrameIndexAndContainerList Is Nothing Then
                        childFrameIndexAndContainerList = New List(Of FrameIndexContainerPair)(1)
                        childFrameToContainers.Add(childSymbol, childFrameIndexAndContainerList)
                    End If

                    childFrameIndexAndContainerList.Add(New FrameIndexContainerPair(childFrameIndex, container))
                Next

                If childFrameToContainers.Count = 1 AndAlso childFrameToContainers.Values.Cast(Of List(Of FrameIndexContainerPair)).First().Count = frameIndexContainerPairList.Count Then
                    ' Since the child frame for all the containers passed into this method is same we're basically passing in a same sized list so instead of doing that let's reuse the same list
                    childFrameToContainers.Clear()
                    childFrameToContainers = Nothing

                    ' Modify frame index so that it can be used as an argument to the recursive call
                    For Each frameIndexContainerPair In frameIndexContainerPairList
                        CheckIfThreadCancelled()
                        frameIndexContainerPair.Index -= 1
                    Next

                    DumpDecendantsToXmlRecursively(frameIndexContainerPairList, depth - CUInt(1))
                Else
                    Dim childFrames(childFrameToContainers.Count - 1) As String
                    childFrameToContainers.Keys.CopyTo(childFrames, 0)

                    ' Recursively dump all the different descendant frames using their associated containers
                    For Each childFrame In childFrames
                        CheckIfThreadCancelled()
                        Dim childFrameIndexAndContainerList = CType(childFrameToContainers(childFrame), List(Of FrameIndexContainerPair))
                        DumpDecendantsToXmlRecursively(childFrameIndexAndContainerList, depth - CUInt(1))

                        ' Destroy the list as we're done with it
                        childFrameIndexAndContainerList.Clear()
                        childFrameIndexAndContainerList = Nothing
                        childFrameToContainers(childFrame) = Nothing
                    Next
                End If
            End If
        Else
            _numberOfProcessedContainer += frameIndexContainerPairList.Count
            Debug.WriteLine("Current {0}", _numberOfProcessedContainer)
        End If

        _xmlWriter.WriteEndElement() ' Close the Stack node started here
    End Sub

    ' Strips the file name and the offset from the symbol of native methods
    Private Shared Function StripSymbolName(symbolName As String) As String
        Dim fileNameSeparatorIndex = symbolName.IndexOf(" : ")
        If fileNameSeparatorIndex < 0 Then
            fileNameSeparatorIndex = 0 ' no file separator
        Else
            fileNameSeparatorIndex += 3 ' + 3 is to skip the separator
        End If

        Dim offsetSeparatorIndex = symbolName.IndexOf(" + ", fileNameSeparatorIndex)
        If (offsetSeparatorIndex < 0) Then
            Return symbolName.Substring(fileNameSeparatorIndex)
        Else
            Return symbolName.Substring(fileNameSeparatorIndex, offsetSeparatorIndex - fileNameSeparatorIndex)
        End If
    End Function

    Private Sub OnChooseFileClick(sender As System.Object, e As System.Windows.RoutedEventArgs)
        Dim saveFileDialog = New SaveFileDialog
        saveFileDialog.AddExtension = True
        saveFileDialog.CheckPathExists = True
        saveFileDialog.DefaultExt = "xml"
        saveFileDialog.Filter = "XML (*.xml)|*.xml"
        saveFileDialog.OverwritePrompt = True
        saveFileDialog.Title = "Choose location for the XML file."

        Dim result = saveFileDialog.ShowDialog
        If result.HasValue And result.Value Then
            _filePathCombo.Text = saveFileDialog.FileName
        End If
    End Sub

    Private Sub OnStartClick(sender As System.Object, e As System.Windows.RoutedEventArgs)
        ' Check XML file path
        If String.IsNullOrWhiteSpace(_filePathCombo.Text) Then
            MessageBox.Show(Me, "Please select the file path.", "Error", MessageBoxButton.OK, MessageBoxImage.Error)
            Return
        End If

        ' Read depth
        If String.IsNullOrWhiteSpace(_depthText.Text) Then
            MessageBox.Show(Me, "Please enter a valid depth. Enter 0 or 'Maximum' for maximum possible depth.", "Error", MessageBoxButton.OK, MessageBoxImage.Error)
            Return
        End If
        Dim depth As UInteger = UInteger.MaxValue
        If String.Compare(_depthText.Text, "maximum", True) = 0 OrElse String.Compare(_depthText.Text, "max", True) = 0 Then
            _depthText.Text = "Maximum"
        ElseIf UInteger.TryParse(_depthText.Text, depth) Then
            If depth = 0 Then
                depth = UInteger.MaxValue
            End If
        Else
            MessageBox.Show(Me, "Please enter a valid depth. Enter 0 or 'Maximum' for maximum possible depth.", "Error", MessageBoxButton.OK, MessageBoxImage.Error)
            Return
        End If

        ' Create the XML file
        Dim message As String = Nothing
        Try
            _xmlWriter = XmlWriter.Create(_filePathCombo.Text)
        Catch nse As NotSupportedException
            message = nse.Message
        Catch dnfe As DirectoryNotFoundException
            message = dnfe.Message
        Catch ptle As PathTooLongException
            message = ptle.Message
        Catch ioe As IOException
            message = ioe.Message
        Catch se As SecurityException
            message = se.Message
        Catch uae As UnauthorizedAccessException
            message = uae.Message
        End Try

        ' Check if XML file creation went wrong
        If Not message Is Nothing Then
            MessageBox.Show(Me, String.Format("Selected file path cannot be used. {0}", message), "Error", MessageBoxButton.OK, MessageBoxImage.Error)
            Return
        End If

        ' Save file path to MRU
        Dim newCollection = New StringCollection()
        newCollection.Add(_filePathCombo.Text)
        Dim oldCollection = CType(_filePathCombo.ItemsSource, StringCollection)
        If Not oldCollection Is Nothing Then
            Dim i = 0
            While i < oldCollection.Count AndAlso i < 10 ' Show only 10 most recently used file paths
                If String.Compare(oldCollection(i), _filePathCombo.Text, ignoreCase:=True) <> 0 Then ' Skip the currently selected one
                    newCollection.Add(oldCollection(i))
                End If
                i += 1
            End While
        End If
        _filePathCombo.ItemsSource = newCollection
        _filePathCombo.SelectedItem = _filePathCombo.Text
        My.Settings.XmlMru = newCollection
        My.Settings.Save()

        'Disable UI
        _filePathCombo.IsEnabled = False
        _filePathButton.IsEnabled = False
        _depthText.IsEnabled = False
        _startButton.IsEnabled = False

        ' Start progress bar timer
        _numberOfProcessedContainer = 0
        _numberOfContainerToBeProcessed = -1
        _progressBarTimer = New DispatcherTimer(TimeSpan.FromMilliseconds(250), DispatcherPriority.Background, AddressOf OnProgressBarTimerTick, Me.Dispatcher)
        _progressBarTimer.Start()

        ' Save the selected block types in an array for filtering
        ReDim _selectedBlockTypes([Enum].GetValues(GetType(BlockTypes)).Length)
        For Each selectedBlockType In _blockTypesListBox.SelectedItems
            _selectedBlockTypes(CInt(selectedBlockType)) = True
        Next

        ' Start the background thread for dumping the XML file
        _shouldStop = False
        _dumpToXmlThread = New Thread(AddressOf DumpToXmlThreadBody)
        _dumpToXmlThread.IsBackground = True
        _dumpToXmlThread.Start(depth)
    End Sub

    Private Sub OnProgressBarTimerTick(sender As Object, e As System.EventArgs)
        If _numberOfContainerToBeProcessed <= 0 Then
            Return
        End If

        _progressBar.Value = _progressBar.Maximum * _numberOfProcessedContainer / _numberOfContainerToBeProcessed
    End Sub

    Private Sub OnCancelClick(sender As System.Object, e As System.Windows.RoutedEventArgs)
        Me.Close()
    End Sub

    Private Sub OnWindowClosed(sender As System.Object, e As System.EventArgs)
        If Not _isWindowClosed Then
            _isWindowClosed = True

            ' Stop the progress bar timer
            If Not _progressBarTimer Is Nothing Then
                _progressBarTimer.Stop()
                _progressBarTimer = Nothing
            End If

            ' Stop the thread
            If Not _dumpToXmlThread Is Nothing Then
                _shouldStop = True
                _dumpToXmlThread.Join()
                _dumpToXmlThread = Nothing
            End If
        End If
    End Sub

    Private Sub DumpToXmlThreadBody(obj As Object)
        Dim depth = CUInt(obj)

        Try
            ' Start the top elements
            _xmlWriter.WriteStartElement("Stack")
            _xmlWriter.WriteAttributeString("Name", "Memspect Stacks")
            _xmlWriter.WriteStartElement("Tree")

            ' Write the stack tree
            DumpDecendantsToXml(depth)

            ' End the top elements
            _xmlWriter.WriteEndElement() ' end of Tree
            _xmlWriter.WriteElementString("Rollup", value:=Nothing)
            _xmlWriter.WriteEndElement() ' end of Stack
        Catch tce As ThreadCancelledException
            ' Thread is cancelled by user
        Finally
            ' Close the file
            _xmlWriter.Close()
            _xmlWriter = Nothing

            ' Close the window if it is not already closed (i.e. closing of the window initiated the termination of this thread)
            Me.Dispatcher.BeginInvoke(Sub()
                                          If Not _isWindowClosed Then
                                              Me.Close()
                                          End If
                                      End Sub)
        End Try
    End Sub

    Private Sub CheckIfThreadCancelled()
        If _shouldStop Then
            Throw New ThreadCancelledException
        End If
    End Sub

    ' Thrown when user cancels XML dumping
    Private Class ThreadCancelledException
        Inherits Exception
    End Class

    Private Class FrameIndexContainerPair
        Friend Index As Integer                     ' Frame index
        Friend Container As HeapAllocationContainer ' Conainer

        Friend Sub New(i As Integer, c As HeapAllocationContainer)
            Index = i
            Container = c
        End Sub
    End Class

    Private Class BlockTypesComparer : Implements IComparer
        Public Function Compare(x As Object, y As Object) As Integer Implements System.Collections.IComparer.Compare
            Return x.ToString().CompareTo(y.ToString())
        End Function
    End Class
End Class
