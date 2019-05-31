Public Class ProgBarWindow
    Implements IDisposable

    Public _IsDone As Boolean
    Public _DidCancel As Boolean

    Sub New(ByVal desc As String, ByVal initText As String, ByVal windOwner As Window, ByVal fCanCancel As Boolean, ByVal fIsIndeterminate As Boolean)
        InitializeComponent()
        Me.Title = desc
        Me._txtBlk.Text = initText
        Me._ProgBar.IsIndeterminate = fIsIndeterminate

        If fCanCancel Then
            AddHandler _btnCancel.Click, Sub()
                                             _IsDone = True
                                             _DidCancel = True
                                             Me.Close()
                                         End Sub
        Else
            _btnCancel.Visibility = Windows.Visibility.Hidden
        End If
        If windOwner IsNot Nothing Then
            If Not MemSpect._IsUnderTest Then ' can't assing owner when window hasn't been shown
                Owner = windOwner
                Me.Top = Owner.Top + 50
                Me.Left = Owner.Left + 50
            End If
        End If

        Show()

    End Sub

    Sub ProgBarValueChanged() Handles _ProgBar.ValueChanged
        If _ProgBar.Value >= _ProgBar.Maximum Then
            _IsDone = True
            Me.Close()
        End If
    End Sub

    Public Sub UpdateText(ByVal newtext As String)
        _txtBlk.Text = newtext
    End Sub

#Region "IDisposable Support"
    Private disposedValue As Boolean ' To detect redundant calls

    ' IDisposable
    Protected Overridable Sub Dispose(ByVal disposing As Boolean)
        If Not Me.disposedValue Then
            If disposing Then
                ' TODO: dispose managed state (managed objects).
                Me._IsDone = True
                Me.Close()
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
