﻿'------------------------------------------------------------------------------
' <auto-generated>
'     This code was generated by a tool.
'     Runtime Version:4.0.30319.34014
'
'     Changes to this file may cause incorrect behavior and will be lost if
'     the code is regenerated.
' </auto-generated>
'------------------------------------------------------------------------------

Option Strict On
Option Explicit On



<Global.System.Runtime.CompilerServices.CompilerGeneratedAttribute(),  _
 Global.System.CodeDom.Compiler.GeneratedCodeAttribute("Microsoft.VisualStudio.Editors.SettingsDesigner.SettingsSingleFileGenerator", "12.0.0.0"),  _
 Global.System.ComponentModel.EditorBrowsableAttribute(Global.System.ComponentModel.EditorBrowsableState.Advanced)>  _
Partial Friend NotInheritable Class MySettings
    Inherits Global.System.Configuration.ApplicationSettingsBase
    
    Private Shared defaultInstance As MySettings = CType(Global.System.Configuration.ApplicationSettingsBase.Synchronized(New MySettings()),MySettings)
    
#Region "My.Settings Auto-Save Functionality"
#If _MyType = "WindowsForms" Then
    Private Shared addedHandler As Boolean

    Private Shared addedHandlerLockObject As New Object

    <Global.System.Diagnostics.DebuggerNonUserCodeAttribute(), Global.System.ComponentModel.EditorBrowsableAttribute(Global.System.ComponentModel.EditorBrowsableState.Advanced)> _
    Private Shared Sub AutoSaveSettings(ByVal sender As Global.System.Object, ByVal e As Global.System.EventArgs)
        If My.Application.SaveMySettingsOnExit Then
            My.Settings.Save()
        End If
    End Sub
#End If
#End Region
    
    Public Shared ReadOnly Property [Default]() As MySettings
        Get
            
#If _MyType = "WindowsForms" Then
               If Not addedHandler Then
                    SyncLock addedHandlerLockObject
                        If Not addedHandler Then
                            AddHandler My.Application.Shutdown, AddressOf AutoSaveSettings
                            addedHandler = True
                        End If
                    End SyncLock
                End If
#End If
            Return defaultInstance
        End Get
    End Property
    
    <Global.System.Configuration.UserScopedSettingAttribute(),  _
     Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
     Global.System.Configuration.DefaultSettingValueAttribute("0, 0")>  _
    Public Property MainWindowLoc() As Global.System.Drawing.Point
        Get
            Return CType(Me("MainWindowLoc"),Global.System.Drawing.Point)
        End Get
        Set
            Me("MainWindowLoc") = value
        End Set
    End Property
    
    <Global.System.Configuration.UserScopedSettingAttribute(),  _
     Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
     Global.System.Configuration.DefaultSettingValueAttribute("300, 600")>  _
    Public Property MainWindowSize() As Global.System.Drawing.Size
        Get
            Return CType(Me("MainWindowSize"),Global.System.Drawing.Size)
        End Get
        Set
            Me("MainWindowSize") = value
        End Set
    End Property
    
    <Global.System.Configuration.UserScopedSettingAttribute(),  _
     Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
     Global.System.Configuration.DefaultSettingValueAttribute("0, 0")>  _
    Public Property ViewWindowLoc() As Global.System.Drawing.Point
        Get
            Return CType(Me("ViewWindowLoc"),Global.System.Drawing.Point)
        End Get
        Set
            Me("ViewWindowLoc") = value
        End Set
    End Property
    
    <Global.System.Configuration.UserScopedSettingAttribute(),  _
     Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
     Global.System.Configuration.DefaultSettingValueAttribute("800, 600")>  _
    Public Property ViewWindowSize() As Global.System.Drawing.Size
        Get
            Return CType(Me("ViewWindowSize"),Global.System.Drawing.Size)
        End Get
        Set
            Me("ViewWindowSize") = value
        End Set
    End Property
    
    <Global.System.Configuration.UserScopedSettingAttribute(),  _
     Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
     Global.System.Configuration.DefaultSettingValueAttribute("0, 0")>  _
    Public Property DlogWindowLoc() As Global.System.Drawing.Point
        Get
            Return CType(Me("DlogWindowLoc"),Global.System.Drawing.Point)
        End Get
        Set
            Me("DlogWindowLoc") = value
        End Set
    End Property
    
    <Global.System.Configuration.UserScopedSettingAttribute(),  _
     Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
     Global.System.Configuration.DefaultSettingValueAttribute("800, 600")>  _
    Public Property DlogWindowSize() As Global.System.Drawing.Size
        Get
            Return CType(Me("DlogWindowSize"),Global.System.Drawing.Size)
        End Get
        Set
            Me("DlogWindowSize") = value
        End Set
    End Property
    
    <Global.System.Configuration.UserScopedSettingAttribute(),  _
     Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
     Global.System.Configuration.DefaultSettingValueAttribute("0, 0")>  _
    Public Property ElseWindowLoc() As Global.System.Drawing.Point
        Get
            Return CType(Me("ElseWindowLoc"),Global.System.Drawing.Point)
        End Get
        Set
            Me("ElseWindowLoc") = value
        End Set
    End Property
    
    <Global.System.Configuration.UserScopedSettingAttribute(),  _
     Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
     Global.System.Configuration.DefaultSettingValueAttribute("800, 600")>  _
    Public Property ElseWindowSize() As Global.System.Drawing.Size
        Get
            Return CType(Me("ElseWindowSize"),Global.System.Drawing.Size)
        End Get
        Set
            Me("ElseWindowSize") = value
        End Set
    End Property
    
    <Global.System.Configuration.UserScopedSettingAttribute(),  _
     Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
     Global.System.Configuration.DefaultSettingValueAttribute("")>  _
    Public Property LauncherBtn() As String
        Get
            Return CType(Me("LauncherBtn"),String)
        End Get
        Set
            Me("LauncherBtn") = value
        End Set
    End Property
    
    <Global.System.Configuration.UserScopedSettingAttribute(),  _
     Global.System.Diagnostics.DebuggerNonUserCodeAttribute()>  _
    Public Property LauncherMRU() As Global.System.Collections.Specialized.StringCollection
        Get
            Return CType(Me("LauncherMRU"),Global.System.Collections.Specialized.StringCollection)
        End Get
        Set
            Me("LauncherMRU") = value
        End Set
    End Property
    
    <Global.System.Configuration.UserScopedSettingAttribute(),  _
     Global.System.Diagnostics.DebuggerNonUserCodeAttribute()>  _
    Public Property XmlMru() As Global.System.Collections.Specialized.StringCollection
        Get
            Return CType(Me("XmlMru"),Global.System.Collections.Specialized.StringCollection)
        End Get
        Set
            Me("XmlMru") = value
        End Set
    End Property
    
    <Global.System.Configuration.UserScopedSettingAttribute(),  _
     Global.System.Diagnostics.DebuggerNonUserCodeAttribute()>  _
    Public Property LauncherArgs() As Global.System.Collections.Specialized.StringCollection
        Get
            Return CType(Me("LauncherArgs"),Global.System.Collections.Specialized.StringCollection)
        End Get
        Set
            Me("LauncherArgs") = value
        End Set
    End Property
    
    <Global.System.Configuration.UserScopedSettingAttribute(),  _
     Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
     Global.System.Configuration.DefaultSettingValueAttribute("")>  _
    Public Property PerfViewSaveDir() As String
        Get
            Return CType(Me("PerfViewSaveDir"),String)
        End Get
        Set
            Me("PerfViewSaveDir") = value
        End Set
    End Property
    
    <Global.System.Configuration.UserScopedSettingAttribute(),  _
     Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
     Global.System.Configuration.DefaultSettingValueAttribute("20, 20")>  _
    Public Property LauncherWindowLoc() As Global.System.Drawing.Point
        Get
            Return CType(Me("LauncherWindowLoc"),Global.System.Drawing.Point)
        End Get
        Set
            Me("LauncherWindowLoc") = value
        End Set
    End Property
    
    <Global.System.Configuration.UserScopedSettingAttribute(),  _
     Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
     Global.System.Configuration.DefaultSettingValueAttribute("0")>  _
    Public Property ViewWindowState() As Integer
        Get
            Return CType(Me("ViewWindowState"),Integer)
        End Get
        Set
            Me("ViewWindowState") = value
        End Set
    End Property
End Class

Namespace My
    
    <Global.Microsoft.VisualBasic.HideModuleNameAttribute(),  _
     Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
     Global.System.Runtime.CompilerServices.CompilerGeneratedAttribute()>  _
    Friend Module MySettingsProperty
        
        <Global.System.ComponentModel.Design.HelpKeywordAttribute("My.Settings")>  _
        Friend ReadOnly Property Settings() As Global.MySettings
            Get
                Return Global.MySettings.Default
            End Get
        End Property
    End Module
End Namespace
