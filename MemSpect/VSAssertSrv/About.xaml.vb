Imports MemSpect.CommonUI
Imports MemSpect.Common

Public Class About
    Sub on_load() Handles MyBase.Loaded
    End Sub

    Public ReadOnly Property MemSpectLink As FrameworkElement
        Get
            Return GetMemSpectHyperLink()
        End Get
    End Property

    Public ReadOnly Property AboutData As FrameworkElement
        Get
            Dim sp As New StackPanel With {.Orientation = Orientation.Vertical}

            Dim strStory = "By Calvin Hsia. Portions by Vance Morrison, Ben Bradley." + vbCrLf +
                "Started in June 2010, originally by enhancing VSAssert.dll." + vbCrLf +
                "The first versions of MemSpect were Margin controls, but gradually I modified it to be out of process."
            sp.Children.Add(New TextBlock With {.Text = strStory})
            sp.Children.Add(CreateHyperLinkControl("MemSpect on ToolBox", "http://Toolbox/MemSpect"))
            Dim strPatent1 = "US Patent # 8881107   Automatic Memory Leak Detection  Granted 11/4/2014  (applied 8/24/2011)"
            sp.Children.Add(CreateHyperLinkControl(strPatent1, "http://www.patentbuddy.com/Patent/8881107"))
            Dim strPatent2 = "US Patent # 8918616  Memory Allocation Analysis Granted 12/23/2014  (applied 8/24/2011)"
            sp.Children.Add(CreateHyperLinkControl(strPatent2, "http://www.patentbuddy.com/Patent/8918616"))
            Dim strPatent3 = "US Patent # 9141510 Memory Allocation Tracking Granted 9/22/2015 (applied 8/24/2011)"
            sp.Children.Add(CreateHyperLinkControl(strPatent3, "http://www.patentbuddy.com/Patent/9141510"))

            sp.Children.Add(New TextBlock With {.Text = "Running as Admin= " + IsRunningAsAdmin.ToString})
            Return sp
        End Get
    End Property

End Class
