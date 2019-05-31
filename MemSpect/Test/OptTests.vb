Imports MemSpect
Imports System.Windows.Controls

<TestClass()>
<CLSCompliant(False)>
Public Class OptTests
    Inherits TestBase
    <ClassInitialize()>
    Public Shared Sub ClassInit(ByVal ctx As TestContext)
        BaseClassInitialize(ctx)
    End Sub
    <ClassCleanup()>
    Public Shared Sub ClassCleanup()
        BaseClassCleanup()
    End Sub

    <TestInitialize()>
    Sub TestInit()
        BaseTestInit()
    End Sub
    <TestCleanup()>
    Public Sub TestCleanup()
        BaseTestCleanup()
    End Sub

    <TestMethod()>
    Public Sub TestOptFolder()
        Try

            InitTestMethodAndSetBaseLine()
            InitializeCommonUI()
            _ConnectionMode = MemSpectMode.Offline
            _VBAssert.OutputText("Testing image folder option, using c:\MemSpect\PartialNGen folder to see Partial Ngen")
            Dim fldr = IO.Path.Combine(memspectInstallDir.ToLower, "PartialNGen")

            Dim imgui = New ImageUi(vm:=Nothing)
            Dim ctrls = imgui.ShowOptDataForFolderHelper(fldr, fShowBrowserDialog:=False)

            Dim TbCtrl = CType(ctrls.SurfaceDetails.Children(0), TabControl)
            Dim tbItem = CType(TbCtrl.Items(0), TabItem)
            Dim browFiles = CType(tbItem.Content, Browse)
            For Each itm In browFiles._BrowseList.Items
                _VBAssert.OutputText(CleanLineForBaseline(itm.ToString))
            Next
        Catch ex As Exception
            HandleTestException(ex)
        End Try
    End Sub

End Class