Imports MemSpect
Imports System.Runtime.InteropServices

<TestClass()>
<CLSCompliant(False)>
Public Class ImmersiveTests
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
    <Description("Test immersive")>
    <Ignore>
    Public Sub ImmersiveTest()

        Try
            InitTestMethodAndSetBaseLine()
            MsgBox("startin")
            Dim pathManifest = "c:\users\calvinh\documents\visual studio 11\Projects\csGridApp1\csGridApp1\bin\Debug\AppX\AppxManifest.xml"

            Dim strPackageFullName = String.Empty '    Package Full Name : 226e5454-7bb8-43ac-b20a-17bb711430b8_1.0.0.0_neutral__faknarqntgs30

            Dim strPackageName = String.Empty              'Package Name : 226e5454-7bb8-43ac-b20a-17bb711430b8
            Dim strPackageFamilyName = String.Empty 'Package Family Name : 226e5454-7bb8-43ac-b20a-17bb711430b8_faknarqntgs30

            Dim strAppUserModelId = String.Empty ' "226e5454-7bb8-43ac-b20a-17bb711430b8_faknarqntgs30!App"

            Dim strpAppContainerNamedObjectPath = New Text.StringBuilder(MAX_PATH) 'AppContainerNamedObjects\S-1-15-2-164615428-3030373648-1861741472-1867675152-3734011038-2714587842-4209093548

            Dim hr = GetPackageNamesFromManifest(pathManifest, 0, strPackageName, strPackageFullName, strPackageFamilyName, strAppUserModelId, strpAppContainerNamedObjectPath)
            If hr <> 0 Then
                Dim res = GetErrorMessageFromHResult(hr)
                MsgBox(pathManifest + vbCrLf + res, MsgBoxStyle.Exclamation)
            Else
                _VBAssert.OutputText("Package Name                     : " + strPackageName)
                _VBAssert.OutputText("Package Full Name                : " + strPackageFullName)
                _VBAssert.OutputText("Package Family Name              : " + strPackageFamilyName)
                _VBAssert.OutputText("AppUserModelId                   : " + strAppUserModelId)
                _VBAssert.OutputText("AppContainerNamedObjectPath      : " + strpAppContainerNamedObjectPath.ToString)
                Dim strCmdLine = "c:\program files\memspect\memspect.exe"
                Dim lamDoit = Sub(nEnable As Integer)
                                  _VBAssert.OutputText("PackageDebugenabler: " + nEnable.ToString)
                                  hr = PackageDebuggingEnabler(strPackageFullName, strCmdLine, strEnvVars:=String.Empty, nEnable:=nEnable)
                                  If hr <> 0 Then
                                      _VBAssert.OutputText(GetErrorMessageFromHResult(hr))
                                  Else
                                      MsgBox("PackageDebugenabler new value set to: " + nEnable.ToString + " waiting")
                                  End If
                              End Sub

                lamDoit.Invoke(0)
                lamDoit.Invoke(1)
                Dim procid = 0
                hr = PackageLauncher(strAppUserModelId, procid)
                If hr <> 0 Then

                    _VBAssert.OutputText("Launcher call failed " + GetErrorMessageFromHResult(hr) + " " + strAppUserModelId)
                    MsgBox("package launcher err " + strAppUserModelId + vbCrLf + GetErrorMessageFromHResult(hr))
                Else
                    _VBAssert.OutputText("Launcher called Procid = " + procid.ToString + " " + strAppUserModelId)
                End If
                lamDoit.Invoke(0)

                'CImmersiveLaunch::CImmersiveLaunch
            End If


        Catch ex As Exception
            HandleTestException(ex)
        End Try

    End Sub


End Class
