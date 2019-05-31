@echo off
rem don't need to sign anymore
rem \\cpvsbuild\drops\dev11\main\raw\current\sources\tools\x86\WIN8\signtool.exe sign /f \\cpvsbuild\drops\dev11\main\raw\current\sources\tools\testdlabsha2.pfx /p "DevDivRocks!" /fd SHA256 MemSpectDll.dll
rem \\cpvsbuild\drops\dev11\RTMRel\raw\current\sources\tools\x86\win8\signtool.exe sign /f \\cpvsbuild\drops\dev11\RTMRel\raw\current\sources\tools\testdlabsha2.pfx /p "DevDivRocks!" /fd SHA256 MemSpectDll.dll
setlocal

if %1x==x goto err
set target= \\calvinh6\c$\public
if not %2x==x set target= \\calvinh6\c$\public\%2

set srcloc="d:\MemSpect"
if not exist %srcloc%\MemSpect.exe set srcloc="c:\MemSpect"
echo target %target%
echo srcloc %srcloc%

copy %srcloc%\vsassertsrv\MemSpect.ini %srcloc%
md %target%\%1
rem make base copy: files that don't change from build to build
xcopy /f %target%\memspect\*.* %target%\%1

rem copy new binaries to drop_hist and drop
for %%x in (
%target%\%1
%target%\memspect
) do (
xcopy /f %srcloc%\MemSpectDll.dll %%x
xcopy /f %srcloc%\MemSpectDll.pdb %%x
xcopy /f %srcloc%\memspect.exe %%x
xcopy /f %srcloc%\memspect.pdb %%x
xcopy /f %srcloc%\memspect.exe.config %%x
xcopy /f %srcloc%\memspect.ini %%x
xcopy /f %srcloc%\memspectBase.dll %%x
xcopy /f %srcloc%\memspectBase.pdb %%x
xcopy /f %srcloc%\fasts*.* %%x
xcopy /f %srcloc%\traceevent*.* %%x
xcopy /f %srcloc%\MapFileDict*.* %%x
xcopy /f %srcloc%\withdll.exe %%x
xcopy /f %srcloc%\ExecCode.cs %%x
xcopy /f %srcloc%\Microsoft.Internal.Performance.CodeMarkers.dll %%x
xcopy /f %srcloc%\CodeMarkerGuidLookup.txt %%x
xcopy /f %srcloc%\MemSpectApex*.* %%x

)

rem copy source to drop hist
xcopy /if %srcloc%\VSAssert %target%\%1\VSAssert
xcopy /if %srcloc%\VSAssertSrv %target%\%1\VSAssertSrv
del %target%\%1\VSAssertSrv\*.sdf
xcopy /if %srcloc%\MemSpectBase %target%\%1\MemSpectBase
xcopy /if %srcloc%\Test %target%\%1\Test
xcopy /if %srcloc%\FastSerialization %target%\%1\FastSerialization
xcopy /if "C:\Users\calvinh\Documents\Visual Studio 12\Projects\ApexTest\MemSpectApex\*.*" %target%\%1\MemSpectApex

goto done

rem copy new binaries to drop hist
xcopy /f %srcloc%\MemSpectDll.* %target%\%1
xcopy /f %srcloc%\memspect.* %target%\%1
xcopy /f %srcloc%\fasts*.* %target%\%1
xcopy /f %srcloc%\withdll.exe %target%\%1

rem copy new binaries to drop 
xcopy /f %srcloc%\MemSpectDll.* %target%\memspect
xcopy /f %srcloc%\memspect.* %target%\memspect
xcopy /f %srcloc%\fasts*.* %target%\memspect
xcopy /f %srcloc%\withdll.exe %target%\memspect



Single command:
C:\MemSpect>\\cpvsbuild\drops\dev11\main\raw\current\sources\tools\x86\WIN8\signtool.exe sign /f \\cpvsbuild\drops\dev11\main\raw\current\sources\tools\testdlabsha2.pfx /p "DevDivRocks!" /fd SHA256 MemSpectDll.dll
Done Adding Additional Store
Successfully signed: MemSpectDll.dll


Or:


C:\MemSpect>md sign

C:\MemSpect>cd sign

C:\MemSpect\sign>copy \\cpvsbuild\drops\dev11\main\raw\50621.00\sources\tools\x86\WIN8
\\cpvsbuild\drops\dev11\main\raw\50621.00\sources\tools\x86\WIN8\AppxPackaging.dll
\\cpvsbuild\drops\dev11\main\raw\50621.00\sources\tools\x86\WIN8\AppxSip.dll
\\cpvsbuild\drops\dev11\main\raw\50621.00\sources\tools\x86\WIN8\MakeAppx.exe
\\cpvsbuild\drops\dev11\main\raw\50621.00\sources\tools\x86\WIN8\MakeAppx.exe.manifest
\\cpvsbuild\drops\dev11\main\raw\50621.00\sources\tools\x86\WIN8\Microsoft.Windows.Build.Appx.AppxPackaging.dll.manifest
\\cpvsbuild\drops\dev11\main\raw\50621.00\sources\tools\x86\WIN8\Microsoft.Windows.Build.Appx.AppxSip.dll.manifest
\\cpvsbuild\drops\dev11\main\raw\50621.00\sources\tools\x86\WIN8\Microsoft.Windows.Build.Appx.OpcServices.dll.manifest
\\cpvsbuild\drops\dev11\main\raw\50621.00\sources\tools\x86\WIN8\Microsoft.Windows.Build.Signing.mssign32.dll.manifest
\\cpvsbuild\drops\dev11\main\raw\50621.00\sources\tools\x86\WIN8\Microsoft.Windows.Build.Signing.wintrust.dll.manifest
\\cpvsbuild\drops\dev11\main\raw\50621.00\sources\tools\x86\WIN8\mssign32.dll
\\cpvsbuild\drops\dev11\main\raw\50621.00\sources\tools\x86\WIN8\OpcServices.dll
\\cpvsbuild\drops\dev11\main\raw\50621.00\sources\tools\x86\WIN8\signtool.exe
\\cpvsbuild\drops\dev11\main\raw\50621.00\sources\tools\x86\WIN8\signtool.exe.manifest
\\cpvsbuild\drops\dev11\main\raw\50621.00\sources\tools\x86\WIN8\wintrust.dll
\\cpvsbuild\drops\dev11\main\raw\50621.00\sources\tools\x86\WIN8\wintrust.dll.ini
\\cpvsbuild\drops\dev11\main\raw\50621.00\sources\tools\x86\WIN8\xapauthenticodesip.dll
       16 file(s) copied.

C:\MemSpect\sign>copy \\cpvsbuild\drops\dev11\main\raw\50621.00\sources\tools\testdlabsha2.pfx
        1 file(s) copied.

C:\MemSpect\sign>signtool.exe sign /f testdlabsha2.pfx /p "DevDivRocks!" /fd SHA256 ..\MemSpectDll.dll
Done Adding Additional Store
Successfully signed: ..\MemSpectDll.dll




rem
goto done
:err
echo usage: pub 100525
echo    or   pub 130326 beta
rem call tf shelve Memspect%1

goto done
:done
