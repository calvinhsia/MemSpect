if not defined VSINSTALLDIR call "c:\Program Files (x86)\Microsoft Visual Studio 10.0\vc\bin\vcvars32.bat"
pushd bin\release
mstest /testcontainer:..\..\..\test\bin\release\MemSpect.Test.dll /detail:errormessage
rem mstest /testcontainer:..\test\bin\debug\MemSpect.Test.dll /detail:errormessage /noisolation /testsettings:local.testsettings
popd
