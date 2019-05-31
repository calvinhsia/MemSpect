// can add the fullpath to an assembly for reference like so:
//Ref: System.dll
//Ref: System.linq.dll
//Ref: System.core.dll
//Ref: MemSpectBase.dll
using MemSpect;
using System;

namespace DoesntMatter
{
    public class SomeClass
    {
        public static string DoMain(string[] args)
        {
            //AppDomain.CurrentDomain.AssemblyResolve += (or, er) =>
            //{
            //    System.Reflection.Assembly assembly = null;
            //    var filename = er.Name.Substring(0, er.Name.IndexOf(","));
            //    System.IO.Path.ChangeExtension(filename, "dll");
            //    if (System.IO.File.Exists(filename))
            //    {
            //        assembly = System.Reflection.Assembly.LoadFrom(filename);
            //    }
            //    return assembly;
            //};

            Common.UpdateStatusMsg("Executing in dynamically generated code: In Main",msgType:Common.StatusMessageType.AlertMsgBox);
            var x = 1;
            var y = 100/x;
            return 
            string.Format("Did Main in thread {0} IntPtr.size = {1}", System.Threading.Thread.CurrentThread.ManagedThreadId, IntPtr.Size);

        }

    }
}

/*
// can add the fullpath to an assembly for reference like so:
//Ref: System.dll
//Ref: System.linq.dll
//Ref: System.core.dll
//Ref: MemSpectBase.dll

////Ref: C:\Program Files (x86)\Microsoft Visual Studio\VS15Preview\Common7\IDE\PrivateAssemblies\Microsoft.VisualStudio.Telemetry.dll
////Ref: C:\Program Files (x86)\Microsoft Visual Studio\VS15Preview\Common7\IDE\PrivateAssemblies\Microsoft.ApplicationInsights.dll
////Ref: C:\Program Files (x86)\Microsoft Visual Studio\VS15Preview\Common7\IDE\PrivateAssemblies\Microsoft.ApplicationInsights.UniversalTelemetryChannel.dll
////Ref: C:\Program Files (x86)\Microsoft Visual Studio\VS15Preview\Common7\IDE\PrivateAssemblies\Microsoft.ApplicationInsights.PersistenceChannel.dll
////Ref: C:\Program Files (x86)\Microsoft Visual Studio\VS15Preview\Common7\IDE\PrivateAssemblies\Microsoft.Threading.Tasks.dll


////Ref: C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\IDE\PrivateAssemblies\Microsoft.VisualStudio.Telemetry.dll
////Ref: C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\IDE\PrivateAssemblies\Microsoft.ApplicationInsights.dll
////Ref: C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\IDE\PrivateAssemblies\Microsoft.ApplicationInsights.UniversalTelemetryChannel.dll
////Ref: C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\IDE\PrivateAssemblies\Microsoft.ApplicationInsights.PersistenceChannel.dll
////Ref: C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\IDE\PrivateAssemblies\Microsoft.Threading.Tasks.dll
//////Ref: C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\Microsoft.CSharp.dll


using System;
using MemSpect;
using Microsoft.VisualStudio.Telemetry;
using Microsoft.VisualStudio.Telemetry.WindowsErrorReporting;
using System.Runtime.InteropServices;

namespace DoesntMatter
{
    public class SomeClass
    {
        public static string DoMain(string[] args)
        {
            var res = new System.Text.StringBuilder();
            res.Append(DoTheWork());
            return res.ToString();
        }

        public static string DoTheWork()
        {
            var res = new System.Text.StringBuilder();
            res.AppendLine("Executing code: In Main " + DateTime.Now.ToString());

            TelemetrySession tsession = TelemetryService.DefaultSession;
            tsession.IsOptedIn = true;
            tsession.Start();
            tsession.SetSharedProperty("myprop", "Myval " + DateTime.Now.ToString());
            //var resProp = tsession.GetSharedProperty("myprop");
            //res.AppendFormat("Shared Property is {0}\r\n", resProp);
            tsession.PostEvent("test event");
            res.AppendFormat("Posted test event\r\n");
            try
            {
		        var tevent = new TelemetryEvent("test/event");
		        tevent.Properties["Reserved.adsf"]="a"; // Add a reserved property makes Telemetry.dll throw, using constant bucket param
		        tsession.PostEvent(tevent);
                var y = 0;
                var x = 2 / y; // divide by zero causes non-constant bucket param because dynamically compiled
            }
            catch (Exception ex)
            {
 // Reg add HKEY_CURRENT_USER\Software\Microsoft\VisualStudio\Telemetry /v FaultEventWatsonSampleRate /t REG_DWORD /d 100 /f
 // Reg add HKEY_CURRENT_USER\Software\Microsoft\VisualStudio\Telemetry /v FaultEventMaximumWatsonReportsPerSession /t REG_DWORD /d 100 /f
 // or create a FaultEvent and set IsIncludedInWatsonSample = true
                res.AppendFormat("Exception {0}\r\n", ex);
                var fEvent = new FaultEvent(
                //tsession.PostFault(
                    "vs/Fault/InjectedFault",
                    "injected fault",
                    ex,
                    (fe) =>
                    {
                        res.AppendFormat("Callback called\r\n");
                        fe.AddErrorInformation("Some error info" + res);
                        //fe.AddProcessDump(System.Diagnostics.Process.GetCurrentProcess().Id);
                        fe.AddFile(@"C:\MemSpect\ExecCode.cs");
                        for (int i = 0; i < 10; i++)
                        {
                            res.AppendFormat("Bucket {0} = {1}\r\n", i, fe.GetBucketParameter(i));
                        }
                        return 0;
                    }
                    );
                fEvent.IsIncludedInWatsonSample = true;
                tsession.PostEvent(fEvent);
                res.AppendFormat("posted fault!\r\n");
            }
            res.AppendFormat("Did main\r\n");
            // System.Threading.Thread.Sleep(5000);
            //tsession.Dispose();
            return res.ToString();
        }

    }
}
*/
