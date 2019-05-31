using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using MemSpect; // add a ref to MemSpect.exe, MemSpectBase.dll and FastSerialization.dll

namespace ClientSample
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
            try
            {
                Title = "MemSpect ClientSample";

                var args = Environment.GetCommandLineArgs();
                switch (args.Length)
                {
                    case 0:
                    case 1: // the EXE itself
                        DoHelp();
                        break;
                    default:
                        break;
                }
                Loaded += (o, e) =>
                {
                    if ("/-".IndexOf(args[1][0]) >= 0)
                    {
                        if (args[1].Length < 2)
                        {
                            DoHelp();
                        }
                        switch (args[1][1])
                        {  // ClientSample.exe -p "c:\Program Files (x86)\Microsoft Visual Studio 10.0\Common7\IDE\devenv.exe"
                            case 'p': //-p "c:\windows\system32\Notepad.exe"
                                if (args.Length < 2)
                                {
                                    DoHelp();
                                }
                                var targFile = args[2];
                                int pid = 0;
                                if (int.TryParse(targFile, out pid))
                                {
                                }
                                else
                                {
                                    Title += " " + targFile;
                                    var pl = new ProcessLauncher()
                                    {
                                        _nmsecsToWaitTilStart = 2000
                                    };
                                    var hProc = pl.LaunchTargProc(targFile, fWithDll: true);
                                    ProcComm.FreezeTarget();
                                    Common.ReadHeaps();
                                    var procHeap = Common._HeapList.Where(hp => hp.HeapName == "__Process Heap").FirstOrDefault();
                                    var procHeapSnap = procHeap.TakeMemSnapshot();
                                    var z = new BrowQueryDelegate((allocs, bmem) =>
                                        {
                                            var q = from a in procHeapSnap.Allocs
                                                    select new
                                                    {
                                                        Address = a.AllocationStruct.Address.ToInt32().ToString("x8"),
                                                        a.AllocationStruct.SeqNo,
                                                        a.AllocationStruct.Thread,
                                                        a.AllocationStruct.Size,
                                                        StringContent = a.GetStringContent(),
                                                        _HeapAllocationContainer = a
                                                    };

                                            return q;
                                        }
                                        );
                                    Content = new BrowseMem(z, procHeapSnap.Allocs);
                                    Closed += (oC, eC) =>
                                        {
                                            ProcComm.UnFreezeTarget();
                                            ProcComm.SendMsg(Common.ProcMsgVerb.Quit,fSendEndMsgSync:false, dwords: new int[] { 1 }); // terminate parent process (which will terminate UI proc too)
                                            hProc.CloseMainWindow();
                                        };
                                    hProc.EnableRaisingEvents = true;
                                    hProc.Exited += (oExit, eExit) =>
                                        {
                                            Common._IsShuttingDown = true; // terminate client thread
                                        };

                                }
                                break;

                        }
                    }
                };
            }
            catch (Exception ex)
            {
                Content = string.Format("Exception {0}\r\n{1}" + ex.ToString(), DoHelp(fShowMessageBox: false));
            }
        }

        string DoHelp(bool fShowMessageBox = true)
        {
            var helpstr = @"
MemSpect Client Sample code
usage: -p ""c:\windows\system32\Notepad.exe""
";
            if (fShowMessageBox)
            {
                MessageBox.Show(helpstr);
                Environment.Exit(1);
            }
            return helpstr;
        }
    }
}
