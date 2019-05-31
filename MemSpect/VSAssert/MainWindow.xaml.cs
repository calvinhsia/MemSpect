//Install Memspect into a folder (see http://calvinh6/MemSpect) 
//Start VS, File->New->Project->C# Windows WPF. name it "csMemSpectClient"
// add a reference to MemSpect.Exe
// Change Project->Properties->Build->Output Path to same folder as MemSpect.
// Make sure TrackClrObjects=1 in MemSpect.ini file if you want to track managed objects
// paste this code into MainWindow.xaml.cs, hit F5.
// you can send verbs directly or you can call lib funcs

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
using MemSpect;
using System.Runtime.InteropServices;

namespace csMemSpectClient
{
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
            var pid = 0;
            while (true) // look for a devenv process to which we can connect
            {
                var procs = from proc in System.Diagnostics.Process.GetProcesses()
                            where proc.ProcessName == "devenv" &&
                                    !proc.MainWindowTitle.Contains("csMemSpectClient") // not ourself
                            select new { PName = proc.ProcessName, proc.Id, proc.MainWindowTitle };
                var b = new Browse(procs); // to display it
                this.Content = b;
                if (procs.Count() == 1)
                {
                    pid = procs.First().Id;
                    if (string.IsNullOrEmpty(ProcComm.InitComm(new string[] { "", pid.ToString() }))) //if we succeed in connecting
                    {
                        this.Title = "Connected to " + procs.First().PName + " " + pid.ToString();
                        break;
                    }
                    if (MessageBox.Show("Try again?", "", MessageBoxButton.YesNo) != MessageBoxResult.Yes)
                    {
                        return;
                    }
                }
                else
                {
                    if (MessageBox.Show("no devenv process found. Try again?", "", MessageBoxButton.YesNo) != MessageBoxResult.Yes)
                    {
                        return;
                    }
                }
            }
            ProcComm.SendMsg(Common.ProcMsgVerb.ClrObjTrk, new int[] { 1 }); // turn on CLR Obj tracking

            Common.ReadHeaps(); //populate Common._HeapList with all the heaps in the process

            var queryHeaps = from hp in Common._HeapList select new { Name = hp.GetHeapName(), hp.CurNumAllocs, _Heap = hp };

            var spControls = new StackPanel() { Orientation = System.Windows.Controls.Orientation.Horizontal };
            var txtStat = new TextBox() { Text = "hi", IsReadOnly = true };
            var chkFreeze = new CheckBox() { Content = "_Freeze", ToolTip = "Freeze/Unfreeze attached process" };

            var btnSeqno = new Button() { Content = "_SeqNo", ToolTip = "Get the current sequence number" };
            btnSeqno.Click += (s, e) =>
            {
                txtStat.Text = Common.GetGlobalPassCount().ToString("n0");
            };
            spControls.Children.Add(btnSeqno);
            var btnClrDump = new Button() { Content = "ClrDump", ToolTip = "Get the entire managed object graph" };
            btnClrDump.Click += (s, e) =>
                {
                    this.Cursor = Cursors.Wait;
                    chkFreeze.IsChecked = true;
                    var resList = new List<Common.ClrObjDump>();
                    var resGCRoots = new List<Common.HeapAllocationContainer>();
                    Common.GetCLRObjectRefs(ref resList, ref resGCRoots);
                    BrowQueryDelegate delquery =  (List<Common.HeapAllocationContainer> lst, BrowseMem bmem) =>
                        {
                            var q = from clrObj in resList
                                    let hctr = clrObj.hctnr
                                    select new
                                    {
                                        Address = hctr.TBlk.Address.ToString("x8"),
                                        hctr.AllocationStruct.SeqNo,
                                        hctr.AllocationStruct.Thread,
                                        hctr.TBlk.Size,
                                        Gen = hctr.GetGen,
                                        MovedCnt = hctr.GetMovedCnt,
                                        SurvivedCnt = hctr.GetSurvivedCnt,
                                        NumRefs = clrObj.Refs.Count,
                                        Classid = hctr.TBlk.UnionData1.ToString("x8"),
                                        ClassName = hctr.GetClassNameFromHeapCtr(true),
                                        _HeapAllocationContainer = hctr
                                    };
                            return q;
                        };
                    var ctrls = Common.DataWindowMain.MakeNewDatasurface("ObjDump", "CLR Object Dump", 40);
                    var ColWidths = new int[] { 65, 60, 60, 60, 60, 60, 60, 60, 65, 500 };
                    ctrls.SurfaceDetails.Children.Add(new BrowseMem(delquery, null, ColWidths, fAllowBrowStringFilter: true));
                    this.Cursor = Cursors.Arrow;
                    txtStat.Text = "Object dump done";
                };
            spControls.Children.Add(btnClrDump);
            var btnMegaSnap = new Button() { Content = "_MegaDump", ToolTip = "create an offline snapshot into a subfolder of MemSpect folder" };
            btnMegaSnap.Click += (s, e) =>
                {
                    this.Cursor = Cursors.Wait;
                    chkFreeze.IsChecked = true;//freeze
                    var fldr = Common.OfflineMegaSnapshot.CreateMegaSnapshot("");
                    this.Cursor = Cursors.Arrow;
                    txtStat.Text = "OfflineSnapshot done " + fldr;
                };
            spControls.Children.Add(btnMegaSnap);
            chkFreeze.Checked += (s, e) =>
            {
                ProcComm.SendMsg(Common.ProcMsgVerb.ThreadsFreeze, new int[] { 0 });
            };
            chkFreeze.Unchecked += (s, e) =>
            {
                ProcComm.SendMsg(Common.ProcMsgVerb.ThreadsUnFreeze, new int[] { 0 });
            };
            spControls.Children.Add(chkFreeze);
            spControls.Children.Add(txtStat);

            var spOuter = new StackPanel() { Orientation = System.Windows.Controls.Orientation.Vertical };
            spOuter.Children.Add(spControls);

            var dp = new DockPanel();
            BrowEventDelegate delDClick = (s, e) =>
                {
                    var dataContext = Common.GetListViewItemDataContextFromObj(e.OriginalSource);
                    var listView = s as Browse.BrowseList;
                    if (dataContext != null && listView != null)
                    {
                        var tdesc = System.ComponentModel.TypeDescriptor.GetProperties(dataContext)["_Heap"];
                        if (tdesc != null)
                        {
                            var heap = tdesc.GetValue(dataContext) as Common.CSpyHeap;
                            if (heap != null)
                            {
                                chkFreeze.IsChecked = true;
                                var snap = heap.TakeMemSnapshot();
                                Common.ShowSnapshot(heap, snap, "Snapshot " + heap.GetHeapName());
                            }
                        }
                    }
                };
            dp.Children.Add(new Browse(queryHeaps, delDClick));
            spOuter.Children.Add(dp);
            this.Content = spOuter;
        }

        private void Window_Closing(object sender, System.ComponentModel.CancelEventArgs e)
        {
            Common._IsShuttingDown = true;
            foreach (Common.ChildWindows w in Common._ChildWindows)
            {
                w.m_Window.Close();
            }
            // sending a Quit allows the namedpipe to be reused for the next time a client tries to connect
            ProcComm.SendMsg(Common.ProcMsgVerb.Quit, new int[] { 0 });
            Application.Current.Shutdown();
        }
    }
}
