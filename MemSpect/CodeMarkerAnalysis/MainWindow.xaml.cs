using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using System.IO;
using System.Diagnostics;
using System.Collections;

namespace CodeMarkerAnalysis
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
            this.Loaded += Window_Loaded;
            this.WindowState = System.Windows.WindowState.Maximized;
        }
        public class MarkerEventEntry // contains either a marker or a Scenario start string
        {
            public string origdata;
            public string EventNameWithThread
            {
                get
                {
                    return string.Format("{0}{1}{2}",
                        cmData.MarkerName,
                        MarkerInstance == 0 ? string.Empty : MarkerInstance.ToString(),
                        ThreadId == 0 ? string.Empty : ThreadId.ToString()
                        );
                }
            }
            public string EventName
            {
                get
                {
                    return string.Format("{0}{1}",
                        cmData.MarkerName,
                        MarkerInstance == 0 ? string.Empty : MarkerInstance.ToString()
                        );
                }
            }
            public CMData cmData;
            public int SeqNo; // 1,2,3...
            public int tStamp;
            public int MarkerInstance; // 0, 1,2...   the 1st is 0, etc.
            public int Depth;   //nesting depth of code marker
            public int Elapsed; // for Begin, accumulated time in self including children
            public int ThreadId;
            public MarkerEventEntry ParentEvent; // 
            public CMData GetMate()
            {
                return cmData.cmdataMate;
            }
            public string GetHeader()
            {
                return string.Format("{0}{1}{2} Depth = {3} Ts={4} Seq={5}",
                    "",
                    cmData == null ? origdata : cmData.MarkerName,
                    MarkerInstance,
                    Depth,
                    tStamp,
                    SeqNo
                    );
            }
            public override string ToString()
            {
                return string.Format("{0}{1}{2} Depth = {3} Ts={4}",
                    new string(' ', Depth),
                    cmData == null ? origdata : cmData.MarkerName,
                    MarkerInstance,
                    Depth,
                    tStamp
                    );
            }
        }
        [DebuggerDisplay("{ToString()}")]
        public class MyTreeViewItem : TreeViewItem
        {
            public MyTreeViewItem GetParentItem()
            {
                Debug.Assert(Tag != null, "null tag?");
                return (MyTreeViewItem)Tag;
            }
            public override string ToString()
            {
                return Header.ToString();
            }
            public void ExpandAll()
            {
                this.IsExpanded = true;
                foreach (MyTreeViewItem itm in this.Items)
                {
                    itm.ExpandAll();
                }
            }
        }
        public TabControl _tabControlMain;
        public CodemarkerNames _codeMarkerNames;
        private void Window_Loaded(object sender, RoutedEventArgs e)
        {
            try
            {
                _tabControlMain = new TabControl();
                this.Content = _tabControlMain;
                //*
                var cmFileName = @"d:\MemSpect\VSAssert\CodeMarkerValues.txt";
                _codeMarkerNames = new CodemarkerNames(cmFileName);
                //var dat = new[] { 12.3, 13.32323, 1232112 };
                ////                var q = new[] { 1.2, 1.3, 123123.2112 };
                //var q = from a in dat select new { a, b = a };
                //ShowBrowse(q, "", "");
                LoadContent();
                /*/
                 
                var cmFileName = @"d:\MemSpect\VSAssert\CodeMarkerValues.txt";
                var cm = new CodemarkerNames(cmFileName);
                var q = from mrkrData in cm.MarkersByName.Values
                        orderby mrkrData.MarkerId
                        select new
                        {
                            mrkrData.MarkerId,
                            mrkrData.MarkerName,
                            mrkrData.IsBegin,
                            End = mrkrData.cmdataMate == null ? "" : mrkrData.cmdataMate.MarkerName
                        };
                Content = new BrowsePanel(q, new int[] { 100, 500, 100 });
                  //*/
            }
            catch (Exception ex)
            {
                Content = ex.ToString();
            }
        }
        public class TestCase
        {
            public string TestName;
            public string MachineName;
            public string Build;
            public int iteration;
            public string FileName; // full path
            public long FileSize
            {
                get
                {
                    return (new FileInfo(FileName)).Length;
                }
            }
            public override string ToString()
            {
                return string.Format("{0} {1} {2} {3}",
                    TestName,
                    Build,
                    iteration,
                    FileName);
            }
        }
        public List<TestCase> _TestCases = new List<TestCase>();

        void LoadContent()
        {

            try
            {
                var dirs = new[] {
                    /*/
                    @"D:\Jon\OneEtl",
                    @"D:\Jon\Two",
                    /*/
                    @"D:\CodeMarkerAnalysis\Data\Baseline\CodeMarkerLogs",
                    @"D:\CodeMarkerAnalysis\Data\Michael"
                     //*/
                };
                foreach (var dir in dirs)
                {
                    var files = System.IO.Directory.GetFiles(dir);
                    foreach (var file in files)
                    {
                        var strs = file.Split('_');
                        var machine = System.IO.Path.GetFileNameWithoutExtension(strs[0]); //D:\CodeMarkerAnalysis\Data\Baseline\CodeMarkerLogs\DDPERFX070
                        var build = strs[1]; //11.8.0.50530.01
                        var rest = string.Join("", strs, 2, strs.Length - 2); //[DevdivenvPicasso][15000][].1.codemarker.log
                        strs = rest.Split('.');
                        var testName = strs[0];
                        var iteration = strs[1];
                        Debug.Assert(strs[2] == "codemarker");
                        Debug.Assert(strs[3] == "log");
                        var tc = new TestCase()
                        {
                            TestName = testName,
                            Build = build,
                            MachineName = machine,
                            iteration = int.Parse(iteration),
                            FileName = file
                        };
                        _TestCases.Add(tc);

                    }
                }
                var filesToTry = new[] {
                    //*
                    @"D:\CodeMarkerAnalysis\Data\Baseline\CodeMarkerLogs\DDPERFX070_11.8.0.50530.01_[DesignTime_designer_winform_vb_largewinform][346][].3.codemarker.log",
                    @"D:\CodeMarkerAnalysis\Data\SlowData\CodeMarkerLogs\DDPERFX078_11.8.0.50606.01_[DesignTime_designer_winform_vb_largewinform][346][].3.codemarker.log",
                    /*/
                    @"D:\CodeMarkerAnalysis\Data\Baseline\CodeMarkerLogs\DDPERFX070_11.8.0.50530.01_[Devdiv_env_Picasso][15000][].3.codemarker.log",
                    @"D:\CodeMarkerAnalysis\Data\SlowData\CodeMarkerLogs\DDPERFX247_11.8.0.50606.01_[Devdiv_env_Picasso][15000][].3.codemarker.log",
                     //*/
                };

                var qFiles = from a in _TestCases
                             select new
                             {
                                 a.TestName,
                                 a.MachineName,
                                 a.iteration,
                                 a.Build,
                                 a.FileSize,
                                 filename = a.FileName,
                                 _TestCase = a
                             };
                var bp = ShowBrowse(qFiles, "Files", string.Empty);
                bp._BrowseList.ContextMenu.AddMenuItem(
                    (o, e) =>
                    {
                        var mitem = e.OriginalSource as MenuItem;
                        if (bp._BrowseList.SelectedItems != null && bp._BrowseList.SelectedItems.Count != 2)
                        {
                            MessageBox.Show("Need 2 selected");
                        }
                        else
                        {
                            for (int i = 0; i < 2; i++)
                            {
                                var itm = bp._BrowseList.SelectedItems[i];
                                var tdesc = System.ComponentModel.TypeDescriptor.GetProperties(itm)["_TestCase"];
                                TestCase tc = (TestCase)tdesc.GetValue(itm);
                                filesToTry[i] = tc.FileName;
                            }
                            ProcessAPairOfFiles(filesToTry);


                        }

                    },
                    "_Diff 2 selected items", "Select 2 items and Diff them (respects your Diff tool setting in VS");



                //                ProcessAPairOfFiles(filesToTry);



                _tabControlMain.SelectedIndex = _tabControlMain.Items.Count - 1;
            }
            catch (Exception ex)
            {
                this.Content = ex.ToString();

            }
        }

        public void ProcessAPairOfFiles(string[] filesToTry)
        {
            SortedList<string, MarkerEventEntry>[] lsts = new SortedList<string, MarkerEventEntry>[2];

            for (int i = 0; i < 2; i++)
            {
                var tcase = _TestCases.Where(t => t.FileName == filesToTry[i]).FirstOrDefault();
                //var tcase = _TestCases[i];
                var lstMarkerEntries = AnalyzeMarkerFile(tcase);
                lsts[i] = lstMarkerEntries;

                var q2 = from aMarkerEntry in lstMarkerEntries.Values.Where(o => o.Elapsed >= 0).OrderBy(o => o.SeqNo)
                         select new
                         {
                             aMarkerEntry.tStamp,
                             aMarkerEntry.cmData.MarkerName,
                             aMarkerEntry.MarkerInstance,
                             aMarkerEntry.Depth,
                             aMarkerEntry.ThreadId,
                             aMarkerEntry.SeqNo,
                             aMarkerEntry.Elapsed,
                             Marker = aMarkerEntry.cmData.MarkerName + (aMarkerEntry.cmData.MarkerId == 0 ? string.Empty : aMarkerEntry.MarkerInstance.ToString()),
                             aMarkerEntry.cmData.MarkerId,
                             MateId = aMarkerEntry.GetMate() == null ? "" : aMarkerEntry.GetMate().MarkerName,
                             //                                EndId = a.cmData != null && !a.cmData.IsBegin && a.cmData.cmdataMate != null ? "" : a.cmData.cmdataMate.MarkerName,
                             aMarkerEntry.origdata,
                         };

                ShowBrowse(q2, tcase.TestName, tcase.ToString());

                ShowTreeView(lstMarkerEntries);

            }
            var qJoin = from entryA in lsts[0].Values.Where(o => o.Elapsed > 0)
                        join
                    entryB in lsts[1].Values.Where(o => o.Elapsed > 0)
                        on entryA.EventName equals entryB.EventName
                        select
                         new
                         {
                             entryA.EventName,
                             ASeqNo = entryA.SeqNo,
                             BSeqNo = entryB.SeqNo,
                             AElapsed = entryA.Elapsed,
                             BElapsed = entryB.Elapsed
                         };
            ShowBrowse(qJoin, "Join", filesToTry[0] + "\n" + filesToTry[1]);


            //var qInstanceCounts = from entryA in lsts[0].Values.Where(o => o.Elapsed > 0)
            //                      group entryA by entryA.cmData.MarkerName into markgroupA
            //                      select
            //                       new
            //                       {
            //                           markgroupA.Key,
            //                           Cnt = markgroupA.Count(),
            //                           Elapsed = markgroupA.Sum(o => o.Elapsed)
            //                       };

            //ShowBrowse(qInstanceCounts, "InstanceCounts", filesToTry[0] + "\n" + filesToTry[1]);

            var qInstanceCounts2 = from entryA in lsts[0].Values.Where(o => o.Elapsed > 0)
                                   join
                                    entryB in lsts[1].Values.Where(o => o.Elapsed > 0)
                                   on entryA.EventName equals entryB.EventName
                                   group new { entryA, entryB } by entryB.cmData.MarkerName into markerGroup
                                   let aTemp =
                                    new
                                    {
                                        markerGroup.Key,
                                        Cnt = markerGroup.Count(),
                                        ElapsedA = markerGroup.Sum(o => o.entryA.Elapsed),
                                        ElapsedB = markerGroup.Sum(o => o.entryB.Elapsed),
                                        AvgA = markerGroup.Average(o => o.entryA.Elapsed),
                                        AvgB = markerGroup.Average(o => o.entryB.Elapsed),
                                    }
                                   select
                                   new
                                   {
                                       aTemp.Key,
                                       aTemp.Cnt,
                                       aTemp.ElapsedA,
                                       aTemp.ElapsedB,
                                       Diff = aTemp.ElapsedB - aTemp.ElapsedA,
                                       AvgA = aTemp.AvgA.ToString("n2"),
                                       AvgB = aTemp.AvgB.ToString("n2"),
                                       DiffAvg = (aTemp.AvgB - aTemp.AvgA).ToString("n2"),
                                   };



            ShowBrowse(qInstanceCounts2, "InstanceCounts2", filesToTry[0] + "\n" + filesToTry[1]);


            var qDelta = from entryA in lsts[0].Values.Where(o => o.Elapsed >= 0)
                         join
                          entryB in lsts[1].Values.Where(o => o.Elapsed >= 0)
                         on entryA.EventName equals entryB.EventName
                         select new
                         {
                             Delt = entryB.tStamp - entryA.tStamp
                         };
            ShowBrowse(qDelta, "Delta", "");
        }
        public BrowsePanel ShowBrowse(IEnumerable q, string tabname, string tooltip)
        {
            var hdr = new TextBlock()
            {
                Text = tabname,
                ToolTip = tooltip
            };
            var bp = new BrowsePanel(q);
            _tabControlMain.Items.Add(
                new TabItem()
                {
                    Header = hdr,
                    Content = bp
                }
            );
            return bp;
        }

        public void ShowTreeView(SortedList<string, MarkerEventEntry> lstMarkerEntries)
        {
            // now create a treeview
            var treeRoot = new TreeView();
            _tabControlMain.Items.Add(
                new TabItem()
                {
                    Header = "Treeview",
                    Content = treeRoot
                }
            );
            var tvCuritem = new MyTreeViewItem()
            {
                Header = "RootNode"
            };
            var tvRootItem = tvCuritem;


            treeRoot.Items.Add(tvCuritem);

            var tvitemStack = new Stack<MyTreeViewItem>();
            int nCurDepth = 0;
            MarkerEventEntry priorItem = null;
            foreach (var item in lstMarkerEntries.Values.Where(o => o.cmData.IsBegin || o.cmData.IsEnd).OrderBy(o => o.SeqNo))
            {
                var tvNewItem = new MyTreeViewItem()
                {
                    Header = item.GetHeader()
                };
                if (item.Depth == nCurDepth) // same depth, just add siblings to parent
                {
                    if (item.cmData.IsBegin &&
                        priorItem != null &&
                        priorItem.GetMate() != null
                        ) // it's a begin, but at the same depth: and prior was an end
                    { // our depth doesn't change
                        tvCuritem = tvitemStack.Pop(); // original parent
                        tvCuritem.Items.Add(tvNewItem);
                        tvitemStack.Push(tvNewItem);
                        tvCuritem = tvNewItem;
                    }
                    else
                    {
                        tvCuritem.Items.Add(tvNewItem); // add sibling to parent
                        if (item.cmData.IsEnd)
                        {
                            nCurDepth--;
                            if (tvitemStack.Count > 0)
                            {
                                tvCuritem = tvitemStack.Pop();
                            }
                            //var parentEntry = (MarkerEntry)tvParentItem.Tag;
                            //if (item.cmData.cmdataMate.MarkerId != parentEntry.cmData.MarkerId)
                            //{
                            //    Debug.Assert(false);
                            //}
                        }
                    }
                }
                else
                {
                    if (item.Depth == nCurDepth + 1) // descend down a level
                    {
                        //    Debug.Assert(item.cmData.IsBegin, "must be begin?");
                        if (item.cmData.IsBegin)
                        {
                            tvCuritem.Items.Add(tvNewItem);
                            nCurDepth++;

                            if (tvCuritem != null)
                            {
                                tvitemStack.Push(tvCuritem); // save parent
                            }
                            else
                            {
                                Debug.Assert(false, "null parent?");
                            }
                            tvCuritem = tvNewItem;
                        }
                    }
                    else
                    {
                        //                                Debug.Assert(false, "can't get here");
                    }
                }
                priorItem = item;
            }

            tvRootItem.ExpandAll();
        }
        public SortedList<string, MarkerEventEntry> AnalyzeMarkerFile(TestCase tcase)
        {
            var lst = new SortedList<string, MarkerEventEntry>();
            var instDict = new Dictionary<int, int>(); // markerid to instance count
            var t0 = 0;
            var threadId = 0;
            MarkerEventEntry parentEntry = new MarkerEventEntry()
            {
            };
            Stack<MarkerEventEntry> stkEvents = new Stack<MarkerEventEntry>();
            using (StreamReader file = new StreamReader(tcase.FileName))
            {
                string line;
                var curdepth = 0;
                var depthToUse = 0; // usualy same as curdepth: Begin will not be 1 lower, End will be 1 lower
                var seqno = 0;
                while ((line = file.ReadLine()) != null)
                {
                    CMData cmdata = null;
                    var strs = line.Split('\t');
                    var tstampStr = string.Empty;
                    var tstamp = 0;
                    switch (strs[0])
                    {
                        case "Fired:":
                            //case "WaitingFor:":
                        case "Discarded:":
                        case "Begin:":
                            var cmNum = strs[2];
                            var mrkInstance = 0;
                            if (strs[0] == "Begin:") // like scenario begin
                            {
                                tstampStr = strs[2];
                                cmdata = new CMData()
                                {
                                    MarkerName = line
                                };
                            }
                            else
                            { // it's a code marker
                                tstampStr = strs[1];
                                if (strs.Length > 3)
                                {
                                    threadId = int.Parse(strs[3]);
                                }
                                cmdata = _codeMarkerNames.NameForNumber(Int32.Parse(cmNum));
                                if (instDict.ContainsKey(cmdata.MarkerId))
                                {
                                    instDict[cmdata.MarkerId] += 1;
                                }
                                else
                                {
                                    instDict[cmdata.MarkerId] = 1;
                                }
                                mrkInstance = instDict[cmdata.MarkerId];
                                if (cmdata.IsBegin)
                                {
                                    curdepth++;
                                    depthToUse = curdepth;
                                }
                                else
                                {
                                    if (cmdata.IsEnd)
                                    {
                                        //*
                                        //if (parentEntry.cmData != null &&  cmdata.cmdataMate.MarkerId != parentEntry.cmData.MarkerId)
                                        {
                                            //Debug.Assert(false);
                                            //throw new InvalidOperationException(
                                            //"mismatch begin/end? " + cmdata.ToString() + ":" + parentEntry.cmData.ToString() + " " +
                                            //"ListCnt=" + lst.Count.ToString() + " " +
                                            //seqno.ToString() + " " + parentEntry.SeqNo.ToString()
                                            //);
                                        }
                                        /*/
                                        Debug.Assert(cmdata.cmdataMate.MarkerId == parentEntry.cmData.MarkerId,
                                            "mismatch begin/end? " + cmdata.ToString() + ":" + parentEntry.cmData.ToString() + " " +
                                            "ListCnt=" + lst.Count.ToString() + " " +
                                            seqno.ToString() + " " + parentEntry.SeqNo.ToString()
                                            );
                                         //*/
                                        depthToUse = curdepth;
                                        if (curdepth > 0)
                                        {
                                            curdepth--;
                                        }
                                    }
                                    else
                                    {
                                        depthToUse = curdepth;
                                    }
                                }
                            }
                            if (tstampStr.Contains("."))
                            {
                                tstampStr = tstampStr.Substring(0, tstampStr.IndexOf("."));
                                //                               tstampStr= tstampStr.Replace(".", "");
                            }
                            if (t0 == 0)
                            {
                                t0 = Int32.Parse(tstampStr);
                            }
                            else
                            {
                                tstamp = Int32.Parse(tstampStr) - t0;
                            }
                            Debug.Assert(cmdata != null);
                            var newEntry = new MarkerEventEntry()
                            {
                                origdata = line,
                                tStamp = tstamp,
                                cmData = cmdata,
                                MarkerInstance = mrkInstance,
                                Depth = depthToUse,
                                ParentEvent = parentEntry,
                                SeqNo = seqno,
                                ThreadId = threadId
                            };
                            seqno++;
                            Debug.WriteLine(newEntry.ToString());
                            lst.Add(newEntry.EventNameWithThread, newEntry);
                            if (cmdata.IsBegin)
                            {
                                stkEvents.Push(parentEntry);
                                parentEntry = newEntry;
                            }
                            else
                            {
                                if (cmdata.IsEnd)
                                {
                                    //                                    Debug.Assert(stkEvents.Count > 0);
                                    if (stkEvents.Count > 0)
                                    {
                                        parentEntry = stkEvents.Pop();
                                        // now we want to find the begin and fix up it's data
                                        var keyBegin = cmdata.cmdataMate.MarkerName + mrkInstance.ToString() + (threadId == 0 ? string.Empty : threadId.ToString());
                                        if (!lst.ContainsKey(keyBegin))
                                        {
                                            //   Debug.Assert(false, "key begin not found " + keyBegin);
                                        }
                                        else
                                        {
                                            var beginEntry = lst[keyBegin];
                                            beginEntry.Elapsed = tstamp - beginEntry.tStamp;
                                        }
                                    }

                                }
                            }

                            break;
                        default:
                            break;
                    }
                }
            }

            return lst;
        }
    }


    [DebuggerDisplay("{ToString()}")]
    public class CMData : IComparable
    {
        public int MarkerId;
        public string MarkerName;
        public bool IsBegin;
        public bool IsEnd
        {
            get
            {
                return cmdataMate == null ? false : true;
            }
        }
        public int Priority; // 0 is high
        public CMData cmdataMate; // if it's an end marker, points to the Begin marker, and v.v.

        public int CompareTo(object obj)
        {
            return string.Compare(MarkerName, ((CMData)obj).MarkerName, ignoreCase: true);
            //return MarkerName.CompareTo(((CMData)obj).MarkerName,);
            //                return ((CMData)obj).MarkerName.CompareTo(MarkerName);
        }
        public override string ToString()
        {
            return string.Format("{0} {1}", MarkerId, MarkerName);
        }
    };

    public class CodemarkerNames
    {
        public int[] FindNearest<T>(
            IList<T> sortedList,
            T value
            )
        {
            var left = 0;
            var right = 1;
            var maxRight = sortedList.Count - 1;
            var maxLeft = 0;
            var partition = new[] { maxLeft, maxRight };
            var nearest = partition;
            var pComparer = Comparer<T>.Default;
            while (partition[right] >= partition[left])
            {
                int split = partition[left] + (partition[right] - partition[left]) / 2;
                var ord = pComparer.Compare(value, sortedList[split]);
                switch (ord)
                {
                    case -1:
                        nearest[right] = split;
                        partition[right] = split - 1;
                        break;
                    case 1:
                        nearest[left] = split;
                        partition[left] = split + 1;
                        break;
                    case 0:
                        nearest = new[] { split, split };
                        return nearest;
                }
            }
            if (partition[right] < maxLeft)
            {
                nearest = new[] { -1, maxLeft };
            }
            else if (partition[left] > maxRight)
            {
                nearest = new[] { maxRight, -1 };
            }
            return nearest;
        }

        private string[] startStrs = new[] { "begin", "start", "started" };  // lowercase
        private string[] endStrs = new[] { "end", "stop", "finish", "finished", "completed" }; //lowercase
        private string[] excludeSects = new[] { "Codemarkers_IncludeClickOnceMarkers", "Codemarkers_IncludeAppEnum" };

        // pairs of start/end that don't follow naming conventions at all
        private int[][] hardcodedMarkerPairs = new[] {
            new [] {9218, 9219}, //perfVSThreadedWaitDialogShowing, perfVSThreadedWaitDialogHidden
            new [] {7533, 7534}, //perfFXCreateDesignSurface, perfFXCreateDesignSurfaceEnd
            new [] {7506, 7507}, //perfFXDesignFromCodeToDesignStart, perfFXDesignFromCodeToDesign
            new [] {7072, 7073}, //perfVSSplashScreenShowing , perfVSSplashScreenDestroyed
        };

        // starts with no stops, or starts with not the same # of stops fired. These'll wreak havoc on nesting
        private int[] startsWithNoStopsFired = new[] { 
                7213, //perfVBCompilerBackgroundThreadStart
                7308, //perfSchemaLoadBegin
                7427,//perfEditorLoadTextImageFromMemoryStart
                7529,  //perfFXGetFileDocDataStart
                9447, //perfVSPerfWatsonUnResponsiveBegin
                 };



        public SortedList<string, CMData> MarkersByName = new SortedList<string, CMData>(StringComparer.OrdinalIgnoreCase);

        public SortedList<int, CMData> MarkersById = new SortedList<int, CMData>();

        public CodemarkerNames(string fileName)
        {
            // "\\cpvsbuild\DROPS\dev11\main\raw\current\sources\vscommon\CodeMarkers\CodeMarkerValues.txt"
            using (StreamReader file = new StreamReader(fileName))
            {
                string wholeLine;

                var InExcludeSection = string.Empty;
                while ((wholeLine = file.ReadLine()) != null)
                {
                    if (string.IsNullOrEmpty(InExcludeSection)) // if we're not in an exclude section
                    {
                        // check to see if we're starting one
                        foreach (string sect in excludeSects)
                        {
                            if (wholeLine.Contains(sect))
                            {
                                InExcludeSection = sect; // flag the sect
                                continue; // ignore lines til end of sect
                            }
                        }
                        // normal line
                    }
                    else
                    {//we're in exclude section
                        // check to see if we're ending it
                        if (wholeLine.Contains(InExcludeSection))
                        {
                            InExcludeSection = null;
                        }
                        continue; // ignore lines til end of sect
                    }
                    {
                        int i = wholeLine.IndexOf("ENUMVALUE");
                        if (i != -1 && !wholeLine.StartsWith("REM"))
                        {
                            var subline = wholeLine.Substring(i + 9);
                            i = subline.IndexOf("(");
                            subline = subline.Substring(i + 1);
                            i = subline.IndexOf(")");
                            subline = subline.Substring(0, i);
                            string[] parts = subline.Split(",".ToCharArray());
                            if (char.IsDigit(parts[0][0]))
                            {
                                int number = Int32.Parse(parts[0]);
                                string markerName = parts[1].Trim();
                                MarkersByName.Add(markerName, new CMData()
                                {
                                    MarkerId = number,
                                    MarkerName = markerName,
                                });
                            }
                        }
                    }
                }
            }

            // now look for begin/end pairs
            // pairs are not always adjacent
            // some Starts have no End and v.v.
            // alpha search problem: perfOpenStart, perfOpenStart match, but perfOpenAttachBegin is between
            // thus need to do exact match
            // first we mark the Begins so we don't care about End/Begin or Begin/End
            foreach (CMData cm in MarkersByName.Values.Where(
                            mrk => !startsWithNoStopsFired.Contains(mrk.MarkerId)
                ))
            {
                var beginRoot = FindSubstr(cm.MarkerName, startStrs);
                if (!string.IsNullOrEmpty(beginRoot))
                {
                    cm.IsBegin = true;
                }
            }

            // now that we've marked the Begins, lets find the Ends
            foreach (CMData cm in MarkersByName.Values)
            {
                var endRoot = FindSubstr(cm.MarkerName, endStrs); // the rootname of the marker
                if (!string.IsNullOrEmpty(endRoot)) // Do we have an End marker?
                {
                    var fGotit = false;
                    foreach (string suff in startStrs) // find it's matching Start
                    {
                        if (MarkersByName.ContainsKey(endRoot + suff))
                        {
                            var matchingBegin = MarkersByName[endRoot + suff];

                            if (matchingBegin.IsBegin) // in some cases, like startsWithNoStopsFired
                            {
                                matchingBegin.cmdataMate = cm;
                                Debug.Assert(cm.cmdataMate == null, "mate already taken?");
                                cm.cmdataMate = matchingBegin;
                                fGotit = true;
                                break;
                            }
                        }
                    }
                    if (!fGotit)
                    {
                        System.Diagnostics.Trace.WriteLine("No Match for " + cm.MarkerName);
                        //No Match for  perfEDT_IdleProcessManagerWorkCompleted
                        //No Match for  perfEDT_LoadTestRunManagementExportCompleted
                        //No Match for  perfEDT_LoadTestRunManagementImportCompleted
                        //No Match for  perfEDT_LoadTestRunManagementOpenCompleted
                        //No Match for  perfEDT_LoadTestRunManagementRemoveCompleted
                        //No Match for  perfEDT_TmiLoadResultsFinished
                        //No Match for  perfEDT_TmiSaveRunResultsFinished
                        //No Match for  perfTsData_SchemaManagerRecycleEnd
                        //No Match for  perfVSTR_WITUIUIGetListOfGroupsEnd
                        //No Match for perfDesignerBootWithProjectStop
                        //No Match for perfExtensionManagerInstallUICompleted
                        //No Match for perfExtensionManagerQueryCompleted
                        //No Match for perfFXCodeGenerationEnd
                        //No Match for perfFXCreateDesignSurfaceEnd
                        //No Match for perfFXDesignCreateComponentEnd
                        //No Match for perfFXDesignElementHostDesignerSetChildEnd
                        //No Match for perfFXDesignPBOnSelectionChangedEnd
                        //No Match for perfFXDesignPropertyBrowserCreateEnd
                        //No Match for perfFXEmitMethodEnd
                        //No Match for perfFXFormatMethodEnd
                        //No Match for perfFXGenerateCodeTreeEnd
                        //No Match for perfFXGetGlobalObjectsEnd
                        //No Match for perfFXIntegrateSerializedTreeEnd
                        //No Match for perfFXNotifyStartupServicesEnd
                        //No Match for perfFXParseEnd
                        //No Match for perfFXToolboxRefreshEnd
                        //No Match for perfHxInitFTSInitTitleArrayInitFreeHxSEnd
                        //No Match for perfHxInitFTSInitTitleArrayInitHxQEnd
                        //No Match for perfMSBuildProjectEvaluatePass0End
                        //No Match for perfMSBuildProjectEvaluatePass1End
                        //No Match for perfMSBuildProjectEvaluatePass2End
                        //No Match for perfMSBuildProjectEvaluatePass3End
                        //No Match for perfMSBuildProjectEvaluatePass4End
                        //No Match for perfProgression_VirtualCanvasUpdateVisualsEnd
                        //No Match for perfSCPEnd
                        //No Match for perfSharePointProjectLoadEnd
                        //No Match for perfShellUI_HierarchyAsyncExpansionCompleted
                        //No Match for perfUmlModelExplorerRefreshEnd
                        //No Match for perfVBInsertSnippetEnd
                        //No Match for perfVBIntelliXMLIndexingEnd
                        //No Match for perfVBRenameSymbolEnd
                        //No Match for perfVCDTParseEnd
                        //No Match for perfViewInBrowserEnd
                        //No Match for perfVisioMoveObjectEnd
                        //No Match for perfVSCSharpBatchedRequestsCompleted
                        //No Match for perfVSData_ServerExplorerEnumerationEnd
                        //No Match for perfVSDebuggerPaintEnd
                        //No Match for perfVSDebuggerScrollEnd
                        //No Match for perfVSDebuggerSessionEnd
                        //No Match for perfVsDebuggerStartWithoutDebuggingEnd
                        //No Match for perfVSHelpSearchCompleted
                        //No Match for perfVSMacrosExplorerShowEnd
                        //No Match for perfVSMacrosIDEShowEnd
                        //No Match for perfVSMacrosMacroRunEnd
                        //No Match for perfVssSSExpRefreshFileListEnd
                        //No Match for perfVssSSExpStartupEnd
                        //No Match for perfVssSSExpStatusSearchEnd
                        //No Match for perfVssSSExpUpdateChildrenEnd
                        //No Match for perfVssSSExpWildcardSearchEnd
                        //No Match for perfVSTF_ELeadConnectToTfsDlgUpdateProjectListEnd
                        //No Match for perfVSTF_ELeadGroupsViewMemberPropertiesEnd
                        //No Match for perfVSTOManagedWarmupASyncEnd
                        //No Match for perfVSTOVSTProjectSetInnerProjectEnd
                        //No Match for perfVSTOVSTProjectWizardOnFinish
                        //No Match for perfVSWhitehorseBackgroundSyncEnd

                    }
                }
            }
            // now go through and remove all the unmatched
            foreach (CMData cm in MarkersByName.Values)
            {
                if (cm.IsBegin && cm.cmdataMate == null) // no matching end, like perfDialogBegin
                {
                    Trace.WriteLine("No matching end " + cm.ToString());
                    cm.IsBegin = false;
                    //No matching end 24205  perfSimulatorConnectStarted
                    //No matching end 24200  perfSimulatorLaunchBegin
                    //No matching end 24206  perfSimulatorLogonStarted
                    //No matching end 20326  perfTeamTest_TCMUIPackageCtorBegin
                    //No matching end 20308  perfTeamTest_TestExecutionManagerCreateAndExecuteTestRunBegin
                    //No matching end 20309  perfTeamTest_TestViewOpenBegin
                    //No matching end 20314  perfTeamTest_TmiTestRunStarted
                    //No matching end 7444  perfVSCloseSolutionBegin
                    //No matching end 9552  perfVSTR_WITUIGetListOfGroupsBegin
                    //No matching end 7651 perfBaselineServiceStart
                    //No matching end 830 perfDatapageOpenStart
                    //No matching end 512 perfDialogBegin
                    //No matching end 7506 perfFXDesignFromCodeToDesignStart
                    //No matching end 7604 perfHelpPageLoadStart
                    //No matching end 7175 perfHxExCollBTLStart
                    //No matching end 9043 perfHxIndexInitializeMergedFileBegin
                    //No matching end 9042 perfHxIndexQueryBegin
                    //No matching end 9407 perfHxIndexTopicId2TopicArrayBegin
                    //No matching end 9030 perfHxInitFTSKeywordBegin
                    //No matching end 9415 perfHxInitTitleBegin
                    //No matching end 9400 perfHxProtocolInitBegin
                    //No matching end 9402 perfHxProtocolInternalStartBegin
                    //No matching end 9412 perfHxTitleGetTopicURLBegin
                    //No matching end 9408 perfHxTitleInformationInitializeBegin
                    //No matching end 8415 perfMSVSEditorsWCFUpdateStart
                    //No matching end 750 perfOfficeFileSaveDlgBegin
                    //No matching end 745 perfOfficeHlinkDialogBegin
                    //No matching end 515 perfOLEInsertBegin
                    //No matching end 720 perfPhd3DStart
                    //No matching end 710 perfPhdBlurSharpStart
                    //No matching end 716 perfPhdColorCorrectStart
                    //No matching end 727 perfPhdCropStart
                    //No matching end 728 perfPhdCutOutStart
                    //No matching end 715 perfPhdDesignerEdgeStart
                    //No matching end 712 perfPhdDesignerEffectStart
                    //No matching end 724 perfPhdDocSwitchStart
                    //No matching end 713 perfPhdDrawAutoShapeStart
                    //No matching end 709 perfPhdFadeStart
                    //No matching end 721 perfPhdInsertTextStart
                    //No matching end 717 perfPhdMoveStart
                    //No matching end 714 perfPhdPhotoArtisticBrushStart
                    //No matching end 711 perfPhdPhotoCorrectionStart
                    //No matching end 718 perfPhdResizeStart
                    //No matching end 719 perfPhdRotateStart
                    //No matching end 708 perfPhdSolidColorFillStart
                    //No matching end 723 perfPhdTemplatesStart
                    //No matching end 722 perfPhdUpdateTextStart
                    //No matching end 725 perfPhdWorkpaneStart
                    //No matching end 726 perfPhdZoomStart
                    //No matching end 729 perfPPTApplyTemplateStart
                    //No matching end 7331 perfSelectionSyncBegin
                    //No matching end 17265 perfSharePointBuildStart
                    //No matching end 510 perfSlideShowBegin
                    //No matching end 516 perfSlideViewScrollBegin
                    //No matching end 9959 perfTsData_SchemaManagerRecycleBegin
                    //No matching end 20501 perfUmlModelExplorerCollapseBegin
                    //No matching end 20500 perfUmlModelExplorerExpandBegin
                    //No matching end 7350 perfVCDTParseOnMainThreadBegin
                    //No matching end 7351 perfVCDTParseOnParserThreadBegin
                    //No matching end 7105 perfVSBrowserDocumentNavigateStart
                    //No matching end 7199 perfVSClientRunStart
                    //No matching end 9093 perfVSContextServiceStart
                    //No matching end 9021 perfVSCreateWebBrowserExStart
                    //No matching end 9103 perfVSDexploreInitAppNameStart
                    //No matching end 18200 perfVSExpress_GettingStartedBegin
                    //No matching end 9438 perfVSHeapCreateBegin
                    //No matching end 9192 perfVSHelpF1LookupGetNameBegin
                    //No matching end 9194 perfVSHelpF1LookupGetUrlBegin
                    //No matching end 9191 perfVSHelpF1LookupLoadKeywordBegin
                    //No matching end 9170 perfVSHelpFilterToolInitFillBegin
                    //No matching end 9186 perfVSHelpGetAllAttrValuesBegin
                    //No matching end 9134 perfVSHelpGetTopicsFromKeywordStart
                    //No matching end 9182 perfVSHelpHrDoLocalF1Begin
                    //No matching end 9171 perfVSHelpHrDoLocalF1LookupBegin
                    //No matching end 9183 perfVSHelpHrDoOnlineF1Begin
                    //No matching end 9051 perfVSHelpIndexInitBegin
                    //No matching end 9147 perfVSHelpInitTocStart
                    //No matching end 9066 perfVSHelpInitValidatorBegin
                    //No matching end 9068 perfVSHelpInitValidatorWithFileBegin
                    //No matching end 9122 perfVSHelpKeywordLookupBegin
                    //No matching end 9055 perfVSHelpMergeIndexBegin
                    //No matching end 9005 perfVSHelpPutSettingsTokenStart
                    //No matching end 9015 perfVSHelpServiceF1Begin
                    //No matching end 9008 perfVSHelpSetCollectionBegin
                    //No matching end 9009 perfVSHelpSetCollectionReInitStart
                    //No matching end 9090 perfVSHelpUserSettingsLoadManagedStart
                    //No matching end 9112 perfVSHelpXLinkIndexSetFilterBegin
                    //No matching end 9261 perfVSLocalRegistryCreateInstanceBegin
                    //No matching end 18203 perfVSNewProjectDlgBegin
                    //No matching end 9312 perfVSSettingsCategoryExportStart
                    //No matching end 9310 perfVSSettingsCategoryImportStart
                    //No matching end 9304 perfVSSettingsExportStart
                    //No matching end 9302 perfVSSettingsImportStart
                    //No matching end 9306 perfVSSettingsLoadBegin
                    //No matching end 9308 perfVSSettingsSaveBegin
                    //No matching end 9300 perfVSSettingsStartupCheckBegin
                    //No matching end 9803 perfVSTF_ELeadProjectCreationBegin
                    //No matching end 9878 perfVSTF_SCCPackageBegin
                    //No matching end 9879 perfVSTF_SCEAsyncRefillBegin
                    //No matching end 8000 perfVSTOVSTProjectPackageSetSiteStart
                    //No matching end 8001 perfVSTOVSTProjectPrecreateForOuterStart
                    //No matching end 9080 perfVSWebBrowserOnDownloadBegin
                    //No matching end 9431 perfVSWebPkgSetSiteBegin
                    //No matching end 8212 perfVSWhitehorseTopLevelTransactionBegin
                    //No matching end 7339 perfWebFormLoadBegin
                }
            }

            // now handle hard coded marker pairs
            foreach (var pair in hardcodedMarkerPairs)
            {
                var begincm = MarkersByName.Values.Where(mrk => mrk.MarkerId == pair[0]).FirstOrDefault();
                var endcm = MarkersByName.Values.Where(mrk => mrk.MarkerId == pair[1]).FirstOrDefault();
                begincm.IsBegin = true;
                begincm.cmdataMate = endcm;
                endcm.cmdataMate = begincm;
            }

            // now create a sorted list b markerid 
            var q = from cm in MarkersByName.Values
                    orderby cm.MarkerId
                    select cm;


            MarkersById = new SortedList<int, CMData>();

            foreach (CMData cm in MarkersByName.Values)
            {
                MarkersById.Add(cm.MarkerId, cm);
            }
            var idle = MarkersById.Keys.Contains(502);
            var nm = MarkersById.Where(o => o.Key == 502);

            // now output the data into a .H format to be read in
            var sb = new StringBuilder();
            foreach (CMData cm in MarkersById.Values)
            {
                sb.AppendFormat("{{{0,10}, {1,-80},{2},{3}, {4,10} }},\r\n",
                    cm.MarkerId,
                    "_T(\"" + cm.MarkerName + "\")",
                    cm.IsBegin ? 1 : 0,
                    cm.Priority,
                    cm.cmdataMate == null ? 0 : cm.cmdataMate.MarkerId
                    );
            }
            System.IO.File.WriteAllText(@"d:\t.txt", sb.ToString());
        }

        private string FindSubstr(string markerName, string[] suffixes)
        {
            var fRoot = string.Empty;
            var testName = markerName.ToLower();
            foreach (string suffix in suffixes)
            {
                if (testName.EndsWith(suffix))
                {
                    fRoot = markerName.Substring(0, testName.Length - suffix.Length); // preserve case?
                    break;
                }
            }
            return fRoot;
        }

        public CMData NameForNumber(int n)
        {
            CMData res = null;
            if (MarkersById.ContainsKey(n))
            {
                res = MarkersById[n];
            }
            else
            {
                Debug.Assert(false, "couldn't find marker id " + n.ToString());
            }
            return res;
        }
    }

}

//  copy \t.txt VSAssert\CodeMarkerValues2.txt


