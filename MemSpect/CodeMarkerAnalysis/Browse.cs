using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows.Controls;
using System.Collections;
using System.Reflection;
using System.Windows;
using System.Windows.Media;
using System.ComponentModel;
using System.Windows.Data;
using System.Diagnostics;
using System.Windows.Input;

namespace CodeMarkerAnalysis
{
    public class BrowsePanel : DockPanel
    {
        public BrowseList _BrowseList { get; private set; }
        internal int[] _colWidths;
        public IEnumerable _query;
        public BrowsePanel(IEnumerable query, int[] colWidths = null)
        {
            try
            {
                _query = query;
                _colWidths = colWidths;
                _BrowseList = new BrowseList(query, this);
                var listFilter = new ListFilter(_BrowseList);
                this.Children.Add(listFilter);
                DockPanel.SetDock(listFilter, Dock.Top);

                this.Children.Add(_BrowseList);
            }
            catch (Exception ex)
            {
                this.Children.Add(new TextBlock() { Text = ex.ToString() });
            }
        }
        public ContextMenu contextMenu
        {
            get
            {
                if (_BrowseList.ContextMenu == null)
                {
                    _BrowseList.ContextMenu = new ContextMenu();
                }
                return _BrowseList.ContextMenu;
            }
        }

    }
    internal class ListFilter : DockPanel
    {
        Button _btnApply = new Button() { Content = "Apply" };
        TextBox _txtFilter = new TextBox { Width = 200 };
        TextBlock _txtStatus = new TextBlock();
        BrowseList _browse;
        internal ListFilter(BrowseList browse)
        {
            _browse = browse;
            var spFilter = new StackPanel { Orientation = Orientation.Horizontal, HorizontalAlignment = System.Windows.HorizontalAlignment.Right };
            spFilter.Children.Add(_txtStatus);
            spFilter.Children.Add(new Label { Content = "StringFilter", ToolTip = "Case insenSitive search in character fields. A filter works on current set" });
            spFilter.Children.Add(_txtFilter);
            spFilter.Children.Add(_btnApply);
            this.Children.Add(spFilter);
            _txtFilter.KeyUp += (o, e) =>
            {
                if (e.Key == System.Windows.Input.Key.Enter)
                {
                    on_BtnApply_Click(o, e);
                }
            };
            RefreshFilterStat();
        }
        void on_BtnApply_Click(object o, RoutedEventArgs e)
        {
            try
            {
                this.Cursor = Cursors.Wait;
                var filtText = _txtFilter.Text.Trim().ToLower();
                if (string.IsNullOrEmpty(filtText))
                {
                    _browse.Items.Filter = null;
                }
                else
                {
                    _browse.Items.Filter = itm =>
                    {
                        var fIsMatch = false;
                        var props = TypeDescriptor.GetProperties(itm);
                        foreach (PropertyDescriptor prop in props)
                        {
                            var propName = prop.Name;
                            if (propName.StartsWith("_x"))
                            {
                                //var dict = ObjectFromXml.GetProps(propName, itm);
                                //foreach (string strVal in dict.Values)
                                //{
                                //    if (strVal.ToLower().Contains(filtText))
                                //    {
                                //        fIsMatch = true;
                                //        break;
                                //    }
                                //}
                                //if (fIsMatch)
                                //{
                                //    break;
                                //}
                            }
                            else
                            {
                                var str = prop.GetValue(itm) as string;
                                if (!string.IsNullOrEmpty(str))
                                {
                                    if (str.ToLower().Contains(filtText))
                                    {
                                        fIsMatch = true;
                                        break;
                                    }
                                }
                            }
                        }
                        return fIsMatch;
                    };
                }
                RefreshFilterStat();
                this.Cursor = Cursors.Arrow;

            }
            catch (Exception ex)
            {
                _txtStatus.Text = ex.ToString();

            }
        }
        void RefreshFilterStat()
        {
            _txtStatus.Text = string.Format("# items = {0:n0} ", _browse.Items.Count);
        }
    }
    public class BrowseList : ListView
    {
        int _nColIndex = 0;
        int[] _colWidths;
        public BrowseList(IEnumerable query, BrowsePanel browPanel)
        {
            this._colWidths = browPanel._colWidths;
            this.Margin = new System.Windows.Thickness(8);
            this.ItemsSource = query;
            var gridvw = new GridView();
            this.View = gridvw;
            var ienum = query.GetType().GetInterface(typeof(IEnumerable<>).FullName);

            var members = ienum.GetGenericArguments()[0].GetMembers().Where(m => m.MemberType == System.Reflection.MemberTypes.Property);
            foreach (var mbr in members)
            {
                //if (mbr.DeclaringType == typeof(EntityObject)) // if using Entity framework, filter out EntityKey, etc.
                //{
                //    continue;
                //}
                var dataType = mbr as System.Reflection.PropertyInfo;
                var colType = dataType.PropertyType.Name;
                GridViewColumn gridcol = null;

                if (mbr.Name.StartsWith("_x")) // == "Request" || mbr.Name == "Reply" || mbr.Name == "xmlData")
                {
                    var methodName = string.Format("get_{0}", mbr.Name); // like "get_Request" or "get_Response"
                    var enumerator = query.GetEnumerator();
                    var fLoopDone = false;
                    while (!fLoopDone)
                    {
                        if (enumerator.MoveNext())
                        {
                            var currentRecord = enumerator.Current;
                            var currentRecType = currentRecord.GetType();
                            var msgObj = currentRecType.InvokeMember(methodName, BindingFlags.InvokeMethod, null, currentRecord, null);
                            if (msgObj != null)
                            {
                                var msgObjType = msgObj.GetType();
                                var msgObjTypeProps = msgObjType.GetProperties();
                                foreach (var prop in msgObjTypeProps)
                                {
                                    AddAColumn(gridvw, prop.Name, mbr.Name);
                                }
                                fLoopDone = true;
                            }
                        }
                        else
                        {
                            fLoopDone = true;
                        }
                    }
                }
                else
                {
                    switch (colType)
                    {
                        case "XmlReader":
                            {

                                gridcol = new GridViewColumn();
                                var colheader = new GridViewColumnHeader() { Content = mbr.Name };
                                gridcol.Header = colheader;
                                colheader.Click += new RoutedEventHandler(colheader_Click);
                                gridvw.Columns.Add(gridcol);
                            }
                            break;
                        default:
                            {
                                AddAColumn(gridvw, mbr.Name);
                            }
                            break;
                    }
                }

            }
            // now create a style for the items
            var style = new Style(typeof(ListViewItem));

            style.Setters.Add(new Setter(ForegroundProperty, Brushes.Blue));

            var trig = new Trigger()
            {
                Property = IsSelectedProperty,// if Selected, use a different color
                Value = true
            };
            trig.Setters.Add(new Setter(ForegroundProperty, Brushes.Red));
            trig.Setters.Add(new Setter(BackgroundProperty, Brushes.Cyan));
            style.Triggers.Add(trig);

            this.ItemContainerStyle = style;
            this.ContextMenu = new ContextMenu();
            this.ContextMenu.AddMenuItem(on_CtxMenu, "_Copy", "Copy selected items to clipboard");
            this.ContextMenu.AddMenuItem(on_CtxMenu, "Export to E_xcel", "Create a temp file of selected items in CSV format");
            this.ContextMenu.AddMenuItem(on_CtxMenu, "Export to _Notepad", "Create a temp file of selected items in TXT format");

        }

        private void on_CtxMenu(object s, RoutedEventArgs e)
        {
            var mitem = e.OriginalSource as MenuItem;
            var vrb = mitem.Header.ToString();
            var fCSV = vrb.Contains("xcel");
            switch (vrb)
            {
                case "_Copy":
                    System.Windows.Clipboard.SetData(DataFormats.Text, DumpListToString(fCSV));
                    break;
                case "_Diff 2 selected items":
                    //[HKEY_CURRENT_USER\Software\Microsoft\VisualStudio\10.0\TeamFoundation\SourceControl\DiffTools\.*\Compare]
                    //"Command"="c:\\bin\\bc2.exe"
                    //"Arguments"="%1 %2"
                    if (this.SelectedItems.Count != 2)
                    {
                        System.Windows.Forms.MessageBox.Show(string.Format("Must select exactly 2 items, not {0}", this.SelectedItems.Count));
                    }
                    else
                    {
                        // 1st try dev10, then dev11
                        var cmpCommand = Microsoft.Win32.Registry.GetValue(@"HKEY_CURRENT_USER\Software\Microsoft\VisualStudio\10.0\TeamFoundation\SourceControl\DiffTools\.*\Compare", "Command", "Windiff.exe") as string;
                        var cmpCommand2 = (string)Microsoft.Win32.Registry.GetValue(@"HKEY_CURRENT_USER\Software\Microsoft\VisualStudio\11.0\TeamFoundation\SourceControl\DiffTools\.*\Compare", "Command", cmpCommand) as string;
                        if (string.IsNullOrEmpty(cmpCommand2))
                        {
                            cmpCommand2 = cmpCommand;
                        }
                        object[] saveSel = new object[2];
                        this.SelectedItems.CopyTo(saveSel, 0);
                        this.SelectedItems.RemoveAt(0);
                        var file1 = WriteOutputToTempFile(DumpListToString(fCSV, this.SelectedItems), fStartIt: false);
                        this.SelectedItems.Clear();
                        this.SelectedItems.Add(saveSel[0]);
                        var file2 = WriteOutputToTempFile(DumpListToString(fCSV, this.SelectedItems), fStartIt: false);
                        foreach (var sel in saveSel)
                        {
                            this.SelectedItems.Add(sel);
                        }
                        Process.Start(cmpCommand2, string.Format(@"""{0}"" ""{1}""  ", file1, file2));
                    }

                    break;
                default:
                    WriteOutputToTempFile(DumpListToString(fCSV), fCSV ? "csv" : "txt");
                    break;
            }
        }

        private string DumpListToString(bool fCSV, IEnumerable srcColl = null)
        {
            var sb = new StringBuilder();
            var gridview = this.View as GridView;
            var isFirst = true;
            var defView = CollectionViewSource.GetDefaultView(this.ItemsSource);
            var colNames = new List<string>();
            var priorPadding = 0;
            int[] widths = new int[gridview.Columns.Count];
            var colndx = 0;
            foreach (var col in gridview.Columns)
            {
                if (isFirst)
                {
                    isFirst = false;
                }
                else
                {
                    if (fCSV)
                    {
                        sb.Append(",");
                    }
                    else
                    {
                        for (var i = 0; i < priorPadding; i++)
                        {
                            sb.Append(" ");
                        }
                    }
                }
                var txt = (col.Header as GridViewColumnHeader).Content.ToString();
                colNames.Add(txt);
                widths[colndx] = (int)(col.ActualWidth / 6);
                priorPadding = widths[colndx] - txt.Length;
                sb.Append(txt);
                colndx++;
            }
            sb.AppendLine();
            if (srcColl == null)
            {
                if (this.SelectedItems.Count > 0)
                {
                    srcColl = this.SelectedItems;
                }
                else
                {
                    srcColl = this.Items;
                }
            }
            Action<string> doit = strval =>
            {
                if (isFirst)
                {
                    isFirst = false;
                }
                else
                {
                    if (fCSV)
                    {
                        sb.Append(",");
                    }
                    else
                    {
                        for (var i = 0; i < priorPadding; i++)
                        {
                            sb.Append(" ");
                        }
                    }
                }
                if (!string.IsNullOrEmpty(strval))
                {
                    strval = strval.ToString();
                    if (fCSV)
                    {
                        if (strval.Contains(","))
                        {
                            strval = "\"" + strval + "\"";
                        }
                    }
                    sb.Append(strval);
                }
                else
                {
                    strval = string.Empty;
                }
                if (colndx < widths.Length)
                {
                    priorPadding = widths[colndx] - strval.Length;
                }
                colndx++;
            };

            foreach (var row in srcColl)
            {
                isFirst = true;
                priorPadding = 0;

                for (colndx = 0; colndx < colNames.Count; )
                {
                    var colName = colNames[colndx];

                    //if (colName.Contains(".")) // could it be a dynobj? like "Request.username"
                    //{
                    //    var propName = "_x" + colName.Substring(0, colName.IndexOf(".")); // like "_xRequest
                    //    var dict = ObjectFromXml.GetProps(propName, row);
                    //    foreach (var val in dict.Values)
                    //    {
                    //        doit(val);
                    //    }
                    //}
                    var typeDescProp = TypeDescriptor.GetProperties(row)[colName];
                    if (typeDescProp != null)
                    {
                        var rawval = typeDescProp.GetValue(row);
                        if (rawval == null)
                        {
                            doit(string.Empty);
                        }
                        else
                        {
                            var val = rawval.ToString();
                            doit(val);
                        }
                    }

                }
                sb.AppendLine();
            }
            return sb.ToString();
        }

        private string WriteOutputToTempFile(string strToOutput, string fExt = "txt", bool fStartIt = true)
        {
            var tmpFileName = System.IO.Path.GetTempFileName(); //"C:\Users\calvinh\AppData\Local\Temp\tmp8509.tmp"
            System.IO.File.WriteAllText(tmpFileName, strToOutput, new UnicodeEncoding(bigEndian:false, byteOrderMark:true));
            var filename = System.IO.Path.ChangeExtension(tmpFileName, fExt);

            System.IO.File.Move(tmpFileName, filename); // rename
            if (fStartIt)
            {
                Process.Start(filename);
            }
            return tmpFileName;
        }

        GridViewColumn AddAColumn(GridView gridvw, string bindingName, string bindingObjectName = null)
        {
            var gridcol = new GridViewColumn();

            // now we make a dataTemplate with a Stackpanel containing a TextBlock
            // The template must create many instances, so factories are used.
            var dataTemplate = new DataTemplate();
            gridcol.CellTemplate = dataTemplate;
            var stackPanelFactory = new FrameworkElementFactory(typeof(StackPanel));
            stackPanelFactory.SetValue(StackPanel.OrientationProperty, Orientation.Horizontal);

            var txtBlkFactory = new FrameworkElementFactory(typeof(TextBlock));
            var textBinder = new Binding(bindingName)
            {
                Converter = new MyValueConverter() // truncate things that are too long, add commas for numbers
            };

            var toolTipBinder = new Binding(bindingName);
            var colheader = new GridViewColumnHeader() { Content = bindingName, Tag = bindingName };

            if (!string.IsNullOrEmpty(bindingObjectName))
            {
                textBinder.Path = new PropertyPath(bindingObjectName + "." + bindingName);
                colheader.Tag = textBinder.Path.Path; //for sorting
                if (bindingObjectName.StartsWith("_x"))
                {
                    colheader.Content = colheader.Tag.ToString().Substring(2);
                }
                else
                {
                    colheader.Content = colheader.Tag;
                }

                toolTipBinder.Path = textBinder.Path;

            }
            txtBlkFactory.SetBinding(TextBlock.TextProperty, textBinder);

            stackPanelFactory.AppendChild(txtBlkFactory);
            txtBlkFactory.SetBinding(TextBlock.ToolTipProperty, toolTipBinder); // the tip will have the non-truncated content

            //txtBlkFactory.SetValue(TextBlock.FontFamilyProperty, new FontFamily("courier new"));
            //txtBlkFactory.SetValue(TextBlock.FontSizeProperty, 10.0);

            dataTemplate.VisualTree = stackPanelFactory;


            gridcol.Header = colheader;
            colheader.Click += new RoutedEventHandler(colheader_Click);
            gridvw.Columns.Add(gridcol);

            if (_colWidths != null)
            {
                if (_nColIndex < _colWidths.Length)
                {
                    gridcol.Width = _colWidths[_nColIndex];
                }
                _nColIndex++;
            }
            return gridcol;
        }
        private ListSortDirection _LastSortDir = ListSortDirection.Ascending;
        private GridViewColumnHeader _LastHeaderClicked = null;
        void colheader_Click(object sender, RoutedEventArgs e)
        {
            GridViewColumnHeader gvh = sender as GridViewColumnHeader;
            if (gvh != null)
            {
                var dir = ListSortDirection.Ascending;
                if (gvh == _LastHeaderClicked) // if clicking on already sorted col, reverse dir
                {
                    dir = 1 - _LastSortDir;
                }
                try
                {

                    var dataView = CollectionViewSource.GetDefaultView(this.ItemsSource);

                    dataView.SortDescriptions.Clear();
                    var sortDesc = new SortDescription(gvh.Tag.ToString(), dir);
                    dataView.SortDescriptions.Add(sortDesc);
                    dataView.Refresh();
                    if (_LastHeaderClicked != null)
                    {
                        _LastHeaderClicked.Column.HeaderTemplate = null; // clear arrow of prior column
                    }
                    SetHeaderTemplate(gvh);
                    _LastHeaderClicked = gvh;
                    _LastSortDir = dir;
                }
                catch (Exception ex)
                {
                    System.Diagnostics.Trace.WriteLine(ex.Message);
                    // some types aren't sortable
                }
            }
        }

        void SetHeaderTemplate(GridViewColumnHeader gvh)
        {
            // now we'll create a header template that will show a little Up or Down indicator
            var hdrTemplate = new DataTemplate();
            var dockPanelFactory = new FrameworkElementFactory(typeof(DockPanel));
            var textBlockFactory = new FrameworkElementFactory(typeof(TextBlock));
            var binder = new Binding();
            binder.Source = gvh.Content; // the column name
            textBlockFactory.SetBinding(TextBlock.TextProperty, binder);
            textBlockFactory.SetValue(TextBlock.HorizontalAlignmentProperty, HorizontalAlignment.Center);
            dockPanelFactory.AppendChild(textBlockFactory);

            // a lot of code for a little arrow
            var pathFactory = new FrameworkElementFactory(typeof(System.Windows.Shapes.Path));
            pathFactory.SetValue(System.Windows.Shapes.Path.FillProperty, Brushes.DarkGray);
            var pathGeometry = new PathGeometry();
            pathGeometry.Figures = new PathFigureCollection();
            var pathFigure = new PathFigure();
            pathFigure.Segments = new PathSegmentCollection();
            if (_LastSortDir != ListSortDirection.Ascending)
            {//"M 4,4 L 12,4 L 8,2"
                pathFigure.StartPoint = new Point(4, 4);
                pathFigure.Segments.Add(new LineSegment() { Point = new Point(12, 4) });
                pathFigure.Segments.Add(new LineSegment() { Point = new Point(8, 2) });
            }
            else
            {//"M 4,2 L 8,4 L 12,2"
                pathFigure.StartPoint = new Point(4, 2);
                pathFigure.Segments.Add(new LineSegment() { Point = new Point(8, 4) });
                pathFigure.Segments.Add(new LineSegment() { Point = new Point(12, 2) });
            }
            pathGeometry.Figures.Add(pathFigure);
            pathFactory.SetValue(System.Windows.Shapes.Path.DataProperty, pathGeometry);

            dockPanelFactory.AppendChild(pathFactory);
            hdrTemplate.VisualTree = dockPanelFactory;

            gvh.Column.HeaderTemplate = hdrTemplate;
        }
    }

    public class MyValueConverter : IValueConverter
    {
        private const int maxwidth = 17000;
        public object Convert(object value, Type targetType, object parameter, System.Globalization.CultureInfo culture)
        {
            if (null != value)
            {
                Type type = value.GetType();
                //trim len of long strings. Doesn't work if type has ToString() override
                if (type == typeof(string))
                {
                    var str = value.ToString().Trim();
                    var ndx = str.IndexOfAny(new[] { '\r', '\n' });
                    var lenlimit = maxwidth;
                    if (ndx >= 0)
                    {
                        lenlimit = ndx - 1;
                    }
                    if (ndx >= 0 || str.Length > lenlimit)
                    {
                        value = str.Substring(0, lenlimit);
                    }
                    else
                    {
                        value = str;
                    }
                }
                else if (type == typeof(Int32))
                {
                    //                    value = ((int)value).ToString("n0"); // Add commas, like 1,000,000
                }
                else if (type == typeof(Int64))
                {
                    value = ((Int64)value).ToString("n0"); // Add commas, like 1,000,000
                }
                else if (type == typeof(double))
                {
                    value = ((double)value).ToString("n2"); // Add commas, like 1,000,000
                }
                //else if (type.Name.StartsWith("XmlObj"))
                //{
                //    value = value.ToString();
                //    //                    value = type.InvokeMember("get_value", BindingFlags.InvokeMethod, null, value, null);
                //}
            }
            return value;
        }

        public object ConvertBack(object value, Type targetType, object parameter, System.Globalization.CultureInfo culture)
        {
            throw new NotImplementedException();
        }

    }

    public static class MyStatics
    {
        public static T GetAncestor<T>(DependencyObject element) where T : DependencyObject
        {
            while (element != null && !(element is T))
            {
                element = VisualTreeHelper.GetParent(element);
            }
            return (T)element;
        }

        public static MenuItem AddMenuItem(this ContextMenu menu, RoutedEventHandler handler, string menuItemContent, string tooltip, int InsertPos = -1)
        {
            var newItem = new MenuItem()
            {
                Header = menuItemContent,
                ToolTip = tooltip
            };
            newItem.Click += handler;
            if (InsertPos == -1)
            {
                menu.Items.Add(newItem);
            }
            else
            {
                menu.Items.Insert(InsertPos, newItem);
            }
            return newItem;
        }
    }

}

