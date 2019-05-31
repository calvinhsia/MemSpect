using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Security.Permissions;
using MemSpect;
using Prism.CollectionService.Definition;
using Prism.CollectionService.Definition.DataStorages;
using Prism.CollectionService.Definition.Snapshot;

namespace Prism.CollectionService.Extensions.Snapshot
{
    /// <summary>
    /// MemSpect collector.
    /// </summary>
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704")]
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA2204")]
    public class MemSpectCollector : SnapshotService<MemSpectSettings>
    {
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="settings">MemspectSettings</param>
        /// <param name="logger">Logger</param>
        public MemSpectCollector(MemSpectSettings settings, IElementLogger logger)
            : base(settings, logger)
        {
            Initialize();
            RecordedSequenceNumbers = new List<string>();
        }

        /// <summary>
        /// Executes the collection, either recording a sequence number of creating a snapshot.
        /// </summary>
        /// <param name="processId">PID for the process to attach to</param>
        /// <param name="outputIdentifier">Output identifier</param>
        /// <returns>Path to the collected data</returns>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Globalization", "CA1303")]
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA2204")]
        [SecurityPermission(SecurityAction.Demand)]
        public override string Execute(int processId, string outputIdentifier)
        {
            // Either the file or directory that was written to during this collection.
            string outputInfo = String.Empty;

            try
            {
                // Check if we're creating a mega snapshot
                if (Settings.CollectMegaSnapshot)
                {
                    outputInfo = CreateMegaSnapshot(processId, outputIdentifier);
                }

                // No harm in always attaching to handle the following cases without needing to 
                // check every available setting and future added settings:
                //      1) A Memspect setting needs to be dynamically modified
                //      2) A Memspect vert/action needs to be executed
                //      3) Need to collect sequence numbers

                // Attempt to attach to the process.
                Attach(processId);

                // If we're not attached, just return.
                if (!IsAttached)
                {
                    return outputInfo;
                }

                // Check if we're collecting a sequence number.
                if (Settings.CollectSequenceNumber)
                {
                    outputInfo = CollectSequenceNumber(processId, outputIdentifier);
                }

                // Check and update each Memspect dynamically configurable setting.
                RefreshClrTrackingState();
                RefreshVirtualAllocTrackingState();
                RefreshHeapAllocTrackingState();
                RefreshTrackingModeState();

                // Check and execute any Memspect verbs/actions.
                if (Settings.ExecuteFreeStackMemory)
                {
                    FreeStackMemoryForAllHeaps();
                }
            }
            finally
            {
                if (IsAttached)
                {
                    // Detach from the process.
                    Detach();
                }
            }

            return outputInfo;
        }

        /// <summary>
        /// Collection of sequence numbers and their corresponding output identifiers
        /// </summary>
        public IList<string> RecordedSequenceNumbers
        {
            get;
            private set;
        }

        #region Private

        /// <summary>
        /// Full path to the file that contains the Memspect sequence numbers.
        /// </summary>
        private string SequenceNumberOutputFile
        {
            get;
            set;
        }

        /// <summary>
        /// Whether or not we are attached to memspect.
        /// </summary>
        private bool IsAttached
        {
            get;
            set;
        }

        /// <summary>
        /// Memspect executable path.
        /// </summary>
        private string MemspectExePath
        {
            get;
            set;
        }

        /// <summary>
        /// Generate mega snapshot file.
        /// </summary>
        /// <param name="processId">Process id.</param>
        /// <param name="outputIdentifier">Output file identifier</param>
        /// <returns>Full path to directory containing the generated mega snapshot.</returns>
        private string CreateMegaSnapshot(int processId, string outputIdentifier)
        {
            // No need to call Attach() since we're using command-line execution to get the snapshot.

            Logger.LogDiagnosticMessage("Memspect - CreateMegaSnapshot() start");

            string previousSymPath = Environment.GetEnvironmentVariable("_NT_SYMBOL_PATH");
            if (!string.IsNullOrEmpty(Settings.SymbolsPath))
            {
                // If a symbol path was specified in the config file, use that
                Environment.SetEnvironmentVariable("_NT_SYMBOL_PATH", Settings.SymbolsPath);
            }
            else if (string.IsNullOrEmpty(previousSymPath))
            {
                // If a symbol path wasn't specified, and there is no current symbol path, use the default one
                Environment.SetEnvironmentVariable("_NT_SYMBOL_PATH", MemSpectSettings.DefaultSymbolPath);
            }
            // Otherwise, use what was already specified.

            // Create the directory that will contain the snapshot
            string outputPath = Path.Combine(Settings.OutputDirectory, outputIdentifier);
            if (!Directory.Exists(outputPath))
            {
                Directory.CreateDirectory(outputPath);
            }

            // There is a MemSpect API for creating snapshots, but it can consume large amounts of memory.  So instead,
            // use the command-line snapshot functionality to avoid OOM exceptions in the PRISM address space.
            ProcessStartInfo psi = new ProcessStartInfo();
            psi.FileName = MemspectExePath;
            psi.Arguments = "/c " + processId.ToString(CultureInfo.InvariantCulture) + " " + outputPath;

            // Snapshots can take a significant amount of time, depending on the number of objects being tracked.
            // Usually, it takes no more than 15-20 minutes. 
            // Max wait defined below have been updated to timeout on 1 1/2 hours instead of 1 hour.
            Process.Start(psi).WaitForExit(1000 * 60 * 90); // milliseconds * seconds * minutes

            // Set the symbol path back to its original value.
            Environment.SetEnvironmentVariable("_NT_SYMBOL_PATH", previousSymPath);

            Logger.LogDiagnosticMessage("Memspect - CreateMegaSnapshot() end");

            return outputPath;
        }

        /// <summary>
        /// Collect sequence number and update sequence number output file.
        /// </summary>
        /// <param name="processId">Process id.</param>
        /// <param name="outputIdentifier">Output file identifier</param>
        /// <returns>Full path to sequence number output file.</returns>
        private string CollectSequenceNumber(int processId, string outputIdentifier)
        {
            // Generate the name for the sequence number output file. This will be written to after every sequence number is collected.
            if (String.IsNullOrEmpty(SequenceNumberOutputFile))
            {
                SequenceNumberOutputFile = Path.Combine(Settings.OutputDirectory, "sequenceNums_" + processId.ToString(CultureInfo.InvariantCulture) + "_" + DateTime.Now.ToString("yyyy_MM_dd_hh_mm", CultureInfo.InvariantCulture) + ".txt");
            }

            // Record and store the last sequence number.
            Logger.LogDiagnosticMessage("Memspect - GetLastSeqNo() start");

            int seqNo = GetLastSeqNo();
            RecordedSequenceNumbers.Add(String.Format(CultureInfo.InvariantCulture, "{0},{1}", seqNo, outputIdentifier));
            Logger.LogDiagnosticMessage(String.Format(CultureInfo.InvariantCulture, "Recorded sequence #{0} for outputIdentifier {1}.", seqNo, outputIdentifier));

            // Write the sequence number and output identifier to the file.
            using (StreamWriter sw = new StreamWriter(SequenceNumberOutputFile, true))
            {
                sw.WriteLine(RecordedSequenceNumbers[RecordedSequenceNumbers.Count - 1]);
            }

            Logger.LogDiagnosticMessage("Memspect - GetLastSeqNo() end");

            return SequenceNumberOutputFile;
        }

        /// <summary>
        /// Attaches to the process.
        /// </summary>
        /// <param name="pid">PID of the process to attach to</param>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Globalization", "CA1303:Do not pass literals as localized parameters", MessageId = "Prism.CollectionService.Definition.DataStorages.IElementLogger.LogWarning(System.String,System.Object[])"), System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Globalization", "CA1303:Do not pass literals as localized parameters", MessageId = "Prism.CollectionService.Definition.DataStorages.IElementLogger.LogDiagnosticMessage(System.String,System.Object[])")]
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA2204")]
        private void Attach(int pid)
        {
            try
            {
                System.Diagnostics.Debug.WriteLine("Memspect - Attach() start");

                // If we're already attached, return
                if (IsAttached)
                {
                    Logger.LogWarning(String.Format(CultureInfo.InvariantCulture, "Already attached to process {0}.", pid));
                    return;
                }

                System.Diagnostics.Debug.WriteLine("Memspect - InitCom() start");

                // Tells Memspect it is running without UI and to not create/access any controls to prevent threading issues.
                Common._IsUnderTest = true;

                // Attach to the process.
                string returnValue = ProcComm.InitComm(new string[] { "0", pid.ToString(CultureInfo.InvariantCulture) });

                // If return value is not empty string, then ProcComm.InitComm was not successful and this should be the 
                // detailed error message. 
                if (!String.IsNullOrEmpty(returnValue))
                {
                    Logger.LogWarning(returnValue);
                    throw new PrismServiceException(returnValue);
                }

                IsAttached = true;

                System.Diagnostics.Debug.WriteLine("Memspect - InitCom() end");
            }
            finally
            {
                System.Diagnostics.Debug.WriteLine("Memspect - Attach() end");
            }
        }

        /// <summary>
        /// Detaches from the process.
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Globalization", "CA1303:Do not pass literals as localized parameters", MessageId = "Prism.CollectionService.Definition.DataStorages.IElementLogger.LogWarning(System.String,System.Object[])"), System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Globalization", "CA1303:Do not pass literals as localized parameters", MessageId = "Prism.CollectionService.Definition.DataStorages.IElementLogger.LogDiagnosticMessage(System.String,System.Object[])")]
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA2204")]
        private void Detach()
        {
            try
            {
                System.Diagnostics.Debug.WriteLine("Memspect - Detach() start");

                // If we aren't attached, don't do anything.
                if (!IsAttached)
                {
                    Logger.LogWarning(String.Format(CultureInfo.InvariantCulture, "Not attached to process."));
                    return;
                }

                System.Diagnostics.Debug.WriteLine("Memspect - CloseComm() start");

                // Detach
                ProcComm.CloseComm();
                IsAttached = false;

                System.Diagnostics.Debug.WriteLine("Memspect - CloseComm() end");
            }
            finally
            {
                System.Diagnostics.Debug.WriteLine("Memspect - Detach() end");
            }
        }

        /// <summary>
        /// Checks and updates CLR tracking state.
        /// </summary>
        private void RefreshClrTrackingState()
        {
            if (IsAttached && Settings.EnableClrTracking != ModificationState.NoModification)
            {
                if (Settings.EnableClrTracking == ModificationState.True)
                {
                    ProcComm.SendMsg(Common.ProcMsgVerb.ClrObjTrk, new int[] { 1 });
                }
                else if (Settings.EnableClrTracking == ModificationState.False)
                {
                    ProcComm.SendMsg(Common.ProcMsgVerb.ClrObjTrk, new int[] { 0 });
                }

                Logger.LogDiagnosticMessage("{0}: {1}", MemSpectSettings.EnableClrTrackingSettingName, Settings.EnableClrTracking);
            }
        }

        /// <summary>
        /// Checks and updates virtual allocation tracking state.
        /// </summary>
        private void RefreshVirtualAllocTrackingState()
        {
            if (IsAttached && Settings.EnableVirtualAllocTracking != ModificationState.NoModification)
            {
                if (Settings.EnableVirtualAllocTracking == ModificationState.True)
                {
                    ; // TODO: this is a stub until we get new Memspect support for this.
                }
                else if (Settings.EnableVirtualAllocTracking == ModificationState.False)
                {
                    ; // TODO: this is a stub until we get new Memspect support for this.
                }

                Logger.LogDiagnosticMessage("{0}: {1}", MemSpectSettings.EnableVirtualAllocTrackingSettingName, Settings.EnableVirtualAllocTracking);
            }
        }

        /// <summary>
        /// Checks and updates heap allocation tracking state.
        /// </summary>
        private void RefreshHeapAllocTrackingState()
        {
            if (IsAttached && Settings.EnableHeapAllocTracking != ModificationState.NoModification)
            {
                if (Settings.EnableHeapAllocTracking == ModificationState.True)
                {
                    ; // TODO: this is a stub until we get new Memspect support for this.
                }
                else if (Settings.EnableHeapAllocTracking == ModificationState.False)
                {
                    ; // TODO: this is a stub until we get new Memspect support for this.
                }

                Logger.LogDiagnosticMessage("{0}: {1}", MemSpectSettings.EnableHeapAllocTrackingSettingName, Settings.EnableHeapAllocTracking);
            }
        }

        /// <summary>
        /// Checks and updates tracking mode state.
        /// </summary>
        private void RefreshTrackingModeState()
        {
            if (IsAttached && Settings.EnableNormalTrackingMode != ModificationState.NoModification)
            {
                if (Settings.EnableNormalTrackingMode == ModificationState.True)
                {
                    Common.SetTrackingMode(Common.TrackingModeEnum.Normal);
                }
                else if (Settings.EnableNormalTrackingMode == ModificationState.False)
                {
                    Common.SetTrackingMode(Common.TrackingModeEnum.Minimal);
                }

                Logger.LogDiagnosticMessage("{0}: {1}", MemSpectSettings.EnableNormalTrackingModeSettingName, Settings.EnableNormalTrackingMode);
            }
        }

        /// <summary>
        /// Frees call stack memory tracked for all heaps.
        /// </summary>
        private void FreeStackMemoryForAllHeaps()
        {
            if (IsAttached)
            {
                int stackFramesFreed = Common.FreeStackMemory(null) * 4;
                Logger.LogDiagnosticMessage("Stack frames freed: {0} bytes", stackFramesFreed);
            }
        }

        /// <summary>
        /// Obtains the last sequence number in the target process.
        /// </summary>
        /// <returns>Returns last sequence number</returns>
        private int GetLastSeqNo()
        {
            if (IsAttached)
            {
                return Common.GetGlobalPassCount();
            }

            return -1;
        }

        /// <summary>
        /// Initialize the Memspect collector.
        /// </summary>
        /// <param name="settings">Profiler settings.</param>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Globalization", "CA1303")]
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA2204")]
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2122")]
        private void Initialize()
        {
            SequenceNumberOutputFile = String.Empty;
            IsAttached = false;
            MemspectExePath = String.Empty;

            Logger.LogDiagnosticMessage("Settings provided to {0} collector:\n{1}: {2}\n{3}: {4}\n{5}: {6}\n{7}: {8}\n{9}: {10}\n{11}: {12}\n{13}: {14}\n{15}: {16}\n{17}: {18}\n{19}: {20}\n{21}: {22}",
                Settings.UniqueName,
                MemSpectSettings.AutoInstallSettingName, Settings.AutoInstall,
                MemSpectSettings.OutputDirectorySettingName, Settings.OutputDirectory,
                MemSpectSettings.OutputFileStartTagSettingName, Settings.OutputFileStartTag,
                MemSpectSettings.ToolDirectorySettingName, Settings.ToolDirectory,
                MemSpectSettings.CollectMegaSnapshotSettingName, Settings.CollectMegaSnapshot,
                MemSpectSettings.CollectSeqNoSettingName, Settings.CollectSequenceNumber,
                MemSpectSettings.EnableClrTrackingSettingName, Settings.EnableClrTracking,
                MemSpectSettings.EnableVirtualAllocTrackingSettingName, Settings.EnableVirtualAllocTracking,
                MemSpectSettings.EnableHeapAllocTrackingSettingName, Settings.EnableHeapAllocTracking,
                MemSpectSettings.EnableNormalTrackingModeSettingName, Settings.EnableNormalTrackingMode,
                MemSpectSettings.ExecuteFreeStackMemorySettingName, Settings.ExecuteFreeStackMemory);

            // Memspect.exe lives next to PrismMonistorHost.exe.
            string prismPath = Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location);
            MemspectExePath = Path.Combine(prismPath, MemSpectSettings.MemspectExeName);

            // If Memspect.exe doesn't exist, throw now instead of waiting for a slow run to complete before trying
            // to take a snapshot and then failing.
            if (!File.Exists(MemspectExePath))
            {
                throw new FileNotFoundException(MemspectExePath);
            }

            // Make sure MemSpectDll.dll is in the correct place. For a more detailed explaination of why this happens,
            // see the end of the file.
            string memspectDllTargetPath = Environment.GetEnvironmentVariable("COR_PROFILER_PATH");
            string memspectDllBackupPath = Path.ChangeExtension(memspectDllTargetPath, "off");

            // The first collector to initialize will do the moving. Subsequent ones should just bypass.
            if (File.Exists(memspectDllBackupPath))
            {
                if (!File.Exists(memspectDllTargetPath))
                {
                    File.Move(memspectDllBackupPath, memspectDllTargetPath);
                }
            }
            else if (!File.Exists(memspectDllTargetPath))
            {
                Logger.LogWarning(String.Format(CultureInfo.InvariantCulture, "Failed to find either {0} or {1}.  Memspect may not work properly.",
                                  memspectDllBackupPath,
                                  memspectDllTargetPath));
            }
        }

        /// <summary>
        /// "Turns off" memspect by moving the MemSpectDll.dll to a different file name to avoid it being loaded 
        /// </summary>
        /// <param name="disposing">disposing</param>
        protected override void Dispose(bool disposing)
        {
            base.Dispose(disposing);

            // Move MemSpectDll.dll back to MemSpectDll.off. For a more detailed explaination of why this happens,
            // see the end of the file.
            string memspectDllTargetPath = Environment.GetEnvironmentVariable("COR_PROFILER_PATH");
            string memspectDllBackupPath = Path.ChangeExtension(memspectDllTargetPath, "off");

            // The first collector to dispose will do the moving.  subsequent ones should just bypass.
            if (File.Exists(memspectDllTargetPath))
            {
                if (!File.Exists(memspectDllBackupPath))
                {
                    File.Move(memspectDllTargetPath, memspectDllBackupPath);
                }
            }
        }

        #endregion
    }
}

/* Detailed explaination for why MemSpectDll.dll is treated the way it is */
//Since the test is what launches VS, it is responsible for launching VS under memspect.  From the Memspect standpoint, this can happen in 1 of two ways:
//1.	The test specifically uses a separate executable to start VS under Memspect.  This is not advised since each test framework would have to be updated to support this.
//2.	The test executes in an environment pre-configured so that VS will automatically start under Memspect.  This is done by having 3 COR_PROFILER environment variables set.

//If we avoid option 1 above, the only other way of launching VS is to ensure the test process has the environment variables set, but this difficult for the following:
//-	A test is sent to the machine and the testdriver starts running.  It has no idea about PRISM or Memspect, so it can’t set the environment variables (unless every test driver is updated to handle this, but that’s the same problem as option 1 above).
//-	PRISM is launched and inherits its environment from the testdriver.  PRISM knows it needs to run under Memspect now and can set the environment variables, but they will only be set in its environment and any child processes it starts.
//-	The test is launched and it inherits its environment from the testdriver, so it won’t have the appropriate environment variables set.
//-	As far as I can tell, it is impossible for PRISM to alter the environment of its parent process or any other running environment, so it can’t update the test process to have these environment variables set without falling back into the trap of option 1.
//-	So, the only way to ensure that the test process has those environment variables is for them to be set before the test process is ever run, which means having them set globally on the machine, which can lead to the other problem of every VS launch being under Memspect.

