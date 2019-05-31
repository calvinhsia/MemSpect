using System;
using System.Diagnostics.CodeAnalysis;
using Prism.CollectionService.Definition.Elements;

namespace Prism.CollectionService.Extensions.Snapshot
{
    /// <summary>
    /// Class to hold all MemSpect settings.
    /// </summary>
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704")]
    public class MemSpectSettings : ServiceSettings
    {
        /// <summary>
        /// Default values.
        /// </summary>
        
        public const string CollectSeqNoSettingName = "CollectSequenceNumbers";
        public const string CollectMegaSnapshotSettingName = "CollectMegaSnapshot";
        public const string SymbolPathSettingName = "SymbolsPath";
        public const string DefaultSymbolPath = "SRV*%SYSTEMDRIVE%\\symcache*\\\\ddrps\\symbols;";

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704")]
        public const string MemspectExeName = "MemSpect.exe";

        /// <summary>
        /// Default constructor
        /// </summary>
        public MemSpectSettings()
            :this(new SettingsContainer())
        { }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="settingsContainer">Settings container</param>
        public MemSpectSettings(SettingsContainer settingsContainer)
            : base(settingsContainer)
        {
            //by default, turn off both collectors.
            CollectSequenceNumber = false;
            CollectMegaSnapshot = false;

            if (settingsContainer == null)
            {
                throw new ArgumentNullException("settingsContainer");
            }

            if (settingsContainer.SettingExist(CollectSeqNoSettingName))
            {
                bool temp = false;
                if (bool.TryParse(settingsContainer.GetSettingValue<string>(CollectSeqNoSettingName), out temp))
                {
                    CollectSequenceNumber = temp;
                }
            }

            if (settingsContainer.SettingExist(CollectMegaSnapshotSettingName))
            {
                bool temp = false;
                if (bool.TryParse(settingsContainer.GetSettingValue<string>(CollectMegaSnapshotSettingName), out temp))
                {
                    CollectMegaSnapshot = temp;
                }
            }

            SymbolsPath = string.Empty;
            if (settingsContainer.SettingExist(SymbolPathSettingName))
            {
                SymbolsPath = settingsContainer.GetSettingValue<string>(SymbolPathSettingName);
            }
        }
        
        /// <summary>
        /// Indicates whether or not to collect sequence numbers
        /// </summary>
        public bool CollectSequenceNumber { get; private set; }

        /// <summary>
        /// Indicates whether or not to collect snapshots.
        /// </summary>
        public bool CollectMegaSnapshot { get; private set; }

        /// <summary>
        /// Path for _NT_SYMBOL_PATH
        /// </summary>
        public string SymbolsPath { get; private set; }
    }
}
