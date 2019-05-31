using System;
using System.ComponentModel.Composition;
using System.Diagnostics.CodeAnalysis;
using Prism.CollectionService.Definition;
using Prism.CollectionService.Definition.Snapshot;
using Prism.CollectionService.Extensions.Snapshot;

namespace Prism.CollectionService.Extensions.Snapshot
{
    /// <summary>
    /// Factory to generate new MemSpect snapshot collector.
    /// </summary>
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704")]
    [Export(typeof(ISnapshotServiceFactory))]
    [Name("MemSpect")]
    public class MemSpectCollectorFactory : SnapshotServiceFactory<MemSpectCollector, MemSpectSettings>
    { }
}
