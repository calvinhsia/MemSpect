



C:\MemSpect\MemSpectBase>ildasm External\TraceEvent.dll -out:t.il

//////////doesn't work: sn -p MemSpectKey.pfx key.snk
/// generted new key
/// sn -k MemSpectKey.snk
/// sn -Tp MemSpectKey.snk   to get public key for InternalsVisibleTo

ilasm t.il /dll /key=MemSpectKey.snk /output=TraceEvent.dll


Update History

V10.2 Release 5/10/10

Generic class, arrays for class names
window not showintaskbar for snapshots out of proc, Owner
Fix possible race condition in freezethreads
Remove dwords in memdump: bytes and chars for narrower display
Add special case: NrlsAloc Size code
Added CodeMarker Support
v10.3
Added Heap Density report
6/7/10
added filter lo/hi context menu, intptr,heapdensity/heapwalkmap
6/8/10
JustDevenv = 1
6/9/10
I’ve fixed Excel export: much better.
Fixed (NULL).Add: wasn’t calling IMetaDataImport->GetTypeDefProps 
    mscorlib.dll!MemberInfoCache`1.Insert
    mscorlib.dll!MemberInfoCache`1.Populate
    mscorlib.dll!MemberInfoCache`1.GetMemberList
6/10/10
Reworked GCRoots to be separate window, display refactoring: MultiWin->MultiTab
6/14/10
made Pivot cross-heap
Added verb GetClrObjDump to dump entire CLR obj graph
6/15
fix string "??" edge case of GC
added Gen0, Gen1, etc. to CLRObj view
Version Checking
Duplicates in any BrowMem, including subsnap

6/22
Reduce mem use Header/Trailer to 0 for retail heaps
Use HashSet rather than wasteful hashtable
wrap all heaps, including ones created prior to my code loading (ProcessHeap, CRTHeap)
6/23
resolve symbols in child process!


Pruned to migrate to GitHub:
git filter-branch --prune-empty --index-filter "git rm -rf --cached --ignore-unmatch MemSpect/Snaps" --prune-empty --tag-name-filter cat -- --all
