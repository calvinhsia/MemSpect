This project intercepts Windows API calls such as HeapAlloc and collects call stacks. Upon free, the stacks are freed.
The UI can then show what memory is still allocated.
GC Heap is monitored for objects that survived, moved and collected.
Used successfully to find hundreds of leaks and memory inefficiencies in Visual Studio.

