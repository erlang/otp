Delayed Dealloc
===============

Problem
-------

An easy way to handle memory allocation in a multi-threaded
environment is to protect the memory allocator with a global lock
which threads performing memory allocations or deallocations have to
have locked during the whole operation. This solution of course scales
very poorly, due to heavy lock contention. An improved solution of
this scheme is to use multiple thread specific instances of such an
allocator. That is, each thread allocates in its own allocator
instance which is protected by a lock. In the general case references
to memory need to be passed between threads. In the case where a
thread that needs to deallocate memory that originates from another
threads allocator instance a lock conflict is possible. In a system as
the Erlang VM where memory allocation/deallocation is frequent and
references to memory also are passed around between threads this
solution will also scale poorly due to lock contention.

Functionality Used to Address This problem
-----------------------------------------

In order to reduce contention due to locking of allocator instances we
introduced completely lock free instances tied to each scheduler
thread, and an extra locked instance for other threads. The scheduler
threads in the system is expected to do the major part of the
work. Other threads may still be needed but should not perform any
major and/or time critical work. The limited amount of contention that
appears on the locked allocator instance can more or less be
disregarded.

Since we still need to be able to pass references to memory between
scheduler threads we need some way to manage this. An allocator
instance belonging to one scheduler thread is only allowed to be
manipulated by that scheduler thread. When other threads need to
deallocate memory originating from a foreign allocator instance, they
only pass the memory block to a "message box" containing deallocation
jobs attached to the originating allocator instance. When a scheduler
thread detects such deallocation job it performs the actual
deallocation.

The "message box" is implemented using a lock free single linked list
through the memory blocks to deallocate. The order of the elements in
this list is not important. Insertion of new free blocks will be made
somewhere near the end of this list. Requiring that the new blocks
need to be inserted at the end would cause unnecessary contention when
large amount of memory blocks are inserted simultaneous by multiple
threads.

The data structure referring to this single linked list cover two cache
lines. One cache line containing information about the head of the
list, and one cache line containing information about the tail of the
list. This in order to reduce cache line ping ponging of this data
structure. The head of the list will only be manipulated by the thread
owning the allocator instance, and the tail will be manipulated by
other threads inserting deallocation jobs.

### Tail ###

In the tail part of the data structure we find a pointer to the last
element of the list, or at least something that is near the end of the
list. In the uncontended case it will point to the end of the list,
but when simultaneous insert operations are performed it will point to
something near the end of the list.

When inserting an element one will try to write a pointer to the new
element in the next pointer of the element pointed to by the last
pointer. This is done using an atomic compare and swap that expects
the next pointer to be `NULL`. If this succeeds the thread performing
this operation moves the last pointer to point to the newly inserted
element.

If the atomic compare and swap described above failed, the last
pointer didn't point to the last element. In this case we need to
insert the new element somewhere between the element that the last
pointer pointed to and the actual last element. If we do it this way
the last pointer will eventually end up at the last element when
threads stop adding new elements. When trying to insert somewhere near
the end and failing to do so, the inserting thread sometimes moves to
the next element and sometimes tries with the same element again. This
in order to spread the inserted elements during heavy contention. That
is, we try to spread the modifications of memory to different
locations instead of letting all threads continue to try to modify the
same location in memory.

### Head ###

The head contains pointers to beginning of the list (`head.first`), and
to the first block which other threads may refer to
(`head.unref_end`). Blocks between these pointers are only refered to
by the head part of the data structure which is only used by the
thread owning the allocator instance. When these two pointers are not
equal the thread owning the allocator instance deallocate block after
block until `head.first` reach `head.unref_end`.

We of course periodically need to move the `head.unref_end` closer to
the end in order to be able to continue deallocating memory
blocks. Since all threads inserting new elements in the linked list
will enter the list using the last pointer we can use this
knowledge. If we call `erts_thr_progress_later()` and wait until we
have reached that thread progress we know that no managed threads can
refer the elements up to the element pointed to by the last pointer at
the time when we called `erts_thr_progress_later()`. This since, all
managed threads must have left the code implementing this at least
once, and they always enters into the list via the last pointer. The
`tail.next` field contains information about next `head.unref_end`
pointer and thread progress that needs to be reached before we can
move `head.unref_end`.

Unfortunately not only threads managed by the thread progress
functionality may insert memory blocks. Other threads also needs to be
taken care of. Other threads will not be as frequent users of this
functionality as managed threads, so using a less efficient scheme for
them is not that big of a problem. In order to handle unmanaged
threads we use two reference counters. When an unmanaged thread enters
this implementation it increments the reference counter currently
used, and when it leaves this implementation it decrements the same
reference counter. When the consumer thread calls
`erts_thr_progress_later()` in order to determine when it is safe to
move `head.unref_end`, it also swaps reference counters for unmanaged
threads. The previous current represents outstanding references from
the time up to this point. The new current represents future reference
following this point. When the consumer thread detects that we have
both reached the desired thread progress and when the previous current
reference counter reach zero it is safe to move the `head.unref_end`.

The reason for using two reference counters is that we need to know
that the reference counter eventually will reach zero. If we only used
one reference counter it would potentially be held above zero for ever
by different unmanaged threads.

### Empty List ###

If no new memory blocks are inserted into the list, it should
eventually be emptied. All pointers to the list however expect to
always point to something. This is solved by inserting an empty
"marker" element, which only has to purpose of being there in the
absense of other elements. That is when the list is empty it only
contains this "marker" element.

### Contention ###

When elements are continuously inserted by threads not owning the
allocator instance, the thread owning the allocator instance will be
able to work more or less undisturbed by other threads at the head end
of the list. At the tail end large amounts of simultaneous inserts may
cause contention, but we reduce such contention by spreading inserts
of new elements near the end instead of requiring all new elements to
be inserted at the end.

### Schedulers and The Locked Allocator Instance ###

Also the locked allocator instance for use by non-scheduler threads
have a message box for deallocation jobs just as all the other
allocator instances. The reason for this is that other threads may
allocate memory pass it to a scheduler that then needs to deallocate
it. We do not want the scheduler to have to wait for the lock on this
locked instance. Since also locked instances has message boxes for
deallocation jobs, the scheduler can just insert the job and avoid the
locking.


### A Benchmark Result ###

When running the ehb benchmark, large amount of messages are passed
around between schedulers. All message passing will in some way or the
other cause memory allocation and deallocation. Since messages are
passed between different schedulers we will get contention on the
allocator instances where messages were allocated. By the introduction
of the delayed dealloc feature, we got a speedup of between 25-45%,
depending on configuration of the benchmark, when running on a
relatively new machine with an Intel i7 quad core processor with
hyper-threading using 8 schedulers.