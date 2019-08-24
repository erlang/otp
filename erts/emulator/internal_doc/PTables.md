Process and Port Tables
=======================

Problems
--------

The process table is a mapping from process identifiers to process
structure pointers. The process structure contains miscellaneous
information about a process, as for example pointers to its heap,
message queue, etc. When the runtime system needs to operate on a
process, it looks up the process structure in the process table using
the process identifier. An example of this is when passing a message
to a process.

The process table has for a very long time just been an array of
pointers to process structures. Since process identifiers internally
in the runtime system are 28-bit integers it is quite easy to map a
process identifier to index into the array. The 28-bits were divided
into two sets. The least significant set of bits was used as index
into the array. The most significant set of bits was only used to be
able to distinguish between a number of identifiers with which map to
the same index in the array. As long as process table sizes of a power
of two was used we had 2^28 unique process identifiers.

When the first SMP support was implemented, the table still was kept
more or less the same way, but protected by two types of locks. One
lock that protected the whole table against modifications and an array
of locks protecting different parts of the table. The exact locking
strategy previously used isn't interesting. What is interesting is
that it suffered from heavy lock contention especially when lots of
modifications was being made, but also when only performing lookups.

In order to be able to detect when it is safe to deallocate a
previously used process structure, reference counting of the structure
was used. Also this was problematic, since simultaneous lookups needed
to modify the reference counter which also caused contention on the
cache line where the reference counter was located. This since all
modifications needs to be communicated between all involved
processors.

The port table is very similar to the process table. The major
difference, at least in concept, is that it is a mapping from port
identifiers to port structures. It had a similar implementation, but
with some differences. Instead of being an array of pointers it was an
array of structures, and instead of being protected by two types of
locks it was only protected by one global lock. This table also
suffered from lock contention in various situations.

Solution
--------

The process table was the major problem to address since processes are
much more frequently used than ports. The first implementation only
implemented this for processes, but since the port table is very
similar and very similar problems occur on the port table, the process
table implementation was later generalized so that it could also be
used implementing the port table. For simplicity I will only talk
about the process table in the following text, but the same will apply
to the port table unless otherwise stated.

If we disregard the locking issues, the original solution is very
appealing. The mapping from process identifier to index into the array
is very fast, and this property is something we would like to
keep. The vast majority of operations on these tables are lookups so
optimizing for lookups is what we want to do.

### Lookup ###

Using a set of bits in the process identifier as index into an array
seems hard to beat. By replacing the array of pointers with an array
of our pointer sized atomic data type, a lookup will consist of the
following:

1.  Mapping the 28-bit integer to an index into the array.

    More about this mapping later.

2.  Read the pointer using an atomic memory operation at determined
    index in array.

    On all platforms that we provide atomic memory operations, this is
    just a `volatile` read, preventing the compiler to use values in
    registers, forcing the a read from memory.

3.  Depending on use, issue appropriate memory barrier.

    A common barrier used is a barrier with acquire semantics. On
    x86/x86\_64 this maps to a compiler barrier preventing the compiler
    to reorder instructions, but on other hardware often some kind of
    light weight hardware memory barrier is also needed.

    When comparing with a locked approach, at least one heavy weight
    memory barrier will be issued when locking the lock on most, if
    not all, hardware architectures (including x86/x86\_64), and often
    some kind of light weight memory barrier will be issued when
    unlocking the lock. 

When looking at this very simple solution with very little overhead
you might wonder why we didn't implement it this way from the
beginning. It all boils down to the read operation of the pointer. We
need some way to know that it is safe to access the memory pointed
to. One way of doing this is to place a reference counter in the
process structure. Increment of the reference counter at lookup needs
to be done atomically with the lookup. A lock can typically provide
this service for us, which was the approach we previously
used. Another approach could be to co-locate the reference counter
with the pointer in the table. The major problem with this approach is
the modifications of the reference counter. This since these
modification would have to be communicated between all involved
processor cause contention on the cache line containing the reference
counter. The new lookup approach above is possible since we can use
the "thread progress" functionality in order to determine when it is
safe to deallocate the process structure. We'll get back to this when
describing deletion in the table.

Using this new lookup approach we wont modify any memory at all which
is important. A lookup conceptually only read memory, now this is true
in the implementation also which is important from a scalability
perspective. The previous implementation modified the cache line
containing the reference counter two times, and the cache line
containing the corresponding lock two times at each lookup.

### Modifications of the Table ###

A lightweight lookup in the table was the most important feature, but
we also wanted to improve modifications of the table. The process
table is modified when a new process is spawned, i.e. a new pointer is
inserted into the table, and when a process terminates, i.e. a pointer
is deleted in the table.

Assuming that we spawn fewer processes than the maximum amount of
unique process identifiers in the system, one has always been able to
determine the order of process creation just by comparing process
identifiers. If PidX is larger than PidY, then PidX was created after
PidY assuming both identifiers originates from the same node. However,
since we have a quite limited amount of unique identifiers today
(2^28), this property cannot be relied upon if we create large amount
of processes. But never the less, this is a property the system always
have had.

If we would have had a huge amount of unique identifiers available, it
would have tempting to drop or modify this ordering property as
described above. The ordering property could for example be based on
the scheduler performing the spawn operation. It would have been
possible to reserve large ranges of identifiers exclusive for each
scheduler thread which could be used minimizing the need for
communication when allocating identifiers. The amount of identifiers
we got to work with today is, however, not even close to be enough for
such an approach.

Since we have a limited amount of unique identifiers, we need to be
careful not to waste them. If previously used identifiers are reused
too quick, identifiers originating from terminated processes will
refer to newly created processes, and mixups will occur. The
previously used approach was quite good at not wasting
identifiers. Using a modified version of the same approach also lets
us keep the ordering property that we have always had.

#### Insert ####

The original approach is more or less to search for next free index or
slot in the array. The search starts from the last slot allocated. If
we reach the end of the array we increase a "wrapped counter" and then
continue the search. The process identifier is constructed by writing
the index to the least significant set of bits, and the "wrapped
counter" to the most significant set of bits. The amount of bits in
each set of bits is decided at boot time, so that maximum index will
just fit into the least significant set of bits.

In the modified lock free version of this approach we more or less do
it the same way, but with some important modifications trying to avoid
unnecessary contention when multiple schedulers create processes
simultaneously. Since multiple threads might be trying to search for
the next free slot at the same time from the same starting point we
want subsequent slots to be located in different cache lines. Multiple
schedulers simultaneously writing new pointers into the table are
therefore very likely to write into adjacent slots. If adjacent slots
are located in the same cache line all modification of this cache line
needs to be communicated between all involved processors which will be
very expensive and scale very poor. By locating adjacent slots in
different cache lines only true conflicts will trigger communication
between involved processors, i.e., avoiding false sharing.

A cache line is larger than a pointer, typically 8 or 16 times larger,
so using one cache line for each slot only containing one pointer
would be a waste of space. Each cache line will be able to hold a
fixed amount of slots. The first slot of the table will be the first
slot of the first cache line, the second slot of the table will be the
first slot of the second cache line until we reach the end of the
array. The next slot after that will be the second slot of the first
cache line, etc, moving forward one cache line internal slot each time
we wrap. This way we will be able to fit the same amount of pointers
into an array of the same size while always keeping adjacent slots in
different cache lines.

The mapping from identifier to slot or index into the array gets a bit
more complicated than before. Instead of a `shift` and a bitwise
`and`, we get two `shift`s, two bitwise `and`s, and an `add` (see
implementation of `erts_ptab_data2pix()` in `erl_ptab.h`). However, by
storing this information optimized for lookup we only need a `shift`
and a bitwise `and` on 32-bit platforms. On 64-bit platforms we got
enough room for the 28-bit identifier in the least significant
halfword, and the index in the most significant halfword, in other
words, we just need to read the most significant halfword to get the
index. That is, this operation is as fast, or faster than before. The
downside is that on 32-bit platforms we need to convert this
information into the 28-bit identifier number when printing, or when
ordering identifiers from the same node. These operations are,
however, extremely infrequent compared to lookups.

When we insert a new element in the table we do the following:

1.  We begin by reserving space in the table by atomically
    incrementing a counter of processes in the table. If our increment
    brings the counter above the maximum size of the table, the
    operation fail and a `system_limit` exception is raised.

2.  The table contains a 64-bit atomic variable of the last identifier
    used. Only the least significant bits will be used when actually
    creating the identifier. This identifier is where the search
    begin.

3.  We increment last identifier value used. In order determine the
    slot that corresponds to this identifier we call
    `erts_ptab_data2pix()` that maps identifier to slot. We read the
    content of the slot. If the slot is free we try to write a
    reservation marker using an atomic compare and swap. If this fails
    we repeat this step until it succeeds. 

4.  Change the table variable of last identifier used. Since multiple
    writes might occur at the same time this value may already have
    been changed by to an identifier larger that the one we got. In
    this case we can continue; otherwise, we need to change it to the
    identifier we got.

5.  We now do some initializations of the process structure that
    cannot be done before we know the process identifier, and have to
    be done before we publish the structure in the table. This, for
    example, includes storing the identifier in the process structure. 

6.  Now we can publish the structure in the table by writing the the
    pointer to the process structure in the slot previously reserved
    in 3.

Using this approach we keep the properties like identifier ordering,
and identifier reuse while improving performance and scalability. It
has one flaw, though. There is no guarantee that the operation will
terminate. This can quite easily be fixed though, and will be fixed in
the next release. We will get back to this below.

#### Delete ####

When a process terminates, we mark the process as terminated in the
process structure, the counter of number of processes in the table is
decreased, and the reference to the process structure is removed by
writing a `NULL` pointer into the corresponding slot. The scheduler
thread performing this then schedule a thread progress later job which
will do the final cleanup and deallocate the process structure. The
thread progress functionality will make sure that this job will not
execute until it is certain that all managed threads have dropped all
references to the process structure.

### BIF Iterating Over the Table ###

The `erlang:processes/1` and `erlang:port/1` BIFs iterate over the
tables and return corresponding identifiers. These BIF should return a
consistent snapshot of the table content during some time when the BIF
is executing. In order to implement this we use locking in a strange
way. We use an "inverted rwlock".

When performing lookups in the table we do not need to bother about
the locking at all, but when modifying the table we read lock the
rwlock protecting the table which allows for multiple writers during
normal operation. When the BIF that iterates over the table need
access to the table it write locks the rwlock and reads content of the
table. The BIF do not read the whole table in one go but instead read
small chunks at time only write locking while reading. The actual
implementation of the BIFs is out of the scope of this document.

An out of the box rwlock will typically suffer from contention on the
single cache line containing the state of the rwlock even in the case
we are only read locking. Instead of using such an rwlock, we have our
own implementation of reader optimized rwlocks which keeps track of
reader threads in separate thread specific cache lines. This in order
to avoid contention on a singe cache line. As long as we only do read
lock operations, threads only need to read a global cache line and
modify its own cache line, and by this minimize communication between
involved processors. The iterating BIFs are normally very infrequently
used, so in the normal case we will only do read lock operations on
the table global rwlock.

### Future Improvements ###

The first improvement is to fix the guarantee so that insert
operations will be guaranteed to terminate. When the operation starts
we verify that there actually exist a free slot that we can use. The
problem is that we might not find it since it may move when multiple
threads modify the table at the same time as we are trying to find the
slot. The easy fix is to abort the operation if an empty slot could
not be found in a finite number operation, and then restart the
operation under a write lock. This will be implemented in next
release, but furter work should be made trying to find a better
solution.

This and also previous implementation do not work well when the table
is nearly full. We will both get long search times for free slots, and
we will reuse identifiers more frequently since we more frequently
wrap during the search. These tables works best when the table is much
larger than the amount of simultaneous existing processes. One easy
improvement is to always have room for more processes than we allow in
the table. This will also be implemented in the next release, but this
should probably also be worked more on trying to find an even better
solution.

It would also be nice to get rid of the rwlock all together. The use
of a reader optimized rwlock makes sure we do not any contention on
the lock, but unnecessary memory barriers will be issued due to the
lock. The main issue here is to modify iterating BIFs so that they do
not require exclusive access to the table while reading a sequence of
slots. In principle this should be rather easy, the code can handle
sequences of variable sizes, so shrinking the sequence size of slots
to one would solv the problem. This will, however, need some tweeks
and modifications of not trival code, but is something that should be
looked at in the future.

By increasing the size of identifiers, at least on 64-bit machines
(which isn't as easy as it first might seem) we get further room for
improvement. Besides the obvious improvement of not reusing
identifiers as fast as we currently do, it makes it possible to
further avoid contention when inserting elements in the table. At
least if we drop this ordering property, which isn't that useful
anyway.

### Some Benchmark Results ###

In order to test modifications of the process table we ran a couple of
benchmarks where lots of processes are spawned and terminated
simultaneously, and got a speedup of between 150-200%. Running a
similar benchmark but with ports we got a speedup of about 130%.

The BIF `erlang:is_process_alive/1` is the closest you can get to a
process table lookup only. The BIF looks up the process corresponding
to the process identifier passed as argument, and then checks if it is
alive. By running multiple processes looping over this BIF checking
the same process, we get a speedup between 20000-23000%. Conceptually
this operation only involve read operations. In the implementation
used in R16B also only read operation are performed, while the
previous implementation need to lock structures in order to read the
data, suffering from both lock contention and contention due to
modifications of cache lines used by lock internal data structures and
the reference counter on the process being looked up.

The benchmarks were run on a relatively new machine with an Intel i7
quad core processor with hyper-threading using 8 schedulers. On a
machine with more communication overhead and/or larger amount of
logical processors the speedups are expected to be even larger.
