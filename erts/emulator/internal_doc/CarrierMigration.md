Carrier Migration
=================

Introduction
------------

The ERTS memory allocators manage memory blocks in two types of raw
memory chunks. We call these chunks of raw memory
*carriers*. Single-block carriers which only contain one large block,
and multi-block carriers which contain multiple blocks. A carrier is
typically created using `mmap()` on unix systems. However, how a
carrier is created is of minor importance. An allocator instance
typically manages a mixture of single- and multi-block carriers.

Problem
-------

When a carrier is empty, i.e. contains only one large free block, it
is deallocated. Since multi-block carriers can contain both allocated
blocks and free blocks at the same time, an allocator instance might
be stuck with a large amount of poorly utilized carriers if the memory
load decreases. After a peak in memory usage it is expected that not
all memory can be returned since the blocks still allocated are likely
to be dispersed over multiple carriers. Such poorly utilized carriers
can usually be reused if the memory load increases again. However,
since each scheduler thread manages its own set of allocator
instances, and memory load is not necessarily correlated to CPU load, we
might get into a situation where there are lots of poorly utilized
multi-block carriers on some allocator instances while we need to
allocate new multi-block carriers on other allocator instances. In
scenarios like this, the demand for multi-block carriers in the system
might increase at the same time as the actual memory demand in the
system has decreased which is both unwanted and quite unexpected for
the end user.

Solution
--------

In order to prevent scenarios like this we've implemented support for
migration of multi-block carriers between allocator instances.

### Management of Free Blocks ###

In order to be able to remove a carrier from one allocator instance
and add it to another we need to be able to move references to the
free blocks of the carrier between the allocator instances. The
allocator instance specific data structure referring to the free
blocks it manages often refers to the same carrier from multiple
places. For example, when the address order best-fit strategy is used
this data structure is a binary search tree spanning all carriers that
the allocator instance manages. Free blocks in one specific carrier
can be referred to from potentially every other carrier that is
managed, and the amount of such references can be huge. That is, the
work of removing the free blocks of such a carrier from the search
tree will be huge. One way of solving this could be not to migrate
carriers that contain lots of free blocks, but this would prevent us
from migrating carriers that potentially need to be migrated in order
to solve the problem we set out to solve.

By using one data structure of free blocks in each carrier and an
allocator instance-wide data structure of carriers managed by the
allocator instance, the work needed in order to remove and add
carriers can be kept to a minimum. When migration of carriers is
enabled on a specific allocator type, we require that an allocation
strategy with such an implementation is used. Currently we've
implemented this for three different allocation strategies. All of
these strategies use a search tree of carriers sorted so that we can
find the carrier with the lowest address that can satisfy the
request. Internally in carriers we use yet another search tree that
either implement address order first fit, address order best fit,
or best fit. The abbreviations used for these different allocation
strategies are `aoff`, and `aoffcaobf`, `aoffcbf`.

### Carrier Pool ###

In order to migrate carriers between allocator instances we move them
through a pool of carriers. In order for a carrier migration to
complete, one scheduler needs to move the carrier into the pool, and
another scheduler needs to take the carrier out of the pool.

The pool is implemented as a lock-free, circular, double linked,
list. The list contains a sentinel which is used as the starting point
when inserting to, or fetching from, the pool. Carriers in the pool are
elements in this list.

The list can be modified by all scheduler threads
simultaneously. During modifications the double linked list is allowed
to get a bit "out of shape". For example, following the `next` pointer
to the next element and then following the `prev` pointer does not
always take you back to were you started. The following is however
always true:

*   Repeatedly following `next` pointers will eventually take you to the
    sentinel.
*   Repeatedly following `prev` pointers will eventually take you to the
    sentinel.
*   Following a `next` or a `prev` pointer will take you to either an
    element in the pool, or an element that used to be in the pool.

When inserting a new element we search for a place to insert the
element by only following `next` pointers, and we always begin by
skipping the first element encountered. When trying to fetch an
element we do the same thing, but instead only follow `prev` pointers.

By going different directions when inserting and fetching, we avoid
contention between threads inserting and threads fetching as much as
possible. By skipping one element when we begin searching, we preserve
the sentinel unmodified as much as possible. This is beneficial since
all search operations need to read the content of the sentinel. If we
were to modify the sentinel, the cache line containing the sentinel
would unnecessarily be bounced between processors.

The `prev` and `next` fields in the elements of the list contain the
value of the pointer, a modification marker, and a deleted
marker. Memory operations on these fields are done using atomic memory
operations. When a thread has set the modification marker in a field,
no-one except the thread that set the marker is allowed to modify the
field. If multiple modification markers need to be set, we always
begin with `next` fields followed by `prev` fields in the order
following the actual pointers. This guarantees that no deadlocks will
occur.

When a carrier is being removed from a pool, we mark it with a thread
progress value that needs to be reached before we are allowed to
modify the `next` and `prev` fields. That is, until we reach this
thread progress we are not allowed to insert the carrier into the pool
again, and we are not allowed to deallocate the carrier. This ensures
that threads inspecting the pool always will be able to traverse the
pool and reach valid elements. Once we have reached the thread
progress value that the carrier was tagged with, we know that no
threads may have references to it via the pool.

### Migration ###

Each allocator instance keeps track of the current utilization of its
multi-block carriers. When the total utilization falls below the "abandon
carrier utilization limit" it starts to inspect the utilization of the
current carrier when deallocations are made. If also the utilization
of the carrier falls below the "abandon carrier utilization limit" it
unlinks the carrier from its data structure of available free blocks
and inserts the carrier into the pool.

Since the carrier has been unlinked from the data structure of
available free blocks, no more allocations will be made in the
carrier.

The allocator instance that created a carrier is called its *owner*.
Ownership never changes.

The allocator instance that has the responsibility to perform deallocations in a
carrier is called its *employer*. The employer may also perform allocations if
the carrier is not in the pool. Employment may change when a carrier is fetched from
or inserted into the pool.

Deallocations in a carrier, while it remains in the pool, is always performed
the owner. That is, all pooled carriers are employed by their owners.

Each carrier has an atomic word containing a pointer to the employing allocator
instance and three bit flags; IN\_POOL, BUSY and HOMECOMING.

When fetching a carrier from the pool, employment may change and further
deallocations in the carrier will be redirected to the new
employer using the delayed dealloc functionality.

When a foreign allocator instance abandons a carrier back into the pool, it will
also pass it back to its *owner* using the delayed dealloc queue. When doing
this it will set the HOMECOMING bit flag to mark it as "enqueued". The owner
will later clear the HOMECOMING bit when the carrier is dequeued. This mechanism
prevents a carrier from being enqueued again before it has been dequeued.

When a carrier becomes empty, it will be deallocated. Carrier deallocation is
always done by the owner that allocated the carrier. By doing this, the
underlying functionality of allocating and deallocating carriers can
remain simple and doesn't have to bother about multiple threads. In a
NUMA system we will also not mix carriers originating from multiple
NUMA nodes.

If a carrier in the pool becomes empty, it will be withdrawn from the
pool and be deallocated by the owner which already employs it.

If a carrier employed by a foreign allocator becomes empty, it will be passed
back to the owner for deallocation using the delayed dealloc functionality.

In short:

* The allocator instance that created a carrier *owns* it.
* An empty carrier is always deallocated by its *owner*.
* *Ownership* never changes.
* The allocator instance that uses a carrier *employs* it.
* An *employer* can abandon a carrier into the pool.
* Pooled carriers are not allocated from.
* Pooled carriers are always *employed* by their *owner*.
* *Employment* can only change from *owner* to a foreign allocator
  when a carrier is fetched from the pool.


### Searching the pool ###

When an allocator instance needs more carrier space, it inspects the pool. If no
carrier could be fetched from the pool, it will allocate a new
carrier. Regardless of where the allocator instance gets the carrier from, it
just links in the carrier into its data structure of free blocks.

To harbor real time characteristics, searching the pool is
limited. We only inspect a limited number of carriers. If none of
those carriers had a free block large enough to satisfy the allocation
request, the search will fail. A carrier in the pool can also be BUSY
if another thread is currently doing block deallocation work on the
carrier. A BUSY carrier will also be skipped by the search as it cannot
satisfy the request. The pool is lock-free and we do not want to
block, waiting for the other thread to finish.

### The bad cluster problem ###

Before OTP-17.4 the search algorithm had a problem as the search always started
at the same position in the pool, the sentinel. This could lead to
contention from concurrent searching processes. But even worse, it
could lead to a "bad" state when searches fail with a high rate
leading to new carriers instead being allocated. These new carriers
may later be inserted into the pool due to bad utilization. If the
frequency of insertions into the pool is higher than successful
fetching from the pool, memory will eventually get exhausted.

This "bad" state consists of a cluster of small and/or highly
fragmented carriers located at the sentinel in the pool. The largest free
block in such a "bad" carrier is rather small, making it unable to satisfy
most allocation requests. As the search always started at the
sentinel, any such "bad" carriers that had been left in the pool would
eventually cluster together at the sentinel. All searches first
have to skip past this cluster of "bad" carriers to reach a "good"
carrier. When the cluster gets to the same size as the search limit,
all searches will essentially fail.

To counter the "bad cluster" problem and also ease the contention, the
search will now always start by first looking at the allocators *own*
carriers. That is, carriers that were initially created by the
allocator itself and later had been abandoned to the pool. If none of
our own abandoned carrier would do, then the search continues into the
pool, as before, to look for carriers created by other
allocators. However, if we have at least one abandoned carrier of our
own that could not satisfy the request, we can use that as entry point
into the pool.

The result is that we prefer carriers created by the thread itself,
which is good for NUMA performance. And we get more entry points when
searching the pool, which will ease contention and clustering.

### Our own pooled tree ###

To do the first search among own carriers, every allocator instance
has a `pooled_tree` of carriers. This tree is only accessed by the allocator
itself and can only contain its own carriers. When a carrier is
abandoned and put in the pool, it is also inserted into `pooled_tree`. This is
either done direct, if the carrier was already employed by its owner, or by
first passing it back to the owner via the delayed dealloc queue.

When we search our `pooled_tree` and find a carrier that is no longer in the
pool, we remove that carrier from `pooled_tree` and mark it as TRAITOR, as it is
now employed by a foreign allocator. We will not find any carriers in
`pooled_tree` that are marked as BUSY by other threads.

If no carrier in `pooled_tree` had a large enough free block, we search it again
to find any carrier that may act as an entry point into the shared list of all
pooled carriers. This in order to, if possible, avoid starting at the sentinel
and thereby ease the "bad clustering" problem.

Furthermore, the search for own carriers that are scheduled
for deallocation is done as the last search option. The idea is
that it is better to reuse a poorly utilized carrier than to
resurrect an empty carrier that was just about to be released back to
the OS.

### Result ###

The use of this strategy of abandoning carriers with poor utilization
and reusing them in allocator instances with an increased carrier
demand is extremely effective and completely eliminates the problems
that otherwise sometimes occurred when CPU load dropped while memory
load did not.

When using the `aoffcaobf` or `aoff` strategies compared to `gf` or
`bf`, we loose some performance since we get more modifications in the
data structure of free blocks. This performance penalty is however
reduced using the `aoffcbf` strategy. A trade off between memory
consumption and performance is however inevitable, and it is up to
the user to decide what is most important. 

