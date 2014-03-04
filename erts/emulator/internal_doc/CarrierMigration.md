Carrier Migration
=================

The ERTS memory allocators manage memory blocks in two types of raw
memory chunks. We call these chunks of raw memory
*carriers*. Singleblock carriers which only contain one large block,
and multiblock carriers which contain multiple blocks. A carrier is
typically created using `mmap()` on unix systems. However, how a
carrier is created is of minor importance. An allocator instance
typically manages a mixture of single- and multiblock carriers.

Problem
-------

When a carrier is empty, i.e. contains only one large free block, it
is deallocated. Since multiblock carriers can contain both allocated
blocks and free blocks at the same time, an allocator instance might
be stuck with a large amount of poorly utilized carriers if the memory
load decrease. After a peak in memory usage it is expected that not
all memory can be returned since the blocks still allocated is likely
to be dispersed over multiple carriers. Such poorly utilized carriers
can usually be reused if the memory load increase again. However,
since each scheduler thread manages its own set of allocator
instances, and memory load is not necessarily connected to CPU load we
might get into a situation where there are lots of poorly utilized
multiblock carriers on some allocator instances while we need to
allocate new multiblock carriers on other allocator instances. In
scenarios like this, the demand for multiblock carriers in the system
might increase at the same time as the actual memory demand in the
system has decreased which is both unwanted and quite unexpected for
the end user.

Solution
--------

In order to prevent scenarios like this we've implemented support for
migration of multiblock carriers between allocator instances of the
same type.

### Management of Free Blocks ###

In order to be able to remove a carrier from one allocator instance
and add it to another we need to be able to move references to the
free blocks of the carrier between the allocator instances. The
allocator instance specific data structure referring to the free
blocks it manages often refers to the same carrier from multiple
places. For example, when the address order bestfit strategy is used
this data structure is a binary search tree spanning all carriers that
the allocator instance manages. Free blocks in one specific carrier
can be referred to from potentially every other carrier that is
managed, and the amount of such references can be huge. That is, the
work of removing the free blocks of such a carrier from the search
tree will be huge. One way of solving this could be to not migrate
carriers that contain lots of free blocks, but this would prevent us
from migrating carriers that potentially needs to be migrated in order
to solve the problem we set out to solve.

By using one data structure of free blocks in each carrier and an
allocator instance wide data structure of carriers managed by the
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

The pool is implemented as a lock free, circular, double linked,
list. The list contains a sentinel which is used as the starting point
when inserting to, or fetching from the pool. Carriers in the pool are
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

The `prev`, and `next` fields in the elements of the list contains the
value of the pointer, a modification marker, and a deleted
marker. Memory operations on these fields are done using atomic memory
operations. When a thread has set the modification marker in a field,
no-one except the thread that set the marker is allowed to modify the
field. If multiple modification markers needs to be set, we always
begin with `next` fields followed by `prev` fields in the order
following the actual pointers. This guarantees that no deadlocks will
occur.

When a carrier is being removed from a pool, we mark it with a thread
progress value that needs to be reached before we are allowed to
modify the `next`, and `prev` fields. That is, until we reach this
thread progress we are not allowed to insert the carrier into the pool
again, and we are not allowed to deallocate the carrier. This ensures
that threads inspecting the pool always will be able to traverse the
pool and reach valid elements. Once we have reached the thread
progress value that the carrier was tagged with, we know that no
threads may have references to it via the pool.

### Migration ###

There exist one pool for each allocator type enabling migration of
carriers between scheduler specific allocator instances of the same
allocator type.

Each allocator instance keeps track of the current utilization of its
multiblock carriers. When the utilization falls below the "abandon
carrier utilization limit" it starts to inspect the utilization of the
current carrier when deallocations are made. If also the utilization
of the carrier falls below the "abandon carrier utilization limit" it
unlinks the carrier from its data structure of available free blocks
and inserts the carrier into the pool.

Since the carrier has been unlinked from the data structure of
available free blocks, no more allocations will be made in the
carrier. The allocator instance putting the carrier into the pool,
however, still has the responsibility of performing deallocations in
it while it remains in the pool.

Each carrier has a flag field containing information about allocator
instance owning the carrier, a flag indicating if the carrier is in
the pool or not, and a flag indicating if it is busy or not. When the
carrier is in the pool, the owning allocator instance needs to mark it
as busy while operating on it. If another thread inspects it in order
to try to fetch it from the pool, it will abort the fetch if it is
busy. When fetching the carrier from the pool, ownership will changed
and further deallocations in the carrier will be redirected to the new
owner using the delayed dealloc functionality.

If a carrier in the pool becomes empty, it will be withdrawn from the
pool. All carriers that become empty are also always passed to its
originating allocator instance for deallocation using the delayed
dealloc functionality. Since carriers this way always will be
deallocated by the allocator instance that allocated the carrier the
underlying functionality of allocating and deallocating carriers can
remain simple and doesn't have to bother about multiple threads. In a
NUMA system we will also not mix carriers originating from multiple
NUMA nodes.

When an allocator instance needs more carrier space, it always begins
by inspecting its own carriers that are waiting for thread progress
before they can be deallocated. If no such carrier could be found, it
then inspects the pool. If no carrier could be fetched from the pool,
it will allocate a new carrier. Regardless of where the allocator
instance gets the carrier from it the just links in the carrier into
its data structure of free blocks.

### Result ###

The use of this strategy of abandoning carriers with poor utilization
and reusing these in allocator instances with an increased carrier
demand is extremely effective and completely eliminates the problems
that otherwise sometimes occurred when CPU load dropped while memory
load did not.

When using the `aoffcaobf` or `aoff` strategies compared to `gf` or
`bf`, we loose some performance since we get more modifications in the
data structure of free blocks. This performance penalty is however
reduced using the `aoffcbf` strategy. A tradeoff between memory
consumption and performance is however inevitable, and it is up to
the user to decide what is most important. 

Further work
------------

It would be quite easy to extend this to allow migration of multiblock
carriers between all allocator types. More or less the only obstacle
is maintenance of the statistics information.


