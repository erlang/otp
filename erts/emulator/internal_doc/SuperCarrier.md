Super Carrier
=============

A super carrier is large memory area, allocated at VM start, which can
be used during runtime to allocate normal carriers from.

The super carrier feature was introduced in OTP R16B03. It is
enabled with command line option +MMscs &lt;size in Mb&gt;
and can be configured with other options.

Problem
-------

The initial motivation for this feature was customers asking for a way
to pre-allocate physcial memory at VM start for it to use.

Other problems were different experienced limitations of the OS
implementation of mmap:

* Increasingly bad performance of mmap/munmap as the number of mmap'ed areas grow.
* Fragmentation problem between mmap'ed areas.

A third problem was management of low memory in the halfword
emulator. The implementation used a naive linear search structure to
hold free segments which would lead to poor performance when
fragmentation increased.


Solution
--------

Allocate one large continious area of address space at VM start and
then use that area to satisfy our dynamic memory need during
runtime. In other words: implement our own mmap.

### Use cases ###

If command line option +MMscrpm (Reserve Physical Memory) is set to
false, only virtual space is allocated for the super carrier from
start. The super carrier then acts as an "alternative mmap" implementation
without changing the consumption of physical memory pages. Physical
pages will be reserved on demand when an allocation is done from the super
carrier and be unreserved when the memory is released back to the
super carrier.

If +MMscrpm is set to true, which is default, the initial allocation
will reserve physical memory for the entire super carrier. This can be
used by users that want to ensure a certain *minimum* amount of
physical memory for the VM.

However, what reservation of physical memory actually means highly
depends on the operating system, and how it is configured. For
example, different memory overcommit settings on Linux drastically
change the behaviour.

A third feature is to have the super carrier limit the *maximum*
amount of memory used by the VM. If +MMsco (Super Carrier Only) is set
to true, which is default, allocations will only be done from the
super carrier. When the super carrier gets full, the VM will fail due
to out of memory.
If +MMsco is false, allocations will use mmap directly if the super
carrier is full.



### Implementation ###

The entire super carrier implementation is kept in erl\_mmap.c. The
name suggest that it can be viewed as our own mmap implementation.

A super carrier needs to satisfy two slightly different kinds of
allocation requests; multi block carriers (MBC) and single block
carriers (SBC). They are both rather large blocks of continious
memory, but MBCs and SBCs have different demands on alignment and
size.

SBCs can have arbitrary size and do only need minimum 8-byte
alignment.

MBCs are more restricted. They can only have a number of fixed
sizes that are powers of 2. The start address need to have a very
large aligment (currently 256 kb, called "super alignment"). This is a
design choice that allows very low overhead per allocated block in the
MBC.

To reduce fragmentation within the super carrier, it is good to keep SBCs
and MBCs apart. MBCs with their uniform alignment and sizes can be
packed very efficiently together. SBCs without demand for aligment can
also be allocated quite efficiently together. But mixing them can lead
to a lot of memory wasted when we need to create large holes of
padding to the next alignment limit.

The super carrier thus contains two areas. One area for MBCs growing from
the bottom and up. And one area for SBCs growing from the top and
down. Like a process with a heap and a stack growing towards each
other.


### Data structures ###

The MBC area is called *sa* as in super aligned and the SBC area is
called *sua* as in super un-aligned.

Note that the "super" in super alignment and the "super" in super
carrier has nothing to do with each other. We could have choosen
another naming to avoid confusion, such as "meta" carrier or "giant"
aligment.

	+-------+ <---- sua.top
	|  sua  |
	|       |
	|-------| <---- sua.bot
	|       |
	|       |
	|       |
	|-------| <---- sa.top
	|       |
	|  sa   |
	|       |
	+-------+ <---- sa.bot


When a carrier is deallocated a free memory segment will be created
inside the corresponding area, unless the carrier was at the very top
(in `sa`) or bottom (in `sua`) in which case the area will just shrink
down or up.

We need to keep track of all the free segments in order to reuse them
for new carrier allocations. One initial idea was to use the same
mechanism that is used to keep track of free blocks within MBCs
(alloc\_util and the different strategies). However, that would not be
as straight forward as one can think and can also waste quite a lot of
memory as it uses prepended block headers. The granularity of the
super carrier is one memory page (usually 4kb). We want to allocate
and free entire pages and we don't want to waste an entire page just
to hold the block header of the following pages.

Instead we store the meta information about all the free segments in a
dedicated area apart from the `sa` and `sua` areas. Every free segment is
represented by a descriptor struct (`ErtsFreeSegDesc`).

    typedef struct {
        RBTNode snode;      /* node in 'stree' */
        RBTNode anode;      /* node in 'atree' */
        char* start;
        char* end;
    }ErtsFreeSegDesc;

To find the smallest free segment that will satisfy a carrier allocation
(best fit), the free segments are organized in a tree sorted by
size (`stree`). We search in this tree at allocation. If no free segment of
sufficient size was found, the area (`sa` or `sua`) is instead expanded.
If two or more free segments with equal size exist, the one at lowest
address is chosen for `sa` and highest address for `sua`.

At carrier deallocation, we want to coalesce with any adjacent free
segments, to form one large free segment. To do that, all free
segments are also organized in a tree sorted in address order (`atree`).

So, in total we keep four trees of free descriptors for the super
carrier; two for `sa` and two for `sua`. They all use the same
red-black-tree implementation that support the different sorting
orders used.

When allocating a new MBC we first search after a free segment in `sa`,
then try to raise `sa.top`, and then as a fallback try to search after a
free segment in `sua`. When an MBC is allocated in `sua`, a larger segment
is allocated which is then trimmed to obtain the right
alignment. Allocation search for an SBC is done in reverse order. When
an SBC is allocated in `sa`, the size is aligned up to super aligned
size.

### The free descriptor area ###

As mentioned above, the descriptors for the free segments are
allocated in a separate area. This area has a constant configurable
size (+MMscrfsd) that defaults to 65536 descriptors. This should be
more than enough in most cases. If the descriptors area should fill up,
new descriptor areas will be allocated first directly from the OS, and
then from `sua` and `sa` in the super carrier, and lastly from the memory
segment itself which is being deallocated. Allocating free descriptor
areas from the super carrier is only a last resort, and should be
avoided, as it creates fragmentation.

### Halfword emulator ###

The halfword emulator uses the super carrier implementation to manage
its low memory mappings thar are needed for all term storage. The
super carrier can here not be configured by command line options. One
could imagine a second configurable instance of the super carrier used
by high memory allocation, but that has not been implemented.
