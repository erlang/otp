Process Management Optimizations
================================

Problems
--------

Early versions of the SMP support for the runtime system completely
relied on locking in order to protect data accesses from multiple
threads. In some cases this isn't that problematic, but in some cases
it really is. It complicates the code, ensuring all locks needed are
actually held, and ensuring that all locks are acquired in such an
order that no deadlock occur. Acquiring locks in the right order often
also involve releasing locks held, forcing threads to reread data
already read. A good recipe for creation of bugs. Trying to use more
fine-grained locking in order to increase possible parallelism in the
system makes the complexity situation even worse. Having to acquire a
bunch of locks when doing operations also often cause heavy lock
contention which cause poor scalability.

Management of processes internally in the runtime system suffered from
these problems. When changing state on a process, for example from
`waiting` to `runnable`, a lock on the process needed to be
locked. When inserting a process into a run queue also a lock
protecting the run queue had to be locked. When migrating a process
from one run queue to another run queue, locks on both run queues and
on the process had to be locked.

This last example is a quite common case in during normal
operation. For example, when a scheduler thread runs out of work it
tries to steal work from another scheduler threads run queue. When
searching for a victim to steal from there was a lot of juggling of
run queue locks involved, and during the actual theft finalized by
having to lock both run queues and the process. When one scheduler
runs out of work, often others also do, causing lots of lock
contention.

Solution
--------

### Process ###

In order to avoid these situations we wanted to be able to do most of
the fundamental operations on a process without having to acquire a
lock on the process. Some examples of such fundamental operations are,
moving a process between run queues, detecting if we need to insert it
into a run queue or not, detecting if it is alive or not.

All of this information in the process structure that was needed by
these operations was protected by the process `status` lock, but the
information was spread across a number of fields. The fields used was
typically state fields that could contain a small number of different
states. By reordering this information a bit we could *easily* fit
this information into a 32-bit wide field of bit flags (only 12-flags
were needed). By moving this information we could remove five 32-bit
wide fields and one pointer field from the process structure! The move
also enabled us to easily read and change the state using atomic
memory operations.

### Run Queue ###

As with processes we wanted to be able to do the most fundamental
operations without having to acquire a lock on it. The most important
being able to determine if we should enqueue a process in a specific
run queue or not. This involves being able to read actual load, and
load balancing information.

The load balancing functionality is triggered at repeated fixed
intervals. The load balancing more or less strives to even out run
queue lengths over the system. When balancing is triggered,
information about every run queue is gathered, migrations paths and
run queue length limits are set up. Migration paths and limits are
fixed until the next balancing has been done. The most important
information about each run queue is the maximum run queue length since
last balancing. All of this information were previously stored in the
run queues themselves.

When a process has become runnable, for example due to reception of a
message, we need to determine which run queue to enqueue it
in. Previously this at least involved locking the run queue that the
process currently was assigned to while holding the status lock on the
process. Depending on load we sometimes also had to acquire a lock on
another run queue in order to be able to determine if it should be
migrated to that run queue or not.

In order to be able to decide which run queue to use without having to
lock any run queues, we moved all fixed balancing information out of
the run queues into a global memory block. That is, migration paths
and run queue limits. Information that need to be frequently updated,
like for example maximum run queue length, were kept in the run queue,
but instead of operating on this information under locks we now use
atomic memory operations when accessing this information. This made it
possible to first determine which run queue to use, without locking
any run queues, and when decided, lock the chosen run queue and insert
the process.

#### Fixed Balancing Information ####

When determining which run queue to choose we need to read the fixed
balancing information that we moved out of the run queues. This
information is global, read only between load balancing operations,
but will be changed during a load balancing. We do not want to
introduce a global lock that needs to be acquired when accessing this
information. A reader optimized rwlock could avoid some of the
overhead since the data is most frequently read, but it would
unavoidably cause disruption during load balancing, since this
information is very frequently read. The likelihood of a large
disruption due to this also increase as number of schedulers grows.

Instead of using a global lock protecting modifications of this
information, we write a completely new version of it at each load
balancing. The new version is written in another memory block than the
previous one, and published by issuing a write memory barrier and then
storing a pointer to the new memory block in a global variable using
an atomic write operation.

When schedulers need to read this information, they read the pointer
to currently used information using an atomic read operation, and then
issue a data dependency read barrier, which on most architectures is a
no-op. That is, it is very little overhead getting access to this
information.

Instead of allocating and deallocating memory blocks for the different
versions of the balancing information we keep old memory blocks and
reuse them when it is safe to do so. In order to be able to determine
when it is safe to reuse a block we use the thread progress
functionality, ensuring that no threads have any references to the
memory block when we reuse it.

#### Be Less Aggressive ####

We implemented a test version using lock free run queues. This
implementation did however not perform as good as the version using
one lock per run queue. The reason for this was not investigated
enough to say why this was. Since the locked version performed better
we kept it, at least for now. The lock free version, however, forced
us to use other solutions, some of them we kept.

Previously when a process that was in a run queue got suspended, we
removed it from the queue straight away. This involved locking the
process, locking the run queue, and then unlinking it from the double
linked list implementing the queue. Removing a process from a lock
free queue gets really complicated. Instead, of removing it from the
queue, we just leave it in the queue and mark it as suspended. When
later selected for execution we check if the process is suspended, if
so just dropped it. During its time in the queue, it might also get
resumed again, if so execute it when it get selected for execution.

By keeping this part when reverting back to a locked implementation,
we could remove a pointer field in each process structure, and avoid
unnecessary operations on the process and the queue which might cause
contention.

### Combined Modifications ###

By combining the modifications of the process state management and the
run queue management, we can do large parts of the work involved when
managing processes with regards to scheduling and migration without
having any locks locked at all. In these situations we previously had
to have multiple locks locked. This of course caused a lot of rewrites
across large parts of the runtime system, but the rewrite both
simplified code and eliminated locking at a number of places. The
major benefit is, of course, reduced contention.

### A Benchmark Result ###

When running the chameneosredux benchmark, schedulers frequently run
out of work trying to steal work from each other. That is, either
succeeding in migrating, or trying to migrate processes which is a
scenario which we wanted to optimize. By the introduction of these
improvements, we got a speedup of 25-35% when running this benchmark
on a relatively new machine with an Intel i7 quad core processor with
hyper-threading using 8 schedulers.