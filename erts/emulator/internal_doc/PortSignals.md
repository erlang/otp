Port Signals
============

Problems
--------

Erlang ports conceptually are very similar to Erlang processes. Erlang
processes execute Erlang code in the virtual machine, while an Erlang
port execute native code typically used for communication with the
outside world. For example, when an Erlang process wants to
communicate using TCP over the network, it communicates via an Erlang
port implementing the TCP socket interface in native code. Both Erlang
Processes and Ports communicate using asynchronous signaling. The
native code executed by an Erlang port is a collection of callback
functions, called a driver. Each callback more or less implements the
code of a signal to, or from the port.

Even though processes and ports conceptually always have been very
similar, the implementations have been very different. Originally,
more or less all port signals were handled synchronously at the time
they occurred. Very early in the development of the SMP support for
the runtime system we recognized that this was a huge problem for
signals between ports and the outside world. That is, I/O events to
and from the outside world, or I/O signals. This was one of the first
things that had to be rewritten in order to be able to do I/O in
parallel at all. The solution was to implement scheduling of these
signals. I/O signals corresponding to different ports could then be
executed in parallel on different scheduler threads. Signals from
processes to ports was not as big of a problem as the I/O signals, and
the implementation of those was left as they were.

Each port is protected by its own lock to protect against simultaneous
execution in multiple threads. Previously when a process, executing on
a scheduler thread, sent a port a signal, it locked the port lock and
synchronously executed the code corresponding to the signal. If the
lock was busy, the scheduler thread blocked waiting until it could
lock the lock. If multiple processes executing simultaneously on
different scheduler threads, sent signals to the same port, schedulers
suffered from heavy lock contention. Such contention could also occur
between I/O signals for the port executing on one scheduler thread,
and a signal from a process to the port executing on another scheduler
thread. Beside the contention issues, we also loose potential work to
execute in parallel on different scheduler threads. This since the
process sending the *asynchronous* signal is blocked while the code
implementing the signal is executed synchronously.

Solution
--------

In order to prevent multiple schedulers from trying to execute signals
to/from the same port simultaneously, we need to be able to ensure
that all signals to/from a port are executed in sequence on one
scheduler. More or less, the only way to do this is to schedule all
types of signals. Signals corresponding to a port can then be executed
in sequence by one single scheduler thread. If only one thread tries
to execute the port, no contention will appear on the port
lock. Besides getting rid of the contention, processes sending signals
to the port can also continue execution of their own Erlang code on
other schedulers at the same time as the signaling code is executing
on another scheduler.

When implementing this there are a couple of important properties that
we either need, or want to preserve:

*   Signal ordering guarantee. Signals from process `X` to port `Y`,
    *must* be delivered to `Y` in the same order as sent from `X`.

*   Signal latency. Due to the previous synchronous implementation,
    latency of signals sent from processes to ports have usually been
    very low. During contention the latency has of course
    increased. Users expect latency of these signals to be low, a
    sudden increase in latency would not be appreciated by our users.

*   Compatible flow control. Ports have for a very long time had the
    possibility to use the busy port functionality when implementing
    flow control. One may argue that this functionality fits very bad
    with the conceptually completely asynchronous signaling, but the
    functionality has been there for ages and is expected to be
    there. When a port sets itself into a busy state, `command`
    signals should not be delivered, and senders of such signals
    should suspend until the port sets itself in a not busy state.

### Scheduling of Port Signals ###

A run queue has four queues for processes of different priority and
one queue for ports. The scheduler thread associated with the run
queue switch evenly between execution of processes and execution of
ports while both processes and ports exist in the queue. This is not
completely true, but not important for this discussion. A port that is
in a run queue also has a queue of tasks to execute. Each task
corresponds to an in- or outgoing signal. When the port is selected
for execution each task will be executed in sequence. The run queue
locks not only protected the queues of ports, but also the queues of
port tasks.

Since we go from a state where I/O signals are the only port related
signals scheduled, to a state where potentially all port related
signals may be scheduled we may drastically increase the load on the
run queue lock. The amount of scheduled port tasks very much depend on
the Erlang application executing, which we do not control, and we do
not want to get increased contention on the run queue locks. We
therefore need another approach of protecting the port task queue.

#### Task Queue ####

We chose a "semi locked" approach, with one public locked task queue,
and a private, lock free, queue like, task data structure. This "semi
locked" approach is similar to how the message boxes of processes are
managed. The lock is port specific and only used for protection of
port tasks, so the run queue lock is now needed in more or less the
same way for ports as for processes. This ensures that we wont see an
increased lock contention on run queue locks due to this rewrite of
the port functionality.

When an executing port runs out of work to execute in the private task
data structure, it moves the public task queue into the private task
data structure while holding the lock. Once tasks has been moved to
the private data structure no lock protects them. This way the port
can continue working on tasks in the private data structure without
having to fight for the lock.

I/O signals may however be aborted. This could be solved by letting
the port specific scheduling lock also protect the private task data
structure, but then the port very frequently would have to fight with
others enqueueing new tasks. In order to handle this while keeping the
private task data structure lock free, we use a similar "non
aggressive" approach as we use when handling processes that gets
suspended while in the run queue. Instead of removing the aborted port
task, we just mark it as aborted using an atomic memory
operation. When a task is selected for execution, we first verify that
it has not been aborted. If aborted we, just drop the task.

A task that can be aborted is referred via another data structure from
other parts of the system, so that a thread that needs to abort the
task can reach it. In order to be sure to safely deallocate a task
that is no longer used, we first clear this reference and then use the
thread progress functionality in order to make sure no references can
exist to the task. Unfortunately, also unmanaged threads might abort
tasks. This is very infrequent, but might occur. This could be handled
locally for each port, but would require extra information in each
port structure which very infrequently would be used. Instead of
implementing this in each port, we implemented general functionality
that can be used from unmanaged threads to delay thread progress.

The private "queue like" task data structure could have been an
ordinary queue if it wasn't for the busy port functionality. When the
port has flagged itself as busy, `command` signals are not allowed to
be delivered and need to be blocked. Other signals sent from the same
sender following a `command` signal that has been blocked also have to
be blocked; otherwise, we would violate the ordering guarantee. At the
same time, other signals that have no dependencies to blocked
`command` signals are expected to be delivered.

The above requirements makes the private task data structure a rather
complex data structure. It has a queue of unprocessed tasks, and a
busy queue. The busy queue contains blocked tasks corresponding to
`command` signals, and tasks with dependencies to such tasks. The busy
queue is accompanied by a table over blocked tasks based on sender
with a references into last task in the busy queue from a specific
sender. This since we need check for dependencies when new tasks are
processed in the queue of unprocessed tasks. When a new task is
processed that needs to be blocked it isn't enqueued at the end of the
busy queue, but instead directly after the last task with the same
sender. This in order to easily be able to detect when we have tasks
that no longer have any dependencies to tasks corresponding to
`command` signals which should be moved out of the busy queue. When
the port executes, it switches between processing tasks from the busy
queue, and processing directly from the unprocessed queue based on its
busy state. When processing directly from the unprocessed queue it
might, of course, have to move a task into the busy queue instead of
executing it.

#### Busy Port Queue ####

Since it is the port itself which decides when it is time to enter a
busy state, it needs to be executing in order to enter the busy
state. As a result of `command` signals being scheduled, we may get
into a situation where the port gets flooded by a huge amount of
`command` signals before it even gets a chance to set itself into a
busy state. This since it has not been scheduled for execution
yet. That is, under these circumstances the busy port functionality
loose the flow control properties it was intended to provide.

In order to solve this, we introduced a new busy feature, namely "busy
port queue". The port has a limit of `command` data that is allowed to
be enqueued in the task queue. When this limit is reached, the port
will automatically enter a busy port queue state. When in this state,
senders of `command` signals will be suspended, but `command` signals
will still be delivered to the port unless it is also in a busy port
state. This limit is known as the high limit.

There is also a low limit. When the amount of queued `command` data
falls below this limit and the port is in a busy port queue state, the
busy port queue state is automatically disabled. The low limit should
typically be significantly lower than the high limit in order to
prevent frequent oscillation around the busy port queue state.

By introduction of this new busy state we still can provide the flow
control. Old driver do not even have to be changed. The limits can,
however, be configured and even disabled by the port. By default the
high limit is 8 KB and the low limit is 4 KB.

### Preparation of Signal Send ###

Previously all operations sending signals to ports began by acquiring
the port lock, then performed preparations for sending the signal, and
then finally sent the signal. The preparations typically included
inspecting the state of the port, and preparing the data to pass along
with the signal. The preparation of data is frequently quite time
consuming, and did not really depend on the port. That is we would
like to do this without having the port lock locked.

In order to improve this, state information was re-organized in the
port structer, so that we can access it using atomic memory
operations. This together with the new port table implementation,
enabled us to lookup the port and inspect the state before acquiring
the port lock, which in turn made it possible to perform preparations
of signal data before acquiring the port lock.

### Preserving Low Latency ###

If we disregard the contended cases, we will inevitably get a higher
latency when scheduling signals for execution at a later time than by
executing the signal immediately. In order to preserve the low latency
we now first check if this is a contended case or not. If it is, we
schedule the signal for later execution; otherwise, we execute the
signal immediately. It is a contended case if other signals already
are scheduled on the port, or if we fail to acquire the port
lock. That is we will not block waiting for the lock.

Doing it this way we will preserve the low latency at the expense of
lost potential parallel execution of the signal and other code in the
process sending the signal. This default behaviour can however be
changed on port basis or system wide, forcing scheduling of all
signals from processes to ports that are not part of a synchronous
communication. That is, an unconditional request/response pair of
asynchronous signals. In this case it is no potential for parallelism,
and by that no point forcing scheduling of the request signal.

The immediate execution of signals may also cause a scheduler that is
about to execute scheduled tasks to block waiting for the port
lock. This is however more or less the only scenario where a scheduler
needs to wait for the port lock. The maximum time it has to wait is
the time it takes to execute one signal, since we always schedule
signals when contention occurs.

### Signal Operations ###

Besides implementing the functionality enabling the scheduling,
preparation of signal data without port lock, etc, each operation
sending signals to ports had to be quite extensively re-written. This
in order to move all sub-operations that can be done without the lock
to a place before we have acquired the lock, and also since signals
now sometimes are executed immediately and sometimes scheduled for
execution at a later time which put different requirements on the data
to pass along with the signal.

### Some Benchmark Results ###

When running some simple benchmarks where contention only occur due to
I/O signals contending with signals from one single process we got a
speedup of 5-15%. When multiple processes send signals to one single
port the improvements can be much larger, but the scenario with one
process contending with I/O is the most common one.

The benchmarks were run on a relatively new machine with an Intel i7
quad core processor with hyper-threading using 8 schedulers.