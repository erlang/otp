Non-Blocking Code Loading
=========================

Introduction
------------

Before OTP R16 when an Erlang code module was loaded, all other
execution in the VM were halted while the load operation was carried
out in single threaded mode. This might not be a big problem for
initial loading of modules during VM boot, but it can be a severe
problem for availability when upgrading modules or adding new code on
a VM with running payload. This problem grows with the number of cores
as both the time it takes to wait for all schedulers to stop increases
as well as the potential amount of halted ongoing work.

In OTP R16, modules are loaded without blocking the VM.
Erlang processes may continue executing undisturbed in parallel during
the entire load operation. The code loading is carried out by a normal
Erlang process that is scheduled like all the others. The load
operation is completed by making the loaded code visible to all
processes in a consistent way with one single atomic
instruction. Non-blocking code loading will improve real-time
characteristics when modules are loaded/upgraded on a running SMP
system.


The Load Phases
---------------

The loading of a module is divided into two phases; a *prepare phase*
and a *finishing phase*. The prepare phase contains reading the BEAM
file format and all the preparations of the loaded code that can
easily be done without interference with the running code. The
finishing phase will make the loaded (and prepared) code accessible
from the running code. Old module versions (replaced or deleted) will
also be made inaccessible by the finishing phase.

The prepare phase is designed to allow several "loader" processes to
prepare separate modules in parallel while the finishing phase can
only be done by one loader process at a time. A second loader process
trying to enter finishing phase will be suspended until the first
loader is done. This will only block the process, the scheduler is
free to schedule other work while the second loader is waiting. (See
`erts_try_seize_code_write_permission` and
`erts_release_code_write_permission`).

The ability to prepare several modules in parallel is not currently
used as almost all code loading is serialized by the code\_server
process. The BIF interface is however prepared for this.

      erlang:prepare_loading(Module, Code) -> LoaderState
      erlang:finish_loading([LoaderState])

The idea is that `prepare_loading` could be called in parallel for
different modules and returns a "magic binary" containing the internal
state of each prepared module. Function `finish_loading` could take a
list of such states and do the finishing of all of them in one go.

Currenlty we use the legacy BIF `erlang:load_module` which is now
implemented in Erlang by calling the above two functions in
sequence. Function `finish_loading` is limited to only accepts a list
with one module state as we do not yet use the multi module loading
feature.


The Finishing Sequence
----------------------

During VM execution, code is accessed through a number of data
structures. These *code access structures* are

* Export table. One entry for every exported function.
* Module table. One entry for each loaded module.
* "beam\_catches". Identifies jump destinations for catch instructions.
* "beam\_ranges". Map code address to function and line in source file.

The most frequently used of these structures is the export table that
is accessed in run time for every executed external function call to
get the address of the callee. For performance reasons, we want to
access all these structures without any overhead from thread
synchronization. Earlier this was solved with an emergency break. Stop
the entire VM to mutate these code access structures, otherwise treat
them as read-only.

The solution in R16 is instead to *replicate* the code access
structures. We have one set of active structures read by the running
code. When new code is loaded the active structures are copied, the
copy is updated to include the newly loaded module and then a switch
is made to make the updated copy the new active set. The active set is
identified by a single global atomic variable
`the_active_code_index`. The switch can thus be made by a single
atomic write operation. The running code have to read this atomic
variable when using the active access structures, which means one
atomic read operation per external function call for example. The
performance penalty from this extra atomic read is however very small
as it can be done without any memory barriers at all (as described
below). With this solution we also preserve the transactional feature
of a load operation. Running code will never see the intermediate
result of a half loaded module.

The finishing phase is carried out in the following sequence by the
BIF `erlang:finish_loading`:

1. Seize exclusive code write permission (suspend process if needed
   until we get it).

2. Make a full copy of all the active access structures. This copy is
   called the staging area and is identified by the global atomic
   variable `the_staging_code_index`.

3. Update all access structures in the staging area to include the
   newly prepared module.

4. Schedule a thread progress event. That is a time in the future when
   all schedulers have yielded and executed a full memory barrier.

5. Suspend the loader process.

6. After thread progress, commit the staging area by assigning
   `the_staging_code_index` to `the_active_code_index`.

7. Release the code write permission allowing other processes to stage
   new code.

8. Resume the loader process allowing it to return from
   `erlang:finish_loading`.


### Thread Progress

The waiting for thread progress in 4-6 is necessary in order for
processes to read `the_active_code_index` atomic during normal
execution without any expensive memory barriers. When we write a new
value into `the_active_code_index` in step 6, we know that all
schedulers will see an updated and consistent view of all the new
active access structures once they become reachable through
`the_active_code_index`.

The total lack of memory barrier when reading `the_active_code_index`
has one interesting consequence however. Different processes may see
the new code at different point in time depending on when different
cores happen to refresh their hardware caches. This may sound unsafe
but it actually does not matter. The only property we must guarantee
is that the ability to see the new code must spread with process
communication. After receiving a message that was triggered by new
code, the receiver must be guaranteed to also see the new code. This
will be guaranteed as all types of process communication involves
memory barriers in order for the receiver to be sure to read what the
sender has written. This implicit memory barrier will then also make
sure that the receiver reads the new value of `the_active_code_index`
and thereby also sees the new code. This is true for all kinds of
inter process communication (TCP, ETS, process name registering,
tracing, drivers, NIFs, etc) not just Erlang messages.

### Code Index Reuse

To optimize the copy operation in step 2, code access structures are
reused. In current solution we have three sets of code access
structures, identified by a code index of 0, 1 and 2. These indexes
are used in a round robin fashion. Instead of having to initialize a
completely new copy of all access structures for every load operation
we just have to update with the changes that have happened since the
last two code load operations. We could get by with only two code
indexes (0 and 1), but that would require yet another round of waiting
for thread progress before step 2 in the `finish_loading` sequence. We
cannot start reusing a code index as staging area until we know that
no lingering scheduler thread is still using it as the active code
index. With three generations of code indexes, the waiting for thread
progress in step 4-6 will give this guarantee for us. Thread progress
will wait for all running schedulers to reschedule at least one
time. No ongoing execution reading code access structures reached from
an old value of `the_active_code_index` can exist after a second round
of thread progress.

The design choice between two or three generations of code access
structures is a trade-off between memory consumption and code loading
latency.

### A Consistent Code View

Some native BIFs may need to get a consistent snapshot view of the
active code. To do this it is important to only read
`the_active_code_index` one time and then use that index value for all
code accessing during the BIF. If a load operation is executed in
parallel, reading `the_active_code_index` a second time might result
in a different value, and thereby a different view of the code.
