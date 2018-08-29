Non-blocking trace setting
==========================

Introduction
------------

Before OTP R16 when trace settings were changed by `erlang:trace_pattern`,
all other execution in the VM were halted while the trace operation
was carried out in single threaded mode. Similar to code loading, this
can impose a severe problem for availability that grows with the
number of cores.

In OTP R16, trace breakpoints are set in the code without blocking the
VM. Erlang processes may continue executing undisturbed in parallel
during the entire operation. The same base technique is used as for
code loading. A staging area of breakpoints is prepared and then made
active with a single atomic operation.


Redesign of Breakpoint Wheel
----------------------------

To make it easier to manage breakpoints without single threaded mode a
redesign of the breakpoint mechanism has been made. The old
"breakpoint wheel" data structure was a circular double-linked list of
breakpoints for each instrumented function. It was invented before the
SMP emulator. To support it in the SMP emulator, is was essentially
expanded to one breakpoint wheel per scheduler. As more breakpoint
types have been added, the implementation have become messy and hard
to understand and maintain.

In the new design the old wheel was dropped and instead replaced by
one struct (`GenericBp`) to hold the data for all types of breakpoints
for each instrumented function. A bit-flag field is used to indicate
what different type of break actions that are enabled.


Same Same but Different
-----------------------

Even though `trace_pattern` use the same technique as the non-blocking
code loading with replicated generations of data structures and an
atomic switch, the implementations are quite separate from each
other. One initial idea was to use the existing mechanism of code
loading to do a dummy load operation that would make a copy of the
affected modules. That copy could then be instrumented with
breakpoints before making it reachable with the same atomic switch as
done for code loading. This approach seems straight forward but has a
number of shortcomings, one being the large memory footprint when many
modules are instrumented. Another problem is how execution will reach
the new instrumented code. Normally loaded code can only be reached
through external functions calls. Trace settings must be activated
instantaneously without the need of external function calls.

The chosen solution is instead for tracing to use the technique of
replication applied on the data structures for breakpoints. Two
generations of breakpoints are kept and indentified by index of 0 and
1. The global atomic variables `erts_active_bp_index` will determine
which generation of breakpoints running code will use.

### Atomicity Without Atomic Operations

Not using the code loading generations (or any other code duplication)
means that `trace_pattern` must at some point write to the active beam
code in order for running processes to reach the staged breakpoints
structures. This can be done with one single atomic write operation
per instrumented function. The beam instruction words are however read
with normal memory loads and not through the atomic API. The only
guarantee we need is that the written instruction word is seen as
atomic. Either fully written or not at all. This is true for word
aligned write operation on all hardware architectures we use.


Adding a new Breakpoint
-----------------------

This is a simplified sequence describing what `trace_pattern` goes
through when adding a new breakpoint.

1. Seize exclusive code write permission (suspend process until we get it).

2. Allocate breakpoint structure `GenericBp` including both generations.
   Set the active part as disabled with a zeroed flagfield. Save the original
   instruction word in the breakpoint.

3. Write a pointer to the breakpoint at offset -4 from the first
   instruction "func\_info" header.

4. Set the staging part of the breakpoint as enabled with specified
   breakpoint data.

5. Wait for thread progress.

6. Write a `op_i_generic_breakpoint` as the first instruction for the function.
   This instruction will execute the breakpoint that it finds at offset -4.

7. Wait for thread progress.

8. Commit the breadpoint by switching `erts_active_bp_index`.

9. Wait for thread progress.

10. Prepare for next call to `trace_pattern` by updating the new staging part
    (the old active) of the breakpoint to be identic to the the new active part.

11. Release code write permission and return from `trace_pattern`.


The code write permission "lock" seized in step 1 is the same as used
by code loading. This will ensure that only one process at a time can
stage new trace settings but it will also prevent concurrent code
loading and make sure we see a consistent view of the beam code during
the entire sequence.

Between step 6 and 8, runninng processes might execute the written
`op_i_generic_breakpoint` instruction. They will get the breakpoint
structure written in step 3, read `erts_active_bp_index` and execute
the corresponding part of the breakpoint. Before the switch in step 8
becomes visible they will however execute the disabled part of the
breakpoint structure and do nothing other than executing the saved
original instruction.


To Updating and Remove Breakpoints
----------------------------------

The above sequence did only describe adding a new breakpoint. We do
basically the same sequence to update the settings of an existing
breakpoint except step 2,3 and 6 can be skipped as it has already been
done.

To remove a breakpoint some more steps are needed. The idea is to
first stage the breakpoint as disabled, do the switch, wait for thread
progress and then remove the disabled breakpoint by restoring the
original beam instruction.

Here is a more complete sequence that contains both adding, updating
and removing breakpoints.

1. Seize exclusive code write permission (suspend process until we get it).

2. Allocate new breakpoint structures with a disabled active part and
   the original beam instruction. Write a pointer to the breakpoint in
   "func\_info" header at offset -4.

3. Update the staging part of all affected breakpoints. Disable
   breakpoints that are to be removed.

4. Wait for thread progress.

5. Write a `op_i_generic_breakpoint` as the first instruction for all
   functions with new breakpoints.

6. Wait for thread progress.

7. Commit all staged breadpoints by switching `erts_active_bp_index`.

8. Wait for thread progress.


9. Restore original beam instruction for disabled breakpoints.

10. Wait for thread progress.

11. Prepare for next call to `trace_pattern` by updating the new
    staging area (the old active) for all enabled breakpoints.

12. Deallocate disabled breakpoint structures.

13. Release code write permission and return from `trace_pattern`.


### All that Waiting for Thread Progress

There are four rounds of waiting for thread progress in the above
sequence. In the code loading sequence we sacrificed memory overhead
of three generations to avoid a second round of thread progress. The
latency of `trace_pattern` should not be such a big problem for
however, as it is normally not called in a rapid sequence.

The waiting in step 4 is to make sure all threads will see an updated
view of the breakpoint structures once they become reachable through
the `op_i_generic_breakpoint` instruction written in step 5.

The waiting in step 6 is to make the activation of the new trace
settings "as atomic as possible". Different cores might see the new
value of `erts_active_bp_index` at different times as it is read
without any memory barrier. But this is the best we can do without
more expensive thread synchronization.

The waiting in step 8 is to make sure we dont't restore the original
bream instructions for disabled breakpoints until we know that no
thread is still accessing the old enabled part of a disabled
breakpoint.

The waiting in step 10 is to make sure no lingering thread is still
accessing disabled breakpoint structures to be deallocated in step
12.


Global Tracing
--------------

Call tracing with `global` option only affects external function
calls. This was earlier handled by inserting a special trace
instruction in export entries without the use of breakpoints. With the
new non-blocking tracing we want to avoid special handling for global
tracing and make use of the staging and atomic switching within the
breakpoint mechanism. The solution was to create the same type of
breakpoint structure for a global call trace. The difference to local
tracing is that we insert the `op_i_generic_breakpoint` instruction
(with its pointer at offset -4) in the export entry rather than in the
code.


Future work
-----------

We still go to single threaded mode when new code is loaded for a
module that is traced, or when loading code when there is a default
trace pattern set. That is not impossible to fix, but that requires
much closer cooperation between tracing BIFs and the loader BIFs.
