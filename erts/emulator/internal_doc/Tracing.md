<!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2014-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
-->

# Tracing

## Implementation
### Call, return, and exception tracing

Tracing is implemented by setting breakpoints in the traced functions,
and sending the appropriate trace messages when they're hit.

* Call trace messages are sent immediately.
* Return tracing pushes a frame to the stack which _returns to_ an
  instruction that sends a trace message when encountered.
* Exception tracing also pushes a frame to the stack, but will only send
  a trace message if we encounter it in stack scanning while throwing an
  exception.

This means that one must be careful not to use return or exception tracing
on functions that never return, as each call pushes a frame that will
never be removed.

Another limitation is that since the breakpoint is in the _callee_ and not
the _caller_, we're limited to the information we have on function ingress.
This means that we can't actually tell who called us: since we're limited
to inspecting the stack we can only say where we're _going to return to_,
which is not quite the same thing.

As an illustration, when the `caller` option is enabled all trace messages
from `bar/1`  will say that they were called from `foo/0`, even though it
went through a bunch of other functions on the way:

    foo() ->
        lots(),
        ok.
    
    lots() ->
        'of'().
    
    'of'() ->
        indirections().
    
    indirections() ->
        bar(10).
    
    bar(0) ->
        done;
    bar(N) ->
        bar(N - 1).

### Export tracing

In the interpreter, breakpoints are set inside the code trampoline for
export entries, and their address vector is updated to point to them.
This way, only remote calls will hit the breakpoint while local calls to
the same function are left alone, but it otherwise acts the same way as
local breakpoints.

Things get a bit more involved in the JIT. See `BeamAsm.md` for more
details.

### Trace sessions

Since OTP 27, isolated trace session can be dynamically created. Each trace
session is represented by an instance of struct `ErtsTraceSession`. The old
legacy session (kept for backward compatibility) is represented by the static
instance `erts_trace_session_0`.

## Setting breakpoints

### Introduction

Before OTP R16 when trace settings were changed by `erlang:trace_pattern`,
all other execution in the VM were halted while the trace operation
was carried out in single threaded mode. Similar to code loading, this
can impose a severe problem for availability that grows with the
number of cores.

Since OTP R16, breakpoints are set in the code without blocking the VM.
Erlang processes may continue executing undisturbed in parallel during the
entire operation. The same base technique is used as for code loading. A
staging area of breakpoints is prepared and then made active with a single
atomic operation.

### Breakpoints

For call tracing, breakpoints are created and inserted in the ingress of each
traced Erlang function. A pointer to the allocated struct `GenericBp` is
inserted that holds all the data for all types of breakpoints. A bit-flag field
is used to indicate what different type of break actions that are
enabled. Struct `GenericBp` is session specific. If more than one trace session
affects a function, one `GenericBp` instance is created for each session. They
are linked together in a singly linked list that is traversed when the
breakpoint is hit.

### Similar to Code Loading but Different

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
generations of breakpoints are kept and identified by index of 0 and
1\. The global atomic variables `erts_active_bp_index` will determine
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

### Adding a new Breakpoint

This is a simplified sequence describing what `trace_pattern` goes
through when adding a new breakpoint.

1. Seize exclusive code modification permission (suspend process until we get
   it).

2. Allocate breakpoint structure `GenericBp` including both generations.
   Set the active area as disabled with a zeroed flagfield. Save the original
   instruction word in the breakpoint.

3. Write a pointer to the breakpoint at offset `-sizeof(UWord)` from the first
   instruction `ErtsFuncInfo` header.

4. Set the staging area of the breakpoint as enabled with specified
   breakpoint data.

5. Wait for thread progress.

6. Write a `op_i_generic_breakpoint` as the first instruction for the function.
   This instruction will execute the breakpoint that it finds at offset
   `-sizeof(UWord)`.

7. Wait for thread progress.

8. Commit the breakpoint by switching `erts_active_bp_index`.

9. Wait for thread progress.

10. "Consolidate"
    Prepare for next call to `trace_pattern` by updating the new staging area
    (the old active) of the breakpoint to be identical to the new active area.

11. Release code modification permission and return from `trace_pattern`.


The code modification permission "lock" seized in step 1 is also taken by code
loading. This ensures that only one process at a time can stage new trace
settings, and also prevents concurrent codeloading and make sure we see a
consistent view of the beam code during the entire sequence.

Between step 6 and 8, runninng processes might execute the written
`op_i_generic_breakpoint` instruction. They will get the breakpoint
structure written in step 3, read `erts_active_bp_index` and execute
the corresponding part of the breakpoint. Before the switch in step 8
becomes visible they will however execute the disabled part of the
breakpoint structure and do nothing other than executing the saved
original instruction.

The consolidation in step 10 will make the new staging area identical
to the new active area. This will make it simpler for the next call to
`trace_pattern` that may not affect all existing breakpoints. The staging area
of all unaffected breakpoints are then ready to become active without any
visitation by `trace_pattern`.

###  To Update and Remove Breakpoints

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

1. Seize exclusive code modification permission (suspend process until we get
   it).

2. Allocate new breakpoint structures with a disabled active area and
   the original beam instruction. Write a pointer to the breakpoint in
   `ErtsFuncInfo` header at offset `-sizeof(UWord)`.

3. Update the staging area of all affected breakpoints. Disable
   breakpoints that are to be removed.

4. Wait for thread progress.

5. Write a `op_i_generic_breakpoint` as the first instruction for all
   functions with new breakpoints.

6. Wait for thread progress.

7. Commit all staged breakpoints by switching `erts_active_bp_index`.

8. Wait for thread progress.


9. Uninstall.
   Restore original beam instruction for disabled breakpoints.

10. Wait for thread progress.

11. Consolidate.
    Prepare for next call to `trace_pattern` by updating the new
    staging area (the old active) for all enabled breakpoints.

12. Deallocate disabled breakpoint structures.

13. Release code modification permission and return from `trace_pattern`.

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

The waiting in step 8 is to make sure we don't restore the original
bream instructions for disabled breakpoints until we know that no
thread is still accessing the old enabled area of a disabled
breakpoint.

The waiting in step 10 is to make sure no lingering thread is still
accessing disabled breakpoint structures to be deallocated in step
12.

### Global Tracing

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

### call_time and call_memory tracing

For profiling, `call_time` and/or `call_memory` tracing can be set for a function.
This will measure the time/memory spent by a function. The measured
time/memory is kept in individual counters for every call traced process
calling that function. To ensure scalability, scheduler specific hash tables
(`BpTimemTrace`) are used in the breakpoint to map the calling process pid to
its time/memory counters.

Function `trace:info` is used to collect stats for `call_time`, `call_memory`
or both (`all`). It has to aggregate the counters from all those scheduler
specific hash tables to build a list with one tuple with counters for each
pid. This cannot be done safely while the hash tables may be concurrently
updated by traced processes.

Since OTP 29, `trace:info` collects `call_time` and `call_memory` stats without
blocking all schedulers from running. This is done by using the active and
staging halves of the breakpoint. During normal operations both halves of the
breakpoint refer to the same thread specific hash tables. To collect the stats
safely, temporary hash tables are created to be used by traced calls happening
during the call to `trace:info`. The temporary hash tables are being made active
while the "real" hash tables are made inactive in the staging half. When the hash
tables are inactive, they can be safely traversed. When done, the real
tables are made active again. A final consolidation step is done to collect any
stats from the temporary tables, delete them and make the two halves of the
breakpoint identical again using the same real hash tables. Scheduling with
thread progress is done between the switching to make sure the traversed hash
tables are not being concurrently updated.

### Future work

We still go to single threaded mode when new code is loaded for a
module that is traced, or when loading code when there is a default
trace pattern set. That is not impossible to fix, but that requires
much closer cooperation between tracing BIFs and the loader BIFs.
