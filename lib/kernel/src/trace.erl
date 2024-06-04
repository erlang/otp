%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2024. All Rights Reserved.
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
%%

-module(trace).
-moduledoc #{since => "OTP 27.0"}.
-moduledoc """
The Erlang trace interface.

The Erlang run-time system exposes several trace points that allow users
to be notified when they are triggered. Trace points are things such as
function calls, message sending and receiving, garbage collection, and
process scheduling.

The functions in this module can be used directly, but can also be
used as building blocks to build more sophisticated debugging or
profiling tools. For debugging Erlang code it is recommended to use
`m:dbg` and for profiling to use `m:tprof`.

## Trace Sessions

All tracing is done within a trace session. Trace sessions can be
[created](`session_create/3`) and [destroyed](`session_destroy/1`)
dynamically. Each session has its own tracer that will receive all trace
messages. Several sessions can exist at the same time without interfering with
each other. When a trace session is destroyed, all its trace settings are
automatically cleaned up.

*Example*:

```erlang
%% Create a tracer process that will receive the trace events
1> Tracer = spawn(fun F() -> receive M -> io:format("~p~n",[M]), F() end end).
<0.91.0>
%% Create a session using the Tracer
2> Session = trace:session_create(my_session, Tracer, []).
{#Ref<0.1543805153.1548353537.92331>,{my_session, 0}}
%% Setup call tracing on self()
3> trace:process(Session, self(), true, [call]).
1
%% Setup call tracing on lists:seq/2
4> trace:function(Session, {lists,seq,2}, [], []).
1
%% Call the traced function
5> lists:seq(1, 10).
{trace,<0.89.0>,call,{lists,seq,[1,10]}} % The trace message
[1,2,3,4,5,6,7,8,9,10] % The return value
%% Cleanup the trace session
6> trace:session_destroy(Session).
ok
```

## Node Local Tracing Only

The functions in this module only operates on the local node. That is, both the
traced processes/ports as well as the tracer process/port/module must all reside
on the same local node as the call is made. To trace remote nodes use `m:dbg` or
`m:ttb`.

> #### Change {: .info }
>
> This `trace` module was introduced in OTP 27.0. The interface and semantics are
> similar to the older functions `erlang:trace/3`, `erlang:trace_pattern/3`,
> and `erlang:trace_info/2`.
>
> The main difference is the old functions operate on a single static
> trace session per node. That could impose the problem that different
> users and tools would interfere with each other's trace settings. The new trace
> functions in this module all operate on dynamically created trace sesssions
> isolated from each other. Also, this makes it easier to safely disable all trace
> settings when done by a single call to `session_destroy/1`.
>
> To change an existing tool to use the interface the following table can be
> useful:
>
> | Old function call                            | corresponds to                                                     |
> | -------------------------------------------  | ------------------------------------------------------------------ |
> | [`erlang:trace(Pid, ...)`][1]                | [`process(S, Pid, ...)`][p]                                        |
> | [`erlang:trace(processes, ...)`][1]          | [`process(S, all, ...)`][p]                                        |
> | [`erlang:trace(existing_processes, ...)`][1] | [`process(S, existing, ...)`][p]                                   |
> | [`erlang:trace(new_processes, ...)`][1]      | [`process(S, new, ...)`][p]                                        |
> | [`erlang:trace(Port, ...)`][1]               | [`port(S, Port, ...)`][o]                                          |
> | [`erlang:trace(ports, ...)`][1]              | [`port(S, all, ...)`][o]                                           |
> | [`erlang:trace(existing_ports, ...)`][1]     | [`port(S, existing, ...)`][o]                                      |
> | [`erlang:trace(new_ports, ...)`][1]          | [`port(S, new, ...)`][o]                                           |
> | [`erlang:trace(all, ...)`][1]                | [`process(S, all, ...)`][p] and [`port(S, all, ...)`][o]           |
> | [`erlang:trace(existing, ...)`][1]           | [`process(S, existing, ...)`][p] and [`port(S, existing, ...)`][o] |
> | [`erlang:trace(new, ...)`][1]                | [`process(S, new, ...)`][p] and [`port(S, new, ...)`][o]           |
> | [`erlang:trace_pattern(MFA, ...)`][2]        | [`function(S, MFA, ...)`][f]                                       |
> | [`erlang:trace_pattern(send, ...)`][2]       | [`send(S, ...)`][s]                                                |
> | [`erlang:trace_pattern('receive', ...)`][2]  | [`recv(S, ...)`][r]                                                |
> | [`erlang:trace_info(...)`][3]                | [`info(S, ...)`][i]                                                |
>
> Argument `S` is the trace session that must first be created with
> `session_create/3`. The other arguments (implied by `...`) are mostly the
> same. The only other difference is that the tracer is always the tracer
> specified when the session was created. Options `{tracer,T}`, `{tracer,M,S}`,
> `{meta,T}`, and `{meta,M,S}` are therefore not allowed, and the default tracer is
> never the calling process.

[1]: `erlang:trace/3`
[2]: `erlang:trace_pattern/3`
[3]: `erlang:trace_info/2`
[p]: `process/4`
[o]: `port/4`
[f]: `function/4`
[s]: `send/3`
[r]: `recv/3`
[i]: `info/3`
""".

-export([function/4,
         send/3,
         recv/3,
         process/4,
         port/4,
         info/3,
         delivered/2,
         session_create/3,
         session_destroy/1,
         session_info/1]).


%% We must inline these functions so that the stacktrace points to
%% the correct function.
-compile({inline, [error_with_inherited_info/3]}).


-export_type([session/0,
              session_strong_ref/0,
              session_weak_ref/0]).

-doc """
A handle to an isolated trace session.
""".
-doc #{ since => "OTP 27.0" }.
-type session() :: {session_strong_ref(), session_weak_ref()}
                   | session_weak_ref().
-opaque session_strong_ref() :: reference().

-doc """
A weak session handle as returned by `session_info/1`. A weak session handle can
be used like a full session handle, but it will not prevent the session from
being destroyed when the last strong handle is garbage collected.
""".
-opaque session_weak_ref() :: {atom(), integer()}.

-type trace_info_flag() ::
      arity |
      call |
      exiting |
      garbage_collection |
      monotonic_timestamp |
      procs |
      ports |
      'receive' |
      return_to |
      running |
      running_procs |
      running_ports |
      send |
      set_on_first_link |
      set_on_first_spawn |
      set_on_link |
      set_on_spawn |
      silent |
      strict_monotonic_timestamp |
      timestamp.

-type trace_flag() ::
      trace_info_flag() | all | cpu_timestamp.

-type trace_pattern_mfa() ::
      {atom(),atom(),arity() | '_'}.

-type match_variable() :: atom(). % Approximation of '$1' | '$2' | ...
-type trace_match_spec() ::
      [{[term()] | '_' | match_variable() ,[term()],[term()]}].

-type trace_pattern_flag() ::
      global | local |
      meta | {meta, Pid :: pid()} |
      {meta, TracerModule :: module(), TracerState :: term()} |
      call_count |
      call_time |
      call_memory.

-type trace_info_item_result() ::
       {traced, global | local | false | undefined} |
       {match_spec, trace_match_spec() | false | undefined} |
       {meta, pid() | port() | false | undefined | []} |
       {meta, module(), term() } |
       {meta_match_spec, trace_match_spec() | false | undefined} |
       {call_count, non_neg_integer() | boolean() | undefined} |
       {call_time | call_memory, [{pid(), non_neg_integer(),
		     non_neg_integer(), non_neg_integer()}] | boolean() | undefined}.

-type trace_info_return() ::
      undefined |
      {flags, [trace_info_flag()]} |
      {tracer, pid() | port() | []} |
      {tracer, module(), term()} |
      trace_info_item_result() |
      {all, [ trace_info_item_result() ] | false | undefined}.


%% process/4
-doc """
Turn on or off trace flags for one or more processes.

Argument `Session` is the trace session to operate on as returned by
`session_create/3`.

Argument `Procs` is either a process identifier (pid) for a local process or
one of the following atoms:

- **`all`** - All currently existing processes and all that will be
  created in the future.

- **`existing`** - All currently existing processes.

- **`new`** - All processes that will be created in the future.

Argument `How` is either `true` to turn on trace flags or `false` to turn them off.

Argument `FlagList` can contain any number of the following flags (the "message
tags" refers to the list of [`trace messages`](#process_trace_messages)):

- **`all`** - Sets all trace flags except `cpu_timestamp`, which
  is in its nature different than the others.

- **`send`** - Traces sending of messages. Limit which sent messages to trace by
    calling `send/3`.

  Message tags: [`send`](#process_trace_messages_send) and
  [`send_to_non_existing_process`](#process_trace_messages_send_to_non_existing_process).

- **`'receive'`** - Traces receiving of messages. Limit which received messages
    to trace by calling `recv/3`.

  Message tags: [`'receive'`](#process_trace_messages_receive).

- **`call`** - Traces certain function calls. Specify which function calls to
  trace by calling `function/4`.

  Message tags: [`call`](#process_trace_messages_call) and
  [`return_from`](#process_trace_messages_return_from).

- **`silent`** - Used with the `call` trace flag. The `call`, `return_from`, and
  `return_to` trace messages are inhibited if this flag is set, but they are
  executed as normal if there are match specifications.

  Silent mode is inhibited by executing `trace:process(_, _, false, [silent|_])`, or
  by a match specification executing the function `{silent, false}`.

  The `silent` trace flag facilitates setting up a trace on many or even all
  processes in the system. The trace can then be activated and deactivated using
  the match specification function `{silent,Bool}`, giving a high degree of
  control of which functions with which arguments that trigger the trace.

  Message tags: [`call`](#process_trace_messages_call),
  [`return_from`](#process_trace_messages_return_from), and
  [`return_to`](#process_trace_messages_return_to). Or rather, the
  absence of.

- **`return_to`** - Used with the `call` trace flag. Traces the exit from
  call traced functions back to where the execution resumes. Only works for
  functions traced with option `local` to `function/4`.

  The semantics is that a `return_to` trace message is sent when a call traced
  function returns or throws and exception that is caught. For tail calls, only
  one trace message is sent per chain of tail calls, so the properties of tail
  recursiveness for function calls are kept while tracing with this
  flag. Similar for exceptions, only one `return_to` trace message is sent, even
  if the exception passed more than one call traced function before it was
  caught.

  Using `call` and `return_to` trace together makes it possible to know exactly
  in which function a process executes at any time.

  To get trace messages containing return values from functions, use the
  `{return_trace}` match specification action instead.

  Message tags: [`return_to`](#process_trace_messages_return_to).

- **`procs`** - Traces process-related events.

  Message tags: [`spawn`](#process_trace_messages_spawn),
  [`spawned`](#process_trace_messages_spawned),
  [`exit`](#process_trace_messages_exit),
  [`register`](#process_trace_messages_register),
  [`unregister`](#process_trace_messages_unregister),
  [`link`](#process_trace_messages_link),
  [`unlink`](#process_trace_messages_unlink),
  [`getting_linked`](#process_trace_messages_getting_linked), and
  [`getting_unlinked`](#process_trace_messages_getting_unlinked).

- **`running`** - Traces scheduling of processes.

  Message tags: [`in`](#process_trace_messages_in_proc) and
  [`out`](#process_trace_messages_out_proc).

- **`exiting`** - Traces scheduling of exiting processes.

  Message tags:
  [`in_exiting`](#process_trace_messages_in_exiting_proc),
  [`out_exiting`](#process_trace_messages_out_exiting_proc), and
  [`out_exited`](#process_trace_messages_out_exited_proc).

- **`running_procs`** - Traces scheduling of processes just like `running`.
  However, this option also includes schedule events when the process executes
  within the context of a port without being scheduled out itself.

  Message tags: [`in`](#process_trace_messages_in_proc) and
  [`out`](#process_trace_messages_out_proc).

- **`garbage_collection`** - Traces garbage collections of processes.

  Message tags:
  [`gc_minor_start`](#process_trace_messages_gc_minor_start),
  [`gc_max_heap_size`](#process_trace_messages_gc_max_heap_size), and
  [`gc_minor_end`](#process_trace_messages_gc_minor_end).

- **`timestamp`{: #timestamp }** - Includes a time stamp in all trace
  messages. The time stamp (Ts) has the same form as returned by
  `erlang:now/0`.

- **`cpu_timestamp`** - A global trace flag for the Erlang node that makes all
  trace time stamps using flag `timestamp` to be in CPU time, not wall clock
  time. That is, `cpu_timestamp` is not be used if `monotonic_timestamp` or
  `strict_monotonic_timestamp` is enabled. Only allowed with `Procs==all`.
  If the host machine OS does not support high-resolution CPU time measurements,
  `process/4` exits with `badarg`. Notice that most OS do not
  synchronize this value across cores, so be prepared that time can seem to go
  backwards when using this option.

- **`monotonic_timestamp`** - Includes an
  [Erlang monotonic time](`e:erts:time_correction.md#erlang-monotonic-time`) time stamp
  in all trace messages. The time stamp (Ts) has the same format and value as
  produced by [`erlang:monotonic_time(nanosecond)`](`erlang:monotonic_time/1`). This
  flag overrides flag `cpu_timestamp`.

- **`strict_monotonic_timestamp`** - Includes an time stamp consisting of
  [Erlang monotonic time](`e:erts:time_correction.md#erlang-monotonic-time`) and a
  monotonically increasing integer in all trace messages. The time stamp (Ts)
  has the same format and value as produced by `{`
  [`erlang:monotonic_time(nanosecond)`](`erlang:monotonic_time/1`)`,`
  [`erlang:unique_integer([monotonic])`](`erlang:unique_integer/1`)`}`. This flag
  overrides flag `cpu_timestamp`.

  If multiple time stamp flags are passed, `timestamp` has precedence over
  `strict_monotonic_timestamp`, which in turn has precedence over
  `monotonic_timestamp`. All time stamp flags are remembered, so if two are
  passed and the one with highest precedence later is disabled, the other one
  becomes active.

- **`arity`** - Used with the `call` trace flag. `{M, F, Arity}` is specified
  instead of `{M, F, Args}` in call trace messages.

- **`set_on_spawn`** - Makes any process created by a traced process inherit all
  its trace flags, including flag `set_on_spawn` itself.

- **`set_on_first_spawn`** - Makes the first process created by a traced process
  inherit all its trace flags, excluding flag `set_on_first_spawn` itself. That
  is, after the first spawn is done, `set_on_first_spawn` will be cleared in
  both the spawned process and the spawning process.

  If both are set, `set_on_first_spawn` will supersede `set_on_spawn`.

- **`set_on_link`** - Makes any process linked by a traced process inherit all
  its trace flags, including flag `set_on_link` itself.

- **`set_on_first_link`** - Makes the first process linked to by a traced
  process inherit all its trace flags, excluding flag `set_on_first_link`
  itself. That is, after the first link is done, `set_on_first_link` will be
  cleared in both the linked process and the linking process.

  If both are set, `set_on_first_link` will supersede `set_on_link`.

The tracing process receives the _trace messages_ described in the following
list. `Pid` is the process identifier of the traced process in which the traced
event has occurred. The third tuple element is the message tag.

If flag `timestamp`, `strict_monotonic_timestamp`, or `monotonic_timestamp` is
specified, the first tuple element is `trace_ts` instead, and the time stamp is
added as an extra element last in the message tuple.

If a match specification (applicable only for `call`, `send`, and `'receive'`
tracing) contains a `{message}` action function with a non-boolean value, that
value is added as an extra element to the message tuple either in the last
position or before the timestamp (if it is present).

Trace messages:

[](){: #process_trace_messages }

- **`{trace, Pid, send, Msg, To}`{: #process_trace_messages_send }** - When
  process `Pid` sends message `Msg` to process `To`.

- **`{trace, Pid, send_to_non_existing_process, Msg, To}`{:
  #process_trace_messages_send_to_non_existing_process }** - When process `Pid`
  sends message `Msg` to the non-existing process `To`.

- **`{trace, Pid, 'receive', Msg}`{: #process_trace_messages_receive }** -
  When process `Pid` receives message `Msg`. If `Msg` is set to time-out, a receive
  statement can have timed out, or the process received a message with the
  payload `timeout`.

- **`{trace, Pid, call, {M, F, Args}}`{: #process_trace_messages_call }** - When
  process `Pid` calls a traced function. The return values of calls are never
  supplied, only the call and its arguments.

  Trace flag `arity` can be used to change the contents of this message, so that
  `Arity` is specified instead of `Args`.

- **`{trace, Pid, return_to, {M, F, Arity}}`{: #process_trace_messages_return_to
  }** - When process `Pid` returns _to_ the specified function. This trace
  message is sent if both the flags `call` and `return_to` are set, and the
  function is set to be traced on _local_ function calls. The message is only
  sent when returning from a chain of tail recursive function calls, where at
  least one call generated a `call` trace message (that is, the functions match
  specification matched, and `{message, false}` was not an action).

- **`{trace, Pid, return_from, {M, F, Arity}, ReturnValue}`{:
  #process_trace_messages_return_from }** - When `Pid` returns _from_ the
  specified function. This trace message is sent if flag `call` is set, and the
  function has a match specification with a `return_trace` or `exception_trace`
  action.

- **`{trace, Pid, exception_from, {M, F, Arity}, {Class, Value}}`{:
  #process_trace_messages_exception_from }** - When `Pid` exits _from_ the
  specified function because of an exception. This trace message is sent if flag
  `call` is set, and the function has a match specification with an
  `exception_trace` action.

- **`{trace, Pid, spawn, Pid2, {M, F, Args}}`{: #process_trace_messages_spawn
  }** - When `Pid` spawns a new process `Pid2` with the specified function call
  as entry point.

  `Args` is supposed to be the argument list, but can be any term if the spawn
  is erroneous.

- **`{trace, Pid, spawned, Pid2, {M, F, Args}}`{:
  #process_trace_messages_spawned }** - When `Pid` is spawned by process `Pid2`
  with the specified function call as entry point.

  `Args` is supposed to be the argument list, but can be any term if the spawn
  is erroneous.

- **`{trace, Pid, exit, Reason}`{: #process_trace_messages_exit }** - When `Pid`
  exits with reason `Reason`.

- **`{trace, Pid, register, RegName}`{: #process_trace_messages_register
  }** - When process `Pid` gets the name `RegName` registered.

- **`{trace, Pid, unregister, RegName}`{: #process_trace_messages_unregister
  }** - When process `Pid` gets the name `RegName` unregistered. This is done
  automatically when a registered process or port exits.

- **`{trace, Pid, link, Pid2}`{: #process_trace_messages_link }** - When `Pid`
  links to a process `Pid2`.

- **`{trace, Pid, unlink, Pid2}`{: #process_trace_messages_unlink }** - When
  `Pid` removes the link from a process `Pid2`.

- **`{trace, Pid, getting_linked, Pid2}`{:
  #process_trace_messages_getting_linked }** - When `Pid` gets linked to a
  process `Pid2`.

- **`{trace, Pid, getting_unlinked, Pid2}`{:
  #process_trace_messages_getting_unlinked }** - When `Pid` gets unlinked
  from a process `Pid2`.

- **`{trace, Port, open, Pid, Driver}`{: #process_trace_messages_open }** - When
  `Pid` opens a new port `Port` with the running `Driver`.

  `Driver` is the name of the driver as an atom.

- **[](){: #process_trace_messages_in_proc }
  `{trace, Pid, in | in_exiting, {M, F, Arity} | 0}`{:
  #process_trace_messages_in_exiting_proc }**
  When `Pid` is scheduled to run. The process runs in function `{M, F, Arity}`.
  On some rare occasions, the current function cannot be determined, then the
  last element is `0`.

- **[](){: #process_trace_messages_out_proc } [](){:
  #process_trace_messages_out_exiting_proc }
  `{trace, Pid, out | out_exiting | out_exited, {M, F, Arity} | 0}`{:
  #process_trace_messages_out_exited_proc }**
  When `Pid` is scheduled out. The process was running in function `{M, F,
  Arity}`. On some rare occasions, the current function cannot be determined,
  then the last element is `0`.

- **`{trace, Pid, gc_minor_start, Info}`{:
  #process_trace_messages_gc_minor_start }** - [](){: #gc_minor_start } Sent
  when a garbage collection of the young generation is about to be started.
  `Info` is a list of two-element tuples, where the first element is a key,
  and the second is the value. Do not depend on any order of the tuples.
  The following keys are defined:

  - **`heap_size`** - The size of the used part of the heap.

  - **`heap_block_size`** - The size of the memory block used for storing the
    heap and the stack.

  - **`old_heap_size`** - The size of the used part of the old heap.

  - **`old_heap_block_size`** - The size of the memory block used for storing
    the old heap.

  - **`stack_size`** - The size of the stack.

  - **`recent_size`** - The size of the data that survived the previous garbage
    collection.

  - **`mbuf_size`** - The combined size of message buffers associated with the
    process.

  - **`bin_vheap_size`** - The total size of unique off-heap binaries referenced
    from the process heap.

  - **`bin_vheap_block_size`** - The total size of binaries allowed in the
    virtual heap in the process before doing a garbage collection.

  - **`bin_old_vheap_size`** - The total size of unique off-heap binaries
    referenced from the process old heap.

  - **`bin_old_vheap_block_size`** - The total size of binaries allowed in the
    virtual old heap in the process before doing a garbage collection.

  - **`wordsize`** - For the `gc_minor_start` event it is the size of the need
    that triggered the GC. For the corresponding `gc_minor_end` event it is the
    size of reclaimed memory = start `heap_size` - end `heap_size`.

  All sizes are in words.

- **`{trace, Pid, gc_max_heap_size, Info}`{:
  #process_trace_messages_gc_max_heap_size }** - Sent when the
  [`max_heap_size`](`e:erts:erlang#process_flag_max_heap_size`) is reached during
  garbage collection. `Info` contains the same kind of list as in message
  `gc_start`, but the sizes reflect the sizes that triggered `max_heap_size` to
  be reached.

- **`{trace, Pid, gc_minor_end, Info}`{: #process_trace_messages_gc_minor_end
  }** - Sent when young garbage collection is finished. `Info` contains the same
  kind of list as in message `gc_minor_start`, but the sizes reflect the new
  sizes after garbage collection.

- **`{trace, Pid, gc_major_start, Info}`{:
  #process_trace_messages_gc_major_start }** - Sent when fullsweep garbage
  collection is about to be started. `Info` contains the same kind of list as in
  message `gc_minor_start`.

- **`{trace, Pid, gc_major_end, Info}`{: #process_trace_messages_gc_major_end
  }** - Sent when fullsweep garbage collection is finished. `Info` contains the
  same kind of list as in message `gc_minor_start`, but the sizes reflect the
  new sizes after a fullsweep garbage collection.

If the tracing process dies or the tracer module returns `remove`, the
flags are silently removed.

Returns a number indicating the number of processes that matched `Procs`.
If `Procs` is a process identifier, the return value is `1`. If
`Procs` is `all` or `existing`, the return value is the number of
processes running. If `Procs` is `new`, the return value is `0`.

Failure: `badarg` if the specified arguments are not supported. For example,
`cpu_timestamp` is not supported on all platforms.
""".
-doc #{ since => <<"OTP 27.0">> }.
-spec process(Session, Procs, How, FlagList) -> integer() when
      Session :: session(),
      Procs :: pid() | all | existing | new,
      How :: boolean(),
      FlagList :: [trace_flag()].
process(Session, Procs, How, FlagList) ->
    ensure_tracer_module_loaded(tracer, FlagList),
    try
        BifProcs = case Procs of
                       _ when is_pid(Procs) -> Procs;
                       all -> processes;
                       existing -> existing_processes;
                       new -> new_processes;
                       processes -> Procs;
                       existing_processes -> Procs;
                       new_processes -> Procs
                   end,
        erts_internal:trace(Session, BifProcs, How, FlagList)
    catch
        error:R:Stk ->
            error_with_inherited_info(R, [Session, Procs, How, FlagList], Stk)
    end.


%% port/4
-doc """
Turn on or off trace flags for one or more ports.

Argument `Session` is the trace session to operate on as returned by
`session_create/3`.

`Ports` is either a port identifier for a local port or one of the following atoms:

- **`all`** - All currently existing ports and all that will be
  created in the future.

- **`existing`** - All currently existing ports.

- **`new`** - All ports that will be created in the future.

`FlagList` can contain any number of the following flags (the "message tags"
refers to the list of [`trace messages`](#port_trace_messages)):

- **`all`** - Sets all trace flags except `cpu_timestamp`, which are in its
    nature different than the others.

- **`send`** - Traces sending of messages.

  Message tags: [`send`](#port_trace_messages_send) and
  [`send_to_non_existing_process`](#port_trace_messages_send_to_non_existing_process).

- **`'receive'`** - Traces receiving of messages.

  Message tags: [`'receive'`](#port_trace_messages_receive).

- **`ports`** - Traces port-related events.

  Message tags: [`open`](#port_trace_messages_open),
  [`closed`](#port_trace_messages_closed),
  [`register`](#port_trace_messages_register),
  [`unregister`](#port_trace_messages_unregister),
  [`getting_linked`](#port_trace_messages_getting_linked), and
  [`getting_unlinked`](#port_trace_messages_getting_unlinked).

- **`running_ports`** - Traces scheduling of ports.

  Message tags: [`in`](#port_trace_messages_in_port) and
  [`out`](#port_trace_messages_out_port).

- **`timestamp`**, **`cpu_timestamp`**, **`monotonic_timestamp`**,
  **`strict_monotonic_timestamp`** - Same as for timestamps in
  [`process/4`](#timestamp).

The tracing process receives the _trace messages_ described in the following
list. `Port` is the port identifier of the traced port in which the traced
event has occurred. The third tuple element is the message tag.

If flag `timestamp`, `strict_monotonic_timestamp`, or `monotonic_timestamp` is
specified, the first tuple element is `trace_ts` instead, and the time stamp is
added as an extra element last in the message tuple. If multiple time stamp
flags are passed, `timestamp` has precedence over `strict_monotonic_timestamp`,
which in turn has precedence over `monotonic_timestamp`. All time stamp flags
are remembered, so if two are passed and the one with highest precedence later
is disabled, the other one becomes active.

If a match specification (applicable only for `send` and `'receive'`
tracing) contains a `{message}` action function with a non-boolean value, that
value is added as an extra element to the message tuple either in the last
position or before the timestamp (if it is present).

Trace messages:

[](){: #port_trace_messages }

- **`{trace, Port, send, Msg, To}`{: #port_trace_messages_send }** - When
  `Port` sends message `Msg` to process `To`.

- **`{trace, Port, send_to_non_existing_process, Msg, To}`{:
  #port_trace_messages_send_to_non_existing_process }** - When `Port`
  sends message `Msg` to the non-existing process `To`.

- **`{trace, Port, 'receive', Msg}`{: #port_trace_messages_receive }** -
  When `Port` receives message `Msg`. If `Msg` is set to time-out, a receive
  statement can have timed out, or the process received a message with the
  payload `timeout`.

- **`{trace, Port, register, RegName}`{: #port_trace_messages_register
  }** - When `Port` gets the name `RegName` registered.

- **`{trace, Port, unregister, RegName}`{: #port_trace_messages_unregister
  }** - When `Port` gets the name `RegName` unregistered. This is done
  automatically when a registered process or port exits.

- **`{trace, Port, getting_linked, Pid2}`{:
  #port_trace_messages_getting_linked }** - When `Port` gets linked to a
  process `Pid2`.

- **`{trace, Port, getting_unlinked, Pid2}`{:
  #port_trace_messages_getting_unlinked }** - When `Port` gets unlinked
  from a process `Pid2`.

- **`{trace, Port, open, Pid, Driver}`{: #port_trace_messages_open }** - When
  `Pid` opens a new port `Port` with the running `Driver`.

  `Driver` is the name of the driver as an atom.

- **`{trace, Port, closed, Reason}`{: #port_trace_messages_closed }** - When
  `Port` closes with `Reason`.

- **`{trace, Port, in, Command | 0}`{: #port_trace_messages_in_port }** -
  When `Port` is scheduled to run. `Command` is the first thing the port will
  execute, it can however run several commands before being scheduled out. On
  some rare occasions, the current function cannot be determined, then the last
  element is `0`.

  The possible commands are `call`, `close`, `command`, `connect`, `control`,
  `flush`, `info`, `link`, `open`, and `unlink`.

- **`{trace, Port, out, Command | 0}`{: #port_trace_messages_out_port }** -
  When `Port` is scheduled out. The last command run was `Command`. On some rare
  occasions, the current function cannot be determined, then the last element is
  `0`. `Command` can contain the same commands as `in`

If the tracing process/port dies or the tracer module returns `remove`, the
flags are silently removed.

Returns a number indicating the number of ports that matched `Ports`.
If `Ports` is a port identifier, the return value is `1`. If
`Ports` is `all` or `existing`, the return value is the number of
existing ports. If `Ports` is `new`, the return value is `0`.

Failure: `badarg` if the specified arguments are not supported. For example,
`cpu_timestamp` is not supported on all platforms.
""".
-doc #{ since => <<"OTP 27.0">> }.
-spec port(Session, Ports, How, FlagList) -> integer() when
      Session :: session(),
      Ports :: port() | all | existing | new,
      How :: boolean(),
      FlagList :: [trace_flag()].
port(Session, Ports, How, FlagList) ->
    ensure_tracer_module_loaded(tracer, FlagList),
    try
        BifPorts = case Ports of
                       _ when is_port(Ports) -> Ports;
                       all -> ports;
                       existing -> existing_ports;
                       new -> new_ports;
                       ports -> Ports;
                       existing_ports -> Ports;
                       new_ports -> Ports
                   end,
        erts_internal:trace(Session, BifPorts, How, FlagList)
    catch error:R:Stk ->
            error_with_inherited_info(R, [Session, Ports, How, FlagList], Stk)
    end.



%% function/4
-doc """
Enable or disable _call tracing_ for one or more functions.

Must be combined with `process/4` to set the `call` trace flag for one or more
processes.

Conceptually, call tracing works as follows. In each trace session, a
set of processes and a set of functions haven been marked for
tracing. If a traced process calls a traced function, the trace action
is taken. Otherwise, nothing happens.

To add or remove one or more processes to the set of traced processes, use
`process/4`.

Use this function to add or remove functions to the set of traced functions
in a trace session.

Argument `Session` is the trace session to operate on as returned by
`session_create/3`.

Argument **`MFA`** is to be a tuple, such as `{Module, Function, Arity}`, or the
atom `on_load` (described below). The `MFA` tuple specifies the module,
function, and arity for the functions to be traced. The atom `'_'` can be used
as a wildcard in any of the following ways:

- **`{Module,Function,'_'}`** - All functions of any arity named `Function` in
  module `Module`.

- **`{Module,'_','_'}`** - All functions in module `Module`.

- **`{'_','_','_'}`** - All functions in all loaded modules.

Other combinations, such as `{Module,'_',Arity}`, are not allowed.

If argument `MFA` is the atom `on_load`, the match specification and flag list
are used on all functions in all modules that are newly loaded.

Argument **`MatchSpec`** can take the following forms:

- **`true`** - Enable tracing for the matching functions. Any match
  specification is removed.

- **`false`** - Disable tracing for the matching functions. Any match
  specification is removed.

- **`MatchExpression`** - A match specification. An empty list is equivalent to
  `true`. For a description of match specifications, see section
  [Match Specifications in Erlang](`e:erts:match_spec.md`) in the User's Guide
  for the ERTS application.

- **`restart`** - For the `FlagList` options `call_count`, `call_time` and
  `call_memory`: restarts the existing counters. The behavior is undefined for
  other `FlagList` options.

- **`pause`** - For the `FlagList` options `call_count`, `call_time` and
  `call_memory`: pauses the existing counters. The behavior is undefined for
  other `FlagList` options.

Argument **`FlagList`** is a list of options. The following are the valid options:

- **`global`** - Turn on or off call tracing for global function calls (that
  is, calls specifying the module explicitly). Only exported functions match and
  only global calls generate trace messages. **This is the default if `FlagList`
  is empty**.

- **`local`** - Turn on or off call tracing for all types of function calls.
  Trace messages are sent whenever any of the specified functions are called,
  regardless of how they are called. If flag `return_to` is set for the process,
  a `return_to` message is also sent when this function returns to its caller.

- **`meta`** - Turn on or off meta-tracing for all types of function
  calls. Trace messages are sent to the tracer whenever any of the specified
  functions are called.

  Meta-tracing traces all processes and does not care about the process trace
  flags set by `process/4`, the trace flags are instead fixed to
  `[call, timestamp]`.

  The match specification function `{return_trace}` works with meta-trace.

- **`call_count`**{: #call_count } - Start (`MatchSpec == true`) or stop
  (`MatchSpec == false`) call count tracing for all types of function calls. For
  every function, a counter is incremented when the function is called, in any
  process. No process trace flags need to be activated.

  If call count tracing is started while already running, the count is restarted
  from zero. To pause running counters, use `MatchSpec == pause`. Paused and
  running counters can be restarted from zero with `MatchSpec == restart`.

  To read the counter value for a function, call
  [`trace:info(_, MFA, call_count)`](`info/3`).

- **`call_time`**{: #call_time } - Start (`MatchSpec` is `true`) or stops (`MatchSpec` is `false`)
  call time tracing for all types of function calls. For every function, a
  counter is incremented when the function is called and the time spent in the
  function is measured and accumulated in another counter. The counters are
  stored for each call traced process.

  If call time tracing is started while already running, the count and time
  restart from zero. To pause running counters, use `MatchSpec == pause`. Paused
  and running counters can be restarted from zero with `MatchSpec == restart`.

  To read the counter values, use `info/3`.

- **`call_memory`**{: #call_memory } - Start (`MatchSpec == true`) or stop
  (`MatchSpec == false`) call memory tracing for all types of function calls.

  If call memory tracing is started while already running, counters and
  allocations restart from zero. To pause running counters, use
  `MatchSpec == pause`. Paused and running counters can be restarted from zero
  with `MatchSpec == restart`.

  To read the counter value, use `info/3`.

Option `global` cannot be combined with any of the other options, which all
perform some kind of local tracing. If global tracing is specified for
a set of functions, then `local`, `meta`, `call_count`, `call_time`,
and `call_memory` tracing for the matching set of functions are
disabled, and vice versa.

When disabling trace, the option must match the type of trace set on the
function. That is, local tracing must be disabled with option `local` and global
tracing with option `global` (or no option), and so on.

Part of a match specification cannot be changed directly. If a function has
a match specification, it can be replaced with a new one. Function `info/3` can
be used to retrieve the existing match specification.

Returns the number of functions matching argument `MFA`. Zero is returned if
none matched or if `on_load` was specified.

Fails by raising an error exception with an error reason of:

- **`badarg`** - If an argument is invalid.

- **`system_limit`** - If a match specification passed as argument has excessive
  nesting which causes scheduler stack exhaustion for the scheduler that the
  calling process is executing on.
  [Scheduler stack size](`e:erts:erl_cmd.md#sched_thread_stack_size`) can be configured
  when starting the runtime system.
""".
-doc #{ since => <<"OTP 27.0">> }.
-spec function(Session, MFA, MatchSpec, FlagList) -> non_neg_integer() when
      Session :: session(),
      MFA :: trace_pattern_mfa() | on_load,
      MatchSpec :: trace_match_spec()
                 | boolean()
                 | restart
                 | pause,
      FlagList :: [ trace_pattern_flag() ].
function(Session, MFA, MatchSpec, FlagList) ->
    ensure_tracer_module_loaded(meta, FlagList),
    try erts_internal:trace_pattern(Session, MFA, MatchSpec, FlagList) of
        Res -> Res
    catch error:R:Stk ->
            error_with_inherited_info(R, [Session, MFA, MatchSpec, FlagList], Stk)
    end.


%% send/3
-doc """
Set trace pattern for _message sending_.

Must be combined with `process/4` or `port/4` to set the `send` trace flag for
one or more processes or ports.

Argument `Session` is the trace session to operate on as returned by
`session_create/3`.

The default value for the `send` trace pattern in each session is
`true`. That is, all messages sent from processes having `send` trace
enabled will be traced. Use this function to limit traced `send`
events based on the message content, the sender, and/or the receiver.

Argument `MatchSpec` can take the following forms:

- **`MatchExpression`** - A match specification. The matching is done on
  the list `[Receiver, Msg]`. `Receiver` is the process or port identity of the
  receiver and `Msg` is the message term. The pid of the sending process can be
  accessed with the guard function `self/0`. An empty list is the same as
  `true`. For more information, see section
  [Match Specifications in Erlang](`e:erts:match_spec.md`) in the User's Guide
  for the ERTS application.

- **`true`** - Enable tracing for all sent messages (from `send` traced
  processes). Any match specification is removed.

- **`false`** - Disable tracing for all sent messages. Any match specification
  is removed.

Argument `FlagList` must be `[]`.

The return value is always `1`.

*Examples:*

Only trace messages to a specific process `Pid`:

```erlang
> trace:send(Session, [{[Pid, '_'],[],[]}], []).
1
```

Only trace messages matching `{reply, _}`:

```erlang
> trace:send(Session, [{['_', {reply,'_'}],[],[]}], []).
1
```

Only trace messages sent to the sender itself:

```erlang
> trace:send(Session, [{['$1', '_'],[{'=:=','$1',{self}}],[]}], []).
1
```

Only trace messages sent to other nodes:

```erlang
> trace:send(Session, [{['$1', '_'],[{'=/=',{node,'$1'},{node}}],[]}], []).
1
```

> #### Note {: .info }
>
> A match specification for `send` trace can use all guard and body functions
> except `caller`.

Fails by raising an error exception with an error reason of:

- **`badarg`** - If an argument is invalid.

- **`system_limit`** - If a match specification passed as argument has excessive
  nesting which causes scheduler stack exhaustion for the scheduler that the
  calling process is executing on.
  [Scheduler stack size](`e:erts:erl_cmd.md#sched_thread_stack_size`) can be configured
  when starting the runtime system.
""".
-doc #{ since => <<"OTP 27.0">> }.
-spec send(Session, MatchSpec, FlagList) -> non_neg_integer() when
      Session :: session(),
      MatchSpec :: trace_match_spec()
                 | boolean(),
      FlagList :: [].
send(Session, MatchSpec, FlagList) ->
    ensure_tracer_module_loaded(meta, FlagList),
    try erts_internal:trace_pattern(Session, send, MatchSpec, FlagList) of
        Res -> Res
    catch error:R:Stk ->
            error_with_inherited_info(R, [Session, MatchSpec, FlagList], Stk)
    end.

%% recv/3
-doc """
Set trace pattern for _message receiving_.

Must be combined with `process/4` or `port/4` to set the `'receive'` trace flag
for one or more processes or ports.

Argument `Session` is the trace session to operate on as returned by
`session_create/3`.

The default value for the `receive` trace pattern in each session is
`true`. That is, all messages received by processes having `'receive'`
trace enabled will be traced. Use this function to limit traced
`'receive'` events based on the message content, the sender, and/or the
receiver.

Argument `MatchSpec` can take the following forms:

- **`MatchExpression`** - A match specification. The matching is done on
  the list `[Node, Sender, Msg]`. `Node` is the node name of the sender.
  `Sender` is the process or port identity of the sender, or the atom
  `undefined` if the sender is not known (which can be the case for remote
  senders). `Msg` is the message term. The pid of the receiving process can be
  accessed with the guard function `self/0`. An empty list is the same as
  `true`. For more information, see section
  [Match Specifications in Erlang](`e:erts:match_spec.md`) in the User's Guide
  for the ERTS application.

- **`true`** - Enable tracing for all received messages (to `'receive'` traced
  processes). Any match specification is removed. _This is the default_.

- **`false`** - Disable tracing for all received messages. Any match
  specification is removed.

Argument `FlagList` must be `[]` for receive tracing.

The return value is always `1`.

*Examples:*

Only trace messages from a specific process `Pid`:

```erlang
> trace:recv(Session, [{['_',Pid, '_'],[],[]}], []).
1
```

Only trace messages matching `{reply, _}`:

```erlang
> trace:recv(Session, [{['_','_', {reply,'_'}],[],[]}], []).
1
```

Only trace messages from other nodes:

```erlang
> trace:recv(Session, [{['$1', '_', '_'],[{'=/=','$1',{node}}],[]}], []).
1
```

> #### Note {: .info }
>
> A match specification for `'receive'` trace can use all guard and body
> functions except `caller`, `is_seq_trace`, `get_seq_token`, `set_seq_token`,
> `enable_trace`, `disable_trace`, `trace`, `silent`, and `process_dump`.

Fails by raising an error exception with an error reason of:

- **`badarg`** - If an argument is invalid.

- **`system_limit`** - If a match specification passed as argument has excessive
  nesting which causes scheduler stack exhaustion for the scheduler that the
  calling process is executing on.
  [Scheduler stack size](`e:erts:erl_cmd.md#sched_thread_stack_size`) can be configured
  when starting the runtime system.
""".
-doc #{ since => <<"OTP 27.0">> }.
-spec recv(Session, MatchSpec, FlagList) -> non_neg_integer() when
      Session :: session(),
      MatchSpec :: trace_match_spec()
                 | boolean(),
      FlagList :: [].
recv(Session, MatchSpec, FlagList) ->
    ensure_tracer_module_loaded(meta, FlagList),
    try erts_internal:trace_pattern(Session, 'receive', MatchSpec, FlagList) of
        Res -> Res
    catch error:R:Stk ->
            error_with_inherited_info(R, [Session, MatchSpec, FlagList], Stk)
    end.


%% info/3
-doc """
Return trace information about a port, process, function, or event.

Argument `Session` is the trace session to inspect as returned by
`session_create/3` or `session_info/1`.

**To get information about a port or process**, `PidPortFuncEvent` is to be a
process identifier (pid), port identifier, or one of the atoms `new`,
`new_processes`, or `new_ports`. The atom `new` or `new_processes` means that
the default trace state for processes to be created is returned. The atom
`new_ports` means that the default trace state for ports to be created is
returned.

Valid `Item` values for ports and processes:

- **`flags`** - Returns a list of atoms indicating what kind of traces is
  enabled for the process. The list is empty if no traces are enabled, and one
  or more of [`trace_info_flag()`](`t:trace_info_flag/0`) if traces are enabled.
  The order is arbitrary.

- **`tracer`** - Returns the identifier for process, port, or a tuple containing
  the tracer module and tracer state tracing this process. If this process is
  not traced, the return value is `[]`.

**To get information about a function**, `PidPortFuncEvent` is to be the
three-element tuple `{Module, Function, Arity}` or the atom `on_load`. No
wildcards are allowed. Returns `undefined` if the function does not exist or
`false` if the function is not traced. If `PidPortFuncEvent` is `on_load`, the
information returned refers to the default value for code that will be loaded.

Valid `Item` values for functions:

- **`traced`** - Returns `global` if this function is traced on global function
  calls, `local` if this function is traced on local function calls (that is,
  local and global function calls), and `false` if local or global function
  calls are not traced.

- **`match_spec`** - Returns the match specification for this function, if it
  has one. If the function is locally or globally traced but has no match
  specification defined, the returned value is `[]`.

- **`meta`** - Returns the meta-trace tracer process, port, or trace module for
  this function, if it has one. If the function is not meta-traced, the returned
  value is `false`. If the function is meta-traced but has once detected that
  the tracer process is invalid, the returned value is `[]`.

- **`meta_match_spec`** - Returns the meta-trace match specification for this
  function, if it has one. If the function is meta-traced but has no match
  specification defined, the returned value is `[]`.

- **`call_count`** - Returns the call count value for this function or `true`
  for the pseudo function `on_load` if call count tracing is active. Otherwise
  `false` is returned.

  See also `function/4`.

- **`call_time`** - Returns the call time values for this function or `true` for
  the pseudo function `on_load` if call time tracing is active. Otherwise
  `false` is returned. The call time values returned, `[{Pid, Count, S, Us}]`,
  is a list of each process that executed the function and its specific
  counters. `Count` is the call count. `S` and `Us` are the accumulated call
  time expressed in seconds and microseconds.

  See also `function/4`.

- **`call_memory`** - Returns the accumulated number of words allocated by this
  function. Accumulation stops at the next memory traced function: if there are
  `outer`, `middle` and `inner` functions each allocating 3 words, but only
  `outer` is traced, it will report 9 allocated words. If `outer` and `inner`
  are traced, 6 words are reported for `outer` and 3 for `inner`. When function
  is not traced, `false` is returned. Returned tuple is `[{Pid, Count, Words}]`,
  for each process that executed the function.

  See also `function/4`.

- **`all`** - Returns a list containing the `{Item, Value}` tuples for all other
  items, or returns `false` if no tracing is active for this function.

**To get information about an event**, `PidPortFuncEvent` is to be one of the
atoms `send` or `'receive'`.

One valid `Item` for events exists:

- **`match_spec`** - Returns the match specification for this event, if it has
  one, or `true` if no match specification has been set.

The return value is `{Item, Value}`, where `Value` is the requested information
as described earlier. If a pid for a dead process was specified, or the name of
a non-existing function, `Value` is `undefined`.
""".
-doc #{ since => <<"OTP 27.0">> }.
-spec info(Session, PidPortFuncEvent, Item) -> Res when
      Session :: session(),
      PidPortFuncEvent :: pid() | port() | new | new_processes | new_ports
                     | MFA | on_load | send | 'receive',
      MFA :: {module(), atom(), arity()},
      Item :: flags | tracer | traced | match_spec
            | meta | meta_match_spec | call_count | call_time | call_memory
            | all,
      Res :: trace_info_return().
info(Session, PidPortFuncEvent, Item) ->
    try erts_internal:trace_info(Session, PidPortFuncEvent, Item) of
        Ret -> Ret
    catch error:R:Stk ->
            error_with_inherited_info(R, [Session, PidPortFuncEvent, Item], Stk)
    end.

-doc """
Equivalent to [`erlang:trace_delivered(Tracee)`](`erlang:trace_delivered/1`)
except that it is run within the given `t:session/0`.
""".
-spec delivered(Session :: session(), Tracee :: pid() | all) -> reference().
delivered(_Session , Tracee) ->
    erlang:trace_delivered(Tracee).


%% session_create/3
-doc """
Create a new trace session.

Argument `Name` is an atom name for the session. It will be returned when
inspecting with `session_info/1`.

Argument `Tracer` specifies the consumer of all trace events for the session. It
can be an identifier of a local process or port to receive all trace
messages.

`Tracer` can also be a tuple `{TracerModule, TracerState}` for a tracer module
to be called instead of sending a trace message. The tracer module can then
ignore or change the trace message. For more details on how to write a tracer
module, see module `m:erl_tracer`.

Argument `Opts` must be `[]`.

Returns an opaque handle to the trace session. The handle will keep the session
alive. If the handle is dropped and garbage collected, the session will be
destroyed and cleaned up as if `session_destroy/1` was called.
""".
-doc #{ since => <<"OTP 27.0">> }.
-spec session_create(Name, Tracer, Opts) -> session() when
      Name :: atom(),
      Tracer :: pid() | port() | {module(), term()},
      Opts :: [].
session_create(Name, Tracer, Opts) ->
    try erts_internal:trace_session_create(Name, Tracer, Opts) of
        Ref -> Ref
    catch error:R:Stk ->
            error_with_inherited_info(R, [Name, Tracer, Opts], Stk)
    end.

%% session_destroy/1
-doc """
Destroy a trace session and cleanup all its settings on processes, ports, and
functions.

The only things not cleaned up are trace messages that have already been sent.

Returns `true` if the session was active. Returns `false` if the session had
already been destroyed by either an earler call to this function or the garbage
collector.
""".
-doc #{ since => <<"OTP 27.0">> }.
-spec session_destroy(Session) -> true | false  when
      Session :: session().
session_destroy(Session) ->
    try erts_internal:trace_session_destroy(Session) of
        Res -> Res
    catch error:R:Stk ->
            error_with_inherited_info(R, [Session], Stk)
    end.



%% session_info/1
-doc """
Return which trace sessions that affect a port, process, function, or event.

Argument `all` returns all active trace sessions that exists on the node.

Returns a list of [weak session handles](`t:session_weak_ref/0`) or `undefined` if the
process/port/function does not exists.
""".
-doc #{ since => <<"OTP 27.0">> }.
-spec session_info(PidPortFuncEvent) -> Res when
      PidPortFuncEvent :: all | pid() | port() | new | new_processes | new_ports
                     | MFA | on_load | send | 'receive',
      MFA :: {module(), atom(), arity()},
      Res :: undefined | [session_weak_ref()].
session_info(all) ->
    {session, List} = erts_internal:trace_info(any, any, session),
    List;
session_info(PidPortFuncEvent) ->
    try erts_internal:trace_info(any, PidPortFuncEvent, session) of
        {session, List} -> List;
        undefined -> undefined
    catch error:R:Stk ->
            error_with_inherited_info(R, [PidPortFuncEvent], Stk)
    end.

%% Make sure that we have loaded the tracer module.
%% Copy-paste from erlang.erl
ensure_tracer_module_loaded(Flag, [{Flag, Module, State}|T]) when erlang:is_atom(Module) ->
    case erlang:module_loaded(Module) of
        false ->
            Module:enabled(trace_status, erlang:self(), State);
        true ->
            ensure_tracer_module_loaded(Flag, T)
    end;
ensure_tracer_module_loaded(Flag, [_ | T]) ->
    ensure_tracer_module_loaded(Flag, T);
ensure_tracer_module_loaded(_, _) ->
    ok.

error_with_inherited_info(Reason0, Args, [{_,_,_,ExtraInfo}|_]) ->
    Reason1 = case Reason0 of
                  {case_clause, _} -> badarg;
                  _ -> Reason0
              end,
    %% We KNOW that lists:keyfind/3 is a BIF and is therefore safe to call.
    Map = case lists:keyfind(error_info, 1, ExtraInfo) of
               {error_info,M} -> M;
               false -> #{}
           end,
    erlang:error(Reason1, Args, [{error_info, Map#{module => erl_kernel_errors}}]).
