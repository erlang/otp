<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2024. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# sys and proc_lib

[](){: #sys-and-proc_lib }

The `m:sys` module has functions for simple debugging of processes implemented
using behaviours. It also has functions that, together with functions in the
`proc_lib` module, can be used to implement a _special process_ that complies to
the OTP design principles without using a standard behaviour. These functions
can also be used to implement user-defined (non-standard) behaviours.

Both `m:sys` and `m:proc_lib` belong to the STDLIB application.

## Simple Debugging

The `m:sys` module has functions for simple debugging of processes implemented
using behaviours. The `code_lock` example from
[gen_statem Behaviour](statem.md#example) is used to illustrate this:

```erlang
Erlang/OTP 27 [erts-15.0] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit]

Eshell V15.0 (press Ctrl+G to abort, type help(). for help)
1> code_lock:start_link([1,2,3,4]).
Lock
{ok,<0.90.0>}
2> sys:statistics(code_lock, true).
ok
3> sys:trace(code_lock, true).
ok
4> code_lock:button(1).
*DBG* code_lock receive cast {button,1} in state locked
ok
*DBG* code_lock consume cast {button,1} in state locked
5> code_lock:button(2).
*DBG* code_lock receive cast {button,2} in state locked
ok
*DBG* code_lock consume cast {button,2} in state locked
6> code_lock:button(3).
*DBG* code_lock receive cast {button,3} in state locked
ok
*DBG* code_lock consume cast {button,3} in state locked
7> code_lock:button(4).
*DBG* code_lock receive cast {button,4} in state locked
ok
Unlock
*DBG* code_lock consume cast {button,4} in state locked => open
*DBG* code_lock start_timer {state_timeout,10000,lock,[]} in state open
*DBG* code_lock receive state_timeout lock in state open
Lock
*DBG* code_lock consume state_timeout lock in state open => locked
8> sys:statistics(code_lock, get).
{ok,[{start_time,{{2024,5,3},{8,11,1}}},
     {current_time,{{2024,5,3},{8,11,48}}},
     {reductions,4098},
     {messages_in,5},
     {messages_out,0}]}
9> sys:statistics(code_lock, false).
ok
10> sys:trace(code_lock, false).
ok
11> sys:get_status(code_lock).
{status,<0.90.0>,
        {module,gen_statem},
        [[{'$initial_call',{code_lock,init,1}},
          {'$ancestors',[<0.88.0>,<0.87.0>,<0.70.0>,<0.65.0>,<0.69.0>,
                         <0.64.0>,kernel_sup,<0.47.0>]}],
         running,<0.88.0>,[],
         [{header,"Status for state machine code_lock"},
          {data,[{"Status",running},
                 {"Parent",<0.88.0>},
                 {"Modules",[code_lock]},
                 {"Time-outs",{0,[]}},
                 {"Logged Events",[]},
                 {"Postponed",[]}]},
          {data,[{"State",
                  {locked,#{code => [1,2,3,4],
                            length => 4,buttons => []}}}]}]]}
```

## Special Processes

This section describes how to write a process that complies to the OTP design
principles, without using a standard behaviour. Such a process is to:

- Be started in a way that makes the process fit into a supervision tree
- Support the `sys` [debug facilities](spec_proc.md#debug)
- Take care of [system messages](spec_proc.md#msg).

System messages are messages with a special meaning, used in the supervision
tree. Typical system messages are requests for trace output, and requests to
suspend or resume process execution (used during release handling). Processes
implemented using standard behaviours automatically understand these messages.

### Example

Here follows the simple server from
[Overview](design_principles.md#ch1),
implemented using `sys` and `proc_lib` to fit into a supervision tree:

```erlang
-module(ch4).
-export([start_link/0]).
-export([alloc/0, free/1]).
-export([init/1]).
-export([system_continue/3, system_terminate/4,
         write_debug/3,
         system_get_state/1, system_replace_state/2]).

start_link() ->
    proc_lib:start_link(ch4, init, [self()]).

alloc() ->
    ch4 ! {self(), alloc},
    receive
        {ch4, Res} ->
            Res
    end.

free(Ch) ->
    ch4 ! {free, Ch},
    ok.

init(Parent) ->
    register(ch4, self()),
    Chs = channels(),
    Deb = sys:debug_options([]),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(Chs, Parent, Deb).

loop(Chs, Parent, Deb) ->
    receive
        {From, alloc} ->
            Deb2 = sys:handle_debug(Deb, fun ch4:write_debug/3,
                                    ch4, {in, alloc, From}),
            {Ch, Chs2} = alloc(Chs),
            From ! {ch4, Ch},
            Deb3 = sys:handle_debug(Deb2, fun ch4:write_debug/3,
                                    ch4, {out, {ch4, Ch}, From}),
            loop(Chs2, Parent, Deb3);
        {free, Ch} ->
            Deb2 = sys:handle_debug(Deb, fun ch4:write_debug/3,
                                    ch4, {in, {free, Ch}}),
            Chs2 = free(Ch, Chs),
            loop(Chs2, Parent, Deb2);

        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent,
                                  ch4, Deb, Chs)
    end.

system_continue(Parent, Deb, Chs) ->
    loop(Chs, Parent, Deb).

system_terminate(Reason, _Parent, _Deb, _Chs) ->
    exit(Reason).

system_get_state(Chs) ->
    {ok, Chs}.

system_replace_state(StateFun, Chs) ->
    NChs = StateFun(Chs),
    {ok, NChs, NChs}.

write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p event = ~p~n", [Name, Event]).
```

As it is not relevant to the example, the channel handling functions have been
omitted. To compile this example, the
[implementation of channel handling](design_principles.md#channels-implementation)
needs to be added to the module.

{: #ex }

Here is an example showing how the debugging functions in the `sys`
module can be used for `ch4`:

```erlang
% erl
Erlang/OTP 27 [erts-15.0] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit]

Eshell V15.0 (press Ctrl+G to abort, type help(). for help)
1> ch4:start_link().
{ok,<0.90.0>}
2> sys:statistics(ch4, true).
ok
3> sys:trace(ch4, true).
ok
4> ch4:alloc().
ch4 event = {in,alloc,<0.88.0>}
ch4 event = {out,{ch4,1},<0.88.0>}
1
5> ch4:free(ch1).
ch4 event = {in,{free,ch1}}
ok
6> sys:statistics(ch4, get).
{ok,[{start_time,{{2024,5,3},{8,26,13}}},
     {current_time,{{2024,5,3},{8,26,49}}},
     {reductions,202},
     {messages_in,2},
     {messages_out,1}]}
7> sys:statistics(ch4, false).
ok
8> sys:trace(ch4, false).
ok
9> sys:get_status(ch4).
{status,<0.90.0>,
        {module,ch4},
        [[{'$initial_call',{ch4,init,1}},
          {'$ancestors',[<0.88.0>,<0.87.0>,<0.70.0>,<0.65.0>,<0.69.0>,
                         <0.64.0>,kernel_sup,<0.47.0>]}],
         running,<0.88.0>,[],
         {[1],[2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19|...]}]}
```

### Starting the Process

A function in the `m:proc_lib` module is to be used to start the process. Several
functions are available, for example,
[`proc_lib:spawn_link/3,4`](`proc_lib:spawn_link/4`)
for asynchronous start and
[`proc_lib:start_link/3,4,5`](`proc_lib:start_link/5`) for synchronous start.

Information necessary for a process within a supervision tree, such as
details on ancestors and the initial call, is stored when a process
is started through one of these functions.

If the process terminates with a reason other than `normal` or `shutdown`, a
crash report is generated. For more information about the crash report, see
[Logging](`e:kernel:logger_chapter.md`) in Kernel User's Guide.

In the example, synchronous start is used. The process starts by calling
`ch4:start_link()`:

```erlang
start_link() ->
    proc_lib:start_link(ch4, init, [self()]).
```

`ch4:start_link/0` calls `proc_lib:start_link/3`, which takes a module
name, a function name, and an argument list as arguments. It then
spawns a new process and establishes a link. The new process starts
by executing the given function, here `ch4:init(Pid)`, where `Pid` is
the pid of the parent process (obtained by the call to
[`self()`](`erlang:self/0`) in the call to `proc_lib:start_link/3`).

All initialization, including name registration, is done in `init/1`. The new
process has to acknowledge that it has been started to the parent:

```erlang
init(Parent) ->
    ...
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(...).
```

`proc_lib:start_link/3` is synchronous and does not return until
[`proc_lib:init_ack/1,2`](`proc_lib:init_ack/2`) or
[`proc_lib:init_fail/2,3`](`proc_lib:init_fail/3`) has been called,
or the process has exited.

[](){: #debug }

### Debugging

To support the debug facilities in `sys`, a _debug structure_ is needed. The
`Deb` term is initialized using `sys:debug_options/1`:

```erlang
init(Parent) ->
    ...
    Deb = sys:debug_options([]),
    ...
    loop(Chs, Parent, Deb).
```

`sys:debug_options/1` takes a list of options. Given an empty list as in this
example means that debugging is initially disabled. For information about the
possible options, see `m:sys` in STDLIB.

For each _system event_ to be logged or traced, the following function
is to be called:

```erlang
sys:handle_debug(Deb, Func, Info, Event) => Deb1
```

The arguments have the follow meaning:

- `Deb` is the debug structure as returned from `sys:debug_options/1`.
- `Func` is a fun specifying a (user-defined) function used to format trace
  output. For each system event, the format function is called as
  `Func(Dev, Event, Info)`, where:
  - `Dev` is the I/O device to which the output is to be printed. See `m:io`
    in STDLIB.
  - `Event` and `Info` are passed as-is from the call to `sys:handle_debug/4`.
- `Info` is used to pass more information to `Func`. It can be any term, and it
  is passed as-is.
- `Event` is the system event. It is up to the user to define what a system
  event is and how it is to be represented. Typically, at least incoming and
  outgoing messages are considered system events and represented by the tuples
  `{in,Msg[,From]}` and `{out,Msg,To[,State]}`, respectively.

`sys:handle_debug/4` returns an updated debug structure `Deb1`.

In the example, `sys:handle_debug/4` is called for each incoming and
outgoing message. The format function `Func` is the function
`ch4:write_debug/3`, which prints the message using `io:format/3`.

```erlang
loop(Chs, Parent, Deb) ->
    receive
        {From, alloc} ->
            Deb2 = sys:handle_debug(Deb, fun ch4:write_debug/3,
                                    ch4, {in, alloc, From}),
            {Ch, Chs2} = alloc(Chs),
            From ! {ch4, Ch},
            Deb3 = sys:handle_debug(Deb2, fun ch4:write_debug/3,
                                    ch4, {out, {ch4, Ch}, From}),
            loop(Chs2, Parent, Deb3);
        {free, Ch} ->
            Deb2 = sys:handle_debug(Deb, fun ch4:write_debug/3,
                                    ch4, {in, {free, Ch}}),
            Chs2 = free(Ch, Chs),
            loop(Chs2, Parent, Deb2);
        ...
    end.

write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p event = ~p~n", [Name, Event]).
```

[](){: #msg }

### Handling System Messages

_System messages_ are received as:

```text
{system, From, Request}
```

The content and meaning of these messages are not to be interpreted by the
process. Instead the following function is to be called:

```erlang
sys:handle_system_msg(Request, From, Parent, Module, Deb, State)
```

The arguments have the following meaning:

- `Request` and `From` from the received system message are to be
  passed as-is to the call to `sys:handle_system_msg/6`.
- `Parent` is the pid of the parent process.
- `Module` is the name of the module implementing the speciall process.
- `Deb` is the debug structure.
- `State` is a term describing the internal state and is passed on to
  `Module:system_continue/3`, `Module:system_terminate/4`/
  `Module:system_get_state/1`, and `Module:system_replace_state/2`.

`sys:handle_system_msg/6` does not return. It handles the system
message and *eventually* calls either of the following functions:

* `Module:system_continue(Parent, Deb, State)` - if process execution is to
  continue.

* `Module:system_terminate(Reason, Parent, Deb, State)` - if the
  process is to terminate.

While handling the system message, `sys:handle_system_msg/6` can call
one of the following functions:

* `Module:system_get_state(State)` - if the process is to return its state.

* `Module:system_replace_state(StateFun, State)` - if the process is
  to replace its state using the fun `StateFun` fun. See `sys:replace_state/3`
  for more information.

* `system_code_change(Misc, Module, OldVsn, Extra)` - if the process is to
  perform a code change.

A process in a supervision tree is expected to terminate with the same reason as
its parent.

In the example, system messages are handed by the following code:

```erlang
loop(Chs, Parent, Deb) ->
    receive
        ...

        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent,
                                  ch4, Deb, Chs)
    end.

system_continue(Parent, Deb, Chs) ->
    loop(Chs, Parent, Deb).

system_terminate(Reason, Parent, Deb, Chs) ->
    exit(Reason).

system_get_state(Chs) ->
    {ok, Chs, Chs}.

system_replace_state(StateFun, Chs) ->
    NChs = StateFun(Chs),
    {ok, NChs, NChs}.
```

If a special process is configured to trap exits, it must take notice
of 'EXIT' messages from its parent process and terminate using the
same exit reason once the parent process has terminated.

Here is an example:

```erlang
init(Parent) ->
    ...,
    process_flag(trap_exit, true),
    ...,
    loop(Parent).

loop(Parent) ->
    receive
        ...
        {'EXIT', Parent, Reason} ->
            %% Clean up here, if needed.
            exit(Reason);
        ...
    end.
```

## User-Defined Behaviours

[](){: #behaviours } To implement a user-defined behaviour, write code similar
to code for a special process, but call functions in a callback module for
handling specific tasks.

If the compiler is to warn for missing callback functions, as it does for the
OTP behaviours, add `-callback` attributes in the behaviour module to describe
the expected callbacks:

```text
-callback Name1(Arg1_1, Arg1_2, ..., Arg1_N1) -> Res1.
-callback Name2(Arg2_1, Arg2_2, ..., Arg2_N2) -> Res2.
...
-callback NameM(ArgM_1, ArgM_2, ..., ArgM_NM) -> ResM.
```

`NameX` are the names of the expected callbacks. `ArgX_Y` and `ResX` are types
as they are described in
[Types and Function Specifications](`e:system:typespec.md`). The whole syntax of
the `-spec` attribute is supported by the `-callback` attribute.

Callback functions that are optional for the user of the behaviour to implement
are specified by use of the `-optional_callbacks` attribute:

```text
-optional_callbacks([OptName1/OptArity1, ..., OptNameK/OptArityK]).
```

where each `OptName/OptArity` specifies the name and arity of a callback
function. Note that the `-optional_callbacks` attribute is to be used together
with the `-callback` attribute; it cannot be combined with the
`behaviour_info()` function described below.

Tools that need to know about optional callback functions can call
`Behaviour:behaviour_info(optional_callbacks)` to get a list of all optional
callback functions.

> #### Note {: .info }
>
> We recommend using the `-callback` attribute rather than the
> `behaviour_info()` function. The reason is that the extra type information can
> be used by tools to produce documentation or find discrepancies.

As an alternative to the `-callback` and `-optional_callbacks` attributes you
may directly implement and export `behaviour_info()`:

```erlang
behaviour_info(callbacks) ->
    [{Name1, Arity1},...,{NameN, ArityN}].
```

where each `{Name, Arity}` specifies the name and arity of a callback function.
This function is otherwise automatically generated by the compiler using the
`-callback` attributes.

When the compiler encounters the module attribute `-behaviour(Behaviour).` in a
module `Mod`, it calls `Behaviour:behaviour_info(callbacks)` and compares the
result with the set of functions actually exported from `Mod`, and issues a
warning if any callback function is missing.

_Example:_

```erlang
%% User-defined behaviour module
-module(simple_server).
-export([start_link/2, init/3, ...]).

-callback init(State :: term()) -> 'ok'.
-callback handle_req(Req :: term(), State :: term()) -> {'ok', Reply :: term()}.
-callback terminate() -> 'ok'.
-callback format_state(State :: term()) -> term().

-optional_callbacks([format_state/1]).

%% Alternatively you may define:
%%
%% -export([behaviour_info/1]).
%% behaviour_info(callbacks) ->
%%     [{init,1},
%%      {handle_req,2},
%%      {terminate,0}].

start_link(Name, Module) ->
    proc_lib:start_link(?MODULE, init, [self(), Name, Module]).

init(Parent, Name, Module) ->
    register(Name, self()),
    ...,
    Dbg = sys:debug_options([]),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(Parent, Module, Deb, ...).

...
```

In a callback module:

```erlang
-module(db).
-behaviour(simple_server).

-export([init/1, handle_req/2, terminate/0]).

...
```

The contracts specified with `-callback` attributes in behaviour modules can be
further refined by adding `-spec` attributes in callback modules. This can be
useful as `-callback` contracts are usually generic. The same callback module
with contracts for the callbacks:

```erlang
-module(db).
-behaviour(simple_server).

-export([init/1, handle_req/2, terminate/0]).

-record(state, {field1 :: [atom()], field2 :: integer()}).

-type state()   :: #state{}.
-type request() :: {'store', term(), term()};
                   {'lookup', term()}.

...

-spec handle_req(request(), state()) -> {'ok', term()}.

...
```

Each `-spec` contract is to be a subtype of the respective `-callback` contract.
