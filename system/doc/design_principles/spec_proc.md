<!--
%CopyrightBegin%

Copyright Ericsson AB 2023. All Rights Reserved.

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

The `sys` module has functions for simple debugging of processes implemented
using behaviours. It also has functions that, together with functions in the
`proc_lib` module, can be used to implement a _special process_ that complies to
the OTP design principles without using a standard behaviour. These functions
can also be used to implement user-defined (non-standard) behaviours.

Both `sys` and `proc_lib` belong to the STDLIB application.

## Simple Debugging

The `sys` module has functions for simple debugging of processes implemented
using behaviours. The `code_lock` example from
[gen_statem Behaviour](statem.md#example) is used to illustrate this:

```erlang
Erlang/OTP 20 [DEVELOPMENT] [erts-9.0] [source-5ace45e] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V9.0  (abort with ^G)
1>  code_lock:start_link([1,2,3,4]).
Lock
{ok,<0.63.0>}
2> sys:statistics(code_lock, true).
ok
3>  sys:trace(code_lock, true).
ok
4>  code_lock:button(1).
*DBG* code_lock receive cast {button,1} in state locked
ok
*DBG* code_lock consume cast {button,1} in state locked
5>  code_lock:button(2).
*DBG* code_lock receive cast {button,2} in state locked
ok
*DBG* code_lock consume cast {button,2} in state locked
6>  code_lock:button(3).
*DBG* code_lock receive cast {button,3} in state locked
ok
*DBG* code_lock consume cast {button,3} in state locked
7>  code_lock:button(4).
*DBG* code_lock receive cast {button,4} in state locked
ok
Unlock
*DBG* code_lock consume cast {button,4} in state locked
*DBG* code_lock receive state_timeout lock in state open
Lock
*DBG* code_lock consume state_timeout lock in state open
8> sys:statistics(code_lock, get).
{ok,[{start_time,{{2017,4,21},{16,8,7}}},
     {current_time,{{2017,4,21},{16,9,42}}},
     {reductions,2973},
     {messages_in,5},
     {messages_out,0}]}
9> sys:statistics(code_lock, false).
ok
10> sys:trace(code_lock, false).
ok
11> sys:get_status(code_lock).
{status,<0.63.0>,
        {module,gen_statem},
        [[{'$initial_call',{code_lock,init,1}},
          {'$ancestors',[<0.61.0>]}],
         running,<0.61.0>,[],
         [{header,"Status for state machine code_lock"},
          {data,[{"Status",running},
                 {"Parent",<0.61.0>},
                 {"Logged Events",[]},
                 {"Postponed",[]}]},
          {data,[{"State",
                  {locked,#{code => [1,2,3,4],remaining => [1,2,3,4]}}}]}]]}
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

The simple server from [Overview](design_principles.md#ch1), implemented using
`sys` and `proc_lib` so it fits into a supervision tree:

```text
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

{: #ex }

Example on how the simple debugging functions in the `sys` module can also be
used for `ch4`:

```erlang
% erl
Erlang (BEAM) emulator version 5.2.3.6 [hipe] [threads:0]

Eshell V5.2.3.6  (abort with ^G)
1> ch4:start_link().
{ok,<0.30.0>}
2> sys:statistics(ch4, true).
ok
3> sys:trace(ch4, true).
ok
4> ch4:alloc().
ch4 event = {in,alloc,<0.25.0>}
ch4 event = {out,{ch4,ch1},<0.25.0>}
ch1
5> ch4:free(ch1).
ch4 event = {in,{free,ch1}}
ok
6> sys:statistics(ch4, get).
{ok,[{start_time,{{2003,6,13},{9,47,5}}},
     {current_time,{{2003,6,13},{9,47,56}}},
     {reductions,109},
     {messages_in,2},
     {messages_out,1}]}
7> sys:statistics(ch4, false).
ok
8> sys:trace(ch4, false).
ok
9> sys:get_status(ch4).
{status,<0.30.0>,
        {module,ch4},
        [[{'$ancestors',[<0.25.0>]},{'$initial_call',{ch4,init,[<0.25.0>]}}],
         running,<0.25.0>,[],
         [ch1,ch2,ch3]]}
```

### Starting the Process

A function in the `proc_lib` module is to be used to start the process. Several
functions are available, for example, `spawn_link/3,4` for asynchronous start
and `start_link/3,4,5` for synchronous start.

A process started using one of these functions stores information (for example,
about the ancestors and initial call) that is needed for a process in a
supervision tree.

If the process terminates with another reason than `normal` or `shutdown`, a
crash report is generated. For more information about the crash report, see the
SASL User's Guide.

In the example, synchronous start is used. The process starts by calling
`ch4:start_link()`:

```erlang
start_link() ->
    proc_lib:start_link(ch4, init, [self()]).
```

`ch4:start_link` calls the function `proc_lib:start_link`. This function takes a
module name, a function name, and an argument list as arguments, spawns, and
links to a new process. The new process starts by executing the given function,
here `ch4:init(Pid)`, where `Pid` is the pid (`self/0`) of the first process,
which is the parent process.

All initialization, including name registration, is done in `init`. The new
process must also acknowledge that it has been started to the parent:

```erlang
init(Parent) ->
    ...
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(...).
```

`proc_lib:start_link` is synchronous and does not return until
`proc_lib:init_ack` or `proc_lib:init_fail` has been called, or when the process
exits.

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

`sys:debug_options/1` takes a list of options as argument. Here the list is
empty, which means no debugging is enabled initially. For information about the
possible options, see the `m:sys` manual page in STDLIB.

Then, for each _system event_ to be logged or traced, the following function is
to be called.

```erlang
sys:handle_debug(Deb, Func, Info, Event) => Deb1
```

Here:

- `Deb` is the debug structure.
- `Func` is a fun specifying a (user-defined) function used to format trace
  output. For each system event, the format function is called as
  `Func(Dev, Event, Info)`, where:
  - `Dev` is the I/O device to which the output is to be printed. See the `m:io`
    manual page in STDLIB.
  - `Event` and `Info` are passed as is from `handle_debug`.
- `Info` is used to pass more information to `Func`. It can be any term and is
  passed as is.
- `Event` is the system event. It is up to the user to define what a system
  event is and how it is to be represented. Typically at least incoming and
  outgoing messages are considered system events and represented by the tuples
  `{in,Msg[,From]}` and `{out,Msg,To[,State]}`, respectively.

`handle_debug` returns an updated debug structure `Deb1`.

In the example, `handle_debug` is called for each incoming and outgoing message.
The format function `Func` is the function `ch4:write_debug/3`, which prints the
message using `io:format/3`.

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

The content and meaning of these messages do not need to be interpreted by the
process. Instead the following function is to be called:

```erlang
sys:handle_system_msg(Request, From, Parent, Module, Deb, State)
```

This function does not return. It handles the system message and then either
calls the following if process execution is to continue:

```erlang
Module:system_continue(Parent, Deb, State)
```

Or calls the following if the process is to terminate:

```text
Module:system_terminate(Reason, Parent, Deb, State)
```

A process in a supervision tree is expected to terminate with the same reason as
its parent.

- `Request` and `From` are to be passed as is from the system message to the
  call to `handle_system_msg`.
- `Parent` is the pid of the parent.
- `Module` is the name of the module.
- `Deb` is the debug structure.
- `State` is a term describing the internal state and is passed to
  `system_continue`/`system_terminate`/
  `system_get_state`/`system_replace_state`.

If the process is to return its state, `handle_system_msg` calls:

```text
Module:system_get_state(State)
```

If the process is to replace its state using the fun `StateFun`,
`handle_system_msg` calls:

```text
Module:system_replace_state(StateFun, State)
```

In the example:

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

If the special process is set to trap exits and if the parent process
terminates, the expected behavior is to terminate with the same reason:

```erlang
init(...) ->
    ...,
    process_flag(trap_exit, true),
    ...,
    loop(...).

loop(...) ->
    receive
        ...

        {'EXIT', Parent, Reason} ->
            ..maybe some cleaning up here..
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

Example:

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
