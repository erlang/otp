<!--
%CopyrightBegin%

SPDX-License-Identifier: Apache-2.0

Copyright Ericsson AB 2023-2025. All Rights Reserved.

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
# Tracing in Erlang with [**dbg**](`m:dbg`)

The [`dbg`](`m:dbg`) module in Erlang provides a text-based interface for tracing function calls, processes, ports, and messages. It simplifies the use of the underlying `trace:process/4`, `trace:port/4`, and `trace:function/4` BIFs (Built-In Functions). This guide will walk you through the basics of using `dbg` for your Erlang applications.

This facility is useful for both quick debugging sessions in the shell and for more structured system testing, especially where other tools might have too much performance impact.

## Quick Start

To trace a call to a function with minimal fuss, call [`dbg:c(Module,
Name, Arguments)`](`dbg:c/3`). It starts a temporary trace
receiver, enables all trace flags, and calls the designated function
from a temporary process. For example, here is how to trace a call
to `application:which_applications/0`:

```erlang
1> dbg:c(application, which_applications, []).
(<0.92.0>) <0.45.0> ! {'$gen_call',{<0.92.0>,
                                    [alias|
                                     #Ref<0.0.11779.270031856.1478295555.230456>]},
                                   which_applications} (Timestamp: {1710,
                                                                    847802,
                                                                    479222})
(<0.92.0>) out {gen,do_call,4} (Timestamp: {1710,847802,479231})
(<0.92.0>) in {gen,do_call,4} (Timestamp: {1710,847802,479271})
(<0.92.0>) << {[alias|#Ref<0.0.11779.270031856.1478295555.230456>],
               [{stdlib,"ERTS  CXC 138 10","5.2.1"},
                {kernel,"ERTS  CXC 138 10","9.2.2"}]} (Timestamp: {1710,
                                                                   847802,
                                                                   479274})
[{stdlib,"ERTS  CXC 138 10","5.2.1"},
 {kernel,"ERTS  CXC 138 10","9.2.2"}]
```

In this example, four trace events are generated:

- A send event (`!`) for the sending of a request from the current process
  to the `application_controller` process.
- A schedule-out event (`out`) when the current process schedules out while
  waiting in a `receive` for the reply to arrive.
- A schedule-in event (`in`) when the current process is scheduled in when
  reply has arrived.
- A `receive` event (`<<`) when the current process retrieves the reply from
  the `application_controller` process.

The `dbg:c/4` function has a fourth argument for specifying the trace flags,
(see [flags](#flags)).


## How-to Trace Systems

For more control, another way of tracing is to explicitly start a
_tracer_ and set the _trace flags_ of your choice on the processes you want to
trace. This is useful, when there is a complex system of processes, ports or nodes
interacting where `dbg:c/3` is to blunt.

### Starting a Tracer ([`dbg:tracer/0,2`](`dbg:tracer/2`))

First, you need to start a tracer process that will receive and display trace
messages.

```erlang
1> dbg:tracer().  % Start the default trace message receiver
{ok,<0.90.0>}     % <0.90.0> is the PID of the tracer process
```

This starts a server on the local node that will be the recipient of all trace
messages. It uses a default handler that prints formatted trace messages to the
Erlang shell.

If you need a custom tracer other than the default, you can create a tracer using
[`dbg:tracer(Type, Data)`](`dbg:tracer/2`):

  * **`Type = process`**: `Data` is `{HandlerFun, InitialState}`. `HandlerFun` is
  a `fun/2` that takes the trace message and the previous state, returning a new
  state.
  * **`Type = port`**: `Data` is a `fun/0` that returns a new trace port (e.g.,
  created by `dbg:trace_port/2`).
  * **`Type = module`**: `Data` is `{TracerModule, TracerState}` or a `fun/0`
  returning this, for use with `m:erl_tracer`.
  * **`Type = file`**: `Data` is a filename where traces will be printed.

Note that only one tracer using this method can be started at a time. You can
use trace sessions to start multiple tracers (see [trace sessions](#trace-sessions)).

### Tracing Processes and Ports ([`dbg:p/1,2`](`dbg:p/2`))

Once the tracer is started, you can tell `dbg` which processes or ports to trace
and what events to trace for them using [`dbg:p(Item, Flags)`](`dbg:p/2`).

`Item` can be:

- **`t:pid/0` or `t:port/0`** - The corresponding process or port is traced. The
  process or port can be a remote process or port (on another Erlang node). The
  node must be in the list of traced nodes (see `dbg:n/1` and `dbg:tracer/3`).

- **`all`** - All processes and ports in the system as well as all processes and
  ports created hereafter are to be traced.

- **`processes`** - All processes in the system as well as all processes created
  hereafter are to be traced.

- **`ports`** - All ports in the system as well as all ports created hereafter
  are to be traced.

- **`new`** - All processes and ports created after the call are to be
  traced.

- **`new_processes`** - All processes created after the call are to be
  traced.

- **`new_ports`** - All ports created after the call are to be traced.

- **`existing`** - All existing processes and ports are traced.

- **`existing_processes`** - All existing processes are traced.

- **`existing_ports`** - All existing ports are traced.

- **`t:atom/0`** - The process or port with the corresponding registered name is
  traced. The process or port can on another Erlang node.
  The node must be in the list of traced nodes (see `dbg:n/1` and `dbg:tracer/3`).

- **`t:integer/0`** - The process `<0.Item.0>` is traced.

- **`{X, Y, Z}`** - The process `<X.Y.Z>` is traced.

- **`t:string/0`** - If the `Item` is a string "<X.Y.Z>" as returned from
  [`pid_to_list/1`](`erlang:pid_to_list/1`), the process `<X.Y.Z>` is traced.

[](){: #flags }

`Flags` can be a single atom or a list of flags. The available flags are:

- **`s (send)`** - Traces the messages the process or port sends.

- **`r (receive)`** - Traces the messages the process or port receives.

- **`m (messages)`** - Traces the messages the process or port receives and
  sends.

- **`c (call)`** - Traces global function calls for the process according to the
  trace patterns set in the system (see `dbg:tp/2`).

- **`p (procs)`** - Traces process related events to the process.

- **`ports`** - Traces port related events to the port.

- **`sos (set on spawn)`** - Lets all processes created by the traced process
  inherit the trace flags of the traced process.

- **`sol (set on link)`** - Lets another process, `P2`, inherit the trace flags
  of the traced process whenever the traced process links to `P2`.

- **`sofs (set on first spawn)`** - This is the same as `sos`, but only for the
  first process spawned by the traced process.

- **`sofl (set on first link)`** - This is the same as `sol`, but only for the
  first call to [`link/1`](`erlang:link/1`) by the traced process.

- **`all`** - Sets all flags except `silent`.

- **`clear`** - Clears all flags.

- Other flags accepted by `trace:process/4` or `trace:port/4`
  (e.g., `timestamp`, `arity`, `return_to`).

[`dbg:p(Item)`](`dbg:p/1`) is a shorthand for `dbg:p(Item, [m])`.

This function returns either an error tuple or an `{ok, List}` tuple. The `List`
consists of specifications of how many processes and ports that matched (in the
case of a single pid exactly 1). The specification of matched processes is
`{matched, Node, N}`. If the remote processor call (using `m:rpc`) to a remote
node fails, the `rpc` error message is returned as the fourth element in the
tuple and the number of matched processes is 0.

**Example: Trace messages and process events for a specific process**

```erlang
1> Pid = spawn(fun() -> receive {From,Msg} -> From ! Msg end end).
<0.90.0>
2> dbg:tracer().
{ok,<0.92.0>}
3> dbg:p(Pid, [m,procs]). % Trace messages and process events for Pid
{ok,[{matched,nonode@nohost,1}]}
4> Pid ! {self(),hello}.
(<0.90.0>) << {<0.88.0>,hello} % Received by Pid
{<0.88.0>,hello}
(<0.90.0>) <0.88.0> ! hello    % Sent by Pid
(<0.90.0>) exit normal         % Process event: Pid exited
5> flush().
Shell got hello
ok
```

### Tracing Function Calls ([`dbg:tp/2,3,4`](`dbg:tp/2`), [`dbg:tpl/2,3,4`](`dbg:tpl/2`))

To trace function calls, you need to:

1.  Enable the **`c`**/**`call`** flag for the process(es) that will make the
calls (using `dbg:p/2`).
2.  Set a *trace pattern* for the function(s) you want to trace using `dbg:tp/2`
or `dbg:tpl/2`.

`tp` stands for **t**race **p**attern (for exported functions by default).
`tpl` stands for **t**race **p**attern **l**ocal (for local or remote calls to
local and exported functions).

The general syntax is [`dbg:tp(ModuleOrMFA, MatchSpec)`](`dbg:tp/2`) or
[`dbg:tpl(ModuleOrMFA, MatchSpec)`](`dbg:tpl/2`).

`ModuleOrMFA` can be:

  * `Module :: atom()`: Equivalent to `{Module, '_', '_'}` (trace all functions in the module).
  * `{Module, Function, Arity}`: Trace specific function. `'_'` can be used as a wildcard.

Note that if the `Module` is specified as `'_'`, the
`Function` and `Arity` parts must be specified as `'_'` as well. The
same holds for the `Function` in relation to `Arity`.

`MatchSpec` defines what to trace and how, see
[match specifications](#match-specifications).

  * For simple call tracing, you can insert the empty list `[]`.
  * The most common generic match specifications used can be found as built-in
  aliases.
    * `x` or `exception_trace`: Shows function names, parameters, return
    values, and exceptions. `[{'_',[],[{exception_trace}]}]`
    * `c` or `caller_trace`: Shows function names, parameters, and caller
    information.
    `[{'_',[],[{message,{caller_line}}]}]`
    * `cx` or `caller_exception_trace`: Combines `x` and `c`.
    `[{'_',[],[{exception_trace},{message,{caller_line}}]}]`

**Example using built-in aliases:**

```erlang
1> dbg:tracer().
{ok,<0.90.0>}
2> dbg:p(all, c). % Short for dbg:p(all, call)
{ok,[{matched,nonode@nohost,49}]}
3> dbg:tp(lists, seq, cx). % cx: call and exception tracing with caller info
{ok,[{matched,nonode@nohost,2},{saved,cx}]}
4> lists:seq(1, 3).
(<0.88.0>) call lists:seq(1,3) ({erl_eval,do_apply,7,{"erl_eval.erl",904}})
[1,2,3]
(<0.88.0>) returned from lists:seq/2 -> [1,2,3]
```

Note that the caller info is the function that called lists:seq with file and
line number.

### Tracing Message Events (`dbg:tpe/2`)

By default, if `send` or `receive` tracing is enabled for a process, all such
events are traced. [`dbg:tpe(Event, MatchSpec)`](`dbg:tpe/2`) allows you to
filter these events.

  * `Event`: `send` or `'receive'`.
  * `MatchSpec`: A [match specifications](#match-specifications).
      * For `send`: Matches on `[Receiver, Msg]`.
      * For `'receive'`: Matches on `[Node, Sender, Msg]`.


### Managing Trace Patterns

You can display, remove, save and load trace pattern matchspecifications, if
any of these bullets are of interest, click the title to read the documentation.

  * **[`dbg:ltp()` (List Trace Patterns)](`dbg:ltp/0`):** Lists all saved match
  specifications (from previous `tp` calls where a pattern was complex enough to
  be saved) and built-in aliases.
  * **[`dbg:dtp()` (Delete Trace Patterns)](`dbg:dtp/0`):** Deletes all *saved* (not built-in)
  match specifications.
  * **[`dbg:dtp(N)` (Delete Trace Pattern `N`)](`dbg:dtp/1`):** Deletes a specific saved
  pattern by its ID `N`.
  * **[`dbg:wtp(FileName)` (Write Trace Patterns)](`dbg:wtp/1`):** Saves current (saved and
  built-in) match specifications to `FileName`.
  * **[`dbg:rtp(FileName)` (Read Trace Patterns)](`dbg:rtp/1`):** Reads match specifications from `FileName` and merges them.

To stop tracing specific functions, you clear their trace patterns.

  * **[`dbg:ctp(ModuleOrMFA)`](`dbg:ctp/1`):** Clears both global and local trace patterns.
      * [`ctp()`](`dbg:ctp/0`): Clears all trace patterns for all functions.
      * `ctp(Module)`: Clears patterns for all functions in `Module`.
      * `ctp(Module, Function)` / `ctp(Module, Function, Arity)`: More specific.
  * **[`dbg:ctpl(ModuleOrMFA)`](`dbg:ctpl/1`):** Clears only *local* trace patterns (set by `tpl`).
  * **[`dbg:ctpg(ModuleOrMFA)`](`dbg:ctpg/1`):** Clears only *global* trace patterns (set by `tp`).
  * **[`dbg:ctpe(Event)`](`dbg:ctpe/1`):** Clears the match specification for `send` or `'receive'`, reverting to tracing all such events if the flag is set.

## Match Specifications

Match Specifications are a powerful mini-language used to define conditions for
tracing and actions to take. The `dbg:tp/2`, `dbg:tpl/2` and `dbg:tpe/2` functions
accept them. For a description of the format for the `MatchSpec` argument, see
[_Match Specifications in Erlang_](`e:erts:match_spec.md`), which explains the
general match specification language. For most users, `dbg:fun2ms/1` explained
below will do.

A match specification is a list of tuples: `[{MatchHead, Guard, BodyActions}]`.

  * `MatchHead`: Patterns to match function arguments. `'_'` matches anything.
  `'$1'` is a variable.
  * `Guard`: Conditions that must be true.
  * `BodyActions`: Actions like `{return_trace}` (trace return value),
  `{message, term()}` (include extra info), `{set_seq_token, ...}`.

### Creating Match Specifications with `dbg:fun2ms/1`
You can use `dbg:fun2ms/1` to translate a literal Erlang fun into a match
specification. This often feels more natural than writing the raw match spec.
The fun must take a single list argument (matching the function arguments) and
its body can use guard expressions and special tracing functions.
The parse transform module `m:ms_transform` must be enabled. The easiest way to
enable it is by adding the following line to the source file:
`-include_lib("stdlib/include/ms_transform.hrl").`
In the shell its already enabled.

The head of the fun must be a single pattern that matches a list. That pattern
will be used to match the arguments for the call:

```erlang
1> dbg:fun2ms(fun([_,_]) -> true end). % Matches a function with two arguments
[{['_','_'],[],[true]}]
2> dbg:fun2ms(fun([A]) when is_atom(A) -> return_trace() end).
[{['$1'],[{is_atom,'$1'}],[{return_trace}]}]
```

The first match specification matches when a function having two
arguments is called. The second matches when a function with more than
6 arguments is called.

## Trace Sessions

To avoid interference between different tracing activities, you can create
isolated `dbg` sessions.

First you create a session with [`dbg:session_create(Name)`](`dbg:session_create/1`)
where the name is an atom, a [`session/0`](`t:dbg:session/0`) is returned.
Several sessions may have the same name.

When you have the [`session/0`](`t:dbg:session/0`), you use [`dbg:session(Session, Fun)`](`dbg:session/2`).
This function runs `dbg` commands within `Fun` using the specified session.

Any `m:dbg` function that is called with in the provided fun
will use the [`session/0`](`t:dbg:session/0`) provided instead of the default
`dbg` session. This means that the tracing will be isolated
from other tracing users on the system.

When you no longer need the session, use [`dbg:session_destroy(Session)`](`dbg:session_destroy/1`).

*Example*:

```erlang
1> S = dbg:session_create(my_session).
<0.91.0>
2> dbg:session(S, fun() -> dbg:tracer(), dbg:p(all,c), dbg:tp(lists,seq,x) end).
{ok,[{matched,nonode@nohost,2},{saved,x}]}
3> lists:seq(1, 10).
(<0.89.0>) call lists:seq(1,10)
(<0.89.0>) returned from lists:seq/2 -> [1,2,3,4,5,6,7,8,9,10]
[1,2,3,4,5,6,7,8,9,10]
4> dbg:session_destroy(S).
ok
```

The state of the [`session/0`](`t:dbg:session/0`) is preserved in between `dbg:session/2` calls, so
you can call `dbg:session/2` multiple times when debugging you application.

*Example*:

```erlang
1> S = dbg:session_create(my_session).
<0.91.0>
%% Setup the initial traces
2> dbg:session(S, fun() -> dbg:tracer(), dbg:p(self(),c), dbg:tp(lists,seq,x) end).
{ok,[{matched,nonode@nohost,2},{saved,x}]}
3> lists:seq(1, 3).
(<0.89.0>) call lists:seq(1,3)
(<0.89.0>) returned from lists:seq/2 -> [1,2,3]
[1,2,3]
%% Add an additional trace pattern
4> dbg:session(S, fun() -> dbg:tpl(lists,seq_loop,x) end).
ok
5> lists:seq(1, 3).
(<0.89.0>) call lists:seq(1,3)
(<0.89.0>) call lists:seq_loop(3,3,[])
(<0.89.0>) call lists:seq_loop(1,1,[2,3])
(<0.89.0>) returned from lists:seq_loop/3 -> [1,2,3]
(<0.89.0>) returned from lists:seq_loop/3 -> [1,2,3]
(<0.89.0>) returned from lists:seq/2 -> [1,2,3]
[1,2,3]
6> dbg:session_destroy(S).
ok
```


## Trace on Remote Nodes

The `dbg` server keeps a list of nodes where tracing should be
performed. Whenever a `dbg:tp/2` call or a `dbg:p/2` call is made, it is
executed for all nodes in this list including the local node (except
for `dbg:p/2` with a specific `t:pid/0` or `t:port/0` as first argument,
in which case the command is executed only on the node where the
designated process or port resides).

**[`dbg:n(Nodename)`](`dbg:n/1`):** When this function is called, it starts a
tracer process on the remote node, which will send all trace messages to the
tracer process on the local node (via the Erlang distribution). If no tracer
process is running on the local node, the error reason `no_local_tracer` is
returned. The tracer process on the local node must be started with
the [`dbg:tracer/0,2`](`dbg:tracer/2`) function.

If `Nodename` is the local node, the error reason `cant_add_local_node` is
returned.
The function will also return an error if the node `Nodename` is not reachable.

If a trace port (see `dbg:trace_port/2`) is running on the local node, remote nodes
cannot be traced with a tracer process. The error reason
`cant_trace_remote_pid_to_local_port` is returned. However, a trace port can be
started on the remote node with the `dbg:tracer/3` function.

**[`dbg:tracer(Nodename, Type, Data)`](`dbg:tracer/3`)**: An independent tracer
is started on the node (`Nodename`) and the node is added to the list of traced nodes.

> #### Note {: .info }
>
> [`dbg:tracer(Nodename, Type, Data)`](`dbg:tracer/3`) is not equivalent to `dbg:n/1`. While `dbg:n/1` starts a process tracer
> which redirects all trace information to a process tracer on the local node
> (that is, the trace control node), `dbg:tracer/3` starts any type of tracer,
> independent of the type of tracer on the trace control node.


### Managing nodes

`dbg` can trace processes and functions on other Erlang nodes in a distributed system.

  * **[`dbg:cn(Nodename)` (Clear Node)](`dbg:cn/1`):** Removes `Nodename` from the list. Tracing already active on that node continues but new global `tp/p` calls won't affect it.
  * **[`dbg:ln()` (List Nodes)](`dbg:ln/0`):** Shows the list of currently traced nodes.

## Trace Ports for Lower Overhead

For high-volume tracing, sending messages to an Erlang process can be too slow.
A trace port is an Erlang port to a dynamically linked-in driver that
handles trace messages directly, without the overhead of sending them
as messages to an Erlang process. Using a trace port significantly
lowers the overhead imposed by tracing.

### Creating Trace Ports (`dbg:trace_port/2`)

[`dbg:trace_port(Type, Parameters)`](`dbg:trace_port/2`) returns a `fun/0` that,
when called, creates and returns a port. This fun is then passed as the second argument to
[`dbg:tracer(port, Fun)`](`dbg:tracer/2`).

Two trace drivers are available: the `file` and the `ip` trace drivers.

  * **`file`**: Writes trace messages to binary file(s).
      * `Parameters`: `Filename` or a wrap files specification:
        `{Filename, wrap, Suffix}`
        `{Filename, wrap, Suffix, WrapSize}`
        `{Filename, wrap, Suffix, WrapSize, WrapCnt}`
        `{Filename, wrap, Suffix, {time, WrapTime}, WrapCnt}`
        Wrap files limit disk space by rotating through `WrapCnt` files, each up to `WrapSize` or open for `WrapTime`.
  * **`ip`**: Opens a TCP/IP listening port. A client connects to receive trace messages.
      * `Parameters`: `PortNumber` or `{PortNumber, QueSize}`.

The `file` trace driver expects a filename or a wrap files
specification as parameter. A file is written with a high degree of
buffering, which is why there is no guarantee that all are saved in the
file in case of a system crash.

A wrap files specification is used to limit the disk space consumed by the
trace. The trace is written to a limited number of files each with a limited
size. The actual filenames are `Filename ++ SeqCnt ++ Suffix`, where `SeqCnt`
counts as a decimal string from `0` to `WrapCnt` and then around again from `0`.
When a trace term written to the current file makes it longer than `WrapSize`,
that file is closed, and if the number of files in this wrap trace is as many as
`WrapCnt` the oldest file is deleted, and a new file is opened to become the
current. Thus, when a wrap trace has been stopped, there are at most `WrapCnt`
trace files saved with a size of at least `WrapSize` (but not much larger),
except for the last file that might even be empty. The default values are
`WrapSize = 128*1024` and `WrapCnt = 8`.

The `SeqCnt` values in the filenames are all in the range `0` through `WrapCnt`
with a gap in the circular sequence. The gap is needed to find the end of the
trace.

If the `WrapSize` is specified as `{time, WrapTime}`, the current file is closed
when it has been open more than `WrapTime` milliseconds, regardless of it being
empty or not.

The `ip` trace driver has a queue of `QueSize` messages waiting to be delivered.
If the driver cannot deliver messages as fast as they are produced by the
runtime system, a special message is sent, which indicates how many messages
that are dropped. That message will arrive at the handler function specified in
`dbg:trace_client/3` as the tuple `{drop, N}` where `N` is the number of consecutive
messages dropped. In case of heavy tracing, drops are likely to occur, and they
surely occur if no client is reading the trace messages. The default value of
`QueSize` is 200.

### Reading Trace Port Data ([`dbg:trace_client/2,3`](`dbg:trace_client/2`))

[`dbg:trace_client(Type, Parameters)`](`dbg:trace_client/2`) Starts a trace
client that reads the output
created by a trace port driver (see `dbg:trace_port/2`) and handles it in mostly
the same way as a tracer process created by the `dbg:tracer/0` function.
[`dbg:trace_client(Type, Parameters, HandlerSpec)`](`dbg:trace_client/3`) This
function works exactly as
`dbg:trace_client/2`, but allows you to write your own handler function.

If `Type` is `file`, the client reads all trace messages stored in the
file named `Filename` or specified by `WrapFilesSpec` (must be the
same as used when creating the trace) and lets the default handler
function format the messages on the console. This is one way to
interpret the data stored in a file by the file trace port driver.

If `Type` is `follow_file`, the client behaves as in the `file` case, but keeps
trying to read (and process) more data from the file until stopped by
`dbg:stop_trace_client/1`. `WrapFilesSpec` is not allowed as second argument for
this `Type`.

If `Type` is `ip`, the client connects to the TCP/IP port `PortNumber` on the
host `Hostname`, from where it reads trace messages until the TCP/IP connection
is closed. If no `Hostname` is specified, the local host is assumed.

The handler function works mostly as the one described in `dbg:tracer/2`,
but must also be prepared to handle trace messages of the form `{drop,
N}`, where `N` is the number of dropped messages. This pseudo trace
message will only occur if the `ip` trace driver is used.

For trace type `file`, the pseudo trace message `end_of_trace` will appear at
the end of the trace. The return value from the handler function is in this case
ignored.

**Example: Using an IP trace port and connecting to it from another node**
As an example, one can let trace messages be sent over the network to another
Erlang node (preferably _not_ distributed), where the formatting occurs.

On the node `stack` there exists an Erlang node `ant@stack`. In the
shell, type the following:

```erlang
ant@stack> dbg:tracer(port, dbg:trace_port(ip, 4711)).
<0.17.0>
ant@stack> dbg:p(self(), send).
{ok,1}
```

All trace messages are now sent to the trace port driver, which in turn listens
for connections on the TCP/IP port 4711. If we want to see the messages on
another node, preferably on another host, we do like this:

```erlang
1> dbg:trace_client(ip, {"stack", 4711}).
<0.42.0>
```
If we now send a message from the shell on the node `ant@stack`, where all sends
from the shell are traced:

```erlang
ant@stack> self() ! hello.
hello
```

The following will appear at the console on the node that started the trace
client:

```erlang
(<0.23.0>) <0.23.0> ! hello
(<0.23.0>) <0.22.0> ! {shell_rep,<0.23.0>,{value,hello,[],[]}}
```

The last line is generated due to internal message passing in the Erlang shell.
The pids will vary.

### Controlling Trace Ports

  * **[`dbg:flush_trace_port()`](`dbg:flush_trace_port/0`) / 
  [`dbg:flush_trace_port(Node)`](`dbg:flush_trace_port/1`):**
  Flushes internal buffers of the trace port driver on the local/specified node
  (currently for `file` driver).
  * **[`dbg:trace_port_control(Operation)`](`dbg:trace_port_control/1`) / 
  [`dbg:trace_port_control(Node, Operation)`](`dbg:trace_port_control/2`):**
      * `Operation = flush`: Same as above.
      * `Operation = get_listen_port`: For `ip` driver, returns `{ok, IpPortNumber}`.
  * **[`dbg:stop_trace_client(Pid)`](`dbg:stop_trace_client/1`):** Shuts down
  the trace client `Pid`.

## Sequential Tracing (`seq_trace`)

The `m:dbg` module is primarily targeted towards tracing through the
`trace:process/4` function. It is sometimes desired to trace messages in a more
delicate way, which can be done with the help of the `m:seq_trace` module.

`m:seq_trace` implements sequential tracing (known in the AXE10 world, and
sometimes called "forlopp tracing"). `m:dbg` can interpret messages generated from
`m:seq_trace` and the same tracer function for both types of tracing can be used.
The `m:seq_trace` messages can also be sent to a trace port for further analysis.

As a match specification can turn on sequential tracing, the combination of
`m:dbg` and `m:seq_trace` can be powerful. This brief example shows a session
where sequential tracing is used to trace the `m:dbg` module and the trace itself:

```erlang
1> dbg:tracer().
{ok,<0.30.0>}
2> {ok, Tracer} = dbg:get_tracer().
{ok,<0.31.0>}
3> seq_trace:set_system_tracer(Tracer).
false
4> dbg:tp(dbg, get_tracer, 0, [{[],[],[{set_seq_token, send, true}]}]).
{ok,[{matched,nonode@nohost,1},{saved,1}]}
5> dbg:p(all,call).
{ok,[{matched,nonode@nohost,22}]}
6> dbg:get_tracer(), seq_trace:set_token([]).
(<0.25.0>) call dbg:get_tracer()
SeqTrace [0]: (<0.25.0>) <0.30.0> ! {<0.25.0>,get_tracer} [Serial: {2,4}]
SeqTrace [0]: (<0.30.0>) <0.25.0> ! {dbg,{ok,<0.31.0>}} [Serial: {4,5}]
{1,0,5,<0.30.0>,4}
```

This session sets the system_tracer to the same process as the
ordinary tracer process (i. e. <0.31.0>) and sets the trace pattern
for the function `dbg:get_tracer` to one that has the action of
setting a sequential token. When the function is called by a traced
process (all processes are traced in this case), the process gets
"contaminated" by the token and `m:seq_trace` messages are sent both for
the server request and the response. The `seq_trace:set_token([])`
after the call clears the `m:seq_trace` token, which is why no messages
are sent when the answer propagates via the shell to the console
port. Otherwise the output would have been more noisy.

## Avoiding Overloads

Tracing can generate a significant amount of data, potentially
overwhelming your system if not managed carefully. To prevent performance
degradation or even crashes, consider these strategies:

**Time-Limited Tracing**: One effective method is to automatically stop tracing
after a set period.

```erlang
dbg:tracer(), dbg:p(all,[c]), dbg:tpl(lists,map,x), timer:sleep(1000), dbg:stop().
```

**Be Specific:**

**Processes:** Instead of `dbg:p(all, Flags).`, try to pinpoint specific
processes if you know which ones are relevant: `dbg:p(Pid, Flags)`. You can also
trace newly spawned processes with `dbg:p(new, Flags).`.

**Modules & Functions:** Rather than tracing all calls, narrow down to specific
modules and functions with `dbg:tp/2` or `dbg:tpl/2`

**Use Match Specifications:** For example, to only trace calls to
`my_module:my_function/1` when the first argument is the atom error:

```erlang
dbg:tpl(my_module, my_function, dbg:fun2ms(fun([error])->true end)).
```

**Limit Trace Flags:** Only enable the flags essential for your debugging task
(e.g., m for message passing, c for function calls).

**Trace to File for High Volumes:** If you anticipate a large volume of trace
data, tracing directly to the console can become a bottleneck. Consider tracing
to a file instead, see [trace ports](#trace-ports-for-lower-overhead).

```erlang
dbg:tracer(port, {file, "trace_output.log"}),
% ... your other dbg commands ...
timer:sleep(5000),
dbg:stop().
```

## Avoiding Deadlocks

When tracing function calls on a group leader process (an I/O process), there is
risk of causing a deadlock. This will happen if a group leader process generates
a trace message and the tracer process, by calling the trace handler function,
sends an I/O request to the same group leader. The problem can only occur if the
trace handler prints to the tty using an `m:io` function such as
[`format/2`](`io:format/2`). Note that when `dbg:p(all, call)` is called, IO
processes are also traced. Here is an example:

```erlang
%% Using a default line editing shell
1> dbg:tracer(process, {fun(Msg,_) -> io:format("~p~n", [Msg]), 0 end, 0}).
{ok,<0.37.0>}
2> dbg:p(all, [call]).
{ok,[{matched,nonode@nohost,25}]}
3> dbg:tp(mymod,[{'_',[],[]}]).
{ok,[{matched,nonode@nohost,0},{saved,1}]}
4> mymod: % TAB pressed here
%% -- Deadlock --
```

Here is another example:

```erlang
%% Using a shell without line editing (oldshell)
1> dbg:tracer(process).
{ok,<0.31.0>}
2> dbg:p(all, [call]).
{ok,[{matched,nonode@nohost,25}]}
3> dbg:tp(lists,[{'_',[],[]}]).
{ok,[{matched,nonode@nohost,0},{saved,1}]}
% -- Deadlock --
```

The reason we get a deadlock in the first example is because when TAB is pressed
to expand the function name, the group leader (which handles character input)
calls `mymod:module_info()`. This generates a trace message which, in turn,
causes the tracer process to send an IO request to the group leader (by calling
`io:format/2`). We end up in a deadlock.

In the second example we use the default trace handler function. This
handler prints to the tty by sending IO requests to the `user`
process. When Erlang is started in the oldshell mode, the shell
process will have `user` as its group leader and so will the tracer
process in this example. Since `user` calls functions in `lists` we
end up in a deadlock as soon as the first IO request is sent.

Here are a few suggestions for avoiding deadlock:

- Do not trace the group leader of the tracer process. If tracing has been
  switched on for all processes, call `dbg:p(TracerGLPid, clear)` to stop tracing
  the group leader (`TracerGLPid`).
  [`process_info(TracerPid, group_leader)`](`process_info/2`) tells you which
  process this is (`TracerPid` is returned from `dbg:get_tracer/0`).
- Do not trace the `user` process if using the default trace handler function.
- In your own trace handler function, call `erlang:display/1` instead of an `io`
  function or, if `user` is not used as group leader, print to `user` instead of
  the default group leader. Example: `io:format(user, Str, Args)`.

[](){: #help }

## Getting Information and Help

  * **[`dbg:i()` (Information)](`dbg:i/0`):** Displays information about all currently traced processes and ports and their active trace flags.
  * **[`dbg:h()` (Help)](`dbg:h/0`):** Lists available help items.
  * **[`dbg:h(Item :: atom())`](`dbg:h/1`):** Gives brief help for a specific `dbg` function or concept (e.g., `dbg:h(tp)`).
  * **[`dbg:get_tracer()`](`dbg:get_tracer/0`) / 
  [`dbg:get_tracer(Node)`](`dbg:get_tracer/1`):** Returns the process, port, or tracer module handling traces on the local/specified node.
  * Consult the [dbg module documentation](`m:dbg`).
