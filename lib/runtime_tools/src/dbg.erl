%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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
-module(dbg).
-moduledoc """
The Text Based Trace Facility

This module implements a text based interface to the
`trace:process/4`, `trace:port/4`, and `trace:function/4` BIFs,
simplifying tracing of functions, processes, ports, and messages.

To quickly get started on tracing function calls you can use the
following code in the Erlang shell:

```erlang
1> dbg:tracer().  % Start the default trace message receiver
{ok,<0.90.0>}
2> dbg:p(all, c). % Set upp call tracing on all processes
{ok,[{matched,nonode@nohost,49}]}
3> dbg:tp(lists, seq, cx). %  Set up call and exception tracing on lists:seq/2,3
{ok,[{matched,nonode@nohost,2},{saved,cx}]}
4> lists:seq(1, 10).
(<0.88.0>) call lists:seq(1,10) ({erl_eval,do_apply,7,{"erl_eval.erl",904}})
[1,2,3,4,5,6,7,8,9,10]
(<0.88.0>) returned from lists:seq/2 -> [1,2,3,4,5,6,7,8,9,10]
```

The utilities are also suitable to use in system testing on large systems, where
other tools have too severe impact on the system performance. Some primitive
support for sequential tracing is also included; see the
[advanced topics](`m:dbg#advanced`) section.

## Simple tracing from the shell with no prior set up

To trace a call to a function with minimal fuss, call [`dbg:c(Module,
Name, Arguments)`](`dbg:c/3`). `dbg:c/3` starts a temporary trace
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

Four trace events are generated:

- A send event (`!`) for the sending of a request from the current process
  to the `application_controller` process.
- A schedule-out event (`out`) when the current process schedules out while
  waiting in a `receive` for the reply to arrive.
- A schedule-in event (`in`) when the current process is scheduled in when
  reply has arrived.
- A `receive` event (`<<`) when the current process retrieves the reply from
  the `application_controller` process.

The `dbg:c/4` function has a fourth argument for specifying the trace flags.
Here is how to only show message sending and receiving:

```erlang
2> dbg:c(application, which_applications, [], m).
(<0.96.0>) <0.45.0> ! {'$gen_call',{<0.96.0>,
                                    [alias|
                                     #Ref<0.0.12291.270031856.1478295555.230496>]},
                                   which_applications}
(<0.96.0>) << {[alias|#Ref<0.0.12291.270031856.1478295555.230496>],
               [{stdlib,"ERTS  CXC 138 10","5.2.1"},
                {kernel,"ERTS  CXC 138 10","9.2.2"}]}
[{stdlib,"ERTS  CXC 138 10","5.2.1"},
 {kernel,"ERTS  CXC 138 10","9.2.2"}]
```

## Tracing from the shell

Another way of tracing from the shell is to explicitly start a _tracer_ and
set the _trace flags_ of your choice on the processes you want to trace.
For example, here is how to trace messages and process events:

```erlang
1> Pid = spawn(fun() -> receive {From,Msg} -> From ! Msg end end).
<0.90.0>
2> dbg:tracer().
{ok,<0.92.0>}
3> dbg:p(Pid, [m,procs]).
{ok,[{matched,nonode@nohost,1}]}
4> Pid ! {self(),hello}.
(<0.90.0>) << {<0.88.0>,hello}
{<0.88.0>,hello}
(<0.90.0>) <0.88.0> ! hello
(<0.90.0>) exit normal
5> flush().
Shell got hello
ok
```

In order to trace functions call, in addition to enabling the `call` trace flag
for the process, it is also necessary to set a _trace pattern_ for the functions
to trace.

_Example:_

```erlang
1> dbg:tracer().
{ok,<0.90.0>}
2> dbg:p(all, call).
{ok,[{matched,nonode@nohost,49}]}
3> dbg:tp(lists, last, 1, []).
{ok,[{matched,nonode@nohost,1}]}
4> lists:last([a,b,c,d,e]).
(<0.88.0>) call lists:last([a,b,c,d,e])
e
5> dbg:tp(lists, last, 1, [{'_',[],[{return_trace}]}]).
{ok,[{matched,nonode@nohost,1},{saved,1}]}
6> lists:last([a,b,c,d,e]).
(<0.88.0>) call lists:last([a,b,c,d,e])
(<0.88.0>) returned from lists:last/1 -> e
e
```

[](){: #advanced }

## Advanced topics - combining with seq_trace

The `dbg` module is primarily targeted towards tracing through the
`trace:process/4` function. It is sometimes desired to trace messages in a more
delicate way, which can be done with the help of the `m:seq_trace` module.

`m:seq_trace` implements sequential tracing (known in the AXE10 world, and
sometimes called "forlopp tracing"). `dbg` can interpret messages generated from
`seq_trace` and the same tracer function for both types of tracing can be used.
The `seq_trace` messages can also be sent to a trace port for further analysis.

As a match specification can turn on sequential tracing, the combination of
`dbg` and `seq_trace` can be powerful. This brief example shows a session
where sequential tracing is used to trace the `dbg` module and the trace itself:

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
"contaminated" by the token and `seq_trace` messages are sent both for
the server request and the response. The `seq_trace:set_token([])`
after the call clears the `seq_trace` token, which is why no messages
are sent when the answer propagates via the shell to the console
port. Otherwise the output would been more noisy.

## Note of caution

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
""".
%% Exports that use `dbg:session/1`.
-export([start/0, stop/0, stop_clear/0,
         tracer/0, tracer/2, tracer/3, get_tracer/0, get_tracer/1,
         p/1, p/2,
         i/0,
         tp/2, tp/3, tp/4, 
	 tpe/2, ctpe/1,
	 ctp/0, ctp/1, ctp/2, ctp/3, tpl/2, tpl/3, tpl/4, ctpl/0, ctpl/1, 
	 ctpl/2, ctpl/3, ctpg/0, ctpg/1, ctpg/2, ctpg/3,
         ltp/0, wtp/1, rtp/1, dtp/0, dtp/1,
         n/1, cn/1, ln/0]).

%% Session exports
-export([session_create/1, session_destroy/1, session/2]).

%% Session less exports
-export([c/3, c/4, h/0, h/1]).

-export([trace_port/2, flush_trace_port/0, flush_trace_port/1,
	 trace_port_control/1, trace_port_control/2, trace_client/2, 
	 trace_client/3, stop_trace_client/1]).

-export([transform_flags/1,dhandler/2]).

-export([fun2ms/1]).

%% Local exports
-export([erlang_trace/4,get_info/0,deliver_and_flush/1,deliver_and_flush/2,do_relay/3]).

%% Debug exports
-export([wrap_presort/2, wrap_sort/2, wrap_postsort/1, wrap_sortfix/2,
	 match_front/2, match_rear/2,
	 match_0_9/1]).

-doc """
    A `m:dbg` session that can be used by `session/2` to
    create isolated debugging sessions.
    """.
-opaque session() :: pid().
-export_type([session/0]).

-type match_pattern() :: atom() | list().
-type match_spec()    :: [{match_pattern(), [_], [_]}].
-type built_in_alias() :: x | c | cx.

-type trace_wrap_files_spec() ::
        {file:name_all(), wrap, Suffix :: string()} |
        {file:name_all(), wrap, Suffix :: string(),
         WrapSize :: trace_wrap_file_size()} |
        {file:name_all(), wrap, Suffix :: string(),
         WrapSize :: trace_wrap_file_size(), WrapCnt :: pos_integer()}.
-type trace_wrap_file_size() :: non_neg_integer() | {time, WrapTime :: pos_integer()}.

-export_type([match_spec/0]).

-deprecated([{stop_clear,0, "use dbg:stop/0 instead"}]).

%%% Shell callable utility
-doc """
Pseudo function that by means of a parse transform translates the
_literal_ fun typed as parameter in the function call to a [match
specification](`e:erts:match_spec.md`).

The meaning of "literal" is that the fun needs to textually be written
as the argument of the function call; it cannot be held in a variable
which in turn is passed to the function. Furthermore, the parse
transform module `m:ms_transform` must be enabled. The easiest way to
enable it is by adding the following line to the source file:

```
-include_lib("stdlib/include/ms_transform.hrl").
```

Failing to include `ms_transform.hrl` in the source will result in a runtime
error, not a compile-time error.

This function can also be invoked directly from the Erlang shell, as shown in
the examples that follow.

The head of the fun must be a single pattern that matches a list. That pattern
will be used to match the arguments for the call:

_Examples_:

```erlang
1> dbg:fun2ms(fun([_,_]) -> true end).
[{['_','_'],[],[true]}]
2> dbg:fun2ms(fun(Args) when length(Args) > 6 -> true end).
[{'$1',[{'>',{length,'$1'},6}],[true]}]
```

The first match specification matches when a function having two
arguments is called. The second matches when a function with more than
6 arguments is called.

_Examples_:

```erlang
1> dbg:fun2ms(fun(42) -> true end).
Error: dbg:fun2ms requires fun with single variable or list parameter
{error,transform_error}
2> dbg:fun2ms(fun([<<H,T/binary>>]) -> true end).
Error: fun head contains bit syntax matching of variable 'H', which cannot be translated into match_spec
{error,transform_error}
```

The preceding two examples show what happens when a fun cannot be
translated into a match specification. In the first example, the fun
head connot possibly match a list. In the second example, an attempt is made
to take apart a binary using the bit syntax, which is currently not
supported in match specifications.

However, note that literal binaries *can* be matched:

```erlang
1> dbg:fun2ms(fun([<<"abc">>]) -> true end).
[{[<<"abc">>],[],[true]}]
```

Match specifications support a large subset of the
[guard expressions](`e:system:expressions.md#guard-expressions`) supported
by Erlang, but not all. For example, updating a map is currently not supported:

```erlang
1> dbg:fun2ms(fun([M]) when map_size(M#{a => b}) > 2 -> true end).
Error: the language element map (in guard) cannot be translated into match_spec
{error,transform_error}
```

However, creating a map in a guard is allowed:

```erlang
1> dbg:fun2ms(fun([M]) when map_size(#{a => b}) > 2 -> true end).
[{['$1'],[{'>',{map_size,#{a => b}},2}],[true]}]
```

Variables from the environment can be imported, so this works:

```erlang
1> X = 3.
3
2> dbg:fun2ms(fun([M,N]) when N > X  -> return_trace() end).
[{['$1','$2'],[{'>','$2',{const,3}}],[{return_trace}]}]
```
The imported variables will be replaced by `const` expressions, which
is consistent with the static scoping for Erlang funs.

In the body of the fun, only guard expressions and calls to the
[special functions for tracing](`e:erts:match_spec.md#functions-allowed-only-for-tracing`)
are allowed.

_Examples_:

```erlang
1> dbg:fun2ms(fun([A]) when is_atom(A) -> return_trace() end).
[{['$1'],[{is_atom,'$1'}],[{return_trace}]}]
2> dbg:fun2ms(fun(_) -> erlang:garbage_collect() end).
Error: fun containing the remote function call 'erlang:garbage_collect/0' (called in body) cannot be translated into match_spec
{error,transform_error}
```

> #### Warning {: .warning }
>
> If the parse transform is not applied to a module which calls `dbg:fun2ms/1`,
> the call will fail in runtime with a `badarg` exception.

More information is available in the documentation for module `m:ms_transform`
in STDLIB.
""".
-spec fun2ms(LiteralFun) -> MatchSpec when
      LiteralFun :: fun((term()) -> term()),
      MatchSpec :: match_spec().
fun2ms(ShellFun) when is_function(ShellFun) ->
    % Check that this is really a shell fun...
    case erl_eval:fun_data(ShellFun) of
        {fun_data,ImportList,Clauses} ->
            case ms_transform:transform_from_shell(
                   ?MODULE,Clauses,ImportList) of
                {error,[{_,[{_,_,Code}|_]}|_],_} ->
                    Modifier = modifier(),
                    io:format("Error: ~"++Modifier++"s~n",
                              [ms_transform:format_error(Code)]),
                    {error,transform_error};
                Else ->
                    Else
            end;
        false ->
            exit({badarg,{?MODULE,fun2ms,
                          [function,called,with,real,'fun',
                           should,be,transformed,with,
                           parse_transform,'or',called,with,
                           a,'fun',generated,in,the,
                           shell]}})
    end.

-doc """
    Create a new `m:dbg` session with the given `Name`.

    The session is linked with the calling process and will be

    Multiple sessions can have the same name.

    > #### Note {: .info }
    >
    > The session functionality is experimental in Erlang/OTP 27
    > and may change in future releases without notice.
    """.
-doc(#{ since => ~"OTP 27.0" }).
-spec session_create(atom()) -> session().
session_create(Name) ->
    S = self(),
    Dbg = spawn(
            fun() ->
                    erlang:monitor(process, S),
                    init(Name, S)
            end),
    receive {Dbg, started} -> ok end,
    Dbg.

-define(DBG_SESSION_TAG, '$dbg_trace_session').

-doc """
    session(Session, Fun)

    Runs `m:dbg` commands using the provides session, or
    creates a session for the duration of the call if a session name
    is provided.

    Any `m:dbg` function that is called with in the provided fun
    will use the `t:session/0` provided instead of the default
    `dbg` session. This means that the tracing will be isolated
    from other tracing users on the system.

    The function returns the term that the fun returns.

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

    The state of the `t:session/0` is preserved in between `session/2` calls, so
    you can call `session/2` multiple when debugging you application.

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

    > #### Note {: .info }
    >
    > The session functionality is experimental in Erlang/OTP 27
    > and may change in future releases without notice.
    """.
-doc(#{ since => ~"OTP 27.0" }).
-spec session(atom(), (fun(() -> term()))) -> term();
             (session(), (fun(() -> term()))) -> term().
session(SessionName, Fun) when is_atom(SessionName) ->
    Session = session_create(SessionName),
    try
        session(Session, Fun)
    after
        session_destroy(Session)
    end;
session(Session, Fun) ->
    {_Pid, Ref} =
        spawn_monitor(
          fun() ->
                  put(?DBG_SESSION_TAG, Session),
                  exit(try {ok, Fun()} catch E:R:ST -> {error, E, R, ST} end)
          end),
    receive
        {'DOWN', Ref, _, _, {ok, Result}} ->
            Result;
        {'DOWN', Ref, _, _, {error, E, R, ST}} ->
            erlang:raise(E, R, ST)
    end.

get_dbg_session() ->
    get(?DBG_SESSION_TAG).

get_trace_session(Node) ->
    %% We cannot use req here as the dbg server might be busy serving another
    %% request
    case erlang:process_info(ensure(),[{dictionary,Node}]) of
        [{{dictionary,Node},undefined}] ->
            {error, {no_tracer_on_node,Node}};
        [{{dictionary,Node},{_Relay, _Tracer, Session}}] ->
            Session
    end.

%%% Client functions.

%%
%% n(Node) -> {ok, Node} | {error, Reason}
%% Adds Node to the list of traced nodes.
%%
-doc """
Adds a remote node (`Nodename`) to the list of nodes where tracing is
performed.

`n` stands for **n**ode.

The `dbg` server keeps a list of nodes where tracing should be
performed. Whenever a `tp/2` call or a `p/2` call is made, it is
executed for all nodes in this list including the local node (except
for `p/2` with a specific `t:pid/0` or `t:port/0` as first argument,
in which case the command is executed only on the node where the
designated process or port resides).

When this function is called, it starts a tracer process on the remote
node, which will send all trace messages to the tracer process on the
local node (via the Erlang distribution). If no tracer process is
running on the local node, the error reason `no_local_tracer` is
returned. The tracer process on the local node must be started with
the [`tracer/0,2`](`tracer/2`) function.

If `Nodename` is the local node, the error reason `cant_add_local_node` is
returned.

If a trace port (see `trace_port/2`) is running on the local node, remote nodes
cannot be traced with a tracer process. The error reason
`cant_trace_remote_pid_to_local_port` is returned. However, a trace port can be
started on the remote node with the `tracer/3` function.

The function will also return an error if the node `Nodename` is not reachable.
""".
-spec n(Nodename) -> {ok, Nodename} | {error, Reason} when
      Nodename :: node(),
      Reason :: term().
n(Node) when Node =:= node() ->
    {error, cant_add_local_node};
n(Node) ->
    case (catch net_adm:ping(Node)) of
	{'EXIT',_} ->
	    {error, {bad_node, Node}};
	pang ->
	    {error, {nodedown, Node}};
	pong ->
	    req({add_node, Node});
	Other ->
	    {error, Other}
    end.

%%
%% cn(Node) -> ok
%% Remove Node from the list of traced nodes.
%%
-doc """
Clears a node from the list of traced nodes.

`cn` stands for **c**lear **n**ode.

Subsequent calls to `tp/2` and `p/2` will not consider that node, but tracing
already activated on the node will continue to be in effect.

Returns `ok`. This call cannot fail.
""".
-spec cn(Nodename) -> ok when Nodename :: node().
cn(Node) ->
    req({remove_node, Node}).

%%
%% ln() -> ok
%% List traced nodes
%%
-doc """
Shows the list of traced nodes on the console.

`ln` stands for **l**ist **n**odes.
""".
-spec ln() -> ok.
ln() ->
    lists:foreach(fun(X) ->
                          io:format("~p~n",[X])
                  end,
                  req(get_nodes)),
    ok.

%%
%% tp/tpl(Module, Pattern) | tp/tpl(Module,Function,Pattern) |
%% tp/tpl(Module,Function,Arity,Pattern) | tp/tpl({M,F,A},Pattern) 
%% -> {ok, [{matched,Node,N}]} | 
%%    {ok, [{matched,Node,N}, {saved,M}]} | 
%%    {ok, [{saved,M}]} | 
%%    {error, Reason}
%% Set trace pattern for function or group of functions.
%%
-type match_desc() :: [match_info()].
-type match_info() :: {saved, tp_id()} | match_num().
-type match_num() :: {matched, node(), integer()} | {matched, node(), 0, RPCError :: term()}.
-type tp_id() :: pos_integer().
-type tp_module() :: module() | '_'.
-type tp_function() :: atom() | '_'.
-type tp_arity() :: arity() | '_'.
-type tp_match_spec() :: tp_id() | built_in_alias() | [] | match_spec().

-doc #{equiv => tp({Module, Function, '_'}, MatchSpec)}.
-spec tp(Module :: tp_module(), Function :: tp_function(),
         MatchSpec :: tp_match_spec()) ->
          {ok, match_desc()} | {error, term()}.
tp(Module, Function, Pattern) ->
    do_tp({Module, Function, '_'}, Pattern, []).

-doc #{equiv => tp({Module, Function, Arity}, MatchSpec)}.
-spec tp(Module :: tp_module(),
         Function :: tp_function(),
         Arity :: tp_arity(),
         MatchSpec :: tp_match_spec()) ->
          {ok, match_desc()} | {error, term()}.
tp(Module, Function, Arity, Pattern) ->
    do_tp({Module, Function, Arity}, Pattern, []).

-doc """
tp(ModuleOrMFA, MatchSpec)

Enables call trace for one or more exported functions specified by `ModuleOrMFA`.

If `ModuleOrMFA` is an atom (a module name), this function call is equivalent to
`tp({ModuleOrMFA, '_', '_'}, MatchSpec)`.

Otherwise, `ModuleOrMFA` should be `{Module, Function, Arity}`.

`tp` stands for **t**race **p**attern.

All exported functions matching the `{Module, Function, Arity}`
argument will be concerned, but the match specification may further
narrow down the set of function calls generating trace messages.

For a description of the format for the `MatchSpec` argument, see
[_Match Specifications in Erlang_](`e:erts:match_spec.md`), which explains the
general match specification language. The most common generic match
specifications used can be found as built-in aliases; see `ltp/0` below for
details.

The Module, Function and/or Arity parts of the tuple may be specified
as the atom `'_'` which is a wildcard matching all modules, functions,
or arities. Note that if the `Module` is specified as `'_'`, the
`Function` and `Arity` parts must be specified as `'_'` as well. The
same holds for the `Function` in relation to `Arity`.

All nodes added with `n/1` or `tracer/3` will be affected by this call, and if
`Module` is not `'_'` the module will be loaded on all nodes.

The function returns either an error tuple or an `{ok, List}` tuple. The `List`
consists of specifications of how many functions that matched, in the same way
as the processes and ports are presented in the return value of `p/2`.

There may be a tuple `{saved, N}` in the return value, if the `MatchSpec` is not
`[]`. The integer `N` can then be used in subsequent calls to this function
and will stand as an "alias" for the given expression.

If the match specification is invalid, an `{error, Errors}` tuple is
returned.  `Errors` is as a list of tuples `{error, string()}`, where
the string is a textual explanation of the compilation error. For
example:

```erlang
1> dbg:tp({dbg,ltp,0},[{[],[],[{message, two, arguments}, {noexist}]}]).
{error,
 [{error,"Special form 'message' called with wrong number of
          arguments in {message,two,arguments}."},
  {error,"Function noexist/1 does_not_exist."}]}
```
""".
-spec tp(Module | {Module, Function, Arity}, MatchSpec) -> {ok, match_desc()} | {error, term()} when
      Module :: tp_module(),
      Function :: tp_function(),
      Arity :: tp_arity(),
      MatchSpec :: tp_match_spec().
tp(Module, Pattern) when is_atom(Module) ->
    do_tp({Module, '_', '_'}, Pattern, []);
tp({_Module, _Function, _Arity} = X, Pattern) ->
    do_tp(X,Pattern,[]).

-doc #{equiv => tpl({Module, Function, '_'}, MatchSpec)}.
-spec tpl(Module :: tp_module(), Function :: tp_function(), MatchSpec :: tp_match_spec()) ->
          {ok, match_desc()} | {error, term()}.
tpl(Module, Function, Pattern) ->
    do_tp({Module, Function, '_'}, Pattern, [local]).

-doc #{equiv => tpl({Module, Function, Arity}, MatchSpec)}.
-spec tpl(Module :: tp_module(),
          Function :: tp_function(),
          Arity :: tp_arity(),
          MatchSpec :: tp_match_spec()) ->
          {ok, match_desc()} | {error, term()}.
tpl(Module, Function, Arity, Pattern) ->
    do_tp({Module, Function, Arity}, Pattern, [local]).

-doc """
tpl({Module, Function, Arity}, MatchSpec)

Enables call trace for one or more functions specified by `ModuleOrMFA`.

If `ModuleOrMFA` is an atom (a module name), this function call is equivalent to
`tpl({ModuleOrMFA, '_', '_'}, MatchSpec)`.

Otherwise, `ModuleOrMFA` should be `{Module, Function, Arity}`.

`tpl` stands for **t**race **p**attern **l**ocal.

This function works as `tp/2`, but enables tracing for local or remote calls
to both local and exported functions.
""".
-spec tpl(Module | {Module, Function :: tp_function(), Arity :: tp_arity()},
          MatchSpec :: tp_match_spec()) ->
          {ok, MatchDesc :: term()} | {error, term()} when
      Module :: tp_module().
tpl(Module, Pattern) when is_atom(Module) ->
    do_tp({Module, '_', '_'}, Pattern, [local]);
tpl({_Module, _Function, _Arity} = X, Pattern) ->
    do_tp(X,Pattern,[local]).

-doc """
Associates a match specification with trace event `send` or
`'receive'`.

`tpe` stands for **t**race **p**attern **e**vent.

By default all executed
`send` and `'receive'` events are traced if enabled for a process. A match
specification can be used to filter traced events based on sender, receiver,
and/or message content.

For a description of the format for the `MatchSpec` argument, see
[_Match Specifications in Erlang_](`e:erts:match_spec.md`), which explains the
general match specification language.

For `send`, the matching is done on the list `[Receiver, Msg]`. `Receiver` is
the process or port identity of the receiver and `Msg` is the message term. The
pid of the sending process can be accessed with the guard function `self/0`.

For `'receive'`, the matching is done on the list `[Node, Sender, Msg]`. `Node`
is the node name of the sender. `Sender` is the process or port identity of the
sender, or the atom `undefined` if the sender is not known (which may be the
case for remote senders). `Msg` is the message term. The pid of the receiving
process can be accessed by calling `self/0`.

All nodes added with `n/1` or `tracer/3` will be affected by this call.

The return value is the same as for `tp/2`. The number of matched events is
always 1 as [`tpe/2`](`tpe/2`) does not accept any form of wildcards
for argument `Event`.
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec tpe(Event, MatchSpec) -> {ok, MatchDesc :: match_desc()} | {error, term()} when
      Event :: send | 'receive',
      MatchSpec :: tp_match_spec().
tpe(Event, Pattern) when Event =:= send;
			 Event =:= 'receive' ->
    do_tp(Event, Pattern, []).

do_tp(X, Pattern, Flags)
  when is_integer(Pattern);
       is_atom(Pattern) ->
    case ets:lookup(get_pattern_table(), Pattern) of
	[{_,NPattern}] ->
	    do_tp(X, binary_to_term(NPattern), Flags);
	_ ->
	    {error, unknown_pattern}
    end;
do_tp(X, Pattern, Flags) when is_list(Pattern) ->
    Nodes = req(get_nodes),
    case X of
	{M,_,_} when is_atom(M) ->
	    %% Try to load M on all nodes
	    lists:foreach(fun(Node) ->
				  rpc:call(Node, M, module_info, [])
			  end,
			  Nodes);
	_ -> ok
    end,
    case lint_tp(Pattern) of
	{ok,_} ->
	    SaveInfo = case save_pattern(Pattern) of
			   N when is_integer(N), N > 0;  is_atom(N)  ->
			       [{saved, N}];
			   _ ->
			       []
		       end,
	    {ok, do_tp_on_nodes(Nodes, X, Pattern, Flags) ++ SaveInfo};
	Other ->
	    Other
    end.

%% All nodes are handled the same way - also the local node if it is traced
do_tp_on_nodes(Nodes, X, P, Flags) ->
    lists:map(fun(Node) ->
		      case rpc:call(Node,trace,function,[get_trace_session(Node),X,P, Flags]) of
			  N when is_integer(N) ->
			      {matched, Node, N};
			  Else ->
			      {matched, Node, 0, Else}
		      end
	      end,
	      Nodes).

%%
%% ctp/ctpl(Module) | ctp/ctpl(Module,Function) |
%% ctp/ctpl(Module,Function,Arity) | ctp/ctpl({M,F,A}) ->
%% {ok, [{matched, N}]} | {error, Reason}
%% Clears trace pattern for function or group of functions.
%%
-doc #{equiv => ctp({'_', '_', '_'})}.
-spec ctp() -> {ok, MatchDesc :: match_desc()} | {error, term()}.
ctp() ->
    do_ctp({'_','_','_'}, []).

-doc #{equiv => ctp({Module, Function, '_'})}.
-spec ctp(Module :: tp_module(), Function :: tp_function()) ->
          {ok, MatchDesc :: match_desc()} | {error, term()}.
ctp(Module, Function) ->
    do_ctp({Module, Function, '_'}, []).

-doc #{equiv => ctp({Module, Function, Arity})}.
-spec ctp(Module :: tp_module(), Function :: tp_function(), Arity :: tp_arity()) ->
          {ok, MatchDesc :: match_desc()} | {error, term()}.
ctp(Module, Function, Arity) ->
    do_ctp({Module, Function, Arity}, []).

-doc """
ctp(ModuleOrMFA)

Disables call tracing for one or more functions specified by `ModuleOrMFA`.

If `ModuleOrMFA` is an atom (a module name), this function call is
equivalent to `ctp({ModuleOrMFA, '_', '_'})`.

Otherwise, `ModuleOrMFA` should be `{Module, Function, Arity}`.

`ctp` stands for **c**lear **t**race **p**attern.

The semantics of `ModuleOrMFA` is the same as for the corresponding function
specification in `tp/2` or `tpl/2`. Both local and global call trace
is disabled.

The return value reflects how many functions that matched, and is constructed as
described in `tp/2`, except that no `{saved, N}` tuple is returned.
""".
-spec ctp(Module | {Module, Function, Arity}) ->
          {ok, MatchDesc :: match_desc()} | {error, term()} when
      Module :: tp_module(),
      Function :: tp_function(),
      Arity :: tp_arity().
ctp(Module) when is_atom(Module) ->
    do_ctp({Module, '_', '_'}, []);
ctp({_Module, _Function, _Arity} = X) ->
    do_ctp(X,[]).

-doc #{equiv => ctpl({'_', '_', '_'})}.
-spec ctpl() -> {ok, MatchDesc :: match_desc()} | {error, term()}.
ctpl() ->
    do_ctp({'_', '_', '_'}, [local]).


-doc #{equiv => ctpl({Module, Function, '_'})}.
-spec ctpl(Module :: tp_module(), Function :: tp_function()) ->
          {ok, MatchDesc :: match_desc()} | {error, term()}.
ctpl(Module, Function) ->
    do_ctp({Module, Function, '_'}, [local]).

-doc #{equiv => ctpl({Module, Function, Arity})}.
-spec ctpl(Module :: tp_module(), Function :: tp_function(), Arity :: tp_arity()) ->
          {ok, MatchDesc :: match_desc()} | {error, term()}.
ctpl(Module, Function, Arity) ->
    do_ctp({Module, Function, Arity}, [local]).

-doc """
ctpl(ModuleOrMFA)

Disables local call tracing for one or more functions specified by `ModuleOrMFA`.

If `ModuleOrMFA` is an atom (a module name), this function call is
equivalent to `ctpl({ModuleOrMFA, '_', '_'})`.

Otherwise, `ModuleOrMFA` should be `{Module, Function, Arity}`.

`ctpl` stands for **c**lear **t**race **p**attern **l**ocal.

This function works as `ctp/1`, but only disables tracing set up with
`tpl/2` (not with `tp/2`).
""".
-spec ctpl(Module | {Module, Function :: tp_function(), Arity :: tp_arity()}) ->
              {ok, MatchDesc :: term()} | {error, term()} when
      Module :: tp_module().
ctpl(Module) when is_atom(Module) ->
    do_ctp({Module, '_', '_'}, [local]);
ctpl({_Module, _Function, _Arity} = X) ->
    do_ctp(X,[local]).

-doc #{equiv => ctpg({'_', '_', '_'})}.
-spec ctpg() -> {ok, MatchDesc :: match_desc()} | {error, term()}.
ctpg() ->
    do_ctp({'_', '_', '_'}, [global]).

-doc #{equiv => ctpg({Module, Function, '_'})}.
-spec ctpg(Module :: tp_module(), Function :: tp_function()) ->
          {ok, MatchDesc :: match_desc()} | {error, term()}.
ctpg(Module, Function) ->
    do_ctp({Module, Function, '_'}, [global]).

-doc #{equiv => ctpg({Module, Function, Arity})}.
-spec ctpg(Module :: tp_module(), Function :: tp_function(), Arity :: tp_arity()) ->
          {ok, MatchDesc :: match_desc()} | {error, term()}.
ctpg(Module, Function, Arity) ->
    do_ctp({Module, Function, Arity}, [global]).

-doc """
ctpg(ModuleOrMFA)

Disables global call tracing for one or more functions specified by `ModuleOrMFA`.

If `ModuleOrMFA` is an atom (a module name), this function call is
equivalent to `ctpg({ModuleOrMFA, '_', '_'})`.

Otherwise, `ModuleOrMFA` should be `{Module, Function, Arity}`.

`ctpg` stands for **c**lear **t**race **p**attern **g**lobal.

This function works as `ctp/1`, but only disables tracing set up with
`tp/2` (not with `tpl/2`).
""".
-spec ctpg(Module | {Module, Function :: tp_function(), Arity :: tp_arity()}) ->
          {ok, MatchDesc :: term()} | {error, term()} when
      Module :: tp_module().
ctpg(Module) when is_atom(Module) ->
    do_ctp({Module, '_', '_'}, [global]);
ctpg({_Module, _Function, _Arity} = X) ->
    do_ctp(X,[global]).

do_ctp({Module, Function, Arity},[]) ->
    {ok,_} = do_ctp({Module, Function, Arity},[global]),
    do_ctp({Module, Function, Arity},[local]);
do_ctp({_Module, _Function, _Arity}=MFA,Flags) ->
    Nodes = req(get_nodes),
    {ok,do_tp_on_nodes(Nodes,MFA,false,Flags)}.

-doc """
Clears match specifications for the specified trace event (`send` or
`'receive'`), reverting to the default of tracing all triggered events.

`ctpe` stands for **c**lear **t**race **p**attern **e**vent.
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec ctpe(Event) -> {ok, MatchDesc} | {error, term()} when
      Event :: send | 'receive',
      MatchDesc :: [MatchNum],
      MatchNum ::
        {matched, node(), 1} |
        {matched, node(), 0, RPCError :: term()}.
ctpe(Event) when Event =:= send;
		 Event =:= 'receive' ->
    Nodes = req(get_nodes),
    {ok,do_tp_on_nodes(Nodes,Event,true,[])}.

%%
%% ltp() -> ok
%% List saved and built-in trace patterns.
%%
-doc """
Lists all match specifications previously used in the session.

`ltp` stands for **l**ist **t**race **p**atterns.

This function lists all match specifications previously saved during
calls to `tp/2` and `tpl/2`, as well as all built-in match
specifications. This avoids having to re-type complicated match
specifications. Note that the match specifications are lost if
`stop/0` is called.

Match specifications can be saved in a file (if a read-write file system is
present) for use in later debugging sessions; see `wtp/1` and `rtp/1`.

There are three built-in trace patterns:

- `exception_trace`, `x` - sets a trace which will show function
  names, parameters, return values, and exceptions raised from
  functions

- `caller_trace`, `c` - sets a trace that displays function names,
  parameters, and information about which function called it

- `caller_exception_trace`, `cx` - combines `exception_trace` and
  `caller_trace`

Here is an example that shows how to use a built-in match specification:

```erlang
1> dbg:tracer().
{ok,<0.90.0>}
2> dbg:tp(lists, seq, 2, cx).
{ok,[{matched,nonode@nohost,1},{saved,cx}]}
3> dbg:p(self(), call).
{ok,[{matched,nonode@nohost,1}]}
4> lists:seq(1, 5).
(<0.88.0>) call lists:seq(1,5) ({erl_eval,do_apply,7,{"erl_eval.erl",904}})
[1,2,3,4,5]
(<0.88.0>) returned from lists:seq/2 -> [1,2,3,4,5]
```
""".
-spec ltp() -> ok.
ltp() ->
    Modifier = modifier(),
    Format = "~p: ~"++Modifier++"p~n",
    pt_doforall(fun({X, El},_Ignore) ->
			io:format(Format, [X,El])
		end,[]).

%%
%% dtp() | dtp(N) -> ok
%% Delete saved pattern with number N or all saved patterns
%%
%% Do not delete built-in trace patterns.
-doc """
Forgets all match specifications saved during calls to `tp/2`.

`dtp` stands for **d**elete **t**race **p**atterns.

Removing all saved match specifications is useful before restoring
other match specifications from a file with `rtp/1`. Use `dtp/1` to
delete specific saved match specifications.
""".
-spec dtp() -> ok.
dtp() ->
    pt_doforall(fun ({Key, _}, _) when is_integer(Key) ->
			dtp(Key);
		    ({_, _}, _) ->
			ok
		end,
		[]).
-doc """
Forgets a specific match specification saved during calls to `tp/2`.

`dtp` stands for **d**elete **t**race **p**attern.
""".
-spec dtp(N) -> ok when N :: tp_id().
dtp(N) when is_integer(N) ->
    ets:delete(get_pattern_table(), N),
    ok;
dtp(_) ->
    ok.

%%
%% wtp(FileName) -> ok | {error, Reason}
%% Writes all current saved trace patterns to a file.
%%
%% Actually write the built-in trace patterns too.
-doc """
Saves all match specifications saved during the session (by calls to
`tp/2` or `tpl/2`), as well as built-in match specifications, in a text
file with the name designated by `Name`.

`wtp` stands for **w**rite **t**race **p**atterns.

The format of the file is textual, which means that it can be edited
with a text editor, and then restored with `rtp/1`.

Each match specification in the file ends with a period (`.`) and
new (syntactically correct) match specifications can be added to the
file manually.

The function returns `ok`, or an error tuple where the second element
indicates the reason that writing the file failed.
""".
-spec wtp(Name) -> ok | {error, IOError} when Name :: string(),
   IOError :: term().
wtp(FileName) ->
    case file:open(FileName,[write,{encoding,utf8}]) of
	{error, Reason} ->
	    {error, Reason};
	{ok, File} ->
            io:format(File, "%% ~s\n", [epp:encoding_to_string(utf8)]),
	    pt_doforall(fun ({_, Val}, _) when is_list(Val) ->
				io:format(File, "~tp.~n", [Val]);
			    ({_, _}, _) ->
				ok
			end,
			[]),
	    ok = file:close(File)
    end.

%%
%% rtp(FileName) -> ok | {error, Reason}
%% Reads in previously saved pattern file and merges the contents
%% with what's there now.
%%
%% So the saved built-in trace patterns will merge with
%% the already existing, which should be the same.
-doc """
Reads match specifications from a text file (possibly) generated by
the `wtp/1` function.

`rtp` stands for **r**ead **t**race **p**atterns.

The function verifies that the syntax of all match specifications are correct.
If any error in any match specification is found, none of the match specifications
are added to the list of saved match specifications for the running system.

The match specifications in the file are _merged_ with the current match
specifications, so that no duplicates are generated. Use `ltp/0` to see what
numbers were assigned to the specifications from the file.

The function will return an error tuple, either due to I/O problems
(like a non-existing or non-readable file) or due to file format
problems. In the latter case, `Reason` is in a more or less textual
format, giving a hint to what is causing the problem.
""".
-spec rtp(Name) -> ok | {error, Error} when Name :: string(),
   Error :: term().
rtp(FileName) ->
    T = get_pattern_table(),
    case file:consult(FileName) of
	{error, Reason1} ->
	    {error, {read_error, Reason1}};
	{ok, Data} ->
	    case check_list(Data) of
		ok ->
		    lists:foreach(fun(X) ->
					  save_pattern(X,T)
				  end, Data),
		    ok;
		{error, Reason2} ->
		    {error, {file_format_error, Reason2}}
	    end
    end.

-doc """
Starts a server on the local node that will be the recipient of
all trace messages.

All subsequent calls to `p/2` will result in messages sent
to the newly started trace server.

A trace server started in this way will simply display the formatted
trace messages the Erlang shell (that is, using `io:format/2`). See `tracer/2`
for a description of how the trace message handler can be customized.

To start a similar tracer on a remote node, use `n/1`.
""".
-spec tracer() -> {ok, pid()} | {error, already_started}.
tracer() ->
    tracer(process, {fun dhandler/2,user}).

-doc """
tracer(Type, Data)

Starts a tracer server with additional parameters on the local
node.

`Type` indicates how trace messages should be handled:

- `process` - by a receiving process
- `port` - by a port; see `trace_port/2`
- `module` - by a tracer module; see `m:erl_tracer`
- `file` - by printing them to a file

If `Type` is `process`, `Data` should be a message handler function
(`HandlerSpec`). The handler function, which should be a `fun` taking two
arguments, will be called for each trace message, with the first argument
containing the message as it is and the second argument containing the return
value from the last invocation of the fun. The initial value of the second
parameter is specified in the `InitialData` part of the `HandlerSpec`. The
`HandlerFun` can choose any appropriate action to take when invoked, and can
save a state for the next invocation by returning it.

If `Type` is `port`, then the second parameter should be a fun which takes no
arguments and returns a newly opened trace port when called. Such a fun is
preferably generated by calling `trace_port/2`.

If `Type` is `module`, `Data` should be either a tuple describing the
`m:erl_tracer` module to be used for tracing and the state to be used
for that tracer module, or a fun returning that kind of tuple.

if `Type` is `file`, `Data` should be a filename specifying a file
where all the traces are to be printed.

If an error is returned, it can either be because a tracer server is
already running (`{error,already_started}`), or because
`HandlerFun` raised an exception.

To start a similar tracer on a remote node, use `tracer/3`.
""".
-spec tracer(port, PortGenerator) -> {ok, pid()} | {error, Error :: term()} when
      PortGenerator :: fun(() -> port());
            (process, HandlerSpec) -> {ok, pid()} | {error, Error :: term()} when
      HandlerSpec :: {HandlerFun, InitialData :: term()},
      HandlerFun :: fun((Event :: term(), Data :: term()) -> NewData :: term());
            (module, ModuleSpec) -> {ok, pid()} | {error, Error :: term()} when
      ModuleSpec :: fun(() -> {TracerModule, TracerState}) | {TracerModule, TracerState},
      TracerModule :: atom(),
      TracerState :: term();
            (file, Filename) -> {ok, pid()} | {error, Error :: term()} when
      Filename :: file:name_all().
tracer(port, Fun) when is_function(Fun) ->
    start(Fun);

tracer(port, Port) when is_port(Port) ->
    start(fun() -> Port end);

tracer(process, {Handler,HandlerData}) ->
    start(fun() -> start_tracer_process(Handler, HandlerData) end);

tracer(module, Fun) when is_function(Fun) ->
    start(Fun);
tracer(module, {Module, State}) ->
    start(fun() -> {Module, State} end);

tracer(file, Filename) ->
    tracer(process,
           {fun F(E, undefined) ->
                    {ok, D} = file:open(Filename, [write]),
                    F(E, D);
                F(E, D) ->
                    dhandler(E, D),
                    D
            end, undefined}).

remote_tracer(port, Fun) when is_function(Fun) ->
    remote_start(Fun);

remote_tracer(port, Port) when is_port(Port) ->
    remote_start(fun() -> Port end);

remote_tracer(process, {Handler,HandlerData}) ->
    remote_start(fun() -> start_tracer_process(Handler, HandlerData) end);

remote_tracer(module, Fun) when is_function(Fun) ->
    remote_start(Fun);
remote_tracer(module, {Module, State}) ->
    remote_start(fun() -> {Module, State} end).


remote_start(StartTracer) ->
    case (catch StartTracer()) of
	{'EXIT', Reason} ->
	    {error, Reason};
	Tracer ->
	    {ok,Tracer}
    end.

%%
%% tracer(Node,Type,Data) -> {ok, Node} | {error, Reason}
%% Add Node to the list of traced nodes and a trace port defined by
%% Type and Data is started on Node.
%%
-doc """
This function is equivalent to `tracer/2`, but acts on the given node.

A tracer is started on the node (`Nodename`) and the node is added to
the list of traced nodes.

> #### Note {: .info }
>
> This function is not equivalent to `n/1`. While `n/1` starts a process tracer
> which redirects all trace information to a process tracer on the local node
> (that is, the trace control node), `tracer/3` starts any type of tracer,
> independent of the type of tracer on the trace control node.

For details, see `tracer/2`.
""".
-spec tracer(Nodename :: node(), Type :: term(), Data :: term()) ->
          {ok, Nodename :: node()} | {error, Reason :: term()}.
tracer(Node,Type,Data) when Node =:= node() ->
    case tracer(Type,Data) of
	{ok,_Dbg} -> {ok,Node};
	Error -> Error
    end;
tracer(Node,Type,Data) ->
    case (catch net_adm:ping(Node)) of
	{'EXIT',_} ->
	    {error, {bad_node, Node}};
	pang ->
	    {error, {nodedown, Node}};
	pong ->
	    req({add_node, Node, Type, Data});
	Other ->
	    {error, Other}
    end.

-doc #{equiv => flush_trace_port(node())}.
-spec flush_trace_port() -> term().
flush_trace_port() ->
    trace_port_control(flush).

-doc #{equiv => trace_port_control(Nodename, flush)}.
-spec flush_trace_port(Nodename :: node()) ->
          ok | {error, Reason :: term()}.
flush_trace_port(Node) ->
    trace_port_control(Node, flush).

-doc #{equiv => trace_port_control(node(), Operation)}.
-spec trace_port_control(Operation :: term()) -> term().
trace_port_control(Operation) ->
    trace_port_control(node(), Operation).

-doc """
This function is used to do a control operation on the active trace port driver
on the given node (`Nodename`).

Which operations are allowed as well as their return values depend on
which trace driver is used.

Returns either `ok` or `{ok, Result}` if the operation was successful, or
`{error, Reason}` if the current tracer is a process, or if it is a port not
supporting the operation.

The allowed values for `Operation` are:

- **`flush`** - This function is used to flush the internal buffers held by a
  trace port driver. Currently only the `file` trace driver supports this
  operation. Returns `ok`.

- **`get_listen_port`** - Returns `{ok, IpPort}` where `IpPort` is the IP port
  number used by the driver listen socket. Only the `ip` trace driver supports
  this operation.
""".
-spec trace_port_control(Nodename :: node(), Operation :: term()) ->
          ok | {ok, Result :: term()} | {error, Reason :: term()}.
trace_port_control(Node, flush) ->
    case get_tracer(Node) of
	{ok, Port} when is_port(Port) ->
	    case catch rpc:call(Node,?MODULE,deliver_and_flush,[get_trace_session(Node), Port]) of
		[0] ->
		    ok;
		_ ->
		    {error, not_supported_by_trace_driver}
	    end;
	_ ->
	    {error, no_trace_driver}
    end;
trace_port_control(Node,get_listen_port) ->
    case trace_port_control(Node,$p, "") of
	{ok, <<0, IpPort:16>>} ->
	    {ok, IpPort};
	{ok, _Other} ->
	    {error, not_supported_by_trace_driver};
	Other ->
	    Other
    end.

trace_port_control(Node, Command, Arg) ->
    case get_tracer(Node) of
	{ok, Port} when is_port(Port) ->
	    {ok, catch rpc:call(Node,erlang,port_control,[Port, Command, Arg])};
	_ ->
	    {error, no_trace_driver}
    end.

%% A bit more than just flush - it also makes sure all trace messages
%% are delivered first, before flushing the driver.
-doc false.
deliver_and_flush(Port) ->
    deliver_and_flush(get_trace_session(node(Port)), Port).
-doc false.
deliver_and_flush(Session, Port) ->
    Ref = trace:delivered(Session, all),
    receive
	{trace_delivered,all,Ref} -> ok
    end,
    erlang:port_control(Port, $f, "").

-doc """
trace_port(Type, Parameters)

Creates a trace-port-generating fun that is suitable as the
second argument to `tracer/2`.

_Example:_

```erlang
dbg:tracer(port, dbg:trace_port(ip, 4711)).
```

A trace port is an Erlang port to a dynamically linked-in driver that
handles trace messages directly, without the overhead of sending them
as messages to an Erlang process. Using a trace port significantly
lowers the overhead imposed by tracing.

Two trace drivers are currently implemented: the `file` and the `ip`
trace drivers.

The `file` driver sends all trace messages into one or
several binary files, from where they later can be fetched and
processed with the `trace_client/2` function.

The `ip` driver opens a TCP/IP port listening port. When a client
(preferably started by calling `trace_client/2` on another Erlang
node) connects, all trace messages are sent over the IP network for
further processing by the remote client.

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
`trace_client/3` as the tuple `{drop, N}` where `N` is the number of consecutive
messages dropped. In case of heavy tracing, drops are likely to occur, and they
surely occur if no client is reading the trace messages. The default value of
`QueSize` is 200.
""".
-spec trace_port(ip, IpPortSpec) -> fun(() -> port()) when
      IpPortSpec :: PortNumber | {PortNumber, QueSize},
      PortNumber :: integer(),
      QueSize :: integer();
                (file, Parameters) -> fun(() -> port()) when
      Parameters :: Filename | WrapFilesSpec,
      Filename :: file:name_all(),
      WrapFilesSpec :: trace_wrap_files_spec().
trace_port(file, {Filename, wrap, Tail}) ->
    trace_port(file, {Filename, wrap, Tail, 128*1024});
trace_port(file, {Filename, wrap, Tail, WrapSize}) ->
    trace_port(file, {Filename, wrap, Tail, WrapSize, 8});
trace_port(file, {Filename, wrap, Tail, WrapSize, WrapCnt})
  when is_list(Tail), 
       is_integer(WrapSize), WrapSize >= 0, WrapSize < (1 bsl 32),
       is_integer(WrapCnt), WrapCnt >= 1, WrapCnt < (1 bsl 32) ->
    trace_port1(file, Filename, {wrap, Tail, WrapSize, WrapCnt, 0});
trace_port(file, {Filename, wrap, Tail, {time, WrapTime}, WrapCnt})
  when is_list(Tail), 
       is_integer(WrapTime), WrapTime >= 1, WrapTime < (1 bsl 32),
       is_integer(WrapCnt), WrapCnt >= 1, WrapCnt < (1 bsl 32) ->
    trace_port1(file, Filename, {wrap, Tail, 0, WrapCnt, WrapTime});
trace_port(file, Filename) ->
    trace_port1(file, Filename, nowrap);

trace_port(ip, Portno) when is_integer(Portno) -> 
    trace_port(ip,{Portno,200});

trace_port(ip, {Portno, Qsiz}) when is_integer(Portno), is_integer(Qsiz) -> 
    fun() ->
	    Driver = "trace_ip_drv",
	    Dir1 = filename:join(code:priv_dir(runtime_tools), "lib"),
	    case catch erl_ddll:load_driver(Dir1, Driver) of
		ok ->
		    ok;
		_ ->
		    Dir2 = filename:join(
			     Dir1, 
			     erlang:system_info(system_architecture)),
		    catch erl_ddll:load_driver(Dir2, Driver)
	    end,
	    L = lists:flatten(
		  io_lib:format("~s ~p ~p 2",
				[Driver, Portno, Qsiz])),
	    open_port({spawn, L}, [eof])
    end.

trace_port1(file, Filename, Options) ->
    Driver = "trace_file_drv",
    fun() ->
	    Name = filename:absname(Filename), 
	    %% Absname is needed since the driver uses 
	    %% the supplied name without further investigations.

	    %% Also, the absname must be found inside the fun,
	    %% in case the actual node where the port shall be
	    %% started is on another node (or even another host)
	    {Wrap, Tail} =
		case Options of
		    {wrap, T, WrapSize, WrapCnt, WrapTime} ->
			{lists:flatten(
			   io_lib:format("w ~p ~p ~p ~p ", 
					 [WrapSize, WrapCnt, WrapTime, 
					  length(Name)])),
			 T};
		    nowrap ->
			{"", ""}
		end,
	    Command = Driver ++ " " ++ Wrap ++ "n " ++ Name ++ Tail,
	    Dir1 = filename:join(code:priv_dir(runtime_tools), "lib"),
	    case catch erl_ddll:load_driver(Dir1, Driver) of
		ok ->
		    ok;
		_ ->
		    Dir2 = filename:join(
			     Dir1, 
			     erlang:system_info(system_architecture)),
		    catch erl_ddll:load_driver(Dir2, Driver)
	    end,
	    if 	element(1, Options) == wrap ->
		    %% Delete all files from any previous wrap log
		    Files = wrap_postsort(wrap_presort(Name, Tail)),
		    lists:foreach(
		      fun(N) -> file:delete(N) end,
		      Files);
		true -> ok
	    end,
	    open_port({spawn, Command}, [eof])
    end.


-doc """
trace_client(Type, Parameters)

Starts a trace client that reads the output created by a trace port
driver (see `trace_port/2`) and handles it in mostly the same way as a
tracer process created by the `tracer/0` function.

If `Type` is `file`, the client reads all trace messages stored in the
file named `Filename` or specified by `WrapFilesSpec` (must be the
same as used when creating the trace) and lets the default handler
function format the messages on the console. This is one way to
interpret the data stored in a file by the file trace port driver.

If `Type` is `follow_file`, the client behaves as in the `file` case, but keeps
trying to read (and process) more data from the file until stopped by
`stop_trace_client/1`. `WrapFilesSpec` is not allowed as second argument for
this `Type`.

If `Type` is `ip`, the client connects to the TCP/IP port `PortNumber` on the
host `Hostname`, from where it reads trace messages until the TCP/IP connection
is closed. If no `Hostname` is specified, the local host is assumed.

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
""".
-spec trace_client(ip, IPClientPortSpec) -> pid() when
      IPClientPortSpec :: PortNumber | {Hostname, PortNumber},
      PortNumber :: integer(),
      Hostname :: string();
                  (Type, Parameters) -> pid() when
      Type :: file | follow_file,
      Parameters :: Filename | WrapFilesSpec,
      Filename :: file:name_all(),
      WrapFilesSpec :: trace_wrap_files_spec().
trace_client(file, Filename) ->
    trace_client(file, Filename, {fun dhandler/2,user});
trace_client(follow_file, Filename) ->
    trace_client(follow_file, Filename, {fun dhandler/2,user});
trace_client(ip, Portno) when is_integer(Portno) ->
    trace_client1(ip, {"localhost", Portno}, {fun dhandler/2,user});
trace_client(ip, {Host, Portno}) when is_integer(Portno) ->
    trace_client1(ip, {Host, Portno}, {fun dhandler/2,user}).

-type handler_spec() :: {HandlerFun :: fun((Event :: term(), Data :: term()) -> NewData :: term()),
                         InitialData :: term()}.

-doc """
trace_client(Type, Parameters, HandlerSpec)

This function works exactly as `trace_client/2`, but allows you to write your
own handler function.

The handler function works mostly as the one described in `tracer/2`,
but must also be prepared to handle trace messages of the form `{drop,
N}`, where `N` is the number of dropped messages. This pseudo trace
message will only occur if the `ip` trace driver is used.

For trace type `file`, the pseudo trace message `end_of_trace` will appear at
the end of the trace. The return value from the handler function is in this case
ignored.
""".
-spec trace_client(ip, IPClientPortSpec, HandlerSpec) -> pid() when
      IPClientPortSpec :: PortNumber | {Hostname, PortNumber},
      PortNumber :: integer(),
      Hostname :: string(),
      HandlerSpec :: handler_spec();
                     (Type, Parameters, HandlerSpec) -> pid() when
      Type :: file | follow_file,
      Parameters :: Filename | WrapFilesSpec,
      Filename :: string() | [string()] | atom(),
      WrapFilesSpec :: trace_wrap_files_spec(),
      HandlerSpec :: handler_spec().
trace_client(file, {Filename, wrap, Tail}, FD) ->
    trace_client(file, {Filename, wrap, Tail, 128*1024}, FD);
trace_client(file, {Filename, wrap, Tail, WrapSize}, FD) ->
    trace_client(file, {Filename, wrap, Tail, WrapSize, 8}, FD);
trace_client(file, 
	     {_Filename, wrap, Tail, _WrapSize, WrapCnt} = WrapSpec, 
	     {Fun, _Data} = FD)
  when is_list(Tail), is_function(Fun), is_integer(WrapCnt), WrapCnt >= 1 ->
    trace_client1(file, WrapSpec, FD);
trace_client(file, Filename, {Fun, Data} ) when is_function(Fun) ->
    trace_client1(file, Filename, {Fun, Data});
trace_client(follow_file, Filename, {Fun, Data} ) when is_function(Fun) ->
    trace_client1(follow_file, Filename, {Fun, Data});
trace_client(ip, Portno, {Fun, Data}) when is_integer(Portno), is_function(Fun) ->
    trace_client1(ip, {"localhost", Portno}, {Fun, Data});
trace_client(ip, {Host, Portno}, {Fun, Data}) when is_integer(Portno), 
						   is_function(Fun) ->
    trace_client1(ip, {Host, Portno}, {Fun, Data}).

trace_client1(Type, OpenData, {Handler,HData}) ->
    case req({link_to, 
	      spawn(
		fun() ->
			tc_loop(gen_reader(Type, OpenData), Handler, HData)
		end)}) of
	{ok, Pid} ->
	    Pid;
	Other ->
	    Other
    end.

-doc """
Shuts down a previously started trace client.

The `Pid` argument is the process id returned from the
`trace_client/2` or `trace_client/3` call.
""".
-spec stop_trace_client(Pid) -> ok when Pid :: pid().
stop_trace_client(Pid) when is_pid(Pid) ->
    process_flag(trap_exit,true),
    link(Pid),
    exit(to_pidspec(Pid),abnormal),
    Res = receive 
	      {'EXIT', Pid, _} ->
		  ok
	  after 5000 ->
		  {error, timeout}
	  end,
    process_flag(trap_exit,false),
    Res.

-doc #{equiv => p(Item, [m])}.
-spec p(Item :: term()) -> {ok, MatchDesc :: term()} | {error, term()}.
p(Pid) ->
    p(Pid, [m]).

-doc """
Traces `Item` in accordance to the value specified by `Flags`.

`p` stands for **p**rocess.

The following kind of values are allowed for `Item`:

- **`t:pid/0` or `t:port/0`** - The corresponding process or port is traced. The
  process or port can be a remote process or port (on another Erlang node). The
  node must be in the list of traced nodes (see `n/1` and `tracer/3`).

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
  The node must be in the list of traced nodes (see `n/1` and `tracer/3`).

- **`t:integer/0`** - The process `<0.Item.0>` is traced.

- **`{X, Y, Z}`** - The process `<X.Y.Z>` is traced.

- **`t:string/0`** - If the `Item` is a string "<X.Y.Z>" as returned from
  [`pid_to_list/1`](`erlang:pid_to_list/1`), the process `<X.Y.Z>` is traced.

When enabling an `Item` that represents a group of processes, the `Item` is
enabled on all nodes added with the `n/1` or `tracer/3` function.

`Flags` can be a single atom or a list of flags. The available flags are:

- **`s (send)`** - Traces the messages the process or port sends.

- **`r (receive)`** - Traces the messages the process or port receives.

- **`m (messages)`** - Traces the messages the process or port receives and
  sends.

- **`c (call)`** - Traces global function calls for the process according to the
  trace patterns set in the system (see `tp/2`).

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

The list can also include any of the flags allowed in `trace:process/4` and
`trace:port/4`.

This function returns either an error tuple or an `{ok, List}` tuple. The `List`
consists of specifications of how many processes and ports that matched (in the
case of a single pid exactly 1). The specification of matched processes is
`{matched, Node, N}`. If the remote processor call (using `m:rpc`) to a remote node
fails, the `rpc` error message is returned as the fourth element in the tuple
and the number of matched processes is 0.
""".
-spec p(Item :: term(), Flags :: term()) ->
           {ok, MatchDesc} | {error, term()}
           when
               MatchDesc :: [MatchNum],
               MatchNum ::
                   {matched, node(), integer()} |
                   {matched, node(), 0, RPCError},
               RPCError :: term().
p(Pid, Flags) when is_atom(Flags) ->
    p(Pid, [Flags]);
p(Pid, Flags) ->
    req({p,Pid,Flags}).

-doc """
Displays information about all traced processes and ports.

`i` stands for **i**nformation.
""".
-spec i() -> ok.
i() -> req(i).

-doc #{equiv => c(Mod, Fun, Args, all)}.
-spec c(Mod :: module(), Fun :: atom(), Args :: list(term())) -> term().
c(M, F, A) ->
    c(M, F, A, all).
-doc """
Evaluates the expression [`apply(Mod, Fun, Args)`](`apply/3`) with the
trace flags in `Flags` set.

`c` stands for **c**all.

This is a convenient way to trace processes from the Erlang shell.

""".
-spec c(Mod :: module(), Fun :: atom(), Args :: list(term()), Flags :: term()) ->
          term().
c(M, F, A, Flags) when is_atom(Flags) ->
    c(M, F, A, [Flags]);
c(M, F, A, Flags) ->
    case transform_flags(Flags) of
	{error,Reason} -> {error,Reason};
	Flags1 ->
            Session = session_create(dbg_temp_session),
            _ = session(Session, fun tracer/0),
            S = self(),
            {Pid, Mref} = spawn_monitor(fun() -> c(Session, S, M, F, A, Flags1) end),
            receive
                {'DOWN', Mref, _, _, Reason} ->
                    session_destroy(Session),
                    {error, Reason};
                {Pid, Res} ->
                    erlang:demonitor(Mref, [flush]),
                    session_destroy(Session),
                    %% 'sleep' prevents the tracer (recv_all_traces) from
                    %% receiving garbage {'EXIT',...} when dbg i stopped.
                    timer:sleep(1),
                    Res
            end
    end.

c(Session, Parent, M, F, A, Flags) ->
    TraceSession = session(Session, fun() -> get_trace_session(node()) end),
    %% The trace BIF is used directly here instead of the existing function
    %% p/2. The reason is that p/2 (when stopping trace) sends messages which 
    %% we don't want to show up in this simple tracing from the shell.
    trace:process(TraceSession, self(), true, Flags),
    Res = apply(M, F, A),
    trace:process(TraceSession, self(), false, [all]),
    Parent ! {self(), Res}.

-doc """
Stops the `dbg` server, clears all trace flags for all processes, clears all
trace patterns for all functions, clears trace patterns for send/receive, shuts
down all trace clients, and closes all trace ports.
""".
-spec stop() -> ok.
stop() ->

    TraceProc =
        case get_dbg_session() of
            undefined -> dbg;
            Session -> Session
        end,

    session_destroy(TraceProc).

-doc """
Destroys a dbg `t:session/0`.

This will terminate all started processes and destroy the `t:trace:session/0`.
""".
-doc(#{ since => ~"OTP 27.0" }).
-spec session_destroy(Session :: session()) -> ok.
session_destroy(TraceProc) ->
    Mref = erlang:monitor(process, TraceProc),
    catch TraceProc ! {self(),stop},

    receive
        {'DOWN',Mref,_,_,_} -> ok
    end.



%% This is a vestigial function that used to be documented as a variant of
%% `stop/0` that also clears global function traces. Since `stop/0` now clears
%% all tracing as the user would expect it to, we've removed this from the
%% documentation but keep it around for backwards compatibility, much like
%% `queue:lait`.
-doc false.
stop_clear() ->
    stop().

%%% Calling the server.

req(R) ->
    P = ensure(), % The pid or perhaps the name of the server
    Mref = erlang:monitor(process, P),
    catch P ! {self(), R}, % May crash if P = atom() and server died
    receive
	{'DOWN', Mref, _, _, _} -> % If server died
	    exit(dbg_server_crash);
	{dbg, Reply} ->
	    erlang:demonitor(Mref, [flush]),
	    Reply
    end.

%% Returns the pid of the dbg server.
%% Starts a new server if necessary.
ensure() ->
    case get_dbg_session() of
        undefined ->
            case whereis(dbg) of
                undefined -> 
                    case start() of
                        {ok, P} ->
                            P;
                        {error, already_started} ->
                            whereis(dbg)
                    end;
                Pid -> 
                    Pid
            end;
        Pid ->
            Pid
    end.

%%% Server implementation.
-doc false.
start() ->
    start(no_tracer).

start(TracerFun) ->
    S = self(),
    case whereis(dbg) of
	undefined ->
	    Dbg = spawn(fun() ->
                                register(dbg, self()),
                                init(dbg, S)
                        end),
	    receive {Dbg,started} -> ok end,
	    case TracerFun of
		no_tracer ->
		    {ok, Dbg};
		Fun when is_function(Fun) ->
		    req({tracer,TracerFun})
	    end;
	Pid when is_pid(Pid), is_function(TracerFun) ->
	    req({tracer,TracerFun})
    end.

init(Name, Parent) ->
    process_flag(trap_exit, true),
    Parent ! {self(),started},
    loop({[],[]},[],Parent,Name).

%
% SurviveLinks = Processes we should take with us while falling, 
%                but not get killed by if they die (i. e. trace clients 
%                and relay processes on other nodes)
%                SurviveLinks = {TraceClients,Relays}
%
loop({C,T}=SurviveLinks, Table, Parent, SessionName) ->
    receive
	{From,i} ->
            Modifier = modifier(),
            Reply = display_info(lists:map(fun({N,_}) -> N end,get()), Modifier),
	    reply(From, Reply),
	    loop(SurviveLinks, Table, Parent, SessionName);
	{From,{p,Pid,Flags}} ->
	    reply(From, trace_process(Pid, Flags)),
	    loop(SurviveLinks, Table, Parent, SessionName);
	{From,{tracer,TracerFun}} when is_function(TracerFun) ->
	    case get(node()) of
		undefined ->
		    case (catch TracerFun()) of
			{'EXIT', Reason} ->
			    reply(From, {error, Reason});
			Tracer when is_pid(Tracer); is_port(Tracer) ->
			    put(node(),
                                {self(), Tracer,
                                 trace:session_create(SessionName, Tracer, [])}),
			    reply(From, {ok,self()});
                        {Module, _State} = Tracer when is_atom(Module) ->
                            put(node(),
                                {self(), Tracer,
                                 trace:session_create(SessionName, Tracer, [])}),
			    reply(From, {ok,self()})
		    end;
		{_Relay,_Tracer, _Session} ->
		    reply(From, {error, already_started})
	    end,
	    loop(SurviveLinks,Table, Parent, SessionName);
	{From,{get_tracer,Node}} ->
	    case get(Node) of
		undefined -> reply(From,{error, {no_tracer_on_node,Node}});
		{_Relay,Tracer,_Session} -> reply(From, {ok,Tracer})
	    end,
	    loop(SurviveLinks, Table, Parent, SessionName);
	{From, get_table} ->
	    Tab = case Table of
		      [] ->
			  new_pattern_table();
		      _exists ->
			  Table
		  end,
	    reply(From, {ok, Tab}),
	    loop(SurviveLinks, Tab, Parent, SessionName);
	{_From,stop} ->
	    %% We want to make sure that all trace messages have been delivered
	    %% on all nodes that might be traced. Since dbg:cn/1 does not turn off
	    %% tracing on the node it removes from the list of active trace nodes,
	    %% we will call trace:delivered/2 on ALL nodes that we have
	    %% connections to.
	    %% If it is a file trace driver, we will also flush the port.
	    lists:foreach(fun({Node,{_Relay,Port,Session}}) ->
				  rpc:call(Node,?MODULE,deliver_and_flush,[Session,Port])
			  end,
			  get()),
	    exit(done);
	{From, {link_to, Pid}} ->
	    case (catch link(Pid)) of
		{'EXIT', Reason} ->
		    reply(From, {error, Reason}),
		    loop(SurviveLinks, Table, Parent, SessionName);
		_ ->
		    reply(From, {ok, Pid}),
		    loop({[Pid|C],T}, Table, Parent, SessionName)
	    end;
	{From, {add_node, Node}} ->
	    case get(node()) of
		undefined -> 
		    reply(From, {error, no_local_tracer}),
		    loop(SurviveLinks, Table, Parent, SessionName);
		{_LocalRelay,Tracer, _Session} when is_port(Tracer) -> 
		    reply(From, {error, cant_trace_remote_pid_to_local_port}),
		    loop(SurviveLinks, Table, Parent, SessionName);
		{_LocalRelay,Tracer, _Session} when is_tuple(Tracer) -> 
		    reply(From, {error, cant_trace_remote_pid_to_local_module}),
		    loop(SurviveLinks, Table, Parent, SessionName);
	        {_LocalRelay,Tracer, _Session} when is_pid(Tracer) ->
		    case (catch relay(SessionName, Node, Tracer)) of
			{ok,Relay} ->
			    reply(From, {ok, Node}),
			    loop({C,[Relay|T]}, Table, Parent, SessionName);
			{'EXIT', Something} ->
			    reply(From, {error, Something}),
			    loop(SurviveLinks, Table, Parent, SessionName);
			Error ->
			    reply(From, Error),
			    loop(SurviveLinks, Table, Parent, SessionName)
		    end
	    end;
	{From, {add_node, Node, Type, Data}} ->
	    case (catch relay(SessionName, Node, {Type,Data})) of
		{ok,Relay} ->
		    reply(From, {ok, Node}),
		    loop({C,[Relay|T]}, Table, Parent, SessionName);
		{'EXIT', Something} ->
		    reply(From, {error, Something}),
		    loop(SurviveLinks, Table, Parent, SessionName);
		Error ->
		    reply(From, Error),
		    loop(SurviveLinks, Table, Parent, SessionName)
	    end;
	{From, {remove_node, Node}} ->
	    erase(Node),
	    reply(From, ok),
	    loop(SurviveLinks, Table, Parent, SessionName);
	{From, get_nodes} ->
	    reply(From, lists:map(fun({N,_}) -> N end, get())),
	    loop(SurviveLinks, Table, Parent, SessionName);
        {From, {get_session, Node}} ->
            case get(Node) of
                undefined -> reply(From,{error, {no_tracer_on_node,Node}});
		{_Relay,_Tracer,Session} -> reply(From, Session)
	    end,
            loop(SurviveLinks, Table, Parent, SessionName);
        {'DOWN', _Ref, process, Parent, _} ->
            case get(node()) of
                undefined -> ok;
                {_Relay, _Tracer, Session} ->
                    session_destroy(Session),
                    exit(shutdown)
            end;
	{'EXIT', Pid, Reason} ->
	    case lists:delete(Pid, C) of
		C ->
		    case lists:delete(Pid,T) of
			T ->
                            Modifier = modifier(user),
			    io:format(user,
                                      "** dbg got EXIT - terminating: ~"++
                                          Modifier++"p~n",
				      [Reason]),
			    exit(done);
			NewT -> 
			    erase(node(Pid)),
			    loop({C,NewT}, Table, Parent, SessionName)
		    end;
		NewC ->
		    loop({NewC,T}, Table, Parent, SessionName)
	    end;
	Other ->
            Modifier = modifier(user),
	    io:format(user,"** dbg got garbage: ~"++Modifier++"p~n",
		      [{Other,SurviveLinks,Table}]),
	    loop(SurviveLinks, Table, Parent, SessionName)
    end.

reply(Pid, Reply) ->
    Pid ! {dbg,Reply},
    ok.


%%% A process-based tracer.

start_tracer_process(Handler, HandlerData) ->
    spawn_opt(fun() -> tracer_init(Handler, HandlerData) end,
	      [link,{priority,max}]).
    

tracer_init(Handler, HandlerData) ->
    process_flag(trap_exit, true),
    tracer_loop(Handler, HandlerData).

tracer_loop(Handler, Hdata) ->
    {State, Suspended, Traces} =  recv_all_traces(),
    NewHdata = handle_traces(Suspended, Traces, Handler, Hdata),
    case State of
	done ->
	    exit(normal);
	loop ->
	    tracer_loop(Handler, NewHdata)
    end.

recv_all_traces() ->
    recv_all_traces([], [], infinity).

recv_all_traces(Suspended0, Traces, Timeout) ->
    receive
	Trace when is_tuple(Trace), element(1, Trace) == trace ->
	    Suspended = suspend(Trace, Suspended0),
	    recv_all_traces(Suspended, [Trace|Traces], 0);
	Trace when is_tuple(Trace), element(1, Trace) == trace_ts ->
	    Suspended = suspend(Trace, Suspended0),
	    recv_all_traces(Suspended, [Trace|Traces], 0);
	Trace when is_tuple(Trace), element(1, Trace) == seq_trace ->
	    Suspended = suspend(Trace, Suspended0),
	    recv_all_traces(Suspended, [Trace|Traces], 0);
	Trace when is_tuple(Trace), element(1, Trace) == drop ->
	    Suspended = suspend(Trace, Suspended0),
	    recv_all_traces(Suspended, [Trace|Traces], 0);
	{'EXIT', _Pid, _Reason} ->
	    {done, Suspended0, Traces};
	Other ->
	    %%% Is this really a good idea?
            Modifier = modifier(user),
	    io:format(user,"** tracer received garbage: ~"++Modifier++"p~n",
                      [Other]),
	    recv_all_traces(Suspended0, Traces, Timeout)
    after Timeout ->
	    {loop, Suspended0, Traces}
    end.

handle_traces(Suspended, Traces, Handler, Hdata) ->
    case catch invoke_handler(Traces, Handler, Hdata) of
	{'EXIT',Reason} -> 
	    resume(Suspended),
	    exit({trace_handler_crashed,Reason});
	NewHdata ->
	    resume(Suspended),
	    NewHdata
    end.

invoke_handler([Tr|Traces], Handler, Hdata0) ->
    Hdata = invoke_handler(Traces, Handler, Hdata0),
    Handler(Tr, Hdata);
invoke_handler([], _Handler, Hdata) ->
    Hdata.

suspend({trace,From,call,_Func}, Suspended) when node(From) == node() ->
    case (catch erlang:suspend_process(From, [unless_suspending,
					      asynchronous])) of
	true ->
	    [From | Suspended];
	_ ->
	    Suspended
    end;
suspend(_Other, Suspended) -> Suspended.

resume([Pid|Pids]) when node(Pid) == node() ->
    (catch erlang:resume_process(Pid)),
    resume(Pids);
resume([]) -> ok.



%%% Utilities.

trac(Proc, How, Flags) when is_atom(Proc) ->
    %% Proc = all | new | existing | RegisteredName
    %% Must go to all nodes
    case get() of
	[] ->
	    {error,no_tracers};
	Nodes ->
	    Matched = [trac(Node, NodeInfo, Proc, How, Flags)
		       || {Node, NodeInfo} <- Nodes],
	    {ok,Matched}
    end;
trac(Proc, How, Flags) ->
    %% Proc = Pid | Integer | {X,Y,Z} | "<X.Y.Z>"
    %% One node only
    Pid = to_pid(Proc),
    case Pid of
	{badpid,_} ->
	    {error,Pid};
	_ ->
	    Node = if is_pid(Pid) -> node(Pid); true -> node() end,
	    case get(Node) of
		undefined ->
		    {error,{no_tracer_on_node,Node}};
		NodeInfo ->
		    Match = trac(Node, NodeInfo, Pid, How, Flags),
		    {ok,[Match]}
	    end
    end.

trac(Node, {_Replay, _Tracer, Session}, AtomPid, How, Flags) ->
    case rpc:call(Node, ?MODULE, erlang_trace,
		  [Session, AtomPid, How, Flags]) of
	N when is_integer(N) ->
	    {matched, Node, N};
	{badrpc,Reason} ->
	    {matched, Node, 0, Reason};
	Else ->
	    {matched, Node, 0, Else}
    end.

-doc false.
erlang_trace(Session, AtomPid, How, Flags) ->
    case to_pidspec(AtomPid) of
	{badpid,_} ->
	    {no_proc,AtomPid};
	P when is_pid(P); P =:= processes; P =:= new_processes; P =:= existing_processes ->
            trace:process(Session, P, How, Flags);
        P when is_port(P); P =:= ports; P =:= new_ports; P =:= existing_ports ->
            trace:port(Session, P, How, Flags);
        P ->
            trace:process(Session, P, How, Flags) +
            trace:port(Session, P, How, Flags)
    end.

%% Since we are not allowed to do trace:process/4 on a remote
%% process, we create a relay process at the remote node.

relay(SessionName, Node,To) when Node /= node() ->
    case get(Node) of
	undefined ->
	    S = self(),
	    Pid = spawn_link(Node, dbg, do_relay, [SessionName, S, To]),
	    receive {started,Remote,Session} -> put(Node, {Pid,Remote,Session}) end,
	    {ok,Pid};
	{_Relay,PortOrPid,_Session} ->
	    {error, {already_started, PortOrPid}}
    end.

-doc false.
do_relay(SessionName, Parent,RelP) ->
    process_flag(trap_exit, true),
    Tracer =
        case RelP of
            {Type, Data} ->
                {ok, Started} = remote_tracer(Type,Data),
                Started;
            Pid when is_pid(Pid) ->
                self()
        end,
    Session = trace:session_create(SessionName, Tracer, []),
    Parent ! {started, Tracer, Session},
    do_relay_1(RelP, Session).

do_relay_1(RelP, Session) ->
    %% This process exists to relay messages and keep trace session
    %% alive - session is ref-counted locally, meaning just the main
    %% node keeping a reference to the session won't prevent the session
    %% from being garbage collected.
    receive
	{'EXIT', _P, _} ->
	    exit(normal);
	TraceInfo when is_pid(RelP) ->  % Here is the normal case for trace i/o
	    RelP ! TraceInfo,
	    do_relay_1(RelP, Session);
	Other ->
            Modifier = modifier(user),
	    io:format(user,"** relay got garbage: ~"++Modifier++"p~n", [Other]),
	    do_relay_1(RelP, Session)
    end.

-doc false.
dhandler(end_of_trace, Out) ->
    Out;
dhandler(Trace, Out) when element(1, Trace) == trace, tuple_size(Trace) >= 3 ->
    dhandler1(Trace, tuple_size(Trace), out(Out));
dhandler(Trace, Out) when element(1, Trace) == trace_ts, tuple_size(Trace) >= 4 ->
    dhandler1(Trace, tuple_size(Trace)-1, element(tuple_size(Trace),Trace)
             , out(Out));
dhandler(Trace, Out) when element(1, Trace) == drop, tuple_size(Trace) =:= 2 ->
    {Device,Modifier} = out(Out),
    io:format(Device, "*** Dropped ~p messages.~n", [element(2,Trace)]),
    {Device,Modifier};
dhandler(Trace, Out) when element(1, Trace) == seq_trace,
                          tuple_size(Trace) >= 3 ->
    {Device,Modifier} = out(Out),
    SeqTraceInfo = case Trace of
		       {seq_trace, Lbl, STI, TS} ->
			   io:format(Device, "SeqTrace ~p [~p]: ",
				     [TS, Lbl]),
			   STI;
		       {seq_trace, Lbl, STI} ->
			  io:format(Device, "SeqTrace [~p]: ",
				     [Lbl]),
			   STI 
		   end,
    case SeqTraceInfo of
	{send, Ser, Fr, To, Mes} ->
	    io:format(Device, "(~p) ~p ! ~"++Modifier++"p [Serial: ~p]~n",
		      [Fr, To, Mes, Ser]);
	{'receive', Ser, Fr, To, Mes} ->
	    io:format(Device, "(~p) << ~"++Modifier++"p [Serial: ~p, From: ~p]~n",
		      [To, Mes, Ser, Fr]);
	{print, Ser, Fr, _, Info} ->
	    io:format(Device, "-> ~"++Modifier++"p [Serial: ~p, From: ~p]~n",
		      [Info, Ser, Fr]);
	Else ->
	    io:format(Device, "~"++Modifier++"p~n", [Else])
    end,
    {Device,Modifier};
dhandler(_Trace, Out) ->
    Out.

dhandler1(Trace, Size, {Device,Modifier}) ->
    From = element(2, Trace),
    case element(3, Trace) of
	'receive' ->
	    case element(4, Trace) of
		{dbg,ok} -> ok;
		Message ->
		    io:format(Device, "(~p) << ~"++Modifier++"p~n",
                              [From,Message])
	    end;
	'send' ->
	    Message = element(4, Trace),
	    To = element(5, Trace),
	    io:format(Device, "(~p) ~p ! ~"++Modifier++"p~n", [From,To,Message]);
	call ->
	    case element(4, Trace) of
		MFA when Size == 5 ->
		    Message = element(5, Trace),
		    io:format(Device,
                              "(~p) call ~"++Modifier++"s (~"++Modifier++"p)~n",
                              [From,ffunc(MFA,Modifier),Message]);
		MFA ->
		    io:format(Device, "(~p) call ~"++Modifier++"s~n",
                              [From,ffunc(MFA,Modifier)])
	    end;
	return -> %% To be deleted...
	    case element(4, Trace) of
		MFA when Size == 5 ->
		    Ret = element(5, Trace),
		    io:format(Device,
                              "(~p) old_ret ~"++Modifier++"s -> ~"++Modifier++
                                  "p~n",
                              [From,ffunc(MFA,Modifier),Ret]);
		MFA ->
		    io:format(Device, "(~p) old_ret ~"++Modifier++"s~n",
                              [From,ffunc(MFA,Modifier)])
	    end;
	return_from ->
	    MFA = element(4, Trace),
	    Ret = element(5, Trace),
	    io:format(Device,
                      "(~p) returned from ~"++Modifier++"s -> ~"++Modifier++"p~n",
                      [From,ffunc(MFA,Modifier),Ret]);
	return_to ->
	    MFA = element(4, Trace),
	    io:format(Device, "(~p) returning to ~"++Modifier++"s~n",
                      [From,ffunc(MFA,Modifier)]);
	spawn when Size == 5 ->
	    Pid = element(4, Trace),
	    MFA = element(5, Trace),
	    io:format(Device, "(~p) spawn ~p as ~"++Modifier++"s~n",
                      [From,Pid,ffunc(MFA,Modifier)]);
	Op ->
	    io:format(Device, "(~p) ~p ~"++Modifier++"s~n",
                      [From,Op,ftup(Trace,4,Size,Modifier)])
    end,
    {Device,Modifier}.

dhandler1(Trace, Size, TS, {Device,Modifier}) ->
    From = element(2, Trace),
    case element(3, Trace) of
	'receive' ->
	    case element(4, Trace) of
		{dbg,ok} -> ok;
		Message ->
		    io:format(Device,
                              "(~p) << ~"++Modifier++"p (Timestamp: ~p)~n",
                              [From,Message,TS])
	    end;
	'send' ->
	    Message = element(4, Trace),
	    To = element(5, Trace),
	    io:format(Device, "(~p) ~p ! ~"++Modifier++"p (Timestamp: ~p)~n",
                      [From,To,Message,TS]);
	call ->
	    case element(4, Trace) of
		MFA when Size == 5 ->
		    Message = element(5, Trace),
		    io:format(Device,
                              "(~p) call ~"++Modifier++"s (~"++Modifier++
                                  "p) (Timestamp: ~p)~n",
                              [From,ffunc(MFA,Modifier),Message,TS]);
		MFA ->
		    io:format(Device,
                              "(~p) call ~"++Modifier++"s (Timestamp: ~p)~n",
                              [From,ffunc(MFA,Modifier),TS])
	    end;
	return -> %% To be deleted...
	    case element(4, Trace) of
		MFA when Size == 5 ->
		    Ret = element(5, Trace),
		    io:format(Device,
                              "(~p) old_ret ~"++Modifier++"s -> ~"++Modifier++
                                  "p (Timestamp: ~p)~n",
                              [From,ffunc(MFA,Modifier),Ret,TS]);
		MFA ->
		    io:format(Device,
                              "(~p) old_ret ~"++Modifier++"s (Timestamp: ~p)~n",
                              [From,ffunc(MFA,Modifier),TS])
	    end;
	return_from ->
	    MFA = element(4, Trace),
	    Ret = element(5, Trace),
	    io:format(Device,
                      "(~p) returned from ~"++Modifier++"s -> ~"++Modifier++
                          "p (Timestamp: ~p)~n",
                      [From,ffunc(MFA,Modifier),Ret,TS]);
	return_to ->
	    MFA = element(4, Trace),
	    io:format(Device,
                      "(~p) returning to ~"++Modifier++"s (Timestamp: ~p)~n",
                      [From,ffunc(MFA,Modifier),TS]);
	spawn when Size == 5 ->
	    Pid = element(4, Trace),
	    MFA = element(5, Trace),
	    io:format(Device,
                      "(~p) spawn ~p as ~"++Modifier++"s (Timestamp: ~p)~n",
                      [From,Pid,ffunc(MFA,Modifier),TS]);
	Op ->
	    io:format(Device, "(~p) ~p ~"++Modifier++"s (Timestamp: ~p)~n",
                      [From,Op,ftup(Trace,4,Size,Modifier),TS])
    end,
    {Device,Modifier}.

%%% These f* functions returns non-flat strings

%% {M,F,[A1, A2, ..., AN]} -> "M:F(A1, A2, ..., AN)"
%% {M,F,A}                 -> "M:F/A"
ffunc({M,F,Argl},Modifier) when is_list(Argl) ->
    io_lib:format("~p:~"++Modifier++"p(~"++Modifier++"s)",
                  [M, F, fargs(Argl,Modifier)]);
ffunc({M,F,Arity},Modifier) ->
    io_lib:format("~p:~"++Modifier++"p/~p", [M,F,Arity]);
ffunc(X,Modifier) -> io_lib:format("~"++Modifier++"p", [X]).

%% Integer           -> "Integer"
%% [A1, A2, ..., AN] -> "A1, A2, ..., AN"
fargs(Arity,_) when is_integer(Arity) -> integer_to_list(Arity);
fargs([],_) -> [];
fargs([A],Modifier) ->
    io_lib:format("~"++Modifier++"p", [A]);  %% last arg
fargs([A|Args],Modifier) ->
    [io_lib:format("~"++Modifier++"p,", [A]) | fargs(Args,Modifier)];
fargs(A,Modifier) ->
    io_lib:format("~"++Modifier++"p", [A]). % last or only arg

%% {A_1, A_2, ..., A_N} -> "A_Index A_Index+1 ... A_Size"
ftup(Trace, Index, Index, Modifier) ->
    io_lib:format("~"++Modifier++"p", [element(Index, Trace)]);
ftup(Trace, Index, Size, Modifier) ->
    [io_lib:format("~"++Modifier++"p ", [element(Index, Trace)])
     | ftup(Trace, Index+1, Size, Modifier)].

out({_,_}=Out) ->
    Out;
out(Device) ->
    {Device,modifier(Device)}.

modifier() ->
    modifier(group_leader()).
modifier(Device) ->
    Encoding =
        case io:getopts(Device) of
            List when is_list(List) ->
                proplists:get_value(encoding,List,latin1);
            _ ->
                latin1
        end,
    encoding_to_modifier(Encoding).

encoding_to_modifier(latin1) -> "";
encoding_to_modifier(_) -> "t".

trace_process(Pid, [clear]) ->
    trac(Pid, false, all());
trace_process(Pid, Flags0) ->
    case transform_flags(Flags0) of
	{error,Reason} -> {error,Reason};
	Flags -> trac(Pid, true, Flags)
    end.

-doc false.
transform_flags(Flags0) ->
    transform_flags(Flags0,[]).
transform_flags([],Acc) -> Acc;
transform_flags([m|Tail],Acc) -> transform_flags(Tail,[send,'receive'|Acc]);
transform_flags([s|Tail],Acc) -> transform_flags(Tail,[send|Acc]);
transform_flags([r|Tail],Acc) -> transform_flags(Tail,['receive'|Acc]);
transform_flags([c|Tail],Acc) -> transform_flags(Tail,[call|Acc]);
transform_flags([call|Tail],Acc) -> transform_flags(Tail,[call|Acc]);
transform_flags([p|Tail],Acc) -> transform_flags(Tail,[procs|Acc]);
transform_flags([sos|Tail],Acc) -> transform_flags(Tail,[set_on_spawn|Acc]);
transform_flags([sol|Tail],Acc) -> transform_flags(Tail,[set_on_link|Acc]);
transform_flags([sofs|Tail],Acc) -> transform_flags(Tail,[set_on_first_spawn|Acc]);
transform_flags([sofl|Tail],Acc) -> transform_flags(Tail,[set_on_first_link|Acc]);
transform_flags([all|_],_Acc) -> all()--[silent,running];
transform_flags([F|Tail]=List,Acc) when is_atom(F) ->
    case lists:member(F, all()) of
	true -> transform_flags(Tail,[F|Acc]);
	false -> {error,{bad_flags,List}}
    end;
transform_flags(Bad,_Acc) -> {error,{bad_flags,Bad}}.

all() ->
    [send,'receive',call,procs,ports,garbage_collection,running,
     set_on_spawn,set_on_first_spawn,set_on_link,set_on_first_link,
     timestamp,monotonic_timestamp,strict_monotonic_timestamp,
     arity,return_to,silent,running_procs,running_ports,exiting].

display_info([Node|Nodes],Modifier) ->
    io:format("~nNode ~w:~n",[Node]),
    io:format("~-12s ~-21s Trace ~n", ["Pid", "Initial call"]),
    List = rpc:call(Node,?MODULE,get_info,[]),
    display_info1(List,Modifier),
    display_info(Nodes,Modifier);
display_info([],_) ->
    ok.

display_info1([{Pid,Call,Flags}|T],Modifier) ->
    io:format("~-12s ~-21"++Modifier++"s ~s~n",
	      [io_lib:format("~w",[Pid]),
	       io_lib:format("~"++Modifier++"p", [Call]),
	       format_trace(Flags)]),
    display_info1(T,Modifier);
display_info1([],_) ->
    ok.

-doc false.
get_info() ->
    get_info(processes(),get_info(erlang:ports(),[])).

get_info([Port|T], Acc) when is_port(Port) ->
    case pinfo(Port, name) of
        undefined ->
            get_info(T,Acc);
        {name, Name} ->
            get_info(T,get_tinfo(Port, Name, Acc))
    end;
get_info([Pid|T],Acc) ->
    case pinfo(Pid, initial_call) of
        undefined ->
            get_info(T,Acc);
        {initial_call, Call} ->
            get_info(T,get_tinfo(Pid, Call, Acc))
    end;
get_info([],Acc) -> Acc.

get_tinfo(P, Id, Acc) ->
    case tinfo(P, flags) of
        undefined ->
            Acc;
		{flags,[]} ->
            Acc;
        {flags,Flags} ->
            [{P,Id,Flags}|Acc]
    end.

format_trace([]) -> [];
format_trace([Item]) -> [ts(Item)];
format_trace([Item|T]) -> [ts(Item) ," | ", format_trace(T)].

ts(send) -> "s";
ts('receive') -> "r";
ts(call) -> "c";
ts(procs) -> "p";
ts(set_on_spawn) -> "sos";
ts(set_on_first_spawn) -> "sofs";
ts(set_on_link) -> "sol";
ts(set_on_first_link) -> "sofl";
ts(Other) -> atom_to_list(Other).

%%
%% Turn (pid or) atom into a PidSpec for trace:process/4,
%% return {badpid,X} on failure 
%%

to_pidspec(X) when is_pid(X) -> 
    case erlang:is_process_alive(X) of
	true -> X;
	false -> {badpid,X}
    end;
to_pidspec(X) when is_port(X) ->
    case erlang:port_info(X) of
        undefined -> {badport, X};
        _ -> X
    end;
to_pidspec(Tag)
  when Tag =:= all;
       Tag =:= ports;
       Tag =:= processes;
       Tag =:= new;
       Tag =:= new_ports;
       Tag =:= new_processes;
       Tag =:= existing;
       Tag =:= existing_ports;
       Tag =:= existing_processes ->
    Tag;
to_pidspec(X) when is_atom(X) ->
    case whereis(X) of
	undefined -> {badpid,X};
	Pid -> Pid
    end;
to_pidspec(X) -> {badpid,X}.

%%
%% Turn (pid or) integer or tuple or list into pid
%%

to_pid(X) when is_pid(X) -> X;
to_pid(X) when is_port(X) -> X;
to_pid(X) when is_integer(X) -> to_pid({0,X,0});
to_pid({X,Y,Z}) ->
    to_pid(lists:concat(["<",integer_to_list(X),".",
			 integer_to_list(Y),".",
			 integer_to_list(Z),">"]));
to_pid(X) when is_list(X) ->
    try list_to_pid(X) of
	Pid -> Pid
    catch
	error:badarg -> {badpid,X}
    end;
to_pid(X) -> {badpid,X}.


pinfo(P, X) when node(P) == node(), is_port(P) -> erlang:port_info(P, X);
pinfo(P, X) when node(P) == node() -> erlang:process_info(P, X);
pinfo(P, X) when is_port(P) -> check(rpc:call(node(P), erlang, port_info, [P, X]));
pinfo(P, X) -> check(rpc:call(node(P), erlang, process_info, [P, X])).


tinfo(P, X) when node(P) == node() -> trace:info(get_trace_session(node()), P, X);
tinfo(P, X) -> check(rpc:call(node(P), trace, info, [get_trace_session(node(P)), P, X])).

check({badrpc, _}) -> undefined;
check(X) -> X.

%% Process loop that processes a trace. Reads the trace with 
%% the reader Reader, and feeds the trace terms 
%% to handler Handler, keeping a state variable for the 
%% handler.
%%
%% Exits 'normal' at end of trace, other exits due to errors.
%%
%% Reader is a lazy list, i.e either a list or a fun/0. 
%% If it is a fun, it is evaluated for rest of the lazy list.
%% A list head is considered to be a trace term. End of list 
%% is interpreted as end of trace.

tc_loop([Term|Tail], Handler, HData0) ->
    HData = Handler(Term, HData0),
    tc_loop(Tail, Handler, HData);
tc_loop([], Handler, HData) ->
    Handler(end_of_trace, HData),
    exit(normal);
tc_loop(Reader, Handler, HData) when is_function(Reader) ->
    tc_loop(Reader(), Handler, HData);
tc_loop(Other, _Handler, _HData) ->
    Modifier = modifier(),
    io:format("~p:tc_loop ~"++Modifier++"p~n", [?MODULE, Other]),
    exit({unknown_term_from_reader, Other}).



%% Returns a reader (lazy list of trace terms) for tc_loop/2.
gen_reader(ip, {Host, Portno}) ->
    case gen_tcp:connect(Host, Portno, [{active, false}, binary]) of
        {ok, Sock} ->
	    %% Just in case this is on the traced node,
	    %% make sure the port is not traced.
	    _ = p(Sock,clear),
	    mk_reader(fun ip_read/2, Sock);
	Error ->
	    exit(Error)
    end;
gen_reader(file, {Filename, wrap, Tail, _, WrapCnt}) ->
    mk_reader_wrap(wrap_sort(wrap_presort(Filename, Tail), WrapCnt));
gen_reader(file, Filename) ->
    gen_reader_file(fun file_read/2, Filename);
gen_reader(follow_file, Filename) ->
    gen_reader_file(fun follow_read/2, Filename).

%% Opens a file and returns a reader (lazy list).
gen_reader_file(ReadFun, Filename) ->
    case file:open(Filename, [read, raw, binary, read_ahead]) of
	{ok, File} ->
	    mk_reader(ReadFun, File);
	Error ->
	    exit({client_cannot_open, Error})
    end.

-dialyzer({no_improper_lists, mk_reader/2}).

%% Creates and returns a reader (lazy list).
mk_reader(ReadFun, Source) ->
    fun() ->
	    case read_term(ReadFun, Source) of
		{ok, Term} ->
		    [Term | mk_reader(ReadFun, Source)];
		eof ->
		    [] % end_of_trace
	    end
    end.

%% Creates and returns a reader (lazy list) for a wrap log.
%% The argument is a sorted list of sort converted 
%% wrap log file names, see wrap_presort/2.

mk_reader_wrap([]) ->
    [];
mk_reader_wrap([Hd | _] = WrapFiles) ->
    case file:open(wrap_name(Hd), [read, raw, binary, read_ahead]) of
	{ok, File} ->
	    mk_reader_wrap(WrapFiles, File);
	Error ->
	    exit({client_cannot_open, Error})
    end.

-dialyzer({no_improper_lists, mk_reader_wrap/2}).

mk_reader_wrap([_Hd | Tail] = WrapFiles, File) ->
    fun() ->
	    case read_term(fun file_read/2, File) of
		{ok, Term} ->
		    [Term | mk_reader_wrap(WrapFiles, File)];
		eof ->
		    ok = file:close(File),
		    case Tail of
			[_|_] ->
			    mk_reader_wrap(Tail);
			[] ->
			    [] % end_of_trace
		    end
	    end
    end.



%% Generic read term function. 
%% Returns {ok, Term} | 'eof'. Exits on errors.

read_term(ReadFun, Source) ->
    case ReadFun(Source, 5) of
	Bin when is_binary(Bin) ->
	    read_term(ReadFun, Source, Bin);
	List when is_list(List) ->
	    read_term(ReadFun, Source, list_to_binary(List));
	eof ->
	    eof
    end.

read_term(ReadFun, Source, <<Op, Size:32>> = Tag) ->
    case Op of
	0 ->
	    case ReadFun(Source, Size) of
		eof ->
		    exit({'trace term missing', 
			  binary_to_list(Tag)});
		Bin when is_binary(Bin) ->
		    {ok, binary_to_term(Bin)};
		List when is_list(List) ->
		    {ok, binary_to_term(list_to_binary(List))}
	    end;
	1 ->
	    {ok, {drop, Size}};
	Junk ->
	    exit({'bad trace tag', Junk})
    end.
    


%% Read functions for different source types, for read_term/2.
%%
%% Returns a binary of length N, an I/O-list of 
%% effective length N or 'eof'. Exits on errors.

file_read(File, N) ->
    case file:read(File, N) of
	{ok, Bin} when byte_size(Bin) =:= N -> 
	    Bin;
	{ok, Bin} when is_binary(Bin) ->
	    exit({'truncated file', binary_to_list(Bin)});
	eof ->
	    eof;
	{error, Reason} ->
	    exit({'file read error', Reason})
    end.

follow_read(File, N) ->
    follow_read(File, N, cur).

follow_read(File, N, Pos) ->
    case file:position(File, Pos) of
	{ok, Offset} ->
	    case file:read(File, N) of
		{ok, Bin} when byte_size(Bin) =:= N -> 
		    Bin;
		{ok, Bin} when is_binary(Bin) ->
		    follow_read(File, N, Offset);
		eof ->
		    follow_read(File, N, Offset);
		{error, Reason} ->
		    exit({'file read error', Reason})
	    end;
	{error, Reason} ->
	    exit({'file position error', Reason})
    end.

ip_read(Socket, N) ->
    case gen_tcp:recv(Socket, N) of
	{ok, Bin} when byte_size(Bin) < N ->
	    [Bin | ip_read(Socket, N-byte_size(Bin))];
	{ok, Bin} when byte_size(Bin) == N ->
	    [Bin];
	{ok, Bin} when is_binary(Bin) ->
	    exit({'socket read too much data', Bin});
	{error, closed} ->
	    eof;
	{error, _Reason} = Error ->
	    exit({'socket read error', Error})
    end.

-doc #{equiv => get_tracer(node())}.
-spec get_tracer() -> term().
get_tracer() ->
    req({get_tracer,node()}).

-doc """
Returns the process, port, or tracer module to which all trace messages are sent.
""".
-spec get_tracer(Nodename) -> {ok, Tracer} when Nodename :: atom(),
   Tracer :: port() | pid() | {module(), term()}.
get_tracer(Node) ->
    req({get_tracer,Node}).

save_pattern([]) ->
    0;
save_pattern(P) ->
    (catch save_pattern(P, get_pattern_table())).

save_pattern(Pattern, PT) ->
    Last = last_pattern(ets:last(PT), PT),
    BPattern = term_to_binary(Pattern),
    case ets:match_object(PT, {'_', BPattern}) of
	[] ->
	    ets:insert(PT, {Last + 1, BPattern}),
	    Last + 1;
	[{N, BPattern}] ->
	    N
    end.

last_pattern('$end_of_table', _PT) ->
    0;
last_pattern(I, PT) when is_atom(I) ->
    last_pattern(ets:prev(PT, I), PT);
last_pattern(I, _PT) when is_integer(I) ->
    I;
last_pattern(_, _) ->
    throw({error, badtable}).


get_pattern_table() ->
    {ok, Ret} = req(get_table),
    Ret.

new_pattern_table() ->
    PT = ets:new(dbg_tab, [ordered_set, public]),
    ets:insert(PT, 
	       {x, 
		term_to_binary([{'_',[],[{exception_trace}]}])}),
    ets:insert(PT, 
	       {exception_trace, 
		term_to_binary(x)}),
    ets:insert(PT,
	       {c,
		term_to_binary([{'_',[],[{message,{caller_line}}]}])}),
    ets:insert(PT,
	       {caller_trace,
		term_to_binary(c)}),
    ets:insert(PT,
	       {cx,
		term_to_binary([{'_',[],[{exception_trace},
					 {message,{caller_line}}]}])}),
    ets:insert(PT,
	       {caller_exception_trace,
		term_to_binary(cx)}),
    PT.


pt_doforall(Fun, Ld) ->
    T = get_pattern_table(),
    pt_doforall(T, Fun, ets:first(T), Ld).

pt_doforall(_, _, '$end_of_table', _Ld) -> 
    ok;
pt_doforall(T, Fun, Key, Ld) ->
    [{A,B}] = ets:lookup(T,Key),
    NLd = Fun({A,binary_to_term(B)},Ld),
    pt_doforall(T,Fun,ets:next(T,Key),NLd).

lint_tp([]) ->
    {ok,[]};
lint_tp(Pattern) ->
    case erlang:match_spec_test([],Pattern,trace) of
	{ok,_Res,Warnings,_Flags} ->
	    {ok, Warnings};
	{error, Reasons} ->
	    {error, Reasons}
    end.

check_list(T) ->
    case (catch lists:foldl(
		  fun(Val,_) ->
			  {ok,_,_,_} = 
			      erlang:match_spec_test([],Val,trace),
			  ok
		  end,
		  ok, T)) of
	{'EXIT',_} ->
	    {error, bad_match_spec};
	ok ->
	    ok;
	_Else ->
	    {error, badfile}
    end.



%% Find all possible wrap log files.
%% Returns: a list of sort converted filenames.
%%
%% The sort conversion is done by extracting the wrap sequence counter
%% from the filename, and calling wrap_encode/2.
-doc false.
wrap_presort(Filename, Tail) ->
    Name = filename:basename(Filename),
    Dirname = filename:dirname(Filename),
    case file:list_dir(Dirname) of
	{ok, Files} ->
	    lists:zf(
	      fun(N) ->
		      case match_front(N, Name) of
			  false ->
			      false;
			  X ->
			      case match_rear(X, Tail) of
				  false ->
				      false;
				  C -> % Counter
				      case match_0_9(C) of
					  true ->
					      {true, 
					       wrap_encode(
						 filename:join(Dirname, N),
						 C)};
					  false ->
					      false
				      end
			      end
		      end
	      end,
	      Files);
	_ ->
	    []
    end.



%% Sorts a list of sort converted files
-doc false.
wrap_sort(Files, N) ->
    wrap_sortfix(lists:sort(Files), N).

%% Finish the sorting, since the lists:sort order is not the correct order.
%% Cut the list of files at the gap (at least one file is supposed
%% to be 'missing') and create a new list by cons'ing the two parts
%% in the right order.
-doc false.
wrap_sortfix([], N) when N >= 1 ->
    [];
wrap_sortfix([], _N) ->
    exit(inconsistent_wrap_file_trace_set);
%% file 0, gap 1..N
wrap_sortfix([{0, _}] = Files, N) when N >= 1 ->
    Files;
wrap_sortfix([{0, _}], _N) ->
    exit(inconsistent_wrap_file_trace_set);
%% files 0, ...
wrap_sortfix([{0, _} | _] = Files, N) when N >= 1->
    wrap_sortfix_1(Files, N, [], Files);
%% gap 0, files 1, ...
wrap_sortfix([{1, _} | _] = Files, N) when N >= 1 ->
    wrap_sortfix_2(Files, N, [], Files);
wrap_sortfix([{_C, _} | _], _N) ->
    exit(inconsistent_wrap_file_trace_set).

%% files 0..C, gap C+1..N
wrap_sortfix_1([{C, _}], N, _R, Files) 
  when C < N ->
    Files;
%% files 0..C1, C1+1==C2, ...
wrap_sortfix_1([{C1, _} = F1 | [{C2, _} | _] = Tail], N, R, Files) 
  when C1+1 == C2, C2 < N ->
    wrap_sortfix_1(Tail, N, [F1 | R], Files);
%% files 0..C1, gap C1+1, files C1+2==C2, ...
wrap_sortfix_1([{C1, _} = F1 | [{C2, _} | _] = Tail], N, R, _Files) 
  when C1+2 == C2, C2 =< N ->
    wrap_sortfix_2(Tail, N, lists:reverse([F1 | R]), Tail);
wrap_sortfix_1([_F1 | [_F2 | _]], _N, _R, _Files) ->
    exit(inconsistent_wrap_file_trace_set).

%% M == length(R); files 0..M-1, gap M, files M+1..N
wrap_sortfix_2([{N, _}], N, R, Files) ->
    Files ++ R;
wrap_sortfix_2([{_C, _}], _N, _R, _Files) ->
    exit(inconsistent_wrap_file_trace_set);
%% M == length(R); files 0..M-1, gap M, files M+1..C1, C1+1==C2, ...
wrap_sortfix_2([{C1, _} | [{C2, _} | _] = Tail], N, R, Files)
  when C1+1 == C2, C2 =< N ->
    wrap_sortfix_2(Tail, N, R, Files);
wrap_sortfix_2([{_C1, _} | [{_C2, _} | _]], _N, _R, _Files) ->
    exit(inconsistent_wrap_file_trace_set).



%% Extract the filenames from a list of sort converted ones.
-doc false.
wrap_postsort(Files) ->    
    lists:map(fun wrap_name/1, Files).

wrap_encode(N, C) ->
    {list_to_integer(C), N}.

wrap_name({_C, N}) ->
    N.

%% Returns what is left of ListA when removing all matching
%% elements from ListB, or false if some element did not match,
%% or if ListA runs out of elements before ListB.
-doc false.
match_front(ListA, []) when is_list(ListA) ->
    ListA;
match_front([], ListB) when is_list(ListB) ->
    false;
match_front([Hd|TlA], [Hd|TlB]) ->
    match_front(TlA,TlB);
match_front([_HdA|_], [_HdB|_]) ->
    false.

%% Reversed version of match_front/2
-doc false.
match_rear(ListA, ListB) when is_list(ListA), is_list(ListB) ->
    case match_front(lists:reverse(ListA), lists:reverse(ListB)) of
	false ->
	    false;
	List ->
	    lists:reverse(List)
    end.

%% Returns true if the non-empty list arguments contains all
%% characters $0 .. $9.
-doc false.
match_0_9([]) ->
    false;
match_0_9([H]) when is_integer(H), $0 =< H, H =< $9 ->
    true;
match_0_9([H|T]) when is_integer(H), $0 =< H, H =< $9 ->
    match_0_9(T);
match_0_9(L) when is_list(L) ->
    false.

%%%%%%%%%%%%%%%%%%
%% Help...
%%%%%%%%%%%%%%%%%%

help_display(Bin) when is_binary(Bin) ->
    io:put_chars(Bin),
    io:nl().

-doc """
Gives a list of items for brief online help.

`h` stands for **h**elp.
""".
-spec h() -> ok .
h() ->
    help_display(
      ~"""
       The following help items are available:
          p, c
              - Set trace flags for processes
          tp, tpl, ctp, ctpl, ctpg, ltp, dtp, wtp, rtp
              - Manipulate trace patterns for functions
          n, cn, ln
              - Add/remove traced nodes
          tracer, trace_port, trace_client, get_tracer, stop, stop_clear
              - Manipulate tracer process/port
          i
              - Info

       call dbg:h(Item) for brief help a brief description,
       of one of the items above.
       """).

-doc """
Gives a brief help text for functions in the `dbg` module.

`h` stands for **h**elp.

The available items can be listed by calling `dbg:h/0`.
""".
-spec h(Item) -> ok  when Item :: atom().
h(Item) when is_atom(Item) ->
    io:put_chars(help_text(Item)),
    io:nl().

help_text(p) ->
    ~"""
      p(Item) -> {ok, MatchDesc} | {error, term()}
       - Traces messages to and from Item.
      p(Item, Flags) -> {ok, MatchDesc} | {error, term()}
       - Traces Item according to Flags.
         Flags can be one of s, r, m, c, p, sos, sol, sofs,
         sofl, all, clear, or any flag accepted by erlang:trace/3
      """;
help_text(c) ->
    ~"""
     c(Mod, Name, Args)
      - Evaluates apply(Mod, Name, Args) with all trace flags set.
     c(Mod, Name, Args, Flags)
       - Evaluates apply(Mod, Name, Args) with Flags trace flags set.
     """;
help_text(i) ->
    ~"""
     i() -> ok
       - Displays information about all traced processes.
     """;
help_text(tp) ->
    ~"""
     tp(Module,MatchSpec)
      - Same as tp({Module, '_', '_'}, MatchSpec)
     tp(Module,Function,MatchSpec)
      - Same as tp({Module, Function, '_'}, MatchSpec)
     tp(Module, Function, Arity, MatchSpec)
      - Same as tp({Module, Function, Arity}, MatchSpec)
     tp({Module, Function, Arity}, MatchSpec) -> {ok, MatchDesc} | {error, term()}
      - Set pattern for traced global function calls.
     """;
help_text(tpl) ->
    ~"""
     tpl(Module,MatchSpec)
      - Same as tpl({Module, '_', '_'}, MatchSpec)
     tpl(Module,Function,MatchSpec)
      - Same as tpl({Module, Function, '_'}, MatchSpec)
     tpl(Module, Function, Arity, MatchSpec)
      - Same as tpl({Module, Function, Arity}, MatchSpec)
     tpl({Module, Function, Arity}, MatchSpec) -> {ok, MatchDesc} | {error, term()}
      - Set pattern for traced local (as well as global) function calls.
     """;
help_text(ctp) ->
    ~"""
     ctp()
      - Same as ctp({'_', '_', '_'})
     ctp(Module)
      - Same as ctp({Module, '_', '_'})
     ctp(Module, Function)
      - Same as ctp({Module, Function, '_'})
     ctp(Module, Function, Arity)
      - Same as ctp({Module, Function, Arity})
     ctp({Module, Function, Arity}) -> {ok, MatchDesc} | {error, term()}
      - Clear call trace pattern for the specified functions
     """;
help_text(ctpl) ->
    ~"""
     ctpl()
      - Same as ctpl({'_', '_', '_'})
     ctpl(Module)
      - Same as ctpl({Module, '_', '_'})
     ctpl(Module, Function)
      - Same as ctpl({Module, Function, '_'})
     ctpl(Module, Function, Arity)
      - Same as ctpl({Module, Function, Arity})
     ctpl({Module, Function, Arity}) -> {ok, MatchDesc} | {error, term()}
      - Clear local call trace pattern for the specified functions
     """;
help_text(ctpg) ->
    ~"""
     ctpg()
      - Same as ctpg({'_', '_', '_'})
     ctpg(Module)
      - Same as ctpg({Module, '_', '_'})
     ctpg(Module, Function)
      - Same as ctpg({Module, Function, '_'})
     ctpg(Module, Function, Arity)
      - Same as ctpg({Module, Function, Arity})
     ctpg({Module, Function, Arity}) -> {ok, MatchDesc} | {error, term()}
      - Clear global call trace pattern for the specified functions
     """;
help_text(ltp) ->
    ~"""
     ltp() -> ok
      - Lists saved and built-in match specifications on the console.
     """;
help_text(dtp) ->
    ~"""
     dtp() -> ok
      - Deletes all saved match specifications.
     dtp(N) -> ok
      - Deletes a specific saved match_spec.
     """;
help_text(wtp) ->
    ~"""
     wtp(Name) -> ok | {error, IOError}
      - Writes all saved match specifications to a file
     """;
help_text(rtp) ->
    ~"""
     rtp(Name) -> ok | {error, Error}
      - Read saved match specifications from file.
     """;
help_text(n) ->
    ~"""
     n(Nodename) -> {ok, Nodename} | {error, Reason}
      - Starts a tracer server on the given node.
     """;
help_text(cn) ->
    ~"""
     cn(Nodename) -> ok
       - Clears a node from the list of traced nodes.
     """;
help_text(ln) ->
    ~"""
     ln() -> ok
      - Shows the list of traced nodes on the console.
     """;
help_text(tracer) ->
    ~"""
     tracer() -> {ok, pid()} | {error, already_started}
      - Starts a tracer server that handles trace messages.
     tracer(Type, Data) -> {ok, pid()} | {error, Error}
      - Starts a tracer server with additional parameters
     """;
help_text(trace_port) ->
    ~"""
     trace_port(Type, Parameters) -> fun()
      - Creates and returns a trace port generating fun
     """;
help_text(trace_client) ->
    ~"""
     trace_client(Type, Parameters) -> pid()
      - Starts a trace client that reads messages created by a trace port driver
     trace_client(Type, Parameters, HandlerSpec) -> pid()
      - Starts a trace client that reads messages created by a trace port driver,
        with a user defined handler
     """;
help_text(get_tracer) ->
    ~"""
     get_tracer() -> {ok, Tracer}
       - Returns the process or port to which all trace messages are sent.
     get_tracer(Node) -> {ok, Tracer}
       - Returns the process or port to which all trace messages are sent.
     """;
help_text(stop) ->
    ~"""
     stop() -> ok
       - Stops the dbg server and the tracing of all processes.
         Does not clear any trace patterns.
     """;
help_text(stop_clear) ->
     ~"""
      stop_clear() -> ok
       - Deprecated. Stops the dbg server and the tracing of all processes,
         and clears all trace patterns.
     """.
