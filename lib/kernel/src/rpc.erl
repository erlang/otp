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
-module(rpc).
-moduledoc """
Remote Procedure Call services.

This module contains services similar to Remote Procedure Calls. It also
contains broadcast facilities and parallel evaluators. A remote procedure call
is a method to call a function on a remote node and collect the answer. It is
used for collecting information on a remote node, or for running a function with
some specific side effects on the remote node.

> #### Note {: .info }
>
> `rpc:call/4` and friends makes it quite hard to distinguish between successful
> results, raised exceptions, and other errors. This cannot be changed due to
> compatibility reasons. As of OTP 23, a new module `m:erpc` was introduced in
> order to provide an API that makes it possible to distinguish between the
> different results. The `erpc` module provides a subset (however, the central
> subset) of the functionality available in the `rpc` module. The `erpc`
> implementation also provides a more scalable implementation with better
> performance than the original `rpc` implementation. However, since the
> introduction of `erpc`, the `rpc` module implements large parts of its central
> functionality using `erpc`, so the `rpc` module won't not suffer scalability
> wise and performance wise compared to `erpc`.

> #### Note {: .info }
>
> For some important information about distributed signals, see the
> [Blocking Signaling Over Distribution](`e:system:ref_man_processes.md#blocking-signaling-over-distribution`)
> section in the _Processes_ chapter of the _Erlang Reference Manual_. Blocking
> signaling can, for example, cause timeouts in `rpc` to be significantly
> delayed.
""".

%%
%% As of OTP 25 the rpc module require server side support for erpc.
%%

%% General rpc, broadcast,multicall, promise and parallel evaluator
%% facility

%% This code used to reside in net.erl, but has now been moved to
%% a separate module.

-define(NAME, rex).
-define(TAB_NAME, rex_nodes_observer).

-behaviour(gen_server).

-compile(nowarn_deprecated_catch).

-export([start/0, start_link/0, stop/0,
	 call/4, call/5,
	 block_call/4, block_call/5,
	 server_call/4,
	 cast/4,
	 abcast/2,
	 abcast/3,
	 sbcast/2,
	 sbcast/3,
	 eval_everywhere/3,
	 eval_everywhere/4,
	 multi_server_call/2,
	 multi_server_call/3,
	 multicall/3,
	 multicall/4,
	 multicall/5,
	 async_call/4,
	 yield/1,
	 nb_yield/2,
	 nb_yield/1,
	 parallel_eval/1,
	 pmap/3, pinfo/1, pinfo/2]).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Internals
-export([proxy_user_flush/0]).

-export_type([key/0]).

%% Removed functions

-removed([{safe_multi_server_call,2,"use rpc:multi_server_call/2 instead"},
          {safe_multi_server_call,3,"use rpc:multi_server_call/3 instead"}]).

%%------------------------------------------------------------------------

-type state() :: map().

%%------------------------------------------------------------------------

-define(MAX_INT_TIMEOUT, 4294967295).
-define(TIMEOUT_TYPE, 0..?MAX_INT_TIMEOUT | 'infinity').
-define(IS_VALID_TMO_INT(TI_), (is_integer(TI_)
                                andalso (0 =< TI_)
                                andalso (TI_ =< ?MAX_INT_TIMEOUT))).
-define(IS_VALID_TMO(T_), ((T_ == infinity) orelse ?IS_VALID_TMO_INT(T_))).

%% The rex server may receive a huge amount of
%% messages. Make sure that they are stored off heap to
%% avoid exessive GCs.

-define(SPAWN_OPTS, [{spawn_opt,[{message_queue_data,off_heap}]}]).

%% Remote execution and broadcasting facility

-doc false.
-spec start() -> {'ok', pid()} | 'ignore' | {'error', term()}.

start() ->
    gen_server:start({local,?NAME}, ?MODULE, [], ?SPAWN_OPTS).

-doc false.
-spec start_link() -> {'ok', pid()} | 'ignore' | {'error', term()}.

start_link() ->
    %% The rex server process may receive a huge amount of
    %% messages. Make sure that they are stored off heap to
    %% avoid exessive GCs.
    gen_server:start_link({local,?NAME}, ?MODULE, [], ?SPAWN_OPTS).

-doc false.
-spec stop() -> term().

stop() ->
    stop(?NAME).

stop(Rpc) ->
    gen_server:call(Rpc, stop, infinity).

-doc false.
-spec init([]) -> {'ok', state()}.

init([]) ->
    process_flag(trap_exit, true),
    {ok, #{nodes_observer => start_nodes_observer()}}.

-doc false.
-spec handle_call(
        term(),
        gen_server:from() | {?NAME,term()},
        state()) ->
                         {'noreply', state()} |
                         {'reply', term(), state()} |
                         {'stop', 'normal', 'stopped', state()}.

handle_call({call, Mod, Fun, Args, Gleader}, To, S) ->
    %% Spawn not to block the rex server.
    ExecCall = fun () ->
                       set_group_leader(Gleader),
                       GleaderBeforeCall = group_leader(),
                       Reply = execute_call(Mod, Fun, Args),
                       case Gleader of
                           {send_stdout_to_caller, _} ->
                               %% The group leader sends the response
                               %% to make sure that the client gets
                               %% all stdout that it should get before
                               %% the response
                               Ref = erlang:make_ref(),
                               GleaderBeforeCall ! {stop, self(), Ref, To, Reply},
                               receive
                                   Ref -> ok
                               end;
                           _ ->
                               reply(To, Reply)
                       end
               end,
    try
        {_,Mon} = spawn_monitor(ExecCall),
        {noreply, maps:put(Mon, To, S)}
    catch
        error:system_limit ->
            {reply, {badrpc, {'EXIT', system_limit}}, S}
    end;
handle_call({block_call, Mod, Fun, Args, Gleader}, _To, S) ->
    MyGL = group_leader(),
    set_group_leader(Gleader),
    Reply = execute_call(Mod, Fun, Args),
    group_leader(MyGL, self()), % restore
    {reply, Reply, S};
handle_call(stop, _To, S) ->
    {stop, normal, stopped, S};
handle_call(_, _To, S) ->
    {noreply, S}.  % Ignore !

-doc false.
-spec handle_cast(term(), state()) -> {'noreply', state()}.

handle_cast({cast, Mod, Fun, Args, Gleader}, S) ->
    _ = try
            spawn(fun() ->
                          set_group_leader(Gleader),
                          erpc:execute_cast(Mod, Fun, Args)
                  end)
        catch
            error:system_limit ->
                ok
        end,
    {noreply, S};
handle_cast(_, S) ->
    {noreply, S}.  % Ignore !

-doc false.
-spec handle_info(term(), state()) -> {'noreply', state()}.

handle_info({'DOWN', M, process, P, _}, #{nodes_observer := {P,M}} = S) ->
    {noreply, S#{nodes_observer => start_nodes_observer()}};
handle_info({'DOWN', M, process, _, normal}, S) ->
    {noreply, maps:remove(M, S)};
handle_info({'DOWN', M, process, _, Reason}, S) ->
    case maps:get(M, S, undefined) of
	undefined ->
	    {noreply, S};
	{_, _} = To ->
	    reply(To, {badrpc, {'EXIT', Reason}}),
	    {noreply, maps:remove(M, S)}
    end;
handle_info({From, {sbcast, Name, Msg}}, S) ->
    _ = case catch Name ! Msg of  %% use catch to get the printout
            {'EXIT', _} ->
                From ! {?NAME, node(), {nonexisting_name, Name}};
            _ ->
                From ! {?NAME, node(), node()}
        end,
    {noreply, S};
handle_info({From, {send, Name, Msg}}, S) ->
    _ = case catch Name ! {From, Msg} of %% use catch to get the printout
            {'EXIT', _} ->
                From ! {?NAME, node(), {nonexisting_name, Name}};
            _ ->
                ok    %% It's up to Name to respond !!!!!
        end,
    {noreply, S};
handle_info({From, {call, Mod, Fun, Args, Gleader}}, S) ->
    %% Special for hidden C node's, uugh ...
    To = {?NAME, From},
    NewGleader =
        case Gleader of
            send_stdout_to_caller ->
                {send_stdout_to_caller, From};
            _ ->
                Gleader
        end,
    Request = {call, Mod, Fun, Args, NewGleader},
    case handle_call(Request, To, S) of
        {noreply, _NewS} = Return ->
            Return;
        {reply, Reply, NewS} ->
            reply(To, Reply),
            {noreply, NewS}
    end;
handle_info({From, features_request}, S) ->
    From ! {features_reply, node(), [erpc]},
    {noreply, S};
handle_info(_, S) ->
    {noreply, S}.

-doc false.
-spec terminate(term(), state()) -> 'ok'.

terminate(_, _S) ->
    ok.

-doc false.
-spec code_change(term(), state(), term()) -> {'ok', state()}.

code_change(_, S, _) ->
    {ok, S}.


%% RPC aid functions ....

reply({?NAME, From}, Reply) ->
    From ! {?NAME, Reply},
    ok;
reply({From, _} = To, Reply) when is_pid(From) ->
    gen_server:reply(To, Reply).


execute_call(Mod, Fun, Args) ->
    try
        {return, Return} = erpc:execute_call(Mod, Fun, Args),
        Return
    catch
        throw:Result ->
            Result;
        exit:Reason ->
            {badrpc, {'EXIT', Reason}};
        error:Reason:Stack ->
            case erpc:is_arg_error(Reason, Mod, Fun, Args) of
                true ->
                    {badrpc, {'EXIT', Reason}};
                false ->
                    RpcStack = erpc:trim_stack(Stack, Mod, Fun, Args),
                    {badrpc, {'EXIT', {Reason, RpcStack}}}
            end
    end.

set_group_leader(Gleader) when is_pid(Gleader) -> 
    group_leader(Gleader, self());
set_group_leader({send_stdout_to_caller, CallerPid}) ->
    group_leader(cnode_call_group_leader_start(CallerPid), self());
set_group_leader(user) -> 
    %% For example, hidden C nodes doesn't want any I/O.
    Gleader = case whereis(user) of
		  Pid when is_pid(Pid) -> Pid;
		  undefined -> proxy_user()
	      end,
    group_leader(Gleader, self()).


%% The 'rex_proxy_user' process serve as group leader for early rpc's that
%% may do IO before the real group leader 'user' has been started (OTP-7903).
proxy_user() ->
    case whereis(rex_proxy_user) of
	Pid when is_pid(Pid) -> Pid;
	undefined ->
	    Pid = spawn(fun() -> proxy_user_loop() end),
	    try register(rex_proxy_user,Pid) of
		true -> Pid
	    catch error:_ -> % spawn race, kill and try again
		exit(Pid,kill),
		proxy_user()
	    end
    end.

proxy_user_loop() ->
    %% Wait for the real 'user' to start
    timer:sleep(200),
    case whereis(user) of
	Pid when is_pid(Pid) -> proxy_user_flush();
	undefined -> proxy_user_loop()
    end.

-doc false.
-spec proxy_user_flush() -> no_return().

proxy_user_flush() ->
    %% Forward all received messages to 'user'
    receive Msg ->
	    user ! Msg
    after 10*1000 ->
	    %% Hibernate but live for ever, as it's not easy to know
	    %% when no more messages will arrive.
	    erlang:hibernate(?MODULE, proxy_user_flush, [])
    end,
    proxy_user_flush().

start_nodes_observer() ->
    Init = fun () ->
                   process_flag(priority, high),
                   process_flag(trap_exit, true),
                   Tab = ets:new(?TAB_NAME,
                                 [{read_concurrency, true},
                                  protected]),
                   persistent_term:put(?TAB_NAME, Tab),
                   ok = net_kernel:monitor_nodes(true),
                   lists:foreach(fun (N) ->
                                         self() ! {nodeup, N}
                                 end,
                                 [node()|nodes()]),
                   nodes_observer_loop(Tab)
        end,
    spawn_monitor(Init).

nodes_observer_loop(Tab) ->
    receive
        {nodeup, nonode@nohost} ->
            ok;
        {nodeup, N} ->
            {?NAME, N} ! {self(), features_request};
        {nodedown, N} ->
            ets:delete(Tab, N);
        {features_reply, N, FeatureList} ->
            try
                SpawnRpc = lists:member(erpc, FeatureList),
                ets:insert(Tab, {N, SpawnRpc})
            catch
                _:_ -> ets:insert(Tab, {N, false})
            end;
        _ ->
            ignore
    end,
    nodes_observer_loop(Tab).

%% THE rpc client interface

%% Call

-define(RPCIFY(ERPC_),
        try ERPC_ of
            {'EXIT', _} = BadRpc_ ->
                {badrpc, BadRpc_};
            Result_ ->
                Result_
        catch
            Class_:Reason_ ->
                rpcify_exception(Class_, Reason_)
        end).

-doc """
Evaluates [`apply(Module, Function, Args)`](`apply/3`) on node `Node` and
returns the corresponding value `Res`, or `{badrpc, Reason}` if the call fails.
The same as calling
[`rpc:call(Node, Module, Function, Args, infinity)`](`call/5`).
""".
-spec call(Node, Module, Function, Args) -> Res | {badrpc, Reason} when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Res :: term(),
      Reason :: term().

call(N,M,F,A) ->
    call(N,M,F,A,infinity).

-doc """
Evaluates [`apply(Module, Function, Args)`](`apply/3`) on node `Node` and
returns the corresponding value `Res`, or `{badrpc, Reason}` if the call fails.
`Timeout` is a time-out value in milliseconds. If the call times out, `Reason`
is `timeout`.

If the reply arrives after the call times out, no message contaminates the
caller's message queue.

> #### Note {: .info }
>
> If you want the ability to distinguish between results, you may want to
> consider using the [`erpc:call()`](`erpc:call/4`) function from the `erpc`
> module instead.

> #### Note {: .info }
>
> Here follows the details of what exactly is returned.
>
> `{badrpc, Reason}` will be returned in the following circumstances:
>
> - The called function fails with an `exit` exception.
> - The called function fails with an `error` exception.
> - The called function returns a term that matches `{'EXIT', _}`.
> - The called function `throws` a term that matches `{'EXIT', _}`.
>
> `Res` is returned in the following circumstances:
>
> - The called function returns normally with a term that does **not** match
>   `{'EXIT',_}`.
> - The called function `throw`s a term that does **not** match `{'EXIT',_}`.

> #### Note {: .info }
>
> You cannot make _any_ assumptions about the process that will perform the
> `apply()`. It may be the calling process itself, an `rpc` server, another
> server, or a freshly spawned process.
""".
-spec call(Node, Module, Function, Args, Timeout) ->
                  Res | {badrpc, Reason} when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Res :: term(),
      Reason :: term(),
      Timeout :: ?TIMEOUT_TYPE.

call(N,M,F,A,T) ->
    ?RPCIFY(erpc:call(N, M, F, A, T)).

-doc """
The same as calling
[`rpc:block_call(Node, Module, Function, Args, infinity)`](`block_call/5`).
""".
-spec block_call(Node, Module, Function, Args) -> Res | {badrpc, Reason} when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Res :: term(),
      Reason :: term().

block_call(N,M,F,A) ->
    block_call(N,M,F,A,infinity).

-doc """
The same as calling
[`rpc:call(Node, Module, Function, Args, Timeout)`](`call/5`) with the exception
that it also blocks other `rpc:block_call/5` operations from executing
concurrently on the node `Node`.

> #### Warning {: .warning }
>
> Note that it also blocks other operations than just `rpc:block_call/5`
> operations, so use it with care.
""".
-spec block_call(Node, Module, Function, Args, Timeout) ->
                  Res | {badrpc, Reason} when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Res :: term(),
      Reason :: term(),
      Timeout :: ?TIMEOUT_TYPE.

block_call(N,M,F,A,Timeout) when is_atom(N),
                                 is_atom(M),
                                 is_list(A),
                                 ?IS_VALID_TMO(Timeout) ->
    do_srv_call(N, {block_call,M,F,A,group_leader()}, Timeout).
    

%% call() implementation utilizing erpc:call()...

rpcify_exception(throw, {'EXIT', _} = BadRpc) ->
    {badrpc, BadRpc};
rpcify_exception(throw, Return) ->
    Return;
rpcify_exception(exit, {exception, Exit}) ->
    {badrpc, {'EXIT', Exit}};
rpcify_exception(exit, {signal, Reason}) ->
    {badrpc, {'EXIT', Reason}};
rpcify_exception(exit, Reason) ->
    exit(Reason);
rpcify_exception(error, {exception, Error, Stack}) ->
    {badrpc, {'EXIT', {Error, Stack}}};
rpcify_exception(error, {erpc, badarg}) ->
    error(badarg);
rpcify_exception(error, {erpc, noconnection}) ->
    {badrpc, nodedown};
rpcify_exception(error, {erpc, timeout}) ->
    {badrpc, timeout};
rpcify_exception(error, {erpc, notsup}) ->
    {badrpc, notsup};
rpcify_exception(error, {erpc, Error}) ->
    {badrpc, {'EXIT', Error}};
rpcify_exception(error, Reason) ->
    error(Reason).

do_srv_call(Node, Request, infinity) ->
    rpc_check(catch gen_server:call({?NAME,Node}, Request, infinity));
do_srv_call(Node, Request, Timeout) ->
    Tag = make_ref(),
    {Receiver,Mref} =
	erlang:spawn_monitor(
	  fun() ->
		  %% Middleman process. Should be unsensitive to regular
		  %% exit signals.
		  process_flag(trap_exit, true),
		  Result = gen_server:call({?NAME,Node}, Request, Timeout),
		  exit({self(),Tag,Result})
	  end),
    receive
	{'DOWN',Mref,_,_,{Receiver,Tag,Result}} ->
	    rpc_check(Result);
	{'DOWN',Mref,_,_,Reason} ->
	    %% The middleman code failed. Or someone did 
	    %% exit(_, kill) on the middleman process => Reason==killed
	    rpc_check_t({'EXIT',Reason})
    end.

rpc_check_t({'EXIT', {timeout,_}}) -> {badrpc, timeout};
rpc_check_t({'EXIT', {timeout_value,_}}) -> error(badarg);
rpc_check_t(X) -> rpc_check(X).
	    
rpc_check({'EXIT', {{nodedown,_},_}}) ->
    {badrpc, nodedown};
rpc_check({'EXIT', _}=Exit) ->
    %% Should only happen if the rex process on the other node
    %% died.
    {badrpc, Exit};
rpc_check(X) -> X.


%% This is a real handy function to be used when interacting with
%% a server called Name at node Node. It is assumed that the server
%% receives messages of the form {From, Request} and replies are of the
%% form From ! {ReplyWrapper, Node, Reply}.
%% This function makes such a server call and ensures that that
%% The entire call is packed into an atomic transaction which 
%% either succeeds or fails, i.e. never hangs (unless the server itself hangs).

-doc """
Can be used when interacting with a server called `Name` on node `Node`. It is
assumed that the server receives messages in the format `{From, Msg}` and
replies using `From ! {ReplyWrapper, Node, Reply}`. This function makes such a
server call and ensures that the entire call is packed into an atomic
transaction, which either succeeds or fails. It never hangs, unless the server
itself hangs.

The function returns the answer `Reply` as produced by the server `Name`, or
`{error, Reason}`.
""".
-spec server_call(Node, Name, ReplyWrapper, Msg) -> Reply | {error, Reason} when
      Node :: node(),
      Name :: atom(),
      ReplyWrapper :: term(),
      Msg :: term(),
      Reply :: term(),
      Reason :: nodedown.

server_call(Node, Name, ReplyWrapper, Msg) 
  when is_atom(Node), is_atom(Name) ->
    if node() =:= nonode@nohost, Node =/= nonode@nohost ->
	    {error, nodedown};
       true ->
	    Ref = erlang:monitor(process, {Name, Node}),
	    {Name, Node} ! {self(), Msg},
	    receive
		{'DOWN', Ref, _, _, _} ->
		    {error, nodedown};
		{ReplyWrapper, Node, Reply} ->
		    erlang:demonitor(Ref, [flush]),
		    Reply
	    end
    end.

-doc """
Evaluates [`apply(Module, Function, Args)`](`apply/3`) on node `Node`. No
response is delivered and the calling process is not suspended until the
evaluation is complete, as is the case with [`call/4,5`](`call/4`).

> #### Note {: .info }
>
> You cannot make _any_ assumptions about the process that will perform the
> `apply()`. It may be an `rpc` server, another server, or a freshly spawned
> process.
""".
-spec cast(Node, Module, Function, Args) -> true when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()].

cast(Node, Mod, Fun, Args) ->
    try
        ok = erpc:cast(Node, Mod, Fun, Args)
    catch
        error:{erpc, badarg} ->
            error(badarg)
    end,
    true.

%% Asynchronous broadcast, returns nothing, it's just send 'n' pray
-doc "Equivalent to [`abcast([node()|nodes()], Name, Msg)`](`abcast/3`).".
-spec abcast(Name, Msg) -> abcast when
      Name :: atom(),
      Msg :: term().

abcast(Name, Mess) ->
    abcast([node() | nodes()], Name, Mess).

-doc """
Broadcasts the message `Msg` asynchronously to the registered process `Name` on
the specified nodes.
""".
-spec abcast(Nodes, Name, Msg) -> abcast when
      Nodes :: [node()],
      Name :: atom(),
      Msg :: term().

abcast([Node|Tail], Name, Mess) ->
    Dest = {Name,Node},
    try erlang:send(Dest, Mess) catch error:_ -> ok end,
    abcast(Tail, Name, Mess);
abcast([], _,_) -> abcast.


%% Synchronous broadcast, returns a list of the nodes which had Name
%% as a registered server. Returns {Goodnodes, Badnodes}.
%% Synchronous in the sense that we know that all servers have received the
%% message when we return from the call, we can't know that they have
%% processed the message though.

-doc "Equivalent to [`sbcast([node()|nodes()], Name, Msg)`](`sbcast/3`).".
-spec sbcast(Name, Msg) -> {GoodNodes, BadNodes} when
      Name :: atom(),
      Msg :: term(),
      GoodNodes :: [node()],
      BadNodes :: [node()].

sbcast(Name, Mess) ->
    sbcast([node() | nodes()], Name, Mess).

-doc """
Broadcasts the message `Msg` synchronously to the registered process `Name` on
the specified nodes.

Returns `{GoodNodes, BadNodes}`, where `GoodNodes` is the list of nodes that
have `Name` as a registered process.

The function is synchronous in the sense that it is known that all servers have
received the message when the call returns. It is not possible to know that the
servers have processed the message.

Any further messages sent to the servers, after this function has returned, are
received by all servers after this message.
""".
-spec sbcast(Nodes, Name, Msg) -> {GoodNodes, BadNodes} when
      Name :: atom(),
      Msg :: term(),
      Nodes :: [node()],
      GoodNodes :: [node()],
      BadNodes :: [node()].

sbcast(Nodes, Name, Mess) ->
    Monitors = send_nodes(Nodes, ?NAME, {sbcast, Name, Mess}, []),
    rec_nodes(?NAME, Monitors).

-doc """
Equivalent to
[`eval_everywhere([node()|nodes()], Module, Function, Args)`](`eval_everywhere/4`).
""".
-spec eval_everywhere(Module, Function, Args) -> abcast when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].

eval_everywhere(Mod, Fun, Args) ->
    eval_everywhere([node() | nodes()] , Mod, Fun, Args).

-doc """
Evaluates [`apply(Module, Function, Args)`](`apply/3`) on the specified nodes.
No answers are collected.
""".
-spec eval_everywhere(Nodes, Module, Function, Args) -> abcast when
      Nodes :: [node()],
      Module :: module(),
      Function :: atom(),
      Args :: [term()].

eval_everywhere(Nodes, Mod, Fun, Args) ->
    lists:foreach(fun (Node) ->
                          cast(Node, Mod, Fun, Args)
                  end,
                  Nodes),
    abcast.

send_nodes([Node|Tail], Name, Msg, Monitors) when is_atom(Node) ->
    Monitor = start_monitor(Node, Name),
    %% Handle non-existing names in rec_nodes.
    catch {Name, Node} ! {self(), Msg},
    send_nodes(Tail, Name, Msg, [Monitor | Monitors]);
send_nodes([_Node|Tail], Name, Msg, Monitors) ->
    %% Skip non-atom _Node
    send_nodes(Tail, Name, Msg, Monitors);
send_nodes([], _Name,  _Req, Monitors) -> 
    Monitors.

%% Starts a monitor, either the new way, or the old.
%% Assumes that the arguments are atoms.
start_monitor(Node, Name) ->
    if node() =:= nonode@nohost, Node =/= nonode@nohost ->
	    Ref = make_ref(),
	    self() ! {'DOWN', Ref, process, {Name, Node}, noconnection},
	    {Node, Ref};
       true ->
	    {Node,erlang:monitor(process, {Name, Node})}
    end.

%% Call apply(M,F,A) on all nodes in parallel
-doc """
Equivalent to
[`multicall([node()|nodes()], Module, Function, Args, infinity)`](`multicall/5`).
""".
-spec multicall(Module, Function, Args) -> {ResL, BadNodes} when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      ResL :: [Res :: term() | {'badrpc', Reason :: term()}],
      BadNodes :: [node()].

multicall(M, F, A) -> 
    multicall(M, F, A, infinity).


-doc """
Equivalent to
[`multicall(Nodes, Module, Function, Args, infinity)`](`multicall/5`).

Equivalent to
[`multicall([node()|nodes()], Module, Function, Args, Timeout)`](`multicall/5`).
""".
-spec multicall(Nodes, Module, Function, Args) -> {ResL, BadNodes} when
                  Nodes :: [node()],
                  Module :: module(),
                  Function :: atom(),
                  Args :: [term()],
                  ResL :: [Res :: term() | {'badrpc', Reason :: term()}],
                  BadNodes :: [node()];
               (Module, Function, Args, Timeout) -> {ResL, BadNodes} when
                  Module :: module(),
                  Function :: atom(),
                  Args :: [term()],
                  Timeout :: ?TIMEOUT_TYPE,
                  ResL :: [Res :: term() | {'badrpc', Reason :: term()}],
                  BadNodes :: [node()].

multicall(Nodes, M, F, A) when is_list(Nodes) ->
    multicall(Nodes, M, F, A, infinity);
multicall(M, F, A, Timeout) ->
    multicall([node() | nodes()], M, F, A, Timeout).

-doc """
In contrast to an RPC, a multicall is an RPC that is sent concurrently from one
client to multiple servers. This is useful for collecting information from a set
of nodes, or for calling a function on a set of nodes to achieve some side
effects. It is semantically the same as iteratively making a series of RPCs on
all the nodes, but the multicall is faster, as all the requests are sent at the
same time and are collected one by one as they come back.

The function evaluates [`apply(Module, Function, Args)`](`apply/3`) on the
specified nodes and collects the answers. It returns `{ResL, BadNodes}`, where
`BadNodes` is a list of the nodes that do not exist, and `ResL` is a list of the
return values, or `{badrpc, Reason}` for failing calls. `Timeout` is a time
(integer) in milliseconds, or `infinity`.

The following example is useful when new object code is to be loaded on all
nodes in the network, and indicates some side effects that RPCs can produce:

```erlang
%% Find object code for module Mod
{Mod, Bin, File} = code:get_object_code(Mod),

%% and load it on all nodes including this one
{ResL, _} = rpc:multicall(code, load_binary, [Mod, File, Bin]),

%% and then maybe check the ResL list.
```

> #### Note {: .info }
>
> If you want the ability to distinguish between results, you may want to
> consider using the [`erpc:multicall()`](`erpc:multicall/4`) function from the
> `erpc` module instead.

> #### Note {: .info }
>
> You cannot make _any_ assumptions about the process that will perform the
> `apply()`. It may be the calling process itself, an `rpc` server, another
> server, or a freshly spawned process.
""".
-spec multicall(Nodes, Module, Function, Args, Timeout) ->
                       {ResL, BadNodes} when
      Nodes :: [node()],
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Timeout :: ?TIMEOUT_TYPE,
      ResL :: [Res :: term() | {'badrpc', Reason :: term()}],
      BadNodes :: [node()].

multicall(Nodes, M, F, A, Timeout) ->
    %%
    %% We want to use erpc:multicall() and then convert the result
    %% instead of using erpc:send_request()/erpc:receive_response()
    %% directly. This since erpc:multicall() is able to utilize the
    %% selective receive optimization when all clauses match on the
    %% same reference. erpc:send_request()/erpc:receive_response()
    %% is not able to utilize such optimizations.
    %%
    ERpcRes = try
                  erpc:multicall(Nodes, M, F, A, Timeout)
              catch
                  error:{erpc, badarg} ->
                      error(badarg)
              end,
    rpcmulticallify(Nodes, ERpcRes, [], []).


rpcmulticallify([], [], Ok, Err) ->
    {lists:reverse(Ok), lists:reverse(Err)};
rpcmulticallify([_N|Ns], [{ok, {'EXIT', _} = Exit}|Rlts], Ok, Err) ->
    rpcmulticallify(Ns, Rlts, [{badrpc, Exit}|Ok], Err);
rpcmulticallify([_N|Ns], [{ok, Return}|Rlts], Ok, Err) ->
    rpcmulticallify(Ns, Rlts, [Return|Ok], Err);
rpcmulticallify([N|Ns], [{error, {erpc, Reason}}|Rlts], Ok, Err)
  when Reason == timeout; Reason == noconnection ->
    rpcmulticallify(Ns, Rlts, Ok, [N|Err]);
rpcmulticallify([_N|Ns], [{Class, Reason}|Rlts], Ok, Err) ->
    rpcmulticallify(Ns, Rlts, [rpcify_exception(Class, Reason)|Ok], Err).

%% Send Msg to Name on all nodes, and collect the answers.
%% Return {Replies, Badnodes} where Badnodes is a list of the nodes
%% that failed during the timespan of the call.
%% This function assumes that if we send a request to a server 
%% called Name, the server will reply with a reply
%% of the form {Name, Node, Reply}, otherwise this function will
%% hang forever. 
%% It also assumes that the server receives messages on the form
%% {From, Msg} and then replies as From ! {Name, node(), Reply}.
%%
%% There is no apparent order among the replies.

-doc """
Equivalent to
[`multi_server_call([node()|nodes()], Name, Msg)`](`multi_server_call/3`).
""".
-spec multi_server_call(Name, Msg) -> {Replies, BadNodes} when
      Name :: atom(),
      Msg :: term(),
      Replies :: [Reply :: term()],
      BadNodes :: [node()].

multi_server_call(Name, Msg) ->
    multi_server_call([node() | nodes()], Name, Msg).

-doc """
Can be used when interacting with servers called `Name` on the specified nodes.
It is assumed that the servers receive messages in the format `{From, Msg}` and
reply using `From ! {Name, Node, Reply}`, where `Node` is the name of the node
where the server is located. The function returns `{Replies, BadNodes}`, where
`Replies` is a list of all `Reply` values, and `BadNodes` is one of the
following:

- A list of the nodes that do not exist
- A list of the nodes where the server does not exist
- A list of the nodes where the server terminated before sending any reply.
""".
-spec multi_server_call(Nodes, Name, Msg) -> {Replies, BadNodes} when
      Nodes :: [node()],
      Name :: atom(),
      Msg :: term(),
      Replies :: [Reply :: term()],
      BadNodes :: [node()].

multi_server_call(Nodes, Name, Msg) 
  when is_list(Nodes), is_atom(Name) ->
    Monitors = send_nodes(Nodes, Name, Msg, []),
    rec_nodes(Name, Monitors).


rec_nodes(Name, Nodes) -> 
    rec_nodes(Name, Nodes, [], []).

rec_nodes(_Name, [],  Badnodes, Replies) ->
    {Replies, Badnodes};
rec_nodes(Name, [{N,R} | Tail], Badnodes, Replies) ->
    receive
	{'DOWN', R, _, _, _} ->
	    rec_nodes(Name, Tail, [N|Badnodes], Replies);
	{?NAME, N, {nonexisting_name, _}} ->  
	    %% used by sbcast()
	    erlang:demonitor(R, [flush]),
	    rec_nodes(Name, Tail, [N|Badnodes], Replies);
	{Name, N, Reply} ->  %% Name is bound !!!
	    erlang:demonitor(R, [flush]),
	    rec_nodes(Name, Tail, Badnodes, [Reply|Replies])
    end.

%% Now for an asynchronous rpc.
%% An asynchronous version of rpc that is faster for series of
%% rpc's towards the same node. I.e. it returns immediately and 
%% it returns a Key that can be used in a subsequent yield(Key).

-doc "Opaque value returned by `async_call/4`.".
-opaque key() :: erpc:request_id().

-doc """
Implements _call streams with promises_, a type of RPC that does not suspend the
caller until the result is finished. Instead, a key is returned, which can be
used later to collect the value. The key can be viewed as a promise to deliver
the answer.

In this case, the key `Key` is returned, which can be used in a subsequent call
to `yield/1` or [`nb_yield/1,2`](`nb_yield/1`) to retrieve the value of
evaluating [`apply(Module, Function, Args)`](`apply/3`) on node `Node`.

> #### Note {: .info }
>
> If you want the ability to distinguish between results, you may want to
> consider using the [`erpc:send_request()`](`erpc:send_request/4`) function
> from the `erpc` module instead. This also gives you the ability retrieve the
> results in other useful ways.

> #### Note {: .info }
>
> `yield/1` and [`nb_yield/1,2`](`nb_yield/1`) must be called by the same
> process from which this function was made otherwise they will never yield
> correctly.

> #### Note {: .info }
>
> You cannot make _any_ assumptions about the process that will perform the
> `apply()`. It may be an `rpc` server, another server, or a freshly spawned
> process.
""".
-spec async_call(Node, Module, Function, Args) -> Key when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Key :: key().

async_call(Node, Mod, Fun, Args) ->
    try
        erpc:send_request(Node, Mod, Fun, Args)
    catch
        error:{erpc, badarg} ->
            error(badarg)
    end.

-doc """
Returns the promised answer from a previous `async_call/4`. If the answer is
available, it is returned immediately. Otherwise, the calling process is
suspended until the answer arrives from `Node`.

> #### Note {: .info }
>
> This function must be called by the same process from which `async_call/4` was
> made otherwise it will never return.

See the note in `call/4` for more details of the return value.
""".
-spec yield(Key) -> Res | {badrpc, Reason} when
      Key :: key(),
      Res :: term(),
      Reason :: term().

yield(Key) ->
    ?RPCIFY(erpc:receive_response(Key)).

-doc """
Non-blocking version of `yield/1`. It returns the tuple `{value, Val}` when the
computation is finished, or `timeout` when `Timeout` milliseconds has elapsed.

See the note in `call/4` for more details of Val.

> #### Note {: .info }
>
> This function must be called by the same process from which `async_call/4` was
> made otherwise it will only return `timeout`.
""".
-spec nb_yield(Key, Timeout) -> {value, Val} | timeout when
      Key :: key(),
      Timeout :: ?TIMEOUT_TYPE,
      Val :: (Res :: term()) | {badrpc, Reason :: term()}.

nb_yield(Key, Tmo) ->
    try erpc:wait_response(Key, Tmo) of
        no_response ->
            timeout;
        {response, {'EXIT', _} = BadRpc} ->
            {value, {badrpc, BadRpc}};
        {response, R} ->
            {value, R}
    catch
        Class:Reason ->
            {value, rpcify_exception(Class, Reason)}
    end.

-doc "Equivalent to [`nb_yield(Key, 0)`](`nb_yield/2`).".
-spec nb_yield(Key) -> {value, Val} | timeout when
      Key :: key(),
      Val :: (Res :: term()) | {badrpc, Reason :: term()}.

nb_yield(Key) ->
    nb_yield(Key, 0).

%% A parallel network evaluator
%% ArgL === [{M,F,Args},........]
%% Returns a lists of the evaluations in the same order as 
%% given to ArgL
-doc """
Evaluates, for every tuple in `FuncCalls`,
[`apply(Module, Function, Args)`](`apply/3`) on some node in the network.
Returns the list of return values, in the same order as in `FuncCalls`.
""".
-spec parallel_eval(FuncCalls) -> ResL when
      FuncCalls :: [{Module, Function, Args}],
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      ResL :: [term()].

parallel_eval(ArgL) ->
    Nodes = [node() | nodes()],
    Keys = map_nodes(ArgL,Nodes,Nodes),
    [yield(K) || K <- Keys].

map_nodes([],_,_) -> [];
map_nodes(ArgL,[],Original) ->
    map_nodes(ArgL,Original,Original); 
map_nodes([{M,F,A}|Tail],[Node|MoreNodes], Original) ->
    [?MODULE:async_call(Node,M,F,A) | 
     map_nodes(Tail,MoreNodes,Original)].

%% Parallel version of lists:map/3 with exactly the same 
%% arguments and return value as lists:map/3,
%% except that it calls exit/1 if a network error occurs.
-doc """
Evaluates [`apply(Module, Function, [Elem|ExtraArgs])`](`apply/3`) for every
element `Elem` in `List1`, in parallel. Returns the list of return values, in
the same order as in `List1`.
""".
-spec pmap(FuncSpec, ExtraArgs, List1) -> List2 when
      FuncSpec :: {Module,Function},
      Module :: module(),
      Function :: atom(),
      ExtraArgs :: [term()],
      List1 :: [Elem :: term()],
      List2 :: [term()].

pmap({M,F}, As, List) ->
    check(parallel_eval(build_args(M,F,As, List, [])), []).

%% By using an accumulator twice we get the whole thing right
build_args(M,F, As, [Arg|Tail], Acc) ->
    build_args(M,F, As, Tail, [{M,F,[Arg|As]}|Acc]);
build_args(M,F, _, [], Acc) when is_atom(M), is_atom(F) -> Acc.

%% If one single call fails, we fail the whole computation
check([{badrpc, _}|_], _) -> exit(badrpc);
check([X|T], Ack) -> check(T, [X|Ack]);
check([], Ack) -> Ack.


%% location transparent version of process_info
-doc "Location transparent version of the BIF `erlang:process_info/1` in ERTS.".
-spec pinfo(Pid) -> [{Item, Info}] | undefined when
      Pid :: pid(),
      Item :: atom(),
      Info :: term().

pinfo(Pid) when node(Pid) =:= node() ->
    process_info(Pid);
pinfo(Pid) ->
    call(node(Pid), erlang, process_info, [Pid]).

-doc "Location transparent version of the BIF `erlang:process_info/2` in ERTS.".
-spec pinfo(Pid, Item) -> {Item, Info} | undefined | [] when
      Pid :: pid(),
      Item :: atom(),
      Info :: term();
           (Pid, ItemList) -> [{Item, Info}] | undefined | [] when
      Pid :: pid(),
      Item :: atom(),
      ItemList :: [Item],
      Info :: term().

pinfo(Pid, Item) when node(Pid) =:= node() ->
    process_info(Pid, Item);
pinfo(Pid, Item) ->
    block_call(node(Pid), erlang, process_info, [Pid, Item]).

%% The following functions with the cnode_call_group_leader_ prefix
%% are used for RPC requests with the group leader field set to
%% send_stdout_to_caller. The group leader that these functions
%% implement sends back data that are written to stdout during the
%% call. The group leader implementation is heavily inspired by the
%% example from the documentation of "The Erlang I/O Protocol".


%% A record is used for the state even though it consists of only one
%% pid to make future extension easier
-record(cnode_call_group_leader_state,
        {
         caller_pid :: pid()
        }).

-spec cnode_call_group_leader_loop(State :: #cnode_call_group_leader_state{}) -> ok | no_return().

cnode_call_group_leader_loop(State) ->
    receive
	{io_request, From, ReplyAs, Request} ->
	    {_, Reply, NewState}
                = cnode_call_group_leader_request(Request, State),
            From ! {io_reply, ReplyAs, Reply},
            cnode_call_group_leader_loop(NewState);
        {stop, StopRequesterPid, Ref, To, Reply} ->
            reply(To, Reply),
            StopRequesterPid ! Ref,
            ok;
	_Unknown ->
            cnode_call_group_leader_loop(State)
    end.

-spec cnode_call_group_leader_request(Request, State) -> Result when
      Request :: any(),
      State :: #cnode_call_group_leader_state{},
      Result :: {ok | error, Reply, NewState},
      Reply :: term(),
      NewState :: #cnode_call_group_leader_state{}.

cnode_call_group_leader_request({put_chars, Encoding, Chars},
                                State) ->
    cnode_call_group_leader_put_chars(Chars, Encoding, State);
cnode_call_group_leader_request({put_chars, Encoding, Module, Function, Args},
                                State) ->
    try
	cnode_call_group_leader_request({put_chars,
                                         Encoding,
                                         apply(Module, Function, Args)},
                                        State)
    catch
	_:_ ->
	    {error, {error, Function}, State}
    end;
cnode_call_group_leader_request({requests, Reqs}, State) ->
     cnode_call_group_leader_multi_request(Reqs, {ok, ok, State});
cnode_call_group_leader_request({get_until, _, _, _, _, _}, State) ->
    {error, {error,enotsup}, State};
cnode_call_group_leader_request({get_chars, _, _, _}, State) ->
    {error, {error,enotsup}, State};
cnode_call_group_leader_request({get_line, _, _}, State) ->
    {error, {error,enotsup}, State};
cnode_call_group_leader_request({get_geometry,_}, State) ->
    {error, {error,enotsup}, State};
cnode_call_group_leader_request({setopts, _Opts}, State) ->
    {error, {error,enotsup}, State};
cnode_call_group_leader_request(getopts, State) ->
    {error, {error,enotsup}, State};
cnode_call_group_leader_request(_Other, State) ->
    {error, {error,request}, State}.

-spec cnode_call_group_leader_multi_request(Requests, PrevResponse) -> Result when
      Requests :: list(),
      PrevResponse :: {ok | error, Reply, State :: #cnode_call_group_leader_state{}},
      Result :: {ok | error, Reply, NewState :: #cnode_call_group_leader_state{}},
      Reply :: term().

cnode_call_group_leader_multi_request([R|Rs], {ok, _Res, State}) ->
    cnode_call_group_leader_multi_request(Rs, cnode_call_group_leader_request(R, State));
cnode_call_group_leader_multi_request([_|_], Error) ->
    Error;
cnode_call_group_leader_multi_request([], Result) ->
    Result.

-spec cnode_call_group_leader_put_chars(Chars, Encoding, State) -> Result when
      Chars :: unicode:latin1_chardata() | unicode:chardata() | unicode:external_chardata(),
      Encoding :: unicode:encoding(),
      State :: #cnode_call_group_leader_state{},
      Result :: {ok | error, term(), NewState},
      NewState :: #cnode_call_group_leader_state{}.

cnode_call_group_leader_put_chars(Chars, Encoding, State) ->
    CNodePid = State#cnode_call_group_leader_state.caller_pid,
    case unicode:characters_to_binary(Chars,Encoding,utf8) of
        Data when is_binary(Data) ->
            CNodePid ! {rex_stdout, Data},
            {ok, ok, State};
        Error ->
            {error, {error, Error}, state}
    end.

-spec cnode_call_group_leader_init(CallerPid :: pid()) -> ok | no_return().

cnode_call_group_leader_init(CallerPid) ->
    State = #cnode_call_group_leader_state{caller_pid = CallerPid},
    cnode_call_group_leader_loop(State).

-spec cnode_call_group_leader_start(CallerPid :: pid()) -> pid().

cnode_call_group_leader_start(CallerPid) ->
    spawn_link(fun() -> cnode_call_group_leader_init(CallerPid) end).
