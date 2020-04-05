%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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

%%
%% Implementations inside '-ifdef(SERVER_SIDE_ERPC_IS_MANDATORY).'
%% below require 'erpc' support to be mandatory on server side.
%% 'erpc' was introduced in OTP 23, so it should be possible to
%% enable this as of OTP 25:
%%   -define(SERVER_SIDE_ERPC_IS_MANDATORY, yes).
%%

%% General rpc, broadcast,multicall, promise and parallel evaluator
%% facility

%% This code used to reside in net.erl, but has now been moved to
%% a separate module.

-define(NAME, rex).
-define(TAB_NAME, rex_nodes_observer).

-behaviour(gen_server).

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

-spec start() -> {'ok', pid()} | 'ignore' | {'error', term()}.

start() ->
    gen_server:start({local,?NAME}, ?MODULE, [], ?SPAWN_OPTS).

-spec start_link() -> {'ok', pid()} | 'ignore' | {'error', term()}.

start_link() ->
    %% The rex server process may receive a huge amount of
    %% messages. Make sure that they are stored off heap to
    %% avoid exessive GCs.
    gen_server:start_link({local,?NAME}, ?MODULE, [], ?SPAWN_OPTS).

-spec stop() -> term().

stop() ->
    stop(?NAME).

stop(Rpc) ->
    gen_server:call(Rpc, stop, infinity).

-spec init([]) -> {'ok', state()}.

init([]) ->
    process_flag(trap_exit, true),
    {ok, #{nodes_observer => start_nodes_observer()}}.

-spec handle_call(term(), term(), state()) ->
        {'noreply', state()} |
	{'reply', term(), state()} |
	{'stop', 'normal', 'stopped', state()}.

handle_call({call, Mod, Fun, Args, Gleader}, To, S) ->
    %% Spawn not to block the rex server.
    ExecCall = fun () ->
                       set_group_leader(Gleader),
                       Reply = execute_call(Mod, Fun, Args),
                       gen_server:reply(To, Reply)
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
	    gen_server:reply(To, {badrpc, {'EXIT', Reason}}),
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
handle_info({From, {call, _Mod, _Fun, _Args, _Gleader} = Request}, S) ->
    %% Special for hidden C node's, uugh ...
    To = {From, ?NAME},
    case handle_call(Request, To, S) of
        {noreply, _NewS} = Return ->
            Return;
        {reply, Reply, NewS} ->
            gen_server:reply(To, Reply),
            {noreply, NewS}
    end;
handle_info({From, features_request}, S) ->
    From ! {features_reply, node(), [erpc]},
    {noreply, S};
handle_info(_, S) ->
    {noreply, S}.

-spec terminate(term(), state()) -> 'ok'.

terminate(_, _S) ->
    ok.

-spec code_change(term(), state(), term()) -> {'ok', state()}.

code_change(_, S, _) ->
    {ok, S}.


%% RPC aid functions ....

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

-ifdef(SERVER_SIDE_ERPC_IS_MANDATORY).

%% Currently node_has_feature() is only needed if
%% it is unknown if 'erpc' is supported by server
%% side or not...

-else. %% ! define SERVER_SIDE_ERPC_IS_MANDATORY

-dialyzer([{nowarn_function, node_has_feature/2}, no_match]).

-spec node_has_feature(Node :: atom(), Feature :: term()) -> boolean().

node_has_feature(N, erpc) when N == node() ->
    true;
node_has_feature(N, erpc) ->
    try
        Tab = persistent_term:get(?TAB_NAME),
        ets:lookup_element(Tab, N, 2)
    catch
        _:_ -> false
    end;
node_has_feature(_N, _Feature) ->
    false.

-endif.

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

-spec call(Node, Module, Function, Args) -> Res | {badrpc, Reason} when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Res :: term(),
      Reason :: term().

call(N,M,F,A) ->
    call(N,M,F,A,infinity).

-spec call(Node, Module, Function, Args, Timeout) ->
                  Res | {badrpc, Reason} when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Res :: term(),
      Reason :: term(),
      Timeout :: ?TIMEOUT_TYPE.

-ifdef(SERVER_SIDE_ERPC_IS_MANDATORY).

call(N,M,F,A,T) ->
    ?RPCIFY(erpc:call(N, M, F, A, T)).

-else. %% ! defined SERVER_SIDE_ERPC_IS_MANDATORY

call(N,M,F,A,T) ->
    DL = try
             deadline(T)
         catch
             error:_ ->
                 error(badarg)
         end,
    case ?RPCIFY(erpc:call(N, M, F, A, T)) of
        {badrpc, notsup} ->
            case time_left(DL) of
                0 ->
                    {badrpc, timeout};
                Timeout ->
                    do_srv_call(N, {call,M,F,A,group_leader()}, Timeout)
            end;
        Res ->
            Res
    end.

-endif. %% ! defined SERVER_SIDE_ERPC_IS_MANDATORY

-spec block_call(Node, Module, Function, Args) -> Res | {badrpc, Reason} when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Res :: term(),
      Reason :: term().

block_call(N,M,F,A) ->
    block_call(N,M,F,A,infinity).

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
%% a server called Name at node Node, It is assumed that the server
%% Receives messages on the form {From, Request} and replies on the
%% form From ! {ReplyWrapper, Node, Reply}.
%% This function makes such a server call and ensures that that
%% The entire call is packed into an atomic transaction which 
%% either succeeds or fails, i.e. never hangs (unless the server itself hangs).

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

-spec cast(Node, Module, Function, Args) -> true when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()].

-ifdef(SERVER_SIDE_ERPC_IS_MANDATORY).

cast(Node, Mod, Fun, Args) ->
    try
        ok = erpc:cast(Node, Mod, Fun, Args)
    catch
        error:{erpc, badarg} ->
            error(badarg)
    end,
    true.

-else.

cast(Node, Mod, Fun, Args) when is_atom(Node),
                                is_atom(Mod),
                                is_atom(Fun),
                                is_list(Args) ->
    _ = case node_has_feature(Node, erpc) of
            false ->
                gen_server:cast({?NAME,Node},
                                {cast,Mod,Fun,Args,group_leader()});
            true ->
                try
                    ok = erpc:cast(Node, Mod, Fun, Args)
                catch
                    error:{erpc, badarg} ->
                        error(badarg)
                end
        end,
    true;
cast(_, _, _, _) ->
    error(badarg).


-endif.


%% Asynchronous broadcast, returns nothing, it's just send 'n' pray
-spec abcast(Name, Msg) -> abcast when
      Name :: atom(),
      Msg :: term().

abcast(Name, Mess) ->
    abcast([node() | nodes()], Name, Mess).

-spec abcast(Nodes, Name, Msg) -> abcast when
      Nodes :: [node()],
      Name :: atom(),
      Msg :: term().

abcast([Node|Tail], Name, Mess) ->
    Dest = {Name,Node},
    try erlang:send(Dest, Mess) catch error:_ -> ok end,
    abcast(Tail, Name, Mess);
abcast([], _,_) -> abcast.


%% Syncronous broadcast, returns a list of the nodes which had Name
%% as a registered server. Returns {Goodnodes, Badnodes}.
%% Syncronous in the sense that we know that all servers have received the
%% message when we return from the call, we can't know that they have
%% processed the message though.

-spec sbcast(Name, Msg) -> {GoodNodes, BadNodes} when
      Name :: atom(),
      Msg :: term(),
      GoodNodes :: [node()],
      BadNodes :: [node()].

sbcast(Name, Mess) ->
    sbcast([node() | nodes()], Name, Mess).

-spec sbcast(Nodes, Name, Msg) -> {GoodNodes, BadNodes} when
      Name :: atom(),
      Msg :: term(),
      Nodes :: [node()],
      GoodNodes :: [node()],
      BadNodes :: [node()].

sbcast(Nodes, Name, Mess) ->
    Monitors = send_nodes(Nodes, ?NAME, {sbcast, Name, Mess}, []),
    rec_nodes(?NAME, Monitors).

-spec eval_everywhere(Module, Function, Args) -> abcast when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].

eval_everywhere(Mod, Fun, Args) ->
    eval_everywhere([node() | nodes()] , Mod, Fun, Args).

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
-spec multicall(Module, Function, Args) -> {ResL, BadNodes} when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      ResL :: [Res :: term() | {'badrpc', Reason :: term()}],
      BadNodes :: [node()].

multicall(M, F, A) -> 
    multicall(M, F, A, infinity).


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

-spec multicall(Nodes, Module, Function, Args, Timeout) ->
                       {ResL, BadNodes} when
      Nodes :: [node()],
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Timeout :: ?TIMEOUT_TYPE,
      ResL :: [Res :: term() | {'badrpc', Reason :: term()}],
      BadNodes :: [node()].

-ifdef(SERVER_SIDE_ERPC_IS_MANDATORY).

%%
%% Use this more efficient implementation of multicall()
%% when 'erpc' support can be made mandatory for server
%% side.
%%
multicall(Nodes, M, F, A, Timeout) ->
    %%
    %% We want to use erpc:multicall() and then convert the result
    %% instead of using erpc:send_request()/erpc:receive_response()
    %% directly. This since it is expected that erpc:multicall()
    %% will be able to utilize a future message queue optimization
    %% that erpc:send_request()/erpc:receive_response() most likely
    %% wont be able to (only the future will tell...).
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

-else. %% ! defined SERVER_SIDE_ERPC_IS_MANDATORY

%%
%% Currently used implementation for multicall(). When
%% 'erpc' support can be required for server side,
%% replace with the implementation above...
%% 

multicall(Nodes, M, F, A, Timeout) ->
    try
        true = is_atom(M),
        true = is_atom(F),
        true = is_list(A),
        Deadline = deadline(Timeout),
        Res = make_ref(),
        MFA = {M, F, A},
        {NRs, ReqMap0} = mc_requests(Res, Nodes, M, F, A, [], #{}),
        ReqMap1 = mc_spawn_replies(Res, maps:size(ReqMap0), ReqMap0,
                                   MFA, Deadline),
        mc_results(Res, NRs, [], [], ReqMap1, MFA, Deadline)
    catch
        error:NotIError when NotIError /= internal_error ->
            error(badarg)
    end.

mc_requests(_Res, [], _M, _F, _A, NRs, ReqMap) ->
    {NRs, ReqMap};
mc_requests(Res, [N|Ns], M, F, A, NRs, ReqMap) ->
    ReqId = try
                spawn_request(N, erpc, execute_call,
                              [Res, M, F, A],
                              [{reply_tag, {spawn_reply, Res, N}},
                               monitor])
            catch
                _:_ ->
                    mc_fail_requests(Res, NRs)
            end,
    NR = {N, ReqId},
    mc_requests(Res, Ns, M, F, A, [NR|NRs], ReqMap#{ReqId => spawn_request});
mc_requests(Res, _Error, _M, _F, _A, NRs, _ReqMap) ->
    mc_fail_requests(Res, NRs).

%% Abandon any requests sent then fail...
mc_fail_requests(_Res, []) ->
    error(badarg);
mc_fail_requests(Res, [{Node, ReqId} | NRs]) ->
    case spawn_request_abandon(ReqId) of
        true ->
            ok;
        false ->
            receive
                {{spawn_reply, Res, Node}, ReqId, error, _} ->
                    ok;
                {{spawn_reply, Res, Node}, ReqId, ok, Pid} ->
                    case erlang:demonitor(ReqId, [info]) of
                        true ->
                            ok;
                        false ->
                            receive
                                {'DOWN', ReqId, process, Pid, _} ->
                                    ok
                            after
                                0 ->
                                    error(internal_error)
                            end
                    end
            after
                0 ->
                    error(internal_error)
            end
    end,
    mc_fail_requests(Res, NRs).

mc_spawn_replies(_Res, 0, ReqMap, _MFA, _Deadline) ->
    ReqMap;
mc_spawn_replies(Res, Outstanding, ReqMap, MFA, Deadline) ->
    Timeout = time_left(Deadline),
    receive
        {{spawn_reply, Res, _}, _, _, _} = Reply ->
            NewReqMap = mc_handle_spawn_reply(Reply, ReqMap, MFA, Deadline),
            mc_spawn_replies(Res, Outstanding-1, NewReqMap, MFA, Deadline)
    after
        Timeout ->
            ReqMap
    end.

mc_handle_spawn_reply({{spawn_reply, _Res, _Node}, ReqId, ok, Pid},
                      ReqMap, _MFA, _Deadline) ->
    ReqMap#{ReqId => {spawn, Pid}};
mc_handle_spawn_reply({{spawn_reply, _Res, Node}, ReqId, error, notsup},
                      ReqMap, MFA, infinity) ->
    {M, F, A} = MFA,
    SrvReqId = gen_server:send_request({?NAME, Node},
                                       {call, M,F,A,
                                        group_leader()}),
    ReqMap#{ReqId => {server, SrvReqId}};
mc_handle_spawn_reply({{spawn_reply, Res, Node}, ReqId, error, notsup},
                      ReqMap, MFA, Deadline) ->
    {M, F, A} = MFA,
    try
        {Pid, Mon} = spawn_monitor(fun () ->
                                           process_flag(trap_exit, true),
                                           Request = {call, M,F,A,
                                                      group_leader()},
                                           Timeout = time_left(Deadline),
                                           Result = gen_server:call({?NAME,
                                                                     Node},
                                                                    Request,
                                                                    Timeout),
                                           exit({Res, Result})
                                   end),
        ReqMap#{ReqId => {spawn_server, Mon, Pid}}
    catch
        error:system_limit ->
            ReqMap#{ReqId => {error, {badrpc, {'EXIT', system_limit}}}}
    end;
mc_handle_spawn_reply({{spawn_reply, _Res, _Node}, ReqId, error, noconnection},
                      ReqMap, _MFA, _Deadline) ->
    ReqMap#{ReqId => {error, badnode}};
mc_handle_spawn_reply({{spawn_reply, _Res, _Node}, ReqId, error, Reason},
                      ReqMap, _MFA, _Deadline) ->
    ReqMap#{ReqId => {error, {badrpc, {'EXIT', Reason}}}}.

mc_results(_Res, [], OkAcc, ErrAcc, _ReqMap, _MFA, _Deadline) ->
    {OkAcc, ErrAcc};
mc_results(Res, [{N,ReqId}|NRs] = OrigNRs, OkAcc, ErrAcc,
           ReqMap, MFA, Deadline) ->
    case maps:get(ReqId, ReqMap) of
        {error, badnode} ->
            mc_results(Res, NRs, OkAcc, [N|ErrAcc], ReqMap, MFA, Deadline);
        {error, BadRpc} ->
            mc_results(Res, NRs, [BadRpc|OkAcc], ErrAcc, ReqMap,
                       MFA, Deadline);
        spawn_request ->
            %% We timed out waiting for spawn replies...
            case spawn_request_abandon(ReqId) of
                true ->
                    %% Timed out request...
                    mc_results(Res, NRs, OkAcc, [N|ErrAcc], ReqMap,
                               MFA, Deadline);
                false ->
                    %% Reply has been delivered now; handle it...
                    receive
                        {{spawn_reply, Res, _}, ReqId, _, _} = Reply ->
                            NewReqMap = mc_handle_spawn_reply(Reply, ReqMap,
                                                              MFA, Deadline),
                            mc_results(Res, OrigNRs, OkAcc, ErrAcc,
                                       NewReqMap, MFA, Deadline)
                    after 0 ->
                            error(internal_error)
                    end
            end;
        {spawn, Pid} ->
            Timeout = time_left(Deadline),
            receive
                {'DOWN', ReqId, process, Pid, Reason} ->
                    case ?RPCIFY(erpc:call_result(down, ReqId, Res, Reason)) of
                        {badrpc, nodedown} ->
                            mc_results(Res, NRs, OkAcc, [N|ErrAcc],
                                       ReqMap, MFA, Deadline);
                        CallRes ->
                            mc_results(Res, NRs, [CallRes|OkAcc],
                                       ErrAcc, ReqMap, MFA, Deadline)
                    end
            after
                Timeout ->
                    case erlang:demonitor(ReqId, [info]) of
                        true ->
                            mc_results(Res, NRs, OkAcc, [N|ErrAcc],
                                       ReqMap, MFA, Deadline);
                        false ->
                            receive
                                {'DOWN', ReqId, process, Pid, Reason} ->
                                    case ?RPCIFY(erpc:call_result(down,
                                                                  ReqId,
                                                                  Res,
                                                                  Reason)) of
                                        {badrpc, nodedown} ->
                                            mc_results(Res, NRs, OkAcc,
                                                       [N|ErrAcc], ReqMap,
                                                       MFA, Deadline);
                                        CallRes ->
                                            mc_results(Res, NRs,
                                                       [CallRes|OkAcc],
                                                       ErrAcc, ReqMap,
                                                       MFA, Deadline)
                                    end
                            after 0 ->
                                    error(internal_error)
                            end
                    end
            end;
        {spawn_server, Mon, Pid} ->
            %% Old node with timeout on the call...
            Result = receive
                         {'DOWN', Mon, process, Pid, {Res, CallRes}} ->
                             rpc_check(CallRes);
                         {'DOWN', Mon, process, Pid, Reason} ->
                             rpc_check_t({'EXIT',Reason})
                     end,
            case Result of
                {badrpc, BadRpcReason} when BadRpcReason == timeout;
                                            BadRpcReason == nodedown ->
                    mc_results(Res, NRs, OkAcc, [N|ErrAcc],
                               ReqMap, MFA, Deadline);
                _ ->
                    mc_results(Res, NRs, [Result|OkAcc], ErrAcc, ReqMap,
                               MFA, Deadline)
            end;
        {server, SrvReqId} ->
            %% Old node without timeout on the call...
            case gen_server:wait_response(SrvReqId, infinity) of
                {reply, Reply} ->
                    Result = rpc_check(Reply),
                    mc_results(Res, NRs, [Result|OkAcc], ErrAcc,
                               ReqMap, MFA, Deadline);
                {error, {noconnection, _}} ->
                    mc_results(Res, NRs, OkAcc, [N|ErrAcc],
                               ReqMap, MFA, Deadline);
                {error, {Reason, _}} ->
                    BadRpc = {badrpc, {'EXIT', Reason}},
                    mc_results(Res, NRs, [BadRpc|OkAcc], ErrAcc,
                               ReqMap, MFA, Deadline)
            end
    end.

deadline(infinity) ->
    infinity;
deadline(?MAX_INT_TIMEOUT) ->
    erlang:convert_time_unit(erlang:monotonic_time(millisecond)
                             + ?MAX_INT_TIMEOUT,
                             millisecond,
                             native);
deadline(T) when ?IS_VALID_TMO_INT(T) ->
    Now = erlang:monotonic_time(),
    NativeTmo = erlang:convert_time_unit(T, millisecond, native),
    Now + NativeTmo.

time_left(infinity) ->
    infinity;
time_left(Deadline) ->
    case Deadline - erlang:monotonic_time() of
        TimeLeft when TimeLeft =< 0 ->
            0;
        TimeLeft ->
            erlang:convert_time_unit(TimeLeft-1, native, millisecond) + 1
    end.

-endif. %% ! defined SERVER_SIDE_ERPC_IS_MANDATORY

%% Send Msg to Name on all nodes, and collect the answers.
%% Return {Replies, Badnodes} where Badnodes is a list of the nodes
%% that failed during the timespan of the call.
%% This function assumes that if we send a request to a server 
%% called Name, the server will reply with a reply
%% on the form {Name, Node, Reply}, otherwise this function will
%% hang forever. 
%% It also assumes that the server receives messages on the form
%% {From, Msg} and then replies as From ! {Name, node(), Reply}.
%%
%% There is no apparent order among the replies.

-spec multi_server_call(Name, Msg) -> {Replies, BadNodes} when
      Name :: atom(),
      Msg :: term(),
      Replies :: [Reply :: term()],
      BadNodes :: [node()].

multi_server_call(Name, Msg) ->
    multi_server_call([node() | nodes()], Name, Msg).

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
%% An asyncronous version of rpc that is faster for series of
%% rpc's towards the same node. I.e. it returns immediately and 
%% it returns a Key that can be used in a subsequent yield(Key).

-ifdef(SERVER_SIDE_ERPC_IS_MANDATORY).

%%
%% Use this more efficient implementation of async_call()
%% when 'erpc' support can be made mandatory for server
%% side.
%%

-opaque key() :: erpc:request_id().

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

-spec yield(Key) -> Res | {badrpc, Reason} when
      Key :: key(),
      Res :: term(),
      Reason :: term().

yield(Key) ->
    ?RPCIFY(erpc:receive_response(Key)).

-spec nb_yield(Key, Timeout) -> {value, Val} | timeout when
      Key :: key(),
      Timeout :: ?TIMEOUT_TYPE,
      Val :: (Res :: term()) | {badrpc, Reason :: term()}.

nb_yield(Key, Tmo) ->
    case ?RPCIFY(erpc:wait_response(Key, Tmo)) of
        no_response ->
            timeout;
        {response, {'EXIT', _} = BadRpc} ->
            %% RPCIFY() cannot handle this case...
            {value, {badrpc, BadRpc}};
        {response, R} ->
            {value, R};
        BadRpc ->
            %% An exception converted by RPCIFY()...
            {value, BadRpc}
    end.

-spec nb_yield(Key) -> {value, Val} | timeout when
      Key :: key(),
      Val :: (Res :: term()) | {badrpc, Reason :: term()}.

nb_yield(Key) ->
    nb_yield(Key, 0).

-else. %% ! defined SERVER_SIDE_ERPC_IS_MANDATORY

%%
%% Currently used implementation for async_call(). When
%% 'erpc' support can be required for server side,
%% replace with the implementation above...
%% 

-opaque key() :: {pid(), reference()}.

-spec async_call(Node, Module, Function, Args) -> Key when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Key :: key().

async_call(Node, Mod, Fun, Args) ->
    try
        true = is_atom(Node),
        true = is_atom(Mod),
        true = is_atom(Fun),
        true = is_integer(length(Args))
    catch
        _:_ ->
            error(badarg)
    end,
    Caller = self(),
    spawn_monitor(fun() ->
                          process_flag(trap_exit, true),
                          R = call(Node, Mod, Fun, Args),
                          exit({async_call_result, Caller, R})
                  end).

-spec yield(Key) -> Res | {badrpc, Reason} when
      Key :: key(),
      Res :: term(),
      Reason :: term().

yield({Pid, Ref} = Key) when is_pid(Pid),
                             is_reference(Ref) ->
    {value,R} = nb_yield(Key, infinity),
    R.

-spec nb_yield(Key) -> {value, Val} | timeout when
      Key :: key(),
      Val :: (Res :: term()) | {badrpc, Reason :: term()}.

nb_yield({Pid, Ref} = Key) when is_pid(Pid),
                                is_reference(Ref) ->
    nb_yield(Key, 0).

-spec nb_yield(Key, Timeout) -> {value, Val} | timeout when
      Key :: key(),
      Timeout :: ?TIMEOUT_TYPE,
      Val :: (Res :: term()) | {badrpc, Reason :: term()}.

nb_yield({Proxy, Mon}, Tmo) when is_pid(Proxy),
                                 is_reference(Mon),
                                 ?IS_VALID_TMO(Tmo) ->
    Me = self(),
    receive
        {'DOWN', Mon, process, Proxy, {async_call_result, Me, R}} ->
            {value,R};
        {'DOWN', Mon, process, Proxy, Reason} ->
            {value, {badrpc, {'EXIT', Reason}}}
    after Tmo ->
            timeout
    end.

-endif. %% ! defined SERVER_SIDE_ERPC_IS_MANDATORY

%% A parallel network evaluator
%% ArgL === [{M,F,Args},........]
%% Returns a lists of the evaluations in the same order as 
%% given to ArgL
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
-spec pinfo(Pid) -> [{Item, Info}] | undefined when
      Pid :: pid(),
      Item :: atom(),
      Info :: term().

pinfo(Pid) when node(Pid) =:= node() ->
    process_info(Pid);
pinfo(Pid) ->
    call(node(Pid), erlang, process_info, [Pid]).

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
