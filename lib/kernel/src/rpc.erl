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

%% General rpc, broadcast,multicall, promise and parallel evaluator
%% facility

%% This code used to reside in net.erl, but has now been moved to
%% a separate module.

-define(NAME, rex).

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

%%------------------------------------------------------------------------

-type state() :: map().

%%------------------------------------------------------------------------


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
    {ok, maps:new()}.

-spec handle_call(term(), term(), state()) ->
        {'noreply', state()} |
	{'reply', term(), state()} |
	{'stop', 'normal', 'stopped', state()}.

handle_call({call, Mod, Fun, Args, Gleader}, To, S) ->
    handle_call_call(Mod, Fun, Args, Gleader, To, S);
handle_call({block_call, Mod, Fun, Args, Gleader}, _To, S) ->
    MyGL = group_leader(),
    set_group_leader(Gleader),
    Reply = 
	case catch apply(Mod,Fun,Args) of
	    {'EXIT', _} = Exit ->
		{badrpc, Exit};
	    Other ->
		Other
	end,
    group_leader(MyGL, self()), % restore
    {reply, Reply, S};
handle_call(stop, _To, S) ->
    {stop, normal, stopped, S};
handle_call(_, _To, S) ->
    {noreply, S}.  % Ignore !

-spec handle_cast(term(), state()) -> {'noreply', state()}.

handle_cast({cast, Mod, Fun, Args, Gleader}, S) ->
    spawn(fun() ->
		  set_group_leader(Gleader),
		  apply(Mod, Fun, Args)
	  end),
    {noreply, S};
handle_cast(_, S) ->
    {noreply, S}.  % Ignore !

-spec handle_info(term(), state()) -> {'noreply', state()}.

handle_info({'DOWN', _, process, Caller, normal}, S) ->
    {noreply, maps:remove(Caller, S)};
handle_info({'DOWN', _, process, Caller, Reason}, S) ->
    case maps:get(Caller, S, undefined) of
	undefined ->
	    {noreply, S};
	{_, _} = To ->
	    gen_server:reply(To, {badrpc, {'EXIT', Reason}}),
	    {noreply, maps:remove(Caller, S)}
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
handle_info({From, {call,Mod,Fun,Args,Gleader}}, S) ->
    %% Special for hidden C node's, uugh ...
    handle_call_call(Mod, Fun, Args, Gleader, {From,?NAME}, S);
handle_info(_, S) ->
    {noreply, S}.

-spec terminate(term(), state()) -> 'ok'.

terminate(_, _S) ->
    ok.

-spec code_change(term(), state(), term()) -> {'ok', state()}.

code_change(_, S, _) ->
    {ok, S}.

%%
%% Auxiliary function to avoid a false dialyzer warning -- do not inline
%%
handle_call_call(Mod, Fun, Args, Gleader, To, S) ->
    %% Spawn not to block the rpc server.
    {Caller,_} =
	erlang:spawn_monitor(
	  fun () ->
		  set_group_leader(Gleader),
		  Reply = 
		      %% in case some sucker rex'es 
		      %% something that throws
		      case catch apply(Mod, Fun, Args) of
			  {'EXIT', _} = Exit ->
			      {badrpc, Exit};
			  Result ->
			      Result
		      end,
		  gen_server:reply(To, Reply)
	  end),
    {noreply, maps:put(Caller, To, S)}.


%% RPC aid functions ....

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


%% THE rpc client interface

-spec call(Node, Module, Function, Args) -> Res | {badrpc, Reason} when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Res :: term(),
      Reason :: term().

call(N,M,F,A) when node() =:= N ->  %% Optimize local call
    local_call(M, F, A);
call(N,M,F,A) ->
    do_call(N, {call,M,F,A,group_leader()}, infinity).

-spec call(Node, Module, Function, Args, Timeout) ->
                  Res | {badrpc, Reason} when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Res :: term(),
      Reason :: term(),
      Timeout :: timeout().

call(N,M,F,A,infinity) when node() =:= N ->  %% Optimize local call
    local_call(M,F,A);
call(N,M,F,A,infinity) ->
    do_call(N, {call,M,F,A,group_leader()}, infinity);
call(N,M,F,A,Timeout) when is_integer(Timeout), Timeout >= 0 ->
    do_call(N, {call,M,F,A,group_leader()}, Timeout).

-spec block_call(Node, Module, Function, Args) -> Res | {badrpc, Reason} when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Res :: term(),
      Reason :: term().

block_call(N,M,F,A) when node() =:= N -> %% Optimize local call
    local_call(M,F,A);
block_call(N,M,F,A) ->
    do_call(N, {block_call,M,F,A,group_leader()}, infinity).

-spec block_call(Node, Module, Function, Args, Timeout) ->
                  Res | {badrpc, Reason} when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Res :: term(),
      Reason :: term(),
      Timeout :: timeout().

block_call(N,M,F,A,_Timeout) when node() =:= N ->  %% Optimize local call
    local_call(M, F, A);
block_call(N,M,F,A,infinity) ->
    do_call(N, {block_call,M,F,A,group_leader()}, infinity);
block_call(N,M,F,A,Timeout) when is_integer(Timeout), Timeout >= 0 ->
    do_call(N, {block_call,M,F,A,group_leader()}, Timeout).

local_call(M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    case catch apply(M, F, A) of
	{'EXIT',_}=V -> {badrpc, V};
	Other -> Other
    end.

do_call(Node, Request, infinity) ->
    rpc_check(catch gen_server:call({?NAME,Node}, Request, infinity));
do_call(Node, Request, Timeout) ->
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

cast(Node, Mod, Fun, Args) when Node =:= node() ->
    catch spawn(Mod, Fun, Args),
    true;
cast(Node, Mod, Fun, Args) ->
    gen_server:cast({?NAME,Node}, {cast,Mod,Fun,Args,group_leader()}),
    true.


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
    gen_server:abcast(Nodes, ?NAME, {cast,Mod,Fun,Args,group_leader()}).


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
                  Timeout :: timeout(),
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
      Timeout :: timeout(),
      ResL :: [Res :: term() | {'badrpc', Reason :: term()}],
      BadNodes :: [node()].

multicall(Nodes, M, F, A, infinity)
  when is_list(Nodes), is_atom(M), is_atom(F), is_list(A) ->
    do_multicall(Nodes, M, F, A, infinity);
multicall(Nodes, M, F, A, Timeout) 
  when is_list(Nodes), is_atom(M), is_atom(F), is_list(A), is_integer(Timeout), 
       Timeout >= 0 ->
    do_multicall(Nodes, M, F, A, Timeout).

do_multicall(Nodes, M, F, A, Timeout) ->
    {Rep,Bad} = gen_server:multi_call(Nodes, ?NAME, 
				      {call, M,F,A, group_leader()}, 
				      Timeout),
    {lists:map(fun({_,R}) -> R end, Rep), Bad}.


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

-opaque key() :: pid().

-spec async_call(Node, Module, Function, Args) -> Key when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Key :: key().

async_call(Node, Mod, Fun, Args) ->
    ReplyTo = self(),
    spawn(
      fun() ->
	      R = call(Node, Mod, Fun, Args),         %% proper rpc
	      ReplyTo ! {self(), {promise_reply, R}}  %% self() is key
      end).

-spec yield(Key) -> Res | {badrpc, Reason} when
      Key :: key(),
      Res :: term(),
      Reason :: term().

yield(Key) when is_pid(Key) ->
    {value,R} = do_yield(Key, infinity),
    R.

-spec nb_yield(Key, Timeout) -> {value, Val} | timeout when
      Key :: key(),
      Timeout :: timeout(),
      Val :: (Res :: term()) | {badrpc, Reason :: term()}.

nb_yield(Key, infinity=Inf) when is_pid(Key) ->
    do_yield(Key, Inf);
nb_yield(Key, Timeout) when is_pid(Key), is_integer(Timeout), Timeout >= 0 ->
    do_yield(Key, Timeout).

-spec nb_yield(Key) -> {value, Val} | timeout when
      Key :: key(),
      Val :: (Res :: term()) | {badrpc, Reason :: term()}.

nb_yield(Key) when is_pid(Key) ->
    do_yield(Key, 0).

-spec do_yield(pid(), timeout()) -> {'value', _} | 'timeout'.

do_yield(Key, Timeout) ->
    receive
        {Key,{promise_reply,R}} ->
            {value,R}
        after Timeout ->
            timeout
    end.


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
