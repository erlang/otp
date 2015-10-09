%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: global.erl,v 1.4 2009/09/17 09:46:19 kostis Exp $
%%
-module(global).
-behaviour(gen_server).

%%  A Global register that allows the global registration of pid's and
%% name's, that dynamically keeps up to date with the entire network.
%% global can operate in two modes; in a fully connected network, or
%% in a non-fully connected network.  In the latter case, the name
%% registration mechanism won't work.
%%

%% External exports
-export([start/0, start_link/0, stop/0, sync/0, sync/1,
	 safe_whereis_name/1, whereis_name/1,  register_name/2, register_name/3,
	 register_name_external/2, register_name_external/3, unregister_name_external/1,
	 re_register_name/2, re_register_name/3,
	 unregister_name/1, registered_names/0, send/2, node_disconnected/1,
	 set_lock/1, set_lock/2, set_lock/3,
	 del_lock/1, del_lock/2,
	 trans/2, trans/3, trans/4,
	 random_exit_name/3, random_notify_name/3, notify_all_name/3, cnode/3]).

%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3, timer/2, sync_init/2, init_locker/5, resolve_it/4,
	 init_the_locker/1]).

-export([info/0]).


%-define(PRINT(X), erlang:display(X)).
-define(PRINT(X), true).

%-define(P2(X), erlang:display(X)).
%-define(P2(X), erlang:display({cs(),X})).
-define(P2(X), true).

%-define(P1(X), erlang:display(X)).
-define(P1(X), true).

%-define(P(X), erlang:display(X)).
-define(P(X), true).

%-define(FORMAT(S, A), format(S, A)).
-define(FORMAT(S, A), ok).

%%% In certain places in the server, calling io:format hangs everything,
%%% so we'd better use erlang:display/1.
% format(S, A) ->
%     erlang:display({format, cs(), S, A}),
% %    io:format(S, A),
%     ok.

% cs() ->
%     {Big, Small, Tiny} = now(),
%     (Small rem 100) * 100 + (Tiny div 10000).

%% Some notes on the internal structure:
%% One invariant is that the list of locker processes is keyed; i.e.,
%% there is only one process per neighboring node.
%% When an item has been stored in the process dictionary, it is not
%% necessarily cleared when not in use anymore. In other words, it's
%% not an error if there is already an item there when one is to be
%% stored.


%% This is the protocol version
%% Vsn 1 is the original protocol.
%% Vsn 2 is enhanced with code to take care of registration of names from
%%       non erlang nodes, e.g. c-nodes.
%% Vsn 3 is enhanced with a tag in the synch messages to distinguish
%%       different synch sessions from each other, see OTP-2766.
%%       Note: This requires also that the ticket OTP-2928 is fixed on the nodes
%%             running vsn 1 or 2; if such nodes will coexist with vsn 3 nodes.
%% Vsn 4 uses a single, permanent, locker process, but works like vsn 3
%% when communicating with vsn 3 nodes.

%% -define(vsn, 4).  %% Now given in options

%%-----------------------------------------------------------------
%% connect_all = boolean() - true if we are supposed to set up a
%%                        fully connected net
%% known       = [Node] - all nodes known to us
%% synced      = [Node] - all nodes that have the same names as us
%% lockers     = [{Node, MyLockerPid}] - the pid of the locker
%%                         process for each Node
%% syncers     = [pid()] - all current syncers processes
%% node_name   = atom()  - our node name (can change if distribution
%%                         is started/stopped dynamically)
%%
%% In addition to these, we keep info about messages arrived in
%% the process dictionary:
%% {pre_connect, Node} = {Vsn, InitMsg} - init_connect msgs that
%%                         arrived before nodeup
%% {wait_lock, Node}   = {exchange, NameList} | lock_is_set
%%                        - see comment below (handle_cast)
%% {save_ops, Node}    = [operation()] - save the ops between
%%                          exchange and resolved
%% {prot_vsn, Node}    = Vsn - the exchange protocol version
%% {sync_tag_my, Node} =  My tag, used at synchronization with Node
%% {sync_tag_his, Node} = The Node's tag, used at synchronization
%%-----------------------------------------------------------------
-record(state, {connect_all, known = [], synced = [],
		lockers = [], syncers = [], node_name = node(),
		the_locker, the_deleter}).

start() -> gen_server:start({local, global_name_server}, global, [], []).
start_link() -> gen_server:start_link({local, global_name_server},global,[],[]).
stop() -> gen_server:call(global_name_server, stop, infinity).

sync() ->
    case check_sync_nodes() of
	{error, Error} ->
	    {error, Error};
	SyncNodes ->
	    gen_server:call(global_name_server, {sync, SyncNodes}, infinity)
    end.
sync(Nodes) ->
    case check_sync_nodes(Nodes) of
	{error, Error} ->
	    {error, Error};
	SyncNodes ->
	    gen_server:call(global_name_server, {sync, SyncNodes}, infinity)
    end.


send(Name, Msg) ->
    case whereis_name(Name) of
	Pid when pid(Pid) ->
	    Pid ! Msg,
	    Pid;
	undefined ->
	    exit({badarg, {Name, Msg}})
    end.

%% See OTP-3737. (safe_whereis_name/1 is in fact not used anywhere in OTP.)
whereis_name(Name) ->
    where(Name).

safe_whereis_name(Name) ->
    gen_server:call(global_name_server, {whereis, Name}, infinity).


node_disconnected(Node) ->
    global_name_server ! {nodedown, Node}.


%%-----------------------------------------------------------------
%% Method = function(Name, Pid1, Pid2) -> Pid | Pid2 | none
%% Method is called if a name conflict is detected when two nodes
%% are connecting to each other.  It is supposed to return one of
%% the Pids or 'none'.  If a pid is returned, that pid is
%% registered as Name on all nodes.  If 'none' is returned, the
%% Name is unregistered on all nodes.  If anything else is returned,
%% the Name is unregistered as well.
%% Method is called once at one of the nodes where the processes reside
%% only.  If different Methods are used for the same name, it is
%% undefined which one of them is used.
%% Method is blocking, i.e. when it is called, no calls to whereis/
%% send is let through until it has returned.
%%-----------------------------------------------------------------
register_name(Name, Pid) when pid(Pid) ->
    register_name(Name, Pid, {global, random_exit_name}).
register_name(Name, Pid, Method) when pid(Pid) ->
    trans_all_known(fun(Nodes) ->
		  case where(Name) of
		      undefined ->
			  gen_server:multi_call(Nodes,
						global_name_server,
						{register, Name, Pid, Method}),
			  yes;
		      _Pid -> no
		  end
	  end).

unregister_name(Name) ->
    case where(Name) of
	undefined ->
	    ok;
	_ ->
	    trans_all_known(fun(Nodes) ->
			  gen_server:multi_call(Nodes,
						global_name_server,
						{unregister, Name}),
			  ok
		  end)
    end.

re_register_name(Name, Pid) when pid(Pid) ->
    re_register_name(Name, Pid, {global, random_exit_name}).
re_register_name(Name, Pid, Method) when pid(Pid) ->
    trans_all_known(fun(Nodes) ->
		  gen_server:multi_call(Nodes,
					global_name_server,
					{register, Name, Pid, Method}),
		  yes
	  end).

%% Returns all globally registered names
registered_names() -> lists:map(fun({Name, _Pid, _Method}) -> Name end,
				ets:tab2list(global_names)).

%%-----------------------------------------------------------------
%% An external node (i.e not an erlang node) (un)registers a name.
%% If the registered Pid crashes the name is to be removed from global.
%% If the external node crashes the name is to be removed from global.
%% If the erlang node which registers the name crashes the name is also to be
%% removed, because the registered process is not supervised any more,
%% (i.e there is no link to the registered Pid).
%%-----------------------------------------------------------------
register_name_external(Name, Pid) when pid(Pid) ->
    register_name_external(Name, Pid, {global, random_exit_name}).
register_name_external(Name, Pid, Method) when pid(Pid) ->
    trans_all_known(fun(Nodes) ->
		  case where(Name) of
		      undefined ->
			  gen_server:multi_call(Nodes,
						global_name_server,
						{register, Name, Pid, Method}),
			  gen_server:multi_call(Nodes,
						global_name_server,
						{register_ext, Name, Pid, node()}),
			  yes;
		      _Pid -> no
		  end
	  end).




unregister_name_external(Name) ->
    case where(Name) of
	undefined ->
	    ok;
	_ ->
	    trans_all_known(fun(Nodes) ->
			  gen_server:multi_call(Nodes,
						global_name_server,
						{unregister, Name}),
			  gen_server:multi_call(Nodes,
						global_name_server,
						{unregister_ext, Name}),
			  ok
		  end)
    end.





%%-----------------------------------------------------------------
%% Args: Id = id()
%%       Nodes = [node()]
%%       id() = {ResourceId, LockRequesterId}
%%       Retries = infinity | int() > 0
%% Purpose: Sets a lock on the specified nodes (or all nodes if
%%          none are specified) on ResourceId for LockRequesterId.  If there
%%          already exists a lock on ResourceId for another owner
%%          than LockRequesterId, false is returned, otherwise true.
%% Returns: boolean()
%%-----------------------------------------------------------------
set_lock(Id) ->
    set_lock(Id, [node() | nodes()], infinity, 1).
set_lock(Id, Nodes) ->
    set_lock(Id, Nodes, infinity, 1).
set_lock(Id, Nodes, Retries) when Retries > 0 ->
    set_lock(Id, Nodes, Retries, 1);
set_lock(Id, Nodes, infinity) ->
    set_lock(Id, Nodes, infinity, 1).
set_lock(_Id, _Nodes, 0, _) -> false;
set_lock({ResourceId, LockRequesterId}, Nodes, Retries, Times) ->
    Id = {ResourceId, LockRequesterId},
    Msg = {set_lock, Id},
    {Replies, _} =
	gen_server:multi_call(Nodes, global_name_server, Msg),
    ?P2({set_lock, node(), self(), {ResourceId, LockRequesterId},
	 Nodes, Retries, Times, Replies, catch erlang:error(kaka)}),
    ?P({set_lock, node(), ResourceId,
	{LockRequesterId, node(LockRequesterId)}}),
    case check_replies(Replies, Id, Nodes) of
	true -> ?P({set_lock_true, node(), ResourceId}),
		true;
	false ->
	    random_sleep(Times),
	    set_lock(Id, Nodes, dec(Retries), Times+1);
	N when integer(N) ->
	    ?P({sleeping, N}),
	    timer:sleep(N*500),
	    set_lock(Id, Nodes, Retries, Times);
	Pid when pid(Pid) ->
	    ?P({waiting_for, Pid}),
	    Ref = erlang:monitor(process, Pid),
	    receive
		{'DOWN', Ref, process, Pid, _Reason} ->
		    ?P({waited_for, Pid, _Reason}),
		    set_lock(Id, Nodes, Retries, Times)
	    end
    end.

check_replies([{_Node, true} | T], Id, Nodes) ->
    check_replies(T, Id, Nodes);
check_replies([{_Node, Status} | _T], Id, Nodes) ->
    gen_server:multi_call(Nodes, global_name_server, {del_lock, Id}),
    Status;
check_replies([], _Id, _Nodes) ->
    true.

del_lock(Id) ->
    del_lock(Id, [node() | nodes()]).
del_lock({ResourceId, LockRequesterId}, Nodes) ->
    Id = {ResourceId, LockRequesterId},
    ?P2({del_lock, node(), self(), ResourceId, LockRequesterId, Nodes}),
    gen_server:multi_call(Nodes, global_name_server, {del_lock, Id}),
    true.

%%-----------------------------------------------------------------
%% Args: Id = id()
%%       Fun = fun() | {M,F}
%%       Nodes = [node()]
%%       Retries = infinity | int() > 0
%% Purpose: Sets a lock on Id (as set_lock), and evaluates
%%          Res = Fun() on success.
%% Returns: Res | aborted  (note, if Retries is infinity, the
%%          transaction won't abort)
%%-----------------------------------------------------------------
trans(Id, Fun) -> trans(Id, Fun, [node() | nodes()], infinity).
trans(Id, Fun, Nodes) -> trans(Id, Fun, Nodes, infinity).
trans(_Id, _Fun, _Nodes, 0) -> aborted;
trans(Id, Fun, Nodes, Retries) ->
    case set_lock(Id, Nodes, Retries) of
	true ->
	    case catch Fun() of
		{'EXIT', R} ->
		    del_lock(Id, Nodes),
		    exit(R);
		Res ->
		    del_lock(Id, Nodes),
		    Res
	    end;
	false ->
	    aborted
    end.

%%% Similar to trans(Id, Fun), but always uses global's own lock,
%%% on all nodes known to global, making sure that no new nodes have
%%% become known while we got the list of known nodes.
trans_all_known(F) ->
    Id = {global, self()},
    Nodes = [node() | gen_server:call(global_name_server, get_known)],
    case set_lock(Id, Nodes) of
	true ->
	    Nodes2 = [node() | gen_server:call(global_name_server, get_known)],
	    case Nodes2 -- Nodes of
		[] ->
		    case catch F(Nodes2) of
			{'EXIT', R} ->
			    del_lock(Id, Nodes2),
			    exit(R);
			Res ->
			    del_lock(Id, Nodes2),
			    Res
		    end;
		_ ->
		    del_lock(Id, Nodes),
		    trans_all_known(F)
	    end;
	false ->
	    aborted
    end.

info() ->
    gen_server:call(global_name_server, info).

%%%-----------------------------------------------------------------
%%% Call-back functions from gen_server
%%%-----------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    ets:new(global_locks, [set, named_table, protected]),
    ets:new(global_names, [set, named_table, protected]),
    ets:new(global_names_ext, [set, named_table, protected]),

    %% multi
    S = #state{the_locker = start_the_locker(self()),
	       the_deleter = start_the_deleter(self())},

    case init:get_argument(connect_all) of
	{ok, [["false"]]} ->
	    {ok, S#state{connect_all = false}};
	_ ->
	    {ok, S#state{connect_all = true}}
    end.

%%-----------------------------------------------------------------
%% Connection algorithm
%% ====================
%% This alg solves the problem with partitioned nets as well.
%%
%% The main idea in the alg is that when two nodes connect, they
%% try to set a lock in their own partition (i.e. all nodes already
%% known to them).  When the lock is set in each partition, these
%% two nodes send each other a list with all registered names in
%% resp partition(*).  If no conflict is found, the name tables are
%% just updated.  If a conflict is found, a resolve function is
%% called once for each conflict.  The result of the resolving
%% is sent to the other node.  When the names are exchanged, all
%% other nodes in each partition are informed of the other nodes,
%% and they ping each other to form a fully connected net.
%%
%% Here's the flow:
%% Suppose nodes A and B connect, and C is connected to A.
%%
%% Node A
%% ------
%% << {nodeup, B}
%%   [spawn locker]
%% B ! {init_connect, MyLocker}
%% << {init_connect, MyLocker}
%%   [The lockers try to set the lock]
%% << {lock_is_set, B}
%%   [Now, lock is set in both partitions]
%% B ! {exchange, Names}
%% << {exchange, Names}
%%   [solve conflict]
%% B ! {resolved, Resolved}
%% << {resolved, Resolved}
%% C ! {new_nodes, Resolved, [B]}
%%
%% Node C
%% ------
%% << {new_nodes, ResolvedOps, NewNodes}
%%   [insert Ops]
%% ping(NewNodes)
%% << {nodeup, B}
%% <ignore this one>
%%
%% Several things can disturb this picture.
%%
%% First, the got_names message may arrive *before* the nodeup
%% message, due to delay in net_kernel and an optimisation in the
%% emulator.  We handle this by keeping track of these messages in the
%% pre_connect and lockers variables in our state.
%%
%% The most common situation is when a new node connects to an
%% existing net.  In this case there's no need to set the lock on
%% all nodes in the net, as we know that there won't be any conflict.
%% This is optimised by sending {first_contact, Node} instead of got_names.
%% This implies that first_contact may arrive before nodeup as well.
%%
%% Of course we must handle that some node goes down during the
%% connection.
%%
%% (*) When this information is being exchanged, no one is allowed
%% to change the global register table.  All calls to register etc
%% are protected by a lock.  If a registered process dies
%% during this phase, the deregistration is done as soon as possible
%% on each node (i.e. when the info about the process has arrived).
%%-----------------------------------------------------------------
%% Messages in the protocol
%% ========================
%% 1. Between connecting nodes  (gen_server:casts)
%%    {init_connect, Vsn, Node, InitMsg}
%%         InitMsg = {locker, LockerPid}
%%    {exchange, Node, ListOfNames}
%%    {resolved, Node, Ops, Known}
%%         Known = list of nodes in Node's partition
%% 2. Between lockers on connecting nodes  (!s)
%%    {his_locker, Pid} (from our global)
%%         lockers link to each other
%%    {lock, Bool} loop until both lockers have lock = true,
%%          then send to global {lock_is_set, Node}
%% 3. From connecting node to other nodes in the partition
%%    {new_nodes, Node, Ops, NewNodes}
%% 4. sync protocol
%%    {in_sync, Node, IsKnown}
%%       - sent by each node to all new nodes
%%-----------------------------------------------------------------

handle_call({whereis, Name}, From, S) ->
    do_whereis(Name, From),
    {noreply, S};

handle_call({register, Name, Pid, Method}, _From, S) ->
    ?P2({register, node(), Name}),
    ins_name(Name, Pid, Method),
    {reply, yes, S};

handle_call({unregister, Name}, _From, S) ->
    case ets:lookup(global_names, Name) of
	[{_, Pid, _}] ->
	    ?P2({unregister, node(), Name, Pid, node(Pid)}),
	    ets:delete(global_names, Name),
	    dounlink(Pid);
	_ -> ok
    end,
    {reply, ok, S};

handle_call({register_ext, Name, Pid, RegNode}, _F, S) ->
    ins_name_ext(Name, Pid, RegNode),
    {reply, yes, S};

handle_call({unregister_ext, Name}, _From, S) ->
    ets:delete(global_names_ext, Name),
    {reply, ok, S};


handle_call({set_lock, Lock}, {Pid, _Tag}, S) ->
    Reply = handle_set_lock(Lock, Pid),
    {reply, Reply, S};

handle_call({del_lock, Lock}, {Pid, _Tag}, S) ->
    handle_del_lock(Lock, Pid),
    {reply, true, S};

handle_call(get_known, _From, S) ->
    {reply, S#state.known, S};

%% R7 may call us?
handle_call(get_known_v2, _From, S) ->
    {reply, S#state.known, S};

handle_call({sync, Nodes}, From, S) ->
    %% If we have several global groups, this won't work, since we will
    %% do start_sync on a nonempty list of nodes even if the system
    %% is quiet.
    Pid = start_sync(lists:delete(node(), Nodes) -- S#state.synced, From),
    {noreply, S#state{syncers = [Pid | S#state.syncers]}};

handle_call(get_protocol_version, _From, S) ->
    {reply, ?vsn, S};

handle_call(get_names_ext, _From, S) ->
    {reply, get_names_ext(), S};

handle_call(info, _From, S) ->
    {reply, S, S};

handle_call(stop, _From, S) ->
    {stop, normal, stopped, S}.


%%=======================================================================================
%% init_connect
%%
%% Vsn 1 is the original protocol.
%% Vsn 2 is enhanced with code to take care of registration of names from
%%       non erlang nodes, e.g. c-nodes.
%% Vsn 3 is enhanced with a tag in the synch messages to distinguish
%%       different synch sessions from each other, see OTP-2766.
%%       Note: This requires also that the ticket OTP-2928 is fixed on the nodes
%%             running vsn 1 or 2; if such nodes will coexist with vsn 3 nodes.
%%=======================================================================================
handle_cast({init_connect, Vsn, Node, InitMsg}, S) ->
    ?FORMAT("~p #### init_connect  Vsn ~p, Node ~p, InitMsg ~p~n",[node(), Vsn, Node, InitMsg]),
    case Vsn of
	%% It is always the responsibility of newer versions to understand
	%% older versions of the protocol.
	{HisVsn, HisTag} when HisVsn > ?vsn ->
	    init_connect(?vsn, Node, InitMsg, HisTag, S#state.lockers, S);
	{HisVsn, HisTag} ->
	    init_connect(HisVsn, Node, InitMsg, HisTag, S#state.lockers, S);
	%% To be future compatible
	Tuple when tuple(Tuple) ->
	    List = tuple_to_list(Tuple),
	    [_HisVsn, HisTag | _] = List,
	    %% use own version handling if his is newer.
	    init_connect(?vsn, Node, InitMsg, HisTag, S#state.lockers, S);
	_ when Vsn < 3 ->
	    init_connect(Vsn, Node, InitMsg, undef, S#state.lockers, S);
	_ ->
	    Txt = io_lib:format("Illegal global protocol version ~p Node: ~p",[Vsn, Node]),
	    error_logger:info_report(lists:flatten(Txt))
    end,
    {noreply, S};

%%=======================================================================================
%% lock_is_set
%%
%% Ok, the lock is now set on both partitions. Send our names to other node.
%%=======================================================================================
handle_cast({lock_is_set, Node, MyTag}, S) ->
    ?FORMAT("~p #### lock_is_set  Node ~p~n",[node(), Node]),
    Sync_tag_my = get({sync_tag_my, Node}),
    PVsn =  get({prot_vsn, Node}),
    ?P2({lock_is_set, node(), Node, {MyTag, PVsn}, Sync_tag_my}),
    case {MyTag, PVsn} of
	{Sync_tag_my, undefined} ->
	    %% Patch for otp-2728, the connection to the Node is flipping up and down
	    %% the messages from the 'older' sync tries can disturb the 'new' sync try
	    %% therefor all messages are discarded if the protocol vsn is not defined.
	    Txt = io_lib:format("undefined global protocol version Node: ~p",[Node]),
	    error_logger:info_report(lists:flatten(Txt)),
	    {noreply, S};
	{Sync_tag_my, _} ->
	    %% Check that the Node is still not known
	    case lists:member(Node, S#state.known) of
		false ->
		    ?P2({lset, node(), Node, false}),
		    lock_is_set(Node, S#state.known),
		    {noreply, S};
		true ->
		    ?P2({lset, node(), Node, true}),
		    erase({wait_lock, Node}),
		    NewS = cancel_locker(Node, S),
		    {noreply, NewS}
	    end;
	_ ->
	    ?P2({lset, illegal, node(), Node}),
	    %% Illegal tag, delete the locker.
	    erase({wait_lock, Node}),
	    NewS = cancel_locker(Node, S),
	    {noreply, NewS}
    end;

%%=======================================================================================
%% exchange
%%
%% Here the names are checked to detect name clashes.
%%=======================================================================================
%% Vsn 3 of the protocol
handle_cast({exchange, Node, NameList, NameExtList, MyTag}, S) ->
    ?FORMAT("~p #### handle_cast 3 lock_is_set  exchange ~p~n",
	    [node(),{Node, NameList, NameExtList, MyTag}]),
    Sync_tag_my = get({sync_tag_my, Node}),
    PVsn =  get({prot_vsn, Node}),
    case {MyTag, PVsn} of
	{Sync_tag_my, undefined} ->
	    %% Patch for otp-2728, the connection to the Node is flipping up and down
	    %% the messages from the 'older' sync tries can disturb the 'new' sync try
	    %% therefor all messages are discarded if the protocol vsn is not defined.
	    Txt = lists:flatten(io_lib:format(
				  "undefined global protocol version Node: ~p",[Node])),
	    error_logger:info_report(Txt),
	    {noreply, S};
	{Sync_tag_my, _} ->
	    exchange(PVsn, Node, {NameList, NameExtList}, S#state.known),
	    {noreply, S};
	_ ->
	    %% Illegal tag, delete the locker.
	    erase({wait_lock, Node}),
	    NewS = cancel_locker(Node, S),
	    {noreply, NewS}
    end;



%%=======================================================================================
%% resolved
%%
%% Here the name clashes are resolved.
%%=======================================================================================
%% Vsn 3 of the protocol
handle_cast({resolved, Node, Resolved, HisKnown, _HisKnown_v2, Names_ext, MyTag}, S) ->
    ?FORMAT("~p #### 2 resolved ~p~n",[node(),{Node, Resolved, HisKnown, Names_ext}]),
    Sync_tag_my = get({sync_tag_my, Node}),
    PVsn =  get({prot_vsn, Node}),
    case {MyTag, PVsn} of
	{Sync_tag_my, undefined} ->
		%% Patch for otp-2728, the connection to the Node is flipping up and down
		%% the messages from the 'older' sync tries can disturb the 'new' sync try
		%% therefor all messages are discarded if the protocol vsn is not defined.
		Txt = lists:flatten(io_lib:format(
				      "undefined global protocol version Node: ~p",[Node])),
		error_logger:info_report(Txt),
		{noreply, S};
	{Sync_tag_my, _} ->
	    NewS = resolved(Node, Resolved, {HisKnown, HisKnown}, Names_ext, S),
	    {noreply, NewS};
	_ ->
	    %% Illegal tag, delete the locker.
	    erase({wait_lock, Node}),
	    NewS = cancel_locker(Node, S),
	    {noreply, NewS}
    end;






%%=======================================================================================
%% new_nodes
%%
%% We get to know the other node's known nodes.
%%=======================================================================================
%% Vsn 2 and 3 of the protocol
handle_cast({new_nodes, _Node, Ops, Names_ext, Nodes, _Nodes_v2}, S) ->
    ?P2({new_nodes, node(), Nodes}),
    ?FORMAT("~p #### 2 new_nodes  ~p~n",[node(),{Ops, Names_ext, Nodes}]),
    NewS = new_nodes(Ops, Names_ext, Nodes, S),
    {noreply, NewS};




%%=======================================================================================
%% in_sync
%%
%% We are in sync with this node (from the other node's known world).
%%=======================================================================================
handle_cast({in_sync, Node, IsKnown}, S) ->
    ?FORMAT("~p #### in_sync  ~p~n",[node(),{Node, IsKnown}]),
    lists:foreach(fun(Pid) -> Pid ! {synced, [Node]} end, S#state.syncers),
    %% moved up:
    NewS = cancel_locker(Node, S),
    erase({wait_lock, Node}),
    erase({pre_connect, Node}),
    erase({sync_tag_my, Node}),
    erase({sync_tag_his, Node}),
    NKnown = case lists:member(Node, Known = NewS#state.known) of
		 false when IsKnown == true ->
		     gen_server:cast({global_name_server, Node},
				     {in_sync, node(), false}),
		     [Node | Known];
		 _ ->
		     Known
	     end,
    NSynced = case lists:member(Node, Synced = NewS#state.synced) of
		  true -> Synced;
		  false -> [Node | Synced]
	      end,
    {noreply, NewS#state{known = NKnown, synced = NSynced}};




%% Called when Pid on other node crashed
handle_cast({async_del_name, Name, Pid}, S) ->
    ?P2({async_del_name, node(), Name, Pid, node(Pid)}),
    case ets:lookup(global_names, Name) of
	[{Name, Pid, _}] ->
	    ets:delete(global_names, Name),
	    dounlink(Pid);
	_ -> ok
    end,
    ets:delete(global_names_ext, Name),
    {noreply, S};

handle_cast({async_del_lock, _ResourceId, Pid}, S) ->
    del_locks2(ets:tab2list(global_locks), Pid),
%    ets:match_delete(global_locks, {ResourceId, '_', Pid}),
    {noreply, S}.


handle_info({'EXIT', Deleter, _Reason}=Exit, #state{the_deleter=Deleter}=S) ->
    {stop, {deleter_died,Exit}, S#state{the_deleter=undefined}};
handle_info({'EXIT', Pid, _Reason}, #state{the_deleter=Deleter}=S)
  when pid(Pid) ->
    ?P2({global, exit, node(), Pid, node(Pid)}),
    check_exit(Deleter, Pid),
    Syncers = lists:delete(Pid, S#state.syncers),
    Lockers = lists:keydelete(Pid, 2, S#state.lockers),
    ?PRINT({exit, Pid, lockers, node(), S#state.lockers}),
    {noreply, S#state{syncers = Syncers, lockers = Lockers}};

handle_info({nodedown, Node}, S) when Node == S#state.node_name ->
    %% Somebody stopped the distribution dynamically - change
    %% references to old node name (Node) to new node name ('nonode@nohost')
    {noreply, change_our_node_name(node(), S)};

handle_info({nodedown, Node}, S) ->
    ?FORMAT("~p #### nodedown 1 ####### Node ~p",[node(),Node]),
    %% moved up:
    do_node_down(Node),
    #state{known = Known, synced = Syncs} = S,
    NewS = cancel_locker(Node, S),

    erase({wait_lock, Node}),
    erase({save_ops, Node}),
    erase({pre_connect, Node}),
    erase({prot_vsn, Node}),
    erase({sync_tag_my, Node}),
    erase({sync_tag_his, Node}),
    {noreply, NewS#state{known = lists:delete(Node, Known),
			 synced = lists:delete(Node, Syncs)}};



handle_info({nodeup, Node}, S) when Node == node() ->
    ?FORMAT("~p ####  nodeup S ####### Node ~p~n",[node(), Node]),
    %% Somebody started the distribution dynamically - change
    %% references to old node name ('nonode@nohost') to Node.
    {noreply, change_our_node_name(Node, S)};

handle_info({nodeup, Node}, S) when S#state.connect_all == true ->
    ?FORMAT("~p #### nodeup 1 ####### Node ~p",[node(),Node]),
    IsKnown = lists:member(Node, S#state.known) or
              %% This one is only for double nodeups (shouldn't occur!)
              lists:keymember(Node, 1, S#state.lockers),
    case IsKnown of
	true ->
	    {noreply, S};
	false ->
	    %% now() is used as a tag to separate different sycnh sessions
	    %% from each others. Global could be confused at bursty nodeups
	    %% because it couldn't separate the messages between the different
	    %% synch sessions started by a nodeup.
	    MyTag = now(),
	    resend_pre_connect(Node),

	    %% multi
	    S#state.the_locker ! {nodeup, Node, S#state.known, MyTag, self()},

	    Pid = start_locker(Node, S#state.known, MyTag, self(), S#state.the_locker),
	    Ls = S#state.lockers,
	    InitC = {init_connect, {?vsn, MyTag}, node(), {locker, Pid, S#state.known}},
	    ?P2({putting, MyTag}),
	    put({sync_tag_my, Node}, MyTag),
	    gen_server:cast({global_name_server, Node}, InitC),
	    {noreply, S#state{lockers = [{Node, Pid} | Ls]}}
    end;


%% This message is only to test otp-2766 Global may be confused at bursty
%% nodeup/nodedowns. It's a copy of the complex part of the handling of
%% the 'nodeup' message.
handle_info({test_vsn_tag_nodeup, Node}, S) when S#state.connect_all == true,
						 Node == node() ->
    {noreply, S};
handle_info({test_vsn_tag_nodeup, Node}, S) when S#state.connect_all == true ->
    ?FORMAT("~p #### test_nodeup 1 ####### Node ~p~n",[node(), Node]),
    MyTag = now(),
    resend_pre_connect(Node),
    S#state.the_locker ! {nodeup, Node, S#state.known, MyTag, self()},
    Pid = start_locker(Node, S#state.known, MyTag, self(), S#state.the_locker),
    Ls = S#state.lockers,
    InitC = {init_connect, {?vsn, MyTag}, node(), {locker, Pid, S#state.known}},
    put({sync_tag_my, Node}, MyTag),
    gen_server:cast({global_name_server, Node}, InitC),
    ?PRINT({lockers, node(), Ls}),
    {noreply, S#state{lockers = [{Node, Pid} | Ls]}};


handle_info({whereis, Name, From}, S) ->
    do_whereis(Name, From),
    {noreply, S};

handle_info(known, S) ->
    io:format(">>>> ~p~n",[S#state.known]),
    {noreply, S};

handle_info(_, S) ->
    {noreply, S}.




%%=======================================================================================
%%=======================================================================================
%%=============================== Internal Functions ====================================
%%=======================================================================================
%%=======================================================================================



%%=======================================================================================
%% Another node wants to synchronize its registered names with us.
%% Start a locker process. Both nodes must have a lock before they are
%% allowed to continue.
%%=======================================================================================
init_connect(Vsn, Node, InitMsg, HisTag, Lockers, S) ->
    ?P2({init_connect, node(), Node}),
    ?FORMAT("~p #### init_connect  Vsn, Node, InitMsg ~p~n",[node(),{Vsn, Node, InitMsg}]),
    %% It is always the responsibility of newer versions to understand
    %% older versions of the protocol.
    put({prot_vsn, Node}, Vsn),
    put({sync_tag_his, Node}, HisTag),
    if
	Vsn =< 3 ->
	    case lists:keysearch(Node, 1, Lockers) of
		{value, {_Node, MyLocker}} ->
		    %% We both have lockers; let them set the lock
		    case InitMsg of
			{locker, HisLocker, HisKnown} -> %% current version
			    ?PRINT({init_connect1, node(), self(), Node,
				    MyLocker, HisLocker}),
			    MyLocker ! {his_locker, HisLocker, HisKnown};

			{locker, _HisLocker, HisKnown, HisTheLocker} -> %% multi
			    ?PRINT({init_connect1, node(), self(), Node,
				    MyLocker, _HisLocker}),
			    S#state.the_locker ! {his_the_locker, HisTheLocker,
						  HisKnown, S#state.known}
		    end;
		false ->
		    ?PRINT({init_connect11, node(), self(), Node}),
		    put({pre_connect, Node}, {Vsn, InitMsg, HisTag})
	    end;
	true ->					% Vsn > 3
	    ?P2(vsn4),
	    case lists:keysearch(Node, 1, Lockers) of
		{value, {_Node, _MyLocker}} ->
		    %% We both have lockers; let them set the lock
		    case InitMsg of
			{locker, HisLocker, HisKnown} -> %% current version
			    ?PRINT({init_connect1, node(), self(), Node,
				    _MyLocker, HisLocker}),
			    HisLocker ! {his_locker_new, S#state.the_locker,
					 {HisKnown, S#state.known}};

			{locker, _HisLocker, HisKnown, HisTheLocker} -> %% multi
			    ?PRINT({init_connect1, node(), self(), Node,
				    _MyLocker, _HisLocker}),
			    S#state.the_locker ! {his_the_locker, HisTheLocker,
						  HisKnown, S#state.known}
		    end;
		false ->
		    ?PRINT({init_connect11, node(), self(), Node}),
		    put({pre_connect, Node}, {Vsn, InitMsg, HisTag})
	    end
    end.



%%=======================================================================================
%% In the simple case, we'll get lock_is_set before we get exchange,
%% but we may get exchange before we get lock_is_set from our locker.
%% If that's the case, we'll have to remember the exchange info, and
%% handle it when we get the lock_is_set.  We do this by using the
%% process dictionary - when the lock_is_set msg is received, we store
%% this info.  When exchange is received, we can check the dictionary
%% if the lock_is_set has been received.  If not, we store info about
%% the exchange instead.  In the lock_is_set we must first check if
%% exchange info is stored, in that case we take care of it.
%%=======================================================================================
lock_is_set(Node, Known) ->
    ?FORMAT("~p ####  lock_is_set ~p~n",[node(),{Node, Node, Known}]),
    PVsn = get({prot_vsn, Node}),
    case PVsn of
	_ ->					% 3 and higher
	    gen_server:cast({global_name_server, Node},
			    {exchange, node(), get_names(), get_names_ext(),
			     get({sync_tag_his, Node})})
    end,
    %% If both have the lock, continue with exchange
    case get({wait_lock, Node}) of
	{exchange, NameList, NameExtList} ->
	    %% vsn 2, 3
	    put({wait_lock, Node}, lock_is_set),
	    exchange(PVsn, Node, {NameList, NameExtList}, Known);
	undefined ->
	    put({wait_lock, Node}, lock_is_set)
    end.



%%=======================================================================================
%% exchange
%%=======================================================================================
%% Vsn 3 and higher of the protocol
exchange(_Vsn, Node, {NameList, NameExtList}, Known) ->
    ?FORMAT("~p #### 3 lock_is_set  exchange ~p~n",[node(),{Node, NameList, NameExtList}]),
    case erase({wait_lock, Node}) of
	lock_is_set ->
	    {Ops, Resolved} = exchange_names(NameList, Node, [], []),
	    put({save_ops, Node}, Ops),
	    gen_server:cast({global_name_server, Node},
			    {resolved, node(), Resolved, Known,
			     Known, get_names_ext(), get({sync_tag_his, Node})});
	undefined ->
	    put({wait_lock, Node}, {exchange, NameList, NameExtList})
    end.





resolved(Node, Resolved, {HisKnown, _HisKnown_v2}, Names_ext, S) ->
    ?P2({resolved, node(), Node, S#state.known}),
    ?FORMAT("~p #### 2 resolved ~p~n",[node(),{Node, Resolved, HisKnown, Names_ext}]),
    erase({prot_vsn, Node}),
    Ops = erase({save_ops, Node}) ++ Resolved,
    Known = S#state.known,
    Synced = S#state.synced,
    NewNodes = [Node | HisKnown],
    do_ops(Ops),
    do_ops_ext(Ops,Names_ext),
    gen_server:abcast(Known, global_name_server,
		      {new_nodes, node(), Ops, Names_ext, NewNodes, NewNodes}),
    %% I am synced with Node, but not with HisKnown yet
    lists:foreach(fun(Pid) -> Pid ! {synced, [Node]} end, S#state.syncers),
    gen_server:abcast(HisKnown, global_name_server, {in_sync, node(), true}),
    NewS = lists:foldl(fun(Node1, S1) -> cancel_locker(Node1, S1) end,
		       S,
		       NewNodes),
    %% See (*) below... we're node b in that description
    NewKnown = Known ++ (NewNodes -- Known),
    NewS#state{known = NewKnown, synced = [Node | Synced]}.




new_nodes(Ops, Names_ext, Nodes, S) ->
    ?FORMAT("~p #### 2 new_nodes  ~p~n",[node(),{Ops, Names_ext, Nodes}]),
    do_ops(Ops),
    do_ops_ext(Ops,Names_ext),
    Known = S#state.known,
    %% (*) This one requires some thought...
    %% We're node a, other nodes b and c:
    %% The problem is that {in_sync, a} may arrive before {resolved, [a]} to
    %% b from c, leading to b sending {new_nodes, [a]} to us (node a).
    %% Therefore, we make sure we never get duplicates in Known.
    NewNodes = lists:delete(node(), Nodes -- Known),
    gen_server:abcast(NewNodes, global_name_server, {in_sync, node(), true}),
    S#state{known = Known ++ NewNodes}.





do_whereis(Name, From) ->
    case is_lock_set(global) of
	false ->
	    gen_server:reply(From, where(Name));
	true ->
	    send_again({whereis, Name, From})
    end.

terminate(_Reason, _S) ->
    ets:delete(global_names),
    ets:delete(global_names_ext),
    ets:delete(global_locks).

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%% Resend init_connect to ourselves.
resend_pre_connect(Node) ->
    case erase({pre_connect, Node}) of
%	{Vsn, InitMsg, undef} ->
%	    %% Vsn 1 & 2
%	    ?PRINT({resend_pre_connect2, node(), self(), Node}),
%	    gen_server:cast(self(), {init_connect, Vsn, Node, InitMsg});
	{Vsn, InitMsg, HisTag} ->
	    %% Vsn 3
	    ?PRINT({resend_pre_connect3, node(), self(), Node}),
	    gen_server:cast(self(), {init_connect, {Vsn, HisTag}, Node, InitMsg});
	_ ->
	    ?PRINT({resend_pre_connect0, node(), self(), Node}),
	    ok
    end.

ins_name(Name, Pid, Method) ->
    case ets:lookup(global_names, Name) of
	[{Name, Pid2, _}] ->
	    dounlink(Pid2);
	[] ->
	    ok
    end,
    dolink(Pid),
    ets:insert(global_names, {Name, Pid, Method}).

ins_name_ext(Name, Pid, RegNode) ->
    case ets:lookup(global_names_ext, Name) of
	[{Name, Pid2, _}] ->
	    dounlink(Pid2);
	[] ->
	    ok
    end,
    dolink_ext(Pid, RegNode),
    ets:insert(global_names_ext, {Name, Pid, RegNode}).

where(Name) ->
    case ets:lookup(global_names, Name) of
	[{_, Pid, _}] -> Pid;
	[] -> undefined
    end.

handle_set_lock({ResourceId, LockRequesterId}, Pid) ->
    case ets:lookup(global_locks, ResourceId) of
	[{ResourceId, LockRequesterId, Pids}] ->
	    case lists:member(Pid, Pids) of
		true ->
		    true;
		false ->
		    dolink(Pid),
		    ets:insert(global_locks, {ResourceId, LockRequesterId, [Pid | Pids]}),
		    true
	    end;
	[{ResourceId, _LockRequesterId2, _Pid2}] ->
	    case ResourceId of
		global ->
		    ?P({before,
			LockRequesterId,
			_LockRequesterId2,
			S#state.lockers}),
		    false;
		_ ->
		    false
	    end;
	[] ->
	    dolink(Pid),
	    ets:insert(global_locks, {ResourceId, LockRequesterId, [Pid]}),
	    true
    end.

is_lock_set(ResourceId) ->
    case ets:lookup(global_locks, ResourceId) of
	[_Lock] -> true;
	[] -> false
    end.

handle_del_lock({ResourceId, LockRequesterId}, Pid) ->
    case ets:lookup(global_locks, ResourceId) of
	[{ResourceId, LockRequesterId, Pids}] when [Pid] == Pids ->
	    ets:delete(global_locks, ResourceId),
	    dounlink(Pid);
	[{ResourceId, LockRequesterId, Pids}] ->
	    NewPids = lists:delete(Pid, Pids),
	    ets:insert(global_locks, {ResourceId, LockRequesterId, NewPids}),
	    dounlink(Pid);
	_ -> ok
    end.

do_ops(Ops) ->
    lists:foreach(fun({insert, Item}) -> ets:insert(global_names, Item);
		     ({delete, Name}) ->
			  case ets:lookup(global_names, Name) of
			      [{Name, Pid, _}] ->
    ?P2({do_ops_delete, node(), Name, Pid, node(Pid)}),
				  ets:delete(global_names, Name),
				  dounlink(Pid);
			      [] ->
				  ok
			  end
		  end, Ops).

%% If a new name, then it must be checked if it is an external name
%% If delete a name it is always deleted from global_names_ext
do_ops_ext(Ops, Names_ext) ->
    lists:foreach(fun({insert, {Name, Pid, _Method}}) ->
			  case lists:keysearch(Name, 1, Names_ext) of
			      {value, {Name, Pid, RegNode}} ->
				  ets:insert(global_names_ext, {Name, Pid, RegNode});
			      _ ->
				  ok
			  end;
		     ({delete, Name}) ->
			  ets:delete(global_names_ext, Name)
		  end, Ops).

%%-----------------------------------------------------------------
%% A locker is a process spawned by global_name_server when a
%% nodeup is received from a new node.  Its purpose is to try to
%% set a lock in our partition, i.e. on all nodes known to us.
%% When the lock is set, it tells global about it, and keeps
%% the lock set.  global sends a cancel message to the locker when
%% the partitions are connected.

%% Versions: at version 2, the messages exchanged between the lockers
%% include the known nodes (see OTP-3576). There is no way of knowing
%% the version number of the other side's locker when sending a message
%% to it, so we send both version 1 and 2, and flush the version 1 if
%% we receive version 2.
%%
%% Due to a mistake, an intermediate version of the new locking protocol
%% (using 3-tuples) went out in R7, which only understands itself. This patch
%% to R7 handles all kinds, which means sending all, and flush the ones we
%% don't want. (It will remain difficult to make a future version of the
%% protocol communicate with this one.)
%%
%%-----------------------------------------------------------------
%% (Version 2 in patched R7. No named version in R6 and older - let's call that
%% version 1.)
-define(locker_vsn, 2).

%%% multi

-record(multi, {known, others = []}).

start_the_locker(Global) ->
    spawn_link(?MODULE, init_the_locker, [Global]).

%init_the_locker(Global) ->
%    ok;
init_the_locker(Global) ->
    process_flag(trap_exit, true),		%needed?
    loop_the_locker(Global, #multi{}),
    erlang:error(locker_exited).

remove_node(_Node, []) ->
    [];
remove_node(Node, [{Node, _HisTheLocker, _HisKnown, _MyTag} | Rest]) ->
    Rest;
remove_node(Node, [E | Rest]) ->
    [E | remove_node(Node, Rest)].

find_node_tag(_Node, []) ->
    false;
find_node_tag(Node, [{Node, _HisTheLocker, _HisKnown, MyTag} | _Rest]) ->
    {true, MyTag};
find_node_tag(Node, [_E | Rest]) ->
    find_node_tag(Node, Rest).

loop_the_locker(Global, S) ->
    ?P2({others, node(), S#multi.others}),
%    Known = S#multi.known,
    Timeout = case S#multi.others of
		  [] ->
		      infinity;
		  _ ->
		      0
	      end,
    receive
%	{nodeup, Node, Known, Tag, P} ->
%	    ?P2({the_locker, nodeup, time(), node(), nodeup, Node, Tag}),
%	    loop_the_locker(Global, S);
	{his_the_locker, HisTheLocker, HisKnown, MyKnown} ->
	    ?P2({his_the_locker, time(), node(), HisTheLocker,
			    node(HisTheLocker)}),
	    receive
		{nodeup, Node, _Known, MyTag, _P} when node(HisTheLocker) == Node ->
		    ?P2({the_locker, nodeup, node(), Node,
				    node(HisTheLocker), MyTag,
				    process_info(self(), messages)}),
		    Others = S#multi.others,
		    loop_the_locker(Global,
				    S#multi{known=MyKnown,
					    others=[{node(HisTheLocker), HisTheLocker, HisKnown, MyTag} | Others]});
		{cancel, Node, _Tag} when node(HisTheLocker) == Node ->
		    loop_the_locker(Global, S)
	    after 60000 ->
		    ?P2({nodeupnevercame, node(), node(HisTheLocker)}),
		    error_logger:error_msg("global: nodeup never came ~w ~w~n",
					   [node(), node(HisTheLocker)]),
		    loop_the_locker(Global, S)
	    end;
	{cancel, Node, undefined} ->
	    ?P2({the_locker, cancel1, undefined, node(), Node}),
%% If we actually cancel something when a cancel message with the tag
%% 'undefined' arrives, we may be acting on an old nodedown, to cancel
%% a new nodeup, so we can't do that.
%	    receive
%		{nodeup, Node, _Known, _MyTag, _P} ->
%		    ?P2({the_locker, cancelnodeup1, node(), Node}),
%		    ok
%	    after 0 ->
%		    ok
%	    end,
%	    Others = remove_node(Node, S#multi.others),
%	    loop_the_locker(Global, S#multi{others = Others});
	    loop_the_locker(Global, S);
	{cancel, Node, Tag} ->
	    ?P2({the_locker, cancel1, Tag, node(), Node}),
	    receive
		{nodeup, Node, _Known, Tag, _P} ->
		    ?P2({the_locker, cancelnodeup2, node(), Node}),
		    ok
	    after 0 ->
		    ok
	    end,
	    Others = remove_node(Node, S#multi.others),
	    loop_the_locker(Global, S#multi{others = Others});
	{lock_set, _Pid, false, _} ->
	    ?P2({the_locker, spurious, node(), node(_Pid)}),
	    loop_the_locker(Global, S);
	{lock_set, Pid, true, HisKnown} ->
	    Node = node(Pid),
	    ?P2({the_locker, spontaneous, node(), Node}),

	    NewKnown = gen_server:call(global_name_server, get_known),

	    Others =
		case find_node_tag(Node, S#multi.others) of
		    {true, MyTag} ->

			BothsKnown = HisKnown -- (HisKnown -- NewKnown),
			Known1 = if
				     node() < Node ->
					 [node() | NewKnown];
				     true ->
					 [node() | NewKnown] -- BothsKnown
				 end,

			?P2({lock1, node()}),
			LockId = {global, self()},
			IsLockSet = set_lock(LockId, Known1, 1),
			Pid ! {lock_set, self(), IsLockSet, NewKnown},
			?P2({the_locker, spontaneous, node(), Node, IsLockSet}),
			case IsLockSet of
			    true ->
				gen_server:cast(global_name_server,
						{lock_is_set, Node, MyTag}),
				?P1({lock_sync_done, time(), node(),
				    {Pid, node(Pid)}, self()}),
				%% Wait for global to tell us to remove lock.
				receive
				    {cancel, Node, _Tag} ->
					%% All conflicts are resolved,
					%% remove lock.
					?PRINT({node(), self(), locked1}),
					del_lock(LockId, Known1);
				    {'EXIT', Pid, _} ->
					?PRINT({node(), self(), locked2}),
					%% Other node died;
					%% remove lock and ignore him.
					del_lock(LockId, Known1),
					link(Global)
				end,
				remove_node(Node, S#multi.others);
			    false ->
				S#multi.others
			end;
		    false ->
			?P2({the_locker, spontaneous, node(), Node, not_there}),
			Pid ! {lock_set, self(), false, NewKnown},
			S#multi.others
		end,
	    loop_the_locker(Global, S#multi{others = Others});
	Other when element(1, Other) /= nodeup ->
	    ?P2({the_locker, other_msg, Other}),
	    loop_the_locker(Global, S)
    after Timeout ->
	    NewKnown = gen_server:call(global_name_server, get_known),
	    [{Node, HisTheLocker, HisKnown, MyTag} | Rest] = S#multi.others,
	    BothsKnown = HisKnown -- (HisKnown -- NewKnown),
	    Known1 = if
			 node() < Node ->
			     [node() | NewKnown];
			 true ->
			     [node() | NewKnown] -- BothsKnown
		     end,
	    ?P2({picking, node(), Node}),
	    case lists:member(Node, NewKnown) of
		false ->
		    LockId = {global, self()},
		    ?P2({lock2, node()}),
		    IsLockSet = set_lock(LockId, Known1, 1),
		    Others =
			case IsLockSet of
			    true ->
				HisTheLocker ! {lock_set, self(),
						IsLockSet, NewKnown},
				%% OTP-4902
				lock_set_loop(Global, S,
					      Node, MyTag, Rest,
					      Known1,
					      LockId);
			    false ->
				?P2({the_locker, not_locked, node(),
				    Node}),
				S#multi.others
			end,
		    loop_the_locker(Global, S#multi{known=NewKnown,
						    others = Others});
		true ->
		    ?P2({is_known, node(), Node}),
		    loop_the_locker(Global, S#multi{known=NewKnown,
						    others = Rest})
	    end
    end.

lock_set_loop(Global, S, Node, MyTag, Rest, Known1, LockId) ->
    receive
	{lock_set, P, true, _} when node(P) == Node ->
	    ?P2({the_locker, both_set, node(), Node}),

	    %% do sync
	    gen_server:cast(global_name_server, {lock_is_set, Node, MyTag}),
	    ?P1({lock_sync_done, time(), node(), {Pid, node(Pid)}, self()}),

	    %% Wait for global to tell us to remove lock.
	    receive
		{cancel, Node, _} ->
		    %% All conflicts are resolved, remove lock.
		    ?PRINT({node(), self(), locked1}),
		    del_lock(LockId, Known1);
		{'EXIT', _Pid, _} ->
		    ?PRINT({node(), self(), locked2}),
		    %% Other node died; remove lock and ignore him.
		    del_lock(LockId, Known1),
		    link(Global)
	    end,
	    Rest;
	{lock_set, P, false, _} when node(P) == Node ->
	    ?P2({the_locker, not_both_set, node(), Node}),
	    del_lock(LockId, Known1),
	    S#multi.others;
	{cancel, Node, _} ->
	    ?P2({the_locker, cancel2, node(), Node}),
	    del_lock(LockId, Known1),
	    remove_node(Node, S#multi.others);
	{'EXIT', _, _} ->
	    ?P2({the_locker, exit, node(), Node}),
	    del_lock(LockId, Known1),
	    S#multi.others

    after
	%% OTP-4902
	%% A cyclic deadlock could occur in rare cases where three or
	%% more nodes waited for a reply from each other.
	%% Therefore, reject lock_set attempts in this state from
	%% nodes < this node (its enough if at least one node in
	%% the cycle rejects and thus breaks the deadlock)
	5000 ->
	    reject_lock_set(),
	    lock_set_loop(Global, S, Node, MyTag, Rest, Known1, LockId)
    end.

reject_lock_set() ->
    receive
	{lock_set, P, true, _} when node(P) < node() ->
	    P ! {lock_set, self(), false, []},
	    reject_lock_set()
    after
	0 ->
	    true
    end.

start_locker(Node, Known, MyTag, Global, TheLocker) ->
    %% No link here!  The del_lock call would delete the link anyway.
    %% global_name_server has control of these processes anyway...
    %% When the locker process exits due to being sent the 'cancel' message
    %% by the server, the server then removes it from its tables.
    %% When the locker terminates due to other reasons, the server must
    %% be told, so we make a link to it just before exiting.
    spawn(?MODULE, init_locker, [Node, Known, MyTag, Global, TheLocker]).

init_locker(Node, Known, MyTag, Global, TheLocker) ->
    process_flag(trap_exit, true),
    ?PRINT({init_locker, node(), self(), Node}),
    ?P1({init_locker, time(), node(), self(), Node}),
    receive
	{his_locker, Pid, HisKnown} ->
	    ?PRINT({init_locker, node(), self(), his_locker, Node}),
	    link(Pid),
	    %% If two nodes in a group of nodes first disconnect
	    %% and then reconnect, this causes global to deadlock.
	    %% This because both of the reconnecting nodes
	    %% tries to set lock on the other nodes in the group.
	    %% This is solved by letting only one of the reconneting nodes set the lock.
	    BothsKnown = HisKnown -- (HisKnown -- Known),
	    ?P({loop_locker1, node(), {Pid, node(Pid)}}),
	    Res = loop_locker(Node, Pid, Known, 1, MyTag, BothsKnown, Global),
	    ?P({loop_locker2, node(), {Pid, node(Pid)}}),
	    Res;
	{his_locker_new, HisTheLocker, {Known1, Known2}} ->
	    %% slide into the vsn 4 stuff
	    ?P2({his_locker_new, node()}),
	    HisTheLocker ! {his_the_locker, TheLocker, Known1, Known2},
	    exit(normal);
	cancel ->
	    ?PRINT({init_locker, node(), self(), cancel, Node}),
	    exit(normal)
    end.

loop_locker(Node, Pid, Known0, Try, MyTag, BothsKnown, Global) ->
    Known = if
		node() < Node ->
		    [node() | Known0];
		true ->
		    [node() | Known0] -- BothsKnown
    end,

    ?PRINT({locking, node(), self(), Known}),
    LockId = {global, self()},
    ?P2({lock3, node()}),
    IsLockSet = set_lock(LockId, Known, 1),
    ?P({loop_locker, IsLockSet,
	node(), {Pid, node(Pid)}, self(), Try}),
    ?P1({loop_locker, time(), IsLockSet,
	 node(), {Pid, node(Pid)}, self(), Try}),
    ?PRINT({locking1, node(), self(), Known, IsLockSet}),
    %% Tell other node that we managed to get the lock.
    Pid ! {lock, ?locker_vsn, IsLockSet, Known},
    Pid ! {lock, IsLockSet, Known},
    Pid ! {lock, IsLockSet},
    %% Wait for other node's result.
    receive
	%% R7 patched and later
	{lock, _LockerVsn, true, _} when IsLockSet == true ->
	    receive
		{lock, _} ->
		    ok
	    end,
	    receive
		{lock, _, _} ->
		    ok
	    end,
	    ?PRINT({node(), self(), locked}),
	    %% Now we got the lock in both partitions.  Tell
	    %% global, and let him resolve name conflict.
	    ?P1({lock_sync, time(), node(), {Pid, node(Pid)}, self()}),
	    gen_server:cast(global_name_server, {lock_is_set, Node, MyTag}),
	    ?P1({lock_sync_done, time(), node(), {Pid, node(Pid)}, self()}),
	    %% Wait for global to tell us to remove lock.
	    receive
		cancel ->
		    %% All conflicts are resolved, remove lock.
		    ?PRINT({node(), self(), locked1}),
		    del_lock(LockId, Known);
		{'EXIT', Pid, _} ->
		    ?PRINT({node(), self(), locked2}),
		    %% Other node died; remove lock and ignore him.
		    del_lock(LockId, Known),
		    link(Global)
	    end;
	{lock, _LockerVsn, _, HisKnown} ->
	    receive
		{lock, _} ->
		    ok
	    end,
	    receive
		{lock, _, _} ->
		    ok
	    end,
	    %% Some of us failed to get the lock; try again
	    ?PRINT({node(), self(), locked0}),
	    d_lock(IsLockSet, LockId, Known),
	    try_again_locker(Node, Pid, Try, MyTag, HisKnown, Global);
	%% R7 unpatched
	{lock, true, _} when IsLockSet == true ->
	    ?PRINT({node(), self(), locked}),
	    %% Now we got the lock in both partitions.  Tell
	    %% global, and let him resolve name conflict.
	    gen_server:cast(global_name_server, {lock_is_set, Node, MyTag}),
	    %% Wait for global to tell us to remove lock.
	    receive
		cancel ->
		    %% All conflicts are resolved, remove lock.
		    ?PRINT({node(), self(), locked1}),
		    del_lock(LockId, Known);
		{'EXIT', Pid, _} ->
		    ?PRINT({node(), self(), locked2}),
		    %% Other node died; remove lock and ignore him.
		    del_lock(LockId, Known),
		    link(Global)
	    end;
	{lock, _, HisKnown} ->
	    %% Some of us failed to get the lock; try again
	    ?PRINT({node(), self(), locked0}),
	    d_lock(IsLockSet, LockId, Known),
	    try_again_locker(Node, Pid, Try, MyTag, HisKnown, Global);
	%% R6 and earlier
	{lock, true} when IsLockSet == true ->
	    ?PRINT({node(), self(), locked}),
	    %% Now we got the lock in both partitions.  Tell
	    %% global, and let him resolve name conflict.
	    gen_server:cast(global_name_server, {lock_is_set, Node, MyTag}),
	    %% Wait for global to tell us to remove lock.
	    receive
		cancel ->
		    %% All conflicts are resolved, remove lock.
		    ?PRINT({node(), self(), locked1}),
		    del_lock(LockId, Known);
		{'EXIT', Pid, _} ->
		    ?PRINT({node(), self(), locked2}),
		    %% Other node died; remove lock and ignore him.
		    del_lock(LockId, Known),
		    link(Global)
	    end;
	{lock, _} ->
	    %% Some of us failed to get the lock; try again
	    ?PRINT({node(), self(), locked0}),
	    d_lock(IsLockSet, LockId, Known),
	    try_again_locker(Node, Pid, Try, MyTag, BothsKnown, Global);
	{'EXIT', Pid, _} ->
	    %% Other node died; remove lock and ignore him.
	    ?PRINT({node(), self(), locked7}),
	    d_lock(IsLockSet, LockId, Known),
	    link(Global);
	cancel ->
	    ?PRINT({node(), self(), locked8}),
	    d_lock(IsLockSet, LockId, Known)
    end.

d_lock(true, LockId, Known) -> del_lock(LockId, Known);
d_lock(false, _, _) -> ok.

try_again_locker(Node, Pid, Try, MyTag, HisKnown, Global) ->
    ?PRINT({try_again, node(), self(), Node, Pid, Known, Try, MyTag}),
    ?P1({try_again, time(), node(), self(), Node, Pid, Known, Try, MyTag}),
    random_sleep(Try),
    ?P1({try_again2, time(), node(), self(), Node, Pid, Known, Try, MyTag}),
    NewKnown = gen_server:call(global_name_server, get_known),
    case lists:member(Node, NewKnown) of
	false ->
	    BothsKnown1 = HisKnown -- (HisKnown -- NewKnown),
	    ?PRINT({node(), self(), Node, again, notknown}),
	    ?PRINT({bothknown, BothsKnown, BothsKnown1}),
	    loop_locker(Node, Pid, NewKnown, Try+1, MyTag,
			BothsKnown1, Global);
	true ->
	    ?PRINT({node(), self(), Node, again, known}),
	    link(Global),
	    %% Node is already handled, we are ready.
	    ok
    end.

cancel_locker(Node, S) ->
    %% multi
    ?P2({cancel, node(), Node, get({sync_tag_my, Node})}),
    S#state.the_locker ! {cancel, Node, get({sync_tag_my, Node})},

    Lockers = S#state.lockers,
    case lists:keysearch(Node, 1, Lockers) of
	{value, {_, Pid}} ->
	    Pid ! cancel,
	    ?PRINT({cancel, Node, lockers, node(), Lockers}),
	    S#state{lockers = lists:keydelete(Node, 1, Lockers)};
	_ ->
	    S
    end.

%% A node sent us his names. When a name clash is found, the resolve
%% function is called from the smaller node => all resolve funcs are called
%% from the same partition.
exchange_names([{Name, Pid, Method} |Tail], Node, Ops, Res) ->
    case ets:lookup(global_names, Name) of
	[{Name, Pid, _}] ->
	    exchange_names(Tail, Node, Ops, Res);
	[{Name, Pid2, Method2}] when node() < Node ->
	    %% Name clash!  Add the result of resolving to Res(olved).
	    %% We know that node(Pid) /= node(), so we don't
	    %% need to link/unlink to Pid.
	    Node2 = node(Pid2), %%&&&&&& check external node???
	    case rpc:call(Node2, ?MODULE, resolve_it,
			  [Method2, Name, Pid, Pid2]) of
		Pid ->
		    dounlink(Pid2),
		    ets:insert(global_names, {Name, Pid, Method}),
		    Op = {insert, {Name, Pid, Method}},
		    exchange_names(Tail, Node, [Op | Ops], [Op | Res]);
		Pid2 ->
		    Op = {insert, {Name, Pid2, Method2}},
		    exchange_names(Tail, Node, Ops, [Op | Res]);
		none ->
		    dounlink(Pid2),
		    ?P2({unregister, node(), Name, Pid2, node(Pid2)}),
		    ets:delete(global_names, Name),
		    Op = {delete, Name},
		    exchange_names(Tail, Node, [Op | Ops], [Op | Res]);
		{badrpc, Badrpc} ->
		    error_logger:info_msg("global: badrpc ~w received when "
					  "conflicting name ~w was found",
					  [Badrpc, Name]),
		    dounlink(Pid2),
		    ets:insert(global_names, {Name, Pid, Method}),
		    Op = {insert, {Name, Pid, Method}},
		    exchange_names(Tail, Node, [Op | Ops], [Op | Res]);
		Else ->
		    error_logger:info_msg("global: Resolve method ~w for "
					  "conflicting name ~w returned ~w~n",
					  [Method, Name, Else]),
		    dounlink(Pid2),
		    ets:delete(global_names, Name),
		    Op = {delete, Name},
		    exchange_names(Tail, Node, [Op | Ops], [Op | Res])
	    end;
	[{Name, _Pid2, _}] ->
	    %% The other node will solve the conflict.
	    exchange_names(Tail, Node, Ops, Res);
	_ ->
	    %% Entirely new name.
	    ets:insert(global_names, {Name, Pid, Method}),
	    exchange_names(Tail, Node,
			   [{insert, {Name, Pid, Method}} | Ops], Res)
    end;
exchange_names([], _, Ops, Res) ->
    {Ops, Res}.

resolve_it(Method, Name, Pid1, Pid2) ->
    catch Method(Name, Pid1, Pid2).

minmax(P1,P2) ->
    if node(P1) < node(P2) -> {P1, P2}; true -> {P2, P1} end.

random_exit_name(Name, Pid, Pid2) ->
    {Min, Max} = minmax(Pid, Pid2),
    error_logger:info_msg("global: Name conflict terminating ~w~n",
			  [{Name, Max}]),
    exit(Max, kill),
    Min.

random_notify_name(Name, Pid, Pid2) ->
    {Min, Max} = minmax(Pid, Pid2),
    Max ! {global_name_conflict, Name},
    Min.

notify_all_name(Name, Pid, Pid2) ->
    Pid ! {global_name_conflict, Name, Pid2},
    Pid2 ! {global_name_conflict, Name, Pid},
    none.

cnode(Name, Pid, Pid2) ->
    {Min, Max} = minmax(Pid, Pid2),
    error_logger:info_msg("global: Name conflict terminating ~w~n",
			  [{Name, Max}]),
    Max ! {global_name_conflict, Name},
    Min.

%% Only link to pids on our own node
dolink(Pid) when node(Pid) == node() ->
    link(Pid);
dolink(_) -> ok.

%% Only link to pids on our own node
dolink_ext(Pid, RegNode) when RegNode == node() -> link(Pid);
dolink_ext(_, _) -> ok.

dounlink(Pid) when node(Pid) == node() ->
    case ets:match(global_names, {'_', Pid, '_'}) of
	[] ->
	    case is_pid_used(Pid) of
		false ->
		    unlink(Pid);
		true -> ok
	    end;
	_ -> ok
    end;
dounlink(_Pid) ->
    ok.

is_pid_used(Pid) ->
    is_pid_used(ets:tab2list(global_locks), Pid).

is_pid_used([], _Pid) ->
    false;
is_pid_used([{_ResourceId, _LockReqId, Pids} | Tail], Pid) ->
    case lists:member(Pid, Pids) of
	true ->
	    true;
	false ->
	    is_pid_used(Tail, Pid)
    end.



%% check_exit/3 removes the Pid from affected tables.
%% This function needs to abcast the thingie since only the local
%% server is linked to the registered process (or the owner of the
%% lock).  All the other servers rely on the nodedown mechanism.
check_exit(Deleter, Pid) ->
    del_names(Deleter, Pid, ets:tab2list(global_names)),
    del_locks(ets:tab2list(global_locks), Pid).

del_names(Deleter, Pid, [{Name, Pid, _Method} | Tail]) ->
    %% First, delete the Pid from the local ets; then send to other nodes
    ets:delete(global_names, Name),
    ets:delete(global_names_ext, Name),
    dounlink(Pid),
    Deleter ! {delete_name,self(),Name,Pid},
    del_names(Deleter, Pid, Tail);
del_names(Deleter, Pid, [_|T]) ->
    del_names(Deleter, Pid, T);
del_names(_Deleter, _Pid, []) -> done.

del_locks([{ResourceId, LockReqId, Pids} | Tail], Pid) ->
    case {lists:member(Pid, Pids), Pids} of
	{true, [Pid]} ->
	    ets:delete(global_locks, ResourceId),
	    gen_server:abcast(nodes(), global_name_server,
			      {async_del_lock, ResourceId, Pid});
	{true, _} ->
	    NewPids = lists:delete(Pid, Pids),
	    ets:insert(global_locks, {ResourceId, LockReqId, NewPids}),
	    gen_server:abcast(nodes(), global_name_server,
			      {async_del_lock, ResourceId, Pid});
	_ ->
	    continue
    end,
    del_locks(Tail, Pid);
del_locks([], _Pid) -> done.

del_locks2([{ResourceId, LockReqId, Pids} | Tail], Pid) ->
    case {lists:member(Pid, Pids), Pids} of
	{true, [Pid]} ->
	    ets:delete(global_locks, ResourceId);
	{true, _} ->
	    NewPids = lists:delete(Pid, Pids),
	    ets:insert(global_locks, {ResourceId, LockReqId, NewPids});
	_ ->
	    continue
    end,
    del_locks2(Tail, Pid);
del_locks2([], _Pid) ->
    done.



%% Unregister all Name/Pid pairs such that node(Pid) == Node
%% and delete all locks where node(Pid) == Node
do_node_down(Node) ->
    do_node_down_names(Node, ets:tab2list(global_names)),
    do_node_down_names_ext(Node, ets:tab2list(global_names_ext)),
    do_node_down_locks(Node, ets:tab2list(global_locks)).

do_node_down_names(Node, [{Name, Pid, _Method} | T]) when node(Pid) == Node ->
    ets:delete(global_names, Name),
    do_node_down_names(Node, T);
do_node_down_names(Node, [_|T]) ->
    do_node_down_names(Node, T);
do_node_down_names(_, []) -> ok.

%%remove all external names registered on the crashed node
do_node_down_names_ext(Node, [{Name, _Pid, Node} | T]) ->
    ets:delete(global_names, Name),
    ets:delete(global_names_ext, Name),
    do_node_down_names_ext(Node, T);
do_node_down_names_ext(Node, [_|T]) ->
    do_node_down_names_ext(Node, T);
do_node_down_names_ext(_, []) -> ok.

do_node_down_locks(Node, [{ResourceId, LockReqId, Pids} | T]) ->
    case do_node_down_locks2(Pids, Node) of
	[] ->
	    continue;
	RemovePids ->
	    case Pids -- RemovePids of
		[] ->
		    ets:delete(global_locks, ResourceId);
		NewPids ->
		    ets:insert(global_locks, {ResourceId, LockReqId, NewPids})
	    end
    end,
    do_node_down_locks(Node, T);
do_node_down_locks(Node, [_|T]) ->
    do_node_down_locks(Node, T);
do_node_down_locks(_, []) -> done.


do_node_down_locks2(Pids, Node) ->
    do_node_down_locks2(Pids, Node, []).

do_node_down_locks2([], _Node, Res) ->
    Res;
do_node_down_locks2([Pid | Pids], Node, Res) when node(Pid) == Node ->
    do_node_down_locks2(Pids, Node, [Pid | Res]);
do_node_down_locks2([_ | Pids], Node, Res) ->
    do_node_down_locks2(Pids, Node, Res).


get_names() ->
    ets:tab2list(global_names).

get_names_ext() ->
    ets:tab2list(global_names_ext).

random_sleep(Times) ->
    case (Times rem 10) of
	0 -> erase(random_seed);
	_ -> ok
    end,
    case get(random_seed) of
	undefined ->
	    {A1, A2, A3} = now(),
	    random:seed(A1, A2, A3 + erlang:phash(node(), 100000));
	_ -> ok
    end,
    %% First time 1/4 seconds, then doubling each time up to 8 seconds max.
    Tmax = if Times > 5 -> 8000;
	      true -> ((1 bsl Times) * 1000) div 8
	   end,
    T = random:uniform(Tmax),
    ?P({random_sleep, node(), self(), Times, T}),
    receive after T -> ok end.

dec(infinity) -> infinity;
dec(N) -> N-1.

send_again(Msg) ->
    spawn_link(?MODULE, timer, [self(), Msg]).

timer(Pid, Msg) ->
    random_sleep(5),
    Pid ! Msg.

change_our_node_name(NewNode, S) ->
    S#state{node_name = NewNode}.


%%-----------------------------------------------------------------
%% Each sync process corresponds to one call to sync.  Each such
%% process asks the global_name_server on all Nodes if it is in sync
%% with Nodes.  If not, that (other) node spawns a syncer process that
%% waits for global to get in sync with all Nodes.  When it is in
%% sync, the syncer process tells the original sync process about it.
%%-----------------------------------------------------------------
start_sync(Nodes, From) ->
    spawn_link(?MODULE, sync_init, [Nodes, From]).

sync_init(Nodes, From) ->
    lists:foreach(fun(Node) -> monitor_node(Node, true) end, Nodes),
    sync_loop(Nodes, From).

sync_loop([], From) ->
    gen_server:reply(From, ok);
sync_loop(Nodes, From) ->
    receive
	{nodedown, Node} ->
	    monitor_node(Node, false),
	    sync_loop(lists:delete(Node, Nodes), From);
	{synced, SNodes} ->
	    lists:foreach(fun(N) -> monitor_node(N, false) end, SNodes),
	    sync_loop(Nodes -- SNodes, From)
    end.


%%%====================================================================================
%%% Get the current global_groups definition
%%%====================================================================================
check_sync_nodes() ->
    case get_own_nodes() of
	{ok, all} ->
	    nodes();
	{ok, NodesNG} ->
	    %% global_groups parameter is defined, we are not allowed to sync
	    %% with nodes not in our own global group.
	    (nodes() -- (nodes() -- NodesNG));
	{error, Error} ->
	    {error, Error}
    end.

check_sync_nodes(SyncNodes) ->
    case get_own_nodes() of
	{ok, all} ->
	    SyncNodes;
	{ok, NodesNG} ->
	    %% global_groups parameter is defined, we are not allowed to sync
	    %% with nodes not in our own global group.
	    OwnNodeGroup = (nodes() -- (nodes() -- NodesNG)),
	    IllegalSyncNodes = (SyncNodes -- [node() | OwnNodeGroup]),
	    case IllegalSyncNodes of
		[] -> SyncNodes;
		_ -> {error, {"Trying to sync nodes not defined in the own global group",
			      IllegalSyncNodes}}
	    end;
	{error, Error} ->
	    {error, Error}
    end.

get_own_nodes() ->
    case global_group:get_own_nodes_with_errors() of
        {error, Error} ->
            {error, {"global_groups definition error", Error}};
        OkTup ->
            OkTup
    end.


%%-----------------------------------------------------------------
%% The deleter process is a satellite process to global_name_server
%% that does background batch deleting of names when a process
%% that had globally registered names dies. It is started by and
%% linked to global_name_server.
%%-----------------------------------------------------------------

start_the_deleter(Global) ->
    spawn_link(
      fun () ->
	      loop_the_deleter(Global)
      end).

loop_the_deleter(Global) ->
    Deletions = collect_deletions(Global, []),
    trans({global, self()},
	  fun() ->
		  lists:map(
		    fun ({Name,Pid}) ->
			    ?P2({delete_name2, Name, Pid, nodes()}),
			    gen_server:abcast(nodes(), global_name_server,
					      {async_del_name, Name, Pid})
		    end, Deletions)
	  end,
	  nodes()),
    loop_the_deleter(Global).

collect_deletions(Global, Deletions) ->
    receive
	{delete_name,Global,Name,Pid} ->
	    ?P2({delete_name, node(), self(), Name, Pid, nodes()}),
	    collect_deletions(Global, [{Name,Pid}|Deletions]);
	Other ->
	    error_logger:error_msg("The global_name_server deleter process "
				   "received an unexpected message:\n~p\n",
				   [Other]),
	    collect_deletions(Global, Deletions)
    after case Deletions of
	      [] -> infinity;
	      _  -> 0
	  end ->
	    lists:reverse(Deletions)
    end.
