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
%%     $Id: mnesia_locker.erl,v 1.2 2009/07/01 15:45:40 kostis Exp $
-module(mnesia_locker).

-export([
	 get_held_locks/0,
	 get_lock_queue/0,
	 global_lock/5,
	 ixrlock/5,
	 init/1,
	 mnesia_down/2,
	 release_tid/1,
	 async_release_tid/2,
	 send_release_tid/2,
	 receive_release_tid_acc/2,
	 rlock/3,
	 rlock_table/3,
	 rwlock/3,
	 sticky_rwlock/3,
	 start/0,
	 sticky_wlock/3,
	 sticky_wlock_table/3,
	 wlock/3,
	 wlock_no_exist/4,
	 wlock_table/3
	]).

%% sys callback functions
-export([system_continue/3,
	 system_terminate/4,
	 system_code_change/4
	]).

-include("mnesia.hrl").
-import(mnesia_lib, [dbg_out/2, error/2, verbose/2]).

-define(dbg(S,V), ok).
%-define(dbg(S,V), dbg_out("~p:~p: " ++ S, [?MODULE, ?LINE] ++ V)).

-define(ALL, '______WHOLETABLE_____').
-define(STICK, '______STICK_____').
-define(GLOBAL, '______GLOBAL_____').

-record(state, {supervisor}).

-record(queue, {oid, tid, op, pid, lucky}).

%% mnesia_held_locks: contain       {Oid, Op, Tid} entries  (bag)
-define(match_oid_held_locks(Oid),  {Oid, '_', '_'}).
%% mnesia_tid_locks: contain        {Tid, Oid, Op} entries  (bag)
-define(match_oid_tid_locks(Tid),   {Tid, '_', '_'}).
%% mnesia_sticky_locks: contain     {Oid, Node} entries and {Tab, Node} entries (set)
-define(match_oid_sticky_locks(Oid),{Oid, '_'}).
%% mnesia_lock_queue: contain       {queue, Oid, Tid, Op, ReplyTo, WaitForTid} entries (ordered_set)
-define(match_oid_lock_queue(Oid),  #queue{oid=Oid, tid='_', op = '_', pid = '_', lucky = '_'}).
%% mnesia_lock_counter:             {{write, Tab}, Number} &&
%%                                  {{read, Tab}, Number} entries  (set)

start() ->
    mnesia_monitor:start_proc(?MODULE, ?MODULE, init, [self()]).

init(Parent) ->
    register(?MODULE, self()),
    process_flag(trap_exit, true),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(#state{supervisor = Parent}).

val(Var) ->
    case ?catch_val(Var) of
	{'EXIT', _ReASoN_} -> mnesia_lib:other_val(Var, _ReASoN_);
	_VaLuE_ -> _VaLuE_
    end.

reply(From, R) ->
    From ! {?MODULE, node(), R}.

l_request(Node, X, Store) ->
    {?MODULE, Node} ! {self(), X},
    l_req_rec(Node, Store).

l_req_rec(Node, Store) ->
    ?ets_insert(Store, {nodes, Node}),
    receive
	{?MODULE, Node, {switch, Node2, Req}} ->
	    ?ets_insert(Store, {nodes, Node2}),
	    {?MODULE, Node2} ! Req,
	    {switch, Node2, Req};
	{?MODULE, Node, Reply} ->
	    Reply;
	{mnesia_down, Node} ->
	    {not_granted, {node_not_running, Node}}
    end.

release_tid(Tid) ->
    ?MODULE ! {release_tid, Tid}.

async_release_tid(Nodes, Tid) ->
    rpc:abcast(Nodes, ?MODULE, {release_tid, Tid}).

send_release_tid(Nodes, Tid) ->
    rpc:abcast(Nodes, ?MODULE, {self(), {sync_release_tid, Tid}}).

receive_release_tid_acc([Node | Nodes], Tid) ->
    receive
	{?MODULE, Node, {tid_released, Tid}} ->
	    receive_release_tid_acc(Nodes, Tid);
	{mnesia_down, Node} ->
	    receive_release_tid_acc(Nodes, Tid)
    end;
receive_release_tid_acc([], _Tid) ->
    ok.

loop(State) ->
    receive
	{From, {write, Tid, Oid}} ->
	    try_sticky_lock(Tid, write, From, Oid),
	    loop(State);

	%% If Key == ?ALL it's a request to lock the entire table
	%%

	{From, {read, Tid, Oid}} ->
	    try_sticky_lock(Tid, read, From, Oid),
	    loop(State);

	%% Really do a  read, but get hold of a write lock
	%% used by mnesia:wread(Oid).

	{From, {read_write, Tid, Oid}} ->
	    try_sticky_lock(Tid, read_write, From, Oid),
	    loop(State);

	%% Tid has somehow terminated, clear up everything
	%% and pass locks on to queued processes.
	%% This is the purpose of the mnesia_tid_locks table

	{release_tid, Tid} ->
	    do_release_tid(Tid),
	    loop(State);

	%% stick lock, first tries this to the where_to_read Node
	{From, {test_set_sticky, Tid, {Tab, _} = Oid, Lock}} ->
	    case ?ets_lookup(mnesia_sticky_locks, Tab) of
		[] ->
		    reply(From, not_stuck),
		    loop(State);
		[{_,Node}] when Node == node() ->
		    %% Lock is stuck here, see now if we can just set
		    %% a regular write lock
		    try_lock(Tid, Lock, From, Oid),
		    loop(State);
		[{_,Node}] ->
		    reply(From, {stuck_elsewhere, Node}),
		    loop(State)
	    end;

	%% If test_set_sticky fails, we send this to all nodes
	%% after acquiring a real write lock on Oid

	{stick, {Tab, _}, N} ->
	    ?ets_insert(mnesia_sticky_locks, {Tab, N}),
	    loop(State);

	%% The caller which sends this message, must have first
	%% acquired a write lock on the entire table
	{unstick, Tab} ->
	    ?ets_delete(mnesia_sticky_locks, Tab),
	    loop(State);

	{From, {ix_read, Tid, Tab, IxKey, Pos}} ->
	    case catch mnesia_index:get_index_table(Tab, Pos) of
		{'EXIT', _} ->
		    reply(From, {not_granted, {no_exists, Tab, {index, [Pos]}}}),
		    loop(State);
		Index ->
		    Rk = mnesia_lib:elems(2,mnesia_index:db_get(Index, IxKey)),
		    %% list of real keys
		    case ?ets_lookup(mnesia_sticky_locks, Tab) of
			[] ->
			    set_read_lock_on_all_keys(Tid, From,Tab,Rk,Rk,
						      []),
			    loop(State);
			[{_,N}] when N == node() ->
			    set_read_lock_on_all_keys(Tid, From,Tab,Rk,Rk,
						      []),
			    loop(State);
			[{_,N}] ->
			    Req = {From, {ix_read, Tid, Tab, IxKey, Pos}},
			    From ! {?MODULE, node(), {switch, N, Req}},
			    loop(State)
		    end
	    end;

	{From, {sync_release_tid, Tid}} ->
	    do_release_tid(Tid),
	    reply(From, {tid_released, Tid}),
	    loop(State);

	{release_remote_non_pending, Node, Pending} ->
	    release_remote_non_pending(Node, Pending),
	    mnesia_monitor:mnesia_down(?MODULE, Node),
	    loop(State);

	{'EXIT', Pid, _} when Pid == State#state.supervisor ->
	    do_stop();

	{system, From, Msg} ->
	    verbose("~p got {system, ~p, ~p}~n", [?MODULE, From, Msg]),
	    Parent = State#state.supervisor,
	    sys:handle_system_msg(Msg, From, Parent, ?MODULE, [], State);

	Msg ->
	    error("~p got unexpected message: ~p~n", [?MODULE, Msg]),
	    loop(State)
    end.

set_lock(Tid, Oid, Op) ->
    ?dbg("Granted ~p ~p ~p~n", [Tid,Oid,Op]),
    ?ets_insert(mnesia_held_locks, {Oid, Op, Tid}),
    ?ets_insert(mnesia_tid_locks, {Tid, Oid, Op}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Acquire locks

try_sticky_lock(Tid, Op, Pid, {Tab, _} = Oid) ->
    case ?ets_lookup(mnesia_sticky_locks, Tab) of
	[] ->
	    try_lock(Tid, Op, Pid, Oid);
	[{_,N}] when N == node() ->
	    try_lock(Tid, Op, Pid, Oid);
	[{_,N}] ->
	    Req = {Pid, {Op, Tid, Oid}},
	    Pid ! {?MODULE, node(), {switch, N, Req}}
    end.

try_lock(Tid, read_write, Pid, Oid) ->
    try_lock(Tid, read_write, read, write, Pid, Oid);
try_lock(Tid, Op, Pid, Oid) ->
    try_lock(Tid, Op, Op, Op, Pid, Oid).

try_lock(Tid, Op, SimpleOp, Lock, Pid, Oid) ->
    case can_lock(Tid, Lock, Oid, {no, bad_luck}) of
	yes ->
	    Reply = grant_lock(Tid, SimpleOp, Lock, Oid),
	    reply(Pid, Reply);
	{no, Lucky} ->
	    C = #cyclic{op = SimpleOp, lock = Lock, oid = Oid, lucky = Lucky},
	    ?dbg("Rejected ~p ~p ~p ~p ~n", [Tid, Oid, Lock, Lucky]),
	    reply(Pid, {not_granted, C});
	{queue, Lucky} ->
	    ?dbg("Queued ~p ~p ~p ~p ~n", [Tid, Oid, Lock, Lucky]),
	    %% Append to queue: Nice place for trace output
	    ?ets_insert(mnesia_lock_queue,
			#queue{oid = Oid, tid = Tid, op = Op,
			       pid = Pid, lucky = Lucky}),
	    ?ets_insert(mnesia_tid_locks, {Tid, Oid, {queued, Op}})
    end.

grant_lock(Tid, read, Lock, {Tab, Key})
  when Key /= ?ALL, Tab /= ?GLOBAL ->
    case node(Tid#tid.pid) == node() of
	true ->
	    set_lock(Tid, {Tab, Key}, Lock),
	    {granted, lookup_in_client};
	false ->
	    case catch mnesia_lib:db_get(Tab, Key) of %% lookup as well
		{'EXIT', _Reason} ->
		    %% Table has been deleted from this node,
		    %% restart the transaction.
		    C = #cyclic{op = read, lock = Lock, oid = {Tab, Key},
				lucky = nowhere},
		    {not_granted, C};
		Val ->
		    set_lock(Tid, {Tab, Key}, Lock),
		    {granted, Val}
	    end
    end;
grant_lock(Tid, read, Lock, Oid) ->
    set_lock(Tid, Oid, Lock),
    {granted, ok};
grant_lock(Tid, write, Lock, Oid) ->
    set_lock(Tid, Oid, Lock),
    granted.

%% 1) Impose an ordering on all transactions favour old (low tid) transactions
%%    newer (higher tid) transactions may never wait on older ones,
%% 2) When releasing the tids from the queue always begin with youngest (high tid)
%%    because of 1) it will avoid the deadlocks.
%% 3) TabLocks is the problem :-) They should not starve and not deadlock
%%    handle tablocks in queue as they had locks on unlocked records.

can_lock(Tid, read, {Tab, Key}, AlreadyQ) when Key /= ?ALL ->
    %% The key is bound, no need for the other BIF
    Oid = {Tab, Key},
    ObjLocks = ?ets_match_object(mnesia_held_locks, {Oid, write, '_'}),
    TabLocks = ?ets_match_object(mnesia_held_locks, {{Tab, ?ALL}, write, '_'}),
    check_lock(Tid, Oid, ObjLocks, TabLocks, yes, AlreadyQ, read);

can_lock(Tid, read, Oid, AlreadyQ) -> % Whole tab
    Tab = element(1, Oid),
    ObjLocks = ?ets_match_object(mnesia_held_locks, {{Tab, '_'}, write, '_'}),
    check_lock(Tid, Oid, ObjLocks, [], yes, AlreadyQ, read);

can_lock(Tid, write, {Tab, Key}, AlreadyQ) when Key /= ?ALL ->
    Oid = {Tab, Key},
    ObjLocks = ?ets_lookup(mnesia_held_locks, Oid),
    TabLocks = ?ets_lookup(mnesia_held_locks, {Tab, ?ALL}),
    check_lock(Tid, Oid, ObjLocks, TabLocks, yes, AlreadyQ, write);

can_lock(Tid, write, Oid, AlreadyQ) -> % Whole tab
    Tab = element(1, Oid),
    ObjLocks = ?ets_match_object(mnesia_held_locks, ?match_oid_held_locks({Tab, '_'})),
    check_lock(Tid, Oid, ObjLocks, [], yes, AlreadyQ, write).

%% Check held locks for conflicting locks
check_lock(Tid, Oid, [Lock | Locks], TabLocks, X, AlreadyQ, Type) ->
    case element(3, Lock) of
	Tid ->
	    check_lock(Tid, Oid, Locks, TabLocks, X, AlreadyQ, Type);
	WaitForTid when WaitForTid > Tid -> % Important order
	    check_lock(Tid, Oid, Locks, TabLocks, {queue, WaitForTid}, AlreadyQ, Type);
	WaitForTid when Tid#tid.pid == WaitForTid#tid.pid ->
	    dbg_out("Spurious lock conflict ~w ~w: ~w -> ~w~n",
		    [Oid, Lock, Tid, WaitForTid]),
%%	    check_lock(Tid, Oid, Locks, TabLocks, {queue, WaitForTid}, AlreadyQ);
	    %% BUGBUG Fix this if possible
	    {no, WaitForTid};
	WaitForTid ->
	    {no, WaitForTid}
    end;

check_lock(_, _, [], [], X, {queue, bad_luck}, _) ->
    X;  %% The queue should be correct already no need to check it again

check_lock(_, _, [], [], X = {queue, _Tid}, _AlreadyQ, _) ->
    X;

check_lock(Tid, Oid, [], [], X, AlreadyQ, Type) ->
    {Tab, Key} = Oid,
    if
	Type == write ->
	    check_queue(Tid, Tab, X, AlreadyQ);
	Key == ?ALL ->
	    %% hmm should be solvable by a clever select expr but not today...
	    check_queue(Tid, Tab, X, AlreadyQ);
	true ->
	    %% If there is a queue on that object, read_lock shouldn't be granted
	    ObjLocks = ets:lookup(mnesia_lock_queue, Oid),
	    Greatest = max(ObjLocks),
	    case Greatest of
		empty ->
		    check_queue(Tid, Tab, X, AlreadyQ);
		ObjL when Tid > ObjL ->
		    {no, ObjL}; %% Starvation Preemption (write waits for read)
		ObjL ->
		    check_queue(Tid, Tab, {queue, ObjL}, AlreadyQ)
	    end
    end;

check_lock(Tid, Oid, [], TabLocks, X, AlreadyQ, Type) ->
    check_lock(Tid, Oid, TabLocks, [], X, AlreadyQ, Type).

%% Check queue for conflicting locks
%% Assume that all queued locks belongs to other tid's

check_queue(Tid, Tab, X, AlreadyQ) ->
    TabLocks = ets:lookup(mnesia_lock_queue, {Tab,?ALL}),
    Greatest = max(TabLocks),
    case Greatest of
	empty ->
	    X;
	Tid ->
	    X;
	WaitForTid when WaitForTid#queue.tid > Tid -> % Important order
	    {queue, WaitForTid};
	WaitForTid ->
	    case AlreadyQ of
		{no, bad_luck} -> {no, WaitForTid};
		_ ->
		    erlang:error({mnesia_locker, assert, AlreadyQ})
	    end
    end.

max([]) ->
    empty;
max([H|R]) ->
    max(R, H#queue.tid).

max([H|R], Tid) when H#queue.tid > Tid ->
    max(R, H#queue.tid);
max([_|R], Tid) ->
    max(R, Tid);
max([], Tid) ->
    Tid.

%% We can't queue the ixlock requests since it
%% becomes to complivated for little me :-)
%% If we encounter an object with a wlock we reject the
%% entire lock request
%%
%% BUGBUG: this is actually a bug since we may starve

set_read_lock_on_all_keys(Tid, From, Tab, [RealKey | Tail], Orig, Ack) ->
    Oid = {Tab, RealKey},
    case can_lock(Tid, read, Oid, {no, bad_luck}) of
	yes ->
	    {granted, Val} = grant_lock(Tid, read, read, Oid),
	    case opt_lookup_in_client(Val, Oid, read) of  % Ought to be invoked
		C when record(C, cyclic) ->               % in the client
		    reply(From, {not_granted, C});
		Val2 ->
		    Ack2 = lists:append(Val2, Ack),
		    set_read_lock_on_all_keys(Tid, From, Tab, Tail, Orig, Ack2)
	    end;
	{no, Lucky} ->
	    C = #cyclic{op = read, lock = read, oid = Oid, lucky = Lucky},
	    reply(From, {not_granted, C});
	{queue, Lucky} ->
	    C = #cyclic{op = read, lock = read, oid = Oid, lucky = Lucky},
	    reply(From, {not_granted, C})
    end;
set_read_lock_on_all_keys(_Tid, From, _Tab, [], Orig, Ack) ->
    reply(From, {granted, Ack, Orig}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Release of locks

%% Release remote non-pending nodes
release_remote_non_pending(Node, Pending) ->
    %% Clear the mnesia_sticky_locks table first, to avoid
    %% unnecessary requests to the failing node
    ?ets_match_delete(mnesia_sticky_locks, {'_' , Node}),

    %% Then we have to release all locks held by processes
    %% running at the failed node and also simply remove all
    %% queue'd requests back to the failed node

    AllTids = ?ets_match(mnesia_tid_locks, {'$1', '_', '_'}),
    Tids = [T || [T] <- AllTids, Node == node(T#tid.pid), not lists:member(T, Pending)],
    do_release_tids(Tids).

do_release_tids([Tid | Tids]) ->
    do_release_tid(Tid),
    do_release_tids(Tids);
do_release_tids([]) ->
    ok.

do_release_tid(Tid) ->
    Locks = ?ets_lookup(mnesia_tid_locks, Tid),
    ?dbg("Release ~p ~p ~n", [Tid, Locks]),
    ?ets_delete(mnesia_tid_locks, Tid),
    release_locks(Locks),
    %% Removed queued locks which has had locks
    UniqueLocks = keyunique(lists:sort(Locks),[]),
    rearrange_queue(UniqueLocks).

keyunique([{_Tid, Oid, _Op}|R], Acc = [{_, Oid, _}|_]) ->
    keyunique(R, Acc);
keyunique([H|R], Acc) ->
    keyunique(R, [H|Acc]);
keyunique([], Acc) ->
    Acc.

release_locks([Lock | Locks]) ->
    release_lock(Lock),
    release_locks(Locks);
release_locks([]) ->
    ok.

release_lock({Tid, Oid, {queued, _}}) ->
    ?ets_match_delete(mnesia_lock_queue,
		      #queue{oid=Oid, tid = Tid, op = '_',
			     pid = '_', lucky = '_'});
release_lock({Tid, Oid, Op}) ->
    if
	Op == write ->
	    ?ets_delete(mnesia_held_locks, Oid);
	Op == read ->
	    ?ets_match_delete(mnesia_held_locks, {Oid, Op, Tid})
    end.

rearrange_queue([{_Tid, {Tab, Key}, _} | Locks]) ->
    if
	Key /= ?ALL->
	    Queue =
		ets:lookup(mnesia_lock_queue, {Tab, ?ALL}) ++
		ets:lookup(mnesia_lock_queue, {Tab, Key}),
	    case Queue of
		[] ->
		    ok;
		_ ->
		    Sorted = lists:reverse(lists:keysort(#queue.tid, Queue)),
		    try_waiters_obj(Sorted)
	    end;
	true ->
	    Pat = ?match_oid_lock_queue({Tab, '_'}),
	    Queue = ?ets_match_object(mnesia_lock_queue, Pat),
	    Sorted = lists:reverse(lists:keysort(#queue.tid, Queue)),
	    try_waiters_tab(Sorted)
    end,
    ?dbg("RearrQ ~p~n", [Queue]),
    rearrange_queue(Locks);
rearrange_queue([]) ->
    ok.

try_waiters_obj([W | Waiters]) ->
    case try_waiter(W) of
	queued ->
	    no;
	_ ->
	    try_waiters_obj(Waiters)
    end;
try_waiters_obj([]) ->
    ok.

try_waiters_tab([W | Waiters]) ->
    case W#queue.oid of
	{_Tab, ?ALL} ->
	    case try_waiter(W) of
		queued ->
		    no;
		_ ->
		    try_waiters_tab(Waiters)
	    end;
	Oid ->
	    case try_waiter(W) of
		queued ->
		    Rest = key_delete_all(Oid, #queue.oid, Waiters),
		    try_waiters_tab(Rest);
		_ ->
		    try_waiters_tab(Waiters)
	    end
    end;
try_waiters_tab([]) ->
    ok.

try_waiter({queue, Oid, Tid, read_write, ReplyTo, _}) ->
    try_waiter(Oid, read_write, read, write, ReplyTo, Tid);
try_waiter({queue, Oid, Tid, Op, ReplyTo, _}) ->
    try_waiter(Oid, Op, Op, Op, ReplyTo, Tid).

try_waiter(Oid, Op, SimpleOp, Lock, ReplyTo, Tid) ->
    case can_lock(Tid, Lock, Oid, {queue, bad_luck}) of
	yes ->
	    %% Delete from queue: Nice place for trace output
	    ?ets_match_delete(mnesia_lock_queue,
			      #queue{oid=Oid, tid = Tid, op = Op,
				     pid = ReplyTo, lucky = '_'}),
	    Reply = grant_lock(Tid, SimpleOp, Lock, Oid),
	    ReplyTo ! {?MODULE, node(), Reply},
	    locked;
	{queue, _Why} ->
	    ?dbg("Keep ~p ~p ~p ~p~n", [Tid, Oid, Lock, _Why]),
	    queued; % Keep waiter in queue
	{no, Lucky} ->
	    C = #cyclic{op = SimpleOp, lock = Lock, oid = Oid, lucky = Lucky},
	    verbose("** WARNING ** Restarted transaction, possible deadlock in lock queue ~w: cyclic = ~w~n",
		    [Tid, C]),
	    ?ets_match_delete(mnesia_lock_queue,
			      #queue{oid=Oid, tid = Tid, op = Op,
				     pid = ReplyTo, lucky = '_'}),
	    Reply = {not_granted, C},
	    ReplyTo ! {?MODULE, node(), Reply},
	    removed
    end.

key_delete_all(Key, Pos, TupleList) ->
    key_delete_all(Key, Pos, TupleList, []).
key_delete_all(Key, Pos, [H|T], Ack) when element(Pos, H) == Key ->
    key_delete_all(Key, Pos, T, Ack);
key_delete_all(Key, Pos, [H|T], Ack) ->
    key_delete_all(Key, Pos, T, [H|Ack]);
key_delete_all(_, _, [], Ack) ->
    lists:reverse(Ack).


%% ********************* end server code ********************
%% The following code executes at the client side of a transactions

mnesia_down(N, Pending) ->
    case whereis(?MODULE) of
	undefined ->
	    %% Takes care of mnesia_down's in early startup
	    mnesia_monitor:mnesia_down(?MODULE, N);
	Pid ->
	    %% Syncronously call needed in order to avoid
	    %% race with mnesia_tm's coordinator processes
	    %% that may restart and acquire new locks.
	    %% mnesia_monitor ensures the sync.
	    Pid ! {release_remote_non_pending, N, Pending}
    end.

%% Aquire a write lock, but do a read, used by
%% mnesia:wread/1

rwlock(Tid, Store, Oid) ->
    {Tab, Key} = Oid,
    case val({Tab, where_to_read}) of
	nowhere ->
	    mnesia:abort({no_exists, Tab});
	Node ->
	    Lock = write,
	    case need_lock(Store, Tab, Key, Lock)  of
		yes ->
		    Ns = w_nodes(Tab),
		    Res = get_rwlocks_on_nodes(Ns, Ns, Node, Store, Tid, Oid),
		    ?ets_insert(Store, {{locks, Tab, Key}, Lock}),
		    Res;
		no ->
		    if
			Key == ?ALL ->
			    w_nodes(Tab);
			Tab == ?GLOBAL ->
			    w_nodes(Tab);
			true ->
			    dirty_rpc(Node, Tab, Key, Lock)
		    end
	    end
    end.

get_rwlocks_on_nodes([Node | Tail], Orig, Node, Store, Tid, Oid) ->
    Op = {self(), {read_write, Tid, Oid}},
    {?MODULE, Node} ! Op,
    ?ets_insert(Store, {nodes, Node}),
    add_debug(Node),
    get_rwlocks_on_nodes(Tail, Orig, Node, Store, Tid, Oid);
get_rwlocks_on_nodes([Node | Tail], Orig, OtherNode, Store, Tid, Oid) ->
    Op = {self(), {write, Tid, Oid}},
    {?MODULE, Node} ! Op,
    add_debug(Node),
    ?ets_insert(Store, {nodes, Node}),
    get_rwlocks_on_nodes(Tail, Orig, OtherNode, Store, Tid, Oid);
get_rwlocks_on_nodes([], Orig, _Node, Store, _Tid, Oid) ->
    receive_wlocks(Orig, read_write_lock, Store, Oid).

%% Return a list of nodes or abort transaction
%% WE also insert any additional where_to_write nodes
%% in the local store under the key == nodes

w_nodes(Tab) ->
    Nodes = ?catch_val({Tab, where_to_write}),
    case Nodes of
	[_ | _] -> Nodes;
	_ ->  mnesia:abort({no_exists, Tab})
    end.

%% aquire a sticky wlock, a sticky lock is a lock
%% which remains at this node after the termination of the
%% transaction.

sticky_wlock(Tid, Store, Oid) ->
    sticky_lock(Tid, Store, Oid, write).

sticky_rwlock(Tid, Store, Oid) ->
    sticky_lock(Tid, Store, Oid, read_write).

sticky_lock(Tid, Store, {Tab, Key} = Oid, Lock) ->
    N = val({Tab, where_to_read}),
    if
	node() == N ->
	    case need_lock(Store, Tab, Key, write) of
		yes ->
		    do_sticky_lock(Tid, Store, Oid, Lock);
		no ->
		    dirty_sticky_lock(Tab, Key, [N], Lock)
	    end;
	true ->
	    mnesia:abort({not_local, Tab})
    end.

do_sticky_lock(Tid, Store, {Tab, Key} = Oid, Lock) ->
    ?MODULE ! {self(), {test_set_sticky, Tid, Oid, Lock}},
    receive
	{?MODULE, _N, granted} ->
	    ?ets_insert(Store, {{locks, Tab, Key}, write}),
	    granted;
	{?MODULE, _N, {granted, Val}} -> %% for rwlocks
	    case opt_lookup_in_client(Val, Oid, write) of
		C when record(C, cyclic) ->
		    exit({aborted, C});
		Val2 ->
		    ?ets_insert(Store, {{locks, Tab, Key}, write}),
		    Val2
	    end;
	{?MODULE, _N, {not_granted, Reason}} ->
	    exit({aborted, Reason});
	{?MODULE, N, not_stuck} ->
	    not_stuck(Tid, Store, Tab, Key, Oid, Lock, N),
	    dirty_sticky_lock(Tab, Key, [N], Lock);
	{mnesia_down, N} ->
	    exit({aborted, {node_not_running, N}});
	{?MODULE, N, {stuck_elsewhere, _N2}} ->
	    stuck_elsewhere(Tid, Store, Tab, Key, Oid, Lock),
	    dirty_sticky_lock(Tab, Key, [N], Lock)
    end.

not_stuck(Tid, Store, Tab, _Key, Oid, _Lock, N) ->
    rlock(Tid, Store, {Tab, ?ALL}),   %% needed?
    wlock(Tid, Store, Oid),           %% perfect sync
    wlock(Tid, Store, {Tab, ?STICK}), %% max one sticker/table
    Ns = val({Tab, where_to_write}),
    rpc:abcast(Ns, ?MODULE, {stick, Oid, N}).

stuck_elsewhere(Tid, Store, Tab, _Key, Oid, _Lock) ->
    rlock(Tid, Store, {Tab, ?ALL}),   %% needed?
    wlock(Tid, Store, Oid),           %% perfect sync
    wlock(Tid, Store, {Tab, ?STICK}), %% max one sticker/table
    Ns = val({Tab, where_to_write}),
    rpc:abcast(Ns, ?MODULE, {unstick, Tab}).

dirty_sticky_lock(Tab, Key, Nodes, Lock) ->
    if
	Lock == read_write ->
	    mnesia_lib:db_get(Tab, Key);
	Key == ?ALL ->
	    Nodes;
	Tab == ?GLOBAL ->
	    Nodes;
	true ->
	    ok
    end.

sticky_wlock_table(Tid, Store, Tab) ->
    sticky_lock(Tid, Store, {Tab, ?ALL}, write).

%% acquire a wlock on Oid
%% We store a {Tabname, write, Tid} in all locktables
%% on all nodes containing a copy of Tabname
%% We also store an item {{locks, Tab, Key}, write} in the
%% local store when we have acquired the lock.
%%
wlock(Tid, Store, Oid) ->
    {Tab, Key} = Oid,
    case need_lock(Store, Tab, Key, write) of
	yes ->
	    Ns = w_nodes(Tab),
	    Op = {self(), {write, Tid, Oid}},
	    ?ets_insert(Store, {{locks, Tab, Key}, write}),
	    get_wlocks_on_nodes(Ns, Ns, Store, Op, Oid);
	no when Key /= ?ALL, Tab /= ?GLOBAL ->
	    [];
	no ->
	    w_nodes(Tab)
    end.

wlock_table(Tid, Store, Tab) ->
    wlock(Tid, Store, {Tab, ?ALL}).

%% Write lock even if the table does not exist

wlock_no_exist(Tid, Store, Tab, Ns) ->
    Oid = {Tab, ?ALL},
    Op = {self(), {write, Tid, Oid}},
    get_wlocks_on_nodes(Ns, Ns, Store, Op, Oid).

need_lock(Store, Tab, Key, LockPattern) ->
    TabL = ?ets_match_object(Store, {{locks, Tab, ?ALL}, LockPattern}),
    if
	TabL == [] ->
	    KeyL = ?ets_match_object(Store, {{locks, Tab, Key}, LockPattern}),
	    if
		KeyL == [] ->
		    yes;
		true  ->
		    no
	    end;
	true ->
	    no
    end.

add_debug(Node) ->  % Use process dictionary for debug info
    case get(mnesia_wlock_nodes) of
	undefined ->
	    put(mnesia_wlock_nodes, [Node]);
	NodeList  ->
	    put(mnesia_wlock_nodes, [Node|NodeList])
    end.

del_debug(Node) ->
    case get(mnesia_wlock_nodes) of
	undefined ->  % Shouldn't happen
	    ignore;
	[Node] ->
	    erase(mnesia_wlock_nodes);
	List ->
	    put(mnesia_wlock_nodes, lists:delete(Node, List))
    end.

%% We first send lock requests to the lockmanagers on all
%% nodes holding a copy of the table

get_wlocks_on_nodes([Node | Tail], Orig, Store, Request, Oid) ->
    {?MODULE, Node} ! Request,
    ?ets_insert(Store, {nodes, Node}),
    add_debug(Node),
    get_wlocks_on_nodes(Tail, Orig, Store, Request, Oid);
get_wlocks_on_nodes([], Orig, Store, _Request, Oid) ->
    receive_wlocks(Orig, Orig, Store, Oid).

receive_wlocks([Node | Tail], Res, Store, Oid) ->
    receive
	{?MODULE, Node, granted} ->
	    del_debug(Node),
	    receive_wlocks(Tail, Res, Store, Oid);
	{?MODULE, Node, {granted, Val}} -> %% for rwlocks
	    del_debug(Node),
	    case opt_lookup_in_client(Val, Oid, write) of
		C when record(C, cyclic) ->
		    flush_remaining(Tail, Node, {aborted, C});
		Val2 ->
		    receive_wlocks(Tail, Val2, Store, Oid)
	    end;
	{?MODULE, Node, {not_granted, Reason}} ->
	    del_debug(Node),
	    Reason1 = {aborted, Reason},
	    flush_remaining(Tail, Node, Reason1);
	{mnesia_down, Node} ->
	    del_debug(Node),
	    Reason1 = {aborted, {node_not_running, Node}},
	    flush_remaining(Tail, Node, Reason1);
	{?MODULE, Node, {switch, Node2, Req}} -> %% for rwlocks
	    del_debug(Node),
	    add_debug(Node2),
	    ?ets_insert(Store, {nodes, Node2}),
	    {?MODULE, Node2} ! Req,
	    receive_wlocks([Node2 | Tail], Res, Store, Oid)
    end;

receive_wlocks([], Res, _Store, _Oid) ->
    Res.

flush_remaining([], _SkipNode, Res) ->
    exit(Res);
flush_remaining([SkipNode | Tail ], SkipNode, Res) ->
    del_debug(SkipNode),
    flush_remaining(Tail, SkipNode, Res);
flush_remaining([Node | Tail], SkipNode, Res) ->
    receive
	{?MODULE, Node, _} ->
	    del_debug(Node),
	    flush_remaining(Tail, SkipNode, Res);
	{mnesia_down, Node} ->
	    del_debug(Node),
	    flush_remaining(Tail, SkipNode, {aborted, {node_not_running, Node}})
    end.

opt_lookup_in_client(lookup_in_client, Oid, Lock) ->
    {Tab, Key} = Oid,
    case catch mnesia_lib:db_get(Tab, Key) of
	{'EXIT', _} ->
	    %% Table has been deleted from this node,
	    %% restart the transaction.
	    #cyclic{op = read, lock = Lock, oid = Oid, lucky = nowhere};
	Val ->
	    Val
    end;
opt_lookup_in_client(Val, _Oid, _Lock) ->
    Val.

return_granted_or_nodes({_, ?ALL}   , Nodes) -> Nodes;
return_granted_or_nodes({?GLOBAL, _}, Nodes) -> Nodes;
return_granted_or_nodes(_           , _Nodes) -> granted.

%% We store a {Tab, read, From} item in the
%% locks table on the node where we actually do pick up the object
%% and we also store an item {lock, Oid, read} in our local store
%% so that we can release any locks we hold when we commit.
%% This function not only aquires a read lock, but also reads the object

%% Oid's are always {Tab, Key} tuples
rlock(Tid, Store, Oid) ->
    {Tab, Key} = Oid,
    case val({Tab, where_to_read}) of
	nowhere ->
	    mnesia:abort({no_exists, Tab});
	Node ->
	    case need_lock(Store, Tab, Key, '_') of
		yes ->
		    R = l_request(Node, {read, Tid, Oid}, Store),
		    rlock_get_reply(Node, Store, Oid, R);
		no ->
		    if
			Key == ?ALL ->
			    [Node];
			Tab == ?GLOBAL ->
			    [Node];
			true ->
			    dirty_rpc(Node, Tab, Key, read)
		    end
	    end
    end.

dirty_rpc(nowhere, Tab, Key, _Lock) ->
    mnesia:abort({no_exists, {Tab, Key}});
dirty_rpc(Node, _Tab, ?ALL, _Lock) ->
    [Node];
dirty_rpc(Node, ?GLOBAL, _Key, _Lock) ->
    [Node];
dirty_rpc(Node, Tab, Key, Lock) ->
    Args = [Tab, Key],
    case rpc:call(Node, mnesia_lib, db_get, Args) of
	{badrpc, Reason} ->
	    case val({Tab, where_to_read}) of
		Node ->
		    ErrorTag = mnesia_lib:dirty_rpc_error_tag(Reason),
		    mnesia:abort({ErrorTag, Args});
		_NewNode ->
		    %% Table has been deleted from the node,
		    %% restart the transaction.
		    C = #cyclic{op = read, lock = Lock, oid = {Tab, Key}, lucky = nowhere},
		    exit({aborted, C})
	    end;
	Other ->
	    Other
    end.

rlock_get_reply(Node, Store, Oid, {granted, V}) ->
    {Tab, Key} = Oid,
    ?ets_insert(Store, {{locks, Tab, Key}, read}),
    ?ets_insert(Store, {nodes, Node}),
    case opt_lookup_in_client(V, Oid, read) of
	C when record(C, cyclic) ->
	    mnesia:abort(C);
	Val ->
	    Val
    end;
rlock_get_reply(Node, Store, Oid, granted) ->
    {Tab, Key} = Oid,
    ?ets_insert(Store, {{locks, Tab, Key}, read}),
    ?ets_insert(Store, {nodes, Node}),
    return_granted_or_nodes(Oid, [Node]);
rlock_get_reply(Node, Store, Tab, {granted, V, RealKeys}) ->
    L = fun(K) -> ?ets_insert(Store, {{locks, Tab, K}, read}) end,
    lists:foreach(L, RealKeys),
    ?ets_insert(Store, {nodes, Node}),
    V;
rlock_get_reply(_Node, _Store, _Oid, {not_granted , Reason}) ->
    exit({aborted, Reason});

rlock_get_reply(_Node, Store, Oid, {switch, N2, Req}) ->
    ?ets_insert(Store, {nodes, N2}),
    {?MODULE, N2} ! Req,
    rlock_get_reply(N2, Store, Oid, l_req_rec(N2, Store)).


rlock_table(Tid, Store, Tab) ->
    rlock(Tid, Store, {Tab, ?ALL}).

ixrlock(Tid, Store, Tab, IxKey, Pos) ->
    case val({Tab, where_to_read}) of
	nowhere ->
	    mnesia:abort({no_exists, Tab});
	Node ->
	    R = l_request(Node, {ix_read, Tid, Tab, IxKey, Pos}, Store),
	    rlock_get_reply(Node, Store, Tab, R)
    end.

%% Grabs the locks or exits
global_lock(Tid, Store, Item, write, Ns) ->
    Oid = {?GLOBAL, Item},
    Op = {self(), {write, Tid, Oid}},
    get_wlocks_on_nodes(Ns, Ns, Store, Op, Oid);
global_lock(Tid, Store, Item, read, Ns) ->
    Oid = {?GLOBAL, Item},
    send_requests(Ns, {read, Tid, Oid}),
    rec_requests(Ns, Oid, Store),
    Ns.

send_requests([Node | Nodes], X) ->
    {?MODULE, Node} ! {self(), X},
    send_requests(Nodes, X);
send_requests([], _X) ->
    ok.

rec_requests([Node | Nodes], Oid, Store) ->
    Res = l_req_rec(Node, Store),
    case catch rlock_get_reply(Node, Store, Oid, Res) of
	{'EXIT', Reason} ->
	    flush_remaining(Nodes, Node, Reason);
	_ ->
	    rec_requests(Nodes, Oid, Store)
    end;
rec_requests([], _Oid, _Store) ->
    ok.

get_held_locks() ->
    ?ets_match_object(mnesia_held_locks, '_').

get_lock_queue() ->
    Q = ?ets_match_object(mnesia_lock_queue, '_'),
    [{Oid, Op, Pid, Tid, WFT} || {queue, Oid, Tid, Op, Pid, WFT} <- Q].

do_stop() ->
    exit(shutdown).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% System upgrade

system_continue(_Parent, _Debug, State) ->
    loop(State).

system_terminate(_Reason, _Parent, _Debug, _State) ->
    do_stop().

system_code_change(State, _Module, _OldVsn, _Extra) ->
    {ok, State}.
