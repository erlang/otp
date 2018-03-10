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
%%     $Id: mnesia_tm.erl,v 1.2 2010/03/04 13:54:20 maria Exp $
-module(mnesia_tm).

-export([
	 start/0,
	 init/1,
	 non_transaction/5,
	 transaction/6,
	 commit_participant/5,
	 dirty/2,
	 display_info/2,
	 do_update_op/3,
	 get_info/1,
	 get_transactions/0,
	 info/1,
	 mnesia_down/1,
	 prepare_checkpoint/2,
	 prepare_checkpoint/1, % Internal
	 prepare_snmp/3,
	 do_snmp/2,
	 put_activity_id/1,
	 block_tab/1,
	 unblock_tab/1
	]).

%% sys callback functions
-export([system_continue/3,
	 system_terminate/4,
	 system_code_change/4
	]).

-include("mnesia.hrl").
-import(mnesia_lib, [set/2]).
-import(mnesia_lib, [fatal/2, verbose/2, dbg_out/2]).

-record(state, {coordinators = [], participants = [], supervisor,
		blocked_tabs = [], dirty_queue = []}).
%% Format on coordinators is [{Tid, EtsTabList} .....

-record(prep, {protocol = sym_trans,
	       %% async_dirty | sync_dirty | sym_trans | sync_sym_trans | asym_trans
	       records = [],
	       prev_tab = [], % initiate to a non valid table name
	       prev_types,
	       prev_snmp,
	       types
	      }).

-record(participant, {tid, pid, commit, disc_nodes = [],
		      ram_nodes = [], protocol = sym_trans}).

start() ->
    mnesia_monitor:start_proc(?MODULE, ?MODULE, init, [self()]).

init(Parent) ->
    register(?MODULE, self()),
    process_flag(trap_exit, true),

    %% Initialize the schema
    IgnoreFallback = mnesia_monitor:get_env(ignore_fallback_at_startup),
    mnesia_bup:tm_fallback_start(IgnoreFallback),
    mnesia_schema:init(IgnoreFallback),

    %% Handshake and initialize transaction recovery
    mnesia_recover:init(),
    Early = mnesia_monitor:init(),
    AllOthers = mnesia_lib:uniq(Early ++ mnesia_lib:all_nodes()) -- [node()],
    set(original_nodes, AllOthers),
    mnesia_recover:connect_nodes(AllOthers),

    %% Recover transactions, may wait for decision
    case mnesia_monitor:use_dir() of
	true ->
	    P = mnesia_dumper:opt_dump_log(startup), % previous log
	    L = mnesia_dumper:opt_dump_log(startup), % latest log
	    Msg = "Initial dump of log during startup: ~p~n",
	    mnesia_lib:verbose(Msg, [[P, L]]),
	    mnesia_log:init();
	false ->
	    ignore
    end,

    mnesia_schema:purge_tmp_files(),
    mnesia_recover:start_garb(),

    ?eval_debug_fun({?MODULE, init},  [{nodes, AllOthers}]),

    case val(debug) of
	Debug when Debug /= debug, Debug /= trace ->
	    ignore;
	_ ->
	    mnesia_subscr:subscribe(whereis(mnesia_event), {table, schema})
    end,
    proc_lib:init_ack(Parent, {ok, self()}),
    doit_loop(#state{supervisor = Parent}).

val(Var) ->
    case ?catch_val(Var) of
	{'EXIT', _ReASoN_} -> mnesia_lib:other_val(Var, _ReASoN_);
	_VaLuE_ -> _VaLuE_
    end.

reply({From,Ref}, R) ->
    From ! {?MODULE, Ref, R};
reply(From, R) ->
    From ! {?MODULE, node(), R}.

reply(From, R, State) ->
    reply(From, R),
    doit_loop(State).

req(R) ->
    case whereis(?MODULE) of
	undefined ->
	    {error, {node_not_running, node()}};
	Pid ->
	    Ref = make_ref(),
	    Pid ! {{self(), Ref}, R},
	    rec(Pid, Ref)
    end.

rec() ->
    rec(whereis(?MODULE)).

rec(Pid) when pid(Pid) ->
    receive
	{?MODULE, _, Reply} ->
	    Reply;

	{'EXIT', Pid, _} ->
	    {error, {node_not_running, node()}}
    end;
rec(undefined) ->
    {error, {node_not_running, node()}}.

rec(Pid, Ref) ->
    receive
	{?MODULE, Ref, Reply} ->
	    Reply;
	{'EXIT', Pid, _} ->
	    {error, {node_not_running, node()}}
    end.

tmlink({From, Ref}) when reference(Ref) ->
    link(From);
tmlink(From) ->
    link(From).
tmpid({Pid, _Ref}) when pid(Pid) ->
    Pid;
tmpid(Pid) ->
    Pid.

%% Returns a list of participant transaction Tid's
mnesia_down(Node) ->
    %% Syncronously call needed in order to avoid
    %% race with mnesia_tm's coordinator processes
    %% that may restart and acquire new locks.
    %% mnesia_monitor takes care of the sync
    case whereis(?MODULE) of
	undefined ->
	    mnesia_monitor:mnesia_down(?MODULE, {Node, []});
	Pid ->
	    Pid ! {mnesia_down, Node}
    end.

prepare_checkpoint(Nodes, Cp) ->
    rpc:multicall(Nodes, ?MODULE, prepare_checkpoint, [Cp]).

prepare_checkpoint(Cp) ->
    req({prepare_checkpoint,Cp}).

block_tab(Tab) ->
    req({block_tab, Tab}).

unblock_tab(Tab) ->
    req({unblock_tab, Tab}).

doit_loop(#state{coordinators = Coordinators, participants = Participants, supervisor = Sup}
	  = State) ->
    receive
	{_From, {async_dirty, Tid, Commit, Tab}} ->
	    case lists:member(Tab, State#state.blocked_tabs) of
		false ->
		    do_async_dirty(Tid, Commit, Tab),
		    doit_loop(State);
		true ->
		    Item = {async_dirty, Tid, Commit, Tab},
		    State2 = State#state{dirty_queue = [Item | State#state.dirty_queue]},
		    doit_loop(State2)
	    end;

	{From, {sync_dirty, Tid, Commit, Tab}} ->
	    case lists:member(Tab, State#state.blocked_tabs) of
		false ->
		    do_sync_dirty(From, Tid, Commit, Tab),
		    doit_loop(State);
		true ->
		    Item = {sync_dirty, From, Tid, Commit, Tab},
		    State2 = State#state{dirty_queue = [Item | State#state.dirty_queue]},
		    doit_loop(State2)
	    end;

	{From, start_outer} -> %% Create and associate ets_tab with Tid
	    case catch ?ets_new_table(mnesia_trans_store, [bag, public]) of
		{'EXIT', Reason} -> %% system limit
		    Msg = "Cannot create an ets table for the "
	                  "local transaction store",
		    reply(From, {error, {system_limit, Msg, Reason}}, State);
		Etab ->
		    tmlink(From),
		    C = mnesia_recover:incr_trans_tid_serial(),
		    ?ets_insert(Etab, {nodes, node()}),
		    Tid = #tid{pid = tmpid(From), counter = C},
		    A2 = [{Tid , [Etab]} | Coordinators],
		    S2 = State#state{coordinators = A2},
		    reply(From, {new_tid, Tid, Etab}, S2)
	    end;

	{From, {ask_commit, Protocol, Tid, Commit, DiscNs, RamNs}} ->
	    ?eval_debug_fun({?MODULE, doit_ask_commit},
			    [{tid, Tid}, {prot, Protocol}]),
	    mnesia_checkpoint:tm_enter_pending(Tid, DiscNs, RamNs),
	    Pid =
		case Protocol of
		    asym_trans when node(Tid#tid.pid) /= node() ->
			Args = [tmpid(From), Tid, Commit, DiscNs, RamNs],
			spawn_link(?MODULE, commit_participant, Args);
		    _ when node(Tid#tid.pid) /= node() -> %% *_sym_trans
			reply(From, {vote_yes, Tid}),
			nopid
		end,
	    P = #participant{tid = Tid,
			     pid = Pid,
			     commit = Commit,
			     disc_nodes = DiscNs,
			     ram_nodes = RamNs,
			     protocol = Protocol},
	    State2 = State#state{participants = [P | Participants]},
	    doit_loop(State2);

	{Tid, do_commit} ->
	    case mnesia_lib:key_search_delete(Tid, #participant.tid, Participants) of
		{none, _} ->
		    verbose("Tried to commit a non participant transaction ~p~n",
			    [Tid]),
		    doit_loop(State);
		{P, Participants2} ->
		    ?eval_debug_fun({?MODULE, do_commit, pre},
				    [{tid, Tid}, {participant, P}]),
		    case P#participant.pid of
			nopid ->
			    Commit = P#participant.commit,
			    Member = lists:member(node(), P#participant.disc_nodes),
			    if Member == false ->
				    ignore;
			       P#participant.protocol == sym_trans ->
				    mnesia_log:log(Commit);
			       P#participant.protocol == sync_sym_trans ->
				    mnesia_log:slog(Commit)
			    end,
			    mnesia_recover:note_decision(Tid, committed),
			    do_commit(Tid, Commit),
			    if
				P#participant.protocol == sync_sym_trans ->
				    Tid#tid.pid ! {?MODULE, node(), {committed, Tid}};
				true ->
				    ignore
			    end,
			    mnesia_locker:release_tid(Tid),
			    transaction_terminated(Tid),
			    ?eval_debug_fun({?MODULE, do_commit, post}, [{tid, Tid}, {pid, nopid}]),
			    doit_loop(State#state{participants = Participants2});
			Pid when pid(Pid) ->
			    Pid ! {Tid, committed},
			    ?eval_debug_fun({?MODULE, do_commit, post}, [{tid, Tid}, {pid, Pid}]),
			    doit_loop(State)
		    end
	    end;

	{Tid, simple_commit} ->
	    mnesia_recover:note_decision(Tid, committed),
	    mnesia_locker:release_tid(Tid),
	    transaction_terminated(Tid),
	    doit_loop(State);

	{Tid, {do_abort, Reason}} ->
	    ?eval_debug_fun({?MODULE, do_abort, pre}, [{tid, Tid}]),
	    mnesia_locker:release_tid(Tid),
	    case mnesia_lib:key_search_delete(Tid, #participant.tid, Participants) of
		{none, _} ->
		    verbose("Tried to abort a non participant transaction ~p: ~p~n",
			    [Tid, Reason]),
		    doit_loop(State);
		{P, Participants2} ->
		    case P#participant.pid of
			nopid ->
			    Commit = P#participant.commit,
			    mnesia_recover:note_decision(Tid, aborted),
			    do_abort(Tid, Commit),
			    if
				P#participant.protocol == sync_sym_trans ->
				    Tid#tid.pid ! {?MODULE, node(), {aborted, Tid}};
				true ->
				    ignore
			    end,
			    transaction_terminated(Tid),
			    ?eval_debug_fun({?MODULE, do_abort, post}, [{tid, Tid}, {pid, nopid}]),
			    doit_loop(State#state{participants = Participants2});
			Pid when pid(Pid) ->
			    Pid ! {Tid, {do_abort, Reason}},
			    ?eval_debug_fun({?MODULE, do_abort, post},
					    [{tid, Tid}, {pid, Pid}]),
			    doit_loop(State)
		    end
	    end;

	{From, {add_store, Tid}} -> %% new store for nested  transaction
	    case catch ?ets_new_table(mnesia_trans_store, [bag, public]) of
		{'EXIT', Reason} -> %% system limit
		    Msg = "Cannot create an ets table for a nested "
                          "local transaction store",
		    reply(From, {error, {system_limit, Msg, Reason}}, State);
		Etab ->
		    A2 = add_coord_store(Coordinators, Tid, Etab),
		    reply(From, {new_store, Etab},
			  State#state{coordinators = A2})
	    end;

	{From, {del_store, Tid, Current, Obsolete, PropagateStore}} ->
	    opt_propagate_store(Current, Obsolete, PropagateStore),
	    A2 = del_coord_store(Coordinators, Tid, Current, Obsolete),
	    reply(From, store_erased, State#state{coordinators = A2});

	{'EXIT', Pid, Reason} ->
	    handle_exit(Pid, Reason, State);

	{From, {restart, Tid, Store}} ->
	    A2 = restore_stores(Coordinators, Tid, Store),
	    ?ets_match_delete(Store, '_'),
	    ?ets_insert(Store, {nodes, node()}),
	    reply(From, {restarted, Tid}, State#state{coordinators = A2});

	{delete_transaction, Tid} ->
	    %% used to clear transactions which are committed
	    %% in coordinator or participant processes
	    case mnesia_lib:key_search_delete(Tid, #participant.tid, Participants) of
		{none, _} ->
		    case mnesia_lib:key_search_delete(Tid, 1, Coordinators) of
			{none, _} ->
			    verbose("** ERROR ** Tried to delete a non transaction ~p~n",
				  [Tid]),
			    doit_loop(State);
			{{_Tid, Etabs}, A2} ->
			    erase_ets_tabs(Etabs),
			    transaction_terminated(Tid),
			    doit_loop(State#state{coordinators = A2})
		    end;
		{_P, Participants2} ->
		    transaction_terminated(Tid),
		    State2 = State#state{participants = Participants2},
		    doit_loop(State2)
	    end;

	{sync_trans_serial, Tid} ->
	    %% Do the Lamport thing here
	    mnesia_recover:sync_trans_tid_serial(Tid),
	    doit_loop(State);

	{From, info} ->
	    reply(From, {info, Participants, Coordinators}, State);

	{mnesia_down, N} ->
	    verbose("Got mnesia_down from ~p, reconfiguring...~n", [N]),
	    reconfigure_coordinators(N, Coordinators),

	    Tids = [P#participant.tid || P <- Participants],
	    reconfigure_participants(N, Participants),
	    mnesia_monitor:mnesia_down(?MODULE, {N, Tids}),
	    doit_loop(State);

	{From, {unblock_me, Tab}} ->
	    case lists:member(Tab, State#state.blocked_tabs) of
		false ->
		    verbose("Wrong dirty Op blocked on ~p ~p ~p",
			    [node(), Tab, From]),
		    reply(From, unblocked),
		    doit_loop(State);
		true ->
		    Item = {Tab, unblock_me, From},
		    State2 = State#state{dirty_queue = [Item | State#state.dirty_queue]},
		    doit_loop(State2)
	    end;

	{From, {block_tab, Tab}} ->
	    State2 = State#state{blocked_tabs = [Tab | State#state.blocked_tabs]},
	    reply(From, ok, State2);

	{From, {unblock_tab, Tab}} ->
	    BlockedTabs2 = State#state.blocked_tabs -- [Tab],
	    case lists:member(Tab, BlockedTabs2) of
		false ->
		    mnesia_controller:unblock_table(Tab),
		    Queue = process_dirty_queue(Tab, State#state.dirty_queue),
		    State2 = State#state{blocked_tabs = BlockedTabs2,
					 dirty_queue = Queue},
		    reply(From, ok, State2);
		true ->
		    State2 = State#state{blocked_tabs = BlockedTabs2},
		    reply(From, ok, State2)
	    end;

	{From, {prepare_checkpoint, Cp}} ->
	    Res = mnesia_checkpoint:tm_prepare(Cp),
	    case Res of
		{ok, _Name, IgnoreNew, _Node} ->
		    prepare_pending_coordinators(Coordinators, IgnoreNew),
		    prepare_pending_participants(Participants, IgnoreNew);
		{error, _Reason} ->
		    ignore
	    end,
	    reply(From, Res, State);

	{system, From, Msg} ->
	    dbg_out("~p got {system, ~p, ~p}~n", [?MODULE, From, Msg]),
	    sys:handle_system_msg(Msg, From, Sup, ?MODULE, [], State);

	Msg ->
	    verbose("** ERROR ** ~p got unexpected message: ~p~n", [?MODULE, Msg]),
	    doit_loop(State)
    end.

do_sync_dirty(From, Tid, Commit, _Tab) ->
    ?eval_debug_fun({?MODULE, sync_dirty, pre}, [{tid, Tid}]),
    Res = (catch do_dirty(Tid, Commit)),
    ?eval_debug_fun({?MODULE, sync_dirty, post}, [{tid, Tid}]),
    From ! {?MODULE, node(), {dirty_res, Res}}.

do_async_dirty(Tid, Commit, _Tab) ->
    ?eval_debug_fun({?MODULE, async_dirty, pre}, [{tid, Tid}]),
    catch do_dirty(Tid, Commit),
    ?eval_debug_fun({?MODULE, async_dirty, post}, [{tid, Tid}]).

%% Process items in fifo order
process_dirty_queue(Tab, [Item | Queue]) ->
    Queue2 = process_dirty_queue(Tab, Queue),
    case Item of
	{async_dirty, Tid, Commit, Tab} ->
	    do_async_dirty(Tid, Commit, Tab),
	    Queue2;
	{sync_dirty, From, Tid, Commit, Tab} ->
	    do_sync_dirty(From, Tid, Commit, Tab),
	    Queue2;
	{Tab, unblock_me, From} ->
	    reply(From, unblocked),
	    Queue2;
	_ ->
	    [Item | Queue2]
    end;
process_dirty_queue(_Tab, []) ->
    [].

prepare_pending_coordinators([{Tid, [Store | _Etabs]} | Coords], IgnoreNew) ->
    case catch ?ets_lookup(Store, pending) of
	[] ->
	    prepare_pending_coordinators(Coords, IgnoreNew);
	[Pending] ->
	    case lists:member(Tid, IgnoreNew) of
		false ->
		    mnesia_checkpoint:tm_enter_pending(Pending);
		true ->
		    ignore
	    end,
	    prepare_pending_coordinators(Coords, IgnoreNew);
	{'EXIT', _} ->
	    prepare_pending_coordinators(Coords, IgnoreNew)
    end;
prepare_pending_coordinators([], _IgnoreNew) ->
    ok.

prepare_pending_participants([Part | Parts], IgnoreNew) ->
    Tid = Part#participant.tid,
    D = Part#participant.disc_nodes,
    R = Part#participant.ram_nodes,
    case lists:member(Tid, IgnoreNew) of
	false ->
	    mnesia_checkpoint:tm_enter_pending(Tid, D, R);
	true ->
	    ignore
    end,
    prepare_pending_participants(Parts, IgnoreNew);
prepare_pending_participants([], _IgnoreNew) ->
    ok.

handle_exit(Pid, Reason, State) when node(Pid) /= node() ->
    %% We got exit from a remote fool
    dbg_out("~p got remote EXIT from unknown ~p~n",
	    [?MODULE, {Pid, Reason}]),
    doit_loop(State);

handle_exit(Pid, _Reason, State) when Pid == State#state.supervisor ->
    %% Our supervisor has died, time to stop
    do_stop(State);

handle_exit(Pid, Reason, State) ->
    %% Check if it is a coordinator
    case pid_search_delete(Pid, State#state.coordinators) of
	{none, _} ->
	    %% Check if it is a participant
	    case mnesia_lib:key_search_delete(Pid, #participant.pid, State#state.participants) of
		{none, _} ->
		    %% We got exit from a local fool
		    verbose("** ERROR ** ~p got local EXIT from unknown process: ~p~n",
			    [?MODULE, {Pid, Reason}]),
		    doit_loop(State);

		{P, RestP} when record(P, participant) ->
		    fatal("Participant ~p in transaction ~p died ~p~n",
			  [P#participant.pid, P#participant.tid, Reason]),
		    doit_loop(State#state{participants = RestP})
	    end;

	{{Tid, Etabs}, RestC} ->
	    %% A local coordinator has died and
	    %% we must determine the outcome of the
	    %% transaction and tell mnesia_tm on the
	    %% other nodes about it and then recover
	    %% locally.
	    recover_coordinator(Tid, Etabs),
	    doit_loop(State#state{coordinators = RestC})
    end.

recover_coordinator(Tid, Etabs) ->
    verbose("Coordinator ~p in transaction ~p died.~n", [Tid#tid.pid, Tid]),

    Store = hd(Etabs),
    CheckNodes = get_nodes(Store),
    TellNodes = CheckNodes -- [node()],
    case catch arrange(Tid, Store, async) of
	{'EXIT', Reason} ->
	    dbg_out("Recovery of coordinator ~p failed:~n", [Tid, Reason]),
	    Protocol = asym_trans,
	    tell_outcome(Tid, Protocol, node(), CheckNodes, TellNodes);
	{_N, Prep} ->
	    %% Tell the participants about the outcome
	    Protocol = Prep#prep.protocol,
	    Outcome = tell_outcome(Tid, Protocol, node(), CheckNodes, TellNodes),

	    %% Recover locally
	    CR = Prep#prep.records,
	    {DiscNs, RamNs} = commit_nodes(CR, [], []),
	    {value, Local} = lists:keysearch(node(), #commit.node, CR),

	    ?eval_debug_fun({?MODULE, recover_coordinator, pre},
			    [{tid, Tid}, {outcome, Outcome}, {prot, Protocol}]),
	    recover_coordinator(Tid, Protocol, Outcome, Local, DiscNs, RamNs),
	    ?eval_debug_fun({?MODULE, recover_coordinator, post},
			    [{tid, Tid}, {outcome, Outcome}, {prot, Protocol}])

    end,
    erase_ets_tabs(Etabs),
    transaction_terminated(Tid),
    mnesia_locker:release_tid(Tid).

recover_coordinator(Tid, sym_trans, committed, Local, _, _) ->
    mnesia_recover:note_decision(Tid, committed),
    do_dirty(Tid, Local);
recover_coordinator(Tid, sym_trans, aborted, _Local, _, _) ->
    mnesia_recover:note_decision(Tid, aborted);
recover_coordinator(Tid, sync_sym_trans, committed, Local, _, _) ->
    mnesia_recover:note_decision(Tid, committed),
    do_dirty(Tid, Local);
recover_coordinator(Tid, sync_sym_trans, aborted, _Local, _, _) ->
    mnesia_recover:note_decision(Tid, aborted);

recover_coordinator(Tid, asym_trans, committed, Local, DiscNs, RamNs) ->
    D = #decision{tid = Tid, outcome = committed,
		  disc_nodes = DiscNs, ram_nodes = RamNs},
    mnesia_recover:log_decision(D),
    do_commit(Tid, Local);
recover_coordinator(Tid, asym_trans, aborted, Local, DiscNs, RamNs) ->
    D = #decision{tid = Tid, outcome = aborted,
		  disc_nodes = DiscNs, ram_nodes = RamNs},
    mnesia_recover:log_decision(D),
    do_abort(Tid, Local).

restore_stores([{Tid, Etstabs} | Tail], Tid, Store) ->
    Remaining  = lists:delete(Store, Etstabs),
    erase_ets_tabs(Remaining),
    [{Tid, [Store]} | Tail];
restore_stores([H | T], Tid, Store) ->
    [H | restore_stores(T, Tid, Store)].
%% No NIL case on purpose

add_coord_store([{Tid, Stores} | Coordinators], Tid, Etab) ->
    [{Tid, [Etab | Stores]} | Coordinators];
add_coord_store([H | T], Tid, Etab) ->
    [H | add_coord_store(T, Tid, Etab)].
%% no NIL case on purpose

del_coord_store([{Tid, Stores} | Coordinators], Tid, Current, Obsolete) ->
    Rest =
	case Stores of
	    [Obsolete, Current | Tail] -> Tail;
	    [Current, Obsolete | Tail] -> Tail
	end,
    ?ets_delete_table(Obsolete),
    [{Tid, [Current | Rest]} | Coordinators];
del_coord_store([H | T], Tid, Current, Obsolete) ->
    [H | del_coord_store(T, Tid, Current, Obsolete)].
%% no NIL case on purpose

erase_ets_tabs([H | T]) ->
    ?ets_delete_table(H),
    erase_ets_tabs(T);
erase_ets_tabs([]) ->
    ok.

%% Deletes a pid from a list of participants
%% or from a list of coordinators and returns
%% {none, All} or {Tr, Rest}
pid_search_delete(Pid, Trs) ->
    pid_search_delete(Pid, Trs, none, []).
pid_search_delete(Pid, [Tr = {Tid, _Ts} | Trs], _Val, Ack) when Tid#tid.pid == Pid ->
    pid_search_delete(Pid, Trs, Tr, Ack);
pid_search_delete(Pid, [Tr | Trs], Val, Ack) ->
    pid_search_delete(Pid, Trs, Val, [Tr | Ack]);

pid_search_delete(_Pid, [], Val, Ack) ->
    {Val, Ack}.

%% When TM gets an EXIT sig, we must also check to see
%% if the crashing transaction is in the Participant list
%%
%% search_participant_for_pid([Participant | Tail], Pid) ->
%%     Tid = Participant#participant.tid,
%%     if
%% 	   Tid#tid.pid == Pid  ->
%% 	       {coordinator, Participant};
%% 	   Participant#participant.pid == Pid  ->
%% 	       {participant, Participant};
%% 	   true ->
%% 	       search_participant_for_pid(Tail, Pid)
%%     end;
%% search_participant_for_pid([], _) ->
%%     fool.

transaction_terminated(Tid)  ->
    mnesia_checkpoint:tm_exit_pending(Tid),
    Pid = Tid#tid.pid,
    if
	node(Pid) == node() ->
	    unlink(Pid);
	true ->  %% Do the Lamport thing here
	    mnesia_recover:sync_trans_tid_serial(Tid)
    end.

non_transaction(OldState, Fun, Args, ActivityKind, Mod) ->
    Id = {ActivityKind, self()},
    NewState = {Mod, Id, non_transaction},
    put(mnesia_activity_state, NewState),
    %% I Want something uniqe here, references are expensive
    Ref = mNeSia_nOn_TrAnSacTioN,
    RefRes = (catch {Ref, apply(Fun, Args)}),
    case OldState of
	undefined -> erase(mnesia_activity_state);
	_ -> put(mnesia_activity_state, OldState)
    end,
    case RefRes of
	{Ref, Res} ->
	    case Res of
		{'EXIT', Reason} -> exit(Reason);
		{aborted, Reason} -> mnesia:abort(Reason);
		_ -> Res
	    end;
	{'EXIT', Reason} ->
	    exit(Reason);
	Throw ->
	    throw(Throw)
    end.

transaction(OldTidTs, Fun, Args, Retries, Mod, Type) ->
    Factor = 1,
    case OldTidTs of
	undefined -> % Outer
	    execute_outer(Mod, Fun, Args, Factor, Retries, Type);
	{_OldMod, Tid, Ts} ->  % Nested
	    execute_inner(Mod, Tid, Ts, Fun, Args, Factor, Retries, Type);
	_ -> % Bad nesting
	    {aborted, nested_transaction}
    end.

execute_outer(Mod, Fun, Args, Factor, Retries, Type) ->
    case req(start_outer) of
	{error, Reason} ->
	    {aborted, Reason};
	{new_tid, Tid, Store} ->
	    Ts = #tidstore{store = Store},
	    NewTidTs = {Mod, Tid, Ts},
	    put(mnesia_activity_state, NewTidTs),
	    execute_transaction(Fun, Args, Factor, Retries, Type)
    end.

execute_inner(Mod, Tid, Ts, Fun, Args, Factor, Retries, Type) ->
    case req({add_store, Tid}) of
	{error, Reason} ->
	    {aborted, Reason};
	{new_store, Ets} ->
	    copy_ets(Ts#tidstore.store, Ets),
	    Up = [Ts#tidstore.store | Ts#tidstore.up_stores],
	    NewTs = Ts#tidstore{level = 1 + Ts#tidstore.level,
				store = Ets,
				up_stores = Up},
	    NewTidTs = {Mod, Tid, NewTs},
	    put(mnesia_activity_state, NewTidTs),
	    execute_transaction(Fun, Args, Factor, Retries, Type)
    end.

copy_ets(From, To) ->
    do_copy_ets(?ets_first(From), From, To).
do_copy_ets('$end_of_table', _,_) ->
    ok;
do_copy_ets(K, From, To) ->
    Objs = ?ets_lookup(From, K),
    insert_objs(Objs, To),
    do_copy_ets(?ets_next(From, K), From, To).

insert_objs([H|T], Tab) ->
    ?ets_insert(Tab, H),
    insert_objs(T, Tab);
insert_objs([], _Tab) ->
    ok.

execute_transaction(Fun, Args, Factor, Retries, Type) ->
    case catch apply_fun(Fun, Args, Type) of
	{'EXIT', Reason} ->
	    check_exit(Fun, Args, Factor, Retries, Reason, Type);
	{'atomic', Value} ->
	    mnesia_lib:incr_counter(trans_commits),
	    erase(mnesia_activity_state),
	    %% no need to clear locks, already done by commit ...
	    %% Flush any un processed mnesia_down messages we might have
	    flush_downs(),
	    {'atomic', Value};
	{nested_atomic, Value} ->
	    mnesia_lib:incr_counter(trans_commits),
	    {'atomic', Value};
	Value -> %% User called throw
	    Reason = {aborted, {throw, Value}},
	    return_abort(Fun, Args, Reason)
    end.

apply_fun(Fun, Args, Type) ->
    Result = apply(Fun, Args),
    case t_commit(Type) of
	do_commit ->
            {'atomic', Result};
        do_commit_nested ->
            {nested_atomic, Result};
        {do_abort, {aborted, Reason}} ->
            {'EXIT', {aborted, Reason}};
        {do_abort, Reason} ->
            {'EXIT', {aborted, Reason}}
    end.

check_exit(Fun, Args, Factor, Retries, Reason, Type) ->
    case Reason of
	{aborted, C} when record(C, cyclic) ->
	    maybe_restart(Fun, Args, Factor, Retries, Type, C);
	{aborted, {node_not_running, N}} ->
	    maybe_restart(Fun, Args, Factor, Retries, Type, {node_not_running, N});
	{aborted, {bad_commit, N}} ->
	    maybe_restart(Fun, Args, Factor, Retries, Type, {bad_commit, N});
	_ ->
	    return_abort(Fun, Args, Reason)
    end.

maybe_restart(Fun, Args, Factor, Retries, Type, Why) ->
    {Mod, Tid, Ts} = get(mnesia_activity_state),
    case try_again(Retries) of
	yes when Ts#tidstore.level == 1 ->
	    restart(Mod, Tid, Ts, Fun, Args, Factor, Retries, Type, Why);
	yes ->
	    return_abort(Fun, Args, Why);
	no ->
	    return_abort(Fun, Args, {aborted, nomore})
    end.

try_again(infinity) -> yes;
try_again(X) when number(X) , X > 1 -> yes;
try_again(_) -> no.

%% We can only restart toplevel transactions.
%% If a deadlock situation occurs in a nested transaction
%% The whole thing including all nested transactions need to be
%% restarted. The stack is thus popped by a consequtive series of
%% exit({aborted, #cyclic{}}) calls

restart(Mod, Tid, Ts, Fun, Args, Factor0, Retries0, Type, Why) ->
    mnesia_lib:incr_counter(trans_restarts),
    Retries = decr(Retries0),
    case Why of
	{bad_commit, _N} ->
	    return_abort(Fun, Args, Why),
	    Factor = 1,
	    SleepTime = mnesia_lib:random_time(Factor, Tid#tid.counter),
	    dbg_out("Restarting transaction ~w: in ~wms ~w~n", [Tid, SleepTime, Why]),
	    timer:sleep(SleepTime),
	    execute_outer(Mod, Fun, Args, Factor, Retries, Type);
	{node_not_running, _N} ->   %% Avoids hanging in receive_release_tid_ack
	    return_abort(Fun, Args, Why),
	    Factor = 1,
	    SleepTime = mnesia_lib:random_time(Factor, Tid#tid.counter),
	    dbg_out("Restarting transaction ~w: in ~wms ~w~n", [Tid, SleepTime, Why]),
	    timer:sleep(SleepTime),
	    execute_outer(Mod, Fun, Args, Factor, Retries, Type);
	_ ->
	    SleepTime = mnesia_lib:random_time(Factor0, Tid#tid.counter),
	    dbg_out("Restarting transaction ~w: in ~wms ~w~n", [Tid, SleepTime, Why]),

	    if
		Factor0 /= 10 ->
		    ignore;
		true ->
		    %% Our serial may be much larger than other nodes ditto
		    AllNodes = val({current, db_nodes}),
		    verbose("Sync serial ~p~n", [Tid]),
		    rpc:abcast(AllNodes, ?MODULE, {sync_trans_serial, Tid})
	    end,
	    intercept_friends(Tid, Ts),
	    Store = Ts#tidstore.store,
	    Nodes = get_nodes(Store),
	    ?MODULE ! {self(), {restart, Tid, Store}},
	    mnesia_locker:send_release_tid(Nodes, Tid),
	    timer:sleep(SleepTime),
	    mnesia_locker:receive_release_tid_acc(Nodes, Tid),
	    case rec() of
		{restarted, Tid} ->
		    execute_transaction(Fun, Args, Factor0 + 1,
					Retries, Type);
		{error, Reason} ->
		    mnesia:abort(Reason)
	    end
    end.

decr(infinity) -> infinity;
decr(X) when integer(X), X > 1 -> X - 1;
decr(_X) -> 0.

return_abort(Fun, Args, Reason)  ->
    {Mod, Tid, Ts} = get(mnesia_activity_state),
    OldStore = Ts#tidstore.store,
    Nodes = get_nodes(OldStore),
    intercept_friends(Tid, Ts),
    catch mnesia_lib:incr_counter(trans_failures),
    Level = Ts#tidstore.level,
    if
	Level == 1 ->
	    mnesia_locker:async_release_tid(Nodes, Tid),
	    ?MODULE ! {delete_transaction, Tid},
	    erase(mnesia_activity_state),
	    dbg_out("Transaction ~p calling ~p with ~p, failed ~p~n",
		    [Tid, Fun, Args, Reason]),
	    flush_downs(),
	    {aborted, mnesia_lib:fix_error(Reason)};
	true ->
	    %% Nested transaction
	    [NewStore | Tail] = Ts#tidstore.up_stores,
	    req({del_store, Tid, NewStore, OldStore, true}),
	    Ts2 = Ts#tidstore{store = NewStore,
			      up_stores = Tail,
			      level = Level - 1},
	    NewTidTs = {Mod, Tid, Ts2},
	    put(mnesia_activity_state, NewTidTs),
	    case Reason of
		#cyclic{} ->
		    exit({aborted, Reason});
		{node_not_running, _N} ->
		    exit({aborted, Reason});
		{bad_commit, _N}->
		    exit({aborted, Reason});
		_ ->
		    {aborted, mnesia_lib:fix_error(Reason)}
	    end
    end.

flush_downs() ->
    receive
	{?MODULE, _, _} -> flush_downs(); % Votes
	{mnesia_down, _} -> flush_downs()
    after 0 -> flushed
    end.

put_activity_id(undefined) ->
    erase_activity_id();
put_activity_id({Mod, Tid, Ts}) when record(Tid, tid), record(Ts, tidstore) ->
    flush_downs(),
    Store = Ts#tidstore.store,
    ?ets_insert(Store, {friends, self()}),
    NewTidTs = {Mod, Tid, Ts},
    put(mnesia_activity_state, NewTidTs);
put_activity_id(SimpleState) ->
    put(mnesia_activity_state, SimpleState).

erase_activity_id() ->
    flush_downs(),
    erase(mnesia_activity_state).

get_nodes(Store) ->
    case catch ?ets_lookup_element(Store, nodes, 2) of
	{'EXIT', _} -> [node()];
	Nodes -> Nodes
    end.

get_friends(Store) ->
    case catch ?ets_lookup_element(Store, friends, 2) of
	{'EXIT', _} -> [];
	Friends -> Friends
    end.

opt_propagate_store(_Current, _Obsolete, false) ->
    ok;
opt_propagate_store(Current, Obsolete, true) ->
    propagate_store(Current, nodes, get_nodes(Obsolete)),
    propagate_store(Current, friends, get_friends(Obsolete)).

propagate_store(Store, Var, [Val | Vals]) ->
    ?ets_insert(Store, {Var, Val}),
    propagate_store(Store, Var, Vals);
propagate_store(_Store, _Var, []) ->
    ok.

%% Tell all processes that are cooperating with the current transaction
intercept_friends(_Tid, Ts) ->
    Friends = get_friends(Ts#tidstore.store),
    Message = {activity_ended, undefined, self()},
    intercept_best_friend(Friends, Message).

intercept_best_friend([], _Message) ->
    ok;
intercept_best_friend([Pid | _], Message) ->
    Pid ! Message,
    wait_for_best_friend(Pid, 0).

wait_for_best_friend(Pid, Timeout) ->
    receive
	{'EXIT', Pid, _} -> ok;
	{activity_ended, _, Pid} -> ok
    after Timeout ->
	    case my_process_is_alive(Pid) of
		true -> wait_for_best_friend(Pid, 1000);
		false -> ok
	    end
    end.

my_process_is_alive(Pid) ->
    case catch erlang:is_process_alive(Pid) of % New BIF in R5
	true ->
	    true;
	false ->
	    false;
	{'EXIT', _} -> % Pre R5 backward compatibility
	    case process_info(Pid, message_queue_len) of
		undefined -> false;
		_ -> true
	    end
    end.

dirty(Protocol, Item) ->
    {{Tab, Key}, _Val, _Op} = Item,
    Tid = {dirty, self()},
    Prep = prepare_items(Tid, Tab, Key, [Item], #prep{protocol= Protocol}),
    CR =  Prep#prep.records,
    case Protocol of
	async_dirty ->
	    %% Send commit records to the other involved nodes,
	    %% but do only wait for one node to complete.
	    %% Preferrably, the local node if possible.

	    ReadNode = val({Tab, where_to_read}),
	    {WaitFor, FirstRes} = async_send_dirty(Tid, CR, Tab, ReadNode),
	    rec_dirty(WaitFor, FirstRes);

	sync_dirty ->
	    %% Send commit records to the other involved nodes,
	    %% and wait for all nodes to complete
	    {WaitFor, FirstRes} = sync_send_dirty(Tid, CR, Tab, []),
	    rec_dirty(WaitFor, FirstRes);
	_ ->
	    mnesia:abort({bad_activity, Protocol})
    end.

%% This is the commit function, The first thing it does,
%% is to find out which nodes that have been participating
%% in this particular transaction, all of the mnesia_locker:lock*
%% functions insert the names of the nodes where it aquires locks
%% into the local shadow Store
%% This function exacutes in the context of the user process
t_commit(Type) ->
    {Mod, Tid, Ts} = get(mnesia_activity_state),
    Store = Ts#tidstore.store,
    if
	Ts#tidstore.level == 1 ->
	    intercept_friends(Tid, Ts),
	    %% N is number of updates
	    case arrange(Tid, Store, Type) of
		{N, Prep} when N > 0 ->
		    multi_commit(Prep#prep.protocol,
				 Tid, Prep#prep.records, Store);
		{0, Prep} ->
		    multi_commit(read_only, Tid, Prep#prep.records, Store)
	    end;
	true ->
	    %% nested commit
	    Level = Ts#tidstore.level,
	    [Obsolete | Tail] = Ts#tidstore.up_stores,
	    req({del_store, Tid, Store, Obsolete, false}),
	    NewTs = Ts#tidstore{store = Store,
				up_stores = Tail,
				level = Level - 1},
	    NewTidTs = {Mod, Tid, NewTs},
	    put(mnesia_activity_state, NewTidTs),
	    do_commit_nested
    end.

%% This function arranges for all objects we shall write in S to be
%% in a list of {Node, CommitRecord}
%% Important function for the performance of mnesia.

arrange(Tid, Store, Type) ->
    %% The local node is always included
    Nodes = get_nodes(Store),
    Recs = prep_recs(Nodes, []),
    Key = ?ets_first(Store),
    N = 0,
    Prep =
	case Type of
	    async -> #prep{protocol = sym_trans, records = Recs};
	    sync -> #prep{protocol = sync_sym_trans, records = Recs}
	end,
    case catch do_arrange(Tid, Store, Key, Prep, N) of
	{'EXIT', Reason} ->
	    dbg_out("do_arrange failed ~p ~p~n", [Reason, Tid]),
	    case Reason of
		{aborted, R} ->
		    mnesia:abort(R);
		_ ->
		    mnesia:abort(Reason)
	    end;
	{New, Prepared} ->
	    {New, Prepared#prep{records = reverse(Prepared#prep.records)}}
    end.

reverse([]) ->
    [];
reverse([H|R]) when record(H, commit) ->
    [
     H#commit{
       ram_copies       =  lists:reverse(H#commit.ram_copies),
       disc_copies      =  lists:reverse(H#commit.disc_copies),
       disc_only_copies =  lists:reverse(H#commit.disc_only_copies),
       snmp             = lists:reverse(H#commit.snmp)
      }
     | reverse(R)].

prep_recs([N | Nodes], Recs) ->
    prep_recs(Nodes, [#commit{decision = presume_commit, node = N} | Recs]);
prep_recs([], Recs) ->
    Recs.

%% storage_types is a list of {Node, Storage} tuples
%% where each tuple represents an active replica
do_arrange(Tid, Store, {Tab, Key}, Prep, N) ->
    Oid = {Tab, Key},
    Items = ?ets_lookup(Store, Oid), %% Store is a bag
    P2 = prepare_items(Tid, Tab, Key, Items, Prep),
    do_arrange(Tid, Store, ?ets_next(Store, Oid), P2, N + 1);
do_arrange(Tid, Store, SchemaKey, Prep, N) when SchemaKey == op ->
    Items = ?ets_lookup(Store, SchemaKey), %% Store is a bag
    P2 = prepare_schema_items(Tid, Items, Prep),
    do_arrange(Tid, Store, ?ets_next(Store, SchemaKey), P2, N + 1);
do_arrange(Tid, Store, RestoreKey, Prep, N) when RestoreKey == restore_op ->
    [{restore_op, R}] = ?ets_lookup(Store, RestoreKey),
    Fun = fun({Tab, Key}, CommitRecs, _RecName, Where, Snmp) ->
		  Item = [{{Tab, Key}, {Tab, Key}, delete}],
		  do_prepare_items(Tid, Tab, Key, Where, Snmp, Item, CommitRecs);
	     (BupRec, CommitRecs, RecName, Where, Snmp) ->
		  Tab = element(1, BupRec),
		  Key = element(2, BupRec),
		  Item =
		      if
			  Tab == RecName ->
			      [{{Tab, Key}, BupRec, write}];
			  true ->
			      BupRec2 = setelement(1, BupRec, RecName),
			      [{{Tab, Key}, BupRec2, write}]
		      end,
		  do_prepare_items(Tid, Tab, Key, Where, Snmp, Item, CommitRecs)
	  end,
    Recs2 = mnesia_schema:arrange_restore(R, Fun, Prep#prep.records),
    P2 = Prep#prep{protocol = asym_trans, records = Recs2},
    do_arrange(Tid, Store, ?ets_next(Store, RestoreKey), P2, N + 1);
do_arrange(_Tid, _Store, '$end_of_table', Prep, N) ->
    {N, Prep};
do_arrange(Tid, Store, IgnoredKey, Prep, N) -> %% locks, nodes ... local atoms...
    do_arrange(Tid, Store, ?ets_next(Store, IgnoredKey), Prep, N).

%% Returns a prep record  with all items in reverse order
prepare_schema_items(Tid, Items, Prep) ->
    Types = [{N, schema_ops} || N <- val({current, db_nodes})],
    Recs = prepare_nodes(Tid, Types, Items, Prep#prep.records, schema),
    Prep#prep{protocol = asym_trans, records = Recs}.

%% Returns a prep record with all items in reverse order
prepare_items(Tid, Tab, Key, Items, Prep) when Prep#prep.prev_tab == Tab ->
    Types = Prep#prep.prev_types,
    Snmp = Prep#prep.prev_snmp,
    Recs = Prep#prep.records,
    Recs2 = do_prepare_items(Tid, Tab, Key, Types, Snmp, Items, Recs),
    Prep#prep{records = Recs2};

prepare_items(Tid, Tab, Key, Items, Prep) ->
    Types = val({Tab, where_to_commit}),
    case Types of
	[] -> mnesia:abort({no_exists, Tab});
	{blocked, _} ->
	    unblocked = req({unblock_me, Tab}),
	    prepare_items(Tid, Tab, Key, Items, Prep);
	_ ->
	    Snmp = val({Tab, snmp}),
	    Recs2 = do_prepare_items(Tid, Tab, Key, Types,
				     Snmp, Items, Prep#prep.records),
	    Prep2 = Prep#prep{records = Recs2, prev_tab = Tab,
			      prev_types = Types, prev_snmp = Snmp},
	    check_prep(Prep2, Types)
    end.

do_prepare_items(Tid, Tab, Key, Types, Snmp, Items, Recs) ->
    Recs2 = prepare_snmp(Tid, Tab, Key, Types, Snmp, Items, Recs), % May exit
    prepare_nodes(Tid, Types, Items, Recs2, normal).

prepare_snmp(Tab, Key, Items) ->
    case val({Tab, snmp}) of
	[] ->
	    [];
	Ustruct when Key /= '_' ->
	    {_Oid, _Val, Op} = hd(Items),
	    %% Still making snmp oid (not used) because we want to catch errors here
	    %% And also it keeps backwards comp. with old nodes.
	    SnmpOid = mnesia_snmp_hook:key_to_oid(Tab, Key, Ustruct), % May exit
	    [{Op, Tab, Key, SnmpOid}];
	_ ->
	    [{clear_table, Tab}]
    end.

prepare_snmp(_Tid, _Tab, _Key, _Types, [], _Items, Recs) ->
    Recs;

prepare_snmp(Tid, Tab, Key, Types, Us, Items, Recs) ->
    if Key /= '_' ->
	    {_Oid, _Val, Op} = hd(Items),
	    SnmpOid = mnesia_snmp_hook:key_to_oid(Tab, Key, Us), % May exit
	    prepare_nodes(Tid, Types, [{Op, Tab, Key, SnmpOid}], Recs, snmp);
       Key == '_' ->
	    prepare_nodes(Tid, Types, [{clear_table, Tab}], Recs, snmp)
    end.

check_prep(Prep, Types) when Prep#prep.types == Types ->
    Prep;
check_prep(Prep, Types) when Prep#prep.types == undefined ->
    Prep#prep{types = Types};
check_prep(Prep, _Types) ->
    Prep#prep{protocol = asym_trans}.

%% Returns a list of commit records
prepare_nodes(Tid, [{Node, Storage} | Rest], Items, C, Kind) ->
    {Rec, C2} = pick_node(Tid, Node, C, []),
    Rec2 = prepare_node(Node, Storage, Items, Rec, Kind),
    [Rec2 | prepare_nodes(Tid, Rest, Items, C2, Kind)];
prepare_nodes(_Tid, [], _Items, CommitRecords, _Kind) ->
    CommitRecords.

pick_node(Tid, Node, [Rec | Rest], Done) ->
    if
	Rec#commit.node == Node ->
	    {Rec, Done ++ Rest};
	true ->
	    pick_node(Tid, Node, Rest, [Rec | Done])
    end;
pick_node(_Tid, Node, [], Done) ->
    {#commit{decision = presume_commit, node = Node}, Done}.

prepare_node(Node, Storage, [Item | Items], Rec, Kind) when Kind == snmp ->
    Rec2 = Rec#commit{snmp = [Item | Rec#commit.snmp]},
    prepare_node(Node, Storage, Items, Rec2, Kind);
prepare_node(Node, Storage, [Item | Items], Rec, Kind) when Kind /= schema ->
    Rec2 =
	case Storage of
	    ram_copies ->
		Rec#commit{ram_copies = [Item | Rec#commit.ram_copies]};
	    disc_copies ->
		Rec#commit{disc_copies = [Item | Rec#commit.disc_copies]};
	    disc_only_copies ->
		Rec#commit{disc_only_copies =
			   [Item | Rec#commit.disc_only_copies]}
	end,
    prepare_node(Node, Storage, Items, Rec2, Kind);
prepare_node(_Node, _Storage, Items, Rec, Kind)
  when Kind == schema, Rec#commit.schema_ops == []  ->
    Rec#commit{schema_ops = Items};
prepare_node(_Node, _Storage, [], Rec, _Kind) ->
    Rec.

%% multi_commit((Protocol, Tid, CommitRecords, Store)
%% Local work is always performed in users process
multi_commit(read_only, Tid, CR, _Store) ->
    %% This featherweight commit protocol is used when no
    %% updates has been performed in the transaction.

    {DiscNs, RamNs} = commit_nodes(CR, [], []),
    Msg = {Tid, simple_commit},
    rpc:abcast(DiscNs -- [node()], ?MODULE, Msg),
    rpc:abcast(RamNs -- [node()], ?MODULE, Msg),
    mnesia_recover:note_decision(Tid, committed),
    mnesia_locker:release_tid(Tid),
    ?MODULE ! {delete_transaction, Tid},
    do_commit;

multi_commit(sym_trans, Tid, CR, Store) ->
    %% This lightweight commit protocol is used when all
    %% the involved tables are replicated symetrically.
    %% Their storage types must match on each node.
    %%
    %% 1  Ask the other involved nodes if they want to commit
    %%    All involved nodes votes yes if they are up
    %% 2a Somebody has voted no
    %%    Tell all yes voters to do_abort
    %% 2b Everybody has voted yes
    %%    Tell everybody to do_commit. I.e. that they should
    %%    prepare the commit, log the commit record and
    %%    perform the updates.
    %%
    %%    The outcome is kept 3 minutes in the transient decision table.
    %%
    %% Recovery:
    %%    If somebody dies before the coordinator has
    %%    broadcasted do_commit, the transaction is aborted.
    %%
    %%    If a participant dies, the table load algorithm
    %%    ensures that the contents of the involved tables
    %%    are picked from another node.
    %%
    %%    If the coordinator dies, each participants checks
    %%    the outcome with all the others. If all are uncertain
    %%    about the outcome, the transaction is aborted. If
    %%    somebody knows the outcome the others will follow.

    {DiscNs, RamNs} = commit_nodes(CR, [], []),
    Pending = mnesia_checkpoint:tm_enter_pending(Tid, DiscNs, RamNs),
    ?ets_insert(Store, Pending),

    {WaitFor, Local} = ask_commit(sym_trans, Tid, CR, DiscNs, RamNs),
    {Outcome, []} = rec_all(WaitFor, Tid, do_commit, []),
    ?eval_debug_fun({?MODULE, multi_commit_sym},
		    [{tid, Tid}, {outcome, Outcome}]),
    rpc:abcast(DiscNs -- [node()], ?MODULE, {Tid, Outcome}),
    rpc:abcast(RamNs -- [node()], ?MODULE, {Tid, Outcome}),
    case Outcome of
	do_commit ->
	    mnesia_recover:note_decision(Tid, committed),
	    do_dirty(Tid, Local),
	    mnesia_locker:release_tid(Tid),
	    ?MODULE ! {delete_transaction, Tid};
	{do_abort, _Reason} ->
	    mnesia_recover:note_decision(Tid, aborted)
    end,
    ?eval_debug_fun({?MODULE, multi_commit_sym, post},
		    [{tid, Tid}, {outcome, Outcome}]),
    Outcome;

multi_commit(sync_sym_trans, Tid, CR, Store) ->
    %%   This protocol is the same as sym_trans except that it
    %%   uses syncronized calls to disk_log and syncronized commits
    %%   when several nodes are involved.

    {DiscNs, RamNs} = commit_nodes(CR, [], []),
    Pending = mnesia_checkpoint:tm_enter_pending(Tid, DiscNs, RamNs),
    ?ets_insert(Store, Pending),

    {WaitFor, Local} = ask_commit(sync_sym_trans, Tid, CR, DiscNs, RamNs),
    {Outcome, []} = rec_all(WaitFor, Tid, do_commit, []),
    ?eval_debug_fun({?MODULE, multi_commit_sym_sync},
		    [{tid, Tid}, {outcome, Outcome}]),
    rpc:abcast(DiscNs -- [node()], ?MODULE, {Tid, Outcome}),
    rpc:abcast(RamNs -- [node()], ?MODULE, {Tid, Outcome}),
    case Outcome of
	do_commit ->
	    mnesia_recover:note_decision(Tid, committed),
	    mnesia_log:slog(Local),
	    do_commit(Tid, Local),
	    %% Just wait for completion result is ignore.
	    rec_all(WaitFor, Tid, ignore, []),
	    mnesia_locker:release_tid(Tid),
	    ?MODULE ! {delete_transaction, Tid};
	{do_abort, _Reason} ->
	    mnesia_recover:note_decision(Tid, aborted)
    end,
    ?eval_debug_fun({?MODULE, multi_commit_sym, post},
		    [{tid, Tid}, {outcome, Outcome}]),
    Outcome;

multi_commit(asym_trans, Tid, CR, Store) ->
    %% This more expensive commit protocol is used when
    %% table definitions are changed (schema transactions).
    %% It is also used when the involved tables are
    %% replicated asymetrically. If the storage type differs
    %% on at least one node this protocol is used.
    %%
    %% 1 Ask the other involved nodes if they want to commit.
    %%   All involved nodes prepares the commit, logs a presume_abort
    %%   commit record and votes yes or no depending of the
    %%   outcome of the prepare. The preparation is also performed
    %%   by the coordinator.
    %%
    %% 2a Somebody has died or voted no
    %%    Tell all yes voters to do_abort
    %% 2b Everybody has voted yes
    %%    Put a unclear marker in the log.
    %%    Tell the others to pre_commit. I.e. that they should
    %%    put a unclear marker in the log and reply
    %%    acc_pre_commit when they are done.
    %%
    %% 3a Somebody died
    %%    Tell the remaining participants to do_abort
    %% 3b Everybody has replied acc_pre_commit
    %%    Tell everybody to committed. I.e that they should
    %%    put a committed marker in the log, perform the updates
    %%    and reply done_commit when they are done. The coordinator
    %%    must wait with putting his committed marker inte the log
    %%    until the committed has been sent to all the others.
    %%    Then he performs local commit before collecting replies.
    %%
    %% 4  Everybody has either died or replied done_commit
    %%    Return to the caller.
    %%
    %% Recovery:
    %%    If the coordinator dies, the participants (and
    %%    the coordinator when he starts again) must do
    %%    the following:
    %%
    %%    If we have no unclear marker in the log we may
    %%    safely abort, since we know that nobody may have
    %%    decided to commit yet.
    %%
    %%    If we have a committed marker in the log we may
    %%    safely commit since we know that everybody else
    %%    also will come to this conclusion.
    %%
    %%    If we have a unclear marker but no committed
    %%    in the log we are uncertain about the real outcome
    %%    of the transaction and must ask the others before
    %%    we can decide what to do. If someone knows the
    %%    outcome we will do the same. If nobody knows, we
    %%    will wait for the remaining involved nodes to come
    %%    up. When all involved nodes are up and uncertain,
    %%    we decide to commit (first put a committed marker
    %%    in the log, then do the updates).

    D = #decision{tid = Tid, outcome = presume_abort},
    {D2, CR2} = commit_decision(D, CR, [], []),
    DiscNs = D2#decision.disc_nodes,
    RamNs = D2#decision.ram_nodes,
    Pending = mnesia_checkpoint:tm_enter_pending(Tid, DiscNs, RamNs),
    ?ets_insert(Store, Pending),
    {WaitFor, Local} = ask_commit(asym_trans, Tid, CR2, DiscNs, RamNs),
    SchemaPrep = (catch mnesia_schema:prepare_commit(Tid, Local, {coord, WaitFor})),
    {Votes, Pids} = rec_all(WaitFor, Tid, do_commit, []),

    ?eval_debug_fun({?MODULE, multi_commit_asym_got_votes},
		    [{tid, Tid}, {votes, Votes}]),
    case Votes of
	do_commit ->
	    case SchemaPrep of
		{_Modified, C, DumperMode} when record(C, commit) ->
		    mnesia_log:log(C), % C is not a binary
		    ?eval_debug_fun({?MODULE, multi_commit_asym_log_commit_rec},
				    [{tid, Tid}]),

		    D3 = C#commit.decision,
		    D4 = D3#decision{outcome = unclear},
		    mnesia_recover:log_decision(D4),
		    ?eval_debug_fun({?MODULE, multi_commit_asym_log_commit_dec},
				    [{tid, Tid}]),
		    tell_participants(Pids, {Tid, pre_commit}),
		    %% Now we are uncertain and we do not know
		    %% if all participants have logged that
		    %% they are uncertain or not
		    rec_acc_pre_commit(Pids, Tid, Store, C,
				       do_commit, DumperMode, [], []);
		{'EXIT', Reason} ->
		    %% The others have logged the commit
		    %% record but they are not uncertain
		    mnesia_recover:note_decision(Tid, aborted),
		    ?eval_debug_fun({?MODULE, multi_commit_asym_prepare_exit},
				    [{tid, Tid}]),
		    tell_participants(Pids, {Tid, {do_abort, Reason}}),
		    do_abort(Tid, Local),
		    {do_abort, Reason}
	    end;

	{do_abort, Reason} ->
	    %% The others have logged the commit
	    %% record but they are not uncertain
	    mnesia_recover:note_decision(Tid, aborted),
	    ?eval_debug_fun({?MODULE, multi_commit_asym_do_abort}, [{tid, Tid}]),
	    tell_participants(Pids, {Tid, {do_abort, Reason}}),
	    do_abort(Tid, Local),
	    {do_abort, Reason}
    end.

%% Returns do_commit or {do_abort, Reason}
rec_acc_pre_commit([Pid | Tail], Tid, Store, Commit, Res, DumperMode,
		   GoodPids, SchemaAckPids) ->
    receive
	{?MODULE, _, {acc_pre_commit, Tid, Pid, true}} ->
	    rec_acc_pre_commit(Tail, Tid, Store, Commit, Res, DumperMode,
			       [Pid | GoodPids], [Pid | SchemaAckPids]);

	{?MODULE, _, {acc_pre_commit, Tid, Pid, false}} ->
	    rec_acc_pre_commit(Tail, Tid, Store, Commit, Res, DumperMode,
			       [Pid | GoodPids], SchemaAckPids);

	{?MODULE, _, {acc_pre_commit, Tid, Pid}} ->
	    %% Kept for backwards compatibility. Remove after Mnesia 4.x
	    rec_acc_pre_commit(Tail, Tid, Store, Commit, Res, DumperMode,
			       [Pid | GoodPids], [Pid | SchemaAckPids]);

	{mnesia_down, Node} when Node == node(Pid) ->
	    AbortRes = {do_abort, {bad_commit, Node}},
	    rec_acc_pre_commit(Tail, Tid, Store, Commit, AbortRes, DumperMode,
			       GoodPids, SchemaAckPids)
    end;
rec_acc_pre_commit([], Tid, Store, Commit, Res, DumperMode, GoodPids, SchemaAckPids) ->
    D = Commit#commit.decision,
    case Res of
	do_commit ->
	    %% Now everybody knows that the others
	    %% has voted yes. We also know that
	    %% everybody are uncertain.
	    prepare_sync_schema_commit(Store, SchemaAckPids),
	    tell_participants(GoodPids, {Tid, committed}),
	    D2 = D#decision{outcome = committed},
	    mnesia_recover:log_decision(D2),
            ?eval_debug_fun({?MODULE, rec_acc_pre_commit_log_commit},
			    [{tid, Tid}]),

	    %% Now we have safely logged committed
	    %% and we can recover without asking others
	    do_commit(Tid, Commit, DumperMode),
            ?eval_debug_fun({?MODULE, rec_acc_pre_commit_done_commit},
			    [{tid, Tid}]),
	    sync_schema_commit(Tid, Store, SchemaAckPids),
	    mnesia_locker:release_tid(Tid),
	    ?MODULE ! {delete_transaction, Tid};

	{do_abort, Reason} ->
	    tell_participants(GoodPids, {Tid, {do_abort, Reason}}),
	    D2 = D#decision{outcome = aborted},
	    mnesia_recover:log_decision(D2),
            ?eval_debug_fun({?MODULE, rec_acc_pre_commit_log_abort},
			    [{tid, Tid}]),
	    do_abort(Tid, Commit),
	    ?eval_debug_fun({?MODULE, rec_acc_pre_commit_done_abort},
			    [{tid, Tid}])
    end,
    Res.

%% Note all nodes in case of mnesia_down mgt
prepare_sync_schema_commit(_Store, []) ->
    ok;
prepare_sync_schema_commit(Store, [Pid | Pids]) ->
    ?ets_insert(Store, {waiting_for_commit_ack, node(Pid)}),
    prepare_sync_schema_commit(Store, Pids).

sync_schema_commit(_Tid, _Store, []) ->
    ok;
sync_schema_commit(Tid, Store, [Pid | Tail]) ->
    receive
	{?MODULE, _, {schema_commit, Tid, Pid}} ->
	    ?ets_match_delete(Store, {waiting_for_commit_ack, node(Pid)}),
	    sync_schema_commit(Tid, Store, Tail);

	{mnesia_down, Node} when Node == node(Pid) ->
	    ?ets_match_delete(Store, {waiting_for_commit_ack, Node}),
	    sync_schema_commit(Tid, Store, Tail)
    end.

tell_participants([Pid | Pids], Msg) ->
    Pid ! Msg,
    tell_participants(Pids, Msg);
tell_participants([], _Msg) ->
    ok.

%% No need for trapping exits. We are only linked
%% to mnesia_tm and if it dies we should also die.
%% The same goes for disk_log and dets.
commit_participant(Coord, Tid, Bin, DiscNs, RamNs) when binary(Bin) ->
    Commit = binary_to_term(Bin),
    commit_participant(Coord, Tid, Bin, Commit, DiscNs, RamNs);
commit_participant(Coord, Tid, C, DiscNs, RamNs) when record(C, commit) ->
    commit_participant(Coord, Tid, C, C, DiscNs, RamNs).

commit_participant(Coord, Tid, Bin, C0, DiscNs, _RamNs) ->
    ?eval_debug_fun({?MODULE, commit_participant, pre}, [{tid, Tid}]),
    case catch mnesia_schema:prepare_commit(Tid, C0, {part, Coord}) of
	{Modified, C, DumperMode} when record(C, commit) ->
	    %% If we can not find any local unclear decision
	    %% we should presume abort at startup recovery
	    case lists:member(node(), DiscNs) of
		false ->
		    ignore;
		true ->
		    case Modified of
			false -> mnesia_log:log(Bin);
			true  -> mnesia_log:log(C)
		    end
	    end,
	    ?eval_debug_fun({?MODULE, commit_participant, vote_yes},
			    [{tid, Tid}]),
	    reply(Coord, {vote_yes, Tid, self()}),

	    receive
		{Tid, pre_commit} ->
		    D = C#commit.decision,
		    mnesia_recover:log_decision(D#decision{outcome = unclear}),
		    ?eval_debug_fun({?MODULE, commit_participant, pre_commit},
				    [{tid, Tid}]),
		    Expect_schema_ack = C#commit.schema_ops /= [],
		    reply(Coord, {acc_pre_commit, Tid, self(), Expect_schema_ack}),

		    %% Now we are vulnerable for failures, since
		    %% we cannot decide without asking others
		    receive
			{Tid, committed} ->
			    mnesia_recover:log_decision(D#decision{outcome = committed}),
			    ?eval_debug_fun({?MODULE, commit_participant, log_commit},
					    [{tid, Tid}]),
			    do_commit(Tid, C, DumperMode),
			    case Expect_schema_ack of
				false -> ignore;
				true -> reply(Coord, {schema_commit, Tid, self()})
			    end,
			    ?eval_debug_fun({?MODULE, commit_participant, do_commit},
					    [{tid, Tid}]);

			{Tid, {do_abort, _Reason}} ->
			    mnesia_recover:log_decision(D#decision{outcome = aborted}),
			    ?eval_debug_fun({?MODULE, commit_participant, log_abort},
					    [{tid, Tid}]),
			    mnesia_schema:undo_prepare_commit(Tid, C),
			    ?eval_debug_fun({?MODULE, commit_participant, undo_prepare},
					    [{tid, Tid}]);

			{'EXIT', _, _} ->
			    mnesia_recover:log_decision(D#decision{outcome = aborted}),
			    ?eval_debug_fun({?MODULE, commit_participant, exit_log_abort},
					    [{tid, Tid}]),
			    mnesia_schema:undo_prepare_commit(Tid, C),
			    ?eval_debug_fun({?MODULE, commit_participant, exit_undo_prepare},
					    [{tid, Tid}]);

			Msg ->
			    verbose("** ERROR ** commit_participant ~p, got unexpected msg: ~p~n",
				  [Tid, Msg])
		    end;
		{Tid, {do_abort, _Reason}} ->
		    mnesia_schema:undo_prepare_commit(Tid, C),
		    ?eval_debug_fun({?MODULE, commit_participant, pre_commit_undo_prepare},
				    [{tid, Tid}]);

		{'EXIT', _, _} ->
		    mnesia_schema:undo_prepare_commit(Tid, C),
		    ?eval_debug_fun({?MODULE, commit_participant, pre_commit_undo_prepare}, [{tid, Tid}]);

		Msg ->
		    verbose("** ERROR ** commit_participant ~p, got unexpected msg: ~p~n",
			  [Tid, Msg])
	    end;

	{'EXIT', Reason} ->
	    ?eval_debug_fun({?MODULE, commit_participant, vote_no},
			    [{tid, Tid}]),
	    reply(Coord, {vote_no, Tid, Reason}),
	    mnesia_schema:undo_prepare_commit(Tid, C0)
    end,
    mnesia_locker:release_tid(Tid),
    ?MODULE ! {delete_transaction, Tid},
    unlink(whereis(?MODULE)),
    exit(normal).

do_abort(Tid, Bin) when binary(Bin) ->
    %% Possible optimization:
    %% If we want we could pass around a flag
    %% that tells us whether the binary contains
    %% schema ops or not. Only if the binary
    %% contains schema ops there are meningful
    %% unpack the binary and perform
    %% mnesia_schema:undo_prepare_commit/1.
    do_abort(Tid, binary_to_term(Bin));
do_abort(Tid, Commit) ->
    mnesia_schema:undo_prepare_commit(Tid, Commit),
    Commit.

do_dirty(Tid, Commit) when Commit#commit.schema_ops == [] ->
    mnesia_log:log(Commit),
    do_commit(Tid, Commit).

%% do_commit(Tid, CommitRecord)
do_commit(Tid, Bin) when binary(Bin) ->
    do_commit(Tid, binary_to_term(Bin));
do_commit(Tid, C) ->
    do_commit(Tid, C, optional).
do_commit(Tid, Bin, DumperMode) when binary(Bin) ->
    do_commit(Tid, binary_to_term(Bin), DumperMode);
do_commit(Tid, C, DumperMode) ->
    mnesia_dumper:update(Tid, C#commit.schema_ops, DumperMode),
    R  = do_snmp(Tid, C#commit.snmp),
    R2 = do_update(Tid, ram_copies, C#commit.ram_copies, R),
    R3 = do_update(Tid, disc_copies, C#commit.disc_copies, R2),
    do_update(Tid, disc_only_copies, C#commit.disc_only_copies, R3).

%% Update the items
do_update(Tid, Storage, [Op | Ops], OldRes) ->
    case catch do_update_op(Tid, Storage, Op) of
	ok ->
	    do_update(Tid, Storage, Ops, OldRes);
	{'EXIT', Reason} ->
	    %% This may only happen when we recently have
	    %% deleted our local replica, changed storage_type
	    %% or transformed table
	    %% BUGBUG: Updates may be lost if storage_type is changed.
	    %%         Determine actual storage type and try again.
	    %% BUGBUG: Updates may be lost if table is transformed.

	    verbose("do_update in ~w failed: ~p -> {'EXIT', ~p}~n",
		    [Tid, Op, Reason]),
	    do_update(Tid, Storage, Ops, OldRes);
	NewRes ->
	    do_update(Tid, Storage, Ops, NewRes)
    end;
do_update(_Tid, _Storage, [], Res) ->
    Res.

do_update_op(Tid, Storage, {{Tab, K}, Obj, write}) ->
    commit_write(?catch_val({Tab, commit_work}), Tid,
		 Tab, K, Obj, undefined),
    mnesia_lib:db_put(Storage, Tab, Obj);

do_update_op(Tid, Storage, {{Tab, K}, Val, delete}) ->
    commit_delete(?catch_val({Tab, commit_work}), Tid, Tab, K, Val, undefined),
    mnesia_lib:db_erase(Storage, Tab, K);

do_update_op(Tid, Storage, {{Tab, K}, {RecName, Incr}, update_counter}) ->
    {NewObj, OldObjs} =
        case catch mnesia_lib:db_update_counter(Storage, Tab, K, Incr) of
            NewVal when integer(NewVal), NewVal >= 0 ->
                {{RecName, K, NewVal}, [{RecName, K, NewVal - Incr}]};
            _ ->
                Zero = {RecName, K, 0},
                mnesia_lib:db_put(Storage, Tab, Zero),
                {Zero, []}
        end,
    commit_update(?catch_val({Tab, commit_work}), Tid, Tab,
		  K, NewObj, OldObjs),
    element(3, NewObj);

do_update_op(Tid, Storage, {{Tab, Key}, Obj, delete_object}) ->
    commit_del_object(?catch_val({Tab, commit_work}),
		      Tid, Tab, Key, Obj, undefined),
    mnesia_lib:db_match_erase(Storage, Tab, Obj);

do_update_op(Tid, Storage, {{Tab, Key}, Obj, clear_table}) ->
    commit_clear(?catch_val({Tab, commit_work}), Tid, Tab, Key, Obj),
    mnesia_lib:db_match_erase(Storage, Tab, Obj).

commit_write([], _, _, _, _, _) -> ok;
commit_write([{checkpoints, CpList}|R], Tid, Tab, K, Obj, Old) ->
    mnesia_checkpoint:tm_retain(Tid, Tab, K, write, CpList),
    commit_write(R, Tid, Tab, K, Obj, Old);
commit_write([H|R], Tid, Tab, K, Obj, Old)
  when element(1, H) == subscribers ->
    mnesia_subscr:report_table_event(H, Tab, Tid, Obj, write, Old),
    commit_write(R, Tid, Tab, K, Obj, Old);
commit_write([H|R], Tid, Tab, K, Obj, Old)
  when element(1, H) == index ->
    mnesia_index:add_index(H, Tab, K, Obj, Old),
    commit_write(R, Tid, Tab, K, Obj, Old).

commit_update([], _, _, _, _, _) -> ok;
commit_update([{checkpoints, CpList}|R], Tid, Tab, K, Obj, _) ->
    Old = mnesia_checkpoint:tm_retain(Tid, Tab, K, write, CpList),
    commit_update(R, Tid, Tab, K, Obj, Old);
commit_update([H|R], Tid, Tab, K, Obj, Old)
  when element(1, H) == subscribers ->
    mnesia_subscr:report_table_event(H, Tab, Tid, Obj, write, Old),
    commit_update(R, Tid, Tab, K, Obj, Old);
commit_update([H|R], Tid, Tab, K, Obj, Old)
  when element(1, H) == index ->
    mnesia_index:add_index(H, Tab, K, Obj, Old),
    commit_update(R, Tid, Tab, K, Obj, Old).

commit_delete([], _, _, _, _, _) ->  ok;
commit_delete([{checkpoints, CpList}|R], Tid, Tab, K, Obj, _) ->
    Old = mnesia_checkpoint:tm_retain(Tid, Tab, K, delete, CpList),
    commit_delete(R, Tid, Tab, K, Obj, Old);
commit_delete([H|R], Tid, Tab, K, Obj, Old)
  when element(1, H) == subscribers ->
    mnesia_subscr:report_table_event(H, Tab, Tid, Obj, delete, Old),
    commit_delete(R, Tid, Tab, K, Obj, Old);
commit_delete([H|R], Tid, Tab, K, Obj, Old)
  when element(1, H) == index ->
    mnesia_index:delete_index(H, Tab, K),
    commit_delete(R, Tid, Tab, K, Obj, Old).

commit_del_object([], _, _, _, _, _) -> ok;
commit_del_object([{checkpoints, CpList}|R], Tid, Tab, K, Obj, _) ->
    Old = mnesia_checkpoint:tm_retain(Tid, Tab, K, delete_object, CpList),
    commit_del_object(R, Tid, Tab, K, Obj, Old);
commit_del_object([H|R], Tid, Tab, K, Obj, Old)
  when element(1, H) == subscribers ->
    mnesia_subscr:report_table_event(H, Tab, Tid, Obj, delete_object, Old),
    commit_del_object(R, Tid, Tab, K, Obj, Old);
commit_del_object([H|R], Tid, Tab, K, Obj, Old)
  when element(1, H) == index ->
    mnesia_index:del_object_index(H, Tab, K, Obj, Old),
    commit_del_object(R, Tid, Tab, K, Obj, Old).

commit_clear([], _, _, _, _) ->  ok;
commit_clear([{checkpoints, CpList}|R], Tid, Tab, K, Obj) ->
    mnesia_checkpoint:tm_retain(Tid, Tab, K, clear_table, CpList),
    commit_clear(R, Tid, Tab, K, Obj);
commit_clear([H|R], Tid, Tab, K, Obj)
  when element(1, H) == subscribers ->
    mnesia_subscr:report_table_event(H, Tab, Tid, Obj, clear_table, undefined),
    commit_clear(R, Tid, Tab, K, Obj);
commit_clear([H|R], Tid, Tab, K, Obj)
  when element(1, H) == index ->
    mnesia_index:clear_index(H, Tab, K, Obj),
    commit_clear(R, Tid, Tab, K, Obj).

do_snmp(_, []) ->   ok;
do_snmp(Tid, [Head | Tail]) ->
    case catch mnesia_snmp_hook:update(Head) of
	{'EXIT', Reason} ->
	    %% This should only happen when we recently have
	    %% deleted our local replica or recently deattached
	    %% the snmp table

	    verbose("do_snmp in ~w failed: ~p -> {'EXIT', ~p}~n",
		    [Tid, Head, Reason]);
	ok ->
	    ignore
    end,
    do_snmp(Tid, Tail).

commit_nodes([C | Tail], AccD, AccR)
        when C#commit.disc_copies == [],
             C#commit.disc_only_copies  == [],
             C#commit.schema_ops == [] ->
    commit_nodes(Tail, AccD, [C#commit.node | AccR]);
commit_nodes([C | Tail], AccD, AccR) ->
    commit_nodes(Tail, [C#commit.node | AccD], AccR);
commit_nodes([], AccD, AccR) ->
    {AccD, AccR}.

commit_decision(D, [C | Tail], AccD, AccR) ->
    N = C#commit.node,
    {D2, Tail2} =
	case C#commit.schema_ops of
	    [] when C#commit.disc_copies == [],
		    C#commit.disc_only_copies  == [] ->
		commit_decision(D, Tail, AccD, [N | AccR]);
	    [] ->
		commit_decision(D, Tail, [N | AccD], AccR);
	    Ops ->
		case ram_only_ops(N, Ops) of
		    true ->
			commit_decision(D, Tail, AccD, [N | AccR]);
		    false ->
			commit_decision(D, Tail, [N | AccD], AccR)
		end
	end,
    {D2, [C#commit{decision = D2} | Tail2]};
commit_decision(D, [], AccD, AccR) ->
    {D#decision{disc_nodes = AccD, ram_nodes = AccR}, []}.

ram_only_ops(N, [{op, change_table_copy_type, N, _FromS, _ToS, Cs} | _Ops ]) ->
    case lists:member({name, schema}, Cs) of
	true ->
	    %% We always use disk if change type of the schema
	    false;
	false ->
	    not lists:member(N, val({schema, disc_copies}))
    end;

ram_only_ops(N, _Ops) ->
    not lists:member(N, val({schema, disc_copies})).

%% Returns {WaitFor, Res}
sync_send_dirty(Tid, [Head | Tail], Tab, WaitFor) ->
    Node = Head#commit.node,
    if
	Node == node() ->
	    {WF, _} = sync_send_dirty(Tid, Tail, Tab, WaitFor),
	    Res =  do_dirty(Tid, Head),
	    {WF, Res};
	true ->
	    {?MODULE, Node} ! {self(), {sync_dirty, Tid, Head, Tab}},
	    sync_send_dirty(Tid, Tail, Tab, [Node | WaitFor])
    end;
sync_send_dirty(_Tid, [], _Tab, WaitFor) ->
    {WaitFor, {'EXIT', {aborted, {node_not_running, WaitFor}}}}.

%% Returns {WaitFor, Res}
async_send_dirty(_Tid, _Nodes, Tab, nowhere) ->
    {[], {'EXIT', {aborted, {no_exists, Tab}}}};
async_send_dirty(Tid, Nodes, Tab, ReadNode) ->
    async_send_dirty(Tid, Nodes, Tab, ReadNode, [], ok).

async_send_dirty(Tid, [Head | Tail], Tab, ReadNode, WaitFor, Res) ->
    Node = Head#commit.node,
    if
	ReadNode == Node, Node == node() ->
	    NewRes =  do_dirty(Tid, Head),
	    async_send_dirty(Tid, Tail, Tab, ReadNode, WaitFor, NewRes);
	ReadNode == Node ->
	    {?MODULE, Node} ! {self(), {sync_dirty, Tid, Head, Tab}},
	    NewRes = {'EXIT', {aborted, {node_not_running, Node}}},
	    async_send_dirty(Tid, Tail, Tab, ReadNode, [Node | WaitFor], NewRes);
	true ->
	    {?MODULE, Node} ! {self(), {async_dirty, Tid, Head, Tab}},
	    async_send_dirty(Tid, Tail, Tab, ReadNode, WaitFor, Res)
    end;
async_send_dirty(_Tid, [], _Tab, _ReadNode, WaitFor, Res) ->
    {WaitFor, Res}.

rec_dirty([Node | Tail], Res) when Node /= node() ->
    NewRes = get_dirty_reply(Node, Res),
    rec_dirty(Tail, NewRes);
rec_dirty([], Res) ->
    Res.

get_dirty_reply(Node, Res) ->
    receive
	{?MODULE, Node, {'EXIT', Reason}} ->
	    {'EXIT', {aborted, {badarg, Reason}}};
	{?MODULE, Node, {dirty_res, ok}} ->
	    case Res of
		{'EXIT', {aborted, {node_not_running, _Node}}} ->
		    ok;
		_ ->
		    %% Prioritize bad results, but node_not_running
		    Res
	    end;
	{?MODULE, Node, {dirty_res, Reply}} ->
	    Reply;
	{mnesia_down, Node} ->
	    %% It's ok to ignore mnesia_down's
	    %% since we will make the replicas
	    %% consistent again when Node is started
	    Res
    after 1000 ->
	    case lists:member(Node, val({current, db_nodes})) of
		true ->
		    get_dirty_reply(Node, Res);
		false ->
		    Res
	    end
    end.

%% Assume that CommitRecord is no binary
%% Return {Res, Pids}
ask_commit(Protocol, Tid, CR, DiscNs, RamNs) ->
    ask_commit(Protocol, Tid, CR, DiscNs, RamNs, [], no_local).

ask_commit(Protocol, Tid, [Head | Tail], DiscNs, RamNs, WaitFor, Local) ->
    Node = Head#commit.node,
    if
	Node == node() ->
	    ask_commit(Protocol, Tid, Tail, DiscNs, RamNs, WaitFor, Head);
	true ->
	    Bin = opt_term_to_binary(Protocol, Head, DiscNs++RamNs),
	    Msg = {ask_commit, Protocol, Tid, Bin, DiscNs, RamNs},
	    {?MODULE, Node} ! {self(), Msg},
	    ask_commit(Protocol, Tid, Tail, DiscNs, RamNs, [Node | WaitFor], Local)
    end;
ask_commit(_Protocol, _Tid, [], _DiscNs, _RamNs, WaitFor, Local) ->
    {WaitFor, Local}.

opt_term_to_binary(asym_trans, Head, Nodes) ->
    opt_term_to_binary(Nodes, Head);
opt_term_to_binary(_Protocol, Head, _Nodes) ->
    Head.

opt_term_to_binary([], Head) ->
    term_to_binary(Head);
opt_term_to_binary([H|R], Head) ->
    case mnesia_monitor:needs_protocol_conversion(H) of
	true -> Head;
	false ->
	    opt_term_to_binary(R, Head)
    end.

rec_all([Node | Tail], Tid, Res, Pids) ->
    receive
	{?MODULE, Node, {vote_yes, Tid}} ->
	    rec_all(Tail, Tid, Res, Pids);
	{?MODULE, Node, {vote_yes, Tid, Pid}} ->
	    rec_all(Tail, Tid, Res, [Pid | Pids]);
	{?MODULE, Node, {vote_no, Tid, Reason}} ->
	    rec_all(Tail, Tid, {do_abort, Reason}, Pids);
	{?MODULE, Node, {committed, Tid}} ->
	    rec_all(Tail, Tid, Res, Pids);
	{?MODULE, Node, {aborted, Tid}} ->
	    rec_all(Tail, Tid, Res, Pids);

	{mnesia_down, Node} ->
	    rec_all(Tail, Tid, {do_abort, {bad_commit, Node}}, Pids)
    end;
rec_all([], _Tid, Res, Pids) ->
    {Res, Pids}.

get_transactions() ->
    {info, Participant, Coordinator} = req(info),
    lists:map(fun({Tid, _Tabs}) ->
		      Status = tr_status(Tid,Participant),
		      {Tid#tid.counter, Tid#tid.pid, Status}
	      end,Coordinator).

tr_status(Tid,Participant) ->
    case lists:keymember(Tid, 1, Participant) of
	true -> participant;
	false  -> coordinator
    end.

get_info(Timeout) ->
    case whereis(?MODULE) of
	undefined ->
	    {timeout, Timeout};
	Pid ->
	    Pid ! {self(), info},
	    receive
		{?MODULE, _, {info, Part, Coord}} ->
		    {info, Part, Coord}
	    after Timeout ->
		    {timeout, Timeout}
	    end
    end.

display_info(Stream, {timeout, T}) ->
    io:format(Stream, "---> No info about coordinator and participant transactions, "
	      "timeout ~p <--- ~n", [T]);

display_info(Stream, {info, Part, Coord}) ->
    io:format(Stream, "---> Participant transactions <--- ~n", []),
    lists:foreach(fun(P) -> pr_participant(Stream, P) end, Part),
    io:format(Stream, "---> Coordinator transactions <---~n", []),
    lists:foreach(fun({Tid, _Tabs}) -> pr_tid(Stream, Tid) end, Coord).

pr_participant(Stream, P) ->
    Commit0 = P#participant.commit,
    Commit =
	if
	    binary(Commit0) -> binary_to_term(Commit0);
	    true -> Commit0
	end,
    pr_tid(Stream, P#participant.tid),
    io:format(Stream, "with participant objects ~p~n", [Commit]).


pr_tid(Stream, Tid) ->
    io:format(Stream, "Tid: ~p (owned by ~p) ~n",
	      [Tid#tid.counter, Tid#tid.pid]).

info(Serial) ->
    io:format( "Info about transaction with serial == ~p~n", [Serial]),
    {info, Participant, Trs} = req(info),
    search_pr_participant(Serial, Participant),
    search_pr_coordinator(Serial, Trs).


search_pr_coordinator(_S, []) -> no;
search_pr_coordinator(S, [{Tid, _Ts}|Tail]) ->
    case Tid#tid.counter of
	S ->
	    io:format( "Tid is coordinator, owner == \n", []),
	    display_pid_info(Tid#tid.pid),
	    search_pr_coordinator(S, Tail);
	_ ->
	    search_pr_coordinator(S, Tail)
    end.

search_pr_participant(_S, []) ->
    false;
search_pr_participant(S, [ P | Tail]) ->
    Tid = P#participant.tid,
    Commit0 = P#participant.commit,
    if
	Tid#tid.counter == S ->
	    io:format( "Tid is participant to commit, owner == \n", []),
	    Pid = Tid#tid.pid,
	    display_pid_info(Pid),
	    io:format( "Tid wants to write objects \n",[]),
	    Commit =
		if
		    binary(Commit0) -> binary_to_term(Commit0);
		    true -> Commit0
		end,

	    io:format("~p~n", [Commit]),
	    search_pr_participant(S,Tail);  %% !!!!!
	true ->
	    search_pr_participant(S, Tail)
    end.

display_pid_info(Pid) ->
    case rpc:pinfo(Pid) of
	undefined ->
	    io:format( "Dead process \n");
	Info ->
	    Call = fetch(initial_call, Info),
	    Curr = case fetch(current_function, Info) of
		       {Mod,F,Args} when list(Args) ->
			   {Mod,F,length(Args)};
		       Other ->
			   Other
		   end,
	    Reds  = fetch(reductions, Info),
	    LM = fetch(message_queue_len, Info),
	    pformat(io_lib:format("~p", [Pid]),
		    io_lib:format("~p", [Call]),
		    io_lib:format("~p", [Curr]), Reds, LM)
    end.

pformat(A1, A2, A3, A4, A5) ->
    io:format( "~-12s ~-21s ~-21s ~9w ~4w~n", [A1,A2,A3,A4,A5]).

fetch(Key, Info) ->
    case lists:keysearch(Key, 1, Info) of
	{value, {_, Val}} ->
	    Val;
	_ ->
	    0
    end.


%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%  reconfigure stuff comes here ......
%%%%%%%%%%%%%%%%%%%%%

reconfigure_coordinators(N, [{Tid, [Store | _]} | Coordinators]) ->
    case mnesia_recover:outcome(Tid, unknown) of
	committed ->
	    WaitingNodes = ?ets_lookup(Store, waiting_for_commit_ack),
	    case lists:keymember(N, 2, WaitingNodes) of
		false ->
		    ignore; % avoid spurious mnesia_down messages
		true ->
		    send_mnesia_down(Tid, Store, N)
	    end;
	aborted ->
	    ignore; % avoid spurious mnesia_down messages
	_ ->
	    %% Tell the coordinator about the mnesia_down
	    send_mnesia_down(Tid, Store, N)
    end,
    reconfigure_coordinators(N, Coordinators);
reconfigure_coordinators(_N, []) ->
    ok.

send_mnesia_down(Tid, Store, Node) ->
    Msg = {mnesia_down, Node},
    send_to_pids([Tid#tid.pid | get_friends(Store)], Msg).

send_to_pids([Pid | Pids], Msg) ->
    Pid ! Msg,
    send_to_pids(Pids, Msg);
send_to_pids([], _Msg) ->
    ok.

reconfigure_participants(N, [P | Tail]) ->
    case lists:member(N, P#participant.disc_nodes) or
	 lists:member(N, P#participant.ram_nodes) of
	false ->
	    %% Ignore, since we are not a participant
	    %% in the transaction.
	    reconfigure_participants(N, Tail);

	true ->
	    %% We are on a participant node, lets
	    %% check if the dead one was a
	    %% participant or a coordinator.
	    Tid  = P#participant.tid,
	    if
		node(Tid#tid.pid) /= N ->
		    %% Another participant node died. Ignore.
		    reconfigure_participants(N, Tail);

		true ->
		    %% The coordinator node has died and
		    %% we must determine the outcome of the
		    %% transaction and tell mnesia_tm on all
		    %% nodes (including the local node) about it
		    verbose("Coordinator ~p in transaction ~p died~n",
			    [Tid#tid.pid, Tid]),

		    Nodes = P#participant.disc_nodes ++
			    P#participant.ram_nodes,
		    AliveNodes = Nodes  -- [N],
		    Protocol =  P#participant.protocol,
		    tell_outcome(Tid, Protocol, N, AliveNodes, AliveNodes),
		    reconfigure_participants(N, Tail)
	    end
    end;
reconfigure_participants(_, []) ->
    [].

%% We need to determine the outcome of the transaction and
%% tell mnesia_tm on all involved nodes (including the local node)
%% about the outcome.
tell_outcome(Tid, Protocol, Node, CheckNodes, TellNodes) ->
    Outcome = mnesia_recover:what_happened(Tid, Protocol, CheckNodes),
    case Outcome of
	aborted ->
	    rpc:abcast(TellNodes, ?MODULE, {Tid,{do_abort, {mnesia_down, Node}}});
	committed ->
	    rpc:abcast(TellNodes, ?MODULE, {Tid, do_commit})
    end,
    Outcome.

do_stop(#state{coordinators = Coordinators}) ->
    Msg = {mnesia_down, node()},
    lists:foreach(fun({Tid, _}) -> Tid#tid.pid ! Msg end, Coordinators),
    mnesia_checkpoint:stop(),
    mnesia_log:stop(),
    exit(shutdown).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% System upgrade

system_continue(_Parent, _Debug, State) ->
    doit_loop(State).

system_terminate(_Reason, _Parent, _Debug, State) ->
    do_stop(State).

system_code_change(State, _Module, _OldVsn, _Extra) ->
    {ok, State}.
