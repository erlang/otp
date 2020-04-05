%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2018
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

%%
-module(mnesia_checkpoint).

%% TM callback interface
-export([
	 tm_add_copy/2,
	 tm_change_table_copy_type/3,
	 tm_del_copy/2,
	 tm_mnesia_down/1,
	 tm_prepare/1,
	 tm_retain/4,
	 tm_retain/5,
	 tm_enter_pending/1,
	 tm_enter_pending/3,
	 tm_exit_pending/1
	]).

%% Public interface
-export([
	 activate/1,
	 checkpoints/0,
	 deactivate/1,
	 deactivate/2,
	 iterate/6,
	 most_local_node/2,
	 really_retain/2,
	 stop/0,
	 stop_iteration/1,
	 tables_and_cookie/1
	]).

%% Internal
-export([
	 call/2,
	 cast/2,
	 init/1,
	 remote_deactivate/1,
	 start/1
	]).

%% sys callback interface
-export([
	 system_code_change/4,
	 system_continue/3,
	 system_terminate/4
	]).

-include("mnesia.hrl").
-import(mnesia_lib, [add/2, del/2, set/2, unset/1]).
-import(mnesia_lib, [dbg_out/2]).

-record(checkpoint_args, {name = {erlang:unique_integer([positive]), node()},
			  allow_remote = true,
			  ram_overrides_dump = false,
			  nodes = [],
			  node = node(),
			  now,  %% unused
			  cookie = ?unique_cookie,
			  min = [],
			  max = [],
			  pending_tab,
			  wait_for_old, % Initially undefined then List
			  is_activated = false,
			  ignore_new = [],
			  retainers = [],
			  iterators = [],
			  supervisor,
			  pid
			 }).

-record(retainer, {cp_name, tab_name, store, writers = [], really_retain = true}).

-record(iter, {tab_name, oid_tab, main_tab, retainer_tab, source, val, pid}).

-record(pending, {tid, disc_nodes = [], ram_nodes = []}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TM callback functions

stop() ->
    lists:foreach(fun(Name) -> call(Name, stop) end,
		  checkpoints()),
    ok.

tm_prepare(Cp) when is_record(Cp, checkpoint_args) ->
    Name = Cp#checkpoint_args.name,
    case lists:member(Name, checkpoints()) of
	false ->
	    start_retainer(Cp);
	true ->
	    {error, {already_exists, Name, node()}}
    end.

tm_mnesia_down(Node) ->
    lists:foreach(fun(Name) -> cast(Name, {mnesia_down, Node}) end,
		  checkpoints()).

%% Returns pending
tm_enter_pending(Tid, DiscNs, RamNs) ->
    Pending = #pending{tid = Tid, disc_nodes = DiscNs, ram_nodes = RamNs},
    tm_enter_pending(Pending).

tm_enter_pending(Pending) ->
    PendingTabs = val(pending_checkpoints),
    tm_enter_pending(PendingTabs, Pending).

tm_enter_pending([], Pending) ->
    Pending;
tm_enter_pending([Tab | Tabs], Pending) ->
    %% io:format("Add ~p ~p ~p~n",[Tab, Pending, hd(tl(element(2, process_info(self(), current_stacktrace))))]),
    ?SAFE(?ets_insert(Tab, Pending)),
    tm_enter_pending(Tabs, Pending).

tm_exit_pending(Tid) ->
    Pids = val(pending_checkpoint_pids),
    tm_exit_pending(Pids, Tid).
    
tm_exit_pending([], Tid) ->
    Tid;
tm_exit_pending([Pid | Pids], Tid) ->
    Pid ! {self(), {exit_pending, Tid}},
    tm_exit_pending(Pids, Tid).

enter_still_pending([Tid | Tids], Tab) ->
    ?ets_insert(Tab, #pending{tid = Tid}),
    enter_still_pending(Tids, Tab);
enter_still_pending([], _Tab) ->
    ok.


%% Looks up checkpoints for functions in mnesia_tm.
tm_retain(Tid, Tab, Key, Op) ->
    case val({Tab, commit_work}) of
	[{checkpoints, Checkpoints} | _ ] ->
	    tm_retain(Tid, Tab, Key, Op, Checkpoints);
	_ -> 
	    undefined
    end.
    
tm_retain(Tid, Tab, Key, Op, Checkpoints) ->
    case Op of
	clear_table ->
	    OldRecs = mnesia_lib:db_match_object(Tab, '_'),
	    send_group_retain(OldRecs, Checkpoints, Tid, Tab, []),
	    OldRecs;
	_ ->
	    OldRecs = mnesia_lib:db_get(Tab, Key),
	    send_retain(Checkpoints, {retain, Tid, Tab, Key, OldRecs}),
	    OldRecs
    end.

send_group_retain([Rec | Recs], Checkpoints, Tid, Tab, [PrevRec | PrevRecs])
  when element(2, Rec) /= element(2, PrevRec) ->
    Key = element(2, PrevRec),
    OldRecs = lists:reverse([PrevRec | PrevRecs]),
    send_retain(Checkpoints, {retain, Tid, Tab, Key, OldRecs}),
    send_group_retain(Recs, Checkpoints, Tid, Tab, [Rec]);
send_group_retain([Rec | Recs], Checkpoints, Tid, Tab, Acc) ->
    send_group_retain(Recs, Checkpoints, Tid, Tab, [Rec | Acc]);
send_group_retain([], Checkpoints, Tid, Tab, [PrevRec | PrevRecs]) ->
    Key = element(2, PrevRec),
    OldRecs = lists:reverse([PrevRec | PrevRecs]),
    send_retain(Checkpoints, {retain, Tid, Tab, Key, OldRecs}),
    ok;
send_group_retain([], _Checkpoints, _Tid, _Tab, []) ->
    ok.

send_retain([Name | Names], Msg) ->
    cast(Name, Msg),
    send_retain(Names, Msg);
send_retain([], _Msg) ->
    ok.
    
tm_add_copy(Tab, Node) when Node /= node() ->
    case val({Tab, commit_work}) of
	[{checkpoints, Checkpoints} | _ ] ->
	    Fun = fun(Name) -> call(Name, {add_copy, Tab, Node}) end,
	    map_call(Fun, Checkpoints, ok);
	_  -> 
	    ok
    end.

tm_del_copy(Tab, Node) when Node == node() ->
    mnesia_subscr:unsubscribe_table(Tab),
    case val({Tab, commit_work}) of
	[{checkpoints, Checkpoints} | _ ] ->	    
	    Fun = fun(Name) -> call(Name, {del_copy, Tab, Node}) end,
	    map_call(Fun, Checkpoints, ok);
	_ ->
	    ok
    end.

tm_change_table_copy_type(Tab, From, To) ->
    case val({Tab, commit_work}) of
	[{checkpoints, Checkpoints} | _ ] ->
	    Fun = fun(Name) -> call(Name, {change_copy, Tab, From, To}) end,
	    map_call(Fun, Checkpoints, ok);
	_ -> 
	    ok
    end.

map_call(Fun, [Name | Names], Res) ->
    case Fun(Name) of
	 ok ->
	    map_call(Fun, Names, Res);
	{error, {no_exists, Name}} ->
	    map_call(Fun, Names, Res);
	{error, Reason} ->
	    %% BUGBUG: We may end up with some checkpoint retainers
	    %% too much in the add_copy case. How do we remove them?
	    map_call(Fun, Names, {error, Reason})
    end;
map_call(_Fun, [], Res) ->
    Res.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public functions

deactivate(Name) ->
    case call(Name, get_checkpoint) of
	{error, Reason} ->
	    {error, Reason};
	Cp ->
	    deactivate(Cp#checkpoint_args.nodes, Name)
    end.

deactivate(Nodes, Name) ->
    rpc:multicall(Nodes, ?MODULE, remote_deactivate, [Name]),
    ok.

remote_deactivate(Name) ->
    call(Name, deactivate).

checkpoints() -> val(checkpoints).

tables_and_cookie(Name) ->
    case call(Name, get_checkpoint) of
	{error, Reason} ->
	    {error, Reason};
	Cp ->
	    Tabs = Cp#checkpoint_args.min ++ Cp#checkpoint_args.max,
	    Cookie = Cp#checkpoint_args.cookie,
	    {ok, Tabs, Cookie}
    end.

most_local_node(Name, Tab) ->
    case ?catch_val({Tab, {retainer, Name}}) of
	{'EXIT', _} -> 
	    {error, {"No retainer attached to table", [Tab, Name]}};
	R -> 	    
	    Writers = R#retainer.writers,
	    LocalWriter = lists:member(node(), Writers),
	    if
		LocalWriter == true ->
		    {ok, node()};
		Writers /= [] ->
		    {ok, hd(Writers)};
		true  ->
		    {error, {"No retainer attached to table", [Tab, Name]}}
	    end
    end.

really_retain(Name, Tab) ->
    R = val({Tab, {retainer, Name}}),
    R#retainer.really_retain.
 
%% Activate a checkpoint.
%%
%% A checkpoint is a transaction consistent state that may be used to
%% perform a distributed backup or to rollback the involved tables to
%% their old state. Backups may also be used to restore tables to
%% their old state. Args is a list of the following tuples:
%%
%% {name, Name}
%%    Name of checkpoint. Each checkpoint must have a name which
%%    is unique on the reachable nodes. The name may be reused when
%%    the checkpoint has been deactivated.
%%    By default a probably unique name is generated.
%%    Multiple checkpoints may be set on the same table.
%%
%% {allow_remote, Bool}
%%   false means that all retainers must be local. If the
%%   table does not reside locally, the checkpoint fails.
%%   true allows retainers on other nodes.
%%
%% {min, MinTabs}
%%   Minimize redundancy and only keep checkpoint info together with
%%   one replica, preferrably at the local node. If any node involved
%%   the checkpoint goes down, the checkpoint is deactivated.
%%
%% {max, MaxTabs}
%%    Maximize redundancy and keep checkpoint info together with all
%%    replicas. The checkpoint becomes more fault tolerant if the
%%    tables has several replicas. When new replicas are added, they
%%    will also get a retainer attached to them.
%%
%% {ram_overrides_dump, Bool}
%% {ram_overrides_dump, Tabs}
%%   Only applicable for ram_copies. Bool controls which versions of
%%   the records that should be included in the checkpoint state.
%%   true means that the latest comitted records in ram (i.e. the
%%   records that the application accesses) should be included
%%   in the checkpoint. false means that the records dumped to
%%   dat-files (the records that will be loaded at startup) should
%%   be included in the checkpoint. Tabs is a list of tables.
%%   Default is false.
%%
%% {ignore_new, TidList}
%%   Normally we wait for all pending transactions to complete
%%   before we allow iteration over the checkpoint. But in order
%%   to cope with checkpoint activation inside a transaction that
%%   currently prepares commit (mnesia_init:get_net_work_copy) we
%%   need to have the ability to ignore the enclosing transaction.
%%   We do not wait for the transactions in TidList to end. The
%%   transactions in TidList are regarded as newer than the checkpoint.

activate(Args) ->
    case args2cp(Args) of
	{ok, Cp} ->
	    do_activate(Cp);
	{error, Reason} ->
	    {error, Reason}
    end.

args2cp(Args) when is_list(Args)->
    try lists:foldl(fun check_arg/2, #checkpoint_args{}, Args) of
	Cp ->
	    case check_tables(Cp) of
		{error, Reason} ->
		    {error, Reason};
		{ok, Overriders, AllTabs} ->
		    arrange_retainers(Cp, Overriders, AllTabs)
	    end
    catch exit:Reason ->
	    {error, Reason};
	  error:Reason ->
	    {error, Reason}
    end;
args2cp(Args) ->
    {error, {badarg, Args}}.

check_arg({name, Name}, Cp) ->
    case lists:member(Name, checkpoints()) of
	true ->
	    exit({already_exists, Name});
	false ->
	    try
		[_|_] = tab2retainer({foo, Name}),
		Cp#checkpoint_args{name = Name}
	    catch _:_ ->
		    exit({badarg, Name})
	    end
    end;
check_arg({allow_remote, true}, Cp) ->
    Cp#checkpoint_args{allow_remote = true};
check_arg({allow_remote, false}, Cp) ->
    Cp#checkpoint_args{allow_remote = false};
check_arg({ram_overrides_dump, true}, Cp) ->
    Cp#checkpoint_args{ram_overrides_dump = true};
check_arg({ram_overrides_dump, false}, Cp) ->
    Cp#checkpoint_args{ram_overrides_dump = false};
check_arg({ram_overrides_dump, Tabs}, Cp) when is_list(Tabs) ->
    Cp#checkpoint_args{ram_overrides_dump = Tabs};
check_arg({min, Tabs}, Cp) when is_list(Tabs) ->
    Cp#checkpoint_args{min = Tabs};
check_arg({max, Tabs}, Cp) when is_list(Tabs) ->
    Cp#checkpoint_args{max = Tabs};
check_arg({ignore_new, Tids}, Cp) when is_list(Tids) ->
    Cp#checkpoint_args{ignore_new = Tids};
check_arg(Arg, _) ->
    exit({badarg, Arg}).

check_tables(Cp) ->
    Min = Cp#checkpoint_args.min,
    Max = Cp#checkpoint_args.max,
    AllTabs = Min ++ Max,
    DoubleTabs = [T || T <- Min, lists:member(T, Max)],
    Overriders = Cp#checkpoint_args.ram_overrides_dump,
    if
	DoubleTabs /= [] ->
	    {error, {combine_error, Cp#checkpoint_args.name,
		     [{min, DoubleTabs}, {max, DoubleTabs}]}};
	Min == [], Max == [] ->
	    {error, {combine_error, Cp#checkpoint_args.name,
		     [{min, Min}, {max, Max}]}};
	Overriders == false ->
	    {ok, [], AllTabs};
	Overriders == true ->
	    {ok, AllTabs, AllTabs};
	is_list(Overriders) ->
	    case [T || T <- Overriders, not lists:member(T, Min)] of
		[] ->
		    case [T || T <- Overriders, not lists:member(T, Max)] of
			[] ->
			    {ok, Overriders, AllTabs};
			Outsiders ->
			    {error, {combine_error, Cp#checkpoint_args.name,
				     [{ram_overrides_dump, Outsiders},
				      {max, Outsiders}]}}
		    end;
		Outsiders ->
		    {error, {combine_error, Cp#checkpoint_args.name,
			     [{ram_overrides_dump, Outsiders},
			      {min, Outsiders}]}}
	    end
    end.

arrange_retainers(Cp, Overriders, AllTabs) ->
    R = #retainer{cp_name = Cp#checkpoint_args.name},
    try [R#retainer{tab_name = Tab,
		    writers = select_writers(Cp, Tab)}
	 || Tab <- AllTabs] of
	Retainers ->
	    {ok, Cp#checkpoint_args{ram_overrides_dump = Overriders,
				    retainers = Retainers,
				    nodes = writers(Retainers)}}
    catch throw:Reason ->
	    {error, Reason}
    end.

select_writers(Cp, Tab) ->
    case filter_remote(Cp, val({Tab, active_replicas})) of
	[] ->
	    throw({"Cannot prepare checkpoint (replica not available)",
		   [Tab, Cp#checkpoint_args.name]});
	Writers ->
	    This = node(),
	    case {lists:member(Tab, Cp#checkpoint_args.max),
		  lists:member(This, Writers)} of
		{true, _} -> Writers; % Max
		{false, true} -> [This];
		{false, false} -> [hd(Writers)]
	    end
    end.

filter_remote(Cp, Writers) when Cp#checkpoint_args.allow_remote == true ->
    Writers;
filter_remote(_Cp, Writers) ->
    This = node(),
    case lists:member(This, Writers) of
	true -> [This];
	false  -> []
    end.
	
writers(Retainers) ->
    Fun = fun(R, Acc) -> R#retainer.writers ++ Acc end,
    Writers = lists:foldl(Fun, [], Retainers),
    mnesia_lib:uniq(Writers).

do_activate(Cp) ->
    Name = Cp#checkpoint_args.name,
    Nodes = Cp#checkpoint_args.nodes,
    case mnesia_tm:prepare_checkpoint(Nodes, Cp) of
	{Replies, []} ->
	    check_prep(Replies, Name, Nodes, Cp#checkpoint_args.ignore_new);
	{_, BadNodes} ->
	    {error, {"Cannot prepare checkpoint (bad nodes)",
		     [Name, BadNodes]}}
    end.
	
check_prep([{ok, Name, IgnoreNew, _Node} | Replies], Name, Nodes, IgnoreNew) ->
    check_prep(Replies, Name, Nodes, IgnoreNew);
check_prep([{error, Reason} | _Replies], Name, _Nodes, _IgnoreNew) ->
    {error, {"Cannot prepare checkpoint (bad reply)",
	     [Name, Reason]}};
check_prep([{badrpc, Reason} | _Replies], Name, _Nodes, _IgnoreNew) ->
    {error, {"Cannot prepare checkpoint (badrpc)",
	     [Name, Reason]}};
check_prep([], Name, Nodes, IgnoreNew) ->
    collect_pending(Name, Nodes, IgnoreNew).

collect_pending(Name, Nodes, IgnoreNew) ->
    case rpc:multicall(Nodes, ?MODULE, call, [Name, collect_pending]) of
	{Replies, []} ->
	    try
		UnionTab = ?ets_new_table(mnesia_union, [bag]),
		compute_union(Replies, Nodes, Name, UnionTab, IgnoreNew)
	    catch error:Reason -> %% system limit
		    Msg = "Cannot create an ets table pending union",
		    {error, {system_limit, Msg, Reason}}
	    end;
	{_, BadNodes} ->
	    deactivate(Nodes, Name),
	    {error, {"Cannot collect from pending checkpoint", Name, BadNodes}}
    end.

compute_union([{ok, Pending} | Replies], Nodes, Name, UnionTab, IgnoreNew) ->
    add_pending(Pending, UnionTab),
    compute_union(Replies, Nodes, Name, UnionTab, IgnoreNew);
compute_union([{error, Reason} | _Replies], Nodes, Name, UnionTab, _IgnoreNew) ->
    deactivate(Nodes, Name),
    ?ets_delete_table(UnionTab),
    {error, Reason};
compute_union([{badrpc, Reason} | _Replies], Nodes, Name, UnionTab, _IgnoreNew) ->
    deactivate(Nodes, Name),
    ?ets_delete_table(UnionTab),
    {error, {badrpc, Reason}};
compute_union([], Nodes, Name, UnionTab, IgnoreNew) ->
    send_activate(Nodes, Nodes, Name, UnionTab, IgnoreNew).

add_pending([P | Pending], UnionTab) ->
    add_pending_node(P#pending.disc_nodes, P#pending.tid, UnionTab),
    add_pending_node(P#pending.ram_nodes, P#pending.tid, UnionTab),
    add_pending(Pending, UnionTab);
add_pending([], _UnionTab) ->
    ok.

add_pending_node([Node | Nodes], Tid, UnionTab) ->
    ?ets_insert(UnionTab, {Node, Tid}),
    add_pending_node(Nodes, Tid, UnionTab);
add_pending_node([], _Tid, _UnionTab) ->
    ok.

send_activate([Node | Nodes], AllNodes, Name, UnionTab, IgnoreNew) ->
    Pending = [Tid || {_, Tid} <- ?ets_lookup(UnionTab, Node), 
		      not lists:member(Tid, IgnoreNew)],
    case rpc:call(Node, ?MODULE, call, [Name, {activate, Pending}]) of
	activated ->
	    send_activate(Nodes, AllNodes, Name, UnionTab, IgnoreNew);
	{badrpc, Reason} ->
	    deactivate(Nodes, Name),
	    ?ets_delete_table(UnionTab),
	    {error, {"Activation failed (bad node)", Name, Node, Reason}};
	{error, Reason} ->
	    deactivate(Nodes, Name),
	    ?ets_delete_table(UnionTab),
	    {error, {"Activation failed", Name, Node, Reason}}
    end;
send_activate([], AllNodes, Name, UnionTab, _IgnoreNew) ->
    ?ets_delete_table(UnionTab),
    {ok, Name, AllNodes}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Checkpoint server

cast(Name, Msg) ->
    case ?catch_val({checkpoint, Name}) of
	{'EXIT', _} ->
	    {error, {no_exists, Name}};
	
	Pid when is_pid(Pid) ->
	    Pid ! {self(), Msg},
	    {ok, Pid}
    end.

call(Name, Msg) ->
    case ?catch_val({checkpoint, Name}) of
	{'EXIT', _} ->
	    {error, {no_exists, Name}};

	Pid when is_pid(Pid) ->
	    Monitor = erlang:monitor(process, Pid), %catch link(Pid), % Always local
	    Pid ! {self(), Msg},
	    Self = self(),
	    receive
		{'EXIT', Pid, Reason} ->
		    {error, {"Got exit", [Name, Reason]}};
		{'DOWN', Monitor, _, Pid, Reason} ->
		    {error, {"Got exit", [Name, Reason]}};
		{Name, Self, Reply} ->
		    erlang:demonitor(Monitor),
		    Reply
	    end;
	Error ->
	    Error
    end.

abcast(Nodes, Name, Msg) ->
    rpc:eval_everywhere(Nodes, ?MODULE, cast, [Name, Msg]).

reply(nopid, _Name, _Reply) ->
    ignore;
reply(ReplyTo, Name, Reply) ->
    ReplyTo ! {Name, ReplyTo, Reply}.

%% Returns {ok, NewCp} or {error, Reason}
start_retainer(Cp) ->
    % Will never be restarted
    Name = Cp#checkpoint_args.name,
    case supervisor:start_child(mnesia_checkpoint_sup, [Cp]) of
	{ok, _Pid} ->
	    {ok, Name, Cp#checkpoint_args.ignore_new, node()};
	{error, Reason} ->
	    {error, {"Cannot create checkpoint retainer",
		     Name, node(), Reason}}
    end.

start(Cp) ->
    Name = Cp#checkpoint_args.name,
    Args = [Cp#checkpoint_args{supervisor = self()}],
    mnesia_monitor:start_proc({?MODULE, Name}, ?MODULE, init, Args).

init(Cp) ->
    process_flag(trap_exit, true),
    process_flag(priority, high), %% Needed dets files might starve the system
    Name = Cp#checkpoint_args.name,
    Props = [set, public, {keypos, 2}],
    try ?ets_new_table(mnesia_pending_checkpoint, Props) of
	PendingTab ->
	    Rs = [prepare_tab(Cp, R) || R <- Cp#checkpoint_args.retainers],
	    Cp2 = Cp#checkpoint_args{retainers = Rs,
				pid = self(),
				pending_tab = PendingTab},
	    add(pending_checkpoint_pids, self()),
	    add(pending_checkpoints, PendingTab),
	    set({checkpoint, Name}, self()),
	    add(checkpoints, Name),
	    dbg_out("Checkpoint ~p (~p) started~n", [Name, self()]),
	    proc_lib:init_ack(Cp2#checkpoint_args.supervisor, {ok, self()}),
	    retainer_loop(Cp2)
    catch error:Reason -> %% system limit
	    Msg = "Cannot create an ets table for pending transactions",
	    Error = {error, {system_limit, Name, Msg, Reason}},
	    proc_lib:init_ack(Cp#checkpoint_args.supervisor, Error)
    end.
    
prepare_tab(Cp, R) ->
    Tab = R#retainer.tab_name,
    prepare_tab(Cp, R, val({Tab, storage_type})).

prepare_tab(Cp, R, Storage) ->
    Tab = R#retainer.tab_name,
    Name = R#retainer.cp_name,
    case lists:member(node(), R#retainer.writers) of
	true ->
	    R2 = retainer_create(Cp, R, Tab, Name, Storage),
	    set({Tab, {retainer, Name}}, R2),
	    %% Keep checkpoint info for table_info & mnesia_session 
	    add({Tab, checkpoints}, Name), 
	    add_chkp_info(Tab, Name),
	    R2;
	false ->
	    set({Tab, {retainer, Name}}, R#retainer{store = undefined}),
	    R
    end.

add_chkp_info(Tab, Name) ->
    case val({Tab, commit_work}) of
	[{checkpoints, OldList} | CommitList] ->
	    case lists:member(Name, OldList) of 
		true -> 
		    ok;
		false -> 
		    NewC = [{checkpoints, [Name | OldList]} | CommitList],
		    mnesia_lib:set({Tab, commit_work}, NewC)
	    end;
	CommitList ->
	    Chkp = {checkpoints, [Name]},
	    %% OBS checkpoints needs to be first in the list!
	    mnesia_lib:set({Tab, commit_work}, [Chkp | CommitList])
    end.

tab2retainer({Tab, Name}) ->
    FlatName = lists:flatten(io_lib:write(Name)),
    mnesia_lib:dir(lists:concat([?MODULE, "_", Tab, "_", FlatName, ".RET"])).

retainer_create(_Cp, R, Tab, Name, Ext = {ext, Alias, Mod}) ->
    T = {Tab, retainer, Name},
    P = mnesia_schema:cs2list(val({Tab, cstruct})),
    Mod:delete_table(Alias, T),
    ok = Mod:create_table(Alias, T, P),
    Cs = val({Tab, cstruct}),
    Mod:load_table(Alias, T, {retainer, create_table},
		   mnesia_schema:cs2list(Cs)),
    dbg_out("Checkpoint retainer created ~p ~tp~n", [Name, Tab]),
    R#retainer{store = {Ext, T}, really_retain = true};
retainer_create(_Cp, R, Tab, Name, disc_only_copies) ->
    Fname = tab2retainer({Tab, Name}),
    file:delete(Fname),
    Args = [{file, Fname}, {type, set}, {keypos, 2}, {repair, false}],
    {ok, _} = mnesia_lib:dets_sync_open({Tab, Name}, Args),
    dbg_out("Checkpoint retainer created ~p ~tp~n", [Name, Tab]),
    R#retainer{store = {dets, {Tab, Name}}, really_retain = true};
retainer_create(Cp, R, Tab, Name, Storage) ->
    T = ?ets_new_table(mnesia_retainer, [set, public, {keypos, 2}]),
    Overriders = Cp#checkpoint_args.ram_overrides_dump,
    ReallyR = R#retainer.really_retain,
    ReallyCp = lists:member(Tab, Overriders),
    ReallyR2 = prepare_ram_tab(Tab, T, Storage, ReallyR, ReallyCp),
    dbg_out("Checkpoint retainer created ~p ~tp~n", [Name, Tab]),
    R#retainer{store = {ets, T}, really_retain = ReallyR2}.

%% Copy the dumped table into retainer if needed
%% If the really_retain flag already has been set to false,
%% it should remain false even if we change storage type
%% while the checkpoint is activated.
prepare_ram_tab(Tab, T, ram_copies, true, false) ->
    Fname = mnesia_lib:tab2dcd(Tab),
    case mnesia_lib:exists(Fname) of
	true -> 
	    Log = mnesia_log:open_log(prepare_ram_tab, 
				      mnesia_log:dcd_log_header(), 
				      Fname, true, 
				      mnesia_monitor:get_env(auto_repair), 
				      read_only),
	    Add = fun(Rec) ->
			  Key = element(2, Rec),
			  Recs =
			      case ?ets_lookup(T, Key) of
				  [] -> [];
				  [{_, _, Old}] -> Old
			      end,
			  ?ets_insert(T, {Tab, Key, [Rec | Recs]}),
			  continue
		  end,
	    traverse_dcd(mnesia_log:chunk_log(Log, start), Log, Add),
	    mnesia_log:close_log(Log);
	false ->
	    ok
    end,
    false;
prepare_ram_tab(_, _, _, ReallyRetain, _) ->
    ReallyRetain.

traverse_dcd({Cont, [LogH | Rest]}, Log, Fun) 
  when is_record(LogH, log_header),  
       LogH#log_header.log_kind == dcd_log, 
       LogH#log_header.log_version >= "1.0" ->    
    traverse_dcd({Cont, Rest}, Log, Fun);   %% BUGBUG Error handling repaired files
traverse_dcd({Cont, Recs}, Log, Fun) ->     %% trashed data?? 
    lists:foreach(Fun, Recs), 
    traverse_dcd(mnesia_log:chunk_log(Log, Cont), Log, Fun);
traverse_dcd(eof, _Log, _Fun) ->
    ok.

retainer_get({{ext, Alias, Mod}, Store}, Key) ->
    Mod:lookup(Alias, Store, Key);
retainer_get({ets, Store}, Key) -> ?ets_lookup(Store, Key);
retainer_get({dets, Store}, Key) -> dets:lookup(Store, Key).

retainer_put({{ext, Alias, Mod}, Store}, Val) ->
    Mod:insert(Alias, Store, Val);
retainer_put({ets, Store}, Val) -> ?ets_insert(Store, Val);
retainer_put({dets, Store}, Val) -> dets:insert(Store, Val).

retainer_first({{ext, Alias, Mod}, Store}) ->
    Mod:first(Alias, Store);
retainer_first({ets, Store}) -> ?ets_first(Store);
retainer_first({dets, Store}) -> dets:first(Store).
 
retainer_next({{ext, Alias, Mod}, Store}, Key) ->
    Mod:next(Alias, Store, Key);
retainer_next({ets, Store}, Key) -> ?ets_next(Store, Key);
retainer_next({dets, Store}, Key) -> dets:next(Store, Key).

%% retainer_next_slot(Tab, Pos) ->
%%     case retainer_slot(Tab, Pos) of
%% 	   '$end_of_table' ->
%% 	       '$end_of_table';
%% 	   [] ->
%% 	       retainer_next_slot(Tab, Pos + 1);
%% 	   Recs when is_list(Recs) ->
%% 	       {Pos, Recs}
%%     end.
%% 
%% retainer_slot({ets, Store}, Pos) -> ?ets_next(Store, Pos);
%% retainer_slot({dets, Store}, Pos) -> dets:slot(Store, Pos).

retainer_fixtable(Tab, Bool) when is_atom(Tab) ->
    mnesia_lib:db_fixtable(val({Tab, storage_type}), Tab, Bool);
retainer_fixtable({Ext = {ext, _, _}, Tab}, Bool) ->
    mnesia_lib:db_fixtable(Ext, Tab, Bool);
retainer_fixtable({ets, Tab}, Bool) ->
    mnesia_lib:db_fixtable(ram_copies, Tab, Bool);
retainer_fixtable({dets, Tab}, Bool) ->
    mnesia_lib:db_fixtable(disc_only_copies, Tab, Bool).

retainer_delete({{ext, Alias, Mod}, Store}) ->
    Mod:close_table(Alias, Store),
    Mod:delete_table(Alias, Store);
retainer_delete({ets, Store}) ->
    ?ets_delete_table(Store);
retainer_delete({dets, Store}) ->
    mnesia_lib:dets_sync_close(Store),
    Fname = tab2retainer(Store),
    file:delete(Fname).

retainer_loop(Cp = #checkpoint_args{is_activated=false, name=Name}) ->
    receive
	%% Adm
	{From, {activate, Pending}} ->
            StillPending = mnesia_recover:still_pending(Pending),
            enter_still_pending(StillPending, Cp#checkpoint_args.pending_tab),
	    Local = [Tid || #tid{pid=Pid} = Tid <- StillPending, node(Pid) =/= node()],
            Cp2 = maybe_activate(Cp#checkpoint_args{wait_for_old = Local}),

            reply(From, Name, activated),
	    retainer_loop(Cp2);

	{_From, {exit_pending, Tid}} when is_list(Cp#checkpoint_args.wait_for_old) ->
	    StillPending = lists:delete(Tid, Cp#checkpoint_args.wait_for_old),
	    Cp2 = Cp#checkpoint_args{wait_for_old = StillPending},
	    Cp3 = maybe_activate(Cp2),
	    retainer_loop(Cp3);

	{From, deactivate} ->
	    do_stop(Cp),
	    reply(From, Name, deactivated),
	    unlink(From),
	    exit(shutdown);

	{From, get_checkpoint} ->
	    reply(From, Name, Cp),
	    retainer_loop(Cp);
	{_From, {add_retainer, R, Node}} ->
	    Cp2 = do_add_retainer(Cp, R, Node),
	    retainer_loop(Cp2);

	{From, collect_pending} ->
	    PendingTab = Cp#checkpoint_args.pending_tab,
	    del(pending_checkpoints, PendingTab),
	    Pending = ?ets_match_object(PendingTab, '_'),
	    reply(From, Name, {ok, Pending}),
	    retainer_loop(Cp);

	{_From, {mnesia_down, Node}} ->
	    Cp2 = do_del_retainers(Cp, Node),
	    retainer_loop(Cp2);

	{'EXIT', Parent, _} when Parent == Cp#checkpoint_args.supervisor ->
	    %% do_stop(Cp),
	    %% assume that entire Mnesia is terminating
	    exit(shutdown);

	{'EXIT', From, _Reason} ->
	    Iters = [Iter || Iter <- Cp#checkpoint_args.iterators,
			     check_iter(From, Iter)],
	    retainer_loop(Cp#checkpoint_args{iterators = Iters});

	{system, From, Msg} ->
	    dbg_out("~p got {system, ~p, ~tp}~n", [?MODULE, From, Msg]),
	    sys:handle_system_msg(Msg, From, Cp#checkpoint_args.supervisor,
				  ?MODULE, [], Cp)
    end;

retainer_loop(Cp = #checkpoint_args{name=Name}) ->
    receive
	{_From, {retain, Tid, Tab, Key, OldRecs}} ->
	    R = ?catch_val({Tab, {retainer, Name}}),
	    PendingTab = Cp#checkpoint_args.pending_tab,
            case is_record(R, retainer) andalso R#retainer.really_retain of
		true ->
		    Store = R#retainer.store,
		    try true = ets:member(PendingTab, Tid),
			 %% io:format("CP: ~p ~p ~p ~p~n",[true, Tab, Key, Tid]),
			 case retainer_get(Store, Key) of
			     [] -> ignore;
			     _  -> ets:delete(element(2,Store), Key)
			 end
		    catch _:_ ->
			    %% io:format("CP: ~p ~p ~p ~p~n",[false, Tab, Key, Tid]),
			    case retainer_get(Store, Key) of
				[] -> retainer_put(Store, {Tab, Key, OldRecs});
				_  -> already_retained
			    end
		    end;
		false ->
		    ignore
	    end,
	    retainer_loop(Cp);

	%% Adm
	{From, get_checkpoint} ->
	    reply(From, Name, Cp),
	    retainer_loop(Cp);
	{From, {add_copy, Tab, Node}} ->
	    {Res, Cp2} = do_add_copy(Cp, Tab, Node),
	    reply(From, Name, Res),
	    retainer_loop(Cp2);
	{From, {del_copy, Tab, Node}} ->
	    Cp2 = do_del_copy(Cp, Tab, Node),
	    reply(From, Name, ok),
	    retainer_loop(Cp2);
	{From, {change_copy, Tab, From, To}} ->
	    Cp2 = do_change_copy(Cp, Tab, From, To),
	    reply(From, Name, ok),
	    retainer_loop(Cp2);
	{_From, {add_retainer, R, Node}} ->
	    Cp2 = do_add_retainer(Cp, R, Node),
	    retainer_loop(Cp2);
	{_From, {del_retainer, R, Node}} ->
	    Cp2 = do_del_retainer(Cp, R, Node),
	    retainer_loop(Cp2);

	%% Iteration
	{From, {iter_begin, Iter}} ->
	    Cp2 = iter_begin(Cp, From, Iter),
	    retainer_loop(Cp2);

	{From, {iter_end, Iter}}  ->
	    ?SAFE(retainer_fixtable(Iter#iter.oid_tab, false)),
	    Iters = Cp#checkpoint_args.iterators -- [Iter],
	    reply(From, Name, ok),
	    retainer_loop(Cp#checkpoint_args{iterators = Iters});

	{_From, {exit_pending, _Tid}} ->
	    retainer_loop(Cp);

	{From, deactivate} ->
	    do_stop(Cp),
	    reply(From, Name, deactivated),
	    unlink(From),
	    exit(shutdown);

	{_From, {mnesia_down, Node}} ->
	    Cp2 = do_del_retainers(Cp, Node),
	    retainer_loop(Cp2);

	{'EXIT', Parent, _} when Parent == Cp#checkpoint_args.supervisor ->
	    %% do_stop(Cp),
	    %% assume that entire Mnesia is terminating
	    exit(shutdown);

	{'EXIT', From, _Reason} ->
	    Iters = [Iter || Iter <- Cp#checkpoint_args.iterators,
			     check_iter(From, Iter)],
	    retainer_loop(Cp#checkpoint_args{iterators = Iters});

	{system, From, Msg} ->
	    dbg_out("~p got {system, ~p, ~tp}~n", [?MODULE, From, Msg]),
	    sys:handle_system_msg(Msg, From, Cp#checkpoint_args.supervisor,
				  ?MODULE, [], Cp);
	Msg ->
	    dbg_out("~p got ~tp~n", [?MODULE, Msg])
    end.

maybe_activate(Cp)
  when Cp#checkpoint_args.wait_for_old == [],
       Cp#checkpoint_args.is_activated == false ->
    Cp#checkpoint_args{%% pending_tab = undefined,
		       is_activated = true};
maybe_activate(Cp) ->
    Cp.

iter_begin(Cp, From, Iter) ->
    Name = Cp#checkpoint_args.name,
    R = val({Iter#iter.tab_name, {retainer, Name}}),
    Iter2 = init_tabs(R, Iter),
    Iter3 = Iter2#iter{pid = From},
    retainer_fixtable(Iter3#iter.oid_tab, true),
    Iters = [Iter3 | Cp#checkpoint_args.iterators],
    reply(From, Name, {ok, Iter3, self()}),
    Cp#checkpoint_args{iterators = Iters}.

do_stop(Cp) ->
    Name = Cp#checkpoint_args.name,
    del(pending_checkpoints, Cp#checkpoint_args.pending_tab),
    del(pending_checkpoint_pids, self()),
    del(checkpoints, Name),
    unset({checkpoint, Name}),
    lists:foreach(fun deactivate_tab/1, Cp#checkpoint_args.retainers),
    Iters = Cp#checkpoint_args.iterators,
    [?SAFE(retainer_fixtable(Tab, false)) || #iter{main_tab=Tab} <- Iters],
    ok.

deactivate_tab(R) ->
    Name = R#retainer.cp_name,
    Tab = R#retainer.tab_name,
    try
	Active = lists:member(node(), R#retainer.writers),
	case R#retainer.store of
	    undefined ->
		ignore;
	    Store when Active == true ->
		retainer_delete(Store);
	    _ ->
		ignore
	end,
	unset({Tab, {retainer, Name}}),
	del({Tab, checkpoints}, Name),   %% Keep checkpoint info for table_info & mnesia_session 
	del_chkp_info(Tab, Name)
    catch _:_ -> ignore
    end.

del_chkp_info(Tab, Name) ->   
    case val({Tab, commit_work}) of
	[{checkpoints, ChkList} | Rest] -> 
	    case lists:delete(Name, ChkList) of
		[] -> 
		    %% The only checkpoint was deleted
		    mnesia_lib:set({Tab, commit_work}, Rest);
		NewList ->
		    mnesia_lib:set({Tab, commit_work}, 
				   [{checkpoints, NewList} | Rest])
	    end;
	_  -> ignore
    end.

do_del_retainers(Cp, Node) ->
    Rs = [do_del_retainer2(Cp, R, Node) || R <- Cp#checkpoint_args.retainers],
    Cp#checkpoint_args{retainers = Rs, nodes = writers(Rs)}.

do_del_retainer2(Cp, R, Node) ->
    Writers = R#retainer.writers -- [Node],
    R2 = R#retainer{writers = Writers},
    set({R2#retainer.tab_name, {retainer, R2#retainer.cp_name}}, R2),
    if
	Writers == [] ->
	    Event = {mnesia_checkpoint_deactivated, Cp#checkpoint_args.name},
	    mnesia_lib:report_system_event(Event),
	    do_stop(Cp),
	    exit(shutdown);
	Node == node() ->
	    deactivate_tab(R), % Avoids unnecessary tm_retain accesses
	    set({R2#retainer.tab_name, {retainer, R2#retainer.cp_name}}, R2),
	    R2; 
	true ->
	    R2
    end.

do_del_retainer(Cp, R0, Node) ->
    {R, Rest} = find_retainer(R0, Cp#checkpoint_args.retainers, []),
    R2 = do_del_retainer2(Cp, R, Node),
    Rs = [R2|Rest],
    Cp#checkpoint_args{retainers = Rs, nodes = writers(Rs)}.

do_del_copy(Cp, Tab, ThisNode) when ThisNode == node() ->
    Name = Cp#checkpoint_args.name,
    Others = Cp#checkpoint_args.nodes -- [ThisNode],
    R = val({Tab, {retainer, Name}}),
    abcast(Others, Name, {del_retainer, R, ThisNode}),
    do_del_retainer(Cp, R, ThisNode).

do_add_copy(Cp, Tab, Node) when Node /= node()->
    case lists:member(Tab, Cp#checkpoint_args.max) of
	false ->
	    {ok, Cp};
	true ->
	    Name = Cp#checkpoint_args.name,
	    R0 = val({Tab, {retainer, Name}}),
	    W = R0#retainer.writers,
	    R = R0#retainer{writers = W ++ [Node]},

	    case lists:member(Node, Cp#checkpoint_args.nodes) of
		true ->
		    send_retainer(Cp, R, Node);
		false ->
		    case tm_remote_prepare(Node, Cp) of
			{ok, Name, _IgnoreNew, Node} ->
			    case lists:member(schema, Cp#checkpoint_args.max) of
				true ->
				    %% We need to send schema retainer somewhere
				    RS0 = val({schema, {retainer, Name}}),
				    WS = RS0#retainer.writers,
				    RS1 = RS0#retainer{writers = WS ++ [Node]},
				    {ok, Cp1} = send_retainer(Cp, RS1, Node),
				    send_retainer(Cp1, R, Node);
				false ->
				    send_retainer(Cp, R, Node)
			    end;
			{badrpc, Reason} ->
			    {{error, {badrpc, Reason}}, Cp};
			{error, Reason} ->
			    {{error, Reason}, Cp}
		    end
	    end
    end.

tm_remote_prepare(Node, Cp) ->
    rpc:call(Node, ?MODULE, tm_prepare, [Cp]).
  
do_add_retainer(Cp, R0, Node) ->
    Writers = R0#retainer.writers,
    {R, Rest} = find_retainer(R0, Cp#checkpoint_args.retainers, []),    
    NewRet = 
	if 
	    Node == node() ->
		prepare_tab(Cp, R#retainer{writers = Writers});
	    true -> 
		R#retainer{writers = Writers}
	end,
    Rs = [NewRet | Rest],
    set({NewRet#retainer.tab_name, {retainer, NewRet#retainer.cp_name}}, NewRet),
    Cp#checkpoint_args{retainers = Rs, nodes = writers(Rs)}.

find_retainer(#retainer{cp_name = CP, tab_name = Tab}, 
	      [Ret = #retainer{cp_name = CP, tab_name = Tab} | R], Acc) ->
    {Ret, R ++ Acc};
find_retainer(Ret, [H|R], Acc) ->
    find_retainer(Ret, R, [H|Acc]).

send_retainer(Cp, R, Node) ->
    Name = Cp#checkpoint_args.name,
    Nodes0 = Cp#checkpoint_args.nodes -- [Node],
    Nodes = Nodes0 -- [node()],
    Msg = {add_retainer, R, Node},
    abcast(Nodes, Name, Msg),
    {ok, _} = rpc:call(Node, ?MODULE, cast, [Name, Msg]),
    Store = R#retainer.store,
    send_retainer2(Node, Name, Store, retainer_first(Store)),
    Cp2 = do_add_retainer(Cp, R, Node),
    {ok, Cp2}.

send_retainer2(_, _, _, '$end_of_table') ->
    ok;
%%send_retainer2(Node, Name, Store, {Slot, Records}) ->
send_retainer2(Node, Name, Store, Key) ->
    [{Tab, _, Records}] = retainer_get(Store, Key),
    abcast([Node], Name, {retain, {dirty, send_retainer}, Tab, Key, Records}),
    send_retainer2(Node, Name, Store, retainer_next(Store, Key)).

do_change_copy(Cp, Tab, FromType, ToType) ->
    Name = Cp#checkpoint_args.name,
    R = val({Tab, {retainer, Name}}),
    R2 = prepare_tab(Cp, R, ToType),
    {_, Old} = R#retainer.store,
    {_, New} = R2#retainer.store,

    Fname = tab2retainer({Tab, Name}),
    if
	FromType == disc_only_copies ->
	    mnesia_lib:dets_sync_close(Old),
	    loaded = mnesia_lib:dets_to_ets(Old, New, Fname, set, no, yes),
	    ok = file:delete(Fname);
	ToType == disc_only_copies ->
	    TabSize = ?ets_info(Old, size),
	    Props = [{file, Fname},
		     {type, set},
		     {keypos, 2},
%%	             {ram_file, true},
		     {estimated_no_objects, TabSize + 256},
		     {repair, false}],
	    {ok, _} = mnesia_lib:dets_sync_open(New, Props),
	    ok = mnesia_dumper:raw_dump_table(New, Old),
	    ?ets_delete_table(Old);
	true ->
	    ignore
    end,
    Pos = #retainer.tab_name,
    Rs = lists:keyreplace(Tab, Pos, Cp#checkpoint_args.retainers, R2),
    Cp#checkpoint_args{retainers = Rs, nodes = writers(Rs)}.

check_iter(From, Iter) when Iter#iter.pid == From ->
    ?SAFE(retainer_fixtable(Iter#iter.oid_tab, false)),
    false;
check_iter(_From, _Iter) ->
    true.

init_tabs(R, Iter) ->
    {Kind, _} = Store = R#retainer.store,
    Main = {Kind, Iter#iter.tab_name},
    Ret = Store,
    Iter2 = Iter#iter{main_tab = Main, retainer_tab = Ret},
    case Iter#iter.source of
	table -> Iter2#iter{oid_tab = Main};
	retainer -> Iter2#iter{oid_tab = Ret}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Iteration
%%
%% Iterates over a table and applies Fun(ListOfRecords)
%% with a suitable amount of records, e.g. 1000 or so.
%% ListOfRecords is [] when the iteration is over.
%%
%% OidKind affects which internal table to be iterated over and
%% ValKind affects which table to pick the actual records from. Legal
%% values for OidKind and ValKind is the atom table or the atom
%% retainer.
%%
%% The iteration may either be performed over the main table (which
%% contains the latest values of the records, i.e. the values that
%% are visible to the applications) or over the checkpoint retainer
%% (which contains the values as the looked like the timepoint when
%% the checkpoint was activated).
%%
%% It is possible to iterate over the main table and pick values
%% from the retainer and vice versa.

iterate(Name, Tab, Fun, Acc, Source, Val) ->
    Iter0 = #iter{tab_name = Tab, source = Source, val = Val},
    case call(Name, {iter_begin, Iter0}) of
	{error, Reason} ->
	    {error, Reason};
	{ok, Iter, Pid} ->
	    link(Pid), % We don't want any pending fixtable's
	    Res = ?CATCH(iter(Fun, Acc, Iter)),
	    unlink(Pid),
	    call(Name, {iter_end, Iter}),
	    case Res of
		{'EXIT', Reason} -> {error, Reason};
		{error, Reason} -> {error, Reason};
		Acc2 -> {ok, Acc2}
	    end
    end.

iter(Fun, Acc, Iter)->
    iter(Fun, Acc, Iter, retainer_first(Iter#iter.oid_tab)).

iter(Fun, Acc, Iter, Key) ->
    case get_records(Iter, Key) of
	{'$end_of_table', []} ->
	    Fun([], Acc);
	{'$end_of_table', Records} ->
	    Acc2 = Fun(Records, Acc),
	    Fun([], Acc2);
	{Next, Records} ->
	    Acc2 = Fun(Records, Acc),
	    iter(Fun, Acc2, Iter, Next)
    end.

stop_iteration(Reason) ->
    throw({error, {stopped, Reason}}).

get_records(Iter, Key) ->
    get_records(Iter, Key, 500, []). % 500 keys

get_records(_Iter, Key, 0, Acc) ->
    {Key, lists:append(lists:reverse(Acc))};
get_records(_Iter, '$end_of_table', _I, Acc) ->
    {'$end_of_table', lists:append(lists:reverse(Acc))};
get_records(Iter, Key, I, Acc) ->
    Recs = get_val(Iter, Key),
    Next = retainer_next(Iter#iter.oid_tab, Key),
    get_records(Iter, Next, I-1, [Recs | Acc]).

get_val(Iter, Key) when Iter#iter.val == latest ->
    get_latest_val(Iter, Key);
get_val(Iter, Key) when Iter#iter.val == checkpoint ->
    get_checkpoint_val(Iter, Key).

get_latest_val(Iter, Key) when Iter#iter.source == table ->
    retainer_get(Iter#iter.main_tab, Key);
get_latest_val(Iter, Key) when Iter#iter.source == retainer ->
    DeleteOid = {Iter#iter.tab_name, Key},
    [DeleteOid | retainer_get(Iter#iter.main_tab, Key)].

get_checkpoint_val(Iter, Key) when Iter#iter.source == table ->
    retainer_get(Iter#iter.main_tab, Key);
get_checkpoint_val(Iter, Key) when Iter#iter.source == retainer ->
    DeleteOid = {Iter#iter.tab_name, Key},
    case retainer_get(Iter#iter.retainer_tab, Key) of
	[{_, _, []}] -> [DeleteOid];
	[{_, _, Records}] -> [DeleteOid | Records]
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% System upgrade

system_continue(_Parent, _Debug, Cp) ->
    retainer_loop(Cp).

system_terminate(_Reason, _Parent,_Debug, Cp) ->
    do_stop(Cp).

system_code_change(Cp, _Module, _OldVsn, _Extra) ->
    {ok, Cp}.

%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Local function in order to avoid external function call
val(Var) ->
    case ?catch_val_and_stack(Var) of
	{'EXIT', Stacktrace} -> mnesia_lib:other_val(Var, Stacktrace);
	Value -> Value
    end.
