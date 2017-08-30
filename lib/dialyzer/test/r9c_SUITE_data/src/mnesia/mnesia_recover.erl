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
%%     $Id: mnesia_recover.erl,v 1.1 2008/12/17 09:53:39 mikpe Exp $
-module(mnesia_recover).

-behaviour(gen_server).

-export([
	 allow_garb/0,
	 call/1,
	 connect_nodes/1,
	 disconnect/1,
	 dump_decision_tab/0,
	 get_master_node_info/0,
	 get_master_node_tables/0,
	 get_master_nodes/1,
	 get_mnesia_downs/0,
	 has_mnesia_down/1,
	 incr_trans_tid_serial/0,
	 init/0,
	 log_decision/1,
	 log_master_nodes/3,
	 log_mnesia_down/1,
	 log_mnesia_up/1,
	 mnesia_down/1,
	 note_decision/2,
	 note_log_decision/2,
	 outcome/2,
	 start/0,
	 start_garb/0,
	 still_pending/1,
	 sync_trans_tid_serial/1,
	 wait_for_decision/2,
	 what_happened/3
	]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).


-include("mnesia.hrl").
-import(mnesia_lib, [set/2, verbose/2, error/2, fatal/2]).

-record(state, {supervisor,
		unclear_pid,
		unclear_decision,
		unclear_waitfor,
		tm_queue_len = 0,
		initiated = false,
		early_msgs = []
	       }).

%%-define(DBG(F, A), mnesia:report_event(list_to_atom(lists:flatten(io_lib:format(F, A))))).
%%-define(DBG(F, A), io:format("DBG: " ++ F, A)).

-record(transient_decision, {tid, outcome}).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [self()],
			  [{timeout, infinity}
			   %%, {debug, [trace]}
			  ]).

init() ->
    call(init).

start_garb() ->
    Pid = whereis(mnesia_recover),
    {ok, _} = timer:send_interval(timer:minutes(2), Pid, garb_decisions),
    {ok, _} = timer:send_interval(timer:seconds(10), Pid, check_overload).

allow_garb() ->
    cast(allow_garb).


%% The transaction log has either been swiched (latest -> previous) or
%% there is nothing to be dumped. This means that the previous
%% transaction log only may contain commit records which refers to
%% transactions noted in the last two of the 'Prev' tables. All other
%% tables may now be garbed by 'garb_decisions' (after 2 minutes).
%% Max 10 tables are kept.
do_allow_garb() ->
    %% The order of the following stuff is important!
    Curr = val(latest_transient_decision),
    Old = val(previous_transient_decisions),
    Next = create_transient_decision(),
    {Prev, ReallyOld} = sublist([Curr | Old], 10, []),
    [?ets_delete_table(Tab) || Tab <- ReallyOld],
    set(previous_transient_decisions, Prev),
    set(latest_transient_decision, Next).

sublist([H|R], N, Acc) when N > 0 ->
    sublist(R, N-1, [H| Acc]);
sublist(List, _N, Acc) ->
    {lists:reverse(Acc), List}.

do_garb_decisions() ->
    case val(previous_transient_decisions) of
	[First, Second | Rest] ->
	    set(previous_transient_decisions, [First, Second]),
	    [?ets_delete_table(Tab) || Tab <- Rest];
	_ ->
	    ignore
    end.

connect_nodes([]) ->
    [];
connect_nodes(Ns) ->
    %% Determine which nodes we should try to connect
    AlreadyConnected = val(recover_nodes),
    {_, Nodes} = mnesia_lib:search_delete(node(), Ns),
    Check = Nodes -- AlreadyConnected,
    GoodNodes = mnesia_monitor:negotiate_protocol(Check),
    if
	GoodNodes == [] ->
	    %% No good noodes to connect to
	    ignore;
	true ->
	    %% Now we have agreed upon a protocol with some new nodes
	    %% and we may use them when we recover transactions
	    mnesia_lib:add_list(recover_nodes, GoodNodes),
	    cast({announce_all, GoodNodes}),
	    case get_master_nodes(schema) of
		[] ->
		    Context = starting_partitioned_network,
		    mnesia_monitor:detect_inconcistency(GoodNodes, Context);
		_ -> %% If master_nodes is set ignore old inconsistencies
		    ignore
	    end
    end,
    {GoodNodes, AlreadyConnected}.

disconnect(Node) ->
    mnesia_monitor:disconnect(Node),
    mnesia_lib:del(recover_nodes, Node).

val(Var) ->
    case ?catch_val(Var) of
	{'EXIT', Reason} -> mnesia_lib:other_val(Var, Reason);
	Value -> Value
    end.

call(Msg) ->
    Pid = whereis(?MODULE),
    case Pid of
	undefined ->
	    {error, {node_not_running, node()}};
	Pid ->
	    link(Pid),
	    Res = gen_server:call(Pid, Msg, infinity),
	    unlink(Pid),

            %% We get an exit signal if server dies
            receive
                {'EXIT', Pid, _Reason} ->
                    {error, {node_not_running, node()}}
            after 0 ->
                    ignore
            end,
	    Res
    end.

multicall(Nodes, Msg) ->
    rpc:multicall(Nodes, ?MODULE, call, [Msg]).

cast(Msg) ->
    case whereis(?MODULE) of
	undefined -> ignore;
	Pid ->  gen_server:cast(Pid, Msg)
    end.

abcast(Nodes, Msg) ->
    gen_server:abcast(Nodes, ?MODULE, Msg).

note_decision(Tid, Outcome) ->
    Tab = val(latest_transient_decision),
    ?ets_insert(Tab, #transient_decision{tid = Tid, outcome = Outcome}).

note_up(Node, _Date, _Time) ->
    ?ets_delete(mnesia_decision, Node).

note_down(Node, Date, Time) ->
    ?ets_insert(mnesia_decision, {mnesia_down, Node, Date, Time}).

note_master_nodes(Tab, []) ->
    ?ets_delete(mnesia_decision, Tab);
note_master_nodes(Tab, Nodes) when list(Nodes) ->
    Master = {master_nodes, Tab, Nodes},
    ?ets_insert(mnesia_decision, Master).

note_outcome(D) when D#decision.disc_nodes == [] ->
%%    ?DBG("~w: note_tmp_decision: ~w~n", [node(), D]),
    note_decision(D#decision.tid, filter_outcome(D#decision.outcome)),
    ?ets_delete(mnesia_decision, D#decision.tid);
note_outcome(D) when D#decision.disc_nodes /= [] ->
%%    ?DBG("~w: note_decision: ~w~n", [node(), D]),
    ?ets_insert(mnesia_decision, D).

log_decision(D) when D#decision.outcome /= unclear ->
    OldD = decision(D#decision.tid),
    MergedD = merge_decisions(node(), OldD, D),
    do_log_decision(MergedD, true);
log_decision(D) ->
    do_log_decision(D, false).

do_log_decision(D, DoTell) ->
    RamNs = D#decision.ram_nodes,
    DiscNs = D#decision.disc_nodes -- [node()],
    Outcome = D#decision.outcome,
    D2 =
	case Outcome of
	    aborted -> D#decision{disc_nodes = DiscNs};
	    committed -> D#decision{disc_nodes = DiscNs};
	    _ -> D
	end,
    note_outcome(D2),
    case mnesia_monitor:use_dir() of
	true ->
	    mnesia_log:append(latest_log, D2),
	    if
		DoTell == true, Outcome /= unclear ->
		    tell_im_certain(DiscNs, D2),
		    tell_im_certain(RamNs, D2);
		true ->
		    ignore
	    end;
	false ->
	    ignore
    end.

tell_im_certain([], _D) ->
    ignore;
tell_im_certain(Nodes, D) ->
    Msg = {im_certain, node(), D},
%%    ?DBG("~w: ~w: tell: ~w~n", [node(), Msg, Nodes]),
    abcast(Nodes, Msg).

log_mnesia_up(Node) ->
    call({log_mnesia_up, Node}).

log_mnesia_down(Node) ->
    call({log_mnesia_down, Node}).

get_mnesia_downs() ->
    Tab = mnesia_decision,
    Pat = {mnesia_down, '_', '_', '_'},
    Downs = ?ets_match_object(Tab, Pat),
    [Node || {mnesia_down, Node, _Date, _Time} <- Downs].

%% Check if we have got a mnesia_down from Node
has_mnesia_down(Node) ->
    case ?ets_lookup(mnesia_decision, Node) of
	[{mnesia_down, Node, _Date, _Time}] ->
	    true;
	[] ->
	    false
    end.

mnesia_down(Node) ->
    case ?catch_val(recover_nodes) of
	{'EXIT', _} ->
	    %% Not started yet
	    ignore;
	_ ->
	    mnesia_lib:del(recover_nodes, Node),
	    cast({mnesia_down, Node})
    end.

log_master_nodes(Args, UseDir, IsRunning) ->
    if
	IsRunning == yes ->
	    log_master_nodes2(Args, UseDir, IsRunning, ok);
	UseDir == false ->
	    ok;
	true ->
	    Name = latest_log,
	    Fname = mnesia_log:latest_log_file(),
	    Exists = mnesia_lib:exists(Fname),
	    Repair = mnesia:system_info(auto_repair),
	    OpenArgs = [{file, Fname}, {name, Name}, {repair, Repair}],
	    case disk_log:open(OpenArgs) of
		{ok, Name} ->
		    log_master_nodes2(Args, UseDir, IsRunning, ok);
		{repaired, Name, {recovered,  _R}, {badbytes, _B}}
		  when Exists == true ->
		    log_master_nodes2(Args, UseDir, IsRunning, ok);
		{repaired, Name, {recovered,  _R}, {badbytes, _B}}
		  when Exists == false ->
		    mnesia_log:write_trans_log_header(),
		    log_master_nodes2(Args, UseDir, IsRunning, ok);
		{error, Reason} ->
		    {error, Reason}
	    end
    end.

log_master_nodes2([{Tab, Nodes} | Tail], UseDir, IsRunning, WorstRes) ->
    Res =
	case IsRunning of
	    yes ->
		R = call({log_master_nodes, Tab, Nodes, UseDir, IsRunning}),
		mnesia_controller:master_nodes_updated(Tab, Nodes),
		R;
	    _ ->
		do_log_master_nodes(Tab, Nodes, UseDir, IsRunning)
	end,
    case Res of
	ok ->
	    log_master_nodes2(Tail, UseDir, IsRunning, WorstRes);
	{error, Reason} ->
	    log_master_nodes2(Tail, UseDir, IsRunning, {error, Reason})
    end;
log_master_nodes2([], _UseDir, IsRunning, WorstRes) ->
    case IsRunning of
	yes ->
	    WorstRes;
	_ ->
	    disk_log:close(latest_log),
	    WorstRes
    end.

get_master_node_info() ->
    Tab = mnesia_decision,
    Pat = {master_nodes, '_', '_'},
    case catch mnesia_lib:db_match_object(ram_copies,Tab, Pat) of
	{'EXIT', _} ->
	    [];
	Masters ->
	    Masters
    end.

get_master_node_tables() ->
    Masters = get_master_node_info(),
    [Tab || {master_nodes, Tab, _Nodes} <- Masters].

get_master_nodes(Tab) ->
    case catch ?ets_lookup_element(mnesia_decision, Tab, 3) of
	{'EXIT', _} -> [];
	Nodes  -> Nodes
    end.

%% Determine what has happened to the transaction
what_happened(Tid, Protocol, Nodes) ->
    Default =
	case Protocol of
	    asym_trans -> aborted;
	    _ -> unclear  %% sym_trans and sync_sym_trans
	end,
    This = node(),
    case lists:member(This, Nodes) of
	true ->
	    {ok, Outcome} = call({what_happened, Default, Tid}),
	    Others = Nodes -- [This],
	    case filter_outcome(Outcome) of
		unclear -> what_happened_remotely(Tid, Default, Others);
		aborted -> aborted;
		committed -> committed
	    end;
	false ->
	    what_happened_remotely(Tid, Default, Nodes)
    end.

what_happened_remotely(Tid, Default, Nodes) ->
    {Replies, _} = multicall(Nodes, {what_happened, Default, Tid}),
    check_what_happened(Replies, 0, 0).

check_what_happened([H | T], Aborts, Commits) ->
    case H of
	{ok, R} ->
	    case filter_outcome(R) of
		committed ->
		    check_what_happened(T, Aborts, Commits + 1);
		aborted ->
		    check_what_happened(T, Aborts + 1, Commits);
		unclear ->
		    check_what_happened(T, Aborts, Commits)
	    end;
	{error, _} ->
	    check_what_happened(T, Aborts, Commits);
	{badrpc, _} ->
	    check_what_happened(T, Aborts, Commits)
    end;
check_what_happened([], Aborts, Commits) ->
    if
	Aborts == 0, Commits == 0 -> aborted;  % None of the active nodes knows
	Aborts > 0 -> aborted;                 % Someody has aborted
	Aborts == 0, Commits > 0 -> committed  % All has committed
    end.

%% Determine what has happened to the transaction
%% and possibly wait forever for the decision.
wait_for_decision(presume_commit, _InitBy) ->
    %% sym_trans
    {{presume_commit, self()}, committed};

wait_for_decision(D, InitBy) when D#decision.outcome == presume_abort ->
    %% asym_trans
    Tid = D#decision.tid,
    Outcome = filter_outcome(outcome(Tid, D#decision.outcome)),
    if
	Outcome /= unclear ->
	    {Tid, Outcome};

	InitBy /= startup ->
	    %% Wait a while for active transactions
	    %% to end and try again
	    timer:sleep(200),
	    wait_for_decision(D, InitBy);

	InitBy == startup ->
	    {ok, Res} = call({wait_for_decision, D}),
	    {Tid, Res}
    end.

still_pending([Tid | Pending]) ->
    case filter_outcome(outcome(Tid, unclear)) of
	unclear -> [Tid | still_pending(Pending)];
	_ -> still_pending(Pending)
    end;
still_pending([]) ->
    [].

load_decision_tab() ->
    Cont = mnesia_log:open_decision_tab(),
    load_decision_tab(Cont, load_decision_tab),
    mnesia_log:close_decision_tab().

load_decision_tab(eof, _InitBy) ->
    ok;
load_decision_tab(Cont, InitBy) ->
    case mnesia_log:chunk_decision_tab(Cont) of
	{Cont2, Decisions} ->
	    note_log_decisions(Decisions, InitBy),
	    load_decision_tab(Cont2, InitBy);
	eof ->
	    ok
    end.

%% Dumps DECISION.LOG and PDECISION.LOG and removes them.
%% From now on all decisions are logged in the transaction log file
convert_old() ->
    HasOldStuff =
	mnesia_lib:exists(mnesia_log:previous_decision_log_file()) or
	mnesia_lib:exists(mnesia_log:decision_log_file()),
    case HasOldStuff of
	true ->
	    mnesia_log:open_decision_log(),
	    dump_decision_log(startup),
	    dump_decision_log(startup),
	    mnesia_log:close_decision_log(),
	    Latest = mnesia_log:decision_log_file(),
	    ok = file:delete(Latest);
	false ->
	    ignore
    end.

dump_decision_log(InitBy) ->
    %% Assumed to be run in transaction log dumper process
    Cont = mnesia_log:prepare_decision_log_dump(),
    perform_dump_decision_log(Cont, InitBy).

perform_dump_decision_log(eof, _InitBy) ->
    confirm_decision_log_dump();
perform_dump_decision_log(Cont, InitBy) when InitBy == startup ->
    case mnesia_log:chunk_decision_log(Cont) of
	{Cont2, Decisions} ->
	    note_log_decisions(Decisions, InitBy),
	    perform_dump_decision_log(Cont2, InitBy);
	eof ->
	    confirm_decision_log_dump()
    end;
perform_dump_decision_log(_Cont, _InitBy) ->
    confirm_decision_log_dump().

confirm_decision_log_dump() ->
    dump_decision_tab(),
    mnesia_log:confirm_decision_log_dump().

dump_decision_tab() ->
    Tab = mnesia_decision,
    All = mnesia_lib:db_match_object(ram_copies,Tab, '_'),
    mnesia_log:save_decision_tab({decision_list, All}).

note_log_decisions([What | Tail], InitBy) ->
    note_log_decision(What, InitBy),
    note_log_decisions(Tail, InitBy);
note_log_decisions([], _InitBy) ->
    ok.

note_log_decision(NewD, InitBy) when NewD#decision.outcome == pre_commit ->
    note_log_decision(NewD#decision{outcome = unclear}, InitBy);

note_log_decision(NewD, _InitBy) when record(NewD, decision) ->
    Tid = NewD#decision.tid,
    sync_trans_tid_serial(Tid),
    OldD = decision(Tid),
    MergedD = merge_decisions(node(), OldD, NewD),
    note_outcome(MergedD);

note_log_decision({trans_tid, serial, _Serial}, startup) ->
    ignore;

note_log_decision({trans_tid, serial, Serial}, _InitBy) ->
    sync_trans_tid_serial(Serial);

note_log_decision({mnesia_up, Node, Date, Time}, _InitBy) ->
    note_up(Node, Date, Time);

note_log_decision({mnesia_down, Node, Date, Time}, _InitBy) ->
    note_down(Node, Date, Time);

note_log_decision({master_nodes, Tab, Nodes}, _InitBy) ->
    note_master_nodes(Tab, Nodes);

note_log_decision(H, _InitBy) when H#log_header.log_kind == decision_log ->
    V = mnesia_log:decision_log_version(),
    if
	H#log_header.log_version == V->
	    ok;
	H#log_header.log_version == "2.0" ->
	    verbose("Accepting an old version format of decision log: ~p~n",
		    [V]),
	    ok;
	true ->
	    fatal("Bad version of decision log: ~p~n", [H])
    end;

note_log_decision(H, _InitBy) when H#log_header.log_kind == decision_tab ->
    V = mnesia_log:decision_tab_version(),
    if
	V == H#log_header.log_version ->
	    ok;
	true ->
	    fatal("Bad version of decision tab: ~p~n", [H])
    end;
note_log_decision({decision_list, ItemList}, InitBy) ->
    note_log_decisions(ItemList, InitBy);
note_log_decision(BadItem, InitBy) ->
    exit({"Bad decision log item", BadItem, InitBy}).

trans_tid_serial() ->
    ?ets_lookup_element(mnesia_decision, serial, 3).

set_trans_tid_serial(Val) ->
    ?ets_insert(mnesia_decision, {trans_tid, serial, Val}).

incr_trans_tid_serial() ->
    ?ets_update_counter(mnesia_decision, serial, 1).

sync_trans_tid_serial(ThatCounter) when integer(ThatCounter) ->
    ThisCounter = trans_tid_serial(),
    if
	ThatCounter > ThisCounter ->
	    set_trans_tid_serial(ThatCounter + 1);
	true ->
	    ignore
    end;
sync_trans_tid_serial(Tid) ->
    sync_trans_tid_serial(Tid#tid.counter).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callback functions from gen_server

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([Parent]) ->
    process_flag(trap_exit, true),
    mnesia_lib:verbose("~p starting: ~p~n", [?MODULE, self()]),
    set(latest_transient_decision, create_transient_decision()),
    set(previous_transient_decisions, []),
    set(recover_nodes, []),
    State = #state{supervisor = Parent},
    {ok, State}.

create_transient_decision() ->
    ?ets_new_table(mnesia_transient_decision, [{keypos, 2}, set, public]).

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%----------------------------------------------------------------------

handle_call(init, From, State) when State#state.initiated == false ->
    Args = [{keypos, 2}, set, public, named_table],
    case mnesia_monitor:use_dir() of
	true ->
	    ?ets_new_table(mnesia_decision, Args),
	    set_trans_tid_serial(0),
	    TabFile = mnesia_log:decision_tab_file(),
	    case mnesia_lib:exists(TabFile) of
		true ->
		    load_decision_tab();
		false ->
		    ignore
	    end,
	    convert_old(),
	    mnesia_dumper:opt_dump_log(scan_decisions);
	false ->
	    ?ets_new_table(mnesia_decision, Args),
	    set_trans_tid_serial(0)
    end,
    handle_early_msgs(State, From);

handle_call(Msg, From, State) when State#state.initiated == false ->
    %% Buffer early messages
    Msgs = State#state.early_msgs,
    {noreply, State#state{early_msgs = [{call, Msg, From} | Msgs]}};

handle_call({what_happened, Default, Tid}, _From, State) ->
    sync_trans_tid_serial(Tid),
    Outcome = outcome(Tid, Default),
    {reply, {ok, Outcome}, State};

handle_call({wait_for_decision, D}, From, State) ->
    Recov = val(recover_nodes),
    AliveRam = (mnesia_lib:intersect(D#decision.ram_nodes, Recov) -- [node()]),
    RemoteDisc = D#decision.disc_nodes -- [node()],
    if
	AliveRam == [], RemoteDisc == [] ->
	    %% No more else to wait for and we may safely abort
	    {reply, {ok, aborted}, State};
	true ->
	    verbose("Transaction ~p is unclear. "
		    "Wait for disc nodes: ~w ram: ~w~n",
		    [D#decision.tid, RemoteDisc, AliveRam]),
	    AliveDisc = mnesia_lib:intersect(RemoteDisc, Recov),
	    Msg = {what_decision, node(), D},
	    abcast(AliveRam, Msg),
	    abcast(AliveDisc, Msg),
	    case val(max_wait_for_decision) of
		infinity ->
		    ignore;
		MaxWait ->
		    ForceMsg =  {force_decision, D#decision.tid},
		    {ok, _} = timer:send_after(MaxWait, ForceMsg)
	    end,
	    State2 = State#state{unclear_pid = From,
				 unclear_decision = D,
				 unclear_waitfor = (RemoteDisc ++ AliveRam)},
	    {noreply, State2}
    end;

handle_call({log_mnesia_up, Node}, _From, State) ->
    do_log_mnesia_up(Node),
    {reply, ok, State};

handle_call({log_mnesia_down, Node}, _From, State) ->
    do_log_mnesia_down(Node),
    {reply, ok, State};

handle_call({log_master_nodes, Tab, Nodes, UseDir, IsRunning}, _From, State) ->
    do_log_master_nodes(Tab, Nodes, UseDir, IsRunning),
    {reply, ok, State};

handle_call(Msg, _From, State) ->
    error("~p got unexpected call: ~p~n", [?MODULE, Msg]),
    {noreply, State}.

do_log_mnesia_up(Node) ->
    Yoyo = {mnesia_up, Node, Date = date(), Time = time()},
    case mnesia_monitor:use_dir() of
	true ->
	    mnesia_log:append(latest_log, Yoyo),
	    disk_log:sync(latest_log);
	false  ->
	    ignore
    end,
    note_up(Node, Date, Time).

do_log_mnesia_down(Node) ->
    Yoyo = {mnesia_down, Node, Date = date(), Time = time()},
    case mnesia_monitor:use_dir() of
	true ->
	    mnesia_log:append(latest_log, Yoyo),
	    disk_log:sync(latest_log);
	false  ->
	    ignore
    end,
    note_down(Node, Date, Time).

do_log_master_nodes(Tab, Nodes, UseDir, IsRunning) ->
    Master = {master_nodes, Tab, Nodes},
    Res =
	case UseDir of
	    true ->
		LogRes = mnesia_log:append(latest_log, Master),
		disk_log:sync(latest_log),
		LogRes;
	    false  ->
		ok
	end,
    case IsRunning of
	yes ->
	    note_master_nodes(Tab, Nodes);
	_NotRunning ->
	    ignore
    end,
    Res.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_cast(Msg, State) when State#state.initiated == false ->
    %% Buffer early messages
    Msgs = State#state.early_msgs,
    {noreply, State#state{early_msgs = [{cast, Msg} | Msgs]}};

handle_cast({im_certain, Node, NewD}, State) ->
    OldD = decision(NewD#decision.tid),
    MergedD = merge_decisions(Node, OldD, NewD),
    do_log_decision(MergedD, false),
    {noreply, State};

handle_cast(allow_garb, State) ->
    do_allow_garb(),
    {noreply, State};

handle_cast({decisions, Node, Decisions}, State) ->
    mnesia_lib:add(recover_nodes, Node),
    State2 = add_remote_decisions(Node, Decisions, State),
    {noreply, State2};

handle_cast({what_decision, Node, OtherD}, State) ->
    Tid = OtherD#decision.tid,
    sync_trans_tid_serial(Tid),
    Decision =
	case decision(Tid) of
	    no_decision -> OtherD;
	    MyD when record(MyD, decision) -> MyD
	end,
    announce([Node], [Decision], [], true),
    {noreply, State};

handle_cast({mnesia_down, Node}, State) ->
    case State#state.unclear_decision of
	undefined ->
	    {noreply, State};
	D ->
	    case lists:member(Node, D#decision.ram_nodes) of
		false ->
		    {noreply, State};
		true ->
		    State2 = add_remote_decision(Node, D, State),
		    {noreply, State2}
		end
    end;

handle_cast({announce_all, Nodes}, State) ->
    announce_all(Nodes, tabs()),
    {noreply, State};

handle_cast(Msg, State) ->
    error("~p got unexpected cast: ~p~n", [?MODULE, Msg]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

%% No need for buffering
%% handle_info(Msg, State) when State#state.initiated == false ->
%%     %% Buffer early messages
%%     Msgs = State#state.early_msgs,
%%     {noreply, State#state{early_msgs = [{info, Msg} | Msgs]}};

handle_info(check_overload, S) ->
    %% Time to check if mnesia_tm is overloaded
    case whereis(mnesia_tm) of
	Pid when pid(Pid) ->

	    Threshold = 100,
	    Prev = S#state.tm_queue_len,
	    {message_queue_len, Len} =
		process_info(Pid, message_queue_len),
	    if
		Len > Threshold, Prev > Threshold ->
		    What = {mnesia_tm, message_queue_len, [Prev, Len]},
		    mnesia_lib:report_system_event({mnesia_overload, What}),
		    {noreply, S#state{tm_queue_len = 0}};

		Len > Threshold ->
		    {noreply, S#state{tm_queue_len = Len}};

		true ->
		    {noreply, S#state{tm_queue_len = 0}}
	    end;
	undefined ->
	    {noreply, S}
    end;

handle_info(garb_decisions, State) ->
    do_garb_decisions(),
    {noreply, State};

handle_info({force_decision, Tid}, State) ->
    %% Enforce a transaction recovery decision,
    %% if we still are waiting for the outcome

    case State#state.unclear_decision of
	U when U#decision.tid == Tid ->
	    verbose("Decided to abort transaction ~p since "
		    "max_wait_for_decision has been exceeded~n",
		    [Tid]),
	    D = U#decision{outcome = aborted},
	    State2 = add_remote_decision(node(), D, State),
	    {noreply, State2};
	_ ->
	    {noreply, State}
    end;

handle_info({'EXIT', Pid, R}, State) when Pid == State#state.supervisor ->
    mnesia_lib:dbg_out("~p was ~p~n",[?MODULE, R]),
    {stop, shutdown, State};

handle_info(Msg, State) ->
    error("~p got unexpected info: ~p~n", [?MODULE, Msg]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------

terminate(Reason, State) ->
    mnesia_monitor:terminate_proc(?MODULE, Reason, State).

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Upgrade process when its code is to be changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

handle_early_msgs(State, From) ->
    Res = do_handle_early_msgs(State#state.early_msgs,
			       State#state{early_msgs = [],
					   initiated = true}),
    gen_server:reply(From, ok),
    Res.

do_handle_early_msgs([Msg | Msgs], State) ->
    %% The messages are in reverted order
    case do_handle_early_msgs(Msgs, State) of
        {stop, Reason, Reply, State2} ->
	    {stop, Reason, Reply, State2};
        {stop, Reason, State2} ->
	    {stop, Reason, State2};
	{noreply, State2} ->
	    handle_early_msg(Msg, State2)
    end;

do_handle_early_msgs([], State) ->
    {noreply, State}.

handle_early_msg({call, Msg, From}, State) ->
    case handle_call(Msg, From, State) of
	{reply, R, S} ->
	    gen_server:reply(From, R),
	    {noreply, S};
	Other ->
	    Other
    end;
handle_early_msg({cast, Msg}, State) ->
    handle_cast(Msg, State);
handle_early_msg({info, Msg}, State) ->
    handle_info(Msg, State).

tabs() ->
    Curr = val(latest_transient_decision),    % Do not miss any trans even
    Prev = val(previous_transient_decisions), % if the tabs are switched
    [Curr, mnesia_decision | Prev].           % Ordered by hit probability

decision(Tid) ->
    decision(Tid, tabs()).

decision(Tid, [Tab | Tabs]) ->
    case catch ?ets_lookup(Tab, Tid) of
	[D] when record(D, decision) ->
	    D;
	[C] when record(C, transient_decision) ->
	    #decision{tid = C#transient_decision.tid,
		      outcome =  C#transient_decision.outcome,
		      disc_nodes = [],
		      ram_nodes = []
		     };
	[] ->
	    decision(Tid, Tabs);
	{'EXIT', _} ->
	    %% Recently switched transient decision table
	    decision(Tid, Tabs)
    end;
decision(_Tid, []) ->
    no_decision.

outcome(Tid, Default) ->
    outcome(Tid, Default, tabs()).

outcome(Tid, Default, [Tab | Tabs]) ->
    case catch ?ets_lookup_element(Tab, Tid, 3) of
	{'EXIT', _} ->
	    outcome(Tid, Default, Tabs);
	Val ->
	    Val
    end;
outcome(_Tid, Default, []) ->
    Default.

filter_outcome(Val) ->
    case Val of
	unclear -> unclear;
	aborted -> aborted;
	presume_abort -> aborted;
	committed -> committed;
	pre_commit -> unclear
    end.

filter_aborted(D) when D#decision.outcome == presume_abort ->
    D#decision{outcome = aborted};
filter_aborted(D) ->
    D.

%% Merge old decision D with new (probably remote) decision
merge_decisions(Node, D, NewD0) ->
    NewD = filter_aborted(NewD0),
    if
	D == no_decision, node() /= Node ->
	    %% We did not know anything about this txn
	    NewD#decision{disc_nodes = []};
	D == no_decision ->
	    NewD;
	record(D, decision) ->
	    DiscNs = D#decision.disc_nodes -- ([node(), Node]),
	    OldD = filter_aborted(D#decision{disc_nodes = DiscNs}),
%%	    mnesia_lib:dbg_out("merge ~w: NewD = ~w~n D = ~w~n OldD = ~w~n",
%%			       [Node, NewD, D, OldD]),
	    if
		OldD#decision.outcome == unclear,
		NewD#decision.outcome == unclear ->
		    D;

		OldD#decision.outcome == NewD#decision.outcome ->
		    %% We have come to the same decision
		    OldD;

		OldD#decision.outcome == committed,
		NewD#decision.outcome == aborted ->
		    %% Interesting! We have already committed,
		    %% but someone else has aborted. Now we
		    %% have a nice little inconcistency. The
		    %% other guy (or some one else) has
		    %% enforced a recovery decision when
		    %% max_wait_for_decision was exceeded.
		    %% We will pretend that we have obeyed
		    %% the forced recovery decision, but we
		    %% will also generate an event in case the
		    %% application wants to do something clever.
		    Msg = {inconsistent_database, bad_decision, Node},
		    mnesia_lib:report_system_event(Msg),
		    OldD#decision{outcome = aborted};

		OldD#decision.outcome == aborted ->
		    %% aborted overrrides anything
		    OldD#decision{outcome = aborted};

		NewD#decision.outcome == aborted ->
		    %% aborted overrrides anything
		    OldD#decision{outcome = aborted};

		OldD#decision.outcome == committed,
		NewD#decision.outcome == unclear ->
		    %% committed overrides unclear
		    OldD#decision{outcome = committed};

		OldD#decision.outcome == unclear,
		NewD#decision.outcome == committed ->
		    %% committed overrides unclear
		    OldD#decision{outcome = committed}
	    end
    end.

add_remote_decisions(Node, [D | Tail], State) when record(D, decision) ->
    State2 = add_remote_decision(Node, D, State),
    add_remote_decisions(Node, Tail, State2);

add_remote_decisions(Node, [C | Tail], State)
        when record(C, transient_decision) ->
    D = #decision{tid = C#transient_decision.tid,
		  outcome = C#transient_decision.outcome,
		  disc_nodes = [],
		  ram_nodes = []},
    State2 = add_remote_decision(Node, D, State),
    add_remote_decisions(Node, Tail, State2);

add_remote_decisions(Node, [{mnesia_down, _, _, _} | Tail], State) ->
    add_remote_decisions(Node, Tail, State);

add_remote_decisions(Node, [{trans_tid, serial, Serial} | Tail], State) ->
    sync_trans_tid_serial(Serial),
    case State#state.unclear_decision of
	undefined ->
	    ignored;
	D ->
	    case lists:member(Node, D#decision.ram_nodes) of
		true ->
		    ignore;
		false ->
		    abcast([Node], {what_decision, node(), D})
	    end
    end,
    add_remote_decisions(Node, Tail, State);

add_remote_decisions(_Node, [], State) ->
    State.

add_remote_decision(Node, NewD, State) ->
    Tid = NewD#decision.tid,
    OldD = decision(Tid),
    D = merge_decisions(Node, OldD, NewD),
    do_log_decision(D, false),
    Outcome = D#decision.outcome,
    if
	OldD == no_decision ->
	    ignore;
	Outcome == unclear ->
	    ignore;
	true ->
	    case lists:member(node(), NewD#decision.disc_nodes) or
		 lists:member(node(), NewD#decision.ram_nodes) of
		true ->
		    tell_im_certain([Node], D);
		false ->
		    ignore
	    end
    end,
    case State#state.unclear_decision of
	U when U#decision.tid == Tid ->
	    WaitFor = State#state.unclear_waitfor -- [Node],
	    if
		Outcome == unclear, WaitFor == [] ->
		    %% Everybody are uncertain, lets abort
		    NewOutcome = aborted,
		    CertainD = D#decision{outcome = NewOutcome,
					  disc_nodes = [],
					  ram_nodes = []},
		    tell_im_certain(D#decision.disc_nodes, CertainD),
		    tell_im_certain(D#decision.ram_nodes, CertainD),
		    do_log_decision(CertainD, false),
		    verbose("Decided to abort transaction ~p "
			    "since everybody are uncertain ~p~n",
			    [Tid, CertainD]),
		    gen_server:reply(State#state.unclear_pid, {ok, NewOutcome}),
		    State#state{unclear_pid = undefined,
				unclear_decision = undefined,
				unclear_waitfor = undefined};
		Outcome /= unclear ->
		    verbose("~p told us that transaction ~p was ~p~n",
			    [Node, Tid, Outcome]),
		    gen_server:reply(State#state.unclear_pid, {ok, Outcome}),
		    State#state{unclear_pid = undefined,
				unclear_decision = undefined,
				unclear_waitfor = undefined};
		Outcome == unclear ->
		    State#state{unclear_waitfor = WaitFor}
	    end;
	_ ->
	    State
    end.

announce_all([], _Tabs) ->
    ok;
announce_all(ToNodes, [Tab | Tabs]) ->
    case catch mnesia_lib:db_match_object(ram_copies, Tab, '_') of
	{'EXIT', _} ->
	    %% Oops, we are in the middle of a 'garb_decisions'
	    announce_all(ToNodes, Tabs);
	List ->
	    announce(ToNodes, List, [], false),
	    announce_all(ToNodes, Tabs)
    end;
announce_all(_ToNodes, []) ->
    ok.

announce(ToNodes, [Head | Tail], Acc, ForceSend) ->
    Acc2 = arrange(ToNodes, Head, Acc, ForceSend),
    announce(ToNodes, Tail, Acc2, ForceSend);

announce(_ToNodes, [], Acc, _ForceSend) ->
    send_decisions(Acc).

send_decisions([{Node, Decisions} | Tail]) ->
    abcast([Node], {decisions, node(), Decisions}),
    send_decisions(Tail);
send_decisions([]) ->
    ok.

arrange([To | ToNodes], D, Acc, ForceSend) when record(D, decision) ->
    NeedsAdd = (ForceSend or
		lists:member(To, D#decision.disc_nodes) or
		lists:member(To, D#decision.ram_nodes)),
    case NeedsAdd of
	true ->
	    Acc2 = add_decision(To, D, Acc),
	    arrange(ToNodes, D, Acc2, ForceSend);
	false ->
	    arrange(ToNodes, D, Acc, ForceSend)
    end;

arrange([To | ToNodes], C, Acc, ForceSend) when record(C, transient_decision) ->
    Acc2 = add_decision(To, C, Acc),
    arrange(ToNodes, C, Acc2, ForceSend);

arrange([_To | _ToNodes], {mnesia_down, _Node, _Date, _Time}, Acc, _ForceSend) ->
    %% The others have their own info about this
    Acc;

arrange([_To | _ToNodes], {master_nodes, _Tab, _Nodes}, Acc, _ForceSend) ->
    %% The others have their own info about this
    Acc;

arrange([To | ToNodes], {trans_tid, serial, Serial}, Acc, ForceSend) ->
    %% Do the lamport thing plus release the others
    %% from uncertainity.
    Acc2 = add_decision(To, {trans_tid, serial, Serial}, Acc),
    arrange(ToNodes, {trans_tid, serial, Serial}, Acc2, ForceSend);

arrange([], _Decision, Acc, _ForceSend) ->
    Acc.

add_decision(Node, Decision, [{Node, Decisions} | Tail]) ->
    [{Node, [Decision | Decisions]} | Tail];
add_decision(Node, Decision, [Head | Tail]) ->
    [Head | add_decision(Node, Decision, Tail)];
add_decision(Node, Decision, []) ->
    [{Node, [Decision]}].
