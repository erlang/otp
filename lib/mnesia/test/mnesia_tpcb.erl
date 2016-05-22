%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% MODULE
%%
%%   mnesia_tpcb - TPC-B benchmarking of Mnesia
%%
%% DESCRIPTION
%%
%%   The metrics used in the TPC-B benchmark are throughput as measured
%%   in transactions per second (TPS). The benchmark uses a single,
%%   simple update-intensive transaction to load the database system.
%%   The single transaction type provides a simple, repeatable
%%   unit of work, and is designed to exercise the basic components of
%%   a database system.
%%
%%   The definition of the TPC-B states lots of detailed rules and
%%   conditions that must be fullfilled, e.g. how the ACID (atomicity,
%%   consistency, isolation and durability) properties are verified,
%%   how the random numbers must be distributed, minimum sizes of
%%   the different types of records, minimum duration of the benchmark,
%%   formulas to calculate prices (dollars per tps), disclosure issues
%%   etc. Please, see http://www.tpc.org/ about the nitty gritty details.
%%
%%   The TPC-B benchmark is stated in terms of a hypothetical bank. The
%%   bank has one or more branches. Each branch has multiple tellers. The
%%   bank has many customers, each with an account. The database represents
%%   the cash position of each entity (branch, teller and account) and a
%%   history of recent transactions run by the bank. The transaction
%%   represents the work done when a customer makes a deposit or a
%%   withdrawal against his account. The transaction is performed by a
%%   teller at some branch.
%%
%%   Each process that performs TPC-B transactions is called a driver.
%%   Drivers generates teller_id, account_id and delta amount of
%%   money randomly. An account, a teller and a branch are read, their
%%   balances are adjusted and a history record is created. The driver
%%   measures the time for 3 reads, 3 writes and 1 create.
%%
%% GETTING STARTED
%%
%%   Generate tables and run with default configuration:
%%
%%     mnesia_tpcb:start().
%%
%%  A little bit more advanced;
%%
%%     spawn(mnesia_tpcb, start, [[[{n_drivers_per_node, 8}, {stop_after, infinity}]]),
%%     mnesia_tpcb:stop().
%%
%%  Really advanced;
%%
%%    mnesia_tpcb:init(([{n_branches, 8}, {replica_type, disc_only_copies}]),
%%    mnesia_tpcb:run(([{n_drivers_per_node, 8}]),
%%    mnesia_tpcb:run(([{n_drivers_per_node, 64}]).
%%    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(mnesia_tpcb).
-author('hakan@erix.ericsson.se').

-export([
	 config/2,
	 count_balance/0,
	 driver_init/2,
	 init/1,
	 reporter_init/2,
	 run/1,
	 start/0,
	 start/1,
	 start/2,
	 stop/0,
	 real_trans/5,
	 verify_tabs/0,
	 reply_gen_branch/3,
	 frag_add_delta/7,

	 conflict_test/1,
	 dist_test/1,
	 replica_test/1,
	 sticky_replica_test/1,
	 remote_test/1,
	 remote_frag2_test/1,

	 conflict_benchmark/1
	]).

-include_lib("common_test/include/ct_event.hrl").

-define(SECOND, 1000000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Account record, total size must be at least 100 bytes

-define(ACCOUNT_FILLER,
	{123456789012345678901234567890123456789012345678901234567890,
	 123456789012345678901234567890123456789012345678901234567890,
	 123456789012345678901234567890123456789012345678901234}).

-record(account,
       {
	id           = 0, % Unique account id
	branch_id    = 0, % Branch where the account is held
	balance      = 0, % Account balance
	filler       = ?ACCOUNT_FILLER  % Gap filler to ensure size >= 100 bytes
       }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Branch record, total size must be at least 100 bytes

-define(BRANCH_FILLER,
	{123456789012345678901234567890123456789012345678901234567890,
	 123456789012345678901234567890123456789012345678901234567890,
	 123456789012345678901234567890123456789012345678901234567890}).

-record(branch,
       {
	id           = 0, % Unique branch id
	balance      = 0, % Total balance of whole branch
	filler       = ?BRANCH_FILLER  % Gap filler to ensure size >= 100 bytes
       }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Teller record, total size must be at least 100 bytes

-define(TELLER_FILLER,
	{123456789012345678901234567890123456789012345678901234567890,
	 123456789012345678901234567890123456789012345678901234567890,
	 1234567890123456789012345678901234567890123456789012345678}).

-record(teller,
       {
	id           = 0, % Unique teller id
	branch_id    = 0, % Branch where the teller is located
	balance      = 0, % Teller balance
	filler       = ?TELLER_FILLER % Gap filler to ensure size >= 100 bytes
       }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% History record, total size must be at least 50 bytes

-define(HISTORY_FILLER, 1234567890).

-record(history,
	{
	 history_id  = {0, 0}, % {DriverId, DriverLocalHistoryid}
	 time_stamp  = erlang:system_time(),  % Time point during active transaction
	 branch_id   = 0,      % Branch associated with teller
	 teller_id   = 0,      % Teller invlolved in transaction
	 account_id  = 0,      % Account updated by transaction
	 amount      = 0,      % Amount (delta) specified by transaction
	 filler      = ?HISTORY_FILLER % Gap filler to ensure size >= 50 bytes
       }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(tab_config,
	{
	  db_nodes = [node()],
	  n_replicas = 1, % Ignored for non-fragmented tables
	  replica_nodes = [node()],
	  replica_type = ram_copies,
	  use_running_mnesia = false,
	  n_fragments = 0,
	  n_branches = 1,
	  n_tellers_per_branch = 10, % Must be 10
	  n_accounts_per_branch = 100000, % Must be 100000
	  branch_filler = ?BRANCH_FILLER,
	  account_filler = ?ACCOUNT_FILLER,
	  teller_filler = ?TELLER_FILLER
	 }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(run_config,
	{
	  driver_nodes = [node()],
	  n_drivers_per_node = 1,
	  use_running_mnesia = false,
	  seed,
	  stop_after = timer:minutes(15), % Minimum 15 min
	  report_interval = timer:minutes(1),
	  send_bench_report = false,
	  use_sticky_locks = false,
	  spawn_near_branch = false,
	  activity_type = transaction,
	  reuse_history_id = false
	 }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(time,
	{
	  n_trans = 0,
	  min_n = 0,
	  max_n = 0,
	  acc_time = 0,
	  max_time = 0
	 }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(driver_state,
	{
	  driver_id,
	  driver_node,
	  seed,
	  n_local_branches,
	  local_branches,
	  tab_config,
	  run_config,
	  history_id,
	  time = #time{},
	  acc_time = #time{},
	  reuse_history_id
	 }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(reporter_state,
	{
	  driver_pids,
	  starter_pid,
	  n_iters = 0,
	  prev_tps = 0,
	  curr = #time{},
	  acc = #time{},
	  init_micros,
	  prev_micros,
	  run_config
	 }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% One driver on each node, table not replicated

config(frag_test, ReplicaType) ->
    Remote = nodes(),
    Local = node(),
    Nodes = [Local | Remote], 
    [
     {n_branches, length(Nodes)},
     {n_fragments, length(Nodes)},
     {replica_nodes, Nodes},
     {db_nodes, Nodes},
     {driver_nodes, Nodes},
     {n_accounts_per_branch, 100},
     {replica_type, ReplicaType},
     {stop_after, timer:minutes(1)},
     {report_interval, timer:seconds(10)},
     {reuse_history_id, true}
    ];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% One driver on each node, table replicated to two nodes.

config(frag2_test, ReplicaType) ->
    Remote = nodes(),
    Local = node(),
    Nodes = [Local | Remote], 
    [
     {n_branches, length(Nodes)},
     {n_fragments, length(Nodes)},
     {n_replicas, 2},
     {replica_nodes,  Nodes},
     {db_nodes, Nodes},
     {driver_nodes, Nodes},
     {n_accounts_per_branch, 100},
     {replica_type, ReplicaType},
     {stop_after, timer:minutes(1)},
     {report_interval, timer:seconds(10)},
     {reuse_history_id, true}
    ];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% One driver on this node, table replicated to all nodes.

config(replica_test, ReplicaType) ->
    Remote = nodes(),
    Local = node(),
    Nodes = [Local | Remote], 
    [
     {db_nodes, Nodes},
     {driver_nodes, [Local]},
     {replica_nodes, Nodes},
     {n_accounts_per_branch, 100},
     {replica_type, ReplicaType},
     {stop_after, timer:minutes(1)},
     {report_interval, timer:seconds(10)},
     {reuse_history_id, true}
    ];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% One driver on this node, table replicated to all nodes.

config(sticky_replica_test, ReplicaType) ->
    Remote = nodes(),
    Local = node(),
    Nodes = [Local | Remote], 
    [
     {db_nodes, Nodes},
     {driver_nodes, [node()]},
     {replica_nodes, Nodes},
     {n_accounts_per_branch, 100},
     {replica_type, ReplicaType},
     {use_sticky_locks, true},
     {stop_after, timer:minutes(1)},
     {report_interval, timer:seconds(10)},
     {reuse_history_id, true}
    ];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ten drivers per node, tables replicated to all nodes, lots of branches

config(dist_test, ReplicaType) ->  
    Remote = nodes(),
    Local = node(),
    Nodes = [Local | Remote], 
    [
     {db_nodes, Nodes},
     {driver_nodes, Nodes},
     {replica_nodes, Nodes},
     {n_drivers_per_node, 10},
     {n_branches, 10 * length(Nodes) * 100},
     {n_accounts_per_branch, 10},
     {replica_type, ReplicaType},
     {stop_after, timer:minutes(1)},
     {report_interval, timer:seconds(10)},
     {reuse_history_id, true}
    ];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ten drivers per node, tables replicated to all nodes, single branch

config(conflict_test, ReplicaType) ->
    Remote = nodes(),
    Local = node(),
    Nodes = [Local | Remote], 
    [
     {db_nodes, Nodes},
     {driver_nodes, Nodes},
     {replica_nodes, Nodes},
     {n_drivers_per_node, 10},
     {n_branches, 1},
     {n_accounts_per_branch, 10},
     {replica_type, ReplicaType},
     {stop_after, timer:minutes(1)},
     {report_interval, timer:seconds(10)},
     {reuse_history_id, true}
    ];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% One driver on this node, table replicated to all other nodes.

config(remote_test, ReplicaType) ->
    Remote = nodes(),
    Local = node(),
    Nodes = [Local | Remote],
    [
     {db_nodes, Nodes},
     {driver_nodes, [Local]},
     {replica_nodes, Remote},
     {n_accounts_per_branch, 100},
     {replica_type, ReplicaType},
     {stop_after, timer:minutes(1)},
     {report_interval, timer:seconds(10)},
     {reuse_history_id, true}
    ];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% One driver on this node, table replicated to two other nodes.

config(remote_frag2_test, ReplicaType) ->
    Remote = nodes(),
    Local = node(),
    Nodes = [Local | Remote],
    [
     {n_branches, length(Remote)},
     {n_fragments, length(Remote)},
     {n_replicas, 2},
     {replica_nodes, Remote},
     {db_nodes, Nodes},
     {driver_nodes, [Local]},
     {n_accounts_per_branch, 100},
     {replica_type, ReplicaType},
     {stop_after, timer:minutes(1)},
     {report_interval, timer:seconds(10)},
     {reuse_history_id, true}
    ];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ten drivers per node, tables replicated to all nodes, single branch

config(conflict_benchmark, ReplicaType) ->
    Remote = nodes(),
    Local = node(),
    Nodes = [Local | Remote],
    [{db_nodes, Nodes},
     {driver_nodes, Nodes},
     {replica_nodes, Nodes},
     {n_drivers_per_node, 10},
     {n_branches, 1},
     {n_accounts_per_branch, 10},
     {replica_type, ReplicaType},
     {stop_after, timer:minutes(1)},
     {report_interval, timer:seconds(10)},
     {send_bench_report, true},
     {reuse_history_id, true}
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(What, ReplicaType) ->
    spawn_link(?MODULE, start, [config(What, ReplicaType)]).

replica_test(ReplicaType) ->
    start(replica_test, ReplicaType).

sticky_replica_test(ReplicaType) ->
    start(sticky_replica_test, ReplicaType).

dist_test(ReplicaType) ->
    start(dist_test, ReplicaType).

conflict_test(ReplicaType) ->
    start(conflict_test, ReplicaType).

remote_test(ReplicaType) ->
    start(remote_test, ReplicaType).

remote_frag2_test(ReplicaType) ->
    start(remote_frag2_test, ReplicaType).

conflict_benchmark(ReplicaType) ->
    start(config(conflict_benchmark, ReplicaType)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Args is a list of {Key, Val} tuples where Key is a field name
%% in either the record tab_config or run_config. Unknown keys are ignored.

start() ->
    start([]).
start(Args) ->
    init(Args),
    run(Args).

list2rec(List, Fields, DefaultTuple) ->
    [Name|Defaults] = tuple_to_list(DefaultTuple),
    List2 = list2rec(List, Fields, Defaults, []),
    list_to_tuple([Name] ++ List2).

list2rec(_List, [], [], Acc) ->
    Acc;
list2rec(List, [F|Fields], [D|Defaults], Acc) ->
    {Val, List2} =
	case lists:keysearch(F, 1, List) of
	    false ->
		{D, List};
	    {value, {F, NewVal}} ->
		{NewVal, lists:keydelete(F, 1, List)}
	end,
    list2rec(List2, Fields, Defaults, Acc ++ [Val]).

stop() ->
    case whereis(mnesia_tpcb) of
	undefined ->
	    {error, not_running};
	Pid ->
	    sync_stop(Pid)
    end.

sync_stop(Pid) ->
    Pid ! {self(), stop},
    receive
	{Pid, {stopped, Res}} ->  Res
    after timer:minutes(1) ->
	    exit(Pid, kill),
	    {error, brutal_kill}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Initialization

%% Args is a list of {Key, Val} tuples where Key is a field name
%% in the record tab_config, unknown keys are ignored.

init(Args) ->
    TabConfig0 = list2rec(Args, record_info(fields, tab_config), #tab_config{}),
    TabConfig = 
	if
	    TabConfig0#tab_config.n_fragments =:= 0 ->
		TabConfig0#tab_config{n_replicas = length(TabConfig0#tab_config.replica_nodes)};
	    true ->
		TabConfig0	
	end,
    Tags = record_info(fields, tab_config),
    Fun = fun(F, Pos) -> {{F, element(Pos, TabConfig)}, Pos + 1} end,
    {List, _} = lists:mapfoldl(Fun, 2, Tags),
    io:format("TPC-B: Table config: ~p ~n", [List]),

    DbNodes = TabConfig#tab_config.db_nodes,
    stop(),
    if
	TabConfig#tab_config.use_running_mnesia =:= true ->
	    ignore;
	true ->
	    rpc:multicall(DbNodes, mnesia, lkill, []),
	    case mnesia:delete_schema(DbNodes) of
		ok ->
		    case mnesia:create_schema(DbNodes) of
			ok ->
			    {Replies, BadNodes} =
				rpc:multicall(DbNodes, mnesia, start, []),
			    case [Res || Res <- Replies, Res =/= ok] of
				[] when BadNodes =:= [] ->
				    ok;
				BadRes ->
				    io:format("TPC-B: <ERROR> "
					      "Failed to start ~p: ~p~n",
					      [BadNodes, BadRes]),
				    exit({start_failed, BadRes, BadNodes})
			    end;
			{error, Reason} ->
			    io:format("TPC-B: <ERROR> "
				      "Failed to create schema on disc: ~p~n",
				      [Reason]),
			    exit({create_schema_failed, Reason})
		    end;
		{error, Reason} ->
		    io:format("TPC-B: <ERROR> "
			      "Failed to delete schema on disc: ~p~n",
			      [Reason]),
		    exit({delete_schema_failed, Reason})
	    end
    end,
    gen_tabs(TabConfig).

gen_tabs(TC) ->
    create_tab(TC, branch, record_info(fields, branch),
	       undefined),
    create_tab(TC, account, record_info(fields, account),
	       {branch, #account.branch_id}),
    create_tab(TC, teller, record_info(fields, teller),
	       {branch, #teller.branch_id}),
    create_tab(TC, history, record_info(fields, history),
	       {branch, #history.branch_id}),

    NB = TC#tab_config.n_branches,
    NT = TC#tab_config.n_tellers_per_branch,
    NA = TC#tab_config.n_accounts_per_branch,
    io:format("TPC-B: Generating ~p branches a ~p bytes~n",
	      [NB, size(term_to_binary(default_branch(TC)))]),
    io:format("TPC-B: Generating ~p * ~p tellers a ~p bytes~n",
	      [NB, NT, size(term_to_binary(default_teller(TC)))]),
    io:format("TPC-B: Generating ~p * ~p accounts a ~p bytes~n",
	      [NB, NA, size(term_to_binary(default_account(TC)))]),
    io:format("TPC-B: Generating 0 history records a ~p bytes~n",
	      [size(term_to_binary(default_history(TC)))]),
    gen_branches(TC),

    case verify_tabs() of
	ok ->
	    ignore;
	{error, Reason} ->
	    io:format("TPC-B: <ERROR> Inconsistent tables: ~w~n",
		      [Reason]),
	    exit({inconsistent_tables, Reason})
    end.

create_tab(TC, Name, Attrs, _ForeignKey) when TC#tab_config.n_fragments =:= 0 ->
    Nodes = TC#tab_config.replica_nodes,
    Type = TC#tab_config.replica_type,
    Def = [{Type, Nodes}, {attributes, Attrs}],
    create_tab(Name, Def);
create_tab(TC, Name, Attrs, ForeignKey) ->
    NReplicas = TC#tab_config.n_replicas,
    NodePool = TC#tab_config.replica_nodes,
    Type = TC#tab_config.replica_type,
    NF = TC#tab_config.n_fragments,
    Props = [{n_fragments, NF},
	     {node_pool, NodePool},
	     {n_copies(Type), NReplicas},
	     {foreign_key, ForeignKey}],
    Def = [{frag_properties, Props},
	   {attributes, Attrs}],
    create_tab(Name, Def).
    
create_tab(Name, Def) ->
    mnesia:delete_table(Name),
    case mnesia:create_table(Name, Def) of
	{atomic, ok} ->
	    ok;
	{aborted, Reason} ->
	    io:format("TPC-B: <ERROR> failed to create table ~w ~w: ~p~n",
		      [Name, Def, Reason]),
	    exit({create_table_failed, Reason})
    end.

n_copies(Type) ->
    case Type of
	ram_copies -> n_ram_copies;
	disc_copies -> n_disc_copies;
	disc_only_copies -> n_disc_only_copies
    end.

gen_branches(TC) ->
    First = 0,
    Last = First + TC#tab_config.n_branches - 1,
    GenPids = gen_branches(TC, First, Last, []),
    wait_for_gen(GenPids).

wait_for_gen([]) ->
    ok;
wait_for_gen(Pids) ->
    receive
	{branch_generated, Pid} -> wait_for_gen(lists:delete(Pid, Pids));
	Exit ->
	    exit({tpcb_failed, Exit})
    end.

gen_branches(TC, BranchId, Last, UsedNs) when BranchId =< Last ->
    UsedNs2 = get_branch_nodes(BranchId, UsedNs),
    Node = hd(UsedNs2),
    Pid = spawn_link(Node, ?MODULE, reply_gen_branch,
		     [self(), TC, BranchId]),
    [Pid | gen_branches(TC, BranchId + 1, Last, UsedNs2)];
gen_branches(_, _, _, _) ->
    [].

reply_gen_branch(ReplyTo, TC, BranchId) ->
    gen_branch(TC, BranchId),
    ReplyTo ! {branch_generated, self()},
    unlink(ReplyTo).

%% Returns a new list of nodes with the best node as head
get_branch_nodes(BranchId, UsedNs) ->
    WriteNs = table_info({branch, BranchId}, where_to_write),
    WeightedNs = [{n_duplicates(N, UsedNs, 0), N} || N <- WriteNs],
    [{_, LeastUsed} | _ ] = lists:sort(WeightedNs),
    [LeastUsed | UsedNs].

n_duplicates(_N, [], Count) ->
    Count;
n_duplicates(N, [N | Tail], Count) ->    
    n_duplicates(N, Tail, Count + 1);
n_duplicates(N, [_ | Tail], Count) ->
    n_duplicates(N, Tail, Count).

gen_branch(TC, BranchId) ->
    A = default_account(TC),
    NA = TC#tab_config.n_accounts_per_branch,
    FirstA = BranchId * NA,
    ArgsA = [FirstA, FirstA + NA - 1, BranchId, A],
    ok = mnesia:activity(async_dirty, fun gen_accounts/4, ArgsA, mnesia_frag),

    T = default_teller(TC),
    NT = TC#tab_config.n_tellers_per_branch,
    FirstT = BranchId * NT,
    ArgsT = [FirstT, FirstT + NT - 1, BranchId, T],
    ok = mnesia:activity(async_dirty, fun gen_tellers/4, ArgsT, mnesia_frag),

    B = default_branch(TC),
    FunB = fun() -> mnesia:write(branch, B#branch{id = BranchId}, write) end,
    ok = mnesia:activity(sync_dirty,  FunB, [], mnesia_frag).
    
gen_tellers(Id, Last, BranchId, T) when Id =< Last ->
    mnesia:write(teller, T#teller{id = Id, branch_id=BranchId}, write),
    gen_tellers(Id + 1, Last, BranchId, T);
gen_tellers(_, _, _, _) ->
    ok.
    
gen_accounts(Id, Last, BranchId, A) when Id =< Last ->
    mnesia:write(account, A#account{id = Id, branch_id=BranchId}, write),
    gen_accounts(Id + 1, Last, BranchId, A);
gen_accounts(_, _, _, _) ->
    ok.

default_branch(TC) ->  #branch{filler = TC#tab_config.branch_filler}.
default_teller(TC) ->  #teller{filler = TC#tab_config.teller_filler}.
default_account(TC) -> #account{filler = TC#tab_config.account_filler}.
default_history(_TC) -> #history{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Run the benchmark

%% Args is a list of {Key, Val} tuples where Key is a field name
%% in the record run_config, unknown keys are ignored.
run(Args) ->
    RunConfig = list2rec(Args, record_info(fields, run_config), #run_config{}),
    Tags = record_info(fields, run_config),
    Fun = fun(F, Pos) -> {{F, element(Pos, RunConfig)}, Pos + 1} end,
    {List, _} = lists:mapfoldl(Fun, 2, Tags),
    io:format("TPC-B: Run config: ~p ~n", [List]),

    Pid = spawn_link(?MODULE, reporter_init, [self(), RunConfig]),
    receive
	{Pid, {stopped, Res}} ->
	    Res; % Stopped by other process
	Else ->
	    {tpcb_got, Else}   
    after RunConfig#run_config.stop_after ->
	    sync_stop(Pid)
    end.

reporter_init(Starter, RC) ->
    register(mnesia_tpcb, self()),
    process_flag(trap_exit, true),
    DbNodes = mnesia:system_info(db_nodes),
    if
	RC#run_config.use_running_mnesia =:= true ->
	    ignore;
	true ->
	    {Replies, BadNodes} =
		rpc:multicall(DbNodes, mnesia, start, []),
	    case [Res || Res <- Replies, Res =/= ok] of
		[] when BadNodes =:= [] ->
		    ok;
		BadRes ->
		    io:format("TPC-B: <ERROR> "
			      "Failed to start ~w: ~p~n",
			      [BadNodes, BadRes]),
		    exit({start_failed, BadRes, BadNodes})
	    end,
	    verify_tabs()
    end,

    N  = table_info(branch, size),
    NT = table_info(teller, size) div N,
    NA = table_info(account, size) div N,

    {Type, NF, RepNodes} = table_storage(branch),
    TC = #tab_config{n_fragments = NF,
		     n_branches = N,
		     n_tellers_per_branch = NT,
		     n_accounts_per_branch = NA,
		     db_nodes = DbNodes,
		     replica_nodes = RepNodes,
		     replica_type = Type
		    },
    Drivers = start_drivers(RC, TC),
    Now = erlang:monotonic_time(),
    State = #reporter_state{driver_pids = Drivers,
			    run_config = RC,
			    starter_pid = Starter,
			    init_micros = Now,
			    prev_micros = Now
			   },
    case catch reporter_loop(State) of
	{'EXIT', Reason} ->
	    io:format("TPC-B: Abnormal termination: ~p~n", [Reason]),
	    if
		RC#run_config.use_running_mnesia =:= true ->
		    ignore;
		true ->
		    rpc:multicall(DbNodes, mnesia, lkill, [])
	    end,
	    unlink(Starter),
	    Starter ! {self(), {stopped, {error, Reason}}}, % To be sure
	    exit(shutdown);
	{ok, Stopper, State2} ->
	    Time = State2#reporter_state.acc,
	    Res =
		case verify_tabs() of
		    ok ->
			{ok, Time};
		    {error, Reason} ->
			io:format("TPC-B: <ERROR> Inconsistent tables, ~p~n",
				  [{error, Reason}]),
			{error, Reason}
		end,
	    if
		RC#run_config.use_running_mnesia =:= true -> 
		    ignore;
		true -> 
		    rpc:multicall(DbNodes, mnesia, stop, [])
	    end,
	    unlink(Starter),
	    Starter ! {self(), {stopped, Res}},
	    if
		Stopper =/= Starter ->
		    Stopper ! {self(), {stopped, Res}};
		true ->
		    ignore
	    end,
	    exit(shutdown)
    end.

table_info(Tab, Item) ->
    Fun = fun() -> mnesia:table_info(Tab, Item) end,
    mnesia:activity(sync_dirty, Fun, mnesia_frag).

%% Returns {Storage, NFragments, ReplicaNodes}
table_storage(Tab) ->
    case mnesia:table_info(branch, frag_properties) of
	[] ->
	    NFO = 0,
	    NR  = length(mnesia:table_info(Tab, ram_copies)),
	    ND  = length(mnesia:table_info(Tab, disc_copies)),
	    NDO = length(mnesia:table_info(Tab, disc_only_copies)),
	    if
		NR  =/= 0 -> {ram_copies, NFO, NR};
		ND  =/= 0 -> {disc_copies, NFO, ND};
		NDO =/= 0 -> {disc_copies, NFO, NDO}
	    end;
	Props ->
	    {value, NFO} = lists:keysearch(n_fragments, 1, Props),
	    NR  = table_info(Tab, n_ram_copies),
	    ND  = table_info(Tab, n_disc_copies),
	    NDO = table_info(Tab, n_disc_only_copies),
	    if
		NR  =/= 0 -> {ram_copies, NFO, NR};
		ND  =/= 0 -> {disc_copies, NFO, ND};
		NDO =/= 0 -> {disc_copies, NFO, NDO}
	    end
    end.
    
reporter_loop(State) ->
    RC = State#reporter_state.run_config,
    receive
	{From, stop} ->
	    {ok, From, call_drivers(State, stop)};
	{'EXIT', Pid, Reason} when Pid =:= State#reporter_state.starter_pid ->
	    %% call_drivers(State, stop),
	    exit({starter_died, Pid, Reason})
    after RC#run_config.report_interval ->
	    Iters = State#reporter_state.n_iters,
	    State2 = State#reporter_state{n_iters = Iters + 1},
	    case call_drivers(State2, report) of
		State3 when State3#reporter_state.driver_pids =/= [] ->
		    State4 = State3#reporter_state{curr = #time{}},
		    reporter_loop(State4);
		_ ->
		    exit(drivers_died)
	    end
    end.

call_drivers(State, Msg) ->
    Drivers = State#reporter_state.driver_pids,
    lists:foreach(fun(Pid) -> Pid ! {self(), Msg} end, Drivers),
    State2 = show_report(calc_reports(Drivers, State)),
    case Msg =:= stop of
	true ->
	    Acc = State2#reporter_state.acc,
	    Init = State2#reporter_state.init_micros,
	    show_report(State2#reporter_state{n_iters = 0,
					      curr = Acc,
					      prev_micros = Init});
	false ->
	    ignore
    end,
    State2.

calc_reports([], State) ->
    State;
calc_reports([Pid|Drivers], State) ->
    receive
	{'EXIT', P, Reason} when P =:= State#reporter_state.starter_pid ->
	    exit({starter_died, P, Reason});
	{'EXIT', Pid, Reason} ->
	    exit({driver_died, Pid, Reason});
	{Pid, Time} when is_record(Time, time) ->
	    %% io:format("~w: ~w~n", [Pid, Time]),
	    A = add_time(State#reporter_state.acc, Time),
	    C = add_time(State#reporter_state.curr, Time),
	    State2 = State#reporter_state{acc = A, curr = C},
	    calc_reports(Drivers, State2)
    end.

add_time(Acc, New) ->
    Acc#time{n_trans = New#time.n_trans + Acc#time.n_trans,
	     min_n = lists:min([New#time.n_trans, Acc#time.min_n] -- [0]),
	     max_n = lists:max([New#time.n_trans, Acc#time.max_n]),
	     acc_time = New#time.acc_time + Acc#time.acc_time,
	     max_time = lists:max([New#time.max_time, Acc#time.max_time])}.

-define(AVOID_DIV_ZERO(_What_), try (_What_) catch _:_ -> 0 end).

show_report(State) ->
    Now    = erlang:timestamp(),
    Iters  = State#reporter_state.n_iters,
    Cfg    = State#reporter_state.run_config,
    Time   = State#reporter_state.curr,
    Max    = Time#time.max_time,
    N      = Time#time.n_trans,
    Avg    = ?AVOID_DIV_ZERO(Time#time.acc_time div N),
    AliveN = length(State#reporter_state.driver_pids),
    Tps    = ?AVOID_DIV_ZERO((?SECOND * AliveN) div Avg),
    PrevTps= State#reporter_state.prev_tps,
    {DiffSign, DiffTps} = signed_diff(Iters, Tps, PrevTps),
    Unfairness = ?AVOID_DIV_ZERO(Time#time.max_n / Time#time.min_n),
    BruttoAvg = ?AVOID_DIV_ZERO((Now - State#reporter_state.prev_micros) div N),
%%    io:format("n_iters=~p, n_trans=~p, n_drivers=~p, avg=~p, now=~p, prev=~p~n",
%%	      [Iters, N, AliveN, BruttoAvg, Now, State#reporter_state.prev_micros]),
    BruttoTps = ?AVOID_DIV_ZERO(?SECOND div BruttoAvg),
    case Iters > 0 of
	true ->
	    io:format("TPC-B: ~p iter ~s~p diff ~p (~p) tps ~p avg micros ~p max micros ~p unfairness~n",
		      [Iters, DiffSign, DiffTps, Tps, BruttoTps, Avg, Max, Unfairness]);
	false ->
	    io:format("TPC-B: ~p (~p) transactions per second, "
		      "duration of longest transaction was ~p milliseconds~n",
		      [Tps, BruttoTps, Max div 1000])
    end,
    case Cfg#run_config.send_bench_report of
	true ->
	    ct_event:notify(
	      #event{name = benchmark_data,
		     data = [{suite,"mnesia_tpcb"},
			     {value,Tps}]});
	_ ->
	    ok
    end,

    State#reporter_state{prev_tps = Tps, prev_micros = Now}.

signed_diff(Iters, Curr, Prev) ->
    case Iters > 1 of
	true  -> sign(Curr - Prev);
	false -> sign(0)
    end.
    
sign(N) when N > 0 -> {"+", N};
sign(N) -> {"", N}.
    
start_drivers(RC, TC) ->
    LastHistoryId = table_info(history, size),
    Reuse = RC#run_config.reuse_history_id,
    DS = #driver_state{tab_config       = TC,
		       run_config       = RC,
		       n_local_branches = 0,
		       local_branches   = [],
		       history_id       = LastHistoryId,
		       reuse_history_id = Reuse},
    Nodes = RC#run_config.driver_nodes,
    NB = TC#tab_config.n_branches,
    First = 0,
    AllBranches = lists:seq(First, First + NB - 1),
    ND = RC#run_config.n_drivers_per_node,
    Spawn = fun(Spec) ->
		    Node = Spec#driver_state.driver_node,
		    spawn_link(Node, ?MODULE, driver_init, [Spec, AllBranches])
	    end,
    Specs = [DS#driver_state{driver_id = Id, driver_node = N}
	     || N <- Nodes, 
		Id <- lists:seq(1, ND)],
    Specs2 = lists:sort(lists:flatten(Specs)),
    {Specs3, OrphanBranches} = alloc_local_branches(AllBranches, Specs2, []),
    case length(OrphanBranches) of
	N when N =< 10 ->
	    io:format("TPC-B: Orphan branches: ~p~n", [OrphanBranches]);
	N ->
	    io:format("TPC-B: Orphan branches: ~p~n", [N])
    end,
    [Spawn(Spec) || Spec <- Specs3].

alloc_local_branches([BranchId | Tail], Specs, OrphanBranches) ->
    Nodes = table_info({branch, BranchId}, where_to_write),
    LocalSpecs = [DS || DS <- Specs,
			lists:member(DS#driver_state.driver_node, Nodes)],
    case lists:keysort(#driver_state.n_local_branches, LocalSpecs) of
	[] ->
	    alloc_local_branches(Tail, Specs, [BranchId | OrphanBranches]);
	[DS | _] ->
	    LocalNB = DS#driver_state.n_local_branches + 1,
	    LocalBranches = [BranchId | DS#driver_state.local_branches],
	    DS2 = DS#driver_state{n_local_branches = LocalNB,
				  local_branches = LocalBranches},
	    Specs2 = Specs -- [DS],
	    Specs3 = [DS2 | Specs2],
	    alloc_local_branches(Tail, Specs3, OrphanBranches)
    end;
alloc_local_branches([], Specs, OrphanBranches) ->
    {Specs, OrphanBranches}.
    
driver_init(DS, AllBranches) ->
    Seed = case (DS#driver_state.run_config)#run_config.seed of
	       undefined -> rand:seed(exsplus);
	       ExpSeed -> rand:seed(ExpSeed)
    end,

    DS2 =
	if
	    DS#driver_state.n_local_branches =:= 0 ->
		DS#driver_state{seed = Seed, 
				n_local_branches = length(AllBranches),
				local_branches   = AllBranches};
	    true ->
		DS#driver_state{seed = Seed}
	end,
    io:format("TPC-B: Driver ~p started as ~p on node ~p with ~p local branches~n",
	      [DS2#driver_state.driver_id, self(), node(), DS2#driver_state.n_local_branches]),
    driver_loop(DS2).

driver_loop(DS) ->
    receive
	{From, report} ->
	    From ! {self(), DS#driver_state.time},
	    Acc = add_time(DS#driver_state.time, DS#driver_state.acc_time),
	    DS2 = DS#driver_state{time=#time{}, acc_time = Acc}, % Reset timer
	    DS3 = calc_trans(DS2),
	    driver_loop(DS3);
	{From, stop} ->
	    Acc = add_time(DS#driver_state.time, DS#driver_state.acc_time),
	    io:format("TPC-B: Driver ~p (~p) on node ~p stopped: ~w~n",
		      [DS#driver_state.driver_id, self(), node(self()), Acc]),	
	    From ! {self(), DS#driver_state.time},
	    unlink(From),
	    exit(stopped)
    after 0 ->
	    DS2 = calc_trans(DS),
	    driver_loop(DS2)
    end.

calc_trans(DS) ->
    {Micros, DS2} = time_trans(DS),
    Time = DS2#driver_state.time,
    Time2 = Time#time{n_trans = Time#time.n_trans + 1,
		      acc_time = Time#time.acc_time + Micros,
		      max_time = lists:max([Micros, Time#time.max_time])
		     },
    case DS#driver_state.reuse_history_id of
	false ->
	    HistoryId = DS#driver_state.history_id + 1,
	    DS2#driver_state{time=Time2, history_id = HistoryId};
	true ->
	    DS2#driver_state{time=Time2}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Generate teller_id, account_id and delta
%% Time the TPC-B transaction
time_trans(DS) ->
    {Random, NewSeed} = rand:uniform_s(DS#driver_state.seed),

    TC = DS#driver_state.tab_config,
    RC = DS#driver_state.run_config,
    {Branchid, Args} = random_to_args(Random, DS),
    {Fun, Mod} = trans_type(TC, RC),
    {Time, Res} = timer:tc(?MODULE, real_trans, [RC, Branchid, Fun, Args, Mod]),

    case Res of
	AccountBal when is_integer(AccountBal) ->
	    {Time, DS#driver_state{seed = NewSeed}};
	Other ->
	    exit({crash, Other, Args, Random, DS})
    end.

random_to_args(Random, DS) ->
    DriverId = DS#driver_state.driver_id,
    TC = DS#driver_state.tab_config,
    HistoryId = DS#driver_state.history_id,
    Delta = trunc(Random * 1999998) - 999999, % -999999 <= Delta <= +999999

    Branches = DS#driver_state.local_branches,
    NB = DS#driver_state.n_local_branches,
    NT = TC#tab_config.n_tellers_per_branch,
    NA = TC#tab_config.n_accounts_per_branch,
    Tmp = trunc(Random * NB * NT),
    BranchPos = (Tmp div NT) + 1,
    BranchId =
	case TC#tab_config.n_fragments of
	    0 -> BranchPos - 1;
	    _ -> lists:nth(BranchPos, Branches)
	end,
    RelativeTellerId = Tmp div NT,
    TellerId = (BranchId * NT) + RelativeTellerId,
    {AccountBranchId, AccountId} =
	if
	    Random >= 0.85, NB > 1 ->
		%% Pick from a remote account
                TmpAccountId= trunc(Random * (NB - 1) * NA),
		TmpAccountBranchId = TmpAccountId div NA,
		if
		    TmpAccountBranchId =:= BranchId ->
			{TmpAccountBranchId + 1, TmpAccountId + NA};
		    true ->
			{TmpAccountBranchId, TmpAccountId}
		end;
	    true ->
		%% Pick from a local account
		RelativeAccountId = trunc(Random * NA),
		TmpAccountId = (BranchId * NA) + RelativeAccountId,
		{BranchId, TmpAccountId}
	end,
    
    {BranchId, [DriverId, BranchId, TellerId, AccountBranchId, AccountId, HistoryId, Delta]}.

real_trans(RC, BranchId, Fun, Args, Mod) ->
    Type = RC#run_config.activity_type,
    case RC#run_config.spawn_near_branch of
	false ->
	    mnesia:activity(Type, Fun, Args, Mod);
	true ->
	    Node = table_info({branch, BranchId}, where_to_read),
	    case rpc:call(Node, mnesia, activity, [Type, Fun, Args, Mod]) of
		{badrpc, Reason} -> exit(Reason);
		Other -> Other
	    end
    end.

trans_type(TC, RC) ->
    if
	TC#tab_config.n_fragments =:= 0,
	RC#run_config.use_sticky_locks =:= false ->
	    {fun add_delta/7, mnesia};
	TC#tab_config.n_fragments =:= 0,
	RC#run_config.use_sticky_locks =:= true ->
	    {fun sticky_add_delta/7, mnesia};
	TC#tab_config.n_fragments > 0,
	RC#run_config.use_sticky_locks =:= false ->
	    {fun frag_add_delta/7, mnesia_frag}
    end.

%%
%% Runs the TPC-B defined transaction and returns NewAccountBalance
%%

add_delta(DriverId, BranchId, TellerId, _AccountBranchId, AccountId, HistoryId, Delta) ->
    %% Grab write lock already when the record is read 
    
    %% Add delta to branch balance
    [B]  = mnesia:read(branch, BranchId, write),
    NewB = B#branch{balance = B#branch.balance + Delta},
    ok = mnesia:write(branch, NewB, write),

    %% Add delta to teller balance
    [T]  = mnesia:read(teller, TellerId, write),
    NewT = T#teller{balance = T#teller.balance + Delta},
    ok = mnesia:write(teller, NewT, write),

    %% Add delta to account balance
    [A]  = mnesia:read(account, AccountId, write),
    NewA = A#account{balance = A#account.balance + Delta},
    ok = mnesia:write(account, NewA, write),

    %% Append to history log
    History = #history{history_id   = {DriverId, HistoryId},
		       account_id   = AccountId,
		       teller_id    = TellerId,
		       branch_id    = BranchId,
		       amount       = Delta
		      },
    ok = mnesia:write(history, History, write),

    %% Return account balance
    NewA#account.balance.

sticky_add_delta(DriverId, BranchId, TellerId, _AccountBranchId, AccountId, HistoryId, Delta) ->
    %% Grab orinary read lock when the record is read
    %% Grab sticky write lock when the record is written
    %% This transaction would benefit of an early  stick_write lock at read

    %% Add delta to branch balance
    [B]  = mnesia:read(branch, BranchId, read),
    NewB = B#branch{balance = B#branch.balance + Delta},
    ok = mnesia:write(branch, NewB, sticky_write),

    %% Add delta to teller balance
    [T]  = mnesia:read(teller, TellerId, read),
    NewT = T#teller{balance = T#teller.balance + Delta},
    ok = mnesia:write(teller, NewT, sticky_write),

    %% Add delta to account balance
    [A]  = mnesia:read(account, AccountId, read),
    NewA = A#account{balance = A#account.balance + Delta},
    ok = mnesia:write(account, NewA, sticky_write),

    %% Append to history log
    History = #history{history_id   = {DriverId, HistoryId},
		       account_id   = AccountId,
		       teller_id    = TellerId,
		       branch_id    = BranchId,
		       amount       = Delta
		      },
    ok = mnesia:write(history, History, sticky_write),

    %% Return account balance
    NewA#account.balance.

frag_add_delta(DriverId, BranchId, TellerId, AccountBranchId, AccountId, HistoryId, Delta) ->
    %% Access fragmented table
    %% Grab write lock already when the record is read 

    %% Add delta to branch balance
    [B] = mnesia:read(branch, BranchId, write),
    NewB = B#branch{balance = B#branch.balance + Delta},
    ok = mnesia:write(NewB),

    %% Add delta to teller balance
    [T] = mnesia:read({teller, BranchId}, TellerId, write),
    NewT = T#teller{balance = T#teller.balance + Delta},
    ok = mnesia:write(NewT),

    %% Add delta to account balance
    %%io:format("frag_add_delta(~p): ~p\n", [node(), {account, BranchId, AccountId}]),
    [A] = mnesia:read({account, AccountBranchId}, AccountId, write),
    NewA = A#account{balance = A#account.balance + Delta},
    ok = mnesia:write(NewA),

    %% Append to history log
    History = #history{history_id   = {DriverId, HistoryId},
		       account_id   = AccountId,
		       teller_id    = TellerId,
		       branch_id    = BranchId,
		       amount       = Delta
		      },
    ok = mnesia:write(History),

    %% Return account balance
    NewA#account.balance.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Verify table consistency

verify_tabs() ->
    Nodes = mnesia:system_info(running_db_nodes),
    case lists:member(node(), Nodes) of
	true ->
	    Tabs = [branch, teller, account, history],
	    io:format("TPC-B: Verifying tables: ~w~n", [Tabs]),
	    rpc:multicall(Nodes, mnesia, wait_for_tables, [Tabs, infinity]),
	    
	    Fun = fun() ->
			  mnesia:write_lock_table(branch),
			  mnesia:write_lock_table(teller),
			  mnesia:write_lock_table(account),
			  mnesia:write_lock_table(history),
			  {Res, BadNodes} =
			      rpc:multicall(Nodes, ?MODULE, count_balance, []),
			  check_balance(Res, BadNodes)
		  end,
	    case mnesia:transaction(Fun) of
		{atomic, Res} -> Res;
		{aborted, Reason} -> {error, Reason}
	    end;
	false ->
	    {error, "Must be initiated from a running db_node"}
    end.

%% Returns a list of {Table, Node, Balance} tuples
%% Assumes that no updates are performed

-record(summary, {table, node, balance, size}).

count_balance() ->
    [count_balance(branch, #branch.balance),
     count_balance(teller, #teller.balance),
     count_balance(account, #account.balance)].

count_balance(Tab, BalPos) ->
    Frags = table_info(Tab, frag_names),
    count_balance(Tab, Frags, 0, 0, BalPos).

count_balance(Tab, [Frag | Frags], Bal, Size, BalPos) ->
    First = mnesia:dirty_first(Frag),
    {Bal2, Size2} = count_frag_balance(Frag, First, Bal, Size, BalPos),
    count_balance(Tab, Frags, Bal2, Size2, BalPos);
count_balance(Tab, [], Bal, Size, _BalPos) ->
    #summary{table = Tab, node = node(), balance = Bal, size = Size}.

count_frag_balance(_Frag, '$end_of_table', Bal, Size, _BalPos) ->
    {Bal, Size};
count_frag_balance(Frag, Key, Bal, Size, BalPos) ->
    [Record] = mnesia:dirty_read({Frag, Key}),
    Bal2 = Bal + element(BalPos, Record),
    Next = mnesia:dirty_next(Frag, Key),
    count_frag_balance(Frag, Next, Bal2, Size + 1, BalPos).

check_balance([], []) ->
    mnesia:abort({"No balance"});
check_balance(Summaries, []) ->
    [One | Rest] = lists:flatten(Summaries),
    Balance = One#summary.balance,
    %% Size = One#summary.size,
    case [S ||  S <- Rest, S#summary.balance =/= Balance] of
	[] ->
	    ok;
	BadSummaries ->
	    mnesia:abort({"Bad balance", One, BadSummaries})
    end;
check_balance(_, BadNodes) ->
    mnesia:abort({"Bad nodes", BadNodes}).
