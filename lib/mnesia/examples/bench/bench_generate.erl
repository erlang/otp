%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File    : bench_generate.hrl
%%% Author  : Hakan Mattsson <hakan@cslab.ericsson.se>
%%% Purpose : Start request generators and collect statistics
%%% Created : 21 Jun 2001 by Hakan Mattsson <hakan@cslab.ericsson.se>
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(bench_generate).
-author('hakan@cslab.ericsson.se').

-include("bench.hrl").

%% Public
-export([start/1]).

%% Internal
-export([
	 monitor_init/2,
	 generator_init/2,
	 worker_init/1
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The traffic generator
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% -------------------------------------------------------------------
%% Start request generators
%% -------------------------------------------------------------------

start(C) when is_record(C, config) ->
    MonPid = spawn_link(?MODULE, monitor_init, [C, self()]),
    receive
	{'EXIT', MonPid, Reason} ->
	    exit(Reason);
	{monitor_done, MonPid, Res} ->
	    Res
    end.

monitor_init(C, Parent) when is_record(C, config) ->
    process_flag(trap_exit, true),
    %% net_kernel:monitor_nodes(true), %% BUGBUG: Needed in order to re-start generators
    Nodes     = C#config.generator_nodes,
    PerNode   = C#config.n_generators_per_node,
    Timer     = C#config.generator_warmup,
    ?d("~n", []),
    ?d("Start ~p request generators each at ~p nodes...~n",
       [PerNode, length(Nodes)]),
    ?d("~n", []),
    warmup_sticky(C),
    ?d("    ~p seconds warmup...~n", [Timer div 1000]),
    Alive = spawn_generators(C, Nodes, PerNode),
    erlang:send_after(Timer, self(), warmup_done),
    monitor_loop(C, Parent, Alive, []).

spawn_generators(C, Nodes, PerNode) ->
    [spawn_link(Node, ?MODULE, generator_init, [self(), C]) ||
	Node <- Nodes,
	_    <- lists:seq(1, PerNode)].
    
warmup_sticky(C) ->
    %% Select one node per fragment as master node
    Tabs = [subscriber, session, server, suffix],
    Fun = fun(S) ->
		  {[Node | _], _, Wlock} = nearest_node(S, transaction, C),
		  Stick = fun() -> [mnesia:read({T, S}, S, Wlock) || T <- Tabs] end,
		  Args = [transaction, Stick, [], mnesia_frag],
		  rpc:call(Node, mnesia, activity, Args)
	  end,
    Suffixes = lists:seq(0, C#config.n_fragments - 1), % Assume even distrib.
    lists:foreach(Fun, Suffixes).

%% Main loop for benchmark monitor
monitor_loop(C, Parent, Alive, Deceased) ->
    receive
        warmup_done ->
            multicall(Alive, reset_statistics),	    
	    Timer = C#config.generator_duration,
	    ?d("    ~p seconds actual benchmarking...~n", [Timer div 1000]),
	    erlang:send_after(Timer, self(), measurement_done),
	    monitor_loop(C, Parent, Alive, Deceased);
        measurement_done ->
            Stats = multicall(Alive, get_statistics),	    
	    Timer = C#config.generator_cooldown,
	    ?d("    ~p seconds cooldown...~n", [Timer div 1000]),
	    erlang:send_after(Timer, self(), {cooldown_done, Stats}),
	    monitor_loop(C, Parent, Alive, Deceased);
        {cooldown_done, Stats} ->
            multicall(Alive, stop),
            display_statistics(Stats, C),
            Parent ! {monitor_done, self(), ok},
	    unlink(Parent),
	    exit(monitor_done);
	{nodedown, _Node} ->
	    monitor_loop(C, Parent, Alive, Deceased);
	{nodeup, Node} ->
	    NeedsBirth = [N || N <- Deceased, N == Node],
	    Born = spawn_generators(C, NeedsBirth, 1),
	    monitor_loop(C, Parent, Born ++ Alive, Deceased -- NeedsBirth);
        {'EXIT', Pid, Reason} when Pid == Parent ->
	    exit(Reason);
        {'EXIT', Pid, Reason} ->
            case lists:member(Pid, Alive) of
                true ->
		    ?d("Generator on node ~p died: ~p~n", [node(Pid), Reason]),
                    monitor_loop(C, Parent, Alive -- [Pid], [node(Pid) | Deceased]);
                false ->
                    monitor_loop(C, Parent, Alive, Deceased)
            end
    end.

%% Send message to a set of processes and wait for their replies
multicall(Pids, Message) ->
    Send =
        fun(Pid) -> 
                Ref = erlang:monitor(process, Pid),
                Pid ! {self(), Ref, Message},
                {Pid, Ref}
        end,
    PidRefs = lists:map(Send, Pids),
    Collect =
        fun({Pid, Ref}) ->
                receive
                    {'DOWN', Ref, process, Pid, Reason} ->
                        {Pid, {'EXIT', Reason}};
                    {Pid, Ref, Reply} ->
                        erlang:demonitor(Ref),
                        {Pid, Reply}
                end
        end,
    lists:map(Collect, PidRefs).

%% Initialize a traffic generator
generator_init(Monitor, C) ->
    process_flag(trap_exit, true),
    Tables = mnesia:system_info(tables),
    ok = mnesia:wait_for_tables(Tables, infinity),
    rand:seed(exsplus),
    Counters = reset_counters(C, C#config.statistics_detail),
    SessionTab = ets:new(bench_sessions, [public, {keypos, 1}]),
    generator_loop(Monitor, C, SessionTab, Counters).

%% Main loop for traffic generator
generator_loop(Monitor, C, SessionTab, Counters) ->
    receive
        {ReplyTo, Ref, get_statistics} ->
	    Stats = get_counters(C, Counters),
	    ReplyTo ! {self(), Ref, Stats},
	    generator_loop(Monitor, C, SessionTab, Counters);
        {ReplyTo, Ref, reset_statistics} ->
	    Stats = get_counters(C, Counters),
	    Counters2 = reset_counters(C, Counters),
	    ReplyTo ! {self(), Ref, Stats},
	    generator_loop(Monitor, C, SessionTab, Counters2);
        {_ReplyTo, _Ref, stop} ->
	    exit(shutdown);
        {'EXIT', Pid, Reason} when Pid == Monitor ->
	    exit(Reason);
	{'EXIT', Pid, Reason} ->
	    Node = node(Pid),
	    ?d("Worker on node ~p(~p) died: ~p~n", [Node, node(), Reason]),
	    Key = {worker,Node},
	    case get(Key) of
		undefined -> ignore;
		Pid       -> erase(Key);
		_         -> ignore
	    end,
	    generator_loop(Monitor, C, SessionTab, Counters)
    after 0 ->
	    {Name, {Nodes, Activity, Wlock}, Fun, CommitSessions} =
		gen_trans(C, SessionTab),
	    Before = erlang:monotonic_time(),
	    Res  = call_worker(Nodes, Activity, Fun, Wlock, mnesia_frag),
	    After = erlang:monotonic_time(),
	    Elapsed = elapsed(Before, After),
	    post_eval(Monitor, C, Elapsed, Res, Name, CommitSessions, SessionTab, Counters)
    end.

%% Perform a transaction on a node near the data
call_worker([Node | _], Activity, Fun, Wlock, Mod) when Node == node() ->
    {Node, catch mnesia:activity(Activity, Fun, [Wlock], Mod)};
call_worker([Node | _] = Nodes, Activity, Fun, Wlock, Mod) ->
    Key = {worker,Node},
    case get(Key) of
	Pid when is_pid(Pid) ->
	    Args = [Activity, Fun, [Wlock], Mod],
	    Pid ! {activity, self(), Args},
	    receive
		{'EXIT', Pid, Reason} ->
		    ?d("Worker on node ~p(~p) died: ~p~n", [Node, node(), Reason]),
		    erase(Key),
		    retry_worker(Nodes, Activity, Fun, Wlock, Mod, {'EXIT', Reason});
		{activity_result, Pid, Result} ->
		    case Result of
			{'EXIT', {aborted, {not_local, _}}} ->
			    retry_worker(Nodes, Activity, Fun, Wlock, Mod, Result);
			_ ->
			    {Node, Result}
		    end
	    end;
	undefined ->
	    GenPid = self(),
	    Pid = spawn_link(Node, ?MODULE, worker_init, [GenPid]),
	    put(Key, Pid),
	    call_worker(Nodes, Activity, Fun, Wlock, Mod)
    end.

retry_worker([], _Activity, _Fun, _Wlock, _Mod, Reason) ->
    {node(), Reason};
retry_worker([BadNode | SpareNodes], Activity, Fun, Wlock, Mod, Reason) ->
    Nodes = SpareNodes -- [BadNode],
    case Nodes of
	[] ->
	    {BadNode, Reason};
	[_] ->
	    call_worker(Nodes, Activity, Fun, write, Mod);
	_ ->
	    call_worker(Nodes, Activity, Fun, Wlock, Mod)
    end.

worker_init(Parent) ->
    Tables = mnesia:system_info(tables),
    ok = mnesia:wait_for_tables(Tables, infinity),
    worker_loop(Parent).

%% Main loop for remote workers
worker_loop(Parent) ->
    receive
	{activity, Parent, [Activity, Fun, Extra, Mod]} ->
	    Result = (catch mnesia:activity(Activity, Fun, Extra, Mod)),
	    Parent ! {activity_result, self(), Result},
	    worker_loop(Parent)
    end.


elapsed(Before, After) ->
    erlang:convert_time_unit(After-Before, native, micro_seconds).

%% Lookup counters
get_counters(_C, {table, Tab}) ->
    ets:match_object(Tab, '_');
get_counters(_C, {NM, NC, NA, NB}) ->
    Trans = any,
    Node  = somewhere,
    [{{Trans, n_micros, Node}, NM},
     {{Trans, n_commits, Node}, NC},
     {{Trans, n_aborts, Node}, NA},
     {{Trans, n_branches_executed, Node}, NB}].

% Clear all counters    
reset_counters(_C, normal) ->
    {0, 0, 0, 0};
reset_counters(C, {_, _, _, _}) ->
    reset_counters(C, normal);
reset_counters(C, debug) ->
    CounterTab = ets:new(bench_pending, [public, {keypos, 1}]),
    reset_counters(C, {table, CounterTab});
reset_counters(C, debug2) ->
    CounterTab = ets:new(bench_pending, [public, {keypos, 1}]),
    reset_counters(C, {table, CounterTab});
reset_counters(C, {table, Tab} = Counters) ->
    Names = [n_micros, n_commits, n_aborts, n_branches_executed],
    Nodes = C#config.generator_nodes ++ C#config.table_nodes,
    TransTypes = [t1, t2, t3, t4, t5, ping],
    [ets:insert(Tab, {{Trans, Name, Node}, 0}) || Name <- Names,
						  Node <- Nodes,
						  Trans <- TransTypes],
    Counters.

%% Determine the outcome of a transaction and increment the counters
post_eval(Monitor, C, Elapsed, {Node, Res}, Name, CommitSessions, SessionTab, {table, Tab} = Counters) ->
    case Res of
	{do_commit, BranchExecuted, _} ->
	    incr(Tab, {Name, n_micros, Node}, Elapsed),
	    incr(Tab, {Name, n_commits, Node}, 1),
	    case BranchExecuted of
		true  ->
		    incr(Tab, {Name, n_branches_executed, Node}, 1),
		    commit_session(CommitSessions),
		    generator_loop(Monitor, C, SessionTab, Counters);
		false ->
		    generator_loop(Monitor, C, SessionTab, Counters)
	    end;
	{'EXIT', {aborted, {do_rollback, BranchExecuted, _}}} ->
	    incr(Tab, {Name, n_micros, Node}, Elapsed),
	    incr(Tab, {Name, n_aborts, Node}, 1),
	    case BranchExecuted of
		true  ->
		    incr(Tab, {Name, n_branches_executed, Node}, 1),
		    generator_loop(Monitor, C, SessionTab, Counters);
		false ->
		    generator_loop(Monitor, C, SessionTab, Counters)
	    end;
	_ ->
	    ?d("Failed(~p): ~p~n", [Node, Res]),
	    incr(Tab, {Name, n_micros, Node}, Elapsed),
	    incr(Tab, {Name, n_aborts, Node}, 1),
	    generator_loop(Monitor, C, SessionTab, Counters)
    end;
post_eval(Monitor, C, Elapsed, {_Node, Res}, _Name, CommitSessions, SessionTab, {NM, NC, NA, NB}) ->
    case Res of
	{do_commit, BranchExecuted, _} ->
	    case BranchExecuted of
		true  ->
		    commit_session(CommitSessions),
		    generator_loop(Monitor, C, SessionTab, {NM + Elapsed, NC + 1, NA, NB + 1});
		false ->
		    generator_loop(Monitor, C, SessionTab, {NM + Elapsed, NC + 1, NA, NB})
	    end;
	{'EXIT', {aborted, {do_rollback, BranchExecuted, _}}} ->
	    case BranchExecuted of
		true  ->
		    generator_loop(Monitor, C, SessionTab, {NM + Elapsed, NC, NA + 1, NB + 1});
		false ->
		    generator_loop(Monitor, C, SessionTab, {NM + Elapsed, NC, NA + 1, NB})
	    end;
	_ ->
	    ?d("Failed: ~p~n", [Res]),
	    generator_loop(Monitor, C, SessionTab, {NM + Elapsed, NC, NA + 1, NB})
    end.

incr(Tab, Counter, Incr) ->
    ets:update_counter(Tab, Counter, Incr).

commit_session(no_fun) ->
    ignore;
commit_session(Fun) when is_function(Fun, 0) ->
    Fun().

%% Randlomly choose a transaction type according to benchmar spec
gen_trans(C, SessionTab) when C#config.generator_profile == random ->
    case rand:uniform(100) of
        Rand when Rand >   0, Rand =<  25 -> gen_t1(C, SessionTab);
        Rand when Rand >  25, Rand =<  50 -> gen_t2(C, SessionTab);
        Rand when Rand >  50, Rand =<  70 -> gen_t3(C, SessionTab);
        Rand when Rand >  70, Rand =<  85 -> gen_t4(C, SessionTab);
        Rand when Rand >  85, Rand =< 100 -> gen_t5(C, SessionTab)
    end;
gen_trans(C, SessionTab) ->
    case C#config.generator_profile of
        t1   -> gen_t1(C, SessionTab);
        t2   -> gen_t2(C, SessionTab);
        t3   -> gen_t3(C, SessionTab);
        t4   -> gen_t4(C, SessionTab);
        t5   -> gen_t5(C, SessionTab);
	ping -> gen_ping(C, SessionTab)
    end.
        
gen_t1(C, _SessionTab) ->
    SubscrId    = rand:uniform(C#config.n_subscribers) - 1,
    SubscrKey   = bench_trans:number_to_key(SubscrId, C),
    Location    = 4711,
    ChangedBy   = <<4711:(8*25)>>,
    ChangedTime = <<4711:(8*25)>>,
    {t1,
     nearest_node(SubscrId, transaction, C),
     fun(Wlock) -> bench_trans:update_current_location(Wlock, SubscrKey, Location, ChangedBy, ChangedTime) end,
     no_fun
    }.

gen_t2(C, _SessionTab) ->
    SubscrId  = rand:uniform(C#config.n_subscribers) - 1,
    SubscrKey = bench_trans:number_to_key(SubscrId, C),
    {t2,
     nearest_node(SubscrId, sync_dirty, C),
     %%nearest_node(SubscrId, transaction, C),
     fun(Wlock) -> bench_trans:read_current_location(Wlock, SubscrKey) end,
     no_fun
    }.

gen_t3(C, SessionTab) ->
    case ets:first(SessionTab) of
	'$end_of_table' ->
	    %% This generator does not have any session,
	    %% try reading someone elses session details
	    SubscrId  = rand:uniform(C#config.n_subscribers) - 1,
	    SubscrKey = bench_trans:number_to_key(SubscrId, C),
	    ServerId  = rand:uniform(C#config.n_servers) - 1,
	    ServerBit = 1 bsl ServerId,
	    {t3,
	     nearest_node(SubscrId, transaction, C),
	     fun(Wlock) -> bench_trans:read_session_details(Wlock, SubscrKey, ServerBit, ServerId) end,
	     no_fun
	    };
	{SubscrId, SubscrKey, ServerId}  ->
	    %% This generator do have a session,
	    %% read its session details
	    ServerBit = 1 bsl ServerId,
	    {t3,
	     nearest_node(SubscrId, transaction, C),
	     fun(Wlock) -> bench_trans:read_session_details(Wlock, SubscrKey, ServerBit, ServerId) end,
	     no_fun
	    }
    end.

gen_t4(C, SessionTab) ->
    %% This generator may already have sessions,
    %% create a new session and hope that no other
    %% generator already has occupied it
    SubscrId   = rand:uniform(C#config.n_subscribers) - 1,
    SubscrKey  = bench_trans:number_to_key(SubscrId, C),
    ServerId   = rand:uniform(C#config.n_servers) - 1,
    ServerBit  = 1 bsl ServerId,
    Details    = <<4711:(8*2000)>>,
    DoRollback = (rand:uniform(100) =< 2),
    Insert     = fun() -> ets:insert(SessionTab, {{SubscrId, SubscrKey, ServerId}, self()}) end,
    {t4,
     nearest_node(SubscrId, transaction, C),
     fun(Wlock) -> bench_trans:create_session_to_server(Wlock, SubscrKey, ServerBit, ServerId, Details, DoRollback) end,
     Insert
    }.

gen_t5(C, SessionTab) ->
    case ets:first(SessionTab) of
	'$end_of_table' ->
	    %% This generator does not have any session,
	    %% try to delete someone elses session details
	    SubscrId   = rand:uniform(C#config.n_subscribers) - 1,
	    SubscrKey  = bench_trans:number_to_key(SubscrId, C),
	    ServerId   = rand:uniform(C#config.n_servers) - 1,
	    ServerBit  = 1 bsl ServerId,
	    DoRollback = (rand:uniform(100) =< 2),
	    {t5,
	     nearest_node(SubscrId, transaction, C),
	     fun(Wlock) -> bench_trans:delete_session_from_server(Wlock, SubscrKey, ServerBit, ServerId, DoRollback) end,
	     no_fun
	    };
	{SubscrId, SubscrKey, ServerId}  ->
	    %% This generator do have at least one session,
	    %% delete it.
	    ServerBit  = 1 bsl ServerId,	
	    DoRollback = (rand:uniform(100) =< 2),
	    Delete     = fun() -> ets:delete(SessionTab, {SubscrId, SubscrKey, ServerId}) end,
	    {t5,
	     nearest_node(SubscrId, transaction, C),
	     fun(Wlock) -> bench_trans:delete_session_from_server(Wlock, SubscrKey, ServerBit, ServerId, DoRollback) end,
	     Delete
	    }
    end.

gen_ping(C, _SessionTab) ->
    SubscrId   = rand:uniform(C#config.n_subscribers) - 1,
    {ping,
     nearest_node(SubscrId, transaction, C),
     fun(_Wlock) -> {do_commit, true, []} end,
     no_fun
    }.    

%% Select a node as near as the subscriber data as possible
nearest_node(SubscrId, Activity, C) ->
    Suffix = bench_trans:number_to_suffix(SubscrId),
    case mnesia_frag:table_info(t, s, {suffix, Suffix}, where_to_write) of
	[] ->
	    {[node()], Activity, write};
	[Node] ->
	    {[Node], Activity, write};
	Nodes ->
	    Wlock = C#config.write_lock_type,
	    if
		C#config.always_try_nearest_node; Wlock =:= write ->
		    case lists:member(node(), Nodes) of
			true ->
			    {[node() | Nodes], Activity, Wlock};
			false ->
			    Node = pick_node(Suffix, C, Nodes),
			    {[Node | Nodes], Activity, Wlock}
		    end;
		Wlock == sticky_write ->
		    Node = pick_node(Suffix, C, Nodes),
		    {[Node | Nodes], Activity, Wlock}
	    end
    end.

pick_node(Suffix, C, Nodes) ->
    Ordered = lists:sort(Nodes),
    NumberOfActive = length(Ordered),
    PoolSize = length(C#config.table_nodes),
    Suffix2 = 
	case PoolSize rem NumberOfActive of
	    0 -> Suffix div (PoolSize div NumberOfActive);
	    _ -> Suffix
	end,
    N = (Suffix2 rem NumberOfActive) + 1,
    lists:nth(N, Ordered).

display_statistics(Stats, C) ->
    GoodStats = [{node(GenPid), GenStats} || {GenPid, GenStats} <- Stats,
					     is_list(GenStats)],
    FlatStats = [{Type, Name, EvalNode, GenNode, Count} ||
                    {GenNode, GenStats} <- GoodStats,
                    {{Type, Name, EvalNode}, Count} <- GenStats],
    TotalStats = calc_stats_per_tag(lists:keysort(2, FlatStats), 2, []),
    {value, {n_aborts, 0, NA, 0, 0}} =
     lists:keysearch(n_aborts, 1, TotalStats ++ [{n_aborts, 0, 0, 0, 0}]),
    {value, {n_commits, NC, 0, 0, 0}} =
	lists:keysearch(n_commits, 1, TotalStats ++ [{n_commits, 0, 0, 0, 0}]),
    {value, {n_branches_executed, 0, 0, _NB, 0}} =
	lists:keysearch(n_branches_executed, 1, TotalStats ++ [{n_branches_executed, 0, 0, 0, 0}]),
    {value, {n_micros, 0, 0, 0, AccMicros}} =
	lists:keysearch(n_micros, 1, TotalStats ++ [{n_micros, 0, 0, 0, 0}]),
    NT = NA + NC,
    NG = length(GoodStats),
    NTN = length(C#config.table_nodes),
    WallMicros = C#config.generator_duration * 1000 * NG,
    Overhead = (catch (WallMicros - AccMicros) / WallMicros),
    ?d("~n", []),
    ?d("Benchmark result...~n", []),
    ?d("~n", []),
    ?d("    ~p transactions per second (TPS).~n", [catch ((NT * 1000000 * NG) div AccMicros)]),
    ?d("    ~p TPS per table node.~n", [catch ((NT * 1000000 * NG) div (AccMicros * NTN))]),
    ?d("    ~p micro seconds in average per transaction, including latency.~n",
       [catch (AccMicros div NT)]),
    ?d("    ~p transactions. ~f% generator overhead.~n", [NT, Overhead * 100]),

    TypeStats = calc_stats_per_tag(lists:keysort(1, FlatStats), 1, []),
    EvalNodeStats = calc_stats_per_tag(lists:keysort(3, FlatStats), 3, []),
    GenNodeStats = calc_stats_per_tag(lists:keysort(4, FlatStats), 4, []),
    if
	C#config.statistics_detail == normal ->
	    ignore;
	true ->
	    ?d("~n", []),
	    ?d("Statistics per transaction type...~n", []),
	    ?d("~n", []),
	    display_type_stats("    ", TypeStats, NT, AccMicros),

	    ?d("~n", []),
	    ?d("Transaction statistics per table node...~n", []),
	    ?d("~n", []),
	    display_calc_stats("    ", EvalNodeStats, NT, AccMicros),

	    ?d("~n", []),
	    ?d("Transaction statistics per generator node...~n", []),
	    ?d("~n", []),
	    display_calc_stats("    ", GenNodeStats, NT, AccMicros)
    end,
    if
	C#config.statistics_detail /= debug2 ->
	    ignore;
	true ->
	    io:format("~n", []),
	    io:format("------ Test Results ------~n", []),
	    io:format("Length        : ~p sec~n", [C#config.generator_duration div 1000]),
	    Host = lists:nth(2, string:tokens(atom_to_list(node()), [$@])),
	    io:format("Processor     : ~s~n", [Host]),
	    io:format("Number of Proc: ~p~n", [NG]),
	    io:format("~n", []),
	    display_trans_stats("    ", TypeStats, NT, AccMicros, NG),
	    io:format("~n", []),
	    io:format("  Overall Statistics~n", []),
	    io:format("     Transactions: ~p~n", [NT]),
	    io:format("     Inner       : ~p TPS~n", [catch ((NT * 1000000 * NG) div AccMicros)]),
	    io:format("     Outer       : ~p TPS~n", [catch ((NT * 1000000 * NG) div WallMicros)]),
	    io:format("~n", [])
    end.
    
 
display_calc_stats(Prefix, [{_Tag, 0, 0, 0, 0} | Rest], NT, Micros) ->
    display_calc_stats(Prefix, Rest, NT, Micros);   
display_calc_stats(Prefix, [{Tag, NC, NA, _NB, NM} | Rest], NT, Micros) ->
    ?d("~s~s n=~s%\ttime=~s%~n",
       [Prefix, left(Tag), percent(NC + NA, NT), percent(NM, Micros)]),
    display_calc_stats(Prefix, Rest, NT, Micros);   
display_calc_stats(_, [], _, _) ->
    ok.

display_type_stats(Prefix, [{_Tag, 0, 0, 0, 0} | Rest], NT, Micros) ->
    display_type_stats(Prefix, Rest, NT, Micros);   
display_type_stats(Prefix, [{Tag, NC, NA, NB, NM} | Rest], NT, Micros) ->
    ?d("~s~s n=~s%\ttime=~s%\tavg micros=~p~n",
       [
	Prefix,
	left(Tag),
	percent(NC + NA, NT),
	percent(NM, Micros),
	catch (NM div (NC + NA))
       ]),
    case NA /= 0 of
	true  -> ?d("~s    ~s% aborted~n", [Prefix, percent(NA, NC + NA)]);
	false -> ignore
    end,
    case NB /= 0 of
	true  -> ?d("~s    ~s% branches executed~n", [Prefix, percent(NB, NC + NA)]);
	false -> ignore
    end,
    display_type_stats(Prefix, Rest, NT, Micros);   
display_type_stats(_, [], _, _) ->
    ok.

left(Term) ->
    string:left(lists:flatten(io_lib:format("~p", [Term])), 27, $.).

percent(_Part, 0)     -> "infinity";
percent(Part, Total) -> io_lib:format("~8.4f", [(Part * 100) / Total]).

calc_stats_per_tag([], _Pos, Acc) ->
    lists:sort(Acc);
calc_stats_per_tag([Tuple | _] = FlatStats, Pos, Acc) when size(Tuple) == 5 ->
    Tag = element(Pos, Tuple),
    do_calc_stats_per_tag(FlatStats, Pos, {Tag, 0, 0, 0, 0}, Acc).

do_calc_stats_per_tag([Tuple | Rest], Pos, {Tag, NC, NA, NB, NM}, Acc)
  when element(Pos, Tuple) == Tag ->
    Val = element(5, Tuple),
    case element(2, Tuple) of
        n_commits ->
            do_calc_stats_per_tag(Rest, Pos, {Tag, NC + Val, NA, NB, NM}, Acc);
        n_aborts ->
            do_calc_stats_per_tag(Rest, Pos, {Tag, NC, NA + Val, NB, NM}, Acc);
        n_branches_executed ->
            do_calc_stats_per_tag(Rest, Pos, {Tag, NC, NA, NB + Val, NM}, Acc);
        n_micros ->
            do_calc_stats_per_tag(Rest, Pos, {Tag, NC, NA, NB, NM + Val}, Acc)
    end;
do_calc_stats_per_tag(GenStats, Pos, CalcStats, Acc) ->
    calc_stats_per_tag(GenStats, Pos, [CalcStats | Acc]).

display_trans_stats(Prefix, [{_Tag, 0, 0, 0, 0} | Rest], NT, Micros, NG) ->
    display_trans_stats(Prefix, Rest, NT, Micros, NG);   
display_trans_stats(Prefix, [{Tag, NC, NA, NB, NM} | Rest], NT, Micros, NG) ->
    Common =
	fun(Name) ->
           Sec = NM / (1000000 * NG),
	   io:format("  ~s: ~p (~p%) Time: ~p sec TPS = ~p~n",
		     [Name,
		      NC + NA,
		      round(((NC + NA) * 100) / NT),
		      round(Sec),
		      round((NC + NA) / Sec)])
        end,
    Branch = 
	fun() -> 
            io:format("      Branches Executed: ~p (~p%)~n",
		      [NB, round((NB * 100) / (NC + NA))])
	end,
    Rollback =
	fun() -> 	
            io:format("      Rollback Executed: ~p (~p%)~n",
		      [NA, round((NA * 100) / (NC + NA))])
	end,
    case Tag of
	t1 ->
	    Common("T1");
	t2 ->
	    Common("T2");
	t3 ->
	    Common("T3"),
	    Branch();
	t4 ->
	    Common("T4"),
	    Branch(),
	    Rollback();
	t5 ->
	    Common("T5"),
	    Branch(),
	    Rollback();
	_ ->
	    Common(io_lib:format("~p", [Tag]))
    end,
    display_trans_stats(Prefix, Rest, NT, Micros, NG);   
display_trans_stats(_, [], _, _, _) ->
    ok.

