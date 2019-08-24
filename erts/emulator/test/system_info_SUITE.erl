%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2018. All Rights Reserved.
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


%%%-------------------------------------------------------------------
%%% File    : system_info_SUITE.erl
%%% Author  : Rickard Green <rickard.s.green@ericsson.com>
%%% Description : Misc tests of erlang:system_info/1
%%%
%%% Created : 15 Jul 2005 by Rickard Green <rickard.s.green@ericsson.com>
%%%-------------------------------------------------------------------
-module(system_info_SUITE).
-author('rickard.s.green@ericsson.com').

%-define(line_trace, 1).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0]).

-export([process_count/1, system_version/1, misc_smoke_tests/1,
         heap_size/1, wordsize/1, memory/1, ets_limit/1, atom_limit/1,
         procs_bug/1,
         ets_count/1, atom_count/1, system_logger/1]).

-export([init/1, handle_event/2, handle_call/2]).

-export([init_per_testcase/2, end_per_testcase/2]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 2}}].

all() -> 
    [process_count, system_version, misc_smoke_tests,
     ets_count, heap_size, wordsize, memory, ets_limit, atom_limit, atom_count,
     procs_bug,
     system_logger].


init_per_testcase(procs_bug, Config) ->
    procs_bug(init_per_testcase, Config);
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(procs_bug, Config) ->
    procs_bug(end_per_testcase, Config);
end_per_testcase(_, _) ->
    ok.

%%%
%%% The test cases -------------------------------------------------------------
%%%

process_count(Config) when is_list(Config) ->
    case catch erlang:system_info(modified_timing_level) of
	Level when is_integer(Level) ->
	    {skipped,
	     "Modified timing (level " ++ integer_to_list(Level)
	     ++ ") is enabled. spawn() is too slow for this "
	     " test when modified timing is enabled."};
	_ ->
	    process_count_test()
    end.

process_count_test() ->
    OldPrio = process_flag(priority, max),
    check_procs(10),
    check_procs(11234),
    check_procs(57),
    check_procs(1030),
    check_procs(687),
    check_procs(7923),
    check_procs(5302),
    check_procs(12456),
    check_procs(14),
    check_procs(1125),
    check_procs(236),
    check_procs(125),
    check_procs(2346),
    process_flag(priority, OldPrio),
    ok.
    

check_procs(N) ->
    CP = length(processes()),
    Procs = start_procs(N),
    check_pc(CP+N),
    stop_procs(Procs),
    check_pc(CP).

check_pc(E) ->
    P = length(processes()),
    SI = erlang:system_info(process_count),
    io:format("E=~p; P=~p; SI=~p~n", [E, P, SI]),
    E = P,
    P = SI.

start_procs(N) ->
    lists:map(fun (_) ->
		      P = spawn_opt(fun () ->
					    receive after infinity -> bye end
				    end,
				    [{priority, max}]),
		      {P, erlang:monitor(process, P)}
	      end,
	      lists:seq(1, N)).

stop_procs(PMs) ->
    lists:foreach(fun ({P, _}) ->
			  exit(P, boom)
		  end, PMs),
    lists:foreach(fun ({P, M}) ->
			  receive {'DOWN', M, process, P, boom} -> ok end
		  end, PMs).


system_version(Config) when is_list(Config) ->
    {comment, erlang:system_info(system_version)}.

misc_smoke_tests(Config) when is_list(Config) ->
    true = is_binary(erlang:system_info(info)),
    true = is_binary(erlang:system_info(procs)),
    true = is_binary(erlang:system_info(loaded)),
    true = is_binary(erlang:system_info(dist)),
    ok = try erlang:system_info({cpu_topology,erts_get_cpu_topology_error_case}), fail catch error:badarg -> ok end,
    true = lists:member(erlang:system_info(tolerant_timeofday), [enabled, disabled]),
    ok.
    

heap_size(Config) when is_list(Config) ->
   {min_bin_vheap_size, VHmin} = erlang:system_info(min_bin_vheap_size),
   {min_heap_size, Hmin} =  erlang:system_info(min_heap_size),
   GCinf =  erlang:system_info(garbage_collection),
   VHmin = proplists:get_value(min_bin_vheap_size, GCinf),
   Hmin  = proplists:get_value(min_heap_size, GCinf),
   ok.

%% Tests the various wordsize variants
wordsize(Config) when is_list(Config) ->
    A = erlang:system_info(wordsize),
    true = is_integer(A),
    A = erlang:system_info({wordsize,internal}),
    B = erlang:system_info({wordsize,external}),
    true = A =< B,
    case {B,A} of
	{4,4} ->
	    {comment, "True 32-bit emulator"};
	{8,8} ->
	    {comment, "True 64-bit emulator"};
	Other ->
	    exit({unexpected_wordsizes,Other})
    end.

%% Verify that erlang:memory/0 and memory results in crashdump produce are similar
memory(Config) when is_list(Config) ->
    %%
    %% Verify that erlang:memory/0 and memory results in
    %% crashdump produce are similar.
    %%
    %% erlang:memory/0 requests information from each scheduler
    %% thread and puts the information together in erlang code
    %% (erlang.erl).
    %%
    %% When a crash dump is written we cannot use the
    %% erlang:memory/0 implementation. The crashdump implementation
    %% is a pure C implementation inspecting all allocator instances
    %% after the system has been blocked (erts_memory() in erl_alloc.c).
    %%
    %% Since we got two implementations, modifications can easily
    %% cause them to produce different results.
    %%
    %% erts_debug:get_internal_state(memory) blocks the system and
    %% execute the same code as the crash dump writing uses.
    %%

    erts_debug:set_internal_state(available_internal_state, true),
    %% Use a large heap size on the controlling process in
    %% order to avoid changes in its heap size during
    %% comparisons.
    MinHeapSize = process_flag(min_heap_size, 1024*1024), 
    Prio = process_flag(priority, max),
    try
	erlang:memory(), %% first call will init stat atoms
	garbage_collect(), %% blow up heap
	memory_test(Config)
    catch
	error:notsup -> {skipped, "erlang:memory() not supported"}
    after
	process_flag(min_heap_size, MinHeapSize),
	process_flag(priority, Prio),
	catch erts_debug:set_internal_state(available_internal_state, false)
    end.

memory_test(_Config) ->

    MWs = spawn_mem_workers(),

    DPs = mem_workers_call(MWs,
			   fun () ->
				   mapn(fun (_) ->
						spawn(fun () ->
							      receive
							      after infinity ->
								      ok
							      end
						      end)
					end,
					1000 div erlang:system_info(schedulers_online))
			   end, []),
    cmp_memory(MWs, "spawn procs"),

    Ps = lists:flatten(DPs),

    mem_workers_call(MWs, 
		     fun () ->
			     lists:foreach(fun (P) -> link(P) end, Ps)
		     end, []),
    cmp_memory(MWs, "link procs"),
    mem_workers_call(MWs,
		     fun () ->
			     lists:foreach(fun (P) -> unlink(P) end, Ps)
		     end, []),
    cmp_memory(MWs, "unlink procs"),

    mem_workers_call(MWs, 
		     fun () ->
			     lists:foreach(
			       fun (P) ->
				       Tmr = erlang:start_timer(1 bsl 34,
								P,
								hello),
				       Tmrs = case get('BIF_TMRS') of
						  undefined -> [];
						  Rs -> Rs
					      end,
				       true = is_reference(Tmr),
				       put('BIF_TMRS', [Tmr|Tmrs])
			       end, Ps)
		     end, []),
    cmp_memory(MWs, "start BIF timer procs"),

    mem_workers_call(MWs, 
		     fun () ->
			     lists:foreach(fun (Tmr) ->
						   true = is_reference(Tmr),
						   true = is_integer(erlang:cancel_timer(Tmr))
					   end, get('BIF_TMRS')),
			     put('BIF_TMRS', undefined),
			     garbage_collect()
		     end, []),
    erts_debug:set_internal_state(wait, deallocations),
    cmp_memory(MWs, "cancel BIF timer procs"),

    DMs = mem_workers_call(MWs,
			   fun () ->
				   lists:map(fun (P) ->
						     monitor(process, P)
					     end, Ps)
			   end, []),
    cmp_memory(MWs, "monitor procs"),
    Ms = lists:flatten(DMs),
    mem_workers_call(MWs,
		     fun () ->
			     lists:foreach(fun (M) ->
						   demonitor(M)
					   end, Ms)
		     end, []),
    cmp_memory(MWs, "demonitor procs"),

    mem_workers_call(MWs,
		     fun () ->
			     lists:foreach(fun (P) ->
						   P ! {a, "message", make_ref()}
					   end, Ps)
		     end, []),
    cmp_memory(MWs, "message procs"),

    mem_workers_call(MWs,
		     fun () ->
			     Mons = lists:map(fun (P) ->
						      exit(P, kill),
						      monitor(process, P)
					      end,
					      Ps),
			     lists:foreach(fun (Mon) ->
						   receive
						       {'DOWN', Mon, _, _, _} -> ok
						   end
					   end,
					   Mons)
		     end, []),
    cmp_memory(MWs, "kill procs"),

    mem_workers_call(MWs,
		     fun () ->
			     put(binary_data,
				 mapn(fun (_) -> list_to_binary(lists:duplicate(256,$?)) end, 100))
		     end, []),

    cmp_memory(MWs, "store binary data"),

    mem_workers_call(MWs,
		     fun () ->
			     put(binary_data, false),
			     garbage_collect()
		     end, []),
    cmp_memory(MWs, "release binary data"),

    mem_workers_call(MWs,
		     fun () ->
			     _ = list_to_atom("an ugly atom "++integer_to_list(erlang:system_info(scheduler_id))),
			     _ = list_to_atom("another ugly atom "++integer_to_list(erlang:system_info(scheduler_id))),
			     _ = list_to_atom("yet another ugly atom "++integer_to_list(erlang:system_info(scheduler_id)))
		     end, []),
    cmp_memory(MWs, "new atoms"),


    mem_workers_call(MWs,
		     fun () ->
			     T = ets:new(?MODULE, []),
			     ets:insert(T, {gurka, lists:seq(1,10000)}),
			     ets:insert(T, {banan, lists:seq(1,1024)}),
			     ets:insert(T, {appelsin, make_ref()}),
			     put(ets_id, T)
		     end, []),
    cmp_memory(MWs, "store ets data"),

    mem_workers_call(MWs,
		     fun () ->
			     ets:delete(get(ets_id)),
			     put(ets_id, false)
		     end, []),
    cmp_memory(MWs, "remove ets data"),

    lists:foreach(fun (MW) ->
			  unlink(MW),
			  Mon = monitor(process, MW),
			  exit(MW, kill),
			  receive
			      {'DOWN', Mon, _, _, _} -> ok
			  end
		  end, MWs),
    ok.

mem_worker() ->
    receive
	{call, From, Fun, Args} ->
	    From ! {reply, self(), apply(Fun, Args)},
	    mem_worker();
	{cast, _From, Fun, Args} ->
	    apply(Fun, Args),
	    mem_worker()
    end.

mem_workers_call(MWs, Fun, Args) ->
    lists:foreach(fun (MW) ->
                          MW ! {call, self(), Fun, Args}
                  end, MWs),
    lists:map(fun (MW) ->
                      receive
                          {reply, MW, Res} ->
                              Res
                      end
              end, MWs).

spawn_mem_workers() ->
    spawn_mem_workers(erlang:system_info(schedulers_online)).

spawn_mem_workers(0) ->
    [];
spawn_mem_workers(N) ->
    [spawn_opt(fun () -> mem_worker() end,
	       [{scheduler, N rem erlang:system_info(schedulers_online) + 1},
		link]) | spawn_mem_workers(N-1)].


mem_get(X, Mem) ->
    case lists:keyfind(X, 1, Mem) of
	{X, Val} -> Val;
	false -> false
    end.

cmp_memory(What, Mem1, Mem2, 1) ->
    R1 = mem_get(What, Mem1),
    R2 = mem_get(What, Mem2),
    true = R1 == R2;
cmp_memory(What, Mem1, Mem2, RelDiff) ->
    %% We allow RealDiff diff
    R1 = mem_get(What, Mem1),
    R2 = mem_get(What, Mem2),
    case R1 == R2 of
	true ->
	    ok;
	false ->
	    case R1 > R2 of
		true ->
		    true = R2*RelDiff > R1;
		false ->
		    true = R1*RelDiff > R2
	    end
    end.

pos_int(Val) when Val >= 0 ->
    Val;
pos_int(Val) ->
    exit({not_pos_int, Val}).

check_sane_memory(Mem) ->
    Tot = pos_int(mem_get(total, Mem)),
    Proc = pos_int(mem_get(processes, Mem)),
    ProcUsed = pos_int(mem_get(processes_used, Mem)),
    Sys = pos_int(mem_get(system, Mem)),
    Atom = pos_int(mem_get(atom, Mem)),
    AtomUsed = pos_int(mem_get(atom_used, Mem)),
    Bin = pos_int(mem_get(binary, Mem)),
    Code = pos_int(mem_get(code, Mem)),
    Ets = pos_int(mem_get(ets, Mem)),

    Tot = Proc + Sys,
    true = Sys > Atom + Bin + Code + Ets,
    true = Proc >= ProcUsed,
    true = Atom >= AtomUsed,

    case mem_get(maximum, Mem) of
	false -> ok;
	Max -> true = pos_int(Max) >= Tot
    end,
    ok.

cmp_memory(MWs, Str) ->
    erlang:display(Str),
    lists:foreach(fun (MW) -> garbage_collect(MW) end, MWs),
    garbage_collect(),
    erts_debug:set_internal_state(wait, deallocations),

    EDM = erts_debug:get_internal_state(memory),
    EM = erlang:memory(),

    io:format("~s:~n"
	      "erlang:memory() = ~p~n"
	      "crash dump memory = ~p~n",
	      [Str, EM, EDM]),

    check_sane_memory(EM),
    check_sane_memory(EDM),

    %% We expect these to always give us exactly the same result

    cmp_memory(atom, EM, EDM, 1),
    cmp_memory(atom_used, EM, EDM, 1),
    cmp_memory(binary, EM, EDM, 1),
    cmp_memory(code, EM, EDM, 1),
    cmp_memory(ets, EM, EDM, 1),

    %% Total, processes, processes_used, and system will seldom
    %% give us exactly the same result since the two readings
    %% aren't taken atomically.
    %%
    %% Torerance is scaled according to the number of schedulers
    %% to match spawn_mem_workers.

    Tolerance = 1.05 + 0.01 * erlang:system_info(schedulers_online),

    cmp_memory(total, EM, EDM, Tolerance),
    cmp_memory(processes, EM, EDM, Tolerance),
    cmp_memory(processes_used, EM, EDM, Tolerance),
    cmp_memory(system, EM, EDM, Tolerance),

    ok.
    
mapn(_Fun, 0) ->
    [];
mapn(Fun, N) ->
    [Fun(N) | mapn(Fun, N-1)].


get_node_name(Config) ->
    list_to_atom(atom_to_list(?MODULE)
		 ++ "-"
		 ++ atom_to_list(proplists:get_value(testcase, Config))
		 ++ "-"
		 ++ integer_to_list(erlang:system_time(second))
		 ++ "-"
		 ++ integer_to_list(erlang:unique_integer([positive]))).

ets_count(Config) when is_list(Config) ->
    [ets_count_do([Type | Named])
     || Type <- [set, bag, duplicate_bag, ordered_set],
        Named <- [[named_table], []]
    ],
    ok.

ets_count_do(Opts) ->
    Before = erlang:system_info(ets_count),
    T = ets:new(?MODULE, Opts),
    After = erlang:system_info(ets_count),
    After = Before + 1,
    ets:delete(T),
    Before = erlang:system_info(ets_count).


%% Verify system_info(ets_limit) reflects max ETS table settings.
ets_limit(Config0) when is_list(Config0) ->
    Config = [{testcase,ets_limit}|Config0],
    true = is_integer(get_ets_limit(Config)),
    12345 = get_ets_limit(Config, 12345),
    ok.

get_ets_limit(Config) ->
    get_ets_limit(Config, 0).
get_ets_limit(Config, EtsMax) ->
    Envs = case EtsMax of
               0 -> [];
               _ -> [{"ERL_MAX_ETS_TABLES", integer_to_list(EtsMax)}]
           end,
    {ok, Node} = start_node_ets(Config, Envs),
    Me = self(),
    Ref = make_ref(),
    spawn_link(Node,
               fun() ->
                       Res = erlang:system_info(ets_limit),
                       unlink(Me),
                       Me ! {Ref, Res}
               end),
    receive
        {Ref, Res} ->
            Res
    end,
    stop_node(Node),
    Res.

start_node_ets(Config, Envs) when is_list(Config) ->
    Pa = filename:dirname(code:which(?MODULE)),
    test_server:start_node(get_node_name(Config), peer,
			   [{args, "-pa "++Pa}, {env, Envs}]).

start_node_atm(Config, AtomsMax) when is_list(Config) ->
    Pa = filename:dirname(code:which(?MODULE)),
    test_server:start_node(get_node_name(Config), peer,
			   [{args, "-pa "++ Pa ++ AtomsMax}]).

stop_node(Node) ->
    test_server:stop_node(Node).


%% Verify system_info(atom_limit) reflects max atoms settings
%% (using " +t").
atom_limit(Config0) when is_list(Config0) ->
    Config = [{testcase,atom_limit}|Config0],
    2186042 = get_atom_limit(Config, " +t 2186042 "),
    ok.

get_atom_limit(Config, AtomsMax) ->
    {ok, Node} = start_node_atm(Config, AtomsMax),
    Me = self(),
    Ref = make_ref(),
    spawn_link(Node,
        fun() ->
            Res = erlang:system_info(atom_limit),
            unlink(Me),
            Me ! {Ref, Res}
        end),
    receive
        {Ref, Res} ->
            Res
    end,
    stop_node(Node),
    Res.

%% Verify that system_info(atom_count) works.
atom_count(Config) when is_list(Config) ->
    Limit = erlang:system_info(atom_limit),
    Count1 = erlang:system_info(atom_count),
    list_to_atom(integer_to_list(erlang:unique_integer())),
    Count2 = erlang:system_info(atom_count),
    true = Limit >= Count2,
    true = Count2 > Count1,
    ok.


system_logger(Config) when is_list(Config) ->

    TC = self(),

    ok = error_logger:add_report_handler(?MODULE, [TC]),

    generate_log_event(),

    flush(1, report_handler),

    Initial = erlang:system_info(system_logger),

    {Logger,_} = spawn_monitor(fun F() -> receive M -> TC ! {system_logger,M}, F() end end),

    Initial = erlang:system_flag(system_logger, Logger),
    Logger = erlang:system_info(system_logger),

    generate_log_event(),
    flush(1, system_logger),

    Logger = erlang:system_flag(system_logger, Logger),

    generate_log_event(),
    flush(1, system_logger),

    exit(Logger, die),
    receive {'DOWN',_,_,_,_} -> ok end,

    generate_log_event(),
    flush(1, report_handler),

    logger = erlang:system_info(system_logger),

    logger = erlang:system_flag(system_logger, undefined),
    generate_log_event(),
    flush(),

    undefined = erlang:system_flag(system_logger, Initial),

    ok.

flush() ->
    receive
        M ->
            ct:fail({unexpected_message, M})
    after 0 ->
            ok
    end.

flush(0, _Pat) ->
    flush();
flush(Cnt, Pat) ->
    receive
        M when element(1,M) =:= Pat ->
            ct:log("~p",[M]),
            flush(Cnt-1, Pat)
    after 500 ->
            ct:fail({missing, Cnt, Pat})
    end.

generate_log_event() ->
    {_Pid, Ref} = spawn_monitor(fun() -> ok = nok end),
    receive {'DOWN', Ref, _, _, _} -> ok end.

init([To]) ->
    {ok, To}.

handle_call(Msg, State) ->
    {ok, Msg, State}.

handle_event(Event, State) ->
    State ! {report_handler, Event},
    {ok, State}.


%% OTP-15909: Provoke bug that would cause VM crash
%% if doing system_info(procs) when process have queued exit/down signals.
procs_bug(init_per_testcase, Config) ->
    %% Use single scheduler and process prio to starve monitoring processes
    %% from handling their received DOWN signals.
    OldSchedOnline = erlang:system_flag(schedulers_online,1),
    [{schedulers_online, OldSchedOnline} | Config];
procs_bug(end_per_testcase, Config) ->
    erlang:system_flag(schedulers_online,
                       proplists:get_value(schedulers_online, Config)),
    ok.

procs_bug(Config) when is_list(Config) ->
    {Monee,_} = spawn_opt(fun () -> receive die -> ok end end,
                         [monitor,{priority,max}]),
    Papa = self(),
    Pids = [begin
                P = spawn_opt(fun () ->
                                      erlang:monitor(process, Monee),
                                      Papa ! {self(),ready},
                                      receive "nada" -> no end
                              end,
                              [link, {priority,normal}]),
                {P, ready} = receive M -> M end,
                P
            end
            || _ <- lists:seq(1,10)],
    process_flag(priority,high),
    Monee ! die,
    {'DOWN',_,process,Monee,normal} = receive M -> M end,

    %% This call did crash VM as Pids have pending DOWN signals.
    erlang:system_info(procs),
    process_flag(priority,normal),
    [begin unlink(P), exit(P, kill) end || P <- Pids],
    ok.
