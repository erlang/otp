%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2011. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
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

-include_lib("test_server/include/test_server.hrl").

%-compile(export_all).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2]).

-export([process_count/1, system_version/1, misc_smoke_tests/1, heap_size/1, wordsize/1, memory/1,
         ets_limit/1]).

-define(DEFAULT_TIMEOUT, ?t:minutes(2)).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [process_count, system_version, misc_smoke_tests,
     heap_size, wordsize, memory, ets_limit].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(_Case, Config) when is_list(Config) ->
    Dog = ?t:timetrap(?DEFAULT_TIMEOUT),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) when is_list(Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

%%%
%%% The test cases -------------------------------------------------------------
%%%

process_count(doc) -> [];
process_count(suite) -> [];
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
    ?line OldPrio = process_flag(priority, max),
    ?line check_procs(10),
    ?line check_procs(11234),
    ?line check_procs(57),
    ?line check_procs(1030),
    ?line check_procs(687),
    ?line check_procs(7923),
    ?line check_procs(5302),
    ?line check_procs(12456),
    ?line check_procs(14),
    ?line check_procs(1125),
    ?line check_procs(236),
    ?line check_procs(125),
    ?line check_procs(2346),
    ?line process_flag(priority, OldPrio),
    ?line ok.
    

check_procs(N) ->
    ?line CP = length(processes()),
    ?line Procs = start_procs(N),
    ?line check_pc(CP+N),
    ?line stop_procs(Procs),
    ?line check_pc(CP).

check_pc(E) ->
    ?line P = length(processes()),
    ?line SI = erlang:system_info(process_count),
    ?line ?t:format("E=~p; P=~p; SI=~p~n", [E, P, SI]),
    ?line E = P,
    ?line P = SI.

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


system_version(doc) -> [];
system_version(suite) -> [];
system_version(Config) when is_list(Config) ->
    ?line {comment, erlang:system_info(system_version)}.

misc_smoke_tests(doc) -> [];
misc_smoke_tests(suite) -> [];
misc_smoke_tests(Config) when is_list(Config) ->
    ?line true = is_binary(erlang:system_info(info)),
    ?line true = is_binary(erlang:system_info(procs)),
    ?line true = is_binary(erlang:system_info(loaded)),
    ?line true = is_binary(erlang:system_info(dist)),
    ?line ok = try erlang:system_info({cpu_topology,erts_get_cpu_topology_error_case}), fail catch error:badarg -> ok end,
    true = lists:member(erlang:system_info(tolerant_timeofday), [enabled, disabled]),
    ?line ok.
    

heap_size(doc) -> [];
heap_size(suite) -> [];
heap_size(Config) when is_list(Config) ->
   ?line {min_bin_vheap_size, VHmin} = erlang:system_info(min_bin_vheap_size),
   ?line {min_heap_size, Hmin} =  erlang:system_info(min_heap_size),
   ?line GCinf =  erlang:system_info(garbage_collection),
   ?line VHmin = proplists:get_value(min_bin_vheap_size, GCinf),
   ?line Hmin  = proplists:get_value(min_heap_size, GCinf),
   ok.

wordsize(suite) ->
    [];
wordsize(doc) ->
    ["Tests the various wordsize variants"];
wordsize(Config) when is_list(Config) ->
    ?line A = erlang:system_info(wordsize),
    ?line true = is_integer(A),
    ?line A = erlang:system_info({wordsize,internal}),
    ?line B = erlang:system_info({wordsize,external}),
    ?line true = A =< B,
    case {B,A} of
	{4,4} ->
	    {comment, "True 32-bit emulator"};
	{8,8} ->
	    {comment, "True 64-bit emulator"};
	{8,4} ->
	    {comment, "Halfword 64-bit emulator"};
	Other ->
	    exit({unexpected_wordsizes,Other})
    end.

memory(doc) -> ["Verify that erlang:memory/0 and memory results in crashdump produce are similar"];
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
    %% Use a large heap size on the controling process in
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
			   end,
			   []),
    cmp_memory(MWs, "spawn procs"),

    Ps = lists:flatten(DPs),

    mem_workers_call(MWs, 
		     fun () ->
			     lists:foreach(fun (P) -> link(P) end, Ps)
		     end,
		     []),
    cmp_memory(MWs, "link procs"),
    mem_workers_call(MWs,
		     fun () ->
			     lists:foreach(fun (P) -> unlink(P) end, Ps)
		     end,
		     []),
    cmp_memory(MWs, "unlink procs"),

    DMs = mem_workers_call(MWs,
			   fun () ->
				   lists:map(fun (P) ->
						     monitor(process, P)
					     end, Ps)
			   end,
			   []),
    cmp_memory(MWs, "monitor procs"),
    Ms = lists:flatten(DMs),
    mem_workers_call(MWs,
		     fun () ->
			     lists:foreach(fun (M) ->
						   demonitor(M)
					   end, Ms)
		     end,
		     []),
    cmp_memory(MWs, "demonitor procs"),

    mem_workers_call(MWs,
		     fun () ->
			     lists:foreach(fun (P) ->
						   P ! {a, "message", make_ref()}
					   end, Ps)
		     end,
		     []),
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
		     end,
		     []),

    cmp_memory(MWs, "store binary data"),

    mem_workers_call(MWs,
		     fun () ->
			     put(binary_data, false),
			     garbage_collect()
		     end,
		     []),
    cmp_memory(MWs, "release binary data"),

    mem_workers_call(MWs,
		     fun () ->
			     list_to_atom("an ugly atom "++integer_to_list(erlang:system_info(scheduler_id))),
			     list_to_atom("another ugly atom "++integer_to_list(erlang:system_info(scheduler_id))),
			     list_to_atom("yet another ugly atom "++integer_to_list(erlang:system_info(scheduler_id)))
		     end,
		     []),
    cmp_memory(MWs, "new atoms"),


    mem_workers_call(MWs,
		     fun () ->
			     T = ets:new(?MODULE, []),
			     ets:insert(T, {gurka, lists:seq(1,10000)}),
			     ets:insert(T, {banan, lists:seq(1,1024)}),
			     ets:insert(T, {appelsin, make_ref()}),
			     put(ets_id, T)
		     end,
		     []),
    cmp_memory(MWs, "store ets data"),

    mem_workers_call(MWs,
		     fun () ->
			     ets:delete(get(ets_id)),
			     put(ets_id, false)
		     end,
		     []),
    cmp_memory(MWs, "remove ets data"),

    lists:foreach(fun (MW) ->
			  unlink(MW),
			  Mon = monitor(process, MW),
			  exit(MW, kill),
			  receive
			      {'DOWN', Mon, _, _, _} -> ok
			  end
		  end,
		  MWs),
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
		  end,
		  MWs),
    lists:map(fun (MW) ->
		      receive
			  {reply, MW, Res} ->
			      Res
		      end
	      end,
	      MWs).

mem_workers_cast(MWs, Fun, Args) ->
    lists:foreach(fun (MW) ->
			  MW ! {cast, self(), Fun, Args}
		  end,
		  MWs).

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

    ?line check_sane_memory(EM),
    ?line check_sane_memory(EDM),

    %% We expect these to always give us exactly the same result

    ?line cmp_memory(atom, EM, EDM, 1),
    ?line cmp_memory(atom_used, EM, EDM, 1),
    ?line cmp_memory(binary, EM, EDM, 1),
    ?line cmp_memory(code, EM, EDM, 1),
    ?line cmp_memory(ets, EM, EDM, 1),

    %% Total, processes, processes_used, and system will seldom
    %% give us exactly the same result since the two readings
    %% aren't taken atomically.

    ?line cmp_memory(total, EM, EDM, 1.05),
    ?line cmp_memory(processes, EM, EDM, 1.05),
    ?line cmp_memory(processes_used, EM, EDM, 1.05),
    ?line cmp_memory(system, EM, EDM, 1.05),

    ok.
    
mapn(_Fun, 0) ->
    [];
mapn(Fun, N) ->
    [Fun(N) | mapn(Fun, N-1)].

ets_limit(doc) ->
    "Verify system_info(ets_limit) reflects max ETS table settings.";
ets_limit(suite) -> [];
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
    {ok, Node} = start_node(Config, Envs),
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

start_node(Config, Envs) when is_list(Config) ->
    Pa = filename:dirname(code:which(?MODULE)),
    {A, B, C} = now(),
    Name = list_to_atom(atom_to_list(?MODULE)
                        ++ "-"
                        ++ atom_to_list(?config(testcase, Config))
                        ++ "-"
                        ++ integer_to_list(A)
                        ++ "-"
                        ++ integer_to_list(B)
                        ++ "-"
                        ++ integer_to_list(C)),
    ?t:start_node(Name, peer, [{args, "-pa "++Pa}, {env, Envs}]).

stop_node(Node) ->
    ?t:stop_node(Node).
