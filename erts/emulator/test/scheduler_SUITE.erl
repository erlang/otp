%% 
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
%%% File    : scheduler_SUITE.erl
%%% Author  : Rickard Green
%%% Description : 
%%%
%%% Created : 27 Oct 2008 by Rickard Green
%%%-------------------------------------------------------------------
-module(scheduler_SUITE).


%-define(line_trace, 1).

-include_lib("common_test/include/ct.hrl").

%-compile(export_all).
-export([all/0, suite/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([equal/1,
	 few_low/1,
	 many_low/1,
	 equal_with_part_time_high/1,
	 equal_with_part_time_max/1,
	 equal_and_high_with_part_time_max/1,
	 equal_with_high/1,
	 equal_with_high_max/1,
	 bound_process/1,
	
	 scheduler_bind_types/1,
	 cpu_topology/1,
	 update_cpu_info/1,
	 sct_cmd/1,
	 sbt_cmd/1,
	 scheduler_threads/1,
	 scheduler_suspend_basic/1,
	 scheduler_suspend/1,
	 dirty_scheduler_threads/1,
	 reader_groups/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 15}}].

all() -> 
    [equal, few_low, many_low, equal_with_part_time_high,
     equal_with_part_time_max,
     equal_and_high_with_part_time_max, equal_with_high,
     equal_with_high_max,
     bound_process,
     {group, scheduler_bind}, scheduler_threads,
     scheduler_suspend_basic, scheduler_suspend,
     dirty_scheduler_threads,
     reader_groups].

groups() -> 
    [{scheduler_bind, [],
      [scheduler_bind_types, cpu_topology, update_cpu_info,
       sct_cmd, sbt_cmd]}].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    catch erts_debug:set_internal_state(available_internal_state, false),
    Config.

init_per_testcase(update_cpu_info, Config) ->
    case os:find_executable("taskset") of
	false ->
	    {skip,"Could not find 'taskset' in path"};
	_ ->
	    init_per_tc(update_cpu_info, Config)
    end;
init_per_testcase(Case, Config) when is_list(Config) ->
    init_per_tc(Case, Config).

init_per_tc(Case, Config) ->
    process_flag(priority, max),
    erlang:display({'------------', ?MODULE, Case, '------------'}),
    OkRes = ok,
    [{testcase, Case}, {ok_res, OkRes} |Config].

end_per_testcase(_Case, Config) when is_list(Config) ->
    ok.

-define(ERTS_RUNQ_CHECK_BALANCE_REDS_PER_SCHED, (2000*2000)).
-define(DEFAULT_TEST_REDS_PER_SCHED, 200000000).

%%
%% Test cases
%%

equal(Config) when is_list(Config) ->
    low_normal_test(Config, 500, 500).

few_low(Config) when is_list(Config) ->
    low_normal_test(Config, 1000, 2*active_schedulers()).

many_low(Config) when is_list(Config) ->
    low_normal_test(Config, 2*active_schedulers(), 1000).

low_normal_test(Config, NW, LW) ->
    Tracer = start_tracer(),
    Low = workers(LW, low),
    Normal = workers(NW, normal),
    Res = do_it(Tracer, Low, Normal, [], []),
    chk_result(Res, LW, NW, 0, 0, true, false, false),
    workers_exit([Low, Normal]),
    ok(Res, Config).

equal_with_part_time_high(Config) when is_list(Config) ->
    NW = 500,
    LW = 500,
    HW = 1,
    Tracer = start_tracer(),
    Normal = workers(NW, normal),
    Low = workers(LW, low),
    High = part_time_workers(HW, high),
    Res = do_it(Tracer, Low, Normal, High, []),
    chk_result(Res, LW, NW, HW, 0, true, true, false),
    workers_exit([Low, Normal, High]),
    ok(Res, Config).

equal_and_high_with_part_time_max(Config) when is_list(Config) ->
    NW = 500,
    LW = 500,
    HW = 500,
    MW = 1,
    Tracer = start_tracer(),
    Low = workers(LW, low),
    Normal = workers(NW, normal),
    High = workers(HW, high),
    Max = part_time_workers(MW, max),
    Res = do_it(Tracer, Low, Normal, High, Max),
    chk_result(Res, LW, NW, HW, MW, false, true, true),
    workers_exit([Low, Normal, Max]),
    ok(Res, Config).

equal_with_part_time_max(Config) when is_list(Config) ->
    NW = 500,
    LW = 500,
    MW = 1,
    Tracer = start_tracer(),
    Low = workers(LW, low),
    Normal = workers(NW, normal),
    Max = part_time_workers(MW, max),
    Res = do_it(Tracer, Low, Normal, [], Max),
    chk_result(Res, LW, NW, 0, MW, true, false, true),
    workers_exit([Low, Normal, Max]),
    ok(Res, Config).

equal_with_high(Config) when is_list(Config) ->
    NW = 500,
    LW = 500,
    HW = 1,
    Tracer = start_tracer(),
    Low = workers(LW, low),
    Normal = workers(NW, normal),
    High = workers(HW, high),
    Res = do_it(Tracer, Low, Normal, High, []),
    LNExe = case active_schedulers() of
		      S when S =< HW -> false;
		      _ -> true
		  end,
    chk_result(Res, LW, NW, HW, 0, LNExe, true, false),
    workers_exit([Low, Normal, High]),
    ok(Res, Config).

equal_with_high_max(Config) when is_list(Config) ->
    NW = 500,
    LW = 500,
    HW = 1,
    MW = 1,
    Tracer = start_tracer(),
    Normal = workers(NW, normal),
    Low = workers(LW, low),
    High = workers(HW, high),
    Max = workers(MW, max),
    Res = do_it(Tracer, Low, Normal, High, Max),
    {LNExe, HExe} = case active_schedulers() of
			      S when S =< MW -> {false, false};
			      S when S =< (MW + HW) -> {false, true};
			      _ -> {true, true}
			  end,
    chk_result(Res, LW, NW, HW, MW, LNExe, HExe, true),
    workers_exit([Low, Normal, Max]),
    ok(Res, Config).

bound_process(Config) when is_list(Config) ->
    case erlang:system_info(run_queues) == erlang:system_info(schedulers) of
        true ->
            NStartBase = 20000,
            NStart = case {erlang:system_info(debug_compiled),
                           erlang:system_info(lock_checking)} of
                         {true, true} -> NStartBase div 100;
                         {_, true} -> NStartBase div 10;
                         _ -> NStartBase
                     end,
            MStart = 100,
            Seq = lists:seq(1, 100),
            Tester = self(),
            Procs = lists:map(
                      fun (N) when N rem 2 == 0 ->
                              spawn_opt(fun () ->
                                                bound_loop(NStart,
                                                           NStart,
                                                           MStart,
                                                           1),
                                                Tester ! {self(), done}
                                        end,
                                        [{scheduler, 1}, link]);
                          (_N) ->
                              spawn_link(fun () ->
                                                 bound_loop(NStart,
                                                            NStart,
                                                            MStart,
                                                            false),
                                                 Tester ! {self(), done}
                                         end)
                      end,
                      Seq),
            lists:foreach(fun (P) -> receive {P, done} -> ok end end,
                          Procs),
            ok;
        false ->
            {skipped, "Functionality not supported"}
    end.

bound_loop(_, 0, 0, _) ->
    ok;
bound_loop(NS, 0, M, false) ->
    bound_loop(NS, NS, M-1, false);
bound_loop(NS, N, M, false) ->
    erlang:system_info(scheduler_id),
    bound_loop(NS, N-1, M, false);
bound_loop(NS, 0, M, Sched) ->
    NewSched = (Sched rem erlang:system_info(schedulers_online)) + 1,
    Sched = process_flag(scheduler, NewSched),
    NewSched = erlang:system_info(scheduler_id),
    bound_loop(NS, NS, M-1, NewSched);
bound_loop(NS, N, M, Sched) ->
    Sched = erlang:system_info(scheduler_id),
    bound_loop(NS, N-1, M, Sched).


-define(TOPOLOGY_A_CMD,
	"+sct"
	"L0-1t0-1c0p0n0"
	":L2-3t0-1c1p0n0"
	":L4-5t0-1c0p1n0"
	":L6-7t0-1c1p1n0"
	":L8-9t0-1c0p2n1"
	":L10-11t0-1c1p2n1"
	":L12-13t0-1c0p3n1"
	":L14-15t0-1c1p3n1").

-define(TOPOLOGY_A_TERM,
	[{node,[{processor,[{core,[{thread,{logical,0}},
				   {thread,{logical,1}}]},
			    {core,[{thread,{logical,2}},
				   {thread,{logical,3}}]}]},
		{processor,[{core,[{thread,{logical,4}},
				   {thread,{logical,5}}]},
			    {core,[{thread,{logical,6}},
				   {thread,{logical,7}}]}]}]},
	 {node,[{processor,[{core,[{thread,{logical,8}},
				   {thread,{logical,9}}]},
			    {core,[{thread,{logical,10}},
				   {thread,{logical,11}}]}]},
		{processor,[{core,[{thread,{logical,12}},
				   {thread,{logical,13}}]},
			    {core,[{thread,{logical,14}},
				   {thread,{logical,15}}]}]}]}]).

-define(TOPOLOGY_B_CMD,
	"+sct"
	"L0-1t0-1c0n0p0"
	":L2-3t0-1c1n0p0"
	":L4-5t0-1c2n1p0"
	":L6-7t0-1c3n1p0"
	":L8-9t0-1c0n2p1"
	":L10-11t0-1c1n2p1"
	":L12-13t0-1c2n3p1"
	":L14-15t0-1c3n3p1").

-define(TOPOLOGY_B_TERM,
	[{processor,[{node,[{core,[{thread,{logical,0}},
				   {thread,{logical,1}}]},
			    {core,[{thread,{logical,2}},
				   {thread,{logical,3}}]}]},
		     {node,[{core,[{thread,{logical,4}},
				   {thread,{logical,5}}]},
			    {core,[{thread,{logical,6}},
				   {thread,{logical,7}}]}]}]},
	 {processor,[{node,[{core,[{thread,{logical,8}},
				   {thread,{logical,9}}]},
			    {core,[{thread,{logical,10}},
				   {thread,{logical,11}}]}]},
		     {node,[{core,[{thread,{logical,12}},
				   {thread,{logical,13}}]},
			    {core,[{thread,{logical,14}},
				   {thread,{logical,15}}]}]}]}]).

-define(TOPOLOGY_C_TERM,
	[{node,[{processor,[{core,[{thread,{logical,0}},
				   {thread,{logical,1}}]},
			    {core,[{thread,{logical,2}},
				   {thread,{logical,3}}]}]},
		{processor,[{core,[{thread,{logical,4}},
				   {thread,{logical,5}}]},
			    {core,[{thread,{logical,6}},
				   {thread,{logical,7}}]}]}]},
	 {processor,[{node,[{core,[{thread,{logical,8}},
				   {thread,{logical,9}}]},
			    {core,[{thread,{logical,10}},
				   {thread,{logical,11}}]}]},
		     {node,[{core,[{thread,{logical,12}},
				   {thread,{logical,13}}]},
			    {core,[{thread,{logical,14}},
				   {thread,{logical,15}}]}]}]},
	 {node,[{processor,[{core,[{thread,{logical,16}},
				   {thread,{logical,17}}]},
			    {core,[{thread,{logical,18}},
				   {thread,{logical,19}}]}]},
		{processor,[{core,[{thread,{logical,20}},
				   {thread,{logical,21}}]},
			    {core,[{thread,{logical,22}},
				   {thread,{logical,23}}]}]}]},
	 {processor,[{node,[{core,[{thread,{logical,24}},
				   {thread,{logical,25}}]},
			    {core,[{thread,{logical,26}},
				   {thread,{logical,27}}]}]},
		     {node,[{core,[{thread,{logical,28}},
				   {thread,{logical,29}}]},
			    {core,[{thread,{logical,30}},
				   {thread,{logical,31}}]}]}]}]).


-define(TOPOLOGY_C_CMD,
	"+sct"
	"L0-1t0-1c0p0n0"
	":L2-3t0-1c1p0n0"
	":L4-5t0-1c0p1n0"
	":L6-7t0-1c1p1n0"
	":L8-9t0-1c0n1p2"
	":L10-11t0-1c1n1p2"
	":L12-13t0-1c2n2p2"
	":L14-15t0-1c3n2p2"
	":L16-17t0-1c0p3n3"
	":L18-19t0-1c1p3n3"
	":L20-21t0-1c0p4n3"
	":L22-23t0-1c1p4n3"
	":L24-25t0-1c0n4p5"
	":L26-27t0-1c1n4p5"
	":L28-29t0-1c2n5p5"
	":L30-31t0-1c3n5p5").

-define(TOPOLOGY_D_TERM,
	[{processor,[{node,[{core,[{thread,{logical,0}},
				   {thread,{logical,1}}]},
			    {core,[{thread,{logical,2}},
				   {thread,{logical,3}}]}]},
		     {node,[{core,[{thread,{logical,4}},
				   {thread,{logical,5}}]},
			    {core,[{thread,{logical,6}},
				   {thread,{logical,7}}]}]}]},
	 {node,[{processor,[{core,[{thread,{logical,8}},
				   {thread,{logical,9}}]},
			    {core,[{thread,{logical,10}},
				   {thread,{logical,11}}]}]},
		{processor,[{core,[{thread,{logical,12}},
				   {thread,{logical,13}}]},
			    {core,[{thread,{logical,14}},
				   {thread,{logical,15}}]}]}]},
	 {processor,[{node,[{core,[{thread,{logical,16}},
				   {thread,{logical,17}}]},
			    {core,[{thread,{logical,18}},
				   {thread,{logical,19}}]}]},
		     {node,[{core,[{thread,{logical,20}},
				   {thread,{logical,21}}]},
			    {core,[{thread,{logical,22}},
				   {thread,{logical,23}}]}]}]},
	 {node,[{processor,[{core,[{thread,{logical,24}},
				   {thread,{logical,25}}]},
			    {core,[{thread,{logical,26}},
				   {thread,{logical,27}}]}]},
		{processor,[{core,[{thread,{logical,28}},
				   {thread,{logical,29}}]},
			    {core,[{thread,{logical,30}},
				   {thread,{logical,31}}]}]}]}]).

-define(TOPOLOGY_D_CMD,
	"+sct"
	"L0-1t0-1c0n0p0"
	":L2-3t0-1c1n0p0"
	":L4-5t0-1c2n1p0"
	":L6-7t0-1c3n1p0"
	":L8-9t0-1c0p1n2"
	":L10-11t0-1c1p1n2"
	":L12-13t0-1c0p2n2"
	":L14-15t0-1c1p2n2"
	":L16-17t0-1c0n3p3"
	":L18-19t0-1c1n3p3"
	":L20-21t0-1c2n4p3"
	":L22-23t0-1c3n4p3"
	":L24-25t0-1c0p4n5"
	":L26-27t0-1c1p4n5"
	":L28-29t0-1c0p5n5"
	":L30-31t0-1c1p5n5").

-define(TOPOLOGY_E_CMD,
	"+sct"
	"L0-1t0-1c0p0n0"
	":L2-3t0-1c1p0n0"
	":L4-5t0-1c2p0n0"
	":L6-7t0-1c3p0n0"
	":L8-9t0-1c0p1n1"
	":L10-11t0-1c1p1n1"
	":L12-13t0-1c2p1n1"
	":L14-15t0-1c3p1n1").

-define(TOPOLOGY_E_TERM,
	[{node,[{processor,[{core,[{thread,{logical,0}},
				   {thread,{logical,1}}]},
			    {core,[{thread,{logical,2}},
				   {thread,{logical,3}}]},
			    {core,[{thread,{logical,4}},
				   {thread,{logical,5}}]},
			    {core,[{thread,{logical,6}},
				   {thread,{logical,7}}]}]}]},
	 {node,[{processor,[{core,[{thread,{logical,8}},
				   {thread,{logical,9}}]},
			    {core,[{thread,{logical,10}},
				   {thread,{logical,11}}]},
			    {core,[{thread,{logical,12}},
				   {thread,{logical,13}}]},
			    {core,[{thread,{logical,14}},
				   {thread,{logical,15}}]}]}]}]).

-define(TOPOLOGY_F_CMD,
	"+sct"
	"L0-1t0-1c0n0p0"
	":L2-3t0-1c1n0p0"
	":L4-5t0-1c2n0p0"
	":L6-7t0-1c3n0p0"
	":L8-9t0-1c4n1p0"
	":L10-11t0-1c5n1p0"
	":L12-13t0-1c6n1p0"
	":L14-15t0-1c7n1p0"
	":L16-17t0-1c8n2p0"
	":L18-19t0-1c9n2p0"
	":L20-21t0-1c10n2p0"
	":L22-23t0-1c11n2p0"
	":L24-25t0-1c12n3p0"
	":L26-27t0-1c13n3p0"
	":L28-29t0-1c14n3p0"
	":L30-31t0-1c15n3p0").

-define(TOPOLOGY_F_TERM,
        [{processor,[{node,[{core,[{thread,{logical,0}},
                                   {thread,{logical,1}}]},
                            {core,[{thread,{logical,2}},
                                   {thread,{logical,3}}]},
                            {core,[{thread,{logical,4}},
                                   {thread,{logical,5}}]},
                            {core,[{thread,{logical,6}},
                                   {thread,{logical,7}}]}]},
                     {node,[{core,[{thread,{logical,8}},
                                   {thread,{logical,9}}]},
                            {core,[{thread,{logical,10}},
                                   {thread,{logical,11}}]},
                            {core,[{thread,{logical,12}},
                                   {thread,{logical,13}}]},
                            {core,[{thread,{logical,14}},
                                   {thread,{logical,15}}]}]},
                     {node,[{core,[{thread,{logical,16}},
                                   {thread,{logical,17}}]},
                            {core,[{thread,{logical,18}},
                                   {thread,{logical,19}}]},
                            {core,[{thread,{logical,20}},
                                   {thread,{logical,21}}]},
                            {core,[{thread,{logical,22}},
                                   {thread,{logical,23}}]}]},
                     {node,[{core,[{thread,{logical,24}},
                                   {thread,{logical,25}}]},
                            {core,[{thread,{logical,26}},
                                   {thread,{logical,27}}]},
                            {core,[{thread,{logical,28}},
                                   {thread,{logical,29}}]},
                            {core,[{thread,{logical,30}},
                                   {thread,{logical,31}}]}]}]}]).

bindings(Node, BindType) ->
    Parent = self(),
    Ref = make_ref(),
    Pid = spawn_link(Node,
                     fun () ->
                             enable_internal_state(),
                             Res = (catch erts_debug:get_internal_state(
                                            {fake_scheduler_bindings,
                                             BindType})),
                             Parent ! {Ref, Res}
                     end),
    receive
        {Ref, Res} ->
            io:format("~p: ~p~n", [BindType, Res]),
            unlink(Pid),
            Res
    end.

scheduler_bind_types(Config) when is_list(Config) ->
    OldRelFlags = clear_erl_rel_flags(),
    try
	scheduler_bind_types_test(Config,
				  ?TOPOLOGY_A_TERM,
				  ?TOPOLOGY_A_CMD,
				  a),
	scheduler_bind_types_test(Config,
				  ?TOPOLOGY_B_TERM,
				  ?TOPOLOGY_B_CMD,
				  b),
	scheduler_bind_types_test(Config,
				  ?TOPOLOGY_C_TERM,
				  ?TOPOLOGY_C_CMD,
				  c),
	scheduler_bind_types_test(Config,
				  ?TOPOLOGY_D_TERM,
				  ?TOPOLOGY_D_CMD,
				  d),
	scheduler_bind_types_test(Config,
				  ?TOPOLOGY_E_TERM,
				  ?TOPOLOGY_E_CMD,
				  e),
	scheduler_bind_types_test(Config,
				  ?TOPOLOGY_F_TERM,
				  ?TOPOLOGY_F_CMD,
				  f)
    after
	restore_erl_rel_flags(OldRelFlags)
    end,
    ok.

scheduler_bind_types_test(Config, Topology, CmdLine, TermLetter) ->
    io:format("Testing (~p): ~p~n", [TermLetter, Topology]),
    {ok, Node0} = start_node(Config),
    _ = rpc:call(Node0, erlang, system_flag, [cpu_topology, Topology]),
    cmp(Topology, rpc:call(Node0, erlang, system_info, [cpu_topology])),
    check_bind_types(Node0, TermLetter),
    stop_node(Node0),
    {ok, Node1} = start_node(Config, CmdLine),
    cmp(Topology, rpc:call(Node1, erlang, system_info, [cpu_topology])),
    check_bind_types(Node1, TermLetter),
    stop_node(Node1).

check_bind_types(Node, a) ->
    {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
    = bindings(Node, no_spread),
    {0,2,4,6,8,10,12,14,1,3,5,7,9,11,13,15}
    = bindings(Node, thread_spread),
    {0,4,8,12,2,6,10,14,1,5,9,13,3,7,11,15}
    = bindings(Node, processor_spread),
    {0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15}
    = bindings(Node, spread),
    {0,2,4,6,1,3,5,7,8,10,12,14,9,11,13,15}
    = bindings(Node, no_node_thread_spread),
    {0,4,2,6,1,5,3,7,8,12,10,14,9,13,11,15}
    = bindings(Node, no_node_processor_spread),
    {0,4,2,6,8,12,10,14,1,5,3,7,9,13,11,15}
    = bindings(Node, thread_no_node_processor_spread),
    {0,4,2,6,8,12,10,14,1,5,3,7,9,13,11,15}
    = bindings(Node, default_bind),
    ok;
check_bind_types(Node, b) ->
    {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
    = bindings(Node, no_spread),
    {0,2,4,6,8,10,12,14,1,3,5,7,9,11,13,15}
    = bindings(Node, thread_spread),
    {0,8,2,10,4,12,6,14,1,9,3,11,5,13,7,15}
    = bindings(Node, processor_spread),
    {0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15}
    = bindings(Node, spread),
    {0,2,1,3,4,6,5,7,8,10,9,11,12,14,13,15}
    = bindings(Node, no_node_thread_spread),
    {0,2,1,3,4,6,5,7,8,10,9,11,12,14,13,15}
    = bindings(Node, no_node_processor_spread),
    {0,2,4,6,8,10,12,14,1,3,5,7,9,11,13,15}
    = bindings(Node, thread_no_node_processor_spread),
    {0,2,4,6,8,10,12,14,1,3,5,7,9,11,13,15}
    = bindings(Node, default_bind),
    ok;
check_bind_types(Node, c) ->
    {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,
     25,26,27,28,29,30,31} = bindings(Node, no_spread),
    {0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,1,3,5,7,9,11,13,15,
     17,19,21,23,25,27,29,31} = bindings(Node, thread_spread),
    {0,4,8,16,20,24,2,6,10,18,22,26,12,28,14,30,1,5,9,17,21,25,
     3,7,11,19,23,27,13,29,15,31} = bindings(Node, processor_spread),
    {0,8,16,24,4,20,12,28,2,10,18,26,6,22,14,30,1,9,17,25,5,21,13,29,3,11,
     19,27,7,23,15,31} = bindings(Node, spread),
    {0,2,4,6,1,3,5,7,8,10,9,11,12,14,13,15,16,18,20,22,17,19,21,23,24,26,
     25,27,28,30,29,31} = bindings(Node, no_node_thread_spread),
    {0,4,2,6,1,5,3,7,8,10,9,11,12,14,13,15,16,20,18,22,17,21,19,23,24,26,
     25,27,28,30,29,31} = bindings(Node, no_node_processor_spread),
    {0,4,2,6,8,10,12,14,16,20,18,22,24,26,28,30,1,5,3,7,9,11,13,15,17,21,
     19,23,25,27,29,31} = bindings(Node, thread_no_node_processor_spread),
    {0,4,2,6,8,10,12,14,16,20,18,22,24,26,28,30,1,5,3,7,9,11,13,15,17,21,
     19,23,25,27,29,31} = bindings(Node, default_bind),
    ok;
check_bind_types(Node, d) ->
    {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,
     25,26,27,28,29,30,31} = bindings(Node, no_spread),
    {0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,1,3,5,7,9,11,13,15,
     17,19,21,23,25,27,29,31} = bindings(Node, thread_spread),
    {0,8,12,16,24,28,2,10,14,18,26,30,4,20,6,22,1,9,13,17,25,29,3,11,15,
     19,27,31,5,21,7,23} = bindings(Node, processor_spread),
    {0,8,16,24,12,28,4,20,2,10,18,26,14,30,6,22,1,9,17,25,13,29,5,21,3,11,
     19,27,15,31,7,23} = bindings(Node, spread),
    {0,2,1,3,4,6,5,7,8,10,12,14,9,11,13,15,16,18,17,19,20,22,21,23,24,26,
     28,30,25,27,29,31} = bindings(Node, no_node_thread_spread),
    {0,2,1,3,4,6,5,7,8,12,10,14,9,13,11,15,16,18,17,19,20,22,21,23,24,28,
     26,30,25,29,27,31} = bindings(Node, no_node_processor_spread),
    {0,2,4,6,8,12,10,14,16,18,20,22,24,28,26,30,1,3,5,7,9,13,11,15,17,19,
     21,23,25,29,27,31} = bindings(Node, thread_no_node_processor_spread),
    {0,2,4,6,8,12,10,14,16,18,20,22,24,28,26,30,1,3,5,7,9,13,11,15,17,19,
     21,23,25,29,27,31} = bindings(Node, default_bind),
    ok;
check_bind_types(Node, e) ->
    {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
    = bindings(Node, no_spread),
    {0,2,4,6,8,10,12,14,1,3,5,7,9,11,13,15}
    = bindings(Node, thread_spread),
    {0,8,2,10,4,12,6,14,1,9,3,11,5,13,7,15}
    = bindings(Node, processor_spread),
    {0,8,2,10,4,12,6,14,1,9,3,11,5,13,7,15}
    = bindings(Node, spread),
    {0,2,4,6,1,3,5,7,8,10,12,14,9,11,13,15}
    = bindings(Node, no_node_thread_spread),
    {0,2,4,6,1,3,5,7,8,10,12,14,9,11,13,15}
    = bindings(Node, no_node_processor_spread),
    {0,2,4,6,8,10,12,14,1,3,5,7,9,11,13,15}
    = bindings(Node, thread_no_node_processor_spread),
    {0,2,4,6,8,10,12,14,1,3,5,7,9,11,13,15}
    = bindings(Node, default_bind),
    ok;
check_bind_types(Node, f) ->
    {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,
     25,26,27,28,29,30,31} = bindings(Node, no_spread),
    {0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,1,3,5,7,9,11,13,15,
     17,19,21,23,25,27,29,31} = bindings(Node, thread_spread),
    {0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,1,3,5,7,9,11,13,
     15,17,19,21,23,25,27,29,31} = bindings(Node, processor_spread),
    {0,8,16,24,2,10,18,26,4,12,20,28,6,14,22,30,1,9,17,25,3,11,19,27,5,13,
     21,29,7,15,23,31} = bindings(Node, spread),
    {0,2,4,6,1,3,5,7,8,10,12,14,9,11,13,15,16,18,20,22,17,19,21,23,24,26,
     28,30,25,27,29,31} = bindings(Node, no_node_thread_spread),
    {0,2,4,6,1,3,5,7,8,10,12,14,9,11,13,15,16,18,20,22,17,19,21,23,24,26,
     28,30,25,27,29,31} = bindings(Node, no_node_processor_spread),
    {0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,1,3,5,7,9,11,13,15,17,19,
     21,23,25,27,29,31} = bindings(Node, thread_no_node_processor_spread),
    {0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,1,3,5,7,9,11,13,15,17,19,
     21,23,25,27,29,31} = bindings(Node, default_bind),
    ok;
check_bind_types(Node, _) ->
    bindings(Node, no_spread),
    bindings(Node, thread_spread),
    bindings(Node, processor_spread),
    bindings(Node, spread),
    bindings(Node, no_node_thread_spread),
    bindings(Node, no_node_processor_spread),
    bindings(Node, thread_no_node_processor_spread),
    bindings(Node, default_bind),
    ok.

cpu_topology(Config) when is_list(Config) ->
    OldRelFlags = clear_erl_rel_flags(),
    try
        cpu_topology_test(
          Config,
          [{node,[{processor,[{core,{logical,0}},
                              {core,{logical,1}}]}]},
           {processor,[{node,[{core,{logical,2}},
                              {core,{logical,3}}]}]},
           {node,[{processor,[{core,{logical,4}},
                              {core,{logical,5}}]}]},
           {processor,[{node,[{core,{logical,6}},
                              {core,{logical,7}}]}]}],
          "+sct "
          "L0-1c0-1p0n0"
          ":L2-3c0-1n1p1"
          ":L4-5c0-1p2n2"
          ":L6-7c0-1n3p3"),
        cpu_topology_test(
          Config,
          [{node,[{processor,[{core,{logical,0}},
                              {core,{logical,1}}]},
                  {processor,[{core,{logical,2}},
                              {core,{logical,3}}]}]},
           {processor,[{node,[{core,{logical,4}},
                              {core,{logical,5}}]},
                       {node,[{core,{logical,6}},
                              {core,{logical,7}}]}]},
           {node,[{processor,[{core,{logical,8}},
                              {core,{logical,9}}]},
                  {processor,[{core,{logical,10}},
                              {core,{logical,11}}]}]},
           {processor,[{node,[{core,{logical,12}},
                              {core,{logical,13}}]},
                       {node,[{core,{logical,14}},
                              {core,{logical,15}}]}]}],
          "+sct "
          "L0-1c0-1p0n0"
          ":L2-3c0-1p1n0"
          ":L4-5c0-1n1p2"
          ":L6-7c2-3n2p2"
          ":L8-9c0-1p3n3"
          ":L10-11c0-1p4n3"
          ":L12-13c0-1n4p5"
          ":L14-15c2-3n5p5"),
        cpu_topology_test(
          Config,
          [{node,[{processor,[{core,{logical,0}},
                              {core,{logical,1}}]}]},
           {processor,[{node,[{core,{logical,2}},
                              {core,{logical,3}}]}]},
           {processor,[{node,[{core,{logical,4}},
                              {core,{logical,5}}]}]},
           {node,[{processor,[{core,{logical,6}},
                              {core,{logical,7}}]}]},
           {node,[{processor,[{core,{logical,8}},
                              {core,{logical,9}}]}]},
           {processor,[{node,[{core,{logical,10}},
                              {core,{logical,11}}]}]}],
          "+sct "
          "L0-1c0-1p0n0"
          ":L2-3c0-1n1p1"
          ":L4-5c0-1n2p2"
          ":L6-7c0-1p3n3"
          ":L8-9c0-1p4n4"
          ":L10-11c0-1n5p5")
    after
        restore_erl_rel_flags(OldRelFlags)
    end,
    ok.

cpu_topology_test(Config, Topology, Cmd) ->
    io:format("Testing~n ~p~n ~p~n", [Topology, Cmd]),
    cpu_topology_bif_test(Config, Topology),
    cpu_topology_cmdline_test(Config, Topology, Cmd),
    ok.

cpu_topology_bif_test(_Config, false) ->
    ok;
cpu_topology_bif_test(Config, Topology) ->
    {ok, Node} = start_node(Config),
    _ = rpc:call(Node, erlang, system_flag, [cpu_topology, Topology]),
    cmp(Topology, rpc:call(Node, erlang, system_info, [cpu_topology])),
    stop_node(Node),
    ok.

cpu_topology_cmdline_test(_Config, _Topology, false) ->
    ok;
cpu_topology_cmdline_test(Config, Topology, Cmd) ->
    {ok, Node} = start_node(Config, Cmd),
    cmp(Topology, rpc:call(Node, erlang, system_info, [cpu_topology])),
    stop_node(Node),
    ok.

update_cpu_info(Config) when is_list(Config) ->
    OldOnline = erlang:system_info(schedulers_online),
    OldAff = get_affinity_mask(),
    io:format("START - Affinity mask: ~p - Schedulers online: ~p - Scheduler bindings: ~p~n",
		    [OldAff, OldOnline, erlang:system_info(scheduler_bindings)]),
    case {erlang:system_info(logical_processors_available), OldAff} of
	      {Avail, _} when Avail == unknown; OldAff == unknown; OldAff == 1 ->
		  %% Nothing much to test; just a smoke test
		  case erlang:system_info(update_cpu_info) of
		      unchanged -> ok;
		      changed -> ok
		  end;
	      {Avail, _} ->
		  try
		      adjust_schedulers_online(),
		      case erlang:system_info(schedulers_online) of
			  1 ->
			      %% Nothing much to test; just a smoke test
			      ok;
			  Onln0 ->
			      Cpus = bits_in_mask(OldAff),
			      RmCpus = case Cpus > Onln0 of
					   true -> Cpus - Onln0 + 1;
					   false -> Onln0 - Cpus + 1
				       end,
			      Onln1 = Cpus - RmCpus,
			      case Onln1 > 0 of
				  false ->
				      %% Nothing much to test; just a smoke test
				      ok;
				  true ->
				      Aff = restrict_affinity_mask(OldAff, RmCpus),
				      set_affinity_mask(Aff),
				      case adjust_schedulers_online() of
					  {Onln0, Onln1} ->
					      Onln1 = erlang:system_info(schedulers_online),
					      receive after 500 -> ok end,
					      io:format("TEST - Affinity mask: ~p - Schedulers online: ~p - Scheduler bindings: ~p~n",
							[Aff, Onln1, erlang:system_info(scheduler_bindings)]),
					      unchanged = adjust_schedulers_online(),
					      ok;
					  Fail ->
					      ct:fail(Fail)
				      end
			      end
		      end
		  after
		      set_affinity_mask(OldAff),
		      adjust_schedulers_online(),
		      erlang:system_flag(schedulers_online, OldOnline),
		      receive after 500 -> ok end,
		      io:format("END - Affinity mask: ~p - Schedulers online: ~p - Scheduler bindings: ~p~n",
				[get_affinity_mask(),
				 erlang:system_info(schedulers_online),
				 erlang:system_info(scheduler_bindings)])
		  end
	  end.

bits_in_mask(Mask) ->
    bits_in_mask(Mask, 0, 0).

bits_in_mask(0, Shift, N) ->
    N;
bits_in_mask(Mask, Shift, N) ->
    case Mask band (1 bsl Shift) of
	0 -> bits_in_mask(Mask, Shift+1, N);
	_ -> bits_in_mask(Mask band (bnot (1 bsl Shift)),
			  Shift+1, N+1)
    end.

restrict_affinity_mask(Mask, N) ->
    try
	restrict_affinity_mask(Mask, 0, N)
    catch
	throw : Reason ->
	    exit({Reason, Mask, N})
    end.

restrict_affinity_mask(Mask, _Shift, 0) ->
    Mask;
restrict_affinity_mask(0, _Shift, _N) ->
    throw(overresticted_affinity_mask);
restrict_affinity_mask(Mask, Shift, N) ->
    case Mask band (1 bsl Shift) of
	0 -> restrict_affinity_mask(Mask, Shift+1, N);
	_ -> restrict_affinity_mask(Mask band (bnot (1 bsl Shift)),
				    Shift+1, N-1)
    end.

adjust_schedulers_online() ->
    case erlang:system_info(update_cpu_info) of
	unchanged ->
	    unchanged;
	changed ->
	    Avail = erlang:system_info(logical_processors_available),
	    Scheds = erlang:system_info(schedulers),
	    SOnln = case Avail > Scheds of
	    	    	 true -> Scheds;
			 false -> Avail
		    end,
	    {erlang:system_flag(schedulers_online, SOnln), SOnln}
    end.

read_affinity(Data) ->
    Exp = "pid " ++ os:getpid() ++ "'s current affinity mask",
    case string:tokens(Data, ":") of
	[Exp, DirtyAffinityStr] ->
	    AffinityStr = string:strip(string:strip(DirtyAffinityStr,
						    both, $ ),
				       both, $\n),
	    case catch erlang:list_to_integer(AffinityStr, 16) of
		Affinity when is_integer(Affinity) ->
		    Affinity;
		_ ->
		    bad
	    end;
	_ ->
	    bad
    end.

get_affinity_mask(Port, Status, Affinity) when Status == unknown;
					       Affinity == unknown ->
    receive
	{Port,{data, Data}} ->
	    get_affinity_mask(Port, Status, read_affinity(Data));
	{Port,{exit_status,S}} ->
	    get_affinity_mask(Port, S, Affinity)
    end;
get_affinity_mask(_Port, _Status, bad) ->
    unknown;
get_affinity_mask(_Port, _Status, Affinity) ->
    Affinity.

get_affinity_mask() ->
    case os:type() of
	{unix, linux} ->
	    case catch open_port({spawn, "taskset -p " ++ os:getpid()},
				 [exit_status]) of
		Port when is_port(Port) ->
		    get_affinity_mask(Port, unknown, unknown);
		_ ->
		    unknown
	    end;
	_ ->
	    unknown
    end.

set_affinity_mask(Port, unknown) ->
    receive
	{Port,{data, _}} ->
	    set_affinity_mask(Port, unknown);
	{Port,{exit_status,Status}} ->
	    set_affinity_mask(Port, Status)
    end;
set_affinity_mask(Port, Status) ->
    receive
	{Port,{data, _}} ->
	    set_affinity_mask(Port, unknown)
    after 0 ->
	    Status
    end.

set_affinity_mask(Mask) ->
    Cmd = lists:flatten(["taskset -p ",
			 io_lib:format("~.16b", [Mask]),
			 " ",
			 os:getpid()]),
    case catch open_port({spawn, Cmd}, [exit_status]) of
	Port when is_port(Port) ->
	    case set_affinity_mask(Port, unknown) of
		0 -> ok;
		_ -> exit(failed_to_set_affinity)
	    end;
	_ ->
	    exit(failed_to_set_affinity)
    end.

sct_cmd(Config) when is_list(Config) ->
    Topology = ?TOPOLOGY_A_TERM,
    OldRelFlags = clear_erl_rel_flags(),
    try
	{ok, Node} = start_node(Config, ?TOPOLOGY_A_CMD),
	cmp(Topology,
		  rpc:call(Node, erlang, system_info, [cpu_topology])),
	cmp(Topology,
		  rpc:call(Node, erlang, system_flag, [cpu_topology, Topology])),
	cmp(Topology,
		  rpc:call(Node, erlang, system_info, [cpu_topology])),
	stop_node(Node)
    after
	restore_erl_rel_flags(OldRelFlags)
    end,
    ok.

-define(BIND_TYPES,
	[{"u", unbound},
	 {"ns", no_spread},
	 {"ts", thread_spread},
	 {"ps", processor_spread},
	 {"s", spread},
	 {"nnts", no_node_thread_spread},
	 {"nnps", no_node_processor_spread},
	 {"tnnps", thread_no_node_processor_spread},
	 {"db", thread_no_node_processor_spread}]).

sbt_cmd(Config) when is_list(Config) ->
    Bind = try
	      OldVal = erlang:system_flag(scheduler_bind_type, default_bind),
	      erlang:system_flag(scheduler_bind_type, OldVal),
	      go_for_it
	  catch
	      error:notsup -> notsup;
		error:_ -> go_for_it
	  end,
    case Bind of
	notsup ->
	    {skipped, "Binding of schedulers not supported"};
	go_for_it ->
	    CpuTCmd = case erlang:system_info({cpu_topology,detected}) of
			  undefined ->
			      case os:type() of
				  linux ->
				      case erlang:system_info(logical_processors) of
					  1 ->
					      "+sctL0";
					  N when is_integer(N) ->
					      NS = integer_to_list(N-1),
					      "+sctL0-"++NS++"p0-"++NS;
					  _ ->
					      false
				      end;
				  _ ->
				      false
			      end;
			  _ ->
			      ""
		      end,
	    case CpuTCmd of
		false ->
		    {skipped, "Don't know how to create cpu topology"};
		_ ->
		    case erlang:system_info(logical_processors) of
			LP when is_integer(LP) ->
			    OldRelFlags = clear_erl_rel_flags(),
			    try
				lists:foreach(fun ({ClBt, Bt}) ->
						      sbt_test(Config,
								     CpuTCmd,
								     ClBt,
								     Bt,
								     LP)
					      end,
					      ?BIND_TYPES)
			    after
				restore_erl_rel_flags(OldRelFlags)
			    end,
			    ok;
			_ ->
			    {skipped,
				   "Don't know the amount of logical processors"}
		    end
	    end
    end.

sbt_test(Config, CpuTCmd, ClBt, Bt, LP) ->
    io:format("Testing +sbt ~s (~p)~n", [ClBt, Bt]),
    LPS = integer_to_list(LP),
    Cmd = CpuTCmd++" +sbt "++ClBt++" +S"++LPS++":"++LPS,
    {ok, Node} = start_node(Config, Cmd),
    Bt = rpc:call(Node,
			erlang,
			system_info,
			[scheduler_bind_type]),
    SB = rpc:call(Node,
			erlang,
			system_info,
			[scheduler_bindings]),
    io:format("scheduler bindings: ~p~n", [SB]),
    BS = case {Bt, erlang:system_info(logical_processors_available)} of
		   {unbound, _} -> 0;
		   {_, Int} when is_integer(Int) -> Int;
		   {_, _} -> LP
		end,
    lists:foldl(fun (S, 0) ->
			      unbound = S,
			      0;
			  (S, N) ->
			      true = is_integer(S),
			      N-1
		      end,
		      BS,
		      tuple_to_list(SB)),
    stop_node(Node),
    ok.

scheduler_threads(Config) when is_list(Config) ->
    SmpSupport = erlang:system_info(smp_support),
    {Sched, SchedOnln, _} = get_sstate(Config, ""),
    %% Configure half the number of both the scheduler threads and
    %% the scheduler threads online.
    {HalfSched, HalfSchedOnln} = {lists:max([1,Sched div 2]),
                                  lists:max([1,SchedOnln div 2])},
    {HalfSched, HalfSchedOnln, _} = get_sstate(Config, "+SP 50:50"),
    %% Use +S to configure 4x the number of scheduler threads and
    %% 4x the number of scheduler threads online, but alter that
    %% setting using +SP to 50% scheduler threads and 25% scheduler
    %% threads online. The result should be 2x scheduler threads and
    %% 1x scheduler threads online.
    TwiceSched = case SmpSupport of
                     false -> 1;
                     true -> Sched*2
                 end,
    FourSched = integer_to_list(Sched*4),
    FourSchedOnln = integer_to_list(SchedOnln*4),
    CombinedCmd1 = "+S "++FourSched++":"++FourSchedOnln++" +SP50:25",
    {TwiceSched, SchedOnln, _} = get_sstate(Config, CombinedCmd1),
    %% Now do the same test but with the +S and +SP options in the
    %% opposite order, since order shouldn't matter.
    CombinedCmd2 = "+SP50:25 +S "++FourSched++":"++FourSchedOnln,
    {TwiceSched, SchedOnln, _} = get_sstate(Config, CombinedCmd2),
    %% Apply two +SP options to make sure the second overrides the first
    TwoCmd = "+SP 25:25 +SP 100:100",
    {Sched, SchedOnln, _} = get_sstate(Config, TwoCmd),
    %% Configure 50% of scheduler threads online only
    {Sched, HalfSchedOnln, _} = get_sstate(Config, "+SP:50"),
    %% Configure 2x scheduler threads only
    {TwiceSched, SchedOnln, _} = get_sstate(Config, "+SP 200"),
    case {erlang:system_info(logical_processors),
	  erlang:system_info(logical_processors_available)} of
	{LProc, LProcAvail} when is_integer(LProc), is_integer(LProcAvail) ->
	    %% Test resetting the scheduler counts
	    ResetCmd = "+S "++FourSched++":"++FourSchedOnln++" +S 0:0",
	    {LProc, LProcAvail, _} = get_sstate(Config, ResetCmd),
	    %% Test negative +S settings, but only for SMP-enabled emulators
	    case {SmpSupport, LProc > 1, LProcAvail > 1} of
		{true, true, true} ->
		    SchedMinus1 = LProc-1,
		    SchedOnlnMinus1 = LProcAvail-1,
		    {SchedMinus1, SchedOnlnMinus1, _} = get_sstate(Config, "+S -1"),
		    {LProc, SchedOnlnMinus1, _} = get_sstate(Config, "+S :-1"),
		    {SchedMinus1, SchedOnlnMinus1, _} = get_sstate(Config, "+S -1:-1"),
		    ok;
		_ ->
		    {comment, "Skipped reduced amount of schedulers test due to too few logical processors"}
	    end;
	_ -> %% Skipped when missing info about logical processors...
	    {comment, "Skipped reset amount of schedulers test, and reduced amount of schedulers test due to too unknown amount of logical processors"}
    end.

dirty_scheduler_threads(Config) when is_list(Config) ->
    case erlang:system_info(dirty_cpu_schedulers) of
        0 -> {skipped, "No dirty scheduler support"};
        _ -> dirty_scheduler_threads_test(Config)
    end.

dirty_scheduler_threads_test(Config) ->
    SmpSupport = erlang:system_info(smp_support),
    {Sched, SchedOnln, _} = get_dsstate(Config, ""),
    {HalfSched, HalfSchedOnln} = {lists:max([1,Sched div 2]),
                                  lists:max([1,SchedOnln div 2])},
    Cmd1 = "+SDcpu "++integer_to_list(HalfSched)++":"++
	integer_to_list(HalfSchedOnln),
    {HalfSched, HalfSchedOnln, _} = get_dsstate(Config, Cmd1),
    {HalfSched, HalfSchedOnln, _} = get_dsstate(Config, "+SDPcpu 50:50"),
    IOSched = 20,
    {_, _, IOSched} = get_dsstate(Config, "+SDio "++integer_to_list(IOSched)),
    {ok, Node} = start_node(Config, ""),
    [ok] = mcall(Node, [fun() -> dirty_schedulers_online_test() end]),
    ok.

dirty_schedulers_online_test() ->
    dirty_schedulers_online_test(erlang:system_info(smp_support)).
dirty_schedulers_online_test(false) -> ok;
dirty_schedulers_online_test(true) ->
    dirty_schedulers_online_smp_test(erlang:system_info(schedulers_online)).
dirty_schedulers_online_smp_test(SchedOnln) when SchedOnln < 4 -> ok;
dirty_schedulers_online_smp_test(SchedOnln) ->
    receive after 500 -> ok end,
    DirtyCPUSchedOnln = erlang:system_info(dirty_cpu_schedulers_online),
    SchedOnln = DirtyCPUSchedOnln,
    HalfSchedOnln = SchedOnln div 2,
    SchedOnln = erlang:system_flag(schedulers_online, HalfSchedOnln),
    HalfDirtyCPUSchedOnln = DirtyCPUSchedOnln div 2,
    HalfDirtyCPUSchedOnln = erlang:system_flag(schedulers_online, SchedOnln),
    DirtyCPUSchedOnln = erlang:system_flag(dirty_cpu_schedulers_online,
					   HalfDirtyCPUSchedOnln),
    receive after 500 -> ok end,
    HalfDirtyCPUSchedOnln = erlang:system_info(dirty_cpu_schedulers_online),
    QrtrDirtyCPUSchedOnln = HalfDirtyCPUSchedOnln div 2,
    SchedOnln = erlang:system_flag(schedulers_online, HalfSchedOnln),
    receive after 500 -> ok end,
    QrtrDirtyCPUSchedOnln = erlang:system_info(dirty_cpu_schedulers_online),
    ok.

get_sstate(Config, Cmd) ->
    {ok, Node} = start_node(Config, Cmd),
    [SState] = mcall(Node, [fun () ->
                                    erlang:system_info(schedulers_state)
                            end]),
    stop_node(Node),
    SState.

get_dsstate(Config, Cmd) ->
    {ok, Node} = start_node(Config, Cmd),
    [DSCPU] = mcall(Node, [fun () ->
				   erlang:system_info(dirty_cpu_schedulers)
			   end]),
    [DSCPUOnln] = mcall(Node, [fun () ->
				       erlang:system_info(dirty_cpu_schedulers_online)
			       end]),
    [DSIO] = mcall(Node, [fun () ->
				  erlang:system_info(dirty_io_schedulers)
			  end]),
    stop_node(Node),
    {DSCPU, DSCPUOnln, DSIO}.

scheduler_suspend_basic(Config) when is_list(Config) ->
    case erlang:system_info(multi_scheduling) of
	disabled ->
	    {skip, "Nothing to test"};
	_ ->
	    Onln = erlang:system_info(schedulers_online),
	    try
		scheduler_suspend_basic_test()
	    after
		erlang:system_flag(schedulers_online, Onln)
	    end
    end.

scheduler_suspend_basic_test() ->
    %% The receives after setting scheduler states are there
    %% since the operation is not fully synchronous. For example,
    %% we do not wait for dirty cpu schedulers online to complete
    %% before returning from erlang:system_flag(schedulers_online, _).

    erlang:system_flag(schedulers_online,
		       erlang:system_info(schedulers)),
    try
	erlang:system_flag(dirty_cpu_schedulers_online,
			   erlang:system_info(dirty_cpu_schedulers)),
	receive after 500 -> ok end
    catch
	_ : _ ->
	    ok
    end,

    S0 = sched_state(),
    io:format("~p~n", [S0]),
    {{normal,NTot0,NOnln0,NAct0},
     {dirty_cpu,DCTot0,DCOnln0,DCAct0},
     {dirty_io,DITot0,DIOnln0,DIAct0}} = S0,
    enabled = erlang:system_info(multi_scheduling),

    DCOne = case DCTot0 of
		0 -> 0;
		_ -> 1
	    end,

    blocked_normal = erlang:system_flag(multi_scheduling, block_normal),
    blocked_normal = erlang:system_info(multi_scheduling),
    {{normal,NTot0,NOnln0,1},
     {dirty_cpu,DCTot0,DCOnln0,DCAct0},
     {dirty_io,DITot0,DIOnln0,DIAct0}} = sched_state(),

    NOnln0 = erlang:system_flag(schedulers_online, 1),
    receive after 500 -> ok end,
    {{normal,NTot0,1,1},
     {dirty_cpu,DCTot0,DCOne,DCOne},
     {dirty_io,DITot0,DIOnln0,DIAct0}} = sched_state(),

    1 = erlang:system_flag(schedulers_online, NOnln0),
    receive after 500 -> ok end,
    {{normal,NTot0,NOnln0,1},
     {dirty_cpu,DCTot0,DCOnln0,DCAct0},
     {dirty_io,DITot0,DIOnln0,DIAct0}} = sched_state(),

    blocked = erlang:system_flag(multi_scheduling, block),
    blocked = erlang:system_info(multi_scheduling),
    receive after 500 -> ok end,
    {{normal,NTot0,NOnln0,1},
     {dirty_cpu,DCTot0,DCOnln0,0},
     {dirty_io,DITot0,DIOnln0,0}} = sched_state(),

    NOnln0 = erlang:system_flag(schedulers_online, 1),
    receive after 500 -> ok end,
    {{normal,NTot0,1,1},
     {dirty_cpu,DCTot0,DCOne,0},
     {dirty_io,DITot0,DIOnln0,0}} = sched_state(),

    1 = erlang:system_flag(schedulers_online, NOnln0),
    receive after 500 -> ok end,
    {{normal,NTot0,NOnln0,1},
     {dirty_cpu,DCTot0,DCOnln0,0},
     {dirty_io,DITot0,DIOnln0,0}} = sched_state(),

    blocked = erlang:system_flag(multi_scheduling, unblock_normal),
    blocked = erlang:system_info(multi_scheduling),
    {{normal,NTot0,NOnln0,1},
     {dirty_cpu,DCTot0,DCOnln0,0},
     {dirty_io,DITot0,DIOnln0,0}} = sched_state(),

    enabled = erlang:system_flag(multi_scheduling, unblock),
    enabled = erlang:system_info(multi_scheduling),
    receive after 500 -> ok end,
    {{normal,NTot0,NOnln0,NAct0},
     {dirty_cpu,DCTot0,DCOnln0,DCAct0},
     {dirty_io,DITot0,DIOnln0,DIAct0}} = sched_state(),

    NOnln0 = erlang:system_flag(schedulers_online, 1),
    receive after 500 -> ok end,
    {{normal,NTot0,1,1},
     {dirty_cpu,DCTot0,DCOne,DCOne},
     {dirty_io,DITot0,DIOnln0,DIAct0}} = sched_state(),

    1 = erlang:system_flag(schedulers_online, NOnln0),
    receive after 500 -> ok end,
    {{normal,NTot0,NOnln0,NAct0},
     {dirty_cpu,DCTot0,DCOnln0,DCAct0},
     {dirty_io,DITot0,DIOnln0,DIAct0}} = sched_state(),

    ok.
    

scheduler_suspend(Config) when is_list(Config) ->
    ct:timetrap({minutes, 5}),
    lists:foreach(fun (S) -> scheduler_suspend_test(Config, S) end,
			[64, 32, 16, default]),
    ok.
scheduler_suspend_test(Config, Schedulers) ->
    Cmd = case Schedulers of
		    default ->
			"";
		    _ ->
			S = integer_to_list(Schedulers),
			"+S"++S++":"++S
		end,
    {ok, Node} = start_node(Config, Cmd),
    [SState] = mcall(Node, [fun () ->
                                    erlang:system_info(schedulers_state)
                            end]),

    io:format("SState=~p~n", [SState]),
    {Sched, SchedOnln, _SchedAvail} = SState,
    true = is_integer(Sched),
    [ok] = mcall(Node, [fun () -> sst0_loop(300) end]),
    [ok] = mcall(Node, [fun () -> sst1_loop(300) end]),
    [ok] = mcall(Node, [fun () -> sst2_loop(300) end]),
    [ok] = mcall(Node, [fun () -> sst4_loop(300) end]),
    [ok] = mcall(Node, [fun () -> sst5_loop(300) end]),
    [ok, ok, ok, ok,
     ok, ok, ok] = mcall(Node,
                         [fun () -> sst0_loop(200) end,
                          fun () -> sst1_loop(200) end,
                          fun () -> sst2_loop(200) end,
                          fun () -> sst2_loop(200) end,
                          fun () -> sst3_loop(Sched, 200) end,
                          fun () -> sst4_loop(200) end,
                          fun () -> sst5_loop(200) end]),
    [SState] = mcall(Node, [fun () ->
                                    case Sched == SchedOnln of
                                        false ->
                                            Sched = erlang:system_flag(
                                                      schedulers_online,
                                                      SchedOnln);
                                        true ->
                                            ok
                                    end,
                                    until(fun () ->
						  {_A, B, C} = erlang:system_info(
								 schedulers_state),
                                              B == C
                                          end,
                                          erlang:monotonic_time()
                                          + erlang:convert_time_unit(1,
								     seconds,
								     native)),
				    erlang:system_info(schedulers_state)
                            end]),
    stop_node(Node),
    ok.

until(Pred, MaxTime) ->
    case Pred() of
	true ->
	    true;
	false ->
	    case erlang:monotonic_time() > MaxTime of
		true ->
		    false;
		false ->
		    receive after 100 -> ok end,
		    until(Pred, MaxTime)
	    end
    end.

sst0_loop(0) ->
    ok;
sst0_loop(N) ->
    erlang:system_flag(multi_scheduling, block),
    erlang:system_flag(multi_scheduling, unblock),
    erlang:yield(),
    sst0_loop(N-1).

sst1_loop(0) ->
    ok;
sst1_loop(N) ->
    erlang:system_flag(multi_scheduling, block),
    erlang:system_flag(multi_scheduling, unblock),
    sst1_loop(N-1).

sst2_loop(0) ->
    ok;
sst2_loop(N) ->
    erlang:system_flag(multi_scheduling, block),
    erlang:system_flag(multi_scheduling, block),
    erlang:system_flag(multi_scheduling, block),
    erlang:system_flag(multi_scheduling, unblock),
    erlang:system_flag(multi_scheduling, unblock),
    erlang:system_flag(multi_scheduling, unblock),
    sst2_loop(N-1).

sst3_loop(S, N) ->
    case erlang:system_info(dirty_cpu_schedulers) of
        0 -> sst3_loop_normal_schedulers_only(S, N);
	DS -> sst3_loop_with_dirty_schedulers(S, DS, N)
    end.

sst3_loop_normal_schedulers_only(_S, 0) ->
    ok;
sst3_loop_normal_schedulers_only(S, N) ->
    erlang:system_flag(schedulers_online, (S div 2)+1),
    erlang:system_flag(schedulers_online, 1),
    erlang:system_flag(schedulers_online, (S div 2)+1),
    erlang:system_flag(schedulers_online, S),
    erlang:system_flag(schedulers_online, 1),
    erlang:system_flag(schedulers_online, S),
    sst3_loop_normal_schedulers_only(S, N-1).

sst3_loop_with_dirty_schedulers(_S, _DS, 0) ->
    ok;
sst3_loop_with_dirty_schedulers(S, DS, N) ->
    erlang:system_flag(schedulers_online, (S div 2)+1),
    erlang:system_flag(dirty_cpu_schedulers_online, (DS div 2)+1),
    erlang:system_flag(schedulers_online, 1),
    erlang:system_flag(schedulers_online, (S div 2)+1),
    erlang:system_flag(dirty_cpu_schedulers_online, 1),
    erlang:system_flag(schedulers_online, S),
    erlang:system_flag(dirty_cpu_schedulers_online, DS),
    erlang:system_flag(schedulers_online, 1),
    erlang:system_flag(schedulers_online, S),
    erlang:system_flag(dirty_cpu_schedulers_online, DS),
    sst3_loop_with_dirty_schedulers(S, DS, N-1).

sst4_loop(0) ->
    ok;
sst4_loop(N) ->
    erlang:system_flag(multi_scheduling, block_normal),
    erlang:system_flag(multi_scheduling, unblock_normal),
    sst4_loop(N-1).

sst5_loop(0) ->
    ok;
sst5_loop(N) ->
    erlang:system_flag(multi_scheduling, block_normal),
    erlang:system_flag(multi_scheduling, unblock_normal),
    sst5_loop(N-1).

reader_groups(Config) when is_list(Config) ->
    %% White box testing. These results are correct, but other results
    %% could be too...

    %% The actual tilepro64 topology
    CPUT0 = [{processor,[{node,[{core,{logical,0}},
                                {core,{logical,1}},
                                {core,{logical,2}},
                                {core,{logical,8}},
                                {core,{logical,9}},
                                {core,{logical,10}},
                                {core,{logical,11}},
                                {core,{logical,16}},
                                {core,{logical,17}},
                                {core,{logical,18}},
                                {core,{logical,19}},
                                {core,{logical,24}},
                                {core,{logical,25}},
                                {core,{logical,27}},
                                {core,{logical,29}}]},
                         {node,[{core,{logical,3}},
                                {core,{logical,4}},
                                {core,{logical,5}},
                                {core,{logical,6}},
                                {core,{logical,7}},
                                {core,{logical,12}},
                                {core,{logical,13}},
                                {core,{logical,14}},
                                {core,{logical,15}},
                                {core,{logical,20}},
                                {core,{logical,21}},
                                {core,{logical,22}},
                                {core,{logical,23}},
                                {core,{logical,28}},
                                {core,{logical,30}}]},
                         {node,[{core,{logical,31}},
                                {core,{logical,36}},
                                {core,{logical,37}},
                                {core,{logical,38}},
                                {core,{logical,44}},
                                {core,{logical,45}},
                                {core,{logical,46}},
                                {core,{logical,47}},
                                {core,{logical,51}},
                                {core,{logical,52}},
                                {core,{logical,53}},
                                {core,{logical,54}},
                                {core,{logical,55}},
                                {core,{logical,60}},
                                {core,{logical,61}}]},
                         {node,[{core,{logical,26}},
                                {core,{logical,32}},
                                {core,{logical,33}},
                                {core,{logical,34}},
                                {core,{logical,35}},
                                {core,{logical,39}},
                                {core,{logical,40}},
                                {core,{logical,41}},
                                {core,{logical,42}},
                                {core,{logical,43}},
                                {core,{logical,48}},
                                {core,{logical,49}},
                                {core,{logical,50}},
                                {core,{logical,58}}]}]}],

    [{0,1},{1,1},{2,1},{3,3},{4,3},{5,3},{6,3},{7,3},{8,1},{9,1},{10,1},
     {11,1},{12,3},{13,3},{14,4},{15,4},{16,2},{17,2},{18,2},{19,2},
     {20,4},{21,4},{22,4},{23,4},{24,2},{25,2},{26,7},{27,2},{28,4},
     {29,2},{30,4},{31,5},{32,7},{33,7},{34,7},{35,7},{36,5},{37,5},
     {38,5},{39,7},{40,7},{41,8},{42,8},{43,8},{44,5},{45,5},{46,5},
     {47,6},{48,8},{49,8},{50,8},{51,6},{52,6},{53,6},{54,6},{55,6},
     {58,8},{60,6},{61,6}]
    = reader_groups_map(CPUT0, 8),

    CPUT1 = [n([p([c([t(l(0)),t(l(1)),t(l(2)),t(l(3))]),
                   c([t(l(4)),t(l(5)),t(l(6)),t(l(7))]),
                   c([t(l(8)),t(l(9)),t(l(10)),t(l(11))]),
                   c([t(l(12)),t(l(13)),t(l(14)),t(l(15))])]),
                p([c([t(l(16)),t(l(17)),t(l(18)),t(l(19))]),
                   c([t(l(20)),t(l(21)),t(l(22)),t(l(23))]),
                   c([t(l(24)),t(l(25)),t(l(26)),t(l(27))]),
                   c([t(l(28)),t(l(29)),t(l(30)),t(l(31))])])]),
             n([p([c([t(l(32)),t(l(33)),t(l(34)),t(l(35))]),
                   c([t(l(36)),t(l(37)),t(l(38)),t(l(39))]),
                   c([t(l(40)),t(l(41)),t(l(42)),t(l(43))]),
                   c([t(l(44)),t(l(45)),t(l(46)),t(l(47))])]),
                p([c([t(l(48)),t(l(49)),t(l(50)),t(l(51))]),
                   c([t(l(52)),t(l(53)),t(l(54)),t(l(55))]),
                   c([t(l(56)),t(l(57)),t(l(58)),t(l(59))]),
                   c([t(l(60)),t(l(61)),t(l(62)),t(l(63))])])]),
             n([p([c([t(l(64)),t(l(65)),t(l(66)),t(l(67))]),
                   c([t(l(68)),t(l(69)),t(l(70)),t(l(71))]),
                   c([t(l(72)),t(l(73)),t(l(74)),t(l(75))]),
                   c([t(l(76)),t(l(77)),t(l(78)),t(l(79))])]),
                p([c([t(l(80)),t(l(81)),t(l(82)),t(l(83))]),
                   c([t(l(84)),t(l(85)),t(l(86)),t(l(87))]),
                   c([t(l(88)),t(l(89)),t(l(90)),t(l(91))]),
                   c([t(l(92)),t(l(93)),t(l(94)),t(l(95))])])]),
             n([p([c([t(l(96)),t(l(97)),t(l(98)),t(l(99))]),
                   c([t(l(100)),t(l(101)),t(l(102)),t(l(103))]),
                   c([t(l(104)),t(l(105)),t(l(106)),t(l(107))]),
                   c([t(l(108)),t(l(109)),t(l(110)),t(l(111))])]),
                p([c([t(l(112)),t(l(113)),t(l(114)),t(l(115))]),
                   c([t(l(116)),t(l(117)),t(l(118)),t(l(119))]),
                   c([t(l(120)),t(l(121)),t(l(122)),t(l(123))]),
                   c([t(l(124)),t(l(125)),t(l(126)),t(l(127))])])])],

    [{0,1},{1,1},{2,1},{3,1},{4,2},{5,2},{6,2},{7,2},{8,3},{9,3},
     {10,3},{11,3},{12,4},{13,4},{14,4},{15,4},{16,5},{17,5},{18,5},
     {19,5},{20,6},{21,6},{22,6},{23,6},{24,7},{25,7},{26,7},{27,7},
     {28,8},{29,8},{30,8},{31,8},{32,9},{33,9},{34,9},{35,9},{36,10},
     {37,10},{38,10},{39,10},{40,11},{41,11},{42,11},{43,11},{44,12},
     {45,12},{46,12},{47,12},{48,13},{49,13},{50,13},{51,13},{52,14},
     {53,14},{54,14},{55,14},{56,15},{57,15},{58,15},{59,15},{60,16},
     {61,16},{62,16},{63,16},{64,17},{65,17},{66,17},{67,17},{68,18},
     {69,18},{70,18},{71,18},{72,19},{73,19},{74,19},{75,19},{76,20},
     {77,20},{78,20},{79,20},{80,21},{81,21},{82,21},{83,21},{84,22},
     {85,22},{86,22},{87,22},{88,23},{89,23},{90,23},{91,23},{92,24},
     {93,24},{94,24},{95,24},{96,25},{97,25},{98,25},{99,25},{100,26},
     {101,26},{102,26},{103,26},{104,27},{105,27},{106,27},{107,27},
     {108,28},{109,28},{110,28},{111,28},{112,29},{113,29},{114,29},
     {115,29},{116,30},{117,30},{118,30},{119,30},{120,31},{121,31},
     {122,31},{123,31},{124,32},{125,32},{126,32},{127,32}]
    = reader_groups_map(CPUT1, 128),

    [{0,1},{1,1},{2,1},{3,1},{4,1},{5,1},{6,1},{7,1},{8,1},{9,1},{10,1},
     {11,1},{12,1},{13,1},{14,1},{15,1},{16,1},{17,1},{18,1},{19,1},
     {20,1},{21,1},{22,1},{23,1},{24,1},{25,1},{26,1},{27,1},{28,1},
     {29,1},{30,1},{31,1},{32,1},{33,1},{34,1},{35,1},{36,1},{37,1},
     {38,1},{39,1},{40,1},{41,1},{42,1},{43,1},{44,1},{45,1},{46,1},
     {47,1},{48,1},{49,1},{50,1},{51,1},{52,1},{53,1},{54,1},{55,1},
     {56,1},{57,1},{58,1},{59,1},{60,1},{61,1},{62,1},{63,1},{64,2},
     {65,2},{66,2},{67,2},{68,2},{69,2},{70,2},{71,2},{72,2},{73,2},
     {74,2},{75,2},{76,2},{77,2},{78,2},{79,2},{80,2},{81,2},{82,2},
     {83,2},{84,2},{85,2},{86,2},{87,2},{88,2},{89,2},{90,2},{91,2},
     {92,2},{93,2},{94,2},{95,2},{96,2},{97,2},{98,2},{99,2},{100,2},
     {101,2},{102,2},{103,2},{104,2},{105,2},{106,2},{107,2},{108,2},
     {109,2},{110,2},{111,2},{112,2},{113,2},{114,2},{115,2},{116,2},
     {117,2},{118,2},{119,2},{120,2},{121,2},{122,2},{123,2},{124,2},
     {125,2},{126,2},{127,2}]
    = reader_groups_map(CPUT1, 2),

    [{0,1},{1,1},{2,1},{3,1},{4,2},{5,2},{6,2},{7,2},{8,3},{9,3},{10,3},
     {11,3},{12,3},{13,3},{14,3},{15,3},{16,4},{17,4},{18,4},{19,4},
     {20,4},{21,4},{22,4},{23,4},{24,5},{25,5},{26,5},{27,5},{28,5},
     {29,5},{30,5},{31,5},{32,6},{33,6},{34,6},{35,6},{36,6},{37,6},
     {38,6},{39,6},{40,7},{41,7},{42,7},{43,7},{44,7},{45,7},{46,7},
     {47,7},{48,8},{49,8},{50,8},{51,8},{52,8},{53,8},{54,8},{55,8},
     {56,9},{57,9},{58,9},{59,9},{60,9},{61,9},{62,9},{63,9},{64,10},
     {65,10},{66,10},{67,10},{68,10},{69,10},{70,10},{71,10},{72,11},
     {73,11},{74,11},{75,11},{76,11},{77,11},{78,11},{79,11},{80,12},
     {81,12},{82,12},{83,12},{84,12},{85,12},{86,12},{87,12},{88,13},
     {89,13},{90,13},{91,13},{92,13},{93,13},{94,13},{95,13},{96,14},
     {97,14},{98,14},{99,14},{100,14},{101,14},{102,14},{103,14},
     {104,15},{105,15},{106,15},{107,15},{108,15},{109,15},{110,15},
     {111,15},{112,16},{113,16},{114,16},{115,16},{116,16},{117,16},
     {118,16},{119,16},{120,17},{121,17},{122,17},{123,17},{124,17},
     {125,17},{126,17},{127,17}]
    = reader_groups_map(CPUT1, 17),

    [{0,1},{1,1},{2,1},{3,1},{4,1},{5,1},{6,1},{7,1},{8,1},{9,1},{10,1},
     {11,1},{12,1},{13,1},{14,1},{15,1},{16,2},{17,2},{18,2},{19,2},
     {20,2},{21,2},{22,2},{23,2},{24,2},{25,2},{26,2},{27,2},{28,2},
     {29,2},{30,2},{31,2},{32,3},{33,3},{34,3},{35,3},{36,3},{37,3},
     {38,3},{39,3},{40,3},{41,3},{42,3},{43,3},{44,3},{45,3},{46,3},
     {47,3},{48,4},{49,4},{50,4},{51,4},{52,4},{53,4},{54,4},{55,4},
     {56,4},{57,4},{58,4},{59,4},{60,4},{61,4},{62,4},{63,4},{64,5},
     {65,5},{66,5},{67,5},{68,5},{69,5},{70,5},{71,5},{72,5},{73,5},
     {74,5},{75,5},{76,5},{77,5},{78,5},{79,5},{80,6},{81,6},{82,6},
     {83,6},{84,6},{85,6},{86,6},{87,6},{88,6},{89,6},{90,6},{91,6},
     {92,6},{93,6},{94,6},{95,6},{96,7},{97,7},{98,7},{99,7},{100,7},
     {101,7},{102,7},{103,7},{104,7},{105,7},{106,7},{107,7},{108,7},
     {109,7},{110,7},{111,7},{112,7},{113,7},{114,7},{115,7},{116,7},
     {117,7},{118,7},{119,7},{120,7},{121,7},{122,7},{123,7},{124,7},
     {125,7},{126,7},{127,7}]
    = reader_groups_map(CPUT1, 7),

    CPUT2 = [p([c(l(0)),c(l(1)),c(l(2)),c(l(3)),c(l(4))]),
             p([t(l(5)),t(l(6)),t(l(7)),t(l(8)),t(l(9))]),
             p([t(l(10))]),
             p([c(l(11)),c(l(12)),c(l(13))]),
             p([c(l(14)),c(l(15))])],

    [{0,1},{1,1},{2,1},{3,1},{4,1},
     {5,2},{6,2},{7,2},{8,2},{9,2},
     {10,3},
     {11,4},{12,4},{13,4},
     {14,5},{15,5}] = reader_groups_map(CPUT2, 5),


    [{0,1},{1,1},{2,2},{3,2},{4,2},
     {5,3},{6,3},{7,3},{8,3},{9,3},
     {10,4},
     {11,5},{12,5},{13,5},
     {14,6},{15,6}] = reader_groups_map(CPUT2, 6),

    [{0,1},{1,1},{2,2},{3,2},{4,2},
     {5,3},{6,3},{7,3},{8,3},{9,3},
     {10,4},
     {11,5},{12,6},{13,6},
     {14,7},{15,7}] = reader_groups_map(CPUT2, 7),

    [{0,1},{1,1},{2,2},{3,2},{4,2},
     {5,3},{6,3},{7,3},{8,3},{9,3},
     {10,4},
     {11,5},{12,6},{13,6},
     {14,7},{15,8}] = reader_groups_map(CPUT2, 8),

    [{0,1},{1,2},{2,2},{3,3},{4,3},
     {5,4},{6,4},{7,4},{8,4},{9,4},
     {10,5},
     {11,6},{12,7},{13,7},
     {14,8},{15,9}] = reader_groups_map(CPUT2, 9),

    [{0,1},{1,2},{2,2},{3,3},{4,3},
     {5,4},{6,4},{7,4},{8,4},{9,4},
     {10,5},
     {11,6},{12,7},{13,8},
     {14,9},{15,10}] = reader_groups_map(CPUT2, 10),

    [{0,1},{1,2},{2,3},{3,4},{4,4},
     {5,5},{6,5},{7,5},{8,5},{9,5},
     {10,6},
     {11,7},{12,8},{13,9},
     {14,10},{15,11}] = reader_groups_map(CPUT2, 11),

    [{0,1},{1,2},{2,3},{3,4},{4,5},
     {5,6},{6,6},{7,6},{8,6},{9,6},
     {10,7},
     {11,8},{12,9},{13,10},
     {14,11},{15,12}] = reader_groups_map(CPUT2, 100),

    CPUT3 = [p([t(l(5)),t(l(6)),t(l(7)),t(l(8)),t(l(9))]),
             p([t(l(10))]),
             p([c(l(11)),c(l(12)),c(l(13))]),
             p([c(l(14)),c(l(15))]),
             p([c(l(0)),c(l(1)),c(l(2)),c(l(3)),c(l(4))])],

    [{0,5},{1,5},{2,6},{3,6},{4,6},
     {5,1},{6,1},{7,1},{8,1},{9,1},
     {10,2},{11,3},{12,3},{13,3},
     {14,4},{15,4}] = reader_groups_map(CPUT3, 6),

    CPUT4 = [p([t(l(0)),t(l(1)),t(l(2)),t(l(3)),t(l(4))]),
             p([t(l(5))]),
             p([c(l(6)),c(l(7)),c(l(8))]),
             p([c(l(9)),c(l(10))]),
             p([c(l(11)),c(l(12)),c(l(13)),c(l(14)),c(l(15))])],

    [{0,1},{1,1},{2,1},{3,1},{4,1},
     {5,2},
     {6,3},{7,3},{8,3},
     {9,4},{10,4},
     {11,5},{12,5},{13,6},{14,6},{15,6}] = reader_groups_map(CPUT4, 6),

    [{0,1},{1,1},{2,1},{3,1},{4,1},
     {5,2},
     {6,3},{7,4},{8,4},
     {9,5},{10,5},
     {11,6},{12,6},{13,7},{14,7},{15,7}] = reader_groups_map(CPUT4, 7),

    [{0,1},{65535,2}] = reader_groups_map([c(l(0)),c(l(65535))], 10),
    ok.
    

reader_groups_map(CPUT, Groups) ->
    Old = erlang:system_info({cpu_topology, defined}),
    erlang:system_flag(cpu_topology, CPUT),
    enable_internal_state(),
    Res = erts_debug:get_internal_state({reader_groups_map, Groups}),
    erlang:system_flag(cpu_topology, Old),
    lists:sort(Res).

%%
%% Utils
%%

sched_state() ->
    sched_state(erlang:system_info(all_schedulers_state),
		undefined,
		{dirty_cpu,0,0,0},
		{dirty_io,0,0,0}).

	    
sched_state([], N, DC, DI) ->
    try
	chk_basic(N),
	chk_basic(DC),
	chk_basic(DI),
	{N, DC, DI}
    catch
	_ : _ ->
	    ct:fail({inconsisten_scheduler_state, {N, DC, DI}})
    end;
sched_state([{normal, _, _, _} = S | Rest], _S, DC, DI) ->
    sched_state(Rest, S, DC, DI);
sched_state([{dirty_cpu, _, _, _} = DC | Rest], S, _DC, DI) ->
    sched_state(Rest, S, DC, DI);
sched_state([{dirty_io, _, _, _} = DI | Rest], S, DC, _DI) ->
    sched_state(Rest, S, DC, DI).

chk_basic({_Type, Tot, Onln, Act}) ->
    true = Tot >= Onln,
    true = Onln >= Act.

l(Id) ->
    {logical, Id}.

t(X) ->
    {thread, X}.

c(X) ->
    {core, X}.

p(X) ->
    {processor, X}.

n(X) ->
    {node, X}.

mcall(Node, Funs) ->
    Parent = self(),
    Refs = lists:map(fun (Fun) ->
                             Ref = make_ref(),
                             spawn_link(Node,
                                        fun () ->
                                                Res = Fun(),
                                                unlink(Parent),
                                                Parent ! {Ref, Res}
                                        end),
                             Ref
                     end, Funs),
    lists:map(fun (Ref) ->
                      receive
                          {Ref, Res} ->
                              Res
                      end
              end, Refs).

erl_rel_flag_var() ->
    "ERL_OTP"++erlang:system_info(otp_release)++"_FLAGS".

clear_erl_rel_flags() ->
    EnvVar = erl_rel_flag_var(),
    case os:getenv(EnvVar) of
	false ->
	    false;
	Value ->
	    os:putenv(EnvVar, ""),
	    Value
    end.

restore_erl_rel_flags(false) ->
    ok;
restore_erl_rel_flags(OldValue) ->
    os:putenv(erl_rel_flag_var(), OldValue),
    ok.

ok(too_slow, _Config) ->
    {comment, "Too slow system to do any actual testing..."};
ok(_Res, Config) ->
    proplists:get_value(ok_res, Config).

chk_result(too_slow,
           _LWorkers,
           _NWorkers,
           _HWorkers,
           _MWorkers,
           _LNShouldWork,
           _HShouldWork,
           _MShouldWork) ->
    ok;
chk_result([{low, L, Lmin, _Lmax},
            {normal, N, Nmin, _Nmax},
            {high, H, Hmin, _Hmax},
            {max, M, Mmin, _Mmax}] = Res,
           LWorkers,
           NWorkers,
           HWorkers,
           MWorkers,
           LNShouldWork,
           HShouldWork,
           MShouldWork) ->
    io:format("~p~n", [Res]),
    Relax = relax_limits(),
    case {L, N} of
        {0, 0} ->
            false = LNShouldWork;
        _ ->
            {LminRatioLim,
             NminRatioLim,
             LNRatioLimMin,
             LNRatioLimMax} = case Relax of
                                  false -> {0.5, 0.5, 0.05, 0.25};
                                  true -> {0.05, 0.05, 0.01, 0.4}
                              end,
            Lavg = L/LWorkers,
            Navg = N/NWorkers,
            Ratio = Lavg/Navg,
            LminRatio = Lmin/Lavg,
            NminRatio = Nmin/Navg,
            io:format("low min ratio=~p~n"
                      "normal min ratio=~p~n"
                      "low avg=~p~n"
                      "normal avg=~p~n"
                      "low/normal ratio=~p~n",
                      [LminRatio, NminRatio, Lavg, Navg, Ratio]),
            erlang:display({low_min_ratio, LminRatio}),
            erlang:display({normal_min_ratio, NminRatio}),
            erlang:display({low_avg, Lavg}),
            erlang:display({normal_avg, Navg}),
            erlang:display({low_normal_ratio, Ratio}),
            chk_lim(LminRatioLim, LminRatio, 1.0, low_min_ratio),
            chk_lim(NminRatioLim, NminRatio, 1.0, normal_min_ratio),
            chk_lim(LNRatioLimMin, Ratio, LNRatioLimMax, low_normal_ratio),
            true = LNShouldWork,
            ok
    end,
    case H of
        0 ->
            false = HShouldWork;
        _ ->
            HminRatioLim = case Relax of
                               false -> 0.5;
                               true -> 0.1
                           end,
            Havg = H/HWorkers,
            HminRatio = Hmin/Havg,
            erlang:display({high_min_ratio, HminRatio}),
            chk_lim(HminRatioLim, HminRatio, 1.0, high_min_ratio),
            true = HShouldWork,
            ok
    end,
    case M of
        0 ->
            false = MShouldWork;
        _ ->
            MminRatioLim = case Relax of
                               false -> 0.5;
                               true -> 0.1
                           end,
            Mavg = M/MWorkers,
            MminRatio = Mmin/Mavg,
            erlang:display({max_min_ratio, MminRatio}),
            chk_lim(MminRatioLim, MminRatio, 1.0, max_min_ratio),
            true = MShouldWork,
            ok
    end,
    ok.

	    
	    
chk_lim(Min, V, Max, _What) when Min =< V, V =< Max ->
    ok;
chk_lim(_Min, V, _Max, What) ->
    ct:fail({bad, What, V}).

snd(_Msg, []) ->
    [];
snd(Msg, [P|Ps]) ->
    P ! Msg,
    Ps.

relax_limits() ->
    case strange_system_scale() of
	Scale when Scale > 1 ->
	    io:format("Relaxing limits~n", []),
	    true;
	_ ->
	    false
    end.

strange_system_scale() ->
    S0 = 1,
    S1 = case erlang:system_info(schedulers_online)
	     > erlang:system_info(logical_processors) of
	     true -> S0*2;
	     false -> S0
	 end,
    S2 = case erlang:system_info(debug_compiled) of
	     true -> S1*10;
	     false ->
		 case erlang:system_info(lock_checking) of
		     true -> S1*2;
		     false -> S1
		 end
	 end,
    S3 = case lock_counting() of
	     true -> S2*2;
	     false -> S2
	 end,
    S3.

lock_counting() ->
    lock_counting(erlang:system_info(system_version)).

lock_counting([]) ->
    false;
lock_counting([$[,$l,$o,$c,$k,$-,$c,$o,$u,$n,$t,$i,$n,$g,$],_]) ->
    true;
lock_counting([_C|Cs]) ->
    lock_counting(Cs).

go_work([], [], [], []) ->
    [];
go_work(L, N, [], []) ->
    go_work(snd(go_work, L), snd(go_work, N), [], []);
go_work(L, N, H, []) ->
    go_work(L, N, snd(go_work, H), []);
go_work(L, N, H, M) ->
    go_work(L, N, H, snd(go_work, M)).

stop_work([], [], [], []) ->
    [];
stop_work([], [], [], M) ->
    stop_work([], [], [], snd(stop_work, M));
stop_work([], [], H, M) ->
    stop_work([], [], snd(stop_work, H), M);
stop_work(L, N, H, M) ->
    stop_work(snd(stop_work, L), snd(stop_work, N), H, M).

wait_balance(N) when is_integer(N) ->
    case erlang:system_info(schedulers_active) of
	1 ->
	    done;
	_ ->
	    erts_debug:set_internal_state(available_internal_state,true),
	    Start = erts_debug:get_internal_state(nbalance),
	    End = (Start + N) band ((1 bsl (8*erlang:system_info(wordsize)))-1),
	    wait_balance(Start, End),
	    erts_debug:set_internal_state(available_internal_state,false)
    end.

wait_balance(Start, End) ->
    X = erts_debug:get_internal_state(nbalance),
    case End =< X of
	true ->
	    case Start =< End of
		true ->
		    done;
		false ->
		    case X < Start of
			true ->
			    done;
			false ->
			    receive after 250 -> ok end,
			    wait_balance(Start, End)
		    end
	    end;
	false ->
	    receive after 250 -> ok end,
	    wait_balance(Start, End)
    end.

wait_reds(RedsLimit, Timeout) ->
    Stop = erlang:start_timer(Timeout, self(), stop),
    statistics(reductions),
    wait_reds(0, RedsLimit, Stop).

wait_reds(Reds, RedsLimit, Stop) when Reds < RedsLimit ->
    receive
	{timeout, Stop, stop} ->
	    erlang:display(timeout),
	    erlang:display({reduction_limit, RedsLimit}),
	    erlang:display({reductions, Reds}),
	    done
    after 10000 ->
	    {_, NewReds} = statistics(reductions),
	    wait_reds(NewReds+Reds, RedsLimit, Stop)
    end;
wait_reds(Reds, RedsLimit, Stop) when is_reference(Stop) ->
    erlang:cancel_timer(Stop),
    receive {timeout, Stop, stop} -> ok after 0 -> ok end,
    wait_reds(Reds, RedsLimit, false);
wait_reds(Reds, RedsLimit, _Stop) ->
    erlang:display({reduction_limit, RedsLimit}),
    erlang:display({reductions, Reds}),
    done.

do_it(Tracer, Low, Normal, High, Max) ->
    do_it(Tracer, Low, Normal, High, Max, ?DEFAULT_TEST_REDS_PER_SCHED).

do_it(Tracer, Low, Normal, High, Max, RedsPerSchedLimit) ->
    OldPrio = process_flag(priority, max),
    go_work(Low, Normal, High, Max),
    StartWait = erlang:monotonic_time(millisecond),
    %% Give the emulator a chance to balance the load...
    wait_balance(5),
    EndWait = erlang:monotonic_time(millisecond),
    BalanceWait = EndWait-StartWait,
    erlang:display({balance_wait, BalanceWait}),
    Timeout = (15 - 4)*60*1000 - BalanceWait,
    Res = case Timeout < 60*1000 of
	      true ->
		  stop_work(Low, Normal, High, Max),
		  too_slow;
	      false ->
		  set_tracing(true, Tracer, normal, Normal),
		  set_tracing(true, Tracer, low, Low),
		  set_tracing(true, Tracer, high, High),
		  set_tracing(true, Tracer, max, Max),
		  wait_reds(RedsPerSchedLimit
			    * erlang:system_info(schedulers_online),
			    Timeout),
		  set_tracing(false, Tracer, normal, Normal),
		  set_tracing(false, Tracer, low, Low),
		  set_tracing(false, Tracer, high, High),
		  set_tracing(false, Tracer, max, Max),
		  stop_work(Low, Normal, High, Max),
		  get_trace_result(Tracer)
	  end,
    process_flag(priority, OldPrio),
    Res.

workers_exit([]) ->
    ok;
workers_exit([P|Ps]) when is_pid(P) ->
    Mon = erlang:monitor(process, P),
    unlink(P),
    exit(P, kill),
    workers_exit(Ps),
    receive {'DOWN', Mon, process, P, _} -> ok end,
    ok;
workers_exit([[]]) ->
    ok;
workers_exit([Ps|Pss])  ->
    workers_exit(Ps),
    workers_exit(Pss).

do_work(PartTime) ->
    lists:reverse(lists:seq(1, 50)),
    receive stop_work -> receive after infinity -> ok end after 0 -> ok end,
    case PartTime of
	true -> receive after 1 -> ok end;
	false -> ok
    end,
    do_work(PartTime).

workers(N, _Prio, _PartTime) when N =< 0 ->
    [];
workers(N, Prio, PartTime) ->
    Parent = self(),
    W = spawn_opt(fun () ->
			  Parent ! {ready, self()},
			  receive
			      go_work ->
				  do_work(PartTime)
			  end
		  end,
		  [{priority, Prio}, link]),
    Ws = workers(N-1, Prio, PartTime),
    receive {ready, W} -> ok end,
    [W|Ws].

workers(N, Prio) ->
    workers(N, Prio, false).

part_time_workers(N, Prio) ->
    workers(N, Prio, true).

tracer(Low, Normal, High, Max) ->
    receive
        {tracees, Prio, Tracees} ->
            save_tracees(Prio, Tracees),
            case Prio of
                low -> tracer(Tracees++Low, Normal, High, Max);
                normal -> tracer(Low, Tracees++Normal, High, Max);
                high -> tracer(Low, Normal, Tracees++High, Max);
                max -> tracer(Low, Normal, High, Tracees++Max)
            end;
        {get_result, Ref, Who} ->
            Delivered = erlang:trace_delivered(all),
            receive
                {trace_delivered, all, Delivered} ->
                    ok
            end,
            {Lc, Nc, Hc, Mc} = read_trace(),
            GetMinMax
            = fun (Prio, Procs) ->
                      LargeNum = 1 bsl 64,
                      case lists:foldl(fun (P, {Mn, Mx} = MnMx) ->
                                               {Prio, C} = get(P),
                                               case C < Mn of
                                                   true ->
                                                       case C > Mx of
                                                           true ->
                                                               {C, C};
                                                           false ->
                                                               {C, Mx}
                                                       end;
                                                   false ->
                                                       case C > Mx of
                                                           true -> {Mn, C};
                                                           false -> MnMx
                                                       end
                                               end
                                       end,
                                       {LargeNum, 0},
                                       Procs) of
                          {LargeNum, 0} -> {0, 0};
                          Res -> Res
                      end
              end,
            {Lmin, Lmax} = GetMinMax(low, Low),
            {Nmin, Nmax} = GetMinMax(normal, Normal),
            {Hmin, Hmax} = GetMinMax(high, High),
            {Mmin, Mmax} = GetMinMax(max, Max),
            Who ! {trace_result, Ref, [{low, Lc, Lmin, Lmax},
                                       {normal, Nc, Nmin, Nmax},
                                       {high, Hc, Hmin, Hmax},
                                       {max, Mc, Mmin, Mmax}]}
    end.

read_trace() ->
    read_trace(0,0,0,0).

read_trace(Low, Normal, High, Max) ->
    receive
	{trace, Proc, in, _} ->
	    {Prio, Count} = get(Proc),
	    put(Proc, {Prio, Count+1}),
	    case Prio of
		low -> read_trace(Low+1, Normal, High, Max);
		normal -> read_trace(Low, Normal+1, High, Max);
		high -> read_trace(Low, Normal, High+1, Max);
		max -> read_trace(Low, Normal, High, Max+1)
	    end;
	{trace, _Proc, out, _} ->
	    read_trace(Low, Normal, High, Max)
    after 0 ->
	    {Low, Normal, High, Max}
    end.

save_tracees(_Prio, []) ->
    ok;
save_tracees(Prio, [T|Ts]) ->
    put(T, {Prio, 0}),
    save_tracees(Prio, Ts).
	    
start_tracer() ->
    Tracer = spawn_link(fun () -> tracer([], [], [], []) end),
    true = erlang:suspend_process(Tracer),
    Tracer.

get_trace_result(Tracer) ->
    erlang:resume_process(Tracer),
    Ref = make_ref(),
    Tracer ! {get_result, Ref, self()},
    receive
	{trace_result, Ref, Res} ->
	    Res
    end.
	

set_tracing(_On, _Tracer, _Prio, []) ->
    ok;
set_tracing(true, Tracer, Prio, Pids) ->
    Tracer ! {tracees, Prio, Pids},
    set_tracing(true, Tracer, Pids);
set_tracing(false, Tracer, _Prio, Pids) ->
    set_tracing(false, Tracer, Pids).

set_tracing(_On, _Tracer, []) ->
    ok;
set_tracing(On, Tracer, [Pid|Pids]) ->
    1 = erlang:trace(Pid, On, [running, {tracer, Tracer}]),
    set_tracing(On, Tracer, Pids).
    
active_schedulers() ->
    case erlang:system_info(schedulers_online) of
	1 ->
	    1;
	N ->
	    case erlang:system_info(multi_scheduling) of
		blocked -> 1;
		enabled -> N
	    end
    end.
    
start_node(Config) ->
    start_node(Config, "").

start_node(Config, Args) when is_list(Config) ->
    Pa = filename:dirname(code:which(?MODULE)),
    Name = list_to_atom(atom_to_list(?MODULE)
			++ "-"
			++ atom_to_list(proplists:get_value(testcase, Config))
			++ "-"
			++ integer_to_list(erlang:system_time(second))
			++ "-"
			++ integer_to_list(erlang:unique_integer([positive]))),
    test_server:start_node(Name, slave, [{args, "-pa "++Pa++" "++Args}]).

stop_node(Node) ->
    test_server:stop_node(Node).


enable_internal_state() ->
    case catch erts_debug:get_internal_state(available_internal_state) of
	true -> true;
	_ -> erts_debug:set_internal_state(available_internal_state, true)
    end.

cmp(X, X) ->
    ok;
cmp(X, Y) ->
    io:format("cmp failed:~n X=~p~n Y=~p~n", [X,Y]),
    cmp_aux(X, Y).


cmp_aux([X0|Y0], [X1|Y1]) ->
    cmp_aux(X0, X1),
    cmp_aux(Y0, Y1);
cmp_aux(T0, T1) when is_tuple(T0), is_tuple(T1), size(T0) == size(T1) ->
    cmp_tuple(T0, T1, 1, size(T0));
cmp_aux(X, X) ->
    ok;
cmp_aux(F0, F1) ->
    ct:fail({no_match, F0, F1}).

cmp_tuple(_T0, _T1, N, Sz) when N > Sz ->
    ok;
cmp_tuple(T0, T1, N, Sz) ->
    cmp_aux(element(N, T0), element(N, T1)),
    cmp_tuple(T0, T1, N+1, Sz).
