%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2009. All Rights Reserved.
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
%%% File    : scheduler_SUITE.erl
%%% Author  : Rickard Green
%%% Description : 
%%%
%%% Created : 27 Oct 2008 by Rickard Green
%%%-------------------------------------------------------------------
-module(scheduler_SUITE).


%-define(line_trace, 1).

-include("test_server.hrl").

%-compile(export_all).
-export([all/1, init_per_testcase/2, fin_per_testcase/2]).

-export([equal/1,
	 few_low/1,
	 many_low/1,
	 equal_with_part_time_high/1,
	 equal_with_part_time_max/1,
	 equal_and_high_with_part_time_max/1,
	 equal_with_high/1,
	 equal_with_high_max/1,
	 bound_process/1,
	 scheduler_bind/1,
	 scheduler_bind_types/1,
	 cpu_topology/1,
	 sct_cmd/1,
	 sbt_cmd/1]).

-define(DEFAULT_TIMEOUT, ?t:minutes(10)).

-define(MIN_SCHEDULER_TEST_TIMEOUT, ?t:minutes(1)).

all(doc) -> [];
all(suite) ->
    [equal,
     few_low,
     many_low,
     equal_with_part_time_high,
     equal_with_part_time_max,
     equal_and_high_with_part_time_max,
     equal_with_high,
     equal_with_high_max,
     bound_process,
     scheduler_bind].

init_per_testcase(Case, Config) when is_list(Config) ->
    Dog = ?t:timetrap(?DEFAULT_TIMEOUT),
    process_flag(priority, max),
    erlang:display({'------------', ?MODULE, Case, '------------'}),
    OkRes = ok,
    [{watchdog, Dog}, {testcase, Case}, {ok_res, OkRes} |Config].

fin_per_testcase(_Case, Config) when is_list(Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
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
    ?line Tracer = start_tracer(),
    ?line Low = workers(LW, low),
    ?line Normal = workers(NW, normal),
    ?line Res = do_it(Tracer, Low, Normal, [], []),
    ?line chk_result(Res, LW, NW, 0, 0, true, false, false),
    ?line workers_exit([Low, Normal]),
    ?line ok(Res, Config).

equal_with_part_time_high(Config) when is_list(Config) ->
    ?line NW = 500,
    ?line LW = 500,
    ?line HW = 1,
    ?line Tracer = start_tracer(),
    ?line Normal = workers(NW, normal),
    ?line Low = workers(LW, low),
    ?line High = part_time_workers(HW, high),
    ?line Res = do_it(Tracer, Low, Normal, High, []),
    ?line chk_result(Res, LW, NW, HW, 0, true, true, false),
    ?line workers_exit([Low, Normal, High]),
    ?line ok(Res, Config).

equal_and_high_with_part_time_max(Config) when is_list(Config) ->
    ?line NW = 500,
    ?line LW = 500,
    ?line HW = 500,
    ?line MW = 1,
    ?line Tracer = start_tracer(),
    ?line Low = workers(LW, low),
    ?line Normal = workers(NW, normal),
    ?line High = workers(HW, high),
    ?line Max = part_time_workers(MW, max),
    ?line Res = do_it(Tracer, Low, Normal, High, Max),
    ?line chk_result(Res, LW, NW, HW, MW, false, true, true),
    ?line workers_exit([Low, Normal, Max]),
    ?line ok(Res, Config).

equal_with_part_time_max(Config) when is_list(Config) ->
    ?line NW = 500,
    ?line LW = 500,
    ?line MW = 1,
    ?line Tracer = start_tracer(),
    ?line Low = workers(LW, low),
    ?line Normal = workers(NW, normal),
    ?line Max = part_time_workers(MW, max),
    ?line Res = do_it(Tracer, Low, Normal, [], Max),
    ?line chk_result(Res, LW, NW, 0, MW, true, false, true),
    ?line workers_exit([Low, Normal, Max]),
    ?line ok(Res, Config).

equal_with_high(Config) when is_list(Config) ->
    ?line NW = 500,
    ?line LW = 500,
    ?line HW = 1,
    ?line Tracer = start_tracer(),
    ?line Low = workers(LW, low),
    ?line Normal = workers(NW, normal),
    ?line High = workers(HW, high),
    ?line Res = do_it(Tracer, Low, Normal, High, []),
    ?line LNExe = case active_schedulers() of
		      S when S =< HW -> false;
		      _ -> true
		  end,
    ?line chk_result(Res, LW, NW, HW, 0, LNExe, true, false),
    ?line workers_exit([Low, Normal, High]),
    ?line ok(Res, Config).

equal_with_high_max(Config) when is_list(Config) ->
    ?line NW = 500,
    ?line LW = 500,
    ?line HW = 1,
    ?line MW = 1,
    ?line Tracer = start_tracer(),
    ?line Normal = workers(NW, normal),
    ?line Low = workers(LW, low),
    ?line High = workers(HW, high),
    ?line Max = workers(MW, max),
    ?line Res = do_it(Tracer, Low, Normal, High, Max),
    ?line {LNExe, HExe} = case active_schedulers() of
			      S when S =< MW -> {false, false};
			      S when S =< (MW + HW) -> {false, true};
			      _ -> {true, true}
			  end,
    ?line chk_result(Res, LW, NW, HW, MW, LNExe, HExe, true),
    ?line workers_exit([Low, Normal, Max]),
    ?line ok(Res, Config).

bound_process(Config) when is_list(Config) ->
    case erlang:system_info(run_queues) == erlang:system_info(schedulers) of
	true ->
	    ?line NStartBase = 20000,
	    ?line NStart = case {erlang:system_info(debug_compiled),
				 erlang:system_info(lock_checking)} of
			       {true, true} -> NStartBase div 100;
			       {_, true} -> NStartBase div 10;
			       _ -> NStartBase
			   end,
	    ?line MStart = 100,
	    ?line Seq = lists:seq(1, 100),
	    ?line Tester = self(),
	    ?line Procs = lists:map(
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
	    ?line lists:foreach(fun (P) -> receive {P, done} -> ok end end,
				Procs),
	    ?line ok;
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

scheduler_bind(suite) ->
    [scheduler_bind_types,
     cpu_topology,
     sct_cmd,
     sbt_cmd].

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
    spawn_link(Node,
	       fun () ->
		       enable_internal_state(),
		       Res = (catch erts_debug:get_internal_state(
				      {fake_scheduler_bindings, BindType})),
		       Parent ! {Ref, Res}
	       end),
    receive
	{Ref, Res} ->
	    ?t:format("~p: ~p~n", [BindType, Res]),
	    Res
    end.

scheduler_bind_types(Config) when is_list(Config) ->
    ?line OldRelFlags = clear_erl_rel_flags(),
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
    ?line ok.

scheduler_bind_types_test(Config, Topology, CmdLine, TermLetter) ->
    ?line ?t:format("Testing (~p): ~p~n", [TermLetter, Topology]),
    ?line {ok, Node0} = start_node(Config),
    ?line _ = rpc:call(Node0, erlang, system_flag, [cpu_topology, Topology]),
    ?line cmp(Topology, rpc:call(Node0, erlang, system_info, [cpu_topology])),
    ?line check_bind_types(Node0, TermLetter),
    ?line stop_node(Node0),
    ?line {ok, Node1} = start_node(Config, CmdLine),
    ?line cmp(Topology, rpc:call(Node1, erlang, system_info, [cpu_topology])),
    ?line check_bind_types(Node1, TermLetter),
    ?line stop_node(Node1).

check_bind_types(Node, a) ->
    ?line {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
	= bindings(Node, no_spread),
    ?line {0,2,4,6,8,10,12,14,1,3,5,7,9,11,13,15}
	= bindings(Node, thread_spread),
    ?line {0,4,8,12,2,6,10,14,1,5,9,13,3,7,11,15}
	= bindings(Node, processor_spread),
    ?line {0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15}
	= bindings(Node, spread),
    ?line {0,2,4,6,1,3,5,7,8,10,12,14,9,11,13,15}
	= bindings(Node, no_node_thread_spread),
    ?line {0,4,2,6,1,5,3,7,8,12,10,14,9,13,11,15}
	= bindings(Node, no_node_processor_spread),
    ?line {0,4,2,6,8,12,10,14,1,5,3,7,9,13,11,15}
	= bindings(Node, thread_no_node_processor_spread),
    ?line {0,4,2,6,8,12,10,14,1,5,3,7,9,13,11,15}
	= bindings(Node, default_bind),
    ?line ok;
check_bind_types(Node, b) ->
    ?line {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
	= bindings(Node, no_spread),
    ?line {0,2,4,6,8,10,12,14,1,3,5,7,9,11,13,15}
	= bindings(Node, thread_spread),
    ?line {0,8,2,10,4,12,6,14,1,9,3,11,5,13,7,15}
	= bindings(Node, processor_spread),
    ?line {0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15}
	= bindings(Node, spread),
    ?line {0,2,1,3,4,6,5,7,8,10,9,11,12,14,13,15}
	= bindings(Node, no_node_thread_spread),
    ?line {0,2,1,3,4,6,5,7,8,10,9,11,12,14,13,15}
	= bindings(Node, no_node_processor_spread),
    ?line {0,2,4,6,8,10,12,14,1,3,5,7,9,11,13,15}
	= bindings(Node, thread_no_node_processor_spread),
    ?line {0,2,4,6,8,10,12,14,1,3,5,7,9,11,13,15}
	= bindings(Node, default_bind),
    ?line ok;
check_bind_types(Node, c) ->
    ?line {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,
	   25,26,27,28,29,30,31} = bindings(Node, no_spread),
    ?line {0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,1,3,5,7,9,11,13,15,
	   17,19,21,23,25,27,29,31} = bindings(Node, thread_spread),
    ?line {0,4,8,16,20,24,2,6,10,18,22,26,12,28,14,30,1,5,9,17,21,25,
	   3,7,11,19,23,27,13,29,15,31} = bindings(Node, processor_spread),
    ?line {0,8,16,24,4,20,12,28,2,10,18,26,6,22,14,30,1,9,17,25,5,21,13,29,3,11,
	   19,27,7,23,15,31} = bindings(Node, spread),
    ?line {0,2,4,6,1,3,5,7,8,10,9,11,12,14,13,15,16,18,20,22,17,19,21,23,24,26,
	   25,27,28,30,29,31} = bindings(Node, no_node_thread_spread),
    ?line {0,4,2,6,1,5,3,7,8,10,9,11,12,14,13,15,16,20,18,22,17,21,19,23,24,26,
	   25,27,28,30,29,31} = bindings(Node, no_node_processor_spread),
    ?line {0,4,2,6,8,10,12,14,16,20,18,22,24,26,28,30,1,5,3,7,9,11,13,15,17,21,
	   19,23,25,27,29,31} = bindings(Node, thread_no_node_processor_spread),
    ?line {0,4,2,6,8,10,12,14,16,20,18,22,24,26,28,30,1,5,3,7,9,11,13,15,17,21,
	   19,23,25,27,29,31} = bindings(Node, default_bind),
    ?line ok;
check_bind_types(Node, d) ->
    ?line {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,
	   25,26,27,28,29,30,31} = bindings(Node, no_spread),
    ?line {0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,1,3,5,7,9,11,13,15,
	   17,19,21,23,25,27,29,31} = bindings(Node, thread_spread),
    ?line {0,8,12,16,24,28,2,10,14,18,26,30,4,20,6,22,1,9,13,17,25,29,3,11,15,
	   19,27,31,5,21,7,23} = bindings(Node, processor_spread),
    ?line {0,8,16,24,12,28,4,20,2,10,18,26,14,30,6,22,1,9,17,25,13,29,5,21,3,11,
	   19,27,15,31,7,23} = bindings(Node, spread),
    ?line {0,2,1,3,4,6,5,7,8,10,12,14,9,11,13,15,16,18,17,19,20,22,21,23,24,26,
	   28,30,25,27,29,31} = bindings(Node, no_node_thread_spread),
    ?line {0,2,1,3,4,6,5,7,8,12,10,14,9,13,11,15,16,18,17,19,20,22,21,23,24,28,
	   26,30,25,29,27,31} = bindings(Node, no_node_processor_spread),
    ?line {0,2,4,6,8,12,10,14,16,18,20,22,24,28,26,30,1,3,5,7,9,13,11,15,17,19,
	   21,23,25,29,27,31} = bindings(Node, thread_no_node_processor_spread),
    ?line {0,2,4,6,8,12,10,14,16,18,20,22,24,28,26,30,1,3,5,7,9,13,11,15,17,19,
	   21,23,25,29,27,31} = bindings(Node, default_bind),
    ?line ok;
check_bind_types(Node, e) ->
    ?line {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
	= bindings(Node, no_spread),
    ?line {0,2,4,6,8,10,12,14,1,3,5,7,9,11,13,15}
	= bindings(Node, thread_spread),
    ?line {0,8,2,10,4,12,6,14,1,9,3,11,5,13,7,15}
	= bindings(Node, processor_spread),
    ?line {0,8,2,10,4,12,6,14,1,9,3,11,5,13,7,15}
	= bindings(Node, spread),
    ?line {0,2,4,6,1,3,5,7,8,10,12,14,9,11,13,15}
	= bindings(Node, no_node_thread_spread),
    ?line {0,2,4,6,1,3,5,7,8,10,12,14,9,11,13,15}
	= bindings(Node, no_node_processor_spread),
    ?line {0,2,4,6,8,10,12,14,1,3,5,7,9,11,13,15}
	= bindings(Node, thread_no_node_processor_spread),
    ?line {0,2,4,6,8,10,12,14,1,3,5,7,9,11,13,15}
	= bindings(Node, default_bind),
    ?line ok;
check_bind_types(Node, f) ->
    ?line {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,
	   25,26,27,28,29,30,31} = bindings(Node, no_spread),
    ?line {0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,1,3,5,7,9,11,13,15,
	   17,19,21,23,25,27,29,31} = bindings(Node, thread_spread),
    ?line {0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,1,3,5,7,9,11,13,
	   15,17,19,21,23,25,27,29,31} = bindings(Node, processor_spread),
    ?line {0,8,16,24,2,10,18,26,4,12,20,28,6,14,22,30,1,9,17,25,3,11,19,27,5,13,
	   21,29,7,15,23,31} = bindings(Node, spread),
    ?line {0,2,4,6,1,3,5,7,8,10,12,14,9,11,13,15,16,18,20,22,17,19,21,23,24,26,
	   28,30,25,27,29,31} = bindings(Node, no_node_thread_spread),
    ?line {0,2,4,6,1,3,5,7,8,10,12,14,9,11,13,15,16,18,20,22,17,19,21,23,24,26,
	   28,30,25,27,29,31} = bindings(Node, no_node_processor_spread),
    ?line {0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,1,3,5,7,9,11,13,15,17,19,
	   21,23,25,27,29,31} = bindings(Node, thread_no_node_processor_spread),
    ?line {0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,1,3,5,7,9,11,13,15,17,19,
	   21,23,25,27,29,31} = bindings(Node, default_bind),
    ?line ok;
check_bind_types(Node, _) ->
    ?line bindings(Node, no_spread),
    ?line bindings(Node, thread_spread),
    ?line bindings(Node, processor_spread),
    ?line bindings(Node, spread),
    ?line bindings(Node, no_node_thread_spread),
    ?line bindings(Node, no_node_processor_spread),
    ?line bindings(Node, thread_no_node_processor_spread),
    ?line bindings(Node, default_bind),
    ?line ok.

cpu_topology(Config) when is_list(Config) ->
    ?line OldRelFlags = clear_erl_rel_flags(),
    try
	?line cpu_topology_test(
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
	?line cpu_topology_test(
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
	?line cpu_topology_test(
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
    ?line ok.

cpu_topology_test(Config, Topology, Cmd) ->
    ?line ?t:format("Testing~n ~p~n ~p~n", [Topology, Cmd]),
    ?line cpu_topology_bif_test(Config, Topology),
    ?line cpu_topology_cmdline_test(Config, Topology, Cmd),
    ?line ok.

cpu_topology_bif_test(_Config, false) ->
    ?line ok;
cpu_topology_bif_test(Config, Topology) ->
    ?line {ok, Node} = start_node(Config),
    ?line _ = rpc:call(Node, erlang, system_flag, [cpu_topology, Topology]),
    ?line cmp(Topology, rpc:call(Node, erlang, system_info, [cpu_topology])),
    ?line stop_node(Node),
    ?line ok.

cpu_topology_cmdline_test(_Config, _Topology, false) ->
    ?line ok;
cpu_topology_cmdline_test(Config, Topology, Cmd) ->
    ?line {ok, Node} = start_node(Config, Cmd),
    ?line cmp(Topology, rpc:call(Node, erlang, system_info, [cpu_topology])),
    ?line stop_node(Node),
    ?line ok.

sct_cmd(Config) when is_list(Config) ->
    ?line Topology = ?TOPOLOGY_A_TERM,
    ?line OldRelFlags = clear_erl_rel_flags(),
    try
	?line {ok, Node} = start_node(Config, ?TOPOLOGY_A_CMD),
	?line cmp(Topology,
		  rpc:call(Node, erlang, system_info, [cpu_topology])),
	?line cmp(Topology,
		  rpc:call(Node, erlang, system_flag, [cpu_topology, Topology])),
	?line cmp(Topology,
		  rpc:call(Node, erlang, system_info, [cpu_topology])),
	?line stop_node(Node)
    after
	restore_erl_rel_flags(OldRelFlags)
    end,
    ?line ok.

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
	    ?line {skipped, "Binding of schedulers not supported"};
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
		    ?line {skipped, "Don't know how to create cpu topology"};
		_ ->
		    case erlang:system_info(logical_processors) of
			LP when is_integer(LP) ->
			    OldRelFlags = clear_erl_rel_flags(),
			    try
				lists:foreach(fun ({ClBt, Bt}) ->
						      ?line sbt_test(Config,
								     CpuTCmd,
								     ClBt,
								     Bt,
								     LP)
					      end,
					      ?BIND_TYPES)
			    after
				restore_erl_rel_flags(OldRelFlags)
			    end,
			    ?line ok;
			_ ->
			    ?line {skipped,
				   "Don't know the amount of logical processors"}
		    end
	    end
    end.

sbt_test(Config, CpuTCmd, ClBt, Bt, LP) ->
    ?line ?t:format("Testing +sbt ~s (~p)~n", [ClBt, Bt]),
    ?line LPS = integer_to_list(LP),
    ?line Cmd = CpuTCmd++" +sbt "++ClBt++" +S"++LPS++":"++LPS,
    ?line {ok, Node} = start_node(Config, Cmd),
    ?line Bt = rpc:call(Node,
			erlang,
			system_info,
			[scheduler_bind_type]),
    ?line SB = rpc:call(Node,
			erlang,
			system_info,
			[scheduler_bindings]),
    ?line ?t:format("scheduler bindings: ~p~n", [SB]),
    ?line BS = case {Bt, erlang:system_info(logical_processors_available)} of
		   {unbound, _} -> 0;
		   {_, Int} when is_integer(Int) -> Int;
		   {_, _} -> LP
		end,
    ?line lists:foldl(fun (S, 0) ->
			      ?line unbound = S,
			      0;
			  (S, N) ->
			      ?line true = is_integer(S),
			      N-1
		      end,
		      BS,
		      tuple_to_list(SB)),
    ?line stop_node(Node),
    ?line ok.
    

%
%% Utils
%%

erl_rel_flag_var() ->
    "ERL_"++erlang:system_info(otp_release)++"_FLAGS".

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
    ?config(ok_res, Config).

chk_result(too_slow,
	   _LWorkers,
	   _NWorkers,
	   _HWorkers,
	   _MWorkers,
	   _LNShouldWork,
	   _HShouldWork,
	   _MShouldWork) ->
    ?line ok;
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
    ?line ?t:format("~p~n", [Res]),
    ?line Relax = relax_limits(),
    case {L, N} of
	{0, 0} ->
	    ?line false = LNShouldWork;
	_ ->
	    ?line {LminRatioLim,
		   NminRatioLim,
		   LNRatioLimMin,
		   LNRatioLimMax} = case Relax of
					false -> {0.5, 0.5, 0.05, 0.25};
					true -> {0.05, 0.05, 0.01, 0.4}
				    end,
	    ?line Lavg = L/LWorkers,
	    ?line Navg = N/NWorkers,
	    ?line Ratio = Lavg/Navg,
	    ?line LminRatio = Lmin/Lavg,
	    ?line NminRatio = Nmin/Navg,
	    ?line ?t:format("low min ratio=~p~n"
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
	    ?line chk_lim(LminRatioLim, LminRatio, 1.0, low_min_ratio),
	    ?line chk_lim(NminRatioLim, NminRatio, 1.0, normal_min_ratio),
	    ?line chk_lim(LNRatioLimMin, Ratio, LNRatioLimMax, low_normal_ratio),
	    ?line true = LNShouldWork,
	    ?line ok
    end,
    case H of
	0 ->
	    ?line false = HShouldWork;
	_ ->
	    ?line HminRatioLim = case Relax of
				     false -> 0.5;
				     true -> 0.1
				 end,
	    ?line Havg = H/HWorkers,
	    ?line HminRatio = Hmin/Havg,
	    erlang:display({high_min_ratio, HminRatio}),
	    ?line chk_lim(HminRatioLim, HminRatio, 1.0, high_min_ratio),
	    ?line true = HShouldWork,
	    ?line ok
    end,
    case M of
	0 ->
	    ?line false = MShouldWork;
	_ ->
	    ?line MminRatioLim = case Relax of
				     false -> 0.5;
				     true -> 0.1
				 end,
	    ?line Mavg = M/MWorkers,
	    ?line MminRatio = Mmin/Mavg,
	    erlang:display({max_min_ratio, MminRatio}),
	    ?line chk_lim(MminRatioLim, MminRatio, 1.0, max_min_ratio),
	    ?line true = MShouldWork,
	    ?line ok
    end,
    ?line ok.

	    
	    
chk_lim(Min, V, Max, _What) when Min =< V, V =< Max ->
    ok;
chk_lim(_Min, V, _Max, What) ->
    ?t:fail({bad, What, V}).

snd(_Msg, []) ->
    [];
snd(Msg, [P|Ps]) ->
    P ! Msg,
    Ps.

relax_limits() ->
    case strange_system_scale() of
	Scale when Scale > 1 ->
	    ?t:format("Relaxing limits~n", []),
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
    StartWait = now(),
    %% Give the emulator a chance to balance the load...
    wait_balance(5),
    EndWait = now(),
    BalanceWait = timer:now_diff(EndWait,StartWait) div 1000,
    erlang:display({balance_wait, BalanceWait}),
    Timeout = ?DEFAULT_TIMEOUT - ?t:seconds(10) - BalanceWait,
    Res = case Timeout < ?MIN_SCHEDULER_TEST_TIMEOUT of
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
    ?line Pa = filename:dirname(code:which(?MODULE)),
    ?line {A, B, C} = now(),
    ?line Name = list_to_atom(atom_to_list(?MODULE)
			      ++ "-"
			      ++ atom_to_list(?config(testcase, Config))
			      ++ "-"
			      ++ integer_to_list(A)
			      ++ "-"
			      ++ integer_to_list(B)
			      ++ "-"
			      ++ integer_to_list(C)),
    ?line ?t:start_node(Name, slave, [{args, "-pa "++Pa++" "++Args}]).

stop_node(Node) ->
    ?t:stop_node(Node).


enable_internal_state() ->
    case catch erts_debug:get_internal_state(available_internal_state) of
	true -> true;
	_ -> erts_debug:set_internal_state(available_internal_state, true)
    end.

cmp(X, X) ->
    ok;
cmp(X, Y) ->
    ?t:format("cmp failed:~n X=~p~n Y=~p~n", [X,Y]),
    cmp_aux(X, Y).


cmp_aux([X0|Y0], [X1|Y1]) ->
    cmp_aux(X0, X1),
    cmp_aux(Y0, Y1);
cmp_aux(T0, T1) when is_tuple(T0), is_tuple(T1), size(T0) == size(T1) ->
    cmp_tuple(T0, T1, 1, size(T0));
cmp_aux(X, X) ->
    ok;
cmp_aux(F0, F1) ->
    ?t:fail({no_match, F0, F1}).

cmp_tuple(_T0, _T1, N, Sz) when N > Sz ->
    ok;
cmp_tuple(T0, T1, N, Sz) ->
    cmp_aux(element(N, T0), element(N, T1)),
    cmp_tuple(T0, T1, N+1, Sz).
