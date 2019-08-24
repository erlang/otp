%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2019. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Verify the application specifics of the Megaco application
%%----------------------------------------------------------------------
-module(megaco_mib_test).

-export([
         t/0, t/1,

         all/0,
         groups/0,
         init_per_testcase/2,
         end_per_testcase/2,
         init_per_group/2,
         end_per_group/2,

         plain/1,
         connect/1,
         traffic/1,

         mg/3,
         mgc/3,

         handle_connect/3,
         handle_disconnect/4,
         handle_syntax_error/4,
         handle_message_error/4,
         handle_trans_request/4,
         handle_trans_long_request/4,
         handle_trans_reply/5,
         handle_trans_ack/5
        ]).

-include("megaco_test_lib.hrl").
-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").

-define(TEST_VERBOSITY, info). % silence | info | debug
-define(MGC_VERBOSITY,  info).
-define(MG_VERBOSITY,   info).

-define(LOAD_COUNTER_START, 100).
-define(A4444, ["11111111", "00000000", "00000000"]).

-record(mgc, {parent  = undefined,
	      tcp_sup = undefined,
	      udp_sup = undefined,
	      mid     = undefined,
	      mg      = []}).
-record(mg, {parent       = undefined,
	     mid          = undefined,
	     conn_handle  = undefined,
	     state        = initiated,
	     load_counter = 0}).

t()     -> megaco_test_lib:t(?MODULE).
t(Case) -> megaco_test_lib:t({?MODULE, Case}).


%% Test server callbacks
init_per_testcase(Case, Config) ->
    progress("init_per_testcase -> ~w", [Case]),
    process_flag(trap_exit, true),
    case Case of
	traffic ->
	    Conf0 = lists:keydelete(tc_timeout, 1, Config),
	    Conf  = [{tc_timeout, timer:minutes(5)}|Conf0],
	    megaco_test_lib:init_per_testcase(Case, Conf);
	_ ->
	    megaco_test_lib:init_per_testcase(Case, Config)
    end.

end_per_testcase(Case, Config) ->
    progress("end_per_testcase -> ~w", [Case]),
    process_flag(trap_exit, false),
    megaco_test_lib:end_per_testcase(Case, Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() -> 
    [plain, connect, traffic].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plain(suite) ->
    [];
plain(doc) ->
    ["Test case for the basic statistics counter handling. "];
plain(Config) when is_list(Config) ->
    io:format("create test table 1~n", []),
    Tab1 = megaco_test_cnt1,
    megaco_stats:init(Tab1),

    io:format("~ncreate test table 2~n", []),
    Tab2 = megaco_test_cnt2,
    megaco_stats:init(Tab2, [kalle, hobbe]),

    io:format("~ntable 1 increments~n", []),
    H1 = #megaco_conn_handle{local_mid  = {deviceName, "a"},
			     remote_mid = {deviceName, "b"}},
    H2 = #megaco_conn_handle{local_mid  = {deviceName, "a"},
			     remote_mid = {deviceName, "c"}},
    1 = megaco_stats:inc(Tab1, H1, sune),
    2 = megaco_stats:inc(Tab1, H2, sune, 2),
    3 = megaco_stats:inc(Tab1, H1, gurka, 3),
    4 = megaco_stats:inc(Tab1, H2, sune, 2),
    4 = megaco_stats:inc(Tab1, H1, gurka),

    io:format("~ntable 2 increments~n", []),
    H3 = #megaco_conn_handle{local_mid  = {deviceName, "e"},
			     remote_mid = {deviceName, "c"}},
    H4 = #megaco_conn_handle{local_mid  = {deviceName, "e"},
			     remote_mid = {deviceName, "d"}},
    1 = megaco_stats:inc(Tab2, H3, tomat),
    4 = megaco_stats:inc(Tab2, H3, tomat, 3),
    5 = megaco_stats:inc(Tab2, H4, paprika, 5),

    io:format("~ntable 2 global increments~n", []),
    1 = megaco_stats:inc(Tab2, kalle),
    1 = megaco_stats:inc(Tab2, hobbe),
    2 = megaco_stats:inc(Tab2, hobbe),
    2 = megaco_stats:inc(Tab2, kalle),
    3 = megaco_stats:inc(Tab2, kalle),
    4 = megaco_stats:inc(Tab2, hobbe, 2),
    
    io:format("~ntable 1 stats~n", []),
    {ok, Stats1} = megaco_stats:get_stats(Tab1),
    io:format("Stats1 = ~p~n", [Stats1]),
    {value, {H1, H1Stats}} = lists:keysearch(H1, 1, Stats1),
    Stats1_2 = lists:keydelete(H1, 1, Stats1),
    {value, {H2, H2Stats}} = lists:keysearch(H2, 1, Stats1_2), 
    Stats1_3 = lists:keydelete(H2, 1, Stats1_2),
    [] = Stats1_3,
    io:format("H1Stats = ~p~n", [H1Stats]),
    io:format("H2Stats = ~p~n", [H2Stats]),

    {value, {sune, 1}}  = lists:keysearch(sune, 1, H1Stats),
    H1Stats_2 = lists:keydelete(sune, 1, H1Stats),
    {value, {gurka, 4}} = lists:keysearch(gurka, 1, H1Stats_2),
    H1Stats_3 = lists:keydelete(gurka, 1, H1Stats_2),
    [] = H1Stats_3,

    {value, {sune, 4}} = lists:keysearch(sune, 1, H2Stats),
    H2Stats_2 = lists:keydelete(sune, 1, H2Stats),
    [] = H2Stats_2,
    

    %% --
    io:format("~ntable 2 stats~n", []),
    {ok, Stats2} = megaco_stats:get_stats(Tab2),
    io:format("Stats2 = ~p~n", [Stats2]),
    {ok, 3} = megaco_stats:get_stats(Tab2, kalle),
    {ok, 4} = megaco_stats:get_stats(Tab2, hobbe),
     

    %% --
    io:format("~ntable 1 reset stats for ~p~n", [H1]),
    megaco_stats:reset_stats(Tab1, H1),
    {ok, Stats1_4} = megaco_stats:get_stats(Tab1),
    io:format("Stats1_4 = ~p~n", [Stats1_4]),
    {ok, 0} = megaco_stats:get_stats(Tab1, H1, sune),
    {ok, 0} = megaco_stats:get_stats(Tab1, H1, gurka),
    

    %% --
    io:format("~ntable 2 reset stats for kalle and ~p~n", [H4]),
    megaco_stats:reset_stats(Tab2, kalle),
    megaco_stats:reset_stats(Tab2, H4),
    {ok, Stats2_2} = megaco_stats:get_stats(Tab2),
    io:format("Stats2_2 = ~p~n", [Stats2_2]),
    {ok, 0} = megaco_stats:get_stats(Tab2, kalle),
    {ok, 4} = megaco_stats:get_stats(Tab2, hobbe),
    {ok, 4} = megaco_stats:get_stats(Tab2, H3, tomat),
    {ok, 0} = megaco_stats:get_stats(Tab2, H4, paprika),
    {ok, Stats4_4} = megaco_stats:get_stats(Tab2, H4),
    io:format("Stats4_4 = ~p~n", [Stats4_4]),

    %% --
    io:format("~ntable 2 stats for nonexisting counters~n", []),
    {error, _} = megaco_stats:get_stats(Tab2, kalla),
    {error, _} = megaco_stats:get_stats(Tab2, H3, paprika),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connect(suite) ->
    [];
connect(doc) ->
    [];
connect(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    i("connect -> starting"),
    progress("start nodes"),
    MgcNode = make_node_name(mgc),
    Mg1Node = make_node_name(mg1),
    Mg2Node = make_node_name(mg2),
    d("connect -> Nodes: "
      "~n   MgcNode: ~p"
      "~n   Mg1Node: ~p"
      "~n   Mg2Node: ~p", [MgcNode, Mg1Node, Mg2Node]),
    ok = megaco_test_lib:start_nodes([MgcNode, Mg1Node, Mg2Node], 
				     ?FILE, ?LINE),

    %% Start the MGC and MGs
    ET = [{text,tcp}, {text,udp}, {binary,tcp}, {binary,udp}],
    progress("start MGC"),
    {ok, Mgc} = 
	start_mgc(MgcNode, {deviceName, "ctrl"}, ET, ?MGC_VERBOSITY),
    progress("start MG1"),
    {ok, Mg1} = 
	start_mg(Mg1Node,  {deviceName, "mg1"}, text,   tcp, ?MG_VERBOSITY),
    progress("start MG2"),
    {ok, Mg2} = 
	start_mg(Mg2Node,  {deviceName, "mg2"}, binary, udp, ?MG_VERBOSITY),

    %% Collect the initial statistics (should be zero if anything)
    progress("collect initial MG1 stats"),
    {ok, Mg1Stats0} = get_stats(Mg1, 1),
    d("connect  -> stats for Mg1: ~n~p", [Mg1Stats0]),
    progress("collect initial MG2 stats"),
    {ok, Mg2Stats0} = get_stats(Mg2, 1),
    d("connect  -> stats for Mg2: ~n~p", [Mg2Stats0]),
    progress("collect initial MGC stats"),
    {ok, MgcStats0} = get_stats(Mgc, 1),
    d("connect  -> stats for Mgc: ~n~p", [MgcStats0]),

    %% Ask Mg1 to do a service change
    progress("perform MG1 service change"),
    {ok, Res1} = service_change(Mg1),
    d("connect -> (Mg1) service change result: ~p", [Res1]),

    %% Collect the statistics
    progress("collect MG1 statistics (after service change)"),
    {ok, Mg1Stats1} = get_stats(Mg1, 1),
    d("connect  -> stats for Mg1: ~n~p", [Mg1Stats1]),
    progress("collect MGC statistics (after MG1 service change)"),
    {ok, MgcStats1} = get_stats(Mgc, 1),
    d("connect  -> stats (1) for Mgc: ~n~p", [MgcStats1]),
    {ok, MgcStats2} = get_stats(Mgc, 2),
    d("connect  -> stats (2) for Mgc: ~n~p", [MgcStats2]),

    %% Ask Mg2 to do a service change
    progress("perform MG2 service change"),
    {ok, Res2} = service_change(Mg2),
    d("connect -> (Mg2) service change result: ~p", [Res2]),

    %% Collect the statistics
    progress("collect MG2 statistics (after service change)"),
    {ok, Mg2Stats1} = get_stats(Mg2, 1),
    d("connect  -> stats for Mg1: ~n~p", [Mg2Stats1]),
    progress("collect MGC statistics (after MG2 service change)"),
    {ok, MgcStats3} = get_stats(Mgc, 1),
    d("connect  -> stats (1) for Mgc: ~n~p", [MgcStats3]),
    {ok, MgcStats4} = get_stats(Mgc, 2),
    d("connect  -> stats (2) for Mgc: ~n~p", [MgcStats4]),

    %% Tell Mg1 to stop
    progress("stop MG1"),
    stop(Mg1),

    %% Collect the statistics
    progress("collect MGC statistics (after MG1 stop)"),
    {ok, MgcStats5} = get_stats(Mgc, 1),
    d("connect  -> stats (1) for Mgc: ~n~p", [MgcStats5]),
    {ok, MgcStats6} = get_stats(Mgc, 2),
    d("connect  -> stats (2) for Mgc: ~n~p", [MgcStats6]),

    %% Tell Mg2 to stop
    progress("stop MG2"),
    stop(Mg2),

    %% Collect the statistics
    progress("collect MGC statistics (after MG2 stop)"),
    {ok, MgcStats7} = get_stats(Mgc, 1),
    d("connect  -> stats (1) for Mgc: ~n~p", [MgcStats7]),
    {ok, MgcStats8} = get_stats(Mgc, 2),
    d("connect  -> stats (2) for Mgc: ~n~p", [MgcStats8]),

    %% Tell Mgc to stop
    progress("stop MGC"),
    stop(Mgc),

    i("connect -> done", []),
    progress("done"),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

traffic(suite) ->
    [];
traffic(doc) ->
    [];
traffic(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    i("traffic -> starting"),
    progress("start nodes"),
    MgcNode = make_node_name(mgc),
    Mg1Node = make_node_name(mg1),
    Mg2Node = make_node_name(mg2),
    Mg3Node = make_node_name(mg3),
    Mg4Node = make_node_name(mg4),
    d("traffic -> Nodes: "
      "~n   MgcNode: ~p"
      "~n   Mg1Node: ~p"
      "~n   Mg2Node: ~p"
      "~n   Mg3Node: ~p"
      "~n   Mg4Node: ~p", 
      [MgcNode, Mg1Node, Mg2Node, Mg3Node, Mg4Node]),
    ok = megaco_test_lib:start_nodes([MgcNode, 
				      Mg1Node, Mg2Node, Mg3Node, Mg4Node], 
				     ?FILE, ?LINE),

    %% Start the MGC and MGs
    i("traffic -> start the MGC"),    
    progress("start MGC"),
    ET = [{text,tcp}, {text,udp}, {binary,tcp}, {binary,udp}],
    {ok, Mgc} = 
	start_mgc(MgcNode, {deviceName, "ctrl"}, ET, ?MGC_VERBOSITY),

    i("traffic -> start and connect the MGs"),    
    progress("start and connect MGs"),
    MgConf0 = [{Mg1Node, "mg1", text,   tcp},
	       {Mg2Node, "mg2", text,   udp},
	       {Mg3Node, "mg3", binary, tcp},
	       {Mg4Node, "mg4", binary, udp}],
    MgConf = traffic_connect_mg(MgConf0, []),

    %% Collect and check the MGs statistics
    i("traffic -> collect and check the MGs stats"),    
    progress("collect and verify MGs (initial) stats"),
    traffic_verify_mg_stats(MgConf, 1, 1),

    %% Collect and check the MGC statistics
    i("traffic -> collect and check the MGC (initial) stats"),    
    progress("collect and verify MGC stats"),
    {ok, MgcStats1} = get_stats(Mgc, 1),
    d("traffic  -> stats (1) for Mgc: ~n~p~n", [MgcStats1]),
    traffic_verify_mgc_stats(Mgc, 1, 1),


    ?SLEEP(1000),


    %% And apply some load 
    i("traffic -> apply traffic load (1)"),
    progress("apply some load (1)"),
    ok = traffic_apply_load(MgConf),

    %% Await completion of load part and the collect traffic
    i("traffic -> await load (1) competion"),
    progress("await load (1) completion"),
    ok = traffic_await_load_complete(MgConf),


    ?SLEEP(1000),


    i("traffic -> collect and check the MGs statistics"),
    progress("collect and verify MGs (after load 1) stats"),
    traffic_verify_mg_stats(MgConf, 
			    1 + ?LOAD_COUNTER_START, 
			    1 + ?LOAD_COUNTER_START),

    i("traffic -> collect and check the MGC statistics"),
    progress("collect and verify MGC (after load 1) stats"),
    {ok, MgcStats3} = get_stats(Mgc, 1),
    d("traffic  -> stats (1) for Mgc: ~n~p~n", [MgcStats3]),
    traffic_verify_mgc_stats(Mgc, 
			     1 + ?LOAD_COUNTER_START, 
			     1 + ?LOAD_COUNTER_START),


    ?SLEEP(1000),


    %% Reset counters
    i("traffic -> reset the MGs statistics"),
    progress("reset MGs stats"),
    traffic_reset_mg_stats(MgConf),
    i("traffic -> collect and check the MGs statistics"),
    progress("collect and verify MGs (after reset) stats"),
    traffic_verify_mg_stats(MgConf, 0, 0),

    i("traffic -> reset the MGC statistics"),
    progress("reset MGC stats"),
    traffic_reset_mgc_stats(Mgc),
    i("traffic -> collect and check the MGC statistics"),
    progress("collect and verify MGC (after reset) stats"),
    traffic_verify_mgc_stats(Mgc, 0, 0),


    ?SLEEP(1000),


    %% And apply some load 
    i("traffic -> apply traffic load (2)"),
    progress("apply some load (2)"),
    ok = traffic_apply_load(MgConf),

    %% Await completion of load part and the collect traffic
    i("traffic -> await load (2) competion"),
    progress("await load (2) completion"),
    ok = traffic_await_load_complete(MgConf),


    ?SLEEP(1000),


    i("traffic -> collect and check the MGs statistics"),
    progress("collect and verify MGs (after load 2) stats"),
    traffic_verify_mg_stats(MgConf, 
			    ?LOAD_COUNTER_START, 
			    ?LOAD_COUNTER_START),

    i("traffic -> collect and check the MGC statistics"),
    progress("collect and verify MGC (after load 2) stats"),
    traffic_verify_mgc_stats(Mgc, 
			     ?LOAD_COUNTER_START, 
			     ?LOAD_COUNTER_START),


    ?SLEEP(1000),


    %% Tell MGs to stop
    i("traffic -> stop the MGs"),
    progress("stop MGs"),
    traffic_stop_mg(MgConf),


    ?SLEEP(1000),


    %% Collect the statistics
    i("traffic -> collect the MGC statistics"),
    progress("collect and verify MGC (after MGs stop) stats"),
    {ok, MgcStats7} = get_stats(Mgc, 1),
    d("traffic -> stats (1) for Mgc: ~n~p~n", [MgcStats7]),
    {ok, MgcStats8} = get_stats(Mgc, 2),
    d("traffic -> stats (2) for Mgc: ~n~p~n", [MgcStats8]),

    %% Tell Mgc to stop
    i("traffic -> stop the MGC"),
    progress("stop MGC"),
    stop(Mgc),

    i("traffic -> done", []),
    progress("done"),
    ok.


traffic_verify_mgc_stats(Pid, Out, In) 
  when is_pid(Pid) andalso is_integer(Out) andalso is_integer(In) ->
    d("traffic_verify_mgc_stats -> entry with"
      "~n   Out:   ~p"
      "~n   In:    ~p", [Out, In]),
    {ok, Stats} = get_stats(Pid, 2),
    d("traffic_verify_mgc_stats -> stats (2) for Mgc: ~n~p~n", [Stats]),
    traffic_verify_mgc_stats(Stats, Out, In);
    
traffic_verify_mgc_stats(Stats, Out, In) when is_list(Stats) ->
    d("traffic_verify_mgc_stats -> checking stats"),
    Gen   = traffic_verify_get_stats(gen, Stats),
    Trans = traffic_verify_get_stats(trans, Stats),
    traffic_verify_mgc_stats_gen(Gen),
    traffic_verify_mgc_stats_trans(Trans, Out, In).

traffic_verify_mgc_stats_gen([]) ->
    d("traffic_verify_mgc_stats_gen -> done"),
    ok;
traffic_verify_mgc_stats_gen([{medGwyGatewayNumErrors, 0}|Stats]) ->
    traffic_verify_mgc_stats_gen(Stats);
traffic_verify_mgc_stats_gen([{medGwyGatewayNumErrors, Val}|_]) ->
    exit({global_error_counter, Val, mgc});
traffic_verify_mgc_stats_gen([{Handle, Counters}|Stats]) ->
    N = {mgc, Handle, Handle},
    traffic_verify_counter(N, medGwyGatewayNumErrors, Counters, 0),
    traffic_verify_mgc_stats_gen(Stats).


traffic_verify_mgc_stats_trans([], _Out, _In) ->    
    ok;
traffic_verify_mgc_stats_trans([{Mod, Stats}|MgcStats], Out, In) ->    
    d("traffic_verify_mgc_stats_trans -> entry with"
      "~n   Mod:   ~p"
      "~n   Stats: ~p", [Mod, Stats]),
    traffic_verify_mgc_stats_trans(Mod, Stats, Out, In),
    traffic_verify_mgc_stats_trans(MgcStats, Out, In).

traffic_verify_mgc_stats_trans(_Mod, [], _Out, _In) ->
    ok;
traffic_verify_mgc_stats_trans(Mod, [{Handle,Counters}|Stats], Out, In) ->
    N = {mgc, Mod, Handle},
    traffic_verify_counter(N, medGwyGatewayNumErrors, Counters, 0),
    traffic_verify_counter(N, medGwyGatewayNumOutMessages, Counters, Out),
    traffic_verify_counter(N, medGwyGatewayNumInMessages, Counters, In),
    traffic_verify_mgc_stats_trans(Mod, Stats, Out, In).
    

traffic_verify_mg_stats(MgConf, Out, In) 
  when is_integer(Out) andalso is_integer(In) ->
    d("traffic_verify_mg_stats -> entry with"
      "~n   Out:   ~p"
      "~n   In:    ~p", [Out, In]),
    Stats = traffic_get_mg_stats(MgConf, []),
    d("traffic_verify_mg_stats -> stats for MGs: ~n~p", [Stats]),
    traffic_verify_mg_stats1(Stats, Out, In).
    
traffic_verify_mg_stats1([], _, _) ->
    ok;
traffic_verify_mg_stats1([{Name, Stats}|MgStats], Out, In) ->
    d("traffic_verify_mg_stats1 -> entry with"
      "~n   Name:  ~s"
      "~n   Stats: ~p", [Name, Stats]),
    Gen   = traffic_verify_get_stats(gen, Stats),
    Trans = traffic_verify_get_stats(trans, Stats),
    traffic_verify_mg_stats_gen(Name, Gen),
    traffic_verify_mg_stats_trans(Name, Trans, Out, In),
    traffic_verify_mg_stats1(MgStats, Out, In).

traffic_verify_mg_stats_gen(Mg, []) ->
    d("traffic_verify_mg_stats_gen -> ~s checked out OK",[Mg]),
    ok;
traffic_verify_mg_stats_gen(Mg, [{medGwyGatewayNumErrors, 0}|Stats]) ->
    traffic_verify_mg_stats_gen(Mg, Stats);
traffic_verify_mg_stats_gen(Mg, [{medGwyGatewayNumErrors, Val}|_]) ->
    exit({global_error_counter, Val, Mg});
traffic_verify_mg_stats_gen(Mg, [{_Handle, Counters}|Stats]) ->
    traffic_verify_counter(Mg, medGwyGatewayNumErrors, Counters, 0),
    traffic_verify_mg_stats_gen(Mg, Stats).

traffic_verify_mg_stats_trans(Mg, Counters, Out, In) ->
    traffic_verify_counter(Mg, medGwyGatewayNumErrors,      Counters, 0),
    traffic_verify_counter(Mg, medGwyGatewayNumOutMessages, Counters, Out),
    traffic_verify_counter(Mg, medGwyGatewayNumInMessages,  Counters, In).


traffic_verify_get_stats(S, Stats) ->
    case lists:keysearch(S, 1, Stats) of
	{value, {S, Val}} ->
	    Val;
	false ->
	    exit({not_found, S, Stats})
    end.
    
traffic_verify_counter(Name, Counter, Counters, Expected) ->
    case lists:keysearch(Counter, 1, Counters) of
	{value, {Counter, Expected}} ->
            i("counter ~w verified for ~p", [Counter, Name]),
	    ok;
	{value, {Counter, Val}} ->
            i("counter ~w *not* verified for ~p: "
              "~n   Expected: ~w"
              "~n   Actual:   ~w", [Counter, Name, Expected, Val]),
	    exit({illegal_counter_value, Counter, Val, Expected, Name});
	false ->
            i("counter ~w *not* found for ~p", [Counter, Name]),
	    exit({not_found, Counter, Counters, Name, Expected})
    end.
    

traffic_connect_mg([], Acc) ->
    lists:reverse(Acc);
traffic_connect_mg([{Node, Name, Coding, Trans}|Mg], Acc) ->
    Pid = traffic_connect_mg(Node, Name, Coding, Trans),
    traffic_connect_mg(Mg, [{Name, Pid}|Acc]).

traffic_connect_mg(Node, Name, Coding, Trans) ->
    Mid = {deviceName, Name}, 
    {ok, Pid} = start_mg(Node, Mid, Coding, Trans, ?MG_VERBOSITY),

    %% Ask the MGs to do a service change
    {ok, Res} = service_change(Pid),
    d("traffic_connect_mg -> (~s) service change result: ~p", [Name, Res]),
    Pid.


traffic_stop_mg(MGs) ->
    [stop(Pid) || {_Name, Pid} <- MGs].


traffic_get_mg_stats([], Acc) ->
    lists:reverse(Acc);
traffic_get_mg_stats([{Name, Pid}|Mgs], Acc) ->
    {ok, Stats} = get_stats(Pid, 1),
    d("traffic_get_mg_stats -> stats for ~s: "
      "~n   ~p"
      "~n", [Name, Stats]),
    traffic_get_mg_stats(Mgs, [{Name, Stats}|Acc]).


traffic_apply_load([]) ->
    ok;
traffic_apply_load([{_,MG}|MGs]) ->
    MG ! {apply_load, self(), ?LOAD_COUNTER_START},
    receive
	{apply_load_ack, MG} ->
	    traffic_apply_load(MGs);
	{'EXIT', MG, Reason} ->
	    exit({mg_exit, MG, Reason})
    after 10000 ->
	    exit({apply_load_ack_timeout, MG})
    end.


traffic_reset_mg_stats([]) ->
    ok;
traffic_reset_mg_stats([{Name, Pid}|MGs]) ->
    d("traffic_reset_mg_stats -> resetting ~s", [Name]),
    traffic_reset_stats(Pid),
    traffic_reset_mg_stats(MGs).

traffic_reset_mgc_stats(Mgc) ->
    d("traffic_reset_mgc_stats -> resetting ~p", [Mgc]),
    traffic_reset_stats(Mgc).
    
traffic_reset_stats(Pid) ->
    Pid ! {reset_stats, self()},
    receive
	{reset_stats_ack, Pid} ->
	    ok;
	{'EXIT', Pid, Reason} ->
	    exit({client_exit, Pid, Reason})
    after 10000 ->
	    exit({reset_stats_ack_timeout, Pid})
    end.

    
traffic_await_load_complete([]) ->
    ok;
traffic_await_load_complete(MGs0) ->
    receive
	{load_complete, Pid} ->
	    d("received load_complete from ~p", [Pid]),
	    MGs1 = lists:keydelete(Pid, 2, MGs0),
	    traffic_await_load_complete(lists:delete(Pid, MGs1));
	{'EXIT', Pid, Reason} ->
	    i("exit signal from ~p: ~p", [Pid, Reason]),
	    case lists:keymember(Pid, 2, MGs0) of
		true ->
		    exit({mg_exit, Pid, Reason});
		false ->
		    MGs1 = lists:keydelete(Pid, 2, MGs0),
		    traffic_await_load_complete(lists:delete(Pid, MGs1))
	    end
    end.
	    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_node_name(Name) ->
    case string:tokens(atom_to_list(node()), [$@]) of
	[_,Host] ->
	    list_to_atom(lists:concat([atom_to_list(Name) ++ "@" ++ Host]));
	_ ->
	    exit("Test node must be started with '-sname'")
     end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_mgc(Node, Mid, ET, Verbosity) ->
    d("start mgc[~p]: ~p", [Node, Mid]),
    RI = {receive_info, mk_recv_info(ET)},
    Config = [{local_mid, Mid}, RI],
    Pid = spawn_link(Node, ?MODULE, mgc, [self(), Verbosity, Config]),
    await_started(Pid).

mk_recv_info(ET) ->
    mk_recv_info(ET, []).

mk_recv_info([], Acc) ->
    Acc;
mk_recv_info([{text,tcp}|ET], Acc) ->
    RI = [{encoding_module, megaco_pretty_text_encoder},
	  {encoding_config, []},
	  {transport_module, megaco_tcp},
	  {port, 2944}],
    mk_recv_info(ET, [RI|Acc]);
mk_recv_info([{text,udp}|ET], Acc) ->
    RI = [{encoding_module, megaco_pretty_text_encoder},
	  {encoding_config, []},
	  {transport_module, megaco_udp},
	  {port, 2944}],
    mk_recv_info(ET, [RI|Acc]);
mk_recv_info([{binary,tcp}|ET], Acc) ->
    RI = [{encoding_module, megaco_ber_encoder},
	  {encoding_config, []},
	  {transport_module, megaco_tcp},
	  {port, 2945}],
    mk_recv_info(ET, [RI|Acc]);
mk_recv_info([{binary,udp}|ET], Acc) ->
    RI = [{encoding_module, megaco_ber_encoder},
	  {encoding_config, []},
	  {transport_module, megaco_udp},
	  {port, 2945}],
    mk_recv_info(ET, [RI|Acc]);
mk_recv_info([ET|_], _) ->
    throw({error, {invaalid_encoding_transport, ET}}).

mgc(Parent, Verbosity, Config) ->
    process_flag(trap_exit, true),
    put(verbosity, Verbosity),
    put(sname,   "MGC"),
    i("mgc -> starting"),
    {Mid, TcpSup, UdpSup} = mgc_init(Config),
    notify_started(Parent),
    S = #mgc{parent = Parent, 
	     tcp_sup = TcpSup, udp_sup = UdpSup, mid = Mid},
    i("mgc -> started"),
    mgc_loop(S).

mgc_init(Config) ->
    d("mgc_init -> entry"),
    Mid = get_conf(local_mid, Config),
    RI  = get_conf(receive_info, Config),
    i("mgc_init -> start megaco"),
    application:start(megaco),
    d("mgc_init -> start megaco user"),
    megaco:start_user(Mid, []),
    d("mgc_init -> update user info (user_mod)"),
    megaco:update_user_info(Mid, user_mod,  ?MODULE),
    d("mgc_init -> update user info (user_args)"),
    megaco:update_user_info(Mid, user_args, [self()]),
    d("mgc_init -> get user info (receive_handle)"),
    RH = megaco:user_info(Mid,receive_handle),
    d("mgc_init -> parse receive info"),
    ListenTo = mgc_parse_receive_info(RI, RH),
    i("mgc_init -> start transport(s)"),
    {Tcp, Udp} = mgc_start_transports(ListenTo),
    {Mid, Tcp, Udp}.
    

mgc_loop(S) ->
    d("mgc_loop -> await request"),
    receive
	{stop, Parent} when S#mgc.parent == Parent ->
	    i("mgc_loop -> stopping", []),
  	    Mid = S#mgc.mid,
	    (catch mgc_close_conns(Mid)),
	    megaco:stop_user(Mid),
	    application:stop(megaco),
	    i("mgc_loop -> stopped", []),
	    Parent ! {stopped, self()},
	    exit(normal);



	%% Reset stats
	{reset_stats, Parent} when S#mgc.parent == Parent ->
	    i("mgc_loop -> got request to reset stats counters"),
	    mgc_reset_stats(S#mgc.mid),
	    Parent ! {reset_stats_ack, self()},
	    mgc_loop(S);


	%% Give me statistics
	{statistics, 1, Parent} when S#mgc.parent == Parent ->
	    i("mgc_loop -> got request for statistics 1"),
	    {ok, Gen} = megaco:get_stats(),
	    GetTrans = 
		fun(CH) ->
			Reason = {statistics, CH}, 
			Pid = megaco:conn_info(CH, control_pid),
			SendMod = megaco:conn_info(CH, send_mod),
			SendHandle = megaco:conn_info(CH, send_handle),
			{ok, Stats} = 
			    case SendMod of
				megaco_tcp -> megaco_tcp:get_stats(SendHandle);
				megaco_udp -> megaco_udp:get_stats(SendHandle);
				SendMod    -> exit(Pid, Reason)
			    end,
			{SendHandle, Stats}
		end,
	    Mid = S#mgc.mid,
	    Trans = 
		lists:map(GetTrans, megaco:user_info(Mid, connections)),
	    Parent ! {statistics, 1, [{gen, Gen}, {trans, Trans}], self()},
	    mgc_loop(S);


	{statistics, 2, Parent} when S#mgc.parent == Parent ->
	    i("mgc_loop -> got request for statistics 2"),
	    {ok, Gen} = megaco:get_stats(),
	    #mgc{tcp_sup = TcpSup, udp_sup = UdpSup} = S,
	    TcpStats = get_trans_stats(TcpSup, megaco_tcp),
	    UdpStats = get_trans_stats(UdpSup, megaco_udp),
	    Parent ! {statistics, 2, [{gen, Gen}, {trans, [TcpStats, UdpStats]}], self()},
	    mgc_loop(S);


	%% Megaco callback messages
	{request, Request, From} ->
	    d("mgc_loop -> received megaco request: ~n~p~n   From: ~p", 
	      [Request, From]),
	    Reply = mgc_handle_request(Request),
	    d("mgc_loop -> send request reply: ~n~p", [Reply]),
	    From ! {reply, Reply, self()},
	    mgc_loop(S);


	{'EXIT', Pid, Reason} ->
	    error_msg("MGC received unexpected exit signal from ~p:~n~p", 
		      [Pid, Reason]),
	    mgc_loop(S);


	Invalid ->
	    i("mgc_loop -> received invalid request: ~p", [Invalid]),
	    mgc_loop(S)
    end.


mgc_reset_stats(Mid) ->
    megaco:reset_stats(),
    mgc_reset_trans_stats(megaco:user_info(Mid, connections), []).

mgc_reset_trans_stats([], _Reset) ->
    ok;
mgc_reset_trans_stats([CH|CHs], Reset) ->
    SendMod = megaco:conn_info(CH, send_mod),
    case lists:member(SendMod, Reset) of
	true ->
	    mgc_reset_trans_stats(CHs, Reset);
	false ->
	    SendMod:reset_stats(),
	    mgc_reset_trans_stats(CHs, [SendMod|Reset])
    end.
	    

mgc_close_conns(Mid) ->
    Reason = {self(), ignore},
    Disco  = fun(CH) ->
		     (catch mgc_close_conn(CH, Reason))
	     end,
    lists:map(Disco, megaco:user_info(Mid, connections)).

mgc_close_conn(CH, Reason) ->
    d("close connection to ~p", [CH#megaco_conn_handle.remote_mid]),
    Pid        = megaco:conn_info(CH, control_pid),
    SendMod    = megaco:conn_info(CH, send_mod),
    SendHandle = megaco:conn_info(CH, send_handle),
    megaco:disconnect(CH, Reason),
    case SendMod of
	megaco_tcp -> megaco_tcp:close(SendHandle);
	megaco_udp -> megaco_udp:close(SendHandle);
	SendMod    -> exit(Pid, Reason)
    end.
    
get_trans_stats(P, SendMod) when is_pid(P) ->
    case (catch SendMod:get_stats()) of
	{ok, Stats} ->
	    {SendMod, Stats};
	Else ->
	    {SendMod, Else}
    end;
get_trans_stats(_P, SendMod) ->
    {SendMod, undefined}.

mgc_parse_receive_info([], _RH) ->
    throw({error, no_receive_info});
mgc_parse_receive_info(RI, RH) ->
    mgc_parse_receive_info(RI, RH, []).

mgc_parse_receive_info([], _RH, ListenTo) ->
    ListenTo;
mgc_parse_receive_info([RI|RIs], RH, ListenTo) ->
    d("mgc_parse_receive_info -> parse receive info"),
    RH1 = mgc_parse_receive_info1(RI, RH),
    case (catch mgc_parse_receive_info1(RI, RH)) of
	{error, Reason} ->
	    i("failed parsing receive info: ~p~n~p", [RI, Reason]),
	    exit({failed_parsing_recv_info, RI, Reason});
	RH1 ->
	    mgc_parse_receive_info(RIs, RH, [RH1|ListenTo])
    end.
    
mgc_parse_receive_info1(RI, RH) ->
    d("mgc_parse_receive_info1 -> get encoding module"),
    EM = get_encoding_module(RI),
    d("mgc_parse_receive_info1 -> get encoding config"),
    EC = get_encoding_config(RI, EM),
    d("mgc_parse_receive_info1 -> get transport module"),
    TM = get_transport_module(RI),
    d("mgc_parse_receive_info1 -> get transport port"),
    TP = get_transport_port(RI),
    RH1 = RH#megaco_receive_handle{send_mod        = TM,
				   encoding_mod    = EM,
				   encoding_config = EC},
    {TP, RH1}.


mgc_start_transports([]) ->
    throw({error, no_transport});
mgc_start_transports(ListenTo) ->
    mgc_start_transports(ListenTo, undefined, undefined).
    

mgc_start_transports([], TcpSup, UdpSup) ->
    {TcpSup, UdpSup};
mgc_start_transports([{Port, RH}|ListenTo], TcpSup, UdpSup) 
  when RH#megaco_receive_handle.send_mod == megaco_tcp ->
    TcpSup1 = mgc_start_tcp(RH, Port, TcpSup),
    mgc_start_transports(ListenTo, TcpSup1, UdpSup);
mgc_start_transports([{Port, RH}|ListenTo], TcpSup, UdpSup) 
  when RH#megaco_receive_handle.send_mod == megaco_udp ->
    UdpSup1 = mgc_start_udp(RH, Port, UdpSup),
    mgc_start_transports(ListenTo, TcpSup, UdpSup1);
mgc_start_transports([{_Port, RH}|_ListenTo], _TcpSup, _UdpSup) ->
    throw({error, {bad_send_mod, RH#megaco_receive_handle.send_mod}}).


mgc_start_tcp(RH, Port, undefined) ->
    i("start tcp transport"),
    case megaco_tcp:start_transport() of
	{ok, Sup} ->
	    mgc_start_tcp(RH, Port, Sup);
	Else ->
	    throw({error, {failed_starting_tcp_transport, Else}})
    end;
mgc_start_tcp(RH, Port, Sup) when is_pid(Sup) ->
    i("tcp listen on ~p", [Port]),
    Opts = [{port,           Port}, 
	    {receive_handle, RH}, 
	    {tcp_options,    [{nodelay, true}]}],
    mgc_tcp_create_listen(Sup, Opts, 3).

mgc_tcp_create_listen(Sup, Opts, N) ->
    mgc_tcp_create_listen(Sup, Opts, N, 1, undefined).

mgc_tcp_create_listen(_Sup, _Opts, N, N, InitialReason) ->
    d("failed creating mgc tcp listen socket after ~p tries: ~p", 
      [N, InitialReason]),
    throw({error, {failed_starting_tcp_listen, InitialReason}});
mgc_tcp_create_listen(Sup, Opts, MaxN, N, _InitialReason) 
  when is_integer(N) andalso is_integer(MaxN) andalso (MaxN > N) ->
    d("try create mgc tcp listen socket [~w]", [N]),
    case megaco_tcp:listen(Sup, Opts) of
	ok ->
	    Sup;
	{error, {could_not_start_listener, {gen_tcp_listen, eaddrinuse} = Reason}} ->
	    ?SLEEP(N * 200),
	    mgc_tcp_create_listen(Sup, Opts, MaxN, N + 1, Reason);
	{error, Reason} ->
	    throw({error, {failed_starting_tcp_listen, Reason}});
	Else ->
	    throw({error, {failed_starting_tcp_listen, Else}})
    end.


mgc_start_udp(RH, Port, undefined) ->
    i("start udp transport"),
    case megaco_udp:start_transport() of
	{ok, Sup} ->
	    mgc_start_udp(RH, Port, Sup);
	Else ->
	    throw({error, {failed_starting_udp_transport, Else}})
    end;
mgc_start_udp(RH, Port, Sup) ->
    i("open udp ~p", [Port]),
    Opts = [{port, Port}, {receive_handle, RH}],
    case megaco_udp:open(Sup, Opts) of
	{ok, _SendHandle, _ControlPid} ->
	    Sup;
	Else ->
	    exit({error, {failed_starting_udp_listen, Else}})
    end.



%% -----------------------
%% Handle megaco callbacks
%%

mgc_handle_request({handle_connect, _CH, _PV}) ->
    ok;
mgc_handle_request({handle_disconnect, CH, _PV, R}) ->
    megaco:cancel(CH, R), % Cancel the outstanding messages
    ok;
mgc_handle_request({handle_syntax_error, _RH, _PV, _ED}) ->
    reply;
mgc_handle_request({handle_message_error, _CH, _PV, _ED}) ->
    no_reply;
mgc_handle_request({handle_trans_request, CH, PV, ARs}) ->
    ED =  #'ErrorDescriptor'{errorCode = ?megaco_not_implemented,
                             errorText = "Only single service change on null context handled"},
    case ARs of
        [AR] ->
            ContextId = AR#'ActionRequest'.contextId,
            case AR#'ActionRequest'.commandRequests of
                [CR] when ContextId == ?megaco_null_context_id ->
                    case CR#'CommandRequest'.command of
                        {serviceChangeReq, Req} ->
                            Rep    = mgc_service_change(CH, PV, Req),
			    CmdRep = [{serviceChangeReply, Rep}],
                            {discard_ack,
                             [#'ActionReply'{contextId    = ContextId,
                                             commandReply = CmdRep}]};
                        _ ->
                            {discard_ack, ED}
                    end;
                _ ->
                    {discard_ack, ED}
            end;
        _ ->
            {discard_ack, ED}
    end;
mgc_handle_request({handle_trans_long_request, _CH, _PV, _RD}) ->
    ED = #'ErrorDescriptor'{errorCode = ?megaco_not_implemented,
                            errorText = "Long transaction requests not handled"},
    {discard_ack,  ED};
mgc_handle_request({handle_trans_reply, _CH, _PV, _AR, _RD}) ->
    ok;
mgc_handle_request({handle_trans_ack, _CH, _PV, _AS, _AD}) ->
    ok.

mgc_service_change(CH, _PV, SCR) ->
    SCP = SCR#'ServiceChangeRequest'.serviceChangeParms,
    #'ServiceChangeParm'{serviceChangeAddress = Address,
                         serviceChangeProfile = Profile,
                         serviceChangeReason  = [_Reason]} = SCP,
    TermId = SCR#'ServiceChangeRequest'.terminationID,
    if
        TermId == [?megaco_root_termination_id] ->
            MyMid = CH#megaco_conn_handle.local_mid,
            Res = {serviceChangeResParms,
                   #'ServiceChangeResParm'{serviceChangeMgcId   = MyMid,
                                           serviceChangeAddress = Address,
                                           serviceChangeProfile = Profile}},
            #'ServiceChangeReply'{terminationID       = TermId,
                                  serviceChangeResult = Res};
        true ->
            Res = {errorDescriptor,
                   #'ErrorDescriptor'{errorCode = ?megaco_not_implemented,
                                      errorText = "Only handled for root"}},
            
             #'ServiceChangeReply'{terminationID       = TermId,
                                   serviceChangeResult = Res}
    end.

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_mg(Node, Mid, Encoding, Transport, Verbosity) ->
    d("start mg[~p]: ~p", [Node, Mid]),
    RI1 = 
	case Encoding of 
	    text ->
		[{encoding_module, megaco_pretty_text_encoder},
		 {encoding_config, []},
		 {port,2944}];
	    binary ->
		[{encoding_module, megaco_ber_encoder},
		 {encoding_config, []},
		 {port,2945}]
	end,
    RI2 = 
	case Transport of
	    tcp ->
		[{transport_module, megaco_tcp}];
	    udp ->
		[{transport_module, megaco_udp}]
	end,
    RI = {receive_info, RI1 ++ RI2},
    Config = [{local_mid, Mid}, RI],
    Pid = spawn_link(Node, ?MODULE, mg, [self(), Verbosity, Config]),
    await_started(Pid).


mg(Parent, Verbosity, Config) ->
    process_flag(trap_exit, true),
    put(verbosity, Verbosity),
    put(sname,   "MG"),
    i("mg -> starting"),
    {Mid, ConnHandle} = mg_init(Config),
    notify_started(Parent),
    S = #mg{parent = Parent, mid = Mid, conn_handle = ConnHandle},
    i("mg -> started"),
    mg_loop(S).

mg_init(Config) ->
    d("mg_init -> entry"),
    Mid = get_conf(local_mid, Config),
    RI  = get_conf(receive_info, Config),
    i("mg_init -> start megaco"),
    application:start(megaco),
    d("mg_init -> start megaco user"),
    megaco:start_user(Mid, []),
    d("mg_init -> update user info (user_mod)"),
    megaco:update_user_info(Mid, user_mod,  ?MODULE),
    d("mg_init -> update user info (user_args)"),
    megaco:update_user_info(Mid, user_args, [self()]),
    d("mg_init -> get user info (receive_handle)"),
    RH = megaco:user_info(Mid,receive_handle),
    d("mg_init -> parse receive info"),
    {MgcPort,RH1} = mg_parse_receive_info(RI, RH),
    i("mg_init -> start transport(s)"),
    ConnHandle = mg_start_transport(MgcPort, RH1),
    {Mid, ConnHandle}.

mg_loop(#mg{state = State} = S) ->
    d("mg_loop(~p) -> await request", [State]),
    receive
	{stop, Parent} when S#mg.parent == Parent ->
	    i("mg_loop(~p) -> stopping", [State]),
	    mg_close_conn(S#mg.conn_handle),
	    megaco:stop_user(S#mg.mid),
	    application:stop(megaco),
	    i("mg_loop(~p) -> stopped", [State]),
	    Parent ! {stopped, self()},
	    exit(normal);


	{reset_stats, Parent} when S#mg.parent == Parent ->
	    i("mg_loop(~p) -> got request to reset stats counters", [State]),
	    %% mg_reset_stats(S#mgc.conn_handle),
	    mg_reset_stats(S#mg.conn_handle),
	    Parent ! {reset_stats_ack, self()},
	    mg_loop(S);



	%% Give me statistics
	{statistics, 1, Parent} when S#mg.parent == Parent ->
	    i("mg_loop(~p) -> got request for statistics 1", [State]),
	    {ok, Gen}   = megaco:get_stats(),
	    CH          = S#mg.conn_handle,
	    Reason      = {statistics, CH}, 
	    Pid         = megaco:conn_info(CH, control_pid),
	    SendMod     = megaco:conn_info(CH, send_mod),
	    SendHandle  = megaco:conn_info(CH, send_handle),
	    {ok, Trans} = 
		case SendMod of
		    megaco_tcp -> megaco_tcp:get_stats(SendHandle);
		    megaco_udp -> megaco_udp:get_stats(SendHandle);
		    SendMod    -> exit(Pid, Reason)
		end,
	    Parent ! {statistics, 1, [{gen, Gen}, {trans, Trans}], self()},
	    mg_loop(S); 


	%% Do a service change
	{service_change, Parent} when S#mg.parent == Parent, 
				      State == initiated ->
	    i("mg_loop(~p) -> received request to perform service change", 
	      [State]),
	    Res = mg_service_change(S#mg.conn_handle),
	    d("mg_loop(~p) -> result: ~p", [State, Res]),
	    mg_loop(S#mg{state = connecting}); 


	%% Apply some load
	{apply_load, Parent, Times} when S#mg.parent == Parent ->
	    i("mg_loop(~p) -> received apply_load request", [State]),
	    apply_load_timer(),
	    Parent ! {apply_load_ack, self()},
	    mg_loop(S#mg{load_counter = Times - 1});


	apply_load_timeout ->
	    d("mg_loop(~p) -> received apply_load timeout [~p]", 
	      [State, S#mg.load_counter]),
	    mg_apply_load(S),
	    mg_loop(S);


	%% Megaco callback messages
	{request, Request, From} ->
	    d("mg_loop(~p) -> received megaco request: ~n~p~n   From: ~p", 
	      [State, Request, From]),
	    {Reply, NewS} = mg_handle_request(Request, S),
	    d("mg_loop(~p) -> send request reply: ~n~p", 
	      [NewS#mg.state, Reply]),
	    From ! {reply, Reply, self()},
	    mg_loop(NewS);


	{'EXIT', Pid, Reason} ->
	    error_msg("MG ~p received unexpected exit signal from ~p:~n~p", 
		      [S#mg.mid, Pid, Reason]),
	    mg_loop(S);


	Invalid ->
	    i("mg_loop(~p) -> received invalid request: ~p", [State, Invalid]),
	    mg_loop(S)

    end.


mg_reset_stats(CH) ->
    megaco:reset_stats(),
    case (catch megaco:conn_info(CH, send_mod)) of
	{error, Reason} ->
	    error_msg("unexpected result when retrieving send module for "
		      "own connection ~p: ~p. "
		      "~nexiting...", [CH, Reason]),
	    exit({invalid_connection, CH, Reason});
	{'EXIT', Reason} ->
	    error_msg("exit signal when retrieving send module for "
		      "own connection ~p: ~p. "
		      "~nexiting...", [CH, Reason]),
	    exit({invalid_connection, CH, Reason});
	SendMod when is_atom(SendMod) ->
	    SendMod:reset_stats()
    end.


mg_close_conn(CH) ->
    Reason = {self(), ignore},
    Pid        = megaco:conn_info(CH, control_pid),
    SendMod    = megaco:conn_info(CH, send_mod),
    SendHandle = megaco:conn_info(CH, send_handle),
    megaco:disconnect(CH, Reason),
    case SendMod of
	megaco_tcp -> megaco_tcp:close(SendHandle);
	megaco_udp -> megaco_udp:close(SendHandle);
	SendMod    -> exit(Pid, Reason)
    end.


mg_parse_receive_info(RI, RH) ->
    d("mg_parse_receive_info -> get encoding module"),
    EM = get_encoding_module(RI),
    d("mg_parse_receive_info -> get encoding config"),
    EC = get_encoding_config(RI, EM),
    d("mg_parse_receive_info -> get transport module"),
    TM = get_transport_module(RI),
    d("mg_parse_receive_info -> get transport port"),
    TP = get_transport_port(RI),
    RH1 = RH#megaco_receive_handle{send_mod        = TM,
				   encoding_mod    = EM,
				   encoding_config = EC},
    {TP, RH1}.


mg_start_transport(MgcPort,
		   #megaco_receive_handle{send_mod = megaco_tcp} = RH) ->
    mg_start_tcp(MgcPort,RH);
mg_start_transport(MgcPort,
		   #megaco_receive_handle{send_mod = megaco_udp} = RH) ->
    mg_start_udp(MgcPort,RH);
mg_start_transport(_, #megaco_receive_handle{send_mod = Mod}) ->
    throw({error, {bad_send_mod, Mod}}).


mg_start_tcp(MgcPort, RH) ->
    i("start tcp transport"),
    case megaco_tcp:start_transport() of
	{ok, Sup} ->
	    {ok, LocalHost} = inet:gethostname(),
	    Opts = [{host,           LocalHost},
		    {port,           MgcPort}, 
		    {receive_handle, RH}, 
		    {tcp_options,    [{nodelay, true}]}],
	    case megaco_tcp:connect(Sup, Opts) of
		{ok, SendHandle, ControlPid} ->
                    PrelMgcMid = preliminary_mid,
		    {ok, ConnHandle} = 
			megaco:connect(RH, PrelMgcMid, 
				       SendHandle, ControlPid),
		    ConnHandle;
		{error, Reason} ->
		    {error, {megaco_tcp_connect, Reason}}
	    end;
        {error, Reason} ->
            {error, {megaco_tcp_start_transport, Reason}}
    end.


mg_start_udp(MgcPort, RH) ->
    i("start udp transport"),
    case megaco_udp:start_transport() of
	{ok, Sup} ->
            %% Some linux (Ubuntu) has "crap" in their /etc/hosts, that 
            %% causes problem for us in this case (UDP). So we can't use
            %% local host. Try instead to "figure out" tha actual address...
            LocalAddr = which_local_addr(),
	    Opts = [{port, 0}, {receive_handle, RH}],
	    case megaco_udp:open(Sup, Opts) of
		{ok, Handle, ControlPid} ->
                    MgcMid = preliminary_mid,
                    SendHandle = megaco_udp:create_send_handle(Handle, 
							       LocalAddr, 
							       MgcPort),
		    {ok, ConnHandle} = 
			megaco:connect(RH, MgcMid, 
				       SendHandle, ControlPid),
		    ConnHandle;
		{error, Reason} ->
                    {error, {megaco_udp_open, Reason}}
	    end;
        {error, Reason} ->
            {error, {megaco_udp_start_transport, Reason}}
    end.


mg_service_change(ConnHandle) ->
    mg_service_change(ConnHandle, restart, ?megaco_cold_boot).

mg_service_change(ConnHandle, Method, Reason) ->
    SCP = #'ServiceChangeParm'{serviceChangeMethod = Method,
                               serviceChangeReason = [Reason]},
    TermId = [?megaco_root_termination_id],
    SCR = #'ServiceChangeRequest'{terminationID = TermId,
                                  serviceChangeParms = SCP},
    CR = #'CommandRequest'{command = {serviceChangeReq, SCR}},
    AR = #'ActionRequest'{contextId = ?megaco_null_context_id,
                          commandRequests = [CR]},
    megaco:cast(ConnHandle, [AR], []).


mg_notify_request(CH) ->
    TimeStamp = cre_timeNotation("19990729", "22000000"),
    Event     = cre_observedEvent("al/of",TimeStamp),
    Desc      = cre_observedEventsDesc(2222,[Event]),
    NotifyReq = cre_notifyReq([#megaco_term_id{id = ?A4444}],Desc),
    CmdReq    = cre_commandReq({notifyReq, NotifyReq}),
    ActReq    = cre_actionReq(?megaco_null_context_id, [CmdReq]),
    megaco:cast(CH, [ActReq], [{reply_data, Desc}]).
    
mg_apply_load(#mg{conn_handle = CH}) ->
    mg_notify_request(CH).


cre_actionReq(Cid, Cmds) ->
    #'ActionRequest'{contextId = Cid,
		     commandRequests = Cmds}.

cre_commandReq(Cmd) ->
    #'CommandRequest'{command = Cmd}.

cre_notifyReq(Tid, EvsDesc) ->
    #'NotifyRequest'{terminationID = Tid, observedEventsDescriptor = EvsDesc}.

cre_observedEventsDesc(Id, EvList) ->
    #'ObservedEventsDescriptor'{requestId = Id, observedEventLst = EvList}.

cre_observedEvent(Name, Not) ->
    #'ObservedEvent'{eventName = Name, timeNotation = Not}.

cre_timeNotation(D,T) ->
    #'TimeNotation'{date = D, time = T}.

%% -----------------------
%% Handle megaco callbacks
%%

mg_handle_request({handle_connect, CH, _PV}, 
		  #mg{state = connecting} = S) ->
    {ok, S#mg{conn_handle = CH}};

mg_handle_request({handle_disconnect, CH, _PV, _R}, S) ->
    {ok, S#mg{conn_handle = CH}};

mg_handle_request({handle_syntax_error, _RH, _PV, _ED}, S) ->
    {reply, S};

mg_handle_request({handle_message_error, CH, _PV, _ED}, S) ->
    {no_reply, S#mg{conn_handle = CH}};

mg_handle_request({handle_trans_request, CH, _PV, _AR}, S) ->
    ED =  #'ErrorDescriptor'{errorCode = ?megaco_not_implemented,
                             errorText = "Transaction requests not handled"},
    {{discard_ack, ED}, S#mg{conn_handle = CH}};

mg_handle_request({handle_trans_long_request, CH, _PV, _RD}, S) ->
    ED = #'ErrorDescriptor'{errorCode = ?megaco_not_implemented,
                            errorText = "Long transaction requests not handled"},
    {{discard_ack,  ED}, S#mg{conn_handle = CH}};

mg_handle_request({handle_trans_reply, CH, _PV, _AR, _RD}, 
		  #mg{parent = Pid, state = connecting} = S) ->
    %% Should really check this...
    Pid ! {service_change_reply, ok, self()},
    {ok, S#mg{conn_handle = CH, state = connected}};
mg_handle_request({handle_trans_reply, _CH, _PV, {error, ED}, RD}, 
		  #mg{parent = Pid, load_counter = 0} = S) 
  when is_record(ED, 'ErrorDescriptor') andalso 
       is_record(RD, 'ObservedEventsDescriptor') ->
    Pid ! {load_complete, self()},
    {ok, S};
mg_handle_request({handle_trans_reply, _CH, _PV, {error, ED}, RD}, 
		  #mg{load_counter = N} = S) 
  when is_record(ED, 'ErrorDescriptor') andalso 
       is_record(RD, 'ObservedEventsDescriptor') ->
    apply_load_timer(),
    {ok, S#mg{load_counter = N-1}};

mg_handle_request({handle_trans_ack, _CH, _PV, _AS, _AD}, S) ->
    {ok, S}.

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

await_started(Pid) ->
    receive
	{started, Pid} ->
	    d("await_started ~p: ok", [Pid]),
	    {ok, Pid};
	{'EXIT', Pid, Reason} ->
	    i("await_started ~p: received exit signal: ~p", [Pid, Reason]),
	    exit({failed_starting, Pid, Reason})
    after 10000 ->
	    i("await_started ~p: timeout", [Pid]),
	    exit({error, timeout})
    end.

stop(Pid) ->
    d("stop ~p", [Pid]),
    Pid ! {stop, self()},
    receive 
	{stopped, Pid} ->
	    d("stop -> received stopped from ~p", [Pid]),
	    ok;
	{'EXIT', Pid, Reason} ->
	    i("stop ~p: received exit signal: ~p", [Pid, Reason]),
	    exit({failed_stopping, Pid, Reason})
    after 10000 ->
	    exit({error, timeout})
    end.

get_stats(Pid, No) ->
    d("get_stats ~p", [Pid]),
    Pid ! {statistics, No, self()},
    receive 
	{statistics, No, Stats, Pid} ->
	    {ok, Stats};
	{'EXIT', Pid, Reason} ->
	    i("get_stats ~p: received exit signal: ~p", [Pid, Reason]),
	    exit({failed_getting_stats, Pid, Reason})
    after 10000 ->
	    exit({error, timeout})
    end.

service_change(Pid) ->
    d("service_change ~p", [Pid]),
    Pid ! {service_change, self()},
    receive 
	{service_change_reply, Res, Pid} ->
	    {ok, Res};
	{'EXIT', Pid, Reason} ->
	    i("service_change ~p: received exit signal: ~p", [Pid, Reason]),
	    exit({failed_service_change, Pid, Reason})
    after 10000 ->
	    exit({error, timeout})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

notify_started(Parent) ->
    Parent ! {started, self()}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The megaco user callback interface 

handle_connect(CH, PV, Pid) ->
%     i("handle_connect -> entry with"
%       "~n    CH:  ~p"
%       "~n    PV:  ~p"
%       "~n    Pid: ~p", [CH, PV, Pid]),
    case CH#megaco_conn_handle.remote_mid of
        preliminary_mid ->
	    %% Avoids deadlock
	    ok;
	_ ->
	    Reply = request(Pid, {handle_connect, CH, PV}),
% 	    d("handle_connect -> Reply:~n~p", [Reply]),
	    Reply
    end.

handle_disconnect(_CH, _PV, 
		  {user_disconnect, {Pid, ignore}}, 
		  Pid) ->
%     i("handle_disconnect(ignore) -> entry with"
%       "~n   CH: ~p"
%       "~n   PV: ~p", [CH, PV]),
    %% Avoids deadlock
    ok;
handle_disconnect(CH, PV, R, Pid) ->
%     i("handle_disconnect -> entry with"
%       "~n   CH: ~p"
%       "~n   PV: ~p"
%       "~n   R:  ~p", [CH, PV, R]),
    request(Pid, {handle_disconnect, CH, PV, R}).

handle_syntax_error(ReceiveHandle, ProtocolVersion, ErrorDescriptor, Pid) ->
%     i("handle_syntax_error -> entry with"
%       "~n   ReceiveHandle:   ~p"
%       "~n   ProtocolVersion: ~p"
%       "~n   ErrorDescriptor: ~p", 
%       [ReceiveHandle, ProtocolVersion, ErrorDescriptor]),
    Req = {handle_syntax_error, ReceiveHandle, ProtocolVersion, 
	   ErrorDescriptor},
    request(Pid, Req).
    
handle_message_error(ConnHandle, ProtocolVersion, ErrorDescriptor, Pid) ->
%     i("handle_message_error -> entry with"
%       "~n   ConnHandle:      ~p"
%       "~n   ProtocolVersion: ~p"
%       "~n   ErrorDescriptor: ~p", 
%       [ConnHandle, ProtocolVersion, ErrorDescriptor]),
    Req = {handle_message_error, ConnHandle, ProtocolVersion, ErrorDescriptor},
    request(Pid, Req).

handle_trans_request(CH, PV, AR, Pid) ->
%     i("handle_trans_request -> entry with" 
%       "~n   CH:  ~p"
%       "~n   PV:  ~p"
%       "~n   AR:  ~p"
%       "~n   Pid: ~p", [CH, PV, AR, Pid]),
    Reply = request(Pid, {handle_trans_request, CH, PV, AR}),
%     i("handle_trans_request -> Reply:~n~p", [Reply]),
    Reply.

handle_trans_long_request(ConnHandle, ProtocolVersion, ReqData, Pid) ->
%     i("handle_trans_long_request -> entry with"
%       "~n   ConnHandle:      ~p"
%       "~n   ProtocolVersion: ~p"
%       "~n   ReqData:         ~p", [ConnHandle, ProtocolVersion, ReqData]),
    Req = {handle_trans_long_request, ConnHandle, ProtocolVersion, ReqData},
    request(Pid, Req).

handle_trans_reply(ConnHandle, ProtocolVersion, ActualReply, ReplyData, Pid) ->
%     i("handle_trans_reply -> entry with"
%       "~n   ConnHandle:      ~p"
%       "~n   ProtocolVersion: ~p"
%       "~n   ActualReply:     ~p"
%       "~n   ReplyData:       ~p", 
%       [ConnHandle, ProtocolVersion, ActualReply, ReplyData]),
    Req = {handle_trans_reply, ConnHandle, ProtocolVersion, 
	   ActualReply, ReplyData},
    request(Pid, Req).

handle_trans_ack(ConnHandle, ProtocolVersion, AckStatus, AckData, Pid) ->
%     i("handle_trans_ack -> entry with"
%       "~n   ConnHandle:      ~p"
%       "~n   ProtocolVersion: ~p"
%       "~n   AckStatus:       ~p"
%       "~n   AckData:         ~p", 
%       [ConnHandle, ProtocolVersion, AckStatus, AckData]),
    Req = {handle_trans_ack, ConnHandle, ProtocolVersion, AckStatus, AckData},
    request(Pid, Req).


request(Pid, Request) ->
    Pid ! {request, Request, self()},
    receive
	{reply, Reply, Pid} ->
	    Reply
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

error_msg(F,A) -> error_logger:error_msg(F ++ "~n",A).


get_encoding_module(RI) ->
    case (catch get_conf(encoding_module, RI)) of
	{error, _} ->
	    undefined;
	Val ->
	    Val
    end.

get_encoding_config(RI, EM) ->
    case text_codec(EM) of
	true ->
	    case megaco:system_info(text_config) of
		[Conf] when is_list(Conf) ->
		    Conf;
		_ ->
		    []
	    end;

	false ->
	    get_conf(encoding_config, RI)
    end.

text_codec(megaco_compact_text_encoder) ->
    true;
text_codec(megaco_pretty_text_encoder) ->
    true;
text_codec(_) ->
    false.


get_transport_module(RI) ->
    get_conf(transport_module, RI).

get_transport_port(RI) ->
    get_conf(port, RI).


get_conf(Key, Config) ->
    case lists:keysearch(Key, 1, Config) of
	{value, {Key, Val}} ->
	    Val;
	_ ->
	    exit({error, {not_found, Key, Config}})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

which_local_addr() ->
    case inet:getifaddrs() of
        {ok, IFs} ->
            which_local_addr(IFs);
        {error, Reason} ->
            i("Failed get local address: "
              "~n   ~p", [Reason]),
            ?SKIP({failed_get_local_addr, Reason})
    end.

which_local_addr([]) ->
    ?SKIP(failed_get_local_addr);
which_local_addr([{"lo" = _IfName, _IfOpts}|IFs]) ->
    which_local_addr(IFs);
which_local_addr([{"br-" ++ _ = _IfName, _IfOpts}|IFs]) ->
    which_local_addr(IFs);
which_local_addr([{"docker" ++ _ = _IfName, _IfOpts}|IFs]) ->
    which_local_addr(IFs);
which_local_addr([{_IfName, IfOpts}|IFs]) ->
    case which_local_addr2(IfOpts) of
        {ok, Addr} ->
            Addr;
        error ->
            which_local_addr(IFs)
    end.


which_local_addr2([]) ->
    error;
which_local_addr2([{addr, Addr}|_]) 
  when (size(Addr) =:= 4) andalso (element(1, Addr) =/= 127) ->
    {ok, Addr};
which_local_addr2([_|T]) ->
    which_local_addr2(T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

i(F) ->
    i(F, []).

i(F, A) ->
    print(info, get(verbosity), "", F, A).


d(F) ->
    d(F, []).

d(F, A) ->
    print(debug, get(verbosity), "DBG: ", F, A).


printable(_, debug)   -> true;
printable(info, info) -> true;
printable(_,_)        -> false.

print(Severity, Verbosity, P, F, A) ->
    print(printable(Severity,Verbosity), P, F, A).

print(true, P, F, A) ->
    io:format("~s~p:~s:~s: " ++ F ++ "~n", [P, self(), get(sname), ?FTS() | A]);
print(_, _, _, _) ->
    ok.


progress(F) ->
    progress(F, []).

progress(F, A) ->
    io:format("~s " ++ F ++ "~n", [?FTS()|A]),
    io:format(user, "~s " ++ F ++ "~n", [?FTS()|A]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

random() ->
    10 * rand:uniform(50).

apply_load_timer() ->
    erlang:send_after(random(), self(), apply_load_timeout).

