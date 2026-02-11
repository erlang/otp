%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2003-2026. All Rights Reserved.
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
%%
%% application:set_env(megaco, test_inet_backends, true).
%%
%% S = fun() -> ts:run(kernel, megaco_load_SUITE, [batch]) end.
%% S = fun(SUITE) -> ts:run(kernel, SUITE, [batch]) end.
%% S = fun() -> ct:run_test([{suite, megaco_load_SUITE}]) end.
%% S = fun(SUITE) -> ct:run_test([{suite, SUITE}]) end.
%% G = fun(GROUP) -> ts:run(kernel, megaco_load_SUITE, {group, GROUP}, [batch]) end.
%% G = fun(SUITE, GROUP) -> ts:run(kernel, SUITE, {group, GROUP}, [batch]) end.
%% G = fun(GROUP) -> ct:run_test([{suite, megaco_load_SUITE}, {group, GROUP}]) end.
%% G = fun(SUITE, GROUP) -> ct:run_test([{suite, SUITE}, {group, GROUP}]) end.
%% T = fun(TC) -> ts:run(kernel, megaco_load_SUITE, TC, [batch]) end.
%% T = fun(TC) -> ct:run_test([{suite, megaco_load_SUITE}, {testcase, TC}]) end.
%% T = fun(TC) -> ct:run_test([{suite, megaco_load_SUITE}, {group, inet_backend_socket}, {testcase, TC}]) end.
%% T = fun(S, TC) -> ct:run_test([{suite, S}, {testcase, TC}]) end.
%% T = fun(S, G, TC) -> ct:run_test([{suite, S}, {group, G}, {testcase, TC}]) end.
%%
%%----------------------------------------------------------------------
%%

-module(megaco_load_SUITE).

-export([
 	 suite/0, all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2,

         single_user_light_load/1,
         single_user_medium_load/1,
         single_user_heavy_load/1,
         single_user_extreme_load/1,

         multi_user_light_load/1,
         multi_user_medium_load/1,
         multi_user_heavy_load/1,
         multi_user_extreme_load/1

        ]).

-export([
         do_multi_load/3,
         multi_load_collector/7
        ]).


-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").
-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").
-include("megaco_test_lib.hrl").

-define(TEST_VERBOSITY, debug).
-define(MGC_VERBOSITY,  silence).
-define(MG_VERBOSITY,   silence).

-define(SINGLE_USER_LOAD_NUM_REQUESTS, 1000).
-define(MULTI_USER_LOAD_NUM_REQUESTS,  1000).

-define(MGC_START(Node, Mid, ET, Conf, Verb), 
        megaco_test_mgc:start(Node, Mid, ET, 
			      Conf ++ [{megaco_trace, false}], Verb)).
-define(MGC_STOP(Pid), megaco_test_mgc:stop(Pid)).
-define(MGC_USER_INFO(Pid,Tag), megaco_test_mgc:user_info(Pid,Tag)).
-define(MGC_CONN_INFO(Pid,Tag), megaco_test_mgc:conn_info(Pid,Tag)).
-define(MGC_SET_VERBOSITY(Pid, V), megaco_test_mgc:verbosity(Pid, V)).

-define(MG_START(Pid, Mid, Enc, Transp, Conf, Verb), 
        megaco_test_mg:start(Pid, Mid, Enc, Transp, 
			     Conf ++ [{megaco_trace, false},
                                      {transport_opts, [{serialize, true}]}], 
			     Verb)).
-define(MG_STOP(Pid), megaco_test_mg:stop(Pid)).
-define(MG_USER_INFO(Pid,Tag), megaco_test_mg:user_info(Pid,Tag)).
-define(MG_CONN_INFO(Pid,Tag), megaco_test_mg:conn_info(Pid,Tag)).
-define(MG_SERV_CHANGE(Pid), megaco_test_mg:service_change(Pid)).
-define(MG_MLOAD(Pid, NL, NR), 
	timer:tc(megaco_test_mg, apply_multi_load, [Pid, NL, NR])).
-define(MG_LOAD(Pid, NL, NR), megaco_test_mg:apply_multi_load(Pid, NL, NR)).
-define(MG_SET_VERBOSITY(Pid, V), megaco_test_mg:verbosity(Pid, V)).


-define(BENCH_SUITE, megaco_load).


%%======================================================================
%% Common Test interface functions
%%======================================================================

suite() -> 
    [{ct_hooks, [ts_install_cth]}].

all() -> 
    %% This is a "temporary" messure to ensure that we can 
    %% test the socket backend without effecting *all*
    %% applications on *all* machines.
    %% This flag is set only for *one* host.
    case ?TEST_INET_BACKENDS() of
        true ->
            [
             {group, inet_backend_default},
             {group, inet_backend_inet},
             {group, inet_backend_socket}
            ];
        _ ->
            [
             {group, inet_backend_default}
            ]
    end.

groups() -> 
    [
     {inet_backend_default, [], inet_backend_default_cases()},
     {inet_backend_inet,    [], inet_backend_inet_cases()},
     {inet_backend_socket,  [], inet_backend_socket_cases()},

     {standard,             [], standard_cases()},
     {single,               [], single_cases()},
     {multi,                [], multi_cases()},

     %% "special" group(s) - only run explicitly
     {bench,                [], bench_cases()},
     {bench_single,         [], bench_single_cases()},
     {bench_multi,          [], bench_multi_cases()}
     
    ].

inet_backend_default_cases() ->
    [{standard, [], standard_cases()}].

inet_backend_inet_cases() ->
    [{standard, [], standard_cases()}].

inet_backend_socket_cases() ->
    [{standard, [], standard_cases()}].

standard_cases() ->
    [
     {group, single},
     {group, multi}
    ].

single_cases() ->
    [
     single_user_light_load,
     single_user_medium_load,
     single_user_heavy_load,
     single_user_extreme_load
    ].

multi_cases() ->
    [
     multi_user_light_load,
     multi_user_medium_load,
     multi_user_heavy_load,
     multi_user_extreme_load
    ].


bench_cases() ->
    [
     {group, bench_single},
     {group, bench_multi}
    ].

bench_single_cases() ->
    [
     single_user_light_load,
     single_user_medium_load
    ].

bench_multi_cases() ->
    [
     multi_user_light_load,
     multi_user_medium_load
    ].




%%
%% -----
%%

init_per_suite(suite) ->
    [];
init_per_suite(doc) ->
    [];
init_per_suite(Config0) when is_list(Config0) ->

    ?ANNOUNCE_SUITE_INIT(),

    p("init_per_suite -> entry with"
      "~n      Config: ~p"
      "~n      Nodes:  ~p", [Config0, erlang:nodes()]),

    case ?LIB:init_per_suite(Config0) of
        {skip, _} = SKIP ->
            SKIP;

        Config1 when is_list(Config1) ->

            %% We need a (local) monitor on this node also
            megaco_test_sys_monitor:start(),

            p("init_per_suite -> end when"
              "~n      Config: ~p"
              "~n      Nodes:  ~p", [Config1, erlang:nodes()]),

            Config1
    end.

end_per_suite(suite) -> [];
end_per_suite(doc) -> [];
end_per_suite(Config0) when is_list(Config0) ->

    p("end_per_suite -> entry with"
      "~n      Config: ~p"
      "~n      Nodes:  ~p", [Config0, erlang:nodes()]),

    megaco_test_sys_monitor:stop(),
    Config1 = ?LIB:end_per_suite(Config0),

    p("end_per_suite -> end when"
      "~n      Nodes:  ~p", [erlang:nodes()]),

    Config1.


%%
%% -----
%%

init_per_group(bench = Group, Config) ->
    ?ANNOUNCE_GROUP_INIT(Group),
    [{category, Group}, {socket_create_opts, []} | Config];
init_per_group(standard = Group, Config) ->
    ?ANNOUNCE_GROUP_INIT(Group),
    %% Just in case someone runs the group directly
    case ?config(socket_create_opts, Config) of
        undefined ->
            [{category, Group}, {socket_create_opts, []} | Config];
        _ ->
            [{category, Group} | Config]
    end;
init_per_group(inet_backend_default = Group, Config) ->
    ?ANNOUNCE_GROUP_INIT(Group),
    [{socket_create_opts, []} | Config];
init_per_group(inet_backend_inet = Group, Config) ->
    ?ANNOUNCE_GROUP_INIT(Group),
    case ?EXPLICIT_INET_BACKEND() of
        true ->
            %% The environment trumps us,
            %% so only the default group should be run!
            {skip, "explicit inet backend"};
        false ->
            [{socket_create_opts, [{inet_backend, inet}]} | Config]
    end;
init_per_group(inet_backend_socket = Group, Config) ->
    ?ANNOUNCE_GROUP_INIT(Group),
    case ?EXPLICIT_INET_BACKEND() of
        true ->
            %% The environment trumps us,
            %% so only the default group should be run!
            {skip, "explicit inet backend"};
        false ->
            [{socket_create_opts, [{inet_backend, socket}]} | Config]
    end;
init_per_group(Group, Config) ->
    ?ANNOUNCE_GROUP_INIT(Group),
    Config.

end_per_group(Group, Config) when (inet_backend_default =:= Group) orelse
                                  (inet_backend_init    =:= Group) orelse
                                  (inet_backend_socket  =:= Group) ->
    ?SLEEP(?SECS(5)),
    Config;
end_per_group(_Group, Config) ->
    Config.



%%
%% -----
%%

init_per_testcase(single_user_light_load = Case, Config) ->
    C = lists:keydelete(tc_timeout, 1, Config),
    do_init_per_testcase(Case, [{tc_timeout, min(2)}|C]);
init_per_testcase(single_user_medium_load = Case, Config) ->
    C = lists:keydelete(tc_timeout, 1, Config),
    do_init_per_testcase(Case, [{tc_timeout, min(5)}|C]);
init_per_testcase(single_user_heavy_load = Case, Config) ->
    C = lists:keydelete(tc_timeout, 1, Config),
    do_init_per_testcase(Case, [{tc_timeout, min(10)}|C]);
init_per_testcase(single_user_extreme_load = Case, Config) ->
    C = lists:keydelete(tc_timeout, 1, Config),
    do_init_per_testcase(Case, [{tc_timeout, min(20)}|C]);
init_per_testcase(multi_user_light_load = Case, Config) ->
    C = lists:keydelete(tc_timeout, 1, Config),
    do_init_per_testcase(Case, [{tc_timeout, min(2)}|C]);
init_per_testcase(multi_user_medium_load = Case, Config) ->
    C = lists:keydelete(tc_timeout, 1, Config),
    do_init_per_testcase(Case, [{tc_timeout, min(5)}|C]);
init_per_testcase(multi_user_heavy_load = Case, Config) ->
    C = lists:keydelete(tc_timeout, 1, Config),
    do_init_per_testcase(Case, [{tc_timeout, min(10)}|C]);
init_per_testcase(multi_user_extreme_load = Case, Config) ->
    C = lists:keydelete(tc_timeout, 1, Config),
    do_init_per_testcase(Case, [{tc_timeout, min(20)}|C]);
init_per_testcase(Case, Config) ->
    do_init_per_testcase(Case, Config).

do_init_per_testcase(Case, Config) ->
    process_flag(trap_exit, true),

    p("init_per_suite -> entry with"
      "~n      Config: ~p"
      "~n      Nodes:  ~p", [Config, erlang:nodes()]),

    megaco_test_global_sys_monitor:reset_events(),
    megaco_test_lib:init_per_testcase(Case, Config).

end_per_testcase(Case, Config) ->
    process_flag(trap_exit, false),

    p("end_per_suite -> entry with"
      "~n      Config: ~p"
      "~n      Nodes:  ~p", [Config, erlang:nodes()]),

    p("system events during test: "
      "~n   ~p", [megaco_test_global_sys_monitor:events()]),

    megaco_test_lib:end_per_testcase(Case, Config).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_user_light_load(suite) ->
    [];
single_user_light_load(doc) ->
    [];
single_user_light_load(Config) when is_list(Config) ->
    try_single_user_load(?FUNCTION_NAME, single_user_light, Config, 5).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_user_medium_load(suite) ->
    [];
single_user_medium_load(doc) ->
    [];
single_user_medium_load(Config) when is_list(Config) ->
    try_single_user_load(?FUNCTION_NAME, single_user_medium, Config, 15).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_user_heavy_load(suite) ->
    [];
single_user_heavy_load(doc) ->
    [];
single_user_heavy_load(Config) when is_list(Config) ->
    try_single_user_load(?FUNCTION_NAME, single_user_high, Config, 25).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_user_extreme_load(suite) ->
    [];
single_user_extreme_load(doc) ->
    [];
single_user_extreme_load(Config) when is_list(Config) ->
    try_single_user_load(?FUNCTION_NAME, single_user_extreme, Config, 100).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_user_light_load(suite) ->
    [];
multi_user_light_load(doc) ->
    [];
multi_user_light_load(Config) when is_list(Config) ->
    try_multi_user_load(?FUNCTION_NAME, multi_user_light, Config, 3, 2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_user_medium_load(suite) ->
    [];
multi_user_medium_load(doc) ->
    [];
multi_user_medium_load(Config) when is_list(Config) ->
    try_multi_user_load(?FUNCTION_NAME, multi_user_medium, Config, 3, 5).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_user_heavy_load(suite) ->
    [];
multi_user_heavy_load(doc) ->
    [];
multi_user_heavy_load(Config) when is_list(Config) ->
    try_multi_user_load(?FUNCTION_NAME, multi_user_high, Config, 3, 10).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_user_extreme_load(suite) ->
    [];
multi_user_extreme_load(doc) ->
    [];
multi_user_extreme_load(Config) when is_list(Config) ->
    try_multi_user_load(?FUNCTION_NAME, multi_user_extreme, Config, 3, 15).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

populate([]) ->
    ok;
populate([{Key,Val}|Env]) ->
    put(Key, Val),
    populate(Env).

load_controller(Config, Fun) when is_list(Config) and is_function(Fun) ->
    process_flag(trap_exit, true),
    {value, {tc_timeout, TcTimeout}} = 
	lists:keysearch(tc_timeout, 1, Config),
    SkipTimeout = trunc(95*TcTimeout/100), % 95% of TcTimeout
    Env = get(),
    Loader = erlang:spawn_link(fun() -> Fun(Env) end),
    receive
	{'EXIT', Loader, {comment, _} = Comment} ->
	    d("~s -> "
	      "loader [~p] terminated with comment", [?FUNCTION_NAME, Loader]),
	    Comment;
	{'EXIT', Loader, normal} ->
	    d("~s -> "
	      "loader [~p] terminated with normal", [?FUNCTION_NAME, Loader]),
	    ok;
	{'EXIT', Loader, ok} ->
	    d("~s -> "
	      "loader [~p] terminated with ok~n", [?FUNCTION_NAME, Loader]),
	    ok;
	{'EXIT', Loader, {skipped, {fatal, Reason, File, Line}}} ->
	    i("~s -> "
	      "loader [~p] terminated with fatal skip"
	      "~n   Reason: ~p"
	      "~n   At:     ~p:~p", [?FUNCTION_NAME, Loader, Reason, File, Line]),
	    ?SKIP(Reason);
	{'EXIT', Loader, {skipped, Reason}} ->
	    i("~s_controller -> "
	      "loader [~p] terminated with skip"
	      "~n   Reason: ~p", [?FUNCTION_NAME, Loader, Reason]),
	    ?SKIP(Reason);
	{'EXIT', Loader, Reason} ->
	    i("~s -> "
	      "loader [~p] terminated with"
	      "~n   ~p", [?FUNCTION_NAME, Loader, Reason]),
	    erlang:error({unexpected_loader_result, Reason})
    after SkipTimeout ->
	    i("~s -> "
	      "loader [~p] timeout", [?FUNCTION_NAME, Loader]),
	    exit(Loader, kill),
	    ?SKIP({timeout, SkipTimeout, TcTimeout})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_single_user_load(Name, BenchName, Config, NumLoaders0) ->
    Factor     = ?config(megaco_factor, Config),
    Category   = ?config(category,      Config),
    NumLoaders =
	if
	    (Factor =:= 1) ->
		NumLoaders0;
	    (Factor > NumLoaders0) ->
		1;
	    true ->
		NumLoaders0 div Factor
	end,
    Pre = fun() ->
		  MgcNode = make_node_name(mgc),
		  MgNode  = make_node_name(mg),
		  d("Nodes: "
		    "~n      MgcNode: ~p"
		    "~n      MgNode:  ~p", [MgcNode, MgNode]),
		  Nodes = [MgcNode, MgNode],
		  ok = ?START_NODES(Nodes),
		  #{bench_name => BenchName,
                    category   => Category,
                    nodes      => Nodes}
	  end,
    Case = fun(State) -> single_user_load(State, Config, NumLoaders) end,
    Post = fun(#{nodes := Nodes}) ->
                   d("stop nodes"),
                   ?STOP_NODES(lists:reverse(Nodes))
           end,
    try_tc(Name, Pre, Case, Post).


single_user_load(State, Config, NumLoaders) ->
    i("starting with ~w loader(s)", [NumLoaders]),
    SCO = ?config(socket_create_opts, Config),
    Res = load_controller(Config, 
			  fun(Env) -> 
				  populate(Env), 
				  exit( do_single_user_load(SCO,
                                                            State, NumLoaders) ) 
			  end),
    i("done"),
    Res.

do_single_user_load(SCO,
                    #{bench_name := BenchName,
                      category   := Category,
                      nodes      := [MgcNode, MgNode]},
                    NumLoaders) ->
    %% Start the MGC and MGs
    i("[MGC] start"),
    MgcMid = {deviceName, "ctrl"},
    ET     = [{text, tcp, [{serialize, true}]}],
    DSI = maybe_display_system_info(NumLoaders),
    {ok, Mgc} = ?MGC_START(MgcNode, MgcMid, ET, SCO ++ DSI, ?MGC_VERBOSITY),

    i("[MG] start"),
    MgMid = {deviceName, "mg"},
    {ok, Mg} = ?MG_START(MgNode, MgMid, text, tcp, SCO ++ DSI, ?MG_VERBOSITY),

    d("MG user info: ~p", [?MG_USER_INFO(Mg, all)]),

    i("[MG] connect to the MGC (service change)"),    
    ServChRes = ?MG_SERV_CHANGE(Mg),
    d("service change result: ~p", [ServChRes]),

    megaco_test_mg:update_conn_info(Mg,reply_timer,1000),
    megaco_test_mgc:update_conn_info(Mgc,reply_timer,1000),

    d("MG conn info: ~p", [?MG_CONN_INFO(Mg, all)]),

    d("apply the load"),
    LoadRes = ?MG_MLOAD(Mg, NumLoaders, ?SINGLE_USER_LOAD_NUM_REQUESTS),
    {CResult, Result} =
        case LoadRes of
            {Time, {ok, Ok, Err}} ->
                MSec = Time div 1000,
                %% Just to be on the safe side we check that the
                %% number of successsful calls is greater than or equal to
                %% the (run) time in msec (otherwise the "performance" result
                %% will be zero)
                NumCalls = Ok * ?SINGLE_USER_LOAD_NUM_REQUESTS,
                if
                    (NumCalls > MSec) ->
                        %% Each successful loader has performed
                        %% ?SINGLE_USER_LOAD_NUM_REQUESTS calls
                        %% Also, should we even count a loader that
                        %% has registered fails? We do not know
                        %% if a failed loader managed any requests...
                        Perf = NumCalls div MSec,
                        io:format("~nMultiple loaders result: ~n"
                                  "   Number of successful:      ~w~n"
                                  "   Number of failure:         ~w~n"
                                  "   Time:                      ~w msec~n"
                                  "   (successful) Calls / msec: ~w~n~n", 
                                  [Ok, Err, MSec, Perf]),
                        {
                         {comment, ?F("~w calls / msec", [Perf])},
                         Perf
                        };

                    %% The resulting "performance" will be 0 (calls / msec),
                    %% so either we are running on a really slow machine or
                    %% something went wrong (lots of errors?).
                    %% Either way, do not report this, will only skew the
                    %% results.

                    (Ok > 0) ->
                        Sec  = Time div 10000000,
                        Perf = NumCalls div Sec,
                        io:format("~nMultiple loaders result: ~n"
                                  "   Number of successful: ~w~n"
                                  "   Number of failure:    ~w~n"
                                  "   Time:                 ~w sec~n"
                                  "   (successful) Calls / sec: ~w~n~n", 
                                  [Ok, Err, Sec, Perf]),
                        {
                         {comment, ?F("~w calls / sec", [Perf])},
                         undefined
                        }
                end;
            {Time, Error} ->
                io:format("SUL: multiple loaders failed: ~p after ~w~n", 
                          [Error, Time]),
                {Error, undefined}
        end,

    i("flush the message queue: ~p", [megaco_test_lib:flush()]),
    
    i("verbosity to trace"),
    ?MGC_SET_VERBOSITY(Mgc, debug),
    ?MG_SET_VERBOSITY(Mg, debug),

    %% Tell MG to stop
    i("[MG] stop"),
    ?MG_STOP(Mg),

    i("flush the message queue: ~p", [megaco_test_lib:flush()]),

    %% Tell Mgc to stop
    i("[MGC] stop"),
    ?MGC_STOP(Mgc),

    i("flush the message queue: ~p", [megaco_test_lib:flush()]),

    maybe_publish_result(Category, BenchName, Result),
    CResult.


maybe_publish_result(bench = _Category,
                     Name, Result) when (Result =/= undefined) ->
    Event = ?BENCH_EVENT(Name, Result),
    ct_event:notify(Event);
maybe_publish_result(_, _, _) ->
    ok.

    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_multi_user_load(Name, BenchName, Config, NumUsers, NumLoaders0) ->
    Factor     = ?config(megaco_factor, Config),
    Category   = ?config(category,      Config),
    NumLoaders =
	if
	    (Factor =:= 1) ->
		NumLoaders0;
	    (Factor > NumLoaders0) ->
		1;
	    true ->
		NumLoaders0 div Factor
	end,
    Pre = fun() ->
		  MgcNode = make_node_name(mgc),
		  MgNodes = make_node_names(mg, NumUsers),
		  d("Nodes: "
		    "~n      MgcNode: ~p"
		    "~n      MgNodes: ~p", [MgcNode, MgNodes]),
		  Nodes = [MgcNode | MgNodes],
		  ok = ?START_NODES(Nodes),
		  #{bench_name => BenchName,
                    category   => Category,
                    nodes      => Nodes}
	  end,
    Case = fun(State) ->
		   multi_user_load(State, Config, NumUsers, NumLoaders)
	   end,
    Post = fun(#{nodes := Nodes}) ->
                   d("stop nodes"),
                   ?STOP_NODES(lists:reverse(Nodes))
           end,
    try_tc(Name, Pre, Case, Post).


multi_user_load(State, Config, NumUsers, NumLoaders) ->
    i("starting with ~w loader(s)", [NumLoaders]),
    SCO = ?config(socket_create_opts, Config),
    Res = load_controller(
	    Config, 
	    fun(Env) -> 
		    populate(Env), 
		    exit( do_multi_user_load(SCO, State, NumUsers, NumLoaders) ) 
	    end),
    i("done"),
    Res.


do_multi_user_load(SCO,
                   #{bench_name := BenchName,
                     category   := Category,
                     nodes      := [MgcNode | MgNodes]},
                   NumUsers, NumLoaders) 
  when (is_integer(NumUsers) andalso (NumUsers > 1) andalso 
	is_integer(NumLoaders) andalso (NumLoaders >= 1)) ->
    %% Start the MGC and MGs
    i("[MGC] start"),
    MgcMid = {deviceName, "ctrl"},
    ET     = [{text, tcp, [{serialize, false}]}],
    DSI = maybe_display_system_info(2 * NumUsers * NumLoaders),
    {ok, Mgc} = ?MGC_START(MgcNode, MgcMid, ET, SCO ++ DSI, ?MGC_VERBOSITY),

    megaco_test_mgc:update_user_info(Mgc,reply_timer,1000),
    d("MGC user info: ~p", [?MGC_USER_INFO(Mgc, all)]),

    MgUsers = make_mids(MgNodes),

    d("start MGs, apply the load and stop MGs"),
    {CResult, Result} =
        multi_load(MgUsers, SCO ++ DSI, NumLoaders,
                   ?MULTI_USER_LOAD_NUM_REQUESTS),

    i("flush the message queue: ~p", [megaco_test_lib:flush()]),

    ?MGC_SET_VERBOSITY(Mgc, debug),

    %% Tell Mgc to stop
    i("[MGC] stop"),
    ?MGC_STOP(Mgc),

    i("flush the message queue: ~p", [megaco_test_lib:flush()]),

    maybe_publish_result(Category, BenchName, Result),
    CResult.


multi_load(MGs, Conf, NumLoaders, NumReqs) ->
    d("multi_load -> entry with"
      "~n   MGs:        ~p"
      "~n   Conf:       ~p"
      "~n   NumLoaders: ~p"
      "~n   NumReqs:    ~p", [MGs, Conf, NumLoaders, NumReqs]),
    Pids = multi_load_collector_start(MGs, Conf, NumLoaders, NumReqs, []),
    case timer:tc(?MODULE, do_multi_load, [Pids, NumLoaders, NumReqs]) of
	{Time, {ok, OKs, []}} ->
	    multi_load_collector_calc(Time, OKs);
	{Time, Error} ->
	    MSec = Time/1000,
	    io:format("~nmulti load failed after ~.1f msec:~n~p~n~n",
                      [MSec,Error]),
	    {
             {comment, ?F("~p", [Error])},
             undefined
            }
    end.

do_multi_load(Pids, _NumLoaders, _NumReqs) ->
    Fun = 
	fun({P,_}) -> 
		d("apply multi load for ~p", [P]),
		P ! {apply_multi_load, self()} 
	end,
    lists:foreach(Fun, Pids),
    await_multi_load_collectors(Pids, [], []).

multi_load_collector_start([], _Conf, _NumLoaders, _NumReqs, Pids) ->
    Pids;
multi_load_collector_start([{Mid, Node}|MGs], Conf, NumLoaders, NumReqs, Pids) ->
    Env = get(),
    Pid = spawn_link(?MODULE, multi_load_collector, 
		     [self(), Node, Mid, Conf, NumLoaders, NumReqs, Env]),
    multi_load_collector_start(MGs, Conf, NumLoaders, NumReqs, [{Pid,Mid}|Pids]).

get_env(Key, Env) ->
    case lists:keysearch(Key, 1, Env) of
	{value, {Key, Val}} ->
	    Val;
	_ ->
	    undefined
    end.

multi_load_collector(Parent, Node, Mid, Conf, NumLoaders, NumReqs, Env) ->
    put(verbosity, get_env(verbosity, Env)),
    put(tc,        get_env(tc, Env)),
    put(sname,     get_env(sname, Env) ++ "-loader"),
    case ?MG_START(Node, Mid, text, tcp, Conf, ?MG_VERBOSITY) of
	{ok, Pid} ->
	    d("MG ~p user info: ~n~p", [Mid, ?MG_USER_INFO(Pid, all)]),
	    ServChRes = ?MG_SERV_CHANGE(Pid),
	    d("service change result: ~p", [ServChRes]),
	    megaco_test_mg:update_conn_info(Pid,reply_timer,1000),
	    d("MG ~p conn info: ~p", [Mid, ?MG_CONN_INFO(Pid,all)]),
	    multi_load_collector_loop(Parent, Pid, Mid, NumLoaders, NumReqs);
	Else ->
	    Parent ! {load_start_failed, self(), Mid, Else}
    end.

multi_load_collector_loop(Parent, Pid, Mid, NumLoaders, NumReqs) ->
    d("~s -> entry with"
      "~n   Parent:     ~p"
      "~n   Pid:        ~p"
      "~n   Mid:        ~p"
      "~n   NumLoaders: ~p"
      "~n   NumReqs:    ~p"
      "~nwhen"
      "~n   self():     ~p"
      "~n   node():     ~p", 
      [?FUNCTION_NAME, Parent, Pid, Mid, NumLoaders, NumReqs, self(), node()]),
    receive
	{apply_multi_load, Parent} ->
            d("~s -> apply load (~w, ~w)",
              [?FUNCTION_NAME, NumLoaders, NumReqs]),
	    Res = ?MG_LOAD(Pid, NumLoaders, NumReqs),
            d("~s -> load result: ~p", [?FUNCTION_NAME, Res]),
	    Parent ! {load_complete, self(), Mid, Res},
	    ?MG_SET_VERBOSITY(Pid, debug),
	    ?MG_STOP(Pid),
	    exit(normal)
    end.    
    

await_multi_load_collectors([], Oks, Errs) ->
    i("await_multi_load_collectors -> done"),
    {ok, Oks, Errs};
await_multi_load_collectors(Pids, Oks, Errs) ->
    receive
	{load_complete, Pid, Mg, {ok, Ok, Err}} ->
	    i("~s -> "
	      "received ok complete: "
	      "~n   From:   ~p [~p]"
	      "~n   Result: ~p, ~p", [?FUNCTION_NAME, Pid, Mg, Ok, Errs]),
	    Pids2 = lists:keydelete(Pid, 1, Pids),
	    Oks2  = [{Mg, Ok, Err}|Oks],
	    await_multi_load_collectors(Pids2, Oks2, Errs);
	{load_complete, Pid, Mg, Error} ->
	    i("~s -> "
	      "received error complete: "
	      "~n   From:  ~p [~p]: "
	      "~n   Error: ~p", [?FUNCTION_NAME, Pid, Mg, Error]),
	    Pids2 = lists:keydelete(Pid, 1, Pids),
	    Errs2 = [{Mg, Error}|Errs],
	    await_multi_load_collectors(Pids2, Oks, Errs2);

	{'EXIT', Pid, normal} ->
	    %% This is assumed to be one of the collectors
	    i("~s -> "
	      "received (normal) exit signal from ~p", [?FUNCTION_NAME, Pid]),
	    await_multi_load_collectors(Pids, Oks, Errs);

	{'EXIT', Pid, Reason} ->
	    i("~s -> "
	      "received unexpected exit from ~p:"
	      "~n   ~p", [?FUNCTION_NAME, Pid, Reason]),
	    case lists:keydelete(Pid, 1, Pids) of
		Pids ->
		    %% Not one of my procs, or a proc I have already
		    %% received a complete from.
		    await_multi_load_collectors(Pids, Oks, Errs);
		Pids2 ->
		    [{Pid,Mg}] = Pids -- Pids2,
		    Errs2 = [{Mg, {unexpected_exit, Reason}}|Errs],
		    await_multi_load_collectors(Pids, Oks, Errs2)
	    end;

	Else ->
	    i("~s -> received unexpected message:"
	      "~n~p", [?FUNCTION_NAME, Else]),
	    await_multi_load_collectors(Pids, Oks, Errs)
    after 
	5000 ->
	    i("~s -> still awaiting reply from:"
	      "~n~p", [?FUNCTION_NAME, Pids]),
	    await_multi_load_collectors(Pids, Oks, Errs)
    end.
	    
		
%% Note that this is an approximation...we run all the
%% MGs in parallel, so it should be "accurate"...
multi_load_collector_calc(Time, Oks) ->
    io:format("~s -> entry with"
              "~n   Time: ~p (usec)"
              "~n   Oks:  ~p"
              "~n", [?FUNCTION_NAME, Time, Oks]),
    Succs    = lists:sum([Ok   || {_, Ok,   _} <- Oks]),
    Fails    = lists:sum([Err  || {_,  _, Err} <- Oks]),
    MSec     = Time div 1000,
    NumCalls = Succs * ?MULTI_USER_LOAD_NUM_REQUESTS,
    if
        (NumCalls > MSec) ->
            %% The resulting "performance" will be a value greater 
            %% or equal to one (1).
            Perf  = NumCalls div MSec,
            io:format("~nTotal multiple loaders result: ~n"
                      "   Number of successful: ~w~n"
                      "   Number of failure:    ~w~n"
                      "   Time:                 ~w msec~n"
                      "   Total (successful) Calls / msec: ~w~n~n", 
                      [Succs, Fails, MSec, Perf]),
            {
             {comment, ?F("~w calls / msec", [Perf])},
             Perf
            };

        %% The resulting "performance" will be 0 (calls / msec),
        %% so either we are running on a really slow machine or
        %% something went wrong (lots of errors?).
        %% Either way, do not report this, will only skew the
        %% results.

        (Succs > 0) ->
            Sec   = Time div 10000000,
            Perf  = NumCalls div Sec,
            io:format("~nTotal multiple loaders result: ~n"
                      "   Number of successful: ~w~n"
                      "   Number of failure:    ~w~n"
                      "   Time:                 ~w sec~n"
                      "   Total (successful) Calls / sec: ~w~n~n", 
                      [Succs, Fails, Sec, Perf]),
            {
             {comment, ?F("~w calls / sec", [Perf])},
             undefined
            };
        true ->
            io:format("~nTotal multiple loaders result: ~n"
                      "   Number of successful: ~w~n"
                      "   Number of failure:    ~w~n"
                      "   Time:                 ~w usec~n~n", 
                      [Succs, Fails, Time]),
            {
             {comment, "No successful calls"},
             undefined
            }
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_node_names(Name, Num) ->
    make_node_names(Name, Num, []).

make_node_names(_, 0, Names) ->
    Names;
make_node_names(BaseName, N, Names) ->
    Name = lists:flatten(io_lib:format("~p~w", [BaseName,N])),
    make_node_names(BaseName, N-1, [make_node_name(Name)|Names]).

make_node_name(Name) when is_atom(Name) ->
    make_node_name(atom_to_list(Name));
make_node_name(Name) when is_list(Name) ->
    case string:tokens(atom_to_list(node()), [$@]) of
	[_,Host] ->
	    list_to_atom(lists:concat([Name ++ "@" ++ Host]));
	_ ->
	    exit("Test node must be started with '-sname'")
     end.

make_mids(MgNodes) when is_list(MgNodes) andalso (length(MgNodes) > 0) ->
    make_mids(MgNodes, []).

make_mids([], Mids) ->
    lists:reverse(Mids);
make_mids([MgNode|MgNodes], Mids) ->
    case string:tokens(atom_to_list(MgNode), [$@]) of
	[Name, _] ->
	    Mid = {deviceName, Name},
	    make_mids(MgNodes, [{Mid, MgNode}|Mids]);
	_Else ->
	    exit("Test node must be started with '-sname'")
    end.

maybe_display_system_info(NumLoaders) when NumLoaders > 50 ->
    [{display_system_info, timer:seconds(2)}];
maybe_display_system_info(NumLoaders) when NumLoaders > 10 ->
    [{display_system_info, timer:seconds(1)}];
maybe_display_system_info(_) ->
    [].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_tc(TCName, Pre, Case, Post) ->
    try_tc(TCName, "TEST", ?TEST_VERBOSITY, Pre, Case, Post).

try_tc(TCName, Name, Verbosity, Pre, Case, Post) ->
    ?TRY_TC(TCName, Name, Verbosity, Pre, Case, Post).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

min(M) -> timer:minutes(M).

p(F, A) ->
    io:format("*** [~s] ~p ***"
	      "~n   " ++ F ++ "~n", 
	      [?FTS(), self() | A]).

i(F) ->
    i(F, []).

i(F, A) ->
    print(info, get(verbosity), get(tc), "INF", F, A).

d(F) ->
    d(F, []).

d(F, A) ->
    print(debug, get(verbosity), get(tc), "DBG", F, A).

printable(_, debug)   -> true;
printable(info, info) -> true;
printable(_,_)        -> false.

print(Severity, Verbosity, Tc, P, F, A) ->
    print(printable(Severity,Verbosity), Tc, P, F, A).

print(true, Tc, P, F, A) ->
    io:format("*** [~s] ~s ~p ~s:~w ***"
              "~n   " ++ F ++ "~n", 
              [?FTS(), P, self(), get(sname), Tc | A]);
print(_, _, _, _, _) ->
    ok.


