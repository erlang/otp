%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2017. All Rights Reserved.
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
-module(application_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2
     ]).

-export([failover/1, failover_comp/1, permissions/1, load/1,
	 load_use_cache/1,
	 otp_1586/1, otp_2078/1, otp_2012/1, otp_2718/1, otp_2973/1,
	 otp_3002/1, otp_3184/1, otp_4066/1, otp_4227/1, otp_5363/1,
	 otp_5606/1,
	 start_phases/1, get_key/1, get_env/1,
	 permit_false_start_local/1, permit_false_start_dist/1, script_start/1, 
	 nodedown_start/1, init2973/0, loop2973/0, loop5606/1]).

-export([config_change/1, persistent_env/1,
	 distr_changed_tc1/1, distr_changed_tc2/1,
	 ensure_started/1, ensure_all_started/1,
	 shutdown_func/1, do_shutdown/1, shutdown_timeout/1, shutdown_deadlock/1]).

-define(TESTCASE, testcase_name).
-define(testcase, proplists:get_value(?TESTCASE, Config)).

-export([init_per_testcase/2, end_per_testcase/2, start_type/0, 
	 start_phase/0, conf_change/0]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,2}}].

all() -> 
    [failover, failover_comp, permissions, load,
     load_use_cache, ensure_started, {group, reported_bugs}, start_phases,
     script_start, nodedown_start, permit_false_start_local,
     permit_false_start_dist, get_key, get_env, ensure_all_started,
     {group, distr_changed}, config_change, shutdown_func, shutdown_timeout,
     shutdown_deadlock,
     persistent_env].

groups() -> 
    [{reported_bugs, [],
      [otp_1586, otp_2078, otp_2012, otp_2718, otp_2973,
       otp_3002, otp_3184, otp_4066, otp_4227, otp_5363,
       otp_5606]},
     {distr_changed, [],
      [distr_changed_tc1, distr_changed_tc2]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



init_per_testcase(otp_2973=Case, Config) ->
    code:add_path(proplists:get_value(data_dir,Config)),
    [{?TESTCASE, Case}|Config];
init_per_testcase(Case, Config) ->
    [{?TESTCASE, Case}|Config].

end_per_testcase(otp_2973, Config) ->
    code:del_path(proplists:get_value(data_dir,Config)),
    ok;
end_per_testcase(_Case, _Config) ->
    ok.

-define(UNTIL(Seq), loop_until_true(fun() -> Seq end)).

-record(st, {
          normal = 0,
          local = 0,
          takeover = 0,
          failover = 0
         }).

loop_until_true(Fun) ->
    case Fun() of
	true ->
	    ok;
	_ ->
	    timer:sleep(100),
	    loop_until_true(Fun)
    end.

%%-----------------------------------------------------------------
%% Should be started in a CC view with:
%% erl -sname XXX -rsh ctrsh where XX not in [cp1, cp2, cp3]
%%-----------------------------------------------------------------
%% Tests failover and takeover for distributed applications.  Tests
%% start, load etc implicitly.
failover(Conf) when is_list(Conf) ->
    %% start a help process to check the start type
    StPid = spawn_link(?MODULE, start_type, []),
    yes = global:register_name(st_type, StPid),

    NodeNames = [Ncp1, Ncp2, Ncp3] = node_names([cp1, cp2, cp3], Conf),
    NoSyncTime = config_fun_fast(config_fo(NodeNames)),
    WithSyncTime = config_fun(config_fo(NodeNames)),

    %% Test [cp1, cp2, cp3]
    {ok, Cp1} = start_node_config(Ncp1, NoSyncTime, Conf),
    {ok, Cp2} = start_node_config(Ncp2, NoSyncTime, Conf),
    {ok, Cp3} = start_node_config(Ncp3, WithSyncTime, Conf),
    Cps = [Cp1, Cp2, Cp3],
    wait_for_ready_net(),

    %% Start app1 and make sure cp1 starts it
    {[ok,ok,ok],[]} = 
        rpc:multicall(Cps, application, load, [app1()]),
    ?UNTIL(is_loaded(app1, Cps)),
    {[ok,ok,ok],[]} = 
        rpc:multicall(Cps, application, start, [app1, permanent]),
    ?UNTIL(is_started(app1, Cp1)),
    false = is_started(app1, Cp2),
    ok = get_start_type(#st{normal = 3}),

    %% Stop cp1 and make sure cp2 starts app1
    stop_node_nice(Cp1),
    ?UNTIL(is_started(app1, Cp2)),
    ok = get_start_type(#st{normal = 3}),

    %% Restart cp1 and make sure it restarts app1
    {ok, Cp1_2} = start_node_config(Ncp1, NoSyncTime, Conf),
    global:sync(),
    ok = rpc:call(Cp1_2, application, load, [app1()]),
    ok = rpc:call(Cp1_2, application, start, [app1, permanent]),
    ?UNTIL(is_started(app1, Cp1)),
    ?UNTIL(not is_started(app1, Cp2)),
    ok = get_start_type(#st{takeover = 3}),

    %% Test [{cp1, cp2}, cp3]
    %% Start app_sp and make sure cp2 starts it (cp1 has more apps started)
    {[ok,ok,ok],[]} =
        rpc:multicall([Cp1_2, Cp2, Cp3], application, load, [app_sp()]),
    {[ok,ok,ok],[]} = 
        rpc:multicall([Cp1_2, Cp2, Cp3], application, start,[app_sp,permanent]),
    ?UNTIL(is_started(app_sp, Cp2)),
    false = is_started(app_sp, Cp1),
    false = is_started(app_sp, Cp3),
    ok = get_start_type(#st{normal = 3}),
	
    %% Stop cp2 and make sure cp1 starts app_sp
    stop_node_nice(Cp2),
    ?UNTIL(is_started(app_sp, Cp1_2)),
    ok = get_start_type(#st{failover = 3}),
	
    %% Stop cp1 and make sure cp3 starts app_sp
    stop_node_nice(Cp1_2),
    ?UNTIL(is_started(app_sp, Cp3)),
    ok = get_start_type(#st{normal = 3, failover = 3}),

    %% Restart cp2 and make sure it restarts app_sp
    {ok, Cp2_2} = start_node_config(Ncp2, NoSyncTime, Conf),
    global:sync(),
    ok = rpc:call(Cp2_2, application, load, [app_sp()]),
    ok = rpc:call(Cp2_2, application, start, [app_sp, permanent]),
    ?UNTIL(is_started(app_sp, Cp2_2)),
    ?UNTIL(not is_started(app_sp, Cp3)),
    ok = get_start_type(#st{takeover = 3}),

    %% Restart cp1 and make sure it doesn't restart app_sp
    {ok, Cp1_3} = start_node_config(Ncp1, NoSyncTime, Conf),
    global:sync(),
    ok = rpc:call(Cp1_3, application, load, [app_sp()]),
    ok = rpc:call(Cp1_3, application, start, [app_sp, permanent]),
    ct:sleep(500),
    false = is_started(app_sp, Cp1_3),
    true = is_started(app_sp, Cp2_2),

    %% Force takeover to cp1
    ok = rpc:call(Cp1_3, application, takeover, [app_sp, permanent]),
    ?UNTIL(is_started(app_sp, Cp1_3)),
    ?UNTIL(not is_started(app_sp, Cp2_2)),
    ok = get_start_type(#st{takeover = 3}),

    %% Kill one child process and see that it is started with type local
    PP = global:whereis_name({ch,3}),
    exit(PP, kill),
    ok = get_start_type(#st{local = 1}),

    global:send(st_type, kill),

    stop_node_nice(Cp1_3),
    stop_node_nice(Cp2_2),
    stop_node_nice(Cp3),
    ok.
    
%%-----------------------------------------------------------------
%% Should be started in a CC view with:
%% erl -sname XXX -rsh ctrsh where XX not in [cp1, cp2, cp3]
%%-----------------------------------------------------------------
%% Tests failover and takeover for distributed applications.  Tests
%% start, load etc implicitly. The applications do not use start_phases
%% i.e. the failover should be transfered to normal start type.
failover_comp(Conf) when is_list(Conf) ->
    %% start a help process to check the start type
    StPid = spawn_link(?MODULE, start_type, []),
    yes = global:register_name(st_type, StPid),

    NodeNames = [Ncp1, Ncp2, Ncp3] = node_names([cp1, cp2, cp3], Conf),
    NoSyncTime = config_fun_fast(config(NodeNames)),
    WithSyncTime = config_fun(config(NodeNames)),

    %% Test [cp1, cp2, cp3]
    {ok, Cp1} = start_node_config(Ncp1, NoSyncTime, Conf),
    {ok, Cp2} = start_node_config(Ncp2, NoSyncTime, Conf),
    {ok, Cp3} = start_node_config(Ncp3, WithSyncTime, Conf),
    Cps = [Cp1, Cp2, Cp3],
    wait_for_ready_net(),

    %% Start app1 and make sure cp1 starts it
    {[ok,ok,ok],[]} = 
        rpc:multicall(Cps, application, load, [app1()]),
    ?UNTIL(is_loaded(app1, Cps)),
    {[ok,ok,ok],[]} = 
        rpc:multicall(Cps, application, start, [app1, permanent]),
    ?UNTIL(is_started(app1, Cp1)),
    false = is_started(app1, Cp2),
    ok = get_start_type(#st{normal = 3}),

    %% Stop cp1 and make sure cp2 starts app1
    stop_node_nice(Cp1),
    ?UNTIL(is_started(app1, Cp2)),
    ok = get_start_type(#st{normal = 3}),

    %% Restart cp1 and make sure it restarts app1
    {ok, Cp1_2} = start_node_config(Ncp1, NoSyncTime, Conf),
    global:sync(),
    ok = rpc:call(Cp1_2, application, load, [app1()]),
    ?UNTIL(is_loaded(app1, Cp1_2)),
    ok = rpc:call(Cp1_2, application, start, [app1, permanent]),
    ?UNTIL(is_started(app1, Cp1_2)),
    ?UNTIL(not is_started(app1, Cp2)),
    ok = get_start_type(#st{takeover = 3}),

    %% Test [{cp1, cp2}, cp3]
    %% Start app3 and make sure cp2 starts it (cp1 has more apps started)
    {[ok,ok,ok],[]} =
        rpc:multicall([Cp1_2, Cp2, Cp3], application, load, [app3()]),
    ?UNTIL(is_loaded(app3, [Cp1_2, Cp2, Cp3])),
    {[ok,ok,ok],[]} = 
        rpc:multicall([Cp1_2, Cp2, Cp3], application, start,[app3,permanent]),
    ?UNTIL(is_started(app3, Cp2)),
    false = is_started(app3, Cp1),
    false = is_started(app3, Cp3),
    ok = get_start_type(#st{normal = 3}),
	
    %% Stop cp2 and make sure cp1 starts app3
    stop_node_nice(Cp2),
    ?UNTIL(is_started(app3, Cp1_2)),
    ok = get_start_type(#st{normal = 3}),
	
    %% Stop cp1 and make sure cp3 starts app3
    stop_node_nice(Cp1_2),
    ?UNTIL(is_started(app3, Cp3)),
    ok = get_start_type(#st{normal = 6}),

    %% Restart cp2 and make sure it restarts app3
    {ok, Cp2_2} = start_node_config(Ncp2, NoSyncTime, Conf),
    global:sync(),
    ok = rpc:call(Cp2_2, application, load, [app3()]),
    ?UNTIL(is_loaded(app3, Cp2_2)),
    ok = rpc:call(Cp2_2, application, start, [app3, permanent]),
    ?UNTIL(is_started(app3, Cp2_2)),
    ?UNTIL(not is_started(app3, Cp3)),
    ok = get_start_type(#st{takeover = 3}),

    %% Restart cp1 and make sure it doesn't restart app3
    {ok, Cp1_3} = start_node_config(Ncp1, NoSyncTime, Conf),
    global:sync(),
    ok = rpc:call(Cp1_3, application, load, [app3()]),
    true = is_loaded(app3, Cp1_3),
    ok = rpc:call(Cp1_3, application, start, [app3, permanent]),
    ct:sleep(5000),
    false = is_started(app3, Cp1_3),
    true = is_started(app3, Cp2_2),

    %% Force takeover to cp1
    ok = rpc:call(Cp1_3, application, takeover, [app3, permanent]),
    ?UNTIL(is_started(app3, Cp1_3)),
    ?UNTIL(not is_started(app3, Cp2_2)),
    ok = get_start_type(#st{takeover = 3}),

    %% Kill one child process and see that it is started with type local
    PP = global:whereis_name({ch,3}),
    exit(PP, kill),
    ok = get_start_type(#st{local = 1}),

    global:send(st_type, kill),

    stop_node_nice(Cp1_3),
    stop_node_nice(Cp2_2),
    stop_node_nice(Cp3),
    ok.

%%-----------------------------------------------------------------
%% Should be started in a CC view with:
%% erl -sname XXX -rsh ctrsh where XX not in [cp1, cp2, cp3]
%%-----------------------------------------------------------------
%% Tests permissions for distributed applications.
permissions(Conf) when is_list(Conf) ->

    NodeNames = [Ncp1, Ncp2, Ncp3] = node_names([cp1, cp2, cp3], Conf),
    NoSyncTime = config_fun_fast(config2(NodeNames)),
    WithSyncTime = config_fun(config2(NodeNames)),

    %% Test [cp1, cp2, cp3]
    {ok, Cp1} = start_node_config(Ncp1, NoSyncTime, Conf),
    {ok, Cp2} = start_node_config(Ncp2, NoSyncTime, Conf),
    {ok, Cp3} = start_node_config(Ncp3, WithSyncTime, Conf),
    Cps = [Cp1, Cp2, Cp3],
    wait_for_ready_net(),

    %% Start app1 and make sure cp1 starts it
    {[ok,ok,ok],[]} =
        rpc:multicall(Cps, application, load, [app1()]),
    ?UNTIL(is_loaded(app1, Cps)),
    {[ok,ok,ok],[]} =
        rpc:multicall(Cps, application, start, [app1, permanent]),
    ?UNTIL(is_started(app1, Cp1)),
    false = is_started(app1, Cp2),

    %% Unpermit app1 on cp1, make sure cp2 starts it
    ok = rpc:call(Cp1, application, permit, [app1, false]),
    false = is_started(app1, Cp1),
    true = is_started(app1, Cp2),

    %% Unpermit app1 on cp2, make sure cp3 starts it
    ok = rpc:call(Cp2, application, permit, [app1, false]),
    false = is_started(app1, Cp1),
    false = is_started(app1, Cp2),
    true = is_started(app1, Cp3),

    %% Permit cp2 again
    ok = rpc:call(Cp2, application, permit, [app1, true]),
    false = is_started(app1, Cp1),
    false = is_started(app1, Cp3),
    true = is_started(app1, Cp2),

    %% Start app3, make sure noone starts it
    {[ok,ok,ok],[]} = 
        rpc:multicall(Cps, application, load, [app3()]),
    ?UNTIL(is_loaded(app3, Cps)),
    {[ok,ok,ok],[]} = 
        rpc:multicall(Cps, application, start, [app3, permanent]),
    ct:sleep(1000),
    false = is_started(app3, Cp1),
    false = is_started(app3, Cp2),
    false = is_started(app3, Cp3),

    %% Permit app3 on Cp3
    ok = rpc:call(Cp3, application, permit, [app3, true]),
    true = is_started(app3, Cp3),

    %% Permit app3 on Cp2, make sure it starts it
    ok = rpc:call(Cp2, application, permit, [app3, true]),
    true = is_started(app3, Cp2),
    false = is_started(app3, Cp3),

    %% Permit app3 on Cp1, make sure it doesn't start it
    ok = rpc:call(Cp1, application, permit, [app3, true]),
    false = is_started(app3, Cp1),
    true = is_started(app3, Cp2),
    false = is_started(app3, Cp3),

    %% Stop Cp2, make sure Cp1 starts app3
    stop_node_nice(Cp2),
    ?UNTIL(is_started(app3, Cp1)),

    stop_node_nice(Cp1),
    stop_node_nice(Cp3),
    ok.

%%-----------------------------------------------------------------
%% Should be started in a CC view with:
%% erl -sname XXX -rsh ctrsh where XX not in [cp1, cp2, cp3]
%%-----------------------------------------------------------------
%% Tests loading of distributed applications.
load(Conf) when is_list(Conf) ->
    NodeNames = [Ncp1, Ncp2, Ncp3] = node_names([cp1, cp2, cp3], Conf),
    NoSyncTime = config_fun_fast(config3(NodeNames)),
    WithSyncTime = config_fun(config3(NodeNames)),

    %% Test [cp1, cp2, cp3]
    {ok, Cp1} = start_node_config(Ncp1, NoSyncTime, Conf),
    {ok, Cp2} = start_node_config(Ncp2, NoSyncTime, Conf),
    {ok, Cp3} = start_node_config(Ncp3, WithSyncTime, Conf),
    Cps = [Cp1, Cp2, Cp3],
    wait_for_ready_net(),

    {[ok,ok,ok],[]} = 
        rpc:multicall(Cps, application, load, [app1(), d1(NodeNames)]),
    ?UNTIL(is_loaded(app1, Cps)),
    {[ok,ok,ok],[]} = 
        rpc:multicall(Cps, application, start, [app1, permanent]),
    ?UNTIL(is_started(app1, Cp1)),
    false = is_started(app1, Cp2),
    false = is_started(app1, Cp3),

    %% Load app1 with different specs and make sure we get an error
    {[{error,_},{error,_}],[]} = 
        rpc:multicall([Cp1, Cp2], application, load, [app1(), d1(NodeNames)]),
    {error, _} = rpc:call(Cp3, application, load, [app1(), d2(NodeNames)]),

    stop_node_nice(Cp1),
    stop_node_nice(Cp2),
    stop_node_nice(Cp3),
    ok.
    
%%-----------------------------------------------------------------
%% Same test as load/1, only with code path cache enabled.
%%-----------------------------------------------------------------
%% Tests loading of distributed applications. Code path cache enabled.
load_use_cache(Conf) when is_list(Conf) ->
    NodeNames = [Ncp1, Ncp2, Ncp3] = node_names([cp1, cp2, cp3], Conf),
    NoSyncTime = config_fun_fast(config3(NodeNames)),
    WithSyncTime = config_fun(config3(NodeNames)),

    %% Test [cp1, cp2, cp3]
    {ok, Cp1} = start_node_with_cache(Ncp1, NoSyncTime, Conf),
    {ok, Cp2} = start_node_with_cache(Ncp2, NoSyncTime, Conf),
    {ok, Cp3} = start_node_with_cache(Ncp3, WithSyncTime, Conf),
    Cps = [Cp1, Cp2, Cp3],
    wait_for_ready_net(),

    {[ok,ok,ok],[]} = 
        rpc:multicall(Cps, application, load, [app1(), d1(NodeNames)]),
    ?UNTIL(is_loaded(app1, Cps)),
    {[ok,ok,ok],[]} = 
        rpc:multicall(Cps, application, start, [app1, permanent]),
    ?UNTIL(is_started(app1, Cp1)),
    false = is_started(app1, Cp2),

    %% Load app1 with different specs and make sure we get an error
    {[{error,_},{error,_}],[]} = 
        rpc:multicall([Cp1, Cp2], application, load, [app1(), d1(NodeNames)]),
    {error, _} = rpc:call(Cp3, application, load, [app1(), d2(NodeNames)]),

    stop_node_nice(Cp1),
    stop_node_nice(Cp2),
    stop_node_nice(Cp3),
    ok.

%%-----------------------------------------------------------------
%% Should be started in a CC view with:
%% erl -sname XXX -rsh ctrsh where XX not in [cp1, cp2, cp3]
%%-----------------------------------------------------------------
%% Tests new start phases and failover.
start_phases(Conf) when is_list(Conf) ->
    %% start a help process to check the start type
    SpPid = spawn_link(?MODULE, start_phase, []),
    yes = global:register_name(start_phase, SpPid),

    NodeNames = [Ncp1, _Ncp2, _Ncp3] = node_names([cp1, cp2, cp3], Conf),
    WithSyncTime = config_fun(config_sf(NodeNames)),

    {ok, Cp1} = start_node_config_sf(Ncp1, WithSyncTime, Conf),
    wait_for_ready_net(),

    %%=============================
    %%Example 1 in the user's guide
    %%=============================
    ok = rpc:call(Cp1, application, load, [myApp, 
                                                 d_any3(myApp, NodeNames)]),
    ?UNTIL(is_loaded(myApp, Cp1)),
    ok = rpc:call(Cp1, application, start, [myApp, permanent]),
    ?UNTIL(is_started(myApp, Cp1)),
    ok = get_start_phase({sp, 0, 1, 0, 0, 1}),
    ok = rpc:call(Cp1, application, stop, [myApp]),

    %%=============================
    %%Example 2 in the user's guide
    %%=============================
    ok = rpc:call(Cp1, application, load, [topApp, 
                                                 d_any3(topApp, NodeNames)]),
    ?UNTIL(is_loaded(topApp, Cp1)),
    ok = rpc:call(Cp1, application, start, [topApp, permanent]),
    ?UNTIL(is_started(topApp, Cp1)),
    ok = get_start_phase({sp, 0, 1, 0, 0, 1}),
    ok = rpc:call(Cp1, application, stop, [topApp]),
	
    %%=============================
    %%Example 3 in the user's guide
    %%=============================
    ok = rpc:call(Cp1, application, load, [topApp2, 
                                                 d_any3(topApp2, NodeNames)]),
    ?UNTIL(is_loaded(topApp2, Cp1)),
    ok = rpc:call(Cp1, application, start, [topApp2, permanent]),
    ?UNTIL(is_started(topApp2, Cp1)),
    ok = get_start_phase({sp, 0, 2, 0, 0, 3}),
    ok = rpc:call(Cp1, application, stop, [topApp2]),

    %%=============================
    %%Example 4 in the user's guide
    %%=============================
    ok = rpc:call(Cp1, application, load, [topApp3, 
                                                 d_any3(topApp3, NodeNames)]),
    ?UNTIL(is_loaded(topApp3, Cp1)),
    ok = rpc:call(Cp1, application, start, [topApp3, permanent]),
    ?UNTIL(is_started(topApp3, Cp1)),
    ok = get_start_phase({sp, 1, 3, 3, 2, 4}),
    ok = rpc:call(Cp1, application, stop, [topApp3]),
	
    global:send(start_phase, kill),

    stop_node_nice(Cp1),
    ok.


%% Start distributed applications from within a boot script.  Test
%%  same as failover.
script_start(Conf) when is_list(Conf) ->
    %% start a help process to check the start type
    StPid = spawn_link(?MODULE, start_type, []),
    yes = global:register_name(st_type, StPid),


    %% Create the .app files and the boot script
    ok = create_app(),
    {{KernelVer,StdlibVer}, _} = create_script("latest"),
    case is_real_system(KernelVer, StdlibVer) of
	      true ->
		  Options = [];
	      false  ->
		  Options = [local]
	  end,
    ok = systools:make_script("latest", Options),

    NodeNames = [Ncp1, Ncp2, Ncp3] = node_names([cp1, cp2, cp3], Conf),
    NoSyncTime = config_fun_fast(config_fo(NodeNames)),
    WithSyncTime = config_fun(config_fo(NodeNames)),

    %% Test [cp1, cp2, cp3]
    {ok, Cp1} = start_node_boot_config(Ncp1, NoSyncTime, Conf, latest),
    {ok, Cp2} = start_node_boot_config(Ncp2, NoSyncTime, Conf, latest),
    {ok, Cp3} = start_node_boot_config(Ncp3, WithSyncTime, Conf, latest),
    wait_for_ready_net(),

    ?UNTIL(is_started(app1, Cp1)),
    ?UNTIL(is_started(app2, Cp1)),
    ?UNTIL(is_started(app_sp, Cp1)),
    false = is_started(app1, Cp2),
    ok = get_start_type(#st{normal = 9}),

    %% Stop cp1 and make sure cp2 starts app1, app2 normally (no
    %% start_phases defined) and app_sp as failover (start_phases
    %% defined)
    stop_node_nice(Cp1),
    ?UNTIL(is_started(app1, Cp2)),
    ?UNTIL(is_started(app2, Cp2)),
    ?UNTIL(is_started(app_sp, Cp2)),
    ok = get_start_type(#st{normal = 6, failover = 3}),
	
    %% Restart cp1, Cp1 takesover app1 and app2
    {ok, Cp1_2} = start_node_boot_config(Ncp1, NoSyncTime, Conf, latest),
    global:sync(),
    ?UNTIL(is_started(app1, Cp1_2)),
    false = is_started(app1, Cp2),
    ?UNTIL(is_started(app2, Cp1_2)),
    true = is_started(app_sp, Cp2),
    ?UNTIL(not is_started(app1, Cp2)),
    ?UNTIL(not is_started(app2, Cp2)),
    ok = get_start_type(#st{takeover = 6}),
	
    %% Stop cp2 and make sure cp1 starts app_sp.
    false = is_started(app_sp, Cp1_2),
    stop_node_nice(Cp2),
    ?UNTIL(is_started(app_sp, Cp1_2)),
    ok = get_start_type(#st{failover = 3}),
	
    %% Stop cp1 and make sure cp3 starts app1, app2 and app_sp
    stop_node_nice(Cp1_2),
    ?UNTIL(is_started(app_sp, Cp3)),
    ?UNTIL(is_started(app1, Cp3)),
    ?UNTIL(is_started(app2, Cp3)),
    ok = get_start_type(#st{normal = 6, failover = 3}),
	
    %% Restart cp2 and make sure it takesover app1, app2 and app_sp
    {ok, Cp2_2} = start_node_boot_config(Ncp2, NoSyncTime, Conf, latest),
    global:sync(),
    ?UNTIL(is_started(app_sp, Cp2_2)),
    ?UNTIL(is_started(app1, Cp2_2)),
    ?UNTIL(is_started(app2, Cp2_2)),
    ?UNTIL(not is_started(app_sp, Cp3)),
    ?UNTIL(not is_started(app1, Cp3)),
    ?UNTIL(not is_started(app2, Cp3)),
    ok = get_start_type(#st{takeover = 9}),

    %% Restart cp1 and make sure it takesover app1, app2
    {ok, Cp1_3} = start_node_boot_config(Ncp1, NoSyncTime, Conf, latest),
    global:sync(),
    ?UNTIL(is_started(app1, Cp1_3)),
    ?UNTIL(is_started(app2, Cp1_3)),
    false = is_started(app_sp, Cp1_3),
    true = is_started(app_sp, Cp2_2),
    ?UNTIL(not is_started(app1, Cp2_2)),
    ?UNTIL(not is_started(app2, Cp2_2)),
    ok = get_start_type(#st{takeover = 6}),

    %% Force takeover to cp1
    ok = rpc:call(Cp1_3, application, takeover, [app_sp, permanent]),
    ?UNTIL(is_started(app_sp, Cp1_3)),
    ?UNTIL(not is_started(app_sp, Cp2_2)),
    ok = get_start_type(#st{takeover = 3}),

    %% Kill one child process and see that it is started with type local
    PP = global:whereis_name({ch,3}),
    exit(PP, kill),
    ok = get_start_type(#st{local = 1}),
	
    global:send(st_type, kill),

    stop_node_nice(Cp1_3),
    stop_node_nice(Cp2_2),
    stop_node_nice(Cp3),
    
    ok = file:delete("latest.boot"),
    ok = file:delete("latest.rel"),
    ok = file:delete("latest.script"),

    ok.

%% Start local applications with permission false.  Set
%% permit true on different nodes.
permit_false_start_local(Conf) when is_list(Conf) ->
    %% This configuration does not start dist_ac.
    Config = write_config_file(fun config_perm/1, Conf),

    %% Test [cp1, cp2, cp3]
    [Ncp1, Ncp2, Ncp3] = node_names([cp1, cp2, cp3], Conf),
    {ok, Cp1} = start_node(Ncp1, Config),
    {ok, Cp2} = start_node(Ncp2, Config),
    {ok, Cp3} = start_node(Ncp3, Config),
    wait_for_ready_net(),

    {[ok,ok,ok],[]} = 
        rpc:multicall([Cp1, Cp2, Cp3], application, load, [app1()]),
    {[ok,ok,ok],[]} = 
        rpc:multicall([Cp1, Cp2, Cp3], application, start, [app1, permanent]),
    {[ok,ok,ok],[]} = 
        rpc:multicall([Cp1, Cp2, Cp3], application, load, [app2()]),
    {[ok,ok,ok],[]} = 
        rpc:multicall([Cp1, Cp2, Cp3], application, start, [app2, permanent]),
    {[ok,ok,ok],[]} = 
        rpc:multicall([Cp1, Cp2, Cp3], application, load, [app3()]),

    ct:sleep(1000),
    false = is_started(app1, Cp1),
    false = is_started(app1, Cp2),
    false = is_started(app1, Cp3),

    %% Permit a not started application
    ok = rpc:call(Cp1, application, permit, [app3, true]),
    ct:sleep(1000),
    false = is_started(app3, Cp1),
    false = is_started(app3, Cp2),
    false = is_started(app3, Cp3),

    %% Permit a not loaded application
    {error,{not_loaded,app_notloaded}} = 
	rpc:call(Cp1, application, permit, [app_notloaded, true]),
    ct:sleep(1000),
    false = is_started(app_notloaded, Cp1),
    false = is_started(app_notloaded, Cp2),
    false = is_started(app_notloaded, Cp3),

    %% Unpermit a not started application
    ok = rpc:call(Cp1, application, permit, [app3, false]),
    ct:sleep(1000),
    false = is_started(app3, Cp1),
    false = is_started(app3, Cp2),
    false = is_started(app3, Cp3),

    %% Unpermit a not loaded application
    {error,{not_loaded,app_notloaded}} = 
	rpc:call(Cp1, application, permit, [app_notloaded, false]),
    ct:sleep(1000),
    false = is_started(app_notloaded, Cp1),
    false = is_started(app_notloaded, Cp2),
    false = is_started(app_notloaded, Cp3),

    %% Permit app1 on CP1 and make sure it is started
    ok = rpc:call(Cp1, application, permit, [app1, true]),
    ?UNTIL(is_started(app1, Cp1)),
    false = is_started(app1, Cp2),
    false = is_started(app1, Cp3),

    %% Permit it again
    ok = rpc:call(Cp1, application, permit, [app1, true]),
    ct:sleep(1000),
    true = is_started(app1, Cp1),
    false = is_started(app1, Cp2),
    false = is_started(app1, Cp3),

    %% Permit app2 on CP1 and make sure it is started
    ok = rpc:call(Cp1, application, permit, [app2, true]),
    ?UNTIL(is_started(app2, Cp1)),
    false = is_started(app2, Cp2),
    false = is_started(app2, Cp3),

    %% Permit app1 on CP2 and make sure it is started
    ok = rpc:call(Cp2, application, permit, [app1, true]),
    ?UNTIL(is_started(app1, Cp2)),
    true = is_started(app1, Cp1),
    false = is_started(app1, Cp3),

    %% Unpermit app1 on CP1 and make sure it is stopped
    ok = rpc:call(Cp1, application, permit, [app1, false]),
    ?UNTIL(false =:= is_started(app1, Cp1)),
    true = is_started(app1, Cp2),
    false = is_started(app1, Cp3),

    %% Unpermit it agin
    ok = rpc:call(Cp1, application, permit, [app1, false]),
    ct:sleep(1000),
    false = is_started(app1, Cp1),
    true = is_started(app1, Cp2),
    false = is_started(app1, Cp3),

    %% Permit app1 on CP1 and make sure it is started
    ok = rpc:call(Cp1, application, permit, [app1, true]),
    ?UNTIL(is_started(app1, Cp1)),
    true = is_started(app1, Cp2),
    false = is_started(app1, Cp3),

    %% Unpermit app1 on CP1 and make sure it is stopped
    ok = rpc:call(Cp1, application, permit, [app1, false]),
    ?UNTIL(false =:= is_started(app1, Cp1)),
    true = is_started(app1, Cp2),
    false = is_started(app1, Cp3),

    %% Unpermit app1 on CP2 and make sure it is stopped
    ok = rpc:call(Cp2, application, permit, [app1, false]),
    ct:sleep(1000),
    ?UNTIL(false =:= is_started(app1, Cp2)),
    false = is_started(app1, Cp1),
    false = is_started(app1, Cp3),

    %% Unpermit app2 on CP1 and make sure it is stopped
    ok = rpc:call(Cp1, application, permit, [app2, false]),
    ?UNTIL(false =:= is_started(app2, Cp2)),
    false = is_started(app2, Cp1),
    false = is_started(app2, Cp3),

    stop_node_nice(Cp1),
    stop_node_nice(Cp2),
    stop_node_nice(Cp3),
    ok.
    

%% Start distributed applications with permission false.  Set
%% permit true on different nodes.
permit_false_start_dist(Conf) when is_list(Conf) ->
    NodeNames = [Ncp1, Ncp2, Ncp3] = node_names([cp1, cp2, cp3], Conf),
    NoSyncTime = config_fun_fast(config_perm2(NodeNames)),
    WithSyncTime = config_fun(config_perm2(NodeNames)),

    %% Test [cp1, cp2, cp3]
    {ok, Cp1} = start_node_config(Ncp1, NoSyncTime, Conf),
    {ok, Cp2} = start_node_config(Ncp2, NoSyncTime, Conf),
    {ok, Cp3} = start_node_config(Ncp3, WithSyncTime, Conf),
    Cps = [Cp1, Cp2, Cp3],
    wait_for_ready_net(),

    {[ok,ok,ok],[]} = 
        rpc:multicall(Cps, application, load, [app1()]),
    ?UNTIL(is_loaded(app1, Cps)),
    {[ok,ok,ok],[]} = 
        rpc:multicall(Cps, application, start, [app1, permanent]),
    {[ok,ok,ok],[]} = 
        rpc:multicall(Cps, application, load, [app2()]),

    ct:sleep(1000),
    false = is_started(app1, Cp1),
    false = is_started(app1, Cp2),
    false = is_started(app1, Cp3),

    %% Permit a not started application
    ok = rpc:call(Cp1, application, permit, [app2, true]),
    ct:sleep(1000),
    false = is_started(app2, Cp1),
    false = is_started(app2, Cp2),
    false = is_started(app2, Cp3),

    %% Permit a not loaded application
    {error,{not_loaded,app3}} = 
	rpc:call(Cp1, application, permit, [app3, true]),
    ct:sleep(1000),
    false = is_started(app3, Cp1),
    false = is_started(app3, Cp2),
    false = is_started(app3, Cp3),

    %% Unpermit a not started application
    ok = rpc:call(Cp1, application, permit, [app2, false]),
    {[ok,ok,ok],[]} = 
        rpc:multicall([Cp1, Cp2, Cp3], application, start, [app2, permanent]),
    ct:sleep(1000),
    false = is_started(app2, Cp1),
    false = is_started(app2, Cp2),
    false = is_started(app2, Cp3),

    %% Unpermit a not loaded application
    {error,{not_loaded,app3}} = 
	rpc:call(Cp1, application, permit, [app3, false]),
    {[ok,ok,ok],[]} = 
        rpc:multicall(Cps, application, load, [app3()]),
    ?UNTIL(is_loaded(app3, Cps)),
    {[ok,ok,ok],[]} = 
        rpc:multicall(Cps, application, start, [app3, permanent]),
    ct:sleep(1000),
    false = is_started(app3, Cp1),
    false = is_started(app3, Cp2),
    false = is_started(app3, Cp3),

    %% Permit app1 on CP1 and make sure it is started
    ok = rpc:call(Cp1, application, permit, [app1, true]),
    ?UNTIL(is_started(app1, Cp1)),
    false = is_started(app1, Cp2),
    false = is_started(app1, Cp3),

    %% Permit it again
    ok = rpc:call(Cp1, application, permit, [app1, true]),
    ?UNTIL(is_started(app1, Cp1)),
    false = is_started(app1, Cp2),
    false = is_started(app1, Cp3),

    %% Permit app2 on CP1 and make sure it is started
    ok = rpc:call(Cp1, application, permit, [app2, true]),
    ?UNTIL(is_started(app2, Cp1)),
    false = is_started(app2, Cp2),
    false = is_started(app2, Cp3),

    %% Permit app1 on CP2 and make sure it is not started
    ok = rpc:call(Cp2, application, permit, [app1, true]),
    ct:sleep(1000),
    true = is_started(app1, Cp1),
    false = is_started(app1, Cp2),
    false = is_started(app1, Cp3),

    %% Crash CP1 and make sure app1, but not app2, is started on CP2
    stop_node_nice(Cp1),
    ?UNTIL(is_started(app1, Cp2)),
    false = is_started(app2, Cp2),

    %% Restart CP1 again, check nothing is running on it
    {ok, Cp1_2} = start_node_config(Ncp1, NoSyncTime, Conf),
    global:sync(),
    ok = rpc:call(Cp1_2, application, load, [app1()]),
    ?UNTIL(is_loaded(app1, Cp1_2)),
    ok = rpc:call(Cp1_2, application, start, [app1, permanent]),
    ok = rpc:call(Cp1_2, application, load, [app2()]),
    ?UNTIL(is_loaded(app2, Cp1_2)),
    ok = rpc:call(Cp1_2, application, start, [app2, permanent]),
    ok = rpc:call(Cp1_2, application, load, [app3()]),
    ?UNTIL(is_loaded(app3, Cp1_2)),
    ok = rpc:call(Cp1_2, application, start, [app3, permanent]),
    false = is_started(app1, Cp1_2),
    false = is_started(app2, Cp1_2),

    %% Permit app3 on CP3 and make sure it is started
    ok = rpc:call(Cp3, application, permit, [app3, true]),
    ?UNTIL(is_started(app3, Cp3)),
    false = is_started(app3, Cp1_2),
    false = is_started(app3, Cp2),

    %% Permit app3 on CP1 and make sure it is moved there from CP3
    ok = rpc:call(Cp1_2, application, permit, [app3, true]),
    ?UNTIL(is_started(app3, Cp1_2)),
    false = is_started(app3, Cp2),
    false = is_started(app3, Cp3),

    %% Unpermit app3 on CP3 and CP1 and make sure it is stopped
    ok = rpc:call(Cp3, application, permit, [app3, false]),
    ok = rpc:call(Cp1_2, application, permit, [app3, false]),
    ?UNTIL(false =:= is_started(app3, Cp1_2)),
    false = is_started(app3, Cp2),
    false = is_started(app3, Cp3),

    stop_node_nice(Cp1_2),
    stop_node_nice(Cp2),
    stop_node_nice(Cp3),
    ok.

%% app1 distributed as [cp1, cp2].  Call application:start(app1) on
%% cp2, but not on cp1.  Kill cp1.  Make sure app1 is started on cp2.
nodedown_start(Conf) when is_list(Conf) ->
    NodeNames = [Ncp1, Ncp2] = node_names([cp1, cp2], Conf),
    NoSyncTime = config_fun_fast(config4(NodeNames)),
    WithSyncTime = config_fun(config4(NodeNames)),

    %% Test [cp1, cp2]
    {ok, Cp1} = start_node_config(Ncp1, NoSyncTime, Conf),
    {ok, Cp2} = start_node_config(Ncp2, WithSyncTime, Conf),
    wait_for_ready_net(),

    %% Start app1 and make sure cp1 starts it
    {[ok,ok],[]} = 
        rpc:multicall([Cp1, Cp2], application, load, [app1()]),
    _ = rpc:cast(Cp2, application, start, [app1, permanent]),
    ct:sleep(1000),

    %% Crash CP1 and make sure app1 is started on CP2
    stop_node_nice(Cp1),
    ?UNTIL(is_started(app1, Cp2)),
    
    stop_node_nice(Cp2),
    ok.


%% Test application:ensure_started/1.
ensure_started(_Conf) ->

    {ok, Fd} = file:open("app1.app", [write]),
    w_app1(Fd),
    file:close(Fd),

    ok = application:ensure_started(app1),
    ok = application:ensure_started(app1),
    {error, {already_started, app1}} = application:start(app1),
    ok = application:stop(app1),
    {error,{"no such file or directory", _ }} = application:ensure_started(hopefully_not_an_existing_app_file),

    ok = application:ensure_started(app1, permanent),
    ok = application:ensure_started(app1, permanent),
    ok = application:stop(app1),
    ok = application:unload(app1),
    ok.

%% Test application:ensure_all_started/1-2.
ensure_all_started(_Conf) ->

    {ok, Fd1} = file:open("app1.app", [write]),
    w_app1(Fd1),
    file:close(Fd1),
    {ok, Fd9} = file:open("app9.app", [write]),
    w_app9(Fd9),
    file:close(Fd9),
    {ok, Fd10} = file:open("app10.app", [write]),
    w_app10_dep9(Fd10),
    file:close(Fd10),
    {ok, FdErr} = file:open("app_chain_error.app", [write]),
    w_app(FdErr, app_chain_error()),
    file:close(FdErr),
    {ok, FdErr2} = file:open("app_chain_error2.app", [write]),
    w_app(FdErr2, app_chain_error2()),
    file:close(FdErr2),

    %% Single app start/stop
    false = lists:keyfind(app1, 1, application:which_applications()),
    {ok, [app1]} = application:ensure_all_started(app1), % app1 started
    {app1, _, _} = lists:keyfind(app1, 1, application:which_applications()),
    {ok, []} = application:ensure_all_started(app1), % no start needed
    ok = application:stop(app1),
    false = lists:keyfind(app1, 1, application:which_applications()),
    ok = application:unload(app1),

    %% App or dependency not found.
    Name = hopefully_not_an_existing_app_file,
    {error,{Name, {"no such file or directory", _ }}} =
	application:ensure_all_started(Name),

    %% Start dependencies.
    {error, {not_started, app9}} = application:start(app10),
    {ok, [app9,app10]} = application:ensure_all_started(app10, temporary),
    {app9, _, _} = lists:keyfind(app9, 1, application:which_applications()),
    {app10, _, _} = lists:keyfind(app10, 1, application:which_applications()),
    %% Only report apps/dependencies that actually needed to start
    ok = application:stop(app10),
    ok = application:unload(app10),
    {ok, [app10]} = application:ensure_all_started(app10, temporary),
    ok = application:stop(app9),
    ok = application:unload(app9),
    ok = application:stop(app10),
    ok = application:unload(app10),

    %% Deeper failure chain. We have the following dependencies:
    %% app_chain_error -> app_chain_error2
    %% app_chain_error2 -> app10
    %% app_chain_error2 -> hopefully_not_an_existing_app
    %% app10 -> app 9
    %% First we have none running and we expect to have neither app9
    %% nor app10 running after failing to start
    %% hopefully_not_an_existing_app
    {error, {hopefully_not_an_existing_app, {"no such file or directory", _}}}=
	application:ensure_all_started(app_chain_error),
    false = lists:keyfind(app9, 1, application:which_applications()),
    false = lists:keyfind(app10, 1, application:which_applications()),
    false = lists:keyfind(app_chain_error2,1,application:which_applications()),
    false = lists:keyfind(app_chain_error, 1, application:which_applications()),
    %% Here we will have app9 already running, and app10 should be
    %% able to boot fine.
    %% In this dependency failing, we expect app9 to still be running, but
    %% not app10 after failing to start hopefully_not_an_existing_app
    {ok, [app9]} = application:ensure_all_started(app9, temporary),
    {error, {hopefully_not_an_existing_app, {"no such file or directory", _}}}=
	application:ensure_all_started(app_chain_error),
    {app9, _, _} = lists:keyfind(app9, 1, application:which_applications()),
    false = lists:keyfind(app10, 1, application:which_applications()),
    false = lists:keyfind(app_chain_error2,1,application:which_applications()),
    false = lists:keyfind(app_chain_error, 1, application:which_applications()),
    ok = application:stop(app9),
    ok = application:unload(app9),
    ok = application:unload(app10),
    ok = application:unload(app_chain_error2),
    ok = application:unload(app_chain_error),
    ok.

%%%-----------------------------------------------------------------
%%% Testing of reported bugs and other tickets.
%%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Ticket: OTP-1586
%% Slogan: recursive load of applications fails
%%-----------------------------------------------------------------
%% Test recursive load of applications.
otp_1586(Conf) when is_list(Conf) ->
    Dir = proplists:get_value(priv_dir,Conf),
    {ok, Fd} = file:open(filename:join(Dir, "app5.app"), [write]),
    w_app5(Fd),
    file:close(Fd),
    try
	true = code:add_patha(Dir),
	ok = application:load(app4()),
	ok = application:unload(app4)
    after
	_ = code:del_path(Dir)
    end.

%%-----------------------------------------------------------------
%% Ticket: OTP-2078
%% Slogan: start of distrib apps fails when the nodes start
%%         simultaneously
%%-----------------------------------------------------------------
%% Test start of distrib apps fails when the nodes start simultaneously.
otp_2078(Conf) when is_list(Conf) ->
    NodeNames = [Ncp1, Ncp2] = node_names([cp1, cp2], Conf),
    NoSyncTime = config_fun_fast(config4(NodeNames)),
    WithSyncTime = config_fun(config4(NodeNames)),

    %% Test [cp1, cp2]
    {ok, Cp1} = start_node_config(Ncp1, NoSyncTime, Conf),
    {ok, Cp2} = start_node_config(Ncp2, WithSyncTime, Conf),
    Cps = [Cp1, Cp2],
    wait_for_ready_net(),

    %% Start app1 and make sure cp1 starts it
    {[ok,ok],[]} = 
        rpc:multicall(Cps, application, load, [app1()]),
    ?UNTIL(is_loaded(app1, Cps)),
    ok = rpc:call(Cp1, application, start, [app1, permanent]),
    ?UNTIL(is_started(app1, Cp1)),
    false = is_started(app1, Cp2),

    %% Start app1 on cp2; make sure it works (the bug was that this start
    %% returned error)
    ok = rpc:call(Cp2, application, start, [app1, permanent]),
    true = is_started(app1, Cp1),
    false = is_started(app1, Cp2),
    
    stop_node_nice(Cp1),
    stop_node_nice(Cp2),
    ok.

%% Test change of configuration parameters without changing code.
otp_2012(Conf) when is_list(Conf) ->
    %% start a help process to check the config change
    CcPid = spawn_link(?MODULE, conf_change, []),
    yes = global:register_name(conf_change, CcPid),

    %% Write a .app file
    {ok, Fd} = file:open("app1.app", [write]),
    w_app1(Fd),
    file:close(Fd),
    {ok, Fd2} = file:open("app2.app", [write]),
    w_app1(Fd2),
    file:close(Fd2),

    %% Start app1
    ok = application:load(app1()),
    ok = application:start(app1, permanent),

    %% Read the current configuration parameters, and change them
    EnvBefore = application_controller:prep_config_change(),
    application_controller:test_change_apps([app1],[[{app1,[{new1, hi}, 
                                                            {new2, moi}]}]]),
    ok = application_controller:config_change(EnvBefore),
    ok = get_conf_change([{[], [{new1, hi}, {new2, moi}], []}]),
    
    %% Start app2
    ok = application:load(app2()),
    ok = application:start(app2, permanent),

    %% Read the current configuration parameters, and change them again
    EnvBefore2 = application_controller:prep_config_change(),
    application_controller:test_change_apps([app1],[[{app1,[{new1, hello}, 
                                                            {new3, mors}]}]]),
    application_controller:test_change_apps([app2],[[{app2,[{new1, si}, 
                                                            {new2, no}]}]]),
    _EnvBefore22 = application_controller:prep_config_change(),
    ok = application_controller:config_change(EnvBefore2),
    
    ok = get_conf_change([{[],[{new1,si},{new2,no}],[]},
                                {[{new1,hello}],[{new3,mors}],[new2]}]),
    
    ok = application:stop(app1),
    ok = application:stop(app2),
    ok.

%%-----------------------------------------------------------------
%% Ticket: OTP-2718
%% Slogan: transient app which fails during start is ignored
%%-----------------------------------------------------------------
%% Test fail of transient app at start.
otp_2718(Conf) when is_list(Conf) ->
    {ok, Cp1} = start_node_args(cp1, "-pa " ++ proplists:get_value(data_dir,Conf)),
    wait_for_ready_net(),

    %% normal exit from the application
    ok = rpc:call(Cp1, application, load, [app_trans_normal()]),
    ?UNTIL(is_loaded(trans_normal, Cp1)),
    {error, {{'EXIT',normal},_}} =
	rpc:call(Cp1, application, start, [trans_normal, transient]),
    ct:sleep(2000),
    false = is_started(trans_normal, Cp1),

    %% abnormal exit from the application
    ok = rpc:call(Cp1, application, load, [app_trans_abnormal()]),
    {error, {bad_return,{{trans_abnormal_sup,start,[normal,[]]},
			       {'EXIT',abnormal}}}} =
	rpc:call(Cp1, application, start, [trans_abnormal, transient]),
    ct:sleep(3000),
    {badrpc,nodedown} = which_applications(Cp1),
    ok.

%%-----------------------------------------------------------------
%% Ticket: OTP-2973
%% Slogan: application:start does not test if an appl is already starting...
%%-----------------------------------------------------------------
%% Test of two processes simultanously starting the same application.
otp_2973(Conf) when is_list(Conf) ->
    %% Write a .app file
    {ok, Fd} = file:open("app0.app", [write]),
    w_app(Fd, app0()),
    file:close(Fd),

    Pid1 =  spawn_link(?MODULE, init2973, []),
    Pid2 =  spawn_link(?MODULE, init2973, []),

    Pid1 ! {start, self(), app0},
    Pid2 ! {start, self(), app0},
    
    {Res1, Res2} = receive 
			     {Pid1, res, Res1x} ->
				 receive 
				     {Pid2, res, Res2x} ->
					 {Res1x, Res2x}
				   after 2000 ->
					   ct:fail(timeout_pid2)
				   end;
			     {Pid2, res, Res2x} ->
				 receive 
				     {Pid1, res, Res1x} ->
					 {Res1x, Res2x}
				 after 2000 ->
					 ct:fail(timeout_pid1)
				 end
			 end,

    %% Stop it. Inteferes with other global.
    ok = application:stop(app0),

    %% Test result.
    case {Res1, Res2} of
	{ok, ok} ->
	    ok;
	_ ->
	    Txt = io_lib:format("Illegal results from start: ~p ~p ",
				      [Res1, Res2]),
	    ct:fail(lists:flatten(Txt))
    end,


    %% Write a .app file
    {ok, Fda} = file:open("app_start_error.app", [write]),
    w_app_start_error(Fda),
    file:close(Fda),

    Pid1 ! {start, self(), app_start_error},
    Pid2 ! {start, self(), app_start_error},
    
    {Res1a, Res2a} = receive 
			       {Pid1, res, Res1y} ->
				   receive 
				       {Pid2, res, Res2y} ->
					   {Res1y, Res2y}
				   after 2000 ->
					   ct:fail(timeout_pid2)
				   end;
			       {Pid2, res, Res2y} ->
				   receive 
				       {Pid1, res, Res1y} ->
					   {Res1y, Res2y}
				   after 2000 ->
					   ct:fail(timeout_pid1)
				   end
			   end,

    case {Res1a, Res2a} of
	{{error,{'start error',{app_start_error,start,[normal,[]]}}}, 
	 {error,{'start error',{app_start_error,start,[normal,[]]}}}} ->
	    ok;
	_ ->
	    Txta = io_lib:format("Illegal results from start ~p ~p ",[Res1a, Res2a]),
	    ct:fail(lists:flatten(Txta))
    end,

    ok.



%%-----------------------------------------------------------------
%% Ticket: OTP-3184
%% Slogan: crash the node if permanent appl has illegal env parameter values
%%-----------------------------------------------------------------
%% When a distributed application is started the permit flag is checked
%% that the permit flag is not changed during the start.
%% The check must only be made if the application is started on the own node.
otp_3184(Conf) when is_list(Conf) ->
    NodeNames = [Ncp1, Ncp2] = node_names([cp1, cp2], Conf),
    NoSyncTime = config_fun_fast(config3184(NodeNames)),
    WithSyncTime = config_fun(config3184(NodeNames)),

    %% Test [cp1, cp2]
    {ok, Cp1} = start_node_config(Ncp1, NoSyncTime, Conf),
    {ok, Cp2} = start_node_config(Ncp2, WithSyncTime, Conf),
    wait_for_ready_net(),

    %% Start app1 and make sure it is not started
    {[ok,ok],[]} = 
        rpc:multicall([Cp1, Cp2], application, load, [app1()]),
    ct:sleep(3000),
    false = is_started(app1, Cp1),
    false = is_started(app1, Cp2),

    %% Start app1 on cp1
    ok = rpc:call(Cp1, application, permit, [app1, true]),
    ok = rpc:call(Cp1, application, start, [app1, permanent]),
    ok = rpc:call(Cp2, application, start, [app1, permanent]),
    ?UNTIL(is_started(app1, Cp1)),
    false = is_started(app1, Cp2),
    
    %% Check that the application is marked as running in application_controller
    X = rpc:call(Cp1, application_controller, info, []),
    {value, {running, Xrunning}} = lists:keysearch(running, 1, X),
    {value, Xapp1} = lists:keysearch(app1, 1, Xrunning),
    {app1, _Xpid} = Xapp1,

    Y = rpc:call(Cp2, application_controller, info, []),
    {value, {running, Yrunning}} = lists:keysearch(running, 1, Y),
    {value, Yapp1} = lists:keysearch(app1, 1, Yrunning),
    {app1, {distributed, Cp1}} = Yapp1,

    stop_node_nice(Cp1),
    stop_node_nice(Cp2),
    ok.

%%-----------------------------------------------------------------
%% Ticket: OTP-3002
%% Slogan: crash the node if permanent appl has illegal env parameter values
%%-----------------------------------------------------------------
%% crash the node if permanent appl has illegal env parameter values.
otp_3002(Conf) when is_list(Conf) ->
    %% Create the boot script
    {{KernelVer,StdlibVer}, {LatestDir, LatestName}} =
	create_script_3002("script_3002"),
    ct:pal(?HI_VERBOSITY, "LatestDir = ~p~n", [LatestDir]),
    ct:pal(?HI_VERBOSITY, "LatestName = ~p~n", [LatestName]),

    case is_real_system(KernelVer, StdlibVer) of
	      true ->
		  Options = [];
	      false  ->
		  Options = [local]
	  end,

    ok = systools:make_script("script_3002", Options),
    ok = systools:script2boot("script_3002"),

    {error, timeout} = start_node_boot_3002(cp1, "script_3002"),

    ok = file:delete("script_3002.boot"),
    ok = file:delete("script_3002.rel"),
    ok = file:delete("script_3002.script"),
    ok.

%%-----------------------------------------------------------------
%% Ticket: OTP-4066
%% Slogan: dist_ac crashed if a distributed application that it
%%         didn't know of was stopped by another dist_ac (bad_match
%%         when it received dist_ac_app_stopped).
%%-----------------------------------------------------------------

%% Check that application stop don't cause dist_ac crash.
otp_4066(Conf) when is_list(Conf) ->
    %% Write config files
    [Ncp1, Ncp2] = node_names([cp1, cp2], Conf),
    Host = from($@, atom_to_list(node())),
    Cp1 = list_to_atom(Ncp1 ++ "@" ++ Host),
    Cp2 = list_to_atom(Ncp2 ++ "@" ++ Host),
    AllNodes = [Cp1, Cp2],
    App1Nodes = {app1, AllNodes},

    Dir = proplists:get_value(priv_dir,Conf),
    {ok, FdC} = file:open(filename:join(Dir, "otp_4066.config"), [write]),
    write_config(FdC, config_4066(AllNodes, 5000, [App1Nodes])),
    file:close(FdC),

    %% Write the app1.app file
    {ok, FdA12} = file:open(filename:join(Dir, "app1.app"), [write]),
    w_app1(FdA12),
    file:close(FdA12),

    Args1 = "-pa " ++ Dir ++ " -config " ++ filename:join(Dir, "otp_4066"),
    Args2 =  "-pa " ++ Dir ++ " -kernel start_dist_ac true",

    {ok, Cp2} = start_node_args(Ncp2, Args2),
    %% Cp1 syncs with cp2 (which is known to be up).
    {ok, Cp1} = start_node_args(Ncp1, Args1),
    wait_for_ready_net(),

    ok = rpc:call(Cp1, application, start, [app1]),
    wait_until_started(app1, [Cp1]),
    io:format("--- App1 started at Cp1 ---~n", []),
    print_dac_state(AllNodes),

    %% Cp2 previously crashed on this stop
    ok = rpc:call(Cp1, application, stop, [app1]),
    wait_until_stopped(app1, [Cp1]),
    io:format("--- App1 stopped at Cp1 ---~n", []),
    print_dac_state(AllNodes),

    ok = rpc:call(Cp1, application, start, [app1]),
    wait_until_started(app1, [Cp1]),
    io:format("--- App1 started at Cp1 ---~n", []),
    print_dac_state(AllNodes),

    ok = rpc:call(Cp2, application, load, [app1, App1Nodes]),
    ok = rpc:call(Cp2, application, start, [app1]),
    io:format("--- App1 started at Cp2 ---~n", []),
    print_dac_state(AllNodes),


    stop_node_nice(Cp1),
    wait_until_started(app1, [Cp2]),
    io:format("--- Cp1 crashed; failover to Cp2  ---~n", []),
    print_dac_state(Cp2),

    stop_node_nice(Cp2),
    ok.

config_4066(SyncNodesOptional, SyncNodesTimeout, Distributed) ->
    [{kernel, [{sync_nodes_optional,SyncNodesOptional},
	       {sync_nodes_timeout, SyncNodesTimeout},
	       {distributed, Distributed}]}].

write_config(Fd, Config) ->
    io:format(Fd, "~p.~n", [Config]).

print_dac_state(Node) when is_atom(Node) ->
    State = gen_server:call({dist_ac, Node}, info),
    io:format(" * dist_ac state on node ~p:~n    ~p~n",
                       [Node, State]);
print_dac_state(Nodes) when is_list(Nodes) ->
    lists:foreach(fun (N) -> print_dac_state(N) end, Nodes).
				

%%-----------------------------------------------------------------
%% Ticket: OTP-4227
%% Slogan: Bad return value from application.
%%-----------------------------------------------------------------
%% Test start of depending app when required app crashed.
otp_4227(Conf) when is_list(Conf) ->
    NodeNames = [Ncp1, Ncp2] = node_names([cp1, cp2], Conf),
    NoSyncTime = config_fun_fast(config_4227(NodeNames)),
    WithSyncTime = config_fun(config_4227(NodeNames)),

    %% Test [cp1, cp2]
    {ok, Cp1} = start_node_config(Ncp1, NoSyncTime, Conf),
    {ok, Cp2} = start_node_config(Ncp2, WithSyncTime, Conf),
    Cps = [Cp1, Cp2],
    wait_for_ready_net(),

    %% Try to start app10 which should fail since app9 is not started
    {[ok,ok],[]} = 
        rpc:multicall(Cps, application, load, [app9()]),
    ?UNTIL(is_loaded(app9, Cps)),
    {[ok,ok],[]} = 
        rpc:multicall(Cps, application, load, [app10_dep9()]),
    {error, {not_started, app9}} = 
	rpc:call(Cp1, application, start, [app10]),

    %% Start app9 and brutally kill it, then try to start app10
    ok = rpc:call(Cp1, application, start, [app9]),
    ct:sleep(1000),
    Pid9 = rpc:call(Cp1, erlang, whereis, [ch_sup19]),
    true = erlang:is_pid(Pid9),
    true = erlang:exit(Pid9, kill),
    ct:sleep(1000),

    %% This gave {error, no_report} before the patch
    {error, {not_running, app9}} = 
	rpc:call(Cp1, application, start, [app10]),

    stop_node_nice(Cp1),
    stop_node_nice(Cp2),
    ok.

config_4227([Ncp1, Ncp2]) ->
    fun(Fd, SyncNodesTimeout) ->
            M = from($@, atom_to_list(node())),
            io:format(Fd, 
                      "[{kernel, "
                      "  [{sync_nodes_optional, ['~s@~s','~s@~s']},"
                      "   {sync_nodes_timeout, ~w},"
                      "   {start_dist_ac, true},"
                      "   {distributed, "
                      "    [{app9,  ['~s@~s','~s@~s']}, "
                      "     {app10, ['~s@~s','~s@~s']}]}]}].~n",
                      [Ncp1, M, Ncp2, M,  
                       SyncNodesTimeout,  
                       Ncp1, M, Ncp2, M,  
                       Ncp1, M, Ncp2, M])
    end.

%%-----------------------------------------------------------------
%% Ticket: OTP-5363
%% Slogan: Slow termination in application_master
%%-----------------------------------------------------------------
otp_5363(Conf) when is_list(Conf) ->
    %% When stopping an application, all processes having the
    %% application master as group leader should get killed.
    %% The killing was done in an inefficient way.
    %%   In this test case, we will not test the efficiency of
    %% the code, but only that the correct processes ARE killed.

    OldPath = code:get_path(),
    code:add_patha(proplists:get_value(data_dir,Conf)),
    try
	ok = application:load(app_group_leader()),
	ok = application:start(group_leader),
	case whereis(nisse) of
		  Pid when is_pid(Pid) ->
		      Mref = erlang:monitor(process, Pid),
		      ok = application:stop(group_leader),
		      receive
			  {'DOWN',Mref,_,_,_} -> ok
		      end,
		      undefined = whereis(nisse);
		  Bad ->
		      io:format("~p\n", [Bad]),
		      ct:fail(failed)
	      end
    after
        code:set_path(OldPath)
    end,
    ok.

%%-----------------------------------------------------------------
%% Ticket: OTP-5606
%% Slogan: Problems with starting a distributed application
%%-----------------------------------------------------------------
%% Test of several processes simultaneously starting the same
%% distributed application.
otp_5606(Conf) when is_list(Conf) ->

    %% Write a config file
    Dir = proplists:get_value(priv_dir, Conf),
    {ok, Fd} = file:open(filename:join(Dir, "sys.config"), [write]),
    NodeNames = [Ncp1, Ncp2] = node_names([cp1, cp2], Conf),
    (config4(NodeNames))(Fd, 10000),
    file:close(Fd),
    Config = filename:join(Dir, "sys"),
    
    %% Test [cp1, cp2]
    {ok, Cp1} = start_node(Ncp1, Config),
    {ok, Cp2} = start_node(Ncp2, Config),
    Cps = [Cp1, Cp2],
    wait_for_ready_net(),

    %% Load app1 on both nodes
    {[ok, ok], []} =
	rpc:multicall(Cps, application, load, [app1()]),

    %% Attempt to start app1 from different processes simultaneously
    Pid11 =  spawn_link(Cp1, ?MODULE, loop5606, [self()]),
    Pid12 =  spawn_link(Cp1, ?MODULE, loop5606, [self()]),
    Pid13 =  spawn_link(Cp1, ?MODULE, loop5606, [self()]),
    Pid2  =  spawn_link(Cp2, ?MODULE, loop5606, [self()]),

    Pid2 ! start,
    Pid11 ! start,
    Pid12 ! start,
    Pid13 ! start,

    ResL = otp_5606_loop([]),

    case ResL of
	[ok, ok, ok, ok] ->
	    ok;
	[Res1, Res2, Res3, Res4] ->
	    Txt = io_lib:format("Illegal results from start ~p ~p ~p ~p",
				[Res1, Res2, Res3, Res4]),
	    ct:fail(lists:flatten(Txt))
    end,

    {error, {already_started, app1}} = 
	rpc:call(Cp1, application, start, [app1]),

    stop_node_nice(Cp1),
    stop_node_nice(Cp2),
    ok.

otp_5606_loop(ResL) when length(ResL)<4 ->
    receive 
	{_Pid, Res} ->
	    otp_5606_loop([Res|ResL])
    after 5000 ->
	    ct:fail(timeout_waiting_for_res)
    end;
otp_5606_loop(ResL) ->
    ResL.

loop5606(Pid) ->
    receive
	start ->
	    Res = application:start(app1),
	    Pid ! {self(), Res}
    end.
	    
%% Tests get_env/* functions.
get_env(Conf) when is_list(Conf) ->
    {ok, _}   = application:get_env(kernel, error_logger),
    undefined = application:get_env(undefined_app, a),
    undefined = application:get_env(kernel, error_logger_xyz),
    default   = application:get_env(kernel, error_logger_xyz, default),
    ok.

%%-----------------------------------------------------------------
%% Should be started in a CC view with:
%% erl -sname XXX -rsh ctrsh where XX not in [cp1, cp2, cp3]
%%-----------------------------------------------------------------
%% Tests read the .app keys.
get_key(Conf) when is_list(Conf) ->
    NodeNames = [Ncp1, _Ncp2, _Ncp3] = node_names([cp1, cp2, cp3], Conf),
    WithSyncTime = config_fun(config_inc(NodeNames)),

    %% Test [cp1, cp2, cp3]
    {ok, Cp1} = start_node_config(Ncp1, WithSyncTime, Conf),

    ok = rpc:call(Cp1, application, load, [appinc(), d3(NodeNames)]),
    ?UNTIL(is_loaded(appinc, Cp1)),
    ok = rpc:call(Cp1, application, start, [appinc, permanent]),
    ?UNTIL(is_started(appinc, Cp1)),
    
    {ok, "Test of new app file, including appnew"} =
	rpc:call(Cp1, application, get_key, [appinc, description]),
    {ok, "CXC 138 ai"} = rpc:call(Cp1, application, get_key, [appinc ,id]),
    {ok, "2.0"} = rpc:call(Cp1, application, get_key, [appinc, vsn]),
    {ok, [kernel]} = rpc:call(Cp1, application, get_key, [appinc, applications]),
    {ok, [appinc1, appinc2]} = 
	rpc:call(Cp1, application, get_key, [appinc, included_applications]),
    {ok, []} = rpc:call(Cp1, application, get_key, [appinc, registered]),
    {ok, [{init, [kalle]}, {takeover, []}, {go, [sune]}]} =
	rpc:call(Cp1, application, get_key, [appinc, start_phases]),
    {ok, Env} = rpc:call(Cp1, application, get_key, [appinc ,env]),
    [{included_applications,[appinc1,appinc2]},
	   {own2,val2},{own_env1,value1}] = lists:sort(Env),
    {ok, []} = rpc:call(Cp1, application, get_key, [appinc, modules]),
    {ok, {application_starter, [ch_sup, {appinc, 41, 43}] }} = 
	rpc:call(Cp1, application, get_key, [appinc, mod]),
    {ok, infinity} = rpc:call(Cp1, application, get_key, [appinc, maxP]),
    {ok, infinity} = rpc:call(Cp1, application, get_key, [appinc, maxT]),
    undefined = rpc:call(Cp1, application, get_key, [appinc, very_unknown]),

    {ok, [{description, "Test of new app file, including appnew"}, 
		{id, "CXC 138 ai"}, 
		{vsn, "2.0"}, 
		{modules, []}, 
		{maxP, infinity}, 
		{maxT, infinity}, 
		{registered, []}, 
		{included_applications, [appinc1, appinc2]}, 
		{applications, [kernel]}, 
		{env, Env}, 
		{mod, {application_starter, [ch_sup, {appinc, 41, 43}] }}, 
		{start_phases, [{init, [kalle]}, {takeover, []}, {go, [sune]}]}]} = 
	rpc:call(Cp1, application, get_all_key, [appinc]),
    [{included_applications,[appinc1,appinc2]},
	   {own2,val2},{own_env1,value1}] = lists:sort(Env),

    {ok, "Test of new app file, including appnew"} =
	gen_server:call({global, {ch,41}}, {get_pid_key, description}),
    {ok, "CXC 138 ai"} = 
	gen_server:call({global, {ch,41}}, {get_pid_key, id}),
    {ok, "2.0"} = 
	gen_server:call({global, {ch,41}}, {get_pid_key, vsn}),
    {ok, [kernel]} = 
	gen_server:call({global, {ch,41}}, {get_pid_key, applications}),
    {ok, [appinc1, appinc2]} = 
	gen_server:call({global, {ch,41}}, {get_pid_key, included_applications}),
    {ok, []} = 
	gen_server:call({global, {ch,41}}, {get_pid_key, registered}),
    {ok, [{init, [kalle]}, {takeover, []}, {go, [sune]}]} =
	gen_server:call({global, {ch,41}}, {get_pid_key, start_phases}),
    {ok, Env} = gen_server:call({global, {ch,41}}, {get_pid_key, env}),
    [{included_applications,[appinc1,appinc2]},
	   {own2,val2},{own_env1,value1}] = lists:sort(Env),
    {ok, []} = 
	gen_server:call({global, {ch,41}}, {get_pid_key, modules}),
    {ok, {application_starter, [ch_sup, {appinc, 41, 43}] }} = 
	gen_server:call({global, {ch,41}}, {get_pid_key, mod}),
    {ok, infinity} = 
	gen_server:call({global, {ch,41}}, {get_pid_key, maxP}),
    {ok, infinity} = 
	gen_server:call({global, {ch,41}}, {get_pid_key, maxT}),
    undefined = 
	gen_server:call({global, {ch,41}}, {get_pid_key, very_unknown}),



    {ok, [{description, "Test of new app file, including appnew"}, 
		{id, "CXC 138 ai"}, 
		{vsn, "2.0"}, 
		{modules, []}, 
		{maxP, infinity}, 
		{maxT, infinity}, 
		{registered, []}, 
		{included_applications, [appinc1, appinc2]}, 
		{applications, [kernel]}, 
		{env, Env}, 
		{mod, {application_starter, [ch_sup, {appinc, 41, 43}] }}, 
		{start_phases, [{init, [kalle]}, {takeover, []}, {go, [sune]}]}]} = 
	gen_server:call({global, {ch,41}}, get_pid_all_key),
    [{included_applications,[appinc1,appinc2]},
	   {own2,val2},{own_env1,value1}] = lists:sort(Env),
    
    stop_node_nice(Cp1),
    ok.

%%%-----------------------------------------------------------------
%%% Testing of change of distributed parameter.
%%%-----------------------------------------------------------------

%% Test change of distributed parameter.
distr_changed_tc1(Conf) when is_list(Conf) ->

    {OldKernel, OldEnv, {Cp1, Cp2, Cp3}, {_Ncp1, _Ncp2, _Ncp3}, _Config2} = 
	distr_changed_prep(Conf),

    NewDist = {distributed, [{app1, [Cp3]},
				   {app2, 5000, [Cp2]},
				   {app3, [Cp3, {Cp1, Cp2}]},
				   {app6, [Cp1, {Cp3, Cp2}]},
				   {app7, 1000, [Cp3]},
				   {app8, [Cp1, {Cp2, Cp3}]}]},
    
    NewKernel = [{kernel, lists:keyreplace(distributed, 1, OldKernel, NewDist)}],
    ok = rpc:call(Cp1, application_controller, test_change_apps, 
			[[kernel], [NewKernel]]),
    ok = rpc:call(Cp2, application_controller, test_change_apps, 
			[[kernel], [NewKernel]]),
    ok = rpc:call(Cp3, application_controller, test_change_apps, 
			[[kernel], [NewKernel]]),
    
    {[ok,ok,ok],[]} = 
	rpc:multicall([Cp1, Cp2, Cp3], 
		      application_controller, config_change, [OldEnv]),
    
    ct:sleep(7000),
    
    DcInfo1 = rpc:call(Cp1, dist_ac, info, []),
    DcInfo2 = rpc:call(Cp2, dist_ac, info, []),
    DcInfo3 = rpc:call(Cp3, dist_ac, info, []),

    DcWa1 = which_applications(Cp1),
    DcWa2 = which_applications(Cp2),
    DcWa3 = which_applications(Cp3),
    
    Wa1 = lists:foldl(fun({A1, _N1, _V1}, AccIn) -> [A1 | AccIn] end,
			    [], DcWa1),
    Wa2 = lists:foldl(fun({A2, _N2, _V2}, AccIn) -> [A2 | AccIn] end,
			    [], DcWa2),
    Wa3 = lists:foldl(fun({A3, _N3, _V3}, AccIn) -> [A3 | AccIn] end,
			    [], DcWa3),
    case lists:sort(Wa1) of
	      [app1, app2, app3, kernel, stdlib] ->
		  ok;
	      EWa1 ->
		  X1 = io_lib:format("distribution error: Cp1 ~p ",[EWa1]),
		  ct:fail(lists:flatten(X1))
	  end,
		  
    case lists:sort(Wa2) of
	      [app6, app8, kernel, stdlib] ->
		  ok;
	      EWa2 ->
		  X2 = io_lib:format("distribution error: Cp2 ~p ",[EWa2]),
		  ct:fail(lists:flatten(X2))
	  end,
		  
    case lists:sort(Wa3) of
	      [app7, kernel, stdlib] ->
		  ok;
	      EWa3 ->
		  X3 = io_lib:format("distribution error: Cp3 ~p ",[EWa3]),
		  ct:fail(lists:flatten(X3))
	  end,
		  
    DcInfo1n = rpc:call(Cp1, dist_ac, info, []),
    DcInfo2n = rpc:call(Cp2, dist_ac, info, []),
    DcInfo3n = rpc:call(Cp3, dist_ac, info, []),

    %% Added afterwards. Got rid of some warnings for unused variables.
    true = DcInfo1 =:= DcInfo1n,
    true = DcInfo2 =:= DcInfo2n,
    true = DcInfo3 =:= DcInfo3n,

    stop_node_nice(Cp1),
    stop_node_nice(Cp2),   
    stop_node_nice(Cp3),   

    ok = file:delete("dc.boot"),
    ok = file:delete("dc.rel"),
    ok = file:delete("dc.script"),

    ok.

%% Test change of distributed parameter, move appls by crashing a node.
distr_changed_tc2(Conf) when is_list(Conf) ->

    {OldKernel, OldEnv, {Cp1, Cp2, Cp3}, {Ncp1, _Ncp2, _Ncp3}, Config2} = 
	distr_changed_prep(Conf),

    NewDist = {distributed, [{app1, [Cp3]},
				   {app2, 5000, [Cp2]},
				   {app3, [Cp3, {Cp1, Cp2}]},
				   {app6, [Cp1, {Cp3, Cp2}]},
				   {app7, 1000, [Cp3]},
				   {app8, [Cp1, {Cp2, Cp3}]}]},
    
    NewKernel = [{kernel, lists:keyreplace(distributed, 1, OldKernel, NewDist)}],
    ok = rpc:call(Cp1, application_controller, test_change_apps, 
			[[kernel], [NewKernel]]),
    ok = rpc:call(Cp2, application_controller, test_change_apps, 
			[[kernel], [NewKernel]]),
    ok = rpc:call(Cp3, application_controller, test_change_apps, 
			[[kernel], [NewKernel]]),
    
    {[ok,ok,ok],[]} = 
	rpc:multicall([Cp1, Cp2, Cp3], 
		      application_controller, config_change, [OldEnv]),
    
    ct:sleep(4000),
    stop_node_nice(Cp1),   
    ct:sleep(10000),

    _DcInfo2 = rpc:call(Cp2, dist_ac, info, []),
    _DcInfo3 = rpc:call(Cp3, dist_ac, info, []),

    DcWa2 = which_applications(Cp2),
    DcWa3 = which_applications(Cp3),
    
    Wa2 = lists:foldl(fun({A2, _N2, _V2}, AccIn) -> [A2 | AccIn] end,
			    [], DcWa2),
    Wa3 = lists:foldl(fun({A3, _N3, _V3}, AccIn) -> [A3 | AccIn] end,
			    [], DcWa3),
		  

    case lists:sort(Wa2) of
	      [app2, app6, app8, kernel, stdlib] ->
		  ok;
	      EWa2 ->
		  X2 = io_lib:format("distribution error: Cp2 ~p ",[EWa2]),
		  ct:fail(lists:flatten(X2))
	  end,
		  
    case lists:sort(Wa3) of
	      [app1, app3, app7, kernel, stdlib] ->
		  ok;
	      EWa3 ->
		  X3 = io_lib:format("distribution error: Cp3 ~p ",[EWa3]),
		  ct:fail(lists:flatten(X3))
	  end,


    {ok, Cp1} = start_node_boot(Ncp1, Config2, dc),
    ct:sleep(10000),

    _DcInfo1rs = rpc:call(Cp1, dist_ac, info, []),
    _DcInfo2rs = rpc:call(Cp2, dist_ac, info, []),
    _DcInfo3rs = rpc:call(Cp3, dist_ac, info, []),

    DcWa1rs = which_applications(Cp1),
    DcWa2rs = which_applications(Cp2),
    DcWa3rs = which_applications(Cp3),
    
    Wa1rs = lists:foldl(fun({A1, _N1, _V1}, AccIn) -> [A1 | AccIn] end,
			      [], DcWa1rs),
    Wa2rs = lists:foldl(fun({A2, _N2, _V2}, AccIn) -> [A2 | AccIn] end,
			      [], DcWa2rs),
    Wa3rs = lists:foldl(fun({A3, _N3, _V3}, AccIn) -> [A3 | AccIn] end,
			      [], DcWa3rs),
		  
    case lists:sort(Wa1rs) of
	      [app6, app8, kernel, stdlib] ->
		  ok;
	      EWa1rs ->
		  X1rs = io_lib:format("distribution error: Cp1 ~p ",[EWa1rs]),
		  ct:fail(lists:flatten(X1rs))
	  end,
		  
    case lists:sort(Wa2rs) of
	      [app2, kernel, stdlib] ->
		  ok;
	      EWa2rs ->
		  X2rs = io_lib:format("distribution error: Cp2 ~p ",[EWa2rs]),
		  ct:fail(lists:flatten(X2rs))
	  end,
		  
    case lists:sort(Wa3rs) of
	      [app1, app3, app7, kernel, stdlib] ->
		  ok;
	      EWa3rs ->
		  X3rs = io_lib:format("distribution error: Cp3 ~p ",[EWa3rs]),
		  ct:fail(lists:flatten(X3rs))
	  end,


    stop_node_nice(Cp1),
    stop_node_nice(Cp2),
    stop_node_nice(Cp3),

    ok = file:delete("dc.boot"),
    ok = file:delete("dc.rel"),
    ok = file:delete("dc.script"),

    ok.



%%%-----------------------------------------------------------------
%%% Testing of application configuration change
%%%-----------------------------------------------------------------
%% Test change of application configuration.
config_change(Conf) when is_list(Conf) ->

    %% Change to data_dir
    {ok, CWD} = file:get_cwd(),
    DataDir = proplists:get_value(data_dir, Conf),
    ok = file:set_cwd(DataDir),

    %% Find out application data from boot script
    Boot = filename:join([code:root_dir(), "bin", "start.boot"]),
    {ok, Bin} = file:read_file(Boot),
    Appls0 = get_appls(binary_to_term(Bin)),

    %% And add app1 in order to test OTP-11864 - included config files
    %% not read for new (not already loaded) applications
    Appls = [app1() | Appls0],

    %% Simulate contents of "sys.config"
    Config = [{stdlib, [{par1,sys},{par2,sys}]},
		    "t1",
		    "t2.config",
		    filename:join([DataDir, "subdir", "t3"]),
		    {stdlib, [{par6,sys}]},
	            "t4.config"],

    %% Check that app1 is not loaded
    false = lists:keymember(app1,1,application:loaded_applications()),

    %% Order application_controller to update configuration
    ok = application_controller:change_application_data(Appls,
							      Config),

    %% Check that stdlib parameters are correctly set
    Env = application:get_all_env(stdlib),
    {value, {par1,sys}} = lists:keysearch(par1, 1, Env),
    {value, {par2,t1}}  = lists:keysearch(par2, 1, Env),
    {value, {par3,t1}}  = lists:keysearch(par3, 1, Env),
    {value, {par4,t2}}  = lists:keysearch(par4, 1, Env),
    {value, {par5,t3}}  = lists:keysearch(par5, 1, Env),
    {value, {par6,sys}} = lists:keysearch(par6, 1, Env),

    %% Check that app1 parameters are correctly set after loading
    [] = application:get_all_env(app1),
    application:load(app1()),
    App1Env = application:get_all_env(app1),
    {value, {par1,t4}} = lists:keysearch(par1, 1, App1Env),
    application:unload(app1),

    ok = file:set_cwd(CWD).

%% This function is stolen from SASL module release_handler, OTP R10B
get_appls({script, _, Script}) ->
    get_appls(Script, []).

%% kernel is taken care of separately
get_appls([{kernelProcess, application_controller, 
	    {application_controller, start, [App]}} | T], Res) ->
    get_appls(T, [App | Res]);
%% other applications but kernel
get_appls([{apply, {application, load, [App]}} | T], Res) ->
    get_appls(T, [App | Res]);
get_appls([_ | T], Res) ->
    get_appls(T, Res);
get_appls([], Res) ->
    Res.


%% Test set_env/4 and unset_env/3 with persistent true.
persistent_env(Conf) when is_list(Conf) ->
    ok = application:set_env(appinc, own2, persist, [{persistent, true}]),
    ok = application:set_env(appinc, key1, persist, [{persistent, true}]),

    %% own_env1 and own2 are set in appinc
    ok = application:load(appinc()),
    {ok, value1} = application:get_env(appinc, own_env1),
    {ok, persist} = application:get_env(appinc, own2),
    {ok, persist} = application:get_env(appinc, key1),

    %% Changing the environment after loaded reflects and should persist
    ok = application:set_env(appinc, own_env1, persist, [{persistent, true}]),
    {ok, persist} = application:get_env(appinc, own_env1),
    {ok, persist} = application:get_env(appinc, own2),
    {ok, persist} = application:get_env(appinc, key1),

    %% On reload, own_env1, own2 and key1 should all persist
    ok = application:unload(appinc),
    ok = application:load(appinc()),
    {ok, persist} = application:get_env(appinc, own_env1),
    {ok, persist} = application:get_env(appinc, own2),
    {ok, persist} = application:get_env(appinc, key1),

    %% Unset own_env1 and key1, own2 should still persist
    ok = application:unset_env(appinc, own_env1, [{persistent, true}]),
    ok = application:unset_env(appinc, key1, [{persistent, true}]),
    undefined = application:get_env(appinc, own_env1),
    {ok, persist} = application:get_env(appinc, own2),
    undefined = application:get_env(appinc, key1),

    %% own_env1 should be back to its application value on reload
    ok = application:unload(appinc),
    ok = application:load(appinc()),
    {ok, value1} = application:get_env(appinc, own_env1),
    {ok, persist} = application:get_env(appinc, own2),
    undefined = application:get_env(appinc, key1),

    %% Clean up
    ok = application:unload(appinc).

%%%-----------------------------------------------------------------
%%% Tests the 'shutdown_func' kernel config parameter
%%%-----------------------------------------------------------------
%% Tests the 'shutdown_func' kernel config parameter.
shutdown_func(Config) when is_list(Config) ->
    {ok,Cp1} = start_node(?MODULE_STRING++"_shutdown_func"),
    wait_for_ready_net(),
    Tag = make_ref(),
    ok = rpc:call(Cp1, application, set_env, 
                        [kernel, shutdown_func, {?MODULE, do_shutdown}]),
    ok = rpc:call(Cp1, application, set_env,
                        [kernel, shutdown_func_test, {self(), Tag}]),
    _ = rpc:call(Cp1, init, stop, []),
    receive
	      {Pid, Tag, shutting_down, shutdown} ->
		  Mref = erlang:monitor(process, Pid),
		  Pid ! {self(), Tag, ok},
		  receive
		      {'DOWN', Mref, _, Pid, noconnection} ->
			  ok
		  after 10000 ->
			  ct:fail(timeout)
		  end
	  after 10000 ->
		  ct:fail(timeout)
	  end.



do_shutdown(Reason) ->
    {ok, {Pid, Tag}} = application:get_env(kernel, shutdown_func_test),
    Pid ! {self(), Tag, shutting_down, Reason},
    receive 
	{Pid, Tag, ok} -> ok 
    end.



%%%-----------------------------------------------------------------
%%% Tests the 'shutdown_timeout' kernel config parameter
%%%-----------------------------------------------------------------
shutdown_timeout(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir,Config),
    {ok,Cp1} = start_node(?MODULE_STRING++"_shutdown_timeout"),
    wait_for_ready_net(),
    ok = rpc:call(Cp1, application, set_env, [kernel, shutdown_timeout, 1000]),
    rpc:call(Cp1, code, add_path, [filename:join([DataDir,deadlock])]),
    ok = rpc:call(Cp1, application, start, [sasl]),
    ok = rpc:call(Cp1, application, start, [deadlock]),
    rpc:call(Cp1, deadlock, restart_and_fail, []),

    ok = net_kernel:monitor_nodes(true),
    _ = rpc:call(Cp1, init, stop, []),
    receive
	{nodedown,Cp1} ->
	    ok
    after 10000 ->
	    ct:fail("timeout 10 sec: node termination hangs")
    end,
    ok.

%%%-----------------------------------------------------------------
%%% Provokes a (previous) application shutdown deadlock
%%%-----------------------------------------------------------------
shutdown_deadlock(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir,Config),
    code:add_path(filename:join([DataDir,deadlock])),
    %% ok = rpc:call(Cp1, application, start, [sasl]),
    ok = application:start(deadlock),
    Tester = self(),
    application:set_env(deadlock, fail_stop, Tester),
    spawn(fun() -> Tester ! {stop, application:stop(deadlock)} end),

    receive
	{deadlock, Server} ->
	    spawn(fun() ->
			  Master = application_controller:get_master(deadlock),
			  Child = application_master:get_child(Master),
			  Tester ! {child, Child}
		  end),
	    timer:sleep(100),
	    erlang:display({self(), "Sending Continue", Server}),
	    Server ! continue
    end,
    [_|_] = application:which_applications(),
    application:unload(deadlock), % clean up!
    ok.


%%-----------------------------------------------------------------
%% Utility functions
%%-----------------------------------------------------------------
app0() ->
    {application, app0,
     [{description, "ERTS  CXC 138 10"},
      {vsn, "2.0"},
      {modules, []},
      {registered, []},
      {applications, [kernel]},
      {mod, {ch_sup, {app0, 77, 80}}}]}.

app1() ->
    {application, app1,
     [{description, "ERTS  CXC 138 10"},
      {vsn, "2.0"},
      {modules, []},
      {registered, []},
      {applications, [kernel]},
      {mod, {ch_sup, {app1, 1, 3}}}]}.

app2() ->
    {application, app2,
     [{description, "ERTS  CXC 138 10"},
      {vsn, "2.0"},
      {modules, []},
      {registered, []},
      {applications, [kernel]},
      {mod, {ch_sup, {app2, 4, 6}}}]}.

app3() ->
    {application, app3,
     [{description, "ERTS  CXC 138 10"},
      {vsn, "2.0"},
      {modules, []},
      {registered, []},
      {applications, [kernel]},
      {mod, {ch_sup, {app3, 7, 9}}}]}.

app4() ->
    {application, app4,
     [{description, "ERTS  CXC 138 10"},
      {vsn, "2.0"},
      {applications, [kernel]},
      {included_applications, [app5]},
      {mod, {ch_sup, {app3, 7, 9}}}]}.

app5() ->
    {application, app5,
     [{description, "ERTS  CXC 138 10"},
      {vsn, "2.0"},
      {applications, [kernel]},
      {mod, {ch_sup, {app3, 7, 9}}}]}.

app6() ->
    {application, app6,
     [{description, "ERTS  CXC 138 10"},
      {vsn, "2.0"},
      {modules, []},
      {registered, []},
      {applications, [kernel]},
      {mod, {ch_sup, {app6, 10, 12}}}]}.

app7() ->
    {application, app7,
     [{description, "ERTS  CXC 138 10"},
      {vsn, "2.0"},
      {modules, []},
      {registered, []},
      {applications, [kernel]},
      {mod, {ch_sup, {app7, 13, 15}}}]}.

app8() ->
    {application, app8,
     [{description, "ERTS  CXC 138 10"},
      {vsn, "2.0"},
      {modules, []},
      {registered, []},
      {applications, [kernel]},
      {mod, {ch_sup, {app7, 16, 18}}}]}.

app9() ->
    {application, app9,
     [{description, "ERTS  CXC 138 10"},
      {vsn, "2.0"},
      {modules, []},
      {registered, []},
      {applications, [kernel]},
      {mod, {ch_sup, {app9, 19, 19}}}]}.

app10_dep9() ->
    {application, app10,
     [{description, "ERTS  CXC 138 10"},
      {vsn, "2.0"},
      {modules, []},
      {registered, []},
      {applications, [kernel, app9]},
      {mod, {ch_sup, {app10, 20, 20}}}]}.

appinc() ->
    {application, appinc,
     [{description, "Test of new app file, including appnew"},
      {id, "CXC 138 ai"},
      {vsn, "2.0"},
      {applications, [kernel]},
      {modules, []},
      {registered, []},
      {env, [{own_env1, value1}, {own2, val2}]},
      {included_applications, [appinc1, appinc2]},
      {start_phases, [{init, [kalle]}, {takeover, []}, {go, [sune]}]},
      {mod, {application_starter, [ch_sup, {appinc, 41, 43}] }}]}. 


app_sp() ->
    {application, app_sp,
     [{description, "ERTS  CXC 138 10"},
      {vsn, "2.0"},
      {start_phases, [{init, [kurt]}, {go, [sune]}]},
      {applications, [kernel]},
      {modules, []},
      {registered, []},
      {mod, {application_starter, [ch_sup, {app_sp, 31, 33}] }}]}. 

app_trans_normal() ->
    {application, trans_normal,
     [{description, "A  CXC 138 11"},
      {vsn, "1.0"},
      {modules, [{transient, 1}, {trans_normal_sup,1}]},
      {registered, [trans_normal_sup]},
      {applications, [kernel, stdlib]},
      {mod, {trans_normal_sup, []}}]}.

app_trans_abnormal() ->
    {application, trans_abnormal,
     [{description, "A  CXC 138 11"},
      {vsn, "1.0"},
      {modules, [{transient, 1}, {trans_abnormal_sup,1}]},
      {registered, [trans_abnormal_sup]},
      {applications, [kernel, stdlib]},
      {mod, {trans_abnormal_sup, []}}]}.

app_start_error() ->
    {application, app_start_error,
     [{description, "ERTS  CXC 138 10"},
      {vsn, "2.0"},
      {modules, []},
      {registered, []},
      {applications, [kernel]},
      {mod, {app_start_error, []}}]}.

app_chain_error() ->
    {application, app_chain_error,
     [{description, "ERTS  CXC 138 ce"},
      {vsn, "2.0"},
      {modules, []},
      {registered, []},
      {applications, [kernel, app_chain_error2]},
      {mod, {ch_sup, {app_chain_error, 20,20}}}]}.

app_chain_error2() ->
    {application, app_chain_error2,
     [{description, "ERTS  CXC 138 ce2"},
      {vsn, "2.0"},
      {modules, []},
      {registered, []},
      {applications, [kernel, app10, hopefully_not_an_existing_app]},
      {mod, {ch_sup, {app_chain_error2, 21,21}}}]}.

app_group_leader() ->
    {application, group_leader,
     [{description, "GROUP_LEADER  CXC 138 11"},
      {vsn, "1.0"},
      {modules, [group_leader,group_leader_sup]},
      {registered, [group_leader_sup]},
      {applications, [kernel,stdlib]},
      {mod, {group_leader_sup, []}}]}.


d1([Ncp1, Ncp2, Ncp3]) ->
    M = from($@, atom_to_list(node())),
    {app1, [list_to_atom(Ncp1 ++ "@" ++ M),
	    list_to_atom(Ncp2 ++ "@" ++ M),
	    list_to_atom(Ncp3 ++ "@" ++ M)]}.

d2([Ncp1, _Ncp2, Ncp3]) ->
    M = from($@, atom_to_list(node())),
    {app1, [list_to_atom(Ncp1 ++ "@" ++ M),
	    list_to_atom(Ncp3 ++ "@" ++ M)]}.

d3([Ncp1, Ncp2, Ncp3]) ->
    M = from($@, atom_to_list(node())),
    {appinc, [list_to_atom(Ncp1 ++ "@" ++ M),
              list_to_atom(Ncp2 ++ "@" ++ M),
              list_to_atom(Ncp3 ++ "@" ++ M)]}.

d_any3(Any, [Ncp1, Ncp2, Ncp3]) ->
    M = from($@, atom_to_list(node())),
    {Any, [list_to_atom(Ncp1 ++ "@" ++ M),
           list_to_atom(Ncp2 ++ "@" ++ M),
           list_to_atom(Ncp3 ++ "@" ++ M)]}.


config([Ncp1, Ncp2, Ncp3]) ->
    fun(Fd, SyncNodesTimeout) ->
            M = from($@, atom_to_list(node())),
            io:format(Fd, "[{kernel, [{sync_nodes_optional, "
                      "['~s@~s','~s@~s','~s@~s']},"
                      "{sync_nodes_timeout, ~w},"
                      "{distributed, [{app1, ['~s@~s', '~s@~s', '~s@~s']},"
                      "{app2, 1000, ['~s@~s', '~s@~s', '~s@~s']},"
                      "{app3, 1000, [{'~s@~s', '~s@~s'}, '~s@~s']}]}]}].~n",
                      [Ncp1, M, Ncp2, M, Ncp3, M,  
                       SyncNodesTimeout,  Ncp1, M, Ncp2, M, Ncp3, M,  
                       Ncp1, M, Ncp2, M, Ncp3, M,  
                       Ncp1, M, Ncp2, M, Ncp3, M])
    end.

config2([Ncp1, Ncp2, Ncp3]) ->
    fun(Fd, SyncNodesTimeout) ->
            M = from($@, atom_to_list(node())),
            io:format(Fd, "[{kernel, [{sync_nodes_optional, "
                      "['~s@~s','~s@~s','~s@~s']},"
                      "{sync_nodes_timeout, ~w},"
                      "{permissions, [{app3, false}]},"
                      "{distributed, [{app1, ['~s@~s', '~s@~s', '~s@~s']},"
                      "{app2, 10000, ['~s@~s', '~s@~s', '~s@~s']},"
                      "{app3, 5000, [{'~s@~s', '~s@~s'}, '~s@~s']}]}]}].~n",
                      [Ncp1, M, Ncp2, M, Ncp3, M,  
                       SyncNodesTimeout,  
                       Ncp1, M, Ncp2, M, Ncp3, M,  
                       Ncp1, M, Ncp2, M, Ncp3, M,  
                       Ncp1, M, Ncp2, M, Ncp3, M])
    end.

config3([Ncp1, Ncp2, Ncp3]) ->
    fun(Fd, SyncNodesTimeout) ->
            M = from($@, atom_to_list(node())),
            io:format(Fd, "[{kernel, [{sync_nodes_optional, "
                      "['~s@~s','~s@~s','~s@~s']},"
                      "{sync_nodes_timeout, ~w},"
                      "{start_dist_ac, true},"
                      "{permissions, [{app3, false}]}]}].~n",
                      [Ncp1, M, Ncp2, M, Ncp3, M,  
                       SyncNodesTimeout])
    end.

config4([Ncp1, Ncp2]) ->
    fun(Fd, SyncNodesTimeout) ->
            M = from($@, atom_to_list(node())),
            io:format(Fd, "[{kernel, [{sync_nodes_optional, "
                      "['~s@~s','~s@~s']},"
                      "{sync_nodes_timeout, ~w},"
                      "{start_dist_ac, true},"
                      "{distributed, [{app1, ['~s@~s', '~s@~s']}]}]}].~n",
                      [Ncp1, M, Ncp2, M,  SyncNodesTimeout,  
                       Ncp1, M, Ncp2, M])
    end.

config3184([Ncp1, Ncp2]) ->
    fun(Fd, SyncNodesTimeout) ->
            M = from($@, atom_to_list(node())),
            io:format(Fd, "[{kernel, [{sync_nodes_optional, "
                      "['~s@~s','~s@~s']},"
                      "{sync_nodes_timeout, ~w},"
                      "{permissions, [{app1, false}]},"
                      "{distributed, [{app1, ['~s@~s', '~s@~s']}]}]}].~n",
                      [Ncp1, M, Ncp2, M,  SyncNodesTimeout,  
                       Ncp1, M, Ncp2, M])
    end.

config_perm(Fd) ->
    io:format(Fd, "[{kernel, [{permissions, "
	      "[{app1, false}, {app2, false}, {app3, false}]} ]}].~n",[]).

config_perm2([Ncp1, Ncp2, Ncp3]) ->
    fun(Fd, SyncNodesTimeout) ->
            M = from($@, atom_to_list(node())),
            io:format(Fd, "[{kernel, [{sync_nodes_optional, "
                      "['~s@~s','~s@~s','~s@~s']},"
                      "{sync_nodes_timeout, ~w},"
                      "{permissions, [{app1, false}, {app2, false}, {app3, false}]},"
                      "{distributed, [{app1, ['~s@~s', '~s@~s', '~s@~s']},"
                      "{app2, 10000, ['~s@~s', '~s@~s', '~s@~s']},"
                      "{app3, 5000, [{'~s@~s', '~s@~s'}, '~s@~s']}]}]}].~n",
                      [Ncp1, M, Ncp2, M, Ncp3, M,
                       SyncNodesTimeout,
                       Ncp1, M, Ncp2, M, Ncp3, M,
                       Ncp1, M, Ncp2, M, Ncp3, M,
                       Ncp1, M, Ncp2, M, Ncp3, M])
    end.

config_inc([Ncp1, Ncp2, Ncp3]) ->
    fun(Fd, SyncNodesTimeout) ->
            M = from($@, atom_to_list(node())),
            io:format(Fd, "[{kernel, [{sync_nodes_optional, "
                      "['~s@~s','~s@~s','~s@~s']},"
                      "{sync_nodes_timeout, ~w},"
                      "{distributed, [{appinc, ['~s@~s', '~s@~s', '~s@~s']},"
                      "{app2, 10000, ['~s@~s', '~s@~s', '~s@~s']},"
                      "{app3, 5000, [{'~s@~s', '~s@~s'}, '~s@~s']}]}]}].~n",
                      [Ncp1, M, Ncp2, M, Ncp3, M,  
                       SyncNodesTimeout,  
                       Ncp1, M, Ncp2, M, Ncp3, M,  
                       Ncp1, M, Ncp2, M, Ncp3, M,  
                       Ncp1, M, Ncp2, M, Ncp3, M])
    end.

config_sf([Ncp1, Ncp2, Ncp3]) ->
    fun(Fd, SyncNodesTimeout) ->
            M = from($@, atom_to_list(node())),
            io:format(Fd, "[{kernel, [{sync_nodes_optional, "
                      "['~s@~s','~s@~s','~s@~s']},"
                      "{sync_nodes_timeout, ~w},"
                      "{distributed, [{myApp, ['~s@~s', '~s@~s', '~s@~s']},"
                      "{topApp, ['~s@~s', '~s@~s', '~s@~s']},"
                      "{inclOne, ['~s@~s', '~s@~s', '~s@~s']},"
                      "{inclTwo, ['~s@~s', '~s@~s', '~s@~s']},"
                      "{inclTwoTop, ['~s@~s', '~s@~s', '~s@~s']},"
                      "{incl2A, ['~s@~s', '~s@~s', '~s@~s']},"
                      "{incl2B, ['~s@~s', '~s@~s', '~s@~s']},"
                      "{with, ['~s@~s', '~s@~s', '~s@~s']},"
                      "{wrapper, ['~s@~s', '~s@~s', '~s@~s']}]}]}].~n",
                      [Ncp1, M, Ncp2, M, Ncp3, M,  
                       SyncNodesTimeout,   
                       Ncp1, M, Ncp2, M, Ncp3, M,  
                       Ncp1, M, Ncp2, M, Ncp3, M,  
                       Ncp1, M, Ncp2, M, Ncp3, M,  
                       Ncp1, M, Ncp2, M, Ncp3, M,  
                       Ncp1, M, Ncp2, M, Ncp3, M,  
                       Ncp1, M, Ncp2, M, Ncp3, M,  
                       Ncp1, M, Ncp2, M, Ncp3, M,  
                       Ncp1, M, Ncp2, M, Ncp3, M,  
                       Ncp1, M, Ncp2, M, Ncp3, M])
    end.

config_fo([Ncp1, Ncp2, Ncp3]) ->
    fun(Fd, SyncNodesTimeout) ->
            M = from($@, atom_to_list(node())),
            io:format(Fd, "[{kernel, [{sync_nodes_optional, "
                      "['~s@~s','~s@~s','~s@~s']},"
                      "{sync_nodes_timeout, ~w},"
                      "{distributed, [{app1, ['~s@~s', '~s@~s', '~s@~s']},"
                      "{app2, 2000, ['~s@~s', '~s@~s', '~s@~s']},"
                      "{app_sp, 1000, [{'~s@~s', '~s@~s'}, '~s@~s']}]}]}].~n",
                      [Ncp1, M, Ncp2, M, Ncp3, M,
                       SyncNodesTimeout,
                       Ncp1, M, Ncp2, M, Ncp3, M,
                       Ncp1, M, Ncp2, M, Ncp3, M,
                       Ncp1, M, Ncp2, M, Ncp3, M])
    end.

config_dc([Ncp1, Ncp2, Ncp3]) ->
    fun(Fd, SyncNodesTimeout) ->
            M = from($@, atom_to_list(node())),
            io:format(Fd, "[{kernel, [{sync_nodes_optional, "
                      "['~s@~s','~s@~s','~s@~s']},"
                      "{sync_nodes_timeout, ~w},"
                      "{distributed, [{app1, ['~s@~s', '~s@~s']},"
                      "               {app2, 10000, ['~s@~s']},"
                      "               {app3, [{'~s@~s', '~s@~s'}]}, "
                      "               {app6, [{'~s@~s', '~s@~s'}]}, "
                      "               {app7, ['~s@~s']}, "
                      "               {app8, ['~s@~s', {'~s@~s', '~s@~s'}]}"
                      "              ]}]}].~n",
                      [Ncp1, M, Ncp2, M, Ncp3, M,  
                       SyncNodesTimeout,  
                       Ncp1, M, Ncp2, M,  
                       Ncp1, M,  
                       Ncp1, M, Ncp2, M,  
                       Ncp3, M, Ncp2, M,  
                       Ncp3, M,  
                       Ncp2, M, Ncp1, M, Ncp3, M])
    end.

config_dc2([Ncp1, Ncp2, Ncp3]) ->
    fun(Fd) ->
            M = from($@, atom_to_list(node())),
            io:format(Fd, "[{kernel, [{sync_nodes_optional, "
                      "['~s@~s','~s@~s','~s@~s']},"
                      "{sync_nodes_timeout, 10000},"
                      "{distributed, [{app1, ['~s@~s']},"
                      "               {app2, 5000, ['~s@~s']},"
                      "               {app3, ['~s@~s', {'~s@~s', '~s@~s'}]}, "
                      "               {app6, ['~s@~s', {'~s@~s', '~s@~s'}]}, "
                      "               {app7, 1000, ['~s@~s']}, "
                      "               {app8, ['~s@~s', {'~s@~s', '~s@~s'}]}"
                      "              ]}]}].~n",
                      [Ncp1, M, Ncp2, M, Ncp3, M,  
                       Ncp3, M, 
                       Ncp2, M, 
                       Ncp3, M, Ncp1, M, Ncp2, M, 
                       Ncp1, M, Ncp3, M, Ncp2, M, 
                       Ncp3, M,  
                       Ncp1, M, Ncp2, M, Ncp3, M])
    end.

w_app1(Fd) ->
    io:format(Fd, "~p.\n", [app1()]).

w_app2(Fd) ->
    io:format(Fd, "~p.\n", [app2()]).

w_app3(Fd) ->
    io:format(Fd, "~p.\n", [app3()]).

w_app5(Fd) ->
    io:format(Fd, "~p.\n", [app5()]).

w_app6(Fd) ->
    io:format(Fd, "~p.\n", [app6()]).

w_app7(Fd) ->
    io:format(Fd, "~p.\n", [app7()]).

w_app8(Fd) ->
    io:format(Fd, "~p.\n", [app8()]).

w_app9(Fd) ->
    io:format(Fd, "~p.\n", [app9()]).

w_app10_dep9(Fd) ->
    io:format(Fd, "~p.\n", [app10_dep9()]).

w_app_start_error(Fd) ->
    io:format(Fd, "~p.\n", [app_start_error()]).

w_app(Fd, AppData) ->
    io:format(Fd, "~p.\n", [AppData]).

from(H, [H | T]) -> T;
from(H, [_ | T]) -> from(H, T);
from(_H, []) -> [].

is_loaded(Name, [Node | Nodes]) ->
    Apps = rpc:call(Node, application, loaded_applications, []),
    case lists:keysearch(Name, 1, Apps) of
	{value, _} -> is_loaded(Name, Nodes);
	false -> false
    end;
is_loaded(_Name, []) ->
    true;
is_loaded(Name, Node) ->
    is_loaded(Name, [Node]).

is_started(Name, Node) ->
    Apps = which_applications(Node),
    case lists:keysearch(Name, 1, Apps) of
	{value, _} -> true;
	false -> false
    end.

%% Waits until application Name is started on at least one node.
wait_until_started(Name, Nodes) ->
    case lists:member(true,
		      lists:map(fun (N) ->
					is_started(Name, N)
				end,
				Nodes)) of
	true ->
	    true;
	false ->
	    ct:sleep(500),
	    wait_until_started(Name, Nodes)
    end.

%% Waits until application Name is stopped on all nodes.
wait_until_stopped(Name, Nodes) ->
    case lists:member(true,
		      lists:map(fun (N) ->
					is_started(Name, N)
				end,
				Nodes)) of
	false ->
	    true;
	true ->
	    ct:sleep(500),
	    wait_until_stopped(Name, Nodes)
    end.

%% The test server has no support for starting nodes in parallel. To
%% avoid long delays a small sync_nodes_timeout is used. Use this
%% function when starting all nodes but the last one, and when
%% restarting nodes (then use global:sync() to synchronize).
config_fun_fast(SysConfigFun) ->
    fun(Fd) -> SysConfigFun(Fd, 1) end.

config_fun(SysConfigFun) ->
    fun(Fd) -> SysConfigFun(Fd, 10000) end.

start_node_config(Name, SysConfigFun, Conf) ->
    ConfigFile = write_config_file(SysConfigFun, Conf),
    start_node(Name, ConfigFile, "").

start_node(Name) ->
    Pa = filename:dirname(code:which(?MODULE)),
    test_server:start_node(Name, slave, [{args, " -pa " ++ Pa}]).

start_node(Name, ConfigFile) ->
    start_node(Name, ConfigFile, "").

start_node(Name, ConfigFile, ExtraArgs) ->
    Pa = filename:dirname(code:which(?MODULE)),
    test_server:start_node(Name, slave, [{args, 
                                          " -pa " ++ Pa ++ 
                                          " -config " ++ ConfigFile ++ 
                                          ExtraArgs}]).

start_node_with_cache(Name, SysConfigFun, Conf) ->
    ConfigFile = write_config_file(SysConfigFun, Conf),
    start_node(Name, ConfigFile, " -code_path_cache").

start_node_args(Name, Args) ->
    Pa = filename:dirname(code:which(?MODULE)),
    test_server:start_node(Name, slave, [{args, " -pa " ++ Pa ++ " " ++ Args}]).

start_node_boot_3002(Name, Boot) ->
    Pa = filename:dirname(code:which(?MODULE)),
    ct:pal(?HI_VERBOSITY, "start_node_boot ~p~n",
	   [" -pa " ++ Pa ++ " -env ERL_CRASH_DUMP erl_crash_dump." ++
		atom_to_list(Name) ++ " -boot " ++ Boot ++
		" -sasl dummy \"missing "]),
    test_server:start_node(Name, slave, 
                           [{args, " -pa " ++ Pa ++
                             " -env ERL_CRASH_DUMP erl_crash_dump." ++
                             atom_to_list(Name) ++ " -boot " ++ Boot ++ 
                             " -sasl dummy \"missing "}]).

start_node_boot_config(Name, SysConfigFun, Conf, Boot) ->
    ConfigFile = write_config_file(SysConfigFun, Conf),
    start_node(Name, ConfigFile, " -boot " ++ atom_to_list(Boot)).

start_node_boot(Name, Config, Boot) ->
    Pa = filename:dirname(code:which(?MODULE)),
    ct:pal(?HI_VERBOSITY,
	   "start_node_boot ~p~n",[" -pa " ++ Pa ++ " -config " ++ Config ++
				       " -boot " ++ atom_to_list(Boot)]),
    test_server:start_node(Name, slave,
			   [{args, " -pa " ++ Pa ++ " -config " ++ Config ++
				 " -boot " ++ atom_to_list(Boot)}]).

start_node_config_sf(Name, SysConfigFun, Conf) ->
    ConfigFile = write_config_file(SysConfigFun, Conf),
    DataDir = proplists:get_value(data_dir, Conf), % is it used?
    start_node(Name, ConfigFile, " -pa " ++ DataDir).

write_config_file(SysConfigFun, Conf) ->
    Dir = proplists:get_value(priv_dir, Conf),
    {ok, Fd} = file:open(filename:join(Dir, "sys.config"), [write]),
    SysConfigFun(Fd),
    file:close(Fd),
    filename:join(Dir,"sys").

node_names(Names, Config) ->
    [node_name(Name, Config) || Name <- Names].

node_name(Name, Config) ->
    U = "_",
    L = integer_to_list(erlang:unique_integer([positive])),
    lists:concat([Name,U,?testcase,U,U,L]).

stop_node_nice(Node) when is_atom(Node) ->
    test_server:stop_node(Node);
stop_node_nice(Nodes) when is_list(Nodes) ->
    lists:foreach(fun (N) -> stop_node_nice(N) end, Nodes).


get_start_type(Expected) ->
    get_start_type(Expected, 30*5, #st{}).

get_start_type(_Expected, 0, Ack) ->
    io:format("====== ~p ======~n", [Ack]),
    ct:fail(not_valid_start_type);
get_start_type(Expected, Times, Ack0) ->
    #st{normal = N0, local = L0, takeover = T0, failover = F0} = Ack0,
    global:send(st_type, {st, read, self()}),
    receive
        {st, N, L, T, F} ->
            Ack = #st{normal = N0 + N, local = L0 + L, 
                      takeover = T0 + T, failover = F0 + F},
            if
                Ack =:= Expected ->
                    ok;
                true -> 
                    timer:sleep(200),
                    get_start_type(Expected, Times-1, Ack)
            end
    after 30*1000 ->
            get_start_type(Expected, 0, Ack0)
    end.

start_type() ->
    st(0, 0, 0, 0).

st(Normal, Local, Takeover, Failover) ->
    receive
	{st, normal} ->
	    st(Normal+1, Local, Takeover, Failover);
	{st, local} ->
	    st(Normal, Local+1, Takeover, Failover);
	{st, takeover} ->
	    st(Normal, Local, Takeover+1, Failover);
	{st, failover} ->
	    st(Normal, Local, Takeover, Failover+1);
	{st, read, From} ->
	    From ! {st, Normal, Local, Takeover, Failover},
	    st(0, 0, 0, 0);
	kill ->
	    exit(normal)
    end.


get_start_phase(Expected) ->
    global:send(start_phase, {sp, read, self()}),
    receive
	Expected ->
	    ok;
	{sp, T1, I1, So1, Sp1, G1} ->
           io:format("=============== {sp,T,I,So,Sp,G} ~p ~n",[" "]),
           io:format("=========== got ~p ~n",
                              [{sp, T1, I1, So1, Sp1, G1}]),
           io:format("====== expected ~p ~n", [Expected]),
           ct:fail(not_valid_start_phase)
    after 5000 ->
           ct:fail(not_valid_start_phase)
    end.

start_phase() ->
    sp(0, 0, 0, 0, 0).

sp(Top, Init, Some, Spec, Go) ->
    receive
	{sp, top} ->
	    sp(Top+1, Init, Some, Spec, Go);
	{sp, init} ->
	    sp(Top, Init+1, Some, Spec, Go);
	{sp, some} ->
	    sp(Top, Init, Some+1, Spec, Go);
	{sp, spec} ->
	    sp(Top, Init, Some, Spec+1, Go);
	{sp, go} ->
	    sp(Top, Init, Some, Spec, Go+1);
	{sp, read, From} ->
	    From ! {sp, Top, Init, Some, Spec, Go},
	    sp(0, 0, 0, 0, 0);
	kill ->
	    exit(normal)
    end.

get_conf_change(Expected) ->
    global:send(conf_change, {cc, read, self()}),
    receive
	{cc, Expected} ->
	    ok;
	{cc, List} ->
	    io:format("====== ~p ======~n",[{cc, List}]),
	    ct:fail(not_valid_conf_change)
    after 5000 ->
	    ct:fail(not_valid_conf_change_to)
    end.

conf_change() ->
    cc([]).

cc(List) ->
    receive
	{cc, New} ->
	    cc(List ++ New);
	{cc, read, From} ->
	    From ! {cc, List},
	    cc([]);
	kill ->
	    exit(normal)
    end.



create_app() ->
    Dir = "./",
    App1 = Dir ++ "app1",
    {ok, Fd1} = file:open(App1++".app",[write]),
    io:format(Fd1, "~p. \n", [app1()]),
    file:close(Fd1),
    App2 = Dir ++ "app2",
    {ok, Fd2} = file:open(App2++".app",[write]),
    io:format(Fd2, "~p. \n", [app2()]),
    file:close(Fd2),
    App3 = Dir ++ "app_sp",
    {ok, Fd3} = file:open(App3++".app",[write]),
    io:format(Fd3, "~p. \n", [app_sp()]),
    file:close(Fd3),
    ok.


create_script(ScriptName) ->
    Dir = "./",
    Name = Dir ++ ScriptName,
    Apps = which_applications(),
    {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),
    {ok,Fd} = file:open(Name++".rel",[write]),
    io:format(Fd,
		    "{release, {\"Test release 3\", \"LATEST\"}, \n"
		    " {erts, \"4.4\"}, \n"
		    " [{kernel, \"~s\"}, {stdlib, \"~s\"}, \n"
		    "  {app1, \"2.0\"}, {app2, \"2.0\"}, {app_sp, \"2.0\"}]}.\n",
		    [KernelVer,StdlibVer]),
    file:close(Fd),
    {{KernelVer,StdlibVer},
     {filename:dirname(Name), filename:basename(Name)}}.



create_script_dc(ScriptName) ->
    Dir = "./",
    Name = Dir ++ ScriptName,
    Apps = which_applications(),
    {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),
    {ok,Fd} = file:open(Name++".rel",[write]),
    io:format(Fd,
		    "{release, {\"Test release 3\", \"LATEST\"}, \n"
		    " {erts, \"4.4\"}, \n"
		    " [{kernel, \"~s\"}, {stdlib, \"~s\"}, \n"
		    "  {app1, \"2.0\"}, {app2, \"2.0\"}, {app3, \"2.0\"}, \n"
		    "  {app6, \"2.0\"}, {app7, \"2.0\"}, {app8, \"2.0\"}]}.\n",
		    [KernelVer,StdlibVer]),
    file:close(Fd),
    {{KernelVer,StdlibVer},
     {filename:dirname(Name), filename:basename(Name)}}.


create_script_3002(ScriptName) ->
    Dir = "./",
    Name = Dir ++ ScriptName,
    Apps = which_applications(),
    {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),
    {value,{_,_,SaslVer}} = lists:keysearch(sasl,1,Apps),
    {ok,Fd} = file:open(Name++".rel",[write]),
    io:format(Fd,
		    "{release, {\"Test release 3\", \"LATEST\"}, \n"
		    " {erts, \"4.4\"}, \n"
		    " [{kernel, \"~s\"}, {stdlib, \"~s\"}, \n"
		    "  {sasl, \"~s\"}]}.\n",
		    [KernelVer, StdlibVer, SaslVer]),
    file:close(Fd),
    {{KernelVer,StdlibVer},
     {filename:dirname(Name), filename:basename(Name)}}.



distr_changed_prep(Conf) when is_list(Conf) ->

    %% Write .app files
    {ok, Fd1} = file:open("app1.app", [write]),
    w_app1(Fd1),
    file:close(Fd1),
    {ok, Fd2} = file:open("app2.app", [write]),
    w_app2(Fd2),
    file:close(Fd2),
    {ok, Fd3} = file:open("app3.app", [write]),
    w_app3(Fd3),
    file:close(Fd3),
    {ok, Fd4} = file:open("app6.app", [write]),
    w_app6(Fd4),
    file:close(Fd4),
    {ok, Fd5} = file:open("app7.app", [write]),
    w_app7(Fd5),
    file:close(Fd5),
    {ok, Fd6} = file:open("app8.app", [write]),
    w_app8(Fd6),
    file:close(Fd6),


    %% Create the .app files and the boot script
    {{KernelVer,StdlibVer}, _} = create_script_dc("dc"),

    case is_real_system(KernelVer, StdlibVer) of
	      true ->
		  Options = [];
	      false  ->
		  Options = [local]
	  end,

    ok = systools:make_script("dc", Options),
    
    NodeNames = [Ncp1, Ncp2, Ncp3] = node_names([cp1, cp2, cp3], Conf),
    NoSyncTime = config_fun_fast(config_dc(NodeNames)),
    WithSyncTime = config_fun(config_dc(NodeNames)),

    Dir = proplists:get_value(priv_dir,Conf),
    {ok, Fd_dc2} = file:open(filename:join(Dir, "sys2.config"), [write]),
    (config_dc2(NodeNames))(Fd_dc2),
    file:close(Fd_dc2),
    Config2 = filename:join(Dir, "sys2"),
    
    %% Test [cp1, cp2, cp3]
    {ok, Cp1} = start_node_boot_config(Ncp1, NoSyncTime, Conf, dc),
    {ok, Cp2} = start_node_boot_config(Ncp2, NoSyncTime, Conf, dc),
    {ok, Cp3} = start_node_boot_config(Ncp3, WithSyncTime, Conf, dc),
    global:sync(),

    %% Read the current configuration parameters, and change them
    OldEnv = rpc:call(Cp1, application_controller, prep_config_change, []),
    {value, {kernel, OldKernel}} = lists:keysearch(kernel, 1, OldEnv),
    {OldKernel, OldEnv, {Cp1, Cp2, Cp3}, {Ncp1, Ncp2, Ncp3}, Config2}.


%%% Copied from init_SUITE.erl.
is_real_system(KernelVsn, StdlibVsn) ->
    LibDir = code:lib_dir(),
    case file:read_file_info(LibDir ++ "/kernel-" ++ KernelVsn) of
	{ok, _} ->
	    case file:read_file_info(LibDir ++ "/stdlib-" ++ StdlibVsn) of
		{ok, _} ->
		    true;
		_ ->
		    false
	    end;
	_ ->
	    false
    end.
    
init2973() -> 
    loop2973().


loop2973() -> 
    receive
	{start, From, App} ->
	    Res = application:start(App),
	    From ! {self(), res, Res},
	    loop2973();

	kill ->
	    exit(normal)
    end.

wait_for_ready_net() ->
    Nodes = lists:sort([node() | nodes()]),
    ?UNTIL(begin
               lists:all(fun(N) -> Nodes =:= get_known(N) end, Nodes) and
               lists:all(fun(N) -> 
                                 LNs = rpc:call(N, erlang, nodes, []),
                                 Nodes =:= lists:sort([N | LNs])
                         end, Nodes)
           end).

get_known(Node) ->
    case catch gen_server:call({global_name_server,Node}, get_known) of
        {'EXIT', _} ->
            [list, without, nodenames];
        Known -> 
            lists:sort([Node | Known])
    end.

which_applications() ->
    application_controller:which_applications(infinity).

which_applications(Node) ->
    rpc:call(Node, application, which_applications, [infinity]).
