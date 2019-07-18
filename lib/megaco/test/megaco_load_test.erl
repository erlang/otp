%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2019. All Rights Reserved.
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
-module(megaco_load_test).

-export([
         all/0,
         groups/0,

         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2,

         single_user_light_load/1,
         single_user_medium_load/1,
         single_user_heavy_load/1,
         single_user_extreme_load/1,

         multi_user_light_load/1,
         multi_user_medium_load/1,
         multi_user_heavy_load/1,
         multi_user_extreme_load/1,

         t/0, t/1
        ]).

-export([
         do_multi_load/3,
         multi_load_collector/7
        ]).


-include("megaco_test_lib.hrl").
-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").

-define(TEST_VERBOSITY, debug).
-define(MGC_VERBOSITY,  silence).
-define(MG_VERBOSITY,   silence).

-define(SINGLE_USER_LOAD_NUM_REQUESTS, 1000).
-define(MULTI_USER_LOAD_NUM_REQUESTS,  1000).

-define(MGC_START(Pid, Mid, ET, Conf, Verb), 
        megaco_test_mgc:start(Pid, Mid, ET, 
			      [{megaco_trace, false}] ++ Conf, Verb)).
-define(MGC_STOP(Pid), megaco_test_mgc:stop(Pid)).
-define(MGC_USER_INFO(Pid,Tag), megaco_test_mgc:user_info(Pid,Tag)).
-define(MGC_CONN_INFO(Pid,Tag), megaco_test_mgc:conn_info(Pid,Tag)).
-define(MGC_SET_VERBOSITY(Pid, V), megaco_test_mgc:verbosity(Pid, V)).

-define(MG_START(Pid, Mid, Enc, Transp, Conf, Verb), 
        megaco_test_mg:start(Pid, Mid, Enc, Transp, 
			     [{megaco_trace, false},
			      {transport_opts, [{serialize, true}]}] ++ Conf, 
			     Verb)).
-define(MG_STOP(Pid), megaco_test_mg:stop(Pid)).
-define(MG_USER_INFO(Pid,Tag), megaco_test_mg:user_info(Pid,Tag)).
-define(MG_CONN_INFO(Pid,Tag), megaco_test_mg:conn_info(Pid,Tag)).
-define(MG_SERV_CHANGE(Pid), megaco_test_mg:service_change(Pid)).
-define(MG_MLOAD(Pid, NL, NR), 
	timer:tc(megaco_test_mg, apply_multi_load, [Pid, NL, NR])).
-define(MG_LOAD(Pid, NL, NR), megaco_test_mg:apply_multi_load(Pid, NL, NR)).
-define(MG_SET_VERBOSITY(Pid, V), megaco_test_mg:verbosity(Pid, V)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


t()     -> megaco_test_lib:t(?MODULE).
t(Case) -> megaco_test_lib:t({?MODULE, Case}).

%% Test server callbacks
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
    megaco_test_lib:init_per_testcase(Case, Config).
    
end_per_testcase(Case, Config) ->
    process_flag(trap_exit, false),
    megaco_test_lib:end_per_testcase(Case, Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() -> 
    [
     {group, single},
     {group, multi}
    ].

groups() -> 
    [
     {single, [], single_cases()},
     {multi,  [], multi_cases()}
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
    
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_user_light_load(suite) ->
    [];
single_user_light_load(doc) ->
    [];
single_user_light_load(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(tc,        single_user_light_load),
    put(sname,     "TEST"),
    i("starting"),

    load_controller(Config, 
		    fun(Env) -> 
			    populate(Env), 
			    exit( single_user_load(5) ) 
		    end),

    i("done", []),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_user_medium_load(suite) ->
    [];
single_user_medium_load(doc) ->
    [];
single_user_medium_load(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(tc,        single_user_medium_load),
    put(sname,     "TEST"),
    i("starting"),

    load_controller(Config, 
		    fun(Env) -> 
			    populate(Env), 
			    exit( single_user_load(15) ) 
		    end),

    i("done", []),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_user_heavy_load(suite) ->
    [];
single_user_heavy_load(doc) ->
    [];
single_user_heavy_load(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(tc,        single_user_heavy_load),
    put(sname,     "TEST"),
    i("starting"),

    load_controller(Config, 
		    fun(Env) -> 
			    populate(Env), 
			    exit( single_user_load(25) ) 
		    end),

    i("done", []),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_user_extreme_load(suite) ->
    [];
single_user_extreme_load(doc) ->
    [];
single_user_extreme_load(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(tc,        single_user_extreme_load),
    put(sname,     "TEST"),
    i("starting"),

    load_controller(Config, 
		    fun(Env) -> 
			    populate(Env), 
			    exit( single_user_load(100) ) 
		    end),

    i("done", []),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_user_light_load(suite) ->
    [];
multi_user_light_load(doc) ->
    [];
multi_user_light_load(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(tc,        multi_user_light_load),
    put(sname,     "TEST"),
    i("starting"),

    load_controller(Config, 
		    fun(Env) -> 
			    populate(Env), 
			    exit( multi_user_load(3,1) ) 
		    end),

    i("done", []),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_user_medium_load(suite) ->
    [];
multi_user_medium_load(doc) ->
    [];
multi_user_medium_load(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(tc,        multi_user_medium_load),
    put(sname,     "TEST"),
    i("starting"),

    load_controller(Config, 
		    fun(Env) -> 
			    populate(Env), 
			    exit( multi_user_load(3,5) ) 
		    end),

    i("done", []),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_user_heavy_load(suite) ->
    [];
multi_user_heavy_load(doc) ->
    [];
multi_user_heavy_load(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(tc,        multi_user_heavy_load),
    put(sname,     "TEST"),
    i("starting"),

    load_controller(Config, 
		    fun(Env) -> 
			    populate(Env), 
			    exit( multi_user_load(3,10) ) 
		    end),

    i("done", []),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_user_extreme_load(suite) ->
    [];
multi_user_extreme_load(doc) ->
    [];
multi_user_extreme_load(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(tc,        multi_user_extreme_load),
    put(sname,     "TEST"),
    i("starting"),

    load_controller(Config, 
		    fun(Env) -> 
			    populate(Env), 
			    exit( multi_user_load(3,15) ) 
		    end),

    i("done", []),
    ok.


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
	{'EXIT', Loader, normal} ->
	    d("load_controller -> "
	      "loader [~p] terminated with normal", [Loader]),
	    ok;
	{'EXIT', Loader, ok} ->
	    d("load_controller -> "
	      "loader [~p] terminated with ok~n", [Loader]),
	    ok;
	{'EXIT', Loader, {skipped, {fatal, Reason, File, Line}}} ->
	    i("load_controller -> "
	      "loader [~p] terminated with fatal skip"
	      "~n   Reason: ~p"
	      "~n   At:     ~p:~p", [Loader, Reason, File, Line]),
	    ?SKIP(Reason);
	{'EXIT', Loader, {skipped, Reason}} ->
	    i("load_controller -> "
	      "loader [~p] terminated with skip"
	      "~n   Reason: ~p", [Loader, Reason]),
	    ?SKIP(Reason);
	{'EXIT', Loader, Reason} ->
	    i("load_controller -> "
	      "loader [~p] terminated with"
	      "~n   ~p", [Loader, Reason]),
	    erlang:error({unexpected_loader_result, Reason})
    after SkipTimeout ->
	    i("load_controller -> "
	      "loader [~p] timeout", [Loader]),
	    exit(Loader, kill),
	    ?SKIP({timeout, SkipTimeout, TcTimeout})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_user_load(NumLoaders) ->
    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("Nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p", [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    %% Start the MGC and MGs
    i("[MGC] start"),
    MgcMid = {deviceName, "ctrl"},
    ET     = [{text, tcp, [{serialize, true}]}],
    DSI = maybe_display_system_info(NumLoaders),
    {ok, Mgc} = ?MGC_START(MgcNode, MgcMid, ET, DSI, ?MGC_VERBOSITY),

    i("[MG] start"),
    MgMid = {deviceName, "mg"},
    {ok, Mg} = ?MG_START(MgNode, MgMid, text, tcp, DSI, ?MG_VERBOSITY),

    d("MG user info: ~p", [?MG_USER_INFO(Mg, all)]),

    i("[MG] connect to the MGC (service change)"),    
    ServChRes = ?MG_SERV_CHANGE(Mg),
    d("service change result: ~p", [ServChRes]),

    megaco_test_mg:update_conn_info(Mg,reply_timer,1000),
    megaco_test_mgc:update_conn_info(Mgc,reply_timer,1000),

    d("MG conn info: ~p", [?MG_CONN_INFO(Mg, all)]),

    d("apply the load"),
    Res = ?MG_MLOAD(Mg, NumLoaders, ?SINGLE_USER_LOAD_NUM_REQUESTS),
    case Res of
	{Time, {ok, Ok, Err}} ->
	    Sec = Time / 1000000,
	    io:format("~nmultiple loaders result: ~n"
		      "   Number of successfull: ~w~n"
		      "   Number of failure:     ~w~n"
		      "   Time:                  ~w seconds~n"
		      "   Calls / seconds        ~w~n~n", 
		      [Ok, Err, Sec, (NumLoaders * ?SINGLE_USER_LOAD_NUM_REQUESTS)/Sec]);
	{Time, Error} ->
	    io:format("SUL: multiple loaders failed: ~p after ~w~n", 
		      [Error, Time])
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
    
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_user_load(NumUsers, NumLoaders) 
  when (is_integer(NumUsers) andalso (NumUsers > 1) andalso 
	is_integer(NumLoaders) andalso (NumLoaders >= 1)) ->
    MgcNode = make_node_name(mgc),
    MgNodes = make_node_names(mg, NumUsers),
    d("Nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNodes: ~p", [MgcNode, MgNodes]),
    ok = megaco_test_lib:start_nodes([MgcNode| MgNodes], ?FILE, ?LINE),

    %% Start the MGC and MGs
    i("[MGC] start"),
    MgcMid = {deviceName, "ctrl"},
    ET     = [{text, tcp, [{serialize, false}]}],
    DSI = maybe_display_system_info(2 * NumUsers * NumLoaders),
    {ok, Mgc} = ?MGC_START(MgcNode, MgcMid, ET, DSI, ?MGC_VERBOSITY),

    megaco_test_mgc:update_user_info(Mgc,reply_timer,1000),
    d("MGC user info: ~p", [?MGC_USER_INFO(Mgc, all)]),

    MgUsers = make_mids(MgNodes),

    d("start MGs, apply the load and stop MGs"),
    ok = multi_load(MgUsers, DSI, NumLoaders, ?MULTI_USER_LOAD_NUM_REQUESTS),

    i("flush the message queue: ~p", [megaco_test_lib:flush()]),

    ?MGC_SET_VERBOSITY(Mgc, debug),

    %% Tell Mgc to stop
    i("[MGC] stop"),
    ?MGC_STOP(Mgc),

    i("flush the message queue: ~p", [megaco_test_lib:flush()]),

    ok.


multi_load(MGs, DSI, NumLoaders, NumReqs) ->
    d("multi_load -> entry with"
      "~n   MGs:        ~p"
      "~n   DSI:        ~p"
      "~n   NumLoaders: ~p"
      "~n   NumReqs:    ~p", [MGs, DSI, NumLoaders, NumReqs]),

    Pids = multi_load_collector_start(MGs, DSI, NumLoaders, NumReqs, []),
    case timer:tc(?MODULE, do_multi_load, [Pids, NumLoaders, NumReqs]) of
	{Time, {ok, OKs, []}} ->
	    Sec = Time / 1000000,
	    multi_load_collector_calc(Sec, OKs);
	{Time, Error} ->
	    Sec = Time/1000000,
	    io:format("~nmulti load failed after ~.1f:~n~p~n~n", [Sec,Error]),
	    {error, Error}
    end.

do_multi_load(Pids, _NumLoaders, _NumReqs) ->
    Fun = 
	fun({P,_}) -> 
		d("apply multi load for ~p", [P]),
		P ! {apply_multi_load, self()} 
	end,
    lists:foreach(Fun, Pids),
    await_multi_load_collectors(Pids, [], []).

multi_load_collector_start([], _DSI, _NumLoaders, _NumReqs, Pids) ->
    Pids;
multi_load_collector_start([{Mid, Node}|MGs], DSI, NumLoaders, NumReqs, Pids) ->
    Env = get(),
    Pid = spawn_link(?MODULE, multi_load_collector, 
		     [self(), Node, Mid, DSI, NumLoaders, NumReqs, Env]),
    multi_load_collector_start(MGs, DSI, NumLoaders, NumReqs, [{Pid,Mid}|Pids]).

get_env(Key, Env) ->
    case lists:keysearch(Key, 1, Env) of
	{value, {Key, Val}} ->
	    Val;
	_ ->
	    undefined
    end.

multi_load_collector(Parent, Node, Mid, DSI, NumLoaders, NumReqs, Env) ->
    put(verbosity, get_env(verbosity, Env)),
    put(tc, get_env(tc, Env)),
    put(sname, get_env(sname, Env) ++ "-loader"),
    case ?MG_START(Node, Mid, text, tcp, DSI, ?MG_VERBOSITY) of
	{ok, Pid} ->
	    d("MG ~p user info: ~n~p", [Mid, ?MG_USER_INFO(Pid,all)]),
	    ServChRes = ?MG_SERV_CHANGE(Pid),
	    d("service change result: ~p", [ServChRes]),
	    megaco_test_mg:update_conn_info(Pid,reply_timer,1000),
	    d("MG ~p conn info: ~p", [Mid, ?MG_CONN_INFO(Pid,all)]),
	    multi_load_collector_loop(Parent, Pid, Mid, NumLoaders, NumReqs);
	Else ->
	    Parent ! {load_start_failed, self(), Mid, Else}
    end.

multi_load_collector_loop(Parent, Pid, Mid, NumLoaders, NumReqs) ->
    d("multi_load_collector_loop -> entry with"
      "~n   Parent:     ~p"
      "~n   Pid:        ~p"
      "~n   Mid:        ~p"
      "~n   NumLoaders: ~p"
      "~n   NumReqs:    ~p"
      "~nwhen"
      "~n   self():     ~p"
      "~n   node():     ~p", 
      [Parent, Pid, Mid, NumLoaders, NumReqs, self(), node()]),
    receive
	{apply_multi_load, Parent} ->
	    Res = ?MG_LOAD(Pid, NumLoaders, NumReqs),
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
	    i("await_multi_load_collectors -> "
	      "received ok complete from "
	      "~n   ~p [~p]", [Pid, Mg]),
	    Pids2 = lists:keydelete(Pid, 1, Pids),
	    Oks2  = [{Mg, Ok, Err}|Oks],
	    await_multi_load_collectors(Pids2, Oks2, Errs);
	{load_complete, Pid, Mg, Error} ->
	    i("await_multi_load_collectors -> "
	      "received error complete from "
	      "~n   ~p [~p]: "
	      "~n   ~p", [Pid, Mg, Error]),
	    Pids2 = lists:keydelete(Pid, 1, Pids),
	    Errs2 = [{Mg, Error}|Errs],
	    await_multi_load_collectors(Pids2, Oks, Errs2);

	{'EXIT', Pid, normal} ->
	    %% This is assumed to be one of the collectors
	    i("await_multi_load_collectors -> "
	      "received (normal) exit signal from ~p", [Pid]),
	    await_multi_load_collectors(Pids, Oks, Errs);

	{'EXIT', Pid, Reason} ->
	    i("await_multi_load_collectors -> "
	      "received unexpected exit from ~p:"
	      "~n   ~p", [Pid, Reason]),
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
	    i("await_multi_load_collectors -> received unexpected message:"
	      "~n~p", [Else]),
	    await_multi_load_collectors(Pids, Oks, Errs)
    after 
	5000 ->
	    i("await_multi_load_collectors -> still awaiting reply from:"
	      "~n~p", [Pids]),
	    await_multi_load_collectors(Pids, Oks, Errs)
    end.
	    
		
%% Note that this is an approximation...we run all the
%% MGs in parrallel, so it should be "accurate"...
multi_load_collector_calc(Sec, Oks) ->
    Succs = lists:sum([Ok   || {_, Ok,   _} <- Oks]),
    Fails = lists:sum([Err  || {_,  _, Err} <- Oks]),
    io:format("~ntotal multiple loaders result: ~n"
	      "   Number of successfull: ~w~n"
	      "   Number of failure:     ~w~n"
	      "   Total Calls / seconds: ~.2f~n~n", 
	      [Succs, Fails, Sec]),
    ok.
    
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

min(M) -> timer:minutes(M).

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


