%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2023. All Rights Reserved.
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
-module(megaco_mreq_SUITE).

-export([
 	 suite/0, all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2,

         req_and_rep/1,
         req_and_pending/1,
         req_and_cancel/1

        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").
-include("megaco_test_lib.hrl").

-define(TEST_VERBOSITY, debug).
-define(MGC_VERBOSITY,  debug).
-define(MG_VERBOSITY,   debug).

-define(LOAD_COUNTER_START, 10).
-define(A4444, ["11111111", "00000000", "00000000"]).
-define(A4445, ["11111111", "00000000", "11111111"]).
-define(A5555, ["11111111", "11111111", "00000000"]).
-define(A5556, ["11111111", "11111111", "11111111"]).

-define(MGC_START(Pid, Mid, ET, Conf, Verb), 
	megaco_test_mgc:start(Pid, Mid, ET, Conf, Verb)).
-define(MGC_STOP(Pid), megaco_test_mgc:stop(Pid)).
-define(MGC_GET_STATS(Pid, No), megaco_test_mgc:get_stats(Pid, No)).
-define(MGC_RESET_STATS(Pid), megaco_test_mgc:reset_stats(Pid)).
-define(MGC_REQ_DISC(Pid,To), megaco_test_mgc:request_discard(Pid,To)).
-define(MGC_REQ_PEND(Pid,To), megaco_test_mgc:request_pending(Pid,To)).
-define(MGC_REQ_HAND(Pid), megaco_test_mgc:request_handle(Pid)).

-define(MG_START(Pid, Mid, Enc, Transp, Conf, Verb), 
	megaco_test_mg:start(Pid, Mid, Enc, Transp, Conf, Verb)).
-define(MG_STOP(Pid),                megaco_test_mg:stop(Pid)).
-define(MG_GET_STATS(Pid),           megaco_test_mg:get_stats(Pid)).
-define(MG_RESET_STATS(Pid),         megaco_test_mg:reset_stats(Pid)).
-define(MG_SERV_CHANGE(Pid),         megaco_test_mg:service_change(Pid)).
-define(MG_NOTIF_RAR(Pid),           megaco_test_mg:notify_request_and_reply(Pid)).
-define(MG_NOTIF_REQ(Pid),           megaco_test_mg:notify_request(Pid)).
-define(MG_NOTIF_AR(Pid),            megaco_test_mg:await_notify_reply(Pid)).
-define(MG_CANCEL(Pid,R),            megaco_test_mg:cancel_request(Pid,R)).
-define(MG_APPLY_LOAD(Pid,CntStart), megaco_test_mg:apply_load(Pid,CntStart)).


%%======================================================================
%% Common Test interface functions
%%======================================================================

suite() -> 
    [{ct_hooks, [ts_install_cth]}].

all() -> 
    %% This is a temporary messure to ensure that we can 
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

     {all,                  [], all_cases()}
    ].

inet_backend_default_cases() ->
    [{all, [], all_cases()}].

inet_backend_inet_cases() ->
    [{all, [], all_cases()}].

inet_backend_socket_cases() ->
    [{all, [], all_cases()}].

all_cases() -> 
    [
     req_and_rep,
     req_and_pending,
     req_and_cancel
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

end_per_group(_Group, Config) ->
    Config.



init_per_testcase(Case, Config) ->
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

req_and_rep(suite) ->
    [];
req_and_rep(doc) ->
    [];
req_and_rep(Config) when is_list(Config) ->
    Pre = fun() ->
                  MgcNode = make_node_name(mgc),
                  Mg1Node = make_node_name(mg1),
                  Mg2Node = make_node_name(mg2),
                  Mg3Node = make_node_name(mg3),
                  Mg4Node = make_node_name(mg4),
                  d("try start nodes: "
                    "~n      MgcNode: ~p"
                    "~n      Mg1Node: ~p"
                    "~n      Mg2Node: ~p"
                    "~n      Mg3Node: ~p"
                    "~n      Mg4Node: ~p", 
                    [MgcNode, Mg1Node, Mg2Node, Mg3Node, Mg4Node]),
                  Nodes = [MgcNode, Mg1Node, Mg2Node, Mg3Node, Mg4Node],
                  ok    = ?START_NODES(Nodes, true),
                  Nodes
          end,
    Case = fun(X) ->
                   SCO = ?config(socket_create_opts, Config),
                   do_req_and_rep(SCO, X)
           end,
    Post = fun(Nodes) ->
                   d("stop nodes (in the reverse order):"
                     "~n       ~p", [Nodes]),
                   ?STOP_NODES(lists:reverse(Nodes))
           end,
    try_tc(req_and_rep, Pre, Case, Post).

do_req_and_rep(SCO, [MgcNode, Mg1Node, Mg2Node, Mg3Node, Mg4Node]) ->
    %% Start the MGC and MGs
    i("start the MGC"),    
    ET = [{text,tcp}, {text,udp}, {binary,tcp}, {binary,udp}],
    {ok, Mgc} = 
	?MGC_START(MgcNode, {deviceName, "ctrl"}, ET, SCO, ?MGC_VERBOSITY),

    i("start and connect the MGs"),    
    MgConf0 = [{Mg1Node, "mg1", text,   tcp, ?MG_VERBOSITY},
	       {Mg2Node, "mg2", text,   udp, ?MG_VERBOSITY},
	       {Mg3Node, "mg3", binary, tcp, ?MG_VERBOSITY},
	       {Mg4Node, "mg4", binary, udp, ?MG_VERBOSITY}],
    MgConf = rar_connect_mg(SCO, MgConf0, []),

    %% Collect the (initial) MGs statistics
    Stats1 = rar_get_mg_stats(MgConf, []),
    d("stats for the MGs: "
      "~n      ~p", [Stats1]),

    %% Collect and check the MGC statistics
    i("collect and check the MGC stats"),    
    {ok, MgcStats1} = ?MGC_GET_STATS(Mgc, 1),
    d("stats (1) for Mgc: "
      "~n      ~p"
      "~n", [MgcStats1]),


    sleep(1000),


    %% And apply some load 
    i("apply traffic load"),
    ok = rar_apply_load(MgConf),

    %% Await completion of load part and the collect traffic
    i("await load completion"),
    ok = rar_await_load_complete(MgConf),


    sleep(1000),


    i("collect the MGs statistics"),
    Stats2 = rar_get_mg_stats(MgConf, []),
    d("stats for MGs: "
      "~n      ~p", [Stats2]),

    i("collect the MGC statistics"),
    {ok, MgcStats2} = ?MGC_GET_STATS(Mgc, 1),
    d("stats (1) for Mgc: "
      "~n      ~p"
      "~n", [MgcStats2]),


    sleep(1000),


    %% Reset counters
    i("reset the MGs statistics"),
    rar_reset_mg_stats(MgConf),
    Stats3 = rar_get_mg_stats(MgConf, []),
    d("stats for the MGs: "
      "~n      ~p", [Stats3]),

    i("reset the MGC statistics"),
    rar_reset_mgc_stats(Mgc),
    {ok, MgcStats3} = ?MGC_GET_STATS(Mgc, 1),
    d("stats (1) for Mgc: "
      "~n      ~p"
      "~n", [MgcStats3]),


    sleep(1000),


    %% Tell MGs to stop
    i("stop the MGs"),
    rar_stop_mg(MgConf),


    sleep(1000),


    %% Collect the statistics
    i("collect the MGC statistics"),
    {ok, MgcStats4} = ?MGC_GET_STATS(Mgc, 1),
    d("stats (1) for Mgc: "
      "~n      ~p", [MgcStats4]),
    {ok, MgcStats5} = ?MGC_GET_STATS(Mgc, 2),
    d("stats (2) for Mgc: "
      "~n      ~p"
      "~n", [MgcStats5]),

    %% Tell Mgc to stop
    i("stop the MGC"),
    ?MGC_STOP(Mgc),

    i("done", []),
    ok.


rar_connect_mg(_, [], Acc) ->
    lists:reverse(Acc);
rar_connect_mg(SCO, [{Node, Name, Coding, Trans, Verb}|Mg], Acc) ->
    Pid = rar_connect_mg(SCO, Node, Name, Coding, Trans, Verb),
    rar_connect_mg(SCO, Mg, [{Name, Pid}|Acc]).

rar_connect_mg(SCO, Node, Name, Coding, Trans, Verb) ->
    Mid = {deviceName, Name}, 
    {ok, Pid} = ?MG_START(Node, Mid, Coding, Trans, SCO, Verb),

    %% Ask the MGs to do a service change
    Res = ?MG_SERV_CHANGE(Pid),
    d("rar_connect_mg -> (~s) service change result: ~p", [Name, Res]),

    Pid.


rar_stop_mg(MGs) ->
    [?MG_STOP(Pid) || {_Name, Pid} <- MGs].


rar_get_mg_stats([], Acc) ->
    lists:reverse(Acc);
rar_get_mg_stats([{Name, Pid}|Mgs], Acc) ->
    {ok, Stats} = ?MG_GET_STATS(Pid),
    d("rar_get_mg_stats -> stats for ~s: "
      "~n      ~p"
      "~n", [Name, Stats]),
    rar_get_mg_stats(Mgs, [{Name, Stats}|Acc]).


rar_apply_load([]) ->
    ok;
rar_apply_load([{_, MG}|MGs]) ->
    ?MG_APPLY_LOAD(MG,?LOAD_COUNTER_START),
    rar_apply_load(MGs).


rar_reset_mg_stats([]) ->
    ok;
rar_reset_mg_stats([{Name, Pid}|MGs]) ->
    d("rar_reset_mg_stats -> resetting ~s", [Name]),
    ?MG_RESET_STATS(Pid),
    rar_reset_mg_stats(MGs).

rar_reset_mgc_stats(Mgc) ->
    d("rar_reset_mgc_stats -> resetting ~p", [Mgc]),
    ?MGC_RESET_STATS(Mgc).

    
rar_await_load_complete([]) ->
    ok;
rar_await_load_complete(MGs0) ->
    receive
	{load_complete, Pid} ->
	    d("received load_complete from ~p", [Pid]),
	    MGs1 = lists:keydelete(Pid, 2, MGs0),
	    rar_await_load_complete(lists:delete(Pid, MGs1));
	{'EXIT', Pid, Reason} ->
	    e("exit signal from ~p: ~p", [Pid, Reason]),
	    case lists:keymember(Pid, 2, MGs0) of
		true ->
		    exit({mg_exit, Pid, Reason});
		false ->
		    MGs1 = lists:keydelete(Pid, 2, MGs0),
		    rar_await_load_complete(lists:delete(Pid, MGs1))
	    end
    end.
	    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

req_and_pending(suite) ->
    [];
req_and_pending(doc) ->
    [];
req_and_pending(Config) when is_list(Config) ->
    Pre = fun() ->
                  MgcNode = make_node_name(mgc),
                  MgNode  = make_node_name(mg),
                  d("try starting nodes: "
                    "~n      MgcNode: ~p"
                    "~n      MgNode:  ~p", [MgcNode, MgNode]),
                  Nodes = [MgcNode, MgNode], 
                  ok    = ?START_NODES(Nodes),
                  Nodes
          end,
    Case = fun(X) ->
                   SCO = ?config(socket_create_opts, Config),
                   do_req_and_pending(SCO, X)
           end,
    Post = fun(Nodes) ->
                   d("stop nodes (in the reverse order):"
                     "~n       ~p", [Nodes]),
                   ?STOP_NODES(lists:reverse(Nodes))
           end,
    try_tc(req_and_pending, Pre, Case, Post).

do_req_and_pending(SCO, [MgcNode, MgNode]) ->

    %% Start the MGC and MGs
    i("try start the MGC"),    
    ET = [{text,tcp}, {text,udp}, {binary,tcp}, {binary,udp}],
    {ok, Mgc} = 
	?MGC_START(MgcNode, {deviceName, "ctrl"}, ET, SCO, ?MGC_VERBOSITY),

    i("try start the MG"),
    {ok, Mg} = 
	?MG_START(MgNode, {deviceName, "mg"}, text, tcp, SCO, ?MG_VERBOSITY),

    i("connect MG (to MFC)"),
    Res1 = ?MG_SERV_CHANGE(Mg),
    d("service change result: ~p", [Res1]),

    sleep(1000),

    i("[MGC] change request action to pending"),
    {ok, _} = ?MGC_REQ_PEND(Mgc, 3500),

    i("[MG] send notify request"),
    {ok, Res2} = ?MG_NOTIF_RAR(Mg),
    d("notify reply: ~p", [Res2]),

    sleep(1000),

    %% Tell MG to stop
    i("stop the MG"),
    ?MG_STOP(Mg),

    %% Tell Mgc to stop
    i("stop the MGC"),
    ?MGC_STOP(Mgc),

    i("done", []),
    ok.


    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

req_and_cancel(suite) ->
    [];
req_and_cancel(doc) ->
    [];
req_and_cancel(Config) when is_list(Config) ->
    Pre = fun() ->
                  MgcNode = make_node_name(mgc),
                  MgNode  = make_node_name(mg),
                  d("try start nodes: "
                    "~n      MgcNode: ~p"
                    "~n      MgNode:  ~p", 
                    [MgcNode, MgNode]),
                  Nodes = [MgcNode, MgNode],
                  ok    = ?START_NODES(Nodes),
                  Nodes
          end,
    Case = fun(X) ->
                   SCO = ?config(socket_create_opts, Config),
                   do_req_and_cancel(SCO, X)
           end,
    Post = fun(Nodes) ->
                   d("stop nodes (in the reverse order):"
                     "~n       ~p", [Nodes]),
                   ?STOP_NODES(lists:reverse(Nodes))
           end,
    try_tc(req_and_cancel, Pre, Case, Post).

do_req_and_cancel(SCO, [MgcNode, MgNode]) ->
    %% Start the MGC and MGs
    i("start the MGC"),    
    ET = [{text,tcp}, {text,udp}, {binary,tcp}, {binary,udp}],
    {ok, Mgc} = 
	?MGC_START(MgcNode, {deviceName, "ctrl"}, ET, SCO, ?MGC_VERBOSITY),

    i("start the MG"),
    {ok, Mg} =
	?MG_START(MgNode, {deviceName, "mg"}, text, tcp, SCO, ?MG_VERBOSITY),

    i("connect the MG"),
    Res1 = ?MG_SERV_CHANGE(Mg),
    d("service change result: ~p", [Res1]),


    sleep(1000),

    i("change request action to pending"),
    {ok, _} = ?MGC_REQ_DISC(Mgc,5000),

    i("send notify request"),
    ?MG_NOTIF_REQ(Mg),
    
    d("wait some to get it going",[]),
    sleep(1000),

    i("now cancel the notify request"),
    ok = ?MG_CANCEL(Mg, req_and_cancel),

    i("now await the notify request result"),
    Res2 = ?MG_NOTIF_AR(Mg),
    rac_analyze_result(Res2),
    

    %% Tell MGs to stop
    i("stop the MG"),
    ?MG_STOP(Mg),

    %% Tell Mgc to stop
    i("stop the MGC"),
    ?MGC_STOP(Mgc),

    i("done", []),
    ok.


rac_analyze_result({ok, {_PV,Res}}) ->
    i("rac_analyze_result -> notify request result:"
      "~n      ~p", [Res]),
    rac_analyze_result2(Res);
rac_analyze_result(Unexpected) ->
    e("rac_analyze_result -> unexpected result: "
      "~n      ~p", [Unexpected]),
    exit({unexpected_result, Unexpected}).

rac_analyze_result2({error,{user_cancel, req_and_cancel}}) ->
    ok;
rac_analyze_result2([]) ->
    ok;
rac_analyze_result2([{error,{user_cancel, req_and_cancel}}|Res]) ->
    rac_analyze_result2(Res);
rac_analyze_result2([Unknown|_Res]) ->
    e("rac_analyze_result2 -> unexpected result: "
      "~n      ~p", [Unknown]),
    exit({unknown_result, Unknown}).
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_node_name(Name) ->
    case string:tokens(atom_to_list(node()), [$@]) of
	[_,Host] ->
	    list_to_atom(lists:concat([atom_to_list(Name) ++ "@" ++ Host]));
	_ ->
	    exit("Test node must be started with '-sname'")
     end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sleep(X) ->
    receive after X -> ok end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_tc(TCName, Pre, Case, Post) ->
    try_tc(TCName, "TEST", ?TEST_VERBOSITY, Pre, Case, Post).

try_tc(TCName, Name, Verbosity, Pre, Case, Post) ->
    ?TRY_TC(TCName, Name, Verbosity, Pre, Case, Post).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

p(F, A) ->
    io:format("*** [~s] ~p ***"
	      "~n   " ++ F ++ "~n", 
	      [?FTS(), self() | A]).

%% e(F) ->
%%     i(F, []).

e(F, A) ->
    print(info, get(verbosity), "ERROR", get(tc), F, A).


i(F) ->
    i(F, []).

i(F, A) ->
    print(info, get(verbosity), "INFO", get(tc), F, A).


%% d(F) ->
%%     d(F, []).

d(F, A) ->
    print(debug, get(verbosity), "DBG", get(tc), F, A).


printable(_, debug)   -> true;
printable(info, info) -> true;
printable(_,_)        -> false.

print(Severity, Verbosity, P, TC, F, A) ->
    print(printable(Severity,Verbosity), P, TC, F, A).

print(true, P, TC, F, A) ->
    io:format("*** [~s] [~s] ~p ~s:~w ***"
	      "~n   " ++ F ++ "~n", 
	      [?FTS(), P, self(), get(sname), TC | A]);
print(_, _, _, _, _) ->
    ok.

