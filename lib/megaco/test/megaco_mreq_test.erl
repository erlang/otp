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
-module(megaco_mreq_test).

-export([
         all/0,
         groups/0,

         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2,

         req_and_rep/1,
         req_and_pending/1,
         req_and_cancel/1,

         t/0, t/1
        ]).

-include("megaco_test_lib.hrl").
-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").

-define(TEST_VERBOSITY, debug).
-define(MGC_VERBOSITY,  debug).
-define(MG_VERBOSITY,   debug).

-define(LOAD_COUNTER_START, 10).
-define(A4444, ["11111111", "00000000", "00000000"]).
-define(A4445, ["11111111", "00000000", "11111111"]).
-define(A5555, ["11111111", "11111111", "00000000"]).
-define(A5556, ["11111111", "11111111", "11111111"]).

-define(MGC_START(Pid, Mid, ET, Verb), 
	megaco_test_mgc:start(Pid, Mid, ET, Verb)).
-define(MGC_STOP(Pid), megaco_test_mgc:stop(Pid)).
-define(MGC_GET_STATS(Pid, No), megaco_test_mgc:get_stats(Pid, No)).
-define(MGC_RESET_STATS(Pid), megaco_test_mgc:reset_stats(Pid)).
-define(MGC_REQ_DISC(Pid,To), megaco_test_mgc:request_discard(Pid,To)).
-define(MGC_REQ_PEND(Pid,To), megaco_test_mgc:request_pending(Pid,To)).
-define(MGC_REQ_HAND(Pid), megaco_test_mgc:request_handle(Pid)).

-define(MG_START(Pid, Mid, Enc, Transp, Verb), 
	megaco_test_mg:start(Pid, Mid, Enc, Transp, Verb)).
-define(MG_STOP(Pid),                megaco_test_mg:stop(Pid)).
-define(MG_GET_STATS(Pid),           megaco_test_mg:get_stats(Pid)).
-define(MG_RESET_STATS(Pid),         megaco_test_mg:reset_stats(Pid)).
-define(MG_SERV_CHANGE(Pid),         megaco_test_mg:service_change(Pid)).
-define(MG_NOTIF_RAR(Pid),           megaco_test_mg:notify_request_and_reply(Pid)).
-define(MG_NOTIF_REQ(Pid),           megaco_test_mg:notify_request(Pid)).
-define(MG_NOTIF_AR(Pid),            megaco_test_mg:await_notify_reply(Pid)).
-define(MG_CANCEL(Pid,R),            megaco_test_mg:cancel_request(Pid,R)).
-define(MG_APPLY_LOAD(Pid,CntStart), megaco_test_mg:apply_load(Pid,CntStart)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t()     -> megaco_test_lib:t(?MODULE).
t(Case) -> megaco_test_lib:t({?MODULE, Case}).


%% Test server callbacks
init_per_testcase(Case, Config) ->
    process_flag(trap_exit, true),
    megaco_test_lib:init_per_testcase(Case, Config).

end_per_testcase(Case, Config) ->
    process_flag(trap_exit, false),
    megaco_test_lib:end_per_testcase(Case, Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() -> 
    [req_and_rep, req_and_pending, req_and_cancel].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

req_and_rep(suite) ->
    [];
req_and_rep(doc) ->
    [];
req_and_rep(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    i("req_and_rep -> starting"),
    MgcNode = make_node_name(mgc),
    Mg1Node = make_node_name(mg1),
    Mg2Node = make_node_name(mg2),
    Mg3Node = make_node_name(mg3),
    Mg4Node = make_node_name(mg4),
    d("req_and_rep -> Nodes: "
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
    i("req_and_rep -> start the MGC"),    
    ET = [{text,tcp}, {text,udp}, {binary,tcp}, {binary,udp}],
    {ok, Mgc} = 
	?MGC_START(MgcNode, {deviceName, "ctrl"}, ET, ?MGC_VERBOSITY),

    i("req_and_rep -> start and connect the MGs"),    
    MgConf0 = [{Mg1Node, "mg1", text,   tcp, ?MG_VERBOSITY},
	       {Mg2Node, "mg2", text,   udp, ?MG_VERBOSITY},
	       {Mg3Node, "mg3", binary, tcp, ?MG_VERBOSITY},
	       {Mg4Node, "mg4", binary, udp, ?MG_VERBOSITY}],
    MgConf = req_and_rep_connect_mg(MgConf0, []),

    %% Collect the (initial) MGs statistics
    Stats1 = req_and_rep_get_mg_stats(MgConf, []),
    d("req_and_rep -> stats for the MGs: ~n~p", [Stats1]),

    %% Collect and check the MGC statistics
    i("req_and_rep -> collect and check the MGC stats"),    
    {ok, MgcStats1} = ?MGC_GET_STATS(Mgc, 1),
    d("req_and_rep  -> stats (1) for Mgc: ~n~p~n", [MgcStats1]),


    sleep(1000),


    %% And apply some load 
    i("req_and_rep -> apply traffic load"),
    ok = req_and_rep_apply_load(MgConf),

    %% Await completion of load part and the collect traffic
    i("req_and_rep -> await load completion"),
    ok = req_and_rep_await_load_complete(MgConf),


    sleep(1000),


    i("req_and_rep -> collect the MGs statistics"),
    Stats2 = req_and_rep_get_mg_stats(MgConf, []),
    d("req_and_rep -> stats for MGs: ~n~p", [Stats2]),

    i("req_and_rep -> collect the MGC statistics"),
    {ok, MgcStats2} = ?MGC_GET_STATS(Mgc, 1),
    d("req_and_rep  -> stats (1) for Mgc: ~n~p~n", [MgcStats2]),


    sleep(1000),


    %% Reset counters
    i("req_and_rep -> reset the MGs statistics"),
    req_and_rep_reset_mg_stats(MgConf),
    Stats3 = req_and_rep_get_mg_stats(MgConf, []),
    d("req_and_rep -> stats for the MGs: ~n~p", [Stats3]),

    i("req_and_rep -> reset the MGC statistics"),
    req_and_rep_reset_mgc_stats(Mgc),
    {ok, MgcStats3} = ?MGC_GET_STATS(Mgc, 1),
    d("req_and_rep  -> stats (1) for Mgc: ~n~p~n", [MgcStats3]),


    sleep(1000),


    %% Tell MGs to stop
    i("req_and_rep -> stop the MGs"),
    req_and_rep_stop_mg(MgConf),


    sleep(1000),


    %% Collect the statistics
    i("req_and_rep -> collect the MGC statistics"),
    {ok, MgcStats4} = ?MGC_GET_STATS(Mgc, 1),
    d("req_and_rep -> stats (1) for Mgc: ~n~p~n", [MgcStats4]),
    {ok, MgcStats5} = ?MGC_GET_STATS(Mgc, 2),
    d("req_and_rep -> stats (2) for Mgc: ~n~p~n", [MgcStats5]),

    %% Tell Mgc to stop
    i("req_and_rep -> stop the MGC"),
    ?MGC_STOP(Mgc),

    i("req_and_rep -> done", []),
    ok.


req_and_rep_connect_mg([], Acc) ->
    lists:reverse(Acc);
req_and_rep_connect_mg([{Node, Name, Coding, Trans, Verb}|Mg], Acc) ->
    Pid = req_and_rep_connect_mg(Node, Name, Coding, Trans, Verb),
    req_and_rep_connect_mg(Mg, [{Name, Pid}|Acc]).

req_and_rep_connect_mg(Node, Name, Coding, Trans, Verb) ->
    Mid = {deviceName, Name}, 
    {ok, Pid} = ?MG_START(Node, Mid, Coding, Trans, Verb),

    %% Ask the MGs to do a service change
    Res = ?MG_SERV_CHANGE(Pid),
    d("req_and_rep_connect_mg -> (~s) service change result: ~p", [Name,Res]),

    Pid.


req_and_rep_stop_mg(MGs) ->
    [?MG_STOP(Pid) || {_Name, Pid} <- MGs].


req_and_rep_get_mg_stats([], Acc) ->
    lists:reverse(Acc);
req_and_rep_get_mg_stats([{Name, Pid}|Mgs], Acc) ->
    {ok, Stats} = ?MG_GET_STATS(Pid),
    d("req_and_rep_get_mg_stats -> stats for ~s: ~n~p~n", [Name, Stats]),
    req_and_rep_get_mg_stats(Mgs, [{Name, Stats}|Acc]).


req_and_rep_apply_load([]) ->
    ok;
req_and_rep_apply_load([{_, MG}|MGs]) ->
    ?MG_APPLY_LOAD(MG,?LOAD_COUNTER_START),
    req_and_rep_apply_load(MGs).


req_and_rep_reset_mg_stats([]) ->
    ok;
req_and_rep_reset_mg_stats([{Name, Pid}|MGs]) ->
    d("req_and_rep_reset_mg_stats -> resetting ~s", [Name]),
    ?MG_RESET_STATS(Pid),
    req_and_rep_reset_mg_stats(MGs).

req_and_rep_reset_mgc_stats(Mgc) ->
    d("req_and_rep_reset_mgc_stats -> resetting ~p", [Mgc]),
    ?MGC_RESET_STATS(Mgc).

    
req_and_rep_await_load_complete([]) ->
    ok;
req_and_rep_await_load_complete(MGs0) ->
    receive
	{load_complete, Pid} ->
	    d("received load_complete from ~p", [Pid]),
	    MGs1 = lists:keydelete(Pid, 2, MGs0),
	    req_and_rep_await_load_complete(lists:delete(Pid, MGs1));
	{'EXIT', Pid, Reason} ->
	    i("exit signal from ~p: ~p", [Pid, Reason]),
	    case lists:keymember(Pid, 2, MGs0) of
		true ->
		    exit({mg_exit, Pid, Reason});
		false ->
		    MGs1 = lists:keydelete(Pid, 2, MGs0),
		    req_and_rep_await_load_complete(lists:delete(Pid, MGs1))
	    end
    end.
	    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

req_and_pending(suite) ->
    [];
req_and_pending(doc) ->
    [];
req_and_pending(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    i("req_and_pending -> starting"),

    MgcNode = make_node_name(mgc),
    Mg1Node = make_node_name(mg1),

    d("req_and_pending -> Nodes: "
      "~n   MgcNode: ~p"
      "~n   Mg1Node: ~p", 
      [MgcNode, Mg1Node]),
    ok = megaco_test_lib:start_nodes([MgcNode, Mg1Node], 
				     ?FILE, ?LINE),

    %% Start the MGC and MGs
    i("req_and_pending -> start the MGC"),    
    ET = [{text,tcp}, {text,udp}, {binary,tcp}, {binary,udp}],
    {ok, Mgc} = 
	?MGC_START(MgcNode, {deviceName, "ctrl"}, ET, ?MGC_VERBOSITY),

    i("req_and_pending -> start the MG"),
    {ok, Mg1} = 
	?MG_START(Mg1Node, {deviceName, "mg1"}, text, tcp, ?MG_VERBOSITY),

    i("req_and_pending -> connect the MG"),
    Res1 = ?MG_SERV_CHANGE(Mg1),
    d("req_and_pending -> service change result: ~p", [Res1]),

    sleep(1000),

    i("req_and_pending -> change request action to pending"),
    {ok, _} = ?MGC_REQ_PEND(Mgc,3500),

    i("req_and_pending -> send notify request"),
    {ok, Res2} = ?MG_NOTIF_RAR(Mg1),
    d("req_and_pending -> notify reply: ~p",[Res2]),

    sleep(1000),

    %% Tell MGs to stop
    i("req_and_rep -> stop the MGs"),
    ?MG_STOP(Mg1),

    %% Tell Mgc to stop
    i("req_and_pending -> stop the MGC"),
    ?MGC_STOP(Mgc),

    i("req_and_pending -> done", []),
    ok.


    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

req_and_cancel(suite) ->
    [];
req_and_cancel(doc) ->
    [];
req_and_cancel(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    i("req_and_cancel -> starting"),

    MgcNode = make_node_name(mgc),
    Mg1Node = make_node_name(mg1),

    d("req_and_cancel -> Nodes: "
      "~n   MgcNode: ~p"
      "~n   Mg1Node: ~p", 
      [MgcNode, Mg1Node]),
    ok = megaco_test_lib:start_nodes([MgcNode, Mg1Node], 
				     ?FILE, ?LINE),

    %% Start the MGC and MGs
    i("req_and_cancel -> start the MGC"),    
    ET = [{text,tcp}, {text,udp}, {binary,tcp}, {binary,udp}],
    {ok, Mgc} = 
	?MGC_START(MgcNode, {deviceName, "ctrl"}, ET, ?MGC_VERBOSITY),

    i("req_and_cancel -> start the MG"),
    {ok, Mg1} = 
	?MG_START(Mg1Node, {deviceName, "mg1"}, text, tcp, ?MG_VERBOSITY),

    i("req_and_cancel -> connect the MG"),
    Res1 = ?MG_SERV_CHANGE(Mg1),
    d("req_and_cancel -> service change result: ~p", [Res1]),


    sleep(1000),

    i("req_and_cancel -> change request action to pending"),
    {ok, _} = ?MGC_REQ_DISC(Mgc,5000),

    i("req_and_cancel -> send notify request"),
    ?MG_NOTIF_REQ(Mg1),
    
    d("req_and_cancel -> wait some to get it going",[]),
    sleep(1000),

    i("req_and_cancel -> now cancel the notify request"),
    ok = ?MG_CANCEL(Mg1,req_and_cancel),

    i("req_and_cancel -> now await the notify request result"),
    Res2 = ?MG_NOTIF_AR(Mg1),
    req_and_cancel_analyze_result(Res2),
    

    %% Tell MGs to stop
    i("req_and_rep -> stop the MGs"),
    ?MG_STOP(Mg1),

    %% Tell Mgc to stop
    i("req_and_cancel -> stop the MGC"),
    ?MGC_STOP(Mgc),

    i("req_and_cancel -> done", []),
    ok.% ?SKIP(not_implemented_yet).


req_and_cancel_analyze_result({ok,{_PV,Res}}) ->
    i("req_and_cancel -> notify request result: ~n   ~p", [Res]),
    req_and_cancel_analyze_result2(Res);
req_and_cancel_analyze_result(Unexpected) ->
    exit({unexpected_result,Unexpected}).

req_and_cancel_analyze_result2({error,{user_cancel,req_and_cancel}}) ->
    ok;
req_and_cancel_analyze_result2([]) ->
    ok;
req_and_cancel_analyze_result2([{error,{user_cancel,req_and_cancel}}|Res]) ->
    req_and_cancel_analyze_result2(Res);
req_and_cancel_analyze_result2([Unknown|_Res]) ->
    exit({unknown_result,Unknown}).
    
    
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

i(F) ->
    i(F, []).

i(F, A) ->
    print(info, get(verbosity), "", F, A).


%% d(F) ->
%%     d(F, []).

d(F, A) ->
    print(debug, get(verbosity), "DBG: ", F, A).


printable(_, debug)   -> true;
printable(info, info) -> true;
printable(_,_)        -> false.

print(Severity, Verbosity, P, F, A) ->
    print(printable(Severity,Verbosity), P, F, A).

print(true, P, F, A) ->
    io:format("*** [~s] ~s ~p ~s ***"
	      "~n   " ++ F ++ "~n", 
	      [?FTS(), P, self(), get(sname) | A]);
print(_, _, _, _) ->
    ok.

