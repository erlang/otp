%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2019. All Rights Reserved.
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
%% Purpose: Verify that it is possible to separately encode 
%%          the action requests list. Do this with all codec's
%%          that supports partial encode.
%%----------------------------------------------------------------------
-module(megaco_actions_test).

-export([
         all/0,
         groups/0,

         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2,

         t/0, t/1,

         pretty_text/1,
         flex_pretty_text/1,
         compact_text/1,
         flex_compact_text/1,
         erl_dist/1, 
         erl_dist_mc/1
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
-define(MGC_STOP(Pid),          megaco_test_mgc:stop(Pid)).
-define(MGC_GET_STATS(Pid, No), megaco_test_mgc:get_stats(Pid, No)).
-define(MGC_RESET_STATS(Pid),   megaco_test_mgc:reset_stats(Pid)).
-define(MGC_REQ_DISC(Pid,To),   megaco_test_mgc:request_discard(Pid,To)).
-define(MGC_REQ_PEND(Pid,To),   megaco_test_mgc:request_pending(Pid,To)).
-define(MGC_REQ_HAND(Pid),      megaco_test_mgc:request_handle(Pid)).

-define(MG_START(Pid, Mid, Codec, EC, Transp, Verb), 
	megaco_test_mg:start(Pid, Mid, {Codec, EC}, Transp, Verb)).
-define(MG_STOP(Pid),        megaco_test_mg:stop(Pid)).
-define(MG_GET_STATS(Pid),   megaco_test_mg:get_stats(Pid)).
-define(MG_RESET_STATS(Pid), megaco_test_mg:reset_stats(Pid)).
-define(MG_SERV_CHANGE(Pid), megaco_test_mg:service_change(Pid)).
-define(MG_NOTIF_RAR(Pid),   megaco_test_mg:notify_request_and_reply(Pid)).
-define(MG_NOTIF_REQ(Pid),   megaco_test_mg:notify_request(Pid)).
-define(MG_NOTIF_AR(Pid),    megaco_test_mg:await_notify_reply(Pid)).
-define(MG_CANCEL(Pid,R),    megaco_test_mg:cancel_request(Pid,R)).
-define(MG_APPLY_LOAD(Pid,CntStart), megaco_test_mg:apply_load(Pid,CntStart)).
-define(MG_EAR(Pid, Val),    megaco_test_mg:encode_ar_first(Pid, Val)).

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
    [pretty_text, flex_pretty_text, compact_text,
     flex_compact_text, erl_dist, erl_dist_mc].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pretty_text(suite) ->
    [];
pretty_text(doc) ->
    [];
pretty_text(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    i("pretty_text -> starting"),

    Codec = pretty_text,
    Version = 1, 
    EncodingConfig = [],
    req_and_rep(Config, Codec, Version, EncodingConfig).


flex_pretty_text(suite) ->
    [];
flex_pretty_text(doc) ->
    [];
flex_pretty_text(Config) when is_list(Config) ->
    ?SKIP(not_implemented_yet).


compact_text(suite) ->
    [];
compact_text(doc) ->
    [];
compact_text(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    i("compact_text -> starting"),

    Codec = compact_text,
    Version = 1, 
    EncodingConfig = [],
    req_and_rep(Config, Codec, Version, EncodingConfig).


flex_compact_text(suite) ->
    [];
flex_compact_text(doc) ->
    [];
flex_compact_text(Config) when is_list(Config) ->
    ?SKIP(not_implemented_yet).


erl_dist(suite) ->
    [];
erl_dist(doc) ->
    [];
erl_dist(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    i("erl_dist -> starting"),

    Codec = erl_dist,
    Version = 1, 
    EncodingConfig = [],
    req_and_rep(Config, Codec, Version, EncodingConfig).


erl_dist_mc(suite) ->
    [];
erl_dist_mc(doc) ->
    [];
erl_dist_mc(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    i("erl_dist_mc -> starting"),

    Codec = erl_dist,
    Version = 1, 
    EncodingConfig = [megaco_compressed],
    req_and_rep(Config, Codec, Version, EncodingConfig).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

req_and_rep(Config, Codec, _Version, EC) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    i("req_and_rep -> starting"),
    MgcNode = make_node_name(mgc),
    Mg1Node = make_node_name(mg1),
    Mg2Node = make_node_name(mg2),
    d("req_and_rep -> Nodes: "
      "~n   MgcNode: ~p"
      "~n   Mg1Node: ~p"
      "~n   Mg2Node: ~p", 
      [MgcNode, Mg1Node, Mg2Node]),
    ok = megaco_test_lib:start_nodes([MgcNode, Mg1Node, Mg2Node], 
				     ?FILE, ?LINE),

    %% Start the MGC and MGs
    i("req_and_rep -> start the MGC"),    
    ET = [{Codec, EC, tcp}, {Codec, EC, udp}],
    {ok, Mgc} = 
	?MGC_START(MgcNode, {deviceName, "ctrl"}, ET, ?MGC_VERBOSITY),

    i("req_and_rep -> start and connect the MGs"),    
    MgConf0 = [{Mg1Node, "mg1", Codec, EC, tcp, ?MG_VERBOSITY},
	       {Mg2Node, "mg2", Codec, EC, udp, ?MG_VERBOSITY}],
    MgConf = connect_mg(MgConf0, []),

    %% Collect the (initial) MGs statistics
    Stats1 = get_mg_stats(MgConf, []),
    d("req_and_rep -> stats for the MGs: ~n~p", [Stats1]),

    %% Collect and check the MGC statistics
    i("req_and_rep -> collect and check the MGC stats"),    
    {ok, MgcStats1} = ?MGC_GET_STATS(Mgc, 1),
    d("req_and_rep  -> stats (1) for Mgc: ~n~p~n", [MgcStats1]),


    sleep(1000),


    %% And apply some load 
    i("req_and_rep -> apply traffic load"),
    ok = apply_load(MgConf),

    %% Await completion of load part and the collect traffic
    i("req_and_rep -> await load completion"),
    ok = await_load_complete(MgConf),


    sleep(1000),


    i("req_and_rep -> collect the MGs statistics"),
    Stats2 = get_mg_stats(MgConf, []),
    d("req_and_rep -> stats for MGs: ~n~p", [Stats2]),

    i("req_and_rep -> collect the MGC statistics"),
    {ok, MgcStats2} = ?MGC_GET_STATS(Mgc, 1),
    d("req_and_rep  -> stats (1) for Mgc: ~n~p~n", [MgcStats2]),


    sleep(1000),


    %% Reset counters
    i("req_and_rep -> reset the MGs statistics"),
    reset_mg_stats(MgConf),
    Stats3 = get_mg_stats(MgConf, []),
    d("req_and_rep -> stats for the MGs: ~n~p", [Stats3]),

    i("req_and_rep -> reset the MGC statistics"),
    reset_mgc_stats(Mgc),
    {ok, MgcStats3} = ?MGC_GET_STATS(Mgc, 1),
    d("req_and_rep  -> stats (1) for Mgc: ~n~p~n", [MgcStats3]),


    sleep(1000),


    %% Tell MGs to stop
    i("req_and_rep -> stop the MGs"),
    stop_mg(MgConf),


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


connect_mg([], Acc) ->
    lists:reverse(Acc);
connect_mg([{Node, Name, Codec, EC, Trans, Verb}|Mg], Acc) ->
    Pid = connect_mg(Node, Name, Codec, EC, Trans, Verb),
    connect_mg(Mg, [{Name, Pid}|Acc]).

connect_mg(Node, Name, Codec, EC, Trans, Verb) ->
    Mid = {deviceName, Name}, 
    {ok, Pid} = ?MG_START(Node, Mid, Codec, EC, Trans, Verb),

    {ok, _} = ?MG_EAR(Pid, true),

    %% Ask the MGs to do a service change
    Res = ?MG_SERV_CHANGE(Pid),
    d("connect_mg -> (~s) service change result: ~p", [Name,Res]),

    Pid.


stop_mg(MGs) ->
    [?MG_STOP(Pid) || {_Name, Pid} <- MGs].


get_mg_stats([], Acc) ->
    lists:reverse(Acc);
get_mg_stats([{Name, Pid}|Mgs], Acc) ->
    {ok, Stats} = ?MG_GET_STATS(Pid),
    d("get_mg_stats -> stats for ~s: ~n~p~n", [Name, Stats]),
    get_mg_stats(Mgs, [{Name, Stats}|Acc]).


apply_load([]) ->
    ok;
apply_load([{_, MG}|MGs]) ->
    d("apply_load -> apply load to ~p", [MG]),
    ?MG_APPLY_LOAD(MG,?LOAD_COUNTER_START),
    apply_load(MGs).


reset_mg_stats([]) ->
    ok;
reset_mg_stats([{Name, Pid}|MGs]) ->
    d("reset_mg_stats -> resetting ~s", [Name]),
    ?MG_RESET_STATS(Pid),
    reset_mg_stats(MGs).

reset_mgc_stats(Mgc) ->
    d("reset_mgc_stats -> resetting ~p", [Mgc]),
    ?MGC_RESET_STATS(Mgc).

    
await_load_complete([]) ->
    ok;
await_load_complete(MGs0) ->
    receive
	{load_complete, Pid} ->
	    d("received load_complete from ~p", [Pid]),
	    MGs1 = lists:keydelete(Pid, 2, MGs0),
	    await_load_complete(lists:delete(Pid, MGs1));
	{'EXIT', Pid, Reason} ->
	    i("exit signal from ~p: ~p", [Pid, Reason]),
	    case lists:keymember(Pid, 2, MGs0) of
		true ->
		    exit({mg_exit, Pid, Reason});
		false ->
		    MGs1 = lists:keydelete(Pid, 2, MGs0),
		    await_load_complete(lists:delete(Pid, MGs1))
	    end;
	Other ->
	    d("received unexpected message: ~n~p", [Other]),
	    await_load_complete(MGs0)
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


