%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

-module(snmp_agent_ms_test).

%% TODO
%% * Test fault-tolerance (kill master etc)
%%

-compile(export_all).

-define(application, snmp).

-include_lib("kernel/include/file.hrl").
-include_lib("common_test/include/ct.hrl").
-include("snmp_test_lib.hrl").
-define(SNMP_USE_V3, true).
-include_lib("snmp/include/snmp_types.hrl").
%% -include_lib("snmp/include/SNMP-COMMUNITY-MIB.hrl").
%% -include_lib("snmp/include/SNMP-VIEW-BASED-ACM-MIB.hrl").
%% -include_lib("snmp/include/SNMP-USER-BASED-SM-MIB.hrl").


-define(klas1, [1,3,6,1,2,1,7]).
-define(klas2, [1,3,6,1,2,1,9]).
-define(klas3, [1,3,6,1,2,1,8,1]).
-define(klas4, [1,3,6,1,2,1,8,4]).
-define(sa, [1,3,6,1,4,1,193,2]).
-define(system, [1,3,6,1,2,1,1]).
-define(snmp, [1,3,6,1,2,1,11]).
-define(snmpTraps, [1,3,6,1,6,3,1,1,5]).
-define(ericsson, [1,3,6,1,4,1,193]).
-define(testTrap, [1,3,6,1,2,1,15,0]).
-define(xDescr, [1,3,6,1,2,1,17,1]).
-define(xDescr2, [1,3,6,1,2,1,17,2]).

-define(active, 1).
-define(notInService, 2).
-define(notReady, 3).
-define(createAndGo, 4).
-define(createAndWait, 5).
-define(destroy, 6).

-define(TRAP_UDP, 5000).

-define(tooBigStr, "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff").

-define(str(X), snmp_pdus:bits_to_str(X)).

-define(break(), begin io:format(user, "break at line ~w: pid: ~p\n",
				 [?LINE, self()]),
		       receive cont -> ok end
		 end).


-import(snmp_test_mgr, [gn/1, g/1, s/1, gb/3]).
-define(v1_2(V1,V2),
	       case get(vsn) of
		   v1 -> V1;
		   _ -> V2
	       end).
		        
-define(v1_2_3(V1,V2,V3),
	       case get(vsn) of
		   v1 -> V1;
		   v2 -> V2;
		   _ -> V3
	       end).

all() -> 
[cases()].

groups() -> 
    [{mib_storage, [],
  [{group, mib_storage_ets}, {group, mib_storage_dets},
   {group, mib_storage_mnesia},
   {group, mib_storage_size_check_ets},
   {group, mib_storage_size_check_dets},
   {group, mib_storage_size_check_mnesia},
   {group, mib_storage_varm_dets},
   {group, mib_storage_varm_mnesia}]},
 {mib_storage_ets, [], mib_storage_ets_cases()},
 {mib_storage_dets, [], mib_storage_dets_cases()},
 {mib_storage_mnesia, [], mib_storage_mnesia_cases()},
 {mib_storage_size_check_ets, [],
  mse_size_check_cases()},
 {mib_storage_size_check_dets, [],
  msd_size_check_cases()},
 {mib_storage_size_check_mnesia, [],
  msm_size_check_cases()},
 {mib_storage_varm_dets, [],
  varm_mib_storage_dets_cases()},
 {mib_storage_varm_mnesia, [],
  varm_mib_storage_mnesia_cases()},
 {test_v1, [], v1_cases()}, {test_v2, [], v2_cases()},
 {test_v1_v2, [], v1_v2_cases()},
 {test_v3, [], v3_cases()},
 {test_multi_threaded, [], mt_cases()},
 {multiple_reqs, [], mul_cases()},
 {multiple_reqs_2, [], mul_cases_2()},
 {v2_inform, [], [v2_inform_i]},
 {v3_security, [],
  [v3_crypto_basic, v3_md5_auth, v3_sha_auth,
   v3_des_priv]},
 {standard_mibs, [],
  [snmp_standard_mib, snmp_community_mib,
   snmp_framework_mib, snmp_target_mib,
   snmp_notification_mib, snmp_view_based_acm_mib]},
 {standard_mibs_2, [],
  [snmpv2_mib_2, snmp_community_mib_2,
   snmp_framework_mib_2, snmp_target_mib_2,
   snmp_notification_mib_2, snmp_view_based_acm_mib_2]},
 {standard_mibs_3, [],
  [snmpv2_mib_3, snmp_framework_mib_3, snmp_mpd_mib_3,
   snmp_target_mib_3, snmp_notification_mib_3,
   snmp_view_based_acm_mib_3, snmp_user_based_sm_mib_3]},
 {reported_bugs, [],
  [otp_1128, otp_1129, otp_1131, otp_1162, otp_1222,
   otp_1298, otp_1331, otp_1338, otp_1342, otp_2776,
   otp_2979, otp_3187, otp_3725]},
 {reported_bugs_2, [],
  [otp_1128_2, otp_1129_2, otp_1131_2, otp_1162_2,
   otp_1222_2, otp_1298_2, otp_1331_2, otp_1338_2,
   otp_1342_2, otp_2776_2, otp_2979_2, otp_3187_2]},
 {reported_bugs_3, [],
  [otp_1128_3, otp_1129_3, otp_1131_3, otp_1162_3,
   otp_1222_3, otp_1298_3, otp_1331_3, otp_1338_3,
   otp_1342_3, otp_2776_3, otp_2979_3, otp_3187_3,
   otp_3542]},
 {tickets, [], [{group, otp_4394}]},
 {otp_4394, [], [otp_4394_test]}].

init_per_group(otp_4394, Config) -> 
	init_otp_4394(Config);
init_per_group(v2_inform, Config) -> 
	init_v2_inform(Config);
init_per_group(multiple_reqs_2, Config) -> 
	init_mul(Config);
init_per_group(multiple_reqs, Config) -> 
	init_mul(Config);
init_per_group(test_multi_threaded, Config) -> 
	init_mt(Config);
init_per_group(test_v3, Config) -> 
	init_v3(Config);
init_per_group(test_v1_v2, Config) -> 
	init_v1_v2(Config);
init_per_group(test_v2, Config) -> 
	init_v2(Config);
init_per_group(test_v1, Config) -> 
	init_v1(Config);
init_per_group(mib_storage_varm_mnesia, Config) -> 
	init_varm_mib_storage_mnesia(Config);
init_per_group(mib_storage_varm_dets, Config) -> 
	init_varm_mib_storage_dets(Config);
init_per_group(mib_storage_size_check_mnesia, Config) -> 
	init_size_check_msm(Config);
init_per_group(mib_storage_size_check_dets, Config) -> 
	init_size_check_msd(Config);
init_per_group(mib_storage_size_check_ets, Config) -> 
	init_size_check_mse(Config);
init_per_group(mib_storage_mnesia, Config) -> 
	init_mib_storage_mnesia(Config);
init_per_group(mib_storage_dets, Config) -> 
	init_mib_storage_dets(Config);
init_per_group(mib_storage_ets, Config) -> 
	init_mib_storage_ets(Config);
init_per_group(_GroupName, Config) ->
	Config.

end_per_group(otp_4394, Config) -> 
	finish_otp_4394(Config);
end_per_group(v2_inform, Config) -> 
	finish_v2_inform(Config);
end_per_group(multiple_reqs_2, Config) -> 
	finish_mul(Config);
end_per_group(multiple_reqs, Config) -> 
	finish_mul(Config);
end_per_group(test_multi_threaded, Config) -> 
	finish_mt(Config);
end_per_group(test_v3, Config) -> 
	finish_v3(Config);
end_per_group(test_v1_v2, Config) -> 
	finish_v1_v2(Config);
end_per_group(test_v2, Config) -> 
	finish_v2(Config);
end_per_group(test_v1, Config) -> 
	finish_v1(Config);
end_per_group(mib_storage_varm_mnesia, Config) -> 
	finish_varm_mib_storage_mnesia(Config);
end_per_group(mib_storage_varm_dets, Config) -> 
	finish_varm_mib_storage_dets(Config);
end_per_group(mib_storage_size_check_mnesia, Config) -> 
	finish_size_check_msm(Config);
end_per_group(mib_storage_size_check_dets, Config) -> 
	finish_size_check_msd(Config);
end_per_group(mib_storage_size_check_ets, Config) -> 
	finish_size_check_mse(Config);
end_per_group(mib_storage_mnesia, Config) -> 
	finish_mib_storage_mnesia(Config);
end_per_group(mib_storage_dets, Config) -> 
	finish_mib_storage_dets(Config);
end_per_group(mib_storage_ets, Config) -> 
	finish_mib_storage_ets(Config);
end_per_group(_GroupName, Config) ->
	Config.


init_per_testcase(_Case, Config) when list(Config) ->
    Dog = ?t:timetrap(?t:minutes(6)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) when list(Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    Config.

cases() -> 
    [
	app_info,
	{group, test_v1}, {group, test_v2},
	{group, test_v1_v2}, {group, test_v3},
	{group, test_multi_threaded}, {group, mib_storage},
	{group, tickets}
    ].


%%%-----------------------------------------------------------------
%%% The test case structure is as follows:
%%%
%%% init_all - starts mnesia, 
%%%      
%%%    init_v1 - starts agent
%%%       simple
%%%       big  - e.g. starts/stops subagent, load/unloads mibs
%%%       init_mul
%%%          mul_get
%%%          mul_set
%%%          <etc>
%%%       finish_mul
%%%       <etc>
%%%    finish_v1
%%%
%%%    init_v2 - starts agent
%%%    finish_v2
%%%      
%%%    init_bilingual - starts agent
%%%    finish_bilingual
%%%      
%%% finish_all
%%%
%%% There is still one problem with these testsuites.  If one test
%%% fails, it may not be possible to run some other cases, as it
%%% may have e.g. created some row or loaded some table, that it
%%% didn't undo (since it failed).
%%%-----------------------------------------------------------------

init_all(Config0) when list(Config0) ->
    ?LOG("init_all -> entry with"
	 "~n   Config0: ~p",[Config0]),

    %% --
    %% Fix config:
    %% 

    DataDir0     = ?config(data_dir, Config0),
    DataDir1     = filename:split(filename:absname(DataDir0)),
    [_|DataDir2] = lists:reverse(DataDir1),
    DataDir3     = filename:join(lists:reverse(DataDir2) ++ [?snmp_test_data]),
    Config1      = lists:keydelete(data_dir, 1, Config0),
    Config       = [{data_dir, DataDir3 ++ "/"}|Config1],

    %% -- 
    %% Start nodes
    %% 

    ?line {ok, SaNode}  = start_node(snmp_sa),
    ?line {ok, MgrNode} = start_node(snmp_mgr),


    %% -- 
    %% Create necessary files
    %% 

    Dir = ?config(priv_dir, Config),
    ?DBG("init_all -> Dir ~p", [Dir]),

    DataDir = ?config(data_dir, Config),
    ?DBG("init_all -> DataDir ~p", [DataDir]),

    file:make_dir(MgrDir = filename:join(Dir, "mgr_dir/")),
    ?DBG("init_all -> MgrDir ~p", [MgrDir]),

    file:make_dir(AgentDir = filename:join(Dir, "agent_dir/")),
    ?DBG("init_all -> AgentDir ~p", [AgentDir]),

    file:make_dir(SaDir = filename:join(Dir, "sa_dir/")),
    ?DBG("init_all -> SaDir ~p", [SaDir]),


    %% -- 
    %% Start and initiate mnesia
    %% 

    ?DBG("init_all -> load application mnesia", []),
    ?line ok = application:load(mnesia),

    ?DBG("init_all -> load application mnesia on node ~p", [SaNode]),
    ?line ok = rpc:call(SaNode, application, load, [mnesia]),
    
    ?DBG("init_all -> application mnesia: set_env dir",[]),
    ?line application_controller:set_env(mnesia, dir, 
					 filename:join(Dir, "Mnesia1")),

    ?DBG("init_all -> application mnesia: set_env dir on node ~p",[SaNode]),
    ?line rpc:call(SaNode, application_controller, set_env,
		   [mnesia, dir,  filename:join(Dir, "Mnesia2")]),

    ?DBG("init_all -> create mnesia schema",[]),
    ?line ok = mnesia:create_schema([SaNode, node()]),
    
    ?DBG("init_all -> start application mnesia",[]),
    ?line ok = application:start(mnesia),

    ?DBG("init_all -> start application mnesia on ~p",[SaNode]),
    ?line ok = rpc:call(SaNode, application, start, [mnesia]),
    Ip = ?LOCALHOST(),
    [{snmp_sa,   SaNode}, 
     {snmp_mgr,  MgrNode}, 
     {agent_dir, AgentDir ++ "/"},
     {mgr_dir,   MgrDir ++ "/"},
     {sa_dir,    SaDir ++ "/"}, 
     {mib_dir,   DataDir}, 
     {ip,        Ip} | 
     Config].

finish_all(Config) when list(Config) ->
    SaNode = ?config(snmp_sa, Config),
    MgrNode = ?config(snmp_mgr, Config),
    stop_node(SaNode),
    stop_node(MgrNode),
    application:stop(mnesia).

start_v1_agent(Config) when list(Config) ->
    start_agent(Config, [v1]).

start_v1_agent(Config,Opts) when list(Config), list(Opts)  ->
    start_agent(Config, [v1], Opts).

start_v2_agent(Config) when list(Config) ->
    start_agent(Config, [v2]).

start_v3_agent(Config) when list(Config) ->
    start_agent(Config, [v3]).

start_bilingual_agent(Config) when list(Config) ->
    start_agent(Config, [v1,v2]).

start_multi_threaded_agent(Config) when list(Config) ->
    start_agent(Config, [v2], [{snmp_multi_threaded, true}]).

stop_agent(Config) when list(Config) ->
    ?LOG("stop_agent -> entry with"
	 "~n   Config: ~p",[Config]),

    {Sup, Par} = ?config(snmp_sup, Config),
    ?DBG("stop_agent -> attempt to stop (sup) ~p"
	"~n   Sup: ~p"
	"~n   Par: ~p",
	[Sup, 
	(catch process_info(Sup)),
	(catch process_info(Par))]),
    stop_sup(Sup, Par),

    {Sup2, Par2} = ?config(snmp_sub, Config),
    ?DBG("stop_agent -> attempt to stop (sub) ~p"
	"~n   Sup2: ~p"
	"~n   Par2: ~p",
	[Sup2,
	(catch process_info(Sup2)),
	(catch process_info(Par2))]),
    stop_sup(Sup2, Par2),

    ?DBG("stop_agent -> done - now cleanup config", []),
    C1 = lists:keydelete(snmp_sup, 1, Config),
    lists:keydelete(snmp_sub, 1, C1).


stop_sup(Pid, _) when node(Pid) == node() ->
    case (catch process_info(Pid)) of
	PI when list(PI) ->
	    ?LOG("stop_sup -> attempt to stop ~p", [Pid]),
	    Ref = erlang:monitor(process, Pid),
	    exit(Pid, kill),
	    await_stopped(Pid, Ref);
	{'EXIT', _Reason} ->
	    ?LOG("stop_sup -> ~p not running", [Pid]),
	    ok
    end;
stop_sup(Pid, _) ->
    ?LOG("stop_sup -> attempt to stop ~p", [Pid]),
    Ref = erlang:monitor(process, Pid),
    ?LOG("stop_sup -> Ref: ~p", [Ref]),
    %% Pid ! {'EXIT', Parent, shutdown}, % usch
    exit(Pid, kill), 
    await_stopped(Pid, Ref).

await_stopped(Pid, Ref) ->
    receive
        {'DOWN', Ref, process, Pid, _Reason} ->
            ?DBG("received down message for ~p", [Pid]),
            ok
    after 10000 ->
	    ?INF("await_stopped -> timeout for ~p",[Pid]),
	    erlang:demonitor(Ref),
	    ?FAIL({failed_stop,Pid})
    end.


start_agent(Config, Vsn) ->
    start_agent(Config, Vsn, []).
start_agent(Config, Vsn, Opts) -> 
    ?LOG("start_agent -> entry (~p) with"
	"~n   Config: ~p"
	"~n   Vsn:    ~p"
	"~n   Opts:   ~p",[node(), Config, Vsn, Opts]),
    
    ?line AgentDir = ?config(agent_dir, Config),
    ?line SaNode   = ?config(snmp_sa,   Config),

    snmp_app_env_init(vsn_init(Vsn) ++ 
                      [{audit_trail_log, read_write_log},
                       {audit_trail_log_dir, AgentDir},
                       {audit_trail_log_size, {10240, 10}},
                       {force_config_reload, false},
                       {snmp_agent_type, master},
                       {snmp_config_dir, AgentDir},
                       {snmp_db_dir, AgentDir},
                       {snmp_local_db_auto_repair, true},
                       {snmp_master_agent_verbosity, trace},
                       {snmp_supervisor_verbosity, trace},
                       {snmp_mibserver_verbosity, trace},
                       {snmp_symbolic_store_verbosity, trace},
                       {snmp_note_store_verbosity, trace},
                       {snmp_net_if_verbosity, trace}],
                      Opts),


    process_flag(trap_exit,true),

    {ok, AppSup} = snmp_app_sup:start_link(),
    unlink(AppSup),
    ?DBG("start_agent -> snmp app supervisor: ~p",[AppSup]),

    ?DBG("start_agent -> start master agent (old style)",[]),
    Sup = case (catch snmpa_app:start(normal)) of
              {ok, S} ->
                  ?DBG("start_agent -> started, Sup: ~p",[S]),
                  S;

              Else ->
		  ?DBG("start_agent -> unknown result: ~n~p",[Else]),
		  %% Get info about the apps we depend on
		  MnesiaInfo = mnesia_running(),
		  ?FAIL({start_failed,Else,MnesiaInfo})
          end,

    ?DBG("start_agent -> unlink from supervisor",[]),
    ?line unlink(Sup),
    ?line SaDir = ?config(sa_dir, Config),
    ?DBG("start_agent -> (rpc) start sub on ~p",[SaNode]),
    ?line {ok, Sub} = rpc:call(SaNode, ?MODULE, start_sub, [SaDir]),
    ?DBG("start_agent -> done",[]),
    ?line [{snmp_sup, {Sup, self()}}, {snmp_sub, Sub} | Config].


vsn_init(Vsn) ->
    vsn_init([v1,v2,v3], Vsn, []).

vsn_init([], _Vsn, Acc) ->
    Acc;
vsn_init([V|Vsns], Vsn, Acc) ->
    case lists:member(V, Vsn) of
        true ->
            vsn_init(Vsns, Vsn, [{V, true}|Acc]);
        false ->
            vsn_init(Vsns, Vsn, [{V, false}|Acc])
    end.

snmp_app_env_init(Env0, Opts) ->
    ?DBG("snmp_app_env_init -> unload snmp",[]),
    ?line application:unload(snmp),
    ?DBG("snmp_app_env_init -> load snmp",[]),
    ?line application:load(snmp),
    ?DBG("snmp_app_env_init -> initiate (snmp) application env",[]),
    F1 = fun({Key,Val} = New, Acc0) -> 
                 ?DBG("snmp_app_env_init -> "
                     "updating setting ~p to ~p", [Key, Val]),
                 case lists:keyreplace(Key, 1, Acc0, New) of
		     Acc0 ->
			 [New|Acc0];
		     Acc ->
			 Acc
		 end
         end, 
    Env = lists:foldr(F1, Env0, Opts),
    ?DBG("snmp_app_env_init -> Env: ~p",[Env]),
    F2 = fun({Key,Val}) ->
                 ?DBG("snmp_app_env_init -> setting ~p to ~p",[Key, Val]),
                 application_controller:set_env(snmp, Key, Val)
         end,
    lists:foreach(F2, Env).




%% Test if application is running
mnesia_running() -> ?IS_MNESIA_RUNNING().
crypto_running() -> ?IS_CRYPTO_RUNNING().


start_sub(Dir) ->
    ?DBG("start_sub -> entry",[]),
    Opts = [{db_dir, Dir}, 
            {supervisor, [{verbosity, trace}]}],
    %% BMK BMK 
%     {ok, P} = snmp_supervisor:start_sub(Dir),
    {ok, P} = snmpa_supervisor:start_sub_sup(Opts),
    unlink(P),
    {ok, {P, self()}}.

create_tables(SaNode) ->
    ?line {atomic, ok} = mnesia:create_table([{name, friendsTable2},
					      {ram_copies, [SaNode]},
					      {snmp, [{key, integer}]},
					      {attributes, [a1,a2,a3]}]),
    ?line {atomic, ok} = mnesia:create_table([{name, kompissTable2},
					      {ram_copies, [SaNode]},
					      {snmp, [{key, integer}]},
					      {attributes, [a1,a2,a3]}]),
    ?line {atomic, ok} = mnesia:create_table([{name, snmp_variables},
					      {attributes, [a1,a2]}]).

delete_tables() ->
    mnesia:delete_table(friendsTable2),
    mnesia:delete_table(kompissTable2),
    mnesia:delete_table(snmp_variables).

%% Creation is done in runtime!
delete_mib_storage_mnesia_tables() ->
    mnesia:delete_table(snmpa_mib_data),
    mnesia:delete_table(snmpa_mib_tree),
    mnesia:delete_table(snmpa_symbolic_store).

%%-----------------------------------------------------------------
%% A test case is always one of:
%%   - v1 specific case
%%   - v2 specific case
%%   - v1 and v2 case
%% All v1 specific cases are prefixed with v1_, and all v2 with
%% v2_.  E.g. v1_trap/v2_trap.
%%
%% All other cases are shared. However, the testserver uses the name
%% of the case to generate a file for that case.  The same case cannot
%% be used in different configurations in the same suite.  Therefore
%% all these functions exists in two variants, the base function
%% <base>, and a second version <base>_2.  There may be several
%% versions as well, <base>_N.
%%-----------------------------------------------------------------









mib_storage_ets_cases() -> 
[mse_simple, mse_v1_processing, mse_big, mse_big2,
 mse_loop_mib, mse_api, mse_sa_register, mse_v1_trap,
 mse_sa_error, mse_next_across_sa, mse_undo,
 mse_standard_mib, mse_community_mib, mse_framework_mib,
 mse_target_mib, mse_notification_mib,
 mse_view_based_acm_mib, mse_sparse_table, mse_me_of,
 mse_mib_of].

mib_storage_dets_cases() -> 
[msd_simple, msd_v1_processing, msd_big, msd_big2,
 msd_loop_mib, msd_api, msd_sa_register, msd_v1_trap,
 msd_sa_error, msd_next_across_sa, msd_undo,
 msd_standard_mib, msd_community_mib, msd_framework_mib,
 msd_target_mib, msd_notification_mib,
 msd_view_based_acm_mib, msd_sparse_table, msd_me_of,
 msd_mib_of].

mib_storage_mnesia_cases() -> 
[msm_simple, msm_v1_processing, msm_big, msm_big2,
 msm_loop_mib, msm_api, msm_sa_register, msm_v1_trap,
 msm_sa_error, msm_next_across_sa, msm_undo,
 msm_standard_mib, msm_community_mib, msm_framework_mib,
 msm_target_mib, msm_notification_mib,
 msm_view_based_acm_mib, msm_sparse_table, msm_me_of,
 msm_mib_of].

mse_size_check_cases() -> 
[mse_size_check].

msd_size_check_cases() -> 
[msd_size_check].

msm_size_check_cases() -> 
[msm_size_check].

varm_mib_storage_dets_cases() -> 
[msd_varm_mib_start].

varm_mib_storage_mnesia_cases() -> 
[msm_varm_mib_start].

init_mib_storage_ets(Config) when list(Config) ->
    ?LOG("init_mib_storage_ets -> entry", []),
    MibStorage = {snmp_mib_storage,ets},
    init_ms(Config, [MibStorage]).

init_mib_storage_dets(Config) when list(Config) ->
    ?LOG("init_mib_storage_ets -> entry", []),
    ?line AgentDir = ?GCONF(agent_dir, Config),
    MibStorage = {snmp_mib_storage,{dets,AgentDir}},
    init_ms(Config, [MibStorage]).

init_mib_storage_mnesia(Config) when list(Config) ->
    ?LOG("init_mib_storage_ets -> entry", []),
    MibStorage = {snmp_mib_storage,{mnesia,[]}},
    init_ms(Config, [MibStorage]).

init_ms(Config, Opts) when list(Config) ->
    ?LOG("init_mib_storage_ets -> entry", []),
    ?line SaNode   = ?GCONF(snmp_sa, Config),
    ?line create_tables(SaNode),
    ?line AgentDir = ?GCONF(agent_dir, Config),
    ?line MgrDir   = ?GCONF(mgr_dir, Config),
    ?line Ip       = ?GCONF(ip, Config),
    ?line config([v1], MgrDir, AgentDir, tuple_to_list(Ip), tuple_to_list(Ip)),
    MasterAgentVerbosity = {snmp_master_agent_verbosity,   trace},
    MibsVerbosity        = {snmp_mibserver_verbosity,      trace},
    SymStoreVerbosity    = {snmp_symbolic_store_verbosity, trace},
    Opts1 = [MasterAgentVerbosity,MibsVerbosity,SymStoreVerbosity|Opts],
    [{vsn, v1} | start_v1_agent(Config,Opts1)].

init_size_check_mse(Config) when list(Config) ->
    MibStorage = {snmp_mib_storage, ets},
    init_size_check_ms(Config, [MibStorage]).

init_size_check_msd(Config) when list(Config) ->
    AgentDir   = ?GCONF(agent_dir, Config),
    MibStorage = {snmp_mib_storage, {dets, AgentDir}},
    init_size_check_ms(Config, [MibStorage]).

init_size_check_msm(Config) when list(Config) ->
    MibStorage = {snmp_mib_storage, {mnesia,[]}},
    init_size_check_ms(Config, [MibStorage]).

init_size_check_ms(Config, Opts) when list(Config) ->
    SaNode = ?GCONF(snmp_sa, Config),
    %% We are using v3 here, so crypto must be supported or else...
    case ?CRYPTO_START() of
	ok ->
	    case ?CRYPTO_SUPPORT() of
		{no, Reason} ->
		    ?SKIP({unsupported_encryption, Reason});
		yes ->
		    ok
	    end;
	{error, Reason} ->
	    ?SKIP({failed_starting_crypto, Reason})
    end,
    create_tables(SaNode),
    AgentDir = ?GCONF(agent_dir, Config),
    MgrDir = ?GCONF(mgr_dir, Config),
    Ip = ?GCONF(ip, Config),
    ?line ok = 
	config([v3], MgrDir, AgentDir, tuple_to_list(Ip), tuple_to_list(Ip)),
    [{vsn, v3} | start_agent(Config, [v3], Opts)].

init_varm_mib_storage_dets(Config) when list(Config) ->
    ?LOG("init_varm_mib_storage_dets -> entry", []),
    ?line SaNode   = ?GCONF(snmp_sa, Config),
    ?line create_tables(SaNode),
    ?line AgentDir = ?GCONF(agent_dir, Config),
    ?line MgrDir   = ?GCONF(mgr_dir, Config),
    ?line Ip       = ?GCONF(ip, Config),
    ?line config([v1], MgrDir, AgentDir, tuple_to_list(Ip), tuple_to_list(Ip)),
    MibStorage           = {snmp_mib_storage,{dets,AgentDir}},
    MasterAgentVerbosity = {snmp_master_agent_verbosity,   trace},
    MibsVerbosity        = {snmp_mibserver_verbosity,      trace},
    SymStoreVerbosity    = {snmp_symbolic_store_verbosity, trace},
    Opts = [MibStorage,MasterAgentVerbosity,MibsVerbosity,SymStoreVerbosity],
    [{vsn, v1}, {agent_opts,Opts} | Config].

init_varm_mib_storage_mnesia(Config) when list(Config) ->
    ?LOG("init_varm_mib_storage_mnesia -> entry", []),
    ?line SaNode   = ?GCONF(snmp_sa, Config),
    ?line create_tables(SaNode),
    ?line AgentDir = ?GCONF(agent_dir, Config),
    ?line MgrDir   = ?GCONF(mgr_dir, Config),
    ?line Ip       = ?GCONF(ip, Config),
    ?line config([v1], MgrDir, AgentDir, tuple_to_list(Ip), tuple_to_list(Ip)),
    MibStorage           = {snmp_mib_storage,{mnesia,[]}},
    MasterAgentVerbosity = {snmp_master_agent_verbosity,   trace},
    MibsVerbosity        = {snmp_mibserver_verbosity,      trace},
    SymStoreVerbosity    = {snmp_symbolic_store_verbosity, trace},
    Opts = [MibStorage,MasterAgentVerbosity,MibsVerbosity,SymStoreVerbosity],
    [{vsn, v1}, {agent_opts,Opts} | Config].

finish_mib_storage_ets(Config) when list(Config) ->
    ?LOG("finish_mib_storage_ets -> entry", []),
    delete_tables(),
    C1 = stop_agent(Config),
    delete_files(C1),
    C2 = lists:keydelete(vsn, 1, C1),
    lists:keydelete(agent_opts, 1, C2).

finish_mib_storage_dets(Config) when list(Config) ->
    ?LOG("finish_mib_storage_dets -> entry", []),
    delete_tables(),
    C1 = stop_agent(Config),
    delete_files(C1),
    C2 = lists:keydelete(vsn, 1, C1),
    lists:keydelete(agent_opts, 1, C2).

finish_mib_storage_mnesia(Config) when list(Config) ->
    ?LOG("finish_mib_storage_mnesia -> entry", []),
    delete_tables(),
    delete_mib_storage_mnesia_tables(),
    C1 = stop_agent(Config),
    delete_files(C1),
    C2 = lists:keydelete(vsn, 1, C1),
    lists:keydelete(agent_opts, 1, C2).

finish_varm_mib_storage_dets(Config) when list(Config) ->
    ?LOG("finish_varm_mib_storage_dets -> entry", []),
    delete_tables(),
    %% C1 = stop_agent(Config), % In case something went wrong...
    delete_files(Config),
    C2 = lists:keydelete(vsn, 1, Config),
    lists:keydelete(agent_opts, 1, C2).

finish_varm_mib_storage_mnesia(Config) when list(Config) ->
    ?LOG("finish_varm_mib_storage_mnesia -> entry", []),
    delete_tables(),
    delete_mib_storage_mnesia_tables(),
    %% C1 = stop_agent(Config), % In case something went wrong...
    delete_files(Config),
    C2 = lists:keydelete(vsn, 1, Config),
    lists:keydelete(agent_opts, 1, C2).

finish_size_check_mse(Config) when list(Config) ->
    finish_size_check_ms(Config).

finish_size_check_msd(Config) when list(Config) ->
    finish_size_check_ms(Config).

finish_size_check_msm(Config) when list(Config) ->
    finish_size_check_ms(Config).

finish_size_check_ms(Config) when list(Config) ->
    delete_tables(),
    C1 = stop_agent(Config),
    delete_files(C1),
    lists:keydelete(vsn, 1, C1).


%% These are just interface functions to fool the test server
mse_simple(X)         -> simple(X).
mse_v1_processing(X)  -> v1_processing(X).
mse_big(X)            -> big(X).
mse_big2(X)           -> big2(X).
mse_loop_mib(X)       -> loop_mib(X).
mse_api(X)            -> api(X).
mse_sa_register(X)    -> sa_register(X).
mse_v1_trap(X)        -> v1_trap(X).
mse_sa_error(X)       -> sa_error(X).
mse_next_across_sa(X) -> next_across_sa(X).
mse_undo(X)           -> undo(X).
mse_standard_mib(X)   -> snmp_standard_mib(X).
mse_community_mib(X)  -> snmp_community_mib(X).
mse_framework_mib(X)  -> snmp_framework_mib(X).
mse_target_mib(X)         -> snmp_target_mib(X).
mse_notification_mib(X)   -> snmp_notification_mib(X).
mse_view_based_acm_mib(X) -> snmp_view_based_acm_mib(X).
mse_sparse_table(X)   -> sparse_table(X).
mse_me_of(X)          -> ms_me_of(X).
mse_mib_of(X)         -> ms_mib_of(X).

msd_simple(X)         -> simple(X).
msd_v1_processing(X)  -> v1_processing(X).
msd_big(X)            -> big(X).
msd_big2(X)           -> big2(X).
msd_loop_mib(X)       -> loop_mib(X).
msd_api(X)            -> api(X).
msd_sa_register(X)    -> sa_register(X).
msd_v1_trap(X)        -> v1_trap(X).
msd_sa_error(X)       -> sa_error(X).
msd_next_across_sa(X) -> next_across_sa(X).
msd_undo(X)           -> undo(X).
msd_standard_mib(X)   -> snmp_standard_mib(X).
msd_community_mib(X)  -> snmp_community_mib(X).
msd_framework_mib(X)  -> snmp_framework_mib(X).
msd_target_mib(X)         -> snmp_target_mib(X).
msd_notification_mib(X)   -> snmp_notification_mib(X).
msd_view_based_acm_mib(X) -> snmp_view_based_acm_mib(X).
msd_sparse_table(X)   -> sparse_table(X).
msd_me_of(X)          -> ms_me_of(X).
msd_mib_of(X)         -> ms_mib_of(X).

msm_simple(X)         -> simple(X).
msm_v1_processing(X)  -> v1_processing(X).
msm_big(X)            -> big(X).
msm_big2(X)           -> big2(X).
msm_loop_mib(X)       -> loop_mib(X).
msm_api(X)            -> api(X).
msm_sa_register(X)    -> sa_register(X).
msm_v1_trap(X)        -> v1_trap(X).
msm_sa_error(X)       -> sa_error(X).
msm_next_across_sa(X) -> next_across_sa(X).
msm_undo(X)           -> undo(X).
msm_standard_mib(X)   -> snmp_standard_mib(X).
msm_community_mib(X)  -> snmp_community_mib(X).
msm_framework_mib(X)  -> snmp_framework_mib(X).
msm_target_mib(X)         -> snmp_target_mib(X).
msm_notification_mib(X)   -> snmp_notification_mib(X).
msm_view_based_acm_mib(X) -> snmp_view_based_acm_mib(X).
msm_sparse_table(X)       -> sparse_table(X).
msm_me_of(X)          -> ms_me_of(X).
msm_mib_of(X)         -> ms_mib_of(X).


mse_size_check(X)     -> p("mse_size_check..."), ms_size_check(X).
msd_size_check(X)     -> p("msd_size_check..."), ms_size_check(X).
msm_size_check(X)     -> p("msm_size_check..."), ms_size_check(X).

msd_varm_mib_start(X) -> p("msd_varm_mib_start..."), varm_mib_start(X).
msm_varm_mib_start(X) -> p("msm_varm_mib_start..."), varm_mib_start(X).

ms_size_check(suite) -> [];
ms_size_check(Config) when list(Config) ->
    p("ms_size_check..."),
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?LOG("mib server size check...", []),

    ?line load_master("Test2"),
    ?line load_master("TestTrap"),
    ?line load_master("TestTrapv2"),
    ?line load_master_std("OTP-SNMPEA-MIB"),
    ?line load_master_std("SNMP-COMMUNITY-MIB"),
    ?line load_master_std("SNMP-FRAMEWORK-MIB"),
    ?line load_master_std("SNMP-MPD-MIB"),
    ?line load_master_std("SNMP-NOTIFICATION-MIB"),
    ?line load_master_std("SNMP-TARGET-MIB"),
    ?line load_master_std("SNMP-USER-BASED-SM-MIB"),
    ?line load_master_std("SNMP-VIEW-BASED-ACM-MIB"),
    ?line load_master_std("SNMPv2-MIB"),
    ?line load_master_std("SNMPv2-TM"),

    ?SLEEP(2000),

    ?line display_memory_usage(),

    ?line unload_master("OTP-SNMPEA-MIB"),
    ?line unload_master("SNMP-COMMUNITY-MIB"),
    ?line unload_master("SNMP-FRAMEWORK-MIB"),
    ?line unload_master("SNMP-MPD-MIB"),
    ?line unload_master("SNMP-NOTIFICATION-MIB"),
    ?line unload_master("SNMP-TARGET-MIB"),
    ?line unload_master("SNMP-USER-BASED-SM-MIB"),
    ?line unload_master("SNMP-VIEW-BASED-ACM-MIB"),
    ?line unload_master("SNMPv2-MIB"),
    ?line unload_master("SNMPv2-TM"),

    ?line unload_master("TestTrapv2"),
    ?line unload_master("TestTrap"),
    ?line unload_master("Test2"),

    ok.


varm_mib_start(suite) -> [];
varm_mib_start(Config) when list(Config) ->
    p("varm_mib_start..."),
    ?LOG("varm_mib_start -> entry", []),
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),

    %% Start the agent
    Opts    = ?GCONF(agent_opts, Config),
    Config1 = start_v1_agent(Config, Opts),

    %% Sleep some in order for the agent to start properly
    ?DBG("varm_mib_start -> sleep some (before loading mobs)", []),
    ?SLEEP(5000),

    %% Load all the mibs
    HardwiredMibs = loaded_mibs(),
    ?DBG("varm_mib_start -> load all mibs", []),
    ?line load_master_std("SNMP-COMMUNITY-MIB"),
    ?line load_master_std("SNMP-FRAMEWORK-MIB"),
    ?line load_master_std("SNMP-TARGET-MIB"),
    ?line load_master_std("SNMP-NOTIFICATION-MIB"),

    %% Unload the hardwired mibs
    ?DBG("varm_mib_start -> sleep some (before unloading hardwired mibs)", []),
    ?SLEEP(1000),
    ?DBG("varm_mib_start -> unload (hardwired) mibs", []),
    ?line unload_mibs(HardwiredMibs),    %% unload hardwired

    ?DBG("varm_mib_start -> sleep some (before stopping agent)", []),
    ?SLEEP(1000),

    %% Stop the agent (without deleting the stored files)
    ?DBG("varm_mib_start -> stop the agent", []),
    Config2 = stop_agent(Config1),

    %% Sleep some in order for the agent to stop properly
    ?DBG("varm_mib_start -> sleep some (before re-starting the agent)", []),
    ?SLEEP(5000),

    %% Start the agent (again)
    ?DBG("varm_mib_start -> start the agent", []),
    Config3 = start_v1_agent(Config2, Opts),

    ?DBG("varm_mib_start -> sleep some (before starting tests)", []),
    ?SLEEP(5000),

    %% Perform the test(s)
    ?DBG("varm_mib_start -> perform the tests", []),
    try_test(snmp_community_mib),
    try_test(snmp_framework_mib),
    try_test(snmp_target_mib),
    try_test(snmp_notification_mib),

    %% Stop the agent (without deleting the stored files)
    ?DBG("varm_mib_start -> stop the agent", []),
    stop_agent(Config3),
    ok.


-define(snmpTrapCommunity_instance, [1,3,6,1,6,3,18,1,4,0]).
-define(vacmViewSpinLock_instance, [1,3,6,1,6,3,16,1,5,1,0]).
-define(usmStatsNotInTimeWindows_instance, [1,3,6,1,6,3,15,1,1,2,0]).

ms_me_of(suite) -> [];
ms_me_of(Config) when list(Config) ->
    p("ms_me_of..."),
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),

    ?line load_master("Test2"),
    ?line load_master("TestTrap"),
    ?line load_master("TestTrapv2"),
    ?line load_master_std("OTP-SNMPEA-MIB"),
    ?line load_master_std("SNMP-COMMUNITY-MIB"),
    ?line load_master_std("SNMP-FRAMEWORK-MIB"),
    ?line load_master_std("SNMP-MPD-MIB"),
    ?line load_master_std("SNMP-NOTIFICATION-MIB"),
    ?line load_master_std("SNMP-TARGET-MIB"),
    ?line load_master_std("SNMP-VIEW-BASED-ACM-MIB"),

    ?SLEEP(2000),

    ?line display_memory_usage(),


    ?DBG("ms_me_of -> find ~w from SNMP-COMMUNITY-MIB",
	[?snmpTrapCommunity_instance]),
    ?line ok = me_of(?snmpTrapCommunity_instance),
    
    ?DBG("ms_me_of -> find ~w from SNMP-VIEW-BASED-ACM-MIB",
         [?vacmViewSpinLock_instance]),
    ?line ok = me_of(?vacmViewSpinLock_instance),
    
    ?DBG("ms_me_of -> find ~w from SNMP-USER-BASED-SM-MIB",
         [?usmStatsNotInTimeWindows_instance]),
    ?line {error, _} = me_of(?usmStatsNotInTimeWindows_instance),
    

    ?line unload_master("OTP-SNMPEA-MIB"),
    ?line unload_master("SNMP-COMMUNITY-MIB"),
    ?line unload_master("SNMP-FRAMEWORK-MIB"),
    ?line unload_master("SNMP-MPD-MIB"),
    ?line unload_master("SNMP-NOTIFICATION-MIB"),
    ?line unload_master("SNMP-TARGET-MIB"),
    ?line unload_master("SNMP-VIEW-BASED-ACM-MIB"),

    ?line unload_master("TestTrapv2"),
    ?line unload_master("TestTrap"),
    ?line unload_master("Test2"),

    ok.

me_of(Oid) ->
    case snmpa:me_of(Oid) of
	{ok, #me{oid = Oid}} ->
            ok;
	{ok, #me{oid = OtherOid}} ->
            case lists:reverse(Oid) of
                [0|Rest] ->
                    case lists:reverse(Rest) of
                        OtherOid ->
                            ok;
                        AnotherOid ->
                            {error, {invalid_oid, Oid, AnotherOid}}
                    end;
                _ ->
                    {error, {invalid_oid, Oid, OtherOid}}
            end;
	{error, Reason} ->
	    {error, Reason};
	Else ->
	    {error, Else}
    end.


ms_mib_of(suite) -> [];
ms_mib_of(Config) when list(Config) ->
    p("ms_mib_of..."),
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),

    ?line load_master("Test2"),
    ?line load_master("TestTrap"),
    ?line load_master("TestTrapv2"),
    ?line load_master_std("OTP-SNMPEA-MIB"),
    ?line load_master_std("SNMP-COMMUNITY-MIB"),
    ?line load_master_std("SNMP-FRAMEWORK-MIB"),
    ?line load_master_std("SNMP-MPD-MIB"),
    ?line load_master_std("SNMP-NOTIFICATION-MIB"),
    ?line load_master_std("SNMP-TARGET-MIB"),
    ?line load_master_std("SNMP-VIEW-BASED-ACM-MIB"),

    ?SLEEP(2000),

    ?line display_memory_usage(),


    ?DBG("ms_mib_of -> find ~w from SNMP-COMMUNITY-MIB",
	[?snmpTrapCommunity_instance]),
    ?line ok = mib_of(?snmpTrapCommunity_instance, 'SNMP-COMMUNITY-MIB'),
    
    ?DBG("ms_mib_of -> find ~w from SNMP-VIEW-BASED-ACM-MIB",
         [?vacmViewSpinLock_instance]),
    ?line ok = mib_of(?vacmViewSpinLock_instance, 'SNMP-VIEW-BASED-ACM-MIB'),
    
    ?DBG("ms_mib_of -> find ~w from SNMP-USER-BASED-SM-MIB",
         [?usmStatsNotInTimeWindows_instance]),
    ?line {error, _} = mib_of(?usmStatsNotInTimeWindows_instance,
			      'SNMP-USER-BASED-SM-MIB'),
    

    ?line unload_master("OTP-SNMPEA-MIB"),
    ?line unload_master("SNMP-COMMUNITY-MIB"),
    ?line unload_master("SNMP-FRAMEWORK-MIB"),
    ?line unload_master("SNMP-MPD-MIB"),
    ?line unload_master("SNMP-NOTIFICATION-MIB"),
    ?line unload_master("SNMP-TARGET-MIB"),
    ?line unload_master("SNMP-VIEW-BASED-ACM-MIB"),

    ?line unload_master("TestTrapv2"),
    ?line unload_master("TestTrap"),
    ?line unload_master("Test2"),

    ok.

mib_of(Oid, ExpectedMibName) ->
    ?DBG("mib_of -> entry with"
	 "~n   Oid:          ~p"
	 "~n   ExpectedMibName: ~p", [Oid, ExpectedMibName]),
    %% case snmpa:mib_of(Oid) of
    MibOf = snmpa:mib_of(Oid),
    ?DBG("mib_of -> MibOf: ~n~p", [MibOf]),
    case MibOf of
	{ok, ExpectedMibName} ->
            ok;
	{ok, OtherMibName} ->
	    {error, {invalid_mib, ExpectedMibName, OtherMibName}};
	{error, Reason} ->
	    {error, Reason};
	Else ->
	    ?DBG("mib_of -> Else: ~n~p", [Else]),
	    {error, Else}
    end.


app_info(suite) -> [];
app_info(Config) when list(Config) ->
    SnmpDir   = app_dir(snmp),
    SslDir    = app_dir(ssl),
    CryptoDir = app_dir(crypto),
    Attr = snmp:module_info(attributes),
    AppVsn = 
	case lists:keysearch(app_vsn, 1, Attr) of
	    {value, {app_vsn, V}} ->
		V;
	    false ->
		"undefined"
	end,
    io:format("Root dir: ~s~n"
	      "SNMP:   Application dir: ~s~n"
	      "        Application ver: ~s~n"
	      "SSL:    Application dir: ~s~n"
	      "CRYPTO: Application dir: ~s~n", 
	      [code:root_dir(), SnmpDir, AppVsn, SslDir, CryptoDir]),
    ok.

app_dir(App) ->
    case code:lib_dir(App) of
	D when list(D) ->
	    filename:basename(D);
	{error, _Reason} ->
	    "undefined"
    end.



%v1_cases() -> [loop_mib];
v1_cases() -> 
[simple, db_notify_client, v1_processing, big, big2,
 loop_mib, api, subagent, mnesia, {group, multiple_reqs},
 sa_register, v1_trap, sa_error, next_across_sa, undo,
 {group, reported_bugs}, {group, standard_mibs},
 sparse_table, cnt_64, opaque, change_target_addr_config].  

init_v1(Config) when list(Config) ->
    ?line SaNode = ?config(snmp_sa, Config),
    ?line create_tables(SaNode),
    ?line AgentDir = ?config(agent_dir, Config),
    ?line MgrDir = ?config(mgr_dir, Config),
    ?line Ip = ?config(ip, Config),
    ?line config([v1], MgrDir, AgentDir, tuple_to_list(Ip), tuple_to_list(Ip)),
    [{vsn, v1} | start_v1_agent(Config)].

finish_v1(Config) when list(Config) ->
    delete_tables(),
    C1 = stop_agent(Config),
    delete_files(C1),
    lists:keydelete(vsn, 1, C1).


%v2_cases() -> [loop_mib_2];
v2_cases() -> 
[simple_2, v2_processing, big_2, big2_2, loop_mib_2,
 api_2, subagent_2, mnesia_2, {group, multiple_reqs_2},
 sa_register_2, v2_trap, {group, v2_inform}, sa_error_2,
 next_across_sa_2, undo_2, {group, reported_bugs_2},
 {group, standard_mibs_2}, v2_types, implied,
 sparse_table_2, cnt_64_2, opaque_2, v2_caps].

init_v2(Config) when list(Config) ->
    SaNode = ?config(snmp_sa, Config),
    create_tables(SaNode),
    AgentDir = ?config(agent_dir, Config),
    MgrDir = ?config(mgr_dir, Config),
    Ip = ?config(ip, Config),
    config([v2], MgrDir, AgentDir, tuple_to_list(Ip), tuple_to_list(Ip)),
    [{vsn, v2} | start_v2_agent(Config)].

finish_v2(Config) when list(Config) ->
    delete_tables(),
    C1 = stop_agent(Config),
    delete_files(C1),
    lists:keydelete(vsn, 1, C1).


v1_v2_cases() -> 
[simple_bi].

init_v1_v2(Config) when list(Config) ->
    SaNode = ?config(snmp_sa, Config),
    create_tables(SaNode),
    AgentDir = ?config(agent_dir, Config),
    MgrDir = ?config(mgr_dir, Config),
    Ip = ?config(ip, Config),
    config([v1,v2], MgrDir, AgentDir, tuple_to_list(Ip), tuple_to_list(Ip)),
    [{vsn, bilingual} | start_bilingual_agent(Config)].

finish_v1_v2(Config) when list(Config) ->
    delete_tables(),
    C1 = stop_agent(Config),
    delete_files(C1),
    lists:keydelete(vsn, 1, C1).


%v3_cases() -> [loop_mib_3];
v3_cases() -> 
[simple_3, v3_processing, big_3, big2_3, api_3,
 subagent_3, mnesia_3, loop_mib_3, multiple_reqs_3,
 sa_register_3, v3_trap, v3_inform, sa_error_3,
 next_across_sa_3, undo_3, {group, reported_bugs_3},
 {group, standard_mibs_3}, {group, v3_security},
 v2_types_3, implied_3, sparse_table_3, cnt_64_3,
 opaque_3, v2_caps_3].

init_v3(Config) when list(Config) ->
    %% Make sure crypto works, otherwise start_agent will fail
    %% and we will be stuck with a bunch of mnesia tables for
    %% the rest of this suite...
    ?DBG("start_agent -> start crypto app",[]),
    case ?CRYPTO_START() of
	ok ->
	    case ?CRYPTO_SUPPORT() of
		{no, Reason} ->
		    ?SKIP({unsupported_encryption, Reason});
		yes ->
		    ok
	    end;
	{error, Reason} ->
	    ?SKIP({failed_starting_crypto, Reason})
    end,
    SaNode = ?config(snmp_sa, Config),
    create_tables(SaNode),
    AgentDir = ?config(agent_dir, Config),
    MgrDir = ?config(mgr_dir, Config),
    Ip = ?config(ip, Config),
    ?line ok = config([v3], MgrDir, AgentDir, 
		      tuple_to_list(Ip), tuple_to_list(Ip)),
    [{vsn, v3} | start_v3_agent(Config)].

finish_v3(Config) when list(Config) ->
    delete_tables(),
    C1 = stop_agent(Config),
    delete_files(C1),
    lists:keydelete(vsn, 1, C1).


mt_cases() -> 
[multi_threaded, mt_trap].

init_mt(Config) when list(Config) ->
    SaNode = ?config(snmp_sa, Config),
    create_tables(SaNode),
    AgentDir = ?config(agent_dir, Config),
    MgrDir = ?config(mgr_dir, Config),
    Ip = ?config(ip, Config),
    ?line ok = config([v2], MgrDir, AgentDir, tuple_to_list(Ip), tuple_to_list(Ip)),
    [{vsn, v2} | start_multi_threaded_agent(Config)].

finish_mt(Config) when list(Config) ->
    delete_tables(),
    C1 = stop_agent(Config),
    delete_files(C1),
    lists:keydelete(vsn, 1, C1).

%% This one *must* be run first in each case.
init_case(Config) when list(Config) ->
    ?DBG("init_case -> entry with"
	   "~n   Config: ~p", [Config]),
    SaNode = ?config(snmp_sa, Config),
    MgrNode = ?config(snmp_mgr, Config),
    MasterNode = node(),

    SaHost     = ?HOSTNAME(SaNode),
    MgrHost    = ?HOSTNAME(MgrNode),
    MasterHost = ?HOSTNAME(MasterNode),
    {ok, MasterIP} = snmp_misc:ip(MasterHost),
    {ok, MIP}      = snmp_misc:ip(MgrHost),
    {ok, SIP}      = snmp_misc:ip(SaHost),


    put(mgr_node,    MgrNode),
    put(sa_node,     SaNode),
    put(master_node, MasterNode),
    put(sa_host,     SaHost),
    put(mgr_host,    MgrHost),
    put(master_host, MasterHost),
    put(mip,         tuple_to_list(MIP)),
    put(masterip ,   tuple_to_list(MasterIP)),
    put(sip,         tuple_to_list(SIP)),
    
    MibDir = ?config(mib_dir, Config),
    put(mib_dir, MibDir),
    StdM = filename:join(code:priv_dir(snmp), "mibs") ++ "/",
    put(std_mib_dir, StdM),

    MgrDir = ?config(mgr_dir, Config),
    put(mgr_dir, MgrDir),

    put(vsn, ?config(vsn, Config)),
    ?DBG("init_case -> exit with"
	"~n   MasterNode: ~p"
	"~n   SaNode:     ~p"
	"~n   MgrNode:    ~p"
	"~n   MibDir:     ~p", [MasterNode, SaNode, MgrNode, MibDir]),
    {SaNode, MgrNode, MibDir}.

load_master(Mib) ->
    ?DBG("load_master -> entry with"
	"~n   Mib: ~p", [Mib]),
    snmpa:unload_mibs(snmp_master_agent, [Mib]),	% Unload for safety
    ok = snmpa:load_mibs(snmp_master_agent, [get(mib_dir) ++ Mib]).

load_master_std(Mib) ->
    ?DBG("load_master_std -> entry with"
	"~n   Mib: ~p", [Mib]),
    snmpa:unload_mibs(snmp_master_agent, [Mib]),	% Unload for safety
    ok = snmpa:load_mibs(snmp_master_agent, [get(std_mib_dir) ++ Mib]).

unload_master(Mib) ->
    ?DBG("unload_master -> entry with"
	"~n   Mib: ~p", [Mib]),
    ok = snmpa:unload_mibs(snmp_master_agent, [Mib]).

loaded_mibs() ->
    ?DBG("loaded_mibs -> entry",[]),
    Info = snmpa:info(snmp_master_agent),
    {value, {loaded_mibs, Mibs}} = lists:keysearch(loaded_mibs, 1, Info),
    [atom_to_list(Mib) || {Mib,_,_} <- Mibs].

unload_mibs(Mibs) ->
    ?DBG("unload_mibs -> entry with"
	"~n   Mibs: ~p", [Mibs]),
    ok = snmpa:unload_mibs(snmp_master_agent, Mibs).

start_subagent(SaNode, RegTree, Mib) ->
    ?DBG("start_subagent -> entry with"
	"~n   SaNode:  ~p"
	"~n   RegTree: ~p"
	"~n   Mib:     ~p", [SaNode, RegTree, Mib]),
    MA = whereis(snmp_master_agent),
    ?DBG("start_subagent -> MA: ~p", [MA]),
    MibDir = get(mib_dir),
    Mib1   = join(MibDir,Mib),
    %% BMK BMK 
%     case rpc:call(SaNode,snmp_supervisor,start_subagent,[MA,RegTree,[Mib1]]) of
    case rpc:call(SaNode, snmpa_supervisor, 
		  start_sub_agent, [MA, RegTree, [Mib1]]) of
	{ok, SA} ->
	    ?DBG("start_subagent -> SA: ~p", [SA]),
	    {ok, SA};
	Error ->
	    ?FAIL({subagent_start_failed, SaNode, Error, [MA, RegTree, Mib1]})
    end.

stop_subagent(SA) ->
    ?DBG("stop_subagent -> entry with"
	"~n   SA: ~p", [SA]),
    %% BNK BMK 
    %% rpc:call(node(SA), snmp_supervisor, stop_subagent, [SA]).
    rpc:call(node(SA), snmpa_supervisor, stop_sub_agent, [SA]).

%%-----------------------------------------------------------------
%% This function takes care of the old OTP-SNMPEA-MIB.
%% Unfortunately, the testcases were written to use the data in the
%% internal tables, and these table are now obsolete and not used
%% by the agent.  Therefore, we emulate them by using
%% OLD-SNMPEA-MIB, which uses the default impl. of all tables.
%%
%% These two rows must exist in intCommunityTable
%%    {[147,214,36,45], "public", 2, readWrite}.
%%    {[147,214,36,45], "standard trap", 2, read}.
%% (But with the manager's IP address)
%%
%%-----------------------------------------------------------------
init_old() ->
    snmpa_local_db:table_create_row(intCommunityTable,
				   get(mip) ++ [6 | "public"],
				   {get(mip), "public", 2, 2}),
    snmpa_local_db:table_create_row(intCommunityTable,
				   get(mip) ++ [13 | "standard trap"],
				   {get(mip), "standard trap", 2, 1}),
    snmpa_local_db:variable_set(intAgentIpAddress, [127,0,0,1]).
    
				    

simple(suite) -> [];
simple(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    
    try_test(simple_standard_test).

simple_2(X) -> simple(X).

simple_bi(suite) -> [];
simple_bi(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),

    put(vsn, v1), % First, try v1 manager
    try_test(simple_standard_test),
    
    put(vsn, v2), % Then, try v2 manager
    try_test(simple_standard_test).
    
simple_3(X) ->
    simple(X).

big(suite) -> [];
big(Config) when list(Config) ->
    ?DBG("big -> entry", []),
    {SaNode, _MgrNode, _MibDir} = init_case(Config),

    p("Starting subagent..."),
    ?line pong = net_adm:ping(SaNode),
    
    ?line {ok, SA} = start_subagent(SaNode, ?klas1, "Klas1"),
    ?DBG("big -> SA: ~p", [SA]),
    ?line load_master("OLD-SNMPEA-MIB"),
    ?line init_old(),
    try_test(big_test),
    ?line stop_subagent(SA),
    ?line unload_master("OLD-SNMPEA-MIB").

big_2(X) -> big(X).

big_3(X) -> big(X).

     
big2(suite) -> [];
big2(Config) when list(Config) ->
    %% This is exactly the same tests as 'big', but with the
    %% v2 equivalent of the mibs.
    {SaNode, _MgrNode, _MibDir} = init_case(Config),

    p("Starting subagent..."),
    ?line pong = net_adm:ping(SaNode),
    
    ?line {ok, SA} = start_subagent(SaNode, ?klas1, "Klas1-v2"),
    ?line load_master("OLD-SNMPEA-MIB-v2"),
    ?line init_old(),
    try_test(big_test),
    ?line stop_subagent(SA),
    ?line unload_master("OLD-SNMPEA-MIB-v2").

big2_2(X) -> big2(X).

big2_3(X) -> big2(X).
    

multi_threaded(suite) -> [];
multi_threaded(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    
    ?line load_master("Test1"),
    try_test(multi_threaded_test),
    ?line unload_master("Test1").

mt_trap(suite) -> [];
mt_trap(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    MA = whereis(snmp_master_agent),
    
    ?line load_master("Test1"),
    ?line load_master("TestTrapv2"),
    try_test(mt_trap_test, [MA]),
    ?line unload_master("TestTrapv2"),
    ?line unload_master("Test1").

v2_types(suite) -> [];
v2_types(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),

    ?line load_master("Test1"),
    try_test(types_v2_test),
    ?line unload_master("Test1").

v2_types_3(X) -> v2_types(X).
    

implied(suite) -> [];
implied(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    MA = whereis(snmp_master_agent),

    ?line load_master("Test1"),
    try_test(implied_test,[MA]),
    ?line unload_master("Test1").

implied_3(X) -> implied(X).
    

sparse_table(suite) -> [];
sparse_table(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),

    ?line load_master("Test1"),
    try_test(sparse_table_test),
    ?line unload_master("Test1").

sparse_table_2(X) -> sparse_table(X).

sparse_table_3(X) -> sparse_table(X).

cnt_64(suite) -> [];
cnt_64(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    MA = whereis(snmp_master_agent),

    ?line load_master("Test1"),
    try_test(cnt_64_test, [MA]),
    ?line unload_master("Test1").

cnt_64_2(X) -> cnt_64(X).

cnt_64_3(X) -> cnt_64(X).

opaque(suite) -> [];
opaque(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),

    ?line load_master("Test1"),
    try_test(opaque_test),
    ?line unload_master("Test1").

opaque_2(X) -> opaque(X).

opaque_3(X) -> opaque(X).


change_target_addr_config(suite) -> [];
change_target_addr_config(Config) when list(Config) ->
    p("Testing changing target address config..."),
    ?LOG("change_target_addr_config -> entry",[]),
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),

    put(sname,snmp_suite),
    put(verbosity,trace),

    MA = whereis(snmp_master_agent),

    ?LOG("change_target_addr_config -> load TestTrap",[]),
    ?line load_master("TestTrap"),

    ?LOG("change_target_addr_config -> set trace verbosity for local_db",[]),
    ?line snmpa:verbosity(local_db,trace),

    %% First send some traps that will arive att the original manager
    ?LOG("change_target_addr_config -> send trap",[]),
    try_test(ma_trap1, [MA]),

    ?LOG("change_target_addr_config -> set silence verbosity for local_db",[]),
    ?line snmpa:verbosity(local_db,silence),

    %% Start new dummy listener
    ?LOG("change_target_addr_config -> start dummy manager",[]),
    ?line {ok,Pid,NewPort} = dummy_manager_start(MA),
    
    %% Reconfigure
    ?LOG("change_target_addr_config -> reconfigure",[]),
    AgentDir = ?config(agent_dir, Config),
    ?line rewrite_target_addr_conf(AgentDir, NewPort),
    ?line snmp_target_mib:reconfigure(AgentDir),

    %% Send the trap again
    ?LOG("change_target_addr_config -> send trap again",[]),
    catch dummy_manager_send_trap2(Pid),

    ?LOG("change_target_addr_config -> await trap ack",[]),
    catch dummy_manager_await_trap2_ack(),

    ?LOG("change_target_addr_config -> stop dummy manager",[]),
    ?line ok = dummy_manager_stop(Pid),

    ?LOG("change_target_addr_config -> reset target address config",[]),
    ?line reset_target_addr_conf(AgentDir),

    ?LOG("change_target_addr_config -> unload TestTrap",[]),
    ?line unload_master("TestTrap").


dummy_manager_start(MA) ->
    ?DBG("dummy_manager_start -> entry",[]),
    Pid = spawn(get(mgr_node), ?MODULE,dummy_manager_init,[self(),MA]),
    ?DBG("dummy_manager_start -> Pid: ~p",[Pid]),
    await_dummy_manager_started(Pid).

await_dummy_manager_started(Pid) ->
    receive
	{dummy_manager_started,Pid,Port} ->
	    ?DBG("dummy_manager_start -> acknowledge received with"
		"~n   Port: ~p",[Port]),
	    {ok,Pid,Port};
	{'EXIT', Pid, Reason} ->
	    {error, Pid, Reason};
	O ->
	    ?LOG("dummy_manager_start -> received unknown message:"
		 "~n   ~p",[O]),
	    await_dummy_manager_started(Pid)
    end.

dummy_manager_stop(Pid) ->
    ?DBG("dummy_manager_stop -> entry with Pid: ~p",[Pid]),
    Pid ! stop,
    receive
	{dummy_manager_stopping, Pid} -> 
	    ?DBG("dummy_manager_stop -> acknowledge received",[]),
	    ok
    after 10000 ->
	    ?ERR("dummy_manager_stop -> timeout",[]),
	    timeout
    end.

dummy_manager_send_trap2(Pid) ->
    ?DBG("dummy_manager_send_trap2 -> entry",[]),
    Pid ! {send_trap,testTrap2}.

dummy_manager_await_trap2_ack() ->
    ?DBG("dummy_manager_await_trap2 -> entry",[]),
    receive
	{received_trap,Trap} ->
	    ?LOG("dummy_manager_await_trap2 -> received trap: ~p",[Trap]),
	    %% Note: 
	    %% Without this sleep the v2_inform_i testcase failes! There
	    %% is no relation between these two test cases as far as I
	    %% able to figure out...
	    sleep(60000),
	    ok;
	O ->
	    ?ERR("dummy_manager_await_trap2 -> unexpected message: ~p",[O]),
	    ok
    after 10000 ->
	    ?ERR("dummy_manager_await_trap2 -> timeout",[]),
	    timeout
    end.

dummy_manager_init(Parent,MA) ->
    ?DBG("dummy_manager_init -> entry with"
	   "~n   Parent: ~p"
	   "~n   MA:     ~p",[Parent,MA]),
    {ok,S} = gen_udp:open(0,[{recbuf,65535}]),
    ?DBG("dummy_manager_init -> S: ~p",[S]),
    {ok,Port} = inet:port(S),
    ?DBG("dummy_manager_init -> Port: ~p",[Port]),
    Parent ! {dummy_manager_started,self(),Port},
    dummy_manager_loop(Parent,S,MA).

dummy_manager_loop(P,S,MA) ->
    ?LOG("dummy_manager_loop -> ready for receive",[]),
    receive
	{send_trap,Trap} ->
	    ?LOG("dummy_manager_loop -> received trap send request"
		 "~n   Trap: ~p",[Trap]),
	    snmpa:send_trap(MA, Trap, "standard trap"),
	    dummy_manager_loop(P,S,MA);
	{udp, _UdpId, Ip, UdpPort, Bytes} ->
	    ?LOG("dummy_manager_loop -> received upd message"
		 "~n   from: ~p:~p"
		 "~n   size: ~p",
		 [Ip, UdpPort, dummy_manager_message_sz(Bytes)]),
	    R = dummy_manager_handle_message(Bytes),
	    ?DBG("dummy_manager_loop -> R: ~p",[R]),
	    P ! R,
	    dummy_manager_loop(P,S,MA);
	stop ->
	    ?DBG("dummy_manager_loop -> received stop request",[]),
	    P ! {dummy_manager_stopping, self()},
	    gen_udp:close(S),
	    exit(normal);
	O ->
	    ?LOG("dummy_manager_loop -> received unknown message:"
		 "~n   ~p",[O]),
	    dummy_manager_loop(P,S,MA)
    end.

dummy_manager_message_sz(B) when binary(B) ->
    size(B);
dummy_manager_message_sz(L) when list(L) ->
    length(L);
dummy_manager_message_sz(_) ->
    undefined.

dummy_manager_handle_message(Bytes) ->
    case (catch snmp_pdus:dec_message(Bytes)) of
	{'EXIT',Reason} ->
	    ?ERR("dummy_manager_handle_message -> "
		   "failed decoding message only:~n   ~p",[Reason]),
	    {error,Reason};
	M ->
	    ?DBG("dummy_manager_handle_message -> decoded message:"
		   "~n   ~p",[M]),
	    {received_trap,M}
    end.


api(suite) -> [];
api(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),

    ?line load_master("OLD-SNMPEA-MIB"),
    ?line init_old(),
    try_test(api_test, [node()]),
    ?line unload_master("OLD-SNMPEA-MIB").

api_2(X) -> api(X).

api_3(X) -> api(X).


subagent(suite) -> [];
subagent(Config) when list(Config) ->
    {SaNode, _MgrNode, MibDir} = init_case(Config),

    ?line {ok, SA} = start_subagent(SaNode, ?klas1, "Klas1"),
    try_test(load_test_sa),
    
    p("Testing unregister subagent..."),
    MA = whereis(snmp_master_agent),
    rpc:call(SaNode, snmp, unregister_subagent, [MA, SA]),
    try_test(unreg_test),

    p("Loading previous subagent mib in master and testing..."),
    ?line ok = snmpa:load_mibs(MA, [MibDir ++ "Klas1"]),
    try_test(load_test),

    p("Unloading previous subagent mib in master and testing..."),
    ?line ok = snmpa:unload_mibs(MA, [MibDir ++ "Klas1"]),
    try_test(unreg_test),
    p("Testing register subagent..."),
    rpc:call(SaNode, snmp, register_subagent,
	     [MA, ?klas1, SA]),
    try_test(load_test_sa),

    ?line stop_subagent(SA),
    try_test(unreg_test).
    
subagent_2(X) -> subagent(X).

subagent_3(X) -> subagent(X).


mnesia(suite) -> [];
mnesia(Config) when list(Config) ->
    {SaNode, _MgrNode, _MibDir} = init_case(Config),

    p("Starting subagent with mnesia impl..."),
    {ok, SA} = start_subagent(SaNode, ?klas2, "Klas2"),
    ?line load_master("OLD-SNMPEA-MIB"),
    ?line init_old(),

    try_test(big_test_2),

    p("Testing unregister subagent..."),
    MA = whereis(snmp_master_agent),
    rpc:call(SaNode, snmp, unregister_subagent, [MA, SA]),
    try_test(unreg_test),
    ?line unload_master("OLD-SNMPEA-MIB"),
    ?line stop_subagent(SA).

mnesia_2(X) -> mnesia(X).

mnesia_3(X) -> mnesia(X).



mul_cases() -> 
[mul_get, mul_get_err, mul_next, mul_next_err,
 mul_set_err].
    

multiple_reqs_3(_X) -> 
    {req, [], {conf, init_mul, mul_cases_3(), finish_mul}}.


mul_cases_2() -> 
[mul_get_2, mul_get_err_2, mul_next_2, mul_next_err_2,
 mul_set_err_2].
    

mul_cases_3() ->
    [mul_get_3, mul_get_err_3, mul_next_3, mul_next_err_3, mul_set_err_3].
    

init_mul(Config) when list(Config) ->
    {SaNode, _MgrNode, _MibDir} = init_case(Config),

    ?line {ok, SA} = start_subagent(SaNode, ?klas1, "Klas1"),
    ?line load_master("OLD-SNMPEA-MIB"),
    ?line init_old(),
    [{mul_sub, SA} | Config].

finish_mul(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    
    SA = ?config(mul_sub, Config),
    
    ?line unload_master("OLD-SNMPEA-MIB"),
    ?line stop_subagent(SA),
    lists:keydelete(mul_sub, 1, Config).
    
mul_get(suite) -> [];
mul_get(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    
    p("Testing multiple get..."),
    try_test(do_mul_get).

mul_get_2(X) -> mul_get(X).

mul_get_3(X) -> mul_get(X).

	     
mul_get_err(suite) -> [];
mul_get_err(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    
    p("Testing multiple get with error..."),
    try_test(do_mul_get_err).

mul_get_err_2(X) -> mul_get_err(X).

mul_get_err_3(X) -> mul_get_err(X).

	     
mul_next(suite) -> [];
mul_next(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    
    p("Testing multiple next..."),
    try_test(do_mul_next).

mul_next_2(X) -> mul_next(X).

mul_next_3(X) -> mul_next(X).

	     
mul_next_err(suite) -> [];
mul_next_err(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    
    p("Testing multiple next..."),
    try_test(do_mul_next_err).

mul_next_err_2(X) -> mul_next_err(X).

mul_next_err_3(X) -> mul_next_err(X).

	     
mul_set(suite) -> [];
mul_set(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    
    p("Testing multiple set..."),
    try_test(do_mul_set).

mul_set_2(X) -> mul_set(X).

mul_set_3(X) -> mul_set(X).

	     
mul_set_err(suite) -> [];
mul_set_err(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    
    p("Testing multiple set with error..."),
    try_test(do_mul_set_err).

mul_set_err_2(X) -> mul_set_err(X).

mul_set_err_3(X) -> mul_set_err(X).


sa_register(suite) -> [];
sa_register(Config) when list(Config) ->
    ?DBG("sa_register -> entry", []),
    {SaNode, _MgrNode, MibDir} = init_case(Config),

    ?DBG("sa_register -> start subagent", []),
    ?line {ok, SA} = start_subagent(SaNode, ?klas1, "Klas1"),

    ?DBG("sa_register -> unregister subagent", []),
    p("Testing unregister subagent (2)..."),
    MA = whereis(snmp_master_agent),
    rpc:call(SaNode, snmp, unregister_subagent, [MA, ?klas1]),
    try_test(unreg_test),

    p("Loading SA-MIB..."),
    ?DBG("sa_register -> unload mibs", []),
    snmpa:unload_mibs(SA, [MibDir ++ "Klas1"]),
    ?DBG("sa_register -> unload mibs", []),
    snmpa:load_mibs(SA, [MibDir ++ "SA-MIB"]),
    ?DBG("sa_register -> register subagent", []),
    rpc:call(SaNode, snmp, register_subagent, [MA,?sa,SA]),
    try_test(sa_mib),

    ?DBG("sa_register -> stop subagent", []),
    ?line stop_subagent(SA).
    
sa_register_2(X) -> sa_register(X).

sa_register_3(X) -> sa_register(X).


v1_trap(suite) -> [];
v1_trap(Config) when list(Config) ->
    {SaNode, _MgrNode, _MibDir} = init_case(Config),

    ?line {ok, SA} = start_subagent(SaNode, ?sa, "SA-MIB"),

    p("Testing trap sending from master agent..."),
    MA = whereis(snmp_master_agent),

    ?line load_master("TestTrap"),
    ?line load_master("TestTrapv2"),

    try_test(ma_trap1, [MA]),
    try_test(ma_trap2, [MA]),
    try_test(ma_v2_2_v1_trap, [MA]),
    try_test(ma_v2_2_v1_trap2, [MA]),

    p("Testing trap sending from subagent..."),
    try_test(sa_trap1, [SA]),
    try_test(sa_trap2, [SA]),
    try_test(sa_trap3, [SA]),
    
    ?line unload_master("TestTrap"),
    ?line unload_master("TestTrapv2"),

    ?line stop_subagent(SA).

v2_trap(suite) -> [];
v2_trap(Config) when list(Config) ->
    {SaNode, _MgrNode, _MibDir} = init_case(Config),

    ?line {ok, SA} = start_subagent(SaNode, ?sa, "SA-MIB"),

    p("Testing trap sending from master agent..."),
    MA = whereis(snmp_master_agent),

    ?line load_master("TestTrap"),
    ?line load_master("TestTrapv2"),

    
    try_test(ma_v2_trap1, [MA]),
    try_test(ma_v2_trap2, [MA]),
    try_test(ma_v1_2_v2_trap, [MA]),
    try_test(ma_v1_2_v2_trap2, [MA]),

    try_test(sa_mib),
    p("Testing trap sending from subagent..."),
    try_test(sa_v1_2_v2_trap1, [SA]),
    try_test(sa_v1_2_v2_trap2, [SA]),
    try_test(sa_v1_2_v2_trap3, [SA]),
    
    ?line unload_master("TestTrap"),
    ?line unload_master("TestTrapv2"),

    ?line stop_subagent(SA).

v3_trap(X) ->
    v2_trap(X).


v3_inform(_X) ->
    %% v2_inform(X).
    {req, [], {conf, init_v3_inform, [v3_inform_i], finish_v3_inform}}. 

init_v2_inform(Config) when list(Config) ->
    _Dir = ?config(agent_dir, Config),
%    snmp_internal_mib:configure(Dir),
    Config.

init_v3_inform(X) ->
    init_v2_inform(X).

finish_v2_inform(Config) when list(Config) ->
    _Dir = ?config(agent_dir, Config),
%   snmp_internal_mib:configure(Dir),
    Config.

finish_v3_inform(X) ->
    finish_v2_inform(X).



v2_inform_i(suite) -> [];
v2_inform_i(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),

    p("Testing inform sending from master agent...  NOTE! This test\ntakes a "
      "few minutes (5) to complete."),
    MA = whereis(snmp_master_agent),

    ?line load_master("TestTrap"),
    ?line load_master("TestTrapv2"),

    try_test(ma_v2_inform1, [MA]),

    ?line unload_master("TestTrap"),
    ?line unload_master("TestTrapv2").

v3_inform_i(X) -> v2_inform_i(X).


sa_error(suite) -> [];
sa_error(Config) when list(Config) ->
    {SaNode, _MgrNode, _MibDir} = init_case(Config),

    ?line load_master("OLD-SNMPEA-MIB"),
    ?line init_old(),
    ?line {ok, SA} = start_subagent(SaNode, ?sa, "SA-MIB"),

    p("Testing sa bad value (is_set_ok)..."),
    try_test(sa_errs_bad_value),

    p("Testing sa gen err (set)..."),
    try_test(sa_errs_gen_err),

    p("Testing too big..."),
    try_test(sa_too_big),

    ?line unload_master("OLD-SNMPEA-MIB"),
    stop_subagent(SA).

sa_error_2(X) -> sa_error(X).

sa_error_3(X) -> sa_error(X).


next_across_sa(suite) -> [];
next_across_sa(Config) when list(Config) ->
    {SaNode, _MgrNode, MibDir} = init_case(Config),
    MA = whereis(snmp_master_agent),

    ?line {ok, SA} = start_subagent(SaNode, ?sa, "SA-MIB"),

    p("Loading another subagent mib..."),
    ?line ok = snmpa:load_mibs(SA, [MibDir ++ "Klas1"]),

    rpc:call(SaNode, snmp, register_subagent, [MA, ?klas1, SA]),
    try_test(load_test_sa),
    
    p("Testing next across subagent (endOfMibView from SA)..."),
    try_test(next_across_sa),

    p("Unloading mib"),
    snmpa:unload_mibs(SA, [MibDir ++ "Klas1"]),
    rpc:call(SaNode, snmp, unregister_subagent, [MA, ?klas1]),
    try_test(unreg_test),

    p("Starting another subagent"),
    ?line {ok, SA2} = start_subagent(SaNode, ?klas1, "Klas1"),
    p("Testing next across subagent (wrong prefix from SA)..."),
    try_test(next_across_sa),
    
    stop_subagent(SA),
    stop_subagent(SA2).

next_across_sa_2(X) -> next_across_sa(X).

next_across_sa_3(X) -> next_across_sa(X).


undo(suite) -> [];
undo(Config) when list(Config) ->
    {SaNode, _MgrNode, MibDir} = init_case(Config),
    MA = whereis(snmp_master_agent),

    ?line {ok, SA} = start_subagent(SaNode, ?sa, "SA-MIB"),

    p("Testing undo phase at master agent..."),
    ?line ok = snmpa:load_mibs(MA, [MibDir ++ "Klas3"]),
    ?line ok = snmpa:load_mibs(MA, [MibDir ++ "Klas4"]),
    try_test(undo_test),
    try_test(api_test2),
    ?line ok = snmpa:unload_mibs(MA, [MibDir ++ "Klas3"]),

    p("Testing bad return values from instrum. funcs..."),
    try_test(bad_return),

    ?line ok = snmpa:unload_mibs(MA, [MibDir ++ "Klas4"]),

    p("Testing undo phase at subagent..."),
    ?line ok = snmpa:load_mibs(SA, [MibDir ++ "Klas3"]),
    ?line ok = snmpa:load_mibs(SA, [MibDir ++ "Klas4"]),
    ?line ok = snmpa:register_subagent(MA, ?klas3, SA),
    ?line ok = snmpa:register_subagent(MA, ?klas4, SA),
    try_test(undo_test),
    try_test(api_test3),

    p("Testing undo phase across master/subagents..."),
    try_test(undo_test),
    try_test(api_test3),
    stop_subagent(SA).

undo_2(X) -> undo(X).

undo_3(X) -> undo(X).

%% Req. Test2
v1_processing(suite) -> [];
v1_processing(Config) when list(Config) ->
    ?DBG("v1_processing -> entry", []),
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),

    ?line load_master("Test2"),
    try_test(v1_proc),
    ?line unload_master("Test2").

%% Req. Test2
v2_processing(suite) -> [];
v2_processing(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),

    ?line load_master("Test2"),
    try_test(v2_proc),
    ?line unload_master("Test2").

%% Req. Test2
v3_processing(suite) -> [];
v3_processing(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),

    ?line load_master("Test2"),
    try_test(v2_proc), % same as v2!
    ?line unload_master("Test2").


%% We'll try get/set/trap and inform for all the auth & priv protocols.
%% For informs, the mgr is auth-engine. The agent has to sync.  This is
%% accomplished by the first inform sent.  That one will generate a
%% report, which makes it in sync.  The notification-generating
%% application times out, and send again.  This time it'll work.

v3_crypto_basic(suite) -> [];
v3_crypto_basic(_Config) ->
    EID = [0,0,0,0,0,0,0,0,0,0,0,2],
    %% From rfc2274 appendix A.3.1
    ?line KMd5_1 = snmp:passwd2localized_key(md5, "maplesyrup", EID),
    ?line [16#52,16#6f,16#5e,16#ed,16#9f,16#cc,16#e2,16#6f,
	   16#89,16#64,16#c2,16#93,16#07,16#87,16#d8,16#2b] =
	KMd5_1,
    %% From rfc2274 appendix A.3.2
    ?line KSHA_1 = snmp:passwd2localized_key(sha, "maplesyrup", EID),
    ?line [16#66,16#95,16#fe,16#bc,16#92,16#88,16#e3,16#62,16#82,16#23,
	   16#5f,16#c7,16#15,16#1f,16#12,16#84,16#97,16#b3,16#8f,16#3f] = 
	KSHA_1,
    %% From rfc2274, appendix A.5.1
    ?line KMd5_2 = snmp:passwd2localized_key(md5, "newsyrup", EID),
    ?line [16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#00,
	   16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#00,
	   16#88,16#05,16#61,16#51,16#41,16#67,16#6c,16#c9,
	   16#19,16#61,16#74,16#e7,16#42,16#a3,16#25,16#51] =
	snmp_user_based_sm_mib:mk_key_change(md5, KMd5_1, KMd5_2, 16,
					     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]),
    %% From rfc2274, appendix A.5.2
    ?line KSHA_2 = snmp:passwd2localized_key(sha, "newsyrup", EID),
    ?line [16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#00,
	   16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#00,
	   16#00,16#00,16#00,16#00,16#9c,16#10,16#17,16#f4,
	   16#fd,16#48,16#3d,16#2d,16#e8,16#d5,16#fa,16#db,
	   16#f8,16#43,16#92,16#cb,16#06,16#45,16#70,16#51] =
	snmp_user_based_sm_mib:mk_key_change(sha, KSHA_1, KSHA_2, 20,
			     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]),
    KSHA_1t = lists:sublist(KSHA_1, 16),
    KSHA_2t = lists:sublist(KSHA_2, 16),
    ?line [16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#00,
	   16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#00,
	   16#7e,16#f8,16#d8,16#a4,16#c9,16#cd,16#b2,16#6b,
	   16#47,16#59,16#1c,16#d8,16#52,16#ff,16#88,16#b5] =
	snmp_user_based_sm_mib:mk_key_change(sha, KSHA_1t, KSHA_2t, 16,
					     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]),

    %% Try with correct random
    ?line Kc1 = snmp_user_based_sm_mib:mk_key_change(md5, KMd5_1, KMd5_2),
    ?line KMd5_2 = snmp_user_based_sm_mib:extract_new_key(md5, KMd5_1, Kc1),
    ?line Kc2 = snmp_user_based_sm_mib:mk_key_change(sha, KSHA_1, KSHA_2),
    ?line KSHA_2 = snmp_user_based_sm_mib:extract_new_key(sha, KSHA_1, Kc2),
    ok.
    


v3_md5_auth(suite) -> [];
v3_md5_auth(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    p("Testing MD5 authentication...takes a few seconds..."),
    
    AgentDir = ?config(agent_dir, Config),
    ?line rewrite_target_params_conf(AgentDir, "authMD5", authNoPriv),
    ?line snmp_target_mib:reconfigure(AgentDir),

    MA = whereis(snmp_master_agent),

    ?line load_master("Test2"),
    ?line load_master("TestTrap"),
    ?line load_master("TestTrapv2"),

    try_test(v3_sync, [[{v2_proc, []},
		   {ma_v2_trap1, [MA]},
		   {v3_inform_sync, [MA]}]],
	[{sec_level, authNoPriv}, {user, "authMD5"}]),

    ?line unload_master("TestTrapv2"),
    ?line unload_master("TestTrap"),
    ?line unload_master("Test2"),
    ?line reset_target_params_conf(AgentDir).

v3_sha_auth(suite) -> [];
v3_sha_auth(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    p("Testing SHA authentication...takes a few seconds..."),

    AgentDir = ?config(agent_dir, Config),
    ?line rewrite_target_params_conf(AgentDir, "authSHA", authNoPriv),
    ?line snmp_target_mib:reconfigure(AgentDir),

    MA = whereis(snmp_master_agent),

    ?line load_master("Test2"),
    ?line load_master("TestTrap"),
    ?line load_master("TestTrapv2"),

    try_test(v3_sync, [[{v2_proc, []},
		   {ma_v2_trap1, [MA]},
		   {v3_inform_sync, [MA]}]],
	[{sec_level, authNoPriv}, {user, "authSHA"}]),

    ?line unload_master("TestTrapv2"),
    ?line unload_master("TestTrap"),
    ?line unload_master("Test2"),
    ?line reset_target_params_conf(AgentDir).

v3_des_priv(suite) -> [];
v3_des_priv(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    p("Testing DES encryption...takes a few seconds..."),

    AgentDir = ?config(agent_dir, Config),
    ?line rewrite_target_params_conf(AgentDir, "privDES", authPriv),
    ?line snmp_target_mib:reconfigure(AgentDir),

    MA = whereis(snmp_master_agent),

    ?line load_master("Test2"),
    ?line load_master("TestTrap"),
    ?line load_master("TestTrapv2"),

    try_test(v3_sync, [[{v2_proc, []},
		   {ma_v2_trap1, [MA]},
		   {v3_inform_sync, [MA]}]],
	[{sec_level, authPriv}, {user, "privDES"}]),

    ?line unload_master("TestTrapv2"),
    ?line unload_master("TestTrap"),
    ?line unload_master("Test2"),
    ?line reset_target_params_conf(AgentDir).

%% -define(usmStatsNotInTimeWindows_instance, [1,3,6,1,6,3,15,1,1,2,0]).

%% Make sure mgr is in sync with agent
v3_sync(Funcs) ->
    ?DBG("v3_sync -> entry with Funcs: ~p",[Funcs]),
    g([[sysDescr, 0]]),
    expect(432, report, [{?usmStatsNotInTimeWindows_instance, any}]),
    g([[sysDescr, 0]]),
    expect(433, [{[sysDescr,0], any}]),
    lists:foreach(fun({Func, Args}) -> apply(?MODULE, Func, Args) end, Funcs).

v3_inform_sync(MA) ->
    ?DBG("v3_sync -> entry with MA: ~p => Send notification",[MA]),
    ?line snmpa:send_notification(MA, testTrapv22, no_receiver,
				 "standard inform", []),
    %% Make sure agent is in sync with mgr...
    ?DBG("v3_sync -> wait some time: ",[]),
    sleep(20000), % more than 1500*10 in target_addr.conf
    ?DBG("v3_sync -> await response",[]),
    ?line expect(1, {inform, true},
		 [{[sysUpTime, 0], any},
		  {[snmpTrapOID, 0], ?system ++ [0,1]}]).


v2_caps(suite) -> [];
v2_caps(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),

    try_test(v2_caps_i, [node()]).

v2_caps_3(X) -> v2_caps(X).


v2_caps_i(Node) ->
    ?line Idx = rpc:call(Node, snmp, add_agent_caps, [[1,2,3,4,5], "test cap"]),
    g([[sysORID, Idx], [sysORDescr, Idx]]),
    ?line expect(1, [{[sysORID, Idx], [1,2,3,4,5]},
		     {[sysORDescr, Idx], "test cap"}]),
    ?line rpc:call(Node, snmp, del_agent_caps, [Idx]),
    g([[sysORID, Idx]]),
    ?line expect(2, [{[sysORID, Idx], noSuchInstance}]).
    

%% Req. Test2
v1_proc() ->
    ?DBG("v1_proc -> entry", []),
    %% According to RFC1157.
    %% Template: <Section>:<list no>
    v1_get_p(),
    v1_get_next_p(),
    v1_set_p().
    
    
v1_get_p() ->
    %% 4.1.2:1
    g([[test2]]),
    ?line expect(10, noSuchName, 1, [{[test2], 'NULL'}]),
    g([[tDescr]]),
    ?line expect(11, noSuchName, 1, [{[tDescr], 'NULL'}]),
    g([[tDescr2,0]]),
    ?line expect(12, noSuchName, 1, [{[tDescr2,0], 'NULL'}]),
    g([[tDescr3,0]]),
    ?line expect(131, noSuchName, 1, [{[tDescr3,0], 'NULL'}]),
    g([[tDescr4,0]]),
    ?line expect(132, noSuchName, 1, [{[tDescr4,0], 'NULL'}]),
    g([[sysDescr, 0], [tDescr,0]]), % Outside mibview
    ?line expect(14, noSuchName, 2, [{[sysDescr, 0], 'NULL'},
				     {[tDescr,0], 'NULL'}]),
    g([[sysDescr,3]]),
    ?line expect(15, noSuchName, 1, [{[sysDescr, 3], 'NULL'}]),
    
    %% 4.1.2:2
    g([[tTable]]),
    ?line expect(20, noSuchName, 1, [{[tTable], 'NULL'}]),
    g([[tEntry]]),
    ?line expect(21, noSuchName, 1, [{[tEntry], 'NULL'}]),
    
    %% 4.1.2:3
    g([[tTooBig, 0]]),
    ?line expect(30, tooBig, 0, [{[tTooBig, 0], 'NULL'}]),

    %% 4.1.2:4
    g([[tGenErr1, 0]]),
    ?line expect(40, genErr, 1, [{[tGenErr1, 0], 'NULL'}]),
    g([[tGenErr2, 0]]),
    ?line expect(41, genErr, 1, [{[tGenErr2, 0], 'NULL'}]),
    g([[sysDescr, 0], [tGenErr3, 0]]),
    ?line expect(42, genErr, 2, [{[sysDescr, 0], 'NULL'},
				 {[tGenErr3, 0], 'NULL'}]).
    
    
v1_get_next_p() ->
    %% 4.1.3:1
    gn([[1,3,7,1]]),
    ?line expect(10, noSuchName, 1, [{[1,3,7,1], 'NULL'}]),
    gn([[tDescr2]]),
    ?line expect(11, tooBig, 0, any),
    
    %% 4.1.3:2
    gn([[tTooBig]]),
    io:format("We currently don't handle tooBig correct!!!\n"),
%    ?line expect(20, tooBig, 0, [{[tTooBig], 'NULL'}]),
    ?line expect(20, tooBig, 0, any),

    %% 4.1.3:3
    gn([[tGenErr1]]),
%    ?line expect(40, genErr, 1, [{[tGenErr1], 'NULL'}]),
    ?line expect(40, genErr, 1, any),
    gn([[tGenErr2]]),
%    ?line expect(41, genErr, 1, [{[tGenErr2], 'NULL'}]),
    ?line expect(41, genErr, 1, any),
    gn([[sysDescr], [tGenErr3]]),
%    ?line expect(42, genErr, 2, [{[sysDescr], 'NULL'},
%				 {[tGenErr3], 'NULL'}]).
    ?line expect(42, genErr, 2, any).
    
v1_set_p() ->
    %% 4.1.5:1
    s([{[1,3,7,0], i, 4}]),
    ?line expect(10, noSuchName, 1, [{[1,3,7,0], 4}]),
    s([{[tDescr,0], s, "outside mibview"}]),
    ?line expect(11, noSuchName, 1, [{[tDescr,0], "outside mibview"}]),
    s([{[tDescr3,0], s, "read-only"}]),
    ?line expect(12, noSuchName, 1, [{[tDescr3,0], "read-only"}]),
    s([{[tDescr3], s, "noSuchObject"}]),
    ?line expect(13, noSuchName, 1, [{[tDescr3], "noSuchObject"}]),
    s([{[tDescr3,1], s, "noSuchInstance"}]),
    ?line expect(14, noSuchName, 1, [{[tDescr3,1], "noSuchInstance"}]),
    s([{[tDescr2,0], s, "inconsistentName"}]),
    ?line expect(15, noSuchName, 1, [{[tDescr2,0], "inconsistentName"}]),

    %% 4.1.5:2
    s([{[tDescr2, 0], i, 4}]),
    ?line expect(20, badValue, 1, [{[tDescr2, 0], 4}]),
    s([{[tDescr2, 0], s, "badValue"}]),
    ?line expect(21, badValue, 1, [{[tDescr2, 0], "badValue"}]),
    
    %% 4.1.5:3
    %% The standard is quite incorrect here.  The resp pdu was too big.  In
    %% the resp pdu, we have the original vbs.  In the tooBig pdu we still
    %% have to original vbs => the tooBig pdu is too big as well!!!  It
    %% may not get it to the manager, unless the agent uses 'NULL' instead
    %% of the std-like original value.
    s([{[tTooBig, 0], s, ?tooBigStr}]),
    %% according to std:
%    ?line expect(30, tooBig, 0, [{[tTooBig, 0], ?tooBigStr}]),
    ?line expect(30, tooBig, 0, [{[tTooBig, 0], 'NULL'}]),
    
    %% 4.1.5:4
    s([{[tDescr2, 0], s, "is_set_ok_fail"}]),
    ?line expect(40, genErr, 1, [{[tDescr2, 0], "is_set_ok_fail"}]),
    s([{[tDescr2, 0], s, "commit_fail"}]),
    ?line expect(41, genErr, 1, [{[tDescr2, 0], "commit_fail"}]).
    
%% Req. Test2
v2_proc() ->
    %% According to RFC1905.
    %% Template: <Section>:<list no>
    ?DBG("v2_proc -> entry",[]),
    v2_get_p(),
    v2_get_next_p(),
    v2_get_bulk_p(),
    v2_set_p().

v2_get_p() ->
    %% 4.2.1:2
    ?DBG("v2_get_p -> entry",[]),
    g([[test2]]),
    ?line expect(10, [{[test2], noSuchObject}]),
    g([[tDescr]]),
    ?line expect(11, [{[tDescr], noSuchObject}]),
    g([[tDescr4,0]]),
    ?line expect(12, [{[tDescr4,0], noSuchObject}]),
    g([[sysDescr, 0], [tDescr,0]]), % Outside mibview
    ?line expect(13, [{[sysDescr,0], "Erlang SNMP agent"},
		      {[tDescr,0], noSuchObject}]),
    g([[tTable]]),
    ?line expect(14, [{[tTable], noSuchObject}]),
    g([[tEntry]]),
    ?line expect(15, [{[tEntry], noSuchObject}]),
    
    %% 4.2.1:3
    g([[tDescr2,0]]), %% instrum ret noSuchName!!!
    ?line expect(20, [{[tDescr2,0], noSuchInstance}]), 
    g([[tDescr3,0]]),
    ?line expect(21, [{[tDescr3,0], noSuchInstance}]),
    g([[sysDescr,3]]),
    ?line expect(22, [{[sysDescr, 3], noSuchInstance}]),
    g([[tIndex,1]]),
    ?line expect(23, [{[tIndex, 1], noSuchInstance}]),

    %% 4.2.1 - any other error: genErr
    g([[tGenErr1, 0]]),
    ?line expect(30, genErr, 1, [{[tGenErr1, 0], 'NULL'}]),
    g([[tGenErr2, 0]]),
    ?line expect(31, genErr, 1, [{[tGenErr2, 0], 'NULL'}]),
    g([[sysDescr, 0], [tGenErr3, 0]]),
    ?line expect(32, genErr, 2, [{[sysDescr, 0], 'NULL'},
				 {[tGenErr3, 0], 'NULL'}]),
    
    %% 4.2.1 - tooBig
    g([[tTooBig, 0]]),
    ?line expect(40, tooBig, 0, []).

    
v2_get_next_p() ->
    %% 4.2.2:2
    ?DBG("v2_get_next_p -> entry",[]),
    gn([[1,3,7,1]]),
    ?line expect(10, [{[1,3,7,1], endOfMibView}]),
    gn([[sysDescr], [1,3,7,1]]),
    ?line expect(11, [{[sysDescr, 0], "Erlang SNMP agent"},
		      {[1,3,7,1], endOfMibView}]),
    gn([[tCnt2, 1]]),
    ?line expect(12, [{[tCnt2,2], 100}]),
    gn([[tCnt2, 2]]),
    ?line expect(12, [{[tCnt2,2], endOfMibView}]),
    
    %% 4.2.2 - any other error: genErr
    gn([[tGenErr1]]),
    ?line expect(20, genErr, 1, [{[tGenErr1], 'NULL'}]),
    gn([[tGenErr2]]),
    ?line expect(21, genErr, 1, [{[tGenErr2], 'NULL'}]),
    gn([[sysDescr], [tGenErr3]]),
    ?line expect(22, genErr, 2, [{[sysDescr], 'NULL'},
				 {[tGenErr3], 'NULL'}]),
    
    %% 4.2.2 - tooBig
    gn([[tTooBig]]),
    ?line expect(20, tooBig, 0, []).

v2_get_bulk_p() ->
    %% 4.2.3
    ?DBG("v2_get_bulk_p -> entry",[]),
    gb(1, 1, []),
    ?line expect(10, []),
    gb(-1, 1, []),
    ?line expect(11, []),
    gb(-1, -1, []),
    ?line expect(12, []),
    gb(-1, -1, []),
    ?line expect(13, []),
    gb(2, 0, [[sysDescr], [1,3,7,1]]),
    ?line expect(14, [{[sysDescr, 0], "Erlang SNMP agent"},
		      {[1,3,7,1], endOfMibView}]),
    gb(1, 2, [[sysDescr], [1,3,7,1]]),
    ?line expect(15, [{[sysDescr, 0], "Erlang SNMP agent"},
		      {[1,3,7,1], endOfMibView}]),
    gb(0, 2, [[sysDescr], [1,3,7,1]]),
    ?line expect(16, [{[sysDescr, 0], "Erlang SNMP agent"},
		      {[1,3,7,1], endOfMibView},
		      {[sysObjectID, 0], [1,2,3]},
		      {[1,3,7,1], endOfMibView}]),
    
    gb(2, 2, [[sysDescr], [1,3,7,1], [sysDescr], [1,3,7,1]]),
    ?line expect(17, [{[sysDescr, 0], "Erlang SNMP agent"},
		      {[1,3,7,1], endOfMibView},
		      {[sysDescr, 0], "Erlang SNMP agent"},		      
		      {[1,3,7,1], endOfMibView},
		      {[sysObjectID, 0], [1,2,3]},
		      {[1,3,7,1], endOfMibView}]),
    
    gb(1, 2, [[sysDescr], [sysDescr], [tTooBig]]),
    ?line expect(18, [{[sysDescr, 0], "Erlang SNMP agent"},
		      {[sysDescr, 0], "Erlang SNMP agent"}]),
    
    gb(1,12, [[tDescr2], [sysDescr]]), % next one after tDescr2 is tTooBig.
    ?line expect(19, []),
    
    gb(2,2, [[sysDescr], [sysObjectID], [tGenErr1], [sysDescr]]),
    ?line expect(20, genErr, 3, [{[sysDescr], 'NULL'},
				 {[sysObjectID], 'NULL'},
				 {[tGenErr1], 'NULL'},
				 {[sysDescr], 'NULL'}]),
    gb(0, 2, [[tCnt2, 1]]),
    ?line expect(21, [{[tCnt2,2], 100},
		      {[tCnt2,2], endOfMibView}]).
    
    
v2_set_p() ->
    %% 4.2.5:1
    ?DBG("v2_set_p -> entry",[]),
    s([{[1,3,7,0], i, 4}]),
    ?line expect(10, noAccess, 1, [{[1,3,7,0], 4}]),
    s([{[tDescr,0], s, "outside mibview"}]),
    ?line expect(11, noAccess, 1, [{[tDescr,0], "outside mibview"}]),
    
    %% 4.2.5:2
    s([{[1,3,6,1,0], s, "noSuchObject"}]),
    ?line expect(20, notWritable, 1, [{[1,3,6,1,0], "noSuchObject"}]),
    
    %% 4.2.5:3
    s([{[tDescr2, 0], i, 4}]),
    ?line expect(30, wrongType, 1, [{[tDescr2, 0], 4}]),
    s([{[tDescr2, 0], s, "badValue"}]),
    ?line expect(31, badValue, 1, [{[tDescr2, 0], "badValue"}]),

    %% 4.2.5:4
    s([{[tStr, 0], s, ""}]),
    ?line expect(40, wrongLength, 1, [{[tStr, 0], ""}]),
    s([{[tStr, 0], s, "12345"}]),
    ?line expect(40, wrongLength, 1, [{[tStr, 0], "12345"}]),
    
    %% 4.2.5:5 - N/A

    %% 4.2.5:6
    s([{[tInt1, 0], i, 0}]),
    ?line expect(60, wrongValue, 1, [{[tInt1, 0], 0}]),
    s([{[tInt1, 0], i, 5}]),
    ?line expect(61, wrongValue, 1, [{[tInt1, 0], 5}]),
    s([{[tInt2, 0], i, 0}]),
    ?line expect(62, wrongValue, 1, [{[tInt2, 0], 0}]),
    s([{[tInt2, 0], i, 5}]),
    ?line expect(63, wrongValue, 1, [{[tInt2, 0], 5}]),
    s([{[tInt3, 0], i, 5}]),
    ?line expect(64, wrongValue, 1, [{[tInt3, 0], 5}]),
    
    %% 4.2.5:7
    s([{[tDescrX, 1, 1], s, "noCreation"}]),
    ?line expect(70, noCreation, 1, [{[tDescrX, 1, 1], "noCreation"}]),

    %% 4.2.5:8
    s([{[tDescrX, 1, 2], s, "inconsistentName"}]),
    ?line expect(80, inconsistentName, 1,
		 [{[tDescrX, 1, 2], "inconsistentName"}]),
    
    %% 4.2.5:9
    s([{[tCnt, 1, 2], i, 5}]),
    ?line expect(90, notWritable, 1, [{[tCnt, 1, 2], 5}]),
    s([{[tDescr3,0], s, "read-only"}]),
    ?line expect(90, notWritable, 1, [{[tDescr3,0], "read-only"}]),

    %% 4.2.5:10
    s([{[tDescr2,0], s, "inconsistentValue"}]),
    ?line expect(100, inconsistentValue, 1,
		 [{[tDescr2,0], "inconsistentValue"}]),
    
    %% 4.2.5:11
    s([{[tDescr2,0], s, "resourceUnavailable"}]),
    ?line expect(110, resourceUnavailable, 1,
		 [{[tDescr2,0],"resourceUnavailable"}]),
    
    %% 4.2.5:12
    s([{[tDescr2, 0], s, "is_set_ok_fail"}]),
    ?line expect(120, genErr, 1, [{[tDescr2, 0], "is_set_ok_fail"}]).
    
    %% commitFailed and undoFailed is tested by the 'undo' case.
    

%% Req. OLD-SNMPEA-MIB
table_test() ->
    io:format("Testing simple get, next and set on communityTable...~n"),
%% {[147,214,36,45], "public", 2, readWrite}.
%% {[147,214,36,45], "standard trap", 2, read}.
    Key1c3 = [intCommunityViewIndex,get(mip),is("public")],
    Key2c3 = [intCommunityViewIndex,get(mip),is("standard trap")],
    Key1c4 = [intCommunityAccess,get(mip),is("public")],
    EndKey = [intCommunityEntry,[9],get(mip),is("public")],
    gn([[intCommunityEntry]]),
    ?line expect(7, [{Key1c3, 2}]),
    gn([[intCommunityTable]]),
    ?line expect(71, [{Key1c3, 2}]),
    gn([[community]]),
    ?line expect(72, [{Key1c3, 2}]),
    gn([[otpSnmpeaMIB]]),
    ?line expect(73, [{Key1c3, 2}]),
    gn([[ericsson]]),
    ?line expect(74, [{Key1c3, 2}]),
    gn([Key1c3]),
    ?line expect(8, [{Key2c3, 2}]),
    gn([Key2c3]),
    ?line expect(9, [{Key1c4, 2}]),
    gn([EndKey]),
    AgentIp = [intAgentIpAddress,0], 
    ?line expect(10, [{AgentIp, any}]),
    g([Key1c3]),
    ?line expect(11, [{Key1c3, 2}]),
    g([EndKey]),
    ?line ?v1_2(expect(12, noSuchName, 1, any),
		expect(12, [{EndKey, noSuchObject}])),

    io:format("Testing row creation/deletion on communityTable...~n"),
    NewKeyc3 = [intCommunityViewIndex,get(mip),is("test")],
    NewKeyc4 = [intCommunityAccess,get(mip),is("test")],
    NewKeyc5 = [intCommunityStatus,get(mip),is("test")],
    s([{NewKeyc5, ?createAndGo}]),
    ?line expect(14, ?v1_2(badValue, inconsistentValue), 1,any),
    s([{NewKeyc5, ?createAndGo}, {NewKeyc3, 2}, {NewKeyc4, 2}]),
    ?line expect(15, [{NewKeyc5, ?createAndGo},{NewKeyc3, 2}, {NewKeyc4, 2}]),
    g([NewKeyc4]),
    ?line expect(16, [{NewKeyc4, 2}]),
    s([{NewKeyc5, ?destroy}]),
    ?line expect(17, [{NewKeyc5, ?destroy}]),
    s([{NewKeyc4, 2}]),
    ?line expect(18, ?v1_2(noSuchName, inconsistentName), 1,[{NewKeyc4, 2}]),
    s([{NewKeyc5, ?createAndWait}]),
    ?line expect(19, [{NewKeyc5, ?createAndWait}]),
    g([NewKeyc5]),
    ?line expect(20, [{NewKeyc5, ?notReady}]),
    s([{NewKeyc4, 2}]),
    ?line expect(21, [{NewKeyc4, 2}]),
    g([NewKeyc5]),
    ?line expect(22, [{NewKeyc5, ?notReady}]),
    s([{NewKeyc3, 2}]),
    ?line expect(23, [{NewKeyc3, 2}]),
    g([NewKeyc5]),
    ?line expect(24, [{NewKeyc5, ?notInService}]),
    s([{NewKeyc5, ?active}]),
    ?line expect(25, [{NewKeyc5, ?active}]),
    s([{NewKeyc5, ?destroy}]),
    ?line expect(26, [{NewKeyc5, ?destroy}]),
    s([{NewKeyc3, 3}]),
    ?line expect(27, ?v1_2(noSuchName, inconsistentName), 1,[{NewKeyc3, 3}]),
    otp_1128().

%% Req. system group
simple_standard_test() ->
    ?DBG("simple_standard_test -> entry",[]),
    gn([[1,1]]),
    ?line expect(1, [{[sysDescr,0], "Erlang SNMP agent"}]),
    gn([[1,3]]),
    ?line expect(11, [{[sysDescr,0], "Erlang SNMP agent"}]),
    gn([[1,3,6]]),
    ?line expect(12, [{[sysDescr,0], "Erlang SNMP agent"}]),
    gn([[1,3,6,1]]),
    ?line expect(13, [{[sysDescr,0], "Erlang SNMP agent"}]),
    gn([[1,3,6,1,2]]),
    ?line expect(14, [{[sysDescr,0], "Erlang SNMP agent"}]),
    gn([[1,3,6,1,2,1]]),
    ?line expect(15, [{[sysDescr,0], "Erlang SNMP agent"}]),
    gn([[1,3,6,1,2,1,1]]),
    ?line expect(16, [{[sysDescr,0], "Erlang SNMP agent"}]),
    gn([[sysDescr]]),
    ?line expect(17, [{[sysDescr,0], "Erlang SNMP agent"}]),
    g([[sysDescr,0]]),
    ?line expect(2, [{[sysDescr,0], "Erlang SNMP agent"}]),
    g([[sysDescr]]),
    ?line ?v1_2(expect(3, noSuchName, 1, any),
		expect(3, [{[sysDescr], noSuchObject}])),
    g([[1,6,7,0]]),
    ?line ?v1_2(expect(41, noSuchName, 1, any),
		expect(3, [{[1,6,7,0], noSuchObject}])),
    gn([[1,13]]),
    ?line ?v1_2(expect(4, noSuchName,1, any),
		expect(4, [{[1,13], endOfMibView}])),
    s([{[sysLocation, 0], "new_value"}]),
    ?line expect(5, [{[sysLocation, 0], "new_value"}]),
    g([[sysLocation, 0]]),
    ?line expect(6, [{[sysLocation, 0], "new_value"}]),
    io:format("Testing noSuchName and badValue...~n"),
    s([{[sysServices,0], 3}]),
    ?line expect(61, ?v1_2(noSuchName, notWritable), 1, any),
    s([{[sysLocation, 0], i, 3}]),
    ?line expect(62, ?v1_2(badValue, wrongType), 1, any),
    ?DBG("simple_standard_test -> done",[]),
    ok.

%% This is run in the agent node
db_notify_client(suite) -> [];
db_notify_client(Config) when list(Config) ->
    {SaNode, MgrNode, MibDir} = init_case(Config),
    ?DBG("~n\tSaNode: ~p~n\tMgrNode: ~p~n\tMibDir: ~p",
	   [SaNode,MgrNode,MibDir]),
    snmpa_local_db:register_notify_client(self(),?MODULE),

    %% This call (the manager) will issue to set operations, so
    %% we expect to receive to notify(insert) calls.
    try_test(db_notify_client_test),

    ?DBG("await first notify",[]),
    receive 
	{db_notify_test_reply,insert} -> ?DBG("first notify received",[]),ok
    end,
    
    ?DBG("await second notify",[]),
    receive 
	{db_notify_test_reply,insert} -> ?DBG("second notify received",[]),ok
    end,

    snmpa_local_db:unregister_notify_client(self()).


%% This is run in the manager node
db_notify_client_test() ->
    ?DBG("set first new sysLocation",[]),
    s([{[sysLocation, 0], "new_value"}]),
    ?line expect(5, [{[sysLocation, 0], "new_value"}]),

    ?DBG("set second new sysLocation",[]),
    s([{[sysLocation, 0], "new_value"}]),
    ?line expect(5, [{[sysLocation, 0], "new_value"}]).

notify(Pid,What) -> 
    ?DBG("notify(~p,~p) -> called",[Pid,What]),
    Pid ! {db_notify_test_reply,What}.


%% Req: system group, OLD-SNMPEA-MIB, Klas1
big_test() ->
    ?DBG("big_test -> testing simple next/get/set @ master agent...",[]),
    simple_standard_test(),
    
    ?DBG("big_test -> testing simple next/get/set @ subagent...",[]),
    gn([[klas1]]),
    ?line expect(1, [{[fname,0], ""}]),
    g([[fname,0]]),
    ?line expect(2, [{[fname,0], ""}]),
    s([{[fname,0], s, "test set"}]),
    ?line expect(3, [{[fname,0], "test set"}]),
    g([[fname,0]]),
    ?line expect(4, [{[fname,0], "test set"}]),
    
    ?DBG("big_test -> "
	"testing next from last instance in master to subagent...",[]),
    gn([[?v1_2(sysServices, sysORLastChange),0]]),
    ?line expect(5, [{[fname,0], "test set"}]),
    gn([[1,1],
	[?v1_2(sysServices, sysORLastChange),0]]),
    ?line expect(51, [{[sysDescr,0], "Erlang SNMP agent"},
		{[fname,0], "test set"}]),
    s([{[fname,0], s, ""}]),
    ?line expect(52, [{[fname,0], ""}]),
    
    table_test(),

    ?DBG("big_test -> adding one row in subagent table",[]),
    _FTab = [friendsEntry],
    s([{[friendsEntry, [2, 3]], s, "kompis3"},
       {[friendsEntry, [3, 3]], i, ?createAndGo}]),
    ?line expect(6, [{[friendsEntry, [2, 3]], "kompis3"},
	       {[friendsEntry, [3, 3]], ?createAndGo}]),
    g([[friendsEntry, [2, 3]],
       [friendsEntry, [3, 3]]]),
    ?line expect(7, [{[friendsEntry, [2, 3]], "kompis3"},
	       {[friendsEntry, [3, 3]], ?active}]),
    s([{[friendsEntry, [3, 3]], i, ?destroy}]),
    ?line expect(8, [{[friendsEntry, [3, 3]], ?destroy}]),
    
    otp_1131(),

    ?DBG("big_test -> adding two rows in subagent table with special INDEX",
       []),
    s([{[kompissEntry, [1, 3]], s, "kompis3"},
       {[kompissEntry, [2, 3]], i, ?createAndGo}]),
    ?line expect(9, [{[kompissEntry, [1, 3]], "kompis3"},
	       {[kompissEntry, [2, 3]], ?createAndGo}]),
    g([[kompissEntry, [1, 3]],
       [kompissEntry, [2, 3]]]),
    ?line expect(10, [{[kompissEntry, [1, 3]], "kompis3"},
		{[kompissEntry, [2, 3]], ?active}]),
    gn([[kompissEntry, [1]],
	[kompissEntry, [2]]]),
    ?line expect(11, [{[kompissEntry, [1, 3]], "kompis3"},
		{[kompissEntry, [2, 3]], ?active}]),
    s([{[kompissEntry, [1, 2]], s, "kompis3"},
       {[kompissEntry, [2, 2]], i, ?createAndGo}]),
    ?line expect(12, [{[kompissEntry, [1, 2]], "kompis3"},
		{[kompissEntry, [2, 2]], ?createAndGo}]),
    gn([[kompissEntry, [1, 1]],
	[kompissEntry, [2, 1]]]),
    ?line expect(13, [{[kompissEntry, [1, 2]], "kompis3"},
		{[kompissEntry, [2, 2]], ?active}]),
    s([{[kompissEntry, [2, 3]], i, ?destroy}]),
    ?line expect(14, [{[kompissEntry, [2, 3]], ?destroy}]),
    s([{[kompissEntry, [2, 2]], i, ?destroy}]),
    ?line expect(15, [{[kompissEntry, [2, 2]], ?destroy}]),
    ?DBG("big_test -> done",[]),
    ok.

%% Req. system group, Klas2, OLD-SNMPEA-MIB
big_test_2() ->
    p("Testing simple next/get/set @ master agent (2)..."),
    simple_standard_test(),
    
    p("Testing simple next/get/set @ subagent (2)..."),
    gn([[klas2]]),
    ?line expect(1, [{[fname2,0], ""}]),
    g([[fname2,0]]),
    ?line expect(2, [{[fname2,0], ""}]),
    s([{[fname2,0], s, "test set"}]),
    ?line expect(3, [{[fname2,0], "test set"}]),
    g([[fname2,0]]),
    ?line expect(4, [{[fname2,0], "test set"}]),

    otp_1298(),

    p("Testing next from last object in master to subagent (2)..."),
    gn([[?v1_2(sysServices, sysORLastChange),0]]),
    ?line expect(5, [{[fname2,0], "test set"}]),
    gn([[1,1],
	[?v1_2(sysServices, sysORLastChange),0]]),
    ?line expect(51, [{[sysDescr,0], "Erlang SNMP agent"},
		{[fname2,0], "test set"}]),
    
    table_test(),
    
    p("Adding one row in subagent table (2)"),
    _FTab = [friendsEntry2],
    s([{[friendsEntry2, [2, 3]], s, "kompis3"},
       {[friendsEntry2, [3, 3]], i, ?createAndGo}]),
    ?line expect(6, [{[friendsEntry2, [2, 3]], "kompis3"},
	       {[friendsEntry2, [3, 3]], ?createAndGo}]),
    g([[friendsEntry2, [2, 3]],
       [friendsEntry2, [3, 3]]]),
    ?line expect(7, [{[friendsEntry2, [2, 3]], "kompis3"},
	       {[friendsEntry2, [3, 3]], ?active}]),
    s([{[friendsEntry2, [3, 3]], i, ?destroy}]),
    ?line expect(8, [{[friendsEntry2, [3, 3]], ?destroy}]),
    
    p("Adding two rows in subagent table with special INDEX (2)"),
    s([{[kompissEntry2, [1, 3]], s, "kompis3"},
       {[kompissEntry2, [2, 3]], i, ?createAndGo}]),
    ?line expect(9, [{[kompissEntry2, [1, 3]], "kompis3"},
	       {[kompissEntry2, [2, 3]], ?createAndGo}]),
    g([[kompissEntry2, [1, 3]],
       [kompissEntry2, [2, 3]]]),
    ?line expect(10, [{[kompissEntry2, [1, 3]], "kompis3"},
		{[kompissEntry2, [2, 3]], ?active}]),
    gn([[kompissEntry2, [1]],
	[kompissEntry2, [2]]]),
    ?line expect(11, [{[kompissEntry2, [1, 3]], "kompis3"},
		{[kompissEntry2, [2, 3]], ?active}]),
    s([{[kompissEntry2, [1, 2]], s, "kompis3"},
       {[kompissEntry2, [2, 2]], i, ?createAndGo}]),
    ?line expect(12, [{[kompissEntry2, [1, 2]], "kompis3"},
		{[kompissEntry2, [2, 2]], ?createAndGo}]),
    gn([[kompissEntry2, [1, 1]],
	[kompissEntry2, [2, 1]]]),
    ?line expect(13, [{[kompissEntry2, [1, 2]], "kompis3"},
		{[kompissEntry2, [2, 2]], ?active}]),
    s([{[kompissEntry2, [2, 3]], i, ?destroy}]),
    ?line expect(14, [{[kompissEntry2, [2, 3]], ?destroy}]),
    s([{[kompissEntry2, [2, 2]], i, ?destroy}]),
    ?line expect(15, [{[kompissEntry2, [2, 2]], ?destroy}]),
    ok.

%% Req. Test1
multi_threaded_test() ->
    p("Testing multi threaded agent..."),
    g([[multiStr,0]]),
    Pid = get_multi_pid(),
    g([[sysUpTime,0]]),
    ?line expect(1, [{[sysUpTime,0], any}]),
    s([{[sysLocation, 0], s, "pelle"}]),
    ?line expect(2, [{[sysLocation, 0], "pelle"}]),
    Pid ! continue,
    ?line expect(3, [{[multiStr,0], "ok"}]),
    
    s([{[multiStr, 0], s, "block"}]),
    Pid2 = get_multi_pid(),    
    g([[sysUpTime,0]]),
    ?line expect(4, [{[sysUpTime,0], any}]),
    g([[multiStr,0]]),
    Pid3 = get_multi_pid(),
    g([[sysUpTime,0]]),
    ?line expect(5, [{[sysUpTime,0], any}]),
    s([{[sysLocation, 0], s, "kalle"}]),
    Pid3 ! continue,
    ?line expect(6, [{[multiStr,0], "ok"}]),
    Pid2 ! continue,
    ?line expect(7, [{[multiStr,0], "block"}]),
    ?line expect(8, [{[sysLocation,0], "kalle"}]).

%% Req. Test1, TestTrapv2
mt_trap_test(MA) ->
    snmpa:send_trap(MA, testTrapv22, "standard trap"),
    ?line expect(1, v2trap, [{[sysUpTime, 0], any},
			     {[snmpTrapOID, 0], ?system ++ [0,1]}]),

    snmpa:send_trap(MA, mtTrap, "standard trap"),
    Pid = get_multi_pid(),
    g([[sysUpTime,0]]),
    ?line expect(2, [{[sysUpTime,0], any}]),
    snmpa:send_trap(MA, testTrapv22, "standard trap"),
    ?line expect(3, v2trap, [{[sysUpTime, 0], any},
			     {[snmpTrapOID, 0], ?system ++ [0,1]}]),
    Pid ! continue,
    ?line expect(4, v2trap, [{[sysUpTime, 0], any},
			     {[snmpTrapOID, 0], ?testTrap ++ [2]},
			     {[multiStr,0], "ok"}]).

    
get_multi_pid() ->
    get_multi_pid(10).
get_multi_pid(0) ->
    ?line ?FAIL(no_global_name);
get_multi_pid(N) ->
    sleep(1000),
    case global:whereis_name(snmp_multi_tester) of
	Pid when pid(Pid) -> Pid;
	_ -> get_multi_pid(N-1)
    end.

%% Req. Test1
types_v2_test() ->
    p("Testing v2 types..."),

    s([{[bits1,0], 2#10}]),
    ?line expect(1, [{[bits1,0], ?str(2#10)}]),
    g([[bits1,0]]),
    ?line expect(2, [{[bits1,0], ?str(2#101)}]),
    
    s([{[bits2,0], 2#11000000110}]),
    ?line expect(3, [{[bits2,0], ?str(2#11000000110)}]),
    g([[bits2,0]]),
    ?line expect(4, [{[bits2,0], ?str(2#11000000110)}]),
    
    g([[bits3,0]]),
    ?line expect(50, genErr, 1, any),
    
    g([[bits4,0]]),
    ?line expect(51, genErr, 1, any),
    
    s([{[bits1,0], s, [2#10]}]),
    ?line expect(6, ?v1_2(badValue, wrongValue), 1, any),

    s([{[bits2,0], 2#11001001101010011}]),
    ?line expect(7, ?v1_2(badValue, wrongValue), 1, any).
    

%% Req. Test1
implied_test(MA) ->
    ?LOG("implied_test -> start",[]),
    p("Testing IMPLIED..."),

    snmpa:verbosity(MA,trace),
    snmpa:verbosity(MA,trace),

    %% Create two rows, check that they are get-nexted in correct order.
    Idx1 = "apa",
    Idx2 = "qq",
    ?DBG("implied_test -> (send) create row 1 '~s' in table 1",[Idx1]),
    s([{[testStatus, Idx1], i, ?createAndGo}, {[testDescr, Idx1],s,"row 1"}]),
    ?line expect(1, [{[testStatus, Idx1], ?createAndGo},
		     {[testDescr, Idx1], "row 1"}]),
    ?DBG("implied_test -> (send) create row 2 '~s' in table 1",[Idx2]),
    s([{[testStatus, Idx2], i, ?createAndGo}, {[testDescr, Idx2],s,"row 2"}]),
    ?line expect(2, [{[testStatus, Idx2], ?createAndGo},
		     {[testDescr, Idx2], "row 2"}]),
    ?DBG("implied_test -> get-next(testDescr)",[]),
    gn([[testDescr]]),
    ?line expect(3, [{[testDescr,Idx1], "row 1"}]),
    ?DBG("implied_test -> get-next(testDescr) of row 1",[]),
    gn([[testDescr,Idx1]]),
    ?line expect(4, [{[testDescr,Idx2], "row 2"}]),

    % Delete the rows
    ?DBG("implied_test -> (send) delete row 1 '~s' from table 1",[Idx1]),
    s([{[testStatus, Idx1], i, ?destroy}]),
    ?line expect(5, [{[testStatus, Idx1], ?destroy}]),
    ?DBG("implied_test -> (send) delete row 2 '~s' from table 1",[Idx2]),
    s([{[testStatus, Idx2], i, ?destroy}]),
    ?line expect(6, [{[testStatus, Idx2], ?destroy}]),

    %% Try the same in other table
    Idx3 = [1, "apa"],
    Idx4 = [1, "qq"],
    ?DBG("implied_test -> (send) create row 1 '~s' in table 2",[Idx3]),
    s([{[testStatus2, Idx3], i, ?createAndGo}, {[testDescr2,Idx3],s,"row 1"}]),
    ?line expect(1, [{[testStatus2, Idx3], ?createAndGo},
		     {[testDescr2, Idx3], "row 1"}]),
    ?DBG("implied_test -> (send) create row 2 '~s' in table 2",[Idx4]),
    s([{[testStatus2, Idx4], i, ?createAndGo}, {[testDescr2,Idx4],s,"row 2"}]),
    ?line expect(2, [{[testStatus2, Idx4], ?createAndGo},
		     {[testDescr2, Idx4], "row 2"}]),
    ?DBG("implied_test -> get-next(testDescr2)",[]),
    gn([[testDescr2]]),
    ?line expect(3, [{[testDescr2,Idx3], "row 1"}]),
    ?DBG("implied_test -> get-next(testDescr2) of row 1",[]),
    gn([[testDescr2,Idx3]]),
    ?line expect(4, [{[testDescr2,Idx4], "row 2"}]),

    % Delete the rows
    ?DBG("implied_test -> (send) delete row 1 '~s' from table 2",[Idx3]),
    s([{[testStatus2, Idx3], i, ?destroy}]),
    ?line expect(5, [{[testStatus2, Idx3], ?destroy}]),
    ?DBG("implied_test -> (send) delete row 2 '~s' from table 2",[Idx4]),
    s([{[testStatus2, Idx4], i, ?destroy}]),
    ?line expect(6, [{[testStatus2, Idx4], ?destroy}]),

    snmpa:verbosity(MA,log),

    ?LOG("implied_test -> done",[]).
    
    

%% Req. Test1
sparse_table_test() ->
    p("Testing sparse table..."),

    %% Create two rows, check that they are get-nexted in correct order.
    Idx1 = 1,
    Idx2 = 2,
    s([{[sparseStatus, Idx1], i, ?createAndGo},
       {[sparseDescr, Idx1], s, "row 1"}]),
    ?line expect(1, [{[sparseStatus, Idx1], ?createAndGo},
		     {[sparseDescr, Idx1], "row 1"}]),
    s([{[sparseStatus, Idx2], i, ?createAndGo},
       {[sparseDescr, Idx2], s, "row 2"}]),
    ?line expect(2, [{[sparseStatus, Idx2], ?createAndGo},
		     {[sparseDescr, Idx2], "row 2"}]),
    ?v1_2(gn([[sparseIndex], [sparseDescr,Idx1], [sparseDescr,Idx2],
	      [sparseStatus,Idx1], [sparseStatus,Idx2]]),
	  gb(0,5,[[sparseIndex]])),
    ?line expect(3, [{[sparseDescr,Idx1], "row 1"},
		     {[sparseDescr,Idx2], "row 2"},
		     {[sparseStatus,Idx1], ?active},
		     {[sparseStatus,Idx2], ?active},
		     {[sparseStr,0], "slut"}]),
    % Delete the rows
    s([{[sparseStatus, Idx1], i, ?destroy}]),
    ?line expect(4, [{[sparseStatus, Idx1], ?destroy}]),
    s([{[sparseStatus, Idx2], i, ?destroy}]),
    ?line expect(5, [{[sparseStatus, Idx2], ?destroy}]).


%% Req. Test1
cnt_64_test(MA) ->
    ?LOG("start cnt64 test (~p)",[MA]),
    snmpa:verbosity(MA,trace),
    ?LOG("start cnt64 test",[]),
    p("Testing Counter64, and at the same time, RowStatus is not last column"),
    
    ?DBG("get cnt64",[]),
    g([[cnt64,0]]),
    ?DBG("await response",[]),
    ?line ?v1_2(expect(1, noSuchName, 1, any),
		expect(1, [{[cnt64,0],18446744073709551615}])),
    ?DBG("get-next cnt64",[]),
    gn([[cnt64]]),
    ?DBG("await response",[]),
    ?line ?v1_2(expect(2, [{[cnt64Str,0], "after cnt64"}]),
		expect(2, [{[cnt64,0],18446744073709551615}])),
    ?DBG("send cntTrap",[]),
    snmpa:send_trap(MA,cntTrap,"standard trap",[{sysContact,"pelle"},
					       {cnt64, 10},
					       {sysLocation, "here"}]),
    ?DBG("await response",[]),
    ?line ?v1_2(expect(3, trap, [test], 6, 1, [{[sysContact,0], "pelle"},
					       {[sysLocation,0], "here"}]),
		expect(3, v2trap, [{[sysUpTime, 0], any},
				   {[snmpTrapOID, 0], ?testTrap ++ [1]},
				   {[sysContact,0], "pelle"},
				   {[cnt64,0], 10},
				   {[sysLocation,0], "here"}])),
    
    %% Create two rows, check that they are get-nexted in correct order.
    Idx1 = 1,
    Idx2 = 2,
    ?DBG("create row (cntStatus): ~p",[Idx1]),
    s([{[cntStatus, Idx1], i, ?createAndGo}]),
    ?DBG("await response",[]),
    ?line expect(1, [{[cntStatus, Idx1], ?createAndGo}]),
    ?DBG("create row (cntStatus): ~p",[Idx2]),
    s([{[cntStatus, Idx2], i, ?createAndGo}]),
    ?DBG("await response",[]),
    ?line expect(2, [{[cntStatus, Idx2], ?createAndGo}]),

    ?DBG("get-next (cntIndex)",[]),
    gn([[cntIndex]]),
    ?DBG("await response",[]),
    ?line ?v1_2(expect(3, [{[cntStatus,Idx1], ?active}]),
		expect(3, [{[cntCnt,Idx1], 0}])),
    % Delete the rows
    ?DBG("delete row (cntStatus): ~p",[Idx1]),
    s([{[cntStatus, Idx1], i, ?destroy}]),
    ?DBG("await response",[]),
    ?line expect(4, [{[cntStatus, Idx1], ?destroy}]),
    ?DBG("delete row (cntStatus): ~p",[Idx2]),
    s([{[cntStatus, Idx2], i, ?destroy}]),
    ?DBG("await response",[]),
    ?line expect(5, [{[cntStatus, Idx2], ?destroy}]),
    catch snmpa:verbosity(MA,log),
    ?DBG("done",[]),
    ok.

%% Req. Test1
opaque_test() ->
    p("Testing Opaque datatype..."),
    g([[opaqueObj,0]]),
    ?line expect(1, [{[opaqueObj,0], "opaque-data"}]).
    
%% Req. OLD-SNMPEA-MIB
api_test(MaNode) ->
    ?line {value, OID} = rpc:call(MaNode, snmp, name_to_oid,
				  [intAgentIpAddress]),
    ?line {value, intAgentIpAddress} = rpc:call(MaNode, snmp,
						oid_to_name, [OID]),
    ?line false = rpc:call(MaNode, snmp, name_to_oid, [intAgentIpAddres]),
    ?line false = rpc:call(MaNode, snmp, oid_to_name,
			   [[1,5,32,3,54,3,3,34,4]]),
    ?line {value, 2} = rpc:call(MaNode, snmp, enum_to_int,
				[intViewType, excluded]),
    ?line {value, excluded} = rpc:call(MaNode, snmp, int_to_enum,
				       [intViewType, 2]),
    ?line false = rpc:call(MaNode, snmp, enum_to_int, [intViewType, exclude]),
    ?line false = rpc:call(MaNode, snmp, enum_to_int,
			   [intAgentIpAddress, exclude]),
    ?line false = rpc:call(MaNode, snmp, enum_to_int,
			   [intAgentIpAddre, exclude]),
    ?line false = rpc:call(MaNode, snmp, int_to_enum, [intViewType, 3]),
    ?line false = rpc:call(MaNode, snmp, int_to_enum, [intAgentIpAddress, 2]),
    ?line false = rpc:call(MaNode, snmp, int_to_enum, [intAgentIpAddre, 2]),
    ?line {value, active} = rpc:call(MaNode, snmp,
				     int_to_enum, ['RowStatus', ?active]),
    ?line {value, ?destroy} = rpc:call(MaNode, snmp,
				       enum_to_int, ['RowStatus', destroy]),
    ?line false = rpc:call(MaNode, snmp,
			   enum_to_int, ['RowStatus', xxxdestroy]),
    ?line false = rpc:call(MaNode, snmp,
			   enum_to_int, ['xxRowStatus', destroy]),
    ?line false = rpc:call(MaNode, snmp, int_to_enum, ['RowStatus', 25]),
    ?line false = rpc:call(MaNode, snmp, int_to_enum, ['xxRowStatus', 1]),
    ?line case snmp:date_and_time() of
	      List when list(List), length(List) == 8 -> ok;
	      List when list(List), length(List) == 11 -> ok
    end.

%% Req. Klas3
api_test2() ->
    g([[fname3,0]]),
    ?line expect(1, [{[fname3,0], "ok"}]),
    g([[fname4,0]]),
    ?line expect(2, [{[fname4,0], 1}]).

api_test3() ->
    g([[fname3,0]]),
    ?line expect(1, [{[fname3,0], "ok"}]).
    
    
unreg_test() ->
    gn([[?v1_2(sysServices, sysORLastChange),0]]),
    ?line expect(1, [{[snmpInPkts, 0], any}]).

load_test() ->
    gn([[?v1_2(sysServices, sysORLastChange),0]]),
    ?line expect(1, [{[fname,0], ""}]).

%% Req. Klas1
load_test_sa() ->
    gn([[?v1_2(sysServices,sysORLastChange), 0]]),
    ?line expect(1, [{[fname,0], any}]).
    
%% Req. system group, Klas1, OLD-SNMPEA-MIB
do_mul_get() ->
    Key1c3 = [intCommunityEntry,[3],get(mip),is("public")],
    Key1c4 = [intCommunityEntry,[4],get(mip),is("public")],
    s([{[fname,0], s, "test set"}]),
    ?line expect(3, [{[fname,0], "test set"}]),
    g([[sysDescr,0], Key1c4, [fname,0],Key1c3,
	       [sysName,0]]),
    ?line expect(1, [{[sysDescr,0], "Erlang SNMP agent"},
		     {Key1c4, 2},
		     {[fname,0], "test set"},
		     {Key1c3, 2},
		     {[sysName,0], "test"}]),
    g([[1,3,7,1], Key1c4, [sysDescr,0], [1,3,7,2], Key1c3, [sysDescr,0]]),
    ?line ?v1_2(expect(2, noSuchName, [1,4], any),
		expect(2, [{[1,3,7,1], noSuchObject},
			   {Key1c4, 2},
			   {[sysDescr,0], "Erlang SNMP agent"},
			   {[1,3,7,2], noSuchObject},
			   {Key1c3, 2},
			   {[sysDescr,0], "Erlang SNMP agent"}])).

%% Req. v1, system group, Klas1, OLD-SNMPEA-MIB, *ej* Klas3.
do_mul_get_err() ->
    Key1c3 = [intCommunityEntry,[3],get(mip),is("public")],
    Key1c4 = [intCommunityEntry,[4],get(mip),is("public")],
    s([{[fname,0], s, "test set"}]),
    ?line expect(3, [{[fname,0], "test set"}]),
    g([[sysDescr,0],Key1c4,[fname,0], Key1c3, [sysName,2]]),
    ?line ?v1_2(expect(1, noSuchName, 5, any),
		expect(1, [{[sysDescr,0], "Erlang SNMP agent"},
			   {Key1c4, 2},
			   {[fname,0], "test set"},
			   {Key1c3, 2},
			   {[sysName,2], noSuchInstance}])),
    g([[sysDescr,0],Key1c4,[fname3,0], Key1c3, [sysName,1]]),
    ?line ?v1_2(expect(1, noSuchName, [3,5], any),
		expect(1, [{[sysDescr,0], "Erlang SNMP agent"},
			   {Key1c4, 2},
			   {[fname3,0], noSuchObject},
			   {Key1c3, 2},
			   {[sysName,1], noSuchInstance}])).


%% Req. system group, Klas1, OLD-SNMPEA-MIB
do_mul_next() ->
    Key1c3s = [intCommunityEntry,[3],get(mip),is("publi")],
    Key1c4s = [intCommunityEntry,[4],get(mip),is("publi")],
    Key1c3 = [intCommunityEntry,[3],get(mip),is("public")],
    Key1c4 = [intCommunityEntry,[4],get(mip),is("public")],
    s([{[fname,0], s, "test set"}]),
    ?line expect(3, [{[fname,0], "test set"}]),
    gn([[sysDescr], Key1c4s, [fname],Key1c3s,[sysName]]),
    ?line expect(1, [{[sysDescr,0], "Erlang SNMP agent"},
	       {Key1c4, 2}, {[fname,0], "test set"},
	       {Key1c3, 2}, {[sysName,0], "test"}]).

%% Req. system group, Klas1, OLD-SNMPEA-MIB
do_mul_next_err() ->
    Key1c3s = [intCommunityEntry,[3],get(mip),is("publi")],
    Key1c4s = [intCommunityEntry,[4],get(mip),is("publi")],
    Key1c3 = [intCommunityEntry,[3],get(mip),is("public")],
    Key1c4 = [intCommunityEntry,[4],get(mip),is("public")],
    s([{[fname,0], s, "test set"}]),
    ?line expect(3, [{[fname,0], "test set"}]),
    gn([[sysDescr], Key1c4s, [1,3,6,999], [fname],[1,3,90], Key1c3s,[sysName]]),
    ?line ?v1_2(expect(1, noSuchName, [3,5], any),
		expect(1, [{[sysDescr,0], "Erlang SNMP agent"},
			   {Key1c4, 2},
			   {[1,3,6,999], endOfMibView},
			   {[fname,0], "test set"},
			   {[1,3,90], endOfMibView},
			   {Key1c3, 2},
			   {[sysName,0], "test"}])).
		

%% Req. system group, Klas1, OLD-SNMPEA-MIB
do_mul_set() ->
    p("Adding one row in subagent table, and one in master table"),
    NewKeyc3 = [intCommunityEntry,[3],get(mip),is("test")],
    NewKeyc4 = [intCommunityEntry,[4],get(mip),is("test")],
    NewKeyc5 = [intCommunityEntry,[5],get(mip),is("test")],
    s([{[friendsEntry, [2, 3]], "kompis3"},
       {NewKeyc3, 2},
       {[sysLocation,0], "new_value"},
       {NewKeyc5, ?createAndGo},
       {NewKeyc4, 2},
       {[friendsEntry, [3, 3]], ?createAndGo}]),
    ?line expect(1, [{[friendsEntry, [2, 3]], "kompis3"},
	       {NewKeyc3, 2},
	       {[sysLocation,0], "new_value"},
	       {NewKeyc5, ?createAndGo},
	       {NewKeyc4, 2},
	       {[friendsEntry, [3, 3]], ?createAndGo}]),
    g([[friendsEntry, [2, 3]],
	       [sysLocation,0],
	       [friendsEntry, [3, 3]]]),
    ?line expect(2, [{[friendsEntry, [2, 3]], "kompis3"},
	       {[sysLocation,0], "new_value"},
	       {[friendsEntry, [3, 3]], ?active}]),
    g([NewKeyc4]),
    ?line expect(3, [{NewKeyc4, 2}]),
    s([{[friendsEntry, [3, 3]], ?destroy},
       {NewKeyc5, ?destroy}]),
    ?line expect(4, [{[friendsEntry, [3, 3]], ?destroy},
	       {NewKeyc5, ?destroy}]).

%% Req. system group, Klas1, OLD-SNMPEA-MIB
do_mul_set_err() ->
    NewKeyc3 = [intCommunityEntry,[3],get(mip),is("test")],
    NewKeyc4 = [intCommunityEntry,[4],get(mip),is("test")],
    NewKeyc5 = [intCommunityEntry,[5],get(mip),is("test")],
    p("Adding one row in subagent table, and one in master table"),
    s([{[friendsEntry, [2, 3]], s, "kompis3"},
       {NewKeyc3, 2},
       {[sysUpTime,0], 45},   % sysUpTime (readOnly)
       {NewKeyc5, ?createAndGo},
       {NewKeyc4, 2},
       {[friendsEntry, [3, 3]], ?createAndGo}]),
    ?line expect(1, ?v1_2(noSuchName, notWritable), 3, any),
    g([[friendsEntry, [2, 3]]]),
    ?line ?v1_2(expect(2, noSuchName, 1, any),
		expect(2, [{[friendsEntry, [2,3]], noSuchInstance}])),
    g([NewKeyc4]),
    ?line ?v1_2(expect(3, noSuchName, 1, any),
		expect(3, [{NewKeyc4, noSuchInstance}])).

%% Req. SA-MIB
sa_mib() ->
    g([[sa, [2,0]]]),
    ?line expect(1, [{[sa, [2,0]], 3}]),
    s([{[sa, [1,0]], s, "sa_test"}]),
    ?line expect(2, [{[sa, [1,0]], "sa_test"}]).

ma_trap1(MA) ->
    snmpa:send_trap(MA, testTrap2, "standard trap"),
    ?line expect(1, trap, [system], 6, 1, [{[system, [4,0]],
				    "{mbj,eklas}@erlang.ericsson.se"}]),
    snmpa:send_trap(MA, testTrap1, "standard trap"),
    ?line expect(2, trap, [1,2,3] , 1, 0, [{[system, [4,0]],
				      "{mbj,eklas}@erlang.ericsson.se"}]).

ma_trap2(MA) ->
    snmpa:send_trap(MA,testTrap2,"standard trap",[{sysContact,"pelle"}]),
    ?line expect(3, trap, [system], 6, 1, [{[system, [4,0]], "pelle"}]).

ma_v2_2_v1_trap(MA) ->
    snmpa:send_trap(MA,testTrapv22,"standard trap",[{sysContact,"pelle"}]),
    ?line expect(3, trap, [system], 6, 1, [{[system, [4,0]], "pelle"}]).    

ma_v2_2_v1_trap2(MA) ->
    snmpa:send_trap(MA,linkUp,"standard trap",[{ifIndex, [1], 1},
					      {ifAdminStatus, [1], 1},
					      {ifOperStatus, [1], 2}]),
    ?line expect(3, trap, [1,2,3], 3, 0, [{[ifIndex, 1], 1},
					 {[ifAdminStatus, 1], 1},
					 {[ifOperStatus, 1], 2}]).    

sa_trap1(SA) ->
    snmpa:send_trap(SA, saTrap, "standard trap"),
    ?line expect(4, trap, [ericsson], 6, 1, [{[system, [4,0]],
				      "{mbj,eklas}@erlang.ericsson.se"},
				     {[sa, [1,0]], "sa_test"}]).

sa_trap2(SA) ->
    snmpa:send_trap(SA, saTrap, "standard trap",[{sysContact,"pelle"}]),
    ?line expect(5, trap, [ericsson], 6, 1, [{[system, [4,0]],
				      "pelle"},
				     {[sa, [1,0]], "sa_test"}]).

sa_trap3(SA) ->
    snmpa:send_trap(SA, saTrap2, "standard trap",
			 [{intViewSubtree, [4], [1,2,3,4]}]),
    ?line expect(6, trap, [ericsson], 6, 2, [{[system, [4,0]],
				      "{mbj,eklas}@erlang.ericsson.se"},
				     {[sa, [1,0]], "sa_test"},
				     {[intViewSubtree,4],[1,2,3,4]}]).

ma_v2_trap1(MA) ->
    ?DBG("ma_v2_traps -> entry with MA = ~p => "
	   "send standard trap: testTrapv22",[MA]),
    snmpa:send_trap(MA, testTrapv22, "standard trap"),
    ?line expect(1, v2trap, [{[sysUpTime, 0], any},
			     {[snmpTrapOID, 0], ?system ++ [0,1]}]),
    ?DBG("ma_v2_traps -> send standard trap: testTrapv21",[]),
    snmpa:send_trap(MA, testTrapv21, "standard trap"),
    ?line expect(2, v2trap, [{[sysUpTime, 0], any},
			     {[snmpTrapOID, 0], ?snmp ++ [1]}]).

ma_v2_trap2(MA) ->
    snmpa:send_trap(MA,testTrapv22,"standard trap",[{sysContact,"pelle"}]),
    ?line expect(3, v2trap, [{[sysUpTime, 0], any},
			     {[snmpTrapOID, 0], ?system ++ [0,1]},
			     {[system, [4,0]], "pelle"}]).

%% Note:  This test case takes a while... actually a couple of minutes.
ma_v2_inform1(MA) ->
    ?DBG("ma_v2_inform -> entry with MA = ~p => "
	   "send notification: testTrapv22",[MA]),
    ?line snmpa:send_notification(MA, testTrapv22, no_receiver, "standard inform", []),
    ?line expect(1, {inform, true},
		 [{[sysUpTime, 0], any},
		  {[snmpTrapOID, 0], ?system ++ [0,1]}]),

    ?DBG("ma_v2_inform -> send notification: testTrapv22",[]),
    snmpa:send_notification(MA, testTrapv22, {tag1, self()},
			   "standard inform", []),
    ?line expect(1, {inform, true},
		 [{[sysUpTime, 0], any},
		  {[snmpTrapOID, 0], ?system ++ [0,1]}]),
    ?DBG("ma_v2_inform -> await targets",[]),
    receive
	{snmp_targets, tag1, [_]} ->
	    ok;
	{snmp_targets, tag1, Addrs1} ->
	    ?line ?FAIL({bad_addrs, Addrs1})
    after
	5000 ->
	    ?ERR("ma_v2_inform1 -> awaiting snmp_targets(tag1) timeout",[]),
	    ?line ?FAIL(nothing_at_all)
    end,
    ?DBG("ma_v2_inform -> await notification",[]),
    receive
	{snmp_notification, tag1, {got_response, _}} ->
	    ok;
	{snmp_notification, tag1, {no_response, _}} ->
	    ?line ?FAIL(no_response)
    after
	20000 ->
	    ?ERR("ma_v2_inform1 -> "
		   "awaiting snmp_notification(tag1) timeout",[]),
	    ?line ?FAIL(nothing_at_all)
    end,
    
    %%
    %% -- The rest is possibly erroneous...
    %% 

    ?DBG("ma_v2_inform -> send notification: testTrapv22",[]),
    snmpa:send_notification(MA, testTrapv22, {tag2, self()},
			   "standard inform", []),
    ?line expect(2, {inform, false},
		 [{[sysUpTime, 0], any},
		  {[snmpTrapOID, 0], ?system ++ [0,1]}]),
    ?DBG("ma_v2_inform -> await targets",[]),
    receive
	{snmp_targets, tag2, [_]} ->
	    ok;
	{snmp_targets, tag2, Addrs2} ->
	    ?ERR("ma_v2_inform1 -> awaiting snmp_targets(tag2) timeout",[]),
	    ?line ?FAIL({bad_addrs, Addrs2})
    after
	5000 ->
	    ?line ?FAIL(nothing_at_all)
    end,
    ?DBG("ma_v2_inform -> await notification",[]),
    receive
	{snmp_notification, tag2, {got_response, _}} ->
	    ?line ?FAIL(got_response);
	{snmp_notification, tag2, {no_response, _}} ->
	    ok
    after
	240000 ->
	    ?ERR("ma_v2_inform1 -> "
		   "awaiting snmp_notification(tag2) timeout",[]),
	    ?line ?FAIL(nothing_at_all)
    end.
    

ma_v1_2_v2_trap(MA) ->
    snmpa:send_trap(MA,linkDown,"standard trap",[{ifIndex, [1], 1}]),
    ?line expect(2, v2trap, [{[sysUpTime, 0], any},
			     {[snmpTrapOID, 0], ?snmpTraps ++ [3]},
			     {[ifIndex, 1], 1},
			     {[snmpTrapEnterprise, 0], [1,2,3]}]).

    
ma_v1_2_v2_trap2(MA) ->
    snmpa:send_trap(MA,testTrap2,"standard trap",[{sysContact,"pelle"}]),
    ?line expect(3, v2trap, [{[sysUpTime, 0], any},
			     {[snmpTrapOID, 0], ?system ++ [0,1]},
			     {[system, [4,0]], "pelle"},
			     {[snmpTrapEnterprise, 0], ?system}]).
    

sa_v1_2_v2_trap1(SA) ->
    snmpa:send_trap(SA, saTrap, "standard trap"),
    ?line expect(4, v2trap, [{[sysUpTime, 0], any},
			     {[snmpTrapOID, 0], ?ericsson ++ [0, 1]},
			     {[system, [4,0]],
			      "{mbj,eklas}@erlang.ericsson.se"},
			     {[sa, [1,0]], "sa_test"},
			     {[snmpTrapEnterprise, 0], ?ericsson}]).

sa_v1_2_v2_trap2(SA) ->
    snmpa:send_trap(SA, saTrap, "standard trap",[{sysContact,"pelle"}]),
    ?line expect(4, v2trap, [{[sysUpTime, 0], any},
			     {[snmpTrapOID, 0], ?ericsson ++ [0, 1]},
			     {[system, [4,0]], "pelle"},
			     {[sa, [1,0]], "sa_test"},
			     {[snmpTrapEnterprise, 0], ?ericsson}]).
			     

sa_v1_2_v2_trap3(SA) ->
    snmpa:send_trap(SA, saTrap2, "standard trap",
			 [{intViewSubtree, [4], [1,2,3,4]}]),
    ?line expect(4, v2trap, [{[sysUpTime, 0], any},
			     {[snmpTrapOID, 0], ?ericsson ++ [0, 2]},
			     {[system, [4,0]],
			      "{mbj,eklas}@erlang.ericsson.se"},
			     {[sa, [1,0]], "sa_test"},
			     {[intViewSubtree,4],[1,2,3,4]},
			     {[snmpTrapEnterprise, 0], ?ericsson}]).
			     

%% Req. SA-MIB, OLD-SNMPEA-MIB
sa_errs_bad_value() ->
    NewKeyc3 = [intCommunityEntry,[3],get(mip),is("test")],
    NewKeyc4 = [intCommunityEntry,[4],get(mip),is("test")],
    NewKeyc5 = [intCommunityEntry,[5],get(mip),is("test")],
    s([{NewKeyc3, 2},
       {[sa, [2,0]], 5}, % badValue (i is_set_ok)
       {NewKeyc5, ?createAndGo},
       {NewKeyc4, 2}]),
    ?line expect(1, badValue, 2, any),   
    s([{NewKeyc3, 2},
       {[sa, [2,0]], 6}, % wrongValue (i is_set_ok)
       {NewKeyc5, ?createAndGo},
       {NewKeyc4, 2}]),
    ?line expect(1, ?v1_2(badValue, wrongValue), 2, any),   
    g([NewKeyc4]),
    ?line ?v1_2(expect(2, noSuchName, 1, any),
		expect(2, [{NewKeyc4, noSuchInstance}])).

%% Req. SA-MIB, OLD-SNMPEA-MIB
sa_errs_gen_err() ->
    NewKeyc3 = [intCommunityEntry,[3],get(mip),is("test")],
    NewKeyc4 = [intCommunityEntry,[4],get(mip),is("test")],
    NewKeyc5 = [intCommunityEntry,[5],get(mip),is("test")],
    s([{NewKeyc3, 2},{NewKeyc4, 2},
       {NewKeyc5, ?createAndGo}, {[sa, [3,0]], 5}]),
    ?line expect(1, genErr, 4, any),
% The row might have been added; we don't know.
% (as a matter of fact we do - it is added, because the agent
% first sets its own vars, and then th SAs. Lets destroy it.
    s([{NewKeyc5, ?destroy}]),
    ?line expect(2, [{NewKeyc5, ?destroy}]).

%% Req. SA-MIB, OLD-SNMPEA-MIB
sa_too_big() ->
    g([[sa, [4,0]]]),
    ?line expect(1, tooBig).

%% Req. Klas1, system group, snmp group (v1/v2)
next_across_sa() ->
    gn([[sysDescr],[klas1,5]]),
    ?line expect(1, [{[sysDescr,0], "Erlang SNMP agent"},
	       {[snmpInPkts, 0], any}]).

%% snmp_test_mgr:s([{[fStatus3, 1], 4}, {[fname3,0], "ok"}]). -> noError
%% snmp_test_mgr:s([{[fStatus3, 1], 4}, {[fname3,0], "hoj"}]). -> {badValue, 2}
%% snmp_test_mgr:s([{[fStatus3, 3], 4}, {[fname3,0], "hoj"}]). -> {genErr, 1}
%% snmp_test_mgr:s([{[fStatus3, 4], 4}, {[fname3,0], "ok"}]). -> {genErr, 1}
%% snmp_test_mgr:s([{[fStatus3, 4], 4}, {[fname3,0], "ufail"}]). -> {genErr, 1}
%% snmp_test_mgr:s([{[fStatus3, 1], 4}, {[fname3,0], "xfail"}]). -> {genErr, 2}
%% Req. Klas3, Klas4
undo_test() ->
    s([{[fStatus3, 1], 4}, {[fname3,0], "ok"}]),
    ?line expect(1, [{[fStatus3, 1], 4}, {[fname3,0], "ok"}]),
    s([{[fStatus3, 1], 4}, {[fname3,0], "hoj"}]),
    ?line expect(2, ?v1_2(badValue, inconsistentValue), 2, any), 
    s([{[fStatus3, 3], 4}, {[fname3,0], "hoj"}]),
    ?line expect(3, ?v1_2(genErr, undoFailed), 1, any), 
    s([{[fStatus3, 4], 4}, {[fname3,0], "ok"}]),
    ?line expect(4, ?v1_2(genErr, commitFailed), 1, any), 
% unfortunatly we don't know if we'll get undoFailed or commitFailed.
% it depends on which order the agent traverses the varbind list.
%    s([{[fStatus3, 4], 4}, {[fname3,0], "ufail"}]),
%    ?line expect(5, ?v1_2(genErr, undoFailed), 1, any),
    s([{[fStatus3, 1], 4}, {[fname3,0], "xfail"}]),
    ?line expect(6, genErr, 2, any).
    
%% Req. Klas3, Klas4
bad_return() ->
    g([[fStatus4,4],
       [fName4,4]]),
    ?line expect(4, genErr, 2, any),
    g([[fStatus4,5],
       [fName4,5]]),
    ?line expect(5, genErr, 1, any),
    g([[fStatus4,6],
       [fName4,6]]),
    ?line expect(6, genErr, 2, any),
    gn([[fStatus4,7],
       [fName4,7]]),
    ?line expect(7, genErr, 2, any),
    gn([[fStatus4,8],
       [fName4,8]]),
    ?line expect(8, genErr, 1, any),
    gn([[fStatus4,9],
       [fName4,9]]),
    ?line expect(9, genErr, 2, any).


%%%-----------------------------------------------------------------
%%% Test the implementation of standard mibs.
%%% We should *at least* try to GET all variables, just to make
%%% sure the instrumentation functions work.
%%% Note that many of the functions in the standard mib is
%%% already tested by the normal tests.
%%%-----------------------------------------------------------------



%%-----------------------------------------------------------------
%% For this test, the agent is configured for v1.
%% o  Test the counters and control objects in SNMP-STANDARD-MIB
%%-----------------------------------------------------------------
snmp_standard_mib(suite) -> [];
snmp_standard_mib(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?DBG("snmp_standard_mib -> std_mib_init", []),
    try_test(std_mib_init),

    ?DBG("snmp_standard_mib -> std_mib_a", []),
    InBadVsns = try_test(std_mib_a),
    put(vsn, v2),
    ?DBG("snmp_standard_mib -> std_mib_read", []),
    try_test(std_mib_read),
    put(vsn, v1),

    ?DBG("snmp_standard_mib -> std_mib_b (~w)", [InBadVsns]),
    Bad = try_test(std_mib_b, [InBadVsns]),
    ?DBG("snmp_standard_mib -> std_mib_read (community: 'bad community')", []),
    try_test(std_mib_read, [], [{community, "bad community"}]),
    ?DBG("snmp_standard_mib -> std_mib_write (community: 'public')", []),
    try_test(std_mib_write, [], [{community, "public"}]),
    ?DBG("snmp_standard_mib -> std_mib_asn_err", []),
    try_test(std_mib_asn_err),
    ?DBG("snmp_standard_mib -> std_mib_c (~w)", [Bad]),
    try_test(std_mib_c, [Bad]),
    ?DBG("snmp_standard_mib -> std_mib_a", []),
    try_test(standard_mib_a),
    
    ?DBG("snmp_standard_mib -> std_mib_finish", []),
    try_test(std_mib_finish),
    ?DBG("snmp_standard_mib -> std_mib_test_finish", []),
    try_test(standard_mib_test_finish, [], [{community, "bad community"}]).

%% Req. SNMP-STANDARD-MIB
standard_mib_a() ->
    ?line [OutPkts] = get_req(2, [[snmpOutPkts,0]]),
    ?line [OutPkts2] = get_req(3, [[snmpOutPkts,0]]),
    ?line OutPkts2 = OutPkts + 1,
    %% There are some more counters we could test here, but it's not that
    %% important, since they are removed from SNMPv2-MIB.
    ok.

%% Req. SNMP-STANDARD-MIB | SNMPv2-MIB
std_mib_init() ->
    %% disable authentication failure traps.  (otherwise w'd get many of
    %% them - this is also a test to see that it works).
    s([{[snmpEnableAuthenTraps,0], 2}]),
    ?line expect(1, [{[snmpEnableAuthenTraps, 0], 2}]).

%% Req. SNMP-STANDARD-MIB | SNMPv2-MIB
std_mib_finish() ->
    %% enable again
    s([{[snmpEnableAuthenTraps,0], 1}]),
    ?line expect(1, [{[snmpEnableAuthenTraps, 0], 1}]).

%% Req. SNMP-STANDARD-MIB
standard_mib_test_finish() ->
    %% force a authenticationFailure
    std_mib_write(),
    %% check that we got a trap
    ?line expect(2, trap, [1,2,3], 4, 0, []).

%% Req. SNMP-STANDARD-MIB | SNMPv2-MIB
std_mib_read() ->
    ?DBG("std_mib_read -> entry", []),
    g([[sysUpTime,0]]), % try a bad <something>; msg dropped, no reply
    ?DBG("std_mib_read -> await timeout (i.e. no reply)", []),
    ?line expect(1, timeout). % make sure we don't get a trap!


%% Req. SNMP-STANDARD-MIB | SNMPv2-MIB
std_mib_write() ->
    ?DBG("std_mib_write -> entry", []),
    s([{[sysLocation, 0], "new_value"}]).

%% Req. SNMP-STANDARD-MIB | SNMPv2-MIB
std_mib_asn_err() ->
    snmp_test_mgr:send_bytes([48,99,67,12,0,0,0,0,0,0,5]).

%%-----------------------------------------------------------------
%% For this test, the agent is configured for v2 and v3.
%% o  Test the counters and control objects in SNMPv2-MIB
%%-----------------------------------------------------------------
snmpv2_mib_2(suite) -> [];
snmpv2_mib_2(Config) when list(Config) ->
    ?LOG("snmpv2_mib_2 -> start",[]),
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),

    ?DBG("snmpv2_mib_2 -> standard mib init",[]),
    try_test(std_mib_init),

    ?DBG("snmpv2_mib_2 -> get number of (so far) bad versions",[]),
    InBadVsns = try_test(std_mib_a),

    ?DBG("snmpv2_mib_2 -> make a bad version read",[]),
    put(vsn, v1),
    try_test(std_mib_read),

    ?DBG("snmpv2_mib_2 -> bad version read",[]),
    put(vsn, v2),
    Bad = try_test(std_mib_b, [InBadVsns]),

    ?DBG("snmpv2_mib_2 -> read with bad community",[]),
    try_test(std_mib_read, [], [{community, "bad community"}]),

    ?DBG("snmpv2_mib_2 -> write with public community",[]),
    try_test(std_mib_write, [], [{community, "public"}]),

    ?DBG("snmpv2_mib_2 -> asn err",[]),
    try_test(std_mib_asn_err),

    ?DBG("snmpv2_mib_2 -> check counters",[]),
    try_test(std_mib_c, [Bad]),

    ?DBG("snmpv2_mib_2 -> get som counters",[]),
    try_test(snmpv2_mib_a),
    
    ?DBG("snmpv2_mib_2 -> enable auth traps, and await some",[]),
    try_test(std_mib_finish),

    ?DBG("snmpv2_mib_2 -> force auth failure, and await trap, "
	  "then disable auth traps",[]),
    try_test(snmpv2_mib_test_finish, [], [{community, "bad community"}]),
    
    ?LOG("snmpv2_mib_2 -> done",[]).
    
%% Req. SNMPv2-MIB
snmpv2_mib_3(suite) -> [];
snmpv2_mib_3(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),

    InBadVsns = try_test(std_mib_a),
    put(vsn, v1),
    try_test(std_mib_read),
    put(vsn, v3),
    _Bad = try_test(std_mib_b, [InBadVsns]),
    try_test(snmpv2_mib_a),

    try_test(std_mib_finish).
    
-define(authenticationFailure, [1,3,6,1,6,3,1,1,5,5]).

%% Req. SNMPv2-MIB
snmpv2_mib_test_finish() ->
    %% force a authenticationFailure
    ?DBG("ma_v2_inform -> write to std mib",[]),
    std_mib_write(),

    %% check that we got a trap
    ?DBG("ma_v2_inform -> await trap",[]),
    ?line expect(2, v2trap, [{[sysUpTime,0], any},
			     {[snmpTrapOID,0], ?authenticationFailure}]),

    %% and the the inform
    ?DBG("ma_v2_inform -> await inform",[]),
    ?line expect(2, {inform,true}, [{[sysUpTime,0], any},
				    {[snmpTrapOID,0],?authenticationFailure}]).

%% Req. SNMP-STANDARD-MIB | SNMPv2-MIB
std_mib_a() ->
    ?line [InPkts] = get_req(2, [[snmpInPkts,0]]),
    ?line [InPkts2] = get_req(3, [[snmpInPkts,0]]),
    ?line InPkts2 = InPkts + 1,

    ?line [InBadVsns] = get_req(4, [[snmpInBadVersions,0]]),
    InBadVsns.

%% Req. SNMP-STANDARD-MIB | SNMPv2-MIB
std_mib_b(InBadVsns) ->
    ?line [InBadVsns2] = get_req(1, [[snmpInBadVersions,0]]),
    ?line InBadVsns2 = InBadVsns + 1,
    ?line [InPkts] = get_req(2, [[snmpInPkts,0]]),
    ?line [InPkts2] = get_req(3, [[snmpInPkts,0]]),
    ?line InPkts2 = InPkts + 1,
    ?line [InBadCommunityNames, InBadCommunityUses, InASNErrs] =
	get_req(4, [[snmpInBadCommunityNames,0],
		    [snmpInBadCommunityUses,0],
		    [snmpInASNParseErrs, 0]]),
    {InBadCommunityNames, InBadCommunityUses, InASNErrs}.
    
%% Req. SNMP-STANDARD-MIB | SNMPv2-MIB
std_mib_c({InBadCommunityNames, InBadCommunityUses, InASNErrs}) ->
    ?line [InBadCommunityNames2, InBadCommunityUses2, InASNErrs2] =
	get_req(1, [[snmpInBadCommunityNames,0],
		    [snmpInBadCommunityUses,0],
		    [snmpInASNParseErrs, 0]]),
    ?line InBadCommunityNames2 = InBadCommunityNames + 1,
    ?line InBadCommunityUses2 = InBadCommunityUses + 1,
    ?line InASNErrs2 = InASNErrs + 1.

%% Req. SNMPv2-MIB
snmpv2_mib_a() ->
    ?line [SetSerial] = get_req(2, [[snmpSetSerialNo,0]]),
    s([{[snmpSetSerialNo,0], SetSerial}, {[sysLocation, 0], "val2"}]),
    ?line expect(3, [{[snmpSetSerialNo,0], SetSerial},
		     {[sysLocation, 0], "val2"}]),
    s([{[sysLocation, 0], "val3"}, {[snmpSetSerialNo,0], SetSerial}]),
    ?line expect(4, inconsistentValue, 2,
		 [{[sysLocation, 0], "val3"},
		  {[snmpSetSerialNo,0], SetSerial}]),
    ?line ["val2"] = get_req(5, [[sysLocation,0]]).
    
    
%%-----------------------------------------------------------------
%% o  Bad community uses/name is tested already
%%    in SNMPv2-MIB and STANDARD-MIB.
%% o  Test add/deletion of rows.
%%-----------------------------------------------------------------
snmp_community_mib(suite) -> [];
snmp_community_mib(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?line load_master_std("SNMP-COMMUNITY-MIB"),
    try_test(snmp_community_mib),
    ?line unload_master("SNMP-COMMUNITY-MIB").

snmp_community_mib_2(X) -> snmp_community_mib(X).

%% Req. SNMP-COMMUNITY-MIB
snmp_community_mib() ->
    ?INF("NOT YET IMPLEMENTED", []),
    nyi.

%%-----------------------------------------------------------------
%% o  Test engine boots / time
%%-----------------------------------------------------------------
snmp_framework_mib(suite) -> [];
snmp_framework_mib(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?line load_master_std("SNMP-FRAMEWORK-MIB"),
    try_test(snmp_framework_mib),
    ?line unload_master("SNMP-FRAMEWORK-MIB").

snmp_framework_mib_2(X) -> snmp_framework_mib(X).

snmp_framework_mib_3(suite) -> [];
snmp_framework_mib_3(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    try_test(snmp_framework_mib).


%% Req. SNMP-FRAMEWORK-MIB
snmp_framework_mib() ->
    ?line ["agentEngine"] = get_req(1, [[snmpEngineID,0]]),
    ?line [EngineTime] = get_req(2, [[snmpEngineTime,0]]),
    sleep(5000),
    ?line [EngineTime2] = get_req(3, [[snmpEngineTime,0]]),
    if 
	EngineTime+7 < EngineTime2 ->
	    ?line ?FAIL({too_large_diff, EngineTime, EngineTime2});
	EngineTime+4 > EngineTime2 ->
	    ?line ?FAIL({too_large_diff, EngineTime, EngineTime2});
	true -> ok
    end,
    ?line case get_req(4, [[snmpEngineBoots,0]]) of
	      [Boots] when integer(Boots) -> ok;
	      Else -> ?FAIL(Else)
	  end,
    ok.

%%-----------------------------------------------------------------
%% o  Test the counters
%%-----------------------------------------------------------------
snmp_mpd_mib_3(suite) -> [];
snmp_mpd_mib_3(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    UnknownPDUHs = try_test(snmp_mpd_mib_a),
    try_test(snmp_mpd_mib_b, [], [{context_engine_id, "bad engine"}]),
    try_test(snmp_mpd_mib_c, [UnknownPDUHs]).
    

%% Req. SNMP-MPD-MIB
snmp_mpd_mib_a() ->
    ?line [UnknownSecs, InvalidMsgs] =
	get_req(1, [[snmpUnknownSecurityModels,0],
		    [snmpInvalidMsgs,0]]),
    Pdu = #pdu{type = 'get-request',
	       request_id = 23,
	       error_status = noError,
	       error_index = 0,
	       varbinds = []},
    SPdu = #scopedPdu{contextEngineID = "agentEngine",
		      contextName = "",
		      data = Pdu},
    ?line SPDUBytes = snmp_pdus:enc_scoped_pdu(SPdu),
    V3Hdr1 = #v3_hdr{msgID = 21,
		     msgMaxSize = 484,
		     msgFlags = [7],
		     msgSecurityModel = 23,  % bad sec model
		     msgSecurityParameters = []},
    V3Hdr2 = #v3_hdr{msgID = 21,
		     msgMaxSize = 484,
		     msgFlags = [6], % bad flag combination
		     msgSecurityModel = 3,
		     msgSecurityParameters = []},
    Message1 = #message{version = 'version-3', vsn_hdr = V3Hdr1,
			data = SPDUBytes},
    Message2 = #message{version = 'version-3', vsn_hdr = V3Hdr2,
			data = SPDUBytes},
    ?line MsgBytes1 = snmp_pdus:enc_message_only(Message1),
    ?line MsgBytes2 = snmp_pdus:enc_message_only(Message2),
    snmp_test_mgr:send_bytes(MsgBytes1),
    snmp_test_mgr:send_bytes(MsgBytes2),

    ?line [UnknownSecs2, InvalidMsgs2, UnknownPDUHs] =
	get_req(1, [[snmpUnknownSecurityModels,0],
		    [snmpInvalidMsgs,0],
		    [snmpUnknownPDUHandlers, 0]]),
    ?line UnknownSecs2 = UnknownSecs + 1,
    ?line InvalidMsgs2 = InvalidMsgs + 1,
    UnknownPDUHs.

-define(snmpUnknownPDUHandlers_instance, [1,3,6,1,6,3,11,2,1,3,0]).
snmp_mpd_mib_b() ->
    g([[sysUpTime,0]]),
    ?line expect(1, report, [{?snmpUnknownPDUHandlers_instance, any}]).
    

snmp_mpd_mib_c(UnknownPDUHs) ->
    ?line [UnknownPDUHs2] = get_req(1, [[snmpUnknownPDUHandlers, 0]]),
    ?line UnknownPDUHs2 = UnknownPDUHs + 1.


snmp_target_mib(suite) -> [];
snmp_target_mib(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?line load_master_std("SNMP-TARGET-MIB"),
    try_test(snmp_target_mib),
    ?line unload_master("SNMP-TARGET-MIB").

snmp_target_mib_2(X) -> snmp_target_mib(X).

snmp_target_mib_3(X) -> snmp_target_mib(X).

snmp_target_mib() ->
    ?INF("NOT YET IMPLEMENTED", []),
    nyi.

snmp_notification_mib(suite) -> [];
snmp_notification_mib(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?line load_master_std("SNMP-NOTIFICATION-MIB"),
    try_test(snmp_notification_mib),
    ?line unload_master("SNMP-NOTIFICATION-MIB").

snmp_notification_mib_2(X) -> snmp_notification_mib(X).

snmp_notification_mib_3(X) -> snmp_notification_mib(X).

snmp_notification_mib() ->
    ?INF("NOT YET IMPLEMENTED", []),
    nyi.

%%-----------------------------------------------------------------
%% o  add/delete views and try them
%% o  try boundaries
%%-----------------------------------------------------------------
snmp_view_based_acm_mib(suite) -> [];
snmp_view_based_acm_mib(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?line load_master_std("SNMP-VIEW-BASED-ACM-MIB"),
    ?line load_master("Test2"),
    snmp_view_based_acm_mib(),
    ?line unload_master("Test2"),
    ?line unload_master("SNMP-VIEW-BASED-ACM-MIB").

snmp_view_based_acm_mib_2(X) -> snmp_view_based_acm_mib(X).

snmp_view_based_acm_mib_3(X) -> snmp_view_based_acm_mib(X).

snmp_view_based_acm_mib() ->
    snmpa:verbosity(net_if,trace),
    snmpa:verbosity(master_agent,trace),
    ?LOG("start snmp_view_based_acm_mib test",[]),
    %% The user "no-rights" is present in USM, and is mapped to security
    %% name 'no-rights", which is not present in VACM.
    %% So, we'll add rights for it, try them and delete them.
    %% We'll give "no-rights" write access to tDescr.0 and read access
    %% to tDescr2.0
    %% These are the options we'll use to the mgr
    Opts = [{user, "no-rights"}, {community, "no-rights"}],
    %% Find the valid secmodel, and one invalid secmodel.
    {SecMod, InvSecMod} = 
	case get(vsn) of
	    v1 -> {?SEC_V1, ?SEC_V2C};
	    v2 -> {?SEC_V2C, ?SEC_USM};
	    v3 -> {?SEC_USM, ?SEC_V1}
	end,
    ?DBG("assign rights for 'no-rights'",[]),
    ?line try_test(use_no_rights, [], Opts),

    %% Now, add a mapping from "no-rights" -> "no-rights-group"
    GRow1Status = [vacmSecurityToGroupStatus,[SecMod, 9,"no-rights"]],
    GRow1 = 
	[{[vacmGroupName, [SecMod, 9,"no-rights"]], "no-rights-group"},
	 {GRow1Status, ?createAndGo}],
    ?DBG("set '~p'",[GRow1]),
    ?line try_test(do_set, [GRow1]),

    ?DBG("assign rights for 'no-rights'",[]),
    ?line try_test(use_no_rights, [], Opts),

    %% Create a mapping for another sec model, and make sure it dosn't
    %% give us access
    GRow2Status = [vacmSecurityToGroupStatus,[InvSecMod, 9,"no-rights"]],
    GRow2 = [{[vacmGroupName, [InvSecMod, 9, "no-rights"]], "initial"},
	     {GRow2Status, ?createAndGo}],

    ?DBG("set '~p'",[GRow2]),
    ?line try_test(do_set, [GRow2]),

    ?DBG("assign rights for 'no-rights'",[]),
    ?line try_test(use_no_rights, [], Opts),

    %% Delete that row
    ?line try_test(del_row, [GRow2Status]),
    
    RVName = "rv_name",
    WVName = "wv_name",

    %% Access row
    ARow1Idx = [15 | "no-rights-group"] ++ [0, ?SEC_ANY, 1],
    ARow1Status = [vacmAccessStatus, ARow1Idx],
    ARow1 = [{[vacmAccessContextMatch, ARow1Idx], 1},
	     {[vacmAccessReadViewName, ARow1Idx], RVName},
	     {[vacmAccessWriteViewName, ARow1Idx], WVName},
	     {ARow1Status, ?createAndGo}],
    
    %% This access row would give acces, if InvSecMod was valid.
    ARow2Idx = [15 | "no-rights-group"] ++ [0, InvSecMod, 1],
    ARow2Status = [vacmAccessStatus, ARow2Idx],
    ARow2 = [{[vacmAccessContextMatch, ARow2Idx], 1},
	     {[vacmAccessReadViewName, ARow2Idx], "internet"},
	     {[vacmAccessWriteViewName, ARow2Idx], "internet"},
	     {ARow2Status, ?createAndGo}],
    
    ?line try_test(do_set, [ARow2]),

    ?line try_test(use_no_rights, [], Opts),

    %% Delete that row
    ?line try_test(del_row, [ARow2Status]),
    

    %% Add valid row
    ?line try_test(do_set, [ARow1]),

    ?line try_test(use_no_rights, [], Opts),

    %% Create the view family
    VRow1Idx = mk_ln(RVName) ++ mk_ln(?xDescr),         % object access
    VRow2Idx = mk_ln(RVName) ++ mk_ln(?xDescr2 ++ [0]), % instance access
    VRow3Idx = mk_ln(WVName) ++ mk_ln(?xDescr),         % object access
    VRow4Idx = mk_ln(WVName) ++ mk_ln(?xDescr ++ [0]),  % instance access
    VRow1Status = [vacmViewTreeFamilyStatus, VRow1Idx],
    VRow2Status = [vacmViewTreeFamilyStatus, VRow2Idx],
    VRow3Status = [vacmViewTreeFamilyStatus, VRow3Idx],
    VRow4Status = [vacmViewTreeFamilyStatus, VRow4Idx],
    
    ?line try_test(add_row, [VRow1Status]),
    ?line try_test(add_row, [VRow2Status]),
    ?line try_test(add_row, [VRow3Status]),

    %% We're supposed to have access now...
    ?line try_test(use_rights, [], Opts),

    %% Change Row3 to Row4
    ?line try_test(del_row, [VRow3Status]),
    ?line try_test(add_row, [VRow4Status]),

    %% We should still have access...
    ?line try_test(use_rights, [], Opts),

    %% Delete rows
    ?line try_test(del_row, [GRow1Status]),
    
    ?line try_test(use_no_rights, [], Opts),

    %% Delete rest of rows
    ?line try_test(del_row, [ARow1Status]),
    ?line try_test(del_row, [VRow1Status]),
    ?line try_test(del_row, [VRow2Status]),
    ?line try_test(del_row, [VRow4Status]),

    ?line try_test(use_no_rights, [], Opts),
    snmpa:verbosity(master_agent,log).

do_set(Row) ->
    s(Row),
    expect(1, Row).
    
add_row(RowStatus) ->
    s([{RowStatus, ?createAndGo}]),
    expect(1, [{RowStatus, ?createAndGo}]).

del_row(RowStatus) ->
    s([{RowStatus, ?destroy}]),
    expect(1, [{RowStatus, ?destroy}]).
    
    

use_no_rights() ->
    g([[xDescr,0]]),
    ?v1_2_3(expect(11, noSuchName, 1, any),
	    expect(12, [{[xDescr,0], noSuchObject}]),
	    expect(13, authorizationError, 1, any)),
    g([[xDescr2,0]]),
    ?v1_2_3(expect(21, noSuchName, 1, any),
	    expect(22, [{[xDescr2,0], noSuchObject}]),
	    expect(23, authorizationError, 1, any)),
    gn([[xDescr]]),
    ?v1_2_3(expect(31, noSuchName, 1, any),
	    expect(32, [{[xDescr], endOfMibView}]),
	    expect(33, authorizationError, 1, any)),
    s([{[xDescr,0], "tryit"}]),
    ?v1_2_3(expect(41, noSuchName, 1, any),
	    expect(42, noAccess, 1, any),
	    expect(43, authorizationError, 1, any)).


use_rights() ->
    g([[xDescr,0]]),
    expect(1, [{[xDescr,0], any}]),
    g([[xDescr2,0]]),
    expect(2, [{[xDescr2,0], any}]),
    s([{[xDescr,0], "tryit"}]),
    expect(3, noError, 0, any),
    g([[xDescr,0]]),
    expect(4, [{[xDescr,0], "tryit"}]).

mk_ln(X) ->
    [length(X) | X].

%%-----------------------------------------------------------------
%% o  add/delete users and try them
%% o  test all secLevels
%% o  test all combinations of protocols
%% o  try bad ops; check counters
%%-----------------------------------------------------------------
snmp_user_based_sm_mib_3(suite) -> [];
snmp_user_based_sm_mib_3(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    _AgentDir = ?config(agent_dir, Config),
    ?line load_master_std("SNMP-USER-BASED-SM-MIB"),

    %% The newUser used here already has VACM access.
    
    %% Add a new user in the simplest way; just createAndGo
    try_test(v3_sync, [[{usm_add_user1, []}]],
	[{sec_level, authPriv}, {user, "privDES"}]),

    %% Try to use the new user
    ?line load_master("Test2"),
    try_test(v3_sync, [[{usm_use_user, []}]],
	[{sec_level, authPriv}, {user, "newUser"}]),
    ?line unload_master("Test2"),

    ShaKey1 = snmp:passwd2localized_key(sha, "new sha password", "agentEngine"),
    DesKey1 = lists:sublist(ShaKey1, 16),

    %% Change the new user's keys - 1
    try_test(v3_sync, [[{usm_key_change1, [ShaKey1, DesKey1]}]],
	[{sec_level, authPriv}, {user, "newUser"}]),

    %% Try to use the new keys
    MgrDir = ?config(mgr_dir, Config),
    ?line rewrite_usm_mgr(MgrDir, ShaKey1, DesKey1),
    ?line load_master("Test2"),
    try_test(v3_sync, [[{usm_use_user, []}]],
	[{sec_level, authPriv}, {user, "newUser"}]),
    ?line unload_master("Test2"),

    ShaKey2 = snmp:passwd2localized_key(sha, "newer password", "agentEngine"),
    DesKey2 = lists:sublist(ShaKey2, 16),

    %% Change the new user's keys - 2
    ?line try_test(v3_sync, 
	      [[{usm_key_change2, [ShaKey1, DesKey1, ShaKey2, DesKey2]}]],
	      [{sec_level, authPriv}, {user, "newUser"}]),

    %% Try to use the new keys
    reset_usm_mgr(MgrDir),
    ?line rewrite_usm_mgr(MgrDir, ShaKey2, DesKey2),
    ?line load_master("Test2"),
    ?line try_test(v3_sync, [[{usm_use_user, []}]],
	      [{sec_level, authPriv}, {user, "newUser"}]),
    ?line unload_master("Test2"),
    reset_usm_mgr(MgrDir),

    %% Change the new user's keys - 3
    ?line try_test(v3_sync,
	      [[{usm_key_change3, [ShaKey2, DesKey2, ShaKey1, DesKey1]}]],
	      [{sec_level, authPriv}, {user, "privDES"}]),

    %% Try to use the new keys
    ?line rewrite_usm_mgr(MgrDir, ShaKey1, DesKey1),
    ?line load_master("Test2"),
    try_test(v3_sync, [[{usm_use_user, []}]],
	[{sec_level, authPriv}, {user, "newUser"}]),
    ?line unload_master("Test2"),
    reset_usm_mgr(MgrDir),

    %% Try some read requests
    ?line try_test(v3_sync, [[{usm_read, []}]],
	      [{sec_level, authPriv}, {user, "privDES"}]),

    %% Delete the new user
    ?line try_test(v3_sync, [[{usm_del_user, []}]],
	      [{sec_level, authPriv}, {user, "privDES"}]),

    %% Try some bad requests
    ?line try_test(v3_sync, [[{usm_bad, []}]],
	      [{sec_level, authPriv}, {user, "privDES"}]),

    ?line unload_master("SNMP-USER-BASED-SM-MIB").

-define(usmUserSecurityName, [1,3,6,1,6,3,15,1,2,2,1,3]).

usm_add_user1() ->
    NewRowIndex = [11,"agentEngine", 7, "newUser"],
    RowPointer = ?usmUserSecurityName ++ [11|"agentEngine"] ++ [7|"privDES"],
    Vbs1  = [{[usmUserCloneFrom, NewRowIndex], RowPointer},
	     {[usmUserStatus, NewRowIndex], ?createAndGo}],
    ?line s(Vbs1),
    ?line expect(1, Vbs1),
    ok.
    
usm_use_user() ->
    v2_proc().


%% Change own public keys
usm_key_change1(ShaKey, DesKey) ->
    NewRowIndex = [11,"agentEngine", 7, "newUser"],
    ShaKeyChange = snmp_user_based_sm_mib:mk_key_change(sha,
							"passwd_shaxxxxxxxxxx",
							ShaKey),
    DesKeyChange = snmp_user_based_sm_mib:mk_key_change(sha,
							"passwd_desxxxxxx",
							DesKey),
    Vbs1 = [{[usmUserAuthKeyChange, NewRowIndex], ShaKeyChange},
	    {[usmUserPrivKeyChange, NewRowIndex], DesKeyChange}],
    s(Vbs1),
    ?line expect(1, Vbs1).
    
%% Change own private keys
usm_key_change2(OldShaKey, OldDesKey, ShaKey, DesKey) ->
    NewRowIndex = [11,"agentEngine", 7, "newUser"],
    ShaKeyChange = snmp_user_based_sm_mib:mk_key_change(sha,
							OldShaKey,
							ShaKey),
    DesKeyChange = snmp_user_based_sm_mib:mk_key_change(sha,
							OldDesKey,
							DesKey),
    Vbs1 = [{[usmUserOwnAuthKeyChange, NewRowIndex], ShaKeyChange},
	    {[usmUserOwnPrivKeyChange, NewRowIndex], DesKeyChange}],
    s(Vbs1),
    ?line expect(1, Vbs1).
    
%% Change other's public keys
usm_key_change3(OldShaKey, OldDesKey, ShaKey, DesKey) ->
    NewRowIndex = [11,"agentEngine", 7, "newUser"],
    ShaKeyChange = snmp_user_based_sm_mib:mk_key_change(sha,
							OldShaKey,
							ShaKey),
    DesKeyChange = snmp_user_based_sm_mib:mk_key_change(sha,
							OldDesKey,
							DesKey),
    Vbs1 = [{[usmUserOwnAuthKeyChange, NewRowIndex], ShaKeyChange}],
    s(Vbs1),
    ?line expect(1, noAccess, 1, any),
    Vbs2 = [{[usmUserOwnPrivKeyChange, NewRowIndex], DesKeyChange}],
    s(Vbs2),
    ?line expect(2, noAccess, 1, any),
    
    
    Vbs3 = [{[usmUserAuthKeyChange, NewRowIndex], ShaKeyChange},
	    {[usmUserPrivKeyChange, NewRowIndex], DesKeyChange}],
    s(Vbs3),
    ?line expect(1, Vbs3).

usm_read() ->
    NewRowIndex = [11,"agentEngine", 7, "newUser"],
    ?line g([[usmUserSecurityName, NewRowIndex],
	     [usmUserCloneFrom, NewRowIndex],
	     [usmUserAuthKeyChange, NewRowIndex],
	     [usmUserOwnAuthKeyChange, NewRowIndex],
	     [usmUserPrivKeyChange, NewRowIndex],
	     [usmUserOwnPrivKeyChange, NewRowIndex]]),
    ?line expect(1, 
		 [{[usmUserSecurityName, NewRowIndex], "newUser"},
		  {[usmUserCloneFrom, NewRowIndex], [0,0]},
		  {[usmUserAuthKeyChange, NewRowIndex], ""},
		  {[usmUserOwnAuthKeyChange, NewRowIndex], ""},
		  {[usmUserPrivKeyChange, NewRowIndex], ""},
		  {[usmUserOwnPrivKeyChange, NewRowIndex], ""}]),
    ok.
    
    

usm_del_user() ->
    NewRowIndex = [11,"agentEngine", 7, "newUser"],
    Vbs1  = [{[usmUserStatus, NewRowIndex], ?destroy}],
    ?line s(Vbs1),
    ?line expect(1, Vbs1),
    ok.

-define(usmUserCloneFrom, [1,3,6,1,6,3,15,1,2,2,1,4]).

-define(usmNoAuthProtocol, [1,3,6,1,6,3,10,1,1,1]).

-define(usmHMACMD5AuthProtocol, [1,3,6,1,6,3,10,1,1,2]).

-define(usmHMACSHAAuthProtocol, [1,3,6,1,6,3,10,1,1,3]).

-define(usmNoPrivProtocol, [1,3,6,1,6,3,10,1,2,1]).

-define(usmDESPrivProtocol, [1,3,6,1,6,3,10,1,2,2]).

usm_bad() ->
    NewRowIndex = [11,"agentEngine", 7, "newUser"],
    RowPointer1 = ?usmUserSecurityName ++ [11|"agentEngine"] ++ [7|"privDOS"],
    Vbs1  = [{[usmUserCloneFrom, NewRowIndex], RowPointer1},
	     {[usmUserStatus, NewRowIndex], ?createAndGo}],
    ?line s(Vbs1),
    ?line expect(1, inconsistentName, 1, any),

    RowPointer2 = ?usmUserCloneFrom ++ [11|"agentEngine"] ++ [7|"privDES"],
    Vbs2  = [{[usmUserCloneFrom, NewRowIndex], RowPointer2},
	     {[usmUserStatus, NewRowIndex], ?createAndGo}],
    ?line s(Vbs2),
    ?line expect(2, wrongValue, 1, any),

    RowPointer3 = ?usmUserSecurityName ++ [11|"agentEngine"] ++ [7|"privDES"],
    Vbs3  = [{[usmUserCloneFrom, NewRowIndex], RowPointer3},
	     {[usmUserStatus, NewRowIndex], ?createAndGo}],
    ?line s(Vbs3),
    ?line expect(3, Vbs3),
    ?line s([{[usmUserAuthProtocol, NewRowIndex], ?usmNoAuthProtocol}]),
    ?line expect(4, inconsistentValue, 1, any),
    ?line s([{[usmUserAuthProtocol, NewRowIndex], ?usmHMACMD5AuthProtocol}]),
    ?line expect(5, inconsistentValue, 1, any),
    ?line s([{[usmUserAuthProtocol, NewRowIndex], ?usmDESPrivProtocol}]),
    ?line expect(6, wrongValue, 1, any),
    ?line s([{[usmUserPrivProtocol, NewRowIndex], ?usmHMACSHAAuthProtocol}]),
    ?line expect(7, wrongValue, 1, any),

    Vbs4  = [{[usmUserStatus, NewRowIndex], ?destroy}],
    ?line s(Vbs4),
    ?line expect(1, Vbs4),

    ok.
    

%%-----------------------------------------------------------------
%% Loop through entire MIB, to make sure that all instrum. funcs
%% works.
%% Load all std mibs that are not loaded by default.
%%-----------------------------------------------------------------
loop_mib(suite) -> [];
loop_mib(Config) when list(Config) ->
    ?LOG("loop_mib -> initiate case",[]),
    %% snmpa:verbosity(master_agent,debug),
    %% snmpa:verbosity(mib_server,info),
    {SaNode, MgrNode, MibDir} = init_case(Config),
    ?DBG("loop_mib -> ~n"
	   "\tSaNode:  ~p~n"
	   "\tMgrNode: ~p~n"
	   "\tMibDir:  ~p",[SaNode, MgrNode, MibDir]),
    ?DBG("loop_mib -> load mib SNMP-COMMUNITY-MIB",[]),
    ?line load_master_std("SNMP-COMMUNITY-MIB"),
    ?DBG("loop_mib -> load mib SNMP-MPD-MIB",[]),
    ?line load_master_std("SNMP-MPD-MIB"),
    ?DBG("loop_mib -> load mib SNMP-TARGET-MIB",[]),
    ?line load_master_std("SNMP-TARGET-MIB"),
    ?DBG("loop_mib -> load mib SNMP-NOTIFICATION-MIB",[]),
    ?line load_master_std("SNMP-NOTIFICATION-MIB"),
    ?DBG("loop_mib -> load mib SNMP-FRAMEWORK-MIB",[]),
    ?line load_master_std("SNMP-FRAMEWORK-MIB"),
    ?DBG("loop_mib -> load mib SNMP-VIEW-BASED-ACM-MIB",[]),
    ?line load_master_std("SNMP-VIEW-BASED-ACM-MIB"),
    ?DBG("loop_mib -> try",[]),
    try_test(loop_mib_1),
    ?DBG("loop_mib -> unload mib SNMP-COMMUNITY-MIB",[]),
    ?line unload_master("SNMP-COMMUNITY-MIB"),
    ?DBG("loop_mib -> unload mib SNMP-MPD-MIB",[]),
    ?line unload_master("SNMP-MPD-MIB"),
    ?DBG("loop_mib -> unload mib SNMP-TARGET-MIB",[]),
    ?line unload_master("SNMP-TARGET-MIB"),
    ?DBG("loop_mib -> unload mib SNMP-NOTIFICATION-MIB",[]),
    ?line unload_master("SNMP-NOTIFICATION-MIB"),
    ?DBG("loop_mib -> unload mib SNMP-FRAMEWORK-MIB",[]),
    ?line unload_master("SNMP-FRAMEWORK-MIB"),
    ?DBG("loop_mib -> unload mib SNMP-VIEW-BASED-ACM-MIB",[]),
    ?line unload_master("SNMP-VIEW-BASED-ACM-MIB"),
    %% snmpa:verbosity(master_agent,log),
    %% snmpa:verbosity(mib_server,silence),
    ?LOG("loop_mib -> done",[]).
    

loop_mib_2(suite) -> [];
loop_mib_2(Config) when list(Config) ->
    ?LOG("loop_mib_2 -> initiate case",[]),
    {SaNode, MgrNode, MibDir} = init_case(Config),
    ?DBG("loop_mib_2 -> ~n"
	   "\tSaNode:  ~p~n"
	   "\tMgrNode: ~p~n"
	   "\tMibDir:  ~p",[SaNode, MgrNode, MibDir]),
    ?DBG("loop_mib_2 -> load mibs",[]),
    ?line load_master_std("SNMP-COMMUNITY-MIB"),
    ?line load_master_std("SNMP-MPD-MIB"),
    ?line load_master_std("SNMP-TARGET-MIB"),
    ?line load_master_std("SNMP-NOTIFICATION-MIB"),
    ?line load_master_std("SNMP-FRAMEWORK-MIB"),
    ?line load_master_std("SNMP-VIEW-BASED-ACM-MIB"),
    try_test(loop_mib_2),
    ?DBG("loop_mib_2 -> unload mibs",[]),
    ?line unload_master("SNMP-COMMUNITY-MIB"),
    ?line unload_master("SNMP-MPD-MIB"),
    ?line unload_master("SNMP-TARGET-MIB"),
    ?line unload_master("SNMP-NOTIFICATION-MIB"),
    ?line unload_master("SNMP-FRAMEWORK-MIB"),
    ?line unload_master("SNMP-VIEW-BASED-ACM-MIB"),
    ?LOG("loop_mib_2 -> done",[]).


loop_mib_3(suite) -> [];
loop_mib_3(Config) when list(Config) ->
    ?LOG("loop_mib_3 -> initiate case",[]),
    {SaNode, MgrNode, MibDir} = init_case(Config),
    ?DBG("loop_mib_3 -> ~n"
	   "\tSaNode:  ~p~n"
	   "\tMgrNode: ~p~n"
	   "\tMibDir:  ~p",[SaNode, MgrNode, MibDir]),
    ?DBG("loop_mib_3 -> load mibs",[]),
    ?line load_master_std("SNMP-TARGET-MIB"),
    ?line load_master_std("SNMP-NOTIFICATION-MIB"),
    ?line load_master_std("SNMP-VIEW-BASED-ACM-MIB"),
    ?line load_master_std("SNMP-USER-BASED-SM-MIB"),
    try_test(loop_mib_2),
    ?DBG("loop_mib_3 -> unload mibs",[]),
    ?line unload_master("SNMP-TARGET-MIB"),
    ?line unload_master("SNMP-NOTIFICATION-MIB"),
    ?line unload_master("SNMP-VIEW-BASED-ACM-MIB"),
    ?line unload_master("SNMP-USER-BASED-SM-MIB"),
    ?LOG("loop_mib_3 -> done",[]).


%% Req. As many mibs all possible
loop_mib_1() ->
    ?DBG("loop_mib_1 -> entry",[]),
    N = loop_it_1([1,1], 0),
    io:format(user, "found ~w varibles\n", [N]),
    ?line N = if N < 100 -> 100;
		 true -> N
	      end.
	    

loop_it_1(Oid, N) ->
    ?DBG("loop_it_1 -> entry with~n"
	   "\tOid: ~p~n"
	   "\tN:   ~p",[Oid,N]),
    case get_next_req([Oid]) of
	#pdu{type='get-response', error_status=noError, error_index=0,
	     varbinds=[#varbind{oid = NOid,value = Value}]} when NOid > Oid ->
	    ?DBG("loop_it_1 -> ~n"
		   "\tNOid:  ~p~n"
		   "\tValue: ~p",[NOid,Value]),
	    ?line [Value2] = get_req(1, [NOid]), % must not be same
	    ?DBG("loop_it_1 -> ~n"
		   "\tValue2: ~p",[Value2]),
	    loop_it_1(NOid, N+1);
	#pdu{type='get-response', error_status=noSuchName, error_index=1,
	     varbinds=[_]} ->
	    ?DBG("loop_it_1 -> done",[]),
	    N;

	#pdu{type = Type, error_status = Err, error_index = Idx,
	     varbinds = Vbs} ->
	    exit({unexpected_pdu, ?LINE, Type, Err, Idx, Vbs})
    end.
	    
%% Req. As many mibs all possible
loop_mib_2() ->
    ?DBG("loop_mib_1 -> entry",[]),
    N = loop_it_2([1,1], 0),
    io:format(user, "found ~w varibles\n", [N]),
    ?line N = if N < 100 -> 100;
		 true -> N
	      end.
    

loop_it_2(Oid, N) ->
    ?DBG("loop_it_2 -> entry with~n"
	   "\tOid: ~p~n"
	   "\tN:   ~p",[Oid,N]),
    case get_next_req([Oid]) of
	#pdu{type='get-response', error_status=noError, error_index=0,
	     varbinds=[#varbind{oid = NOid, value = endOfMibView}]} ->
	    ?DBG("loop_it_2 -> ~n"
		   "\tNOid: ~p",[NOid]),
	    N;
	#pdu{type='get-response', error_status=noError, error_index=0,
	     varbinds=[#varbind{oid = NOid,value = Value}]} when NOid > Oid ->
	    ?DBG("loop_it_2 -> ~n"
		   "\tNOid:  ~p~n"
		   "\tValue: ~p",[NOid,Value]),
	    ?line [Value2] = get_req(1, [NOid]), % must not be same
	    ?DBG("loop_it_2 -> ~n"
		   "\tValue2: ~p",[Value2]),
	    loop_it_2(NOid, N+1)
    end.
	    

%%%-----------------------------------------------------------------
%%% Testing of reported bugs and other tickets.
%%%-----------------------------------------------------------------





%% These are (ticket) test cases where the initiation has to be done
%% individually.

%%-----------------------------------------------------------------
%% Ticket: OTP-1128
%% Slogan: Bug in handling of createAndWait set-requests.
%%-----------------------------------------------------------------
otp_1128(suite) -> [];
otp_1128(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?line load_master("OLD-SNMPEA-MIB"),
    ?line init_old(),
    try_test(otp_1128),
    ?line unload_master("OLD-SNMPEA-MIB").

otp_1128_2(X) -> otp_1128(X).

otp_1128_3(X) -> otp_1128(X).

otp_1128() ->
    io:format("Testing bug reported in ticket OTP-1128...~n"),

    NewKeyc3 = [intCommunityViewIndex,get(mip),is("test")],
    NewKeyc4 = [intCommunityAccess,get(mip),is("test")],
    NewKeyc5 = [intCommunityStatus,get(mip),is("test")],

    s([{NewKeyc5, ?createAndWait}, {NewKeyc4, 2}]),
    ?line expect(28, [{NewKeyc5, ?createAndWait}, {NewKeyc4, 2}]),
    g([NewKeyc5]),
    ?line expect(29, [{NewKeyc5, ?notReady}]),
    s([{NewKeyc5, ?active}, {NewKeyc3, 2}]),
    ?line expect(30, [{NewKeyc5, ?active}, {NewKeyc3, 2}]),
    g([NewKeyc5]),
    ?line expect(31, [{NewKeyc5, ?active}]),
    s([{NewKeyc5, ?destroy}]),
    ?line expect(32, [{NewKeyc5, ?destroy}]).

%%-----------------------------------------------------------------
%% Ticket: OTP-1129, OTP-1169
%% Slogan: snmpa:int_to_enum crashes on bad oids
%%-----------------------------------------------------------------
otp_1129(suite) -> [];
otp_1129(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?line load_master("Klas3"),
    try_test(otp_1129_i, [node()]),
    ?line unload_master("Klas3").

otp_1129_2(X) -> otp_1129(X).

otp_1129_3(X) -> otp_1129(X).

otp_1129_i(MaNode) ->
    io:format("Testing bug reported in ticket OTP-1129...~n"),
    false = rpc:call(MaNode, snmp, int_to_enum, [iso, 1]),
    false = rpc:call(MaNode, snmp, int_to_enum, [isox, 1]).

%%-----------------------------------------------------------------
%% Ticket: OTP-1131
%% Slogan: Agent crashes / erlang node halts if RowIndex in a
%%         setrequest is of bad type, e.g. an INDEX {INTEGER},
%%         and RowIdenx [3,2].
%%-----------------------------------------------------------------
otp_1131(suite) -> [];
otp_1131(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?line load_master("Klas1"),
    try_test(otp_1131),
    ?line unload_master("Klas1").

otp_1131_2(X) -> otp_1131(X).

otp_1131_3(X) -> otp_1131(X).

otp_1131() ->
    io:format("Testing bug reported in ticket OTP-1131...~n"),
    s([{[friendsEntry, [2, 3, 1]], s, "kompis3"},
       {[friendsEntry, [3, 3, 1]], i, ?createAndGo}]),
    ?line expect(1, ?v1_2(noSuchName, noCreation), 2, any).


%%-----------------------------------------------------------------
%% Ticket: OTP-1162
%% Slogan: snmp_agent can't handle wrongValue from instrum.func
%%-----------------------------------------------------------------
otp_1162(suite) -> [];
otp_1162(Config) when list(Config) ->
    {SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?line {ok, SA} = start_subagent(SaNode, ?sa, "SA-MIB"),
    try_test(otp_1162),
    stop_subagent(SA).

otp_1162_2(X) -> otp_1162(X).

otp_1162_3(X) -> otp_1162(X).

otp_1162() ->
    s([{[sa, [2,0]], 6}]), % wrongValue (i is_set_ok)
    ?line expect(1, ?v1_2(badValue, wrongValue), 1, any).


%%-----------------------------------------------------------------
%% Ticket: OTP-1222
%% Slogan: snmp agent crash if faulty index is returned from instrum
%%-----------------------------------------------------------------
otp_1222(suite) -> [];
otp_1222(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?line load_master("Klas3"),
    ?line load_master("Klas4"),
    try_test(otp_1222),
    ?line unload_master("Klas3"),
    ?line unload_master("Klas4").

otp_1222_2(X) -> otp_1222(X).

otp_1222_3(X) -> otp_1222(X).

otp_1222() ->
    io:format("Testing bug reported in ticket OTP-1222...~n"),
    s([{[fStatus4,1], 4}, {[fName4,1], 1}]),
    ?line expect(1, genErr, 0, any),
    s([{[fStatus4,2], 4}, {[fName4,2], 1}]),
    ?line expect(2, genErr, 0, any).

%%-----------------------------------------------------------------
%% Ticket: OTP-1298
%% Slogan: Negative INTEGER values are treated as positive.
%%-----------------------------------------------------------------
otp_1298(suite) -> [];
otp_1298(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?line load_master("Klas2"),
    try_test(otp_1298),
    ?line unload_master("Klas2").

otp_1298_2(X) -> otp_1298(X).

otp_1298_3(X) -> otp_1298(X).

otp_1298() ->
    io:format("Testing bug reported in ticket OTP-1298...~n"),
    s([{[fint,0], -1}]),
    ?line expect(1298, [{[fint,0], -1}]).
    

%%-----------------------------------------------------------------
%% Ticket: OTP-1331
%% Slogan: snmp_generic should return noError when deleting non-ex row
%%-----------------------------------------------------------------
otp_1331(suite) -> [];
otp_1331(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?line load_master("OLD-SNMPEA-MIB"),
    ?line init_old(),
    try_test(otp_1331),
    ?line unload_master("OLD-SNMPEA-MIB").

otp_1331_2(X) -> otp_1331(X).

otp_1331_3(X) -> otp_1331(X).

otp_1331() ->
    NewKeyc5 = [intCommunityStatus,[127,32,0,0],is("test")],
    s([{NewKeyc5, ?destroy}]),
    ?line expect(1, [{NewKeyc5, ?destroy}]).


%%-----------------------------------------------------------------
%% Ticket: OTP-1338
%% Slogan: snmp bug in initialisation of default values for mnesia tabs
%%-----------------------------------------------------------------
otp_1338(suite) -> [];
otp_1338(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?line load_master("Klas2"),
    try_test(otp_1338),
    ?line unload_master("Klas2").

otp_1338_2(X) -> otp_1338(X).

otp_1338_3(X) -> otp_1338(X).

otp_1338() ->
    s([{[kStatus2, 7], i, ?createAndGo}]),
    ?line expect(1, [{[kStatus2, 7], ?createAndGo}]),
    g([[kName2, 7]]),
    ?line expect(2, [{[kName2, 7], "JJJ"}]).

%%-----------------------------------------------------------------
%% Ticket: OTP-1342
%% Slogan: default impl of snmp table can't handle bad index access,
%%         Set when INDEX is read-write gets into an infinite loop!
%%-----------------------------------------------------------------
otp_1342(suite) -> [];
otp_1342(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?line load_master("Klas4"),
    try_test(otp_1342),
    ?line unload_master("Klas4").

otp_1342_2(X) -> otp_1342(X).

otp_1342_3(X) -> otp_1342(X).

otp_1342() ->
    s([{[fIndex5, 1], i, 1},
       {[fName5, 1], i, 3},
       {[fStatus5, 1], i, ?createAndGo}]),
    ?line expect(1, ?v1_2(noSuchName, noCreation), 3, any).


%%-----------------------------------------------------------------
%% Ticket: OTP-1366
%% Slogan: snmp traps not sent to all managers
%% Note: NYI! We need a way to tell the test server that we need
%%       mgrs on two different machines.
%%-----------------------------------------------------------------
otp_1366(suite) -> [];
otp_1366(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?line load_master("OLD-SNMPEA-MIB"),
    ?line init_old(),
    try_test(otp_1366),
    ?line unload_master("OLD-SNMPEA-MIB").

otp_1366_2(X) -> otp_1366(X).

otp_1366_3(X) -> otp_1366(X).

otp_1366() ->
    ?INF("NOT YET IMPLEMENTED", []),
    'NYI'.

%%-----------------------------------------------------------------
%% Ticket: OTP-2776
%% Slogan: snmp:validate_date_and_time() fails when time is 00:00
%%-----------------------------------------------------------------
otp_2776(suite) -> [];
otp_2776(Config) when list(Config) ->
  {_SaNode, _MgrNode, _MibDir} = init_case(Config),
  try_test(otp_2776).
 
otp_2776_2(X) -> otp_2776(X).

otp_2776_3(X) -> otp_2776(X).
 
otp_2776() ->
  io:format("Testing bug reported in ticket OTP-2776...~n"),
 
  Dt01_valid   = [19,98,9,1,1,0,23,0,43,0,0],
  Dt02_valid   = [19,98,9,1,0,0,0,0,43,0,0],  % This is what is fixed: 00:00
  Dt03_valid   = [19,98,2,28,1,0,23,0,43,0,0],
  Dt04_invalid = [19,98,2,29,1,0,23,0,43,0,0],
  Dt05_valid   = [19,96,2,29,1,0,23,0,43,0,0],
  Dt06_valid   = [20,0,2,29,1,0,23,0,43,0,0],
  Dt07_invalid = [19,96,2,30,1,0,23,0,43,0,0], % This is also fixed: 30/2
  Dt08_valid   = [19,98,4,30,1,0,23,0,43,0,0],
  Dt09_invalid = [19,98,4,31,1,0,23,0,43,0,0], % This is also fixed: 31/4
  Dt10_invalid = [], 
  Dt11_invalid = [kalle,hobbe], 
  L = [{ 1, true,  Dt01_valid},
       { 2, true,  Dt02_valid},
       { 3, true,  Dt03_valid},
       { 4, false, Dt04_invalid},
       { 5, true,  Dt05_valid},
       { 6, true,  Dt06_valid},
       { 7, false, Dt07_invalid},
       { 8, true,  Dt08_valid},
       { 9, false, Dt09_invalid},
       {10, false, Dt10_invalid},
       {11, false, Dt11_invalid}],
  
  ?line ok = validate_dat(L).
 

validate_dat(L) -> validate_dat(L,[]).
 
validate_dat([],V) -> 
  Fun = fun({_,X}) -> case X of
                        ok -> false;
                        _  -> true
                      end
        end,
  validate_dat1( lists:reverse( lists:filter(Fun,V) ) );
validate_dat([{Id,E,Dat}|T],V) ->
  validate_dat(T,[validate_dat2(Id,E,Dat) | V]).
 
validate_dat1([]) -> ok;
validate_dat1(L)  -> {error,L}.
 
validate_dat2(Id, E, Dat) ->
  Res = case {E,snmp:validate_date_and_time(Dat)} of
          {E,E} -> ok;
          {E,A} -> {E,A}
        end,
  {Id, Res}.


%%-----------------------------------------------------------------
%% Ticket: OTP-2979
%% Slogan: get-next on more than 1 column in an empty table
%%         returns bad response.
%%-----------------------------------------------------------------
otp_2979(suite) -> [];
otp_2979(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?line load_master("Test1"),
    ?line init_old(),
    try_test(otp_2979),
    ?line unload_master("Test1").

otp_2979_2(X) -> otp_2979(X).

otp_2979_3(X) -> otp_2979(X).

otp_2979() ->
    gn([[sparseDescr], [sparseStatus]]),
    ?line expect(1, [{[sparseStr,0], "slut"},
		     {[sparseStr,0], "slut"}]).

%%-----------------------------------------------------------------
%% Ticket: OTP-3187
%% Slogan: get-next on vacmAccessTable for colums > 5 returns
%%         endOfTable - should return value.
%%-----------------------------------------------------------------
otp_3187(suite) -> [];
otp_3187(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?line load_master_std("SNMP-VIEW-BASED-ACM-MIB"),
    otp_3187(),
    ?line unload_master("SNMP-VIEW-BASED-ACM-MIB").

otp_3187_2(X) -> otp_3187(X).

otp_3187_3(X) -> otp_3187(X).

otp_3187() ->
    ?line Elements =
       snmp_view_based_acm_mib:vacmAccessTable(get_next,[],[4,5,6]),
    lists:foreach(fun(E) ->
			   ?line if E == endOfTable ->
					?FAIL(endOfTable);
				       true -> ok
				end
		   end, Elements).

%%-----------------------------------------------------------------
%% Ticket: OTP-3542
%% Slogan: 
%%-----------------------------------------------------------------
otp_3542(suite) -> [];
otp_3542(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    try_test(otp_3542).

otp_3542() ->
    io:format("SNMP v3 discovery...~n"),
    ?line Res = snmp_test_mgr:d(),
    io:format("SNMP v3 discovery result: ~p~n",[Res]).


%%-----------------------------------------------------------------
%% Ticket: OTP-3725
%% Slogan: Slow response time on snmpa:int_to_enum
%%-----------------------------------------------------------------
otp_3725(suite) -> [];
otp_3725(Config) when list(Config) ->
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),

    ?line load_master("OLD-SNMPEA-MIB"),
    ?line init_old(),
    try_test(otp_3725_test, [node()]),
    ?line unload_master("OLD-SNMPEA-MIB").

%% Req. OLD-SNMPEA-MIB
otp_3725_test(MaNode) ->
    io:format("Testing feature requested in ticket OTP-3725...~n"),
    ?line rpc:call(MaNode,snmpa,verbosity,[symbolic_store,trace]),
    ?line Db = rpc:call(MaNode,snmp,get_symbolic_store_db,[]),
    ?DBG("otp_3725_test -> Db = ~p",[Db]),

    ?line {value, OID} = rpc:call(MaNode, snmp, name_to_oid,
				  [Db, intAgentIpAddress]),
    ?DBG("otp_3725_test -> name_to_oid for ~p: ~p",[intAgentIpAddress,OID]),
    ?line {value, intAgentIpAddress} = rpc:call(MaNode, snmp, oid_to_name, 
						[Db,OID]),
    ?DBG("otp_3725_test -> oid_to_name for ~p: ~p",[OID,intAgentIpAddress]),
    ?line false = rpc:call(MaNode, snmp, name_to_oid, [Db, intAgentIpAddres]),
    ?line false = rpc:call(MaNode, snmp, oid_to_name,
			   [Db, [1,5,32,3,54,3,3,34,4]]),
    ?line {value, 2} = rpc:call(MaNode, snmp, enum_to_int,
				[Db, intViewType, excluded]),
    ?line {value, excluded} = rpc:call(MaNode, snmp, int_to_enum,
				       [Db, intViewType, 2]),
    ?line false = rpc:call(MaNode, snmp, enum_to_int, 
			   [Db, intViewType, exclude]),
    ?line false = rpc:call(MaNode, snmp, enum_to_int,
			   [Db, intAgentIpAddress, exclude]),
    ?line false = rpc:call(MaNode, snmp, enum_to_int,
			   [Db, intAgentIpAddre, exclude]),
    ?line false = rpc:call(MaNode, snmp, int_to_enum, [Db, intViewType, 3]),
    ?line false = rpc:call(MaNode, snmp, int_to_enum, 
			   [Db, intAgentIpAddress, 2]),
    ?line false = rpc:call(MaNode, snmp, int_to_enum, 
			   [Db, intAgentIpAddre, 2]),
    ?line {value, active} = rpc:call(MaNode, snmp, int_to_enum, 
				     [Db, 'RowStatus', ?active]),
    ?line {value, ?destroy} = rpc:call(MaNode, snmp, enum_to_int, 
				       [Db, 'RowStatus', destroy]),
    ?line false = rpc:call(MaNode, snmp, enum_to_int, 
			   [Db, 'RowStatus', xxxdestroy]),
    ?line false = rpc:call(MaNode, snmp, enum_to_int, 
			   [Db, 'xxRowStatus', destroy]),
    ?line false = rpc:call(MaNode, snmp, int_to_enum, [Db, 'RowStatus', 25]),
    ?line false = rpc:call(MaNode, snmp, int_to_enum, [Db, 'xxRowStatus', 1]),
    ok.


%%-----------------------------------------------------------------
%% Ticket: OTP-4394
%% Slogan: Target mib tag list check invalid
%%-----------------------------------------------------------------



init_otp_4394(Config) when list(Config) ->
    ?DBG("init_otp_4394 -> entry with"
	   "~n   Config: ~p", [Config]),
    ?line AgentDir = ?config(agent_dir, Config),
    ?line MgrDir   = ?config(mgr_dir, Config),
    ?line Ip       = ?config(ip, Config),
    ?line otp_4394_config(AgentDir, MgrDir, Ip),
    MasterAgentVerbosity = {master_agent_verbosity, trace},
    NetIfVerbosity       = {net_if_verbosity,       trace},
    Opts = [MasterAgentVerbosity,NetIfVerbosity],
    [{vsn, v1} | start_v1_agent(Config,Opts)].

otp_4394_config(AgentDir, MgrDir, Ip0) ->
    ?DBG("otp_4394_config -> entry with"
	   "~n   AgentDir: ~p"
	   "~n   MgrDir:   ~p"
	   "~n   Ip0:      ~p", [AgentDir, MgrDir, Ip0]),
    Vsn = [v1],
    Ip = tuple_to_list(Ip0), 
    ?line snmp_config:write_agent_snmp_files(AgentDir, Vsn, Ip, 
					     ?TRAP_UDP, Ip, 4000, 
					     "OTP-4394 test"),
    ?line case update_usm(Vsn, AgentDir) of
	true ->
	    ?line copy_file(filename:join(AgentDir, "usm.conf"),
			    filename:join(MgrDir, "usm.conf")),
	    ?line update_usm_mgr(Vsn, MgrDir);
	false ->
	    ?line ok
    end,
    C1 = {"a", "all-rights", "initial", "", "pc"},
    C2 = {"c", "secret", "secret_name", "", "secret_tag"},
    ?line write_community_conf(AgentDir, [C1, C2]),
    ?line update_vacm(Vsn, AgentDir),
    Ta1 = {"shelob v1", 
	   [134,138,177,177], 5000, 1500, 3, %% Använd Ip och modda
	   "pc1", 
	   "target_v1", "", 
	   %% [255,255,255,255,0,0], 
	   [],
	   2048},
    Ta2 = {"bifur v1", 
	   [134,138,177,75], 5000, 1500, 3, %% Använd Ip
	   "pc2", 
	   "target_v1", "", 
	   %% [255,255,255,255,0,0],
	   [], 2048},
    ?line write_target_addr_conf(AgentDir, [Ta1, Ta2]),
    ?line write_target_params_conf(AgentDir, Vsn),
    ?line write_notify_conf(AgentDir),
    ok.
    


finish_otp_4394(Config) when list(Config) ->
    ?DBG("finish_otp_4394 -> entry", []),
    C1 = stop_agent(Config),
    delete_files(C1),
    erase(mgr_node),
    lists:keydelete(vsn, 1, C1).

otp_4394_test(suite) -> [];
otp_4394_test(Config) ->
    ?DBG("otp_4394_test -> entry", []),
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    try_test(otp_4394_test1),
    ?DBG("otp_4394_test -> done", []),
    ok.

otp_4394_test1() ->
    ?DBG("otp_4394_test1 -> entry", []),
    gn([[1,1]]),
    Res = 
	case snmp_test_mgr:expect(1, [{[sysDescr,0],  "Erlang SNMP agent"}]) of
	    %% {error, 1, {"?",[]}, {"~w",[timeout]}}
	    {error, 1, _, {_, [timeout]}} ->
		?DBG("otp_4394_test1 -> expected result: timeout", []),
		ok;
	    Else ->
		Else
	end,
    ?DBG("otp_4394_test1 -> done with: ~p", [Res]),
    Res.


%%%--------------------------------------------------
%%% Used to test the standard mib with our
%%% configuration.
%%%--------------------------------------------------
run(F, A, Opts) ->
    M = get(mib_dir),
    Dir = get(mgr_dir),
    User = snmp_misc:get_option(user, Opts, "all-rights"),
    SecLevel = snmp_misc:get_option(sec_level, Opts, noAuthNoPriv),
    EngineID = snmp_misc:get_option(engine_id, Opts, "agentEngine"),
    CtxEngineID = snmp_misc:get_option(context_engine_id, Opts, EngineID),
    Community = snmp_misc:get_option(community, Opts, "all-rights"),
    ?DBG("run -> start crypto app",[]),
    Crypto = ?CRYPTO_START(),
    ?DBG("run -> Crypto: ~p",[Crypto]),
    catch snmp_test_mgr:stop(), % If we had a running mgr from a failed case
    StdM = filename:join(code:priv_dir(snmp), "mibs") ++ "/",
    ?DBG("run -> config:~n"
	   "\tM:           ~p~n"
	   "\tDir:         ~p~n"
	   "\tUser:        ~p~n"
	   "\tSecLevel:    ~p~n"
	   "\tEngineID:    ~p~n"
	   "\tCtxEngineID: ~p~n"
	   "\tCommunity:   ~p~n"
	   "\tStdM:        ~p",
	   [M,Dir,User,SecLevel,EngineID,CtxEngineID,Community,StdM]),
    case snmp_test_mgr:start([%% {agent, snmp_test_lib:hostname()},
			 {packet_server_debug,true},
			 {debug,true},
			 {agent, get(master_host)}, 
			 {agent_udp, 4000},
			 {trap_udp, 5000},
			 {recbuf,65535},
			 quiet,
			 get(vsn),
			 {community, Community},
			 {user, User},
			 {sec_level, SecLevel},
			 {engine_id, EngineID},
			 {context_engine_id, CtxEngineID},
			 {dir, Dir},
			 {mibs, mibs(StdM, M)}]) of
	{ok, _Pid} ->
	    Res = apply(?MODULE, F, A),
	    catch snmp_test_mgr:stop(),
	    Res;
	Err ->
	    io:format("Error starting manager: ~p\n", [Err]),
	    catch snmp_test_mgr:stop(),
	    ?line exit({mgr_start, Err})
    end.
	    

mibs(StdMibDir,MibDir) ->
    [join(StdMibDir, ?v1_2("STANDARD-MIB.bin", "SNMPv2-MIB.bin")),
     join(MibDir, "OLD-SNMPEA-MIB.bin"),
     join(StdMibDir, "SNMP-FRAMEWORK-MIB"),
     join(StdMibDir, "SNMP-MPD-MIB"),
     join(StdMibDir, "SNMP-VIEW-BASED-ACM-MIB"),
     join(StdMibDir, "SNMP-USER-BASED-SM-MIB"),
     join(StdMibDir, "SNMP-TARGET-MIB"),
     join(StdMibDir, "SNMP-NOTIFICATION-MIB"),
     join(MibDir, "Klas1.bin"),
     join(MibDir, "Klas2.bin"), 
     join(MibDir, "Klas3.bin"),
     join(MibDir, "Klas4.bin"),
     join(MibDir, "SA-MIB.bin"),
     join(MibDir, "TestTrap.bin"),
     join(MibDir, "Test1.bin"),
     join(MibDir, "Test2.bin"),
     join(MibDir, "TestTrapv2.bin")].

join(D,F) ->
    filename:join(D,F).

%% string used in index
is(S) -> [length(S) | S].

try_test(Func) ->
    call(get(mgr_node), ?MODULE, run, [Func, [], []]).

try_test(Func, A) ->
    call(get(mgr_node), ?MODULE, run, [Func, A, []]).

try_test(Func, A, Opts) ->
    call(get(mgr_node), ?MODULE, run, [Func, A, Opts]).

call(N,M,F,A) ->
    ?DBG("call -> entry with~n"
	   "    N:     ~p~n"
	   "    M:     ~p~n"
	   "    F:     ~p~n"
	   "    A:     ~p~n"
	   "  when~n"
	   "    get(): ~p",
	   [N,M,F,A,get()]),
    spawn(N, ?MODULE, wait, [self(),get(),M,F,A]),
    receive
	{done, {'EXIT', Rn}, Loc} ->
	    ?DBG("call -> returned ~p",[{done, {'EXIT', Rn}, Loc}]),
	    put(test_server_loc, Loc),
	    exit(Rn);
	{done, Ret, Zed} -> 
	    ?DBG("call -> returned ~p~n",[{done, Ret, Zed}]),
	    Ret
    end.

wait(From, Env, M, F, A) ->
    ?DBG("wait -> entry with ~n"
	   "\tFrom: ~p~n"
	   "\tEnv:  ~p",[From,Env]),
    lists:foreach(fun({K,V}) -> put(K,V) end, Env),
    Rn = (catch apply(M, F, A)),
    ?DBG("wait -> Rn: ~n~p", [Rn]),
    From ! {done, Rn, get(test_server_loc)},
    exit(Rn).

expect(A,B) -> ok = snmp_test_mgr:expect(A,B).
expect(A,B,C) -> ok = snmp_test_mgr:expect(A,B,C).
expect(A,B,C,D) -> ok = snmp_test_mgr:expect(A,B,C,D).
expect(A,B,C,D,E,F) -> ok = snmp_test_mgr:expect(A,B,C,D,E,F).

get_req(Id, Vars) ->
    ?DBG("get_req -> entry with~n"
	   "\tId:   ~p~n"
	   "\tVars: ~p",[Id,Vars]),
    g(Vars),
    ?DBG("get_req -> await response",[]),
    {ok, Val} = snmp_test_mgr:get_response(Id, Vars), 
    ?DBG("get_req -> response: ~p",[Val]),
    Val.

get_next_req(Vars) ->
    ?DBG("get_next_req -> entry with Vars '~p', send request",[Vars]),
    gn(Vars),
    ?DBG("get_next_req -> await response",[]),
    Response = snmp_test_mgr:receive_response(),
    ?DBG("get_next_req -> response: ~p",[Response]),
    Response.



start_node(Name) ->
    ?LOG("start_node -> entry with Name: ~p",[Name]),
    M = list_to_atom(?HOSTNAME(node())),
    ?DBG("start_node -> M: ~p",[M]),
    Pa = filename:dirname(code:which(?MODULE)),
    ?DBG("start_node -> Pa: ~p",[Pa]),

    Args = case init:get_argument('CC_TEST') of
	       {ok, [[]]} ->
		   " -pa /clearcase/otp/libraries/snmp/ebin ";
	       {ok, [[Path]]} ->
		   " -pa " ++ Path;
	       error ->
		      ""
	      end,
    %% Do not use start_link!!! (the proc that calls this one is tmp)
    ?DBG("start_node -> Args: ~p~n",[Args]),
    A = Args ++ " -pa " ++ Pa,
    case (catch ?START_NODE(Name, A)) of
	{ok, Node} ->
	    %% Tell the test_server to not clean up things it never started.
	    ?DBG("start_node -> Node: ~p",[Node]),
	    {ok, Node};
	Else  -> 
	    ?ERR("start_node -> failed with(other): Else: ~p",[Else]),
	    ?line ?FAIL(Else)
    end.


stop_node(Node) ->
    ?LOG("stop_node -> Node: ~p",[Node]),
    rpc:cast(Node, erlang, halt, []).

p(X) ->
    io:format(user, X++"\n", []).

sleep(X) ->
    receive
	after
	    X -> ok
	end.

%%%-----------------------------------------------------------------
%%% Configuration
%%%-----------------------------------------------------------------
config(Vsns, MgrDir, AgentDir, MIp, AIp) ->
    ?line snmp_config:write_agent_snmp_files(AgentDir, Vsns, MIp, 
					     ?TRAP_UDP, AIp, 4000, 
					     "test"),
    ?line case update_usm(Vsns, AgentDir) of
	true ->
	    ?line copy_file(filename:join(AgentDir, "usm.conf"),
			    filename:join(MgrDir, "usm.conf")),
	    ?line update_usm_mgr(Vsns, MgrDir);
	false ->
	    ?line ok
    end,
    ?line update_community(Vsns, AgentDir),
    ?line update_vacm(Vsns, AgentDir),
    ?line write_target_addr_conf(AgentDir, MIp, ?TRAP_UDP, Vsns),
    ?line write_target_params_conf(AgentDir, Vsns),
    ?line write_notify_conf(AgentDir),
    ok.

delete_files(Config) ->
    Dir = ?config(agent_dir, Config),
    {ok, List} = file:list_dir(Dir),
    lists:foreach(fun(FName) -> file:delete(filename:join(Dir, FName)) end,
		  List).

update_usm(Vsns, Dir) ->
    case lists:member(v3, Vsns) of
	true ->
	    {ok, Fid} = file:open(filename:join(Dir,"usm.conf"),[read,write]),
	    file:position(Fid, eof),
	    ok = io:format(Fid, "{\"agentEngine\", \"all-rights\", "
			   "\"all-rights\", zeroDotZero, "
			   "usmNoAuthProtocol, \"\", \"\", "
			   "usmNoPrivProtocol, \"\", \"\", \"\", "
			   "\"\", \"\"}.\n", []),
	    ok = io:format(Fid, "{\"agentEngine\", \"no-rights\", "
			   "\"no-rights\", zeroDotZero, "
			   "usmNoAuthProtocol, \"\", \"\", "
			   "usmNoPrivProtocol, \"\", \"\", \"\", "
			   "\"\", \"\"}.\n", []),
	    ok = io:format(Fid, "{\"agentEngine\", \"authMD5\", "
			   "\"authMD5\", zeroDotZero, "
			   "usmHMACMD5AuthProtocol, \"\", \"\", "
			   "usmNoPrivProtocol, \"\", \"\", \"\", "
			   "\"passwd_md5xxxxxx\", \"\"}.\n", []),
	    ok = io:format(Fid, "{\"agentEngine\", \"authSHA\", "
			   "\"authSHA\", zeroDotZero, "
			   "usmHMACSHAAuthProtocol, \"\", \"\", "
			   "usmNoPrivProtocol, \"\", \"\", \"\", "
			   "\"passwd_shaxxxxxxxxxx\", \"\"}.\n", []),
	    ok = io:format(Fid, "{\"agentEngine\", \"privDES\", "
			   "\"privDES\", zeroDotZero, "
			   "usmHMACSHAAuthProtocol, \"\", \"\", "
			   "usmDESPrivProtocol, \"\", \"\", \"\", "
			   "\"passwd_shaxxxxxxxxxx\", \"passwd_desxxxxxx\"}.\n",
			   []),
	    ok = io:format(Fid, "{\"mgrEngine\", \"all-rights\", "
			   "\"all-rights\", zeroDotZero, "
			   "usmNoAuthProtocol, \"\", \"\", "
			   "usmNoPrivProtocol, \"\", \"\", \"\", "
			   "\"\", \"\"}.\n", []),
	    ok = io:format(Fid, "{\"mgrEngine\", \"no-rights\", "
			   "\"no-rights\", zeroDotZero, "
			   "usmNoAuthProtocol, \"\", \"\", "
			   "usmNoPrivProtocol, \"\", \"\", \"\", "
			   "\"\", \"\"}.\n", []),
	    ok = io:format(Fid, "{\"mgrEngine\", \"authMD5\", "
			   "\"authMD5\", zeroDotZero, "
			   "usmHMACMD5AuthProtocol, \"\", \"\", "
			   "usmNoPrivProtocol, \"\", \"\", \"\", "
			   "\"passwd_md5xxxxxx\", \"\"}.\n", []),
	    ok = io:format(Fid, "{\"mgrEngine\", \"authSHA\", "
			   "\"authSHA\", zeroDotZero, "
			   "usmHMACSHAAuthProtocol, \"\", \"\", "
			   "usmNoPrivProtocol, \"\", \"\", \"\", "
			   "\"passwd_shaxxxxxxxxxx\", \"\"}.\n", []),
	    ok = io:format(Fid, "{\"mgrEngine\", \"privDES\", "
			   "\"privDES\", zeroDotZero, "
			   "usmHMACSHAAuthProtocol, \"\", \"\", "
			   "usmDESPrivProtocol, \"\", \"\", \"\", "
			   "\"passwd_shaxxxxxxxxxx\", \"passwd_desxxxxxx\"}.\n",
			   []),
	    file:close(Fid),
	    true;
	false ->
	    false
    end.
    
update_usm_mgr(Vsns, Dir) ->
    case lists:member(v3, Vsns) of
	true ->
	    {ok, Fid} = file:open(filename:join(Dir,"usm.conf"),[read,write]),
	    file:position(Fid, eof),
	    ok = io:format(Fid, "{\"agentEngine\", \"newUser\", "
			   "\"newUser\", zeroDotZero, "
			   "usmHMACSHAAuthProtocol, \"\", \"\", "
			   "usmDESPrivProtocol, \"\", \"\", \"\", "
			   "\"passwd_shaxxxxxxxxxx\", \"passwd_desxxxxxx\"}.\n",
			   []),
	    ok = io:format(Fid, "{\"mgrEngine\", \"newUser\", "
			   "\"newUser\", zeroDotZero, "
			   "usmHMACSHAAuthProtocol, \"\", \"\", "
			   "usmDESPrivProtocol, \"\", \"\", \"\", "
			   "\"passwd_shaxxxxxxxxxx\", \"passwd_desxxxxxx\"}.\n",
			   []),
	    file:close(Fid),
	    true;
	false ->
	    false
    end.

rewrite_usm_mgr(Dir, ShaKey, DesKey) -> 
    ?line ok = file:rename(filename:join(Dir,"usm.conf"),
			   filename:join(Dir,"usm.old")),
    ?line {ok, Fid} = file:open(filename:join(Dir,"usm.conf"),write),
    ok = io:format(Fid, "{\"agentEngine\", \"newUser\", "
		   "\"newUser\", zeroDotZero, "
		   "usmHMACSHAAuthProtocol, \"\", \"\", "
		   "usmDESPrivProtocol, \"\", \"\", \"\", "
		   "\"~s\", \"~s\"}.\n",
		   [ShaKey, DesKey]),
    ok = io:format(Fid, "{\"mgrEngine\", \"newUser\", "
		   "\"newUser\", zeroDotZero, "
		   "usmHMACSHAAuthProtocol, \"\", \"\", "
		   "usmDESPrivProtocol, \"\", \"\", \"\", "
		   "\"~s\", \"~s\"}.\n",
		   [ShaKey, DesKey]),
    file:close(Fid).

reset_usm_mgr(Dir) ->
    ?line ok = file:rename(filename:join(Dir,"usm.old"),
			   filename:join(Dir,"usm.conf")).


update_community([v3], _Dir) -> ok;
update_community(_, Dir) ->
    {ok, Fid} = file:open(filename:join(Dir,"community.conf"),[read,write]),
    file:position(Fid, eof),
    ok=io:format(Fid,"{\"no-rights\",\"no-rights\",\"no-rights\",\"\",\"\"}.\n",
		 []),
    file:close(Fid).
    
    
-define(tDescr_instance, [1,3,6,1,2,1,16,1,0]).
update_vacm(_Vsn, Dir) ->
    {ok, Fid} = file:open(filename:join(Dir,"vacm.conf"),[read,write]),
    file:position(Fid, eof),
    ok=io:format(Fid,"{vacmSecurityToGroup,usm,\"authMD5\",\"initial\"}.\n",[]),
    ok=io:format(Fid,"{vacmSecurityToGroup,usm,\"authSHA\",\"initial\"}.\n",[]),
    ok=io:format(Fid,"{vacmSecurityToGroup,usm,\"privDES\",\"initial\"}.\n",[]),
    ok=io:format(Fid,"{vacmSecurityToGroup,usm,\"newUser\",\"initial\"}.\n",[]),
    ok = io:format(Fid, "{vacmViewTreeFamily, \"internet\", "
		   "~w, excluded, null}.\n", [?tDescr_instance]),
    file:close(Fid).
    
    
vacm_ver(v1) -> v1;
vacm_ver(v2) -> v2c;
vacm_ver(v3) -> usm.
     

write_community_conf(Dir, Confs) ->
    {ok, Fid} = file:open(filename:join(Dir,"community.conf"),write),
    ok = write_community_conf1(Fid, Confs),
    file:close(Fid).

write_community_conf1(_, []) ->
    ok;
write_community_conf1(Fid, [{ComIdx, ComName, SecName, CtxName, TransTag}|Confs]) ->
    ok = io:format(Fid, "{\"~s\", \"~s\", \"~s\", \"~s\", \"~s\"}.~n",
		   [ComIdx, ComName, SecName, CtxName, TransTag]),
    write_community_conf1(Fid, Confs).
    

write_target_addr_conf(Dir, Confs) ->
    {ok, Fid} = file:open(filename:join(Dir,"target_addr.conf"),write),
    ok = write_target_addr_conf1(Fid, Confs),
    file:close(Fid).


write_target_addr_conf1(_, []) ->
    ok;
write_target_addr_conf1(Fid, 
		       [{Name, Ip, Port, Timeout, Retry, TagList, ParamName, 
			 EngineId, TMask, MaxMsgSz}|Confs]) ->
    ok = io:format(Fid, "{\"~s\", ~w, ~w, ~w, ~w, \"~s\", \"~s\", \"~s\", ~w, ~w}.~n",
		   [Name, Ip, Port, Timeout, Retry, TagList, ParamName, 
		    EngineId, TMask, MaxMsgSz]),
    write_target_addr_conf1(Fid, Confs).
    
write_target_addr_conf(Dir, ManagerIp, UDP, Vsns) -> 
    {ok, Fid} = file:open(filename:join(Dir,"target_addr.conf"),write),
    lists:foreach(fun(Vsn) ->
			  ok = io:format(Fid, 
					 "{\"~s\", ~w, ~w, 1500, 3, "
					 "\"std_trap\", \"~s\"}.~n",
					 [mk_ip(ManagerIp, Vsn),
					  ManagerIp, UDP, mk_param(Vsn)]),
			  case Vsn of
			      v1 -> ok;
			      v2 ->
				  ok = io:format(Fid, 
						 "{\"~s.2\",~w,~w,1500,3, "
						 "\"std_inform\", \"~s\"}.~n",
						 [mk_ip(ManagerIp, Vsn),
						  ManagerIp, UDP,
						  mk_param(Vsn)]);
			      v3 ->
				  ok = io:format(Fid, 
						 "{\"~s.3\",~w,~w,1500,3, "
						 "\"std_inform\", \"~s\", "
						 "\"mgrEngine\", [], 1024}.~n",
						 [mk_ip(ManagerIp, Vsn),
						  ManagerIp, UDP,
						  mk_param(Vsn)])
			  end
		  end,
		  Vsns),
    file:close(Fid).

mk_param(v1) -> "target_v1";
mk_param(v2) -> "target_v2";
mk_param(v3) -> "target_v3".
     
mk_ip([A,B,C,D], Vsn) ->
    io_lib:format("~w.~w.~w.~w ~w", [A,B,C,D,Vsn]).


rewrite_target_addr_conf(Dir,NewPort) -> 
    TAFile = filename:join(Dir, "target_addr.conf"),
    ?DBG("rewrite_target_addr_conf -> read target file info of address config file",[]),
    case file:read_file_info(TAFile) of
	{ok, _} -> ok;
	{error, R} -> ?ERR("failure reading file info of "
			  "target address config file: ~p",[R]),
		      ok  
    end,

    ?line [TrapAddr|Addrs] = 
	snmp_conf:read(TAFile,fun(R) -> rewrite_target_addr_conf1(R) end),

    ?DBG("rewrite_target_addr_conf -> TrapAddr: ~p",[TrapAddr]),

    NewAddrs = [rewrite_target_addr_conf2(NewPort,TrapAddr)|Addrs],
    
    ?DBG("rewrite_target_addr_conf -> NewAddrs: ~p",[NewAddrs]),

    ?line ok = file:rename(filename:join(Dir,"target_addr.conf"),
			   filename:join(Dir,"target_addr.old")),
    ?line {ok, Fid} = file:open(filename:join(Dir,"target_addr.conf"),write),
    
    ?line ok = rewrite_target_addr_conf3(Fid,NewAddrs),

    file:close(Fid).

rewrite_target_addr_conf1(O) -> 
    {ok,O}.

rewrite_target_addr_conf2(NewPort,{Name,Ip,_Port,Timeout,Retry,
				   "std_trap",EngineId}) -> 
    ?LOG("rewrite_target_addr_conf2 -> entry with std_trap",[]),
    {Name,Ip,NewPort,Timeout,Retry,"std_trap",EngineId};
rewrite_target_addr_conf2(_NewPort,O) -> 
    ?LOG("rewrite_target_addr_conf2 -> entry with "
	 "~n   O: ~p",[O]),
    O.


rewrite_target_addr_conf3(_,[]) -> ok;
rewrite_target_addr_conf3(Fid,[{Name,Ip,Port,Timeout,Retry,
				ParamName,EngineId}|T]) -> 
    ?LOG("rewrite_target_addr_conf3 -> write(1) ~s",[ParamName]),
    io:format(Fid, 
	      "{\"~s\", " % Name
	      "~p, "      % Ip
	      "~p, "      % Port
	      "~p, "      % Timeout
	      "~p, "      % Retry
	      "\"~s\", "  % ParamsName
	      "\"~s\"}.", % EngineId
	      [Name,Ip,Port,Timeout,Retry,ParamName,EngineId]),
    rewrite_target_addr_conf3(Fid,T);
rewrite_target_addr_conf3(Fid,[{Name,Ip,Port,Timeout,Retry,TagList,
				ParamName,EngineId,TMask,MMS}|T]) ->
    ?LOG("rewrite_target_addr_conf3 -> write(2) ~s",[ParamName]),
    io:format(Fid, 
	      "{\"~s\", " % Name
	      "~p, "      % Ip
	      "~p, "      % Port
	      "~p, "      % Timeout
	      "~p, "      % Retry
	      "\"~s\", "  % TagList
	      "\"~s\", "  % ParamsName
	      "\"~s\","   % EngineId
	      "~p, "      % TMask
	      "~p}.",     % MMS
	      [Name,Ip,Port,Timeout,Retry,TagList,ParamName,
	       EngineId,TMask,MMS]),
    rewrite_target_addr_conf3(Fid,T).
    
reset_target_addr_conf(Dir) ->
    ?line ok = file:rename(filename:join(Dir,"target_addr.old"),
			   filename:join(Dir,"target_addr.conf")).

write_target_params_conf(Dir, Vsns) -> 
    {ok, Fid} = file:open(filename:join(Dir,"target_params.conf"),write),
    lists:foreach(fun(Vsn) ->
			  MP = if Vsn == v1 -> v1;
				  Vsn == v2 -> v2c;
				  Vsn == v3 -> v3
			       end,
			  SM = if Vsn == v1 -> v1;
				  Vsn == v2 -> v2c;
				  Vsn == v3 -> usm
			       end,
			  ok = io:format(Fid, "{\"target_~w\", ~w, ~w, "
					 "\"all-rights\", noAuthNoPriv}.~n",
					 [Vsn, MP, SM])
		  end,
		  Vsns),
    file:close(Fid).

rewrite_target_params_conf(Dir, SecName, SecLevel) -> 
    ?line ok = file:rename(filename:join(Dir,"target_params.conf"),
			   filename:join(Dir,"target_params.old")),
    ?line {ok, Fid} = file:open(filename:join(Dir,"target_params.conf"),write),
    ?line ok = io:format(Fid, "{\"target_v3\", v3, usm, \"~s\", ~w}.~n",
			 [SecName, SecLevel]),
    file:close(Fid).

reset_target_params_conf(Dir) ->
    ?line ok = file:rename(filename:join(Dir,"target_params.old"),
			   filename:join(Dir,"target_params.conf")).

write_notify_conf(Dir) -> 
    {ok, Fid} = file:open(filename:join(Dir,"notify.conf"),write),
    ok = io:format(Fid, "{\"standard trap\", \"std_trap\", trap}.~n", []),
    ok = io:format(Fid, "{\"standard inform\", \"std_inform\",inform}.~n", []),
    file:close(Fid).

ver_to_trap_str([v1]) -> "v1";
ver_to_trap_str([v2]) -> "v2";
% default is to use the latest snmp version
ver_to_trap_str([v1,v2]) -> "v2".



write_view_conf(Dir) -> 
    {ok, Fid} = file:open(a(Dir,"view.conf"),write),
    ok = io:format(Fid, "{2, [1,3,6], included, null}.~n", []),
    ok = io:format(Fid, "{2, ~w, excluded, null}.~n", [?tDescr_instance]),
    file:close(Fid).

a(A,B) -> lists:append(A,B).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

copy_file(From, To) ->
    {ok, Bin} = file:read_file(From),
    ok = file:write_file(To, Bin).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

display_memory_usage() ->
    Info       = snmpa:info(snmp_master_agent),
    TreeSize   = lists_key1search(tree_size_bytes,  Info),
    ProcMem    = lists_key1search(process_memory,   Info),
    MibDbSize  = lists_key1search([db_memory,mib],  Info),
    NodeDbSize = lists_key1search([db_memory,node], Info),
    TreeDbSize = lists_key1search([db_memory,tree], Info),
    ?INF("Memory usage: "
	"~n   Tree size:           ~p"
	"~n   Process memory size: ~p"
	"~n   Mib db size:         ~p"
	"~n   Node db size:        ~p"
	"~n   Tree db size:        ~p", 
    [TreeSize, ProcMem, MibDbSize, NodeDbSize, TreeDbSize]).
    
lists_key1search([], Res) ->
    Res;
lists_key1search([Key|Keys], List) when atom(Key), list(List) ->
    case lists:keysearch(Key, 1, List) of
	{value, {Key, Val}} ->
	    lists_key1search(Keys, Val);
	false ->
	    undefined
    end;
lists_key1search(Key, List) when atom(Key) ->
    case lists:keysearch(Key, 1, List) of
	{value, {Key, Val}} ->
	    Val;
	false ->
	    undefined
    end.


regs() ->
    lists:sort(registered()).
