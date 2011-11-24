%% 
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2011. All Rights Reserved.
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

-module(snmp_agent_test_lib).


-export([
	 start_v1_agent/1,        start_v1_agent/2, 
	 start_v2_agent/1,        start_v2_agent/2, 
	 start_v3_agent/1,        start_v3_agent/2, 
	 start_bilingual_agent/1, start_bilingual_agent/2, 
	 start_mt_agent/1,        start_mt_agent/2, 
	 stop_agent/1,

	 start_sup/0,      stop_sup/2,
	 start_subagent/3, stop_subagent/1, 
	 start_sub_sup/1,  start_sub_sup/2, 

	 start_node/1, stop_node/1,

	 load_master/1, load_master_std/1, unload_master/1, 
	 loaded_mibs/0, unload_mibs/1,

	 get_req/2, get_next_req/1,

	 config/5,
	 delete_files/1, 
	 copy_file/2, 
	 update_usm/2, 
	 update_usm_mgr/2, rewrite_usm_mgr/3, reset_usm_mgr/1, 
	 update_community/2, 
	 update_vacm/2,
	 write_community_conf/2, 
	 write_target_addr_conf/2, write_target_addr_conf/4, 
	 rewrite_target_addr_conf/2, reset_target_addr_conf/1,
	 write_target_params_conf/2, rewrite_target_params_conf/3, 
	 reset_target_params_conf/1,
	 write_notify_conf/1, write_view_conf/1, 

	 display_memory_usage/0,

	 init_all/1, finish_all/1,
	 init_case/1,
	 try_test/2, try_test/3, try_test/4,
	 expect/2, expect/3, expect/4, expect/6, 
	 
	 regs/0,
	 rpc/3
	]).

%% Internal exports
-export([wait/5, run/4]).

-include_lib("kernel/include/file.hrl").
-include("test_server.hrl").
-include("snmp_test_lib.hrl").
-define(SNMP_USE_V3, true).
-include_lib("snmp/include/snmp_types.hrl").

-define(TRAP_UDP, 5000).

-define(v1_2(V1,V2),
	case get(vsn) of
	    v1 -> V1;
	    _  -> V2
	end).
                         
-define(v1_2_3(V1,V2,V3),
	case get(vsn) of
	    v1 -> V1;
	    v2 -> V2;
	    _  -> V3
	end).


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

init_all(Config0) when is_list(Config0) ->
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

    PrivDir = ?config(priv_dir, Config),
    ?DBG("init_all -> PrivDir ~p", [PrivDir]),

    TopDir = filename:join(PrivDir, snmp_agent_test),
    case file:make_dir(TopDir) of
	ok ->
	    ok;
	{error, eexist} ->
	    ok;
	Error ->
	    ?FAIL({failed_creating_subsuite_top_dir, Error})
    end,

    DataDir = ?config(data_dir, Config),
    ?DBG("init_all -> DataDir ~p", [DataDir]),

    ?line ok = file:make_dir(MgrDir = filename:join(TopDir, "mgr_dir/")),
    ?DBG("init_all -> MgrDir ~p", [MgrDir]),

    ?line ok = file:make_dir(AgentDir = filename:join(TopDir, "agent_dir/")),
    ?DBG("init_all -> AgentDir ~p", [AgentDir]),

    ?line ok = file:make_dir(SaDir = filename:join(TopDir, "sa_dir/")),
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
					 filename:join(TopDir, "Mnesia1")),

    ?DBG("init_all -> application mnesia: set_env dir on node ~p",[SaNode]),
    ?line rpc:call(SaNode, application_controller, set_env,
		   [mnesia, dir,  filename:join(TopDir, "Mnesia2")]),

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


finish_all(Config) when is_list(Config) ->
    SaNode = ?config(snmp_sa, Config),
    MgrNode = ?config(snmp_mgr, Config),
    stop_node(SaNode),
    stop_node(MgrNode),
    application:stop(mnesia).


%% --- This one *must* be run first in each case ---

init_case(Config) when is_list(Config) ->
    ?DBG("init_case -> entry with"
	   "~n   Config: ~p", [Config]),
    SaNode     = ?config(snmp_sa, Config),
    MgrNode    = ?config(snmp_mgr, Config),
    MasterNode = node(),

    SaHost         = ?HOSTNAME(SaNode),
    MgrHost        = ?HOSTNAME(MgrNode),
    MasterHost     = ?HOSTNAME(MasterNode),
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
    put(masterip,    tuple_to_list(MasterIP)),
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


%%%--------------------------------------------------
%%% Used to test the standard mib with our
%%% configuration.
%%%--------------------------------------------------

try_test(Mod, Func) ->
    call(get(mgr_node), ?MODULE, run, [Mod, Func, [], []]).

try_test(Mod, Func, A) ->
    call(get(mgr_node), ?MODULE, run, [Mod, Func, A, []]).

try_test(Mod, Func, A, Opts) ->
    call(get(mgr_node), ?MODULE, run, [Mod, Func, A, Opts]).

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
	    ?DBG("call -> done with exit: "
		 "~n   Rn:  ~p"
		 "~n   Loc: ~p", [Rn, Loc]),
	    put(test_server_loc, Loc),
	    exit(Rn);
	{done, Ret, Zed} -> 
	    ?DBG("call -> done:"
		 "~n   Ret: ~p"
		 "~n   Zed: ~p", [Ret, Zed]),
	    Ret
    end.

wait(From, Env, M, F, A) ->
    ?DBG("wait -> entry with"
	 "~n   From: ~p"
	 "~n   Env:  ~p"
	 "~n   M:    ~p"
	 "~n   F:    ~p"
	 "~n   A:    ~p", [From, Env, M, F, A]),
    lists:foreach(fun({K,V}) -> put(K,V) end, Env),
    Rn = (catch apply(M, F, A)),
    ?DBG("wait -> Rn: ~n~p", [Rn]),
    From ! {done, Rn, get(test_server_loc)},
    exit(Rn).

run(Mod, Func, Args, Opts) ->
    ?DBG("run -> entry with"
	 "~n   Mod:  ~p"
	 "~n   Func: ~p"
	 "~n   Args: ~p"
	 "~n   Opts: ~p", [Mod, Func, Args, Opts]),
    M = get(mib_dir),
    Dir = get(mgr_dir),
    User = snmp_misc:get_option(user, Opts, "all-rights"),
    SecLevel = snmp_misc:get_option(sec_level, Opts, noAuthNoPriv),
    EngineID = snmp_misc:get_option(engine_id, Opts, "agentEngine"),
    CtxEngineID = snmp_misc:get_option(context_engine_id, Opts, EngineID),
    Community = snmp_misc:get_option(community, Opts, "all-rights"),
    ?DBG("run -> start crypto app",[]),
    Crypto = case os:type() of
		 vxworks ->
		     no_crypto;
		 _ ->
		     ?CRYPTO_START()
	     end,
    ?DBG("run -> Crypto: ~p", [Crypto]),
    catch snmp_test_mgr:stop(), % If we had a running mgr from a failed case
    StdM = filename:join(code:priv_dir(snmp), "mibs") ++ "/",
    Vsn = get(vsn), 
    ?DBG("run -> config:"
	   "~n   M:           ~p"
	   "~n   Vsn:         ~p"
	   "~n   Dir:         ~p"
	   "~n   User:        ~p"
	   "~n   SecLevel:    ~p"
	   "~n   EngineID:    ~p"
	   "~n   CtxEngineID: ~p"
	   "~n   Community:   ~p"
	   "~n   StdM:        ~p",
	   [M,Vsn,Dir,User,SecLevel,EngineID,CtxEngineID,Community,StdM]),
    case snmp_test_mgr:start([%% {agent, snmp_test_lib:hostname()},
			      {packet_server_debug,true},
			      {debug,true},
			      {agent, get(master_host)}, 
			      {agent_udp, 4000},
			      {trap_udp, 5000},
			      {recbuf,65535},
			      quiet,
			      Vsn, 
			      {community, Community},
			      {user, User},
			      {sec_level, SecLevel},
			      {engine_id, EngineID},
			      {context_engine_id, CtxEngineID},
			      {dir, Dir},
			      {mibs, mibs(StdM, M)}]) of
	{ok, _Pid} ->
	    case (catch apply(Mod, Func, Args)) of
		{'EXIT', Reason} ->
		    catch snmp_test_mgr:stop(),
		    ?FAIL({apply_failed, {Mod, Func, Args}, Reason});
		Res ->
		    catch snmp_test_mgr:stop(),
		    Res
	    end;
	Err ->
	    io:format("Error starting manager: ~p\n", [Err]),
	    catch snmp_test_mgr:stop(),
	    ?line ?FAIL({mgr_start, Err})
    end.


%% ---------------------------------------------------------------
%% ---                                                         ---
%% ---                   Start the agent                       ---
%% ---                                                         ---
%% ---------------------------------------------------------------

start_v1_agent(Config) when is_list(Config) ->
    start_agent(Config, [v1]).
 
start_v1_agent(Config, Opts) when is_list(Config) andalso is_list(Opts)  ->
    start_agent(Config, [v1], Opts).
 
start_v2_agent(Config) when is_list(Config) ->
    start_agent(Config, [v2]).
 
start_v2_agent(Config, Opts) when is_list(Config) andalso is_list(Opts) ->
    start_agent(Config, [v2], Opts).
 
start_v3_agent(Config) when is_list(Config) ->
    start_agent(Config, [v3]).
 
start_v3_agent(Config, Opts) when is_list(Config) andalso is_list(Opts) ->
    start_agent(Config, [v3], Opts).
 
start_bilingual_agent(Config) when is_list(Config) ->
    start_agent(Config, [v1,v2]).
 
start_bilingual_agent(Config, Opts) when is_list(Config) andalso is_list(Opts) ->
    start_agent(Config, [v1,v2], Opts).
 
start_mt_agent(Config) when is_list(Config) ->
    start_agent(Config, [v2], [{snmp_multi_threaded, true}]).
 
start_mt_agent(Config, Opts) when is_list(Config) andalso is_list(Opts) ->
    start_agent(Config, [v2], [{snmp_multi_threaded, true}|Opts]).
 
start_agent(Config, Vsns) ->
    start_agent(Config, Vsns, []).
start_agent(Config, Vsns, Opts) -> 
    ?LOG("start_agent -> entry (~p) with"
	"~n   Config: ~p"
	"~n   Vsns:   ~p"
	"~n   Opts:   ~p", [node(), Config, Vsns, Opts]),
    
    ?line AgentDir = ?config(agent_dir, Config),
    ?line SaNode   = ?config(snmp_sa,   Config),

%%     AgentConfig = 
%% 	[{agent_type, master},
%% 	 %% {multi_threaded,         MultiT},
%% 	 %% {priority,               Prio}, 
%% 	 %% {error_report_mod,       ErrorReportMod},
%% 	 {versions,   Vsns},
%% 	 {db_dir,     AgentDir}, 
%% 	 %% {db_init_error,          DbInitError},
%% 	 %% {set_mechanism,          SetModule},
%% 	 %% {authentication_service, AuthModule},
%% 	 {audit_trail_log, [{type,   read_write},
%% 			    {dir,    AgentDir},
%% 			    {size,   {10240, 10}},
%% 			    {repair, true}]},
%% 	 {config, [{verbosity,  info},
%% 		   {dir,        AgentDir},
%% 		   {force_load, false}]},
%% 	 {mibs, Mibs},
%% 	 %% {mib_storage, MibStorage}, 
%% 	 {local_db, []},
%% 	 {mib_server, []},
%% 	 {symbolic_store, []},
%% 	 {note_store, []}, 
%% 	 {net_if, []}, 
%% 	 %% {supervisor,             SupOpts}
%% 	],
    
    app_env_init(vsn_init(Vsns) ++ 
		 [{audit_trail_log, read_write_log},
		  {audit_trail_log_dir, AgentDir},
		  {audit_trail_log_size, {10240, 10}},
		  {force_config_reload, false},
		  {snmp_agent_type, master},
		  {snmp_config_dir, AgentDir},
		  {snmp_db_dir, AgentDir},
		  {snmp_local_db_auto_repair, true},
		  {snmp_local_db_verbosity, log},
		  {snmp_master_agent_verbosity, trace},
		  {snmp_supervisor_verbosity, trace},
		  {snmp_mibserver_verbosity, log},
		  {snmp_symbolic_store_verbosity, log},
		  {snmp_note_store_verbosity, log},
		  {snmp_net_if_verbosity, trace}],
		 Opts),


    process_flag(trap_exit,true),

    {ok, AppSup} = snmp_app_sup:start_link(),
    unlink(AppSup),
    ?DBG("start_agent -> snmp app supervisor: ~p",[AppSup]),

    ?DBG("start_agent -> start master agent (old style)",[]),
    ?line Sup = start_sup(), 

    ?DBG("start_agent -> unlink from supervisor",[]),
    ?line unlink(Sup),
    ?line SaDir = ?config(sa_dir, Config),
    ?DBG("start_agent -> (rpc) start sub on ~p",[SaNode]),
    ?line {ok, Sub} = start_sub_sup(SaNode, SaDir),
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

app_env_init(Env0, Opts) ->
    ?DBG("app_env_init -> unload snmp",[]),
    ?line application:unload(snmp),
    ?DBG("app_env_init -> load snmp",[]),
    ?line application:load(snmp),
    ?DBG("app_env_init -> initiate (snmp) application env",[]),
    F1 = fun({Key, Val} = New, Acc0) -> 
                 ?DBG("app_env_init -> "
                     "updating setting ~p to ~p", [Key, Val]),
                 case lists:keyreplace(Key, 1, Acc0, New) of
		     Acc0 ->
			 [New|Acc0];
		     Acc ->
			 Acc
		 end
         end, 
    Env = lists:foldr(F1, Env0, Opts),
    ?DBG("app_env_init -> Env: ~p",[Env]),
    F2 = fun({Key,Val}) ->
                 ?DBG("app_env_init -> setting ~p to ~p",[Key, Val]),
                 application_controller:set_env(snmp, Key, Val)
         end,
    lists:foreach(F2, Env).


stop_agent(Config) when is_list(Config) ->
    ?LOG("stop_agent -> entry with"
	 "~n   Config: ~p",[Config]),

    {Sup, Par} = ?config(snmp_sup, Config),
    ?DBG("stop_agent -> attempt to stop (sup) ~p"
	"~n   Sup: ~p"
	"~n   Par: ~p",
	[Sup, 
	(catch process_info(Sup)),
	(catch process_info(Par))]),
    
    Info = agent_info(Sup),
    ?DBG("stop_agent -> Agent info: "
	 "~n   ~p", [Info]),
    
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


start_sup() ->
    case (catch snmpa_app:start(normal)) of
	{ok, S} ->
	    ?DBG("start_agent -> started, Sup: ~p",[S]),
	    S;
	
	Else ->
	    ?DBG("start_agent -> unknown result: ~n~p",[Else]),
	    %% Get info about the apps we depend on
	    ?FAIL({start_failed,Else, ?IS_MNESIA_RUNNING()})
    end.

stop_sup(Pid, _) when (node(Pid) =:= node()) ->
    case (catch process_info(Pid)) of
	PI when is_list(PI) ->
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


%% --- start subagent supervisor ---

start_sub_sup(Node, Dir) ->
    rpc:call(Node, ?MODULE, start_sub_sup, [Dir]).
    
start_sub_sup(Dir) ->
    ?DBG("start_sub -> entry",[]),
    Opts = [{db_dir, Dir}, 
            {supervisor, [{verbosity, trace}]}],
    {ok, P} = snmpa_supervisor:start_sub_sup(Opts),
    unlink(P),
    {ok, {P, self()}}.


%% --- start and stop subagents ---

start_subagent(SaNode, RegTree, Mib) ->
    ?DBG("start_subagent -> entry with"
	"~n   SaNode:  ~p"
	"~n   RegTree: ~p"
	"~n   Mib:     ~p", [SaNode, RegTree, Mib]),
    MA = whereis(snmp_master_agent),
    ?DBG("start_subagent -> MA: ~p", [MA]),
    MibDir = get(mib_dir),
    Mib1   = join(MibDir,Mib),
    Mod    = snmpa_supervisor,
    Func   = start_sub_agent,
    Args   = [MA, RegTree, [Mib1]], 
    case rpc:call(SaNode, Mod, Func, Args) of
	{ok, SA} ->
	    ?DBG("start_subagent -> SA: ~p", [SA]),
	    {ok, SA};
	Error ->
	    ?FAIL({subagent_start_failed, SaNode, Error, [MA, RegTree, Mib1]})
    end.

stop_subagent(SA) ->
    ?DBG("stop_subagent -> entry with"
	 "~n   SA: ~p", [SA]),
    rpc:call(node(SA), snmpa_supervisor, stop_sub_agent, [SA]).


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


%% --- various mib load/unload functions ---

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


agent_info(Sup) ->
    ?DBG("agent_info -> entry with"
	 "~n   Sup: ~p", [Sup]),
    rpc:call(node(Sup), snmpa, info, []).


%% --- 

expect(Id, A) -> 
    Fun = fun() -> do_expect(A) end,
    expect2(Id, Fun).

expect(Id, A, B) ->          
    Fun = fun() -> do_expect(A, B) end,
    expect2(Id, Fun).
	 
expect(Id, A, B, C) -> 
    Fun = fun() -> do_expect(A, B, C) end,
    expect2(Id, Fun).

expect(Id, A, B, C, D, E) -> 
    Fun = fun() -> do_expect(A, B, C, D, E) end,
    expect2(Id, Fun).

expect2(Id, F) ->
    io:format("~w:expect2 -> entry with"
	      "~n   Id:   ~w"
	      "~n", [?MODULE, Id]),
    case F() of
	{error, Reason} ->
	    {error, Id, Reason};
	Else ->
	    io:format("~w:expect2 -> "
		      "~n   Id:   ~w"
		      "~n   Else: ~p"
		      "~n", [?MODULE, Id, Else]),
	    Else
    end.

	
%% ----------------------------------------------------------------------

get_timeout() ->
    get_timeout(os:type()).

get_timeout(vxworks) -> 7000;
get_timeout(_)       -> 3500.

receive_pdu(To) ->
    receive
	{snmp_pdu, PDU} when is_record(PDU, pdu) ->
	    PDU
    after To ->
	    {error, timeout}
    end.

receive_trap(To) ->
    receive
	{snmp_pdu, PDU} when is_record(PDU, trappdu) ->
	    PDU
    after To ->
	    {error, timeout}
    end.


do_expect(Expect) when is_atom(Expect) ->
    do_expect({Expect, get_timeout()});

do_expect({any_pdu, To}) 
  when is_integer(To) orelse (To =:= infinity) ->
    io:format("~w:do_expect(any_pdu) -> entry with"
	      "~n   To:   ~w"
	      "~n", [?MODULE, To]),
    receive_pdu(To);

do_expect({any_trap, To}) ->
    io:format("~w:do_expect(any_trap) -> entry with"
	      "~n   To:   ~w"
	      "~n", [?MODULE, To]),
    receive_trap(To);

do_expect({timeout, To}) ->
    io:format("~w:do_expect(timeout) -> entry with"
	      "~n   To:   ~w"
	      "~n", [?MODULE, To]),
    receive
	X ->
	    {error, {unexpected, X}}
    after 
	To ->
	    ok
    end;

do_expect({Err, To}) 
  when is_atom(Err) andalso (is_integer(To) orelse (To =:= infinity)) ->
    do_expect({{error, Err}, To});

do_expect({error, Err}) when is_atom(Err) ->
    Check = fun(_, R) -> R end,
    do_expect2(Check, any, Err, any, any, get_timeout());
do_expect({{error, Err}, To}) ->
    Check = fun(_, R) -> R end,
    do_expect2(Check, any, Err, any, any, To);

%% exp_varbinds() -> [exp_varbind()]
%% exp_varbind()  -> any | {Oid, any} | {Oid, Value}
%% Oid            -> [integer()]
%% Value          -> term()
%% ExpVBs         -> exp_varbinds() | {VbsCondition, exp_varbinds()}
do_expect(ExpVBs) ->
    Check = fun(_, R) -> R end,
    do_expect2(Check, 'get-response', noError, 0, ExpVBs, get_timeout()).


do_expect(v2trap, ExpVBs) ->
    Check = fun(_, R) -> R end,
    do_expect2(Check, 'snmpv2-trap', noError, 0, ExpVBs, get_timeout());


do_expect(report, ExpVBs) ->
    Check = fun(_, R) -> R end,
    do_expect2(Check, 'report', noError, 0, ExpVBs, get_timeout());


do_expect(inform, ExpVBs) ->
    do_expect({inform, true}, ExpVBs);

do_expect({inform, false}, ExpVBs) ->
    io:format("~w:do_expect(inform, false) -> entry with"
	      "~n   ExpVBs: ~p"
	      "~n", [?MODULE, ExpVBs]),
    Check = fun(_, R) -> R end,
    do_expect2(Check, 'inform-request', noError, 0, ExpVBs, get_timeout());

do_expect({inform, true}, ExpVBs) ->
    io:format("~w:do_expect(inform, true) -> entry with"
	      "~n   ExpVBs: ~p"
	      "~n", [?MODULE, ExpVBs]),
    Check = 
	fun(PDU, ok) ->
		RespPDU = PDU#pdu{type         = 'get-response',
				  error_status = noError,
				  error_index  = 0},
		snmp_test_mgr:rpl(RespPDU),
		ok;
	   (_, Err) ->
		Err
	end,
    do_expect2(Check, 'inform-request', noError, 0, ExpVBs, get_timeout());

do_expect({inform, {error, EStat, EIdx}}, ExpVBs) 
  when is_atom(EStat) andalso is_integer(EIdx) ->
    Check = 
	fun(PDU, ok) ->
		RespPDU = PDU#pdu{type         = 'get-response',
				  error_status = EStat,
				  error_index  = EIdx},
		snmp_test_mgr:rpl(RespPDU),
		ok;
	   (_, Err) ->
		Err
	end,
    do_expect2(Check, 'inform-request', noError, 0, ExpVBs, get_timeout()).


do_expect(Err, Idx, ExpVBs) ->
    do_expect(Err, Idx, ExpVBs, get_timeout()).

do_expect(Err, Idx, ExpVBs, To) 
  when is_atom(Err) andalso 
       (is_integer(Idx) orelse is_list(Idx) orelse (Idx == any)) ->
    Check = fun(_, R) -> R end,
    do_expect2(Check, 'get-response', Err, Idx, ExpVBs, To).


do_expect(Type, Enterp, Generic, Specific, ExpVBs) ->
    do_expect(Type, Enterp, Generic, Specific, ExpVBs, 3500).

do_expect(trap, Enterp, Generic, Specific, ExpVBs, To) ->
    io:format("~w:do_expect(trap) -> entry with"
	      "~n   Enterp:   ~w"
	      "~n   Generic:  ~w"
	      "~n   Specific: ~w"
	      "~n   ExpVBs:   ~w"
	      "~n   To:       ~w"
	      "~nwhen"
	      "~n   Time:   ~w"
	      "~n", [?MODULE, Enterp, Generic, Specific, ExpVBs, To, t()]),
    PureE = purify_oid(Enterp),
    case receive_trap(To) of
	#trappdu{enterprise    = PureE, 
		 generic_trap  = Generic,
		 specific_trap = Specific, 
		 varbinds      = VBs} ->
	    check_vbs(purify_oids(ExpVBs), VBs);

	#trappdu{enterprise    = Ent2, 
		 generic_trap  = G2,
		 specific_trap = Spec2, 
		 varbinds      = VBs} ->
	    {error, {unexpected_trap, 
		     {PureE, Generic, Specific, ExpVBs}, 
		     {Ent2, G2, Spec2, VBs}}};

	Error ->
	    Error
    end.


do_expect2(Check, Type, Err, Idx, ExpVBs, To) 
  when is_function(Check) andalso 
       is_atom(Type) andalso 
       is_atom(Err) andalso 
       (is_integer(Idx) orelse is_list(Idx) orelse (Idx =:= any)) andalso 
       (is_list(ExpVBs) orelse (ExpVBs =:= any)) andalso
       (is_integer(To) orelse (To =:= infinity)) ->

    io:format("~w:do_expect2 -> entry with"
	      "~n   Type:   ~w"
	      "~n   Err:    ~w"
	      "~n   Idx:    ~w"
	      "~n   ExpVBs: ~w"
	      "~n   To:     ~w"
	      "~nwhen"
	      "~n   Time:   ~w"
	      "~n", [?MODULE, Type, Err, Idx, ExpVBs, To, t()]),

    case receive_pdu(To) of

	#pdu{type         = Type, 
	     error_status = Err,
	     error_index  = Idx} when ExpVBs =:= any -> 
	    ok;

	#pdu{type         = Type, 
	     request_id   = ReqId, 
	     error_status = Err2,
	     error_index  = Idx} when ExpVBs =:= any -> 
	    {error, {unexpected_error_status, Err, Err2, ReqId}};

	#pdu{error_status = Err} when (Type   =:= any) andalso 
				      (Idx    =:= any) andalso 
				      (ExpVBs =:= any) -> 
	    ok;

	#pdu{request_id   = ReqId, 
	     error_status = Err2} when (Type   =:= any) andalso 
				       (Idx    =:= any) andalso 
				       (ExpVBs =:= any) -> 
	    {error, {unexpected_error_status, Err, Err2, ReqId}};

	#pdu{type         = Type, 
	     error_status = Err} when (Idx =:= any) andalso 
				      (ExpVBs =:= any) -> 
	    ok;

	#pdu{type         = Type, 
	     request_id   = ReqId, 
	     error_status = Err2} when (Idx =:= any) andalso 
				       (ExpVBs =:= any) -> 
	    {error, {unexpected_error_status, Err, Err2, ReqId}};

	#pdu{type         = Type, 
	     request_id   = ReqId, 
	     error_status = Err,
	     error_index  = EI} when is_list(Idx) andalso (ExpVBs =:= any) -> 
	    case lists:member(EI, Idx) of
		true ->
		    ok;
		false ->
		    {error, {unexpected_error_index, EI, Idx, ReqId}}
	    end;

	#pdu{type         = Type, 
	     request_id   = ReqId, 
	     error_status = Err2,
	     error_index  = EI} when is_list(Idx) andalso (ExpVBs =:= any) -> 
	    case lists:member(EI, Idx) of
		true ->
		    {error, {unexpected_error_status, Err, Err2, ReqId}};
		false ->
		    {error, {unexpected_error, {Err, Idx}, {Err2, EI}, ReqId}}
	    end;

	#pdu{type         = Type2, 
	     request_id   = ReqId, 
	     error_status = Err2, 
	     error_index  = Idx2} when ExpVBs =:= any ->
	    {error, 
	     {unexpected_pdu, 
	      {Type, Err, Idx}, {Type2, Err2, Idx2}, ReqId}};

	#pdu{type         = Type, 
	     error_status = Err, 
	     error_index  = Idx,
	     varbinds     = VBs} = PDU ->
	    Check(PDU, check_vbs(purify_oids(ExpVBs), VBs));

	#pdu{type         = Type, 
	     error_status = Err, 
	     varbinds     = VBs} = PDU when Idx =:= any ->
	    Check(PDU, check_vbs(purify_oids(ExpVBs), VBs));

	#pdu{type         = Type, 
	     request_id   = ReqId, 
	     error_status = Err, 
	     error_index  = EI,
	     varbinds     = VBs} = PDU when is_list(Idx) ->
	    PureVBs = purify_oids(ExpVBs), 
	    case lists:member(EI, Idx) of
		true ->
		    Check(PDU, check_vbs(PureVBs, VBs));
		false ->
		    {error, {unexpected_error_index, Idx, EI, ReqId}}
	    end;

	#pdu{type         = Type2, 
	     request_id   = ReqId, 
	     error_status = Err2, 
	     error_index  = Idx2,
	     varbinds     = VBs2} ->
	    {error, 
	     {unexpected_pdu, 
	      {Type,  Err,  Idx, purify_oids(ExpVBs)}, 
	      {Type2, Err2, Idx2, VBs2}, 
	      ReqId}};
	
	Error ->
	    Error
    end.



check_vbs([], []) ->		
    ok;
check_vbs(Exp, []) ->
    {error, {to_few_vbs, Exp}};
check_vbs([], VBs) ->
    {error, {to_many_vbs, VBs}};
check_vbs([any|Exp], [_|VBs]) ->
    check_vbs(Exp, VBs);
check_vbs([{Oid, any}|Exp], [#varbind{oid = Oid}|VBs]) ->
    check_vbs(Exp, VBs);
check_vbs([{Oid, Val}|Exp], [#varbind{oid = Oid, value = Val}|VBs]) ->
    check_vbs(Exp, VBs);
check_vbs([{Oid, Val1}|_], [#varbind{oid = Oid, value = Val2}|_]) ->
    {error, {unexpected_vb_value, Oid, Val1, Val2}};
check_vbs([{Oid1, _}|_], [#varbind{oid = Oid2}|_]) ->
    {error, {unexpected_vb_oid, Oid1, Oid2}}.


purify_oids({VbsCondition, VBs}) 
  when ((VbsCondition =:= true) orelse (VbsCondition =:= false)) andalso 
       is_list(VBs) ->
    {VbsCondition, do_purify_oids(VBs)};
purify_oids(VBs) when is_list(VBs) ->
    do_purify_oids(VBs).

do_purify_oids([]) -> 
    [];
do_purify_oids([{XOid, Q}|T]) ->
    [{purify_oid(XOid), Q} | do_purify_oids(T)].


purify_oid(Oid) ->
    io:format("~w:purify_oid -> entry with"
	      "~n   Oid:      ~w"
	      "~n", 
	      [?MODULE, Oid]), 
    case (catch snmp_test_mgr:purify_oid(Oid)) of
	{error, Reason} ->
	    io:format("~w:purify_oid -> error: "
		      "~n   Reason: ~p"
		      "~n", 
		      [?MODULE, Reason]),
	    exit({malformed_oid, Reason});
	{ok, Oid2} when is_list(Oid2) -> 
	    io:format("~w:purify_oid -> ok: "
		      "~n   Oid2: ~p"
		      "~n", 
		      [?MODULE, Oid2]),
	    Oid2;
	Error ->
	    io:format("~w:purify_oid -> unexpected return value: "
		      "~n   Error: ~p"
		      "~n", 
		      [?MODULE, Error]),
	    exit({unexpected_purify_result, Error})
	    
    end.


%% ----------------------------------------------------------------------

get_req(Id, Vars) ->
    ?DBG("get_req -> entry with"
	   "~n   Id:   ~p"
	   "~n   Vars: ~p",[Id,Vars]),
    snmp_test_mgr:g(Vars),
    ?DBG("get_req -> await response",[]),
    case snmp_test_mgr:get_response(Id, Vars) of
	{ok, Val} ->
	    ?DBG("get_req -> response: ~p",[Val]),
	    Val;
	{error, _, {ExpFmt, ExpArg}, {ActFmt, ActArg}} ->
	    ?DBG("get_req -> error for ~p: "
		 "~n   " ++ ExpFmt ++ 
		 "~n   " ++ ActFmt, 
		 [Id] ++ ExpArg ++ ActArg),
	    exit({unexpected_response, ExpArg, ActArg});
	Error ->
	    ?DBG("get_req -> error: ~n~p",[Error]),
	    exit({unknown, Error})
    end.


get_next_req(Vars) ->
    ?DBG("get_next_req -> entry with"
	 "~n   Vars: ~p",[Vars]),
    snmp_test_mgr:gn(Vars),
    ?DBG("get_next_req -> await response",[]),
    Response = snmp_test_mgr:receive_response(),
    ?DBG("get_next_req -> response: ~p",[Response]),
    Response.


%% --- start and stop nodes ---

start_node(Name) ->
    ?LOG("start_node -> entry with"
	 "~n   Name: ~p"
	 "~n when"
	 "~n   hostname of this node: ~p",
	 [Name, list_to_atom(?HOSTNAME(node()))]),
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
	    Conf = [{"agentEngine", "all-rights", "all-rights", zeroDotZero, 
		     usmNoAuthProtocol, "", "", 
		     usmNoPrivProtocol, "", "", "", "", ""}, 

		    {"agentEngine", "no-rights", "no-rights", zeroDotZero, 
		     usmNoAuthProtocol, "", "", 
		     usmNoPrivProtocol, "", "", "", "", ""}, 

		    {"agentEngine", "authMD5", "authMD5", zeroDotZero, 
		     usmHMACMD5AuthProtocol, "", "", 
		     usmNoPrivProtocol, "", "", "", "passwd_md5xxxxxx", ""}, 

		    {"agentEngine", "authSHA", "authSHA", zeroDotZero, 
		     usmHMACSHAAuthProtocol, "", "", 
		     usmNoPrivProtocol, "", "", "", 
		     "passwd_shaxxxxxxxxxx", ""}, 

		    {"agentEngine", "privDES", "privDES", zeroDotZero, 
		     usmHMACSHAAuthProtocol, "", "", 
		     usmDESPrivProtocol, "", "", "", 
		     "passwd_shaxxxxxxxxxx", "passwd_desxxxxxx"}, 

		    {"mgrEngine", "all-rights", "all-rights", zeroDotZero, 
		     usmNoAuthProtocol, "", "", 
		     usmNoPrivProtocol, "", "", "", "", ""}, 

		    {"mgrEngine", "no-rights", "no-rights", zeroDotZero, 
		     usmNoAuthProtocol, "", "", 
		     usmNoPrivProtocol, "", "", "", "", ""}, 

		    {"mgrEngine", "authMD5", "authMD5", zeroDotZero, 
		     usmHMACMD5AuthProtocol, "", "", 
		     usmNoPrivProtocol, "", "", "", "passwd_md5xxxxxx", ""}, 

		    {"mgrEngine", "authSHA", "authSHA", zeroDotZero, 
		     usmHMACSHAAuthProtocol, "", "", 
		     usmNoPrivProtocol, "", "", "", 
		     "passwd_shaxxxxxxxxxx", ""}, 

		    {"mgrEngine", "privDES", "privDES", zeroDotZero, 
		     usmHMACSHAAuthProtocol, "", "", 
		     usmDESPrivProtocol, "", "", "", 
		     "passwd_shaxxxxxxxxxx", "passwd_desxxxxxx"}],
	    ?line ok = snmp_config:update_agent_usm_config(Dir, Conf),
	    true;
	false ->
	    false
    end.
    
update_usm_mgr(Vsns, Dir) ->
    case lists:member(v3, Vsns) of
	true ->
	    Conf = [{"agentEngine", "newUser", "newUser", zeroDotZero, 
		     usmHMACSHAAuthProtocol, "", "", 
		     usmDESPrivProtocol, "", "", "", 
		     "passwd_shaxxxxxxxxxx", "passwd_desxxxxxx"}, 
		    
		    {"mgrEngine", "newUser", "newUser", zeroDotZero, 
		     usmHMACSHAAuthProtocol, "", "", 
		     usmDESPrivProtocol, "", "", "", 
		     "passwd_shaxxxxxxxxxx", "passwd_desxxxxxx"}], 

	    ?line ok = snmp_config:update_agent_usm_config(Dir, Conf),
	    true;
	false ->
	    false
    end.

rewrite_usm_mgr(Dir, ShaKey, DesKey) -> 
    ?line ok = file:rename(filename:join(Dir,"usm.conf"),
			   filename:join(Dir,"usm.old")),
    Conf = [{"agentEngine", "newUser", "newUser", zeroDotZero, 
	     usmHMACSHAAuthProtocol, "", "", 
	     usmDESPrivProtocol, "", "", "", ShaKey, DesKey}, 
	    {"mgrEngine", "newUser", "newUser", zeroDotZero, 
	     usmHMACSHAAuthProtocol, "", "", 
	     usmDESPrivProtocol, "", "", "", ShaKey, DesKey}], 
    ok = snmp_config:write_agent_usm_config(Dir, "", Conf).

reset_usm_mgr(Dir) ->
    ?line ok = file:rename(filename:join(Dir,"usm.old"),
			   filename:join(Dir,"usm.conf")).


update_community([v3], _Dir) -> 
    ok;
update_community(_, Dir) ->
    Conf = [{"no-rights", "no-rights", "no-rights", "", ""}],
    ?line ok = snmp_config:update_agent_community_config(Dir, Conf).
    
    
-define(tDescr_instance, [1,3,6,1,2,1,16,1,0]).
update_vacm(_Vsn, Dir) ->
    Conf = [{vacmSecurityToGroup, usm, "authMD5", "initial"}, 
	    {vacmSecurityToGroup, usm, "authSHA", "initial"}, 
	    {vacmSecurityToGroup, usm, "privDES", "initial"}, 
	    {vacmSecurityToGroup, usm, "newUser", "initial"},
	    {vacmViewTreeFamily, "internet", ?tDescr_instance, 
	     excluded, null}],
    ?line ok = snmp_config:update_agent_vacm_config(Dir, Conf).
    
    
write_community_conf(Dir, Conf) ->
    snmp_config:write_agent_community_config(Dir, "", Conf).

write_target_addr_conf(Dir, Conf) ->
    snmp_config:write_agent_target_addr_config(Dir, "", Conf).

write_target_addr_conf(Dir, ManagerIp, UDP, Vsns) -> 
    snmp_config:write_agent_snmp_target_addr_conf(Dir, ManagerIp, UDP, Vsns).

rewrite_target_addr_conf(Dir, NewPort) -> 
    ?DBG("rewrite_target_addr_conf -> entry with"
	 "~n   NewPort: ~p", [NewPort]),
    TAFile = filename:join(Dir, "target_addr.conf"),
    case file:read_file_info(TAFile) of
	{ok, _} -> 
	    ok;
	{error, R} -> 
	    ?ERR("failure reading file info of "
		 "target address config file: ~p",[R]),
	    ok  
    end,

    ?line [TrapAddr|Addrs] = 
	snmp_conf:read(TAFile, 
		       fun(R) -> rewrite_target_addr_conf_check(R) end),

    ?DBG("rewrite_target_addr_conf -> TrapAddr: ~p",[TrapAddr]),

    NewAddrs = [rewrite_target_addr_conf2(NewPort,TrapAddr)|Addrs],
    
    ?DBG("rewrite_target_addr_conf -> NewAddrs: ~p",[NewAddrs]),

    ?line ok = file:rename(filename:join(Dir,"target_addr.conf"),
			   filename:join(Dir,"target_addr.old")),

    ?line ok = snmp_config:write_agent_target_addr_config(Dir, "", NewAddrs).

rewrite_target_addr_conf_check(O) -> 
    {ok,O}.

rewrite_target_addr_conf2(NewPort,
			  {Name, Ip, _Port, Timeout, Retry,
			   "std_trap", EngineId}) -> 
    ?LOG("rewrite_target_addr_conf2 -> entry with std_trap",[]),
    {Name,Ip,NewPort,Timeout,Retry,"std_trap",EngineId};
rewrite_target_addr_conf2(_NewPort,O) -> 
    ?LOG("rewrite_target_addr_conf2 -> entry with "
	 "~n   O: ~p",[O]),
    O.

reset_target_addr_conf(Dir) ->
    ?line ok = file:rename(filename:join(Dir,"target_addr.old"),
			   filename:join(Dir,"target_addr.conf")).

write_target_params_conf(Dir, Vsns) -> 
    F = fun(v1) -> {"target_v1", v1,  v1,  "all-rights", noAuthNoPriv};
	   (v2) -> {"target_v2", v2c, v2c, "all-rights", noAuthNoPriv};
	   (v3) -> {"target_v3", v3,  usm, "all-rights", noAuthNoPriv}
	end,
    Conf = [F(Vsn) || Vsn <- Vsns],
    snmp_config:write_agent_target_params_config(Dir, "", Conf).

rewrite_target_params_conf(Dir, SecName, SecLevel) 
  when is_list(SecName) andalso is_atom(SecLevel) -> 
    ?line ok = file:rename(filename:join(Dir,"target_params.conf"),
			   filename:join(Dir,"target_params.old")),
    Conf = [{"target_v3", v3, usm, SecName, SecLevel}],
    snmp_config:write_agent_target_params_config(Dir, "", Conf).

reset_target_params_conf(Dir) ->
    ?line ok = file:rename(filename:join(Dir,"target_params.old"),
			   filename:join(Dir,"target_params.conf")).

write_notify_conf(Dir) -> 
    Conf = [{"standard trap",   "std_trap",   trap}, 
	    {"standard inform", "std_inform", inform}],
    snmp_config:write_agent_notify_config(Dir, "", Conf).

write_view_conf(Dir) -> 
    Conf = [{2, [1,3,6], included, null},
	    {2, ?tDescr_instance, excluded, null}], 
    snmp_config:write_agent_view_config(Dir, "", Conf).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

copy_file(From, To) ->
    {ok, Bin} = file:read_file(From),
    ok = file:write_file(To, Bin).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

display_memory_usage() ->
    Info       = snmpa:info(snmp_master_agent),
    TreeSize   = key1search(tree_size_bytes,  Info),
    ProcMem    = key1search(process_memory,   Info),
    MibDbSize  = key1search([db_memory,mib],  Info),
    NodeDbSize = key1search([db_memory,node], Info),
    TreeDbSize = key1search([db_memory,tree], Info),
    ?INF("Memory usage: "
	"~n   Tree size:           ~p"
	"~n   Process memory size: ~p"
	"~n   Mib db size:         ~p"
	"~n   Node db size:        ~p"
	"~n   Tree db size:        ~p", 
    [TreeSize, ProcMem, MibDbSize, NodeDbSize, TreeDbSize]).
    
key1search([], Res) ->
    Res;
key1search([Key|Keys], List) when is_atom(Key) andalso is_list(List) ->
    case lists:keysearch(Key, 1, List) of
	{value, {Key, Val}} ->
	    key1search(Keys, Val);
	false ->
	    undefined
    end;
key1search(Key, List) when is_atom(Key) ->
    case lists:keysearch(Key, 1, List) of
	{value, {Key, Val}} ->
	    Val;
	false ->
	    undefined
    end.


regs() ->
    lists:sort(registered()).


rpc(Node, F, A) ->
    rpc:call(Node, snmpa, F, A).


%% await_pdu(To) ->
%%     await_response(To, pdu).
%% 
%% await_trap(To) ->
%%     await_response(To, trap).
%% 
%% await_any(To) ->
%%     await_response(To, any).
%% 
%% 
%% await_response(To, What) ->
%%     await_response(To, What, []).
%% 
%% await_response(To, What, Stuff) when is_integer(To) andalso (To >= 0) ->
%%     T = t(),
%%     receive
%% 	{snmp_pdu, PDU} when is_record(Trap, pdu) andalso (What =:= pdu) ->
%% 	    {ok, PDU};
%% 	{snmp_pdu, Trap} is_when record(Trap, trappdu) andalso (What =:= trap) ->
%% 	    {ok, Trap};
%% 	Any when What =:= any ->
%% 	    {ok, Any};
%% 	Any ->
%% 	    %% Recalc time
%% 	    NewTo = To - (t() - T)
%% 	    await_reponse(NewTo, What, [{NewTo, Any}|Stuff])
%%     after To ->
%% 	    {error, {timeout, Stuff}}
%%     end;
%% await_response(_, Stuff) ->
%%     {error, {timeout, Stuff}}.
%% 
%% 
%% t() ->
%%     {A,B,C} = erlang:now(),
%%     A*1000000000+B*1000+(C div 1000).
%% 
%% 
%% timeout() ->
%%     timeout(os:type()).
%% 
%% timeout(vxworks) -> 7000;
%% timeout(_)       -> 3500.
    

%% Time in milli seconds
t() ->
    {A,B,C} = erlang:now(),
    A*1000000000+B*1000+(C div 1000).
