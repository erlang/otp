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

-module(snmp_agent_test_lib).


-export([
	 start_v1_agent/1,        start_v1_agent/2, 
	 start_v2_agent/1,        start_v2_agent/2, 
	 start_v3_agent/1,        start_v3_agent/2, 
	 start_bilingual_agent/1, start_bilingual_agent/2, 
	 start_mt_agent/1,        start_mt_agent/2, 
	 stop_agent/1,

	 %% start_sup/0,      stop_sup/2,
	 start_subagent/3, stop_subagent/1, 
	 start_sub_sup/1,  start_sub_sup/2, 

	 start_node/1, stop_node/1,

	 load_master/1, load_master_std/1, unload_master/1, 
	 loaded_mibs/0, unload_mibs/1,

	 get_req/2, get_next_req/1,

	 config/5, config/6,
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
	 expect/3, expect/4, expect/5, expect/7, 
	 
	 regs/0,
	 rpc/3
	]).

%% Internal exports
-export([wait/5, run/4]).

-include_lib("kernel/include/file.hrl").
-include_lib("common_test/include/ct.hrl").
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

init_all(Config) when is_list(Config) ->

    ?LOG("init_all -> entry with"
	 "~n   Config: ~p",[Config]),

    %% -- 
    %% Start nodes
    %% 

    ?line {ok, SaNode}  = start_node(snmp_sa),
    ?line {ok, MgrNode} = start_node(snmp_mgr),


    %% -- 
    %% Create necessary files ( and dirs ) 
    %% 

    SuiteTopDir = ?config(snmp_suite_top_dir, Config),
    ?DBG("init_all -> SuiteTopDir ~p", [SuiteTopDir]),

    AgentDir = join(SuiteTopDir, "agent/"), 
    ?line ok = file:make_dir(AgentDir),
    ?DBG("init_all -> AgentDir ~p", [AgentDir]),

    AgentDbDir = join(AgentDir, "db/"), 
    ?line ok   = file:make_dir(AgentDbDir),
    ?DBG("init_all -> AgentDbDir ~p", [AgentDbDir]),

    AgentLogDir = join(AgentDir, "log/"), 
    ?line ok    = file:make_dir(AgentLogDir),
    ?DBG("init_all -> AgentLogDir ~p", [AgentLogDir]),

    AgentConfDir = join(AgentDir, "conf/"), 
    ?line ok     = file:make_dir(AgentConfDir),
    ?DBG("init_all -> AgentConfDir ~p", [AgentConfDir]),

    MgrDir   = join(SuiteTopDir, "mgr/"), 
    ?line ok = file:make_dir(MgrDir),
    ?DBG("init_all -> MgrDir ~p", [MgrDir]),

    SaDir    = join(SuiteTopDir, "sa/"), 
    ?line ok = file:make_dir(SaDir),
    ?DBG("init_all -> SaDir ~p", [SaDir]),

    SaDbDir  = join(SaDir, "db/"), 
    ?line ok = file:make_dir(SaDbDir),
    ?DBG("init_all -> SaDbDir ~p", [SaDbDir]),

    %% MibDir = ?config(mib_dir, Config),
    %% ?DBG("init_all -> MibDir ~p", [DataDir]),


    %% -- 
    %% Start and initiate mnesia
    %% 

    ?DBG("init_all -> load application mnesia", []),
    ?line ok = application:load(mnesia),

    ?DBG("init_all -> load application mnesia on node ~p", [SaNode]),
    ?line ok = rpc:call(SaNode, application, load, [mnesia]),
    
    ?DBG("init_all -> application mnesia: set_env dir",[]),
    ?line application_controller:set_env(mnesia, dir, 
					 join(AgentDbDir, "Mnesia1")),

    ?DBG("init_all -> application mnesia: set_env dir on node ~p",[SaNode]),
    ?line rpc:call(SaNode, application_controller, set_env, 
		   [mnesia, dir,  join(SaDir, "Mnesia2")]),

    ?DBG("init_all -> create mnesia schema",[]),
    ?line ok = mnesia:create_schema([SaNode, node()]),
    
    ?DBG("init_all -> start application mnesia",[]),
    ?line ok = application:start(mnesia),

    ?DBG("init_all -> start application mnesia on ~p",[SaNode]),
    ?line ok = rpc:call(SaNode, application, start, [mnesia]),
    Ip = ?LOCALHOST(),
    [{snmp_sa,        SaNode}, 
     {snmp_mgr,       MgrNode}, 
     {snmp_master,    node()}, 
     {agent_dir,      AgentDir ++ "/"},
     {agent_db_dir,   AgentDbDir ++ "/"},
     {agent_log_dir,  AgentLogDir ++ "/"},
     {agent_conf_dir, AgentConfDir ++ "/"},
     {sa_dir,         SaDir ++ "/"}, 
     {sa_db_dir,      SaDbDir ++ "/"}, 
     {mgr_dir,        MgrDir ++ "/"},
     %% {mib_dir,        DataDir}, 
     {ip,             Ip} | 
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

    SaNode     = ?config(snmp_sa,     Config),
    MgrNode    = ?config(snmp_mgr,    Config),
    MasterNode = ?config(snmp_master, Config),
    %% MasterNode = node(),
    IpFamily  = proplists:get_value(ipfamily, Config, inet),
    
    SaHost         = ?HOSTNAME(SaNode),
    MgrHost        = ?HOSTNAME(MgrNode),
    MasterHost     = ?HOSTNAME(MasterNode),
    {ok, MasterIP} = snmp_misc:ip(MasterHost, IpFamily),
    {ok, MIP}      = snmp_misc:ip(MgrHost, IpFamily),
    {ok, SIP}      = snmp_misc:ip(SaHost, IpFamily),


    put(mgr_node,    MgrNode),
    put(sa_node,     SaNode),
    put(master_node, MasterNode),
    put(sa_host,     SaHost),
    put(mgr_host,    MgrHost),
    put(master_host, MasterHost),
    put(mip,         tuple_to_list(MIP)),
    put(masterip,    tuple_to_list(MasterIP)),
    put(sip,         tuple_to_list(SIP)),
    put(ipfamily,    IpFamily),
    
    MibDir = ?config(mib_dir, Config),
    put(mib_dir, MibDir),
    StdM = join(code:priv_dir(snmp), "mibs") ++ "/",
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
	{done, Ret, _Zed} -> 
	    ?DBG("call -> done:"
		 "~n   Ret: ~p"
		 "~n   Zed: ~p", [Ret, _Zed]),
	    case Ret of
		{error, Reason} ->
		    exit(Reason);
		OK ->
		    OK
	    end
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
    _CryptoRes = ?CRYPTO_START(),
    ?DBG("run -> Crypto: ~p", [_CryptoRes]),
    catch snmp_test_mgr:stop(), % If we had a running mgr from a failed case
    StdM = join(code:priv_dir(snmp), "mibs") ++ "/",
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
			      {ipfamily, get(ipfamily)},
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
 
start_bilingual_agent(Config, Opts) 
  when is_list(Config) andalso is_list(Opts) ->
    start_agent(Config, [v1,v2], Opts).
 
start_mt_agent(Config) when is_list(Config) ->
    start_agent(Config, [v2], [{multi_threaded, true}]).
 
start_mt_agent(Config, Opts) when is_list(Config) andalso is_list(Opts) ->
    start_agent(Config, [v2], [{multi_threaded, true}|Opts]).
 
start_agent(Config, Vsns) ->
    start_agent(Config, Vsns, []).
start_agent(Config, Vsns, Opts) -> 

    ?LOG("start_agent -> entry (~p) with"
	"~n   Config: ~p"
	"~n   Vsns:   ~p"
	"~n   Opts:   ~p", [node(), Config, Vsns, Opts]),
    
    ?line AgentLogDir  = ?config(agent_log_dir,  Config),
    ?line AgentConfDir = ?config(agent_conf_dir, Config),
    ?line AgentDbDir   = ?config(agent_db_dir,   Config),
    ?line SaNode       = ?config(snmp_sa,        Config),

    Env = app_agent_env_init(
	    [{versions,         Vsns}, 
	     {agent_type,       master},
	     {agent_verbosity,  trace},
	     {db_dir,           AgentDbDir},
	     {audit_trail_log,  [{type, read_write},
				 {dir,  AgentLogDir},
				 {size, {10240, 10}}]},
	     {config,           [{dir, AgentConfDir},
				 {force_load, false}, 
				 {verbosity,  trace}]}, 
	     {local_db,         [{repair,    true},
				 {verbosity, log}]}, 
	     {mib_server,       [{verbosity, log}]},
	     {symbolic_store,   [{verbosity, log}]},
	     {note_store,       [{verbosity, log}]},
	     {net_if,           [{verbosity, trace}]}],
	    Opts),
    

    process_flag(trap_exit,true),

    {ok, AppSup} = snmp_app_sup:start_link(),
    unlink(AppSup),
    ?DBG("start_agent -> snmp app supervisor: ~p", [AppSup]),

    ?DBG("start_agent -> start master agent",[]),
    ?line Sup = start_sup(Env), 

    ?DBG("start_agent -> unlink from supervisor", []),
    ?line unlink(Sup),
    ?line SaDir = ?config(sa_dir, Config),
    ?DBG("start_agent -> (rpc) start sub on ~p", [SaNode]),
    ?line {ok, Sub} = start_sub_sup(SaNode, SaDir),
    ?DBG("start_agent -> done",[]),
    ?line [{snmp_sup, {Sup, self()}}, {snmp_sub, Sub} | Config].


app_agent_env_init(Env0, Opts) ->
    ?DBG("app_agent_env_init -> unload snmp",[]),
    ?line application:unload(snmp),

    ?DBG("app_agent_env_init -> load snmp",[]),
    ?line application:load(snmp),

    ?DBG("app_agent_env_init -> "
	 "merge or maybe replace (snmp agent) app env",[]),
    Env = add_or_maybe_merge_agent_env(Opts, Env0),
    ?DBG("app_agent_env_init -> merged env: "
	 "~n   ~p", [Env]),

    %% We put it into the app environment just as 
    %% a precaution, since when starting normally, 
    %% this is where the environment is extracted from.
    app_agent_set_env(Env),
    Env.

app_agent_set_env(Value) ->
    application_controller:set_env(snmp, agent, Value).

add_or_maybe_merge_agent_env([], Env) ->
    ?DBG("merging agent env -> merged", []),
    lists:keysort(1, Env);
add_or_maybe_merge_agent_env([{Key, Value1}|Opts], Env) ->
    ?DBG("merging agent env -> add, replace or merge ~p", [Key]),
    case lists:keysearch(Key, 1, Env) of
	{value, {Key, Value1}} ->
	    %% Identical, move on
	    ?DBG("merging agent env -> "
		 "no need to merge ~p - identical - keep: "
		 "~n   ~p", [Key, Value1]),
	    add_or_maybe_merge_agent_env(Opts, Env);
	{value, {Key, Value2}} ->
	    %% Another value, merge or replace
	    NewValue = merge_or_replace_agent_env(Key, Value1, Value2),
	    Env2 = lists:keyreplace(Key, 1, Env, {Key, NewValue}), 
	    add_or_maybe_merge_agent_env(Opts, Env2);
	false ->
	    ?DBG("merging agent env -> no old ~p to merge with - add: "
		 "~n   ~p", [Key, Value1]),
	    add_or_maybe_merge_agent_env(Opts, [{Key, Value1}|Env])
    end.

merge_or_replace_agent_env(versions, NewVersions, _OldVersions) ->
    ?DBG("merging agent env -> versions replaced: ~p -> ~p", 
	 [NewVersions, _OldVersions]),
    NewVersions;
merge_or_replace_agent_env(agent_type, NewType, _OldType) ->
    ?DBG("merging agent env -> agent type replaced: ~p -> ~p", 
	 [NewType, _OldType]),
    NewType;
merge_or_replace_agent_env(agent_verbosity, NewVerbosity, _OldVerbosity) ->
    ?DBG("merging agent env -> agent verbosity replaced: ~p -> ~p", 
	 [NewVerbosity, _OldVerbosity]),
    NewVerbosity;
merge_or_replace_agent_env(db_dir, NewDbDir, _OldDbDir) ->
    ?DBG("merging agent env -> db-dir replaced: ~p -> ~p", 
	 [NewDbDir, _OldDbDir]),
    NewDbDir;
merge_or_replace_agent_env(audit_trail_log, NewATL, OldATL) ->
    merge_or_replace_agent_env_atl(NewATL, OldATL);
merge_or_replace_agent_env(config, NewConfig, OldConfig) ->
    merge_or_replace_agent_env_config(NewConfig, OldConfig);
merge_or_replace_agent_env(local_db, NewLdb, OldLdb) ->
    merge_or_replace_agent_env_ldb(NewLdb, OldLdb);
merge_or_replace_agent_env(mib_storage, NewMst, OldMst) ->
    merge_or_replace_agent_env_mib_storage(NewMst, OldMst);
merge_or_replace_agent_env(mib_server, NewMibs, OldMibs) ->
    merge_or_replace_agent_env_mib_server(NewMibs, OldMibs);
merge_or_replace_agent_env(symbolic_store, NewSymStore, OldSymStore) ->
    merge_or_replace_agent_env_symbolic_store(NewSymStore, OldSymStore);
merge_or_replace_agent_env(note_store, NewNoteStore, OldNoteStore) ->
    merge_or_replace_agent_env_note_store(NewNoteStore, OldNoteStore);
merge_or_replace_agent_env(net_if, NewNetIf, OldNetIf) ->
    merge_or_replace_agent_env_net_if(NewNetIf, OldNetIf);
merge_or_replace_agent_env(Key, NewValue, OldValue) ->
    ?FAIL({not_implemented_merge_or_replace, 
	   Key, NewValue, OldValue}).

merge_or_replace_agent_env_atl(New, Old) ->
    ATL = merge_agent_options(New, Old),
    ?DBG("merging agent env -> audit-trail-log merged: "
	 "~n   ~p | ~p -> ~p", [New, Old, ATL]),
    ATL.

merge_or_replace_agent_env_config(New, Old) ->
    Config = merge_agent_options(New, Old),
    case lists:keymember(dir, 1, Config) of
	true ->
	    ?DBG("merging agent env -> config merged: "
		 "~n   ~p | ~p -> ~p", [New, Old, Config]),
	    Config;
	false ->
	    ?FAIL({missing_mandatory_option, {config, dir}})
    end.

merge_or_replace_agent_env_ldb(New, Old) ->
    LDB = merge_agent_options(New, Old),
    ?DBG("merging agent env -> local-db merged: "
	 "~n   ~p | ~p -> ~p", [New, Old, LDB]),
    LDB.

merge_or_replace_agent_env_mib_storage(NewMibStorage, OldMibStorage) ->
    %% Shall we merge or replace?
    %% module is mandatory. We will only merge if NewModule is
    %% equal to OldModule. 
    NewModule = 
	case lists:keysearch(module, 1, NewMibStorage) of
	    {value, {module, M}} ->
		M;
	    false ->
		?FAIL({missing_mandatory_option, {mib_storage, module}})
	end,
    case lists:keysearch(module, 1, OldMibStorage) of
	{value, {module, NewModule}} ->
	    %% Same module => merge
	    %% Non-ex new options => remove
	    %% Ex new options and non-ex old options => replace
	    %% Otherwise merge
	    case lists:keysearch(options, 1, NewMibStorage) of
		false ->
		    ?DBG("merging agent env -> "
			 "no mib-storage ~p merge needed - "
			 "no new options (= remove old options)", [NewModule]),
		    NewMibStorage;
		{value, {options, NewOptions}} ->
		    case lists:keysearch(options, 1, OldMibStorage) of
			false ->
			    ?DBG("merging agent env -> "
				 "no mib-storage ~p merge needed - "
				 "no old options", [NewModule]),
			    NewMibStorage;
			{value, {options, OldOptions}} ->
			    MergedOptions = 
				merge_agent_options(NewOptions, OldOptions), 
			    ?DBG("merging agent env -> mib-storage ~p merged: "
				 "~n   Options: ~p | ~p -> ~p", 
				 [NewModule, 
				  NewOptions, OldOptions, MergedOptions]),
			    [{module,  NewModule}, 
			     {options, MergedOptions}]
		    end
	    end;
	_ ->
	    %% Diff module => replace
	    ?DBG("merging agent env -> "
		 "no mib-storage ~p merge needed - "
		 "new module", [NewModule]),
	    NewMibStorage
    end.

merge_or_replace_agent_env_mib_server(New, Old) ->
    MibServer = merge_agent_options(New, Old),
    ?DBG("merging agent env -> mib-server merged: "
	 "~n   ~p | ~p -> ~p", [New, Old, MibServer]),
    MibServer.

merge_or_replace_agent_env_symbolic_store(New, Old) ->
    SymbolicStore = merge_agent_options(New, Old),
    ?DBG("merging agent env -> symbolic-store merged: "
	 "~n   ~p | ~p -> ~p", [New, Old, SymbolicStore]),
    SymbolicStore.

merge_or_replace_agent_env_note_store(New, Old) ->
    NoteStore = merge_agent_options(New, Old),
    ?DBG("merging agent env -> note-store merged: "
	 "~n   ~p | ~p -> ~p", [New, Old, NoteStore]),
    NoteStore.

merge_or_replace_agent_env_net_if(New, Old) ->
    NetIf = merge_agent_options(New, Old),
    ?DBG("merging agent env -> net-if merged: "
	 "~n   ~p | ~p -> ~p", [New, Old, NetIf]),
    NetIf.

merge_agent_options([], Options) ->
    lists:keysort(1, Options);
merge_agent_options([{Key, _Value} = Opt|Opts], Options) ->
    case lists:keysearch(Key, 1, Options) of
	{value, _} ->
	    NewOptions = lists:keyreplace(Key, 1, Options, Opt), 
	    merge_agent_options(Opts, NewOptions);
	false ->
	    merge_agent_options(Opts, [Opt|Options])
    end.


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
    
    _Info = agent_info(Sup),
    ?DBG("stop_agent -> Agent info: "
	 "~n   ~p", [_Info]),
    
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


start_sup(Env) ->
    case (catch snmp_app_sup:start_agent(normal, Env)) of
	{ok, S} ->
	    ?DBG("start_agent -> started, Sup: ~p",[S]),
	    S;
	
	Else ->
	    ?DBG("start_agent -> unknown result: ~n~p",[Else]),
	    %% Get info about the apps we depend on
	    ?FAIL({start_failed, Else, ?IS_MNESIA_RUNNING()})
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
    Opts = [{db_dir,     Dir}, 
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
    Mib1   = join(MibDir, Mib),
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


%% --- various mib load/unload functions ---

load_master(Mib) ->
    ?DBG("load_master -> entry with"
	"~n   Mib: ~p", [Mib]),
    snmpa:unload_mib(snmp_master_agent, Mib),	% Unload for safety
    ok = snmpa:load_mib(snmp_master_agent, join(get(mib_dir), Mib)).

load_master_std(Mib) ->
    ?DBG("load_master_std -> entry with"
	"~n   Mib: ~p", [Mib]),
    snmpa:unload_mib(snmp_master_agent, Mib),	% Unload for safety
    ok = snmpa:load_mibs(snmp_master_agent, join(get(std_mib_dir), Mib)).

unload_master(Mib) ->
    ?DBG("unload_master -> entry with"
	"~n   Mib: ~p", [Mib]),
    ok = snmpa:unload_mib(snmp_master_agent, Mib).

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
%% The first two arguments are simple to be able to find where in the 
%% (test) code this call is made. 

expect(Mod, Line, What) -> 
    Fun = fun() -> do_expect(What) end,
    expect2(Mod, Line, Fun).

expect(Mod, Line, What, ExpVBs) ->          
    Fun = fun() -> do_expect(What, ExpVBs) end,
    expect2(Mod, Line, Fun).
	 
expect(Mod, Line, Error, Index, ExpVBS) -> 
    Fun = fun() -> do_expect(Error, Index, ExpVBS) end,
    expect2(Mod, Line, Fun).

expect(Mod, Line, Type, Enterp, Generic, Specific, ExpVBs) -> 
    Fun = fun() -> do_expect(Type, Enterp, Generic, Specific, ExpVBs) end,
    expect2(Mod, Line, Fun).

expect2(Mod, Line, F) ->
    io:format("EXPECT for ~w:~w~n", [Mod, Line]),
    case F() of
	{error, Reason} ->
	    io:format("EXPECT failed at ~w:~w => ~n~p~n", [Mod, Line, Reason]),
	    throw({error, {expect, Mod, Line, Reason}});
	Else ->
	    io:format("EXPECT result for ~w:~w => ~n~p~n", [Mod, Line, Else]),
	    Else
    end.

	
%% ----------------------------------------------------------------------

get_timeout() ->
    get_timeout(os:type()).

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
    io:format("EXPECT any PDU~n", []),
    receive_pdu(To);

do_expect({any_trap, To}) ->
    io:format("EXPECT any TRAP within ~w~n", [To]),
    receive_trap(To);

do_expect({timeout, To}) ->
    io:format("EXPECT nothing within ~w~n", [To]),
    receive
	X ->
	    {error, {unexpected, X}}
    after 
	To ->
	    ok
    end;

do_expect({Err, To}) 
  when (is_atom(Err) andalso 
	((is_integer(To) andalso To > 0) orelse (To =:= infinity))) ->
    io:format("EXPECT error ~w within ~w~n", [Err, To]),
    do_expect({{error, Err}, To});

do_expect({error, Err}) when is_atom(Err) ->
    Check = fun(_, R) -> R end,
    io:format("EXPECT error ~w~n", [Err]),
    do_expect2(Check, any, Err, any, any, get_timeout());
do_expect({{error, Err}, To}) ->
    Check = fun(_, R) -> R end,
    io:format("EXPECT error ~w within ~w~n", [Err, To]),
    do_expect2(Check, any, Err, any, any, To);

%% exp_varbinds() -> [exp_varbind()]
%% exp_varbind()  -> any | {Oid, any} | {Oid, Value}
%% Oid            -> [integer()]
%% Value          -> term()
%% ExpVBs         -> exp_varbinds() | {VbsCondition, exp_varbinds()}
do_expect(ExpVBs) ->
    Check = fun(_, R) -> R end,
    io:format("EXPECT 'get-response'"
	      "~n   with"
	      "~n      Varbinds: ~p~n", [ExpVBs]),
    do_expect2(Check, 'get-response', noError, 0, ExpVBs, get_timeout()).


do_expect(v2trap, ExpVBs) ->
    Check = fun(_, R) -> R end,
    io:format("EXPECT 'snmpv2-trap'"
	      "~n   with"
	      "~n      Varbinds: ~p~n", [ExpVBs]),
    do_expect2(Check, 'snmpv2-trap', noError, 0, ExpVBs, get_timeout());


do_expect(report, ExpVBs) ->
    Check = fun(_, R) -> R end,
    io:format("EXPECT 'report'"
	      "~n   with"
	      "~n      Varbinds: ~p~n", [ExpVBs]),
    do_expect2(Check, 'report', noError, 0, ExpVBs, get_timeout());


do_expect(inform, ExpVBs) ->
    do_expect({inform, true}, ExpVBs);

do_expect({inform, false}, ExpVBs) ->
    Check = fun(_, R) -> R end,
    io:format("EXPECT 'inform-request' (false)"
	      "~n   with"
	      "~n      Varbinds: ~p~n", [ExpVBs]),
    do_expect2(Check, 'inform-request', noError, 0, ExpVBs, get_timeout());

do_expect({inform, true}, ExpVBs) ->
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
    io:format("EXPECT 'inform-request' (true)"
	      "~n   with"
	      "~n      Varbinds: ~p~n", [ExpVBs]),
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
    io:format("EXPECT 'inform-request' (error)"
	      "~n   with"
	      "~n      Error Status: ~p"
	      "~n      Error Index:  ~p"
	      "~n      Varbinds:     ~p~n", [EStat, EIdx, ExpVBs]),
    do_expect2(Check, 'inform-request', noError, 0, ExpVBs, get_timeout()).


do_expect(Err, Idx, ExpVBs) ->
    do_expect(Err, Idx, ExpVBs, get_timeout()).

do_expect(Err, Idx, ExpVBs, To) 
  when is_atom(Err) andalso 
       (is_integer(Idx) orelse is_list(Idx) orelse (Idx == any)) ->
    Check = fun(_, R) -> R end,
    io:format("EXPECT 'get-response'"
	      "~n   with"
	      "~n      Error:    ~p"
	      "~n      Index:    ~p"
	      "~n      Varbinds: ~p"
	      "~n   within ~w~n", [Err, Idx, ExpVBs, To]),
    do_expect2(Check, 'get-response', Err, Idx, ExpVBs, To).


do_expect(Type, Enterp, Generic, Specific, ExpVBs) ->
    do_expect(Type, Enterp, Generic, Specific, ExpVBs, 3500).

do_expect(trap, Enterp, Generic, Specific, ExpVBs, To) ->
    io:format("EXPECT trap"
	      "~n   with"
	      "~n      Enterp:   ~w"
	      "~n      Generic:  ~w"
	      "~n      Specific: ~w"
	      "~n      Varbinds: ~w"
	      "~n   within ~w~n", [Enterp, Generic, Specific, ExpVBs, To]),
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

    case receive_pdu(To) of

	#pdu{type         = Type, 
	     error_status = Err,
	     error_index  = Idx} when ExpVBs =:= any -> 
	    io:format("EXPECT received expected pdu (1)~n", []),
	    ok;

	#pdu{type         = Type, 
	     request_id   = ReqId, 
	     error_status = Err2,
	     error_index  = Idx} when ExpVBs =:= any -> 
	    io:format("EXPECT received expected pdu with "
		      "unexpected error status (2): "
		      "~n   Error Status: ~p~n", [Err2]),
	    {error, {unexpected_error_status, Err, Err2, ReqId}};

	#pdu{error_status = Err} when (Type   =:= any) andalso 
				      (Idx    =:= any) andalso 
				      (ExpVBs =:= any) -> 
	    io:format("EXPECT received expected pdu (3)~n", []),
	    ok;

	#pdu{request_id   = ReqId, 
	     error_status = Err2} when (Type   =:= any) andalso 
				       (Idx    =:= any) andalso 
				       (ExpVBs =:= any) -> 
	    io:format("EXPECT received expected pdu with "
		      "unexpected error status (4): "
		      "~n   Error Status: ~p~n", [Err2]),
	    {error, {unexpected_error_status, Err, Err2, ReqId}};

	#pdu{type         = Type, 
	     error_status = Err} when (Idx =:= any) andalso 
				      (ExpVBs =:= any) -> 
	    io:format("EXPECT received expected pdu (5)~n", []),
	    ok;

	#pdu{type         = Type, 
	     request_id   = ReqId, 
	     error_status = Err2} when (Idx =:= any) andalso 
				       (ExpVBs =:= any) -> 
	    io:format("EXPECT received expected pdu with "
		      "unexpected error status (6): "
		      "~n   Error Status: ~p~n", [Err2]),
	    {error, {unexpected_error_status, Err, Err2, ReqId}};

	#pdu{type         = Type, 
	     request_id   = ReqId, 
	     error_status = Err,
	     error_index  = EI} when is_list(Idx) andalso (ExpVBs =:= any) -> 
	    case lists:member(EI, Idx) of
		true ->
		    io:format("EXPECT received expected pdu with "
			      "expected error index (7)~n", []),
		    ok;
		false ->
		    io:format("EXPECT received expected pdu with "
			      "unexpected error index (8): "
			      "~n   Error Index: ~p~n", [EI]),
		    {error, {unexpected_error_index, EI, Idx, ReqId}}
	    end;

	#pdu{type         = Type, 
	     request_id   = ReqId, 
	     error_status = Err2,
	     error_index  = EI} when is_list(Idx) andalso (ExpVBs =:= any) -> 
	    case lists:member(EI, Idx) of
		true ->
		    io:format("EXPECT received expected pdu with "
			      "unexpected error status (9): "
			      "~n   Error Status: ~p~n", [Err2]),
		    {error, {unexpected_error_status, Err, Err2, ReqId}};
		false ->
		    io:format("EXPECT received expected pdu with "
			      "unexpected error (10): "
			      "~n   Error Status: ~p"
			      "~n   Error index:  ~p~n", [Err2, EI]),
		    {error, {unexpected_error, {Err, Idx}, {Err2, EI}, ReqId}}
	    end;

	#pdu{type         = Type2, 
	     request_id   = ReqId, 
	     error_status = Err2, 
	     error_index  = Idx2} when ExpVBs =:= any ->
	    io:format("EXPECT received unexpected pdu with (11) "
		      "~n   Type:         ~p"
		      "~n   ReqId:        ~p"
		      "~n   Errot status: ~p"
		      "~n   Error index:  ~p"
		      "~n", [Type2, ReqId, Err2, Idx2]),
	    {error, 
	     {unexpected_pdu, 
	      {Type, Err, Idx}, {Type2, Err2, Idx2}, ReqId}};

	#pdu{type         = Type, 
	     error_status = Err, 
	     error_index  = Idx,
	     varbinds     = VBs} = PDU ->
	    io:format("EXPECT received pdu (12): "
		      "~n   [exp] Type:         ~p"
		      "~n   [exp] Error Status: ~p"
		      "~n   [exp] Error Index:  ~p"
		      "~n   VBs:                ~p"
		      "~nwhen"
		      "~n   ExpVBs:             ~p"
		      "~n", [Type, Err, Idx, VBs, ExpVBs]),
	    Check(PDU, check_vbs(purify_oids(ExpVBs), VBs));

	#pdu{type         = Type, 
	     error_status = Err, 
	     varbinds     = VBs} = PDU when Idx =:= any ->
	    io:format("EXPECT received pdu (13): "
		      "~n   [exp] Type:         ~p"
		      "~n   [exp] Error Status: ~p"
		      "~n   VBs:                ~p"
		      "~nwhen"
		      "~n   ExpVBs:             ~p"
		      "~n", [Type, Err, VBs, ExpVBs]),
	    Check(PDU, check_vbs(purify_oids(ExpVBs), VBs));

	#pdu{type         = Type, 
	     request_id   = ReqId, 
	     error_status = Err, 
	     error_index  = EI,
	     varbinds     = VBs} = PDU when is_list(Idx) ->
	    io:format("EXPECT received pdu (14): "
		      "~n   [exp] Type:         ~p"
		      "~n   ReqId:              ~p"
		      "~n   [exp] Error Status: ~p"
		      "~n   [exp] Error Index:  ~p"
		      "~n   VBs:                ~p"
		      "~nwhen"
		      "~n   ExpVBs:             ~p"
		      "~n", [Type, ReqId, Err, EI, VBs, ExpVBs]),
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
	    io:format("EXPECT received unexpected pdu with (15) "
		      "~n   Type:         ~p"
		      "~n   ReqId:        ~p"
		      "~n   Errot status: ~p"
		      "~n   Error index:  ~p"
		      "~n   Varbinds:     ~p"
		      "~n", [Type2, ReqId, Err2, Idx2, VBs2]),
	    {error, 
	     {unexpected_pdu, 
	      {Type,  Err,  Idx, purify_oids(ExpVBs)}, 
	      {Type2, Err2, Idx2, VBs2}, 
	      ReqId}};
	
	Error ->
	    io:format("EXPECT received error (16):  "
		      "~n   Error: ~p"
		      "~n", [Error]),
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
	{error, _, {_ExpFmt, ExpArg}, {_ActFmt, ActArg}} ->
	    ?DBG("get_req -> error for ~p: "
		 "~n   " ++ _ExpFmt ++ 
		 "~n   " ++ _ActFmt, 
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

config(Vsns, MgrDir, AgentConfDir, MIp, AIp) ->
    config(Vsns, MgrDir, AgentConfDir, MIp, AIp, inet).

config(Vsns, MgrDir, AgentConfDir, MIp, AIp, IpFamily) ->
    ?LOG("config -> entry with"
	 "~n   Vsns:         ~p"
	 "~n   MgrDir:       ~p"
	 "~n   AgentConfDir: ~p"
	 "~n   MIp:          ~p"
	 "~n   AIp:          ~p"
	 "~n   IpFamily:     ~p",
	 [Vsns, MgrDir, AgentConfDir, MIp, AIp, IpFamily]),
    ?line {Domain, ManagerAddr} =
	case IpFamily of
	    inet6 ->
		Ipv6Domain = transportDomainUdpIpv6,
		AgentIpv6Addr = {AIp, 4000},
		ManagerIpv6Addr = {MIp, ?TRAP_UDP},
		?line ok =
		    snmp_config:write_agent_snmp_files(
		      AgentConfDir, Vsns,
		      Ipv6Domain, ManagerIpv6Addr, AgentIpv6Addr, "test"),
		{Ipv6Domain, ManagerIpv6Addr};
	    _ ->
		?line ok =
		    snmp_config:write_agent_snmp_files(
		      AgentConfDir, Vsns, MIp, ?TRAP_UDP, AIp, 4000, "test"),
		{snmpUDPDomain, {MIp, ?TRAP_UDP}}
	  end,

    ?line case update_usm(Vsns, AgentConfDir) of
	      true ->
		  ?line copy_file(join(AgentConfDir, "usm.conf"),
				  join(MgrDir, "usm.conf")),
		  ?line update_usm_mgr(Vsns, MgrDir);
	      false ->
		  ?line ok
	  end,
    ?line update_community(Vsns, AgentConfDir),
    ?line update_vacm(Vsns, AgentConfDir),
    ?line write_target_addr_conf(AgentConfDir, Domain, ManagerAddr, Vsns),
    ?line write_target_params_conf(AgentConfDir, Vsns),
    ?line write_notify_conf(AgentConfDir),
    ok.

delete_files(Config) ->
    AgentDir = ?config(agent_dir, Config),
    delete_files(AgentDir, [db, conf]).

delete_files(_AgentFiles, []) ->
    ok;
delete_files(AgentDir, [DirName|DirNames]) ->
    Dir = join(AgentDir, DirName),
    {ok, Files} = file:list_dir(Dir),
    lists:foreach(fun(FName) -> file:delete(join(Dir, FName)) end,
		  Files),
    delete_files(AgentDir, DirNames).

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
    ?line ok = file:rename(join(Dir,"usm.conf"),
			   join(Dir,"usm.old")),
    Conf = [{"agentEngine", "newUser", "newUser", zeroDotZero, 
	     usmHMACSHAAuthProtocol, "", "", 
	     usmDESPrivProtocol, "", "", "", ShaKey, DesKey}, 
	    {"mgrEngine", "newUser", "newUser", zeroDotZero, 
	     usmHMACSHAAuthProtocol, "", "", 
	     usmDESPrivProtocol, "", "", "", ShaKey, DesKey}], 
    ?line ok = snmp_config:write_agent_usm_config(Dir, "", Conf).

reset_usm_mgr(Dir) ->
    ?line ok = file:rename(join(Dir,"usm.old"),
			   join(Dir,"usm.conf")).


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
    ?line ok = snmp_config:write_agent_community_config(Dir, "", Conf).

write_target_addr_conf(Dir, Conf) ->
    ?line ok = snmp_config:write_agent_target_addr_config(Dir, "", Conf).

write_target_addr_conf(Dir, Ip_or_Domain, Port_or_Addr, Vsns) ->
    ?line ok =
	snmp_config:write_agent_snmp_target_addr_conf(
	  Dir, Ip_or_Domain, Port_or_Addr, Vsns).

rewrite_target_addr_conf(Dir, NewPort) -> 
    ?DBG("rewrite_target_addr_conf -> entry with"
	 "~n   NewPort: ~p", [NewPort]),
    TAFile = join(Dir, "target_addr.conf"),
    case file:read_file_info(TAFile) of
	{ok, _} -> 
	    ok;
	{error, _R} -> 
	    ?ERR("failure reading file info of "
		 "target address config file: ~p", [_R]),
	    ok  
    end,

    ?line [TrapAddr|Addrs] = 
	snmp_conf:read(TAFile, fun rewrite_target_addr_conf_check/1),

    ?DBG("rewrite_target_addr_conf -> TrapAddr: ~p",[TrapAddr]),

    NewAddrs = [rewrite_target_addr_conf2(NewPort,TrapAddr)|Addrs],
    
    ?DBG("rewrite_target_addr_conf -> NewAddrs: ~p",[NewAddrs]),

    ?line ok = file:rename(join(Dir,"target_addr.conf"),
			   join(Dir,"target_addr.old")),

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
    ?line ok = file:rename(join(Dir, "target_addr.old"),
			   join(Dir, "target_addr.conf")).

write_target_params_conf(Dir, Vsns) -> 
    F = fun(v1) -> {"target_v1", v1,  v1,  "all-rights", noAuthNoPriv};
	   (v2) -> {"target_v2", v2c, v2c, "all-rights", noAuthNoPriv};
	   (v3) -> {"target_v3", v3,  usm, "all-rights", noAuthNoPriv}
	end,
    Conf = [F(Vsn) || Vsn <- Vsns],
    ?line ok = snmp_config:write_agent_target_params_config(Dir, "", Conf).

rewrite_target_params_conf(Dir, SecName, SecLevel) 
  when is_list(SecName) andalso is_atom(SecLevel) -> 
    ?line ok = file:rename(join(Dir,"target_params.conf"),
			   join(Dir,"target_params.old")),
    Conf = [{"target_v3", v3, usm, SecName, SecLevel}],
    ?line ok = snmp_config:write_agent_target_params_config(Dir, "", Conf).

reset_target_params_conf(Dir) ->
    ?line ok = file:rename(join(Dir,"target_params.old"),
			   join(Dir,"target_params.conf")).

write_notify_conf(Dir) -> 
    Conf = [{"standard trap",   "std_trap",   trap}, 
	    {"standard inform", "std_inform", inform}],
    ?line ok = snmp_config:write_agent_notify_config(Dir, "", Conf).

write_view_conf(Dir) -> 
    Conf = [{2, [1,3,6], included, null},
	    {2, ?tDescr_instance, excluded, null}], 
    ?line ok = snmp_config:write_agent_view_config(Dir, "", Conf).


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


join(Dir, File) ->
    filename:join(Dir, File).

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
%%     {A,B,C} = os:timestamp(),
%%     A*1000000000+B*1000+(C div 1000).
%% 
%% 
%% timeout() ->
%%     timeout(os:type()).
%% 
%% timeout(_)       -> 3500.
    

%% Time in milli seconds
%% t() ->
%%     {A,B,C} = os:timestamp(),
%%     A*1000000000+B*1000+(C div 1000).
