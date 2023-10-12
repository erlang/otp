%% 
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2022. All Rights Reserved.
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
	 start_mt_agent/1,        start_mt_agent/2,         start_mt_agent/3, 
	 stop_agent/1,

	 %% start_sup/0,      stop_sup/2,
	 start_subagent/3, stop_subagent/1, 
	 start_sub_sup/1,  start_sub_sup/2, 

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
-export([tc_wait/5, tc_run/4]).

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

    ?IPRINT("init_all -> entry with"
            "~n   Config: ~p"
            "~n   Nodes:  ~p", [Config, nodes()]),

    %% --
    %% Start nodes
    %%

    ?IPRINT("init_all -> start sub-agent node"),
    Args = ["-s", "snmp_test_sys_monitor", "start", "-s", "global", "sync"],

    {ok, SaPeer, SaNode}  = ?CT_PEER(#{name => ?CT_PEER_NAME(snmp_sa), args => Args}),
    unlink(SaPeer), %% must unlink, otherwise peer will exit before test case

    ?IPRINT("init_all -> start manager node"),
    {ok, MgrPeer, MgrNode} = ?CT_PEER(#{name => ?CT_PEER_NAME(snmp_mgr), args => Args}),
    unlink(MgrPeer), %% must unlink, otherwise peer will exit before test case

    global:sync(),

    %% -- 
    %% Create necessary files ( and dirs ) 
    %% 

    ?IPRINT("init_all -> lookup group top dir"),
    GroupTopDir = ?config(snmp_group_top_dir, Config),
    ?DBG("init_all -> GroupTopDir ~p", [GroupTopDir]),

    ?IPRINT("init_all -> create agent dir"),
    AgentDir = join(GroupTopDir, "agent/"), 
    ok = file:make_dir(AgentDir),
    ?DBG("init_all -> AgentDir ~p", [AgentDir]),

    ?IPRINT("init_all -> create agent db dir"),
    AgentDbDir = join(AgentDir, "db/"), 
    ok   = file:make_dir(AgentDbDir),
    ?DBG("init_all -> AgentDbDir ~p", [AgentDbDir]),

    ?IPRINT("init_all -> create agent log dir"),
    AgentLogDir = join(AgentDir, "log/"), 
    ok    = file:make_dir(AgentLogDir),
    ?DBG("init_all -> AgentLogDir ~p", [AgentLogDir]),

    ?IPRINT("init_all -> create agent config dir"),
    AgentConfDir = join(AgentDir, "conf/"), 
    ok     = file:make_dir(AgentConfDir),
    ?DBG("init_all -> AgentConfDir ~p", [AgentConfDir]),

    ?IPRINT("init_all -> create manager dir"),
    MgrDir   = join(GroupTopDir, "mgr/"), 
    ok = file:make_dir(MgrDir),
    ?DBG("init_all -> MgrDir ~p", [MgrDir]),

    ?IPRINT("init_all -> create sub-agent dir"),
    SaDir    = join(GroupTopDir, "sa/"), 
    ok = file:make_dir(SaDir),
    ?DBG("init_all -> SaDir ~p", [SaDir]),

    ?IPRINT("init_all -> create sub-agent db dir"),
    SaDbDir  = join(SaDir, "db/"), 
    ok = file:make_dir(SaDbDir),
    ?DBG("init_all -> SaDbDir ~p", [SaDbDir]),

    %% MibDir = ?config(mib_dir, Config),
    %% ?DBG("init_all -> MibDir ~p", [DataDir]),


    %% -- 
    %% Start and initiate mnesia
    %% 

    ?IPRINT("init_all -> load mnesia application (local)"),
    ok = application:load(mnesia),

    ?IPRINT("init_all -> load application mnesia on node ~p", [SaNode]),
    ok = rpc:call(SaNode, application, load, [mnesia]),
    
    ?IPRINT("init_all -> application mnesia (local): set_env dir"),
    application_controller:set_env(mnesia, dir,
					 join(AgentDbDir, "Mnesia1")),

    ?IPRINT("init_all -> application mnesia: set_env dir on node ~p", [SaNode]),
    rpc:call(SaNode, application_controller, set_env,
		   [mnesia, dir,  join(SaDir, "Mnesia2")]),

    ?IPRINT("init_all -> create mnesia schema"),
    ok = mnesia:create_schema([SaNode, node()]),
    
    ?IPRINT("init_all -> start application mnesia (local)"),
    ok = application:start(mnesia),

    ?IPRINT("init_all -> start application mnesia on ~p", [SaNode]),
    ok = rpc:call(SaNode, application, start, [mnesia]),

    ?IPRINT("init_all -> get localhost"),
    Ip = ?LOCALHOST(),

    ?IPRINT("init_all -> done when"
            "~n   Nodes: ~p", [nodes()]),
    [{snmp_sa,        SaNode},
     {snmp_sa_peer,   SaPeer},
     {snmp_mgr,       MgrNode},
     {snmp_mgr_peer,  MgrPeer},
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

    ?IPRINT("finish_all -> entry with"
            "~n   Config: ~p"
            "~n   Nodes:  ~p", [Config, nodes()]),

    SaPeer  = ?config(snmp_sa_peer, Config),
    MgrPeer = ?config(snmp_mgr_peer, Config),

    peer:stop(SaPeer),

    peer:stop(MgrPeer),

    ?IPRINT("finish_all -> stop mnesia application"),
    application:stop(mnesia),

    ?IPRINT("finish_all -> unload mnesia application"),
    application:unload(mnesia),

    ?IPRINT("finish_all -> stop when"
            "~n   Nodes: ~p", [nodes()]),
    ok.


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
    {ok, MasterIP} = ?LIB:which_host_ip(MasterHost, IpFamily),
    {ok, MIP}      = ?LIB:which_host_ip(MgrHost,    IpFamily),
    {ok, SIP}      = ?LIB:which_host_ip(SaHost,     IpFamily),

    ?IPRINT("init_case -> "
            "~n   SaHost:     ~p"
            "~n   MgrHost:    ~p"
            "~n   MasterHost: ~p"
            "~n   MasterIP:   ~p"
            "~n   MIP:        ~p"
            "~n   SIP:        ~p",
            [SaHost, MgrHost, MasterHost, MasterIP, MIP, SIP]),

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

    put(receive_response_timeout, receive_response_timeout(Config)),
    
    MibDir = ?config(mib_dir, Config),
    put(mib_dir, MibDir),
    StdM = join(code:priv_dir(snmp), "mibs") ++ "/",
    put(std_mib_dir, StdM),

    MgrDir = ?config(mgr_dir, Config),
    put(mgr_dir, MgrDir),

    put(vsn, ?config(vsn, Config)),

    ?IPRINT("init_case -> done with"
            "~n   MasterNode: ~p"
            "~n   SaNode:     ~p"
            "~n   MgrNode:    ~p"
            "~n   MibDir:     ~p", [MasterNode, SaNode, MgrNode, MibDir]),

    {SaNode, MgrNode, MibDir}.


receive_response_timeout(Config) ->
    case lists:keysearch(snmp_factor, 1, Config) of
        {value, {snmp_factor, F}} when (F < 4) ->
            ?SECS(5);
        {value, {snmp_factor, F}} when (F < 6) ->
            ?SECS(10);
        {value, {snmp_factor, F}} when (F < 8) ->
            ?SECS(15);
        {value, {snmp_factor, _}} ->
            ?SECS(20);
        _ ->
            ?SECS(10)
    end.

%%%--------------------------------------------------
%%% Used to test the standard mib with our
%%% configuration.
%%%--------------------------------------------------

try_test(TcRunMod, TcRunFunc) ->
    try_test(TcRunMod, TcRunFunc, []).

try_test(TcRunMod, TcRunFunc, TcRunArgs) ->
    try_test(TcRunMod, TcRunFunc, TcRunArgs, []).

try_test(TcRunMod, TcRunFunc, TcRunArgs, TcRunOpts) ->
    Node      = get(mgr_node),
    Mod       = ?MODULE,
    Func      = tc_run,
    Args      = [TcRunMod, TcRunFunc, TcRunArgs, TcRunOpts],
    tc_try(Node, Mod, Func, Args).

%% We spawn a test case runner process on the manager node.
%% The assumption is that the manager shall do something, but
%% not all test cases have the manager perform actions.
%% In some cases we make a rpc call back to the agent node directly
%% and call something in the agent... (for example the info_test
%% test case).
%% We should use link (instead of monitor) in order for the test case
%% timeout cleanup (kills) should have effect on the test case runner
%% process as well.

tc_try(N, M, F, A) ->
    ?IPRINT("tc_try -> entry with"
            "~n      N:     ~p"
            "~n      M:     ~p"
            "~n      F:     ~p"
            "~n      A:     ~p"
            "~n   when"
            "~n      get(): ~p"
            "~n", [N,
                   M, F, A,
                   get()]),
    case net_adm:ping(N) of
        pong ->
            ?IPRINT("tc_try -> ~p still running - start runner~n", [N]),
            OldFlag = trap_exit(true), % Make sure we catch it
            Runner  = spawn_link(N, ?MODULE, tc_wait, [self(), get(), M, F, A]),
            await_tc_runner_started(Runner, OldFlag),
            await_tc_runner_done(Runner, OldFlag);
        pang ->
            ?WPRINT("tc_try -> ~p *not* running~n", [N]),
            skip({node_not_running, N})
    end.

await_tc_runner_started(Runner, OldFlag) ->
    ?IPRINT("await tc-runner (~p) start ack~n", [Runner]),
    receive
        {'EXIT', Runner, Reason} ->
            ?EPRINT("TC runner start failed: "
                    "~n   ~p~n", [Reason]),
            exit({tc_runner_start_failed, Reason});
        {tc_runner_started, Runner} ->
            ?IPRINT("TC runner start acknowledged~n"),
            ok
    after 10000 -> %% We should *really* not have to wait this long, but...
            trap_exit(OldFlag),
            unlink_and_flush_exit(Runner),
            RunnerInfo = ?PINFO(Runner),
            ?EPRINT("TC runner start timeout: "
                    "~n   ~p", [RunnerInfo]),
            %% If we don't get a start ack within 10 seconds, we are f*ed
            exit(Runner, kill),
            exit({tc_runner_start, timeout, RunnerInfo})
    end.

await_tc_runner_done(Runner, OldFlag) ->
    receive
        {'EXIT', Runner, {udp_error, _} = Reason} ->
	    ?EPRINT("TC runner failed with an udp error: "
		    "~n   Reason: ~p"
		    "~n", [Reason]),
	    skip([{reason, Reason}]);
	    
	{'EXIT', Runner, Reason} ->
            %% This is not a normal (tc) failure (that is the clause below).
            %% Instead the tc runner process crashed, for some reason. So
            %% check if have got any system events, and if so, skip.
            SysEvs = snmp_test_global_sys_monitor:events(),
            if
                (SysEvs =:= []) ->
                    ?EPRINT("TC runner failed: "
                            "~n   ~p"
			    "~n", [Reason]),
                    exit({tc_runner_failed, Reason});
                true ->
                    ?WPRINT("TC runner failed when we got system events: "
                            "~n   Reason:     ~p"
                            "~n   Sys Events: ~p"
                            "~n", [Reason, SysEvs]),
                    skip([{reason, Reason}, {system_events, SysEvs}])
            end;
	{tc_runner_done, Runner, {'EXIT', {skip, Reason}}, Loc} ->
	    ?WPRINT("call -> done with skip: "
                    "~n   Reason: ~p"
                    "~n   Loc:    ~p"
                    "~n", [Reason, Loc]),
            trap_exit(OldFlag),
            unlink_and_flush_exit(Runner),
	    put(test_server_loc, Loc),
	    skip(Reason);
	{tc_runner_done, Runner, {'EXIT', Rn}, Loc} ->
	    ?EPRINT("call -> done with exit: "
                    "~n   Rn:  ~p"
                    "~n   Loc: ~p"
                    "~n", [Rn, Loc]),
            trap_exit(OldFlag),
            unlink_and_flush_exit(Runner),
	    put(test_server_loc, Loc),
	    exit(Rn);
	{tc_runner_done, Runner, Ret, _Loc} -> 
	    ?IPRINT("call -> done:"
                    "~n   Ret: ~p"
                    "~n   Loc: ~p", [Ret, _Loc]),
            trap_exit(OldFlag),
            unlink_and_flush_exit(Runner),
	    case Ret of
		{error, Reason} ->
                    %% Any failures while we have system events are skipped
                    SysEvs = snmp_test_global_sys_monitor:events(),
                    if
                        (SysEvs =:= []) ->
                            ?EPRINT("TC failure: "
                                    "~n   ~p"
                                    "~n", [Reason]),
                            exit(Reason);
                        true ->
                            ?WPRINT("TC failure when we got system events: "
                                    "~n   Reason:     ~p"
                                    "~n   Sys Events: ~p"
                                    "~n", [Reason, SysEvs]),
                            skip([{reason, Reason}, {system_events, SysEvs}])
                    end;
		{skip, Reason} ->
		    skip(Reason);
		OK ->
		    OK
	    end
    end.

trap_exit(Flag) when is_boolean(Flag) ->    
    erlang:process_flag(trap_exit, Flag).

unlink_and_flush_exit(Pid) ->
    unlink(Pid),
    receive
        {'EXIT', Pid, _} ->
            ok
    after 0 ->
            ok
    end.

tc_wait(From, Env, M, F, A) ->
    ?IPRINT("tc_wait -> entry with"
            "~n   From: ~p"
            "~n   Env:  ~p"
            "~n   M:    ~p"
            "~n   F:    ~p"
            "~n   A:    ~p", [From, Env, M, F, A]),
    From ! {tc_runner_started, self()},
    lists:foreach(fun({K,V}) -> put(K,V) end, Env),
    ?IPRINT("tc_wait -> env set - now run tc~n"),
    Res = (catch apply(M, F, A)),
    ?IPRINT("tc_wait -> tc run done: "
            "~n   ~p"
            "~n", [Res]),
    From ! {tc_runner_done, self(), Res, get(test_server_loc)},
    %% The point of this is that in some cases we have seen that the 
    %% exit signal having been "passed on" to the CT, which consider any
    %% exit a fail (even if its {'EXIT', ok}).
    %% So, just to be on the safe side, convert an 'ok' to a 'normal'.
    case Res of
        ok ->
            exit(normal);
        {ok, _} ->
            exit(normal);
        _ ->
            exit(Res)
    end.

tc_run(Mod, Func, Args, Opts) ->
    ?IPRINT("tc_run -> entry with"
            "~n   Mod:  ~p"
            "~n   Func: ~p"
            "~n   Args: ~p"
            "~n   Opts: ~p"
            "~n", [Mod, Func, Args, Opts]),
    (catch snmp_test_mgr:stop()), % If we had a running mgr from a failed case
    M           = get(mib_dir),
    Dir         = get(mgr_dir),
    User        = snmp_misc:get_option(user, Opts, "all-rights"),
    SecLevel    = snmp_misc:get_option(sec_level, Opts, noAuthNoPriv),
    EngineID    = snmp_misc:get_option(engine_id, Opts, "agentEngine"),
    CtxEngineID = snmp_misc:get_option(context_engine_id, Opts, EngineID),
    Community   = snmp_misc:get_option(community, Opts, "all-rights"),
    ?DBG("tc_run -> start crypto app",[]),
    _CryptoRes  = ?CRYPTO_START(),
    ?DBG("tc_run -> Crypto: ~p", [_CryptoRes]),
    StdM        = join(code:priv_dir(snmp), "mibs") ++ "/",
    Vsn         = get(vsn), 
    ?IPRINT("tc_run -> config:"
            "~n   M:           ~p"
            "~n   Vsn:         ~p"
            "~n   Dir:         ~p"
            "~n   User:        ~p"
            "~n   SecLevel:    ~p"
            "~n   EngineID:    ~p"
            "~n   CtxEngineID: ~p"
            "~n   Community:   ~p"
            "~n   StdM:        ~p"
            "~n", [M,Vsn,Dir,User,SecLevel,EngineID,CtxEngineID,Community,StdM]),
    case snmp_test_mgr:start_link([%% {agent, snmp_test_lib:hostname()},
                                   {packet_server_debug, true},
                                   {debug,               false},
                                   {agent,               get(master_host)}, 
                                   {ipfamily,            get(ipfamily)},
                                   {agent_udp,           4000},
                                   %% <SEP-TRANSPORTS>
                                   %% First port is used to request replies
                                   %% Second port is used for traps sent
                                   %% by the agent.
                                   %% {agent_udp,           {4000, 4001}},
                                   %% </SEP-TRANSPORTS>
                                   {trap_udp,            5000},
                                   {recbuf,              65535},
                                   quiet,
                                   Vsn, 
                                   {community,           Community},
                                   {user,                User},
                                   {sec_level,           SecLevel},
                                   {engine_id,           EngineID},
                                   {context_engine_id,   CtxEngineID},
                                   {dir,                 Dir},
                                   {mibs,                mibs(StdM, M)}]) of
	{ok, _Pid} ->
	    try apply(Mod, Func, Args) of
                Res ->
		    (catch snmp_test_mgr:stop()),
		    Res
            catch
                C:{skip, Reason} ->
                    ?WPRINT("apply (~w-) skip detected: "
                            "~n   ~p", [C, Reason]),
		    (catch snmp_test_mgr:stop()),
                    ?SKIP(Reason);

                throw:{error, Reason} ->
                    tc_run_skip_check(Mod, Func, Args, Reason, throw);

		exit:Reason ->
                    tc_run_skip_check(Mod, Func, Args, Reason, exit)
	    end;

	{error, Reason} ->
	    ?EPRINT("Failed starting (test) manager: "
                    "~n   ~p", [Reason]),
	    (catch snmp_test_mgr:stop()),
	    ?FAIL({mgr_start_error, Reason});

	Err ->
	    ?EPRINT("Failed starting (test) manager: "
                    "~n   ~p", [Err]),
	    (catch snmp_test_mgr:stop()),
	    ?FAIL({mgr_start_failure, Err})
    end.

%% We have some crap machines that generate this every now and then
%% (thay miss the window with 1 or 2 ms). If also detected by the
%% test manager, we get this and can skip.
tc_run_skip_check(_Mod, _Func, _Args,
                  {securityError, usmStatsNotInTimeWindows} = Reason,
                  _Cat) ->
    ?SKIP([{reason, Reason}]);
%% We have hosts (mostly *very* slooow VMs) that
%% can timeout anything. Since we are basically
%% testing communication, we therefore must check
%% for system events at every failure. Grrr!
tc_run_skip_check(Mod, Func, Args, Reason, Cat) ->
    SysEvs = snmp_test_global_sys_monitor:events(),
    (catch snmp_test_mgr:stop()),
    if
        (SysEvs =:= []) ->
            ?EPRINT("TC runner (~w-) failed: "
                    "~n   ~p~n", [Cat, Reason]),
            ?FAIL({apply_failed, {Mod, Func, Args}, Reason});
        true ->
            ?WPRINT("apply (~w) caught "
                    "when we got system events: "
                    "~n   Reason:     ~p"
                    "~n   Sys Events: ~p"
                    "~n", [Cat, Reason, SysEvs]),
            ?SKIP([{category, Cat},
                   {reason, Reason}, {system_events, SysEvs}])
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
 
start_mt_agent(Config) ->
    start_mt_agent(Config, true, []).
 
start_mt_agent(Config, MT) ->
    start_mt_agent(Config, MT, []).

start_mt_agent(Config, MT, Opts)
  when is_list(Config) andalso 
       ((MT =:= true) orelse (MT =:= extended)) andalso 
       is_list(Opts) ->
    start_agent(Config, [v2], [{multi_threaded, MT} | Opts]).

start_agent(Config, Vsns) ->
    start_agent(Config, Vsns, []).
start_agent(Config, Vsns, Opts) -> 

    ?IPRINT("start_agent -> entry (~p) with"
            "~n   Config: ~p"
            "~n   Vsns:   ~p"
            "~n   Opts:   ~p", [node(), Config, Vsns, Opts]),
    
    AgentLogDir  = ?config(agent_log_dir,      Config),
    AgentConfDir = ?config(agent_conf_dir,     Config),
    AgentDbDir   = ?config(agent_db_dir,       Config),
    SaNode       = ?config(snmp_sa,            Config),
    InetBackend  = ?config(socket_create_opts, Config),

    Env = app_agent_env_init(
	    [{versions,         Vsns}, 
	     {agent_type,       master},
	     {agent_verbosity,  trace},
             {get_mechanism,    snmp_agent_test_get},
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
	     {net_if,           [{verbosity, trace},
                                 {options,   InetBackend}]}],
	    Opts),
    

    process_flag(trap_exit,true),

    ?IPRINT("start_agent -> try start snmp app supervisor", []),
    AppSup = start_app_sup(),
    unlink(AppSup),
    ?DBG("start_agent -> snmp app supervisor: ~p", [AppSup]),

    ?IPRINT("start_agent -> try start master agent",[]),
    Sup = start_sup(Env),
    unlink(Sup),
    ?DBG("start_agent -> snmp supervisor: ~p", [Sup]),

    ?IPRINT("start_agent -> try (rpc) start sub agent on ~p", [SaNode]),
    SaDir = ?config(sa_dir, Config),
    {ok, Sub} = start_sub_sup(SaNode, SaDir),
    ?DBG("start_agent -> done", []),

    [{snmp_app_sup, AppSup},
           {snmp_sup,     {Sup, self()}}, 
           {snmp_sub,     Sub} | Config].


app_agent_env_init(Env0, Opts) ->
    ?DBG("app_agent_env_init -> unload snmp",[]),
    application:unload(snmp),

    ?DBG("app_agent_env_init -> load snmp",[]),
    application:load(snmp),

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
    ?IPRINT("stop_agent -> entry with"
            "~n   Config: ~p",[Config]),


    %% Stop the sub-agent (the agent supervisor)
    {SubSup, SubPar} = ?config(snmp_sub, Config),
    ?IPRINT("stop_agent -> attempt to stop sub agent (~p)"
            "~n   Sub Sup info: "
            "~n      ~p"
            "~n   Sub Par info: "
            "~n      ~p",
            [SubSup, ?PINFO(SubSup), ?PINFO(SubPar)]),
    stop_sup(SubSup, SubPar),
    Config2 = lists:keydelete(snmp_sub, 1, Config),


    %% Stop the master-agent (the top agent supervisor)
    {MasterSup, MasterPar} = ?config(snmp_sup, Config),
    ?IPRINT("stop_agent -> attempt to stop master agent (~p)"
            "~n   Master Sup: "
            "~n      ~p"
            "~n   Master Par: "
            "~n      ~p"
            "~n   Agent Info: "
            "~n      ~p",
            [MasterSup,
             ?PINFO(MasterSup), ?PINFO(MasterPar),
             agent_info(MasterSup)]),
    stop_sup(MasterSup, MasterPar),
    Config3 = lists:keydelete(snmp_sup, 1, Config2),


    %% Stop the top supervisor (of the snmp app)
    AppSup = ?config(snmp_app_sup, Config),
    ?IPRINT("stop_agent -> attempt to app sup ~p"
            "~n   App Sup: ~p",
            [AppSup, ?PINFO(AppSup)]),
    Config4 = lists:keydelete(snmp_app_sup, 1, Config3),


    ?IPRINT("stop_agent -> done", []),
    Config4.

start_app_sup() ->
    case snmp_app_sup:start_link() of
        {ok, AppSup} ->
            AppSup;
        {error, {already_started, Pid}} ->
            ?EPRINT("start_agent -> "
                    "SNMP app supervisor already started: "
                    "~n      (existing) Pid:          ~p"
                    "~n      (existing) Process Info: ~p",
                    [Pid, (catch process_info(Pid))]),
            ?FAIL({already_started, snmp_app_supervisor})
    end.

start_sup(Env) ->
    case (catch snmp_app_sup:start_agent(normal, Env)) of
	{ok, S} ->
	    ?DBG("start_agent -> started, Sup: ~p", [S]),
	    S;
	
	Else ->
	    ?EPRINT("start_agent -> unknown result: ~n~p", [Else]),
	    %% Get info about the apps we depend on
	    ?FAIL({start_failed, Else, ?IS_MNESIA_RUNNING()})
    end.

stop_sup(Pid, _) when (node(Pid) =:= node()) ->
    case (catch process_info(Pid)) of
	PI when is_list(PI) ->
	    ?IPRINT("stop_sup -> attempt to stop ~p", [Pid]),
	    Ref = erlang:monitor(process, Pid),
	    exit(Pid, kill),
	    await_stopped(Pid, Ref);
	{'EXIT', _Reason} ->
	    ?IPRINT("stop_sup -> ~p not running", [Pid]),
	    ok
    end;
stop_sup(Pid, _) ->
    ?IPRINT("stop_sup -> attempt to stop ~p", [Pid]),
    Ref = erlang:monitor(process, Pid),
    ?IPRINT("stop_sup -> Ref: ~p", [Ref]),
    exit(Pid, kill), 
    await_stopped(Pid, Ref).

await_stopped(Pid, Ref) ->
    receive
        {'DOWN', Ref, process, Pid, _Reason} ->
            ?DBG("received down message for ~p", [Pid]),
            ok
    after 10000 ->
	    ?EPRINT("await_stopped -> timeout for ~p",[Pid]),
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
    io_format_expect("for ~w:~w", [Mod, Line]),
    case F() of
	{error, {securityError, usmStatsNotInTimeWindows}} ->
	    io_format_expect("(USM) Stats not-in-windows => ", []),
	    skip({securityError, usmStatsNotInTimeWindows});
	{error, Reason} ->
	    io_format_expect("failed at ~w:~w => "
                             "~n      ~p", [Mod, Line, Reason]),
	    throw({error, {expect, Mod, Line, Reason}});
	Else ->
	    io_format_expect("result for ~w:~w => "
                             "~n      ~p", [Mod, Line, Else]),
	    Else
    end.

	
%% ----------------------------------------------------------------------

-define(BASE_REQ_TIMEOUT, 3500).

get_timeout() ->
    %% Try to figure out how "fast" a machine is.
    %% We assume that the number of schedulers
    %% (which depends on the number of core:s)
    %% effect the performance of the host...
    %% This is obviously not enough. The network
    %% also matterns, clock freq or the CPU, ...
    %% But its better than what we had before...
    case erlang:system_info(schedulers) of
        N when is_integer(N) ->
            ?BASE_REQ_TIMEOUT + timer:seconds(10 div N);
        _ ->
            ?BASE_REQ_TIMEOUT
    end.

receive_pdu(To) ->
    receive
	{snmp_pdu, PDU} when is_record(PDU, pdu) ->
	    PDU;
        {error, Reason} = ERROR ->
	    ?EPRINT("[await response-pdu] received unexpected error: "
                    "~n      ~p", [Reason]),
            ERROR
    after To ->
	    ?EPRINT("[await response-pdu] unexpected timeout"),
	    {error, timeout}
    end.

receive_trap(To) ->
    receive
	{snmp_pdu, PDU} when is_record(PDU, trappdu) ->
	    PDU
    after To ->
	    {error, timeout}
    end.


io_format_expect(F) ->
    io_format_expect(F, []).

io_format_expect(F, A) ->
    ?IPRINT("EXPECT " ++ F, A).
    

do_expect(Expect) when is_atom(Expect) ->
    do_expect({Expect, get_timeout()});

do_expect({any_pdu, To}) 
  when is_integer(To) orelse (To =:= infinity) ->
    io_format_expect("any PDU"),
    receive_pdu(To);

do_expect({any_trap, To}) ->
    io_format_expect("any TRAP within ~w", [To]),
    receive_trap(To);

do_expect({timeout, To}) ->
    io_format_expect("nothing within ~w", [To]),
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
    io_format_expect("error ~w within ~w", [Err, To]),
    do_expect({{error, Err}, To});

do_expect({error, Err}) when is_atom(Err) ->
    Check = fun(_, R) -> R end,
    io_format_expect("error ~w", [Err]),
    do_expect2(Check, any, Err, any, any, get_timeout());
do_expect({{error, Err}, To}) ->
    Check = fun(_, R) -> R end,
    io_format_expect("error ~w within ~w", [Err, To]),
    do_expect2(Check, any, Err, any, any, To);

%% exp_varbinds() -> [exp_varbind()]
%% exp_varbind()  -> any | {Oid, any} | {Oid, Value}
%% Oid            -> [integer()]
%% Value          -> term()
%% ExpVBs         -> exp_varbinds() | {VbsCondition, exp_varbinds()}
do_expect(ExpVBs) ->
    Check = fun(_, R) -> R end,
    io_format_expect("'get-response'"
                     "~n   with"
                     "~n      Varbinds: ~p", [ExpVBs]),
    do_expect2(Check, 'get-response', noError, 0, ExpVBs, get_timeout()).


do_expect(v2trap, ExpVBs) ->
    Check = fun(_, R) -> R end,
    io_format_expect("'snmpv2-trap' with"
                     "~n      Varbinds: ~p", [ExpVBs]),
    do_expect2(Check, 'snmpv2-trap', noError, 0, ExpVBs, get_timeout());


do_expect(report, ExpVBs) ->
    Check = fun(_, R) -> R end,
    io_format_expect("'report' with"
                     "~n      Varbinds: ~p", [ExpVBs]),
    do_expect2(Check, 'report', noError, 0, ExpVBs, get_timeout());


do_expect(inform, ExpVBs) ->
    do_expect({inform, true}, ExpVBs);

do_expect({inform, false}, ExpVBs) ->
    Check = fun(_, R) -> R end,
    io_format_expect("'inform-request' (false) with"
                     "~n      Varbinds: ~p", [ExpVBs]),
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
    io_format_expect("'inform-request' (true) with"
                     "~n      Varbinds: ~p", [ExpVBs]),
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
    io_format_expect("'inform-request' (error) with"
                     "~n      Error Status: ~p"
                     "~n      Error Index:  ~p"
                     "~n      Varbinds:     ~p", [EStat, EIdx, ExpVBs]),
    do_expect2(Check, 'inform-request', noError, 0, ExpVBs, get_timeout()).


do_expect(Err, Idx, ExpVBs) ->
    do_expect(Err, Idx, ExpVBs, get_timeout()).

do_expect(Err, Idx, ExpVBs, To) 
  when is_atom(Err) andalso 
       (is_integer(Idx) orelse is_list(Idx) orelse (Idx == any)) ->
    Check = fun(_, R) -> R end,
    io_format_expect("'get-response' within ~w ms with"
                     "~n      Error:    ~p"
                     "~n      Index:    ~p"
                     "~n      Varbinds: ~p", [To, Err, Idx, ExpVBs]),
    do_expect2(Check, 'get-response', Err, Idx, ExpVBs, To).


do_expect(Type, Enterp, Generic, Specific, ExpVBs) ->
    do_expect(Type, Enterp, Generic, Specific, ExpVBs, get_timeout()).

do_expect(trap, Enterp, Generic, Specific, ExpVBs, To) ->
    io_format_expect("trap within ~w ms with"
                     "~n      Enterp:   ~w"
                     "~n      Generic:  ~w"
                     "~n      Specific: ~w"
                     "~n      Varbinds: ~w",
                     [To, Enterp, Generic, Specific, ExpVBs]),
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

	{error, timeout} = Error ->
            SysEvs = snmp_test_global_sys_monitor:events(),
	    io_format_expect("[expecting trap] got timeout when system events:"
                             "~n   ~p", [SysEvs]),
            if
                (SysEvs =:= []) ->
                    Error;
                true ->
                    skip({system_events, SysEvs})
            end;


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
	    io_format_expect("received expected pdu (1)"),
	    ok;

	#pdu{type         = Type, 
	     request_id   = ReqId, 
	     error_status = Err2,
	     error_index  = Idx} when ExpVBs =:= any -> 
	    io_format_expect("received expected pdu with "
                             "unexpected error status (2): "
                             "~n   Error Status: ~p", [Err2]),
	    {error, {unexpected_error_status, Err, Err2, ReqId}};

	#pdu{error_status = Err} when (Type   =:= any) andalso 
				      (Idx    =:= any) andalso 
				      (ExpVBs =:= any) -> 
	    io_format_expect("received expected pdu (3)"),
	    ok;

	#pdu{request_id   = ReqId, 
	     error_status = Err2} when (Type   =:= any) andalso 
				       (Idx    =:= any) andalso 
				       (ExpVBs =:= any) -> 
	    io_format_expect("received expected pdu with "
                             "unexpected error status (4): "
                             "~n   Error Status: ~p", [Err2]),
	    {error, {unexpected_error_status, Err, Err2, ReqId}};

	#pdu{type         = Type, 
	     error_status = Err} when (Idx =:= any) andalso 
				      (ExpVBs =:= any) -> 
	    io_format_expect("received expected pdu (5)", []),
	    ok;

	#pdu{type         = Type, 
	     request_id   = ReqId, 
	     error_status = Err2} when (Idx =:= any) andalso 
				       (ExpVBs =:= any) -> 
	    io_format_expect("received expected pdu with "
                             "unexpected error status (6): "
                             "~n   Error Status: ~p", [Err2]),
	    {error, {unexpected_error_status, Err, Err2, ReqId}};

	#pdu{type         = Type, 
	     request_id   = ReqId, 
	     error_status = Err,
	     error_index  = EI} when is_list(Idx) andalso (ExpVBs =:= any) -> 
	    case lists:member(EI, Idx) of
		true ->
		    io_format_expect("received expected pdu with "
                                     "expected error index (7)"),
		    ok;
		false ->
		    io_format_expect("received expected pdu with "
                                     "unexpected error index (8): "
                                     "~n   Error Index: ~p", [EI]),
		    {error, {unexpected_error_index, EI, Idx, ReqId}}
	    end;

	#pdu{type         = Type, 
	     request_id   = ReqId, 
	     error_status = Err2,
	     error_index  = EI} when is_list(Idx) andalso (ExpVBs =:= any) -> 
	    case lists:member(EI, Idx) of
		true ->
		    io_format_expect("received expected pdu with "
                                     "unexpected error status (9): "
                                     "~n   Error Status: ~p", [Err2]),
		    {error, {unexpected_error_status, Err, Err2, ReqId}};
		false ->
		    io_format_expect("received expected pdu with "
                                     "unexpected error (10): "
                                     "~n   Error Status: ~p"
                                     "~n   Error index:  ~p", [Err2, EI]),
		    {error, {unexpected_error, {Err, Idx}, {Err2, EI}, ReqId}}
	    end;

	#pdu{type         = Type2, 
	     request_id   = ReqId, 
	     error_status = Err2, 
	     error_index  = Idx2} when ExpVBs =:= any ->
	    io_format_expect("received unexpected pdu with (11) "
                             "~n   Type:         ~p"
                             "~n   ReqId:        ~p"
                             "~n   Error status: ~p"
                             "~n   Error index:  ~p",
                             [Type2, ReqId, Err2, Idx2]),
	    {error, 
	     {unexpected_pdu, 
	      {Type, Err, Idx}, {Type2, Err2, Idx2}, ReqId}};

	#pdu{type         = Type, 
	     error_status = Err, 
	     error_index  = Idx,
	     varbinds     = VBs} = PDU ->
	    io_format_expect("received pdu (12): "
                             "~n   [exp] Type:         ~p"
                             "~n   [exp] Error Status: ~p"
                             "~n   [exp] Error Index:  ~p"
                             "~n   VBs:                ~p"
                             "~nwhen"
                             "~n   ExpVBs:             ~p",
                             [Type, Err, Idx, VBs, ExpVBs]),
	    Check(PDU, check_vbs(purify_oids(ExpVBs), VBs));

	#pdu{type         = Type, 
	     error_status = Err, 
	     varbinds     = VBs} = PDU when Idx =:= any ->
	    io_format_expect("received pdu (13): "
                             "~n   [exp] Type:         ~p"
                             "~n   [exp] Error Status: ~p"
                             "~n   VBs:                ~p"
                             "~nwhen"
                             "~n   ExpVBs:             ~p",
                             [Type, Err, VBs, ExpVBs]),
	    Check(PDU, check_vbs(purify_oids(ExpVBs), VBs));

	#pdu{type         = Type, 
	     request_id   = ReqId, 
	     error_status = Err, 
	     error_index  = EI,
	     varbinds     = VBs} = PDU when is_list(Idx) ->
	    io_format_expect("received pdu (14): "
                             "~n   [exp] Type:         ~p"
                             "~n   ReqId:              ~p"
                             "~n   [exp] Error Status: ~p"
                             "~n   [exp] Error Index:  ~p"
                             "~n   VBs:                ~p"
                             "~nwhen"
                             "~n   ExpVBs:             ~p",
                             [Type, ReqId, Err, EI, VBs, ExpVBs]),
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
	    io_format_expect("received unexpected pdu with (15) "
                             "~n   Type:         ~p"
                             "~n   ReqId:        ~p"
                             "~n   Error status: ~p"
                             "~n   Error index:  ~p"
                             "~n   Varbinds:     ~p",
                             [Type2, ReqId, Err2, Idx2, VBs2]),
	    {error, 
	     {unexpected_pdu, 
	      {Type,  Err,  Idx, purify_oids(ExpVBs)}, 
	      {Type2, Err2, Idx2, VBs2}, 
	      ReqId}};
	

	{error, timeout} = Error ->
            SysEvs = snmp_test_global_sys_monitor:events(),
	    io_format_expect("got timeout (16) when system events:"
                             "~n   ~p", [SysEvs]),
            if
                (SysEvs =:= []) ->
                    Error;
                true ->
                    skip({system_events, SysEvs})
            end;


        Error ->
            io_format_expect("received error (17):  "
                             "~n   Error: ~p", [Error]),
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
	 "~n   Vars: ~p", [Vars]),
    snmp_test_mgr:gn(Vars),
    ?DBG("get_next_req -> await response",[]),
    Response = snmp_test_mgr:receive_response(),
    ?DBG("get_next_req -> response: ~p",[Response]),
    Response.



%%%-----------------------------------------------------------------
%%% Configuration
%%%-----------------------------------------------------------------

config(Vsns, MgrDir, AgentConfDir, MIp, AIp) ->
    config(Vsns, MgrDir, AgentConfDir, MIp, AIp, inet).

config(Vsns, MgrDir, AgentConfDir, MIp, AIp, IpFamily) ->
    ?IPRINT("config -> entry with"
            "~n   Vsns:         ~p"
            "~n   MgrDir:       ~p"
            "~n   AgentConfDir: ~p"
            "~n   MIp:          ~p"
            "~n   AIp:          ~p"
            "~n   IpFamily:     ~p",
            [Vsns, MgrDir, AgentConfDir, MIp, AIp, IpFamily]),
    {Domain, ManagerAddr} =
	case IpFamily of
	    inet6 ->
		TransportDomain6 = transportDomainUdpIpv6,
		AgentAddr6       = {AIp, 4000},
		ManagerAddr6     = {MIp, ?TRAP_UDP},
		ok =
		    snmp_config:write_agent_snmp_files(
		      AgentConfDir, Vsns,
		      TransportDomain6, ManagerAddr6, AgentAddr6, "test"),
		{TransportDomain6, ManagerAddr6};
	    inet ->
		TransportDomain4 = transportDomainUdpIpv4,
                AIp2 = maybe_fix_addr(AIp),
		ManagerAddr4     = {MIp, ?TRAP_UDP},
                %% AgentPreTransport  =
                %%     [#{addr => {AIp2, 4000}, kind => req_responder},
                %%      #{addr => {AIp2, 4001}, kind => trap_sender}],
                AgentPreTransport  = [#{addr => {AIp2, 4000}}],
		ok =
		    snmp_config:write_agent_snmp_files(
		      AgentConfDir, Vsns,
		      TransportDomain4, ManagerAddr4, AgentPreTransport,
                      "test"),
		{TransportDomain4, ManagerAddr4};
	    _ ->
		ok =
		    snmp_config:write_agent_snmp_files(
		      AgentConfDir, Vsns, MIp, ?TRAP_UDP, AIp, 4000, "test"),
		{snmpUDPDomain, {MIp, ?TRAP_UDP}}
	  end,

    case update_usm(Vsns, AgentConfDir) of
	      true ->
		  copy_file(join(AgentConfDir, "usm.conf"),
				  join(MgrDir, "usm.conf")),
		  update_usm_mgr(Vsns, MgrDir);
	      false ->
		  ok
	  end,
    update_community(Vsns, AgentConfDir),
    update_vacm(Vsns, AgentConfDir),
    write_target_addr_conf(AgentConfDir, Domain, ManagerAddr, Vsns),
    write_target_params_conf(AgentConfDir, Vsns),
    write_notify_conf(AgentConfDir),
    ok.

maybe_fix_addr(Addr) when is_list(Addr) ->
    list_to_tuple(Addr);
maybe_fix_addr(Addr) when is_tuple(Addr) ->
    Addr.

    
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

		    {"agentEngine", "authSHA224", "authSHA224", zeroDotZero, 
		     usmHMAC128SHA224AuthProtocol, "", "", 
		     usmNoPrivProtocol, "", "", "", 
		     "passwd_sha224xxxxxxxxxxxxxxx", ""}, 

		    {"agentEngine", "authSHA256", "authSHA256", zeroDotZero, 
		     usmHMAC192SHA256AuthProtocol, "", "", 
		     usmNoPrivProtocol, "", "", "", 
		     "passwd_sha256xxxxxxxxxxxxxxxxxxx", ""}, 

		    {"agentEngine", "authSHA384", "authSHA384", zeroDotZero, 
		     usmHMAC256SHA384AuthProtocol, "", "", 
		     usmNoPrivProtocol, "", "", "", 
		     "passwd_sha384xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx", ""}, 

		    {"agentEngine", "authSHA512", "authSHA512", zeroDotZero, 
		     usmHMAC384SHA512AuthProtocol, "", "", 
		     usmNoPrivProtocol, "", "", "", 
		     "passwd_sha512xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx", ""}, 

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

		    {"mgrEngine", "authSHA224", "authSHA224", zeroDotZero, 
		     usmHMAC128SHA224AuthProtocol, "", "", 
		     usmNoPrivProtocol, "", "", "", 
		     "passwd_sha224xxxxxxxxxxxxxxx", ""}, 

		    {"mgrEngine", "authSHA256", "authSHA256", zeroDotZero, 
		     usmHMAC192SHA256AuthProtocol, "", "", 
		     usmNoPrivProtocol, "", "", "", 
		     "passwd_sha256xxxxxxxxxxxxxxxxxxx", ""}, 

		    {"mgrEngine", "authSHA384", "authSHA384", zeroDotZero, 
		     usmHMAC256SHA384AuthProtocol, "", "", 
		     usmNoPrivProtocol, "", "", "", 
		     "passwd_sha384xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx", ""}, 

		    {"mgrEngine", "authSHA512", "authSHA512", zeroDotZero, 
		     usmHMAC384SHA512AuthProtocol, "", "", 
		     usmNoPrivProtocol, "", "", "", 
		     "passwd_sha512xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx", ""}, 

		    {"mgrEngine", "privDES", "privDES", zeroDotZero, 
		     usmHMACSHAAuthProtocol, "", "", 
		     usmDESPrivProtocol, "", "", "", 
		     "passwd_shaxxxxxxxxxx", "passwd_desxxxxxx"}],
	    ok = snmp_config:update_agent_usm_config(Dir, Conf),
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

	    ok = snmp_config:update_agent_usm_config(Dir, Conf),
	    true;
	false ->
	    false
    end.

rewrite_usm_mgr(Dir, ShaKey, DesKey) -> 
    ok = file:rename(join(Dir,"usm.conf"),
			   join(Dir,"usm.old")),
    Conf = [{"agentEngine", "newUser", "newUser", zeroDotZero, 
	     usmHMACSHAAuthProtocol, "", "", 
	     usmDESPrivProtocol, "", "", "", ShaKey, DesKey}, 
	    {"mgrEngine", "newUser", "newUser", zeroDotZero, 
	     usmHMACSHAAuthProtocol, "", "", 
	     usmDESPrivProtocol, "", "", "", ShaKey, DesKey}], 
    ok = snmp_config:write_agent_usm_config(Dir, "", Conf).

reset_usm_mgr(Dir) ->
    ok = file:rename(join(Dir,"usm.old"),
			   join(Dir,"usm.conf")).


update_community([v3], _Dir) -> 
    ok;
update_community(_, Dir) ->
    Conf = [{"no-rights", "no-rights", "no-rights", "", ""}],
    ok = snmp_config:update_agent_community_config(Dir, Conf).
    
    
-define(tDescr_instance, [1,3,6,1,2,1,16,1,0]).
update_vacm(_Vsn, Dir) ->
    Conf = [{vacmSecurityToGroup, usm, "authMD5",    "initial"}, 
	    {vacmSecurityToGroup, usm, "authSHA",    "initial"}, 
	    {vacmSecurityToGroup, usm, "authSHA224", "initial"}, 
	    {vacmSecurityToGroup, usm, "authSHA256", "initial"}, 
	    {vacmSecurityToGroup, usm, "authSHA384", "initial"}, 
	    {vacmSecurityToGroup, usm, "authSHA512", "initial"}, 
	    {vacmSecurityToGroup, usm, "privDES",    "initial"}, 
	    {vacmSecurityToGroup, usm, "newUser",    "initial"},
	    {vacmViewTreeFamily, "internet", ?tDescr_instance, 
	     excluded, null}],
    ok = snmp_config:update_agent_vacm_config(Dir, Conf).
    
    
write_community_conf(Dir, Conf) ->
    ok = snmp_config:write_agent_community_config(Dir, "", Conf).

write_target_addr_conf(Dir, Conf) ->
    ok = snmp_config:write_agent_target_addr_config(Dir, "", Conf).

write_target_addr_conf(Dir, Ip_or_Domain, Port_or_Addr, Vsns) ->
    ok =
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
	    ?WPRINT("failure reading file info of "
                    "target address config file: ~p", [_R]),
	    ok  
    end,

    [TrapAddr|Addrs] =
	snmp_conf:read(TAFile, fun rewrite_target_addr_conf_check/1),

    ?DBG("rewrite_target_addr_conf -> TrapAddr: ~p",[TrapAddr]),

    NewAddrs = [rewrite_target_addr_conf2(NewPort,TrapAddr)|Addrs],
    
    ?DBG("rewrite_target_addr_conf -> NewAddrs: ~p",[NewAddrs]),

    ok = file:rename(join(Dir,"target_addr.conf"),
			   join(Dir,"target_addr.old")),

    ok = snmp_config:write_agent_target_addr_config(Dir, "", NewAddrs).

rewrite_target_addr_conf_check(O) -> 
    {ok,O}.

rewrite_target_addr_conf2(NewPort,
			  {Name, Ip, _Port, Timeout, Retry,
			   "std_trap", EngineId}) -> 
    ?IPRINT("rewrite_target_addr_conf2 -> entry with std_trap",[]),
    {Name,Ip,NewPort,Timeout,Retry,"std_trap",EngineId};
rewrite_target_addr_conf2(_NewPort,O) -> 
    ?IPRINT("rewrite_target_addr_conf2 -> entry with "
	 "~n   O: ~p",[O]),
    O.

reset_target_addr_conf(Dir) ->
    ok = file:rename(join(Dir, "target_addr.old"),
			   join(Dir, "target_addr.conf")).

write_target_params_conf(Dir, Vsns) -> 
    F = fun(v1) -> {"target_v1", v1,  v1,  "all-rights", noAuthNoPriv};
	   (v2) -> {"target_v2", v2c, v2c, "all-rights", noAuthNoPriv};
	   (v3) -> {"target_v3", v3,  usm, "all-rights", noAuthNoPriv}
	end,
    Conf = [F(Vsn) || Vsn <- Vsns],
    ok = snmp_config:write_agent_target_params_config(Dir, "", Conf).

rewrite_target_params_conf(Dir, SecName, SecLevel) 
  when is_list(SecName) andalso is_atom(SecLevel) -> 
    ok = file:rename(join(Dir,"target_params.conf"),
			   join(Dir,"target_params.old")),
    Conf = [{"target_v3", v3, usm, SecName, SecLevel}],
    ok = snmp_config:write_agent_target_params_config(Dir, "", Conf).

reset_target_params_conf(Dir) ->
    ok = file:rename(join(Dir,"target_params.old"),
			   join(Dir,"target_params.conf")).

write_notify_conf(Dir) -> 
    Conf = [{"standard trap",   "std_trap",   trap}, 
	    {"standard inform", "std_inform", inform}],
    ok = snmp_config:write_agent_notify_config(Dir, "", Conf).

write_view_conf(Dir) -> 
    Conf = [{2, [1,3,6], included, null},
	    {2, ?tDescr_instance, excluded, null}], 
    ok = snmp_config:write_agent_view_config(Dir, "", Conf).


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
    ?IPRINT("Memory usage: "
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


skip(R) ->
    exit({skip, R}).

