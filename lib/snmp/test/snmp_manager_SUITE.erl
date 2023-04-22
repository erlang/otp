%% 
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2022. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% Purpose:
%%
%% Test:    ts:run(snmp, snmp_manager_SUITE, [batch]).
%% Test:    ts:run(snmp, snmp_manager_SUITE, event_tests, [batch]).
%% Test:    ts:run(snmp, snmp_manager_SUITE, inform_swarm, [batch]).
%% 
%%----------------------------------------------------------------------
-module(snmp_manager_SUITE).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------

-include_lib("common_test/include/ct.hrl").
-include("snmp_test_lib.hrl").
-include("snmp_test_data/Test2.hrl").

-include_lib("snmp/include/snmp_types.hrl").
-include_lib("snmp/include/STANDARD-MIB.hrl").
-include_lib("snmp/include/SNMP-USER-BASED-SM-MIB.hrl").
-include_lib("snmp/src/manager/snmpm_internal.hrl").


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([
         suite/0, all/0, groups/0,
	 init_per_suite/1,    end_per_suite/1, 
	 init_per_group/2,    end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2, 


	 simple_start_and_stop1/1,
	 simple_start_and_stop2/1,
	 simple_start_and_stop3/1,
	 simple_start_and_monitor_crash1/1,
	 simple_start_and_monitor_crash2/1,
	 notify_started01/1,
	 notify_started02/1,

	 register_user1/1,
	
	 register_agent_old/0, register_agent_old/1,
	 register_agent2/0, register_agent2/1,
	 register_agent3/0, register_agent3/1,

	 info/1,
         usm_priv_aes/1,
         usm_sha224_priv_aes/1,
         usm_sha256_priv_aes/1,
         usm_sha384_priv_aes/1,
         usm_sha512_priv_aes/1,
	 
	 simple_sync_get3/0, simple_sync_get3/1,
	 simple_async_get3/0, simple_async_get3/1,
	
         simple_sync_get_next3/0, simple_sync_get_next3/1,
         simple_async_get_next3_cbp_def/0, simple_async_get_next3_cbp_def/1,
         simple_async_get_next3_cbp_temp/0, simple_async_get_next3_cbp_temp/1,
         simple_async_get_next3_cbp_perm/0, simple_async_get_next3_cbp_perm/1,
	 
         simple_sync_set3/0, simple_sync_set3/1,
         simple_async_set3_cbp_def/0, simple_async_set3_cbp_def/1,
         simple_async_set3_cbp_temp/0, simple_async_set3_cbp_temp/1,
         simple_async_set3_cbp_perm/0, simple_async_set3_cbp_perm/1,
	 
  	 simple_sync_get_bulk3/0, simple_sync_get_bulk3/1,
         simple_async_get_bulk3_cbp_def/0, simple_async_get_bulk3_cbp_def/1,
         simple_async_get_bulk3_cbp_temp/0, simple_async_get_bulk3_cbp_temp/1,
         simple_async_get_bulk3_cbp_perm/0, simple_async_get_bulk3_cbp_perm/1,

	 simple_v3_exchange_md5/0, simple_v3_exchange_md5/1,
	 simple_v3_exchange_sha/0, simple_v3_exchange_sha/1,
	 simple_v3_exchange_sha224/0, simple_v3_exchange_sha224/1,
	 simple_v3_exchange_sha256/0, simple_v3_exchange_sha256/1,
	 simple_v3_exchange_sha384/0, simple_v3_exchange_sha384/1,
	 simple_v3_exchange_sha512/0, simple_v3_exchange_sha512/1,

	 discovery/1,
	 
	 trap1/1,
	 trap2/1,

	 inform1/1,
	 inform2/1,
	 inform3/1,
	 inform4/1,
	 inform_swarm_cbp_def/1,
         inform_swarm_cbp_temp/1,
         inform_swarm_cbp_perm/1,

	 report/1,

	 otp8015_1/0, otp8015_1/1,
	
	 otp8395_1/0, otp8395_1/1

	]).


%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------

%% -export([async_exec/2]).


%%----------------------------------------------------------------------
%% Macros and constants
%%----------------------------------------------------------------------

-define(AGENT_PORT,       4000).
-define(AGENT_MMS,        1024).
-define(AGENT_ENGINE_ID,  "agentEngine").

-define(MGR_PORT,       5000).
-define(MGR_MMS,        1024).
-define(MGR_ENGINE_ID,  "mgrEngine").

-define(NS_TIMEOUT,     10000).

-define(DEFAULT_MNESIA_DEBUG, none).


%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------

%%======================================================================
%% Common Test interface functions
%%======================================================================

suite() -> 
    [{ct_hooks, [ts_install_cth]}].

all() -> 
    %% This is a temporary measure to ensure that we can 
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

     {all,                  [], all_cases()},
     {start_and_stop_tests, [], start_and_stop_tests_cases()},
     {misc_tests,           [], misc_tests_cases()},
     {usm_priv_aes_tests,   [], usm_priv_aes_tests_cases()},
     {user_tests,           [], user_tests_cases()},
     {agent_tests,          [], agent_tests_cases()},
     {request_tests,        [], request_tests_cases()},
     {request_tests_mt,     [], request_tests_mt_cases()},
     {get_tests,            [], get_tests_cases()},
     {get_next_tests,       [], get_next_tests_cases()},
     {set_tests,            [], set_tests_cases()},
     {bulk_tests,           [], bulk_tests_cases()},
     {event_tests,          [], event_tests_cases()},
     {event_tests_mt,       [], event_tests_mt_cases()},
     {tickets,              [], tickets_cases()},
     {otp8015,              [], otp8015_cases()},
     {otp8395,              [], otp8395_cases()},
     {ipv6,                 [], ipv6_tests()},
     {ipv6_mt,              [], ipv6_tests()},

     {v3,                   [], v3_cases()}
    ].

inet_backend_default_cases() ->
    [{all, [], all_cases()}].

inet_backend_inet_cases() ->
    [{all, [], all_cases()}].

inet_backend_socket_cases() ->
    [{all, [], all_cases()}].

all_cases() -> 
    [
     {group, start_and_stop_tests},
     {group, misc_tests},
     {group, usm_priv_aes_tests},
     {group, user_tests},
     {group, agent_tests},
     {group, request_tests},
     {group, request_tests_mt},
     {group, event_tests},
     {group, event_tests_mt},
     discovery,
     {group, tickets},
     {group, ipv6},
     {group, ipv6_mt},
     {group, v3}
    ].

start_and_stop_tests_cases() ->
    [
     simple_start_and_stop1, 
     simple_start_and_stop2,
     simple_start_and_stop3, 
     simple_start_and_monitor_crash1,
     simple_start_and_monitor_crash2, 
     notify_started01,
     notify_started02
    ].

misc_tests_cases() ->
    [
     info,
     {group, usm_priv_aes_tests}
    ].

usm_priv_aes_tests_cases() ->
    [
     usm_priv_aes,
     usm_sha224_priv_aes,
     usm_sha256_priv_aes,
     usm_sha384_priv_aes,
     usm_sha512_priv_aes
    ].

user_tests_cases() ->
    [
     register_user1
    ].

agent_tests_cases() ->
    [
     register_agent_old,
     register_agent2, 
     register_agent3
    ].
    
request_tests_cases() ->
    [
     {group, get_tests}, 
     {group, get_next_tests}, 
     {group, set_tests}, 
     {group, bulk_tests}
    ].

request_tests_mt_cases() -> request_tests_cases().

get_tests_cases() ->
    [
     simple_sync_get3, 
     simple_async_get3
    ].

get_next_tests_cases() ->
    [
     simple_sync_get_next3,
     simple_async_get_next3_cbp_def,
     simple_async_get_next3_cbp_temp,
     simple_async_get_next3_cbp_perm
    ].

set_tests_cases() ->
    [
     simple_sync_set3, 
     simple_async_set3_cbp_def,
     simple_async_set3_cbp_temp,
     simple_async_set3_cbp_perm
    ].

bulk_tests_cases() ->
    [
     simple_sync_get_bulk3,
     simple_async_get_bulk3_cbp_def,
     simple_async_get_bulk3_cbp_temp,
     simple_async_get_bulk3_cbp_perm
    ].

ipv6_tests() ->
    [
     register_agent_old,
     simple_sync_get_next3,
     simple_sync_get3,
     simple_sync_set3,
     simple_async_get_bulk3_cbp_def,
     simple_async_get_bulk3_cbp_temp,
     simple_async_get_bulk3_cbp_perm,
     inform1,
     inform_swarm_cbp_def,
     inform_swarm_cbp_temp,
     inform_swarm_cbp_perm
    ].

event_tests_cases() ->
    [
     trap1, 
     trap2, 
     inform1, 
     inform2, 
     inform3, 
     inform4,
     inform_swarm_cbp_def,
     inform_swarm_cbp_temp,
     inform_swarm_cbp_perm,
     report
    ].

v3_cases() ->
    [
     simple_v3_exchange_md5,
     simple_v3_exchange_sha,
     simple_v3_exchange_sha224,
     simple_v3_exchange_sha256,
     simple_v3_exchange_sha384,
     simple_v3_exchange_sha512
    ].
    

event_tests_mt_cases() -> event_tests_cases().

tickets_cases() ->
    [
     {group, otp8015}, 
     {group, otp8395}
    ].

otp8015_cases() ->
    [
     otp8015_1
    ].

otp8395_cases() ->
    [
     otp8395_1
    ].

%%
%% -----
%%

init_per_suite(Config0) when is_list(Config0) ->

    ?IPRINT("init_per_suite -> entry with"
            "~n      Config0: ~p"
            "~n      Nodes:  ~p"
            "~n      explicit inet backend: ~p"
            "~n      test inet backends:    ~p",
            [Config0, erlang:nodes(),
             ?EXPLICIT_INET_BACKEND(),
             ?TEST_INET_BACKENDS()]),

    case ?LIB:init_per_suite(Config0) of
        {skip, _} = SKIP ->
            SKIP;

        Config1 ->

            ?IPRINT("init_per_suite -> common init done"
                    "~n      Config1: ~p", [Config1]),

            %% We need one on this node also
            snmp_test_sys_monitor:start(),

            Config2   = ?LIB:init_suite_top_dir(?MODULE, Config1), 
            Config3   = ?LIB:fix_data_dir(Config2),
            %% Mib-dirs
            %% data_dir is trashed by the test-server / common-test
            %% so there is no point in fixing it...
            MibDir    = ?LIB:lookup(data_dir, Config3),
            StdMibDir = filename:join([code:priv_dir(snmp), "mibs"]),
            [{mib_dir, MibDir}, {std_mib_dir, StdMibDir} | Config3]

    end.

end_per_suite(Config0) when is_list(Config0) ->

    ?IPRINT("end_per_suite -> entry with"
            "~n      Config0: ~p"
            "~n      Nodes:  ~p", [Config0, erlang:nodes()]),

    snmp_test_sys_monitor:stop(),
    Config1 = ?LIB:end_per_suite(Config0),

    ?IPRINT("end_per_suite -> end when"
            "~n      Nodes:  ~p", [erlang:nodes()]),
    Config1.


%%
%% -----
%%

init_per_group(GroupName, Config0) ->
    ?IPRINT("init_per_group -> entry with"
            "~n      GroupName: ~p"
            "~n      Config:    ~p"
            "~n   when"
            "~n      Nodes:     ~p",
            [GroupName, Config0, nodes()]),

    Config1 = init_per_group2(GroupName, Config0),

    ?IPRINT("init_per_group -> done when"
            "~n      GroupName: ~p"
            "~n      Config:    ~p"
            "~n      Nodes:     ~p",
            [GroupName, Config1, nodes()]),

    Config1.


init_per_group2(inet_backend_default = _GroupName, Config0) ->
    Config1 = [{socket_create_opts, []} | Config0],
    case ?EXPLICIT_INET_BACKEND() of
        true ->
            ?LIB:init_group_top_dir(default, Config1);
        false ->
            %% For a "standard" test (that is if we do not run the "extended"
            %% inet backends test) then we should always run this group!
            %% So, if we have an extended test, *then* (and only then) 
            %% check the factor.
            case ?TEST_INET_BACKENDS() of
                true ->
                    case lists:keysearch(snmp_factor, 1, Config0) of
                        {value, {snmp_factor, Factor}} when (Factor < 3) ->
                            ?LIB:init_group_top_dir(default, Config1);
                        _ ->
                            {skip, "Machine too slow"}
                    end;
                _ ->
                    ?LIB:init_group_top_dir(default, Config1)
            end                    
    end;
init_per_group2(inet_backend_inet = _GroupName, Config0) ->
    case ?EXPLICIT_INET_BACKEND() of
        true ->
            %% The environment trumps us,
            %% so only the default group should be run!
            {skip, "explicit inet backend"};
        false ->
            case lists:keysearch(snmp_factor, 1, Config0) of
                {value, {snmp_factor, Factor}} when (Factor < 5) ->
                    Config1 = [{socket_create_opts, [{inet_backend, inet}]} |
                               Config0],
                    ?LIB:init_group_top_dir(inet, Config1);
                _ ->
                    {skip, "Machine too slow"}
            end
    end;
init_per_group2(inet_backend_socket = _GroupName, Config0) ->
    case ?EXPLICIT_INET_BACKEND() of
        true ->
            %% The environment trumps us,
            %% so only the default group should be run!
            {skip, "explicit inet backend"};
        false ->
            %% Always run this unless a backend has been explicitly
            %% configured (since this is really what we want to test).
            Config1 = [{socket_create_opts, [{inet_backend, socket}]} |
                       Config0],
            ?LIB:init_group_top_dir(socket, Config1)
    end;
init_per_group2(all = GroupName, Config) ->
    ?LIB:init_group_top_dir(GroupName, Config);
init_per_group2(request_tests_mt = GroupName, Config) ->
    ?LIB:init_group_top_dir(
      GroupName,
      [{manager_net_if_module, snmpm_net_if_mt} | Config]);
init_per_group2(event_tests_mt = GroupName, Config) ->
    ?LIB:init_group_top_dir(
      GroupName,
      [{manager_net_if_module, snmpm_net_if_mt} | Config]);
init_per_group2(ipv6_mt = GroupName, Config) ->
    init_per_group_ipv6(GroupName,
                        [{manager_net_if_module, snmpm_net_if_mt} | Config]);   
init_per_group2(ipv6 = GroupName, Config) ->
    init_per_group_ipv6(GroupName, Config);
init_per_group2(v3 = GroupName, Config) ->
    case ?CRYPTO_START() of
	ok ->
            ?IPRINT("crypto started - check support"),
            case ?CRYPTO_SUPPORT() of
                {no, Reason} ->
                    ?WPRINT("crypto support not sufficient:"
                            "~n      ~p", [Reason]),
                    {skip, {unsupported_encryption, Reason}};
                yes ->
                    ?IPRINT("crypto supported"),
                    ?LIB:init_group_top_dir(GroupName, Config)
                end;
	{error, Reason} ->
            ?IPRINT("crypto not started:"
                    "~n      ~p", [Reason]),
	    {skip, {failed_starting_crypto, Reason}}
    end;
init_per_group2(usm_priv_aes_tests = GroupName, Config) ->
    %% Check crypto support
    case snmp_misc:is_crypto_supported(aes_128_cfb128) of
        true ->
            ?LIB:init_group_top_dir(GroupName, Config);
        false ->
            throw({skip, {not_supported, aes_128_cfb128}})
    end;
init_per_group2(GroupName, Config) ->
    ?LIB:init_group_top_dir(GroupName, Config).


init_per_group_ipv6(GroupName, Config) ->
    %% <OS-CONDITIONAL-SKIP>
    OSSkipable = [{unix, 
                   [
                    {darwin, fun(V) when (V > {9, 8, 0}) ->
				     %% This version is OK: No Skip
				     false;
				(_) ->
				     %% This version is *not* ok: Skip
                                     %% We need a fully qualified hostname
                                     %% to get a proper IPv6 address (in this
                                     %% version), but its just to messy, so 
                                     %% instead we skip this **OLD** darwin...
				     true
                             end}
                   ]
                  }],
    %% </OS-CONDITIONAL-SKIP>
    case ?OS_BASED_SKIP(OSSkipable) of
        true ->
            {skip, "Host *may* not *properly* support IPV6"};
        false ->
            %% Even if this host supports IPv6 we don't use it unless its
            %% one of the configures/supported IPv6 hosts...
            case ?HAS_SUPPORT_IPV6() of
                true ->
                    ipv6_init(?LIB:init_group_top_dir(GroupName, Config));
                false ->
                    {skip, "Host does not support IPv6"}
            end
    end.


end_per_group(_GroupName, Config) ->
    %% Do we really need to do this?
    lists:keydelete(snmp_group_top_dir, 1, Config).




%%
%% -----
%%

init_per_testcase(Case, Config) when is_list(Config) ->

    ?IPRINT("init_per_testcase -> entry with"
            "~n   Config: ~p"
            "~n   Nodes:  ~p", [Config, erlang:nodes()]),

    snmp_test_global_sys_monitor:reset_events(),
    
    %% This version of the API, based on Addr and Port, has been deprecated
    DeprecatedApiCases = 
	[
	],
    Result = 
	case lists:member(Case, DeprecatedApiCases) of
	    true ->
		{skip, "API no longer supported"};
	    false ->
		try init_per_testcase2(Case, Config)
                catch
                    C:{skip, _} = E:_ when ((C =:= throw) orelse
                                            (C =:= exit)) ->
                        E;
                    exit:{suite_failed, {{Reason, _CS},_MFA}, Mod, Line}:_ ->
                        {skip, {Reason, Mod, Line}};
                    exit:{suite_failed, Reason, Mod, Line}:_ ->
                        {skip, {Reason, Mod, Line}};
                    C:E:_ when ((C =:= throw) orelse
                                (C =:= exit)) ->
                        {skip, {caught, C, E}}
                end
	end,
    ?IPRINT("init_per_testcase end when"
            "~n      Nodes:  ~p"
            "~n      Result: ~p"
            "~n~n", [erlang:nodes(), Result]),
    Result.

init_per_testcase2(Case, Config) ->
    ?DBG("init_per_testcase2 -> "
	 "~n   Case:   ~p"
	 "~n   Config: ~p"
	 "~n   Nodes:  ~p", [Case, Config, erlang:nodes()]),

    CaseTopDir = snmp_test_lib:init_testcase_top_dir(Case, Config), 

    %% -- Manager dirs  --
    MgrTopDir  = filename:join(CaseTopDir, "manager/"), 
    ?DBG("init_per_testcase2 -> try create manager top dir: ~n~p", 
	 [MgrTopDir]),
    ok   = file:make_dir(MgrTopDir),

    MgrConfDir = filename:join(MgrTopDir,  "conf/"),
    ok   = file:make_dir(MgrConfDir),

    MgrDbDir   = filename:join(MgrTopDir,  "db/"),
    ok   = file:make_dir(MgrDbDir),

    MgrLogDir  = filename:join(MgrTopDir,  "log/"),
    ok   = file:make_dir(MgrLogDir),

    %% -- Agent dirs --
    AgTopDir  = filename:join(CaseTopDir, "agent/"),
    ok  = file:make_dir(AgTopDir),

    AgConfDir = filename:join(AgTopDir,   "conf/"),
    ok  = file:make_dir(AgConfDir),

    AgDbDir   = filename:join(AgTopDir,   "db/"),
    ok  = file:make_dir(AgDbDir),

    AgLogDir  = filename:join(AgTopDir,   "log/"),
    ok  = file:make_dir(AgLogDir),

    Family = proplists:get_value(ipfamily, Config, inet),

    Factor = ?config(snmp_factor, Config),
    TO = case Case of
             inform3 ->
                 ?MINS(2 + (Factor div 2));
             InformSwarm when (InformSwarm =:= inform_swarm_cbp_def) orelse
                              (InformSwarm =:= inform_swarm_cbp_temp) orelse
                              (InformSwarm =:= inform_swarm_cbp_perm) ->
                 ?MINS(1 + Factor);
             _ ->
                 ?MINS(1 + (Factor div 2))
         end,
    ?IPRINT("Set test case timetrap: ~p", [TO]),
    ct:timetrap(TO),

    Conf = [{ipfamily,                  Family},
	    {ip,                        ?LOCALHOST(Family)},
	    {case_top_dir,              CaseTopDir},
	    {agent_dir,                 AgTopDir},
	    {agent_conf_dir,            AgConfDir},
	    {agent_db_dir,              AgDbDir},
	    {agent_log_dir,             AgLogDir}, 
	    {manager_agent_target_name, "agent01"}, 
	    {manager_dir,               MgrTopDir},
	    {manager_conf_dir,          MgrConfDir},
	    {manager_db_dir,            MgrDbDir},
	    {manager_log_dir,           MgrLogDir} | Config],
    Conf2 = init_per_testcase3(Case, Conf),
    ?DBG("init [~w] Nodes [2]: ~p", [Case, erlang:nodes()]),
    Conf2.

init_per_testcase3(simple_v3_exchange_md5 = Case, Config) ->
    init_v3_testcase(Case, [{auth_alg, md5} | Config]);
init_per_testcase3(simple_v3_exchange_sha = Case, Config) ->
    init_v3_testcase(Case, [{auth_alg, sha} | Config]);
init_per_testcase3(simple_v3_exchange_sha224 = Case, Config) ->
    init_v3_testcase(Case, [{auth_alg, sha224} | Config]);
init_per_testcase3(simple_v3_exchange_sha256 = Case, Config) ->
    init_v3_testcase(Case, [{auth_alg, sha256} | Config]);
init_per_testcase3(simple_v3_exchange_sha384 = Case, Config) ->
    init_v3_testcase(Case, [{auth_alg, sha384} | Config]);
init_per_testcase3(simple_v3_exchange_sha512 = Case, Config) ->
    init_v3_testcase(Case, [{auth_alg, sha512} | Config]);
init_per_testcase3(Case, Config) ->
    ApiCases02 = 
	[
	 otp8395_1
	],
    ApiCases03 = 
	[
	 simple_sync_get3,
	 simple_async_get3,
	 simple_sync_get_next3,
	 simple_async_get_next3_cbp_def,
	 simple_async_get_next3_cbp_temp,
	 simple_async_get_next3_cbp_perm,
	 simple_sync_set3,
	 simple_async_set3_cbp_def,
	 simple_async_set3_cbp_temp,
	 simple_async_set3_cbp_perm,
	 simple_sync_get_bulk3,
         simple_async_get_bulk3_cbp_def,
         simple_async_get_bulk3_cbp_temp,
         simple_async_get_bulk3_cbp_perm
	],
    Cases = 
	[
	 trap1,
	 trap2,
	 inform1,
	 inform2,
	 inform3,
	 inform4,
	 inform_swarm_cbp_def,
         inform_swarm_cbp_temp,
         inform_swarm_cbp_perm,
	 report
	] ++ 
	ApiCases02 ++
	ApiCases03,
    case lists:member(Case, Cases) of
	true ->
	    NoAutoInformCases = [inform1, inform2, inform3,
                                 inform_swarm_cbp_def,
                                 inform_swarm_cbp_temp,
                                 inform_swarm_cbp_perm], 
	    AutoInform = not lists:member(Case, NoAutoInformCases),
	    Conf1 = if 
                        %% We turn off verbosity for the swarm cases
                        %% (too much output).
			(Case =:= inform_swarm_cbp_def) orelse
                        (Case =:= inform_swarm_cbp_temp) orelse
                        (Case =:= inform_swarm_cbp_perm) ->
			    Verb = [{manager_config_verbosity,     silence},
                                    {manager_note_store_verbosity, silence},
                                    {manager_server_verbosity,     info},
                                    {manager_net_if_verbosity,     info},
                                    {agent_verbosity,              info}, 
                                    {agent_net_if_verbosity,       info}],
                            Verb ++ Config;
			Case =:= otp8395_1 ->
			    [{manager_atl_seqno, true} | Config];
			true ->
			    Config
		    end,
            Conf2 = if
                        (Case =:= simple_async_get_next3_cbp_temp) orelse
                        (Case =:= simple_async_set3_cbp_temp) orelse
                        (Case =:= simple_async_get_bulk3_cbp_temp) orelse
                        (Case =:= inform_swarm_cbp_temp) ->
                            [{manager_server_cbproxy, temporary} | Conf1];
                        (Case =:= simple_async_get_next3_cbp_perm) orelse
                        (Case =:= simple_async_set3_cbp_perm) orelse
                        (Case =:= simple_async_get_bulk3_cbp_perm) orelse
                        (Case =:= inform_swarm_cbp_perm) ->
                            [{manager_server_cbproxy, permanent} | Conf1];
                        true ->
                            Conf1
                    end,
            %% We don't need to try catch this (init_agent)
            %% since we have a try catch "higher up"...
	    Conf3 = init_agent(Case, Conf2),
	    Conf4 = try init_manager(Case, AutoInform, Conf3)
                    catch AC:AE:_ ->
                            %% Ouch we need to clean up: 
                            %% The init_agent starts an agent node!
                            init_per_testcase_fail_agent_cleanup(Conf2),
                            throw({skip, {manager_init_failed, AC, AE}})
                    end,
	    Conf5 = try init_mgr_user(Conf4)
                    catch MC:ME:_ ->
                            %% Ouch we need to clean up: 
                            %% The init_agent starts an agent node!
                            %% The init_magager starts an manager node!
                            init_per_testcase_fail_manager_cleanup(Conf3),
                            init_per_testcase_fail_agent_cleanup(Conf3),
                            throw({skip, {manager_user_init_failed, MC, ME}})
                    end,
	    case lists:member(Case, ApiCases02 ++ ApiCases03) of
		true ->
		    init_mgr_user_data2(Conf5);
		false ->
		    init_mgr_user_data1(Conf5)
	    end;
	false ->
	    Config
    end.

init_v3_testcase(Case, Conf) ->
    Conf2 = init_v3_agent(Case, Conf),
    Conf3 = try init_v3_manager(Case, Conf2)
            catch AC:AE:AS ->
                    %% Ouch we need to clean up:
                    %% The init_agent starts an agent node!
                    ?IPRINT("init_v3_testcase -> failed init v3 manager:"
                            "~n      AC: ~p"
                            "~n      AE: ~p"
                            "~n      AS: ~p", [AC, AE, AS]),
                    init_per_testcase_fail_agent_cleanup(Conf),
                    throw({skip, {manager_init_failed, AC, AE}})
            end,
    Conf4 = try init_mgr_v3_user(Conf3)
            catch MC:ME:MS ->
                    %% Ouch we need to clean up:
                    %% The init_agent starts an agent node!
                    %% The init_magager starts an manager node!
                    ?IPRINT("init_v3_testcase -> failed init v3 manager user:"
                            "~n      MC: ~p"
                            "~n      ME: ~p"
                            "~n      MS: ~p", [MC, ME, MS]),
                    init_per_testcase_fail_manager_cleanup(Conf2),
                    init_per_testcase_fail_agent_cleanup(Conf2),
                    throw({skip, {manager_user_init_failed, MC, ME}})
            end,
    init_mgr_v3_user_data(Conf4).

init_per_testcase_fail_manager_cleanup(Conf) ->
    (catch fin_manager(Conf)).

init_per_testcase_fail_agent_cleanup(Conf) ->
    (catch fin_agent(Conf)).

end_per_testcase(Case, Config) when is_list(Config) ->
    ?IPRINT("end_per_testcase -> entry with"
            "~n   Config:        ~p"
            "~n   Nodes:         ~p",
            [Config, erlang:nodes()]),

    ?IPRINT("system events during test: "
            "~n   ~p", [snmp_test_global_sys_monitor:events()]),

    Conf1  = Config,
    Conf2  = end_per_testcase2(Case, Conf1),

    ?IPRINT("end_per_testcase -> done with"
            "~n   Config: ~p"
            "~n   Nodes:  ~p", [Conf2, erlang:nodes()]),

    Conf2.

end_per_testcase2(Case, Config)
  when ((Case =:= simple_v3_exchange_md5)    orelse
        (Case =:= simple_v3_exchange_sha)    orelse
        (Case =:= simple_v3_exchange_sha224) orelse
        (Case =:= simple_v3_exchange_sha256) orelse
        (Case =:= simple_v3_exchange_sha384) orelse
        (Case =:= simple_v3_exchange_sha512)) ->
    Conf1 = fin_mgr_user_data2(Config),
    Conf2 = fin_mgr_user(Conf1),
    Conf3 = fin_v3_manager(Conf2),
    fin_v3_agent(Conf3);
end_per_testcase2(Case, Config) ->
    ApiCases02 = 
	[
	 otp8395_1
	],
    ApiCases03 = 
	[
	 simple_sync_get3, 
	 simple_async_get3, 
	 simple_sync_get_next3, 
	 simple_async_get_next3_cbp_def, 
	 simple_async_get_next3_cbp_temp, 
	 simple_async_get_next3_cbp_perm, 
	 simple_sync_set3, 
	 simple_async_set3_cbp_def,
	 simple_async_set3_cbp_temp,
	 simple_async_set3_cbp_perm,
	 simple_sync_get_bulk3, 
         simple_async_get_bulk3_cbp_def,
         simple_async_get_bulk3_cbp_temp,
         simple_async_get_bulk3_cbp_perm
	],
    Cases = 
	[
	 trap1,
	 trap2,
	 inform1,
	 inform2,
	 inform3,
	 inform4,
         inform_swarm_cbp_def,
         inform_swarm_cbp_temp,
         inform_swarm_cbp_perm,
	 report
	] ++ 
	ApiCases02 ++ 
	ApiCases03,
    case lists:member(Case, Cases) of
	true ->
	    Conf1 = case lists:member(Case, ApiCases02 ++ ApiCases03) of
			true ->
			    fin_mgr_user_data2(Config);
			false ->
			    fin_mgr_user_data1(Config)
		    end,
	    Conf2 = fin_mgr_user(Conf1),
	    Conf3 = fin_manager(Conf2),
	    fin_agent(Conf3);
	false ->
	    Config
    end.


%%======================================================================
%% Test case definitions
%%======================================================================


%%======================================================================
%% Test functions
%%======================================================================

simple_start_and_stop1(Config) when is_list(Config) ->
    ?TC_TRY(simple_start_and_stop1,
            fun() -> do_simple_start_and_stop1(Config) end).

do_simple_start_and_stop1(Config) ->
    ?IPRINT("starting with Config: "
            "~n      ~p", [Config]),
    SCO     = ?config(socket_create_opts, Config),
    ConfDir = ?config(manager_conf_dir,   Config),
    DbDir   = ?config(manager_db_dir,     Config),

    write_manager_conf(ConfDir),

    Opts = [{server,     [{verbosity, trace}]},
	    {net_if,     [{verbosity, trace}, {options, SCO}]},
	    {note_store, [{verbosity, trace}]},
	    {config,     [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],

    ?IPRINT("try starting manager"),
    ok = snmpm:start_link(Opts),

    ?SLEEP(1000),

    ?IPRINT("manager started, now try to stop"),
    ok = snmpm:stop(),

    ?SLEEP(1000),

    ok.


%%======================================================================

simple_start_and_stop2(Config) when is_list(Config) ->
    Pre  = fun() ->
                   ManagerNode = start_node(simple_start_and_stop2),
                   [ManagerNode]
           end,
    Case = fun(State) -> do_simple_start_and_stop2(State, Config) end,
    Post = fun([{ManagerPeer, _ManagerNode}]) -> peer:stop(ManagerPeer) end,
    ?TC_TRY(simple_start_and_stop2, Pre, Case, Post).

do_simple_start_and_stop2([{_ManagerPeer, ManagerNode}], Config) ->
    ?IPRINT("starting with Config: "
            "~n      ~p"
            "~n", [Config]),

    SCO     = ?config(socket_create_opts, Config),
    ConfDir = ?config(manager_conf_dir,   Config),
    DbDir   = ?config(manager_db_dir,     Config),

    write_manager_conf(ConfDir),

    Opts = [{server,     [{verbosity, trace}]},
	    {net_if,     [{verbosity, trace}, {options, SCO}]},
	    {note_store, [{verbosity, trace}]},
	    {config,     [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],


    ?IPRINT("try load snmp application"),
    ok = load_snmp(ManagerNode),

    ?IPRINT("try set manager env for the snmp application"),
    ok = set_mgr_env(ManagerNode, Opts),

    ?IPRINT("try starting snmp application (with only manager)"),
    ok = start_snmp(ManagerNode),

    ?IPRINT("started"),

    ?SLEEP(1000),

    ?IPRINT("try stopping snmp application (with only manager)"),
    ok = stop_snmp(ManagerNode),

    ?SLEEP(1000),
    ?IPRINT("end"),

    ok.


%%======================================================================

simple_start_and_stop3(Config) when is_list(Config) ->
    ?TC_TRY(simple_start_and_stop3,
            fun() -> do_simple_start_and_stop3(Config) end).

do_simple_start_and_stop3(Config) ->
    ?IPRINT("starting with Config: "
            "~n      ~p", [Config]),

    SCO     = ?config(socket_create_opts, Config),
    ConfDir = ?config(manager_conf_dir, Config),
    DbDir   = ?config(manager_db_dir, Config),

    write_manager_conf(ConfDir),

    Opts = [{server,     [{verbosity, trace}]},
	    {net_if,     [{verbosity, trace},
                          {options,   SCO ++ [{extra_sock_opts, ['this-should-not-work']}]}]},
	    {note_store, [{verbosity, trace}]},
	    {config,     [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],

    ?IPRINT("try starting manager"),
    try snmpm:start_link(Opts) of
        ok ->
            (catch snmpm:stop()),
            ?FAIL('unexpected-success')
    catch
        _:_:_ ->
            ?IPRINT("expected start failure"),
            ok
    end,

    ?SLEEP(1000),
    ?IPRINT("end"),

    ok.


%%======================================================================

simple_start_and_monitor_crash1(Config) when is_list(Config) ->
    ?TC_TRY(simple_start_and_monitor_crash1,
            fun() -> do_simple_start_and_monitor_crash1(Config) end).

do_simple_start_and_monitor_crash1(Config) ->
    ?IPRINT("starting with Config: "
            "~n      ~p", [Config]),

    SCO     = ?config(socket_create_opts, Config),
    ConfDir = ?config(manager_conf_dir,   Config),
    DbDir   = ?config(manager_db_dir,     Config),

    write_manager_conf(ConfDir),

    Opts = [{server,     [{verbosity, trace}]},
	    {net_if,     [{verbosity, trace}, {options, SCO}]},
	    {note_store, [{verbosity, trace}]},
	    {config,     [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],

    ?IPRINT("try starting manager"),
    ok = snmpm:start(Opts),

    ?SLEEP(1000),

    ?IPRINT("create the monitor"),
    Ref = snmpm:monitor(),

    ?IPRINT("make sure it has not already crashed..."),
    receive
	{'DOWN', Ref, process, Obj1, Reason1} ->
	    ?FAIL({unexpected_down, Obj1, Reason1})
    after 1000 ->
	    ok
    end,

    ?IPRINT("stop the manager"),
    ok = snmpm:stop(),

    ?IPRINT("await the down-message"),
    receive
	{'DOWN', Ref, process, Obj2, Reason2} ->
	    ?IPRINT("received expected down-message: "
                    "~n   Obj2:    ~p"
                    "~n   Reason2: ~p", 
                    [Obj2, Reason2]),
	    ok
    after 1000 ->
            %% The manager is an entire process tree and we can't
            %% wait for all of them. Instead, we assume that if
            %% we deal with the top supervisor, all the other procs
            %% will also follow...
            ?ENSURE_NOT_RUNNING(snmpm_supervisor,
                                fun() -> snmpm:stop() end,
                                1000),
	    ?FAIL(timeout)
    end,
    ?IPRINT("end"),

    ok.


%%======================================================================

simple_start_and_monitor_crash2(Config) when is_list(Config) ->
    Cond = fun() -> case os:type() of
			{unix, netbsd} ->
			    {skip, "Unstable on NetBSD"};
			_ ->
			    ok
		    end
	   end,
    Pre  = fun()  -> undefined end,
    Case = fun(_) -> do_simple_start_and_monitor_crash2(Config) end,
    Post = fun(_) -> ok end,
    ?TC_TRY(simple_start_and_monitor_crash2,
	    Cond, Pre, Case, Post).

do_simple_start_and_monitor_crash2(Config) ->
    ?IPRINT("starting with Config: "
            "~n      ~p", [Config]),

    SCO     = ?config(socket_create_opts, Config),
    ConfDir = ?config(manager_conf_dir,   Config),
    DbDir   = ?config(manager_db_dir,     Config),

    write_manager_conf(ConfDir),

    Opts = [{restart_type, permanent},
	    {server,     [{verbosity, trace}]},
	    {net_if,     [{verbosity, trace}, {options, SCO}]},
	    {note_store, [{verbosity, trace}]},
	    {config,     [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],

    ?IPRINT("try starting manager"),
    ok = snmpm:start(Opts),

    ?SLEEP(1000),

    ?IPRINT("create the monitor"),
    Ref = snmpm:monitor(),

    ?IPRINT("make sure it has not already crashed..."),
    receive
	{'DOWN', Ref, process, Obj1, Reason1} ->
	    ?FAIL({unexpected_down, Obj1, Reason1})
    after 1000 ->
	    ok
    end,

    ?IPRINT("crash the manager"),
    simulate_crash(),

    ?IPRINT("await the down-message"),
    receive
	{'DOWN', Ref, process, Obj2, Reason2} ->
	    ?IPRINT("received expected down-message: "
                    "~n   Obj2:    ~p"
                    "~n   Reason2: ~p", 
                    [Obj2, Reason2]),
	    ok
    after 1000 ->
	    ?FAIL(timeout)
    end,

    ok.


%% The server supervisor allow 5 restarts in 500 msec.
server_pid() ->
    whereis(snmpm_server).

-define(MAX_KILLS, 6).

simulate_crash() ->
    simulate_crash(0, server_pid()).

simulate_crash(?MAX_KILLS, _) ->
    ?SLEEP(1000),
    case server_pid() of
	P when is_pid(P) ->
	    exit({error, {still_alive, P}});
	_ ->
	    ok
    end;
simulate_crash(NumKills, Pid) when (NumKills < ?MAX_KILLS) and is_pid(Pid) ->
    ?IPRINT("similate_crash -> ~w, ~p", [NumKills, Pid]),
    Ref = erlang:monitor(process, Pid),
    exit(Pid, kill),
    receive 
	{'DOWN', Ref, process, _Object, _Info} ->
	    ?IPRINT("received expected 'DOWN' message"),
	    simulate_crash(NumKills + 1, server_pid())
    after 1000 ->
	    case server_pid() of
		P when is_pid(P) ->
                    ?EPRINT("received expected 'DOWN' message"),
		    exit({error, {no_down_from_server, P}});
		_ ->
		    ok
	    end
    end;
simulate_crash(NumKills, _) ->
    ?SLEEP(10),
    simulate_crash(NumKills, server_pid()).


%%======================================================================

notify_started01(Config) when is_list(Config) ->
    ?TC_TRY(notify_started01,
            fun() -> do_notify_started01(Config) end).

do_notify_started01(Config) ->
    ?IPRINT("starting with Config: "
            "~n      ~p", [Config]),

    SCO     = ?config(socket_create_opts, Config),
    ConfDir = ?config(manager_conf_dir,   Config),
    DbDir   = ?config(manager_db_dir,     Config),

    write_manager_conf(ConfDir),

    Opts = [{server,     [{verbosity, log}]},
	    {net_if,     [{verbosity, silence}, {options, SCO}]},
	    {note_store, [{verbosity, silence}]},
	    {config,     [{verbosity, log}, {dir, ConfDir}, {db_dir, DbDir}]}],

    ?IPRINT("request start notification (1)"),
    Pid1 = snmpm:notify_started(10000),
    receive
	{snmpm_start_timeout, Pid1} ->
	    ?IPRINT("received expected start timeout"),
	    ok;
	Any1 ->
	    ?FAIL({unexpected_message, Any1})
    after 15000 ->
	    ?FAIL({unexpected_timeout, Pid1})
    end,

    ?IPRINT("request start notification (2)"),
    Pid2 = snmpm:notify_started(10000),

    ?IPRINT("start the snmpm starter"),
    Pid = snmpm_starter(Opts, 5000),

    ?IPRINT("await the start notification"),
    Ref = 
	receive
	    {snmpm_started, Pid2} ->
		?IPRINT("received started message -> create the monitor"),
		snmpm:monitor();
	    Any2 ->
		?FAIL({unexpected_message, Any2})
	after 15000 ->
		?FAIL({unexpected_timeout, Pid2})
	end,

    ?IPRINT("[~p] make sure it has not already crashed...", [Ref]),
    receive
	{'DOWN', Ref, process, Obj1, Reason1} ->
	    ?FAIL({unexpected_down, Obj1, Reason1})
    after 1000 ->
	    ok
    end,

    ?IPRINT("stop the manager"),
    Pid ! {stop, self()}, %ok = snmpm:stop(),

    ?IPRINT("await the down-message"),
    receive
	{'DOWN', Ref, process, Obj2, Reason2} ->
	    ?IPRINT("received expected down-message: "
                    "~n   Obj2:    ~p"
                    "~n   Reason2: ~p", 
                    [Obj2, Reason2]),
	    ok
    after 5000 ->
	    ?FAIL(down_timeout)
    end,

    ?IPRINT("end"),
    ok.


snmpm_starter(Opts, To) ->
    Parent = self(),
    spawn(
      fun() -> 
	      ?SLEEP(To), 
	      ok = snmpm:start(Opts),
	      receive
		  {stop, Parent} ->
		      snmpm:stop()
	      end
      end).


%%======================================================================

notify_started02(Config) when is_list(Config) ->
    ?TC_TRY(notify_started02,
            fun() -> notify_started02_cond(Config) end,
            fun() -> do_notify_started02(Config) end).

notify_started02_cond(Config) ->
    LinuxVersionVerify = 
	fun() ->
                case os:version() of
                    V when V > {2, 6, 16} ->
                        ?IPRINT("(Linux) kernel version check: "
                                "~p > {2, 6, 16} => *NO* SKIP", [V]),
                        false;
                    V ->
                        ?IPRINT("(Linux) kernel version check: "
                                "~p =< {2, 6, 16}  => *SKIP*", [V]),
                        true
		end
	end,
    Skippable = [{unix, [{linux, LinuxVersionVerify}]}],
    Condition = fun() -> ?OS_BASED_SKIP(Skippable) end,
    ?NON_PC_TC_MAYBE_SKIP(Config, Condition).
    
do_notify_started02(Config) ->
    ?IPRINT("starting with Config: "
            "~n      ~p", [Config]),

    SCO     = ?config(socket_create_opts, Config),
    ConfDir = ?config(manager_conf_dir,   Config),
    DbDir   = ?config(manager_db_dir,     Config),

    write_manager_conf(ConfDir),

    Opts = [{server,     [{verbosity, log}]},
	    {net_if,     [{verbosity, silence}, {options, SCO}]},
	    {note_store, [{verbosity, silence}]},
	    {config,     [{verbosity, debug}, {dir, ConfDir}, {db_dir, DbDir}]}],

    ?IPRINT("start snmpm client process"),
    NumIterations = 5,
    Pid1 = ns02_client_start(NumIterations),

    ?IPRINT("start snmpm ctrl (starter) process"),
    Pid2 = ns02_ctrl_start(Opts, NumIterations),

    %% On a reasonably fast machine, one iteration takes approx 4 seconds.
    %% We measure the first iteration, and then we wait for the remaining
    %% ones (4 in this case).
    ApproxStartTime =
        case ns02_client_await_approx_runtime(Pid1) of
            {ok, T} ->
                T;
            {error, Reason} ->
                %% Attempt cleanup just in case
                exit(Pid1, kill),
                exit(Pid2, kill),
                ?FAIL(Reason);
            {skip, Reason} ->
                %% Attempt cleanup just in case
                exit(Pid1, kill),
                exit(Pid2, kill),
                ?SKIP(Reason)
        end,

    ?IPRINT("await snmpm client process exit (max ~p+10000 msec)", 
            [ApproxStartTime]),
    receive 
        %% We take this opportunity to check if we got a skip from
        %% the ctrl process.
	{'EXIT', Pid2, {skip, SkipReason1}} ->
	    ?SKIP(SkipReason1);
	{'EXIT', Pid1, normal} ->
	    ok;
	{'EXIT', Pid1, {suite_failed, Reason1}} ->
	    ?FAIL({client, Reason1});
	{'EXIT', Pid1, Reason1} ->
	    ?FAIL({client, Reason1})
    after ApproxStartTime + 10000 ->
            exit(Pid1, kill),
            exit(Pid2, kill),
	    ?FAIL(timeout)
    end,
	
    ?IPRINT("await snmpm starter process exit"),
    receive 
	{'EXIT', Pid2, normal} ->
	    ok;
	{'EXIT', Pid2, {skip, SkipReason2}} ->
            %% In case of a race
	    ?SKIP(SkipReason2);
	{'EXIT', Pid2, Reason2} ->
	    ?FAIL({ctrl, Reason2})
    after 5000 ->
            exit(Pid2, kill),
	    ?FAIL(timeout)
    end,
	
    ?IPRINT("end"),
    ok.


ns02_client_start(N) ->
    Self = self(),
    spawn_link(fun() -> ns02_client(Self, N) end).

ns02_client_await_approx_runtime(Pid) ->
    receive
        {?MODULE, client_time, Time} ->
            {ok, Time};
        {'EXIT', Pid, Reason} ->
            ?EPRINT("client (~p) failed: "
                    "~n      ~p", [Pid, Reason]),
            {error, Reason}
                
    after 30000 ->
            %% Either something is *really* wrong or this machine 
            %% is dog slow. Either way, this is a skip-reason...
            {skip, approx_runtime_timeout}
    end.
    
		       
ns02_client(Parent, N) when is_pid(Parent) ->
    put(tname, ns02_client),
    ?IPRINT("starting"),
    ns02_client_loop(Parent, 
                     dummy, snmpm:notify_started(?NS_TIMEOUT),
                     snmp_misc:now(ms), undefined,
                     N).

ns02_client_loop(_Parent, _Ref, _Pid, _Begin, _End, 0) ->
    %% ?IPRINT("loop -> done"),
    exit(normal);
ns02_client_loop(Parent, Ref, Pid, Begin, End, N) 
  when is_pid(Parent) andalso is_integer(Begin) andalso is_integer(End) ->
    %% ?IPRINT("loop -> [~w] inform parent: ~w, ~w => ~w", [N, Begin, End, End-Begin]),
    Parent ! {?MODULE, client_time, N*(End-Begin)},
    ns02_client_loop(undefined, Ref, Pid, snmp_misc:now(ms), undefined, N);    
ns02_client_loop(Parent, Ref, Pid, Begin, End, N) 
  when is_integer(Begin) andalso is_integer(End) ->
    %% ?IPRINT("loop -> [~w] entry when"
    %%   "~n      Ref:   ~p"
    %%   "~n      Pid:   ~p"
    %%   "~n      Begin: ~p"
    %%   "~n      End:   ~p", [N, Ref, Pid, Begin, End]),
    ns02_client_loop(Parent, Ref, Pid, snmp_misc:now(ms), undefined, N);    
ns02_client_loop(Parent, Ref, Pid, Begin, End, N) ->
    %% ?IPRINT("loop(await message) -> [~w] entry when"
    %%   "~n      Ref:   ~p"
    %%   "~n      Pid:   ~p"
    %%   "~n      Begin: ~p"
    %%   "~n      End:   ~p", [N, Ref, Pid, Begin, End]),
    receive
	{snmpm_started, Pid} ->
	    ?IPRINT("received expected started message (~w)", [N]),
	    ns02_client_loop(Parent,
                             snmpm:monitor(), dummy,
                             Begin, End,
                             N);
	{snmpm_start_timeout, Pid} ->
	    ?EPRINT("unexpected timeout"),
	    ?FAIL({unexpected_start_timeout, Pid});
	{'DOWN', Ref, process, Obj, Reason} ->
	    ?IPRINT("received expected DOWN message (~w) with"
                    "~n   Obj:    ~p"
                    "~n   Reason: ~p", [N, Obj, Reason]),
	    ns02_client_loop(Parent,
                             dummy, snmpm:notify_started(?NS_TIMEOUT),
                             Begin, snmp_misc:now(ms),
                             N-1)
    end.

ns02_ctrl_start(Opts, N) ->
    spawn_link(fun() -> ns02_ctrl(Opts, N) end).
		       
ns02_ctrl(Opts, N) ->
    put(tname, ns02_ctrl),
    ?IPRINT("starting"),
    ns02_ctrl_loop(Opts, N).


%% We have seen that some times it takes unreasonably long time to
%% start the manager (it got "stuck" in snmpm_config). But since
%% we did not have enough verbosity, we do not know how far it got.
%% So, we try to monitor each start attempt. We allow 5 sec (just 
%% to give slow boxes a chance).
ns02_ctrl_loop(_Opts, 0) ->
    ?IPRINT("done"),
    exit(normal);
ns02_ctrl_loop(Opts, N) ->
    ?IPRINT("entry when N: ~p", [N]),
    ?SLEEP(2000),
    ?IPRINT("start manager"),
    TS1 = erlang:system_time(millisecond),
    {StarterPid, StarterMRef} =
        erlang:spawn_monitor(fun() -> exit(snmpm:start(Opts)) end),
    receive
        {'DOWN', StarterMRef, process, StarterPid, ok} ->
            TS2 = erlang:system_time(millisecond),
            ?IPRINT("manager started: ~w ms", [TS2-TS1]),
            ok
    after 5000 ->
            ?EPRINT("manager (~p) start timeout - kill", [StarterPid]),
            exit(StarterPid, kill),
            exit({skip, start_timeout})
    end,
    ?SLEEP(2000),
    ?IPRINT("stop manager"),
    ?SLEEP(100), % Give the verbosity to take effect...
    TS3 = erlang:system_time(millisecond),
    case snmpm:stop(5000) of
        ok ->
            TS4 = erlang:system_time(millisecond),
            ?IPRINT("manager stopped: ~p ms", [TS4-TS3]),
            ok;
        {error, timeout} ->
            ?EPRINT("manager stop timeout - kill (cleanup) and skip"),
            exit(whereis(snmpm_supervisor), kill),
            exit({skip, stop_timeout})
    end,
    ns02_ctrl_loop(Opts, N-1).



%%======================================================================

info(Config) when is_list(Config) ->
    Pre = fun() ->
                  SCO     = ?config(socket_create_opts, Config),
                  ConfDir = ?config(manager_conf_dir,   Config),
                  DbDir   = ?config(manager_db_dir,     Config),

                  write_manager_conf(ConfDir),

                  Opts = [{server,     [{verbosity, trace}]},
                          {net_if,     [{verbosity, trace}, {options, SCO}]},
                          {note_store, [{verbosity, trace}]},
                          {config,    [{verbosity, trace}, 
                                       {dir,       ConfDir}, 
                                       {db_dir,    DbDir}]}],
                  ?IPRINT("try starting manager"),
                  ok = snmpm:start(Opts),
                  ?SLEEP(1000),
                  ok
          end,
    Case = fun(_) -> do_info(Config) end,
    Post = fun(_) ->
                   ?IPRINT("info verified, now try to stop"),
                   snmpm:stop(),
                   ?SLEEP(1000),
                   ok
           end,
    ?TC_TRY(info, Pre, Case, Post).


do_info(Config) ->
    ?IPRINT("starting with Config: "
            "~n      ~p", [Config]),

    ?IPRINT("get info"),
    Info = snmpm:info(), 
    ?IPRINT("got info, now verify: "
            "~n   ~p", [Info]),
    ok = verify_info( Info ),

    ?IPRINT("end"),
    ok.

verify_info(Info) when is_list(Info) ->
    Keys = [{server,     [process_memory, db_memory]},
	    {config,     [process_memory, db_memory]},
	    {net_if,     [process_memory, transport_info]},
	    {note_store, [process_memory, db_memory]},
	    stats_counters],
    try verify_info(Keys, Info)
    catch
        C:E:S ->
            ?IPRINT("Verification Failed: "
                    "~n      Class: ~p"
                    "~n      Error: ~p"
                    "~n      Stack: ~p", [C, E, S]),
            {error, {verification_failed, C, E, S}}
    end;
verify_info(BadInfo) ->
    {error, {bad_info, BadInfo}}.

verify_info([], _) ->
    ?IPRINT("verified"),
    ok;
verify_info([Key|Keys], Info) when is_atom(Key) ->
    ?IPRINT("try verify '~p'", [Key]),
    case lists:keymember(Key, 1, Info) of
	true ->
	    verify_info(Keys, Info);
	false ->
            ?IPRINT("Verification of '~p' failed", [Key]),
	    {error, {missing_info, {Key, Info}}}
    end;
verify_info([{Key, SubKeys}|Keys], Info) ->
    ?IPRINT("try verify '~p' with sub-keys: "
            "~n      ~p", [Key, SubKeys]),    
    case lists:keysearch(Key, 1, Info) of
	{value, {Key, SubInfo}} ->
            SubInfo2 =
                if is_list(SubInfo) -> SubInfo;
                   is_map(SubInfo)  -> maps:to_list(SubInfo)
                end,
            ?IPRINT("try verify sub-key(s) with sub-info: "
                    "~n      (Sub-) Keys: ~p"
                    "~n      (Sub-) Info: ~p", [SubKeys, SubInfo2]),
	    case verify_info(SubKeys, SubInfo2) of
		ok ->
		    verify_info(Keys, Info);
		{error, {missing_info, {SubKey, _}}} ->
                    ?IPRINT("Verification of sub-key '~p' failed", [SubKey]),
		    {error, {missing_subinfo, {Key, SubKey, Info}}}
	    end;
	false ->
            ?IPRINT("Verification of key '~p' failed", [Key]),
	    {error, {missing_info, {Key, Info}}}
    end.


%%======================================================================

%% USM privacy fails with AES in OTP 22.2.3. Test to prevent
%% regression in future releases.
%%
usm_priv_aes(Config) when is_list(Config) ->
    Pre = fun() ->
                  SCO     = ?config(socket_create_opts, Config),
                  ConfDir = ?config(manager_conf_dir,   Config),
                  DbDir   = ?config(manager_db_dir,     Config),

                  write_manager_conf(ConfDir),

                  Opts = [{server,     [{verbosity, trace}]},
                          {net_if,     [{verbosity, trace}, {options, SCO}]},
                          {note_store, [{verbosity, trace}]},
                          {config,     [{verbosity, trace},
                                        {dir,       ConfDir},
                                        {db_dir,    DbDir}]}],

                  ?IPRINT("try starting manager"),
                  ok = snmpm:start(Opts),
                  ?SLEEP(1000), % Give it time to settle
                  ok
          end,
    Case = fun(_) -> do_usm_priv_aes(sha, Config) end,
    Post = fun(_) ->
                   ?IPRINT("try stop manager"),
                   ok = snmpm:stop(),
                   ?SLEEP(1000), % Give it time to settle
                   ok
           end,
    ?TC_TRY(usm_priv_aes, Pre, Case, Post).


%%======================================================================

usm_sha224_priv_aes(Config) when is_list(Config) ->
    Pre = fun() ->
                  SCO     = ?config(socket_create_opts, Config),
                  ConfDir = ?config(manager_conf_dir,   Config),
                  DbDir   = ?config(manager_db_dir,     Config),

                  write_manager_conf(ConfDir),

                  Opts = [{server,     [{verbosity, trace}]},
                          {net_if,     [{verbosity, trace}, {options, SCO}]},
                          {note_store, [{verbosity, trace}]},
                          {config,     [{verbosity, trace},
                                        {dir,       ConfDir},
                                        {db_dir,    DbDir}]}],

                  ?IPRINT("try starting manager"),
                  ok = snmpm:start(Opts),
                  ?SLEEP(1000), % Give it time to settle
                  ok
          end,
    Case = fun(_) -> do_usm_priv_aes(sha224, Config) end,
    Post = fun(_) ->
                   ?IPRINT("try stop manager"),
                   ok = snmpm:stop(),
                   ?SLEEP(1000), % Give it time to settle
                   ok
           end,
    ?TC_TRY(usm_sha224_priv_aes, Pre, Case, Post).


%%======================================================================

usm_sha256_priv_aes(Config) when is_list(Config) ->
    Pre = fun() ->
                  SCO     = ?config(socket_create_opts, Config),
                  ConfDir = ?config(manager_conf_dir,   Config),
                  DbDir   = ?config(manager_db_dir,     Config),

                  write_manager_conf(ConfDir),

                  Opts = [{server,     [{verbosity, trace}]},
                          {net_if,     [{verbosity, trace}, {options, SCO}]},
                          {note_store, [{verbosity, trace}]},
                          {config,     [{verbosity, trace},
                                        {dir,       ConfDir},
                                        {db_dir,    DbDir}]}],

                  ?IPRINT("try starting manager"),
                  ok = snmpm:start(Opts),
                  ?SLEEP(1000), % Give it time to settle
                  ok
          end,
    Case = fun(_) -> do_usm_priv_aes(sha256, Config) end,
    Post = fun(_) ->
                   ?IPRINT("try stop manager"),
                   ok = snmpm:stop(),
                   ?SLEEP(1000), % Give it time to settle
                   ok
           end,
    ?TC_TRY(usm_sha256_priv_aes, Pre, Case, Post).


%%======================================================================

usm_sha384_priv_aes(Config) when is_list(Config) ->
    Pre = fun() ->
                  SCO     = ?config(socket_create_opts, Config),
                  ConfDir = ?config(manager_conf_dir,   Config),
                  DbDir   = ?config(manager_db_dir,     Config),

                  write_manager_conf(ConfDir),

                  Opts = [{server,     [{verbosity, trace}]},
                          {net_if,     [{verbosity, trace}, {options, SCO}]},
                          {note_store, [{verbosity, trace}]},
                          {config,     [{verbosity, trace},
                                        {dir,       ConfDir},
                                        {db_dir,    DbDir}]}],

                  ?IPRINT("try starting manager"),
                  ok = snmpm:start(Opts),
                  ?SLEEP(1000), % Give it time to settle
                  ok
          end,
    Case = fun(_) -> do_usm_priv_aes(sha384, Config) end,
    Post = fun(_) ->
                   ?IPRINT("try stop manager"),
                   ok = snmpm:stop(),
                   ?SLEEP(1000), % Give it time to settle
                   ok
           end,
    ?TC_TRY(usm_sha384_priv_aes, Pre, Case, Post).


%%======================================================================

usm_sha512_priv_aes(Config) when is_list(Config) ->
    Pre = fun() ->
                  SCO     = ?config(socket_create_opts, Config),
                  ConfDir = ?config(manager_conf_dir,   Config),
                  DbDir   = ?config(manager_db_dir,     Config),

                  write_manager_conf(ConfDir),

                  Opts = [{server,     [{verbosity, trace}]},
                          {net_if,     [{verbosity, trace}, {options, SCO}]},
                          {note_store, [{verbosity, trace}]},
                          {config,     [{verbosity, trace},
                                        {dir,       ConfDir},
                                        {db_dir,    DbDir}]}],

                  ?IPRINT("try starting manager"),
                  ok = snmpm:start(Opts),
                  ?SLEEP(1000), % Give it time to settle
                  ok
          end,
    Case = fun(_) -> do_usm_priv_aes(sha512, Config) end,
    Post = fun(_) ->
                   ?IPRINT("try stop manager"),
                   ok = snmpm:stop(),
                   ?SLEEP(1000), % Give it time to settle
                   ok
           end,
    ?TC_TRY(usm_sha512_priv_aes, Pre, Case, Post).


select_auth_proto(md5)    -> usmHMACMD5AuthProtocol;
select_auth_proto(sha)    -> usmHMACSHAAuthProtocol;
select_auth_proto(sha224) -> usmHMAC128SHA224AuthProtocol;
select_auth_proto(sha256) -> usmHMAC192SHA256AuthProtocol;
select_auth_proto(sha384) -> usmHMAC256SHA384AuthProtocol;
select_auth_proto(sha512) -> usmHMAC384SHA512AuthProtocol.

do_usm_priv_aes(AuthAlg, Config) ->
    ?IPRINT("starting with Config: "
            "~n   AuthAlg: ~p"
            "~n   Config:  ~p", [AuthAlg, Config]),

    put(sname,     "TC[usm-priv-aes]"),
    put(verbosity, trace),

    ?IPRINT("generate AES-encrypted message"),

    EngineID = [128,0,0,0,6],
    SecName  = "v3_user",
    AuthPass = "authpass",
    AuthKey  =
      snmp:passwd2localized_key(AuthAlg, AuthPass, EngineID),
    PrivPass = "privpass",
    PrivKey  =
      snmp:passwd2localized_key(md5, PrivPass, EngineID),

    Credentials =
      [ {auth,     select_auth_proto(AuthAlg)},
        {auth_key, AuthKey},
        {priv,     usmAesCfb128Protocol},
        {priv_key, PrivKey}
      ],

    AgentConfig =
      [ {engine_id, EngineID},
        {address,   {192,0,2,1}},
        {version,   v3},
        {sec_model, usm},
        {sec_level, authPriv},
        {sec_name,  SecName}
      ],

    ?IPRINT("register user, usm-user and agent"),
    snmpm:register_user(SecName, snmpm_user_default, nil),
    snmpm:register_usm_user(EngineID, SecName, Credentials),
    snmpm:register_agent(SecName, "v3_agent", AgentConfig),

    PduType   = 'get-request',
    ScopedPDU =
      { scopedPdu,
        "",        % CtxEngineID
        "",        % Context
        { pdu,
          PduType,
          0,       % RequestID
          noError, % ErrorStatus
          0,       % ErrorIndex
          [ {varbind, [1,3,6,1,2,1,1,5,0], 'OCTET STRING', [], 0}
          ]
        }
      },

    MsgSecurityParameters =
      { usmSecurityParameters,
        _MsgAuthoritativeEngineID    = EngineID,
        _MsgAuthoritativeEngineBoots = 1,
        _MsgAuthoritativeEngineTime  = 0,
        _MsgUserName                 = SecName,
        _MsgAuthenticationParameters = AuthKey,
        _MsgPrivacyParameters        = PrivKey
      },

    ?IPRINT("get engine mms"),
    {ok, MsgMaxSize} =
      snmpm_config:get_engine_max_message_size(),

    ?IPRINT("encode scoped pdu"),
    Message =
      { message,
        'version-3',
        { v3_hdr,
          1, % MsgID1 
          MsgMaxSize,
          snmp_misc:mk_msg_flags(PduType, 2), % MsgFlags1
          _MsgSecurityModel = 3,  % SEC_USM
          MsgSecurityParameters,
          0
        },
        Data = snmp_pdus:enc_scoped_pdu(ScopedPDU)
      },

    {_, CredVals} = lists:unzip(Credentials),

    SecLevel = 2,

    ?IPRINT("generate outgoing message"),
    Msg =
      snmpm_usm:generate_outgoing_msg(
        Message,
        EngineID,
        SecName,
        list_to_tuple([SecName|CredVals]),
        SecLevel
      ),

    ?IPRINT("got AES-encrypted message, now decrypt: "
            "~n   ~p", [Msg]),

    {message, _Version, Hdr, NextData} =
      snmp_pdus:dec_message_only(Msg),

    ?IPRINT("AES-encrypted message decrypted - now match"),

    { v3_hdr,
      _MsgID,
      _MsgMaxSize,
      _MsgFlags,
      _SecModel,
      SecParams,
      _Hdr_size
    } = Hdr,

    ?IPRINT("process incoming message"),

    { ok,
      { _MsgAuthEngineID,
        _SecName,
        ScopedPDUBytes,
        _CachedSecData
      }
    } =
      snmpm_usm:process_incoming_msg(
        Msg,
        NextData,
        SecParams,
        SecLevel
      ),

    Data = ScopedPDUBytes,

    ?IPRINT("message decrypted"),
    ok.


%%======================================================================

register_user1(Config) when is_list(Config) ->
    Pre  = fun() ->
                   ManagerNode = start_node(register_user1),
                   [ManagerNode]
           end,
    Case = fun(State) -> do_register_user1(State, Config) end,
    Post = fun([{ManagerPeer, _ManagerNode}]) -> peer:stop(ManagerPeer) end,
    ?TC_TRY(register_user1, Pre, Case, Post).

do_register_user1([{_ManagerPeer, ManagerNode}], Config) ->
    ?IPRINT("starting with Config: "
            "~n   ~p"
            "~n", [Config]),

    SCO     = ?config(socket_create_opts, Config),
    ConfDir = ?config(manager_conf_dir,   Config),
    DbDir   = ?config(manager_db_dir,     Config),

    write_manager_conf(ConfDir),

    Opts = [{server,     [{verbosity, trace}]},
	    {net_if,     [{verbosity, trace}, {options, SCO}]},
	    {note_store, [{verbosity, trace}]},
	    {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],


    ?IPRINT("load snmp application"),
    ok = load_snmp(ManagerNode),

    ?IPRINT("set manager env for the snmp application"),
    ok = set_mgr_env(ManagerNode, Opts),

    ?IPRINT("starting snmp application (with only manager)"),
    ok = start_snmp(ManagerNode),

    ?IPRINT("started"),

    ?SLEEP(1000),

    ?IPRINT("manager info: ~p~n", [mgr_info(ManagerNode)]),

    ?IPRINT("try register user(s)"),
    ok = mgr_register_user(ManagerNode, calvin, snmpm_user_default,
				 [self(), "various misc info"]),

    Users1 = mgr_which_users(ManagerNode),
    ?IPRINT("users: ~p~n", [Users1]),
    ok = verify_users(Users1, [calvin]),
    ?IPRINT("manager info: ~p~n", [mgr_info(ManagerNode)]),

    ok = mgr_register_user(ManagerNode, hobbe, snmpm_user_default,
				 {"misc info", self()}),

    Users2 = mgr_which_users(ManagerNode),
    ?IPRINT("users: ~p~n", [Users2]),
    ok = verify_users(Users2, [calvin, hobbe]),
    ?IPRINT("manager info: ~p~n", [mgr_info(ManagerNode)]),

    ?IPRINT("try unregister user(s)"),
    ok = mgr_unregister_user(ManagerNode, calvin),

    Users3 = mgr_which_users(ManagerNode),
    ?IPRINT("users: ~p~n", [Users3]),
    ok = verify_users(Users3, [hobbe]),
    ?IPRINT("manager info: ~p~n", [mgr_info(ManagerNode)]),

    ok = mgr_unregister_user(ManagerNode, hobbe),

    Users4 = mgr_which_users(ManagerNode),
    ?IPRINT("users: ~p~n", [Users4]),
    ok = verify_users(Users4, []),
    ?IPRINT("manager info: ~p~n", [mgr_info(ManagerNode)]),

    ?SLEEP(1000),

    ?IPRINT("stop snmp application (with only manager)"),
    ok = stop_snmp(ManagerNode),

    ?SLEEP(1000),

    ok.

verify_users([], []) ->
    ok;
verify_users(ActualUsers, []) ->
    {error, {unexpected_users, ActualUsers}};
verify_users(ActualUsers0, [User|RegUsers]) ->
    case lists:delete(User, ActualUsers0) of
	ActualUsers0 ->
	    {error, {not_registered, User}};
	ActualUsers ->
	    verify_users(ActualUsers, RegUsers)
    end.
    

%%======================================================================

register_agent_old() ->
    [{doc, "Test registration of agents with the OLD interface functions"}].

register_agent_old(Config) when is_list(Config) ->
    Pre  = fun() ->
                   ManagerNode = start_node(register_agent_old),
                   [ManagerNode]
           end,
    Case = fun(State) -> do_register_agent_old(State, Config) end,
    Post = fun([{ManagerPeer, _ManagerNode}]) -> peer:stop(ManagerPeer) end,
    ?TC_TRY(register_agent_old, Pre, Case, Post).

do_register_agent_old([{_ManagerPeer, ManagerNode}], Config) ->
    ?IPRINT("starting with Config: "
            "~n      ~p"
            "~n", [Config]),

    SCO     = ?config(socket_create_opts, Config),
    ConfDir = ?config(manager_conf_dir,   Config),
    DbDir   = ?config(manager_db_dir,     Config),

    write_manager_conf(ConfDir),

    Opts = [{server,     [{verbosity, trace}]},
	    {net_if,     [{verbosity, trace}, {options, SCO}]},
	    {note_store, [{verbosity, trace}]},
	    {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],


    ?IPRINT("load snmp application"),
    ok = load_snmp(ManagerNode),

    ?IPRINT("set manager env for the snmp application"),
    ok = set_mgr_env(ManagerNode, Opts),

    ?IPRINT("starting snmp application (with only manager)"),
    ok = start_snmp(ManagerNode),

    ?IPRINT("started"),

    ?SLEEP(1000),

    ?IPRINT("manager info: ~p~n", [mgr_info(ManagerNode)]),

    ?IPRINT("register user(s) user_alfa & user_beta"),
    ok = mgr_register_user(ManagerNode, user_alfa, snmpm_user_default, []),
    ok = mgr_register_user(ManagerNode, user_beta, snmpm_user_default, []),
    ?IPRINT("manager info: ~p~n", [mgr_info(ManagerNode)]),

    ?IPRINT("register agent(s)"),
    ok = mgr_register_agent(ManagerNode, user_alfa, 5000, []),
    ok = mgr_register_agent(ManagerNode, user_alfa, 5001, []),
    ok = mgr_register_agent(ManagerNode, user_beta, 5002, []),
    ok = mgr_register_agent(ManagerNode, user_beta, 5003, []),

    ?IPRINT("verify all agent(s): expect 4"),
    case mgr_which_agents(ManagerNode) of
	Agents1 when length(Agents1) =:= 4 ->
	    ?IPRINT("all agents: ~p~n", [Agents1]),
	    ok;
	Agents1 ->
	    ?FAIL({agent_registration_failure, Agents1})
    end,

    ?IPRINT("verify user_alfa agent(s)"),
    case mgr_which_agents(ManagerNode, user_alfa) of
	Agents2 when length(Agents2) =:= 2 ->
	    ?IPRINT("calvin agents: ~p", [Agents2]),
	    ok;
	Agents2 ->
	    ?FAIL({agent_registration_failure, Agents2})
    end,

    ?IPRINT("verify user_beta agent(s)"),
    case mgr_which_agents(ManagerNode, user_beta) of
	Agents3 when length(Agents3) =:= 2 ->
	    ?IPRINT("hobbe agents: ~p", [Agents3]),
	    ok;
	Agents3 ->
	    ?FAIL({agent_registration_failure, Agents3})
    end,

    ?IPRINT("manager info: "
            "~n      ~p", [mgr_info(ManagerNode)]),
    
    ?IPRINT("unregister user user_alfa"),
    ok = mgr_unregister_user(ManagerNode, user_alfa),

    ?IPRINT("verify all agent(s): expect 2"),
    case mgr_which_agents(ManagerNode) of
	Agents4 when length(Agents4) =:= 2 ->
	    ?IPRINT("all agents: ~p", [Agents4]),
	    ok;
	Agents4 ->
	    ?FAIL({agent_unregistration_failure, Agents4})
    end,
    ?IPRINT("manager info: "
            "~n      ~p~n", [mgr_info(ManagerNode)]),

    ?IPRINT("unregister user_beta agents"),
    ok = mgr_unregister_agent(ManagerNode, user_beta, 5002),
    ok = mgr_unregister_agent(ManagerNode, user_beta, 5003),

    ?IPRINT("verify all agent(s): expect 0"),
    case mgr_which_agents(ManagerNode) of
	[] ->
	    ok;
	Agents5 ->
	    ?IPRINT("all agents: ~p~n", [Agents5]),
	    ?FAIL({agent_unregistration_failure, Agents5})
    end,

    ?IPRINT("manager info: "
            "~n      ~p", [mgr_info(ManagerNode)]),

    ?IPRINT("unregister user hobbe"),
    ok = mgr_unregister_user(ManagerNode, user_beta),

    ?IPRINT("manager info: "
            "~n      ~p", [mgr_info(ManagerNode)]),

    ?SLEEP(1000),

    ?IPRINT("stop snmp application (with only manager)"),
    ok = stop_snmp(ManagerNode),

    ?SLEEP(1000),
    ?IPRINT("end"),

    ok.


%%======================================================================

register_agent2() ->
    [{doc, "Test registration of agents with the NEW interface functions"}].

register_agent2(Config) when is_list(Config) ->
    Pre  = fun() ->
                   ManagerNode = start_node(register_agent2),
                   [ManagerNode]
           end,
    Case = fun(State) -> do_register_agent2(State, Config) end,
    Post = fun([{ManagerPeer, _ManagerNode}]) -> peer:stop(ManagerPeer) end,
    ?TC_TRY(register_agent2, Pre, Case, Post).

do_register_agent2([{_ManagerPeer, ManagerNode}], Config) ->
    ?IPRINT("starting with Config: "
            "~n      ~p", [Config]),

    SCO       = ?config(socket_create_opts, Config),
    ConfDir   = ?config(manager_conf_dir,   Config),
    DbDir     = ?config(manager_db_dir,     Config),
    LocalHost = snmp_test_lib:localhost(), 

    write_manager_conf(ConfDir),

    Opts = [{server,     [{verbosity, trace}]},
	    {net_if,     [{verbosity, trace}, {options, SCO}]},
	    {note_store, [{verbosity, trace}]},
	    {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],

    ?IPRINT("load snmp application"),
    ok = load_snmp(ManagerNode),

    ?IPRINT("set manager env for the snmp application"),
    ok = set_mgr_env(ManagerNode, Opts),

    ?IPRINT("starting snmp application (with only manager)"),
    ok = start_snmp(ManagerNode),

    ?IPRINT("started"),

    ?SLEEP(1000),

    ?IPRINT("manager info: ~p~n", [mgr_info(ManagerNode)]),

    ?IPRINT("register user(s) user_alfa & user_beta"),
    ok = mgr_register_user(ManagerNode, user_alfa, snmpm_user_default, []),
    ok = mgr_register_user(ManagerNode, user_beta, snmpm_user_default, []),
    ?IPRINT("manager info: ~p~n", [mgr_info(ManagerNode)]),

    ?IPRINT("register agent(s)"),
    TargetName1 = "agent1", 
    ok = mgr_register_agent(ManagerNode, user_alfa, TargetName1,
				  [{address,   LocalHost},
				   {port,      5001},
				   {engine_id, "agentEngineId-1"}]),
    TargetName2 = "agent2", 
    ok = mgr_register_agent(ManagerNode, user_alfa, TargetName2,
				  [{address,   LocalHost},
				   {port,      5002},
				   {engine_id, "agentEngineId-2"}]),
    TargetName3 = "agent3", 
    ok = mgr_register_agent(ManagerNode, user_beta, TargetName3,
				  [{address,   LocalHost},
				   {port,      5003},
				   {engine_id, "agentEngineId-3"}]),
    TargetName4 = "agent4", 
    ok = mgr_register_agent(ManagerNode, user_beta, TargetName4,
				  [{address,   LocalHost},
				   {port,      5004},
				   {engine_id, "agentEngineId-4"}]),

    ?IPRINT("verify all agent(s): expect 4"),
    case mgr_which_agents(ManagerNode) of
	Agents1 when length(Agents1) =:= 4 ->
	    ?IPRINT("all agents: ~p~n", [Agents1]),
	    ok;
	Agents1 ->
	    ?FAIL({agent_registration_failure, Agents1})
    end,

    ?IPRINT("verify user_alfa agent(s)"),
    case mgr_which_agents(ManagerNode, user_alfa) of
	Agents2 when length(Agents2) =:= 2 ->
	    ?IPRINT("calvin agents: ~p~n", [Agents2]),
	    ok;
	Agents2 ->
	    ?FAIL({agent_registration_failure, Agents2})
    end,

    ?IPRINT("verify user_beta agent(s)"),
    case mgr_which_agents(ManagerNode, user_beta) of
	Agents3 when length(Agents3) =:= 2 ->
	    ?IPRINT("hobbe agents: ~p~n", [Agents3]),
	    ok;
	Agents3 ->
	    ?FAIL({agent_registration_failure, Agents3})
    end,

    ?IPRINT("manager info: ~p~n", [mgr_info(ManagerNode)]),

    ?IPRINT("unregister user user_alfa"),
    ok = mgr_unregister_user(ManagerNode, user_alfa),

    ?IPRINT("verify all agent(s): expect 2"),
    case mgr_which_agents(ManagerNode) of
	Agents4 when length(Agents4) =:= 2 ->
	    ?IPRINT("all agents: ~p~n", [Agents4]),
	    ok;
	Agents4 ->
	    ?FAIL({agent_unregistration_failure, Agents4})
    end,
    ?IPRINT("manager info: ~p~n", [mgr_info(ManagerNode)]),

    ?IPRINT("unregister user_beta agents"),
    ok = mgr_unregister_agent(ManagerNode, user_beta, TargetName3),
    ok = mgr_unregister_agent(ManagerNode, user_beta, TargetName4),

    ?IPRINT("verify all agent(s): expect 0"),
    case mgr_which_agents(ManagerNode) of
	[] ->
	    ok;
	Agents5 ->
	    ?IPRINT("all agents: ~p~n", [Agents5]),
	    ?FAIL({agent_unregistration_failure, Agents5})
    end,

    ?IPRINT("manager info: ~p~n", [mgr_info(ManagerNode)]),

    ?IPRINT("unregister user user_beta"),
    ok = mgr_unregister_user(ManagerNode, user_beta),

    ?IPRINT("manager info: ~p~n", [mgr_info(ManagerNode)]),

    ?SLEEP(1000),

    ?IPRINT("stop snmp application (with only manager)"),
    ok = stop_snmp(ManagerNode),

    ?SLEEP(1000),

    ok.


%%======================================================================

register_agent3() ->
    [{doc, "Test registration of agents with the NEW interface functions "
     "and specifying transport domain"}].

register_agent3(Config) when is_list(Config) ->
    Pre  = fun() ->
                   ManagerNode = start_node(register_agent3),
                   [ManagerNode]
           end,
    Case = fun(State) -> do_register_agent3(State, Config) end,
    Post = fun([{ManagerPeer, _ManagerNode}]) -> peer:stop(ManagerPeer) end,
    ?TC_TRY(register_agent3, Pre, Case, Post).

do_register_agent3([{_ManagerPeer, ManagerNode}], Config) ->
    ?IPRINT("starting with Config: "
            "~n      ~p", [Config]),

    SCO       = ?config(socket_create_opts, Config),
    ConfDir   = ?config(manager_conf_dir,   Config),
    DbDir     = ?config(manager_db_dir,     Config),
    LocalHost = snmp_test_lib:localhost(), 


    write_manager_conf(ConfDir),

    Opts = [{server,     [{verbosity, trace}]},
	    {net_if,     [{verbosity, trace}, {options, SCO}]},
	    {note_store, [{verbosity, trace}]},
	    {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],


    ?IPRINT("load snmp application"),
    ok = load_snmp(ManagerNode),

    ?IPRINT("set manager env for the snmp application"),
    ok = set_mgr_env(ManagerNode, Opts),

    ?IPRINT("starting snmp application (with only manager)"),
    ok = start_snmp(ManagerNode),

    ?IPRINT("started"),

    ?SLEEP(1000),

    ?IPRINT("manager info: ~p~n", [mgr_info(ManagerNode)]),

    ?IPRINT("register user(s) user_alfa & user_beta"),
    ok = mgr_register_user(ManagerNode, user_alfa, snmpm_user_default, []),
    ok = mgr_register_user(ManagerNode, user_beta, snmpm_user_default, []),
    ?IPRINT("manager info: ~p~n", [mgr_info(ManagerNode)]),

    ?IPRINT("register agent(s)"),
    TargetName1 = "agent2", 
    ok = mgr_register_agent(ManagerNode, user_alfa, TargetName1,
				  [{tdomain,   transportDomainUdpIpv4},
				   {address,   LocalHost},
				   {port,      5001},
				   {engine_id, "agentEngineId-1"}]),
    TargetName2 = "agent3", 
    ok = mgr_register_agent(ManagerNode, user_alfa, TargetName2,
				  [{tdomain,   transportDomainUdpIpv6},
				   {address,   {0,0,0,0,0,0,0,1}},
				   {port,      5002},
				   {engine_id, "agentEngineId-2"}]),
    TargetName3 = "agent4", 
    {error, {unsupported_domain, _} = Reason4} =
	mgr_register_agent(ManagerNode, user_beta, TargetName3,
			   [{tdomain,   transportDomainTcpIpv4},
			    {address,   LocalHost},
			    {port,      5003},
			    {engine_id, "agentEngineId-3"}]),
    ?IPRINT("Expected registration failure: ~p", [Reason4]),
    TargetName4 = "agent5", 
    {error, {unknown_domain, _} = Reason5} =
	mgr_register_agent(ManagerNode, user_beta, TargetName4,
			   [{tdomain,   transportDomainUdpIpv4_bad},
			    {address,   LocalHost},
			    {port,      5004},
			    {engine_id, "agentEngineId-4"}]),
    ?IPRINT("Expected registration failure: ~p", [Reason5]),

    ?IPRINT("verify all agent(s): expect 2"),
    case mgr_which_agents(ManagerNode) of
	Agents1 when length(Agents1) =:= 2 ->
	    ?IPRINT("all agents: ~p~n", [Agents1]),
	    ok;
	Agents1 ->
	    ?FAIL({agent_registration_failure, Agents1})
    end,

    ?IPRINT("verify user_alfa agent(s)"),
    case mgr_which_agents(ManagerNode, user_alfa) of
	Agents2 when length(Agents2) =:= 2 ->
	    ?IPRINT("calvin agents: ~p~n", [Agents2]),
	    ok;
	Agents2 ->
	    ?FAIL({agent_registration_failure, Agents2})
    end,

    ?IPRINT("verify user_beta agent(s)"),
    case mgr_which_agents(ManagerNode, user_beta) of
	Agents3 when length(Agents3) =:= 0 ->
	    ?IPRINT("hobbe agents: ~p~n", [Agents3]),
	    ok;
	Agents3 ->
	    ?FAIL({agent_registration_failure, Agents3})
    end,

    ?IPRINT("manager info: ~p~n", [mgr_info(ManagerNode)]),

    ?IPRINT("unregister user user_alfa"),
    ok = mgr_unregister_user(ManagerNode, user_alfa),

    ?IPRINT("verify all agent(s): expect 0"),
    case mgr_which_agents(ManagerNode) of
	Agents4 when length(Agents4) =:= 0 ->
	    ?IPRINT("all agents: ~p~n", [Agents4]),
	    ok;
	Agents4 ->
	    ?FAIL({agent_unregistration_failure, Agents4})
    end,
    ?IPRINT("manager info: ~p~n", [mgr_info(ManagerNode)]),

    ?IPRINT("verify all agent(s): expect 0"),
    case mgr_which_agents(ManagerNode) of
	[] ->
	    ok;
	Agents5 ->
	    ?EPRINT("all agents: ~p~n", [Agents5]),
	    ?FAIL({agent_unregistration_failure, Agents5})
    end,

    ?IPRINT("manager info: ~p~n", [mgr_info(ManagerNode)]),

    ?IPRINT("unregister user user_beta"),
    ok = mgr_unregister_user(ManagerNode, user_beta),

    ?IPRINT("manager info: ~p~n", [mgr_info(ManagerNode)]),

    ?SLEEP(1000),

    ?IPRINT("stop snmp application (with only manager)"),
    ok = stop_snmp(ManagerNode),

    ?SLEEP(1000),

    ok.


%%======================================================================

simple_sync_get3() ->
    [{doc, "Simple sync get-request - Version 3 API (TargetName and send-opts)"}].

simple_sync_get3(Config) when is_list(Config) ->
    ?TC_TRY(simple_sync_get3,
            fun() -> do_simple_sync_get3(Config) end).

do_simple_sync_get3(Config) ->
    ?IPRINT("starting with Config: "
            "~n      ~p", [Config]),
    Self  = self(), 
    Msg   = simple_sync_get3, 
    Fun   = fun() -> Self ! Msg end,
    Extra = {?SNMPM_EXTRA_INFO_TAG, Fun}, 
    SendOpts = 
	[
	 {extra, Extra}
	], 
    Get = fun(Node, TargetName, Oids) -> 
		  mgr_user_sync_get2(Node, TargetName, Oids, SendOpts) 
	  end,
    PostVerify = 
	fun() ->
		receive
		    Msg ->
			ok
		end
	end,
    Res = do_simple_sync_get3(Config, Get, PostVerify),
    display_log(Config),
    Res.


do_simple_sync_get3(Config, Get, PostVerify) ->
    ?IPRINT("starting with Config: "
            "~n      ~p", [Config]),

    Node       = ?config(manager_node, Config),
    TargetName = ?config(manager_agent_target_name, Config),

    ?IPRINT("issue get-request without loading the mib"),
    Oids1 = [?sysObjectID_instance, ?sysDescr_instance, ?sysUpTime_instance],
    ok = do_simple_sync_get3(Node, TargetName, Oids1, Get, PostVerify),

    ?IPRINT("issue get-request after first loading the mibs"),
    ok = mgr_user_load_mib(Node, std_mib()),
    Oids2 = [[sysObjectID, 0], [sysDescr, 0], [sysUpTime, 0]],
    ok = do_simple_sync_get3(Node, TargetName, Oids2, Get, PostVerify),
    ok.

do_simple_sync_get3(Node, TargetName, Oids, Get, PostVerify) 
  when is_function(Get, 3) andalso is_function(PostVerify, 0) ->

    ?IPRINT("try get for ~p (on ~p):"
            "~n      Oids: ~p", [TargetName, Node, Oids]),
    Reply =
        case Get(Node, TargetName, Oids) of
            {ok, R, _Rem} ->
                ?IPRINT("get reply: "
                        "~n       Reply: ~p"
                        "~n       Rem:   ~w", [R, _Rem]),

                R;
            {error, Reason} = ERROR ->
                ?EPRINT("get failed: "
                        "~n      ~p", [Reason]),
                ERROR
        end,

    %% verify that the operation actually worked:
    %% The order should be the same, so no need to search
    ok = case Reply of
		   {noError, 0, [#varbind{oid   = ?sysObjectID_instance,
					  value = SysObjectID}, 
				 #varbind{oid   = ?sysDescr_instance,
					  value = SysDescr},
				 #varbind{oid   = ?sysUpTime_instance,
					  value = SysUpTime}]} ->
		       ?IPRINT("expected result from get: "
                               "~n   SysObjectID: ~p"
                               "~n   SysDescr:    ~s"
                               "~n   SysUpTime:   ~w", 
                               [SysObjectID, SysDescr, SysUpTime]),
		       PostVerify();
		   {noError, 0, Vbs} ->
		       ?EPRINT("unexpected varbinds: "
                               "~n      ~p", [Vbs]),
		       {error, {unexpected_vbs, Vbs}};
		   Else ->
		       ?EPRINT("unexpected reply: "
                               "~n      ~p", [Else]),
		       {error, {unexpected_response, Else}}
	       end,
    ok.


%%======================================================================

sag_verify({noError, 0, _Vbs}, any) ->
    ?IPRINT("verified [any]"),
    ok;
sag_verify({noError, 0, Vbs}, Exp) ->
    ?DBG("verified first stage ok: "
	 "~n   Vbs: ~p"
	 "~n   Exp: ~p", [Vbs, Exp]),
    sag_verify_vbs(Vbs, Exp);
sag_verify(Error, _) ->
    {error, {unexpected_response, Error}}.

sag_verify_vbs([], []) ->
    ?DBG("verified second stage ok", []),
    ok;
sag_verify_vbs(Vbs, []) ->
    {error, {unexpected_vbs, Vbs}};
sag_verify_vbs([], Exp) ->
    {error, {expected_vbs, Exp}};
sag_verify_vbs([#varbind{oid = Oid}|Vbs], [any|Exp]) ->
    ?IPRINT("verified [any] oid ~w", [Oid]),
    sag_verify_vbs(Vbs, Exp);
sag_verify_vbs([#varbind{oid = Oid, value = Value}|Vbs], [Oid|Exp]) ->
    ?IPRINT("verified oid ~w [~p]", [Oid, Value]),
    sag_verify_vbs(Vbs, Exp);
sag_verify_vbs([#varbind{oid = Oid, value = Value}|Vbs], [{Oid,Value}|Exp]) ->
    ?IPRINT("verified oid ~w and ~p", [Oid, Value]),
    sag_verify_vbs(Vbs, Exp);
sag_verify_vbs([Vb|_], [E|_]) ->
    {error, {unexpected_vb, Vb, E}}.


%%======================================================================

simple_async_get3() ->
    [{doc, "Simple (async) get-request - Version 3 API (TargetName and send-opts)"}].

simple_async_get3(Config) when is_list(Config) ->
    ?TC_TRY(simple_async_get3,
            fun() -> do_simple_async_get3(Config) end).

do_simple_async_get3(Config) ->
    ?IPRINT("starting with Config: "
            "~n      ~p", [Config]),
    MgrNode    = ?config(manager_node, Config),
    AgentNode  = ?config(agent_node, Config),
    TargetName = ?config(manager_agent_target_name, Config),
    Self  = self(), 
    Msg   = simple_async_get3, 
    Fun   = fun() -> Self ! Msg end,
    Extra = {?SNMPM_EXTRA_INFO_TAG, Fun}, 
    SendOpts = 
	[
	 {extra, Extra}
	], 
    Get = fun(Oids) -> async_g_exec3(MgrNode, TargetName, Oids, SendOpts) end,
    PostVerify = fun(ok)    -> receive Msg -> ok end;
		    (Error) -> Error 
		 end,
    Res = do_simple_async_sync_get3(Config, MgrNode, AgentNode,
                                    Get, PostVerify),
    display_log(Config),
    Res.

do_simple_async_sync_get3(Config, MgrNode, AgentNode, Get, PostVerify) ->
    ok = mgr_user_load_mib(MgrNode, std_mib()),
    Test2Mib = test2_mib(Config), 
    ok = mgr_user_load_mib(MgrNode, Test2Mib),
    ok = agent_load_mib(AgentNode, Test2Mib),
    do_simple_async_sync_get3(fun() -> mgr_info(MgrNode) end,
			      fun() -> agent_info(AgentNode) end,
			      Get, PostVerify).

do_simple_async_sync_get3(MgrInfo, AgentInfo, Get, PostVerify) 
  when is_function(MgrInfo, 0) andalso 
       is_function(AgentInfo, 0) andalso 
       is_function(Get, 1) andalso 
       is_function(PostVerify, 1) ->
    Requests = 
	[
	 { 1,  
	   [?sysObjectID_instance], 
	   Get, 
	   fun(X) -> 
		   PostVerify(sag_verify(X, [?sysObjectID_instance])) end}, 
	 { 2,  
	   [?sysDescr_instance, ?sysUpTime_instance],
	   Get, 
	   fun(X) -> 
		   PostVerify(sag_verify(X, [?sysObjectID_instance, 
					     ?sysUpTime_instance]))
	   end}, 
	 { 3,  
	   [[sysObjectID, 0], [sysDescr, 0], [sysUpTime, 0]],
	   Get, 
	   fun(X) -> 
		   PostVerify(sag_verify(X, [?sysObjectID_instance, 
					     ?sysDescr_instance, 
					     ?sysUpTime_instance]))
		       
	   end}, 
	 { 4,  
	   [?sysObjectID_instance, 
	    ?sysDescr_instance, 
	    ?sysUpTime_instance],
	   Get, 
	   fun(X) -> 
		   PostVerify(sag_verify(X, [?sysObjectID_instance, 
					     ?sysDescr_instance, 
					     ?sysUpTime_instance]))
	   end}
	],
    
    ?IPRINT("manager info when starting test: "
            "~n      ~p", [MgrInfo()]),
    ?IPRINT("agent info when starting test: "
            "~n      ~p",   [AgentInfo()]),

    ok = async_exec(Requests, []),

    ?IPRINT("manager info when ending test: "
            "~n      ~p", [MgrInfo()]),
    ?IPRINT("agent info when ending test: "
            "~n      ~p",   [AgentInfo()]),

    ok.

async_g_exec3(Node, TargetName, Oids, SendOpts) ->
    mgr_user_async_get2(Node, TargetName, Oids, SendOpts).


%%======================================================================

verify_ssgn_reply1({noError, 0, _Vbs}, any) ->
    ok;
verify_ssgn_reply1({noError, 0, Vbs}, Expected) ->
    check_ssgn_vbs(Vbs, Expected);
verify_ssgn_reply1(R, _) ->
    {error, {unexpected_reply, R}}.

verify_ssgn_reply2({ErrStatus, ErrIdx, _Vbs}, {ErrStatus, ErrIdx, any}) ->
    ok;
verify_ssgn_reply2({ErrStatus, ErrIdx, Vbs}, {ErrStatus, ErrIdx, Expected}) ->
    check_ssgn_vbs(Vbs, Expected);
verify_ssgn_reply2(R, _) ->
    {error, {unexpected_reply, R}}.

check_ssgn_vbs([], []) ->
    ok;
check_ssgn_vbs(Unexpected, []) ->
    {error, {unexpected_vbs, Unexpected}};
check_ssgn_vbs([], Expected) ->
    {error, {expected_vbs, Expected}};
check_ssgn_vbs([#varbind{value = endOfMibView}|R], 
	       [endOfMibView|Expected]) ->
    check_ssgn_vbs(R, Expected);
check_ssgn_vbs([#varbind{oid = Oid}|R], [Oid|Expected]) ->
    check_ssgn_vbs(R, Expected);
check_ssgn_vbs([#varbind{oid = Oid, value = Value}|R], 
	       [{Oid, Value}|Expected]) ->
    check_ssgn_vbs(R, Expected);
check_ssgn_vbs([Vb|_], [E|_]) ->
    {error, {unexpected_vb, Vb, E}}.


%%======================================================================

simple_sync_get_next3() ->
    [{doc, "Simple (sync) get_next-request - "
     "Version 3 API (TargetName with send-opts)"}].

simple_sync_get_next3(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    put(tname, ssgn3),
    ?IPRINT("starting with Config: "
            "~n      ~p", [Config]),
    Self  = self(), 
    Msg   = simple_sync_get_next3, 
    Fun   = fun() -> Self ! Msg end,
    Extra = {?SNMPM_EXTRA_INFO_TAG, Fun}, 
    SendOpts = 
	[
	 {extra, Extra}
	], 
    GetNext = fun(Node, TargetName, Oids) -> 
		      mgr_user_sync_get_next2(Node, TargetName, Oids, SendOpts) 
	      end,
    PostVerify = fun(ok)    -> receive Msg -> ok end;
		    (Error) -> Error 
		 end,
    do_simple_sync_get_next3(Config, GetNext, PostVerify),
    display_log(Config),
    ok.


do_simple_sync_get_next3(Config, GetNext, PostVerify) 
  when is_function(GetNext, 3) andalso is_function(PostVerify, 1) ->

    MgrNode    = ?config(manager_node, Config),
    AgentNode  = ?config(agent_node, Config),
    TargetName = ?config(manager_agent_target_name, Config),

    %% -- 1 --
    Oids01 = [[1,3,7,1]],
    VF01   = fun(X) -> verify_ssgn_reply1(X, [{[1,3,7,1],endOfMibView}]) end,
    ok = do_simple_get_next(1,
				  MgrNode, TargetName, Oids01, VF01, 
				  GetNext, PostVerify),
    
    ok = mgr_user_load_mib(MgrNode, std_mib()),

    %% -- 2 --
    Oids02 = [[sysDescr], [1,3,7,1]], 
    VF02   = fun(X) -> 
		     verify_ssgn_reply1(X, [?sysDescr_instance, endOfMibView]) 
	     end,
    ok = do_simple_get_next(2,
				  MgrNode, TargetName, Oids02, VF02, 
				  GetNext, PostVerify),
    
    Test2Mib = test2_mib(Config), 
    ok = mgr_user_load_mib(MgrNode, Test2Mib),
    ok = agent_load_mib(AgentNode, Test2Mib),

    %% -- 3 --
    {ok, [TCnt2|_]} = mgr_user_name_to_oid(MgrNode, tCnt2),
    Oids03 = [[TCnt2, 1]], 
    VF03   = fun(X) -> 
		     verify_ssgn_reply1(X, [{fl([TCnt2,2]), 100}]) 
	     end,
    ok = do_simple_get_next(3,
				  MgrNode, TargetName, Oids03, VF03, 
				  GetNext, PostVerify),
    
    %% -- 4 --
    Oids04 = [[TCnt2, 2]], 
    VF04   = fun(X) -> 
		     verify_ssgn_reply1(X, [{fl([TCnt2,2]), endOfMibView}]) 
	     end,
    ok = do_simple_get_next(4,
				  MgrNode, TargetName, Oids04, VF04, 
				  GetNext, PostVerify),
    
    %% -- 5 --
    {ok, [TGenErr1|_]} = mgr_user_name_to_oid(MgrNode, tGenErr1),
    Oids05 = [TGenErr1], 
    VF05   = fun(X) -> 
		     verify_ssgn_reply2(X, {genErr, 1, [TGenErr1]}) 
	     end,
    ok = do_simple_get_next(5,
				  MgrNode, TargetName, Oids05, VF05, 
				  GetNext, PostVerify),
    
    %% -- 6 --
    {ok, [TGenErr2|_]} = mgr_user_name_to_oid(MgrNode, tGenErr2),
    Oids06 = [TGenErr2], 
    VF06   = fun(X) -> 
		     verify_ssgn_reply2(X, {genErr, 1, [TGenErr2]}) 
	     end,
    ok = do_simple_get_next(6,
				  MgrNode, TargetName, Oids06, VF06, 
				  GetNext, PostVerify),
    
    %% -- 7 --
    {ok, [TGenErr3|_]} = mgr_user_name_to_oid(MgrNode, tGenErr3),
    Oids07 = [[sysDescr], TGenErr3], 
    VF07   = fun(X) -> 
		     verify_ssgn_reply2(X, {genErr, 2, 
					   [?sysDescr, TGenErr3]}) 
	     end,
    ok = do_simple_get_next(7,
				  MgrNode, TargetName, Oids07, VF07, 
				  GetNext, PostVerify),
    
    %% -- 8 --
    {ok, [TTooBig|_]} = mgr_user_name_to_oid(MgrNode, tTooBig),
    Oids08 = [TTooBig], 
    VF08   = fun(X) -> 
		     verify_ssgn_reply2(X, {tooBig, 0, []}) 
	     end,
    ok = do_simple_get_next(8,
				  MgrNode, TargetName, Oids08, VF08, 
				  GetNext, PostVerify),
    ok.

do_simple_get_next(N, Node, TargetName, Oids, Verify, GetNext, PostVerify) ->
    ?IPRINT("issue get-next command ~w", [N]),
    case GetNext(Node, TargetName, Oids) of
	{ok, Reply, _Rem} ->
	    ?DBG("get-next ok:"
		 "~n   Reply: ~p"
		 "~n   Rem:   ~w", [Reply, _Rem]),
	    PostVerify(Verify(Reply));

	Error ->
	    {error, {unexpected_reply, Error}}
    end.



%%======================================================================

simple_async_get_next3_cbp_def() ->
    [{doc, "Simple (async) get_next-request - "
     "Version 3 API (TargetName with send-opts)"}].

simple_async_get_next3_cbp_def(Config) when is_list(Config) ->
    simple_async_get_next3(ssgn2_cbp_def, Config).

simple_async_get_next3_cbp_temp() ->
    [{doc, "Simple (async) get_next-request - "
     "Version 3 API (TargetName with send-opts)"}].

simple_async_get_next3_cbp_temp(Config) when is_list(Config) ->
    simple_async_get_next3(ssgn2_cbp_temp, Config).

simple_async_get_next3_cbp_perm() ->
    [{doc, "Simple (async) get_next-request - "
     "Version 3 API (TargetName with send-opts)"}].

simple_async_get_next3_cbp_perm(Config) when is_list(Config) ->
    simple_async_get_next3(ssgn2_cbp_perm, Config).

simple_async_get_next3(Case, Config) when is_list(Config) ->
    ?TC_TRY(Case,
            fun() -> do_simple_async_get_next3(Config) end).

do_simple_async_get_next3(Config) ->
    %% process_flag(trap_exit, true),
    ?IPRINT("starting with Config: "
            "~n      ~p", [Config]),

    MgrNode    = ?config(manager_node, Config),
    AgentNode  = ?config(agent_node, Config),
    TargetName = ?config(manager_agent_target_name, Config),

    ok = mgr_user_load_mib(MgrNode, std_mib()),
    Test2Mib = test2_mib(Config), 
    ok = mgr_user_load_mib(MgrNode, Test2Mib),
    ok = agent_load_mib(AgentNode, Test2Mib),

    Self  = self(), 
    Msg   = simple_async_get_next3, 
    Fun   = fun() -> Self ! Msg end,
    Extra = {?SNMPM_EXTRA_INFO_TAG, Fun}, 
    SendOpts = 
	[
	 {extra, Extra}
	], 

    GetNext = fun(Oids) ->
		      async_gn_exec3(MgrNode, TargetName, Oids, SendOpts)
	      end,
    PostVerify = fun(ok)    -> receive Msg -> ok end;
		    (Error) -> Error 
		 end,

    Res = do_simple_async_get_next3(MgrNode, AgentNode, GetNext, PostVerify),
    display_log(Config),
    Res.

do_simple_async_get_next3(MgrNode, AgentNode, GetNext, PostVerify) 
  when is_function(GetNext, 1) andalso is_function(PostVerify, 1) ->
    {ok, [TCnt2|_]}    = mgr_user_name_to_oid(MgrNode, tCnt2),
    {ok, [TGenErr1|_]} = mgr_user_name_to_oid(MgrNode, tGenErr1),
    {ok, [TGenErr2|_]} = mgr_user_name_to_oid(MgrNode, tGenErr2),
    {ok, [TGenErr3|_]} = mgr_user_name_to_oid(MgrNode, tGenErr3),
    {ok, [TTooBig|_]}  = mgr_user_name_to_oid(MgrNode, tTooBig),

    Requests = 
	[
	 {1, 
	  [[1,3,7,1]], 
	  GetNext, 
	  fun(X) ->
		  PostVerify(
		    verify_ssgn_reply1(X, [{[1,3,7,1], endOfMibView}])) 
		  
	  end}, 
	 {2, 
	  [[sysDescr], [1,3,7,1]], 
	  GetNext, 
	  fun(X) ->
		  PostVerify(
		    verify_ssgn_reply1(X, [?sysDescr_instance, endOfMibView]))
	  end}, 
	 {3, 
	  [[TCnt2, 1]], 
	  GetNext, 
	  fun(X) ->
		  PostVerify(
		    verify_ssgn_reply1(X, [{fl([TCnt2,2]), 100}]))
	  end}, 
	 {4, 
	  [[TCnt2, 2]], 
	  GetNext, 
	  fun(X) ->
		  PostVerify(
		    verify_ssgn_reply1(X, [{fl([TCnt2,2]), endOfMibView}]))
	  end}, 
	 {5, 
	  [TGenErr1], 
	  GetNext, 
	  fun(X) ->
		  PostVerify(
		    verify_ssgn_reply2(X, {genErr, 1, [TGenErr1]}))
	  end}, 
	 {6, 
	  [TGenErr2], 
	  GetNext, 
	  fun(X) ->
		  PostVerify(
		    verify_ssgn_reply2(X, {genErr, 1, [TGenErr2]}))
	  end}, 
	 {7, 
	  [[sysDescr], TGenErr3], 
	  GetNext, 
	  fun(X) ->
		  PostVerify(
		    verify_ssgn_reply2(X, {genErr, 2, [TGenErr3]}))
	  end}, 
	 {8, 
	  [TTooBig], 
	  GetNext, 
	  fun(X) ->
		  PostVerify(
		    verify_ssgn_reply2(X, {tooBig, 0, []}))
	  end}
	],

    ?IPRINT("manager info when starting test: "
            "~n      ~p", [mgr_info(MgrNode)]),
    ?IPRINT("agent info when starting test: "
            "~n      ~p", [agent_info(AgentNode)]),

    ok = async_exec(Requests, []),

    ?IPRINT("manager info when ending test: "
            "~n      ~p", [mgr_info(MgrNode)]),
    ?IPRINT("agent info when ending test: "
            "~n      ~p", [agent_info(AgentNode)]),

    ok.


async_gn_exec3(Node, TargetName, Oids, SendOpts) ->
    mgr_user_async_get_next2(Node, TargetName, Oids, SendOpts).


%%======================================================================

value_of_vavs(VAVs) ->
    value_of_vavs(VAVs, []).

value_of_vavs([], Acc) ->
    lists:reverse(Acc);
value_of_vavs([{_Oid, _Type, Val}|VAVs], Acc) ->
    value_of_vavs(VAVs, [Val|Acc]);
value_of_vavs([{_Oid, Val}|VAVs], Acc) ->
    value_of_vavs(VAVs, [Val|Acc]).

			       
%%======================================================================

simple_sync_set3() ->
    [{doc, "Simple (sync) set-request - Version 3 API (TargetName with send-opts)"}].

simple_sync_set3(Config) when is_list(Config) ->
    ?TC_TRY(simple_sync_set3,
            fun() -> do_simple_sync_set3(Config) end).

do_simple_sync_set3(Config) ->
    ?IPRINT("starting with Config: "
            "~n      ~p", [Config]),

    Self  = self(), 
    Msg   = simple_sync_set3, 
    Fun   = fun() -> Self ! Msg end,
    Extra = {?SNMPM_EXTRA_INFO_TAG, Fun}, 
    SendOpts = 
	[
	 {extra, Extra}
	], 
    
    Set = fun(Node, TargetName, VAVs) -> 
		  mgr_user_sync_set2(Node, TargetName, VAVs, SendOpts) 
	  end,
    PostVerify = fun() -> receive Msg -> ok end end,

    Res = do_simple_sync_set3(Config, Set, PostVerify),
    display_log(Config),
    Res.

do_simple_sync_set3(Config, Set, PostVerify) 
  when is_function(Set, 3) andalso is_function(PostVerify, 0) ->

    Node       = ?config(manager_node, Config),
    TargetName = ?config(manager_agent_target_name, Config),

    ?IPRINT("issue set-request without loading the mib"),
    Val11 = "Arne Anka",
    Val12 = "Stockholm",
    VAVs1 = [
	     {?sysName_instance,     s, Val11},
	     {?sysLocation_instance, s, Val12}
	    ],
    ok = do_simple_set3(Node, TargetName, VAVs1, Set, PostVerify),

    ?IPRINT("issue set-request after first loading the mibs"),
    ok = mgr_user_load_mib(Node, std_mib()),
    Val21 = "Sune Anka",
    Val22 = "Gothenburg",
    VAVs2 = [
	     {[sysName, 0],     Val21},
	     {[sysLocation, 0], Val22}
	    ],
    ok = do_simple_set3(Node, TargetName, VAVs2, Set, PostVerify),
    ok.

do_simple_set3(Node, TargetName, VAVs, Set, PostVerify) ->
    [SysName, SysLoc] = value_of_vavs(VAVs),
    {ok, Reply, _Rem} = Set(Node, TargetName, VAVs),

    ?DBG("~n   Reply: ~p"
	 "~n   Rem:   ~w", [Reply, _Rem]),

    %% verify that the operation actually worked:
    %% The order should be the same, so no need to search
    %% The value we get should be exactly the same as we sent
    ok = case Reply of
		   {noError, 0, [#varbind{oid   = ?sysName_instance,
					  value = SysName},
				 #varbind{oid   = ?sysLocation_instance,
					  value = SysLoc}]} ->
		       PostVerify();
		   {noError, 0, Vbs} ->
		       {error, {unexpected_vbs, Vbs}};
		   Else ->
		       ?EPRINT("unexpected reply: "
                               "~n      ~p", [Else]),
		       {error, {unexpected_response, Else}}
	       end,
    ok.


%%======================================================================

sas_verify({noError, 0, _Vbs}, any) ->
    ?IPRINT("verified [any]"),
    ok;
sas_verify({noError, 0, Vbs}, Expected) ->
    ?DBG("verified stage 1: "
	 "~n   Vbs: ~p"
	 "~n   Exp: ~p", [Vbs, Expected]),
    sas_verify_vbs(Vbs, Expected);
sas_verify(Error, _) ->
    {error, {unexpected_reply, Error}}.

sas_verify_vbs([], []) ->
    ok;
sas_verify_vbs(Vbs, []) ->
    {error, {unexpected_vbs, Vbs}};
sas_verify_vbs([], Exp) ->
    {error, {expected_vbs, Exp}};
sas_verify_vbs([#varbind{oid = Oid}|Vbs], [any|Exp]) ->
    ?IPRINT("verified [any] oid ~w", [Oid]),
    sas_verify_vbs(Vbs, Exp);
sas_verify_vbs([#varbind{oid = Oid, value = Value}|Vbs], [Oid|Exp]) ->
    ?IPRINT("verified oid ~w [~p]", [Oid, Value]),
    sas_verify_vbs(Vbs, Exp);
sas_verify_vbs([#varbind{oid = Oid, value = Value}|Vbs], [{Oid,Value}|Exp]) ->
    ?IPRINT("verified oid ~w and ~p", [Oid, Value]),
    sas_verify_vbs(Vbs, Exp);
sas_verify_vbs([Vb|_], [E|_]) ->
    {error, {unexpected_vb, Vb, E}}.

    
%%======================================================================

simple_async_set3_cbp_def() ->
    [{doc, "Simple (async) set-request - Version 3 API (TargetName with send-opts)"}].

simple_async_set3_cbp_def(Config) when is_list(Config) ->
    simple_async_set3(sas3_cbp_def, Config).

simple_async_set3_cbp_temp() ->
    [{doc, "Simple (async) set-request - Version 3 API (TargetName with send-opts)"}].

simple_async_set3_cbp_temp(Config) when is_list(Config) ->
    simple_async_set3(sas3_cbp_temp, Config).

simple_async_set3_cbp_perm() ->
    [{doc, "Simple (async) set-request - Version 3 API (TargetName with send-opts)"}].

simple_async_set3_cbp_perm(Config) when is_list(Config) ->
    simple_async_set3(sas3_cbp_perm, Config).

simple_async_set3(Case, Config) ->
    ?TC_TRY(Case,
            fun() -> do_simple_async_set3(Config) end).

do_simple_async_set3(Config) ->
    ?IPRINT("starting with Config: "
            "~n      ~p~n", [Config]),

    MgrNode    = ?config(manager_node, Config),
    AgentNode  = ?config(agent_node, Config),
    TargetName = ?config(manager_agent_target_name, Config),

    ok = mgr_user_load_mib(MgrNode, std_mib()),
    Test2Mib = test2_mib(Config), 
    ok = mgr_user_load_mib(MgrNode, Test2Mib),
    ok = agent_load_mib(AgentNode, Test2Mib),

    Self  = self(), 
    Msg   = simple_async_set3, 
    Fun   = fun() -> Self ! Msg end,
    Extra = {?SNMPM_EXTRA_INFO_TAG, Fun}, 
    SendOpts = 
	[
	 {extra, Extra}
	], 

    Set = 
	fun(Oids) ->
		async_s_exec3(MgrNode, TargetName, Oids, SendOpts)
	end,
    PostVerify = fun(ok)  -> receive Msg -> ok end; 
		    (Res) -> Res 
		 end,

    Res = do_simple_async_set3(MgrNode, AgentNode, Set, PostVerify),
    display_log(Config),
    Res.

do_simple_async_set3(MgrNode, AgentNode, Set, PostVerify) ->
    Requests = 
	[
	 {1,
	  [{?sysName_instance, s, "Arne Anka"}],
	  Set,
	  fun(X) ->
		  PostVerify(sas_verify(X, [?sysName_instance]))
	  end},
	 {2,
	  [{?sysLocation_instance, s, "Stockholm"}, 
	   {?sysName_instance,     s, "Arne Anka"}],
	  Set,
	  fun(X) ->
		  PostVerify(sas_verify(X, 
					[?sysLocation_instance, 
					 ?sysName_instance]))
	  end},
	 {3,
	  [{[sysName, 0],     "Gothenburg"}, 
	   {[sysLocation, 0], "Sune Anka"}],
	  Set,
	  fun(X) ->
		  PostVerify(sas_verify(X, 
					[?sysName_instance, 
					 ?sysLocation_instance]))
	  end}
	],

    ?IPRINT("manager info when starting test: "
            "~n      ~p", [mgr_info(MgrNode)]),
    ?IPRINT("agent info when starting test: "
            "~n      ~p", [agent_info(AgentNode)]),

    ok = async_exec(Requests, []),

    ?IPRINT("manager info when ending test: "
            "~n      ~p", [mgr_info(MgrNode)]),
    ?IPRINT("agent info when ending test: "
            "~n      ~p", [agent_info(AgentNode)]),

    ok.


async_s_exec3(Node, TargetName, VAVs, SendOpts) ->
    mgr_user_async_set2(Node, TargetName, VAVs, SendOpts).


%%======================================================================

fl(L) ->
    lists:flatten(L).

verify_ssgb_reply1({noError, 0, []}) ->
    ok;
verify_ssgb_reply1(X) ->
    {error, {unexpected_reply, X}}.

verify_ssgb_reply2({noError, 0, Vbs}, ExpectedVbs) ->
    check_ssgb_vbs(Vbs, ExpectedVbs);
verify_ssgb_reply2(Error, _) ->
    {error, {unexpected_reply, Error}}.

verify_ssgb_reply3({genErr, 3, Vbs}, ExpectedVbs) ->
    check_ssgb_vbs(Vbs, ExpectedVbs);
verify_ssgb_reply3(Unexpected, _) ->
    {error, {unexpected_reply, Unexpected}}.

check_ssgb_vbs([], []) ->
    ok;
check_ssgb_vbs(Unexpected, []) ->
    {error, {unexpected_vbs, Unexpected}};
check_ssgb_vbs([], Expected) ->
    {error, {expected_vbs, Expected}};
check_ssgb_vbs([#varbind{value = endOfMibView}|R], 
	       [endOfMibView|Expected]) ->
    check_ssgb_vbs(R, Expected);
check_ssgb_vbs([#varbind{oid = Oid}|R], [Oid|Expected]) ->
    check_ssgb_vbs(R, Expected);
check_ssgb_vbs([#varbind{oid = Oid, value = Value}|R], 
	       [{Oid, Value}|Expected]) ->
    check_ssgb_vbs(R, Expected);
check_ssgb_vbs([R|_], [E|_]) ->
    {error, {unexpected_vb, R, E}}.

%%======================================================================

simple_sync_get_bulk3() ->
    [{doc, "Simple (sync) get_bulk-request - "
     "Version 3 API (TargetName with send-opts)"}].

simple_sync_get_bulk3(Config) when is_list(Config) ->
    ?TC_TRY(simple_sync_get_bulk3,
            fun() -> do_simple_sync_get_bulk3(Config) end).

do_simple_sync_get_bulk3(Config) ->
    ?IPRINT("starting with Config: "
            "~n      ~p~n", [Config]),

    MgrNode    = ?config(manager_node, Config),
    AgentNode  = ?config(agent_node, Config),
    TargetName = ?config(manager_agent_target_name, Config),

    Self  = self(), 
    Msg   = simple_async_set3, 
    Fun   = fun() -> Self ! Msg end,
    Extra = {?SNMPM_EXTRA_INFO_TAG, Fun}, 
    SendOpts = 
	[
	 {extra, Extra}
	], 

    GetBulk = 
	fun(NonRep, MaxRep, Oids) ->
		mgr_user_sync_get_bulk2(MgrNode, TargetName, 
					NonRep, MaxRep, Oids, SendOpts)    
	end,
    PostVerify = fun(ok) -> receive Msg -> ok end;
		    (Res) -> Res 
		 end,

    Res = do_simple_sync_get_bulk3(Config, MgrNode, AgentNode, GetBulk, PostVerify),
    display_log(Config),
    Res.

do_simple_sync_get_bulk3(Config, MgrNode, AgentNode, GetBulk, PostVerify) ->
    %% -- 1 --
    ok = do_simple_get_bulk3(1,
				   1,  1, [], 
				   fun verify_ssgb_reply1/1, 
				   GetBulk, PostVerify), 
    
    %% -- 2 --
    ok = do_simple_get_bulk3(2,
				   -1,  1, [], 
				   fun verify_ssgb_reply1/1, 
				   GetBulk, PostVerify), 

    %% -- 3 --
    ok = do_simple_get_bulk3(3,
				   -1, -1, [], 
				   fun verify_ssgb_reply1/1, 
				   GetBulk, PostVerify), 

    ok = mgr_user_load_mib(MgrNode, std_mib()),
    %% -- 4 --
    VF04 = fun(X) -> 
		   verify_ssgb_reply2(X, [?sysDescr_instance, endOfMibView]) 
	   end,
    ok = do_simple_get_bulk3(4,
				   2, 0, [[sysDescr],[1,3,7,1]], VF04, 
				   GetBulk, PostVerify),

    %% -- 5 --
    ok = do_simple_get_bulk3(5,
				   1, 2, [[sysDescr],[1,3,7,1]], VF04, 
				   GetBulk, PostVerify),

    %% -- 6 --
    VF06 = fun(X) -> 
		   verify_ssgb_reply2(X, 
				      [?sysDescr_instance,    endOfMibView,
				       ?sysObjectID_instance, endOfMibView]) 
	   end,
    ok = do_simple_get_bulk3(6,
				   0, 2, [[sysDescr],[1,3,7,1]], VF06, 
				   GetBulk, PostVerify), 

    %% -- 7 --
    VF07 = fun(X) -> 
		   verify_ssgb_reply2(X, 
				      [?sysDescr_instance,    endOfMibView,
				       ?sysDescr_instance,    endOfMibView,
				       ?sysObjectID_instance, endOfMibView]) 
	   end,
    ok = do_simple_get_bulk3(7,
				   2, 2, 
				   [[sysDescr],[1,3,7,1],[sysDescr],[1,3,7,1]],
				   VF07, 
				   GetBulk, PostVerify), 

    Test2Mib = test2_mib(Config), 
    ok = mgr_user_load_mib(MgrNode, Test2Mib),
    ok = agent_load_mib(AgentNode, Test2Mib),

    %% -- 8 --
    VF08 = fun(X) -> 
		   verify_ssgb_reply2(X, 
				      [?sysDescr_instance, 
				       ?sysDescr_instance]) 
	   end,
    ok = do_simple_get_bulk3(8,
				   1, 2, 
				   [[sysDescr],[sysDescr],[tTooBig]],
				   VF08, 
				   GetBulk, PostVerify), 

    %% -- 9 --
    ok = do_simple_get_bulk3(9,
				   1, 12, 
				   [[tDescr2], [sysDescr]], 
				   fun verify_ssgb_reply1/1, 
				   GetBulk, PostVerify),

    %% -- 10 --
    VF10 = fun(X) -> 
		   verify_ssgb_reply3(X, 
				      [{?sysDescr,    'NULL'}, 
				       {?sysObjectID, 'NULL'},
				       {?tGenErr1,    'NULL'},
				       {?sysDescr,    'NULL'}]) 
	   end,
    ok = do_simple_get_bulk3(10,
				   2, 2, 
				   [[sysDescr], 
				    [sysObjectID], 
				    [tGenErr1], 
				    [sysDescr]],
				   VF10, 
				   GetBulk, PostVerify), 

    %% -- 11 --
    {ok, [TCnt2|_]} = mgr_user_name_to_oid(MgrNode, tCnt2),
    ?IPRINT("TCnt2: ~p", [TCnt2]),
    VF11 = fun(X) -> 
		   verify_ssgb_reply2(X, 
				      [{fl([TCnt2,2]), 100}, 
				       {fl([TCnt2,2]), endOfMibView}]) 
	   end,
    ok = do_simple_get_bulk3(11,
				   0, 2, 
				   [[TCnt2, 1]], VF11, 
				   GetBulk, PostVerify),
    
    ok.

do_simple_get_bulk3(N, 
		    NonRep, MaxRep, Oids, 
		    Verify, GetBulk, PostVerify) 
  when is_function(Verify, 1) andalso 
       is_function(GetBulk, 3) andalso 
       is_function(PostVerify) ->
    ?IPRINT("issue get-bulk command ~w", [N]),
    case GetBulk(NonRep, MaxRep, Oids) of
	{ok, Reply, _Rem} ->
	    ?DBG("get-bulk ok:"
		 "~n   Reply: ~p"
		 "~n   Rem:   ~w", [Reply, _Rem]),
	    PostVerify(Verify(Reply));

	Error ->
	    {error, {unexpected_reply, Error}}
    end.


%%======================================================================

simple_async_get_bulk3_cbp_def() ->
    [{doc, "Simple (async) get_bulk-request - "
     "Version 3 API (TargetName with send-opts)"}].

simple_async_get_bulk3_cbp_def(Config) when is_list(Config) ->
    simple_async_get_bulk3(sagb3_cbp_def, Config).

simple_async_get_bulk3(Case, Config) ->
    ?TC_TRY(Case,
            fun() -> do_simple_async_get_bulk3(Config) end).

do_simple_async_get_bulk3(Config) ->
    process_flag(trap_exit, true),
    ?IPRINT("starting with Config: "
            "~n      ~p~n", [Config]),

    MgrNode    = ?config(manager_node, Config),
    AgentNode  = ?config(agent_node, Config),
    TargetName = ?config(manager_agent_target_name, Config),

    ok = mgr_user_load_mib(MgrNode, std_mib()),
    Test2Mib = test2_mib(Config), 
    ok = mgr_user_load_mib(MgrNode, Test2Mib),
    ok = agent_load_mib(AgentNode, Test2Mib),

    Self  = self(), 
    Msg   = simple_async_get_bulk3, 
    Fun   = fun() -> Self ! Msg end,
    Extra = {?SNMPM_EXTRA_INFO_TAG, Fun}, 
    SendOpts = 
	[
	 {extra, Extra}
	], 

    GetBulk = 
	fun(Data) ->
		async_gb_exec3(MgrNode, TargetName, Data, SendOpts)
	end,
    PostVerify = fun(ok)  -> receive Msg -> ok end;
		    (Res) -> Res 
		 end,

    Res = do_simple_async_get_bulk3(MgrNode, AgentNode, GetBulk, PostVerify),
    display_log(Config),
    Res.

do_simple_async_get_bulk3(MgrNode, AgentNode, GetBulk, PostVerify) ->
    %% We re-use the verification functions from the ssgb test-case
    VF04 = fun(X) -> 
		   PostVerify(
		     verify_ssgb_reply2(X, [?sysDescr_instance, endOfMibView]))
	   end,
    VF06 = fun(X) -> 
		   PostVerify(
		     verify_ssgb_reply2(X, 
					[?sysDescr_instance,    endOfMibView,
					 ?sysObjectID_instance, endOfMibView]))
	   end,
    VF07 = fun(X) -> 
		   PostVerify(
		     verify_ssgb_reply2(X, 
					[?sysDescr_instance,    endOfMibView,
					 ?sysDescr_instance,    endOfMibView,
					 ?sysObjectID_instance, endOfMibView]))
	   end,
    VF08 = fun(X) -> 
		   PostVerify(
		     verify_ssgb_reply2(X, 
					[?sysDescr_instance, 
					 ?sysDescr_instance])) 
	   end,
    VF10 = fun(X) -> 
		   PostVerify(
		     verify_ssgb_reply3(X, 
					[{?sysDescr,    'NULL'}, 
					 {?sysObjectID, 'NULL'},
					 {?tGenErr1,    'NULL'},
					 {?sysDescr,    'NULL'}])) 
	   end,
    {ok, [TCnt2|_]} = mgr_user_name_to_oid(MgrNode, tCnt2),
    VF11 = fun(X) -> 
		   PostVerify(
		     verify_ssgb_reply2(X, 
					[{fl([TCnt2,2]), 100}, 
					 {fl([TCnt2,2]), endOfMibView}]))
	   end,
    Requests = [
		{ 1,  
		  {1,  1, []}, 
		  GetBulk, 
		  fun(X) -> PostVerify(verify_ssgb_reply1(X)) end},
		{ 2, 
		  {-1,  1, []}, 
		  GetBulk, 
		  fun(X) -> PostVerify(verify_ssgb_reply1(X)) end},
		{ 3, 
		  {-1, -1, []}, 
		  GetBulk, 
		  fun(X) -> PostVerify(verify_ssgb_reply1(X)) end},
		{ 4,  
		  {2,  0, [[sysDescr],[1,3,7,1]]}, 
		  GetBulk, 
		  VF04},
		{ 5,  
		  {1,  2, [[sysDescr],[1,3,7,1]]}, 
		  GetBulk, 
		  VF04},
		{ 6,  
		  {0,  2, [[sysDescr],[1,3,7,1]]}, 
		  GetBulk, 
		  VF06},
		{ 7,  
		  {2,  2, [[sysDescr],[1,3,7,1],[sysDescr],[1,3,7,1]]}, 
		  GetBulk, 
		  VF07},
		{ 8,  
		  {1,  2, [[sysDescr],[sysDescr],[tTooBig]]}, 
		  GetBulk, 
		  VF08},
		{ 9,  
		  {1, 12, [[tDescr2], [sysDescr]]}, 
		  GetBulk, 
		  fun(X) -> PostVerify(verify_ssgb_reply1(X)) end},
		{10,  
		 {2,  2, [[sysDescr],[sysObjectID], [tGenErr1],[sysDescr]]}, 
		 GetBulk, 
		 VF10},
		{11,  
		 {0,  2, [[TCnt2, 1]]}, 
		 GetBulk,
		 VF11}, 
		{12,  
		 {2,  0, [[sysDescr],[1,3,7,1]]}, 
		 GetBulk,
		 VF04},
		{13,  
		 {1, 12, [[tDescr2], [sysDescr]]},
		 GetBulk, 
		 fun(X) -> PostVerify(verify_ssgb_reply1(X)) end},
		{14,  
		 {2,  2, [[sysDescr],[sysObjectID],[tGenErr1],[sysDescr]]},
		 GetBulk, 
		 VF10},
		{15,  
		 {0,  2, [[TCnt2, 1]]},
		 GetBulk, 
		 VF11},
		{16,  
		 {2,  2, [[sysDescr],[1,3,7,1],[sysDescr],[1,3,7,1]]},
		 GetBulk, 
		 VF07},
		{17,  
		 {2,  2, [[sysDescr],[sysObjectID], [tGenErr1],[sysDescr]]},
		 GetBulk, 
		 VF10}
	       ],

    ?IPRINT("manager info when starting test: "
            "~n      ~p", [mgr_info(MgrNode)]),
    ?IPRINT("agent info when starting test: "
            "~n      ~p", [agent_info(AgentNode)]),

    ok = async_exec(Requests, []),

    ?IPRINT("manager info when ending test: "
            "~n      ~p", [mgr_info(MgrNode)]),
    ?IPRINT("agent info when ending test: "
            "~n      ~p", [agent_info(AgentNode)]),

    ok.


async_gb_exec3(Node, TargetName, {NR, MR, Oids}, SendOpts) ->
    mgr_user_async_get_bulk2(Node, TargetName, NR, MR, Oids, SendOpts).


%%======================================================================

simple_async_get_bulk3_cbp_temp() ->
    [{doc, "Simple (async) get_bulk-request - "
     "Version 3 API (TargetName with send-opts)"}].

simple_async_get_bulk3_cbp_temp(Config) when is_list(Config) ->
    simple_async_get_bulk3(sagb3_cbp_temp, Config).


%%======================================================================

simple_async_get_bulk3_cbp_perm() ->
    [{doc, "Simple (async) get_bulk-request - "
     "Version 3 API (TargetName with send-opts)"}].

simple_async_get_bulk3_cbp_perm(Config) when is_list(Config) ->
    simple_async_get_bulk3(sagb3_cbp_perm, Config).


%%======================================================================

simple_v3_exchange_md5() ->
    [{doc, "Simple message exchange using v3 (MD5) messages"}].

simple_v3_exchange_md5(Config) when is_list(Config) ->
    simple_v3_exchange(Config).

simple_v3_exchange_sha() ->
    [{doc, "Simple message exchange using v3 (SHA) messages"}].

simple_v3_exchange_sha(Config) when is_list(Config) ->
    simple_v3_exchange(Config).

simple_v3_exchange_sha224() ->
    [{doc, "Simple message exchange using v3 (SHA224) messages"}].

simple_v3_exchange_sha224(Config) when is_list(Config) ->
    simple_v3_exchange(Config).

simple_v3_exchange_sha256() ->
    [{doc, "Simple message exchange using v3 (SHA256) messages"}].

simple_v3_exchange_sha256(Config) when is_list(Config) ->
    simple_v3_exchange(Config).

simple_v3_exchange_sha384() ->
    [{doc, "Simple message exchange using v3 (SHA384) messages"}].

simple_v3_exchange_sha384(Config) when is_list(Config) ->
    simple_v3_exchange(Config).

simple_v3_exchange_sha512() ->
    [{doc, "Simple message exchange using v3 (SHA512) messages"}].

simple_v3_exchange_sha512(Config) when is_list(Config) ->
    simple_v3_exchange(Config).

%% This difference between these test cases are handled in the
%% init_per_testcase function (basically auth algorithm).
simple_v3_exchange(Config) when is_list(Config) ->
    ?IPRINT("starting with Config: "
            "~n      ~p", [Config]),

    Node       = ?config(manager_node, Config),
    TargetName = ?config(manager_agent_target_name, Config),

    Oids1 = [?sysDescr_instance],
    ?IPRINT("initial try get (expect failure and a report)"),
    {error, timeout} = mgr_user_sync_get2(Node, TargetName, Oids1, []),
    receive
        {async_event, TargetName, {report, {noError, 0, [#varbind{oid = ?usmStatsNotInTimeWindows_instance}]}} = Report1} ->
            ?IPRINT("received expected report: "
                    "~n      ~p", [Report1]),
            ok
    end,
    ?IPRINT("second try get (expect success)"),
    {ok, Reply2, _} = mgr_user_sync_get2(Node, TargetName, Oids1, []),
    case Reply2 of
        {noError, 0, [#varbind{oid = ?sysDescr_instance}]} ->
            ?IPRINT("expected get success"),
            display_log(Config),
            ok;
        X ->
            {error, X}
    end.



%%======================================================================

discovery(Config) when is_list(Config) ->
    ?SKIP(not_yet_implemented).


%%======================================================================
%% 
%% Utility functions for cases trap1 and trap2
%% 

collect_traps(N) ->
    collect_traps(N, []).

collect_traps(0, TrapInfo) ->
    TrapInfo;
collect_traps(N, Acc) ->
    receive
	{async_event, _From, {trap, TrapInfo}} ->
	    ?IPRINT("collect_traps -> received trap: "
                    "~n      ~p", [TrapInfo]),
	    collect_traps(N-1, [TrapInfo|Acc])
    after 10000 ->
	    ?WPRINT("collect_traps -> still awaiting ~w trap(s) - giving up", [N]),
	    Acc
    end.

verify_traps([], []) ->
    ?IPRINT("verify_traps -> done"),
    ok;
verify_traps([], Verifiers) ->
    ?IPRINT("verify_traps -> done when ~w verifiers remain", [length(Verifiers)]),
    {error, {failed_verify, [Id || {Id, _} <- Verifiers]}};
verify_traps([Trap|Traps], Verifiers0) ->
    ?IPRINT("verify_traps -> entry"),
    case verify_trap(Trap, Verifiers0) of
	{ok, Id} ->
	    ?IPRINT("verify_traps -> trap verified: ~p", [Id]),
	    Verifiers = lists:keydelete(Id, 1, Verifiers0),
	    verify_traps(Traps, Verifiers);
	error ->
	    ?EPRINT("verify_traps -> failed verifying trap: "
                    "~n      ~p", [Trap]),
	    {error, {failed_verifying_trap, Trap}}
    end.

verify_trap(Trap, []) ->
    ?EPRINT("verify_trap -> could not verify trap:"
            "~n   Trap: ~p", [Trap]),
    error;
verify_trap(Trap, [{Id, Verifier}|Verifiers]) ->
    ?IPRINT("verify_trap -> entry with"
            "~n   Id:   ~p"
            "~n   Trap: ~p", [Id, Trap]),
    case Verifier(Trap) of
	ok ->
	    ?IPRINT("verify_trap -> verified"),
	    {ok, Id};
	{error, _} ->
	    ?NPRINT("verify_trap -> not verified"),
	    verify_trap(Trap, Verifiers)
    end.


%%======================================================================

trap1(Config) when is_list(Config) ->
    ?TC_TRY(trap1,
            fun() -> do_trap1(Config) end).

do_trap1(Config) ->
    ?IPRINT("starting with Config: "
            "~n      ~p"
            "~n", [Config]),

    MgrNode   = ?config(manager_node, Config),
    AgentNode = ?config(agent_node, Config),

    ok = mgr_user_load_mib(MgrNode, snmpv2_mib()),
    Test2Mib      = test2_mib(Config), 
    TestTrapMib   = test_trap_mib(Config), 
    TestTrapv2Mib = test_trap_v2_mib(Config), 
    ok = mgr_user_load_mib(MgrNode, Test2Mib),
    ok = mgr_user_load_mib(MgrNode, TestTrapMib),
    ok = mgr_user_load_mib(MgrNode, TestTrapv2Mib),
    ok = agent_load_mib(AgentNode,  Test2Mib),
    ok = agent_load_mib(AgentNode,  TestTrapMib),
    ok = agent_load_mib(AgentNode,  TestTrapv2Mib),

    %% Version 1 trap verification function:
    VerifyTrap_v1 = 
	fun(Ent, Gen, Spec, ExpVBs, Trap) ->
		case Trap of
		    {Ent, Gen, Spec, _Timestamp, VBs} ->
			?IPRINT("trap info as expected"), 
			case (catch validate_vbs(MgrNode, 
						 ExpVBs, VBs)) of
			    ok ->
				?IPRINT("valid trap"),
				ok;
			    Error ->
				?EPRINT("invalid trap: "
                                        "~n      ~p", [Error]),
				Error
			end;
		    {Enteprise, Generic, Spec, Timestamp, VBs} ->
		       ?EPRINT("unepxected v1 trap info:"
                               "~n   Enteprise: ~p"
                               "~n   Generic:   ~p"
                               "~n   Spec:      ~p"
                               "~n   Timestamp: ~p"
                               "~n   VBs:       ~p", 
                               [Enteprise, Generic, Spec, Timestamp, VBs]),
			ExpTrap = {Ent, Gen, Spec, ignore, ExpVBs}, 
			Reason = {unexpected_trap, {ExpTrap, Trap}},
			{error, Reason};
		    {Err, Idx, VBs} ->
			?EPRINT("unexpected trap info: "
                                "~n   Err: ~p"
                                "~n   Idx: ~p"
                                "~n   VBs: ~p", [Err, Idx, VBs]),
			Reason = {unexpected_status, {Err, Idx, VBs}},
			{error, Reason}
		end
	end,

    %% Version 2 trap verification function:
    VerifyTrap_v2 = 
	fun(ExpVBs, Trap) ->
		case Trap of
		    {noError, 0, VBs0} ->
			?IPRINT("trap info as expected: "
                                "~n      ~p", [VBs0]), 
			%% The first two are a timestamp and oid
			[_,_|VBs] = VBs0,
			case (catch validate_vbs(MgrNode, 
						 ExpVBs, VBs)) of
			    ok ->
				?IPRINT("valid trap"),
				ok;
			    Error ->
				?EPRINT("invalid trap: "
                                        "~n      ~p", [Error]),
				Error
			end;
		    {Err, Idx, VBs} ->
			?EPRINT("unexpected error status: "
                                "~n   Err: ~p"
                                "~n   Idx: ~p"
                                "~n   VBs: ~p", [Err, Idx, VBs]),
			Reason = {unexpected_status, {Err, Idx, VBs}},
			{error, Reason}
		end
	end,


    %% -- command 1 --
    %% Collect various info about the manager and the agent
    Cmd1 = 
	fun() ->
		?IPRINT("manager info: "
                        "~n      ~p", [mgr_info(MgrNode)]),
		?IPRINT("agent info: "
                        "~n      ~p", [agent_info(AgentNode)]),
		ok
	end,

    %% -- command 2 --
    %% Make the agent send trap(s) (both a v1 and a v2 trap)
    Cmd2 = 
	fun() ->
		VBs = [{ifIndex,       [1], 1},
		       {ifAdminStatus, [1], 1},
		       {ifOperStatus,  [1], 2}],
		agent_send_trap(AgentNode, linkUp, "standard trap", VBs),
		ok
	end,

    %% -- command 3 --
    %% Version 1 trap verify function
    Cmd3_VerifyTrap_v1 = 
	fun(Trap) ->
		Ent    = [1,2,3], 
		Gen    = 3, 
		Spec   = 0, 
		ExpVBs = [{[ifIndex,       1], 1},
			  {[ifAdminStatus, 1], 1},
			  {[ifOperStatus,  1], 2}],
		VerifyTrap_v1(Ent, Gen, Spec, ExpVBs, Trap)
	end,

    %% Version 2 trap verify function
    Cmd3_VerifyTrap_v2 = 
	fun(Trap) ->
		ExpVBs = [{[ifIndex,       1], 1},
			  {[ifAdminStatus, 1], 1},
			  {[ifOperStatus,  1], 2}],
		VerifyTrap_v2(ExpVBs, Trap)
	end,

    %% Verify the two traps. The order of them is unknown
    Cmd3 = 
	fun() ->
		Verifiers = [{"v1 trap verifier", Cmd3_VerifyTrap_v1},
			     {"v2 trap verifier", Cmd3_VerifyTrap_v2}],
		verify_traps(collect_traps(2), Verifiers)
	end,

    Cmd4 = fun() -> ?SLEEP(1000), ok end,

    Commands = 
	[
	 {1, "Manager and agent info at start of test", Cmd1},
	 {2, "Send trap from agent", Cmd2},
	 {3, "Await trap(s) to manager", Cmd3},
	 {4, "Sleep some time (1 sec)", Cmd4},
	 {5, "Manager and agent info after test completion", Cmd1}
	],

    Res = command_handler(Commands),
    display_log(Config),
    Res.

    
%%======================================================================

trap2(Config) when is_list(Config) ->
    ?TC_TRY(trap2,
            fun() -> do_trap2(Config) end).

do_trap2(Config) ->
    ?IPRINT("starting with Config: "
            "~n      ~p"
            "~n", [Config]),

    MgrNode   = ?config(manager_node, Config),
    AgentNode = ?config(agent_node, Config),

    ok = mgr_user_load_mib(MgrNode, snmpv2_mib()),
    Test2Mib      = test2_mib(Config), 
    TestTrapMib   = test_trap_mib(Config), 
    TestTrapv2Mib = test_trap_v2_mib(Config), 
    ok = mgr_user_load_mib(MgrNode, Test2Mib),
    ok = mgr_user_load_mib(MgrNode, TestTrapMib),
    ok = mgr_user_load_mib(MgrNode, TestTrapv2Mib),
    ok = agent_load_mib(AgentNode,  Test2Mib),
    ok = agent_load_mib(AgentNode,  TestTrapMib),
    ok = agent_load_mib(AgentNode,  TestTrapv2Mib),

    %% Version 1 trap verification function:
    VerifyTrap_v1 = 
	fun(Ent, Gen, Spec, ExpVBs, Trap) ->
		case Trap of
		    {Ent, Gen, Spec, _Timestamp, VBs} ->
			?IPRINT("trap info as expected"), 
			case (catch validate_vbs(MgrNode, 
						 ExpVBs, VBs)) of
			    ok ->
				?IPRINT("valid trap"),
				ok;
			    Error ->
				?EPRINT("invalid trap: ~n   Error: ~p", [Error]),
				Error
			end;
		    {Enteprise, Generic, Spec, Timestamp, VBs} ->
			?EPRINT("unepxected v1 trap info:"
                                "~n   Enteprise: ~p"
                                "~n   Generic:   ~p"
                                "~n   Spec:      ~p"
                                "~n   Timestamp: ~p"
                                "~n   VBs:       ~p", 
                                [Enteprise, Generic, Spec, Timestamp, VBs]),
			ExpTrap = {Ent, Gen, Spec, ignore, ExpVBs}, 
			Reason = {unexpected_trap, {ExpTrap, Trap}},
			{error, Reason};
		    {Err, Idx, VBs} ->
			?EPRINT("unexpected trap info: "
                                "~n   Err: ~p"
                                "~n   Idx: ~p"
                                "~n   VBs: ~p", [Err, Idx, VBs]),
			Reason = {unexpected_status, {Err, Idx, VBs}},
			{error, Reason}
		end
	end,

    %% Version 2 trap verification function:
    VerifyTrap_v2 = 
	fun(ExpVBs, Trap) ->
		case Trap of
		    {noError, 0, VBs0} ->
			?IPRINT("trap info as expected: ~n~p", [VBs0]), 
			%% The first two are a timestamp and oid
			[_,_|VBs] = VBs0,
			case (catch validate_vbs(MgrNode, 
						 ExpVBs, VBs)) of
			    ok ->
				?IPRINT("valid trap"),
				ok;
			    Error ->
				?EPRINT("invalid trap: "
                                        "~n      ~p", [Error]),
				Error
			end;
		    {Err, Idx, VBs} ->
			?EPRINT("unexpected error status: "
                                "~n   Err: ~p"
                                "~n   Idx: ~p"
                                "~n   VBs: ~p", [Err, Idx, VBs]),
			Reason = {unexpected_status, {Err, Idx, VBs}},
			{error, Reason}
		end
	end,

    %% -- command 1 --
    %% Collect various info about the manager and the agent
    Cmd1 = 
	fun() ->
		?IPRINT("manager info: "
                        "~n      ~p", [mgr_info(MgrNode)]),
		?IPRINT("agent info: "
                        "~n      ~p",   [agent_info(AgentNode)]),
		ok
	end,

    %% -- command 2 --
    %% Make the agent send trap(s) (both a v1 and a v2 trap)
    Cmd2 = 
	fun() ->
		VBs = [{sysContact, "pelle"}],
		agent_send_trap(AgentNode, testTrap1, "standard trap", VBs),
		ok
	end,

    %% -- command 3 --
    %% Version 1 trap verify function
    Cmd3_VerifyTrap_v1 = 
	fun(Trap) ->
		Ent    = [1,2,3], 
		Gen    = 1, 
		Spec   = 0, 
		ExpVBs = [{[system, [4,0]], "pelle"}],
		VerifyTrap_v1(Ent, Gen, Spec, ExpVBs, Trap)
	end,

    %% Version 2 trap verify function
    Cmd3_VerifyTrap_v2 = 
	fun(Trap) ->
		ExpVBs = [{[system, [4,0]], "pelle"},
			  {[snmpTrapEnterprise,0], any}],
		VerifyTrap_v2(ExpVBs, Trap)
	end,
		
    %% Verify the two traps. The order of them is unknown
    Cmd3 = 
	fun() ->
		Verifiers = [{"v1 trap verifier", Cmd3_VerifyTrap_v1},
			     {"v2 trap verifier", Cmd3_VerifyTrap_v2}],
		verify_traps(collect_traps(2), Verifiers)
	end,

    %% -- command 4 --
    %% Make the agent send another set of trap(s) (both a v1 and a v2 trap)
    Cmd4 = 
	fun() ->
		VBs = [{ifIndex,       [1], 1},
		       {ifAdminStatus, [1], 1},
		       {ifOperStatus,  [1], 2}],
		agent_send_trap(AgentNode, linkUp, "standard trap", VBs),
		ok
	end,

    
    %% -- command 5 --
    %% Expected varbinds
    ExpVBs5 = [{[ifIndex,       1], 1},
	       {[ifAdminStatus, 1], 1},
	       {[ifOperStatus,  1], 2}],

    
    %% Version 1 trap verify function
    Cmd5_VerifyTrap_v1 = 
	fun(Trap) ->
		Ent    = [1,2,3], 
		Gen    = 3, 
		Spec   = 0, 
		VerifyTrap_v1(Ent, Gen, Spec, ExpVBs5, Trap)
	end,

    %% Version 2 trap verify function
    Cmd5_VerifyTrap_v2 = 
	fun(Trap) ->
		VerifyTrap_v2(ExpVBs5, Trap)
	end,

    %% Verify the two traps. The order of them is unknown
    Cmd5 = 
	fun() ->
		Verifiers = [{"v1 trap verifier", Cmd5_VerifyTrap_v1},
			     {"v2 trap verifier", Cmd5_VerifyTrap_v2}],
		verify_traps(collect_traps(2), Verifiers)
	end,

    %% -- command 6 --
    %% Some sleep before we are done
    Cmd6 = fun() -> ?SLEEP(1000), ok end,

    Commands = 
	[
	 {1, "Manager and agent info at start of test", Cmd1},
	 {2, "Send first trap(s) from agent", Cmd2},
	 {3, "Await the trap(s) from agent", Cmd3},
	 {4, "Send second trap(s) from agent", Cmd4},
	 {5, "Await the trap(s) from the agent", Cmd5},
	 {6, "Sleep some time (1 sec)", Cmd6},
	 {7, "Manager and agent info after test completion", Cmd1}
	],

    Res = command_handler(Commands),
    display_log(Config),
    Res.

    
%%======================================================================

inform1(Config) when is_list(Config) ->
    ?TC_TRY(inform1,
            fun() -> do_inform1(Config) end).

do_inform1(Config) ->
    ?IPRINT("starting with Config: "
            "~n      ~p"
            "~n", [Config]),

    MgrNode   = ?config(manager_node, Config),
    AgentNode = ?config(agent_node, Config),

    ok = mgr_user_load_mib(MgrNode, snmpv2_mib()),
    Test2Mib      = test2_mib(Config), 
    TestTrapMib   = test_trap_mib(Config), 
    TestTrapv2Mib = test_trap_v2_mib(Config), 
    ok = mgr_user_load_mib(MgrNode, Test2Mib),
    ok = mgr_user_load_mib(MgrNode, TestTrapMib),
    ok = mgr_user_load_mib(MgrNode, TestTrapv2Mib),
    ok = agent_load_mib(AgentNode,  Test2Mib),
    ok = agent_load_mib(AgentNode,  TestTrapMib),
    ok = agent_load_mib(AgentNode,  TestTrapv2Mib),

    
    Cmd1 = 
	fun() ->
		?IPRINT("manager info: "
                        "~n      ~p", [mgr_info(MgrNode)]),
		?IPRINT("manager system info: "
                        "~n      ~p", [mgr_sys_info(MgrNode)]),
		?IPRINT("agent info: "
                        "~n      ~p", [agent_info(AgentNode)]),
		ok
	end,

    Cmd2 = 
	fun() ->
		agent_send_notif(AgentNode, testTrapv22, "standard inform"),
		ok
	end,

    Cmd3 = 
	fun() ->
		receive
		    {async_event, From, {inform, Pid, Inform}} ->
			?IPRINT("received inform"),
			case Inform of
			    {noError, 0, VBs} when is_list(VBs) ->
				case (catch validate_testTrapv22_vbs(MgrNode, 
								     VBs)) of
				    ok ->
					?IPRINT("valid inform"),
					Pid ! {handle_inform_no_response, 
					       From}, 
					ok;
				    Error ->
					?EPRINT("invalid inform: "
                                                "~n      ~p", [Error]),
					Error
				end;
			    {Err, Idx, VBs} ->
				?EPRINT("unexpected error status: "
                                        "~n   Err: ~p"
                                        "~n   Idx: ~p"
                                        "~n   VBs: ~p", [Err, Idx, VBs]),
				Reason = {unexpected_status, {Err, Idx, VBs}},
				{error, Reason}
			end
		after 10000 ->
			receive 
			    Any ->
				{error, {timeout_crap, Any}}
			after 1000 ->
				{error, timeout}
			end
		end
	end,

    Cmd4 = 
	fun() ->
		receive
		    {async_event, From, {inform, Pid, Inform}} ->
			?IPRINT("received inform"),
			case Inform of
			    {noError, 0, VBs} when is_list(VBs) ->
				case (catch validate_testTrapv22_vbs(MgrNode, 
								     VBs)) of
				    ok ->
                                        ?IPRINT("valid inform"),
					Pid ! {handle_inform_response, From}, 
					ok;
				    Error ->
					?EPRINT("invalid inform: "
                                                "~n      ~p", [Error]),
					Error
				end;
			    {Err, Idx, VBs} ->
				?EPRINT("unexpected error status: "
                                        "~n   Err: ~p"
                                        "~n   Idx: ~p"
                                        "~n   VBs: ~p", [Err, Idx, VBs]),
				Reason = {unexpected_status, {Err, Idx, VBs}},
				{error, Reason}
			end
		after 20000 ->
			receive 
			    Any ->
				{error, {timeout_crap, Any}}
			after 1000 ->
				{error, timeout}
			end
		end
	end,

    Cmd5 = fun() -> ?SLEEP(5000), ok end,

    Commands = 
	[
	 {1, "Manager and agent info at start of test", Cmd1},
	 {2, "Send notification [no receiver] from agent", Cmd2},
	 {3, "Await first inform to manager - do not reply", Cmd3},
	 {4, "Await second inform to manager - reply", Cmd4},
	 {5, "Sleep some time (5 sec)", Cmd5},
	 {6, "Manager and agent info after test completion", Cmd1}
	],

    Res = command_handler(Commands),
    display_log(Config),
    Res.


%%======================================================================

inform2(Config) when is_list(Config) ->
    ?TC_TRY(inform2,
            fun() -> do_inform2(Config) end).

do_inform2(Config) ->
    ?IPRINT("starting with Config: "
            "~n      ~p"
            "~n", [Config]),

    MgrNode   = ?config(manager_node, Config),
    AgentNode = ?config(agent_node, Config),
    %% Addr = ?config(ip, Config),
    %% Port = ?AGENT_PORT,

    ok = mgr_user_load_mib(MgrNode, snmpv2_mib()),
    Test2Mib      = test2_mib(Config), 
    TestTrapMib   = test_trap_mib(Config), 
    TestTrapv2Mib = test_trap_v2_mib(Config), 
    ok = mgr_user_load_mib(MgrNode, Test2Mib),
    ok = mgr_user_load_mib(MgrNode, TestTrapMib),
    ok = mgr_user_load_mib(MgrNode, TestTrapv2Mib),
    ok = agent_load_mib(AgentNode,  Test2Mib),
    ok = agent_load_mib(AgentNode,  TestTrapMib),
    ok = agent_load_mib(AgentNode,  TestTrapv2Mib),

    Cmd1 = 
	fun() ->
		?IPRINT("manager info: "
                        "~n    ~p", [mgr_info(MgrNode)]),
		?IPRINT("agent info: "
                        "~n    ~p", [agent_info(AgentNode)]),
		ok
	end,

    Cmd2 = 
	fun() ->
		agent_send_notif(AgentNode, 
				 testTrapv22, 
				 {inform2_tag1, self()}, 
				 "standard inform",
				[]),
		ok
	end,

    Cmd3 = 
	fun() ->
		receive
		    {snmp_targets, inform2_tag1, Addrs} ->
			?IPRINT("sent inform to ~p", [Addrs]),
			ok
		after 10000 ->
			receive 
			    Any ->
				{error, {timeout_crap, Any}}
			after 1000 ->
				{error, timeout}
			end
		end
	end,

    Cmd4 = 
	fun() ->
		receive
		    {async_event, From, {inform, Pid, Inform}} ->
			?IPRINT("received inform"),
			case Inform of
			    {noError, 0, VBs} when is_list(VBs) ->
				case (catch validate_testTrapv22_vbs(MgrNode, 
								     VBs)) of
				    ok ->
					?IPRINT("valid inform"),
					Pid ! {handle_inform_no_response, 
					       From}, 
					ok;
				    Error ->
					?IPRINT("invalid inform: "
                                                "~n      ~p", [Error]),
					Error
				end;
			    {Err, Idx, VBs} ->
				?EPRINT("unexpected error status: "
                                        "~n   Err: ~p"
                                        "~n   Idx: ~p"
                                        "~n   VBs: ~p", [Err, Idx, VBs]),
				Reason = {unexpected_status, {Err, Idx, VBs}},
				{error, Reason}
			end
		after 10000 ->
			receive 
			    Any ->
				{error, {timeout_crap, Any}}
			after 1000 ->
				{error, timeout}
			end
		end
	end,

    Cmd5 = 
	fun() ->
		receive
		    {async_event, From, {inform, Pid, Inform}} ->
			?IPRINT("received inform"),
			case Inform of
			    {noError, 0, VBs} when is_list(VBs) ->
				case (catch validate_testTrapv22_vbs(MgrNode, 
								     VBs)) of
				    ok ->
					?IPRINT("valid inform"),
					Pid ! {handle_inform_response, From}, 
					ok;
				    Error ->
					?EPRINT("invalid inform: "
                                                "~n      ~p", [Error]),
					Error
				end;
			    {Err, Idx, VBs} ->
				?EPRINT("unexpected error status: "
                                        "~n   Err: ~p"
                                        "~n   Idx: ~p"
                                        "~n   VBs: ~p", [Err, Idx, VBs]),
				Reason = {unexpected_status, {Err, Idx, VBs}},
				{error, Reason}
			end
		after 20000 ->
			receive 
			    Any ->
				{error, {timeout_crap, Any}}
			after 1000 ->
				{error, timeout}
			end
		end
	end,

    Cmd6 = 
	fun() ->
		receive
		    {snmp_notification, inform2_tag1, {got_response, Addr}} ->
			?IPRINT("received expected \"got response\" notification "
                                "from: "
                                "~n      ~p", [Addr]),
			ok;
		    {snmp_notification, inform2_tag1, {no_response, Addr}} ->
			?EPRINT("Received unexpected \"no response\" "
                                "notification from: "
                                "~n      ~p", [Addr]),
			{error, no_response}
		after 10000 ->
			receive 
			    Any ->
				{error, {timeout_crap, Any}}
			after 1000 ->
				{error, timeout}
			end
		end
	end,

    Cmd7 = fun() -> ?SLEEP(5000), ok end,

    Commands = 
	[
	 {1, "Manager and agent info at start of test", Cmd1},
	 {2, "Send notification [no receiver] from agent", Cmd2},
	 {3, "Await inform-sent acknowledge from agent", Cmd3},
	 {4, "Await first inform to manager - do not reply", Cmd4},
	 {5, "Await second inform to manager - reply", Cmd5},
	 {6, "await inform-acknowledge from agent", Cmd6},
	 {7, "Sleep some time (5 sec)", Cmd7},
	 {8, "Manager and agent info after test completion", Cmd1}
	],

    Res = command_handler(Commands),
    display_log(Config),
    Res.

    
%%======================================================================

inform3(Config) when is_list(Config) ->
    ?TC_TRY(inform3,
            fun() -> do_inform3(Config) end).

do_inform3(Config) ->
    ?IPRINT("starting with Config: "
            "~n      ~p~n", [Config]),

    MgrNode   = ?config(manager_node, Config),
    AgentNode = ?config(agent_node, Config),

    ok = mgr_user_load_mib(MgrNode, snmpv2_mib()),
    Test2Mib      = test2_mib(Config), 
    TestTrapMib   = test_trap_mib(Config), 
    TestTrapv2Mib = test_trap_v2_mib(Config), 
    ok = mgr_user_load_mib(MgrNode, Test2Mib),
    ok = mgr_user_load_mib(MgrNode, TestTrapMib),
    ok = mgr_user_load_mib(MgrNode, TestTrapv2Mib),
    ok = agent_load_mib(AgentNode,  Test2Mib),
    ok = agent_load_mib(AgentNode,  TestTrapMib),
    ok = agent_load_mib(AgentNode,  TestTrapv2Mib),

    Cmd1 = 
	fun() ->
		?IPRINT("manager info: "
                        "~n      ~p", [mgr_info(MgrNode)]),
		?IPRINT("agent info: "
                        "~n      ~p", [agent_info(AgentNode)]),
		ok
	end,

    Cmd2 = 
	fun() ->
		agent_send_notif(AgentNode, 
				 testTrapv22, 
				 {inform3_tag1, self()}, 
				 "standard inform",
				 []),
		ok
	end,

    Cmd3 = 
	fun() ->
		receive
		    {snmp_targets, inform3_tag1, [_Addr]} ->
			?IPRINT("received inform-sent ack"),
			ok
		after 10000 ->
			receive 
			    Crap ->
				{error, {timeout_crap, Crap}}
			after 0 ->
				{error, timeout}
			end
		end
	end,

    Cmd4 = 
	fun() ->
		receive
		    {async_event, From, {inform, Pid, Inform}} ->
			?IPRINT("received inform"),
			case Inform of
			    {noError, 0, VBs} when is_list(VBs) ->
				case (catch validate_testTrapv22_vbs(MgrNode, 
								     VBs)) of
				    ok ->
					?IPRINT("valid inform"),
					Pid ! {handle_inform_no_response, From}, 
					ok;
				    Error ->
					?EPRINT("invalid inform: "
                                                "~n      ~p", [Error]),
					Error
				end;
			    {Err, Idx, VBs} ->
				?EPRINT("unexpected error status: "
                                        "~n   Err: ~p"
                                        "~n   Idx: ~p"
                                        "~n   VBs: ~p", [Err, Idx, VBs]),
				Reason = {unexpected_status, {Err, Idx, VBs}},
				{error, Reason}
			end
		after 50000 ->
			receive 
			    Any ->
				{error, {timeout_crap, Any}}
			after 0 ->
				{error, timeout}
			end
		end
	end,

    Cmd7 = 
	fun() ->
		receive
		    {snmp_notification, inform3_tag1, {no_response, Addr}} ->
			?IPRINT("received expected \"no response\" notification "
                                "from: "
                                "~n   ~p", [Addr]),
			ok;
		    {snmp_notification, inform3_tag1, {got_response, Addr}} ->
			?EPRINT("Received unexpected \"got response\" "
                                "notification from: "
                                "~n   ~p", 
                                [Addr]),
			{error, {got_response, Addr}}
		after 120000 ->
			receive 
			    Crap ->
				{error, {timeout_crap, Crap}}
			after 0 ->
				{error, timeout}
			end
		end
	end,

    Cmd8 = fun() -> ?SLEEP(1000), ok end,

    Commands = 
	[
	 {1, "Manager and agent info at start of test", Cmd1},
	 {2, "Send notification from agent", Cmd2},
	 {3, "await inform-sent acknowledge from agent", Cmd3},
	 {4, "Await first inform to manager - do not reply", Cmd4},
	 {5, "Await first inform to manager - do not reply", Cmd4},
	 {6, "Await first inform to manager - do not reply", Cmd4},
	 {7, "await inform-acknowledge from agent", Cmd7},
	 {8, "Sleep some time (1 sec)", Cmd8},
	 {9, "Manager and agent info after test completion", Cmd1}
	],

    Res = command_handler(Commands),
    display_log(Config),
    Res.

    
%%======================================================================

inform4(Config) when is_list(Config) ->
    ?TC_TRY(inform4,
            fun() -> do_inform4(Config) end).

do_inform4(Config) ->
    ?IPRINT("starting with Config: "
            "~n      ~p~n", [Config]),

    MgrNode   = ?config(manager_node, Config),
    AgentNode = ?config(agent_node, Config),

    ok = mgr_user_load_mib(MgrNode, snmpv2_mib()),
    Test2Mib      = test2_mib(Config), 
    TestTrapMib   = test_trap_mib(Config), 
    TestTrapv2Mib = test_trap_v2_mib(Config), 
    ok = mgr_user_load_mib(MgrNode, Test2Mib),
    ok = mgr_user_load_mib(MgrNode, TestTrapMib),
    ok = mgr_user_load_mib(MgrNode, TestTrapv2Mib),
    ok = agent_load_mib(AgentNode,  Test2Mib),
    ok = agent_load_mib(AgentNode,  TestTrapMib),
    ok = agent_load_mib(AgentNode,  TestTrapv2Mib),

    Cmd1 = 
	fun() ->
		?IPRINT("manager info: "
                        "~n      ~p", [mgr_info(MgrNode)]),
		?IPRINT("agent info: "
                        "~n      ~p", [agent_info(AgentNode)]),
		ok
	end,

    Cmd2 = 
	fun() ->
		agent_send_notif(AgentNode, testTrapv22, "standard inform"),
		ok
	end,

    Cmd3 = 
	fun() ->
		receive
		    {async_event, From, {inform, Pid, Inform}} ->
			?IPRINT("received inform"),
			case Inform of
			    {noError, 0, VBs} when is_list(VBs) ->
				case (catch validate_testTrapv22_vbs(MgrNode, 
								     VBs)) of
				    ok ->
					?IPRINT("valid inform"),
					%% Actually, as we have
					%% configured the manager in
					%% this test case (irb = auto)
					%% it has already responded
					Pid ! {handle_inform_response, From}, 
					ok;
				    Error ->
					?EPRINT("invalid inform: "
                                                "~n      ~p", [Error]),
					Error
				end;
			    {Err, Idx, VBs} ->
				?EPRINT("unexpected error status: "
                                        "~n   Err: ~p"
                                        "~n   Idx: ~p"
                                        "~n   VBs: ~p", [Err, Idx, VBs]),
				Reason = {unexpected_status, {Err, Idx, VBs}},
				{error, Reason}
			end
		after 20000 ->
			receive 
			    Any ->
				{error, {crap, Any}}
			after 1000 ->
				{error, timeout}
			end
		end
	end,

    %% This is the a result of erroneous configuration.
%%     Cmd4 = 
%% 	fun() ->
%% 		receive 
%% 		    {async_event, _ReqId, {error, Reason}} ->
%% 			?IPRINT("received error"),
%% 			case Reason of
%% 			    {failed_processing_message,
%% 			     {securityError, usmStatsUnknownEngineIDs}} ->
%% 				?IPRINT("expected error"), 
%% 				ok;
%% 			    _ ->
%% 				p("unexpected error: "
%% 				  "~n   Reason: ~p", [Reason]),
%% 				{error, {unexpected_error, Reason}}
%% 			end
%% 		after 20000 ->
%% 			receive 
%% 			    Any ->
%% 				{error, {crap, Any}}
%% 			after 1000 ->
%% 				{error, timeout}
%% 			end
%% 		end
%% 	end,

    Cmd5 = fun() -> ?SLEEP(1000), ok end,

    Commands = 
	[
	 {1, "Manager and agent info at start of test", Cmd1},
	 {2, "Send notification [no receiver] from agent", Cmd2},
	 {3, "Await inform to manager", Cmd3},
%% 	 {4, "Await error info (because of erroneous config)", Cmd4},
	 {5, "Sleep some time (1 sec)", Cmd5},
	 {6, "Manager and agent info after test completion", Cmd1}
	],

    Res = command_handler(Commands),
    display_log(Config),
    Res.


%%======================================================================
%% 
%% Test: ts:run(snmp, snmp_manager_test, inform_swarm_cbp_def, [batch]).

inform_swarm_cbp_def(Config) when is_list(Config) ->
    inform_swarm(is_cbp_def, 1500, Config).

inform_swarm_cbp_temp(Config) when is_list(Config) ->
    inform_swarm(is_cbp_temp, 1500, Config).

inform_swarm_cbp_perm(Config) when is_list(Config) ->
    inform_swarm(is_cbp_perm, 1800, Config).

inform_swarm(Case, NumI, Config) ->
    ?TC_TRY(Case,
            fun() -> do_inform_swarm(NumI, Config) end).

do_inform_swarm(NumI, Config) ->
    %% process_flag(trap_exit, true),
    ?IPRINT("starting with"
            "~n      NumI:   ~p"
            "~n      Config: ~p"
            "~n", [NumI, Config]),

    MgrNode   = ?config(manager_node, Config),
    AgentNode = ?config(agent_node,   Config),
    Factor    = ?config(snmp_factor,  Config),

    ok = mgr_user_load_mib(MgrNode, snmpv2_mib()),
    Test2Mib      = test2_mib(Config), 
    TestTrapMib   = test_trap_mib(Config), 
    TestTrapv2Mib = test_trap_v2_mib(Config), 
    ok = mgr_user_load_mib(MgrNode, Test2Mib),
    ok = mgr_user_load_mib(MgrNode, TestTrapMib),
    ok = mgr_user_load_mib(MgrNode, TestTrapv2Mib),
    ok = agent_load_mib(AgentNode,  Test2Mib),
    ok = agent_load_mib(AgentNode,  TestTrapMib),
    ok = agent_load_mib(AgentNode,  TestTrapv2Mib),
    NumInforms = NumI div Factor,

    Collector = self(),

    Generator = 
	erlang:spawn(
	  fun() ->
		  receive
		      {Collector, start} ->
			  ok
		  end,
		  Seqs = lists:seq(1, NumInforms),
		  lists:foreach(
		    fun(N) ->
			    ?IPRINT("send notification ~w", [N]),
			    agent_send_notif(AgentNode, 
					     testTrapv22, 
					     {{inform2_tag1, N}, Collector},
					     "standard inform",
					     []),
			    %% Sleep 1000 ms every 100th notif
			    %% Sleep 100  ms every 10th notif
			    if
                                N rem 100 == 0 ->
                                    Sleep = 1000,
				    ?IPRINT("sleep ~w [~w]", [Sleep, N]),
                                    ?SLEEP(Sleep);
				N rem 10 == 0 ->
                                    Sleep = 100,
				    ?IPRINT("sleep ~w [~w]", [Sleep, N]),
				    ?SLEEP(Sleep);
				true ->
				    ok
			    end
		    end,
		    Seqs),
		  ok
	  end), 

    Cmd1 = 
	fun() ->
		?IPRINT("manager info: "
                        "~n      ~p", [mgr_info(MgrNode)]),
		?IPRINT("agent info: "
                        "~n      ~p", [agent_info(AgentNode)]),
		ok
	end,

    Cmd2 = fun() -> Generator ! {Collector, start}, ok end,

    Cmd3 = 
	fun() ->
		inform_swarm_collector(NumInforms)
	end,


    Cmd4 = fun() -> ?SLEEP(1000), ok end,

    Commands = 
	[
	 {1, "Manager and agent info at start of test",             Cmd1},
	 {2, ?F("Send ~p notification(s) from agent", [NumInforms]), Cmd2},
	 {3, "Await send-ack(s)/inform(s)/response(s)",             Cmd3},
	 {4, "Sleep some time (1 sec)",                             Cmd4},
	 {5, "Manager and agent info after test completion",        Cmd1}
	],

    Res = command_handler(Commands),
    display_log(Config),
    Res.


inform_swarm_collector(N) ->
    inform_swarm_collector(N, 0, 0, 0, ?SECS(60)).

%% Note that we need to deal with re-transmissions!
%% That is, the agent did not receive the ack in time,
%% and therefore did a re-transmit. This means that we
%% expect to receive more inform's then we actually
%% sent. So for success we assume:
%%
%%     SentAckCnt =  N
%%     RespCnt    =  N
%%     RecvCnt    >= N
%% 
inform_swarm_collector(N, SentAckCnt, RecvCnt, RespCnt, _) 
  when ((N == SentAckCnt) and 
	(N == RespCnt)    and
	(N =< RecvCnt)) ->
    ?IPRINT("inform_swarm_collector -> done when"
            "~n      N:          ~w"
            "~n      SentAckCnt: ~w"
            "~n      RecvCnt:    ~w"
            "~n      RespCnt:    ~w", [N, SentAckCnt, RecvCnt, RespCnt]),
    ok;
inform_swarm_collector(N, SentAckCnt, RecvCnt, RespCnt, Timeout) ->
    %% ?IPRINT("inform_swarm_collector -> entry with"
    %%         "~n      N:          ~w"
    %%         "~n      SentAckCnt: ~w"
    %%         "~n      RecvCnt:    ~w"
    %%         "~n      RespCnt:    ~w", [N, SentAckCnt, RecvCnt, RespCnt]),
    receive
	{snmp_targets, {inform2_tag1, Id}, [_Addr]} ->
	    ?IPRINT("received inform-sent acknowledgement for ~w", [Id]),
	    inform_swarm_collector(N, SentAckCnt+1, RecvCnt, RespCnt, 
				   Timeout);

	%% The manager has received the actual inform
	{async_event, From, {inform, Pid, Inform}} ->
	    ?IPRINT("received inform (~p of ~p)", [RecvCnt+1, N]),
	    case Inform of
		{noError, 0, VBs} when is_list(VBs) ->
		    Pid ! {handle_inform_response, From}, 
		    inform_swarm_collector(N, SentAckCnt, RecvCnt+1, RespCnt, 
					   Timeout);
		{Err, Idx, VBs} ->
		    ?EPRINT("Unexpected error status: "
                            "~n   Err: ~p"
                            "~n   Idx: ~p"
                            "~n   VBs: ~p", [Err, Idx, VBs]),
		    Reason = {unexpected_status, {Err, Idx, VBs}},
		    {error, Reason}
	    end;

	%% The agent has received ack from the manager 
	{snmp_notification, {inform2_tag1, Id}, {got_response, Addr}} ->
	    ?IPRINT("received expected \"got response\" for ~w "
                    "notification from: "
                    "~n      ~p", [Id, Addr]),
	    inform_swarm_collector(N, SentAckCnt, RecvCnt, RespCnt+1, 
				   Timeout);

	%% The agent did not received ack from the manager in time 
	{snmp_notification, inform2_tag1, {no_response, Addr}} ->
	    ?EPRINT("Received expected \"no response\" notification "
                    "from: "
                    "~n      ~p", [Addr]),
	    Reason = {no_response, Addr, {N, SentAckCnt, RecvCnt, RespCnt}},
	    {error, Reason}

    after Timeout ->
	    %% Give up when we have been dead in the water for Timeout ms
            ?EPRINT("timeout when"
                    "~n   N:          ~p"
                    "~n   SentAckCnt: ~p"
                    "~n   RecvCnt:    ~p"
                    "~n   RespCnt:    ~p",
                    [N, SentAckCnt, RecvCnt, RespCnt]),
	    {error, {timeout, N, SentAckCnt, RecvCnt, RespCnt}}
    end.
		    

%%======================================================================

report(Config) when is_list(Config) ->
    ?SKIP(not_yet_implemented).

    

%%======================================================================

otp8015_1() ->
    [{doc, "OTP-8015:1 - testing the new api-function."}].

otp8015_1(Config) when is_list(Config) ->
    Pre = fun() ->
                  SCO     = ?config(socket_create_opts, Config),
                  ConfDir = ?config(manager_conf_dir,   Config),
                  DbDir   = ?config(manager_db_dir,     Config),

                  write_manager_conf(ConfDir),

                  Opts = [{server,     [{verbosity, trace}]},
                          {net_if,     [{verbosity, trace}, {options, SCO}]},
                          {note_store, [{verbosity, trace}]},
                          {config,     [{verbosity, trace},
                                        {dir,       ConfDir},
                                        {db_dir,    DbDir}]}],

                  ?IPRINT("starting manager"),
                  ok = snmpm:start_link(Opts),

                  ?SLEEP(1000),
                  ok
          end,
    Case = fun(_) -> do_otp8015_1(Config) end,
    Post = fun(_) ->
                   ?IPRINT("stop manager"),
                   ok = snmpm:stop(),
                   ?SLEEP(1000),
                   ok
           end,
    ?TC_TRY(otp8015_1, Pre, Case, Post).

do_otp8015_1(Config) ->
    ?IPRINT("starting with Config: "
           "~n      ~p"
           "~n", [Config]),

    snmpm:load_mib(std_mib()), 
    snmpm:load_mib(test_trap_mib(Config)),

    ?IPRINT("manager started, now sleep some"),

    ?SLEEP(1000),

    ?IPRINT("loaded mibs: ~p", [snmpm:which_mibs()]),

    ?IPRINT("get some type(s) from the mibs"),    
    {ok, 'Counter32'} = snmpm:oid_to_type(?snmpOutTraps), 
    {ok, [IfIndex]}   = snmpm:name_to_oid(ifIndex),
    {ok, 'INTEGER'}   = snmpm:oid_to_type(IfIndex),
    
    ?IPRINT("end"),
    ok.


%%======================================================================

otp8395_1() ->
    [{doc, "OTP-8395:1 - simple get with ATL sequence numbering."}].

otp8395_1(Config) when is_list(Config) ->
    ?TC_TRY(otp8395_1,
            fun() -> do_otp8395_1(Config) end).

do_otp8395_1(Config) ->
    do_simple_sync_get3(Config).


%%======================================================================
%% async snmp utility functions
%%======================================================================

async_exec([], Acc) ->
    ?IPRINT("all async request's sent => now await responses"),
    async_verify(async_collector(Acc, []));
async_exec([{Id, Data, Exec, Ver}|Reqs], Acc) ->
    ?IPRINT("issue async request ~w", [Id]),
    {ok, ReqId} = Exec(Data),
    async_exec(Reqs, [{ReqId, Id, Ver}|Acc]).

async_collector([], Acc) ->
    ?IPRINT("received replies for all requests - now sort"),
    lists:keysort(1, Acc);
async_collector(Expected, Acc) ->
    receive
	{async_event, ReqId, Reply} ->
	    ?IPRINT("received async event with request-id ~w", [ReqId]),
	    case lists:keysearch(ReqId, 1, Expected) of
		{value, {_, Id, Ver}} ->
		    ?IPRINT("event was for request ~w", [Id]),
		    Expected2 = lists:keydelete(ReqId, 1, Expected),
		    async_collector(Expected2, [{Id, Ver, Reply}|Acc]);
		false ->
		    % Duplicate reply?
                    ?EPRINT("unexpected async event: "
                            "~n      ReqId: ~p"
                            "~n      Reply: ~p", [ReqId, Reply]),
		    ?FAIL({unexpected_async_event, ReqId, Reply})
	    end
    after 10000 ->
	    ?FAIL({timeout, {Expected, Acc}})
    end.

async_verify([]) ->
    ok;
async_verify([{Id, Verify, Reply}|Replies]) ->
    ?IPRINT("verify reply ~w", [Id]),
    Verify(Reply),
    async_verify(Replies).



%%======================================================================
%% Internal functions
%%======================================================================


%% -- Verify varbinds --

validate_vbs(Node, ExpVBs, VBs) ->
    validate_vbs(purify_oids(Node, ExpVBs), VBs).

validate_testTrapv22_vbs(Node, VBs) ->
    ExpVBs = [{[sysUpTime, 0], any},
	      {[snmpTrapOID, 0], ?system ++ [0,1]}],
    validate_vbs(purify_oids(Node, ExpVBs), VBs).

validate_vbs([], []) ->
    ok;
validate_vbs(Exp, []) ->
    {error, {expected_vbs, Exp}};
validate_vbs([], VBs) ->
    {error, {unexpected_vbs, VBs}};
validate_vbs([any|Exp], [_|VBs]) ->
    validate_vbs(Exp, VBs);
validate_vbs([{_, any}|Exp], [#varbind{}|VBs]) ->
    validate_vbs(Exp, VBs);
validate_vbs([{Oid, Val}|Exp], [#varbind{oid = Oid, value = Val}|VBs]) ->
    validate_vbs(Exp, VBs);
validate_vbs([{Oid, Val1}|_], [#varbind{oid = Oid, value = Val2}|_]) ->
    {error, {unexpected_vb_value, Oid, Val1, Val2}};
validate_vbs([{Oid1, _}|_], [#varbind{oid = Oid2}|_]) ->
    {error, {unexpected_vb_oid, Oid1, Oid2}}.

purify_oids(_, []) ->
    [];
purify_oids(Node, [{Oid, Val}|Oids]) ->
    [{purify_oid(Node, Oid), Val}| purify_oids(Node, Oids)].

purify_oid(Node, Oid) ->
    case mgr_user_purify_oid(Node, Oid) of
	Oid2 when is_list(Oid2) ->
	    Oid2;
	{error, _} = Error ->
	    throw(Error)
    end.


%% -- Test case command handler (executor) ---

command_handler([]) ->
    ok;
command_handler([{No, Desc, Cmd}|Cmds]) ->
    ?IPRINT("command_handler -> command ~w: "
            "~n   ~s", [No, Desc]),
    case (catch Cmd()) of
	ok ->
            ?IPRINT("command_handler -> ~w: ok", [No]),
            command_handler(Cmds);
        {error, Reason} ->
            ?EPRINT("Command_handler -> ~w error: "
                    "~n      ~p", [No, Reason]),
            ?FAIL({command_failed, No, Reason});
        Error ->
            ?EPRINT("Command_handler -> ~w unexpected: "
                    "~n      ~p", [No, Error]),
            ?FAIL({unexpected_command_result, No, Error})
    end.


%% -- Misc manager functions --

init_manager(Case, AutoInform, Config) ->

    ?IPRINT("init_manager -> entry with"
            "~n   AutoInform: ~p"
            "~n   Config:     ~p", [AutoInform, Config]),

    %% -- 
    %% Start node
    %% 

    {Peer, Node} = start_node(lists:concat([Case, "-mgr"])),

    %% The point with this (try catch block) is to be 
    %% able to do some cleanup in case we fail to 
    %% start some of the apps. That is, if we fail to 
    %% start the apps (mnesia, crypto and snmp agent) 
    %% we stop the (agent) node!

    try
	begin

	    %% -- 
	    %% Start and initiate crypto on manager node
	    %% 

	    ok = init_crypto(Node),

	    %% 
	    %% Write manager config
	    %% 

	    ok = write_manager_config(Config),

	    IRB  = case AutoInform of
		       true ->
			   auto;
		       _ ->
			   user
		   end,
	    Conf = [{manager_node, Node}, {manager_peer, Peer}, {irb, IRB} | Config],
	    Vsns = [v1,v2,v3], 
	    start_manager(Node, Vsns, Conf)
	end
    catch
	C:{suite_failed, Reason, _M, _L} = E:S when (C =:= exit) ->
	    ?EPRINT("Failure during manager start (suite-failed):"
                    "~n      Reason:      ~p"
                    "~n      StackTrace:  ~p", [Reason, S]), 
	    %% And now, *try* to cleanup
	    (catch peer:stop(Peer)),
	    erlang:raise(C, E, S);
	C:E:S ->
	    ?EPRINT("Failure during manager start: "
                    "~n      Error Class: ~p"
                    "~n      Error:       ~p"
                    "~n      StackTrace:  ~p", [C, E, S]), 
	    %% And now, *try* to cleanup
	    (catch peer:stop(Peer)),
	    ?FAIL({failed_starting_manager, C, E, S})
    end.

init_v3_manager(Case, Config) ->

    ?IPRINT("init_v3_manager -> entry with"
            "~n   Config: ~p", [Config]),

    %% -- 
    %% Start node
    %% 

    {Peer, Node} = start_node(lists:concat([Case, "-v3mgr"])),

    %% The point with this (try catch block) is to be 
    %% able to do some cleanup in case we fail to 
    %% start some of the apps. That is, if we fail to 
    %% start the apps (mnesia, crypto and snmp agent) 
    %% we stop the (agent) node!

    try
	begin

	    %% -- 
	    %% Start and initiate crypto on manager node
	    %% 

	    ok = init_crypto(Node),

	    %% 
	    %% Write manager config
	    %% 

            %% DomainType is just a "dummy" arg to make the 
            %% function choose transportDomainUdpIpv4 as transport domain.
	    ok = write_manager_config(transport, Config),

	    IRB  = auto,
	    Conf = [{manager_node, Node}, {manager_peer, Peer}, {irb, IRB} | Config],
	    Vsns = [v3], 
	    start_manager(Node, Vsns, Conf)
	end
    catch
	C:{suite_failed, Reason, _M, _L} = E:S when (C =:= exit) ->
	    ?EPRINT("Failure during manager start (suite-failed):"
                    "~n      Reason:      ~p"
                    "~n      StackTrace:  ~p", [Reason, S]), 
	    %% And now, *try* to cleanup
	    (catch peer:stop(Peer)),
	    erlang:raise(C, E, S);
	C:E:S ->
	    ?EPRINT("Failure during manager start: "
                    "~n      Error Class: ~p"
                    "~n      Error:       ~p"
                    "~n      StackTrace:  ~p", [C, E, S]), 
	    %% And now, *try* to cleanup
	    (catch peer:stop(Peer)),
	    ?FAIL({failed_starting_manager, C, E, S})
    end.

fin_v3_manager(Config) ->
    fin_manager(Config).

fin_manager(Config) ->
    Node = ?config(manager_node, Config),
    StopMgrRes    = stop_manager(Node),
    StopCryptoRes = fin_crypto(Node),
    StopNode      = peer:stop(?config(manager_peer, Config)),
    ?IPRINT("fin_manager -> stop apps and (mgr node ~p) node results: "
            "~n      SNMP Mgr: ~p"
            "~n      Crypto:   ~p"
            "~n      Node:     ~p", 
            [Node, StopMgrRes, StopCryptoRes, StopNode]),
    Config.
    

%% -- Misc agent functions --

init_agent(Case, Config) ->
    ?IPRINT("init_agent -> entry with"
            "~n   Config: ~p", [Config]),

    %% -- 
    %% Retrieve some dir's
    %% 
    Dir    = ?config(agent_dir, Config),
    MibDir = ?config(mib_dir,  Config),

    %% -- 
    %% Start node
    %% 

    {Peer, Node} = start_node(lists:concat([Case, "-agent"])),

    %% The point with this (try catch block) is to be 
    %% able to do some cleanup in case we fail to 
    %% start some of the apps. That is, if we fail to 
    %% start the apps (mnesia, crypto and snmp agent) 
    %% we stop the (agent) node!

    try
	begin

	    %% -- 
	    %% Start and initiate mnesia on agent node
	    %% 

	    ok = init_mnesia(Node, Dir, ?config(mnesia_debug, Config)),


	    %% -- 
	    %% Start and initiate crypto on agent node
	    %% 

	    ok = init_crypto(Node),


	    %% 
	    %% Write agent config
	    %% 

	    Vsns = [v1,v2], 
	    ok = write_agent_config(Vsns, Config),

	    Conf = [{agent_node, Node},  {agent_peer, Peer},
		    {mib_dir,    MibDir} | Config],

	    %% 
	    %% Start the agent 
	    %% 

	    start_agent(Node, Vsns, Conf)
	end
    catch
	C:{suite_failed, Reason, _M, _L} = E:S when (C =:= exit) ->
	    ?EPRINT("Failure during agent start (suite-failed):"
                    "~n   Reason:     ~p"
                    "~n   StackTrace: ~p", [Reason, S]), 
	    %% And now, *try* to cleanup
	    (catch peer:stop(Peer)),
	    erlang:raise(C, E, S);
	C:E:S ->
	    ?EPRINT("Failure during agent start: "
                    "~n   Error Class: ~p"
                    "~n   Error:       ~p"
                    "~n   StackTrace:  ~p", [C, E, S]), 
	    %% And now, *try* to cleanup
	    (catch peer:stop(Peer)),
	    ?FAIL({failed_starting_agent, C, E, S})
    end.


init_v3_agent(Case, Config) ->
    ?IPRINT("init_v3_agent -> entry with"
            "~n   Config: ~p", [Config]),

    %% -- 
    %% Retrieve some dir's
    %% 
    _Dir    = ?config(agent_dir, Config),
    MibDir = ?config(mib_dir,  Config),

    %% -- 
    %% Start node
    %% 

    %% ?line Node = self(),
    ?line {Peer, Node} = start_node(lists:concat([Case, "-v3agent"])),

    %% The point with this (try catch block) is to be 
    %% able to do some cleanup in case we fail to 
    %% start some of the apps. That is, if we fail to 
    %% start the apps (mnesia, crypto and snmp agent) 
    %% we stop the (agent) node!

    try
	begin

	    %% -- 
	    %% Start and initiate crypto on agent node
	    %% 

	    ok = init_crypto(Node),


	    %% 
	    %% Write agent config
	    %% 

	    Vsns = [v3], 
	    ok = write_agent_config(Vsns, Config),

	    Conf = [{agent_node, Node}, {agent_peer, Peer},
		    {mib_dir,    MibDir} | Config],

	    %% 
	    %% Start the agent 
	    %% 

	    start_agent(Node, Vsns, Conf)
	end
    catch
	C:{suite_failed, Reason, _M, _L} = E:S when (C =:= exit) ->
	    ?EPRINT("Failure during agent start (suite-failed):"
                    "~n   Reason:     ~p"
                    "~n   StackTrace: ~p", [Reason, S]), 
	    %% And now, *try* to cleanup
	    (catch peer:stop(Peer)),
	    erlang:raise(C, E, S);
	C:E:S ->
	    ?EPRINT("Failure during agent start: "
                    "~n   Error Class: ~p"
                    "~n   Error:       ~p"
                    "~n   StackTrace:  ~p", [C, E, S]), 
	    %% And now, *try* to cleanup
	    (catch peer:stop(Peer)),
	    ?FAIL({failed_starting_agent, C, E, S})
    end.


fin_agent(Config) ->
    Node          = ?config(agent_node, Config),
    StopAgentRes  = stop_agent(Node),
    StopCryptoRes = fin_crypto(Node),
    StopMnesiaRes = fin_mnesia(Node),
    StopNode      = peer:stop(?config(agent_peer, Config)),
    ?IPRINT("fin_agent -> stop apps and (agent node ~p) node results: "
            "~n   SNMP Agent: ~p"
            "~n   Crypto:     ~p"
            "~n   Mnesia:     ~p"
            "~n   Node:       ~p", 
            [Node, StopAgentRes, StopCryptoRes, StopMnesiaRes, StopNode]),
    Config.

fin_v3_agent(Config) ->
    Node          = ?config(agent_node, Config),
    StopAgentRes  = stop_agent(Node),
    StopCryptoRes = fin_crypto(Node),
    StopNode      = peer:stop(?config(agent_peer, Config)),
    ?IPRINT("fin_agent -> stop apps and (agent node ~p) node results: "
            "~n   SNMP Agent: ~p"
            "~n   Crypto:     ~p"
            "~n   Node:       ~p", 
            [Node, StopAgentRes, StopCryptoRes, StopNode]),
    Config.

init_mnesia(Node, Dir, MnesiaDebug) 
  when ((MnesiaDebug =/= none) andalso 
	(MnesiaDebug =/= debug) andalso (MnesiaDebug =/= trace)) ->
    init_mnesia(Node, Dir, ?DEFAULT_MNESIA_DEBUG);
init_mnesia(Node, Dir, MnesiaDebug) ->
    ?DBG("init_mnesia -> load application mnesia", []),
    ok = load_mnesia(Node),

    ?DBG("init_mnesia -> application mnesia: set_env dir: ~n~p",[Dir]),
    ok = set_mnesia_env(Node, dir, filename:join(Dir, "mnesia")),

    %% Just in case, only set (known to be) valid values for debug
    if
	((MnesiaDebug =:= debug) orelse (MnesiaDebug =:= trace)) ->
	    ?DBG("init_mnesia -> application mnesia: set_env debug: ~w", 
		 [MnesiaDebug]),
	    ok = set_mnesia_env(Node, debug, MnesiaDebug);
	true ->
	    ok
    end,

    ?DBG("init_mnesia -> create mnesia schema",[]),
    case create_schema(Node) of
	      ok ->
		  ok;
	      {error, {Node, {already_exists, Node}}} ->
		  ok = delete_schema(Node),
		  ok = create_schema(Node);
	      Error ->
		  ?FAIL({failed_creating_mnesia_schema, Error})
	  end,
    
    ?DBG("init_mnesia -> start application mnesia",[]),
    ok = start_mnesia(Node),

    ?DBG("init_mnesia -> create tables",[]),
    ok = create_tables(Node),
    ok.

fin_mnesia(Node) ->
    ok = delete_tables(Node),
    ok = stop_mnesia(Node),
    ok.


init_crypto(Node) ->
    ok = load_crypto(Node),
    ok = start_crypto(Node),
    ok.

fin_crypto(Node) ->
    ok = stop_crypto(Node),
    ok.


%% -- Misc application wrapper functions --

load_app(Node, App) ->
    VerifySuccess = fun(ok) ->
			    ok;
		       ({error, {already_loaded, LoadedApp}}) when (LoadedApp =:= App) ->
			    ok;
		       ({error, Reason}) ->
			    ?EPRINT("failed loading app ~w on ~p: "
                                    "~n      ~p", [App, Node, Reason]),
			    ?FAIL({failed_load, Node, App, Reason})
		    end,
    do_load_app(Node, App, VerifySuccess).

do_load_app(Node, App, VerifySuccess) 
  when (Node =:= node()) andalso is_atom(App) ->
    %% Local app
    exec(fun() -> application:load(App) end, VerifySuccess);
do_load_app(Node, App, VerifySuccess) ->
    %% Remote app
    exec(fun() -> rcall(Node, application, load, [App]) end, VerifySuccess).


start_app(Node, App) ->
    VerifySuccess = fun(ok) ->
			    ok;
		       ({error, {already_started, LoadedApp}}) when (LoadedApp =:= App) ->
			    ok;
		       ({error, Reason}) ->
			    ?EPRINT("failed starting app ~w on ~p: "
                                    "~n   Reason: ~p", [App, Node, Reason]),
			    ?FAIL({failed_start, Node, App, Reason})
		    end,
    start_app(Node, App, VerifySuccess).

start_app(Node, App, VerifySuccess) 
  when (Node =:= node()) andalso is_atom(App) ->
    exec(fun() -> application:start(App) end, VerifySuccess);
start_app(Node, App, VerifySuccess) ->
    exec(fun() -> rcall(Node, application, start, [App]) end, VerifySuccess).


stop_app(Node, App) ->
    VerifySuccess = fun(ok) ->
			    ok;
		       ({error, {not_started, LoadedApp}}) when (LoadedApp =:= App) ->
			    ok;
		       ({error, Reason}) ->
			    ?EPRINT("failed stopping app ~w on ~p: "
                                    "~n      ~p", [App, Node, Reason]),
			    ?FAIL({failed_stop, Node, App, Reason})
		    end,
    stop_app(Node, App, VerifySuccess).
    
stop_app(Node, App, VerifySuccess) 
  when (Node =:= node()) andalso is_atom(App)  ->
    exec(fun() -> application:stop(App) end, VerifySuccess);
stop_app(Node, App, VerifySuccess) when is_atom(App) ->
    exec(fun() -> rcall(Node, application, stop, [App]) end, VerifySuccess).


set_app_env(Node, App, Key, Val) ->
    VerifySuccess = fun(ok) ->
			    ok;
		       ({error, Reason}) ->
			    ?EPRINT("failed setting app ~w env on ~p"
                                    "~n      Key:    ~p"
                                    "~n      Val:    ~p"
                                    "~n      Reason: ~p",
                                    [App, Node, Key, Val, Reason]),
			    ?FAIL({failed_set_app_env, 
				   Node, App, Key, Val, Reason})
		    end,
    set_app_env(Node, App, Key, Val, VerifySuccess).

set_app_env(Node, App, Key, Val, VerifySuccess) 
  when (Node =:= node()) andalso is_atom(App) ->
    exec(fun() -> application:set_env(App, Key, Val) end, VerifySuccess);
set_app_env(Node, App, Key, Val, VerifySuccess) when is_atom(App) ->
    exec(fun() -> rcall(Node, application, set_env, [App, Key, Val]) end, 
	 VerifySuccess).


exec(Cmd, VerifySuccess) ->
    VerifySuccess(Cmd()).


%% -- Misc snmp wrapper functions --

load_snmp(Node)                 -> load_app(Node, snmp).
start_snmp(Node)                -> start_app(Node, snmp).
stop_snmp(Node)                 -> stop_app(Node, snmp).
set_agent_env(Node, Env)        -> set_snmp_env(Node, agent, Env).
set_mgr_env(Node, Env)          -> set_snmp_env(Node, manager, Env).
set_snmp_env(Node, Entity, Env) -> set_app_env(Node, snmp, Entity, Env).

mgr_info(Node) ->
    rcall(Node, snmpm, info, []).

mgr_sys_info(Node) ->
    rcall(Node, snmpm_config, system_info, []).

mgr_register_user(Node, Id, Mod, Data) when is_atom(Mod) ->
    rcall(Node, snmpm, register_user, [Id, Mod, Data]).

mgr_register_usm_user(Node, EngineID, SecName, Credentials)
  when (Node =:= node()) ->
    snmpm:register_usm_user(EngineID, SecName, Credentials);
mgr_register_usm_user(Node, EngineID, SecName, Credentials) ->
    rcall(Node, snmpm, register_usm_user, [EngineID, SecName, Credentials]).

mgr_unregister_user(Node, Id) ->
    rcall(Node, snmpm, unregister_user, [Id]).

mgr_which_users(Node) ->
    rcall(Node, snmpm, which_users, []).

mgr_register_agent(Node, Id, Port, Conf) 
  when is_integer(Port) andalso is_list(Conf) ->
    Localhost = snmp_test_lib:localhost(),
    mgr_register_agent(Node, Id, Localhost, Port, Conf);
mgr_register_agent(Node, Id, TargetName, Config) 
  when is_list(TargetName) andalso is_list(Config) ->
    rcall(Node, snmpm, register_agent, [Id, TargetName, Config]).

mgr_register_agent(Node, Id, Addr, Port, Conf) 
  when is_integer(Port) andalso is_list(Conf) ->
    rcall(Node, snmpm, register_agent, [Id, Addr, Port, Conf]).

mgr_unregister_agent(Node, Id, Port) when is_integer(Port) ->
    Localhost = snmp_test_lib:localhost(),
    rcall(Node, snmpm, unregister_agent, [Id, Localhost, Port]);
mgr_unregister_agent(Node, Id, TargetName) when is_list(TargetName) ->
    rcall(Node, snmpm, unregister_agent, [Id, TargetName]).

mgr_which_agents(Node) ->
    rcall(Node, snmpm, which_agents, []).

mgr_which_agents(Node, Id) ->
    rcall(Node, snmpm, which_agents, [Id]).


%% -- Misc crypto wrapper functions --

load_crypto(Node)  -> load_app(Node,  crypto).
start_crypto(Node) -> start_app(Node, crypto).
stop_crypto(Node)  -> stop_app(Node,  crypto).


%% -- Misc mnesia wrapper functions --

load_mnesia(Node)              -> load_app(Node,    mnesia).
start_mnesia(Node)             -> start_app(Node,   mnesia).
stop_mnesia(Node)              -> stop_app(Node,    mnesia).
set_mnesia_env(Node, Key, Val) -> set_app_env(Node, mnesia, Key, Val).

create_schema(Node) ->
    rcall(Node, mnesia, create_schema, [[Node]]).

delete_schema(Node) ->
    rcall(Node, mnesia, delete_schema, [[Node]]).

create_table(Node, Table) ->
    rcall(Node, mnesia, create_table, [Table]).

delete_table(Node, Table) ->
    rcall(Node, mnesia, delete_table, [Table]).

create_tables(Node) ->
    Tab1 = [{name,       friendsTable2},
	    {ram_copies, [Node]},
	    {snmp,       [{key, integer}]},
	    {attributes, [a1,a2,a3]}],
    Tab2 = [{name,       kompissTable2},
	    {ram_copies, [Node]},
	    {snmp,       [{key, integer}]},
	    {attributes, [a1,a2,a3]}],
    Tab3 = [{name,       snmp_variables},
	    {attributes, [a1,a2]}],
    Tabs = [Tab1, Tab2, Tab3],
    create_tables(Node, Tabs).

create_tables(_Node, []) ->
    ok;
create_tables(Node, [Tab|Tabs]) ->
    case create_table(Node, Tab) of
	{atomic, ok} ->
	    create_tables(Node, Tabs);
	Error ->
	    ?FAIL({failed_creating_table, Node, Tab, Error})
    end.

delete_tables(Node) ->
    Tabs = [friendsTable2, kompissTable2, snmp_variables],
    delete_tables(Node, Tabs).

%% delete_mib_storage_tables(Node) ->
%%     Tabs = [snmpa_mib_data, snmpa_mib_tree, snmpa_symbolic_store],
%%     delete_tables(Node, Tabs).

delete_tables(Node, Tabs) ->
    lists:foreach(fun(Tab) -> delete_table(Node, Tab) end, Tabs).


%% -- Misc manager user wrapper functions --

init_mgr_user(Conf) ->
    ?IPRINT("init_mgr_user -> entry with"
            "~n   Conf: ~p", [Conf]),

    Node   = ?config(manager_node, Conf),
    %% UserId = ?config(user_id, Conf),

    {ok, User} = mgr_user_start(Node),
    ?DBG("start_mgr_user -> User: ~p", [User]),
    link(User),
    
    [{user_pid, User} | Conf].

init_mgr_v3_user(Conf) ->
    ?IPRINT("init_mgr_v3_user -> entry with"
            "~n   Conf: ~p", [Conf]),

    Node   = ?config(manager_node, Conf),
    %% UserId = ?config(user_id, Conf),

    {ok, User} = mgr_user_start(Node),
    ?IPRINT("start_mgr_v3_user -> User: ~p", [User]),
    link(User),

    EngineID    = "agentEngine",
    AuthAlg     = ?config(auth_alg, Conf),
    SecName     = select_secname_from_authalg(AuthAlg),% "authSHA224",
    AuthKey     = select_authkey_from_authalg(AuthAlg),% "passwd_sha224xxxxxxxxxxxxxxx",
    Credentials =
      [ {auth,     select_auth_proto(AuthAlg)},
        {auth_key, AuthKey}
      ],
    ok = mgr_register_usm_user(Node, EngineID, SecName, Credentials),
    
    ?IPRINT("start_mgr_v3_user -> done"),
    [{user_pid, User} | Conf].

select_secname_from_authalg(md5) ->
    "authMD5";
select_secname_from_authalg(sha) ->
    "authSHA";
select_secname_from_authalg(sha224) ->
    "authSHA224";
select_secname_from_authalg(sha256) ->
    "authSHA256";
select_secname_from_authalg(sha384) ->
    "authSHA384";
select_secname_from_authalg(sha512) ->
    "authSHA512".

select_authkey_from_authalg(md5) ->
    "passwd_md5xxxxxx";
select_authkey_from_authalg(sha) ->
    "passwd_shaxxxxxxxxxx";
select_authkey_from_authalg(sha224) ->
    "passwd_sha224xxxxxxxxxxxxxxx";
select_authkey_from_authalg(sha256) ->
    "passwd_sha256xxxxxxxxxxxxxxxxxxx";
select_authkey_from_authalg(sha384) ->
    "passwd_sha384xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
select_authkey_from_authalg(sha512) ->
    "passwd_sha512xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx".


fin_mgr_user(Conf) ->
    User = ?config(user_pid, Conf),
    unlink(User),
    Node = ?config(manager_node, Conf),
    ok = mgr_user_stop(Node),
    Conf.

init_mgr_user_data1(Conf) ->
    Node = ?config(manager_node, Conf),
    TargetName = ?config(manager_agent_target_name, Conf),
    IpFamily   = ?config(ipfamily, Conf),
    Ip         = ?config(ip, Conf),
    Port       = ?AGENT_PORT,
    ok =
	case IpFamily of
	    inet ->
		mgr_user_register_agent(
		  Node, TargetName,
		  [{address,   Ip},
		   {port,      Port},
		   {engine_id, "agentEngine"}]);
	    inet6 ->
		mgr_user_register_agent(
		  Node, TargetName,
		  [{tdomain,   transportDomainUdpIpv6},
		   {taddress,  {Ip, Port}},
		   {engine_id, "agentEngine"}])
	end,
    _Agents = mgr_user_which_own_agents(Node),
    ?DBG("Own agents: ~p", [_Agents]),

    {ok, _DefAgentConf} = mgr_user_agent_info(Node, TargetName, all),
    ?DBG("Default agent config: ~n~p", [_DefAgentConf]),

    ok = mgr_user_update_agent_info(Node, TargetName,
					  community, "all-rights"),
    ok = mgr_user_update_agent_info(Node, TargetName,
					  sec_name, "all-rights"),
    ok = mgr_user_update_agent_info(Node, TargetName,
					  engine_id, "agentEngine"),
    ok = mgr_user_update_agent_info(Node, TargetName,
					  max_message_size, 1024),

    {ok, _AgentConf} = mgr_user_agent_info(Node, TargetName, all),
    ?DBG("Updated agent config: ~n~p", [_AgentConf]),
    Conf.

init_mgr_user_data2(Conf) ->
    ?DBG("init_mgr_user_data2 -> entry with"
	 "~n   Conf: ~p", [Conf]),
    Node       = ?config(manager_node, Conf),
    TargetName = ?config(manager_agent_target_name, Conf),
    IpFamily   = ?config(ipfamily, Conf),
    Ip         = ?config(ip, Conf),
    Port       = ?AGENT_PORT,
    ok =
	case IpFamily of
	    inet ->
		mgr_user_register_agent(
		  Node, TargetName,
		  [{address,   Ip},
		   {port,      Port},
		   {engine_id, "agentEngine"}]);
	    inet6 ->
		mgr_user_register_agent(
		  Node, TargetName,
		  [{tdomain,   transportDomainUdpIpv6},
		   {taddress,  {Ip, Port}},
		   {engine_id, "agentEngine"}])
	end,
    _Agents = mgr_user_which_own_agents(Node),
    ?DBG("Own agents: ~p", [_Agents]),

    {ok, _DefAgentConf} = mgr_user_agent_info(Node, TargetName, all),
    ?DBG("Default agent config: ~n~p", [_DefAgentConf]),

    ok = mgr_user_update_agent_info(Node, TargetName,
					  community, "all-rights"),
    ok = mgr_user_update_agent_info(Node, TargetName,
					  sec_name, "all-rights"),
    ok = mgr_user_update_agent_info(Node, TargetName,
					  max_message_size, 1024),

    {ok, _AgentConf} = mgr_user_agent_info(Node, TargetName, all),
    ?DBG("Updated agent config: ~n~p", [_AgentConf]),
    Conf.

init_mgr_v3_user_data(Conf) ->
    ?IPRINT("init_mgr_v3_user_data -> entry with"
            "~n   Conf: ~p", [Conf]),
    Node       = ?config(manager_node, Conf),
    TargetName = ?config(manager_agent_target_name, Conf),
    IpFamily   = ?config(ipfamily, Conf),
    Ip         = ?config(ip, Conf),
    Port       = ?AGENT_PORT,
    ok =
	case IpFamily of
	    inet ->
		mgr_user_register_agent(
		  Node, TargetName,
		  [{tdomain,   transportDomainUdpIpv4},
		   {taddress,  {Ip, Port}},
		   {engine_id, "agentEngine"},
                   {version,   v3}]);
	    inet6 ->
		mgr_user_register_agent(
		  Node, TargetName,
		  [{tdomain,   transportDomainUdpIpv6},
		   {taddress,  {Ip, Port}},
		   {engine_id, "agentEngine"},
                   {version,   v3}])
	end,
    _Agents = mgr_user_which_own_agents(Node),
    ?IPRINT("Own agents: ~p", [_Agents]),

    {ok, _DefAgentConf} = mgr_user_agent_info(Node, TargetName, all),
    ?IPRINT("Default agent config: ~n~p", [_DefAgentConf]),

    ok = mgr_user_update_agent_info(Node, TargetName,
					  community, "all-rights"),
    SecName = select_secname_from_authalg(?config(auth_alg, Conf)),
    ok = mgr_user_update_agent_info(Node, TargetName,
					  sec_name, SecName),
    ok = mgr_user_update_agent_info(Node, TargetName,
					  sec_level, authNoPriv),
    ok = mgr_user_update_agent_info(Node, TargetName,
					  sec_model, usm),
    ok = mgr_user_update_agent_info(Node, TargetName,
					  max_message_size, 1024),

    {ok, _AgentConf} = mgr_user_agent_info(Node, TargetName, all),
    ?IPRINT("Updated agent config: ~n~p", [_AgentConf]),
    Conf.

fin_mgr_user_data1(Conf) ->
    Node = ?config(manager_node, Conf),
    TargetName = ?config(manager_agent_target_name, Conf),
    mgr_user_unregister_agent(Node, TargetName),
    mgr_user_which_own_agents(Node),
    Conf.

fin_mgr_user_data2(Conf) ->
    Node = ?config(manager_node, Conf),
    TargetName = ?config(manager_agent_target_name, Conf),
    mgr_user_unregister_agent(Node, TargetName),
    mgr_user_which_own_agents(Node),
    Conf.

mgr_user_start(Node) ->
    mgr_user_start(Node, snmp_manager_test_user).
mgr_user_start(Node, Id) ->
    rcall(Node, snmp_manager_user, start, [self(), Id]).

mgr_user_stop(Node) ->
    rcall(Node, snmp_manager_user, stop, []).

mgr_user_register_agent(Node, TargetName, Conf) 
  when is_list(TargetName) andalso is_list(Conf) ->
    rcall(Node, snmp_manager_user, register_agent, [TargetName, Conf]).

mgr_user_unregister_agent(Node, TargetName) when is_list(TargetName) ->
    rcall(Node, snmp_manager_user, unregister_agent, [TargetName]).

mgr_user_agent_info(Node, TargetName, Item) 
  when is_list(TargetName) andalso is_atom(Item) ->
    rcall(Node, snmp_manager_user, agent_info, [TargetName, Item]).

mgr_user_update_agent_info(Node, TargetName, Item, Val) 
  when is_list(TargetName) andalso is_atom(Item) ->
    rcall(Node, snmp_manager_user, update_agent_info, [TargetName, Item, Val]).

mgr_user_which_own_agents(Node) ->
    rcall(Node, snmp_manager_user, which_own_agents, []).

mgr_user_load_mib(Node, Mib) ->
    rcall(Node, snmp_manager_user, load_mib, [Mib]).

%% mgr_user_sync_get2(Node, TargetName, Oids) when is_list(TargetName) ->
%%     mgr_user_sync_get2(Node, TargetName, Oids, []).

mgr_user_sync_get2(Node, TargetName, Oids, SendOpts) when is_list(TargetName) ->
    rcall(Node, snmp_manager_user, sync_get2, [TargetName, Oids, SendOpts]).

mgr_user_async_get2(Node, TargetName, Oids, SendOpts) 
  when is_list(TargetName) ->
    rcall(Node, snmp_manager_user, async_get2, [TargetName, Oids, SendOpts]).

mgr_user_sync_get_next2(Node, TargetName, Oids, SendOpts) 
  when is_list(TargetName) ->
    rcall(Node, snmp_manager_user, sync_get_next2, [TargetName, Oids, SendOpts]).

mgr_user_async_get_next2(Node, TargetName, Oids, SendOpts) 
  when is_list(TargetName) ->
    rcall(Node, snmp_manager_user, async_get_next2, [TargetName, Oids, SendOpts]).

mgr_user_sync_set2(Node, TargetName, VAV, SendOpts) when is_list(TargetName) ->
    rcall(Node, snmp_manager_user, sync_set2, [TargetName, VAV, SendOpts]).

mgr_user_async_set2(Node, TargetName, VAV, SendOpts) when is_list(TargetName) ->
    rcall(Node, snmp_manager_user, async_set2, [TargetName, VAV, SendOpts]).

mgr_user_sync_get_bulk2(Node, TargetName, NonRep, MaxRep, Oids, SendOpts) 
  when is_list(TargetName) ->
    rcall(Node, snmp_manager_user, sync_get_bulk2, 
	  [TargetName, NonRep, MaxRep, Oids, SendOpts]).

mgr_user_async_get_bulk2(Node, TargetName, NonRep, MaxRep, Oids, SendOpts) 
  when is_list(TargetName) ->
    rcall(Node, snmp_manager_user, async_get_bulk2, 
	  [TargetName, NonRep, MaxRep, Oids, SendOpts]).

mgr_user_purify_oid(Node, Oid) ->
    rcall(Node, snmp_manager_user, purify_oid, [Oid]).

mgr_user_name_to_oid(Node, Name) ->
    rcall(Node, snmp_manager_user, name_to_oid, [Name]).

   
%% -- Misc manager wrapper functions --

start_manager(Node, Vsns, Config) ->
    start_manager(Node, Vsns, Config, []).
start_manager(Node, Vsns, Conf0, _Opts) ->

    ?IPRINT("start_manager -> entry with"
            "~n   Node:   ~p"
            "~n   Vsns:   ~p"
            "~n   Conf0:  ~p"
            "~n   Opts:   ~p", [Node, Vsns, Conf0, _Opts]),

    SCO     = ?config(socket_create_opts, Conf0),

    AtlDir  = ?config(manager_log_dir,    Conf0),
    ConfDir = ?config(manager_conf_dir,   Conf0),
    DbDir   = ?config(manager_db_dir,     Conf0),
    IRB     = ?config(irb,                Conf0),

    ConfigVerbosity    = get_opt(manager_config_verbosity,     Conf0, trace),
    NoteStoreVerbosity = get_opt(manager_note_store_verbosity, Conf0, log),
    NetIfVerbosity     = get_opt(manager_net_if_verbosity,     Conf0, trace),

    AtlSeqNo           = get_opt(manager_atl_seqno,            Conf0, false),

    ServerVerbosity    = get_opt(manager_server_verbosity,     Conf0, trace),
    CBP                = get_opt(manager_server_cbproxy,       Conf0, temporary),
    NIS                = get_opt(manager_server_nis,           Conf0, none),

    NetIfConf = 
	case get_opt(manager_net_if_module, Conf0, no_module) of
	    no_module ->
		[{verbosity, NetIfVerbosity},
                 {options,   SCO}];
	    NetIfModule ->
		[{module,    NetIfModule}, 
		 {verbosity, NetIfVerbosity},
                 {options,   SCO}]
	end,

    Env = [{versions,                 Vsns},
	   {inform_request_behaviour, IRB},
	   {audit_trail_log,          [{type,      read_write},
                                       {dir,       AtlDir},
                                       {size,      {10240, 10}},
                                       {repair,    true},
                                       {seqno,     AtlSeqNo}]},
	   {config,                   [{dir,       ConfDir}, 
                                       {db_dir,    DbDir}, 
                                       {verbosity, ConfigVerbosity}]},
	   {note_store,               [{verbosity, NoteStoreVerbosity}]},
	   {server,                   [{verbosity, ServerVerbosity},
                                       {cbproxy,   CBP},
                                       {netif_sup, NIS}]},
	   {net_if,                   NetIfConf}],
    ok = set_mgr_env(Node, Env),

    ok = try start_snmp(Node) of
		   ok ->
		       ok;
		   {error, Reason} ->
		       ?FAIL({failed_start_manager, Reason})
	       catch
		   exit:{suite_failed, {failed_start, _, _, Reason}, _M, _L}:_ ->
		       ?FAIL({failed_start_manager, Reason});
                   C:E:S ->
                       erlang:raise(C, E, S)
	       end,
    Conf0.

stop_manager(Node) ->
    stop_snmp(Node).


%% -- Misc agent wrapper functions --

start_agent(Node, Vsns, Config) ->
    start_agent(Node, Vsns, Config, []).
start_agent(Node, Vsns, Conf0, _Opts) ->
    ?DBG("start_agent -> entry with"
	 "~n   Node:   ~p"
	 "~n   Vsns:   ~p"
	 "~n   Conf0:  ~p"
	 "~n   Opts:   ~p", [Node, Vsns, Conf0, _Opts]),
    
    AtlDir  = ?config(agent_log_dir,  Conf0),
    ConfDir = ?config(agent_conf_dir, Conf0),
    DbDir   = ?config(agent_db_dir,   Conf0),

    MAV  = get_opt(agent_verbosity,                Conf0, trace),
    CV   = get_opt(agent_config_verbosity,         Conf0, info),
    LDBV = get_opt(agent_local_db_verbosity,       Conf0, info),
    MSV  = get_opt(agent_mib_server_verbosity,     Conf0, info),
    NSV  = get_opt(agent_note_store_verbosity,     Conf0, info),
    SSV  = get_opt(agent_symbolic_store_verbosity, Conf0, info),
    %% NIV  = get_opt(agent_net_if_verbosity,         Conf0, log),
    NIV  = get_opt(agent_net_if_verbosity,         Conf0, trace),

    Env = [{versions,        Vsns},
	   {type,            master},
	   {agent_verbosity, MAV},
	   {audit_trail_log, [{type,   read_write},
			      {dir,    AtlDir},
			      {size,   {10240, 10}},
			      {repair, true}]},
	   {config,          [{dir,        ConfDir}, 
			      {force_load, false}, 
			      {verbosity,  CV}]},
	   {db_dir,          DbDir},
	   {local_db,        [{repair,    true},
			      {auto_save, 10000},
			      {verbosity, LDBV}]},
	   {mib_server,      [{verbosity, MSV}]},
	   {note_store,      [{verbosity, NSV}]},
	   {stymbolic_store, [{verbosity, SSV}]},
	   {net_if,          [{verbosity, NIV},
                              %% On some linux "they" add a 127.0.1.1 or something
                              %% similar, so if we don't specify bind_to
                              %% we don't know which address will be selected
                              %% (which will cause problems for some test cases).
                              {options, [{bind_to, true}]}]},
	   {multi_threaded,  true}],
    ok = set_agent_env(Node, Env),

    try start_snmp(Node) of
	      ok ->
		  ok;
	      {error, Reason} ->
		  ?FAIL({failed_start_agent, Reason})
	  catch
	      exit:{suite_failed, {failed_start, _, _, Reason}, _M, _L}:_ ->
		  ?FAIL({failed_start_agent, Reason});
              C:E:S ->
		  erlang:raise(C, E, S)
	  end,
    Conf0.

stop_agent(Node) ->
    stop_snmp(Node).

agent_load_mib(Node, Mib) ->
    rcall(Node, snmpa, load_mibs, [[Mib]]).

agent_send_trap(Node, Trap, Community, VBs) ->
    Args = [snmp_master_agent, Trap, Community, VBs],
    rcall(Node, snmpa, send_trap, Args).

agent_send_notif(Node, Trap, Name) ->
    agent_send_notif(Node, Trap, Name, []).

agent_send_notif(Node, Trap, Name, VBs) ->
    agent_send_notif(Node, Trap, no_receiver, Name, VBs).

agent_send_notif(Node, Trap, Recv, Name, VBs) ->
    Args = [snmp_master_agent, Trap, Recv, Name, VBs],
    rcall(Node, snmpa, send_notification, Args).

agent_info(Node) ->
    rcall(Node, snmpa, info, []).


%% -- Misc node operation wrapper functions --

start_node(Case) ->
    Args = ["-s", "snmp_test_sys_monitor", "start", "-s", "global", "sync"],
    Name = peer:random_name(lists:concat([?MODULE, "-", Case])),
    {ok, Peer, Node}  = ?CT_PEER(#{name => Name, args => Args}),
    global:sync(),
    {Peer, Node}.

%% -- Misc config wrapper functions --

write_manager_config(Config) ->
    write_manager_config(default, Config).

write_manager_config(DomainType, Config) ->
    Dir = ?config(manager_conf_dir, Config),
    Ip  = tuple_to_list(?config(ip, Config)),
    %% Note that Addr and Port are actually only Addr and Port
    %% when DomainType is default.
    %% In all other cases the Addr is TransportDomain and 
    %% port is {Addr, Port}...
    {Addr, Port} =
	case ?config(ipfamily, Config) of
	    inet when (DomainType =:= default) ->
		{Ip, ?MGR_PORT};
	    inet ->
		{transportDomainUdpIpv4, {Ip, ?MGR_PORT}};
	    inet6 ->
		{transportDomainUdpIpv6, {Ip, ?MGR_PORT}}
	end,
    snmp_config:write_manager_snmp_files(
      Dir, Addr, Port, ?MGR_MMS, ?MGR_ENGINE_ID, [], [], []).

write_manager_conf(Dir) ->
    Port = "5000",
    MMS  = "484",
    EngineID = "\"mgrEngine\"",
    Str = lists:flatten(
            io_lib:format("%% Minimum manager config file\n"
                          "{port,             ~s}.\n"
                          "{max_message_size, ~s}.\n"
                          "{engine_id,        ~s}.\n",
                          [Port, MMS, EngineID])),
    write_manager_conf(Dir, Str).

write_manager_conf(Dir, Str) ->
    write_conf_file(Dir, "manager.conf", Str).


write_agent_config(Vsns, Conf) ->
    Dir = ?config(agent_conf_dir, Conf),
    Ip  = tuple_to_list(?config(ip, Conf)),
    Domain =
	case ?config(ipfamily, Conf) of
	    inet ->
		transportDomainUdpIpv4;
	    inet6 ->
		transportDomainUdpIpv6
	end,
    ok = write_agent_config_files(Dir, Vsns, Domain, Ip),
    ok = update_agent_usm(Vsns, Dir),
    ok = update_agent_community(Vsns, Dir),
    ok = update_agent_vacm(Vsns, Dir),
    ok = write_agent_target_addr_conf(Dir, Domain, Ip, Vsns),
    ok = write_agent_target_params_conf(Dir, Vsns),
    ok = write_agent_notify_conf(Dir),
    ok.
    
write_agent_config_files(Dir, Vsns, Domain, Ip) ->
    snmp_config:write_agent_snmp_files(
      Dir, Vsns, Domain, {Ip, ?MGR_PORT}, {Ip, ?AGENT_PORT}, "mgr-test",
      trap, none, "", ?AGENT_ENGINE_ID, ?AGENT_MMS).

update_agent_usm(Vsns, Dir) ->
    case lists:member(v3, Vsns) of
        true ->
            Conf = [{"agentEngine", "all-rights", "all-rights", zeroDotZero, 
                     usmNoAuthProtocol, "", "", 
                     usmNoPrivProtocol, "", "", "",
                     "", ""}, 

                    {"agentEngine", "no-rights", "no-rights", zeroDotZero, 
                     usmNoAuthProtocol, "", "", 
                     usmNoPrivProtocol, "", "", "",
                     "", ""}, 

                    {"agentEngine", "authMD5", "authMD5", zeroDotZero, 
                     usmHMACMD5AuthProtocol, "", "", 
                     usmNoPrivProtocol, "", "", "",
                     "passwd_md5xxxxxx", ""}, 

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
                     usmNoPrivProtocol, "", "", "",
                     "", ""}, 

                    {"mgrEngine", "no-rights", "no-rights", zeroDotZero, 
                     usmNoAuthProtocol, "", "", 
                     usmNoPrivProtocol, "", "", "",
                     "", ""}, 

                    {"mgrEngine", "authMD5", "authMD5", zeroDotZero, 
                     usmHMACMD5AuthProtocol, "", "", 
                     usmNoPrivProtocol, "", "", "",
                     "passwd_md5xxxxxx", ""}, 

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
            snmp_config:update_agent_usm_config(Dir, Conf);
        false ->
            ok
    end.

update_agent_community([v3], _Dir) -> 
    ok;
update_agent_community(_, Dir) ->
    Conf = [{"no-rights", "no-rights", "no-rights", "", ""}],
    snmp_config:update_agent_community_config(Dir, Conf).

update_agent_vacm(_Vsns, Dir) ->
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
    snmp_config:update_agent_vacm_config(Dir, Conf).

write_agent_target_addr_conf(Dir, Domain, Ip, Vsns) ->
    snmp_config:write_agent_snmp_target_addr_conf(
      Dir, Domain, {Ip, ?MGR_PORT}, 300, 3, Vsns).

write_agent_target_params_conf(Dir, Vsns) -> 
    F = fun(v1) -> {"target_v1", v1,  v1,  "all-rights", noAuthNoPriv};
           (v2) -> {"target_v2", v2c, v2c, "all-rights", noAuthNoPriv};
           (v3) -> {"target_v3", v3,  usm, "all-rights", noAuthNoPriv}
        end,
    Conf = [F(Vsn) || Vsn <- Vsns],
    snmp_config:write_agent_target_params_config(Dir, "", Conf).

write_agent_notify_conf(Dir) -> 
    Conf = [{"standard trap",   "std_trap",   trap}, 
            {"standard inform", "std_inform", inform}],
    snmp_config:write_agent_notify_config(Dir, "", Conf).


write_conf_file(Dir, File, Str) ->
    {ok, Fd} = file:open(filename:join(Dir, File), write),
    ok = io:format(Fd, "~s", [Str]),
    file:close(Fd).
 

%% ------

display_log(Config) ->
    case lists:keysearch(manager_log_dir, 1, Config) of
	{value, {_, Dir}} ->
	    case lists:keysearch(manager_node, 1, Config) of
		{value, {_, Node}} ->
		    LogDir  = Dir, 
		    Mibs    = [], 
		    OutFile = j(LogDir, "snmpm_log.txt"), 
		    ?IPRINT("~n"
                            "========================="
                            "  < Audit Trail Log >  "
                            "========================="
                            "~n"),
		    rcall(Node, snmpm, log_to_txt, [LogDir, Mibs, OutFile]),
		    rcall(Node, snmpm, log_to_io, [LogDir, Mibs]),
		    ?IPRINT("~n"
                            "========================="
                            " < / Audit Trail Log > "
                            "========================="
                            "~n");
		false ->
		    ?IPRINT("display_log -> no manager node found"),
		    ok
	    end;
	false ->
	    ?IPRINT("display_log -> no manager log dir found"),
	    ok
    end.


%% ------

test2_mib(Config) ->
    j(test_mib_dir(Config), "Test2.bin").

test_trap_mib(Config) ->
    j(test_mib_dir(Config), "TestTrap.bin").

test_trap_v2_mib(Config) ->
    j(test_mib_dir(Config), "TestTrapv2.bin").

std_mib() ->
    j(mib_dir(), "STANDARD-MIB.bin").

snmpv2_mib() ->
    j(mib_dir(), "SNMPv2-MIB.bin").

test_mib_dir(Config) ->
    ?config(mib_dir, Config).

mib_dir() ->
    j(code:priv_dir(snmp), "mibs").

j(A, B) ->
    filename:join(A, B).


%% ------

get_opt(Key, Opts, Def) ->
    snmp_misc:get_option(Key, Opts, Def).


%% ------

rcall(Node, Mod, Func, Args) when (Node =:= self()) ->
    apply(Mod, Func, Args);
rcall(Node, Mod, Func, Args) ->
    case rpc:call(Node, Mod, Func, Args) of
	{badrpc, nodedown} ->
	    ?FAIL({rpc_failure, Node});
	Else ->
	    Else
    end.


%% ------

ipv6_init(Config) when is_list(Config) ->
    [{ipfamily, inet6} | Config].
