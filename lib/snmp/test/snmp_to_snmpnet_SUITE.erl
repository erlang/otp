%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2014-2025. All Rights Reserved.
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
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test suite uses the following external programs:
%%     snmpget    From packet 'snmp' (in Ubuntu 12.04)
%%     snmpd      From packet 'snmpd' (in Ubuntu 12.04)
%%     snmptrapd  From packet 'snmpd' (in Ubuntu 12.04)
%% They originate from the Net-SNMP applications, see:
%%     http://net-snmp.sourceforge.net/


-module(snmp_to_snmpnet_SUITE).

-export([
         netsnmp_init/1,
         suite/0, all/0, groups/0,
         init_per_suite/1,    end_per_suite/1,
         init_per_group/2,    end_per_group/2, 
         init_per_testcase/2, end_per_testcase/2,

         erlang_agent_netsnmp_get/0,    erlang_agent_netsnmp_get/1,
         erlang_agent_netsnmp_inform/0, erlang_agent_netsnmp_inform/1,
         erlang_manager_netsnmp_get/0,  erlang_manager_netsnmp_get/1

        ]).

-include("snmp_test_lib.hrl").
-include("snmp_to_snmpnet_SUITE_data/OTP-C64-MIB.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("snmp/include/STANDARD-MIB.hrl").
-include_lib("snmp/include/snmp_types.hrl").

-define(AGENT_ENGINE_ID, "ErlangSnmpAgent").
-define(MANAGER_ENGINE_ID, "ErlangSnmpManager").
-define(AGENT_PORT, 4000).
-define(MANAGER_PORT, 8989).
-define(DEFAULT_MAX_MESSAGE_SIZE, 484).

expected(?sysDescr_instance = Oid, get) ->
    OidStr = oid_str(Oid),
    lists:flatten([OidStr | " = STRING: \"Erlang SNMP agent\""]);
expected(?otpC64Num1_instance = Oid, get) ->
    OidStr = oid_str(Oid),
    ?F("~s = Counter64: ~w", [OidStr, ?default_otpC64Num1]);
expected(?otpC64Num2_instance = Oid, get) ->
    OidStr = oid_str(Oid),
    ?F("~s = Counter64: ~w", [OidStr, ?default_otpC64Num2]);
expected(?otpC64Num3_instance = Oid, get) ->
    OidStr = oid_str(Oid),
    ?F("~s = Counter64: ~w", [OidStr, ?default_otpC64Num3]);
expected(?otpC64Num4_instance = Oid, get) ->
    OidStr = oid_str(Oid),
    ?F("~s = Counter64: ~w", [OidStr, ?default_otpC64Num4]).

oids() ->
    [?sysDescr_instance,
     ?otpC64Num1_instance,
     ?otpC64Num2_instance,
     ?otpC64Num3_instance,
     ?otpC64Num4_instance].


%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [
     {group, ipv4},
     {group, ipv6},
     {group, ipv4_ipv6}
    ].

groups() ->
    [
     {ipv4,      [], ipv4_cases()},
     {ipv6,      [], ipv6_cases()},
     {ipv4_ipv6, [], ipv4_ipv6_cases()},

     {snmpget,   [], snmpget_cases()},
     {snmptrapd, [], snmptrapd_cases()},
     {snmpd_mt,  [], snmpd_mt_cases()},
     {snmpd,     [], snmpd_cases()}
    ].

ipv4_cases() ->
    [
     {group, snmpget},
     {group, snmptrapd},
     {group, snmpd_mt},
     {group, snmpd}
    ].

ipv6_cases() ->
    [
     {group, snmpget},
     {group, snmptrapd},
     {group, snmpd_mt},
     {group, snmpd}
    ].

ipv4_ipv6_cases() ->
     [
      {group, snmpget},
      {group, snmptrapd},
      {group, snmpd_mt},
      {group, snmpd}
     ].

snmpget_cases() ->
    [
     erlang_agent_netsnmp_get
    ].

snmptrapd_cases() ->
    [
     erlang_agent_netsnmp_inform
    ].

snmpd_mt_cases() ->
    [
     erlang_manager_netsnmp_get
    ].

snmpd_cases() ->
    [
     erlang_manager_netsnmp_get
    ].



%%
%% -----
%%

init_per_suite(Config0) ->
    ?IPRINT("init_per_suite -> entry with"
            "~n   Config: ~p", [Config0]),

    case netsnmp_init(Config0) of
        {skip, _} = SKIP ->
            SKIP;

        Config1 ->
            case ?LIB:init_per_suite(Config1) of
                {skip, _} = SKIP ->
                    SKIP;

                Config2 when is_list(Config2) ->
                    snmp_test_sys_monitor:start(),

                    MibDir    = ?LIB:lookup(data_dir, Config2),
                    StdMibDir = join(code:lib_dir(snmp), "mibs"),
                    
                    Config3 = [{mib_dir,     MibDir},
                               {std_mib_dir, StdMibDir} | Config2],
                    
                    ?IPRINT("init_per_suite -> end when"
                            "~n      Config: ~p", [Config3]),

                    Config3
            end
    end.

netsnmp_init(Config) ->
    ?IPRINT("check if Net-SNMP is installed"),
    case has_netsnmp() of
        true ->
            ?IPRINT("Net-SNMP installed - check version"),
            case proper_netsnmp_version() of
                true ->
                    ?IPRINT("Net-SNMP accaptable version"),
                    [{agent_port,   ?AGENT_PORT},
                     {manager_port, ?MANAGER_PORT} | Config];
                false ->
                    ?IPRINT("Net-SNMP buggy version"),
                    {skip, "Buggy NetSNMP"}
            end;
        false ->
            ?IPRINT("Net-SNMP not installed"),
            {skip, "No NetSNMP"}
    end.

has_netsnmp() ->
    netsnmp_check("NET-SNMP").

proper_netsnmp_version() ->
    %% These are versions with ("known") issues
    not netsnmp_check("5.4|5.6.2.1").

netsnmp_check(RE) ->
    case re:run(os:cmd("snmpd -v"), RE, [{capture, first}]) of
        nomatch ->
            false;
        {match, _} ->
            true
    end.


end_per_suite(Config) ->
    ?IPRINT("end_per_suite -> entry with"
            "~n   Config: ~p", [Config]),

    snmp_test_sys_monitor:stop(),
    ?LIB:end_per_suite(Config),

    ?IPRINT("end_per_suite -> end"),

    Config.

%%
%% -----
%%

init_per_group(ipv4, Config) ->
    init_per_group_ip([inet], Config);
init_per_group(ipv6, Config) ->
    case os:type() of
	{unix, netbsd} ->
	    {skip, "Host *may* not *properly* support IPV6"};
	_ ->
	    init_per_group_ipv6([inet6], Config)
    end;
init_per_group(ipv4_ipv6, Config) ->
    case os:type() of
	{unix, netbsd} ->
	    {skip, "Host *may* not *properly* support IPV6"};
	_ ->
	    init_per_group_ipv6([inet, inet6], Config)
    end;
init_per_group(snmpget = Exec, Config) ->
    %% From Ubuntu package snmp
    init_per_group_agent(Exec, Config);
init_per_group(snmptrapd = Exec, Config) ->
    %% From Ubuntu package snmpd
    init_per_group_agent(Exec, Config);
init_per_group(snmpd_mt, Config) ->
    Port = mk_port_number(),
    %% From Ubuntu package snmp
    init_per_group_manager(
      snmpd,
      [{agentXPort, Port},
       {manager_net_if_module, snmpm_net_if_mt} | Config]);
init_per_group(snmpd = Exec, Config) ->
    %% From Ubuntu package snmp
    Port = mk_port_number(),
    init_per_group_manager(
       Exec,
       [{agentXPort, Port},
        {manager_net_if_module, snmpm_net_if} | Config]);

init_per_group(_, Config) ->
    Config.

init_per_group_ipv6(Families, Config) ->
    case ?HAS_SUPPORT_IPV6() of
        true ->
            init_per_group_ip(Families, Config);
        false ->
            {skip, "Host does not support IPv6"}
    end.

init_per_group_ip(Families, Config) ->
    AgentPort = ?config(agent_port, Config),
    ManagerPort = ?config(manager_port, Config),
    Transports =
	[begin
             Addr = ?LIB:localhost(Family),
	     {domain(Family), {Addr, AgentPort}}
	 end || Family <- Families],
    Targets =
	[begin
             Addr = ?LIB:localhost(Family),
	     {domain(Family), {Addr, ManagerPort}}
	 end || Family <- Families],
    [{transports, Transports}, {targets, Targets} | Config].

init_per_group_agent(Exec, Config) ->
    Versions = [v2],
    Dir = ?config(priv_dir, Config),
    Transports = ?config(transports, Config),
    Targets = ?config(targets, Config),
    agent_config(Dir, Transports, Targets, Versions),
    find_executable(Exec, [{snmp_versions, Versions} | Config]).

init_per_group_manager(Exec, Config) ->
    Versions = [v2],
    Dir = ?config(priv_dir, Config),
    Targets = ?config(targets, Config),
    manager_config(Dir, Targets),
    find_executable(Exec, [{snmp_versions, Versions} | Config]).


end_per_group(_GroupName, Config) ->
    Config.



%%
%% -----
%%

init_per_testcase(erlang_manager_netsnmp_get = Case, Config) ->

    ?IPRINT("init_per_testcase(~w) -> entry with"
            "~n   Config:  ~p", [Case, Config]),

    case has_mib2c() of
        true ->
            ?IPRINT("init_per_testcase(~w) -> has mib2c", [Case]),
            maybe
                {ok, Name} ?= run_mib2c(["OTP-C64-MIB:OTP-REG"], "otpC64MIB", Config),
                true ?= has_netsnmpconfig(),                
                ?IPRINT("init_per_testcase(~w) -> try create subagent with"
                        "~n   Name: ~p", [Case, Name]),
                {ok, SubAgentName} ?= create_netsnmp_subagent(Name, Config),
                do_init_per_testcase([{subAgentName, SubAgentName},
                                      {manager_net_if_module, snmpm_net_if} | Config])
            end;
        _ ->
            {skip, "Required mib2c not found"}
    end;
init_per_testcase(Case, Config) ->
    ?IPRINT("init_per_testcase(~w) -> entry with"
            "~n   Config:  ~p", [Case, Config]),
    do_init_per_testcase(Config).

do_init_per_testcase(Config) ->
    ?IPRINT("do_init_per_testcase -> entry with"
            "~n   Config: ~p", [Config]),

    snmp_test_global_sys_monitor:reset_events(),

    Dog = ct:timetrap(?SECS(20)),
    application:stop(snmp),
    application:unload(snmp),
    Config1 = [{watchdog, Dog} | Config],

    ?IPRINT("do_init_per_testcase -> done when"
            "~n   Config: ~p", [Config1]),

    Config1.

end_per_testcase(Case, Config) ->

    ?IPRINT("end_per_testcase(~w) -> entry with"
            "~n   Config:  ~p", [Case, Config]),

    ?IPRINT("system events during test: "
            "~n   ~p", [snmp_test_global_sys_monitor:events()]),

    case application:stop(snmp) of
	ok ->
	    ok;
	E1 ->
	    ct:pal("application:stop(snmp) -> ~p", [E1])
    end,
    case application:unload(snmp) of
	ok ->
	    ok;
	E2 ->
	    ct:pal("application:unload(snmp) -> ~p", [E2])
    end,

    ?IPRINT("end_per_testcase(~w) -> done with"
            "~n   Config: ~p", [Case, Config]),

    Config.

find_executable(Exec, Config) ->
    ExecStr = atom_to_list(Exec),
    case os:find_executable(ExecStr) of
	false ->
	    %% The sbin dirs are not in the PATH on all platforms...
	    find_sys_executable(
	      Exec, ExecStr,
	      [["usr", "local", "sbin"],
	       ["usr", "sbin"],
	       ["sbin"]],
	     Config);
	Path ->
	    [{Exec, Path} | Config]
    end.

find_sys_executable(_Exec, ExecStr, [], _Config) ->
    {skip, ExecStr ++ " not found"};
find_sys_executable(Exec, ExecStr, [Dir | Dirs], Config) ->
    case os:find_executable(join(["/" | Dir] ++ [ExecStr])) of
	false ->
	    find_sys_executable(Exec, ExecStr, Dirs, Config);
	Path ->
	    [{Exec, Path} | Config]
    end.

start_agent(Config) ->
    Dir = ?config(mib_dir, Config),
    ok  = application:load(snmp),
    ok  = application:set_env(snmp, agent, agent_app_env(Config)),
    ok  = application:start(snmp),
    Mibs = ["OTP-C64-MIB"],
    load_mibs(Dir, Mibs),
    ok.

load_mibs(Dir, Mibs) ->
    lists:foreach(fun(Mib) -> load_mib(Dir, Mib) end, Mibs).

load_mib(Dir, Mib) ->
    _   = snmpa:unload_mib(snmp_master_agent, Mib),
    ok  = snmpa:load_mib(snmp_master_agent, join(Dir, Mib)).
    
                           

%% stop_agent(_Config) ->
%%     case application:stop(snmp) of
%% 	ok ->
%% 	    ok;
%% 	E1 ->
%% 	    ct:pal("application:stop(snmp) -> ~p", [E1])
%%     end,
%%     case application:unload(snmp) of
%% 	ok ->
%% 	    ok;
%% 	E2 ->
%% 	    ct:pal("application:unload(snmp) -> ~p", [E2])
%%     end.

start_manager(Config) ->
    ok = application:load(snmp),
    ok = application:set_env(snmp, manager, manager_app_env(Config)),
    ok = application:start(snmp).

%% stop_manager(_Config) ->
%%     case application:stop(snmp) of
%% 	ok ->
%% 	    ok;
%% 	E1 ->
%% 	    ct:pal("application:stop(snmp) -> ~p", [E1])
%%     end,
%%     case application:unload(snmp) of
%% 	ok ->
%% 	    ok;
%% 	E2 ->
%% 	    ct:pal("application:unload(snmp) -> ~p", [E2])
%%     end.



%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
erlang_agent_netsnmp_get() ->
    [{doc,"Test that we can access erlang snmp agent " 
      "from snmpnet manager"}].

erlang_agent_netsnmp_get(Config) when is_list(Config) ->
    Transports = ?config(transports, Config),
    start_agent(Config),
    Oids = oids(),
    try
        begin
            [do_get(Oid, Transports, Config) || Oid <- Oids],
            ok
        end
    catch
        throw:{skip, _} = SKIP ->
            SKIP
    end.

do_get(Oid, Transports, Config) ->
    Expected = expected(Oid, get),
    ct:pal("Try get ~p"
           "~n   Expected: ~p", [Oid, Expected]),
    Get = fun(Transport) ->
                  case snmpget(Oid, Transport, Config) of
                      Expected ->
                          ct:pal("Received expected", []),
                          ok;
                      "Timeout: " ++ Rest ->
                          ct:pal("Received unexpected timeout: "
                                 "~n   ~s", [Rest]),
                          throw({skip, Rest});
                      Any ->
                          ct:pal("Received unexpected response: "
                                 "~n   ~p", [Any]),
                          exit({unexpected, Any})
                  end
          end,
    lists:foreach(Get, Transports).


%%--------------------------------------------------------------------
erlang_manager_netsnmp_get() ->
    [{doc,"Test that the erlang snmp manager can access snmpnet agent"}].

erlang_manager_netsnmp_get(Config) when is_list(Config) ->
    Community  = "happy-testing",
    SysDescr   = "Net-SNMP agent",
    TargetName = "Target Net-SNMP agent",
    Transports = ?config(transports, Config),
    SubAgentName = ?config(subAgentName, Config),
    case start_snmpd(Community, SysDescr, Config) of
        {skip, _} = SKIP ->
            SKIP;
        ProgHandle ->
            SubAgentHandle = start_netsnmp_subagent(SubAgentName, Config),
            start_manager(Config),
            snmp_manager_user:start_link(self(), test_user),
            [snmp_manager_user:register_agent(
               TargetName++domain_suffix(Domain),
               [{reg_type, target_name},
                {tdomain, Domain}, {taddress, Addr},
                {community, Community}, {engine_id, "EngineId"},
                {version, v2}, {sec_model, v2c}, {sec_level, noAuthNoPriv}])
             || {Domain, Addr} <- Transports],
            Results =
                [snmp_manager_user:sync_get2(
                   TargetName++domain_suffix(Domain),
                   oids(), [])
                 || {Domain, _} <- Transports],
            ct:pal("sync_get -> ~p", [Results]),
            snmp_manager_user:stop(),
            stop_program(ProgHandle),
            stop_program(SubAgentHandle),
            [verify_result(R) || R <- Results],
            ok
    end.

verify_result({ok, {noError, 0, VBs}, _}) ->
    verify_varbinds(VBs);
verify_result(Result) ->
    exit({invalid_result, Result}).

verify_varbinds([] = _VBs) ->
    ok;
verify_varbinds([VB | VBs]) ->
    verify_varbind(VB),
    verify_varbinds(VBs).

verify_varbind(#varbind{oid          = Oid,
                        variabletype = Type,
                        value        = Value}) ->
    verify_oid(Oid, Type, Value).

verify_oid(?sysDescr_instance = Oid, 'OCTET STRING', "Net-SNMP agent") ->
    ct:pal("oid verification success for ~p (sys description)", [Oid]),
    ok;
verify_oid(?otpC64Num1_instance = Oid, 'Counter64', ?default_otpC64Num1) ->
    ct:pal("oid verification success for ~p (num 1)", [Oid]),
    ok;
verify_oid(?otpC64Num2_instance = Oid, 'Counter64', ?default_otpC64Num2) ->
    ct:pal("oid verification success for ~p (num 2)", [Oid]),
    ok;
verify_oid(?otpC64Num3_instance = Oid, 'Counter64', ?default_otpC64Num3) ->
    ct:pal("oid verification success for ~p (num 3)", [Oid]),
    ok;
verify_oid(?otpC64Num4_instance = Oid, 'Counter64', ?default_otpC64Num4) ->
    ct:pal("oid verification success for ~p (num 4)", [Oid]),
    ok;
verify_oid(Oid, 'NULL', noSuchObject = Value) ->
    %% This means we where unable to load the proper MIBs
    ct:pal("oid verification failed for ~p => ~p => IGNORE", [Oid, Value]),
    exit({skip, {Oid, Value}});
verify_oid(Oid, Type, Value) ->
    ct:pal("oid verification failed: ~p => ~p", [Oid, Value]),
    exit({invalid_vb, Oid, Type, Value}).


%%--------------------------------------------------------------------
erlang_agent_netsnmp_inform() ->
    [{doc, "TBD"}].

erlang_agent_netsnmp_inform(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    Mib = "TestTrapv2",

    start_agent(Config),
    ok = snmpa:load_mib(snmp_master_agent, join(DataDir, Mib)),

    case start_snmptrapd(Mib, Config) of
        {skip, _} = SKIP ->
            SKIP;
        ProgHandle ->
            snmpa:send_notification(
              snmp_master_agent, testTrapv22, {erlang_agent_test, self()}),
            receive
                {snmp_targets, erlang_agent_test, Addresses} ->
                    ct:pal("Notification sent to: ~p~n", [Addresses]),
                    erlang_agent_netsnmp_inform_responses(Addresses)
            end,
            stop_program(ProgHandle)
    end.

erlang_agent_netsnmp_inform_responses([]) ->
    receive
	{snmp_notification, erlang_agent_test, _} = Unexpected ->
	    ct:pal("Unexpected response: ~p", [Unexpected]),
	    erlang_agent_netsnmp_inform_responses([])
    after 0 ->
	    ok
    end;
erlang_agent_netsnmp_inform_responses([Address | Addresses]) ->
    receive
	{snmp_notification, erlang_agent_test,
	 {got_response, Address}} ->
	    ct:pal("Got response from: ~p~n", [Address]),
	    erlang_agent_netsnmp_inform_responses(Addresses);
	{snmp_notification, erlang_agent_test,
	 {no_response, _} = NoResponse} ->
	    ct:fail(NoResponse)
    end.

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------

snmpget(Oid, Transport, Config) ->
    ct:pal("Try get ~p on ~p~n", [Oid, Transport]),
    Versions = ?config(snmp_versions, Config),

    Args =
	["-c", "public", net_snmp_version(Versions),
	 "-m", ":",
	 "-Cf",
	 net_snmp_addr_str(Transport),
	 oid_str(Oid)],
    case start_program(snmpget, Args, none, Config) of
        {skip, _} = SKIP ->
            throw(SKIP);
        ProgHandle ->
            {_, line, Line} = get_program_output(ProgHandle),
            stop_program(ProgHandle),
            binary_to_list(Line)
    end.

start_snmptrapd(Mibs, Config) ->
    DataDir = ?config(data_dir, Config),
    MibDir = join(code:lib_dir(snmp), "mibs"),
    Targets = ?config(targets, Config),
    SnmptrapdArgs =
	["-f", "-Lo", "-C",
	 "-m", Mibs,
	 "-M", MibDir++":"++DataDir,
         case os:type() of
             {unix, sunos} ->
                 "-a";
             _ ->
                 "--disableAuthorization=yes"
         end,
	 "--snmpTrapdAddr=" ++ net_snmp_addr_str(Targets)],
    {ok, StartCheckMP} = re:compile("NET-SNMP version ", [anchored]),
    start_program(snmptrapd, SnmptrapdArgs, StartCheckMP, Config).

start_snmpd(Community, SysDescr, Config) ->
    DataDir    = ?config(data_dir, Config),
    MibDir     = ?config(mib_dir, Config),
    StdMibDir  = ?config(std_mib_dir, Config),
    Targets    = ?config(targets, Config),
    Transports = ?config(transports, Config),
    Port       = ?config(agentXPort, Config),
    CommunityArgs =
	["--rocommunity"++domain_suffix(Domain)++"="
	 ++Community++" "++inet_parse:ntoa(Ip)
	 || {Domain, {Ip, _}} <- Targets],

    Mibs    = "SNMPv2-SMI" ++
        ":" ++ "OTP-REG" ++
        ":" "OTP-C64-MIB",
    MibDirs = StdMibDir ++ ":" ++ MibDir,

    SnmpdArgs =
        ["-f", "-r", %"-Dverbose",
         "-c", join(DataDir, "snmpd.conf"),
         "-C",
         "-Lo",
         "-m", Mibs,
         "-M", MibDirs,
	 "--sysDescr="++SysDescr,
	 "--agentXSocket=tcp:localhost:"++integer_to_list(Port)]
	++ CommunityArgs
	++ [net_snmp_addr_str(Transports)],
    {ok, StartCheckMP} = re:compile("NET-SNMP version ", [anchored]),
    start_program(snmpd, SnmpdArgs, StartCheckMP, Config).
has_mib2c() ->
        mib2c_check("You didn't give mib2c a valid OID to start with.").

mib2c_check(RE) ->
    case re:run(os:cmd("mib2c"), RE, [{capture, first}]) of
        nomatch ->
            false;
        {match, _} ->
            true
    end.

has_netsnmpconfig() ->
        netsnmpconfig_check("Usage:\n  net-snmp-config").
netsnmpconfig_check(RE) ->
        R = os:cmd("net-snmp-config --help"),
        ct:pal("net-snmp-config -> ~p", [R]),
        case re:run(R, RE, [{capture, first}]) of
                nomatch ->
                {skip, "No net-snmp-config available"};
                {match, _} ->
                true
        end.
run_mib2c(MibList, Anchor, Config) ->
        StdMibDir = join(code:lib_dir(snmp), "mibs"),
        MibDir    = ?LIB:lookup(data_dir, Config),
        Cmd = lists:flatten(["env MIBDIRS=", StdMibDir, ":", MibDir,
                             " MIBS=+", MibList,
                             " mib2c -c mib2c.c64.conf -q -I ", MibDir, " ", Anchor]),
        R = os:cmd(Cmd),
        {ok, Cwd} = file:get_cwd(),
        ct:pal("~p -> ~p", [Cmd, R]),
        case file:list_dir(Cwd) of
            {ok, Files} ->
                %% Check that the generated files are present
                AreGenerated =
                lists:all(fun(Pattern) ->
                        lists:any(fun(File) -> re:run(File, Pattern) =/= nomatch end, Files)
                end, [Anchor ++ "\.c", Anchor ++ "\.h"]),
                case AreGenerated of
                    true ->
                        {ok, Anchor};
                    false ->
                        {skip, lists:flatten(["Failed to generate ",Anchor, " with mib2c"])}
                end;
            {error, _Reason} ->
                {skip, lists:flatten(["Failed to list ",Cwd," after mib2c"])}
        end.

create_netsnmp_subagent(Name, Config) ->
        AgentName = case lists:split(length(Name) - 3, Name) of
                {Prefix, Mib} when Mib == "MIB" orelse Mib == "mib" ->
                        Prefix ++ "_netsnmp_subagent";
                _ ->
                        Name ++ "_netsnmp_subagent"
                end,
        _PrivDir = ?LIB:lookup(priv_dir, Config),
        Cmd = ["net-snmp-config ",
               "--compile-subagent ", AgentName, " ",
               Name ++ ".c"],
        R = os:cmd(Cmd),
        {ok, Cwd} = file:get_cwd(),
        ct:pal("net-snmp-config -> ~p", [R]),
        case file:list_dir(Cwd) of
            {ok, Files} ->
                %% Check that the generated files are present
                AreGenerated = lists:any(fun(File) -> re:run(File, AgentName) =/= nomatch end, Files),
                case AreGenerated of
                    true ->
                        os:cmd("chmod +x " ++ AgentName),
                        {ok, AgentName};
                    false ->
                        {skip, lists:flatten(["Failed to create subagent: ",AgentName])}
                end;
            {error, _Reason} ->
                {skip, lists:flatten(["Failed to list ",Cwd," after net-snmp-config"])}
        end.

start_netsnmp_subagent(Name, Config) ->
        Port       = ?config(agentXPort, Config),
        MibDir     = ?config(mib_dir, Config),
        StdMibDir  = ?config(std_mib_dir, Config),
        MibDirs = StdMibDir ++ ":" ++ MibDir,
        {ok, Cwd} = file:get_cwd(),
        Program = join(Cwd, Name),
        AgentArgs = ["-f", "-C", "-M", MibDirs, "-x", "tcp:localhost:"++integer_to_list(Port)],
        start_program(list_to_atom(Name), AgentArgs, none, [{list_to_atom(Name), Program} | Config]).
%% Debug function: It simple makes printing the args string simple
prep([]) ->
    "";
prep([Arg]) when is_list(Arg) ->
    Arg;
prep([Arg|Args]) ->
    Arg ++ " " ++ prep(Args).

start_program(Prog, Args, StartCheckMP, Config) ->
    ct:pal("Starting program: ~w ~p:"
           "~n~s", [Prog, Args, ?F("~w ~s", [Prog, prep(Args)])]),
    Path = ?config(Prog, Config),
    DataDir = ?config(data_dir, Config),
    StartWrapper = join(DataDir, "start_stop_wrapper"),
    Parent = self(),
    %% process_flag(trap_exit, true),
    {Pid, Mon} =
	spawn_monitor(
	  fun () ->
		  run_program(Parent, StartWrapper, [Path | Args])
	  end),
    start_check(Pid, Mon, StartCheckMP).

start_check(Pid, Mon, none) ->
    {Pid, Mon};
start_check(Pid, Mon, StartCheckMP) ->
    receive
	{Pid, line, Line} ->
	    case re:run(Line, StartCheckMP, [{capture, none}]) of
		match ->
		    {Pid, Mon};
		nomatch ->
		    start_check(Pid, Mon, StartCheckMP)
	    end;
	{'DOWN', Mon, _, _Pid, {skip, Reason} = SKIP} ->
	    ct:pal("Received DOWN from ~p"
                   "~n   Skip Reason: ~p", [_Pid, Reason]),
            SKIP;
	{'DOWN', Mon, _, _, Reason} ->
	    ct:fail("Prog ~p start failed: ~p", [Pid, Reason])
    end.

get_program_output({Pid, Mon}) ->
    receive
	{Pid, _, _} = Msg ->
	    Msg;
	{'DOWN', Mon, _, _, Reason} ->
	    ct:fail("Prog ~p crashed: ~p", [Pid, Reason])
    end.

stop_program({Pid, _} = Handle) ->
    Pid ! {self(), stop},
    wait_program_stop(Handle).

wait_program_stop({Pid, Mon}) ->
    receive
	{Pid, exit, ExitStatus} ->
	    receive
		{'DOWN', Mon, _, _, _} ->
		    ExitStatus
	    end;
	{'DOWN', Mon, _, _, Reason} ->
	    ct:fail("Prog stop: ~p", [Reason])
    end.

run_program(Parent, StartWrapper, ProgAndArgs) ->
    [Prog | _] = ProgAndArgs,
    Port =
	open_port(
	  {spawn_executable, StartWrapper},
	  [{args, ProgAndArgs}, binary, stderr_to_stdout, {line, 80},
	   exit_status]),
    ct:pal("Prog ~p started: ~p", [Port, Prog]),
    run_program_loop(Parent, Port, []).

run_program_loop(Parent, Port, Buf) ->
    receive
	{Parent, stop} ->
	    true = port_command(Port, <<"stop\n">>),
	    ct:pal("Prog ~p stop", [Port]),
	    run_program_loop(Parent, Port, Buf);
	{Port, {data, {Flag, Data}}} ->
	    case Flag of
		eol ->
		    Line  = iolist_to_binary(lists:reverse(Buf, Data)),
                    ct:pal("Prog ~p output: ~s", [Port, Line]),
                    %% There are potentially many different fail outputs,
                    %% but for now we test for just this one: illegal option
                    IOpt = "illegal option",
                    case string:find(binary_to_list(Line), IOpt) of
                        nomatch ->
                            Parent ! {self(), line, Line},
                            run_program_loop(Parent, Port, []);
                        Line2 ->
                            %% Try to extract the actual illegal option string
                            IOpt2 =
                                case string:take(
                                       string:prefix(Line2, IOpt), [$-, $ ]) of
                                    {_, Str} when length(Str) > 0 ->
                                        Str;
                                    _X ->
                                        Line2
                                end,
                            ct:pal("Force program ~p stop", [Port]),
                            true = port_command(Port, <<"stop\n">>),
                            (catch port_close(Port)),
                            exit({skip, {illegal_option, IOpt2}})
                    end;
		noeol ->
		    run_program_loop(Parent, Port, [Data | Buf])
	    end;
	{Port, {exit_status, ExitStatus}} ->
	    ct:pal("Prog ~p exit: ~p", [Port, ExitStatus]),
	    catch port_close(Port),
	    Parent ! {self(), exit, ExitStatus};
	Unexpected ->
	    ct:pal("run_program_loop Unexpected: ~p", [Unexpected]),
	    run_program_loop(Parent, Port, Buf)
    end.


agent_app_env(Config) ->
    Dir = ?config(priv_dir, Config),
    Vsns = ?config(snmp_versions, Config),
    [{versions,         Vsns}, 
     {agent_type,       master},
     {agent_verbosity,  trace},
     {db_dir,           Dir},
     {audit_trail_log,  [{type, read_write},
			 {dir,  Dir},
			 {size, {10240, 10}}]},
     {config,           [{dir, Dir},
			 {force_load, true},
			 {verbosity,  trace}]}, 
     {local_db,         [{repair,    true},
			 {verbosity,  silence}]}, 
     {mib_server,       [{verbosity, silence}]},
     {symbolic_store,   [{verbosity, silence}]},
     {note_store,       [{verbosity, silence}]},
     {net_if,           [{verbosity, trace}]}].

manager_app_env(Config) ->
    Dir = ?config(priv_dir, Config),
    Vsns = ?config(snmp_versions, Config),
    NetIfModule = ?config(manager_net_if_module, Config),
    [{versions,         Vsns},
     {audit_trail_log,  [{type, read_write},
			 {dir, Dir},
			 {size, {10240, 10}}]},
     {net_if,           [{module, NetIfModule}]},
     {config,           [{dir, Dir},
			 {db_dir, Dir},
			 {verbosity, trace}]}
    ].

oid_str([1 | Ints]) ->
    "iso." ++ oid_str_tl(Ints);
oid_str(Ints) ->
    oid_str_tl(Ints).

oid_str_tl([]) ->
    "";
oid_str_tl([Int]) ->
    integer_to_list(Int);
oid_str_tl([Int | Ints]) ->
    integer_to_list(Int) ++ "." ++ oid_str_tl(Ints).

agent_config(Dir, Transports, Targets, Versions) ->
    EngineID = ?AGENT_ENGINE_ID,
    MMS = ?DEFAULT_MAX_MESSAGE_SIZE,
    ok = snmp_config:write_agent_snmp_conf(Dir, Transports, EngineID, MMS),
    ok = snmp_config:write_agent_snmp_context_conf(Dir),
    ok = snmp_config:write_agent_snmp_community_conf(Dir),
    ok =
	snmp_config:write_agent_snmp_standard_conf(
	  Dir, "snmp_to_snmpnet_SUITE"),
    ok =
	snmp_config:write_agent_snmp_target_addr_conf(
	  Dir, Targets, Versions),
    ok = snmp_config:write_agent_snmp_target_params_conf(Dir, Versions),
    ok = snmp_config:write_agent_snmp_notify_conf(Dir, inform),
    ok = snmp_config:write_agent_snmp_vacm_conf(Dir, Versions, none).

manager_config(Dir, Targets) ->
    EngineID = ?MANAGER_ENGINE_ID,
    MMS = ?DEFAULT_MAX_MESSAGE_SIZE,
    ct:pal("Dir:~p~nTargets:~p",[Dir, Targets]),
    ok = snmp_config:write_manager_snmp_conf(Dir, Targets, MMS, EngineID).

net_snmp_version([v3 | _]) ->
    "-v3";
net_snmp_version([v2 | _]) ->
    "-v2c";
net_snmp_version([v1 | _]) ->
    "-v1".

domain(inet) ->
    transportDomainUdpIpv4;
domain(inet6) ->
    transportDomainUdpIpv6.

net_snmp_addr_str([Target | Targets]) ->
    net_snmp_addr_str(Target) ++
	case Targets of
	    [] ->
		[];
	    [_ | _] ->
		"," ++ net_snmp_addr_str(Targets)
	end;
net_snmp_addr_str({transportDomainUdpIpv4, {Addr, Port}}) ->
    "udp:" ++
	inet_parse:ntoa(Addr) ++ ":" ++
	integer_to_list(Port);
net_snmp_addr_str({transportDomainUdpIpv6, {Addr, Port}}) ->
    "udp6:[" ++
	inet_parse:ntoa(Addr) ++ "]:" ++
	integer_to_list(Port).

domain_suffix(transportDomainUdpIpv4) ->
    "";
domain_suffix(transportDomainUdpIpv6) ->
    "6".

mk_port_number() ->
    {ok, Socket} = gen_udp:open(0, [{reuseaddr, true}]),
    {ok, PortNum} = inet:port(Socket),
    ok = gen_udp:close(Socket),
    PortNum.

join(List) ->
    filename:join(List).
join(Dir, File) ->
    filename:join(Dir, File).

