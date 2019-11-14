%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014-2018. All Rights Reserved.
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
         suite/0, all/0, groups/0,
         init_per_suite/1,    end_per_suite/1,
         init_per_group/2,    end_per_group/2, 
         init_per_testcase/2, end_per_testcase/2,

         erlang_agent_netsnmp_get/0,    erlang_agent_netsnmp_get/1,
         erlang_agent_netsnmp_inform/0, erlang_agent_netsnmp_inform/1,
         erlang_manager_netsnmp_get/0,  erlang_manager_netsnmp_get/1

        ]).

-include("snmp_test_lib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("snmp/include/STANDARD-MIB.hrl").

-define(AGENT_ENGINE_ID, "ErlangSnmpAgent").
-define(MANAGER_ENGINE_ID, "ErlangSnmpManager").
-define(AGENT_PORT, 4000).
-define(MANAGER_PORT, 8989).
-define(DEFAULT_MAX_MESSAGE_SIZE, 484).

expected(?sysDescr_instance = Oid, get) ->
    OidStr = oid_str(Oid),
    iolist_to_binary([OidStr | " = STRING: \"Erlang SNMP agent\""]).


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

init_per_suite(Config) ->
    try
        begin
            Config2 = ?LIB:init_per_suite(netsnmp_init(Config)),
            snmp_test_sys_monitor:start(),
            Config2
        end
    catch
        throw:{skip, _} = SKIP ->
            SKIP
    end.

netsnmp_init(Config) ->
    case has_netsnmp() of
        true ->
            case proper_netsnmp_version() of
                true ->
                    [{agent_port,   ?AGENT_PORT},
                     {manager_port, ?MANAGER_PORT} | Config];
                false ->
                    throw({skip, "Buggy NetSNMP"})
            end;
        false ->
            throw({skip, "No NetSNMP"})
    end.

has_netsnmp() ->
    netsnmp_check("NET-SNMP").

proper_netsnmp_version() ->
    not netsnmp_check("5.4|5.6.2.1").

netsnmp_check(RE) ->
    case re:run(os:cmd("snmpd -v"), RE, [{capture, first}]) of
        nomatch ->
            false;
        {match, _} ->
            true
    end.


end_per_suite(Config) ->
    snmp_test_sys_monitor:stop(),
    ?LIB:end_per_suite(Config).


%%
%% -----
%%

init_per_group(ipv4, Config) ->
    init_per_group_ip([inet], Config);
init_per_group(ipv6, Config) ->
    init_per_group_ipv6([inet6], Config);
init_per_group(ipv4_ipv6, Config) ->
    init_per_group_ipv6([inet, inet6], Config);

init_per_group(snmpget = Exec, Config) ->
    %% From Ubuntu package snmp
    init_per_group_agent(Exec, Config);
init_per_group(snmptrapd = Exec, Config) ->
    %% From Ubuntu package snmpd
    init_per_group_agent(Exec, Config);
init_per_group(snmpd_mt, Config) ->
    %% From Ubuntu package snmp
    init_per_group_manager(
      snmpd,
      [{manager_net_if_module, snmpm_net_if_mt} | Config]);
init_per_group(snmpd = Exec, Config) ->
    %% From Ubuntu package snmp
    init_per_group_manager(
      Exec,
      [{manager_net_if_module, snmpm_net_if} | Config]);

init_per_group(_, Config) ->
    Config.

init_per_group_ipv6(Families, Config) ->
    case ?LIB:has_support_ipv6() of
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

init_per_testcase(_Case, Config) ->

    snmp_test_global_sys_monitor:reset_events(),

    Dog = ct:timetrap(20000),
    application:stop(snmp),
    application:unload(snmp),
    [{watchdog, Dog} | Config].

end_per_testcase(_, Config) ->

    ?PRINT2("system events during test: "
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
    case os:find_executable(filename:join(["/" | Dir] ++ [ExecStr])) of
	false ->
	    find_sys_executable(Exec, ExecStr, Dirs, Config);
	Path ->
	    [{Exec, Path} | Config]
    end.

start_agent(Config) ->
    ok = application:load(snmp),
    ok = application:set_env(snmp, agent, agent_app_env(Config)),
    ok = application:start(snmp).

start_manager(Config) ->
    ok = application:load(snmp),
    ok = application:set_env(snmp, manager, manager_app_env(Config)),
    ok = application:start(snmp).



%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
erlang_agent_netsnmp_get() ->
    [{doc,"Test that we can access erlang snmp agent " 
      "from snmpnet manager"}].

erlang_agent_netsnmp_get(Config) when is_list(Config) ->
    Transports = ?config(transports, Config),
    start_agent(Config),
    Oid = ?sysDescr_instance,
    Expected = expected(Oid, get),
    try
        begin
            [Expected = snmpget(Oid, Transport, Config)
             || Transport <- Transports],
            ok
        end
    catch
        throw:{skip, _} = SKIP ->
            SKIP
    end.


%%--------------------------------------------------------------------
erlang_manager_netsnmp_get() ->
    [{doc,"Test that the erlang snmp manager can access snmpnet agent"}].

erlang_manager_netsnmp_get(Config) when is_list(Config) ->
    Community  = "happy-testing",
    SysDescr   = "Net-SNMP agent",
    TargetName = "Target Net-SNMP agent",
    Transports = ?config(transports, Config),
    case start_snmpd(Community, SysDescr, Config) of
        {skip, _} = SKIP ->
            SKIP;
        ProgHandle ->
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
                [snmp_manager_user:sync_get(
                   TargetName++domain_suffix(Domain),
                   [?sysDescr_instance])
                 || {Domain, _} <- Transports],
            ct:pal("sync_get -> ~p", [Results]),
            snmp_manager_user:stop(),
            stop_program(ProgHandle),
            [{ok,
              {noError, 0,
               [{varbind, ?sysDescr_instance, 'OCTET STRING', SysDescr,1}] },
              _} = R || R <- Results],
            ok
    end.


%%--------------------------------------------------------------------
erlang_agent_netsnmp_inform() ->
    [{doc, "TBD"}].

erlang_agent_netsnmp_inform(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    Mib = "TestTrapv2",

    start_agent(Config),
    ok = snmpa:load_mib(snmp_master_agent, filename:join(DataDir, Mib)),

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
            Line
    end.

start_snmptrapd(Mibs, Config) ->
    DataDir = ?config(data_dir, Config),
    MibDir = filename:join(code:lib_dir(snmp), "mibs"),
    Targets = ?config(targets, Config),
    SnmptrapdArgs =
	["-f", "-Lo", "-C",
	 "-m", Mibs,
	 "-M", MibDir++":"++DataDir,
	 "--disableAuthorization=yes",
	 "--snmpTrapdAddr=" ++ net_snmp_addr_str(Targets)],
    {ok, StartCheckMP} = re:compile("NET-SNMP version ", [anchored]),
    start_program(snmptrapd, SnmptrapdArgs, StartCheckMP, Config).

start_snmpd(Community, SysDescr, Config) ->
    DataDir = ?config(data_dir, Config),
    Targets = ?config(targets, Config),
    Transports = ?config(transports, Config),
    Port = mk_port_number(),
    CommunityArgs =
	["--rocommunity"++domain_suffix(Domain)++"="
	 ++Community++" "++inet_parse:ntoa(Ip)
	 || {Domain, {Ip, _}} <- Targets],

    SnmpdArgs =
        ["-f", "-r", %"-Dverbose",
         "-c", filename:join(DataDir, "snmpd.conf"),
         "-C",
         "-Lo",
	 "-m", ":",
	 "--sysDescr="++SysDescr,
	 "--agentXSocket=tcp:localhost:"++integer_to_list(Port)]
	++ CommunityArgs
	++ [net_snmp_addr_str(Transports)],
    {ok, StartCheckMP} = re:compile("NET-SNMP version ", [anchored]),
    start_program(snmpd, SnmpdArgs, StartCheckMP, Config).

start_program(Prog, Args, StartCheckMP, Config) ->
    ct:pal("Starting program: ~w ~p", [Prog, Args]),
    Path = ?config(Prog, Config),
    DataDir = ?config(data_dir, Config),
    StartWrapper = filename:join(DataDir, "start_stop_wrapper"),
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

