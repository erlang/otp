%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014-2014. All Rights Reserved.
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
%%

-module(snmp_to_snmpnet_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("snmp/include/STANDARD-MIB.hrl").

-define(AGENT_ENGIN_ID, "ErlangSnmpAgent").
-define(AGENT_PORT, 4000).
-define(MANAGER_PORT, 8989).
-define(DEFAULT_MAX_MESSAGE_SIZE, 484).
-define(SYS_DESC, <<"iso.3.6.1.2.1.1.1.0 = STRING: \"Erlang SNMP agent\"">>).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [
     {group, ipv4},
     {group, ipv6}
    ].

groups() ->
    [{ipv4, [],    [{group, get},
		    {group, inform}
		   ]},
     {ipv6, [],    [{group, get},
		    {group, inform},
		    {group, dual_ip}
		   ]},
     {get, [],     [erlang_agent_netsnmp_get]},
     {inform, [],  [erlang_agent_netsnmp_inform]},
     {dual_ip, [], [erlang_agent_dual_ip_get]}
    ].

init_per_suite(Config) ->
    [{agent_port, ?AGENT_PORT}, {manager_port, ?MANAGER_PORT} | Config].
    
end_per_suite(_Config) ->
    ok.

init_per_group(ipv6, Config) ->
    case ct:require(ipv6_hosts) of
	ok ->
	    Dir = ?config(priv_dir, Config),
	    Domain =  transportDomainUdpIpv6,
	    AgentPort = ?config(agent_port, Config),
	    ManagerPort = ?config(manager_port, Config),
	    {ok, Host} = inet:gethostname(),
	    {ok, IpAddr} = inet:getaddr(Host, inet6),
	    Transports = [{Domain, {IpAddr, AgentPort}}],
	    TrapAddr = {IpAddr, ManagerPort},
	    Versions = [v2],
	    agent_config(Dir, Transports, Domain, TrapAddr, Versions),
	    [{host, Host}, {snmp_versions, Versions}, {ip_version, ipv6}
	     | Config];
	_ ->
	    {skip, "Host does not support IPV6"}
    end;

init_per_group(ipv4, Config) ->
    Dir = ?config(priv_dir, Config),
    Domain =  transportDomainUdpIpv4,
    AgentPort = ?config(agent_port, Config),
    ManagerPort = ?config(manager_port, Config),
    {ok, Host} = inet:gethostname(),
    {ok, IpAddr} = inet:getaddr(Host, inet),
    Transports = [{Domain, {IpAddr, AgentPort}}],
    TrapAddr = {IpAddr, ManagerPort},
    Versions = [v2],
    agent_config(Dir, Transports, Domain, TrapAddr, Versions),
    [{host, Host}, {snmp_versions, Versions}, {ip_version, ipv4}
     | Config];

init_per_group(dual_ip, Config) ->
    case os:find_executable("snmpget") of
	false ->
	    {skip, "snmpget not found"};
	Path ->
	    case ct:require(ipv6_hosts) of
		ok ->
		    Dir = ?config(priv_dir, Config),
		    {ok, Host} = inet:gethostname(),
		    {ok, IPv4Addr} = inet:getaddr(Host, inet),
		    {ok, IPv6Addr} = inet:getaddr(Host, inet6),
		    Domain = snmpUDPDomain,
		    Transports =
			[{Domain, {IPv4Addr, ?AGENT_PORT}},
			 {transportDomainUdpIpv6, {IPv6Addr, ?AGENT_PORT}}],
		    TrapAddr = {IPv4Addr, 0},
		    Versions = [v2],
		    agent_config(
		      Dir, Transports, Domain, TrapAddr, Versions),
		    [{host, Host}, {port, ?AGENT_PORT},
		     {snmp_versions, Versions},
		     {snmpget, Path} | Config];
		_ ->
		    {skip, "Host does not support IPV6"}
	    end
    end;

init_per_group(get, Config) ->
    %% From Ubuntu package snmp
    case os:find_executable("snmpget") of
	false ->
	    {skip, "snmpget not found"};
	Path ->
	    [{snmpget, Path} | Config]
    end;

init_per_group(inform, Config) ->
    %% From Ubuntu package snmptrapfmt
    case os:find_executable("snmptrapd") of
	false ->
	    {skip, "snmptrapd not found"};
	Path ->
	    [{snmptrapd, Path} | Config]
    end;
init_per_group(_, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_Case, Config) ->
    Dog = ct:timetrap(10000),
    application:stop(snmp),
    application:unload(snmp),
    [{watchdog, Dog} | Config].

end_per_testcase(_, Config) ->
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

start_agent(Config) ->
    ok = application:load(snmp),
    ok = application:set_env(snmp, agent, app_env(Config)),
    ok = application:start(snmp).

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
erlang_agent_netsnmp_get() ->
    [{doc,"Test that we can access erlang snmp agent " 
      "from snmpnet manager"}].

erlang_agent_netsnmp_get(Config) when is_list(Config) ->
    start_agent(Config),
    ?SYS_DESC = snmpget(oid_str(?sysDescr_instance), Config),
    ok.

%%--------------------------------------------------------------------
erlang_agent_dual_ip_get() ->
    [{doc,"Test that we can access erlang snmp agent from both " 
      "snmpnet ipv4 and snmpnet ipv6 manager at the same time"}].
erlang_agent_dual_ip_get(Config) when is_list(Config) ->
    start_agent(Config),

    OidStr = oid_str(?sysDescr_instance),
    ?SYS_DESC = snmpget(OidStr, [{ip_version, ipv4} | Config]),
    ?SYS_DESC = snmpget(OidStr, [{ip_version, ipv6} | Config]),
    ok.

%%--------------------------------------------------------------------
erlang_agent_netsnmp_inform(Config) when is_list(Config) ->
    Host = ?config(host, Config),
    IPVersion = ?config(ip_version, Config),    
    DataDir = ?config(data_dir, Config),  

    start_agent(Config),
    ok =
      snmpa:load_mib(
	snmp_master_agent, filename:join(DataDir, "TestTrapv2")),
    
    SnmptrapdArgs =
	["-f", "-Lo",
	 "-M", DataDir,
	 "--disableAuthorization=yes",
	 "--snmpTrapdAddr=" ++ net_snmp_transport(IPVersion) ++
	     Host ++ ":" ++ integer_to_list(?config(manager_port, Config))],
    {ok, CheckMP} = re:compile("NET-SNMP version ", [anchored]),
    ProgHandle =
	start_program(snmptrapd, SnmptrapdArgs, CheckMP, Config),

    snmpa:send_notification(
      snmp_master_agent, testTrapv22, {erlang_agent_test, self()}),

    receive
	{snmp_targets, erlang_agent_test, Addresses} ->
	    ct:pal("Notification sent to: ~p~n", [Addresses])
    end,
    receive
	{snmp_notification, erlang_agent_test, {got_response, Address}} ->
	    ct:pal("Got response from: ~p~n", [Address]),
	    ok;
	{snmp_notification, erlang_agent_test, {no_response, _} = 
	     NoResponse} ->
	    ct:fail(NoResponse)
    end,

    stop_program(ProgHandle).
	
%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
snmpget(OidStr, Config) ->
    Versions = ?config(snmp_versions, Config),
    IPVersion = ?config(ip_version, Config),
    Host = ?config(host, Config),
    Port = ?config(agent_port, Config),

    Args =
	["-c", "public", net_snmp_version(Versions),
	 net_snmp_transport(IPVersion) ++
	     Host ++ ":" ++ integer_to_list(Port),
	 OidStr],
    ProgHandle = start_program(snmpget, Args, none, Config),
    {_, line, Line} = get_program_output(ProgHandle),
    stop_program(ProgHandle),
    Line.


start_program(Prog, Args, StartCheckMP, Config) ->
    Path = ?config(Prog, Config),
    DataDir = ?config(data_dir, Config),
    StartWrapper = filename:join(DataDir, "start_stop_wrapper"),
    Parent = self(),
    Pid =
	spawn_link(
	  fun () ->
		  run_program(Parent, StartWrapper, [Path | Args])
	  end),
    start_check(Pid, erlang:monitor(process, Pid), StartCheckMP).

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
    Port =
	open_port(
	  {spawn_executable, StartWrapper},
	  [{args, ProgAndArgs}, binary, stderr_to_stdout, {line, 80},
	   exit_status]),
    ct:pal("Prog ~p started: ~p", [Port, ProgAndArgs]),
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
		    Line = iolist_to_binary(lists:reverse(Buf, Data)),
		    ct:pal("Prog ~p output: ~s", [Port, Line]),
		    Parent ! {self(), line, Line},
		    run_program_loop(Parent, Port, []);
		noeol ->
		    run_program_loop(Parent, Port, [Data | Buf])
	    end;
	{Port, {exit_status,ExitStatus}} ->
	    ct:pal("Prog ~p exit: ~p", [Port, ExitStatus]),
	    catch port_close(Port),
	    Parent ! {self(), exit, ExitStatus};
	Unexpected ->
	    ct:pal("run_program_loop Unexpected: ~p", [Unexpected]),
	    run_program_loop(Parent, Port, Buf)
    end.


app_env(Config) ->
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

oid_str([Int | Rest]) ->
    oid_str(Rest, integer_to_list(Int)).

oid_str([], Acc) ->
    Acc;
oid_str([Int | Rest], Acc) ->
    oid_str(Rest, Acc ++ "." ++ integer_to_list(Int)).

agent_config(Dir, Transports, TargetDomain, TargetAddr, Versions) ->
    EngineID = ?AGENT_ENGIN_ID,
    MMS = ?DEFAULT_MAX_MESSAGE_SIZE,
    ok = snmp_config:write_agent_snmp_conf(Dir, Transports, EngineID, MMS),
    ok = snmp_config:write_agent_snmp_context_conf(Dir),
    ok = snmp_config:write_agent_snmp_community_conf(Dir),
    ok =
	snmp_config:write_agent_snmp_standard_conf(
	  Dir, "snmp_to_snmpnet_SUITE"),
    ok =
	snmp_config:write_agent_snmp_target_addr_conf(
	  Dir, TargetDomain, TargetAddr, Versions),
    ok = snmp_config:write_agent_snmp_target_params_conf(Dir, Versions),
    ok = snmp_config:write_agent_snmp_notify_conf(Dir, inform),
    ok = snmp_config:write_agent_snmp_vacm_conf(Dir, Versions, none).

net_snmp_version([v3 | _]) ->
    "-v3";
net_snmp_version([v2 | _]) ->
    "-v2c";
net_snmp_version([v1 | _]) ->
    "-v1".

net_snmp_transport(ipv4) ->
    "udp:";
net_snmp_transport(ipv6) ->
    "udp6:".
