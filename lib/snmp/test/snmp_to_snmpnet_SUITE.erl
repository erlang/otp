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
-define(AGENT_PORT,       4000).
-define(DEFAULT_MAX_MESSAGE_SIZE, 484).
-define(SYS_DESC, "iso.3.6.1.2.1.1.1.0 = STRING: \"Erlang SNMP agent\"\n").

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
    [{ipv4, [], ipv4_tests()},
     {ipv6, [], ipv6_tests()},
     {get, [], get_tests()},
     %%{trap, [], trap_tests()},
     {dual_ip, [], dual_ip_tests()}].

get_tests() ->
    [erlang_agent_netsnmp_get].
trap_tests() ->
    [erlang_agent_netsnmp_trap].
dual_ip_tests() ->
    [erlang_agent_dual_ip_get].
ipv4_tests() ->
    [{group, get}].
ipv6_tests() ->
    [{group, get},
     {group, dual_ip}
    ].

init_per_suite(Config) ->
    case os:find_executable("snmpget") of
	false ->
	    {skip, "snmpget not found"};
	_ ->
	    Config
    end.

end_per_suite(_Config) ->
    ok.

init_per_group(ipv6, Config) ->
    case ct:require(ipv6_hosts) of
	ok ->
	    Dir = ?config(priv_dir, Config),
	    Domain =  transportDomainUdpIpv6,
	    {ok, Host} = inet:gethostname(),
	    {ok, IpAddr} = inet:getaddr(Host, inet6),
	    Versions = [v2],
	    agent_config(Dir, Domain, IpAddr, IpAddr, ?AGENT_PORT, Versions),
	    [{host, Host}, {port, ?AGENT_PORT}, 
	     {snmp_versions, Versions}, {ip_version, ipv6} | Config];	
	_ ->
	    {skip, "Host does not support IPV6"}
    end;

init_per_group(ipv4, Config) ->
    Dir = ?config(priv_dir, Config),
    Domain =  transportDomainUdpIpv4,
    {ok, Host} = inet:gethostname(),
    {ok, IpAddr} = inet:getaddr(Host, inet),
    Versions = [v2],
    agent_config(Dir, Domain, IpAddr, IpAddr, ?AGENT_PORT, Versions),
    [{host, Host}, {port, ?AGENT_PORT}, {snmp_versions, Versions},
     {ip_version, ipv4} | Config];

init_per_group(get, Config) ->
    case os:find_executable("snmpget") of
	false ->
	    {skip, "snmpget not found"};
	_ ->
	    Config
    end;

init_per_group(trap, Config) ->
    case os:find_executable("snmptrapd") of
	false ->
	    {skip, "snmptrapd not found"};
	_ ->
	    Config
    end;
init_per_group(_, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(Case, Config) ->
    end_per_testcase(Case, Config),
    application:start(snmp),
    application:load(snmp),
    application:set_env(snmp, agent, app_env(Case, Config)),
    snmp:start_agent(normal),
    Config.

end_per_testcase(_, Config) ->
    application:stop(snmp),
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
erlang_agent_netsnmp_get() ->
    [{doc,"Test that we can access erlang snmp agent " 
      "from snmpnet manager"}].

erlang_agent_netsnmp_get(Config) when is_list(Config) ->
    Host = ?config(host, Config),
    Port = ?config(port, Config),
    IPVersion = ?config(ip_version, Config),
    Versions = ?config(snmp_versions, Config),

    Cmd = "snmpget -c public " ++ net_snmp_version(Versions) ++ " " ++
	net_snmp_ip_version(IPVersion) ++ 
	Host ++ ":" ++ integer_to_list(Port) ++ 
	" " ++ oid_str(?sysDescr_instance),
    net_snmp(Cmd, ?SYS_DESC).

%%--------------------------------------------------------------------
erlang_agent_dual_ip_get() ->
    [{doc,"Test that we can access erlang snmp agent from both " 
      "snmpnet ipv4 and snmpnet ipv6 manager at the same time"}].
erlang_agent_dual_ip_get(Config) when is_list(Config) ->
    erlang_agent_netsnmp_get([{ip_version, ipv4}]),
    erlang_agent_netsnmp_get([{ip_version, ipv6}]).
%%--------------------------------------------------------------------
erlang_agent_netsnmp_trap() ->
    %% Host = ?config(host, Config),
    %% Port = ?config(port, Config),
    %% IPVersion = ?config(ip_version, Config),
    %% Versions = ?config(snmp_versions, Config),
    
    Cmd = "",
    net_snmp(Cmd, "").

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
net_snmp(Cmd, Expect) ->
    SnmpNetPort = open_port({spawn, Cmd}, [stderr_to_stdout]), 
    receive 
	{SnmpNetPort, {data, Expect}} ->
	    ok;
	Msg ->
	    ct:fail({{expected, {SnmpNetPort, {data, Expect}}}, 
		     {got, Msg}})
    end.

app_env(_Case, Config) ->
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
			 {force_load, false}, 
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

agent_config(Dir, Domain, IpA, IpM, Port, Versions) ->
    EngineID = ?AGENT_ENGIN_ID,
    MMS = ?DEFAULT_MAX_MESSAGE_SIZE,
    snmp_config:write_agent_snmp_conf(Dir, Domain, {IpA, Port},
				      EngineID, MMS),
    snmp_config:write_agent_snmp_context_conf(Dir),
    snmp_config:write_agent_snmp_community_conf(Dir),
    snmp_config:write_agent_snmp_standard_conf(Dir, "snmp_to_snmpnet_SUITE"),
    snmp_config:write_agent_snmp_target_addr_conf(Dir, Domain, 
						  IpM, Versions),
    snmp_config:write_agent_snmp_target_params_conf(Dir, Versions),
    snmp_config:write_agent_snmp_notify_conf(Dir, inform),
    snmp_config:write_agent_snmp_vacm_conf(Dir, Versions, none).

net_snmp_version([v3 | _]) ->
    "-v3";
net_snmp_version([v2 | _]) ->
    "-v2c";
net_snmp_version([v1 | _]) ->
    "-v1".
net_snmp_ip_version(ipv4) ->
    "udp:";
net_snmp_ip_version(ipv6) ->
    "udp6:".
