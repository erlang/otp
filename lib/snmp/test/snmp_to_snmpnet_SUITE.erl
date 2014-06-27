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
	    {ok, Host} = inet:gethostname(),
	    {ok, IpAddr} = inet:getaddr(Host, inet6),
	    Versions = [v2],
	    agent_config(Dir, Domain, IpAddr, IpAddr, ?config(agent_port, Config), Versions),
	    [{host, Host}, 
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
    agent_config(Dir, Domain, IpAddr, {IpAddr, ?config(manager_port, Config)}, 
		 ?config(agent_port, Config), Versions),
    [{host, Host}, {snmp_versions, Versions},
     {ip_version, ipv4} | Config];

init_per_group(get, Config) ->
    %% From Ubuntu package snmp
    case os:find_executable("snmpget") of
	false ->
	    {skip, "snmpget not found"};
	_ ->
	    Config
    end;

init_per_group(inform, Config) ->
    %% From Ubuntu package snmptrapfmt
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
    Dog = ct:timetrap(10000),
    end_per_testcase(Case, Config),
    application:start(snmp),
    application:load(snmp),
    application:set_env(snmp, agent, app_env(Case, Config)),
    snmp:start_agent(normal),
    [{watchdog, Dog} | Config].

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
    Port = ?config(agent_port, Config),
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
erlang_agent_netsnmp_inform(Config) when is_list(Config) ->
    Host = ?config(host, Config),
    IPVersion = ?config(ip_version, Config),    
    DataDir = ?config(data_dir, Config),  
    ok = snmpa:load_mib(snmp_master_agent, filename:join(DataDir, "TestTrapv2")),
    
    Cmd = "snmptrapd -L o -M " ++ DataDir ++ 
	" --disableAuthorization=yes" ++
	" --snmpTrapdAddr=" ++ net_snmp_ip_version(IPVersion) ++ 
	Host ++ ":" ++ integer_to_list(?config(manager_port, Config)),

    NetSnmpPort = net_snmp_trapd(Cmd),
    snmpa:send_notification(snmp_master_agent, testTrapv22, 
			    {erlang_agent_test, self()}),
    net_snmp_log(NetSnmpPort),
    receive
	{snmp_targets, erlang_agent_test, Addresses} ->
	    ct:pal("Notification sent to: ~p~n", [Addresses])
    end,
    receive
	{snmp_notification, erlang_agent_test, {got_response, Address}} ->
	    ct:pal("Got respons from: ~p~n", [Address]),
	    ok;
	{snmp_notification, erlang_agent_test, {no_response, _} = 
	     NoResponse} ->
	    ct:fail(NoResponse)
    end.
	
%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
net_snmp(Cmd, Expect) ->
    NetSnmpPort = open_port({spawn, Cmd}, [stderr_to_stdout]),
    receive 
	{NetSnmpPort, {data, Expect}} ->
	    ok;
	Msg ->
	    ct:fail({{expected, {NetSnmpPort, {data, Expect}}}, 
		     {got, Msg}})
    end,
    catch erlang:port_close(NetSnmpPort).

net_snmp_trapd(Cmd) ->
    open_port({spawn, Cmd}, [stderr_to_stdout]).

net_snmp_log(NetSnmpPort) -> 
    receive 
	{NetSnmpPort, {data, Data}} ->
	    ct:pal("Received from netsnmp: ~p~n", [Data]),
	    net_snmp_log(NetSnmpPort)
    after 500 ->
	    catch erlang:port_close(NetSnmpPort)
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
    ok = snmp_config:write_agent_snmp_conf(Dir, Domain, {IpA, Port},
				      EngineID, MMS),
    ok = snmp_config:write_agent_snmp_context_conf(Dir),
    ok = snmp_config:write_agent_snmp_community_conf(Dir),
    ok = snmp_config:write_agent_snmp_standard_conf(Dir, "snmp_to_snmpnet_SUITE"),
    ok = snmp_config:write_agent_snmp_target_addr_conf(Dir, Domain, 
						  IpM, Versions),
    ok = snmp_config:write_agent_snmp_target_params_conf(Dir, Versions),
    ok = snmp_config:write_agent_snmp_notify_conf(Dir, inform),
    ok = snmp_config:write_agent_snmp_vacm_conf(Dir, Versions, none).

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
