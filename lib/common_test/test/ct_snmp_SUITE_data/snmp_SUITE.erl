%%--------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2018. All Rights Reserved.
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
%% File: ct_snmp_SUITE.erl
%%
%% Description:
%%    This file contains the test cases for the ct_snmp API.
%%
%% Test  of SNMP support in common_test
%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
-module(snmp_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("snmp/include/STANDARD-MIB.hrl").
-include_lib("snmp/include/SNMP-USER-BASED-SM-MIB.hrl").
-include_lib("snmp/include/snmp_types.hrl").

-compile(export_all).

%% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).

-define(AGENT_UDP, 4000).

suite() ->
    [
     {require, snmp1, snmp1},
     {require, snmp_app1, snmp_app1},
     {require, snmp2, snmp2},
     {require, snmp_app2, snmp_app2},
     {require, snmp3, snmp3}
    ].

all() ->
    [start_stop,
     {group,get_set},
     {group,register},
     {group,override},
     set_info].


groups() ->
    [{get_set,[get_values,
	       get_next_values,
	       set_values,
	       load_mibs]},
     {register,[register_users,
		register_users_fail,
		register_agents,
		register_agents_fail,
		register_usm_users,
		register_usm_users_fail]},
     {override,[override_usm,
		override_standard,
		override_context,
		override_community,
		override_notify,
		override_target_addr,
		override_target_params,
		override_vacm]}].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_group(get_set, Config) ->
    ok = ct_snmp:start(Config,snmp1,snmp_app1),
    Config;
init_per_group(register, Config) ->
    ok = ct_snmp:start(Config,snmp2,snmp_app2),
    Config;
init_per_group(_, Config) ->
    ok = ct_snmp:start(Config,snmp3,snmp_app2),
    Config.

end_per_group(_Group, Config) ->
    catch ct_snmp:stop(Config),
    Config.

init_per_testcase(_Case, Config) ->
    Dog = test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].

end_per_testcase(Case, Config) ->
    try apply(?MODULE,Case,[cleanup,Config])
    catch error:undef -> ok
    end,
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

%%%-----------------------------------------------------------------
%%% Test cases
break(_Config) ->
    test_server:break(""),
    ok.

start_stop(Config) ->
    ok = ct_snmp:start(Config,snmp1,snmp_app1),
    ct:sleep(1000),
    {snmp,_,_} = lists:keyfind(snmp,1,application:which_applications()),
    [_|_] = filelib:wildcard("*/*.conf",?config(priv_dir,Config)),

    ok = ct_snmp:stop(Config),
    ct:sleep(1000),
    false = lists:keyfind(snmp,1,application:which_applications()),
    [] = filelib:wildcard("*/*.conf",?config(priv_dir,Config)),
    ok.

get_values(_Config) ->
    Oids1 = [?sysDescr_instance, ?sysName_instance],
    {noError,_,V1} = ct_snmp:get_values(agent_name,Oids1,snmp1),
    [#varbind{oid=?sysDescr_instance,value="Erlang SNMP agent"},
     #varbind{oid=?sysName_instance,value="ct_test"}] = V1,
    ok.

get_next_values(_Config) ->
    Oids2 = [?system],
    {noError,_,V2} = ct_snmp:get_next_values(agent_name,Oids2,snmp1),
    [#varbind{oid=?sysDescr_instance,value="Erlang SNMP agent"}] = V2,
    ok.

set_values(Config) ->
    Oid3 = ?sysName_instance,
    NewName = "ct_test changed by " ++ atom_to_list(?MODULE),
    VarsAndVals = [{Oid3,s,NewName}],
    {noError,_,_} =
	ct_snmp:set_values(agent_name,VarsAndVals,snmp1,Config),

    Oids4 = [?sysName_instance],
    {noError,_,V4} = ct_snmp:get_values(agent_name,Oids4,snmp1),
    [#varbind{oid=?sysName_instance,value=NewName}] = V4,

    ok.

load_mibs(_Config) ->
    [{'SNMPv2-MIB',_}=SnmpV2Mib] = snmpa:which_mibs(),
    Mib = filename:join([code:priv_dir(snmp),"mibs","SNMP-USER-BASED-SM-MIB"]),
    ok = ct_snmp:load_mibs([Mib]),
    TwoMibs = [_,_] = snmpa:which_mibs(),
    [{'SNMP-USER-BASED-SM-MIB',_}] = lists:delete(SnmpV2Mib,TwoMibs),
    ok = ct_snmp:unload_mibs([Mib]),
    [{'SNMPv2-MIB',_}] = snmpa:which_mibs(),
    ok.


register_users(_Config) ->
    [] = snmpm:which_users(),
    ok = ct_snmp:register_users(snmp2,[{reg_user1,[snmpm_user_default,[]]}]),
    [_] = snmpm:which_users(),
    [_] = ct:get_config({snmp2,users}),
    ok = ct_snmp:register_users(snmp2,[{reg_user2,[snmpm_user_default,[]]}]),
    [_,_] = snmpm:which_users(),
    [_,_] = ct:get_config({snmp2,users}),
    ok = ct_snmp:register_users(snmp2,[{reg_user3,[snmpm_user_default,[]]}]),
    [_,_,_] = snmpm:which_users(),
    [_,_,_] = ct:get_config({snmp2,users}),
    ok = ct_snmp:unregister_users(snmp2,[reg_user3]),
    [_,_] = snmpm:which_users(),
    [_,_] = ct:get_config({snmp2,users}),
    ok = ct_snmp:unregister_users(snmp2),
    [] = snmpm:which_users(),
    [] = ct:get_config({snmp2,users}),
    ok.
register_users(cleanup,_Config) ->
    ct_snmp:unregister_users(snmp2).

register_users_fail(_Config) ->
    [] = snmpm:which_users(),
    {error,_} = ct_snmp:register_users(snmp2,[{reg_user3,[unknown_module,[]]}]),
    [] = snmpm:which_users(),
    ok.
register_users_fail(cleanup,_Config) ->
    ct_snmp:unregister_users(snmp2).

register_agents(_Config) ->
    {ok, HostName} = inet:gethostname(),
    {ok, Addr} = inet:getaddr(HostName, inet),

    [] = snmpm:which_agents(),
    ok = ct_snmp:register_users(snmp2,[{reg_user1,[snmpm_user_default,[]]}]),
    ok = ct_snmp:register_agents(snmp2,[{reg_agent1,
					 [reg_user1,Addr,?AGENT_UDP,[]]}]),
    [_] = snmpm:which_agents(),
    [_] = ct:get_config({snmp2,managed_agents}),
    ok = ct_snmp:register_agents(snmp2,[{reg_agent2,
					 [reg_user1,Addr,?AGENT_UDP,[]]}]),
    [_,_] = snmpm:which_agents(),
    [_,_] = ct:get_config({snmp2,managed_agents}),

    ok = ct_snmp:register_agents(snmp2,[{reg_agent3,
					 [reg_user1,Addr,?AGENT_UDP,[]]}]),
    [_,_,_] = snmpm:which_agents(),
    [_,_,_] = ct:get_config({snmp2,managed_agents}),

    ok = ct_snmp:unregister_agents(snmp2,[reg_agent3]),
    [_,_] = snmpm:which_agents(),
    [_,_] = ct:get_config({snmp2,managed_agents}),

    ok = ct_snmp:unregister_agents(snmp2),
    ok = ct_snmp:unregister_users(snmp2),
    [] = snmpm:which_agents(),
    [] = ct:get_config({snmp2,managed_agents}),
    ok.
register_agents(cleanup,_Config) ->
    ct_snmp:unregister_agents(snmp2),
    ct_snmp:unregister_users(snmp2).

register_agents_fail(_Config) ->
    {ok, HostName} = inet:gethostname(),
    {ok, Addr} = inet:getaddr(HostName, inet),

    [] = snmpm:which_agents(),
    {error,_}
	= ct_snmp:register_agents(snmp2,[{reg_agent3,
					  [unknown_user,Addr,?AGENT_UDP,[]]}]),
    [] = snmpm:which_agents(),
    ok.
register_agents_fail(cleanup,_Config) ->
    ct_snmp:unregister_agents(snmp2).

register_usm_users(_Config) ->
    [] = snmpm:which_usm_users(),
    ok = ct_snmp:register_usm_users(snmp2,[{"reg_usm_user1",[]}]),
    [_] = snmpm:which_usm_users(),
    [_] = ct:get_config({snmp2,usm_users}),
    ok = ct_snmp:register_usm_users(snmp2,[{"reg_usm_user2",[]}]),
    [_,_] = snmpm:which_usm_users(),
    [_,_] = ct:get_config({snmp2,usm_users}),
    ok = ct_snmp:register_usm_users(snmp2,[{"reg_usm_user3",[]}]),
    [_,_,_] = snmpm:which_usm_users(),
    [_,_,_] = ct:get_config({snmp2,usm_users}),
    ok = ct_snmp:unregister_usm_users(snmp2,["reg_usm_user3"]),
    [_,_] = snmpm:which_usm_users(),
    [_,_] = ct:get_config({snmp2,usm_users}),
    ok = ct_snmp:unregister_usm_users(snmp2),
    [] = snmpm:which_usm_users(),
    [] = ct:get_config({snmp2,usm_users}),
    ok.
register_usm_users(cleanup,_Config) ->
    ct_snmp:unregister_usm_users(snmp2).

register_usm_users_fail(_Config) ->
    [] = snmpm:which_usm_users(),
    {error,_}
	= ct_snmp:register_usm_users(snmp2,[{"reg_usm_user3",
					     [{sec_name,invalid_data_type}]}]),
    [] = snmpm:which_usm_users(),
    ok.
register_usm_users_fail(cleanup,_Config) ->
    ct_snmp:unregister_usm_users(snmp2).

%% Test that functionality for overriding default configuration file
%% works - i.e. that the files are written and that the configuration
%% is actually used.
%%
%% Note that the config files used in this test case do not
%% necessarily make up a reasonable configuration for the snmp
%% application...
override_usm(Config) ->
    DataDir = ?config(data_dir,Config),
    PrivDir = ?config(priv_dir,Config),
    ConfDir = filename:join(PrivDir,"conf"),

    Mib = filename:join([code:priv_dir(snmp),"mibs","SNMP-USER-BASED-SM-MIB"]),
    ok = ct_snmp:load_mibs([Mib]),

    %% Check that usm.conf is overwritten
    {ok,MyUsm} = snmpa_conf:read_usm_config(DataDir),
    {ok,UsedUsm} = snmpa_conf:read_usm_config(ConfDir),
    ct:pal(
      "MyUsm = ~p~nUsedUsm = ~p",
      [MyUsm, UsedUsm]),
    true = (MyUsm == UsedUsm),

    %% Check that the usm user is actually configured...
    [{Index,"secname"}] =
	snmp_user_based_sm_mib:usmUserTable(get_next,?usmUserEntry,[3]),
    true = lists:suffix("usm_user_name",Index),
    ok.

override_standard(Config) ->
    DataDir = ?config(data_dir,Config),
    PrivDir = ?config(priv_dir,Config),
    ConfDir = filename:join(PrivDir,"conf"),

    %% Check that standard.conf is overwritten
    {ok,MyStandard} = snmpa_conf:read_standard_config(DataDir),
    {ok,UsedStandard} = snmpa_conf:read_standard_config(ConfDir),
    ct:pal(
      "MyStandard = ~p~nUsedStandard = ~p",
      [MyStandard, UsedStandard]),
    true = (MyStandard == UsedStandard),

    %% Check that the values from standard.conf is actually configured...
    {value,"name for override test"} = snmp_standard_mib:sysName(get),
    {value,"agent for ct_snmp override test"} = snmp_standard_mib:sysDescr(get),
    ok.

override_context(Config) ->
    DataDir = ?config(data_dir,Config),
    PrivDir = ?config(priv_dir,Config),
    ConfDir = filename:join(PrivDir,"conf"),

    %% Check that context.conf is overwritten
    {ok,MyContext} = snmpa_conf:read_context_config(DataDir),
    {ok,UsedContext} = snmpa_conf:read_context_config(ConfDir),
    ct:pal(
      "MyContext = ~p~nUsedContext = ~p",
      [MyContext, UsedContext]),
    true = (MyContext == UsedContext),
    ok.

override_community(Config) ->
    DataDir = ?config(data_dir,Config),
    PrivDir = ?config(priv_dir,Config),
    ConfDir = filename:join(PrivDir,"conf"),

    %% Check that community.conf is overwritten
    {ok,MyCommunity} = snmpa_conf:read_community_config(DataDir),
    {ok,UsedCommunity} = snmpa_conf:read_community_config(ConfDir),
    ct:pal(
      "MyCommunity = ~p~nUsedCommunity = ~p",
      [MyCommunity, UsedCommunity]),
    true = (MyCommunity == UsedCommunity),
    ok.

override_notify(Config) ->
    DataDir = ?config(data_dir,Config),
    PrivDir = ?config(priv_dir,Config),
    ConfDir = filename:join(PrivDir,"conf"),

    %% Check that notify.conf is overwritten
    {ok,MyNotify} = snmpa_conf:read_notify_config(DataDir),
    {ok,UsedNotify} = snmpa_conf:read_notify_config(ConfDir),
    ct:pal(
      "MyNotify = ~p~nUsedNotify = ~p",
      [MyNotify, UsedNotify]),
    true = (MyNotify == UsedNotify),
    ok.

override_target_addr(Config) ->
    DataDir = ?config(data_dir,Config),
    PrivDir = ?config(priv_dir,Config),
    ConfDir = filename:join(PrivDir,"conf"),

    %% Check that target_addr.conf is overwritten
    {ok,MyTargetAddr} = snmpa_conf:read_target_addr_config(DataDir),
    {ok,UsedTargetAddr} = snmpa_conf:read_target_addr_config(ConfDir),
    ct:pal(
      "MyTargetAddr = ~p~nUsedTargetAddr = ~p",
      [MyTargetAddr, UsedTargetAddr]),
    true = (MyTargetAddr == UsedTargetAddr),
    ok.

override_target_params(Config) ->
    DataDir = ?config(data_dir,Config),
    PrivDir = ?config(priv_dir,Config),
    ConfDir = filename:join(PrivDir,"conf"),

    %% Check that target_params.conf is overwritten
    {ok,MyTargetParams} = snmpa_conf:read_target_params_config(DataDir),
    {ok,UsedTargetParams} = snmpa_conf:read_target_params_config(ConfDir),
    ct:pal(
      "MyTargetParams = ~p~nUsedTargetParams = ~p",
      [MyTargetParams, UsedTargetParams]),
    true = (MyTargetParams == UsedTargetParams),
    ok.

override_vacm(Config) ->
    DataDir = ?config(data_dir,Config),
    PrivDir = ?config(priv_dir,Config),
    ConfDir = filename:join(PrivDir,"conf"),

    %% Check that vacm.conf is overwritten
    {ok,MyVacm} = snmpa_conf:read_vacm_config(DataDir),
    {ok,UsedVacm} = snmpa_conf:read_vacm_config(ConfDir),
    ct:pal(
      "MyVacm = ~p~nUsedVacm = ~p",
      [MyVacm, UsedVacm]),
    true = (MyVacm == UsedVacm),
    ok.




%% NOTE!! This test must always be executed last in the suite, and
%% should match all set requests performed in the suite. I.e. if you
%% add a set request, you must add an entry in the return value of
%% ct_snmp:set_info/1 below.
set_info(Config) ->
    %% From test case set_values/1:
    Oid1 = ?sysName_instance,
    NewValue1 = "ct_test changed by " ++ atom_to_list(?MODULE),

    %% The test...
    [{_AgentName,_,[{Oid1,_,NewValue1}]}]
      = ct_snmp:set_info(Config),
    ok.
