%%--------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File: ct_snmp_SUITE.erl
%%
%% Description:
%%    This file contains the test cases for the ct_snmp API.
%%
%% @author Support
%% @doc Test  of SNMP support in common_test
%% @end
%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
-module(snmp1_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("snmp/include/STANDARD-MIB.hrl").
-include_lib("snmp/include/snmp_types.hrl").

-compile(export_all).

%% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).

%% SNMP user stuff
-behaviour(snmpm_user).
-export([handle_error/3,
	 handle_agent/5,
	 handle_pdu/4,
	 handle_trap/3,
	 handle_inform/3,
	 handle_report/3]).


suite() ->
    [{require, snmp_mgr_agent, snmp},
     {require, snmp_app_cfg, snmp_app}].

all() ->
    [start_stop,
    {group,get_set}].


groups() ->
    [{get_set,[get_values,get_next_values,set_values]}].

init_per_group(get_set, Config) ->
    ok = ct_snmp:start(Config,snmp_mgr_agent,snmp_app_cfg),
    Config.

end_per_group(get_set, Config) ->
    ok = ct_snmp:stop(Config),
    Config.

init_per_testcase(_Case, Config) ->
    Dog = test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

break(_Config) ->
    test_server:break(""),
    ok.

start_stop(Config) ->
    ok = ct_snmp:start(Config,snmp_mgr_agent,snmp_app_cfg),
    timer:sleep(1000),
    {snmp,_,_} = lists:keyfind(snmp,1,application:which_applications()),
    [_|_] = filelib:wildcard("*/*.conf",?config(priv_dir,Config)),

    ok = ct_snmp:stop(Config),
    timer:sleep(1000),
    false = lists:keyfind(snmp,1,application:which_applications()),
    [] = filelib:wildcard("*/*.conf",?config(priv_dir,Config)),
    ok.

get_values(_Config) ->
    Oids1 = [?sysDescr_instance, ?sysName_instance],
    {noError,_,V1} = ct_snmp:get_values(agent_name,Oids1,snmp_mgr_agent),
    [#varbind{oid=?sysDescr_instance,value="Erlang SNMP agent"},
     #varbind{oid=?sysName_instance,value="ct_test"}] = V1,
    ok.

get_next_values(_Config) ->
    Oids2 = [?system],
    {noError,_,V2} = ct_snmp:get_next_values(agent_name,Oids2,snmp_mgr_agent),
    [#varbind{oid=?sysDescr_instance,value="Erlang SNMP agent"}] = V2,
    ok.

set_values(Config) ->
    Oid3 = ?sysName_instance,
    NewName = "ct_test changed by " ++ atom_to_list(?MODULE),
    VarsAndVals = [{Oid3,s,NewName}],
    {noError,_,_} =
	ct_snmp:set_values(agent_name,VarsAndVals,snmp_mgr_agent,Config),

    Oids4 = [?sysName_instance],
    {noError,_,V4} = ct_snmp:get_values(agent_name,Oids4,snmp_mgr_agent),
    [#varbind{oid=?sysName_instance,value=NewName}] = V4,

    ok.


%%%-----------------------------------------------------------------
%%% SNMP Manager User callback
handle_error(ReqId, Reason, UserData) ->
    erlang:display({handle_error,ReqId, Reason, UserData}),
    ignore.

handle_agent(Addr, Port, Type, SnmpInfo, UserData) ->
    erlang:display({handle_agent,Addr, Port, Type, SnmpInfo, UserData}),
    ignore.

handle_pdu(TargetName, ReqId, SnmpPduInfo, UserData) ->
    erlang:display({handle_pdu,TargetName, ReqId, SnmpPduInfo, UserData}),
    ignore.

handle_trap(TargetName, SnmpTrapInfo, UserData) ->
    erlang:display({handle_trap,TargetName, SnmpTrapInfo, UserData}),
    ignore.

handle_inform(TargetName, SnmpInformInfo, UserData) ->
    erlang:display({handle_inform,TargetName, SnmpInformInfo, UserData}),
    ignore.

handle_report(TargetName, SnmpReportInfo, UserData) ->
    erlang:display({handle_report,TargetName, SnmpReportInfo, UserData}),
    ignore.
