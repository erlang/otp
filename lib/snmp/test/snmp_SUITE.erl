%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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

-module(snmp_SUITE).

-export([all/1, 
	 init_per_testcase/2, fin_per_testcase/2
	]).

-export([app/1, compiler/1, misc/1, agent/1, manager/1]).

-export([
	 app_test/1,
	 appup_test/1,
	 compiler_test/1,
	 conf_test/1,
	 pdus_test/1,
	 log_test/1,
	 note_store_test/1,
	 mibs_test/1,
	 nfilter_test/1,
	 agent_test/1,
	 manager_config_test/1,
	 manager_user_test/1,
	 manager_test/1
	]).

%%
%% -----
%%

init_per_testcase(_Case, Config) when is_list(Config) ->
    Config.

fin_per_testcase(_Case, Config) when is_list(Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Top test case

all(doc) ->
    ["Test suites for the snmp application.",
     "There are eight different sub test-suites."];

all(suite) ->
    [
     app,
     compiler,
     misc,
     agent,
     manager

    ].

app(suite) ->
    [
     app_test,
     appup_test
     ].

compiler(suite) ->
    [
     compiler_test
    ].

misc(suite) ->
    [
     conf_test,
     pdus_test,
     log_test,
     note_store_test
    ].

agent(suite) ->
    [
     mibs_test,
     nfilter_test,
     agent_test
    ].

manager(suite) ->
    [
     manager_config_test,
     manager_user_test,
     manager_test
    ].


app_test(suite) ->
    [{snmp_app_test, all}].


appup_test(suite) ->
    [{snmp_appup_test, all}].


compiler_test(suite) ->
    [{snmp_compiler_test, all}].  


conf_test(suite) ->
    [{snmp_conf_test, all}].  


pdus_test(suite) ->
    [{snmp_pdus_test, all}].  


log_test(suite) ->
    [{snmp_log_test, all}].  


note_store_test(suite) ->
    [{snmp_note_store_test, all}].  


mibs_test(suite) ->
    [{snmp_agent_mibs_test, all}].


nfilter_test(suite) ->
    [{snmp_agent_nfilter_test, all}].


agent_test(suite) ->
    [{snmp_agent_test, all}].


manager_config_test(suite) ->
    [{snmp_manager_config_test, all}].  


manager_user_test(suite) ->
    [{snmp_manager_user_test, all}].  


manager_test(suite) ->
    [{snmp_manager_test, all}].  


