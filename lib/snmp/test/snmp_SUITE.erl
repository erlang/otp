%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2011. All Rights Reserved.
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

-export([all/0, 
	 suite/0,
	 groups/0, 
	 init_per_suite/1,    end_per_suite/1, 
	 init_per_group/2,    end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2
	]).


%%
%% -----
%%

init_per_testcase(_Case, Config) when is_list(Config) ->
    Config.

end_per_testcase(_Case, Config) when is_list(Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Top test case

suite() -> 
    [{ct_hooks, [ts_install_cth]}].

all() -> 
    [{group, app}, 
     {group, compiler}, 
     {group, misc},
     {group, agent}, 
     {group, manager}].

groups() -> 
    [{app,      [], [{group, app_test}, 
		     {group, appup_test}]},
     {compiler, [], [{group, compiler_test}]},
     {misc,     [], [{group, conf_test}, 
		     {group, pdus_test},
		     {group, log_test}, 
		     {group, note_store_test}]},
     {agent, [],    [{group, mibs_test}, 
		     {group, nfilter_test},
		     {group, agent_test}]},
     {manager, [],  [{group, manager_config_test},
		     {group, manager_user_test}, 
		     {group, manager_test}]},
     {app_test,            [], [{snmp_app_test,            all}]},
     {appup_test,          [], [{snmp_appup_test,          all}]},
     {compiler_test,       [], [{snmp_compiler_test,       all}]},
     {conf_test,           [], [{snmp_conf_test,           all}]},
     {pdus_test,           [], [{snmp_pdus_test,           all}]},
     {log_test,            [], [{snmp_log_test,            all}]},
     {note_store_test,     [], [{snmp_note_store_test,     all}]},
     {mibs_test,           [], [{snmp_agent_mibs_test,     all}]},
     {nfilter_test,        [], [{snmp_agent_nfilter_test,  all}]},
     {agent_test,          [], [{snmp_agent_test,          all}]},
     {manager_config_test, [], [{snmp_manager_config_test, all}]},
     {manager_user_test,   [], [{snmp_manager_user_test,   all}]},
     {manager_test,        [], [{snmp_manager_test,        all}]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

