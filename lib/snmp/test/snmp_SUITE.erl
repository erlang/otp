%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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

-module(snmp_SUITE).

-include("snmp_test_lib.hrl").

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


init_per_suite(Config) when is_list(Config) ->

    ?DBG("init_per_suite -> entry with"
	 "~n   Config: ~p", [Config]),

    Config.

end_per_suite(Config) when is_list(Config) ->

    ?DBG("end_per_suite -> entry with"
	 "~n   Config: ~p", [Config]),

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
    [
     {app,      [], [{group, app_test}, 
		     {group, appup_test}]},
     {compiler, [], [{group, compiler_test}]},
     {misc,     [], [{group, conf_test}, 
		     {group, pdus_test},
		     {group, log_test}, 
		     {group, note_store_test}]},
     {agent, [],    [{group, mibs_test}, 
		     {group, nfilter_test},
             {group, agent_test},
             {group, agent_conf_test},
		     {group, snmpnet_test}]},
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
     {agent_conf_test,     [], [{snmp_agent_conf_test,     all}]},
     {snmpnet_test,        [], [{snmp_to_snmpnet_SUITE,    all}]},
     {manager_config_test, [], [{snmp_manager_config_test, all}]},
     {manager_user_test,   [], [{snmp_manager_user_test,   all}]},
     {manager_test,        [], [{snmp_manager_test,        all}]}
    ].


init_per_group(GroupName, Config0) ->

    ?DBG("init_per_group -> entry with"
	 "~n   GroupName: ~p"
	 "~n   Config0:   ~p", [GroupName, Config0]),

    case GroupName of
	snmpnet_test ->
	    Config0;
	_ ->
	    %% Group name is not really the suite name
	    %% (but it is a good enough approximation),
	    %% but it does not matter since we only need
	    %% it to be unique.
	    snmp_test_lib:init_suite_top_dir(GroupName, Config0)
    end.

end_per_group(snmpnet_test, Config) ->
    Config;
end_per_group(_GroupName, Config) ->
    lists:keydelete(snmp_suite_top_dir, 1, Config).
