%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2019. All Rights Reserved.
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

-module(snmp_agent_conf_SUITE).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------

-include("snmp_test_lib.hrl").
-include_lib("common_test/include/ct.hrl").

-export([
         suite/0, all/0, groups/0,
         init_per_suite/1,    end_per_suite/1,
         init_per_group/2,    end_per_group/2, 
         init_per_testcase/2, end_per_testcase/2,
         
         check_agent/1,
         check_usm/1,
         check_vacm/1
        ]).



%%======================================================================
%% Common Test interface functions
%%======================================================================

suite() -> 
    [{ct_hooks, [ts_install_cth]}].

all() ->
    [
     check_agent,
     check_usm,
     check_vacm
    ].


groups() ->
    [].


%%
%% -----
%%

init_per_suite(Config0) ->
    case ?LIB:init_per_suite(Config0) of
        {skip, _} = SKIP ->
            SKIP;
        Config1 when is_list(Config1) ->
            snmp_test_sys_monitor:start(),
            PrivDir = ?config(priv_dir, Config1),
            PrivSubdir = filename:join(PrivDir, "snmp_agent_conf_test"),
            ok = filelib:ensure_dir(filename:join(PrivSubdir, "dummy")),
            [{priv_subdir, PrivSubdir} | Config1]
    end.

end_per_suite(Config) ->
    snmp_test_sys_monitor:stop(),
    ?LIB:end_per_suite(Config).



%%
%% -----
%%

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%
%% -----
%%

init_per_testcase(_Case, Config) when is_list(Config) ->
    snmp_test_global_sys_monitor:reset_events(),
    Config.

end_per_testcase(_Case, Config) when is_list(Config) ->
    ?PRINT2("system events during test: "
            "~n   ~p", [snmp_test_global_sys_monitor:events()]),
    Config.




%%======================================================================
%% Test data
%%======================================================================

engine_ids() -> [
    "plain eid",
    "here\"eid",
    "comes\neid",
    "trouble\0eid",
    binary_to_list(<<"中国引擎标识符"/utf8>>)
].

snmp_admin_strings() -> [
    "plain string",
    "heres\"eid",
    "trouble\neid",
    binary_to_list(<<"中国引擎标识符"/utf8>>)
].


%%======================================================================
%% Test functions
%%======================================================================


check_agent(Config) ->
    Dir = ?config(priv_subdir, Config),
    lists:foreach(
        fun(EngineId) -> check_agent_by_engineid(Dir, EngineId) end,
        engine_ids()
    ),
    ok.

check_agent_by_engineid(Dir, EngineId) ->
    WEntries = [
        snmpa_conf:agent_entry(intAgentIpAddress, {0,0,0,0}),
        snmpa_conf:agent_entry(intAgentUDPPort, 161),
        snmpa_conf:agent_entry(snmpEngineMaxMessageSize, 484),
        snmpa_conf:agent_entry(snmpEngineID, EngineId)
    ],

    ok = snmpa_conf:write_agent_config(Dir, WEntries),
    {ok, REntries} = snmpa_conf:read_agent_config(Dir),

    true = is_subset(WEntries, REntries),
    ok.

%%======================================================================

check_usm(Config) ->
    Dir = ?config(priv_subdir, Config),
    EngineId = hd(engine_ids()),
    UserName = hd(snmp_admin_strings()),
    SecName  = hd(snmp_admin_strings()),

    %% vary engine id
    lists:foreach(
        fun(EngineId_) -> check_usm_by_params(Dir, EngineId_, UserName, SecName) end,
        engine_ids()
    ),

    %% vary user name
    lists:foreach(
        fun(UserName_) -> check_usm_by_params(Dir, EngineId, UserName_, SecName) end,
        snmp_admin_strings()
    ),

    %% vary sec name
    lists:foreach(
        fun(SecName_) -> check_usm_by_params(Dir, EngineId, UserName, SecName_) end,
        snmp_admin_strings()
    ),

    ok.

check_usm_by_params(Dir, EngineId, UserName, SecName) ->
    WEntries = [
        snmpa_conf:usm_entry(
            EngineId,
            UserName,
            SecName,
            zeroDotZero,
            usmNoAuthProtocol, % authproto
            "", "",
            usmNoPrivProtocol, % privproto
            "", "", "",
            [], %AuthKey
            [])  %PrivKey
    ],

    ok = snmpa_conf:write_usm_config(Dir, WEntries),
    {ok, REntries} = snmpa_conf:read_usm_config(Dir),

    true = is_subset(WEntries, REntries),
    ok.

%%======================================================================

check_vacm(Config) ->
    Dir = ?config(priv_subdir, Config),

    %% vary sec name
    lists:foreach(
        fun(SecName_) -> check_vacm_by_params(Dir, SecName_) end,
        snmp_admin_strings()
    ),

    ok.


check_vacm_by_params(Dir, SecName) ->
    WEntries = [
        %%                      SecModel, SecName, GroupName
        snmpa_conf:vacm_s2g_entry(usm, SecName,  SecName),
        %%                     GroupName,Prefix,SecModel,SecLevel,Match,ReadView,WriteView,NotifyView
        snmpa_conf:vacm_acc_entry(SecName,  "", any, noAuthNoPriv, exact, "all", "all", "all")
    ],

    ok = snmpa_conf:write_vacm_config(Dir, WEntries),
    {ok, REntries} = snmpa_conf:read_vacm_config(Dir),

    true = is_subset(WEntries, REntries),
    ok.



%%======================================================================


%% additional tests needed:
% check_context()
% check_community()
% check_standard()
% check_target_addr()
% check_target_params()
% check_notify()


%%======================================================================
%% Local utility functions
%%======================================================================

is_subset(List1, List2) ->
    io:format("Check ~p is subset of ~p\n", [List1, List2]),
    sets:is_subset(
        sets:from_list(List1),
        sets:from_list(List2)
    ).
