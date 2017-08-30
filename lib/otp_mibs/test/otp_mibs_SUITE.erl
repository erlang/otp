%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
-module(otp_mibs_SUITE).

%%-----------------------------------------------------------------
%% This suite can no longer be executed standalone, i.e. it must be
%% executed with common test. The reason is that ct_snmp is used
%% instead of the snmp application directly. The suite requires a
%% config file, otp_mibs_SUITE.cfg, found in the same directory as
%% the suite.
%%
%% Execute with:
%% > ct_run -suite otp_mibs_SUITE -config otp_mibs_SUITE.cfg
%%-----------------------------------------------------------------

-include_lib("common_test/include/ct.hrl").
-include_lib("otp_mibs/include/OTP-MIB.hrl").
-include_lib("snmp/include/snmp_types.hrl").

% Test server specific exports
-export([all/0,
	 suite/0,
	 groups/0,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2]).

% Test cases must be exported.
-export([app/1, appup/1, nt_basic_types/1, nt_high_reduction_count/1]).

-define(TRAP_UDP,      5000).
-define(AGENT_UDP,     4000).
-define(CONF_FILE_VER, [v2]).
-define(SYS_NAME,      "Test otp_mibs").
-define(MAX_MSG_SIZE,  484).
-define(ENGINE_ID,     "mgrEngine").
-define(MGR_PORT,      5001).

%% Since some cases are only interested in single entries of the OTP-MIB's
%% node table, one row must be chosen. The first row should be sufficient
%% for this.
-define(NODE_ENTRY, 1).

%%---------------------------------------------------------------------
%% CT setup
%%---------------------------------------------------------------------

init_per_testcase(_Case, Config) when is_list(Config) ->
    Dog = test_server:timetrap(test_server:minutes(6)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) when is_list(Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    Config.

suite() -> [{ct_hooks,[ts_install_cth]}, {require, snmp_mgr_agent, snmp}].

all() -> [{group, app}, {group, node_table}].

groups() -> [{app, [], [app, appup]},
             {node_table, [], [nt_basic_types, nt_high_reduction_count]}].

init_per_group(_GroupName, Config) -> Config.

end_per_group(_GroupName, Config) -> Config.

init_per_suite(Config) ->
    ?line application:start(sasl),
    ?line application:start(mnesia),
    ?line application:start(otp_mibs),

    ok = ct_snmp:start(Config,snmp_mgr_agent),

    %% Load the mibs that should be tested
    otp_mib:load(snmp_master_agent),

    Config.

end_per_suite(Config) ->
    PrivDir = ?config(priv_dir, Config),
    ConfDir = filename:join(PrivDir,"conf"),
    DbDir = filename:join(PrivDir,"db"),
    MgrDir = filename:join(PrivDir, "mgr"),

    %% Uload mibs
    otp_mib:unload(snmp_master_agent),

    %% Clean up
    application:stop(snmp),
    application:stop(mnesia),
    application:stop(otp_mibs),

    del_dir(ConfDir),
    del_dir(DbDir),
    (catch del_dir(MgrDir)),
    ok.

%%---------------------------------------------------------------------
%% Test cases
%%---------------------------------------------------------------------

%% Test that the otp_mibs app file is ok
app(Config) when is_list(Config) ->
    ok = ?t:app_test(otp_mibs).

%% Test that the otp_mibs appup file is ok
appup(Config) when is_list(Config) ->
    ok = ?t:appup_test(otp_mibs).

nt_basic_types(suite) ->
    [];
nt_basic_types(doc) ->
    ["Query every item of the node table and check its variable "
     "type and content for sensible values."];
nt_basic_types(Config) when is_list(Config) ->
    ok = otp_mib:update_erl_node_table(),

    NodeNameId = ?erlNodeEntry ++ [?erlNodeName, ?NODE_ENTRY],
    {noError, 0, [NodeNameVal]} = snmp_get([NodeNameId]),
    #varbind{variabletype = 'OCTET STRING'} = NodeNameVal,
    true = is_list(NodeNameVal#varbind.value),

    NodeMachineId = ?erlNodeEntry ++ [?erlNodeMachine, ?NODE_ENTRY],
    {noError, 0, [NodeMachineVal]} = snmp_get([NodeMachineId]),
    #varbind{variabletype = 'OCTET STRING'} = NodeMachineVal,
    true = is_list(NodeMachineVal#varbind.value),

    NodeVersionId = ?erlNodeEntry ++ [?erlNodeVersion, ?NODE_ENTRY],
    {noError, 0, [NodeVersionVal]} = snmp_get([NodeVersionId]),
    #varbind{variabletype = 'OCTET STRING'} = NodeVersionVal,
    true = is_list(NodeVersionVal#varbind.value),

    NodeRunQueueId = ?erlNodeEntry ++ [?erlNodeRunQueue, ?NODE_ENTRY],
    {noError, 0, [NodeRunQueueVal]} = snmp_get([NodeRunQueueId]),
    #varbind{variabletype = 'Unsigned32'} = NodeRunQueueVal,
    true = is_integer(NodeRunQueueVal#varbind.value),
    NodeRunQueueVal#varbind.value >= 0,
    NodeRunQueueVal#varbind.value =< 4294967295,

    NodeRunTimeId = ?erlNodeEntry ++ [?erlNodeRunTime, ?NODE_ENTRY],
    {noError, 0, [NodeRunTimeVal]} = snmp_get([NodeRunTimeId]),
    #varbind{variabletype = 'Counter64'} = NodeRunTimeVal,
    true = is_integer(NodeRunTimeVal#varbind.value),
    NodeRunTimeVal#varbind.value >= 0,
    NodeRunTimeVal#varbind.value =< 18446744073709551615,

    NodeWallClockId = ?erlNodeEntry ++ [?erlNodeWallClock, ?NODE_ENTRY],
    {noError, 0, [NodeWallClockVal]} = snmp_get([NodeWallClockId]),
    #varbind{variabletype = 'Counter64'} = NodeWallClockVal,
    true = is_integer(NodeWallClockVal#varbind.value),
    NodeWallClockVal#varbind.value >= 0,
    NodeWallClockVal#varbind.value =< 18446744073709551615,

    NodeReductionsId = ?erlNodeEntry ++ [?erlNodeReductions, ?NODE_ENTRY],
    {noError, 0, [NodeReductionsVal]} = snmp_get([NodeReductionsId]),
    #varbind{variabletype = 'Counter64'} = NodeReductionsVal,
    true = is_integer(NodeReductionsVal#varbind.value),
    NodeReductionsVal#varbind.value >= 0,
    NodeReductionsVal#varbind.value =< 18446744073709551615,

    NodeProcessesId = ?erlNodeEntry ++ [?erlNodeProcesses, ?NODE_ENTRY],
    {noError, 0, [NodeProcessesVal]} = snmp_get([NodeProcessesId]),
    #varbind{variabletype = 'Unsigned32'} = NodeProcessesVal,
    true = is_integer(NodeProcessesVal#varbind.value),
    NodeProcessesVal#varbind.value >= 0,
    NodeProcessesVal#varbind.value =< 4294967295,

    NodeInBytesId = ?erlNodeEntry ++ [?erlNodeInBytes, ?NODE_ENTRY],
    {noError, 0, [NodeInBytesVal]} = snmp_get([NodeInBytesId]),
    #varbind{variabletype = 'Counter64'} = NodeInBytesVal,
    true = is_integer(NodeInBytesVal#varbind.value),
    NodeInBytesVal#varbind.value >= 0,
    NodeInBytesVal#varbind.value =< 18446744073709551615,

    NodeOutBytesId = ?erlNodeEntry ++ [?erlNodeOutBytes, ?NODE_ENTRY],
    {noError, 0, [NodeOutBytesVal]} = snmp_get([NodeOutBytesId]),
    #varbind{variabletype = 'Counter64'} = NodeOutBytesVal,
    true = is_integer(NodeOutBytesVal#varbind.value),
    NodeOutBytesVal#varbind.value >= 0,
    NodeOutBytesVal#varbind.value =< 18446744073709551615,

    ok.

nt_high_reduction_count(suite) ->
    [];
nt_high_reduction_count(doc) ->
    ["Check that no error occurs when the erlNodeReductions field"
     "exceeds the 32bit boundary, this may take about 10min."];
nt_high_reduction_count(Config) when is_list(Config) ->
    NodeReductions = ?erlNodeEntry ++ [?erlNodeReductions, ?NODE_ENTRY],

    BumpFun = fun(F, Limit) ->
		      case erlang:statistics(reductions) of
			  {Total, _} when Total < Limit ->
			      F(F, Limit);
			  _ ->
			      ok
		      end
	      end,

    ok = otp_mib:update_erl_node_table(),

    {noError, 0, [StartVal]} = snmp_get([NodeReductions]),
    #varbind{variabletype = 'Counter64'} = StartVal,
    true = is_integer(StartVal#varbind.value),
    StartVal#varbind.value >= 0,
    case StartVal#varbind.value =< 4294967295 of
	true ->
	    ok = otp_mib:update_erl_node_table(),
	    BumpFun(BumpFun, 4294967295),
	    {noError, 0, [EndVal]} = snmp_get([NodeReductions]),
	    #varbind{variabletype = 'Counter64'} = EndVal,
	    true = is_integer(EndVal#varbind.value),
	    EndVal#varbind.value >= 4294967295,
	    EndVal#varbind.value =< 18446744073709551615;
	false ->
	    %% no need to bump more reductions, since the initial get
	    %% command already returned successfully with a large value
	    ok
    end.

%%---------------------------------------------------------------------
%% Internal functions
%%---------------------------------------------------------------------

snmp_get(OIdList) ->
    ct_snmp:get_values(otp_mibs_test, OIdList, snmp_mgr_agent).

del_dir(Dir) ->
    io:format("Deleting: ~s~n",[Dir]),
    {ok, Files} = file:list_dir(Dir),
    FullPathFiles = lists:map(fun(File) -> filename:join(Dir, File) end, Files),
    lists:foreach(fun file:delete/1, FullPathFiles),
    file:del_dir(Dir).
