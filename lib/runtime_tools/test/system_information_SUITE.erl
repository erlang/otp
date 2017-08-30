%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2013. All Rights Reserved.
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
%%

-module(system_information_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Test server callbacks
-export([suite/0, all/0, groups/0,
	 init_per_suite/1, end_per_suite/1,
	 init_per_group/2, end_per_group/2,
	 init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
	%% API-test
	api_report/1,
	api_to_file/1,
	api_from_file/1,
	sanity_check/1,
	%% server
	api_start_stop/1,
	validate_server_interface/1
    ]).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%%
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%%   The name of the group.
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%   Group properties that may be combined.
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%%   The name of a test case.
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%%
%% Description: Returns a list of test case group definitions.
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%% Reason = term()
%%   The reason for skipping all groups and test cases.
%%
%% Description: Returns the list of groups and test cases that
%%              are to be executed.
%%--------------------------------------------------------------------
all() -> [
	api_report,
	api_to_file,
	api_from_file,
	api_start_stop,
	validate_server_interface,
	sanity_check
    ].


%%--------------------------------------------------------------------
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Description: Returns list of tuples to set default properties
%%              for the suite.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%--------------------------------------------------------------------
suite() -> [
	{timetrap,{minutes,1}},
	{ct_hooks,[ts_install_cth]}
    ].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Description: Initialization before the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after the suite.
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%% Reason = term()
%%   The reason for skipping all test cases and subgroups in the group.
%%
%% Description: Initialization before each test case group.
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%%
%% Description: Cleanup after each test case group.
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% TestCase = atom()
%%   Name of the test case that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Description: Initialization before each test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%%
%% TestCase = atom()
%%   Name of the test case that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for failing the test case.
%%
%% Description: Cleanup after each test case.
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%% Comment = term()
%%   A comment about the test case that will be printed in the html log.
%%
%% Description: Test case function. (The name of it must be specified in
%%              the all/0 list or in a test case group for the test case
%%              to be executed).
%%--------------------------------------------------------------------


api_report(_Config) ->
    Report = system_information:report(),
    ok     = validate_report(Report),
    ok.

api_to_file(Config) ->
    DataDir  = proplists:get_value(data_dir, Config),
    Filename = filename:join([DataDir, "system_information_report_1.dat"]),
    ok      = system_information:to_file(Filename),
    {ok, _} = file:consult(Filename),
    {save_config, [{report_name, Filename}]}.

api_from_file(Config) ->
    {api_to_file, Saved} = proplists:get_value(saved_config, Config),
    DataDir = proplists:get_value(data_dir, Config),
    Fname1  = filename:join([DataDir, "information_test_report.dat"]),
    Report1 = system_information:from_file(Fname1),
    ok      = validate_report(Report1),
    Fname2  = proplists:get_value(report_name, Saved),
    Report2 = system_information:from_file(Fname2),
    ok      = validate_report(Report2),
    ok.

api_start_stop(_Config) ->
    {ok, _} = system_information:start(),
    ok      = system_information:stop(),
    ok.

validate_server_interface(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Fname1  = filename:join([DataDir, "information_test_report.dat"]),
    %% load old report
    ok = system_information:load_report(file, Fname1),
    ok = validate_loaded_report(),
    ok = system_information:stop(),
    %% load local
    ok = system_information:load_report(),
    ok = validate_loaded_report(),
    ok = system_information:stop(),
    ok.

sanity_check(Config) when is_list(Config) ->
    ok = system_information:sanity_check().


%% aux

validate_loaded_report() ->
    ok = system_information:applications(),
    ok = system_information:applications([full]),
    ok = system_information:environment(),
    ok = system_information:environment([full]),
    ok = system_information:application(kernel),
    ok = system_information:application(kernel,[full]),
    ok = system_information:module(gen_server),
    ok = system_information:module(gen_server,[full]),
    ok = system_information:modules(native),
    ok.


validate_report([]) -> {error, no_entries};
validate_report(Report) ->
    ensure_report_keys([
	    init_arguments,
	    code_paths,
	    code,
	    system_info,
	    erts_compile_info,
	    beam_dynamic_libraries,
	    environment_erts,
	    environment,
	    sanity_check
	], Report).

ensure_report_keys([], _) -> ok;
ensure_report_keys([K|Ks], Report) ->
    case lists:keysearch(K, 1, Report) of
	false -> {error, key_not_found, K};
	_     -> ensure_report_keys(Ks, Report)
    end.

