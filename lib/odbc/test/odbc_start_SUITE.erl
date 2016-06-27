%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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

-module(odbc_start_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("odbc_test.hrl").

%% Test server callback functions
%%--------------------------------------------------------------------
%% Function: init_per_suite(Config) -> Config
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Initialization before the whole suite
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    case odbc_test_lib:skip() of
	true ->
	    {skip, "ODBC not supported"};
	false ->
	    case code:which(odbc) of
		non_existing ->
		    {skip, "No ODBC built"};
		_ ->
		    %% Make sure odbc is not already started
		    odbc:stop(),
		    ct:timetrap(?TIMEOUT),
		    [{tableName, odbc_test_lib:unique_table_name()} | Config]
	    end
    end.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.
%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config) -> Config
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Initialization before each test case
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%% Description: Initialization before each test case
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    ct:pal("ODBCINI = ~p~n", [os:getenv("ODBCINI")]),
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config) -> _
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after each test case
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.
%%--------------------------------------------------------------------
%% Function: all(Clause) -> TestCases
%% Clause - atom() - suite | doc
%% TestCases - [Case] 
%% Case - atom()
%%   Name of a test case.
%% Description: Returns a list of all test cases in this test suite
%%--------------------------------------------------------------------
suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    case odbc_test_lib:odbc_check() of
	ok -> [app, appup, start, long_connection_line];
	_Other -> [app, appup]
    end.

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



%% Test cases starts here.
%%--------------------------------------------------------------------

%% Test that the odbc app file is ok
app(Config) when is_list(Config) ->
    ok = ?t:app_test(odbc).

%% Test that the odbc appup file is ok
appup(Config) when is_list(Config) ->
    ok = ?t:appup_test(odbc).

start() -> 
    [{doc,"Test start/stop of odbc"}].
start(Config) when is_list(Config) -> 
    PlatformOptions = odbc_test_lib:platform_options(),
	{error,odbc_not_started} = odbc:connect(?RDBMS:connection_string(),
					    PlatformOptions),
    odbc:start(),
    case odbc:connect(?RDBMS:connection_string(), PlatformOptions) of
	{ok, Ref0} ->
	    ok = odbc:disconnect(Ref0),
	    odbc:stop(),
	    {error,odbc_not_started} = 
		odbc:connect(?RDBMS:connection_string(), PlatformOptions),
	    start_odbc(transient),
	    start_odbc(permanent);
	{error, odbc_not_started} ->
	    ct:fail(start_failed);
	Error ->
	    ct:pal("Connection failed: ~p~n", [Error]),
	    {skip, "ODBC is not properly setup"}
    end.
    
start_odbc(Type) ->
    ok = odbc:start(Type),
    case odbc:connect(?RDBMS:connection_string(), odbc_test_lib:platform_options()) of
	{ok, Ref} ->
	    ok = odbc:disconnect(Ref),
	    odbc:stop();
	{error, odbc_not_started} ->
	    ct:fail(start_failed)
    end.


long_connection_line()->
    [{doc,"Test a connection line longer than 127 characters"}].
long_connection_line(_Config)  ->
    odbc:start(),
    String133 = "unknown_odbc_parameter=01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789",
    {error, Reason} = odbc:connect(String133, []),
    odbc:stop(),
    ct:pal("Driver error reason: ~p",[Reason]).
