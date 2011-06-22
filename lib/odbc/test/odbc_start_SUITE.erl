%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2011. All Rights Reserved.
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

-module(odbc_start_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("test_server_line.hrl").
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
init_per_testcase(_TestCase, Config0) ->
    test_server:format("ODBCINI = ~p~n", [os:getenv("ODBCINI")]),
    Config = lists:keydelete(watchdog, 1, Config0),
    Dog = test_server:timetrap(?TIMEOUT),
    [{watchdog, Dog} | Config].

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config) -> _
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after each test case
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, Config) ->
    Dog = ?config(watchdog, Config),
    case Dog of 
	undefined ->
	    ok;
	_ ->
	    test_server:timetrap_cancel(Dog)
    end.

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
	ok -> [start];
	Other -> {skip, Other}
    end.

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



%% Test cases starts here.
%%--------------------------------------------------------------------

start(doc) -> 
    ["Test start/stop of odbc"];
start(suite) -> 
    [];
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
	    test_server:fail(start_failed);
	Error ->
	    test_server:format("Connection failed: ~p~n", [Error]),
	    {skip, "ODBC is not properly setup"}
    end.
    
start_odbc(Type) ->
    ok = odbc:start(Type),
    case odbc:connect(?RDBMS:connection_string(), odbc_test_lib:platform_options()) of
	{ok, Ref} ->
	    ok = odbc:disconnect(Ref),
	    odbc:stop();
	{error, odbc_not_started} ->
	    test_server:fail(start_failed)
    end.
