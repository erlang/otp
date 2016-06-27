%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2016. All Rights Reserved.
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

-module(odbc_connect_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("odbc_test.hrl").

-define(MAX_SEQ_TIMEOUTS, 10).

%%--------------------------------------------------------------------
%% all(Arg) -> [Doc] | [Case] | {skip, Comment}
%% Arg - doc | suite
%% Doc - string()
%% Case - atom() 
%%	Name of a test case function. 
%% Comment - string()
%% Description: Returns documentation/test cases in this test suite
%%		or a skip tuple if the platform is not supported.  
%%--------------------------------------------------------------------

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    case odbc_test_lib:odbc_check() of
	ok ->
	    [not_exist_db, commit, rollback, not_explicit_commit,
	     no_c_executable, port_dies, control_process_dies,
	     {group, client_dies}, connect_timeout, timeout,
	     many_timeouts, timeout_reset, disconnect_on_timeout,
	     connection_closed, disable_scrollable_cursors,
	     return_rows_as_lists, api_missuse, extended_errors];
	Other -> {skip, Other}
    end.

groups() -> 
    [{client_dies, [],
      [client_dies_normal, client_dies_timeout,
       client_dies_error]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%--------------------------------------------------------------------
%% Function: init_per_suite(Config) -> Config
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Initiation before the whole suite
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) when is_list(Config) ->
    file:write_file(filename:join([proplists:get_value(priv_dir,Config),
				   "..","..","..","ignore_core_files"]),""),
    case odbc_test_lib:skip() of
	true ->
	    {skip, "ODBC not supported"};
	false ->
	    case (catch odbc:start()) of
		ok ->
		    case catch odbc:connect(?RDBMS:connection_string(),
					    [{auto_commit, off}] ++ odbc_test_lib:platform_options()) of
			{ok, Ref} ->
			    odbc:disconnect(Ref),
			    ct:timetrap(?default_timeout),
			    [{tableName, odbc_test_lib:unique_table_name()} | Config];
			_  ->
			    {skip, "ODBC is not properly setup"}
		    end;
		_ ->
		    {skip,"ODBC not startable"}
	    end
    end.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    application:stop(odbc).

%%--------------------------------------------------------------------
%% Function: init_per_testcase(Case, Config) -> Config
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Initiation before each test case
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_testcase(connect_port_timeout, Config) ->
    odbc:stop(),
    application:load(odbc),
    application:set_env(odbc, port_timeout, 0),
    odbc:start(),
    init_per_testcase_common(Config);
init_per_testcase(_TestCase, Config) ->
    init_per_testcase_common(Config).

init_per_testcase_common(Config) ->
    ct:pal("ODBCINI = ~p~n", [os:getenv("ODBCINI")]),
    lists:keydelete(connection_ref, 1, Config).

%%--------------------------------------------------------------------
%% Function: end_per_testcase(Case, Config) -> _
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after each test case
%%--------------------------------------------------------------------

end_per_testcase(connect_port_timeout, Config) ->
    application:unset_env(odbc, port_timeout),
    odbc:stop(),
    odbc:start(),
    end_per_testcase_common(Config);
end_per_testcase(_TestCase, Config) ->
    end_per_testcase_common(Config).

end_per_testcase_common(Config) ->
    Table = proplists:get_value(tableName, Config),
    {ok, Ref} = odbc:connect(?RDBMS:connection_string(), odbc_test_lib:platform_options()),
    Result = odbc:sql_query(Ref, "DROP TABLE " ++ Table),
    io:format("Drop table: ~p ~p~n", [Table, Result]),
    odbc:disconnect(Ref).

%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------
commit()->
    [{doc,"Test the use of explicit commit"}].
commit(Config)  ->
    {ok, Ref} =  odbc:connect(?RDBMS:connection_string(), 
			      [{auto_commit, off}] ++ odbc_test_lib:platform_options()),

    Table = proplists:get_value(tableName, Config),
    TransStr = transaction_support_str(?RDBMS),

    {updated, _} = 
	odbc:sql_query(Ref, 
		       "CREATE TABLE " ++ Table ++
		       " (ID integer, DATA varchar(10))" ++ TransStr),

    {updated, 1} = 
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(1,'bar')"),

    {updated, 1} = 
	odbc:sql_query(Ref, "UPDATE " ++ Table ++
		       " SET DATA = 'foo' WHERE ID = 1"),

    ok = odbc:commit(Ref, commit),
    UpdateResult = ?RDBMS:update_result(),
    UpdateResult = 
	odbc:sql_query(Ref, "SELECT * FROM " ++ Table),

    {updated, 1} = 
	odbc:sql_query(Ref, "UPDATE " ++ Table ++
		       " SET DATA = 'bar' WHERE ID = 1"),
    ok = odbc:commit(Ref, commit, ?TIMEOUT),
    InsertResult = ?RDBMS:insert_result(),
    InsertResult = 
	odbc:sql_query(Ref, "SELECT * FROM " ++ Table),

    {'EXIT', {function_clause, _}} = 
	(catch odbc:commit(Ref, commit, -1)),

    ok = odbc:disconnect(Ref).
%%-------------------------------------------------------------------------

rollback()->
    [{doc,"Test the use of explicit rollback"}].
rollback(Config)  ->
    {ok, Ref} =  odbc:connect(?RDBMS:connection_string(),
			      [{auto_commit, off}] ++ odbc_test_lib:platform_options()),

    Table = proplists:get_value(tableName, Config),

    TransStr = transaction_support_str(?RDBMS),

    {updated, _} =
	odbc:sql_query(Ref,
		       "CREATE TABLE " ++ Table ++
			   " (ID integer, DATA varchar(10))" ++ TransStr),
    {updated, 1} =
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++ " VALUES(1,'bar')"),
    ok = odbc:commit(Ref, commit),

    {updated, 1} =
	odbc:sql_query(Ref, "UPDATE " ++ Table ++
			   " SET DATA = 'foo' WHERE ID = 1"),
    ok = odbc:commit(Ref, rollback),
    InsertResult = ?RDBMS:insert_result(),
    InsertResult =
	odbc:sql_query(Ref, "SELECT * FROM " ++ Table),
    {updated, 1} =
	odbc:sql_query(Ref, "UPDATE " ++ Table ++
			   " SET DATA = 'foo' WHERE ID = 1"),
    ok = odbc:commit(Ref, rollback, ?TIMEOUT),
    InsertResult = ?RDBMS:insert_result(),
    InsertResult =
	odbc:sql_query(Ref, "SELECT * FROM " ++ Table),

    {'EXIT', {function_clause, _}} =
	(catch odbc:commit(Ref, rollback, -1)),

    ok = odbc:disconnect(Ref).

%%-------------------------------------------------------------------------
not_explicit_commit() ->
    [{doc,"Test what happens if you try using commit on a auto_commit connection."}].
not_explicit_commit(_Config) ->
    {ok, Ref} = 
	odbc:connect(?RDBMS:connection_string(), [{auto_commit, on}] ++
		    odbc_test_lib:platform_options()),
    {error, _} = odbc:commit(Ref, commit),
    ok = odbc:disconnect(Ref).

%%-------------------------------------------------------------------------
not_exist_db() ->
    [{doc,"Tests valid data format but invalid data in the connection parameters."}].
not_exist_db(_Config)  ->
    {error, _} = odbc:connect("DSN=foo;UID=bar;PWD=foobar",
			      odbc_test_lib:platform_options()),
    %% So that the odbc control server can be stoped "in the correct way"
    ct:sleep(100).

%%-------------------------------------------------------------------------
no_c_executable() ->
    [{doc,"Test what happens if the port-program can not be found"}].
no_c_executable(_Config) ->
    process_flag(trap_exit, true),
    Dir = filename:nativename(filename:join(code:priv_dir(odbc), 
					    "bin")),
    FileName1 = filename:nativename(os:find_executable("odbcserver", 
						       Dir)),
    FileName2 = filename:nativename(filename:join(Dir, "odbcsrv")),
    case file:rename(FileName1, FileName2) of
	ok ->
	    Result = 
		case catch odbc:connect(?RDBMS:connection_string(),
					odbc_test_lib:platform_options()) of
		    {error, port_program_executable_not_found} ->
			ok;
		    Else ->
			Else
		end,
	    ok = file:rename(FileName2, FileName1), 
	    ok = Result;
	_ ->
	    {skip, "File permission issues"}
    end.
%%------------------------------------------------------------------------

port_dies() ->
    [{doc,"Tests what happens if the port program dies"}].
port_dies(_Config) ->
    {ok, Ref} =  odbc:connect(?RDBMS:connection_string(), odbc_test_lib:platform_options()),
    {status, _} = process_info(Ref, status),   
    process_flag(trap_exit, true),
    NamedPorts =  [{P,  erlang:port_info(P, name)} || P <- erlang:ports()],
    case [P || {P, {name, Name}} <- NamedPorts,  is_odbcserver(Name)]  of
	[Port] ->
	    exit(Port, kill),
	    %% Wait for exit_status from port 5000 ms (will not get a exit
	    %% status in this case), then wait a little longer to make sure
	    %% the port and the controlprocess has had time to terminate.
	    ct:sleep(10000),
	    undefined = process_info(Ref, status);
	[] ->
	    ct:fail([erlang:port_info(P, name) || P <- erlang:ports()])  
    end.


%%-------------------------------------------------------------------------
control_process_dies() ->
    [{doc,"Tests what happens if the Erlang control process dies"}].
control_process_dies(_Config) ->
    {ok, Ref} =  odbc:connect(?RDBMS:connection_string(), odbc_test_lib:platform_options()),
    process_flag(trap_exit, true),
    NamedPorts =  [{P,  erlang:port_info(P, name)} || P <- erlang:ports()],
    case [P || {P, {name, Name}} <- NamedPorts,  is_odbcserver(Name)] of
	[Port] ->
	    {connected, Ref} = erlang:port_info(Port, connected),  
	    exit(Ref, kill),
	    ct:sleep(500),
	    undefined = erlang:port_info(Port, connected);
	%% Check for c-program still running, how?
	[] ->
	    ct:fail([erlang:port_info(P, name) || P <- erlang:ports()])    
    end.

%%-------------------------------------------------------------------------
client_dies_normal() ->
    [{doc,"Client dies with reason normal."}].
client_dies_normal(Config) when is_list(Config) ->
    Pid = spawn(?MODULE, client_normal, [self()]),

    MonitorReference =
	receive 
	    {dbRef, Ref}  ->
		MRef = erlang:monitor(process, Ref),
		Pid ! continue,
		MRef
	end,

    receive 
	{'DOWN', MonitorReference, _Type, _Object, _Info} ->
	    ok
    after 5000 ->
	    ct:fail(control_process_not_stopped)
    end.

client_normal(Pid) ->
    {ok, Ref} =  odbc:connect(?RDBMS:connection_string(), odbc_test_lib:platform_options()),
    Pid ! {dbRef, Ref},
    receive 
	continue ->
	    ok
    end,
    exit(self(), normal).


%%-------------------------------------------------------------------------
client_dies_timeout() ->
    [{doc,"Client dies with reason timeout."}].
client_dies_timeout(Config) when is_list(Config) ->
    Pid = spawn(?MODULE, client_timeout, [self()]),

    MonitorReference =
	receive 
	    {dbRef, Ref}  ->
		MRef = erlang:monitor(process, Ref),
		Pid ! continue,
		MRef
	end,

    receive 
	{'DOWN', MonitorReference, _Type, _Object, _Info} ->
	    ok
    after 5000 ->
	    ct:fail(control_process_not_stopped)
    end.

client_timeout(Pid) ->
    {ok, Ref} =  odbc:connect(?RDBMS:connection_string(), odbc_test_lib:platform_options()),
    Pid ! {dbRef, Ref},
    receive 
	continue ->
	    ok
    end,
    exit(self(), timeout).


%%-------------------------------------------------------------------------
client_dies_error() ->
    [{doc,"Client dies with reason error."}].
client_dies_error(Config) when is_list(Config) ->
    Pid = spawn(?MODULE, client_error, [self()]),

    MonitorReference =
	receive 
	    {dbRef, Ref}  ->
		MRef = erlang:monitor(process, Ref),
		Pid ! continue,
		MRef
	end,

    receive 
	{'DOWN', MonitorReference, _Type, _Object, _Info} ->
	    ok
    after 5000 ->
	    ct:fail(control_process_not_stopped)
    end.

client_error(Pid) ->
    {ok, Ref} =  odbc:connect(?RDBMS:connection_string(), odbc_test_lib:platform_options()),
    Pid ! {dbRef, Ref},
    receive 
	continue ->
	    ok
    end,
    exit(self(), error).


%%-------------------------------------------------------------------------
connect_timeout() ->
    [{doc,"Test the timeout for the connect function."}].
connect_timeout(Config) when is_list(Config) ->
    {'EXIT',timeout} = (catch odbc:connect(?RDBMS:connection_string(),
					   [{timeout, 0}] ++
					       odbc_test_lib:platform_options())),
    %% Need to return ok here "{'EXIT',timeout} return value" will
    %% be interpreted as that the testcase has timed out.
    ok.

%%-------------------------------------------------------------------------
connect_port_timeout() ->
    [{"Test the timeout for the port program to connect back to the odbc "
     "application within the connect function."}].
connect_port_timeout(Config) when is_list(Config) ->
    %% Application environment var 'port_timeout' has been set to 0 by
    %% init_per_testcase/2.
    {error,timeout} = odbc:connect(?RDBMS:connection_string(),
                                   odbc_test_lib:platform_options()).

%%-------------------------------------------------------------------------
timeout() ->
    [{"Test that timeouts don't cause unwanted behavior sush as receiving"
     " an anwser to a previously tiemed out query."}].
timeout(Config)  when is_list(Config) ->

    {ok, Ref} =  odbc:connect(?RDBMS:connection_string(),
			      [{auto_commit, off}]),
    Table = proplists:get_value(tableName, Config),

    TransStr = transaction_support_str(?RDBMS),

    {updated, _} = 
	odbc:sql_query(Ref, 
		       "CREATE TABLE " ++ Table ++
		       " (ID integer, DATA varchar(10), PRIMARY KEY(ID))" ++ TransStr),

    {updated, 1} = 
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++ " VALUES(1,'bar')"),

    ok = odbc:commit(Ref, commit),

    {updated, 1} = 
	odbc:sql_query(Ref, "UPDATE " ++ Table ++
		       " SET DATA = 'foo' WHERE ID = 1"),

    {updated, 1} = 
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++ " VALUES(2,'baz')"),

    Pid = spawn_link(?MODULE, update_table_timeout, [Table, 5000, self()]),

    receive 
	timout_occurred ->
	    ok = odbc:commit(Ref, commit),
	    Pid ! continue
    end,

    receive 
	altered ->
	    ok
    end,

    {selected, Fields, [{"foobar"}]} = 
	odbc:sql_query(Ref, "SELECT DATA FROM " ++ Table ++ " WHERE ID = 1"),
    ["DATA"] = odbc_test_lib:to_upper(Fields),

    ok = odbc:commit(Ref, commit),
    ok = odbc:disconnect(Ref).

update_table_timeout(Table, TimeOut, Pid) ->

    {ok, Ref} =  odbc:connect(?RDBMS:connection_string(),
			      [{auto_commit, off}] ++ odbc_test_lib:platform_options()),
    UpdateQuery = "UPDATE " ++ Table ++ " SET DATA = 'foobar' WHERE ID = 1",

    case catch odbc:sql_query(Ref, UpdateQuery, TimeOut) of
	{'EXIT', timeout} ->
	    Pid ! timout_occurred;
	{updated, 1} ->
	    ct:fail(database_locker_failed)
    end,

    receive 
	continue ->
	    ok
    end,

    %% Make sure we receive the correct result and not the answer
    %% to the previous query.
    {selected, Fields, [{"baz"}]} = 
	odbc:sql_query(Ref, "SELECT DATA FROM " ++ Table ++ " WHERE ID = 2"),
    ["DATA"] = odbc_test_lib:to_upper(Fields),

    %% Do not check {updated, 1} as some drivers will return 0
    %% even though the update is done, which is checked by the test
    %% case when the altered message is recived.
    {updated, _} = odbc:sql_query(Ref, UpdateQuery, TimeOut),

    ok = odbc:commit(Ref, commit),

    Pid ! altered,

    ok = odbc:disconnect(Ref).
%%-------------------------------------------------------------------------
many_timeouts() ->
    [{doc, "Tests that many consecutive timeouts lead to that the connection "
     "is shutdown."}].
many_timeouts(Config) when is_list(Config) ->
    {ok, Ref} =  odbc:connect(?RDBMS:connection_string(),
			      [{auto_commit, off}] ++ odbc_test_lib:platform_options()),

    Table = proplists:get_value(tableName, Config),
    TransStr = transaction_support_str(?RDBMS),

    {updated, _} = 
	odbc:sql_query(Ref, 
		       "CREATE TABLE " ++ Table ++
		       " (ID integer, DATA varchar(10), PRIMARY KEY(ID))" ++ TransStr),

    {updated, 1} = 
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++ " VALUES(1,'bar')"),

    ok = odbc:commit(Ref, commit),

    {updated, 1} = 
	odbc:sql_query(Ref, "UPDATE " ++ Table ++
		       " SET DATA = 'foo' WHERE ID = 1"),

    _Pid = spawn_link(?MODULE, update_table_many_timeouts, 
		     [Table, 5000, self()]),

    receive 
	many_timeouts_occurred ->
	    ok
    end,

    ok = odbc:commit(Ref, commit),
    ok = odbc:disconnect(Ref).


update_table_many_timeouts(Table, TimeOut, Pid) ->

    {ok, Ref} =  odbc:connect(?RDBMS:connection_string(),
			      [{auto_commit, off}] ++ odbc_test_lib:platform_options()),
    UpdateQuery = "UPDATE " ++ Table ++ " SET DATA = 'foobar' WHERE ID = 1",

    ok = loop_many_timouts(Ref, UpdateQuery, TimeOut),

    Pid ! many_timeouts_occurred, 

    ok = odbc:disconnect(Ref).


loop_many_timouts(Ref, UpdateQuery, TimeOut) ->
    case catch odbc:sql_query(Ref, UpdateQuery, TimeOut) of
	{'EXIT',timeout} ->
	    loop_many_timouts(Ref, UpdateQuery, TimeOut);
	{updated, 1} ->
	    ct:fail(database_locker_failed);
	{error, connection_closed} ->
	    ok
    end.
%%-------------------------------------------------------------------------
timeout_reset() ->
    [{doc, "Check that the number of consecutive timouts is reset to 0 when "
      "a successful call to the database is made."}].
timeout_reset(Config) when is_list(Config) ->
    {ok, Ref} =  odbc:connect(?RDBMS:connection_string(),
			      [{auto_commit, off}] ++ odbc_test_lib:platform_options()),
    Table = proplists:get_value(tableName, Config),
    TransStr = transaction_support_str(?RDBMS),

    {updated, _} = 
	odbc:sql_query(Ref, 
		       "CREATE TABLE " ++ Table ++
		       " (ID integer, DATA varchar(10), PRIMARY KEY(ID))" ++  TransStr),

    {updated, 1} = 
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++ " VALUES(1,'bar')"),

    ok = odbc:commit(Ref, commit),

    {updated, 1} = 
	odbc:sql_query(Ref, "UPDATE " ++ Table ++
		       " SET DATA = 'foo' WHERE ID = 1"),

    {updated, 1} = 
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++ " VALUES(2,'baz')"),


    Pid = spawn_link(?MODULE, update_table_timeout_reset, 
		     [Table, 5000, self()]),

    receive 
	many_timeouts_occurred ->
	    ok
    end,

    ok = odbc:commit(Ref, commit),
    Pid ! continue,

    receive 
	altered ->
	    ok
    end,

    {selected, Fields, [{"foobar"}]} = 
	odbc:sql_query(Ref, "SELECT DATA FROM " ++ Table ++ " WHERE ID = 1"),
    ["DATA"] = odbc_test_lib:to_upper(Fields),

    ok = odbc:commit(Ref, commit),
    ok = odbc:disconnect(Ref).

update_table_timeout_reset(Table, TimeOut, Pid) ->

    {ok, Ref} = odbc:connect(?RDBMS:connection_string(),
			     [{auto_commit, off}] ++ odbc_test_lib:platform_options()),
    UpdateQuery = "UPDATE " ++ Table ++ " SET DATA = 'foobar' WHERE ID = 1",

    ok = loop_timout_reset(Ref, UpdateQuery, TimeOut, 
			   ?MAX_SEQ_TIMEOUTS-1),

    Pid ! many_timeouts_occurred,

    receive 
	continue ->
	    ok
    end,

    {selected, Fields, [{"baz"}]} =
	odbc:sql_query(Ref, "SELECT DATA FROM " ++ Table ++ " WHERE ID = 2"),
    ["DATA"] = odbc_test_lib:to_upper(Fields),

    %% Do not check {updated, 1} as some drivers will return 0
    %% even though the update is done, which is checked by the test
    %% case when the altered message is recived.
    {updated, _} = odbc:sql_query(Ref, UpdateQuery, TimeOut),

    ok = odbc:commit(Ref, commit),

    Pid ! altered,

    ok = odbc:disconnect(Ref).

loop_timout_reset(_, _, _, 0) ->
    ok;

loop_timout_reset(Ref, UpdateQuery, TimeOut, NumTimeouts) ->
    case catch odbc:sql_query(Ref, UpdateQuery, TimeOut) of
	{'EXIT',timeout} ->
	    loop_timout_reset(Ref, UpdateQuery, 
			      TimeOut, NumTimeouts - 1);
	{updated, 1} ->
	    ct:fail(database_locker_failed);
	{error, connection_closed} ->
	    ct:fail(connection_closed_premature)
    end.

%%-------------------------------------------------------------------------

disconnect_on_timeout() ->
    [{doc,"Check that disconnect after a time out works properly"}].
disconnect_on_timeout(Config) when is_list(Config) ->

    {ok, Ref} =  odbc:connect(?RDBMS:connection_string(),
			      [{auto_commit, off}] ++ odbc_test_lib:platform_options()),
    Table = proplists:get_value(tableName, Config),
    TransStr = transaction_support_str(?RDBMS),

    {updated, _} = 
	odbc:sql_query(Ref, 
		       "CREATE TABLE " ++ Table ++
		       " (ID integer, DATA varchar(10), PRIMARY KEY(ID))" ++ TransStr),

    {updated, 1} = 
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++ " VALUES(1,'bar')"),

    ok = odbc:commit(Ref, commit),

    {updated, 1} = 
	odbc:sql_query(Ref, "UPDATE " ++ Table ++
		       " SET DATA = 'foo' WHERE ID = 1"),


    _Pid = spawn_link(?MODULE, update_table_disconnect_on_timeout,
		     [Table, 5000, self()]),
    receive 
	ok ->
	    ok = odbc:commit(Ref, commit);
	nok ->
	    ct:fail(database_locker_failed)
    end.

update_table_disconnect_on_timeout(Table, TimeOut, Pid) ->

    {ok, Ref} =  odbc:connect(?RDBMS:connection_string(),
			      [{auto_commit, off}] ++ odbc_test_lib:platform_options()),
    UpdateQuery = "UPDATE " ++ Table ++ " SET DATA = 'foobar' WHERE ID = 1",

    case catch odbc:sql_query(Ref, UpdateQuery, TimeOut) of
	{'EXIT', timeout} ->
	    ok = odbc:disconnect(Ref),
	    Pid ! ok;
	{updated, 1} ->
	    Pid ! nok
    end.

%%-------------------------------------------------------------------------
connection_closed() ->
    [{doc, "Checks that you get an appropriate error message if you try to"
     " use a connection that has been closed"}].
connection_closed(Config) when is_list(Config) ->
    {ok, Ref} =  odbc:connect(?RDBMS:connection_string(), odbc_test_lib:platform_options()),

    Table = proplists:get_value(tableName, Config), 
    {updated, _} = 
	odbc:sql_query(Ref, 
		       "CREATE TABLE " ++ Table ++
		       " (ID integer, DATA char(10), PRIMARY KEY(ID))"),

    ok = odbc:disconnect(Ref),

    {error, connection_closed} = 
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++ " VALUES(1,'bar')"),
    {error, connection_closed} =  
	odbc:select_count(Ref, "SELECT * FROM " ++ Table),
    {error, connection_closed} = odbc:first(Ref),
    {error, connection_closed} = odbc:last(Ref),
    {error, connection_closed} = odbc:next(Ref),
    {error, connection_closed} = odbc:prev(Ref),
    {error, connection_closed} = odbc:select(Ref, next, 3),
    {error, connection_closed} = odbc:commit(Ref, commit).

%%-------------------------------------------------------------------------
disable_scrollable_cursors() ->
    [{doc,"Test disabling of scrollable cursors."}].
disable_scrollable_cursors(Config) when is_list(Config) ->
    {ok, Ref} =  odbc:connect(?RDBMS:connection_string(),
			      [{scrollable_cursors, off}]),

    Table = proplists:get_value(tableName, Config),

    {updated, _} = 
	odbc:sql_query(Ref, 
		       "CREATE TABLE " ++ Table ++
		       " (ID integer, DATA varchar(10), PRIMARY KEY(ID))"),

    {updated, _} = 
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++ " VALUES(1,'bar')"),

    {ok, _} = odbc:select_count(Ref, "SELECT ID FROM " ++ Table),

    NextResult = ?RDBMS:selected_ID(1, next),

    ct:pal("Expected: ~p~n", [NextResult]),

    Result = odbc:next(Ref),
    ct:pal("Got: ~p~n", [Result]),
    NextResult = Result,

    {error, scrollable_cursors_disabled} = odbc:first(Ref),
    {error, scrollable_cursors_disabled} = odbc:last(Ref),
    {error, scrollable_cursors_disabled} = odbc:prev(Ref),
    {error, scrollable_cursors_disabled} = 
	odbc:select(Ref, {relative, 2}, 5),
    {error, scrollable_cursors_disabled} =
	odbc:select(Ref, {absolute, 2}, 5),

    {selected, _ColNames,[]} = odbc:select(Ref, next, 1).

%%-------------------------------------------------------------------------
return_rows_as_lists()->
    [{doc,"Test the option that a row may be returned as a list instead " 
     "of a tuple. Too be somewhat backward compatible."}].
return_rows_as_lists(Config) when is_list(Config) ->
    {ok, Ref} =  odbc:connect(?RDBMS:connection_string(),
			      [{tuple_row, off}] ++ odbc_test_lib:platform_options()),

    Table = proplists:get_value(tableName, Config),

    {updated, _} = 
	odbc:sql_query(Ref, 
		       "CREATE TABLE " ++ Table ++
		       " (ID integer, DATA varchar(10), PRIMARY KEY(ID))"),

    {updated, _} = 
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++ " VALUES(1,'bar')"),

    {updated, _} = 
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++ " VALUES(2,'foo')"),

    ListRows = ?RDBMS:selected_list_rows(),
    ListRows = 
	odbc:sql_query(Ref, "SELECT * FROM " ++ Table),

    {ok, _} = odbc:select_count(Ref, "SELECT * FROM " ++ Table),

    case proplists:get_value(scrollable_cursors, odbc_test_lib:platform_options()) of
	off ->
	    Next = ?RDBMS:next_list_rows(),
	    Next = odbc:next(Ref);
	_ ->
	    First = ?RDBMS:first_list_rows(),
	    Last =  ?RDBMS:last_list_rows(),
	    Prev = ?RDBMS:prev_list_rows(),
	    Next = ?RDBMS:next_list_rows(),

	    Last = odbc:last(Ref),
	    Prev = odbc:prev(Ref),
	    First = odbc:first(Ref),
	    Next = odbc:next(Ref)
    end.

%%-------------------------------------------------------------------------

api_missuse()->
    [{doc,"Test that behaviour of the control process if the api is abused"}].
api_missuse(Config) when is_list(Config)->

    {ok, Ref} =  odbc:connect(?RDBMS:connection_string(), odbc_test_lib:platform_options()),
    %% Serious programming fault, connetion will be shut down 
    gen_server:call(Ref, {self(), foobar, 10}, infinity),
    ct:sleep(10),
    undefined = process_info(Ref, status),

    {ok, Ref2} =  odbc:connect(?RDBMS:connection_string(),
			       odbc_test_lib:platform_options()),
    %% Serious programming fault, connetion will be shut down 
    gen_server:cast(Ref2, {self(), foobar, 10}),
    ct:sleep(10),
    undefined = process_info(Ref2, status),

    {ok, Ref3} =  odbc:connect(?RDBMS:connection_string(),
			       odbc_test_lib:platform_options()),
    %% Could be an innocent misstake the connection lives. 
    Ref3 ! foobar, 
    ct:sleep(10),
    {status, _} = process_info(Ref3, status).

transaction_support_str(mysql) ->
    "ENGINE = InnoDB";
transaction_support_str(_) ->
    "".


%%-------------------------------------------------------------------------
extended_errors()->
    [{doc, 
      "Test the extended errors connection option: When off; the old behaviour of just an error "
      "string is returned on error. When on, the error string is replaced by a 3 element tuple "
      "that also exposes underlying ODBC provider error codes."}].
extended_errors(Config) when is_list(Config)->
    Table = proplists:get_value(tableName, Config),
    {ok, Ref} = odbc:connect(?RDBMS:connection_string(), odbc_test_lib:platform_options()),
    {updated, _} = odbc:sql_query(Ref, "create table " ++ Table ++" ( id integer, data varchar(10))"),

    % Error case WITHOUT extended errors on...
    case odbc:sql_query(Ref, "create table " ++ Table ++" ( id integer, data varchar(10))") of
        {error, ErrorString} when is_list(ErrorString) -> ok
    end,

    % Now the test case with extended errors on - This should return a tuple, not a list/string now.
    % The first element is a string that is the ODBC error string; the 2nd element is a native integer error
    % code passed from the underlying provider driver. The last is the familiar old error string.
    % We can't check the actual error code; as each different underlying provider will return
    % a different value - So we just check the return types at least.
    {ok, RefExtended} = odbc:connect(?RDBMS:connection_string(), odbc_test_lib:platform_options() ++ [{extended_errors, on}]),
    case odbc:sql_query(RefExtended, "create table " ++ Table ++" ( id integer, data varchar(10))") of
        {error, {ODBCCodeString, NativeCodeNum, ShortErrorString}} when is_list(ODBCCodeString), is_number(NativeCodeNum), is_list(ShortErrorString) -> ok
    end,

    ok = odbc:disconnect(Ref),
    ok = odbc:disconnect(RefExtended).


is_odbcserver(Name) ->
    case re:run(Name, "odbcserver") of
	{match, _} ->
	    true;
	_ ->
	    false
    end.

