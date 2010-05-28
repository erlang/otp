%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2010. All Rights Reserved.
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

-module(odbc_data_type_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("test_server.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("test_server_line.hrl").
-include("odbc_test.hrl").

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
all(doc) ->
    ["Tests data types"];
all(suite) ->
    case odbc_test_lib:odbc_check() of
	ok -> all();
	Other -> {skip,Other}
    end.						  

all() ->
    [char, int, floats, dec_and_num, timestamp].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config) -> Config
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Initiation before the whole suite
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    application:start(odbc),
    [{tableName, odbc_test_lib:unique_table_name()} | Config].

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    application:stop(odbc),
    ok.

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
init_per_testcase(Case, Config) ->
    case atom_to_list(Case) of
	"binary" ++ _  ->
	    {ok, Ref} = odbc:connect(?RDBMS:connection_string(), 
				     [{binary_strings, on}]);
	"unicode" ->
	    {ok, Ref} = odbc:connect(?RDBMS:connection_string(), 
				     [{binary_strings, on}]);
	_ ->
	    {ok, Ref} = odbc:connect(?RDBMS:connection_string(), [])
    end,
    Dog = test_server:timetrap(?default_timeout),
    Temp = lists:keydelete(connection_ref, 1, Config),
    NewConfig = lists:keydelete(watchdog, 1, Temp),
    [{watchdog, Dog}, {connection_ref, Ref} | NewConfig].

%%--------------------------------------------------------------------
%% Function: end_per_testcase(Case, Config) -> _
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after each test case
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, Config) ->
    Ref = ?config(connection_ref, Config),
    ok = odbc:disconnect(Ref),
    %% Clean up if needed 
    Table = ?config(tableName, Config),
    {ok, NewRef} = odbc:connect(?RDBMS:connection_string(), []),
    odbc:sql_query(NewRef, "DROP TABLE " ++ Table), 
    odbc:disconnect(NewRef),
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------
char(doc) ->
    ["Tests char data types"]; 

char(suite) ->
    [char_fixed_lower_limit, char_fixed_upper_limit,
     char_fixed_padding, varchar_lower_limit, varchar_upper_limit,
     varchar_no_padding, text_lower_limit, text_upper_limit, unicode
    ].

char_fixed_lower_limit(doc) ->
    ["Tests fixed length char data type lower boundaries."];
char_fixed_lower_limit(suite) ->
    [];
char_fixed_lower_limit(Config) when is_list(Config) ->
    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),

    %% Below limit
    {error, _} = 
	odbc:sql_query(Ref, 
		       "CREATE TABLE " ++ Table ++    
		       ?RDBMS:create_fixed_char_table(
			  (?RDBMS:fixed_char_min() - 1))), 
    %% Lower limit
    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       ?RDBMS:create_fixed_char_table(
			  ?RDBMS:fixed_char_min())), 

    %% Right length data
    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++
		       "'" ++ string:chars($a, ?RDBMS:fixed_char_min())
		       ++ "')"),
    %% Select data
    {selected, Fields,[{"a"}]} =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),

    ["FIELD"] = odbc_test_lib:to_upper(Fields),

    %% Too long data
    {error, _}  =  
	odbc:sql_query(Ref,"INSERT INTO " ++ Table ++" VALUES(" ++
		       "'" ++ string:chars($a, 
					   (?RDBMS:fixed_char_min()
					    + 1)) 
		       ++ "')"),		
    ok.
%%-------------------------------------------------------------------------

char_fixed_upper_limit(doc) ->
    ["Tests fixed length char data type upper boundaries."];
char_fixed_upper_limit(suite) ->
    [];
char_fixed_upper_limit(Config) when is_list(Config) ->

    case ?RDBMS of
	postgres ->
	    {skip, "Limit unknown"};
	_ ->
	    Ref = ?config(connection_ref, Config),   
	    Table = ?config(tableName, Config),
	    
	    %% Upper limit 
	    {updated, _} =  % Value == 0 || -1 driver dependent!
		odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
			       ?RDBMS:create_fixed_char_table(
				  ?RDBMS:fixed_char_max())), 
	    {updated, _} =  
		odbc:sql_query(Ref,"INSERT INTO " ++ Table ++" VALUES(" ++ 
			       "'" ++ string:chars($a, 
						   ?RDBMS:fixed_char_max())
			       ++ "')"),
	    %% Select data
	    {selected, Fields, [{CharStr}]} =
		odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
	    true = length(CharStr) == ?RDBMS:fixed_char_max(),
	    
	    ["FIELD"] = odbc_test_lib:to_upper(Fields),
	    
	    %% Too long data
	    {error, _} =  
		odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++  
			       "'" ++ string:chars($a, 
						   (?RDBMS:fixed_char_max()
						    + 1)) 
			       ++ "')"),		
	    %% Clean up
	    {updated, _} =  % Value == 0 || -1 driver dependent!
		odbc:sql_query(Ref, "DROP TABLE " ++ Table),
	    
	    %% Above limit 
	    {error, _} =
		odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++  
			       ?RDBMS:create_fixed_char_table(
				  (?RDBMS:fixed_char_max() + 1))), 
	    ok
    end.

%%-------------------------------------------------------------------------

char_fixed_padding(doc) ->
    ["Tests that data that is shorter than the given size is padded " 
     "with blanks."];
char_fixed_padding(suite) ->
    [];
char_fixed_padding(Config) when is_list(Config) ->
    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),

    %% Data should be padded with blanks
    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       ?RDBMS:create_fixed_char_table(
			  ?RDBMS:fixed_char_max())), 

    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
		       "'" ++ string:chars($a, 
					   ?RDBMS:fixed_char_min())
		       ++ "')"),

    {selected, Fields, [{CharStr}]} =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
    true = length(CharStr) == ?RDBMS:fixed_char_max(),
    ["FIELD"] = odbc_test_lib:to_upper(Fields),
    ok.
%%-------------------------------------------------------------------------

varchar_lower_limit(doc) ->
    ["Tests variable length char data type lower boundaries."];
varchar_lower_limit(suite) ->
    [];
varchar_lower_limit(Config) when is_list(Config) ->
    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),

    %% Below limit
    {error, _} = 
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       ?RDBMS:create_var_char_table(
			  ?RDBMS:var_char_min() - 1)), 
    %% Lower limit
    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       ?RDBMS:create_var_char_table(
			  ?RDBMS:var_char_min())), 

    %% Right length data
    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
		       "'" ++ string:chars($a, ?RDBMS:var_char_min())
		       ++ "')"),
    %% Select data
    {selected, Fields, [{"a"}]} =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
    
    ["FIELD"] = odbc_test_lib:to_upper(Fields),

    %% Too long data
    {error, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
		       "'" ++ string:chars($a, 
					   (?RDBMS:var_char_min()+1)) 
		       ++ "')"),		
    ok.

%%-------------------------------------------------------------------------

varchar_upper_limit(doc) ->
    ["Tests variable length char data type upper boundaries."];
varchar_upper_limit(suite) ->
    [];
varchar_upper_limit(Config) when is_list(Config) ->
    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),

    case ?RDBMS of
	oracle ->
	    {skip, "Known bug in database"};
	postgres ->
	    {skip, "Limit unknown"};
	_ ->
	    %% Upper limit 
	    {updated, _} =  % Value == 0 || -1 driver dependent!
		odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
			       ?RDBMS:create_var_char_table(
				  ?RDBMS:var_char_max())), 
	    {updated, _} =  
		odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
			       "'" ++ string:chars($a, 
						   ?RDBMS:var_char_max())
			       ++ "')"),
	    
	    {selected, Fields, [{CharStr}]} =
		odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
	    true = length(CharStr) == ?RDBMS:var_char_max(),
	    
	    ["FIELD"] = odbc_test_lib:to_upper(Fields),
	    
	    %% Too long data
	    {error, _} =  
		odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
			       "'" ++ string:chars($a, 
						   (?RDBMS:var_char_max()+1)) 
		       ++ "')"),
	    %% Clean up
	    {updated, _} =  % Value == 0 || -1 driver dependent!
		odbc:sql_query(Ref, "DROP TABLE " ++ Table), 
	    
	    %% Above limit 
	    {error, _} =
		odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
			       ?RDBMS:create_var_char_table(
				  (?RDBMS:var_char_max() + 1))),
	    ok
    end.
%%-------------------------------------------------------------------------

varchar_no_padding(doc) ->
    ["Tests that data that is shorter than the given max size is not padded " 
     "with blanks."];
varchar_no_padding(suite) ->
    [];
varchar_no_padding(Config) when is_list(Config) ->
    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),

    %% Data should NOT be padded with blanks
    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       ?RDBMS:create_var_char_table(
			  ?RDBMS:var_char_max())), 
    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
		       "'" ++ string:chars($a, ?RDBMS:var_char_min())
		       ++ "')"),

    {selected, Fields, [{CharStr}]} =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
    true = length(CharStr) /= ?RDBMS:var_char_max(),
    ["FIELD"] = odbc_test_lib:to_upper(Fields),
    ok.

%%-------------------------------------------------------------------------

text_lower_limit(doc) ->
    ["Tests 'long' char data type lower boundaries."];
text_lower_limit(suite) ->
    [];
text_lower_limit(Config) when is_list(Config) ->
    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       ?RDBMS:create_text_table()), 

    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
		       "'" ++ string:chars($a, ?RDBMS:text_min())
		       ++ "')"),

    {selected, Fields, [{"a"}]} =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
    ["FIELD"] = odbc_test_lib:to_upper(Fields),
    ok.

%%-------------------------------------------------------------------------

text_upper_limit(doc) ->
    [];
text_upper_limit(suite) ->
    [];
text_upper_limit(Config) when is_list(Config) ->
    
    {skip,"Consumes too much resources" }.
%%     Ref = ?config(connection_ref, Config),
%%     Table = ?config(tableName, Config),

%%     {updated, _} =  % Value == 0 || -1 driver dependent!
%% 	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
%% 		       ?RDBMS:create_text_table()), 
%%     {updated, _} =  
%% 	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
%% 		       "'" ++ string:chars($a, ?RDBMS:text_max())
%% 		       ++ "')"),

%%     {selected, Fields, [{CharStr}]} =
%% 	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
%%     length(CharStr) == ?RDBMS:text_max(),
%%     ["FIELD"] = odbc_test_lib:to_upper(Fields),
    
%%     {error, _} =  
%% 	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
%% 		       "'" ++ string:chars($a, (?RDBMS:text_max()+1)) 
%% 		       ++ "')"),		
%%     ok.

%%-------------------------------------------------------------------------
binary_char(doc) ->
    ["Tests char data types returned as erlang binaries"]; 

binary_char(suite) ->
    [binary_char_fixed_lower_limit, binary_char_fixed_upper_limit,
     binary_char_fixed_padding, binary_varchar_lower_limit, binary_varchar_upper_limit,
     binary_varchar_no_padding, binary_text_lower_limit, binary_text_upper_limit, unicode
    ].

binary_char_fixed_lower_limit(doc) ->
    ["Tests fixed length char data type lower boundaries."];
binary_char_fixed_lower_limit(suite) ->
    [];
binary_char_fixed_lower_limit(Config) when is_list(Config) ->
    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),

    %% Below limit
    {error, _} = 
	odbc:sql_query(Ref, 
		       "CREATE TABLE " ++ Table ++    
		       ?RDBMS:create_fixed_char_table(
			  (?RDBMS:fixed_char_min() - 1))), 
    %% Lower limit
    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       ?RDBMS:create_fixed_char_table(
			  ?RDBMS:fixed_char_min())), 

    %% Right length data
    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++
		       "'" ++ string:chars($a, ?RDBMS:fixed_char_min())
		       ++ "')"),
    %% Select data
    {selected, Fields,[{<<"a">>}]} =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),

    ["FIELD"] = odbc_test_lib:to_upper(Fields),

    %% Too long data
    {error, _}  =  
	odbc:sql_query(Ref,"INSERT INTO " ++ Table ++" VALUES(" ++
		       "'" ++ string:chars($a, 
					   (?RDBMS:fixed_char_min()
					    + 1)) 
		       ++ "')"),		
    ok.
%%-------------------------------------------------------------------------

binary_char_fixed_upper_limit(doc) ->
    ["Tests fixed length char data type upper boundaries."];
binary_char_fixed_upper_limit(suite) ->
    [];
binary_char_fixed_upper_limit(Config) when is_list(Config) ->

    case ?RDBMS of
	postgres ->
	    {skip, "Limit unknown"};
	_ ->
	    Ref = ?config(connection_ref, Config),   
	    Table = ?config(tableName, Config),
	    
	    %% Upper limit 
	    {updated, _} =  % Value == 0 || -1 driver dependent!
		odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
			       ?RDBMS:create_fixed_char_table(
				  ?RDBMS:fixed_char_max())), 
	    {updated, _} =  
		odbc:sql_query(Ref,"INSERT INTO " ++ Table ++" VALUES(" ++ 
			       "'" ++ string:chars($a, 
						   ?RDBMS:fixed_char_max())
			       ++ "')"),
	    %% Select data
	    {selected, Fields, [{CharBin}]} =
		odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
	    true = size(CharBin) == ?RDBMS:fixed_char_max(),
	    
	    ["FIELD"] = odbc_test_lib:to_upper(Fields),
	    
	    %% Too long data
	    {error, _} =  
		odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++  
			       "'" ++ string:chars($a, 
						   (?RDBMS:fixed_char_max()
						    + 1)) 
			       ++ "')"),		
	    %% Clean up
	    {updated, _} =  % Value == 0 || -1 driver dependent!
		odbc:sql_query(Ref, "DROP TABLE " ++ Table),
	    
	    %% Above limit 
	    {error, _} =
		odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++  
			       ?RDBMS:create_fixed_char_table(
				  (?RDBMS:fixed_char_max() + 1))), 
	    ok
    end.

%%-------------------------------------------------------------------------

binary_char_fixed_padding(doc) ->
    ["Tests that data that is shorter than the given size is padded " 
     "with blanks."];
binary_char_fixed_padding(suite) ->
    [];
binary_char_fixed_padding(Config) when is_list(Config) ->
    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),

    %% Data should be padded with blanks
    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       ?RDBMS:create_fixed_char_table(
			  ?RDBMS:fixed_char_max())), 

    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
		       "'" ++ string:chars($a, 
					   ?RDBMS:fixed_char_min())
		       ++ "')"),

    {selected, Fields, [{CharBin}]} =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
    true = size(CharBin) == ?RDBMS:fixed_char_max(),
    ["FIELD"] = odbc_test_lib:to_upper(Fields),
    ok.
%%-------------------------------------------------------------------------

binary_varchar_lower_limit(doc) ->
    ["Tests variable length char data type lower boundaries."];
binary_varchar_lower_limit(suite) ->
    [];
binary_varchar_lower_limit(Config) when is_list(Config) ->
    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),

    %% Below limit
    {error, _} = 
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       ?RDBMS:create_var_char_table(
			  ?RDBMS:var_char_min() - 1)), 
    %% Lower limit
    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       ?RDBMS:create_var_char_table(
			  ?RDBMS:var_char_min())), 

    %% Right length data
    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
		       "'" ++ string:chars($a, ?RDBMS:var_char_min())
		       ++ "')"),
    %% Select data
    {selected, Fields, [{<<"a">>}]} =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
    
    ["FIELD"] = odbc_test_lib:to_upper(Fields),

    %% Too long data
    {error, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
		       "'" ++ string:chars($a, 
					   (?RDBMS:var_char_min()+1)) 
		       ++ "')"),		
    ok.

%%-------------------------------------------------------------------------

binary_varchar_upper_limit(doc) ->
    ["Tests variable length char data type upper boundaries."];
binary_varchar_upper_limit(suite) ->
    [];
binary_varchar_upper_limit(Config) when is_list(Config) ->
    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),

    case ?RDBMS of
	oracle ->
	    {skip, "Known bug in database"};
	postgres ->
	    {skip, "Limit unknown"};
	_ ->
	    %% Upper limit 
	    {updated, _} =  % Value == 0 || -1 driver dependent!
		odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
			       ?RDBMS:create_var_char_table(
				  ?RDBMS:var_char_max())), 
	    {updated, _} =  
		odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
			       "'" ++ string:chars($a, 
						   ?RDBMS:var_char_max())
			       ++ "')"),
	    
	    {selected, Fields, [{CharBin}]} =
		odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
	    true = size(CharBin) == ?RDBMS:var_char_max(),
	    
	    ["FIELD"] = odbc_test_lib:to_upper(Fields),
	    
	    %% Too long data
	    {error, _} =  
		odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
			       "'" ++ string:chars($a, 
						   (?RDBMS:var_char_max()+1)) 
		       ++ "')"),
	    %% Clean up
	    {updated, _} =  % Value == 0 || -1 driver dependent!
		odbc:sql_query(Ref, "DROP TABLE " ++ Table), 
	    
	    %% Above limit 
	    {error, _} =
		odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
			       ?RDBMS:create_var_char_table(
				  (?RDBMS:var_char_max() + 1))),
	    ok
    end.
%%-------------------------------------------------------------------------

binary_varchar_no_padding(doc) ->
    ["Tests that data that is shorter than the given max size is not padded " 
     "with blanks."];
binary_varchar_no_padding(suite) ->
    [];
binary_varchar_no_padding(Config) when is_list(Config) ->
    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),

    %% Data should NOT be padded with blanks
    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       ?RDBMS:create_var_char_table(
			  ?RDBMS:var_char_max())), 
    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
		       "'" ++ string:chars($a, ?RDBMS:var_char_min())
		       ++ "')"),

    {selected, Fields, [{CharBin}]} =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
    true = size(CharBin) /= ?RDBMS:var_char_max(),
    ["FIELD"] = odbc_test_lib:to_upper(Fields),
    ok.

%%-------------------------------------------------------------------------

binary_text_lower_limit(doc) ->
    ["Tests 'long' char data type lower boundaries."];
binary_text_lower_limit(suite) ->
    [];
binary_text_lower_limit(Config) when is_list(Config) ->
    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       ?RDBMS:create_text_table()), 

    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
		       "'" ++ string:chars($a, ?RDBMS:text_min())
		       ++ "')"),

    {selected, Fields, [{<<"a">>}]} =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
    ["FIELD"] = odbc_test_lib:to_upper(Fields),
    ok.

%%-------------------------------------------------------------------------

binary_text_upper_limit(doc) ->
    [];
binary_text_upper_limit(suite) ->
    [];
binary_text_upper_limit(Config) when is_list(Config) ->
    
    {skip,"Consumes too much resources" }.
%%     Ref = ?config(connection_ref, Config),
%%     Table = ?config(tableName, Config),

%%     {updated, _} =  % Value == 0 || -1 driver dependent!
%% 	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
%% 		       ?RDBMS:create_text_table()), 
%%     {updated, _} =  
%% 	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
%% 		       "'" ++ string:chars($a, ?RDBMS:text_max())
%% 		       ++ "')"),

%%     {selected, Fields, [{CharBin}]} =
%% 	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
%%     size(CharBin) == ?RDBMS:text_max(),
%%     ["FIELD"] = odbc_test_lib:to_upper(Fields),
    
%%     {error, _} =  
%% 	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
%% 		       "'" ++ string:chars($a, (?RDBMS:text_max()+1)) 
%% 		       ++ "')"),		
%%     ok.


%%-------------------------------------------------------------------------

int(doc) ->
    ["Tests integer data types"]; 

int(suite) ->
    [tiny_int_lower_limit, tiny_int_upper_limit, small_int_lower_limit,
     small_int_upper_limit, int_lower_limit, int_upper_limit,
     big_int_lower_limit, big_int_upper_limit, bit_false, bit_true].

%%-------------------------------------------------------------------------

tiny_int_lower_limit(doc) ->
    ["Tests integer of type tinyint."];
tiny_int_lower_limit(suite) ->
    [];
tiny_int_lower_limit(Config) when is_list(Config) -> 
    case ?RDBMS of
	postgres ->
	    {skip, "Type tiniyint not supported"};
	_ ->
	    Ref = ?config(connection_ref, Config),   
	    Table = ?config(tableName, Config),
	    
	    {updated, _} =  % Value == 0 || -1 driver dependent!
		odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
			       ?RDBMS:create_tiny_int_table()), 
	    
	    {updated, _} =  
		odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
			       "'" ++ integer_to_list(?RDBMS:tiny_int_min())
			       ++ "')"),
	    
	    SelectResult =  ?RDBMS:tiny_int_min_selected(),
	    SelectResult =
		odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
	    
	    {error, _} =  
		odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
			       "'" ++ integer_to_list(?RDBMS:tiny_int_min() 
						      - 1)
			       ++ "')"),
	    ok
    end.

%%-------------------------------------------------------------------------

tiny_int_upper_limit(doc) ->
    ["Tests integer of type tinyint."];
tiny_int_upper_limit(suite) ->
    [];
tiny_int_upper_limit(Config) when is_list(Config) ->
    case ?RDBMS of
	postgres ->
	    {skip, "Type tiniyint not supported"};
	_ ->
	    Ref = ?config(connection_ref, Config),   
	    Table = ?config(tableName, Config),
	    
	    {updated, _} =  % Value == 0 || -1 driver dependent!
		odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
			       ?RDBMS:create_tiny_int_table()), 
	    
	    {updated, _} =  
		odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
			       "'" ++ integer_to_list(?RDBMS:tiny_int_max())
			       ++ "')"),
	    
	    SelectResult =  ?RDBMS:tiny_int_max_selected(),
	    SelectResult =
		odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
	    
	    {error, _} =  
		odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
			       "'" ++ integer_to_list(?RDBMS:tiny_int_max()
						      + 1)
			       ++ "')"),
	    ok
    end.

%%-------------------------------------------------------------------------

small_int_lower_limit(doc) ->
    ["Tests integer of type smallint."];
small_int_lower_limit(suite) ->
    [];
small_int_lower_limit(Config) when is_list(Config) ->
    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       ?RDBMS:create_small_int_table()), 

    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
		       "'" ++ integer_to_list(?RDBMS:small_int_min())
		       ++ "')"),

    SelectResult = ?RDBMS:small_int_min_selected(),
    SelectResult =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),

    {error, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
		       "'"  ++ integer_to_list(?RDBMS:small_int_min()
					       - 1)
		       ++ "')"),
    ok.

%%-------------------------------------------------------------------------

small_int_upper_limit(doc) ->
    ["Tests integer of type smallint."];
small_int_upper_limit(suite) ->
    [];
small_int_upper_limit(Config) when is_list(Config) ->
    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       ?RDBMS:create_small_int_table()), 

    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
		       "'" ++ integer_to_list(?RDBMS:small_int_max())
		       ++ "')"),

    SelectResult = ?RDBMS:small_int_max_selected(),
    SelectResult =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),

    {error, _} =  
	odbc:sql_query(Ref,"INSERT INTO " ++ Table ++" VALUES(" ++  
		       "'" ++ integer_to_list(?RDBMS:small_int_max() 
					      + 1)
		       ++ "')"),
    ok.

%%-------------------------------------------------------------------------
int_lower_limit(doc) ->
    ["Tests integer of type int."];
int_lower_limit(suite) ->
    [];
int_lower_limit(Config) when is_list(Config) ->
    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       ?RDBMS:create_int_table()), 

    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
		       "'" ++ integer_to_list(?RDBMS:int_min())
		       ++ "')"),

    SelectResult = ?RDBMS:int_min_selected(),
    SelectResult =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),

    {error, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
		       "'" ++ integer_to_list(?RDBMS:int_min() - 1)
		       ++ "')"),
    ok.

%%-------------------------------------------------------------------------

int_upper_limit(doc) ->
    ["Tests integer of type int."];
int_upper_limit(suite) ->
    [];
int_upper_limit(Config) when is_list(Config) ->
    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref, "CREATE TABLE " ++ Table ++ 
		       ?RDBMS:create_int_table()), 

    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
		       "'" ++ integer_to_list(?RDBMS:int_max())
		       ++ "')"),

    SelectResult = ?RDBMS:int_max_selected(),
    SelectResult =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),

    {error, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
		       "'" ++ integer_to_list(?RDBMS:int_max() + 1)
		       ++ "')"),    
    ok.


%%-------------------------------------------------------------------------
big_int_lower_limit(doc) ->
    ["Tests integer of type bigint"];
big_int_lower_limit(suite) ->
    [];
big_int_lower_limit(Config) when is_list(Config) ->
    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       ?RDBMS:create_big_int_table()), 

    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
		       "'" ++ integer_to_list(?RDBMS:big_int_min())
		       ++ "')"),

    SelectResult = ?RDBMS:big_int_min_selected(),
    SelectResult =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),

    {error, _} =  
 	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
		       "'" ++ integer_to_list(?RDBMS:big_int_min() 
					      - 1)
		       ++ "')"),
    ok.

%%-------------------------------------------------------------------------

big_int_upper_limit(doc) ->
    ["Tests integer of type bigint."];
big_int_upper_limit(suite) ->
    [];
big_int_upper_limit(Config) when is_list(Config) ->
    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       ?RDBMS:create_big_int_table()), 

    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
		       "'" ++ integer_to_list(?RDBMS:big_int_max())
		       ++ "')"),

    SelectResult = ?RDBMS:big_int_max_selected(),
    SelectResult =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),

    {error, _} =  
 	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
		       "'" ++ integer_to_list(?RDBMS:big_int_max() 
					      + 1)
		       ++ "')"),
    ok.
%%-------------------------------------------------------------------------

bit_false(doc) ->
    [""];
bit_false(suite) ->
    [];
bit_false(Config) when is_list(Config) ->  
    case ?RDBMS of
	oracle ->
	    {skip, "Not supported by driver"};
	_ ->
	    Ref = ?config(connection_ref, Config),   
	    Table = ?config(tableName, Config),
	    
	    {updated, _} =  % Value == 0 || -1 driver dependent!
		odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
			       ?RDBMS:create_bit_table()), 
	    
	    {updated, _} =  
		odbc:sql_query(Ref, "INSERT INTO " ++ Table ++
			       " VALUES(" ++ 	
			       "'" ++ integer_to_list(?RDBMS:bit_false())
			       ++ "')"),
	    
	    SelectResult = ?RDBMS:bit_false_selected(),
	    SelectResult =
		odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
	    
	    {error, _} =  
		odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
			       "'" ++ integer_to_list(-1)
			       ++ "')"),
	    ok
    end.

%%-------------------------------------------------------------------------

bit_true(doc) ->
    [""];
bit_true(suite) ->
    [];
bit_true(Config) when is_list(Config) ->
    case ?RDBMS of
	oracle ->
	    {skip, "Not supported by driver"};
	_ ->
	    Ref = ?config(connection_ref, Config),   
	    Table = ?config(tableName, Config),
	    
	    
	    {updated, _} =  % Value == 0 || -1 driver dependent!
		odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
			       ?RDBMS:create_bit_table()), 
	    
	    {updated, _} =  
		odbc:sql_query(Ref, 
			       "INSERT INTO " ++ Table ++" VALUES(" ++ 
			       "'" ++ integer_to_list(?RDBMS:bit_true())
			       ++ "')"),
	    
	    SelectResult =  ?RDBMS:bit_true_selected(),
	    SelectResult =
		odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
	    
	    {error, _} =  
		odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
			       "'" ++ integer_to_list(-1)
			       ++ "')"),
	    ok
    end.

%%-------------------------------------------------------------------------

floats(doc) ->
    ["Test the datatype float."];
floats(suite) ->
    [float_lower_limit, float_upper_limit, float_zero, real_zero].

%%-------------------------------------------------------------------------
float_lower_limit(doc) ->
    [""];
float_lower_limit(suite) ->
    [];
float_lower_limit(Config) when is_list(Config) ->   

    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       ?RDBMS:create_float_table()), 

    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
		       "'" ++ float_to_list(
				?RDBMS:float_min())
		       ++ "')"),
    {selected,[_ColName],[{MinFloat}]} = 
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),

    true = ?RDBMS:float_min() == MinFloat,

    case ?RDBMS of
	oracle ->
	    {updated, _} =  % Value == 0 || -1 driver dependent!
		odbc:sql_query(Ref, "DROP TABLE " ++ Table), 

	    {updated, _} =  % Value == 0 || -1 driver dependent!
		odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
			       ?RDBMS:create_float_table()), 

	    {updated, _} =  
		odbc:sql_query(Ref, 
			       "INSERT INTO " ++ Table ++" VALUES(" ++ 
			       ?RDBMS:float_underflow() ++ ")"),

	    SelectResult = ?RDBMS:float_zero_selected(),
	    SelectResult =
		odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table);
	_ ->
	    {error, _} =  
		odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
			       ?RDBMS:float_underflow() ++ ")")
    end,
    ok.


%%-------------------------------------------------------------------------
float_upper_limit(doc) ->
    [""];
float_upper_limit(suite) ->
    [];
float_upper_limit(Config) when is_list(Config) ->   
    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       ?RDBMS:create_float_table()), 

    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
		       "'" ++ float_to_list(
				?RDBMS:float_max())
		       ++ "')"),


    {selected,[_ColName],[{MaxFloat}]}
	= odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),

    true = ?RDBMS:float_max() == MaxFloat,

    {error, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
		       ?RDBMS:float_overflow() ++ ")"),
    ok.

%%-------------------------------------------------------------------------
float_zero(doc) ->
    ["Test the float value zero."];
float_zero(suite) ->
    [];
float_zero(Config) when is_list(Config) ->   
    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       ?RDBMS:create_float_table()), 

    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES('0')"),

    SelectResult = ?RDBMS:float_zero_selected(),
    SelectResult =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
    ok.
%%-------------------------------------------------------------------------
real_zero(doc) ->
    ["Test the real value zero."];
real_zero(suite) ->
    [];
real_zero(Config) when is_list(Config) ->   
    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),

    case ?RDBMS of
	oracle ->
	    {skip, "Not supported in Oracle"};  
        _ ->	    
	    {updated, _} =  % Value == 0 || -1 driver dependent!
		odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
			       ?RDBMS:create_real_table()), 

	    {updated, _} =  
		odbc:sql_query(Ref, "INSERT INTO " ++ Table ++
			       " VALUES('0')"),

	    SelectResult = ?RDBMS:real_zero_selected(),
	    SelectResult =
		odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
	    ok
    end.
%%-------------------------------------------------------------------------
dec_and_num(doc) ->
    ["Tests decimal and numeric datatypes."];
dec_and_num(suite) ->
    [dec_long, dec_double, dec_bignum, num_long, num_double, num_bignum].
%%------------------------------------------------------------------------
dec_long(doc) ->
    [""];
dec_long(suit) ->
    [];
dec_long(Config) when is_list(Config) ->
    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       "(FIELD DECIMAL (9,0))"), 

    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(1.6)"),

    {selected, Fields, [{2}]} =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
    ["FIELD"] = odbc_test_lib:to_upper(Fields),
    ok.
%%------------------------------------------------------------------------
dec_double(doc) ->
    [""];
dec_double(suit) ->
    [];
dec_double(Config) when is_list(Config) ->
    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       "(FIELD DECIMAL (10,0))"), 
    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(1.6)"),

    {selected, Fields, [{2.00000}]} =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
    ["FIELD"] = odbc_test_lib:to_upper(Fields),

    %% Clean up
    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref, "DROP TABLE " ++ Table), 

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       "(FIELD DECIMAL (15,0))"), 
    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(1.6)"),

    {selected, Fields1, [{2.00000}]} =		
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
     ["FIELD"] = odbc_test_lib:to_upper(Fields1),

    %% Clean up
    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref, "DROP TABLE " ++ Table), 


    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       "(FIELD DECIMAL (15, 1))"), 
    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(1.6)"),

    {selected, Fields2, [{1.60000}]} =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
    ["FIELD"] = odbc_test_lib:to_upper(Fields2),
    ok.

%%------------------------------------------------------------------------
dec_bignum(doc) ->
    [""];
dec_bignum(suit) ->
    [];
dec_bignum(Config) when is_list(Config) ->
    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       "(FIELD DECIMAL (16,0))"), 
    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(1.6)"),

    {selected, Fields, [{"2"}]} =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
    ["FIELD"] = odbc_test_lib:to_upper(Fields),

    %% Clean up
    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref, "DROP TABLE " ++ Table), 

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       "(FIELD DECIMAL (16,1))"), 
    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(1.6)"),

    {selected, Fields1, [{"1.6"}]} =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
    ["FIELD"] = odbc_test_lib:to_upper(Fields1),
    ok.
%%------------------------------------------------------------------------
num_long(doc) ->
    [""];
num_long(suit) ->
    [];
num_long(Config) when is_list(Config) ->
    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       "(FIELD DECIMAL (9,0))"), 

    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(1.5)"),

    {selected, Fields, [{2}]} =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
    ["FIELD"] = odbc_test_lib:to_upper(Fields),
    ok.
%%------------------------------------------------------------------------
num_double(doc) ->
    [""];
num_double(suit) ->
    [];
num_double(Config) when is_list(Config) ->
    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       "(FIELD DECIMAL (10,0))"), 
    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(1.6)"),

    {selected, Fields, [{2.0000}]} =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
    ["FIELD"] = odbc_test_lib:to_upper(Fields),

    %% Clean up
    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref, "DROP TABLE " ++ Table), 

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       "(FIELD DECIMAL (15,0))"), 
    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(1.6)"),

    {selected, Fields1, [{2.0000}]} =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
    ["FIELD"] = odbc_test_lib:to_upper(Fields1),

    %% Clean up
    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref, "DROP TABLE " ++ Table), 

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       "(FIELD DECIMAL (15,1))"), 
    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(1.6)"),

    {selected, Fields2, [{1.6000}]} =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
     ["FIELD"] = odbc_test_lib:to_upper(Fields2),
    ok.
%%------------------------------------------------------------------------
num_bignum(doc) ->
    [""];
num_bignum(suit) ->
    [];
num_bignum(Config) when is_list(Config) ->
    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       "(FIELD DECIMAL (16,0))"), 
    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(1.6)"),

    {selected, Fields, [{"2"}]} =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
    ["FIELD"] = odbc_test_lib:to_upper(Fields),

    %% Clean up
    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref, "DROP TABLE " ++ Table), 

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       "(FIELD DECIMAL (16,1))"), 
    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(1.6)"),

    {selected, Fields1, [{"1.6"}]} =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
    ["FIELD"] = odbc_test_lib:to_upper(Fields1),
    ok.

%%------------------------------------------------------------------------
unicode(doc) ->
    ["Test unicode support"];
unicode(suit) ->
    [];
unicode(Config) when is_list(Config) ->
    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),
    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       ?RDBMS:create_unicode_table()), 
    
    Latin1Data = ["ÖÄÅÄÖÅäöå",
                  "testasdf",
                  "Row 3",
                  "Row 4",
                  "Row 5",
                  "Row 6",
                  "Row 7",
                  "Row 8",
                  "Row 9",
                  "Row 10",
                  "Row 11",
                  "Row 12"],
  
    case ?RDBMS of
	sqlserver ->
	    w_char_support_win(Ref, Table, Latin1Data);
	postgres ->
	    direct_utf8(Ref, Table, Latin1Data);
	oracle ->
	    {skip, "not currently supported"}
    end.

w_char_support_win(Ref, Table, Latin1Data) ->
    UnicodeIn = lists:map(fun(S) ->
                                  unicode:characters_to_binary(S,latin1,{utf16,little})
                          end,
                          Latin1Data),
     
    test_server:format("UnicodeIn (utf 16): ~p ~n",[UnicodeIn]),
    
    {updated, _} = odbc:param_query(Ref, "INSERT INTO " ++ Table ++ "(FIELD) values(?)",
				    [{{sql_wvarchar,50},UnicodeIn}]),
    
    {selected,_,UnicodeOut} = odbc:sql_query(Ref,"SELECT * FROM " ++ Table),

    test_server:format("UnicodeOut: ~p~n", [UnicodeOut]),
    
    Result = lists:map(fun({Unicode}) ->
			       unicode:characters_to_list(Unicode,{utf16,little})
		       end,
		       UnicodeOut),
    Latin1Data = Result.


direct_utf8(Ref, Table, Latin1Data) ->    
    UnicodeIn = lists:map(fun(String) ->
				  unicode:characters_to_binary(String,latin1,utf8)
			  end,
			  Latin1Data),
    
    test_server:format("UnicodeIn: ~p ~n",[UnicodeIn]),
    {updated, _} = odbc:param_query(Ref,"INSERT INTO " ++ Table ++ "(FIELD) values(?)",
				    [{{sql_varchar,50}, UnicodeIn}]),
    
    {selected,_,UnicodeOut} = odbc:sql_query(Ref,"SELECT * FROM " ++ Table),

    test_server:format("UnicodeOut: ~p~n", [UnicodeOut]),
     
    Result = lists:map(fun({Char}) ->
			       unicode:characters_to_list(Char,utf8)
		       end, UnicodeOut),

    test_server:format("Result: ~p ~n", [Result]),
    
    Latin1Data = Result.

%%------------------------------------------------------------------------
timestamp(doc) ->
    [""];
timestamp(suit) ->
    [];
timestamp(Config) when is_list(Config) ->
    Ref = ?config(connection_ref, Config),   
    Table = ?config(tableName, Config),

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       ?RDBMS:create_timestamp_table()),

    Data = [calendar:local_time(),
	    {{2009,6,17},{20,54,59}},
	    {{2009,6,18},{20,54,59}},
	    {{2009,6,19},{20,54,59}},
	    {{2009,6,20},{20,54,59}},
	    {{2009,6,21},{20,54,59}}],
    
    {updated, _} = odbc:param_query(Ref,"INSERT INTO " ++ Table ++  "(FIELD) values(?)",
				    [{sql_timestamp,Data}]),
    
    %%% Crate list or database table rows 
    TimeStamps = lists:map(fun(Value) -> {Value} end, Data),
   
    {selected,_, TimeStamps} = odbc:sql_query(Ref, "SELECT * FROM " ++ Table).
