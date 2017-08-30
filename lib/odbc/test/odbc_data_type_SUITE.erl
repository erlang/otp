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

-module(odbc_data_type_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
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
suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    case odbc_test_lib:odbc_check() of
	ok ->
	    [{group, char},{group, fixed_char}, {group, binary_char},
	     {group, fixed_binary_char}, {group, unicode},
	     {group, int}, {group, floats},
	     {group, dec_and_num}, timestamp];
	Other -> {skip, Other}
    end.

groups() -> 
    [{char, [],
      [varchar_lower_limit,
       varchar_upper_limit, varchar_no_padding,
       text_lower_limit, text_upper_limit]},
     {fixed_char, [],
      [char_fixed_lower_limit, char_fixed_upper_limit,
       char_fixed_padding]},
     {binary_char, [],
      [binary_varchar_lower_limit,
       binary_varchar_upper_limit, binary_varchar_no_padding,
       binary_text_lower_limit, binary_text_upper_limit]},
     {fixed_binary_char, [], [binary_char_fixed_lower_limit,
			      binary_char_fixed_upper_limit,
			      binary_char_fixed_padding]},
     {unicode, [], [utf8, nchar, nvarchar]},
     {int, [],
      [tiny_int_lower_limit, tiny_int_upper_limit,
       small_int_lower_limit, small_int_upper_limit,
       int_lower_limit, int_upper_limit, big_int_lower_limit,
       big_int_upper_limit, bit_false, bit_true]},
     {floats, [],
      [float_lower_limit, float_upper_limit, float_zero,
       real_zero]},
     {dec_and_num, [],
      [dec_long, dec_double, dec_bignum, num_long, num_double,
       num_bignum]}].

init_per_group(GroupName, Config) when GroupName == fixed_char;
				       GroupName == fixed_binary_char ->
    case ?RDBMS of
	mysql ->
	    {skip, "No supported by MYSQL"};
	_ ->
	    Config
    end;

init_per_group(unicode, Config) ->
    case {os:type(), erlang:system_info({wordsize, external})} of
	{{unix, _}, 4} ->
	    Config;
	{{unix, _}, _} ->
	    {skip, "Postgres drivers pre version psqlODBC 08.04.0200 have utf8-problems"};
	_ ->
	    Config
    end;

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
    case odbc_test_lib:skip() of
	true ->
	    {skip, "ODBC not supported"};
	false ->
	    case (catch odbc:start()) of
		ok ->
		    ct:timetrap(?default_timeout),
		    [{tableName, odbc_test_lib:unique_table_name()}| Config];
		_ ->
		    {skip, "ODBC not startable"}
	    end
    end.
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
init_per_testcase(Case, Config) when Case == varchar_upper_limit;
				     Case == binary_varchar_upper_limit;
				     Case == varchar_no_padding;
				     Case == binary_varchar_no_padding ->
    case is_fixed_upper_limit(?RDBMS) of
	true ->
	    common_init_per_testcase(Case, Config);
	false ->
	    {skip, "Upper limit is not fixed in" ++ atom_to_list(?RDBMS)}
    end;

init_per_testcase(text_upper_limit, _Config) ->
    {skip, "Consumes too much resources"};

init_per_testcase(Case, Config) when Case == bit_true; Case == bit_false ->
    case is_supported_bit(?RDBMS) of
	true ->
	    common_init_per_testcase(Case, Config);
	false ->
	    {skip, "Not supported by driver"}
    end;

init_per_testcase(param_insert_tiny_int = Case, Config) ->
    case is_supported_tinyint(?RDBMS) of
	true ->
	    common_init_per_testcase(Case, Config);
	false ->
	    {skip, "Not supported by driver"}
    end;

init_per_testcase(Case, Config) when Case == nchar;
				     Case == nvarchar ->
    case ?RDBMS of
	sqlserver ->
	    common_init_per_testcase(Case, Config);
	_ ->
	    {skip, "Not supported by driver"}
    end;

init_per_testcase(Case, Config) ->
    common_init_per_testcase(Case, Config).

common_init_per_testcase(Case, Config) ->
    PlatformOptions = odbc_test_lib:platform_options(),
    {ok, Ref} = 
	case atom_to_list(Case) of
	    "binary" ++ _  ->
		odbc:connect(?RDBMS:connection_string(), 
			     [{binary_strings, on}] ++ PlatformOptions);
	    LCase when LCase == "utf8";
		       LCase == "nchar";
		       LCase == "nvarchar" ->
		odbc:connect(?RDBMS:connection_string(), 
			     [{binary_strings, on}] ++ PlatformOptions);
	    _ ->
		odbc:connect(?RDBMS:connection_string(), PlatformOptions)
	end,
    odbc_test_lib:strict(Ref, ?RDBMS),
    NewConfig = lists:keydelete(connection_ref, 1, Config),
    [{connection_ref, Ref} | NewConfig].

is_fixed_upper_limit(mysql) ->
    false;
is_fixed_upper_limit(_) ->
    true.
is_supported_tinyint(sqlserver) ->
    true;
is_supported_tinyint(_) ->
    false.
is_supported_bit(sqlserver) ->
    true;
is_supported_bit(_) ->
     false.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(Case, Config) -> _
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after each test case
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, Config) ->
    Ref = proplists:get_value(connection_ref, Config),
    ok = odbc:disconnect(Ref),
    %% Clean up if needed 
    Table = proplists:get_value(tableName, Config),
    {ok, NewRef} = odbc:connect(?RDBMS:connection_string(), odbc_test_lib:platform_options()),
    odbc:sql_query(NewRef, "DROP TABLE " ++ Table), 
    odbc:disconnect(NewRef).

%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------

char_fixed_lower_limit() ->
    [{doc,"Tests fixed length char data type lower boundaries."}].
char_fixed_lower_limit(Config) when is_list(Config) ->
    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),

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
		       ++ "')").

%%-------------------------------------------------------------------------

char_fixed_upper_limit() ->
    [{doc,"Tests fixed length char data type upper boundaries."}].
char_fixed_upper_limit(Config) when is_list(Config) ->

    case ?RDBMS of
	postgres ->
	    {skip, "Limit unknown"};
	_ ->
	    Ref = proplists:get_value(connection_ref, Config),   
	    Table = proplists:get_value(tableName, Config),
	    
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
				  (?RDBMS:fixed_char_max() + 1)))
    end.

%%-------------------------------------------------------------------------

char_fixed_padding() ->
    [{doc, "Tests that data that is shorter than the given size is padded " 
     "with blanks."}].
char_fixed_padding(Config) when is_list(Config) ->
    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),

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
	    ["FIELD"] = odbc_test_lib:to_upper(Fields).

%%-------------------------------------------------------------------------

varchar_lower_limit() ->
    [{doc,"Tests variable length char data type lower boundaries."}].
varchar_lower_limit(Config) when is_list(Config) ->
    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),

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

    Str = string:chars($a, ?RDBMS:var_char_min()),

    %% Right length data
    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
			   "'" ++ Str ++ "')"),
    %% Select data
    {selected, Fields, [{Str}]} =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
    
    ["FIELD"] = odbc_test_lib:to_upper(Fields),

    %% Too long datae
    {error, _} =
		odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++
				   "'" ++ string:chars($a,
						       (?RDBMS:var_char_min()+1))
			       ++ "')").

%%-------------------------------------------------------------------------

varchar_upper_limit() ->
    [{doc,"Tests variable length char data type upper boundaries."}].
varchar_upper_limit(Config) when is_list(Config) ->
    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),

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

varchar_no_padding() ->
    [{doc, "Tests that data that is shorter than the given max size is not padded " 
     "with blanks."}].
varchar_no_padding(Config) when is_list(Config) ->
    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),

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
    ["FIELD"] = odbc_test_lib:to_upper(Fields).

%%-------------------------------------------------------------------------

text_lower_limit() ->
    [{doc,"Tests 'long' char data type lower boundaries."}].
text_lower_limit(Config) when is_list(Config) ->
    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       ?RDBMS:create_text_table()), 

    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
		       "'" ++ string:chars($a, ?RDBMS:text_min())
		       ++ "')"),

    {selected, Fields, [{"a"}]} =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
    ["FIELD"] = odbc_test_lib:to_upper(Fields).

%%-------------------------------------------------------------------------

text_upper_limit() ->
    [{doc,"Tests 'text' char data type upper boundaries."}].
text_upper_limit(Config) when is_list(Config) ->
    
    {skip,"Consumes too much resources" }.
%%     Ref = proplists:get_value(connection_ref, Config),
%%     Table = proplists:get_value(tableName, Config),

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
%% 		       ++ "')").

%%-------------------------------------------------------------------------

binary_char_fixed_lower_limit() ->
    [{doc,"Tests fixed length char data type lower boundaries."}].
binary_char_fixed_lower_limit(Config) when is_list(Config) ->
    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),

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

    Str = string:chars($a, ?RDBMS:fixed_char_min()),

    %% Right length data
    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++
		       "'" ++ Str
		       ++ "')"),

    Bin = list_to_binary(Str),

    %% Select data
    {selected, Fields,[{Bin}]} =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),

    ["FIELD"] = odbc_test_lib:to_upper(Fields),

    %% Too long data
    {error, _}  =  
	odbc:sql_query(Ref,"INSERT INTO " ++ Table ++" VALUES(" ++
		       "'" ++ string:chars($a, 
					   (?RDBMS:fixed_char_min()
					    + 1)) 
		       ++ "')").
%%-------------------------------------------------------------------------

binary_char_fixed_upper_limit() ->
    [{doc,"Tests fixed length char data type upper boundaries."}].
binary_char_fixed_upper_limit(Config) when is_list(Config) ->

    case ?RDBMS of
	postgres ->
	    {skip, "Limit unknown"};
	_ ->
	    Ref = proplists:get_value(connection_ref, Config),   
	    Table = proplists:get_value(tableName, Config),
	    
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

binary_char_fixed_padding() ->
    [{doc, "Tests that data that is shorter than the given size is padded " 
     "with blanks."}].
binary_char_fixed_padding(Config) when is_list(Config) ->
    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),

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
    ["FIELD"] = odbc_test_lib:to_upper(Fields).

%%-------------------------------------------------------------------------

binary_varchar_lower_limit() ->
    [{doc,"Tests variable length char data type lower boundaries."}].
binary_varchar_lower_limit(Config) when is_list(Config) ->
    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),

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

    Str = string:chars($a, ?RDBMS:var_char_min()),

    %% Right length data
    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
		       "'" ++ Str
		       ++ "')"),
    BinStr = list_to_binary(Str),

    %% Select data
    {selected, Fields, [{BinStr}]} =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
    
    ["FIELD"] = odbc_test_lib:to_upper(Fields),

    %% Too long data
    {error, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
		       "'" ++ string:chars($a, 
					   (?RDBMS:var_char_min()+1)) 
		       ++ "')").

%%-------------------------------------------------------------------------

binary_varchar_upper_limit() ->
    [{doc,"Tests variable length char data type upper boundaries."}].
binary_varchar_upper_limit(Config) when is_list(Config) ->
    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),

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
				  (?RDBMS:var_char_max() + 1)))
    end.
%%-------------------------------------------------------------------------

binary_varchar_no_padding() ->
    [{doc,"Tests that data that is shorter than the given max size is not padded " 
     "with blanks."}].
binary_varchar_no_padding(Config) when is_list(Config) ->
    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),

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
    ["FIELD"] = odbc_test_lib:to_upper(Fields).

%%-------------------------------------------------------------------------

binary_text_lower_limit() ->
    [{doc,"Tests 'long' char data type lower boundaries."}].
binary_text_lower_limit(Config) when is_list(Config) ->
    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       ?RDBMS:create_text_table()), 

    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(" ++ 
		       "'" ++ string:chars($a, ?RDBMS:text_min())
		       ++ "')"),

    {selected, Fields, [{<<"a">>}]} =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
    ["FIELD"] = odbc_test_lib:to_upper(Fields).

%%-------------------------------------------------------------------------

binary_text_upper_limit() ->
    [{doc,"Tests text char data type upper boundaries."}].
binary_text_upper_limit(Config) when is_list(Config) ->
    
    {skip,"Consumes too much resources" }.
%%     Ref = proplists:get_value(connection_ref, Config),
%%     Table = proplists:get_value(tableName, Config),

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
%% 		       ++ "')").


%%-------------------------------------------------------------------------

tiny_int_lower_limit() ->
    [{doc,"Tests integer of type tinyint."}].
tiny_int_lower_limit(Config) when is_list(Config) -> 
    case ?RDBMS of
	postgres ->
	    {skip, "Type tiniyint not supported"};
	_ ->
	    Ref = proplists:get_value(connection_ref, Config),   
	    Table = proplists:get_value(tableName, Config),
	    
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
			       ++ "')")
    end.

%%-------------------------------------------------------------------------

tiny_int_upper_limit() ->
    [{doc,"Tests integer of type tinyint."}].
tiny_int_upper_limit(Config) when is_list(Config) ->
    case ?RDBMS of
	postgres ->
	    {skip, "Type tiniyint not supported"};
	_ ->
	    Ref = proplists:get_value(connection_ref, Config),   
	    Table = proplists:get_value(tableName, Config),
	    
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
			       ++ "')")
    end.

%%-------------------------------------------------------------------------

small_int_lower_limit() ->
    [{doc,"Tests integer of type smallint."}].
small_int_lower_limit(Config) when is_list(Config) ->
    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),

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
		       ++ "')").

%%-------------------------------------------------------------------------

small_int_upper_limit() ->
    [{doc,"Tests integer of type smallint."}].
small_int_upper_limit(Config) when is_list(Config) ->
    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),

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
		       ++ "')").

%%-------------------------------------------------------------------------
int_lower_limit() ->
    [{doc,"Tests integer of type int."}].
int_lower_limit(Config) when is_list(Config) ->
    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),

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
		       ++ "')").

%%-------------------------------------------------------------------------

int_upper_limit() ->
    [{doc,"Tests integer of type int."}].
int_upper_limit(Config) when is_list(Config) ->
    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),

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
		       ++ "')").


%%-------------------------------------------------------------------------
big_int_lower_limit() ->
    [{doc,"Tests integer of type bigint"}].
big_int_lower_limit(Config) when is_list(Config) ->
    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),

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
		       ++ "')").

%%-------------------------------------------------------------------------

big_int_upper_limit() ->
    [{doc,"Tests integer of type bigint."}].
big_int_upper_limit(Config) when is_list(Config) ->
    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),

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
		       ++ "')").
%%-------------------------------------------------------------------------

bit_false(Config) when is_list(Config) ->  
    case ?RDBMS of
	oracle ->
	    {skip, "Not supported by driver"};
	_ ->
	    Ref = proplists:get_value(connection_ref, Config),   
	    Table = proplists:get_value(tableName, Config),
	    
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
			       ++ "')")
    end.

%%-------------------------------------------------------------------------

bit_true(Config) when is_list(Config) ->
    case ?RDBMS of
	oracle ->
	    {skip, "Not supported by driver"};
	_ ->
	    Ref = proplists:get_value(connection_ref, Config),   
	    Table = proplists:get_value(tableName, Config),
	    
	    
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
			       ++ "')")
    end.

%%-------------------------------------------------------------------------

float_lower_limit(Config) when is_list(Config) ->   

    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),

    case ?RDBMS of
	mysql ->
	    {skip, "Not clearly defined in MYSQL"};
	_ ->
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
	    end
    end.

%%-------------------------------------------------------------------------

float_upper_limit(Config) when is_list(Config) ->   
    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),

    case ?RDBMS of
	mysql ->
	    {skip, "Not clearly defined in MYSQL"};
	_->
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
				   ?RDBMS:float_overflow() ++ ")")
    end.

%%-------------------------------------------------------------------------
float_zero() ->
    [{doc,"Test the float value zero."}].
float_zero(Config) when is_list(Config) ->   
    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       ?RDBMS:create_float_table()), 

    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES('0')"),

    SelectResult = ?RDBMS:float_zero_selected(),
    SelectResult =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table).
%%-------------------------------------------------------------------------
real_zero() ->
    [{doc,"Test the real value zero."}].
real_zero(Config) when is_list(Config) ->   
    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),

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
		odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table)
    end.
%%------------------------------------------------------------------------
dec_long(suit) ->
    [];
dec_long(Config) when is_list(Config) ->
    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       "(FIELD DECIMAL (9,0))"), 

    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(1.6)"),

    {selected, Fields, [{2}]} =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
    ["FIELD"] = odbc_test_lib:to_upper(Fields).
%%------------------------------------------------------------------------
dec_double(suit) ->
    [];
dec_double(Config) when is_list(Config) ->
    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),

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
    ["FIELD"] = odbc_test_lib:to_upper(Fields2).

%%------------------------------------------------------------------------
dec_bignum(suit) ->
    [];
dec_bignum(Config) when is_list(Config) ->
    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),

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
    ["FIELD"] = odbc_test_lib:to_upper(Fields1).
%%------------------------------------------------------------------------
num_long(suit) ->
    [];
num_long(Config) when is_list(Config) ->
    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ 
		       "(FIELD DECIMAL (9,0))"), 

    {updated, _} =  
	odbc:sql_query(Ref, "INSERT INTO " ++ Table ++" VALUES(1.5)"),

    {selected, Fields, [{2}]} =
	odbc:sql_query(Ref,"SELECT FIELD FROM " ++ Table),
    ["FIELD"] = odbc_test_lib:to_upper(Fields).
%%------------------------------------------------------------------------
num_double(suit) ->
    [];
num_double(Config) when is_list(Config) ->
    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),

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
     ["FIELD"] = odbc_test_lib:to_upper(Fields2).
%%------------------------------------------------------------------------
num_bignum(suit) ->
    [];
num_bignum(Config) when is_list(Config) ->
    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),

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
    ["FIELD"] = odbc_test_lib:to_upper(Fields1).

%%------------------------------------------------------------------------
utf8() ->
    [{doc,"Test unicode support"}].
utf8(suit) ->
    [];
utf8(Config) when is_list(Config) ->
    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),
    
    odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++ "(FIELD text)"),

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
    
    UnicodeIn = lists:map(fun(String) ->
				  unicode:characters_to_binary(String,latin1,utf8)
			  end,
			  Latin1Data),
    
    ct:pal("UnicodeIn: ~p ~n",[UnicodeIn]),
    {updated, _} = odbc:param_query(Ref,"INSERT INTO " ++ Table ++ "(FIELD) values(?)",
				    [{{sql_varchar,50}, UnicodeIn}]),
    
    {selected,_,UnicodeOut} = odbc:sql_query(Ref,"SELECT * FROM " ++ Table),

    ct:pal("UnicodeOut: ~p~n", [UnicodeOut]),
     
    Result = lists:map(fun({Char}) ->
			       unicode:characters_to_list(Char,utf8)
		       end, UnicodeOut),

    ct:pal("Result: ~p ~n", [Result]),
    
    Latin1Data = Result.
%%------------------------------------------------------------------------

nchar() ->
    [{doc,"Test unicode nchar support in sqlserver"}].
nchar(suit) ->
    [];
nchar(Config) when is_list(Config) ->
    Ref = proplists:get_value(connection_ref, Config),
    Table = proplists:get_value(tableName, Config),

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++
			   "(FIELD nchar(50))"),

    w_char_support(Ref, Table, sql_wvarchar, 50).

%%------------------------------------------------------------------------

nvarchar() ->
    [{doc,"Test 'unicode' nvarchar support"}].
nvarchar(suit) ->
    [];
nvarchar(Config) when is_list(Config) ->
    Ref = proplists:get_value(connection_ref, Config),
    Table = proplists:get_value(tableName, Config),

    {updated, _} =  % Value == 0 || -1 driver dependent!
	odbc:sql_query(Ref,  "CREATE TABLE " ++ Table ++
			   "(FIELD nvarchar(50))"),

    w_char_support(Ref, Table, sql_wlongvarchar, 50).

%%------------------------------------------------------------------------
timestamp(suit) ->
    [];
timestamp(Config) when is_list(Config) ->
    Ref = proplists:get_value(connection_ref, Config),   
    Table = proplists:get_value(tableName, Config),

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
%%------------------------------------------------------------------------

w_char_support(Ref, Table, CharType, Size) ->
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

    UnicodeIn = lists:map(fun(S) ->
                                  unicode:characters_to_binary(S,latin1,{utf16,little})
                          end,
                          Latin1Data),

    ct:pal("UnicodeIn (utf 16): ~p ~n",[UnicodeIn]),

    {updated, _} = odbc:param_query(Ref, "INSERT INTO " ++ Table ++ "(FIELD) values(?)",
				    [{{CharType, Size},UnicodeIn}]),

    {selected,_,UnicodeOut} = odbc:sql_query(Ref,"SELECT * FROM " ++ Table),

    ct:pal("UnicodeOut: ~p~n", [UnicodeOut]),

    PadResult = lists:map(fun({Unicode}) ->
			       unicode:characters_to_list(Unicode,{utf16,little})
		       end,
		       UnicodeOut),

    ct:pal("Result: ~p~n", [PadResult]),

    Result = lists:map(fun(Str) -> string:strip(Str) end, PadResult),

    Latin1Data = Result.
