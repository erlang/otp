%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2011. All Rights Reserved.
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

-module(postgres).

%% Note: This directive should only be used in test suites.
-compile(export_all).

%-------------------------------------------------------------------------
connection_string() ->
    case test_server:os_type() of
	{unix, sunos} ->
	    "DSN=Postgres;UID=odbctest";
	{unix, linux} ->
	    Size = erlang:system_info({wordsize, external}),
	    linux_dist_connection_string(Size)  
    end.

linux_dist_connection_string(4) ->
    case linux_dist() of
	"ubuntu" ->
	    "DSN=PostgresLinuxUbuntu;UID=odbctest";  
	_ ->  
	    "DSN=PostgresLinux;UID=odbctest"
    end;

linux_dist_connection_string(_) ->
    case linux_dist() of
	"ubuntu" ->
	    "DSN=PostgresLinux64Ubuntu;UID=odbctest";
	_ ->
	    "DSN=PostgresLinux64;UID=odbctest"
    end.
			
linux_dist() ->
    case file:read_file("/etc/issue") of
	{ok, Binary} ->
	    [Dist | _ ] = string:tokens(binary_to_list(Binary), " "),
	    string:to_lower(Dist);
	{error, _} ->
	    other
    end.
	
	  
%-------------------------------------------------------------------------
insert_result() ->
    {selected,["id","data"],[{1,"bar"}]}.

update_result() ->
    {selected,["id","data"],[{1,"foo"}]}.

selected_ID(N, next) ->
    {selected,["id"],[{N}]};

selected_ID(_, _) ->
    {error, driver_does_not_support_function}.

selected_next_N(1)->
    {selected,["id"],
     [{1},
      {2},
      {3}]};

selected_next_N(2)->
    {selected,["id"],
     [{4},
      {5}]}.

selected_relative_N(_)->
    {error, driver_does_not_support_function}.

selected_absolute_N(_)->
    {error, driver_does_not_support_function}.

selected_list_rows() ->
    {selected,["id", "data"],[[1, "bar"],[2,"foo"]]}.
    
first_list_rows() ->
    {error, driver_does_not_support_function}.
last_list_rows() ->
    {error, driver_does_not_support_function}.
prev_list_rows() ->
    {error, driver_does_not_support_function}.
next_list_rows() ->
    {selected,["id","data"],[[1,"bar"]]}.

%% In case we get a better postgres driver that support this some day .....
multiple_select()->
    [{selected,["id", "data"],[{1, "bar"},{2, "foo"}]},
     {selected,["id"],[{"foo"}]}].

multiple_mix()->
    [{updated, 1},{updated, 1},
     {selected,["id", "data"],[{1, "foobar"},{2, "foo"}]},
     {updated, 1}, {selected,["data"],[{"foo"}]}].

%-------------------------------------------------------------------------
fixed_char_min() ->
    1.
fixed_char_max() ->
    2000. 

create_fixed_char_table(Size) ->
    " (FIELD char(" ++ integer_to_list(Size) ++ "))".

%-------------------------------------------------------------------------
var_char_min() ->
    1.
var_char_max() ->
    2000. 

create_var_char_table(Size) ->
    " (FIELD varchar(" ++ integer_to_list(Size) ++ "))".

%-------------------------------------------------------------------------
text_min() ->
    1.
text_max() ->
   2147483646. % 2147483647. %% 2^31 - 1 

create_text_table() ->
    " (FIELD text)". 

%-------------------------------------------------------------------------
create_timestamp_table() ->
    " (FIELD TIMESTAMP)". 

%-------------------------------------------------------------------------
small_int_min() ->
    -32768.
small_int_max() ->
    32767.

create_small_int_table() ->
     " (FIELD smallint)".

small_int_min_selected() ->
    {selected,["field"],[{-32768}]}.

small_int_max_selected() ->
    {selected,["field"], [{32767}]}.

%-------------------------------------------------------------------------
int_min() ->
   -2147483648.
int_max() ->
    2147483647.

create_int_table() ->
     " (FIELD int)".

int_min_selected() ->
    {selected,["field"],[{-2147483648}]}.

int_max_selected() ->
    {selected,["field"], [{2147483647}]}.

%-------------------------------------------------------------------------
big_int_min() ->
    -9223372036854775808.
   
big_int_max() ->
    9223372036854775807.

create_big_int_table() ->
     " (FIELD bigint )".

big_int_min_selected() ->
    {selected,["field"], [{"-9223372036854775808"}]}.

big_int_max_selected() ->
    {selected,["field"], [{"9223372036854775807"}]}.

%-------------------------------------------------------------------------
bit_false() ->
    0.
bit_true() ->
    1.

create_bit_table() ->
     " (FIELD bit)".

bit_false_selected() ->
    {selected,["field"],[{"0"}]}.

bit_true_selected() ->
    {selected,["field"], [{"1"}]}.

%-------------------------------------------------------------------------
float_min() ->
    1.79e-307. 
float_max() ->
    1.79e+308.

create_float_table() ->
    " (FIELD float)".

float_underflow() ->
    "1.80e-308".
float_overflow() ->
    "1.80e+308".

float_zero_selected() ->
    {selected,["field"],[{0.00000e+0}]}.

%-------------------------------------------------------------------------
real_min() ->
    -3.40e+38.
real_max() ->
    3.40e+38.

real_underflow() ->
    "-3.41e+38".

real_overflow() ->
    "3.41e+38".

create_real_table() ->
    " (FIELD real)".

real_zero_selected() ->
    {selected,["field"],[{0.00000e+0}]}.

%-------------------------------------------------------------------------
param_select_small_int() ->
    {selected,["field"],[{1}, {2}]}.

param_select_int() ->
    Int = small_int_max() + 1,
    {selected,["field"],[{1}, {Int}]}.

param_select_decimal() ->
    {selected,["field"],[{1},{2}]}.

param_select_numeric() ->
    {selected,["field"],[{1},{2}]}.

param_select_float() ->
    {selected,["field"],[{1.30000},{1.20000}]}.

param_select_real() ->
    {selected,["field"],[{1.30000},{1.20000}]}.

param_select_double() ->
    {selected,["field"],[{1.30000},{1.20000}]}.

param_select_mix() ->
    {selected,["id","data"],[{1, "foo"}, {2, "bar"}]}.

param_update() ->
    {selected,["id","data"],[{3, "baz"},{1, "foobar"}, {2, "foobar"}]}.

param_delete() ->
    {selected,["id","data"],[{3, "baz"}]}.

param_select() ->
    {selected,["id","data"],[{1, "foo"},{3, "foo"}]}.

%-------------------------------------------------------------------------
describe_integer() ->
    {ok,[{"myint1",sql_smallint},
	 {"myint2",sql_integer},
	 {"myint3",sql_integer}]}.

describe_string() ->
    {ok,[{"str1",{sql_char,10}},                           
	 {"str2",{sql_char,10}},
	 {"str3",{sql_varchar,10}},
	 {"str4",{sql_varchar,10}}]}.

describe_floating() ->
    {ok,[{"f",sql_real},{"r",sql_real},{"d",{sql_float,15}}]}.
describe_dec_num() ->
    {ok,[{"mydec",{sql_numeric,9,3}},{"mynum",{sql_numeric,9,2}}]}.

describe_timestamp() ->
    {ok, [{"field", sql_timestamp}]}.

%-------------------------------------------------------------------------
drop_proc() ->
    "drop function test_proc1(OUT integer, OUT integer);".

stored_proc_integer_out() ->
    "create or replace FUNCTION test_proc1(" ++
        "OUT int_a INTEGER, " ++
        "OUT int_b INTEGER) " ++
        "AS $$ " ++
        "BEGIN " ++
        " int_a := 123; " ++
        " int_b := 456; " ++
        "END " ++
        "$$ LANGUAGE plpgsql ".

%% This does not test what you might think it is supposed to test.
%% Since the stored procedure has got 2 out parameters and no
%% in parameters it is of arity 0 as called below.
%%
%% The port program odbcserver.c will marshal these out parameters
%% and hand them to ODBC. The ODBC driver for postgres will
%% apparently not give a hoot about these out parameters and instead
%% return the result in a regular result select set. The port program
%% will assume it has the result in the out parameters and marshal
%% these as they are i.e as it itself had packed them, so they
%% come back unchanged.
%%
%% The real function result goes into the void but the code in odbcserver.c
%% that marshals out parameters returned from ODBC will be run
%% so that is what this test tests...
%%
param_query(Ref) ->
    odbc:param_query(Ref, "select * from test_proc1()",
                     [{sql_integer, out, [111]},
                      {sql_integer, out, [444]}]).

query_result() ->
    {executed, 2, [{111, 444}]}.
