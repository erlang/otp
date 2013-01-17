%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2011. All Rights Reserved.
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

-module(oracle).

%% Note: This directive should only be used in test suites.
-compile(export_all).

%-------------------------------------------------------------------------
connection_string() ->
    "DSN=Oracle8;UID=odbctest".

%-------------------------------------------------------------------------
insert_result() ->
    {selected,["ID","DATA"],[{"1","bar"}]}.

update_result() ->
    {selected,["ID","DATA"],[{"1","foo"}]}.

selected_ID(N, next) ->
    {selected,["ID"],[{integer_to_list(N)}]};

selected_ID(_, _) ->
    {error, driver_does_not_support_function}.

selected_next_N(1)->
    {selected,["ID"],
     [{"1"},
      {"2"},
      {"3"}]};

selected_next_N(2)->
    {selected,["ID"],
     [{"4"},
      {"5"}]}.

selected_relative_N(_)->
    {error, driver_does_not_support_function}.

selected_absolute_N(_)->
    {error, driver_does_not_support_function}.

selected_list_rows() ->
    {selected,["ID", "DATA"],[["1", "bar"],["2","foo"]]}.
    
first_list_rows() ->
    {error, driver_does_not_support_function}.
last_list_rows() ->
    {error, driver_does_not_support_function}.
prev_list_rows() ->
    {error, driver_does_not_support_function}.
next_list_rows() ->
    {selected,["ID","DATA"],[["1","bar"]]}.

%% In case we get a better oracle driver that support this some day .....
multiple_select()->
    [{selected,["ID", "DATA"],[{"1", "bar"},{"2", "foo"}]},
     {selected,["ID"],[{"foo"}]}].

multiple_mix()->
    [{updated, 1},{updated, 1},
     {selected,["ID", "DATA"],[{"1", "foobar"},{"2", "foo"}]},
     {updated, 1}, {selected,["DATA"],[{"foo"}]}].

%-------------------------------------------------------------------------
fixed_char_min() ->
    1.
fixed_char_max() ->
    2000. %% Should be 255 acording to manual but empirical tests say 2000

create_fixed_char_table(Size) ->
    " (FIELD char(" ++ integer_to_list(Size) ++ "))".

%-------------------------------------------------------------------------
var_char_min() ->
    1.
var_char_max() ->
    2000. 

create_var_char_table(Size) ->
    " (FIELD varchar2(" ++ integer_to_list(Size) ++ "))".

%-------------------------------------------------------------------------
text_min() ->
    1.
text_max() ->
   2147483646. % 2147483647. %% 2^31 - 1 

create_text_table() ->
    " (FIELD long)". %Oracle long is variable length char data

%-------------------------------------------------------------------------
create_timestamp_table() ->
    " (FIELD DATETIME)". 

%-------------------------------------------------------------------------
tiny_int_min() ->
    -999.
tiny_int_max() ->
    999.

create_tiny_int_table() ->
     " (FIELD number(3, 0))".

tiny_int_min_selected() ->
    {selected,["FIELD"],[{-999}]}.

tiny_int_max_selected() ->
    {selected,["FIELD"], [{999}]}.

%-------------------------------------------------------------------------
small_int_min() ->
    -99999.
small_int_max() ->
    99999.

create_small_int_table() ->
     " (FIELD number(5, 0))".

small_int_min_selected() ->
    {selected,["FIELD"],[{-99999}]}.

small_int_max_selected() ->
    {selected,["FIELD"], [{99999}]}.

%-------------------------------------------------------------------------
int_min() ->
    -999999999.
int_max() ->
    999999999.

create_int_table() ->
     " (FIELD number(9, 0))".

int_min_selected() ->
    {selected,["FIELD"],[{-999999999}]}.

int_max_selected() ->
    {selected,["FIELD"], [{999999999}]}.

%-------------------------------------------------------------------------
big_int_min() ->
    -99999999999999999999999999999999999999.
   
big_int_max() ->
    99999999999999999999999999999999999999.

create_big_int_table() ->
     " (FIELD number(38,0))".

big_int_min_selected() ->
    {selected,["FIELD"], [{"-99999999999999999999999999999999999999"}]}.

big_int_max_selected() ->
    {selected,["FIELD"], [{"99999999999999999999999999999999999999"}]}.

%-------------------------------------------------------------------------
float_min() ->
    1.40129846432481707e-45.
    
float_max() ->
    3.40282346638528860e+38.

create_float_table() ->
    " (FIELD float(32))".

float_underflow() ->
    "'4.94065645841246544e-324'".
float_overflow() ->
    "'1.79769313486231570e+308'".

float_zero_selected() ->
    {selected,["FIELD"],[{0.00000e+0}]}.

%-------------------------------------------------------------------------
param_select_small_int() ->
    {selected,["FIELD"],[{"1"}, {"2"}]}.

param_select_int() ->
    Int = small_int_max() + 1,
    {selected,["FIELD"],[{"1"}, {integer_to_list(Int)}]}.

param_select_decimal() ->
    {selected,["FIELD"],[{1},{2}]}.

param_select_numeric() ->
    {selected,["FIELD"],[{1},{2}]}.

param_select_float() ->
    {selected,["FIELD"],[{1.30000},{1.20000}]}.

param_select_real() ->
    {selected,["FIELD"],[{1.30000},{1.20000}]}.

param_select_double() ->
    {selected,["FIELD"],[{1.30000},{1.20000}]}.

param_select_mix() ->
    {selected,["ID","DATA"],[{"1", "foo"}, {"2", "bar"}]}.

param_update() ->
    {selected,["ID","DATA"],[{"1", "foobar"}, {"2", "foobar"}, {"3", "baz"}]}.

param_delete() ->
    {selected,["ID","DATA"],[{"3", "baz"}]}.

param_select() ->
    {selected,["ID","DATA"],[{"1", "foo"},{"3", "foo"}]}.

%-------------------------------------------------------------------------
describe_integer() ->
    {ok,[{"MYINT1",{sql_decimal,38,0}},{"MYINT2",{sql_decimal,38,0}},
	 {"MYINT3",{sql_decimal,38,0}}]}.
    
describe_string() ->
    {ok,[{"STR1",{sql_char,10}},                           
	 {"STR2",{sql_char,10}},
	 {"STR3",{sql_varchar,10}},
	 {"STR4",{sql_varchar,10}}]}.

describe_floating() ->
    {ok,[{"F",sql_double},{"R",sql_double},{"D",sql_double}]}.
describe_dec_num() ->
    {ok,[{"MYDEC",{sql_decimal,9,3}},{"MYNUM",{sql_decimal,9,2}}]}.

%-------------------------------------------------------------------------
drop_proc() ->
    "drop procedure test_proc1;".

stored_proc_integer_out() ->
    "create or replace PROCEDURE  test_proc1(" ++
        "int_a OUT NUMBER, " ++
        "int_b OUT NUMBER) " ++
        "is " ++
        "begin " ++
        " int_a := 123; " ++
        " int_b := 456; " ++
        "exception " ++
        "WHEN NO_DATA_FOUND THEN " ++
        " int_a := 0; " ++
        " int_b := 0; " ++
        "end;".

param_query(Ref) ->
    odbc:param_query(Ref, "call test_proc1(?,?)",
                     [{sql_integer, out, [0]},
                      {sql_integer, out, [0]}]).


query_result() ->
    {executed, 2, [{123, 456}]}.
