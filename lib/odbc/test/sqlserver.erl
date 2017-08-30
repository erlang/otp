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

-module(sqlserver).

%% Note: This directive should only be used in test suites.
-compile(export_all).

%-------------------------------------------------------------------------
connection_string() ->
  "DSN=sql-server;UID=odbctest;PWD=gurka".

%-------------------------------------------------------------------------
insert_result() ->
    {selected,["ID","DATA"],[{1,"bar"}]}.

update_result() ->
    {selected,["ID","DATA"],[{1,"foo"}]}.

selected_ID(N, _) ->
    {selected,["ID"],[{N}]}.

selected_next_N(1)->
    {selected,["ID"],
     [{1},
      {2},
      {3}]};

selected_next_N(2)->
    {selected,["ID"],
     [{4},
      {5}]}.

selected_relative_N(1)->
    {selected,["ID"],
     [{2},
      {3},
      {4}]};

selected_relative_N(2)->
    {selected,["ID"],
     [{7},
      {8}]}.

selected_absolute_N(1)->
    {selected,["ID"],
     [{1},
      {2},
      {3}]};

selected_absolute_N(2)->
    {selected,["ID"],
     [{1},
      {2},
      {3},
      {4},
      {5}]}.

selected_list_rows() ->
    {selected,["ID", "DATA"],[[1, "bar"],[2,"foo"]]}.

first_list_rows() ->
    {selected,["ID", "DATA"],[[1, "bar"]]}.
last_list_rows() ->
    {selected,["ID", "DATA"],[[2, "foo"]]}.
prev_list_rows() ->
    {selected,["ID", "DATA"],[[1, "bar"]]}.
next_list_rows() ->
    {selected,["ID", "DATA"],[[2, "foo"]]}.

multiple_select()->
    [{selected,["ID", "DATA"],[{1, "bar"},{2, "foo"}]},
     {selected,["DATA"],[{"foo"}]}].

multiple_mix()->
    [{updated, 1},{updated, 1},
     {selected,["ID", "DATA"],[{1, "foobar"},{2, "foo"}]},
     {updated, 1}, {selected,["DATA"],[{"foo"}]}].

%-------------------------------------------------------------------------
fixed_char_min() ->
    1.

fixed_char_max() ->
    8000.

create_fixed_char_table(Size) ->
    " (FIELD char(" ++ integer_to_list(Size) ++ "))".

%-------------------------------------------------------------------------
var_char_min() ->
    1.
var_char_max() ->
    8000. 

create_var_char_table(Size) ->
    " (FIELD varchar(" ++ integer_to_list(Size) ++ "))".
%-------------------------------------------------------------------------
text_min() ->
    1.
text_max() ->
    2147483647. %% 2^31 - 1 

create_text_table() ->
    " (FIELD text)".

%-------------------------------------------------------------------------
create_timestamp_table() ->
    " (FIELD DATETIME)". 

%-------------------------------------------------------------------------
tiny_int_min() ->
    0.
tiny_int_max() ->
    255.

create_tiny_int_table() ->
     " (FIELD tinyint)".

tiny_int_min_selected() ->
    {selected,["FIELD"],[{tiny_int_min()}]}.

tiny_int_max_selected() ->
    {selected,["FIELD"], [{tiny_int_max()}]}.

%-------------------------------------------------------------------------
small_int_min() ->
    -32768. % -2^15
small_int_max() ->
    32767. % 2^15-1

create_small_int_table() ->
     " (FIELD smallint)".

small_int_min_selected() ->
    {selected,["FIELD"],[{small_int_min()}]}.

small_int_max_selected() ->
    {selected,["FIELD"], [{small_int_max()}]}.

%-------------------------------------------------------------------------
int_min() ->
    -2147483648. % -2^31
int_max() ->
    2147483647.  % 2^31-1

create_int_table() ->
     " (FIELD int)".

int_min_selected() ->
    {selected,["FIELD"],[{int_min()}]}.

int_max_selected() ->
    {selected,["FIELD"], [{int_max()}]}.

%-------------------------------------------------------------------------
big_int_min() ->
    -9223372036854775808. % -2^63
big_int_max() ->
    9223372036854775807. % 2^63-1

create_big_int_table() ->
     " (FIELD bigint)". 

big_int_min_selected() ->
    {selected,["FIELD"],[{integer_to_list(big_int_min())}]}.

big_int_max_selected() ->
    {selected,["FIELD"], [{integer_to_list(big_int_max())}]}.

%-------------------------------------------------------------------------
bit_false() ->
    0.
bit_true() ->
    1.

create_bit_table() ->
     " (FIELD bit)".

bit_false_selected() ->
    {selected,["FIELD"],[{false}]}.

bit_true_selected() ->
    {selected,["FIELD"], [{true}]}.
%-------------------------------------------------------------------------
float_min() ->
    -1.79e+308.
float_max() ->
    1.79e+308.

float_underflow() ->
    "'-1.80e+308'".

float_overflow() ->
    "'-1.80e+308'".

create_float_table() ->
    " (FIELD float)".

float_zero_selected() ->
    {selected,["FIELD"],[{0.00000e+0}]}.
%-------------------------------------------------------------------------
real_min() ->
    -3.40e+38.
real_max() ->
    3.40e+38.

real_underflow() ->
    -3.41e+38.

real_overflow() ->
    3.41e+38.

create_real_table() ->
    " (FIELD real)".

real_zero_selected() ->
    {selected,["FIELD"],[{0.00000e+0}]}.
%-------------------------------------------------------------------------
param_select_tiny_int() ->
    {selected,["FIELD"],[{1}, {2}]}.

param_select_small_int() ->
    {selected,["FIELD"],[{1}, {2}]}.

param_select_int() ->
    Int = small_int_max() + 1,
    {selected,["FIELD"],[{1}, {Int}]}.

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
    {selected,["ID","DATA"],[{1, "foo"}, {2, "bar"}]}.

param_update() ->
    {selected,["ID","DATA"],[{1, "foobar"}, {2, "foobar"}, {3, "baz"}]}.

param_delete() ->
    {selected,["ID","DATA"],[{3, "baz"}]}.

param_select() ->
    {selected,["ID","DATA"],[{1, "foo"},{3, "foo"}]}.

%-------------------------------------------------------------------------

describe_integer() ->
    {ok,[{"myint1", sql_smallint},{"myint2", sql_integer},
	 {"myint3", sql_integer}]}.

describe_string() ->
    {ok,[{"str1",{sql_char,10}},                           
	 {"str2",{sql_char,10}},
	 {"str3",{sql_varchar,10}},
	 {"str4",{sql_varchar,10}}]}.

describe_floating() ->
    {ok,[{"f", sql_real},{"r", sql_real}, {"d", {sql_float, 53}}]}.

describe_dec_num() ->
    {ok,[{"mydec",{sql_decimal,9,3}},{"mynum",{sql_numeric,9,2}}]}.

describe_timestamp() ->
    {ok, [{"field", sql_timestamp}]}.
