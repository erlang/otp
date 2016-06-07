%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2016. All Rights Reserved.
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

-module(mysql).

%% Note: This directive should only be used in test suites.
-compile(export_all).

%-------------------------------------------------------------------------
connection_string() ->
    case test_server:os_type() of
	{unix, linux} ->
	    "DSN=MySQL;Database=odbctest;Uid=odbctest;Pwd=gurka;CHARSET=utf8;SSTMT=SET NAMES 'utf8';";
	{unix, sunos} ->
	    solaris_str();
	{unix, darwin} ->
	    "DSN=MySQLMac;Database=odbctest;Uid=odbctest;Pwd=gurka;CHARSET=utf8;SSTMT=SET NAMES 'utf8';"
    end.

solaris_str() ->
    case erlang:system_info(system_architecture) of
	"sparc" ++ _ ->
	    "DSN=MySQLSolaris10;Database=odbctest;Uid=odbctest;Pwd=gurka;CHARSET=utf8;SSTMT=SET NAMES 'utf8';";
	"i386" ++ _ ->
	    "DSN=MySQLSolaris10i386;Database=odbctest;Uid=odbctest;Pwd=gurka;CHARSET=utf8;SSTMT=SET NAMES 'utf8';"
    end.

%-------------------------------------------------------------------------
insert_result() ->
    {selected,["ID","DATA"],[{1,"bar"}]}.

update_result() ->
    {selected,["ID","DATA"],[{1,"foo"}]}.

selected_ID(N, next) ->
    {selected,["ID"],[{N}]};

selected_ID(_, _) ->
    {error, driver_does_not_support_function}.

selected_next_N(1)->
    {selected,["ID"],
     [{1},
      {2},
      {3}]};

selected_next_N(2)->
    {selected,["ID"],
     [{4},
      {5}]}.

selected_relative_N(_)->
    {error, driver_does_not_support_function}.

selected_absolute_N(_)->
    {error, driver_does_not_support_function}.

selected_list_rows() ->
    {selected,["ID", "DATA"],[[1, "bar"],[2,"foo"]]}.

first_list_rows() ->
    {error, driver_does_not_support_function}.
last_list_rows() ->
    {error, driver_does_not_support_function}.
prev_list_rows() ->
    {error, driver_does_not_support_function}.
next_list_rows() ->
    {selected,["ID","DATA"],[[1,"bar"]]}.

multiple_select()->
    [{selected,["ID", "DATA"],[{1, "bar"},{2, "foo"}]},
     {selected,["ID"],[{"foo"}]}].

multiple_mix()->
    [{updated, 1},{updated, 1},
     {selected,["ID", "DATA"],[{1, "foobar"},{2, "foo"}]},
     {updated, 1}, {selected,["DATA"],[{"foo"}]}].

%-------------------------------------------------------------------------
var_char_min() ->
    0.
var_char_max() ->
    65535.

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
tiny_int_min() ->
    -128.
tiny_int_max() ->
    127.

create_tiny_int_table() ->
     " (FIELD tinyint)".

tiny_int_min_selected() ->
    {selected,["FIELD"],[{tiny_int_min()}]}.

tiny_int_max_selected() ->
    {selected,["FIELD"], [{tiny_int_max()}]}.

%-------------------------------------------------------------------------
small_int_min() ->
    -32768.
small_int_max() ->
    32767.

create_small_int_table() ->
     " (FIELD smallint)".

small_int_min_selected() ->
    {selected,["FIELD"],[{-32768}]}.

small_int_max_selected() ->
    {selected,["FIELD"], [{32767}]}.

%-------------------------------------------------------------------------
int_min() ->
   -2147483648.
int_max() ->
    2147483647.

create_int_table() ->
     " (FIELD int)".

int_min_selected() ->
    {selected,["FIELD"],[{-2147483648}]}.

int_max_selected() ->
    {selected,["FIELD"], [{2147483647}]}.

%-------------------------------------------------------------------------
big_int_min() ->
    -9223372036854775808.

big_int_max() ->
    9223372036854775807.

create_big_int_table() ->
     " (FIELD bigint )".

big_int_min_selected() ->
    {selected,["FIELD"], [{"-9223372036854775808"}]}.

big_int_max_selected() ->
    {selected,["FIELD"], [{"9223372036854775807"}]}.

%-------------------------------------------------------------------------
bit_false() ->
    0.
bit_true() ->
    1.

create_bit_table() ->
     " (FIELD bit)".

bit_false_selected() ->
    {selected,["FIELD"],[{"0"}]}.

bit_true_selected() ->
    {selected,["FIELD"], [{"1"}]}.

%-------------------------------------------------------------------------

%% Do not test float min/max as value is only theoretical defined in
%% mysql and may vary depending on hardware.

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
    "-3.41e+38".

real_overflow() ->
    "3.41e+38".

create_real_table() ->
    " (FIELD real)".

real_zero_selected() ->
    {selected,["FIELD"],[{0.00000e+0}]}.

%-------------------------------------------------------------------------
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
    {ok,[{"myint1",sql_smallint},
	 {"myint2",sql_integer},
	 {"myint3",sql_integer}]}.

describe_string() ->
    {ok,[{"str1",{sql_char,10}},
	 {"str2",{sql_char,10}},
	 {"str3",{sql_varchar,10}},
	 {"str4",{sql_varchar,10}}]}.

describe_floating() ->
     {ok,[{"f",sql_real},{"r",sql_double},{"d",sql_double}]}.
describe_dec_num() ->
    {ok,[{"mydec",{sql_decimal,9,3}},{"mynum",{sql_decimal,9,2}}]}.

describe_timestamp() ->
    {ok, [{"FIELD", sql_timestamp}]}.
