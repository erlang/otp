%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
%%-----------------------------------------------------------------
%% 
%% Description:
%% Test suite for the CDR basic type encode/decode functions
%%
%%-----------------------------------------------------------------
-module(cdrlib_SUITE).

-include_lib("common_test/include/ct.hrl").

-define(default_timeout, test_server:minutes(3)).

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-compile(export_all).

%%-----------------------------------------------------------------
%% Func: all/1
%% Args: 
%% Returns: 
%%-----------------------------------------------------------------
suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [short, ushort, long, ulong, longlong, ulonglong,
     boolean, character, octet, float, double, enum].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%-----------------------------------------------------------------
%% Init and cleanup functions.
%%-----------------------------------------------------------------

init_per_testcase(_Case, Config) ->
    Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].


end_per_testcase(_Case, Config) ->
    Dog = proplists:get_value(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

%%-----------------------------------------------------------------
%% Test Case: short integer test
%% Description: 
%%-----------------------------------------------------------------
short(_) ->
    short_big_loop([-32768, -4040, -1, 0, 4040, 32767]),
    short_little_loop([-32768, -4040, -1, 0, 4040, 32767]),
    bad_short().

short_big_loop([]) ->
    ok;
short_big_loop([X |List]) ->
    [CodedType] = cdrlib:enc_short(X, []),
    {X, <<>>} = cdrlib:dec_short(big, CodedType),
    short_big_loop(List),
    ok.

short_little_loop([]) ->
    ok;
short_little_loop([X |List]) ->
    CodedType = enc_short_little(X, []),
    {X, <<>>} = cdrlib:dec_short(little, CodedType),
    short_little_loop(List),
    ok.

enc_short_little(X, Message) -> 
    list_to_binary([(X) band 16#ff, ((X) bsr 8) band 16#ff | Message]).

bad_short() ->
    {'EXCEPTION', _} = (catch cdrlib:enc_short('atom', [])),
    [CodedType] = cdrlib:enc_char($a, []),
    {'EXIT', _} = (catch cdrlib:dec_short(big, CodedType)),
    ok.
%%-----------------------------------------------------------------
%% Test Case: unsigned short integer test
%% Description: 
%%-----------------------------------------------------------------
ushort(_) ->
    ushort_big_loop([0, 4040, 65535]),
    ushort_little_loop([0, 4040, 65535]),
    bad_ushort().

ushort_big_loop([]) ->
    ok;
ushort_big_loop([X |List]) ->
    [CodedType] = cdrlib:enc_unsigned_short(X, []),
    {X, <<>>} = cdrlib:dec_unsigned_short(big, CodedType),
    ushort_big_loop(List),
    ok.

ushort_little_loop([]) ->
    ok;
ushort_little_loop([X |List]) ->
    CodedType = enc_ushort_little(X, []),
    {X, <<>>} = cdrlib:dec_unsigned_short(little, CodedType),
    ushort_little_loop(List),
    ok.

enc_ushort_little(X, Message) -> 
    list_to_binary([(X) band 16#ff, ((X) bsr 8) band 16#ff | Message]).

bad_ushort() ->
    ok.
%%-----------------------------------------------------------------
%% Test Case: long integer test
%% Description: 
%%-----------------------------------------------------------------
long(_) ->
    long_big_loop([-2147483648, -40404040, -32768, -4040, -1,
		   0, 4040, 32767, 40404040, 2147483647]),
    long_little_loop([-2147483648, -40404040, -32768, -4040, -1,
		      0, 4040, 32767, 40404040, 2147483647]),
    bad_long().
    

long_big_loop([]) ->
    ok;
long_big_loop([X |List]) ->
    [CodedType] = cdrlib:enc_long(X, []),
    {X, <<>>} = cdrlib:dec_long(big, CodedType),
    long_big_loop(List),
    ok.

long_little_loop([]) ->
    ok;
long_little_loop([X |List]) ->
    CodedType = enc_long_little(X, []),
    {X, <<>>} = cdrlib:dec_long(little, CodedType),
    long_little_loop(List),
    ok.

enc_long_little(X, Message) -> 
    list_to_binary([(X) band 16#ff, ((X) bsr 8) band 16#ff, ((X) bsr 16) band 16#ff,
		    ((X) bsr 24) band 16#ff | Message]).

bad_long() ->
    ok.

%%-----------------------------------------------------------------
%% Test Case: unsigned long integer test
%% Description: 
%%-----------------------------------------------------------------
ulong(_) -> 
    ulong_big_loop([0, 4040, 65535, 40404040, 2147483647, 4294967295]),
    ulong_little_loop([0, 4040, 65535, 40404040, 2147483647, 4294967295]),
    bad_ulong().
    

ulong_big_loop([]) ->
    ok;
ulong_big_loop([X |List]) ->
    [CodedType] = cdrlib:enc_unsigned_long(X, []),
    {X, <<>>} = cdrlib:dec_unsigned_long(big, CodedType),
    ulong_big_loop(List),
    ok.

ulong_little_loop([]) ->
    ok;
ulong_little_loop([X |List]) ->
    CodedType = enc_ulong_little(X, []),
    {X, <<>>} = cdrlib:dec_unsigned_long(little, CodedType),
    ulong_little_loop(List),
    ok.

enc_ulong_little(X, Message) -> 
    list_to_binary([(X) band 16#ff, ((X) bsr 8) band 16#ff, ((X) bsr 16) band 16#ff,
		    ((X) bsr 24) band 16#ff | Message]).


bad_ulong() ->
    ok.

%%-----------------------------------------------------------------
%% Test Case: long integer test
%% Description: 
%%-----------------------------------------------------------------
longlong(_) ->
    longlong_big_loop([-2147483648, -40404040, -32768, -4040, -1,
		   0, 4040, 32767, 40404040, 2147483647]),
    longlong_little_loop([-2147483648, -40404040, -32768, -4040, -1,
		      0, 4040, 32767, 40404040, 2147483647]),
    bad_longlong().
    

longlong_big_loop([]) ->
    ok;
longlong_big_loop([X |List]) ->
    [CodedType] = cdrlib:enc_longlong(X, []),
    {X, <<>>} = cdrlib:dec_longlong(big, CodedType),
    longlong_big_loop(List),
    ok.

longlong_little_loop([]) ->
    ok;
longlong_little_loop([X |List]) ->
    CodedType = enc_longlong_little(X, []),
    {X, <<>>} = cdrlib:dec_longlong(little, CodedType),
    longlong_little_loop(List),
    ok.

enc_longlong_little(X, Message) -> 
    list_to_binary([(X) band 16#ff, ((X) bsr 8) band 16#ff, ((X) bsr 16) band 16#ff,
		    ((X) bsr 24) band 16#ff, ((X) bsr 32) band 16#ff, ((X) bsr 40) band 16#ff,
		    ((X) bsr 48) band 16#ff, ((X) bsr 56) band 16#ff | Message]).

bad_longlong() ->
    ok.

%%-----------------------------------------------------------------
%% Test Case: unsigned long integer test
%% Description: 
%%-----------------------------------------------------------------
ulonglong(_) -> 
    ulonglong_big_loop([0, 4040, 65535, 40404040, 2147483647, 4294967295]),
    ulonglong_little_loop([0, 4040, 65535, 40404040, 2147483647, 4294967295]),
    bad_ulonglong().
    

ulonglong_big_loop([]) ->
    ok;
ulonglong_big_loop([X |List]) ->
    [CodedType] = cdrlib:enc_unsigned_longlong(X, []),
    {X, <<>>} = cdrlib:dec_unsigned_longlong(big, CodedType),
    ulonglong_big_loop(List),
    ok.

ulonglong_little_loop([]) ->
    ok;
ulonglong_little_loop([X |List]) ->
    CodedType = enc_ulonglong_little(X, []),
    {X, <<>>} = cdrlib:dec_unsigned_longlong(little, CodedType),
    ulonglong_little_loop(List),
    ok.

enc_ulonglong_little(X, Message) -> 
    list_to_binary([(X) band 16#ff, ((X) bsr 8) band 16#ff, ((X) bsr 16) band 16#ff,
		    ((X) bsr 24) band 16#ff, ((X) bsr 32) band 16#ff, ((X) bsr 40) band 16#ff,
		    ((X) bsr 48) band 16#ff, ((X) bsr 56) band 16#ff | Message]).

bad_ulonglong() ->
    ok.



%%-----------------------------------------------------------------
%% Test Case: boolean test
%% Description: 
%%-----------------------------------------------------------------
boolean(_) ->
    [CodedTrue] = cdrlib:enc_bool('true', []),
    {'true', <<>>} = cdrlib:dec_bool(CodedTrue),
    [CodedFalse] = cdrlib:enc_bool('false', []),
    {'false', <<>>} = cdrlib:dec_bool(CodedFalse),
    ok.

%%-----------------------------------------------------------------
%% Test Case: character test
%% Description: 
%%-----------------------------------------------------------------
character(_) ->
    [Coded_0] = cdrlib:enc_char($0, []),
    {$0, <<>>} = cdrlib:dec_char(Coded_0),
    [Coded_a] = cdrlib:enc_char($a, []),
    {$a, <<>>} = cdrlib:dec_char(Coded_a),
    [Coded_Z] = cdrlib:enc_char($Z, []),
    {$Z, <<>>} = cdrlib:dec_char(Coded_Z),
    [Coded_dollar] = cdrlib:enc_char($$, []),
    {$$, <<>>} = cdrlib:dec_char(Coded_dollar),
    ok.

%%-----------------------------------------------------------------
%% Test Case: octet test
%% Description: 
%%-----------------------------------------------------------------
octet(_) ->
    [Coded_ff] = cdrlib:enc_octet(16#ff, []),
    {16#ff, <<>>} = cdrlib:dec_octet(Coded_ff),
    [Coded_00] = cdrlib:enc_octet(16#00, []),
    {16#00, <<>>} = cdrlib:dec_octet(Coded_00),
    [Coded_5a] = cdrlib:enc_octet(16#5a, []),
    {16#5a, <<>>} = cdrlib:dec_octet(Coded_5a),
    [Coded_48] = cdrlib:enc_octet(16#48, []),
    {16#48, <<>>} = cdrlib:dec_octet(Coded_48),
    ok.



%%-----------------------------------------------------------------
%% Test Case: float test
%% Description: 
%%-----------------------------------------------------------------
float(_) ->
    G = 16#7fffff / 16#800000 + 1.0,
    H1 = math:pow(2, 127),
    H2 = math:pow(2, -126),
    float_big_loop([-H1 * G, -H1 * 1.0, -H2 * G, -H2 * 1.0,
		    -4040.313131, -3.141592, 0.0, 3.141592, 4040.313131,
		    H1 * G, H1 * 1.0, H2 * G, H2 * 1.0]),
    float_little_loop([-H1 * G, -H1 * 1.0, -H2 * G, -H2 * 1.0,
		       -4040.313131, -3.141592, 0.0, 3.141592, 4040.313131,
		       H1 * G, H1 * 1.0, H2 * G, H2 * 1.0]),
    ok.

float_big_loop([]) ->
    ok;
float_big_loop([X |List]) ->
    [CodedType] = cdrlib:enc_float(X, []),
    {Y, <<>>} = cdrlib:dec_float(big, CodedType),
    float_comp(X,Y),
    float_big_loop(List),
    ok.

float_little_loop([]) ->
    ok;
float_little_loop([X |List]) ->
    [CodedType] = enc_float_little(X, []),
    {Y, <<>>} = cdrlib:dec_float(little, CodedType),
    float_comp(X,Y),
    float_little_loop(List),
    ok.

float_comp(X,Y) when X == 0.0, Y == 0.0 ->
    ok;
float_comp(X,Y) ->
    Div = abs(Y) / abs(X),
    %% io:format("~p~n", [float_to_list(Div)]),
    true = (Div < 1.0000001),
    true = (Div > 0.9999999),
    ok.

enc_float_little(X, Message) -> 
    [ <<X:32/little-float>> | Message].

%%-----------------------------------------------------------------
%% Test Case: double test
%% Description: 
%%-----------------------------------------------------------------
double(_) ->
    F = 16#0fffffffffffff / 16#10000000000000 + 1.0,
    E1 = math:pow(2, 1023),
    E2 = math:pow(2, -1022),
    G = 16#7fffff / 16#800000 + 1.0,
    H1 = math:pow(2, 128),
    H2 = math:pow(2, -127),
    double_big_loop([-E1 * F, -E1 * 1.0, -E2 * F, -E2 * 1.0,
		     -H1 * G, -H1 * 1.0, -H2 * G, -H2 * 1.0,
		     -4040.313131, -3.141592, 0.0, 3.141592, 4040.313131,
		     H1 * G, H1 * 1.0, H2 * G, H2 * 1.0,
		     E1 * F, E1 * 1.0, E2 * F, E2 * 1.0]),
    double_little_loop([-E1 * F, -E1 * 1.0, -E2 * F, -E2 * 1.0,
			-H1 * G, -H1 * 1.0, -H2 * G, -H2 * 1.0,
			-4040.313131, -3.141592, 0.0, 3.141592, 4040.313131,
			H1 * G, H1 * 1.0, H2 * G, H2 * 1.0,
			E1 * F, E1 * 1.0, E2 * F, E2 * 1.0]),
    ok.

double_big_loop([]) ->
    ok;
double_big_loop([X |List]) ->
    [CodedType] = cdrlib:enc_double(X, []),
    {Y, <<>>} = cdrlib:dec_double(big, CodedType),
    double_comp(X,Y), 
    double_big_loop(List),
    ok.

double_little_loop([]) ->
    ok;
double_little_loop([X |List]) ->
    [CodedType] = enc_double_little(X, []),
    {Y, <<>>} = cdrlib:dec_double(little, CodedType),
    double_comp(X,Y), 
    double_little_loop(List),
    ok.

enc_double_little(X, Message) -> 
    [ <<X:64/little-float>> | Message].

double_comp(X,Y) when X == 0.0, Y == 0.0 ->
    ok;
double_comp(X,Y) ->
    Div = abs(Y) / abs(X),
    %% io:format("~p~n", [float_to_list(Div)]),
    true = (Div < 1.00000000000001),
    true = (Div > 0.99999999999999),
    ok.

double_should_be_ok(_) ->
    F = 16#0fffffffffffff / 16#10000000000000 + 1.0,
    E1 = math:pow(2, 1024), % erlang can't handle this.
    E2 = math:pow(2, -1023),
    double_big_loop([-E1 * F, -E1 * 1.0, -E2 * F, -E2 * 1.0,
		     E1 * F, E1 * 1.0, E2 * F, E2 * 1.0]),
    double_little_loop([-E1 * F, -E1 * 1.0, -E2 * F, -E2 * 1.0,
			E1 * F, E1 * 1.0, E2 * F, E2 * 1.0]),
    ok.

%%-----------------------------------------------------------------
%% Test Case: enum test
%% Description: 
%%-----------------------------------------------------------------
enum(_) ->
    enum_big(),
    enum_little(),
    ok.

enum_big() ->
    [Coded_a] = cdrlib:enc_enum(a,[a,b,c],[]),
    {a, <<>>} = cdrlib:dec_enum(big, ["a","b","c"], Coded_a),
    [Coded_b] = cdrlib:enc_enum(b,[a,b,c],[]),
    {b, <<>>} = cdrlib:dec_enum(big, ["a","b","c"], Coded_b),
    [Coded_c] = cdrlib:enc_enum(c,[a,b,c],[]),
    {c, <<>>} = cdrlib:dec_enum(big, ["a","b","c"], Coded_c),
    ok.

enum_little() ->
    Coded_a = enc_r_enum(a,[a,b,c],[]),
    {a, <<>>} = cdrlib:dec_enum(little, ["a","b","c"], Coded_a),
    Coded_b = enc_r_enum(b,[a,b,c],[]),
    {b, <<>>} = cdrlib:dec_enum(little, ["a","b","c"], Coded_b),
    Coded_c = enc_r_enum(c,[a,b,c],[]),
    {c, <<>>} = cdrlib:dec_enum(little, ["a","b","c"], Coded_c),
    ok.

enc_r_enum(Enum, ElemList, Message) ->
    Val = getEnumValue(ElemList,Enum, 0),
    enc_r_unsigned_long(Val, Message).

getEnumValue([Enum |_List], Enum, N) ->
    N;
getEnumValue([_ |List], Enum, N) ->
    getEnumValue(List, Enum, N + 1).

enc_r_unsigned_long(X, Message) -> 
    list_to_binary([(X) band 16#ff, ((X) bsr 8) band 16#ff,
		    ((X) bsr 16) band 16#ff, ((X) bsr 24) band 16#ff | Message]).
