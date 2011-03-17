%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2011. All Rights Reserved.
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
%%-----------------------------------------------------------------
%% 
%% Description:
%% Test suite for the CDR basic type encode/decode functions
%%
%%-----------------------------------------------------------------
-module(cdrlib_SUITE).

-include_lib("test_server/include/test_server.hrl").

-define(default_timeout, ?t:minutes(3)).

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
    ?line Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].


end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

%%-----------------------------------------------------------------
%% Test Case: short integer test
%% Description: 
%%-----------------------------------------------------------------
short(doc) -> ["Description", "more description"];
short(suite) -> [];
short(_) ->
    short_big_loop([-32768, -4040, -1, 0, 4040, 32767]),
    short_little_loop([-32768, -4040, -1, 0, 4040, 32767]),
    bad_short().

short_big_loop([]) ->
    ok;
short_big_loop([X |List]) ->
    ?line [CodedType] = cdrlib:enc_short(X, []),
    ?line {X, <<>>} = cdrlib:dec_short(big, CodedType),
    short_big_loop(List),
    ok.

short_little_loop([]) ->
    ok;
short_little_loop([X |List]) ->
    ?line CodedType = enc_short_little(X, []),
    ?line {X, <<>>} = cdrlib:dec_short(little, CodedType),
    short_little_loop(List),
    ok.

enc_short_little(X, Message) -> 
    list_to_binary([(X) band 16#ff, ((X) bsr 8) band 16#ff | Message]).

bad_short() ->
    ?line {'EXCEPTION', _} = (catch cdrlib:enc_short('atom', [])),
    ?line [CodedType] = cdrlib:enc_char($a, []),
    ?line {'EXIT', _} = (catch cdrlib:dec_short(big, CodedType)),
    ok.
%%-----------------------------------------------------------------
%% Test Case: unsigned short integer test
%% Description: 
%%-----------------------------------------------------------------
ushort(doc) -> ["Description", "more description"];
ushort(suite) -> [];
ushort(_) ->
    ushort_big_loop([0, 4040, 65535]),
    ushort_little_loop([0, 4040, 65535]),
    bad_ushort().

ushort_big_loop([]) ->
    ok;
ushort_big_loop([X |List]) ->
    ?line [CodedType] = cdrlib:enc_unsigned_short(X, []),
    ?line {X, <<>>} = cdrlib:dec_unsigned_short(big, CodedType),
    ushort_big_loop(List),
    ok.

ushort_little_loop([]) ->
    ok;
ushort_little_loop([X |List]) ->
    ?line CodedType = enc_ushort_little(X, []),
    ?line {X, <<>>} = cdrlib:dec_unsigned_short(little, CodedType),
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
long(doc) -> ["Description", "more description"];
long(suite) -> [];
long(_) ->
    long_big_loop([-2147483648, -40404040, -32768, -4040, -1,
		   0, 4040, 32767, 40404040, 2147483647]),
    long_little_loop([-2147483648, -40404040, -32768, -4040, -1,
		      0, 4040, 32767, 40404040, 2147483647]),
    bad_long().
    

long_big_loop([]) ->
    ok;
long_big_loop([X |List]) ->
    ?line [CodedType] = cdrlib:enc_long(X, []),
    ?line {X, <<>>} = cdrlib:dec_long(big, CodedType),
    long_big_loop(List),
    ok.

long_little_loop([]) ->
    ok;
long_little_loop([X |List]) ->
    ?line CodedType = enc_long_little(X, []),
    ?line {X, <<>>} = cdrlib:dec_long(little, CodedType),
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
ulong(doc) -> ["Description", "more description"];
ulong(suite) -> [];
ulong(_) -> 
    ulong_big_loop([0, 4040, 65535, 40404040, 2147483647, 4294967295]),
    ulong_little_loop([0, 4040, 65535, 40404040, 2147483647, 4294967295]),
    bad_ulong().
    

ulong_big_loop([]) ->
    ok;
ulong_big_loop([X |List]) ->
    ?line [CodedType] = cdrlib:enc_unsigned_long(X, []),
    ?line {X, <<>>} = cdrlib:dec_unsigned_long(big, CodedType),
    ulong_big_loop(List),
    ok.

ulong_little_loop([]) ->
    ok;
ulong_little_loop([X |List]) ->
    ?line CodedType = enc_ulong_little(X, []),
    ?line {X, <<>>} = cdrlib:dec_unsigned_long(little, CodedType),
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
longlong(doc) -> ["Description", "more description"];
longlong(suite) -> [];
longlong(_) ->
    longlong_big_loop([-2147483648, -40404040, -32768, -4040, -1,
		   0, 4040, 32767, 40404040, 2147483647]),
    longlong_little_loop([-2147483648, -40404040, -32768, -4040, -1,
		      0, 4040, 32767, 40404040, 2147483647]),
    bad_longlong().
    

longlong_big_loop([]) ->
    ok;
longlong_big_loop([X |List]) ->
    ?line [CodedType] = cdrlib:enc_longlong(X, []),
    ?line {X, <<>>} = cdrlib:dec_longlong(big, CodedType),
    longlong_big_loop(List),
    ok.

longlong_little_loop([]) ->
    ok;
longlong_little_loop([X |List]) ->
    ?line CodedType = enc_longlong_little(X, []),
    ?line {X, <<>>} = cdrlib:dec_longlong(little, CodedType),
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
ulonglong(doc) -> ["Description", "more description"];
ulonglong(suite) -> [];
ulonglong(_) -> 
    ulonglong_big_loop([0, 4040, 65535, 40404040, 2147483647, 4294967295]),
    ulonglong_little_loop([0, 4040, 65535, 40404040, 2147483647, 4294967295]),
    bad_ulonglong().
    

ulonglong_big_loop([]) ->
    ok;
ulonglong_big_loop([X |List]) ->
    ?line [CodedType] = cdrlib:enc_unsigned_longlong(X, []),
    ?line {X, <<>>} = cdrlib:dec_unsigned_longlong(big, CodedType),
    ulonglong_big_loop(List),
    ok.

ulonglong_little_loop([]) ->
    ok;
ulonglong_little_loop([X |List]) ->
    ?line CodedType = enc_ulonglong_little(X, []),
    ?line {X, <<>>} = cdrlib:dec_unsigned_longlong(little, CodedType),
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
boolean(doc) -> ["Description", "more description"];
boolean(suite) -> [];
boolean(_) ->
    ?line [CodedTrue] = cdrlib:enc_bool('true', []),
    ?line {'true', <<>>} = cdrlib:dec_bool(CodedTrue),
    ?line [CodedFalse] = cdrlib:enc_bool('false', []),
    ?line {'false', <<>>} = cdrlib:dec_bool(CodedFalse),
    ok.

%%-----------------------------------------------------------------
%% Test Case: character test
%% Description: 
%%-----------------------------------------------------------------
character(doc) -> ["Description", "more description"];
character(suite) -> [];
character(_) ->
    ?line [Coded_0] = cdrlib:enc_char($0, []),
    ?line {$0, <<>>} = cdrlib:dec_char(Coded_0),
    ?line [Coded_a] = cdrlib:enc_char($a, []),
    ?line {$a, <<>>} = cdrlib:dec_char(Coded_a),
    ?line [Coded_Z] = cdrlib:enc_char($Z, []),
    ?line {$Z, <<>>} = cdrlib:dec_char(Coded_Z),
    ?line [Coded_dollar] = cdrlib:enc_char($$, []),
    ?line {$$, <<>>} = cdrlib:dec_char(Coded_dollar),
    ok.

%%-----------------------------------------------------------------
%% Test Case: octet test
%% Description: 
%%-----------------------------------------------------------------
octet(doc) -> ["Description", "more description"];
octet(suite) -> [];
octet(_) ->
    ?line [Coded_ff] = cdrlib:enc_octet(16#ff, []),
    ?line {16#ff, <<>>} = cdrlib:dec_octet(Coded_ff),
    ?line [Coded_00] = cdrlib:enc_octet(16#00, []),
    ?line {16#00, <<>>} = cdrlib:dec_octet(Coded_00),
    ?line [Coded_5a] = cdrlib:enc_octet(16#5a, []),
    ?line {16#5a, <<>>} = cdrlib:dec_octet(Coded_5a),
    ?line [Coded_48] = cdrlib:enc_octet(16#48, []),
    ?line {16#48, <<>>} = cdrlib:dec_octet(Coded_48),
    ok.



%%-----------------------------------------------------------------
%% Test Case: float test
%% Description: 
%%-----------------------------------------------------------------
float(doc) -> ["Description", "more description"];
float(suite) -> [];
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
    ?line [CodedType] = cdrlib:enc_float(X, []),
    ?line {Y, <<>>} = cdrlib:dec_float(big, CodedType),
    ?line float_comp(X,Y),
    float_big_loop(List),
    ok.

float_little_loop([]) ->
    ok;
float_little_loop([X |List]) ->
    ?line [CodedType] = enc_float_little(X, []),
    ?line {Y, <<>>} = cdrlib:dec_float(little, CodedType),
    ?line float_comp(X,Y),
    float_little_loop(List),
    ok.

float_comp(X,Y) when X == 0.0, Y == 0.0 ->
    ok;
float_comp(X,Y) ->
    Div = abs(Y) / abs(X),
    %% io:format("~p~n", [float_to_list(Div)]),
    ?line true = (Div < 1.0000001),
    ?line true = (Div > 0.9999999),
    ok.

enc_float_little(X, Message) -> 
    [ <<X:32/little-float>> | Message].

%%-----------------------------------------------------------------
%% Test Case: double test
%% Description: 
%%-----------------------------------------------------------------
double(doc) -> ["Description", "more description"];
double(suite) -> [];
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
    ?line [CodedType] = cdrlib:enc_double(X, []),
    ?line {Y, <<>>} = cdrlib:dec_double(big, CodedType),
    ?line double_comp(X,Y), 
    double_big_loop(List),
    ok.

double_little_loop([]) ->
    ok;
double_little_loop([X |List]) ->
    ?line [CodedType] = enc_double_little(X, []),
    ?line {Y, <<>>} = cdrlib:dec_double(little, CodedType),
    ?line double_comp(X,Y), 
    double_little_loop(List),
    ok.

enc_double_little(X, Message) -> 
    [ <<X:64/little-float>> | Message].

double_comp(X,Y) when X == 0.0, Y == 0.0 ->
    ok;
double_comp(X,Y) ->
    Div = abs(Y) / abs(X),
    %% io:format("~p~n", [float_to_list(Div)]),
    ?line true = (Div < 1.00000000000001),
    ?line true = (Div > 0.99999999999999),
    ok.

double_should_be_ok(doc) -> ["Description", "more description"];
double_should_be_ok(suite) -> [];
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
enum(doc) -> ["Description", "more description"];
enum(suite) -> [];
enum(_) ->
    enum_big(),
    enum_little(),
    ok.

enum_big() ->
    ?line [Coded_a] = cdrlib:enc_enum(a,[a,b,c],[]),
    ?line {a, <<>>} = cdrlib:dec_enum(big, ["a","b","c"], Coded_a),
    ?line [Coded_b] = cdrlib:enc_enum(b,[a,b,c],[]),
    ?line {b, <<>>} = cdrlib:dec_enum(big, ["a","b","c"], Coded_b),
    ?line [Coded_c] = cdrlib:enc_enum(c,[a,b,c],[]),
    ?line {c, <<>>} = cdrlib:dec_enum(big, ["a","b","c"], Coded_c),
    ok.

enum_little() ->
    ?line Coded_a = enc_r_enum(a,[a,b,c],[]),
    ?line {a, <<>>} = cdrlib:dec_enum(little, ["a","b","c"], Coded_a),
    ?line Coded_b = enc_r_enum(b,[a,b,c],[]),
    ?line {b, <<>>} = cdrlib:dec_enum(little, ["a","b","c"], Coded_b),
    ?line Coded_c = enc_r_enum(c,[a,b,c],[]),
    ?line {c, <<>>} = cdrlib:dec_enum(little, ["a","b","c"], Coded_c),
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
