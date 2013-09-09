%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2013. All Rights Reserved.
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

-module(num_bif_SUITE).

-include_lib("test_server/include/test_server.hrl").

%% Tests the BIFs:
%% 	abs/1
%%	float/1
%%	float_to_list/1
%%  float_to_list/2
%%	integer_to_list/1
%%	list_to_float/1
%%	list_to_integer/1
%%	round/1
%%	trunc/1
%%	integer_to_binary/1
%%	integer_to_binary/2
%%	binary_to_integer/1

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1, 
	 init_per_group/2, end_per_group/2, t_abs/1, t_float/1,
	 t_float_to_string/1, t_integer_to_string/1,
	 t_string_to_integer/1,
	 t_string_to_float_safe/1, t_string_to_float_risky/1,
	 t_round/1, t_trunc/1
     ]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [t_abs, t_float, t_float_to_string, t_integer_to_string,
     {group, t_string_to_float}, t_string_to_integer, t_round,
     t_trunc].

groups() -> 
    [{t_string_to_float, [],
      [t_string_to_float_safe, t_string_to_float_risky]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


t_abs(Config) when is_list(Config) ->
    %% Floats.
    5.5 = abs(id(5.5)),
    0.0 = abs(id(0.0)),
    100.0 = abs(id(-100.0)),
    
    %% Integers.
    5 = abs(id(5)),
    0 = abs(id(0)),
    100 = abs(id(-100)),

    %% The largest smallnum. OTP-3190.
    X = id((1 bsl 27) - 1),
    X = abs(X),
    X = abs(X-1)+1,
    X = abs(X+1)-1,
    X = abs(-X),
    X = abs(-X-1)-1,
    X = abs(-X+1)+1,

    %% Bignums.
    BigNum = id(13984792374983749),
    BigNum = abs(BigNum),
    BigNum = abs(-BigNum),
    ok.
    
t_float(Config) when is_list(Config) ->
    0.0 = float(id(0)),
    2.5 = float(id(2.5)),
    0.0 = float(id(0.0)),
    -100.55 = float(id(-100.55)),
    42.0 = float(id(42)),
    -100.0 = float(id(-100)),

    %% Bignums.
    4294967305.0 = float(id(4294967305)),
    -4294967305.0 = float(id(-4294967305)),

    %% Extremly big bignums.
    Big = id(list_to_integer(id(lists:duplicate(2000, $1)))),
    {'EXIT', {badarg, _}} = (catch float(Big)),
    
    ok.


%% Tests float_to_list/1, float_to_list/2, float_to_binary/1, float_to_binary/2

t_float_to_string(Config) when is_list(Config) ->
    test_fts("0.00000000000000000000e+00", 0.0),
    test_fts("2.50000000000000000000e+01", 25.0),
    test_fts("2.50000000000000000000e+00", 2.5),
    test_fts("2.50000000000000000000e-01", 0.25),
    test_fts("-3.50000000000000000000e+17", -350.0e15),
    test_fts("1.00000000000000000000e+00",1.0),
    test_fts("1.00000000000000000000e+00",1.0,  []),
    test_fts("-1.00000000000000000000e+00",-1.0, []),
    test_fts("-1.00000000000000000000",-1.0, [{decimals, 20}]),
    {'EXIT', {badarg, _}} = (catch float_to_list(1.0,  [{decimals, -1}])),
    {'EXIT', {badarg, _}} = (catch float_to_list(1.0,  [{decimals, 254}])),
    {'EXIT', {badarg, _}} = (catch float_to_list(1.0,  [{scientific, 250}])),
    {'EXIT', {badarg, _}} = (catch float_to_list(1.0e+300, [{decimals, 1}])),
    {'EXIT', {badarg, _}} = (catch float_to_binary(1.0,  [{decimals, -1}])),
    {'EXIT', {badarg, _}} = (catch float_to_binary(1.0,  [{decimals, 254}])),
    {'EXIT', {badarg, _}} = (catch float_to_binary(1.0,  [{scientific, 250}])),
    {'EXIT', {badarg, _}} = (catch float_to_binary(1.0e+300, [{decimals, 1}])),
    test_fts("1.0e+300",1.0e+300, [{scientific, 1}]),
    test_fts("1.0",1.0,  [{decimals,   249}, compact]),
    test_fts("1",1.0,[{decimals,0}]),
    test_fts("2",1.9,[{decimals,0}]),
    test_fts("123456789012345680.0",123456789012345678.0,
	     [{decimals, 236}, compact]),
    {'EXIT', {badarg, _}} = (catch float_to_list(
				     123456789012345678.0, [{decimals, 237}])),
    {'EXIT', {badarg, _}} = (catch float_to_binary(
				     123456789012345678.0, [{decimals, 237}])),
    test_fts("1." ++ string:copies("0", 249) ++ "e+00",
	     1.0,  [{scientific, 249}, compact]),

    X1 = float_to_list(1.0),
    X2 = float_to_list(1.0, [{scientific, 20}]),
    X1 = X2,

    Y1 = float_to_binary(1.0),
    Y2 = float_to_binary(1.0, [{scientific, 20}]),
    Y1 = Y2,

    test_fts("1.000e+00",1.0,   [{scientific, 3}]),
    test_fts("1.000",1.0,   [{decimals,   3}]),
    test_fts("1.0",1.0, [{decimals, 1}]),
    test_fts("1.0",1.0, [{decimals, 3}, compact]),
    test_fts("1.12",1.123, [{decimals, 2}]),
    test_fts("1.123",1.123, [{decimals, 3}]),
    test_fts("1.123",1.123, [{decimals, 3}, compact]),
    test_fts("1.1230",1.123, [{decimals, 4}]),
    test_fts("1.12300",1.123, [{decimals, 5}]),
    test_fts("1.123",1.123, [{decimals, 5}, compact]),
    test_fts("1.1234",1.1234,[{decimals, 6}, compact]),
    test_fts("1.01",1.005, [{decimals, 2}]),
    test_fts("-1.01",-1.005,[{decimals, 2}]),
    test_fts("0.999",0.999, [{decimals, 3}]),
    test_fts("-0.999",-0.999,[{decimals, 3}]),
    test_fts("1.0",0.999, [{decimals, 2}, compact]),
    test_fts("-1.0",-0.999,[{decimals, 2}, compact]),
    test_fts("0.5",0.5,   [{decimals, 1}]),
    test_fts("-0.5",-0.5,  [{decimals, 1}]),
    "2.333333"  = erlang:float_to_list(7/3, [{decimals, 6}, compact]),
    "2.333333"  = erlang:float_to_list(7/3, [{decimals, 6}]),
    <<"2.333333">>  = erlang:float_to_binary(7/3, [{decimals, 6}, compact]),
    <<"2.333333">>  = erlang:float_to_binary(7/3, [{decimals, 6}]),
    test_fts("0.00000000000000000000e+00",0.0, [compact]),
    test_fts("0.0",0.0,   [{decimals, 10}, compact]),
    test_fts("123000000000000000000.0",1.23e20, [{decimals,   10}, compact]),
    test_fts("1.2300000000e+20",1.23e20, [{scientific, 10}, compact]),
    test_fts("1.23000000000000000000e+20",1.23e20, []),
    ok.
    
test_fts(Expect, Float) ->
    Expect = float_to_list(Float),
    BinExpect = list_to_binary(Expect),
    BinExpect = float_to_binary(Float).

test_fts(Expect, Float, Args) ->
    Expect = float_to_list(Float,Args),
    BinExpect = list_to_binary(Expect),
    BinExpect = float_to_binary(Float,Args).


%% Tests list_to_float/1.

t_string_to_float_safe(Config) when is_list(Config) ->
    test_stf(0.0,"0.0"),
    test_stf(0.0,"-0.0"),
    test_stf(0.5,"0.5"),
    test_stf(-0.5,"-0.5"),
    test_stf(100.0,"1.0e2"),
    test_stf(127.5,"127.5"),
    test_stf(-199.5,"-199.5"),

    {'EXIT',{badarg,_}} = (catch list_to_float(id("0"))),
    {'EXIT',{badarg,_}} = (catch list_to_float(id("0..0"))),
    {'EXIT',{badarg,_}} = (catch list_to_float(id("0e12"))),
    {'EXIT',{badarg,_}} = (catch list_to_float(id("--0.0"))),
    {'EXIT',{badarg,_}} = (catch binary_to_float(id(<<"0">>))),
    {'EXIT',{badarg,_}} = (catch binary_to_float(id(<<"0..0">>))),
    {'EXIT',{badarg,_}} = (catch binary_to_float(id(<<"0e12">>))),
    {'EXIT',{badarg,_}} = (catch binary_to_float(id(<<"--0.0">>))),

    UBin = <<0:3,(id(<<"0.0">>))/binary,0:5>>,
    <<_:3,UnAlignedBin:3/binary,0:5>> = id(UBin),
    0.0 = binary_to_float(UnAlignedBin),

    ABin = <<0:8,(id(<<"1.0">>))/binary,0:8>>,
    <<_:8,AlignedBin:3/binary,0:8>> = id(ABin),
    1.0 = binary_to_float(AlignedBin),

    ok.

%% This might crash the emulator...
%% (Known to crash the Unix version of Erlang 4.4.1)

t_string_to_float_risky(Config) when is_list(Config) ->
    Many_Ones = lists:duplicate(25000, id($1)),
    id(list_to_float("2."++Many_Ones)),
    {'EXIT', {badarg, _}} = (catch list_to_float("2"++Many_Ones)),

    id(binary_to_float(list_to_binary("2."++Many_Ones))),
    {'EXIT', {badarg, _}} = (catch binary_to_float(
				     list_to_binary("2"++Many_Ones))),
    ok.

test_stf(Expect,List) ->
    Expect = list_to_float(List),
    Bin = list_to_binary(List),
    Expect = binary_to_float(Bin).

%% Tests round/1.

t_round(Config) when is_list(Config) ->
    0 = round(id(0.0)),
    0 = round(id(0.4)),
    1 = round(id(0.5)),
    0 = round(id(-0.4)),
    -1 = round(id(-0.5)),
    255 = round(id(255.3)),
    256 = round(id(255.6)),
    -1033 = round(id(-1033.3)),
    -1034 = round(id(-1033.6)),
    
    % OTP-3722:
    X = id((1 bsl 27) - 1),
    MX = -X,
    MXm1 = -X-1,
    MXp1 = -X+1,
    F = id(X + 0.0),
    X = round(F),
    X = round(F+1)-1,
    X = round(F-1)+1,
    MX = round(-F),
    MXm1 = round(-F-1),
    MXp1 = round(-F+1),

    X = round(F+0.1),
    X = round(F+1+0.1)-1,
    X = round(F-1+0.1)+1,
    MX = round(-F+0.1),
    MXm1 = round(-F-1+0.1),
    MXp1 = round(-F+1+0.1),

    X = round(F-0.1),
    X = round(F+1-0.1)-1,
    X = round(F-1-0.1)+1,
    MX = round(-F-0.1),
    MXm1 = round(-F-1-0.1),
    MXp1 = round(-F+1-0.1),

    0.5 = abs(round(F+0.5)-(F+0.5)),
    0.5 = abs(round(F-0.5)-(F-0.5)),
    0.5 = abs(round(-F-0.5)-(-F-0.5)),
    0.5 = abs(round(-F+0.5)-(-F+0.5)),

    %% Bignums.
    4294967296 = round(id(4294967296.1)),
    4294967297 = round(id(4294967296.9)),
    -4294967296 = -round(id(4294967296.1)),
    -4294967297 = -round(id(4294967296.9)),
    ok.

t_trunc(Config) when is_list(Config) ->
    0 = trunc(id(0.0)),
    5 = trunc(id(5.3333)),
    -10 = trunc(id(-10.978987)),

    % The largest smallnum, converted to float (OTP-3722):
    X = id((1 bsl 27) - 1),
    F = id(X + 0.0),
    io:format("X = ~p/~w/~w, F = ~p/~w/~w, trunc(F) = ~p/~w/~w~n",
	      [X, X, binary_to_list(term_to_binary(X)),
	       F, F, binary_to_list(term_to_binary(F)),
	       trunc(F), trunc(F), binary_to_list(term_to_binary(trunc(F)))]),
    X = trunc(F),
    X = trunc(F+1)-1,
    X = trunc(F-1)+1,
    X = -trunc(-F),
    X = -trunc(-F-1)-1,
    X = -trunc(-F+1)+1,

    %% Bignums.
    4294967305 = trunc(id(4294967305.7)),
    -4294967305 = trunc(id(-4294967305.7)),
    ok.


%% Tests integer_to_binary/1.

t_integer_to_string(Config) when is_list(Config) ->
    test_its("0",0),
    test_its("42",42),
    test_its("-42",-42),
    test_its("32768",32768),
    test_its("268435455",268435455),
    test_its("-268435455",-268435455),
    test_its("123456932798748738738",123456932798748738738),

    %% 1 bsl 33, just beyond 32 bit
    test_its("8589934592",8589934592),
    test_its("-8589934592",-8589934592),
    %% 1 bsl 65, just beyond 64 bit
    test_its("36893488147419103232",36893488147419103232),
    test_its("-36893488147419103232",-36893488147419103232),

    %% Bignums.
    BigBin = id(list_to_binary(lists:duplicate(2000, id($1)))),
    Big    = erlang:binary_to_integer(BigBin),
    BigBin = erlang:integer_to_binary(Big),

    %% Invalid types
    lists:foreach(fun(Value) ->
			  {'EXIT', {badarg, _}} = 
			      (catch erlang:integer_to_binary(Value)),
			  {'EXIT', {badarg, _}} = 
			      (catch erlang:integer_to_list(Value))
		  end,[atom,1.2,0.0,[$1,[$2]]]),

    %% Base-2 integers
    test_its("0", 0, 2),
    test_its("1", 1, 2),
    test_its("110110", 54, 2),
    test_its("-1000000", -64, 2),
    %% Base-16 integers
    test_its("0", 0, 16),
    test_its("A", 10, 16),
    test_its("D4BE", 54462, 16),
    test_its("-D4BE", -54462, 16),

    lists:foreach(fun(Value) ->
			  {'EXIT', {badarg, _}} =
			      (catch erlang:integer_to_binary(Value, 8)),
			  {'EXIT', {badarg, _}} =
			      (catch erlang:integer_to_list(Value, 8))
		  end,[atom,1.2,0.0,[$1,[$2]]]),

    ok.

test_its(List,Int) ->
    Int = list_to_integer(List),
    Int = binary_to_integer(list_to_binary(List)).

test_its(List,Int,Base) ->
    Int = list_to_integer(List, Base),
    Int = binary_to_integer(list_to_binary(List), Base).

%% Tests binary_to_integer/1.

t_string_to_integer(Config) when is_list(Config) ->
    0 = erlang:binary_to_integer(id(<<"00">>)),
    0 = erlang:binary_to_integer(id(<<"-0">>)),
    0 = erlang:binary_to_integer(id(<<"+0">>)),

    test_sti(0),
    test_sti(1),
    test_sti(-1),
    test_sti(42),
    test_sti(-12),
    test_sti(32768),
    test_sti(268435455),
    test_sti(-268435455),

    %% 1 bsl 28 - 1, just before 32 bit bignum
    test_sti(1 bsl 28 - 1),
    %% 1 bsl 28, just beyond 32 bit small
    test_sti(1 bsl 28),
    %% 1 bsl 33, just beyond 32 bit
    test_sti(1 bsl 33),
    %% 1 bsl 60 - 1, just before 64 bit bignum
    test_sti(1 bsl 60 - 1),
    %% 1 bsl 60, just beyond 64 bit small
    test_sti(1 bsl 60),
    %% 1 bsl 65, just beyond 64 bit
    test_sti(1 bsl 65),
    %% Bignums.
    test_sti(123456932798748738738,16),
    test_sti(list_to_integer(lists:duplicate(2000, $1))),

    %% unalign string
    Str = <<"10">>,
    UnalignStr = <<0:3, (id(Str))/binary, 0:5>>,
    <<_:3, SomeStr:2/binary, _:5>> = id(UnalignStr),
    10 = erlang:binary_to_integer(SomeStr),

    %% Invalid types
    lists:foreach(fun(Value) ->
			  {'EXIT', {badarg, _}} = 
			      (catch binary_to_integer(Value)),
			  {'EXIT', {badarg, _}} = 
			      (catch erlang:list_to_integer(Value))
		  end,[atom,1.2,0.0,[$1,[$2]]]),
    
    % Default base error cases
    lists:foreach(fun(Value) ->
			  {'EXIT', {badarg, _}} = 
			      (catch erlang:binary_to_integer(
				       list_to_binary(Value))),
			  {'EXIT', {badarg, _}} = 
			      (catch erlang:list_to_integer(Value))
		  end,["1.0"," 1"," -1",""]),
    
    % Custom base error cases
    lists:foreach(fun({Value,Base}) ->
			  {'EXIT', {badarg, _}} = 
			      (catch binary_to_integer(
				       list_to_binary(Value),Base)),
			  {'EXIT', {badarg, _}} = 
			      (catch erlang:list_to_integer(Value,Base))
		  end,[{" 1",1},{" 1",37},{"2",2},{"C",11},
		       {"1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111z",16},
		       {"1z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",16},
		       {"111z11111111",16}]),
    
    ok.

test_sti(Num) ->
    [begin
	 io:format("Testing ~p:~p",[Num,Base]),
	 test_sti(Num,Base) 
     end|| Base <- lists:seq(2,36)].

test_sti(Num,Base) ->
    Num  = list_to_integer(int2list(Num,Base),Base),
    Num = -1*list_to_integer(int2list(Num*-1,Base),Base),
    Num  = binary_to_integer(int2bin(Num,Base),Base),
    Num = -1*binary_to_integer(int2bin(Num*-1,Base),Base).

% Calling this function (which is not supposed to be inlined) prevents
% the compiler from calculating the answer, so we don't test the compiler
% instead of the newest runtime system.
id(X) -> X.

%% Uses the printing library to to integer_to_binary conversions.
int2bin(Int,Base) when Base < 37 ->
    iolist_to_binary(int2list(Int,Base)).

int2list(Int,Base) when Base < 37 ->
    lists:flatten(io_lib:format("~."++integer_to_list(Base)++"B",[Int])).
