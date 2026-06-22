%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1997-2026. All Rights Reserved.
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

-module(num_bif_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

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
%%      floor/1
%%      ceil/1
%%	integer_to_binary/1
%%	integer_to_binary/2
%%	binary_to_integer/1

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1,
	 init_per_group/2, end_per_group/2, t_abs/1, t_float/1,
	 t_float_to_string/1, t_integer_to_string/1,
         t_integer_to_string_large/1,
	 t_string_to_integer/1, t_list_to_integer_edge_cases/1,
	 t_string_to_float_safe/1, t_string_to_float_risky/1,
	 t_round/1, t_trunc_and_friends/1
     ]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [t_abs, t_float, t_float_to_string, t_integer_to_string,
     t_integer_to_string_large,
     {group, t_string_to_float}, t_string_to_integer, t_round,
     t_trunc_and_friends, t_list_to_integer_edge_cases].

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

    %% Extremely big bignums.
    Big = id(list_to_integer(id(lists:duplicate(2000, $1)))),
    {'EXIT', {badarg, _}} = (catch float(Big)),

    ok.


%% Tests float_to_list/1, float_to_list/2, float_to_binary/1, float_to_binary/2

t_float_to_string(Config) when is_list(Config) ->
    rand_seed(),
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
    test_fts("1." ++ lists:duplicate(249, $0) ++ "e+00",
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
    test_fts("10",10.0, [{decimals, 0}, compact]),
    test_fts("1.12",1.123, [{decimals, 2}]),
    test_fts("1.123",1.123, [{decimals, 3}]),
    test_fts("1.123",1.123, [{decimals, 3}, compact]),
    test_fts("1.1230",1.123, [{decimals, 4}]),
    test_fts("1.12300",1.123, [{decimals, 5}]),
    test_fts("1.123",1.123, [{decimals, 5}, compact]),
    test_fts("1.1234",1.1234,[{decimals, 6}, compact]),
    test_fts("1.00",1.005, [{decimals, 2}]),  %% 1.005 is really 1.0049999999...
    test_fts("-1.00",-1.005,[{decimals, 2}]),
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

    %% Negative zero
    <<NegZero/float>> = <<16#8000000000000000:64>>,
    "-0.0" = float_to_list(NegZero, [{decimals, 1}, compact]),
    "-0.0" = float_to_list(NegZero, [{decimals, 1}]),
    "-0.0" = float_to_list(NegZero, [short]),
    "-0.0e+00" = float_to_list(NegZero, [{scientific, 1}]),
    "-0.0e+00" = float_to_list(NegZero, [{scientific, 1}, compact]),
    <<"-0.0">> = float_to_binary(NegZero, [{decimals, 1}, compact]),
    <<"-0.0">> = float_to_binary(NegZero, [{decimals, 1}]),
    <<"-0.0">> = float_to_binary(NegZero, [short]),
    <<"-0.0e+00">> = float_to_binary(NegZero, [{scientific, 1}]),
    <<"-0.0e+00">> = float_to_binary(NegZero, [{scientific, 1}, compact]),

    fts_rand_float_decimals(1000),

    % test short option

    % test switch for big integers
    test_fts("-9007199254740991.0", -float((1 bsl 53) -1), [short]),
    test_fts("-9.007199254740992e15", -float(1 bsl 53), [short]),
    test_fts("-9.007199254740992e15", -float((1 bsl 53) +1), [short]),
    test_fts("9007199254740991.0", float((1 bsl 53) -1), [short]),
    test_fts("9.007199254740992e15", float(1 bsl 53), [short]),
    test_fts("9.007199254740992e15", float((1 bsl 53) +1), [short]),

    % test basic
    test_fts("2.018", 2.018, [short]),
    test_fts("-2.018", -2.018, [short]),

    % test switching logic between decimal and scientific
    test_fts("1.0e-6", 1.0e-6, [short]),
    test_fts("1.0e-5", 1.0e-5, [short]),
    test_fts("0.0001", 1.0e-4, [short]),
    test_fts("0.001", 1.0e-3, [short]),
    test_fts("0.01", 1.0e-2, [short]),
    test_fts("0.1", 1.0e-1, [short]),
    test_fts("1.0", 1.0e0, [short]),
    test_fts("10.0", 1.0e1, [short]),
    test_fts("100.0", 1.0e2, [short]),
    test_fts("1.0e3", 1.0e3, [short]),
    test_fts("1.0e4", 1.0e4, [short]),
    test_fts("1.0e5", 1.0e5, [short]),
    test_fts("1.0e6", 1.0e6, [short]),
    test_fts("1.0e7", 1.0e7, [short]),
    test_fts("1.234e-6", 1.234e-6, [short]),
    test_fts("1.234e-5", 1.234e-5, [short]),
    test_fts("1.234e-4", 1.234e-4, [short]),
    test_fts("0.001234", 1.234e-3, [short]),
    test_fts("0.01234", 1.234e-2, [short]),
    test_fts("0.1234", 1.234e-1, [short]),
    test_fts("1.234", 1.234e0, [short]),
    test_fts("12.34", 1.234e1, [short]),
    test_fts("123.4", 1.234e2, [short]),
    test_fts("1234.0", 1.234e3, [short]),
    test_fts("12340.0", 1.234e4, [short]),
    test_fts("1.234e5", 1.234e5, [short]),
    test_fts("1.234e6", 1.234e6, [short]),

    % test the switch to subnormals
    test_fts("2.2250738585072014e-308", 2.2250738585072014e-308, [short]),

    % test lots of trailing zeroes
    test_fts("2.9802322387695312e-8", 2.98023223876953125e-8, [short]),

    % test some ryu regressions
    test_fts("-2.109808898695963e16", -2.109808898695963e16, [short]),
    test_fts("4.940656e-318", 4.940656e-318, [short]),
    test_fts("1.18575755e-316", 1.18575755e-316, [short]),
    test_fts("2.989102097996e-312", 2.989102097996e-312, [short]),
    test_fts("9.0608011534336e15", 9.0608011534336e15, [short]),
    test_fts("4.708356024711512e18", 4.708356024711512e18, [short]),
    test_fts("9.409340012568248e18", 9.409340012568248e18, [short]),
    test_fts("1.2345678", 1.2345678, [short]),

    % test roundtrip of lots of short float strings
    [begin
         Float = binary_to_float(String),
         ?assertEqual(String, float_to_binary(Float, [short])),

         NegString = <<$-, String/binary>>,
         ?assertEqual(NegString, float_to_binary(-Float, [short]))
     end
     || String <- short_float_strings()],

    ok.

test_fts(Expect, Float) ->
    ?assertEqual(Expect, float_to_list(Float)),
    BinExpect = list_to_binary(Expect),
    ?assertEqual(BinExpect, float_to_binary(Float)).

test_fts(Expect, Float, Args) ->
    ?assertEqual(Expect, float_to_list(Float,Args)),
    BinExpect = list_to_binary(Expect),
    ?assertEqual(BinExpect, float_to_binary(Float,Args)).


rand_float_reasonable() ->
    F = rand_float(),
    case abs(F) > 1.0e238 of
        true -> rand_float_reasonable();
        false -> F
    end.

fts_rand_float_decimals(0) -> ok;
fts_rand_float_decimals(N) ->
    [begin
         F0 = rand_float_reasonable(),
         L0 = float_to_list(F0, [{decimals, D}]),
         case conform_with_io_lib_format_os(F0,D) of
             false -> ok;
             true ->
                 IOL = lists:flatten(io_lib:format("~.*f", [D, F0])),
                 true = case L0 =:= IOL of
                            true -> true;
                            false ->
                                io:format("F0 = ~w ~w\n",  [F0, <<F0/float>>]),
                                io:format("decimals = ~w\n",  [D]),
                                io:format("float_to_list = ~s\n",  [L0]),
                                io:format("io_lib:format = ~s\n",  [IOL]),
                                false
                        end
         end,
         L1 = case D of
                  0 -> L0 ++ ".0";
                  _ -> L0
              end,
         F1 = list_to_float(L1),
         Diff = abs(F0-F1),
         MaxDiff = max_diff_decimals(F0, D-1),
         ok = case Diff =< MaxDiff of
                  true -> ok;
                  false ->
                      io:format("F0 = ~w ~w\n",  [F0, <<F0/float>>]),
                      io:format("L1 = ~s\n",  [L1]),
                      io:format("F1 = ~w ~w\n",  [F1, <<F1/float>>]),
                      io:format("Diff = ~w, MaxDiff = ~w\n", [Diff, MaxDiff]),
                      error
              end
     end
     || D <- lists:seq(0,15)],

    fts_rand_float_decimals(N-1).

conform_with_io_lib_format_os(F, D) ->
    case os:type() of
        {win32,_} ->
            %% io_lib:format("~.*f") buggy on windows? OTP-15010
            false;
        _ ->
            conform_with_io_lib_format(F, D)
    end.

conform_with_io_lib_format(_, 0) ->
    %% io_lib:format("~.*f") does not support zero decimals
    false;
conform_with_io_lib_format(_, D) when D > 10 ->
    %% Seems float_to_list gets it slightly wrong sometimes for many decimals
    false;
conform_with_io_lib_format(F, D) ->
    %% io_lib:format prints '0' for input bits beyond mantissa precision
    %% float_to_list treats those unknown input bits as if they were zeros.
    math:log2(abs(F) * math:pow(10,D)) < 54.

max_diff_decimals(F, D) ->
    IntBits = floor(math:log2(abs(F))) + 1,
    FracBits = (52 - IntBits),
    Log10_2 = 0.3010299956639812,  % math:log10(2)
    MaxDec = floor(FracBits * Log10_2),

    Resolution = math:pow(2, IntBits - 53),

    (math:pow(10, -min(D,MaxDec)) / 2) + Resolution.

%% Tests list_to_float/1.

t_string_to_float_safe(Config) when is_list(Config) ->
    test_stf(0.0,"0.0"),
    test_stf(-0.0,"-0.0"),
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

    6209607916799025 = round(id(6209607916799025.0)),
    -6209607916799025 = round(id(-6209607916799025.0)),
    ok.

%% Test trunc/1, floor/1, ceil/1, and round/1.
t_trunc_and_friends(_Config) ->
    MinusZero = 0.0 / (-1.0),
    0 = trunc_and_friends(MinusZero),
    0 = trunc_and_friends(0.0),
    5 = trunc_and_friends(5.3333),
    -10 = trunc_and_friends(-10.978987),

    %% The largest smallnum, converted to float (OTP-3722):
    X = id((1 bsl 27) - 1),
    F = X + 0.0,
    io:format("X = ~p/~w/~w, F = ~p/~w/~w, trunc(F) = ~p/~w/~w~n",
	      [X, X, binary_to_list(term_to_binary(X)),
	       F, F, binary_to_list(term_to_binary(F)),
	       trunc_and_friends(F),
	       trunc_and_friends(F),
	       binary_to_list(term_to_binary(trunc_and_friends(F)))]),
    X = trunc_and_friends(F),
    X = trunc_and_friends(F+1)-1,
    X = trunc_and_friends(F-1)+1,
    X = -trunc_and_friends(-F),
    X = -trunc_and_friends(-F-1)-1,
    X = -trunc_and_friends(-F+1)+1,

    %% Bignums.
    4294967305 = trunc_and_friends(4294967305.7),
    -4294967305 = trunc_and_friends(-4294967305.7),
    18446744073709551616 = trunc_and_friends(float(1 bsl 64)),
    -18446744073709551616 = trunc_and_friends(-float(1 bsl 64)),

    %% Random.
    rand_seed(),
    t_trunc_and_friends_rand(100),
    ok.

rand_seed() ->
    rand:seed(exrop),
    io:format("\n*** rand:export_seed() = ~w\n\n", [rand:export_seed()]),
    ok.

rand_float() ->
    F0 = rand:uniform() * math:pow(10, 50*rand:normal()),
    case rand:uniform() of
        U when U < 0.5 -> -F0;
        _ -> F0
    end.

t_trunc_and_friends_rand(0) ->
    ok;
t_trunc_and_friends_rand(N) ->
    _ = trunc_and_friends(rand_float()),
    t_trunc_and_friends_rand(N-1).

trunc_and_friends(F) ->
    Trunc = trunc(F),
    Floor = floor(F),
    Ceil = ceil(F),
    Round = round(F),

    Trunc = trunc(Trunc),
    Floor = floor(Floor),
    Ceil = ceil(Ceil),
    Round = round(Round),

    Trunc = trunc(float(Trunc)),
    Floor = floor(float(Floor)),
    Ceil = ceil(float(Ceil)),
    Round = round(float(Round)),

    true = Floor =< Trunc andalso Trunc =< Ceil,
    true = Ceil - Floor =< 1,
    true = Round =:= Floor orelse Round =:= Ceil,

    if
	F < 0 ->
	    Trunc = Ceil;
	true ->
	    Trunc = Floor
    end,
    Trunc.

%% Tests integer_to_binary/{1,2} and integer_to_list/{1,2}.

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
    Big    = bin_to_int(BigBin),
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
    test_its("FFFFFFFFFF", 1099511627775, 16),
    test_its("123456789ABCDEF123456789ABCDEF123456789ABCDEF",
             108977460683796539709587792812439445667270661579197935,
             16),

    lists:foreach(fun(Value) ->
			  {'EXIT', {badarg, _}} =
			      (catch erlang:integer_to_binary(Value, 8)),
			  {'EXIT', {badarg, _}} =
			      (catch erlang:integer_to_list(Value, 8))
		  end,[atom,1.2,0.0,[$1,[$2]]]),

    ok.

test_its(List,Int) ->
    List = integer_to_list(Int),
    Binary = list_to_binary(List),
    Binary = integer_to_binary(Int).

test_its(List,Int,Base) ->
    List = integer_to_list(Int, Base),
    Binary = list_to_binary(List),
    Binary = integer_to_binary(Int, Base).

%% Exercises the bignum integer-to-string render path across the
%% schoolbook / divide-and-conquer / Burnikel-Ziegler / Barrett
%% reciprocal threshold boundaries in big.c. Each size is round-tripped
%% through both integer_to_list/integer_to_binary and back via
%% list_to_integer/binary_to_integer. Includes negatives and the
%% high-half-zero split case (a power of the rendering base).
t_integer_to_string_large(Config) when is_list(Config) ->
    rand_seed(),
    Sizes = [240, 250, 260,                 % WRITE_BIG_DC_THRESHOLD = 250
             499, 500, 501,                 % 2 * threshold (use_dc gate)
             999, 1000, 1001,               % first D&C split level
             1999, 2000, 2001,
             7999, 8000, 8001,              % spans BARRETT_LEVEL_THRESHOLD = 100 ErtsDigit
             16383, 16384, 16385,
             65535, 65536, 65537],          % deep recursion through the cache
    ImportantBases = [2, 8, 10, 16, 36],
    Bases = lists:seq(2, 36),
    _ = [check_int_to_str_size(Size, Base) ||
            Size <- Sizes,
            Base <- Bases,
            Size < 10_000 orelse lists:member(Base, ImportantBases)],
    %% Power of base => high-half-zero branch in write_big_dc_padded.
    PowBase10 = pow_int(10, 1024),
    PowList = integer_to_list(PowBase10),
    PowBase10 = list_to_integer(PowList),
    NegPow = -PowBase10,
    NegList = integer_to_list(NegPow),
    NegPow = list_to_integer(NegList),

    %% Try an integer near the system limit.
    9943072 = bit_size(integer_to_binary(1 bsl (63 bsl 16))),

    ok.

check_int_to_str_size(NumDigits, Base) ->
    N = random_int_with_digits(NumDigits, Base),
    Pos = N,
    Neg = -N,
    PosList = integer_to_list(Pos, Base),
    PosBin = integer_to_binary(Pos, Base),
    PosBin = list_to_binary(PosList),
    Pos = list_to_integer(PosList, Base),
    Pos = binary_to_integer(PosBin, Base),
    NegList = integer_to_list(Neg, Base),
    NegBin = integer_to_binary(Neg, Base),
    NegBin = list_to_binary(NegList),
    Neg = list_to_integer(NegList, Base),
    Neg = binary_to_integer(NegBin, Base),
    %% For base 10, also verify the no-base BIFs.
    case Base of
        10 ->
            PosList = integer_to_list(Pos),
            PosBin = integer_to_binary(Pos),
            Pos = list_to_integer(PosList),
            Pos = binary_to_integer(PosBin);
        _ ->
            ok
    end.

random_int_with_digits(NumDigits, Base) ->
    N = pow_int(Base, NumDigits-1),
    N + rand:uniform(N).

pow_int(_, 0) -> 1;
pow_int(B, N) when N rem 2 =:= 0 ->
    H = pow_int(B, N div 2),
    H * H;
pow_int(B, N) ->
    B * pow_int(B, N - 1).

%% Tests list_to_integer/{1,2} and binary_to_integer/{1,2}.

t_string_to_integer(Config) when is_list(Config) ->
    _ = rand:uniform(),				%Seed generator
    io:format("Seed: ~p", [rand:export_seed()]),

    0 = bin_to_int(id(<<"00">>)),
    0 = bin_to_int(id(<<"-0">>)),
    0 = bin_to_int(id(<<"+0">>)),

    test_sti(0),
    test_sti(1),
    test_sti(12),
    test_sti(42),
    test_sti(32768),
    test_sti(268435455),

    %% Interesting values around 2-pows, such as MIN_SMALL and MAX_SMALL.
    lists:foreach(fun(Bits) ->
			  N = 1 bsl Bits,
			  test_sti(N - 1),
			  test_sti(N),
			  test_sti(N + 1)
		  end,
		  lists:seq(16, 130)),

    %% Bignums
    _ = [test_sti(rand_bignum()) || _ <- lists:seq(1, 1000)],
    test_sti(123456932798748738738, 16),
    test_sti(list_to_integer(lists:duplicate(2000, $1))),

    %% Unaligned string
    Str = <<"10">>,
    UnalignStr = <<0:3, (id(Str))/binary, 0:5>>,
    <<_:3, SomeStr:2/binary, _:5>> = id(UnalignStr),
    10 = bin_to_int(SomeStr),

    %% Invalid types
    lists:foreach(fun(Value) ->
			  {'EXIT', {badarg, _}} =
			      (catch bin_to_int(Value)),
			  {'EXIT', {badarg, _}} =
			      (catch list_to_integer(Value))
		  end,[atom,1.2,0.0,[$1,[$2]]]),

    %% Default base error cases
    lists:foreach(fun(Value) ->
			  {'EXIT', {badarg, _}} =
			      (catch bin_to_int(list_to_binary(Value))),
			  {'EXIT', {badarg, _}} =
			      (catch list_to_integer(Value))
		  end,["1.0"," 1"," -1","","+"]),

    %% Custom base error cases
    lists:foreach(fun({Value,Base}) ->
			  {'EXIT', {badarg, _}} =
			      (catch binary_to_integer(list_to_binary(Value), Base)),
			  {'EXIT', {badarg, _}} =
			      (catch list_to_integer(Value, Base))
		  end,
                  [{" 1",1},{" 1",37},{"2",2},{"B",11},{"b",11},{":", 16},
                   {"1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111z",16},
                   {"1z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",16},
                   {"111z11111111",16},
                   %% Untagging atoms at the beginning of atom.names
                   %% would produce a base in the valid range.
                   {"10",true},                 %Base 4
                   {"10",'_'},                  %Base 8
                   {"10",nonode@nohost},        %Base 12
                   {"10",'$end_of_table'},      %Base 16
                   {"10",''}                    %Base 20
                  ]),

    %% System limit
    Digits = lists:duplicate(3_000_000, $9),
    {'EXIT',{system_limit,_}} = catch list_to_integer(Digits),
    _ = erlang:garbage_collect(),
    {'EXIT',{system_limit,_}} = catch list_to_integer(Digits, 16),
    _ = erlang:garbage_collect(),
    {error,system_limit} = string:to_integer(Digits),
    _ = erlang:garbage_collect(),

    ok.

rand_bignum() ->
    Sz = max(floor(rand:normal() * 128 + 64), 2*8),
    <<Int:Sz/unit:8>> = rand:bytes(Sz),
    Int.

%% Tests edge cases for list_to_integer; compares with known good values

t_list_to_integer_edge_cases(Config) when is_list(Config) ->
    %% Take integer literals and compare to their representation in ExtTerm
    T = [
        {16, "0", <<131,97,0>>},
        {16, "-0", <<131,97,0>>},

        {16, "f", <<131,97,15>>},
        {16, "-f", <<131,98,255,255,255,241>>},

        {16, "0000000000000000000000000000000000000000000000000f",
            <<131,97,15>>},
        {16, "-0000000000000000000000000000000000000000000000000f",
            <<131,98,255,255,255,241>>},

        {16, "ffffffff", <<131,110,4,0,255,255,255,255>>},
        {16, "-ffffffff", <<131,110,4,1,255,255,255,255>>},

        {16, "7fffffff", <<131,110,4,0,255,255,255,127>>},
        {16, "-7fffffff", <<131,98,128,0,0,1>>},

        {16, "ffffffffffffffff",
            <<131,110,8,0,255,255,255,255,255,255,255,255>>},
        {16, "-ffffffffffffffff",
            <<131,110,8,1,255,255,255,255,255,255,255,255>>},

        {16, "7fffffffffffffff",
            <<131,110,8,0,255,255,255,255,255,255,255,127>>},
        {16, "-7fffffffffffffff",
            <<131,110,8,1,255,255,255,255,255,255,255,127>>},

        %% Alleged 32-bit corner case (should not happen on 64-bit). At 32-4
        %% bits we may corrupt sign bit and fall out of SMALL_INT range.
        {2, "1000000000000000000000000000", <<131,98,8,0,0,0>>},
        {2, "-1000000000000000000000000000", <<131,98,248,0,0,0>>},

        %% 64-bit corner case (should not happen on 32-bit) at 64-4 bits we
        %% corrupt sign bit and fall out of SMALL_INT range (bam! all dead)
        {2, "100000000000000000000000000000000000000000000000000000000000",
            <<131,110,8,0,0,0,0,0,0,0,0,8>>},
        {2, "-100000000000000000000000000000000000000000000000000000000000",
            <<131,110,8,1,0,0,0,0,0,0,0,8>>}
    ],
    [begin
         io:format("~s base ~p vs ~p~n", [Str, Base, Bin]),
         FromStr = list_to_integer(Str, Base),
         FromStr = binary_to_term(Bin)
     end || {Base, Str, Bin} <- T],
    ok.

test_sti(Num) ->
    [begin
	 io:format("Testing ~p:~p",[Num,Base]),
	 test_sti(Num,Base)
     end|| Base <- lists:seq(2,36)].

test_sti(Num, Base) ->
    Neg = -Num,

    NumList = int2list(Num, Base),
    NegNumList = int2list(Neg, Base),

    Num = list_to_integer(NumList, Base),
    Neg = list_to_integer(NegNumList, Base),
    Num = binary_to_integer(iolist_to_binary(NumList), Base),
    Neg = binary_to_integer(iolist_to_binary(NegNumList), Base),

    if
        Base =:= 10 ->
            Num = list_to_integer(NumList),
            Neg = list_to_integer(NegNumList),
            Num = bin_to_int(iolist_to_binary(NumList)),
            Neg = bin_to_int(iolist_to_binary(NegNumList));
        true ->
            ok
    end,

    ok.

%% Calling this function (which is not supposed to be inlined)
%% prevents the compiler from calculating the answer, so we don't test
%% the compiler instead of the newest runtime system.
id(X) -> X.

%% Use the printing library to convert to list.
int2list(Int, Base) when is_integer(Base), 2 =< Base, Base =< 36 ->
    lists:flatten(io_lib:format("~."++integer_to_list(Base)++"B",[Int])).

bin_to_int(Bin) ->
    Unaligned = erts_debug:unaligned_bitstring(Bin, 3),
    try binary_to_integer(Bin) of
        Int ->
            Int = binary_to_integer(Unaligned),
            Int
    catch
        C:E ->
            try binary_to_integer(Unaligned) of
                _ ->
                    exit(should_fail)
            catch
                OtherC:OtherE when C =/= OtherC; E =/= OtherE ->
                    exit(exceptions_different)
            end
    end.

make_unaligned_sub_binary(Bin) ->
    erts_debug:unaligned_bitstring(Bin, 3).


short_float_strings() ->
    [~"1.0",            ~"10.0",           ~"100.0",          ~"1.0e3",          ~"1.0e4",          ~"1.0e5",          ~"1.0e6",          ~"1.0e7",          ~"1.0e8",         ~"1.0e9",
     ~"1.2",            ~"12.0",           ~"120.0",          ~"1.2e3",          ~"1.2e4",          ~"1.2e5",          ~"1.2e6",          ~"1.2e7",          ~"1.2e8",         ~"1.2e9",
     ~"1.23",           ~"12.3",           ~"123.0",          ~"1230.0",         ~"1.23e4",         ~"1.23e5",         ~"1.23e6",         ~"1.23e7",         ~"1.23e8",        ~"1.23e9",
     ~"1.234",          ~"12.34",          ~"123.4",          ~"1234.0",         ~"12340.0",        ~"1.234e5",        ~"1.234e6",        ~"1.234e7",        ~"1.234e8",       ~"1.234e9",
     ~"1.2345",         ~"12.345",         ~"123.45",         ~"1234.5",         ~"12345.0",        ~"123450.0",       ~"1.2345e6",       ~"1.2345e7",       ~"1.2345e8",      ~"1.2345e9",
     ~"1.23456",        ~"12.3456",        ~"123.456",        ~"1234.56",        ~"12345.6",        ~"123456.0",       ~"1234560.0",      ~"1.23456e7",      ~"1.23456e8",     ~"1.23456e9",
     ~"1.234567",       ~"12.34567",       ~"123.4567",       ~"1234.567",       ~"12345.67",       ~"123456.7",       ~"1234567.0",      ~"12345670.0",     ~"1.234567e8",    ~"1.23456e9",
     ~"1.2345678",      ~"12.345678",      ~"123.45678",      ~"1234.5678",      ~"12345.678",      ~"123456.78",      ~"1234567.8",      ~"12345678.0",     ~"123456780.0",   ~"1.2345678e9",
     ~"1.23456789",     ~"12.3456789",     ~"123.456789",     ~"1234.56789",     ~"12345.6789",     ~"123456.789",     ~"1234567.89",     ~"12345678.9",     ~"123456789.0",   ~"1234567890.0",
     ~"1.234567899",    ~"12.34567899",    ~"123.4567899",    ~"1234.567899",    ~"12345.67899",    ~"123456.7899",    ~"1234567.899",    ~"12345678.99",    ~"123456789.9",   ~"1234567899.0",
     ~"1.2345678901",   ~"12.345678901",   ~"123.45678901",   ~"1234.5678901",   ~"12345.678901",   ~"123456.78901",   ~"1234567.8901",   ~"12345678.901",   ~"123456789.01",  ~"1234567890.1",

     ~"1.23456789012345",~"12.3456789012345",~"123.456789012345",~"1234.56789012345",~"12345.6789012345",~"123456.789012345",~"1234567.89012345",~"12345678.9012345",
     ~"123456789.012345",~"1234567890.12345",~"12345678901.2345",~"123456789012.345",~"1234567890123.45",~"12345678901234.5",~"123456789012345.0",~"1234567890123456.0",

     ~"1.0e10",         ~"1.0e11",         ~"1.0e99",          ~"1.0e100",          ~"1.0e101",          ~"1.0e199",          ~"1.0e200",          ~"1.0e201",         ~"1.0e299",          ~"1.0e300",          ~"1.0e301",
     ~"1.2e10",         ~"1.2e11",         ~"1.2e99",          ~"1.2e100",          ~"1.2e101",          ~"1.2e199",          ~"1.2e200",          ~"1.2e201",         ~"1.2e299",          ~"1.2e300",          ~"1.2e301",
     ~"1.23e10",        ~"1.23e11",        ~"1.23e99",         ~"1.23e100",         ~"1.23e101",         ~"1.23e199",         ~"1.23e200",         ~"1.23e201",        ~"1.23e299",         ~"1.23e300",         ~"1.23e301",
     ~"1.234e10",       ~"1.234e11",       ~"1.234e99",        ~"1.234e100",        ~"1.234e101",        ~"1.234e199",        ~"1.234e200",        ~"1.234e201",       ~"1.234e299",        ~"1.234e300",        ~"1.234e301",
     ~"1.2345e10",      ~"1.2345e11",      ~"1.2345e99",       ~"1.2345e100",       ~"1.2345e101",       ~"1.2345e199",       ~"1.2345e200",       ~"1.2345e201",      ~"1.2345e299",       ~"1.2345e300",       ~"1.2345e301",
     ~"1.23456e10",     ~"1.23456e11",     ~"1.23456e99",      ~"1.23456e100",      ~"1.23456e101",      ~"1.23456e199",      ~"1.23456e200",      ~"1.23456e201",     ~"1.23456e299",      ~"1.23456e300",      ~"1.23456e301",
     ~"1.234567e10",    ~"1.234567e11",    ~"1.234567e99",     ~"1.234567e100",     ~"1.234567e101",     ~"1.234567e199",     ~"1.234567e200",     ~"1.234567e201",    ~"1.234567e299",     ~"1.234567e300",     ~"1.234567e301",
     ~"1.2345678e10",   ~"1.2345678e11",   ~"1.2345678e99",    ~"1.2345678e100",    ~"1.2345678e101",    ~"1.2345678e199",    ~"1.2345678e200",    ~"1.2345678e201",   ~"1.2345678e299",    ~"1.2345678e300",    ~"1.2345678e301",
     ~"12345678900.0",  ~"1.23456789e11",  ~"1.23456789e99",   ~"1.23456789e100",   ~"1.23456789e101",   ~"1.23456789e199",   ~"1.23456789e200",   ~"1.23456789e201",  ~"1.23456789e299",   ~"1.23456789e300",   ~"1.23456789e301",
     ~"12345678990.0",  ~"123456789900.0", ~"1.234567895e99",  ~"1.234567895e100",  ~"1.234567895e101",  ~"1.234567895e199",  ~"1.234567895e200",  ~"1.234567895e201", ~"1.234567895e299",  ~"1.234567895e300",  ~"1.234567895e301",
     ~"12345678901.0",  ~"123456789010.0", ~"1.2345678901e99", ~"1.2345678901e100", ~"1.2345678901e101", ~"1.2345678901e199", ~"1.2345678901e200", ~"1.2345678901e201",~"1.2345678901e299", ~"1.2345678901e300", ~"1.2345678901e301",

     ~"0.1",            ~"0.01",           ~"0.001",          ~"0.0001",         ~"1.0e-5",         ~"1.0e-6",         ~"1.0e-7",         ~"1.0e-8",         ~"1.0e-9",         ~"1.0e-10",        ~"1.0e-11",        ~"1.0e-12",
     ~"0.12",           ~"0.012",          ~"0.0012",         ~"1.2e-4",         ~"1.2e-5",         ~"1.2e-6",         ~"1.2e-7",         ~"1.2e-8",         ~"1.2e-9",         ~"1.2e-10",        ~"1.2e-11",        ~"1.2e-12",
     ~"0.123",          ~"0.0123",         ~"0.00123",        ~"1.23e-4",        ~"1.23e-5",        ~"1.23e-6",        ~"1.23e-7",        ~"1.23e-8",        ~"1.23e-9",        ~"1.23e-10",       ~"1.23e-11",       ~"1.23e-12",
     ~"0.1234",         ~"0.01234",        ~"0.001234",       ~"1.234e-4",       ~"1.234e-5",       ~"1.234e-6",       ~"1.234e-7",       ~"1.234e-8",       ~"1.234e-9",       ~"1.234e-10",      ~"1.234e-11",      ~"1.234e-12",
     ~"0.12345",        ~"0.012345",       ~"0.0012345",      ~"1.2345e-4",      ~"1.2345e-5",      ~"1.2345e-6",      ~"1.2345e-7",      ~"1.2345e-8",      ~"1.2345e-9",      ~"1.2345e-10",     ~"1.2345e-11",     ~"1.2345e-12",
     ~"0.123456",       ~"0.0123456",      ~"0.00123456",     ~"1.23456e-4",     ~"1.23456e-5",     ~"1.23456e-6",     ~"1.23456e-7",     ~"1.23456e-8",     ~"1.23456e-9",     ~"1.23456e-10",    ~"1.23456e-11",    ~"1.23456e-12",
     ~"0.1234567",      ~"0.01234567",     ~"0.001234567",    ~"1.234567e-4",    ~"1.234567e-5",    ~"1.234567e-6",    ~"1.234567e-7",    ~"1.234567e-8",    ~"1.234567e-9",    ~"1.234567e-10",   ~"1.234567e-11",   ~"1.234567e-12",
     ~"0.12345678",     ~"0.012345678",    ~"0.0012345678",   ~"1.2345678e-4",   ~"1.2345678e-5",   ~"1.2345678e-6",   ~"1.2345678e-7",   ~"1.2345678e-8",   ~"1.2345678e-9",   ~"1.2345678e-10",  ~"1.2345678e-11",  ~"1.2345678e-12",
     ~"0.123456789",    ~"0.0123456789",   ~"0.00123456789",  ~"1.23456789e-4",  ~"1.23456789e-5",  ~"1.23456789e-6",  ~"1.23456789e-7",  ~"1.23456789e-8",  ~"1.23456789e-9",  ~"1.23456789e-10", ~"1.23456789e-11", ~"1.23456789e-12"
].
