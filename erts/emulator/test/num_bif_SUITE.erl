%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
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
	 t_string_to_integer/1, t_list_to_integer_edge_cases/1,
	 t_string_to_float_safe/1, t_string_to_float_risky/1,
	 t_round/1, t_trunc_and_friends/1
     ]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [t_abs, t_float, t_float_to_string, t_integer_to_string,
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

    fts_rand_float_decimals(1000),

    ok.

test_fts(Expect, Float) ->
    Expect = float_to_list(Float),
    BinExpect = list_to_binary(Expect),
    BinExpect = float_to_binary(Float).

test_fts(Expect, Float, Args) ->
    Expect = float_to_list(Float,Args),
    BinExpect = list_to_binary(Expect),
    BinExpect = float_to_binary(Float,Args).


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
         L1 = case D of
                  0 -> L0 ++ ".0";
                  _ -> L0
              end,
         F1 = list_to_float(L1),
         Diff = abs(F0-F1),
         MaxDiff = max_diff_decimals(F0, D),
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

    % Interesting values around 2-pows, such as MIN_SMALL and MAX_SMALL.
    lists:foreach(fun(Bits) ->
			  N = 1 bsl Bits,
			  test_sti(N - 1),
			  test_sti(N),
			  test_sti(N + 1)
		  end,
		  lists:seq(16, 130)),

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
		  end,["1.0"," 1"," -1","","+"]),

    % Custom base error cases
    lists:foreach(fun({Value,Base}) ->
			  {'EXIT', {badarg, _}} =
			      (catch binary_to_integer(
				       list_to_binary(Value),Base)),
			  {'EXIT', {badarg, _}} =
			      (catch erlang:list_to_integer(Value,Base))
		  end,[{" 1",1},{" 1",37},{"2",2},{"B",11},{"b",11},{":", 16},
		       {"1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111z",16},
		       {"1z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",16},
		       {"111z11111111",16}]),

    %% log2 calculation overflow bug in do_integer_to_list (OTP-12624)
    %% Would crash with segv
    0 = list_to_integer(lists:duplicate(10000000,$0)),

    ok.

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

test_sti(Num,Base) ->
    Neg = -Num,
    Num = list_to_integer(int2list(Num,Base),Base),
    Neg = list_to_integer(int2list(Num*-1,Base),Base),
    Num = binary_to_integer(int2bin(Num,Base),Base),
    Neg = binary_to_integer(int2bin(Num*-1,Base),Base).

% Calling this function (which is not supposed to be inlined) prevents
% the compiler from calculating the answer, so we don't test the compiler
% instead of the newest runtime system.
id(X) -> X.

%% Uses the printing library to to integer_to_binary conversions.
int2bin(Int,Base) when Base < 37 ->
    iolist_to_binary(int2list(Int,Base)).

int2list(Int,Base) when Base < 37 ->
    lists:flatten(io_lib:format("~."++integer_to_list(Base)++"B",[Int])).
