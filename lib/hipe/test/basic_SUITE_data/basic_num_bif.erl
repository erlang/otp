%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File	: basic_num_bif.erl
%%% Description : Taken from the compiler test suite
%%%-------------------------------------------------------------------
-module(basic_num_bif).

-export([test/0]).

%% Tests optimization of the BIFs:
%% 	abs/1
%%	float/1
%%	float_to_list/1
%%	integer_to_list/1
%%	list_to_float/1
%%	list_to_integer/1
%%	round/1
%%	trunc/1

test() -> 
  Funs = [fun t_abs/0, fun t_float/0,
	  fun t_float_to_list/0, fun t_integer_to_list/0,
	  fun t_list_to_float_safe/0, fun t_list_to_float_risky/0,
	  fun t_list_to_integer/0, fun t_round/0, fun t_trunc/0],
  lists:foreach(fun (F) -> ok = F() end, Funs).

t_abs() ->
  %% Floats.
  5.5 = abs(5.5),
  0.0 = abs(0.0),
  100.0 = abs(-100.0),
  %% Integers.
  5 = abs(5),
  0 = abs(0),
  100 = abs(-100),
  %% The largest smallnum. OTP-3190.
  X = (1 bsl 27) - 1,
  X = abs(X),
  X = abs(X-1)+1,
  X = abs(X+1)-1,
  X = abs(-X),
  X = abs(-X-1)-1,
  X = abs(-X+1)+1,
  %% Bignums.
  BigNum = 13984792374983749,
  BigNum = abs(BigNum),
  BigNum = abs(-BigNum),
  ok.

t_float() ->
  0.0 = float(0),
  2.5 = float(2.5),
  0.0 = float(0.0),
  -100.55 = float(-100.55),
  42.0 = float(42),
  -100.0 = float(-100),
  %% Bignums.
  4294967305.0 = float(4294967305),
  -4294967305.0 = float(-4294967305),
  %% Extremely big bignums.
  Big = list_to_integer(lists:duplicate(2000, $1)),
  {'EXIT', {badarg, _}} = (catch float(Big)),
  ok.

%% Tests float_to_list/1.

t_float_to_list() ->
  test_ftl("0.0e+0", 0.0),
  test_ftl("2.5e+1", 25.0),
  test_ftl("2.5e+0", 2.5),
  test_ftl("2.5e-1", 0.25),
  test_ftl("-3.5e+17", -350.0e15),
  ok.

test_ftl(Expect, Float) ->
  %% No on the next line -- we want the line number from t_float_to_list.
  Expect = remove_zeros(lists:reverse(float_to_list(Float)), []).

%% Removes any non-significant zeros in a floating point number.
%% Example: 2.500000e+01 -> 2.5e+1

remove_zeros([$+, $e|Rest], [$0, X|Result]) ->
  remove_zeros([$+, $e|Rest], [X|Result]);
remove_zeros([$-, $e|Rest], [$0, X|Result]) ->
  remove_zeros([$-, $e|Rest], [X|Result]);
remove_zeros([$0, $.|Rest], [$e|Result]) ->
  remove_zeros(Rest, [$., $0, $e|Result]);
remove_zeros([$0|Rest], [$e|Result]) ->
  remove_zeros(Rest, [$e|Result]);
remove_zeros([Char|Rest], Result) ->
  remove_zeros(Rest, [Char|Result]);
remove_zeros([], Result) ->
  Result.

%% Tests integer_to_list/1.

t_integer_to_list() ->
  "0" = integer_to_list(0),
  "42" = integer_to_list(42),
  "-42" = integer_to_list(-42),
  "-42" = integer_to_list(-42),
  "32768" = integer_to_list(32768),
  "268435455" = integer_to_list(268435455),
  "-268435455" = integer_to_list(-268435455),
  "123456932798748738738" = integer_to_list(123456932798748738738),
  Big_List = lists:duplicate(2000, $1),
  Big = list_to_integer(Big_List),
  Big_List = integer_to_list(Big),
  ok.

%% Tests list_to_float/1.

t_list_to_float_safe() ->
  0.0 = list_to_float("0.0"),
  0.0 = list_to_float("-0.0"),
  0.5 = list_to_float("0.5"),
  -0.5 = list_to_float("-0.5"),
  100.0 = list_to_float("1.0e2"),
  127.5 = list_to_float("127.5"),
  -199.5 = list_to_float("-199.5"),
  ok.

%% This might crash the emulator...
%% (Known to crash the Unix version of Erlang 4.4.1)

t_list_to_float_risky() ->
  Many_Ones = lists:duplicate(25000, $1),
  _ = list_to_float("2."++Many_Ones),
  {'EXIT', {badarg, _}} = (catch list_to_float("2"++Many_Ones)),
  ok.

%% Tests list_to_integer/1.

t_list_to_integer() ->
  0 = list_to_integer("0"),
  0 = list_to_integer("00"),
  0 = list_to_integer("-0"),
  1 = list_to_integer("1"),
  -1 = list_to_integer("-1"),
  42 = list_to_integer("42"),
  -12 = list_to_integer("-12"),
  32768 = list_to_integer("32768"),
  268435455 = list_to_integer("268435455"),
  -268435455 = list_to_integer("-268435455"),
  %% Bignums.
  123456932798748738738 = list_to_integer("123456932798748738738"),
  _ = list_to_integer(lists:duplicate(2000, $1)),
  ok.

%% Tests round/1.

t_round() ->
  0 = round(0.0),
  0 = round(0.4),
  1 = round(0.5),
  0 = round(-0.4),
  -1 = round(-0.5),
  255 = round(255.3),
  256 = round(255.6),
  -1033 = round(-1033.3),
  -1034 = round(-1033.6),
  %% OTP-3722:
  X = (1 bsl 27) - 1,
  MX = -X,
  MXm1 = -X-1,
  MXp1 = -X+1,
  F = X + 0.0,
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
  4294967296 = round(4294967296.1),
  4294967297 = round(4294967296.9),
  -4294967296 = -round(4294967296.1),
  -4294967297 = -round(4294967296.9),
  ok.

t_trunc() ->
  0 = trunc(0.0),
  5 = trunc(5.3333),
  -10 = trunc(-10.978987),
  %% The largest smallnum, converted to float (OTP-3722):
  X = (1 bsl 27) - 1,
  F = X + 0.0,
  %%  io:format("X = ~p/~w/~w, F = ~p/~w/~w, trunc(F) = ~p/~w/~w~n",
  %%	      [X, X, binary_to_list(term_to_binary(X)),
  %%	       F, F, binary_to_list(term_to_binary(F)),
  %%	       trunc(F), trunc(F), binary_to_list(term_to_binary(trunc(F)))]),
  X = trunc(F),
  X = trunc(F+1)-1,
  X = trunc(F-1)+1,
  X = -trunc(-F),
  X = -trunc(-F-1)-1,
  X = -trunc(-F+1)+1,
  %% Bignums.
  4294967305 = trunc(4294967305.7),
  -4294967305 = trunc(-4294967305.7),
  ok.
