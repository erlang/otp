%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%%
%%% Contains tests for handling of BIFs in guards and body calls.
%%%-------------------------------------------------------------------
-module(basic_bifs).

-export([test/0]).

-define(BIG, 1398479237498374913984792374983749).

test() ->
  ok = test_abs(),
  ok = test_binary_part(),
  ok = test_element(),
  ok = test_float(),
  ok = test_float_to_list(),
  ok = test_integer_to_list(),
  ok = test_list_to_float(),
  ok = test_list_to_integer(),
  ok = test_round(),
  ok = test_trunc(),
  ok.

%%--------------------------------------------------------------------

test_abs() ->
  t_abs(5.5, 0.0, -100.0, 5, 0, -100, ?BIG).

t_abs(F1, F2, F3, I1, I2, I3, BigNum) ->
  %% Floats.
  5.5 = abs(F1),
  0.0 = abs(F2),
  100.0 = abs(F3),
  %% Integers.
  5 = abs(I1),
  0 = abs(I2),
  100 = abs(I3),
  %% Bignums.
  BigNum = abs(BigNum),
  BigNum = abs(-BigNum),
  ok.

%%--------------------------------------------------------------------
%% Checks that 2-ary and 3-ary BIFs can be compiled to native code.

test_binary_part() ->
  Bin = <<1,2,3,4,5,6,7,8,9,10>>,
  BinPart = bp3(Bin),
  <<7,8>> = bp2(BinPart),
  ok.

bp2(Bin) ->
  binary_part(Bin, {1, 2}).

bp3(Bin) ->
  binary_part(Bin, byte_size(Bin), -5).

%%--------------------------------------------------------------------

test_element() ->
  true  = elem({a, b}),
  false = elem({a, c}),
  other = elem(gazonk),
  ok.

elem(T) when element(1, T) == a -> element(2, T) == b;
elem(_) -> other.

%%--------------------------------------------------------------------

test_float() ->
  t_float(0, 42, -100, 2.5, 0.0, -100.42, ?BIG, -?BIG).

t_float(I1, I2, I3, F1, F2, F3, B1, B2) ->
  0.0 = float(I1),
  2.5 = float(F1),
  0.0 = float(F2),
  -100.42 = float(F3),
  42.0 = float(I2),
  -100.0 = float(I3),
  %% Bignums.
   1398479237498374913984792374983749.0 = float(B1),
  -1398479237498374913984792374983749.0 = float(B2),
  %% Extremly big bignums.
  Big = list_to_integer(duplicate(2000, $1)),
  {'EXIT', _} = (catch float(Big)),
  %% Invalid types and lists.
  {'EXIT', _} = (catch my_list_to_integer(atom)),
  {'EXIT', _} = (catch my_list_to_integer(123)),
  {'EXIT', _} = (catch my_list_to_integer([$1, [$2]])),
  {'EXIT', _} = (catch my_list_to_integer("1.2")),
  {'EXIT', _} = (catch my_list_to_integer("a")),
  {'EXIT', _} = (catch my_list_to_integer("")),
  ok.

my_list_to_integer(X) ->
  list_to_integer(X).

%%--------------------------------------------------------------------

test_float_to_list() ->
  test_ftl("0.0e+0", 0.0),
  test_ftl("2.5e+1", 25.0),
  test_ftl("2.5e+0", 2.5),
  test_ftl("2.5e-1", 0.25),
  test_ftl("-3.5e+17", -350.0e15),
  ok.

test_ftl(Expect, Float) ->
  %% No \n on the next line -- we want the line number from t_float_to_list.
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

%%--------------------------------------------------------------------

test_integer_to_list() ->
  t_integer_to_list(0, 42, 32768, 268435455, 123456932798748738738).

t_integer_to_list(I1, I2, I3, I4, BIG) ->
  "0" = integer_to_list(I1),
  "42" = integer_to_list(I2),
  "-42" = integer_to_list(-I2),
  "-42" = integer_to_list(-I2),
  "32768" = integer_to_list(I3),
  "268435455" = integer_to_list(I4),
  "-268435455" = integer_to_list(-I4),
  "123456932798748738738" = integer_to_list(BIG),
  BigList = duplicate(2000, $1),
  Big = list_to_integer(BigList),
  BigList = integer_to_list(Big),
  ok.

%%--------------------------------------------------------------------

test_list_to_float() ->
  ok = t_list_to_float_safe(),
  ok = t_list_to_float_risky().

t_list_to_float_safe() ->
  0.0 = my_list_to_float("0.0"),
  0.0 = my_list_to_float("-0.0"),
  0.5 = my_list_to_float("0.5"),
  -0.5 = my_list_to_float("-0.5"),
  100.0 = my_list_to_float("1.0e2"),
  127.5 = my_list_to_float("127.5"),
  -199.5 = my_list_to_float("-199.5"),    
  {'EXIT', _} = (catch my_list_to_float("0")),
  {'EXIT', _} = (catch my_list_to_float("0..0")),
  {'EXIT', _} = (catch my_list_to_float("0e12")),
  {'EXIT', _} = (catch my_list_to_float("--0.0")),
  ok.

my_list_to_float(X) ->
  list_to_float(X).

%% This might crash the emulator. (Used to crash Erlang 4.4.1 on Unix.)

t_list_to_float_risky() ->
  Many_Ones = duplicate(25000, $1),
  ok = case list_to_float("2." ++ Many_Ones) of
	 F when is_float(F), 0.0 < F, F =< 3.14 -> ok
       end,
  {'EXIT', _} = (catch list_to_float("2" ++ Many_Ones)),
  ok.

%%--------------------------------------------------------------------

test_list_to_integer() ->
  ok = t_list_to_integer_small("0", "00", "-0", "1", "-1", "42", "-12", 
			       "32768", "268435455", "-268435455"),
  ok = t_list_to_integer_bignum("123456932798748738738666"),
  ok.

t_list_to_integer_small(S1, S2, S3, S4, S5, S6, S7, S8, S9, S10) ->
  0 = list_to_integer(S1),
  0 = list_to_integer(S2),
  0 = list_to_integer(S3),
  1 = list_to_integer(S4),
  -1 = list_to_integer(S5),
  42 = list_to_integer(S6),
  -12 = list_to_integer(S7),
  32768 = list_to_integer(S8),
  268435455 = list_to_integer(S9),
  -268435455 = list_to_integer(S10),
  ok.

t_list_to_integer_bignum(S) ->
  123456932798748738738666 = list_to_integer(S),
  case list_to_integer(duplicate(2000, $1)) of
    I when is_integer(I), I > 123456932798748738738666 -> ok
  end.

%%--------------------------------------------------------------------

test_round() ->
  ok = t_round_small(0.0, 0.4, 0.5, -0.4, -0.5, 255.3, 255.6, -1033.3, -1033.6),
  ok = t_round_big(4294967296.1, 4294967296.9),
  ok.

t_round_small(F1, F2, F3, F4, F5, F6, F7, F8, F9) ->
  0 = round(F1),
  0 = round(F2),
  1 = round(F3),
  0 = round(F4),
  -1 = round(F5),
  255 = round(F6),
  256 = round(F7),
  -1033 = round(F8),
  -1034 = round(F9),
  ok.

t_round_big(B1, B2) ->
  4294967296 = round(B1),
  4294967297 = round(B2),
  -4294967296 = round(-B1),
  -4294967297 = round(-B2),
  ok.

%%--------------------------------------------------------------------

test_trunc() ->
  t_trunc(0.0, 5.3333, -10.978987, 4294967305.7).

t_trunc(F1, F2, F3, B) ->
  0 = trunc(F1),
  5 = trunc(F2),
  -10 = trunc(F3),
  %% Bignums.
  4294967305 = trunc(B),
  -4294967305 = trunc(-B),
  ok.

%%--------------------------------------------------------------------
%% Auxiliary functions below

duplicate(N, X) when is_integer(N), N >= 0 ->
  duplicate(N, X, []).

duplicate(0, _, L) -> L;
duplicate(N, X, L) -> duplicate(N-1, X, [X|L]).
