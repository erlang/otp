%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%%
%%% Contains tests that manipulate floating point numbers.
%%%-------------------------------------------------------------------
-module(basic_floats).

-export([test/0]).
-export([test_fmt_double_fpe_leak/0]).   % suppress the unused warning

test() ->
  ok = test_arith_ops(),
  ok = test_fp_ebb(),
  ok = test_fp_phi(),
  ok = test_big_bad_float(),
  ok = test_catch_bad_fp_arith(),
  ok = test_catch_fp_conv(),
  ok = test_fp_with_fp_exceptions(),
  %% ok = test_fmt_double_fpe_leak(),    % this requires printing
  ok = test_icode_type_crash(),
  ok = test_icode_type_crash_2(),
  ok.

%%--------------------------------------------------------------------

test_arith_ops() ->
  E = 2.5617,
  5.703200000000001  = add(E),
  0.5798000000000001 = sub(E),
  8.047580550000001 = mult(E),
  -6.023e23 = negate(6.023e23),
  ok.

add(X) ->
  3.1415 + X.

sub(X) ->
  3.1415 - X.

mult(X) ->
  3.1415 * X.

%% tests the translation of the fnegate BEAM instruction.
negate(X) ->
  - (X + 0.0).

%%--------------------------------------------------------------------
%% Test the construction of overlapping extended basic blocks where
%% BEAM has constructed one and hipe_icode_fp constructs the other.
%%--------------------------------------------------------------------

test_fp_ebb() ->
  1.0 = foo(2 * math:pi()),
  1.0 = bar(2 * math:pi()),
  ok.

foo(X) ->
  X / (2 * math:pi()).

bar(X) ->
  F = float_two(),
  case F < 3.0 of
    true -> (X * F) / ((2 * F) * math:pi());
    false -> weird
  end.

float_two() ->
  2.0.

%%--------------------------------------------------------------------

test_fp_phi() ->
  10 = fp_phi(10, 100),
  undefined = fp_phi(1.1e302, 0.000000001),
  ok.

fp_phi(A, B) ->
  case catch A / B of
    {'EXIT', _Reason} -> undefined;
    _ -> round(100 * (A / B))
  end.

%%--------------------------------------------------------------------

-define(BS, "93904329458954829589425849258998492384932849328493284932849328493284932389248329432932483294832949245827588578423578435783475834758375837580745807304258924584295924588459834958349589348589345934859384958349583945893458934859438593485995348594385943859438593458934589345938594385934859483958348934589435894859485943859438594594385938459438595034950439504395043950495043593485943758.0").

test_big_bad_float() ->
  ok = try f2l(?BS) catch error:badarg -> ok end,
  ok = case catch f2l(?BS) of {'EXIT', {badarg, _}} -> ok end,
  ok.

f2l(F) ->
  float_to_list(list_to_float(F)).

%%--------------------------------------------------------------------
%% Tests catching of floating point bad arithmetic.

test_catch_bad_fp_arith() ->
 5.7 = f(2.56),
 {'EXIT', {badarith, _}} = bad_arith(9.9),
 ok.

f(F) when is_float(F) -> F + 3.14.

bad_arith(F) when is_float(F) ->
  catch F * 1.70000e+308.

%%--------------------------------------------------------------------
%% Tests proper catching of exceptions due to illegal convertion of
%% bignums to floating point numbers.

test_catch_fp_conv() ->
  F = 1.7e308, %% F is a number very close to a maximum float.
  ok = big_arith(F),
  ok = big_const_float(F),
  ok.

big_arith(F) ->
  I = trunc(F),
  {'EXIT', {badarith, _}} = big_int_arith(I),
  ok.

big_int_arith(I) when is_integer(I) ->
  catch(3.0 + 2*I).

big_const_float(F) ->
  I = trunc(F),
  badarith = try (1/(2*I)) catch error:Err -> Err end,
  _ = 2/I,
  {'EXIT', _} = (catch 4/(2*I)),
  ok.

%%--------------------------------------------------------------------
%% Forces floating point exceptions and tests that subsequent, legal,
%% operations are calculated correctly.

test_fp_with_fp_exceptions() ->
  0.0 = math:log(1.0),
  badarith = try math:log(float_minus_one()) catch error:E1 -> E1 end,
  0.0 = math:log(1.0),
  badarith = try math:log(float_zero()) catch error:E2 -> E2 end,
  0.0 = math:log(1.0),
  %% An old-fashioned exception here just so as to test this case also
  {'EXIT', _} = (catch fp_mult(3.23e133, 3.57e257)),
  0.0 = math:log(1.0),
  badarith = try fp_div(5.0, 0.0) catch error:E3 -> E3 end,
  0.0 = math:log(1.0),
  ok.

fp_mult(X, Y) -> X * Y.

fp_div(X, Y) -> X / Y.

%% The following two function definitions appear here just to shut
%% off 'expression will fail with a badarg' warnings from the compiler

float_zero() -> 0.0.

float_minus_one() -> -1.0.

%%--------------------------------------------------------------------
%% Test that erl_printf_format.c:fmt_double() does not leak pending FP
%% exceptions to subsequent code.  This used to break x87 FP code on
%% 32-bit x86.  Based on a problem report from Richard Carlsson.

test_fmt_double_fpe_leak() ->
  test_fmt_double_fpe_leak(float_zero(), int_two()),
  ok.

%% We need the specific sequence of erlang:display/1 on a float that
%% triggers faulting ops in fmt_double() followed by a simple FP BIF.
%% We also need to repeat this at least three times.
test_fmt_double_fpe_leak(X, Y) ->
  erlang:display(X), _ = math:log10(Y),
  erlang:display(X), _ = math:log10(Y),
  erlang:display(X), _ = math:log10(Y),
  erlang:display(X), _ = math:log10(Y),
  erlang:display(X),
  math:log10(Y).

int_two() -> 2.

%%--------------------------------------------------------------------
%% Contains code which confuses the icode_type analysis and results
%% in a compiler crash.  Stipped down from code sent by Paul Guyot.
%% Compiles alright with the option 'no_icode_type' but that defeats
%% the purpose of the test.

test_icode_type_crash() ->
  Fun = f(1, 2, 3),
  42.0 = Fun(),
  ok.

f(A, B, C) ->
  fun () ->
      X = case A of
	    0 -> 1 / B;
	    _ -> A / C
	  end,
      Y = case B of
	    a -> 1.0;
	    b -> 2.0;
	    _ -> 6.0
	  end,
      Z = case C of
	    c -> 0.1 * X;
	    _ -> 7.0
	  end,
      Y * Z
  end.

%%--------------------------------------------------------------------
%% Contains another case that crashed hipe_icode_fp. This sample was
%% sent by Mattias Jansson (25 Nov 2015). It compiled alright with the
%% option 'no_icode_type' but that defeats the purpose of the test.
%% Unfortunately, the execution of this code goes into an infinite
%% loop, even if the map in the second argument of eat_what/2 gets the
%% appropriate key-value pairs. Still, it is retained as a test
%% because it exposed a different crash than test_icode_type_crash/0.

test_icode_type_crash_2() ->
  {'EXIT', {function_clause, _}} = (catch eat()),
  ok.

eat() ->
  eat_what(1.0, #{}).

eat_what(Factor, #{rat_type := LT} = Rat) ->
  #{cheese := Cheese} = Rat,
  UnitCheese = Cheese / 2,
  RetA = case eat() of
	   {full, RetA1} ->
	     CheeseB2 = min(RetA1, UnitCheese) * Factor,
	     case eat() of
	       full -> {win, RetA1};
	       hungry -> {partial, RetA1 - CheeseB2}
	     end;
	   AOther -> AOther
	 end,
  RetB = case eat() of
	   {full, RetB1} ->
	     CheeseA2 = min(RetB1, UnitCheese) * Factor,
	     rat:init(single, LT, CheeseA2),
	     case eat() of
	       full -> {full, RetB1};
	       hungry -> {hungry, RetB1 - CheeseA2}
	     end
	 end,
  {RetA, RetB}.
