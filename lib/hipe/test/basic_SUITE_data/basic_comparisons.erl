%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%%
%%% Contains tests for correct execution of comparison operators.
%%%-------------------------------------------------------------------
-module(basic_comparisons).

-export([test/0]).

test() ->
  Ns = [0, 0.0, 42, 42.0, gazonk],
  T1F4 = [true, false, false, false, false],
  T2F3 = [true, true, false, false, false],
  F1T4 = [false, true, true, true, true],
  F2T3 = [false, false, true, true, true],
  %% tests for calls
  T1F4 = [eq_exact_call(0, N) || N <- Ns],
  F1T4 = [ne_exact_call(0, N) || N <- Ns],
  T2F3 = [eq_call(0, N) || N <- Ns],
  F2T3 = [ne_call(0, N) || N <- Ns],
  %% tests for guards
  T1F4 = [eq_exact_guard(0, N) || N <- Ns],
  F1T4 = [ne_exact_guard(0, N) || N <- Ns],
  T2F3 = [eq_guard(0, N) || N <- Ns],
  F2T3 = [ne_guard(0, N) || N <- Ns],
  %% some more tests
  ok = test_against_zero(),
  ok = test_against_other_terms(),
  ok = test_sofs_func(),
  ok.

test_against_zero() ->
  Xs = [0, 1, 0.0],
  [true, false, false] = [is_zero_int(X) || X <- Xs],
  [true, false,  true] = [is_zero_num(X) || X <- Xs],
  [false, true,  true] = [is_nonzero_int(X) || X <- Xs],
  [false, true, false] = [is_nonzero_num(X) || X <- Xs],
  ok.

test_against_other_terms() ->
  TTT = {true, true, true},
  FFF = {false, false, false},
  TTT = {is_foo_exact(foo), is_foo_term1(foo), is_foo_term2(foo)},
  FFF = {is_foo_exact(bar), is_foo_term1(bar), is_foo_term2(bar)},
  FFF = {is_nonfoo_exact(foo), is_nonfoo_term1(foo), is_nonfoo_term2(foo)},
  TTT = {is_nonfoo_exact(bar), is_nonfoo_term1(bar), is_nonfoo_term2(bar)},
  Tup = {a, {42}, [c]},
  TTT = {is_tuple_skel(Tup), is_tuple_exact(Tup), is_tuple_term(Tup)},
  BNi = <<42>>,
  TTT = {is_bin_exact(BNi), is_bin_term1(BNi), is_bin_term2(BNi)},
  BNf = <<42/float>>,
  FFF = {is_bin_exact(BNf), is_bin_term1(BNf), is_bin_term2(BNf)},
  ok.

test_sofs_func() ->
  L = [0, 0.0],
  ok = sofs_func(L, L, L).

%%--------------------------------------------------------------------
%% Test for comparison operators used in body calls

eq_exact_call(X, Y) -> X =:= Y.

ne_exact_call(X, Y) -> X =/= Y.

eq_call(X, Y) -> X == Y.

ne_call(X, Y) -> X /= Y.

%%--------------------------------------------------------------------
%% Tests for comparison operators used as guards

eq_exact_guard(X, Y) when X =:= Y -> true;
eq_exact_guard(_, _) -> false.

ne_exact_guard(X, Y) when X =/= Y -> true;
ne_exact_guard(_, _) -> false.

eq_guard(X, Y) when X == Y -> true;
eq_guard(_, _) -> false.

ne_guard(X, Y) when X /= Y -> true;
ne_guard(_, _) -> false.

%%--------------------------------------------------------------------

is_zero_int(N) when N =:= 0 -> true;
is_zero_int(_) -> false.

is_nonzero_int(N) when N =/= 0 -> true;
is_nonzero_int(_) -> false.

is_zero_num(N) when N == 0 -> true;
is_zero_num(_) -> false.

is_nonzero_num(N) when N /= 0 -> true;
is_nonzero_num(_) -> false.

%%--------------------------------------------------------------------
%% There should not really be any difference in the generated code
%% for the following three functions.

is_foo_exact(A) when A =:= foo -> true;
is_foo_exact(_) -> false.

is_foo_term1(A) when A == foo -> true;
is_foo_term1(_) -> false.

is_foo_term2(A) when foo == A -> true;
is_foo_term2(_) -> false.

%%--------------------------------------------------------------------
%% Same for these cases

is_nonfoo_exact(A) when A =/= foo -> true;
is_nonfoo_exact(_) -> false.

is_nonfoo_term1(A) when A /= foo -> true;
is_nonfoo_term1(_) -> false.

is_nonfoo_term2(A) when foo /= A -> true;
is_nonfoo_term2(_) -> false.

%%--------------------------------------------------------------------

is_tuple_skel({A,{B},[C]}) when is_atom(A), is_integer(B), is_atom(C) -> true;
is_tuple_skel(T) when is_tuple(T) -> false.

is_tuple_exact(T) when T =:= {a,{42},[c]} -> true;
is_tuple_exact(T) when is_tuple(T) -> false.

is_tuple_term(T) when T == {a,{42.0},[c]} -> true;
is_tuple_term(T) when is_tuple(T) -> false.

%%--------------------------------------------------------------------
%% But for binaries the treatment has to be different, due to the need
%% for construction of the binary in the guard.

is_bin_exact(B) when B =:= <<42>> -> true;
is_bin_exact(_) -> false.

is_bin_term1(B) when B == <<42>> -> true;
is_bin_term1(_) -> false.

is_bin_term2(B) when <<42>> == B -> true;
is_bin_term2(_) -> false.

%%--------------------------------------------------------------------
%% a test from sofs.erl which failed at some point

sofs_func([X | Ts], X0, L) when X /= X0 ->
  sofs_func(Ts, X, L);
sofs_func([X | _Ts], X0, _L) when X == X0 ->
  ok;
sofs_func([], _X0, L) ->
  L.
