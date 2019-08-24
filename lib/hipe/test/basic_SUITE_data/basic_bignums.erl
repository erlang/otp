%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%%
%%% Contains code examples that test bignum arithmetic and matching.
%%%-------------------------------------------------------------------
-module(basic_bignums).

-export([test/0, test_bsl/0]).

test() ->
  ok = test_ops(),
  ok = test_big_fac(),
  ok = test_int_overfl_32(),
  ok = test_int_overfl_64(),
  ok = test_int_overfl_32_guard(),
  ok = test_int_overfl_64_guard(),
  ok.

%%--------------------------------------------------------------------
%% Define some constants for the tests of arithmetic operators

-define(X, 68719476736).
-define(Y, 98765432101234).
-define(Z, 4722366482869645213696).
-define(W, 339254531512339254531512).

-define(B1,  4398046511104).
-define(B5,  1645504557321206042154969182557350504982735865633579863348609024).
-define(B17, 86182066610968551542636378241108028056376767329454880514019834315878107616003372189510312530372009184902888961739623919010110377987011442493486117202360415845666384627768436296772219009176743399772868636439042064384).

%%--------------------------------------------------------------------

test_ops() ->
  ok = test_mult(),
  ok = test_div(),
  ok = test_round(),
  ok = test_trunc(),
  ok = test_bsl(),
  ok.

test_mult() ->
  ?Z = mult(?X, ?X),
  ok.

mult(X, Y) -> X * Y.

test_div() ->
   4 = div_f(339254531512, ?X),
   0 = div_f(?Y, ?Y+1),
  64 = div_f(?B1, ?X),
  ?X = div_f(?Z, ?X),
  1073741824 = div_f(?Z, ?B1),
  ok.

div_f(X, Y) -> X div Y.

test_round() ->
  0 = round_f(?Z, ?W),
  1 = round_f(?Y, ?Y),
  71 = round_f(?W, ?Z),
  1437 = round_f(?Y, ?X),
  47813960 = round_f(?Z, ?Y),
  4936803183406 = round_f(?W, ?X),
  ok.

trunc_f(X, Y) -> round(X/Y).

test_trunc() ->
  0 = trunc_f(?Z, ?W),
  1 = trunc_f(?Y, ?Y),
  72 = trunc_f(?W, ?Z),
  1437 = trunc_f(?Y, ?X),
  47813961 = trunc_f(?Z, ?Y),
  4936803183407 = trunc_f(?W, ?X),
  ok.

round_f(X, Y) -> trunc(X/Y).

test_bsl() ->
  ?B1  = bsl_f(1, 42),
  ?B5  = n(5, fun erlang:'bsl'/2, 1, 42), % use the operator
  ?B17 = n(17, fun bsl_f/2, 1, 42),       % use the local function
  ok.

bsl_f(X, Y) -> X bsl Y.

%% applies a binary function N times
n(1, F, X, Y) -> F(X, Y);
n(N, F, X, Y) when N > 1 -> n(N-1, F, F(X, Y), Y).

%%--------------------------------------------------------------------

-define(FAC42, 1405006117752879898543142606244511569936384000000000).

test_big_fac() ->
  ?FAC42 = fac(42),
  ok.

fac(0) -> 1;
fac(N) -> N * fac(N-1).

%%--------------------------------------------------------------------
%% Tests for correct handling of integer overflow

test_int_overfl_32() ->
  16#7FFFFFF = add(16#7FFFFFF, 0),
  16#8000000 = add(16#8000000, 0),
  16#8000001 = add(16#8000000, 1),
  case add(16#7FFFFFF, 1) of
    16#8000000 -> ok;
    -16#7FFFFFF -> error
  end.

test_int_overfl_64() ->
  16#7FFFFFFFFFFFFFF = add(16#7FFFFFFFFFFFFFF, 0),
  16#800000000000000 = add(16#800000000000000, 0),
  16#800000000000001 = add(16#800000000000000, 1),
  case add(16#7FFFFFFFFFFFFFF, 1) of
    16#800000000000000 -> ok;
    -16#7FFFFFFFFFFFFFF -> error
  end.

add(X, Y) -> X + Y.

%%--------------------------------------------------------------------
%% Tests for correct handling of integer overflow in guards

test_int_overfl_32_guard() ->
  ok = overfl_in_guard(16#7ffffff, 0),
  ok = overfl_in_guard(16#7ffffff, 16#7ffffff),
  ok.

test_int_overfl_64_guard() ->
  ok = overfl_in_guard(16#7ffffffffffffff, 0),
  ok = overfl_in_guard(16#7ffffffffffffff, 16#7ffffffffffffff),
  ok.

overfl_in_guard(X, Y) ->
  case ok of
    V when X+Y > 12 -> V;
    _ -> bad
  end.
