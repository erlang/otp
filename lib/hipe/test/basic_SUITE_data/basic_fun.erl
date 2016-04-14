%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%%
%%% Tests for correct handling of funs.
%%%-------------------------------------------------------------------
-module(basic_fun).

-export([test/0]).

-export([dummy_foo/4, add1/1, test_fun03/0]).

test() ->
  ok = test_calls(),
  ok = test_is_function(),
  ok = test_is_function2(),
  ok.

%%--------------------------------------------------------------------
%% Tests function and fun calls.

test_calls() ->
  ok = test_apply_call(?MODULE, dummy_foo),
  ok = test_fun_call(fun dummy_foo/4),
  ok = test_fun_call(fun ?MODULE:dummy_foo/4),
  ok.

test_apply_call(M, F) ->
  M:F(bar, 42, foo, 17).

test_fun_call(Fun) ->
  Fun(bar, 42, foo, 17).

dummy_foo(_, _, foo, _) -> ok.

%%--------------------------------------------------------------------
%% Tests handling of funs out of exported functions and 2-tuple funs.

test_fun03() ->
  MFPair = add1_as_2tuple(),
  4712 = do_call(add1_as_export(), 4711),
  {badfun, MFPair} = try do_call(MFPair, 88) catch error:Err -> Err end,
  true = do_guard(add1_as_export()),
  false = do_guard(MFPair), % 2-tuples do not satisfy is_function/1
  ok.

do_call(F, X) -> F(X).

do_guard(F) when is_function(F) -> true;
do_guard(_) -> false.

add1_as_export() -> fun ?MODULE:add1/1.

add1_as_2tuple() -> {?MODULE, add1}.

add1(X) -> X+1.

%%--------------------------------------------------------------------
%% Tests the is_function guard and BIF.

test_is_function() ->
  Fun = fun (X, foo) -> dummy_foo(X, mnesia_lib, foo, [X]) end,
  ok = test_when_guard(Fun),
  ok = test_if_guard(Fun),
  ok.

test_when_guard(X) when is_function(X) -> ok.

test_if_guard(X) ->
  if is_function(X) -> ok;
     true -> weird
  end.

%%--------------------------------------------------------------------
%% Tests the is_function2 guard and BIF.

test_is_function2() ->
  ok = test_guard(),
  ok = test_guard2(),
  ok = test_call(),
  ok.

test_guard() ->
  zero_fun = test_f2(fun () -> ok end),
  unary_fun = test_f2(fun(X) -> X end),
  binary_fun = test_f2(fun (X, Y) -> {X, Y} end),
  no_fun = test_f2(gazonk),
  ok.

test_f2(Fun) when is_function(Fun, 0) ->
  zero_fun;
test_f2(Fun) when is_function(Fun, 1) ->
  unary_fun;
test_f2(Fun) when is_function(Fun, 2) ->
  binary_fun;
test_f2(_) ->
  no_fun.

test_guard2() ->
  zero_fun = test_f2_n(fun () -> ok end, 0),
  unary_fun = test_f2_n(fun (X) -> X end, 1),
  binary_fun = test_f2_n(fun (X, Y) -> {X, Y} end, 2),
  no_fun = test_f2_n(gazonk, 0),
  ok.

test_f2_n(F, N) when is_function(F, N) ->
  case N of
    0 -> zero_fun;
    1 -> unary_fun;
    2 -> binary_fun
  end;
test_f2_n(_, _) ->
  no_fun.

test_call() ->
  true  = test_fn2(fun (X, Y) -> {X,Y} end, 2),
  false = test_fn2(fun (X, Y) -> {X,Y} end, 3),
  false = test_fn2(gazonk, 2),
  {'EXIT', {badarg, _TR1}} = (catch test_fn2(gazonk, gazonk)),
  {'EXIT', {badarg, _TR2}} = (catch test_fn2(fun (X, Y) -> {X, Y} end, gazonk)),
  ok.

test_fn2(F, N) ->
  is_function(F, N).
