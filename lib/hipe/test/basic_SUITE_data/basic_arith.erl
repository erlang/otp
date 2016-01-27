%%% -*- erlang-indent-level: 2 -*-
%%%---------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%%
%%% Contains tests cases for compilation of arithmetic.
%%%---------------------------------------------------------------------
-module(basic_arith).

-export([test/0]).

test() ->
  ok = test_rem(),
  ok = test_bit_ops(),
  ok = test_uplus(),
  ok = test_bsl_errors(),
  ok.

%%----------------------------------------------------------------------
%% Tests the remainder operator.

test_rem() ->
   2 = ret_rem(42, 20),
  -2 = ret_rem(-42, 20),
  -2 = ret_rem(-42, -20),
  {'EXIT', {badarith, _}} = ret_rem(3.14, 2),
  {'EXIT', {badarith, _}} = ret_rem(42, 3.14),
  ok.

ret_rem(X, Y) ->
  catch X rem Y.

%%----------------------------------------------------------------------
%%

test_bit_ops() ->
  2 = bbb(11, 2, 16#3ff),
  ok.

bbb(X, Y, Z) ->
  ((1 bsl X) bor Y) band Z.

%%----------------------------------------------------------------------
%% Tests unary plus: it used to be the identity function but not anymore

test_uplus() ->
  badarith = try uplus(gazonk) catch error:Err -> Err end,
  42 = uplus(42),
  ok.

uplus(X) -> +(X).

%%----------------------------------------------------------------------
%% The first part of this test triggered a bug in the emulator as one
%% of the arguments to bsl is not an integer.
%%
%% The second part triggered a compilation crash since an arithmetic
%% expression resulting in a 'system_limit' exception was statically
%% evaluated and an arithmetic result was expected.

test_bsl_errors() ->
  {'EXIT', {'badarith', _}} = (catch (t1(0, pad, 0))),
  badarith = try t2(0, pad, 0) catch error:Err1 -> Err1 end,
  system_limit = try (id(1) bsl 100000000) catch error:Err2 -> Err2 end,
  ok.

t1(_, X, _) ->
  (1 bsl X) + 1.

t2(_, X, _) ->
  (X bsl 1) + 1.

id(I) -> I.
