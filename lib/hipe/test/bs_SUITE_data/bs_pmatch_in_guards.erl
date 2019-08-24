%% -*- erlang-indent-level: 2 -*-
%%--------------------------------------------------------------------
%% Tests that basic cases of binary pattern matching in guards work
%%--------------------------------------------------------------------
-module(bs_pmatch_in_guards).

-export([test/0]).

test() ->
  1 = in_guard(<<16#74ad:16>>, 16#e95, 5),
  2 = in_guard(<<16#3A,16#F7,"hello">>, 16#3AF7, <<"hello">>),
  3 = in_guard(<<16#FBCD:14,3.1415/float,3:2>>, 16#FBCD, 3.1415),
  nope = in_guard(<<1>>, 42, b),
  nope = in_guard(<<1>>, a, b),
  nope = in_guard(<<1,2>>, 1, 1),
  nope = in_guard(<<4,5>>, 1, 2.71),
  nope = in_guard(<<4,5>>, 1, <<12,13>>),
  ok.

in_guard(Bin, A, B) when <<A:13,B:3>> == Bin -> 1;
in_guard(Bin, A, B) when <<A:16,B/binary>> == Bin -> 2;
in_guard(Bin, A, B) when <<A:14,B/float,3:2>> == Bin -> 3;
in_guard(_, _, _) -> nope.
