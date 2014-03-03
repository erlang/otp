%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%%
%%% Contains tests that manipulate and pattern match against lists.
%%%-------------------------------------------------------------------
-module(basic_lists).

-export([test/0]).

test() ->
  ok = test_length(),
  ok = test_lists_and_strings(),
  ok.

%%--------------------------------------------------------------------

test_length() ->
  Len = 42,
  Lst = mklist(Len, []),
  Len = iterate(100, Lst),
  ok.

mklist(0, L) -> L;
mklist(X, L) -> mklist(X-1, [X|L]).

iterate(0, L) -> len(L, 0);
iterate(X, L) -> len(L, 0), iterate(X-1, L).

len([_|X], L) -> len(X, L+1);
len([], L) -> L.

%%--------------------------------------------------------------------

test_lists_and_strings() ->
  LL = ["H'A", " H'B", " H'C"],
  LL2 = lists:map(fun string:strip/1, LL),
  HexFormat = fun(X, Acc) -> {string:substr(X, 3), Acc} end,
  {LL3,_Ret} = lists:mapfoldl(HexFormat, 0, LL2),
  ["A", "B", "C"] = lists:sublist(LL3, 42),
  ok.
