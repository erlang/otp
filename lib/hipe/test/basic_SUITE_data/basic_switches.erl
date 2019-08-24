%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%%
%%% Contains tests for pattern matching switches.
%%%-------------------------------------------------------------------
-module(basic_switches).

-export([test/0]).

test() ->
  ok = test_switch_mix(),
  ok.

%%---------------------------------------------------------------------

-define(BIG1, 21323233222132323322).
-define(BIG2, 4242424242424242424242424242424242).

test_switch_mix() ->
  small1 = t(42),
  small2 = t(17),
  big1 = t(?BIG1),
  big2 = t(?BIG2),
  atom = t(foo),
  pid = t(self()),
  float = t(4.2),
  ok.

t(V) ->
  S = self(),
  case V of
    42 -> small1;
    17 -> small2;
    ?BIG1 -> big1;
    ?BIG2 -> big2;
    1 -> no;
    2 -> no;
    3 -> no;
    4 -> no;
    5 -> no;
    6 -> no;
    7 -> no;
    8 -> no;
    foo -> atom;
    9 -> no;
    4.2 -> float;
    S -> pid;
    _ -> no
  end.

%%---------------------------------------------------------------------
