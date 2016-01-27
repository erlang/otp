%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%%
%%% Contains tests that manipulate and pattern match against records.
%%%-------------------------------------------------------------------
-module(basic_records).

-export([test/0]).

test() ->
  ok = test_rec1(),
  ok.

%%--------------------------------------------------------------------

-record(r, {ra}).
-record(s, {sa, sb, sc, sd}).

test_rec1() ->
  R = #r{},
  S = #s{},
  S1 = S#s{sc=R, sd=1},
  R1 = S1#s.sc,
  undefined = R1#r.ra,
  ok.

%%--------------------------------------------------------------------
