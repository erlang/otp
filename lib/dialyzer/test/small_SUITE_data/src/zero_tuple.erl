-module(zero_tuple).
-export([t1/0, t2/0]).

t1() ->
  {} = a(),
  ok.

t2() ->
  b = a(),
  ok.

a() -> a.
