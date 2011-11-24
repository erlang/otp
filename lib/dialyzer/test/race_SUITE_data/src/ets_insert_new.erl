%% This tests the presence of possible races due to an ets:lookup/ets:insert
%% combination. It takes into account multiple ets:new calls that might exist.

-module(ets_insert_new).
-export([test/0]).

test() ->
  T1 = ets:new(foo, [public]),
  T2 = ets:new(bar, []),
  ets:lookup(T2, counter),
  aux(T1),
  aux(T2).

aux(Tab) ->
  ets:insert(Tab, {counter, 1}).
