%% This tests the presence of possible races due to an ets:lookup/ets:insert
%% combination. It takes into account the argument types of the calls.

-module(ets_insert_args8).
-export([test/1]).

test(Foo) ->
  ets:new(Foo, [named_table, public]),
  race(Foo).

race(Tab) ->
    [{_, N}] = ets:lookup(Tab, counter),
    aux(Tab, N).

aux(Table, N) ->
  ets:insert(Table, [{counter, N+1}]).
