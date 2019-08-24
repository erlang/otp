%% This tests that the race condition detection between ets:lookup/
%% ets:insert is robust even when the functions are called with
%% different atoms as arguments.

-module(ets_insert_diff_atoms_race1).
-export([test/0]).

test() ->
  ets:new(foo, [named_table, public]),
  {race(foo), no_race(foo)}.

race(Tab) ->
    [{_, N}] = ets:lookup(Tab, counter),
    aux(Tab, N).

no_race(Tab) ->
    [{_, N}] = ets:lookup(Tab, counter),
    AnotherTab = bar,
    aux(AnotherTab, N).

aux(Table, N) ->
  ets:insert(Table, [{counter, N+1}]).
