%% This tests that the race condition detection between ets:lookup/
%% ets:insert is robust even when the functions are called with
%% different atoms as arguments.

-module(ets_insert_diff_atoms_race4).
-export([test/0]).

test() ->
  ets:new(foo, [named_table, public]),
  {race(foo, counter), no_race(foo, counter)}.

race(Tab, Counter) ->
    [{_, N}] = ets:lookup(Tab, Counter),
    aux(Tab, Counter, N).

no_race(Tab, Counter) ->
    [{_, N}] = ets:lookup(Tab, Counter),
    AnotherTab = bar,
    aux(AnotherTab, Counter, N).

aux(Table, Counter, N) ->
  ets:insert(Table, {Counter, N+1}).
