%% This tests that the race condition detection between whereis/register
%% is robust w.r.t. having the calls in separate modules.

-module(whereis_diff_modules2_nested).
-export([no_race/1, race/2]).

no_race(Pid) ->
  register(master, Pid).

race(Atom, Pid) ->
  whereis_diff_modules3_nested:race(Atom, Pid).
