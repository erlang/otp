%% This tests that the race condition detection between whereis/register
%% is robust w.r.t. having the calls in separate modules.

-module(whereis_diff_modules4).
-export([no_race/1, race/1]).

no_race(Pid) ->
  register(master, Pid).

race(Atom) ->
  whereis(Atom).
