%% This tests that the race condition detection between whereis/register
%% is robust w.r.t. having the calls in separate modules (backward analysis).
%% It takes into account control flow that might exist.

-module(whereis_diff_modules2_pathsens).
-export([no_race/1, race/2]).

no_race(Pid) ->
  register(master, Pid).

race(Atom, Pid) ->
  register(Atom, Pid).
