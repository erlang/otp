%% This tests that the race condition detection between whereis/register
%% is robust w.r.t. having the calls in separate modules (forward analysis).
%% It takes into account control flow that might exist.

-module(whereis_diff_modules4_pathsens).
-export([no_race/1, race/4]).

no_race(Pid) ->
  register(master, Pid).

race(Atom, Fun, FunName, Pid) ->
  whereis_diff_modules3_pathsens:start(Atom, Fun, FunName),
  register(Atom, Pid).
