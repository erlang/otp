%% This tests that the race condition detection between whereis/register
%% is robust even when the functions are called with different variables
%% as arguments.

-module(whereis_diff_vars_no_race).
-export([test/3]).

test(AnAtom, AnotherAtom, Pid) ->
  {aux(AnAtom, Pid), aux(AnotherAtom, Pid)}.

aux(Atom, Pid) ->
  register(Atom, Pid),
  whereis(Atom).
