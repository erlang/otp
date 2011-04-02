%% This tests that the race condition detection between whereis/register
%% is robust w.r.t. having the calls in separate modules.

-module(whereis_diff_modules2_twice).
-export([race/2, race_again/2]).

race(Atom, Pid) ->
  register(Atom, Pid).

race_again(Atom, Pid) ->
  register(Atom, Pid).
