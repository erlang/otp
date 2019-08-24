%% This tests that the race condition detection between whereis/register
%% is robust w.r.t. having the calls in separate functions.

-module(whereis_diff_functions3).
-export([start/1]).

start(AnAtom) ->
  register(AnAtom, race(AnAtom)).

race(Atom) ->
  whereis(Atom).
