%% This tests that the race condition detection between whereis/unregister
%% is robust w.r.t. having the calls in separate functions.

-module(whereis_diff_functions3_nested).
-export([test/1]).

test(AnAtom) ->
  start(AnAtom).

start(AnAtom) ->
  case whereis(AnAtom) of
    undefined -> true;
    P when is_pid(P) ->
      race1(AnAtom)
  end.

race1(Atom) ->
  race2(Atom).

race2(Atom) ->
  unregister(Atom).
