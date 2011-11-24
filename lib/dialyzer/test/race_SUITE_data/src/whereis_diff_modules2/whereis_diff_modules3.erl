%% This tests that the race condition detection between whereis/register
%% is robust w.r.t. having the calls in separate modules.

-module(whereis_diff_modules3).
-export([start/1]).

start(AnAtom) ->
  register(AnAtom, whereis_diff_modules4:race(AnAtom)).
