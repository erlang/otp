%% This tests that the race condition detection between whereis/register
%% is robust w.r.t. having the calls in separate modules.

-module(whereis_diff_modules1).
-export([start/2]).

start(AnAtom, Fun) ->
  Pid1 = spawn(Fun),
  whereis_diff_modules2:no_race(Pid1),
  case whereis(AnAtom) of
    undefined ->
      Pid2 = spawn(Fun),
      whereis_diff_modules2:race(AnAtom, Pid2);
    P when is_pid(P) ->
      true
  end.
