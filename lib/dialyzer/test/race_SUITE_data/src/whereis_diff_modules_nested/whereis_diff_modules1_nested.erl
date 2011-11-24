%% This tests that the race condition detection between whereis/register
%% is robust w.r.t. having the calls in separate modules.

-module(whereis_diff_modules1_nested).
-export([start/2]).

start(AnAtom, Fun) ->
  case whereis(AnAtom) of
    undefined ->
      Pid = spawn(Fun),
      whereis_diff_modules2_nested:race(AnAtom, Pid);
    P when is_pid(P) ->
      true
  end.
