%% This tests that the race condition detection between whereis/register
%% is robust w.r.t. having the calls in separate modules.

-module(whereis_diff_modules5).
-export([start/2]).

start(AnAtom, Fun) ->
  Pid1 = spawn(Fun),
  whereis_diff_modules6:no_race(Pid1),
  case whereis(AnAtom) of
    undefined ->
      Pid2 = spawn(Fun),
      whereis_diff_modules6:race(AnAtom, Pid2),
      case whereis(AnAtom) of
        undefined ->
          Pid3 = spawn(Fun),
          whereis_diff_modules6:race(AnAtom, Pid3);
        P when is_pid(P) ->
          true
      end;
    P when is_pid(P) ->
      true
  end.
