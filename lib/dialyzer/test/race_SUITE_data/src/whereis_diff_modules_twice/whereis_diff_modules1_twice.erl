%% This tests that the race condition detection between whereis/register
%% is robust w.r.t. having multiple calls in separate modules.

-module(whereis_diff_modules1_twice).
-export([start/2]).

start(AnAtom, Fun) ->
  case whereis(AnAtom) of
    undefined ->
      Pid1 = spawn(Fun),
      whereis_diff_modules2_twice:race(AnAtom, Pid1),
      case whereis(AnAtom) of
        undefined ->
          Pid2 = spawn(Fun),
          whereis_diff_modules2_twice:race_again(AnAtom, Pid2);
        P when is_pid(P) ->
        true
      end;
    P when is_pid(P) ->
      true
  end.
