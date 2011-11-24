%% This tests that the race condition detection between whereis/register
%% is robust w.r.t. having multiple calls in separate functions.

-module(whereis_diff_functions2_twice).
-export([start/2]).

start(AnAtom, Fun) ->
  case whereis(AnAtom) of
    undefined ->
      Pid1 = spawn(Fun),
      race(AnAtom, Pid1),
      case whereis(AnAtom) of
        undefined ->
          Pid2 = spawn(Fun),
          race_again(AnAtom, Pid2);
        P when is_pid(P) ->
        true
      end;
    P when is_pid(P) ->
      true
  end.

race(Atom, Pid) ->
  register(Atom, Pid).

race_again(Atom, Pid) ->
  register(Atom, Pid).
