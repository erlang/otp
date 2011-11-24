%% This tests that the race condition detection between whereis/register
%% is robust even when the functions are called with different atoms
%% as arguments.

-module(whereis_diff_atoms_race).
-export([test/0]). %, race/1, no_race/1]).

test() ->
  Fun = fun () -> foo end,
  {race(maria, Fun), no_race(maria, Fun)}.

race(AnAtom, Fun) ->
  %AnAtom = maria,
  case whereis(AnAtom) of
    undefined ->
      Pid = spawn(Fun),
      aux(AnAtom, Pid);
    P when is_pid(P) ->
      ok
  end.

no_race(AnAtom, Fun) ->
  %AnAtom = maria,
  case whereis(AnAtom) of
    undefined ->
      Pid = spawn(Fun),
      AnotherAtom = kostis,
      aux(AnotherAtom, Pid);
    P when is_pid(P) ->
      ok
  end.

aux(Atom, Pid) ->
  register(Atom, Pid).
