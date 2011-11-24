%% This tests that the race condition detection between whereis/register
%% is robust even when the functions are called with different atoms
%% as arguments.

-module(whereis_diff_atoms_no_race).
-export([test/0]).

test() ->
  Fun = fun () -> foo end,
  {no_race(maria, Fun)}.

no_race(AnAtom, Fun) ->
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
