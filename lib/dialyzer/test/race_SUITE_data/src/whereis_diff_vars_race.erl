%% This tests that the race condition detection between whereis/register
%% is robust even when the functions are called with different variables
%% as arguments.

-module(whereis_diff_vars_race).
-export([test/2]).

test(AnAtom, AnotherAtom) ->
  Fun = fun () -> foo end,
  {aux(AnAtom, AnotherAtom, Fun), aux(AnotherAtom, AnAtom, Fun)}.

aux(Atom1, Atom2, Fun) ->
  case whereis(Atom1) of
    undefined ->
      Pid = spawn(Fun),
      register(Atom2, Pid);
    P when is_pid(P) ->
      ok
  end.
