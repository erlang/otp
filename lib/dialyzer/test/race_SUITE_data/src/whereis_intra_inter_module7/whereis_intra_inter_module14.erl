%% This tests that the race condition detection between whereis/register
%% is robust w.r.t. having the calls in separate functions and modules.

-module(whereis_intra_inter_module14).
-export([no_race/1, race/2, start/2]).

no_race(Pid) ->
  register(master, Pid).

race(Atom, Pid) ->
  register(Atom, Pid).

start(AnAtom, Fun) ->
  Pid1 = spawn(Fun),
  no_race(Pid1),
  case whereis(AnAtom) of
    undefined ->
      Pid2 = spawn(Fun),
      race(AnAtom, Pid2);
    P when is_pid(P) ->
      true
  end.
