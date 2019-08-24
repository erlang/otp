%% This tests that the race condition detection between whereis/register
%% is robust w.r.t. having the calls in separate functions and modules.

-module(whereis_intra_inter_module13).
-export([start/2]).

start(AnAtom, Fun) ->
  Pid1 = spawn(Fun),
  whereis_intra_inter_module14:no_race(Pid1),
  case whereis(AnAtom) of
    undefined ->
      Pid2 = spawn(Fun),
      continue(AnAtom, Pid2);
    P when is_pid(P) ->
      true
  end.

continue(Atom, Pid) ->
  whereis_intra_inter_module14:race(Atom, Pid).
