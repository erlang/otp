%% This tests that the race condition detection between whereis/register
%% is robust w.r.t. having the calls in separate functions and modules.

-module(whereis_intra_inter_module3).
-export([start/2]).

start(AnAtom, Fun) ->
  Pid1 = spawn(Fun),
  whereis_intra_inter_module4:no_race(Pid1),
  case whereis(AnAtom) of
    undefined ->
      Pid2 = spawn(Fun),
      whereis_intra_inter_module4:race(AnAtom, Pid2);
    P when is_pid(P) ->
      true
  end.
