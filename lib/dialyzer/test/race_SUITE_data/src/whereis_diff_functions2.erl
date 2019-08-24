%% This tests that the race condition detection between whereis/register
%% is robust w.r.t. having the calls in separate functions.

-module(whereis_diff_functions2).
-export([test/0]).

test() ->
  start(kostis, mod:function()).

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

no_race(Pid) ->
  register(master, Pid).

race(Atom, Pid) ->
  register(Atom, Pid).
