%% This tests that the race condition detection between whereis/register
%% is robust w.r.t. having the calls in separate functions.

-module(whereis_diff_functions1).
-export([start/2]).

continue(Fun) ->
  case whereis(master) of
    undefined ->
      register(master, spawn(Fun));
    _ -> ok
  end.

start(AnAtom, Fun) ->
  case whereis(AnAtom) of
    undefined ->
      Pid = spawn(Fun),
      register(AnAtom, Pid);
    _ ->
      ok
  end,
  continue(Fun).
