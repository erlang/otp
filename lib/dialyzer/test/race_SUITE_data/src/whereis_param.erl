%% This tests the presence of possible races due to a whereis/register
%% combination in higher order functions.

-module(whereis_param).
-export([start/2]).

start(AnAtom, Fun) ->
  register(AnAtom, continue(AnAtom, Fun)).

continue(AnAtom, Fun) ->
  case whereis(AnAtom) of
    undefined ->
      Pid = spawn(Fun);
    P when is_pid(P) ->
      P
  end.
