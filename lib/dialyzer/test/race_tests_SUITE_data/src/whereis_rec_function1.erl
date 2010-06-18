%% This tests the presence of possible races due to a whereis/register
%% combination in a recursive function.

-module(whereis_rec_function1).
-export([start/2]).

start(AnAtom, Fun) ->
  case whereis(AnAtom) of
    undefined ->
      Pid = spawn(Fun),
      case Pid =:= self() of
        true -> ok;
        false ->
	  register(AnAtom, Pid),
	  start(AnAtom, Fun)
      end;
    P when is_pid(P) ->
      ok
  end.
