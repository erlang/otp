%% This tests the presence of possible races due to a whereis/register
%% combination in a recursive function.

-module(whereis_rec_function8).
-export([test/2]).

test(AnAtom, Fun) ->
  start(AnAtom, Fun).

start(AnAtom, Fun) ->
  case whereis(AnAtom) of
    undefined ->
      Pid = spawn(Fun),
      case Pid =:= self() of
        true -> ok;
        false ->
	  start(AnAtom, Fun),
	  register(AnAtom, Pid)
      end;
    P when is_pid(P) ->
      ok
  end.
