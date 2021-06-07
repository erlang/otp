%% This tests the presence of possible races due to a whereis/register
%% combination. It takes into account control flow that might exist.

-module(whereis_control_flow1).
-export([start/2]).

start(AnAtom, Fun) ->
  case whereis(AnAtom) of
    undefined ->
      Pid = spawn(Fun),
      case Pid =:= self() of
        true -> ok;
        false -> register(AnAtom, Pid)
      end;
    P when is_pid(P) ->
      ok
  end.
