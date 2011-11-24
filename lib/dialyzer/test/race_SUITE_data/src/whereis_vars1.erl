%% This tests that no warnings appear when there is no specific
%% information about the types and the variables are not bound.

-module(whereis_vars1).
-export([start/3]).

start(AnAtom, OtherAtom, Fun) ->
  case whereis(AnAtom) of
    undefined ->
      Pid = spawn(Fun),
      case Pid =:= self() of
        true -> ok;
        false -> register(OtherAtom, Pid)
      end;
    P when is_pid(P) ->
      ok
  end.
