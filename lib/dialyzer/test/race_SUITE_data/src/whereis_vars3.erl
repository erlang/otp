%% This tests that warnings do appear when there is no specific
%% information about the types and the variables are bound.

-module(whereis_vars3).
-export([start/2]).

start(AnAtom, Fun) ->
  case whereis(AnAtom) of
    undefined ->
      Pid = spawn(Fun),
      {OtherAtom, APid} = {AnAtom, Pid},
      case Pid =:= self() of
        true -> ok;
        false -> register(OtherAtom, APid)
      end;
    P when is_pid(P) ->
      ok
  end.
