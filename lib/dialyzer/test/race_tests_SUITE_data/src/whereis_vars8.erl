%% This tests that warnings do appear when there is no specific
%% information about the types and the variables are bound.

-module(whereis_vars8).
-export([start/2]).

start(AnAtom, Fun) ->
  case whereis(AnAtom) of
    undefined ->
      Pid = spawn(Fun),
      OtherAtom = kostis,
      case Pid =:= self() of
        true -> ok;
        false ->
          case AnAtom =:= OtherAtom of
            true -> register(OtherAtom, Pid);
	    false -> ok
	  end
      end;
    P when is_pid(P) ->
      ok
  end.
