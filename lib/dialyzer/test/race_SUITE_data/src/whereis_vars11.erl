%% This tests that no warnings appear when there is no specific
%% information about the types and the variables are not bound.

-module(whereis_vars11).
-export([start/2]).

start(AnAtom, Fun) ->
  case whereis(AnAtom) of
    undefined ->
      Pid = spawn(Fun),
      OtherAtom = kostis,
      case Pid =:= self() of
        true -> ok;
        false ->
          case AnAtom of
            OtherAtom -> ok;
	    _Other -> register(OtherAtom, Pid)
	  end
      end;
    P when is_pid(P) ->
      ok
  end.
