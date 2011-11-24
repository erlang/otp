%% This tests that no warnings appear when there is no specific
%% information about the types and the variables are not bound.

-module(whereis_vars19).
-export([start/3]).

start(AnAtom, OtherAtom, Fun) ->
  case whereis(AnAtom) of
    undefined ->
      Pid = spawn(Fun),
      case Pid =:= self() of
        true -> ok;
        false ->
          case AnAtom of
            maria -> ok;
            kostis when AnAtom =/= OtherAtom ->
              register(OtherAtom, Pid);
	    _Other -> ok
	  end
      end;
    P when is_pid(P) ->
      ok
  end.
