%% This tests that no warnings appear when there is no specific
%% information about the types and the variables are not bound.

-module(whereis_vars20).
-export([start/2]).

start(AnAtom, Fun) ->
  case whereis(AnAtom) of
    undefined ->
      Pid = spawn(Fun),
      OtherAtom = kostis,
      case Pid =:= self() of
        true -> ok;
        false ->
          if
            AnAtom =:= OtherAtom -> ok;
            AnAtom =/= OtherAtom -> register(OtherAtom, Pid)
	  end
      end;
    P when is_pid(P) ->
      ok
  end.
