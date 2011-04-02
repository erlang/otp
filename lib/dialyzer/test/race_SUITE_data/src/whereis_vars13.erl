%% This tests that warnings do appear when there is no specific
%% information about the types and the variables are bound.

-module(whereis_vars13).
-export([start/3]).

start(AnAtom, APid, Fun) ->
  case whereis(AnAtom) of
    undefined ->
      Pid = spawn(Fun),
      OtherAtom = kostis,
      case Pid =:= self() of
        true -> ok;
        false ->
          if
            {AnAtom, Pid} =:= {OtherAtom, APid} -> register(OtherAtom, APid);
            {AnAtom, Pid} =/= {OtherAtom, APid} -> ok
	  end
      end;
    P when is_pid(P) ->
      ok
  end.
