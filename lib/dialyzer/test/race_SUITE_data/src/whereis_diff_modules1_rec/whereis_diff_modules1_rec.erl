%% This tests the presence of possible races due to a register/whereis
%% combination in an indirectly recursive inter-modular function.

-module(whereis_diff_modules1_rec).
-export([start/4]).

start(AnAtom, NextAtom, Fun, Id) ->
  case AnAtom of
    undefined -> register(start, Id);
    _ -> register(AnAtom, Id)
  end,
  case whereis(NextAtom) of
    undefined ->
      Pid = spawn(Fun),
      case Pid =:= self() of
        true -> ok;
        false ->
          whereis_diff_modules2_rec:continue(NextAtom, mod:next(), Pid, Id)
      end;
    P when is_pid(P) ->
      ok
  end.
