%% This tests the presence of possible races due to a register/whereis
%% combination in an indirectly recursive inter-modular function.

-module(whereis_diff_modules3_rec).
-export([test/0, start/4]).

test() ->
  start(undefined, second, mod:f(), self()).

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
          whereis_diff_modules4_rec:continue(NextAtom, mod:next(), Pid, Id)
      end;
    P when is_pid(P) ->
      ok
  end.
