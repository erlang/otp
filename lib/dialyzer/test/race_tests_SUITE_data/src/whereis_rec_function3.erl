%% This tests the presence of possible races due to a register/whereis
%% combination in a recursive function.

-module(whereis_rec_function3).
-export([test/0]).

test() ->
  start(undefined, second, mod:f(), self()).

start(AnAtom, NextAtom, Fun, Id) ->
  case AnAtom of
    undefined -> register(start, Id);
    _ -> register(AnAtom, Id)
  end,
  Pid =
    case whereis(NextAtom) of
      undefined -> spawn(Fun);
      P1 when is_pid(P1) -> P1
    end,
  case whereis(NextAtom) of
    undefined ->
      case Pid =:= self() of
        true -> ok;
        false -> start(NextAtom, mod:next(), Pid, Id), io:format("", [])
      end;
    P2 when is_pid(P2) -> ok
  end.
