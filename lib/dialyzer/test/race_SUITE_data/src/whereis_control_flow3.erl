%% This tests the presence of possible races due to a whereis/register
%% combination. It takes into account control flow that might exist.

-module(whereis_control_flow3).
-export([start/3]).

start(AnAtom, Fun, FunName) ->
  Pid =
    case FunName of
      master ->
        case whereis(AnAtom) of
          undefined ->
            spawn(Fun);
          P when is_pid(P) ->
            P
        end;
      slave ->
        case whereis(AnAtom) of
          undefined ->
            spawn(Fun);
          P when is_pid(P) ->
            P
        end
    end,
  register(AnAtom, Pid).
