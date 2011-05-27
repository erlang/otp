%% This tests that the race condition detection between whereis/register
%% is robust w.r.t. having the calls in separate functions.
%% It takes into account control flow that might exist.

-module(whereis_diff_functions1_pathsens).
-export([test/1]).

test(FunName) ->
  start(kostis, mod:function(), FunName).

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
  race(AnAtom, Pid).

race(Atom, Pid) ->
  register(Atom, Pid).
