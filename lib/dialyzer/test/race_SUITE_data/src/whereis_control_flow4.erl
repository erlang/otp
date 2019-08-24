%% This tests the presence of possible races due to a whereis/register
%% combination. It takes into account control flow that might exist.

-module(whereis_control_flow4).
-export([start/1]).

start(Fun) ->
  case whereis(maria) of
    undefined ->
      Pid1 = spawn(Fun),
      case Pid1 =:= self() of
        true ->
          case whereis(kostis) of
            undefined ->
              Pid2 = spawn(Fun),
              case Pid2 =:= self() of
                true ->
                  register(maria, Pid1),
                  register(kostis, Pid2);
                false -> ok
              end;
            P when is_pid(P) ->
              ok
          end;
        false -> ok
      end;
    P when is_pid(P) ->
      ok
  end.
