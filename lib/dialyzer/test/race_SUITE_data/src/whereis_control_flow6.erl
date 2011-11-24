%% This tests the presence of possible races due to a whereis/unregister
%% combination. It takes into account control flow that might exist.

-module(whereis_control_flow6).
-export([start/0]).

start() ->
  case whereis(kostis) of
    undefined -> ok;
    P when is_pid(P) ->
      unregister(kostis)
  end.
