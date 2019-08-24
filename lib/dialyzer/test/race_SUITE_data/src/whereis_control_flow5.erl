%% This tests the presence of possible races due to a whereis/unregister
%% combination. It takes into account control flow that might exist.

-module(whereis_control_flow5).
-export([start/1]).

start(AnAtom) ->
  case whereis(AnAtom) of
    undefined -> ok;
    P when is_pid(P) ->
      unregister(AnAtom)
  end.
