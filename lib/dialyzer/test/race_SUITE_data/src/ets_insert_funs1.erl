%% This tests the presence of possible races due to an ets:lookup/ets:insert
%% combination. It takes into account the anonymous functions.

-module(ets_insert_funs1).
-export([start/0]).

start() ->
  F = fun(T) ->
        ets:lookup(T, counter)
      end,
  io:format("Created ~w\n", [ets:new(foo, [named_table, public])]),
  ets:insert(foo, {counter, 0}),
  io:format("Inserted ~w\n", [{counter, 0}]),
  [{_, N}] = F(foo),
  ets:insert(foo, [{counter, N+1}]),
  io:format("Update complete\n", []),
  ObjectList = ets:lookup(foo, counter),
  io:format("Counter: ~w\n", [ObjectList]).
