%% This tests the presence of possible races due to an ets:lookup/ets:insert
%% combination. It takes into account the anonymous functions.

-module(ets_insert_funs2).
-export([start/0]).

start() ->
  F = fun(T, N) ->
        ets:insert(T, [{counter, N+1}])
      end,
  io:format("Created ~w\n", [ets:new(foo, [named_table, public])]),
  ets:insert(foo, {counter, 0}),
  io:format("Inserted ~w\n", [{counter, 0}]),
  [{_, N}] = ets:lookup(foo, counter),
  F(foo, N),
  io:format("Update complete\n", []),
  ObjectList = ets:lookup(foo, counter),
  io:format("Counter: ~w\n", [ObjectList]).
