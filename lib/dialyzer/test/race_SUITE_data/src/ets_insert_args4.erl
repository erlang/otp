%% This tests the presence of possible races due to an ets:lookup/ets:insert
%% combination. It takes into account the argument types of the calls.

-module(ets_insert_args4).
-export([start/0]).

start() ->
  F = fun(T)-> [{_, N}] = ets:lookup(T, counter),
               ets:insert(T, {counter, N+1})
      end,
  io:format("Created ~w\n", [ets:new(foo, [named_table, public])]),
  ets:insert(foo, {counter, 0}),
  io:format("Inserted ~w\n", [{counter, 0}]),
  F(foo),
  io:format("Update complete\n", []),
  ObjectList = ets:lookup(foo, counter),
  io:format("Counter: ~w\n", [ObjectList]).
