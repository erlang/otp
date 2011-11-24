%% This tests the presence of possible races due to an ets:lookup/ets:insert
%% combination. It takes into account control flow that might exist.

-module(ets_insert_control_flow1).
-export([start/0]).

start() ->
  io:format("Created ~w\n", [ets:new(foo, [named_table, public])]),
  ets:insert(foo, {random, random:uniform(maria:get_int())}),
  io:format("Inserted ~w\n", [{_, N}] = ets:lookup(foo, random)),
  case (N rem 2 == 0) of
    true ->
      io:format("\nInserted an even number\n", []),
      io:format("\nWill make it odd\n", []),
      ets:insert(foo, {random, N+1});
    false -> ok
  end,
  io:format("Update complete\n", []),
  ObjectList = ets:lookup(foo, random),
  io:format("Random odd integer: ~w\n", [ObjectList]).
