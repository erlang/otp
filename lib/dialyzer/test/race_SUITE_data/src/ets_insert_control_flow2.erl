%% This tests the presence of possible races due to an ets:lookup/ets:insert
%% combination. It takes into account control flow that might exist.

-module(ets_insert_control_flow2).
-export([start/0]).

start() ->
  io:format("Created ~w\n", [ets:new(foo, [named_table, public])]),
  ets:insert(foo, {random, random:uniform(150)}),
  io:format("Inserted ~w\n", [{_, N}] = ets:lookup(foo, random)),
  case (N rem 2 == 0) of
    true ->
      io:format("\nInserted an even integer\n", []),
      io:format("\nWill make it odd and generate password\n", []),
      ets:insert(foo, [{random, N+1}, {pass, generate_password(N)}]);
    false ->
      io:format("\nInserted an odd integer\n", []),
      io:format("\nWill make it even and generate password\n", []),
      ets:insert(foo, [{random, N+1}, {pass, generate_password(N)}])
  end,
  io:format("Update complete\n", []),
  ObjectList = ets:lookup(foo, pass),
  io:format("New password: ~w\n", [ObjectList]).

generate_password(N) ->
  lists:map(fun (_) -> random:uniform(90)+$\s+1 end, lists:seq(1,N)).
