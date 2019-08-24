%% This tests the presence of possible races due to an ets:lookup/ets:insert
%% combination. It takes into account multiple ets:inserts that might exist.

-module(ets_insert_double1).
-export([start/0]).

start() ->
  io:format("Created ~w\n", [ets:new(foo, [named_table, public])]),
  ets:insert(foo, {random, random:uniform(150)}),
  io:format("Inserted ~w\n", [{_, N}] = ets:lookup(foo, random)),
  case (N rem 2 == 0) of
    true ->
      io:format("\nInserted an even integer\n", []),
      io:format("\nWill make it odd and generate new password\n", []),
      ets:insert(foo, [{random, N+1}, {pass, generate_password(N)}]);
    false ->
      io:format("\nInserted an odd integer\n", []),
      io:format("\nWill make it even and generate new password\n", []),
      ets:insert(foo, [{random, N+1}, {pass, generate_password(N)}])
  end,
  io:format("Update complete\n", []),
  ObjectList = ets:lookup(foo, pass),
  io:format("New password: ~w\n", [ObjectList]),
  ets:insert(foo, {pass, 'empty'}).

generate_password(N) ->
  [{_, P}] = ets:lookup(foo, pass),
  lists:map(fun (_) -> random:uniform(90)+P+$\s+1 end, lists:seq(1,N)).
