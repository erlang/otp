%% This tests the presence of possible races due to an ets:lookup/ets:insert
%% combination in higher order functions.

-module(ets_insert_param).
-export([start/1]).

start(User) ->
  Table = ets:new(table, [public]),
  mod:process(Table),
  [{_, Msg}] = ets:lookup(Table, welcome_msg),
  case User of
    root ->
      ets:insert(Table, {welcome_msg, Msg ++ "root"}),
      ets:insert(Table, {pass, Pass = generate_password(ets:lookup(Table, pass))
                                      ++ generate_strong_password(ets:lookup(Table, pass))});
    user ->
      ets:insert(Table, {welcome_msg, Msg ++ "user"}),
      ets:insert(Table, {pass, Pass = generate_password(ets:lookup(Table, pass))})
  end,
  io:format("\nYour new pass is ~w\n", [Pass]).

generate_password([{_, N}]) ->
  lists:map(fun (_) -> random:uniform(90)+$\s+1 end, lists:seq(1,N)).

generate_strong_password([{_, N}]) ->
  lists:map(fun (_) -> random:uniform(90)+$\s+1 end, lists:seq(1,(N rem 2) * 5)).
