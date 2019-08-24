%% This tests the presence of possible races due to an ets:lookup/ets:insert
%% combination. It takes into account control flow that might exist.

-module(ets_insert_control_flow5).
-export([start/1]).

start(User) ->
  Table = ets:new(table, [public]),
  mod:process(Table),
  [{_, N}] =
    case User of
      root -> ets:lookup(Table, pass);
      user -> ets:lookup(Table, pass);
      Other -> [{undefined, -1}]
    end,
  [{_, Msg}] = ets:lookup(Table, welcome_msg),
  case N of
    -1 -> io:format("\nUnknown User\n", []);
    0 ->
      case User of
        root ->
          ets:insert(Table, {welcome_msg, Msg ++ "root"}),
          ets:insert(Table, {pass, Pass = generate_password(N) ++ generate_password(N+1)});
        user ->
          ets:insert(Table, {welcome_msg, Msg ++ "user"}),
          ets:insert(Table, {pass, Pass = generate_password(N)})
      end,
      io:format("\nYour new pass is ~w\n", [Pass]);
    P ->
      io:format("\nYour pass is ~w\n", [P])
  end.

generate_password(N) ->
  lists:map(fun (_) -> random:uniform(90)+$\s+1 end, lists:seq(1,N)).
