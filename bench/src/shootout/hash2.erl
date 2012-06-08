%%% The Great Computer Language Shootout 
%%% http://shootout.alioth.debian.org/
%%% 
%%% improvements by James Hague 
%%% modified by Isaac Gouy
%%%
%%% Use ETS tables (Erlang's associative store).

-module(hash2).
-export([main/0, main/1]).
-export([small/0,medium/0,big/0]).

small() -> 200.
medium() -> 2000.
big() -> 8000.


main() -> main(["1"]).
main(Arg) ->
   N = Arg,
   H1 = ets:new(one, [set]),
   H2 = ets:new(two, [set]),
   doinserts1(0, H1),
   doinserts2(N, H1, H2),
   io:format("~w ~w ~w ~w~n", [value(H1, list_to_atom("foo_1")),
      value(H1, list_to_atom("foo_9999")),
      value(H2, list_to_atom("foo_1")),
      value(H2, list_to_atom("foo_9999"))]),
   exit(ok).

doinserts1(10000, _) -> ok;
doinserts1(I, H) ->
   Key = list_to_atom(lists:append("foo_", integer_to_list(I))),
   ets:insert(H, { Key, I }),
   doinserts1(I+1, H).

doinserts2(0, _, _) -> ok;
doinserts2(I, H1, H2) ->
   addTables(H1, H2),
   doinserts2(I-1, H1, H2).

addTables(H1, H2) ->
   Key = ets:first(H1),
   addTables(Key, H1, H2).

value(Tab, Key) -> { _, V } = hd(ets:lookup(Tab, Key)), V.

addTables('$end_of_table', _, _) -> ok;
addTables(Key, H1, H2) ->
   Val1 = value(H1, Key),
   case (catch ets:update_counter(H2, Key, Val1)) of
      {'EXIT', {badarg, _}} -> ets:insert(H2, {Key, Val1});
      _                     -> true
   end,
   addTables(ets:next(H1, Key), H1, H2).
