-module(ets_select).
-export([test/0]).

test() ->
  Table = ets:new(table, [set,{keypos,1}]),
  ets:insert(Table, {foo, bar, baz}),
  foo(Table). % ets:select(Table, [{{'_', '$1', '$2'}, [], ['$$']}]).

foo(Table) ->
  Tuples = ets:select(Table, [{{'_', '$1', '$2'}, [], ['$$']}]),
  [list_to_tuple(Tuple) || Tuple <- Tuples].
