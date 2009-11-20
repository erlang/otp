-module(lists2).
-vsn(2).

-export([assoc/2, multi_map/2]).

assoc(Key, [{Key, Val} | _]) -> {ok, Val};
assoc(Key, [H | T]) -> assoc(Key, T);
assoc(Key, []) -> false.

multi_map(Func, [[] | ListOfLists]) -> [];
multi_map(Func, ListOfLists) ->
    [apply(Func, lists:map({erlang, hd}, ListOfLists)) |
     multi_map(Func, lists:map({erlang, tl}, ListOfLists))].
