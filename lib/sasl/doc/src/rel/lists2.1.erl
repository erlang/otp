-module(lists2).
-vsn(1).

-export([assoc/2]).

assoc(Key, [{Key, Val} | _]) -> {ok, Val};
assoc(Key, [H | T]) -> assoc(Key, T);
assoc(Key, []) -> false.
