-module(lzip).
-export([test/0, test/1]).

test() ->
  lists:zip([],[]).

test(L) ->
  lists:zip(L, []).
