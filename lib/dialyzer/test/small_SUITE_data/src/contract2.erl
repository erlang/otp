-module(contract2).
-export([test/2]).

-spec test(list(), list()) -> ok.

test([], []) ->
  ok;
test([], L) ->
  raise(L);
test([H|T], L) ->
  case H of
    true -> test(T, L);
    false -> test(T, [H|L])
  end.

-spec raise(_) -> no_return().
raise(X) ->
  throw(X).
