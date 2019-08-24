-module(union_use).

-export([test/1, wrong_a/0, wrong_rec/0]).

test(X) ->
  case union_adt:new(X) of
    A when is_atom(A)  -> atom;
    T when is_tuple(T) -> tuple
  end.

wrong_a() ->
  aaa = union_adt:new_a(a),
  ok.

wrong_rec() ->
  is_tuple(union_adt:new_rec(42)).
