-module(no_unused_fun).
-export([main/2]).

main(X, Bool) ->
  case Bool of
    true ->
      F = fun foo/1;
    false ->
      F = fun foobar/1
  end,
  calc(X, F).

calc(X, Fun) ->
  Fun(X).

foo(A) ->
  A+42.

foobar(A) ->
  A-42.
