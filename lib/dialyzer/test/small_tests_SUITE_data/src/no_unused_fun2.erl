-module(no_unused_fun2).
-export([main/2]).

main(X, Bool) ->
  case Bool of
    true ->
      F = fun foo/1;
    false ->
      F = fun foobar/1
  end,
  spawn(fun()->calc(X, F)end).

calc(X, Fun) ->
  Fun(X).

foo(A) ->
  A+42.

foobar(A) ->
  A-42.
