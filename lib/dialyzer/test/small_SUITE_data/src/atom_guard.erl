-module(atom_guard).
-export([test/0]).

test() ->
    foo(42).

foo(X) when is_atom(x) ->
    X.
