-module(singletondoc).

-export([main/0, foo/1]).

-doc "
Doc test module
".
main() ->
    ok.

-doc "
foo(ok)

Tests multi-clauses
".
foo(X) when is_atom(X) ->
    X;
foo(_) ->
    ok.
