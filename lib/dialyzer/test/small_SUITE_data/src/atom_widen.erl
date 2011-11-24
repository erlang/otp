%%---------------------------------------------------------------------
%% Tests that the set widening limit is at least as big as 13,
%% which allows for the following discrepancy to be detected.
%%---------------------------------------------------------------------

-module(atom_widen).
-export([test/0, foo/1]).

test() ->
  foo(z).

foo(a) ->  1;
foo(b) ->  2;
foo(c) ->  3;
foo(d) ->  4;
foo(e) ->  5;
foo(f) ->  6;
foo(g) ->  7;
foo(h) ->  8;
foo(i) ->  9;
foo(k) -> 10;
foo(l) -> 11;
foo(m) -> 12;
foo(n) -> 13.
