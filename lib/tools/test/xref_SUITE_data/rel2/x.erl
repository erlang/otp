-module(x).

-export([t/0, xx/0]).
-deprecated({t,0,eventually}).

t() ->
    true.

xx() ->
    x:undef().

l() ->
    l1().

l1() ->
    l().
