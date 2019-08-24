-module(x__x).

-export([t/1]).

t(A) ->
    y__y:t(A),
    y__y:t(A,A).
