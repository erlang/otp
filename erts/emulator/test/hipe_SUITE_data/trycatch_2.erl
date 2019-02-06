-module(trycatch_2).
-export([two/1]).

two(Term) ->
    Res = trycatch_3:three(Term),
    foo(),
    Res.

foo() ->
    ok.
