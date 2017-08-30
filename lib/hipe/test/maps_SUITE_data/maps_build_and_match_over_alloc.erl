-module(maps_build_and_match_over_alloc).
-export([test/0]).

test() ->
    Ls = id([1,2,3]),
    V0 = [a|Ls],
    M0 = id(#{ "a" => V0 }),
    #{ "a" := V1 } = M0,
    V2 = id([c|Ls]),
    M2 = id(#{ "a" => V2 }),
    #{ "a" := V3 } = M2,
    {[a,1,2,3],[c,1,2,3]} = id({V1,V3}),
    ok.

%% Use this function to avoid compile-time evaluation of an expression.
id(I) -> I.
