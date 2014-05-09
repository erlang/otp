-module(maps_warn_useless_build).
-export([test/0]).

test() ->
    [#{ a => id(I)} || I <- [1,2,3]],
    ok.

%% Use this function to avoid compile-time evaluation of an expression.
id(I) -> I.
