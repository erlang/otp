-module(maps_export).
-export([test/0]).

test() ->
    Raclette = id(#{}),
    case brie of brie -> Fromage = Raclette end,
    Raclette = Fromage#{},
    ok.

%% Use this function to avoid compile-time evaluation of an expression.
id(I) -> I.
