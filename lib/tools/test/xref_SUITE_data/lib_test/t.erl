-module(t).

-export([t/0]).

t() ->
    %% lib1: only unknown functions used
    %% lib2: one known used, one unknown function used, one local used
    %% lib3: one known function used
    lib1:unknown(),
    lib2:f(), %% known, g/0 not used
    lib2:unknown(),
    lib2:local(),
    lib3:f(),
    unknown:unknown().
