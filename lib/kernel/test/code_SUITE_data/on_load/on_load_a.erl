-module(on_load_a).
-on_load(on_load/0).
-export([data/0]).

on_load() ->
    ?MASTER ! {?MODULE,start},
    on_load_b:data(),

    %% Call local function.
    120 = fact(5),

    %% Call remote function.
    LibDir = code:lib_dir(kernel),

    ?MASTER ! {?MODULE,LibDir},
    ok.

data() ->
    [a|on_load_b:data()].

fact(N) ->
    fact(N, 1).

fact(0, P) -> P;
fact(1, P) -> P;
fact(N, P) -> fact(N-1, P*N).

    
