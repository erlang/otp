-module(on_load_b).
-on_load(on_load/0).
-export([on_load/0,data/0]).

on_load() ->
    ?MASTER ! {?MODULE,start},
    on_load_c:data(),
    ?MASTER ! {?MODULE,done},
    ok.

data() ->
    [b|on_load_c:data()].
