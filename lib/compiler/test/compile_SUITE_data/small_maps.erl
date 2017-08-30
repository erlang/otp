-module(small_maps).

-export([go/0,go/1]).

go() ->
    go(1337).

go(V0) ->
    M0 = #{ a => 1, val => V0},
    V1 = get_val(M0),
    M1 = M0#{ val := [V0,V1] },
    {some_val,[1337,{some_val,1337}]} = get_val(M1),
    ok.

get_val(#{ "wazzup" := _, val := V}) -> V;
get_val(#{ val := V }) -> {some_val, V}.
