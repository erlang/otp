%% ERL-1002, maps:remove

-module(maps_remove).

-export([t1/0]).

t1() ->
    A = new(),
    B = put(a, 1, A),
    C = remove(a, B),
    get(a, C).

new() ->
    maps:new().

put(K, V, M) ->
    maps:put(K, V, M).

remove(K, M) ->
    maps:remove(K, M).

get(K, M) ->
    maps:get(K, M).
