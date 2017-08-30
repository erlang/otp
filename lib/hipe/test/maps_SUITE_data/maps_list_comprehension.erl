-module(maps_list_comprehension).
-export([test/0]).

test() ->
    [#{k:=1},#{k:=2},#{k:=3}] = [#{k=>I} || I <- [1,2,3]],
    ok.
