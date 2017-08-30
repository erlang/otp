-module(maps_guard_update).
-export([test/0]).

test() ->
    error  = map_guard_update(#{},#{}),
    first  = map_guard_update(#{}, #{x=>first}),
    second = map_guard_update(#{y=>old}, #{x=>second,y=>old}),
    third  = map_guard_update(#{x=>old,y=>old}, #{x=>third,y=>old}),
    ok.

map_guard_update(M1, M2) when M1#{x=>first}  =:= M2 -> first;
map_guard_update(M1, M2) when M1#{x=>second} =:= M2 -> second;
map_guard_update(M1, M2) when M1#{x:=third}  =:= M2 -> third;
map_guard_update(_, _) -> error.
