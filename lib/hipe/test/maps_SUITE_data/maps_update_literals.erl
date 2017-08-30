-module(maps_update_literals).
-export([test/0]).

test() ->
    Map = #{x=>1,y=>2,z=>3,q=>4},
    #{x:="d",q:="4"} = loop_update_literals_x_q(Map, [
		{"a","1"},{"b","2"},{"c","3"},{"d","4"}
	]),
    ok.

loop_update_literals_x_q(Map, []) -> Map;
loop_update_literals_x_q(Map, [{X,Q}|Vs]) ->
    loop_update_literals_x_q(Map#{q=>Q,x=>X},Vs).
