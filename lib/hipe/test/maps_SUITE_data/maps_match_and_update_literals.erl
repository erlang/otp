-module(maps_match_and_update_literals).
-export([test/0]).

test() ->
    Map = #{x=>0,y=>"untouched",z=>"also untouched",q=>1},
    #{x:=16,q:=21,y:="untouched",z:="also untouched"} = loop_match_and_update_literals_x_q(Map, [
	    {1,2},{3,4},{5,6},{7,8}
	]),
    M0 = id(#{ "hi" => "hello", int => 3, <<"key">> => <<"value">>,
	    4 => number, 18446744073709551629 => wat}),
    M1 = id(#{}),
    M2 = M1#{ "hi" => "hello", int => 3, <<"key">> => <<"value">>,
	4 => number, 18446744073709551629 => wat},
    M0 = M2,

    #{ 4 := another_number, int := 3 } = M2#{ 4 => another_number },
    ok.

loop_match_and_update_literals_x_q(Map, []) -> Map;
loop_match_and_update_literals_x_q(#{q:=Q0,x:=X0} = Map, [{X,Q}|Vs]) ->
    loop_match_and_update_literals_x_q(Map#{q=>Q0+Q,x=>X0+X},Vs).

%% Use this function to avoid compile-time evaluation of an expression.
id(I) -> I.
