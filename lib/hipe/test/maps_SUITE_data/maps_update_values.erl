-module(maps_update_values).
-export([test/0]).

test() ->
    V0 = id(1337),
    M0 = #{ a => 1, val => V0},
    V1 = get_val(M0),
    M1 = M0#{ val := [V0,V1], "wazzup" => 42 },
    [1337, {some_val, 1337}] = get_val(M1),

    N = 110,
    List = [{[I,1,2,3,I],{1,2,3,"wat",I}}|| I <- lists:seq(1,N)],

    {_,_,#{val2 := {1,2,3,"wat",N}, val1 := [N,1,2,3,N]}} = lists:foldl(fun
	    ({V2,V3},{Old2,Old3,Mi}) ->
		ok = check_val(Mi,Old2,Old3),
		#{ val1 := Old2, val2 := Old3 } = Mi,
		{V2,V3, Mi#{ val1 := id(V2), val2 := V1, val2 => id(V3)}}
	end, {none, none, #{val1=>none,val2=>none}},List),
    ok.

get_val(#{ "wazzup" := _, val := V}) -> V;
get_val(#{ val := V }) -> {some_val, V}.

check_val(#{val1:=V1, val2:=V2},V1,V2) -> ok.

%% Use this function to avoid compile-time evaluation of an expression.
id(I) -> I.
