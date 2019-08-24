-module(maps_expand_map_update).
-export([test/0]).

test() ->
    M = #{<<"hello">> => <<"world">>}#{<<"hello">> := <<"les gens">>},
    #{<<"hello">> := <<"les gens">>} = M,
    ok.
