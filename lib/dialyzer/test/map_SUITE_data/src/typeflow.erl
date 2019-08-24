-module(typeflow).

-export([t1/1, t2/1, t3/1, t4/1]).

t1(M = #{}) ->
    a_is_integer(M),
    case M of
	#{a := X} when is_integer(X) -> ok;
	_ -> fail
    end.

a_is_integer(#{a := X}) when is_integer(X) -> ok.

t2(M = #{}) ->
    a_is_integer(M),
    lists:sort(maps:get(a, M)),
    ok.

t3(M = #{}) ->
    lists:sort(maps:get(a, M)),
    ok.

t4(M) ->
    lists:sort(maps:get(a, M)),
    ok.
