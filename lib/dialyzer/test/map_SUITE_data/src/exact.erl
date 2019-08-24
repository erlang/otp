-module(exact).

-export([t1/1, t2/1]).

t1(M = #{}) ->
    any_map(M),
    case M of
	#{a := _} -> error(fail);
	_ -> ok
    end.

any_map(X) ->
    X#{a => 1, a := 2}.

t2(M = #{}) ->
    has_a(M),
    case M of
	#{a := _} -> error(ok);
	_ -> unreachable
    end.

has_a(M) ->
    M#{a := 1, a => 2}.
