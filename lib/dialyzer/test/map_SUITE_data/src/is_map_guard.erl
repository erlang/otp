-module(is_map_guard).

-export([t1/0, t2/0, implicit/1, explicit/1
	]).

t1() ->
    _ = implicit(#{}),
    implicit(not_a_map).

implicit(M) ->
    M#{}.

t2() ->
    explicit(#{q=>d}),
    explicit(still_not_map).

explicit(M) when is_map(M) -> ok.
