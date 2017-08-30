-module(suppression3).

-export([a/1, b/1]).

-dialyzer({nowarn_function, a/1}).

-spec a(_) -> integer().

a(A) ->
    ?MODULE:missing(A).

-dialyzer({no_missing_calls, b/1}).

-spec b(_) -> integer().

b(A) ->
    ?MODULE:missing(A).
