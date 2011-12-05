-module(maybe_improper).

-export([s/1]).

-spec s(maybe_improper_list(X,Y)) -> {[X], maybe_improper_list(X,Y)}.
s(A) ->
    lists:split(2,A).
