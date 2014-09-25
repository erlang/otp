-module(request1).

-export([a/0]).

-dialyzer(unmatched_returns).

a() ->
    b(),
    1.

b() ->
    {a, b}.
