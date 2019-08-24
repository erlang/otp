-module(send).

-export([s/0]).

s() ->
    self() ! n(), % no warning
    erlang:send(self(), n()), % no warning
    ok.

n() ->
    {1, 1}.
