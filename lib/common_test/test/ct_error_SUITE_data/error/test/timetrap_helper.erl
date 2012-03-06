-module(timetrap_helper).

-export([sleep/1]).

sleep(T) ->
    timer:sleep(T),
    ok.
