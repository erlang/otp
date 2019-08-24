-module(timetrap_helper).

-export([sleep/1]).

sleep(T) ->
    ct:sleep(T),
    ok.
