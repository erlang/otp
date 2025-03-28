-module(ping).
-export([ping/1]).

ping(Pid) ->
    Pid ! {pong, self()},
    ok.
