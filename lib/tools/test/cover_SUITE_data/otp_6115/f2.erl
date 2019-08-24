-module(f2).
-export([wait/0]).

wait() ->
    receive after infinity -> ok end.
