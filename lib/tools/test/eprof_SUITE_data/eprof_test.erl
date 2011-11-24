-module(eprof_test).
-export([go/1]).

go(N) ->
    0 = dec(N),
    ok.

dec(0) -> 0;
dec(N) -> dec(N - 1).
