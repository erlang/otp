-module(eprof_test).
-export([go/1, do/1]).

go(N) ->
    0 = dec(N),
    ok.

dec(0) -> 0;
dec(N) -> dec(N - 1).



load(N, Pid) ->
    _ = lists:sort(lists:reverse(lists:seq(1, N))),
    Pid ! {self(), ok}.


do(N) ->
    Me  = self(),
    Pid = spawn_link(fun() -> load(N, Me) end),
    ok  = go(N),
    receive {Pid, ok} -> ok end.
