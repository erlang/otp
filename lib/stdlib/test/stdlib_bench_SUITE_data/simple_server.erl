-module(simple_server).

%% Local process. No timer. No monitor.

-export([start/1, reply/2, stop/1]).

start(State) ->
    spawn(fun() -> loop(State) end).

stop(P) ->
    P ! {stop, self()},
    receive
        ok ->
            ok
    end.

loop(S) ->
    receive
        {reply, P, M} ->
            P ! M,
            loop(S);
        {stop, P} ->
            P ! ok
    end.

reply(P, M) ->
    P ! {reply, self(), M},
    receive
        M ->
            M
    end.
