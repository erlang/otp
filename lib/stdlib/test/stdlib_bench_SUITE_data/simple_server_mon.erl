-module(simple_server_mon).

%% Local process. No timer. Monitor.

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
        {reply, P, Mref, M} ->
            P ! {ok, Mref, M},
            loop(S);
        {stop, P} ->
            P ! ok
    end.

reply(P, M) ->
    Mref = erlang:monitor(process, P),
    P ! {reply, self(), Mref, M},
    receive
        {ok, Mref, M} ->
            erlang:demonitor(Mref, [flush]),
            ok
    end.
