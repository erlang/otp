-module(hello_server).

-export([start_server/0, stop/1, loop/0]).

start_server() ->
    erlang:spawn_link(?MODULE, loop, []).

stop(Pid) ->
    Pid ! stop.

loop() ->
    receive
        stop ->
            ok;
        update ->
            ?MODULE:loop();
        Sender ->
            Sender ! hej,
            loop()
    end.
