-module(b).
-export([start/0, loop/0]).

start() ->
    spawn(?MODULE, loop, []).

loop() ->
    receive
	{ping, Ping} ->
	    a:pong(Ping),
	    loop();
	stop ->
	    done
    end.
