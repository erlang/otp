-module(b).
-export([start/0, loop/0, wait/0]).

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

%% This checks for a bug in expressions which have no
%% "main" clauses (only after and friends) followed by
%% a return value in the same line.
wait() ->
    receive after 1000 -> done end, ok.
