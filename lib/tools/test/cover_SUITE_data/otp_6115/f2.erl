-module(f2).
-export([start/1]).

start(Fun) ->
    spawn(fun() ->
		  wait(Fun)
	  end).

wait(Fun) ->
    receive
	go ->
	    Fun()
    end.
