-module(f1).
-export([start_fail/0, start_ok/0]).

start_fail() ->
    f2:start(fun() -> 
		     io:format("this does not work\n",[]) 
	     end).

start_ok() ->
    f2:start(fun fun1/0).
fun1() ->
    io:format("this works\n",[]).
