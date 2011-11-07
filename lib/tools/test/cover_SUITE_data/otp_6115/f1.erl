-module(f1).
-export([start_a/0, start_b/0]).

start_a() ->
    f2:start(fun() -> 
		     ok
	     end).

start_b() ->
    f2:start(fun fun1/0).

fun1() ->
    ok.
