-module(on_load_c).
-on_load(on_load/0).
-export([data/0]).

on_load() ->
    ?MASTER ! {?MODULE,self()},
    receive
	go ->
	    ?MASTER ! {?MODULE,done},
	    ok
    end.

data() ->
    [c].
