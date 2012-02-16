-module(on_load_inline).
-export([?MODULE/0]).
-on_load(on_load/0).
-compile(inline).

?MODULE() ->
    [{pid,Pid}] = ets:lookup(on_load_executed, pid),
    exit(Pid, kill),
    ok.

on_load() ->
    Parent = self(),
    spawn(fun() ->
		  T = ets:new(on_load_executed, [named_table]),
		  ets:insert(T, {pid,self()}),
		  Parent ! done,
		  receive
		      wait_forever -> ok
		  end
	  end),
    receive
	done -> ok
    end.
