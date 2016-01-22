-module(cc).
-compile(export_all).

%% This is a dummy module used only for cover compiling. The content
%% of this module has no meaning for the test.

foo() ->
    T = erlang:time(),
    spawn(fun() -> bar(T) end).

bar(T) ->
    receive
	X ->
	    T1 = erlang:time(),
	    io:format("received ~p at ~p. Last time: ~p~n",[X,T1,T]),
	    bar(T1)
    end.
