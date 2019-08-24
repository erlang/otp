-module(yes_7).
-compile(export_all).

?MODULE() ->
    ok.

f(X, Y) ->
    Ref = make_ref(),
    receive
	{Z,_} when X =:= Y, Ref =:= Z ->
	    ok
    end.
