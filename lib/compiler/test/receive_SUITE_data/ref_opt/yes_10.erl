-module(yes_10).
-compile(export_all).

?MODULE() ->
    ok.

f() ->
    Ref = make_ref(),
    receive
	%% Artificial example to cover more code in beam_receive.
	{X,Y} when Ref =/= X, Ref =:= Y ->
	    ok
    end.
