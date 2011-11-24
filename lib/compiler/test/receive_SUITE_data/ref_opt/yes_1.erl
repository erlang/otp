-module(yes_1).
-compile(export_all).

?MODULE() ->
    ok.

f() ->
    Ref = make_ref(),
    receive
	{Ref,Reply} ->
	    Reply
    end.
