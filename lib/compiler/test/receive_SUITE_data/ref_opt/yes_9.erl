-module(yes_9).
-compile(export_all).

?MODULE() ->
    ok.

f(Fun) ->
    Ref = make_ref(),
    Fun(),
    receive
	{Ref,Reply} ->
	    Reply
    end.
