-module(yes_2).
-compile(export_all).

?MODULE() ->
    ok.

f(Pid, Msg) ->
    Ref = make_ref(),
    Pid ! Msg,
    receive
	{Ref,Reply} ->
	    Reply
    end.
