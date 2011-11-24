-module(yes_3).
-compile(export_all).

?MODULE() ->
    ok.

f(Pid, Msg) ->
    Ref = make_ref(),
    do_send(Pid, Msg),
    receive
	{Ref,Reply} ->
	    Reply
    end.

do_send(Pid, Msg) ->
    Pid ! Msg.
