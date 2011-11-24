-module(yes_4).
-compile(export_all).

?MODULE() ->
    ok.

f(Pid, Msg) ->
    Ref = make_ref(),
    catch do_send(Pid, Msg),
    receive
	{Ref,Reply} ->
	    Reply
    end.

do_send(Pid, Msg) ->
    Pid ! Msg.
