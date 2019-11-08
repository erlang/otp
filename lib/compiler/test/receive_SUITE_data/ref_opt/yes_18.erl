-module(yes_18).
-export([?MODULE/0,f/2]).

?MODULE() ->
    ok.

f(Pid, Msg) ->
    Ref = make_ref(),
    Pid ! Msg,
    receive
	{Ref,Reply} ->
	    Reply
    after 0 ->
            ok
    end.
