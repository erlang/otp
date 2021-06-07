-module(yes_19).
-export([?MODULE/0,f/2]).

?MODULE() ->
    ok.

f(Pid, Msg) ->
    MyRef = make_ref(),
    Pid ! Msg,
    receive
	{Ref,Reply} when Ref == MyRef ->
	    Reply
    after 0 ->
            ok
    end.
