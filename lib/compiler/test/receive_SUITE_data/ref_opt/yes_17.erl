-module(yes_17).
-export([?MODULE/0,f/0]).

?MODULE() ->
    ok.

f() ->
    Ref = make_ref(),
    receive
	{Ref,Reply} ->
	    Reply
    after 0 ->
            ok
    end.
