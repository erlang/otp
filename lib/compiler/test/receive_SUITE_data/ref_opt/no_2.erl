-module(no_2).
-compile(export_all).

?MODULE() ->
    ok.

f1() ->
    Ref = make_ref(),
    receive
	{'DOWN',Ref} ->
	    ok;
	{'DOWN',_} ->
	    ok
    end.

f2(Pid, Msg) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! Msg,
    receive
	{ok,Ref,Reply} ->
	    {ok,Reply};
	{error,Ref,Reply} ->
	    {error,Reply};
	{error,A,B} ->
	    {error,A,B}
    end.
