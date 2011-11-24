-module(yes_6).
-compile(export_all).

?MODULE() ->
    ok.

f(Pid, Msg) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! Msg,
    receive
	{ok,Ref,Reply} ->
	    {ok,Reply};
	{error,Ref,Reply} ->
	    {error,Reply}
    end.
