-module(yes_14).
-compile(export_all).

?MODULE() ->
    ok.

do_call(Process, Request) ->
    Mref = erlang:monitor(process, Process),
    Process ! Request,
    Local = case node(Process) of
                Node when Node =:= node() -> true;
                _Node -> false
            end,
    id(Local),
    receive
	{X,Y,Z} when Mref =/= X, Z =:= 42, Mref =:= Y ->
	    error;
	{X,Y,_} when Mref =/= X, Mref =:= Y ->
	    error;
	{Mref, Reply} ->
	    erlang:demonitor(Mref, [flush]),
	    {ok, Reply};
	{'DOWN', Mref, _, _, _} ->
	    error
    end.

id(I) -> I.
