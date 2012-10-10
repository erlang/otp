-module(yes_11).
-compile(export_all).

?MODULE() ->
    ok.

%% Artifical example to cover more code in beam_receive.
do_call(Process, Request) ->
    Mref = erlang:monitor(process, Process),
    Process ! Request,
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
