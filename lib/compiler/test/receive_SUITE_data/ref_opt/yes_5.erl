-module(yes_5).
-compile(export_all).

?MODULE() ->
    ok.

do_call(Process, Label, Request, Timeout) ->
    Node = case Process of
	       {_S, N} when is_atom(N) ->
		   N;
	       _ when is_pid(Process) ->
		   node(Process)
	   end,
    try erlang:monitor(process, Process) of
	Mref ->
	    catch erlang:send(Process, {Label, {self(), Mref}, Request},
			      [noconnect]),
	    receive
		{Mref, Reply} ->
		    erlang:demonitor(Mref, [flush]),
		    {ok, Reply};
		{'DOWN', Mref, _, _, noconnection} ->
		    exit({nodedown, Node});
		{'DOWN', Mref, _, _, Reason} ->
		    exit(Reason)
	    after Timeout ->
		    erlang:demonitor(Mref, [flush]),
		    exit(timeout)
	    end
    catch
	error:_ ->
	    monitor_node(Node, true),
	    receive
		{nodedown, Node} ->
		    monitor_node(Node, false),
		    exit({nodedown, Node})
	    after 0 ->
		    Tag = make_ref(),
		    Process ! {Label, {self(), Tag}, Request},
		    ?MODULE:wait_resp(Node, Tag, Timeout)
	    end
    end.
