-module(m).
-compile(export_all).

start(NProcs) ->
    Modules = [],
    Pids = [spawn_link(fun() ->
			       Cs = [M:bar() || M <- Modules],
			       receive stop -> Cs end
		       end) ||
	       _ <- lists:seq(1,NProcs)],
    {Modules,Pids}.
