-module(m).
-compile(export_all).

start(NProcs) ->
    Modules = [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10],
    Pids = [spawn_link(fun() ->
			       Cs = [M:bar() || M <- Modules],
			       receive stop -> Cs end
		       end) ||
	       _ <- lists:seq(1,NProcs)],
    {Modules,Pids}.
