-module(yes_13).
-compile(export_all).

?MODULE() ->
    ok.

f() ->
    {Pid,Ref} = spawn_monitor(fun() -> ok end),
    receive
	{'DOWN',Ref,process,Pid,Reason} ->
	    Reason
    end.
