-module(yes_25).
-compile(export_all).

?MODULE() ->
    ok.

f() ->
    {_,Ref} = spawn_monitor(name@host, fun() -> ok end),
    receive
	{'DOWN',Ref,_,_,Reason} ->
	    Reason
    end.
