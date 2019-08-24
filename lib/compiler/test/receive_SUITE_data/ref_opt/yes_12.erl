-module(yes_12).
-compile(export_all).

?MODULE() ->
    ok.

f() ->
    {_,Ref} = spawn_monitor(fun() -> ok end),
    receive
	{'DOWN',Ref,_,_,Reason} ->
	    Reason
    end.
