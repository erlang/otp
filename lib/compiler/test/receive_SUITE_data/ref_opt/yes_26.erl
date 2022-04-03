-module(yes_26).
-compile(export_all).

?MODULE() ->
    ok.

f() ->
    {_,Ref} = spawn_monitor(?MODULE, init, []),
    receive
	{'DOWN',Ref,_,_,Reason} ->
	    Reason
    end.

init() ->
    ok.
