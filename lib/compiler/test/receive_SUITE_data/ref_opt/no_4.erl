-module(no_4).
-compile(export_all).

?MODULE() ->
    ok.

f(X) ->
    {Pid,Ref} = spawn_monitor(fun() -> ok end),
    r(Pid, Ref).

r(_, _) ->
    ok.
