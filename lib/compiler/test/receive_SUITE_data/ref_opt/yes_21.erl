-module(yes_21).
-compile(export_all).

?MODULE() ->
    ok.

f() ->
    Ref = spawn_request(fun () -> ok end),
    receive
        {spawn_reply, Ref, _, _} ->
            ok
    end,
    receive
        {'DOWN', Ref, _, _, _} ->
            ok
    end.
