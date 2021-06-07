-module(yes_20).
-compile(export_all).

?MODULE() ->
    ok.

f() ->
    Ref = spawn_request(fun () -> ok end),
    Pid = receive
              {spawn_reply, Ref, _, P} ->
                  P
          end,
    receive
        {'DOWN', Ref, process, Pid, normal} ->
            ok
    end.
