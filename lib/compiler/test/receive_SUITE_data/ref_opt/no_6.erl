-module(no_6).
-export([?MODULE/0]).

?MODULE() ->
    ok = return_before_receive(ok),
    ok = return_before_receive(ok),
    {error, whatever} = return_before_receive(error),
    ok.

return_before_receive(Cmd)  ->
    RecvRef = make_ref(),
    case value(Cmd, RecvRef) of
        ok ->
            ok;
        {error, eagain} ->
            receive
                {abort, {RecvRef, Reason}} ->
                    {error, Reason}
            end
    end.

value(error, Ref) ->
    self() ! {abort, {Ref, whatever}},
    {error, eagain};
value(ok, _Ref) ->
    ok.
