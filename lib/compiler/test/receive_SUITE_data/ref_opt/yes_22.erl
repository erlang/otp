-module(yes_22).
-compile([export_all,nowarn_export_all]).

?MODULE() ->
    ok.

nested_receives(Pid) ->
    Ref = make_ref(),
    MRef = erlang:monitor(process, Pid),
    ARef = erlang:alias(),
    receive
        Ref ->
            receive
                MRef ->
                    receive
                        ARef -> gurka
                    end
            end;
        MRef ->
            gaffel
    end.
