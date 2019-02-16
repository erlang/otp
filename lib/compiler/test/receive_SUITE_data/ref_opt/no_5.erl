-module(no_5).
-compile([export_all,nowarn_export_all]).

?MODULE() ->
    ok.

%% Nested receives were not handled properly.

confusing_recv_mark(Pid) ->
    Ref = make_ref(),
    %% There would be a recv_mark here.
    MRef = erlang:monitor(process, Pid),
    receive
        Ref ->
            %% And a recv_set here.
            receive
                MRef -> gurka
            end;
        MRef ->
            gaffel
    end.

%% The optimization could potentially be improved to
%% handle matching of multiple refs, like this:

proper_recv_mark(Pid) ->
    %% Place the recv_mark before the creation of both refs.
    Ref = make_ref(),
    MRef = erlang:monitor(process, Pid),
    %% Place the recv_set here.
    receive
        Ref ->
            receive
                MRef -> gurka
            end;
        MRef ->
            gaffel
    end.
