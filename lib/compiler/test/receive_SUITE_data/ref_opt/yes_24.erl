-module(yes_24).
-export([?MODULE/0,id/1]).

?MODULE() ->
    coverage().

coverage() ->
    RecvRef = make_ref(),
    self() ! {RecvRef, ok},
    case id(<<>>) of
        %% Cover #b_switch{}
        0 -> ok;
        1 -> ok;
        2 -> ok;
        3 -> ok;
        4 -> ok;
        5 -> ok;
        7 -> ok;
        8 -> ok;
        9 -> ok;

        %% Cover `bs_extract` hoisting
        <<BsExtract, _/bits>> ->
            BsExtract;

        <<>> ->
            receive
                {RecvRef, Msg} -> Msg
            end
    end.

id(I) -> I.

