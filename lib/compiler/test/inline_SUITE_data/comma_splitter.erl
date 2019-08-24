-module(comma_splitter).
-export([?MODULE/0]).

?MODULE() ->
    {<<"def">>,<<"cba">>} = split_at_comma(<<"abc,   def">>, <<>>),
    ok.

strip_leading_ws(<<N, Rest/binary>>) when N =< $\s ->
    strip_leading_ws(Rest);
strip_leading_ws(B) ->
    B.

split_at_comma(<<>>, Accu) ->
    {<<>>, Accu};
split_at_comma(<<$,, Rest/binary>>, Accu) ->
    {strip_leading_ws(Rest), Accu};
split_at_comma(<<C, Rest/binary>>, Accu) ->
    split_at_comma(Rest, <<C, Accu/binary>>).
