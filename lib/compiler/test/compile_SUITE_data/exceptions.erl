-module(exceptions).
-export([?MODULE/1]).

wrong_line() ->                                 %Line 4
    {ok,_} = id(error),                         %Line 5
    ok.                                         %Line 6

?MODULE(Unknown) ->
    id(Unknown),

    {'EXIT',{{badmatch,error},
             [{?MODULE,wrong_line,0,Loc}|_]}} = catch wrong_line(),
    {line,5} = lists:keyfind(line, 1, Loc),

    ok.

id(I) ->
    I.
