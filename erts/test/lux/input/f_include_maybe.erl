-module(f_include_maybe).

-include("maybe.hrl").

-export([foo/0,
         bar/0]).


foo() ->
    #conditional{}.

-if(?enable_maybe == 1).
bar() ->
    until.
-else.
bar() ->
    no.
-endif.
