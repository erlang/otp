-module(f_include_exp2).

-include("exp2.hrl").

-export([foo/0,
         bar/0]).


foo() ->
    #conditional{}.

-if(?enable_exp_2 == 1).
bar() ->
    until.
-else.
bar() ->
    no.
-endif.
