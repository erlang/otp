-module(suppression2).

-export([a/1, b/1, c/0]).

-dialyzer({nowarn_function, [a/1, b/1, c/0]}).
-dialyzer([no_undefined_callbacks]).

-behaviour(not_a_behaviour).

-spec a(_) -> integer().

a(_) ->
    A = fun(_) ->
                B = fun(_) ->
                            x = 7
                    end,
                B = 1
        end,
    A.

-spec b(_) -> integer().

b(_) ->
    A = fun(_) ->
                1
        end,
    A = 2.

-record(r, {a = a :: integer()}).

c() ->
    #r{}.
