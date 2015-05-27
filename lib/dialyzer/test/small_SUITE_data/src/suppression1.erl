-module(suppression1).

-export([a/1, b/1, c/0]).

-dialyzer({nowarn_function, a/1}).

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

-dialyzer({nowarn_function, b/1}).

b(_) ->
    A = fun(_) ->
                1
        end,
    A = 2.

-record(r, {a = a :: integer()}).

-dialyzer({nowarn_function, c/0}).

c() ->
    #r{}.
