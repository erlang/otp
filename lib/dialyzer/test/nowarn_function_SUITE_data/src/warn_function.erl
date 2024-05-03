-module(warn_function).

-export([a/1, b/1, c/0, d/0]).

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

d() ->
    nonexistent:foo().
