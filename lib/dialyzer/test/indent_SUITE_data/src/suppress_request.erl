-module(suppress_request).

-export([test1/1, test1_b/1, test2/0, test2_b/0,
         test3/0, test3_b/0, test4/0, test4_b/0]).

-dialyzer({[specdiffs], test1/1}).
-spec test1(a | b) -> ok. % spec is subtype
test1(A) ->
    ok = test1_1(A).

-spec test1_b(a | b) -> ok. % spec is subtype (suppressed by default)
test1_b(A) ->
    ok = test1_1(A).

-spec test1_1(a | b | c) -> ok.
test1_1(_) ->
    ok.

-dialyzer(unmatched_returns).
test2() ->
    tuple(), % unmatched
    ok.

test2_b() ->
    tuple(), % unmatched
    ok.

-dialyzer({[no_return, no_match], [test3/0]}).
test3() -> % no local return (suppressed)
    A = fun(_) ->
                1
        end,
    A = 2. % can never succeed (suppressed)

test3_b() -> % no local return (requested by default)
    A = fun(_) ->
                1
        end,
    A = 2. % can never succeed (requested by default)

-dialyzer(no_improper_lists).
test4() ->
    [1 | 2]. % improper list (suppressed)

-dialyzer({no_improper_lists, test4_b/0}).
test4_b() ->
    [1 | 2]. % improper list (suppressed)

tuple() ->
    {a, b}.
