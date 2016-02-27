-module(initial_dataflow).

-export([test/0]).

test() ->
    false = assoc_guard(#{}),
    true  = assoc_guard(not_a_map),
    ok.

assoc_guard(#{}) -> true;
assoc_guard(Q) -> false.
