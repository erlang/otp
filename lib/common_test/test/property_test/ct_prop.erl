-module(ct_prop).
-export([prop_sort/0]).

-include_lib("common_test/include/ct_property_test.hrl").

prop_sort() ->
    ?FORALL(UnSorted, list(),
            is_sorted(lists:sort(UnSorted))
           ).

is_sorted([]) ->
    true;
is_sorted([_]) ->
    true;
is_sorted([H1,H2|SortedTail]) when H1 =< H2 ->
    is_sorted([H2|SortedTail]);
is_sorted(_) ->
    false.
