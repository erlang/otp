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
is_sorted(Sorted) ->
    try
        lists:foldl(fun chk_sorted_pair/2, hd(Sorted), tl(Sorted))
    of
        _ ->
            true
    catch
        throw:false ->
            false
    end.

chk_sorted_pair(A, B) when A>=B -> A;
chk_sorted_pair(_, _) -> throw(false).
