-module(map_in_guard2).

-export([test/0]).

test() ->
    false = assoc_guard(not_a_map),
    {'EXIT', {{badmap, not_a_map}, [{?MODULE, assoc_update, 1, _}|_]}}
	= (catch assoc_update(not_a_map)),
    {'EXIT', {function_clause, [{?MODULE, assoc_guard_clause, _, _}|_]}}
	= (catch assoc_guard_clause(not_a_map)),
    {'EXIT', {function_clause, [{?MODULE, assoc_guard_clause, _, _}|_]}}
	= (catch (begin true = exact_guard(#{}) end)),
    {'EXIT', {function_clause, [{?MODULE, exact_guard_clause, _, _}|_]}}
	= (catch exact_guard_clause(#{})),
    ok.

assoc_guard(M) when is_map(M#{a => b}) -> true;
assoc_guard(_) -> false.

assoc_update(M) -> M#{a => true}.

assoc_guard_clause(M) when is_map(M#{a => 3}) -> ok.

exact_guard(M) when (false =/= M#{a := b}) -> true;
exact_guard(_) -> false.

exact_guard_clause(M) when (false =/= M#{a := b}) -> ok.
