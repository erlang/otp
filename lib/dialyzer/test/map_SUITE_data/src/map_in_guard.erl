-module(map_in_guard).

-export([test/0, raw_expr/0]).

test() ->
    false = assoc_guard(#{}),
    true  = assoc_guard(not_a_map),
    #{a := true} = assoc_update(#{}),
    {'EXIT', {{badmap, not_a_map}, [{?MODULE, assoc_update, 1, _}|_]}}
	= (catch assoc_update(not_a_map)),
    ok = assoc_guard_clause(#{}),
    {'EXIT', {function_clause, [{?MODULE, assoc_guard_clause, _, _}|_]}}
	= (catch assoc_guard_clause(not_a_map)),
    true = exact_guard(#{a=>1}),
    {'EXIT', {function_clause, [{?MODULE, assoc_guard_clause, _, _}|_]}}
    %% There's nothing we can do to find the error here, is there?
	= (catch (begin true = exact_guard(#{}) end)),
    ok = exact_guard_clause(#{a => q}),
    {'EXIT', {function_clause, [{?MODULE, exact_guard_clause, _, _}|_]}}
	= (catch exact_guard_clause(#{})),
    ok.

assoc_guard(M) when is_map(M#{a => b}) -> true;
assoc_guard(Q) -> false.

assoc_update(M) -> M#{a => true}.

assoc_guard_clause(M) when is_map(M#{a => 3}) -> ok.

exact_guard(M) when (false =/= M#{a := b}) -> true;
exact_guard(_) -> false.

exact_guard_clause(M) when (false =/= M#{a := b}) -> ok.

raw_expr() when #{}; true -> ok. %% Must not warn here!
