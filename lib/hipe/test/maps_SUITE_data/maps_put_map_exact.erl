%% -*- erlang-indent-level: 2 -*-
%%-------------------------------------------------------------------------
-module(maps_put_map_exact).

-export([test/0]).

test() ->
  false = exact_guard(#{b => a}),
  false = exact_guard(not_a_map),
  true  = exact_guard(#{a => false}),
  #{a := true} = exact_update(#{a => false}),
  {'EXIT', {{badmap, not_a_map}, [{?MODULE, exact_update, 1, _}|_]}}
    = (catch exact_update(not_a_map)),
  {'EXIT', {{badkey, a}, [{?MODULE, exact_update, 1, _}|_]}}
    = (catch exact_update(#{})),
  ok = exact_guard_clause(#{a => yes}),
  {'EXIT', {function_clause, [{?MODULE, exact_guard_clause, _, _}|_]}}
    = (catch exact_guard_clause(#{})),
  {'EXIT', {function_clause, [{?MODULE, exact_guard_clause, _, _}|_]}}
    = (catch exact_guard_clause(not_a_map)),
  ok.

exact_guard(M) when is_map(M#{a := b}) -> true;
exact_guard(_) -> false.

exact_update(M) -> M#{a := true}.

exact_guard_clause(M) when is_map(M#{a := 42}) -> ok.
