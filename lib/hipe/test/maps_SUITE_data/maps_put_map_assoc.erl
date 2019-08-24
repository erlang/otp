%% -*- erlang-indent-level: 2 -*-
%%-------------------------------------------------------------------------
-module(maps_put_map_assoc).

-export([test/0]).

test() ->
  true  = assoc_guard(#{}),
  false = assoc_guard(not_a_map),
  #{a := true} = assoc_update(#{}),
  {'EXIT', {{badmap, not_a_map}, [{?MODULE, assoc_update, 1, _}|_]}}
    = (catch assoc_update(not_a_map)),
  ok = assoc_guard_clause(#{}),
  {'EXIT', {function_clause, [{?MODULE, assoc_guard_clause, _, _}|_]}}
    = (catch assoc_guard_clause(not_a_map)),
  ok.

assoc_guard(M) when is_map(M#{a => b}) -> true;
assoc_guard(_) -> false.

assoc_update(M) -> M#{a => true}.

assoc_guard_clause(M) when is_map(M#{a => 3}) -> ok.
