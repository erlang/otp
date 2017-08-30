-module(maps_guard_bifs).
-export([test/0]).

test() ->
    true   = map_guard_empty(),
    true   = map_guard_empty_2(),
    true   = map_guard_head(#{a=>1}),
    false  = map_guard_head([]),
    true   = map_guard_body(#{a=>1}),
    false  = map_guard_body({}),
    true   = map_guard_pattern(#{a=>1, <<"hi">> => "hi" }),
    false  = map_guard_pattern("list"),
    true   = map_guard_tautology(),
    true   = map_guard_ill_map_size(),
    ok.

map_guard_empty() when is_map(#{}); false -> true.

map_guard_empty_2() when true; #{} andalso false -> true.

map_guard_head(M) when is_map(M) -> true;
map_guard_head(_) -> false.

map_guard_body(M) -> is_map(M).

map_guard_pattern(#{}) -> true;
map_guard_pattern(_)   -> false.

map_guard_tautology() when #{} =:= #{}; true -> true.

map_guard_ill_map_size() when true; map_size(0) -> true.
