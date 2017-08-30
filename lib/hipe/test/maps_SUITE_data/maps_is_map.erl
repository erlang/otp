%% -*- erlang-indent-level: 2 -*-
%%-------------------------------------------------------------------------
-module(maps_is_map).

-export([test/0]).

test() ->
  true  = test_is_map(#{}),
  false = test_is_map(<<"hej">>),
  true  = test_is_map_guard(#{a => b}),
  false = test_is_map_guard(3),
  true  = test_is_map_with_binary_guard(#{"a" => <<"b">>}),
  false = test_is_map_with_binary_guard(12),
  ok.

test_is_map(X) ->
  is_map(X).

test_is_map_guard(Map) when is_map(Map) -> true;
test_is_map_guard(_) -> false.

test_is_map_with_binary_guard(B) when is_binary(B) -> false;
test_is_map_with_binary_guard(#{}) -> true;
test_is_map_with_binary_guard(_) -> false.
