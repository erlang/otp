%% -*- erlang-indent-level: 2 -*-
%%-------------------------------------------------------------------------
-module(maps_get_map_elements).

-export([test/0]).

test() ->
  {A, B} = id({"hej", <<123>>}),
  Map = maps:from_list([{a, A}, {b, B}]),
  #{a := A, b := B} = id(Map),
  false = test_pattern(Map),
  true  = test_pattern(#{b => 1, a => "hej"}),
  case Map of
    #{a := C, b := <<124>>} -> yay;
    _ -> C = B, nay
  end,
  C = id(B),
  ok.

id(X) -> X.

test_pattern(#{a := _, b := 1}) -> true;
test_pattern(#{}) -> false.
