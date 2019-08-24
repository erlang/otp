%% -*- erlang-indent-level: 2 -*-
%%-------------------------------------------------------------------------
-module(maps_has_map_fields).

-export([test/0]).

test() ->
  false = has_a_field(#{}),
  false = has_a_field(#{b => 2}),
  true  = has_a_field(#{a => 3}),
  true  = has_a_field(#{b => c, a => false}),
  
  false = has_a_b_field(#{a => true}),
  false = has_a_b_field(#{b => a}),
  true  = has_a_b_field(#{a => 1, b => 2}),
  true  = has_a_b_field(#{b => 3, a => 4}),

  false = has_binary_field(#{}),
  false = has_binary_field(#{#{} => yay}),
  true  = has_binary_field(#{<<"true">> => false}),
  
  false = has_binary_but_no_map_field(#{}),
  false = has_map_but_no_binary_field(#{}),
  false = has_binary_but_no_map_field(#{#{} => 1}),
  false = has_map_but_no_binary_field(#{<<"true">> => true}),
  true  = has_binary_but_no_map_field(#{<<"true">> => false}),
  true  = has_map_but_no_binary_field(#{#{} => 1}),
  false = has_binary_but_no_map_field(#{<<"true">> => true, #{} => 1}),
  false = has_map_but_no_binary_field(#{<<"true">> => true, #{} => 1}),
  ok.

has_a_field(#{a := _}) -> true;
has_a_field(#{}) -> false.

has_a_b_field(#{a := _, b := _}) -> true;
has_a_b_field(#{}) -> false.

has_binary_field(#{<<"true">> := _}) -> true;
has_binary_field(#{}) -> false.

has_map_but_no_binary_field(#{<<"true">> := _}) -> false;
has_map_but_no_binary_field(#{} = M) -> maps:is_key(#{}, M).

has_binary_but_no_map_field(#{<<"true">> := _} = M) ->
    not maps:is_key(#{}, M);
has_binary_but_no_map_field(#{}) -> false.
