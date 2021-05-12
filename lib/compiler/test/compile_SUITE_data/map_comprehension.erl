-module(map_comprehension).
-export([main/0]).

main() ->
    [
     map_comprehension_map_generator(#{1 => 2, 3 => 4}),
     map_comprehension_map_generator_filter(#{1 => 2, 3 => 4}),
     map_comprehension_list_generator([1, 2, 3, 4]),
     list_comprehension_map_generator(#{1 => 2, 3 => 4}),
     map_comprehension_binary_generator(<<1, 2, 3, 4>>),
     binary_comprehension_map_generator(#{1 => 2, 3 => 4})
    ].

map_comprehension_map_generator(Map) ->
   #{K => V || K := V <- Map}.

map_comprehension_map_generator_filter(Map) ->
   #{K => V || K := V <- Map, K > 1}.

map_comprehension_list_generator(List) ->
    #{K => K || K <- List}.

list_comprehension_map_generator(Map) ->
   [K + V || K := V <- Map].

map_comprehension_binary_generator(Bin) ->
    #{K => V || <<K, V>> <= Bin}.

binary_comprehension_map_generator(Map) ->
    << <<K, V>> || K := V <- Map >>.
