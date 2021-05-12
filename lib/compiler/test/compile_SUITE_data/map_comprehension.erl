-module(map_comprehension).
-export([main/0]).

main() ->
    [
     map_comprehension_map_generator(#{1 => 2, 3 => 4}),
     map_comprehension_map_generator_filter(#{1 => 2, 3 => 4}),
     map_comprehension_list_generator([1, 2, 3, 4]),
     list_comprehension_map_generator(#{1 => 2, 3 => 4}),
     map_comprehension_binary_generator(<<1, 2, 3, 4>>),
     binary_comprehension_map_generator(#{1 => 2, 3 => 4}),
     map_comprehension_map_generator_real_pattern(
       #{ {1, 1} => 2,
          {2, 2} => 1,
          {1, 2} => 2,
          {2, 3} => 1,
          {3, 3} => 2
        }),
     map_comprehension_map_generator_kv_exprs(#{1 => 2, 3 => 4}),
     map_comprehension_nested_map_generator(
       #{
         a => #{1 => 2, 3 => 4},
         b => #{5 => 6, 7 => 8}
        })
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

map_comprehension_map_generator_real_pattern(Map) ->
    #{K => V || {K, K} := 2 = V <- Map}.

map_comprehension_map_generator_kv_exprs(Map) ->
    #{K + 1 => {V + 2, ok} || K := V <- Map}.

map_comprehension_nested_map_generator(Map) ->
    #{{K0, K} => V || K0 := V0 <- Map, K := V <- V0}.
