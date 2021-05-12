-module(map_comprehension).
-export([main/0]).

map_comprehension_map_generator(Map) ->
    #{K => V || K := V <- Map}.

map_comprehension_list_generator(List) ->
    #{K => K || K <- List}.

list_comprehension_map_generator(Map) ->
    [K + V || K := V <- Map].

main() ->
    map_comprehension_map_generator(#{}),
    map_comprehension_list_generator([]),
    list_comprehension_map_generator(#{}).
