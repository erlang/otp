-module(maps_from_list).

-export([main/0]).

main() ->
    maps:from_list([X || X <- [1, 2]]).
