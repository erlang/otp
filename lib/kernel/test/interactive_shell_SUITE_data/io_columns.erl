-module(io_columns).

-export([main/1]).

main(_) ->
    io:format("~p",[io:columns()]).
