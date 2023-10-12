-module(io_rows).

-export([main/1]).

main(_) ->
    io:format("~p",[io:rows()]).
