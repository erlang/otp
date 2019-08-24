-module(unknown_type).

-export([t/0]).

-spec t() -> unknown:type().
t() ->
    a.
