-module(hipe_compiled).

-export([try_load_nif/0]).

try_load_nif() ->
    erlang:load_nif("doesn't matter", 0).
