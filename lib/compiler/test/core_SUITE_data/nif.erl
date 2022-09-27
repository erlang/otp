-module(nif).

-export([init/1, start/1]).

-ifdef(WITH_ATTRIBUTE).
-nifs([start/1]).
-endif.

-ifdef(WITH_LOAD_NIF).
init(File) ->
    ok = erlang:load_nif(File, 0).
-else.
init(_File) ->
    ok.
-endif.

start(_) -> erlang:nif_error(not_loaded).
