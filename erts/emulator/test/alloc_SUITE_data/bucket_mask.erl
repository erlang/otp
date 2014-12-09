-module(bucket_mask).

-export([init/1, start/1, run/1, stop/1]).

init(File) ->
    ok = erlang:load_nif(File, 0).

start(_) -> erlang:nif_error(not_loaded).
run(_)  -> erlang:nif_error(not_loaded).
stop(_) -> erlang:nif_error(not_loaded).
