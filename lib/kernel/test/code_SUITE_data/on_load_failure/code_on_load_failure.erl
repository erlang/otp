-module(code_on_load_failure).

-on_load(nif_load/0).

nif_load() ->
    exit(load_failure).
