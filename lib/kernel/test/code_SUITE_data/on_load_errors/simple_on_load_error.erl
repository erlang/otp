-module(simple_on_load_error).
-on_load(on_load/0).

on_load() ->
    nope.
