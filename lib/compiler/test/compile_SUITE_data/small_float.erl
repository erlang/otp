-module(small_float).
-export([f/1]).

f(F) when is_float(F) ->
    F / 2.
