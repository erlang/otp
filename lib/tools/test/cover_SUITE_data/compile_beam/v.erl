-module(v).
-compile({ no_auto_import, [is_integer/1] }).
-export([f/0,f/1]).

f() ->
    ok.

f(Number) when erlang:is_integer(Number) ->
	Number.
