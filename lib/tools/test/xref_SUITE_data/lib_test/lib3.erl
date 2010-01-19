-module(lib3).

-export([f/0, g/0]).

-deprecated(module).

f() ->
    true.

g() ->
    true.
