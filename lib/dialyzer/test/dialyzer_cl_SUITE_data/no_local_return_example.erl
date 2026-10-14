-module(no_local_return_example).
-export([g/0]).

f() ->
    erlang:error(bad).

g() ->
    f().
