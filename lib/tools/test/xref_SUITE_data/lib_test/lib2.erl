-module(lib2).

-export([f/0, g/0]).

-deprecated({f,0,next_major_release}).

f() ->
    local().

g() ->
    true.

local() ->
    true.
