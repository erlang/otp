-module(fix).

-export([m/0]).

-spec m() -> integer().
-ifdef(error).
m() -> 3.14.
-else.
m() -> 3.
-endif.
