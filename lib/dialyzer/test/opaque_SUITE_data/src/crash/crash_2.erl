-module(crash_2).
-export([crash/0]).

-spec crash() -> {tuple(), integer()}.
crash() ->
    {tuple(), queue:new()}.

-spec tuple() -> tuple().
tuple() ->
    ext:ernal().
