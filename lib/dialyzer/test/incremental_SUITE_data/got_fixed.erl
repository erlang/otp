-module(got_fixed).

-export([m/0]).

-spec m() -> integer().
m() -> fix:m().
