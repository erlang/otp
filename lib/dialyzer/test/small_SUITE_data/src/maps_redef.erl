-module(maps_redef).

-export([t/0]).

%% OK in Erlang/OTP 17, at least.

-type map() :: atom(). % redefine built-in type

-spec t() -> map().

t() ->
    a. % OK
