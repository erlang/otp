-module(binary_redef).

-export([t/0]).

-export_type([nonempty_binary/0, nonempty_bitstring/0]).

-type nonempty_binary() :: integer().
-type nonempty_bitstring() :: integer().

t() ->
    I = new(),
    t1(I).

-spec t1(nonempty_bitstring()) -> nonempty_bitstring().

t1(A) ->
    A + A.

-spec new() -> nonempty_binary().

new() ->
    3.
