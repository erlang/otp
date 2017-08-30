%% Prettyprint bitstrings.

-module(pretty_bitstring).

-export([t/0]).

t() ->
    binary:copy(<<1,2,3:3>>,2).
