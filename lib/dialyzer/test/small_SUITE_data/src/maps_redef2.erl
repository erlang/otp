%% In 17, the linter says that map(A) redefines 'type map', which is
%% allowed until next release. However, Dialyzer used to replace
%% map(A) with #{}, which resulted in warnings.

-module(maps_redef2).

-export([t/0]).

-type map(_A) :: integer().

t() ->
    M = new(),
    t1(M).

-spec t1(map(_)) -> map(_).

t1(A) ->
    A + A.

-spec new() -> map(_).

new() ->
    3.
