%%
%% File:    maps_difftype.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-04-29
%%
-module(maps_difftype).

-export([empty_mismatch/1]).

empty_mismatch(Tuple) when is_tuple(Tuple) ->
    case Tuple of #{} -> ok end.
