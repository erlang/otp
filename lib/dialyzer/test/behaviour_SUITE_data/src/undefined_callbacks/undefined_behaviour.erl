%%% This is a behaviour with undefined info about its calllbacks.

-module(undefined_behaviour).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{foo, 0}, {bar, 2}];
behaviour_info(_Other) ->
    undefined.
