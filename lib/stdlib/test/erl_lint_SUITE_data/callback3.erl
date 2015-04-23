-module(callback3).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{f1, 1}];
behaviour_info(_) ->
    undefined.
