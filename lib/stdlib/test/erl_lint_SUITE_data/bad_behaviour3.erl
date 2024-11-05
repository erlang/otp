-module(bad_behaviour3).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{good,1}];
behaviour_info(optional_callbacks) ->
    [{b,1,bad}].
