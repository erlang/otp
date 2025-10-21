-module(bad_behaviour1).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{a,1,bad}];
behaviour_info(optional_callbacks) ->
    [{b,1,bad}].
