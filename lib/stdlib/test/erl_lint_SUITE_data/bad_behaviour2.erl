-module(bad_behaviour2).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    undefined.
