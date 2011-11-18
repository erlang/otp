%%% This is a correct callback module for the correct_behaviour.

-module(missing_callback).

-behaviour(correct_behaviour).

-export([bar/2]).

bar({'reply', _Any}, []) ->
    yes.
