%%% This is a correct callback module for the correct_behaviour.

-module(undefined_beh_callback).

-behaviour(undefined_behaviour).

-export([foo/0, bar/2]).

foo() ->
    yes.

bar({'reply', _Any}, yes) ->
    yes.
