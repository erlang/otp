%%% This is a correct callback module for the correct_behaviour.

-module(incorrect_args_callback).

-behaviour(correct_behaviour).

-export([foo/0, bar/2]).

foo() ->
    yes.

bar({'reply', _Any}, yes) -> %% Should be a tuple and a list.
    yes.
