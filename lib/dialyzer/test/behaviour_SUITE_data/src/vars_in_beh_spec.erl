-module(vars_in_beh_spec).

-behaviour(gen_server).

-export([code_change/3]).

-spec code_change(_, State, _) -> {ok, State}.

code_change(_, State, _) ->
    {ok, State}.
