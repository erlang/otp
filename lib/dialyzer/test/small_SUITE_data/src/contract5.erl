%%%-------------------------------------------------------------------
%%% File    : contract5.erl
%%% Author  : Tobias Lindahl <tobiasl@fan.it.uu.se>
%%% Description : Excercise modified record types.
%%%
%%% Created : 15 Apr 2008 by Tobias Lindahl <tobiasl@fan.it.uu.se>
%%%-------------------------------------------------------------------
-module(contract5).
-export([t/0]).

-record(bar, {baz}).

-spec t() -> #bar{baz :: boolean()}.

t() -> #bar{baz = not_a_boolean}.
