-module(ms).
-export([t/0]).

-include_lib("stdlib/include/ms_transform.hrl").

t() ->
    MS = dbg:fun2ms(fun(All) -> message(All) end),
    erlang:trace_pattern({m, f, '_'}, MS).
