%% Second arg of is_record call wasn't checked properly

-module(opaque_bug5).

-export([b/0]).

b() ->
    is_record(id({a}), id(a)).

id(I) -> I.
