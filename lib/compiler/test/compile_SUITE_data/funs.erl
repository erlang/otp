-module(funs).
-export([go/0]).

go() ->
    Id = id(fun id/1),
    {Id(ok), Id(42)}.

id(I) -> I.
