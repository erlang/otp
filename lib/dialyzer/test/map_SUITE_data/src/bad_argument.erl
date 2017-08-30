-module(bad_argument).

-export([t/0, t2/0, t3/0]).

t() ->
    _=(id1(#{a=>q}))#{b:=9}.

t2() ->
    _ = id2(4),
    X = id2(3),
    _ = (#{ X => q})#{3 := p},
    X.

t3() ->
    (id3(not_a_map))#{a => b}.

id1(X) -> X.
id2(X) -> X.
id3(X) -> X.
