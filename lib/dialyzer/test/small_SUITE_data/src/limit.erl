%% Misc cases where Dialyzer would fail with system_limit or crash

-module(limit).

-export([tu/0, big/1, b2/0]).

tu() ->
    erlang:make_tuple(1 bsl 24, def, [{5,e},{1,a},{3,c}]).

big(<<Int:1152921504606846976/unit:128,0,_/binary>>) -> {5,Int}.

b2() ->
    Maxbig = maxbig(),
    _ = bnot Maxbig,
    ok.

maxbig() ->
    %% We assume that the maximum arity is (1 bsl 19) - 1.
    Ws = erlang:system_info(wordsize),
    (((1 bsl ((16777184 * (Ws div 4))-1)) - 1) bsl 1) + 1.
