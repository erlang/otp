-module(unsafe_beamcode_bug).
-export([test/1]).

test(N) -> i(r(N)).

%% this function cannot be exported, or the error does not occur
i({one}) -> ok1;
i({two, _}) -> ok2;
i({three, {_,R}, _}) -> R.

r(1) -> {one};
r(2) -> {two, 2};
r(42)-> {dummy, 42};	% without this clause, no problem ... hmm
r(3) -> {three, {rec,ok3}, 2}.
