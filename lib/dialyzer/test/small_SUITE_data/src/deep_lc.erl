-module(deep_lc).

-export([t/0]).

%% This is compile/test/lc_SUITE:deeply_nested/1
%%
%% Used to be _very_ slow. Unknown how slow, but more than 15 hours.

t() ->
    [[X1,X2,X3,X4,X5,X6,X7(),X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18(),X19,X20] ||
        X1 <- [99],X2 <- [98],X3 <- [97],X4 <- [96],X5 <- [42],X6 <- [17],
	X7 <- [fun() -> X5*X5 end],X8 <- [12],X9 <- [11],X10 <- [10],
        X11 <- [9],X12 <- [8],X13 <- [7],X14 <- [6],X15 <- [5],
	X16 <- [4],X17 <- [3],X18 <- [fun() -> X16+X17 end],X19 <- [2],X20 <- [1]].
