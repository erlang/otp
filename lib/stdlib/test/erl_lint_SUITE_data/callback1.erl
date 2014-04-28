-module(callback1).

-callback b1(I :: integer()) -> atom().
-callback b2(A :: atom()) -> integer().

-optional_callbacks([{b2,1}]).
