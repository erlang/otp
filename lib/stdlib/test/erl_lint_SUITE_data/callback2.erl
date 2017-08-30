-module(callback2).

-callback b1(I :: integer()) -> atom().
-callback b2(A :: atom()) -> integer().

-optional_callbacks([b1/1, b2/1]).
