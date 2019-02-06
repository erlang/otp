-module(abs).

%% OTP-12948. erlang:abs/1 bug fix.

-export([t/0]).

t() ->
    Fs = [fun i1/0, fun i2/0, fun i3/0, fun i4/0, fun f1/0, fun erl_551/0],
    _ = [catch F() || F <- Fs],
    ok.

i1() ->
    A = int(),
    I1 = i1(A),
    true = I1 < 2,
    true = I1 < 1. % can never match

-spec i1(neg_integer()) -> non_neg_integer().

i1(A) when is_integer(A), A < 0 ->
    abs(A).

i2() ->
    A = int(),
    I2 = i2(A),
    true = I2 < 1,
    true = I2 < 0. % can never match

-spec i2(non_neg_integer()) -> non_neg_integer().

i2(A) when is_integer(A), A >= 0 ->
    abs(A).

i3() ->
    A = int(),
    I3 = i3(A),
    true = I3 < -1,
    true = I3 < 0. % can never match

-spec i3(integer()) -> non_neg_integer().

i3(A) when is_integer(A) ->
    abs(A).

i4() ->
    A = int(),
    I4 = i4(A),
    true = I4 =:= 0 orelse I4 =:= 1,
    true = I4 < 0 orelse I4 > 1. % can never match

-spec i4(integer()) -> number().

i4(A) when A =:= -1; A =:= 0; A =:= 1 ->
    abs(A).

f1() ->
    F1 = f1(float()),
    math:sqrt(F1).

f1(A) ->
    abs(A).

erl_551() ->
    accept(9),
    accept(-3).

accept(Number) when abs(Number) >= 8 -> first;
accept(_Number) -> second.

-spec int() -> integer().

int() ->
    foo:bar().

-spec float() -> float().

float() ->
    math:sqrt(1.0).
