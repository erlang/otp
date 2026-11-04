%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(bitops_SUITE).
-export([suite/0, all/0, id/1, init_per_suite/1, end_per_suite/1,
         test_ctz_smalls/1, test_ctz_bignums/1,
         test_invalid_types/1, test_ctz_in_guard/1, test_ctz_repeated_calls/1,
         test_popcount_smalls/1, test_popcount_bignums/1,
         test_popcount_in_guard/1,
         test_popcount_repeated_calls/1, test_mixed_bitops/1,
         test_ctz_popcount_conditional_logic/1,
         test_unsigned_width_smalls/1, test_unsigned_width_bignums/1,
         test_unsigned_width_in_guard/1, test_unsigned_width_repeated_calls/1,
         test_signed_width_smalls/1, test_signed_width_bignums/1,
         test_signed_width_in_guard/1, test_signed_width_repeated_calls/1,
         test_width_relationships/1, test_width_conditional_logic/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

suite() -> [{timetrap, {seconds, 30}}].

all() ->
    [
     test_ctz_smalls,
     test_ctz_bignums,
     test_invalid_types,
     test_ctz_in_guard,
     test_ctz_repeated_calls,

     test_popcount_smalls,
     test_popcount_bignums,
     test_popcount_in_guard,
     test_popcount_repeated_calls,

     test_unsigned_width_smalls,
     test_unsigned_width_bignums,
     test_unsigned_width_in_guard,
     test_unsigned_width_repeated_calls,

     test_signed_width_smalls,
     test_signed_width_bignums,
     test_signed_width_in_guard,
     test_signed_width_repeated_calls,

     test_mixed_bitops,
     test_ctz_popcount_conditional_logic,
     test_width_relationships,
     test_width_conditional_logic
    ].

init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.

%% ===== integer_trailing_zeroes/1 Tests =====

test_ctz_smalls(_Config) ->
    0 = ctz(1),
    1 = ctz(2),
    0 = ctz(3),
    2 = ctz(4),
    3 = ctz(8),
    4 = ctz(16),

    ?assertError(badarg, integer_trailing_zeroes(id(0))),

    32 = ctz(1 bsl 32),
    48 = ctz(1 bsl 48),
    62 = ctz(1 bsl 62),
    63 = ctz(1 bsl 63),

    %% Test transitions around important powers of 2
    _ = [begin
             0 = ctz((1 bsl P) - 1),
             P = ctz(1 bsl P)
         end || P <- [27,28,29,32,48,58,59,60,63]],

    ok.

test_ctz_bignums(_Config) ->
    BigNum64 = 1 bsl 64,
    64 = ctz(BigNum64),

    BigNum100 = 1 bsl 100,
    100 = ctz(BigNum100),
    BigNum128 = 1 bsl 128,
    128 = ctz(BigNum128),
    %% multi-limb bignum with low set bit in a lower limb
    BigMixed = (1 bsl 70) + (1 bsl 5),
    5 = ctz(BigMixed),
    %% bignum with trailing zeros spanning multiple limbs
    BigSpan = (1 bsl 192) + (1 bsl 130) + (1 bsl 5),
    5 = ctz(BigSpan),

    %% All-ones bignums: bit 0 is always set, so integer_trailing_zeroes = 0
    AllOnes64 = (1 bsl 64) - 1,
    0 = ctz(AllOnes64),
    AllOnes100 = (1 bsl 100) - 1,
    0 = ctz(AllOnes100),
    AllOnes200 = (1 bsl 200) - 1,
    0 = ctz(AllOnes200),
    ok.

%% ===== integer_set_bits/1 Tests =====

test_popcount_smalls(_Config) ->
    _ = rand:uniform(),				%Seed generator
    io:format("Seed: ~p", [rand:export_seed()]),

    0 = popcnt(0),
    1 = popcnt(1),
    1 = popcnt(2),
    2 = popcnt(3),
    1 = popcnt(4),
    2 = popcnt(5),
    2 = popcnt(6),
    3 = popcnt(7),
    1 = popcnt(8),
    4 = popcnt(15),

    %% Test some random smalls
    _ = [begin
             N = rand:uniform(1 bsl 59),
             Res = popcnt(N),
             Res = count_bits(N)
         end || _ <- lists:seq(1, 10)],

    %% Negative numbers should raise badarg in Erlang
    ?assertError(badarg, integer_set_bits(id(-1))),
    ?assertError(badarg, integer_set_bits(id(-7))),
    ?assertError(badarg, integer_set_bits(id(-128))),

    %% Test transitions around important powers of 2
    _ = [begin
             P = popcnt((1 bsl P) - 1),
             1 = popcnt(1 bsl P)
         end || P <- [27,28,29,32,48,58,59,60,63]],

    ok.

test_popcount_bignums(_Config) ->
    _ = rand:uniform(),				%Seed generator
    io:format("Seed: ~p", [rand:export_seed()]),

    BigNum128AllBits = (1 bsl 128) - 1,
    128 = popcnt(BigNum128AllBits),
    BigNum128SingleBit = 1 bsl 128,
    1 = popcnt(BigNum128SingleBit),
    BigNum = (1 bsl 96) + (1 bsl 65) + (1 bsl 32) + 1,
    4 = popcnt(BigNum),

    %% Larger multi-limb all-ones
    BigAll192 = (1 bsl 192) - 1,
    192 = popcnt(BigAll192),

    %% Scattered bits across many limbs
    BigScattered = (1 bsl 200) + (1 bsl 150) + (1 bsl 70) + (1 bsl 1),
    4 = popcnt(BigScattered),

    %% All-ones bignums: all bits up to N are set, so integer_set_bits = N
    AllOnes64 = (1 bsl 64) - 1,
    64 = popcnt(AllOnes64),
    AllOnes100 = (1 bsl 100) - 1,
    100 = popcnt(AllOnes100),
    AllOnes200 = (1 bsl 200) - 1,
    200 = popcnt(AllOnes200),

    %% Test some random bignums
    _ = [begin
             N = rand:uniform(1 bsl 60) + (1 bsl 60),
             Res = popcnt(N),
             Res = count_bits(N)
         end || _ <- lists:seq(1, 10)],

    ok.

count_bits(N) ->
    count_bits(N, 0).

count_bits(0, Bits) ->
    Bits;
count_bits(N, Bits) when N > 0 ->
    count_bits(N band (N - 1), Bits + 1).

%% ===== integer_unsigned_width/1 Tests =====

test_unsigned_width_smalls(_Config) ->
    0 = uw(0),
    1 = uw(1),
    2 = uw(2),
    2 = uw(3),
    3 = uw(4),
    3 = uw(7),
    4 = uw(8),
    4 = uw(15),
    5 = uw(16),
    8 = uw(255),
    9 = uw(256),

    %% Negative numbers have no unsigned representation
    ?assertError(badarg, integer_unsigned_width(id(-1))),
    ?assertError(badarg, integer_unsigned_width(id(-8))),
    ?assertError(badarg, integer_unsigned_width(id(-100))),

    %% Test transitions around important powers of 2
    _ = [begin
             P = uw((1 bsl P) - 1),
             P = uw(1 bsl P) - 1
         end || P <- [27,28,29,32,48,58,59,60,62,63]],

    ok.

test_unsigned_width_bignums(_Config) ->
    BigNum64 = 1 bsl 64,
    65 = uw(BigNum64),
    BigNum100 = 1 bsl 100,
    101 = uw(BigNum100),
    BigNum128 = 1 bsl 128,
    129 = uw(BigNum128),

    %% Only the most significant bit decides the width; lower limbs
    %% are irrelevant
    BigMixed = (1 bsl 70) + (1 bsl 5),
    71 = uw(BigMixed),
    BigSpan = (1 bsl 192) + (1 bsl 130) + (1 bsl 5),
    193 = uw(BigSpan),

    %% All-ones bignums: N bits set means a width of N
    AllOnes64 = (1 bsl 64) - 1,
    64 = uw(AllOnes64),
    AllOnes100 = (1 bsl 100) - 1,
    100 = uw(AllOnes100),
    AllOnes200 = (1 bsl 200) - 1,
    200 = uw(AllOnes200),

    %% Negative bignums are rejected just like negative smalls
    ?assertError(badarg, integer_unsigned_width(id(-BigNum100))),
    ?assertError(badarg, integer_unsigned_width(id(-AllOnes200))),
    ok.

%% ===== integer_signed_width/1 Tests =====

test_signed_width_smalls(_Config) ->
    1 = sw(0),

    %% Positive values need one extra bit for the sign
    2 = sw(1),
    3 = sw(2),
    3 = sw(3),
    4 = sw(4),
    4 = sw(7),
    5 = sw(8),
    5 = sw(15),
    8 = sw(127),
    9 = sw(128),
    9 = sw(255),

    %% The 2's complement range is asymmetric: -(1 bsl (W-1)) still
    %% fits in W bits, while one less than that needs an extra bit
    1 = sw(-1),
    2 = sw(-2),
    3 = sw(-3),
    3 = sw(-4),
    4 = sw(-5),
    4 = sw(-7),
    4 = sw(-8),
    5 = sw(-9),
    8 = sw(-127),
    8 = sw(-128),
    9 = sw(-129),
    9 = sw(-256),
    10 = sw(-257),

    33 = sw((1 bsl 32 - 1)),
    32 = sw(-(1 bsl 31)),
    64 = sw(1 bsl 62),
    63 = sw(-(1 bsl 62)),

    %% Test transitions around important powers of 2
    _ = [begin
             P = sw((1 bsl P) - 1) - 1,
             P = sw(1 bsl P) - 2,

             P = sw(-((1 bsl P) + 1)) - 2,
             P = sw(-(1 bsl P)) - 1,
             P = sw(-((1 bsl P) - 1)) - 1
         end || P <- [27,28,29,32,48,58,59,60,62,63]],

    ok.

test_signed_width_bignums(_Config) ->
    BigNum64 = 1 bsl 64,
    66 = sw(BigNum64),
    BigNum100 = 1 bsl 100,
    102 = sw(BigNum100),
    BigNum128 = 1 bsl 128,
    130 = sw(BigNum128),

    %% Negated powers of two are the extreme value of their width, so they do
    %% not need an extra bit
    65 = sw(-BigNum64),
    101 = sw(-BigNum100),
    129 = sw(-BigNum128),

    %% One step beyond the extreme value does need an extra bit
    66 = sw(-BigNum64 - 1),
    102 = sw(-BigNum100 - 1),

    %% Multi-limb values with more than one set bit
    BigMixed = (1 bsl 70) + (1 bsl 5),
    72 = sw(BigMixed),
    72 = sw(-BigMixed),
    BigSpan = (1 bsl 192) + (1 bsl 130) + (1 bsl 5),
    194 = sw(BigSpan),
    194 = sw(-BigSpan),

    %% All-ones bignums are never powers of two, so both signs need N+1 bits
    AllOnes64 = (1 bsl 64) - 1,
    65 = sw(AllOnes64),
    65 = sw(-AllOnes64),
    AllOnes100 = (1 bsl 100) - 1,
    101 = sw(AllOnes100),
    101 = sw(-AllOnes100),
    AllOnes200 = (1 bsl 200) - 1,
    201 = sw(AllOnes200),
    201 = sw(-AllOnes200),
    ok.

test_invalid_types(_Config) ->
    %% Type errors should raise badarg
    ?assertError(badarg, integer_trailing_zeroes(id(3.14))),
    ?assertError(badarg, integer_trailing_zeroes(id(atom))),
    ?assertError(badarg, integer_trailing_zeroes(id([1,2,3]))),
    ?assertError(badarg, integer_trailing_zeroes(id({}))),

    ?assertError(badarg, integer_set_bits(id(3.14))),
    ?assertError(badarg, integer_set_bits(id(foo))),
    ?assertError(badarg, integer_set_bits(id(ok))),
    ?assertError(badarg, integer_set_bits(id([1,2,3]))),

    ?assertError(badarg, integer_unsigned_width(id(3.14))),
    ?assertError(badarg, integer_unsigned_width(id(atom))),
    ?assertError(badarg, integer_unsigned_width(id([1,2,3]))),
    ?assertError(badarg, integer_unsigned_width(id({}))),

    ?assertError(badarg, integer_signed_width(id(3.14))),
    ?assertError(badarg, integer_signed_width(id(-3.14))),
    ?assertError(badarg, integer_signed_width(id(foo))),
    ?assertError(badarg, integer_signed_width(id([1,2,3]))),
    ?assertError(badarg, integer_signed_width(id({}))),
    ok.

test_ctz_in_guard(_Config) ->
    %% Test integer_trailing_zeroes/1 in guard context (different JIT code
    %% path). id/1 cannot be called from a guard, so the input is made opaque
    %% before the guard test.
    Eight = id(8),
    One = id(1),
    Sixteen = id(16),
    ok = if integer_trailing_zeroes(Eight) =:= 3 -> ok; true -> fail end,
    ok = if integer_trailing_zeroes(One) =:= 0 -> ok; true -> fail end,
    ok = if integer_trailing_zeroes(Sixteen) > 3 -> ok; true -> fail end,
    ok.

test_popcount_in_guard(_Config) ->
    %% Test integer_set_bits in guard context (different JIT code path)
    Fifteen = id(15),
    One = id(1),
    TwoFiftyFive = id(255),
    ok = if integer_set_bits(Fifteen) =:= 4 -> ok; true -> fail end,
    ok = if integer_set_bits(One) > 0 -> ok; true -> fail end,
    ok = if integer_set_bits(TwoFiftyFive) =:= 8 -> ok; true -> fail end,
    ok.

test_ctz_repeated_calls(_Config) ->
    [N = ctz(1 bsl N) || N <- lists:seq(0, 63)],
    ok.

test_popcount_repeated_calls(_Config) ->
    [1 = popcnt(1 bsl N) || N <- lists:seq(0, 127)],
    ok.

test_unsigned_width_in_guard(_Config) ->
    %% Test integer_unsigned_width/1 in guard context (different JIT code path)
    Zero = id(0),
    Fifteen = id(15),
    Sixteen = id(16),
    Big = id(1 bsl 100),
    ok = if integer_unsigned_width(Zero) =:= 0 -> ok; true -> fail end,
    ok = if integer_unsigned_width(Fifteen) =:= 4 -> ok; true -> fail end,
    ok = if integer_unsigned_width(Sixteen) > 4 -> ok; true -> fail end,
    ok = if integer_unsigned_width(Big) =:= 101 -> ok;
            true -> fail
         end,

    Neg = id(-1),
    ok = if integer_unsigned_width(Neg) =:= 1 -> fail; true -> ok end,
    ok.

test_signed_width_in_guard(_Config) ->
    %% Test integer_signed_width/1 in guard context (different JIT code path)
    Zero = id(0),
    Pos127 = id(127),
    Neg128 = id(-128),
    Neg129 = id(-129),
    NegBig = id(-(1 bsl 100)),
    ok = if integer_signed_width(Zero) =:= 1 -> ok; true -> fail end,
    ok = if integer_signed_width(Pos127) =:= 8 -> ok; true -> fail end,
    ok = if integer_signed_width(Neg128) =:= 8 -> ok; true -> fail end,
    ok = if integer_signed_width(Neg129) > 8 -> ok; true -> fail end,
    ok = if integer_signed_width(NegBig) =:= 101 -> ok;
            true -> fail
         end,

    NotAnInt = id(foo),
    ok = if integer_signed_width(NotAnInt) =:= 1 -> fail;
            true -> ok
         end,
    ok.

test_unsigned_width_repeated_calls(_Config) ->
    [begin
         W = uw(1 bsl N),
         W = N + 1
     end || N <- lists:seq(0, 127)],
    [N = uw((1 bsl N - 1)) || N <- lists:seq(1, 127)],
    ok.

test_signed_width_repeated_calls(_Config) ->
    [begin
         W = sw(1 bsl N),
         W = N + 2
     end || N <- lists:seq(0, 127)],
    [begin
         W = sw(-(1 bsl N)),
         W = N + 1
     end || N <- lists:seq(0, 127)],
    ok.

test_mixed_bitops(_Config) ->
    %% Test that ctz/popcount work correctly mixed with other operations
    X = 16,
    4 = integer_trailing_zeroes(id(X)),
    Y = X * 2,
    5 = integer_trailing_zeroes(id(Y)),
    Z = integer_set_bits(id(Y)),
    1 = Z,

    %% More complex expression
    A = 7,
    B = integer_set_bits(id(A)),
    3 = B,
    C = integer_trailing_zeroes(id(A)),
    0 = C,
    ok.

test_ctz_popcount_conditional_logic(_Config) ->
    %% Test with conditional logic and pattern matching
    TestCtz = fun(N) ->
        case ctz(N) of
            0 -> odd;
            _ -> even
        end
    end,
    odd = TestCtz(1),
    even = TestCtz(2),
    even = TestCtz(4),
    TestPopcount = fun(N) ->
        PC = popcnt(N),
        if PC =:= 1 -> single_bit;
           PC < 4 -> few_bits;
           true -> many_bits
        end
    end,
    single_bit = TestPopcount(1 bsl 50),
    few_bits = TestPopcount(7),
    many_bits = TestPopcount((1 bsl 64) - 1),
    ok.

test_width_relationships(_Config) ->
    %% For non-negative integers the signed width is always one bit
    %% wider than the unsigned width, to make room for the sign bit
    [begin
         UW = uw(N),
         SW = sw(N),
         SW = UW + 1
     end || N <- [0, 1, 2, 3, 7, 8, 255, 1 bsl 40,
                  (1 bsl 64) - 1, 1 bsl 100]],

    %% A power of two has a single set bit, so its unsigned width is
    %% exactly one more than its number of trailing zeroes
    [begin
         1 = popcnt(P),
         UW = uw(P),
         UW = ctz(P) + 1
     end || P <- [1, 2, 4, 1 bsl 31, 1 bsl 64, 1 bsl 130]],

    %% Negated powers of two are the only negative numbers whose
    %% signed width equals the unsigned width of their magnitude
    [begin
         UW = uw(P),
         UW = sw(-P),
         Wider = sw(-P - 1),
         Wider = UW + 1
     end || P <- [1, 2, 4, 128, 1 bsl 40, 1 bsl 100]],

    %% An all-ones value of N bits needs N bits unsigned and N+1 bits
    %% signed, and has N set bits
    [begin
         AllOnes = (1 bsl N) - 1,
         N = popcnt(AllOnes),
         N = uw(AllOnes),
         SW = sw(AllOnes),
         SW = N + 1
     end || N <- [1, 8, 56, 64, 100, 200]],
    ok.

test_width_conditional_logic(_Config) ->
    %% Test with conditional logic and pattern matching
    Classify = fun(N) ->
        case uw(N) of
            0 -> zero;
            W when W =< 8 -> byte;
            W when W =< 64 -> word;
            _ -> big
        end
    end,
    zero = Classify(0),
    byte = Classify(1),
    byte = Classify(255),
    word = Classify(256),
    word = Classify((1 bsl 64) - 1),
    big = Classify(1 bsl 64),

    %% integer_signed_width/1 answers "does this fit in a signed
    %% field?"
    Fits = fun(N, Bits) -> sw(N) =< Bits end,
    true = Fits(0, 1),
    true = Fits(127, 8),
    false = Fits(128, 8),
    true = Fits(-128, 8),
    false = Fits(-129, 8),
    true = Fits(-(1 bsl 100), 101),
    false = Fits(-(1 bsl 100) - 1, 101),
    ok.

%%%
%%% Common utilities.
%%%

ctz(I0) ->
    I = id(I0),

    %% Trailing zero count is invariant under negation, so negatives
    %% give the same answer as their magnitude

    Res = integer_trailing_zeroes(I),
    Res = integer_trailing_zeroes(-I),
    Erlang = id(erlang),
    Res = Erlang:integer_trailing_zeroes(I),
    Res = Erlang:integer_trailing_zeroes(-I),
    Res.

popcnt(I0) ->
    I1 = id(I0),

    Res = integer_set_bits(I1),
    Erlang = id(erlang),
    Res = Erlang:integer_set_bits(I1),

    if
        is_integer(I1, -(1 bsl 59), (1 bsl 59) - 1) ->
            %% Test for small is unnecessary in native code.
            Res = integer_set_bits(I1),
            I = id(I1),
            if
                is_integer(I1, 0, (1 bsl 59) - 1) ->
                    %% Test for non-negative integer is unnecessary
                    Res = integer_set_bits(I);
                true ->
                    ok
            end;
        true ->
            ok
    end,

    Res.

sw(I0) ->
    I1 = id(I0),

    Res = integer_signed_width(I1),
    Erlang = id(erlang),
    Res = Erlang:integer_signed_width(I1),

    if
        is_integer(I1, -(1 bsl 59), (1 bsl 59) - 1) ->
            %% Test for small is unnecessary in native code.
            Res = integer_signed_width(I1);
        true ->
            ok
    end,

    Res.

uw(I0) ->
    I1 = id(I0),

    Res = integer_unsigned_width(I1),
    Erlang = id(erlang),
    Res = Erlang:integer_unsigned_width(I1),

    if
        is_integer(I1, 0, (1 bsl 59) - 1) ->
            %% Test for non-negative small is unnecessary in native code.
            Res = integer_unsigned_width(I1);
        true ->
            ok
    end,

    Res.

id(X) ->
    X.
