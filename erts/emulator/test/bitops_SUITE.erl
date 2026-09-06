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
 -export([suite/0, all/0, init_per_suite/1, end_per_suite/1]).

-include_lib("common_test/include/ct.hrl").

suite() -> [{timetrap, {seconds, 30}}].

all() -> [
  test_ctz_small_integers,
  test_ctz_zero,
  test_ctz_powers_of_two,
  test_ctz_large_integers,
  test_ctz_bignums,
  test_ctz_all_ones_bignums,
  test_ctz_boundary_cases,
  test_ctz_negative_numbers,
  test_ctz_invalid_types,
  test_ctz_in_guard,
  test_ctz_repeated_calls,

  test_popcount_small_integers,
  test_popcount_zero,
  test_popcount_powers_of_two,
  test_popcount_large_integers,
  test_popcount_bignums,
  test_popcount_all_ones_bignums,
  test_popcount_boundary_cases,
  test_popcount_negative_numbers,
  test_popcount_invalid_types,
  test_popcount_in_guard,
  test_popcount_repeated_calls,

  test_mixed_bitops,
  test_ctz_popcount_conditional_logic
].

init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.

%% ===== ctz/1 Tests =====

test_ctz_small_integers(_Config) ->
  ?assertEqual(0, erlang:ctz(1)),
  ?assertEqual(1, erlang:ctz(2)),
  ?assertEqual(0, erlang:ctz(3)),
  ?assertEqual(2, erlang:ctz(4)),
  ?assertEqual(3, erlang:ctz(8)),
  ?assertEqual(4, erlang:ctz(16)),
  ok.

test_ctz_zero(_Config) ->
  ?assertEqual(-1, erlang:ctz(0)).

test_ctz_powers_of_two(_Config) ->
  Powers = [1,2,4,8,16,32,64,128,256,512,1024],
  [begin Expected = N-1, ?assertEqual(N-1, erlang:ctz(lists:nth(N, Powers))) end || N <- lists:seq(1,length(Powers))],
  ok.

test_ctz_large_integers(_Config) ->
  ?assertEqual(32, erlang:ctz(1 bsl 32)),
  ?assertEqual(48, erlang:ctz(1 bsl 48)),
  ?assertEqual(63, erlang:ctz(1 bsl 63)),
  ok.

test_ctz_bignums(_Config) ->
  BigNum64 = 1 bsl 64,
  ?assertEqual(64, erlang:ctz(BigNum64)),
  BigNum100 = 1 bsl 100,
  ?assertEqual(100, erlang:ctz(BigNum100)),
  BigNum128 = 1 bsl 128,
  ?assertEqual(128, erlang:ctz(BigNum128)),
  % multi-limb bignum with low set bit in a lower limb
  BigMixed = (1 bsl 70) + (1 bsl 5),
  ?assertEqual(5, erlang:ctz(BigMixed)),
  % bignum with trailing zeros spanning multiple limbs
  BigSpan = (1 bsl 192) + (1 bsl 130) + (1 bsl 5),
  ?assertEqual(5, erlang:ctz(BigSpan)),
  ok.

test_ctz_all_ones_bignums(_Config) ->
  % All-ones bignums: bit 0 is always set, so ctz = 0
  AllOnes64 = (1 bsl 64) - 1,
  ?assertEqual(0, erlang:ctz(AllOnes64)),
  AllOnes100 = (1 bsl 100) - 1,
  ?assertEqual(0, erlang:ctz(AllOnes100)),
  AllOnes200 = (1 bsl 200) - 1,
  ?assertEqual(0, erlang:ctz(AllOnes200)),
  ok.

%% ===== popcount/1 Tests =====

test_popcount_small_integers(_Config) ->
  ?assertEqual(0, erlang:popcount(0)),
  ?assertEqual(1, erlang:popcount(1)),
  ?assertEqual(1, erlang:popcount(2)),
  ?assertEqual(2, erlang:popcount(3)),
  ?assertEqual(1, erlang:popcount(4)),
  ?assertEqual(4, erlang:popcount(15)),
  ?assertEqual(1, erlang:popcount(8)),
  ok.

test_popcount_zero(_Config) ->
  ?assertEqual(0, erlang:popcount(0)).

test_popcount_powers_of_two(_Config) ->
  [?assertEqual(1, erlang:popcount(1 bsl N)) || N <- lists:seq(0, 63)],
  ok.

test_popcount_large_integers(_Config) ->
  ?assertEqual(32, erlang:popcount((1 bsl 32) - 1)),
  ?assertEqual(48, erlang:popcount((1 bsl 48) - 1)),
  ok.

test_popcount_bignums(_Config) ->
  BigNum128AllBits = (1 bsl 128) - 1,
  ?assertEqual(128, erlang:popcount(BigNum128AllBits)),
  BigNum128SingleBit = 1 bsl 128,
  ?assertEqual(1, erlang:popcount(BigNum128SingleBit)),
  BigNum = (1 bsl 96) + (1 bsl 65) + (1 bsl 32) + 1,
  ?assertEqual(4, erlang:popcount(BigNum)),
  % larger multi-limb all-ones
  BigAll192 = (1 bsl 192) - 1,
  ?assertEqual(192, erlang:popcount(BigAll192)),
  % scattered bits across many limbs
  BigScattered = (1 bsl 200) + (1 bsl 150) + (1 bsl 70) + (1 bsl 1),
  ?assertEqual(4, erlang:popcount(BigScattered)),
  ok.

test_popcount_all_ones_bignums(_Config) ->
  % All-ones bignums: all bits up to N are set, so popcount = N
  AllOnes64 = (1 bsl 64) - 1,
  ?assertEqual(64, erlang:popcount(AllOnes64)),
  AllOnes100 = (1 bsl 100) - 1,
  ?assertEqual(100, erlang:popcount(AllOnes100)),
  AllOnes200 = (1 bsl 200) - 1,
  ?assertEqual(200, erlang:popcount(AllOnes200)),
  ok.

%% ===== Additional JIT-specific Tests =====

test_ctz_boundary_cases(_Config) ->
  % Test around SMALL_INT_MAX transition (56 bits on 64-bit systems)
  SmallMax = (1 bsl 56) - 1,
  ?assertEqual(0, erlang:ctz(SmallMax)),
  BeyondMax = SmallMax + 1,
  ?assertEqual(56, erlang:ctz(BeyondMax)),
  % Test near 63-bit boundary
  ?assertEqual(62, erlang:ctz(1 bsl 62)),
  ?assertEqual(63, erlang:ctz(1 bsl 63)),
  ok.

test_ctz_negative_numbers(_Config) ->
  % Negative numbers should raise badarg
  ?assertError(badarg, erlang:ctz(-1)),
  ?assertError(badarg, erlang:ctz(-8)),
  ?assertError(badarg, erlang:ctz(-100)),
  ok.

test_ctz_invalid_types(_Config) ->
  % Type errors should raise badarg
  ?assertError(badarg, erlang:ctz(3.14)),
  ?assertError(badarg, erlang:ctz(atom)),
  ?assertError(badarg, erlang:ctz([1,2,3])),
  ?assertError(badarg, erlang:ctz({})),
  ok.

test_ctz_in_guard(_Config) ->
  % Test ctz in guard context (different JIT code path)
  Result1 = if erlang:ctz(8) =:= 3 -> ok; true -> fail end,
  ?assertEqual(ok, Result1),
  Result2 = if erlang:ctz(1) =:= 0 -> ok; true -> fail end,
  ?assertEqual(ok, Result2),
  Result3 = if erlang:ctz(16) > 3 -> ok; true -> fail end,
  ?assertEqual(ok, Result3),
  ok.

test_ctz_repeated_calls(_Config) ->
  % Call multiple times to ensure JIT compilation consistency
  [?assertEqual(N-1, erlang:ctz(1 bsl N)) || N <- lists:seq(0, 63)],
  ok.

test_popcount_boundary_cases(_Config) ->
  % Test around SMALL_INT_MAX transition
  SmallMax = (1 bsl 56) - 1,
  ?assertEqual(56, erlang:popcount(SmallMax)),
  BeyondMax = SmallMax + 1,
  ?assertEqual(1, erlang:popcount(BeyondMax)),
  % Test large all-ones
  AllOnes63 = (1 bsl 63) - 1,
  ?assertEqual(63, erlang:popcount(AllOnes63)),
  ok.

test_popcount_negative_numbers(_Config) ->
  % Negative numbers should raise badarg in Erlang
  ?assertError(badarg, erlang:popcount(-1)),
  ?assertError(badarg, erlang:popcount(-7)),
  ?assertError(badarg, erlang:popcount(-128)),
  ok.

test_popcount_invalid_types(_Config) ->
  % Type errors should raise badarg
  ?assertError(badarg, erlang:popcount(3.14)),
  ?assertError(badarg, erlang:popcount(foo)),
  ?assertError(badarg, erlang:popcount(ok)),
  ?assertError(badarg, erlang:popcount([1,2,3])),
  ok.

test_popcount_in_guard(_Config) ->
  % Test popcount in guard context (different JIT code path)
  Result1 = if erlang:popcount(15) =:= 4 -> ok; true -> fail end,
  ?assertEqual(ok, Result1),
  Result2 = if erlang:popcount(1) > 0 -> ok; true -> fail end,
  ?assertEqual(ok, Result2),
  Result3 = if erlang:popcount(255) =:= 8 -> ok; true -> fail end,
  ?assertEqual(ok, Result3),
  ok.

test_popcount_repeated_calls(_Config) ->
  % Repeated calls to trigger JIT warmup
  [?assertEqual(1, erlang:popcount(1 bsl N)) || N <- lists:seq(0, 127)],
  ok.

test_mixed_bitops(_Config) ->
  % Test that ctz/popcount work correctly mixed with other operations
  X = 16,
  ?assertEqual(4, erlang:ctz(X)),
  Y = X * 2,
  ?assertEqual(5, erlang:ctz(Y)),
  Z = erlang:popcount(Y),
  ?assertEqual(1, Z),
  % More complex expression
  A = 7,
  B = erlang:popcount(A),
  ?assertEqual(3, B),
  C = erlang:ctz(A),
  ?assertEqual(0, C),
  ok.

test_ctz_popcount_conditional_logic(_Config) ->
  % Test with conditional logic and pattern matching
  TestCtz = fun(N) ->
    case erlang:ctz(N) of
      0 -> odd;
      _ -> even
    end
  end,
  ?assertEqual(odd, TestCtz(1)),
  ?assertEqual(even, TestCtz(2)),
  ?assertEqual(even, TestCtz(4)),
  TestPopcount = fun(N) ->
    PC = erlang:popcount(N),
    if PC =:= 1 -> single_bit;
       PC < 4 -> few_bits;
       true -> many_bits
    end
  end,
  ?assertEqual(single_bit, TestPopcount(1 bsl 50)),
  ?assertEqual(few_bits, TestPopcount(7)),
  ?assertEqual(many_bits, TestPopcount((1 bsl 64) - 1)),
  ok.
