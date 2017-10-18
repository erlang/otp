%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2017. All Rights Reserved.
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
%% =====================================================================
%% Multiple PRNG module for Erlang/OTP
%% Copyright (c) 2015-2016 Kenji Rikitake
%%
%% exrop (xoroshiro116+) added, statistical distribution
%% improvements and uniform_real added by the Erlang/OTP team 2017
%% =====================================================================

-module(rand).

-export([seed_s/1, seed_s/2, seed/1, seed/2,
	 export_seed/0, export_seed_s/1,
         uniform/0, uniform/1, uniform_s/1, uniform_s/2,
         uniform_real/0, uniform_real_s/1,
         jump/0, jump/1,
	     normal/0, normal/2, normal_s/1, normal_s/3
	]).

%% Debug
-export([make_float/3, float2str/1, bc64/1]).

-compile({inline, [exs64_next/1, exsplus_next/1,
		   exs1024_next/1, exs1024_calc/2,
                   exrop_next/1, exrop_next_s/2,
		   get_52/1, normal_kiwi/1]}).

-define(DEFAULT_ALG_HANDLER, exrop).
-define(SEED_DICT, rand_seed).

%% =====================================================================
%% Bit fiddling macros
%% =====================================================================

-define(BIT(Bits), (1 bsl (Bits))).
-define(MASK(Bits), (?BIT(Bits) - 1)).
-define(MASK(Bits, X), ((X) band ?MASK(Bits))).
-define(
   BSL(Bits, X, N),
   %% N is evaluated 2 times
   (?MASK((Bits)-(N), (X)) bsl (N))).
-define(
   ROTL(Bits, X, N),
   %% Bits is evaluated 2 times
   %% X is evaluated 2 times
   %% N i evaluated 3 times
   (?BSL((Bits), (X), (N)) bor ((X) bsr ((Bits)-(N))))).

-define(
   BC(V, N),
   bc((V), ?BIT((N) - 1), N)).

%%-define(TWO_POW_MINUS53, (math:pow(2, -53))).
-define(TWO_POW_MINUS53, 1.11022302462515657e-16).

%% =====================================================================
%% Types
%% =====================================================================

-type uint64() :: 0..?MASK(64).
-type uint58() :: 0..?MASK(58).

%% This depends on the algorithm handler function
-type alg_state() ::
        exs64_state() | exsplus_state() | exs1024_state() |
        exrop_state() | term().

%% This is the algorithm handling definition within this module,
%% and the type to use for plugins.
%%
%% The 'type' field must be recognized by the module that implements
%% the algorithm, to interpret an exported state.
%%
%% The 'bits' field indicates how many bits the integer
%% returned from 'next' has got, i.e 'next' shall return
%% an random integer in the range 0..(2^Bits - 1).
%% At least 55 bits is required for the floating point
%% producing fallbacks, but 56 bits would be more future proof.
%%
%% The fields 'next', 'uniform' and 'uniform_n'
%% implement the algorithm.  If 'uniform' or 'uniform_n'
%% is not present there is a fallback using 'next' and either
%% 'bits' or the deprecated 'max'.  The 'next' function
%% must generate a word with at least 56 good random bits.
%%
%% The 'weak_low_bits' field indicate how many bits are of
%% lesser quality and they will not be used by the floating point
%% producing functions, nor by the range producing functions
%% when more bits are needed, to avoid weak bits in the middle
%% of the generated bits.  The lowest bits from the range
%% functions still have the generator's quality.
%%
-type alg_handler() ::
        #{type := alg(),
          bits => non_neg_integer(),
          weak_low_bits => non_neg_integer(),
          max => non_neg_integer(), % Deprecated
          next :=
              fun ((alg_state()) -> {non_neg_integer(), alg_state()}),
          uniform =>
              fun ((state()) -> {float(), state()}),
          uniform_n =>
              fun ((pos_integer(), state()) -> {pos_integer(), state()}),
          jump =>
              fun ((state()) -> state())}.

%% Algorithm state
-type state() :: {alg_handler(), alg_state()}.
-type builtin_alg() :: exs64 | exsplus | exsp | exs1024 | exs1024s | exrop.
-type alg() :: builtin_alg() | atom().
-type export_state() :: {alg(), alg_state()}.
-export_type(
   [builtin_alg/0, alg/0, alg_handler/0, alg_state/0,
    state/0, export_state/0]).
-export_type(
   [exs64_state/0, exsplus_state/0, exs1024_state/0, exrop_state/0]).

%% =====================================================================
%% Range macro and helper
%% =====================================================================

-define(
   uniform_range(Range, Alg, R, V, MaxMinusRange, I),
   if
       0 =< (MaxMinusRange) ->
           if
               %% Really work saving in odd cases;
               %% large ranges in particular
               (V) < (Range) ->
                   {(V) + 1, {(Alg), (R)}};
               true ->
                   (I) = (V) rem (Range),
                   if
                       (V) - (I) =< (MaxMinusRange) ->
                           {(I) + 1, {(Alg), (R)}};
                       true ->
                           %% V in the truncated top range
                           %% - try again
                           ?FUNCTION_NAME((Range), {(Alg), (R)})
                   end
           end;
       true ->
           uniform_range((Range), (Alg), (R), (V))
   end).

%% For ranges larger than the algorithm bit size
uniform_range(Range, #{next:=Next, bits:=Bits} = Alg, R, V) ->
    WeakLowBits = maps:get(weak_low_bits, Alg, 0),
    %% Maybe waste the lowest bit(s) when shifting in new bits
    Shift = Bits - WeakLowBits,
    ShiftMask = bnot ?MASK(WeakLowBits),
    RangeMinus1 = Range - 1,
    if
        (Range band RangeMinus1) =:= 0 -> % Power of 2
            %% Generate at least the number of bits for the range
            {V1, R1, _} =
                uniform_range(
                  Range bsr Bits, Next, R, V, ShiftMask, Shift, Bits),
            {(V1 band RangeMinus1) + 1, {Alg, R1}};
        true ->
            %% Generate a value with at least two bits more than the range
            %% and try that for a fit, otherwise recurse
            %%
            %% Just one bit more should ensure that the generated
            %% number range is at least twice the size of the requested
            %% range, which would make the probability to draw a good
            %% number better than 0.5.  And repeating that until
            %% success i guess would take 2 times statistically amortized.
            %% But since the probability for fairly many attemtpts
            %% is not that low, use two bits more than the range which 
            %% should make the probability to draw a bad number under 0.25,
            %% which decreases the bad case probability a lot.
            {V1, R1, B} =
                uniform_range(
                  Range bsr (Bits - 2), Next, R, V, ShiftMask, Shift, Bits),
            I = V1 rem Range,
            if
                (V1 - I) =< (1 bsl B) - Range ->
                    {I + 1, {Alg, R1}};
                true ->
                    %% V1 drawn from the truncated top range
                    %% - try again
                    {V2, R2} = Next(R1),
                    uniform_range(Range, Alg, R2, V2)
            end
    end.
%%
uniform_range(Range, Next, R, V, ShiftMask, Shift, B) ->
    if 
        Range =< 1 ->
            {V, R, B};
        true ->
            {V1, R1} = Next(R),
            %% Waste the lowest bit(s) when shifting in new bits
            uniform_range(
              Range bsr Shift, Next, R1,
              ((V band ShiftMask) bsl Shift) bor V1,
              ShiftMask, Shift, B + Shift)
    end.

%% =====================================================================
%% API
%% =====================================================================

%% Return algorithm and seed so that RNG state can be recreated with seed/1
-spec export_seed() -> undefined | export_state().
export_seed() ->
    case get(?SEED_DICT) of
	{#{type:=Alg}, Seed} -> {Alg, Seed};
	_ -> undefined
    end.

-spec export_seed_s(State :: state()) -> export_state().
export_seed_s({#{type:=Alg}, Seed}) -> {Alg, Seed}.

%% seed(Alg) seeds RNG with runtime dependent values
%% and return the NEW state

%% seed({Alg,Seed}) setup RNG with a previously exported seed
%% and return the NEW state

-spec seed(
        AlgOrStateOrExpState :: builtin_alg() | state() | export_state()) ->
                  state().
seed(Alg) ->
    seed_put(seed_s(Alg)).

-spec seed_s(
        AlgOrStateOrExpState :: builtin_alg() | state() | export_state()) ->
                    state().
seed_s({AlgHandler, _Seed} = State) when is_map(AlgHandler) ->
    State;
seed_s({Alg0, Seed}) ->
    {Alg,_SeedFun} = mk_alg(Alg0),
    {Alg, Seed};
seed_s(Alg) ->
    seed_s(Alg, {erlang:phash2([{node(),self()}]),
		 erlang:system_time(),
		 erlang:unique_integer()}).

%% seed/2: seeds RNG with the algorithm and given values
%% and returns the NEW state.

-spec seed(
        Alg :: builtin_alg(), Seed :: {integer(), integer(), integer()}) ->
                  state().
seed(Alg0, S0) ->
    seed_put(seed_s(Alg0, S0)).

-spec seed_s(
        Alg :: builtin_alg(), Seed :: {integer(), integer(), integer()}) ->
                    state().
seed_s(Alg0, S0 = {_, _, _}) ->
    {Alg, Seed} = mk_alg(Alg0),
    AS = Seed(S0),
    {Alg, AS}.

%%% uniform/0, uniform/1, uniform_s/1, uniform_s/2 are all
%%% uniformly distributed random numbers.

%% uniform/0: returns a random float X where 0.0 =< X < 1.0,
%% updating the state in the process dictionary.

-spec uniform() -> X :: float().
uniform() ->
    {X, Seed} = uniform_s(seed_get()),
    _ = seed_put(Seed),
    X.

%% uniform/1: given an integer N >= 1,
%% uniform/1 returns a random integer X where 1 =< X =< N,
%% updating the state in the process dictionary.

-spec uniform(N :: pos_integer()) -> X :: pos_integer().
uniform(N) ->
    {X, Seed} = uniform_s(N, seed_get()),
    _ = seed_put(Seed),
    X.

%% uniform_s/1: given a state, uniform_s/1
%% returns a random float X where 0.0 =< X < 1.0,
%% and a new state.

-spec uniform_s(State :: state()) -> {X :: float(), NewState :: state()}.
uniform_s(State = {#{uniform:=Uniform}, _}) ->
    Uniform(State);
uniform_s({#{bits:=Bits, next:=Next} = Alg, R0}) ->
    {V, R1} = Next(R0),
    %% Produce floats on the form N * 2^(-53)
    {(V bsr (Bits - 53)) * ?TWO_POW_MINUS53, {Alg, R1}};
uniform_s({#{max:=Max, next:=Next} = Alg, R0}) ->
    {V, R1} = Next(R0),
    %% Old algorithm with non-uniform density
    {V / (Max + 1), {Alg, R1}}.


%% uniform_s/2: given an integer N >= 1 and a state, uniform_s/2
%% uniform_s/2 returns a random integer X where 1 =< X =< N,
%% and a new state.

-spec uniform_s(N :: pos_integer(), State :: state()) ->
                       {X :: pos_integer(), NewState :: state()}.
uniform_s(N, State = {#{uniform_n:=UniformN}, _})
  when is_integer(N), 1 =< N ->
    UniformN(N, State);
uniform_s(N, {#{bits:=Bits, next:=Next} = Alg, R0})
  when is_integer(N), 1 =< N ->
    {V, R1} = Next(R0),
    MaxMinusN = ?BIT(Bits) - N,
    ?uniform_range(N, Alg, R1, V, MaxMinusN, I);
uniform_s(N, {#{max:=Max, next:=Next} = Alg, R0})
  when is_integer(N), 1 =< N ->
    %% Old algorithm with skewed probability
    %% and gap in ranges > Max
    {V, R1} = Next(R0),  
    if
        N =< Max ->
            {(V rem N) + 1, {Alg, R1}};
        true ->
            F = V / (Max + 1),
            {trunc(F * N) + 1, {Alg, R1}}
    end.

%% uniform_real/0: returns a random float X where 0.0 < X =< 1.0,
%% updating the state in the process dictionary.

-spec uniform_real() -> X :: float().
uniform_real() ->
    {X, Seed} = uniform_real_s(seed_get()),
    _ = seed_put(Seed),
    X.

%% uniform_real_s/1: given a state, uniform_s/1
%% returns a random float X where 0.0 < X =< 1.0,
%% and a new state.
%%
%% This function does not use the same form of uniformity
%% as the uniform_s/1 function.
%%
%% Instead, this function does not generate numbers with equal
%% distance in the interval, but rather tries to keep all mantissa
%% bits random also for small numbers, meaning that the distance
%% between possible numbers decreases when the numbers
%% approaches 0.0, as does the possibility for a particular
%% number.  Hence uniformity is preserved.
%%
%% To generate 56 bits at the time instead of 53 is actually
%% a speed optimization since the probability to have to
%% generate a second word decreases by 1/2 for every extra bit.
%%
%% This function generates normalized numbers, so the smallest number
%% that can be generated is 2^-1022 with the distance 2^-1074
%% to the next to smallest number, compared to 2^-53 for uniform_s/1.
%%
%% This concept of uniformity should work better for applications
%% where you need to calculate 1.0/X or math:log(X) since those
%% operations benefits from larger precision approaching 0.0,
%% and that this function does not return 0.0 nor denormalized
%% numbers very close to 0.0.  The log() operation in The Box-Muller
%% transformation for normal distribution is an example of this.
%%
%%-define(TWO_POW_MINUS55, (math:pow(2, -55))).
%%-define(TWO_POW_MINUS110, (math:pow(2, -110))).
%%-define(TWO_POW_MINUS55, 2.7755575615628914e-17).
%%-define(TWO_POW_MINUS110, 7.7037197775489436e-34).
%%
-spec uniform_real_s(State :: state()) -> {X :: float(), NewState :: state()}.
uniform_real_s({#{bits:=Bits, next:=Next} = Alg, R0}) ->
    %% Generate a 56 bit number without using the weak low bits.
    %%
    %% Be sure to use only 53 bits when multiplying with
    %% math:pow(2.0, -N) to avoid rounding which would make
    %% "even" floats more probable than "odd".
    %%
    {V1, R1} = Next(R0),
    M1 = V1 bsr (Bits - 56),
    if
        ?BIT(55) =< M1 ->
            %% We have 56 bits - waste 3
            {(M1 bsr 3) * math:pow(2.0, -53), {Alg, R1}};
        ?BIT(54) =< M1 ->
            %% We have 55 bits - waste 2
            {(M1 bsr 2) * math:pow(2.0, -54), {Alg, R1}};
        ?BIT(53) =< M1 ->
            %% We have 54 bits - waste 1
            {(M1 bsr 1) * math:pow(2.0, -55), {Alg, R1}};
        ?BIT(52) =< M1 ->
            %% We have 53 bits - use all
            {M1 * math:pow(2.0, -56), {Alg, R1}};
        true ->
            %% Need more bits
            {V2, R2} = Next(R1),
            uniform_real_s(Alg, Next, M1, -56, R2, V2, Bits)
    end;
uniform_real_s({#{max:=_, next:=Next} = Alg, R0}) ->
    %% Generate a 56 bit number.
    %% Ignore the weak low bits for these old algorithms,
    %% just produce something reasonable.
    %%
    %% Be sure to use only 53 bits when multiplying with
    %% math:pow(2.0, -N) to avoid rounding which would make
    %% "even" floats more probable than "odd".
    %%
    {V1, R1} = Next(R0),
    M1 = ?MASK(56, V1),
    if
        ?BIT(55) =< M1 ->
            %% We have 56 bits - waste 3
            {(M1 bsr 3) * math:pow(2.0, -53), {Alg, R1}};
        ?BIT(54) =< M1 ->
            %% We have 55 bits - waste 2
            {(M1 bsr 2) * math:pow(2.0, -54), {Alg, R1}};
        ?BIT(53) =< M1 ->
            %% We have 54 bits - waste 1
            {(M1 bsr 1) * math:pow(2.0, -55), {Alg, R1}};
        ?BIT(52) =< M1 ->
            %% We have 53 bits - use all
            {M1 * math:pow(2.0, -56), {Alg, R1}};
        true ->
            %% Need more bits
            {V2, R2} = Next(R1),
            uniform_real_s(Alg, Next, M1, -56, R2, V2, 56)
    end.

uniform_real_s(Alg, _Next, M0, -1064, R1, V1, Bits) -> % 19*56
    %% This is a very theoretical bottom case.
    %% The odds of getting here is about 2^-1008,
    %% through a white box test case, or thanks to
    %% a malfunctioning PRNG producing 18 56-bit zeros in a row.
    %%
    %% Fill up to 53 bits, we have at most 52
    B0 = (53 - ?BC(M0, 52)), % Missing bits
    {(((M0 bsl B0) bor (V1 bsr (Bits - B0))) * math:pow(2.0, -1064 - B0)),
     {Alg, R1}};
uniform_real_s(Alg, Next, M0, BitNo, R1, V1, Bits) ->
    if
        %% Optimize the most probable.
        %% Fill up to 53 bits.
        ?BIT(51) =< M0 ->
            %% We have 52 bits in M0 - need 1
            {(((M0 bsl 1) bor (V1 bsr (Bits - 1)))
              * math:pow(2.0, BitNo - 1)),
             {Alg, R1}};
        ?BIT(50) =< M0 ->
            %% We have 51 bits in M0 - need 2
            {(((M0 bsl 2) bor (V1 bsr (Bits - 2)))
              * math:pow(2.0, BitNo - 2)),
             {Alg, R1}};
        ?BIT(49) =< M0 ->
            %% We have 50 bits in M0 - need 3
            {(((M0 bsl 3) bor (V1 bsr (Bits - 3)))
              * math:pow(2.0, BitNo - 3)),
             {Alg, R1}};
        M0 == 0 ->
            M1 = V1 bsr (Bits - 56),
            if
                ?BIT(55) =< M1 ->
                    %% We have 56 bits - waste 3
                    {(M1 bsr 3) * math:pow(2.0, BitNo - 53), {Alg, R1}};
                ?BIT(54) =< M1 ->
                    %% We have 55 bits - waste 2
                    {(M1 bsr 2) * math:pow(2.0, BitNo - 54), {Alg, R1}};
                ?BIT(53) =< M1 ->
                    %% We have 54 bits - waste 1
                    {(M1 bsr 1) * math:pow(2.0, BitNo - 55), {Alg, R1}};
                ?BIT(52) =< M1 ->
                    %% We have 53 bits - use all
                    {M1 * math:pow(2.0, BitNo - 56), {Alg, R1}};
                BitNo =:= -1008 ->
                    %% Endgame
                    %% For the last round we can not have 14 zeros or more
                    %% at the top of M1 because then we will underflow,
                    %% so we need at least 43 bits
                    if
                        ?BIT(42) =< M1 ->
                            %% We have 43 bits - get the last bits
                            uniform_real_s(Alg, Next, M1, BitNo - 56, R1);
                        true ->
                            %% Would underflow 2^-1022 - start all over
                            %%
                            %% We could just crash here since the odds for
                            %% the PRNG being broken is much higher than
                            %% for a good PRNG generating this many zeros
                            %% in a row.  Maybe we should write an error
                            %% report or call this a system limit...?
                            uniform_real_s({Alg, R1})
                    end;
                true ->
                    %% Need more bits
                    uniform_real_s(Alg, Next, M1, BitNo - 56, R1)
            end;
        true ->
            %% Fill up to 53 bits
            B0 = 53 - ?BC(M0, 49), % Number of bits we need to append
            {(((M0 bsl B0) bor (V1 bsr (Bits - B0)))
              * math:pow(2.0, BitNo - B0)),
             {Alg, R1}}
    end.
%%
uniform_real_s(#{bits:=Bits} = Alg, Next, M0, BitNo, R0) ->
    {V1, R1} = Next(R0),
    uniform_real_s(Alg, Next, M0, BitNo, R1, V1, Bits);
uniform_real_s(#{max:=_} = Alg, Next, M0, BitNo, R0) ->
    {V1, R1} = Next(R0),
    uniform_real_s(Alg, Next, M0, BitNo, R1, ?MASK(56, V1), 56).

%% jump/1: given a state, jump/1
%% returns a new state which is equivalent to that
%% after a large number of call defined for each algorithm.
%% The large number is algorithm dependent.

-spec jump(state()) -> NewState :: state().
jump(State = {#{jump:=Jump}, _}) ->
    Jump(State);
jump({#{}, _}) ->
    erlang:error(not_implemented).


%% jump/0: read the internal state and
%% apply the jump function for the state as in jump/1
%% and write back the new value to the internal state,
%% then returns the new value.

-spec jump() -> NewState :: state().
jump() ->
    seed_put(jump(seed_get())).

%% normal/0: returns a random float with standard normal distribution
%% updating the state in the process dictionary.

-spec normal() -> float().
normal() ->
    {X, Seed} = normal_s(seed_get()),
    _ = seed_put(Seed),
    X.

%% normal/2: returns a random float with N(μ, σ²) normal distribution
%% updating the state in the process dictionary.

-spec normal(Mean :: number(), Variance :: number()) -> float().
normal(Mean, Variance) ->
    Mean + (math:sqrt(Variance) * normal()).

%% normal_s/1: returns a random float with standard normal distribution
%% The Ziggurat Method for generating random variables - Marsaglia and Tsang
%% Paper and reference code: http://www.jstatsoft.org/v05/i08/

-spec normal_s(State :: state()) -> {float(), NewState :: state()}.
normal_s(State0) ->
    {Sign, R, State} = get_52(State0),
    Idx = ?MASK(8, R),
    Idx1 = Idx+1,
    {Ki, Wi} = normal_kiwi(Idx1),
    X = R * Wi,
    case R < Ki of
	%% Fast path 95% of the time
	true when Sign =:= 0 -> {X, State};
	true -> {-X, State};
	%% Slow path
	false when Sign =:= 0 -> normal_s(Idx, Sign, X, State);
	false -> normal_s(Idx, Sign, -X, State)
    end.

%% normal_s/3: returns a random float with normal N(μ, σ²) distribution

-spec normal_s(Mean :: number(), Variance :: number(), state()) -> {float(), NewS :: state()}.
normal_s(Mean, Variance, State0) when Variance > 0 ->
    {X, State} = normal_s(State0),
    {Mean + (math:sqrt(Variance) * X), State}.

%% =====================================================================
%% Internal functions

-spec seed_put(state()) -> state().
seed_put(Seed) ->
    put(?SEED_DICT, Seed),
    Seed.

seed_get() ->
    case get(?SEED_DICT) of
        undefined -> seed(?DEFAULT_ALG_HANDLER);
        Old -> Old  % no type checking here
    end.

%% Setup alg record
mk_alg(exs64) ->
    {#{type=>exs64, max=>?MASK(64), next=>fun exs64_next/1},
     fun exs64_seed/1};
mk_alg(exsplus) ->
    {#{type=>exsplus, max=>?MASK(58), next=>fun exsplus_next/1,
       jump=>fun exsplus_jump/1},
     fun exsplus_seed/1};
mk_alg(exsp) ->
    {#{type=>exsp, bits=>58, weak_low_bits=>1, next=>fun exsplus_next/1,
       uniform=>fun exsp_uniform/1, uniform_n=>fun exsp_uniform/2,
       jump=>fun exsplus_jump/1},
     fun exsplus_seed/1};
mk_alg(exs1024) ->
    {#{type=>exs1024, max=>?MASK(64), next=>fun exs1024_next/1,
       jump=>fun exs1024_jump/1},
     fun exs1024_seed/1};
mk_alg(exs1024s) ->
    {#{type=>exs1024s, bits=>64, weak_low_bits=>3, next=>fun exs1024_next/1,
       jump=>fun exs1024_jump/1},
     fun exs1024_seed/1};
mk_alg(exrop) ->
    {#{type=>exrop, bits=>58, weak_low_bits=>1, next=>fun exrop_next/1,
       uniform=>fun exrop_uniform/1, uniform_n=>fun exrop_uniform/2,
       jump=>fun exrop_jump/1},
     fun exrop_seed/1}.

%% =====================================================================
%% exs64 PRNG: Xorshift64*
%% Algorithm by Sebastiano Vigna
%% Reference URL: http://xorshift.di.unimi.it/
%% =====================================================================

-opaque exs64_state() :: uint64().

exs64_seed({A1, A2, A3}) ->
    {V1, _} = exs64_next((?MASK(32, A1) * 4294967197 + 1)),
    {V2, _} = exs64_next((?MASK(32, A2) * 4294967231 + 1)),
    {V3, _} = exs64_next((?MASK(32, A3) * 4294967279 + 1)),
    ((V1 * V2 * V3) rem (?MASK(64) - 1)) + 1.

%% Advance xorshift64* state for one step and generate 64bit unsigned integer
-spec exs64_next(exs64_state()) -> {uint64(), exs64_state()}.
exs64_next(R) ->
    R1 = R bxor (R bsr 12),
    R2 = R1 bxor ?BSL(64, R1, 25),
    R3 = R2 bxor (R2 bsr 27),
    {?MASK(64, R3 * 2685821657736338717), R3}.

%% =====================================================================
%% exsplus PRNG: Xorshift116+
%% Algorithm by Sebastiano Vigna
%% Reference URL: http://xorshift.di.unimi.it/
%% 58 bits fits into an immediate on 64bits erlang and is thus much faster.
%% Modification of the original Xorshift128+ algorithm to 116
%% by Sebastiano Vigna, a lot of thanks for his help and work.
%% =====================================================================
-opaque exsplus_state() :: nonempty_improper_list(uint58(), uint58()).

-dialyzer({no_improper_lists, exsplus_seed/1}).

exsplus_seed({A1, A2, A3}) ->
    {_, R1} = exsplus_next(
                [?MASK(58, (A1 * 4294967197) + 1)|
                 ?MASK(58, (A2 * 4294967231) + 1)]),
    {_, R2} = exsplus_next(
                [?MASK(58, (A3 * 4294967279) + 1)|
                 tl(R1)]),
    R2.

-dialyzer({no_improper_lists, exsplus_next/1}).

%% Advance xorshift116+ state for one step and generate 58bit unsigned integer
-spec exsplus_next(exsplus_state()) -> {uint58(), exsplus_state()}.
exsplus_next([S1|S0]) ->
    %% Note: members s0 and s1 are swapped here
    S11 = S1 bxor ?BSL(58, S1, 24),
    S12 = S11 bxor S0 bxor (S11 bsr 11) bxor (S0 bsr 41),
    {?MASK(58, S0 + S12), [S0|S12]}.


exsp_uniform({Alg, R0}) ->
    {I, R1} = exsplus_next(R0),
    %% Waste the lowest bit since it is of lower
    %% randomness quality than the others
    {(I bsr (58-53)) * ?TWO_POW_MINUS53, {Alg, R1}}.

exsp_uniform(Range, {Alg, R}) ->
    {V, R1} = exsplus_next(R),
    MaxMinusRange = ?BIT(58) - Range,
    ?uniform_range(Range, Alg, R1, V, MaxMinusRange, I).


%% This is the jump function for the exsplus generator, equivalent
%% to 2^64 calls to next/1; it can be used to generate 2^52
%% non-overlapping subsequences for parallel computations.
%% Note: the jump function takes 116 times of the execution time of
%% next/1.

%% -define(JUMPCONST, 16#000d174a83e17de2302f8ea6bc32c797).
%% split into 58-bit chunks
%% and two iterative executions

-define(JUMPCONST1, 16#02f8ea6bc32c797).
-define(JUMPCONST2, 16#345d2a0f85f788c).
-define(JUMPELEMLEN, 58).

-dialyzer({no_improper_lists, exsplus_jump/1}).
-spec exsplus_jump(state()) -> state().
exsplus_jump({Alg, S}) ->
    {S1, AS1} = exsplus_jump(S, [0|0], ?JUMPCONST1, ?JUMPELEMLEN),
    {_,  AS2} = exsplus_jump(S1, AS1,  ?JUMPCONST2, ?JUMPELEMLEN),
    {Alg, AS2}.

-dialyzer({no_improper_lists, exsplus_jump/4}).
exsplus_jump(S, AS, _, 0) ->
    {S, AS};
exsplus_jump(S, [AS0|AS1], J, N) ->
    {_, NS} = exsplus_next(S),
    case ?MASK(1, J) of
        1 ->
            [S0|S1] = S,
            exsplus_jump(NS, [(AS0 bxor S0)|(AS1 bxor S1)], J bsr 1, N-1);
        0 ->
            exsplus_jump(NS, [AS0|AS1], J bsr 1, N-1)
    end.

%% =====================================================================
%% exs1024 PRNG: Xorshift1024*
%% Algorithm by Sebastiano Vigna
%% Reference URL: http://xorshift.di.unimi.it/
%% =====================================================================

-opaque exs1024_state() :: {list(uint64()), list(uint64())}.

exs1024_seed({A1, A2, A3}) ->
    B1 = ?MASK(21, (?MASK(21, A1) + 1) * 2097131),
    B2 = ?MASK(21, (?MASK(21, A2) + 1) * 2097133),
    B3 = ?MASK(21, (?MASK(21, A3) + 1) * 2097143),
    {exs1024_gen1024((B1 bsl 43) bor (B2 bsl 22) bor (B3 bsl 1) bor 1),
     []}.

%% Generate a list of 16 64-bit element list
%% of the xorshift64* random sequence
%% from a given 64-bit seed.
%% Note: dependent on exs64_next/1
-spec exs1024_gen1024(uint64()) -> list(uint64()).
exs1024_gen1024(R) ->
    exs1024_gen1024(16, R, []).

exs1024_gen1024(0, _, L) ->
    L;
exs1024_gen1024(N, R, L) ->
    {X, R2} = exs64_next(R),
    exs1024_gen1024(N - 1, R2, [X|L]).

%% Calculation of xorshift1024*.
%% exs1024_calc(S0, S1) -> {X, NS1}.
%% X: random number output
-spec exs1024_calc(uint64(), uint64()) -> {uint64(), uint64()}.
exs1024_calc(S0, S1) ->
    S11 = S1 bxor ?BSL(64, S1, 31),
    S12 = S11 bxor (S11 bsr 11),
    S01 = S0 bxor (S0 bsr 30),
    NS1 = S01 bxor S12,
    {?MASK(64, NS1 * 1181783497276652981), NS1}.

%% Advance xorshift1024* state for one step and generate 64bit unsigned integer
-spec exs1024_next(exs1024_state()) -> {uint64(), exs1024_state()}.
exs1024_next({[S0,S1|L3], RL}) ->
    {X, NS1} = exs1024_calc(S0, S1),
    {X, {[NS1|L3], [S0|RL]}};
exs1024_next({[H], RL}) ->
    NL = [H|lists:reverse(RL)],
    exs1024_next({NL, []}).


%% This is the jump function for the exs1024 generator, equivalent
%% to 2^512 calls to next(); it can be used to generate 2^512
%% non-overlapping subsequences for parallel computations.
%% Note: the jump function takes ~2000 times of the execution time of
%% next/1.

%% Jump constant here split into 58 bits for speed
-define(JUMPCONSTHEAD, 16#00242f96eca9c41d).
-define(JUMPCONSTTAIL,
        [16#0196e1ddbe5a1561,
         16#0239f070b5837a3c,
         16#03f393cc68796cd2,
         16#0248316f404489af,
         16#039a30088bffbac2,
         16#02fea70dc2d9891f,
         16#032ae0d9644caec4,
         16#0313aac17d8efa43,
         16#02f132e055642626,
         16#01ee975283d71c93,
         16#00552321b06f5501,
         16#00c41d10a1e6a569,
         16#019158ecf8aa1e44,
         16#004e9fc949d0b5fc,
         16#0363da172811fdda,
         16#030e38c3b99181f2,
         16#0000000a118038fc]).
-define(JUMPTOTALLEN, 1024).
-define(RINGLEN, 16).

-spec exs1024_jump(state()) -> state().

exs1024_jump({Alg, {L, RL}}) ->
    P = length(RL),
    AS = exs1024_jump({L, RL},
         [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
         ?JUMPCONSTTAIL, ?JUMPCONSTHEAD, ?JUMPELEMLEN, ?JUMPTOTALLEN),
    {ASL, ASR} = lists:split(?RINGLEN - P, AS),
    {Alg, {ASL, lists:reverse(ASR)}}.

exs1024_jump(_, AS, _, _, _, 0) ->
    AS;
exs1024_jump(S, AS, [H|T], _, 0, TN) ->
    exs1024_jump(S, AS, T, H, ?JUMPELEMLEN, TN);
exs1024_jump({L, RL}, AS, JL, J, N, TN) ->
    {_, NS} = exs1024_next({L, RL}),
    case ?MASK(1, J) of
        1 ->
            AS2 = lists:zipwith(fun(X, Y) -> X bxor Y end,
                        AS, L ++ lists:reverse(RL)),
            exs1024_jump(NS, AS2, JL, J bsr 1, N-1, TN-1);
        0 ->
            exs1024_jump(NS, AS, JL, J bsr 1, N-1, TN-1)
    end.

%% =====================================================================
%% exrop PRNG: Xoroshiro116+
%%
%% Reference URL: http://xorshift.di.unimi.it/
%%
%% 58 bits fits into an immediate on 64bits Erlang and is thus much faster.
%% In fact, an immediate number is 60 bits signed in Erlang so you can
%% add two positive 58 bit numbers and get a 59 bit number that still is
%% a positive immediate, which is a property we utilize here...
%%
%% Modification of the original Xororhiro128+ algorithm to 116 bits
%% by Sebastiano Vigna.  A lot of thanks for his help and work.
%% =====================================================================
%% (a, b, c) = (24, 2, 35)
%% JUMP Polynomial = 0x9863200f83fcd4a11293241fcb12a (116 bit)
%%
%% From http://xoroshiro.di.unimi.it/xoroshiro116plus.c:
%% ---------------------------------------------------------------------
%% /* Written in 2017 by Sebastiano Vigna (vigna@acm.org).
%%
%% To the extent possible under law, the author has dedicated all copyright
%% and related and neighboring rights to this software to the public domain
%% worldwide. This software is distributed without any warranty.
%%
%% See <http://creativecommons.org/publicdomain/zero/1.0/>. */
%%
%% #include <stdint.h>
%%
%% #define UINT58MASK (uint64_t)((UINT64_C(1) << 58) - 1)
%%
%% uint64_t s[2];
%%
%% static inline uint64_t rotl58(const uint64_t x, int k) {
%%     return (x << k) & UINT58MASK | (x >> (58 - k));
%% }
%% 
%% uint64_t next(void) {
%%     uint64_t s1 = s[1];
%%     const uint64_t s0 = s[0];
%%     const uint64_t result = (s0 + s1) & UINT58MASK;
%%
%%     s1 ^= s0;
%%     s[0] = rotl58(s0, 24) ^ s1 ^ ((s1 << 2) & UINT58MASK); // a, b
%%     s[1] = rotl58(s1, 35); // c
%%     return result;
%% }
%%
%% void jump(void) {
%%     static const uint64_t JUMP[] =
%%         { 0x4a11293241fcb12a, 0x0009863200f83fcd };
%%
%%     uint64_t s0 = 0;
%%     uint64_t s1 = 0;
%%     for(int i = 0; i < sizeof JUMP / sizeof *JUMP; i++)
%%         for(int b = 0; b < 64; b++) {
%%             if (JUMP[i] & UINT64_C(1) << b) {
%%                 s0 ^= s[0];
%% 	           s1 ^= s[1];
%% 	       }
%% 	       next();
%% 	   }
%%     s[0] = s0;
%%     s[1] = s1;
%% }

-opaque exrop_state() :: nonempty_improper_list(uint58(), uint58()).

-dialyzer({no_improper_lists, exrop_seed/1}).
exrop_seed({A1, A2, A3}) ->
    [_|S1] =
        exrop_next_s(
          ?MASK(58, (A1 * 4294967197) + 1),
          ?MASK(58, (A2 * 4294967231) + 1)),
    exrop_next_s(?MASK(58, (A3 * 4294967279) + 1), S1).

-dialyzer({no_improper_lists, exrop_next_s/2}).
%% Advance xoroshiro116+ state one step
%% [a, b, c] = [24, 2, 35]
-define(
   exrop_next_s(S0, S1, S1_a),
   begin
       S1_a = S1 bxor S0,
       [?ROTL(58, S0, 24) bxor S1_a bxor ?BSL(58, S1_a, 2)| % a, b
        ?ROTL(58, S1_a, 35)] % c
   end).
exrop_next_s(S0, S1) ->
    ?exrop_next_s(S0, S1, S1_a).

-dialyzer({no_improper_lists, exrop_next/1}).
%% Advance xoroshiro116+ state one step, generate 58 bit unsigned integer,
%% and waste the lowest bit since it is of lower randomness quality
exrop_next([S0|S1]) ->
    {?MASK(58, S0 + S1), ?exrop_next_s(S0, S1, S1_a)}.

exrop_uniform({Alg, R}) ->
    {V, R1} = exrop_next(R),
    %% Waste the lowest bit since it is of lower
    %% randomness quality than the others
    {(V bsr (58-53)) * ?TWO_POW_MINUS53, {Alg, R1}}.

exrop_uniform(Range, {Alg, R}) ->
    {V, R1} = exrop_next(R),
    MaxMinusRange = ?BIT(58) - Range,
    ?uniform_range(Range, Alg, R1, V, MaxMinusRange, I).

%% Split a 116 bit constant into two 58 bit words,
%% a top '1' marks the end of the low word.
-define(
   JUMP_116(Jump),
   [?BIT(58) bor ?MASK(58, (Jump)),(Jump) bsr 58]).
%%
exrop_jump({Alg,S}) ->
    [J|Js] = ?JUMP_116(16#9863200f83fcd4a11293241fcb12a),
    {Alg, exrop_jump(S, 0, 0, J, Js)}.
%%
-dialyzer({no_improper_lists, exrop_jump/5}).
exrop_jump(_S, S0, S1, 0, []) -> % End of jump constant
    [S0|S1];
exrop_jump(S, S0, S1, 1, [J|Js]) -> % End of word
    exrop_jump(S, S0, S1, J, Js);
exrop_jump([S__0|S__1] = _S, S0, S1, J, Js) ->
    case ?MASK(1, J) of
        1 ->
            NewS = exrop_next_s(S__0, S__1),
            exrop_jump(NewS, S0 bxor S__0, S1 bxor S__1, J bsr 1, Js);
        0 ->
            NewS = exrop_next_s(S__0, S__1),
            exrop_jump(NewS, S0, S1, J bsr 1, Js)
    end.

%% =====================================================================
%% Ziggurat cont
%% =====================================================================
-define(NOR_R, 3.6541528853610087963519472518).
-define(NOR_INV_R, 1/?NOR_R).

%% return a {sign, Random51bits, State}
get_52({Alg=#{bits:=Bits, next:=Next}, S0}) ->
    %% Use the high bits
    {Int,S1} = Next(S0),
    {?BIT(Bits - 51 - 1) band Int, Int bsr (Bits - 51), {Alg, S1}};
get_52({Alg=#{next:=Next}, S0}) ->
    {Int,S1} = Next(S0),
    {?BIT(51) band Int, ?MASK(51, Int), {Alg, S1}}.

%% Slow path
normal_s(0, Sign, X0, State0) ->
    {U0, S1} = uniform_s(State0),
    X = -?NOR_INV_R*math:log(U0),
    {U1, S2} = uniform_s(S1),
    Y = -math:log(U1),
    case Y+Y > X*X of
	false ->
	    normal_s(0, Sign, X0, S2);
	true when Sign =:= 0 ->
	    {?NOR_R + X, S2};
	true ->
	    {-?NOR_R - X, S2}
    end;
normal_s(Idx, _Sign, X, State0) ->
    Fi2 = normal_fi(Idx+1),
    {U0, S1} = uniform_s(State0),
    case ((normal_fi(Idx) - Fi2)*U0 + Fi2) < math:exp(-0.5*X*X) of
	true ->  {X, S1};
	false -> normal_s(S1)
    end.

%% Tables for generating normal_s
%% ki is zipped with wi (slightly faster)
normal_kiwi(Indx) ->
    element(Indx,
	{{2104047571236786,1.736725412160263e-15}, {0,9.558660351455634e-17},
	 {1693657211986787,1.2708704834810623e-16},{1919380038271141,1.4909740962495474e-16},
	 {2015384402196343,1.6658733631586268e-16},{2068365869448128,1.8136120810119029e-16},
	 {2101878624052573,1.9429720153135588e-16},{2124958784102998,2.0589500628482093e-16},
	 {2141808670795147,2.1646860576895422e-16},{2154644611568301,2.2622940392218116e-16},
	 {2164744887587275,2.353271891404589e-16},{2172897953696594,2.438723455742877e-16},
	 {2179616279372365,2.5194879829274225e-16},{2185247251868649,2.5962199772528103e-16},
	 {2190034623107822,2.6694407473648285e-16},{2194154434521197,2.7395729685142446e-16},
	 {2197736978774660,2.8069646002484804e-16},{2200880740891961,2.871905890411393e-16},
	 {2203661538010620,2.9346417484728883e-16},{2206138681109102,2.9953809336782113e-16},
	 {2208359231806599,3.054303000719244e-16},{2210361007258210,3.111563633892157e-16},
	 {2212174742388539,3.1672988018581815e-16},{2213825672704646,3.2216280350549905e-16},
	 {2215334711002614,3.274657040793975e-16},{2216719334487595,3.326479811684171e-16},
	 {2217994262139172,3.377180341735323e-16},{2219171977965032,3.4268340353119356e-16},
	 {2220263139538712,3.475508873172976e-16},{2221276900117330,3.523266384600203e-16},
	 {2222221164932930,3.5701624633953494e-16},{2223102796829069,3.616248057159834e-16},
	 {2223927782546658,3.661569752965354e-16},{2224701368170060,3.7061702777236077e-16},
	 {2225428170204312,3.75008892787478e-16},{2226112267248242,3.7933619401549554e-16},
	 {2226757276105256,3.836022812967728e-16},{2227366415328399,3.8781025861250247e-16},
	 {2227942558554684,3.919630085325768e-16},{2228488279492521,3.9606321366256378e-16},
	 {2229005890047222,4.001133755254669e-16},{2229497472775193,4.041158312414333e-16},
	 {2229964908627060,4.080727683096045e-16},{2230409900758597,4.119862377480744e-16},
	 {2230833995044585,4.1585816580828064e-16},{2231238597816133,4.1969036444740733e-16},
	 {2231624991250191,4.234845407152071e-16},{2231994346765928,4.272423051889976e-16},
	 {2232347736722750,4.309651795716294e-16},{2232686144665934,4.346546035512876e-16},
	 {2233010474325959,4.383119410085457e-16},{2233321557544881,4.4193848564470665e-16},
	 {2233620161276071,4.455354660957914e-16},{2233906993781271,4.491040505882875e-16},
	 {2234182710130335,4.52645351185714e-16},{2234447917093496,4.561604276690038e-16},
	 {2234703177503020,4.596502910884941e-16},{2234949014150181,4.631159070208165e-16},
	 {2235185913274316,4.665581985600875e-16},{2235414327692884,4.699780490694195e-16},
	 {2235634679614920,4.733763047158324e-16},{2235847363174595,4.767537768090853e-16},
	 {2236052746716837,4.8011124396270155e-16},{2236251174862869,4.834494540935008e-16},
	 {2236442970379967,4.867691262742209e-16},{2236628435876762,4.900709524522994e-16},
	 {2236807855342765,4.933555990465414e-16},{2236981495548562,4.966237084322178e-16},
	 {2237149607321147,4.998759003240909e-16},{2237312426707209,5.031127730659319e-16},
	 {2237470176035652,5.0633490483427195e-16},{2237623064889403,5.095428547633892e-16},
	 {2237771290995388,5.127371639978797e-16},{2237915041040597,5.159183566785736e-16},
	 {2238054491421305,5.190869408670343e-16},{2238189808931712,5.222434094134042e-16},
	 {2238321151397660,5.253882407719454e-16},{2238448668260432,5.285218997682382e-16},
	 {2238572501115169,5.316448383216618e-16},{2238692784207942,5.34757496126473e-16},
	 {2238809644895133,5.378603012945235e-16},{2238923204068402,5.409536709623993e-16},
	 {2239033576548190,5.440380118655467e-16},{2239140871448443,5.471137208817361e-16},
	 {2239245192514958,5.501811855460336e-16},{2239346638439541,5.532407845392784e-16},
	 {2239445303151952,5.56292888151909e-16},{2239541276091442,5.593378587248462e-16},
	 {2239634642459498,5.623760510690043e-16},{2239725483455293,5.65407812864896e-16},
	 {2239813876495186,5.684334850436814e-16},{2239899895417494,5.714534021509204e-16},
	 {2239983610673676,5.744678926941961e-16},{2240065089506935,5.774772794756965e-16},
	 {2240144396119183,5.804818799107686e-16},{2240221591827230,5.834820063333892e-16},
	 {2240296735208969,5.864779662894365e-16},{2240369882240293,5.894700628185872e-16},
	 {2240441086423386,5.924585947256134e-16},{2240510398907004,5.95443856841806e-16},
	 {2240577868599305,5.984261402772028e-16},{2240643542273726,6.014057326642664e-16},
	 {2240707464668391,6.043829183936125e-16},{2240769678579486,6.073579788423606e-16},
	 {2240830224948980,6.103311925956439e-16},{2240889142947082,6.133028356617911e-16},
	 {2240946470049769,6.162731816816596e-16},{2241002242111691,6.192425021325847e-16},
	 {2241056493434746,6.222110665273788e-16},{2241109256832602,6.251791426088e-16},
	 {2241160563691400,6.281469965398895e-16},{2241210444026879,6.311148930905604e-16},
	 {2241258926538122,6.34083095820806e-16},{2241306038658137,6.370518672608815e-16},
	 {2241351806601435,6.400214690888025e-16},{2241396255408788,6.429921623054896e-16},
	 {2241439408989313,6.459642074078832e-16},{2241481290160038,6.489378645603397e-16},
	 {2241521920683062,6.519133937646159e-16},{2241561321300462,6.548910550287415e-16},
	 {2241599511767028,6.578711085350741e-16},{2241636510880960,6.608538148078259e-16},
	 {2241672336512612,6.638394348803506e-16},{2241707005631362,6.668282304624746e-16},
	 {2241740534330713,6.698204641081558e-16},{2241772937851689,6.728163993837531e-16},
	 {2241804230604585,6.758163010371901e-16},{2241834426189161,6.78820435168298e-16},
	 {2241863537413311,6.818290694006254e-16},{2241891576310281,6.848424730550038e-16},
	 {2241918554154466,6.878609173251664e-16},{2241944481475843,6.908846754557169e-16},
	 {2241969368073071,6.939140229227569e-16},{2241993223025298,6.969492376174829e-16},
	 {2242016054702685,6.999906000330764e-16},{2242037870775710,7.030383934552151e-16},
	 {2242058678223225,7.060929041565482e-16},{2242078483339331,7.091544215954873e-16},
	 {2242097291739040,7.122232386196779e-16},{2242115108362774,7.152996516745303e-16},
	 {2242131937479672,7.183839610172063e-16},{2242147782689725,7.214764709364707e-16},
	 {2242162646924736,7.245774899788387e-16},{2242176532448092,7.276873311814693e-16},
	 {2242189440853337,7.308063123122743e-16},{2242201373061537,7.339347561177405e-16},
	 {2242212329317416,7.370729905789831e-16},{2242222309184237,7.4022134917658e-16},
	 {2242231311537397,7.433801711647648e-16},{2242239334556717,7.465498018555889e-16},
	 {2242246375717369,7.497305929136979e-16},{2242252431779415,7.529229026624058e-16},
	 {2242257498775893,7.561270964017922e-16},{2242261571999416,7.5934354673958895e-16},
	 {2242264645987196,7.625726339356756e-16},{2242266714504453,7.658147462610487e-16},
	 {2242267770526109,7.690702803721919e-16},{2242267806216711,7.723396417018299e-16},
	 {2242266812908462,7.756232448671174e-16},{2242264781077289,7.789215140963852e-16},
	 {2242261700316818,7.822348836756411e-16},{2242257559310145,7.855637984161084e-16},
	 {2242252345799276,7.889087141441755e-16},{2242246046552082,7.922700982152271e-16},
	 {2242238647326615,7.956484300529366e-16},{2242230132832625,7.99044201715713e-16},
	 {2242220486690076,8.024579184921259e-16},{2242209691384458,8.058900995272657e-16},
	 {2242197728218684,8.093412784821501e-16},{2242184577261310,8.128120042284501e-16},
	 {2242170217290819,8.163028415809877e-16},{2242154625735679,8.198143720706533e-16},
	 {2242137778609839,8.23347194760605e-16},{2242119650443327,8.26901927108847e-16},
	 {2242100214207556,8.304792058805374e-16},{2242079441234906,8.340796881136629e-16},
	 {2242057301132135,8.377040521420222e-16},{2242033761687079,8.413529986798028e-16},
	 {2242008788768107,8.450272519724097e-16},{2241982346215682,8.487275610186155e-16},
	 {2241954395725356,8.524547008695596e-16},{2241924896721443,8.562094740106233e-16},
	 {2241893806220517,8.599927118327665e-16},{2241861078683830,8.638052762005259e-16},
	 {2241826665857598,8.676480611245582e-16},{2241790516600041,8.715219945473698e-16},
	 {2241752576693881,8.754280402517175e-16},{2241712788642916,8.793671999021043e-16},
	 {2241671091451078,8.833405152308408e-16},{2241627420382235,8.873490703813135e-16},
	 {2241581706698773,8.913939944224086e-16},{2241533877376767,8.954764640495068e-16},
	 {2241483854795281,8.9959770648911e-16},{2241431556397035,9.037590026260118e-16},
	 {2241376894317345,9.079616903740068e-16},{2241319774977817,9.122071683134846e-16},
	 {2241260098640860,9.164968996219135e-16},{2241197758920538,9.208324163262308e-16},
	 {2241132642244704,9.252153239095693e-16},{2241064627262652,9.296473063086417e-16},
	 {2240993584191742,9.341301313425265e-16},{2240919374095536,9.38665656618666e-16},
	 {2240841848084890,9.432558359676707e-16},{2240760846432232,9.479027264651738e-16},
	 {2240676197587784,9.526084961066279e-16},{2240587717084782,9.57375432209745e-16},
	 {2240495206318753,9.622059506294838e-16},{2240398451183567,9.671026058823054e-16},
	 {2240297220544165,9.720681022901626e-16},{2240191264522612,9.771053062707209e-16},
	 {2240080312570155,9.822172599190541e-16},{2239964071293331,9.874071960480671e-16},
	 {2239842221996530,9.926785548807976e-16},{2239714417896699,9.980350026183645e-16},
	 {2239580280957725,1.003480452143618e-15},{2239439398282193,1.0090190861637457e-15},
	 {2239291317986196,1.0146553831467086e-15},{2239135544468203,1.0203941464683124e-15},
	 {2238971532964979,1.0262405372613567e-15},{2238798683265269,1.0322001115486456e-15},
	 {2238616332424351,1.03827886235154e-15},{2238423746288095,1.044483267600047e-15},
	 {2238220109591890,1.0508203448355195e-15},{2238004514345216,1.057297713900989e-15},
	 {2237775946143212,1.06392366906768e-15},{2237533267957822,1.0707072623632994e-15},
	 {2237275200846753,1.0776584002668106e-15},{2237000300869952,1.0847879564403425e-15},
	 {2236706931309099,1.0921079038149563e-15},{2236393229029147,1.0996314701785628e-15},
	 {2236057063479501,1.1073733224935752e-15},{2235695986373246,1.1153497865853155e-15},
	 {2235307169458859,1.1235791107110833e-15},{2234887326941578,1.1320817840164846e-15},
	 {2234432617919447,1.140880924258278e-15},{2233938522519765,1.1500027537839792e-15},
	 {2233399683022677,1.159477189144919e-15},{2232809697779198,1.169338578691096e-15},
	 {2232160850599817,1.17962663529558e-15},{2231443750584641,1.190387629928289e-15},
	 {2230646845562170,1.2016759392543819e-15},{2229755753817986,1.2135560818666897e-15},
	 {2228752329126533,1.2261054417450561e-15},{2227613325162504,1.2394179789163251e-15},
	 {2226308442121174,1.2536093926602567e-15},{2224797391720399,1.268824481425501e-15},
	 {2223025347823832,1.2852479319096109e-15},{2220915633329809,1.3031206634689985e-15},
	 {2218357446087030,1.3227655770195326e-15},{2215184158448668,1.3446300925011171e-15},
	 {2211132412537369,1.3693606835128518e-15},{2205758503851065,1.397943667277524e-15},
	 {2198248265654987,1.4319989869661328e-15},{2186916352102141,1.4744848603597596e-15},
	 {2167562552481814,1.5317872741611144e-15},{2125549880839716,1.6227698675312968e-15}}).

normal_fi(Indx) ->
    element(Indx,
	    {1.0000000000000000e+00,9.7710170126767082e-01,9.5987909180010600e-01,
	     9.4519895344229909e-01,9.3206007595922991e-01,9.1999150503934646e-01,
	     9.0872644005213032e-01,8.9809592189834297e-01,8.8798466075583282e-01,
	     8.7830965580891684e-01,8.6900868803685649e-01,8.6003362119633109e-01,
	     8.5134625845867751e-01,8.4291565311220373e-01,8.3471629298688299e-01,
	     8.2672683394622093e-01,8.1892919160370192e-01,8.1130787431265572e-01,
	     8.0384948317096383e-01,7.9654233042295841e-01,7.8937614356602404e-01,
	     7.8234183265480195e-01,7.7543130498118662e-01,7.6863731579848571e-01,
	     7.6195334683679483e-01,7.5537350650709567e-01,7.4889244721915638e-01,
	     7.4250529634015061e-01,7.3620759812686210e-01,7.2999526456147568e-01,
	     7.2386453346862967e-01,7.1781193263072152e-01,7.1183424887824798e-01,
	     7.0592850133275376e-01,7.0009191813651117e-01,6.9432191612611627e-01,
	     6.8861608300467136e-01,6.8297216164499430e-01,6.7738803621877308e-01,
	     6.7186171989708166e-01,6.6639134390874977e-01,6.6097514777666277e-01,
	     6.5561147057969693e-01,6.5029874311081637e-01,6.4503548082082196e-01,
	     6.3982027745305614e-01,6.3465179928762327e-01,6.2952877992483625e-01,
	     6.2445001554702606e-01,6.1941436060583399e-01,6.1442072388891344e-01,
	     6.0946806492577310e-01,6.0455539069746733e-01,5.9968175261912482e-01,
	     5.9484624376798689e-01,5.9004799633282545e-01,5.8528617926337090e-01,
	     5.8055999610079034e-01,5.7586868297235316e-01,5.7121150673525267e-01,
	     5.6658776325616389e-01,5.6199677581452390e-01,5.5743789361876550e-01,
	     5.5291049042583185e-01,5.4841396325526537e-01,5.4394773119002582e-01,
	     5.3951123425695158e-01,5.3510393238045717e-01,5.3072530440366150e-01,
	     5.2637484717168403e-01,5.2205207467232140e-01,5.1775651722975591e-01,
	     5.1348772074732651e-01,5.0924524599574761e-01,5.0502866794346790e-01,
	     5.0083757512614835e-01,4.9667156905248933e-01,4.9253026364386815e-01,
	     4.8841328470545758e-01,4.8432026942668288e-01,4.8025086590904642e-01,
	     4.7620473271950547e-01,4.7218153846772976e-01,4.6818096140569321e-01,
	     4.6420268904817391e-01,4.6024641781284248e-01,4.5631185267871610e-01,
	     4.5239870686184824e-01,4.4850670150720273e-01,4.4463556539573912e-01,
	     4.4078503466580377e-01,4.3695485254798533e-01,4.3314476911265209e-01,
	     4.2935454102944126e-01,4.2558393133802180e-01,4.2183270922949573e-01,
	     4.1810064983784795e-01,4.1438753404089090e-01,4.1069314827018799e-01,
	     4.0701728432947315e-01,4.0335973922111429e-01,3.9972031498019700e-01,
	     3.9609881851583223e-01,3.9249506145931540e-01,3.8890886001878855e-01,
	     3.8534003484007706e-01,3.8178841087339344e-01,3.7825381724561896e-01,
	     3.7473608713789086e-01,3.7123505766823922e-01,3.6775056977903225e-01,
	     3.6428246812900372e-01,3.6083060098964775e-01,3.5739482014578022e-01,
	     3.5397498080007656e-01,3.5057094148140588e-01,3.4718256395679348e-01,
	     3.4380971314685055e-01,3.4045225704452164e-01,3.3711006663700588e-01,
	     3.3378301583071823e-01,3.3047098137916342e-01,3.2717384281360129e-01,
	     3.2389148237639104e-01,3.2062378495690530e-01,3.1737063802991350e-01,
	     3.1413193159633707e-01,3.1090755812628634e-01,3.0769741250429189e-01,
	     3.0450139197664983e-01,3.0131939610080288e-01,2.9815132669668531e-01,
	     2.9499708779996164e-01,2.9185658561709499e-01,2.8872972848218270e-01,
	     2.8561642681550159e-01,2.8251659308370741e-01,2.7943014176163772e-01,
	     2.7635698929566810e-01,2.7329705406857691e-01,2.7025025636587519e-01,
	     2.6721651834356114e-01,2.6419576399726080e-01,2.6118791913272082e-01,
	     2.5819291133761890e-01,2.5521066995466168e-01,2.5224112605594190e-01,
	     2.4928421241852824e-01,2.4633986350126363e-01,2.4340801542275012e-01,
	     2.4048860594050039e-01,2.3758157443123795e-01,2.3468686187232990e-01,
	     2.3180441082433859e-01,2.2893416541468023e-01,2.2607607132238020e-01,
	     2.2323007576391746e-01,2.2039612748015194e-01,2.1757417672433113e-01,
	     2.1476417525117358e-01,2.1196607630703015e-01,2.0917983462112499e-01,
	     2.0640540639788071e-01,2.0364274931033485e-01,2.0089182249465656e-01,
	     1.9815258654577511e-01,1.9542500351413428e-01,1.9270903690358912e-01,
	     1.9000465167046496e-01,1.8731181422380025e-01,1.8463049242679927e-01,
	     1.8196065559952254e-01,1.7930227452284767e-01,1.7665532144373500e-01,
	     1.7401977008183875e-01,1.7139559563750595e-01,1.6878277480121151e-01,
	     1.6618128576448205e-01,1.6359110823236570e-01,1.6101222343751107e-01,
	     1.5844461415592431e-01,1.5588826472447920e-01,1.5334316106026283e-01,
	     1.5080929068184568e-01,1.4828664273257453e-01,1.4577520800599403e-01,
	     1.4327497897351341e-01,1.4078594981444470e-01,1.3830811644855071e-01,
	     1.3584147657125373e-01,1.3338602969166913e-01,1.3094177717364430e-01,
	     1.2850872227999952e-01,1.2608687022018586e-01,1.2367622820159654e-01,
	     1.2127680548479021e-01,1.1888861344290998e-01,1.1651166562561080e-01,
	     1.1414597782783835e-01,1.1179156816383801e-01,1.0944845714681163e-01,
	     1.0711666777468364e-01,1.0479622562248690e-01,1.0248715894193508e-01,
	     1.0018949876880981e-01,9.7903279038862284e-02,9.5628536713008819e-02,
	     9.3365311912690860e-02,9.1113648066373634e-02,8.8873592068275789e-02,
	     8.6645194450557961e-02,8.4428509570353374e-02,8.2223595813202863e-02,
	     8.0030515814663056e-02,7.7849336702096039e-02,7.5680130358927067e-02,
	     7.3522973713981268e-02,7.1377949058890375e-02,6.9245144397006769e-02,
	     6.7124653827788497e-02,6.5016577971242842e-02,6.2921024437758113e-02,
	     6.0838108349539864e-02,5.8767952920933758e-02,5.6710690106202902e-02,
	     5.4666461324888914e-02,5.2635418276792176e-02,5.0617723860947761e-02,
	     4.8613553215868521e-02,4.6623094901930368e-02,4.4646552251294443e-02,
	     4.2684144916474431e-02,4.0736110655940933e-02,3.8802707404526113e-02,
	     3.6884215688567284e-02,3.4980941461716084e-02,3.3093219458578522e-02,
	     3.1221417191920245e-02,2.9365939758133314e-02,2.7527235669603082e-02,
	     2.5705804008548896e-02,2.3902203305795882e-02,2.2117062707308864e-02,
	     2.0351096230044517e-02,1.8605121275724643e-02,1.6880083152543166e-02,
	     1.5177088307935325e-02,1.3497450601739880e-02,1.1842757857907888e-02,
	     1.0214971439701471e-02,8.6165827693987316e-03,7.0508754713732268e-03,
	     5.5224032992509968e-03,4.0379725933630305e-03,2.6090727461021627e-03,
	     1.2602859304985975e-03}).

%%%bitcount64(0) -> 0;
%%%bitcount64(V) -> 1 + bitcount(V, 64).
%%%
%%%-define(
%%%   BITCOUNT(V, N),
%%%   bitcount(V, N) ->
%%%       if
%%%           (1 bsl ((N) bsr 1)) =< (V) ->
%%%               ((N) bsr 1) + bitcount((V) bsr ((N) bsr 1), ((N) bsr 1));
%%%           true ->
%%%               bitcount((V), ((N) bsr 1))
%%%       end).
%%%?BITCOUNT(V, 64);
%%%?BITCOUNT(V, 32);
%%%?BITCOUNT(V, 16);
%%%?BITCOUNT(V, 8);
%%%?BITCOUNT(V, 4);
%%%?BITCOUNT(V, 2);
%%%bitcount(_, 1) -> 0.

bc64(V) -> ?BC(V, 64).

%% Linear from high bit - higher probability first gives faster execution
bc(V, B, N) when B =< V -> N;
bc(V, B, N) -> bc(V, B bsr 1, N - 1).
    
make_float(S, E, M) ->
    <<F/float>> = <<S:1, E:11, M:52>>,
    F.

float2str(N) ->
    <<S:1, E:11, M:52>> = <<(float(N))/float>>,
    lists:flatten(
      io_lib:format(
      "~c~c.~13.16.0bE~b",
      [case S of 1 -> $-; 0 -> $+ end,
       case E of 0 -> $0; _ -> $1 end,
       M, E - 16#3ff])).
