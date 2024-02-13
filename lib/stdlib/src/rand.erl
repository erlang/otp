%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2024. All Rights Reserved.
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
-moduledoc """
Pseudo random number generation.

This module provides a pseudo random number generator. The module contains a
number of algorithms. The uniform distribution algorithms are based on the
[Xoroshiro and Xorshift algorithms ](http://xorshift.di.unimi.it)by Sebastiano
Vigna. The normal distribution algorithm uses the
[Ziggurat Method by Marsaglia and Tsang ](http://www.jstatsoft.org/v05/i08)on
top of the uniform distribution algorithm.

For most algorithms, jump functions are provided for generating non-overlapping
sequences for parallel computations. The jump functions perform calculations
equivalent to perform a large number of repeated calls for calculating new
states, but execute in a time roughly equivalent to one regular iteration per
generator bit.

At the end of this module documentation there are also some
[niche algorithms ](`m:rand#niche-algorithms-api`)to be used without this
module's normal [plug-in framework API ](`m:rand#plug-in-framework-api`)that may
be useful for special purposes like short generation time when quality is not
essential, for seeding other generators, and such.

[](){: #algorithms } The following algorithms are provided:

- **`exsss`**(Since OTP 22.0)  
  Xorshift116\*\*, 58 bits precision and period of 2^116-1

  Jump function: equivalent to 2^64 calls

  This is the Xorshift116 generator combined with the StarStar scrambler from
  the 2018 paper by David Blackman and Sebastiano Vigna:
  [Scrambled Linear Pseudorandom Number Generators](http://vigna.di.unimi.it/ftp/papers/ScrambledLinear.pdf)

  The generator does not need 58-bit rotates so it is faster than the
  Xoroshiro116 generator, and when combined with the StarStar scrambler it does
  not have any weak low bits like `exrop` (Xoroshiro116+).

  Alas, this combination is about 10% slower than `exrop`, but is despite that
  the [_default algorithm_ ](`m:rand#default-algorithm`)thanks to its
  statistical qualities.

- **`exro928ss`**(Since OTP 22.0)  
  Xoroshiro928\*\*, 58 bits precision and a period of 2^928-1

  Jump function: equivalent to 2^512 calls

  This is a 58 bit version of Xoroshiro1024\**, from the 2018 paper by David
  Blackman and Sebastiano Vigna:
  [Scrambled Linear Pseudorandom Number Generators ](http://vigna.di.unimi.it/ftp/papers/ScrambledLinear.pdf)that
  on a 64 bit Erlang system executes only about 40% slower than the [*default*
  `exsss` *algorithm\* ](`m:rand#default-algorithm`)but with much longer period
  and better statistical properties, but on the flip side a larger state.

  Many thanks to Sebastiano Vigna for his help with the 58 bit adaption.

- **`exrop`**(Since OTP 20.0)  
  Xoroshiro116+, 58 bits precision and period of 2^116-1

  Jump function: equivalent to 2^64 calls

- **`exs1024s`**(Since OTP 20.0)  
  Xorshift1024\*, 64 bits precision and a period of 2^1024-1

  Jump function: equivalent to 2^512 calls

- **`exsp`**(Since OTP 20.0)  
  Xorshift116+, 58 bits precision and period of 2^116-1

  Jump function: equivalent to 2^64 calls

  This is a corrected version of the previous
  [_default algorithm_, ](`m:rand#default-algorithm`)that now has been
  superseded by Xoroshiro116+ (`exrop`). Since there is no native 58 bit rotate
  instruction this algorithm executes a little (say < 15%) faster than `exrop`.
  See the [algorithms' homepage](http://xorshift.di.unimi.it).

[](){: #default-algorithm } The current _default algorithm_ is
[`exsss` (Xorshift116\*\*). ](`m:rand#algorithms`)If a specific algorithm is
required, ensure to always use `seed/1` to initialize the state.

Which algorithm that is the default may change between Erlang/OTP releases, and
is selected to be one with high speed, small state and "good enough" statistical
properties.

Undocumented (old) algorithms are deprecated but still implemented so old code
relying on them will produce the same pseudo random sequences as before.

> #### Note {: .info }
>
> There were a number of problems in the implementation of the now undocumented
> algorithms, which is why they are deprecated. The new algorithms are a bit
> slower but do not have these problems:
>
> Uniform integer ranges had a skew in the probability distribution that was not
> noticable for small ranges but for large ranges less than the generator's
> precision the probability to produce a low number could be twice the
> probability for a high.
>
> Uniform integer ranges larger than or equal to the generator's precision used
> a floating point fallback that only calculated with 52 bits which is smaller
> than the requested range and therefore were not all numbers in the requested
> range even possible to produce.
>
> Uniform floats had a non-uniform density so small values i.e less than 0.5 had
> got smaller intervals decreasing as the generated value approached 0.0
> although still uniformly distributed for sufficiently large subranges. The new
> algorithms produces uniformly distributed floats on the form N \* 2.0^(-53)
> hence equally spaced.

Every time a random number is requested, a state is used to calculate it and a
new state is produced. The state can either be implicit or be an explicit
argument and return value.

The functions with implicit state use the process dictionary variable
`rand_seed` to remember the current state.

If a process calls `uniform/0`, `uniform/1` or `uniform_real/0` without setting
a seed first, `seed/1` is called automatically with the
[_default algorithm_ ](`m:rand#default-algorithm`)and creates a non-constant
seed.

The functions with explicit state never use the process dictionary.

_Examples:_

Simple use; creates and seeds the
[_default algorithm_ ](`m:rand#default-algorithm`)with a non-constant seed if
not already done:

```text
R0 = rand:uniform(),
R1 = rand:uniform(),
```

Use a specified algorithm:

```text
_ = rand:seed(exs928ss),
R2 = rand:uniform(),
```

Use a specified algorithm with a constant seed:

```erlang
_ = rand:seed(exs928ss, {123, 123534, 345345}),
R3 = rand:uniform(),
```

Use the functional API with a non-constant seed:

```text
S0 = rand:seed_s(exsss),
{R4, S1} = rand:uniform_s(S0),
```

Textbook basic form Box-Muller standard normal deviate

```erlang
R5 = rand:uniform_real(),
R6 = rand:uniform(),
SND0 = math:sqrt(-2 * math:log(R5)) * math:cos(math:pi() * R6)
```

Create a standard normal deviate:

```text
{SND1, S2} = rand:normal_s(S1),
```

Create a normal deviate with mean -3 and variance 0.5:

```text
{ND0, S3} = rand:normal_s(-3, 0.5, S2),
```

> #### Note {: .info }
>
> The builtin random number generator algorithms are not cryptographically
> strong. If a cryptographically strong random number generator is needed, use
> something like `crypto:rand_seed/0`.

For all these generators except `exro928ss` and `exsss` the lowest bit(s) has
got a slightly less random behaviour than all other bits. 1 bit for `exrop` (and
`exsp`), and 3 bits for `exs1024s`. See for example the explanation in the
[Xoroshiro128+ ](http://xoroshiro.di.unimi.it/xoroshiro128plus.c)generator
source code:

```text
Beside passing BigCrush, this generator passes the PractRand test suite
up to (and included) 16TB, with the exception of binary rank tests,
which fail due to the lowest bit being an LFSR; all other bits pass all
tests. We suggest to use a sign test to extract a random Boolean value.
```

If this is a problem; to generate a boolean with these algorithms use something
like this:

```erlang
(rand:uniform(256) > 128) % -> boolean()
```

```text
((rand:uniform(256) - 1) bsr 7) % -> 0 | 1
```

For a general range, with `N = 1` for `exrop`, and `N = 3` for `exs1024s`:

```text
(((rand:uniform(Range bsl N) - 1) bsr N) + 1)
```

The floating point generating functions in this module waste the lowest bits
when converting from an integer so they avoid this snag.
""".
-moduledoc(#{since => "OTP 18.0",
             titles =>
                 [{function,<<"Plug-in framework API">>},
                  {function,<<"Niche algorithms API">>}]}).

-export([seed_s/1, seed_s/2, seed/1, seed/2,
	 export_seed/0, export_seed_s/1,
         uniform/0, uniform/1, uniform_s/1, uniform_s/2,
         uniform_real/0, uniform_real_s/1,
         bytes/1, bytes_s/2,
         jump/0, jump/1,
         normal/0, normal/2, normal_s/1, normal_s/3
	]).

%% Utilities
-export([exsp_next/1, exsp_jump/1, splitmix64_next/1,
         mwc59/1, mwc59_value32/1, mwc59_value/1, mwc59_float/1,
         mwc59_seed/0, mwc59_seed/1]).

%% Test, dev and internal
-export([exro928_jump_2pow512/1, exro928_jump_2pow20/1,
	 exro928_seed/1, exro928_next/1, exro928_next_state/1,
	 format_jumpconst58/1, seed58/2]).

%% Debug
-export([make_float/3, float2str/1, bc64/1]).

-compile({inline, [exs64_next/1, exsp_next/1, exsss_next/1,
		   exs1024_next/1, exs1024_calc/2,
                   exro928_next_state/4,
                   exrop_next/1, exrop_next_s/2,
                   mwc59_value/1,
		   get_52/1, normal_kiwi/1]}).

-define(DEFAULT_ALG_HANDLER, exsss).
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

-doc "0 .. (2^64 - 1)".
-type uint64() :: 0..?MASK(64).
-doc "0 .. (2^58 - 1)".
-type uint58() :: 0..?MASK(58).

%% This depends on the algorithm handler function
-type alg_state() ::
	exsplus_state() | exro928_state() |  exrop_state() | exs1024_state() |
	exs64_state() | dummy_state() | term().

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
-doc "Algorithm-dependent state.".
-type state() :: {alg_handler(), alg_state()}.
-type builtin_alg() ::
	exsss | exro928ss | exrop | exs1024s | exsp | exs64 | exsplus |
        exs1024 | dummy.
-type alg() :: builtin_alg() | atom().
-doc "Algorithm-dependent state that can be printed or saved to file.".
-type export_state() :: {alg(), alg_state()}.
-doc """
A seed value for the generator.

A list of integers sets the generator's internal state directly, after
algorithm-dependent checks of the value and masking to the proper word size. The
number of integers must be equal to the number of state words in the generator.

An integer is used as the initial state for a SplitMix64 generator. The output
values of that is then used for setting the generator's internal state after
masking to the proper word size and if needed avoiding zero values.

A traditional 3-tuple of integers seed is passed through algorithm-dependent
hashing functions to create the generator's initial state.
""".
-type seed() :: [integer()] | integer() | {integer(), integer(), integer()}.
-export_type(
   [builtin_alg/0, alg/0, alg_handler/0, alg_state/0,
    state/0, export_state/0, seed/0]).
-export_type(
   [exsplus_state/0, exro928_state/0, exrop_state/0, exs1024_state/0,
    exs64_state/0, mwc59_state/0, dummy_state/0]).
-export_type(
   [uint58/0, uint64/0, splitmix64_state/0]).

%% =====================================================================
%% Range macro and helper
%% =====================================================================

-define(
   uniform_range(Range, AlgHandler, R, V, MaxMinusRange, I),
   if
       0 =< (MaxMinusRange) ->
           if
               %% Really work saving in odd cases;
               %% large ranges in particular
               (V) < (Range) ->
                   {(V) + 1, {(AlgHandler), (R)}};
               true ->
                   (I) = (V) rem (Range),
                   if
                       (V) - (I) =< (MaxMinusRange) ->
                           {(I) + 1, {(AlgHandler), (R)}};
                       true ->
                           %% V in the truncated top range
                           %% - try again
                           ?FUNCTION_NAME((Range), {(AlgHandler), (R)})
                   end
           end;
       true ->
           uniform_range((Range), (AlgHandler), (R), (V))
   end).

%% For ranges larger than the algorithm bit size
uniform_range(Range, #{next:=Next, bits:=Bits} = AlgHandler, R, V) ->
    WeakLowBits = maps:get(weak_low_bits, AlgHandler, 0),
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
            {(V1 band RangeMinus1) + 1, {AlgHandler, R1}};
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
                    {I + 1, {AlgHandler, R1}};
                true ->
                    %% V1 drawn from the truncated top range
                    %% - try again
                    {V2, R2} = Next(R1),
                    uniform_range(Range, AlgHandler, R2, V2)
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
-doc "Returns the random number state in an external format. To be used with `seed/1`.".
-doc(#{title => <<"Plug-in framework API">>,since => <<"OTP 18.0">>}).
-spec export_seed() -> 'undefined' | export_state().
export_seed() ->
    case get(?SEED_DICT) of
	{#{type:=Alg}, AlgState} -> {Alg, AlgState};
	_ -> undefined
    end.

-doc """
Returns the random number generator state in an external format. To be used with
`seed/1`.
""".
-doc(#{title => <<"Plug-in framework API">>,since => <<"OTP 18.0">>}).
-spec export_seed_s(State :: state()) -> export_state().
export_seed_s({#{type:=Alg}, AlgState}) -> {Alg, AlgState}.

%% seed(Alg) seeds RNG with runtime dependent values
%% and return the NEW state

%% seed({Alg,AlgState}) setup RNG with a previously exported seed
%% and return the NEW state

-doc """
Seeds random number generation with the specifed algorithm and time-dependent
data if `AlgOrStateOrExpState` is an algorithm. `Alg = default` is an alias for
the [_default algorithm_.](`m:rand#default-algorithm`)

Otherwise recreates the exported seed in the process dictionary, and returns the
state. See also `export_seed/0`.
""".
-doc(#{title => <<"Plug-in framework API">>,since => <<"OTP 18.0,OTP 24.0">>}).
-spec seed(
        AlgOrStateOrExpState :: builtin_alg() | state() | export_state()) ->
                  state();
          (Alg :: 'default') ->
                  state().
seed(Alg) ->
    seed_put(seed_s(Alg)).

-doc """
Seeds random number generation with the specifed algorithm and time-dependent
data if `AlgOrStateOrExpState` is an algorithm. `Alg = default` is an alias for
the [_default algorithm_.](`m:rand#default-algorithm`)

Otherwise recreates the exported seed and returns the state. See also
`export_seed/0`.
""".
-doc(#{title => <<"Plug-in framework API">>,since => <<"OTP 18.0,OTP 24.0">>}).
-spec seed_s(
        AlgOrStateOrExpState :: builtin_alg() | state() | export_state()) ->
                    state();
            (Alg :: 'default') ->
                    state().
seed_s({AlgHandler, _AlgState} = State) when is_map(AlgHandler) ->
    State;
seed_s({Alg, AlgState}) when is_atom(Alg) ->
    {AlgHandler,_SeedFun} = mk_alg(Alg),
    {AlgHandler,AlgState};
seed_s(Alg) ->
    seed_s(Alg, default_seed()).

default_seed() ->
    {erlang:phash2([{node(),self()}]),
     erlang:system_time(),
     erlang:unique_integer()}.

%% seed/2: seeds RNG with the algorithm and given values
%% and returns the NEW state.

-doc """
Seeds random number generation with the specified algorithm and integers in the
process dictionary and returns the state. `Alg = default` is an alias for the
[_default algorithm_.](`m:rand#default-algorithm`)
""".
-doc(#{title => <<"Plug-in framework API">>,since => <<"OTP 18.0,OTP 24.0">>}).
-spec seed(Alg :: builtin_alg(),  Seed :: seed()) -> state();
          (Alg :: 'default',  Seed :: seed()) -> state().
seed(Alg, Seed) ->
    seed_put(seed_s(Alg, Seed)).

-doc """
Seeds random number generation with the specified algorithm and integers and
returns the state. `Alg = default` is an alias for the
[_default algorithm_.](`m:rand#default-algorithm`)
""".
-doc(#{title => <<"Plug-in framework API">>,since => <<"OTP 18.0,OTP 24.0">>}).
-spec seed_s(Alg :: builtin_alg(), Seed :: seed()) -> state();
            (Alg :: 'default', Seed :: seed()) -> state().
seed_s(default, Seed) -> seed_s(?DEFAULT_ALG_HANDLER, Seed);
seed_s(Alg, Seed) ->
    {AlgHandler,SeedFun} = mk_alg(Alg),
    AlgState = SeedFun(Seed),
    {AlgHandler,AlgState}.

%%% uniform/0, uniform/1, uniform_s/1, uniform_s/2 are all
%%% uniformly distributed random numbers.

%% uniform/0: returns a random float X where 0.0 =< X < 1.0,
%% updating the state in the process dictionary.

-doc """
Returns a random float uniformly distributed in the value range `0.0 =< X < 1.0`
and updates the state in the process dictionary.

The generated numbers are on the form N \* 2.0^(-53), that is; equally spaced in
the interval.

> #### Warning {: .warning }
>
> This function may return exactly `0.0` which can be fatal for certain
> applications. If that is undesired you can use `(1.0 - rand:uniform())` to get
> the interval `0.0 < X =< 1.0`, or instead use `uniform_real/0`.
>
> If neither endpoint is desired you can test and re-try like this:
>
> ```erlang
> my_uniform() ->
>     case rand:uniform() of
>         0.0 -> my_uniform();
> 	X -> X
>     end
> end.
> ```
""".
-doc(#{title => <<"Plug-in framework API">>,since => <<"OTP 18.0">>}).
-spec uniform() -> X :: float().
uniform() ->
    {X, State} = uniform_s(seed_get()),
    _ = seed_put(State),
    X.

%% uniform/1: given an integer N >= 1,
%% uniform/1 returns a random integer X where 1 =< X =< N,
%% updating the state in the process dictionary.

-doc """
Returns, for a specified integer `N >= 1`, a random integer uniformly
distributed in the value range `1 =< X =< N` and updates the state in the
process dictionary.
""".
-doc(#{title => <<"Plug-in framework API">>,since => <<"OTP 18.0">>}).
-spec uniform(N :: pos_integer()) -> X :: pos_integer().
uniform(N) ->
    {X, State} = uniform_s(N, seed_get()),
    _ = seed_put(State),
    X.

%% uniform_s/1: given a state, uniform_s/1
%% returns a random float X where 0.0 =< X < 1.0,
%% and a new state.

-doc """
Returns, for a specified state, random float uniformly distributed in the value
range `0.0 =< X < 1.0` and a new state.

The generated numbers are on the form N \* 2.0^(-53), that is; equally spaced in
the interval.

> #### Warning {: .warning }
>
> This function may return exactly `0.0` which can be fatal for certain
> applications. If that is undesired you can use `(1.0 - rand:uniform(State))`
> to get the interval `0.0 < X =< 1.0`, or instead use `uniform_real_s/1`.
>
> If neither endpoint is desired you can test and re-try like this:
>
> ```erlang
> my_uniform(State) ->
>     case rand:uniform(State) of
>         {0.0, NewState} -> my_uniform(NewState);
> 	Result -> Result
>     end
> end.
> ```
""".
-doc(#{title => <<"Plug-in framework API">>,since => <<"OTP 18.0">>}).
-spec uniform_s(State :: state()) -> {X :: float(), NewState :: state()}.
uniform_s(State = {#{uniform:=Uniform}, _}) ->
    Uniform(State);
uniform_s({#{bits:=Bits, next:=Next} = AlgHandler, R0}) ->
    {V, R1} = Next(R0),
    %% Produce floats on the form N * 2^(-53)
    {(V bsr (Bits - 53)) * ?TWO_POW_MINUS53, {AlgHandler, R1}};
uniform_s({#{max:=Max, next:=Next} = AlgHandler, R0}) ->
    {V, R1} = Next(R0),
    %% Old algorithm with non-uniform density
    {V / (Max + 1), {AlgHandler, R1}}.


%% uniform_s/2: given an integer N >= 1 and a state, uniform_s/2
%% uniform_s/2 returns a random integer X where 1 =< X =< N,
%% and a new state.

-doc """
Returns, for a specified integer `N >= 1` and a state, a random integer
uniformly distributed in the value range `1 =< X =< N` and a new state.
""".
-doc(#{title => <<"Plug-in framework API">>,since => <<"OTP 18.0">>}).
-spec uniform_s(N :: pos_integer(), State :: state()) ->
                       {X :: pos_integer(), NewState :: state()}.
uniform_s(N, State = {#{uniform_n:=UniformN}, _})
  when is_integer(N), 1 =< N ->
    UniformN(N, State);
uniform_s(N, {#{bits:=Bits, next:=Next} = AlgHandler, R0})
  when is_integer(N), 1 =< N ->
    {V, R1} = Next(R0),
    MaxMinusN = ?BIT(Bits) - N,
    ?uniform_range(N, AlgHandler, R1, V, MaxMinusN, I);
uniform_s(N, {#{max:=Max, next:=Next} = AlgHandler, R0})
  when is_integer(N), 1 =< N ->
    %% Old algorithm with skewed probability
    %% and gap in ranges > Max
    {V, R1} = Next(R0),  
    if
        N =< Max ->
            {(V rem N) + 1, {AlgHandler, R1}};
        true ->
            F = V / (Max + 1),
            {trunc(F * N) + 1, {AlgHandler, R1}}
    end.

%% uniform_real/0: returns a random float X where 0.0 < X =< 1.0,
%% updating the state in the process dictionary.

-doc """
Returns a random float uniformly distributed in the value range
`DBL_MIN =< X < 1.0` and updates the state in the process dictionary.

Conceptually, a random real number `R` is generated from the interval
`0 =< R < 1` and then the closest rounded down normalized number in the IEEE 754
Double precision format is returned.

> #### Note {: .info }
>
> The generated numbers from this function has got better granularity for small
> numbers than the regular `uniform/0` because all bits in the mantissa are
> random. This property, in combination with the fact that exactly zero is never
> returned is useful for algorithms doing for example `1.0 / X` or
> `math:log(X)`.

See `uniform_real_s/1` for more explanation.
""".
-doc(#{title => <<"Plug-in framework API">>,since => <<"OTP 21.0">>}).
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
-doc """
Returns, for a specified state, a random float uniformly distributed in the
value range `DBL_MIN =< X < 1.0` and updates the state in the process
dictionary.

Conceptually, a random real number `R` is generated from the interval
`0 =< R < 1` and then the closest rounded down normalized number in the IEEE 754
Double precision format is returned.

> #### Note {: .info }
>
> The generated numbers from this function has got better granularity for small
> numbers than the regular `uniform_s/1` because all bits in the mantissa are
> random. This property, in combination with the fact that exactly zero is never
> returned is useful for algorithms doing for example `1.0 / X` or
> `math:log(X)`.

The concept implicates that the probability to get exactly zero is extremely
low; so low that this function is in fact guaranteed to never return zero. The
smallest number that it might return is `DBL_MIN`, which is 2.0^(-1022).

The value range stated at the top of this function description is technically
correct, but `0.0 =< X < 1.0` is a better description of the generated numbers'
statistical distribution. Except that exactly 0.0 is never returned, which is
not possible to observe statistically.

For example; for all sub ranges `N*2.0^(-53) =< X < (N+1)*2.0^(-53)` where
`0 =< integer(N) < 2.0^53` the probability is the same. Compare that with the
form of the numbers generated by `uniform_s/1`.

Having to generate extra random bits for small numbers costs a little
performance. This function is about 20% slower than the regular `uniform_s/1`
""".
-doc(#{title => <<"Plug-in framework API">>,since => <<"OTP 21.0">>}).
-spec uniform_real_s(State :: state()) -> {X :: float(), NewState :: state()}.
uniform_real_s({#{bits:=Bits, next:=Next} = AlgHandler, R0}) ->
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
            {(M1 bsr 3) * math:pow(2.0, -53), {AlgHandler, R1}};
        ?BIT(54) =< M1 ->
            %% We have 55 bits - waste 2
            {(M1 bsr 2) * math:pow(2.0, -54), {AlgHandler, R1}};
        ?BIT(53) =< M1 ->
            %% We have 54 bits - waste 1
            {(M1 bsr 1) * math:pow(2.0, -55), {AlgHandler, R1}};
        ?BIT(52) =< M1 ->
            %% We have 53 bits - use all
            {M1 * math:pow(2.0, -56), {AlgHandler, R1}};
        true ->
            %% Need more bits
            {V2, R2} = Next(R1),
            uniform_real_s(AlgHandler, Next, M1, -56, R2, V2, Bits)
    end;
uniform_real_s({#{max:=_, next:=Next} = AlgHandler, R0}) ->
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
            {(M1 bsr 3) * math:pow(2.0, -53), {AlgHandler, R1}};
        ?BIT(54) =< M1 ->
            %% We have 55 bits - waste 2
            {(M1 bsr 2) * math:pow(2.0, -54), {AlgHandler, R1}};
        ?BIT(53) =< M1 ->
            %% We have 54 bits - waste 1
            {(M1 bsr 1) * math:pow(2.0, -55), {AlgHandler, R1}};
        ?BIT(52) =< M1 ->
            %% We have 53 bits - use all
            {M1 * math:pow(2.0, -56), {AlgHandler, R1}};
        true ->
            %% Need more bits
            {V2, R2} = Next(R1),
            uniform_real_s(AlgHandler, Next, M1, -56, R2, V2, 56)
    end.

uniform_real_s(AlgHandler, _Next, M0, -1064, R1, V1, Bits) -> % 19*56
    %% This is a very theoretical bottom case.
    %% The odds of getting here is about 2^-1008,
    %% through a white box test case, or thanks to
    %% a malfunctioning PRNG producing 18 56-bit zeros in a row.
    %%
    %% Fill up to 53 bits, we have at most 52
    B0 = (53 - ?BC(M0, 52)), % Missing bits
    {(((M0 bsl B0) bor (V1 bsr (Bits - B0))) * math:pow(2.0, -1064 - B0)),
     {AlgHandler, R1}};
uniform_real_s(AlgHandler, Next, M0, BitNo, R1, V1, Bits) ->
    if
        %% Optimize the most probable.
        %% Fill up to 53 bits.
        ?BIT(51) =< M0 ->
            %% We have 52 bits in M0 - need 1
            {(((M0 bsl 1) bor (V1 bsr (Bits - 1)))
              * math:pow(2.0, BitNo - 1)),
             {AlgHandler, R1}};
        ?BIT(50) =< M0 ->
            %% We have 51 bits in M0 - need 2
            {(((M0 bsl 2) bor (V1 bsr (Bits - 2)))
              * math:pow(2.0, BitNo - 2)),
             {AlgHandler, R1}};
        ?BIT(49) =< M0 ->
            %% We have 50 bits in M0 - need 3
            {(((M0 bsl 3) bor (V1 bsr (Bits - 3)))
              * math:pow(2.0, BitNo - 3)),
             {AlgHandler, R1}};
        M0 == 0 ->
            M1 = V1 bsr (Bits - 56),
            if
                ?BIT(55) =< M1 ->
                    %% We have 56 bits - waste 3
                    {(M1 bsr 3) * math:pow(2.0, BitNo - 53),
                     {AlgHandler, R1}};
                ?BIT(54) =< M1 ->
                    %% We have 55 bits - waste 2
                    {(M1 bsr 2) * math:pow(2.0, BitNo - 54),
                     {AlgHandler, R1}};
                ?BIT(53) =< M1 ->
                    %% We have 54 bits - waste 1
                    {(M1 bsr 1) * math:pow(2.0, BitNo - 55),
                     {AlgHandler, R1}};
                ?BIT(52) =< M1 ->
                    %% We have 53 bits - use all
                    {M1 * math:pow(2.0, BitNo - 56),
                     {AlgHandler, R1}};
                BitNo =:= -1008 ->
                    %% Endgame
                    %% For the last round we cannot have 14 zeros or more
                    %% at the top of M1 because then we will underflow,
                    %% so we need at least 43 bits
                    if
                        ?BIT(42) =< M1 ->
                            %% We have 43 bits - get the last bits
                            uniform_real_s(
                              AlgHandler, Next, M1, BitNo - 56, R1);
                        true ->
                            %% Would underflow 2^-1022 - start all over
                            %%
                            %% We could just crash here since the odds for
                            %% the PRNG being broken is much higher than
                            %% for a good PRNG generating this many zeros
                            %% in a row.  Maybe we should write an error
                            %% report or call this a system limit...?
                            uniform_real_s({AlgHandler, R1})
                    end;
                true ->
                    %% Need more bits
                    uniform_real_s(AlgHandler, Next, M1, BitNo - 56, R1)
            end;
        true ->
            %% Fill up to 53 bits
            B0 = 53 - ?BC(M0, 49), % Number of bits we need to append
            {(((M0 bsl B0) bor (V1 bsr (Bits - B0)))
              * math:pow(2.0, BitNo - B0)),
             {AlgHandler, R1}}
    end.
%%
uniform_real_s(#{bits:=Bits} = AlgHandler, Next, M0, BitNo, R0) ->
    {V1, R1} = Next(R0),
    uniform_real_s(AlgHandler, Next, M0, BitNo, R1, V1, Bits);
uniform_real_s(#{max:=_} = AlgHandler, Next, M0, BitNo, R0) ->
    {V1, R1} = Next(R0),
    uniform_real_s(AlgHandler, Next, M0, BitNo, R1, ?MASK(56, V1), 56).


%% bytes/1: given a number N,
%% returns a random binary with N bytes

-doc """
Returns, for a specified integer `N >= 0`, a `t:binary/0` with that number of
random bytes. Generates as many random numbers as required using the selected
algorithm to compose the binary, and updates the state in the process dictionary
accordingly.
""".
-doc(#{title => <<"Plug-in framework API">>,since => <<"OTP 24.0">>}).
-spec bytes(N :: non_neg_integer()) -> Bytes :: binary().
bytes(N) ->
    {Bytes, State} = bytes_s(N, seed_get()),
    _ = seed_put(State),
    Bytes.


%% bytes_s/2: given a number N and a state,
%% returns a random binary with N bytes and a new state

-doc """
Returns, for a specified integer `N >= 0` and a state, a `t:binary/0` with that
number of random bytes, and a new state. Generates as many random numbers as
required using the selected algorithm to compose the binary, and the new state.
""".
-doc(#{title => <<"Plug-in framework API">>,since => <<"OTP 24.0">>}).
-spec bytes_s(N :: non_neg_integer(), State :: state()) ->
                     {Bytes :: binary(), NewState :: state()}.
bytes_s(N, {#{bits:=Bits, next:=Next} = AlgHandler, R})
  when is_integer(N), 0 =< N ->
    WeakLowBits = maps:get(weak_low_bits, AlgHandler, 0),
    bytes_r(N, AlgHandler, Next, R, Bits, WeakLowBits);
bytes_s(N, {#{max:=Mask, next:=Next} = AlgHandler, R})
  when is_integer(N), 0 =< N, ?MASK(58) =< Mask ->
    %% Old spec - assume 58 bits and 2 weak low bits
    %% giving 56 bits i.e precisely 7 bytes per generated number
    Bits = 58,
    WeakLowBits = 2,
    bytes_r(N, AlgHandler, Next, R, Bits, WeakLowBits).

%% N:           Number of bytes to generate
%% Bits:        Number of bits in the generated word
%% WeakLowBits: Number of low bits in the generated word
%%              to waste due to poor quality
bytes_r(N, AlgHandler, Next, R, Bits, WeakLowBits) ->
    %% We use whole bytes from each generator word,
    %% GoodBytes: that number of bytes
    GoodBytes = (Bits - WeakLowBits) bsr 3,
    GoodBits = GoodBytes bsl 3,
    %% Shift: how many bits of each generator word to waste
    %% by shifting right - we use the bits from the big end
    Shift = Bits - GoodBits,
    bytes_r(N, AlgHandler, Next, R, <<>>, GoodBytes, GoodBits, Shift).
%%
bytes_r(N0, AlgHandler, Next, R0, Bytes0, GoodBytes, GoodBits, Shift)
  when (GoodBytes bsl 2) < N0 ->
    %% Loop unroll 4 iterations
    %% - gives about 25% shorter time for large binaries
    {V1, R1} = Next(R0),
    {V2, R2} = Next(R1),
    {V3, R3} = Next(R2),
    {V4, R4} = Next(R3),
    Bytes1 =
        <<Bytes0/binary,
          (V1 bsr Shift):GoodBits,
          (V2 bsr Shift):GoodBits,
          (V3 bsr Shift):GoodBits,
          (V4 bsr Shift):GoodBits>>,
    N1 = N0 - (GoodBytes bsl 2),
    bytes_r(N1, AlgHandler, Next, R4, Bytes1, GoodBytes, GoodBits, Shift);
bytes_r(N0, AlgHandler, Next, R0, Bytes0, GoodBytes, GoodBits, Shift)
  when GoodBytes < N0 ->
    {V, R1} = Next(R0),
    Bytes1 = <<Bytes0/binary, (V bsr Shift):GoodBits>>,
    N1 = N0 - GoodBytes,
    bytes_r(N1, AlgHandler, Next, R1, Bytes1, GoodBytes, GoodBits, Shift);
bytes_r(N, AlgHandler, Next, R0, Bytes, _GoodBytes, GoodBits, _Shift) ->
    {V, R1} = Next(R0),
    Bits = N bsl 3,
    %% Use the big end bits
    Shift = GoodBits - Bits,
    {<<Bytes/binary, (V bsr Shift):Bits>>, {AlgHandler, R1}}.


%% jump/1: given a state, jump/1
%% returns a new state which is equivalent to that
%% after a large number of call defined for each algorithm.
%% The large number is algorithm dependent.

-doc """
Returns the state after performing jump calculation to the given state.

This function generates a `not_implemented` error exception when the jump
function is not implemented for the algorithm specified in the state.
""".
-doc(#{title => <<"Plug-in framework API">>,since => <<"OTP 20.0">>}).
-spec jump(state()) -> NewState :: state().
jump(State = {#{jump:=Jump}, _}) ->
    Jump(State);
jump({#{}, _}) ->
    erlang:error(not_implemented).


%% jump/0: read the internal state and
%% apply the jump function for the state as in jump/1
%% and write back the new value to the internal state,
%% then returns the new value.

-doc """
Returns the state after performing jump calculation to the state in the process
dictionary.

This function generates a `not_implemented` error exception when the jump
function is not implemented for the algorithm specified in the state in the
process dictionary.
""".
-doc(#{title => <<"Plug-in framework API">>,since => <<"OTP 20.0">>}).
-spec jump() -> NewState :: state().
jump() ->
    seed_put(jump(seed_get())).

%% normal/0: returns a random float with standard normal distribution
%% updating the state in the process dictionary.

-doc """
Returns a standard normal deviate float (that is, the mean is 0 and the standard
deviation is 1) and updates the state in the process dictionary.
""".
-doc(#{title => <<"Plug-in framework API">>,since => <<"OTP 18.0">>}).
-spec normal() -> float().
normal() ->
    {X, Seed} = normal_s(seed_get()),
    _ = seed_put(Seed),
    X.

%% normal/2: returns a random float with N(μ, σ²) normal distribution
%% updating the state in the process dictionary.

-doc """
Returns a normal N(Mean, Variance) deviate float and updates the state in the
process dictionary.
""".
-doc(#{title => <<"Plug-in framework API">>,since => <<"OTP 20.0">>}).
-spec normal(Mean :: number(), Variance :: number()) -> float().
normal(Mean, Variance) ->
    Mean + (math:sqrt(Variance) * normal()).

%% normal_s/1: returns a random float with standard normal distribution
%% The Ziggurat Method for generating random variables - Marsaglia and Tsang
%% Paper and reference code: http://www.jstatsoft.org/v05/i08/

-doc """
Returns, for a specified state, a standard normal deviate float (that is, the
mean is 0 and the standard deviation is 1) and a new state.
""".
-doc(#{title => <<"Plug-in framework API">>,since => <<"OTP 18.0">>}).
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

-doc """
Returns, for a specified state, a normal N(Mean, Variance) deviate float and a
new state.
""".
-doc(#{title => <<"Plug-in framework API">>,since => <<"OTP 20.0">>}).
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
    {#{type=>exsplus, max=>?MASK(58), next=>fun exsp_next/1,
       jump=>fun exsplus_jump/1},
     fun exsplus_seed/1};
mk_alg(exsp) ->
    {#{type=>exsp, bits=>58, weak_low_bits=>1, next=>fun exsp_next/1,
       uniform=>fun exsp_uniform/1, uniform_n=>fun exsp_uniform/2,
       jump=>fun exsplus_jump/1},
     fun exsplus_seed/1};
mk_alg(exsss) ->
    {#{type=>exsss, bits=>58, next=>fun exsss_next/1,
       uniform=>fun exsss_uniform/1, uniform_n=>fun exsss_uniform/2,
       jump=>fun exsplus_jump/1},
     fun exsss_seed/1};
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
     fun exrop_seed/1};
mk_alg(exro928ss) ->
    {#{type=>exro928ss, bits=>58, next=>fun exro928ss_next/1,
       uniform=>fun exro928ss_uniform/1,
       uniform_n=>fun exro928ss_uniform/2,
       jump=>fun exro928_jump/1},
     fun exro928_seed/1};
mk_alg(dummy=Name) ->
    {#{type=>Name, bits=>58, next=>fun dummy_next/1,
       uniform=>fun dummy_uniform/1,
       uniform_n=>fun dummy_uniform/2},
     fun dummy_seed/1}.

%% =====================================================================
%% exs64 PRNG: Xorshift64*
%% Algorithm by Sebastiano Vigna
%% Reference URL: http://xorshift.di.unimi.it/
%% =====================================================================

-doc "Algorithm specific internal state".
-opaque exs64_state() :: uint64().

exs64_seed(L) when is_list(L) ->
    [R] = seed64_nz(1, L),
    R;
exs64_seed(A) when is_integer(A) ->
    [R] = seed64(1, A),
    R;
%%
%% Traditional integer triplet seed
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
%%
%% Reference C code for Xorshift116+ and Xorshift116**
%%
%% #include <stdint.h>
%%
%% #define MASK(b, v) (((UINT64_C(1) << (b)) - 1) & (v))
%% #define BSL(b, v, n) (MASK((b)-(n), (v)) << (n))
%% #define ROTL(b, v, n) (BSL((b), (v), (n)) | ((v) >> ((b)-(n))))
%%
%% uint64_t s[2];
%%
%% uint64_t next(void) {
%%     uint64_t s1 = s[0];
%%     const uint64_t s0 = s[1];
%%
%%     s1 ^= BSL(58, s1, 24); // a
%%     s1 ^= s0 ^ (s1 >> 11) ^ (s0 >> 41); // b, c
%%     s[0] = s0;
%%     s[1] = s1;
%%
%%     const uint64_t result_plus = MASK(58, s0 + s1);
%%     uint64_t result_starstar = s0;
%%     result_starstar = MASK(58, result_starstar * 5);
%%     result_starstar = ROTL(58, result_starstar, 7);
%%     result_starstar = MASK(58, result_starstar * 9);
%%
%%     return result_plus;
%%     return result_starstar;
%% }
%%
%% =====================================================================
-doc "Algorithm specific internal state".
-opaque exsplus_state() :: nonempty_improper_list(uint58(), uint58()).

-dialyzer({no_improper_lists, exsplus_seed/1}).

exsplus_seed(L) when is_list(L) ->
    [S0,S1] = seed58_nz(2, L),
    [S0|S1];
exsplus_seed(X) when is_integer(X) ->
    [S0,S1] = seed58(2, X),
    [S0|S1];
%%
%% Traditional integer triplet seed
exsplus_seed({A1, A2, A3}) ->
    {_, R1} = exsp_next(
                [?MASK(58, (A1 * 4294967197) + 1)|
                 ?MASK(58, (A2 * 4294967231) + 1)]),
    {_, R2} = exsp_next(
                [?MASK(58, (A3 * 4294967279) + 1)|
                 tl(R1)]),
    R2.

-dialyzer({no_improper_lists, exsss_seed/1}).

exsss_seed(L) when is_list(L) ->
    [S0,S1] = seed58_nz(2, L),
    [S0|S1];
exsss_seed(X) when is_integer(X) ->
    [S0,S1] = seed58(2, X),
    [S0|S1];
%%
%% Seed from traditional integer triple - mix into splitmix
exsss_seed({A1, A2, A3}) ->
    {_, X0} = seed58(A1),
    {S0, X1} = seed58(A2 bxor X0),
    {S1, _} = seed58(A3 bxor X1),
    [S0|S1].

%% Advance Xorshift116 state one step
-define(
   exs_next(S0, S1, S1_b),
   begin
       S1_b = ?MASK(58, S1) bxor ?BSL(58, S1, 24),
       S1_b bxor S0 bxor (S1_b bsr 11) bxor (S0 bsr 41)
   end).

-define(
   scramble_starstar(S, V_a, V_b),
   begin
       %% The multiply by add shifted trick avoids creating bignums
       %% which improves performance significantly
       %%
       %% Scramble ** (all operations modulo word size)
       %% ((S * 5) rotl 7) * 9
       %%
       V_a = S + ?BSL(58, S, 2),                             % * 5
       V_b = ?BSL(58, V_a, 7) bor ?MASK(7, V_a bsr (58-7)),  % rotl 7
       ?MASK(58, V_b + ?BSL(58, V_b, 3))                     % * 9
   end).

%% Advance state and generate 58bit unsigned integer
%%
-dialyzer({no_improper_lists, exsp_next/1}).
-doc """
Returns a random 58-bit integer `X` and a new generator state `NewAlgState`,
according to the Xorshift116+ algorithm.

This is an API function into the internal implementation of the
[`exsp`](`m:rand#algorithms`) algorithm that enables using it without the
overhead of the plug-in framework, which might be useful for time critial
applications. On a typical 64 bit Erlang VM this approach executes in just above
30% (1/3) of the time for the default algorithm through this module's normal
plug-in framework.

To seed this generator use [`{_, AlgState} = rand:seed_s(exsp)` ](`seed_s/1`)or
[`{_, AlgState} = rand:seed_s(exsp, Seed)` ](`seed_s/1`)with a specific `Seed`.

> #### Note {: .info }
>
> This function offers no help in generating a number on a selected range, nor
> in generating a floating point number. It is easy to accidentally mess up the
> fairly good statistical properties of this generator when doing either. See
> the recepies at the start of this
> [Niche algorithms API ](`m:rand#niche-algorithms-api`)description. Note also
> the caveat about weak low bits that this generator suffers from. The generator
> is exported in this form primarily for performance.
""".
-doc(#{title => <<"Niche algorithms API">>,since => <<"OTP 25.0">>}).
-spec exsp_next(AlgState :: exsplus_state()) ->
                       {X :: uint58(), NewAlgState :: exsplus_state()}.
exsp_next([S1|S0]) ->
    %% Note: members s0 and s1 are swapped here
    S0_1 = ?MASK(58, S0),
    NewS1 = ?exs_next(S0_1, S1, S1_b),
    %% Scramble + (all operations modulo word size)
    %% S0 + NewS1
    {?MASK(58, S0_1 + NewS1), [S0_1|NewS1]}.

-dialyzer({no_improper_lists, exsss_next/1}).

-spec exsss_next(exsplus_state()) -> {uint58(), exsplus_state()}.
exsss_next([S1|S0]) ->
    %% Note: members s0 and s1 are swapped here
    S0_1 = ?MASK(58, S0),
    NewS1 = ?exs_next(S0_1, S1, S1_b),
    {?scramble_starstar(S0_1, V_1, V_2), [S0_1|NewS1]}.

exsp_uniform({AlgHandler, R0}) ->
    {I, R1} = exsp_next(R0),
    %% Waste the lowest bit since it is of lower
    %% randomness quality than the others
    {(I bsr (58-53)) * ?TWO_POW_MINUS53, {AlgHandler, R1}}.

exsss_uniform({AlgHandler, R0}) ->
    {I, R1} = exsss_next(R0),
    {(I bsr (58-53)) * ?TWO_POW_MINUS53, {AlgHandler, R1}}.

exsp_uniform(Range, {AlgHandler, R}) ->
    {V, R1} = exsp_next(R),
    MaxMinusRange = ?BIT(58) - Range,
    ?uniform_range(Range, AlgHandler, R1, V, MaxMinusRange, I).

exsss_uniform(Range, {AlgHandler, R}) ->
    {V, R1} = exsss_next(R),
    MaxMinusRange = ?BIT(58) - Range,
    ?uniform_range(Range, AlgHandler, R1, V, MaxMinusRange, I).


%% This is the jump function for the exs... generators,
%% i.e the Xorshift116 generators,  equivalent
%% to 2^64 calls to next/1; it can be used to generate 2^52
%% non-overlapping subsequences for parallel computations.
%% Note: the jump function takes 116 times of the execution time of
%% next/1.
%%
%% #include <stdint.h>
%%
%% void jump(void) {
%%   static const uint64_t JUMP[] = { 0x02f8ea6bc32c797,
%% 				   0x345d2a0f85f788c };
%%   int i, b;
%%   uint64_t s0 = 0;
%%   uint64_t s1 = 0;
%%   for(i = 0; i < sizeof JUMP / sizeof *JUMP; i++)
%%     for(b = 0; b < 58; b++) {
%%       if (JUMP[i] & 1ULL << b) {
%% 	s0 ^= s[0];
%% 	s1 ^= s[1];
%%       }
%%       next();
%%     }
%%   s[0] = s0;
%%   s[1] = s1;
%% }
%%
%% -define(JUMPCONST, 16#000d174a83e17de2302f8ea6bc32c797).
%% split into 58-bit chunks
%% and two iterative executions

-define(JUMPCONST1, 16#02f8ea6bc32c797).
-define(JUMPCONST2, 16#345d2a0f85f788c).
-define(JUMPELEMLEN, 58).

-dialyzer({no_improper_lists, exsplus_jump/1}).
-spec exsplus_jump({alg_handler(), exsplus_state()}) ->
                          {alg_handler(), exsplus_state()}.
exsplus_jump({AlgHandler, S}) ->
    {AlgHandler, exsp_jump(S)}.

-dialyzer({no_improper_lists, exsp_jump/1}).
-doc """
Returns a new generator state equivalent of the state after iterating over
`exsp_next/1` 2^64 times.

See the description of jump functions at the top of this module description.
""".
-doc(#{title => <<"Niche algorithms API">>,since => <<"OTP 25.0">>}).
-spec exsp_jump(AlgState :: exsplus_state()) ->
                       NewAlgState :: exsplus_state().
exsp_jump(S) ->
    {S1, AS1} = exsplus_jump(S, [0|0], ?JUMPCONST1, ?JUMPELEMLEN),
    {_,  AS2} = exsplus_jump(S1, AS1,  ?JUMPCONST2, ?JUMPELEMLEN),
    AS2.

-dialyzer({no_improper_lists, exsplus_jump/4}).
exsplus_jump(S, AS, _, 0) ->
    {S, AS};
exsplus_jump(S, [AS0|AS1], J, N) ->
    {_, NS} = exsp_next(S),
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

-doc "Algorithm specific internal state".
-opaque exs1024_state() :: {list(uint64()), list(uint64())}.

exs1024_seed(L) when is_list(L) ->
    {seed64_nz(16, L), []};
exs1024_seed(X) when is_integer(X) ->
    {seed64(16, X), []};
%%
%% Seed from traditional triple, remain backwards compatible
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

-spec exs1024_jump({alg_handler(), exs1024_state()}) ->
                          {alg_handler(), exs1024_state()}.
exs1024_jump({AlgHandler, {L, RL}}) ->
    P = length(RL),
    AS = exs1024_jump({L, RL},
         [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
         ?JUMPCONSTTAIL, ?JUMPCONSTHEAD, ?JUMPELEMLEN, ?JUMPTOTALLEN),
    {ASL, ASR} = lists:split(?RINGLEN - P, AS),
    {AlgHandler, {ASL, lists:reverse(ASR)}}.

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
%% exro928ss PRNG: Xoroshiro928**
%%
%% Reference URL: http://vigna.di.unimi.it/ftp/papers/ScrambledLinear.pdf
%% i.e the Xoroshiro1024 generator with ** scrambler
%% with {S, R, T} = {5, 7, 9} as recommended in the paper.
%%
%% {A, B, C} were tried out and selected as {44, 9, 45}
%% and the jump coefficients calculated.
%%
%% Standard jump function pseudocode:
%% 
%%     Jump constant j = 0xb10773cb...44085302f77130ca
%%     Generator state: s
%%     New generator state: t = 0
%%     foreach bit in j, low to high:
%%         if the bit is one:
%%             t ^= s
%%         next s
%%     s = t
%%
%% Generator used for reference value calculation:
%%
%%     #include <stdint.h>
%%     #include <stdio.h>
%%     
%%     int p = 0;
%%     uint64_t s[16];
%%     
%%     #define MASK(x) ((x) & ((UINT64_C(1) << 58) - 1))
%%     static __inline uint64_t rotl(uint64_t x, int n) {
%%         return MASK(x << n) | (x >> (58 - n));
%%     }
%%     
%%     uint64_t next() {
%%         const int q = p;
%%         const uint64_t s0 = s[p = (p + 1) & 15];
%%         uint64_t s15 = s[q];
%%     
%%         const uint64_t result_starstar = MASK(rotl(MASK(s0 * 5), 7) * 9);
%%     
%%         s15 ^= s0;
%%         s[q] = rotl(s0, 44) ^ s15 ^ MASK(s15 << 9);
%%         s[p] = rotl(s15, 45);
%%     
%%         return result_starstar;
%%     }
%%
%%     static const uint64_t jump_2pow512[15] =
%%         { 0x44085302f77130ca, 0xba05381fdfd14902, 0x10a1de1d7d6813d2,
%%           0xb83fe51a1eb3be19, 0xa81b0090567fd9f0, 0x5ac26d5d20f9b49f,
%%           0x4ddd98ee4be41e01, 0x0657e19f00d4b358, 0xf02f778573cf0f0a,
%%           0xb45a3a8a3cef3cc0, 0x6e62a33cc2323831, 0xbcb3b7c4cc049c53,
%%           0x83f240c6007e76ce, 0xe19f5fc1a1504acd, 0x00000000b10773cb };
%%
%%     static const uint64_t jump_2pow20[15] =
%%         { 0xbdb966a3daf905e6, 0x644807a56270cf78, 0xda90f4a806c17e9e,
%%           0x4a426866bfad3c77, 0xaf699c306d8e7566, 0x8ebc73c700b8b091,
%%           0xc081a7bf148531fb, 0xdc4d3af15f8a4dfd, 0x90627c014098f4b6,
%%           0x06df2eb1feaf0fb6, 0x5bdeb1a5a90f2e6b, 0xa480c5878c3549bd,
%%           0xff45ef33c82f3d48, 0xa30bebc15fefcc78, 0x00000000cb3d181c };
%%
%%     void jump(const uint64_t *jump) {
%%         uint64_t j, t[16] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
%%         int m, n, k;
%%         for (m = 0;  m < 15;  m++, jump++) {
%%             for (n = 0, j = *jump;  n < 64;  n++, j >>= 1) {
%%                 if ((j & 1) != 0) {
%%                     for (k = 0;  k < 16;  k++) {
%%                         t[k] ^= s[(p + k) & 15];
%%                     }
%%                 }
%%                 next();
%%             }
%%         }
%%         for (k = 0;  k < 16;  k++) {
%%             s[(p + k) & 15] = t[k];
%%         }
%%     }
%%
%% =====================================================================

-doc "Algorithm specific internal state".
-opaque exro928_state() :: {list(uint58()), list(uint58())}.

-doc false.
-spec exro928_seed(
        list(uint58()) | integer() | {integer(), integer(), integer()}) ->
                          exro928_state().
exro928_seed(L) when is_list(L) ->
    {seed58_nz(16, L), []};
exro928_seed(X) when is_integer(X) ->
    {seed58(16, X), []};
%%
%% Seed from traditional integer triple - mix into splitmix
exro928_seed({A1, A2, A3}) ->
    {S0, X0} = seed58(A1),
    {S1, X1} = seed58(A2 bxor X0),
    {S2, X2} = seed58(A3 bxor X1),
    {[S0,S1,S2|seed58(13, X2)], []}.


%% Update the state and calculate output word
-spec exro928ss_next(exro928_state()) -> {uint58(), exro928_state()}.
exro928ss_next({[S15,S0|Ss], Rs}) ->
    SR = exro928_next_state(Ss, Rs, S15, S0),
    %%
    %% {S, R, T} = {5, 7, 9}
    %% const uint64_t result_starstar = rotl(s0 * S, R) * T;
    %%
    {?scramble_starstar(S0, V_0, V_1), SR};
exro928ss_next({[S15], Rs}) ->
    exro928ss_next({[S15|lists:reverse(Rs)], []}).

-doc false.
-spec exro928_next(exro928_state()) -> {{uint58(),uint58()}, exro928_state()}.
exro928_next({[S15,S0|Ss], Rs}) ->
    SR = exro928_next_state(Ss, Rs, S15, S0),
    {{S15,S0}, SR};
exro928_next({[S15], Rs}) ->
    exro928_next({[S15|lists:reverse(Rs)], []}).

%% Just update the state
-doc false.
-spec exro928_next_state(exro928_state()) -> exro928_state().
exro928_next_state({[S15,S0|Ss], Rs}) ->
    exro928_next_state(Ss, Rs, S15, S0);
exro928_next_state({[S15], Rs}) ->
    [S0|Ss] = lists:reverse(Rs),
    exro928_next_state(Ss, [], S15, S0).

exro928_next_state(Ss, Rs, S15, S0) ->
    %% {A, B, C} = {44, 9, 45},
    %% s15 ^= s0;
    %% NewS15: s[q] = rotl(s0, A) ^ s15 ^ (s15 << B);
    %% NewS0: s[p] = rotl(s15, C);
    %%
    S0_1 = ?MASK(58, S0),
    Q = ?MASK(58, S15) bxor S0_1,
    NewS15 = ?ROTL(58, S0_1, 44) bxor Q bxor ?BSL(58, Q, 9),
    NewS0 = ?ROTL(58, Q, 45),
    {[NewS0|Ss], [NewS15|Rs]}.


exro928ss_uniform({AlgHandler, SR}) ->
    {V, NewSR} = exro928ss_next(SR),
    {(V bsr (58-53)) * ?TWO_POW_MINUS53, {AlgHandler, NewSR}}.

exro928ss_uniform(Range, {AlgHandler, SR}) ->
    {V, NewSR} = exro928ss_next(SR),
    MaxMinusRange = ?BIT(58) - Range,
    ?uniform_range(Range, AlgHandler, NewSR, V, MaxMinusRange, I).


-spec exro928_jump({alg_handler(), exro928_state()}) ->
                          {alg_handler(), exro928_state()}.
exro928_jump({AlgHandler, SR}) ->
    {AlgHandler,exro928_jump_2pow512(SR)}.

-doc false.
-spec exro928_jump_2pow512(exro928_state()) -> exro928_state().
exro928_jump_2pow512(SR) ->
    polyjump(
      SR, fun exro928_next_state/1,
      %% 2^512
      [16#4085302F77130CA, 16#54E07F7F4524091,
       16#5E1D7D6813D2BA0, 16#4687ACEF8644287,
       16#4567FD9F0B83FE5, 16#43E6D27EA06C024,
       16#641E015AC26D5D2, 16#6CD61377663B92F,
       16#70A0657E19F00D4, 16#43C0BDDE15CF3C3,
       16#745A3A8A3CEF3CC, 16#58A8CF308C8E0C6,
       16#7B7C4CC049C536E, 16#431801F9DB3AF2C,
       16#41A1504ACD83F24, 16#6C41DCF2F867D7F]).

-doc false.
-spec exro928_jump_2pow20(exro928_state()) -> exro928_state().
exro928_jump_2pow20(SR) ->
    polyjump(
      SR, fun exro928_next_state/1,
      %% 2^20
      [16#5B966A3DAF905E6, 16#601E9589C33DE2F,
       16#74A806C17E9E644, 16#59AFEB4F1DF6A43,
       16#46D8E75664A4268, 16#42E2C246BDA670C,
       16#4531FB8EBC73C70, 16#537F702069EFC52,
       16#4B6DC4D3AF15F8A, 16#5A4189F0050263D,
       16#46DF2EB1FEAF0FB, 16#77AC696A43CB9AC,
       16#4C5878C3549BD5B, 16#7CCF20BCF522920,
       16#415FEFCC78FF45E, 16#72CF460728C2FAF]).

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

-doc "Algorithm specific internal state".
-opaque exrop_state() :: nonempty_improper_list(uint58(), uint58()).

-dialyzer({no_improper_lists, exrop_seed/1}).

exrop_seed(L) when is_list(L) ->
    [S0,S1] = seed58_nz(2, L),
    [S0|S1];
exrop_seed(X) when is_integer(X) ->
    [S0,S1] = seed58(2, X),
    [S0|S1];
%%
%% Traditional integer triplet seed
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

exrop_uniform({AlgHandler, R}) ->
    {V, R1} = exrop_next(R),
    %% Waste the lowest bit since it is of lower
    %% randomness quality than the others
    {(V bsr (58-53)) * ?TWO_POW_MINUS53, {AlgHandler, R1}}.

exrop_uniform(Range, {AlgHandler, R}) ->
    {V, R1} = exrop_next(R),
    MaxMinusRange = ?BIT(58) - Range,
    ?uniform_range(Range, AlgHandler, R1, V, MaxMinusRange, I).

%% Split a 116 bit constant into two 58 bit words,
%% a top '1' marks the end of the low word.
-define(
   JUMP_116(Jump),
   [?BIT(58) bor ?MASK(58, (Jump)),(Jump) bsr 58]).
%%
exrop_jump({AlgHandler,S}) ->
    [J|Js] = ?JUMP_116(16#9863200f83fcd4a11293241fcb12a),
    {AlgHandler, exrop_jump(S, 0, 0, J, Js)}.
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
%% dummy "PRNG": Benchmark dummy overhead reference
%%
%% As fast as possible - return something daft and update state;
%% to measure plug-in framework overhead.
%%
%% =====================================================================

-doc "Algorithm specific internal state".
-type dummy_state() :: uint58().

dummy_uniform(_Range, {AlgHandler,R}) ->
    {1, {AlgHandler,(R bxor ?MASK(58))}}. % 1 is always in Range
dummy_next(R) ->
    {R, R bxor ?MASK(58)}.
dummy_uniform({AlgHandler,R}) ->
    {0.5, {AlgHandler,(R bxor ?MASK(58))}}. % Perfect mean value

%% Serious looking seed, to avoid rand_SUITE seed test failure
%%
dummy_seed(L) when is_list(L) ->
    case L of
        [] ->
            erlang:error(zero_seed);
        [X] when is_integer(X) ->
            ?MASK(58, X);
        [X|_] when is_integer(X) ->
            erlang:error(too_many_seed_integers);
        [_|_] ->
            erlang:error(non_integer_seed)
    end;
dummy_seed(X) when is_integer(X) ->
    {Z1, _} = splitmix64_next(X),
    ?MASK(58, Z1);
dummy_seed({A1, A2, A3}) ->
    {_, X1} = splitmix64_next(A1),
    {_, X2} = splitmix64_next(A2 bxor X1),
    {Z3, _} = splitmix64_next(A3 bxor X2),
    ?MASK(58, Z3).


%% =====================================================================
%% mcg58 PRNG: Multiply With Carry generator
%%
%% Parameters deduced in collaboration with
%% Prof. Sebastiano Vigna of the University of Milano.
%%
%% X = CX0 & (2^B - 1)  % Low B bits - digit
%% C = CX0 >> B         % High bits  - carry
%% CX1 = A * X0 + C0
%%
%% An MWC generator is an efficient alternative implementation of
%% a Multiplicative Congruential Generator, that is, the generator
%% CX1 = (CX0 * 2^B) rem P
%% where P is the safe prime (A * 2^B - 1), that generates
%% the same sequence in the reverse order.  The generator
%% CX1 = (A * CX0) rem P
%% that uses the multiplicative inverse mod P is, indeed,
%% an exact equivalent to the corresponding MWC generator.
%%
%% An MWC generator has, due to the power of two multiplier
%% in the corresponding MCG, got known statistical weaknesses
%% in the spectral score for 3 dimensions, so it should be used
%% with a scrambler that hides the flaws.  The scramblers
%% have been tried out in the PractRand and TestU01 frameworks
%% and settled for a single Xorshift to get B good bits,
%% and a double Xorshift to get all bits good enough.
%%
%% The chosen parameters are:
%% A = 16#7fa6502
%% B = 32
%% Single Xorshift: 8
%% Double Xorshift: 4, 27
%%
%% These parameters gives the MWC "digit" size 32 bits
%% which gives them theoretical statistical guarantees,
%% and keeps the state in 59 bits.
%%
%% The state should only be used to mask or rem out low bits.
%% The scramblers return 58 bits from which a number should
%% be masked or rem:ed out.
%%
%% =====================================================================
-define(MWC59_A, (16#7fa6502)).
-define(MWC59_B, (32)).
-define(MWC59_P, ((?MWC59_A bsl ?MWC59_B) - 1)).

-define(MWC59_XS, 8).
-define(MWC59_XS1, 4).
-define(MWC59_XS2, 27).

-doc """
1 .. ((16#1ffb072 \* 2^29 - 1) - 1)
""".
-type mwc59_state() :: 1..?MWC59_P-1.

-doc """
Returns a new generator state `CX1`, according to a Multiply With Carry
generator, which is an efficient implementation of a Multiplicative Congruential
Generator with a power of 2 multiplier and a prime modulus.

This generator uses the multiplier 2^32 and the modulus 16#7fa6502 * 2^32 - 1,
which have been selected, in collaboration with Sebastiano Vigna, to avoid
bignum operations and still get good statistical quality. It can be written
as:  
`C = CX0 bsr 32`  
`X = CX0 band ((1 bsl 32)-1))`  
`CX1 = 16#7fa6502 * X + C`

Because the generator uses a multiplier that is a power of 2 it gets statistical
flaws for collision tests and birthday spacings tests in 2 and 3 dimensions, and
even these caveats apply only to the MWC "digit", that is the low 32 bits (due
to the multiplier) of the generator state.

The quality of the output value improves much by using a scrambler instead of
just taking the low bits. Function [`mwc59_value32` ](`mwc59_value32/1`)is a
fast scrambler that returns a decent 32-bit number. The slightly slower
[`mwc59_value` ](`mwc59_value/1`)scrambler returns 59 bits of very good quality,
and [`mwc59_float`](`mwc59_float/1`) returns a `t:float/0` of very good quality.

The low bits of the base generator are surprisingly good, so the lowest 16 bits
actually pass fairly strict PRNG tests, despite the generator's weaknesses that
lie in the high bits of the 32-bit MWC "digit". It is recommended to use `rem`
on the the generator state, or bit mask extracting the lowest bits to produce
numbers in a range 16 bits or less. See the recepies at the start of this
[Niche algorithms API ](`m:rand#niche-algorithms-api`)description.

On a typical 64 bit Erlang VM this generator executes in below 8% (1/13) of the
time for the default algorithm in the
[plug-in framework API ](`m:rand#plug-in-framework-api`)of this module. With the
[`mwc59_value32` ](`mwc59_value32/1`)scrambler the total time becomes 16% (1/6),
and with [`mwc59_value` ](`mwc59_value/1`)it becomes 20% (1/5) of the time for
the default algorithm. With [`mwc59_float`](`mwc59_float/1`) the total time is
60% of the time for the default algorithm generating a `t:float/0`.

> #### Note {: .info }
>
> This generator is a niche generator for high speed applications. It has a much
> shorter period than the default generator, which in itself is a quality
> concern, although when used with the value scramblers it passes strict PRNG
> tests. The generator is much faster than `exsp_next/1` but with a bit lower
> quality.
""".
-doc(#{title => <<"Niche algorithms API">>,since => <<"OTP 25.0">>}).
-spec mwc59(CX0 :: mwc59_state()) -> CX1 :: mwc59_state().
mwc59(CX) when is_integer(CX), 1 =< CX, CX < ?MWC59_P ->
    C = CX bsr ?MWC59_B,
    X = ?MASK(?MWC59_B, CX),
    ?MWC59_A * X + C.

%%% %% Verification by equivalent MCG generator
%%% mwc59_r(CX1) ->
%%%     (CX1 bsl ?MWC59_B) rem ?MWC59_P. % Reverse
%%% %%%     (CX1 * ?MWC59_A) rem ?MWC59_P. % Forward
%%%
%%% mwc59(CX0, 0) ->
%%%     CX0;
%%% mwc59(CX0, N) ->
%%%     CX1 = mwc59(CX0),
%%%     CX0 = mwc59_r(CX1),
%%%     mwc59(CX1, N - 1).

-doc """
Returns a 32-bit value `V` from a generator state `CX`. The generator state is
scrambled using an 8-bit xorshift which masks the statistical imperfecions of
the base generator [`mwc59`](`mwc59/1`) enough to produce numbers of decent
quality. Still some problems in 2- and 3-dimensional birthday spacing and
collision tests show through.

When using this scrambler it is in general better to use the high bits of the
value than the low. The lowest 8 bits are of good quality and pass right through
from the base generator. They are combined with the next 8 in the xorshift
making the low 16 good quality, but in the range 16..31 bits there are weaker
bits that you do not want to have as the high bits of your generated values.
Therefore it is in general safer to shift out low bits. See the recepies at the
start of this [Niche algorithms API ](`m:rand#niche-algorithms-api`)description.

For a non power of 2 range less than about 16 bits (to not get too much bias and
to avoid bignums) truncated multiplication can be used, which is much faster
than using `rem`: `(Range*V) bsr 32`.
""".
-doc(#{title => <<"Niche algorithms API">>,since => <<"OTP 25.0">>}).
-spec mwc59_value32(CX :: mwc59_state()) -> V :: 0..?MASK(32).
mwc59_value32(CX1) when is_integer(CX1), 1 =< CX1, CX1 < ?MWC59_P ->
    CX = ?MASK(32, CX1),
    CX bxor ?BSL(32, CX, ?MWC59_XS).

-doc """
Returns a 59-bit value `V` from a generator state `CX`. The generator state is
scrambled using an 4-bit followed by a 27-bit xorshift, which masks the
statistical imperfecions of the base generator [`mwc59`](`mwc59/1`) enough that
all 59 bits are of very good quality.

Be careful to not accidentaly create a bignum when handling the value `V`.

It is in general general better to use the high bits from this scrambler than
the low. See the recepies at the start of this
[Niche algorithms API ](`m:rand#niche-algorithms-api`)description.

For a non power of 2 range less than about 29 bits (to not get too much bias and
to avoid bignums) truncated multiplication can be used, which is much faster
than using `rem`. Example for range 1'000'000'000; the range is 30 bits, we use
29 bits from the generator, adding up to 59 bits, which is not a bignum:
`(1000000000 * (V bsr (59-29))) bsr 29`.
""".
-doc(#{title => <<"Niche algorithms API">>,since => <<"OTP 25.0">>}).
-spec mwc59_value(CX :: mwc59_state()) -> V :: 0..?MASK(59).
mwc59_value(CX) when is_integer(CX), 1 =< CX, CX < ?MWC59_P ->
    CX2 = CX bxor ?BSL(59, CX, ?MWC59_XS1),
    CX2 bxor ?BSL(59, CX2, ?MWC59_XS2).

-doc """
Returns the generator value `V` from a generator state `CX`, as a `t:float/0`.
The generator state is scrambled as with
[`mwc59_value/1` ](`mwc59_value/1`)before converted to a `t:float/0`.
""".
-doc(#{title => <<"Niche algorithms API">>,since => <<"OTP 25.0">>}).
-spec mwc59_float(CX :: mwc59_state()) -> V :: float().
mwc59_float(CX1) when is_integer(CX1), 1 =< CX1, CX1 < ?MWC59_P ->
    CX = ?MASK(53, CX1),
    CX2 = CX bxor ?BSL(53, CX, ?MWC59_XS1),
    CX3 = CX2 bxor ?BSL(53, CX2, ?MWC59_XS2),
    CX3 * ?TWO_POW_MINUS53.

-doc(#{equiv => mwc59_seed/1}).
-doc(#{title => <<"Niche algorithms API">>,since => <<"OTP 25.0">>}).
-spec mwc59_seed() -> CX :: mwc59_state().
mwc59_seed() ->
    {A1, A2, A3} = default_seed(),
    X1 = hash58(A1),
    X2 = hash58(A2),
    X3 = hash58(A3),
    (X1 bxor X2 bxor X3) + 1.

-doc """
Returns a generator state `CX`. `S` is hashed to create the generator state, to
avoid that similar seeds create similar sequences.

Without `S`, the generator state is created as for
[`seed_s(atom())`](`seed_s/1`).
""".
-doc(#{title => <<"Niche algorithms API">>,since => <<"OTP 25.0">>}).
-spec mwc59_seed(S :: 0..?MASK(58)) -> CX :: mwc59_state().
mwc59_seed(S) when is_integer(S), 0 =< S, S =< ?MASK(58) ->
    hash58(S) + 1.

%% Constants a'la SplitMix64, MurMurHash, etc.
%% Not that critical, just mix the bits using bijections
%% (reversible mappings) to not have any two user input seeds
%% become the same generator start state.
%%
hash58(X) ->
    X0 = ?MASK(58, X),
    X1 = ?MASK(58, (X0 bxor (X0 bsr 29)) * 16#351afd7ed558ccd),
    X2 = ?MASK(58, (X1 bxor (X1 bsr 29)) * 16#0ceb9fe1a85ec53),
    X2 bxor (X2 bsr 29).


%% =====================================================================
%% Mask and fill state list, ensure not all zeros
%% =====================================================================

seed58_nz(N, Ss) ->
    seed_nz(N, Ss, 58, false).

seed64_nz(N, Ss) ->
    seed_nz(N, Ss, 64, false).

seed_nz(_N, [], _M, false) ->
    erlang:error(zero_seed);
seed_nz(0, [_|_], _M, _NZ) ->
    erlang:error(too_many_seed_integers);
seed_nz(0, [], _M, _NZ) ->
    [];
seed_nz(N, [], M, true) ->
    [0|seed_nz(N - 1, [], M, true)];
seed_nz(N, [S|Ss], M, NZ) ->
    if
	is_integer(S) ->
	    R = ?MASK(M, S),
	    [R|seed_nz(N - 1, Ss, M, NZ orelse R =/= 0)];
	true ->
	    erlang:error(non_integer_seed)
    end.

%% =====================================================================
%% Splitmix seeders, lowest bits of SplitMix64, zeros skipped
%% =====================================================================

-doc false.
-spec seed58(non_neg_integer(), uint64()) -> list(uint58()).
seed58(0, _X) ->
    [];
seed58(N, X) ->
    {Z,NewX} = seed58(X),
    [Z|seed58(N - 1, NewX)].
%%
seed58(X_0) ->
    {Z0,X} = splitmix64_next(X_0),
    case ?MASK(58, Z0) of
	0 ->
	    seed58(X);
	Z ->
	    {Z,X}
    end.

-spec seed64(non_neg_integer(), uint64()) -> list(uint64()).
seed64(0, _X) ->
    [];
seed64(N, X) ->
    {Z,NewX} = seed64(X),
    [Z|seed64(N - 1, NewX)].
%%
seed64(X_0) ->
    {Z,X} = ZX = splitmix64_next(X_0),
    if
	Z =:= 0 ->
	    seed64(X);
	true ->
	    ZX
    end.

%% =====================================================================
%% The SplitMix64 generator:
%%
%% uint64_t splitmix64_next() {
%% 	uint64_t z = (x += 0x9e3779b97f4a7c15);
%% 	z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9;
%% 	z = (z ^ (z >> 27)) * 0x94d049bb133111eb;
%% 	return z ^ (z >> 31);
%% }
%%

-doc "Algorithm specific state".
-type splitmix64_state() :: uint64().

-doc """
Returns a random 64-bit integer `X` and a new generator state `NewAlgState`,
according to the SplitMix64 algorithm.

This generator is used internally in the `rand` module for seeding other
generators since it is of a quite different breed which reduces the probability
for creating an accidentally bad seed.
""".
-doc(#{title => <<"Niche algorithms API">>,since => <<"OTP 25.0">>}).
-spec splitmix64_next(AlgState :: integer()) ->
                             {X :: uint64(), NewAlgState :: splitmix64_state()}.
splitmix64_next(X_0) ->
    X = ?MASK(64, X_0 + 16#9e3779b97f4a7c15),
    Z_0 = ?MASK(64, (X bxor (X bsr 30)) * 16#bf58476d1ce4e5b9),
    Z_1 = ?MASK(64, (Z_0 bxor (Z_0 bsr 27)) * 16#94d049bb133111eb),
    {?MASK(64, Z_1 bxor (Z_1 bsr 31)),X}.

%% =====================================================================
%% Polynomial jump with a jump constant word list,
%% high bit in each word marking top of word,
%% SR is a {Forward, Reverse} queue tuple with Forward never empty
%% =====================================================================

polyjump({Ss, Rs} = SR, NextState, JumpConst) ->
    %% Create new state accumulator T
    Ts = lists:duplicate(length(Ss) + length(Rs), 0),
    polyjump(SR, NextState, JumpConst, Ts).
%%
%% Foreach jump word
polyjump(_SR, _NextState, [], Ts) ->
    %% Return new calculated state
    {Ts, []};
polyjump(SR, NextState, [J|Js], Ts) ->
    polyjump(SR, NextState, Js, Ts, J).
%%
%% Foreach bit in jump word until top bit
polyjump(SR, NextState, Js, Ts, 1) ->
    polyjump(SR, NextState, Js, Ts);
polyjump({Ss, Rs} = SR, NextState, Js, Ts, J) when J =/= 0 ->
    NewSR = NextState(SR),
    NewJ = J bsr 1,
    case ?MASK(1, J) of
        0 ->
            polyjump(NewSR, NextState, Js, Ts, NewJ);
        1 ->
            %% Xor this state onto T
            polyjump(NewSR, NextState, Js, xorzip_sr(Ts, Ss, Rs), NewJ)
    end.

xorzip_sr([], [], undefined) ->
    [];
xorzip_sr(Ts, [], Rs) ->
    xorzip_sr(Ts, lists:reverse(Rs), undefined);
xorzip_sr([T|Ts], [S|Ss], Rs) ->
    [T bxor S|xorzip_sr(Ts, Ss, Rs)].

%% =====================================================================

-doc false.
format_jumpconst58(String) ->
    ReOpts = [{newline,any},{capture,all_but_first,binary},global],
    {match,Matches} = re:run(String, "0x([a-zA-Z0-9]+)", ReOpts),
    format_jumcons58_matches(lists:reverse(Matches), 0).

format_jumcons58_matches([], J) ->
    format_jumpconst58_value(J);
format_jumcons58_matches([[Bin]|Matches], J) ->
    NewJ = (J bsl 64) bor binary_to_integer(Bin, 16),
    format_jumcons58_matches(Matches, NewJ).

format_jumpconst58_value(0) ->
    ok;
format_jumpconst58_value(J) ->
    io:format("16#~s,~n", [integer_to_list(?MASK(58, J) bor ?BIT(58), 16)]),
    format_jumpconst58_value(J bsr 58).

%% =====================================================================
%% Ziggurat cont
%% =====================================================================
-define(NOR_R, 3.6541528853610087963519472518).
-define(NOR_INV_R, 1/?NOR_R).

%% return a {sign, Random51bits, State}
get_52({#{bits:=Bits, next:=Next} = AlgHandler, S0}) ->
    %% Use the high bits
    {Int,S1} = Next(S0),
    {?BIT(Bits - 51 - 1) band Int, Int bsr (Bits - 51), {AlgHandler, S1}};
get_52({#{next:=Next} = AlgHandler, S0}) ->
    {Int,S1} = Next(S0),
    {?BIT(51) band Int, ?MASK(51, Int), {AlgHandler, S1}}.

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

-doc false.
bc64(V) -> ?BC(V, 64).

%% Linear from high bit - higher probability first gives faster execution
bc(V, B, N) when B =< V -> N;
bc(V, B, N) -> bc(V, B bsr 1, N - 1).


%%% %% Non-negative rem
%%% mod(Q, X) when 0 =< X, X < Q ->
%%%     X;
%%% mod(Q, X) ->
%%%     Y = X rem Q,
%%%     if
%%%         Y < 0 ->
%%%             Y + Q;
%%%         true ->
%%%             Y
%%%     end.


-doc false.
make_float(S, E, M) ->
    <<F/float>> = <<S:1, E:11, M:52>>,
    F.

-doc false.
float2str(N) ->
    <<S:1, E:11, M:52>> = <<(float(N))/float>>,
    lists:flatten(
      io_lib:format(
      "~c~c.~13.16.0bE~b",
      [case S of 1 -> $-; 0 -> $+ end,
       case E of 0 -> $0; _ -> $1 end,
       M, E - 16#3ff])).
