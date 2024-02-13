%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
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
-module(random).
-moduledoc """
Pseudo-random number generation.

This module provides a random number generator. The method is attributed to B.A.
Wichmann and I.D. Hill in 'An efficient and portable pseudo-random number
generator', Journal of Applied Statistics. AS183. 1982. Also Byte March 1987.

The algorithm is a modification of the version attributed to Richard A. O'Keefe
in the standard Prolog library.

Every time a random number is requested, a state is used to calculate it, and a
new state is produced. The state can either be implicit (kept in the process
dictionary) or be an explicit argument and return value. In this implementation,
the state (the type `t:ran/0`) consists of a tuple of three integers.

> #### Note {: .info }
>
> This random number generator is not cryptographically strong. If a strong
> cryptographic random number generator is needed, use one of functions in the
> `m:crypto` module, for example, [`crypto:strong_rand_bytes/1`](`m:crypto`).

> #### Note {: .info }
>
> The improved `m:rand` module is to be used instead of this module.

## Note

Some of the functions use the process dictionary variable `random_seed` to
remember the current seed.

If a process calls `uniform/0` or `uniform/1` without setting a seed first,
`seed/0` is called automatically.

The implementation changed in Erlang/OTP R15. Upgrading to R15 breaks
applications that expect a specific output for a specified seed. The output is
still deterministic number series, but different compared to releases older than
R15. Seed `{0,0,0}` does, for example, no longer produce a flawed series of only
zeros.
""".
-deprecated({'_','_',"use the 'rand' module instead"}).

%% Reasonable random number generator.
%%  The method is attributed to B. A. Wichmann and I. D. Hill
%%  See "An efficient and portable pseudo-random number generator",
%%  Journal of Applied Statistics. AS183. 1982. Also Byte March 1987.

-export([seed/0, seed/1, seed/3, uniform/0, uniform/1,
	 uniform_s/1, uniform_s/2, seed0/0]).

-define(PRIME1, 30269).
-define(PRIME2, 30307).
-define(PRIME3, 30323).

%%-----------------------------------------------------------------------
%% The type of the state

-doc "The state.".
-type ran() :: {integer(), integer(), integer()}.

%%-----------------------------------------------------------------------

-doc "Returns the default state.".
-spec seed0() -> ran().

seed0() ->
    {3172, 9814, 20125}.

%% seed()
%%  Seed random number generation with default values

-doc """
Seeds random number generation with default (fixed) values in the process
dictionary and returns the old state.
""".
-spec seed() -> ran().

seed() ->
    case seed_put(seed0()) of
	undefined -> seed0();
	{_,_,_} = Tuple -> Tuple
    end.	


%% seed({A1, A2, A3}) 
%%  Seed random number generation 

-doc """
[`seed({A1, A2, A3})`](`seed/1`) is equivalent to
[`seed(A1, A2, A3)`](`seed/3`).
""".
-spec seed(SValue) -> 'undefined' | ran() when
      SValue :: {A1, A2, A3} | integer(),
      A1 :: integer(),
      A2 :: integer(),
      A3 :: integer().

seed(Int) when is_integer(Int) ->
    A1 = (Int bsr 16) band 16#fffffff,
    A2 = Int band 16#ffffff,
    A3 = (Int bsr 36) bor (A2 bsr 16),
    seed(A1, A2, A3);
seed({A1, A2, A3}) ->
    seed(A1, A2, A3).

%% seed(A1, A2, A3) 
%%  Seed random number generation 

-doc """
Seeds random number generation with integer values in the process dictionary and
returns the old state.

The following is an easy way of obtaining a unique value to seed with:

```erlang
random:seed(erlang:phash2([node()]),
            erlang:monotonic_time(),
            erlang:unique_integer())
```

For details, see `erlang:phash2/1`, `erlang:node/0`, `erlang:monotonic_time/0`,
and `erlang:unique_integer/0`.
""".
-spec seed(A1, A2, A3) -> 'undefined' | ran() when
      A1 :: integer(),
      A2 :: integer(),
      A3 :: integer().

seed(A1, A2, A3) ->
    seed_put({(abs(A1) rem (?PRIME1-1)) + 1,   % Avoid seed numbers that are
	      (abs(A2) rem (?PRIME2-1)) + 1,   % even divisors of the
	      (abs(A3) rem (?PRIME3-1)) + 1}). % corresponding primes.


-spec seed_put(ran()) -> 'undefined' | ran().
     
seed_put(Seed) ->
    put(random_seed, Seed).

%% uniform()
%%  Returns a random float between 0 and 1.

-doc """
Returns a random float uniformly distributed between `0.0` and `1.0`, updating
the state in the process dictionary.
""".
-spec uniform() -> float().

uniform() ->
    {A1, A2, A3} = case get(random_seed) of
		       undefined -> seed0();
		       Tuple -> Tuple
		   end,
    B1 = (A1*171) rem ?PRIME1,
    B2 = (A2*172) rem ?PRIME2,
    B3 = (A3*170) rem ?PRIME3,
    put(random_seed, {B1,B2,B3}),
    R = B1/?PRIME1 + B2/?PRIME2 + B3/?PRIME3,
    R - trunc(R).

%% uniform(N) -> I
%%  Given an integer N >= 1, uniform(N) returns a random integer
%%  between 1 and N.

-doc """
Returns, for a specified integer `N >= 1`, a random integer uniformly
distributed between `1` and `N`, updating the state in the process dictionary.
""".
-spec uniform(N) -> pos_integer() when
      N :: pos_integer().

uniform(N) when is_integer(N), N >= 1 ->
    trunc(uniform() * N) + 1.


%%% Functional versions

%% uniform_s(State) -> {F, NewState}
%%  Returns a random float between 0 and 1.

-doc """
Returns, for a specified state, a random float uniformly distributed between
`0.0` and `1.0`, and a new state.
""".
-spec uniform_s(State0) -> {float(), State1} when
      State0 :: ran(),
      State1 :: ran().

uniform_s({A1, A2, A3}) ->
    B1 = (A1*171) rem ?PRIME1,
    B2 = (A2*172) rem ?PRIME2,
    B3 = (A3*170) rem ?PRIME3,
    R = B1/?PRIME1 + B2/?PRIME2 + B3/?PRIME3,
    {R - trunc(R), {B1,B2,B3}}.

%% uniform_s(N, State) -> {I, NewState}
%%  Given an integer N >= 1, uniform(N) returns a random integer
%%  between 1 and N.

-doc """
Returns, for a specified integer `N >= 1` and a state, a random integer
uniformly distributed between `1` and `N`, and a new state.
""".
-spec uniform_s(N, State0) -> {integer(), State1} when
      N :: pos_integer(),
      State0 :: ran(),
      State1 :: ran().

uniform_s(N, State0) when is_integer(N), N >= 1 ->
    {F, State1} = uniform_s(State0),
    {trunc(F * N) + 1, State1}.
