%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%% =====================================================================
%% Multiple PRNG module for Erlang/OTP
%% Copyright (c) 2015 Kenji Rikitake
%% =====================================================================

-module(rand).

-export([seed_s/1, seed_s/2, seed/1, seed/2,
	 export_seed/0, export_seed_s/1,
         uniform/0, uniform/1, uniform_s/1, uniform_s/2]).

-compile({inline, [exs64_next/1, exsplus_next/1,
		   exs1024_next/1, exs1024_calc/2]}).

-define(DEFAULT_ALG_HANDLER, exsplus).
-define(SEED_DICT, rand_seed).

%% =====================================================================
%% Types
%% =====================================================================

%% This depends on the algorithm handler function
-opaque alg_seed() :: exs64_state() | exsplus_state() | exs1024_state().
%% This is the algorithm handler function within this module
-type alg_handler() :: #{type      => alg(),
			 max       => integer(),
			 uniform   => fun(),
			 uniform_n => fun()}.

%% Internal state
-type state() :: {alg_handler(), alg_seed()}.
-type alg() :: exs64 | exsplus | exs1024.
-export_type([alg/0, alg_handler/0, state/0, alg_seed/0]).

%% =====================================================================
%% API
%% =====================================================================

%% Return algorithm and seed so that RNG state can be recreated with seed/1
-spec export_seed() -> undefined | {alg(), alg_seed()}.
export_seed() ->
    case seed_get() of
	{#{type:=Alg}, Seed} -> {Alg, Seed};
	_ -> undefined
    end.

-spec export_seed_s(state()) -> {alg(), alg_seed()}.
export_seed_s({#{type:=Alg}, Seed}) -> {Alg, Seed}.

%% seed(Alg) seeds RNG with runtime dependent values
%% and return the NEW state

%% seed({Alg,Seed}) setup RNG with a previously exported seed
%% and return the NEW state

-spec seed(alg() | {alg(), alg_seed()}) -> state().
seed(Alg) ->
    R = seed_s(Alg),
    _ = seed_put(R),
    R.

-spec seed_s(alg() | {alg(), alg_seed()}) -> state().
seed_s(Alg) when is_atom(Alg) ->
    seed_s(Alg, {erlang:phash2([{node(),self()}]),
		 erlang:system_time(),
		 erlang:unique_integer()});
seed_s({Alg0, Seed}) ->
    {Alg,_SeedFun} = mk_alg(Alg0),
    {Alg, Seed}.

%% seed/2: seeds RNG with the algorithm and given values
%% and returns the NEW state.

-spec seed(Alg :: alg(), {integer(), integer(), integer()}) -> state().
seed(Alg0, S0) ->
    State = seed_s(Alg0, S0),
    _ = seed_put(State),
    State.

-spec seed_s(Alg :: alg(), {integer(), integer(), integer()}) -> state().
seed_s(Alg0, S0 = {_, _, _}) ->
    {Alg, Seed} = mk_alg(Alg0),
    AS = Seed(S0),
    {Alg, AS}.

%%% uniform/0, uniform/1, uniform_s/1, uniform_s/2 are all
%%% uniformly distributed random numbers.

%% uniform/0: returns a random float X where 0.0 < X < 1.0,
%% updating the state in the process dictionary.

-spec uniform() -> float().
uniform() ->
    {X, Seed} = uniform_s(seed_get()),
    _ = seed_put(Seed),
    X.

%% uniform/1: given an integer N >= 1,
%% uniform/1 returns a random integer X where 1 =< X =< N,
%% updating the state in the process dictionary.

-spec uniform(N :: pos_integer()) -> pos_integer().
uniform(N) ->
    {X, Seed} = uniform_s(N, seed_get()),
    _ = seed_put(Seed),
    X.

%% uniform_s/1: given a state, uniform_s/1
%% returns a random float X where 0.0 < X < 1.0,
%% and a new state.

-spec uniform_s(state()) -> {float(), NewS :: state()}.
uniform_s(State = {#{uniform:=Uniform}, _}) ->
    Uniform(State).

%% uniform_s/2: given an integer N >= 1 and a state, uniform_s/2
%% uniform_s/2 returns a random integer X where 1 =< X =< N,
%% and a new state.

-spec uniform_s(N::pos_integer(), state()) -> {pos_integer(), NewS::state()}.
uniform_s(N, State = {#{uniform_n:=Uniform, max:=Max}, _})
  when 0 < N, N =< Max ->
    Uniform(N, State);
uniform_s(N, State0 = {#{uniform:=Uniform}, _})
  when is_integer(N), 0 < N ->
    {F, State} = Uniform(State0),
    {trunc(F * N) + 1, State}.

%% =====================================================================
%% Internal functions

-define(UINT21MASK, 16#00000000001fffff).
-define(UINT32MASK, 16#00000000ffffffff).
-define(UINT33MASK, 16#00000001ffffffff).
-define(UINT39MASK, 16#0000007fffffffff).
-define(UINT58MASK, 16#03ffffffffffffff).
-define(UINT64MASK, 16#ffffffffffffffff).

-type uint64() :: 0..16#ffffffffffffffff.
-type uint58() :: 0..16#03ffffffffffffff.

-spec seed_put(state()) -> undefined | state().
seed_put(Seed) ->
    put(?SEED_DICT, Seed).

seed_get() ->
    case get(?SEED_DICT) of
        undefined -> seed(?DEFAULT_ALG_HANDLER);
        Old -> Old  % no type checking here
    end.

%% Setup alg record
mk_alg(exs64) ->
    {#{type=>exs64, max=>?UINT64MASK,
       uniform=>fun exs64_uniform/1, uniform_n=>fun exs64_uniform/2},
     fun exs64_seed/1};
mk_alg(exsplus) ->
    {#{type=>exsplus, max=>?UINT58MASK,
       uniform=>fun exsplus_uniform/1, uniform_n=>fun exsplus_uniform/2},
     fun exsplus_seed/1};
mk_alg(exs1024) ->
    {#{type=>exs1024, max=>?UINT64MASK,
       uniform=>fun exs1024_uniform/1, uniform_n=>fun exs1024_uniform/2},
     fun exs1024_seed/1}.

%% =====================================================================
%% exs64 PRNG: Xorshift64*
%% Algorithm by Sebastiano Vigna
%% Reference URL: http://xorshift.di.unimi.it/
%% =====================================================================

-type exs64_state() :: uint64().

exs64_seed({A1, A2, A3}) ->
    {V1, _} = exs64_next(((A1 band ?UINT32MASK) * 4294967197 + 1)),
    {V2, _} = exs64_next(((A2 band ?UINT32MASK) * 4294967231 + 1)),
    {V3, _} = exs64_next(((A3 band ?UINT32MASK) * 4294967279 + 1)),
    ((V1 * V2 * V3) rem (?UINT64MASK - 1)) + 1.

%% Advance xorshift64* state for one step and generate 64bit unsigned integer
-spec exs64_next(exs64_state()) -> {uint64(), exs64_state()}.
exs64_next(R) ->
    R1 = R bxor (R bsr 12),
    R2 = R1 bxor ((R1 band ?UINT39MASK) bsl 25),
    R3 = R2 bxor (R2 bsr 27),
    {(R3 * 2685821657736338717) band ?UINT64MASK, R3}.

exs64_uniform({Alg, R0}) ->
    {V, R1} = exs64_next(R0),
    {V / 18446744073709551616, {Alg, R1}}.

exs64_uniform(Max, {Alg, R}) ->
    {V, R1} = exs64_next(R),
    {(V rem Max) + 1, {Alg, R1}}.

%% =====================================================================
%% exsplus PRNG: Xorshift116+
%% Algorithm by Sebastiano Vigna
%% Reference URL: http://xorshift.di.unimi.it/
%% 58 bits fits into an immediate on 64bits erlang and is thus much faster.
%% Modification of the original Xorshift128+ algorithm to 116
%% by Sebastiano Vigna, a lot of thanks for his help and work.
%% =====================================================================
-type exsplus_state() :: [uint58()|uint58()].

exsplus_seed({A1, A2, A3}) ->
    {_, R1} = exsplus_next([(((A1 * 4294967197) + 1) band ?UINT58MASK)|
			    (((A2 * 4294967231) + 1) band ?UINT58MASK)]),
    {_, R2} = exsplus_next([(((A3 * 4294967279) + 1) band ?UINT58MASK)|
			    tl(R1)]),
    R2.

%% Advance xorshift116+ state for one step and generate 58bit unsigned integer
-spec exsplus_next(exsplus_state()) -> {uint58(), exsplus_state()}.
exsplus_next([S1|S0]) ->
    %% Note: members s0 and s1 are swapped here
    S11 = (S1 bxor (S1 bsl 24)) band ?UINT58MASK,
    S12 = S11 bxor S0 bxor (S11 bsr 11) bxor (S0 bsr 41),
    {(S0 + S12) band ?UINT58MASK, [S0|S12]}.

exsplus_uniform({Alg, R0}) ->
    {I, R1} = exsplus_next(R0),
    {I / (?UINT58MASK+1), {Alg, R1}}.

exsplus_uniform(Max, {Alg, R}) ->
    {V, R1} = exsplus_next(R),
    {(V rem Max) + 1, {Alg, R1}}.

%% =====================================================================
%% exs1024 PRNG: Xorshift1024*
%% Algorithm by Sebastiano Vigna
%% Reference URL: http://xorshift.di.unimi.it/
%% =====================================================================

-type exs1024_state() :: {list(uint64()), list(uint64())}.

exs1024_seed({A1, A2, A3}) ->
    B1 = (((A1 band ?UINT21MASK) + 1) * 2097131) band ?UINT21MASK,
    B2 = (((A2 band ?UINT21MASK) + 1) * 2097133) band ?UINT21MASK,
    B3 = (((A3 band ?UINT21MASK) + 1) * 2097143) band ?UINT21MASK,
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
    S11 = S1 bxor ((S1 band ?UINT33MASK) bsl 31),
    S12 = S11 bxor (S11 bsr 11),
    S01 = S0 bxor (S0 bsr 30),
    NS1 = S01 bxor S12,
    {(NS1 * 1181783497276652981) band ?UINT64MASK, NS1}.

%% Advance xorshift1024* state for one step and generate 64bit unsigned integer
-spec exs1024_next(exs1024_state()) -> {uint64(), exs1024_state()}.
exs1024_next({[S0,S1|L3], RL}) ->
    {X, NS1} = exs1024_calc(S0, S1),
    {X, {[NS1|L3], [S0|RL]}};
exs1024_next({[H], RL}) ->
    NL = [H|lists:reverse(RL)],
    exs1024_next({NL, []}).

exs1024_uniform({Alg, R0}) ->
    {V, R1} = exs1024_next(R0),
    {V / 18446744073709551616, {Alg, R1}}.

exs1024_uniform(Max, {Alg, R}) ->
    {V, R1} = exs1024_next(R),
    {(V rem Max) + 1, {Alg, R1}}.
