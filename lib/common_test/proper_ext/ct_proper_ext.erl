%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2023-2025. All Rights Reserved.
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

%% For internal use only.
%%
%% Some generators of the PropEr framework used by OTP for property tests
%% create atoms at random, ie from random strings, and are therefore likely
%% to exhaust the atom table.
%%
%% This module provides additional variants of these generators which do
%% not create new atoms but pick from the already existing atoms.
%%
%% Other than in PropEr, the respective atom generators provided by this module
%% do not shrink.

-module(ct_proper_ext).
-moduledoc false.

-export([existing_atom/0]).
-export([safe_any/0]).
-export([safe_atom/0]).
-export([safe_list/0]).
-export([safe_map/0]).
-export([safe_term/0]).
-export([safe_tuple/0]).

%% Atomlimit-safe variant of `proper_types:list()'
-spec safe_list() -> proper_types:type().
safe_list() ->
    proper_types:list(safe_any()).


%% Atomlimit-safe variant of `proper_types:map()'
-spec safe_map() -> proper_types:type().
safe_map() ->
    proper_types:map(safe_any(), safe_any()).


%% Atomlimit-safe variant of `proper_types:tuple()'
-spec safe_tuple() -> proper_types:type().
safe_tuple() ->
    proper_types:loose_tuple(safe_any()).


%% Atomlimit-safe variant of `proper_types:atom()'.
-spec existing_atom() -> proper_types:type().
existing_atom() ->
    existing_atom(atom_count()).

existing_atom(N) ->
    proper_types:noshrink(
        proper_types:lazy(fun() ->
                              get_existing_atom(rand_int0(N - 1))
                          end)).

-define(ATOM_TERM_BIN(Index), <<131, 75, Index:24>>).
get_existing_atom(Index) ->
    case binary_to_term(?ATOM_TERM_BIN(Index)) of
        '' ->
            '';
        Atom ->
            case hd(atom_to_list(Atom)) of
                $$ when Index > 0 ->
                    get_existing_atom(Index - 1);
                $$ ->
                    '';
                _ ->
                    Atom
            end
    end.


%% Atomlimit-safe variant of `proper_types:atom()'.
%% Like `existing_atom()', but also emphasizes some common atoms
%% like `undefined', `false', `ok' etc
-spec safe_atom() -> proper_types:type().
safe_atom() ->
    safe_atom(atom_count()).

safe_atom(N) ->
    proper_types:oneof([proper_types:oneof(['', true, false, ok,
                                            error, undefined,
                                            infinity, 'ätöm',
                                            '原子', '_', '"',
                                            '\'', '\\', '+', '-',
                                            '*', '/', '(', ')',
                                            '[', ']', '{', '}',
                                            '#']),
                        existing_atom(N)]).


%% Atomlimit-safe variant of `proper_types:term()'.
%% Alias for `safe_any/0'.
-spec safe_term() -> proper_types:type().
safe_term() ->
    safe_any().


%% Atomlimit-safe variant of `proper_types:any()'.
-spec safe_any() -> proper_types:type().
safe_any() ->
    N = atom_count(),
    proper_types:sized(fun(Size) -> safe_any(N, Size) end).

safe_any(N, 0) ->
    proper_types:oneof([safe_atom(N),
                        proper_types:integer(),
                        proper_types:float()]);
safe_any(N, Size) ->
    case pick_type(Size) of
        simple ->
            safe_any(N, 0);
        binary ->
            proper_types:resize(Size, proper_types:bitstring());
        {list, 0} ->
            [];
        {list, 1} ->
            [proper_types:lazy(fun() -> safe_any(N, Size - 1) end)];
        {list, NumEls} ->
            ElSizes = distribute(Size - 1, NumEls),
            proper_types:fixed_list([proper_types:lazy(fun() ->
                                                           safe_any(N, S)
                                                       end)
                                     || S <- ElSizes]);
        {tuple, 0} ->
            {};
        {tuple, 1} ->
            {proper_types:lazy(fun() -> safe_any(N, Size - 1) end)};
        {tuple, NumEls} ->
            ElSizes = distribute(Size - 1, NumEls),
            proper_types:tuple([proper_types:lazy(fun() ->
                                                      safe_any(N, S)
                                                  end)
                                || S <- ElSizes])
    end.

%% Randomly picks a type with the following distribution (same as in PropEr):
%% * 25% tuples
%% * 25% lists
%% * 12.5% bitstrings
%% * 37.5% simple types
pick_type(Size) ->
    case rand:uniform(1000) of
        X when X =< 250 ->
            {tuple, rand_int0(Size)};
        X when X =< 500 ->
            {list, rand_int0(Size)};
        X when X =< 625 ->
            binary;
        _ ->
            simple
    end.

%% Randomly distributes the given number of `Credits' over the given
%% number of `Slots'
distribute(Slots, Credits) ->
    [X || {_, X} <- lists:sort(distribute_1(Slots, Credits))].

distribute_1(0, 0) ->
    [];
distribute_1(1, Credits) ->
    [{rand:uniform(1000), Credits}];
distribute_1(Slots, 0) ->
    [{rand:uniform(1000), 0} || _ <- lists:seq(1, Slots)];
distribute_1(Slots, Credits) ->
    N = rand_int0(Credits),
    [{rand:uniform(1000), N}|distribute_1(Slots - 1, Credits - N)].


%% Random non-neg integer
rand_int0(Max) ->
    rand:uniform(Max + 1) - 1.


%% Number of currently existing atoms
atom_count() ->
    erlang:system_info(atom_count).
