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
%% Some generators of the QuickCheck framework used by OTP for property tests
%% create atoms at random, ie from random strings, and are therefore likely
%% to exhaust the atom table.
%%
%% This module provides additional variants of these generators which do
%% not create new atoms but pick from the already existing atoms.
%%
%% Other than in QuickCheck, the respective atom generators provided by this module
%% do not shrink.

-module(ct_quickcheck_ext).
-moduledoc false.

-export([existing_atom/0]).
-export([safe_any/0]).
-export([safe_atom/0]).
-export([safe_list/0]).
-export([safe_map/0]).
-export([safe_term/0]).
-export([safe_tuple/0]).

%% Atomlimit-safe variant of `eqc_gen:list()'
-spec safe_list() -> eqc_gen:gen(list()).
safe_list() ->
    eqc_gen:list(safe_any()).


%% Atomlimit-safe variant of `eqc_gen:map()'
-spec safe_map() -> eqc_gen:gen(map()).
safe_map() ->
    eqc_gen:map(safe_any(), safe_any()).


%% Atomlimit-safe variant of tuple generator
-spec safe_tuple() -> eqc_gen:gen(tuple()).
safe_tuple() ->
    eqc_gen:bind(eqc_gen:list(safe_any()), fun(L) -> list_to_tuple(L) end).


%% Atomlimit-safe atom generator.
-spec existing_atom() -> eqc_gen:gen(atom()).
existing_atom() ->
    existing_atom(atom_count()).

existing_atom(N) ->
    eqc_gen:noshrink(
      eqc_gen:bind(eqc_gen:choose(0, N - 1), fun(X) -> get_existing_atom(X) end)).

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

%% Atomlimit-safe atom generator.
%% Like `existing_atom()', but also emphasizes some common atoms
%% like `undefined', `false', `ok' etc
-spec safe_atom() -> eqc_gen:gen(atom()).
safe_atom() ->
    safe_atom(atom_count()).

safe_atom(N) ->
    eqc_gen:oneof([eqc_gen:oneof(['', true, false, ok,
                                   error, undefined,
                                   infinity, 'ätöm',
                                   '原子', '_', '"',
                                   '\'', '\\', '+', '-',
                                   '*', '/', '(', ')',
                                   '[', ']', '{', '}',
                                   '#']),
                   existing_atom(N)]).

%% Atomlimit-safe variant of term generator.
%% Alias for `safe_any/0'.
-spec safe_term() -> eqc_gen:gen(term()).
safe_term() ->
    safe_any().

%% Atomlimit-safe variant of `eqc_gen:any()'.
-spec safe_any() -> eqc_gen:gen(term()).
safe_any() ->
    N = atom_count(),
    eqc_gen:sized(fun(Size) -> safe_any(N, Size) end).

%% I have simplified 'safe_any/2' here, it is missing the distribution function that
%% ct_proper_ext have.
%% The solution in Proper uses a lot of random calls,
%% but that didn't work with Quickcheck combined with functionX generators
%% that uses safe_any() as type.
%% My guess is that Quickcheck is more lazy and funs becomes non-functional
%% and returns random output on the same input, when rand is invoked.

safe_any(N, 0) ->
    eqc_gen:oneof([safe_atom(N),
                   eqc_gen:int(),
                   eqc_gen:real()]);

safe_any(N, Size) ->
    eqc_gen:frequency(
      [{25, eqc_gen:bind(eqc_gen:choose(0, Size), make_tuple(N, Size))},
       {25, eqc_gen:bind(eqc_gen:choose(0, Size), make_list(N, Size))},
       {12, eqc_gen:resize(Size, eqc_gen:bitstring())},
       {38, safe_any(N,0)}]).

make_list(N, Size) ->
    fun(X) ->
            case X of
                0 ->
                    [];
                1 ->
                    [eqc_gen:lazy(fun() -> safe_any(N, Size - 1) end)];
                _ ->
                    Content = eqc_gen:lazy(fun() -> safe_any(N, Size div X) end),
                    eqc_gen:list(X, Content)
            end
    end.

make_tuple(N, Size) ->
    fun(X) ->
            case X of
                0 ->
                    {};
                1 ->
                    {eqc_gen:lazy(fun() -> safe_any(N, Size - 1) end)};
                _ ->
                    Content = eqc_gen:lazy(fun() -> safe_any(N, Size div X) end),
                    list_to_tuple(lists:duplicate(X, Content))
            end
    end.

%% Number of currently existing atoms
atom_count() ->
    erlang:system_info(atom_count).
