%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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
%% This module tests the ssa_opt_no_reuse compiler pass. The test
%% strategy is to ensure 100% coverage of the pass and to check for
%% correct functioning for each of the different categories of
%% operations inhibiting reuse: phis, known bifs, functions and
%% instructions.
%%

-module(no_reuse_hint).

-export([coverage0/0,
         inhibit_by_known_bif/2,
         inhibit_by_known_fun/1,
         inhibit_by_known_op/2,
         inhibit_by_map/4,
         inhibit_by_map_key/2,
         inhibit_by_map_value/2,
         inhibit_by_phi/1]).

inhibit_by_known_bif({_}=X, Y) ->
%ssa% (X, Y) when post_ssa_opt ->
%ssa% Bif = bif:'+'(...),
%ssa% R = update_record(copy, 1, X, 1, Bif),
%ssa% ret(R).
    setelement(1, X, Y + 1).

inhibit_by_known_fun(X) ->
    case X of
	{_} ->
%ssa% (X) when post_ssa_opt ->
%ssa% KnownFun = call(fun erlang:'++'/2, ...),
%ssa% R = update_record(copy, 1, X, 1, KnownFun),
%ssa% ret(R).
	    setelement(1, X, e:f() ++ e:f())
    end.

inhibit_by_known_op({_, _}=X, Y) ->
%ssa% (X, Y) when post_ssa_opt ->
%ssa% Op = put_tuple(...),
%ssa% R = update_record(copy, 2, X, 1, Op),
%ssa% ret(R).
    setelement(1, X, {Y}).

inhibit_by_map(A, B, C, {_}=D) ->
%ssa% (A, B, C, D) when post_ssa_opt ->
%ssa% Map1 = put_map(_, C, B, _),
%ssa% Map = put_map(_, Map1, A, _),
%ssa% R = update_record(copy, 1, D, 1, Map),
%ssa% ret(R).
    setelement(1, D, C#{B => {e:f()}, A => e:f()}).

inhibit_by_map_key({Y0}=Z, K) ->
%ssa% (X, Y) when post_ssa_opt ->
%ssa% Key = put_tuple(...),
%ssa% Map = put_map(_, _, Key, value),
%ssa% R = update_record(copy, 1, Z, 1, Map),
%ssa% ret(R).
    Y = Y0#{{K} => value},
    setelement(1, Z, Y).

inhibit_by_map_value(X, {Y}=Z) ->
%ssa% (X, Y) when post_ssa_opt ->
%ssa% T = put_tuple(...),
%ssa% Map = put_map(_, _, key, T),
%ssa% R = update_record(copy, 1, Z, 1, Map),
%ssa% ret(R).
    M = Y#{key => {X}},
    setelement(1, Z, M).

inhibit_by_phi({_}=X) ->
%ssa% (X) when post_ssa_opt ->
%ssa% Phi = phi(...),
%ssa% R = update_record(copy, 1, X, 1, Phi),
%ssa% ret(R).
    Y = case e:f() of
            a -> {e:f(), 1};
            b -> {e:f(), 2}
        end,
    setelement(1, X, Y).

%%
%% Ensure full coverage of the functions in the ssa_opt_no_reuse pass.
%%
coverage0() ->
    erlang:send_after(500, self(), fun() -> ok end).
