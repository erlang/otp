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
%% This module tests functions which have previously crashed the
%% compiler when the `no_type_opt` option was used.
%%

-module(non_throwing_bifs).
-export([try_bif1/1, try_bif2/2, try_bif3/1]).

try_bif1(B) ->
%ssa% (B) when post_ssa_opt ->
%ssa% X = bif:binary_to_atom(B, utf8),
%ssa% Y = succeeded:guard(X),
%ssa% ret(X).
    try binary_to_atom(B)
    catch _:_ -> []
    end.

try_bif2(A, B) ->
%ssa% (A, B) when post_ssa_opt ->
%ssa% X = bif:binary_to_atom(A, B),
%ssa% Y = succeeded:guard(X),
%ssa% ret(X).
    try binary_to_atom(A, B)
    catch _:_ -> []
    end.

try_bif3(A) ->
%ssa% (A) when post_ssa_opt ->
%ssa% X = call(fun erlang:float_to_list/1, A),
%ssa% Y = succeeded:body(X),
%ssa% br(Y, Succ, Fail),
%ssa% label Succ,
%ssa% ret(X),
%ssa% label Fail,
%ssa% ret([]).
    try float_to_list(A)
    catch _:_ -> []
    end.
