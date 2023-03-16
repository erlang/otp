%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2023. All Rights Reserved.
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
%% This module tests that beam_ssa_alias_opt:opt/2 does not annotate
%% instructions with information about aliased operands when more than
%% MAX_REPETITIONS are required for the analysis to converge.
%%

-compile(no_ssa_opt_destructive_update).

-module(alias_non_convergence).

-export([a/1]).

%% a/17 is designed to trigger the iteration limit for alias analysis,
%% check that no alias information is generated.
a(X) ->
%ssa% fail (A) when post_ssa_opt ->
%ssa% _ = call(...) { aliased => [A] }.
    a(X, a, b, c, d, e, f, g, h, i, j, k, l, m, o, p, q).

a([X|Xs], A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, _) ->
    a(Xs, X, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O);
a([], A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) ->
    {A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P}.
