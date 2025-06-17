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
%% Check that the compiler doesn't enter an infinite loop while trying
%% to run the destructive update pass.
%%
-module(tuple_inplace_abort3).

-export([f0/0,f1/0,f2/0,f3/0]).

-record(rec, {a, b = ext:ernal()}).

f0() ->
    g([#rec{}]).

f1() ->
    g({#rec{}}).

f2() ->
    g([[{#rec{}}]]).

f3() ->
    g([[[#rec{}]]]).

g({A}) ->
    g(A);
g([[A]]) ->
    g(A);
g([A]) ->
    g(A);
g(#rec{} = A) ->
%ssa% (_) when post_ssa_opt ->
%ssa% _ = update_record(reuse, 3, ...).
    A#rec{ a = a }.
