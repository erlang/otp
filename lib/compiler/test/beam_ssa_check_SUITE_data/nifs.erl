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
%% This module tests that beam_ssa_opt:opt/2 correctly handles modules
%% containing nifs.
%%
-module(nifs).

-export([load_nif/0, calling_normal_fun/0, calling_nif/0]).

-nifs([a_nif/0]).

load_nif() ->
    ok = erlang:load_nif("dummy", 0).

not_a_nif() ->
    1.

a_nif() ->
    2.

%% If beam_ssa_opt:isolate_nifs/1 and beam_ssa_opt:restore_nifs/2 fail
%% to do their jobs, and somehow disable beam_ssa_opt-optimizations,
%% the result of not_a_nif() + not_a_nif() won't be statically
%% evaluated.
calling_normal_fun() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(2),
%ssa% label 1,
%ssa% ret(_).
    not_a_nif() + not_a_nif().

%% If beam_ssa_opt:isolate_nifs/1 and beam_ssa_opt:restore_nifs/2 fail
%% to do their jobs and somehow allow the function marked as a NIF to
%% be statically evaluated, the addition will have been removed. Also
%% check that the local calls to the NIFs have been restored to a call
%% within the module.
calling_nif() ->
%ssa% () when post_ssa_opt ->
%ssa% A = call(fun a_nif/0),
%ssa% B = call(fun a_nif/0),
%ssa% Sum = bif:'+'(A, B),
%ssa% ret(Sum),
%ssa% label 1,
%ssa% ret(_).
    a_nif() + a_nif().
