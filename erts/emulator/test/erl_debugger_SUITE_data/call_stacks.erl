-module(call_stacks).
-export([three_levels/2, args_as_yvars/3, call_with_catches/1, sync_and_hibernate/0]).

three_levels(X, Y) ->
    Foo = two_levels(X + 1, Y * 2),
    Foo + 3.

two_levels(X, Y) ->
    Bar = one_level(X * Y),
    Bar * 21.

one_level(X) ->
    Hey = base_level(X * 5),
    Hey + 1.

base_level(X) ->
    erl_debugger_SUITE ! {sync, self()},
    receive continue -> ok end,
    X * 25.

args_as_yvars(X, Y, Z) ->
    erl_debugger_SUITE ! {sync, self()},
    receive continue -> ok end,
    {X, Y, Z}.

call_with_catches(X) ->
    Y = catch call_with_catches_aux(X + 1),
    {X, Y}.

call_with_catches_aux(Z) ->
    try
        erl_debugger_SUITE ! {sync, self()},
        receive continue -> ok end
    catch _:_ ->
        error({bam, Z})
    end.

sync_and_hibernate() ->
    erl_debugger_SUITE ! {sync, self()},
    erlang:hibernate(?MODULE, three_levels, [10, 20]).

%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright (c) Meta Platforms, Inc. and affiliates.
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
