%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
%% Copyright Richard Carlsson 2026. All Rights Reserved.
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
-module(cprof_SUITE_test).

-export([seq/3, seq_r/3]).



%% Stack recursive seq
seq(Stop, Stop, Succ) when is_function(Succ) ->
    [Stop];
seq(Start, Stop, Succ) when is_function(Succ) ->
    [Start | seq(Succ(Start), Stop, Succ)].



%% Tail recursive seq, result list is reversed
seq_r(Start, Stop, Succ) when is_function(Succ) ->
    seq_r(Start, Stop, Succ, []).

seq_r(Stop, Stop, _, R) ->
    [Stop | R];
seq_r(Start, Stop, Succ, R) ->
    seq_r(Succ(Start), Stop, Succ, [Start | R]).



