-module(inlined_funs).
-export([go/2]).

-compile({inline, [f/2]}).

-define(breadcrumb(Pid), breadcrumb(Pid, {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, ?LINE)).

go(Pid, X0) ->
    X1 = f(Pid, X0),
    X2 = f(Pid, X1),
    Pid ! {done, self(), X2}.

f(Pid, X) ->
    ?breadcrumb(Pid),
    X + 1.

breadcrumb(Pid, MFA, Line) ->
    Pid ! {executed, self(), MFA, {line, Line}}.

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
