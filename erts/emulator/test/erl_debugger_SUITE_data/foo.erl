-module(foo).
-export([go/1]).

-define(breadcrumb(Pid), breadcrumb(Pid, {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, ?LINE)).

go(Pid) ->
    ?breadcrumb(Pid),
    ?breadcrumb(Pid),

    do_stuff(Pid),

    ?breadcrumb(Pid),

    Pid ! {done, self()},
    ok.

do_stuff(Pid) ->
    ?breadcrumb(Pid),
    ?breadcrumb(Pid),
    ok.

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
