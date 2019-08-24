%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016. All Rights Reserved.
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

%%% This tests that the correct XML is produced when pre_init_per_suite
%%% fails in a hook
-module(fail_pre_init_per_suite).

%% CT Hooks
-export([init/2, pre_init_per_suite/3]).

-type config() :: proplists:proplist().
-type reason() :: term().
-type skip_or_fail() :: skip | auto_skip | fail | 'EXIT'.

-record(state, {}).

-spec init(Id :: term(), Opts :: proplists:proplist()) ->
    {ok, proplists:proplist()}.
init(_Id, Opts) ->
    {ok, Opts}.

-spec pre_init_per_suite(Suite :: atom(),
                         Config :: config(),
                         State :: #state{}) ->
    {config() | {skip_or_fail(), reason()}, NewState :: #state{}}.
pre_init_per_suite(fail_SUITE, _Config, State) ->
    {{fail, pre_init_per_suite}, State};
pre_init_per_suite(_Suite, Config, State) ->
    {Config, State}.

