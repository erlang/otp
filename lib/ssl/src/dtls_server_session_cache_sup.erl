%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2020-2025. All Rights Reserved.
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

%%
%%----------------------------------------------------------------------
%% Purpose: Supervisor for a listen options tracker
%%----------------------------------------------------------------------
-module(dtls_server_session_cache_sup).
-moduledoc false.

-behaviour(supervisor).

-include("ssl_internal.hrl").

%% API
-export([start_link/0]).
-export([start_child/1]).

%% Supervisor callback
-export([init/1]).

-define(DEFAULT_MAX_SESSION_CACHE, 1000).
%%%=========================================================================
%%%  API
%%%=========================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
start_child(Listener) ->
    supervisor:start_child(?MODULE, [Listener | [ssl_config:pre_1_3_session_opts(server)]]).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init(_) ->
    SupFlags = #{strategy  => simple_one_for_one, 
                 intensity =>   0,
                 period    => 3600
                },
    ChildSpecs = [#{id       => undefined,
                    start    => {ssl_server_session_cache, start_link, []},
                    restart  => temporary, 
                    shutdown => 4000,
                    modules  => [ssl_server_session_cache],
                    type     => worker
                   }], 
    {ok, {SupFlags, ChildSpecs}}.
