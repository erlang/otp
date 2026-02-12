%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%% 
%% Copyright Ericsson AB 2026-2026. All Rights Reserved.
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
%% Purpose: Supervises the TLS generic state machine, a process that
%% owns the transport socket and hence is a significant child, and the
%% corresponding TLS sender process that sends data to avoid blocking
%% the state machine process.
%% ----------------------------------------------------------------------

-module(dyn_sup).
-moduledoc false.

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(Args1, Args2) ->
    supervisor:start_link(?MODULE, [Args1, Args2]).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([Args1, Args2]) ->
    SupFlags = #{strategy      => one_for_all,
                 auto_shutdown => any_significant,
                 intensity     =>    0,
                 period        => 3600
                },
    ChildSpecs = [foo(Args1), bar(Args2)],
    {ok, {SupFlags, ChildSpecs}}.

foo(Args) ->
    #{id          => foo,
      restart     => temporary,
      type        => worker,
      start       => {supervisor_1, start_child, Args},
      modules     => [supervisor_1]
     }.

bar(Args) ->
    #{id          => bar,
      restart     => temporary,
      type        => worker,
      significant => true,
      start       => {supervisor_1, start_child, Args},
      modules     => [supervisor_1]
     }.
