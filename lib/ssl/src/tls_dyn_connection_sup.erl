%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%% 
%% Copyright Ericsson AB 2021-2025. All Rights Reserved.
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

-module(tls_dyn_connection_sup).
-moduledoc false.

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(SenderArgs, ReciverArgs) ->
    supervisor:start_link(?MODULE, [SenderArgs, ReciverArgs]).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([SenderArgs, ReciverArgs]) ->
    SupFlags = #{strategy      => one_for_all,
                 auto_shutdown => any_significant,
                 intensity     =>    0,
                 period        => 3600
                },
    ChildSpecs = [sender(SenderArgs), receiver(ReciverArgs)],
    {ok, {SupFlags, ChildSpecs}}.

sender(Args) ->
    #{id          => sender,
      restart     => temporary,
      type        => worker,
      start       => {tls_sender, start_link, Args},
      modules     => [tls_sender]
     }.

receiver(Args) ->
    #{id          => receiver,
      restart     => temporary,
      type        => worker,
      significant => true,
      start       => {ssl_gen_statem, tls_start_link, Args},
      modules     => [ssl_gen_statem,
                      tls_client_connection,
                      tls_server_connection,
                      tls_gen_connection,
                      tls_client_connection_1_3,
                      tls_server_connection_1_3,
                      tls_gen_connection_1_3]
     }.
