%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2021-2023. All Rights Reserved.
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

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_child/3]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link() ->
    supervisor:start_link(?MODULE, []).

start_child(Sup, sender, Args) ->
    supervisor:start_child(Sup, sender(Args));
start_child(Sup, receiver, Args) ->
    supervisor:start_child(Sup, receiver(Args)).
    
%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init(_) ->
    SupFlags = #{strategy      => one_for_all,
                 auto_shutdown => any_significant,
                 intensity     =>    0,
                 period        => 3600
                },
    ChildSpecs = [],
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
      start       => {ssl_gen_statem, start_link, Args},
      modules     => [ssl_gen_statem,
                      tls_connection,
                      tls_gen_connection,
                      tls_client_connection_1_3,
                      tls_server_connection_1_3,
                      tls_gen_connection_1_3]
     }.
