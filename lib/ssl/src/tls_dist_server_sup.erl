%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2021-2024. All Rights Reserved.
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

-module(tls_dist_server_sup).
-moduledoc false.

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
			
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================

init([]) ->  
    SupFlags = #{strategy  => one_for_all,
                 intensity =>   10,
                 period    => 3600
                },
    ChildSpecs = [listen_options_tracker_child_spec(), 
                  tls_server_session_child_spec(),
                  ssl_server_session_child_spec()],
    {ok, {SupFlags, ChildSpecs}}.
 

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% Handles emulated options so that they inherited by the accept
%% socket, even when setopts is performed on the listen socket
listen_options_tracker_child_spec() ->
    #{id       => dist_ssl_listen_tracker_sup,
      start    => {ssl_listen_tracker_sup, start_link_dist, []},
      restart  => permanent, 
      shutdown => 4000,
      modules  => [ssl_listen_tracker_sup],
      type     => supervisor
     }.

tls_server_session_child_spec() ->
    #{id       => dist_tls_server_session_ticket,
      start    => {tls_server_session_ticket_sup, start_link_dist, []},
      restart  => permanent, 
      shutdown => 4000,
      modules  => [tls_server_session_ticket_sup],
      type     => supervisor
     }.

ssl_server_session_child_spec() ->
    #{id       => dist_ssl_upgrade_server_session_cache_sup,
      start    => {ssl_upgrade_server_session_cache_sup, start_link_dist, []},
      restart  => permanent, 
      shutdown => 4000,
      modules  => [ssl_upgrade_server_session_cache_sup],
      type     => supervisor
     }.
