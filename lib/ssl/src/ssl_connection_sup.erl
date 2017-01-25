%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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

-module(ssl_connection_sup).

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
  
    TLSConnetionManager = tls_connection_manager_child_spec(),
    %% Handles emulated options so that they inherited by the accept
    %% socket, even when setopts is performed on the listen socket
    ListenOptionsTracker = listen_options_tracker_child_spec(), 
    
    DTLSConnetionManager = dtls_connection_manager_child_spec(),
    DTLSUdpListeners = dtls_udp_listeners_spec(),

    {ok, {{one_for_one, 10, 3600}, [TLSConnetionManager, 
				    ListenOptionsTracker,
				    DTLSConnetionManager, 
				    DTLSUdpListeners
				   ]}}.

    
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

tls_connection_manager_child_spec() ->
    Name = tls_connection,  
    StartFunc = {tls_connection_sup, start_link, []},
    Restart = permanent, 
    Shutdown = 4000,
    Modules = [tls_connection_sup],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

dtls_connection_manager_child_spec() ->
    Name = dtls_connection,
    StartFunc = {dtls_connection_sup, start_link, []},
    Restart = permanent,
    Shutdown = 4000,
    Modules = [dtls_connection_sup],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

listen_options_tracker_child_spec() ->
    Name = tls_socket,  
    StartFunc = {ssl_listen_tracker_sup, start_link, []},
    Restart = permanent, 
    Shutdown = 4000,
    Modules = [tls_socket],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

dtls_udp_listeners_spec() ->
    Name = dtls_udp_listener,  
    StartFunc = {dtls_udp_sup, start_link, []},
    Restart = permanent, 
    Shutdown = 4000,
    Modules = [],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.
