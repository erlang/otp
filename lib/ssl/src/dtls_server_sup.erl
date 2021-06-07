%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020-2020. All Rights Reserved.
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

-module(dtls_server_sup).

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
    DTLSListeners = dtls_listeners_spec(),
    %% Add SessionTracker if we add DTLS-1.3
    Pre_1_3SessionTracker = ssl_server_session_child_spec(),
    
    {ok, {{one_for_all, 10, 3600}, [DTLSListeners,
                                    Pre_1_3SessionTracker
				   ]}}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
dtls_listeners_spec() ->
    Name = dtls_listener,  
    StartFunc = {dtls_listener_sup, start_link, []},
    Restart = permanent, 
    Shutdown = 4000,
    Modules = [],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

ssl_server_session_child_spec() ->
    Name = dtls_server_session_cache_sup,  
    StartFunc = {dtls_server_session_cache_sup, start_link, []},
    Restart = permanent, 
    Shutdown = 4000,
    Modules = [dtls_server_session_cache_sup],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.
