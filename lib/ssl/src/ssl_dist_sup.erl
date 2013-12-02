%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2013. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%

-module(ssl_dist_sup).

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
    SessionCertManager = session_and_cert_manager_child_spec(),
    ConnetionManager = connection_manager_child_spec(),
    ProxyServer = proxy_server_child_spec(),

    {ok, {{one_for_all, 10, 3600}, [SessionCertManager, ConnetionManager,
				    ProxyServer]}}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
session_and_cert_manager_child_spec() ->
    Opts = ssl_sup:manager_opts(),
    Name = ssl_manager_dist,  
    StartFunc = {ssl_manager, start_link_dist, [Opts]},
    Restart = permanent, 
    Shutdown = 4000,
    Modules = [ssl_manager],
    Type = worker,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

connection_manager_child_spec() ->
    Name = ssl_connection_dist,  
    StartFunc = {tls_connection_sup, start_link_dist, []},
    Restart = permanent, 
    Shutdown = 4000,
    Modules = [ssl_connection],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

proxy_server_child_spec() ->
    Name = ssl_tls_dist_proxy,  
    StartFunc = {ssl_tls_dist_proxy, start_link, []},
    Restart = permanent, 
    Shutdown = 4000,
    Modules = [ssl_tls_dist_proxy],
    Type = worker,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

