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

-module(ssl_admin_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, manager_opts/0]).

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
    PEMCache = pem_cache_child_spec(),
    SessionCertManager = session_and_cert_manager_child_spec(),
    {ok, {{rest_for_one, 10, 3600}, [PEMCache, SessionCertManager]}}.

manager_opts() ->
    CbOpts = case application:get_env(ssl, session_cb) of
		 {ok, Cb} when is_atom(Cb) ->
		     InitArgs = session_cb_init_args(),
		     [{session_cb, Cb}, {session_cb_init_args, InitArgs}];
		 _  ->
		     []
	     end,
    case application:get_env(ssl, session_lifetime) of
	{ok, Time} when is_integer(Time) ->
	    [{session_lifetime, Time}| CbOpts];
	_  ->
	    CbOpts
    end.
    
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

pem_cache_child_spec() ->
    Name = ssl_pem_cache,  
    StartFunc = {ssl_pem_cache, start_link, [[]]},
    Restart = permanent, 
    Shutdown = 4000,
    Modules = [ssl_pem_cache],
    Type = worker,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

session_and_cert_manager_child_spec() ->
    Opts = manager_opts(),
    Name = ssl_manager,  
    StartFunc = {ssl_manager, start_link, [Opts]},
    Restart = permanent, 
    Shutdown = 4000,
    Modules = [ssl_manager],
    Type = worker,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

session_cb_init_args() ->
    case application:get_env(ssl, session_cb_init_args) of
	{ok, Args} when is_list(Args) ->
	    Args;
	_  ->
	    []
    end.
