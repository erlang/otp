%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2013. All Rights Reserved.
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

-module(ssl_sup).

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
    SessionCertManager = session_and_cert_manager_child_spec(),
    TLSConnetionManager = tls_connection_manager_child_spec(),
    %% Not supported yet
    %%DTLSConnetionManager = tls_connection_manager_child_spec(),

    {ok, {{one_for_all, 10, 3600}, [SessionCertManager, TLSConnetionManager]}}.


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

session_and_cert_manager_child_spec() ->
    Opts = manager_opts(),
    Name = ssl_manager,  
    StartFunc = {ssl_manager, start_link, [Opts]},
    Restart = permanent, 
    Shutdown = 4000,
    Modules = [ssl_manager],
    Type = worker,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

tls_connection_manager_child_spec() ->
    Name = tls_connection,  
    StartFunc = {tls_connection_sup, start_link, []},
    Restart = permanent, 
    Shutdown = 4000,
    Modules = [tls_connection, ssl_connection],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

dtls_connection_manager_child_spec() ->
    Name = dtls_connection,  
    StartFunc = {dtls_connection_sup, start_link, []},
    Restart = permanent, 
    Shutdown = 4000,
    Modules = [dtls_connection, ssl_connection],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

session_cb_init_args() ->
    case application:get_env(ssl, session_cb_init_args) of
	{ok, Args} when is_list(Args) ->
	    Args;
	_  ->
	    []
    end.
