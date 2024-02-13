%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2024. All Rights Reserved.
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
-moduledoc false.

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
    SupFlags = #{strategy  => rest_for_one, 
                 intensity =>   10,
                 period    => 3600
                },
    ChildSpecs = [pem_cache_child_spec(), 
                  session_and_cert_manager_child_spec(),
                  ticket_store_spec()],    
    {ok, {SupFlags, ChildSpecs}}.

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
    #{id       => ssl_pem_cache,
      start    => {ssl_pem_cache, start_link, [[]]},
      restart  => permanent, 
      shutdown => 4000,
      modules  => [ssl_pem_cache],
      type     => worker
     }.
session_and_cert_manager_child_spec() ->
    Opts = manager_opts(),
    #{id       => ssl_manager,
      start    => {ssl_manager, start_link, [Opts]},
      restart  => permanent, 
      shutdown => 4000,
      modules  => [ssl_manager],
      type     => worker
     }.

ticket_store_spec() ->
    Size = client_session_ticket_store_size(),
    Lifetime = client_session_ticket_lifetime(),
    #{id       => tls_client_ticket_store,
      start    => {tls_client_ticket_store, start_link, [Size, Lifetime]},
      restart  => permanent,
      shutdown => 4000,
      modules  => [tls_client_ticket_store],
      type     => worker
     }.

session_cb_init_args() ->
    case application:get_env(ssl, session_cb_init_args) of
	{ok, Args} when is_list(Args) ->
	    Args;
	_  ->
	    []
    end.

client_session_ticket_store_size() ->
    case application:get_env(ssl, client_session_ticket_store_size) of
	{ok, Size} when is_integer(Size) andalso
                        Size > 0 ->
	    Size;
	_  ->
	    1000
    end.

client_session_ticket_lifetime() ->
    case application:get_env(ssl, client_session_ticket_lifetime) of
	{ok, Size} when is_integer(Size) andalso
                        Size > 0 ->
	    Size;
	_  ->
	    7200
    end.
