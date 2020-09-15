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
%%----------------------------------------------------------------------
%% Purpose: Supervisor for a listen options tracker
%%----------------------------------------------------------------------
-module(ssl_server_session_cache_sup).

-behaviour(supervisor).

-include("ssl_internal.hrl").

%% API
-export([start_link/0,
         start_link_dist/0]).
-export([start_child/1,
         start_child_dist/1,
         session_opts/0]).

%% Supervisor callback
-export([init/1]).

-define(DEFAULT_MAX_SESSION_CACHE, 1000).
%%%=========================================================================
%%%  API
%%%=========================================================================
start_link() ->
    supervisor:start_link({local, tracker_name(normal)}, ?MODULE, []).

start_link_dist() ->
    supervisor:start_link({local, tracker_name(dist)}, ?MODULE, []).

start_child(Args) ->
    supervisor:start_child(tracker_name(normal), [self() | Args]).

start_child_dist(Args) ->
    supervisor:start_child(tracker_name(dist), [self() | Args]).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init(_O) ->
    RestartStrategy = simple_one_for_one,
    MaxR = 3,
    MaxT = 3600,

    Name = undefined, % As simple_one_for_one is used.
    StartFunc = {ssl_server_session_cache, start_link, []},
    Restart = transient, % Should be restarted only on abnormal termination
    Shutdown = 4000,
    Modules = [ssl_server_session_cache],
    Type = worker,

    ChildSpec = {Name, StartFunc, Restart, Shutdown, Type, Modules},
    {ok, {{RestartStrategy, MaxR, MaxT}, [ChildSpec]}}.

tracker_name(normal) ->
    ?MODULE;
tracker_name(dist) ->
    list_to_atom(atom_to_list(?MODULE) ++ "dist").

session_opts() ->
    CbOpts = case application:get_env(ssl, session_cb) of
		 {ok, Cb} when is_atom(Cb) ->
		     InitArgs = session_cb_init_args(),
		     #{session_cb => Cb,
                       session_cb_init_args => InitArgs};
		 _  ->
		     #{session_cb => ssl_server_session_cache_db,
                       session_cb_init_args => []}
	     end,
    LifeTime = session_lifetime(),
    Max = max_session_cache_size(),
    [CbOpts#{lifetime => LifeTime, max => Max}].

session_cb_init_args() ->
    case application:get_env(ssl, session_cb_init_args) of
	{ok, Args} when is_list(Args) ->
	    Args;
	_  ->
	    []
    end.

session_lifetime() ->
    case application:get_env(ssl, session_lifetime) of
	{ok, Time} when is_integer(Time) ->
            Time;
        _  ->
            ?'24H_in_sec'
    end.

max_session_cache_size() ->
    case application:get_env(ssl, session_cache_server_max) of
	{ok, Size} when is_integer(Size) ->
	    Size;
	_ ->
	   ?DEFAULT_MAX_SESSION_CACHE
    end.
