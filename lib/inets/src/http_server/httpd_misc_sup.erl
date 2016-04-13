%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%% Purpose: The supervisor for auth and sec processes in the http server, 
%%          hangs under the httpd_instance_sup_<Addr>_<Port> supervisor.
%%----------------------------------------------------------------------

-module(httpd_misc_sup).

-behaviour(supervisor).

%% API 
-export([start_link/3, start_auth_server/3, stop_auth_server/3, 
	 start_sec_server/3,  stop_sec_server/3]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================

start_link(Addr, Port, Profile) ->
    SupName = make_name(Addr, Port, Profile),
    supervisor:start_link({local, SupName}, ?MODULE, []).

%%----------------------------------------------------------------------
%% Function: [start|stop]_[auth|sec]_server/3
%% Description: Starts a [auth | security] worker (child) process
%%----------------------------------------------------------------------
start_auth_server(Addr, Port, Profile) ->
    start_permanent_worker(mod_auth_server, Addr, Port, Profile, [gen_server]).

stop_auth_server(Addr, Port, Profile) ->
    stop_permanent_worker(mod_auth_server, Addr, Port, Profile).


start_sec_server(Addr, Port, Profile) ->
    start_permanent_worker(mod_security_server, Addr, Port, Profile, [gen_server]).

stop_sec_server(Addr, Port, Profile) ->
    stop_permanent_worker(mod_security_server, Addr, Port, Profile).


%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init(_) -> 
    Flags     = {one_for_one, 0, 1},
    Workers   = [],
    {ok, {Flags, Workers}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
start_permanent_worker(Mod, Addr, Port, Profile, Modules) ->
    SupName = make_name(Addr, Port, Profile),
    Spec    = {{Mod, Addr, Port},
	       {Mod, start_link, [Addr, Port, Profile]}, 
	       permanent, timer:seconds(1), worker, [Mod] ++ Modules},
    supervisor:start_child(SupName, Spec).

stop_permanent_worker(Mod, Addr, Port, Profile) ->
    SupName = make_name(Addr, Port, Profile),
    Name    = {Mod, Addr, Port},
    case supervisor:terminate_child(SupName, Name) of
	ok ->
	    supervisor:delete_child(SupName, Name);
	Error ->
	    Error
    end.
    
make_name(Addr,Port, Profile) ->
    httpd_util:make_name("httpd_misc_sup",Addr,Port, Profile).
