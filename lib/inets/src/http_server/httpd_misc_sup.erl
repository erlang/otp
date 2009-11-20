%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2009. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: The supervisor for auth and sec processes in the http server, 
%%          hangs under the httpd_instance_sup_<Addr>_<Port> supervisor.
%%----------------------------------------------------------------------

-module(httpd_misc_sup).

-behaviour(supervisor).

%% API 
-export([start_link/2, start_auth_server/2, stop_auth_server/2, 
	 start_sec_server/2,  stop_sec_server/2]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================

start_link(Addr, Port) ->
    SupName = make_name(Addr, Port),
    supervisor:start_link({local, SupName}, ?MODULE, []).

%%----------------------------------------------------------------------
%% Function: [start|stop]_[auth|sec]_server/3
%% Description: Starts a [auth | security] worker (child) process
%%----------------------------------------------------------------------
start_auth_server(Addr, Port) ->
    start_permanent_worker(mod_auth_server, Addr, Port, [gen_server]).

stop_auth_server(Addr, Port) ->
    stop_permanent_worker(mod_auth_server, Addr, Port).


start_sec_server(Addr, Port) ->
    start_permanent_worker(mod_security_server, Addr, Port, [gen_server]).

stop_sec_server(Addr, Port) ->
    stop_permanent_worker(mod_security_server, Addr, Port).


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
start_permanent_worker(Mod, Addr, Port, Modules) ->
    SupName = make_name(Addr, Port),
    Spec    = {{Mod, Addr, Port},
	       {Mod, start_link, [Addr, Port]}, 
	       permanent, timer:seconds(1), worker, [Mod] ++ Modules},
    supervisor:start_child(SupName, Spec).

stop_permanent_worker(Mod, Addr, Port) ->
    SupName = make_name(Addr, Port),
    Name    = {Mod, Addr, Port},
    case supervisor:terminate_child(SupName, Name) of
	ok ->
	    supervisor:delete_child(SupName, Name);
	Error ->
	    Error
    end.
    
make_name(Addr,Port) ->
    httpd_util:make_name("httpd_misc_sup",Addr,Port).
