%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: httpd_misc_sup.erl,v 1.1 2008/12/17 09:53:34 mikpe Exp $
%%
%%----------------------------------------------------------------------
%% Purpose: The top supervisor for the Megaco/H.248 application
%%----------------------------------------------------------------------

-module(httpd_misc_sup).

-behaviour(supervisor).

-include("httpd_verbosity.hrl").

%% public
-export([start/3, stop/1, init/1]).

-export([start_auth_server/3, stop_auth_server/2,
	 start_sec_server/3,  stop_sec_server/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% supervisor callback functions


start(Addr, Port, MiscSupVerbosity) ->
    SupName = make_name(Addr, Port),
    supervisor:start_link({local, SupName}, ?MODULE, [MiscSupVerbosity]).

stop(StartArgs) ->
    ok.

init([Verbosity]) -> % Supervisor
    do_init(Verbosity);
init(BadArg) ->
    {error, {badarg, BadArg}}.

do_init(Verbosity) ->
    put(verbosity,?vvalidate(Verbosity)),
    put(sname,misc_sup),
    ?vlog("starting", []),
    Flags     = {one_for_one, 0, 1},
    KillAfter = timer:seconds(1),
    Workers   = [],
    {ok, {Flags, Workers}}.


%%----------------------------------------------------------------------
%% Function: [start|stop]_[auth|sec]_server/3
%% Description: Starts a [auth | security] worker (child) process
%%----------------------------------------------------------------------

start_auth_server(Addr, Port, Verbosity) ->
    start_permanent_worker(mod_auth_server, Addr, Port,
			   Verbosity, [gen_server]).

stop_auth_server(Addr, Port) ->
    stop_permanent_worker(mod_auth_server, Addr, Port).


start_sec_server(Addr, Port, Verbosity) ->
    start_permanent_worker(mod_security_server, Addr, Port,
			   Verbosity, [gen_server]).

stop_sec_server(Addr, Port) ->
    stop_permanent_worker(mod_security_server, Addr, Port).



%%----------------------------------------------------------------------
%% Function:    start_permanent_worker/5
%% Description: Starts a permanent worker (child) process
%%----------------------------------------------------------------------

start_permanent_worker(Mod, Addr, Port, Verbosity, Modules) ->
    SupName = make_name(Addr, Port),
    Spec    = {{Mod, Addr, Port},
	       {Mod, start_link, [Addr, Port, Verbosity]},
	       permanent, timer:seconds(1), worker, [Mod] ++ Modules},
    supervisor:start_child(SupName, Spec).


%%----------------------------------------------------------------------
%% Function:    stop_permanent_worker/3
%% Description: Stops a permanent worker (child) process
%%----------------------------------------------------------------------

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
