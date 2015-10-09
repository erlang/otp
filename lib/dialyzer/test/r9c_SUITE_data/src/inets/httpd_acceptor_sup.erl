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
%%     $Id: httpd_acceptor_sup.erl,v 1.1 2008/12/17 09:53:33 mikpe Exp $
%%
%%----------------------------------------------------------------------
%% Purpose: The top supervisor for the Megaco/H.248 application
%%----------------------------------------------------------------------

-module(httpd_acceptor_sup).

-behaviour(supervisor).

-include("httpd_verbosity.hrl").

%% public
-export([start/3, stop/1, init/1]).

-export([start_acceptor/4, stop_acceptor/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% supervisor callback functions


start(Addr, Port, AccSupVerbosity) ->
    SupName = make_name(Addr, Port),
    supervisor:start_link({local, SupName}, ?MODULE, [AccSupVerbosity]).

stop(StartArgs) ->
    ok.

init([Verbosity]) -> % Supervisor
    do_init(Verbosity);
init(BadArg) ->
    {error, {badarg, BadArg}}.

do_init(Verbosity) ->
    put(verbosity,?vvalidate(Verbosity)),
    put(sname,acc_sup),
    ?vlog("starting", []),
    Flags     = {one_for_one, 500, 100},
    KillAfter = timer:seconds(1),
    Workers   = [],
    {ok, {Flags, Workers}}.


%%----------------------------------------------------------------------
%% Function: [start|stop]_acceptor/5
%% Description: Starts a [auth | security] worker (child) process
%%----------------------------------------------------------------------

start_acceptor(SocketType, Addr, Port, ConfigDb) ->
    Verbosity = get_acc_verbosity(),
    start_worker(httpd_acceptor, SocketType, Addr, Port,
		 ConfigDb, Verbosity, self(), []).

stop_acceptor(Addr, Port) ->
    stop_worker(httpd_acceptor, Addr, Port).


%%----------------------------------------------------------------------
%% Function:    start_worker/5
%% Description: Starts a (permanent) worker (child) process
%%----------------------------------------------------------------------

start_worker(M, SocketType, Addr, Port, ConfigDB, Verbosity, Manager,
	     Modules) ->
    SupName = make_name(Addr, Port),
    Args    = [Manager, SocketType, Addr, Port, ConfigDB, Verbosity],
    Spec    = {{M, Addr, Port},
	       {M, start_link, Args},
	       permanent, timer:seconds(1), worker, [M] ++ Modules},
    supervisor:start_child(SupName, Spec).


%%----------------------------------------------------------------------
%% Function:    stop_permanent_worker/3
%% Description: Stops a permanent worker (child) process
%%----------------------------------------------------------------------

stop_worker(M, Addr, Port) ->
    SupName = make_name(Addr, Port),
    Name    = {M, Addr, Port},
    case supervisor:terminate_child(SupName, Name) of
	ok ->
	    supervisor:delete_child(SupName, Name);
	Error ->
	    Error
    end.


make_name(Addr,Port) ->
    httpd_util:make_name("httpd_acc_sup",Addr,Port).



get_acc_verbosity() ->
    get_verbosity(get(acceptor_verbosity)).

get_verbosity(undefined) ->
    ?default_verbosity;
get_verbosity(V) ->
    ?vvalidate(V).
