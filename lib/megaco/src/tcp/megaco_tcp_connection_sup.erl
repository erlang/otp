%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
%%-----------------------------------------------------------------
%% Purpose: Supervisor for all active connections
%%          independent of transport protocol.
%%-----------------------------------------------------------------

-module(megaco_tcp_connection_sup).

-behaviour(supervisor).

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include_lib("megaco/include/megaco.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([
	 start_link/0,
	 start_child/2
	]).


%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([
	 init/1, 
	 terminate/2, 
	 start_connection/1
	]).


%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: start_link
%% Description: Starts the connection supervisor
%%-----------------------------------------------------------------
start_link() ->
    supervisor:start_link(?MODULE, [[]]).


%% -----------------------------------------------------------------
%% Func: start_child/2
%% DEscription: Start a child (the connection) process 
%% -----------------------------------------------------------------
start_child(Pid, TcpRec) ->
    supervisor:start_child(Pid, [TcpRec]).


%%-----------------------------------------------------------------
%% Internal interface functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: start_connection/1
%% Description: Function which the supervisor calls to start a child
%%-----------------------------------------------------------------
start_connection(Args) ->
    megaco_tcp_connection:start_link(Args).


%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: init/1
%% Description: Init funcion for the supervisor
%%-----------------------------------------------------------------
init(_) ->
    SupFlags = {simple_one_for_one, 500, 100},
    ChildSpec = [
		 {megaco_tcp_connection,
		  {?MODULE, start_connection, []},
		  temporary, 
		  10000, 
		  worker,
		  []}
		],
    {ok, {SupFlags, ChildSpec}}.


%%-----------------------------------------------------------------
%% Func: terminate/1
%% Description: Termination function for the supervisor
%%-----------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
