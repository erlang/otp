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
%% Purpose: Supervisor for the tcp "accept" processes.
%%-----------------------------------------------------------------
-module(megaco_tcp_accept_sup).

-behaviour(supervisor).


%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([
	 start_link/0,
	 start_child/4
	]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([
	 init/1, 
	 terminate/2
	]).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: start_link/1
%% Description: Starts the process that keeps track of the TCP 
%%              accept processes
%%-----------------------------------------------------------------
start_link() ->
    supervisor:start_link(?MODULE, []).


%%-----------------------------------------------------------------
%% Func: start_link/1
%% Description: Starts the acceptor process (the TCP accept 
%%              processes)
%%-----------------------------------------------------------------
start_child(Pid, Rec, Ref, Listen) ->
    ChildSpec = [{Rec, Ref, Listen}],  % Simple one-for-one
    supervisor:start_child(Pid, ChildSpec).


%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: init/1
%% Description: Init funcion for the supervisor
%%-----------------------------------------------------------------
init(_Options) ->
    SupFlags = {simple_one_for_one, 500, 100},
    ChildSpec = [
		 {megaco_accept,
		  {megaco_tcp_accept, start_link, []},
		  temporary, 10000, worker, []}
		],
    {ok, {SupFlags, ChildSpec}}.


%%----------------------------------------------------------------- 
%% Func: terminate/2
%% Description: Termination function for the supervisor
%%-----------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

