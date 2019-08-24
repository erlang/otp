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
%% Purpose: The top supervisor for the Megaco/H.248 application
%%----------------------------------------------------------------------

-module(megaco_misc_sup).

-behaviour(supervisor).

%% public
-export([start/0, start/2, stop/1, init/1]).
-export([start_permanent_worker/4]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% application and supervisor callback functions

start(normal, Args) ->
    SupName = {local,?MODULE},
    case supervisor:start_link(SupName, ?MODULE, [Args]) of
	{ok, Pid} ->
	    {ok, Pid, {normal, Args}};
	Error -> 
	    Error
    end;
start(_, _) ->
    {error, badarg}.

start() ->
    SupName = {local,?MODULE},
    supervisor:start_link(SupName, ?MODULE, []).

stop(_StartArgs) ->
    ok.

init([]) -> % Supervisor
    init();
init(BadArg) ->
    {error, {badarg, BadArg}}.

init() ->
    Flags     = {one_for_one, 0, 1},
    Workers   = [],
    {ok, {Flags, Workers}}.


%%----------------------------------------------------------------------
%% Function: start_permanent_worker/3
%% Description: Starts a permanent worker (child) process
%%----------------------------------------------------------------------

start_permanent_worker(M, F, A, Modules) ->
    Spec = {M, {M,F,A}, permanent, timer:seconds(1), worker, [M] ++ Modules},
    supervisor:start_child(?MODULE, Spec).

    


