%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
%% Purpose: Supervisor
%%-----------------------------------------------------------------
-module(megaco_tcp_sup).

-behaviour(supervisor).

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([
	 start_link/0, 
	 which_accept_sup/1,
	 which_connection_sup/1,
	 start_accept_child/2
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
%% Description: Start an megaco net element supervisor
%%-----------------------------------------------------------------
start_link() ->
    supervisor:start_link(?MODULE, []).


%%-----------------------------------------------------------------
%% Func: which_accept_sup/1
%% Description: Get the pid() of the accept supervisor process
%%-----------------------------------------------------------------
which_accept_sup(Pid) ->
    which_child(Pid, megaco_tcp_accept_sup).


%%-----------------------------------------------------------------
%% Func: which_connection_sup/1
%% Description: Get the pid() of the connection supervisor process
%%-----------------------------------------------------------------
which_connection_sup(Pid) ->
    which_child(Pid, megaco_tcp_connection_sup).


%%-----------------------------------------------------------------
%% Func: start_accept_child/1
%% Description: Starts the process that keeps track of the TCP 
%%              accept processes
%%-----------------------------------------------------------------
start_accept_child(SupPid, Data) ->
    case supervisor:start_child(SupPid, 
				{megaco_tcp_accept, 
				 {megaco_tcp_accept, start_link, [Data]},  
				 temporary, 10000, worker, 
				 [megaco_tcp_accept]}) of
	{ok, ChildPid} ->
	    ChildPid;
	{error, Reason} ->
	    {error, Reason}
    end.


%%-----------------------------------------------------------------
%% Internal interface functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: init/1
%% Description: Init funcion for the supervisor
%%-----------------------------------------------------------------
init([]) ->
    SupFlags = {one_for_one, 5, 1000},	% Max 5 restarts in 1 second
    ChildSpec = [sup_spec(megaco_tcp_accept_sup, []),
		 sup_spec(megaco_tcp_connection_sup,[]),
		 worker_spec(megaco_tcp, [{self(), []}], [gen_server])],
    {ok, {SupFlags, ChildSpec}}.


%%-----------------------------------------------------------------
%% Func: terminate/2
%% Description: Termination function for the supervisor
%%-----------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.


%%-----------------------------------------------------------------
%% Local functions
%%-----------------------------------------------------------------

which_child(Pid, Name) ->
    ProcList = supervisor:which_children(Pid),
    %% ProcList of type [{Name, Pid, Type, Modules}]
    case lists:keysearch(Name, 1, ProcList) of
	{value, {_Name, ChildPid, _Type, _Modules}} ->
	    {ok, ChildPid};
	false ->
	    {error, no_such_process}
    end.    


sup_spec(Name, Args) ->
    {Name, 
     {Name, start_link, Args}, 
     permanent, 10000, supervisor, [Name, supervisor]}.

worker_spec(Name, Args, Mods) ->
    {Name, {Name, start_link, Args}, 
     permanent, 10000, worker, [Name] ++ Mods}.

