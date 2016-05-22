%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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
%% Purpose: The top supervisor for tftp hangs under inets_sup.
%%----------------------------------------------------------------------

-module(tftp_sup).

-behaviour(supervisor).

%% API
-export([start_link/1,
	 start_child/1,
	 stop_child/1,
	 which_children/0]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================

start_link(TftpServices) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [TftpServices]).

start_child(Options) ->
    KillAfter = default_kill_after(),
    ChildSpec = worker_spec(KillAfter, Options),
    supervisor:start_child(?MODULE, ChildSpec).

stop_child(Pid) when is_pid(Pid) ->
    Children = supervisor:which_children(?MODULE),
    case [Id || {Id, P, _Type, _Modules} <- Children, P =:= Pid] of
	[] ->
	    {error, not_found};
	[Id] ->
	    case supervisor:terminate_child(?MODULE, Id) of
		ok ->
		    supervisor:delete_child(?MODULE, Id);
		{error, not_found} ->
		    supervisor:delete_child(?MODULE, Id);
		{error, Reason} ->
		    {error, Reason}
	    end
    end.

which_children() ->
    Children = supervisor:which_children(?MODULE),
    [{tftpd, Pid} || {_Id, Pid, _Type, _Modules} <- Children, Pid =/= undefined].
    
%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================

init([Services]) when is_list(Services) ->
    RestartStrategy = one_for_one,
    MaxR = 10,
    MaxT = 3600,
    KillAfter = default_kill_after(),
    Children = [worker_spec(KillAfter, Options) || {tftpd, Options} <- Services],
    {ok, {{RestartStrategy, MaxR, MaxT}, Children}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================

worker_spec(KillAfter, Options) ->
    Modules = [proc_lib, tftp, tftp_engine],
    KA = supervisor_timeout(KillAfter),
    Name = unique_name(Options),
    {Name, {tftp, start, [Options]}, permanent, KA, worker, Modules}.

unique_name(Options) ->
    case lists:keysearch(port, 1, Options) of
	{value, {_, Port}} when is_integer(Port), Port > 0 -> 
	    {tftpd, Port};
	_ ->
	    {tftpd, erlang:unique_integer([positive])}
    end.

default_kill_after() ->
    timer:seconds(3).

%% supervisor_spec(Name) ->
%%     {Name, {Name, start, []}, permanent, infinity, supervisor,
%%      [Name, supervisor]}.
    
-ifdef(debug_shutdown).
supervisor_timeout(_KillAfter) -> timer:hours(24).
-else.
supervisor_timeout(KillAfter) -> KillAfter.
-endif.    
