%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
%%----------------------------------------------------------------------
%%% Purpose : Exported API to the Pman graphical tool
%%----------------------------------------------------------------------

-module(pman).


%% ---------------------------------------------------------------
%% The user interface exports 
%% ---------------------------------------------------------------
-export([start/0,
	 start_notimeout/0,
	 start/1,
	 start_notimeout/1,
	 proc/1,
	 proc/3]).

%% ---------------------------------------------------------------

%% Timeout for the startup function.
%% If no  {initialization_complete, Pid} message has been received
%% from the spawned init-function within ?STARTUP_TIMEOUT ms
%% the start-function will call exit(Reason).
-define(STARTUP_TIMEOUT, 20000).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% start/0

start() ->
    start([], ?STARTUP_TIMEOUT).		%Start w/o excluded modules

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% start_notimeout/0

start_notimeout() ->
    start([],infinity).				%Start w/o excluded modules

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% start/1

start(LIModuleExcluded) ->
    start(LIModuleExcluded, ?STARTUP_TIMEOUT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% start_notimeout/1

start_notimeout(LIModuleExcluded) ->
    start(LIModuleExcluded, infinity).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% start/2 - Spawns the main Pman process, that will supervise
%%    all processes except those running code from the modules
%%    specified in LIModuleExcluded
%%

start(LIModuleExcluded, Timeout) ->

    OSModuleExcluded = ordsets:from_list(LIModuleExcluded),

    PidInit = spawn(pman_main, init, [self(), OSModuleExcluded]),

    %% Wait for a initialization completion message from
    %% the spawned process before returning its Pid.
    %% 

    receive
	{initialization_complete, PidInit} ->
	    PidInit

    %% (Conditional) Failure to start within the time limit will
    %% result in termination

    after
	Timeout ->
	    exit(PidInit, kill),
	    exit({startup_timeout, ?MODULE})
    end.



%% ---------------------------------------------------------------
%% If we want to trace just one process, we can call proc, giving it
%% either the Pid, or the registered name, (Global or local).
%%
%% (???)
%% Note that this function must not be used internally to create a
%% trace window, since it is assumed that it is started from any
%% process (esp. the shell) it will not have any supervisor process
%% that shall be notified about it's exit/death.
%%
%% Returns: Trace loop Pid|udefined

%% ---------------------------------------------------------------


proc(undefined) ->
    exit(undefined);

proc({shell,P}) when is_pid(P) ->
    pman_shell:start({{shell,P},self()});

proc(P) when is_atom(P) ->
    proc(whereis(P));

proc({global, N}) ->
    proc(global:whereis_name(N));

proc(P) when is_pid(P) ->
    pman_shell:start({P,self()}).

proc(X,Y,Z) ->
    proc(c:pid(X,Y,Z)).

