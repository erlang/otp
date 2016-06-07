%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(supervisor_bridge).

-behaviour(gen_server).

%% External exports
-export([start_link/2, start_link/3]).
%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([code_change/3]).

-callback init(Args :: term()) ->
    {ok, Pid :: pid(), State :: term()} | ignore | {error, Error :: term()}.
-callback terminate(Reason :: (shutdown | term()), State :: term()) ->
    Ignored :: term().

%%%-----------------------------------------------------------------
%%% This is a rewrite of supervisor_bridge from BS.3.
%%%
%%% This module is built to function as process code
%%% for a process sitting inbetween a real supervisor
%%% and a not start&recovery complient server/system
%%% The process inbetween simulates start&recovery
%%% behaviour of the server/system below.
%%%
%%% The supervisor_bridge behaviour must export the following
%%% functions:
%%%    init(Args) -> {ok, Pid, State} | {error, Reason} | ignore
%%%       where Pid is the child process
%%%    terminate(Reason, State) -> ok
%%%-----------------------------------------------------------------
-record(state, {mod, pid, child_state, name}).

-spec start_link(Module, Args) -> Result when
      Module :: module(),
      Args :: term(),
      Result :: {ok, Pid} | ignore | {error, Error},
      Error :: {already_started, Pid} | term(),
      Pid :: pid().

start_link(Mod, StartArgs) ->
    gen_server:start_link(supervisor_bridge, [Mod, StartArgs, self], []).

-spec start_link(SupBridgeName, Module, Args) -> Result when
      SupBridgeName :: {local, Name} | {global, Name},
      Name :: atom(),
      Module :: module(),
      Args :: term(),
      Result :: {ok, Pid} | ignore | {error, Error},
      Error :: {already_started, Pid} | term(),
      Pid :: pid().

start_link(Name, Mod, StartArgs) ->
    gen_server:start_link(Name, supervisor_bridge, [Mod, StartArgs, Name], []).

%%-----------------------------------------------------------------
%% Callback functions from gen_server
%%-----------------------------------------------------------------
init([Mod, StartArgs, Name0]) ->  
    process_flag(trap_exit, true),
    Name = supname(Name0, Mod),
    case Mod:init(StartArgs) of
	{ok, Pid, ChildState} when is_pid(Pid) ->
	    link(Pid),
	    report_progress(Pid, Mod, StartArgs, Name),
	    {ok, #state{mod = Mod, pid = Pid,
			child_state = ChildState, name = Name}};
	ignore ->
	    ignore;
	{error, Reason} ->
	    {stop, Reason}
    end.

supname(self, Mod) -> {self(),Mod};
supname(N, _)      -> N.

%% A supervisor *must* answer the supervisor:which_children call.
handle_call(which_children, _From, State) ->
    {reply, [], State};
handle_call(_Req, _From, State) ->
    {reply, {error, badcall}, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, State) when State#state.pid =:= Pid ->
	case Reason of
	normal ->
	    ok;
	shutdown ->
	    ok;
	{shutdown, _Term} ->
	    ok;
	_ ->
	    report_error(child_terminated, Reason, State)
	end,
    {stop, Reason, State#state{pid = undefined}};
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, #state{pid = undefined}) ->
    ok;
terminate(Reason, State) ->
    terminate_pid(Reason, State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% This function is supposed to terminate the 'real' server.
terminate_pid(Reason, #state{mod = Mod, child_state = ChildState}) ->
    Mod:terminate(Reason, ChildState).

report_progress(Pid, Mod, StartArgs, SupName) ->
    Progress = [{supervisor, SupName},
		{started, [{pid, Pid}, {mfa, {Mod, init, [StartArgs]}}]}],
    error_logger:info_report(progress, Progress).

report_error(Error, Reason, #state{name = Name, pid = Pid, mod = Mod}) ->
    ErrorMsg = [{supervisor, Name},
		{errorContext, Error},
		{reason, Reason},
		{offender, [{pid, Pid}, {mod, Mod}]}],
    error_logger:error_report(supervisor_report, ErrorMsg).
