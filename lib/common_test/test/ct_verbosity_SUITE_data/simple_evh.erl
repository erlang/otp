%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2018. All Rights Reserved.
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

%%% Common Test Framework Event Handler
%%%
%%% This module implements an event handler that CT uses to
%%% handle status and progress notifications during test runs.
%%% The notifications are handled locally (per node) and passed
%%% on to ct_master when CT runs in distributed mode. This
%%% module may be used as a template for other event handlers
%%% that can be plugged in to handle local logging and reporting.
-module(simple_evh).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-include_lib("common_test/include/ct_event.hrl").
-include_lib("common_test/src/ct_util.hrl").

%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%% Description: Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%--------------------------------------------------------------------
init(_) ->
    io:format("Event handler ~w started!~n", [?MODULE]),
    {ok,[]}.

%%--------------------------------------------------------------------
%% Function:  
%% handle_event(Event, State) -> {ok, State} |
%%                               {swap_handler, Args1, State1, Mod2, Args2} |
%%                               remove_handler
%% Description:Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for
%% each installed event handler to handle the event. 
%%--------------------------------------------------------------------
handle_event(Event = #event{name = test_stats},State) ->
    %% this could cause a deadlock
    ct:pal("~p: ~p~n", [Event#event.name,Event#event.data]),
    {ok,State};
handle_event(_Event,State) ->
    {ok,State}.

%%============================== EVENTS ==============================
%%
%% Name = test_start
%% Data = {StartTime,LogDir}
%%
%% Name = start_info
%% Data = {Tests,Suites,Cases}
%% Tests = Suites = Cases = integer()
%%
%% Name = test_done
%% Data = EndTime
%%
%% Name = start_make
%% Data = Dir
%%
%% Name = finished_make
%% Data = Dir
%%
%% Name = tc_start
%% Data = {Suite,CaseOrGroup}
%% CaseOrGroup = atom() | {Conf,GroupName,GroupProperties}
%% Conf = init_per_group | end_per_group
%% GroupName = atom()
%% GroupProperties = list()
%%
%% Name = tc_done
%% Data = {Suite,CaseOrGroup,Result}
%% CaseOrGroup = atom() | {Conf,GroupName,GroupProperties}
%% Conf = init_per_group | end_per_group
%% GroupName = atom()
%% GroupProperties = list()
%% Result = ok | {skipped,Reason} | {failed,Reason} 
%%
%% Name = tc_user_skip
%% Data = {Suite,Case,Comment}
%% Comment = string()
%%
%% Name = tc_auto_skip
%% Data = {Suite,Case,Comment}
%% Comment = string()
%%
%% Name = test_stats
%% Data = {Ok,Failed,Skipped}
%% Ok = Failed = integer()
%% Skipped = {UserSkipped,AutoSkipped}
%% UserSkipped = AutoSkipped = integer()
%%
%% Name = start_logging
%% Data = CtRunDir
%%
%% Name = stop_logging
%% Data = []
%%
%% Name = start_write_file
%% Data = FullNameFile
%%
%% Name = finished_write_file
%% Data = FullNameFile
%%
%% Name = 
%% Data = 
%%

%%--------------------------------------------------------------------
%% Function: 
%% handle_call(Request, State) -> {ok, Reply, State} |
%%                                {swap_handler, Reply, Args1, State1, 
%%                                  Mod2, Args2} |
%%                                {remove_handler, Reply}
%% Description: Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified event 
%% handler to handle the request.
%%--------------------------------------------------------------------
handle_call(_Req, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info, State) -> {ok, State} |
%%                             {swap_handler, Args1, State1, Mod2, Args2} |
%%                              remove_handler
%% Description: This function is called for each installed event handler when
%% an event manager receives any other message than an event or a synchronous
%% request (or a system message).
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description:Whenever an event handler is deleted from an event manager,
%% this function is called. It should be the opposite of Module:init/1 and 
%% do any necessary cleaning up. 
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra) -> {ok, NewState} 
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

