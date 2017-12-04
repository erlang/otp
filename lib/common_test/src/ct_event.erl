%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2017. All Rights Reserved.
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

%%% @doc Common Test Framework Event Handler
%%%
%%% <p>This module implements an event handler that CT uses to
%%% handle status and progress notifications during test runs.
%%% The notifications are handled locally (per node) and passed
%%% on to ct_master when CT runs in distributed mode. This
%%% module may be used as a template for other event handlers
%%% that can be plugged in to handle local logging and reporting.</p>
-module(ct_event).

-behaviour(gen_event).

%% API
-export([start_link/0, add_handler/0, add_handler/1, stop/0]).
-export([notify/1, notify/2, sync_notify/1,sync_notify/2]).
-export([is_alive/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-include("ct_event.hrl").
-include("ct_util.hrl").

%% receivers = [{RecvTag,Pid}]
-record(state, {receivers=[]}).


%%====================================================================
%% gen_event callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | {error,Error} 
%% Description: Creates an event manager.
%%--------------------------------------------------------------------
start_link() ->
    gen_event:start_link({local,?CT_EVMGR}). 

%%--------------------------------------------------------------------
%% Function: add_handler() -> ok | {'EXIT',Reason} | term()
%% Description: Adds an event handler
%%--------------------------------------------------------------------
add_handler() ->
    gen_event:add_handler(?CT_EVMGR_REF,?MODULE,[]).
add_handler(Args) ->
    gen_event:add_handler(?CT_EVMGR_REF,?MODULE,Args).

%%--------------------------------------------------------------------
%% Function: stop() -> ok
%% Description: Stops the event manager
%%--------------------------------------------------------------------
stop() ->
    case whereis(?CT_EVMGR) of
	undefined ->
	    ok;
	_Pid ->
	    gen_event:stop(?CT_EVMGR_REF)
    end.

%%--------------------------------------------------------------------
%% Function: notify(Event) -> ok
%% Description: Asynchronous notification to event manager.
%%--------------------------------------------------------------------
notify(Event) ->
    case catch gen_event:notify(?CT_EVMGR_REF,Event) of
	{'EXIT',Reason} ->
	    {error,{notify,Reason}};
	Result ->
	    Result
    end.

%%--------------------------------------------------------------------
%% Function: notify(Name,Data) -> ok
%% Description: Asynchronous notification to event manager.
%%--------------------------------------------------------------------
notify(Name, Data) ->
    notify(#event{ name = Name, data = Data}).

%%--------------------------------------------------------------------
%% Function: sync_notify(Event) -> ok
%% Description: Synchronous notification to event manager.
%%--------------------------------------------------------------------
sync_notify(Event) ->
    case catch gen_event:sync_notify(?CT_EVMGR_REF,Event) of
	{'EXIT',Reason} ->
	    {error,{sync_notify,Reason}};
	Result ->
	    Result
    end.

%%--------------------------------------------------------------------
%% Function: sync_notify(Name,Data) -> ok
%% Description: Synchronous notification to event manager.
%%--------------------------------------------------------------------
sync_notify(Name,Data) ->
    sync_notify(#event{ name = Name, data = Data}).

%%--------------------------------------------------------------------
%% Function: is_alive() -> true | false
%% Description: Check if Event Manager is alive.
%%--------------------------------------------------------------------
is_alive() ->
    case whereis(?CT_EVMGR) of
	undefined ->
	    false;
	_Pid ->
	    true
    end.    

%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%% Description: Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%--------------------------------------------------------------------
init(RecvPids) ->
    ct_util:mark_process(),
    %% RecvPids = [{RecvTag,Pid}]
    {ok,#state{receivers=RecvPids}}.

%%--------------------------------------------------------------------
%% Function:  
%% handle_event(Event, State) -> {ok, State} |
%%                               {swap_handler, Args1, State1, Mod2, Args2} |
%%                               remove_handler
%% Description:Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for
%% each installed event handler to handle the event. 
%%--------------------------------------------------------------------
handle_event(Event,State=#state{receivers=RecvPids}) ->
    print("~n=== ~w ===~n", [?MODULE]),
    print("~tw: ~tw~n", [Event#event.name,Event#event.data]),
    lists:foreach(fun(Recv) -> report_event(Recv,Event) end, RecvPids),
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

%% report to master
report_event({master,Master},E=#event{name=_Name,node=_Node,data=_Data}) ->
    ct_master:status(Master,E);

%% report to VTS
report_event({vts,VTS},#event{name=Name,node=_Node,data=Data}) ->
    if Name == start_info ;
       Name == test_stats ;
       Name == test_done ->
	    vts:test_info(VTS,Name,Data);
       true ->
	    ok
    end.


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
%% Function: terminate(Reason, State) -> ok
%% Description: Whenever an event handler is deleted from an event manager,
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

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

print(_Str,_Args) ->
%    io:format(_Str,_Args),
    ok.
