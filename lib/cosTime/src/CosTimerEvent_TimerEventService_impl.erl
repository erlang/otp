%%--------------------------------------------------------------------
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
%%----------------------------------------------------------------------
%% File    : CosTimerEvent_TimerEventService_impl.erl
%% Purpose : 
%%----------------------------------------------------------------------

-module('CosTimerEvent_TimerEventService_impl').

%%--------------- INCLUDES -----------------------------------
-include("cosTimeApp.hrl").


%%--------------- EXPORTS ------------------------------------
%%--------------- External -----------------------------------
%% Interface functions
-export([register/4, unregister/3, event_time/3]).

%%--------------- gen_server specific exports ----------------
-export([handle_info/2, code_change/3]).
-export([init/1, terminate/2]).


%% Data structures
-record(state, {timer}).
%% Data structures constructors
-define(get_InitState(T), 
	#state{timer=T}).

%% Data structures selectors
-define(get_TimerObj(S),    S#state.timer).

%% Data structures modifiers

%% MISC

%%-----------------------------------------------------------%
%% function : handle_info, code_change
%% Arguments: 
%% Returns  : 
%% Effect   : Functions demanded by the gen_server module. 
%%------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    ?debug_print("INFO: ~p~n", [_Info]),
    {noreply, State}.

%%----------------------------------------------------------%
%% function : init, terminate
%% Arguments: 
%%-----------------------------------------------------------

init([Timer]) ->
    process_flag(trap_exit, true),
    timer:start(),
    {ok, ?get_InitState(Timer)}.

terminate(_Reason, _State) ->
    ok.

%%-----------------------------------------------------------
%%------- Exported external functions -----------------------
%%-----------------------------------------------------------
%%----------------------------------------------------------%
%% function : register
%% Arguments: EventInterface - CosEventComm::PushConsumer
%%            Data - #any
%% Returns  : TimerEventHandler - objref#
%%-----------------------------------------------------------
register(OE_THIS, State, EventInterface, Data) ->
    {reply, 
     cosTime:start_event_handler([OE_THIS, self(),EventInterface, Data, 
				  ?get_TimerObj(State)]), 
     State}.

%%----------------------------------------------------------%
%% function : unregister
%% Arguments: TimerEventHandler - objref#
%% Returns  : ok
%%-----------------------------------------------------------
unregister(_OE_THIS, State, TimerEventHandler) ->
    catch corba:dispose(TimerEventHandler),
    {reply, ok, State}.

%%----------------------------------------------------------%
%% function : event_time
%% Arguments: TimerEvent - #'CosTimerEvent_TimerEventT'{utc, event_data}
%% Returns  : CosTime::UTO
%%-----------------------------------------------------------
event_time(_OE_THIS, State, #'CosTimerEvent_TimerEventT'{utc=Utc}) ->
    {reply,  'CosTime_UTO':oe_create([Utc],[{pseudo,true}]), State}.


%%--------------- LOCAL FUNCTIONS ----------------------------

%%--------------- MISC FUNCTIONS, E.G. DEBUGGING -------------
%%--------------- END OF MODULE ------------------------------
