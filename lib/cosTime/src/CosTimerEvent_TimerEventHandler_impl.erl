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
%% File    : CosTimerEvent_TimerEventHandler_impl.erl
%% Purpose : 
%%----------------------------------------------------------------------

-module('CosTimerEvent_TimerEventHandler_impl').

%%--------------- INCLUDES -----------------------------------
-include("cosTimeApp.hrl").


%%--------------- EXPORTS ------------------------------------
%%--------------- External -----------------------------------
%% Attributes (external)
-export(['_get_status'/2]).
%% Interface functions
-export([time_set/2, set_timer/4]).
-export([cancel_timer/2, set_data/3]).

%%--------------- gen_server specific exports ----------------
-export([handle_info/2, code_change/3]).
-export([init/1, terminate/2]).


%% Data structures
-record(state, {parent,
		parentPid,
		event,
		status = 'ESTimeCleared',
		timer,
		time,
		timeObj,
		myType,
		pushConsumer,
		uto}).
%% Data structures constructors
-define(get_InitState(P,PP,E,PC,TO), 
	#state{parent=P,
	       parentPid=PP,
	       event=E,
	       pushConsumer=PC,
	       timeObj=TO}).

%% Data structures selectors
-define(get_Status(S),          S#state.status).
-define(get_ParentPid(S),       S#state.parentPid).
-define(get_Parent(S),          S#state.parent).
-define(get_Event(S),           S#state.event).
-define(get_Timer(S),           S#state.timer).
-define(get_Time(S),            S#state.time).
-define(get_TimeObj(S),         S#state.timeObj).
-define(get_MyType(S),          S#state.myType).
-define(get_PushConsumer(S),    S#state.pushConsumer).
-define(get_Uto(S),             S#state.uto).

%% Data structures modifiers
-define(set_Status(S,V),        S#state{status=V}).
-define(set_ParentPid(S,PP),    S#state{parentPid=PP}).
-define(set_Parent(S,P),        S#state{parent=P}).
-define(set_Event(S,E),         S#state{event=E}).
-define(set_Timer(S,T),         S#state{timer=T}).
-define(set_Time(S,T),          S#state{time=T}).
-define(set_MyType(S,Ty),       S#state{myType=Ty}).
-define(set_PushConsumer(S,P),  S#state{pushConsumer=P}).
-define(set_Uto(S,U,Type),      S#state{uto=U, myType=Type}).
-define(set_TimeData(S,U,Ty,Ti),S#state{uto=U, myType=Ty, time=Ti}).

%% MISC
-define(not_Cancelled(S),       S#state.status =/= 'ESTimeCleared').
-define(is_TimeSet(S),          S#state.status == 'ESTimeSet').
-define(is_UtoSet(S),           S#state.uto =/= undefined).
-define(is_NotAbsolute(S),      S#state.myType =/= 'TTAbsolute').

%%-----------------------------------------------------------%
%% function : handle_info, code_change
%% Arguments: 
%% Returns  : 
%% Effect   : Functions demanded by the gen_server module. 
%%------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(Info, State) ->
    ?debug_print("INFO: ~p~n", [Info]),
    case Info of
        {'EXIT', Pid, _Reason} when Pid == ?get_ParentPid(State) ->
            ?debug_print("PARENT TERMINATED with reason: ~p~n",[_Reason]),
            {noreply, State};
        oe_event when ?not_Cancelled(State) ->
	    %% Push event
	    case catch 'CosEventComm_PushConsumer':push(?get_PushConsumer(State), 
							?get_Event(State)) of
		ok ->
		    ?debug_print("PUSHED: ~p~n", [?get_Event(State)]),
		    {noreply, ?set_Status(State, 'ESTriggered')};
		_Other->
		    ?debug_print("FAILED PUSH: ~p   ~p~n", [?get_Event(State), _Other]),
		    {noreply, ?set_Status(State, 'ESFailedTrigger')}
	    end;
        oe_periodic_event when ?not_Cancelled(State) ->
	    %% Push event
	    catch 'CosEventComm_PushConsumer':push(?get_PushConsumer(State), 
						   ?get_Event(State)),
	    {noreply, State};
        _ ->
            {noreply, State}
    end.

%%----------------------------------------------------------%
%% function : init, terminate
%% Arguments: 
%%-----------------------------------------------------------

init([Parent, ParentPid, PushConsumer, Event, TimeObj]) ->
    process_flag(trap_exit, true),
    {ok, ?get_InitState(Parent, ParentPid, Event, PushConsumer, TimeObj)}.

terminate(_Reason, State) ->
    clear_timer(State),
    ok.

%%-----------------------------------------------------------
%%------------------------ attributes -----------------------
%%-----------------------------------------------------------
%%----------------------------------------------------------%
%% Attribute: '_get_status'
%% Type     : readonly
%% Returns  : 'ESTimeSet' | 'ESTimeCleared' | 'ESTriggered' |
%%            'ESFailedTrigger'
%%-----------------------------------------------------------
'_get_status'(_OE_THIS, State) ->
    {reply, ?get_Status(State), State}.

%%-----------------------------------------------------------
%%------- Exported external functions -----------------------
%%-----------------------------------------------------------
%%----------------------------------------------------------%
%% function : time_set
%% Arguments: -
%% Returns  : {boolean(), CosTime::UTO}
%%-----------------------------------------------------------
time_set(_OE_THIS, State) when ?is_UtoSet(State) ->
    {reply, {?is_TimeSet(State), ?get_Uto(State)}, State};
time_set(_OE_THIS, State) ->
    Utc = #'TimeBase_UtcT'{time=0, inacclo = 0,inacchi = 0, tdf = 0},
    {reply, 
     {?is_TimeSet(State), 
      'CosTime_UTO':oe_create([Utc, ?get_TimeObj(State)], [{pseudo,true}|?CREATE_OPTS])}, 
     State}.


%%-----------------------------------------------------------
%% function : set_timer
%% Arguments: TimeType - 'TTAbsolute' | 'TTRelative' | 'TTPeriodic'
%%            TriggerTime - CosTime::UTO
%% Returns  : ok
%%-----------------------------------------------------------
set_timer(_OE_THIS, State, 'TTAbsolute', TriggerTime) ->
    NewState = clear_timer(State),
    ?time_TypeCheck(TriggerTime, 'CosTime_UTO'),
     case catch {'CosTime_UTO':'_get_time'(TriggerTime), 
		 'CosTime_UTO':'_get_time'(
		   'CosTime_TimeService':universal_time(?get_TimeObj(State)))} of
	 {Time, CurrentTime} when is_integer(Time) andalso is_integer(CurrentTime) andalso
				  Time > CurrentTime ->
	     %% Set a timer to send a message in (Time-CurrentTime)*10^-7 secs.
	     case timer:send_after(?convert_TimeT2TimerT(Time-CurrentTime), oe_event) of
		 {ok, TRef} ->
		     NewState1 = ?set_Timer(NewState, TRef),
		     NewState2 = ?set_Uto(NewState1, TriggerTime, 'TTAbsolute'),
		    ?debug_print("TIMER SET: ~p~n", [?convert_TimeT2TimerT(Time-CurrentTime)]),
		     {reply, ok, ?set_Status(NewState2, 'ESTimeSet')};
		 _->
		     corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
	     end;
	 {Time, CurrentTime} when is_integer(Time) andalso is_integer(CurrentTime) ->
	     corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO});
	 _->
	     corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
     end;
set_timer(_OE_THIS, State, 'TTRelative', TriggerTime) ->
    NewState = clear_timer(State),
    ?time_TypeCheck(TriggerTime, 'CosTime_UTO'),
    case catch {'CosTime_UTO':'_get_time'(TriggerTime), ?get_Time(State)} of
	{0,OldTime} when ?is_NotAbsolute(NewState) andalso is_integer(OldTime) ->
	    %% Set a timer to send a message within Time*10^-7 secs
	    case timer:send_after(OldTime, oe_event) of
		{ok, TRef} ->
		    NewState1 = ?set_Timer(NewState, TRef),
		    NewState2 = ?set_Uto(NewState1, TriggerTime, 'TTRelative'),
		    ?debug_print("TIMER SET: ~p~n", [?convert_TimeT2TimerT(OldTime)]),
		    {reply, ok, ?set_Status(NewState2, 'ESTimeSet')};
		_->
		    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
	    end;
	{UtoTime,_} when is_integer(UtoTime) ->
	    %% Set a timer to send a message within Time*10^-7 secs
	    Time = ?convert_TimeT2TimerT(UtoTime),
	    case timer:send_after(Time, oe_event) of
		{ok, TRef} ->
		    NewState1 = ?set_Timer(NewState, TRef),
		    NewState2 = ?set_TimeData(NewState1, TriggerTime, 
					      'TTRelative', Time),
		    ?debug_print("TIMER SET: ~p~n", [?convert_TimeT2TimerT(Time)]),
		    {reply, ok, ?set_Status(NewState2, 'ESTimeSet')};
		_->
		    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
	    end;
	
	_->
	    {reply, {'EXCEPTION', #'BAD_PARAM'{completion_status=?COMPLETED_NO}}, NewState}
    end;
set_timer(_OE_THIS, State, 'TTPeriodic', TriggerTime) ->
    NewState = clear_timer(State),
    ?time_TypeCheck(TriggerTime, 'CosTime_UTO'),
    case catch {'CosTime_UTO':'_get_time'(TriggerTime), ?get_Time(State)} of
	{0,OldTime} when ?is_NotAbsolute(NewState) andalso is_integer(OldTime) ->
	    %% Set a timer to send a message within Time*10^-7 secs
	    case timer:send_interval(OldTime, oe_periodic_event) of
		{ok, TRef} ->
		    NewState1 = ?set_Timer(NewState, TRef),
		    NewState2 = ?set_Uto(NewState1, TriggerTime, 'TTPeriodic'),
		    ?debug_print("TIMER SET: ~p~n", [?convert_TimeT2TimerT(OldTime)]),
		    {reply, ok, ?set_Status(NewState2, 'ESTimeSet')};
		_->
		    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
	    end;
	{UtoTime,_} when is_integer(UtoTime) ->
	    %% Set a timer to send a message within Time*10^-7 secs
	    Time = ?convert_TimeT2TimerT(UtoTime),
	    case timer:send_interval(Time, oe_periodic_event) of
		{ok, TRef} ->
		    NewState1 = ?set_Timer(NewState, TRef),
		    NewState2 = ?set_TimeData(NewState1, TriggerTime, 
					      'TTPeriodic', Time),
		    ?debug_print("TIMER SET: ~p~n", [?convert_TimeT2TimerT(Time)]),
		    {reply, ok, ?set_Status(NewState2, 'ESTimeSet')};
		_->
		    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
	    end;
	
	_->
	    {reply, {'EXCEPTION', #'BAD_PARAM'{completion_status=?COMPLETED_NO}}, NewState}
    end;
set_timer(_OE_THIS, _State, _, _) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%----------------------------------------------------------%
%% function : cancel_timer
%% Arguments: -
%% Returns  : boolean()
%%-----------------------------------------------------------
cancel_timer(_OE_THIS, State) ->
    NewState=clear_timer(State),
    case ?get_Status(NewState) of
	'ESTriggered' ->
	    {reply, false, NewState};
	'ESFailedTrigger' ->
	    {reply, false, NewState};
	_ ->
	    {reply, true, ?set_Status(NewState, 'ESTimeCleared')}
    end.

%%----------------------------------------------------------%
%% function : set_data
%% Arguments: EventData - any#
%% Returns  : ok
%%-----------------------------------------------------------
set_data(_OE_THIS, State, EventData) when is_record(EventData, any) ->
    {reply, ok, ?set_Event(State, EventData)};
set_data(_OE_THIS, _State, _EventData) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).


%%--------------- LOCAL FUNCTIONS ----------------------------
clear_timer(State) when ?get_Timer(State) == undefined ->
    State;
clear_timer(State) ->
    catch timer:cancel(?get_Timer(State)),
    ?set_Timer(State, undefined).

%%--------------- MISC FUNCTIONS, E.G. DEBUGGING -------------
%%--------------- END OF MODULE ------------------------------
