%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2009. All Rights Reserved.
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
%%
%%----------------------------------------------------------------------
%% File    : cosNotification_eventDB.erl
%% Purpose : 
%% Purpose : This module is supposed to centralize Event storage.
%% Comments: 
%% * Setting Order Policy to AnyOrder eq. Priority order
%%
%% * Setting Discard Policy to AnyOrder eq. RejectNewEvents.
%%
%% * DB ordering: Since the deliver- and discard-order may differ we need
%%   two ets-tables, both of type 'ordered_set'. They contain:
%%   - table 1 (T1): deliver order key and the associated event.
%%   - table 2 (T2): discard order key.
%%   
%%   When adding a new event we add, if necessary, related keys in T2.
%%   For example, if we should discard events in FIFO order, the delivery
%%   order may be set to Priority order. If the Max Event limit is reached
%%   we first look in T2 to find out which event to discard by using and
%%   reorder the key elements. T2 gives {TimeStamp, Priority}, which is used
%%   to lookup in T1 as {Priority, TimeStamp}.
%%   A TimeStamp is always included in the DB keys, even if FIFO or LIFO
%%   is used, since lots of events probably will have the same prioity and
%%   with a little bit of bad luck some events will never be delivered.
%%   
%%   Note: deliver order AnyOrder and PriorityOrder is equal since the later
%%         is defined as default.
%%         discard order AnyOrder and RejectNewEvents is equal since the later
%%         is defined as default.
%%   The keys used is ('-' indicates T2 is not needed and, thus, not instantiated):
%%   
%%   T1 policy         T1 Key             T2 Policy       T2 Key
%%   ------------------------------------------------------------------
%%   DeadlineOrder     {DL, Key, Prio}    PriorityOrder   {Prio, Key, DL}
%%   DeadlineOrder     {DL, Key}          FifoOrder       {Key, DL}
%%   DeadlineOrder     {DL, Key}          LifoOrder       {Key, DL}
%%   DeadlineOrder     {DL, Key}          RejectNewEvents     -
%%   DeadlineOrder     {DL, Key}          DeadlineOrder       -
%%   FifoOrder         {Key, DL}          DeadlineOrder   {DL, Key}
%%   FifoOrder         {Key, Prio}        PriorityOrder   {Prio, Key}
%%   FifoOrder         Key                RejectNewEvents     -
%%   FifoOrder         Key                Fifo                -
%%   FifoOrder         Key                Lifo                -
%%   PriorityOrder     {Prio, Key, DL}    DeadlineOrder   {DL, Key, Prio}
%%   PriorityOrder     {Prio, Key}        Fifo            {Key, Prio}
%%   PriorityOrder     {Prio, Key}        Lifo            {Key, Prio}
%%   PriorityOrder     {Prio, Key}        RejectNewEvents     -
%%   ------------------------------------------------------------------
%%   DL == Deadline, Key == TimeStamp, Prio == Priority
%%   
%%   NOTE: If defined, the Discard DB Keys are the same as in Event DB, except 
%%   that the first and last Key change place. {K1,K2}<->{K2,K1} and 
%%   {K1,K2,K3}<->{K3,K2,K1}.
%%----------------------------------------------------------------------

-module(cosNotification_eventDB).


%%--------------- INCLUDES -----------------------------------
-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").
-include_lib("cosTime/include/TimeBase.hrl").

%% Application files
-include("CosNotification.hrl").
-include("CosNotifyChannelAdmin.hrl").
-include("CosNotifyComm.hrl").
-include("CosNotifyFilter.hrl").

-include("CosNotification_Definitions.hrl").

%%--------------- EXPORTS ------------------------------------
%% Internal Filter Functions
-export([validate_event/5, 
	 create_db/4,
	 destroy_db/1,
	 get_event/1,
	 get_event/2,
	 get_events/2,
	 get_events/3,
	 delete_events/1,
	 update/2,
	 update/4,
	 add_event/2,
	 add_event/4,
	 add_and_get_event/2,
	 add_and_get_event/3,
	 add_and_get_event/4,
	 add_and_get_event/5,
	 gc_events/2,
	 gc_events_local/4,
	 gc_start/2,
	 filter_events/2,
	 filter_events/3,
	 status/2]).

%%--------------- DATA STRUCTURES ----------------------------
-record(dbRef, {orderRef, discardRef, orderPolicy, discardPolicy,
		defPriority, maxEvents, defStopT, startTsupport,
		stopTsupport, gcTime, gcLimit, timeRef}).



%%--------------- DEFINES ------------------------------------

-define(CreateRef(OR, DR, O, D, DP, ME, DS, StaT, StoT, GT, GL, TR), 
	#dbRef{orderRef=OR, discardRef=DR, orderPolicy=O, discardPolicy=D,
	       defPriority=DP, maxEvents=ME, defStopT=DS, startTsupport=StaT,
	       stopTsupport=StoT, gcTime=GT, gcLimit=round(ME*GL/100),
	       timeRef=TR}).


-define(get_OrderP(DR),            DR#dbRef.orderPolicy).
-define(get_DiscardP(DR),          DR#dbRef.discardPolicy).
-define(get_OrderRef(DR),          DR#dbRef.orderRef).
-define(get_DiscardRef(DR),        DR#dbRef.orderRef).
-define(get_DefPriority(DR),       DR#dbRef.defPriority).
-define(get_MaxEvents(DR),         DR#dbRef.maxEvents).
-define(get_DefStopT(DR),          DR#dbRef.defStopT).
-define(get_StartTsupport(DR),     DR#dbRef.startTsupport).
-define(get_StopTsupport(DR),      DR#dbRef.stopTsupport).
-define(get_GCTime(DR),            DR#dbRef.gcTime).
-define(get_GCLimit(DR),           DR#dbRef.gcLimit).
-define(get_TimeRef(DR),           DR#dbRef.timeRef).

-define(set_OrderP(DR, O),         DR#dbRef{orderPolicy = O}).
-define(set_DiscardP(DR, D),       DR#dbRef{discardPolicy = D}).
-define(set_OrderRef(DR, E),       DR#dbRef{orderRef = E}).
-define(set_DiscardRef(DR, E),     DR#dbRef{orderRef = E}).
-define(set_DefPriority(DR, DP),   DR#dbRef{defPriority = DP}).
-define(set_MaxEvents(DR, ME),     DR#dbRef{maxEvents = ME}).
-define(set_DefStopT(DR, DS),      DR#dbRef{defStopT = DS}).
-define(set_StartTsupport(DR, B),  DR#dbRef{startTsupport = B}).
-define(set_StopTsupport(DR, B),   DR#dbRef{stopTsupport = B}).

-define(is_StartTNotSupported(DR), DR#dbRef.startTsupport == false).
-define(is_StopTNotSupported(DR),  DR#dbRef.stopTsupport  == false).
-define(is_TimeoutNotUsed(DR),     DR#dbRef.defStopT  == 0).


%%------------------------------------------------------------
%% function : status
%% Arguments: DBRef
%%            Key - which information we want.
%% Returns  : Data related to the Key.
%%------------------------------------------------------------
status(DBRef, eventCounter) ->
    ets:info(?get_OrderRef(DBRef), size);
status(DBRef, {batchLimit, Limit}) ->
    case ets:info(?get_OrderRef(DBRef), size) of
	Current when is_integer(Current) andalso Current >= Limit ->
	    ?debug_print("BATCH LIMIT (~p) REACHED, CONTAINS: ~p~n", [Limit, Current]),
	    true;
	_Other ->
	    ?debug_print("BATCH LIMIT (~p) NOT REACHED, CONTAINS: ~p~n", 
			 [Limit, _Other]),
	    false
    end;
status(DBRef, {batchLimit, Limit, TemporaryMax}) ->
    case ets:info(?get_OrderRef(DBRef), size) of
	Current when is_integer(Current) andalso Current >= TemporaryMax ->
	    ?debug_print("MAX LIMIT (~p) REACHED, CONTAINS: ~p~n", 
			 [TemporaryMax, Current]),
	    true;
	Current when is_integer(Current) andalso Current >= Limit ->
	    ?debug_print("BATCH LIMIT (~p) REACHED, CONTAINS: ~p~n", [Limit, Current]),
	    true;
	_Other ->
	    ?debug_print("BATCH LIMIT (~p) NOT REACHED, CONTAINS: ~p~n", 
			 [Limit, _Other]),
	    false
    end;
status(_, _) ->
    error.


%%------------------------------------------------------------
%% function : gc_events_local
%% Arguments: DBRef
%% Returns  : 
%% Comment  : This function is intended for "emergency" GC, i.e.,
%%            when the DB must discard events we should first try
%%            to remove events with expired deadlines.
%%------------------------------------------------------------
gc_events_local(_, _, false, _) ->
    ok;
gc_events_local(_, _, _, 0) ->
    ok;
gc_events_local(ORef, DRef, _, _) ->
    gc_loop(ets:first(ORef), ORef, DRef).

%%------------------------------------------------------------
%% function : gc_events
%% Arguments: DBRef
%%            Priority - 'low', 'medium' or 'high'; will determine
%%            how important a fast gc is.
%% Returns  : 
%% Comment  : This function is intended for background GC.
%%------------------------------------------------------------
gc_events(DBRef, _Priority) when ?is_TimeoutNotUsed(DBRef) ->
    ok;
gc_events(DBRef, _Priority) when ?is_StopTNotSupported(DBRef) ->
    ok;
gc_events(DBRef, Priority) ->
    {M,S,U} = now(),
    case get(oe_GC_timestamp) of
	Num when {M,S,U} > Num ->
	    put(oe_GC_timestamp, {M,S+?get_GCTime(DBRef),U}),
	    spawn_link(?MODULE, gc_start, [DBRef, Priority]);
	_->
	    ok
    end.


%%------------------------------------------------------------
%% function : gc_start
%% Arguments: 
%% Returns  : 
%%------------------------------------------------------------
gc_start(#dbRef{orderRef = ORef, discardRef = DRef}, Priority) ->
    process_flag(priority, Priority),
    gc_loop(ets:first(ORef), ORef, DRef).

gc_loop('$end_of_table', _, _) ->
    ok;
gc_loop(Key, ORef, DRef) ->
    [{Keys,DL,_,_,_}]=ets:lookup(ORef, Key),
    case check_deadline(DL) of
	true when DRef == undefined ->
	    ets:delete(ORef, Key);
	true ->
	    ets:delete(ORef, Key),
	    gc_discard_DB(Keys, DRef);
	_ ->
	    ok
    end,
    gc_loop(ets:next(ORef, Key), ORef, DRef).

gc_discard_DB({Key1, Key2}, DRef) ->
    ets:delete(DRef, {Key2, Key1});
gc_discard_DB({Key1, Key2, Key3}, DRef) ->
    ets:delete(DRef, {Key3, Key2, Key1}).

%%------------------------------------------------------------
%% function : create_FIFO_Key
%% Arguments: 
%% Returns  : 
%%------------------------------------------------------------
create_FIFO_Key() ->
    {M, S, U} = erlang:now(),
    -M*1000000000000 - S*1000000 - U.

%%------------------------------------------------------------
%% function : convert_FIFO_Key
%% Arguments: 
%% Returns  : A now tuple
%% Comment  : Used when we must reuse a timestamp, i.e., only
%%            when we must reorder the DB.
%%------------------------------------------------------------
convert_FIFO_Key(Key) ->
    K = abs(Key),
    Secs = trunc(K/1000000),
    M = trunc(K/1000000000000),
    S = Secs-M*1000000,
    U = K - S*1000000-M*1000000000000,
    {M, S, U}.

%%------------------------------------------------------------
%% function : extract_priority
%% Arguments: Event
%%            Defalt Value
%%            Mapping Filter Value 
%%             - false  value not needed (depends on QoS type)
%%             - undefined value needed but no filter associated.
%% Returns  : 
%%------------------------------------------------------------
extract_priority(_, _, false) ->
    false;
extract_priority(#'CosNotification_StructuredEvent'
		 {header = #'CosNotification_EventHeader'
		  {variable_header = VH}}, DefPriority, undefined) ->
    extract_value(VH, ?not_Priority, DefPriority);
%% Maybe a unstructured event.
extract_priority(_, DefPriority, undefined) ->
    DefPriority;
extract_priority(_, _, PriorityOverride) ->
    %% Must have an associated MappingFilter for Priority.
    PriorityOverride.

%%------------------------------------------------------------
%% function : extract_start_time
%% Arguments: 
%% Returns  : 
%%------------------------------------------------------------
extract_start_time(_, false, _) ->
    false;
extract_start_time(#'CosNotification_StructuredEvent'
		 {header = #'CosNotification_EventHeader'
		  {variable_header = VH}}, _, TRef) ->
    ST = case extract_value(VH, ?not_StartTime, undefined) of
	     UTC when is_record(UTC, 'TimeBase_UtcT') ->
		 UTC;
	     _ ->
		 false
	 end,
    convert_time(ST, TRef, now());
extract_start_time(_, _, _) ->
    false.

%%------------------------------------------------------------
%% function : extract_deadline
%% Arguments: Structured Event
%%            Default Timeout Value - TimeT or UtcT (see cosTime).
%%            StopTSupported - boolean().
%%            TRef - reference to TimeService
%%            Mapping Filter Value 
%%             - false eq. value not needed (depends on QoS type)
%%             - undefined eq. value needed but no filter associated.
%%            Now - used when we want to reuse old TimeStamp which
%%                  must be done when changing QoS.
%% Returns  : A modified return from now().
%%------------------------------------------------------------
extract_deadline(_, _, _, _, false) ->
    false;
extract_deadline(Event, DefaultT, StopTSupported, TRef, MappingVal) ->
    extract_deadline(Event, DefaultT, StopTSupported, TRef, MappingVal, now()).

extract_deadline(_, _, _, _, false, _) ->
    false;
extract_deadline(#'CosNotification_StructuredEvent'
		 {header = #'CosNotification_EventHeader'
		  {variable_header = VH}}, DefaultT, StopTSupported, 
		 TRef, undefined, Now) ->
    DL = case extract_value(VH, ?not_Timeout, undefined) of
	     undefined when StopTSupported == true, TRef =/= undefined ->
		 case extract_value(VH, ?not_StopTime, undefined) of
		     undefined ->
			 DefaultT;
		     DefinedTime ->
			 DefinedTime
		 end;
	     undefined ->
		 DefaultT;
	     DefinedTime ->
		 DefinedTime
	 end,
    convert_time(DL, TRef, Now);
%% Maybe a unstructured event.
extract_deadline(_, Time, _, TRef, undefined, Now) ->
    convert_time(Time, TRef, Now);
extract_deadline(_, _, _, TRef, DOverride, Now) ->
    %% Must have an associated MappingFilter defining a Deadline.
    convert_time(DOverride, TRef, Now).

convert_time(0, _, _) ->
    false;
convert_time(UTC, TRef, {M,S,U}) when is_record(UTC, 'TimeBase_UtcT') ->
    case catch get_time_diff(UTC, TRef) of
	{'EXCEPTION', _} ->
	    false;
	{'EXIT', _} ->
	    false;
	DL ->
	    MicroSecs = round(DL/10),
	    Secs      = round(MicroSecs/1000000),
	    MegaSecs  = round(Secs/1000000),
	    {-M-MegaSecs, -S-Secs+MegaSecs, -U-MicroSecs+Secs}
    end;
convert_time(DL, _, {M,S,U}) when is_integer(DL) ->
    MicroSecs = round(DL/10),
    Secs      = round(MicroSecs/1000000),
    MegaSecs  = round(Secs/1000000),
    {-M-MegaSecs, -S-Secs+MegaSecs, -U-MicroSecs+Secs};
convert_time(_, _, _) ->
    false.


get_time_diff(UTC, TRef) ->
    UTO  = 'CosTime_TimeService':universal_time(TRef),
    UTO2 = 'CosTime_TimeService':uto_from_utc(TRef, UTC),
    TIO  = 'CosTime_UTO':time_to_interval(UTO, UTO2),
    #'TimeBase_IntervalT'{lower_bound=LB, upper_bound = UB} = 
	'CosTime_TIO':'_get_time_interval'(TIO),
    UB-LB.

check_deadline(DL) when is_tuple(DL) ->
    {M,S,U}  = now(),
    DL >= {-M,-S,-U};
check_deadline(_DL) ->
    %% This case will cover if no timeout is set.
    false.

check_start_time(ST) when is_tuple(ST) ->
    {M,S,U}  = now(),
    ST >= {-M,-S,-U};
check_start_time(_ST) ->
    %% This case will cover if no earliest delivery time is set.
    true.

%%------------------------------------------------------------
%% function : extract_value
%% Arguments: A Property Sequence
%%            ID - wanted property string()
%%            Other - default-value.
%% Returns  : Value associated with given ID or default value.
%%------------------------------------------------------------
extract_value([], _, Other) ->
    Other;
extract_value([#'CosNotification_Property'{name=ID, value=V}|_], ID, _) ->
    any:get_value(V);
extract_value([_H|T], ID, Other) ->
    extract_value(T, ID, Other).

%%------------------------------------------------------------
%% function : get_event
%% Arguments: 
%% Returns  : 
%%------------------------------------------------------------
get_event(DBRef) ->
    get_event(DBRef, true).
get_event(DBRef, Delete) ->
    case get_events(DBRef, 1, Delete) of
	{[], false} ->
	    {[], false};
	{[], false, Keys} ->
	    {[], false, Keys};
	{[Event], Bool} ->
	    {Event, Bool};
	{[Event], Bool, Keys} ->
	    {Event, Bool, Keys}
    end.

%%------------------------------------------------------------
%% function : get_events
%% Arguments: 
%% Returns  : A list of events (possibly empty) and a boolean
%%            indicating if event found.
%% Comments : Try to extract Max events from the database.
%%------------------------------------------------------------
get_events(#dbRef{orderRef = ORef, discardRef = DRef}, Max) ->
    event_loop(ets:last(ORef), ORef, DRef, Max, [], [], true).

get_events(#dbRef{orderRef = ORef, discardRef = DRef}, Max, Delete) ->
    event_loop(ets:last(ORef), ORef, DRef, Max, [], [], Delete).

event_loop('$end_of_table', _, _, _, [], _, true) ->
    {[], false};
event_loop('$end_of_table', _, _, _, [], [], _) ->
    {[], false, []};
event_loop('$end_of_table', _ORef, _, _, Accum, _Keys, true) ->
    {lists:reverse(Accum), true};
event_loop('$end_of_table', _ORef, _, _, Accum, Keys, _) ->
    {lists:reverse(Accum), true, Keys};
event_loop(_, _ORef, _, 0, [], _Keys, true) ->
    %% Only possible if some tries to pull a sequence of 0 events.
    %% Should we really test for this case?
    {[], false};
event_loop(_, _ORef, _, 0, [], Keys, _) ->
    {[], false, Keys};
event_loop(_, _ORef, _, 0, Accum, _Keys, true) ->
    {lists:reverse(Accum), true};
event_loop(_, _ORef, _, 0, Accum, Keys, _) ->
    {lists:reverse(Accum), true, Keys};
event_loop(Key, ORef, undefined, Left, Accum, Keys, Delete) ->
    [{_,DL,ST,_PO,Event}]=ets:lookup(ORef, Key),
    case check_deadline(DL) of
	true ->
	    ets:delete(ORef, Key),
	    event_loop(ets:prev(ORef, Key), ORef, undefined, 
		       Left, Accum, Keys, Delete);
	false ->
	    case check_start_time(ST) of
		true when Delete == true ->
		    ets:delete(ORef, Key),
		    event_loop(ets:prev(ORef, Key), ORef, undefined, 
			       Left-1, [Event|Accum], Keys, Delete);
		true ->
		    event_loop(ets:prev(ORef, Key), ORef, undefined, 
			       Left-1, [Event|Accum], [{ORef, Key}|Keys], Delete);
		false ->
		    event_loop(ets:prev(ORef, Key), ORef, undefined, 
			       Left, Accum, Keys, Delete)
	    end
    end;
event_loop({Key1, Key2}, ORef, DRef, Left, Accum, Keys, Delete) ->
    [{_,DL,ST,_PO,Event}]=ets:lookup(ORef, {Key1, Key2}),
    case check_deadline(DL) of
	true ->
	    ets:delete(ORef, {Key1, Key2}),
	    ets:delete(DRef, {Key2, Key1}),
	    event_loop(ets:prev(ORef, {Key1, Key2}), ORef, DRef, 
		       Left, Accum, Keys, Delete);
	false ->
	    case check_start_time(ST) of
		true when Delete == true ->
		    ets:delete(ORef, {Key1, Key2}),
		    ets:delete(DRef, {Key2, Key1}),
		    event_loop(ets:prev(ORef, {Key1, Key2}), ORef, DRef, 
			       Left-1, [Event|Accum], Keys, Delete);
		true ->
		    event_loop(ets:prev(ORef, {Key1, Key2}), ORef, DRef, 
			       Left-1, [Event|Accum], 
			       [{ORef, {Key1, Key2}}, {DRef, {Key2, Key1}}|Keys], 
			       Delete);
		false ->
		    event_loop(ets:prev(ORef, {Key1, Key2}), ORef, DRef, 
			       Left, Accum, Keys, Delete)
	    end
    end;    
event_loop({Key1, Key2, Key3}, ORef, DRef, Left, Accum, Keys, Delete) ->
    [{_,DL,ST,_PO,Event}]=ets:lookup(ORef, {Key1, Key2, Key3}),
    case check_deadline(DL) of
	true ->
	    ets:delete(ORef, {Key1, Key2, Key3}),
	    ets:delete(DRef, {Key3, Key2, Key1}),
	    event_loop(ets:prev(ORef, {Key1, Key2, Key3}), ORef, DRef, 
		       Left, Accum, Keys, Delete);
	false ->
	    case check_start_time(ST) of
		true when Delete  == true ->
		    ets:delete(ORef, {Key1, Key2, Key3}),
		    ets:delete(DRef, {Key3, Key2, Key1}),
		    event_loop(ets:prev(ORef, {Key1, Key2, Key3}), ORef, DRef, 
			       Left-1, [Event|Accum], Keys, Delete);
		true ->
		    event_loop(ets:prev(ORef, {Key1, Key2, Key3}), ORef, DRef, 
			       Left-1, [Event|Accum], 
			       [{ORef, {Key1, Key2, Key3}}, 
				{DRef, {Key3, Key2, Key1}}|Keys], Delete);
		false ->
		    event_loop(ets:prev(ORef, {Key1, Key2, Key3}), ORef, DRef, 
			       Left, Accum, Keys, Delete)
	    end
    end.

%%------------------------------------------------------------
%% function : delete_events
%% Arguments: EventList - what's returned by get_event, get_events
%%                        and add_and_get_event.
%% Returns  : 
%% Comment  : Shall be invoked when it's safe to premanently remove
%%            the events found in the EventList.
%%            
%%------------------------------------------------------------
delete_events([]) ->
    ok;
delete_events([{DB, Key}|T]) ->
    ets:delete(DB, Key),
    delete_events(T).

%%------------------------------------------------------------
%% function : update
%% Arguments: 
%% Returns  : 
%% Comment  : As default we shall deliver Events in Priority order.
%%            Hence, if AnyOrder set we will still deliver in
%%            Priority order.
%%------------------------------------------------------------
update(undefined, _QoS) ->
    ok;
update(DBRef, QoS) ->
    update(DBRef, QoS, undefined, undefined).

update(DBRef, QoS, LifeFilter, PrioFilter) ->
    case updated_order(DBRef, ?not_GetOrderPolicy(QoS)) of
	false ->
	    case updated_discard(DBRef, ?not_GetDiscardPolicy(QoS)) of
		false ->
		    DBR2 = ?set_DefPriority(DBRef, ?not_GetPriority(QoS)), 
		    DBR3 = ?set_MaxEvents(DBR2, ?not_GetMaxEventsPerConsumer(QoS)),
		    DBR4 = ?set_DefStopT(DBR3, ?not_GetTimeout(QoS)),
		    DBR5 = ?set_StartTsupport(DBR4, ?not_GetStartTimeSupported(QoS)),
		    DBR6 = ?set_StopTsupport(DBR5, ?not_GetStopTimeSupported(QoS)),
		    case ets:info(?get_OrderRef(DBR6), size) of
			N when N =< ?get_MaxEvents(DBR6) ->
			    %% Even if the QoS MaxEvents have been changed
			    %% we don't reach the limit.
			    DBR6;
			N ->
			    %% The QoS MaxEvents must have been decreased.
			    discard_events(DBR6, N-?get_MaxEvents(DBR6)),
			    DBR6
		    end;
		true ->
		    destroy_discard_db(DBRef),
		    NewDBRef = create_db(QoS, ?get_GCTime(DBRef), ?get_GCLimit(DBRef),
					 ?get_TimeRef(DBRef)),
		    move_events(DBRef, NewDBRef, ets:first(?get_OrderRef(DBRef)),
				LifeFilter, PrioFilter)
	    end;
	true ->
	    destroy_discard_db(DBRef),
	    NewDBRef = create_db(QoS, ?get_GCTime(DBRef), ?get_GCLimit(DBRef),
				?get_TimeRef(DBRef)),
	    move_events(DBRef, NewDBRef, ets:first(?get_OrderRef(DBRef)),
			LifeFilter, PrioFilter)
    end.

updated_order(#dbRef{orderPolicy = Equal}, Equal) -> false;
updated_order(#dbRef{orderPolicy = ?not_PriorityOrder}, ?not_AnyOrder) -> false;
updated_order(#dbRef{orderPolicy = ?not_AnyOrder}, ?not_PriorityOrder) -> false;
updated_order(_, _) -> true.

updated_discard(#dbRef{discardPolicy = Equal}, Equal) -> false;
updated_discard(#dbRef{discardPolicy = ?not_RejectNewEvents}, ?not_AnyOrder) -> false;
updated_discard(#dbRef{discardPolicy = ?not_AnyOrder}, ?not_RejectNewEvents) -> false;
updated_discard(_, _) -> true.

move_events(DBRef, NewDBRef, '$end_of_table', _, _) ->
    destroy_order_db(DBRef),
    case ets:info(?get_OrderRef(NewDBRef), size) of
	N when N =< ?get_MaxEvents(NewDBRef) ->
	    %% Even if the QoS MaxEvents have been changed
	    %% we don't reach the limit.
	    NewDBRef;
	N ->
	    %% The QoS MaxEvents must have been decreased.
	    discard_events(DBRef, N-?get_MaxEvents(NewDBRef)),
	    NewDBRef
    end;
move_events(DBRef, NewDBRef, Key, LifeFilter, PrioFilter) ->
    [{Keys, DeadLine, StartTime, PriorityOverride, Event}] = 
	ets:lookup(?get_OrderRef(DBRef), Key),
    case check_deadline(DeadLine) of
	true ->
	    ok;
	_->
	    write_event(?get_OrderP(DBRef), 
			{Keys, DeadLine, StartTime, PriorityOverride, Event}, 
			DBRef, NewDBRef, Key, LifeFilter, PrioFilter)
    end,
    ets:delete(?get_OrderRef(DBRef), Key),
    move_events(DBRef, NewDBRef, ets:next(?get_OrderRef(DBRef), Key), 
		LifeFilter, PrioFilter).

%% We cannot use do_add_event directly since we MUST lookup the timestamp (TS).
write_event(?not_DeadlineOrder, {{_, TS, _Prio}, DL, ST, PO, Event}, _DBRef, NewDBRef, 
	    _Key, _LifeFilter, _PrioFilter) ->
    StartT = update_starttime(NewDBRef, Event, ST),
    %% Deadline and Priority previously extracted.
    do_add_event(NewDBRef, Event, TS, DL, StartT, PO);
write_event(?not_DeadlineOrder, {{_, TS}, DL, _ST, PO, Event}, _DBRef, NewDBRef, 
	    _Key, _LifeFilter, PrioFilter) ->
    %% Priority not previously extracted.
    POverride = update_priority(NewDBRef, PrioFilter, Event, PO),
    StartT    = extract_start_time(Event, ?get_StartTsupport(NewDBRef), 
				   ?get_TimeRef(NewDBRef)),
    do_add_event(NewDBRef, Event, TS, DL, StartT, POverride);
write_event(?not_FifoOrder, {{TS, _PorD}, DL, ST, PO, Event}, _DBRef, NewDBRef, 
	    _Key, LifeFilter, PrioFilter) ->
    %% Priority or Deadline have been extracted before but we cannot tell which.
    POverride = update_priority(NewDBRef, PrioFilter, Event, PO),
    DeadL     = update_deadline(NewDBRef, LifeFilter, Event, TS, DL),
    StartT    = update_starttime(NewDBRef, Event, ST),
    do_add_event(NewDBRef, Event, TS, DeadL, StartT, POverride);
write_event(?not_FifoOrder, {TS, DL, ST, PO, Event}, _DBRef, NewDBRef, 
	    _Key, LifeFilter, PrioFilter) ->
    %% Priority and Deadline not extracetd before. Do it now.
    POverride = update_priority(NewDBRef, PrioFilter, Event, PO),
    DeadL     = update_deadline(NewDBRef, LifeFilter, Event, TS, DL),
    StartT    = update_starttime(NewDBRef, Event, ST),
    do_add_event(NewDBRef, Event, TS, DeadL, StartT, POverride);
%% Order Policy must be AnyOrder or PriorityOrder.
write_event(_, {{_Prio, TS}, DL, ST, PO, Event}, _DBRef, NewDBRef, 
	    _Key, LifeFilter, _PrioFilter) ->
    DeadL  = update_deadline(NewDBRef, LifeFilter, Event, TS, DL),
    StartT = update_starttime(NewDBRef, Event, ST),
    do_add_event(NewDBRef, Event, TS, DeadL, StartT, PO);
write_event(_, {{_Prio, TS, DL}, DL, ST, PO, Event}, _DBRef, NewDBRef, _Key, _, _) ->
    %% Both Deadline and Priority have been extracetd before.
    StartT = update_starttime(NewDBRef, Event, ST),
    do_add_event(NewDBRef, Event, TS, DL, StartT, PO).


%%------------------------------------------------------------
%% function : update_priority
%% Arguments: 
%% Returns  : 
%% Comment  : The purpose with this function is to avoid
%%            calling MappingFilter priority again, especially
%%            deadline again (we especially want to avoid calling
%%            since it may require intra-ORB communication. 
%%            Use only when doing an update.
%%------------------------------------------------------------
update_priority(DBRef, PrioFilter, Event, OldPrio) when is_atom(OldPrio) ->
    get_prio_mapping_value(DBRef, PrioFilter, Event);
update_priority(_DBRef, _PrioFilter, _Event, OldPrio) ->
    OldPrio.

%%------------------------------------------------------------
%% function : update_deadline
%% Arguments: 
%% Returns  : 
%% Comment  : The purpose with this function is to avoid
%%            calling MappingFilter or parsing the events for
%%            deadline again (we especially want to avoid calling
%%            the MappingFilter since it may require intra-ORB
%%            communication. Use only when doing an update.
%%------------------------------------------------------------
update_deadline(DBRef, _LifeFilter, _Event, _TS, _OldDeadL) when 
  ?get_DiscardP(DBRef) =/= ?not_DeadlineOrder,
  ?get_OrderP(DBRef) =/= ?not_DeadlineOrder,
  ?is_StopTNotSupported(DBRef) ->
    %% We do not need to extract the Deadline since it will not be used.
    false;
update_deadline(DBRef, LifeFilter, Event, TS, OldDeadL) when is_atom(OldDeadL) ->
    %% We need the Deadline and it have not been extracetd before.
    DOverride = get_life_mapping_value(DBRef, LifeFilter, Event),
    %% We must find out when the event was delivered; setting a deadline using
    %% a new timestamp would not be accurate since we cannot tell for how long
    %% the event have been waiting.
    OldNow = convert_FIFO_Key(TS),
    extract_deadline(Event, ?get_DefStopT(DBRef), ?get_StopTsupport(DBRef), 
		     ?get_TimeRef(DBRef), DOverride, OldNow);
update_deadline(_DBRef, _LifeFilter, _Event, _TS, OldDeadL) ->
    %% We need the Deadline and it have been extracetd before.
    OldDeadL.

%%------------------------------------------------------------
%% function : update_starttime
%% Arguments: 
%% Returns  : 
%% Comment  : The purpose with this function is to avoid
%%            parsing the events for starttime again.
%%            Use only when doing an update.
%%------------------------------------------------------------
update_starttime(DBRef, Event, OldStartT) when is_atom(OldStartT) ->
    %% Probably not previously extracted; try to get it.
    extract_start_time(Event, ?get_StartTsupport(DBRef), ?get_TimeRef(DBRef));
update_starttime(_DBRef, _Event, OldStartT) ->
    %% Previously extracted.
    OldStartT.

%%------------------------------------------------------------
%% function : discard_events
%% Arguments: DBRef
%%            N - number of events we must discard.
%% Returns  : 
%% Comment  : As default we shall Reject New Events when the limit
%%            is reached. Any discard order will do the same.
%%
%%            This function can only be used for the discard policies
%%            Fifo, Priority and Deadline. Any or RejectNewEvents
%%            will not allow events to be stored at all, i.e., no events
%%            to discard. Lifo will not be stored either since when
%%            trying to add an event it is definitely the last event in.
%%------------------------------------------------------------
%% Since no Discard DB must the same Order policy.
discard_events(#dbRef{orderRef = ORef, discardRef = undefined, 
		      discardPolicy = ?not_DeadlineOrder}, N) ->
    ?debug_print("Discarding ~p events Deadline Order.",[N]),
    index_loop_backward(ets:last(ORef), undefined, ORef, N);
discard_events(#dbRef{orderRef = ORef, discardRef = DRef, 
		      discardPolicy = ?not_DeadlineOrder}, N) ->
    ?debug_print("Discarding ~p events Deadline Order.",[N]),
    index_loop_backward(ets:last(DRef), DRef, ORef, N);
%% Fifo.
discard_events(#dbRef{orderRef = ORef, discardRef = undefined, 
		      discardPolicy = ?not_FifoOrder}, N) ->
    ?debug_print("Discarding ~p events Fifo Order.",[N]),
    index_loop_backward(ets:last(ORef), undefined, ORef, N);
discard_events(#dbRef{orderRef = ORef, discardRef = DRef, 
		      discardPolicy = ?not_FifoOrder}, N) ->
    ?debug_print("Discarding ~p events Fifo Order.",[N]),
    index_loop_backward(ets:last(DRef), DRef, ORef, N);
%% Lifo- or Priority-Order
discard_events(#dbRef{orderRef = ORef, discardRef = undefined}, N) ->
    ?debug_print("Discarding ~p events Lifo- or Priority-Order.",[N]),
    index_loop_forward(ets:first(ORef), undefined, ORef, N);
discard_events(#dbRef{orderRef = ORef, discardRef = DRef}, N) ->
    ?debug_print("Discarding ~p events Lifo- or Priority-Order.",[N]),
    index_loop_forward(ets:first(DRef), DRef, ORef, N).


index_loop_forward('$end_of_table', _, _, _Left) ->
    ok;
index_loop_forward(_, _, _, 0) ->
    ok;
index_loop_forward(Key, undefined, ORef, Left) ->
    ets:delete(ORef, Key),
    NewKey=ets:next(ORef, Key),
    index_loop_forward(NewKey, undefined, ORef, Left-1);

index_loop_forward({Key1, Key2, Key3}, DRef, ORef, Left) ->
    ets:delete(DRef, {Key1, Key2, Key3}),
    ets:delete(ORef, {Key3, Key2, Key1}),
    NewKey=ets:next(DRef, {Key1, Key2, Key3}),
    index_loop_forward(NewKey, DRef, ORef, Left-1);

index_loop_forward({Key1, Key2}, DRef, ORef, Left) ->
    ets:delete(DRef, {Key1, Key2}),
    ets:delete(ORef, {Key2, Key1}),
    NewKey=ets:next(DRef, {Key1, Key2}),
    index_loop_forward(NewKey, DRef, ORef, Left-1).

index_loop_backward('$end_of_table', _, _, _) ->
    ok;
index_loop_backward(_, _, _, 0) ->
    ok;
index_loop_backward(Key, undefined, ORef, Left) ->
    ets:delete(ORef, Key),
    NewKey=ets:prev(ORef, Key),
    index_loop_backward(NewKey, undefined, ORef, Left-1);
index_loop_backward({Key1, Key2}, DRef, ORef, Left) ->
    ets:delete(DRef, {Key1, Key2}),
    ets:delete(ORef, {Key2, Key1}),
    NewKey=ets:prev(DRef, {Key1, Key2}),
    index_loop_backward(NewKey, DRef, ORef, Left-1);
index_loop_backward({Key1, Key2, Key3}, DRef, ORef, Left) ->
    ets:delete(DRef, {Key1, Key2, Key3}),
    ets:delete(ORef, {Key3, Key2, Key1}),
    NewKey=ets:prev(DRef, {Key1, Key2, Key3}),
    index_loop_backward(NewKey, DRef, ORef, Left-1).

%%------------------------------------------------------------
%% function : add_and_get_event
%% Arguments: DBRef and Event
%% Returns  : {[], bool()} | {Event, bool()} 
%% Comment  : This function is a mixture of ad anf get events.
%%            The intended use to avoid storing an event when
%%            not necessary.
%%------------------------------------------------------------
add_and_get_event(DBRef, Event) ->
    add_and_get_event(DBRef, Event, undefined, undefined, true).

add_and_get_event(DBRef, Event, Delete) ->
    add_and_get_event(DBRef, Event, undefined, undefined, Delete).

add_and_get_event(DBRef, Event, LifeFilter, PrioFilter) ->
    add_and_get_event(DBRef, Event, LifeFilter, PrioFilter, true).

add_and_get_event(DBRef, Event, LifeFilter, PrioFilter, Delete) ->
    case ets:info(?get_OrderRef(DBRef), size) of
	0 when ?is_StartTNotSupported(DBRef), ?is_StopTNotSupported(DBRef),
	       Delete == true ->
	    %% No stored events and no timeouts used; just return the event.
	    {Event, false};
	0 when ?is_StartTNotSupported(DBRef), ?is_StopTNotSupported(DBRef) ->
	    %% No stored events and no timeouts used; just return the event.
	    {Event, false, []};
	0 when ?is_StartTNotSupported(DBRef) ->
	    %% Only deadline supported, lookup values and cehck if ok.
	    DOverride = get_life_mapping_value(DBRef, LifeFilter, Event),
	    DL = extract_deadline(Event, ?get_DefStopT(DBRef), 
				  ?get_StopTsupport(DBRef), ?get_TimeRef(DBRef), 
				  DOverride),
	    case check_deadline(DL) of
		true when Delete == true ->
		    %% Expired, just discard the event.
		    {[], false};
		true ->
		    {[], false, []};
		_ when Delete == true ->
		    %% Not expired, we can safely return the event.
		    {Event, false};
		_ ->
		    %% Not expired, we can safely return the event.
		    {Event, false, []}
	    end;
	0 when ?is_StopTNotSupported(DBRef) ->
	    %% Only starttime allowed, test if we can deliver the event now.
	    ST = extract_start_time(Event, ?get_StartTsupport(DBRef), 
				    ?get_TimeRef(DBRef)),
	    case check_start_time(ST) of
		false when Delete == true ->
		    DOverride = get_life_mapping_value(DBRef, LifeFilter, Event),
		    POverride = get_prio_mapping_value(DBRef, PrioFilter, Event),
		    DL = extract_deadline(Event, ?get_DefStopT(DBRef), 
					  ?get_StopTsupport(DBRef), 
					  ?get_TimeRef(DBRef), DOverride),
		    do_add_event(DBRef, Event, create_FIFO_Key(), DL, ST, POverride),
		    {[], true};
		false ->
		    DOverride = get_life_mapping_value(DBRef, LifeFilter, Event),
		    POverride = get_prio_mapping_value(DBRef, PrioFilter, Event),
		    DL = extract_deadline(Event, ?get_DefStopT(DBRef), 
					  ?get_StopTsupport(DBRef), 
					  ?get_TimeRef(DBRef), DOverride),
		    do_add_event(DBRef, Event, create_FIFO_Key(), DL, ST, POverride),
		    {[], true, []};
		_ when Delete == true ->
		    %% Starttime ok, just return the event.
		    {Event, false};
		_ ->
		    %% Starttime ok, just return the event.
		    {Event, false, []}
	    end;
	_->
	    %% Event already stored, just have to accept the overhead.
	    ST = extract_start_time(Event, ?get_StartTsupport(DBRef), 
				    ?get_TimeRef(DBRef)),
	    DOverride = get_life_mapping_value(DBRef, LifeFilter, Event),
	    POverride = get_prio_mapping_value(DBRef, PrioFilter, Event),
	    DL = extract_deadline(Event, ?get_DefStopT(DBRef),
				  ?get_StopTsupport(DBRef), 
				  ?get_TimeRef(DBRef), DOverride),
	    do_add_event(DBRef, Event, create_FIFO_Key(), DL, ST, POverride),
	    get_event(DBRef, Delete)
    end.
	    
%%------------------------------------------------------------
%% function : add_event
%% Arguments: DBRef and Event
%% Returns  : true (or whatever 'ets:insert' returns) |
%%            {'EXCEPTION',  #'IMP_LIMIT'{}}
%% Comment  : As default we shall deliver Events in Priority order.
%%            Hence, if AnyOrder set we will still deliver in
%%            Priority order. But we cannot use only the Priority
%%            value since if "all" events have the same priority
%%            there is a risk that some never will be delivered if
%%            the EventDB always contain events.
%% 
%%            When discard and order policy is equal we only use one
%%            DB since all we have to do is to "read from the other
%%            end" to discard the correct event(s).
%%
%%            In the discard DB we must also store keys necessary to
%%            lookup the event in the order DB.
%%
%%            If event limit reached 'IMPL_LIMIT' is raised if
%%            the discard policy is RejectNewEvents or AnyOrder.
%%            Theses two policies we currently define to be equal.
%%------------------------------------------------------------

add_event(DBRef, Event) ->
    %% Save overhead by first checking if we really need to extract
    %% Deadline and/or Priority.
    Deadline = get_life_mapping_value(DBRef, undefined, Event),
    Priority = get_prio_mapping_value(DBRef, undefined, Event),
    add_event_helper(DBRef, Event, Deadline, Priority).

add_event(DBRef, Event, LifeFilter, PrioFilter) ->
    %% Save overhead by first checking if we really need to extract
    %% Deadline and/or Priority.
    Deadline = get_life_mapping_value(DBRef, LifeFilter, Event),
    Priority = get_prio_mapping_value(DBRef, PrioFilter, Event),
    add_event_helper(DBRef, Event, Deadline, Priority).
    
add_event_helper(DBRef, Event, DOverride, POverride) ->
    case ets:info(?get_OrderRef(DBRef), size) of
	N when N < ?get_MaxEvents(DBRef), N > ?get_GCLimit(DBRef) ->
	    gc_events(DBRef, low),
	    DL = extract_deadline(Event, ?get_DefStopT(DBRef), 
				  ?get_StopTsupport(DBRef), ?get_TimeRef(DBRef), 
				  DOverride),
	    case check_deadline(DL) of
		true ->
		    true;
		_ ->
		    ST = extract_start_time(Event, ?get_StartTsupport(DBRef), 
					    ?get_TimeRef(DBRef)),
		    do_add_event(DBRef, Event, create_FIFO_Key(), DL, ST, POverride)
	    end;
	N when N < ?get_MaxEvents(DBRef) ->
	    DL = extract_deadline(Event, ?get_DefStopT(DBRef), 
				  ?get_StopTsupport(DBRef), ?get_TimeRef(DBRef), 
				  DOverride),
	    case check_deadline(DL) of
		true ->
		    true;
		_ ->
		    ST = extract_start_time(Event, ?get_StartTsupport(DBRef), 
					    ?get_TimeRef(DBRef)),
		    do_add_event(DBRef, Event, create_FIFO_Key(), DL, ST, POverride)
	    end;
	_N when ?get_DiscardP(DBRef) == ?not_RejectNewEvents ->
	    gc_events(DBRef, low),
	    corba:raise(#'IMP_LIMIT'{completion_status=?COMPLETED_NO});
	_N when ?get_DiscardP(DBRef) == ?not_AnyOrder ->
	    gc_events(DBRef, low),
	    corba:raise(#'IMP_LIMIT'{completion_status=?COMPLETED_NO});
	_N when ?get_DiscardP(DBRef) == ?not_LifoOrder ->
	    gc_events(DBRef, low),
	    corba:raise(#'IMP_LIMIT'{completion_status=?COMPLETED_NO});
	_N ->
	    gc_events(DBRef, low),
	    %% Other discard policy; we must first store the event
	    %% and the look up in the Discard DB which event we
	    %% should remove.
	    DL = extract_deadline(Event, ?get_DefStopT(DBRef), 
				  ?get_StopTsupport(DBRef), ?get_TimeRef(DBRef), 
				  DOverride),
	    case check_deadline(DL) of
		true ->
		    true;
		_ ->
		    ST = extract_start_time(Event, ?get_StartTsupport(DBRef), 
					    ?get_TimeRef(DBRef)),
		    do_add_event(DBRef, Event, create_FIFO_Key(), DL, ST, POverride),
		    discard_events(DBRef, 1)
	    end
    end.


do_add_event(#dbRef{orderRef = ORef, orderPolicy = ?not_DeadlineOrder,
		 discardRef = DRef, discardPolicy = ?not_PriorityOrder,
		 defPriority = DefPrio, defStopT = _DefStopT}, Event, Key, DL, ST, PO) ->
    Prio = extract_priority(Event, DefPrio, PO),
    ets:insert(ORef, {{DL, Key, Prio}, DL, ST, PO, Event}),
    ets:insert(DRef, {{Prio, Key, DL}});
do_add_event(#dbRef{orderRef = ORef, orderPolicy = ?not_DeadlineOrder,
		 discardRef = DRef, discardPolicy = ?not_FifoOrder,
		 defStopT = _DefStopT}, Event, Key, DL, ST, PO) ->
    ets:insert(ORef, {{DL, Key}, DL, ST, PO, Event}),
    ets:insert(DRef, {{Key, DL}});
do_add_event(#dbRef{orderRef = ORef, orderPolicy = ?not_DeadlineOrder,
		 discardRef = DRef, discardPolicy = ?not_LifoOrder,
		 defStopT = _DefStopT}, Event, Key, DL, ST, PO) ->
    ets:insert(ORef, {{DL, Key}, DL, ST, PO, Event}),
    ets:insert(DRef, {{Key, DL}});
%% Either the same (DeadlineOrder), RejectNewEvents or AnyOrder. No need
%% to store anything in the discard policy, i.e., if the same we'll just
%% read "from the other end" and AnyOrder and RejectNewEvents is equal.
do_add_event(#dbRef{orderRef = ORef, orderPolicy = ?not_DeadlineOrder,
		 defStopT = _DefStopT}, Event, Key, DL, ST, PO) ->
    ets:insert(ORef, {{DL, Key}, DL, ST, PO, Event});
    

do_add_event(#dbRef{orderRef = ORef, orderPolicy = ?not_FifoOrder,
		 discardRef = DRef, discardPolicy = ?not_DeadlineOrder,
		 defStopT = _DefStopT}, Event, Key, DL, ST, PO) ->
    ets:insert(ORef, {{Key, DL}, DL, ST, PO, Event}),
    ets:insert(DRef, {{DL, Key}});
do_add_event(#dbRef{orderRef = ORef, orderPolicy = ?not_FifoOrder,
		 discardRef = DRef, discardPolicy = ?not_PriorityOrder,
		 defPriority = DefPrio}, Event, Key, DL, ST, PO) ->
    Prio = extract_priority(Event, DefPrio, PO),
    ets:insert(ORef, {{Key, Prio}, DL, ST, PO, Event}),
    ets:insert(DRef, {{Prio, Key}});
%% The discard policy must RejectNewEvents, AnyOrder, Fifo or Lifo order.
do_add_event(#dbRef{orderRef = ORef, orderPolicy = ?not_FifoOrder,
		 discardRef = _DRef}, Event, Key, DL, ST, PO) ->
    ets:insert(ORef, {Key, DL, ST, PO, Event});

%% Order Policy must be AnyOrder or PriorityOrder.
do_add_event(#dbRef{orderRef = ORef,
		 discardRef = DRef, discardPolicy = ?not_DeadlineOrder,
		 defPriority = DefPrio, defStopT = _DefStopT}, Event, Key, DL, ST, PO) ->
    Prio = extract_priority(Event, DefPrio, PO),
    ets:insert(ORef, {{Prio, Key, DL}, DL, ST, PO, Event}),
    ets:insert(DRef, {{DL, Key, Prio}});
do_add_event(#dbRef{orderRef = ORef,
		 discardRef = DRef, discardPolicy = ?not_FifoOrder,
		 defPriority = DefPrio}, Event, Key, DL, ST, PO) ->
    Prio = extract_priority(Event, DefPrio, PO),
    ets:insert(ORef, {{Prio, Key}, DL, ST, PO, Event}),
    ets:insert(DRef, {{Key, Prio}});
    
do_add_event(#dbRef{orderRef = ORef,
		 discardRef = DRef, discardPolicy = ?not_LifoOrder,
		 defPriority = DefPrio}, Event, Key, DL, ST, PO) ->
    Prio = extract_priority(Event, DefPrio, PO),
    ets:insert(ORef, {{Prio, Key}, DL, ST, PO, Event}),
    ets:insert(DRef, {{Key, Prio}});

%% Order Policy must be AnyOrder or PriorityOrder and Discard Policy must be
%% AnyOrder or RejectNewEvents
do_add_event(#dbRef{orderRef = ORef, defPriority = DefPrio}, Event, Key, DL, ST, PO) ->
    Prio = extract_priority(Event, DefPrio, PO),
    ets:insert(ORef, {{Prio, Key}, DL, ST, PO, Event}).

%%------------------------------------------------------------
%% function : destroy_db
%% Arguments: A DB reference
%% Returns  : 
%%------------------------------------------------------------
destroy_db(#dbRef{orderRef = ORef, discardRef = undefined}) ->
    ets:delete(ORef);
destroy_db(#dbRef{orderRef = ORef, discardRef = DRef}) ->
    ets:delete(ORef),
    ets:delete(DRef).

%%------------------------------------------------------------
%% function : destroy_discard_db
%% Arguments: A DB reference
%% Returns  : 
%%------------------------------------------------------------
destroy_discard_db(#dbRef{discardRef = undefined}) ->
    ok;
destroy_discard_db(#dbRef{discardRef = DRef}) ->
    ets:delete(DRef).

%%------------------------------------------------------------
%% function : destroy_order_db
%% Arguments: A DB reference
%% Returns  : 
%%------------------------------------------------------------
destroy_order_db(#dbRef{orderRef = ORef}) ->
    ets:delete(ORef).

%%------------------------------------------------------------
%% function : create_db
%% Arguments: QoS (local representation).
%% Returns  : A DB reference
%%------------------------------------------------------------
create_db(QoS, GCTime, GCLimit, TimeRef) ->
    DiscardRef = 
	case {?not_GetDiscardPolicy(QoS), ?not_GetOrderPolicy(QoS)} of
	    {Equal, Equal} ->
		undefined;
	    {?not_PriorityOrder, ?not_AnyOrder} ->
		%% NOTE: Any- and Priority-Order delivery policy is equal.
		undefined;
	    {?not_RejectNewEvents, _} ->
		undefined;
	    {?not_AnyOrder, _} ->
		undefined;
	    {?not_LifoOrder, ?not_FifoOrder} ->
		undefined;
	    _ ->
		ets:new(oe_ets, [set, public, ordered_set])
	end,
    DBRef = ?CreateRef(ets:new(oe_ets, [set, public, ordered_set]), 
		       DiscardRef, 
		       ?not_GetOrderPolicy(QoS), ?not_GetDiscardPolicy(QoS), 
		       ?not_GetPriority(QoS), ?not_GetMaxEventsPerConsumer(QoS),
		       ?not_GetTimeout(QoS), ?not_GetStartTimeSupported(QoS),
		       ?not_GetStopTimeSupported(QoS), GCTime, GCLimit, TimeRef),
    if
	?is_TimeoutNotUsed(DBRef), ?is_StopTNotSupported(DBRef) ->	
	    ok;
	true ->
	    {M,S,U} = now(),
	    put(oe_GC_timestamp, {M,S+GCTime,U})
    end,
    DBRef.

%%------------------------------------------------------------
%% function : get_prio_mapping_value
%% Arguments: A MappingFilter reference | undefined
%%            Event (Any or Structured)
%% Returns  : undefined | Data 
%%------------------------------------------------------------
get_prio_mapping_value(DBRef, _, _) when ?get_DiscardP(DBRef) =/= ?not_PriorityOrder,
					 ?get_OrderP(DBRef) =/= ?not_AnyOrder,
					 ?get_OrderP(DBRef) =/= ?not_PriorityOrder ->
    false;
get_prio_mapping_value(_, undefined, _) ->
    undefined;
get_prio_mapping_value(_, MFilter, Event) when is_record(Event, 'any') ->
    case catch 'CosNotifyFilter_MappingFilter':match(MFilter, Event) of
	{false, DefVal} when is_record(DefVal, 'any') ->
	    any:get_value(DefVal);
	{true, Matched} when is_record(Matched, 'any') ->
	    any:get_value(Matched);
	_ ->
	    undefined
    end;
get_prio_mapping_value(_, MFilter, Event) ->
    case catch 'CosNotifyFilter_MappingFilter':match_structured(MFilter, Event) of
	{false, DefVal} when is_record(DefVal, 'any') ->
	    any:get_value(DefVal);
	{true, Matched} when is_record(Matched, 'any') ->
	    any:get_value(Matched);
	_ ->
	    undefined
    end.

%%------------------------------------------------------------
%% function : get_life_mapping_value
%% Arguments: A MappingFilter reference | undefined
%%            Event (Any or Structured)
%% Returns  : undefined | Data 
%%------------------------------------------------------------
get_life_mapping_value(DBRef, _, _) when ?get_DiscardP(DBRef) =/= ?not_DeadlineOrder,
					 ?get_OrderP(DBRef) =/= ?not_DeadlineOrder,
					 ?is_StopTNotSupported(DBRef) ->
    false;
get_life_mapping_value(_, undefined, _) ->
    undefined;
get_life_mapping_value(_, MFilter, Event) when is_record(Event, 'any') ->
    case catch 'CosNotifyFilter_MappingFilter':match(MFilter, Event) of
	{false, DefVal} when is_record(DefVal, 'any') ->
	    any:get_value(DefVal);
	{true, Matched} when is_record(Matched, 'any') ->
	    any:get_value(Matched);
	_ ->
	    undefined
    end;
get_life_mapping_value(_, MFilter, Event) ->
    case catch 'CosNotifyFilter_MappingFilter':match_structured(MFilter, Event) of
	{false, DefVal} when is_record(DefVal, 'any') ->
	    any:get_value(DefVal);
	{true, Matched} when is_record(Matched, 'any') ->
	    any:get_value(Matched);
	_ ->
	    undefined
    end.

%%------------------------------------------------------------
%% function : validate_event
%% Arguments: Subscribe data
%%            A sequence of Events, 'structured' or an 'any' record
%%            A list of filter references
%%            Status, i.e., do we have to filter the events or just check subscr.
%% Returns  : A tuple of two lists; list1 the events that passed 
%%            and list2 the events that didn't pass.
%%------------------------------------------------------------
validate_event(true, Events, Filters, _, 'MATCH') ->
    filter_events(Events, Filters, false);
validate_event(true, Events, _Filters, _, _) ->
    {Events, []};
validate_event({_Which, _WC}, Event, Filters, _, 'MATCH') when is_record(Event, any) ->
    filter_events(Event, Filters, false);
validate_event({_Which, _WC}, Event, _Filters, _, _) when is_record(Event, any) ->
    {Event, []};
validate_event({Which, WC}, Events, Filters, DBRef, 'MATCH')  ->
    Passed=validate_event2(DBRef, Events, Which, WC, []),
    filter_events(Passed, Filters, true);
validate_event({Which, WC}, Events, _Filters, DBRef, _)  ->
    Passed=validate_event2(DBRef, Events, Which, WC, []),
    {lists:reverse(Passed), []}.

validate_event2(_, [], _, _, []) ->
    [];
validate_event2(_, [], _, _, Acc) ->
    Acc;
validate_event2(DBRef, [Event|T], Which, WC, Acc) ->
    ET = ((Event#'CosNotification_StructuredEvent'.header)
	  #'CosNotification_EventHeader'.fixed_header)
	#'CosNotification_FixedEventHeader'.event_type,
    CheckList = 
	case Which of
	    both ->
		[ET];
	    domain ->
		[ET, 
		 ET#'CosNotification_EventType'{type_name=""},
		 ET#'CosNotification_EventType'{type_name="*"}];
	    type ->
		[ET, 
		 ET#'CosNotification_EventType'{domain_name=""},
		 ET#'CosNotification_EventType'{domain_name="*"}];
	    _ ->
		[ET,
		 ET#'CosNotification_EventType'{type_name=""},
		 ET#'CosNotification_EventType'{type_name="*"},
		 ET#'CosNotification_EventType'{domain_name=""},
		 ET#'CosNotification_EventType'{domain_name="*"}]
	end,
    case check_subscription(DBRef, CheckList) of
	true ->
	    validate_event2(DBRef, T, Which, WC, [Event|Acc]);
	_->
	    case catch cosNotification_Filter:match_types(
			 ET#'CosNotification_EventType'.domain_name,
			 ET#'CosNotification_EventType'.type_name, 
			 WC) of
		true ->
		    validate_event2(DBRef, T, Which, WC, [Event|Acc]);
		_->
		    validate_event2(DBRef, T, Which, WC, Acc)
	    end
    end.

check_subscription(_, []) ->
    false;
check_subscription(DBRef, [H|T]) ->
    case ets:lookup(DBRef, H) of
	[] ->
	    check_subscription(DBRef, T);
	_ ->
	    true
    end.
    

%%------------------------------------------------------------
%% function : filter_events
%% Arguments: A sequence of structured Events or #any
%% Returns  : A tuple of two lists; list1 the events that passed 
%%            and list2 the events that didn't pass.
%%------------------------------------------------------------

filter_events(Events, []) ->
    {Events, []};
filter_events(Events, Filters) ->
    filter_events(Events, Filters, [], [], false).

filter_events(Events, [], false) ->
    {Events, []};
filter_events(Events, [], _) ->
    {lists:reverse(Events), []};
filter_events(Events, Filters, Reversed) ->
    filter_events(Events, Filters, [], [], Reversed).

filter_events([], _, AccPassed, AccFailed, false) ->
    {lists:reverse(AccPassed), lists:reverse(AccFailed)};
filter_events([], _, AccPassed, AccFailed, _) ->
    {AccPassed, AccFailed};
filter_events([H|T], Filters, AccPassed, AccFailed, Reversed) ->
    case call_filters(Filters, H) of
	true ->
	    filter_events(T, Filters, [H|AccPassed], AccFailed, Reversed);
	_ ->
	    filter_events(T, Filters, AccPassed, [H|AccFailed], Reversed)
    end;
filter_events(Any, Filters, _AccPassed, _AccFailed, _Reversed) ->
    case call_filters(Filters, Any) of
	true ->
	    {Any, []};
	_ ->
	    {[], Any}
    end.

call_filters([], _) ->
    false;
call_filters([{_,H}|T], Event) when is_record(Event, any) ->
    case catch 'CosNotifyFilter_Filter':match(H, Event) of
	true ->
	    true;
	_->
	    call_filters(T, Event)
    end;
call_filters([{_,H}|T], Event) when ?not_isConvertedAny(Event) ->
    case catch 'CosNotifyFilter_Filter':match(H, 
					      Event#'CosNotification_StructuredEvent'.remainder_of_body) of
	true ->
	    true;
	_->
	    call_filters(T, Event)
    end;
call_filters([{_,H}|T], Event) ->
    case catch 'CosNotifyFilter_Filter':match_structured(H, Event) of
	true ->
	    true;
	_->
	    call_filters(T, Event)
    end.


%%--------------- END OF MODULE ------------------------------
