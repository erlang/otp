%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
%% File    : PusherConsumer_impl.erl
%% Purpose : 
%%----------------------------------------------------------------------

-module('PusherConsumer_impl').

%%--------------- INCLUDES -----------------------------------
-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").
%% cosEvent files.
-include_lib("cosEvent/include/CosEventChannelAdmin.hrl").
-include_lib("cosEvent/include/CosEventComm.hrl").
%% Application files
-include("CosNotification.hrl").
-include("CosNotifyChannelAdmin.hrl").
-include("CosNotifyComm.hrl").
-include("CosNotifyFilter.hrl").

-include("CosNotification_Definitions.hrl").

%%--------------- EXPORTS ------------------------------------
%%--------------- External -----------------------------------
%%----- CosNotifyChannelAdmin::ProxyPushConsumer -------------
-export([connect_any_push_supplier/4]).

%%----- CosNotifyChannelAdmin::SequenceProxyPushConsumer -----
-export([connect_sequence_push_supplier/4]).

%%----- CosNotifyChannelAdmin::StructuredProxyPushConsumer ---
-export([connect_structured_push_supplier/4]).

%%----- Inherit from CosNotifyChannelAdmin::ProxyConsumer ----
-export([obtain_subscription_types/4,
	 validate_event_qos/4]).

%%----- Inherit from CosNotification::QoSAdmin ---------------
-export([get_qos/3,
	 set_qos/4,
	 validate_qos/4]).

%%----- Inherit from CosNotifyComm::NotifyPublish ------------
-export([offer_change/5]).

%%----- Inherit from CosNotifyFilter::FilterAdmin ------------
-export([add_filter/4, 
	 remove_filter/4, 
	 get_filter/4,
	 get_all_filters/3, 
	 remove_all_filters/3]).

%%----- Inherit from CosEventComm::PushConsumer -------------
-export([push/4,
	 disconnect_push_consumer/3]).

%%----- Inherit from CosNotifyComm::SequencePushConsumer ----
-export([push_structured_events/4,
	 disconnect_sequence_push_consumer/3]).

%%----- Inherit from CosNotifyComm::StructuredPushConsumer --
-export([push_structured_event/4,
	 disconnect_structured_push_consumer/3]).

%%----- Inherit from CosEventChannelAdmin::ProxyPushConsumer
-export([connect_push_supplier/4]).

%% Attributes (external) CosNotifyChannelAdmin::ProxySupplier
-export(['_get_MyType'/3, 
	 '_get_MyAdmin'/3]).

%%--------------- gen_server specific exports ----------------
-export([handle_info/2, code_change/3]).
-export([init/1, terminate/2]).

%%--------------- LOCAL DEFINITIONS --------------------------
%% Data structures
-record(state, {myType,
	        myAdmin,
	        myAdminPid,
		myChannel,
		myFilters = [],
		myOperator,
		idCounter = 0,
		client,
		qosGlobal,
		qosLocal,
		publishType = false,
		etsR,
		eventDB}).

%% Data structures constructors
-define(get_InitState(_MyT, _MyA, _MyAP, _QS, _LQS, _Ch, _EDB, _MyOP), 
	#state{myType    = _MyT,
	       myAdmin   = _MyA,
	       myAdminPid= _MyAP,
	       myChannel = _Ch,
	       myOperator= _MyOP,
	       qosGlobal = _QS,
	       qosLocal  = _LQS,
	       etsR      = ets:new(oe_ets, [set, protected]),
	       eventDB   = _EDB}).

%%-------------- Data structures selectors -----------------
%% Attributes
-define(get_MyType(S),           S#state.myType).
-define(get_MyAdmin(S),          S#state.myAdmin).
-define(get_MyAdminPid(S),       S#state.myAdminPid).
-define(get_MyChannel(S),        S#state.myChannel).
-define(get_MyOperator(S),       S#state.myOperator).
%% Client Object
-define(get_Client(S),           S#state.client).
%% QoS
-define(get_GlobalQoS(S),        S#state.qosGlobal).
-define(get_LocalQoS(S),         S#state.qosLocal).
-define(get_BothQoS(S),          {S#state.qosGlobal, S#state.qosLocal}).
%% Filters
-define(get_Filter(S, I),        find_obj(lists:keysearch(I, 1, S#state.myFilters))).
-define(get_AllFilter(S),        S#state.myFilters).
-define(get_AllFilterID(S),      find_ids(S#state.myFilters)).
%% Publish
-define(get_AllPublish(S),       lists:flatten(ets:match(S#state.etsR,
							 {'$1',publish}))).
-define(get_PublishType(S),      S#state.publishType).
%% ID
-define(get_IdCounter(S),        S#state.idCounter).
%% Event
-define(get_Event(S),            cosNotification_eventDB:get_event(S#state.eventDB)).
-define(get_Events(S,M),         cosNotification_eventDB:get_events(S#state.eventDB, M)).

-define(get_EventCounter(S),     S#state.eventCounter).
%% Admin
-define(get_BatchLimit(S),       ?not_GetMaximumBatchSize((S#state.qosLocal))).

%%-------------- Data structures modifiers -----------------
%% Client Object
-define(set_Client(S,D),         S#state{client=D}).
-define(del_Client(S),           S#state{client=undefined}).
-define(set_Unconnected(S),      S#state{client=undefined}).
%% QoS
-define(set_LocalQoS(S,D),       S#state{qosLocal=D}).
-define(set_GlobalQoS(S,D),      S#state{qosGlobal=D}).
-define(set_BothQoS(S,GD,LD),    S#state{qosGlobal=GD, qosLocal=LD}).
%% Filters
-define(add_Filter(S,I,O),       S#state{myFilters=[{I,O}|S#state.myFilters]}).
-define(del_Filter(S,I),         S#state{myFilters=
					 delete_obj(lists:keydelete(I, 1, S#state.myFilters),
						    S#state.myFilters)}).
-define(del_AllFilter(S),        S#state{myFilters=[]}).
%% Publish
-define(add_Publish(S,E),        ets:insert(S#state.etsR, {E, publish})).
-define(del_Publish(S,E),        ets:delete(S#state.etsR, E)).
-define(set_PublishType(S,T),    S#state{publishType=T}).
%% ID
-define(set_IdCounter(S,V),      S#state{idCounter=V}).
-define(new_Id(S),               'CosNotification_Common':create_id(S#state.idCounter)).
%% Event
-define(add_Event(S,E),          cosNotification_eventDB:add_event(S#state.eventDB, E)).
-define(update_EventDB(S,Q),     S#state{eventDB=
					 cosNotification_eventDB:update(S#state.eventDB, Q)}).

-define(set_EventCounter(S,V),   S#state{eventCounter=V}).
-define(add_to_EventCounter(S,V),S#state{eventCounter=S#state.eventCounter+V}).
-define(reset_EventCounter(S),   S#state{eventCounter=0}).
-define(increase_EventCounter(S),S#state{eventCounter=(S#state.eventCounter+1)}).
-define(decrease_EventCounter(S),S#state{eventCounter=S#state.eventCounter-1}).
-define(add_ToEventCounter(S,A), S#state{eventCounter=(S#state.eventCounter+A)}).
-define(sub_FromEventCounter(S,_A), S#state{eventCounter=(S#state.eventCounter-_A)}).
-define(set_EventCounterTo(S,V), S#state{eventCounter=V}).

%% MISC
-define(is_ANY(S),               S#state.myType == 'PUSH_ANY').
-define(is_STRUCTURED(S),        S#state.myType == 'PUSH_STRUCTURED').
-define(is_SEQUENCE(S),          S#state.myType == 'PUSH_SEQUENCE').
-define(is_ANDOP(S),             S#state.myOperator == 'AND_OP').
-define(is_UnConnected(S),       S#state.client == undefined).
-define(is_Connected(S),         S#state.client =/= undefined).
-define(is_PersistentConnection(S),
	?not_GetConnectionReliability((S#state.qosLocal)) == ?not_Persistent).
-define(is_PersistentEvent(S),
	?not_GetEventReliability((S#state.qosLocal)) == ?not_Persistent).
-define(is_BatchLimitReached(S), S#state.eventCounter >= 
                                 ?not_GetMaximumBatchSize((S#state.qosLocal))).


%%----------------------------------------------------------%
%% function : handle_info, code_change
%% Arguments: 
%% Returns  : 
%% Effect   : Functions demanded by the gen_server module. 
%%-----------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(Info, State) ->
    ?DBG("INFO: ~p~n", [Info]),
    case Info of
        {'EXIT', Pid, Reason} when ?get_MyAdminPid(State)==Pid->
            ?DBG("PARENT ADMIN: ~p  TERMINATED.~n",[Reason]),
	    {stop, Reason, State};
        {'EXIT', _Pid, _Reason} ->
            ?DBG("PROXYPUSHCONSUMER: ~p  TERMINATED.~n",[_Reason]),
            {noreply, State};
        _ ->
            {noreply, State}
    end.

%%----------------------------------------------------------%
%% function : init, terminate
%% Arguments: 
%%-----------------------------------------------------------

init(['PUSH_SEQUENCE', MyAdmin, MyAdminPid, InitQoS, LQS, 
      MyChannel, Options, Operator]) ->
    process_flag(trap_exit, true),
    %% Only if MyType is 'PUSH_SEQUENCE' we need an ets to store events in.
    %% Otherwise we'll forward them at once. Why? We don't know when the next event
    %% is due.
    GCTime = 'CosNotification_Common':get_option(gcTime, Options, 
						 ?not_DEFAULT_SETTINGS),
    GCLimit = 'CosNotification_Common':get_option(gcLimit, Options, 
						  ?not_DEFAULT_SETTINGS),
    TimeRef = 'CosNotification_Common':get_option(timeService, Options, 
						  ?not_DEFAULT_SETTINGS),
    {ok, ?get_InitState('PUSH_SEQUENCE', MyAdmin, MyAdminPid, InitQoS, LQS, MyChannel,
			cosNotification_eventDB:create_db(LQS, GCTime, GCLimit, TimeRef), 
			Operator)};
init([MyType, MyAdmin, MyAdminPid, InitQoS, LQS, MyChannel, _Options, Operator]) ->
    process_flag(trap_exit, true),
    {ok, ?get_InitState(MyType, MyAdmin, MyAdminPid, InitQoS, LQS, MyChannel, 
			undefined, Operator)}.

terminate(_Reason, State) when ?is_UnConnected(State) ->
    ok;
terminate(_Reason, State) ->
    Client = ?get_Client(State),
    case catch corba_object:is_nil(Client) of
	false when ?is_ANY(State) ->
	    'CosNotification_Common':disconnect('CosEventComm_PushSupplier', 
						disconnect_push_supplier,
						Client);
	false when ?is_SEQUENCE(State) ->
	    'CosNotification_Common':disconnect('CosNotifyComm_SequencePushSupplier',
						disconnect_sequence_push_supplier,
						Client);
	false when ?is_STRUCTURED(State) ->
	    'CosNotification_Common':disconnect('CosNotifyComm_StructuredPushSupplier',
						disconnect_structured_push_supplier,
						Client);
	_ ->
	    ok
    end.

%%-----------------------------------------------------------
%%----- CosNotifyChannelAdmin_ProxyConsumer attributes ------
%%-----------------------------------------------------------
%%----------------------------------------------------------%
%% Attribute: '_get_MyType'
%% Type     : readonly
%% Returns  : 
%%-----------------------------------------------------------
'_get_MyType'(_OE_THIS, _OE_FROM, State) ->
    {reply, ?get_MyType(State), State}.

%%----------------------------------------------------------%
%% Attribute: '_get_MyAdmin'
%% Type     : readonly
%% Returns  : 
%%-----------------------------------------------------------
'_get_MyAdmin'(_OE_THIS, _OE_FROM, State) ->
    {reply, ?get_MyAdmin(State), State}.

%%-----------------------------------------------------------
%%------- Exported external functions -----------------------
%%-----------------------------------------------------------
%%----- CosEventChannelAdmin::ProxyPushConsumer -------------
%%----------------------------------------------------------%
%% function : connect_push_supplier
%% Arguments: Client - CosEventComm::PushSupplier
%% Returns  :  ok | {'EXCEPTION', #'AlreadyConnected'{}} |
%%            {'EXCEPTION', #'TypeError'{}}
%%            Both exceptions from CosEventChannelAdmin!!!
%%-----------------------------------------------------------
connect_push_supplier(OE_THIS, OE_FROM, State, Client) ->
    connect_any_push_supplier(OE_THIS, OE_FROM, State, Client).

%%----- CosNotifyChannelAdmin::ProxyPushConsumer ------------
%%----------------------------------------------------------%
%% function : connect_any_push_supplier
%% Arguments: Client - CosEventComm::PushSupplier
%% Returns  :  ok | {'EXCEPTION', #'AlreadyConnected'{}} |
%%            {'EXCEPTION', #'TypeError'{}}
%%            Both exceptions from CosEventChannelAdmin!!!
%%-----------------------------------------------------------
connect_any_push_supplier(_OE_THIS, _OE_FROM, State, Client) when ?is_ANY(State) ->
    ?not_TypeCheck(Client, 'CosEventComm_PushSupplier'),
    if
	?is_Connected(State) ->
	    corba:raise(#'CosEventChannelAdmin_AlreadyConnected'{});
	true ->
	    {reply, ok, ?set_Client(State, Client)}
    end;
connect_any_push_supplier(_, _, _, _) ->
    corba:raise(#'BAD_OPERATION'{completion_status=?COMPLETED_NO}).

%%----- CosNotifyChannelAdmin::SequenceProxyPushConsumer ----
%%----------------------------------------------------------%
%% function : connect_sequence_push_supplier
%% Arguments: Client - CosNotifyComm::SequencePushSupplier
%% Returns  :  ok | {'EXCEPTION', #'AlreadyConnected'{}} |
%%            {'EXCEPTION', #'TypeError'{}}
%%-----------------------------------------------------------
connect_sequence_push_supplier(_OE_THIS, _OE_FROM, State, Client) when ?is_SEQUENCE(State) ->
    ?not_TypeCheck(Client, 'CosNotifyComm_SequencePushSupplier'),
     if
	?is_Connected(State) ->
	    corba:raise(#'CosEventChannelAdmin_AlreadyConnected'{});
	true ->
	    {reply, ok, ?set_Client(State, Client)}
    end;
connect_sequence_push_supplier(_, _, _, _) ->
    corba:raise(#'BAD_OPERATION'{completion_status=?COMPLETED_NO}).

%%----- CosNotifyChannelAdmin::StructuredProxyPushConsumer --
%%----------------------------------------------------------%
%% function : connect_structured_push_supplier
%% Arguments: Client - CosNotifyComm::StructuredPushSupplier
%% Returns  :  ok | {'EXCEPTION', #'AlreadyConnected'{}} |
%%            {'EXCEPTION', #'TypeError'{}}
%%-----------------------------------------------------------
connect_structured_push_supplier(_OE_THIS, _OE_FROM, State, Client) when ?is_STRUCTURED(State) ->
    ?not_TypeCheck(Client, 'CosNotifyComm_StructuredPushSupplier'),
     if
	?is_Connected(State) ->
	    corba:raise(#'CosEventChannelAdmin_AlreadyConnected'{});
	true ->
	    {reply, ok, ?set_Client(State, Client)}
    end;
connect_structured_push_supplier(_, _, _, _) ->
    corba:raise(#'BAD_OPERATION'{completion_status=?COMPLETED_NO}).

%%----- Inherit from CosNotifyChannelAdmin::ProxyConsumer ---
%%----------------------------------------------------------%
%% function : obtain_subscription_types
%% Arguments: Mode - enum 'ObtainInfoMode' (CosNotifyChannelAdmin)
%% Returns  : CosNotification::EventTypeSeq
%%-----------------------------------------------------------
obtain_subscription_types(_OE_THIS, _OE_FROM, State, 'ALL_NOW_UPDATES_OFF') ->
    {reply, ?get_AllPublish(State), ?set_PublishType(State, false)};
obtain_subscription_types(_OE_THIS, _OE_FROM, State, 'ALL_NOW_UPDATES_ON') ->
    {reply, ?get_AllPublish(State), ?set_PublishType(State, true)};
obtain_subscription_types(_OE_THIS, _OE_FROM, State, 'NONE_NOW_UPDATES_OFF') ->
    {reply, [], ?set_PublishType(State, false)};
obtain_subscription_types(_OE_THIS, _OE_FROM, State, 'NONE_NOW_UPDATES_ON') ->
    {reply, [], ?set_PublishType(State, true)};
obtain_subscription_types(_,_,_,What) ->
    orber:dbg("[~p] PusherConsumer:obtain_subscription_types(~p);~n"
	      "Incorrect enumerant", [?LINE, What], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%----------------------------------------------------------%
%% function : validate_event_qos
%% Arguments: RequiredQoS - CosNotification::QoSProperties
%% Returns  : ok | {'EXCEPTION', #'UnsupportedQoS'{}}
%%            AvilableQoS - CosNotification::NamedPropertyRangeSeq (out)
%%-----------------------------------------------------------
validate_event_qos(_OE_THIS, _OE_FROM, State, RequiredQoS) ->
    AvilableQoS = 'CosNotification_Common':validate_event_qos(RequiredQoS,
							      ?get_LocalQoS(State)),
    {reply, {ok, AvilableQoS}, State}.

%%----- Inherit from CosNotification::QoSAdmin --------------
%%----------------------------------------------------------%
%% function : get_qos
%% Arguments: 
%% Returns  : 
%%-----------------------------------------------------------
get_qos(_OE_THIS, _OE_FROM, State) ->
    {reply, ?get_GlobalQoS(State), State}.    

%%----------------------------------------------------------%
%% function : set_qos
%% Arguments: QoS - CosNotification::QoSProperties, i.e.,
%%            [#'Property'{name, value}, ...] where name eq. string()
%%            and value eq. any().
%% Returns  : ok | {'EXCEPTION', CosNotification::UnsupportedQoS}
%%-----------------------------------------------------------
set_qos(_OE_THIS, _OE_FROM, State, QoS) ->
    {NewQoS, LQS} = 'CosNotification_Common':set_qos(QoS, ?get_BothQoS(State), 
						     proxy, ?get_MyAdmin(State), 
						     false),
    NewState = ?update_EventDB(State, LQS),
    {reply, ok, ?set_BothQoS(NewState, NewQoS, LQS)}.

%%----------------------------------------------------------%
%% function : validate_qos
%% Arguments: Required_qos - CosNotification::QoSProperties
%%            [#'Property'{name, value}, ...] where name eq. string()
%%            and value eq. any().
%% Returns  : {'EXCEPTION', CosNotification::UnsupportedQoS}
%%            {ok, CosNotification::NamedPropertyRangeSeq}
%%-----------------------------------------------------------
validate_qos(_OE_THIS, _OE_FROM, State, Required_qos) ->
    QoS = 'CosNotification_Common':validate_qos(Required_qos, ?get_BothQoS(State), 
						proxy, ?get_MyAdmin(State), 
						false),
    {reply, {ok, QoS}, State}.

%%----- Inherit from CosNotifyComm::NotifyPublish -----------
%%----------------------------------------------------------%
%% function : offer_change
%% Arguments: Added - #'CosNotification_EventType'{}
%%            Removed - #'CosNotification_EventType'{}
%% Returns  : ok | 
%%            {'EXCEPTION', #'CosNotifyComm_InvalidEventType'{}}
%%-----------------------------------------------------------
offer_change(_OE_THIS, _OE_FROM, State, Added, Removed) ->
    cosNotification_Filter:validate_types(Added), 
    cosNotification_Filter:validate_types(Removed),
    %% On this "side" we don't really care about which
    %% type of events the client will supply. 
    %% Perhaps, later on, if we want to check this against Filters
    %% associated with this object we may change this approach, i.e., if
    %% the filter will not allow passing certain event types. But the
    %% user should see to that that situation never occurs. It would add
    %% extra overhead. Also see PusherSupplier- and PullerSuppler-
    %% 'subscription_change'.
    update_publish(add, State, Added),
    update_publish(remove, State, Removed),
    case ?get_PublishType(State) of
	true ->
	    %% Perhaps we should handle exception here?!
	    %% Probably not. Better to stay "on-line".
	    catch 'CosNotifyComm_NotifySubscribe':
			subscription_change(?get_Client(State), Added, Removed),
	    ok;
	_->
	    ok
    end,	
    {reply, ok, State}.

update_publish(_, _, [])->
    ok;
update_publish(add, State, [H|T]) ->
    ?add_Publish(State, H),
    update_publish(add, State, T);
update_publish(remove, State, [H|T]) ->
    ?del_Publish(State, H),
    update_publish(remove, State, T).
    
%%----- Inherit from CosNotifyFilter::FilterAdmin -----------
%%----------------------------------------------------------%
%% function : add_filter
%% Arguments: Filter - CosNotifyFilter::Filter
%% Returns  : FilterID - long
%%-----------------------------------------------------------
add_filter(_OE_THIS, _OE_FROM, State, Filter) ->
    'CosNotification_Common':type_check(Filter, 'CosNotifyFilter_Filter'),
    FilterID = ?new_Id(State),
    NewState = ?set_IdCounter(State, FilterID),
    {reply, FilterID, ?add_Filter(NewState, FilterID, Filter)}.

%%----------------------------------------------------------%
%% function : remove_filter
%% Arguments: FilterID - long
%% Returns  : ok
%%-----------------------------------------------------------
remove_filter(_OE_THIS, _OE_FROM, State, FilterID) when is_integer(FilterID) ->
    {reply, ok, ?del_Filter(State, FilterID)};
remove_filter(_,_,_,What) ->
    orber:dbg("[~p] PusherConsumer:remove_filter(~p); Not an integer", 
	      [?LINE, What], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%----------------------------------------------------------%
%% function : get_filter
%% Arguments: FilterID - long
%% Returns  : Filter - CosNotifyFilter::Filter |
%%            {'EXCEPTION', #'CosNotifyFilter_FilterNotFound'{}}
%%-----------------------------------------------------------
get_filter(_OE_THIS, _OE_FROM, State, FilterID) when is_integer(FilterID) ->
    {reply, ?get_Filter(State, FilterID), State};
get_filter(_,_,_,What) ->
    orber:dbg("[~p] PusherConsumer:get_filter(~p); Not an integer", 
	      [?LINE, What], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%----------------------------------------------------------%
%% function : get_all_filters
%% Arguments: -
%% Returns  : Filter - CosNotifyFilter::FilterIDSeq
%%-----------------------------------------------------------
get_all_filters(_OE_THIS, _OE_FROM, State) ->
    {reply, ?get_AllFilterID(State), State}.

%%----------------------------------------------------------%
%% function : remove_all_filters
%% Arguments: -
%% Returns  : ok
%%-----------------------------------------------------------
remove_all_filters(_OE_THIS, _OE_FROM, State) ->
    {reply, ok, ?del_AllFilter(State)}.

%%----- Inherit from CosEventComm::PushConsumer -------------
%%----------------------------------------------------------%
%% function : disconnect_push_consumer
%% Arguments: -
%% Returns  : ok
%%-----------------------------------------------------------
disconnect_push_consumer(_OE_THIS, _OE_FROM, State) ->
    {stop, normal, ok, ?set_Unconnected(State)}.

%%----------------------------------------------------------%
%% function : push
%% Arguments: AnyEvent
%% Returns  : ok | 
%%-----------------------------------------------------------
push(OE_THIS, OE_FROM, State, Event) when ?is_ANY(State) ->
    corba:reply(OE_FROM, ok),
    case {?not_isConvertedStructured(Event),
	  cosNotification_eventDB:filter_events([Event], ?get_AllFilter(State))} of
	{_, {[],_}} when ?is_ANDOP(State) ->
	    {noreply, State};
	{true, {[],[_]}} ->
	    %% Is OR and converted, change back and forward to Admin
	    forward(seq, ?get_MyAdmin(State), State, [any:get_value(Event)], 
		    'MATCH', OE_THIS);
	{_, {[],[_]}} ->
	    %% Is OR and not converted, forward to Admin
	    forward(any, ?get_MyAdmin(State), State, Event, 'MATCH', OE_THIS);
	{true, {[_],_}} when ?is_ANDOP(State) ->
	    %% Is AND and converted, change back and forward to Admin
	    forward(seq, ?get_MyAdmin(State), State, [any:get_value(Event)], 
		    'MATCH', OE_THIS);
	{true, {[_],_}} ->
	    %% Is OR and converted, change back and forward to Channel
	    forward(seq, ?get_MyChannel(State), State, [any:get_value(Event)], 
		    'MATCHED', OE_THIS);
	{_, {[_],_}} when ?is_ANDOP(State) ->
	    %% Is AND and not converted, forward to Admin
	    forward(any, ?get_MyAdmin(State), State, Event, 'MATCH', OE_THIS);
	_ ->
	    %% Is OR and not converted, forward to Channel
	    forward(any, ?get_MyChannel(State), State, Event, 'MATCHED', OE_THIS)
    end;
push(_, _, _, _) ->
    corba:raise(#'BAD_OPERATION'{completion_status=?COMPLETED_NO}).

%%----- Inherit from CosNotifyComm::SequencePushConsumer ----
%%----------------------------------------------------------%
%% function : disconnect_sequence_push_consumer
%% Arguments: -
%% Returns  : ok
%%-----------------------------------------------------------
disconnect_sequence_push_consumer(_OE_THIS, _OE_FROM, State) ->
    {stop, normal, ok, ?set_Unconnected(State)}.

%%----------------------------------------------------------%
%% function : push_structured_events
%% Arguments: CosNotification::EventBatch
%% Returns  : ok | 
%%-----------------------------------------------------------
push_structured_events(OE_THIS, OE_FROM, State, Events) when ?is_SEQUENCE(State) ->
    corba:reply(OE_FROM, ok),
    %% We cannot convert parts of the sequence to any, event though they
    %% are converted from any to structured. Would be 'impossible' to send.
    case cosNotification_eventDB:filter_events(Events, ?get_AllFilter(State)) of
	{[],_} when ?is_ANDOP(State) ->
	    {noreply, State};
	{[],Failed} ->
	    forward(seq, ?get_MyAdmin(State), State, Failed, 'MATCH', OE_THIS);
	{Passed, _} when ?is_ANDOP(State) ->
	    forward(seq, ?get_MyAdmin(State), State, Passed, 'MATCH', OE_THIS);
	{Passed, []} ->
	    forward(seq, ?get_MyChannel(State), State, Passed, 'MATCHED', OE_THIS);
	{Passed, Failed} ->
	    %% Is OR, send Passed to channel and Failed to Admin.
	    forward(seq, ?get_MyChannel(State), State, Passed, 'MATCHED', OE_THIS),
	    forward(seq, ?get_MyAdmin(State), State, Failed, 'MATCH', OE_THIS)
    end;
push_structured_events(_,_,_,_) ->
    corba:raise(#'BAD_OPERATION'{completion_status=?COMPLETED_NO}).

%%----- Inherit from CosNotifyComm::StructuredPushConsumer --
%%----------------------------------------------------------%
%% function : disconnect_structured_push_consumer
%% Arguments: -
%% Returns  : ok
%%-----------------------------------------------------------
disconnect_structured_push_consumer(_OE_THIS, _OE_FROM, State) ->
    {stop, normal, ok, ?set_Unconnected(State)}.

%%----------------------------------------------------------%
%% function : push_structured_event
%% Arguments: CosNotification::StructuredEvent
%% Returns  : ok | 
%%-----------------------------------------------------------
push_structured_event(OE_THIS, OE_FROM, State, Event) when ?is_STRUCTURED(State) ->
    corba:reply(OE_FROM, ok),
    case {?not_isConvertedAny(Event),
	  cosNotification_eventDB:filter_events([Event], ?get_AllFilter(State))} of
	{_, {[],_}} when ?is_ANDOP(State) ->
	    {noreply, State};
	{true, {[],[_]}} ->
	    %% Is OR and converted, change back and forward to Admin
	    forward(any, ?get_MyAdmin(State), State, 
		    Event#'CosNotification_StructuredEvent'.remainder_of_body, 
		    'MATCH', OE_THIS);
	{_, {[],[_]}} ->
	    %% Is OR and not converted, forward to Admin
	    forward(seq, ?get_MyAdmin(State), State, [Event], 'MATCH', OE_THIS);
	{true, {[_],_}} when ?is_ANDOP(State) ->
	    %% Is AND and converted, change back and forward to Admin
	    forward(any, ?get_MyAdmin(State), State, 
		    Event#'CosNotification_StructuredEvent'.remainder_of_body, 
		    'MATCH', OE_THIS);
	{true, {[_],_}} ->
	    %% Is OR and converted, change back and forward to Channel
	    forward(any, ?get_MyChannel(State), State, 
		    Event#'CosNotification_StructuredEvent'.remainder_of_body, 
		    'MATCHED', OE_THIS);
	{_, {[_],_}} when ?is_ANDOP(State) ->
	    %% Is AND and not converted, forward to Admin
	    forward(seq, ?get_MyAdmin(State), State, [Event], 'MATCH', OE_THIS);
	_ ->
	    %% Is OR and not converted, forward to Channel
	    forward(seq, ?get_MyChannel(State), State, [Event], 'MATCHED', OE_THIS)
    end;
push_structured_event(_,_,_,_) ->
    corba:raise(#'BAD_OPERATION'{completion_status=?COMPLETED_NO}).

%%--------------- LOCAL FUNCTIONS ----------------------------
find_obj({value, {_, Obj}}) -> Obj;
find_obj(_) -> {'EXCEPTION', #'CosNotifyFilter_FilterNotFound'{}}.

find_ids(List) ->           find_ids(List, []).
find_ids([], Acc) ->        Acc;
find_ids([{I,_}|T], Acc) -> find_ids(T, [I|Acc]);
find_ids(What, _) ->
    orber:dbg("[~p] PusherConsumer:find_ids();~n"
	      "Id corrupt: ~p", [?LINE, What], ?DEBUG_LEVEL),
    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO}).

%% Delete a single object.
%% The list don not differ, i.e., no filter removed, raise exception.
delete_obj(List,List) -> corba:raise(#'CosNotifyFilter_FilterNotFound'{});
delete_obj(List,_) -> List.

%% Forward events
forward(any, SendTo, State, Event, Status, OE_THIS) ->
    case catch oe_CosNotificationComm_Event:callAny(SendTo, Event, Status) of
	ok ->
	    ?DBG("PROXY FORWARD ANY: ~p~n",[Event]),
	    {noreply, State};
	{'EXCEPTION', E} when is_record(E, 'OBJECT_NOT_EXIST') orelse
			      is_record(E, 'NO_PERMISSION') orelse
			      is_record(E, 'CosEventComm_Disconnected') ->
	    orber:dbg("[~p] PusherConsumer:forward();~n"
		      "Admin/Channel no longer exists; terminating and dropping: ~p", 
		      [?LINE, Event], ?DEBUG_LEVEL),
	    'CosNotification_Common':notify([{proxy, OE_THIS},
					     {client, ?get_Client(State)}, 
					     {reason, {'EXCEPTION', E}}]),
	    {stop, normal, State};
	R when ?is_PersistentConnection(State) ->
	    orber:dbg("[~p] PusherConsumer:forward();~n"
		      "Admin/Channel respond incorrect: ~p~n"
		      "Dropping: ~p", [?LINE, R, Event], ?DEBUG_LEVEL),
	    {noreply, State};
	R ->
	    orber:dbg("[~p] PusherConsumer:forward();~n"
		      "Admin/Channel respond incorrect: ~p~n"
		      "Terminating and dropping: ~p", 
		      [?LINE, R, Event], ?DEBUG_LEVEL),
	    'CosNotification_Common':notify([{proxy, OE_THIS},
					     {client, ?get_Client(State)}, 
					     {reason, R}]),
	    {stop, normal, State}
    end;
forward(seq, SendTo, State, Event, Status, OE_THIS) ->
    case catch oe_CosNotificationComm_Event:callSeq(SendTo, Event, Status) of
	ok ->
	    {noreply, State};
	{'EXCEPTION', E} when is_record(E, 'OBJECT_NOT_EXIST') orelse
			      is_record(E, 'NO_PERMISSION') orelse
			      is_record(E, 'CosEventComm_Disconnected') ->
	    ?DBG("ADMIN NO LONGER EXIST; DROPPING: ~p~n", [Event]),
	    'CosNotification_Common':notify([{proxy, OE_THIS},
					     {client, ?get_Client(State)}, 
					     {reason, {'EXCEPTION', E}}]),
	    {stop, normal, State};
	R when ?is_PersistentConnection(State) ->
	    orber:dbg("[~p] PusherConsumer:forward();~n"
		      "Admin/Channel respond incorrect: ~p~n"
		      "Dropping: ~p", [?LINE, R, Event], ?DEBUG_LEVEL),
	    {noreply, State};
	R ->
	    orber:dbg("[~p] PusherConsumer:forward();~n"
		      "Admin/Channel respond incorrect: ~p~n"
		      "Terminating and dropping: ~p", 
		      [?LINE, R, Event], ?DEBUG_LEVEL),
	    'CosNotification_Common':notify([{proxy, OE_THIS},
					     {client, ?get_Client(State)}, 
					     {reason, R}]),
	    {stop, normal, State}
    end.

%%--------------- MISC FUNCTIONS, E.G. DEBUGGING -------------
%%--------------- END OF MODULE ------------------------------
