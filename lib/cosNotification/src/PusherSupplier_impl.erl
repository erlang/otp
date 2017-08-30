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
%% File    : PusherSupplier_impl.erl
%% Purpose : 
%%----------------------------------------------------------------------

-module('PusherSupplier_impl').


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
%%----- CosNotifyChannelAdmin::ProxyPushSupplier -------------
-export([connect_any_push_consumer/4]).

%%----- CosNotifyChannelAdmin::StructuredProxyPushSupplier ---
-export([connect_structured_push_consumer/4]).

%%----- CosNotifyChannelAdmin::SequenceProxyPushSupplier -----
-export([connect_sequence_push_consumer/4]).

%%----- CosNotifyChannelAdmin::*ProxyPushSupplier ------------
-export([suspend_connection/3, 
	 resume_connection/3]).

%%----- Inherit from CosNotifyChannelAdmin::ProxySupplier ----
-export([obtain_offered_types/4,
	 validate_event_qos/4]).

%%----- Inherit from CosNotification::QoSAdmin ---------------
-export([get_qos/3,
	 set_qos/4,
	 validate_qos/4]).

%%----- Inherit from CosNotifyComm::NotifySubscribe ----------
-export([subscription_change/5]).

%%----- Inherit from CosNotifyFilter::FilterAdmin ------------
-export([add_filter/4, 
	 remove_filter/4, 
	 get_filter/4,
	 get_all_filters/3, 
	 remove_all_filters/3]).

%%----- Inherit from CosEventComm::PushSupplier -------------
-export([disconnect_push_supplier/3]).

%%----- Inherit from CosNotifyComm::StructuredPushSupplier --
-export([disconnect_structured_push_supplier/3]).

%%----- Inherit from CosNotifyComm::SequencePushSupplier ----
-export([disconnect_sequence_push_supplier/3]).

%%----- Inherit from CosEventChannelAdmin::ProxyPushSupplier 
-export([connect_push_consumer/4]).

%% Attributes (external) CosNotifyChannelAdmin::ProxySupplier
-export(['_get_MyType'/3, 
	 '_get_MyAdmin'/3, 
	 '_get_priority_filter'/3,
	 '_set_priority_filter'/4,
	 '_get_lifetime_filter'/3,
	 '_set_lifetime_filter'/4]).

%%--------------- Internal -----------------------------------
%%----- Inherit from cosNotificationComm ---------------------
-export([callAny/5,
	 callSeq/5]).

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
		prioFil,
		lifetFil,
		client,
		qosGlobal,
		qosLocal,
		suspended = false,
		pacingTimer,
		subscribeType = false,
		subscribeData = true,
		etsR,
		eventDB,
		this,
		maxCache,
		cacheTimeout,
		cacheInterval}).

%% Data structures constructors
-define(get_InitState(_MyT, _MyA, _MyAP, _QS, _LQS, _Ch, _MyOp, _GT, _GL, _TR), 
	#state{myType    = _MyT,
	       myAdmin   = _MyA,
	       myAdminPid= _MyAP,
	       qosGlobal = _QS,
	       qosLocal  = _LQS,
	       myChannel = _Ch,
	       myOperator=_MyOp,
	       etsR      = ets:new(oe_ets, [set, protected]),
	       eventDB   = cosNotification_eventDB:create_db(_LQS, _GT, _GL, _TR),
	       maxCache = cosNotificationApp:max_events()}).

%% Data structures selectors
%%-------------- Data structures selectors -----------------
%% Attributes
-define(get_MyType(S),           S#state.myType).
-define(get_MyAdmin(S),          S#state.myAdmin).
-define(get_MyAdminPid(S),       S#state.myAdminPid).
-define(get_MyChannel(S),        S#state.myChannel).
-define(get_MyOperator(S),       S#state.myOperator).
-define(get_PrioFil(S),          S#state.prioFil).
-define(get_LifeTFil(S),         S#state.lifetFil).
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
%% Amin
-define(get_PacingTimer(S),      S#state.pacingTimer).
-define(get_PacingInterval(S),   round(?not_GetPacingInterval((S#state.qosLocal))/10000000)).
-define(get_BatchLimit(S),       ?not_GetMaximumBatchSize((S#state.qosLocal))).
%% Subscribe
-define(get_AllSubscribe(S),     lists:flatten(ets:match(S#state.etsR,
							 {'$1',subscribe}))).
-define(get_SubscribeType(S),    S#state.subscribeType).
-define(get_SubscribeData(S),    S#state.subscribeData).
%% ID
-define(get_IdCounter(S),        S#state.idCounter).
-define(get_SubscribeDB(S),      S#state.etsR).
%% Event
-define(is_PersistentConnection(S),
	(?not_GetConnectionReliability((S#state.qosLocal)) == ?not_Persistent)).
-define(is_PersistentEvent(S),
	(?not_GetEventReliability((S#state.qosLocal)) == ?not_Persistent)).

-define(get_Event(S),            cosNotification_eventDB:get_event(S#state.eventDB, 
								   false)).
%								   (not ?is_PersistentEvent(S)))).
-define(get_Events(S,M),         cosNotification_eventDB:get_events(S#state.eventDB, M, false)).
%								    (not ?is_PersistentEvent(S)))).

%%-------------- Data structures modifiers -----------------
%% Attributes
-define(set_PrioFil(S,D),        S#state{prioFil=D}).
-define(set_LifeTFil(S,D),       S#state{lifetFil=D}).
%% Client Object
-define(set_Client(S,D),         S#state{client=D}).
-define(del_Client(S),           S#state{client=undefined}).
-define(set_Unconnected(S),      S#state{client=undefined}).
-define(set_Suspended(S),        S#state{suspended=true}).
-define(set_NotSuspended(S),     S#state{suspended=false}).
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
%% Admin
-define(set_PacingTimer(S,T),    S#state{pacingTimer=T}).
%% Publish
%% Subscribe
-define(add_Subscribe(S,E),      ets:insert(S#state.etsR, {E, subscribe})).
-define(del_Subscribe(S,E),      ets:delete(S#state.etsR, E)).
-define(set_SubscribeType(S,T),  S#state{subscribeType=T}).
-define(set_SubscribeData(S,D),  S#state{subscribeData=D}).
%% ID
-define(set_IdCounter(S,V),      S#state{idCounter=V}).
-define(new_Id(S),               'CosNotification_Common':create_id(S#state.idCounter)).
%% Events
-define(add_Event(S,E),          catch cosNotification_eventDB:
	add_event(S#state.eventDB, E, S#state.lifetFil, S#state.prioFil)).
-define(addAndGet_Event(S,E),    catch cosNotification_eventDB:
	add_and_get_event(S#state.eventDB, E, S#state.lifetFil, S#state.prioFil,
			  false)).
%			  ?is_PersistentEvent(S))).
-define(update_EventDB(S,Q),     S#state{eventDB=
					 cosNotification_eventDB:update(S#state.eventDB, Q)}).


%%-------------- MISC ----------------------------------------
-define(is_ANY(S),               S#state.myType == 'PUSH_ANY').
-define(is_STRUCTURED(S),        S#state.myType == 'PUSH_STRUCTURED').
-define(is_SEQUENCE(S),          S#state.myType == 'PUSH_SEQUENCE').
-define(is_ANDOP(S),             S#state.myOperator == 'AND_OP').
-define(is_UnConnected(S),       S#state.client == undefined).
-define(is_Connected(S),         S#state.client =/= undefined).
-define(is_Suspended(S),         S#state.suspended == true).
-define(is_NotSuspended(S),      S#state.suspended == false).
-define(is_BatchLimitReached(S), cosNotification_eventDB:status(S#state.eventDB,
								{batchLimit, 
								 ?not_GetMaximumBatchSize((S#state.qosLocal))})).
-define(has_Filters(S),          S#state.myFilters =/= []).

%%----------------------------------------------------------%
%% function : handle_info, code_change
%% Arguments: 
%% Returns  : 
%% Effect   : Functions demanded by the gen_server module. 
%%-----------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(Info, #state{cacheTimeout = Timeout, 
			 cacheInterval = Interval} = State) ->
    ?DBG("INFO: ~p~n", [Info]),
    case Info of
        {'EXIT', Pid, Reason} when ?get_MyAdminPid(State)==Pid ->
            ?DBG("PARENT ADMIN: ~p  TERMINATED.~n",[Reason]),
	    {stop, Reason, State};
        {'EXIT', _Pid, _Reason} ->
            ?DBG("PROXYPUSHSUPPLIER: ~p  TERMINATED.~n",[_Reason]),
            {noreply, State};
        pacing ->
	    lookup_and_push(State, true);
        cacheInterval ->
	    lookup_and_push(State, true);
	cacheTimeout when Timeout == undefined, Interval == undefined ->
	    %% Late message, do not terminate
	    {noreply, State};
	cacheTimeout ->
	    'CosNotification_Common':notify([{proxy, State#state.this},
					     {client, State#state.client}, 
					     {reason, 
					      {timer, "Reached upper limit"}}]),
	    {stop, normal, State};
        _ ->
            {noreply, State}
    end.

%%----------------------------------------------------------%
%% function : init, terminate
%% Arguments: 
%%-----------------------------------------------------------

init([MyType, MyAdmin, MyAdminPid, InitQoS, LQS, MyChannel, Options, Operator]) ->
    process_flag(trap_exit, true),
    GCTime = 'CosNotification_Common':get_option(gcTime, Options, 
						 ?not_DEFAULT_SETTINGS),
    GCLimit = 'CosNotification_Common':get_option(gcLimit, Options, 
						  ?not_DEFAULT_SETTINGS),
    TimeRef = 'CosNotification_Common':get_option(timeService, Options, 
						  ?not_DEFAULT_SETTINGS),
    timer:start(),
    {ok, ?get_InitState(MyType, MyAdmin, MyAdminPid, 
			InitQoS, LQS, MyChannel, Operator, GCTime, GCLimit, TimeRef)}.

terminate(_Reason, State) when ?is_UnConnected(State) ->
    stop_timer(State#state.cacheTimeout),
    stop_timer(State#state.cacheInterval),
    stop_timer(State#state.pacingTimer),
    %% We are not connected to a Client. Hence, no need to invoke disconnect.
    ok;
terminate(_Reason, State) when ?is_ANY(State) ->
    stop_timer(State#state.cacheTimeout),
    stop_timer(State#state.cacheInterval),
    stop_timer(State#state.pacingTimer),
    'CosNotification_Common':disconnect('CosEventComm_PushConsumer', 
					disconnect_push_consumer, 
					?get_Client(State));
terminate(_Reason, State) when ?is_SEQUENCE(State) ->
    stop_timer(State#state.cacheTimeout),
    stop_timer(State#state.cacheInterval),
    stop_timer(State#state.pacingTimer),
    'CosNotification_Common':disconnect('CosNotifyComm_SequencePushConsumer',
					disconnect_sequence_push_consumer,
					?get_Client(State));
terminate(_Reason, State) when ?is_STRUCTURED(State) ->
    stop_timer(State#state.cacheTimeout),
    stop_timer(State#state.cacheInterval),
    stop_timer(State#state.pacingTimer),
    'CosNotification_Common':disconnect('CosNotifyComm_StructuredPushConsumer', 
					disconnect_structured_push_consumer, 
					?get_Client(State)).

%%-----------------------------------------------------------
%%----- CosNotifyChannelAdmin_ProxySupplier attributes ------
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

%%----------------------------------------------------------%
%% Attribute: '_*et_priority_filter'
%% Type     : read/write
%% Returns  : 
%%-----------------------------------------------------------
'_get_priority_filter'(_OE_THIS, _OE_FROM, State) ->
    {reply, ?get_PrioFil(State), State}.
'_set_priority_filter'(_OE_THIS, _OE_FROM, State, PrioF) ->
    {reply, ok, ?set_PrioFil(State, PrioF)}.

%%----------------------------------------------------------%
%% Attribute: '_*et_lifetime_filter'
%% Type     : read/write
%% Returns  : 
%%-----------------------------------------------------------
'_get_lifetime_filter'(_OE_THIS, _OE_FROM, State) ->
    {reply, ?get_LifeTFil(State), State}.
'_set_lifetime_filter'(_OE_THIS, _OE_FROM, State, LifeTF) ->
    {reply, ok, ?set_LifeTFil(State, LifeTF)}.

%%-----------------------------------------------------------
%%------- Exported external functions -----------------------
%%-----------------------------------------------------------
%%----- CosEventChannelAdmin::ProxyPushSupplier -------------
%%----------------------------------------------------------%
%% function : connect_push_consumer
%% Arguments: Client - CosEventComm::PushConsumer
%% Returns  :  ok | {'EXCEPTION', #'AlreadyConnected'{}} |
%%            {'EXCEPTION', #'TypeError'{}}
%%            Both exceptions from CosEventChannelAdmin!!!!
%%-----------------------------------------------------------
connect_push_consumer(OE_THIS, OE_FROM, State, Client) ->
    connect_any_push_consumer(OE_THIS, OE_FROM, State, Client).

%%----- CosNotifyChannelAdmin::ProxyPushSupplier ------------
%%----------------------------------------------------------%
%% function : connect_any_push_consumer
%% Arguments: Client - CosEventComm::PushConsumer
%% Returns  :  ok | {'EXCEPTION', #'AlreadyConnected'{}} |
%%            {'EXCEPTION', #'TypeError'{}}
%%            Both exceptions from CosEventChannelAdmin!!!!
%%-----------------------------------------------------------
connect_any_push_consumer(OE_THIS, _OE_FROM, State, Client) when ?is_ANY(State) ->
    'CosNotification_Common':type_check(Client, 'CosEventComm_PushConsumer'),
    if
	?is_Connected(State) ->
	    corba:raise(#'CosEventChannelAdmin_AlreadyConnected'{});
	true ->
	    {reply, ok, State#state{client = Client, this = OE_THIS}}
    end;
connect_any_push_consumer(_, _, _, _) ->
    corba:raise(#'BAD_OPERATION'{completion_status=?COMPLETED_NO}).

%%----- CosNotifyChannelAdmin::SequenceProxyPushSupplier ----
%%----------------------------------------------------------%
%% function : connect_sequence_push_consumer
%% Arguments: Client - CosNotifyComm::SequencePushConsumer
%% Returns  :  ok | {'EXCEPTION', #'AlreadyConnected'{}} |
%%            {'EXCEPTION', #'TypeError'{}}
%%-----------------------------------------------------------
connect_sequence_push_consumer(OE_THIS, _OE_FROM, State, Client) when ?is_SEQUENCE(State) ->
    'CosNotification_Common':type_check(Client, 
					'CosNotifyComm_SequencePushConsumer'),
    if
	?is_Connected(State) ->
	    corba:raise(#'CosEventChannelAdmin_AlreadyConnected'{});
	true ->
	    NewState = start_timer(State),
	    {reply, ok, NewState#state{client = Client, this = OE_THIS}}
    end;
connect_sequence_push_consumer(_, _, _, _) ->
    corba:raise(#'BAD_OPERATION'{completion_status=?COMPLETED_NO}).

%%----- CosNotifyChannelAdmin::StructuredProxyPushSupplier ---
%%----------------------------------------------------------%
%% function : connect_structured_push_consumer
%% Arguments: Client - CosNotifyComm::StructuredPushConsumer
%% Returns  :  ok | {'EXCEPTION', #'AlreadyConnected'{}} |
%%            {'EXCEPTION', #'TypeError'{}}
%%-----------------------------------------------------------
connect_structured_push_consumer(OE_THIS, _OE_FROM, State, Client) when ?is_STRUCTURED(State) ->
    'CosNotification_Common':type_check(Client, 
					'CosNotifyComm_StructuredPushConsumer'),
    if
	?is_Connected(State) ->
	    corba:raise(#'CosEventChannelAdmin_AlreadyConnected'{});
	true ->
	    {reply, ok, State#state{client = Client, this = OE_THIS}}
    end;
connect_structured_push_consumer(_, _, _, _) ->
    corba:raise(#'BAD_OPERATION'{completion_status=?COMPLETED_NO}).

%%----- CosNotifyChannelAdmin::*ProxyPushSupplier -----------
%%----------------------------------------------------------%
%% function : suspend_connection
%% Arguments: 
%% Returns  : ok | {'EXCEPTION', #'ConnectionAlreadyInactive'{}} |
%%            {'EXCEPTION', #'NotConneced'{}}
%%-----------------------------------------------------------
suspend_connection(_OE_THIS, _OE_FROM, State) when ?is_Connected(State) ->
    if
	?is_Suspended(State) ->
	    corba:raise(#'CosNotifyChannelAdmin_ConnectionAlreadyInactive'{});
	true ->
	    stop_timer(State#state.pacingTimer),
	    {reply, ok, State#state{pacingTimer = undefined,
				    suspended=true}}
    end;
suspend_connection(_,_,_)->
      corba:raise(#'CosNotifyChannelAdmin_NotConnected'{}).
  
%%----------------------------------------------------------%
%% function : resume_connection
%% Arguments: 
%% Returns  :  ok | {'EXCEPTION', #'ConnectionAlreadyActive'{}} |
%%            {'EXCEPTION', #'NotConneced'{}}
%%-----------------------------------------------------------
resume_connection(_OE_THIS, OE_FROM, State) when ?is_Connected(State) ->
    if
	?is_NotSuspended(State) ->
	    corba:raise(#'CosNotifyChannelAdmin_ConnectionAlreadyActive'{});
	true ->
	    corba:reply(OE_FROM, ok),
	    if
		?is_SEQUENCE(State) ->
		    start_timer(State);
		true ->
		    ok
	    end,
	    lookup_and_push(?set_NotSuspended(State))
    end;
resume_connection(_,_,_) ->
    corba:raise(#'CosNotifyChannelAdmin_NotConnected'{}).

%%----- Inherit from CosNotifyChannelAdmin::ProxySupplier ---
%%----------------------------------------------------------%
%% function : obtain_offered_types
%% Arguments: Mode - enum 'ObtainInfoMode' (CosNotifyChannelAdmin)
%% Returns  : CosNotification::EventTypeSeq
%%-----------------------------------------------------------
obtain_offered_types(_OE_THIS, _OE_FROM, State, 'ALL_NOW_UPDATES_OFF') ->
    {reply, ?get_AllSubscribe(State), ?set_SubscribeType(State, false)};
obtain_offered_types(_OE_THIS, _OE_FROM, State, 'ALL_NOW_UPDATES_ON') ->
    {reply, ?get_AllSubscribe(State), ?set_SubscribeType(State, true)};
obtain_offered_types(_OE_THIS, _OE_FROM, State, 'NONE_NOW_UPDATES_OFF') ->
    {reply, [], ?set_SubscribeType(State, false)};
obtain_offered_types(_OE_THIS, _OE_FROM, State, 'NONE_NOW_UPDATES_ON') ->
    {reply, [], ?set_SubscribeType(State, true)};
obtain_offered_types(_,_,_,What) ->
    orber:dbg("[~p] PusherSupplier:obtain_offered_types(~p);~n"
	      "Bad enumerant", [?LINE, What], ?DEBUG_LEVEL),
    corba:raise(#'BAD_OPERATION'{completion_status=?COMPLETED_NO}).

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

%%----- Inherit from CosNotifyComm::NotifySubscribe ---------
%%----------------------------------------------------------%
%% function : subscription_change
%% Arguments: Added - #'CosNotification_EventType'{}
%%            Removed - #'CosNotification_EventType'{}
%% Returns  : ok | 
%%            {'EXCEPTION', #'CosNotifyComm_InvalidEventType'{}}
%%-----------------------------------------------------------
subscription_change(_OE_THIS, _OE_FROM, State, Added, Removed) ->
    cosNotification_Filter:validate_types(Added), 
    cosNotification_Filter:validate_types(Removed),
    %% On this "side", we care about which type of events the client 
    %% will require, since the client (or an agent) clearly stated
    %% that it's only interested in these types of events.
    %% Also see PusherConsumer- and PullerConsumer-'offer_change'.
    update_subscribe(remove, State, Removed),
    CurrentSub = ?get_AllSubscribe(State),
    NewState = 
	case cosNotification_Filter:check_types(Added++CurrentSub) of
	    true ->
		%% Types supplied does in some way cause all events to be valid.
		%% Smart? Would have been better to not supply any at all.
		?set_SubscribeData(State, true);
	    {ok, Which, WC} ->
		?set_SubscribeData(State, {Which, WC})
    end,
    update_subscribe(add, NewState, Added),
    case ?get_SubscribeType(NewState) of
	true ->
	    %% Perhaps we should handle exception here?!
	    %% Probably not. Better to stay "on-line".
	    catch 'CosNotifyComm_NotifyPublish':
		offer_change(?get_Client(NewState), Added, Removed),
	    ok;
	_->
	    ok
    end,	
    {reply, ok, NewState}.

update_subscribe(_, _, [])->
    ok;
update_subscribe(add, State, [H|T]) ->
    ?add_Subscribe(State, H),
    update_subscribe(add, State, T);
update_subscribe(remove, State, [H|T]) ->
    ?del_Subscribe(State, H),
    update_subscribe(remove, State, T).

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
    orber:dbg("[~p] PusherSupplier:remove_filter(~p); Not an integer", 
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
    orber:dbg("[~p] PusherSupplier:get_filter(~p); Not an integer", 
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


%%----- Inherit from CosEventComm::PushSupplier -------------
%%----------------------------------------------------------%
%% function : disconnect_push_supplier
%% Arguments: -
%% Returns  : ok
%%-----------------------------------------------------------
disconnect_push_supplier(_OE_THIS, _OE_FROM, State) ->
    {stop, normal, ok, ?set_Unconnected(State)}.

%%----- Inherit from CosNotifyComm::StructuredPushSupplier --
%%----------------------------------------------------------%
%% function : disconnect_structured_push_supplier
%% Arguments: -
%% Returns  : ok
%%-----------------------------------------------------------
disconnect_structured_push_supplier(_OE_THIS, _OE_FROM, State) ->
    {stop, normal, ok, ?set_Unconnected(State)}.

%%----- Inherit from CosNotifyComm::SequencePushSupplier ----
%%----------------------------------------------------------%
%% function : disconnect_sequence_push_supplier
%% Arguments: -
%% Returns  : ok
%%-----------------------------------------------------------
disconnect_sequence_push_supplier(_OE_THIS, _OE_FROM, State) ->
    {stop, normal, ok, ?set_Unconnected(State)}.

%%--------------- LOCAL FUNCTIONS ----------------------------
find_obj({value, {_, Obj}}) -> Obj;
find_obj(_) -> {'EXCEPTION', #'CosNotifyFilter_FilterNotFound'{}}.

find_ids(List) ->           find_ids(List, []).
find_ids([], Acc) ->        Acc;
find_ids([{I,_}|T], Acc) -> find_ids(T, [I|Acc]);
find_ids(What, _) -> 
    orber:dbg("[~p] PusherSupplier:find_ids();~n"
	      "Id corrupt: ~p", [?LINE, What], ?DEBUG_LEVEL),
    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO}).

%% Delete a single object.
%% The list do not differ, i.e., no filter removed, raise exception.
delete_obj(List,List) -> corba:raise(#'CosNotifyFilter_FilterNotFound'{});
delete_obj(List,_) -> List.

%%-----------------------------------------------------------
%% function : callSeq
%% Arguments: 
%% Returns  : 
%%-----------------------------------------------------------
callSeq(_OE_THIS, OE_FROM, State, EventsIn, Status) ->
    corba:reply(OE_FROM, ok),
    case cosNotification_eventDB:validate_event(?get_SubscribeData(State), EventsIn,
						?get_AllFilter(State),
						?get_SubscribeDB(State),
						Status) of
	{[],_} ->
	    ?DBG("PROXY NOSUBSCRIPTION SEQUENCE/STRUCTURED: ~p~n",[EventsIn]),
	    {noreply, State};
	{Events,_} when ?is_Suspended(State) ->
	    store_events(State, Events),
	    {noreply, State};
	{Events,_} when ?is_UnConnected(State) ->
	    orber:dbg("[~p] PusherSupplier:callAny();~n"
		      "Not connected, dropping event(s): ~p", 
		      [?LINE, Events], ?DEBUG_LEVEL),
	    {noreply, State};
	{[Event],_} when ?is_STRUCTURED(State) ->
	    ?DBG("PROXY RECEIVED SEQUENCE: ~p~n",[Event]),
	    empty_db(State, ?addAndGet_Event(State, Event));
	{[Event],_} when ?is_ANY(State) ->
	    ?DBG("PROXY RECEIVED SEQUENCE: ~p~n",[Event]),
	    AnyEvent = any:create('CosNotification_StructuredEvent':tc(),Event),
	    empty_db(State, ?addAndGet_Event(State, AnyEvent));
	{Events,_} ->
	    ?DBG("PROXY RECEIVED SEQUENCE: ~p~n",[Events]),
	    store_events(State, Events),
	    lookup_and_push(State)
    end.

%%-----------------------------------------------------------
%% function : callAny
%% Arguments: 
%% Returns  : 
%%-----------------------------------------------------------
callAny(_OE_THIS, OE_FROM, State, EventIn, Status) ->
    corba:reply(OE_FROM, ok),
    case cosNotification_eventDB:validate_event(?get_SubscribeData(State), EventIn,
						?get_AllFilter(State),
						?get_SubscribeDB(State),
						Status) of
	{[],_} ->
	    ?DBG("PROXY NOSUBSCRIPTION ANY: ~p~n",[EventIn]),
	    %% To be on the safe side, test if there are any events that not
	    %% have been forwarded (should only be possible if StartTime is used).
	    lookup_and_push(State);
	{Event,_} when ?is_Suspended(State), ?is_ANY(State) ->
	    ?add_Event(State, Event),
	    {noreply, State};
	{Event,_} when ?is_Suspended(State) ->
	    ?add_Event(State, ?not_CreateSE("","%ANY","",[],[],Event)),
	    {noreply, State};
	{Event,_} when ?is_UnConnected(State) ->
	    orber:dbg("[~p] PusherSupplier:callAny();~n"
		      "Not connected, dropping event: ~p", 
		      [?LINE, Event], ?DEBUG_LEVEL),
	    {noreply, State};
	{Event,_} when ?is_ANY(State) ->
	    ?DBG("PROXY RECEIVED ANY: ~p~n",[Event]),
	    %% We must store the event since there may be other events that should
	    %% be delivered first, e.g., higher priority.
	    empty_db(State, ?addAndGet_Event(State, Event));
	{Event,_} when ?is_SEQUENCE(State) ->
	    ?DBG("PROXY RECEIVED ANY==>SEQUENCE: ~p~n",[Event]),
	    StrEvent = ?not_CreateSE("","%ANY","",[],[],Event),
	    ?add_Event(State, StrEvent),
	    lookup_and_push(State);
	{Event,_} ->
	    ?DBG("PROXY RECEIVED ANY==>STRUCTURED: ~p~n",[Event]),
	    StrEvent = ?not_CreateSE("","%ANY","",[],[],Event),
	    empty_db(State, ?addAndGet_Event(State, StrEvent))
    end.

%% Lookup and push "the correct" amount of events.
lookup_and_push(State) ->
    %% The boolean indicates, if false, that we will only push events if we have 
    %% passed the BatchLimit. If true we will ignore this limit and push events
    %% anyway (typcially invoked when pacing limit passed).
    lookup_and_push(State, false).
lookup_and_push(State, false) when ?is_SEQUENCE(State) ->
    case ?is_BatchLimitReached(State) of
	true ->
	    case ?get_Events(State, ?get_BatchLimit(State)) of
		{[], _, _} ->
		    ?DBG("BATCHLIMIT (~p) REACHED BUT NO EVENTS FOUND~n",
				 [?get_BatchLimit(State)]),
		    {noreply, State};
		{Events, _, Keys} ->
		    ?DBG("BATCHLIMIT (~p) REACHED, EVENTS FOUND: ~p~n",
				 [?get_BatchLimit(State), Events]),
		    case catch 'CosNotifyComm_SequencePushConsumer':
			push_structured_events(?get_Client(State), Events) of
			ok ->
			    cosNotification_eventDB:delete_events(Keys),
			    lookup_and_push(reset_cache(State), false);
			{'EXCEPTION', E} when is_record(E, 'OBJECT_NOT_EXIST') orelse
					      is_record(E, 'NO_PERMISSION') orelse
					      is_record(E, 'CosEventComm_Disconnected') ->
			    ?DBG("PUSH SUPPLIER CLIENT NO LONGER EXIST~n", []),
			    'CosNotification_Common':notify([{proxy, State#state.this},
							     {client, ?get_Client(State)}, 
							     {reason, {'EXCEPTION', E}}]),
			    {stop, normal, State};
			What when ?is_PersistentEvent(State),
				  ?is_PersistentConnection(State) ->
			    orber:dbg("[~p] PusherSupplier:lookup_and_push();~n"
				      "Client respond incorrect: ~p", 
				      [?LINE, What], ?DEBUG_LEVEL),
			    check_cache(State);
			What when ?is_PersistentConnection(State) ->
			    %% Here we should do something when we want to handle
			    %% Persistent EventReliability.
			    orber:dbg("[~p] PusherSupplier:lookup_and_push();~n"
				      "Client respond incorrect: ~p~n"
				      "Dropping events: ~p", 
				      [?LINE, What, Events], ?DEBUG_LEVEL),
			    cosNotification_eventDB:delete_events(Keys),
			    {noreply, State};
			WhatII ->
			    orber:dbg("[~p] PusherSupplier:lookup_and_push();~n"
				      "Client respond incorrect: ~p~n"
				      "Terminating and dropping events: ~p",
				      [?LINE, WhatII, Events], ?DEBUG_LEVEL),
			    'CosNotification_Common':notify([{proxy, State#state.this},
							     {client, ?get_Client(State)}, 
							     {reason, WhatII}]),
			    {stop, normal, State}
		    end
	    end;
	_ ->
	    ?DBG("BATCHLIMIT (~p) NOT REACHED~n",[?get_BatchLimit(State)]),
	    {noreply, State}
    end;
lookup_and_push(State, true) when ?is_SEQUENCE(State) ->
    case ?get_Events(State, ?get_BatchLimit(State)) of
	{[], _, _} ->
	    ?DBG("PACELIMIT REACHED BUT NO EVENTS FOUND~n", []),
	    {noreply, State};
	{Events, _, Keys} ->
	    ?DBG("PACELIMIT REACHED, EVENTS FOUND: ~p~n", [Events]),
	    case catch 'CosNotifyComm_SequencePushConsumer':
		push_structured_events(?get_Client(State), Events) of
		ok ->
		    cosNotification_eventDB:delete_events(Keys),
		    lookup_and_push(reset_cache(State), false);
		{'EXCEPTION', E} when is_record(E, 'OBJECT_NOT_EXIST') orelse
				      is_record(E, 'NO_PERMISSION') orelse
				      is_record(E, 'CosEventComm_Disconnected') ->
		    orber:dbg("[~p] PusherSupplier:lookup_and_push();~n"
			      "Client no longer exists; terminating and dropping events: ~p", 
			      [?LINE, Events], ?DEBUG_LEVEL),
		    'CosNotification_Common':notify([{proxy, State#state.this},
						     {client, ?get_Client(State)}, 
						     {reason, {'EXCEPTION', E}}]),
		    {stop, normal, State};
		What when ?is_PersistentEvent(State),
			  ?is_PersistentConnection(State) ->
		    orber:dbg("[~p] PusherSupplier:lookup_and_push();~n"
			      "Client respond incorrect: ~p", 
			      [?LINE, What], ?DEBUG_LEVEL),
		    check_cache(State);
		What when ?is_PersistentConnection(State) ->
		    %% Here we should do something when we want to handle
		    %% Persistent EventReliability.
		    orber:dbg("[~p] PusherSupplier:lookup_and_push();~n"
			      "Client respond incorrect: ~p~n"
			      "Dropping events: ~p", 
			      [?LINE, What, Events], ?DEBUG_LEVEL),
		    cosNotification_eventDB:delete_events(Keys),
		    {noreply, State};
		WhatII ->
		    orber:dbg("[~p] PusherSupplier:lookup_and_push();~n"
			      "Client respond incorrect: ~p~n"
			      "Terminating and dropping events: ~p", 
			      [?LINE, WhatII, Events], ?DEBUG_LEVEL),
		    'CosNotification_Common':notify([{proxy, State#state.this},
						     {client, ?get_Client(State)}, 
						     {reason, WhatII}]),
		    {stop, normal, State}
	    end
    end;
lookup_and_push(State, _) ->
    empty_db(State, ?get_Event(State)).


%% Push all events stored while not connected or received in sequence.
empty_db(State, {[], _, _}) ->
    {noreply, State};
empty_db(State, {Event, _, Keys}) when ?is_STRUCTURED(State) ->
    case catch 'CosNotifyComm_StructuredPushConsumer':
	push_structured_event(?get_Client(State), Event) of
	ok ->
	    cosNotification_eventDB:delete_events(Keys),
	    NewState = reset_cache(State),
	    empty_db(NewState, ?get_Event(NewState));
	{'EXCEPTION', E} when is_record(E, 'OBJECT_NOT_EXIST') orelse
			      is_record(E, 'NO_PERMISSION') orelse
			      is_record(E, 'CosEventComm_Disconnected') ->
	    orber:dbg("[~p] PusherSupplier:empty_db();~n"
		      "Client no longer exists; terminating and dropping: ~p", 
		      [?LINE, Event], ?DEBUG_LEVEL),
	    'CosNotification_Common':notify([{proxy, State#state.this},
					     {client, ?get_Client(State)}, 
					     {reason, {'EXCEPTION', E}}]),
	    {stop, normal, State};
	What when ?is_PersistentEvent(State),
		  ?is_PersistentConnection(State) ->
	    orber:dbg("[~p] PusherSupplier:lookup_and_push();~n"
		      "Client respond incorrect: ~p", 
		      [?LINE, What], ?DEBUG_LEVEL),
	    check_cache(State);
	What when ?is_PersistentConnection(State) ->
	    %% Here we should do something when we want to handle
	    %% Persistent EventReliability.
	    orber:dbg("[~p] PusherSupplier:empty_db();~n"
		      "Client respond incorrect: ~p~n"
		      "Dropping event: ~p", 
		      [?LINE, What, Event], ?DEBUG_LEVEL),
	    cosNotification_eventDB:delete_events(Keys),
	    {noreply, State};
	WhatII ->
	    orber:dbg("[~p] PusherSupplier:empty_db();~n"
		      "Client respond incorrect: ~p~n"
		      "Terminating and dropping: ~p", 
		      [?LINE, WhatII, Event], ?DEBUG_LEVEL),
	    'CosNotification_Common':notify([{proxy, State#state.this},
					     {client, ?get_Client(State)}, 
					     {reason, WhatII}]),
	    {stop, normal, State}
    end;
empty_db(State, {Event, _, Keys}) when ?is_ANY(State) ->
    case catch 'CosEventComm_PushConsumer':push(?get_Client(State), Event) of
	ok ->
	    cosNotification_eventDB:delete_events(Keys),
	    NewState = reset_cache(State),
	    empty_db(NewState, ?get_Event(NewState));
	{'EXCEPTION', E} when is_record(E, 'OBJECT_NOT_EXIST') orelse
			      is_record(E, 'NO_PERMISSION') orelse
			      is_record(E, 'CosEventComm_Disconnected') ->
	    orber:dbg("[~p] PusherSupplier:empty_db();~n"
		      "Client no longer exists; terminating and dropping: ~p", 
		      [?LINE, Event], ?DEBUG_LEVEL),
	    'CosNotification_Common':notify([{proxy, State#state.this},
					     {client, ?get_Client(State)}, 
					     {reason, {'EXCEPTION', E}}]),
	    {stop, normal, State};
	What when ?is_PersistentEvent(State),
		  ?is_PersistentConnection(State) ->
	    orber:dbg("[~p] PusherSupplier:lookup_and_push();~n"
		      "Client respond incorrect: ~p", 
		      [?LINE, What], ?DEBUG_LEVEL),
	    check_cache(State);
	What when ?is_PersistentConnection(State) ->
	    %% Here we should do something when we want to handle
	    %% Persistent EventReliability.
	    orber:dbg("[~p] PusherSupplier:empty_db();~n"
		      "Client respond incorrect: ~p~n"
		      "Dropping Event: ~p", 
		      [?LINE, What, Event], ?DEBUG_LEVEL),
	    cosNotification_eventDB:delete_events(Keys),
	    {noreply, State};
	WhatII ->
	    orber:dbg("[~p] PusherSupplier:empty_db();~n"
		      "Client respond incorrect: ~p~n"
		      "Terminating and dropping: ~p", 
		      [?LINE, WhatII, Event], ?DEBUG_LEVEL),
	    'CosNotification_Common':notify([{proxy, State#state.this},
					     {client, ?get_Client(State)}, 
					     {reason, WhatII}]),
	    {stop, normal, State}
    end.

reset_cache(#state{cacheTimeout = undefined, 
		   cacheInterval = undefined} = State) ->
    State;
reset_cache(State) ->
    stop_timer(State#state.cacheTimeout),
    stop_timer(State#state.cacheInterval),
    State#state{cacheTimeout = undefined, 
		cacheInterval = undefined}.

check_cache(#state{maxCache = Max, cacheTimeout = Timeout, 
		   cacheInterval = Interval} = State) ->
    case cosNotification_eventDB:status(State#state.eventDB, eventCounter) of
	Count when Count > Max ->
	    %% Reached the upper limit, terminate.
	    'CosNotification_Common':notify([{proxy, State#state.this},
					     {client, State#state.client}, 
					     {reason, {max_events, Max}}]),
	    {stop, normal, State};
	_ when Timeout == undefined, Interval == undefined ->
	    case {timer:send_interval(cosNotificationApp:interval_events(), 
				      cacheInterval),
		  timer:send_after(cosNotificationApp:timeout_events(), 
				   cacheTimeout)} of
		{{ok, IntervalRef}, {ok, TimeoutRef}} ->
		    {noreply, State#state{cacheTimeout = TimeoutRef, 
					  cacheInterval = IntervalRef}};
		Error ->
		    orber:dbg("[~p] PusherSupplier:check_cache();~n"
			      "Unable to start timers: ~p", 
			      [?LINE, Error], ?DEBUG_LEVEL),
		    'CosNotification_Common':notify([{proxy, State#state.this},
						     {client, State#state.client}, 
						     {reason, {timer, Error}}]),
		    {stop, normal, State}
	    end;
	_ ->
	    %% Timers already started.
	    {noreply, State}
    end.

store_events(_State, []) ->
    ok;
store_events(State, [Event|Rest]) when ?is_ANY(State) ->
    AnyEvent = any:create('CosNotification_StructuredEvent':tc(),Event),
    ?add_Event(State, AnyEvent),
    store_events(State, Rest);
store_events(State, [Event|Rest]) ->
    ?add_Event(State, Event),
    store_events(State, Rest).

%% Start timers which send a message each time we should push events. Only used
%% when this objects is defined to supply sequences.
start_timer(State) ->
    case ?get_PacingInterval(State) of
	0 ->
	    ?DBG("PUSH SUPPLIER STARTED NO TIMER (0), BATCH LIMIT: ~p~n", 
		 [?get_BatchLimit(State)]),

	    State;
	PacInt ->
	    case catch timer:send_interval(timer:seconds(PacInt), pacing) of
		{ok,PacTRef} ->
		    ?DBG("PUSH SUPPLIER STARTED TIMER, BATCH LIMIT: ~p~n",
			 [?get_BatchLimit(State)]),
		    ?set_PacingTimer(State, PacTRef);
		What ->
		    orber:dbg("[~p] PusherSupplier:start_timer();~n"
			      "Unable to invoke timer:send_interval/2: ~p", 
			      [?LINE, What], ?DEBUG_LEVEL),
		    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
	    end
    end.

stop_timer(undefined) ->
    ?DBG("PUSH SUPPLIER HAVE NO TIMER TO STOP~n",[]),
    ok;
stop_timer(Timer) ->
    ?DBG("PUSH SUPPLIER STOPPED TIMER~n",[]),
    timer:cancel(Timer),
    ok.

	    
%%--------------- MISC FUNCTIONS, E.G. DEBUGGING -------------
%%--------------- END OF MODULE ------------------------------
