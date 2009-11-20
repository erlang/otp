%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2009. All Rights Reserved.
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
%% File    : PullerSupplier_impl.erl
%% Purpose : 
%%----------------------------------------------------------------------

-module('PullerSupplier_impl').

%%--------------- INCLUDES -----------------------------------
-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").
%% cosEvent files.
-include_lib("cosEvent/include/CosEventChannelAdmin.hrl").
%% Application files
-include("CosNotification.hrl").
-include("CosNotifyChannelAdmin.hrl").
-include("CosNotifyComm.hrl").
-include("CosNotifyFilter.hrl").

-include("CosNotification_Definitions.hrl").

%%--------------- EXPORTS ------------------------------------
%%--------------- External -----------------------------------
%%----- CosNotifyChannelAdmin::ProxyPullSupplier -------------
-export([connect_any_pull_consumer/4]).

%%----- CosNotifyChannelAdmin::SequenceProxyPullSupplier -----
-export([connect_sequence_pull_consumer/4]).

%%----- CosNotifyChannelAdmin::StructuredProxyPullSupplier ---
-export([connect_structured_pull_consumer/4]).

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

%%----- Inherit from CosEventComm::PullSupplier -------------
-export([pull/3, 
	 try_pull/3, 
	 disconnect_pull_supplier/3]).

%%----- Inherit from CosNotifyComm::SequencePullSupplier --
-export([pull_structured_events/4, 
	 try_pull_structured_events/4, 
	 disconnect_sequence_pull_supplier/3]).

%%----- Inherit from CosNotifyComm::StructuredPullSupplier --
-export([pull_structured_event/3, 
	 try_pull_structured_event/3, 
	 disconnect_structured_pull_supplier/3]).

%%----- Inherit from CosEventChannelAdmin::ProxyPullSupplier
-export([connect_pull_consumer/4]).

%% Attributes (external) CosNotifyChannelAdmin::ProxySupplier
-export(['_get_MyType'/3, 
	 '_get_MyAdmin'/3, 
	 '_get_priority_filter'/3,
	 '_set_priority_filter'/4,
	 '_get_lifetime_filter'/3,
	 '_set_lifetime_filter'/4]).

%%--------------- Internal -----------------------------------
%%----- Inherit from cosNotificationComm --------------------
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
		pacingTimer,
		respondTo,
		subscribeType = false,
		subscribeData = true,
		etsR,
		eventDB}).

%% Data structures constructors
-define(get_InitState(_MyT, _MyA, _MyAP, _QS, _LQS, _Ch, _MyOp, _GT, _GL, _TR), 
	#state{myType    = _MyT,
	       myAdmin   = _MyA,
	       myAdminPid= _MyAP,
	       myChannel = _Ch,
	       myOperator= _MyOp,
	       qosGlobal = _QS,
	       qosLocal  = _LQS,
	       etsR      = ets:new(oe_ets, [set, protected]),
	       eventDB   = cosNotification_eventDB:create_db(_LQS, _GT, _GL, _TR)}).


%% Data structures selectors
%% Attributes
-define(get_MyType(S),           S#state.myType).
-define(get_MyAdmin(S),          S#state.myAdmin).
-define(get_MyAdminPid(S),       S#state.myAdmin).
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
%% Event
-define(get_Event(S),            cosNotification_eventDB:get_event(S#state.eventDB)).
-define(get_Events(S,M),         cosNotification_eventDB:get_events(S#state.eventDB, M)).
-define(get_RespondTo(S),        S#state.respondTo).
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

%% Data structures modifiers
%% Attributes
-define(set_PrioFil(S,D),        S#state{prioFil=D}).
-define(set_LifeTFil(S,D),       S#state{lifetFil=D}).
%% Client Object
-define(set_Client(S,D),         S#state{client=D}).
-define(del_Client(S),           S#state{client=undefined}).
%% QoS
-define(set_LocalQoS(S,D),       S#state{qosLocal=D}).
-define(set_GlobalQoS(S,D),      S#state{qosGlobal=D}).
-define(set_BothQoS(S,GD,LD),    S#state{qosGlobal=GD, qosLocal=LD}).
-define(update_EventDB(S,Q),     S#state{eventDB=
					 cosNotification_eventDB:update(S#state.eventDB, Q)}).
%% Filters
-define(add_Filter(S,I,O),       S#state{myFilters=[{I,O}|S#state.myFilters]}).
-define(del_Filter(S,I),         S#state{myFilters=
					 delete_obj(lists:keydelete(I, 1, S#state.myFilters),
						    S#state.myFilters)}).
-define(del_AllFilter(S),        S#state{myFilters=[]}).
-define(set_Unconnected(S),      S#state{client=undefined}).
-define(reset_RespondTo(S),      S#state{respondTo=undefined}).
-define(set_RespondTo(S,F),      S#state{respondTo=F}).
%% Event
-define(add_Event(S,E),          catch cosNotification_eventDB:
	add_event(S#state.eventDB, E, S#state.lifetFil, S#state.prioFil)).
-define(addAndGet_Event(S,E),    catch cosNotification_eventDB:
	add_and_get_event(S#state.eventDB, E, S#state.lifetFil, S#state.prioFil)).
%% Admin
-define(set_PacingTimer(S,T),    S#state{pacingTimer=T}).
%% Subscribe
-define(add_Subscribe(S,E),      ets:insert(S#state.etsR, {E, subscribe})).
-define(del_Subscribe(S,E),      ets:delete(S#state.etsR, E)).
-define(set_SubscribeType(S,T),  S#state{subscribeType=T}).
-define(set_SubscribeData(S,D),  S#state{subscribeData=D}).
%% ID
-define(set_IdCounter(S,V),      S#state{idCounter=V}).
-define(new_Id(S),               'CosNotification_Common':create_id(S#state.idCounter)).

%% MISC
-define(is_ANY(S),               S#state.myType == 'PULL_ANY').
-define(is_STRUCTURED(S),        S#state.myType == 'PULL_STRUCTURED').
-define(is_SEQUENCE(S),          S#state.myType == 'PULL_SEQUENCE').
-define(is_ANDOP(S),             S#state.myOperator == 'AND_OP').
-define(is_UnConnected(S),       S#state.client == undefined).
-define(is_Connected(S),         S#state.client =/= undefined).
-define(is_Waiting(S),           S#state.respondTo =/= undefined).
-define(is_SubscribedFor(S,K),   ets:lookup(S#state.etsR, K) =/= []).
-define(is_BatchLimitReached(S,M), cosNotification_eventDB:
	status(S#state.eventDB, {batchLimit, 
				 ?not_GetMaximumBatchSize((S#state.qosLocal)), M})).

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
            ?DBG("PROXYPUSHSUPPLIER: ~p  TERMINATED.~n",[_Reason]),
            {noreply, State};
	{pacing, TS} when ?is_Waiting(State) ->
	    case ?get_PacingTimer(State) of
		{_, TS} ->
		    ?DBG("PULL SUPPLIER PACING LIMIT REACHED~n",[]),
		    {RespondTo, Max} = ?get_RespondTo(State),
		    {EventSeq, _} = ?get_Events(State, Max),
		    corba:reply(RespondTo, EventSeq),
		    {noreply, ?reset_RespondTo(State)};
		_ ->
		    %% Must have been an old timer event, i.e., we reached the
		    %% Batch Limit before Pace limit and we were not able
		    %% to stop the timer before it triggered an event.
		    ?DBG("PULL SUPPLIER OLD PACING LIMIT REACHED~n",[]),
		    {noreply, State}
	    end;
	{pacing, _} ->
	    ?DBG("PULL SUPPLIER PACING LIMIT REACHED BUT NO CLIENT; IMPOSSIBLE!!!~n",[]),
	    {noreply, State};
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
    {ok, ?get_InitState(MyType, MyAdmin, MyAdminPid, InitQoS, LQS, MyChannel, 
			Operator, GCTime, GCLimit, TimeRef)}.

terminate(_Reason, State) when ?is_UnConnected(State) ->
    ok;
terminate(_Reason, State) ->
    Client = ?get_Client(State),
    case catch corba_object:is_nil(Client) of
	false when ?is_ANY(State) ->
	    'CosNotification_Common':disconnect('CosEventComm_PullConsumer', 
						disconnect_pull_consumer,
						Client);
	false when ?is_SEQUENCE(State) ->
	    'CosNotification_Common':disconnect('CosNotifyComm_SequencePullConsumer',
						disconnect_sequence_pull_consumer,
						Client);
	false when ?is_STRUCTURED(State) ->
	    'CosNotification_Common':disconnect('CosNotifyComm_StructuredPullConsumer',
						disconnect_structured_pull_consumer,
						Client);
	_ ->
	    ok
    end.

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
%%----- CosEventChannelAdmin::ProxyPullSupplier -------------
%%----------------------------------------------------------%
%% function : connect_pull_consumer
%% Arguments: Client - CosEventComm::PullConsumer
%% Returns  :  ok | {'EXCEPTION', #'AlreadyConnected'{}} |
%%            {'EXCEPTION', #'TypeError'{}} |
%%            {'EXCEPTION', #'BAD_OPERATION'{}}
%%            Both exceptions from CosEventChannelAdmin!!!
%%-----------------------------------------------------------
connect_pull_consumer(OE_THIS, OE_FROM, State, Client) ->
    connect_any_pull_consumer(OE_THIS, OE_FROM, State, Client).

%%----- CosNotifyChannelAdmin::ProxyPullSupplier ------------
%%----------------------------------------------------------%
%% function : connect_any_pull_consumer
%% Arguments: Client - CosEventComm::PullConsumer
%% Returns  :  ok | {'EXCEPTION', #'AlreadyConnected'{}} |
%%            {'EXCEPTION', #'TypeError'{}} |
%%            {'EXCEPTION', #'BAD_OPERATION'{}}
%%            Both exceptions from CosEventChannelAdmin!!!
%%-----------------------------------------------------------
connect_any_pull_consumer(_OE_THIS, _OE_FROM, State, Client) when ?is_ANY(State) ->
    ?not_TypeCheck(Client, 'CosEventComm_PullConsumer'),
    if
	?is_Connected(State) ->
	    corba:raise(#'CosEventChannelAdmin_AlreadyConnected'{});
	true ->
	    {reply, ok, ?set_Client(State, Client)}
    end;
connect_any_pull_consumer(_, _, _, _) ->
      corba:raise(#'BAD_OPERATION'{completion_status=?COMPLETED_NO}).
  
%%----- CosNotifyChannelAdmin::SequenceProxyPullSupplier ----
%%----------------------------------------------------------%
%% function : connect_sequence_pull_consumer
%% Arguments: Client - CosNotifyComm::SequencePullConsumer
%% Returns  :  ok | {'EXCEPTION', #'AlreadyConnected'{}} |
%%            {'EXCEPTION', #'TypeError'{}} |
%%            {'EXCEPTION', #'BAD_OPERATION'{}}
%%-----------------------------------------------------------
connect_sequence_pull_consumer(_OE_THIS, _OE_FROM, State, Client) when ?is_SEQUENCE(State) ->
    ?not_TypeCheck(Client, 'CosNotifyComm_SequencePullConsumer'),
    if
	?is_Connected(State) ->
	    corba:raise(#'CosEventChannelAdmin_AlreadyConnected'{});
	true ->
	    {reply, ok, ?set_Client(State, Client)}
    end;
connect_sequence_pull_consumer(_, _, _, _) ->
    corba:raise(#'BAD_OPERATION'{completion_status=?COMPLETED_NO}).

%%----- CosNotifyChannelAdmin::StructuredProxyPullSupplier --
%%----------------------------------------------------------%
%% function : connect_structured_pull_consumer
%% Arguments: Client - CosNotifyComm::StructuredPullConsumer
%% Returns  :  ok | {'EXCEPTION', #'AlreadyConnected'{}} |
%%            {'EXCEPTION', #'TypeError'{}} |
%%            {'EXCEPTION', #'BAD_OPERATION'{}}
%%-----------------------------------------------------------
connect_structured_pull_consumer(_OE_THIS, _OE_FROM, State, Client) when ?is_STRUCTURED(State) ->
    ?not_TypeCheck(Client, 'CosNotifyComm_StructuredPullConsumer'),
    if
	?is_Connected(State) ->
	    corba:raise(#'CosEventChannelAdmin_AlreadyConnected'{});
	true ->
	    {reply, ok, ?set_Client(State, Client)}
    end;
connect_structured_pull_consumer(_, _, _, _) ->
    corba:raise(#'BAD_OPERATION'{completion_status=?COMPLETED_NO}).

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
    orber:dbg("[~p] PullerSupplier:obtain_offered_types(~p);~n"
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

%%----- Inherit from CosNotifyComm::NotifySubscribe ---------
%%----------------------------------------------------------%
%% function : subscription_change
%% Arguments: Added - Removed - CosNotification::EventTypeSeq
%% Returns  : ok
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
    orber:dbg("[~p] PullerSupplier:remove_filter(~p); Not an integer", 
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
    orber:dbg("[~p] PullerSupplier:get_filter(~p); Not an integer", 
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

%%----- Inherit from CosEventComm::PullSupplier -------------
%%----------------------------------------------------------%
%% function : disconnect_pull_supplier
%% Arguments: -
%% Returns  : ok
%%-----------------------------------------------------------
disconnect_pull_supplier(_OE_THIS, _OE_FROM, State) ->
    {stop, normal, ok, ?set_Unconnected(State)}.

%%----------------------------------------------------------%
%% function : pull
%% Arguments: -
%% Returns  : any - CORBA::ANY
%%-----------------------------------------------------------
pull(_OE_THIS, OE_FROM, State) when ?is_ANY(State) ->
    case ?get_Event(State) of
	{[], _} ->
	    {noreply, ?set_RespondTo(State, OE_FROM)};
	{Event,_} ->
	    {reply, Event, State}
    end;
pull(_,_,_) ->
    corba:raise(#'BAD_OPERATION'{completion_status=?COMPLETED_NO}).

%%----------------------------------------------------------%
%% function : try_pull
%% Arguments: -
%% Returns  : any - CORBA::ANY
%%            HasEvent - boolean (out-type)
%%-----------------------------------------------------------
try_pull(_OE_THIS, _OE_FROM, State) when ?is_ANY(State) ->
    case ?get_Event(State) of
	{[], _} ->
	    {reply, {any:create(orber_tc:null(), null), false}, State};
	{Event, Bool} ->
	    {reply, {Event, Bool}, State}
    end;
try_pull(_,_,_) ->
    corba:raise(#'BAD_OPERATION'{completion_status=?COMPLETED_NO}).

%%----- Inherit from CosNotifyComm::SequencePullSupplier ----
%%----------------------------------------------------------%
%% function : disconnect_sequence_pull_supplier
%% Arguments: -
%% Returns  : ok
%%-----------------------------------------------------------
disconnect_sequence_pull_supplier(_OE_THIS, _OE_FROM, State) ->
    {stop, normal, ok, ?set_Unconnected(State)}.

%%----------------------------------------------------------%
%% function : pull_structured_events
%% Arguments: Max - long()
%% Returns  : [StructuredEvent, ..]
%%-----------------------------------------------------------
pull_structured_events(_OE_THIS, OE_FROM, State, Max) when ?is_SEQUENCE(State) ->
    case ?is_BatchLimitReached(State, Max) of
	true ->
	    %% This test is not fool-proof; if Events have been stored
	    %% using StartTime they will still be there but we cannot
	    %% deliver them anyway. To solve this "problem" would cost!
	    %% Hence, since it works fine otherwise it will do.
	    case ?get_Events(State, Max) of
		{[], false} ->
		    NewState = start_timer(State),
		    {noreply, ?set_RespondTo(NewState, {OE_FROM, Max})};
		{Event,_} ->
		    {reply, Event, State}
	    end;
	_->
	    NewState = start_timer(State),
	    {noreply, ?set_RespondTo(NewState, {OE_FROM, Max})}
    end;
pull_structured_events(_,_,_,_) ->
    corba:raise(#'BAD_OPERATION'{completion_status=?COMPLETED_NO}).

%%----------------------------------------------------------%
%% function : try_pull_structured_events
%% Arguments: Max - long()
%% Returns  : [StructuredEvent, ..]
%%            HasEvent - Boolean()
%%-----------------------------------------------------------
try_pull_structured_events(_OE_THIS, _OE_FROM, State, Max) when ?is_SEQUENCE(State) ->
    {reply, ?get_Events(State, Max), State};
try_pull_structured_events(_,_,_,_) ->
    corba:raise(#'BAD_OPERATION'{completion_status=?COMPLETED_NO}).

%%----- Inherit from CosNotifyComm::StructuredPullSupplier --
%%----------------------------------------------------------%
%% function : disconnect_structured_pull_supplier
%% Arguments: -
%% Returns  : ok
%%-----------------------------------------------------------
disconnect_structured_pull_supplier(_OE_THIS, _OE_FROM, State) ->
    {stop, normal, ok, ?set_Unconnected(State)}.

%%----------------------------------------------------------%
%% function : pull_structured_event
%% Arguments: -
%% Returns  : 
%%-----------------------------------------------------------
pull_structured_event(_OE_THIS, OE_FROM, State) when ?is_STRUCTURED(State) ->
    case ?get_Event(State) of
	{[], _} ->
	    {noreply, ?set_RespondTo(State, OE_FROM)};
	{Event,_} ->
	    {reply, Event, State}
    end;
pull_structured_event(_,_,_) ->
    corba:raise(#'BAD_OPERATION'{completion_status=?COMPLETED_NO}).

%%----------------------------------------------------------%
%% function : try_pull_structured_event
%% Arguments: -
%% Returns  : 
%%-----------------------------------------------------------
try_pull_structured_event(_OE_THIS, _OE_FROM, State) when ?is_STRUCTURED(State) ->
    case ?get_Event(State) of
	{[], _} ->
	    {reply, 
	     {?not_CreateSE("","","",[],[],any:create(orber_tc:null(), null)), false}, 
	     State};
	{Event, Bool} ->
	    {reply, {Event, Bool}, State}
    end;
try_pull_structured_event(_,_,_) ->
    corba:raise(#'BAD_OPERATION'{completion_status=?COMPLETED_NO}).


%%--------------- LOCAL FUNCTIONS ----------------------------
find_obj({value, {_, Obj}}) -> Obj;
find_obj(_) -> {'EXCEPTION', #'CosNotifyFilter_FilterNotFound'{}}.

find_ids(List) ->           find_ids(List, []).
find_ids([], Acc) ->        Acc;
find_ids([{I,_}|T], Acc) -> find_ids(T, [I|Acc]);
find_ids(_, _) -> corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO}).

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
    %% We should do something here, i.e., see what QoS this Object offers and
    %% act accordingly.
    corba:reply(OE_FROM, ok),
    case cosNotification_eventDB:validate_event(?get_SubscribeData(State), EventsIn,
						?get_AllFilter(State),
						?get_SubscribeDB(State),
						Status) of
	{[], _} ->
	    ?DBG("PROXY NOSUBSCRIPTION SEQUENCE/STRUCTURED: ~p~n",[EventsIn]),
	    {noreply, State};
	%% Just one event and we got a client waiting => there is no need to store
	%% the event, just transform it and pass it on.
	{[Event], _} when ?is_ANY(State), ?is_Waiting(State) ->
	    ?DBG("PROXY RECEIVED SEQUENCE[1]==>ANY: ~p~n",[Event]),
	    AnyEvent = any:create('CosNotification_StructuredEvent':tc(),Event),
	    case ?addAndGet_Event(State, AnyEvent) of
		{[], _} ->
		    ?DBG("PROXY RECEIVED UNDELIVERABLE SEQUENCE[1]: ~p~n",
				 [Event]),
		    %% Cannot deliver the event at the moment; perhaps Starttime
		    %% set or Deadline passed.
		    {noreply, State};
		{PossiblyOtherEvent, _} ->
		    ?DBG("PROXY RECEIVED SEQUENCE[1] ~p; DELIVER: ~p~n",
				 [Event, PossiblyOtherEvent]),
		    corba:reply(?get_RespondTo(State), PossiblyOtherEvent),
		    {noreply, ?reset_RespondTo(State)}
	    end;
	{[Event],_} when ?is_STRUCTURED(State), ?is_Waiting(State) ->
	    case ?addAndGet_Event(State, Event) of
		{[], _} ->
		    ?DBG("PROXY RECEIVED UNDELIVERABLE SEQUENCE[1]: ~p~n",
				 [Event]),
		    %% Cannot deliver the event at the moment; perhaps Starttime
		    %% set or Deadline passed.
		    {noreply, State};
		{PossiblyOtherEvent, _} ->
		    ?DBG("PROXY RECEIVED SEQUENCE[1] ~p; DELIVER: ~p~n",
				 [Event, PossiblyOtherEvent]),
		    corba:reply(?get_RespondTo(State), PossiblyOtherEvent),
		    {noreply, ?reset_RespondTo(State)}
	    end;
	%% A sequence of events => store them and extract the first (according to QoS)
	%% event and forward it.
	{Events,_} when ?is_ANY(State), ?is_Waiting(State) ->
	    ?DBG("PROXY RECEIVED SEQUENCE==>ANY: ~p~n",[Events]),
	    store_events(State, Events),
	    case ?get_Event(State) of
		{[], _} ->
		    {noreply, State};
		{AnyEvent, _} ->
		    corba:reply(?get_RespondTo(State), AnyEvent),
		    {noreply, ?reset_RespondTo(State)}
	    end;
	{Events, _} when ?is_STRUCTURED(State), ?is_Waiting(State) ->
	    ?DBG("PROXY RECEIVED SEQUENCE: ~p~n",[Events]),
	    store_events(State, Events),
	    case ?get_Event(State) of
		{[], _} ->
		    {noreply, State};
		{_StrEvent, _} ->
		    corba:reply(?get_RespondTo(State), Events),
		    {noreply, ?reset_RespondTo(State)}
	    end;
	{Events, _} when ?is_SEQUENCE(State), ?is_Waiting(State) ->
	    ?DBG("PROXY RECEIVED SEQUENCE: ~p~n",[Events]),
	    %% Store them first and extract Max events in QoS order.
	    store_events(State, Events),
	    {RespondTo, Max} = ?get_RespondTo(State),
	    case ?is_BatchLimitReached(State, Max) of
		true ->
		    {EventSeq, _} = ?get_Events(State, Max),
		    corba:reply(RespondTo, EventSeq),
		    stop_timer(State),
		    {noreply, ?reset_RespondTo(State)};
		_->
		   {noreply, State}
	    end;
	%% No client waiting. Store the event(s).
	{Events, _} ->
	    ?DBG("PROXY RECEIVED SEQUENCE: ~p~n",[Events]),
	    store_events(State, Events),
	    {noreply, State}
    end.

store_events(_State, []) ->
    ok;
store_events(State, [Event|Rest]) when ?is_ANY(State) ->
    AnyEvent = any:create('CosNotification_StructuredEvent':tc(),Event),
    ?add_Event(State,AnyEvent),
    store_events(State, Rest);
store_events(State, [Event|Rest]) ->
    ?add_Event(State,Event),
    store_events(State, Rest).

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
	    {noreply, State};
	{Event,_} when ?is_ANY(State), ?is_Waiting(State) ->
	    ?DBG("PROXY RECEIVED ANY: ~p~n",[Event]),
	    case ?addAndGet_Event(State, Event) of
		{[],_} ->
		    %% Unable to deliver the event (Starttime, Deadline etc).
		    {noreply, State};
		{MaybeOtherEvent , _} ->
		    corba:reply(?get_RespondTo(State), MaybeOtherEvent),
		    {noreply, ?reset_RespondTo(State)}
	    end;
	{Event,_} when ?is_ANY(State) ->
	    ?DBG("PROXY RECEIVED ANY: ~p~n",[Event]),
	    ?add_Event(State,Event),
	    {noreply, State};
	{Event,_} when ?is_STRUCTURED(State), ?is_Waiting(State) ->
	    ?DBG("PROXY RECEIVED ANY==>STRUCTURED: ~p~n",[Event]),
	    case ?addAndGet_Event(State, ?not_CreateSE("","%ANY","",[],[],Event)) of
		{[],_} ->
		    %% Unable to deliver the event (Starttime, Deadline etc).
		    {noreply, State};
		{MaybeOtherEvent , _} ->
		    corba:reply(?get_RespondTo(State), MaybeOtherEvent),
		    {noreply, ?reset_RespondTo(State)}
	    end;
	{Event,_} when ?is_SEQUENCE(State), ?is_Waiting(State) ->
	    ?DBG("PROXY RECEIVED ANY==>SEQUENCE[1]: ~p~n",[Event]),
	    ?add_Event(State,?not_CreateSE("","%ANY","",[],[],Event)),
	    {RespondTo, Max} = ?get_RespondTo(State),
	    case ?is_BatchLimitReached(State, Max) of
		true ->
		    {EventSeq, _} = ?get_Events(State, Max),
		    corba:reply(RespondTo, EventSeq),
		    stop_timer(State),
		    {noreply, ?reset_RespondTo(State)};
		_ ->
		    {noreply, State}
	    end;
	{Event,_} ->
	    ?DBG("PROXY RECEIVED ANY==>STRUCTURED/SEQUENCE: ~p~n",[Event]),
	    ?add_Event(State,?not_CreateSE("","%ANY","",[],[],Event)),
	    {noreply, State}
    end.


	    
%% Start timers which send a message each time we should push events. Only used
%% when this objects is defined to supply sequences.
start_timer(State) ->
    TS = now(),
    case catch timer:send_after(timer:seconds(?get_PacingInterval(State)), 
				{pacing, TS}) of
	{ok,PacTRef} ->
	    ?DBG("PULL SUPPLIER STARTED TIMER, BATCH LIMIT: ~p~n",
			 [?get_BatchLimit(State)]),
	    ?set_PacingTimer(State, {PacTRef, TS});
	What ->
	    orber:dbg("[~p] PullerSupplier:start_timer();~n"
		      "Unable to invoke timer:send_interval/2: ~p", 
		      [?LINE, What], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.

stop_timer(State) ->
    case ?get_PacingTimer(State) of
	undefined ->
	    ok;
	{Timer, _} ->
	    ?DBG("PULL SUPPLIER STOPPED TIMER~n",[]),
	    timer:cancel(Timer)
    end.

%%--------------- MISC FUNCTIONS, E.G. DEBUGGING -------------
%%--------------- END OF MODULE ------------------------------
