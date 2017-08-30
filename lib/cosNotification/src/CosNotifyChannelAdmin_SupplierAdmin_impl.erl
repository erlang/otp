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
%%-------------------------------------------------------------------
%% File    : CosNotifyChannelAdmin_SupplierAdmin_impl.erl
%% Purpose : 
%%-------------------------------------------------------------------

-module('CosNotifyChannelAdmin_SupplierAdmin_impl').

%%--------------- INCLUDES -----------------------------------
-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").
%% Application files
-include("CosNotification.hrl").
-include("CosNotifyChannelAdmin.hrl").
-include("CosNotifyComm.hrl").
-include("CosNotifyFilter.hrl").
-include("CosNotification_Definitions.hrl").

%%--------------- EXPORTS ------------------------------------
%%--------------- External -----------------------------------
%%----- CosNotifyChannelAdmin::SupplierAdmin -----------------
-export([get_proxy_consumer/4,
	 obtain_notification_pull_consumer/4, 
	 obtain_notification_push_consumer/4,
	 destroy/3]).

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

%%----- Inherit from CosEventChannelAdmin::SupplierAdmin -----
-export([obtain_push_consumer/3, 
	 obtain_pull_consumer/3]).

%% Attributes (external)
-export(['_get_MyID'/3, 
	 '_get_MyChannel'/3, 
	 '_get_MyOperator'/3,
	 '_get_pull_consumers'/3, 
	 '_get_push_consumers'/3]).

%%--------------- Internal -----------------------------------
%%----- Inherit from cosNotificationComm ---------------------
-export([callAny/5,
	 callSeq/5]).

%%--------------- gen_server specific exports ----------------
-export([handle_info/2, code_change/3]).
-export([init/1, terminate/2]).

%%--------------- LOCAL DEFINITIONS --------------------------
%% Data structures
-record(state, {myId,
	        myChannel,
		myChannelPid,
		myOperator,
		myFilters = [],
		idCounter = 0,
		etsR,
		qosGlobal,
		qosLocal,
		options}).

%% Data structures constructors
-define(get_InitState(_MyID, _MyCh, _MyChP, _MyOp, _QoS, _LQS, _O), 
	#state{myId           = _MyID,
	       myChannel      = _MyCh,
	       myChannelPid   = _MyChP,
	       myOperator     = _MyOp,
	       qosGlobal      = _QoS,
	       qosLocal       = _LQS,
	       options        = _O,
	       etsR = ets:new(oe_ets, [set, protected])}).

%% Data structures selectors
-define(get_PushConsumers(S),     lists:flatten(ets:match(S#state.etsR, 
							  {'_','$1','_',pusher}))).
-define(get_PullConsumers(S),     lists:flatten(ets:match(S#state.etsR, 
							  {'_','$1','_',puller}))).
-define(get_AllConsumers(S),      lists:flatten(ets:match(S#state.etsR, 
							  {'_','$1','_','_'}))).
-define(get_PushConsumerIDs(S),   lists:flatten(ets:match(S#state.etsR, 
							  {'$1','_','_',pusher}))).
-define(get_PullConsumerIDs(S),   lists:flatten(ets:match(S#state.etsR, 
							  {'$1','_','_',puller}))).

-define(get_Consumer(S, I),       find_obj(ets:lookup(S#state.etsR, I), consumer)).

-define(get_MyID(S),              S#state.myId).
-define(get_MyChannel(S),         S#state.myChannel).
-define(get_MyChannelPid(S),      S#state.myChannelPid).
-define(get_MyOperator(S),        S#state.myOperator).
-define(get_GlobalQoS(S),         S#state.qosGlobal).
-define(get_LocalQoS(S),          S#state.qosLocal).
-define(get_BothQoS(S),           {S#state.qosGlobal, S#state.qosLocal}).
-define(get_Filter(S, I),         find_obj(lists:keysearch(I, 1, S#state.myFilters), 
					   filter)).
-define(get_AllFilter(S),         S#state.myFilters).
-define(get_AllFilterID(S),       find_ids(S#state.myFilters)).
-define(get_Options(S),           S#state.options).
-define(get_IdCounter(S),         S#state.idCounter).

%% Data structures modifiers
-define(set_LocalQoS(S,D),        S#state{qosLocal=D}).
-define(set_GlobalQoS(S,D),       S#state{qosGlobal=D}).
-define(set_BothQoS(S,GD,LD),     S#state{qosGlobal=GD, qosLocal=LD}).
-define(add_PushConsumer(S,I,R,P),ets:insert(State#state.etsR, {I,R,P,pusher})).
-define(add_PullConsumer(S,I,R,P),ets:insert(State#state.etsR, {I,R,P,puller})).
-define(del_Consumer(S,I),        ets:delete(S#state.etsR, I)).
-define(del_ConsumerPid(S,P),     ets:match_delete(S#state.etsR, {'_','_',P,'_'})).
-define(add_Filter(S,I,O),        S#state{myFilters=[{I,O}|S#state.myFilters]}).
-define(del_Filter(S,I),          S#state{myFilters=
					  delete_filter(lists:keydelete(I, 1, 
									S#state.myFilters),
							S#state.myFilters)}).
-define(del_AllFilter(S),         S#state{myFilters=[]}).
-define(set_IdCounter(S,V),       S#state{idCounter=V}).
-define(new_Id(S),                'CosNotification_Common':create_id(S#state.idCounter)).

%% MISC
-define(is_PersistentEvent(S),
	?not_GetEventReliability((S#state.qosLocal)) == ?not_Persistent).
-define(is_PersistentConnection(S),
	?not_GetConnectionReliability((S#state.qosLocal)) == ?not_Persistent).

%%----------------------------------------------------------%
%% function : handle_info, code_change
%% Arguments: 
%% Returns  : 
%% Effect   : Functions demanded by the gen_server module. 
%%-----------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(Info, State) ->
    case Info of
        {'EXIT', Pid, Reason} when ?get_MyChannelPid(State) == Pid ->
            ?DBG("PARENT CHANNEL: ~p  TERMINATED.~n",[Reason]),
	    {stop, Reason, State};
        {'EXIT', Pid, normal} ->
	    ?del_ConsumerPid(State, Pid),
            {noreply, State};
        _Other ->
            {noreply, State}
    end.

%%----------------------------------------------------------%
%% function : init, terminate
%% Arguments: 
%%-----------------------------------------------------------

init([MyId, MyChannel, MyChannelPid, MyOperator, InitQoS, LQS, Options]) ->
    process_flag(trap_exit, true),
    {ok, ?get_InitState(MyId, MyChannel, MyChannelPid, MyOperator, InitQoS, LQS, Options)}.

terminate(_Reason, _State) ->
    ok.

%%-----------------------------------------------------------
%%----- CosNotifyChannelAdmin_ConsumerAdmin attributes ------
%%-----------------------------------------------------------
%%----------------------------------------------------------%
%% Attribute: '_get_MyID'
%% Type     : readonly
%% Returns  : 
%%-----------------------------------------------------------
'_get_MyID'(_OE_THIS, _OE_FROM, State) ->
    {reply, ?get_MyID(State), State}.

%%----------------------------------------------------------%
%% Attribute: '_get_MyChannel'
%% Type     : readonly
%% Returns  : 
%%-----------------------------------------------------------
'_get_MyChannel'(_OE_THIS, _OE_FROM, State) ->
    {reply, ?get_MyChannel(State), State}.

%%----------------------------------------------------------%
%% Attribute: '_get_MyOperator'
%% Type     : readonly
%% Returns  : 
%%-----------------------------------------------------------
'_get_MyOperator'(_OE_THIS, _OE_FROM, State) ->
    {reply, ?get_MyOperator(State), State}.

%%----------------------------------------------------------%
%% Attribute: '_get_pull_consumers'
%% Type     : readonly
%% Returns  : ProxyIDSeq
%%-----------------------------------------------------------
'_get_pull_consumers'(_OE_THIS, _OE_FROM, State) ->
    {reply, ?get_PullConsumerIDs(State), State}.

%%----------------------------------------------------------%
%% Attribute: '_get_push_consumers'
%% Type     : readonly
%% Returns  : ProxyIDSeq
%%-----------------------------------------------------------
'_get_push_consumers'(_OE_THIS, _OE_FROM, State) ->
    {reply, ?get_PushConsumerIDs(State), State}.

%%-----------------------------------------------------------
%%------- Exported external functions -----------------------
%%-----------------------------------------------------------
%%----------------------------------------------------------%
%% function : get_proxy_consumer
%% Arguments: ProxyId - unique identifier (long)
%% Returns  : ObjRef | {'EXCEPTION', #'ProxyNotFound'{}}
%%-----------------------------------------------------------
get_proxy_consumer(_OE_THIS, _OE_FROM, State, ProxyId) ->
    {reply, ?get_Consumer(State, ProxyId), State}.    

%%----------------------------------------------------------%
%% function : obtain_notification_pull_consumer
%% Arguments: 
%% Returns  : 
%%-----------------------------------------------------------
obtain_notification_pull_consumer(OE_THIS, _OE_FROM, State, Ctype) ->
    %% Choose which module to use.
    {Mod, Type} = 
	case Ctype of
	    'ANY_EVENT' ->
		{'CosNotifyChannelAdmin_ProxyPullConsumer', 'PULL_ANY'};
	    'STRUCTURED_EVENT' ->
		{'CosNotifyChannelAdmin_StructuredProxyPullConsumer', 'PULL_STRUCTURED'};
	    'SEQUENCE_EVENT' ->
		{'CosNotifyChannelAdmin_SequenceProxyPullConsumer', 'PULL_SEQUENCE'};
	    _ ->
		orber:dbg("[~p] CosNotifyChannelAdmin_SupplierAdmin:"
			  "obtain_notification_pull_consumer(~p);~n"
			  "Incorrect enumerant", 
			  [?LINE, Ctype], ?DEBUG_LEVEL),
		corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})
	end,
    SO = 'CosNotification_Common':get_option(server_options, ?get_Options(State), 
					     ?not_DEFAULT_SETTINGS),
    case catch Mod:oe_create_link([Type, OE_THIS, self(), ?get_GlobalQoS(State), 
				   ?get_LocalQoS(State), ?get_MyChannel(State),
				   ?get_Options(State), ?get_MyOperator(State)], 
				  [{sup_child, true}|SO]) of
	{ok, Pid, Proxy} ->
	    ProxyID = ?new_Id(State),
	    ?add_PullConsumer(State, ProxyID, Proxy, Pid),
	    {reply, {Proxy, ProxyID}, ?set_IdCounter(State, ProxyID)};
	What ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_SupplierAdmin:"
		      "obtain_notification_pull_consumer();~n"
		      "Unable to create: ~p/~p~n"
		      "Reason: ~p", [?LINE, Mod, Type, What], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.

%%----------------------------------------------------------%
%% function : obtain_notification_push_supplier
%% Arguments: 
%% Returns  : 
%%-----------------------------------------------------------
obtain_notification_push_consumer(OE_THIS, _OE_FROM, State, Ctype) ->
    %% Choose which module to use.
    {Mod, Type} = 
	case Ctype of
	    'ANY_EVENT' ->
		{'CosNotifyChannelAdmin_ProxyPushConsumer', 'PUSH_ANY'};
	    'STRUCTURED_EVENT' ->
		{'CosNotifyChannelAdmin_StructuredProxyPushConsumer', 'PUSH_STRUCTURED'};
	    'SEQUENCE_EVENT' ->
		{'CosNotifyChannelAdmin_SequenceProxyPushConsumer', 'PUSH_SEQUENCE'};
	    _ ->
		orber:dbg("[~p] CosNotifyChannelAdmin_SupplierAdmin:"
			  "obtain_notification_push_consumer(~p);~n"
			  "Incorrect enumerant", [?LINE, Ctype], ?DEBUG_LEVEL),
		corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})
	end,
    SO = 'CosNotification_Common':get_option(server_options, ?get_Options(State), 
					     ?not_DEFAULT_SETTINGS),
    case catch Mod:oe_create_link([Type, OE_THIS, self(), ?get_GlobalQoS(State), 
				   ?get_LocalQoS(State), ?get_MyChannel(State),
				   ?get_Options(State), ?get_MyOperator(State)], 
				  [{sup_child, true}|SO]) of
	{ok, Pid, Proxy} ->
	    ProxyID = ?new_Id(State),
	    ?add_PushConsumer(State, ProxyID, Proxy, Pid),
	    {reply, {Proxy, ProxyID}, ?set_IdCounter(State, ProxyID)};
	What ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_SupplierAdmin:"
		      "obtain_notification_push_consumer();~n"
		      "Unable to create: ~p/~p~n"
		      "Reason: ~p", [?LINE, Mod, Type, What], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.

%%----------------------------------------------------------%
%% function : destroy
%% Arguments: -
%% Returns  : ok
%%------------------------------------------------------------
destroy(_OE_THIS, _OE_FROM, State) ->
    {stop, normal, ok, State}.    

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
						     admin, ?get_MyChannel(State),
						     ?get_AllConsumers(State)),
    {reply, ok, ?set_BothQoS(State, NewQoS, LQS)}.

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
						admin, ?get_MyChannel(State), 
						?get_AllConsumers(State)),
    {reply, {ok, QoS}, State}.

%%----- Inherit from CosNotifyComm::NotifyPublish -----------
%%----------------------------------------------------------*
%% function : offer_change
%% Arguments: 
%% Returns  : 
%%-----------------------------------------------------------
offer_change(_OE_THIS, _OE_FROM, State, _Added, _Removed) ->
    {reply, ok, State}.

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
    orber:dbg("[~p] CosNotifyChannelAdmin_SupplierAdmin:remove_filter(~p);~n"
	      "Not an integer", [?LINE, What], ?DEBUG_LEVEL),
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
    orber:dbg("[~p] CosNotifyChannelAdmin_SupplierAdmin:get_filter(~p);~n"
	      "Not an integer", [?LINE, What], ?DEBUG_LEVEL),
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

%%----- Inherit from CosEventChannelAdmin::SupplierAdmin ----
%%----------------------------------------------------------%
%% function : obtain_push_consumer
%% Arguments: -
%% Returns  : ProxyPushConsumer
%%-----------------------------------------------------------
obtain_push_consumer(OE_THIS, _OE_FROM, State) ->
    SO = 'CosNotification_Common':get_option(server_options, ?get_Options(State), 
					     ?not_DEFAULT_SETTINGS),
    case catch 'CosNotifyChannelAdmin_ProxyPushConsumer':
	oe_create_link(['PUSH_ANY', OE_THIS, self(), ?get_GlobalQoS(State), 
			?get_LocalQoS(State), ?get_MyChannel(State),
			?get_Options(State), ?get_MyOperator(State)], 
		       [{sup_child, true}|SO]) of
	{ok, Pid, Proxy} ->
	    ProxyID = ?new_Id(State),
	    ?add_PushConsumer(State, ProxyID, Proxy, Pid),
	    {reply, Proxy, ?set_IdCounter(State, ProxyID)};
	What ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_SupplierAdmin:obtain_push_consumer();~n"
		      "Unable to create: CosNotifyChannelAdmin_ProxyPushConsumer~n"
		      "Reason: ~p", [?LINE, What], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.

%%----------------------------------------------------------%
%% function : obtain_pull_consumer
%% Arguments: -
%% Returns  : ProxyPullConsumer
%%-----------------------------------------------------------
obtain_pull_consumer(OE_THIS, _OE_FROM, State) ->
    SO = 'CosNotification_Common':get_option(server_options, ?get_Options(State), 
					     ?not_DEFAULT_SETTINGS),
    case catch 'CosNotifyChannelAdmin_ProxyPullConsumer':
	oe_create_link(['PULL_ANY', OE_THIS, self(), ?get_GlobalQoS(State), 
			?get_LocalQoS(State), ?get_MyChannel(State),
			?get_Options(State), ?get_MyOperator(State)], 
		       [{sup_child, true}|SO]) of
	{ok, Pid, Proxy} ->
	    ProxyID = ?new_Id(State),
	    ?add_PullConsumer(State, ProxyID, Proxy, Pid),
	    {reply, Proxy, ?set_IdCounter(State, ProxyID)};
	What ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_SupplierAdmin:obtain_push_consumer();~n"
		      "Unable to create: CosNotifyChannelAdmin_ProxyPullConsumer~n"
		      "Reason: ~p", [?LINE, What], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.

%%--------------- LOCAL FUNCTIONS ----------------------------
find_obj([], consumer) -> {'EXCEPTION', #'CosNotifyChannelAdmin_ProxyNotFound'{}};
find_obj([], filter) -> {'EXCEPTION', #'CosNotifyFilter_FilterNotFound'{}};
%% To match consumers
find_obj([{_,Obj,_,_}],_) -> Obj;
%% To match filters
find_obj({value, {_,Obj}},_) -> Obj;
find_obj(_,consumer) -> {'EXCEPTION', #'CosNotifyChannelAdmin_ProxyNotFound'{}};
find_obj(_,filter) -> {'EXCEPTION', #'CosNotifyFilter_FilterNotFound'{}}.

find_ids(List) ->           find_ids(List, []).
find_ids([], Acc) ->        Acc;
find_ids([{I,_}|T], Acc) -> find_ids(T, [I|Acc]);
find_ids(What, _) ->     
    orber:dbg("[~p] CosNotifyChannelAdmin_SupplierAdmin:find_ids();~n"
	      "Id corrupt: ~p", [?LINE, What], ?DEBUG_LEVEL),
    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO}).

%% Delete a single filter.
%% The list do not differ, i.e., no filter removed, raise exception.
delete_filter(List,List) -> corba:raise(#'CosNotifyFilter_FilterNotFound'{});
delete_filter(List, _) -> List.

%%-----------------------------------------------------------
%% function : callSeq
%% Arguments: 
%% Returns  : 
%%-----------------------------------------------------------
callSeq(_OE_THIS, OE_FROM, State, Events, _Status) ->
    corba:reply(OE_FROM, ok),
    case cosNotification_eventDB:filter_events(Events, ?get_AllFilter(State)) of
	{[], _} ->
	    {noreply, State};
	{Passed, _} ->
	    forward(seq, State, Passed, 'MATCHED')
    end.

%%-----------------------------------------------------------
%% function : callAny
%% Arguments: 
%% Returns  : 
%%-----------------------------------------------------------
callAny(_OE_THIS, OE_FROM, State, Event, _Status) ->
    corba:reply(OE_FROM, ok),
    case cosNotification_eventDB:filter_events([Event], ?get_AllFilter(State)) of
	{[], _} ->
	    {noreply, State};
	{[Passed], _} ->
	    forward(any, State, Passed, 'MATCHED')
    end.

%% Forward events
forward(any, State, Event, Status) ->
    case catch oe_CosNotificationComm_Event:callAny(?get_MyChannel(State), 
						    Event, Status) of
	ok ->
	    ?DBG("SUPPLIERADM FORWARD ANY: ~p~n",[Event]),
	    {noreply, State};
	{'EXCEPTION', E} when is_record(E, 'OBJECT_NOT_EXIST') ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_SupplierAdmin:forward();~n"
		      "Channel no longer exists; terminating and dropping: ~p", 
		      [?LINE, Event], ?DEBUG_LEVEL),
	    {stop, normal, State};
	R  when ?is_PersistentConnection(State) ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_SupplierAdmin:forward();~n"
		      "Channel respond incorrect: ~p~n"
		      "Dropping: ~p", [?LINE, R, Event], ?DEBUG_LEVEL),
	    {noreply, State};
	R ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_SupplierAdmin:forward();~n"
		      "Channel respond incorrect: ~p~n"
		      "Terminating and dropping: ~p", 
		      [?LINE, R, Event], ?DEBUG_LEVEL),
	    {stop, normal, State}
    end;
forward(seq, State, Event, Status) ->
    case catch oe_CosNotificationComm_Event:callSeq(?get_MyChannel(State), 
						    Event, Status) of
	ok ->
	    ?DBG("SUPPLIERADM FORWARD SEQUENCE: ~p~n",[Event]),
	    {noreply, State};
	{'EXCEPTION', E} when is_record(E, 'OBJECT_NOT_EXIST') ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_SupplierAdmin:forward();~n"
		      "Channel no longer exists; terminating and dropping: ~p", 
		      [?LINE, Event], ?DEBUG_LEVEL),
	    {stop, normal, State};
	R  when ?is_PersistentConnection(State) ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_SupplierAdmin:forward();~n"
		      "Channel respond incorrect: ~p~n"
		      "Dropping: ~p", [?LINE, R, Event], ?DEBUG_LEVEL),
	    {noreply, State};
	R ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_SupplierAdmin:forward();~n"
		      "Channel respond incorrect: ~p~n"
		      "Terminating and dropping: ~p", 
		      [?LINE, R, Event], ?DEBUG_LEVEL),
	    {stop, normal, State}
    end.


%%--------------- MISC FUNCTIONS, E.G. DEBUGGING -------------
%%--------------- END OF MODULE ------------------------------
