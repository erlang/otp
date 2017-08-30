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
%% File    : CosNotifyChannelAdmin_ConsumerAdmin_impl.erl
%% Purpose : 
%%-------------------------------------------------------------------

-module('CosNotifyChannelAdmin_ConsumerAdmin_impl').

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
%%----- CosNotifyChannelAdmin::ConsumerAdmin -----------------
-export([get_proxy_supplier/4, 
	 obtain_notification_pull_supplier/4, 
	 obtain_notification_push_supplier/4,
	 destroy/3]).

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

%%----- Inherit from CosEventChannelAdmin::ConsumerAdmin -----
-export([obtain_push_supplier/3, 
	 obtain_pull_supplier/3]).

%% Attributes (external)
-export(['_get_MyID'/3, 
	 '_get_MyChannel'/3, 
	 '_get_MyOperator'/3,
	 '_get_priority_filter'/3, 
	 '_set_priority_filter'/4, 
	 '_get_lifetime_filter'/3,
	 '_set_lifetime_filter'/4, 
	 '_get_pull_suppliers'/3, 
	 '_get_push_suppliers'/3]).

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
		mySuppliers = [],
		idCounter = 0,
		priorityFilter,
		lifetimeFilter,
		etsR,
		qosGlobal,
		qosLocal,
		options}).

%% Data structures constructors
-define(get_InitState(_MyID, _MyCh, _MyChP, _MyOp, _PFil, _LFil, _QoS, _LQS, _O), 
	#state{myId           = _MyID,
	       myChannel      = _MyCh,
	       myChannelPid   = _MyChP,
	       myOperator     = _MyOp,
	       priorityFilter = _PFil,
	       lifetimeFilter = _LFil,
	       qosGlobal      = _QoS,
	       qosLocal       = _LQS,
	       options        = _O,
	       etsR = ets:new(oe_ets, [set, protected])}).

%% Data structures selectors
-define(get_PushSupplierIDs(S),   find_ids(S#state.mySuppliers, pusher)).
-define(get_PullSupplierIDs(S),   find_ids(S#state.mySuppliers, puller)).
-define(get_AllSuppliers(S),      S#state.mySuppliers).
-define(get_AllSupplierRefs(S),   find_refs(S#state.mySuppliers)).
-define(get_Supplier(S, I),       find_obj(lists:keysearch(I,1,S#state.mySuppliers), 
					   supplier)).

-define(get_MyID(S),              S#state.myId).
-define(get_MyChannel(S),         S#state.myChannel).
-define(get_MyChannelPid(S),      S#state.myChannelPid).
-define(get_MyOperator(S),        S#state.myOperator).
-define(get_PrioFilter(S),        S#state.priorityFilter).
-define(get_LifeFilter(S),        S#state.lifetimeFilter).
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
-define(set_PrioFilter(S,D),      S#state{priorityFilter=D}).
-define(set_LifeFilter(S,D),      S#state{lifetimeFilter=D}).
-define(set_LocalQoS(S,D),        S#state{qosLocal=D}).
-define(set_GlobalQoS(S,D),       S#state{qosGlobal=D}).
-define(set_BothQoS(S,GD,LD),     S#state{qosGlobal=GD, qosLocal=LD}).
-define(add_PushSupplier(S,I,R,P),S#state{mySuppliers=[{I,R,P,pusher}|S#state.mySuppliers]}).
-define(add_PullSupplier(S,I,R,P),S#state{mySuppliers=[{I,R,P,puller}|S#state.mySuppliers]}).
-define(del_Supplier(S,I),        S#state{mySuppliers=
					  lists:keydelete(I, 1, 
							  S#state.mySuppliers)}).
-define(del_SupplierRef(S,O),     S#state{mySuppliers=
					  lists:keydelete(O, 2, 
							  S#state.mySuppliers)}).
-define(del_SupplierPid(S,P),     S#state{mySuppliers=
					  lists:keydelete(P, 3, 
							  S#state.mySuppliers)}).
-define(add_Filter(S,I,O),        S#state{myFilters=[{I,O}|S#state.myFilters]}).
-define(del_Filter(S,I),          S#state{myFilters=
					  delete_filter(lists:keydelete(I, 1, 
									S#state.myFilters),
							S#state.myFilters)}).
-define(del_AllFilter(S),         S#state{myFilters=[]}).
-define(set_IdCounter(S,V),       S#state{idCounter=V}).
-define(new_Id(S),               'CosNotification_Common':create_id(
							   S#state.idCounter)).

%% MISC
-define(is_PersistentConnection(S),
	?not_GetConnectionReliability((S#state.qosLocal)) == ?not_Persistent).
-define(is_PersistentEvent(S),
	?not_GetEventReliability((S#state.qosLocal)) == ?not_Persistent).
-define(is_ANDOP(S),              S#state.myOperator == 'AND_OP').

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
            {noreply, ?del_SupplierPid(State, Pid)};
        _Other ->
            {noreply, State}
    end.

%%----------------------------------------------------------%
%% function : init, terminate
%% Arguments: 
%%-----------------------------------------------------------

init([MyId, MyChannel, MyChannelPid, MyOperator, InitQoS, LQS, Options]) ->
    process_flag(trap_exit, true),
    PriorityFilter = corba:create_nil_objref(),
    LifeTimeFilter = corba:create_nil_objref(),
    {ok, ?get_InitState(MyId, MyChannel, MyChannelPid, MyOperator, 
			PriorityFilter, LifeTimeFilter, InitQoS, LQS, Options)}.

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
%% Attribute: '_get_priority_filter'
%% Type     : read and write
%% Returns  : 
%%-----------------------------------------------------------
'_get_priority_filter'(_OE_THIS, _OE_FROM, State) ->
    {reply, ?get_PrioFilter(State), State}.

'_set_priority_filter'(_OE_THIS, _OE_FROM, State, PrioFilter) ->
    {reply, ok, ?set_PrioFilter(State, PrioFilter)}.

%%----------------------------------------------------------%
%% Attribute: '_get_lifetime_filter'
%% Type     : read and write
%% Returns  : 
%%-----------------------------------------------------------
'_get_lifetime_filter'(_OE_THIS, _OE_FROM, State) ->
    {reply, ?get_LifeFilter(State), State}.
'_set_lifetime_filter'(_OE_THIS, _OE_FROM, State, LifeFilter) ->
    {reply, ok, ?set_LifeFilter(State, LifeFilter)}.

%%----------------------------------------------------------%
%% Attribute: '_get_pull_suppliers'
%% Type     : readonly
%% Returns  : ProxyIDSeq
%%-----------------------------------------------------------
'_get_pull_suppliers'(_OE_THIS, _OE_FROM, State) ->
    {reply, ?get_PullSupplierIDs(State), State}.

%%----------------------------------------------------------%
%% Attribute: '_get_push_suppliers'
%% Type     : readonly
%% Returns  : ProxyIDSeq
%%-----------------------------------------------------------
'_get_push_suppliers'(_OE_THIS, _OE_FROM, State) ->
    {reply, ?get_PushSupplierIDs(State), State}.

%%-----------------------------------------------------------
%%------- Exported external functions -----------------------
%%-----------------------------------------------------------
%%----------------------------------------------------------%
%% function : get_proxy_supplier
%% Arguments: ProxyID - unique identifier (long)
%% Returns  : ObjRef | {'EXCEPTION', #'ProxyNotFound'{}}
%%-----------------------------------------------------------
get_proxy_supplier(_OE_THIS, _OE_FROM, State, Proxy_id) ->
    {reply, ?get_Supplier(State, Proxy_id), State}.    

%%----------------------------------------------------------%
%% function : obtain_notification_pull_supplier
%% Arguments: Ctype - 'ANY_EVENT' | 'STRUCTURED_EVENT' | 'SEQUENCE_EVENT'
%% Returns  : A Proxy of the requested type.
%%-----------------------------------------------------------
obtain_notification_pull_supplier(OE_THIS, _OE_FROM, State, Ctype) ->
    %% Choose which module to use.
    {Mod, Type} = 
	case Ctype of
	    'ANY_EVENT' ->
		{'CosNotifyChannelAdmin_ProxyPullSupplier', 'PULL_ANY'};
	    'STRUCTURED_EVENT' ->
		{'CosNotifyChannelAdmin_StructuredProxyPullSupplier', 'PULL_STRUCTURED'};
	    'SEQUENCE_EVENT' ->
		{'CosNotifyChannelAdmin_SequenceProxyPullSupplier', 'PULL_SEQUENCE'};
	    _ ->
		orber:dbg("[~p] CosNotifyChannelAdmin_ConsumerAdmin:"
			  "obtain_notification_pull_supplier(~p);~n"
			  "Incorrect enumerant", 
			  [?LINE, Ctype], ?DEBUG_LEVEL),
		corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})
	end,
    SO = 'CosNotification_Common':get_option(server_options, ?get_Options(State), 
					     ?not_DEFAULT_SETTINGS),
    case Mod:oe_create_link([Type, OE_THIS, self(), ?get_GlobalQoS(State), 
			     ?get_LocalQoS(State), ?get_MyChannel(State),
			     ?get_Options(State), ?get_MyOperator(State)], 
			    [{sup_child, true}|SO]) of
	{ok, Pid, PrRef} ->
	    ProxyID = ?new_Id(State),
	    NewState = ?add_PullSupplier(State, ProxyID, PrRef, Pid),
	    {reply, {PrRef, ProxyID},  ?set_IdCounter(NewState, ProxyID)};
	What ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_ConsumerAdmin:"
		      "obtain_notification_pull_supplier();~n"
		      "Unable to create: ~p/~p~n"
		      "Reason: ~p", [?LINE, Mod, Type, What], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.

%%----------------------------------------------------------%
%% function : obtain_notification_push_supplier
%% Arguments: Ctype - 'ANY_EVENT' | 'STRUCTURED_EVENT' | 'SEQUENCE_EVENT'
%% Returns  : A Proxy of the requested type.
%%-----------------------------------------------------------
obtain_notification_push_supplier(OE_THIS, _OE_FROM, State, Ctype) ->
    %% Choose which module to use.
    {Mod, Type} = 
	case Ctype of
	    'ANY_EVENT' ->
		{'CosNotifyChannelAdmin_ProxyPushSupplier', 'PUSH_ANY'};
	    'STRUCTURED_EVENT' ->
		{'CosNotifyChannelAdmin_StructuredProxyPushSupplier', 'PUSH_STRUCTURED'};
	    'SEQUENCE_EVENT' ->
		{'CosNotifyChannelAdmin_SequenceProxyPushSupplier', 'PUSH_SEQUENCE'};
	    _ ->
		orber:dbg("[~p] CosNotifyChannelAdmin_ConsumerAdmin:"
			  "obtain_notification_push_supplier(~p);~n"
			  "Incorrect enumerant", [?LINE, Ctype], ?DEBUG_LEVEL),
		corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})
	end,
    SO = 'CosNotification_Common':get_option(server_options, ?get_Options(State), 
					     ?not_DEFAULT_SETTINGS),
    case Mod:oe_create_link([Type, OE_THIS, self(), ?get_GlobalQoS(State), 
			     ?get_LocalQoS(State), ?get_MyChannel(State),
			     ?get_Options(State), ?get_MyOperator(State)], 
			    [{sup_child, true}|SO]) of
	{ok, Pid, PrRef} ->
	    ProxyID = ?new_Id(State),
	    NewState = ?add_PushSupplier(State, ProxyID, PrRef, Pid),
	    {reply, {PrRef, ProxyID}, ?set_IdCounter(NewState, ProxyID)};
	What ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_ConsumerAdmin:obtain_notification_push_supplier();~n"
		      "Unable to create: ~p/~p~n"
		      "Reason: ~p", 
		      [?LINE, Mod, Type, What], ?DEBUG_LEVEL),
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
						     ?get_AllSupplierRefs(State)),
    {reply, ok, ?set_BothQoS(State, NewQoS, LQS)}.

%%----------------------------------------------------------%
%% function : validate_qos
%% Arguments: Required_qos - CosNotification::QoSProperties
%%            [#'Property'{name, value}, ...] where name eq. string()
%%            and value eq. any().
%% Returns  : {'EXCEPTION', CosNotification::UnsupportedQoS}
%%-----------------------------------------------------------
validate_qos(_OE_THIS, _OE_FROM, State, Required_qos) ->
    QoS = 'CosNotification_Common':validate_qos(Required_qos, ?get_BothQoS(State), 
						admin, ?get_MyChannel(State), 
						?get_AllSupplierRefs(State)),
    {reply, {ok, QoS}, State}.

%%----- Inherit from CosNotifyComm::NotifySubscribe ---------
%%----------------------------------------------------------*
%% function : subscription_change
%% Arguments: 
%% Returns  : ok | 
%%            {'EXCEPTION', #'CosNotifyComm_InvalidEventType'{type}}
%%-----------------------------------------------------------
subscription_change(_OE_THIS, _OE_FROM, State, _Added, _Removed) ->
    ?DBG("CALLBACK INFORMED:  ~p   ~p~n",[_Added, _Removed]),
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
remove_filter(_,_,_,_) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%----------------------------------------------------------%
%% function : get_filter
%% Arguments: FilterID - long
%% Returns  : Filter - CosNotifyFilter::Filter |
%%            {'EXCEPTION', #'CosNotifyFilter_FilterNotFound'{}}
%%-----------------------------------------------------------
get_filter(_OE_THIS, _OE_FROM, State, FilterID) when is_integer(FilterID) ->
    {reply, ?get_Filter(State, FilterID), State};
get_filter(_,_,_,_) ->
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

%%----- Inherit from CosEventChannelAdmin::ConsumerAdmin ----
%%----------------------------------------------------------%
%% function : obtain_push_supplier
%% Arguments: -
%% Returns  : ProxyPushSupplier
%%-----------------------------------------------------------
obtain_push_supplier(OE_THIS, _OE_FROM, State) ->
    SO = 'CosNotification_Common':get_option(server_options, ?get_Options(State), 
					     ?not_DEFAULT_SETTINGS),
    case 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_create_link(['PUSH_ANY', OE_THIS, 
								   self(), 
								   ?get_GlobalQoS(State), 
								   ?get_LocalQoS(State),
								   ?get_MyChannel(State),
								   ?get_Options(State),
								   ?get_MyOperator(State)], 
								  [{sup_child, true}|SO]) of
	{ok, Pid, PrRef} ->
	    ProxyID = ?new_Id(State),
	    NewState = ?add_PushSupplier(State, ProxyID, PrRef, Pid),
	    {reply, PrRef, ?set_IdCounter(NewState, ProxyID)};
	What ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_ConsumerAdmin:obtain_push_supplier();~n"
		      "Unable to create: CosNotifyChannelAdmin_ProxyPushSupplier~n"
		      "Reason: ~p", [?LINE, What], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.

%%----------------------------------------------------------%
%% function : obtain_pull_supplier
%% Arguments: -
%% Returns  : ProxyPullSupplier
%%-----------------------------------------------------------
obtain_pull_supplier(OE_THIS, _OE_FROM, State) ->
    SO = 'CosNotification_Common':get_option(server_options, ?get_Options(State), 
					     ?not_DEFAULT_SETTINGS),
    case 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_create_link(['PULL_ANY', OE_THIS, 
								   self(), 
								   ?get_GlobalQoS(State), 
								   ?get_LocalQoS(State),
								   ?get_MyChannel(State),
								   ?get_Options(State), 
								   ?get_MyOperator(State)], 
								  [{sup_child, true}|SO]) of
	{ok, Pid, PrRef} ->
	    ProxyID = ?new_Id(State),
	    NewState = ?add_PullSupplier(State, ProxyID, PrRef, Pid),
	    {reply, PrRef, ?set_IdCounter(NewState, ProxyID)};
	What ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_ConsumerAdmin:obtain_pull_supplier();~n"
		      "Unable to create: CosNotifyChannelAdmin_ProxyPullSupplier~n"
		      "Reason: ~p", [?LINE, What], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.

%%--------------- LOCAL FUNCTIONS ----------------------------
%% To match suppliers
find_obj({value, {_,Obj,_,_}},_) -> Obj;
%% To match filters
find_obj({value, {_,Obj}},_) -> Obj;
find_obj(_, supplier) -> {'EXCEPTION', #'CosNotifyChannelAdmin_ProxyNotFound'{}};
find_obj(_, filter) -> {'EXCEPTION', #'CosNotifyFilter_FilterNotFound'{}}.

find_ids(List) ->              
    find_ids(List, [], false).
find_ids(List, Type) ->        
    find_ids(List, [], Type).

find_ids([], Acc, _) ->        
    Acc;
find_ids([{I,_}|T], Acc, Type) -> 
    find_ids(T, [I|Acc], Type);
find_ids([{I,_,_,Type}|T], Acc, Type) -> 
    find_ids(T, [I|Acc], Type);
find_ids([{_I,_,_,_}|T], Acc, Type) -> 
    find_ids(T, Acc, Type);
find_ids(What, _, _) -> 
    orber:dbg("[~p] CosNotifyChannelAdmin_ConsumerAdmin:find_ids();~n"
	      "Id corrupt: ~p", [?LINE, What], ?DEBUG_LEVEL),
    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO}).

find_refs(List) ->              
    find_refs(List, []).

find_refs([], Acc) ->      
    Acc;
find_refs([{_,R,_,_}|T], Acc) -> 
    find_refs(T, [R|Acc]);
find_refs(What, _) -> 
    orber:dbg("[~p] CosNotifyChannelAdmin_ConsumerAdmin:find_refs();~n"
	      "Reference corrupt: ~p", [?LINE, What], ?DEBUG_LEVEL),
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
	{[], _}  when ?is_ANDOP(State) ->
	    %% Since AND it doesn't matter what the proxies 'think'. Done.
	    {noreply, State};
	{[], Failed} ->
	    %% Is OR but the Proxy may allow the events; pass on.
	    forward(seq, State, Failed, 'MATCH');
	{Passed, _}  when ?is_ANDOP(State) ->
	    %% Since AND we only forward those who passed this objects filters.
	    forward(seq, State, Passed, 'MATCH');
	{Passed, []} ->
	    %% Since OR we forward and tell the proxy to do no filtering.
	    forward(seq, State, Passed, 'MATCHED');
	{Passed, Failed} ->
	    %% Since OR we forward both and instruct the proxy to check only
	    %% the ones that failed.
	    forward(seq, State, Passed, 'MATCHED'),
	    forward(seq, State, Failed, 'MATCH')
    end.


%%-----------------------------------------------------------
%% function : callAny
%% Arguments: 
%% Returns  : 
%%-----------------------------------------------------------
callAny(_OE_THIS, OE_FROM, State, Event, _Status) ->
    corba:reply(OE_FROM, ok),
    case cosNotification_eventDB:filter_events([Event], ?get_AllFilter(State)) of
	{[], _}  when ?is_ANDOP(State) ->
	    %% Since AND it doesn't matter what the proxies 'think'. Done.
	    {noreply, State};
	{[], [Failed]} ->
	    %% Is OR but the Proxy may allow the event; pass on.
	    forward(any, State, Failed, 'MATCH');
	{[Passed], _}  when ?is_ANDOP(State) ->
	    %% Since AND we only forward those who passed this objects filters.
	    forward(any, State, Passed, 'MATCH');
	{[Passed], _} ->
	    %% Since OR we forward and instruct the proxy to do no checks.
	    forward(any, State, Passed, 'MATCHED')
    end.
 
    

%% Forward events
forward(Type, State, Event, Status) ->
    forward(Type, ?get_AllSuppliers(State), State, Event, Status).
forward(_, [], State, _, _) ->
    {noreply, State};
forward(any, [{_,H,_,_}|T], State, Event, Status) ->
    case catch oe_CosNotificationComm_Event:callAny(H, Event, Status) of
	ok ->
	    ?DBG("CONSUMERADM FORWARD ANY: ~p~n",[Event]),
	    forward(any, T, State, Event, Status);
	{'EXCEPTION', E} when is_record(E, 'OBJECT_NOT_EXIST') ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_ConsumerAdmin:forward();~n"
		      "Proxy no longer exists; dropping it: ~p", 
		      [?LINE, H], ?DEBUG_LEVEL),
	    NewState = ?del_SupplierRef(State,H),
	    forward(any, T, NewState, Event, Status);
	R when ?is_PersistentConnection(State) ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_ConsumerAdmin:forward();~n"
		      "Proxy behaves badly: ~p/~p", 
		      [?LINE, R, H], ?DEBUG_LEVEL),
	    forward(any, T, State, Event, Status);
	R ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_ConsumerAdmin:forward();~n"
		      "Proxy behaves badly: ~p~n"
		      "Dropping it: ~p", [?LINE, R, H], ?DEBUG_LEVEL),
	    NewState = ?del_SupplierRef(State, H),
	    forward(any, T, NewState, Event, Status)
    end;
forward(seq, [{_,H,_,_}|T], State, Event, Status) ->
    case catch oe_CosNotificationComm_Event:callSeq(H, Event, Status) of
	ok ->
	    ?DBG("CONSUMERADM FORWARD SEQUENCE: ~p~n",[Event]),
	    forward(seq, T, State, Event, Status);
	{'EXCEPTION', E} when is_record(E, 'OBJECT_NOT_EXIST') ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_ConsumerAdmin:forward();~n"
		      "Proxy no longer exists; dropping it: ~p", 
		      [?LINE, H], ?DEBUG_LEVEL),
	    NewState = ?del_SupplierRef(State,H),
	    forward(seq, T, NewState, Event, Status);
	R when ?is_PersistentConnection(State) ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_ConsumerAdmin:forward();~n"
		      "Proxy behaves badly: ~p/~p", [?LINE, R, H], ?DEBUG_LEVEL),
	    forward(seq, T, State, Event, Status);
	R ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_ConsumerAdmin:forward();~n"
		      "Proxy behaves badly: ~p~n"
		      "Dropping it: ~p", [?LINE, R, H], ?DEBUG_LEVEL),
	    NewState = ?del_SupplierRef(State, H),
	    forward(seq, T, NewState, Event, Status)
    end.


%%--------------- MISC FUNCTIONS, E.G. DEBUGGING -------------
%%--------------- END OF MODULE ------------------------------
