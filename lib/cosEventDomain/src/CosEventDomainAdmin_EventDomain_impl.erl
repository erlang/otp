%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%% File        : CosEventDomainAdmin_EventDomain_impl.erl
%% Description : 
%%
%%----------------------------------------------------------------------
-module('CosEventDomainAdmin_EventDomain_impl').

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").
-include_lib("cosNotification/include/CosNotifyChannelAdmin.hrl").
-include_lib("cosNotification/include/CosNotification.hrl").

-include("cosEventDomainApp.hrl").
-include("CosEventDomainAdmin.hrl").

%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([init/1,
	 terminate/2,
	 code_change/3,
	 handle_info/2]).

%%------------------ CosEventDomainAdmin::EventDomain ------------------
-export([add_channel/3, 
	 get_all_channels/2, 
	 get_channel/3,
	 remove_channel/3,
	 add_connection/3,
	 get_all_connections/2,
	 get_connection/3,
	 remove_connection/3,
	 get_offer_channels/3,
	 get_subscription_channels/3,
	 destroy/2,
	 get_cycles/2,
	 get_diamonds/2,
	 set_default_consumer_channel/3,
	 set_default_supplier_channel/3,
	 connect_push_consumer/3,
	 connect_pull_consumer/3,
	 connect_push_supplier/3,
	 connect_pull_supplier/3,
	 connect_structured_push_consumer/3,
	 connect_structured_pull_consumer/3,
	 connect_structured_push_supplier/3,
	 connect_structured_pull_supplier/3,
	 connect_sequence_push_consumer/3,
	 connect_sequence_pull_consumer/3,
	 connect_sequence_push_supplier/3,
	 connect_sequence_pull_supplier/3,
	 connect_push_consumer_with_id/4,
	 connect_pull_consumer_with_id/4,
	 connect_push_supplier_with_id/4,
	 connect_pull_supplier_with_id/4,
	 connect_structured_push_consumer_with_id/4,
	 connect_structured_pull_consumer_with_id/4,
	 connect_structured_push_supplier_with_id/4,
	 connect_structured_pull_supplier_with_id/4,
	 connect_sequence_push_consumer_with_id/4,
	 connect_sequence_pull_consumer_with_id/4,
	 connect_sequence_push_supplier_with_id/4,
	 connect_sequence_pull_supplier_with_id/4]).

%%------------------ CosNotification::QoSAdmin -------------------------
-export([get_qos/2, 
	 set_qos/3, 
	 validate_qos/3]).

%%------------------ CosNotification::AdminPropertiesAdmin -------------
-export([get_admin/2, 
	 set_admin/3]).



%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
-export([]).

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------
-record(state, {parent_pid, id, graph, ch_counter=-1, 
		co_counter=-1, def_supplier, def_consumer, diamonds, cyclic}).

-record(connection, {supplier, consumer, data}).

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------

%%======================================================================
%% External functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function   : init/1
%% Returns    : {ok, State}          |
%%              {ok, State, Timeout} |
%%              ignore               |
%%              {stop, Reason}
%% Description: Initiates the server
%%----------------------------------------------------------------------
init([ParentPid, MyId, QoS, _Admin]) ->
    process_flag(trap_exit, true),
    Diamonds = case lists:keysearch(?DiamondDetection, 1, QoS) of
		   false ->
		       ?ForbidDiamonds;
		   {value, {_, Value}} ->
		       Value
	       end,
    case lists:keysearch(?CycleDetection, 1, QoS) of
	{value, {_, ?AuthorizeCycles}} ->
	    {ok, #state{parent_pid = ParentPid, id = MyId, 
			graph = digraph:new([private]),
			diamonds = Diamonds, cyclic = ?AuthorizeCycles}};
	_ ->
	    {ok, #state{parent_pid = ParentPid, id = MyId, 
			graph = digraph:new([acyclic, private]),
			diamonds = Diamonds, cyclic = ?ForbidCycles}}
    end.

%%----------------------------------------------------------------------
%% Function   : terminate/2
%% Returns    : any (ignored by gen_server)
%% Description: Shutdown the server
%%----------------------------------------------------------------------
terminate(_Reason, #state{graph = DG} = _State) ->
    Connections = digraph:edges(DG),
    close_connections(DG, Connections),
    digraph:delete(DG),
    ok.

%%----------------------------------------------------------------------
%% Function   : code_change/3
%% Returns    : {ok, NewState}
%% Description: Convert process state when code is changed
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% Function   : handle_info/2
%% Returns    : {noreply, State}   |
%%              {stop, Reason, State}  
%% Description: Handle, for example, exit signals.
%%----------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, #state{parent_pid = Pid} = State) ->
    {stop, Reason, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%%------------------ CosEventDomainAdmin::EventDomain ------------------
%%---------------------------------------------------------------------%
%% Function   : add_channel
%% Arguments  : Channel - CosNotifyChannelAdmin::EventChannel
%% Returns    : MemberId - long()
%% Description: 
%%----------------------------------------------------------------------
add_channel(_OE_This, #state{ch_counter=C, graph = DG} = State, Channel) ->
    type_check(Channel, 'CosNotifyChannelAdmin_EventChannel'),
    Id = cosEventDomainApp:create_id(C),
    digraph:add_vertex(DG, Id, Channel),
    {reply, Id, State#state{ch_counter=Id}}.

%%---------------------------------------------------------------------%
%% Function   : get_all_channels
%% Arguments  : -
%% Returns    : CosEventDomainAdmin::MemberIDSeq ([long()])
%% Description: 
%%----------------------------------------------------------------------
get_all_channels(_OE_This, #state{graph = DG} = State) ->
    {reply, digraph:vertices(DG), State}.

%%---------------------------------------------------------------------%
%% Function   : get_channel
%% Arguments  : Id - CosEventDomainAdmin::MemberID (long())
%% Returns    : CosNotifyChannelAdmin::EventChannel |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}}
%% Description: 
%%----------------------------------------------------------------------
get_channel(_OE_This, #state{graph = DG} = State, Id) ->
    {reply, lookup_channel(DG, Id), State}.

%%---------------------------------------------------------------------%
%% Function   : remove_channel
%% Arguments  : Id - CosEventDomainAdmin::MemberID (long())
%% Returns    : ok |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}}
%% Description: 
%%----------------------------------------------------------------------
remove_channel(_OE_This, #state{graph = DG} = State, Id) ->
    lookup_channel(DG, Id),
    close_connections(DG, digraph:edges(DG, Id)),
    digraph:del_vertex(DG, Id),
    {reply, ok, State}.
	    
%%---------------------------------------------------------------------%
%% Function   : add_connection
%% Arguments  : Connection - CosEventDomainAdmin::Connection
%% Returns    : ConnectionID |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}} |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_TypeError'{}} |
%%              {'EXCEPTION', #'CosEventDomainAdmin_AlreadyExists'{}} |
%%              {'EXCEPTION', #'CosEventDomainAdmin_CycleCreationForbidden'{cyc}} |
%%              {'EXCEPTION', #'CosEventDomainAdmin_DiamondCreationForbidden'{diam}}
%% Description: 
%%----------------------------------------------------------------------
add_connection(_OE_This, #state{graph = DG, co_counter = C} = State, 
	       Connection) when is_record(Connection, 
					  'CosEventDomainAdmin_Connection') ->
    SId = Connection#'CosEventDomainAdmin_Connection'.supplier_id,
    SChannel = lookup_channel(DG, SId),
    CId = Connection#'CosEventDomainAdmin_Connection'.consumer_id,
    CChannel = lookup_channel(DG, CId),
    case lists:member(CId, digraph:out_neighbours(DG, SId)) of
	false ->
	    Id = cosEventDomainApp:create_id(C),
	    %% Try to insert the new connection before we actually setup a connection.
	    %% Note that #connection is NOT complete, hence, we must update it later.
	    case digraph:add_edge(DG, Id, SId, CId, #connection{data=Connection}) of
		{error, {bad_edge, Path}} ->
		    corba:raise(#'CosEventDomainAdmin_CycleCreationForbidden'{cyc=Path});
		Id when State#state.diamonds == ?AuthorizeDiamonds ->
		    case catch setup_connection(Connection, SChannel, CChannel) of
			{ok, SProxy, CProxy} ->
			    %% Now we can update the connection with complete data.
			    digraph:add_edge(DG, Id, SId, CId, #connection{supplier=SProxy,
									   consumer=CProxy,
									   data=Connection}),
			    {reply, Id, State#state{co_counter = Id}};
			{'EXCEPTION', E} ->
			    digraph:del_edge(DG, Id),
			    corba:raise(E);
			What ->
			    digraph:del_edge(DG, Id),
			    orber:dbg("[~p] CosEventDomainAdmin_EventDomain:"
				      "add_connection(~p);~nFailed setting up"
				      " connection due to: ~p", 
				      [?LINE, Connection, What], ?DEBUG_LEVEL),
			    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_MAYBE})
		    end;
		Id ->
		    case get_diamonds_helper(State, false, SId) of
			[] ->
			    case catch setup_connection(Connection, SChannel, CChannel) of
				{ok, SProxy, CProxy} ->
				    %% Now we can update the connection with complete data.
				    digraph:add_edge(DG, Id, SId, CId, #connection{supplier=SProxy,
										   consumer=CProxy,
										   data=Connection}),
				    {reply, Id, State#state{co_counter = Id}};
				{'EXCEPTION', E} ->
				    digraph:del_edge(DG, Id),
				    corba:raise(E);
				What ->
				    digraph:del_edge(DG, Id),
				    orber:dbg("[~p] CosEventDomainAdmin_EventDomain:"
					      "add_connection(~p);~nFailed setting"
					      " up connection due to: ~p", 
					      [?LINE, Connection, What], 
					      ?DEBUG_LEVEL),
				    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_MAYBE})
			    end;
			Diamond ->
			    %% Since no diamonds should exist the returned list can
			    %% only describe the diamond we just created.
			    digraph:del_edge(DG, Id),
			    corba:raise(#'CosEventDomainAdmin_DiamondCreationForbidden'
					{diam=Diamond})
		    end
	    end;
	true ->
	    corba:raise(#'CosEventDomainAdmin_AlreadyExists'{})
    end.


%%---------------------------------------------------------------------%
%% Function   : get_all_connections
%% Arguments  : -
%% Returns    : CosEventDomainAdmin::ConnectionIDSeq - [long()]
%% Description: 
%%----------------------------------------------------------------------
get_all_connections(_OE_This, #state{graph = DG} = State) ->
    {reply, digraph:edges(DG), State}.

%%---------------------------------------------------------------------%
%% Function   : get_connection
%% Arguments  : Id - CosEventDomainAdmin::ConnectionID (long())
%% Returns    : CosEventDomainAdmin::Connection |
%%              {'EXCEPTION', #'CosEventDomainAdmin_ConnectionNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
get_connection(_OE_This, #state{graph = DG} = State, Id) ->
    {reply, lookup_connection_data(DG, Id), State}.

%%---------------------------------------------------------------------%
%% Function   : remove_connection
%% Arguments  : Id - CosEventDomainAdmin::ConnectionID (long())
%% Returns    : ok |
%%              {'EXCEPTION', #'CosEventDomainAdmin_ConnectionNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
remove_connection(_OE_This, #state{graph = DG} = State, Id) ->
    #connection{supplier=S, consumer=C, data=Connection} = 
	lookup_connection(DG, Id),
    close_connection(Connection, S, C),
    digraph:del_edge(DG, Id),
    {reply, ok, State}.


%%---------------------------------------------------------------------%
%% Function   : get_offer_channels
%% Arguments  : Id - CosEventDomainAdmin::MemberID (long())
%% Returns    : CosNotifyChannelAdmin::ChannelIDSeq |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
get_offer_channels(_OE_This, #state{graph = DG, cyclic = Cyclic} = State, Id) ->
    lookup_channel(DG, Id),
    case digraph:vertex(DG, Id) of
	{Id, _Channel} when Cyclic == ?ForbidCycles ->
	    {reply, digraph_utils:reaching_neighbours([Id], DG), State};
	{Id, _Channel} ->
	    %% If cyclic graphs is allowed 'Id' will appear in the returned list.
	    %% Hence, we must delete it.
	    {reply,lists:delete(Id, digraph_utils:reaching_neighbours([Id], DG)), 
	     State};
	false ->
	    corba:raise(#'CosNotifyChannelAdmin_ChannelNotFound'{})
    end.

%%---------------------------------------------------------------------%
%% Function   : get_subscription_channels
%% Arguments  : Id - CosEventDomainAdmin::MemberID (long())
%% Returns    : CosNotifyChannelAdmin::ChannelIDSeq |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
get_subscription_channels(_OE_This, #state{graph = DG, cyclic = Cyclic} = State, Id) ->
    lookup_channel(DG, Id),
    case digraph:vertex(DG, Id) of
	{Id, _Channel} when Cyclic == ?ForbidCycles ->
	    {reply, digraph_utils:reachable_neighbours([Id], DG), State};
	{Id, _Channel} ->
	    %% If cyclic graphs is allowed 'Id' will appear in the returned list.
	    %% Hence, we must delete it.
	    {reply, lists:delete(Id, digraph_utils:reachable_neighbours([Id], DG)), 
	     State};
	false ->
	    corba:raise(#'CosNotifyChannelAdmin_ChannelNotFound'{})
    end.

%%---------------------------------------------------------------------%
%% Function   : destroy
%% Arguments  : -
%% Returns    : ok
%% Description: 
%%----------------------------------------------------------------------
destroy(_OE_This, #state{graph = _DG} = State) ->
    {stop, normal, ok, State}.

%%---------------------------------------------------------------------%
%% Function   : get_cycles
%% Arguments  : -
%% Returns    : CosEventDomainAdmin::CycleSeq
%% Description: 
%%----------------------------------------------------------------------
get_cycles(_OE_This, #state{cyclic = ?ForbidCycles} = State) ->
    {reply, [], State};
get_cycles(_OE_This, #state{graph = DG} = State) ->
    {reply, digraph_utils:cyclic_strong_components(DG), State}.

%%----------------------------------------------------------------------
%% Function   : get_diamonds
%% Arguments  : -
%% Returns    : CosEventDomainAdmin::DiamondSeq
%% Description: 
%%----------------------------------------------------------------------
get_diamonds(_OE_This, #state{diamonds = ?ForbidDiamonds} = State) ->
    {reply, [], State};
get_diamonds(_OE_This, State) ->
    {reply, get_diamonds_helper(State, true), State}.

get_diamonds_helper(#state{graph = DG} = _State, FindAll) ->
     case find_candidates(DG) of
	{[], _, _} ->
	    [];
	{_, [], _} ->
	    [];
	{COut, CIn, Max} ->
	    %% In this case we cannot tell if a diamond exists. Got to
	    %% check the paths between the candidates.
	    evaluate_candidates(DG, COut, CIn, [], Max, FindAll)
    end.

get_diamonds_helper(#state{graph = DG} = _State, FindAll, Vertex) ->
    case find_candidates(DG, Vertex) of
	{[], _, _} ->
	    [];
	{_, [], _} ->
	    [];
	{COut, CIn, Max} ->
	    %% In this case we cannot tell if a diamond exists. Got to
	    %% check the paths between the candidates.
	    evaluate_candidates(DG, COut, CIn, [], Max, FindAll)
    end.

%%---------------------------------------------------------------------%
%% Function   : set_default_consumer_channel
%% Arguments  : Id - CosEventDomainAdmin::MemberID (long())
%% Returns    : ok |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
set_default_consumer_channel(_OE_This, #state{graph = DG} = State, Id) ->
    lookup_channel(DG, Id),
    {reply, ok, State#state{def_consumer=Id}}.

%%---------------------------------------------------------------------%
%% Function   : set_default_supplier_channel
%% Arguments  : Id - CosEventDomainAdmin::MemberID (long())
%% Returns    : ok |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
set_default_supplier_channel(_OE_This, #state{graph = DG} = State, Id) ->
    lookup_channel(DG, Id),
    {reply, ok, State#state{def_supplier=Id}}.

%%---------------------------------------------------------------------%
%% Function   : connect_push_consumer
%% Arguments  : PC - CosEventComm::PushConsumer
%% Returns    : CosNotifyChannelAdmin::ProxyPushSupplier |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
connect_push_consumer(_OE_This, #state{def_supplier = Ch} = State, PC) ->
    type_check(PC, 'CosEventComm_PushConsumer'),
    Proxy = connect_a_push_consumer(Ch, PC, 'ANY_EVENT'),
    {reply, Proxy, State}.

%%---------------------------------------------------------------------%
%% Function   : connect_pull_consumer
%% Arguments  : PC - CosEventComm::PullConsumer
%% Returns    : CosNotifyChannelAdmin::ProxyPullSupplier |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
connect_pull_consumer(_OE_This, #state{def_consumer = Ch} = State, PC) ->
    Proxy = connect_a_pull_consumer(Ch, PC, 'ANY_EVENT'),
    {reply, Proxy, State}.

%%---------------------------------------------------------------------%
%% Function   : connect_push_supplier
%% Arguments  : PS - CosEventComm::PushSupplier
%% Returns    : CosNotifyChannelAdmin::ProxyPushConsumer |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
connect_push_supplier(_OE_This, #state{def_supplier = Ch} = State, PS) ->
    Proxy = connect_a_push_supplier(Ch, PS, 'ANY_EVENT'),
    {reply, Proxy, State}.

%%---------------------------------------------------------------------%
%% Function   : connect_pull_supplier
%% Arguments  : PS - CosEventComm::PullSupplier
%% Returns    : CosNotifyChannelAdmin::ProxyPullConsumer
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
connect_pull_supplier(_OE_This, #state{def_consumer = Ch} = State, PS) ->
    type_check(PS, 'CosEventComm_PullSupplier'),
    Proxy = connect_a_pull_supplier(Ch, PS, 'ANY_EVENT'),
    {reply, Proxy, State}.

%%---------------------------------------------------------------------%
%% Function   : connect_structured_push_consumer
%% Arguments  : PC - CosNotifyComm::StructuredPushConsumer
%% Returns    : CosNotifyChannelAdmin::StructuredProxyPushSupplier |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
connect_structured_push_consumer(_OE_This, #state{def_supplier = Ch} = State, PC) ->
    type_check(PC, 'CosNotifyComm_StructuredPushConsumer'),
    Proxy = connect_a_push_consumer(Ch, PC, 'STRUCTURED_EVENT'),
    {reply, Proxy, State}.

%%---------------------------------------------------------------------%
%% Function   : connect_structured_pull_consumer
%% Arguments  : PC - CosNotifyComm::StructuredPullConsumer
%% Returns    : CosNotifyChannelAdmin::StructuredProxyPullSupplier |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
connect_structured_pull_consumer(_OE_This, #state{def_supplier = Ch} = State, PC) ->
    Proxy = connect_a_pull_consumer(Ch, PC, 'STRUCTURED_EVENT'),
    {reply, Proxy, State}.

%%---------------------------------------------------------------------%
%% Function   : connect_structured_push_supplier
%% Arguments  : PS - CosNotifyComm::StructuredPushSupplier
%% Returns    : CosNotifyChannelAdmin::StructuredProxyPushConsumer |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
connect_structured_push_supplier(_OE_This, #state{def_consumer = Ch} = State, PS) ->
    Proxy = connect_a_push_supplier(Ch, PS, 'STRUCTURED_EVENT'),
    {reply, Proxy, State}.

%%---------------------------------------------------------------------%
%% Function   : connect_structured_pull_supplier
%% Arguments  : PS - CosNotifyComm::StructuredPullSupplier
%% Returns    : CosNotifyChannelAdmin::StructuredProxyPullConsumer |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
connect_structured_pull_supplier(_OE_This, #state{def_consumer = Ch} = State, PS) ->
    type_check(PS, 'CosNotifyComm_StructuredPullSupplier'),
    Proxy = connect_a_pull_supplier(Ch, PS, 'STRUCTURED_EVENT'),
    {reply, Proxy, State}.

%%---------------------------------------------------------------------%
%% Function   : connect_sequence_push_consumer
%% Arguments  : PC - CosNotifyComm::SequencePushConsumer
%% Returns    : CosNotifyChannelAdmin::SequenceProxyPushSupplier |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
connect_sequence_push_consumer(_OE_This, #state{def_supplier = Ch} = State, PC) ->
    type_check(PC, 'CosNotifyComm_SequencePushConsumer'),
    Proxy = connect_a_push_consumer(Ch, PC, 'SEQUENCE_EVENT'),
    {reply, Proxy, State}.

%%---------------------------------------------------------------------%
%% Function   : connect_sequence_pull_consumer
%% Arguments  : PC - CosNotifyComm::SequencePullConsumer
%% Returns    : CosNotifyChannelAdmin::SequenceProxyPullSupplier |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
connect_sequence_pull_consumer(_OE_This, #state{def_supplier = Ch} = State, PC) ->
    Proxy = connect_a_pull_consumer(Ch, PC, 'SEQUENCE_EVENT'),
    {reply, Proxy, State}.

%%---------------------------------------------------------------------%
%% Function   : connect_sequence_push_supplier
%% Arguments  : PS - CosNotifyComm::SequencePushSupplier
%% Returns    : CosNotifyChannelAdmin::SequenceProxyPushConsumer |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
connect_sequence_push_supplier(_OE_This, #state{def_consumer = Ch} = State, PS) ->
    Proxy = connect_a_push_supplier(Ch, PS, 'SEQUENCE_EVENT'),
    {reply, Proxy, State}.

%%---------------------------------------------------------------------%
%% Function   : connect_sequence_pull_supplier
%% Arguments  : PS - CosNotifyComm::SequencePullSupplier
%% Returns    : CosNotifyChannelAdmin::SequenceProxyPullConsumer |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
connect_sequence_pull_supplier(_OE_This, #state{def_consumer = Ch} = State, PS) ->
    type_check(PS, 'CosNotifyComm_SequencePullSupplier'),
    Proxy = connect_a_pull_supplier(Ch, PS, 'SEQUENCE_EVENT'),
    {reply, Proxy, State}.

%%---------------------------------------------------------------------%
%% Function   : connect_push_consumer_with_id
%% Arguments  : PC - CosEventComm::PushConsumer
%%              Id - CosEventDomainAdmin::MemberID (long())
%% Returns    : CosNotifyChannelAdmin::ProxyPushSupplier |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
connect_push_consumer_with_id(_OE_This, #state{graph = DG} = State, PC, Id) ->
    type_check(PC, 'CosEventComm_PushConsumer'),
    Channel = lookup_channel(DG, Id),
    Proxy = connect_a_push_consumer(Channel, PC, 'ANY_EVENT'),
    {reply, Proxy, State}.

%%---------------------------------------------------------------------%
%% Function   : connect_pull_consumer_with_id
%% Arguments  : PC - CosEventComm::PullConsumer
%%              Id - CosEventDomainAdmin::MemberID (long())
%% Returns    : CosNotifyChannelAdmin::ProxyPullSupplier |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
connect_pull_consumer_with_id(_OE_This, #state{graph = DG} = State, PC, Id) ->
    Channel = lookup_channel(DG, Id),
    Proxy = connect_a_pull_consumer(Channel, PC, 'ANY_EVENT'),
    {reply, Proxy, State}.

%%---------------------------------------------------------------------%
%% Function   : connect_push_supplier_with_id
%% Arguments  : PS - CosEventComm::PushSupplier
%%              Id - CosEventDomainAdmin::MemberID (long())
%% Returns    : CosNotifyChannelAdmin::ProxyPushConsumer |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
connect_push_supplier_with_id(_OE_This, #state{graph = DG} = State, PS, Id) ->
    Channel = lookup_channel(DG, Id),
    Proxy = connect_a_push_supplier(Channel, PS, 'ANY_EVENT'),
    {reply, Proxy, State}.

%%---------------------------------------------------------------------%
%% Function   : connect_pull_supplier_with_id
%% Arguments  : PS - CosEventComm::PullSupplier
%%              Id - CosEventDomainAdmin::MemberID (long())
%% Returns    :  CosNotifyChannelAdmin::ProxyPullConsumer |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
connect_pull_supplier_with_id(_OE_This, #state{graph = DG} = State, PS, Id) ->
    type_check(PS, 'CosEventComm_PullSupplier'),
    Channel = lookup_channel(DG, Id),
    Proxy = connect_a_pull_supplier(Channel, PS, 'ANY_EVENT'),
    {reply, Proxy, State}.

%%---------------------------------------------------------------------%
%% Function   : connect_structured_push_consumer_with_id
%% Arguments  : PC - CosNotifyComm::StructuredPushConsumer
%%              Id - CosEventDomainAdmin::MemberID (long())
%% Returns    : CosNotifyChannelAdmin::StructuredProxyPushSupplier |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
connect_structured_push_consumer_with_id(_OE_This, #state{graph = DG} = State, PC, Id) ->
    type_check(PC, 'CosNotifyComm_StructuredPushConsumer'),
    Channel = lookup_channel(DG, Id),
    Proxy = connect_a_push_consumer(Channel, PC, 'STRUCTURED_EVENT'),
    {reply, Proxy, State}.

%%---------------------------------------------------------------------%
%% Function   : connect_structured_pull_consumer_with_id
%% Arguments  : PC - CosNotifyComm::StructuredPullConsumer
%%              Id - CosEventDomainAdmin::MemberID (long())
%% Returns    : CosNotifyChannelAdmin::StructuredProxyPullSupplier |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
connect_structured_pull_consumer_with_id(_OE_This, #state{graph = DG} = State, PC, Id) ->
    Channel = lookup_channel(DG, Id),
    Proxy = connect_a_pull_consumer(Channel, PC, 'STRUCTURED_EVENT'),
    {reply, Proxy, State}.

%%---------------------------------------------------------------------%
%% Function   : connect_structured_push_supplier_with_id
%% Arguments  : PS - CosNotifyComm::StructuredPushSupplier
%%              Id - CosEventDomainAdmin::MemberID (long())
%% Returns    : CosNotifyChannelAdmin::StructuredProxyPushConsumer |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
connect_structured_push_supplier_with_id(_OE_This, #state{graph = DG} = State, PS, Id) ->
    Channel = lookup_channel(DG, Id),
    Proxy = connect_a_push_supplier(Channel, PS, 'STRUCTURED_EVENT'),
    {reply, Proxy, State}.

%%---------------------------------------------------------------------%
%% Function   : connect_structured_pull_supplier_with_id
%% Arguments  : PS - CosNotifyComm::StructuredPullSupplier
%%              Id - CosEventDomainAdmin::MemberID (long())
%% Returns    : CosNotifyChannelAdmin::StructuredProxyPullConsumer |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
connect_structured_pull_supplier_with_id(_OE_This, #state{graph = DG} = State, PS, Id) ->
    type_check(PS, 'CosNotifyComm_StructuredPullSupplier'),
    Channel = lookup_channel(DG, Id),
    Proxy = connect_a_pull_supplier(Channel, PS, 'STRUCTURED_EVENT'),
    {reply, Proxy, State}.

%%---------------------------------------------------------------------%
%% Function   : connect_sequence_push_consumer_with_id
%% Arguments  : PC - CosNotifyComm::SequencePushConsumer
%%              Id - CosEventDomainAdmin::MemberID (long())
%% Returns    : CosNotifyChannelAdmin::SequenceProxyPushSupplier |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
connect_sequence_push_consumer_with_id(_OE_This, #state{graph = DG} = State, PC, Id) ->
    type_check(PC, 'CosNotifyComm_SequencePushConsumer'),
    Channel = lookup_channel(DG, Id),
    Proxy = connect_a_push_consumer(Channel, PC, 'SEQUENCE_EVENT'),
    {reply, Proxy, State}.

%%---------------------------------------------------------------------%
%% Function   : connect_sequence_pull_consumer_with_id
%% Arguments  : PC - CosNotifyComm::SequencePullConsumer
%%              Id - CosEventDomainAdmin::MemberID (long())
%% Returns    : CosNotifyChannelAdmin::SequenceProxyPullSupplier |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
connect_sequence_pull_consumer_with_id(_OE_This, #state{graph = DG} = State, PC, Id) ->
    Channel = lookup_channel(DG, Id),
    Proxy = connect_a_pull_consumer(Channel, PC, 'SEQUENCE_EVENT'),
    {reply, Proxy, State}.

%%---------------------------------------------------------------------%
%% Function   : connect_sequence_push_supplier_with_id
%% Arguments  : PS - CosNotifyComm::SequencePushSupplier
%%              Id - CosEventDomainAdmin::MemberID (long())
%% Returns    : CosNotifyChannelAdmin::SequenceProxyPushConsumer |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
connect_sequence_push_supplier_with_id(_OE_This, #state{graph = DG} = State, PS, Id) ->
    Channel = lookup_channel(DG, Id),
    Proxy = connect_a_push_supplier(Channel, PS, 'SEQUENCE_EVENT'),
    {reply, Proxy, State}.

%%---------------------------------------------------------------------%
%% Function   : connect_sequence_pull_supplier_with_id
%% Arguments  : PS - CosNotifyComm::SequencePullSupplier
%%              Id - CosEventDomainAdmin::MemberID (long())
%% Returns    : CosNotifyChannelAdmin::SequenceProxyPullConsumer |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
connect_sequence_pull_supplier_with_id(_OE_This, #state{graph = DG} = State, PS, Id) ->
    type_check(PS, 'CosNotifyComm_SequencePullSupplier'),
    Channel = lookup_channel(DG, Id),
    Proxy = connect_a_pull_supplier(Channel, PS, 'SEQUENCE_EVENT'),
    {reply, Proxy, State}.


%%----------------------------------------------------------------------
%%------------------ CosNotification::QoSAdmin -------------------------
%%---------------------------------------------------------------------%
%% Function   : get_qos
%% Arguments  : -
%% Returns    : CosNotification::QoSProperties
%% Description: 
%%----------------------------------------------------------------------
get_qos(_OE_This, #state{cyclic = Cyclic, diamonds = Diamonds} = State) ->
    {reply, [#'CosNotification_Property'
	     {name = ?DiamondDetection, 
	      value = any:create(orber_tc:short(), Diamonds)}, 
	     #'CosNotification_Property'
	     {name = ?CycleDetection, 
	      value = any:create(orber_tc:short(), Cyclic)}], State}.

%%---------------------------------------------------------------------%
%% Function   : set_qos
%% Arguments  : NewQoS - CosNotification::QoSProperties
%% Returns    : ok |
%%              {'EXCEPTION', #'CosNotification_UnsupportedQoS{}}
%% Description: 
%%----------------------------------------------------------------------
set_qos(_OE_This, State, NewQoS) ->
    QoS = cosEventDomainApp:get_qos(NewQoS),
    case set_qos_helper(QoS, State, []) of
	{ok, NewState} ->
	    {reply, ok, NewState};
	{error, Errors} ->
	    corba:raise(#'CosNotification_UnsupportedQoS'{qos_err = Errors})
    end.
    
set_qos_helper([], State, []) ->
    {ok, State}; %{reply, ok, State};
set_qos_helper([], _, Errors) ->
    {error, Errors};
set_qos_helper([{?DiamondDetection, Diamonds}|T], #state{diamonds = Diamonds} = State,
	       Errors) ->
    set_qos_helper(T, State, Errors);
set_qos_helper([{?CycleDetection, Cyclic}|T], #state{cyclic = Cyclic} = State, 
	       Errors) ->
    set_qos_helper(T, State, Errors);
set_qos_helper([{?DiamondDetection, ?AuthorizeDiamonds}|T], State, Errors) ->
    %% Diamonds have not been allowed so far so it's safe to allow it.
    set_qos_helper(T, State#state{diamonds = ?AuthorizeDiamonds}, Errors);
set_qos_helper([{?DiamondDetection, ?ForbidDiamonds}|T], State, Errors) ->
    %% If any diamonds already exists we cannot allow this. Hence, now we must check
    %% if we can update the QoS.
    case get_diamonds_helper(State, false) of
	[] ->
	    set_qos_helper(T, State#state{diamonds = ?ForbidDiamonds}, Errors);
	_ ->
	    set_qos_helper(T, State, 
			   [#'CosNotification_PropertyError'
			    {code = 'UNAVAILABLE_VALUE', 
			     name = ?DiamondDetection, 
			     available_range = #'CosNotification_PropertyRange'
			     {low_val=any:create(orber_tc:short(), ?AuthorizeDiamonds), 
			      high_val=any:create(orber_tc:short(), ?AuthorizeDiamonds)}}|Errors])
    end;
set_qos_helper([{?CycleDetection, _}|T], #state{cyclic = Cyclic} = State, Errors) ->
    %% Currently we do not support changing the Cycle schema. If we want to,
    %% we must copy the graph to a new instance of the correct type.
    set_qos_helper(T, State, 
		   [#'CosNotification_PropertyError'
		    {code = 'UNAVAILABLE_VALUE', 
		     name = ?CycleDetection, 
		     available_range = #'CosNotification_PropertyRange'
		     {low_val=any:create(orber_tc:short(), Cyclic), 
		      high_val=any:create(orber_tc:short(), Cyclic)}}|Errors]).

%%---------------------------------------------------------------------%
%% Function   : validate_qos
%% Arguments  : WantedQoS - CosNotification::QoSProperties
%% Returns    : {ok, CosNotification::NamedPropertyRangeSeq} |
%%              {'EXCEPTION', #'CosNotification_UnsupportedQoS{}}
%% Description: NamedPropertyRangeSeq is of out-type
%%----------------------------------------------------------------------
validate_qos(_OE_This, State, WantedQoS) ->
    QoS = cosEventDomainApp:get_qos(WantedQoS),
    case validate_qos_helper(QoS, State, [], []) of
	{ok, Properties} ->
	    {reply, {ok, Properties}, State};
	{error, Errors} ->
	    corba:raise(#'CosNotification_UnsupportedQoS'{qos_err = Errors})
    end.

validate_qos_helper([], _, Properties, []) ->
    {ok, Properties};
validate_qos_helper([], _, _, Errors) ->
    {error, Errors};
validate_qos_helper([{?DiamondDetection, ?ForbidDiamonds}|T], State, Properties, 
		    Errors) ->
    case get_diamonds_helper(State, false) of
	[] ->
	    Property = 
		#'CosNotification_NamedPropertyRange'
	      {name = ?DiamondDetection, 
	       range = #'CosNotification_PropertyRange'
	       {low_val=any:create(orber_tc:short(), ?AuthorizeDiamonds), 
		high_val=any:create(orber_tc:short(), ?ForbidDiamonds)}},
	    validate_qos_helper(T, State, [Property|Properties], Errors);
	_ ->
	    Error = 
		#'CosNotification_PropertyError'
	      {code = 'UNAVAILABLE_VALUE', 
	       name = ?DiamondDetection, 
	       available_range = #'CosNotification_PropertyRange'
	       {low_val=any:create(orber_tc:short(), ?AuthorizeDiamonds), 
		high_val=any:create(orber_tc:short(), ?AuthorizeDiamonds)}},
	    validate_qos_helper(T, State, Properties, [Error|Errors])
    end;
validate_qos_helper([{?DiamondDetection, ?AuthorizeDiamonds}|T], State, Properties, 
		    Errors) ->
    Property = 
	#'CosNotification_NamedPropertyRange'
      {name = ?DiamondDetection, 
       range = #'CosNotification_PropertyRange'
       {low_val=any:create(orber_tc:short(), ?AuthorizeDiamonds), 
	high_val=any:create(orber_tc:short(), ?ForbidDiamonds)}},
    validate_qos_helper(T, State, [Property|Properties], Errors);
validate_qos_helper([{?CycleDetection, Cyclic}|T], #state{cyclic = Cyclic} = State, 
		    Properties, Errors) ->
    validate_qos_helper(T, State, Properties, Errors);
validate_qos_helper([{?CycleDetection, _}|T], #state{cyclic = Cyclic} = State, 
		    Properties, Errors) ->
    Error = 
	#'CosNotification_PropertyError'
      {code = 'UNAVAILABLE_VALUE', 
       name = ?CycleDetection, 
       available_range = #'CosNotification_PropertyRange'
       {low_val=any:create(orber_tc:short(), Cyclic), 
	high_val=any:create(orber_tc:short(), Cyclic)}},
    validate_qos_helper(T, State, Properties, [Error|Errors]).


%%----------------------------------------------------------------------
%%------------------ CosNotification::AdminPropertiesAdmin -------------
%%---------------------------------------------------------------------%
%% Function   : get_admin
%% Arguments  : -
%% Returns    : CosNotification::AdminProperties
%% Description: No Admins currently supported
%%----------------------------------------------------------------------
get_admin(_OE_This, State) ->
    {reply, [], State}.

%%---------------------------------------------------------------------%
%% Function   : set_admin
%% Arguments  : NewAdmins - CosNotification::AdminProperties
%% Returns    : ok |
%%              {'EXCEPTION', #'CosNotification_UnsupportedAdmin{}}
%% Description: No Admins currently supported
%%----------------------------------------------------------------------
set_admin(_OE_This, State, NewAdmins) ->
    cosEventDomainApp:get_admin(NewAdmins),
    {reply, ok, State}.


%%======================================================================
%% Internal functions
%%======================================================================
%%---------------------------------------------------------------------%
%% Function   : find_candidates
%% Arguments  : Digraph reference
%%              (Vertex - if we're interested in a specific vertex.
%% Returns    : {[SourceVertices], [SinkVertices], Max}
%%              SourceVertices - {Vertice, [ReachableVertices]}
%%              SinkVertices - {Vertice, [ReachingVertices]}
%%              Max - number of edges in the graph.
%% Description: To be a part of a diamond ("transitive" relation xRy, yRz => xRz; 
%%              in comparison with discrete mathematics we do not require that the
%%              entire graph is transitive) a vertex must have more than one
%%              outgoing and/or incoming. Hence, a digraph must contain at least
%%              one vertex with more than one outgoing edges and at least
%%              one with more than one incoming edges for a diamond to exist.
%%              Hence, the purpose of this function is to find vertices that
%%              look like:
%%              
%%              Vout->V1           V6->Vin
%%                  \         and      ^
%%                   +->V2             |
%%                                 V8--+
%%----------------------------------------------------------------------
find_candidates(DG) ->
    Edges = digraph:edges(DG),
    {COut, CIn, Max} = find_candidates_helper(Edges, [], [], DG, 0),
    {filter_candidates(COut), filter_candidates(CIn), Max}.


find_candidates(DG, _Vertex) ->
    %% We should use the fact that we know one of the vertices.
    Edges = digraph:edges(DG),
    {COut, CIn, Max} = find_candidates_helper(Edges, [], [], DG, 0),
    {filter_candidates(COut), filter_candidates(CIn), Max}.

find_candidates_helper([], AccOut, AccIn, _, Counter) ->
    {lists:sort(AccOut), lists:sort(AccIn), Counter};
find_candidates_helper([H|T], AccOut, AccIn, DG, Counter) ->
    {H, V1, V2, _Label} = digraph:edge(DG, H),
    find_candidates_helper(T, [{V1, V2}|AccOut], [{V2,V1}|AccIn], DG, Counter+1).

filter_candidates([]) ->
    [];
filter_candidates([{V1, V2}|T]) ->
    filter_candidates([{V1, V2}|T], V1, [], []).
filter_candidates([], _V, [_Acc1], Acc2) ->
    %% Only one in/out connection. Hence, cannot be start- or end-point
    %% of a diamond.
    lists:reverse(Acc2);
filter_candidates([], V, Acc1, Acc2) ->
    lists:reverse([{V, lists:reverse(Acc1)}|Acc2]);
filter_candidates([{V1, V2}|T], V1, Acc1, Acc2) ->
    filter_candidates(T, V1, [V2|Acc1], Acc2);
filter_candidates([{V1, V2}|T], _V, [_Acc1], Acc2) ->
    %% Only one in/out connection. Hence, cannot be start- or end-point
    %% of a diamond.
    filter_candidates(T, V1, [V2], Acc2);
filter_candidates([{V1, V2}|T], V, Acc1, Acc2) ->
    filter_candidates(T, V1, [V2], [{V, lists:reverse(Acc1)}|Acc2]).

%%---------------------------------------------------------------------%
%% Function   : evaluate_candidates
%% Arguments  : -
%% Returns    : [Diamond]
%% Description: There are several scenarios but they can be categorized as:
%%              (1)       (2)     (3)        (4)
%%                2         2                  2-..-56
%%               / \       / \                /      \
%%              1---4     1---4   1---4      1        4
%%               \ /               \ /        \      /
%%                3                 3          3-..-11
%%
%%              The purpose of this function is to find these in the cheapest
%%              way possible. For complex diamonds (may also include cycles)
%%              duplicates may be generated. For example, #2/#3 is a sub-set of #1
%%              but they are as well diamonds.
%%----------------------------------------------------------------------
evaluate_candidates(_DG, [], _, Acc, _Max, _) ->
    Acc;
evaluate_candidates(DG, [{V, OutV}|T], CIn, Acc, Max, FindAll) ->
    case evaluate_candidates_helper(DG, V, OutV, CIn, [], FindAll) of
	[] ->
	    evaluate_candidates(DG, T, CIn, Acc, Max, FindAll);
	Diamonds when FindAll == true ->
	    %% May be more than one diamond.
	    evaluate_candidates(DG, T, CIn, Diamonds ++ Acc, Max, FindAll);
	Diamond ->
	    Diamond
    end.

evaluate_candidates_helper(_, _, _, _, [Diamond], false) ->
    Diamond;
evaluate_candidates_helper(_, _, _, [], Diamonds, _) ->
    Diamonds;
evaluate_candidates_helper(DG, V1, OutV, [{V1, _InV}|T], Diamonds, FindAll) ->
    evaluate_candidates_helper(DG, V1, OutV, T, Diamonds, FindAll);
evaluate_candidates_helper(DG, V1, OutV, [{V2, InV}|T], Diamonds, FindAll) ->
    case double_match(OutV, InV, [], V1, V2) of
	[] ->
	    case is_member(InV, V1) of
		true ->
		    %% At this point we know that we have:
		    %%  x -> y
		    %% For this pair to be a part of a diamond we have two options:
		    %% (1) x - y      (2) x ---- y
		    %%      \ /   or       \    /   or a additional path besides z1-zN,
		    %%       z              z1-zN
		    case double_match_exclude(OutV, InV, [], V1, V2) of
			[] ->
			    %% Nope it's not #1.
			    case digraph_match(OutV, V2, V1, DG, 1) of
				false ->
				    evaluate_candidates_helper(DG, V1, OutV, T, 
							       Diamonds, FindAll);
				Diamond ->
				    evaluate_candidates_helper(DG, V1, OutV, T,  
							       [[[V1, V2]|Diamond]|Diamonds], 
							       FindAll)
			    end;
			Diamond ->
			    %% We've found a diamond looking like:
			    %%   x - y     xRy, yRz => xRz
			    %%    \ /
			    %%     z
			    evaluate_candidates_helper(DG, V1, OutV, T, 
						       [[[V1, V2]|Diamond]|Diamonds], 
						       FindAll)
		    end;
		false ->
		    case digraph_match(OutV, V2, V1, DG) of
			false ->
			    evaluate_candidates_helper(DG, V1, OutV, T, 
						       Diamonds, FindAll);
			Diamond ->
			    evaluate_candidates_helper(DG, V1, OutV, T,  
						       [Diamond|Diamonds], FindAll)
		    end
	    end;
	Diamond ->
	    %% We've found a diamond looking something like:
	    %%   2
	    %%  / \
	    %% 1-5-4  V1 eq. 1, V2 eq 4.
	    %%  \ / 
	    %%   3
	    evaluate_candidates_helper(DG, V1, OutV, T, [Diamond|Diamonds], 
				       FindAll)
    end.

is_member([], _) ->
    false;
is_member([H|_], H) ->
    true;
is_member([H|_], H2) when H > H2 ->
    %% Since it's a sorted list no need to look any further.
    false;
is_member([_|T], H) ->
    is_member(T, H).

double_match([], _, [_Matched], _, _) ->
    [];
double_match([], _, Matched, _, _) ->
    Matched;
double_match(_, [], [_Matched], _, _) ->
    [];
double_match(_, [], Matched, _, _) ->
    Matched;
double_match([H|T1], [H|T2], Matched, V1, V2) ->
    double_match(T1, T2, [[V1,H,V2] | Matched], V1, V2);
double_match([H1|T1], [H2|T2], Matched, V1, V2) when H1 > H2 ->
    double_match([H1|T1], T2, Matched, V1, V2);
double_match([_H1|T1], [H2|T2], Matched, V1, V2) ->
    double_match(T1, [H2|T2], Matched, V1, V2).

double_match_exclude([], _, Matched, _, _) ->
    Matched;
double_match_exclude(_, [], Matched, _, _) ->
    Matched;
%% exclude it
double_match_exclude([V2|T1], CIn, Matched, V1, V2) ->
    double_match_exclude(T1, CIn, Matched, V1, V2);
%% exclude it
double_match_exclude(COut, [V1|T2], Matched, V1, V2) ->
    double_match_exclude(COut, T2, Matched, V1, V2);
%% Found match
double_match_exclude([H|T1], [H|T2], Matched, V1, V2) ->
    double_match_exclude(T1, T2, [[V1,H,V2] | Matched], V1, V2);
double_match_exclude([H1|T1], [H2|T2], Matched, V1, V2) when H1 > H2 ->
    double_match_exclude([H1|T1], T2, Matched, V1, V2);
double_match_exclude([_H1|T1], [H2|T2], Matched, V1, V2) ->
    double_match_exclude(T1, [H2|T2], Matched, V1, V2).


digraph_match(OutV, V2, V1, DG) ->
    digraph_match(OutV, V2, V1, DG, [], 0).

digraph_match(OutV, V2, V1, DG, Counter) ->
    digraph_match(OutV, V2, V1, DG, [], Counter).

digraph_match([], _, _, _, _, Counter) when Counter < 2 ->
    false;
digraph_match([], _, _, _, Acc, _) ->
    Acc;
digraph_match([Vin|T], Vin, Vout, DG, Acc, Counter) ->
    digraph_match(T, Vin, Vout, DG, Acc, Counter);
digraph_match([H|T], Vin, Vout, DG, Acc, Counter) ->
    case get_path(DG, H, Vin, H, Vout) of
	false ->
	    digraph_match(T, Vin, Vout, DG, Acc, Counter);
	Path ->
	    %% Found one path; now me must se if there are one more.
	    digraph_match(T, Vin, Vout, DG, [[Vout|lists:reverse(Path)] | Acc], 
			  Counter+1)
    end.

get_path(G, V1, V2, E1, E2) ->
    one_path(digraph:out_neighbours(G, V1), V2, [], [V1], [V1], G, E1, E2).

one_path([E1|_Vs], W, Cont, Xs, Ps, G, E1, E2) ->
    one_path([], W, Cont, Xs, Ps, G, E1, E2);
one_path([E2|_Vs], W, Cont, Xs, Ps, G, E1, E2) ->
    one_path([], W, Cont, Xs, Ps, G, E1, E2);
one_path([W|_Ws], W, _Cont, _Xs, Ps, _G, _E1, _E2) ->
    [W|Ps];
one_path([V|Vs], W, Cont, Xs, Ps, G, E1, E2) ->
    case lists:member(V, Xs) of
	true ->  one_path(Vs, W, Cont, Xs, Ps, G, E1, E2);
	false -> one_path(digraph:out_neighbours(G, V), W, 
			  [{Vs,Ps} | Cont], [V|Xs], [V|Ps], G, E1, E2)
    end;
one_path([], W, [{Vs,Ps}|Cont], Xs, _, G, E1, E2) ->
    one_path(Vs, W, Cont, Xs, Ps, G, E1, E2);
one_path([], _, [], _, _, _, _, _) -> false.

%%---------------------------------------------------------------------%
%% function : type_check
%% Arguments: Obj  - objectrefernce to test.
%%            Mod  - Module which contains typeID/0.
%% Returns  : 'ok' or raises exception.
%% Effect   : 
%%----------------------------------------------------------------------
type_check(Obj, Mod) ->
    case catch corba_object:is_a(Obj,Mod:typeID()) of
        true ->
            ok;
        What ->
	    orber:dbg("[~p] CosEventDomainAdmin:type_check();~n"
		      "Object of incorrect type supplied; should be: ~p~n"
		      "Failed due to: ~p", [?LINE, Mod, What], ?DEBUG_LEVEL),
            corba:raise(#'BAD_PARAM'{minor=507, completion_status=?COMPLETED_NO})
    end.

%%---------------------------------------------------------------------%
%% function : lookup_channel
%% Arguments: DG - digraph reference
%%            Id - CosEventDomainAdmin::MemberID (long())
%% Returns    : CosNotifyChannelAdmin::EventChannel |
%%              {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}}
%% Effect   : 
%%----------------------------------------------------------------------
lookup_channel(DG, Id) ->
    case digraph:vertex(DG, Id) of
	{Id, Channel} ->
	    Channel;
	false ->
	    corba:raise(#'CosNotifyChannelAdmin_ChannelNotFound'{})
    end.


%%---------------------------------------------------------------------%
%% function : lookup_connection
%% Arguments: DG - digraph reference
%%            Id - CosEventDomainAdmin::ConnectionID (long())
%% Returns    : #connectio{} |
%%              {'EXCEPTION', #'CosEventDomainAdmin_ConnectionNotFound'{}}
%% Effect   : 
%%----------------------------------------------------------------------
lookup_connection(DG, Id) ->
    case digraph:edge(DG, Id) of
	{Id, _SId, _CId, Connection} ->
	    Connection;
	false ->
	    corba:raise(#'CosEventDomainAdmin_ConnectionNotFound'{})
    end.

%%---------------------------------------------------------------------%
%% function : lookup_connection_data
%% Arguments: DG - digraph reference
%%            Id - CosEventDomainAdmin::ConnectionID (long())
%% Returns  : CosEventDomainAdmin::Connection |
%%            {'EXCEPTION', #'CosEventDomainAdmin_ConnectionNotFound'{}}
%% Effect   : 
%%----------------------------------------------------------------------
lookup_connection_data(DG, Id) ->
    case digraph:edge(DG, Id) of
	{Id, _SId, _CId, #connection{data = Connection}} ->
	    Connection;
	false ->
	    corba:raise(#'CosEventDomainAdmin_ConnectionNotFound'{})
    end.

%%---------------------------------------------------------------------%
%% function : close_connections
%% Arguments: DG -digraph reference
%%            [CosEventDomainAdmin::ConnectionID] - [long()]
%% Returns  : ok
%% Effect   : 
%%----------------------------------------------------------------------
close_connections(_DG, []) ->
    ok;
close_connections(DG, [H|T]) ->
    #connection{supplier=S, consumer=C, data=Connection} = 
	lookup_connection(DG, H),
    close_connection(Connection, S, C),
    digraph:del_edge(DG, H),
    close_connections(DG, T).

%%---------------------------------------------------------------------%
%% function : close_connection
%% Arguments: CosEventDomainAdmin::Connection
%%            S - SupplierProxy
%%            C - ConsumerProxy
%% Returns  : ok
%% Effect   : 
%%----------------------------------------------------------------------
close_connection(#'CosEventDomainAdmin_Connection'{ctype=Type,
						  notification_style = Style},
		S, C) ->
    case {Type, Style} of
	{'ANY_EVENT', 'Push'} ->
	    catch 'CosNotifyChannelAdmin_ProxyPushSupplier':disconnect_push_supplier(S),
	    catch 'CosNotifyChannelAdmin_ProxyPushConsumer':disconnect_push_consumer(C);
	{'ANY_EVENT', 'Pull'} ->
	    catch 'CosNotifyChannelAdmin_ProxyPullSupplier':disconnect_pull_supplier(S),
	    catch 'CosNotifyChannelAdmin_ProxyPullConsumer':disconnect_pull_consumer(C);
	{'STRUCTURED_EVENT', 'Push'} ->
	    catch 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':disconnect_structured_push_supplier(S),
	    catch 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':disconnect_structured_push_consumer(C);
	{'STRUCTURED_EVENT', 'Pull'} ->
	    catch 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':disconnect_structured_pull_supplier(S),
	    catch 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':disconnect_structured_pull_consumer(C);
	{'SEQUENCE_EVENT', 'Push'} ->
	    catch 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':disconnect_sequence_push_supplier(S),
	    catch 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':disconnect_sequence_push_consumer(C);
	{'SEQUENCE_EVENT', 'Pull'}->
	    catch 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':disconnect_sequence_pull_supplier(S),
	    catch 'CosNotifyChannelAdmin_SequenceProxyPullConsumer':disconnect_sequence_pull_consumer(C)
    end,
    ok.

%%---------------------------------------------------------------------%
%% function : setup_connection
%% Arguments: CosEventDomainAdmin::Connection
%%            S - SupplierProxy
%%            C - ConsumerProxy
%% Returns  : {ok, SupplierProxy, ConsumerProxy};
%% Effect   : 
%%----------------------------------------------------------------------
setup_connection(#'CosEventDomainAdmin_Connection'{ctype=Type,
						   notification_style = Style},
		S, C) ->
    Admin = 
	'CosNotifyChannelAdmin_EventChannel':'_get_default_consumer_admin'(S),
    case Style of
	'Push' ->
	    {Proxy, _Id} = 
		'CosNotifyChannelAdmin_ConsumerAdmin':
		obtain_notification_push_supplier(Admin, Type),
	    CProxy = connect_a_push_supplier(C, Proxy, Type),
	    case Type of
		'ANY_EVENT' ->
		    'CosNotifyChannelAdmin_ProxyPushSupplier':
			connect_any_push_consumer(Proxy, CProxy);
		'STRUCTURED_EVENT' ->
		    'CosNotifyChannelAdmin_StructuredProxyPushSupplier':
			connect_structured_push_consumer(Proxy, CProxy);
		'SEQUENCE_EVENT' ->
		    'CosNotifyChannelAdmin_SequenceProxyPushSupplier':
			connect_sequence_push_consumer(Proxy, CProxy)
	    end,
	    {ok, Proxy, CProxy};
	'Pull' ->
	    {Proxy, _Id} = 
		'CosNotifyChannelAdmin_ConsumerAdmin':
		obtain_notification_pull_supplier(Admin, Type),
	    CProxy = connect_a_pull_supplier(C, Proxy, Type),
	    case Type of
		'ANY_EVENT' ->
		    'CosNotifyChannelAdmin_ProxyPullSupplier':
			connect_any_pull_consumer(Proxy, CProxy);
		'STRUCTURED_EVENT' ->
		    'CosNotifyChannelAdmin_StructuredProxyPullSupplier':
			connect_structured_pull_consumer(Proxy, CProxy);
		'SEQUENCE_EVENT' ->
		    'CosNotifyChannelAdmin_SequenceProxyPullSupplier':
			connect_sequence_pull_consumer(Proxy, CProxy)
	    end,
	    {ok, Proxy, CProxy}
    end.

%%---------------------------------------------------------------------%
%% function : connect_a_pull_consumer
%% Arguments: Channel - CosNotifyChannelAdmin::EventChannel | undefined
%%            PC - a PullConsumer
%%            Type - 'ANY_EVENT' | 'STRUCTURED_EVENT' | 'SEQUENCE_EVENT'
%% Returns  : 
%% Effect   : 
%%----------------------------------------------------------------------
connect_a_pull_consumer(undefined, _, _) ->
    corba:raise(#'CosNotifyChannelAdmin_ChannelNotFound'{});
connect_a_pull_consumer(Channel, PC, Type) ->
    Admin = 
	'CosNotifyChannelAdmin_EventChannel':'_get_default_consumer_admin'(Channel),
    {Proxy, _Id} = 
	'CosNotifyChannelAdmin_ConsumerAdmin':obtain_notification_pull_supplier(Admin, 
										Type),
    case Type of
	'ANY_EVENT' ->
	    'CosNotifyChannelAdmin_ProxyPullSupplier':connect_any_pull_consumer(Proxy, PC);
	'STRUCTURED_EVENT' ->
	    'CosNotifyChannelAdmin_StructuredProxyPullSupplier':connect_structured_pull_consumer(Proxy, PC);
	'SEQUENCE_EVENT' ->
	    'CosNotifyChannelAdmin_SequenceProxyPullSupplier':connect_sequence_pull_consumer(Proxy, PC)
    end,
    Proxy.

%%---------------------------------------------------------------------%
%% function : connect_a_push_consumer
%% Arguments: Channel - CosNotifyChannelAdmin::EventChannel | undefined
%%            PC - a PushConsumer
%%            Type - 'ANY_EVENT' | 'STRUCTURED_EVENT' | 'SEQUENCE_EVENT'
%% Returns  : 
%% Effect   : 
%%----------------------------------------------------------------------
connect_a_push_consumer(undefined, _, _) ->
    corba:raise(#'CosNotifyChannelAdmin_ChannelNotFound'{});
connect_a_push_consumer(Channel, PC, Type) ->
    Admin = 
	'CosNotifyChannelAdmin_EventChannel':'_get_default_consumer_admin'(Channel),
    {Proxy, _Id} = 
	'CosNotifyChannelAdmin_ConsumerAdmin':obtain_notification_push_supplier(Admin, 
										Type),
    case Type of
	'ANY_EVENT' ->
	    'CosNotifyChannelAdmin_ProxyPushSupplier':connect_any_push_consumer(Proxy, PC);
	'STRUCTURED_EVENT' ->
	    'CosNotifyChannelAdmin_StructuredProxyPushSupplier':connect_structured_push_consumer(Proxy, PC);
	'SEQUENCE_EVENT' ->
	    'CosNotifyChannelAdmin_SequenceProxyPushSupplier':connect_sequence_push_consumer(Proxy, PC)
    end,
    Proxy.

%%---------------------------------------------------------------------%
%% function : connect_a_pull_supplier
%% Arguments: Channel - CosNotifyChannelAdmin::EventChannel | undefined
%%            PC - a PullSupplier
%%            Type - 'ANY_EVENT' | 'STRUCTURED_EVENT' | 'SEQUENCE_EVENT'
%% Returns  : 
%% Effect   : 
%%----------------------------------------------------------------------
connect_a_pull_supplier(undefined, _, _) ->
    corba:raise(#'CosNotifyChannelAdmin_ChannelNotFound'{});
connect_a_pull_supplier(Channel, PS, Type) ->
    Admin = 
	'CosNotifyChannelAdmin_EventChannel':'_get_default_supplier_admin'(Channel),
    {Proxy, _Id} = 
	'CosNotifyChannelAdmin_SupplierAdmin':obtain_notification_pull_consumer(Admin, 
										Type),
    case Type of
	'ANY_EVENT' ->
	    'CosNotifyChannelAdmin_ProxyPullConsumer':connect_any_pull_supplier(Proxy, PS);
	'STRUCTURED_EVENT' ->
	    'CosNotifyChannelAdmin_StructuredProxyPullConsumer':connect_structured_pull_supplier(Proxy, PS);
	'SEQUENCE_EVENT' ->
	    'CosNotifyChannelAdmin_SequenceProxyPullConsumer':connect_sequence_pull_supplier(Proxy, PS)
    end,
    Proxy.

%%---------------------------------------------------------------------%
%% function : connect_a_push_supplier
%% Arguments: Channel - CosNotifyChannelAdmin::EventChannel | undefined
%%            PC - a PushSupplier
%%            Type - 'ANY_EVENT' | 'STRUCTURED_EVENT' | 'SEQUENCE_EVENT'
%% Returns  : 
%% Effect   : 
%%----------------------------------------------------------------------
connect_a_push_supplier(undefined, _, _) ->
    corba:raise(#'CosNotifyChannelAdmin_ChannelNotFound'{});
connect_a_push_supplier(Channel, PS, Type) ->
    Admin = 
	'CosNotifyChannelAdmin_EventChannel':'_get_default_supplier_admin'(Channel),
    {Proxy, _Id} = 
	'CosNotifyChannelAdmin_SupplierAdmin':obtain_notification_push_consumer(Admin, 
										Type),
    case Type of
	'ANY_EVENT' ->
	    'CosNotifyChannelAdmin_ProxyPushConsumer':connect_any_push_supplier(Proxy, PS);
	'STRUCTURED_EVENT' ->
	    'CosNotifyChannelAdmin_StructuredProxyPushConsumer':connect_structured_push_supplier(Proxy, PS);
	'SEQUENCE_EVENT' ->
	    'CosNotifyChannelAdmin_SequenceProxyPushConsumer':connect_sequence_push_supplier(Proxy, PS)
    end,
    Proxy.


%%======================================================================
%% END OF MODULE
%%======================================================================

