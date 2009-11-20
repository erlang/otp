%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2009. All Rights Reserved.
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
%% File        : oe_CosEventComm_PullerS_impl.erl
%% Description : 
%%
%%----------------------------------------------------------------------
-module(oe_CosEventComm_PullerS_impl).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("orber/include/corba.hrl").
-include("CosEventChannelAdmin.hrl").
-include("CosEventComm.hrl").
-include("cosEventApp.hrl").

%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([init/1,
	 terminate/2,
	 code_change/3,
         handle_info/2]).
%% Exports from "CosEventChannelAdmin::ProxyPullSupplier"
-export([connect_pull_consumer/4]).
 
%% Exports from "CosEventComm::PullSupplier"
-export([pull/3, 
	 try_pull/3, 
	 disconnect_pull_supplier/3]).

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
%% Exports from "oe_CosEventComm::Event
-export([send/3, send_sync/4]).

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------
-record(state, {admin_pid, client, db, respond_to, typecheck, maxevents}).

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------

%%======================================================================
%% External functions
%%======================================================================
%%---------------------------------------------------------------------%
%% Function   : init/1
%% Returns    : {ok, State}          |
%%              {ok, State, Timeout} |
%%              ignore               |
%%              {stop, Reason}
%% Description: Initiates the server
%%----------------------------------------------------------------------
init([AdminPid, TypeCheck, MaxEvents]) ->
    process_flag(trap_exit, true),
    {ok, #state{admin_pid = AdminPid, 
		db = ets:new(oe_ets, [set, private, ordered_set]),
		typecheck = TypeCheck, maxevents = MaxEvents}}.

%%---------------------------------------------------------------------%
%% function : handle_info
%% Arguments: 
%% Returns  : {noreply, State} | 
%%            {stop, Reason, State}
%% Effect   : Functions demanded by the gen_server module.
%%            The CosEvent specification states:
%% "A nil object reference may be passed to the connect_pull_consumer operation; 
%%  if so a channel cannot invoke a disconnect_pull_consumer operation on the 
%%  consumer; the consumer may be disconnected from the channel without being 
%%  informed." 
%%            If we would invoke the disconnect_pull_consumer operation
%%            at the same time as the client tries to pull an event it
%%            would cause a dead-lock. We can solve this by spawning a process
%%            but as is the client will discover that the object no longer exists
%%            the next time it tries to pull an event.
%%----------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, #state{admin_pid = Pid} = State) ->
    orber:dbg("[~p] oe_CosEventComm_PullerS_impl:handle_info(~p);~n"
	      "My Admin terminated and so will I.", 
	      [?LINE, Reason], ?DEBUG_LEVEL),
    {stop, Reason, State};
handle_info(_Info, State) ->
    ?DBG("Unknown Info ~p~n", [_Info]),
    {noreply, State}.

%%---------------------------------------------------------------------%
%% Function   : terminate/2
%% Returns    : any (ignored by gen_server)
%% Description: Shutdown the server
%%----------------------------------------------------------------------
terminate(_Reason, #state{client = undefined, respond_to = undefined, db = DB}) ->
    ?DBG("Terminating ~p; no client connected and no pending pull's.~n", [_Reason]),
    ets:delete(DB),
    ok;
terminate(_Reason, #state{client = undefined, respond_to = ReplyTo, db = DB}) ->
    ?DBG("Terminating ~p; no client connected but a pending pull.~n", [_Reason]),
    corba:reply(ReplyTo, {'EXCEPTION', #'CosEventComm_Disconnected'{}}),
    ets:delete(DB),
    ok;
terminate(_Reason, #state{client = Client, respond_to = undefined, db = DB}) ->
    ?DBG("Terminating ~p; no pending pull~n", [_Reason]),
    cosEventApp:disconnect('CosEventComm_PullConsumer', 
			   disconnect_pull_consumer, Client),
    ets:delete(DB),
    ok;
terminate(_Reason, #state{client = Client, respond_to = ReplyTo, db = DB}) ->
    ?DBG("Terminating ~p; pending pull~n", [_Reason]),
    corba:reply(ReplyTo, {'EXCEPTION', #'CosEventComm_Disconnected'{}}),
    cosEventApp:disconnect('CosEventComm_PullConsumer', 
			   disconnect_pull_consumer, Client),
    ets:delete(DB),
    ok.

%%---------------------------------------------------------------------%
%% Function   : code_change/3
%% Returns    : {ok, NewState}
%% Description: Convert process state when code is changed
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%---------------------------------------------------------------------%
%% Function   : connect_pull_consumer
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
connect_pull_consumer(_OE_This, _OE_From, #state{client = undefined, 
					       typecheck = TypeCheck} = State, 
		      NewClient) ->
    case corba_object:is_nil(NewClient) of
	true ->
	    ?DBG("A NIL client supplied.~n", []),
	    {reply, ok, State};
	false ->
	    cosEventApp:type_check(NewClient, 'CosEventComm_PullConsumer', TypeCheck),
	    ?DBG("Connected to client.~n", []),
	    {reply, ok, State#state{client = NewClient}}
    end;
connect_pull_consumer(_, _, _, _) ->
    corba:raise(#'CosEventChannelAdmin_AlreadyConnected'{}).

		    
%%---------------------------------------------------------------------%
%% Function   : pull
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
pull(_OE_This, OE_From, State) ->
    case get_event(State#state.db) of
	false ->
	    ?DBG("pull invoked but no event stored; put the client on hold.~n", []),
	    {noreply, State#state{respond_to = OE_From}};
	Event ->
	    ?DBG("pull invoked and returned: ~p~n", [Event]),
	    {reply, Event, State}
    end.

%%---------------------------------------------------------------------%
%% Function   : try_pull
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
try_pull(_OE_This, _OE_From, State) ->
    case get_event(State#state.db) of
	false ->
	    ?DBG("try_pull invoked but no event stored.~n", []),
	    {reply, {any:create(orber_tc:long(), 0), false}, State};
	Event ->
	    ?DBG("try_pull invoked and returned: ~p~n", [Event]),
	    {reply, {Event, true}, State}
    end.

%%---------------------------------------------------------------------%
%% Function   : disconnect_pull_supplier
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
disconnect_pull_supplier(_OE_This, _OE_From, State) ->
    ?DBG("Disconnect invoked ~p ~n", [State]),
    {stop, normal, ok, State#state{client = undefined}}.


%%======================================================================
%% Internal functions
%%======================================================================
%%---------------------------------------------------------------------%
%% Function   : send
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
send(_OE_This, #state{respond_to = undefined} = State, Any) ->
    ?DBG("Received event ~p and stored it.~n", [Any]),
    store_event(State#state.db, State#state.maxevents, Any),
    {noreply, State};
send(_OE_This, State, Any) ->
    ?DBG("Received event ~p and sent it to pending client.~n", [Any]),
    corba:reply(State#state.respond_to, Any),
    {noreply, State#state{respond_to = undefined}}.

%%---------------------------------------------------------------------%
%% Function   : send_sync
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
send_sync(_OE_This, _OE_From, #state{respond_to = undefined} = State, Any) ->
    ?DBG("Received event ~p and stored it (sync).~n", [Any]),
    store_event(State#state.db, State#state.maxevents, Any),
    {reply, ok, State};
send_sync(_OE_This, _OE_From, State, Any) ->
    ?DBG("Received event ~p and sent it to pending client (sync).~n", [Any]),
    corba:reply(State#state.respond_to, Any),
    {reply, ok, State#state{respond_to = undefined}}.


%%---------------------------------------------------------------------%
%% Function   : store_event
%% Arguments  : DB - ets reference
%%              Event - CORBA::Any
%% Returns    : true
%% Description: Insert the event in FIFO order.
%%----------------------------------------------------------------------
store_event(DB, Max, Event) ->
    case ets:info(DB, size) of
	CurrentSize when CurrentSize < Max -> 
	    ets:insert(DB, {now(), Event}); 
	_ ->
	    orber:dbg("[~p] oe_CosEventComm_PullerS:store_event(~p); DB full drop event.", 
		      [?LINE, Event], ?DEBUG_LEVEL),
	    true
    end.

%%---------------------------------------------------------------------%
%% Function   : get_event
%% Arguments  : DB - ets reference
%%              Event - CORBA::Any
%% Returns    : false | Event (CORBA::Any)
%% Description: Lookup event in FIFO order; return false if no event exists.
%%----------------------------------------------------------------------
get_event(DB) ->
    case ets:first(DB) of
	'$end_of_table' ->
	    false;
	Key ->
	    [{_, Event}] = ets:lookup(DB, Key),
	    ets:delete(DB, Key),
	    Event
    end.

%%======================================================================
%% END OF MODULE
%%======================================================================
