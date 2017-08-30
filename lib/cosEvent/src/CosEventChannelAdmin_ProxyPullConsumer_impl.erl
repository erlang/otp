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
%% File        : CosEventChannelAdmin_ProxyPullConsumer_impl.erl
%% Description : 
%%
%%----------------------------------------------------------------------
-module('CosEventChannelAdmin_ProxyPullConsumer_impl').

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include("CosEventChannelAdmin.hrl").
-include("CosEventComm.hrl").
-include("cosEventApp.hrl").

%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
%% Mandatory
-export([init/1,
         terminate/2,
         code_change/3,
         handle_info/2]).
 
%% Interface functions
-export([connect_pull_supplier/3]).
 
%% Exports from "CosEventComm::PullConsumer"
-export([disconnect_pull_consumer/2]).


%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
 
%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------
-record(state, {admin, admin_pid, channel, client, 
		typecheck, pull_interval, timer_ref}).
 
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
init([Admin, AdminPid, Channel, TypeCheck, PullInterval]) ->
    process_flag(trap_exit, true),
    Secs = timer:seconds(PullInterval),
    timer:start(),
    {ok, #state{admin = Admin, admin_pid = AdminPid, channel = Channel, 
		typecheck = TypeCheck, pull_interval = Secs}}.
 
%%----------------------------------------------------------------------
%% Function   : terminate/2
%% Returns    : any (ignored by gen_server)
%% Description: Shutdown the server
%%----------------------------------------------------------------------
terminate(_Reason, #state{client = undefined}) ->
    ?DBG("Terminating ~p; no client connected.~n", [_Reason]),
    ok;
terminate(_Reason, #state{client = Client} = State) ->
    stop_timer(State),
    ?DBG("Terminating ~p~n", [_Reason]),
    cosEventApp:disconnect('CosEventComm_PullSupplier', 
			   disconnect_pull_supplier, Client),
    ok.
 
%%----------------------------------------------------------------------
%% Function   : code_change/3
%% Returns    : {ok, NewState}
%% Description: Convert process state when code is changed
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%%---------------------------------------------------------------------%
%% function : handle_info
%% Arguments: 
%% Returns  : {noreply, State} | 
%%            {stop, Reason, State}
%% Effect   : If the Parent Admin or the Channel terminates so must this object.
%%----------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, #state{admin_pid = Pid} = State) ->
    ?DBG("Parent Admin terminated ~p~n", [Reason]),
    orber:dbg("[~p] CosEventChannelAdmin_ProxyPullConsumer:handle_info(~p);~n"
	      "My Admin terminated and so will I.", [?LINE, Reason], ?DEBUG_LEVEL),
    {stop, Reason, State};
handle_info(try_pull_event, State) ->
    try_pull_event(State);
handle_info(_Info, State) ->
    ?DBG("Unknown Info ~p~n", [_Info]),
    {noreply, State}.
 
%%----------------------------------------------------------------------
%% Function   : connect_pull_supplier
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
connect_pull_supplier(_OE_This, #state{client = undefined, 
				       typecheck = TypeCheck} = State, NewClient) ->
    case corba_object:is_nil(NewClient) of
	true ->
	    ?DBG("A NIL client supplied.~n", []),
	    orber:dbg("[~p] CosEventChannelAdmin_ProxyPullConsumer:connect_pull_supplier(..);~n"
		      "Supplied a NIL reference which is not allowed.", 
		      [?LINE], ?DEBUG_LEVEL),
	    corba:raise(#'BAD_PARAM'{completion_status = ?COMPLETED_NO});
	false ->
	    cosEventApp:type_check(NewClient, 'CosEventComm_PullSupplier', TypeCheck),
	    NewState = start_timer(State),
	    {reply, ok, NewState#state{client = NewClient}}
    end;
connect_pull_supplier(_, _, _) ->
    corba:raise(#'CosEventChannelAdmin_AlreadyConnected'{}).


%%----------------------------------------------------------------------
%% Function   : disconnect_pull_consumer
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
disconnect_pull_consumer(_OE_This, State) ->
    NewState = stop_timer(State),
    ?DBG("Disconnect invoked ~p~n", [NewState]),
    {stop, normal, ok, NewState#state{client = undefined}}.
 
%%======================================================================
%% Internal functions
%%======================================================================
%% Start timer which send a message each time we should pull for new events.
start_timer(State) ->
    case catch timer:send_interval(State#state.pull_interval, try_pull_event) of
	{ok,PullTRef} ->
	    ?DBG("Started timer: ~p~n", [State#state.pull_interval]),
	    State#state{timer_ref =  PullTRef};
	_ ->
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.
stop_timer(#state{timer_ref = undefined} = State) ->
    ?DBG("No timer to stop~n",[]),
    State;
stop_timer(State) ->
    ?DBG("Stopped timer~n",[]),
    timer:cancel(State#state.timer_ref),
    State#state{timer_ref = undefined}.


try_pull_event(State) ->
    case catch 'CosEventComm_PullSupplier':try_pull(State#state.client) of
	{_,false} ->
	    ?DBG("Client did not supply event~n", []),
	    {noreply, State}; 
	{Any, true} ->
	    'oe_CosEventComm_Channel':send_sync(State#state.channel, Any),
	    ?DBG("Received Event ~p and forwarded it successfully.~n", [Any]),
	    {noreply, State}; 
	{'EXCEPTION', #'CosEventComm_Disconnected'{}} ->
	    ?DBG("Client claims we are disconnectedwhen trying to pull event.~n", []),
	    orber:dbg("[~p] CosEventChannelAdmin_ProxyPullConsumer:try_pull_event();~n"
		      "Client claims we are disconnected when trying to pull event so I terminate.", 
		      [?LINE], ?DEBUG_LEVEL),
	    {stop, normal, State#state{client = undefined}};
	What ->
	    orber:dbg("[~p] CosEventChannelAdmin_ProxyPullConsumer:try_pull_event(~p);~n"
		      "My Client behaves badly so I terminate.", 
		      [?LINE, What], ?DEBUG_LEVEL),
	    {stop, normal, State}
    end.


%%======================================================================
%% END OF MODULE
%%======================================================================
