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
%% File        : oe_CosEventComm_PusherS_impl.erl
%% Description : 
%%
%%----------------------------------------------------------------------
-module(oe_CosEventComm_PusherS_impl).

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

%% Exports from "CosEventChannelAdmin::ProxyPushSupplier"
-export([connect_push_consumer/4]).
 
%% Exports from "CosEventComm::PushSupplier"
-export([disconnect_push_supplier/3]).
 

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
%% Exports from "oe_CosEventComm::Event"
-export([send/3, send_sync/4]).

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------
-record(state, {admin_pid, client, typecheck}).

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
init([AdminPid, TypeCheck]) ->
    process_flag(trap_exit, true),
    {ok, #state{admin_pid = AdminPid, typecheck = TypeCheck}}.

%%----------------------------------------------------------------------
%% Function   : terminate/2
%% Returns    : any (ignored by gen_server)
%% Description: Shutdown the server
%%----------------------------------------------------------------------
terminate(_Reason, #state{client = undefined}) ->
    ?DBG("Terminating ~p; no client connected.~n", [_Reason]),
    ok;
terminate(_Reason, #state{client = Client} = _State) ->
    ?DBG("Terminating ~p~n", [_Reason]),
    cosEventApp:disconnect('CosEventComm_PushConsumer', 
			   disconnect_push_consumer, Client),
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
%% Effect   : Functions demanded by the gen_server module. 
%%----------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, #state{admin_pid = Pid} = State) ->
    ?DBG("Parent Admin terminated ~p~n", [Reason]),
    orber:dbg("[~p] oe_CosEventComm_PusherS_impl:handle_info(~p);~n"
	      "My Admin terminated and so will I.", 
	      [?LINE, Reason], ?DEBUG_LEVEL),
    {stop, Reason, State};
handle_info(_Info, State) ->
    ?DBG("Unknown Info ~p~n", [_Info]),
    {noreply, State}.

%%---------------------------------------------------------------------%
%% Function   : connect_push_consumer
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
connect_push_consumer(_OE_This, _, #state{client = undefined, 
					  typecheck = TypeCheck} = State, NewClient) ->
    case corba_object:is_nil(NewClient) of
	true ->
	    orber:dbg("[~p] oe_CosEventComm_PusherS_impl:connect_push_consumer(..);~n"
		      "Supplied a NIL reference which is not allowed.", 
		      [?LINE], ?DEBUG_LEVEL),
	    corba:raise(#'BAD_PARAM'{completion_status = ?COMPLETED_NO});
	false ->
	    cosEventApp:type_check(NewClient, 'CosEventComm_PushConsumer', TypeCheck),
	    ?DBG("Connected to client.~n", []),
	    {reply, ok, State#state{client = NewClient}}
    end;
connect_push_consumer(_, _, _, _) ->
    corba:raise(#'CosEventChannelAdmin_AlreadyConnected'{}).


%%---------------------------------------------------------------------%
%% Function   : disconnect_push_supplier
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
disconnect_push_supplier(_OE_This, _, State) ->
    ?DBG("Disconnect invoked ~p ~n", [State]),
    {stop, normal, ok, State#state{client = undefined}}.

%%======================================================================
%% Internal functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function   : send
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
send(_OE_This, #state{client = undefined} = State, _Any) ->
    %% No consumer connected.
    ?DBG("Received event ~p but have no client.~n", [_Any]),
    {noreply, State};
send(_OE_This, #state{client = Client} = State, Any) ->
    %% Push Data
    case catch 'CosEventComm_PushConsumer':push(Client, Any) of
	ok ->
	    ?DBG("Received event ~p and delivered it client.~n", [Any]),
	    {noreply, State};
	{'EXCEPTION', #'CosEventComm_Disconnected'{}} ->
	    ?DBG("Received event ~p but failed to deliver it since the client claims we are disconnected.~n", [Any]),
	    {stop, normal, State#state{client = undefined}};
	Other ->
	    ?DBG("Received event ~p but failed to deliver it to client.~n", [Any]),
	    orber:dbg("[~p] oe_CosEventComm_PusherS_impl:send(~p);~n"
		      "My Client behaves badly, returned ~p, so I will terminate.", 
		      [?LINE, Any, Other], ?DEBUG_LEVEL),
	    {stop, normal, State}
    end.


%%----------------------------------------------------------------------
%% Function   : send_sync
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
send_sync(_OE_This, _OE_From, #state{client = undefined} = State, _Any) ->
    %% No consumer connected.
    ?DBG("Received event ~p but have no client.~n", [_Any]),
    {reply, ok, State};
send_sync(_OE_This, OE_From, #state{client = Client} = State, Any) ->
    corba:reply(OE_From, ok),
    %% Push Data
    case catch 'CosEventComm_PushConsumer':push(Client, Any) of
	ok ->
	    ?DBG("Received event ~p and delivered (sync) it client.~n", [Any]),
	    {noreply, State};
	{'EXCEPTION', #'CosEventComm_Disconnected'{}} ->
	    ?DBG("Received event ~p but failed to deliver (sync) it since the client claims we are disconnected.~n", [Any]),
	    {stop, normal, State#state{client = undefined}};
	Other ->
	    ?DBG("Received event ~p but failed to deliver (sync) it to client.~n", [Any]),
	    orber:dbg("[~p] oe_CosEventComm_PusherS_impl:send_sync(~p);~n"
		      "My Client behaves badly, returned ~p, so I will terminate.", 
		      [?LINE, Any, Other], ?DEBUG_LEVEL),
	    {stop, normal, State}
    end.


%%======================================================================
%% END OF MODULE
%%======================================================================

