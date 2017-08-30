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
%% File        : CosEventChannelAdmin_ProxyPushConsumer_impl.erl
%% Description : 
%%
%%----------------------------------------------------------------------
-module('CosEventChannelAdmin_ProxyPushConsumer_impl').

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
 
%% Exports from "CosEventChannelAdmin::ProxyPushConsumer"
-export([connect_push_supplier/3]).
 
%% Exports from "CosEventComm::PushConsumer"
-export([push/3, 
         disconnect_push_consumer/2]).
 
%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
 
%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------
-record(state, {admin, admin_pid, channel, client, typecheck}).
 
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
init([Admin, AdminPid, Channel, TypeCheck]) ->
    process_flag(trap_exit, true),
    {ok, #state{admin = Admin, admin_pid = AdminPid, channel = Channel, 
		typecheck = TypeCheck}}.
 
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
    cosEventApp:disconnect('CosEventComm_PushSupplier', 
			   disconnect_push_supplier, Client),
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
%% Effect   : If the Parnet Admin or the Channel terminates so must this object.
%%----------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, #state{admin_pid = Pid} = State) ->
    ?DBG("Parent Admin terminated ~p~n", [Reason]),
    orber:dbg("[~p] CosEventChannelAdmin_ProxyPushConsumer:handle_info(~p);~n"
	      "My Admin terminated and so will I.", 
	      [?LINE, Reason], ?DEBUG_LEVEL),
    {stop, Reason, State};
handle_info(_Info, State) ->
    ?DBG("Unknown Info ~p~n", [_Info]),
    {noreply, State}.
 
%%----------------------------------------------------------------------
%% Function   : connect_push_supplier
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
connect_push_supplier(_OE_This, #state{client = undefined, 
				       typecheck = TypeCheck} = State, NewClient) ->
    case corba_object:is_nil(NewClient) of
	true ->
	    ?DBG("A NIL client supplied.~n", []),
	    {reply, ok, State};
	false ->
	    cosEventApp:type_check(NewClient, 'CosEventComm_PushSupplier', TypeCheck),
	    ?DBG("Connected to client.~n", []),
	    {reply, ok, State#state{client = NewClient}}
    end;
connect_push_supplier(_, _, _) ->
    corba:raise(#'CosEventChannelAdmin_AlreadyConnected'{}).	

 
%%----------------------------------------------------------------------
%% Function   : push
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
push(_OE_This, State, Any) ->
    %% We should not use corba:reply here since if we block incoming
    %% events this will prevent producers to flood the system.
    ?DBG("Received Event ~p and forwarded it successfully.~n", [Any]),
    'oe_CosEventComm_Channel':send_sync(State#state.channel, Any),
    {reply, ok, State}.
 
%%----------------------------------------------------------------------
%% Function   : disconnect_push_consumer
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
disconnect_push_consumer(_OE_This, State) ->
    ?DBG("Disconnect invoked ~p~n", [State]),
    {stop, normal, ok, State#state{client = undefined}}.
 
%%======================================================================
%% Internal functions
%%======================================================================
 
%%======================================================================
%% END OF MODULE
%%======================================================================
