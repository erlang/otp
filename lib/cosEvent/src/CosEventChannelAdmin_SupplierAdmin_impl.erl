%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
%% File        : CosEventChannelAdmin_SupplierAdmin_impl.erl
%% Created     : 21 Mar 2001
%% Description : 
%%
%%----------------------------------------------------------------------
-module('CosEventChannelAdmin_SupplierAdmin_impl').
 
 
%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include("cosEventApp.hrl").

 
%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
%% Mandatory
-export([init/1,
         terminate/2,
         code_change/3,
         handle_info/2]).
 
%% Exports from "CosEventChannelAdmin::SupplierAdmin"
-export([obtain_push_consumer/2, 
         obtain_pull_consumer/2]).
 
%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
 
%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------
-record(state, {channel, channel_pid, typecheck, pull_interval, server_options}).
 
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
init([Channel, ChannelPid, TypeCheck, PullInterval, ServerOpts]) ->
    process_flag(trap_exit, true),
    {ok, #state{channel = Channel, channel_pid = ChannelPid, typecheck = TypeCheck,
		pull_interval = PullInterval, server_options = ServerOpts}}.
 
%%----------------------------------------------------------------------
%% Function   : terminate/2
%% Returns    : any (ignored by gen_server)
%% Description: Shutdown the server
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ?DBG("Terminating ~p~n", [_Reason]),
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
handle_info({'EXIT', Pid, Reason}, #state{channel_pid = Pid} = State) ->
    ?DBG("Parent Channel terminated ~p~n", [Reason]),
    orber:dbg("[~p] CosEventChannelAdmin_SupplierAdmin:handle_info(~p);~n"
	      "My Channel terminated and so will I.", 
	      [?LINE, Reason], ?DEBUG_LEVEL),
    {stop, Reason, State};
handle_info(_Info, State) ->
    ?DBG("Unknown Info ~p~n", [_Info]),
    {noreply, State}.
 
 
%%----------------------------------------------------------------------
%% Function   : obtain_push_consumer
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
obtain_push_consumer(OE_This, #state{channel = Channel, 
				     channel_pid = _ChannelPid,
				     typecheck = TypeCheck,
				     server_options = ServerOpts} = State) ->
    ?DBG("Starting a new CosEventChannelAdmin_ProxyPushConsumer.~n", []),
    {reply, 
     'CosEventChannelAdmin_ProxyPushConsumer':oe_create_link([OE_This, 
                                                              self(),
                                                              Channel,
							      TypeCheck],
							     ServerOpts), 
     State}.
 
%%----------------------------------------------------------------------
%% Function   : obtain_pull_consumer
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
obtain_pull_consumer(OE_This, #state{channel = Channel, 
				     channel_pid = _ChannelPid,
				     typecheck = TypeCheck,
				     pull_interval= PullInterval,
				     server_options = ServerOpts} = State) ->
    ?DBG("Starting a new CosEventChannelAdmin_ProxyPullConsumer.~n", []),
    {reply, 
     'CosEventChannelAdmin_ProxyPullConsumer':oe_create_link([OE_This, 
                                                              self(),
                                                              Channel,
							      TypeCheck,
							      PullInterval],
							     ServerOpts), 
     State}.
 
%%======================================================================
%% Internal functions
%%======================================================================
 
%%======================================================================
%% END OF MODULE
%%======================================================================
