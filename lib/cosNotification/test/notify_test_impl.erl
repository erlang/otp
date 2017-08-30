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
%% File    : notify_test_impl.erl
%%----------------------------------------------------------------------

-module(notify_test_impl).

-include_lib("orber/include/corba.hrl").
-include("idl_output/notify_test.hrl").

%%--------------- specified functions ------------------------
-export([stop_normal/2, 
	 stop_brutal/2, 
	 print/2, 
	 doAction/3,
	 delay/5,
	 %% Exports from CosNotifyComm::StructuredPushConsumer
	 push_structured_event/3, disconnect_structured_push_consumer/2,
	 %% Exports from "CosNotifyComm::SequencePushConsumer"
	 push_structured_events/3, disconnect_sequence_push_consumer/2,
	 %% Exports from CosEventComm::PushConsumer
	 push/3, disconnect_push_consumer/2,
	 %% Exports from CosNotifyComm::NotifyPublish
	 disconnect_sequence_pull_consumer/2,
	 %% Exports from CosNotifyComm::StructuredPullConsumer
	 disconnect_structured_pull_consumer/2,
	 %% Exports from CosEventComm::PullConsumer
	 disconnect_pull_consumer/2,
	 %% Exports from CosNotifyComm::SequencePushSupplier
	 disconnect_sequence_push_supplier/2,
	 %% Exports from CosNotifyComm::StructuredPushSupplier
	 disconnect_structured_push_supplier/2,
	 %% Exports from CosEventComm::PushSupplier
	 disconnect_push_supplier/2,
	 %% Exports from CosNotifyComm::SequencePullSupplier
	 pull_structured_events/3, 
	 try_pull_structured_events/3, 
	 disconnect_sequence_pull_supplier/2,
	 %% Exports from CosNotifyComm::StructuredPullSupplier
	 pull_structured_event/2, 
	 try_pull_structured_event/2, 
	 disconnect_structured_pull_supplier/2,
	 %% Exports from CosEventComm::PullSupplier
	 pull/2, 
	 try_pull/2, 
	 disconnect_pull_supplier/2,
	 %% Exports from CosNotifyComm::SequencePullConsumer
	 offer_change/4,
	 %% Exports from CosNotifyComm::NotifySubscribe
	 subscription_change/4]).

%%--------------- gen_server specific ------------------------
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
%% Data structures
-record(state, {myType, proxy, data, action}).

%%--------------- LOCAL DATA ---------------------------------

%%------------------------------------------------------------
%% function : init, terminate
%%------------------------------------------------------------
init([MyType, Proxy]) ->
    process_flag(trap_exit,true),
    {ok, #state{myType=MyType, proxy=Proxy, data=[]}}.

terminate(Reason, State) ->
    io:format("notify_test:terminate(~p  ~p)~n",[Reason, State#state.myType]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
handle_call(_,_, State) ->
    {noreply, State}.
handle_cast(_, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------- SERVER FUNCTIONS ---------------------------

print(Self, State) ->
    io:format("notify_test:print(~p  ~p)~n",[Self, State]),
    {reply, ok, State}.

doAction(_Self, State, {set_data, Data}) ->
    io:format("notify_test:doAction(add_data)  ~p~n",[Data]),
    {reply, ok, State#state{data=Data}};
doAction(_Self, State, {add_data, Data}) ->
    io:format("notify_test:doAction(add_data)  ~p~n",[Data]),
    {reply, ok, State#state{data=State#state.data++Data}};
doAction(_Self, State, return_data) ->
    io:format("notify_test:doAction(return_data)~n",[]),
    {reply, State#state.data, State#state{data=[]}};
doAction(_Self, State, clear_data) ->
    io:format("notify_test:doAction(return_data)~n",[]),
    {reply, ok, State#state{data=[]}};
doAction(_Self, State, pull_any) ->
    io:format("notify_test:doAction(pull_any)~n",[]),
    Event='CosNotifyChannelAdmin_ProxyPullSupplier':pull(State#state.proxy),
    {reply, Event, State};
doAction(_Self, State, {pull_seq, Max}) ->
    io:format("notify_test:doAction(pull_sequence)~n",[]),
    Event='CosNotifyChannelAdmin_SequenceProxyPullSupplier':pull_structured_events(State#state.proxy, Max),
    {reply, Event, State};
doAction(_Self, State, pull_str) ->
    Event='CosNotifyChannelAdmin_StructuredProxyPullSupplier':pull_structured_event(State#state.proxy),
    io:format("notify_test:doAction(pull_structured)~n",[]),
    {reply, Event, State};
doAction(_Self, State, try_pull_any) ->
    io:format("notify_test:doAction(try_pull_any)~n",[]),
    Event='CosNotifyChannelAdmin_ProxyPullSupplier':try_pull(State#state.proxy),
    {reply, Event, State};
doAction(_Self, State, {try_pull_seq, Max}) ->
    io:format("notify_test:doAction(try_pull_sequence)~n",[]),
    Event='CosNotifyChannelAdmin_SequenceProxyPullSupplier':try_pull_structured_events(State#state.proxy, Max),
    {reply, Event, State};
doAction(_Self, State, try_pull_str) ->
    Event='CosNotifyChannelAdmin_StructuredProxyPullSupplier':try_pull_structured_event(State#state.proxy),
    io:format("notify_test:doAction(try_pull_structured)~n",[]),
    {reply, Event, State};
doAction(_Self, State, {action, Action}) ->
    io:format("notify_test:doAction(~p)~n",[Action]),
    {reply, ok, State#state{action = Action}};

doAction(_, State, _) ->
    {reply, nop, State}.

stop_normal(_Self, State) ->
    {stop, normal, ok, State}.

stop_brutal(_Self, _State) ->
    exit("killed_brutal").



%%--------------- CosNotifyComm::NotifyPublish --------
offer_change(_Self, State, Added, Removed) ->
    ND=loop(Removed, State#state.data),
    ND2=Added++ND,
    {reply, ok, State#state{data=ND2}}.

loop([],Data) ->
    Data;
loop([H|T], Data) ->
    ND=lists:delete(H,Data),
    loop(T, ND).

%%--------------- CosNotifyComm::NotifySubscribe --------
subscription_change(_Self, State, Added, Removed) ->
    ND=loop(Removed, State#state.data),
    ND2=Added++ND,
    {reply, ok, State#state{data=ND2}}.

%%--------------- CosNotifyComm::SequencePushConsumer --------
push_structured_events(_Self, #state{action = undefined} = State, Event) ->
    io:format("notify_test:push_structured_events(~p)~n",[Event]),
    {reply, ok, State#state{data=State#state.data++Event}};
push_structured_events(_Self, #state{action = Action} = State, Event) ->
    io:format("notify_test:push_structured_events(~p)~nAction: ~p~n",
	      [Event, Action]),
    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO}),
    {reply, ok, State#state{data=State#state.data++Event}}.
disconnect_sequence_push_consumer(_Self, State) ->
    io:format("disconnect_sequence_push_consumer~n",[]),
    {stop, normal, ok, State}.

%%--------------- CosNotifyComm::StructuredPushConsumer --------
push_structured_event(_Self, State, Event) ->
    io:format("notify_test:push_structured_event(~p)~n",[Event]),
    {reply, ok, State#state{data=State#state.data++[Event]}}.
disconnect_structured_push_consumer(_Self, State) ->
    io:format("disconnect_structured_push_consumer~n",[]),
    {stop, normal, ok, State}.

%%--------------- CosEventComm::PushConsumer --------
push(_Self, State, Event) ->
    io:format("notify_test:push(~p)~n",[Event]),
    {reply, ok, State#state{data=State#state.data++[Event]}}.
disconnect_push_consumer(_Self, State) ->
    io:format("disconnect_push_consumer~n",[]),
    {stop, normal, ok, State}.

%%--------------- CosNotifyComm::SequencePullConsumer --------
disconnect_sequence_pull_consumer(_Self, State) ->
    io:format("disconnect_sequence_pull_consumer~n",[]),
    {stop, normal, ok, State}.

%%--------------- CosNotifyComm::StructuredPullConsumer --------
disconnect_structured_pull_consumer(_Self, State) ->
    io:format("disconnect_structured_pull_consumer~n",[]),
    {stop, normal, ok, State}.

%%--------------- CosEventComm::PullConsumer --------
disconnect_pull_consumer(_Self, State) ->
    io:format("disconnect_pull_consumer~n",[]),
    {stop, normal, ok, State}.

%%--------------- CosNotifyComm::SequencePushSupplier --------
disconnect_sequence_push_supplier(_Self, State) ->
    io:format("disconnect_sequence_push_supplier~n",[]),
    {stop, normal, ok, State}.

%%--------------- CosNotifyComm::StructuredPushSupplier --------
disconnect_structured_push_supplier(_Self, State) ->
    io:format("disconnect_structured_push_supplier~n",[]),
    {stop, normal, ok, State}.

%%--------------- CosEventComm::PushSupplier --------
disconnect_push_supplier(_Self, State) ->
    io:format("disconnect_push_supplier~n",[]),
    {stop, normal, ok, State}.

%%--------------- CosNotifyComm::SequencePullSupplier --------
pull_structured_events(_Self, State, _Max) ->
    io:format("notify_test:pullstructured_events()~n",[]),
    {reply, ok, State}.
try_pull_structured_events(_Self, State, Max) ->
    io:format("notify_test:try_pull_structured_events()~n",[]),
    case State#state.data of
	[] ->
	    {reply, {[],false}, State};
	List ->
	    R = split(List,Max),
	    {reply, {lists:sublist(List, Max), true}, State#state{data=R}}
    end.

split([],_) ->
    [];
split(R,0) ->
    R;
split([_H|T],Max) ->
    split(T, Max-1).

disconnect_sequence_pull_supplier(_Self, State) ->
    io:format("disconnect_sequence_pull_supplier~n",[]),
    {stop, normal, ok, State}.

%%--------------- CosNotifyComm::StructuredPullSupplier --------
pull_structured_event(_Self, State) ->
    io:format("notify_test:pull_structured_event()~n",[]),
    {reply, ok, State}.
try_pull_structured_event(_Self, State) ->
    io:format("notify_test:try_pull_structured_event()~n",[]),
    case State#state.data of
	[] ->
	    {reply, {[],false}, State};
	[H|T] ->
	    {reply, {H, true}, State#state{data=T}}
    end.
disconnect_structured_pull_supplier(_Self, State) ->
    io:format("disconnect_structured_pull_supplier~n",[]),
    {stop, normal, ok, State}.

%%--------------- CosEventComm::PullSupplier --------
pull(_Self, State) ->
    io:format("notify_test:pull()~n",[]),
    {reply, 'CosEventComm_PullSupplier':pull(State#state.proxy), State}.
try_pull(_Self, State) ->
    io:format("notify_test:try_pull()~n",[]),
    case State#state.data of
	[] ->
	    {reply, {[],false}, State};
	[H|T] ->
	    {reply, {H, true}, State#state{data=T}}
    end.
disconnect_pull_supplier(_Self, State) ->
    io:format("disconnect_pull_supplier~n",[]),
    {stop, normal, ok, State}.

%%--------------- LOCAL FUNCTIONS ----------------------------

delay(Obj, Event, Time, Mod, F) ->
    io:format("notify_test:delay(~p)  TIME: ~p~n",[Event, erlang:timestamp()]),
    timer:sleep(Time),
    Mod:F(Obj, Event),
    io:format("notify_test:delay() DONE: ~p~n",[erlang:timestamp()]),
    ok.

%%--------------- END OF MODULE ------------------------------

