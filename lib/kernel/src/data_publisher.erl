%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2022-2025. All Rights Reserved.
%% Copyright WhatsApp Inc. and its affiliates. All rights reserved.
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
%%-------------------------------------------------------------------
%%
%% @author Zeyu Zhang <zzydxm@gmail.com>

-module(data_publisher).
-moduledoc """
A behavior for building eventually consistent, replicated data stores
across distributed nodes.

This module provides the infrastructure for:
- Replicating data across all nodes running the same scope
- Automatic peer discovery
- Subscription-based notifications for data changes
- Version translation for rolling upgrades

Implementing modules define how data is stored, updated, and how changes
are calculated and propagated. This is a generalization of the `pg` module.
""".

-export([
    start/2,
    start_link/2,
    start/3,
    start_link/3,
    update/2,
    subscribe/2,
    unsubscribe/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-type scope() :: atom().
-type options() :: #{standalone => boolean()}.
-type version() :: dynamic().
-type local_data() :: dynamic().
-type update() :: dynamic().
-type global_view() :: dynamic().
-type subscribe_result() :: dynamic().
-type subscription() :: dynamic().
-type subscriptions() :: #{subscription() => #{reference() => pid()}}.

-record(state, {
    scope :: atom(),
    module :: module(),
    options :: options(),
    version :: version(),
    global_view :: global_view(),
    local_data :: local_data(),
    peers = #{} :: #{pid() => {reference(), version(), local_data()}},
    subscriptions = #{} :: subscriptions(),
    subscribe_refs = #{} :: #{reference() => subscription()}
}).
-type state() :: #state{}.

% Current version of data_publisher instance
-callback version() -> version().
% Init custom publisher state, which should include a global view cache (e.g. an ETS table)
% of all nodes' local data, and potentially some internal state.
% For example, it can also store some monitors, so that it can automatically do some local data
% updates when monitored events happened, see the optional translate_message callback
-callback init_global_view(scope()) -> global_view().
% Init local data of one node
-callback init_local_data() -> local_data().
% Stop global view cache
-callback stop_global_view(global_view()) -> term().
% Apply an update to local data (used for internal transitions)
% Need to be backward compatible for all possible update() in older versions
-callback update_local_data(update(), local_data()) -> local_data().
% Update global view cache with a change from any node in the cluster,
% and notify subscribers for the update
% Need to be backward compatible for all possible update() in older versions
-callback update_global_view_and_notify(node(), update(), subscriptions(), global_view()) -> global_view().
% Calculate the diff (delta) between two local data
% Need to be backward compatible for all possible  New :: local_data() in older versions
-callback data_diff(Old :: local_data(), New :: local_data()) -> update().
% Return initial result for a new subscriber based on the global view cache
-callback new_subscription(subscription(), global_view()) -> subscribe_result().
% Translate an update to an older version
-callback translate_update(MyVersion :: version(), PeerVersion :: version(), update()) ->
    update()
    % legacy support, to be removed
    | {'$plain_messages', [dynamic()]}.
% Translate local_data to an older version
-callback translate_local_data(MyVersion :: version(), PeerVersion :: version(), local_data()) ->
    local_data()
    % legacy support, to be removed
    | {'$plain_message', dynamic()}.
% Translate cast / info message to a potential update, for example a monitor is DOWN
-callback translate_message(dynamic(), global_view()) ->
    {update, update(), global_view()}
    | {drop, global_view()}
    % legacy support, to be removed
    | {update, pid(), update(), global_view()}
    % legacy support, to be removed
    | {local_data, pid(), version(), local_data(), global_view()}
    % legacy support, to be removed
    | {discover, pid(), version(), global_view()}.

-optional_callbacks([
    translate_message/2
]).

-spec start(scope(), module()) -> gen_server:start_ret().
start(Scope, Module) ->
    start(Scope, Module, #{}).

-spec start_link(scope(), module()) -> gen_server:start_ret().
start_link(Scope, Module) ->
    start_link(Scope, Module, #{}).

-spec start(scope(), module(), options()) -> gen_server:start_ret().
start(Scope, Module, Options) ->
    gen_server:start({local, Scope}, ?MODULE, {Scope, Module, Options}, []).

-spec start_link(scope(), module(), options()) -> gen_server:start_ret().
start_link(Scope, Module, Options) ->
    gen_server:start_link({local, Scope}, ?MODULE, {Scope, Module, Options}, []).

-spec update(scope(), update()) -> ok.
update(Scope, Update) ->
    gen_server:call(Scope, {update, Update}, infinity).

-spec subscribe(scope(), subscription()) -> {reference(), subscribe_result()}.
subscribe(Scope, Subscription) ->
    gen_server:call(Scope, {subscribe, Subscription}, infinity).

-spec unsubscribe(scope(), reference()) -> ok.
unsubscribe(Scope, Ref) ->
    gen_server:call(Scope, {unsubscribe, Ref}, infinity).

-spec init({scope(), module(), options()}) -> {ok, state()}.
init({Scope, Module, Options}) ->
    Standalone = maps:get(standalone, Options, false),
    Standalone orelse (ok = net_kernel:monitor_nodes(true)),
    State = #state{
        scope = Scope,
        module = Module,
        options = Options,
        version = Module:version(),
        global_view = Module:init_global_view(Scope),
        local_data = Module:init_local_data()
    },
    %% discover all nodes running this scope in the cluster
    _ = Standalone orelse [do_send({Scope, Node}, {discover, self(), State#state.version}) || Node <- nodes()],
    {ok, State}.

-spec handle_call
    ({update, update()}, gen_server:from(), state()) -> {reply, ok, state()};
    ({subscribe, subscription()}, gen_server:from(), state()) -> {reply, {reference(), subscribe_result()}, state()};
    ({unsubscribe, reference()}, gen_server:from(), state()) -> {reply, ok, state()}.
handle_call({update, Update}, _From, State) ->
    {reply, ok, do_update(Update, State)};
handle_call({subscribe, Subscription}, {Pid, _Tag}, #state{module = Module, subscriptions = Subscriptions} = State) ->
    Ref = erlang:monitor(process, Pid, [{tag, {'DOWN', subscribe}}]),
    SubscribeResult = Module:new_subscription(Subscription, State#state.global_view),
    NewSubscriptions = Subscriptions#{Subscription => (maps:get(Subscription, Subscriptions, #{}))#{Ref => Pid}},
    NewSubscribeRefs = (State#state.subscribe_refs)#{Ref => Subscription},
    {reply, {Ref, SubscribeResult}, State#state{subscriptions = NewSubscriptions, subscribe_refs = NewSubscribeRefs}};
handle_call({unsubscribe, Ref}, _From, State) ->
    {reply, ok, remove_subscribe(Ref, State)}.

-spec handle_cast(Message, state()) -> {noreply, state()} when
    Message ::
        {local_data, pid(), version(), local_data()}
        | dynamic().
handle_cast({local_data, Peer, Version, LocalState}, #state{module = Module} = State) ->
    case Peers = State#state.peers of
        #{Peer := {Ref, _Version, OldLocalState}} ->
            Update = Module:data_diff(OldLocalState, LocalState),
            NewGlobalView = Module:update_global_view_and_notify(
                node(Peer), Update, State#state.subscriptions, State#state.global_view
            ),
            NewPeers = Peers#{Peer => {Ref, Version, LocalState}},
            {noreply, State#state{global_view = NewGlobalView, peers = NewPeers}};
        _ ->
            Ref = erlang:monitor(process, Peer, [{tag, {'DOWN', peer}}]),
            Update = Module:data_diff(Module:init_local_data(), LocalState),
            NewGlobalView = Module:update_global_view_and_notify(
                node(Peer), Update, State#state.subscriptions, State#state.global_view
            ),
            NewPeers = Peers#{Peer => {Ref, Version, LocalState}},
            {noreply, State#state{global_view = NewGlobalView, peers = NewPeers}}
    end;
handle_cast(Message, State) ->
    do_message(Message, State).

-spec handle_info(Message, state()) -> {noreply, state()} when
    Message ::
        {discover, pid(), version()}
        | {update, pid(), update()}
        | {nodeup, node()}
        | {nodedown, node()}
        | {{'DOWN', subscribe}, reference(), process, pid(), dynamic()}
        | {{'DOWN', peer}, reference(), process, pid(), dynamic()}
        | dynamic().
handle_info({discover, Peer, _Version}, #state{options = #{standalone := true}} = State) when is_pid(Peer) ->
    {noreply, State};
handle_info({discover, Peer, Version}, #state{module = Module} = State) ->
    gen_server:cast(Peer, translate_local_data(State#state.local_data, Version, State)),
    case is_map_key(Peer, State#state.peers) of
        true ->
            {noreply, State};
        _ ->
            Ref = erlang:monitor(process, Peer, [{tag, {'DOWN', peer}}]),
            erlang:send(Peer, {discover, self(), State#state.version}, [noconnect]),
            {noreply, State#state{peers = (State#state.peers)#{Peer => {Ref, Version, Module:init_local_data()}}}}
    end;
handle_info({update, Peer, Update}, #state{module = Module} = State) ->
    case State#state.peers of
        #{Peer := {Ref, Version, LocalState}} = Peers ->
            NewGlobalView = Module:update_global_view_and_notify(
                node(Peer), Update, State#state.subscriptions, State#state.global_view
            ),
            NewLocalState = Module:update_local_data(Update, LocalState),
            NewPeers = Peers#{Peer => {Ref, Version, NewLocalState}},
            {noreply, State#state{global_view = NewGlobalView, peers = NewPeers}};
        _ ->
            %% Handle race condition: remote node disconnected, but scope process
            %%  of the remote node was just about to send update message. In this
            %%  case, local node handles 'DOWN' first, but then connection is
            %%  restored, and update message gets delivered when it's not expected.
            %% It also handles the case when node outside of overlay network sends
            %%  unexpected request.
            {noreply, State}
    end;
handle_info({nodeup, Node}, State) when Node =:= node() ->
    {noreply, State};
handle_info({nodeup, Node}, State) ->
    erlang:send({State#state.scope, Node}, {discover, self(), State#state.version}, [noconnect]),
    {noreply, State};
handle_info({nodedown, _Node}, State) ->
    {noreply, State};
handle_info({{'DOWN', subscribe}, Ref, process, _Pid, _Info}, State) ->
    {noreply, remove_subscribe(Ref, State)};
handle_info({{'DOWN', peer}, Ref, process, Peer, _Info}, #state{module = Module} = State) ->
    case maps:take(Peer, State#state.peers) of
        {{Ref, _Version, LocalState}, NewPeers} ->
            Update = Module:data_diff(LocalState, Module:init_local_data()),
            NewGlobalView = Module:update_global_view_and_notify(
                node(Peer), Update, State#state.subscriptions, State#state.global_view
            ),
            {noreply, State#state{global_view = NewGlobalView, peers = NewPeers}};
        _ ->
            % should never happen
            {noreply, State}
    end;
handle_info(Message, State) ->
    do_message(Message, State).

-spec terminate(dynamic(), state()) -> term().
terminate(_Reasion, #state{module = Module} = State) ->
    Module:stop_global_view(State#state.global_view).

-spec do_message(dynamic(), state()) -> {noreply, state()}.
do_message(Message, #state{module = Module} = State) ->
    case
        erlang:function_exported(Module, translate_message, 2) andalso
            Module:translate_message(Message, State#state.global_view)
    of
        {drop, GlobalView} ->
            {noreply, State#state{global_view = GlobalView}};
        {update, Update, GlobalView} ->
            {noreply, do_update(Update, State#state{global_view = GlobalView})};
        % legacy support, to be removed
        {update, Peer, Update, GlobalView} ->
            handle_info({update, Peer, Update}, State#state{global_view = GlobalView});
        % legacy support, to be removed
        {local_data, Peer, Version, LocalState, GlobalView} ->
            handle_cast({local_data, Peer, Version, LocalState}, State#state{global_view = GlobalView});
        % legacy support, to be removed
        {discover, Peer, Version, GlobalView} ->
            handle_info({discover, Peer, Version}, State#state{global_view = GlobalView});
        _ ->
            {noreply, State}
    end.

-spec translate_update(update(), version(), state()) -> {update, pid(), update()} | [dynamic()].
translate_update(Update, Version, #state{module = Module} = State) ->
    case Module:translate_update(State#state.version, Version, Update) of
        % legacy support, to be removed
        {'$plain_messages', Messages} ->
            Messages;
        TranslatedUpdate ->
            {update, self(), TranslatedUpdate}
    end.

-spec translate_local_data(local_data(), version(), state()) -> {local_data, pid(), version(), local_data()} | dynamic().
translate_local_data(LocalState, Version, #state{module = Module} = State) ->
    case Module:translate_local_data(State#state.version, Version, LocalState) of
        % legacy support, to be removed
        {'$plain_message', Message} ->
            Message;
        TranslatedLocalState ->
            {local_data, self(), Version, TranslatedLocalState}
    end.

-spec do_send(pid() | {scope(), node()}, dynamic()) -> term().
do_send(Dest, Msgs) when is_list(Msgs) ->
    % legacy support, to be removed
    [do_send(Dest, Msg) || Msg <- Msgs];
do_send(Dest, Msg) ->
    % do not use 'nosuspend', as it will lead to missing update messages when dist buffer is full
    erlang:send(Dest, Msg, [noconnect]).

-spec do_update(update(), state()) -> state().
do_update(Update, #state{module = Module} = State) ->
    NewGlobalView = Module:update_global_view_and_notify(node(), Update, State#state.subscriptions, State#state.global_view),
    NewLocalState = Module:update_local_data(Update, State#state.local_data),
    _ = [do_send(Peer, translate_update(Update, Version, State)) || Peer := {_, Version, _} <- State#state.peers],
    State#state{global_view = NewGlobalView, local_data = NewLocalState}.

-spec remove_subscribe(reference(), state()) -> state().
remove_subscribe(Ref, State) ->
    case maps:take(Ref, State#state.subscribe_refs) of
        {Subscription, NewSubscribeRefs} ->
            erlang:demonitor(Ref),
            NewSubscriptions =
                case Subscriptions = State#state.subscriptions of
                    #{Subscription := Subscribers} when map_size(Subscribers) =:= 1 ->
                        maps:remove(Subscription, Subscriptions);
                    #{Subscription := Subscribers} ->
                        Subscriptions#{Subscription => maps:remove(Ref, Subscribers)}
                end,
            State#state{subscriptions = NewSubscriptions, subscribe_refs = NewSubscribeRefs};
        _ ->
            State
    end.
