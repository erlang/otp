-module(data_publisher).
-moduledoc """
A eventual consistent database behavior for publishing local data.
A generalization of pg module.
""".

-export([
    start/2,
    start_link/2,
    start_standalone/2,
    start_link_standalone/2,
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
-type version() :: dynamic().
-type data() :: dynamic().
-type update() :: dynamic().
-type storage() :: dynamic().
-type subscribe_result() :: dynamic().
-type subscription() :: dynamic().
-type subscriptions() :: #{subscription() => #{reference() => pid()}}.

-record(state, {
    scope :: atom(),
    module :: module(),
    standalone :: boolean(),
    version :: version(),
    storage :: storage(),
    data :: data(),
    peers = #{} :: #{pid() => {reference(), version(), data()}},
    subscriptions = #{} :: subscriptions(),
    subscribe_refs = #{} :: #{reference() => subscription()}
}).
-type state() :: #state{}.

% Current version of data_publisher instance
-callback version() -> version().
% Init data cache for all nodes
-callback init_storage(scope()) -> storage().
% Init data of one node
-callback init_data() -> data().
% Delete data cache storage
-callback stop_storage(storage()) -> term().
% Update data of one node
% Need to be backward compatible for all possible update() in older versions
-callback update(update(), data()) -> data().
% Update data cache of all nodes, with notifying subscribers
% Need to be backward compatible for all possible update() in older versions
-callback update_storage_and_notify(node(), update(), subscriptions(), storage()) -> storage().
% Calculate the update of data
% Need to be backward compatible for all possible  New :: data() in older versions
-callback data_diff(Old :: data(), New :: data()) -> update().
% Return initial subscription result from data cache for all nodes
-callback new_subscription(subscription(), storage()) -> subscribe_result().
% Translate an update to an older version
-callback translate_update(MyVersion :: version(), PeerVersion :: version(), update()) ->
    update()
    % legacy support, to be removed
    | {'$plain_messages', [dynamic()]}.
% Translate data to an older version
-callback translate_data(MyVersion :: version(), PeerVersion :: version(), data()) ->
    data()
    % legacy support, to be removed
    | {'$plain_message', dynamic()}.
% Translate cast / info message to a potential update, for example a monitor is DOWN
-callback translate_message(dynamic(), storage()) ->
    {update, update(), storage()}
    | {drop, storage()}
    % legacy support, to be removed
    | {update, pid(), update(), storage()}
    % legacy support, to be removed
    | {data, pid(), version(), data(), storage()}
    % legacy support, to be removed
    | {discover, pid(), version(), storage()}.

-optional_callbacks([
    translate_message/2
]).

-spec start(scope(), module()) -> gen_server:start_ret().
start(Scope, Module) ->
    gen_server:start({local, Scope}, ?MODULE, {Scope, Module, false}, []).

-spec start_link(scope(), module()) -> gen_server:start_ret().
start_link(Scope, Module) ->
    gen_server:start_link({local, Scope}, ?MODULE, {Scope, Module, false}, []).

-spec start_standalone(scope(), module()) -> gen_server:start_ret().
start_standalone(Scope, Module) ->
    gen_server:start({local, Scope}, ?MODULE, {Scope, Module, true}, []).

-spec start_link_standalone(scope(), module()) -> gen_server:start_ret().
start_link_standalone(Scope, Module) ->
    gen_server:start_link({local, Scope}, ?MODULE, {Scope, Module, true}, []).

-spec update(scope(), update()) -> ok.
update(Scope, Update) ->
    gen_server:call(Scope, {update, Update}, infinity).

-spec subscribe(scope(), subscription()) -> {reference(), subscribe_result()}.
subscribe(Scope, Subscription) ->
    gen_server:call(Scope, {subscribe, Subscription}, infinity).

-spec unsubscribe(scope(), reference()) -> ok.
unsubscribe(Scope, Ref) ->
    gen_server:call(Scope, {unsubscribe, Ref}, infinity).

-spec init({scope(), module(), boolean()}) -> {ok, state()}.
init({Scope, Module, Standalone}) ->
    Standalone andalso (ok = net_kernel:monitor_nodes(true)),
    State = #state{
        scope = Scope,
        module = Module,
        standalone = Standalone,
        version = Module:version(),
        storage = Module:init_storage(Scope),
        data = Module:init_data()
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
    SubscribeResult = Module:new_subscription(Subscription, State#state.storage),
    NewSubscriptions = Subscriptions#{Subscription => (maps:get(Subscription, Subscriptions, #{}))#{Ref => Pid}},
    NewSubscribeRefs = (State#state.subscribe_refs)#{Ref => Subscription},
    {reply, {Ref, SubscribeResult}, State#state{subscriptions = NewSubscriptions, subscribe_refs = NewSubscribeRefs}};
handle_call({unsubscribe, Ref}, _From, State) ->
    {reply, ok, remove_subscribe(Ref, State)}.

-spec handle_cast(Message, state()) -> {noreply, state()} when
    Message ::
        {data, pid(), version(), data()}
        | dynamic().
handle_cast({data, Peer, Version, Data}, #state{module = Module} = State) ->
    case Peers = State#state.peers of
        #{Peer := {Ref, _Version, OldData}} ->
            Update = Module:data_diff(OldData, Data),
            NewStorage = Module:update_storage_and_notify(
                node(Peer), Update, State#state.subscriptions, State#state.storage
            ),
            NewPeers = Peers#{Peer => {Ref, Version, Data}},
            {noreply, State#state{storage = NewStorage, peers = NewPeers}};
        _ ->
            Ref = erlang:monitor(process, Peer, [{tag, {'DOWN', peer}}]),
            Update = Module:data_diff(Module:init_data(), Data),
            NewStorage = Module:update_storage_and_notify(
                node(Peer), Update, State#state.subscriptions, State#state.storage
            ),
            NewPeers = Peers#{Peer => {Ref, Version, Data}},
            {noreply, State#state{storage = NewStorage, peers = NewPeers}}
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
handle_info({discover, Peer, Version}, #state{standalone = true} = State) when is_pid(Peer) ->
    {noreply, State};
handle_info({discover, Peer, Version}, #state{module = Module} = State) ->
    gen_server:cast(Peer, translate_data(State#state.data, Version, State)),
    case is_map_key(Peer, State#state.peers) of
        true ->
            {noreply, State};
        _ ->
            Ref = erlang:monitor(process, Peer, [{tag, {'DOWN', peer}}]),
            erlang:send(Peer, {discover, self(), State#state.version}, [noconnect]),
            {noreply, State#state{peers = (State#state.peers)#{Peer => {Ref, Version, Module:init_data()}}}}
    end;
handle_info({update, Peer, Update}, #state{module = Module} = State) ->
    case State#state.peers of
        #{Peer := {Ref, Version, Data}} = Peers ->
            NewStorage = Module:update_storage_and_notify(
                node(Peer), Update, State#state.subscriptions, State#state.storage
            ),
            NewData = Module:update(Update, Data),
            NewPeers = Peers#{Peer => {Ref, Version, NewData}},
            {noreply, State#state{storage = NewStorage, peers = NewPeers}};
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
        {{Ref, _Version, Data}, NewPeers} ->
            Update = Module:data_diff(Data, Module:init_data()),
            NewStorage = Module:update_storage_and_notify(
                node(Peer), Update, State#state.subscriptions, State#state.storage
            ),
            {noreply, State#state{storage = NewStorage, peers = NewPeers}};
        _ ->
            % should never happen
            {noreply, State}
    end;
handle_info(Message, State) ->
    do_message(Message, State).

-spec terminate(dynamic(), state()) -> term().
terminate(_Reasion, #state{module = Module} = State) ->
    Module:stop_storage(State#state.storage).

-spec do_message(dynamic(), state()) -> {noreply, state()}.
do_message(Message, #state{module = Module} = State) ->
    case
        erlang:function_exported(Module, translate_message, 2) andalso
            Module:translate_message(Message, State#state.storage)
    of
        {drop, Storage} ->
            {noreply, State#state{storage = Storage}};
        {update, Update, Storage} ->
            {noreply, do_update(Update, State#state{storage = Storage})};
        % legacy support, to be removed
        {update, Peer, Update, Storage} ->
            handle_info({update, Peer, Update}, State#state{storage = Storage});
        % legacy support, to be removed
        {data, Peer, Version, Data, Storage} ->
            handle_cast({data, Peer, Version, Data}, State#state{storage = Storage});
        % legacy support, to be removed
        {discover, Peer, Version, Storage} ->
            handle_info({discover, Peer, Version}, State#state{storage = Storage});
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

-spec translate_data(data(), version(), state()) -> {data, pid(), version(), data()} | dynamic().
translate_data(Data, Version, #state{module = Module} = State) ->
    case Module:translate_data(State#state.version, Version, Data) of
        % legacy support, to be removed
        {'$plain_message', Message} ->
            Message;
        TranslatedData ->
            {data, self(), Version, TranslatedData}
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
    NewStorage = Module:update_storage_and_notify(node(), Update, State#state.subscriptions, State#state.storage),
    NewData = Module:update(Update, State#state.data),
    _ = [do_send(Peer, translate_update(Update, Version, State)) || Peer := {_, Version, _} <- State#state.peers],
    State#state{storage = NewStorage, data = NewData}.

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
