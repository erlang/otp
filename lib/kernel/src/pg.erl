%%
%%
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
%%
%%-------------------------------------------------------------------
%%
%% @author Maxim Fedorov <maximfca@gmail.com>
%% Process Groups with eventually consistent membership.
%%
%% Differences (compared to pg2):
%%  * non-existent and empty group treated the same (empty list of pids),
%%     thus create/1 and delete/1 have no effect (and not implemented).
%%     which_groups() return only non-empty groups
%%  * no cluster lock required, and no dependency on global
%%  * all join/leave operations require local process (it's not possible to join
%%     a process from a different node)
%%  * multi-join: join/leave several processes with a single call
%%
%% Why empty groups are not supported:
%%  Unlike a process, group does not have originating node. So it's possible
%% that during net split one node deletes the group, that still exists for
%% another partition. pg2 will recover the group, as soon as net
%% split converges, which is quite unexpected.
%%
%% Exchange protocol:
%%  * when pg process starts, it broadcasts
%%     'discover' message to all nodes in the cluster
%%  * when pg server receives 'discover', it responds with 'sync' message
%%     containing list of groups with all local processes, and starts to
%%     monitor process that sent 'discover' message (assuming it is a part
%%     of an overlay network)
%%  * every pg process monitors 'nodeup' messages to attempt discovery for
%%     nodes that are (re)joining the cluster
%%
%% Leave/join operations:
%%  * processes joining the group are monitored on the local node
%%  * when process exits (without leaving groups prior to exit), local
%%     instance of pg scoped process detects this and sends 'leave' to
%%     all nodes in an overlay network (no remote monitoring done)
%%  * all leave/join operations are serialised through pg server process
%%
-module(pg).

-export([
    start/1,
    start_link/0,
    start_link/1,
    join/2,
    join/3,
    join/4,
    leave/2,
    leave/3,
    leave/4,
    update/1,
    update/2,
    monitor_scope/0,
    monitor_scope/1,
    monitor_scope/2,
    monitor/1,
    monitor/2,
    monitor/3,
    demonitor/1,
    demonitor/2,
    get_all_members/1,
    get_all_members/2,
    get_all_members/3,
    get_all_local_members/1,
    get_all_local_members/2,
    get_all_local_members/3,
    get_members/1,
    get_members/2,
    get_members/3,
    get_local_members/1,
    get_local_members/2,
    get_local_members/3,
    which_groups/0,
    which_groups/1,
    which_local_groups/0,
    which_local_groups/1,
    which_sharded_groups/0,
    which_sharded_groups/1,
    which_sharded_local_groups/0,
    which_sharded_local_groups/1,
    which_shard_ranges/1,
    which_shard_ranges/2,
    which_local_shard_ranges/1,
    which_local_shard_ranges/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%% Default scope started by kernel app
-define(DEFAULT_SCOPE, ?MODULE).

%% Exported Types
-type scope() :: atom().
-type group() :: any().
-type metadata() :: any().
-type member_metadata() :: {pid(), metadata()}.
-type member() :: pid() | member_metadata().
-type members() :: [member()].
-type shard() :: non_neg_integer().
-type shard_range() :: {shard(), shard()}.
-type shard_ranges() :: [shard_range()].
-type update() :: {group(), singleton | shard_ranges(), members(), members()}.
-type updates() :: [update()].
-type leave_error_singleton() :: not_joined.
-type features() :: #{shards_and_metadata => boolean()}.
-type monitor_scope_return_singleton() :: {reference(), #{group() => [pid()]}}.
-type monitor_group_return_singleton() :: {reference(), [pid()]}.
-type monitor_message_singleton() :: {reference(), join | leave, group(), [pid()]}.
-type monitor_message() :: {reference(), updates()}.

-export_type([
    scope/0,
    group/0,
    metadata/0,
    member_metadata/0,
    member/0,
    members/0,
    shard/0,
    shard_range/0,
    shard_ranges/0,
    update/0,
    updates/0,
    leave_error_singleton/0,
    features/0,
    monitor_scope_return_singleton/0,
    monitor_group_return_singleton/0,
    monitor_message_singleton/0,
    monitor_message/0
]).

%% Internal Types
-type peer() :: pid().
-type key() :: {group(), singleton} | {group(), shard(), shard()}.
-type range_tree() :: gb_trees:tree(key(), members()).

%% Protocol Types
-type discover_message_singleton() :: {discover, peer()}.
-type discover_message() :: {discover, peer(), features()}.
-type sync_message_singleton() :: {sync, peer(), [{group(), [pid()]}]}.
-type sync_message() :: {sync, peer(), range_tree()}.
-type update_message_singleton() :: {join, peer(), group(), pid() | [pid()]} | {leave, peer(), pid() | [pid()], [group()]}.
-type update_message() :: {update, peer(), [update()]}.

%% gen_server state
-record(state, {
    %% ETS table name, and also the registered process name (self())
    scope :: atom(),
    %% ETS tid
    tid :: ets:tid(),
    %% feature options
    features :: features(),
    %% monitored local processes and groups they joined, for efficient
    %% leave when process dies and fast update of metadata
    local = #{} :: #{pid() => {reference(), range_tree()}},
    %% cached data to be synced to peers
    sync_data = gb_trees:empty() :: range_tree(),
    %% remote node: scope process monitor and map of groups to Handlers for fast sync routine
    remote = #{} :: #{peer() => {reference(), features(), range_tree()}},
    %% processes monitoring group membership
    scope_monitors = #{} :: #{reference() => {pid(), features()}},
    %% processes monitoring specific groups (forward and reverse map)
    group_monitors = #{} :: #{reference() => group()},
    monitored_groups = #{} :: #{group() => #{reference() => {pid(), features()}}}
}).

-type state() :: #state{}.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server outside of supervision hierarchy.
-spec start(Scope :: atom()) -> gen_server:start_ret().
start(Scope) when is_atom(Scope) ->
    gen_server:start({local, Scope}, ?MODULE, [Scope], []).

%% @doc
%% Starts the server and links it to calling process.
%% Uses default scope, which is the same as as the module name.
-spec start_link() -> gen_server:start_ret().
start_link() ->
    start_link(?DEFAULT_SCOPE).

%% @doc
%% Starts the server and links it to calling process.
%% Scope name is supplied.
-spec start_link(Scope :: atom()) -> gen_server:start_ret().
start_link(Scope) when is_atom(Scope) ->
    gen_server:start_link({local, Scope}, ?MODULE, [Scope], []).

%%--------------------------------------------------------------------
%% @doc
%% Joins a single or a list of members.
%% Group is created automatically.
%% Processes must be local to this node.
-spec join(Group :: group(), MemberOrMembers :: member() | members()) -> ok.
join(Group, MemberOrMembers) ->
    join(?DEFAULT_SCOPE, Group, MemberOrMembers).

-spec join(
    Scope :: atom() | ShardRanges :: shard_ranges(),
    Group :: group(),
    MemberOrMembers :: member() | members()
) -> ok.
join(Scope, Group, MemberOrMembers) when is_atom(Scope) ->
    ListMembers = if is_list(MemberOrMembers) -> MemberOrMembers; true -> [MemberOrMembers] end,
    Updates = [{Group, singleton, [], ListMembers}],
    validate_updates(Updates),
    gen_server:call(Scope, {update, Updates}, infinity),
    ok;
join(ShardRanges, Group, MemberOrMembers) when is_list(ShardRanges) ->
    join(?DEFAULT_SCOPE, ShardRanges, Group, MemberOrMembers).

-spec join(
    Scope :: atom(),
    ShardRanges :: shard_ranges(),
    Group :: group(),
    MemberOrMembers :: member() | members()
) -> ok.
join(Scope, ShardRanges, Group, MemberOrMembers) ->
    ListMembers = if is_list(MemberOrMembers) -> MemberOrMembers; true -> [MemberOrMembers] end,
    Updates = [{Group, ShardRanges, [], ListMembers}],
    validate_updates(Updates),
    gen_server:call(Scope, {update, Updates}, infinity),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% single or list of members leaving the group.
%% Processes must be local to this node.
-spec leave(Group :: group(), MemberOrMembers :: member() | members()) ->
    ok | leave_error_singleton().
leave(Group, MemberOrMembers) ->
    leave(?DEFAULT_SCOPE, Group, MemberOrMembers).

-spec leave(
    Scope :: atom() | ShardRanges :: shard_ranges(),
    Group :: group(),
    MemberOrMembers :: member() | members()
) -> ok | leave_error_singleton() | updates().
leave(Scope, Group, MemberOrMembers) when is_atom(Scope) ->
    ListMembers = if is_list(MemberOrMembers) -> MemberOrMembers; true -> [MemberOrMembers] end,
    Updates = [{Group, singleton, ListMembers, []}],
    validate_updates(Updates),
    gen_server:call(Scope, {leave_singleton, Updates}, infinity);
leave(ShardRanges, Group, MemberOrMembers) when is_list(ShardRanges) ->
    leave(?DEFAULT_SCOPE, ShardRanges, Group, MemberOrMembers).

-spec leave(
    Scope :: atom(),
    ShardRanges :: shard_ranges(),
    Group :: group(),
    MemberOrMembers :: member() | members()
) -> updates().
leave(Scope, ShardRanges, Group, MemberOrMembers) ->
    ListMembers = if is_list(MemberOrMembers) -> MemberOrMembers; true -> [MemberOrMembers] end,
    Updates = [{Group, ShardRanges, ListMembers, []}],
    validate_updates(Updates),
    gen_server:call(Scope, {update, Updates}, infinity).

-spec update(Updates :: updates()) -> updates().
update(Updates) ->
    update(?DEFAULT_SCOPE, Updates).

-spec update(Scope :: atom(), Updates :: updates()) -> updates().
update(Scope, Updates) ->
    validate_updates(Updates),
    gen_server:call(Scope, {update, Updates}, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Begins monitoring all group changes.
%% If Features is #{shards_and_metadata => true}:
%%     Calling process will receive
%%         {Ref, [{group(), singleton | shard_ranges(), Remove :: members(), Add :: members()}]}
%%     messages.
%%     The function returns the first update.
%% Otherwise:
%%     Calling process will receive
%%         {Ref, join, Group, Pids} message when new Pids join the Group, and
%%         {Ref, leave, Group, Pids} when Pids leave the group.
%%     The function returns a Group to Pids map.
-spec monitor_scope() -> monitor_scope_return_singleton().
monitor_scope() ->
    monitor_scope(?DEFAULT_SCOPE).

-spec monitor_scope(Scope :: atom()) -> monitor_scope_return_singleton().
monitor_scope(Scope) ->
    monitor_scope(Scope, #{}).

-spec monitor_scope(Scope :: atom(), Features :: features()) ->
    monitor_scope_return_singleton() | monitor_message().
monitor_scope(Scope, Features) when is_map(Features) ->
    gen_server:call(Scope, {monitor, Features}, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Begins monitoring group changes.
%% If Features is #{shards_and_metadata => true}:
%%     Calling process will receive
%%         {Ref, [{group(), singleton | shard_ranges(), Remove :: members(), Add :: members()}]}
%%     messages.
%%     The function returns the first update.
%% Otherwise:
%%     Calling process will receive
%%         {Ref, join, Group, Pids} message when new Pids join the Group, and
%%         {Ref, leave, Group, Pids} when Pids leave the group.
%%     The function returns a Group to Pids map.
-spec monitor(Group :: group()) -> monitor_group_return_singleton().
monitor(Group) ->
    ?MODULE:monitor(?DEFAULT_SCOPE, Group).

-spec monitor(Scope :: atom(), Group :: group()) -> monitor_group_return_singleton().
monitor(Scope, Group) ->
    ?MODULE:monitor(Scope, Group, #{}).

-spec monitor(Scope :: atom(), Group :: group(), Features :: features()) ->
    monitor_group_return_singleton() | monitor_message().
monitor(Scope, Group, Features) when is_map(Features) ->
    gen_server:call(Scope, {monitor, Group, Features}, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Stops monitoring Scope for groups changes. Flushes all update messages from the calling process queue.
-spec demonitor(Ref :: reference()) -> ok | false.
demonitor(Ref) ->
    pg:demonitor(?DEFAULT_SCOPE, Ref).

-spec demonitor(Scope :: atom(), Ref :: reference()) -> ok | false.
demonitor(Scope, Ref) ->
    gen_server:call(Scope, {demonitor, Ref}, infinity) =:= ok andalso flush(Ref).

%%--------------------------------------------------------------------
%% @doc
%% Returns all members in a group
-spec get_all_members(Group :: group()) -> members().
get_all_members(Group) ->
    get_all_members(?DEFAULT_SCOPE, Group).

-spec get_all_members(Scope :: atom() | Shard :: shard(), Group :: group()) -> members().
get_all_members(Scope, Group) when is_atom(Scope) ->
    do_lookup(Scope, Group, singleton, 2);
get_all_members(Shard, Group) when is_integer(Shard) ->
    get_all_members(?DEFAULT_SCOPE, Shard, Group).

-spec get_all_members(Scope :: atom(), Shard :: shard(), Group :: group()) -> members().
get_all_members(Scope, Shard, Group) ->
    do_lookup(Scope, Group, Shard, 2).

%%--------------------------------------------------------------------
%% @doc
%% Returns members in a group, running on local node.
-spec get_all_local_members(Group :: group()) -> members().
get_all_local_members(Group) ->
    get_all_local_members(?DEFAULT_SCOPE, Group).

-spec get_all_local_members(Scope :: atom() | Shard :: shard(), Group :: group()) -> members().
get_all_local_members(Scope, Group) when is_atom(Scope) ->
    do_lookup(Scope, Group, singleton, 3);
get_all_local_members(Shard, Group) when is_integer(Shard) ->
    get_all_local_members(?DEFAULT_SCOPE, Shard, Group).

-spec get_all_local_members(Scope :: atom(), Shard :: shard(), Group :: group()) -> members().
get_all_local_members(Scope, Shard, Group) ->
    do_lookup(Scope, Group, Shard, 3).

%%--------------------------------------------------------------------
%% @doc
%% Returns all pid members in a group
-spec get_members(Group :: group()) -> [pid()].
get_members(Group) ->
    [Pid || Pid <- get_all_members(Group), is_pid(Pid)].

-spec get_members(Scope :: atom() | Shard :: shard(), Group :: group()) -> [pid()].
get_members(ScopeOrShard, Group) ->
    [Pid || Pid <- get_all_members(ScopeOrShard, Group), is_pid(Pid)].

-spec get_members(Scope :: atom(), Shard :: shard(), Group :: group()) -> [pid()].
get_members(Scope, Shard, Group) ->
    [Pid || Pid <- get_all_members(Scope, Shard, Group), is_pid(Pid)].

%%--------------------------------------------------------------------
%% @doc
%% Returns pid members in a group, running on local node.
-spec get_local_members(Group :: group()) -> [pid()].
get_local_members(Group) ->
    [Pid || Pid <- get_all_local_members(Group), is_pid(Pid)].

-spec get_local_members(Scope :: atom() | Shard :: shard(), Group :: group()) -> [pid()].
get_local_members(ScopeOrShard, Group) ->
    [Pid || Pid <- get_all_local_members(ScopeOrShard, Group), is_pid(Pid)].

-spec get_local_members(Scope :: atom(), Shard :: shard(), Group :: group()) -> [pid()].
get_local_members(Scope, Shard, Group) ->
    [Pid || Pid <- get_all_local_members(Scope, Shard, Group), is_pid(Pid)].

%%--------------------------------------------------------------------
%% @doc
%% Returns a list of all known singleton groups.
-spec which_groups() -> [Group :: group()].
which_groups() ->
    which_groups(?DEFAULT_SCOPE).

-spec which_groups(Scope :: atom()) -> [Group :: group()].
which_groups(Scope) when is_atom(Scope) ->
    [G || [G] <- ets:match(Scope, {{'$1', singleton}, '_', '_'})].

%%--------------------------------------------------------------------
%% @private
%% Returns a list of singleton groups that have any local member joined.
-spec which_local_groups() -> [Group :: group()].
which_local_groups() ->
    which_local_groups(?DEFAULT_SCOPE).

-spec which_local_groups(Scope :: atom()) -> [Group :: group()].
which_local_groups(Scope) when is_atom(Scope) ->
    ets:select(Scope, [{{{'$1', singleton}, '_', '$2'}, [{'=/=', '$2', []}], ['$1']}]).

%%--------------------------------------------------------------------
%% @doc
%% Returns a list of all known singleton groups.
-spec which_sharded_groups() -> [Group :: group()].
which_sharded_groups() ->
    which_sharded_groups(?DEFAULT_SCOPE).

-spec which_sharded_groups(Scope :: atom()) -> [Group :: group()].
which_sharded_groups(Scope) when is_atom(Scope) ->
    lists:uniq([G || [G] <- ets:match(Scope, {{'$1', '_', '_'}, '_', '_'})]).

%%--------------------------------------------------------------------
%% @private
%% Returns a list of singleton groups that have any local member joined.
-spec which_sharded_local_groups() -> [Group :: group()].
which_sharded_local_groups() ->
    which_sharded_local_groups(?DEFAULT_SCOPE).

-spec which_sharded_local_groups(Scope :: atom()) -> [Group :: group()].
which_sharded_local_groups(Scope) when is_atom(Scope) ->
    lists:uniq(ets:select(Scope, [{{{'$1', '_', '_'}, '_', '$2'}, [{'=/=', '$2', []}], ['$1']}])).

%%--------------------------------------------------------------------
%% @doc
%% Returns a list of all known shard ranges of a sharded group.
-spec which_shard_ranges(Group :: group()) -> ShardRanges :: shard_ranges().
which_shard_ranges(Group) ->
    which_shard_ranges(?DEFAULT_SCOPE, Group).

-spec which_shard_ranges(Scope :: atom(), Group :: group()) -> ShardRanges :: shard_ranges().
which_shard_ranges(Scope, Group) when is_atom(Scope) ->
    [{L, R} || [L, R] <- ets:match(Scope, {{Group, '$1', '$2'}, '_', '_'})].

%%--------------------------------------------------------------------
%% @private
%% Returns a list of shard ranges of a sharded group that have any local member joined.
-spec which_local_shard_ranges(Group :: group) -> ShardRanges :: shard_ranges().
which_local_shard_ranges(Group) ->
    which_local_shard_ranges(?DEFAULT_SCOPE, Group).

-spec which_local_shard_ranges(Scope :: atom(), Group :: group()) -> ShardRanges :: shard_ranges().
which_local_shard_ranges(Scope, Group) when is_atom(Scope) ->
    ets:select(Scope, [{{{Group, '$1', '$2'}, '_', '$3'}, [{'=/=', '$3', []}], [{{'$1', '$2'}}]}]).

%%--------------------------------------------------------------------
%% Internal implementation

%% gen_server implementation

-spec init([Scope :: atom()]) -> {ok, state()}.
init([Scope]) ->
    ok = net_kernel:monitor_nodes(true),
    Scope = ets:new(Scope, [ordered_set, protected, named_table, {read_concurrency, true}]),
    State = #state{
        scope = Scope,
        tid = ets:info(Scope, id),
        features = #{
            shards_and_metadata => application:get_env(kernel, pg_shards_and_metadata, false)
        }
    },
    %% discover all nodes running this scope in the cluster
    [do_send({Scope, Node}, discover_message(State)) || Node <- nodes()],
    {ok, State}.

-spec handle_call(Call, From, State) -> {reply, Reply, State} when
    Call :: {leave_singleton, updates()}
        | {update, Updates :: updates()}
        | {monitor, Features}
        | {monitor, Group :: group()}
        | {demonitor, Ref :: reference(), Features},
    Features :: features(),
    From :: {pid(), Tag :: any()},
    State :: state(),
    Reply :: ok |
        leave_error_singleton() |
        updates() |
        monitor_scope_return_singleton() |
        monitor_group_return_singleton() |
        monitor_message() |
        false.

% This clause is special handling for backword compatibility of not_joined return
handle_call(
    {leave_singleton, [{Group, singleton, Remove, []}]},
    _From,
    #state{scope = Scope, local = Local, sync_data = SyncData} = State
) ->
    NewRemove = Remove -- (Remove -- get_local_members(Scope, Group)),
    case NewRemove =:= [] of
        true ->
            {reply, not_joined, State};
        _ ->
            Updates = [{Group, singleton, NewRemove, []}],
            NewState = State#state{local = local_update(Updates, Local), sync_data = update_tree(Updates, SyncData)},
            update_ets(Updates, NewState#state.tid),
            notify(Updates, NewState),
            broadcast(Updates, NewState),
            {reply, ok, NewState}
    end;
% handle general update
handle_call({update, Updates}, _From, #state{local = Local, sync_data = SyncData} = State) ->
    NewUpdates = update_ets_unsafe(Updates, State#state.tid),
    NewState = State#state{local = local_update(NewUpdates, Local), sync_data = update_tree(Updates, SyncData)},
    notify(NewUpdates, NewState),
    broadcast(NewUpdates, NewState),
    {reply, NewUpdates, NewState};

handle_call({monitor, Features}, {Pid, _Tag}, #state{tid = Tid, scope_monitors = ScopeMon} = State) ->
    Reply =
        case Features of
            #{shards_and_metadata := true} ->
                [
                    {get_group(Key), get_positions(Key), [], Members}
                 || {Key, Members, _} <- ets:tab2list(Tid)];
            _ ->
                % (OTP 26)
                #{
                    Group => [P || P <- Members, is_pid(P)]
                 || [Group, Members] <- ets:match(Tid, {{'$1', singleton}, '$2', '_'})}
        end,
    MRef = erlang:monitor(process, Pid, [{tag, {'DOWN', scope_monitors}}]), %% to discard it upon termination
    {reply, {MRef, Reply}, State#state{scope_monitors = ScopeMon#{MRef => {Pid, Features}}}};

handle_call(
    {monitor, Group, Features},
    {Pid, _Tag},
    #state{scope = Scope, tid = Tid, group_monitors = GM, monitored_groups = MG} = State
) ->
    Reply =
        case Features of
            #{shards_and_metadata := true} ->
                Spec = [{{'$1', '$2', '_'}, [{'=:=', {element, 1, '$1'}, Group}], [{{'$1', '$2'}}]}],
                [
                    {get_group(Key), get_positions(Key), [], Members}
                 || {Key, Members} <- ets:select(Tid, Spec)];
            _ ->
                % (OTP 26)
                get_members(Scope, Group)
        end,
    MRef = erlang:monitor(process, Pid, [{tag, {'DOWN', group_monitors}}]),
    NewMG = maps:update_with(Group, fun (Map) -> Map#{MRef => {Pid, Features}} end, #{MRef => {Pid, Features}}, MG),
    {reply, {MRef, Reply}, State#state{group_monitors = GM#{MRef => Group}, monitored_groups = NewMG}};

handle_call(
    {demonitor, Ref},
    _From,
    #state{scope_monitors = ScopeMon, group_monitors = GM, monitored_groups = MG} = State
) ->
    %% not using maybe_drop_monitor here as it does not demonitor, and can not return 'false'
    case maps:take(Ref, ScopeMon) of
        {_, NewMons} ->
            erlang:demonitor(Ref),
            {reply, ok, State#state{scope_monitors = NewMons}};
        error ->
            %% group monitor
            case maps:take(Ref, GM) of
                {Group, NewGM} ->
                    erlang:demonitor(Ref),
                    {reply, ok, State#state{group_monitors = NewGM,
                        monitored_groups = demonitor_group(Ref, Group, MG)}};
                error ->
                    {reply, false, State}
            end
    end;

handle_call(_Request, _From, _S) ->
    erlang:error(badarg).

-spec handle_cast(
    Sync :: sync_message_singleton() | sync_message(),
    State :: state()) -> {noreply, state()}.

handle_cast({sync, Peer, Groups}, State) when is_list(Groups) ->
    RangeTree = gb_trees:from_orddict(lists:sort([{{Group, singleton}, Pids} || {Group, Pids} <- Groups])),
    handle_cast({sync, Peer, RangeTree}, State);

handle_cast({sync, Peer, RangeTree}, #state{remote = Remote} = State) ->
    SortedTree =
        gb_trees:from_orddict(lists:sort([{Key, lists:sort(Members)} || {Key, Members} <- gb_trees:to_list(RangeTree)])),
    {Updates, NewRemote} =
        case Remote of
            #{Peer := {MRef, Features, OldRangeTree}} ->
                {diff(OldRangeTree, SortedTree), Remote#{Peer => {MRef, Features, SortedTree}}};
            _ ->
                {
                    diff(gb_trees:empty(), SortedTree),
                    Remote#{Peer => {erlang:monitor(process, Peer), #{}, SortedTree}}
                }
        end,
    NewState = State#state{remote = NewRemote},
    update_ets(Updates, NewState#state.tid),
    notify(Updates, NewState),
    {noreply, State#state{remote = NewRemote}};

handle_cast(_, _State) ->
    erlang:error(badarg).

-spec handle_info(
    discover_message_singleton() |
    discover_message() |
    update_message_singleton() |
    update_message() |
    {'DOWN', reference(), process, pid(), term()} |
    {nodedown, node()} | {nodeup, node()},
    State :: state()
) -> {noreply, state()}.

%% (OTP 26) remote pid or several pids joining the group
handle_info({join, Peer, Group, PidOrPids}, State) ->
    Members = if is_pid(PidOrPids) -> [PidOrPids]; true -> PidOrPids end,
    handle_info({update, Peer, [{Group, singleton, [], Members}]}, State);

%% (OTP 26) remote pid leaving (multiple groups at once)
handle_info({leave, Peer, PidOrPids, Groups}, State) ->
    Members = if is_pid(PidOrPids) -> [PidOrPids]; true -> PidOrPids end,
    handle_info({update, Peer, [{Group, singleton, Members, []} || Group <- Groups]}, State);

handle_info({update, Peer, Updates}, #state{remote = Remote} = State) ->
    case maps:get(Peer, Remote, []) of
        {MRef, Features, RangeTree} ->
            update_ets(Updates, State#state.tid),
            notify(Updates, State),
            {noreply, State#state{remote = Remote#{Peer => {MRef, Features, update_tree(Updates, RangeTree)}}}};
        [] ->
            %% Handle race condition: remote node disconnected, but scope process
            %%  of the remote node was just about to send 'leave' message. In this
            %%  case, local node handles 'DOWN' first, but then connection is
            %%  restored, and 'leave' message gets delivered when it's not expected.
            %% It also handles the case when node outside of overlay network sends
            %%  unexpected leave request.
            {noreply, State}
    end;

%% we're being discovered, let's exchange!
handle_info({discover, Peer}, State) ->
    handle_info({discover, Peer, #{}}, State);

%% New discover message sent by a future pg version.
%% Accepted first in OTP 26, to be used by OTP 28 or later.
handle_info({discover, Peer, Features}, #state{remote = Remote, sync_data = SyncData} = State) ->
    Data =
        case Features of
            #{shards_and_metadata := true} ->
                SyncData;
            _ ->
                % (OTP 26)
                [
                    {Group, [Pid || Pid <- Members, is_pid(Pid)]}
                    || {{Group, singleton}, Members} <- gb_trees:to_list(SyncData)]
        end,
    gen_server:cast(Peer, {sync, self(), Data}),
    %% do we know who is looking for us?
    case Remote of
        #{Peer := {MRef, _Features, RangeTree}} ->
            {noreply, State#state{remote = Remote#{Peer => {MRef, Features, RangeTree}}}};
        #{} ->
            MRef = erlang:monitor(process, Peer),
            do_send(Peer, discover_message(State)),
            {noreply, State#state{remote = Remote#{Peer => {MRef, Features, gb_trees:empty()}}}}
    end;

%% handle local process exit
handle_info({'DOWN', MRef, process, Pid, _Info}, #state{local = Local, sync_data = SyncData} = State) when
    node(Pid) =:= node()
->
    case maps:take(Pid, Local) of
        error ->
            %% ignore late monitor: this can only happen when leave request and 'DOWN' are in pg queue
            {noreply, State};
        {{MRef, RangeTree}, NewLocal} ->
            Updates = [
                {get_group(Key), get_positions(Key), Members, []}
             || {Key, Members} <- gb_trees:to_list(RangeTree)],
            NewState = State#state{local = NewLocal, sync_data = update_tree(Updates, SyncData)},
            update_ets(Updates, NewState#state.tid),
            notify(Updates, NewState),
            broadcast(Updates, NewState),
            {noreply, NewState}
    end;

%% handle remote node down or scope leaving overlay network
handle_info({'DOWN', MRef, process, Peer, _Info}, #state{remote = Remote} = State) ->
    case maps:take(Peer, Remote) of
        {{MRef, _Features, RangeTree}, NewRemote} ->
            Updates = [
                {get_group(Key), get_positions(Key), Members, []}
             || {Key, Members} <- gb_trees:to_list(RangeTree)],
            NewState = State#state{remote = NewRemote},
            update_ets(Updates, NewState#state.tid),
            notify(Updates, NewState),
            {noreply, NewState};
        error ->
            {noreply, State}
    end;

%% handle scope monitor exiting
handle_info({{'DOWN', scope_monitors}, MRef, process, _Pid, _Info}, #state{scope_monitors = ScopeMon} = State) ->
    {noreply, State#state{scope_monitors = maps:remove(MRef, ScopeMon)}};

%% handle group monitor exiting
handle_info({{'DOWN', group_monitors}, MRef, process, _Pid, _Info}, #state{
    group_monitors = GMs, monitored_groups = MG} = State) ->
    case maps:take(MRef, GMs) of
        error ->
            {noreply, State};
        {Group, NewGM} ->
            %% clean up the inverse map
            {noreply, State#state{group_monitors = NewGM, monitored_groups = demonitor_group(MRef, Group, MG)}}
    end;

%% nodedown: ignore, and wait for 'DOWN' signal for monitored process
handle_info({nodedown, _Node}, State) ->
    {noreply, State};

%% nodeup: discover if remote node participates in the overlay network
handle_info({nodeup, Node}, State) when Node =:= node() ->
    {noreply, State};
handle_info({nodeup, Node}, #state{scope = Scope} = State) ->
    do_send({Scope, Node}, discover_message(State)),
    {noreply, State};

handle_info(_Info, _State) ->
    erlang:error(badarg).

-spec terminate(Reason :: any(), State :: state()) -> true.
terminate(_Reason, #state{scope = Scope}) ->
    true = ets:delete(Scope).

%%--------------------------------------------------------------------
%% Internal implementation

get_member_pid({Pid, _}) ->
    Pid;
get_member_pid(Pid) ->
    Pid.

is_local(Pid) when is_pid(Pid), node(Pid) =:= node() ->
    true;
is_local({Pid, _}) when is_pid(Pid), node(Pid) =:= node() ->
    true;
is_local(_) ->
    false.

is_range({L, R}) when L >=0, L =< R, is_integer(L), is_integer(R) ->
    true;
is_range(_) ->
    false.

validate_updates([]) ->
    true;
validate_updates([Update | Updates]) ->
    validate_update(Update),
    validate_updates(Updates).

get_group({Group, singleton}) ->
    Group;
get_group({Group, _L, _R}) ->
    Group.

get_positions({_Group, singleton}) ->
    singleton;
get_positions({_Group, L, R}) ->
    [{L, R}].

validate_update({_Group, Positions, Remove, Add}) ->
    maps:intersect(#{K => ok || K <- Remove}, #{K => ok || K <- Add}) =:= #{}
        orelse erlang:error({intersection, Remove, Add}),
    [is_local(Member) orelse erlang:error({nolocal, Member}) || Member <- Remove],
    [is_local(Member) orelse erlang:error({nolocal, Member}) || Member <- Add],
    Positions =:= singleton orelse [is_range(Range) orelse erlang:error({norange, Range}) || Range <- Positions],
    ok.

demonitor_group(Ref, Group, MG) ->
    case maps:get(Group, MG) of
        #{Ref := _} = M when map_size(M) =:= 1 ->
            maps:remove(Group, MG);
        #{Ref := _} = M ->
            MG#{Group => maps:remove(Ref, M)}
    end.

%% remove all messages that were send to monitor groups
flush(Ref) ->
    receive
        {Ref, _Updates} ->
            flush(Ref);
        % (OTP 26)
        {Ref, Verb, _Group, _Pids} when Verb =:= join; Verb =:= leave ->
            flush(Ref)
    after 0 ->
        ok
    end.

do_lookup(Scope, Group, singleton, ElementPosition) ->
    try
        ets:lookup_element(Scope, {Group, singleton}, ElementPosition, [])
    catch
        %% Case where the table does not exist yet.
        error:badarg -> []
    end;
do_lookup(Scope, Group, Shard, ElementPosition) ->
    try
        % TODO: replace by ets:prev_object
        case ets:prev(Scope, {Group, Shard + 1, Shard + 1}) of
            {Group, L, R} when L =< Shard, R >= Shard ->
                try
                    ets:lookup_element(Scope, {Group, L, R}, ElementPosition)
                catch
                    %% Racing when the table is updating, should be rare
                    error:badarg ->
                        receive
                        after 10 -> ok
                        end,
                        do_lookup(Scope, Group, Shard, ElementPosition)
                end;
            _ ->
                []
        end
    catch
        %% Case where the table does not exist yet.
        error:badarg ->
            []
    end.

discover_message(#state{features = #{shards_and_metadata := true} = Features}) ->
    {discover, self(), Features};
% (OTP 26)
discover_message(_) ->
    {discover, self()}.

do_send(Dest, Msg) ->
    %% do not use 'nosuspend', as it will lead to missing
    %%   join/leave messages when dist buffer is full
    erlang:send(Dest, Msg, [noconnect]).

local_update([], Local) ->
    Local;
local_update([{Group, Positions, Remove, Add} | Updates], Local) ->
    AfterAdd =
        lists:foldl(
            fun
                (Member, Acc) ->
                    Pid = get_member_pid(Member),
                    case Acc of
                        #{Pid := {MRef, RangeTree}} ->
                            Acc#{Pid => {MRef, update_tree([{Group, Positions, [], [Member]}], RangeTree)}};
                        _ ->
                            Acc#{
                                Pid => {
                                    erlang:monitor(process, Pid),
                                    update_tree([{Group, Positions, [], [Member]}], gb_trees:empty())
                                }
                            }
                    end
            end,
            Local,
            Add
        ),
    AfterRemove =
        lists:foldl(
            fun
                (Member, Acc) ->
                    Pid = get_member_pid(Member),
                    case Acc of
                        #{Pid := {MRef, RangeTree}} ->
                            NewRangeTree = update_tree([{Group, Positions, [Member], []}], RangeTree),
                            case gb_trees:size(NewRangeTree) of
                                0 ->
                                    erlang:demonitor(MRef),
                                    maps:remove(Pid, Acc);
                                _ ->
                                    Acc#{Pid => {MRef, NewRangeTree}}
                            end;
                        _ ->
                            Acc
                    end
            end,
            AfterAdd,
            Remove
        ),
    local_update(Updates, AfterRemove).

% Calculate range tree difference, instead of removing all old sync data and fill it with new data
diff(Old, New) ->
    diff_impl(gb_trees:to_list(Old), gb_trees:to_list(New), []).

diff_impl([], [], Diff) ->
    Diff;
% New is empty
diff_impl([{Key, Members} | Old], [], Diff) ->
    diff_impl(Old, [], [{get_group(Key), get_positions(Key), Members, []} | Diff]);
% Old is empty
diff_impl([], [{Key, Members} | New], Diff) ->
    diff_impl([], New, [{get_group(Key), get_positions(Key), [], Members} | Diff]);
% old group  is smaller
diff_impl([{OldKey, OldMembers} | Old], [{NewKey, _NewMembers} | _Rest] = New, Diff)
    when element(1, OldKey) < element(1, NewKey)
->
    diff_impl(Old, New, [{get_group(OldKey), get_positions(OldKey), OldMembers, []} | Diff]);
% new group is smaller
diff_impl([{OldKey, _OldMembers} | _Rest] = Old, [{NewKey, NewMembers} | New], Diff)
    when element(1, OldKey) > element(1, NewKey)
->
    diff_impl(Old, New, [{get_group(NewKey), get_positions(NewKey), [], NewMembers} | Diff]);
% keys are the same
diff_impl([{Key, OldMembers} | Old], [{Key, NewMembers} | New], Diff) ->
    Update = {get_group(Key), get_positions(Key), OldMembers -- NewMembers, NewMembers -- OldMembers},
    diff_impl(Old, New, [Update | Diff]);
% old group is singleton
diff_impl([{{Group, singleton}, OldMembers} | Old], [{{Group, _NL, _NR}, _NewMembers} | _Rest] = New, Diff) ->
    diff_impl(Old, New, [{Group, singleton, OldMembers, []} | Diff]);
% new group is singleton
diff_impl([{{Group, _OL, _OR}, _OldMembers} | _Rest] = Old, [{{Group, singleton}, NewMembers} | New], Diff) ->
    diff_impl(Old, New, [{Group, singleton, [], NewMembers} | Diff]);
% following must be range keys
% old range on the left of new range
diff_impl([{{Group, OL, OR}, OldMembers} | Old], [{{Group, NL, _NR}, _NewMembers} | _Rest] = New, Diff)
    when OR < NL
->
    diff_impl(Old, New, [{Group, [{OL, OR}], OldMembers, []} | Diff]);
% old range on the right of new range
diff_impl([{{Group, OL, _OR}, _OldMembers} | _Rest] = Old, [{{Group, NL, NR}, NewMembers} | New], Diff)
    when OL > NR
->
    diff_impl(Old, New, [{Group, [{NL, NR}], [], NewMembers} | Diff]);
% following {OL, OR} must intersect with {NL, NR}
% OL < NL
diff_impl([{{Group, OL, OR}, OldMembers} | Old], [{{Group, NL, _NR}, _NewMembers} | _Rest] = New, Diff)
    when OL < NL
->
    diff_impl(
        [{{Group, NL, OR}, OldMembers} | Old],
        New,
        [{Group, [{OL, NL - 1}], OldMembers, []} | Diff]
    );
% OL > NL
diff_impl([{{Group, OL, _OR}, _OldMembers} | _Rest] = Old, [{{Group, NL, NR}, NewMembers} | New], Diff)
    when OL > NL
->
    diff_impl(
        Old,
        [{{Group, OL, NR}, NewMembers} | New],
        [{Group, [{NL, OL - 1}], [], NewMembers} | Diff]
    );
% range start equals, OR < NR
diff_impl([{{Group, L, OR}, OldMembers} | Old], [{{Group, L, NR}, NewMembers} | New], Diff)
    when OR < NR
->
    diff_impl(
        Old,
        [{{Group, OR + 1, NR}, NewMembers} | New],
        [{Group, [{L, OR}], OldMembers -- NewMembers, NewMembers -- OldMembers} | Diff]
    );
% range start equals, OR > NR
diff_impl([{{Group, L, OR}, OldMembers} | Old], [{{Group, L, NR}, NewMembers} | New], Diff)
    when OR > NR
->
    diff_impl(
        [{{Group, NR + 1, OR}, OldMembers} | Old],
        New,
        [{Group, [{L, NR}], OldMembers -- NewMembers, NewMembers -- OldMembers} | Diff]
    ).

broadcast(Updates, #state{remote = Remote}) ->
    _ = [
        case Features of
            #{shards_and_metadata := true} ->
                do_send(Peer, {update, self(), Updates});
            _ ->
                % (OTP 26)
                [
                    begin
                        ToAdd = [Pid || Pid <- Add, is_pid(Pid)],
                        ToAdd =/= [] andalso
                            do_send(Peer, {join, self(), Group, ToAdd}),
                        ToRemove = [Pid || Pid <- Remove, is_pid(Pid)],
                        ToRemove =/= [] andalso
                            do_send(Peer, {leave, self(), ToRemove, [Group]})
                    end
                 || {Group, singleton, Remove, Add} <- Updates]
        end
     || Peer := {_Ref, Features, _RangeTree} <- Remote],
    ok.

notify(Updates, #state{scope_monitors = ScopeMon, monitored_groups = MG}) ->
    [
        do_send(MonitorPid, {MonitorRef, Updates})
     || MonitorRef := {MonitorPid, #{shard_and_metadata := true}} <- ScopeMon],
    [
        do_send(MonitorPid, {MonitorRef, GroupUpdates})
     || Group := GroupUpdates <- maps:groups_from_list(fun(Update) -> element(1, Update) end, Updates),
        MonitorRef := {MonitorPid, #{shard_and_metadata := true}} <- maps:get(Group, MG, #{})],
    % (OTP 26)
    _ = [
        begin
            ToAdd = [Pid || Pid <- Add, is_pid(Pid)],
            ToAdd =/= [] andalso begin
                [
                    do_send(MonitorPid, {MonitorRef, join, Group, ToAdd})
                 || MonitorRef := {MonitorPid, Features} <- ScopeMon,
                    not maps:get(shard_and_metadata, Features, false)],
                [
                    do_send(MonitorPid, {MonitorRef, join, Group, ToAdd})
                 || MonitorRef := {MonitorPid, Features} <- maps:get(Group, MG, #{}),
                    not maps:get(shard_and_metadata, Features, false)]
            end,
            ToRemove = [Pid || Pid <- Remove, is_pid(Pid)],
            ToRemove =/= [] andalso begin
                [
                    do_send(MonitorPid, {MonitorRef, leave, Group, ToRemove})
                 || MonitorRef := {MonitorPid, Features} <- ScopeMon,
                    not maps:get(shard_and_metadata, Features, false)],
                [
                    do_send(MonitorPid, {MonitorRef, leave, Group, ToRemove})
                 || MonitorRef := {MonitorPid, Features} <- maps:get(Group, MG, #{}),
                    not maps:get(shard_and_metadata, Features, false)]
            end
        end
     || {Group, singleton, Remove, Add} <- Updates],
    ok.

update_value_unordered(Members, [], Add) ->
    Add ++ Members;
update_value_unordered(Members, Remove, Add) ->
    Add ++ (Members -- Remove).

update_value_ordered(Members, [], Add) ->
    lists:merge(Add, Members);
update_value_ordered(Members, Remove, Add) ->
    lists:merge(Add, Members -- Remove).

% range tree update
update_tree([], RangeTree) ->
    RangeTree;
update_tree([{Group, Ranges, Remove, Add} | Updates], RangeTree) ->
    update_tree(Updates, update_tree(Group, Ranges, lists:sort(Remove), lists:sort(Add), RangeTree)).

update_tree(Group, singleton, Remove, Add, RangeTree) ->
    NewValue =
        case gb_trees:lookup({Group, singleton}, RangeTree) of
            {value, Members} ->
                update_value_unordered(Members, Remove, Add);
            none ->
                Add
        end,
    case NewValue of
        [] ->
            gb_trees:delete_any({Group, singleton}, RangeTree);
        _ ->
            gb_trees:enter({Group, singleton}, NewValue, RangeTree)
    end;
update_tree(_Group, [], _Remove, _Add, RangeTree) ->
    RangeTree;
update_tree(Group, [{L, R} | Ranges], Remove, Add, RangeTree) ->
    NewRangeTree = do_update_tree(Group, {L, R}, Remove, Add, RangeTree),
    update_tree(Group, Ranges, Remove, Add, NewRangeTree).

do_update_tree(Group, {L, R}, Remove, Add, RangeTree) ->
    % Find the right most range that overlap with {L, R}
    case gb_trees:smaller({Group, R + 1, R + 1}, RangeTree) of
        {{Group, L, R}, Old} ->
            RangeTree1 =
                case update_value_ordered(Old, Remove, Add) of
                    [] ->
                        gb_trees:delete({Group, L, R}, RangeTree);
                    New ->
                        gb_trees:update({Group, L, R}, New, RangeTree)
                end,
            RangeTree2 = maybe_merge_tree(Group, R, RangeTree1),
            maybe_merge_tree(Group, L - 1, RangeTree2);
        {{Group, LL, R}, Old} when LL > L ->
            RangeTree1 =
                case update_value_ordered(Old, Remove, Add) of
                    [] ->
                        gb_trees:delete({Group, LL, R}, RangeTree);
                    New ->
                        gb_trees:update({Group, LL, R}, New, RangeTree)
                end,
            RangeTree2 = maybe_merge_tree(Group, R, RangeTree1),
            do_update_tree(Group, {L, LL - 1}, Remove, Add, RangeTree2);
        {{Group, LL, R}, Old} when LL < L ->
            RangeTree2 =
                case update_value_ordered(Old, Remove, Add) of
                    [] ->
                        gb_trees:insert({Group, LL, L - 1}, Old, RangeTree);
                    New ->
                        RangeTree1 = gb_trees:insert({Group, LL, L - 1}, Old, RangeTree),
                        gb_trees:insert({Group, L, R}, New, RangeTree1)
                end,
            RangeTree3 = gb_trees:delete({Group, LL, R}, RangeTree2),
            maybe_merge_tree(Group, R, RangeTree3);
        %   Not necessary here but leave for completeness
        %   maybe_merge_tree(Group, L - 1);
        {{Group, LL, RR}, Old} when RR > R ->
            RangeTree1 = gb_trees:insert({Group, LL, R}, Old, RangeTree),
            RangeTree2 = gb_trees:insert({Group, R + 1, RR}, Old, RangeTree1),
            RangeTree3 = gb_trees:delete({Group, LL, RR}, RangeTree2),
            do_update_tree(Group, {L, R}, Remove, Add, RangeTree3);
        {{Group, _LL, RR}, _Members} when RR < R, RR >= L ->
            % Remove must be empty and Add must not be empty
            RangeTree1 = gb_trees:insert({Group, RR + 1, R}, Add, RangeTree),
            RangeTree2 = maybe_merge_tree(Group, R, RangeTree1),
            do_update_tree(Group, {L, RR}, Remove, Add, RangeTree2);
        _ ->
            % Remove must be empty and Add must not be empty
            RangeTree1 = gb_trees:insert({Group, L, R}, Add, RangeTree),
            RangeTree2 = maybe_merge_tree(Group, R, RangeTree1),
            maybe_merge_tree(Group, L - 1, RangeTree2)
    end.

maybe_merge_tree(Group, I, RangeTree) ->
    case gb_trees:smaller({Group, I + 1, I + 1}, RangeTree) of
        {{Group, L, I}, Members} ->
            case gb_trees:larger({Group, I + 1, I}, RangeTree) of
                {{Group, II, R}, Members} when II =:= I + 1 ->
                    RangeTree1 = gb_trees:insert({Group, L, R}, Members, RangeTree),
                    RangeTree2 = gb_trees:delete({Group, L, I}, RangeTree1),
                    gb_trees:delete({Group, I + 1, R}, RangeTree2);
                _ ->
                    RangeTree
            end;
        _ ->
            RangeTree
    end.

% range ETS update
%
% The algorithm promises that an
% update() :: {group(), singleton | shard_ranges(), Remove :: members(), Add :: members()}
% is an 'atomic' operation by a single shard's view (if the shard ranges are disjoint).
% A join is just Remove = [], a leave is just Add = [].
%
% Assume for Group we currently have this shard mapping:
% {1, 5} => [Pid1, {Pid2, Meta2}]
% {6, 10} => [Pid1, Pid3]
%
% An update
% {Group, [{4,5}], [{Pid2, Meta2}], [Pid3]}
% will remove {Pid2, Meta2} from range {4, 5} and add Pid3 to range {4, 5} at the same time.
%
% The final map will be:
% {1, 3} => [Pid1, {Pid2, Meta2}]
% {4, 10} => [Pid1, Pid3]
%
% Internally, the update is split to 6 steps:
% (split and update current ranges)
% 1. ​insert {1, 3} => [Pid1, {Pid2, Meta2}]
% 2. insert {4, 5} => [Pid1, Pid3]
% 3. delete {1, 5} => [Pid1, {Pid2, Meta2}]
% (merge adjacent ranges with same members)
% 4. insert {4,10} => [Pid1, Pid3]
% 5. delete {4, 5} => [Pid1, Pid3]
% 6. delete {6, 10} => [Pid1, Pid3]
%
% The algorithm does this way to make sure that during update, an ets:prev lookup will either
% find a range with old value or find a range with new value.
% There won't be a situation that it returns empty if both old value and new value are non-empty.
% The reason is that the algorithm always inserts split/merged entries first, and then do delete.
update_ets([], _Tid) ->
    ok;
update_ets([{Group, Positions, Remove, Add} | Updates], Tid) ->
    GlobalRemove = lists:sort(Remove),
    GlobalAdd = lists:sort(Add),
    LocalRemove = [Member || Member <- GlobalRemove, is_local(Member)],
    LocalAdd = [Member || Member <- GlobalAdd, is_local(Member)],
    update_ets(Group, Positions, GlobalRemove, GlobalAdd, LocalRemove, LocalAdd, Tid),
    update_ets(Updates, Tid).

update_ets(Group, singleton, GR, GA, LR, LA, Tid) ->
    {NewGlobal, NewLocal} =
        case ets:lookup(Tid, {Group, singleton}) of
            [{_, Global, Local}] ->
                {
                    update_value_unordered(Global, GR, GA),
                    update_value_unordered(Local, LR, LA)
                };
            [] ->
                {GA, LA}
        end,
    case NewGlobal of
        [] ->
            ets:delete(Tid, {Group, singleton});
        _ ->
            ets:insert(Tid, {{Group, singleton}, NewGlobal, NewLocal})
    end;
update_ets(_Group, [], _GR, _GA, _LR, _LA, _Tid) ->
    ok;
update_ets(Group, [{L, R} | Ranges], GR, GA, LR, LA, Tid) ->
    do_update_ets(Group, {L, R}, GR, GA, LR, LA, Tid),
    update_ets(Group, Ranges, GR, GA, LR, LA, Tid).

do_update_ets(Group, {L, R}, GR, GA, LR, LA, Tid) ->
    % Find the right most range that overlap with {L, R}
    case ets:prev(Tid, {Group, R + 1, R + 1}) of
        {Group, L, R} ->
            [{_, OldGlobal, OldLocal}] = ets:lookup(Tid, {Group, L, R}),
            case update_value_ordered(OldGlobal, GR, GA) of
                [] ->
                    ets:delete(Tid, {Group, L, R});
                NewGlobal ->
                    NewLocal = update_value_ordered(OldLocal, LR, LA),
                    ets:insert(Tid, {{Group, L, R}, NewGlobal, NewLocal})
            end,
            maybe_merge_ets(Tid, Group, R),
            maybe_merge_ets(Tid, Group, L - 1);
        {Group, LL, R} when LL > L ->
            [{_, OldGlobal, OldLocal}] = ets:lookup(Tid, {Group, LL, R}),
            case update_value_ordered(OldGlobal, GR, GA) of
                [] ->
                    ets:delete(Tid, {Group, LL, R});
                NewGlobal ->
                    NewLocal = update_value_ordered(OldLocal, LR, LA),
                    ets:insert(Tid, {{Group, LL, R}, NewGlobal, NewLocal})
            end,
            maybe_merge_ets(Tid, Group, R),
            do_update_ets(Group, {L, LL - 1}, GR, GA, LR, LA, Tid);
        {Group, LL, R} when LL < L ->
            [{_, OldGlobal, OldLocal}] = ets:lookup(Tid, {Group, LL, R}),
            case update_value_ordered(OldGlobal, GR, GA) of
                [] ->
                    ets:insert(Tid, {{Group, LL, L - 1}, OldGlobal, OldLocal});
                NewGlobal ->
                    NewLocal = update_value_ordered(OldLocal, LR, LA),
                    ets:insert(Tid, [
                        {{Group, LL, L - 1}, OldGlobal, OldLocal},
                        {{Group, L, R}, NewGlobal, NewLocal}
                    ])
            end,
            ets:delete(Tid, {Group, LL, R}),
            maybe_merge_ets(Tid, Group, R);
        %   Not necessary here but leave for completeness
        %   maybe_merge_ets(Tid, Group, L - 1);
        {Group, LL, RR} when RR > R ->
            [{_, OldGlobal, OldLocal}] = ets:lookup(Tid, {Group, LL, RR}),
            ets:insert(Tid, [
                {{Group, LL, R}, OldGlobal, OldLocal},
                {{Group, R + 1, RR}, OldGlobal, OldLocal}
            ]),
            ets:delete(Tid, {Group, LL, RR}),
            do_update_ets(Group, {L, R}, GR, GA, LR, LA, Tid);
        {Group, _LL, RR} when RR < R, RR >= L ->
            % Remove must be empty and Add must not be empty
            ets:insert(Tid, {{Group, RR + 1, R}, GA, LA}),
            maybe_merge_ets(Tid, Group, R),
            do_update_ets(Group, {L, RR}, GR, GA, LR, LA, Tid);
        _ ->
            % Remove must be empty and Add must not be empty
            ets:insert(Tid, {{Group, L, R}, GA, LA}),
            maybe_merge_ets(Tid, Group, R),
            maybe_merge_ets(Tid, Group, L - 1)
    end.

maybe_merge_ets(Tid, Group, I) ->
    case ets:prev(Tid, {Group, I + 1, I + 1}) of
        {Group, L, I} ->
            case ets:next(Tid, {Group, I + 1, I}) of
                {Group, II, R} when II =:= I + 1 ->
                    % Use sorting for now, we may instead store map later
                    [{_, A, B}] = ets:lookup(Tid, {Group, L, I}),
                    C = ets:lookup_element(Tid, {Group, I + 1, R}, 2),
                    A =:= C andalso
                        begin
                            ets:insert(Tid, {{Group, L, R}, A, B}),
                            ets:delete(Tid, {Group, L, I}),
                            ets:delete(Tid, {Group, I + 1, R})
                        end;
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

% Cleanup non-op local Removes from Updates while updating ETS
update_ets_unsafe(Updates, Tid) ->
    update_ets_unsafe(Updates, Tid, []).

update_ets_unsafe([], _Tid, Acc) ->
    lists:reverse(Acc);
% It is always safe when Remove is empty
update_ets_unsafe([{_, _, [], _} = Update | Updates], Tid, Acc) ->
    update_ets([Update], Tid),
    update_ets_unsafe(Updates, Tid, add_to_update(Update, Acc));
% When Remove is non empty
update_ets_unsafe([{Group, singleton, Remove, Add} | Updates], Tid, Acc) ->
    {NewRemove, NewGlobal, NewLocal} =
        case ets:lookup(Tid, {Group, singleton}) of
            [{_, Global, Local}] ->
                {
                    Remove -- (Remove -- Local),
                    update_value_unordered(Global, Remove, Add),
                    update_value_unordered(Local, Remove, Add)
                };
            [] ->
                {[], Add, Add}
        end,
    case NewGlobal of
        [] ->
            ets:delete(Tid, {Group, singleton});
        _ ->
            ets:insert(Tid, {{Group, singleton}, NewGlobal, NewLocal})
    end,
    update_ets_unsafe(Updates, Tid, add_to_update({Group, singleton, NewRemove, Add}, Acc));
update_ets_unsafe([{Group, Positions, Remove, Add} | Updates], Tid, Acc) ->
    NewAcc = update_ets_unsafe(Group, Positions, lists:sort(Remove), lists:sort(Add), Tid, Acc),
    update_ets_unsafe(Updates, Tid, NewAcc).

update_ets_unsafe(_Group, [], _Remove, _Add, _Tid, Acc) ->
    lists:reverse(Acc);
update_ets_unsafe(Group, [{L, R} | Ranges], Remove, Add, Tid, Acc) ->
    NewAcc = do_update_ets_unsafe(Group, {L, R}, Remove, Add, Tid, Acc),
    update_ets_unsafe(Group, Ranges, Remove, Add, Tid, NewAcc).

do_update_ets_unsafe(Group, {L, R}, Remove, Add, Tid, Acc) ->
    % Find the right most range that overlap with {L, R}
    case ets:prev(Tid, {Group, R + 1, R + 1}) of
        {Group, L, R} ->
            [{_, OldGlobal, OldLocal}] = ets:lookup(Tid, {Group, L, R}),
            NewRemove = Remove -- (Remove -- OldLocal),
            case update_value_ordered(OldGlobal, NewRemove, Add) of
                [] ->
                    ets:delete(Tid, {Group, L, R});
                NewGlobal ->
                    NewLocal = update_value_ordered(OldLocal, NewRemove, Add),
                    ets:insert(Tid, {{Group, L, R}, NewGlobal, NewLocal})
            end,
            maybe_merge_ets(Tid, Group, R),
            maybe_merge_ets(Tid, Group, L - 1),
            add_to_update({Group, [{L, R}], NewRemove, Add}, Acc);
        {Group, LL, R} when LL > L ->
            [{_, OldGlobal, OldLocal}] = ets:lookup(Tid, {Group, LL, R}),
            NewRemove = Remove -- (Remove -- OldLocal),
            case update_value_ordered(OldGlobal, NewRemove, Add) of
                [] ->
                    ets:delete(Tid, {Group, LL, R});
                NewGlobal ->
                    NewLocal = update_value_ordered(OldLocal, NewRemove, Add),
                    ets:insert(Tid, {{Group, LL, R}, NewGlobal, NewLocal})
            end,
            maybe_merge_ets(Tid, Group, R),
            NewAcc = add_to_update({Group, [{LL, R}], NewRemove, Add}, Acc),
            do_update_ets_unsafe(Group, {L, LL - 1}, NewRemove, Add, Tid, NewAcc);
        {Group, LL, R} when LL < L ->
            [{_, OldGlobal, OldLocal}] = ets:lookup(Tid, {Group, LL, R}),
            NewRemove = Remove -- (Remove -- OldLocal),
            case update_value_ordered(OldGlobal, NewRemove, Add) of
                [] ->
                    ets:insert(Tid, {{Group, LL, L - 1}, OldGlobal, OldLocal});
                NewGlobal ->
                    NewLocal = update_value_ordered(OldLocal, NewRemove, Add),
                    ets:insert(Tid, [
                        {{Group, LL, L - 1}, OldGlobal, OldLocal},
                        {{Group, L, R}, NewGlobal, NewLocal}
                    ])
            end,
            ets:delete(Tid, {Group, LL, R}),
            maybe_merge_ets(Tid, Group, R),
            maybe_merge_ets(Tid, Group, L - 1),
            add_to_update({Group, [{L, R}], NewRemove, Add}, Acc);
        {Group, LL, RR} when RR > R ->
            [{_, OldGlobal, OldLocal}] = ets:lookup(Tid, {Group, LL, RR}),
            ets:insert(Tid, [
                {{Group, LL, R}, OldGlobal, OldLocal},
                {{Group, R + 1, RR}, OldGlobal, OldLocal}
            ]),
            ets:delete(Tid, {Group, LL, RR}),
            do_update_ets_unsafe(Group, {L, R}, Remove, Add, Tid, Acc);
        {Group, _LL, RR} when RR < R, RR >= L ->
            Add =/= [] andalso ets:insert(Tid, {{Group, RR + 1, R}, Add, Add}),
            maybe_merge_ets(Tid, Group, R),
            NewAcc = add_to_update({Group, [{RR + 1, R}], [], Add}, Acc),
            do_update_ets_unsafe(Group, {L, RR}, Remove, Add, Tid, NewAcc);
        _ ->
            Add =/= [] andalso ets:insert(Tid, {{Group, L, R}, Add, Add}),
            maybe_merge_ets(Tid, Group, R),
            maybe_merge_ets(Tid, Group, L - 1),
            add_to_update({Group, [{L, R}], [], Add}, Acc)
    end.

add_to_update({_, _, [], []}, Updates) ->
    Updates;
add_to_update({Group, [{L, R}], Remove, Add}, [{Group, Ranges, Remove, Add} | Updates]) when is_list(Ranges) ->
    NewRanges =
        case Ranges of
            [{LL, RR} | Rest] when LL =:= R + 1 ->
                [{L, RR} | Rest];
            _ ->
                [{L, R} | Ranges]
        end,
    [{Group, NewRanges, Remove, Add} | Updates];
add_to_update(Update, Updates) ->
    [Update | Updates].
