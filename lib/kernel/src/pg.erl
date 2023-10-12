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

%% API: default scope
-export([
    start_link/0,

    join/2,
    leave/2,
    monitor_scope/0,
    monitor_scope/1,
    monitor/1,
    monitor/2,
    demonitor/1,
    demonitor/2,
    get_members/1,
    get_local_members/1,
    which_groups/0,
    which_local_groups/0
]).

%% Scoped API: overlay networks
-export([
    start/1,
    start_link/1,

    join/3,
    leave/3,
    get_members/2,
    get_local_members/2,
    which_groups/1,
    which_local_groups/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%% Types
-type group() :: any().

%% Default scope started by kernel app
-define(DEFAULT_SCOPE, ?MODULE).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server and links it to calling process.
%% Uses default scope, which is the same as as the module name.
-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    start_link(?DEFAULT_SCOPE).

%% @doc
%% Starts the server outside of supervision hierarchy.
-spec start(Scope :: atom()) -> {ok, pid()} | {error, any()}.
start(Scope) when is_atom(Scope) ->
    gen_server:start({local, Scope}, ?MODULE, [Scope], []).

%% @doc
%% Starts the server and links it to calling process.
%% Scope name is supplied.
-spec start_link(Scope :: atom()) -> {ok, pid()} | {error, any()}.
start_link(Scope) when is_atom(Scope) ->
    gen_server:start_link({local, Scope}, ?MODULE, [Scope], []).

%%--------------------------------------------------------------------
%% @doc
%% Joins a single or a list of processes.
%% Group is created automatically.
%% Processes must be local to this node.
-spec join(Group :: group(), PidOrPids :: pid() | [pid()]) -> ok.
join(Group, PidOrPids) ->
    join(?DEFAULT_SCOPE, Group, PidOrPids).

-spec join(Scope :: atom(), Group :: group(), PidOrPids :: pid() | [pid()]) -> ok.
join(Scope, Group, PidOrPids) when is_pid(PidOrPids); is_list(PidOrPids) ->
    ok = ensure_local(PidOrPids),
    gen_server:call(Scope, {join_local, Group, PidOrPids}, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Single or list of processes leaving the group.
%% Processes must be local to this node.
-spec leave(Group :: group(), PidOrPids :: pid() | [pid()]) -> ok.
leave(Group, PidOrPids) ->
    leave(?DEFAULT_SCOPE, Group, PidOrPids).

-spec leave(Scope :: atom(), Group :: group(), PidOrPids :: pid() | [pid()]) -> ok | not_joined.
leave(Scope, Group, PidOrPids) when is_pid(PidOrPids); is_list(PidOrPids) ->
    ok = ensure_local(PidOrPids),
    gen_server:call(Scope, {leave_local, Group, PidOrPids}, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Returns currently known groups, and begins monitoring
%%  all group changes. Calling process will receive {Ref, join, Group, Pids}
%%  message when new Pids join the Group, and {Ref, leave, Group, Pids} when
%%  Pids leave the group.
-spec monitor_scope() -> {reference(), #{group() => [pid()]}}.
monitor_scope() ->
    monitor_scope(?DEFAULT_SCOPE).

-spec monitor_scope(Scope :: atom()) -> {reference(), #{group() => [pid()]}}.
monitor_scope(Scope) ->
    gen_server:call(Scope, monitor, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Returns list of processes in the requested group, and begins monitoring
%%  group changes. Calling process will receive {Ref, join, Group, Pids}
%%  message when new Pids join the Group, and {Ref, leave, Group, Pids} when
%%  Pids leave the group.
-spec monitor(Group :: group()) -> {reference(), [pid()]}.
monitor(Group) ->
    ?MODULE:monitor(?DEFAULT_SCOPE, Group).

%%--------------------------------------------------------------------
%% @doc
%% Returns list of processes in the requested group, and begins monitoring
%%  group changes. Calling process will receive {Ref, join, Group, Pids}
%%  message when new Pids join the Group, and {Ref, leave, Group, Pids} when
%%  Pids leave the group.
-spec monitor(Scope :: atom(), Group :: group()) -> {reference(), [pid()]}.
monitor(Scope, Group) ->
    gen_server:call(Scope, {monitor, Group}, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Stops monitoring Scope for groups changes. Flushes all
%%  {Ref, join|leave, Group, Pids} messages from the calling process queue.
-spec demonitor(Ref :: reference()) -> ok | false.
demonitor(Ref) ->
    pg:demonitor(?DEFAULT_SCOPE, Ref).

-spec demonitor(Scope :: atom(), Ref :: reference()) -> ok | false.
demonitor(Scope, Ref) ->
    gen_server:call(Scope, {demonitor, Ref}, infinity) =:= ok andalso flush(Ref).

%%--------------------------------------------------------------------
%% @doc
%% Returns all processes in a group
-spec get_members(Group :: group()) -> [pid()].
get_members(Group) ->
    get_members(?DEFAULT_SCOPE, Group).

-spec get_members(Scope :: atom(), Group :: group()) -> [pid()].
get_members(Scope, Group) ->
    try
        ets:lookup_element(Scope, Group, 2, [])
    catch
        %% Case where the table does not exist yet.
        error:badarg -> []
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns processes in a group, running on local node.
-spec get_local_members(Group :: group()) -> [pid()].
get_local_members(Group) ->
    get_local_members(?DEFAULT_SCOPE, Group).

-spec get_local_members(Scope :: atom(), Group :: group()) -> [pid()].
get_local_members(Scope, Group) ->
    try
        ets:lookup_element(Scope, Group, 3, [])
    catch
        %% Case where the table does not exist yet.
        error:badarg -> []
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns a list of all known groups.
-spec which_groups() -> [Group :: group()].
which_groups() ->
    which_groups(?DEFAULT_SCOPE).

-spec which_groups(Scope :: atom()) -> [Group :: group()].
which_groups(Scope) when is_atom(Scope) ->
    [G || [G] <- ets:match(Scope, {'$1', '_', '_'})].

%%--------------------------------------------------------------------
%% @private
%% Returns a list of groups that have any local processes joined.
-spec which_local_groups() -> [Group :: group()].
which_local_groups() ->
    which_local_groups(?DEFAULT_SCOPE).

-spec which_local_groups(Scope :: atom()) -> [Group :: group()].
which_local_groups(Scope) when is_atom(Scope) ->
    ets:select(Scope, [{{'$1', '_', '$2'}, [{'=/=', '$2', []}], ['$1']}]).

%%--------------------------------------------------------------------
%% Internal implementation

%% gen_server implementation
-record(state, {
    %% ETS table name, and also the registered process name (self())
    scope :: atom(),
    %% monitored local processes and groups they joined
    local = #{} :: #{pid() => {MRef :: reference(), Groups :: [group()]}},
    %% remote node: scope process monitor and map of groups to pids for fast sync routine
    remote = #{} :: #{pid() => {reference(), #{group() => [pid()]}}},
    %% processes monitoring group membership
    scope_monitors = #{} :: #{reference() => pid()},
    %% processes monitoring specific groups (forward and reverse map)
    group_monitors = #{} :: #{reference() => group()},
    monitored_groups = #{} :: #{group() => [{pid(), reference()}]}
}).

-type state() :: #state{}.

-spec init([Scope :: atom()]) -> {ok, state()}.
init([Scope]) ->
    ok = net_kernel:monitor_nodes(true),
    %% discover all nodes running this scope in the cluster
    broadcast([{Scope, Node} || Node <- nodes()], {discover, self()}),
    Scope = ets:new(Scope, [set, protected, named_table, {read_concurrency, true}]),
    {ok, #state{scope = Scope}}.

-spec handle_call(Call :: {join_local, Group :: group(), Pid :: pid()}
                        | {leave_local, Group :: group(), Pid :: pid()}
                        | monitor
                        | {monitor, Group :: group()}
                        | {demonitor, Ref :: reference()},
                  From :: {pid(), Tag :: any()},
                  State :: state()) ->
    {reply, ok | not_joined | {reference(), #{group() => [pid()]}} | false, state()}.

handle_call({join_local, Group, PidOrPids}, _From, #state{scope = Scope, local = Local,
    remote = Remote, scope_monitors = ScopeMon, monitored_groups = MG} = State) ->
    NewLocal = join_local(PidOrPids, Group, Local),
    join_local_update_ets(Scope, ScopeMon, MG, Group, PidOrPids),
    broadcast(maps:keys(Remote), {join, self(), Group, PidOrPids}),
    {reply, ok, State#state{local = NewLocal}};

handle_call({leave_local, Group, PidOrPids}, _From, #state{scope = Scope, local = Local,
    remote = Remote, scope_monitors = ScopeMon, monitored_groups = MG} = State) ->
    case leave_local(PidOrPids, Group, Local) of
        Local ->
            {reply, not_joined, State};
        NewLocal ->
            leave_local_update_ets(Scope, ScopeMon, MG, Group, PidOrPids),
            broadcast(maps:keys(Remote), {leave, self(), PidOrPids, [Group]}),
            {reply, ok, State#state{local = NewLocal}}
    end;

handle_call(monitor, {Pid, _Tag}, #state{scope = Scope, scope_monitors = ScopeMon} = State) ->
    %% next line could also be done with iterating over process state, but it appears to be slower
    Local = #{G => P || [G,P] <- ets:match(Scope, {'$1', '$2', '_'})},
    MRef = erlang:monitor(process, Pid), %% monitor the monitor, to discard it upon termination, and generate MRef
    {reply, {MRef, Local}, State#state{scope_monitors = ScopeMon#{MRef => Pid}}};

handle_call({monitor, Group}, {Pid, _Tag}, #state{scope = Scope, group_monitors = GM, monitored_groups = MG} = State) ->
    %% ETS cache is writable only from this process - so get_members is safe to use
    Members = get_members(Scope, Group),
    MRef = erlang:monitor(process, Pid),
    NewMG = maps:update_with(Group, fun (Ex) -> [{Pid, MRef} | Ex] end, [{Pid, MRef}], MG),
    {reply, {MRef, Members}, State#state{group_monitors = GM#{MRef => {Pid, Group}}, monitored_groups = NewMG}};

handle_call({demonitor, Ref}, _From, #state{scope_monitors = ScopeMon, group_monitors = GM,
    monitored_groups = MG} = State) ->
    %% not using maybe_drop_monitor here as it does not demonitor, and can not return 'false'
    case maps:take(Ref, ScopeMon) of
        {_, NewMons} ->
            erlang:demonitor(Ref),
            {reply, ok, State#state{scope_monitors = NewMons}};
        error ->
            %% group monitor
            case maps:take(Ref, GM) of
                {{Pid, Group}, NewGM} ->
                    erlang:demonitor(Ref),
                    {reply, ok, State#state{group_monitors = NewGM,
                        monitored_groups = demonitor_group({Pid, Ref}, Group, MG)}};
                error ->
                    {reply, false, State}
            end
    end;

handle_call(_Request, _From, _S) ->
    erlang:error(badarg).

-spec handle_cast(
    {sync, Peer :: pid(), Groups :: [{group(), [pid()]}]},
    State :: state()) -> {noreply, state()}.

handle_cast({sync, Peer, Groups}, #state{scope = Scope, remote = Remote, scope_monitors = ScopeMon,
    monitored_groups = MG} = State) ->
    {noreply, State#state{remote = handle_sync(Scope, ScopeMon, MG, Peer, Remote, Groups)}};

handle_cast(_, _State) ->
    erlang:error(badarg).

-spec handle_info(
    {discover, Peer :: pid()} |
    {discover, Peer :: pid(), any()} |
    {join, Peer :: pid(), group(), pid() | [pid()]} |
    {leave, Peer :: pid(), pid() | [pid()], [group()]} |
    {'DOWN', reference(), process, pid(), term()} |
    {nodedown, node()} | {nodeup, node()}, State :: state()) -> {noreply, state()}.

%% remote pid or several pids joining the group
handle_info({join, Peer, Group, PidOrPids}, #state{scope = Scope, remote = Remote, scope_monitors = ScopeMon,
    monitored_groups = MG} = State) ->
    case maps:get(Peer, Remote, []) of
        {MRef, RemoteGroups} ->
            join_remote_update_ets(Scope, ScopeMon, MG, Group, PidOrPids),
            %% store remote group => pids map for fast sync operation
            NewRemoteGroups = join_remote(Group, PidOrPids, RemoteGroups),
            {noreply, State#state{remote = Remote#{Peer => {MRef, NewRemoteGroups}}}};
        [] ->
            %% handle possible race condition, when remote node is flickering up/down,
            %%  and remote join can happen after the node left overlay network
            %% It also handles the case when node outside of overlay network sends
            %%  unexpected join request.
            {noreply, State}
    end;

%% remote pid leaving (multiple groups at once)
handle_info({leave, Peer, PidOrPids, Groups}, #state{scope = Scope, remote = Remote, scope_monitors = ScopeMon,
    monitored_groups = MG} = State) ->
    case maps:get(Peer, Remote, []) of
        {MRef, RemoteMap} ->
            _ = leave_remote_update_ets(Scope, ScopeMon, MG, PidOrPids, Groups),
            NewRemoteMap = leave_remote(PidOrPids, RemoteMap, Groups),
            {noreply, State#state{remote = Remote#{Peer => {MRef, NewRemoteMap}}}};
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
    handle_discover(Peer, State);

%% New discover message sent by a future pg version.
%% Accepted first in OTP 26, to be used by OTP 28 or later.
handle_info({discover, Peer, _ProtocolVersion}, State) ->
    handle_discover(Peer, State);

%% handle local process exit, or a local monitor exit
handle_info({'DOWN', MRef, process, Pid, _Info}, #state{scope = Scope, local = Local,
    remote = Remote, scope_monitors = ScopeMon, monitored_groups = MG} = State) when node(Pid) =:= node() ->
    case maps:take(Pid, Local) of
        error ->
            {noreply, maybe_drop_monitor(MRef, State)};
        {{MRef, Groups}, NewLocal} ->
            [leave_local_update_ets(Scope, ScopeMon, MG, Group, Pid) || Group <- Groups],
            %% send update to all remote peers
            broadcast(maps:keys(Remote), {leave, self(), Pid, Groups}),
            {noreply, State#state{local = NewLocal}}
    end;

%% handle remote node down or scope leaving overlay network, or a monitor from the remote node went down
handle_info({'DOWN', MRef, process, Pid, _Info}, #state{scope = Scope, remote = Remote,
    scope_monitors = ScopeMon, monitored_groups = MG} = State)  ->
    case maps:take(Pid, Remote) of
        {{MRef, RemoteMap}, NewRemote} ->
            maps:foreach(fun (Group, Pids) ->
                leave_remote_update_ets(Scope, ScopeMon, MG, Pids, [Group]) end, RemoteMap),
            {noreply, State#state{remote = NewRemote}};
        error ->
            {noreply, maybe_drop_monitor(MRef, State)}
    end;

%% nodedown: ignore, and wait for 'DOWN' signal for monitored process
handle_info({nodedown, _Node}, State) ->
    {noreply, State};

%% nodeup: discover if remote node participates in the overlay network
handle_info({nodeup, Node}, State) when Node =:= node() ->
    {noreply, State};
handle_info({nodeup, Node}, #state{scope = Scope} = State) ->
    erlang:send({Scope, Node}, {discover, self()}, [noconnect]),
    {noreply, State};

handle_info(_Info, _State) ->
    erlang:error(badarg).

-spec terminate(Reason :: any(), State :: state()) -> true.
terminate(_Reason, #state{scope = Scope}) ->
    true = ets:delete(Scope).

%%--------------------------------------------------------------------
%% Internal implementation

handle_discover(Peer, #state{remote = Remote, local = Local} = State) ->
    gen_server:cast(Peer, {sync, self(), all_local_pids(Local)}),
    %% do we know who is looking for us?
    case maps:is_key(Peer, Remote) of
        true ->
            {noreply, State};
        false ->
            MRef = erlang:monitor(process, Peer),
            erlang:send(Peer, {discover, self()}, [noconnect]),
            {noreply, State#state{remote = Remote#{Peer => {MRef, #{}}}}}
    end;
handle_discover(_, _) ->
    erlang:error(badarg).


%% Ensures argument is either a node-local pid or a list of such, or it throws an error
ensure_local(Pid) when is_pid(Pid), node(Pid) =:= node() ->
    ok;
ensure_local(Pids) when is_list(Pids) ->
    lists:foreach(
        fun
            (Pid) when is_pid(Pid), node(Pid) =:= node() ->
                ok;
            (Bad) ->
                erlang:error({nolocal, Bad})
        end, Pids);
ensure_local(Bad) ->
    erlang:error({nolocal, Bad}).

%% Override all knowledge of the remote node with information it sends
%%  to local node. Current implementation must do the full table scan
%%  to remove stale pids (just as for 'nodedown').
handle_sync(Scope, ScopeMon, MG, Peer, Remote, Groups) ->
    %% can't use maps:get() because it evaluates 'default' value first,
    %%   and in this case monitor() call has side effect.
    {MRef, RemoteGroups} =
        case maps:find(Peer, Remote) of
            error ->
                {erlang:monitor(process, Peer), #{}};
            {ok, MRef0} ->
                MRef0
        end,
    %% sync RemoteMap and transform ETS table
    _ = sync_groups(Scope, ScopeMon, MG, RemoteGroups, Groups),
    Remote#{Peer => {MRef, maps:from_list(Groups)}}.

sync_groups(Scope, ScopeMon, MG, RemoteGroups, []) ->
    %% leave all missing groups
    [leave_remote_update_ets(Scope, ScopeMon, MG, Pids, [Group]) || {Group, Pids} <- maps:to_list(RemoteGroups)];
sync_groups(Scope, ScopeMon, MG, RemoteGroups, [{Group, Pids} | Tail]) ->
    case maps:take(Group, RemoteGroups) of
        {Pids, NewRemoteGroups} ->
            sync_groups(Scope, ScopeMon, MG, NewRemoteGroups, Tail);
        {OldPids, NewRemoteGroups} ->
            [{_Group, AllOldPids, LocalPids}] = ets:lookup(Scope, Group),
            %% should be really rare...
            AllNewPids = Pids ++ AllOldPids -- OldPids,
            true = ets:insert(Scope, {Group, AllNewPids, LocalPids}),
            sync_groups(Scope, ScopeMon, MG, NewRemoteGroups, Tail);
        error ->
            join_remote_update_ets(Scope, ScopeMon, MG, Group, Pids),
            sync_groups(Scope, ScopeMon, MG, RemoteGroups, Tail)
    end.

join_local(Pid, Group, Local) when is_pid(Pid) ->
    case maps:find(Pid, Local) of
        {ok, {MRef, Groups}} ->
            maps:put(Pid, {MRef, [Group | Groups]}, Local);
        error ->
            MRef = erlang:monitor(process, Pid),
            Local#{Pid => {MRef, [Group]}}
    end;
join_local([], _Group, Local) ->
    Local;
join_local([Pid | Tail], Group, Local) ->
    join_local(Tail, Group, join_local(Pid, Group, Local)).

join_local_update_ets(Scope, ScopeMon, MG, Group, Pid) when is_pid(Pid) ->
    case ets:lookup(Scope, Group) of
        [{_Group, All, Local}] ->
            ets:insert(Scope, {Group, [Pid | All], [Pid | Local]});
        [] ->
            ets:insert(Scope, {Group, [Pid], [Pid]})
    end,
    notify_group(ScopeMon, MG, join, Group, [Pid]);
join_local_update_ets(Scope, ScopeMon, MG, Group, Pids) ->
    case ets:lookup(Scope, Group) of
        [{_Group, All, Local}] ->
            ets:insert(Scope, {Group, Pids ++ All, Pids ++ Local});
        [] ->
            ets:insert(Scope, {Group, Pids, Pids})
    end,
    notify_group(ScopeMon, MG, join, Group, Pids).

join_remote_update_ets(Scope, ScopeMon, MG, Group, Pid) when is_pid(Pid) ->
    case ets:lookup(Scope, Group) of
        [{_Group, All, Local}] ->
            ets:insert(Scope, {Group, [Pid | All], Local});
        [] ->
            ets:insert(Scope, {Group, [Pid], []})
    end,
    notify_group(ScopeMon, MG, join, Group, [Pid]);
join_remote_update_ets(Scope, ScopeMon, MG, Group, Pids) ->
    case ets:lookup(Scope, Group) of
        [{_Group, All, Local}] ->
            ets:insert(Scope, {Group, Pids ++ All, Local});
        [] ->
            ets:insert(Scope, {Group, Pids, []})
    end,
    notify_group(ScopeMon, MG, join, Group, Pids).

join_remote(Group, Pid, RemoteGroups) when is_pid(Pid) ->
    maps:update_with(Group, fun (List) -> [Pid | List] end, [Pid], RemoteGroups);
join_remote(Group, Pids, RemoteGroups) ->
    maps:update_with(Group, fun (List) -> Pids ++ List end, Pids, RemoteGroups).

leave_local(Pid, Group, Local) when is_pid(Pid) ->
    case maps:find(Pid, Local) of
        {ok, {MRef, [Group]}} ->
            erlang:demonitor(MRef),
            maps:remove(Pid, Local);
        {ok, {MRef, Groups}} ->
            case lists:member(Group, Groups) of
                true ->
                    maps:put(Pid, {MRef, lists:delete(Group, Groups)}, Local);
                false ->
                    Local
            end;
        _ ->
            Local
    end;
leave_local([], _Group, Local) ->
    Local;
leave_local([Pid | Tail], Group, Local) ->
    leave_local(Tail, Group, leave_local(Pid, Group, Local)).

leave_local_update_ets(Scope, ScopeMon, MG, Group, Pid) when is_pid(Pid) ->
    case ets:lookup(Scope, Group) of
        [{_Group, [Pid], [Pid]}] ->
            ets:delete(Scope, Group),
            notify_group(ScopeMon, MG, leave, Group, [Pid]);
        [{_Group, All, Local}] ->
            ets:insert(Scope, {Group, lists:delete(Pid, All), lists:delete(Pid, Local)}),
            notify_group(ScopeMon, MG, leave, Group, [Pid]);
        [] ->
            %% rare race condition when 'DOWN' from monitor stays in msg queue while process is leave-ing.
            true
    end;
leave_local_update_ets(Scope, ScopeMon, MG, Group, Pids) ->
    case ets:lookup(Scope, Group) of
        [{_Group, All, Local}] ->
            case All -- Pids of
                [] ->
                    ets:delete(Scope, Group);
                NewAll ->
                    ets:insert(Scope, {Group, NewAll, Local -- Pids})
            end,
            notify_group(ScopeMon, MG, leave, Group, Pids);
        [] ->
            true
    end.

leave_remote_update_ets(Scope, ScopeMon, MG, Pid, Groups) when is_pid(Pid) ->
    _ = [
        case ets:lookup(Scope, Group) of
            [{_Group, [Pid], []}] ->
                ets:delete(Scope, Group),
                notify_group(ScopeMon, MG, leave, Group, [Pid]);
            [{_Group, All, Local}] ->
                ets:insert(Scope, {Group, lists:delete(Pid, All), Local}),
                notify_group(ScopeMon, MG, leave, Group, [Pid]);
            [] ->
                true
        end ||
        Group <- Groups];
leave_remote_update_ets(Scope, ScopeMon, MG, Pids, Groups) ->
    _ = [
        case ets:lookup(Scope, Group) of
            [{_Group, All, Local}] ->
                case All -- Pids of
                    [] when Local =:= [] ->
                        ets:delete(Scope, Group);
                    NewAll ->
                        ets:insert(Scope, {Group, NewAll, Local})
                end,
                notify_group(ScopeMon, MG, leave, Group, Pids);
            [] ->
                true
        end ||
        Group <- Groups].

leave_remote(Pid, RemoteMap, Groups) when is_pid(Pid) ->
    leave_remote([Pid], RemoteMap, Groups);
leave_remote(Pids, RemoteMap, Groups) ->
    lists:foldl(
        fun (Group, Acc) ->
            case maps:get(Group, Acc) -- Pids of
                [] ->
                    maps:remove(Group, Acc);
                Remaining ->
                    Acc#{Group => Remaining}
            end
        end, RemoteMap, Groups).

all_local_pids(Local) ->
    maps:to_list(maps:fold(
        fun(Pid, {_Ref, Groups}, Acc) ->
            lists:foldl(
                fun(Group, Acc1) ->
                    Acc1#{Group => [Pid | maps:get(Group, Acc1, [])]}
                end, Acc, Groups)
        end, #{}, Local)).

%% Works as gen_server:abcast(), but accepts a list of processes
%%   instead of nodes list.
broadcast([], _Msg) ->
    ok;
broadcast([Dest | Tail], Msg) ->
    %% do not use 'nosuspend', as it will lead to missing
    %%   join/leave messages when dist buffer is full
    erlang:send(Dest, Msg, [noconnect]),
    broadcast(Tail, Msg).

%% drops a monitor if DOWN was received
maybe_drop_monitor(MRef, #state{scope_monitors = ScopeMon, group_monitors = GMs, monitored_groups = MG} = State) ->
    %% could be a local monitor going DOWN. Since it's a rare event, check should
    %%  not stay in front of any other, more frequent events
    case maps:take(MRef, ScopeMon) of
        error ->
            case maps:take(MRef, GMs) of
                error ->
                    State;
                {{Pid, Group}, NewGM} ->
                    %% clean up the inverse map
                    State#state{group_monitors = NewGM, monitored_groups = demonitor_group({Pid, MRef}, Group, MG)}
            end;
        {_Pid, NewScopeMon} ->
            State#state{scope_monitors = NewScopeMon}
    end.

demonitor_group(Tag, Group, MG) ->
    case maps:find(Group, MG) of
        {ok, [Tag]} ->
            maps:remove(Group, MG);
        {ok, Tags} ->
            maps:put(Group, Tags -- [Tag], MG)
    end.

%% notify all monitors about an Action in Groups for Pids
notify_group(ScopeMonitors, MG, Action, Group, Pids) ->
    maps:foreach(
        fun (Ref, Pid) ->
            erlang:send(Pid, {Ref, Action, Group, Pids}, [noconnect])
        end, ScopeMonitors),
    case maps:find(Group, MG) of
        error ->
            ok;
        {ok, Monitors} ->
            [erlang:send(Pid, {Ref, Action, Group, Pids}, [noconnect]) || {Pid, Ref} <- Monitors],
            ok
    end.

%% remove all messages that were send to monitor groups
flush(Ref) ->
    receive
        {Ref, Verb, _Group, _Pids} when Verb =:= join; Verb =:= leave ->
            flush(Ref)
    after 0 ->
        ok
    end.
