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
%% Joins a single process
%% Group is created automatically.
%% Process must be local to this node.
-spec join(Group :: group(), PidOrPids :: pid() | [pid()]) -> ok.
join(Group, PidOrPids) ->
    join(?DEFAULT_SCOPE, Group, PidOrPids).

-spec join(Scope :: atom(), Group :: group(), PidOrPids :: pid() | [pid()]) -> ok.
join(Scope, Group, PidOrPids) ->
    Node = node(),
    is_list(PidOrPids) andalso [error({nolocal, Pid}) || Pid <- PidOrPids, node(Pid) =/= Node orelse not is_pid(Pid)],
    gen_server:call(Scope, {join_local, Group, PidOrPids}, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Single process leaving the group.
%% Process must be local to this node.
-spec leave(Group :: group(), PidOrPids :: pid() | [pid()]) -> ok.
leave(Group, PidOrPids) ->
    leave(?DEFAULT_SCOPE, Group, PidOrPids).

-spec leave(Scope :: atom(), Group :: group(), Pid :: pid() | [pid()]) -> ok | not_joined.
leave(Scope, Group, PidOrPids) ->
    Node = node(),
    is_list(PidOrPids) andalso [error({nolocal, Pid}) || Pid <- PidOrPids, node(Pid) =/= Node orelse not is_pid(Pid)],
    gen_server:call(Scope, {leave_local, Group, PidOrPids}, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Returns all processes in a group
-spec get_members(Group :: group()) -> [pid()].
get_members(Group) ->
    get_members(?DEFAULT_SCOPE, Group).

-spec get_members(Scope :: atom(), Group :: group()) -> [pid()].
get_members(Scope, Group) ->
    try
        ets:lookup_element(Scope, Group, 2)
    catch
        error:badarg ->
            []
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
        ets:lookup_element(Scope, Group, 3)
    catch
        error:badarg ->
            []
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
    monitors = #{} :: #{pid() => {MRef :: reference(), Groups :: [group()]}},
    %% remote node: scope process monitor and map of groups to pids for fast sync routine
    nodes = #{} :: #{pid() => {reference(), #{group() => [pid()]}}}
}).

-type state() :: #state{}.

-spec init([Scope :: atom()]) -> {ok, state()}.
init([Scope]) ->
    ok = net_kernel:monitor_nodes(true),
    %% discover all nodes in the cluster
    broadcast([{Scope, Node} || Node <- nodes()], {discover, self()}),
    Scope = ets:new(Scope, [set, protected, named_table, {read_concurrency, true}]),
    {ok, #state{scope = Scope}}.

-spec handle_call(Call :: {join_local, Group :: group(), Pid :: pid()}
                        | {leave_local, Group :: group(), Pid :: pid()},
                  From :: {pid(),Tag :: any()},
                  State :: state()) -> {reply, ok | not_joined, state()}.

handle_call({join_local, Group, PidOrPids}, _From, #state{scope = Scope, monitors = Monitors, nodes = Nodes} = State) ->
    NewMons = join_monitors(PidOrPids, Group, Monitors),
    join_local_group(Scope, Group, PidOrPids),
    broadcast(maps:keys(Nodes), {join, self(), Group, PidOrPids}),
    {reply, ok, State#state{monitors = NewMons}};

handle_call({leave_local, Group, PidOrPids}, _From, #state{scope = Scope, monitors = Monitors, nodes = Nodes} = State) ->
    case leave_monitors(PidOrPids, Group, Monitors) of
        Monitors ->
            {reply, not_joined, State};
        NewMons ->
            leave_local_group(Scope, Group, PidOrPids),
            broadcast(maps:keys(Nodes), {leave, self(), PidOrPids, [Group]}),
            {reply, ok, State#state{monitors = NewMons}}
    end;

handle_call(_Request, _From, _S) ->
    error(badarg).

-spec handle_cast(
    {sync, Peer :: pid(), Groups :: [{group(), [pid()]}]},
    State :: state()) -> {noreply, state()}.

handle_cast({sync, Peer, Groups}, #state{scope = Scope, nodes = Nodes} = State) ->
    {noreply, State#state{nodes = handle_sync(Scope, Peer, Nodes, Groups)}};

handle_cast(_, _State) ->
    error(badarg).

-spec handle_info(
    {discover, Peer :: pid()} |
    {join, Peer :: pid(), group(), pid() | [pid()]} |
    {leave, Peer :: pid(), pid() | [pid()], [group()]} |
    {'DOWN', reference(), process, pid(), term()} |
    {nodedown, node()} | {nodeup, node()}, State :: state()) -> {noreply, state()}.

%% remote pid or several pids joining the group
handle_info({join, Peer, Group, PidOrPids}, #state{scope = Scope, nodes = Nodes} = State) ->
    join_remote(Scope, Group, PidOrPids),
    % store remote group => pids map for fast sync operation
    {MRef, RemoteGroups} = maps:get(Peer, Nodes),
    NewRemoteGroups = join_remote_map(Group, PidOrPids, RemoteGroups),
    {noreply, State#state{nodes = Nodes#{Peer => {MRef, NewRemoteGroups}}}};

%% remote pid leaving (multiple groups at once)
handle_info({leave, Peer, PidOrPids, Groups}, #state{scope = Scope, nodes = Nodes} = State) ->
    _ = leave_remote(Scope, PidOrPids, Groups),
    {MRef, RemoteMap} = maps:get(Peer, Nodes),
    NewRemoteMap = lists:foldl(
        fun (Group, Acc) ->
            case maps:get(Group, Acc) of
                PidOrPids ->
                    Acc;
                [PidOrPids] ->
                    Acc;
                Existing when is_pid(PidOrPids) ->
                    Acc#{Group => lists:delete(PidOrPids, Existing)};
                Existing ->
                    Acc#{Group => Existing-- PidOrPids}
            end
        end, RemoteMap, Groups),
    {noreply, State#state{nodes = Nodes#{Peer => {MRef, NewRemoteMap}}}};

%% we're being discovered, let's exchange!
handle_info({discover, Peer}, #state{scope = Scope, nodes = Nodes} = State) ->
    gen_server:cast(Peer, {sync, self(), all_local_pids(Scope)}),
    %% do we know who is looking for us?
    case maps:is_key(Peer, Nodes) of
        true ->
            {noreply, State};
        false ->
            MRef = monitor(process, Peer),
            erlang:send(Peer, {discover, self()}, [noconnect]),
            {noreply, State#state{nodes = Nodes#{Peer => {MRef, #{}}}}}
    end;

%% handle local process exit
handle_info({'DOWN', MRef, process, Pid, _Info}, #state{scope = Scope, monitors = Monitors, nodes = Nodes} = State) when node(Pid) =:= node() ->
    case maps:take(Pid, Monitors) of
        error ->
            %% this can only happen when leave request and 'DOWN' are in pg queue
            {noreply, State};
        {{MRef, Groups}, NewMons} ->
            [leave_local_group(Scope, Group, Pid) || Group <- Groups],
            %% send update to all nodes
            broadcast(maps:keys(Nodes), {leave, self(), Pid, Groups}),
            {noreply, State#state{monitors = NewMons}}
    end;

%% handle remote node down or leaving overlay network
handle_info({'DOWN', MRef, process, Pid, _Info}, #state{scope = Scope, nodes = Nodes} = State)  ->
    {{MRef, RemoteMap}, NewNodes} = maps:take(Pid, Nodes),
    _ = maps:map(fun (Group, Pids) -> leave_remote(Scope, Pids, [Group]) end, RemoteMap),
    {noreply, State#state{nodes = NewNodes}};

%% nodedown: ignore, and wait for 'DOWN' signal for monitored process
handle_info({nodedown, _Node}, State) ->
    {noreply, State};

%% nodeup: discover if remote node participates in the overlay network
handle_info({nodeup, Node}, #state{scope = Scope} = State) ->
    {Scope, Node} ! {discover, self()},
    {noreply, State};

handle_info(_Info, _State) ->
    error(badarg).

-spec terminate(Reason :: any(), State :: state()) -> true.
terminate(_Reason, #state{scope = Scope}) ->
    true = ets:delete(Scope).

%%--------------------------------------------------------------------
%% Internal implementation

%% Override all knowledge of the remote node with information it sends
%%  to local node. Current implementation must do the full table scan
%%  to remove stale pids (just as for 'nodedown').
handle_sync(Scope, Peer, Nodes, Groups) ->
    %% can't use maps:get() because it evaluates 'default' value first,
    %%   and in this case monitor() call has side effect.
    {MRef, RemoteGroups} =
        case maps:find(Peer, Nodes) of
            error ->
                {monitor(process, Peer), #{}};
            {ok, MRef0} ->
                MRef0
        end,
    %% sync RemoteMap and transform ETS table
    _ = sync_groups(Scope, RemoteGroups, Groups),
    Nodes#{Peer => {MRef, maps:from_list(Groups)}}.

sync_groups(Scope, RemoteGroups, []) ->
    %% leave all missing groups
    [leave_remote(Scope, Pids, [Group]) || {Group, Pids} <- maps:to_list(RemoteGroups)];
sync_groups(Scope, RemoteGroups, [{Group, Pids} | Tail]) ->
    case maps:take(Group, RemoteGroups) of
        {Pids, NewRemoteGroups} ->
            sync_groups(Scope, NewRemoteGroups, Tail);
        {OldPids, NewRemoteGroups} ->
            [{Group, AllOldPids, LocalPids}] = ets:lookup(Scope, Group),
            %% should be really rare...
            AllNewPids = Pids ++ AllOldPids -- OldPids,
            true = ets:insert(Scope, {Group, AllNewPids, LocalPids}),
            sync_groups(Scope, NewRemoteGroups, Tail);
        error ->
            join_remote(Scope, Group, Pids),
            sync_groups(Scope, RemoteGroups, Tail)
    end.

join_monitors(Pid, Group, Monitors) when is_pid(Pid) ->
    case maps:find(Pid, Monitors) of
        {ok, {MRef, Groups}} ->
            maps:put(Pid, {MRef, [Group | Groups]}, Monitors);
        error ->
            MRef = erlang:monitor(process, Pid),
            Monitors#{Pid => {MRef, [Group]}}
    end;
join_monitors([], _Group, Monitors) ->
    Monitors;
join_monitors([Pid | Tail], Group, Monitors) ->
    join_monitors(Tail, Group, join_monitors(Pid, Group, Monitors)).

join_local_group(Scope, Group, Pid) when is_pid(Pid) ->
    case ets:lookup(Scope, Group) of
        [{Group, All, Local}] ->
            ets:insert(Scope, {Group, [Pid | All], [Pid | Local]});
        [] ->
            ets:insert(Scope, {Group, [Pid], [Pid]})
    end;
join_local_group(Scope, Group, Pids) ->
    case ets:lookup(Scope, Group) of
        [{Group, All, Local}] ->
            ets:insert(Scope, {Group, Pids ++ All, Pids ++ Local});
        [] ->
            ets:insert(Scope, {Group, Pids, Pids})
    end.

join_remote(Scope, Group, Pid) when is_pid(Pid) ->
    case ets:lookup(Scope, Group) of
        [{Group, All, Local}] ->
            ets:insert(Scope, {Group, [Pid | All], Local});
        [] ->
            ets:insert(Scope, {Group, [Pid], []})
    end;
join_remote(Scope, Group, Pids) ->
    case ets:lookup(Scope, Group) of
        [{Group, All, Local}] ->
            ets:insert(Scope, {Group, Pids ++ All, Local});
        [] ->
            ets:insert(Scope, {Group, Pids, []})
    end.

join_remote_map(Group, Pid, RemoteGroups) when is_pid(Pid) ->
    maps:update_with(Group, fun (List) -> [Pid | List] end, [Pid], RemoteGroups);
join_remote_map(Group, Pids, RemoteGroups) ->
    maps:update_with(Group, fun (List) -> Pids ++ List end, Pids, RemoteGroups).

leave_monitors(Pid, Group, Monitors) when is_pid(Pid) ->
    case maps:find(Pid, Monitors) of
        {ok, {MRef, [Group]}} ->
            erlang:demonitor(MRef),
            maps:remove(Pid, Monitors);
        {ok, {MRef, Groups}} ->
            case lists:member(Group, Groups) of
                true ->
                    maps:put(Pid, {MRef, lists:delete(Group, Groups)}, Monitors);
                false ->
                    Monitors
            end;
        _ ->
            Monitors
    end;
leave_monitors([], _Group, Monitors) ->
    Monitors;
leave_monitors([Pid | Tail], Group, Monitors) ->
    leave_monitors(Tail, Group, leave_monitors(Pid, Group, Monitors)).

leave_local_group(Scope, Group, Pid) when is_pid(Pid) ->
    case ets:lookup(Scope, Group) of
        [{Group, [Pid], [Pid]}] ->
            ets:delete(Scope, Group);
        [{Group, All, Local}] ->
            ets:insert(Scope, {Group, lists:delete(Pid, All), lists:delete(Pid, Local)});
        [] ->
            %% rare race condition when 'DOWN' from monitor stays in msg queue while process is leave-ing.
            true
    end;
leave_local_group(Scope, Group, Pids) ->
    case ets:lookup(Scope, Group) of
        [{Group, All, Local}] ->
            case All -- Pids of
                [] ->
                    ets:delete(Scope, Group);
                NewAll ->
                    ets:insert(Scope, {Group, NewAll, Local -- Pids})
            end;
        [] ->
            true
    end.

leave_remote(Scope, Pid, Groups) when is_pid(Pid) ->
    _ = [
        case ets:lookup(Scope, Group) of
            [{Group, [Pid], []}] ->
                ets:delete(Scope, Group);
            [{Group, All, Local}] ->
                ets:insert(Scope, {Group, lists:delete(Pid, All), Local});
            [] ->
                true
        end ||
        Group <- Groups];
leave_remote(Scope, Pids, Groups) ->
    _ = [
        case ets:lookup(Scope, Group) of
            [{Group, All, Local}] ->
                case All -- Pids of
                    [] when Local =:= [] ->
                        ets:delete(Scope, Group);
                    NewAll ->
                        ets:insert(Scope, {Group, NewAll, Local})
                end;
            [] ->
                true
        end ||
        Group <- Groups].

all_local_pids(Scope) ->
    %% selector: ets:fun2ms(fun({N,_,L}) when L =/=[] -> {N,L}end).
    ets:select(Scope, [{{'$1','_','$2'},[{'=/=','$2',[]}],[{{'$1','$2'}}]}]).

%% Works as gen_server:abcast(), but accepts a list of processes
%%   instead of nodes list.
broadcast([], _Msg) ->
    ok;
broadcast([Dest | Tail], Msg) ->
    %% do not use 'nosuspend', as it will lead to missing
    %%   join/leave messages when dist buffer is full
    erlang:send(Dest, Msg, [noconnect]),
    broadcast(Tail, Msg).
