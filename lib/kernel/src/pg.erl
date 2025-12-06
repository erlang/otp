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
-moduledoc """
Distributed named process groups.

This module implements process groups. A message can be sent to one, some, or
all group members.

Up until OTP 17 there used to exist an experimental `pg` module in `stdlib`.
This `pg` module is not the same module as that experimental `pg` module, and
only share the same module name.

A group of processes can be accessed by a common name. For example, if there is
a group named `foobar`, there can be a set of processes (which can be located on
different nodes) that are all members of the group `foobar`. There are no
special functions for sending a message to the group. Instead, client functions
are to be written with the functions `get_members/1` and `get_local_members/1`
to determine which processes are members of the group. Then the message can be
sent to one or more group members.

If a member terminates, it is automatically removed from the group.

A process may join multiple groups. It may join the same group multiple times.
It is only allowed to join processes running on local node.

Process Groups implement strong eventual consistency. Process Groups membership
view may temporarily diverge. For example, when processes on `node1` and `node2`
join concurrently, `node3` and `node4` may receive updates in a different order.

Membership view is not transitive. If `node1` is not directly connected to
`node2`, they will not see each other groups. But if both are connected to
`node3`, `node3` will have the full view.

Groups are automatically created when any process joins, and are removed when
all processes leave the group. Non-existing group is considered empty
(containing no processes).

Process groups can be organised into multiple scopes. Scopes are completely
independent of each other. A process may join any number of groups in any number
of scopes. Scopes are designed to decouple single mesh into a set of overlay
networks, reducing amount of traffic required to propagate group membership
information. Default scope `pg` is started automatically when
[Kernel](kernel_app.md#start_pg) is configured to do so.

> #### Note {: .info }
>
> Scope name is used to register process locally, and to name an ETS table. If
> there is another process registered under this name, or another ETS table
> exists, scope fails to start.
>
> Local membership is not preserved if scope process exits and restarts.
>
> A scope can be kept local-only by using a scope name that is unique
> cluster-wide, e.g. the node name: `pg:start_link(node()).`

## See Also

[Kernel](kernel_app.md)
""".
-moduledoc(#{since => "OTP 23.0"}).

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

%% data_publisher callbacks
-export([
    version/0,
    init_storage/1,
    init_data/0,
    stop_storage/1,
    update/2,
    update_storage_and_notify/4,
    data_diff/2,
    new_subscription/2,
    translate_update/3,
    translate_data/3,
    translate_message/2
]).

%% Types
-doc "The identifier of a process group.".
-type group() :: any().
-type scope() :: atom().
-type version() :: 0 | 1.
-type data() :: #{group() => [pid()]}.
-type update() :: [{group(), Add :: [pid()], Remove :: [pid()]}].
-type storage() :: {atom(), #{pid() => {reference(), [group()]}}}.
-type subscribe_result() :: #{group() => [pid()]} | [pid()].
-type subscription() :: scope | {group, group()}.
-type subscriptions() :: #{subscription() => #{reference() => pid()}}.

%% Default scope started by kernel app
-define(DEFAULT_SCOPE, ?MODULE).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server and links it to calling process.
%% Uses default scope, which is the same as as the module name.
-doc """
Starts the default `pg` scope within supervision tree.

Kernel may be configured to do it automatically by setting
the Kernel configuration parameter [`start_pg`](kernel_app.md#start_pg).
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    data_publisher:start_link(?DEFAULT_SCOPE, ?MODULE).

%% @doc
%% Starts the server outside of supervision hierarchy.
-doc "Starts additional scope.".
-doc(#{since => <<"OTP 23.0">>}).
-spec start(Scope :: atom()) -> {ok, pid()} | {error, any()}.
start(Scope) when is_atom(Scope) ->
    data_publisher:start(Scope, ?MODULE).

%% @doc
%% Starts the server and links it to calling process.
%% Scope name is supplied.
-doc """
Equivalent to [`start(Scope)`](`start/1`), except that it also creates
a `link/1` with the calling process.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec start_link(Scope :: atom()) -> {ok, pid()} | {error, any()}.
start_link(Scope) when is_atom(Scope) ->
    data_publisher:start_link(Scope, ?MODULE).

%%--------------------------------------------------------------------
%% @doc
%% Joins a single or a list of processes.
%% Group is created automatically.
%% Processes must be local to this node.
-doc(#{equiv => join(?DEFAULT_SCOPE, Group, PidOrPids)}).
-doc(#{since => <<"OTP 23.0">>}).
-spec join(Group :: group(), PidOrPids :: pid() | [pid()]) -> ok.
join(Group, PidOrPids) ->
    join(?DEFAULT_SCOPE, Group, PidOrPids).

-doc """
Joins single process or multiple processes to the group `Group`. A process can
join a group many times and must then leave the group the same number of times.

`PidOrPids` may contain the same process multiple times.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec join(Scope :: atom(), Group :: group(), PidOrPids :: pid() | [pid()]) -> ok.
join(Scope, Group, PidOrPids) when is_pid(PidOrPids); is_list(PidOrPids) ->
    ok = ensure_local(PidOrPids),
    data_publisher:update(Scope, [{Group, to_list(PidOrPids), []}]).

%%--------------------------------------------------------------------
%% @doc
%% Single or list of processes leaving the group.
%% Processes must be local to this node.
-doc(#{equiv => leave(?DEFAULT_SCOPE, Group, PidOrPids)}).
-doc(#{since => <<"OTP 23.0">>}).
-spec leave(Group :: group(), PidOrPids :: pid() | [pid()]) -> ok.
leave(Group, PidOrPids) ->
    leave(?DEFAULT_SCOPE, Group, PidOrPids).

-doc """
Makes the process `PidOrPids` leave the group `Group`. If the process is not a
member of the group, `not_joined` is returned.

When list of processes is passed as `PidOrPids`, function returns `not_joined`
only when all processes of the list are not joined.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec leave(Scope :: atom(), Group :: group(), PidOrPids :: pid() | [pid()]) -> ok | not_joined.
leave(Scope, Group, PidOrPids) when is_pid(PidOrPids); is_list(PidOrPids) ->
    ok = ensure_local(PidOrPids),
    Pids = to_list(PidOrPids),
    case Pids -- ets:lookup_element(Scope, Group, 2, []) of
        Pids ->
            not_joined;
        _ ->
            data_publisher:update(Scope, [{Group, [], Pids}])
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns currently known groups, and begins monitoring
%%  all group changes. Calling process will receive {Ref, join, Group, Pids}
%%  message when new Pids join the Group, and {Ref, leave, Group, Pids} when
%%  Pids leave the group.
-doc(#{equiv => monitor_scope(?DEFAULT_SCOPE)}).
-doc(#{since => <<"OTP 25.1">>}).
-spec monitor_scope() -> {reference(), #{group() => [pid()]}}.
monitor_scope() ->
    monitor_scope(?DEFAULT_SCOPE).

-doc """
Subscribes the caller to updates from the specified scope.

Returns content of the entire scope and a reference to match the upcoming
notifications.

Whenever any group membership changes, an update message is sent to the
subscriber:

```erlang
{Ref, join, Group, [JoinPid1, JoinPid2]}
```

```erlang
{Ref, leave, Group, [LeavePid1]}
```
""".
-doc(#{since => <<"OTP 25.1">>}).
-spec monitor_scope(Scope :: atom()) -> {reference(), #{group() => [pid()]}}.
monitor_scope(Scope) ->
    data_publisher:subscribe(Scope, scope).

%%--------------------------------------------------------------------
%% @doc
%% Returns list of processes in the requested group, and begins monitoring
%%  group changes. Calling process will receive {Ref, join, Group, Pids}
%%  message when new Pids join the Group, and {Ref, leave, Group, Pids} when
%%  Pids leave the group.
-doc(#{equiv => monitor(?DEFAULT_SCOPE, Group)}).
-doc(#{since => <<"OTP 25.1">>}).
-spec monitor(Group :: group()) -> {reference(), [pid()]}.
monitor(Group) ->
    ?MODULE:monitor(?DEFAULT_SCOPE, Group).

%%--------------------------------------------------------------------
%% @doc
%% Returns list of processes in the requested group, and begins monitoring
%%  group changes. Calling process will receive {Ref, join, Group, Pids}
%%  message when new Pids join the Group, and {Ref, leave, Group, Pids} when
%%  Pids leave the group.
-doc """
Subscribes the caller to updates for the specified group.

Returns list of processes currently in the group, and a reference to match the
upcoming notifications.

See `monitor_scope/0` for the update message structure.
""".
-doc(#{since => <<"OTP 25.1">>}).
-spec monitor(Scope :: atom(), Group :: group()) -> {reference(), [pid()]}.
monitor(Scope, Group) ->
    data_publisher:subscribe(Scope, {group, Group}).

%%--------------------------------------------------------------------
%% @doc
%% Stops monitoring Scope for groups changes. Flushes all
%%  {Ref, join|leave, Group, Pids} messages from the calling process queue.
-doc(#{equiv => demonitor(?DEFAULT_SCOPE, Ref)}).
-doc(#{since => <<"OTP 25.1">>}).
-spec demonitor(Ref :: reference()) -> ok | false.
demonitor(Ref) ->
    ?MODULE:demonitor(?DEFAULT_SCOPE, Ref).

-doc """
Unsubscribes the caller from updates (scope or group). Flushes all outstanding
updates that were already in the message queue of the calling process.
""".
-doc(#{since => <<"OTP 25.1">>}).
-spec demonitor(Scope :: atom(), Ref :: reference()) -> ok | false.
demonitor(Scope, Ref) ->
    data_publisher:unsubscribe(Scope, Ref),
    flush(Ref).

%%--------------------------------------------------------------------
%% @doc
%% Returns all processes in a group
-doc(#{equiv => get_members(?DEFAULT_SCOPE, Group)}).
-doc(#{since => <<"OTP 23.0">>}).
-spec get_members(Group :: group()) -> [pid()].
get_members(Group) ->
    get_members(?DEFAULT_SCOPE, Group).

-doc """
Returns all processes in the group `Group`. Processes are returned in no
specific order. This function is optimised for speed.
""".
-doc(#{since => <<"OTP 23.0">>}).
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
-doc(#{equiv => get_local_members(?DEFAULT_SCOPE, Group)}).
-doc(#{since => <<"OTP 23.0">>}).
-spec get_local_members(Group :: group()) -> [pid()].
get_local_members(Group) ->
    get_local_members(?DEFAULT_SCOPE, Group).

-doc """
Returns all processes running on the local node in the group `Group`. Processes
are returned in no specific order. This function is optimised for speed.
""".
-doc(#{since => <<"OTP 23.0">>}).
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
-doc(#{equiv => which_groups(?DEFAULT_SCOPE)}).
-doc(#{since => <<"OTP 23.0">>}).
-spec which_groups() -> [Group :: group()].
which_groups() ->
    which_groups(?DEFAULT_SCOPE).

-doc "Returns a list of all known groups.".
-doc(#{since => <<"OTP 23.0">>}).
-spec which_groups(Scope :: atom()) -> [Group :: group()].
which_groups(Scope) when is_atom(Scope) ->
    [G || [G] <- ets:match(Scope, {'$1', '_', '_'})].

%%--------------------------------------------------------------------
%% @private
%% Returns a list of groups that have any local processes joined.
-doc false.
-spec which_local_groups() -> [Group :: group()].
which_local_groups() ->
    which_local_groups(?DEFAULT_SCOPE).

-doc false.
-spec which_local_groups(Scope :: atom()) -> [Group :: group()].
which_local_groups(Scope) when is_atom(Scope) ->
    ets:select(Scope, [{{'$1', '_', '$2'}, [{'=/=', '$2', []}], ['$1']}]).

%%--------------------------------------------------------------------
%% Internal implementation

%% data_published implementation
-spec version() -> version().
version() ->
    1.

-spec init_storage(scope()) -> storage().
init_storage(Scope) ->
    _ = ets:new(Scope, [set, protected, named_table, {read_concurrency, true}]),
    {Scope, #{}}.

-spec init_data() -> data().
init_data() ->
    #{}.

-spec stop_storage(storage()) -> term().
stop_storage({Scope, _Monitors}) ->
    ets:delete(Scope).

-spec update(update(), data()) -> data().
update([], Data) ->
    Data;
update([{Group, Add, Remove} | Tail], Data) ->
    case Data of
        #{Group := Pids} ->
            case Add ++ (Pids -- Remove) of
                [] ->
                    update(Tail, maps:remove(Group, Data));
                NewPids ->
                    update(Tail, Data#{Group => NewPids})
            end;
        _ ->
            update(Tail, Data#{Group => Add})
    end.

-spec update_storage_and_notify(node(), update(), subscriptions(), storage()) -> storage().
update_storage_and_notify(_Node, [], _Subscriptions, Storage) ->
    Storage;
update_storage_and_notify(Node, [{Group, Add, Remove} | Tail], Subscriptions, {Scope, Monitors}) when Node =:= node()->
    case ets:lookup(Scope, Group) of
        [{_Group, All, Local}] ->
            case Add ++ (All -- Remove) of
                [] ->
                    ets:delete(Scope, Group);
                NewAll ->
                    ets:insert(Scope, {Group, NewAll, Add ++ (Local -- Remove)})
            end;
        _ ->
            ets:insert(Scope, {Group, Add, Add})
    end,
    _ = notify(Group, Add, Remove, Subscriptions),
    MonitorsAfterAdd =
        lists:foldl(
            fun(Pid, Acc) ->
                case Acc of
                    #{Pid := {Ref, Groups}} ->
                        Acc#{Pid => {Ref, [Group | Groups]}};
                    _ ->
                        Ref = erlang:monitor(process, Pid, [{tag, {'DOWN', ?MODULE}}]),
                        Acc#{Pid => {Ref, [Group]}}
                end
            end,
            Monitors,
            Add
        ),
    MonitorsAfterRemove =
        lists:foldl(
            fun(Pid, Acc) ->
                case Acc of
                    #{Pid := {Ref, [Group]}} ->
                        erlang:demonitor(Ref),
                        maps:remove(Ref, Acc);
                    #{Pid := {Ref, Groups}} ->
                        Acc#{Pid => {Ref, Groups -- [Group]}};
                    _ ->
                        Acc
                end
            end,
            MonitorsAfterAdd,
            Remove
        ),
    update_storage_and_notify(Node, Tail, Subscriptions, {Scope, MonitorsAfterRemove});
update_storage_and_notify(Node, [{Group, Add, Remove} | Tail], Subscriptions, {Scope, Monitors}) ->
    case ets:lookup(Scope, Group) of
        [{_Group, All, Local}] ->
            ets:insert(Scope, {Group, Add ++ (All -- Remove), Local});
        _ ->
            ets:insert(Scope, {Group, Add, []})
    end,
    _ = notify(Group, Add, Remove, Subscriptions),
    update_storage_and_notify(Node, Tail, Subscriptions, {Scope, Monitors}).

-spec data_diff(Old :: data(), New :: data()) -> update().
data_diff(Old, New) ->
    OldKeys = maps:keys(Old),
    NewKeys = maps:keys(New),
    [
        {Group, Add, Remove}
     || Group <- (OldKeys -- NewKeys) ++ NewKeys,
        case {Old, New} of
            {#{Group := OldGroups}, #{Group := NewGroups}} ->
                Add = NewGroups -- OldGroups,
                Remove = OldGroups -- NewGroups,
                Add =/= [] orelse Remove =/= [];
            {#{Group := OldGroups}, _} ->
                Add = [],
                Remove = OldGroups,
                Remove =/= [];
            {_, #{Group := NewGroups}} ->
                Add = NewGroups,
                Remove = [],
                Add =/= []
        end
    ].

-spec new_subscription(subscription(), storage()) -> subscribe_result().
new_subscription(scope, {Scope, _Monitors}) ->
    #{G => P || [G, P] <- ets:match(Scope, {'$1', '$2', '_'})};
new_subscription({group, Group}, {Scope, _Monitors}) ->
    get_members(Scope, Group).

-spec translate_update(MyVersion :: version(), PeerVersion :: version(), update()) ->
    update() | {'$plain_messages', [dynamic()]}.
% legacy support, to be removed
translate_update(1, 0, Update) ->
    Leaves = maps:groups_from_list(
        fun({_Group, _Add, Remove}) -> Remove end,
        fun({Group, _Add, _Remove}) -> Group end,
        Update
    ),
    LeaveMessages =
        [
            {leave, self(), Remove, Groups}
         || Remove := Groups <- Leaves
        ],
    JoinMessages =
        [
            {join, self(), Group, Add}
         || {Group, Add, _Remove} <- Update
        ],
    {'$plain_messages', LeaveMessages ++ JoinMessages};
translate_update(1, _, Update) ->
    Update.

-spec translate_data(MyVersion :: version(), PeerVersion :: version(), data()) ->
    data() | {'$plain_message', dynamic()}.
% legacy support, to be removed
translate_data(1, 0, Data) ->
    {'$plain_message', {sync, self(), maps:to_list(Data)}};
translate_data(1, _, Data) ->
    Data.

-spec translate_message
    ({join, pid(), group(), pid() | [pid()]}, storage()) -> {update, pid(), update(), storage()};
    ({leave, pid(), pid() | [pid()], [group()]}, storage()) -> {update, pid(), update(), storage()};
    ({{'DOWN', ?MODULE}, reference(), process, pid(), dynamic()}, storage()) -> {update, update(), storage()};
    ({sync, pid(), [{group(), [pid()]}]}, storage()) -> {data, pid(), version(), data(), storage()};
    ({discover, pid()}, storage()) -> {discover, pid(), version(), storage()}.
% legacy support, to be removed
translate_message({join, Peer, Group, PidOrPids}, Storage) ->
    {update, Peer, [{Group, to_list(PidOrPids), []}], Storage};
% legacy support, to be removed
translate_message({leave, Peer, PidOrPids, Groups}, Storage) ->
    Update =
        [
            {Group, [], to_list(PidOrPids)}
         || Group <- Groups
        ],
    {update, Peer, Update, Storage};
translate_message({{'DOWN', ?MODULE}, Ref, process, Pid, _Info}, {Scope, Monitors} = Storage) ->
    case Monitors of
        #{Pid := {Ref, Groups}} ->
            Update = [{Group, [], [Pid]} || Group <- Groups],
            {update, Update, {Scope, maps:remove(Ref, Monitors)}};
        _ ->
            {drop, Storage}
    end;
% legacy support, to be removed
translate_message({sync, Peer, Sync}, Storage) ->
    {data, Peer, 0, maps:from_list(Sync), Storage};
% legacy support, to be removed
translate_message({discover, Peer}, Storage) ->
    {discover, Peer, 0, Storage};
translate_message(_, _Storage) ->
    error(badarg).
    %{drop, Storage}.

-spec notify(group(), [pid()], [pid()], subscriptions()) -> term().
notify(Group, Add, Remove, Subscriptions) ->
    [
        notify(Group, Add, Remove, Ref, Pid)
     || Ref := Pid <- maps:get(scope, Subscriptions, #{})
    ],
    [
        notify(Group, Add, Remove, Ref, Pid)
     || Ref := Pid <- maps:get({group, Group}, Subscriptions, #{})
    ].

-spec notify(group(), [pid()], [pid()], reference(), pid()) -> term().
notify(Group, Add, Remove, Ref, Pid) ->
    Add =/= [] andalso erlang:send(Pid, {Ref, join, Group, Add}, [noconnect]),
    Remove =/= [] andalso erlang:send(Pid, {Ref, leave, Group, Remove}, [noconnect]).

-spec to_list(pid() | [pid()]) -> [pid()].
to_list(PidOrPids) when is_list(PidOrPids) ->
    PidOrPids;
to_list(PidOrPids) ->
    [PidOrPids].

%%--------------------------------------------------------------------
%% Internal implementation

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

%% remove all messages that were send to monitor groups
flush(Ref) ->
    receive
        {Ref, Verb, _Group, _Pids} when Verb =:= join; Verb =:= leave ->
            flush(Ref)
    after 0 ->
        ok
    end.
