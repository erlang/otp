%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1998-2025. All Rights Reserved.
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
-module(global_group).
-moduledoc """
Grouping nodes to global name registration groups.

This module makes it possible to partition the nodes of a system into _global
groups_. Each global group has its own global namespace, see `m:global`.

The main advantage of dividing systems into global groups is that the background
load decreases while the number of nodes to be updated is reduced when
manipulating globally registered names.

The Kernel configuration parameter [`global_groups`](kernel_app.md#global_groups)
defines the global groups:

```erlang
{global_groups, [GroupTuple :: group_tuple()]}
```

For the processes and nodes to run smoothly using the global group
functionality, the following criteria must be met:

- An instance of the global group server, `global_group`, must be running on
  each node. The processes are automatically started and synchronized when a
  node is started.
- All involved nodes must agree on the global group definition, otherwise the
  behavior of the system is undefined.
- _All_ nodes in the system must belong to exactly one global group.

In the following descriptions, a _group node_ is a node belonging to the same
global group as the local node.

## Notes

- In the situation where a node has lost its connections to other nodes in its
  global group, but has connections to nodes in other global groups, a request
  from another global group can produce an incorrect or misleading result. For
  example, the isolated node can have inaccurate information about registered
  names in its global group.
- Function [`send/2,3`](`send/2`) is not secure.
- Distribution of applications is highly dependent of the global group
  definitions. It is not recommended that an application is distributed over
  many global groups, as the registered names can be moved to another global
  group at failover/takeover. Nothing prevents this to be done, but the
  application code must then handle the situation.

## See Also

`m:global`, [`erl`](`e:erts:erl_cmd.md`)
""".

-compile(nowarn_deprecated_catch).

%% Groups nodes into global groups with an own global name space.

-behaviour(gen_server).

%% External exports
-export([start/0, start_link/0, stop/0, init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-export([global_groups/0]).
-export([monitor_nodes/1]).
-export([own_nodes/0]).
-export([registered_names/1]).
-export([send/2]).
-export([send/3]).
-export([whereis_name/1]).
-export([whereis_name/2]).
-export([sync/0]).
-export([info/0]).

%% Kernel application internal exports

-export([global_groups_changed/1,
         global_groups_added/1,
         global_groups_removed/1,
         get_own_nodes/0,
         get_own_nodes_with_errors/0,
         member/1,
         participant/1,
         publish/2,
         group_configured/0]).

%% Internal exports
-export([ng_add_check/2,
         ng_add_check/3,
         registered_names_test/1,
         send_test/2,
         whereis_name_test/1]).

-define(cc_vsn, 3).

%%%====================================================================================

-doc """
A node started with command-line flag `-hidden` (see
[`erl`](`e:erts:erl_cmd.md`)) is said to be a _hidden_ node. A hidden node
establishes hidden connections to nodes not part of the same global group, but
normal (visible) connections to nodes part of the same global group.

A global group defined with `PublishType` equal to `hidden` is said to be a
hidden global group. All nodes in a hidden global group are hidden nodes,
whether they are started with command-line flag `-hidden` or not.
""".
-type publish_type() :: 'hidden' | 'normal'.
-type sync_state()   :: 'no_conf' | 'synced'.

-type group_name()  :: atom().
-doc """
A `GroupTuple` without `PublishType` is the same as a `GroupTuple` with
`PublishType` equal to `normal`.
""".
-type group_tuple() :: {GroupName :: group_name(), [node()]}
                     | {GroupName :: group_name(),
                        PublishType :: publish_type(),
                        [node()]}.
-type node_state() :: 'sync' | 'sync_error' | 'no_contact'.
%%%====================================================================================
%%% The state of the global_group process
%%% 
%%% sync_state =  no_conf (global_groups not defined, initial state) |
%%%               synced 
%%% group_name =  Own global group name
%%% nodes =       Nodes in the own global group
%%% no_contact =  Nodes which we haven't had contact with yet
%%% sync_error =  Nodes which we haven't had contact with yet
%%% other_grps =  list of other global group names and nodes, [{otherName, [Node]}]
%%% monitor =     List of Pids requesting nodeup/nodedown
%%%====================================================================================

-record(state, {sync_state = no_conf        :: sync_state(),
		group_name = []             :: group_name() | [],
		nodes = #{}                 :: #{node() => node_state()},
		other_grps = [], 
		monitor = [],
		group_publish_type = normal :: publish_type(),
                connections                 :: #{node() => non_neg_integer()},
                erpc_requests               :: erpc:request_id_collection(),
                config_check                :: 'undefined'
                                             | {reference(),
                                                #{node() => reference()}}}).

%%%====================================================================================
%%% Configuration record. Returned by:
%%% * fetch_new_group_conf/1,2        -- Fetch and install new configuration
%%% * new_group_conf/2,3              -- Install new configuration
%%% * lookup_group_conf/1             -- Lookup installed configuration (will fetch
%%%                                      and install configuration if it has not
%%%                                      been initialized)
%%% * alive_state_change_group_conf/1 -- Adjust configuration according to alive
%%%                                      state
%%%====================================================================================
-record(gconf, {parameter_value = invalid   :: [group_tuple()]
                                             | undefined
                                             | invalid,
                node_name                   :: node() | 'undefined',
                group_name = []             :: group_name() | [],
                group_publish_type = normal :: publish_type(),
                group_list = []             :: [node()],
                group_map = all             :: 'all' | #{ node() => ok },
                other_groups = []           :: [group_tuple()],
                state = no_conf             :: 'no_conf'
                                             | 'conf'
                                             | {error,
                                                term(),
                                                [group_tuple()]}}).

%%%====================================================================================
%%% External exported
%%%====================================================================================

-doc """
Returns a tuple containing the name of the global group that the local node
belongs to, and the list of all other known group names.

Returns `undefined` if no global groups are defined.
""".
-spec global_groups() -> {GroupName, GroupNames} | undefined when
      GroupName :: group_name(),
      GroupNames :: [GroupName].
global_groups() ->
    request(global_groups).

-doc """
Alter the calling process' subscription of node status change messages.

If `Flag` is equal to `true` the calling process starts subscribing to
node status change messages. If equal to `false` it stops subscribing.

A process that has subscribed receives the messages `{nodeup, Node}` and
`{nodedown, Node}` when a group node connects or disconnects, respectively.
""".
-spec monitor_nodes(Flag) -> 'ok' when
      Flag :: boolean().
monitor_nodes(Flag) -> 
    case Flag of
	true -> request({monitor_nodes, Flag});
	false -> request({monitor_nodes, Flag});
	_ -> {error, not_boolean}
    end.

-doc "Returns the names of all group nodes, regardless of their current status.".
-spec own_nodes() -> Nodes when
      Nodes :: [Node :: node()].
own_nodes() ->
    request(own_nodes).

-doc "A registered name.".
-type name()  :: atom().
-type where() :: {'node', node()} | {'group', group_name()}.

-doc """
Returns a list of all names that are globally registered on the specified node
or in the specified global group.
""".
-spec registered_names(Where) -> Names when
      Where :: where(),
      Names :: [Name :: name()].
registered_names(Arg) ->
    request({registered_names, Arg}).

-doc """
Sends `Msg` to the pid represented by the globally registered name `Name`.

`send/2` searches for `Name` any any global group. The global groups are searched
in the order that they appear in the value of configuration parameter
[`global_groups`](kernel_app.md#global_groups).

If `Name` is found, message `Msg` is sent to the corresponding pid. The pid is
also the return value of the function. If the name is not found, the function
returns `{badarg, {Name, Msg}}`.
""".
-spec send(Name, Msg) -> pid() | {'badarg', {Name, Msg}} when
      Name :: name(),
      Msg :: term().
send(Name, Msg) ->
    request({send, Name, Msg}).

-doc """
Equivalent to [`send(Name, Msg)`](`send/2`) except that he search is limited
to the node or global group specified by `Where`.
""".
-spec send(Where, Name, Msg) -> pid() | {'badarg', {Name, Msg}} when
      Where :: where(),
      Name :: name(),
      Msg :: term().
send(Group, Name, Msg) ->
    request({send, Group, Name, Msg}).

-doc """
Searched for `Name` in any global group.

The global groups are searched in the order that they appear in the value
of configuration parameter `global_groups`.

If `Name` is found, the corresponding pid is returned. If the name is not found,
the function returns `undefined`.
""".
-spec whereis_name(Name) -> pid() | 'undefined' when
      Name :: name().
whereis_name(Name) ->
    request({whereis_name, Name}).

-doc """
Equivalent to [`whereis_name(Name)`](`whereis_name/1`) except that he search is limited
to the node or global group specified by `Where`.
""".
-spec whereis_name(Where, Name) -> pid() | 'undefined' when
      Where :: where(),
      Name :: name().
whereis_name(Group, Name) ->
    request({whereis_name, Group, Name}).

-doc false.
global_groups_changed(NewPara) ->
    request({global_groups_changed, NewPara}).

-doc false.
global_groups_added(NewPara) ->
    request({global_groups_added, NewPara}).

-doc false.
global_groups_removed(NewPara) ->
    request({global_groups_removed, NewPara}).

-doc """
Synchronizes the group nodes, that is, the global name servers on the group
nodes. Also checks the names globally registered in the current global group and
unregisters them on any known node not part of the group.

If synchronization is not possible, an error report is sent to the error logger
(see also `m:error_logger`.

Returns `{error, {'invalid global_groups definition', Bad}}` if configuration
parameter `global_groups` has an invalid value `Bad`.
""".
-spec sync() -> 'ok'.
sync() ->
    request(sync).

-type info_item() :: {'state', State :: sync_state()}
                   | {'own_group_name', GroupName :: group_name()}
                   | {'own_group_nodes', Nodes :: [node()]}
                   | {'synched_nodes', Nodes :: [node()]}
                   | {'sync_error', Nodes :: [node()]}
                   | {'no_contact', Nodes :: [node()]}
                   | {'other_groups', Groups :: [group_tuple()]}
                   | {'monitoring', Pids :: [pid()]}.

-doc """
Returns a list containing information about the global groups. Each list element
is a tuple. The order of the tuples is undefined.

- **`{state, State}`** - If the local node is part of a global group, `State` is
  equal to `synced`. If no global groups are defined, `State` is equal to
  `no_conf`.

- **`{own_group_name, GroupName}`** - The name (atom) of the group that the
  local node belongs to.

- **`{own_group_nodes, Nodes}`** - A list of node names (atoms), the group
  nodes.

- **`{synced_nodes, Nodes}`** - A list of node names, the group nodes currently
  synchronized with the local node.

- **`{sync_error, Nodes}`** - A list of node names, the group nodes with which
  the local node has failed to synchronize.

- **`{no_contact, Nodes}`** - A list of node names, the group nodes to which
  there are currently no connections.

- **`{other_groups, Groups}`** - `Groups` is a list of tuples
  `{GroupName, Nodes}`, specifying the name and nodes of the other global
  groups.

- **`{monitoring, Pids}`** - A list of pids, specifying the processes that have
  subscribed to `nodeup` and `nodedown` messages.
""".
-spec info() -> [info_item()].
info() ->
    request(info, 3000).

%% global_group internal...

-doc false.
ng_add_check(Node, OthersNG) ->
    ng_add_check(Node, normal, OthersNG).

-doc false.
ng_add_check(Node, PubType, OthersNG) ->
    request({ng_add_check, Node, PubType, OthersNG}).

%% ==== ONLY for test suites ====
-doc false.
registered_names_test(Arg) ->
    request({registered_names_test, Arg}).
-doc false.
send_test(Name, Msg) ->
    request({send_test, Name, Msg}).
-doc false.
whereis_name_test(Name) ->
    request({whereis_name_test, Name}).
%% ==== ONLY for test suites ====


request(Req) ->
    request(Req, infinity).

request(Req, Time) ->
    case whereis(global_group) of
	P when is_pid(P) ->
	    gen_server:call(global_group, Req, Time);
	_Other -> 
	    {error, global_group_not_runnig}
    end.

%%%====================================================================================
%%% gen_server start
%%%
%%% The first thing to happen is to read if the global_groups key is defined in the
%%% .config file. If not defined, the whole system is started as one global_group, 
%%% and the services of global_group are superfluous.
%%% Otherwise a sync process is started to check that all nodes in the own global
%%% group have the same configuration. This is done by sending 'conf_check' to all
%%% other nodes and requiring 'conf_check_result' back.
%%% If the nodes are not in agreement of the configuration the global_group process,
%%% these nodes will be set in a state different than 'sync'. This can be a normal case
%%% at release upgrade when all nodes are not yet upgraded.
%%%
%%% It is possible to manually force a sync of the global_group. This is done for 
%%% instance after a release upgrade, after all nodes in the group being upgraded.
%%% The nodes are not synced automatically because it would cause the node to be
%%% disconnected from those not yet being upgraded.
%%%
%%% The three process dictionary variables (registered_names, send, and whereis_name) 
%%% are used to store information needed if the search process crashes. 
%%% The search process is a help process to find registered names in the system.
%%%====================================================================================
-doc false.
start() -> gen_server:start({local, global_group}, global_group, [], []).
-doc false.
start_link() -> gen_server:start_link({local, global_group}, global_group,[],[]).
-doc false.
stop() -> gen_server:call(global_group, stop, infinity).

-doc false.
init([]) ->
    _ = process_flag(async_dist, true),
    process_flag(priority, max),
    ok = net_kernel:monitor_nodes(true, #{connection_id => true}),
    put(registered_names, [undefined]),
    put(send, [undefined]),
    put(whereis_name, [undefined]),
    process_flag(trap_exit, true),

    GGC = spawn_link(fun global_group_check_dispatcher/0),
    register(global_group_check, GGC),
    put(global_group_check, GGC),

    %% There are most likely no connected nodes at this stage,
    %% but check to make sure...
    Conns = lists:foldl(fun ({N, #{connection_id := CId}}, Cs) ->
                                Cs#{N => CId}
                        end,
                        #{},
                        nodes(visible, #{connection_id => true})),
    S = initial_group_setup(fetch_new_group_conf(true, node()), Conns,
                            erpc:reqids_new()),
    {ok, S}.

initial_group_setup(#gconf{state = no_conf}, Conns, Reqs) ->
    #state{connections = Conns,
           erpc_requests = Reqs};
initial_group_setup(#gconf{state = {error, _Err, NodeGrps}},
                    _Conns, _Reqs) ->
    exit({error, {'invalid global_groups definition', NodeGrps}});
initial_group_setup(#gconf{node_name = NodeName,
                           group_name = DefGroupName,
                           group_list = DefNodesT,
                           group_publish_type = PubTpGrp,
                           other_groups = DefOther}, Conns, Reqs) ->

    DefNodes = lists:delete(NodeName, DefNodesT),
    ConnectedNodes = maps:keys(Conns),
    DisconnectNodes = ConnectedNodes -- DefNodes,
    NotConnectedOwnNodes = DefNodes -- ConnectedNodes,
    ConnectedOwnNodes = DefNodes -- NotConnectedOwnNodes,
    %% First disconnect any nodes not belonging to our own group
    disconnect_nodes(DisconnectNodes, Conns),
    
    %% Schedule group consistency check for all nodes. The response
    %% is later handled in handle_erpc_response(). 
    NewReqs = schedule_conf_changed_checks(DefNodes, Reqs, Conns),

    %% Set connected nodes in sync_error state since, we
    %% have not yet verified their configuration...
    Nodes0 = lists:foldl(fun (Node, Acc) ->
                                 Acc#{Node => sync_error}
                         end,
                         #{}, ConnectedOwnNodes),
    Nodes = lists:foldl(fun (Node, Acc) ->
                                Acc#{Node => no_contact}
                        end,
                        Nodes0, NotConnectedOwnNodes),

    #state{group_publish_type = PubTpGrp,
           sync_state = synced, group_name = DefGroupName,
           nodes = Nodes, other_grps = DefOther,
           connections = Conns, erpc_requests = NewReqs}.

%%%====================================================================================
%%% sync() -> ok 
%%%
%%% An operator ordered sync of the own global group. This must be done after
%%% a release upgrade. It can also be ordered if something has made the nodes
%%% to disagree of the global_groups definition.
%%%====================================================================================
-doc false.
handle_call(sync, _From, #state{nodes = OldNodes,
                                connections = Conns} = S) ->
%    io:format("~p sync ~p~n",[node(), application:get_env(kernel, global_groups)]),

    case lookup_group_conf(true) of
        #gconf{state = no_conf,
               group_name = DefGroupName,
               group_list = _DefNodesT,
               group_publish_type = PubTpGrp,
               other_groups = DefOther} ->
	    {reply, ok, S#state{sync_state = no_conf,
                                group_name = DefGroupName,
                                nodes = #{},
                                group_publish_type = PubTpGrp,
                                other_grps = DefOther}};

        #gconf{state = {error, _Err, NodeGrps}} ->
            exit({error, {'invalid global_groups definition', NodeGrps}});

        #gconf{group_name = DefGroupName,
               group_list = DefNodesT,
               group_publish_type = PubTpGrp,
               other_groups = DefOther} ->
            DefNodes = lists:delete(node(), DefNodesT),
            %% First inform global on all nodes not belonging to our own group
            disconnect_nodes(nodes() -- DefNodes, Conns),
            %% Sync with the nodes in the own group

            SyncSession = make_ref(),

            CCMsg = {conf_check, ?cc_vsn, node(), {self(), SyncSession},
                     sync, DefGroupName, PubTpGrp, DefNodesT},
            {NewNodes, Mons}
                = lists:foldl(fun (N, {Nacc, Macc}) ->
                                      GG = {global_group, N},
                                      M = erlang:monitor(process, GG),
                                      gen_server:cast(GG, CCMsg),
                                      NS = maps:get(N, OldNodes, no_contact),
                                      {Nacc#{N => NS}, Macc#{N => M}}
                              end, {#{}, #{}}, DefNodes),
	    {reply, ok, S#state{sync_state = synced, group_name = DefGroupName,
                                nodes = NewNodes, other_grps = DefOther,
                                group_publish_type = PubTpGrp,
                                config_check = {SyncSession, Mons}}}
    end;

%%%====================================================================================
%%% global_groups() -> {OwnGroupName, [OtherGroupName]} | undefined
%%%
%%% Get the names of the global groups
%%%====================================================================================
handle_call(global_groups, _From, S) ->
    Result = case S#state.sync_state of
		 no_conf ->
		     undefined;
		 synced ->
		     Other = lists:foldl(fun({N,_L}, Acc) -> Acc ++ [N]
					 end,
					 [], S#state.other_grps),
		     {S#state.group_name, Other}
	     end,
    {reply, Result, S};



%%%====================================================================================
%%% monitor_nodes(bool()) -> ok 
%%%
%%% Monitor nodes in the own global group. 
%%%   True => send nodeup/nodedown to the requesting Pid
%%%   False => stop sending nodeup/nodedown to the requesting Pid
%%%====================================================================================
handle_call({monitor_nodes, Flag}, {Pid, _}, StateIn) ->
%    io:format("***** handle_call ~p~n",[monitor_nodes]),
    {Res, State} = monitor_nodes(Flag, Pid, StateIn),
    {reply, Res, State};

%%%====================================================================================
%%% own_nodes() -> [Node] 
%%%
%%% Get a list of nodes in the own global group
%%%====================================================================================
handle_call(own_nodes, _From, S) ->
    Nodes = case S#state.sync_state of
		no_conf ->
		    [node() | nodes()];
		synced ->
		    get_own_nodes(true)
	    end,
    {reply, Nodes, S};



%%%====================================================================================
%%% registered_names({node, Node}) -> [Name] | {error, ErrorMessage}
%%% registered_names({group, GlobalGroupName}) -> [Name] | {error, ErrorMessage}
%%%
%%% Get the registered names from a specified Node, or GlobalGroupName.
%%%====================================================================================
handle_call({registered_names, {group, Group}}, _From, S) when Group =:= S#state.group_name ->
    Res = global:registered_names(),
    {reply, Res, S};
handle_call({registered_names, {group, Group}}, From, S) ->
    case lists:keysearch(Group, 1, S#state.other_grps) of
	false ->
	    {reply, [], S};
	{value, {Group, []}} ->
	    {reply, [], S};
	{value, {Group, Nodes}} ->
	    Pid = global_search:start(names, {group, Nodes, From}),
	    Wait = get(registered_names),
	    put(registered_names, [{Pid, From} | Wait]),
	    {noreply, S}
    end;
handle_call({registered_names, {node, Node}}, _From, S) when Node =:= node() ->
    Res = global:registered_names(),
    {reply, Res, S};
handle_call({registered_names, {node, Node}}, From, S) ->
    Pid = global_search:start(names, {node, Node, From}),
%    io:format(">>>>> registered_names Pid ~p~n",[Pid]),
    Wait = get(registered_names),
    put(registered_names, [{Pid, From} | Wait]),
    {noreply, S};



%%%====================================================================================
%%% send(Name, Msg) -> Pid | {badarg, {Name, Msg}}
%%% send({node, Node}, Name, Msg) -> Pid | {badarg, {Name, Msg}}
%%% send({group, GlobalGroupName}, Name, Msg) -> Pid | {badarg, {Name, Msg}}
%%%
%%% Send the Msg to the specified globally registered Name in own global group,
%%% in specified Node, or GlobalGroupName.
%%% But first the receiver is to be found, the thread is continued at
%%% handle_cast(send_res)
%%%====================================================================================
%% Search in the whole known world, but check own node first.
handle_call({send, Name, Msg}, From, S) ->
    case global:whereis_name(Name) of
	undefined ->
	    Pid = global_search:start(send, {any, S#state.other_grps, Name, Msg, From}),
	    Wait = get(send),
	    put(send, [{Pid, From, Name, Msg} | Wait]),
	    {noreply, S};
	Found ->
	    Found ! Msg,
	    {reply, Found, S}
    end;
%% Search in the specified global group, which happens to be the own group.
handle_call({send, {group, Grp}, Name, Msg}, _From, S) when Grp =:= S#state.group_name ->
    case global:whereis_name(Name) of
	undefined ->
	    {reply, {badarg, {Name, Msg}}, S};
	Pid ->
	    Pid ! Msg,
	    {reply, Pid, S}
    end;
%% Search in the specified global group.
handle_call({send, {group, Group}, Name, Msg}, From, S) ->
    case lists:keysearch(Group, 1, S#state.other_grps) of
	false ->
	    {reply, {badarg, {Name, Msg}}, S};
	{value, {Group, []}} ->
	    {reply, {badarg, {Name, Msg}}, S};
	{value, {Group, Nodes}} ->
	    Pid = global_search:start(send, {group, Nodes, Name, Msg, From}),
	    Wait = get(send),
	    put(send, [{Pid, From, Name, Msg} | Wait]),
	    {noreply, S}
    end;
%% Search on the specified node.
handle_call({send, {node, Node}, Name, Msg}, From, S) ->
    Pid = global_search:start(send, {node, Node, Name, Msg, From}),
    Wait = get(send),
    put(send, [{Pid, From, Name, Msg} | Wait]),
    {noreply, S};



%%%====================================================================================
%%% whereis_name(Name) -> Pid | undefined
%%% whereis_name({node, Node}, Name) -> Pid | undefined
%%% whereis_name({group, GlobalGroupName}, Name) -> Pid | undefined
%%%
%%% Get the Pid of a globally registered Name in own global group,
%%% in specified Node, or GlobalGroupName.
%%% But first the process is to be found, 
%%% the thread is continued at handle_cast(find_name_res)
%%%====================================================================================
%% Search in the whole known world, but check own node first.
handle_call({whereis_name, Name}, From, S) ->
    case global:whereis_name(Name) of
	undefined ->
	    Pid = global_search:start(whereis, {any, S#state.other_grps, Name, From}),
	    Wait = get(whereis_name),
	    put(whereis_name, [{Pid, From} | Wait]),
	    {noreply, S};
	Found ->
	    {reply, Found, S}
    end;
%% Search in the specified global group, which happens to be the own group.
handle_call({whereis_name, {group, Group}, Name}, _From, S) 
  when Group =:= S#state.group_name ->
    Res = global:whereis_name(Name),
    {reply, Res, S};
%% Search in the specified global group.
handle_call({whereis_name, {group, Group}, Name}, From, S) ->
    case lists:keysearch(Group, 1, S#state.other_grps) of
	false ->
	    {reply, undefined, S};
	{value, {Group, []}} ->
	    {reply, undefined, S};
	{value, {Group, Nodes}} ->
	    Pid = global_search:start(whereis, {group, Nodes, Name, From}),
	    Wait = get(whereis_name),
	    put(whereis_name, [{Pid, From} | Wait]),
	    {noreply, S}
    end;
%% Search on the specified node.
handle_call({whereis_name, {node, Node}, Name}, From, S) ->
    Pid = global_search:start(whereis, {node, Node, Name, From}),
    Wait = get(whereis_name),
    put(whereis_name, [{Pid, From} | Wait]),
    {noreply, S};


%%%====================================================================================
%%% global_groups parameter changed
%%% The node is not resynced automatically because it would cause this node to
%%% be disconnected from those nodes not yet been upgraded.
%%%====================================================================================
handle_call({global_groups_changed, NewPara}, _From,
            #state{erpc_requests = Reqs,
                   nodes = OldNodes,
                   connections = Conns} = S) ->

    #gconf{group_name = NewGroupName,
           group_publish_type = PubTpGrp,
           group_list = NewNodesListT,
           other_groups = NewOther,
           state = GState} = new_group_conf(true, NewPara),

    case GState of
        no_conf ->
            exit({error, 'no global_groups definiton'});
        {error, _Err, NodeGrps} ->
            exit({error, {'invalid global_groups definition', NodeGrps}});
        _ ->
            ok
    end,

    NewNodesList = lists:delete(node(), NewNodesListT),

    %% Disconnect the connection to nodes which are not in our old global group.
    %% This is done because if we already are aware of new nodes (to our global
    %% group) global is not going to be synced to these nodes. We disconnect instead
    %% of connect because upgrades can be done node by node and we cannot really
    %% know what nodes these new nodes are synced to. The operator can always 
    %% manually force a sync of the nodes after all nodes being uppgraded.
    %% We must disconnect also if some nodes to which we have a connection
    %% will not be in any global group at all.
    force_nodedown(nodes(connected) -- NewNodesList, Conns),

    NewNodes = lists:foldl(fun (N, Nacc) ->
                                   NS = maps:get(N, OldNodes, no_contact),
                                   Nacc#{N => NS}
                           end, #{}, NewNodesList),

    %% Schedule group consistency check due to config change. The response is
    %% later handled in handle_erpc_response()...
    NewReqs = schedule_conf_changed_checks(NewNodesList, Reqs, Conns),

    NewS = S#state{group_name = NewGroupName,
		   nodes = NewNodes,
		   other_grps = NewOther,
		   group_publish_type = PubTpGrp,
                   erpc_requests = NewReqs,
                   config_check = undefined},
    {reply, ok, NewS};

%%%====================================================================================
%%% global_groups parameter added
%%% The node is not resynced automatically because it would cause this node to
%%% be disconnected from those nodes not yet been upgraded.
%%%====================================================================================
handle_call({global_groups_added, NewPara}, _From,
            #state{connections = Conns,
                   erpc_requests = Reqs} = S) ->
%    io:format("### global_groups_changed, NewPara ~p ~n",[NewPara]),

    #gconf{group_name = NewGroupName,
           group_publish_type = PubTpGrp,
           group_list = NewNodesList,
           other_groups = NewOther,
           state = GState} = new_group_conf(true, NewPara),

    case GState of
        no_conf ->
            exit({error, 'no global_groups definiton'});
        {error, _Err, NodeGrps} ->
            exit({error, {'invalid global_groups definition', NodeGrps}});
        _ ->
            ok
    end,

    %% disconnect from those nodes which are not going to be in our global group
    force_nodedown(nodes(connected) -- NewNodesList, Conns),

    %% Check which nodes are already updated
    NGACArgs = [node(), PubTpGrp, NewNodesList],

    %% Schedule ng_add_check on all nodes in our configuration and build up
    %% the nodes map. The responses to the ng_add_check are later handled in
    %% handle_erpc_response(). 
    {NewReqs, NewNodes}
        = lists:foldl(
            fun (N, {Racc, Nacc}) ->
                    CId = maps:get(N, Conns, not_connected),
                    NRacc = erpc:send_request(N, ?MODULE, ng_add_check,
                                              NGACArgs,
                                              {ng_add_check, N, CId},
                                              Racc),
                    What = if CId == not_connected -> no_contact;
                              true -> sync_error
                           end,
                    {NRacc, Nacc#{N => What}}
            end,
            {Reqs, #{}}, lists:delete(node(), NewNodesList)),

    NewS = S#state{sync_state = synced, group_name = NewGroupName,
                   nodes = NewNodes, erpc_requests = NewReqs,
                   other_grps = NewOther, group_publish_type = PubTpGrp,
                   config_check = undefined},
    {reply, ok, NewS};


%%%====================================================================================
%%% global_groups parameter removed
%%%====================================================================================
handle_call({global_groups_removed, _NewPara}, _From, S) ->
%    io:format("### global_groups_removed, NewPara ~p ~n",[_NewPara]),

    #gconf{group_name = NewGroupName,
           group_publish_type = PubTpGrp,
           group_list = _NewNodes,
           other_groups = NewOther,
           state = no_conf} = new_group_conf(true, undefined),

    NewS = S#state{sync_state = no_conf, group_name = NewGroupName, nodes = #{}, 
		   other_grps = NewOther, group_publish_type = PubTpGrp,
                   config_check = undefined},
    {reply, ok, NewS};


%%%====================================================================================
%%% global_groups parameter added to some other node which thinks that we
%%% belong to the same global group.
%%% It could happen that our node is not yet updated with the new node_group parameter
%%%====================================================================================
handle_call({ng_add_check, Node, PubType, OthersNG}, _From,
            #state{group_publish_type = OwnPubType} = S) ->
    %% Check which nodes are already updated
    OwnNodes = get_own_nodes(true),
    case {PubType, lists:sort(OthersNG)} of
        {OwnPubType, OwnNodes} ->
            {reply, agreed, node_state(sync, Node, S)};
        _ ->
            {reply, not_agreed, node_state(sync_error, Node, S)}
    end;

%%%====================================================================================
%%% Misceleaneous help function to read some variables
%%%====================================================================================
handle_call(info, _From, S) ->
    {InSync, SyncError, NoContact}
        = maps:fold(fun (N, sync, {ISacc, SEacc, NCacc}) ->
                            {[N|ISacc], SEacc, NCacc};
                        (N, sync_error, {ISacc, SEacc, NCacc}) ->
                            {ISacc, [N|SEacc], NCacc};
                        (N, no_contact, {ISacc, SEacc, NCacc}) ->
                            {ISacc, SEacc, [N|NCacc]}
                    end,
                    {[],[],[]},
                    S#state.nodes),
    Reply = [{state,          S#state.sync_state},
	     {own_group_name, S#state.group_name},
	     {own_group_nodes, get_own_nodes(true)},
	     {synced_nodes,   lists:sort(InSync)},
	     {sync_error,     lists:sort(SyncError)},
	     {no_contact,     lists:sort(NoContact)},
	     {other_groups,   S#state.other_grps},
	     {monitoring,     S#state.monitor}],

    {reply, Reply, S};

handle_call(get, _From, S) ->
    {reply, get(), S};


%%%====================================================================================
%%% Only for test suites. These tests when the search process exits.
%%%====================================================================================
handle_call({registered_names_test, {node, 'test3844zty'}}, From, S) ->
    Pid = global_search:start(names_test, {node, 'test3844zty'}),
    Wait = get(registered_names),
    put(registered_names, [{Pid, From} | Wait]),
    {noreply, S};
handle_call({registered_names_test, {node, _Node}}, _From, S) ->
    {reply, {error, illegal_function_call}, S};
handle_call({send_test, Name, 'test3844zty'}, From, S) ->
    Pid = global_search:start(send_test, 'test3844zty'),
    Wait = get(send),
    put(send, [{Pid, From, Name, 'test3844zty'} | Wait]),
    {noreply, S};
handle_call({send_test, _Name, _Msg }, _From, S) ->
    {reply, {error, illegal_function_call}, S};
handle_call({whereis_name_test, 'test3844zty'}, From, S) ->
    Pid = global_search:start(whereis_test, 'test3844zty'),
    Wait = get(whereis_name),
    put(whereis_name, [{Pid, From} | Wait]),
    {noreply, S};
handle_call({whereis_name_test, _Name}, _From, S) ->
    {reply, {error, illegal_function_call}, S};

handle_call(Call, _From, S) ->
%    io:format("***** handle_call ~p~n",[Call]),
    {reply, {illegal_message, Call}, S}.
    




%%%====================================================================================
%%% registered_names({node, Node}) -> [Name] | {error, ErrorMessage}
%%% registered_names({group, GlobalGroupName}) -> [Name] | {error, ErrorMessage}
%%%
%%% Get a list of nodes in the own global group
%%%====================================================================================
-doc false.
handle_cast({registered_names, User}, S) ->
%    io:format(">>>>> registered_names User ~p~n",[User]),
    Res = global:registered_names(),
    User ! {registered_names_res, Res},
    {noreply, S};

handle_cast({registered_names_res, Result, Pid, From}, S) ->
%    io:format(">>>>> registered_names_res Result ~p~n",[Result]),
    unlink(Pid),
    Pid ! kill,
    Wait = get(registered_names),
    NewWait = lists:delete({Pid, From},Wait),
    put(registered_names, NewWait),
    gen_server:reply(From, Result),
    {noreply, S};



%%%====================================================================================
%%% send(Name, Msg) -> Pid | {error, ErrorMessage}
%%% send({node, Node}, Name, Msg) -> Pid | {error, ErrorMessage}
%%% send({group, GlobalGroupName}, Name, Msg) -> Pid | {error, ErrorMessage}
%%%
%%% The registered Name is found; send the message to it, kill the search process,
%%% and return to the requesting process.
%%%====================================================================================
handle_cast({send_res, Result, Name, Msg, Pid, From}, S) ->
%    io:format("~p>>>>> send_res Result ~p~n",[node(), Result]),
    case Result of
	{badarg,{Name, Msg}} ->
	    continue;
	ToPid ->
	    ToPid ! Msg
    end,
    unlink(Pid),
    Pid ! kill,
    Wait = get(send),
    NewWait = lists:delete({Pid, From, Name, Msg},Wait),
    put(send, NewWait),
    gen_server:reply(From, Result),
    {noreply, S};



%%%====================================================================================
%%% A request from a search process to check if this Name is registered at this node.
%%%====================================================================================
handle_cast({find_name, User, Name}, S) ->
    Res = global:whereis_name(Name),
%    io:format(">>>>> find_name Name ~p   Res ~p~n",[Name, Res]),
    User ! {find_name_res, Res},
    {noreply, S};

%%%====================================================================================
%%% whereis_name(Name) -> Pid | undefined
%%% whereis_name({node, Node}, Name) -> Pid | undefined
%%% whereis_name({group, GlobalGroupName}, Name) -> Pid | undefined
%%%
%%% The registered Name is found; kill the search process
%%% and return to the requesting process.
%%%====================================================================================
handle_cast({find_name_res, Result, Pid, From}, S) ->
%    io:format(">>>>> find_name_res Result ~p~n",[Result]),
%    io:format(">>>>> find_name_res get() ~p~n",[get()]),
    unlink(Pid),
    Pid ! kill,
    Wait = get(whereis_name),
    NewWait = lists:delete({Pid, From},Wait),
    put(whereis_name, NewWait),
    gen_server:reply(From, Result),
    {noreply, S};

%%%====================================================================================
%%% Another node is checking this node's group configuration
%%%====================================================================================
handle_cast({conf_check, Vsn, Node, From, sync, CCName, CCNodes}, S) ->
    handle_cast({conf_check, Vsn, Node, From, sync, CCName, normal, CCNodes}, S);
    
handle_cast({conf_check, Vsn, Node, From, sync, CCName, PubType, CCNodes},
            #state{connections = Conns} = S) ->
%    io:format(">>>>> conf_check,sync  Node ~p~n",[Node]),
    %% Another node is syncing, 
    %% done for instance after upgrade of global_groups parameter
    try
        CId = case maps:get(Node, Conns, undefined) of
                  undefined ->
                      %% We got garbage from someone...
                      throw({noreply, S});
                  CId0 ->
                      CId0
              end,
        To = if is_integer(Vsn) andalso Vsn >= 3 ->
                     case From of
                         {Pid, _Session} when is_pid(Pid) ->
                             %% New node that does not use the
                             %% global_group_check process...
                             Pid;
                         _Garbage ->
                             %% We got garbage from someone...
                             throw({noreply, S})
                     end;
                true ->
                     %% Old node that still use the
                     %% global_group_check process...
                     {global_group_check, Node}
             end,
        case lookup_group_conf(true) of
            #gconf{state = no_conf} ->
                %% We didn't have any node_group definition
                disconnect_nodes([Node], Conns),
                To ! {config_error, Vsn, From, node()},
                {noreply, S};

            #gconf{state = {error, _Err, _NodeGrps}} ->
                disconnect_nodes([Node], Conns),
                To ! {config_error, Vsn, From, node()},
                {noreply, node_state(remove, Node, S)};

            #gconf{group_name = CCName,
                   group_list = CCNodes,
                   group_publish_type = PubType} ->
                %% OK, change the state of the node to 'sync'
                global_name_server ! {group_nodeup, Node, CId},
                To ! {config_ok, Vsn, From, node()},
                {noreply, node_state(sync, Node, S)};

            #gconf{} ->
                %% group definitions were not in agreement
                disconnect_nodes([Node], Conns),
                To ! {config_error, Vsn, From, node()},
                {noreply, node_state(sync_error, Node, S)}
        end
    catch
        throw:{noreply, _} = Return -> Return
    end;

handle_cast(_Cast, S) ->
%    io:format("***** handle_cast ~p~n",[_Cast]),
    {noreply, S}.

-doc false.
handle_info(Msg, #state{erpc_requests = Requests} = S) ->
    try erpc:check_response(Msg, Requests, true) of
        NoMatch when NoMatch == no_request; NoMatch == no_response ->
            continue_handle_info(Msg, S);
        {{response, Result}, Label, NewRequests} ->
            {noreply,
             handle_erpc_response(ok, Result, Label,
                                  S#state{erpc_requests = NewRequests})}
    catch
        Class:{Reason, Label, NewRequests} ->
            {noreply,
             handle_erpc_response(Class, Reason, Label,
                                  S#state{erpc_requests = NewRequests})}
    end.

%%%====================================================================================
%%% Distribution on this node was started...
%%%====================================================================================
continue_handle_info({nodeup, Node, #{connection_id := undefined}},
                     #state{connections = Conns, erpc_requests = Reqs}) ->
    %% Check configuration since we now know how to interpret it
    %% w<hen we know our name...
    S = initial_group_setup(alive_state_change_group_conf(Node), Conns, Reqs),
    send_monitor(S#state.monitor, {nodeup, Node}, S#state.sync_state),
    {noreply, S};

%%%====================================================================================
%%% A new node connected...
%%%====================================================================================
continue_handle_info({nodeup, Node, #{connection_id := CId}},
                     #state{sync_state = no_conf,
                            connections = Conns} = S) ->
%    io:format("~p>>>>> nodeup, Node ~p ~n",[node(), Node]),
    send_monitor(S#state.monitor, {nodeup, Node}, S#state.sync_state),
    {noreply, S#state{connections = Conns#{Node => CId}}};
continue_handle_info({nodeup, Node, #{connection_id := CId}},
                     #state{erpc_requests = Reqs, connections = Conns} = S) ->
%    io:format("~p>>>>> nodeup, Node ~p ~n",[node(), Node]),

    NewConns = Conns#{Node => CId},
    case member(true, Node) of
        false ->
            %% Node is not part of our group configuration...
            disconnect_nodes([Node], NewConns),
            {noreply, S#state{connections = NewConns}};
        true ->
                    
            %% Node is part of our group configuration. Check that it has the
            %% same view of the configuration as us...

            NewReqs = erpc:send_request(Node, global_group, get_own_nodes, [],
                                        {nodeup_conf_check, Node, CId}, Reqs),

            %% The response is later handled in handle_erpc_response()...

            %% Set the node in 'sync_error' state. It will later be changed
            %% to 'sync' if it passes the 'nodeup_conf_check' test...
            {noreply, node_state(sync_error, Node,
                                 S#state{erpc_requests = NewReqs,
                                         connections = NewConns})}
    end;

%%%====================================================================================
%%% Distribution on this node was shut down...
%%%====================================================================================
continue_handle_info({nodedown, Node, #{connection_id := undefined}}, S) ->
    %% Clear group configuration. We don't know how to interpret it
    %% unless we know our own name...
    #gconf{state = no_conf,
           group_name = DefGroupName,
           group_list = _DefNodes,
           group_publish_type = PubTpGrp,
           other_groups = DefOther} = alive_state_change_group_conf(nonode@nohost),
    send_monitor(S#state.monitor, {nodedown, Node}, no_conf),
    {noreply, S#state{group_publish_type = PubTpGrp,
                      sync_state = no_conf, group_name = DefGroupName,
                      nodes = #{}, other_grps = DefOther,
                      config_check = undefined}};

%%%====================================================================================
%%% A node has crashed. 
%%%====================================================================================
continue_handle_info({nodedown, Node, _Info},
                     #state{sync_state = no_conf,
                            monitor = Monitor,
                            connections = Conns} = S) ->
%    io:format("~p>>>>> nodedown, no_conf Node ~p~n",[node(), Node]),
    send_monitor(Monitor, {nodedown, Node}, no_conf),
    {noreply, S#state{connections = maps:remove(Node, Conns)}};
continue_handle_info({nodedown, Node, _Info},
                     #state{sync_state = SyncState,
                            monitor = Monitor,
                            connections = Conns} = S) ->
%    io:format("~p>>>>> nodedown, Node ~p  ~n",[node(), Node]),
    send_monitor(Monitor, {nodedown, Node}, SyncState),
    {noreply, node_state(no_contact, Node,
                         S#state{connections = maps:remove(Node, Conns)})};


%%%====================================================================================
%%% A node has changed its global_groups definition, and is telling us that we are not
%%% included in his group any more. This could happen at release upgrade.
%%%====================================================================================
continue_handle_info({disconnect_node, Node}, #state{monitor = Monitor,
                                                     sync_state = SyncState,
                                                     nodes = Nodes,
                                                     connections = Conns} = S) ->
%    io:format("~p>>>>> disconnect_node Node ~p CN ~p~n",[node(), Node, S#state.nodes]),
    case {SyncState, maps:get(Node, Nodes, not_member)} of
	{synced, sync} -> send_monitor(Monitor, {nodedown, Node}, SyncState);
	_ -> ok
    end,
    CId = maps:get(Node, Conns, not_connected),
    global_name_server ! {group_nodedown, Node, CId},
    {noreply, node_state(sync_error, Node, S)};

continue_handle_info({config_ok, ?cc_vsn, {Pid, CCSession}, Node},
                     #state{config_check = {CCSession, Mons},
                            connections = Conns} = S0) when Pid == self() ->
    try
        {Mon, NewMons} = case maps:take(Node, Mons) of
                             error -> throw({noreply, S0});
                             MonTake -> MonTake
                         end,
        erlang:demonitor(Mon),
        S1 = if map_size(NewMons) == 0 ->
                     S0#state{config_check = undefined};
                true ->
                     S0#state{config_check = {CCSession, NewMons}}
             end,
        CId = case maps:get(Node, Conns, undefined) of
                  undefined -> throw({noreply, S1});
                  CId0 -> CId0
              end,
        global_name_server ! {group_nodeup, Node, CId},
        {noreply, node_state(sync, Node, S0)}
    catch
        throw:{noreply, _} = Return -> Return
    end;

continue_handle_info({config_error, ?cc_vsn, {Pid, CCSession}, Node},
                     #state{config_check = {CCSession, Mons},
                            connections = Conns} = S0) when Pid == self() ->
    try
        {Mon, NewMons} = case maps:take(Node, Mons) of
                             error -> throw({noreply, S0});
                             MonTake -> MonTake
                         end,
        erlang:demonitor(Mon),
        S1 = if map_size(NewMons) == 0 ->
                     S0#state{config_check = undefined};
                true ->
                     S0#state{config_check = {CCSession, NewMons}}
             end,
        CId = maps:get(Node, Conns, not_connected),
        global_name_server ! {group_nodedown, Node, CId},
        log_sync_error(Node),
        {noreply, node_state(sync_error, Node, S1)}
    catch
        throw:{noreply, _} = Return -> Return
    end;

continue_handle_info({'DOWN', Mon, process, {global_group, Node}, Reason},
                     #state{config_check = {CCSession, Mons},
                            connections = Conns} = S0) ->
    %% This clause needs to be the last one matching on 'DOWN'
    %% messages...
    try
        NewMons = case maps:take(Node, Mons) of
                      {Mon, NewMons0} -> NewMons0;
                      _ -> throw({noreply, S0})
                  end,
        S1 = if map_size(NewMons) == 0 ->
                     S0#state{config_check = undefined};
                true ->
                     S0#state{config_check = {CCSession, NewMons}}
             end,
        CId = maps:get(Node, Conns, not_connected),
        global_name_server ! {group_nodedown, Node, CId},
        What = if Reason == noconnection ->
                       no_contact;
                  true ->
                       log_sync_error(Node),
                       sync_error
               end,
        {noreply, node_state(What, Node, S1)}
    catch
        throw:{noreply, _} = Return -> Return
    end;

continue_handle_info({'EXIT', ExitPid, Reason}, S) ->
    check_exit(ExitPid, Reason),
    {noreply, S};


continue_handle_info(_Info, S) ->
%    io:format("***** handle_info = ~p~n",[_Info]),
    {noreply, S}.



-doc false.
terminate(_Reason, _S) ->
    ok.
    

-doc false.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

log_sync_error(Node) ->
    Txt = io_lib:format("global_group: Could not synchronize with node ~p~n"
                        "because global_groups parameter were not in agreement.~n",
                        [Node]),
    error_logger:error_report(Txt),
    ok.

%%%====================================================================================
%%% Schedule group consistency check due to config change. The response is
%%% later handled in handle_erpc_response()...
%%%====================================================================================

schedule_conf_changed_checks(Nodes, Requests, Connections) ->
    lists:foldl(fun (Node, RequestsAcc) ->
                        CId = maps:get(Node, Connections, not_connected),
                        erpc:send_request(Node, global_group, get_own_nodes, [],
                                          {conf_changed_check, Node, CId},
                                          RequestsAcc)
                end, Requests, Nodes).

%%%====================================================================================
%%% We got a response to an asynchronous erpc request
%%%====================================================================================

handle_erpc_response(ok, Nodes, {nodeup_conf_check, Node, ReqCId},
                     #state{connections = Conns} = S) when is_list(Nodes) ->
    case maps:get(Node, Conns, undefined) of
        CId when ReqCId == CId orelse (ReqCId == not_connected
                                       andalso is_integer(CId)) ->
            OwnNodes = get_own_nodes(true),

            case lists:sort(Nodes) of
                OwnNodes -> %% Node has the same group config as us...
                    send_monitor(S#state.monitor, {nodeup, Node}, S#state.sync_state),
                    global_name_server ! {group_nodeup, Node, CId},
                    node_state(sync, Node, S);

                _ -> %% Node has not the same group config but is in our config...
                    disconnect_nodes([Node], Conns),
                    node_state(sync_error, Node, S)
            end;
        _ ->
            %% Connection has gone down since we issued the request;
            %% no action taken...
            S
    end;
handle_erpc_response(error, {erpc,noconnection},
                     {nodeup_conf_check, Node, ReqCId},
                     #state{connections = Conns} = S) ->
    case maps:get(Node, Conns, undefined) of
        CId when ReqCId == CId orelse (ReqCId == not_connected
                                       andalso is_integer(CId)) ->
            node_state(no_contact, Node, S);
        _ ->
            %% Connection has gone down since we issued the request;
            %% no action taken...
            S
    end;
handle_erpc_response(_, _, {nodeup_conf_check, Node, ReqCId},
                     #state{connections = Conns} = S) ->
    case maps:get(Node, Conns, undefined) of
        CId when ReqCId == CId orelse (ReqCId == not_connected
                                       andalso is_integer(CId)) ->
            disconnect_nodes([Node], Conns),
            node_state(sync_error, Node, S);
        _ ->
            %% Connection has gone down since we issued the request;
            %% no action taken...
            S
    end;
handle_erpc_response(ok, Nodes, {conf_changed_check, Node, ReqCId},
                     #state{connections = Conns} = S) when is_list(Nodes) ->
    case maps:get(Node, Conns, undefined) of
        CId when ReqCId == CId orelse (ReqCId == not_connected
                                       andalso is_integer(CId)) ->
            OwnNodes = get_own_nodes(true),

            case lists:sort(Nodes) of
                OwnNodes -> %% Node has the same group config as us...
                    node_state(sync, Node, S);

                _ -> %% Node has not the same group config but is in our config...
                    disconnect_nodes([Node], Conns),
                    node_state(sync_error, Node, S)
            end;
        _ ->
            %% Connection has gone down since we issued the request;
            %% no action taken...
            S
    end;
handle_erpc_response(error, {erpc,noconnection},
                     {conf_changed_check, Node, ReqCId},
                     #state{connections = Conns} = S) ->
    case maps:get(Node, Conns, undefined) of
        CId when ReqCId == CId orelse (ReqCId == not_connected
                                       andalso is_integer(CId)) ->
            node_state(no_contact, Node, S);
        _ ->
            %% Connection has gone down since we issued the request;
            %% no action taken...
            S
    end;
handle_erpc_response(_, _, {conf_changed_check, Node, ReqCId},
                     #state{connections = Conns} = S) ->
    case maps:get(Node, Conns, undefined) of
        CId when ReqCId == CId orelse (ReqCId == not_connected
                                       andalso is_integer(CId)) ->
            disconnect_nodes([Node], Conns),
            node_state(sync_error, Node, S);
        _ ->
            %% Connection has gone down since we issued the request;
            %% no action taken...
            S
    end;
handle_erpc_response(ok, agreed, {ng_add_check, Node, ReqCId},
                     #state{connections = Conns} = S) ->
    case maps:get(Node, Conns, undefined) of
        CId when ReqCId == CId orelse (ReqCId == not_connected
                                       andalso is_integer(CId)) ->
            node_state(sync, Node, S);
        _ ->
            %% Connection has gone down since we issued the request;
            %% no action taken...
            S
    end;
handle_erpc_response(error, {erpc,noconnection}, {ng_add_check, Node, ReqCId},
                     #state{connections = Conns} = S) ->
    case maps:get(Node, Conns, undefined) of
        CId when ReqCId == CId orelse (ReqCId == not_connected
                                       andalso is_integer(CId)) ->
            node_state(no_contact, Node, S);
        _ ->
            %% Connection has gone down since we issued the request;
            %% no action taken...
            S
    end;
handle_erpc_response(_, _, {ng_add_check, Node, ReqCId},
                     #state{connections = Conns} = S) ->
    case maps:get(Node, Conns, undefined) of
        CId when ReqCId == CId orelse (ReqCId == not_connected
                                       andalso is_integer(CId)) ->
            disconnect_nodes([Node], Conns),
            node_state(sync_error, Node, S);
        _ ->
            %% Connection has gone down since we issued the request;
            %% no action taken...
            S
    end.


%%%====================================================================================
%%% Change state of node
%%%====================================================================================

node_state(What, Node, #state{nodes = Ns} = S)
  when What == sync; What == sync_error; What == no_contact ->
    case member(true, Node) of
        true -> S#state{nodes = Ns#{Node => What}};
        false -> S#state{nodes = maps:remove(Node, Ns)}
    end;
node_state(remove, Node, #state{nodes = Ns} = S) ->
    case member(true, Node) of
        true -> error({removing_node_state_of_member_node, Node});
        false -> S#state{nodes = maps:remove(Node, Ns)}
    end.

%%%====================================================================================
%%% Check the global group configuration.
%%%====================================================================================

config_scan(MyNode, NodeGrps) ->
    config_scan(MyNode, normal, NodeGrps, no_name, [], []).

config_scan(_MyNode, PubType, [], Own_name, OwnNodes, OtherNodeGrps) ->
    {Own_name, PubType, lists:sort(OwnNodes), lists:reverse(OtherNodeGrps)};
config_scan(MyNode, PubType, [GrpTuple|NodeGrps], Own_name, OwnNodes, OtherNodeGrps) ->
    {Name, PubTypeGroup, Nodes} = grp_tuple(GrpTuple),
    case lists:member(MyNode, Nodes) of
	true ->
	    case Own_name of
		no_name ->
		    config_scan(MyNode, PubTypeGroup, NodeGrps, Name, Nodes, OtherNodeGrps);
		_ ->
		    {error, {'node defined twice', {Own_name, Name}}}
	    end;
	false ->
	    config_scan(MyNode,PubType,NodeGrps,Own_name,OwnNodes,
			[{Name, Nodes}|OtherNodeGrps])
    end.

grp_tuple({Name, Nodes}) ->
    {Name, normal, Nodes};
grp_tuple({Name, hidden, Nodes}) ->
    {Name, hidden, Nodes};
grp_tuple({Name, normal, Nodes}) ->
    {Name, normal, Nodes}.

%%%====================================================================================
%%% Get/set configuration
%%%====================================================================================

%%
%% Fetch and install group configuration...
%%
fetch_new_group_conf(GG) ->
    fetch_new_group_conf(GG, undefined).

fetch_new_group_conf(GG, NodeName) ->
    GGConf = case application:get_env(kernel, global_groups) of
                 undefined -> undefined;
                 {ok, V} -> V
             end,
    new_group_conf(GG, GGConf, NodeName).

%%
%% Install new group configuration...
%%
new_group_conf(GG, KernParamValue) ->
    new_group_conf(GG, KernParamValue, undefined).

new_group_conf(GG, KernParamValue, NodeName) ->
    case persistent_term:get(?MODULE, #gconf{}) of
        #gconf{parameter_value = KernParamValue,
               node_name = Name} = GConf when NodeName == Name;
                                              NodeName == undefined ->
            GConf;
        #gconf{node_name = Name} ->
            UseNodeName = if NodeName == undefined -> Name;
                             true -> NodeName
                          end,
            GConf = make_group_conf(UseNodeName, KernParamValue),
            %% Only save in persistent term if called by
            %% the global_group server...
            if GG == true -> persistent_term:put(?MODULE, GConf);
               true -> ok
            end,
            GConf
    end.

make_group_conf(NodeName, KernParamValue) when KernParamValue == undefined;
                                               KernParamValue == [];
                                               NodeName == nonode@nohost ->
    %% Empty group configuration if it is not defined, or if we are not
    %% alive (if we are not alive we cannot interpret the configuration
    %% since we don't know our own node name)...
    #gconf{parameter_value = KernParamValue,
           node_name = NodeName};
make_group_conf(NodeName, KernParamValue) ->
    case catch config_scan(NodeName, KernParamValue) of

        {error, Error} ->
            #gconf{parameter_value = KernParamValue,
                   node_name = NodeName,
                   state = {error, Error, KernParamValue}};

        {GName, PubTpGrp, OwnNodes, OtherGroups} ->
            GMap = if OwnNodes == [] ->
                           all;
                      true ->
                           #{Node => ok || Node <- OwnNodes}
                   end,
            #gconf{parameter_value = KernParamValue,
                   node_name = NodeName,
                   group_name = GName,
                   group_publish_type = PubTpGrp,
                   group_list = lists:sort(OwnNodes),
                   group_map = GMap,
                   other_groups = OtherGroups,
                   state = conf}
    end.

%%
%% Adjust group configuration according to alive state
%%
alive_state_change_group_conf(NodeName) when NodeName /= undefined ->
    case persistent_term:get(?MODULE, #gconf{}) of
        #gconf{parameter_value = ParamValue} when ParamValue /= invalid ->
            new_group_conf(true, ParamValue, NodeName);
        #gconf{} ->
            fetch_new_group_conf(true, NodeName)
    end.

%%
%% Lookup current group configuration
%%

lookup_group_conf(GG) ->
    try
        persistent_term:get(?MODULE)
    catch
        error:badarg -> fetch_new_group_conf(GG)
    end.

%%%====================================================================================
%%% The global_group_check process. It used to take care of syncing other nodes,
%%% but nowadays only serve as a dispatcher config check reply messages from old
%%% global_group servers to our global_group server.
%%%====================================================================================

global_group_check_dispatcher() ->
    receive
        {config_ok, _Vsn, _From, _Node} = Msg ->
            global_group ! Msg, ok;
        {config_error, _Vsn, _From, _Node} = Msg ->
            global_group ! Msg, ok;
        _Garbage ->
            ok
    end,
    global_group_check_dispatcher().


%%%====================================================================================
%%% A process wants to toggle monitoring nodeup/nodedown from nodes.
%%%====================================================================================
monitor_nodes(true, Pid, State) ->
    link(Pid),
    Monitor = State#state.monitor,
    {ok, State#state{monitor = [Pid|Monitor]}};
monitor_nodes(false, Pid, State) ->
    Monitor = State#state.monitor,
    State1 = State#state{monitor = delete_all(Pid,Monitor)},
    do_unlink(Pid, State1),
    {ok, State1};
monitor_nodes(_, _, State) ->
    {error, State}.

delete_all(From, [From |Tail]) -> delete_all(From, Tail);
delete_all(From, [H|Tail]) ->  [H|delete_all(From, Tail)];
delete_all(_, []) -> [].

%% do unlink if we have no more references to Pid.
do_unlink(Pid, State) ->
    case lists:member(Pid, State#state.monitor) of
	true ->
	    false;
	_ ->
%	    io:format("unlink(Pid) ~p~n",[Pid]),
	    unlink(Pid)
    end.



%%%====================================================================================
%%% Send a nodeup/down messages to monitoring Pids in the own global group.
%%%====================================================================================
send_monitor([P|T], M, no_conf) ->
    _ = safesend_nc(P, M),
    send_monitor(T, M, no_conf);
send_monitor([P|T], M, SyncState) ->
    _ = safesend(P, M),
    send_monitor(T, M, SyncState);
send_monitor([], _, _) ->
    ok.

safesend(Name, {Msg, Node}) when is_atom(Name) ->
    case member(true, Node) of
	true ->
	    case whereis(Name) of 
		undefined ->
		    {Msg, Node};
		P when is_pid(P) ->
		    P ! {Msg, Node}
	    end;
	false ->
	    not_own_group
    end;
safesend(Pid, {Msg, Node}) -> 
    case member(true, Node) of
	true ->
	    Pid ! {Msg, Node};
	false ->
	    not_own_group
    end.

safesend_nc(Name, {Msg, Node}) when is_atom(Name) ->
    case whereis(Name) of 
	undefined ->
	    {Msg, Node};
	P when is_pid(P) ->
	    P ! {Msg, Node}
    end;
safesend_nc(Pid, {Msg, Node}) -> 
    Pid ! {Msg, Node}.






%%%====================================================================================
%%% Check which user is associated to the crashed process.
%%%====================================================================================
check_exit(ExitPid, Reason) ->
%    io:format("===EXIT===  ~p ~p ~n~p   ~n~p   ~n~p ~n~n",[ExitPid, Reason, get(registered_names), get(send), get(whereis_name)]),
    check_exit_reg(get(registered_names), ExitPid, Reason),
    check_exit_send(get(send), ExitPid, Reason),
    check_exit_where(get(whereis_name), ExitPid, Reason),
    check_exit_ggc(ExitPid, Reason).


check_exit_reg(undefined, _ExitPid, _Reason) ->
    ok;
check_exit_reg(Reg, ExitPid, Reason) ->
    case lists:keysearch(ExitPid, 1, lists:delete(undefined, Reg)) of
	{value, {ExitPid, From}} ->
	    NewReg = lists:delete({ExitPid, From}, Reg),
	    put(registered_names, NewReg),
	    gen_server:reply(From, {error, Reason});
	false ->
	    not_found_ignored
    end.


check_exit_send(undefined, _ExitPid, _Reason) ->
    ok;
check_exit_send(Send, ExitPid, _Reason) ->
    case lists:keysearch(ExitPid, 1, lists:delete(undefined, Send)) of
	{value, {ExitPid, From, Name, Msg}} ->
	    NewSend = lists:delete({ExitPid, From, Name, Msg}, Send),
	    put(send, NewSend),
	    gen_server:reply(From, {badarg, {Name, Msg}});
	false ->
	    not_found_ignored
    end.


check_exit_where(undefined, _ExitPid, _Reason) ->
    ok;
check_exit_where(Where, ExitPid, Reason) ->
    case lists:keysearch(ExitPid, 1, lists:delete(undefined, Where)) of
	{value, {ExitPid, From}} ->
	    NewWhere = lists:delete({ExitPid, From}, Where),
	    put(whereis_name, NewWhere),
	    gen_server:reply(From, {error, Reason});
	false ->
	    not_found_ignored
    end.

check_exit_ggc(ExitPid, Reason) ->
    case get(global_group_check) of
        ExitPid ->
            %% Our global_group_check companion died; terminate...
            exit(Reason);
        _ ->
            ok
    end.

%%%====================================================================================
%%% Disconnect nodes not belonging to own global_groups
%%%====================================================================================
disconnect_nodes(DisconnectNodes, Conns) ->
    lists:foreach(fun(Node) ->
                          CId = maps:get(Node, Conns, not_connected),
                          global_name_server ! {group_nodedown, Node, CId},
			  {global_group, Node} ! {disconnect_node, node()}
		  end,
		  DisconnectNodes).


%%%====================================================================================
%%% Disconnect nodes not belonging to own global_groups
%%%====================================================================================
force_nodedown(DisconnectNodes, Conns) ->
    lists:foreach(fun(Node) ->
                          CId = maps:get(Node, Conns, not_connected),
                          global_name_server ! {group_nodedown, Node, CId},
			  erlang:disconnect_node(Node)
		  end,
		  DisconnectNodes).


%%%====================================================================================
%%% Get the current global_groups definition
%%%====================================================================================
-doc false.
get_own_nodes_with_errors() ->
    case lookup_group_conf(false) of
        #gconf{state = {error, Error, _NodeGrps}} ->
            {error, Error};
        #gconf{group_list = []} ->
            {ok, all};
        #gconf{group_list = Nodes} ->
            {ok, Nodes}
    end.

-doc false.
get_own_nodes() ->
    get_own_nodes(false).

get_own_nodes(GG) when is_boolean(GG) ->
    get_own_nodes(lookup_group_conf(GG));
get_own_nodes(#gconf{group_list = Nodes}) ->
    Nodes.

%%%====================================================================================
%%% Is a group configured?
%%%====================================================================================

-doc false.
-spec group_configured() -> boolean().

group_configured() ->
    group_configured(lookup_group_conf(false)).

group_configured(GConf) ->
    case GConf of
        #gconf{state = no_conf} ->
            false;
        #gconf{} ->
            true
    end.

%%%====================================================================================
%%% Is node a participant?
%%%
%%% That is, a node is a participant if it either is a member of our configured group,
%%% or there are no group configured (in which case all nodes are participants).
%%%====================================================================================

-doc false.
-spec participant(Node::node()) -> boolean().

participant(Node) ->
    case lookup_group_conf(false) of
        #gconf{group_map = all} ->
            true;
        #gconf{group_map = #{Node := ok}} ->
            true;
        #gconf{} ->
            false
    end.

%%%====================================================================================
%%% Is node member of our configured group?
%%%====================================================================================

-doc false.
-spec member(Node::node()) -> boolean().

member(Node) ->
    member(false, Node).

member(GG, Node) ->
    case lookup_group_conf(GG) of
        #gconf{group_map = #{Node := ok}} ->
            true;
        #gconf{} ->
            false
    end.

%%%====================================================================================
%%% Publish on node?
%%%====================================================================================

-doc false.
-spec publish(OwnPublishType, Node) -> boolean() when
      OwnPublishType :: 'hidden' | 'normal',
      Node :: node().

publish(OwnPublishType, Node) when (OwnPublishType == normal
                                    orelse OwnPublishType == hidden)
                                   andalso is_atom(Node) ->
    case lookup_group_conf(false) of
        #gconf{group_map = all} when OwnPublishType == normal ->
            true;
        #gconf{group_map = all} when OwnPublishType == hidden ->
            false;
        #gconf{group_publish_type = normal} when OwnPublishType == normal ->
            true;
        #gconf{group_map = #{Node := ok}} ->
            true;
        #gconf{} ->
            false
    end.
