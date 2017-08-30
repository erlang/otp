%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2015. All Rights Reserved.
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
-export([global_groups_changed/1]).
-export([global_groups_added/1]).
-export([global_groups_removed/1]).
-export([sync/0]).
-export([ng_add_check/2, ng_add_check/3]).

-export([info/0]).
-export([registered_names_test/1]).
-export([send_test/2]).
-export([whereis_name_test/1]).
-export([get_own_nodes/0, get_own_nodes_with_errors/0]).
-export([publish_on_nodes/0]).

-export([config_scan/1, config_scan/2]).

%% Internal exports
-export([sync_init/4]).


-define(cc_vsn, 2).

%%%====================================================================================

-type publish_type() :: 'hidden' | 'normal'.
-type sync_state()   :: 'no_conf' | 'synced'.

-type group_name()  :: atom().
-type group_tuple() :: {GroupName :: group_name(), [node()]}
                     | {GroupName :: group_name(),
                        PublishType :: publish_type(),
                        [node()]}.

%%%====================================================================================
%%% The state of the global_group process
%%% 
%%% sync_state =  no_conf (global_groups not defined, inital state) |
%%%               synced 
%%% group_name =  Own global group name
%%% nodes =       Nodes in the own global group
%%% no_contact =  Nodes which we haven't had contact with yet
%%% sync_error =  Nodes which we haven't had contact with yet
%%% other_grps =  list of other global group names and nodes, [{otherName, [Node]}]
%%% node_name =   Own node 
%%% monitor =     List of Pids requesting nodeup/nodedown
%%%====================================================================================

-record(state, {sync_state = no_conf        :: sync_state(),
		connect_all                 :: boolean(),
		group_name = []             :: group_name() | [],
		nodes = []                  :: [node()],
		no_contact = []             :: [node()],
		sync_error = [],
		other_grps = [], 
		node_name = node()          :: node(),
		monitor = [],
		publish_type = normal       :: publish_type(),
		group_publish_type = normal :: publish_type()}).


%%%====================================================================================
%%% External exported
%%%====================================================================================

-spec global_groups() -> {GroupName, GroupNames} | undefined when
      GroupName :: group_name(),
      GroupNames :: [GroupName].
global_groups() ->
    request(global_groups).

-spec monitor_nodes(Flag) -> 'ok' when
      Flag :: boolean().
monitor_nodes(Flag) -> 
    case Flag of
	true -> request({monitor_nodes, Flag});
	false -> request({monitor_nodes, Flag});
	_ -> {error, not_boolean}
    end.

-spec own_nodes() -> Nodes when
      Nodes :: [Node :: node()].
own_nodes() ->
    request(own_nodes).

-type name()  :: atom().
-type where() :: {'node', node()} | {'group', group_name()}.

-spec registered_names(Where) -> Names when
      Where :: where(),
      Names :: [Name :: name()].
registered_names(Arg) ->
    request({registered_names, Arg}).

-spec send(Name, Msg) -> pid() | {'badarg', {Name, Msg}} when
      Name :: name(),
      Msg :: term().
send(Name, Msg) ->
    request({send, Name, Msg}).

-spec send(Where, Name, Msg) -> pid() | {'badarg', {Name, Msg}} when
      Where :: where(),
      Name :: name(),
      Msg :: term().
send(Group, Name, Msg) ->
    request({send, Group, Name, Msg}).

-spec whereis_name(Name) -> pid() | 'undefined' when
      Name :: name().
whereis_name(Name) ->
    request({whereis_name, Name}).

-spec whereis_name(Where, Name) -> pid() | 'undefined' when
      Where :: where(),
      Name :: name().
whereis_name(Group, Name) ->
    request({whereis_name, Group, Name}).

global_groups_changed(NewPara) ->
    request({global_groups_changed, NewPara}).

global_groups_added(NewPara) ->
    request({global_groups_added, NewPara}).

global_groups_removed(NewPara) ->
    request({global_groups_removed, NewPara}).

-spec sync() -> 'ok'.
sync() ->
    request(sync).

ng_add_check(Node, OthersNG) ->
    ng_add_check(Node, normal, OthersNG).

ng_add_check(Node, PubType, OthersNG) ->
    request({ng_add_check, Node, PubType, OthersNG}).

-type info_item() :: {'state', State :: sync_state()}
                   | {'own_group_name', GroupName :: group_name()}
                   | {'own_group_nodes', Nodes :: [node()]}
                   | {'synched_nodes', Nodes :: [node()]}
                   | {'sync_error', Nodes :: [node()]}
                   | {'no_contact', Nodes :: [node()]}
                   | {'other_groups', Groups :: [group_tuple()]}
                   | {'monitoring', Pids :: [pid()]}.

-spec info() -> [info_item()].
info() ->
    request(info, 3000).

%% ==== ONLY for test suites ====
registered_names_test(Arg) ->
    request({registered_names_test, Arg}).
send_test(Name, Msg) ->
    request({send_test, Name, Msg}).
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
%%% If the nodes are not in agreement of the configuration the global_group process 
%%% will remove these nodes from the #state.nodes list. This can be a normal case
%%% at release upgrade when all nodes are not yet upgraded.
%%%
%%% It is possible to manually force a sync of the global_group. This is done for 
%%% instance after a release upgrade, after all nodes in the group beeing upgraded.
%%% The nodes are not synced automatically because it would cause the node to be
%%% disconnected from those not yet beeing upgraded.
%%%
%%% The three process dictionary variables (registered_names, send, and whereis_name) 
%%% are used to store information needed if the search process crashes. 
%%% The search process is a help process to find registered names in the system.
%%%====================================================================================
start() -> gen_server:start({local, global_group}, global_group, [], []).
start_link() -> gen_server:start_link({local, global_group}, global_group,[],[]).
stop() -> gen_server:call(global_group, stop, infinity).

init([]) ->
    process_flag(priority, max),
    ok = net_kernel:monitor_nodes(true),
    put(registered_names, [undefined]),
    put(send, [undefined]),
    put(whereis_name, [undefined]),
    process_flag(trap_exit, true),
    Ca = case init:get_argument(connect_all) of
	     {ok, [["false"]]} ->
		 false;
	     _ ->
		 true
	 end,
    PT = publish_arg(),
    case application:get_env(kernel, global_groups) of
	undefined ->
	    update_publish_nodes(PT),
	    {ok, #state{publish_type = PT,
			connect_all = Ca}};
	{ok, []} ->
	    update_publish_nodes(PT),
	    {ok, #state{publish_type = PT,
			connect_all = Ca}};
	{ok, NodeGrps} ->
	    {DefGroupName, PubTpGrp, DefNodes, DefOther} = 
		case catch config_scan(NodeGrps, publish_type) of
		    {error, _Error2} ->
			update_publish_nodes(PT),
			exit({error, {'invalid global_groups definition', NodeGrps}});
		    {DefGroupNameT, PubType, DefNodesT, DefOtherT} ->
			update_publish_nodes(PT, {PubType, DefNodesT}),
			%% First disconnect any nodes not belonging to our own group
			disconnect_nodes(nodes(connected) -- DefNodesT),
			lists:foreach(fun(Node) ->
					      erlang:monitor_node(Node, true)
				      end,
				      DefNodesT),
			{DefGroupNameT, PubType, lists:delete(node(), DefNodesT), DefOtherT}
		end,
	    {ok, #state{publish_type = PT, group_publish_type = PubTpGrp,
			sync_state = synced, group_name = DefGroupName, 
			no_contact = lists:sort(DefNodes), 
			other_grps = DefOther, connect_all = Ca}}
    end.


%%%====================================================================================
%%% sync() -> ok 
%%%
%%% An operator ordered sync of the own global group. This must be done after
%%% a release upgrade. It can also be ordered if somthing has made the nodes
%%% to disagree of the global_groups definition.
%%%====================================================================================
handle_call(sync, _From, S) ->
%    io:format("~p sync ~p~n",[node(), application:get_env(kernel, global_groups)]),
    case application:get_env(kernel, global_groups) of
	undefined ->
	    update_publish_nodes(S#state.publish_type),
	    {reply, ok, S};
	{ok, []} ->
	    update_publish_nodes(S#state.publish_type),
	    {reply, ok, S};
	{ok, NodeGrps} ->
	    {DefGroupName, PubTpGrp, DefNodes, DefOther} = 
		case catch config_scan(NodeGrps, publish_type) of
		    {error, _Error2} ->
			exit({error, {'invalid global_groups definition', NodeGrps}});
		    {DefGroupNameT, PubType, DefNodesT, DefOtherT} ->
			update_publish_nodes(S#state.publish_type, {PubType, DefNodesT}),
			%% First inform global on all nodes not belonging to our own group
			disconnect_nodes(nodes(connected) -- DefNodesT),
			%% Sync with the nodes in the own group
			kill_global_group_check(),
			Pid = spawn_link(?MODULE, sync_init, 
					 [sync, DefGroupNameT, PubType, DefNodesT]),
			register(global_group_check, Pid),
			{DefGroupNameT, PubType, lists:delete(node(), DefNodesT), DefOtherT}
		end,
	    {reply, ok, S#state{sync_state = synced, group_name = DefGroupName, 
				no_contact = lists:sort(DefNodes), 
				other_grps = DefOther, group_publish_type = PubTpGrp}}
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
		    get_own_nodes()
%		    S#state.nodes
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
handle_call({global_groups_changed, NewPara}, _From, S) ->
    {NewGroupName, PubTpGrp, NewNodes, NewOther} = 
	case catch config_scan(NewPara, publish_type) of
	    {error, _Error2} ->
		exit({error, {'invalid global_groups definition', NewPara}});
	    {DefGroupName, PubType, DefNodes, DefOther} ->
		update_publish_nodes(S#state.publish_type, {PubType, DefNodes}),
		{DefGroupName, PubType, DefNodes, DefOther}
	end,

    %% #state.nodes is the common denominator of previous and new definition
    NN = NewNodes -- (NewNodes -- S#state.nodes),
    %% rest of the nodes in the new definition are marked as not yet contacted
    NNC = (NewNodes -- S#state.nodes) --  S#state.sync_error,
    %% remove sync_error nodes not belonging to the new group
    NSE = NewNodes -- (NewNodes -- S#state.sync_error),

    %% Disconnect the connection to nodes which are not in our old global group.
    %% This is done because if we already are aware of new nodes (to our global
    %% group) global is not going to be synced to these nodes. We disconnect instead
    %% of connect because upgrades can be done node by node and we cannot really
    %% know what nodes these new nodes are synced to. The operator can always 
    %% manually force a sync of the nodes after all nodes beeing uppgraded.
    %% We must disconnect also if some nodes to which we have a connection
    %% will not be in any global group at all.
    force_nodedown(nodes(connected) -- NewNodes),

    NewS = S#state{group_name = NewGroupName, 
		   nodes = lists:sort(NN), 
		   no_contact = lists:sort(lists:delete(node(), NNC)), 
		   sync_error = lists:sort(NSE), 
		   other_grps = NewOther,
		   group_publish_type = PubTpGrp},
    {reply, ok, NewS};


%%%====================================================================================
%%% global_groups parameter added
%%% The node is not resynced automatically because it would cause this node to
%%% be disconnected from those nodes not yet been upgraded.
%%%====================================================================================
handle_call({global_groups_added, NewPara}, _From, S) ->
%    io:format("### global_groups_changed, NewPara ~p ~n",[NewPara]),
    {NewGroupName, PubTpGrp, NewNodes, NewOther} = 
	case catch config_scan(NewPara, publish_type) of
	    {error, _Error2} ->
		exit({error, {'invalid global_groups definition', NewPara}});
	    {DefGroupName, PubType, DefNodes, DefOther} ->
		update_publish_nodes(S#state.publish_type, {PubType, DefNodes}),
		{DefGroupName, PubType, DefNodes, DefOther}
	end,

    %% disconnect from those nodes which are not going to be in our global group
    force_nodedown(nodes(connected) -- NewNodes),

    %% Check which nodes are already updated
    OwnNG = get_own_nodes(),
    NGACArgs = case S#state.group_publish_type of
		   normal ->
		       [node(), OwnNG];
		   _ ->
		       [node(), S#state.group_publish_type, OwnNG]
	       end,
    {NN, NNC, NSE} = 
	lists:foldl(fun(Node, {NN_acc, NNC_acc, NSE_acc}) -> 
			    case rpc:call(Node, global_group, ng_add_check, NGACArgs) of
				{badrpc, _} ->
				    {NN_acc, [Node | NNC_acc], NSE_acc};
				agreed ->
				    {[Node | NN_acc], NNC_acc, NSE_acc};
				not_agreed ->
				    {NN_acc, NNC_acc, [Node | NSE_acc]}
			    end
		    end,
		    {[], [], []}, lists:delete(node(), NewNodes)),
    NewS = S#state{sync_state = synced, group_name = NewGroupName, nodes = lists:sort(NN), 
		   sync_error = lists:sort(NSE), no_contact = lists:sort(NNC), 
		   other_grps = NewOther, group_publish_type = PubTpGrp},
    {reply, ok, NewS};


%%%====================================================================================
%%% global_groups parameter removed
%%%====================================================================================
handle_call({global_groups_removed, _NewPara}, _From, S) ->
%    io:format("### global_groups_removed, NewPara ~p ~n",[_NewPara]),
    update_publish_nodes(S#state.publish_type),
    NewS = S#state{sync_state = no_conf, group_name = [], nodes = [], 
		   sync_error = [], no_contact = [], 
		   other_grps = []},
    {reply, ok, NewS};


%%%====================================================================================
%%% global_groups parameter added to some other node which thinks that we
%%% belong to the same global group.
%%% It could happen that our node is not yet updated with the new node_group parameter
%%%====================================================================================
handle_call({ng_add_check, Node, PubType, OthersNG}, _From, S) ->
    %% Check which nodes are already updated
    OwnNG = get_own_nodes(),
    case S#state.group_publish_type =:= PubType of
	true ->
	    case OwnNG of
		OthersNG ->
		    NN = [Node | S#state.nodes],
		    NSE = lists:delete(Node, S#state.sync_error),
		    NNC = lists:delete(Node, S#state.no_contact),
		    NewS = S#state{nodes = lists:sort(NN), 
				   sync_error = NSE, 
				   no_contact = NNC},
		    {reply, agreed, NewS};
		_ ->
		    {reply, not_agreed, S}
	    end;
	_ ->
	    {reply, not_agreed, S}
    end;



%%%====================================================================================
%%% Misceleaneous help function to read some variables
%%%====================================================================================
handle_call(info, _From, S) ->    
    Reply = [{state,          S#state.sync_state},
	     {own_group_name, S#state.group_name},
	     {own_group_nodes, get_own_nodes()},
%	     {"nodes()",      lists:sort(nodes())},
	     {synced_nodes,   S#state.nodes},
	     {sync_error,     S#state.sync_error},
	     {no_contact,     S#state.no_contact},
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
%%% The node is synced successfully
%%%====================================================================================
handle_cast({synced, NoContact}, S) ->
%    io:format("~p>>>>> synced ~p  ~n",[node(), NoContact]),
    kill_global_group_check(),
    Nodes = get_own_nodes() -- [node() | NoContact],
    {noreply, S#state{nodes = lists:sort(Nodes),
		      sync_error = [],
		      no_contact = NoContact}};    


%%%====================================================================================
%%% The node could not sync with some other nodes.
%%%====================================================================================
handle_cast({sync_error, NoContact, ErrorNodes}, S) ->
%    io:format("~p>>>>> sync_error ~p ~p ~n",[node(), NoContact, ErrorNodes]),
    Txt = io_lib:format("Global group: Could not synchronize with these nodes ~p~n"
			"because global_groups were not in agreement. ~n", [ErrorNodes]),
    error_logger:error_report(Txt),
    kill_global_group_check(),
    Nodes = (get_own_nodes() -- [node() | NoContact]) -- ErrorNodes,
    {noreply, S#state{nodes = lists:sort(Nodes), 
		      sync_error = ErrorNodes,
		      no_contact = NoContact}};


%%%====================================================================================
%%% Another node is checking this node's group configuration
%%%====================================================================================
handle_cast({conf_check, Vsn, Node, From, sync, CCName, CCNodes}, S) ->
    handle_cast({conf_check, Vsn, Node, From, sync, CCName, normal, CCNodes}, S);
    
handle_cast({conf_check, Vsn, Node, From, sync, CCName, PubType, CCNodes}, S) ->
    CurNodes = S#state.nodes,
%    io:format(">>>>> conf_check,sync  Node ~p~n",[Node]),
    %% Another node is syncing, 
    %% done for instance after upgrade of global_groups parameter
    NS = 
	case application:get_env(kernel, global_groups) of
	    undefined ->
		%% We didn't have any node_group definition
		update_publish_nodes(S#state.publish_type),
		disconnect_nodes([Node]),
		{global_group_check, Node} ! {config_error, Vsn, From, node()},
		S;
	    {ok, []} ->
		%% Our node_group definition was empty
		update_publish_nodes(S#state.publish_type),
		disconnect_nodes([Node]),
		{global_group_check, Node} ! {config_error, Vsn, From, node()},
		S;
	    %%---------------------------------
	    %% global_groups defined
	    %%---------------------------------
	    {ok, NodeGrps} ->
		case catch config_scan(NodeGrps, publish_type) of
		    {error, _Error2} ->
			%% Our node_group definition was erroneous
			disconnect_nodes([Node]),
			{global_group_check, Node} ! {config_error, Vsn, From, node()},
			S#state{nodes = lists:delete(Node, CurNodes)};

		    {CCName, PubType, CCNodes, _OtherDef} ->
			%% OK, add the node to the #state.nodes if it isn't there
			update_publish_nodes(S#state.publish_type, {PubType, CCNodes}),
			global_name_server ! {nodeup, Node},
			{global_group_check, Node} ! {config_ok, Vsn, From, node()},
			case lists:member(Node, CurNodes) of
			    false ->
				NewNodes = lists:sort([Node | CurNodes]),
				NSE = lists:delete(Node, S#state.sync_error),
				NNC = lists:delete(Node, S#state.no_contact),
				S#state{nodes = NewNodes, 
				        sync_error = NSE,
				        no_contact = NNC};
			    true ->
				S
			end;
		    _ ->
			%% node_group definitions were not in agreement
			disconnect_nodes([Node]),
			{global_group_check, Node} ! {config_error, Vsn, From, node()},
			NN = lists:delete(Node, S#state.nodes),
			NSE = lists:delete(Node, S#state.sync_error),
			NNC = lists:delete(Node, S#state.no_contact),
			S#state{nodes = NN,
				sync_error = NSE,
				no_contact = NNC}
		end
	end,
    {noreply, NS};


handle_cast(_Cast, S) ->
%    io:format("***** handle_cast ~p~n",[_Cast]),
    {noreply, S}.
    


%%%====================================================================================
%%% A node went down. If no global group configuration inform global;
%%% if global group configuration inform global only if the node is one in
%%% the own global group.
%%%====================================================================================
handle_info({nodeup, Node}, S) when S#state.sync_state =:= no_conf ->
%    io:format("~p>>>>> nodeup, Node ~p ~n",[node(), Node]),
    send_monitor(S#state.monitor, {nodeup, Node}, S#state.sync_state),
    global_name_server ! {nodeup, Node},
    {noreply, S};
handle_info({nodeup, Node}, S) ->
%    io:format("~p>>>>> nodeup, Node ~p ~n",[node(), Node]),
    OthersNG = case S#state.sync_state of
		   synced ->
		       X = (catch rpc:call(Node, global_group, get_own_nodes, [])),
		       case X of
			   X when is_list(X) ->
			       lists:sort(X);
			   _ ->
			       []
		       end;
		   no_conf ->
		       []
	       end,

    NNC = lists:delete(Node, S#state.no_contact),
    NSE = lists:delete(Node, S#state.sync_error),
    OwnNG = get_own_nodes(),
    case OwnNG of
	OthersNG ->
	    send_monitor(S#state.monitor, {nodeup, Node}, S#state.sync_state),
	    global_name_server ! {nodeup, Node},
	    case lists:member(Node, S#state.nodes) of
		false ->
		    NN = lists:sort([Node | S#state.nodes]),
		    {noreply, S#state{nodes = NN, 
				      no_contact = NNC,
				      sync_error = NSE}};
		true ->
		    {noreply, S#state{no_contact = NNC,
				      sync_error = NSE}}
	    end;
	_ ->
	    case {lists:member(Node, get_own_nodes()), 
		  lists:member(Node, S#state.sync_error)} of
		{true, false} ->
		    NSE2 = lists:sort([Node | S#state.sync_error]),
		    {noreply, S#state{no_contact = NNC,
				      sync_error = NSE2}};
		_ ->
		    {noreply, S}
	    end
    end;

%%%====================================================================================
%%% A node has crashed. 
%%% nodedown must always be sent to global; this is a security measurement
%%% because during release upgrade the global_groups parameter is upgraded
%%% before the node is synced. This means that nodedown may arrive from a
%%% node which we are not aware of.
%%%====================================================================================
handle_info({nodedown, Node}, S) when S#state.sync_state =:= no_conf ->
%    io:format("~p>>>>> nodedown, no_conf Node ~p~n",[node(), Node]),
    send_monitor(S#state.monitor, {nodedown, Node}, S#state.sync_state),
    global_name_server ! {nodedown, Node},
    {noreply, S};
handle_info({nodedown, Node}, S) ->
%    io:format("~p>>>>> nodedown, Node ~p  ~n",[node(), Node]),
    send_monitor(S#state.monitor, {nodedown, Node}, S#state.sync_state),
    global_name_server ! {nodedown, Node},
    NN = lists:delete(Node, S#state.nodes),
    NSE = lists:delete(Node, S#state.sync_error),
    NNC = case {lists:member(Node, get_own_nodes()), 
		lists:member(Node, S#state.no_contact)} of
	      {true, false} ->
		  [Node | S#state.no_contact];
	      _ ->
		  S#state.no_contact
	  end,
    {noreply, S#state{nodes = NN, no_contact = NNC, sync_error = NSE}};


%%%====================================================================================
%%% A node has changed its global_groups definition, and is telling us that we are not
%%% included in his group any more. This could happen at release upgrade.
%%%====================================================================================
handle_info({disconnect_node, Node}, S) ->
%    io:format("~p>>>>> disconnect_node Node ~p CN ~p~n",[node(), Node, S#state.nodes]),
    case {S#state.sync_state, lists:member(Node, S#state.nodes)} of
	{synced, true} ->
	    send_monitor(S#state.monitor, {nodedown, Node}, S#state.sync_state);
	_ ->
	    cont
    end,
    global_name_server ! {nodedown, Node}, %% nodedown is used to inform global of the
                                           %% disconnected node
    NN = lists:delete(Node, S#state.nodes),
    NNC = lists:delete(Node, S#state.no_contact),
    NSE = lists:delete(Node, S#state.sync_error),
    {noreply, S#state{nodes = NN, no_contact = NNC, sync_error = NSE}};




handle_info({'EXIT', ExitPid, Reason}, S) ->
    check_exit(ExitPid, Reason),
    {noreply, S};


handle_info(_Info, S) ->
%    io:format("***** handle_info = ~p~n",[_Info]),
    {noreply, S}.



terminate(_Reason, _S) ->
    ok.
    

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.





%%%====================================================================================
%%% Check the global group configuration.
%%%====================================================================================

config_scan(NodeGrps) ->
    config_scan(NodeGrps, original).

config_scan(NodeGrps, original) ->
    case config_scan(NodeGrps, publish_type) of
	{DefGroupName, _, DefNodes, DefOther} ->
	    {DefGroupName, DefNodes, DefOther};
	Error ->
	    Error
    end;
config_scan(NodeGrps, publish_type) ->
    config_scan(node(), normal, NodeGrps, no_name, [], []).

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
%%% The special process which checks that all nodes in the own global group
%%% agrees on the configuration.
%%%====================================================================================
-spec sync_init(_, _, _, _) -> no_return().
sync_init(Type, Cname, PubType, Nodes) ->
    {Up, Down} = sync_check_node(lists:delete(node(), Nodes), [], []),
    sync_check_init(Type, Up, Cname, Nodes, Down, PubType).

sync_check_node([], Up, Down) ->
    {Up, Down};
sync_check_node([Node|Nodes], Up, Down) ->
    case net_adm:ping(Node) of
	pang ->
	    sync_check_node(Nodes, Up, [Node|Down]);
	pong ->
	    sync_check_node(Nodes, [Node|Up], Down)
    end.



%%%-------------------------------------------------------------
%%% Check that all nodes are in agreement of the global
%%% group configuration.
%%%-------------------------------------------------------------
-spec sync_check_init(_, _, _, _, _, _) -> no_return().
sync_check_init(Type, Up, Cname, Nodes, Down, PubType) ->
    sync_check_init(Type, Up, Cname, Nodes, 3, [], Down, PubType).

-spec sync_check_init(_, _, _, _, _, _, _, _) -> no_return().
sync_check_init(_Type, NoContact, _Cname, _Nodes, 0, ErrorNodes, Down, _PubType) ->
    case ErrorNodes of
	[] -> 
	    gen_server:cast(global_group, {synced, lists:sort(NoContact ++ Down)});
	_ ->
	    gen_server:cast(global_group, {sync_error, lists:sort(NoContact ++ Down),
					   ErrorNodes})
    end,
    receive
	kill ->
	    exit(normal)
    after 5000 ->
	    exit(normal)
    end;

sync_check_init(Type, Up, Cname, Nodes, N, ErrorNodes, Down, PubType) ->
    ConfCheckMsg = case PubType of
		       normal ->
			   {conf_check, ?cc_vsn, node(), self(), Type, Cname, Nodes};
		       _ ->
			   {conf_check, ?cc_vsn, node(), self(), Type, Cname, PubType, Nodes}
		   end,
    lists:foreach(fun(Node) -> 
			  gen_server:cast({global_group, Node}, ConfCheckMsg)
		  end, Up),
    case sync_check(Up) of
	{ok, synced} ->
	    sync_check_init(Type, [], Cname, Nodes, 0, ErrorNodes, Down, PubType);
	{error, NewErrorNodes} ->
	    sync_check_init(Type, [], Cname, Nodes, 0, ErrorNodes ++ NewErrorNodes, Down, PubType);
	{more, Rem, NewErrorNodes} ->
	    %% Try again to reach the global_group, 
	    %% obviously the node is up but not the global_group process.
	    sync_check_init(Type, Rem, Cname, Nodes, N-1, ErrorNodes ++ NewErrorNodes, Down, PubType)
    end.

sync_check(Up) ->
    sync_check(Up, Up, []).

sync_check([], _Up, []) ->
    {ok, synced};
sync_check([], _Up, ErrorNodes) ->
    {error, ErrorNodes};
sync_check(Rem, Up, ErrorNodes) ->
    receive
	{config_ok, ?cc_vsn, Pid, Node} when Pid =:= self() ->
	    global_name_server ! {nodeup, Node},
	    sync_check(Rem -- [Node], Up, ErrorNodes);
	{config_error, ?cc_vsn, Pid, Node} when Pid =:= self() ->
	    sync_check(Rem -- [Node], Up, [Node | ErrorNodes]);
	{no_global_group_configuration, ?cc_vsn, Pid, Node} when Pid =:= self() ->
	    sync_check(Rem -- [Node], Up, [Node | ErrorNodes]);
	%% Ignore, illegal vsn or illegal Pid
	_ ->
	    sync_check(Rem, Up, ErrorNodes)
    after 2000 ->
	    %% Try again, the previous conf_check message  
	    %% apparently disapared in the magic black hole.
	    {more, Rem, ErrorNodes}
    end.


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
    case lists:member(Node, get_own_nodes()) of
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
    case lists:member(Node, get_own_nodes()) of
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
    check_exit_where(get(whereis_name), ExitPid, Reason).


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



%%%====================================================================================
%%% Kill any possible global_group_check processes
%%%====================================================================================
kill_global_group_check() ->
    case whereis(global_group_check) of
	undefined ->
	    ok;
	Pid ->
	    unlink(Pid),
	    global_group_check ! kill,
	    unregister(global_group_check)
    end.


%%%====================================================================================
%%% Disconnect nodes not belonging to own global_groups
%%%====================================================================================
disconnect_nodes(DisconnectNodes) ->
    lists:foreach(fun(Node) ->
			  {global_group, Node} ! {disconnect_node, node()},
			  global:node_disconnected(Node)
		  end,
		  DisconnectNodes).


%%%====================================================================================
%%% Disconnect nodes not belonging to own global_groups
%%%====================================================================================
force_nodedown(DisconnectNodes) ->
    lists:foreach(fun(Node) ->
			  erlang:disconnect_node(Node),
			  global:node_disconnected(Node)
		  end,
		  DisconnectNodes).


%%%====================================================================================
%%% Get the current global_groups definition
%%%====================================================================================
get_own_nodes_with_errors() ->
    case application:get_env(kernel, global_groups) of
	undefined ->
	    {ok, all};
	{ok, []} ->
	    {ok, all};
	{ok, NodeGrps} ->
	    case catch config_scan(NodeGrps, publish_type) of
		{error, Error} ->
		    {error, Error};
		{_, _, NodesDef, _} ->
		    {ok, lists:sort(NodesDef)}
	    end
    end.

get_own_nodes() ->
    case get_own_nodes_with_errors() of
	{ok, all} ->
	    [];
	{error, _} ->
	    [];
	{ok, Nodes} ->
	    Nodes
    end.

%%%====================================================================================
%%% -hidden command line argument
%%%====================================================================================
publish_arg() ->
    case init:get_argument(hidden) of
	{ok,[[]]} ->
	    hidden;
	{ok,[["true"]]} ->
	    hidden;
	_ ->
	    normal
    end.


%%%====================================================================================
%%% Own group publication type and nodes
%%%====================================================================================
own_group() ->
    case application:get_env(kernel, global_groups) of
	undefined ->
	    no_group;
	{ok, []} ->
	    no_group;
	{ok, NodeGrps} ->
	    case catch config_scan(NodeGrps, publish_type) of
		{error, _} ->
		    no_group;
		{_, PubTpGrp, NodesDef, _} ->
		    {PubTpGrp, NodesDef}
	    end
    end.


%%%====================================================================================
%%% Help function which computes publication list
%%%====================================================================================
publish_on_nodes(normal, no_group) ->
    all;
publish_on_nodes(hidden, no_group) ->
    [];
publish_on_nodes(normal, {normal, _}) ->
    all;
publish_on_nodes(hidden, {_, Nodes}) ->
    Nodes;
publish_on_nodes(_, {hidden, Nodes}) ->
    Nodes.

%%%====================================================================================
%%% Update net_kernels publication list
%%%====================================================================================
update_publish_nodes(PubArg) ->
    update_publish_nodes(PubArg, no_group).
update_publish_nodes(PubArg, MyGroup) ->
    net_kernel:update_publish_nodes(publish_on_nodes(PubArg, MyGroup)).


%%%====================================================================================
%%% Fetch publication list
%%%====================================================================================
publish_on_nodes() ->
    publish_on_nodes(publish_arg(), own_group()).
