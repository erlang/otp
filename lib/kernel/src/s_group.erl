%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
-module(s_group).

%% The module groups nodes into s_groups where each s\_group has its own
%% name space. Nodes may belong to multiple s\_groups. Global s\_group
%% node uniquiness is not guaranteed.

-behaviour(gen_server).

%% External exports
-export([start/0, start_link/0, stop/0, init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).
-export([config_scan/1, config_scan/2]).

-export([register_name/3, register_name_external/3,
	 unregister_name/2, unregister_name_external/2,
	 re_register_name/3,
	 send/3, send/4,
	 whereis_name/2, whereis_name/3,
	 registered_names/1,
	 s_groups/0,
	 own_nodes/0, own_nodes/1,
	 own_s_groups/0,
	 sync/0,
	 new_s_group/1, new_s_group/2,
	 add_nodes/2,
	 info/0,
	 delete_s_group/1, remove_nodes/2,
	 choose_nodes/1,
	 add_attributes/1, add_attributes/2,
	 remove_attributes/1, remove_attributes/2,
	 registered_attributes/0,
 	 monitor_nodes/1]).

-export([publish_on_nodes/0, 
	 get_own_nodes/0, get_own_nodes_with_errors/0,
         get_own_s_groups_with_nodes/0,
	 force_nodedown/1,
	 ng_pairs/1,
	 new_s_group_check/4, s_group_conflict_check/1,
	 delete_s_group_check/2, add_nodes_check/3, remove_nodes_check/3,
	 record_state/0]).

-export([connect_free_nodes/1, connect_s_group_nodes/1, is_free_normal/0]).

%% Delete?
%-export([s_groups_changed/1]).
%-export([s_groups_added/1]).
%-export([s_groups_removed/1]).
%-export([ng_add_check/2, ng_add_check/3]).
-export([registered_names_test/1]).
-export([send_test/2]).
-export([whereis_name_test/1]).

%% Internal exports
-export([sync_init/4]).

-include_lib("kernel/include/file.hrl").

-define(cc_vsn, 2).

-define(debug(_), ok).

%%-define(debug(Term), erlang:display(Term)).

%%%====================================================================================

%-type publish_type() :: 'hidden' | 'normal'.
-type publish_type() :: 'normal'.
-type sync_state()   :: 'no_conf' | 'synced'.

-type group_name()  :: atom().
-type group_tuple() :: {GroupName :: group_name(), [node()]}
                     | {GroupName :: group_name(),
                        PublishType :: publish_type(),
                        [node()]}.

%%%====================================================================================
%%% The state of the s_group process
%%% 
%%% sync_state =  no_conf (s_groups not defined, inital state) |
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
		node_name = node()          :: node(),
		connect_all                 :: boolean(),
		group_names = []            :: [group_name()],
		nodes = []                  :: [node()],
		no_contact = []             :: [node()],
		sync_error = []             :: [node()],
                own_grps =[]                :: [{group_name(), [node()]}],
		other_grps = []             :: [{group_name(), [node()]}],
		monitor = []                :: [pid()],
		publish_type = normal       :: publish_type(),
		group_publish_type = normal :: publish_type(),
		attributes = []}).


%%%====================================================================================
%%% External exported
%%%====================================================================================

-spec s_groups() ->  {OwnGroupNames, GroupNames}  | undefined when
    OwnGroupNames :: [group_name()],
    GroupNames :: [group_name()].
s_groups() ->
    request(s_groups).

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
    request({own_nodes}).

-spec own_nodes(SGroupName) -> Nodes when
      SGroupName :: group_name(),
      Nodes :: [Node :: node()].
own_nodes(SGroupName) ->
    request({own_nodes, SGroupName}).

-spec own_s_groups() -> GroupTuples when
      GroupTuples :: [GroupTuple :: group_tuple()].
own_s_groups() ->
    request(own_s_groups).

-type name()  :: atom().
-type where() :: {'node', node()} | {'group', group_name()}.

-spec registered_names(Where) -> [{SGroupName, Name}] when
      Where :: where(),
      SGroupName :: group_name(),
      Name :: name().
registered_names(Arg) ->
    request({registered_names, Arg}).

-spec send(SGroupName, Name, Msg) -> Pid | {badarg, {SGroupName, Name, Msg}} when
      SGroupName :: group_name(),
      Name :: name(),
      Msg :: term(),
      Pid :: pid().
send(SGroupName, Name, Msg) ->
    request({send, SGroupName, Name, Msg}).

-spec send(Node, SGroupName, Name, Msg) -> Pid | {badarg, {SGroupName, Name, Msg}} when
      Node :: node(),
      SGroupName :: group_name(),
      Name :: name(),
      Msg :: term(),
      Pid :: pid().
send(Node, SGroupName, Name, Msg) ->
    request({send, Node, SGroupName, Name, Msg}).

-spec register_name(SGroupName, Name, Pid) -> 'yes' | {no, Reason} when
      SGroupName :: group_name(),
      Name :: term(),
      Pid :: pid(),
      Reason :: term().
register_name(SGroupName, Name, Pid) when is_pid(Pid) ->
    request({register_name, SGroupName, Name, Pid}).

-spec register_name_external(SGroupName, Name, Pid) -> 'yes' | {no, Reason} when
      SGroupName :: group_name(),
      Name :: term(),
      Pid :: pid(),
      Reason :: term().
register_name_external(SGroupName, Name, Pid) when is_pid(Pid) ->
    request({register_name_external, SGroupName, Name, Pid}).

-spec unregister_name(SGroupName, Name) -> _ when
      SGroupName :: group_name(),
      Name :: term().
unregister_name(SGroupName, Name) ->
      request({unregister_name, SGroupName, Name}).

unregister_name_external(SGroupName, Name) ->
      request({unregister_name, SGroupName, Name}).

-spec re_register_name(SGroupName, Name, Pid) -> 'yes' | {no, Reason} when
      SGroupName :: group_name(),
      Name :: term(),
      Pid :: pid(),
      Reason :: term().
re_register_name(SGroupName, Name, Pid) when is_pid(Pid) ->
      request({re_register_name, SGroupName, Name, Pid}).

-spec whereis_name(SGroupName, Name) -> pid() | 'undefined' when
      SGroupName :: group_name(),
      Name :: name().
whereis_name(SGroupName, Name) ->
    request({whereis_name, SGroupName, Name}).

-spec whereis_name(Node, SGroupName, Name) -> pid() | 'undefined' when
      Node :: node(),
      SGroupName :: group_name(),
      Name :: name().
whereis_name(Node, SGroupName, Name) ->
    request({whereis_name, Node, SGroupName, Name}).

s_group_conflict_check(SGroupName) ->
    request({s_group_conflict_check, SGroupName}).

-spec new_s_group(Nodes) -> {ok, SGroupName, Nodes} |
      			    {error, Reason} when
      Nodes :: [Node :: node()],
      SGroupName :: group_name(),
      Reason :: term().
new_s_group(Nodes) ->
    SGroupName = erlang:list_to_atom(erlang:binary_to_list(crypto:strong_rand_bytes(30))),
    case new_s_group(SGroupName, Nodes) of
        {error, s_group_name_is_in_use} ->
	    new_s_group(Nodes);
	Result ->
	    Result
    end.

-spec new_s_group(SGroupName, Nodes) -> {ok, SGroupName, Nodes} |
      			                {error, Reason} when
      SGroupName :: group_name(),
      Nodes :: [Node :: node()],
      Reason :: term().
new_s_group(SGroupName, Nodes0) ->
    case SGroupName of
        undefined ->
	    {error, reserved_s_group_name};
  	_ ->
	    Nodes = lists:usort(Nodes0),
    	    case Nodes of
                [] ->
	    	    {ok, SGroupName, []};
	    	_ -> 
	    	    case lists:member(node(), Nodes) of
	       	        false ->
                    	    {error, remote_s_group};
	       	    	true ->
                    	    request({new_s_group, SGroupName, Nodes})
	            end
    	    end
    end.

new_s_group_check(Node, PubType, SGroupName, Nodes) ->
    request({new_s_group_check, Node, PubType, SGroupName, Nodes}).

-spec add_nodes(SGroupName, Nodes) -> {ok, SGroupName, Nodes} |
      			              {error, Reason} when
      SGroupName :: group_name(),
      Nodes :: [Node :: node()],
      Reason :: term().
add_nodes(SGroupName, Nodes0) ->
    Nodes = lists:usort(Nodes0) -- [node()],
    case Nodes of
        [] ->
	   {ok, SGroupName, []};
	_ ->
	   add_nodes_do(SGroupName, Nodes)
    end.

add_nodes_do(SGroupName, Nodes) -> 
    request({add_nodes, SGroupName, Nodes}).

add_nodes_check(Node, PubType, {SGroupName, Nodes}) ->
    request({add_nodes_check, Node, PubType, {SGroupName, Nodes}}).

-spec remove_nodes(SGroupName, Nodes) -> 'ok' | {error, Reason} when
      SGroupName :: group_name(),
      Nodes :: [Node :: node()],
      Reason :: term().
remove_nodes(SGroupName, Nodes0) ->
    Nodes = lists:usort(Nodes0),
    case Nodes of
        [] ->
	    ok;
	_ ->
	    case lists:member(node(), Nodes) of
	        true ->
		    {error, "cannot_remove_itself"};
		false ->
		    remove_nodes_request(SGroupName, Nodes)
	    end
    end.

remove_nodes_request(SGroupName, Nodes) ->
    request({remove_nodes, SGroupName, Nodes}).	  

remove_nodes_check(SGroupName, NewSGroupNodes, NodesToRmv) ->
    request({remove_nodes_check, SGroupName, NewSGroupNodes, NodesToRmv}).

-spec delete_s_group(SGroupName) -> 'ok' | {error, Reason} when
      SGroupName :: group_name(),
      Reason :: term().
delete_s_group(SGroupName) ->
    case request({delete_s_group, SGroupName}) of
        ok ->
	    case application:get_env(kernel, s_groups) of
	        undefined ->
		    connect_free_nodes(node());
		_ ->
		    ok
	    end,
	    ok;
	Result ->
	    Result
    end.

delete_s_group_check(InitNode, SGroupName) ->
    request({delete_s_group_check, InitNode, SGroupName}).

choose_nodes(Args0) ->
    Args = combine_attribute_args(Args0, [], []),
    ListOfNodes = choose_nodes(Args, []),
    overlap_nodes(ListOfNodes, []).

choose_nodes([], ListOfNodes) ->
    ListOfNodes;
choose_nodes([Arg | Args], ListOfNodes) ->
    Nodes = request({choose_nodes, Arg}),
    choose_nodes(Args, [Nodes | ListOfNodes]).

combine_attribute_args([], Attribs, RemArgs) ->
    [{attributes, Attribs}] ++ RemArgs;
combine_attribute_args([Arg | Args0], Attribs, RemArgs) ->
    case Arg of
        {attribute, Attrib} ->
	    combine_attribute_args(Args0, [Attrib | Attribs], RemArgs);
	_ ->
	    combine_attribute_args(Args0, Attribs, [Arg | RemArgs])
    end.

-spec add_attributes(Args) -> {ok, Args} | {error, Reason} when
      Args :: [term()],
      Reason :: term().
add_attributes(Args) ->
    case is_list(Args) of
        true ->
	    request({add_attributes, lists:usort(Args)});
	_ ->
	    {error, parameter_should_be_a_list}
    end.

add_attributes(Nodes, Args) ->
    case is_list(Nodes) andalso is_list(Args) of
        true ->
	    request({add_attributes, lists:usort(Nodes), lists:usort(Args)});
	_ ->
	    {error, both_parameters_should_be_lists}
    end.

-spec remove_attributes(Args) -> {ok, Args} | {error, Reason} when
      Args :: [term()],
      Reason :: term().
remove_attributes(Args) ->
    case is_list(Args) of
        true ->
	    request({remove_attributes, Args});
	_ ->
	    {error, parameter_should_be_a_list}
    end.

remove_attributes(Nodes, Args) ->
    case is_list(Nodes) andalso is_list(Args) of
        true ->
	    request({remove_attributes, lists:usort(Nodes), lists:usort(Args)});
	_ ->
	    {error, both_parameters_should_be_lists}
    end.

registered_attributes() ->
    request(registered_attributes).


%%s_groups_changed(NewPara) ->
%%    request({s_groups_changed, NewPara}).

%%s_groups_added(NewPara) ->
%%    request({s_groups_added, NewPara}).

%%s_groups_removed(NewPara) ->
%%    request({s_groups_removed, NewPara}).

-spec sync() -> 'ok'.
sync() ->
    request(sync).

%%ng_add_check(Node, OthersNG) ->
%%    ng_add_check(Node, normal, OthersNG).

%%ng_add_check(Node, PubType, OthersNG) ->
%%    request({ng_add_check, Node, PubType, OthersNG}).

-type info_item() :: {'state', 	 	  State :: sync_state()}
                   | {'own_group_names',  GroupName :: [group_name()]}
                   | {'own_group_nodes',  Nodes :: [node()]}
                   | {'synched_nodes', 	  Nodes :: [node()]}
                   | {'sync_error', 	  Nodes :: [node()]}
                   | {'no_contact', 	  Nodes :: [node()]}
                   | {'own_s_groups', 	  OwnGroups::[group_tuple()]}
                   | {'other_groups', 	  Groups :: [group_tuple()]}
                   | {'monitoring', 	  Pids :: [pid()]}.

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
    case whereis(s_group) of
	P when is_pid(P) ->
	    gen_server:call(s_group, Req, Time);
	_Other -> 
	    {error, s_group_not_runnig}
    end.

%%%====================================================================================
%%% gen_server start
%%%
%%% The first thing to happen is to read if the s_groups key is defined in the
%%% .config file. If not defined then the node started as free.
%%% Otherwise a sync process is started to check that all nodes in the own s_group
%%% group have the same configuration. This is done by sending 'conf_check' to all
%%% other nodes and requiring 'conf_check_result' back.
%%% If the nodes are not in agreement of the configuration the s_group process 
%%% will remove these nodes from the #state.nodes list. This can be a normal case
%%% at release upgrade when all nodes are not yet upgraded.
%%%
%%% It is possible to manually force a sync of the s_group. This is done for 
%%% instance after a release upgrade, after all nodes in the group beeing upgraded.
%%% The nodes are not synced automatically because it would cause the node to be
%%% disconnected from those not yet beeing upgraded.
%%%
%%% The three process dictionary variables (registered_names, send, and whereis_name) 
%%% are used to store information needed if the search process crashes. 
%%% The search process is a help process to find registered names in the system.
%%%====================================================================================
start() -> gen_server:start({local, s_group}, s_group, [], []).
start_link() -> gen_server:start_link({local, s_group},s_group,[],[]).
stop() -> gen_server:call(s_group, stop, infinity).

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
    _UpdateConfigFile = is_rsconfig(),
    case application:get_env(kernel, s_groups) of
	undefined ->
	    update_publish_nodes(PT),
	    NewState = #state{publish_type = PT,
			      connect_all = Ca},
	    {ok, NewState};
	{ok, []} ->
	    update_publish_nodes(PT),
	    application:unset_env(kernel, s_groups),
	    NewState = #state{publish_type = PT,
			      connect_all = Ca},
	    {ok, NewState};
	{ok, NodeGrps0} ->
	    ?debug({"s_group_init_NodeGrps", NodeGrps0}),
	    NodeGrps = update_conf(NodeGrps0),
	    case NodeGrps of
	        [] ->
		    update_publish_nodes(PT),
	    	    application:unset_env(kernel, s_groups),
		    NewState = #state{publish_type = PT,
			              connect_all = Ca},
	    	    {ok, NewState};
		_ ->
	    	    AllNodes = lists:append([Nds || {_G, _T, Nds} <- NodeGrps]),
	    	    case lists:member(node(), AllNodes) of
	                true ->
                    	    case catch config_scan(NodeGrps, publish_type) of
                                {error, _Error2} ->
                    	    	    update_publish_nodes(PT),
                    	    	    exit({error, {'invalid g_groups definition', NodeGrps}});
                	        {ok, DefOwnSGroupsT, DefOtherSGroups} ->
%% Check that no s_group is called 'undefined'
			            DefOwnSGroupsT1 = [{GroupName, GroupNodes} ||
                                                       {GroupName, _PubType, GroupNodes}
                                                       <- DefOwnSGroupsT, GroupName/=undefined],

				    {DefSGroupNamesT1, DefSGroupNodesT1} = lists:unzip(DefOwnSGroupsT1),
                    	    	    DefSGroupNamesT = lists:usort(DefSGroupNamesT1),
                    	    	    DefSGroupNodesT = lists:usort(lists:append(DefSGroupNodesT1)),
                    	    	    update_publish_nodes(PT, {normal, DefSGroupNodesT}),
                    	    	    %% First disconnect any nodes not belonging to our own s_group
                    	    	    disconnect_nodes(nodes(connected) -- DefSGroupNodesT),
                    	    	    lists:foreach(fun(Node) ->
                                              erlang:monitor_node(Node, true)
                                              end, DefSGroupNodesT),
                                    global_name_server ! {init_own_s_groups, DefOwnSGroupsT1},
			            DefOtherSGroups1 = [{GroupName, GroupNodes} ||
                                                        {GroupName, GroupNodes}
                                                        <- DefOtherSGroups, GroupName/=undefined],
                    	    	    NewState = #state{sync_state = synced,
				                      group_names = DefSGroupNamesT,
                                      	      	      own_grps = DefOwnSGroupsT1,
                                      	      	      other_grps = DefOtherSGroups1,
                                      	      	      no_contact = lists:delete(node(), DefSGroupNodesT),
				      	      	      publish_type = PT,
		    	              	      	      group_publish_type = normal
				      	      	      },
                    	            {ok, NewState}
                            end;
	                _ ->
		    	    update_publish_nodes(PT),
	    	    	    application:unset_env(kernel, s_groups),
	    	    	    {ok, #state{publish_type = PT,
			                connect_all = Ca}}
                    end
            end
    end.


%%%====================================================================================
%%% sync() -> ok 
%%%
%%% An operator ordered sync of the own s_group. This must be done after
%%% a release upgrade. It can also be ordered if something has made the nodes
%%% to disagree of the s_groups definition.
%%%====================================================================================
handle_call(sync, _From, S) ->
    case application:get_env(kernel, s_groups) of
	undefined ->
	    update_publish_nodes(S#state.publish_type),
	    {reply, ok, S};
	{ok, []} ->
	    update_publish_nodes(S#state.publish_type),
	    {reply, ok, S};
	{ok, NodeGrps} ->
	    {DefGroupNames, PubTpGrp, DefNodes, DefOwn, DefOther} = 
		case catch config_scan(NodeGrps, publish_type) of
		    {error, _Error2} ->
			exit({error, {'invalid s_groups definition', NodeGrps}});
                    {ok, DefOwnSGroupsT, DefOtherSGroupsT} ->
                        DefOwnSGroupsT1 = [{GroupName,GroupNodes}||
                                              {GroupName, _PubType, GroupNodes}
                                                  <- DefOwnSGroupsT],
                        {DefSGroupNamesT1, DefSGroupNodesT1}=lists:unzip(DefOwnSGroupsT1),
                        DefSGroupNamesT = lists:usort(DefSGroupNamesT1),
                        DefSGroupNodesT = lists:usort(lists:append(DefSGroupNodesT1)),
                        PubType = normal,
                        update_publish_nodes(S#state.publish_type, {PubType, DefSGroupNodesT}),
			?debug({"s_group_sync_S", S}),
			?debug({"sync_nodes_connected_DefSGroupNodesT",
			              nodes(connected), DefSGroupNodesT}),
                        %% First inform global_name_server process on all nodes
			%% not belonging to our own s_groups
			disconnect_nodes(nodes(connected) -- DefSGroupNodesT),
			%% Sync with the nodes in the own s_groups
                        kill_s_group_check(),
                        Pid = spawn_link(?MODULE, sync_init, 
					 [sync, DefSGroupNamesT,
					  PubType, DefOwnSGroupsT1]),

			?debug({"sync_DefSGroupNamesT_PubType_DefOwnSGroupsT1",
			              DefSGroupNamesT, PubType, DefOwnSGroupsT1}),
			register(s_group_check, Pid),
                        {DefSGroupNamesT, PubType,
			     lists:delete(node(), DefSGroupNodesT),
                             DefOwnSGroupsT1, DefOtherSGroupsT}
                end,
            {reply, ok, S#state{sync_state = synced,
	    	    		group_names = DefGroupNames, 
				no_contact = lists:sort(DefNodes), 
                                own_grps = DefOwn,
				other_grps = DefOther,
				group_publish_type = PubTpGrp}}
    end;



%%%====================================================================================
%%% Get the names of the s_groups
%%%====================================================================================
handle_call(s_groups, _From, S) ->
    Result = case S#state.sync_state of
		 no_conf ->
		     undefined;
		 synced ->
		     Other = lists:foldl(fun({N,_L}, Acc) -> Acc ++ [N]
					 end,
					 [], S#state.other_grps),
		     {S#state.group_names, Other}
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
    {Res, State} = monitor_nodes(Flag, Pid, StateIn),
    {reply, Res, State};


%%%====================================================================================
%%% own_nodes() -> [Node]
%%% own_nodes(SGroupName) -> [Node] 
%%%
%%% Get a list of nodes in the own s_groups
%%%====================================================================================
handle_call({own_nodes}, _From, S) ->
    Nodes = case application:get_env(kernel, s_groups) of
		undefined ->
		    lists:usort([node() | global:get_known()]);
		_ ->
		    get_own_nodes()
	    end,
    {reply, Nodes, S};


handle_call({own_nodes, SGroupName}, _From, S) ->
    Nodes = case application:get_env(kernel, s_groups) of
            undefined ->
	        %% Free node
	    	case SGroupName of
	            undefined -> lists:usort([node() | global:get_known()]);
		    _ -> []
	    	end;
	    _ ->
	        %% S_group node
	    	case S#state.sync_state of
		    no_conf ->
		        [];
		    synced ->
		        case lists:member(SGroupName, S#state.group_names) of
		       	    true ->
			    	{SGroupName, Nodes1} = lists:keyfind(SGroupName, 1, S#state.own_grps),
				Nodes1;
			    _ ->
				[]
		        end
	        end
    	    end,
    {reply, Nodes, S};


%%%====================================================================================
%%% own_s_groups() -> [GroupTuples] 
%%%
%%% Get a list of own s_group tuples
%%%====================================================================================
handle_call(own_s_groups, _From, S) ->
    GroupTuples = case application:get_env(kernel, s_groups) of
		      undefined -> [];
		      _ -> S#state.own_grps
	          end,
    {reply, GroupTuples, S};

%%%====================================================================================
%%% registered_names({node, Node}) -> [Name] | {error, ErrorMessage}
%%% registered_names({s_group, SGroupName}) -> [Name] | {error, ErrorMessage}
%%%
%%% Get the registered names from a specified Node or SGroupName.
%%%====================================================================================
handle_call({registered_names, {s_group, SGroupName}}, From, S) ->
    case application:get_env(kernel, s_groups) of
        undefined ->
	    %% Free node
	    Res = case SGroupName of
	              undefined -> global:registered_names(undefined);
	    	      _ -> {error, no_information_about_remote_group}
	    	  end,
            {reply, Res, S};
	_ ->
	    %% S_group node
	    case lists:member(SGroupName, S#state.group_names) of 
	        true ->
	    	    Res = s_group_names(global:registered_names(all_names), SGroupName),
	    	    {reply, Res, S};
	    	false ->		 
	    	    case lists:keysearch(SGroupName, 1, S#state.other_grps) of
	    	        false ->
	    	    	    {reply, {error, no_information_about_remote_group}, S};
	    	    	{value, {SGroupName, []}} ->
	    	    	    {reply, {error, no_information_about_remote_group}, S};
	    	    	{value, {SGroupName, Nodes}} ->
	    	    	    Pid = global_search:start(names, {s_group, SGroupName, Nodes, From}),
	    	    	    Wait = get(registered_names),
	    	    	    put(registered_names, [{Pid, From} | Wait]),
	    	    	    {noreply, S}
	    	    end
	    end
    end;


handle_call({registered_names, {node, Node}}, _From, S) when Node =:= node() ->
    Res = global:registered_names(all_names),
    {reply, Res, S};
handle_call({registered_names, {node, Node}}, From, S) ->
    Pid = global_search:start(names, {node, Node, From}),
    Wait = get(registered_names),
    put(registered_names, [{Pid, From} | Wait]),
    {noreply, S};


%%%====================================================================================
%%% send(SGroupName, Name, Msg) -> Pid | {badarg, {SGroupName, Name, Msg}}
%%% send(Node, SGroupName, Name, Msg) -> Pid | {badarg, {SGroupName, Name, Msg}}
%%%
%%% Send the Msg to the specified group registered Name in own s_group,
%%% in specified Node, or SGroupName.
%%% But first the receiver is to be found, the thread is continued at
%%% handle_cast(send_res)
%%%====================================================================================
%% Search in the whole known world, but check own node first.
handle_call({send, SGroupName, Name, Msg}, From, S) ->
    case global:whereis_name(SGroupName, Name) of
        undefined ->
    	    Pid = global_search:start(send, {any, S#state.other_grps, SGroupName, Name, Msg, From}),
    	    Wait = get(send),
    	    put(send, [{Pid, From, SGroupName, Name, Msg} | Wait]),
    	    {noreply, S};
    	Found ->
    	    Found ! Msg,
    	    {reply, Found, S}
    end;


%% Search on the specified node.
handle_call({send, Node, SGroupName, Name, Msg}, From, S) ->
    Pid = global_search:start(send, {node, Node, SGroupName, Name, Msg, From}),
    Wait = get(send),
    put(send, [{Pid, From, SGroupName, Name, Msg} | Wait]),
    {noreply, S};


%%%====================================================================================
%%% register_name(SGroupName, Name, Pid) -> 'yes' | {no, Reason}
%%% register_name_external(SGroupName, Name, Pid) -> 'yes' | {no, Reason}
%%% unregister_name(SGroupName, Name) -> _
%%% re_register_name(SGroupName, Name, Pid) -> 'yes' | {no, Reason}
%%%
%%% Registration, unregistration, and re-registration of a Name
%%% in a specified s_group
%%%====================================================================================
handle_call({register_name, SGroupName, Name, Pid}, _From, S) ->
    Res = case application:get_env(kernel, s_groups) of
	      undefined ->
	      	   %% Free node
		   case SGroupName of
		       undefined -> global:register_name(undefined, Name, Pid, fun global:random_exit_name/3);
		       _ -> {no, cannot_register_in_remote_group}
		   end;
	      _ ->
		   %% S_group node
    		   case lists:member(SGroupName, S#state.group_names) of
        	       true -> global:register_name(SGroupName, Name, Pid, fun global:random_exit_name/3);
        	       _ -> {no, cannot_register_in_remote_group}
    		   end
          end,
    {reply, Res, S};


handle_call({register_name_external, SGroupName, Name, Pid}, _From, S) ->
    Res = case application:get_env(kernel, s_groups) of
	      undefined ->
	      	   %% Free node
		   case SGroupName of
		       undefined -> global:register_name_external(undefined, Name, Pid, fun global:random_exit_name/3);
		       _ -> {no, cannot_register_in_remote_group}
		   end;
	      _ ->
		   %% S_group node
    		   case lists:member(SGroupName, S#state.group_names) of
        	       true -> global:register_name_external(SGroupName, Name, Pid, fun global:random_exit_name/3);
        	       _ -> {no, cannot_register_in_remote_group}
    		   end
          end,
    {reply, Res, S};


handle_call({unregister_name, SGroupName, Name}, _From, S) ->
    Res = case application:get_env(kernel, s_groups) of
	      undefined ->
	      	   %% Free node
		   case SGroupName of
		       undefined -> global:unregister_name(SGroupName, Name);
		       _ -> {no, cannot_unregister_from_remote_group}
		   end;
	      _ ->
		   %% S_group node
    		   case lists:member(SGroupName, S#state.group_names) of
        	       true -> global:unregister_name(SGroupName, Name);
        	       _ -> {no, cannot_unregister_from_remote_group}
    		   end
          end,
    {reply, Res, S};


handle_call({re_register_name, SGroupName, Name, Pid}, _From, S) ->
    Res = case application:get_env(kernel, s_groups) of
	      undefined ->
	      	   %% Free node
		   case SGroupName of
		       undefined -> global:re_register_name(SGroupName, Name, Pid, fun global:random_exit_name/3);
		       _ -> {no, cannot_re_register_in_remote_group}
		   end;
	      _ ->
		   %% S_group node
    		   case lists:member(SGroupName, S#state.group_names) of
        	       true -> global:re_register_name(SGroupName, Name, Pid, fun global:random_exit_name/3);
        	       _ -> {no, cannot_re_register_in_remote_group}
    		   end
          end,
    {reply, Res, S};


%%%====================================================================================
%%% whereis_name(SGroupName, Name) -> Pid | undefined
%%% whereis_name(Node, SGroupName, Name) -> Pid | undefined
%%%
%%% Get the Pid of a registered Name in own s_group, in specified Node,
%%% or SGroupName. But first the process is to be found, 
%%% the thread is continued at handle_cast(find_name_res)
%%%====================================================================================
%% Search on the specified node.
handle_call({whereis_name, Node, SGroupName, Name}, From, S) ->
    Pid = global_search:start(whereis, {node, Node, SGroupName, Name, From}),
    Wait = get(whereis_name),
    put(whereis_name, [{Pid, From} | Wait]),
    {noreply, S};

%% Search in the whole known world, but check own node first.
handle_call({whereis_name, SGroupName, Name}, From, S) ->
    case global:whereis_name(SGroupName, Name) of
	undefined ->
	    Pid = global_search:start(whereis, {any, S#state.other_grps, SGroupName, Name, From}),
	    Wait = get(whereis_name),
	    put(whereis_name, [{Pid, From} | Wait]),
	    {noreply, S};
	Found ->
	    {reply, Found, S}
    end;


%%%======================================================================
%%% Check whether remote node already belongs to a new s_group
%%%======================================================================
handle_call({s_group_conflict_check, SGroupName}, _From, S) ->
    ?debug({"s_group_conflict_check_SGroupName", SGroupName}),
    case lists:member(SGroupName, S#state.group_names) of
        true ->
	     {reply, not_agreed, S};
        false ->
    	     {reply, agreed, S}
    end;


%%%======================================================================
%%% new_s_group(SGroupName, Nodes) -> {ok, SGroupName, NewNodes} |
%%% 	  			      {error, Reason}.
%%% Create a new s_group.
%%%======================================================================
handle_call({new_s_group, SGroupName, Nodes0}, _From, S) ->
    ?debug({"new_s_group_SGroupName_Nodes0", SGroupName, Nodes0}),
    case lists:member(SGroupName, S#state.group_names) of 
        true ->
            {reply, {error, s_group_name_is_in_use}, S};
        false ->
	    ?debug({"new_s_group_Nodes0", Nodes0}),
	    NewNodes0 = Nodes0 -- [node()],
	    ?debug({"new_s_group_NewNodes0", NewNodes0}),
	    case application:get_env(kernel, s_groups) of
                undefined ->
	   	   FreeConnectedNodes0 = global:get_known_s_group('no_group'),
		   NodesToDisconnect = intersection(NewNodes0, FreeConnectedNodes0),
		   NodesToDisconnect0 = FreeConnectedNodes0 -- NodesToDisconnect,
		   disconnect_nodes(NodesToDisconnect0);
	        _ ->
		   NodesToDisconnect = intersection(NewNodes0, nodes(connected))
    	    end,

	    NewNodes = s_group_conflict(NewNodes0, [SGroupName]),
	    Nodes = lists:usort(NewNodes ++ [node()]),

	    NewSGroupNames = lists:sort([SGroupName | S#state.group_names]),
	    NewOwnSGroups = [{SGroupName, Nodes} | S#state.own_grps],
	    NewOtherSGroups =
	          case lists:keyfind(SGroupName, 1, S#state.other_grps) of
    		      false -> S#state.other_grps;
		      _ -> lists:keydelete(SGroupName, 1, S#state.other_grps)
		  end,
	    
            NewConf = mk_new_s_group_tuples(SGroupName, Nodes),
            ?debug({"Newconf:", NewConf}),
            application:set_env(kernel, s_groups, NewConf),

            NGACArgs = [node(), normal, SGroupName, Nodes],
            ?debug({"NGACArgs:",NGACArgs}),
	    %% NS: agreed nodes; NNC: badrpc nodes; NSE: not_agreed nodes
            {NS, NNC, NSE} =
                lists:foldl(fun(Node, {NN_acc, NNC_acc, NSE_acc}) -> 
                           case rpc:call(Node, s_group, new_s_group_check, NGACArgs) of
                               {badrpc, _} ->
                                       {NN_acc, [Node | NNC_acc], NSE_acc};
                               agreed ->
                                       {[Node | NN_acc], NNC_acc, NSE_acc};
                               not_agreed ->
                                       {NN_acc, NNC_acc, [Node | NSE_acc]}
                           end
                end, {[], [], []}, NewNodes),
            ?debug({"NS_NNC_NSE1:",  {NS, NNC, NSE}}),

	    disconnect_nodes(NodesToDisconnect),

	    %% Send s_group info update to the own global_name_server
	    global_name_server ! {own_s_group_update, SGroupName, Nodes},
	    
	    NewOwnNodes = lists:usort(lists:append([Nds || {_G, Nds} <- NewOwnSGroups])),
	    update_publish_nodes(S#state.publish_type, {normal, NewOwnNodes}),

	    ?debug({"s_group_handle_new_s_group_S", S}),
            NewS = S#state{sync_state = synced, 
		           group_names = NewSGroupNames,
                           own_grps = NewOwnSGroups,
			   other_grps = NewOtherSGroups,
			   %% Remove newly added nodes from sync
                           nodes = S#state.nodes -- NewNodes,
                           no_contact = lists:usort(NNC ++ S#state.no_contact -- NS),
                           sync_error = lists:usort(NSE ++ S#state.sync_error -- NS)
			   %%monitor = S#state.monitor,
                          },
            ?debug({"s_group_handle_new_s_group_NewS", NewS}),
	    _UpdateConfigFile = is_rsconfig(NewS),
    	    spawn(?MODULE, connect_s_group_nodes, [Nodes--[node()]]),

            %% Fire the appropriate probes.
            case dyntrace:available() of
                true  ->
                      dyntrace:put_tag("s_groups"),
                      %% Fire the user probe 0 once for the creation of the s_group.
                      dyntrace:pn(0, SGroupName),
                      %% Fire the user probe 2 once for each newly added node.
                      lists:foreach(fun(Node) -> dyntrace:pn(2, SGroupName, atom_to_list(Node)) end, Nodes);
                false ->
                      ok
            end,

            {reply, {ok, SGroupName, Nodes}, NewS}
    end;


handle_call({new_s_group_check, Node, _PubType, SGroupName, Nodes}, _From, S) ->
    ?debug({"new_s_group_check_Node_SGroupName_Nodes", Node, SGroupName, Nodes}),
    case lists:member(SGroupName, S#state.group_names) of
        true ->
	     {reply, not_agreed, S};
        false ->
	     NewNodes = Nodes -- [node()],
             RenewSyncNodes =
	         case application:get_env(kernel, s_groups) of
                      undefined ->
		            NoGroupNodes = global:get_known_s_group('no_group'),
			    disconnect_nodes(NoGroupNodes -- [Node]),
    	   	      	    ?debug({"new_s_group_check_NoGroupNodes", NoGroupNodes}),
			    [];
	              _ ->
	   	            ConnectedNodes = NewNodes -- (NewNodes -- nodes(connected)),
    	   	      	    ?debug({"s_group_handle_new_s_group_check_ConnectedNodes",
		                                                     ConnectedNodes}),
			    disconnect_nodes(ConnectedNodes -- [Node]),
                            ConnectedNodes
               end,

    	     NewConf = mk_new_s_group_tuples(SGroupName, Nodes),
    	     application:set_env(kernel, s_groups, NewConf),

   	     NewSGroupNames = lists:sort([SGroupName | S#state.group_names]),
    	     NewOwnSGroups = [{SGroupName, Nodes} | S#state.own_grps],
    	     NewOtherSGroups =
	              case lists:keyfind(SGroupName, 1, S#state.other_grps) of
    		          false -> S#state.other_grps;
			  _ -> lists:keydelete(SGroupName, 1, S#state.other_grps)
		      end,

	     %% Send s_group info update to the own global_name_server
	     global_name_server ! {own_s_group_update, SGroupName, Nodes},

	     NewOwnNodes = lists:usort(lists:append([Nds || {_G, Nds} <- NewOwnSGroups])),
	     update_publish_nodes(S#state.publish_type, {normal, NewOwnNodes}),

             ?debug({"s_group_handle_new_s_group_check_S", S}),
    	     NewS= S#state{sync_state = synced,
    	                   group_names = NewSGroupNames,
    		           own_grps = NewOwnSGroups,
		           other_grps = NewOtherSGroups,
                           nodes = S#state.nodes -- RenewSyncNodes,
                           no_contact=lists:delete(Node, S#state.no_contact),
                  	   sync_error = lists:delete(Node, S#state.sync_error)
		  	   %%monitor = S#state.monitor
		  	  },
             ?debug({"s_group_handle_new_s_group_check_NewS", NewS}),
	     _UpdateConfigFile = is_rsconfig(NewS),
    	     spawn(?MODULE, connect_s_group_nodes, [Nodes--[node()]]),
    	     {reply, agreed, NewS}
    end;


%%%======================================================================
%%% delete_s_group(SGroupName) -> 'ok' | {error, Reason}.
%%%
%%% Delete an s_group
%%%======================================================================
handle_call({delete_s_group, SGroupName}, _From, S) ->
    ?debug({delete_s_group, SGroupName}),
    case lists:keyfind(SGroupName, 1, S#state.own_grps) of 
        false ->
            {reply, {error, {"remote_s_group", SGroupName}}, S};
	{SGroupName, SGroupNodes} ->
	    %% Delete s_group SGroupName
	    NewSGroupNames = lists:delete(SGroupName, S#state.group_names),
	    case NewSGroupNames of
	        [] ->
		    %% The node becomes free
		    application:unset_env(kernel, s_groups),

		    NewSyncState = no_conf,
	    	    NewNodes = [],
	    	    NewNoContact = [],
	    	    NewSyncError = [],
	    	    NewOwnSGroups = [],
	    	    NewOtherSGroups = [];
		_ ->
		    %% The node remains an s_group node
		    NewConf = dlt_from_s_group_tuples(SGroupName),
	    	    application:set_env(kernel, s_groups, NewConf),

		    NewSyncState = synced,
	    	    NewOwnSGroups = lists:keydelete(SGroupName, 1, S#state.own_grps),
	    	    NewOtherSGroups = S#state.other_grps,
	    	    NewNodes = lists:usort(lists:append([Nds || {_, Nds} <- NewOwnSGroups])),
	    	    NewNoContact = [Nds || Nds1 <- NewNodes, Nds <- S#state.no_contact, Nds1==Nds],
	    	    NewSyncError = [Nds || Nds1 <- NewNodes, Nds <- S#state.sync_error, Nds1==Nds]
	    end,

	    DelArgs = [node(), SGroupName],
	    ?debug({"DelArgs:", DelArgs}),
	    lists:foreach(fun(Node) ->	%% NC?
                        rpc:call(Node, s_group, delete_s_group_check, DelArgs)
            end, SGroupNodes--[node()]),

	    %% Disconnect nodes
	    case NewSGroupNames of
	        [] ->
		    NodesToDisconnect = nodes(connected),
		    disconnect_nodes(NodesToDisconnect),

		    update_publish_nodes(S#state.publish_type),

	    	    %% Send s_group info update to the own global_name_server
	    	    global_name_server ! {own_s_group_delete, SGroupName};
		_ ->
 		    %% Send s_group info update to the own global_name_server
    	    	    global_name_server ! {own_s_group_delete, SGroupName},

		    NodesKeepConnected = intersection(SGroupNodes, NewNodes) -- [node()],
		    NodesToDisconnect = (SGroupNodes -- NodesKeepConnected) -- [node()],
		    disconnect_nodes(NodesToDisconnect),
	    	    ?debug({"delete_s_group_check_NodesToDisconnect", NodesToDisconnect}),
	    	    ?debug({"delete_s_group_check_NodesKeepConnected", NodesKeepConnected}),

		    update_publish_nodes(S#state.publish_type, {normal, NewNodes})
	    end,

	    %% New parameters for the node
	    NewS = S#state{sync_state = NewSyncState, 
		           group_names = NewSGroupNames,
                   	   nodes = NewNodes,
		   	   no_contact = NewNoContact,
		   	   sync_error = NewSyncError,
                   	   own_grps = NewOwnSGroups,
		   	   other_grps = NewOtherSGroups
                   	   },
	    _UpdateConfigFile = is_rsconfig(NewS),

            %% Fire the appropriate probes.
            case dyntrace:available() of
                true  ->
                      dyntrace:put_tag("s_groups"),
                      %% Fire the user probe 1 once for the deletion of the s_group.
                      dyntrace:pn(1, SGroupName);
                false ->
                      ok
            end,

    	    {reply, ok, NewS}
    end;

handle_call({delete_s_group_check, InitNode, SGroupName}, _From, S) ->
    ?debug({delete_s_group_check, InitNode, SGroupName}),
    SGroupNodes = case lists:keyfind(SGroupName, 1, S#state.own_grps) of
                      false -> [];
		      {SGroupName, SGNodes} -> SGNodes
                  end,
    NewSGroupNames = lists:delete(SGroupName, S#state.group_names),

    case NewSGroupNames of
        [] ->
	    %% The node becomes free
	    disconnect_nodes(nodes(connected)--[InitNode]),

    	    %% Send s_group info update to the own global_name_server
    	    global_name_server ! {own_s_group_delete, SGroupName},

	    NewSyncState = no_conf,
	    NewNodes = [],
	    NewNoContact = [],
	    NewSyncError = [],
	    NewOwnSGroups = [],
	    NewOtherSGroups = [],

    	    application:unset_env(kernel, s_groups),

	    update_publish_nodes(S#state.publish_type),

	    spawn(?MODULE, connect_free_nodes, [InitNode]);
	_ ->
	    NewConf = dlt_from_s_group_tuples(SGroupName),
	    application:set_env(kernel, s_groups, NewConf),

	    %% Send s_group info update to the own global_name_server
    	    global_name_server ! {own_s_group_delete, SGroupName},

	    %% The node belongs to other s_groups
	    NewSyncState = synced,
	    NewOwnSGroups = lists:keydelete(SGroupName, 1, S#state.own_grps),
	    NewOtherSGroups = S#state.other_grps,
	    NewNodes = lists:usort(lists:append([Nds || {_, Nds} <- NewOwnSGroups])),
	    NewNoContact = [Nds || Nds1 <- NewNodes, Nds <- S#state.no_contact, Nds1==Nds],
	    NewSyncError = [Nds || Nds1 <- NewNodes, Nds <- S#state.sync_error, Nds1==Nds],

	    NodesKeepConnected = intersection(SGroupNodes, NewNodes) -- [node()],
	    NodesToDisconnect = (SGroupNodes -- NodesKeepConnected) -- [InitNode, node()],
	    disconnect_nodes(NodesToDisconnect),
    	    ?debug({"delete_s_group_check_NodesToDisconnect", NodesToDisconnect}),
	    ?debug({"delete_s_group_check_NodesKeepConnected", NodesKeepConnected}),

	    update_publish_nodes(S#state.publish_type, {normal, NewNodes})
    end,

    NewS = S#state{sync_state = NewSyncState, 
		   group_names = NewSGroupNames,
                   nodes = NewNodes,
		   no_contact = NewNoContact,
		   sync_error = NewSyncError,
                   own_grps = NewOwnSGroups,
		   other_grps = NewOtherSGroups
                   },
    ?debug({delete_s_group_check_NewS, NewS}),
    _UpdateConfigFile = is_rsconfig(NewS),
    {reply, agreed, NewS};


%%%======================================================================
%%% add_nodes(SGroupName, Nodes) -> {ok, SGroupName, NewNodes} |
%%%                                 {error, Reason}.
%%% Add nodes to an existing s_group.
%%%======================================================================
handle_call({add_nodes, SGroupName, Nodes}, _From, S) ->
    ?debug({"add_nodes_SGroupName_Nodes", SGroupName, Nodes}),
    case lists:member(SGroupName, S#state.group_names) of 
        false ->
            {reply, {error, node_does_not_belong_to_the_s_group}, S};
        true ->
            {_, SGroupNodes} = lists:keyfind(SGroupName, 1,
	                                     S#state.own_grps),
            case Nodes -- SGroupNodes of 
                [] -> {reply, {ok, SGroupName, Nodes}, S};
                NewNodes0 ->
		    ?debug({"add_nodes_Nodes", Nodes}),
		    ?debug({"add_nodes_NewNodes0", NewNodes0}),
		    NodesToDisConnect = Nodes -- (Nodes -- nodes(connected)),
		    ?debug({"add_nodes_NodesToDisConnect", NodesToDisConnect}),
		    disconnect_nodes(NodesToDisConnect),

		    NewNodes = s_group_conflict(NewNodes0, [SGroupName]),

                    NewSGroupNodes = lists:usort(NewNodes ++ SGroupNodes),
                    NewConf = mk_new_s_group_tuples(SGroupName, NewSGroupNodes),
                    ?debug({"Newconf:", NewConf}),
                    application:set_env(kernel, s_groups, NewConf),

                    NGACArgs = [node(), normal, {SGroupName, NewSGroupNodes}],
                    ?debug({"NGACArgs:",NGACArgs}),
		    %% NS: agreed nodes; NNC: badrpc nodes; NSE: not_agreed nodes
                    {NS, NNC, NSE} =
                        lists:foldl(fun(Node, {NN_acc, NNC_acc, NSE_acc}) -> 
                                            case rpc:call(Node, s_group, add_nodes_check, NGACArgs) of
                                                {badrpc, _} ->
                                                    {NN_acc, [Node | NNC_acc], NSE_acc};
                                                agreed ->
                                                    {[Node | NN_acc], NNC_acc, NSE_acc};
                                                not_agreed ->
                                                    {NN_acc, NNC_acc, [Node | NSE_acc]}
                                            end
                                    end,
                                    {[], [], []}, lists:delete(node(), NewSGroupNodes)),
                    ?debug({"NS_NNC_NSE1:",  {NS, NNC, NSE}}),
		    ?debug({"add_nodes_S#state.nodes_NodesToDisConnect",
			               S#state.nodes, NodesToDisConnect}),

                    %% Send s_group info update to the own global_name_server
	    	    global_name_server ! {own_s_group_update, SGroupName, NewSGroupNodes},

		    NewOwnSGroups = lists:keyreplace(SGroupName, 1, S#state.own_grps,
					                     {SGroupName, NewSGroupNodes}),
		    NewOwnNodes = lists:usort(lists:append([Nds || {_G, Nds} <- NewOwnSGroups])),
		    update_publish_nodes(S#state.publish_type, {normal, NewOwnNodes}),

		    ?debug({"s_group_handle_add_nodes_S", S}),
                    NewS = S#state{sync_state = synced, 
		                   %%group_names = S#state.group_names,
                                   own_grps = NewOwnSGroups,
				   %%other_grps = S#state.other_grps,
				   %% Remove newly added nodes from sync
                                   nodes = S#state.nodes -- NodesToDisConnect,
                                   no_contact = lists:usort(NNC ++ S#state.no_contact -- NS),
                                   sync_error = lists:usort(NSE ++ S#state.sync_error -- NS)
				   %%monitor = S#state.monitor,
                                  },
                    ?debug({"s_group_handle_add_nodes_NewS", NewS}),
	     	    _UpdateConfigFile = is_rsconfig(NewS),
    	    	    spawn(?MODULE, connect_s_group_nodes, [Nodes--[node()]]),

                    %% Fire the appropriate probes.
                    case dyntrace:available() of
                        true  ->
                              dyntrace:put_tag("s_groups"),
                              %% Fire the user probe 2 once for each newly added node.
                              lists:foreach(fun(Node) -> dyntrace:pn(2, SGroupName, atom_to_list(Node)) end, NewNodes);
                        false ->
                              ok
                    end,

                    {reply, {ok, SGroupName, NewNodes}, NewS}
            end
    end;

handle_call({add_nodes_check, Node, _PubType, {SGroupName, Nodes}}, _From, S) ->
    ?debug({{add_nodes_check, Node, _PubType, {SGroupName, Nodes}}, _From, S}),
    SGroupNodes = case lists:keyfind(SGroupName, 1, S#state.own_grps) of 
                             {_, Ns} -> Ns;
                             false -> []
                  end,
    NewNodes = lists:usort(Nodes -- SGroupNodes) -- [node()],
    case application:get_env(kernel, s_groups) of
         undefined ->
	   NoGroupNodes = global:get_known_s_group('no_group'),
	   disconnect_nodes(NoGroupNodes -- [Node]),
    	   ?debug({"add_nodes_check_NoGroupNodes", NoGroupNodes});
	 _ ->
	   ConnectedNodes = NewNodes -- (NewNodes -- nodes(connected)),
    	   disconnect_nodes(ConnectedNodes -- [Node]),
    	   ?debug({"s_group_handle_add_nodes_check_ConnectedNodes", ConnectedNodes})
    end,
    
    NewConf = mk_new_s_group_tuples(SGroupName, Nodes),
    application:set_env(kernel, s_groups, NewConf),

    NewSGroupNames = case lists:member(SGroupName, S#state.group_names) of
                        true -> S#state.group_names;
                        false -> lists:sort([SGroupName | S#state.group_names])
                    end,
    NewOwnSGroups = case lists:keyfind(SGroupName, 1, S#state.own_grps) of
                        false -> [{SGroupName, Nodes} | S#state.own_grps];
                        _ -> lists:keyreplace(SGroupName, 1, S#state.own_grps,
                                             {SGroupName, Nodes})
                    end,
    NewOtherSGroups = case lists:keyfind(SGroupName, 1, S#state.other_grps) of
    		          false -> S#state.other_grps;
			  _ -> lists:keydelete(SGroupName, 1, S#state.other_grps)
		      end,
    %% Send s_group info update to the own global_name_server
    global_name_server ! {own_s_group_update, SGroupName, Nodes},

    NewOwnNodes = lists:usort(lists:append([Nds || {_G, Nds} <- NewOwnSGroups])),
    update_publish_nodes(S#state.publish_type, {normal, NewOwnNodes}),

    ?debug({"s_group_handle_add_nodes_check_S", S}),
    NewS= S#state{sync_state = synced,
    	          group_names = NewSGroupNames,
    		  own_grps = NewOwnSGroups,
		  other_grps = NewOtherSGroups,
                  %%nodes = S#state.nodes,
                  no_contact=lists:delete(Node, S#state.no_contact),
                  sync_error = lists:delete(Node, S#state.sync_error)
		  %%monitor = S#state.monitor
		  },
    ?debug({"s_group_handle_add_nodes_check_NewS", NewS}),
    _UpdateConfigFile = is_rsconfig(NewS),
    spawn(?MODULE, connect_s_group_nodes, [Nodes--[node()]]),
    {reply, agreed, NewS};


%%%======================================================================
%%% remove_nodes(SGroupName, Nodes) -> 'ok' | {error, Reason}.
%%%
%%% Remove nodes from an s_group
%%%======================================================================
handle_call({remove_nodes, SGroupName, NodesToRmv0}, _From, S) ->
    ?debug({"remove_nodes_SGroupName_NodesToRmv0", SGroupName, NodesToRmv0}),
    case lists:member(SGroupName, S#state.group_names) of 
        false ->
            {reply, {error, {"node_does_not_belong_to_s_group", SGroupName}}, S};
        true ->
	    {_, SGroupNodes} = lists:keyfind(SGroupName, 1, S#state.own_grps),
    	    NodesToRmv = [Nd || Nd <- SGroupNodes, Nd0 <- NodesToRmv0, Nd==Nd0],
	    case NodesToRmv of
	        [] ->
		    {reply, 'ok', S};
		_ ->
		    NewSGroupNodes = SGroupNodes -- NodesToRmv,
		    %% Remove NodesToRmv from s_group SGroupName
    		    NewConf = rmv_nodes_from_s_group_tuples(SGroupName, NodesToRmv),
    		    ?debug({"Newconf:", NewConf}),
    		    application:set_env(kernel, s_groups, NewConf),

		    DelArgs = [node(), SGroupName],
    		    ?debug({"DelArgs:", DelArgs}),
         	    lists:foreach(fun(Node) ->
                	        rpc:call(Node, s_group, delete_s_group_check, DelArgs)
         	    end, NodesToRmv),

		    RmvArgs = [SGroupName, NewSGroupNodes, NodesToRmv],
    		    ?debug({"RmvArgs:", RmvArgs}),
    		    %% NS: agreed nodes; NNC: badrpc nodes; NSE: not_agreed nodes
         	    lists:foreach(fun(Node) ->
                	        rpc:call(Node, s_group, remove_nodes_check, RmvArgs)
         	    end, lists:delete(node(), NewSGroupNodes)),
		    ?debug({"remove_nodes_S#state.nodes_Nodes", S#state.nodes, NewSGroupNodes}),

		    NewOwnSGroups = lists:keyreplace(SGroupName, 1, S#state.own_grps,
					                     {SGroupName, NewSGroupNodes}),
		    NewOwnNodes = lists:usort(lists:append([Nds || {_G, Nds} <- NewOwnSGroups])),
		    NodesKeepConnected = intersection(NodesToRmv, NewOwnNodes) -- [node()],
	    	    NodesToDisconnect = (NodesToRmv -- NodesKeepConnected) -- [node()],
    	    	    ?debug({"delete_s_group_check_NodesToDisconnect", NodesToDisconnect}),
	    	    ?debug({"delete_s_group_check_NodesKeepConnected", NodesKeepConnected}),

		    %% Send s_group info update to the own global_name_server
		    global_name_server ! {own_s_group_remove, SGroupName,
		                                              NewSGroupNodes, NodesKeepConnected},

		    %% Disconnect NodesToRmv
	    	    disconnect_nodes(NodesToDisconnect),

		    update_publish_nodes(S#state.publish_type, {normal, NewOwnNodes}),

		    NewS = S#state{sync_state = synced, 
	  	                   %%group_names = S#state.group_names,
               	    		   own_grps = NewOwnSGroups,
                                   %%other_grps = S#state.other_grps,
               	    		   nodes = S#state.nodes--NodesToRmv
               	    		   %%no_contact = S#state.no_contact,
               	    		   %%sync_error = S#state.sync_error
	   	    		   %%monitor = S#state.monitor,
               	    		   },
	     	    _UpdateConfigFile = is_rsconfig(NewS),

                    %% Fire the appropriate probes.
                    case dyntrace:available() of
                        true  ->
                              dyntrace:put_tag("s_groups"),
                              %% Fire the user probe 3 once for each removed node.
                              lists:foreach(fun(Node) -> dyntrace:pn(3, SGroupName, atom_to_list(Node)) end, NodesToRmv);
                        false ->
                              ok
                    end,

		    {reply, 'ok', NewS}
	    end
    end;

     
handle_call({remove_nodes_check, SGroupName, NewSGroupNodes, NodesToRmv}, _From, S) ->
    ?debug({remove_nodes_check, SGroupName, NewSGroupNodes, NodesToRmv}),

    NewConf = rmv_nodes_from_s_group_tuples(SGroupName, NodesToRmv),
    application:set_env(kernel, s_groups, NewConf),

    %% Send s_group info update to the own global_name_server
    global_name_server ! {own_s_group_update, SGroupName, NewSGroupNodes},

    %% Disconnect from NodesToRmv
    disconnect_nodes(NodesToRmv),
    
    NewOwnSGroups = lists:keyreplace(SGroupName, 1, S#state.own_grps,
		                     {SGroupName, NewSGroupNodes}),
    NewNodes0 = lists:usort(lists:append([Nds || {_, Nds} <- NewOwnSGroups])),
    NewNodes = (NewNodes0 -- S#state.no_contact) -- S#state.sync_error,
    NewNoContact = [Nds || Nds1 <- NewNodes, Nds <- S#state.no_contact, Nds1==Nds],
    NewSyncError = [Nds || Nds1 <- NewNodes, Nds <- S#state.sync_error, Nds1==Nds],

    NewOwnNodes = lists:usort(lists:append([Nds || {_G, Nds} <- NewOwnSGroups])),
    update_publish_nodes(S#state.publish_type, {normal, NewOwnNodes}),

    NewS = S#state{sync_state = synced, 
                   %%group_names = S#state.group_names,
    		   own_grps = NewOwnSGroups,
                   %%other_grps = S#state.other_grps,
                   nodes = NewNodes,
                   no_contact = NewNoContact,
                   sync_error = NewSyncError
		   %%monitor = S#state.monitor,
                   },
    _UpdateConfigFile = is_rsconfig(NewS),
    {reply, agreed, NewS};


%%%======================================================================
%%% choose_nodes(Arg) -> [Node].
%%%
%%% Choose nodes that satisfy the given argument
%%%======================================================================
handle_call({choose_nodes, Arg}, _From, S) ->
    Nodes =
        case Arg of
            {s_group, SGroupName} ->
	        case lists:keyfind(SGroupName, 1, S#state.own_grps) of 
	            {SGroupName, SGroupNodes} ->
		        SGroupNodes -- [node()];
		    _ ->
		        []
	        end;
	    {attributes, Attribs0} ->
	        Attribs = lists:usort(Attribs0),
	        AttribNodes = nodes(connected),
    		NodeAttribs = lists:foldl(fun(Node, NN_cc) ->
                          NAt = rpc:call(Node, global, registered_attributes, []),
			  case is_list(NAt) of
                              true ->
                      	          [{Node, NAt} | NN_cc];
                              _ ->
                      	          NN_cc
		          end
                end, [], AttribNodes),
		?debug({"choose_nodes_NodeAttribs", NodeAttribs}),

		%% From NodeAttribs pick only nodes that have all Attribs
		OverlapAttribs = [{N, At--(At--Attribs)} || {N, At} <- NodeAttribs],
		[N || {N, At} <- OverlapAttribs, At==Attribs];
	    _ ->
	        []
        end,
    {reply, Nodes, S};


%%%======================================================================
%%% add_attributes(Args) -> ok | {error, Reason}
%%% add_attributes(Nodes, Args) -> ok | {error, Reason}.
%%% remove_attributes(Args) -> ok
%%% remove_attributes(Nodes, Args) -> ok
%%%
%%% Adding attribute to, and removing attributes from, a node or a list of Nodes
%%%======================================================================
handle_call({add_attributes, Args}, _From, S) ->
    NewArgs = lists:usort(S#state.attributes++Args),
    {reply, ok, S#state{attributes = NewArgs}};

handle_call({add_attributes, Nodes, Args}, _From, S) ->
    NewNodes = lists:foldl(fun(Node, NN_cc) ->
                          case rpc:call(Node, s_group, add_attributes, [Args]) of
                              ok ->
                      	          [Node | NN_cc];
                              _ ->
                      	          NN_cc
		          end
               end, [], Nodes),
    {reply, NewNodes, S};

handle_call({remove_attributes, Args}, _From, S) ->
    NewArgs = S#state.attributes--Args,
    {reply, ok, S#state{attributes = NewArgs}};

handle_call({remove_attributes, Nodes, Args}, _From, S) ->
    NewNodes = lists:foldl(fun(Node, NN_cc) ->
                          case rpc:call(Node, s_group, remove_attributes, [Args]) of
                              ok ->
                      	          [Node | NN_cc];
                              _ ->
                      	          NN_cc
		          end
               end, [], Nodes),
    {reply, NewNodes, S};

handle_call(registered_attributes, _From, S) ->
    {reply, S#state.attributes, S};


%%%====================================================================================
%%% Misceleaneous help function to read some variables
%%%====================================================================================
handle_call(info, _From, S) ->    
    Reply = [{state,            S#state.sync_state},
	     {own_group_names,  S#state.group_names},
	     {own_group_nodes,  get_own_nodes()},
	     {synced_nodes,     S#state.nodes},
	     {sync_error,       S#state.sync_error},
	     {no_contact,       S#state.no_contact},
             {own_s_groups,     S#state.own_grps},
	     {other_groups,     S#state.other_grps},
	     {monitoring,       S#state.monitor},
	     {attributes,       S#state.attributes}],
    %%Reply1 = S,     
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
    {reply, {illegal_message, Call}, S}.


%%%====================================================================================
%%% registered_names({node, Node}) -> [Name] | {error, ErrorMessage}
%%% registered_names({s_group, SGroupName}) -> [Name] | {error, ErrorMessage}
%%%
%%% Get a list of nodes on a Node or in the known s_group
%%%====================================================================================
handle_cast({registered_names, User}, S) ->
    Res = global:registered_names(all_names),
    User ! {registered_names_res, Res},
    {noreply, S};

handle_cast({registered_names_res, SGroupName, Result0, Pid, From}, S) ->
    Result = case SGroupName of
    	         undefined ->
	    	     Result0;
	 	 _ ->
	    	     s_group_names(Result0, SGroupName)
             end,
    unlink(Pid),
    Pid ! kill,
    Wait = get(registered_names),
    NewWait = lists:delete({Pid, From},Wait),
    put(registered_names, NewWait),
    gen_server:reply(From, Result),
    {noreply, S};


%%%====================================================================================
%%% send(SGroupName, Name, Msg) -> Pid | {error, Reason}
%%% send(Node, SGroupName, Name, Msg) -> Pid | {error, Reason}
%%%
%%% The registered Name is found; send the message to it, kill the search process,
%%% and return to the requesting process.
%%%====================================================================================
handle_cast({send_res, Result, SGroupName, Name, Msg, Pid, From}, S) ->
    case Result of
	{badarg,{SGroupName, Name, Msg}} ->
	    continue;
	ToPid ->
	    ToPid ! Msg
    end,
    unlink(Pid),
    Pid ! kill,
    Wait = get(send),
    NewWait = lists:delete({Pid, From, SGroupName, Name, Msg},Wait),
    put(send, NewWait),
    gen_server:reply(From, Result),
    {noreply, S};


%%%====================================================================================
%%% A request from a search process to check if this Name is registered at this node.
%%%====================================================================================
handle_cast({find_name, User, Name}, S) ->
    Res = global:whereis_name(Name),
    User ! {find_name_res, Res},
    {noreply, S};

handle_cast({find_name, User, SGroupName, Name}, S) ->
    Res = global:whereis_name(SGroupName, Name),
    User ! {find_name_res, Res},
    {noreply, S};


%%%====================================================================================
%%% whereis_name(SGroupName, Name) -> Pid | undefined
%%% whereis_name(Node, SGroupName, Name) -> Pid | undefined
%%%
%%% The registered Name is found; kill the search process
%%% and return to the requesting process.
%%%====================================================================================
handle_cast({find_name_res, Result, Pid, From}, S) ->
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
    kill_s_group_check(),
    Nodes = get_own_nodes() -- [node() | NoContact],
    ?debug({"s_group_handle_synced_Nodes", Nodes}),
    {noreply, S#state{nodes = lists:sort(Nodes),
		      sync_error = [],
		      no_contact = NoContact}};


%%%====================================================================================
%%% The node could not sync with some other nodes.
%%%====================================================================================
handle_cast({sync_error, NoContact, ErrorNodes}, S) ->
    Txt = io_lib:format("Global group: Could not synchronize with these nodes ~p~n"
			"because s_groups were not in agreement. ~n", [ErrorNodes]),
    error_logger:error_report(Txt),
    %%?debug(lists:flatten(Txt)),
    kill_s_group_check(),
    Nodes = (get_own_nodes() -- [node() | NoContact]) -- ErrorNodes,
    {noreply, S#state{nodes = lists:sort(Nodes), 
		      sync_error = ErrorNodes,
		      no_contact = NoContact}};


%%%====================================================================================
%%% Another node is checking this node's group configuration
%%%====================================================================================
%%handle_cast({conf_check, Vsn, Node, From, sync, CmnGroups}, S) ->
%%    ?debug({"s_group_handle_conf_check", Vsn, Node, From, sync, CmnGroups}),
%%    handle_cast({conf_check, Vsn, Node, From, sync, CmnGroups}, S);

handle_cast({conf_check, Vsn, Node, From, sync, CmnGroups}, S) ->
    ?debug({"s_group_handle_conf_check", Vsn, Node, From, sync, CmnGroups}),
    CurNodes = S#state.nodes,
    ?debug({"s_group_handle_conf_check_CurNodes", CurNodes}),
    %% Another node is syncing, 
    %% done for instance after upgrade of s_group parameters
    NS = 
	case application:get_env(kernel, s_groups) of
	    undefined ->
		%% The current node doesn't an s_group definition
		update_publish_nodes(S#state.publish_type),
		disconnect_nodes([Node]),
		{s_group_check, Node} ! {config_error, Vsn, From, node()},
		S;
	    {ok, []} ->
		%% The current node s_group definition is empty
		update_publish_nodes(S#state.publish_type),
		disconnect_nodes([Node]),
		{s_group_check, Node} ! {config_error, Vsn, From, node()},
		S;
	    %%---------------------------------
	    %% s_groups are defined
	    %%---------------------------------
	    {ok, GroupNodes} ->
                case config_scan(GroupNodes, publish_type) of
		    {error, _Error2} ->
			%% The current node s_group definition is faulty
			disconnect_nodes([Node]),
			{s_group_check, Node} ! {config_error, Vsn, From, node()},
			S#state{nodes = lists:delete(Node, CurNodes)};
                    {ok, OwnSGroups, _OtherSGroups} ->
		        %% OwnSGroups::[{SGroupName, PubType, Nodes}]
                        ?debug({"s_group_handle_conf_check_CmnGroups_OwnSGroups",
				                   CmnGroups, OwnSGroups}),
			{OwnCmnGroups, S1} =
			    handle_conf_check(CmnGroups, [], OwnSGroups, Node, CurNodes, S),
			%% OwnCmnGroups::[GroupName]
			case OwnCmnGroups of
                            [] ->
                                %% node_group definitions were not in agreement
                  		disconnect_nodes([Node]),
                  		{s_group_check, Node} ! {config_error, Vsn, From, node()},
                  		NN = lists:delete(Node, S1#state.nodes),
                  		NSE = lists:delete(Node, S1#state.sync_error),
                  		NNC = lists:delete(Node, S1#state.no_contact),
                  		S1#state{nodes = NN,
                          	         sync_error = NSE,
                          		 no_contact = NNC};
			    _ ->
			        ?debug({"s_group_handle_conf_check_OwnCmnGroups_Node",
				                           OwnCmnGroups, Node}), 
                		global_name_server ! {nodeup_s, OwnCmnGroups, Node},
                  		{s_group_check, Node} !
				       {config_s_ok, Vsn, From, OwnCmnGroups, node()},
				S1
                        end
                end
        end,
    {noreply, NS};

handle_cast(_Cast, S) ->
    {noreply, S}.
    

%%%====================================================================================
%%% A node went down. If no global group configuration inform global;
%%% if global group configuration inform global only if the node is one in
%%% the own global group.
%%%====================================================================================
handle_info({nodeup, Node}, S) when S#state.sync_state =:= no_conf ->
    case application:get_env(kernel, s_groups) of 
        undefined ->
            ?debug({"NodeUp:", node(), Node}),
            send_monitor(S#state.monitor, {nodeup, Node}, S#state.sync_state),
	    ?debug({"S", S}),
            global_name_server ! {nodeup, no_group, Node},
            {noreply, S};
        _ ->
         handle_node_up(Node,S)
    end;
handle_info({nodeup, Node}, S) ->
    ?debug({"NodeUp:",  node(), Node}),
    handle_node_up(Node, S);

handle_info({nodeup, no_group, Node}, S) ->
    case application:get_env(kernel, s_groups) of 
        undefined ->
            ?debug({"NodeUp:", node(), Node}),
            send_monitor(S#state.monitor, {nodeup, Node}, S#state.sync_state),
	    ?debug({"S", S}),
            global_name_server ! {nodeup, no_group, Node},
            {global_name_server, Node} ! {nodeup, no_group, node()},
            {noreply, S};
        _ ->
         {noreply, S}
    end;

%%%====================================================================================
%%% Updating s_group information in the node .config file
%%%====================================================================================

handle_info(record_state, S) ->
    ?debug({"Record state:", node(), S}),
    ConfigMsg = {calendar:local_time(), S},
    case whereis(rsconfig) of
        undefined ->
	    RsconfigPid = spawn(?MODULE, record_state, []),
	    register(rsconfig, RsconfigPid);
	RsconfigPid ->
	    true
    end,
    RsconfigPid ! ConfigMsg,
    {noreply, S};

%%%====================================================================================
%%% A node has crashed. 
%%% nodedown must always be sent to global; this is a security measurement
%%% because during release upgrade the s_groups parameter is upgraded
%%% before the node is synced. This means that nodedown may arrive from a
%%% node which we are not aware of.
%%%====================================================================================
handle_info({nodedown, Node}, S) when S#state.sync_state =:= no_conf ->
    send_monitor(S#state.monitor, {nodedown, Node}, S#state.sync_state),
    global_name_server ! {nodedown, Node},
    {noreply, S};
handle_info({nodedown, Node}, S) ->
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
%%% A node has changed its s_groups definition, and is telling us that we are not
%%% included in his group any more. This could happen at release upgrade.
%%%====================================================================================
handle_info({disconnect_node, Node}, S) ->
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
    {noreply, S}.

handle_node_up(Node, S) ->
    OthersNG = case S#state.sync_state==no_conf andalso 
                   application:get_env(kernel, s_groups)==undefined of 
                   true -> 
                       [];
                   false ->
                       X = (catch rpc:call(Node, s_group, get_own_s_groups_with_nodes, [])),
		       case X of
			   X when is_list(X) ->
			       X;
			   _ ->
			       []
		       end
               end,
    OwnNGs = get_own_s_groups_with_nodes(),
    OwnGroups = element(1, lists:unzip(OwnNGs)),
    NNC = lists:delete(Node, S#state.no_contact),
    NSE = lists:delete(Node, S#state.sync_error),
    case shared_s_groups_match(OwnNGs, OthersNG) of 
        true->
            OthersGroups = element(1, lists:unzip(OthersNG)),
            CmnGroups = intersection(OwnGroups, OthersGroups),
            send_monitor(S#state.monitor, {nodeup, Node}, S#state.sync_state),
            ?debug({s_group_nodeup_handle_node_up, OwnGroups, Node, CmnGroups}),
	    global_name_server ! {nodeup_s, CmnGroups, Node},
	    case lists:member(Node, S#state.nodes) of
		false ->
		    NN = lists:sort([Node | S#state.nodes]),
		    {noreply, S#state{
                                sync_state = synced,
                                group_names = OwnGroups,
                                nodes = NN, 
                                no_contact = NNC,
                                sync_error = NSE}};
		true ->
		    {noreply, S#state{
                                sync_state = synced,
                                group_names = OwnGroups,
                                no_contact = NNC,
                                sync_error = NSE}}
	    end;
	false ->
            case {lists:member(Node, get_own_nodes()), 
		  lists:member(Node, S#state.sync_error)} of
		{true, false} ->
		    NSE2 = lists:sort([Node | S#state.sync_error]),
		    {noreply, S#state{
                                sync_state = synced,
                                group_names = OwnGroups,
                                no_contact = NNC,
                                sync_error = NSE2}};
                _ ->
                    {noreply, S#state{sync_state=synced,
                                      group_names = OwnGroups}}
	    end
    end.

%%%====================================================================================
%%% Extra functions
%%%====================================================================================
shared_s_groups_match(OwnSGroups, OthersSGroups) ->
    OwnSGroupNames = [G || {G, _Nodes} <- OwnSGroups],
    OthersSGroupNames = [G || {G, _Nodes} <- OthersSGroups],
    SharedSGroups = intersection(OwnSGroupNames, OthersSGroupNames),
    case SharedSGroups of 
        [] -> false;
        Gs ->
            Own =[{G, lists:sort(Nodes)}
                  || {G, Nodes} <- OwnSGroups, lists:member(G, Gs)],
            Others= [{G, lists:sort(Nodes)}
                     || {G, Nodes} <- OthersSGroups, lists:member(G, Gs)],
            lists:sort(Own) == lists:sort(Others)
    end.

intersection(_, []) -> 
    [];
intersection(L1, L2) ->
    L1 -- (L1 -- L2).

terminate(_Reason, _S) ->
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%====================================================================================
%%% Check the global group configuration.
%%%====================================================================================
-spec config_scan(NodeSGroups) -> {ok, OwnSGroups, OtherSGroups} |
                                 {error, Reason} when
    NodeSGroups :: [group_tuple()],
    OwnSGroups :: [SGroup],
    OtherSGroups :: [SGroup],
    SGroup :: {SGroupName, Nodes},
    SGroupName :: group_name(),
    Nodes :: [node()],
    Reason :: term().
config_scan(NodeGrps) ->
    config_scan(NodeGrps, original).

config_scan(NodeGrps, original) ->
     config_scan(NodeGrps, publish_type);

config_scan(NodeGrps, publish_type) ->
    config_scan(node(), NodeGrps, [], []).

config_scan(_MyNode, [], MyOwnNodeGrps, OtherNodeGrps) ->
    {ok, MyOwnNodeGrps, OtherNodeGrps};
config_scan(MyNode, [GrpTuple|NodeGrps], MyOwnNodeGrps, OtherNodeGrps) ->
    {GrpName, PubTypeGroup, Nodes} = grp_tuple(GrpTuple),
    case lists:member(MyNode, Nodes) of
	true ->
            config_scan(MyNode, NodeGrps, 
                        [{GrpName, PubTypeGroup,lists:sort(Nodes)}
                         |MyOwnNodeGrps], 
                        OtherNodeGrps);
	false ->
	    config_scan(MyNode,NodeGrps, MyOwnNodeGrps,
                        [{GrpName, lists:sort(Nodes)}|
                         OtherNodeGrps])
    end.

grp_tuple({Name, Nodes}) ->
    {Name, normal, Nodes};
%grp_tuple({Name, hidden, Nodes}) ->
%    {Name, hidden, Nodes};
grp_tuple({Name, normal, Nodes}) ->
    {Name, normal, Nodes}.

    
%%%====================================================================================
%%% The special process which checks that all nodes in the own global group
%%% agrees on the configuration.
%%%====================================================================================
-spec sync_init(_, _, _, _) -> no_return().
sync_init(Type, _Cname, PubType, SGroupNodesPairs) ->
    ?debug({"sync_int:", Type, _Cname, PubType, SGroupNodesPairs}),
    NodeGroupPairs = ng_pairs(SGroupNodesPairs),
    ?debug({"sync_init_NodeGroupPairs", NodeGroupPairs}),
    Nodes = lists:usort(element(1,lists:unzip(NodeGroupPairs))),
    ?debug({"sync_init_node()_Nodes:", node(), Nodes}),
    {Up, Down} = sync_check_node(lists:delete(node(), Nodes), [], []),
    ?debug({"sync_init_Up_Down:", Up, Down}),
    sync_check_init(Type, Up, NodeGroupPairs, Down, PubType).

sync_check_node([], Up, Down) ->
    {Up, Down};
sync_check_node([Node|Nodes], Up, Down) ->
    case net_adm:ping(Node) of
	pang ->
	    sync_check_node(Nodes, Up, [Node|Down]);
	pong ->
	    sync_check_node(Nodes, [Node|Up], Down)
    end.

%%%====================================================================================
%%% Converting GroupTup = [{GroupName, Nodes}] in
%%% NodeGroupPairs = [{Node, GroupNames}]
%%%====================================================================================
ng_pairs(GroupTup) ->
    NGPairs = lists:append([[{Node, GroupName} || Node<-Nodes]
    	      		     || {GroupName, Nodes} <- GroupTup]),
    ng_pairs(NGPairs, []).

ng_pairs([], NodeTup) ->
    NodeTup;
ng_pairs([{Node, GroupName} | RemPairs], NodeTup) ->
    NewNodeTup = case lists:keyfind(Node, 1, NodeTup) of
        false ->
	      [{Node, [GroupName]} | NodeTup];
	{Node, GroupNames} ->
	      lists:keyreplace(Node, 1, NodeTup, {Node, [GroupName | GroupNames]})
    end,
    ng_pairs(RemPairs, NewNodeTup).


%%%====================================================================================
%%% Check that all nodes are in agreement with the s_group configuration.
%%%====================================================================================
-spec sync_check_init(_, _, _, _, _) -> no_return().
sync_check_init(Type, Up, NodeGroupPairs, Down, PubType) ->
    sync_check_init(Type, Up, NodeGroupPairs, 3, [], Down, PubType).

-spec sync_check_init(_, _, _, _, _, _,  _) -> no_return().
sync_check_init(_Type, NoContact, _NodeGroupPairss, 0,
                ErrorNodes, Down, _PubType) ->
    case ErrorNodes of
	[] -> 
	    gen_server:cast(?MODULE, {synced, lists:sort(NoContact ++ Down)});
	_ ->
	    gen_server:cast(?MODULE, {sync_error, lists:sort(NoContact ++ Down),
					   ErrorNodes})
    end,
    receive
	kill ->
	    exit(normal)
    after 5000 ->
	    exit(normal)
    end;

sync_check_init(Type, Up, NodeGroupPairs, N, ErrorNodes, Down, PubType) ->
    ?debug({"sync_check_init_Type_Up_NodeGroupPairs_N_ErrorNodes_Down_PubType",
                      Type, Up, NodeGroupPairs, N, ErrorNodes, Down, PubType}),
    lists:foreach(fun(Node) ->
                          {Node, Groups} = lists:keyfind(Node, 1, NodeGroupPairs),
                          %%GroupNodes = gn_pairs(Node, NodeGroupPairs),
                          ConfCheckMsg = {conf_check, ?cc_vsn, node(),
			  	          self(), Type, Groups},
                          ?debug({sync_check_init_conf_check, Node, ConfCheckMsg}),
                          gen_server:cast({?MODULE, Node}, ConfCheckMsg)
		  end, Up),
    case sync_check(Up) of
	{ok, synced} ->
	    sync_check_init(Type, [], NodeGroupPairs, 0,
                            ErrorNodes, Down, PubType);
	{error, NewErrorNodes} ->
	    sync_check_init(Type, [], NodeGroupPairs, 0,
                            ErrorNodes ++ NewErrorNodes, Down, PubType);
	{more, Rem, NewErrorNodes} ->
	    %% Try again to reach the s_group, 
	    %% obviously the node is up but not the s_group process.
	    sync_check_init(Type, Rem, NodeGroupPairs, N - 1,
                            ErrorNodes ++ NewErrorNodes, Down, PubType)
    end.

%%gn_pairs(Node, NodeGroupPairs) ->
%%	{Node, Groups} = lists:keyfind(Node, 1, NodeGroupPairs),
%%	InitGroupNodePairs = [{G, [Node]} || G <- Groups],
%%	NodeGroupPairs1 = lists:delete({Node, Groups}, NodeGroupPairs),
%%	NodeGroupList = ng_list(NodeGroupPairs1, []),
%%	gn_pairs_do(InitGroupNodePairs, NodeGroupList, []).

%%ng_list([], NodeGroupList) ->
%%	NodeGroupList;
%%ng_list([{Node, Groups} | NodeGroupPairs], NodeGroupList) ->
%%	NewNodeGroupList = [{Node, G} || G <- Groups],
%%	ng_list(NodeGroupPairs, NodeGroupList++NewNodeGroupList).

%%gn_pairs_do([], _NodeGroupPairs, GroupNodePairs) ->
%%	lists:sort(GroupNodePairs);
%%gn_pairs_do([{Group, Node} | InitGroupNodePairs], NodeGroupPairs, GroupNodePairs) ->
%%	Nodes1 = [Node1 || {Node1, G} <- NodeGroupPairs, G==Group],
%%	NewNodes = lists:sort(Node ++ Nodes1),
%%	gn_pairs_do(InitGroupNodePairs, NodeGroupPairs, [{Group, NewNodes} | GroupNodePairs]).


handle_conf_check([], OwnCmnGroups, _OwnSGroups, _Node, _CurNodes, S) ->
    {OwnCmnGroups, S};
handle_conf_check([CmnGroup | CmnGroups], OwnCmnGroups, OwnSGroups, Node, CurNodes, S) ->
    case lists:keyfind(CmnGroup, 1, OwnSGroups) of
        {CmnGroup, PubType, CmnNodes} ->
	     S1 = handle_conf_check1(PubType, CmnNodes, Node, CurNodes, S),
    	     handle_conf_check(CmnGroups, [CmnGroup | OwnCmnGroups],
	                       OwnSGroups, Node, CurNodes, S1);
	_ ->
             handle_conf_check(CmnGroups, OwnCmnGroups, OwnSGroups, Node, CurNodes, S)
    end.

handle_conf_check1(PubType, CmnNodes, Node, CurNodes, S) ->
    %% Add Node to the #state.nodes if it isn't there
    update_publish_nodes(S#state.publish_type, {PubType, CmnNodes}),
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
    end.
 

sync_check(Up) ->
    sync_check(Up, Up, []).

sync_check([], _Up, []) ->
    {ok, synced};
sync_check([], _Up, ErrorNodes) ->
    {error, ErrorNodes};
sync_check(Rem, Up, ErrorNodes) ->
    receive
	{config_ok, ?cc_vsn, Pid, GroupName, Node} when Pid =:= self() ->
	    global_name_server ! {nodeup, GroupName, Node},
	    ?debug({"s_group_nodeup_sync_check", nodeup, GroupName, Node}),
	    sync_check(Rem -- [Node], Up, ErrorNodes);
	{config_s_ok, ?cc_vsn, Pid, CmnGroups, Node} when Pid =:= self() ->
	    global_name_server ! {nodeup_s, CmnGroups, Node},
	    ?debug({"s_group_nodeup_sync_check_s", CmnGroups, Node}),
	    sync_check(Rem -- [Node], Up, ErrorNodes);
	{config_error, ?cc_vsn, Pid, Node} when Pid =:= self() ->
	    sync_check(Rem -- [Node], Up, [Node | ErrorNodes]);
	{no_s_group_configuration, ?cc_vsn, Pid, Node} when Pid =:= self() ->
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
	    unlink(Pid)
    end.


%%%====================================================================================
%%% Send a nodeup/down messages to monitoring Pids in the own global group.
%%%====================================================================================
send_monitor([P|T], M, no_conf) -> safesend_nc(P, M), send_monitor(T, M, no_conf);
send_monitor([P|T], M, SyncState) -> safesend(P, M), send_monitor(T, M, SyncState);
send_monitor([], _, _) -> ok.

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
%%% Check which user is associated with the crashed process.
%%%====================================================================================
check_exit(ExitPid, Reason) ->
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
%%% Kill any possible s_group_check processes
%%%====================================================================================
kill_s_group_check() ->
    case whereis(s_group_check) of
	undefined ->
	    ok;
	Pid ->
	    unlink(Pid),
	    s_group_check ! kill,
	    unregister(s_group_check)
    end.


%%%====================================================================================
%%% Disconnect nodes not belonging to own s_groups
%%% The function will keep all links with remote processes
%%%====================================================================================
disconnect_nodes(DisconnectNodes) ->
    lists:foreach(fun(Node) ->
			  {s_group, Node} ! {disconnect_node, node()},
			  global:node_disconnected(Node)
		  end,
		  DisconnectNodes).

%%%====================================================================================
%%% Disconnect nodes not belonging to own s_groups
%%%====================================================================================
force_nodedown(DisconnectNodes) ->
    lists:foreach(fun(Node) ->
			  erlang:disconnect_node(Node),
			  global:node_disconnected(Node)
		  end,
		  DisconnectNodes).

%%%====================================================================================
%%% Get the current s_groups definition
%%%====================================================================================
get_own_nodes_with_errors() ->
    case application:get_env(kernel, s_groups) of
	undefined ->
	    {ok, all};
	{ok, []} ->
	    {ok, all};
	{ok, NodeGrps} ->
            case catch config_scan(NodeGrps, publish_type) of
		{error, Error} ->
		    {error, Error};
                {ok, OwnSGroups, _} ->
                    Nodes = lists:append([Nodes||{_, _, Nodes}<-OwnSGroups]),
                    {ok, lists:usort(Nodes)}
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


get_own_s_groups_with_nodes() ->
    case application:get_env(kernel, s_groups) of
	undefined ->
	    [];
	{ok, []} ->
	    [];
	{ok, NodeGrps} ->
            case catch config_scan(NodeGrps, publish_type) of
                {error,_Error} ->
                    [];
                {ok, OwnSGroups, _} ->
                    [{Group, Nodes} || {Group, _PubType, Nodes} <- OwnSGroups]
            end
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
    case application:get_env(kernel, s_groups) of
	undefined ->
	    no_group;
	{ok, []} ->
	    no_group;
	{ok, NodeGrps} ->
	    case catch config_scan(NodeGrps, publish_type) of
		{error, _} ->
		    no_group;
                {ok, OwnSGroups, _OtherSGroups} ->
                    NodesDef = lists:append([Nodes||{_, _, Nodes}<-OwnSGroups]),
		    ?debug({"own_group_NodesDef", NodesDef}),
                    {normal, NodesDef}
            end
    end.
 

%%%====================================================================================
%%% Help function which computes publication list
%%%====================================================================================
publish_on_nodes(normal, no_group) ->
    all;
publish_on_nodes(hidden, no_group) ->
    [];
publish_on_nodes(normal, {normal, Nodes}) ->
    Nodes;
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

%% SGroupName conflict is not considered
-spec mk_new_s_group_tuples(SGroupName, Nodes) -> [{SGroupName, PubType, Nodes}] when
      SGroupName :: group_name(),
      PubType :: publish_type(),
      Nodes :: [Node :: node()].
mk_new_s_group_tuples(SGroupName, Nodes0) ->
    Nodes = lists:sort(Nodes0),
    case application:get_env(kernel, s_groups) of
        undefined ->
            [{SGroupName, normal, Nodes}];
        {ok, []} ->
             [{SGroupName, normal, Nodes}];
        {ok, NodeSGroups} ->
            case lists:keyfind(SGroupName, 1, NodeSGroups) of 
                false ->
                    [{SGroupName, normal, Nodes} | NodeSGroups];
                _ ->
                    lists:keyreplace(SGroupName, 1, NodeSGroups,
		                     {SGroupName, normal, Nodes})
            end
    end.


dlt_from_s_group_tuples(SGroupName) ->
    case application:get_env(kernel, s_groups) of
        undefined ->
            [];
        {ok, []} ->
            [];
        {ok, NodeSGroups} ->
            lists:keydelete(SGroupName, 1, NodeSGroups)
    end.

rmv_nodes_from_s_group_tuples(SGroupName, NodesToRmv) ->
    case application:get_env(kernel, s_groups) of
        undefined ->
            [];
        {ok, []} ->
            [];
        {ok, NodeSGroups} ->
            case lists:keyfind(SGroupName, 1, NodeSGroups) of 
                false ->
                    NodeSGroups;
                {SGroupName, normal, Nodes} ->
		    NewNodes = Nodes -- NodesToRmv,
                    lists:keyreplace(SGroupName, 1, NodeSGroups,
		                     {SGroupName, normal, NewNodes})
            end
    end.


%%%====================================================================================
%%% Additional function for registered_names
%%%====================================================================================
s_group_names(NameTuples, SGroupName) ->
    [{SGroupName, Name} || {G, Name} <- NameTuples, G==SGroupName].

s_group_conflict(Nodes, CCArgs) ->
    ?debug({"CCArgs:",CCArgs}),
    NewNodes = lists:foldl(fun(Node, NN_cc) -> 
                          case rpc:call(Node, ?MODULE,
    			                s_group_conflict_check,
					CCArgs) of
                              agreed ->
                      	          [Node | NN_cc];
                              _ ->
                      	          NN_cc
		          end
               end, [], Nodes),
    ?debug({"s_group_conflict_NewNodes:", NewNodes}),
    lists:usort(NewNodes).

connect_free_nodes(InitNode) ->
    case is_free_normal() of
	yes ->
	    ConnectedNodes = lists:usort(nodes(connected)--[node(), InitNode]),
	    ?debug({"connect_free_nodes_ConnectedNodes", ConnectedNodes}),
	    lists:foreach(fun(Node) ->
                  {s_group, Node} ! {nodeup, no_group, node()}
            end, ConnectedNodes);
	_ ->
	    ok
    end.

is_free_normal() ->
    case application:get_env(kernel, s_groups) of
        undefined ->
	    %% The node is free
	    case publish_arg() of
	        normal ->
		    %% The node is free normal
		    yes;
		_ ->
		    %% The node is free hidden
		    no
	    end;	
	_ ->
	    %% The node is an s_group node
	    no
    end.

overlap_nodes([], OverlapNodes) ->
    lists:usort(OverlapNodes);
overlap_nodes([Nodes | ListOfNodes], OverlapNodes) ->
    NewOverlapNodes = case OverlapNodes of
        	          [] ->
			      Nodes;
			  _ ->
			      intersection(OverlapNodes, Nodes)
    		      end,
    overlap_nodes(ListOfNodes, NewOverlapNodes).

connect_s_group_nodes(SGroupNodes) ->
    lists:foreach(fun(Node) ->
          {s_group, Node} ! {nodeup, node()}
    end, SGroupNodes).


%%%====================================================================================
%%% Sort s_groups and nodes in s_group configuration, i.e.
%%% * Remove duplicate s_groups
%%% * Check that no s_group is called 'no_group' or 'undefined'
%%% * Remove duplicate nodes in s_groups
%%% * Merge s_groups with the same name
%%% * Reset env_kernel
%%%====================================================================================
update_conf(SGroupsT) ->
    SGroupNames = lists:usort([G || {G, _T, _Nds} <- SGroupsT, G/='no_group', G/='undefined']),
    NewSGroupsT = [{G1, normal,
    		    lists:usort(lists:append([Nds || {G, _T, Nds} <- SGroupsT, G==G1]))}
                    || G1 <- SGroupNames],
    application:set_env(kernel, s_groups, NewSGroupsT),
    NewSGroupsT.


%%%====================================================================================
%%% Keeping s_group information in LongNodeName.config file
%%% 
%%%====================================================================================
is_rsconfig() ->
    case init:get_argument(rsconfig) of
        {ok, _Args} ->
    	    s_group ! record_state;
        _ ->
            no
    end.

is_rsconfig(S) ->
    case init:get_argument(rsconfig) of
        {ok, _Args} ->
    	    ConfigMsg = {calendar:local_time(), S},
    	    case whereis(rsconfig) of
                undefined ->
	    	    RsconfigPid = spawn(?MODULE, record_state, []),
	    	    register(rsconfig, RsconfigPid);
	    	RsconfigPid ->
	    	    true
    	    end,
    	    RsconfigPid ! ConfigMsg;
        _ ->
            no
    end.

record_state() ->
    receive
      {STime, S} ->
          ?debug({"record_state_S", S}),
    	  FileName = config_file_name(),
    	  NewConfigData = config_to_file(S#state.own_grps),
   	  case filelib:is_file(FileName) of
	      true ->
	          ?debug({"record_state_FileName", FileName}),
	          case file:read_file_info(FileName) of
	              {ok, FileInfo} ->
		          ?debug({"record_state_FileInfo", FileInfo}),
	                  case stime_lastmod_difference(STime, FileInfo#file_info.mtime) > 0 of
		       	      true ->
			          ?debug({"record_state_update"}),
		  		  write_data(FileName, NewConfigData);
			      _ ->
				  ?debug({"record_state", the_info_is_aready_up_to_date})
		           end;
	    	      _ ->
		          ?debug({"record_state", error_cannot_read_file})
	          end;
	      _ ->
	          ?debug({"record_state", file_does_not_exist}),
		  write_data(FileName, NewConfigData)
	  end,
    	  record_state()
    end.

write_data(FileName, Data) ->
    case file:open(FileName, [read,write]) of
        {ok, FileId} ->
    	    ?debug({"write_data_FileId", FileId}),
    	    file:write_file(FileName, io_lib:fwrite("~p.\n", [Data])),
    	    file:close(FileId);
    	_ ->
    	    ?debug({"write_data_error", cannot_open_file})
    end.

config_file_name() ->
    erlang:list_to_atom(string:concat(erlang:atom_to_list(node()), ".config")).

config_to_file(OwnGroups) ->
    [{kernel, [{s_groups, own_grps_to_config(OwnGroups)}]}].

own_grps_to_config(OwnGroups) ->
    own_grps_to_config(OwnGroups, []).

own_grps_to_config([], Config) ->
   Config;
own_grps_to_config([{Group, Nodes} | OwnGroups], Config) ->
    own_grps_to_config(OwnGroups, [{Group, normal, Nodes} | Config]).

stime_lastmod_difference(STime, LastModified) ->
    STimeSec = calendar:datetime_to_gregorian_seconds(STime),
    LastModifiedSec = calendar:datetime_to_gregorian_seconds(LastModified),
    STimeSec - LastModifiedSec.

