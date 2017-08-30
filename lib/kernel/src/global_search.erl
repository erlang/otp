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
-module(global_search).

%% Search for globally registered names in the global groups.
%% This is a help module to the global_group.erl


%% External exports
-export([start/2]).
-export([init_send/1]).
-export([init_whereis/1]).
-export([init_names/1]).


%% ONLY for test purpose
-export([send_test/1]).
-export([whereis_test/1]).
-export([names_test/1]).




%%%====================================================================================
%%% The search is done in a process separate from the global_group process
%%%====================================================================================
start(Flag, Arg) ->
    case Flag of
	send ->
	    spawn_link(?MODULE, init_send, [Arg]);
	whereis ->
	    spawn_link(?MODULE, init_whereis, [Arg]);
	names ->
	    spawn_link(?MODULE, init_names, [Arg]);
	%% ONLY for test suites, tests what happens when this process exits.
	send_test ->
	    spawn_link(?MODULE, send_test, [Arg]);
	whereis_test ->
	    spawn_link(?MODULE, whereis_test, [Arg]);
	names_test ->
	    spawn_link(?MODULE, names_test, [Arg])
    end.


%%%====================================================================================
%%%====================================================================================
%%%====================================================================================
%%% Search after a registered global Name anywhere (any), in a specified group or
%%% in a specified node.
%%% Return the result to the global_group process in own node and wait for
%%% this process to be killed.
%%%====================================================================================
%%%====================================================================================
%%%====================================================================================

-spec init_send(_) -> no_return().
init_send({any, NodesList, Name, Msg, From}) ->
    case whereis_any_loop(NodesList, Name) of
	undefined ->
	    Res = {badarg,{Name, Msg}},
	    gen_server:cast(global_group, {send_res, Res, Name, Msg, self(), From});
	Pid ->
	    gen_server:cast(global_group, {send_res, Pid, Name, Msg, self(), From})
    end,
    end_loop();
init_send({group, Nodes, Name, Msg, From}) ->
    case whereis_group_loop(Nodes, Name) of
	group_down ->
	    Res = {badarg,{Name, Msg}},
	    gen_server:cast(global_group, {send_res, Res, Name, Msg, self(), From});
	undefined ->
	    Res = {badarg,{Name, Msg}},
	    gen_server:cast(global_group, {send_res, Res, Name, Msg, self(), From});
	Pid ->
	    gen_server:cast(global_group, {send_res, Pid, Name, Msg, self(), From})
    end,
    end_loop();
init_send({node, Node, Name, Msg, From}) ->
    case whereis_check_node(Node, Name) of
	node_down ->
	    Res = {badarg,{Name, Msg}},
	    gen_server:cast(global_group, {send_res, Res, Name, Msg, self(), From});
	undefined ->
	    Res = {badarg,{Name, Msg}},
	    gen_server:cast(global_group, {send_res, Res, Name, Msg, self(), From});
	Pid ->
	    gen_server:cast(global_group, {send_res, Pid, Name, Msg, self(), From})
    end,
    end_loop().


%%%====================================================================================
%%%====================================================================================
%%%====================================================================================
%%% Search after a registered global Name anywhere (any), in a specified group or
%%% in a specified node.
%%% Return the result to the global_group process in own node and wait for
%%% this process to be killed.
%%%====================================================================================
%%%====================================================================================
%%%====================================================================================

-spec init_whereis(_) -> no_return().
init_whereis({any, NodesList, Name, From}) ->
    R = whereis_any_loop(NodesList, Name),
    gen_server:cast(global_group, {find_name_res, R, self(), From}),
    end_loop();
init_whereis({group, Nodes, Name, From}) ->
    case whereis_group_loop(Nodes, Name) of
	group_down ->
	    gen_server:cast(global_group, {find_name_res, undefined, self(), From});
	R ->
	    gen_server:cast(global_group, {find_name_res, R, self(), From})
    end,
    end_loop();
init_whereis({node, Node, Name, From}) ->
    case whereis_check_node(Node, Name) of
	node_down ->
	    gen_server:cast(global_group, {find_name_res, undefined, self(), From});
	R ->
	    gen_server:cast(global_group, {find_name_res, R, self(), From})
    end,
    end_loop().


%%%====================================================================================
%%%====================================================================================
%%%====================================================================================
%%% Get the registered names, in a specified group or in a specified node.
%%% Return the result to the global_group process in own node and wait for
%%% this process to be killed.
%%%====================================================================================
%%%====================================================================================
%%%====================================================================================
-spec init_names(_) -> no_return().
init_names({group, Nodes, From}) ->
    case names_group_loop(Nodes) of
	group_down ->
	    gen_server:cast(global_group, {registered_names_res, [], self(), From});
	R ->
	    gen_server:cast(global_group, {registered_names_res, R, self(), From})
    end,
    end_loop();
init_names({node, Node, From}) ->
    case names_check_node(Node) of
	node_down ->
	    gen_server:cast(global_group, {registered_names_res, [], self(), From});
	R ->
	    gen_server:cast(global_group, {registered_names_res, R, self(), From})
    end,
    end_loop().

%%%====================================================================================
%%% Wait for the kill message.
%%%====================================================================================

-spec end_loop() -> no_return().

end_loop() ->
    receive
	kill ->
	    exit(normal)
    end.

%%%====================================================================================
%%% Search for the globally registered name in the whole known world.
%%%====================================================================================
whereis_any_loop([], _Name) ->
    undefined;
whereis_any_loop([{_Group_name, Nodes}|T], Name) ->
    case whereis_group_loop(Nodes, Name) of
	group_down ->
	    whereis_any_loop(T, Name);
	undefined ->
	    whereis_any_loop(T, Name);
	R ->
	    R
    end.

%%%====================================================================================
%%% Search for the globally registered name in a specified global group.
%%%====================================================================================
whereis_group_loop([], _Name) ->
    group_down;
whereis_group_loop([Node|T], Name) ->
    case whereis_check_node(Node, Name) of
	node_down ->
	    whereis_group_loop(T, Name);
	R ->
	    R
    end.
%%%====================================================================================
%%% Search for the globally registered name on a specified node.
%%%====================================================================================
whereis_check_node(Node, Name) ->
    case net_adm:ping(Node) of
	pang ->
	    node_down;
	pong ->
	    monitor_node(Node, true),
	    gen_server:cast({global_group, Node},{find_name, self(), Name}),
	    receive
		{nodedown, Node} ->
		    node_down;
		{find_name_res, Result} ->
		    monitor_node(Node, false),
		    Result
	    end
    end.




%%%====================================================================================
%%% Search for all globally registered name in a specified global group.
%%%====================================================================================
names_group_loop([]) ->
    group_down;
names_group_loop([Node|T]) ->
    case names_check_node(Node) of
	node_down ->
	    names_group_loop(T);
	R ->
	    R
    end.
%%%====================================================================================
%%% Search for all globally registered name on a specified node.
%%%====================================================================================
names_check_node(Node) ->
    case net_adm:ping(Node) of
	pang ->
	    node_down;
	pong ->
	    monitor_node(Node, true),
	    gen_server:cast({global_group, Node},{registered_names, self()}),
	    receive
		{nodedown, Node} ->
		    node_down;
		{registered_names_res, Result} ->
		    monitor_node(Node, false),
		    Result
	    end
    end.






%%%====================================================================================
%%% Test what happens when this process exits.
%%%====================================================================================
send_test(_Args) ->
    timer:sleep(5000),
    exit(testing_exit).

whereis_test(_Args) ->
    timer:sleep(5000),
    exit(testing_exit).

names_test(_Args) ->
    timer:sleep(5000),
    exit(testing_exit).



