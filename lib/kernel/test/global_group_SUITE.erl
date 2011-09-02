%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2011. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

-module(global_group_SUITE).

-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2,
	 init_per_suite/1, end_per_suite/1]).
-export([start_gg_proc/1, no_gg_proc/1, no_gg_proc_sync/1, compatible/1, 
	 one_grp/1, one_grp_x/1, two_grp/1, hidden_groups/1, test_exit/1]).
-export([init/1, init/2, init2/2, start_proc/1, start_proc_rereg/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

%-compile(export_all).

-include_lib("test_server/include/test_server.hrl").

-define(NODES, [node()|nodes()]).

-define(UNTIL(Seq), loop_until_true(fun() -> Seq end)).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [start_gg_proc, no_gg_proc, no_gg_proc_sync, compatible,
     one_grp, one_grp_x, two_grp, test_exit, hidden_groups].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.


init_per_suite(Config) ->

    %% Copied from test_server_ctrl ln 647, we have to do this here as
    %% the test_server only does this when run without common_test
    global:sync(),
    case global:whereis_name(test_server) of
	undefined ->
	    io:format(user, "Registering test_server globally!~n",[]),
	    global:register_name(test_server, whereis(test_server_ctrl));
	Pid ->
	    case node() of
		N when N == node(Pid) ->
		    io:format(user, "Warning: test_server already running!\n", []),
		    global:re_register_name(test_server,self());
		_ ->
		    ok
	    end
    end,
    Config.

end_per_suite(_Config) ->
    global:unregister_name(test_server),
    ok.

-define(TESTCASE, testcase_name).
-define(testcase, ?config(?TESTCASE, Config)).

init_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    Dog=?t:timetrap(?t:minutes(5)),
    [{?TESTCASE, Case}, {watchdog, Dog}|Config].

end_per_testcase(_Func, Config) ->
    Dog=?config(watchdog, Config),
    ?t:timetrap_cancel(Dog).

%%-----------------------------------------------------------------
%% Test suites for global groups.
%% Should be started in a CC view with:
%% erl -sname XXX -rsh ctrsh where XXX not in [cp1 .. cpN]
%%-----------------------------------------------------------------


start_gg_proc(suite) -> [];
start_gg_proc(doc) -> ["Check that the global_group processes are started automatically. "];
start_gg_proc(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(120)),

    ?line Dir = ?config(priv_dir, Config),
    ?line File = filename:join(Dir, "global_group.config"),
    ?line {ok, Fd}=file:open(File, [write]),
    [Ncp1,Ncp2,Ncp3] = node_names([cp1, cp2, cp3], Config),
    ?line config(Fd, Ncp1, Ncp2, Ncp3, "cpx", "cpy", "cpz", "cpq"),

    ?line Cp1nn = node_at(Ncp1),
    ?line Cp2nn = node_at(Ncp2),
    ?line Cp3nn = node_at(Ncp3),

    ?line {ok, Cp1} = start_node(Ncp1, Config),
    ?line {ok, Cp2} = start_node(Ncp2, Config),
    ?line {ok, Cp3} = start_node(Ncp3, Config),

    ?line [] = rpc:call(Cp1, global_group, registered_names, [{node, Cp1nn}]),
    ?line [] = rpc:call(Cp2, global_group, registered_names, [{node, Cp2nn}]),
    ?line [] = rpc:call(Cp3, global_group, registered_names, [{node, Cp3nn}]),

    % stop the nodes, and make sure names are released.
    stop_node(Cp1),
    stop_node(Cp2),
    stop_node(Cp3),

    ?line ?UNTIL(undefined =:= global:whereis_name(test)),
    ?line test_server:timetrap_cancel(Dog),
    ok.
    


no_gg_proc(suite) -> [];
no_gg_proc(doc) -> ["Start a system without global groups. Nodes are not "
		    "synced at start (sync_nodes_optional is not defined)"];
no_gg_proc(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(200)),

    ?line Dir = ?config(priv_dir, Config),
    ?line File = filename:join(Dir, "no_global_group.config"),
    ?line {ok, Fd} = file:open(File, [write]),
    ?line config_no(Fd),

    ?line NN = node_name(atom_to_list(node())),
    ?line Cp1nn = list_to_atom("cp1@" ++ NN),
    ?line Cp2nn = list_to_atom("cp2@" ++ NN),
    ?line Cp3nn = list_to_atom("cp3@" ++ NN),
    ?line Cpxnn = list_to_atom("cpx@" ++ NN),
    ?line Cpynn = list_to_atom("cpy@" ++ NN),
    ?line Cpznn = list_to_atom("cpz@" ++ NN),

    ?line {ok, Cp1} = start_node_no(cp1, Config),
    ?line {ok, Cp2} = start_node_no(cp2, Config),
    ?line {ok, Cp3} = start_node_no(cp3, Config),
    ?line {ok, Cpx} = start_node_no(cpx, Config),
    ?line {ok, Cpy} = start_node_no(cpy, Config),
    ?line {ok, Cpz} = start_node_no(cpz, Config),

    %% let the nodes know of each other
    ?line pong = rpc:call(Cp1, net_adm, ping, [Cp2nn]),
    ?line pong = rpc:call(Cp2, net_adm, ping, [Cp3nn]),
    ?line pong = rpc:call(Cp3, net_adm, ping, [Cpxnn]),
    ?line pong = rpc:call(Cpx, net_adm, ping, [Cpynn]),
    ?line pong = rpc:call(Cpy, net_adm, ping, [Cpznn]),

    ?line wait_for_ready_net(),

    ?line [test_server] = rpc:call(Cp1, global_group, registered_names, [{node, Cp1nn}]),
    ?line [test_server] = rpc:call(Cp2, global_group, registered_names, [{node, Cp2nn}]),
    ?line [test_server] = rpc:call(Cp3, global_group, registered_names, [{node, Cp3nn}]),
    ?line [test_server] = rpc:call(Cp1, global_group, registered_names, [{node, Cpxnn}]),
    ?line [test_server] = rpc:call(Cp2, global_group, registered_names, [{node, Cpynn}]),
    ?line [test_server] = rpc:call(Cp3, global_group, registered_names, [{node, Cpznn}]),


    % start a proc and register it
    ?line {Pid2, yes} = rpc:call(Cp2, ?MODULE, start_proc, [test2]),

    ?line RegNames = lists:sort([test2,test_server]),

    ?line RegNames = 
	lists:sort(
	  rpc:call(Cp1, global_group, registered_names, [{node, Cp1nn}])),
    ?line RegNames = 
	lists:sort(
	  rpc:call(Cp2, global_group, registered_names, [{node, Cp2nn}])),
    ?line RegNames = 
	lists:sort(
	  rpc:call(Cp3, global_group, registered_names, [{node, Cp3nn}])),
    ?line RegNames = 
	lists:sort(
	  rpc:call(Cp1, global_group, registered_names, [{node, Cpxnn}])),
    ?line RegNames = 
	lists:sort(
	  rpc:call(Cp2, global_group, registered_names, [{node, Cpynn}])),
    ?line RegNames = 
	lists:sort(
	  rpc:call(Cp3, global_group, registered_names, [{node, Cpznn}])),


    ?line undefined = rpc:call(Cp3, global_group, global_groups, []),

    ?line Own_nodes_should = [node(), Cp1nn, Cp2nn, Cp3nn, 
			      Cpxnn, Cpynn, Cpznn],
    ?line Own_nodes = rpc:call(Cp3, global_group, own_nodes, []), 
    ?line [] = (Own_nodes -- Own_nodes_should),
    ?line [] = (Own_nodes_should -- Own_nodes),
    
    ?line Pid2 = rpc:call(Cp1, global_group, send, [test2, {ping, self()}]),
    ?line receive
	      {pong, Cp2} -> ok
				 after
				     2000 -> test_server:fail(timeout2)
				 end,
    ?line Pid2 = rpc:call(Cp2, global_group, send, [test2, {ping, self()}]),
    ?line receive
	      {pong, Cp2} -> ok
				 after
				     2000 -> test_server:fail(timeout3)
				 end,
    ?line Pid2 = rpc:call(Cpz, global_group, send, [test2, {ping, self()}]),
    ?line receive
	      {pong, Cp2} -> ok
				 after
				     2000 -> test_server:fail(timeout4)
				 end,


    % start a proc and register it
    ?line {PidX, yes} = rpc:call(Cpx, ?MODULE, start_proc, [test]),


    %%------------------------------------
    %% Test monitor nodes
    %%------------------------------------
    ?line Pid2 = rpc:call(Cp1, global_group, send, [{node, Cp2nn}, test2, monitor]),
    ?line PidX = rpc:call(Cpx, global_group, send, [{node, Cpxnn}, test, monitor]),


    % Kill node Cp1
    ?line Pid2 = 
	rpc:call(Cp2, global_group, send, [{node, Cp2nn}, test2, {wait_nodedown, Cp1}]),
    ?line PidX = 
	rpc:call(Cpx, global_group, send, [{node, Cpxnn}, test, {wait_nodedown, Cp1}]),
    ?line test_server:sleep(100),
    ?line stop_node(Cp1),
    ?line test_server:sleep(1000),
    
    ?line ok = assert_loop(Cp2, Cp2nn, test2, Pid2, loop),
    ?line ok = assert_loop(Cpx, Cpxnn, test, PidX, loop),

    % Kill node Cpz
    ?line Pid2 = 
	rpc:call(Cp2, global_group, send, [{node, Cp2nn}, test2, {wait_nodedown, Cpz}]),
    ?line PidX = 
	rpc:call(Cpx, global_group, send, [{node, Cpxnn}, test, {wait_nodedown, Cpz}]),
    ?line test_server:sleep(100),
    ?line stop_node(Cpz),
    ?line test_server:sleep(1000),
    
    ?line ok = assert_loop(Cp2, Cp2nn, test2, Pid2, loop),
    ?line ok = assert_loop(Cpx, Cpxnn, test, PidX, loop),

    % Restart node Cp1
    ?line Pid2 = 
	rpc:call(Cp2, global_group, send, [{node, Cp2nn}, test2, {wait_nodeup, Cp1}]),
    ?line PidX = 
	rpc:call(Cpx, global_group, send, [{node, Cpxnn}, test, {wait_nodeup, Cp1}]),
    ?line {ok, Cp1} = start_node_no(cp1, Config),
    ?line pong = rpc:call(Cp2, net_adm, ping, [Cp1nn]),
    ?line pong = rpc:call(Cpx, net_adm, ping, [Cp1nn]),
    ?line wait_for_ready_net(),
    
    ?line ok = assert_loop(Cp2, Cp2nn, test2, Pid2, loop),
    ?line ok = assert_loop(Cpx, Cpxnn, test, PidX, loop),

    % Restart node Cpz
    ?line Pid2 = 
	rpc:call(Cp2, global_group, send, [{node, Cp2nn}, test2, {wait_nodeup, Cpz}]),
    ?line PidX = 
	rpc:call(Cpx, global_group, send, [{node, Cpxnn}, test, {wait_nodeup, Cpz}]),
    ?line {ok, Cpz} = start_node_no(cpz, Config),
    ?line pong = rpc:call(Cp2, net_adm, ping, [Cpznn]),
    ?line pong = rpc:call(Cpx, net_adm, ping, [Cpznn]),
    ?line wait_for_ready_net(),
    
    ?line ok = assert_loop(Cp2, Cp2nn, test2, Pid2, loop),
    ?line ok = assert_loop(Cpx, Cpxnn, test, PidX, loop),

    % stop the nodes, and make sure names are released.
    stop_node(Cp1),
    stop_node(Cp2),
    stop_node(Cp3),
    stop_node(Cpx),
    stop_node(Cpy),
    stop_node(Cpz),

    ?line ?UNTIL(undefined =:= global:whereis_name(test)),
    ?line test_server:timetrap_cancel(Dog),
    ok.
    



no_gg_proc_sync(suite) -> [];
no_gg_proc_sync(doc) -> 
    ["Start a system without global groups, but syncing the nodes by using " 
     "sync_nodes_optional."];
no_gg_proc_sync(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(200)),

    ?line Dir = ?config(priv_dir, Config),
    ?line File = filename:join(Dir, "no_global_group_sync.config"),
    ?line {ok, Fd} = file:open(File, [write]),

    [Ncp1,Ncp2,Ncp3,Ncpx,Ncpy,Ncpz] = 
        node_names([cp1,cp2,cp3,cpx,cpy,cpz], Config),
    ?line config_sync(Fd, Ncp1, Ncp2, Ncp3, Ncpx, Ncpy, Ncpz),

    ?line Cp1nn = node_at(Ncp1),
    ?line Cp2nn = node_at(Ncp2),
    ?line Cp3nn = node_at(Ncp3),
    ?line Cpxnn = node_at(Ncpx),
    ?line Cpynn = node_at(Ncpy),
    ?line Cpznn = node_at(Ncpz),

    ?line {ok, Cp1} = start_node_no2(Ncp1, Config),
    ?line {ok, Cp2} = start_node_no2(Ncp2, Config),
    ?line {ok, Cp3} = start_node_no2(Ncp3, Config),
    ?line {ok, Cpx} = start_node_no2(Ncpx, Config),
    ?line {ok, Cpy} = start_node_no2(Ncpy, Config),
    ?line {ok, Cpz} = start_node_no2(Ncpz, Config),

    %% let the nodes know of each other
    ?line pong = rpc:call(Cp1, net_adm, ping, [Cp2nn]),
    ?line pong = rpc:call(Cp2, net_adm, ping, [Cp3nn]),
    ?line pong = rpc:call(Cp3, net_adm, ping, [Cpxnn]),
    ?line pong = rpc:call(Cpx, net_adm, ping, [Cpynn]),
    ?line pong = rpc:call(Cpy, net_adm, ping, [Cpznn]),

    ?line wait_for_ready_net(),

    ?line [test_server] = rpc:call(Cp1, global_group, registered_names, [{node, Cp1nn}]),
    ?line [test_server] = rpc:call(Cp2, global_group, registered_names, [{node, Cp2nn}]),
    ?line [test_server] = rpc:call(Cp3, global_group, registered_names, [{node, Cp3nn}]),
    ?line [test_server] = rpc:call(Cp1, global_group, registered_names, [{node, Cpxnn}]),
    ?line [test_server] = rpc:call(Cp2, global_group, registered_names, [{node, Cpynn}]),
    ?line [test_server] = rpc:call(Cp3, global_group, registered_names, [{node, Cpznn}]),


    % start a proc and register it
    ?line {Pid2, yes} = rpc:call(Cp2, ?MODULE, start_proc, [test2]),

    ?line RegNames = lists:sort([test2,test_server]),

    ?line RegNames = 
	lists:sort(
	  rpc:call(Cp1, global_group, registered_names, [{node, Cp1nn}])),
    ?line RegNames = 
	lists:sort(
	  rpc:call(Cp2, global_group, registered_names, [{node, Cp2nn}])),
    ?line RegNames = 
	lists:sort(
	  rpc:call(Cp3, global_group, registered_names, [{node, Cp3nn}])),
    ?line RegNames = 
	lists:sort(
	  rpc:call(Cp1, global_group, registered_names, [{node, Cpxnn}])),
    ?line RegNames = 
	lists:sort(
	  rpc:call(Cp2, global_group, registered_names, [{node, Cpynn}])),
    ?line RegNames = 
	lists:sort(
	  rpc:call(Cp3, global_group, registered_names, [{node, Cpznn}])),


    ?line undefined = rpc:call(Cp3, global_group, global_groups, []),

    ?line Own_nodes_should = [node(), Cp1nn, Cp2nn, Cp3nn, 
			      Cpxnn, Cpynn, Cpznn],
    ?line Own_nodes = rpc:call(Cp3, global_group, own_nodes, []), 
    ?line [] = (Own_nodes -- Own_nodes_should),
    ?line [] = (Own_nodes_should -- Own_nodes),
    
    ?line Pid2 = rpc:call(Cp1, global_group, send, [test2, {ping, self()}]),
    ?line receive
	      {pong, Cp2} -> ok
				 after
				     2000 -> test_server:fail(timeout2)
				 end,
    ?line Pid2 = rpc:call(Cp2, global_group, send, [test2, {ping, self()}]),
    ?line receive
	      {pong, Cp2} -> ok
				 after
				     2000 -> test_server:fail(timeout3)
				 end,
    ?line Pid2 = rpc:call(Cpz, global_group, send, [test2, {ping, self()}]),
    ?line receive
	      {pong, Cp2} -> ok
				 after
				     2000 -> test_server:fail(timeout4)
				 end,


    % start a proc and register it
    ?line {PidX, yes} = rpc:call(Cpx, ?MODULE, start_proc, [test]),


    %%------------------------------------
    %% Test monitor nodes
    %%------------------------------------
    ?line Pid2 = rpc:call(Cp1, global_group, send, [{node, Cp2nn}, test2, monitor]),
    ?line PidX = rpc:call(Cpx, global_group, send, [{node, Cpxnn}, test, monitor]),


    % Kill node Cp1
    ?line Pid2 = 
	rpc:call(Cp2, global_group, send, [{node, Cp2nn}, test2, {wait_nodedown, Cp1}]),
    ?line PidX = 
	rpc:call(Cpx, global_group, send, [{node, Cpxnn}, test, {wait_nodedown, Cp1}]),
    ?line test_server:sleep(100),
    ?line stop_node(Cp1),
    ?line test_server:sleep(1000),
    
    ?line ok = assert_loop(Cp2, Cp2nn, test2, Pid2, loop),
    ?line ok = assert_loop(Cpx, Cpxnn, test, PidX, loop),

    % Kill node Cpz
    ?line Pid2 = 
	rpc:call(Cp2, global_group, send, [{node, Cp2nn}, test2, {wait_nodedown, Cpz}]),
    ?line PidX = 
	rpc:call(Cpx, global_group, send, [{node, Cpxnn}, test, {wait_nodedown, Cpz}]),
    ?line test_server:sleep(100),
    ?line stop_node(Cpz),
    ?line test_server:sleep(1000),
    
    ?line ok = assert_loop(Cp2, Cp2nn, test2, Pid2, loop),
    ?line ok = assert_loop(Cpx, Cpxnn, test, PidX, loop),

    % Restart node Cp1
    ?line Pid2 = 
	rpc:call(Cp2, global_group, send, [{node, Cp2nn}, test2, {wait_nodeup, Cp1}]),
    ?line PidX = 
	rpc:call(Cpx, global_group, send, [{node, Cpxnn}, test, {wait_nodeup, Cp1}]),
    ?line {ok, Cp1} = start_node_no2(Ncp1, Config),
    ?line pong = rpc:call(Cp2, net_adm, ping, [Cp1nn]),
    ?line pong = rpc:call(Cpx, net_adm, ping, [Cp1nn]),
    ?line wait_for_ready_net(),
    
    ?line ok = assert_loop(Cp2, Cp2nn, test2, Pid2, loop),
    ?line ok = assert_loop(Cpx, Cpxnn, test, PidX, loop),

    % Restart node Cpz
    ?line Pid2 = 
	rpc:call(Cp2, global_group, send, [{node, Cp2nn}, test2, {wait_nodeup, Cpz}]),
    ?line PidX = 
	rpc:call(Cpx, global_group, send, [{node, Cpxnn}, test, {wait_nodeup, Cpz}]),
    ?line {ok, Cpz} = start_node_no2(Ncpz, Config),
    ?line pong = rpc:call(Cp2, net_adm, ping, [Cpznn]),
    ?line pong = rpc:call(Cpx, net_adm, ping, [Cpznn]),
    ?line wait_for_ready_net(),
    
    ?line ok = assert_loop(Cp2, Cp2nn, test2, Pid2, loop),
    ?line ok = assert_loop(Cpx, Cpxnn, test, PidX, loop),

    % stop the nodes, and make sure names are released.
    stop_node(Cp1),
    stop_node(Cp2),
    stop_node(Cp3),
    stop_node(Cpx),
    stop_node(Cpy),
    stop_node(Cpz),

    ?line ?UNTIL(undefined =:= global:whereis_name(test)),
    ?line test_server:timetrap_cancel(Dog),
    ok.
    



compatible(suite) -> [];
compatible(doc) -> 
    ["Check that a system without global groups is compatible with the old R4 system."];
compatible(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(200)),

    ?line Dir = ?config(priv_dir, Config),
    ?line File = filename:join(Dir, "global_group_comp.config"),
    ?line {ok, Fd} = file:open(File, [write]),

    [Ncp1,Ncp2,Ncp3,Ncpx,Ncpy,Ncpz] = 
        node_names([cp1,cp2,cp3,cpx,cpy,cpz], Config),
    ?line config_comp(Fd, Ncp1, Ncp2, Ncp3, Ncpx, Ncpy, Ncpz),

    ?line Cp1nn = node_at(Ncp1),
    ?line Cp2nn = node_at(Ncp2),
    ?line Cp3nn = node_at(Ncp3),
    ?line Cpxnn = node_at(Ncpx),
    ?line Cpynn = node_at(Ncpy),
    ?line Cpznn = node_at(Ncpz),

    ?line {ok, Cp1} = start_node_comp(Ncp1, Config),
    ?line {ok, Cp2} = start_node_comp(Ncp2, Config),
    ?line {ok, Cp3} = start_node_comp(Ncp3, Config),
    ?line {ok, Cpx} = start_node_comp(Ncpx, Config),
    ?line {ok, Cpy} = start_node_comp(Ncpy, Config),
    ?line {ok, Cpz} = start_node_comp(Ncpz, Config),

    %% let the nodes know of each other
    ?line pong = rpc:call(Cp1, net_adm, ping, [Cp2nn]),
    ?line pong = rpc:call(Cp2, net_adm, ping, [Cp3nn]),
    ?line pong = rpc:call(Cp3, net_adm, ping, [Cpxnn]),
    ?line pong = rpc:call(Cpx, net_adm, ping, [Cpynn]),
    ?line pong = rpc:call(Cpy, net_adm, ping, [Cpznn]),

    ?line wait_for_ready_net(),

    ?line [test_server] = rpc:call(Cp1, global_group, registered_names, [{node, Cp1nn}]),
    ?line [test_server] = rpc:call(Cp2, global_group, registered_names, [{node, Cp2nn}]),
    ?line [test_server] = rpc:call(Cp3, global_group, registered_names, [{node, Cp3nn}]),
    ?line [test_server] = rpc:call(Cp1, global_group, registered_names, [{node, Cpxnn}]),
    ?line [test_server] = rpc:call(Cp2, global_group, registered_names, [{node, Cpynn}]),
    ?line [test_server] = rpc:call(Cp3, global_group, registered_names, [{node, Cpznn}]),


    % start a proc and register it
    ?line {Pid2, yes} = rpc:call(Cp2, ?MODULE, start_proc, [test2]),

    ?line RegNames = lists:sort([test2,test_server]),

    ?line RegNames = 
	lists:sort(
	  rpc:call(Cp1, global_group, registered_names, [{node, Cp1nn}])),
    ?line RegNames = 
	lists:sort(
	  rpc:call(Cp2, global_group, registered_names, [{node, Cp2nn}])),
    ?line RegNames = 
	lists:sort(
	  rpc:call(Cp3, global_group, registered_names, [{node, Cp3nn}])),
    ?line RegNames = 
	lists:sort(
	  rpc:call(Cp1, global_group, registered_names, [{node, Cpxnn}])),
    ?line RegNames = 
	lists:sort(
	  rpc:call(Cp2, global_group, registered_names, [{node, Cpynn}])),
    ?line RegNames = 
	lists:sort(
	  rpc:call(Cp3, global_group, registered_names, [{node, Cpznn}])),


    ?line undefined = rpc:call(Cp3, global_group, global_groups, []),

    ?line Own_nodes_should = [node(), Cp1nn, Cp2nn, Cp3nn, 
			      Cpxnn, Cpynn, Cpznn],
    ?line Own_nodes = rpc:call(Cp3, global_group, own_nodes, []), 
    ?line [] = (Own_nodes -- Own_nodes_should),
    ?line [] = (Own_nodes_should -- Own_nodes),
    
    ?line Pid2 = rpc:call(Cp1, global_group, send, [test2, {ping, self()}]),
    ?line receive
	      {pong, Cp2} -> ok
				 after
				     2000 -> test_server:fail(timeout2)
				 end,
    ?line Pid2 = rpc:call(Cp2, global_group, send, [test2, {ping, self()}]),
    ?line receive
	      {pong, Cp2} -> ok
				 after
				     2000 -> test_server:fail(timeout3)
				 end,
    ?line Pid2 = rpc:call(Cpz, global_group, send, [test2, {ping, self()}]),
    ?line receive
	      {pong, Cp2} -> ok
				 after
				     2000 -> test_server:fail(timeout4)
				 end,


    % start a proc and register it
    ?line {PidX, yes} = rpc:call(Cpx, ?MODULE, start_proc, [test]),


    %%------------------------------------
    %% Test monitor nodes
    %%------------------------------------
    ?line Pid2 = rpc:call(Cp1, global_group, send, [{node, Cp2nn}, test2, monitor]),
    ?line PidX = rpc:call(Cpx, global_group, send, [{node, Cpxnn}, test, monitor]),


    % Kill node Cp1
    ?line Pid2 = 
	rpc:call(Cp2, global_group, send, [{node, Cp2nn}, test2, {wait_nodedown, Cp1}]),
    ?line PidX = 
	rpc:call(Cpx, global_group, send, [{node, Cpxnn}, test, {wait_nodedown, Cp1}]),
    ?line test_server:sleep(100),
    ?line stop_node(Cp1),
    ?line test_server:sleep(1000),
    
    ?line ok = assert_loop(Cp2, Cp2nn, test2, Pid2, loop),
    ?line ok = assert_loop(Cpx, Cpxnn, test, PidX, loop),

    % Kill node Cpz
    ?line Pid2 = 
	rpc:call(Cp2, global_group, send, [{node, Cp2nn}, test2, {wait_nodedown, Cpz}]),
    ?line PidX = 
	rpc:call(Cpx, global_group, send, [{node, Cpxnn}, test, {wait_nodedown, Cpz}]),
    ?line test_server:sleep(100),
    ?line stop_node(Cpz),
    ?line test_server:sleep(1000),
    
    ?line ok = assert_loop(Cp2, Cp2nn, test2, Pid2, loop),
    ?line ok = assert_loop(Cpx, Cpxnn, test, PidX, loop),

    % Restart node Cp1
    ?line Pid2 = 
	rpc:call(Cp2, global_group, send, [{node, Cp2nn}, test2, {wait_nodeup, Cp1}]),
    ?line PidX = 
	rpc:call(Cpx, global_group, send, [{node, Cpxnn}, test, {wait_nodeup, Cp1}]),
    ?line {ok, Cp1} = start_node_comp(Ncp1, Config),
    ?line pong = rpc:call(Cp2, net_adm, ping, [Cp1nn]),
    ?line pong = rpc:call(Cpx, net_adm, ping, [Cp1nn]),
    ?line wait_for_ready_net(),
    
    ?line ok = assert_loop(Cp2, Cp2nn, test2, Pid2, loop),
    ?line ok = assert_loop(Cpx, Cpxnn, test, PidX, loop),

    % Restart node Cpz
    ?line Pid2 = 
	rpc:call(Cp2, global_group, send, [{node, Cp2nn}, test2, {wait_nodeup, Cpz}]),
    ?line PidX = 
	rpc:call(Cpx, global_group, send, [{node, Cpxnn}, test, {wait_nodeup, Cpz}]),
    ?line {ok, Cpz} = start_node_comp(Ncpz, Config),
    ?line pong = rpc:call(Cp2, net_adm, ping, [Cpznn]),
    ?line pong = rpc:call(Cpx, net_adm, ping, [Cpznn]),
    ?line wait_for_ready_net(),
    
    ?line ok = assert_loop(Cp2, Cp2nn, test2, Pid2, loop),
    ?line ok = assert_loop(Cpx, Cpxnn, test, PidX, loop),

    % stop the nodes, and make sure names are released.
    stop_node(Cp1),
    stop_node(Cp2),
    stop_node(Cp3),
    stop_node(Cpx),
    stop_node(Cpy),
    stop_node(Cpz),

    ?line ?UNTIL(undefined =:= global:whereis_name(test)),
    ?line test_server:timetrap_cancel(Dog),
    ok.
    



one_grp(suite) -> [];
one_grp(doc) -> ["Test a system with only one global group. "];
one_grp(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(120)),

    ?line Dir = ?config(priv_dir, Config),
    ?line File = filename:join(Dir, "global_group.config"),
    ?line {ok, Fd} = file:open(File, [write]),
    [Ncp1,Ncp2,Ncp3] = node_names([cp1, cp2, cp3], Config),
    ?line config(Fd, Ncp1, Ncp2, Ncp3, "cpx", "cpy", "cpz", "cpq"),

    ?line {ok, Cp1} = start_node(Ncp1, Config),
    ?line {ok, Cp2} = start_node(Ncp2, Config),
    ?line {ok, Cp3} = start_node(Ncp3, Config),

    % sleep a while to make the global_group to sync...
    test_server:sleep(1000),

    % start a proc and register it
    ?line {Pid, yes} = rpc:call(Cp1, ?MODULE, start_proc, [test]),

    % test that it is registered at all nodes
    ?line Pid = rpc:call(Cp1, global, whereis_name, [test]),
    ?line Pid = rpc:call(Cp2, global, whereis_name, [test]),
    ?line Pid = rpc:call(Cp3, global, whereis_name, [test]),
    
    % try to register the same name
    ?line no = rpc:call(Cp1, global, register_name, [test, self()]),
    
    % let process exit, check that it is unregistered automatically
    Pid ! die,
    ?line 
    ?UNTIL(begin
               (undefined =:= rpc:call(Cp1, global, whereis_name, [test])) and
               (undefined =:= rpc:call(Cp2, global, whereis_name, [test])) and
               (undefined =:= rpc:call(Cp3, global, whereis_name, [test]))
           end),
    
    % test re_register
    ?line {Pid2, yes} = rpc:call(Cp1, ?MODULE, start_proc, [test]),
    ?line Pid2 = rpc:call(Cp3, global, whereis_name, [test]),    
    Pid3 = rpc:call(Cp3, ?MODULE, start_proc_rereg, [test]),
    ?line Pid3 = rpc:call(Cp3, global, whereis_name, [test]),        

    % test sending
    rpc:call(Cp1, global, send, [test, {ping, self()}]),
    receive
	{pong, Cp3} -> ok
    after
	2000 -> test_server:fail(timeout1)
    end,

    rpc:call(Cp3, global, send, [test, {ping, self()}]),
    receive
	{pong, Cp3} -> ok
    after
	2000 -> test_server:fail(timeout2)
    end,

    ?line rpc:call(Cp3, global, unregister_name, [test]),    
    ?line undefined = rpc:call(Cp1, global, whereis_name, [test]),
    ?line undefined = rpc:call(Cp2, global, whereis_name, [test]),
    ?line undefined = rpc:call(Cp3, global, whereis_name, [test]),

    Pid3 ! die,
    ?line ?UNTIL(undefined =:= rpc:call(Cp3, global, whereis_name, [test])),

    % register a proc
    ?line {_, yes} = rpc:call(Cp3, ?MODULE, start_proc, [test]),

    % stop the nodes, and make sure names are released.
    stop_node(Cp3),

    ?line ?UNTIL(undefined =:= rpc:call(Cp1, global, whereis_name, [test])),
    Pid2 ! die,

    stop_node(Cp1),
    stop_node(Cp2),

    ?line test_server:timetrap_cancel(Dog),
    ok.
    




one_grp_x(suite) -> [];
one_grp_x(doc) -> ["Check a system with only one global group. "
		   "Start the nodes with different time intervals. "];
one_grp_x(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(120)),

    ?line Dir = ?config(priv_dir, Config),
    ?line File = filename:join(Dir, "global_group.config"),
    ?line {ok, Fd} = file:open(File, [write]),
    [Ncp1,Ncp2,Ncp3] = node_names([cp1, cp2, cp3], Config),
    ?line config(Fd, Ncp1, Ncp2, Ncp3, "cpx", "cpy", "cpz", "cpq"),

    ?line {ok, Cp1} = start_node(Ncp1, Config),
    % sleep a while to make the global_group to sync...
    test_server:sleep(1000),

    % start a proc and register it
    ?line {Pid, yes} = rpc:call(Cp1, ?MODULE, start_proc, [test]),

    ?line {ok, Cp2} = start_node(Ncp2, Config),
    % sleep a while to make the global_group to sync...
    test_server:sleep(1000),

    % test that it is registered at all nodes
    ?line Pid = rpc:call(Cp1, global, whereis_name, [test]),
    ?line Pid = rpc:call(Cp2, global, whereis_name, [test]),

    ?line {ok, Cp3} = start_node(Ncp3, Config),
    % sleep a while to make the global_group to sync...
    test_server:sleep(1000),

    ?line Pid = rpc:call(Cp3, global, whereis_name, [test]),
    
    % try to register the same name
    ?line no = rpc:call(Cp1, global, register_name, [test, self()]),
    
    % let process exit, check that it is unregistered automatically
    Pid ! die,
    ?line 
    ?UNTIL(begin
               (undefined =:= rpc:call(Cp1, global, whereis_name, [test])) and
               (undefined =:= rpc:call(Cp2, global, whereis_name, [test])) and
               (undefined =:= rpc:call(Cp3, global, whereis_name, [test]))
           end),
    
    % test re_register
    ?line {Pid2, yes} = rpc:call(Cp1, ?MODULE, start_proc, [test]),
    ?line Pid2 = rpc:call(Cp3, global, whereis_name, [test]),    

    Pid2 ! die,

    stop_node(Cp1),
    stop_node(Cp2),
    stop_node(Cp3),

    ?line test_server:timetrap_cancel(Dog),
    ok.
    





two_grp(suite) -> [];
two_grp(doc) -> ["Test a two global group system. "];
two_grp(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(200)),

    ?line Dir = ?config(priv_dir, Config),
    ?line File = filename:join(Dir, "global_group.config"),
    ?line {ok, Fd} = file:open(File, [write]),

    [Ncp1,Ncp2,Ncp3,Ncpx,Ncpy,Ncpz,Ncpq] = 
        node_names([cp1,cp2,cp3,cpx,cpy,cpz,cpq], Config),
    ?line config(Fd, Ncp1, Ncp2, Ncp3, Ncpx, Ncpy, Ncpz, Ncpq),

    ?line Cp1nn = node_at(Ncp1),
    ?line Cp2nn = node_at(Ncp2),
    ?line Cp3nn = node_at(Ncp3),
    ?line Cpxnn = node_at(Ncpx),
    ?line Cpynn = node_at(Ncpy),
    ?line Cpznn = node_at(Ncpz),

    ?line {ok, Cp1} = start_node(Ncp1, Config),
    ?line {ok, Cp2} = start_node(Ncp2, Config),
    ?line {ok, Cp3} = start_node(Ncp3, Config),
    ?line {ok, Cpx} = start_node(Ncpx, Config),
    ?line {ok, Cpy} = start_node(Ncpy, Config),
    ?line {ok, Cpz} = start_node(Ncpz, Config),

    %% The groups (cpq not started):
    %% [{nc1, [cp1,cp2,cp3]}, {nc2, [cpx,cpy,cpz]}, {nc3, [cpq]}]

    % sleep a while to make the global_groups to sync...
    test_server:sleep(1000),

    % check the global group names
    ?line {nc1, [nc2, nc3]} = rpc:call(Cp1, global_group, global_groups, []),
    ?line {nc1, [nc2, nc3]} = rpc:call(Cp2, global_group, global_groups, []),
    ?line {nc1, [nc2, nc3]} = rpc:call(Cp3, global_group, global_groups, []),
    ?line {nc2, [nc1, nc3]} = rpc:call(Cpx, global_group, global_groups, []),
    ?line {nc2, [nc1, nc3]} = rpc:call(Cpy, global_group, global_groups, []),
    ?line {nc2, [nc1, nc3]} = rpc:call(Cpz, global_group, global_groups, []),

    % check the global group nodes
    ?line [Cp1nn, Cp2nn, Cp3nn] = rpc:call(Cp1, global_group, own_nodes, []),
    ?line [Cp1nn, Cp2nn, Cp3nn] = rpc:call(Cp2, global_group, own_nodes, []),
    ?line [Cp1nn, Cp2nn, Cp3nn] = rpc:call(Cp3, global_group, own_nodes, []),
    ?line [Cpxnn, Cpynn, Cpznn] = rpc:call(Cpx, global_group, own_nodes, []),
    ?line [Cpxnn, Cpynn, Cpznn] = rpc:call(Cpy, global_group, own_nodes, []),
    ?line [Cpxnn, Cpynn, Cpznn] = rpc:call(Cpz, global_group, own_nodes, []),


    % start a proc and register it
    ?line {Pid1, yes} = rpc:call(Cp1, ?MODULE, start_proc, [test]),

    ?line Pid1 = rpc:call(Cp1, global_group, send, [test, {io, from_cp1}]),
    ?line Pid1 = rpc:call(Cpx, global_group, send, [test, {io, from_cpx}]),
    ?line Pid1 = rpc:call(Cp1, global_group, send, [{group,nc1}, test, 
                                                    {io, from_cp1}]),
    ?line [test] = 
        rpc:call(Cpx, global_group, registered_names, [{node, Cp1nn}]),
    ?line [test] = 
        rpc:call(Cpx, global_group, registered_names, [{group, nc1}]),
    ?line [] = rpc:call(Cpx, global_group, registered_names, [{node, Cpxnn}]),
    ?line [] = rpc:call(Cpx, global_group, registered_names, [{group, nc2}]),
    ?line Pid1 = rpc:call(Cpx, global_group, send, [{group,nc1}, test, 
                                                    {io, from_cp1}]),
    ?line {badarg,{test,{io,from_cpx}}} = 
	rpc:call(Cp1, global_group, send, [{group,nc2}, test, {io, from_cpx}]),
    ?line {badarg,{test,{io,from_cpx}}} = 
	rpc:call(Cpx, global_group, send, [{group,nc2}, test, {io, from_cpx}]),



    % test that it is registered at all nodes
    ?line Pid1 = rpc:call(Cp1, global, whereis_name, [test]),
    ?line Pid1 = rpc:call(Cp2, global, whereis_name, [test]),
    ?line Pid1 = rpc:call(Cp3, global, whereis_name, [test]),
    ?line undefined = rpc:call(Cpx, global, whereis_name, [test]),
    ?line undefined = rpc:call(Cpy, global, whereis_name, [test]),
    ?line undefined = rpc:call(Cpz, global, whereis_name, [test]),
    
    % start a proc and register it
    ?line {PidX, yes} = rpc:call(Cpx, ?MODULE, start_proc, [test]),

    % test that it is registered at all nodes
    ?line Pid1 = rpc:call(Cp1, global, whereis_name, [test]),
    ?line Pid1 = rpc:call(Cp2, global, whereis_name, [test]),
    ?line Pid1 = rpc:call(Cp3, global, whereis_name, [test]),
    ?line PidX = rpc:call(Cpx, global, whereis_name, [test]),
    ?line PidX = rpc:call(Cpy, global, whereis_name, [test]),
    ?line PidX = rpc:call(Cpz, global, whereis_name, [test]),
    
    Pid1 ! die,
    %% If we don't wait for global on other nodes to have updated its
    %% tables, 'test' may still be defined at the point when it is
    %% tested a few lines below.
    ?line
    ?UNTIL(begin 
               Pid = rpc:call(Cp2, global, whereis_name, [test]),
               undefined =:= Pid
           end),

    % start a proc and register it
    ?line {Pid2, yes} = rpc:call(Cp2, ?MODULE, start_proc, [test2]),

    % test that it is registered at all nodes
    ?line Pid2 = rpc:call(Cp1, global, whereis_name, [test2]),
    ?line Pid2 = rpc:call(Cp2, global, whereis_name, [test2]),
    ?line Pid2 = rpc:call(Cp3, global, whereis_name, [test2]),
    ?line PidX = rpc:call(Cpx, global, whereis_name, [test]),
    ?line PidX = rpc:call(Cpy, global, whereis_name, [test]),
    ?line PidX = rpc:call(Cpz, global, whereis_name, [test]),
    
    ?line undefined = rpc:call(Cp1, global, whereis_name, [test]),
    ?line undefined = rpc:call(Cp2, global, whereis_name, [test]),
    ?line undefined = rpc:call(Cp3, global, whereis_name, [test]),
    ?line undefined = rpc:call(Cpx, global, whereis_name, [test2]),
    ?line undefined = rpc:call(Cpy, global, whereis_name, [test2]),
    ?line undefined = rpc:call(Cpz, global, whereis_name, [test2]),
    

    ?line Pid2 = rpc:call(Cp1, global_group, send, [test2, {ping, self()}]),
    receive
	{pong, Cp2} -> ok
    after
	2000 -> test_server:fail(timeout2)
    end,
    ?line Pid2 = rpc:call(Cp2, global_group, send, [test2, {ping, self()}]),
    receive
	{pong, Cp2} -> ok
    after
	2000 -> test_server:fail(timeout2)
    end,
    ?line Pid2 = rpc:call(Cp3, global_group, send, [test2, {ping, self()}]),
    receive
	{pong, Cp2} -> ok
    after
	2000 -> test_server:fail(timeout2)
    end,

    ?line PidX = rpc:call(Cpx, global_group, send, [test, {ping, self()}]),
    receive
	{pong, Cpx} -> ok
    after
	2000 -> test_server:fail(timeout2)
    end,
    ?line PidX = rpc:call(Cpy, global_group, send, [test, {ping, self()}]),
    receive
	{pong, Cpx} -> ok
    after
	2000 -> test_server:fail(timeout2)
    end,
    ?line PidX = rpc:call(Cpz, global_group, send, [test, {ping, self()}]),
    receive
	{pong, Cpx} -> ok
    after
	2000 -> test_server:fail(timeout2)
    end,

    ?line Pid2 = rpc:call(Cpx, global_group, send, [{node, Cp1nn}, test2, 
                                                    {ping, self()}]),
    receive
	{pong, Cp2} -> ok
    after
	2000 -> test_server:fail(timeout2)
    end,
    ?line Pid2 = rpc:call(Cpy, global_group, send, [{node, Cp2nn}, test2, 
                                                    {ping, self()}]),
    receive
	{pong, Cp2} -> ok
    after
	2000 -> test_server:fail(timeout2)
    end,
    ?line Pid2 = rpc:call(Cpz, global_group, send, [{node, Cp3nn}, test2, 
                                                    {ping, self()}]),
    receive
	{pong, Cp2} -> ok
    after
	2000 -> test_server:fail(timeout2)
    end,

    ?line PidX = rpc:call(Cpx, global_group, send, [{node, Cpznn}, test, 
                                                    {ping, self()}]),
    receive
	{pong, Cpx} -> ok
    after
	2000 -> test_server:fail(timeout2)
    end,
    ?line PidX = rpc:call(Cpy, global_group, send, [{node, Cpxnn}, test, 
                                                    {ping, self()}]),
    receive
	{pong, Cpx} -> ok
    after
	2000 -> test_server:fail(timeout2)
    end,
    ?line PidX = rpc:call(Cpz, global_group, send, [{node, Cpynn}, test, 
                                                    {ping, self()}]),
    receive
	{pong, Cpx} -> ok
    after
	2000 -> test_server:fail(timeout2)
    end,

    ?line Pid2 = rpc:call(Cpx, global_group, send, [{group, nc1}, test2, 
                                                    {ping, self()}]),
    receive
	{pong, Cp2} -> ok
    after
	2000 -> test_server:fail(timeout2)
    end,
    ?line PidX = rpc:call(Cpy, global_group, send, [{group, nc2}, test, 
                                                    {ping, self()}]),
    receive
	{pong, Cpx} -> ok
    after
	2000 -> test_server:fail(timeout2)
    end,

    %%------------------------------------
    %% Test monitor nodes
    %%------------------------------------
    ?line Pid2 = 
        rpc:call(Cp1, global_group, send, [{node, Cp2nn}, test2, monitor]),
    ?line PidX = 
        rpc:call(Cpx, global_group, send, [{node, Cpxnn}, test, monitor]),    


    % Kill node Cp1
    ?line Pid2 = rpc:call(Cp2, global_group, send, [{node, Cp2nn}, test2, 
                                                    {wait_nodedown, Cp1}]),
    ?line PidX = rpc:call(Cpx, global_group, send, [{node, Cpxnn}, test, 
                                                    {wait_nodedown, Cp1}]),
    ?line test_server:sleep(100),
    ?line stop_node(Cp1),
    ?line test_server:sleep(1000),
    
    ?line ok = assert_loop(Cp2, Cp2nn, test2, Pid2, loop),
    ?line ok = assert_loop(Cpx, Cpxnn, test, PidX, loop_nodedown),
    ?line  PidX = 
	rpc:call(Cpx, global_group, send, [{node, Cpxnn}, test, to_loop]),

    % Kill node Cpz
    ?line Pid2 = rpc:call(Cp2, global_group, send, [{node, Cp2nn}, test2, 
                                                    {wait_nodedown, Cpz}]),
    ?line PidX = rpc:call(Cpx, global_group, send, [{node, Cpxnn}, test, 
                                                    {wait_nodedown, Cpz}]),
    ?line test_server:sleep(100),
    ?line stop_node(Cpz),
    ?line test_server:sleep(1000),
    
    ?line ok = assert_loop(Cp2, Cp2nn, test2, Pid2, loop_nodedown),
    ?line ok = assert_loop(Cpx, Cpxnn, test, PidX, loop),
    ?line Pid2 = 
	rpc:call(Cp2, global_group, send, [{node, Cp2nn}, test2, to_loop]),

    % Restart node Cp1
    ?line [Cp1nn, Cp2nn, Cp3nn] = rpc:call(Cp2, global_group, own_nodes, []),
    ?line Pid2 = rpc:call(Cp2, global_group, send, [{node, Cp2nn}, test2, 
                                                    {wait_nodeup, Cp1}]),
    ?line PidX = rpc:call(Cpx, global_group, send, [{node, Cpxnn}, test, 
                                                    {wait_nodeup, Cp1}]),
    ?line test_server:sleep(100),
    ?line {ok, Cp1} = start_node(Ncp1, Config),
    ?line test_server:sleep(5000),
    
    ?line ok = assert_loop(Cp2, Cp2nn, test2, Pid2, loop),
    ?line ok = assert_loop(Cpx, Cpxnn, test, PidX, loop_nodeup),
    ?line PidX = 
	rpc:call(Cpx, global_group, send, [{node, Cpxnn}, test, to_loop]),


    % Restart node Cpz
    ?line Pid2 = rpc:call(Cp2, global_group, send, [{node, Cp2nn}, test2, 
                                                    {wait_nodeup, Cpz}]),
    ?line PidX = rpc:call(Cpx, global_group, send, [{node, Cpxnn}, test, 
                                                    {wait_nodeup, Cpz}]),
    ?line test_server:sleep(100),
    ?line {ok, Cpz} = start_node(Ncpz, Config),
    ?line test_server:sleep(5000),
    
    ?line ok = assert_loop(Cp2, Cp2nn, test2, Pid2, loop_nodeup),
    ?line ok = assert_loop(Cpx, Cpxnn, test, PidX, loop),
    ?line Pid2 = 
	rpc:call(Cp2, global_group, send, [{node, Cp2nn}, test2, to_loop]),


    Pid2 ! die,
    PidX ! die,

    stop_node(Cp1),
    stop_node(Cp2),
    stop_node(Cp3),
    stop_node(Cpx),
    stop_node(Cpy),
    stop_node(Cpz),

    ?line test_server:timetrap_cancel(Dog),
    ok.
    


hidden_groups(suite) -> [];
hidden_groups(doc) -> ["Test hidden global groups."];
hidden_groups(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(200)),

    ?line Dir = ?config(priv_dir, Config),
    ?line File = filename:join(Dir, "global_group.config"),
    ?line {ok, Fd} = file:open(File, [write]),

    [Ncp1,Ncp2,Ncp3,Ncpx,Ncpy,Ncpz,Ncpq] = 
        node_names([cp1,cp2,cp3,cpx,cpy,cpz,cpq], Config),
    ?line config_hidden(Fd, Ncp1, Ncp2, Ncp3, Ncpx, Ncpy, Ncpz, Ncpq),

    ?line {ok, Cp1} = start_node(Ncp1, Config),
    ?line {ok, Cp2} = start_node(Ncp2, Config),
    ?line {ok, Cp3} = start_node(Ncp3, Config),
    ?line {ok, Cpx} = start_node(Ncpx, Config),
    ?line {ok, Cpy} = start_node(Ncpy, Config),
    ?line {ok, Cpz} = start_node(Ncpz, Config),
    ?line {ok, Cpq} = start_node(Ncpq, Config),

    % sleep a while to make the global_groups to sync...
    test_server:sleep(1000),

    % check the global group names
    ?line {nc1, [nc2, nc3]} = rpc:call(Cp1, global_group, global_groups, []),
    ?line {nc1, [nc2, nc3]} = rpc:call(Cp2, global_group, global_groups, []),
    ?line {nc1, [nc2, nc3]} = rpc:call(Cp3, global_group, global_groups, []),
    ?line {nc2, [nc1, nc3]} = rpc:call(Cpx, global_group, global_groups, []),
    ?line {nc2, [nc1, nc3]} = rpc:call(Cpy, global_group, global_groups, []),
    ?line {nc2, [nc1, nc3]} = rpc:call(Cpz, global_group, global_groups, []),

    % check the global group nodes
    ?line [Cp1, Cp2, Cp3] = rpc:call(Cp1, global_group, own_nodes, []),
    ?line [Cp1, Cp2, Cp3] = rpc:call(Cp2, global_group, own_nodes, []),
    ?line [Cp1, Cp2, Cp3] = rpc:call(Cp3, global_group, own_nodes, []),
    ?line [Cpx, Cpy, Cpz] = rpc:call(Cpx, global_group, own_nodes, []),
    ?line [Cpx, Cpy, Cpz] = rpc:call(Cpy, global_group, own_nodes, []),
    ?line [Cpx, Cpy, Cpz] = rpc:call(Cpz, global_group, own_nodes, []),
    ?line [Cpq]           = rpc:call(Cpq, global_group, own_nodes, []),

    % Make some inter group connections
    ?line pong = rpc:call(Cp1, net_adm, ping, [Cpx]),
    ?line pong = rpc:call(Cpy, net_adm, ping, [Cp2]),
    ?line pong = rpc:call(Cp3, net_adm, ping, [Cpx]),
    ?line pong = rpc:call(Cpz, net_adm, ping, [Cp3]),
    ?line pong = rpc:call(Cpq, net_adm, ping, [Cp1]),
    ?line pong = rpc:call(Cpz, net_adm, ping, [Cpq]),

    % Check that no inter group connections are visible
    NC1Nodes = lists:sort([Cp1, Cp2, Cp3]),
    NC2Nodes = lists:sort([Cpx, Cpy, Cpz]),
    ?line NC1Nodes = lists:sort([Cp1|rpc:call(Cp1, erlang, nodes, [])]),
    ?line NC1Nodes = lists:sort([Cp2|rpc:call(Cp2, erlang, nodes, [])]),
    ?line NC1Nodes = lists:sort([Cp3|rpc:call(Cp3, erlang, nodes, [])]),
    ?line NC2Nodes = lists:sort([Cpx|rpc:call(Cpx, erlang, nodes, [])]),
    ?line NC2Nodes = lists:sort([Cpy|rpc:call(Cpy, erlang, nodes, [])]),
    ?line NC2Nodes = lists:sort([Cpz|rpc:call(Cpz, erlang, nodes, [])]),
    NC12Nodes = lists:append(NC1Nodes, NC2Nodes),
    ?line false = lists:any(fun(N) -> lists:member(N, NC12Nodes) end,
			    rpc:call(Cpq, erlang, nodes, [])),


    stop_node(Cp1),
    stop_node(Cp2),
    stop_node(Cp3),
    stop_node(Cpx),
    stop_node(Cpy),
    stop_node(Cpz),
    stop_node(Cpq),

    ?line test_server:timetrap_cancel(Dog),
    ok.
    

test_exit(suite) -> [];
test_exit(doc) -> ["Checks when the search process exits. "];
test_exit(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(120)),

    ?line NN = node_name(atom_to_list(node())),
    ?line Cp1nn = list_to_atom("cp1@" ++ NN),

    ?line {ok, Cp1} = start_node(cp1, Config),
    ?line {ok, Cp2} = start_node(cp2, Config),
    ?line {ok, Cp3} = start_node(cp3, Config),

    test_server:sleep(1000),

    ?line {error, illegal_function_call} = 
        rpc:call(Cp1, global_group, registered_names_test, [{node, Cp1nn}]),
    ?line {badarg,_} = 
        rpc:call(Cp1, global_group, send, [king, "The message"]),
    ?line undefined = rpc:call(Cp1, global_group, whereis_name, [king]),

    % stop the nodes, and make sure names are released.
    stop_node(Cp1),
    stop_node(Cp2),
    stop_node(Cp3),

    % sleep to let the nodes die
    test_server:sleep(1000),

    ?line test_server:timetrap_cancel(Dog),
    ok.
    

start_node(Name, Config) ->
    Pa=filename:dirname(code:which(?MODULE)),
    Dir=?config(priv_dir, Config),
    ConfFile = "  -config " ++ filename:join(Dir, "global_group"),
    test_server:start_node(Name, slave, [{args, "-pa " ++ Pa ++ ConfFile}]).

start_node_no(Name, Config) ->
    Pa=filename:dirname(code:which(?MODULE)),
    Dir=?config(priv_dir, Config),
    ConfFile = "  -config " ++ filename:join(Dir, "no_global_group"),
    test_server:start_node(Name, slave, [{args, "-pa " ++ Pa ++ ConfFile}]).

start_node_no2(Name, Config) ->
    Pa=filename:dirname(code:which(?MODULE)),
    Dir=?config(priv_dir, Config),
    ConfFile = "  -config " ++ filename:join(Dir, "no_global_group_sync"),
    test_server:start_node(Name, slave, [{args, "-pa " ++ Pa ++ ConfFile}]).

start_node_comp(Name, Config) ->
    Pa=filename:dirname(code:which(?MODULE)),
    Dir=?config(priv_dir, Config),
    ConfFile = "  -config " ++ filename:join(Dir, "global_group_comp"),
    test_server:start_node(Name, slave, [{args, "-pa " ++ Pa ++ ConfFile}]).

node_names(Names, Config) ->
    [node_name(Name, Config) || Name <- Names].

node_name(Name, Config) ->
    U = "_",
    Pid = os:getpid(),
    {{Y,M,D}, {H,Min,S}} = calendar:now_to_local_time(now()),
    Date = io_lib:format("~4w_~2..0w_~2..0w__~2..0w_~2..0w_~2..0w", 
                         [Y,M,D, H,Min,S]),
    L = lists:flatten(Date),
    lists:concat([Name,U,?testcase,U,Pid,U,U,L]).

stop_node(Node) ->
    ?t:stop_node(Node).


wait_for_ready_net() ->
    Nodes = lists:sort(?NODES),
    ?UNTIL(begin
               lists:all(fun(N) -> Nodes =:= get_known(N) end, Nodes) and
               lists:all(fun(N) -> 
                                 LNs = rpc:call(N, erlang, nodes, []),
                                 Nodes =:= lists:sort([N | LNs])
                         end, Nodes)
           end).

get_known(Node) ->
    Known = gen_server:call({global_name_server,Node}, get_known),
    lists:sort([Node | Known]).

config_hidden(Fd, Ncp1, Ncp2, Ncp3, Ncpx, Ncpy, Ncpz, Ncpq) ->
    M = from($@, atom_to_list(node())),
    io:format(Fd, "[{kernel, [{sync_nodes_optional, ['~s@~s','~s@~s','~s@~s', "
                                                   " '~s@~s','~s@~s','~s@~s']},"
	                     "{sync_nodes_timeout, 1000},"
                             "{global_groups, [{nc1, hidden, ['~s@~s','~s@~s','~s@~s']}, "
                                              "{nc2, hidden, ['~s@~s','~s@~s','~s@~s']}, "
	                                      "{nc3, normal, ['~s@~s']}]} ] }]. ~n",
	      [Ncp1, M, Ncp2, M, Ncp3, M,  
               Ncpx, M, Ncpy, M, Ncpz, M, 
               Ncp1, M, Ncp2, M, Ncp3, M,  
               Ncpx, M, Ncpy, M, Ncpz, M, 
               Ncpq, M]).

config(Fd, Ncp1, Ncp2, Ncp3, Ncpx, Ncpy, Ncpz, Ncpq) ->
    M = from($@, atom_to_list(node())),
    io:format(Fd, "[{kernel, [{sync_nodes_optional, ['~s@~s','~s@~s','~s@~s', "
                                                   " '~s@~s','~s@~s','~s@~s']},"
	          "{sync_nodes_timeout, 1000},"
                  "{global_groups, [{nc1, ['~s@~s','~s@~s','~s@~s']}, "
                                " {nc2, ['~s@~s','~s@~s','~s@~s']}, "
	                         "{nc3, ['~s@~s']}]} ] }]. ~n",
	      [Ncp1, M, Ncp2, M, Ncp3, M,  
               Ncpx, M, Ncpy, M, Ncpz, M, 
               Ncp1, M, Ncp2, M, Ncp3, M,  
               Ncpx, M, Ncpy, M, Ncpz, M, 
               Ncpq, M]).

config_no(Fd) ->
    io:format(Fd, "[{kernel, [{global_groups, []}]}]. ~n",[]).

config_sync(Fd, Ncp1, Ncp2, Ncp3, Ncpx, Ncpy, Ncpz) ->
    M = from($@, atom_to_list(node())),
    io:format(Fd, "[{kernel, [{sync_nodes_optional, ['~s@~s','~s@~s','~s@~s', "
                                                   " '~s@~s','~s@~s','~s@~s']},"
	          "{sync_nodes_timeout, 1000},"
                  "{global_groups, []} ] }] .~n",
	      [Ncp1, M, Ncp2, M, Ncp3, M,  
               Ncpx, M, Ncpy, M, Ncpz, M]).


config_comp(Fd, Ncp1, Ncp2, Ncp3, Ncpx, Ncpy, Ncpz) ->
    M = from($@, atom_to_list(node())),
    io:format(Fd, "[{kernel, [{sync_nodes_optional, ['~s@~s','~s@~s','~s@~s', "
                                                   " '~s@~s','~s@~s','~s@~s']},"
	          "{sync_nodes_timeout, 1000} ] }] .~n",
	      [Ncp1, M, Ncp2, M, Ncp3, M,  
               Ncpx, M, Ncpy, M, Ncpz, M]).

node_at(N) ->
    NN = node_name(atom_to_list(node())),
    list_to_atom(lists:concat([N, "@", NN])).

node_name(L) ->
    from($@, L).

from(H, [H | T]) -> T;
from(H, [_ | T]) -> from(H, T);
from(_, []) -> [].


start_proc(Name) ->
    Pid = spawn(?MODULE, init, [self(), Name]),
    receive
	{Pid, Res} -> {Pid, Res}
    end.
    
start_proc_rereg(Name) ->
    Pid = spawn(?MODULE, init2, [self(), Name]),
    receive
	Pid -> Pid
    end.







init(Parent) ->
    Parent ! self(),
    loop().

init(Parent, Name) ->
    X = global:register_name(Name, self()),
    Parent ! {self(),X},
    loop().

init2(Parent, Name) ->
    global:re_register_name(Name, self()),
    Parent ! self(),
    loop().

loop() ->
    receive
	monitor ->
	    global_group:monitor_nodes(true),
	    loop();
	stop_monitor ->
	    global_group:monitor_nodes(false),
	    loop();
	{wait_nodeup, Node} ->
	    loop_nodeup(Node);
	{wait_nodedown, Node} ->
	    loop_nodedown(Node);
	{io, _Msg} ->
	    loop();
	{ping, From} ->
	    From ! {pong, node()},
	    loop();
	{del_lock, Id} ->
	    global:del_lock({Id, self()}),
	    loop();
	{del_lock, Id, Nodes} ->
	    global:del_lock({Id, self()}, Nodes),
	    loop();
	{set_lock, Id, From} ->
	    Res = global:set_lock({Id, self()}, ?NODES, 1),
	    From ! Res,
	    loop();
	{set_lock, Id, From, Nodes} ->
	    Res = global:set_lock({Id, self()}, Nodes, 1),
	    From ! Res,
	    loop();
	{set_lock_loop, Id, From} ->
	    global:set_lock({Id, self()}, ?NODES),
	    From ! {got_lock, self()},
	    loop();
	{{got_notify, From}, Ref} ->
	    receive
		X when element(1, X) == global_name_conflict ->
		    From ! {Ref, yes}
	    after
		0 -> From ! {Ref, no}
	    end,
	    loop();
	{which_loop, From} ->
	    From ! loop,
	    loop();
	die ->
	    exit(normal)
    end.


loop_nodeup(Node) ->
    receive
	{nodeup, Node} ->
	    loop();
	to_loop ->
	    loop();
	{which_loop, From} ->
	    From ! loop_nodeup,
	    loop_nodeup(Node);
	die ->
	    exit(normal)
    end.


loop_nodedown(Node) ->
    receive
	{nodedown, Node} ->
	    loop();
	to_loop ->
	    loop();
	{which_loop, From} ->
	    From ! loop_nodedown,
	    loop_nodedown(Node);
	die ->
	    exit(normal)
    end.

assert_loop(Cp, CpName, Name, NamePid, Loop) ->
    M = {which_loop, self()},
    NamePid = rpc:call(Cp, global_group, send, [{node, CpName}, Name, M]),
    receive
        Loop ->
            ok;
        Other1 ->
            test_server:fail(Other1)
    after 5000 ->
            test_server:fail(timeout)
    end.

loop_until_true(Fun) ->
    case Fun() of
	true ->
	    ok;
	_ ->
	    loop_until_true(Fun)
    end.

