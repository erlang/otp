%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2022. All Rights Reserved.
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

-module(global_group_SUITE).

-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2,
	 init_per_suite/1, end_per_suite/1]).
-export([start_gg_proc/1, no_gg_proc/1, no_gg_proc_sync/1, compatible/1, 
	 one_grp/1, one_grp_x/1, two_grp/1, hidden_groups/1, test_exit/1,
         global_disconnect/1]).
-export([init/1, init/2, init2/2, start_proc/1, start_proc_rereg/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

%%-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(NODES, [node()|nodes()]).

-define(UNTIL(Seq), loop_until_true(fun() -> Seq end)).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,5}}].

all() -> 
    [start_gg_proc, no_gg_proc, no_gg_proc_sync, compatible,
     one_grp, one_grp_x, two_grp, test_exit, hidden_groups,
     global_disconnect].

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
-define(testcase, proplists:get_value(?TESTCASE, Config)).

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

%%-----------------------------------------------------------------
%% Test suites for global groups.
%% Should be started in a CC view with:
%% erl -sname XXX -rsh ctrsh where XXX not in [cp1 .. cpN]
%%-----------------------------------------------------------------


%% Check that the global_group processes are started automatically. .
start_gg_proc(Config) when is_list(Config) ->
    Dir = proplists:get_value(priv_dir, Config),
    File = filename:join(Dir, "global_group.config"),
    {ok, Fd}=file:open(File, [write]),
    [Ncp1,Ncp2,Ncp3] = [?CT_PEER_NAME() || _ <- [cp1, cp2, cp3]],
    config(Fd, Ncp1, Ncp2, Ncp3, "cpx", "cpy", "cpz", "cpq"),

    Args = ["-config", filename:join(Dir, "global_group")],

    {ok, Peer1, Cp1} = ?CT_PEER(#{name => Ncp1, args => Args}),
    {ok, Peer2, Cp2} = ?CT_PEER(#{name => Ncp2, args => Args}),
    {ok, Peer3, Cp3} = ?CT_PEER(#{name => Ncp3, args => Args}),

    [] = rpc:call(Cp1, global_group, registered_names, [{node, Cp1}]),
    [] = rpc:call(Cp2, global_group, registered_names, [{node, Cp2}]),
    [] = rpc:call(Cp3, global_group, registered_names, [{node, Cp3}]),

    %% stop the nodes, and make sure names are released.
    peer:stop(Peer1),
    peer:stop(Peer2),
    peer:stop(Peer3),

    ?UNTIL(undefined =:= global:whereis_name(test)),
    ok.



%% Start a system without global groups. Nodes are not
%% synced at start (sync_nodes_optional is not defined).
no_gg_proc(Config) when is_list(Config) ->
    Dir = proplists:get_value(priv_dir, Config),
    File = filename:join(Dir, "no_global_group.config"),
    {ok, Fd} = file:open(File, [write]),
    config_no(Fd),

    Args = ["-config", filename:join(Dir, "no_global_group")],
    {ok, Peer1, Cp1} = ?CT_PEER(#{args => Args}),
    {ok, Peer2, Cp2} = ?CT_PEER(#{args => Args}),
    {ok, Peer3, Cp3} = ?CT_PEER(#{args => Args}),
    {ok, Peerx, Cpx} = ?CT_PEER(#{args => Args}),
    {ok, Peery, Cpy} = ?CT_PEER(#{args => Args}),
    {ok, Peerz, Cpz} = ?CT_PEER(#{args => Args}),

    %% let the nodes know of each other
    pong = rpc:call(Cp1, net_adm, ping, [Cp2]),
    pong = rpc:call(Cp2, net_adm, ping, [Cp3]),
    pong = rpc:call(Cp3, net_adm, ping, [Cpx]),
    pong = rpc:call(Cpx, net_adm, ping, [Cpy]),
    pong = rpc:call(Cpy, net_adm, ping, [Cpz]),

    wait_for_ready_net(),

    [test_server] = rpc:call(Cp1, global_group, registered_names, [{node, Cp1}]),
    [test_server] = rpc:call(Cp2, global_group, registered_names, [{node, Cp2}]),
    [test_server] = rpc:call(Cp3, global_group, registered_names, [{node, Cp3}]),
    [test_server] = rpc:call(Cp1, global_group, registered_names, [{node, Cpx}]),
    [test_server] = rpc:call(Cp2, global_group, registered_names, [{node, Cpy}]),
    [test_server] = rpc:call(Cp3, global_group, registered_names, [{node, Cpz}]),


    %% start a proc and register it
    {Pid2, yes} = rpc:call(Cp2, ?MODULE, start_proc, [test2]),

    RegNames = lists:sort([test2,test_server]),

    RegNames =
	lists:sort(
	  rpc:call(Cp1, global_group, registered_names, [{node, Cp1}])),
    RegNames =
	lists:sort(
	  rpc:call(Cp2, global_group, registered_names, [{node, Cp2}])),
    RegNames =
	lists:sort(
	  rpc:call(Cp3, global_group, registered_names, [{node, Cp3}])),
    RegNames =
	lists:sort(
	  rpc:call(Cp1, global_group, registered_names, [{node, Cpx}])),
    RegNames =
	lists:sort(
	  rpc:call(Cp2, global_group, registered_names, [{node, Cpy}])),
    RegNames =
	lists:sort(
	  rpc:call(Cp3, global_group, registered_names, [{node, Cpz}])),


    undefined = rpc:call(Cp3, global_group, global_groups, []),

    Own_nodes_should = [node(), Cp1, Cp2, Cp3,
			Cpx, Cpy, Cpz],
    Own_nodes = rpc:call(Cp3, global_group, own_nodes, []),
    [] = (Own_nodes -- Own_nodes_should),
    [] = (Own_nodes_should -- Own_nodes),

    Pid2 = rpc:call(Cp1, global_group, send, [test2, {ping, self()}]),
    receive
	{pong, Cp2} -> ok
    after
	2000 -> ct:fail(timeout2)
    end,
    Pid2 = rpc:call(Cp2, global_group, send, [test2, {ping, self()}]),
    receive
	{pong, Cp2} -> ok
    after
	2000 -> ct:fail(timeout3)
    end,
    Pid2 = rpc:call(Cpz, global_group, send, [test2, {ping, self()}]),
    receive
	{pong, Cp2} -> ok
    after
	2000 -> ct:fail(timeout4)
    end,


    %% start a proc and register it
    {PidX, yes} = rpc:call(Cpx, ?MODULE, start_proc, [test]),


    %%------------------------------------
    %% Test monitor nodes
    %%------------------------------------
    Pid2 = rpc:call(Cp1, global_group, send, [{node, Cp2}, test2, monitor]),
    PidX = rpc:call(Cpx, global_group, send, [{node, Cpx}, test, monitor]),


    %% Kill node Cp1
    Pid2 =
	rpc:call(Cp2, global_group, send, [{node, Cp2}, test2, {wait_nodedown, Cp1}]),
    PidX =
	rpc:call(Cpx, global_group, send, [{node, Cpx}, test, {wait_nodedown, Cp1}]),
    ct:sleep(100),
    peer:stop(Peer1),
    ct:sleep(1000),

    ok = assert_loop(Cp2, Cp2, test2, Pid2, loop),
    ok = assert_loop(Cpx, Cpx, test, PidX, loop),

    %% Kill node Cpz
    Pid2 =
	rpc:call(Cp2, global_group, send, [{node, Cp2}, test2, {wait_nodedown, Cpz}]),
    PidX =
	rpc:call(Cpx, global_group, send, [{node, Cpx}, test, {wait_nodedown, Cpz}]),
    ct:sleep(100),
    peer:stop(Peerz),
    ct:sleep(1000),

    ok = assert_loop(Cp2, Cp2, test2, Pid2, loop),
    ok = assert_loop(Cpx, Cpx, test, PidX, loop),

    %% Restart node Cp1
    Pid2 =
	rpc:call(Cp2, global_group, send, [{node, Cp2}, test2, {wait_nodeup, Cp1}]),
    PidX =
	rpc:call(Cpx, global_group, send, [{node, Cpx}, test, {wait_nodeup, Cp1}]),
    {ok, Peer11, Cp1} = ?CT_PEER(#{name => Cp1, args => Args}),
    pong = rpc:call(Cp2, net_adm, ping, [Cp1]),
    pong = rpc:call(Cpx, net_adm, ping, [Cp1]),
    wait_for_ready_net(),

    ok = assert_loop(Cp2, Cp2, test2, Pid2, loop),
    ok = assert_loop(Cpx, Cpx, test, PidX, loop),

    %% Restart node Cpz
    Pid2 =
	rpc:call(Cp2, global_group, send, [{node, Cp2}, test2, {wait_nodeup, Cpz}]),
    PidX =
	rpc:call(Cpx, global_group, send, [{node, Cpx}, test, {wait_nodeup, Cpz}]),
    {ok, Peer1z, Cpz} = ?CT_PEER(#{name => Cpz, args => Args}),
    pong = rpc:call(Cp2, net_adm, ping, [Cpz]),
    pong = rpc:call(Cpx, net_adm, ping, [Cpz]),
    wait_for_ready_net(),

    ok = assert_loop(Cp2, Cp2, test2, Pid2, loop),
    ok = assert_loop(Cpx, Cpx, test, PidX, loop),

    %% stop the nodes, and make sure names are released.
    peer:stop(Peer11),
    peer:stop(Peer2),
    peer:stop(Peer3),
    peer:stop(Peerx),
    peer:stop(Peery),
    peer:stop(Peer1z),

    ?UNTIL(undefined =:= global:whereis_name(test)),
    ok.




%% Start a system without global groups, but syncing the nodes by using
%% sync_nodes_optional.
no_gg_proc_sync(Config) when is_list(Config) ->
    Dir = proplists:get_value(priv_dir, Config),
    File = filename:join(Dir, "no_global_group_sync.config"),
    {ok, Fd} = file:open(File, [write]),

    [Ncp1,Ncp2,Ncp3,Ncpx,Ncpy,Ncpz] = [?CT_PEER_NAME() ||
        _ <- [cp1,cp2,cp3,cpx,cpy,cpz]],
    config_sync(Fd, Ncp1, Ncp2, Ncp3, Ncpx, Ncpy, Ncpz),

    Args = ["-config", filename:join(Dir, "no_global_group_sync")],
    {ok, Peer1, Cp1} = ?CT_PEER(#{name => Ncp1, args => Args}),
    {ok, Peer2, Cp2} = ?CT_PEER(#{name => Ncp2, args => Args}),
    {ok, Peer3, Cp3} = ?CT_PEER(#{name => Ncp3, args => Args}),
    {ok, Peerx, Cpx} = ?CT_PEER(#{name => Ncpx, args => Args}),
    {ok, Peery, Cpy} = ?CT_PEER(#{name => Ncpy, args => Args}),
    {ok, Peerz, Cpz} = ?CT_PEER(#{name => Ncpz, args => Args}),

    %% let the nodes know of each other
    pong = rpc:call(Cp1, net_adm, ping, [Cp2]),
    pong = rpc:call(Cp2, net_adm, ping, [Cp3]),
    pong = rpc:call(Cp3, net_adm, ping, [Cpx]),
    pong = rpc:call(Cpx, net_adm, ping, [Cpy]),
    pong = rpc:call(Cpy, net_adm, ping, [Cpz]),

    wait_for_ready_net(),

    [test_server] = rpc:call(Cp1, global_group, registered_names, [{node, Cp1}]),
    [test_server] = rpc:call(Cp2, global_group, registered_names, [{node, Cp2}]),
    [test_server] = rpc:call(Cp3, global_group, registered_names, [{node, Cp3}]),
    [test_server] = rpc:call(Cp1, global_group, registered_names, [{node, Cpx}]),
    [test_server] = rpc:call(Cp2, global_group, registered_names, [{node, Cpy}]),
    [test_server] = rpc:call(Cp3, global_group, registered_names, [{node, Cpz}]),


    %% start a proc and register it
    {Pid2, yes} = rpc:call(Cp2, ?MODULE, start_proc, [test2]),

    RegNames = lists:sort([test2,test_server]),

    RegNames =
	lists:sort(
	  rpc:call(Cp1, global_group, registered_names, [{node, Cp1}])),
    RegNames =
	lists:sort(
	  rpc:call(Cp2, global_group, registered_names, [{node, Cp2}])),
    RegNames =
	lists:sort(
	  rpc:call(Cp3, global_group, registered_names, [{node, Cp3}])),
    RegNames =
	lists:sort(
	  rpc:call(Cp1, global_group, registered_names, [{node, Cpx}])),
    RegNames =
	lists:sort(
	  rpc:call(Cp2, global_group, registered_names, [{node, Cpy}])),
    RegNames =
	lists:sort(
	  rpc:call(Cp3, global_group, registered_names, [{node, Cpz}])),


    undefined = rpc:call(Cp3, global_group, global_groups, []),

    Own_nodes_should = [node(), Cp1, Cp2, Cp3,
			Cpx, Cpy, Cpz],
    Own_nodes = rpc:call(Cp3, global_group, own_nodes, []),
    [] = (Own_nodes -- Own_nodes_should),
    [] = (Own_nodes_should -- Own_nodes),

    Pid2 = rpc:call(Cp1, global_group, send, [test2, {ping, self()}]),
    receive
	{pong, Cp2} -> ok
    after
	2000 -> ct:fail(timeout2)
    end,
    Pid2 = rpc:call(Cp2, global_group, send, [test2, {ping, self()}]),
    receive
	{pong, Cp2} -> ok
    after
	2000 -> ct:fail(timeout3)
    end,
    Pid2 = rpc:call(Cpz, global_group, send, [test2, {ping, self()}]),
    receive
	{pong, Cp2} -> ok
    after
	2000 -> ct:fail(timeout4)
    end,


    %% start a proc and register it
    {PidX, yes} = rpc:call(Cpx, ?MODULE, start_proc, [test]),


    %%------------------------------------
    %% Test monitor nodes
    %%------------------------------------
    Pid2 = rpc:call(Cp1, global_group, send, [{node, Cp2}, test2, monitor]),
    PidX = rpc:call(Cpx, global_group, send, [{node, Cpx}, test, monitor]),


    %% Kill node Cp1
    Pid2 =
	rpc:call(Cp2, global_group, send, [{node, Cp2}, test2, {wait_nodedown, Cp1}]),
    PidX =
	rpc:call(Cpx, global_group, send, [{node, Cpx}, test, {wait_nodedown, Cp1}]),
    ct:sleep(100),
    peer:stop(Peer1),
    ct:sleep(1000),

    ok = assert_loop(Cp2, Cp2, test2, Pid2, loop),
    ok = assert_loop(Cpx, Cpx, test, PidX, loop),

    %% Kill node Cpz
    Pid2 =
	rpc:call(Cp2, global_group, send, [{node, Cp2}, test2, {wait_nodedown, Cpz}]),
    PidX =
	rpc:call(Cpx, global_group, send, [{node, Cpx}, test, {wait_nodedown, Cpz}]),
    ct:sleep(100),
    peer:stop(Peerz),
    ct:sleep(1000),

    ok = assert_loop(Cp2, Cp2, test2, Pid2, loop),
    ok = assert_loop(Cpx, Cpx, test, PidX, loop),

    %% Restart node Cp1
    Pid2 =
	rpc:call(Cp2, global_group, send, [{node, Cp2}, test2, {wait_nodeup, Cp1}]),
    PidX =
	rpc:call(Cpx, global_group, send, [{node, Cpx}, test, {wait_nodeup, Cp1}]),
    {ok, Peer11, Cp1} = ?CT_PEER(#{name => Cp1, args => Args}),
    pong = rpc:call(Cp2, net_adm, ping, [Cp1]),
    pong = rpc:call(Cpx, net_adm, ping, [Cp1]),
    wait_for_ready_net(),

    ok = assert_loop(Cp2, Cp2, test2, Pid2, loop),
    ok = assert_loop(Cpx, Cpx, test, PidX, loop),

    %% Restart node Cpz
    Pid2 =
	rpc:call(Cp2, global_group, send, [{node, Cp2}, test2, {wait_nodeup, Cpz}]),
    PidX =
	rpc:call(Cpx, global_group, send, [{node, Cpx}, test, {wait_nodeup, Cpz}]),
    {ok, Peerz1, Cpz} = ?CT_PEER(#{name => Cpz, args => Args}),
    pong = rpc:call(Cp2, net_adm, ping, [Cpz]),
    pong = rpc:call(Cpx, net_adm, ping, [Cpz]),
    wait_for_ready_net(),

    ok = assert_loop(Cp2, Cp2, test2, Pid2, loop),
    ok = assert_loop(Cpx, Cpx, test, PidX, loop),

    %% stop the nodes, and make sure names are released.
    peer:stop(Peer11),
    peer:stop(Peer2),
    peer:stop(Peer3),
    peer:stop(Peerx),
    peer:stop(Peery),
    peer:stop(Peerz1),

    ?UNTIL(undefined =:= global:whereis_name(test)),
    ok.




%% Check that a system without global groups is compatible with the old R4 system.
compatible(Config) when is_list(Config) ->
    Dir = proplists:get_value(priv_dir, Config),
    File = filename:join(Dir, "global_group_comp.config"),
    {ok, Fd} = file:open(File, [write]),

    [Ncp1,Ncp2,Ncp3,Ncpx,Ncpy,Ncpz] = [?CT_PEER_NAME() ||
        _ <- [cp1,cp2,cp3,cpx,cpy,cpz]],
    config_sync(Fd, Ncp1, Ncp2, Ncp3, Ncpx, Ncpy, Ncpz),

    Args = ["-config", filename:join(Dir, "global_group_comp")],
    {ok, Peer1, Cp1} = ?CT_PEER(#{args => Args}),
    {ok, Peer2, Cp2} = ?CT_PEER(#{args => Args}),
    {ok, Peer3, Cp3} = ?CT_PEER(#{args => Args}),
    {ok, Peerx, Cpx} = ?CT_PEER(#{args => Args}),
    {ok, Peery, Cpy} = ?CT_PEER(#{args => Args}),
    {ok, Peerz, Cpz} = ?CT_PEER(#{args => Args}),

    %% let the nodes know of each other
    pong = rpc:call(Cp1, net_adm, ping, [Cp2]),
    pong = rpc:call(Cp2, net_adm, ping, [Cp3]),
    pong = rpc:call(Cp3, net_adm, ping, [Cpx]),
    pong = rpc:call(Cpx, net_adm, ping, [Cpy]),
    pong = rpc:call(Cpy, net_adm, ping, [Cpz]),

    wait_for_ready_net(),

    [test_server] = rpc:call(Cp1, global_group, registered_names, [{node, Cp1}]),
    [test_server] = rpc:call(Cp2, global_group, registered_names, [{node, Cp2}]),
    [test_server] = rpc:call(Cp3, global_group, registered_names, [{node, Cp3}]),
    [test_server] = rpc:call(Cp1, global_group, registered_names, [{node, Cpx}]),
    [test_server] = rpc:call(Cp2, global_group, registered_names, [{node, Cpy}]),
    [test_server] = rpc:call(Cp3, global_group, registered_names, [{node, Cpz}]),


    %% start a proc and register it
    {Pid2, yes} = rpc:call(Cp2, ?MODULE, start_proc, [test2]),

    RegNames = lists:sort([test2,test_server]),

    RegNames =
	lists:sort(
	  rpc:call(Cp1, global_group, registered_names, [{node, Cp1}])),
    RegNames =
	lists:sort(
	  rpc:call(Cp2, global_group, registered_names, [{node, Cp2}])),
    RegNames =
	lists:sort(
	  rpc:call(Cp3, global_group, registered_names, [{node, Cp3}])),
    RegNames =
	lists:sort(
	  rpc:call(Cp1, global_group, registered_names, [{node, Cpx}])),
    RegNames =
	lists:sort(
	  rpc:call(Cp2, global_group, registered_names, [{node, Cpy}])),
    RegNames =
	lists:sort(
	  rpc:call(Cp3, global_group, registered_names, [{node, Cpz}])),


    undefined = rpc:call(Cp3, global_group, global_groups, []),

    Own_nodes_should = [node(), Cp1, Cp2, Cp3,
			Cpx, Cpy, Cpz],
    Own_nodes = rpc:call(Cp3, global_group, own_nodes, []),
    [] = (Own_nodes -- Own_nodes_should),
    [] = (Own_nodes_should -- Own_nodes),

    Pid2 = rpc:call(Cp1, global_group, send, [test2, {ping, self()}]),
    receive
	{pong, Cp2} -> ok
    after
	2000 -> ct:fail(timeout2)
    end,
    Pid2 = rpc:call(Cp2, global_group, send, [test2, {ping, self()}]),
    receive
	{pong, Cp2} -> ok
    after
	2000 -> ct:fail(timeout3)
    end,
    Pid2 = rpc:call(Cpz, global_group, send, [test2, {ping, self()}]),
    receive
	{pong, Cp2} -> ok
    after
	2000 -> ct:fail(timeout4)
    end,


    %% start a proc and register it
    {PidX, yes} = rpc:call(Cpx, ?MODULE, start_proc, [test]),


    %%------------------------------------
    %% Test monitor nodes
    %%------------------------------------
    Pid2 = rpc:call(Cp1, global_group, send, [{node, Cp2}, test2, monitor]),
    PidX = rpc:call(Cpx, global_group, send, [{node, Cpx}, test, monitor]),


    %% Kill node Cp1
    Pid2 =
	rpc:call(Cp2, global_group, send, [{node, Cp2}, test2, {wait_nodedown, Cp1}]),
    PidX =
	rpc:call(Cpx, global_group, send, [{node, Cpx}, test, {wait_nodedown, Cp1}]),
    ct:sleep(100),
    peer:stop(Peer1),
    ct:sleep(1000),

    ok = assert_loop(Cp2, Cp2, test2, Pid2, loop),
    ok = assert_loop(Cpx, Cpx, test, PidX, loop),

    %% Kill node Cpz
    Pid2 =
	rpc:call(Cp2, global_group, send, [{node, Cp2}, test2, {wait_nodedown, Cpz}]),
    PidX =
	rpc:call(Cpx, global_group, send, [{node, Cpx}, test, {wait_nodedown, Cpz}]),
    ct:sleep(100),
    peer:stop(Peerz),
    ct:sleep(1000),

    ok = assert_loop(Cp2, Cp2, test2, Pid2, loop),
    ok = assert_loop(Cpx, Cpx, test, PidX, loop),

    %% Restart node Cp1
    Pid2 =
	rpc:call(Cp2, global_group, send, [{node, Cp2}, test2, {wait_nodeup, Cp1}]),
    PidX =
	rpc:call(Cpx, global_group, send, [{node, Cpx}, test, {wait_nodeup, Cp1}]),
    {ok, Peer11, Cp1} = ?CT_PEER(#{name => Cp1, args => Args}),
    pong = rpc:call(Cp2, net_adm, ping, [Cp1]),
    pong = rpc:call(Cpx, net_adm, ping, [Cp1]),
    wait_for_ready_net(),

    ok = assert_loop(Cp2, Cp2, test2, Pid2, loop),
    ok = assert_loop(Cpx, Cpx, test, PidX, loop),

    %% Restart node Cpz
    Pid2 =
	rpc:call(Cp2, global_group, send, [{node, Cp2}, test2, {wait_nodeup, Cpz}]),
    PidX =
	rpc:call(Cpx, global_group, send, [{node, Cpx}, test, {wait_nodeup, Cpz}]),
    {ok, Peer1z, Cpz} = ?CT_PEER(#{name => Cpz, args => Args}),
    pong = rpc:call(Cp2, net_adm, ping, [Cpz]),
    pong = rpc:call(Cpx, net_adm, ping, [Cpz]),
    wait_for_ready_net(),

    ok = assert_loop(Cp2, Cp2, test2, Pid2, loop),
    ok = assert_loop(Cpx, Cpx, test, PidX, loop),

    %% stop the nodes, and make sure names are released.
    peer:stop(Peer11),
    peer:stop(Peer2),
    peer:stop(Peer3),
    peer:stop(Peerx),
    peer:stop(Peery),
    peer:stop(Peer1z),

    ?UNTIL(undefined =:= global:whereis_name(test)),
    ok.




%% Test a system with only one global group. .
one_grp(Config) when is_list(Config) ->
    Dir = proplists:get_value(priv_dir, Config),
    File = filename:join(Dir, "global_group.config"),
    {ok, Fd} = file:open(File, [write]),
    [Ncp1,Ncp2,Ncp3] = [?CT_PEER_NAME() || _ <- [cp1, cp2, cp3]],
    config(Fd, Ncp1, Ncp2, Ncp3, "cpx", "cpy", "cpz", "cpq"),

    Args = ["-config", filename:join(Dir, "global_group")],
    {ok, Peer1, Cp1} = ?CT_PEER(#{name => Ncp1, args => Args}),
    {ok, Peer2, Cp2} = ?CT_PEER(#{name => Ncp2, args => Args}),
    {ok, Peer3, Cp3} = ?CT_PEER(#{name => Ncp3, args => Args}),

    %% sleep a while to make the global_group to sync...
    ct:sleep(1000),

    %% start a proc and register it
    {Pid, yes} = rpc:call(Cp1, ?MODULE, start_proc, [test]),

    %% test that it is registered at all nodes
    Pid = rpc:call(Cp1, global, whereis_name, [test]),
    Pid = rpc:call(Cp2, global, whereis_name, [test]),
    Pid = rpc:call(Cp3, global, whereis_name, [test]),
    
    %% try to register the same name
    no = rpc:call(Cp1, global, register_name, [test, self()]),
    
    %% let process exit, check that it is unregistered automatically
    Pid ! die,
        ?UNTIL(begin
               (undefined =:= rpc:call(Cp1, global, whereis_name, [test])) and
               (undefined =:= rpc:call(Cp2, global, whereis_name, [test])) and
               (undefined =:= rpc:call(Cp3, global, whereis_name, [test]))
           end),
    
    %% test re_register
    {Pid2, yes} = rpc:call(Cp1, ?MODULE, start_proc, [test]),
    Pid2 = rpc:call(Cp3, global, whereis_name, [test]),
    Pid3 = rpc:call(Cp3, ?MODULE, start_proc_rereg, [test]),
    Pid3 = rpc:call(Cp3, global, whereis_name, [test]),

    %% test sending
    rpc:call(Cp1, global, send, [test, {ping, self()}]),
    receive
	{pong, Cp3} -> ok
    after
	2000 -> ct:fail(timeout1)
    end,

    rpc:call(Cp3, global, send, [test, {ping, self()}]),
    receive
	{pong, Cp3} -> ok
    after
	2000 -> ct:fail(timeout2)
    end,

    rpc:call(Cp3, global, unregister_name, [test]),
    undefined = rpc:call(Cp1, global, whereis_name, [test]),
    undefined = rpc:call(Cp2, global, whereis_name, [test]),
    undefined = rpc:call(Cp3, global, whereis_name, [test]),

    Pid3 ! die,
    ?UNTIL(undefined =:= rpc:call(Cp3, global, whereis_name, [test])),

    %% register a proc
    {_, yes} = rpc:call(Cp3, ?MODULE, start_proc, [test]),

    %% stop the nodes, and make sure names are released.
    peer:stop(Peer3),

    ?UNTIL(undefined =:= rpc:call(Cp1, global, whereis_name, [test])),
    Pid2 ! die,

    peer:stop(Peer1),
    peer:stop(Peer2),

    ok.





%% Check a system with only one global group.
%% Start the nodes with different time intervals.
one_grp_x(Config) when is_list(Config) ->
    Dir = proplists:get_value(priv_dir, Config),
    File = filename:join(Dir, "global_group.config"),
    {ok, Fd} = file:open(File, [write]),
    [Ncp1,Ncp2,Ncp3] = [?CT_PEER_NAME() || _ <- [cp1, cp2, cp3]],
    config(Fd, Ncp1, Ncp2, Ncp3, "cpx", "cpy", "cpz", "cpq"),

    Args = ["-config", filename:join(Dir, "global_group")],
    {ok, Peer1, Cp1} = ?CT_PEER(#{name => Ncp1, args => Args}),
    %% sleep a while to make the global_group to sync...
    ct:sleep(1000),

    %% start a proc and register it
    {Pid, yes} = rpc:call(Cp1, ?MODULE, start_proc, [test]),

    {ok, Peer2, Cp2} = ?CT_PEER(#{name => Ncp2, args => Args}),
    %% sleep a while to make the global_group to sync...
    ct:sleep(1000),

    %% test that it is registered at all nodes
    Pid = rpc:call(Cp1, global, whereis_name, [test]),
    Pid = rpc:call(Cp2, global, whereis_name, [test]),

    {ok, Peer3, Cp3} = ?CT_PEER(#{name => Ncp3, args => Args}),
    %% sleep a while to make the global_group to sync...
    ct:sleep(1000),

    Pid = rpc:call(Cp3, global, whereis_name, [test]),
    
    %% try to register the same name
    no = rpc:call(Cp1, global, register_name, [test, self()]),
    
    %% let process exit, check that it is unregistered automatically
    Pid ! die,
        ?UNTIL(begin
               (undefined =:= rpc:call(Cp1, global, whereis_name, [test])) and
               (undefined =:= rpc:call(Cp2, global, whereis_name, [test])) and
               (undefined =:= rpc:call(Cp3, global, whereis_name, [test]))
           end),
    
    %% test re_register
    {Pid2, yes} = rpc:call(Cp1, ?MODULE, start_proc, [test]),
    Pid2 = rpc:call(Cp3, global, whereis_name, [test]),

    Pid2 ! die,

    peer:stop(Peer1),
    peer:stop(Peer2),
    peer:stop(Peer3),

    ok.






%% Test a two global group system. .
two_grp(Config) when is_list(Config) ->
    Dir = proplists:get_value(priv_dir, Config),
    File = filename:join(Dir, "global_group.config"),
    {ok, Fd} = file:open(File, [write]),

    [Ncp1,Ncp2,Ncp3,Ncpx,Ncpy,Ncpz,Ncpq] = [?CT_PEER_NAME() ||
        _ <- [cp1,cp2,cp3,cpx,cpy,cpz,cpq]],
    config(Fd, Ncp1, Ncp2, Ncp3, Ncpx, Ncpy, Ncpz, Ncpq),

    Args = ["-config", filename:join(Dir, "global_group")],
    {ok, Peer1, Cp1} = ?CT_PEER(#{name => Ncp1, args => Args}),
    {ok, Peer2, Cp2} = ?CT_PEER(#{name => Ncp2, args => Args}),
    {ok, Peer3, Cp3} = ?CT_PEER(#{name => Ncp3, args => Args}),
    {ok, Peerx, Cpx} = ?CT_PEER(#{name => Ncpx, args => Args}),
    {ok, Peery, Cpy} = ?CT_PEER(#{name => Ncpy, args => Args}),
    {ok, Peerz, Cpz} = ?CT_PEER(#{name => Ncpz, args => Args}),

    %% The groups (cpq not started):
    %% [{nc1, [cp1,cp2,cp3]}, {nc2, [cpx,cpy,cpz]}, {nc3, [cpq]}]

    %% sleep a while to make the global_groups to sync...
    ct:sleep(1000),

    %% check the global group names
    {nc1, [nc2, nc3]} = rpc:call(Cp1, global_group, global_groups, []),
    {nc1, [nc2, nc3]} = rpc:call(Cp2, global_group, global_groups, []),
    {nc1, [nc2, nc3]} = rpc:call(Cp3, global_group, global_groups, []),
    {nc2, [nc1, nc3]} = rpc:call(Cpx, global_group, global_groups, []),
    {nc2, [nc1, nc3]} = rpc:call(Cpy, global_group, global_groups, []),
    {nc2, [nc1, nc3]} = rpc:call(Cpz, global_group, global_groups, []),

    %% check the global group nodes
    Nodes1 = lists:sort([Cp1, Cp2, Cp3]),
    Nodes2 = lists:sort([Cpx, Cpy, Cpz]),
    Nodes1 = rpc:call(Cp1, global_group, own_nodes, []),
    Nodes1 = rpc:call(Cp2, global_group, own_nodes, []),
    Nodes1 = rpc:call(Cp3, global_group, own_nodes, []),
    Nodes2 = rpc:call(Cpx, global_group, own_nodes, []),
    Nodes2 = rpc:call(Cpy, global_group, own_nodes, []),
    Nodes2 = rpc:call(Cpz, global_group, own_nodes, []),


    %% start a proc and register it
    {Pid1, yes} = rpc:call(Cp1, ?MODULE, start_proc, [test]),

    Pid1 = rpc:call(Cp1, global_group, send, [test, {io, from_cp1}]),
    Pid1 = rpc:call(Cpx, global_group, send, [test, {io, from_cpx}]),
    Pid1 = rpc:call(Cp1, global_group, send, [{group,nc1}, test,
					      {io, from_cp1}]),
    [test] =
        rpc:call(Cpx, global_group, registered_names, [{node, Cp1}]),
    [test] =
        rpc:call(Cpx, global_group, registered_names, [{group, nc1}]),
    [] = rpc:call(Cpx, global_group, registered_names, [{node, Cpx}]),
    [] = rpc:call(Cpx, global_group, registered_names, [{group, nc2}]),
    Pid1 = rpc:call(Cpx, global_group, send, [{group,nc1}, test,
					      {io, from_cp1}]),
    {badarg,{test,{io,from_cpx}}} =
	rpc:call(Cp1, global_group, send, [{group,nc2}, test, {io, from_cpx}]),
    {badarg,{test,{io,from_cpx}}} =
	rpc:call(Cpx, global_group, send, [{group,nc2}, test, {io, from_cpx}]),



    %% test that it is registered at all nodes
    Pid1 = rpc:call(Cp1, global, whereis_name, [test]),
    Pid1 = rpc:call(Cp2, global, whereis_name, [test]),
    Pid1 = rpc:call(Cp3, global, whereis_name, [test]),
    undefined = rpc:call(Cpx, global, whereis_name, [test]),
    undefined = rpc:call(Cpy, global, whereis_name, [test]),
    undefined = rpc:call(Cpz, global, whereis_name, [test]),

    %% start a proc and register it
    {PidX, yes} = rpc:call(Cpx, ?MODULE, start_proc, [test]),

    %% test that it is registered at all nodes
    Pid1 = rpc:call(Cp1, global, whereis_name, [test]),
    Pid1 = rpc:call(Cp2, global, whereis_name, [test]),
    Pid1 = rpc:call(Cp3, global, whereis_name, [test]),
    PidX = rpc:call(Cpx, global, whereis_name, [test]),
    PidX = rpc:call(Cpy, global, whereis_name, [test]),
    PidX = rpc:call(Cpz, global, whereis_name, [test]),

    Pid1 ! die,
    %% If we don't wait for global on other nodes to have updated its
    %% tables, 'test' may still be defined at the point when it is
    %% tested a few lines below.
    ?UNTIL(begin
               Pid = rpc:call(Cp2, global, whereis_name, [test]),
               undefined =:= Pid
           end),

    %% start a proc and register it
    {Pid2, yes} = rpc:call(Cp2, ?MODULE, start_proc, [test2]),

    %% test that it is registered at all nodes
    Pid2 = rpc:call(Cp1, global, whereis_name, [test2]),
    Pid2 = rpc:call(Cp2, global, whereis_name, [test2]),
    Pid2 = rpc:call(Cp3, global, whereis_name, [test2]),
    PidX = rpc:call(Cpx, global, whereis_name, [test]),
    PidX = rpc:call(Cpy, global, whereis_name, [test]),
    PidX = rpc:call(Cpz, global, whereis_name, [test]),

    undefined = rpc:call(Cp1, global, whereis_name, [test]),
    undefined = rpc:call(Cp2, global, whereis_name, [test]),
    undefined = rpc:call(Cp3, global, whereis_name, [test]),
    undefined = rpc:call(Cpx, global, whereis_name, [test2]),
    undefined = rpc:call(Cpy, global, whereis_name, [test2]),
    undefined = rpc:call(Cpz, global, whereis_name, [test2]),


    Pid2 = rpc:call(Cp1, global_group, send, [test2, {ping, self()}]),
    receive
	{pong, Cp2} -> ok
    after
	2000 -> ct:fail(timeout2)
    end,
    Pid2 = rpc:call(Cp2, global_group, send, [test2, {ping, self()}]),
    receive
	{pong, Cp2} -> ok
    after
	2000 -> ct:fail(timeout2)
    end,
    Pid2 = rpc:call(Cp3, global_group, send, [test2, {ping, self()}]),
    receive
	{pong, Cp2} -> ok
    after
	2000 -> ct:fail(timeout2)
    end,

    PidX = rpc:call(Cpx, global_group, send, [test, {ping, self()}]),
    receive
	{pong, Cpx} -> ok
    after
	2000 -> ct:fail(timeout2)
    end,
    PidX = rpc:call(Cpy, global_group, send, [test, {ping, self()}]),
    receive
	{pong, Cpx} -> ok
    after
	2000 -> ct:fail(timeout2)
    end,
    PidX = rpc:call(Cpz, global_group, send, [test, {ping, self()}]),
    receive
	{pong, Cpx} -> ok
    after
	2000 -> ct:fail(timeout2)
    end,

    Pid2 = rpc:call(Cpx, global_group, send, [{node, Cp1}, test2,
					      {ping, self()}]),
    receive
	{pong, Cp2} -> ok
    after
	2000 -> ct:fail(timeout2)
    end,
    Pid2 = rpc:call(Cpy, global_group, send, [{node, Cp2}, test2,
					      {ping, self()}]),
    receive
	{pong, Cp2} -> ok
    after
	2000 -> ct:fail(timeout2)
    end,
    Pid2 = rpc:call(Cpz, global_group, send, [{node, Cp3}, test2,
					      {ping, self()}]),
    receive
	{pong, Cp2} -> ok
    after
	2000 -> ct:fail(timeout2)
    end,

    PidX = rpc:call(Cpx, global_group, send, [{node, Cpz}, test,
					      {ping, self()}]),
    receive
	{pong, Cpx} -> ok
    after
	2000 -> ct:fail(timeout2)
    end,
    PidX = rpc:call(Cpy, global_group, send, [{node, Cpx}, test,
					      {ping, self()}]),
    receive
	{pong, Cpx} -> ok
    after
	2000 -> ct:fail(timeout2)
    end,
    PidX = rpc:call(Cpz, global_group, send, [{node, Cpy}, test,
					      {ping, self()}]),
    receive
	{pong, Cpx} -> ok
    after
	2000 -> ct:fail(timeout2)
    end,

    Pid2 = rpc:call(Cpx, global_group, send, [{group, nc1}, test2,
					      {ping, self()}]),
    receive
	{pong, Cp2} -> ok
    after
	2000 -> ct:fail(timeout2)
    end,
    PidX = rpc:call(Cpy, global_group, send, [{group, nc2}, test,
					      {ping, self()}]),
    receive
	{pong, Cpx} -> ok
    after
	2000 -> ct:fail(timeout2)
    end,

    %%------------------------------------
    %% Test monitor nodes
    %%------------------------------------
    Pid2 =
        rpc:call(Cp1, global_group, send, [{node, Cp2}, test2, monitor]),
    PidX =
        rpc:call(Cpx, global_group, send, [{node, Cpx}, test, monitor]),


    %% Kill node Cp1
    Pid2 = rpc:call(Cp2, global_group, send, [{node, Cp2}, test2,
					      {wait_nodedown, Cp1}]),
    PidX = rpc:call(Cpx, global_group, send, [{node, Cpx}, test,
					      {wait_nodedown, Cp1}]),
    ct:sleep(100),
    peer:stop(Peer1),
    ct:sleep(1000),

    ok = assert_loop(Cp2, Cp2, test2, Pid2, loop),
    ok = assert_loop(Cpx, Cpx, test, PidX, loop_nodedown),
    PidX =
	rpc:call(Cpx, global_group, send, [{node, Cpx}, test, to_loop]),

    %% Kill node Cpz
    Pid2 = rpc:call(Cp2, global_group, send, [{node, Cp2}, test2,
					      {wait_nodedown, Cpz}]),
    PidX = rpc:call(Cpx, global_group, send, [{node, Cpx}, test,
					      {wait_nodedown, Cpz}]),
    ct:sleep(100),
    peer:stop(Peerz),
    ct:sleep(1000),

    ok = assert_loop(Cp2, Cp2, test2, Pid2, loop_nodedown),
    ok = assert_loop(Cpx, Cpx, test, PidX, loop),
    Pid2 =
	rpc:call(Cp2, global_group, send, [{node, Cp2}, test2, to_loop]),

    %% Restart node Cp1
    Nodes1 = rpc:call(Cp2, global_group, own_nodes, []),
    Pid2 = rpc:call(Cp2, global_group, send, [{node, Cp2}, test2,
					      {wait_nodeup, Cp1}]),
    PidX = rpc:call(Cpx, global_group, send, [{node, Cpx}, test,
					      {wait_nodeup, Cp1}]),
    ct:sleep(100),
    {ok, Peer11, Cp1} = ?CT_PEER(#{name => Cp1, args => Args}),
    ct:sleep(5000),

    ok = assert_loop(Cp2, Cp2, test2, Pid2, loop),
    ok = assert_loop(Cpx, Cpx, test, PidX, loop_nodeup),
    PidX =
	rpc:call(Cpx, global_group, send, [{node, Cpx}, test, to_loop]),


    %% Restart node Cpz
    Pid2 = rpc:call(Cp2, global_group, send, [{node, Cp2}, test2,
					      {wait_nodeup, Cpz}]),
    PidX = rpc:call(Cpx, global_group, send, [{node, Cpx}, test,
					      {wait_nodeup, Cpz}]),
    ct:sleep(100),
    {ok, Peer1z, Cpz} = ?CT_PEER(#{name => Cpz, args => Args}),
    ct:sleep(5000),

    ok = assert_loop(Cp2, Cp2, test2, Pid2, loop_nodeup),
    ok = assert_loop(Cpx, Cpx, test, PidX, loop),
    Pid2 =
	rpc:call(Cp2, global_group, send, [{node, Cp2}, test2, to_loop]),


    Pid2 ! die,
    PidX ! die,

    peer:stop(Peer11),
    peer:stop(Peer2),
    peer:stop(Peer3),
    peer:stop(Peerx),
    peer:stop(Peery),
    peer:stop(Peer1z),

    ok.



%% Test hidden global groups.
hidden_groups(Config) when is_list(Config) ->
    Dir = proplists:get_value(priv_dir, Config),
    File = filename:join(Dir, "hidden_groups.config"),
    {ok, Fd} = file:open(File, [write]),

    [Ncp1,Ncp2,Ncp3,Ncpx,Ncpy,Ncpz,Ncpq] = [?CT_PEER_NAME() ||
        _ <- [cp1,cp2,cp3,cpx,cpy,cpz,cpq]],
    config_hidden(Fd, Ncp1, Ncp2, Ncp3, Ncpx, Ncpy, Ncpz, Ncpq),

    Args = ["-config", filename:join(Dir, "hidden_groups")],
    {ok, Peer1, Cp1} = ?CT_PEER(#{name => Ncp1, args => Args}),
    {ok, Peer2, Cp2} = ?CT_PEER(#{name => Ncp2, args => Args}),
    {ok, Peer3, Cp3} = ?CT_PEER(#{name => Ncp3, args => Args}),
    {ok, Peerx, Cpx} = ?CT_PEER(#{name => Ncpx, args => Args}),
    {ok, Peery, Cpy} = ?CT_PEER(#{name => Ncpy, args => Args}),
    {ok, Peerz, Cpz} = ?CT_PEER(#{name => Ncpz, args => Args}),
    {ok, Peerq, Cpq} = ?CT_PEER(#{name => Ncpq, args => Args}),

    %% sleep a while to make the global_groups to sync...
    ct:sleep(1000),

    %% check the global group names
    {nc1, [nc2, nc3]} = rpc:call(Cp1, global_group, global_groups, []),
    {nc1, [nc2, nc3]} = rpc:call(Cp2, global_group, global_groups, []),
    {nc1, [nc2, nc3]} = rpc:call(Cp3, global_group, global_groups, []),
    {nc2, [nc1, nc3]} = rpc:call(Cpx, global_group, global_groups, []),
    {nc2, [nc1, nc3]} = rpc:call(Cpy, global_group, global_groups, []),
    {nc2, [nc1, nc3]} = rpc:call(Cpz, global_group, global_groups, []),

    %% check the global group nodes
    Nodes1 = lists:sort([Cp1, Cp2, Cp3]),
    Nodes2 = lists:sort([Cpx, Cpy, Cpz]),
    Nodes1 = rpc:call(Cp1, global_group, own_nodes, []),
    Nodes1 = rpc:call(Cp2, global_group, own_nodes, []),
    Nodes1 = rpc:call(Cp3, global_group, own_nodes, []),
    Nodes2 = rpc:call(Cpx, global_group, own_nodes, []),
    Nodes2 = rpc:call(Cpy, global_group, own_nodes, []),
    Nodes2 = rpc:call(Cpz, global_group, own_nodes, []),
    [Cpq]           = rpc:call(Cpq, global_group, own_nodes, []),

    %% Make some inter group connections
    pong = rpc:call(Cp1, net_adm, ping, [Cpx]),
    pong = rpc:call(Cpy, net_adm, ping, [Cp2]),
    pong = rpc:call(Cp3, net_adm, ping, [Cpx]),
    pong = rpc:call(Cpz, net_adm, ping, [Cp3]),
    pong = rpc:call(Cpq, net_adm, ping, [Cp1]),
    pong = rpc:call(Cpz, net_adm, ping, [Cpq]),

    %% Check that no inter group connections are visible
    Nodes1 = lists:sort([Cp1|rpc:call(Cp1, erlang, nodes, [])]),
    Nodes1 = lists:sort([Cp2|rpc:call(Cp2, erlang, nodes, [])]),
    Nodes1 = lists:sort([Cp3|rpc:call(Cp3, erlang, nodes, [])]),
    Nodes2 = lists:sort([Cpx|rpc:call(Cpx, erlang, nodes, [])]),
    Nodes2 = lists:sort([Cpy|rpc:call(Cpy, erlang, nodes, [])]),
    Nodes2 = lists:sort([Cpz|rpc:call(Cpz, erlang, nodes, [])]),
    NC12Nodes = lists:append(Nodes1, Nodes2),
    false = lists:any(fun(N) -> lists:member(N, NC12Nodes) end,
		      rpc:call(Cpq, erlang, nodes, [])),


    peer:stop(Peer1),
    peer:stop(Peer2),
    peer:stop(Peer3),
    peer:stop(Peerx),
    peer:stop(Peery),
    peer:stop(Peerz),
    peer:stop(Peerq),

    ok.


%% Checks when the search process exits. .
test_exit(Config) when is_list(Config) ->
    Dir=proplists:get_value(priv_dir, Config),
    Args = ["-config", filename:join(Dir, "global_group")],

    {ok, Peer1, Cp1} = ?CT_PEER(Args),
    {ok, Peer2, _Cp2} = ?CT_PEER(Args),
    {ok, Peer3, _Cp3} = ?CT_PEER(Args),

    ct:sleep(1000),

    {error, illegal_function_call} =
        rpc:call(Cp1, global_group, registered_names_test, [{node, Cp1}]),
    {badarg,_} =
        rpc:call(Cp1, global_group, send, [king, "The message"]),
    undefined = rpc:call(Cp1, global_group, whereis_name, [king]),

    % make sure the search process really exits after every global_group operations
    ProcessCount0 = rpc:call(Cp1, erlang, system_info, [process_count]),
    _ = rpc:call(Cp1, global_group, whereis_name, [{node, Cp1}, whatever_pid_name]),
    ProcessCount1 = rpc:call(Cp1, erlang, system_info, [process_count]),
    _ = rpc:call(Cp1, global_group, registered_names, [{node, Cp1}]),
    ProcessCount2 = rpc:call(Cp1, erlang, system_info, [process_count]),
    _ = rpc:call(Cp1, global_group, send, [{node, Cp1}, whatever_pid_name, msg]),
    ProcessCount3 = rpc:call(Cp1, erlang, system_info, [process_count]),
    ProcessCount0 = ProcessCount1 = ProcessCount2 = ProcessCount3,

    %% stop the nodes, and make sure names are released.
    peer:stop(Peer1),
    peer:stop(Peer2),
    peer:stop(Peer3),

    %% sleep to let the nodes die
    ct:sleep(1000),

    ok.

global_disconnect(Config) when is_list(Config) ->
    Dir = proplists:get_value(priv_dir, Config),

    [NProxy,Ncp1,Ncp2,Ncp3,Ncpx,Ncpy,Ncpz,Nh]
        = [?CT_PEER_NAME(atom_to_list(?FUNCTION_NAME)++"-"++atom_to_list(NN))
           || NN <- [proxy,cp1,cp2,cp3,cpx,cpy,cpz,h]],

    Host = from($@, atom_to_list(node())),

    WriteConf = fun (File,
                     [S1, S2, S3],
                     [NC11, NC12, NC13],
                     [NC21, NC22, NC23]) ->
                        {ok, Fd} = file:open(filename:join(Dir, File), [write]),
                        io:format(Fd,
                                  "[{kernel,~n"
                                  "  [~n"
                                  "   {sync_nodes_optional, ['~s@~s','~s@~s','~s@~s']},~n"
                                  "   {sync_nodes_timeout, 1000},~n"
                                  "   {global_groups,~n"
                                  "    [{nc1, ['~s@~s','~s@~s','~s@~s']},~n"
                                  "     {nc2, ['~s@~s','~s@~s','~s@~s']}]}~n"
                                  "  ]~n"
                                  " }].~n",
                                  [S1, Host, S2, Host, S3, Host,
                                   NC11, Host, NC12, Host, NC13, Host,
                                   NC21, Host, NC22, Host, NC23, Host]),
                        file:close(Fd)
                end,

    WriteConf("nc1.config", [Ncp1,Ncp2,Ncp3], [Ncp1,Ncp2,Ncp3], [Ncpx,Ncpy,Ncpz]),
    WriteConf("nc2.config", [Ncpx,Ncpy,Ncpz], [Ncp1,Ncp2,Ncp3], [Ncpx,Ncpy,Ncpz]),

    NC1Args = ["-config", filename:join(Dir, "nc1")],
    NC2Args = ["-config", filename:join(Dir, "nc2")],

    PeerNodes = lists:map(fun ({Name, Args}) ->
                                  ?CT_PEER(#{name => Name,
                                             args => Args,
                                             connection => 0})
                          end,
                          [{Ncp1, NC1Args},
                           {Ncp2, NC1Args},
                           {Ncp3, NC1Args},
                           {Ncpx, NC2Args},
                           {Ncpy, NC2Args},
                           {Ncpz, NC2Args},
                           {Nh, ["-hidden"]},
                           {NProxy, ["-hidden"]}]),

    [{ok, _, Cp1}, {ok, _, Cp2}, {ok, _, Cp3},
     {ok, _, Cpx}, {ok, _, Cpy}, {ok, _, Cpz},
     {ok, _, H}, {ok, _, P}] = PeerNodes,

    %% RPC() - An rpc via a hidden proxy node...
    RPC = fun (N, M, F, A) ->
                  erpc:call(P, erpc, call, [N, M, F, A])
          end,

    %% The groups
    NC1Nodes = lists:sort([Cp1, Cp2, Cp3]),
    NC2Nodes = lists:sort([Cpx, Cpy, Cpz]),

    %% Wait with disconnect from group nodes a while, so we
    %% don't have any ongoing communication that brings up
    %% the connections again...
    receive after 500 -> ok end,

    %% Disconnect test_server from the global group nodes...
    lists:foreach(fun (N) -> erlang:disconnect_node(N) end, NC1Nodes++NC2Nodes),

    %% wait some more to ensure that global group nodes have synced...
    receive after 500 -> ok end,

    %% check the global group names
    {nc1, [nc2]} = RPC(Cp1, global_group, global_groups, []),
    {nc1, [nc2]} = RPC(Cp2, global_group, global_groups, []),
    {nc1, [nc2]} = RPC(Cp3, global_group, global_groups, []),
    {nc2, [nc1]} = RPC(Cpx, global_group, global_groups, []),
    {nc2, [nc1]} = RPC(Cpy, global_group, global_groups, []),
    {nc2, [nc1]} = RPC(Cpz, global_group, global_groups, []),

    %% check the global group nodes
    NC1Nodes = lists:sort(RPC(Cp1, global_group, own_nodes, [])),
    NC1Nodes = lists:sort(RPC(Cp2, global_group, own_nodes, [])),
    NC1Nodes = lists:sort(RPC(Cp3, global_group, own_nodes, [])),
    NC2Nodes = lists:sort(RPC(Cpx, global_group, own_nodes, [])),
    NC2Nodes = lists:sort(RPC(Cpy, global_group, own_nodes, [])),
    NC2Nodes = lists:sort(RPC(Cpz, global_group, own_nodes, [])),

    %% Set up connections that should *not* be affected
    %% by our global:disconnect() call made later (this since
    %% these connections are not internal in the group). One
    %% hidden and one visible connection. We don't use the
    %% proxy node for the hidden connection since it will be
    %% brought up by our erpc calls if it should have been
    %% taken down.
    pong = RPC(H, net_adm, ping, [Cp1]),
    pong = RPC(Cpx, net_adm, ping, [Cp1]),

    %% Verify that the connections have been set up as
    %% expected...
    HiddenNodes = lists:sort([P, H]),

    Cp1ConnectedNodes = lists:sort([P, H, Cpx, Cp2, Cp3]),
    Cp1VisibleNodes = Cp1ConnectedNodes -- HiddenNodes,
    Cp2ConnectedNodes = lists:sort([P, Cp1, Cp3]),
    Cp2VisibleNodes = Cp2ConnectedNodes -- HiddenNodes,
    Cp3ConnectedNodes = lists:sort([P, Cp1, Cp2]),
    Cp3VisibleNodes = Cp3ConnectedNodes -- HiddenNodes,

    CpxConnectedNodes = lists:sort([P, Cp1, Cpy, Cpz]),
    CpxVisibleNodes = CpxConnectedNodes -- HiddenNodes,
    CpyConnectedNodes = lists:sort([P, Cpx, Cpz]),
    CpyVisibleNodes = CpyConnectedNodes -- HiddenNodes,
    CpzConnectedNodes = lists:sort([P, Cpx, Cpy]),
    CpzVisibleNodes = CpzConnectedNodes -- HiddenNodes,

    Cp1ConnectedNodes = lists:sort(RPC(Cp1, erlang, nodes, [connected])),
    Cp1VisibleNodes = lists:sort(RPC(Cp1, erlang, nodes, [])),
    Cp2ConnectedNodes = lists:sort(RPC(Cp2, erlang, nodes, [connected])),
    Cp2VisibleNodes = lists:sort(RPC(Cp2, erlang, nodes, [])),
    Cp3ConnectedNodes = lists:sort(RPC(Cp3, erlang, nodes, [connected])),
    Cp3VisibleNodes = lists:sort(RPC(Cp3, erlang, nodes, [])),

    CpxConnectedNodes = lists:sort(RPC(Cpx, erlang, nodes, [connected])),
    CpxVisibleNodes = lists:sort(RPC(Cpx, erlang, nodes, [])),
    CpyConnectedNodes = lists:sort(RPC(Cpy, erlang, nodes, [connected])),
    CpyVisibleNodes = lists:sort(RPC(Cpy, erlang, nodes, [])),
    CpzConnectedNodes = lists:sort(RPC(Cpz, erlang, nodes, [connected])),
    CpzVisibleNodes = lists:sort(RPC(Cpz, erlang, nodes, [])),

    %% Expected disconnects made by global on Cp1...
    Cp1DisconnectNodes = lists:sort([Cp2, Cp3]),

    %% Perform the global:disconnect() on Cp1...
    Cp1DisconnectNodes = lists:sort(RPC(Cp1, global, disconnect, [])),

    %% Wait a while giving the other nodes time to react to the disconnects
    %% before we check that everything is as expected...
    receive after 2000 -> ok end,

    %% Verify that only the connections Cp1-Cp2 and Cp1-Cp3 were
    %% taken down...
    Cp1PostConnectedNodes = Cp1ConnectedNodes -- Cp1DisconnectNodes,
    Cp1PostVisibleNodes = Cp1PostConnectedNodes -- HiddenNodes,
    Cp2PostConnectedNodes = Cp2ConnectedNodes -- [Cp1],
    Cp2PostVisibleNodes = Cp2PostConnectedNodes -- HiddenNodes,
    Cp3PostConnectedNodes = Cp3ConnectedNodes -- [Cp1],
    Cp3PostVisibleNodes = Cp3PostConnectedNodes -- HiddenNodes,

    Cp1PostConnectedNodes = lists:sort(RPC(Cp1, erlang, nodes, [connected])),
    Cp1PostVisibleNodes = lists:sort(RPC(Cp1, erlang, nodes, [])),
    Cp2PostConnectedNodes = lists:sort(RPC(Cp2, erlang, nodes, [connected])),
    Cp2PostVisibleNodes = lists:sort(RPC(Cp2, erlang, nodes, [])),
    Cp3PostConnectedNodes = lists:sort(RPC(Cp3, erlang, nodes, [connected])),
    Cp3PostVisibleNodes = lists:sort(RPC(Cp3, erlang, nodes, [])),

    CpxConnectedNodes = lists:sort(RPC(Cpx, erlang, nodes, [connected])),
    CpxVisibleNodes = lists:sort(RPC(Cpx, erlang, nodes, [])),
    CpyConnectedNodes = lists:sort(RPC(Cpy, erlang, nodes, [connected])),
    CpyVisibleNodes = lists:sort(RPC(Cpy, erlang, nodes, [])),
    CpzConnectedNodes = lists:sort(RPC(Cpz, erlang, nodes, [connected])),
    CpzVisibleNodes = lists:sort(RPC(Cpz, erlang, nodes, [])),

    lists:foreach(fun ({ok, Peer, _Node}) ->
                          peer:stop(Peer)
                  end, PeerNodes),

    ok.

%%%%% End of test-cases %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
            ct:fail(Other1)
    after 5000 ->
            ct:fail(timeout)
    end.

loop_until_true(Fun) ->
    case Fun() of
	true ->
	    ok;
	_ ->
	    loop_until_true(Fun)
    end.
