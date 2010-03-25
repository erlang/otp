%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2010. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File: ct_master_SUITE
%%%
%%% Description:
%%% Test ct_master.
%%%
%%% The suites used for the test are located in the data directory.
%%%-------------------------------------------------------------------
-module(ct_master_SUITE).
-compile(export_all).

-include_lib("test_server/include/test_server.hrl").
-include_lib("common_test/include/ct_event.hrl").

-define(eh, ct_test_support_eh).

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Description: Since Common Test starts another Test Server
%% instance, the tests need to be performed on a separate node (or
%% there will be clashes with logging processes etc).
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config1 = ct_test_support:init_per_suite(Config),
    Config1.

end_per_suite(Config) ->
    ct_test_support:end_per_suite(Config).

init_per_testcase(TestCase, Config) ->
    ct_test_support:init_per_testcase(TestCase, [{master, true}|Config]).

end_per_testcase(TestCase, Config) ->
    ct_test_support:end_per_testcase(TestCase, Config).

all(doc) ->
    [""];

all(suite) ->
    [
	ct_master_test_peer,
	ct_master_test_slave
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------
ct_master_test_peer(Config) when is_list(Config)->
    NodeCount = 5,
    DataDir = ?config(data_dir, Config),
    NodeNames = [list_to_atom("testnode_"++integer_to_list(N)) ||
		 N <- lists:seq(1, NodeCount)],
    FileName = filename:join(DataDir, "ct_master_spec.spec"),
    Suites = [master_SUITE],
    make_spec(DataDir, FileName, NodeNames, Suites, Config),
    start_nodes(NodeNames, peer),
    run_test(ct_master_test, FileName, Config),
    stop_nodes(NodeNames, peer),
    file:delete(filename:join(DataDir, FileName)).

ct_master_test_slave(Config) when is_list(Config)->
    NodeCount = 5,
    DataDir = ?config(data_dir, Config),
    NodeNames = [list_to_atom("testnode_"++integer_to_list(N)) ||
		 N <- lists:seq(1, NodeCount)],
    FileName = filename:join(DataDir, "ct_master_spec.spec"),
    Suites = [master_SUITE],
    make_spec(DataDir, FileName, NodeNames, Suites, Config),
    start_nodes(NodeNames, slave),
    run_test(ct_master_test, FileName, Config),
    stop_nodes(NodeNames, slave),
    file:delete(filename:join(DataDir, FileName)).

%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------
make_spec(DataDir, FileName, NodeNames, Suites, Config)->
    {ok, HostName} = inet:gethostname(),

    N = lists:map(fun(NodeName)->
	    {node, NodeName, enodename(HostName, NodeName)}
	end,
	NodeNames),

    C = lists:map(fun(NodeName)->
	Rnd = random:uniform(2),
	if Rnd == 1->
	    {config, NodeName, "master/config.txt"};
	true->
	    {userconfig, NodeName, {ct_config_xml, "master/config.xml"}}
        end
	end,
	NodeNames),

    S = [{suites, NodeNames, filename:join(DataDir, "master"), Suites}],

    PrivDir = ?config(priv_dir, Config),
    LD = [{logdir, PrivDir}, {logdir, master, PrivDir}],

    ct_test_support:write_testspec(N++C++S++LD, DataDir, FileName).

run_test(_Name, FileName, Config)->
    [{FileName, ok}] = ct_test_support:run(ct_master, run, [FileName], Config).

wait_for_node_alive(_Node, 0)->
    pang;
wait_for_node_alive(Node, N)->
    timer:sleep(1000),
    case net_adm:ping(Node) of
	pong->
	    pong;
	pang->
	    wait_for_node_alive(Node, N-1)
    end.

wait_for_node_dead(_Node, 0)->
    error;
wait_for_node_dead(Node, N)->
    timer:sleep(1000),
    case lists:member(Node, nodes()) of
	true->
	    wait_for_node_dead(Node, N-1);
	false->
	    ok
    end.

enodename(HostName, NodeName)->
    list_to_atom(atom_to_list(NodeName)++"@"++HostName).

start_node(HostName, NodeName, peer)->
    ENodeName = enodename(HostName, NodeName),
    Cmd = "erl -detached -noinput -sname "++atom_to_list(NodeName),
    open_port({spawn, Cmd}, [stream]),
    pong = wait_for_node_alive(ENodeName, 3);
start_node(HostName, NodeName, slave)->
    ENodeName = enodename(HostName, NodeName),
    {ok, ENodeName} =
	slave:start(list_to_atom(HostName), NodeName).

stop_node(HostName, NodeName, peer)->
    ENodeName = enodename(HostName, NodeName),
    spawn(ENodeName, init, stop, []),
    wait_for_node_dead(ENodeName, 3);
stop_node(HostName, NodeName, slave)->
    ENodeName = enodename(HostName, NodeName),
    ok = slave:stop(ENodeName).

start_nodes(NodeNames, Type)->
    {ok, HostName} = inet:gethostname(),
    lists:foreach(fun(NodeName)->
		      start_node(HostName, NodeName, Type)
		  end,
		  NodeNames).

stop_nodes(NodeNames, Type)->
    {ok, HostName} = inet:gethostname(),
    lists:foreach(fun(NodeName)->
		      stop_node(HostName, NodeName, Type)
		  end,
		  NodeNames).

reformat_events(Events, EH) ->
    ct_test_support:reformat(Events, EH).

%%%-----------------------------------------------------------------
%%% TEST EVENTS
%%%-----------------------------------------------------------------
expected_events(_)->
[].
