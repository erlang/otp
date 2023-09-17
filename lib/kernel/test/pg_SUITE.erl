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
%%-------------------------------------------------------------------
%% @author Maxim Fedorov <maximfca@gmail.com>
%%     Process Groups smoke test.
-module(pg_SUITE).
-author("maximfca@gmail.com").

%% Test server callbacks
-export([
    suite/0,
    all/0,
    groups/0,
    init_per_testcase/2,
    end_per_testcase/2,
    init_per_group/2,
    end_per_group/2,
    stop_proc/1,
    ensure_peers_info/2
]).

%% Test cases exports
-export([
    pg/0, pg/1,
    errors/0, errors/1,
    leave_exit_race/0, leave_exit_race/1,
    dyn_distribution/0, dyn_distribution/1,
    process_owner_check/0, process_owner_check/1,
    overlay_missing/0, overlay_missing/1,
    single/0, single/1,
    two/1,
    empty_group_by_remote_leave/0, empty_group_by_remote_leave/1,
    thundering_herd/0, thundering_herd/1,
    initial/1,
    netsplit/1,
    trisplit/1,
    foursplit/1,
    exchange/1,
    nolocal/1,
    double/1,
    scope_restart/1,
    missing_scope_join/1,
    disconnected_start/1,
    forced_sync/0, forced_sync/1,
    group_leave/1,
    monitor_nonempty_scope/0, monitor_nonempty_scope/1,
    monitor_scope/0, monitor_scope/1,
    monitor/1,
    monitor_self/1,
    multi_monitor/1,
    protocol_upgrade/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

suite() ->
    [{timetrap, {seconds, 60}}].

init_per_testcase(TestCase, Config) ->
    {ok, Pid} = pg:start_link(TestCase),
    trace_start(TestCase, Config, Pid).

end_per_testcase(TestCase, Config) ->
    gen_server:stop(TestCase),
    trace_end(Config),
    ok.

all() ->
    [dyn_distribution,
     {group, basic},
     {group, cluster},
     {group, performance},
     {group, monitor},
     {group, old_release}].

groups() ->
    [
        {basic, [parallel], [errors, pg, leave_exit_race, single, overlay_missing,
                             protocol_upgrade, monitor_self, multi_monitor]},
        {performance, [], [thundering_herd]},
        {cluster, [parallel], [process_owner_check, two, initial, netsplit, trisplit, foursplit,
            exchange, nolocal, double, scope_restart, missing_scope_join, empty_group_by_remote_leave,
            disconnected_start, forced_sync, group_leave]},
        {monitor, [parallel], [monitor_nonempty_scope, monitor_scope, monitor]},
        {old_release, [parallel], [process_owner_check, two, overlay_missing,
                                   empty_group_by_remote_leave, initial, netsplit,
                                   nolocal]}
    ].

init_per_group(old_release, Config) ->
    PrevRel = integer_to_list(list_to_integer(erlang:system_info(otp_release)) - 1),
    case test_server:is_release_available(PrevRel) of
        true ->
            [{otp_release, PrevRel} | Config];
        false ->
            {skip, "No OTP "++PrevRel++" release found"}
    end;
init_per_group(_, Config) ->
    Config.

end_per_group(_,_) -> ok.


%%--------------------------------------------------------------------
%% TEST CASES

pg() ->
    [{doc, "This test must be names pg, to stay inline with default scope"}].

pg(_Config) ->
    ?assertNotEqual(undefined, whereis(?FUNCTION_NAME)), %% ensure scope was started
    ?assertEqual(ok, pg:join(?FUNCTION_NAME, self())),
    ?assertEqual([self()], pg:get_local_members(?FUNCTION_NAME)),
    ?assertEqual([?FUNCTION_NAME], pg:which_groups()),
    ?assertEqual([?FUNCTION_NAME], pg:which_local_groups()),
    ?assertEqual(ok, pg:leave(?FUNCTION_NAME, self())),
    ?assertEqual([], pg:get_members(?FUNCTION_NAME)),
    ?assertEqual([], pg:which_groups(?FUNCTION_NAME)),
    ?assertEqual([], pg:which_local_groups(?FUNCTION_NAME)).

errors() ->
    [{doc, "Tests that errors are handled as expected, for example pg server crashes when it needs to"}].

errors(_Config) ->
    %% kill with 'info' and 'cast'
    ?assertException(error, badarg, pg:handle_info(garbage, garbage)),
    ?assertException(error, badarg, pg:handle_cast(garbage, garbage)),
    %% kill with call
    {ok, _Pid} = pg:start(second),
    ?assertException(exit, {{badarg, _}, _}, gen_server:call(second, garbage, infinity)).

leave_exit_race() ->
    [{doc, "Tests that pg correctly handles situation when leave and 'DOWN' messages are both in pg queue"}].

leave_exit_race(Config) when is_list(Config) ->
    process_flag(priority, high),
    [
        begin
            Pid = spawn(fun () -> ok end),
            pg:join(leave_exit_race, test, Pid),
            pg:leave(leave_exit_race, test, Pid)
        end
        || _ <- lists:seq(1, 100)].

single() ->
    [{doc, "Tests single node groups"}, {timetrap, {seconds, 5}}].

single(Config) when is_list(Config) ->
    ?assertEqual(ok, pg:join(?FUNCTION_NAME, ?FUNCTION_NAME, self())),
    ?assertEqual(ok, pg:join(?FUNCTION_NAME, ?FUNCTION_NAME, [self(), self()])),
    ?assertEqual([self(), self(), self()], pg:get_local_members(?FUNCTION_NAME, ?FUNCTION_NAME)),
    ?assertEqual([self(), self(), self()], pg:get_members(?FUNCTION_NAME, ?FUNCTION_NAME)),
    ?assertEqual(not_joined, pg:leave(?FUNCTION_NAME, '$missing$', self())),
    ?assertEqual(ok, pg:leave(?FUNCTION_NAME, ?FUNCTION_NAME, [self(), self()])),
    ?assertEqual(ok, pg:leave(?FUNCTION_NAME, ?FUNCTION_NAME, self())),
    ?assertEqual([], pg:which_groups(?FUNCTION_NAME)),
    ?assertEqual([], pg:get_local_members(?FUNCTION_NAME, ?FUNCTION_NAME)),
    ?assertEqual([], pg:get_members(?FUNCTION_NAME, ?FUNCTION_NAME)),
    %% double
    ?assertEqual(ok, pg:join(?FUNCTION_NAME, ?FUNCTION_NAME, self())),
    Pid = erlang:spawn(forever()),
    ?assertEqual(ok, pg:join(?FUNCTION_NAME, ?FUNCTION_NAME, Pid)),
    Expected = lists:sort([Pid, self()]),
    ?assertEqual(Expected, lists:sort(pg:get_members(?FUNCTION_NAME, ?FUNCTION_NAME))),
    ?assertEqual(Expected, lists:sort(pg:get_local_members(?FUNCTION_NAME, ?FUNCTION_NAME))),

    stop_proc(Pid),
    sync(?FUNCTION_NAME),
    ?assertEqual([self()], pg:get_local_members(?FUNCTION_NAME, ?FUNCTION_NAME)),
    ?assertEqual(ok, pg:leave(?FUNCTION_NAME, ?FUNCTION_NAME, self())),
    ok.

dyn_distribution() ->
    [{doc, "Tests that local node when distribution is started dynamically is not treated as remote node"}].

dyn_distribution(Config) when is_list(Config) ->
    %% When distribution is started or stopped dynamically,
    %%  there is a nodeup/nodedown message delivered to pg
    %% It is possible but non-trivial to simulate this
    %%  behaviour with starting slave nodes being not
    %%  distributed, and calling net_kernel:start/1, however
    %%  the effect is still the same as simply sending nodeup,
    %%  which is also documented.
    ?FUNCTION_NAME ! {nodeup, node()},
    %%
    ?assertEqual(ok, pg:join(?FUNCTION_NAME, ?FUNCTION_NAME, self())),
    ?assertEqual([self()], pg:get_members(?FUNCTION_NAME, ?FUNCTION_NAME)),
    ok.

process_owner_check() ->
    [{doc, "Tests that process owner is local node"}].

process_owner_check(Config) when is_list(Config) ->
    {Peer, Node} = spawn_node(?FUNCTION_NAME, Config),
    %% spawn remote process
    LocalPid = erlang:spawn(forever()),
    RemotePid = spawn_sleeper_at(Node),
    %% check they can't be joined locally
    ?assertException(error, {nolocal, _}, pg:join(?FUNCTION_NAME, ?FUNCTION_NAME, RemotePid)),
    ?assertException(error, {nolocal, _}, pg:join(?FUNCTION_NAME, ?FUNCTION_NAME, [RemotePid, RemotePid])),
    ?assertException(error, {nolocal, _}, pg:join(?FUNCTION_NAME, ?FUNCTION_NAME, [LocalPid, RemotePid])),
    %% check that non-pid also triggers error
    ?assertException(error, function_clause, pg:join(?FUNCTION_NAME, ?FUNCTION_NAME, undefined)),
    ?assertException(error, {nolocal, _}, pg:join(?FUNCTION_NAME, ?FUNCTION_NAME, [undefined])),
    %% stop the peer
    peer:stop(Peer),
    ok.

overlay_missing() ->
    [{doc, "Tests that scope process that is not a part of overlay network does not change state"}].

overlay_missing(Config) ->
    {Peer, Node} = spawn_node(?FUNCTION_NAME, Config),
    %% join self (sanity check)
    ?assertEqual(ok, pg:join(?FUNCTION_NAME, group, self())),
    %% remember pid from remote
    PgPid = rpc:call(Node, erlang, whereis, [?FUNCTION_NAME]),
    RemotePid = spawn_sleeper_at(Node),
    %% stop remote scope
    gen_server:stop(PgPid),
    %% craft white-box request: ensure it's rejected
    ?FUNCTION_NAME ! {join, PgPid, group, RemotePid},
    %% rejected!
    ?assertEqual([self()], pg:get_members(?FUNCTION_NAME, group)),
    %% ... reject leave too
    ?FUNCTION_NAME ! {leave, PgPid, RemotePid, [group]},
    ?assertEqual([self()], pg:get_members(?FUNCTION_NAME, group)),
    %% join many times on remote
    %RemotePids = [erlang:spawn(TwoPeer, forever()) || _ <- lists:seq(1, 1024)],
    %?assertEqual(ok, rpc:call(TwoPeer, pg, join, [?FUNCTION_NAME, ?FUNCTION_NAME, Pid2])),
    %% check they can't be joined locally
    %?assertException(error, {nolocal, _}, pg:join(?FUNCTION_NAME, ?FUNCTION_NAME, RemotePid)),
    %?assertException(error, {nolocal, _}, pg:join(?FUNCTION_NAME, ?FUNCTION_NAME, [RemotePid, RemotePid])),
    %?assertException(error, {nolocal, _}, pg:join(?FUNCTION_NAME, ?FUNCTION_NAME, [LocalPid, RemotePid])),
    %% check that non-pid also triggers error
    %?assertException(error, function_clause, pg:join(?FUNCTION_NAME, ?FUNCTION_NAME, undefined)),
    %?assertException(error, {nolocal, _}, pg:join(?FUNCTION_NAME, ?FUNCTION_NAME, [undefined])),
    %% stop the peer
    peer:stop(Peer).


two(Config) when is_list(Config) ->
    {Peer, Node} = spawn_node(?FUNCTION_NAME, Config),
    Pid = erlang:spawn(forever()),
    ?assertEqual(ok, pg:join(?FUNCTION_NAME, ?FUNCTION_NAME, Pid)),
    ?assertEqual([Pid], pg:get_local_members(?FUNCTION_NAME, ?FUNCTION_NAME)),
    %% first RPC must be serialised 3 times
    sync({?FUNCTION_NAME, Node}),
    sync(?FUNCTION_NAME),
    sync({?FUNCTION_NAME, Node}),
    ?assertEqual([Pid], rpc:call(Node, pg, get_members, [?FUNCTION_NAME, ?FUNCTION_NAME])),
    ?assertEqual([], rpc:call(Node, pg, get_local_members, [?FUNCTION_NAME, ?FUNCTION_NAME])),
    stop_proc(Pid),
    %% again, must be serialised
    sync(?FUNCTION_NAME),
    sync({?FUNCTION_NAME, Node}),
    ?assertEqual([], pg:get_local_members(?FUNCTION_NAME, ?FUNCTION_NAME)),
    ?assertEqual([], rpc:call(Node, pg, get_members, [?FUNCTION_NAME, ?FUNCTION_NAME])),

    Pid2 = spawn_sleeper_at(Node),
    Pid3 = spawn_sleeper_at(Node),
    ?assertEqual(ok, rpc:call(Node, pg, join, [?FUNCTION_NAME, ?FUNCTION_NAME, Pid2])),
    ?assertEqual(ok, rpc:call(Node, pg, join, [?FUNCTION_NAME, ?FUNCTION_NAME, Pid3])),
    %% serialise through the *other* node
    sync_via({?FUNCTION_NAME, Node}, ?FUNCTION_NAME),
    ?assertEqual(lists:sort([Pid2, Pid3]),
        lists:sort(pg:get_members(?FUNCTION_NAME, ?FUNCTION_NAME))),
    %% stop the peer
    peer:stop(Peer),
    %% hope that 'nodedown' comes before we route our request
    sync(?FUNCTION_NAME),
    ok.

empty_group_by_remote_leave() ->
    [{doc, "Empty group should be deleted from nodes."}].

empty_group_by_remote_leave(Config) when is_list(Config) ->
    {Peer, Node} = spawn_node(?FUNCTION_NAME, Config),
    RemoteNode = rpc:call(Node, erlang, whereis, [?FUNCTION_NAME]),
    RemotePid = spawn_sleeper_at(Node),
    % remote join
    ?assertEqual(ok, rpc:call(Node, pg, join, [?FUNCTION_NAME, ?FUNCTION_NAME, RemotePid])),
    sync_via({?FUNCTION_NAME, Node}, ?FUNCTION_NAME),
    ?assertEqual([RemotePid], pg:get_members(?FUNCTION_NAME, ?FUNCTION_NAME)),
    % inspecting internal state is not best practice, but there's no other way to check if the state is correct.
    {_, RemoteMap} = maps:get(RemoteNode, element(4, sys:get_state(?FUNCTION_NAME))),
    ?assertEqual(#{?FUNCTION_NAME => [RemotePid]}, RemoteMap),
    % remote leave
    ?assertEqual(ok, rpc:call(Node, pg, leave, [?FUNCTION_NAME, ?FUNCTION_NAME, RemotePid])),
    sync_via({?FUNCTION_NAME, Node}, ?FUNCTION_NAME),
    ?assertEqual([], pg:get_members(?FUNCTION_NAME, ?FUNCTION_NAME)),
    {_, NewRemoteMap} = maps:get(RemoteNode, element(4, sys:get_state(?FUNCTION_NAME))),
    % empty group should be deleted.
    ?assertEqual(#{}, NewRemoteMap),

    %% another variant of emptying a group remotely: join([Pi1, Pid2]) and leave ([Pid2, Pid1])
    RemotePid2 = spawn_sleeper_at(Node),
    ?assertEqual(ok, rpc:call(Node, pg, join, [?FUNCTION_NAME, ?FUNCTION_NAME, [RemotePid, RemotePid2]])),
    sync_via({?FUNCTION_NAME, Node}, ?FUNCTION_NAME),
    ?assertEqual([RemotePid, RemotePid2], pg:get_members(?FUNCTION_NAME, ?FUNCTION_NAME)),
    %% now leave
    ?assertEqual(ok, rpc:call(Node, pg, leave, [?FUNCTION_NAME, ?FUNCTION_NAME, [RemotePid2, RemotePid]])),
    sync_via({?FUNCTION_NAME, Node}, ?FUNCTION_NAME),
    ?assertEqual([], pg:get_members(?FUNCTION_NAME, ?FUNCTION_NAME)),
    {_, NewRemoteMap} = maps:get(RemoteNode, element(4, sys:get_state(?FUNCTION_NAME))),
    peer:stop(Peer),
    ok.

thundering_herd() ->
    [{doc, "Thousands of overlay network nodes sending sync to us, and we time out!"}, {timetrap, {seconds, 5}}].

thundering_herd(Config) when is_list(Config) ->
    GroupCount = 10000,
    SyncCount = 2000,
    %% make up a large amount of groups
    [pg:join(?FUNCTION_NAME, {group, Seq}, self()) || Seq <- lists:seq(1, GroupCount)],
    %% initiate a few syncs - and those are really slow...
    {Peer, Node} = spawn_node(?FUNCTION_NAME, Config),
    PeerPid = erlang:spawn(Node, forever()),
    PeerPg = rpc:call(Node, erlang, whereis, [?FUNCTION_NAME], 1000),
    %% WARNING: code below acts for white-box! %% WARNING
    FakeSync = [{{group, 1}, [PeerPid, PeerPid]}],
    [gen_server:cast(?FUNCTION_NAME, {sync, PeerPg, FakeSync}) || _ <- lists:seq(1, SyncCount)],
    %% next call must not timetrap, otherwise test fails
    pg:join(?FUNCTION_NAME, ?FUNCTION_NAME, self()),
    peer:stop(Peer).

initial(Config) when is_list(Config) ->
    Pid = erlang:spawn(forever()),
    ?assertEqual(ok, pg:join(?FUNCTION_NAME, ?FUNCTION_NAME, Pid)),
    ?assertEqual([Pid], pg:get_local_members(?FUNCTION_NAME, ?FUNCTION_NAME)),
    {Peer, Node} = spawn_node(?FUNCTION_NAME, Config),
    %% first sync makes the peer node to process 'nodeup' (and send discover)
    sync({?FUNCTION_NAME, Node}),
    %% second sync makes origin node pg to reply to discover'
    sync(?FUNCTION_NAME),
    %% third sync makes peer node to finish processing 'exchange'
    sync({?FUNCTION_NAME, Node}),
    ?assertEqual([Pid], rpc:call(Node, pg, get_members, [?FUNCTION_NAME, ?FUNCTION_NAME])),

    ?assertEqual([], rpc:call(Node, pg, get_local_members, [?FUNCTION_NAME, ?FUNCTION_NAME])),
    stop_proc(Pid),
    sync({?FUNCTION_NAME, Node}),
    ?assertEqual([], rpc:call(Node, pg, get_members, [?FUNCTION_NAME, ?FUNCTION_NAME])),
    peer:stop(Peer),
    ok.

netsplit(Config) when is_list(Config) ->
    {Peer, Node} = spawn_node(?FUNCTION_NAME, Config),
    ?assertEqual(Node, peer:call(Peer, erlang, node, [])), %% just to test RPC
    RemoteOldPid = spawn_sleeper_at(Node),
    ?assertEqual(ok, rpc:call(Node, pg, join, [?FUNCTION_NAME, '$invisible', RemoteOldPid])),
    sync_via({?FUNCTION_NAME, Node}, ?FUNCTION_NAME),
    ?assertEqual([RemoteOldPid], pg:get_members(?FUNCTION_NAME, '$invisible')),

    %% hohoho, partition!
    disconnect_nodes([Node]),
    ?assertEqual(Node, peer:call(Peer, erlang, node, [])), %% just to ensure RPC still works
    RemotePid = peer:call(Peer, erlang, spawn, sleeper_mfa()),
    ?assertEqual([], peer:call(Peer, erlang, nodes, [])),
    ?assertNot(lists:member(Node, nodes())), %% should be no nodes in the cluster
    ?assertEqual(ok, peer:call(Peer, pg, join, [?FUNCTION_NAME, ?FUNCTION_NAME, RemotePid])), %% join - in a partition!

    ?assertEqual(ok, peer:call(Peer, pg, leave, [?FUNCTION_NAME, '$invisible', RemoteOldPid])),
    ?assertEqual(ok, peer:call(Peer, pg, join, [?FUNCTION_NAME, '$visible', RemoteOldPid])),
    ?assertEqual([RemoteOldPid], peer:call(Peer, pg, get_local_members, [?FUNCTION_NAME, '$visible'])),
    %% join locally too
    LocalPid = erlang:spawn(forever()),
    ?assertEqual(ok, pg:join(?FUNCTION_NAME, ?FUNCTION_NAME, LocalPid)),

    ?assertNot(lists:member(Node, nodes())), %% should be no nodes in the cluster

    PgPid = whereis(?FUNCTION_NAME),
    1 = erlang:trace(PgPid, true, ['receive']),
    pong = net_adm:ping(Node),
    receive
        {trace, PgPid, 'receive', {nodeup, Node}} -> ok
    end,
    1 = erlang:trace(PgPid, false, ['receive']),

    %% now ensure sync happened
    sync_via(?FUNCTION_NAME, {?FUNCTION_NAME, Node}),
    sync_via({?FUNCTION_NAME, Node}, ?FUNCTION_NAME),
    ?assertEqual(lists:sort([RemotePid, LocalPid]),
                 lists:sort(rpc:call(Node, pg, get_members, [?FUNCTION_NAME, ?FUNCTION_NAME]))),
    ?assertEqual([RemoteOldPid], pg:get_members(?FUNCTION_NAME, '$visible')),
    peer:stop(Peer),
    ok.

trisplit(Config) when is_list(Config) ->
    {Peer, Node} = spawn_node(?FUNCTION_NAME, Config),
    _PeerPid1 = spawn_sleeper_at(Node),
    PeerPid2 = spawn_sleeper_at(Node),
    ?assertEqual(ok, rpc:call(Node, pg, join, [?FUNCTION_NAME, three, PeerPid2])),
    disconnect_nodes([Node]),
    ?assertEqual(true, net_kernel:connect_node(Node)),
    ?assertEqual(ok, rpc:call(Node, pg, join, [?FUNCTION_NAME, one, PeerPid2])),
    %% now ensure sync happened
    {Peer2, Node2} = spawn_node(?FUNCTION_NAME, Config),
    ?assertEqual(true, rpc:call(Node2, net_kernel, connect_node, [Node])),
    ?assertEqual(lists:sort([node(), Node]), lists:sort(rpc:call(Node2, erlang, nodes, []))),
    ok = rpc:call(Node2, ?MODULE, ensure_peers_info, [?FUNCTION_NAME, [node(), Node]]),
    ?assertEqual([PeerPid2], rpc:call(Node2, pg, get_members, [?FUNCTION_NAME, one])),
    peer:stop(Peer),
    peer:stop(Peer2),
    ok.

foursplit(Config) when is_list(Config) ->
    Pid = erlang:spawn(forever()),
    {Peer, Node} = spawn_node(?FUNCTION_NAME, Config),
    ?assertEqual(ok, pg:join(?FUNCTION_NAME, one, Pid)),
    ?assertEqual(ok, pg:join(?FUNCTION_NAME, two, Pid)),
    PeerPid1 = erlang:spawn(Node, forever()),
    ?assertEqual(ok, pg:leave(?FUNCTION_NAME, one, Pid)),
    ?assertEqual(not_joined, pg:leave(?FUNCTION_NAME, three, Pid)),
    disconnect_nodes([Node]),
    ?assertEqual(ok, peer:call(Peer, ?MODULE, stop_proc, [PeerPid1])),
    ?assertEqual(not_joined, pg:leave(?FUNCTION_NAME, three, Pid)),
    ?assertEqual(true, net_kernel:connect_node(Node)),
    ?assertEqual([], pg:get_members(?FUNCTION_NAME, one)),
    ?assertEqual([], peer:call(Peer, pg, get_members, [?FUNCTION_NAME, one])),
    peer:stop(Peer),
    ok.

exchange(Config) when is_list(Config) ->
    {Peer1, Node1} = spawn_node(?FUNCTION_NAME, Config),
    {Peer2, Node2} = spawn_node(?FUNCTION_NAME, Config),
    Pids10 = [peer:call(Peer1, erlang, spawn, [forever()]) || _ <- lists:seq(1, 10)],
    Pids2 = [peer:call(Peer2, erlang, spawn, [forever()]) || _ <- lists:seq(1, 10)],
    Pids11 = [peer:call(Peer1, erlang, spawn, [forever()]) || _ <- lists:seq(1, 10)],
    %% kill first 3 pids from node1
    {PidsToKill, Pids1} = lists:split(3, Pids10),

    ?assertEqual(ok, peer:call(Peer1, pg, join, [?FUNCTION_NAME, ?FUNCTION_NAME, Pids10])),
    sync({?FUNCTION_NAME, Node1}), %% Join broadcast have reached local
    sync(?FUNCTION_NAME), %% Join broadcast has been processed by local
    ?assertEqual(lists:sort(Pids10), lists:sort(pg:get_members(?FUNCTION_NAME, ?FUNCTION_NAME))),
    [peer:call(Peer1, ?MODULE, stop_proc, [Pid]) || Pid <- PidsToKill],
    sync(?FUNCTION_NAME),
    sync({?FUNCTION_NAME, Node1}),

    Pids = lists:sort(Pids1 ++ Pids2 ++ Pids11),
    ?assert(lists:all(fun erlang:is_pid/1, Pids)),

    disconnect_nodes([Node1, Node2]),

    sync(?FUNCTION_NAME), %% Processed nodedowns...
    ?assertEqual([], lists:sort(pg:get_members(?FUNCTION_NAME, ?FUNCTION_NAME))),

    [?assertEqual(ok, peer:call(Peer2, pg, join, [?FUNCTION_NAME, ?FUNCTION_NAME, Pid])) || Pid <- Pids2],
    [?assertEqual(ok, peer:call(Peer1, pg, join, [?FUNCTION_NAME, second, Pid])) || Pid <- Pids11],
    ?assertEqual(ok, peer:call(Peer1, pg, join, [?FUNCTION_NAME, third, Pids11])),
    %% rejoin
    ?assertEqual(true, net_kernel:connect_node(Node1)),
    ?assertEqual(true, net_kernel:connect_node(Node2)),
    %% need to sleep longer to ensure both nodes made the exchange
    ensure_peers_info(?FUNCTION_NAME, [Node1, Node2]),
    ?assertEqual(Pids, lists:sort(pg:get_members(?FUNCTION_NAME, second) ++ pg:get_members(?FUNCTION_NAME, ?FUNCTION_NAME))),
    ?assertEqual(lists:sort(Pids11), lists:sort(pg:get_members(?FUNCTION_NAME, third))),

    {Left, Stay} = lists:split(3, Pids11),
    ?assertEqual(ok, peer:call(Peer1, pg, leave, [?FUNCTION_NAME, third, Left])),
    sync({?FUNCTION_NAME, Node1}),
    sync(?FUNCTION_NAME),
    ?assertEqual(lists:sort(Stay), lists:sort(pg:get_members(?FUNCTION_NAME, third))),
    ?assertEqual(not_joined, peer:call(Peer1, pg, leave, [?FUNCTION_NAME, left, Stay])),
    ?assertEqual(ok, peer:call(Peer1, pg, leave, [?FUNCTION_NAME, third, Stay])),
    sync({?FUNCTION_NAME, Node1}),
    sync(?FUNCTION_NAME),
    ?assertEqual([], lists:sort(pg:get_members(?FUNCTION_NAME, third))),
    sync({?FUNCTION_NAME, Node1}),
    sync(?FUNCTION_NAME),

    peer:stop(Peer1),
    peer:stop(Peer2),
    ok.

nolocal(Config) when is_list(Config) ->
    {Peer, Node} = spawn_node(?FUNCTION_NAME, Config),
    RemotePid = spawn_sleeper_at(Node),
    ?assertEqual(ok, rpc:call(Node, pg, join, [?FUNCTION_NAME, ?FUNCTION_NAME, RemotePid])),
    ?assertEqual(ok, rpc:call(Node, pg, join, [?FUNCTION_NAME, ?FUNCTION_NAME, RemotePid])),
    ?assertEqual([], pg:get_local_members(?FUNCTION_NAME, ?FUNCTION_NAME)),
    peer:stop(Peer),
    ok.

double(Config) when is_list(Config) ->
    Pid = erlang:spawn(forever()),
    ?assertEqual(ok, pg:join(?FUNCTION_NAME, ?FUNCTION_NAME, Pid)),
    {Peer, Node} = spawn_node(?FUNCTION_NAME, Config),
    ?assertEqual(ok, pg:join(?FUNCTION_NAME, ?FUNCTION_NAME, [Pid])),
    ?assertEqual([Pid, Pid], pg:get_members(?FUNCTION_NAME, ?FUNCTION_NAME)),
    sync({?FUNCTION_NAME, Node}),
    sync_via(?FUNCTION_NAME, {?FUNCTION_NAME, Node}),
    ?assertEqual([Pid, Pid], rpc:call(Node, pg, get_members, [?FUNCTION_NAME, ?FUNCTION_NAME])),
    peer:stop(Peer),
    ok.

scope_restart(Config) when is_list(Config) ->
    Pid = erlang:spawn(forever()),
    ?assertEqual(ok, pg:join(?FUNCTION_NAME, ?FUNCTION_NAME, [Pid, Pid])),
    {Peer, Node} = spawn_node(?FUNCTION_NAME, Config),
    RemotePid = erlang:spawn(Node, forever()),
    ?assertEqual(ok, rpc:call(Node, pg, join, [?FUNCTION_NAME, ?FUNCTION_NAME, RemotePid])),
    sync_via({?FUNCTION_NAME, Node}, ?FUNCTION_NAME),
    ?assertEqual(lists:sort([RemotePid, Pid, Pid]), lists:sort(pg:get_members(?FUNCTION_NAME, ?FUNCTION_NAME))),
    %% stop scope locally, and restart
    gen_server:stop(?FUNCTION_NAME),
    pg:start(?FUNCTION_NAME),
    %% ensure remote pids joined, local are missing
    sync(?FUNCTION_NAME),
    sync({?FUNCTION_NAME, Node}),
    sync(?FUNCTION_NAME),
    ?assertEqual([RemotePid], pg:get_members(?FUNCTION_NAME, ?FUNCTION_NAME)),
    peer:stop(Peer),
    ok.

missing_scope_join(Config) when is_list(Config) ->
    {Peer, Node} = spawn_node(?FUNCTION_NAME, Config),
    ?assertEqual(ok, rpc:call(Node, gen_server, stop, [?FUNCTION_NAME])),
    RemotePid = erlang:spawn(Node, forever()),
    ?assertMatch({badrpc, {'EXIT', {noproc, _}}}, rpc:call(Node, pg, join, [?FUNCTION_NAME, ?FUNCTION_NAME, RemotePid])),
    ?assertMatch({badrpc, {'EXIT', {noproc, _}}}, rpc:call(Node, pg, leave, [?FUNCTION_NAME, ?FUNCTION_NAME, RemotePid])),
    peer:stop(Peer),
    ok.

disconnected_start(Config) when is_list(Config) ->
    case test_server:is_cover() of
        true ->
            {skip, "Cover is running"};
        false ->
            disconnected_start_test(Config)
    end.

disconnected_start_test(Config) when is_list(Config) ->
    {Peer, Node} = spawn_disconnected_node(?FUNCTION_NAME, ?FUNCTION_NAME, Config),
    ?assertNot(lists:member(Node, nodes())),
    ?assertEqual(ok, peer:call(Peer, gen_server, stop, [?FUNCTION_NAME])),
    ?assertMatch({ok, _Pid}, peer:call(Peer, pg, start,[?FUNCTION_NAME])),
    ?assertEqual(ok, peer:call(Peer, gen_server, stop, [?FUNCTION_NAME])),
    RemotePid = peer:call(Peer, erlang, spawn, [forever()]),
    ?assert(is_pid(RemotePid)),
    peer:stop(Peer),
    ok.

forced_sync() ->
    [{doc, "This test was added when lookup_element was erroneously used instead of lookup, crashing pg with badmatch, and it tests rare out-of-order sync operations"}].

forced_sync(Config) when is_list(Config) ->
    {Peer, Node} = spawn_node(?FUNCTION_NAME, Config),
    Pid = erlang:spawn(forever()),
    RemotePid = erlang:spawn(Node, forever()),
    Expected = lists:sort([Pid, RemotePid]),
    pg:join(?FUNCTION_NAME, one, Pid),

    ?assertEqual(ok, rpc:call(Node, pg, join, [?FUNCTION_NAME, one, RemotePid])),
    RemoteScopePid = rpc:call(Node, erlang, whereis, [?FUNCTION_NAME]),
    ?assert(is_pid(RemoteScopePid)),
    %% hohoho, partition!
    disconnect_nodes([Node]),
    ?assertEqual(true, net_kernel:connect_node(Node)),
    ensure_peers_info(?FUNCTION_NAME, [Node]),
    ?assertEqual(Expected, lists:sort(pg:get_members(?FUNCTION_NAME, one))),

    %% Do extra sync to make sure any redundant sync message has arrived
    %% before we send our fake sync message below.
    sync_via({?FUNCTION_NAME, Node}, ?FUNCTION_NAME),

    %% WARNING: this code uses pg as white-box, exploiting internals,
    %%  only to simulate broken 'sync'
    %% Fake Groups: one should disappear, one should be replaced, one stays
    %% This tests handle_sync function.
    FakeGroups = [{one, [RemotePid, RemotePid]}, {?FUNCTION_NAME, [RemotePid, RemotePid]}],
    gen_server:cast(?FUNCTION_NAME, {sync, RemoteScopePid, FakeGroups}),
    %% ensure it is broken well enough
    sync(?FUNCTION_NAME),
    ?assertEqual(lists:sort([RemotePid, RemotePid]), lists:sort(pg:get_members(?FUNCTION_NAME, ?FUNCTION_NAME))),
    ?assertEqual(lists:sort([RemotePid, RemotePid, Pid]), lists:sort(pg:get_members(?FUNCTION_NAME, one))),
    %% simulate force-sync via 'discover' - ask peer to send sync to us
    {?FUNCTION_NAME, Node} ! {discover, whereis(?FUNCTION_NAME)},
    sync({?FUNCTION_NAME, Node}),
    sync(?FUNCTION_NAME),
    ?assertEqual(Expected, lists:sort(pg:get_members(?FUNCTION_NAME, one))),
    ?assertEqual([], lists:sort(pg:get_members(?FUNCTION_NAME, ?FUNCTION_NAME))),
    %% and simulate extra sync
    sync({?FUNCTION_NAME, Node}),
    sync(?FUNCTION_NAME),
    ?assertEqual(Expected, lists:sort(pg:get_members(?FUNCTION_NAME, one))),

    peer:stop(Peer),
    ok.

group_leave(Config) when is_list(Config) ->
    {Peer, Node} = spawn_node(?FUNCTION_NAME, Config),
    RemotePid = erlang:spawn(Node, forever()),
    Total = lists:duplicate(16, RemotePid),
    {Left, Remain} = lists:split(4, Total),
    %% join 16 times!
    ?assertEqual(ok, rpc:call(Node, pg, join, [?FUNCTION_NAME, two, Total])),
    ?assertEqual(ok, rpc:call(Node, pg, leave, [?FUNCTION_NAME, two, Left])),

    sync({?FUNCTION_NAME, Node}),
    sync(?FUNCTION_NAME),
    ?assertEqual(Remain, pg:get_members(?FUNCTION_NAME, two)),

    PgPid = whereis(?FUNCTION_NAME),
    1 = erlang:trace(PgPid, true, ['receive']),
    peer:stop(Peer),
    receive
        {trace, PgPid, 'receive', {nodedown, Node}} -> ok
    end,
    1 = erlang:trace(PgPid, false, ['receive']),
    sync(?FUNCTION_NAME),
    ?assertEqual([], pg:get_members(?FUNCTION_NAME, two)),
    ok.

monitor_nonempty_scope() ->
    [{doc, "Ensure that monitor_scope returns full map of groups in the scope"}].

monitor_nonempty_scope(Config) when is_list(Config) ->
    {Peer, Node} = spawn_node(?FUNCTION_NAME, Config),
    Pid = erlang:spawn_link(forever()),
    RemotePid = erlang:spawn(Node, forever()),
    Expected = lists:sort([Pid, RemotePid]),
    pg:join(?FUNCTION_NAME, one, Pid),
    ?assertEqual(ok, rpc:call(Node, pg, join, [?FUNCTION_NAME, one, RemotePid])),
    %% Ensure that initial monitoring request returns current map of groups to pids
    {Ref, #{one := Actual} = FullScope} = pg:monitor_scope(?FUNCTION_NAME),
    ?assertEqual(Expected, Actual),
    %% just in case - check there are no extra groups in that scope
    ?assertEqual(1, map_size(FullScope)),
    pg:demonitor(?FUNCTION_NAME, Ref),
    %% re-check
    {_Ref, FullScope} = pg:monitor_scope(?FUNCTION_NAME),
    peer:stop(Peer),
    exit(Pid, normal).

monitor_scope() ->
    [{doc, "Tests monitor_scope/1 and demonitor/2"}].

monitor_scope(Config) when is_list(Config) ->
    %% ensure that demonitoring returns 'false' when monitor is not installed
    ?assertEqual(false, pg:demonitor(?FUNCTION_NAME, erlang:make_ref())),
    InitialMonitor = fun (Scope) -> {Ref, #{}} = pg:monitor_scope(Scope), Ref end,
    SecondMonitor = fun (Scope, Group, Control) -> {Ref, #{Group := [Control]}} = pg:monitor_scope(Scope), Ref end,
    %% WHITE BOX: knowing pg state internals - only the original monitor should stay
    DownMonitor = fun (Scope, Ref, Self) ->
        {state, _, _, _, ScopeMonitors, _, _} = sys:get_state(Scope),
        ?assertEqual(#{Ref => Self}, ScopeMonitors, "pg did not remove DOWNed scope monitor")
                  end,
    monitor_test_impl(Config, ?FUNCTION_NAME, ?FUNCTION_ARITY, InitialMonitor,
                      SecondMonitor, DownMonitor).

monitor(Config) when is_list(Config) ->
    ExpectedGroup = {?FUNCTION_NAME, ?FUNCTION_ARITY},
    InitialMonitor = fun (Scope) -> {Ref, []} = pg:monitor(Scope, ExpectedGroup), Ref end,
    SecondMonitor = fun (Scope, Group, Control) ->
        {Ref, [Control]} = pg:monitor(Scope, Group), Ref end,
    DownMonitor = fun (Scope, Ref, Self) ->
        {state, _, _, _, _, GM, MG} = sys:get_state(Scope),
        ?assertEqual(#{Ref => {Self, ExpectedGroup}}, GM, "pg did not remove DOWNed group monitor"),
        ?assertEqual(#{ExpectedGroup => [{Self, Ref}]}, MG, "pg did not remove DOWNed group")
                  end,
    monitor_test_impl(Config, ?FUNCTION_NAME, ExpectedGroup, InitialMonitor,
                      SecondMonitor, DownMonitor).

monitor_test_impl(Config, Scope, Group, InitialMonitor, SecondMonitor, DownMonitor) ->
    Self = self(),
    Ref = InitialMonitor(Scope),
    %% local join
    ?assertEqual(ok, pg:join(Scope, Group, Self)),
    wait_message(Ref, join, Group, [Self], "Local"),
    %% start second monitor (which has 1 local pid at the start)
    ExtraMonitor = spawn_link(fun() -> second_monitor(Scope, Group, Self, SecondMonitor) end),
    Ref2 = receive {ExtraMonitor, SecondRef} -> SecondRef end,
    %% start a remote node, and a remote monitor
    {Peer, Node} = spawn_node(Scope, Config),
    ScopePid = whereis(Scope),
    %% do not care about the remote monitor, it is started only to check DOWN handling
    ThirdMonitor = spawn_link(Node, fun() -> second_monitor(ScopePid, Group, Self, SecondMonitor) end),
    Ref3 = receive {ThirdMonitor, ThirdRef} -> ThirdRef end,
    %% remote join
    RemotePid = erlang:spawn(Node, forever()),
    ?assertEqual(ok, rpc:call(Node, pg, join, [Scope, Group, [RemotePid, RemotePid]])),
    wait_message(Ref, join, Group, [RemotePid, RemotePid], "Remote"),
    %% verify leave event
    ?assertEqual([Self], pg:get_local_members(Scope, Group)),
    ?assertEqual(ok, pg:leave(Scope, Group, self())),
    wait_message(Ref, leave, Group, [Self], "Local"),
    %% remote leave
    ?assertEqual(ok, rpc:call(Node, pg, leave, [Scope, Group, RemotePid])),
    %% flush the local pg scope via remote pg (to ensure local pg finished sending notifications)
    sync_via({?FUNCTION_NAME, Node}, ?FUNCTION_NAME),
    wait_message(Ref, leave, Group, [RemotePid], "Remote"),
    %% drop the ExtraMonitor - this keeps original and remote monitors
    SecondMonMsgs = gen_server:call(ExtraMonitor, flush),
    %% inspect the queue, it should contain double remote join, then single local and single remove leave
    ExpectedLocalMessages = [
        {Ref2, join, Group, [RemotePid, RemotePid]},
        {Ref2, leave, Group, [Self]},
        {Ref2, leave, Group, [RemotePid]}],
    ?assertEqual(ExpectedLocalMessages, SecondMonMsgs, "Local monitor failed"),
    %% inspect remote monitor queue
    ThirdMonMsgs = gen_server:call(ThirdMonitor, flush),
    ExpectedRemoteMessages = [
        {Ref3, join, Group, [RemotePid, RemotePid]},
        {Ref3, leave, Group, [Self]},
        {Ref3, leave, Group, [RemotePid]}],
    ?assertEqual(ExpectedRemoteMessages, ThirdMonMsgs, "Remote monitor failed"),
    %% remote leave via stop (causes remote monitor to go DOWN)
    ok = peer:stop(Peer),
    wait_message(Ref, leave, Group, [RemotePid], "Remote stop"),
    DownMonitor(Scope, Ref, Self),
    %% demonitor
    ?assertEqual(ok, pg:demonitor(Scope, Ref)),
    ?assertEqual(false, pg:demonitor(Scope, Ref)),
    %% ensure messages don't come
    ?assertEqual(ok, pg:join(Scope, Group, Self)),
    sync(Scope),
    %% join should not be here
    receive {Ref, Action, Group, [Self]} -> ?assert(false, lists:concat(["Unexpected ", Action, "event"]))
    after 0 -> ok end.

wait_message(Ref, Action, Group, Pids, Msg) ->
    receive
        {Ref, Action, Group, Pids} ->
            ok
    after 1000 ->
        {messages, Msgs} = process_info(self(), messages),
        ct:pal("Message queue: ~0p", [Msgs]),
        ?assert(false, lists:flatten(io_lib:format("Expected ~s ~s for ~p", [Msg, Action, Group])))
    end.

second_monitor(Scope, Group, Control, SecondMonitor) ->
    Ref = SecondMonitor(Scope, Group, Control),
    Control ! {self(), Ref},
    second_monitor([]).

second_monitor(Msgs) ->
    receive
        {'$gen_call', Reply, flush} ->
            gen:reply(Reply, lists:reverse(Msgs));
        Msg ->
            second_monitor([Msg | Msgs])
    end.

%% Test for GH-7625: monitor process that joined a group
monitor_self(Config) when is_list(Config) ->
    F = fun() ->
        %% spawned process both monitor and group-joined
        pg:monitor(?FUNCTION_NAME, ?FUNCTION_NAME),
        pg:join(?FUNCTION_NAME, ?FUNCTION_NAME, self())
        end,
    {Pid, Mon} = spawn_monitor(F),
    receive
        {'DOWN', Mon, process, Pid, Reason} ->
            ?assertEqual(normal, Reason)
    end,
    %% if pg crashes, next expression fails the test
    sync(?FUNCTION_NAME).

%% check same process monitoring several things at once,
%% and also joining a few groups
multi_monitor(Config) when is_list(Config) ->
    F = fun() ->
        Self = self(),
        %% spawned process both monitor and group-joined
        {RefOne, []} = pg:monitor(?FUNCTION_NAME, one),
        {RefTwo, []} = pg:monitor(?FUNCTION_NAME, two),
        {RefScope, _} = pg:monitor_scope(?FUNCTION_NAME),
        ok = pg:join(?FUNCTION_NAME, one, Self),
        ok = pg:join(?FUNCTION_NAME, two, Self),
        sync(?FUNCTION_NAME),
        %% ensure receiving 4 messages: two per group this process
        [wait_message(Ref, join, Group, [Self], "Local") || {Ref, Group} <-
            [{RefOne, one}, {RefScope, one}, {RefTwo, two}, {RefScope, two}]]
        end,
    {Pid, Mon} = spawn_monitor(F),
    receive
        {'DOWN', Mon, process, Pid, Reason} ->
            ?assertEqual(normal, Reason)
    end,
    %% if pg crashes, next expression fails the test
    sync(?FUNCTION_NAME),
    %% white box: pg should not have any group or scope monitors
    {state, _, _, _, SM, GM, _} = sys:get_state(?FUNCTION_NAME),
    ?assertEqual(#{}, SM),
    ?assertEqual(#{}, GM).

protocol_upgrade(Config) when is_list(Config) ->
    Scope = ?FUNCTION_NAME,
    Group = ?FUNCTION_NAME,
    {Peer, Node} = spawn_node(Scope, Config),
    PgPid = rpc:call(Node, erlang, whereis, [Scope]),

    RemotePid = erlang:spawn(Node, forever()),
    ok = rpc:call(Node, pg, join, [Scope, Group, RemotePid]),

    %% OTP 26:
    %% Just do a white-box test and verify that pg accepts
    %% a "future" discover message and replies with a sync.
    PgPid ! {discover, self(), "Protocol version (ignore me)"},
    {'$gen_cast', {sync, PgPid, [{Group, [RemotePid]}]}} = receive_any(),

    %% stop the peer
    peer:stop(Peer),
    ok.


%%--------------------------------------------------------------------
%% Test Helpers - start/stop additional Erlang nodes

receive_any() ->
    receive M -> M end.

%% flushes GS (GenServer) queue, ensuring that all prior
%%  messages have been processed
sync(GS) ->
    _ = sys:log(GS, get).

%% flushes GS queue from the point of view of a registered process RegName
%%  running on the Node.
sync_via({RegName, Node}, GS) ->
    MyNode = node(),
    rpc:call(Node, sys, replace_state,
             [RegName, fun (S) -> (catch sys:get_state({GS, MyNode})), S end]);

%% flush remote GS queue from local process RegName
sync_via(RegName, {GS, Node}) ->
    sys:replace_state(RegName,
                      fun (S) -> _R = (catch sys:get_state({GS, Node})),
                                 %%io:format("sync_via: ~p -> R = ~p\n", [{GS, Node},_R]),
                                 S
                      end).

ensure_peers_info(Scope, Nodes) ->
    %% Ensures that pg server on local node has gotten info from
    %% pg servers on all Peer nodes passed as argument (assuming
    %% no connection failures).
    %% 
    %% This function assumes that all nodeup messages has been
    %% delivered to all local recipients (pg server) when called.
    %%
    %% Note that this relies on current ERTS implementation; not
    %% language guarantees.
    %%

    sync(Scope),
    %% Known: nodeup handled and discover sent to Peer

    lists:foreach(fun (Node) -> sync({Scope, Node}) end, Nodes),
    %% Known: nodeup handled by Peers and discover sent to local
    %% Known: discover received/handled by Peers and sync sent to local
    %% Known: discover received from Peer
    %% Known: sync received from Peer

    sync(Scope),
    %% Known: discover handled from Peers and sync sent to Peers
    %% Known: sync from Peers handled
    ok.

-ifdef(CURRENTLY_UNUSED_BUT_SERVES_AS_DOC).

ensure_synced(Scope, Nodes) ->
    %% Ensures that the pg server on local node have synced
    %% with pg servers on all Peer nodes (assuming no connection
    %% failures).
    %% 
    %% This function assumes that all nodeup messages has been
    %% delivered to all local recipients (pg server) when called.
    %%
    %% Note that this relies on current ERTS implementation; not
    %% language guarantees.
    %%
    ensure_peer_info(Scope, Node),
    %% Known: local has gotten info from all Peers
    %% Known: discover from Peers handled and sync sent to Peers
    lists:foreach(fun (Node) -> sync({Scope, Node}) end, Nodes),
    %% Known: sync from local handled by Peers
    ok.

-endif.

disconnect_nodes(Nodes) ->
    %% The following is not a language guarantee, but internal
    %% knowledge about current implementation of ERTS and pg.
    %%
    %% The pg server reacts on 'DOWN's via process monitors of
    %% its peers. These are delivered before 'nodedown's from
    %% net_kernel:monitor_nodes(). That is, by waiting for
    %% 'nodedown' from net_kernel:monitor_nodes() we know that
    %% the 'DOWN' has been delivered to the pg server.
    %%
    %% We do this in a separate process to avoid stray
    %% nodeup/nodedown messages in the test process after
    %% the operation...
    F = fun () ->
                ok = net_kernel:monitor_nodes(true),
                lists:foreach(fun (Node) ->
                                      true = erlang:disconnect_node(Node)
                              end,
                              Nodes),
                lists:foreach(fun (Node) ->
                                      receive {nodedown, Node} -> ok end
                              end,
                              Nodes)
        end,
    {Pid, Mon} = spawn_monitor(F),
    receive
        {'DOWN', Mon, process, Pid, Reason} ->
            ?assertEqual(normal, Reason)
    end,
    ok.

%% @doc Kills process Pid and waits for it to exit using monitor,
%%      and yields after (for 1 ms).
-spec stop_proc(pid()) -> ok.
stop_proc(Pid) ->
    monitor(process, Pid),
    erlang:exit(Pid, kill),
    receive
        {'DOWN', _MRef, process, Pid, _Info} ->
            timer:sleep(1)
    end.

forever() ->
    Parent = self(),
    fun() ->
            %% forever() is used both locally and on a remote node,
            %% if used locally, we want to terminate when the
            %% parent terminates in order to not leak process to
            %% later test cases
            Ref = monitor(process,Parent),
            receive
                {'DOWN',Ref,_,_,_} when node() =:= node(Parent) ->
                    ok
            end
    end.

%% Spawn a sleeping process on remote node.
%% Works on older nodes also without having to run any specially compiled code.
spawn_sleeper_at(Node) when Node =/= node() ->
    spawn(Node, erlang, hibernate, [?MODULE,dummy,[]]).

sleeper_mfa() ->
    [erlang, hibernate, [?MODULE,dummy,[]]].

spawn_node(TestCase, Config) ->
    {Peer, Node} = spawn_disconnected_node(TestCase, TestCase, Config),
    true = net_kernel:connect_node(Node),
    {Peer, Node}.

spawn_disconnected_node(Scope, TestCase, Config) ->
    Opts = #{name => ?CT_PEER_NAME(TestCase),
             connection => 0,
             args => ["-connect_all", "false",
                      "-kernel", "dist_auto_connect", "never"]},
    {ok, Peer, Node} =
        case proplists:get_value(otp_release, Config) of
            undefined ->
                ?CT_PEER(Opts);
            Release ->
                TcPrivDir = filename:join(proplists:get_value(priv_dir, Config),
                                          TestCase),
                ok = ensure_dir(TcPrivDir),
                ?CT_PEER_REL(Opts, Release, TcPrivDir)
        end,
    {ok, _Pid} = peer:call(Peer, pg, start, [Scope]),
    {Peer, Node}.

ensure_dir(Dir) ->
    case file:make_dir(Dir) of
        ok -> ok;
        {error, eexist} -> ok;
        E -> E
    end.


%%--------------------------------------------------------------------
%% Debug Helpers

%% Add test cases here to enable 'receive' trace of the local pg process
traced_testcases() -> [].

trace_start(TestCase, Config, Tracee) ->
    case lists:member(TestCase, traced_testcases()) of
        true ->
            Tracer = spawn_link(fun() -> tracer() end),
            1 = erlang:trace(Tracee, true, ['receive', {tracer, Tracer}, timestamp]),
            [{tracer, Tracer} | Config];
        false ->
            Config
    end.

trace_end(Config) ->
    case proplists:get_value(tracer, Config) of
        undefined -> ok;
        Tracer ->
            Mon = erlang:monitor(process, Tracer),
            Tracer ! flush,
            normal = receive
                         {'DOWN', Mon, process, Tracer, R} -> R
                     end
    end.

tracer() ->
    receive flush -> ok end,
    io:format("Flush trace messages:\n"),
    tracer_flush().

tracer_flush() ->
    receive M ->
            io:format("~p\n", [M]),
            tracer_flush()
    after 0 ->
            io:format("Flush done.\n")
    end.
