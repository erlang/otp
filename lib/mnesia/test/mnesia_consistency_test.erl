%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1997-2025. All Rights Reserved.
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

%%
-module(mnesia_consistency_test).
-author('hakan@erix.ericsson.se').

-export([init_per_testcase/2, end_per_testcase/2,
         init_per_group/2, end_per_group/2,
         all/0, groups/0]).

-export([consistency_after_change_table_copy_type/1,
         consistency_after_restart_1_ram/1,
         consistency_after_restart_1_disc/1,
         consistency_after_restart_1_disc_only/1,
         consistency_after_restart_2_ram/1,
         consistency_after_restart_2_disc/1,
         consistency_after_restart_2_disc_only/1,
         consistency_after_isolated_restart_2_nodes/1,
         consistency_after_isolated_restart_3_nodes/1,
         consistency_after_isolated_restart_local_master_3_nodes/1,
         consistency_after_isolated_restart_remote_master_3_nodes/1,
         master_nodes_ignores_active_non_master_3_nodes/1,
         master_nodes_multiple_remote_masters_3_nodes/1,
         master_nodes_clear_while_waiting_3_nodes/1,
         master_nodes_expand_while_waiting_3_nodes/1,
         master_nodes_mutual_remote_masters_3_nodes/1,
         master_nodes_remote_master_loads_locally_3_nodes/1,
         master_nodes_mixed_storage_orphan_master_3_nodes/1,
         master_nodes_local_member_precedence_3_nodes/1,
         master_nodes_two_local_masters_partitioned_3_nodes/1,
         consistency_after_dump_tables_1_ram/1,
         consistency_after_dump_tables_2_ram/1,
         consistency_after_add_replica_2_ram/1,
         consistency_after_add_replica_2_disc/1,
         consistency_after_add_replica_2_disc_only/1,
         consistency_after_add_replica_3_ram/1,
         consistency_after_add_replica_3_disc/1,
         consistency_after_add_replica_3_disc_only/1,
         consistency_after_del_replica_2_ram/1,
         consistency_after_del_replica_2_disc/1,
         consistency_after_del_replica_2_disc_only/1,
         consistency_after_del_replica_3_ram/1,
         consistency_after_del_replica_3_disc/1,
         consistency_after_del_replica_3_disc_only/1,
         consistency_after_move_replica_2_ram/1,
         consistency_after_move_replica_2_disc/1,
         consistency_after_move_replica_2_disc_only/1,
         consistency_after_move_replica_3_ram/1,
         consistency_after_move_replica_3_disc/1,
         consistency_after_move_replica_3_disc_only/1,
         consistency_after_transform_table_ram/1,
         consistency_after_transform_table_disc/1,
         consistency_after_transform_table_disc_only/1,
         consistency_after_fallback_2_ram/1,
         consistency_after_fallback_2_disc/1,
         consistency_after_fallback_2_disc_only/1,
         consistency_after_fallback_3_ram/1,
         consistency_after_fallback_3_disc/1,
         consistency_after_fallback_3_disc_only/1,
         consistency_after_restore_clear_ram/1,
         consistency_after_restore_clear_disc/1,
         consistency_after_restore_clear_disc_only/1,
         consistency_after_restore_recreate_ram/1,
         consistency_after_restore_recreate_disc/1,
         consistency_after_restore_recreate_disc_only/1,
         updates_during_checkpoint_activation_1_ram/1,
         updates_during_checkpoint_activation_1_disc/1,
         updates_during_checkpoint_activation_1_disc_only/1,
         updates_during_checkpoint_activation_2_ram/1,
         updates_during_checkpoint_activation_2_disc/1,
         updates_during_checkpoint_activation_2_disc_only/1,
         updates_during_checkpoint_activation_3_ram/1,
         updates_during_checkpoint_activation_3_disc/1,
         updates_during_checkpoint_activation_3_disc_only/1,
         updates_during_checkpoint_iteration_2_ram/1,
         updates_during_checkpoint_iteration_2_disc/1,
         updates_during_checkpoint_iteration_2_disc_only/1,
         load_table_with_activated_checkpoint_ram/1,
         load_table_with_activated_checkpoint_disc/1,
         load_table_with_activated_checkpoint_disc_only/1,
         add_table_copy_to_table_checkpoint_ram/1,
         add_table_copy_to_table_checkpoint_disc/1,
         add_table_copy_to_table_checkpoint_disc_only/1,
         inst_fallback_process_dies/1, fatal_when_inconsistency/1,
         after_delete/1,cause_switch_before/1, cause_switch_after/1,
         cause_abort_before/1, cause_abort_after/1,
         change_schema_before/1, change_schema_after/1]).

-export([change_tab/3]).

-import(mnesia_test_lib,
        [mnesia_node_call/2,
         mnesia_node_set_masters/3,
         mnesia_node_assert_master_policies/2,
         mnesia_node_stop/2,
         mnesia_node_assert_down_logged/2,
         mnesia_node_kill/2,
         mnesia_node_start/1,
         mnesia_node_set_partitions/1,
         mnesia_node_set_connected_groups/1,
         mnesia_node_assert_topology/1,
         mnesia_node_assert_running/1,
         mnesia_node_connect/1,
         mnesia_node_assert_active_replica/3,
         mnesia_node_assert_waiting/2,
         mnesia_node_assert_locally_readable/2,
         mnesia_node_assert_loaded_from/4,
         mnesia_node_assert_load_reason/3,
         mnesia_node_assert_local_records/3,
         mnesia_node_assert_eventually/2,
         mnesia_node_with_observer/4,
         mnesia_node_get_observed_events/2,
         mnesia_node_get_orphan_load_requests/3,
         mnesia_node_has_inconsistency_event/3]).

-include("mnesia_test_lib.hrl").
-include_lib("stdlib/include/assert.hrl").

init_per_testcase(Func, Conf) ->
    mnesia_test_lib:init_per_testcase(Func, Conf).

end_per_testcase(Func, Conf) ->
    mnesia_test_lib:end_per_testcase(Func, Conf).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all() -> 
    [{group, consistency_after_restart},
     {group, consistency_after_isolated_restart},
     {group, consistency_after_dump_tables},
     {group, consistency_after_add_replica},
     {group, consistency_after_del_replica},
     {group, consistency_after_move_replica},
     {group, consistency_after_transform_table},
     %% consistency_after_change_table_copy_type,
     {group, consistency_after_restore},
     {group, checkpoint_retainer_consistency},
     {group, backup_consistency}].

groups() -> 
    [{consistency_after_restart, [],
      [consistency_after_restart_1_ram,
       consistency_after_restart_1_disc,
       consistency_after_restart_1_disc_only,
       consistency_after_restart_2_ram,
       consistency_after_restart_2_disc,
       consistency_after_restart_2_disc_only]},
     {consistency_after_isolated_restart, [],
      [consistency_after_isolated_restart_2_nodes,
       consistency_after_isolated_restart_3_nodes,
       consistency_after_isolated_restart_local_master_3_nodes,
       consistency_after_isolated_restart_remote_master_3_nodes,
       master_nodes_ignores_active_non_master_3_nodes,
       master_nodes_multiple_remote_masters_3_nodes,
       master_nodes_clear_while_waiting_3_nodes,
       master_nodes_expand_while_waiting_3_nodes,
       master_nodes_mutual_remote_masters_3_nodes,
       master_nodes_remote_master_loads_locally_3_nodes,
       master_nodes_mixed_storage_orphan_master_3_nodes,
       master_nodes_local_member_precedence_3_nodes,
       master_nodes_two_local_masters_partitioned_3_nodes]},
     {consistency_after_dump_tables, [],
      [consistency_after_dump_tables_1_ram,
       consistency_after_dump_tables_2_ram]},
     {consistency_after_add_replica, [],
      [consistency_after_add_replica_2_ram,
       consistency_after_add_replica_2_disc,
       consistency_after_add_replica_2_disc_only,
       consistency_after_add_replica_3_ram,
       consistency_after_add_replica_3_disc,
       consistency_after_add_replica_3_disc_only]},
     {consistency_after_del_replica, [],
      [consistency_after_del_replica_2_ram,
       consistency_after_del_replica_2_disc,
       consistency_after_del_replica_2_disc_only,
       consistency_after_del_replica_3_ram,
       consistency_after_del_replica_3_disc,
       consistency_after_del_replica_3_disc_only]},
     {consistency_after_move_replica, [],
      [consistency_after_move_replica_2_ram,
       consistency_after_move_replica_2_disc,
       consistency_after_move_replica_2_disc_only,
       consistency_after_move_replica_3_ram,
       consistency_after_move_replica_3_disc,
       consistency_after_move_replica_3_disc_only]},
     {consistency_after_transform_table, [],
      [consistency_after_transform_table_ram,
       consistency_after_transform_table_disc,
       consistency_after_transform_table_disc_only]},
     {consistency_after_fallback, [],
      [consistency_after_fallback_2_ram,
       consistency_after_fallback_2_disc,
       consistency_after_fallback_2_disc_only,
       consistency_after_fallback_3_ram,
       consistency_after_fallback_3_disc,
       consistency_after_fallback_3_disc_only]},
     {consistency_after_restore, [],
      [consistency_after_restore_clear_ram,
       consistency_after_restore_clear_disc,
       consistency_after_restore_clear_disc_only,
       consistency_after_restore_recreate_ram,
       consistency_after_restore_recreate_disc,
       consistency_after_restore_recreate_disc_only]},
     {checkpoint_retainer_consistency, [],
      [{group, updates_during_checkpoint_activation},
       {group, updates_during_checkpoint_iteration},
       {group, load_table_with_activated_checkpoint},
       {group, add_table_copy_to_table_checkpoint},
       {group, consistency_after_fallback}
      ]},
     {updates_during_checkpoint_activation, [],
      [updates_during_checkpoint_activation_1_ram,
       updates_during_checkpoint_activation_1_disc,
       updates_during_checkpoint_activation_1_disc_only,
       updates_during_checkpoint_activation_2_ram,
       updates_during_checkpoint_activation_2_disc,
       updates_during_checkpoint_activation_2_disc_only,
       updates_during_checkpoint_activation_3_ram,
       updates_during_checkpoint_activation_3_disc,
       updates_during_checkpoint_activation_3_disc_only]},
     {updates_during_checkpoint_iteration, [],
      [updates_during_checkpoint_iteration_2_ram,
       updates_during_checkpoint_iteration_2_disc,
       updates_during_checkpoint_iteration_2_disc_only]},
     {load_table_with_activated_checkpoint, [],
      [load_table_with_activated_checkpoint_ram,
       load_table_with_activated_checkpoint_disc,
       load_table_with_activated_checkpoint_disc_only]},
     {add_table_copy_to_table_checkpoint, [],
      [add_table_copy_to_table_checkpoint_ram,
       add_table_copy_to_table_checkpoint_disc,
       add_table_copy_to_table_checkpoint_disc_only]},
     {backup_consistency, [],
      [{group, interupted_install_fallback},
       {group, interupted_uninstall_fallback},
       {group, mnesia_down_during_backup_causes_switch},
       {group, mnesia_down_during_backup_causes_abort},
       {group, schema_transactions_during_backup}]},
     {interupted_install_fallback, [],
      [inst_fallback_process_dies, fatal_when_inconsistency]},
     {interupted_uninstall_fallback, [], [after_delete]},
     {mnesia_down_during_backup_causes_switch, [],
      [cause_switch_before, cause_switch_after]},
     {mnesia_down_during_backup_causes_abort, [],
      [cause_abort_before, cause_abort_after]},
     {schema_transactions_during_backup, [],
      [change_schema_before, change_schema_after]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  stolen from mnesia_tpcb.erl:

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Account record, total size must be at least 100 bytes

-define(ACCOUNT_FILLER,
        {123456789012345678901234567890123456789012345678901234567890,
         123456789012345678901234567890123456789012345678901234567890,
         123456789012345678901234567890123456789012345678901234}).

-record(account,
       {
        id           = 0, %% Unique account id
        branch_id    = 0, %% Branch where the account is held
        balance      = 0, %% Account balance
        filler       = ?ACCOUNT_FILLER  %% Gap filler to ensure size >= 100 bytes
       }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Branch record, total size must be at least 100 bytes

-define(BRANCH_FILLER,
        {123456789012345678901234567890123456789012345678901234567890,
         123456789012345678901234567890123456789012345678901234567890,
         123456789012345678901234567890123456789012345678901234567890}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Teller record, total size must be at least 100 bytes

-define(TELLER_FILLER,
        {123456789012345678901234567890123456789012345678901234567890,
         123456789012345678901234567890123456789012345678901234567890,
         1234567890123456789012345678901234567890123456789012345678}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% History record, total size must be at least 50 bytes

-define(HISTORY_FILLER, 1234567890).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(tab_config,
        {
         db_nodes = [node()],
         replica_nodes = [node()],
         replica_type = ram_copies,
         use_running_mnesia = false,
         n_branches = 1,
         n_tellers_per_branch = 10, %% Must be 10
         n_accounts_per_branch = 100000, %% Must be 100000
         branch_filler = ?BRANCH_FILLER,
         account_filler = ?ACCOUNT_FILLER,
         teller_filler = ?TELLER_FILLER
        }).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  stolen from mnesia_tpcb.erl:

list2rec(List, Fields, DefaultTuple) ->
    [Name|Defaults] = tuple_to_list(DefaultTuple),
    List2 = list2rec(List, Fields, Defaults, []),
    list_to_tuple([Name] ++ List2).

list2rec(_List, [], [], Acc) ->
    Acc;
list2rec(List, [F|Fields], [D|Defaults], Acc) ->
    {Val, List2} =
        case lists:keysearch(F, 1, List) of
            false ->
                {D, List};
            {value, {F, NewVal}} ->
                {NewVal, lists:keydelete(F, 1, List)}
        end,
    list2rec(List2, Fields, Defaults, Acc ++ [Val]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tpcb_config(ReplicaType, _NodeConfig, Nodes, NoDriverNodes) ->
    [{n_branches, 10},
     {n_drivers_per_node, 10},
     {replica_nodes, Nodes},
     {driver_nodes, Nodes -- NoDriverNodes},
     {use_running_mnesia, true},
     {report_interval, infinity},
     {n_accounts_per_branch, 100},
     {replica_type, ReplicaType},
     {reuse_history_id, true}].

%% Stolen from mnesia_tpcb:dist
tpcb_config_dist(ReplicaType, _NodeConfig, Nodes, _Config) ->
    [{db_nodes, Nodes},
     {driver_nodes, Nodes},
     {replica_nodes, Nodes},
     {n_drivers_per_node, 10},
     {n_branches, 1},
     {use_running_mnesia, true},
     {n_accounts_per_branch, 10},
     {replica_type, ReplicaType},
     {stop_after, timer:minutes(15)},
     {report_interval, timer:seconds(10)},
     {reuse_history_id, true}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  stolen from mnesia_recovery_test.erl:

receive_messages([]) -> [];
receive_messages(ListOfMsgs) ->
    receive 
        {Pid, Msg} ->     
            case lists:member(Msg, ListOfMsgs) of
                false -> 
                    ?warning("I (~p) have received unexpected msg~n ~p ~n",
                        [self(),{Pid, Msg}]),
                    receive_messages(ListOfMsgs);
                true -> 
                    ?verbose("I (~p) got msg ~p from ~p ~n", [self(),Msg, Pid]),
                    [{Pid, Msg} | receive_messages(ListOfMsgs -- [Msg])]
            end;
        Else -> ?warning("Received unexpected Msg~n ~p ~n", [Else])
    after timer:minutes(3) -> 
            ?error("Timeout in receive msgs while waiting for ~p~n", 
                   [ListOfMsgs])
    end.  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

consistency_after_restart_1_ram(suite) -> [];
consistency_after_restart_1_ram(Config) when is_list(Config) ->
    consistency_after_restart(ram_copies, 2, Config).

consistency_after_restart_1_disc(suite) -> [];
consistency_after_restart_1_disc(Config) when is_list(Config) ->
    consistency_after_restart(disc_copies, 2, Config).

consistency_after_restart_1_disc_only(suite) -> [];
consistency_after_restart_1_disc_only(Config) when is_list(Config) ->
    consistency_after_restart(disc_only_copies, 2, Config).

consistency_after_restart_2_ram(suite) -> [];
consistency_after_restart_2_ram(Config) when is_list(Config) ->
    consistency_after_restart(ram_copies, 3, Config).

consistency_after_restart_2_disc(suite) -> [];
consistency_after_restart_2_disc(Config) when is_list(Config) ->
    consistency_after_restart(disc_copies, 3, Config).

consistency_after_restart_2_disc_only(suite) -> [];
consistency_after_restart_2_disc_only(Config) when is_list(Config) ->
    consistency_after_restart(disc_only_copies, 3, Config).

consistency_after_restart(ReplicaType, NodeConfig, Config) ->
    [Node1 | _] = Nodes = ?acquire_nodes(NodeConfig, Config),
    {success, [A]} = ?start_activities([Node1]),
    ?log("consistency_after_restart with ~p on ~p~n",
         [ReplicaType, Nodes]),
    TpcbConfig = tpcb_config(ReplicaType, NodeConfig, Nodes, [Node1]),
    mnesia_tpcb:init(TpcbConfig),
    A ! fun () -> mnesia_tpcb:run(TpcbConfig) end,
    timer:sleep(timer:seconds(3)),
    mnesia_test_lib:kill_mnesia([Node1]),
    %% Start and wait for tables to be loaded on all nodes
    timer:sleep(timer:seconds(3)),
    ?match([], mnesia_test_lib:start_mnesia(Nodes,[account,branch,teller, history])), 
    mnesia_tpcb:stop(),
    ?match(ok, mnesia_tpcb:verify_tabs()),
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

consistency_after_isolated_restart_2_nodes(suite) -> [];
consistency_after_isolated_restart_2_nodes(Config) when is_list(Config) ->
    [Node1, Node2] = Nodes = ?acquire_nodes(2, Config),

    N = 10,

    ?match({atomic, ok}, mnesia:create_table(ram, [{ram_copies, Nodes}])),
    ?match(ok, mnesia:sync_dirty(fun() ->
        [mnesia:write({ram, K, K}) || K <- lists:seq(1, N)], ok end)),

    case mnesia_test_lib:diskless(Config) of
        true ->
            ok;
        false ->
            ?match({atomic, ok}, mnesia:create_table(disc, [{disc_copies, Nodes}])),
            ?match(ok, mnesia:sync_dirty(fun() ->
                [mnesia:write({disc, K, K}) || K <- lists:seq(1, N)], ok end)),
            ?match({atomic, ok}, mnesia:create_table(disc_only, [{disc_only_copies, Nodes}])),
            ?match(ok, mnesia:sync_dirty(fun() ->
                [mnesia:write({disc_only, K, K}) || K <- lists:seq(1, N)], ok end))
    end,

    ?match([], mnesia_test_lib:kill_mnesia([Node1])),

    OldCookie = erlang:get_cookie(),
    P2 = mnesia_test_lib:get_peer_ref(Node2),
    ?match(true, peer:call(P2, erlang, set_cookie, [invalid_cookie])),
    try
        ?match(true, peer:call(P2, net_kernel, disconnect, [Node1])),

        case mnesia_test_lib:diskless(Config) of
            true ->
                ?match(ok, mnesia:start([{extra_db_nodes, [Node2]}])),
                ?match({timeout, [ram]}, mnesia:wait_for_tables([ram], 5000));
            false ->
                ?match(ok, mnesia:start()),
                ?match({timeout, [ram, disc, disc_only]}, mnesia:wait_for_tables([ram, disc, disc_only], 5000))
        end,

        ?match(true, peer:call(P2, erlang, set_cookie, [OldCookie])),
        ?match(pong, peer:call(P2, net_adm, ping, [Node1])),

        ?match({ok, _}, mnesia_controller:connect_nodes(Nodes)),
        case mnesia_test_lib:diskless(Config) of
            true ->
                ?match({[ok, ok], []}, rpc:multicall(Nodes, mnesia, wait_for_tables, [[ram], 5000]));
            false ->
                ?match({[ok, ok], []}, rpc:multicall(Nodes, mnesia, wait_for_tables, [[ram, disc, disc_only], 5000]))
        end,

        RamPat = [{{ram, '_', '_'}, [], ['$_']}],
        ExpectedRam = sets:from_list([{ram, K, K} || K <- lists:seq(1, N)]),
        {[ActualRamN1, ActualRamN2], []} = rpc:multicall(Nodes, mnesia, dirty_select, [ram, RamPat]),
        ?match(ExpectedRam, sets:from_list(ActualRamN1)),
        ?match(ExpectedRam, sets:from_list(ActualRamN2)),

        case mnesia_test_lib:diskless(Config) of
            true ->
                ok;
            false ->
                DiscPat = [{{disc, '_', '_'}, [], ['$_']}],
                ExpectedDisc = sets:from_list([{disc, K, K} || K <- lists:seq(1, N)]),
                {[ActualDiscN1, ActualDiscN2], []} = rpc:multicall(Nodes, mnesia, dirty_select, [disc, DiscPat]),
                ?match(ExpectedDisc, sets:from_list(ActualDiscN1)),
                ?match(ExpectedDisc, sets:from_list(ActualDiscN2)),

                DiscOnlyPat = [{{disc_only, '_', '_'}, [], ['$_']}],
                ExpectedDiscOnly = sets:from_list([{disc_only, K, K} || K <- lists:seq(1, N)]),
                {[ActualDiscOnlyN1, ActualDiscOnlyN2], []} = rpc:multicall(Nodes, mnesia, dirty_select, [disc_only, DiscOnlyPat]),
                ?match(ExpectedDiscOnly, sets:from_list(ActualDiscOnlyN1)),
                ?match(ExpectedDiscOnly, sets:from_list(ActualDiscOnlyN2))
        end
    after
        ?match(true, peer:call(P2, erlang, set_cookie, [OldCookie]))
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

consistency_after_isolated_restart_3_nodes(suite) -> [];
consistency_after_isolated_restart_3_nodes(Config) when is_list(Config) ->
    [Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config),

    TableNodes = [Node2, Node3],
    N = 10,

    ?match({atomic, ok}, mnesia:create_table(ram, [{ram_copies, TableNodes}])),
    ?match(ok, mnesia:sync_dirty(fun() ->
        [mnesia:write({ram, K, K}) || K <- lists:seq(1, N)], ok end)),

    case mnesia_test_lib:diskless(Config) of
        true ->
            ok;
        false ->
            ?match({atomic, ok}, mnesia:create_table(disc, [{disc_copies, TableNodes}])),
            ?match(ok, mnesia:sync_dirty(fun() ->
                [mnesia:write({disc, K, K}) || K <- lists:seq(1, N)], ok end)),
            ?match({atomic, ok}, mnesia:create_table(disc_only, [{disc_only_copies, TableNodes}])),
            ?match(ok, mnesia:sync_dirty(fun() ->
                [mnesia:write({disc_only, K, K}) || K <- lists:seq(1, N)], ok end))
    end,

    ?match([], mnesia_test_lib:kill_mnesia([Node2])),

    OldCookie = erlang:get_cookie(),
    P2 = mnesia_test_lib:get_peer_ref(Node2),
    P3 = mnesia_test_lib:get_peer_ref(Node3),
    ?match(true, peer:call(P2, erlang, set_cookie, [invalid_cookie1])),
    ?match(true, peer:call(P3, erlang, set_cookie, [invalid_cookie2])),
    try
        ?match(true, peer:call(P3, net_kernel, disconnect, [Node2])),
        %% Global could already have disconnected, ignore return value
        peer:call(P3, net_kernel, disconnect, [Node1]),

        ?match(true, peer:call(P2, erlang, set_cookie, [OldCookie])),
        ?match(pong, net_adm:ping(Node2)),
        case mnesia_test_lib:diskless(Config) of
            true ->
                ?match(ok, rpc:call(Node2, mnesia, start, [[{extra_db_nodes, [Node1, Node3]}]])),
                ?match({timeout, [ram]}, rpc:call(Node2, mnesia, wait_for_tables, [[ram], 5000]));
            false ->
                ?match(ok, rpc:call(Node2, mnesia, start, [])),
                ?match({timeout, [ram, disc, disc_only]}, rpc:call(Node2, mnesia, wait_for_tables, [[ram, disc, disc_only], 5000]))
        end,

        ?match(true, peer:call(P3, erlang, set_cookie, [OldCookie])),
        ?match(pong, peer:call(P3, net_adm, ping, [Node1])),
        ?match(pong, peer:call(P3, net_adm, ping, [Node2])),

        ?match({ok, _}, mnesia_controller:connect_nodes(Nodes)),
        case mnesia_test_lib:diskless(Config) of
            true ->
                ?match({[ok, ok], []}, rpc:multicall(TableNodes, mnesia, wait_for_tables, [[ram], 5000]));
            false ->
                ?match({[ok, ok], []}, rpc:multicall(TableNodes, mnesia, wait_for_tables, [[ram, disc, disc_only], 5000]))
        end,

        RamPat = [{{ram, '_', '_'}, [], ['$_']}],
        ExpectedRam = sets:from_list([{ram, K, K} || K <- lists:seq(1, N)]),
        {[ActualRamN2, ActualRamN3], []} = rpc:multicall(TableNodes, mnesia, dirty_select, [ram, RamPat]),
        ?match(ExpectedRam, sets:from_list(ActualRamN2)),
        ?match(ExpectedRam, sets:from_list(ActualRamN3)),

        case mnesia_test_lib:diskless(Config) of
            true ->
                ok;
            false ->
                DiscPat = [{{disc, '_', '_'}, [], ['$_']}],
                ExpectedDisc = sets:from_list([{disc, K, K} || K <- lists:seq(1, N)]),
                {[ActualDiscN2, ActualDiscN3], []} = rpc:multicall(TableNodes, mnesia, dirty_select, [disc, DiscPat]),
                ?match(ExpectedDisc, sets:from_list(ActualDiscN2)),
                ?match(ExpectedDisc, sets:from_list(ActualDiscN3)),

                DiscOnlyPat = [{{disc_only, '_', '_'}, [], ['$_']}],
                ExpectedDiscOnly = sets:from_list([{disc_only, K, K} || K <- lists:seq(1, N)]),
                {[ActualDiscOnlyN2, ActualDiscOnlyN3], []} = rpc:multicall(TableNodes, mnesia, dirty_select, [disc_only, DiscOnlyPat]),
                ?match(ExpectedDiscOnly, sets:from_list(ActualDiscOnlyN2)),
                ?match(ExpectedDiscOnly, sets:from_list(ActualDiscOnlyN3))
        end
    after
        ?match(true, peer:call(P2, erlang, set_cookie, [OldCookie])),
        ?match(true, peer:call(P3, erlang, set_cookie, [OldCookie]))
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

consistency_after_isolated_restart_local_master_3_nodes(suite) -> [];
consistency_after_isolated_restart_local_master_3_nodes(Config) when is_list(Config) ->
    run_restart_case(Config, fun(A, B, C) ->
        %% GIVEN A < B, two replicas of each storage type, and coordinator C
        %% without table copies. A trusts only itself; B has no masters.
        Tabs = create_tables([A, B]),
        mnesia_node_set_masters(A, Tabs, [A]),
        mnesia_node_assert_master_policies(Tabs, [{A, [A]}, {B, []}, {C, []}]),

        %% WHEN A's Mnesia is abruptly killed, then restarted in {A,C} | {B}.
        %% Keep crash recovery distinct from the graceful-stop master cases.
        mnesia_node_kill(A, [B, C]),
        mnesia_node_set_partitions([[A, C], [B]]),
        mnesia_node_start(A),
        mnesia_node_assert_master_policies(Tabs, [{A, [A]}, {B, []}, {C, []}]),
        ?assertEqual(false, mnesia_node_call(A, fun() ->
            mnesia_recover:has_mnesia_down(B)
        end)),

        %% THEN A loads locally despite the potentially better live B.
        %% Without a RAM dump, A's RAM is empty; its disk data survives.
        mnesia_node_assert_loaded_from(A, Tabs, A, fun restart_records/1),
        mnesia_node_assert_load_reason(A, Tabs, local_master),
        mnesia_node_assert_locally_readable(B, Tabs),
        mnesia_node_assert_local_records(B, Tabs, fun expected_records/1),

        %% WHEN the partition heals, THEN already-loaded copies are retained:
        %% A still has empty RAM, while B still has the original RAM records.
        mnesia_node_set_connected_groups([[A, B, C]]),
        mnesia_node_connect([A, B, C]),
        mnesia_node_assert_loaded_from(A, Tabs, A, fun restart_records/1),
        mnesia_node_assert_locally_readable(B, Tabs),
        mnesia_node_assert_local_records(B, Tabs, fun expected_records/1),

        %% Cleanup: clearing RAM replicates the empty state. This does not
        %% assert automatic convergence with the live replica on reconnection.
        ?assertEqual({atomic, ok}, mnesia_node_call(A, fun() ->
            mnesia:clear_table(ram)
        end)),
        [mnesia_node_assert_local_records(N, Tabs, fun restart_records/1)
         || N <- [A, B]]
    end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

consistency_after_isolated_restart_remote_master_3_nodes(suite) -> [];
consistency_after_isolated_restart_remote_master_3_nodes(Config) when is_list(Config) ->
    run_restart_case(Config, fun(A, B, C) ->
        %% GIVEN A < B, two replicas of each storage type, and coordinator C
        %% without table copies. Only A names B as its remote master.
        Tabs = create_tables([A, B]),
        mnesia_node_set_masters(A, Tabs, [B]),
        mnesia_node_assert_master_policies(Tabs, [{A, [B]}, {B, []}, {C, []}]),

        %% WHEN B's Mnesia is abruptly killed before A's, then both restart
        %% in {A,C} | {B}. B cannot have recorded A as down before its crash.
        mnesia_node_kill(B, [A, C]),
        mnesia_node_kill(A, [C]),
        mnesia_node_set_partitions([[A, C], [B]]),
        mnesia_node_start(A),
        mnesia_node_start(B),
        mnesia_node_assert_master_policies(Tabs, [{A, [B]}, {B, []}, {C, []}]),
        ?assertEqual(false, mnesia_node_call(B, fun() ->
            mnesia_recover:has_mnesia_down(A)
        end)),

        %% THEN B waits for A's potentially better copy, while A waits for B
        %% under its master policy. Neither replica has loaded locally.
        [mnesia_node_assert_waiting(N, Tabs) || N <- [A, B]],

        %% WHEN the partition heals, THEN A delegates orphan loading to B.
        %% B bootstraps locally and supplies A, including the RAM-only table.
        mnesia_node_set_connected_groups([[A, B, C]]),
        mnesia_node_connect([A, B, C]),
        mnesia_node_assert_loaded_from(B, Tabs, B, fun restart_records/1),
        mnesia_node_assert_load_reason(B, Tabs, {adopt_orphan, A}),
        mnesia_node_assert_loaded_from(A, Tabs, B, fun restart_records/1),
        mnesia_node_assert_master_policies(Tabs, [{A, [B]}, {B, []}, {C, []}])
    end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

master_nodes_ignores_active_non_master_3_nodes(suite) -> [];
master_nodes_ignores_active_non_master_3_nodes(Config) when is_list(Config) ->
    run_active_source_case(Config, chain).

master_nodes_multiple_remote_masters_3_nodes(suite) -> [];
master_nodes_multiple_remote_masters_3_nodes(Config) when is_list(Config) ->
    run_active_source_case(Config, multiple).

master_nodes_clear_while_waiting_3_nodes(suite) -> [];
master_nodes_clear_while_waiting_3_nodes(Config) when is_list(Config) ->
    run_active_source_case(Config, clear).

master_nodes_expand_while_waiting_3_nodes(suite) -> [];
master_nodes_expand_while_waiting_3_nodes(Config) when is_list(Config) ->
    run_active_source_case(Config, expand).

master_nodes_mutual_remote_masters_3_nodes(suite) -> [];
master_nodes_mutual_remote_masters_3_nodes(Config) when is_list(Config) ->
    run_restart_case(Config, fun(A, B, C) ->
        %% GIVEN A < B, two replicas that each trust only the other, and a
        %% coordinator without table copies. Neither may bootstrap locally.
        Tabs = create_tables([A, B]),
        mnesia_node_set_masters(A, Tabs, [B]),
        mnesia_node_set_masters(B, Tabs, [A]),
        mnesia_node_stop(B, [A, C]),
        mnesia_node_stop(A, [C]),
        mnesia_node_set_partitions([[A, C], [B]]),
        mnesia_node_start(A),
        mnesia_node_start(B),
        mnesia_node_assert_master_policies(Tabs, [{A, [B]}, {B, [A]}, {C, []}]),
        [mnesia_node_assert_waiting(N, Tabs) || N <- [A, B]],
        mnesia_node_with_observer(A, [{mnesia_late_loader, maybe_async_late_disc_load, 3}],
                                  false, fun(TraceA) ->
            mnesia_node_with_observer(B, [{mnesia_controller, schedule_late_disc_load, 2}],
                                      false, fun(TraceB) ->
                %% WHEN all nodes reconnect, A asks B to adopt the orphans.
                mnesia_node_set_connected_groups([[A, B, C]]),
                mnesia_node_connect([A, B, C]),
                mnesia_node_assert_running([A, B, C]),
                mnesia_node_assert_eventually(fun() ->
                    Requests = mnesia_node_get_orphan_load_requests(A, TraceA, A),
                    lists:all(fun(Tab) -> lists:member({B, Tab}, Requests) end,
                              Tabs)
                end, {orphan_requests, A, B}),
                %% THEN B processes the request but schedules no tables:
                %% its own master list excludes itself.
                mnesia_node_assert_eventually(fun() ->
                    lists:any(fun
                        ({trace, _, call,
                          {mnesia_controller, schedule_late_disc_load,
                           [[], {adopt_orphan, Origin}]}}) -> Origin == A;
                        (_) -> false
                    end, mnesia_node_get_observed_events(B, TraceB))
                end, {rejected_orphans, B}),
                [mnesia_node_assert_waiting(N, Tabs) || N <- [A, B]]
            end)
        end),
        %% Cleanup: WHEN B is explicitly made an authority and restarted,
        %% THEN it bootstraps locally and supplies the waiting A.
        mnesia_node_stop(B, [A, C]),
        mnesia_node_set_masters(B, Tabs, [B]),
        mnesia_node_start(B),
        mnesia_node_assert_loaded_from(B, Tabs, B, fun restart_records/1),
        mnesia_node_assert_load_reason(B, Tabs, local_master),
        mnesia_node_assert_loaded_from(A, Tabs, B, fun restart_records/1)
    end).

master_nodes_remote_master_loads_locally_3_nodes(suite) -> [];
master_nodes_remote_master_loads_locally_3_nodes(Config) when is_list(Config) ->
    run_restart_case(Config, fun(A, B, C) ->
        %% GIVEN A trusts B and B trusts itself, with two replicas and a
        %% neutral coordinator. The policy is local to each node.
        Tabs = create_tables([A, B]),
        mnesia_node_set_masters(A, Tabs, [B]),
        mnesia_node_set_masters(B, Tabs, [B]),
        mnesia_node_stop(B, [A, C]),
        mnesia_node_stop(A, [C]),
        mnesia_node_set_partitions([[A, C], [B]]),
        %% WHEN both replicas restart on opposite sides of the partition.
        mnesia_node_start(A),
        mnesia_node_start(B),
        mnesia_node_assert_master_policies(Tabs, [{A, [B]}, {B, [B]}, {C, []}]),
        %% THEN B loads locally while A waits for its remote master.
        mnesia_node_assert_waiting(A, Tabs),
        mnesia_node_assert_loaded_from(B, Tabs, B, fun restart_records/1),
        mnesia_node_assert_load_reason(B, Tabs, local_master),
        %% WHEN the partition heals, THEN A loads from the already-active B.
        mnesia_node_set_connected_groups([[A, B, C]]),
        mnesia_node_connect([A, B, C]),
        mnesia_node_assert_loaded_from(A, Tabs, B, fun restart_records/1),
        mnesia_node_assert_local_records(B, Tabs, fun restart_records/1)
    end).

master_nodes_mixed_storage_orphan_master_3_nodes(suite) -> [];
master_nodes_mixed_storage_orphan_master_3_nodes(Config) when is_list(Config) ->
    run_restart_case(Config, fun(A, B, C) ->
        %% GIVEN A < B with disk copies on A/B and RAM on C. Both disk
        %% storage variants exercise exclusion of the RAM master from adoption.
        Tabs = create_tables(
                 [{mixed_disc, [{disc_copies, [A, B]}, {ram_copies, [C]}]},
                  {mixed_disc_only,
                   [{disc_only_copies, [A, B]}, {ram_copies, [C]}]}], [A, B, C]),
        mnesia_node_set_masters(A, Tabs, [B, C]),
        mnesia_node_stop(B, [A, C]),
        mnesia_node_stop(C, [A]),
        mnesia_node_stop(A, []),
        mnesia_node_set_partitions([[A], [B], [C]]),
        [mnesia_node_start(N) || N <- [A, B, C]],
        mnesia_node_assert_master_policies(Tabs, [{A, [B, C]}, {B, []}, {C, []}]),
        [mnesia_node_assert_waiting(N, Tabs) || N <- [A, B, C]],
        ?assertEqual(false, mnesia_node_call(B, fun() ->
            mnesia_recover:has_mnesia_down(A)
        end)),
        ?assertEqual(false, mnesia_node_call(C, fun() ->
            mnesia_recover:has_mnesia_down(A)
        end)),
        mnesia_node_with_observer(A, [{mnesia_late_loader, maybe_async_late_disc_load, 3}],
                                  false, fun(Trace) ->
            %% WHEN the partition heals, observe the entire adoption pass.
            mnesia_node_set_connected_groups([[A, B, C]]),
            mnesia_node_connect([A, B, C]),
            mnesia_node_assert_running([A, B, C]),
            mnesia_node_assert_loaded_from(B, Tabs, B, fun expected_records/1),
            mnesia_node_assert_load_reason(B, Tabs, {adopt_orphan, A}),
            mnesia_node_assert_loaded_from(A, Tabs, B, fun expected_records/1),
            mnesia_node_assert_locally_readable(C, Tabs),
            [begin
                 Source = mnesia_node_call(C, fun() -> mnesia:table_info(Tab, load_node) end),
                 ?assert(lists:member(Source, [A, B]))
             end || Tab <- Tabs],
            mnesia_node_assert_local_records(C, Tabs, fun expected_records/1),
            %% THEN B received every orphan, but C received none. A controller
            %% barrier plus trace delivery covers all targets, not just the
            %% first request that happened to produce a successful load.
            mnesia_node_call(A, fun() ->
                %% SYNC Barrier.
                _ = sys:get_status(mnesia_controller),
                Ref = erlang:trace_delivered(whereis(mnesia_controller)),
                receive {trace_delivered, _, Ref} -> ok after 5000 -> error(trace_timeout) end
            end),
            Requests = mnesia_node_get_orphan_load_requests(A, Trace, A),
            ?assertEqual(lists:sort([{B, Tab} || Tab <- Tabs]),
                         lists:usort(Requests))
        end)
    end).

master_nodes_local_member_precedence_3_nodes(suite) -> [];
master_nodes_local_member_precedence_3_nodes(Config) when is_list(Config) ->
    run_restart_case(Config, fun(A, B, C) ->
        %% GIVEN A trusts [B,A], not just [A], and stops while B is still live.
        Tabs = create_tables([A, B]),
        mnesia_node_set_masters(A, Tabs, [B, A]),
        mnesia_node_stop(A, [B, C]),
        mnesia_node_set_partitions([[A, C], [B]]),
        %% WHEN A restarts without access to the potentially better B.
        mnesia_node_start(A),
        ?assertEqual(false, mnesia_node_call(A, fun() ->
            mnesia_recover:has_mnesia_down(B)
        end)),
        mnesia_node_assert_master_policies(Tabs, [{A, [B, A]}, {B, []}, {C, []}]),
        %% THEN local membership wins regardless of list order. RAM is empty
        %% on A while the still-live B retains old data; disk data remains intact.
        mnesia_node_assert_loaded_from(A, Tabs, A, fun restart_records/1),
        mnesia_node_assert_load_reason(A, Tabs, local_master),
        mnesia_node_assert_local_records(B, Tabs, fun expected_records/1),
        %% Cleanup explicitly selects A; reconnection alone is not a merge.
        recover_follower(A, B, C, Tabs, fun restart_records/1)
    end).

master_nodes_two_local_masters_partitioned_3_nodes(suite) -> [];
master_nodes_two_local_masters_partitioned_3_nodes(Config) when is_list(Config) ->
    run_restart_case(Config, fun(A, B, C) ->
        %% GIVEN self masters and no schema masters. Partition while both
        %% replicas run, so both durably record the other as down.
        Tabs = create_tables([A, B]),
        mnesia_node_set_masters(A, Tabs, [A]),
        mnesia_node_set_masters(B, Tabs, [B]),
        mnesia_node_set_partitions([[A, C], [B]]),
        mnesia_node_assert_down_logged(A, B),
        mnesia_node_assert_down_logged(B, A),
        mnesia_node_stop(B, []),
        mnesia_node_stop(A, [C]),
        %% WHEN both replicas restart in isolation and receive different writes.
        mnesia_node_start(A),
        mnesia_node_start(B),
        mnesia_node_assert_master_policies(Tabs, [{A, [A]}, {B, [B]}, {C, []}]),
        [begin
             mnesia_node_assert_loaded_from(N, Tabs, N, fun restart_records/1),
             mnesia_node_assert_load_reason(N, Tabs, local_master)
         end || N <- [A, B]],
        mnesia_node_assert_down_logged(A, B),
        mnesia_node_assert_down_logged(B, A),
        write_marker(A, Tabs, side_a),
        write_marker(B, Tabs, side_b),
        RecordsA = fun(Tab) -> restart_records(Tab) ++ [{Tab, marker, side_a}] end,
        RecordsB = fun(Tab) -> restart_records(Tab) ++ [{Tab, marker, side_b}] end,
        mnesia_node_assert_local_records(A, Tabs, RecordsA),
        mnesia_node_assert_local_records(B, Tabs, RecordsB),
        mnesia_node_with_observer(A, [], true, fun(EventsA) ->
            mnesia_node_with_observer(B, [], true, fun(EventsB) ->
                %% WHEN connectivity returns with subscriptions already active.
                mnesia_node_set_connected_groups([[A, B, C]]),
                %% THEN report the inconsistent pair (either event context or
                %% direction), and keep both already-loaded contents unchanged.
                mnesia_node_assert_eventually(fun() ->
                    mnesia_node_has_inconsistency_event(A, EventsA, B) orelse
                        mnesia_node_has_inconsistency_event(B, EventsB, A)
                end, {inconsistent_database, A, B}),
                mnesia_node_connect([A, B, C]),
                mnesia_node_assert_local_records(A, Tabs, RecordsA),
                mnesia_node_assert_local_records(B, Tabs, RecordsB)
            end)
        end),
        %% Cleanup: THEN restarting B with A as sole authority replaces RB by RA.
        recover_follower(A, B, C, Tabs, RecordsA)
    end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

consistency_after_dump_tables_1_ram(suite) -> [];
consistency_after_dump_tables_1_ram(Config) when is_list(Config) ->
     consistency_after_dump_tables(ram_copies, 1, Config).

consistency_after_dump_tables_2_ram(suite) -> [];
consistency_after_dump_tables_2_ram(Config) when is_list(Config) ->
    consistency_after_dump_tables(ram_copies, 2, Config).

consistency_after_dump_tables(ReplicaType, NodeConfig, Config) ->
    [Node1 | _] = Nodes = ?acquire_nodes(NodeConfig, Config),
    {success, [A]} = ?start_activities([Node1]),
    ?log("consistency_after_dump_tables with ~p on ~p~n",
         [ReplicaType, Nodes]),
    TpcbConfig = tpcb_config(ReplicaType, NodeConfig, Nodes, []),
    mnesia_tpcb:init(TpcbConfig),
    A ! fun() -> mnesia_tpcb:run(TpcbConfig) end,
    timer:sleep(timer:seconds(3)),
    ?match({atomic, ok}, rpc:call(Node1, mnesia, dump_tables,
                        [[branch, teller, account, history]])),
    mnesia_tpcb:stop(),
    ?match(ok, mnesia_tpcb:verify_tabs()),

    mnesia_test_lib:kill_mnesia(Nodes),
    timer:sleep(timer:seconds(1)),
    ?match([], mnesia_test_lib:start_mnesia(Nodes,[account, branch,
						   teller, history])),
    mnesia_tpcb:stop(),
    ?match(ok, mnesia_tpcb:verify_tabs()),
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

consistency_after_add_replica_2_ram(suite) -> [];
consistency_after_add_replica_2_ram(Config) when is_list(Config) ->
    consistency_after_add_replica(ram_copies, 2, Config).

consistency_after_add_replica_2_disc(suite) -> [];
consistency_after_add_replica_2_disc(Config) when is_list(Config) ->
    consistency_after_add_replica(disc_copies, 2, Config).

consistency_after_add_replica_2_disc_only(suite) -> [];
consistency_after_add_replica_2_disc_only(Config) when is_list(Config) ->
    consistency_after_add_replica(disc_only_copies, 2, Config).

consistency_after_add_replica_3_ram(suite) -> [];
consistency_after_add_replica_3_ram(Config) when is_list(Config) ->
    consistency_after_add_replica(ram_copies, 3, Config).

consistency_after_add_replica_3_disc(suite) -> [];
consistency_after_add_replica_3_disc(Config) when is_list(Config) ->
    consistency_after_add_replica(disc_copies, 3, Config).

consistency_after_add_replica_3_disc_only(suite) -> [];
consistency_after_add_replica_3_disc_only(Config) when is_list(Config) ->
    consistency_after_add_replica(disc_only_copies, 3, Config).

consistency_after_add_replica(ReplicaType, NodeConfig, Config) ->
    Nodes0 = ?acquire_nodes(NodeConfig, Config),
    AddNode = lists:last(Nodes0),
    Nodes = Nodes0 -- [AddNode],
    Node1 = hd(Nodes),
    {success, [A]} = ?start_activities([Node1]),
    ?log("consistency_after_add_replica with ~p on ~p~n",
         [ReplicaType, Nodes0]),
    TpcbConfig = tpcb_config(ReplicaType, NodeConfig, Nodes, []),
    mnesia_tpcb:init(TpcbConfig),
    A ! fun () -> mnesia_tpcb:run(TpcbConfig) end,
    timer:sleep(timer:seconds(2)),
    ?match({atomic, ok}, mnesia:add_table_copy(account, AddNode, ReplicaType)),
    mnesia_tpcb:stop(),
    ?match(ok, mnesia_tpcb:verify_tabs()),
    ?verify_mnesia(Nodes0, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

consistency_after_del_replica_2_ram(suite) -> [];
consistency_after_del_replica_2_ram(Config) when is_list(Config) ->
    consistency_after_del_replica(ram_copies, 2, Config).

consistency_after_del_replica_2_disc(suite) -> [];
consistency_after_del_replica_2_disc(Config) when is_list(Config) ->
    consistency_after_del_replica(disc_copies, 2, Config).

consistency_after_del_replica_2_disc_only(suite) -> [];
consistency_after_del_replica_2_disc_only(Config) when is_list(Config) ->
    consistency_after_del_replica(disc_only_copies, 2, Config).

consistency_after_del_replica_3_ram(suite) -> [];
consistency_after_del_replica_3_ram(Config) when is_list(Config) ->
    consistency_after_del_replica(ram_copies, 3, Config).

consistency_after_del_replica_3_disc(suite) -> [];
consistency_after_del_replica_3_disc(Config) when is_list(Config) ->
    consistency_after_del_replica(disc_copies, 3, Config).

consistency_after_del_replica_3_disc_only(suite) -> [];
consistency_after_del_replica_3_disc_only(Config) when is_list(Config) ->
    consistency_after_del_replica(disc_only_copies, 3, Config).

consistency_after_del_replica(ReplicaType, NodeConfig, Config) ->
    Nodes = ?acquire_nodes(NodeConfig, Config),
    Node1 = hd(Nodes),
    Node2 = lists:last(Nodes),
    {success, [A]} = ?start_activities([Node1]),
    ?log("consistency_after_del_replica with ~p on ~p~n",
         [ReplicaType, Nodes]),
    TpcbConfig = tpcb_config(ReplicaType, NodeConfig, Nodes, []),
    mnesia_tpcb:init(TpcbConfig),
    A ! fun () -> mnesia_tpcb:run(TpcbConfig) end,
    timer:sleep(timer:seconds(3)),
    ?match({atomic, ok}, mnesia:del_table_copy(account, Node2)),
    mnesia_tpcb:stop(),
    ?match(ok, mnesia_tpcb:verify_tabs()),
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

consistency_after_move_replica_2_ram(suite) -> [];
consistency_after_move_replica_2_ram(Config) when is_list(Config) ->
    consistency_after_move_replica(ram_copies, 2, Config).

consistency_after_move_replica_2_disc(suite) -> [];
consistency_after_move_replica_2_disc(Config) when is_list(Config) ->
    consistency_after_move_replica(disc_copies, 2, Config).

consistency_after_move_replica_2_disc_only(suite) -> [];
consistency_after_move_replica_2_disc_only(Config) when is_list(Config) ->
    consistency_after_move_replica(disc_only_copies, 2, Config).

consistency_after_move_replica_3_ram(suite) -> [];
consistency_after_move_replica_3_ram(Config) when is_list(Config) ->
    consistency_after_move_replica(ram_copies, 3, Config).

consistency_after_move_replica_3_disc(suite) -> [];
consistency_after_move_replica_3_disc(Config) when is_list(Config) ->
    consistency_after_move_replica(disc_copies, 3, Config).

consistency_after_move_replica_3_disc_only(suite) -> [];
consistency_after_move_replica_3_disc_only(Config) when is_list(Config) ->
    consistency_after_move_replica(disc_only_copies, 3, Config).

consistency_after_move_replica(ReplicaType, NodeConfig, Config) ->
    Nodes = ?acquire_nodes(NodeConfig, Config ++ [{tc_timeout, timer:minutes(10)}]),
    Node1 = hd(Nodes),
    Node2 = lists:last(Nodes),
    {success, [A]} = ?start_activities([Node1]),
    ?log("consistency_after_move_replica with ~p on ~p~n",
         [ReplicaType, Nodes]),
    TpcbConfig = tpcb_config(ReplicaType, NodeConfig, Nodes -- [Node2], []),
    mnesia_tpcb:init(TpcbConfig),
    A ! fun () -> mnesia_tpcb:run(TpcbConfig) end,
    timer:sleep(timer:seconds(3)),
    ?match({atomic, ok}, mnesia:move_table_copy(account, Node1, Node2)),    
    ?log("First move completed from node ~p to ~p ~n", [Node1, Node2]),
    ?match({atomic, ok}, mnesia:move_table_copy(account, Node2, Node1)),
    mnesia_tpcb:stop(),
    ?match(ok, mnesia_tpcb:verify_tabs()),
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


consistency_after_transform_table_ram(suite) -> [];
consistency_after_transform_table_ram(Config) when is_list(Config) ->
    consistency_after_transform_table(ram_copies, Config).

consistency_after_transform_table_disc(suite) -> [];
consistency_after_transform_table_disc(Config) when is_list(Config) ->
    consistency_after_transform_table(disc_copies, Config).

consistency_after_transform_table_disc_only(suite) -> [];
consistency_after_transform_table_disc_only(Config) when is_list(Config) ->
    consistency_after_transform_table(disc_only_copies, Config).

consistency_after_transform_table(Type, Config) ->
    Nodes = [N1, N2,_N3] = ?acquire_nodes(3, Config),

    ?match({atomic, ok}, mnesia:create_table(tab1, [{index, [3]}, {Type, [N1]}])),
    ?match({atomic, ok}, mnesia:create_table(tab2, [{index, [3]}, {Type, [N1,N2]}])),
    ?match({atomic, ok}, mnesia:create_table(tab3, [{index, [3]}, {Type, Nodes}])),
    ?match({atomic, ok}, mnesia:create_table(empty, [{index, [3]},{Type, Nodes}])),
    
    Tabs = lists:sort([tab1, tab2, tab3, empty]),
    
    [[mnesia:dirty_write({Tab, N, N}) || N <- lists:seq(1,10)] || 
	Tab <- Tabs -- [empty, tab4]],
    mnesia:dump_log(),
    
    Ok = lists:duplicate(4, {atomic, ok}),
    ?match(Ok, [mnesia:transform_table(Tab, fun({T, N, N}) ->  {T, N, N, ok} end,
	[k,a,n]) || Tab <- Tabs]),
    [?match([k,a,n], mnesia:table_info(Tab, attributes)) || Tab <- Tabs],

    Filter = fun(Tab) -> mnesia:foldl(fun(A, Acc) when tuple_size(A) == 3 -> [A|Acc];
					 (A, Acc) when tuple_size(A) == 4 -> Acc
				      end, [], Tab)
	     end,    
	
    ?match([[],[],[],[]], [element(2,mnesia:transaction(Filter, [Tab])) || Tab <- Tabs]),
    
    mnesia_test_lib:kill_mnesia(Nodes),
    mnesia_test_lib:start_mnesia(Nodes, Tabs),
    
    ?match([Tabs, Tabs, Tabs], 
	[lists:sort(rpc:call(Node, mnesia,system_info, [tables]) -- [schema]) || Node <- Nodes]),
    
    ?match([[],[],[],[]], [element(2,mnesia:transaction(Filter, [Tab])) || Tab <- Tabs]),
    [?match([k,a,n], mnesia:table_info(Tab, attributes)) || Tab <- Tabs],

    ?verify_mnesia(Nodes, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
consistency_after_change_table_copy_type(doc) ->
    ["Check that the database is consistent after change of copy type.",
     " While applications are updating the involved tables. "].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

consistency_after_fallback_2_ram(suite) -> [];
consistency_after_fallback_2_ram(Config) when is_list(Config) ->
    consistency_after_fallback(ram_copies, 2, Config).

consistency_after_fallback_2_disc(suite) -> [];
consistency_after_fallback_2_disc(Config) when is_list(Config) ->
    consistency_after_fallback(disc_copies, 2, Config).

consistency_after_fallback_2_disc_only(suite) -> [];
consistency_after_fallback_2_disc_only(Config) when is_list(Config) ->
    consistency_after_fallback(disc_only_copies, 2, Config).

consistency_after_fallback_3_ram(suite) -> [];
consistency_after_fallback_3_ram(Config) when is_list(Config) ->
    consistency_after_fallback(ram_copies, 3, Config).

consistency_after_fallback_3_disc(suite) -> [];
consistency_after_fallback_3_disc(Config) when is_list(Config) ->
    consistency_after_fallback(disc_copies, 3, Config).

consistency_after_fallback_3_disc_only(suite) -> [];
consistency_after_fallback_3_disc_only(Config) when is_list(Config) ->
    consistency_after_fallback(disc_only_copies, 3, Config).

consistency_after_fallback(ReplicaType, NodeConfig, Config) ->
    put(mnesia_test_verbose, true),
    %%?verbose("Starting consistency_after_fallback2 at ~p~n", [self()]),
    Delay = 3,
    Nodes = ?acquire_nodes(NodeConfig, [{tc_timeout, timer:minutes(10)} | Config]),
    Node1 = hd(Nodes),
    %%?verbose("Mnesia info: ~p~n", [mnesia:info()]),

    {success, [A]} = ?start_activities([Node1]),
    ?log("consistency_after_fallback with ~p on ~p~n",
         [ReplicaType, Nodes]),
    TpcbConfig = tpcb_config(ReplicaType, NodeConfig, Nodes, []),
    mnesia_tpcb:init(TpcbConfig),
    A ! fun () -> mnesia_tpcb:run(TpcbConfig) end,
    timer:sleep(timer:seconds(Delay)),
    
    %% Make a backup
    ?verbose("Doing backup~n", []),
    ?match(ok, mnesia:backup(consistency_after_fallback2)),
    
    %% Install the backup as a fallback
    ?verbose("Doing fallback~n", []),
    ?match(ok, mnesia:install_fallback(consistency_after_fallback2)),
    timer:sleep(timer:seconds(Delay)),

    %% Stop tpcb
    ?verbose("Stopping TPC-B~n", []),
    mnesia_tpcb:stop(),    
    ?match(ok, mnesia_tpcb:verify_tabs()),
    
    %% Stop and then start mnesia and check table consistency
    ?verbose("Kill Mnesia~n", []),
    mnesia_test_lib:kill_mnesia(Nodes),
    ?verbose("Start Mnesia~n", []),
    mnesia_test_lib:start_mnesia(Nodes,[account,branch,teller,history]),
    ?verbose("Verify tabs~n", []),
    ?match(ok, mnesia_tpcb:verify_tabs()),
    if 
	ReplicaType == ram_copies ->
	    %% Test that change_table_copy work i.e. no account.dcd file exists.
	    ?match({atomic, ok}, mnesia:change_table_copy_type(account, node(), disc_copies));
	true -> 
	    ignore
    end,
    file:delete(consistency_after_fallback2),
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

consistency_after_restore_clear_ram(suite) -> [];
consistency_after_restore_clear_ram(Config) when is_list(Config) ->
    consistency_after_restore(ram_copies, clear_tables, Config).

consistency_after_restore_clear_disc(suite) -> [];
consistency_after_restore_clear_disc(Config) when is_list(Config) ->
    consistency_after_restore(disc_copies, clear_tables, Config).

consistency_after_restore_clear_disc_only(suite) -> [];
consistency_after_restore_clear_disc_only(Config) when is_list(Config) ->
    consistency_after_restore(disc_only_copies, clear_tables, Config).

consistency_after_restore_recreate_ram(suite) -> [];
consistency_after_restore_recreate_ram(Config) when is_list(Config) ->
    consistency_after_restore(ram_copies, recreate_tables, Config).

consistency_after_restore_recreate_disc(suite) -> [];
consistency_after_restore_recreate_disc(Config) when is_list(Config) ->
    consistency_after_restore(disc_copies, recreate_tables, Config).

consistency_after_restore_recreate_disc_only(suite) -> [];
consistency_after_restore_recreate_disc_only(Config) when is_list(Config) ->
    consistency_after_restore(disc_only_copies, recreate_tables, Config).

consistency_after_restore(ReplicaType, Op, Config) ->
    Delay = 1,
    Nodes = ?acquire_nodes(3, [{tc_timeout, timer:minutes(10)} | Config]),
    [Node1, Node2, _Node3] = Nodes,
    File = "cons_backup_restore",
    
    ?log("consistency_after_restore with ~p on ~p~n",
         [ReplicaType, Nodes]),
    Tabs = [carA, carB, carC, carD],
    
    ?match({atomic, ok}, mnesia:create_table(carA, [{ReplicaType, Nodes}])),
    ?match({atomic, ok}, mnesia:create_table(carB, [{ReplicaType, Nodes -- [Node1]}])),
    ?match({atomic, ok}, mnesia:create_table(carC, [{ReplicaType, Nodes -- [Node2]}])),
    ?match({atomic, ok}, mnesia:create_table(carD, [{ReplicaType, [Node2]}])),

    NList = lists:seq(0, 20),
    [lists:foreach(fun(E) -> ok = mnesia:dirty_write({Tab, E, 1}) end, NList) ||
	Tab <- Tabs],
    
    {ok, Name, _} = mnesia:activate_checkpoint([{max, [schema | Tabs]}, 
						{ram_overrides_dump, true}]),
    ?verbose("Doing backup~n", []),
    ?match(ok, mnesia:backup_checkpoint(Name, File)),
    ?match(ok, mnesia:deactivate_checkpoint(Name)),
    
    [lists:foreach(fun(E) -> ok = mnesia:dirty_write({Tab, E, 2}) end, NList) ||
	Tab <- Tabs],
    
    Pids1 = [{'EXIT', spawn_link(?MODULE, change_tab, [self(), carA, Op]), carA} || _ <- lists:seq(1, 5)],
    Pids2 = [{'EXIT', spawn_link(?MODULE, change_tab, [self(), carB, Op]), carB} || _ <- lists:seq(1, 5)],
    Pids3 = [{'EXIT', spawn_link(?MODULE, change_tab, [self(), carC, Op]), carC} || _ <- lists:seq(1, 5)],
    Pids4 = [{'EXIT', spawn_link(?MODULE, change_tab, [self(), carD, Op]), carD} || _ <- lists:seq(1, 5)],
    
    AllPids = Pids1 ++ Pids2 ++ Pids3 ++ Pids4,
    
    Restore = fun(F, Args) ->
		      case mnesia:restore(F, Args) of
			  {atomic, List} -> lists:sort(List);
			  Else -> Else
		      end
	      end,

    timer:sleep(timer:seconds(Delay)),  %% Let changers grab locks
    ?verbose("Doing restore~n", []),
    ?match(Tabs, Restore(File, [{default_op, Op}])),

    Collect = fun(Msg, Acc) ->
		      receive Msg -> Acc
		      after 10000 -> [Msg|Acc]
		      end
	      end,

    Failed1 = lists:foldl(Collect, [], AllPids),
    Failed  = lists:foldl(Collect, [], Failed1),

    case Failed of
	[] -> ok;
	_  ->
	    ?match([], Failed),
	    io:format("TIME: ~p sec~n", [erlang:system_time(seconds) band 16#FF]),
	    Dbg = fun({_, Pid, Tab}) ->
			  io:format("Tab ~p: ~p~n",[Tab, process_info(Pid, current_stacktrace)]),
			  [io:format(" ~p~n", [Rec]) || Rec <- mnesia:dirty_match_object({Tab, '_', '_'})]
		  end,
	    [Dbg(Msg) || Msg <- Failed],
	    io:format(" Held: ~p~n", [mnesia_locker:get_held_locks()]),
	    io:format("Queue: ~p~n", [mnesia_locker:get_lock_queue()])
    end,

    case ?match(ok, restore_verify_tabs(Tabs)) of
	{success, ok} ->
	    file:delete(File);
	_ ->
	    {T, M, S} = time(),
	    File2 = ?flat_format("consistency_error~w~w~w.BUP", [T, M, S]),
	    file:rename(File, File2)
    end,
    ?verify_mnesia(Nodes, []).

change_tab(Father, Tab, Test) ->
    Key = rand:uniform(20),
    Update = fun() ->
		     Time = erlang:system_time(seconds) band 16#FF,
		     case put(time, Time) of
			 Time -> ok;
			 _ -> io:format("~p ~p ~p sec~n", [self(), Tab, Time])
		     end,
		     case mnesia:read({Tab, Key}) of
			 [{Tab, Key, 1}] ->  quit;
			 [{Tab, Key, _N}] -> mnesia:write({Tab, Key, 3})
		     end
	     end,
    case mnesia:transaction(Update) of
	{atomic, quit} ->
	    exit(Tab);
	{aborted, {no_exists, Tab}} when Test == recreate_tables -> %% I'll allow this
	    change_tab(Father, Tab, Test);
	{atomic, ok} ->
	    change_tab(Father, Tab, Test)
    end.

restore_verify_tabs([Tab | R]) ->
    ?match({atomic, ok}, 
	   mnesia:transaction(fun() -> mnesia:foldl(fun({_, _, 1}, ok) -> 
							    ok;
						       (Else, Acc) ->
							    [Else|Acc]
						    end, ok, Tab) 
			      end)),
    restore_verify_tabs(R);
restore_verify_tabs([]) ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

updates_during_checkpoint_activation_1_ram(suite) -> [];
updates_during_checkpoint_activation_1_ram(Config) when is_list(Config) ->
    updates_during_checkpoint_activation(ram_copies, 1, Config).

updates_during_checkpoint_activation_1_disc(suite) -> [];
updates_during_checkpoint_activation_1_disc(Config) when is_list(Config) ->
    updates_during_checkpoint_activation(disc_copies, 1, Config).

updates_during_checkpoint_activation_1_disc_only(suite) -> [];
updates_during_checkpoint_activation_1_disc_only(Config) when is_list(Config) ->
    updates_during_checkpoint_activation(disc_only_copies, 1, Config).

updates_during_checkpoint_activation_2_ram(suite) -> [];
updates_during_checkpoint_activation_2_ram(Config) when is_list(Config) ->
    updates_during_checkpoint_activation(ram_copies, 2, Config).

updates_during_checkpoint_activation_2_disc(suite) -> [];
updates_during_checkpoint_activation_2_disc(Config) when is_list(Config) ->
    updates_during_checkpoint_activation(disc_copies, 2, Config).

updates_during_checkpoint_activation_2_disc_only(suite) -> [];
updates_during_checkpoint_activation_2_disc_only(Config) when is_list(Config) ->
    updates_during_checkpoint_activation(disc_only_copies, 2, Config).

updates_during_checkpoint_activation_3_ram(suite) -> [];
updates_during_checkpoint_activation_3_ram(Config) when is_list(Config) ->
    updates_during_checkpoint_activation(ram_copies, 3, Config).

updates_during_checkpoint_activation_3_disc(suite) -> [];
updates_during_checkpoint_activation_3_disc(Config) when is_list(Config) ->
    updates_during_checkpoint_activation(disc_copies, 3, Config).

updates_during_checkpoint_activation_3_disc_only(suite) -> [];
updates_during_checkpoint_activation_3_disc_only(Config) when is_list(Config) ->
    updates_during_checkpoint_activation(disc_only_copies, 3, Config).

updates_during_checkpoint_activation(ReplicaType,NodeConfig,Config) ->
    %%?verbose("updates_during_checkpoint_activation2 at ~p~n", [self()]),
    Delay = 2,
    Nodes = ?acquire_nodes(NodeConfig, Config),
    Node1 = hd(Nodes),
    %%?verbose("Mnesia info: ~p~n", [mnesia:info()]),

    {success, [A]} = ?start_activities([Node1]),
    ?log("consistency_after_fallback with ~p on ~p~n",
         [ReplicaType, Nodes]),
    TpcbConfig = tpcb_config_dist(ReplicaType, NodeConfig, Nodes, Config),
    %%TpcbConfig = tpcb_config(ReplicaType, NodeConfig, Nodes),
    mnesia_tpcb:init(TpcbConfig),
    A ! fun () -> mnesia_tpcb:run(TpcbConfig) end,
    timer:sleep(timer:seconds(Delay)),

    {ok, CPName, _NodeList} =
        mnesia:activate_checkpoint([{max, mnesia:system_info(tables)},
				    {ram_overrides_dump, true}]),
    timer:sleep(timer:seconds(Delay)),

    %% Stop tpcb
    ?verbose("Stopping TPC-B~n", []),
    mnesia_tpcb:stop(),
    ?match(ok, mnesia_tpcb:verify_tabs()),
    
    ?match(ok, mnesia:backup_checkpoint(CPName, 
					updates_during_checkpoint_activation2)),
    timer:sleep(timer:seconds(Delay)),

    ?match(ok, mnesia:install_fallback(updates_during_checkpoint_activation2)),

    %% Stop and then start mnesia and check table consistency
    %%?verbose("Restarting Mnesia~n", []),
    mnesia_test_lib:kill_mnesia(Nodes),
    file:delete(updates_during_checkpoint_activation2),
    mnesia_test_lib:start_mnesia(Nodes,[account,branch,teller, history]),

    ?match(ok, mnesia_tpcb:verify_tabs()),
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

updates_during_checkpoint_iteration_2_ram(suite) -> [];
updates_during_checkpoint_iteration_2_ram(Config) when is_list(Config) ->
    updates_during_checkpoint_iteration(ram_copies, 2, Config).

updates_during_checkpoint_iteration_2_disc(suite) -> [];
updates_during_checkpoint_iteration_2_disc(Config) when is_list(Config) ->
    updates_during_checkpoint_iteration(disc_copies, 2, Config).

updates_during_checkpoint_iteration_2_disc_only(suite) -> [];
updates_during_checkpoint_iteration_2_disc_only(Config) when is_list(Config) ->
    updates_during_checkpoint_iteration(disc_only_copies, 2, Config).

updates_during_checkpoint_iteration(ReplicaType,NodeConfig,Config) ->
   %?verbose("updates_during_checkpoint_iteration2 at ~p~n", [self()]),
    Delay = 2,
    Nodes = ?acquire_nodes(NodeConfig, Config),
    Node1 = hd(Nodes),
   %?verbose("Mnesia info: ~p~n", [mnesia:info()]),
    File = updates_during_checkpoint_iteration2,
    {success, [A]} = ?start_activities([Node1]),
    ?log("updates_during_checkpoint_iteration with ~p on ~p~n",
         [ReplicaType, Nodes]),
    TpcbConfig = tpcb_config_dist(ReplicaType, NodeConfig, Nodes, Config),
    %%TpcbConfig = tpcb_config(ReplicaType, NodeConfig, Nodes),
    TpcbConfigRec = list2rec(TpcbConfig, 
                                         record_info(fields,tab_config),
                                         #tab_config{}),
    mnesia_tpcb:init(TpcbConfig),
    ?match(ok, mnesia_tpcb:verify_tabs()),

    {ok, CPName, _NodeList} =
        mnesia:activate_checkpoint([{max, mnesia:system_info(tables)},
                                    {ram_overrides_dump,true}]),  
    A ! fun () -> mnesia:backup_checkpoint(CPName, File) end,

    do_changes_during_backup(TpcbConfigRec),

    ?match_receive({A,ok}),

    timer:sleep(timer:seconds(Delay)), 
    ?match(ok, mnesia:install_fallback(File)),
    timer:sleep(timer:seconds(Delay)),

    ?match({error,{"Bad balance",_,_}}, mnesia_tpcb:verify_tabs()),
 
    mnesia_test_lib:kill_mnesia(Nodes),
    mnesia_test_lib:start_mnesia(Nodes,[account,branch,teller, history]),

    ?match(ok, mnesia_tpcb:verify_tabs()),

    ?match(ok, file:delete(File)),
    ?verify_mnesia(Nodes, []).

do_changes_during_backup(TpcbConfig) ->
    loop_branches(TpcbConfig#tab_config.n_branches,
                  TpcbConfig#tab_config.n_accounts_per_branch).

loop_branches(N_br,N_acc) when N_br >= 1 ->
   loop_accounts(N_br,N_acc),
   loop_branches(N_br-1,N_acc);
loop_branches(_,_) -> done.

loop_accounts(N_br, N_acc) when N_acc >= 1 ->
    A = #account{id=N_acc, branch_id=N_br, balance = 4711},
    ok = mnesia:dirty_write(A),
    loop_accounts(N_br, N_acc-1);

loop_accounts(_,_) -> done.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_table_with_activated_checkpoint_ram(suite) -> [];
load_table_with_activated_checkpoint_ram(Config) when is_list(Config) ->
    load_table_with_activated_checkpoint(ram_copies, Config).

load_table_with_activated_checkpoint_disc(suite) -> [];
load_table_with_activated_checkpoint_disc(Config) when is_list(Config) ->
    load_table_with_activated_checkpoint(disc_copies, Config).

load_table_with_activated_checkpoint_disc_only(suite) -> [];
load_table_with_activated_checkpoint_disc_only(Config) when is_list(Config) ->
    load_table_with_activated_checkpoint(disc_only_copies, Config).

load_table_with_activated_checkpoint(Type, Config) ->
    Nodes = ?acquire_nodes(2, Config),
    Node1 = hd(Nodes),
    Tab = load_test,
    Def = [{attributes, [key, value]},
           {Type, Nodes}],            %%  ??? important that RAM  ???
    
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    ?match(ok, mnesia:dirty_write({Tab, 1, 4711})),
    ?match(ok, mnesia:dirty_write({Tab, 2, 42})),
    ?match(ok, mnesia:dirty_write({Tab, 3, 256})),
    
    timer:sleep(timer:seconds(1)), 
    
    {ok, CPName, _NodeList} =
        mnesia:activate_checkpoint([{max, mnesia:system_info(tables)},
                                    {ram_overrides_dump,true}]),
    
    mnesia_test_lib:stop_mnesia([Node1]),    
    mnesia_test_lib:start_mnesia([Node1],[Tab]),      
    %%--- check, whether the checkpiont is attached to both replicas
    {success, [A,B]} = ?start_activities(Nodes),
    
    A ! fun () ->
		mnesia:table_info(Tab,checkpoints)
	end,
    ?match_receive({A,[CPName]}),
    
    B ! fun () ->
		mnesia:table_info(Tab,checkpoints)
	end,
    ?match_receive({B,[CPName]}),    
    
    %%--- check, whether both retainers are consistent    
    ?match(ok, mnesia:dirty_write({Tab, 1, 815})),       
    A ! fun () ->
		mnesia:backup_checkpoint(CPName, load_table_a)
	end,
    ?match_receive({A,ok}),
    B ! fun () ->
		mnesia:backup_checkpoint(CPName, load_table_b)
	end,
    ?match_receive({B,ok}),
    
    Mod = mnesia_backup, %% Assume local files
    List_a =  view(load_table_a, Mod),
    List_b =  view(load_table_b, Mod),
    
    ?match(List_a, List_b),
    
    ?match(ok,file:delete(load_table_a)),
    ?match(ok,file:delete(load_table_b)),
    ?verify_mnesia(Nodes, []).

view(Source, Mod) ->
    View = fun(Item, Acc) ->
                   ?verbose("tab - item : ~p ~n",[Item]),
		   case Item of
		       {schema, Tab, Cs} ->  %% Remove cookie information
			   NewCs = lists:keyreplace(cookie, 1, Cs,
						    {cookie, skip_cookie}),
			   Item2 = {schema, Tab, NewCs},
			   {[Item], [Item2|Acc]};
		       _ ->   
			   {[Item], [Item|Acc]}
		   end
           end,
    {ok,TabList} =  
	mnesia:traverse_backup(Source, Mod, dummy, read_only, View, []),
    lists:sort(TabList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_table_copy_to_table_checkpoint_ram(suite) -> [];
add_table_copy_to_table_checkpoint_ram(Config) when is_list(Config) ->
    add_table_copy_to_table_with_activated_checkpoint(ram_copies, Config).

add_table_copy_to_table_checkpoint_disc(suite) -> [];
add_table_copy_to_table_checkpoint_disc(Config) when is_list(Config) ->
    add_table_copy_to_table_with_activated_checkpoint(disc_copies, Config).

add_table_copy_to_table_checkpoint_disc_only(suite) -> [];
add_table_copy_to_table_checkpoint_disc_only(Config) when is_list(Config) ->
    add_table_copy_to_table_with_activated_checkpoint(disc_only_copies, Config).

add_table_copy_to_table_with_activated_checkpoint(Type,Config) -> 
    Nodes = ?acquire_nodes(2, Config),
						%?verbose("NODES = ~p ~n",[Nodes]),
    [Node1,Node2] = Nodes,

    Tab = add_test,
    Def = [{attributes, [key, value]},
           {Type, [Node1]}],            %%  ??? important that RAM  ???

    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    ?match(ok, mnesia:dirty_write({Tab, 1, 4711})),
    ?match(ok, mnesia:dirty_write({Tab, 2, 42})),
    ?match(ok, mnesia:dirty_write({Tab, 3, 256})),

    {ok, CPName, _NodeList} =
        mnesia:activate_checkpoint([{max, mnesia:system_info(tables)},
                                    {ram_overrides_dump,true}]),

    ?match({atomic,ok},mnesia:add_table_copy(Tab,Node2,ram_copies)),

    %%--- check, whether the checkpiont is attached to both replicas
    {success, [A,B]} = ?start_activities(Nodes),

    A ! fun () ->
		mnesia:table_info(Tab,checkpoints)
	end,
    ?match_receive({A,[CPName]}),

    B ! fun () ->
		mnesia:table_info(Tab,checkpoints)
	end,
    ?match_receive({B,[CPName]}),

    %%--- check, whether both retainers are consistent

    ?match(ok, mnesia:dirty_write({Tab, 1, 815})),
    ?match(ok, mnesia:dirty_write({Tab, 2, 815})),

    A ! fun () ->
		mnesia:backup_checkpoint(CPName, add_table_a)
	end,
    ?match_receive({A,ok}),
    B ! fun () ->
		mnesia:backup_checkpoint(CPName, add_table_b)
	end,
    ?match_receive({B,ok}),

    Mod = mnesia_backup, %% Assume local files

    List_a = view(add_table_a, Mod),
    List_b = view(add_table_b, Mod),

    ?match(List_a, List_b),

    ?match(ok,file:delete(add_table_a)),
    ?match(ok, file:delete(add_table_b)),
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inst_fallback_process_dies(suite) -> 
    [];
inst_fallback_process_dies(Config) when is_list(Config) ->
    ?is_debug_compiled,
    
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    {success, [A,_B,_C]} = ?start_activities(Nodes),
    
    TestPid = self(),
    DebugId = {mnesia_bup, fallback_receiver_loop, pre_swap},
    DebugFun =
        fun(PrevContext, _EvalContext) ->
                ?verbose("fallback_receiver_loop -  pre_swap pid ~p  #~p~n",
                     [self(),PrevContext]),
                TestPid ! {self(),fallback_preswap},
		case receive_messages([fallback_continue]) of
		    [{TestPid,fallback_continue}] ->
			?deactivate_debug_fun(DebugId),
			PrevContext+1
		end
        end,
    ?activate_debug_fun(DebugId, DebugFun, 1),
        
    Tab = install_table,
    Def = [{attributes, [key, value]}, {disc_copies, Nodes}], 
    
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    
    ?match(ok, mnesia:dirty_write({Tab, 1, 4711})),
    ?match(ok, mnesia:dirty_write({Tab, 2, 42})),
    ?match(ok, mnesia:dirty_write({Tab, 3, 256})),
    
    {ok, CPName, _NodeList} =
        mnesia:activate_checkpoint([{max, mnesia:system_info(tables)},
                                    {ram_overrides_dump,true}]),
    
    ?match(ok, mnesia:backup_checkpoint(CPName, install_backup)),
    
    A ! fun() -> mnesia:install_fallback(install_backup) end,    
    [{AnsPid,fallback_preswap}] = receive_messages([fallback_preswap]),
    exit(A, kill),
    AnsPid ! {self(), fallback_continue},
    ?match_receive({'EXIT', A, killed}), 
    timer:sleep(2000),  %% Wait till fallback is installed everywhere

    mnesia_test_lib:kill_mnesia(Nodes),    
    ?verbose("~n---->Mnesia is stopped everywhere<-----~n", []),
    ?match([], mnesia_test_lib:start_mnesia(Nodes,[Tab])),
    
    check_data(Nodes, Tab),
    ?match(ok, file:delete(install_backup)),
    ?verify_mnesia(Nodes, []).

check_data([N1 | R], Tab) ->
    ?match([{Tab, 1, 4711}], rpc:call(N1, mnesia, dirty_read, [{Tab, 1}])),
    ?match([{Tab, 2, 42}],   rpc:call(N1, mnesia, dirty_read, [{Tab, 2}])),
    ?match([{Tab, 3, 256}],  rpc:call(N1, mnesia, dirty_read, [{Tab, 3}])),
    check_data(R, Tab);
check_data([], _Tab) -> 
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fatal_when_inconsistency(suite) -> 
    [];
fatal_when_inconsistency(Config) when is_list(Config) ->
    ?is_debug_compiled,
    
    [Node1, Node2, Node3] = Nodes = 
	?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    {success, [A,_B,_C]} = ?start_activities(Nodes),
    
    TestPid = self(),
    DebugId = {mnesia_bup, fallback_receiver_loop, pre_swap},
    DebugFun =
        fun(PrevContext, _EvalContext) ->
                ?verbose("fallback_receiver_loop -  pre_swap pid ~p  #~p~n",
                     [self(),PrevContext]),
                TestPid ! {self(),fallback_preswap},
		case receive_messages([fallback_continue])  of
		    [{TestPid,fallback_continue}] ->
			?deactivate_debug_fun(DebugId),
			PrevContext+1
		end
        end,
    ?activate_debug_fun(DebugId, DebugFun, 1),
        
    Tab = install_table,
    Def = [{attributes, [key, value]}, {disc_copies, Nodes}], 
    
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    
    ?match(ok, mnesia:dirty_write({Tab, 1, 4711})),
    ?match(ok, mnesia:dirty_write({Tab, 2, 42})),
    ?match(ok, mnesia:dirty_write({Tab, 3, 256})),
    
    {ok, CPName, _NodeList} =
        mnesia:activate_checkpoint([{max, mnesia:system_info(tables)},
                                    {ram_overrides_dump,true}]),
    
    ?match(ok, mnesia:backup_checkpoint(CPName, install_backup)),
    ?match(ok, mnesia:dirty_write({Tab, 2, 42424242})),
    
    A ! fun() ->
                mnesia:install_fallback(install_backup)
        end,    

    [{AnsPid,fallback_preswap}] = receive_messages([fallback_preswap]),
    exit(AnsPid, kill),  %% Kill install-fallback on local node will 
    AnsPid ! {self(), fallback_continue},
    ?deactivate_debug_fun(DebugId),
        
    ?match_receive({A,{error,{"Cannot install fallback",
			      {'EXIT',AnsPid,killed}}}}),
    mnesia_test_lib:kill_mnesia(Nodes),
    ?verbose("EXPECTING FATAL from 2 nodes WITH CORE DUMP~n", []),
    
    ?match([], mnesia_test_lib:start_mnesia([Node1],[])),
    is_running(Node1, yes),
    ?match([{Node2, mnesia, _}], mnesia_test_lib:start_mnesia([Node2],[])),
    is_running(Node2, no),
    ?match([{Node3, mnesia, _}], mnesia_test_lib:start_mnesia([Node3],[])),
    is_running(Node3, no),
    mnesia_test_lib:kill_mnesia(Nodes),
    
    ?match(ok, mnesia:install_fallback(install_backup)),    
    mnesia_test_lib:start_mnesia(Nodes,[Tab]),
    
    check_data(Nodes, Tab),
    
    ?match(ok,file:delete(install_backup)),
    ?verify_mnesia(Nodes, []).

is_running(Node, Shouldbe) ->
    timer:sleep(1000),
    Running = rpc:call(Node, mnesia, system_info, [is_running]),
    case Running of
	Shouldbe -> ok;
	_  -> is_running(Node, Shouldbe)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

after_delete(doc) ->
    ["interrupt the uninstall after deletion of ",
     "fallback files - there shall be no fallback"];
after_delete(suite) -> [];
after_delete(Config) when is_list(Config) ->
        do_uninstall(Config, post_delete).

%%%%%%%%%%%%%%%%%%%%%%%%%

do_uninstall(Config,DebugPoint) ->
    ?is_debug_compiled,
    
    Nodes = ?acquire_nodes(3, Config),
    %%?verbose("NODES = ~p ~n",[Nodes]),
    
    {success, [P1,P2,P3]} = ?start_activities(Nodes),
    
    NP1 = node(P1),
    NP2 = node(P2),
    
    {A,B,C} = case node() of
		  NP1 ->
		      %%?verbose("first case ~n"),
		      {P3,P2,P1};
		  NP2 ->
		      %%?verbose("second case ~n"),
		      {P3, P1, P2};
		  _  ->
		      { P1, P2, P3}
	      end,
    
    Node1 = node(A),
    Node2 = node(B),
    Node3 = node(C),
    
    ?verbose(" A   pid:~p  node:~p ~n",[A,Node1]),
    ?verbose(" B   pid:~p  node:~p ~n",[B,Node2]),
    ?verbose(" C   pid:~p  node:~p ~n",[C,Node3]),
    
    
    TestPid = self(),
    %%?verbose("TestPid : ~p~n",[TestPid]),
    DebugId = {mnesia_bup, uninstall_fallback2, DebugPoint},
    DebugFun = fun(PrevContext, _EvalContext) ->
		       ?verbose("uninstall_fallback pid ~p  #~p~n"
				,[self(),PrevContext]),
		       TestPid ! {self(),uninstall_predelete},
		       case receive_messages([uninstall_continue]) of
			   [{TestPid,uninstall_continue}] ->
			       ?deactivate_debug_fun(DebugId),
			       %%?verbose("uninstall_fallback continues~n"),
			       PrevContext+1
		       end
	       end,
    ?remote_activate_debug_fun(Node1,DebugId, DebugFun, 1),
    
    Tab = install_table,
    Def = [{attributes, [key, value]},
           {ram_copies, Nodes}],     %% necessary to test different types ???  
    
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    
    ?match(ok, mnesia:dirty_write({Tab, 1, 4711})),
    ?match(ok, mnesia:dirty_write({Tab, 2, 42})),
    ?match(ok, mnesia:dirty_write({Tab, 3, 256})),
    
    {ok, CPName, _NodeList} =
        mnesia:activate_checkpoint([{max, mnesia:system_info(tables)},
                                    {ram_overrides_dump,true}]),
    
    ?match(ok, mnesia:backup_checkpoint(CPName,install_backup)),
    timer:sleep(timer:seconds(1)), 
    
    A ! fun () ->
		mnesia:install_fallback(install_backup)
	end,
    ?match_receive({A,ok}),
    
    A ! fun () ->
		mnesia:uninstall_fallback()
	end,
    %%
    %%  catch the debug entry in mnesia and kill one Mnesia node
    %%
    
    
    [{AnsPid,uninstall_predelete}] = receive_messages([uninstall_predelete]),
    
    ?verbose("AnsPid : ~p~n",[AnsPid]),
    
    mnesia_test_lib:kill_mnesia([Node2]),
    timer:sleep(timer:seconds(1)), 
    
    AnsPid ! {self(),uninstall_continue},
    
    ?match_receive({A,ok}),
    
    mnesia_test_lib:kill_mnesia(Nodes) ,
    mnesia_test_lib:start_mnesia(Nodes,[Tab]),
    
    A ! fun () ->
		R1 = mnesia:dirty_read({Tab,1}),
		R2 = mnesia:dirty_read({Tab,2}),
		R3 = mnesia:dirty_read({Tab,3}),
		{R1,R2,R3}
	end,    
    ?match_receive({ A, {[],[],[]} }),
    
    B ! fun () ->
		R1 = mnesia:dirty_read({Tab,1}),
		R2 = mnesia:dirty_read({Tab,2}),
		R3 = mnesia:dirty_read({Tab,3}),
		{R1,R2,R3}
	end,
    ?match_receive({ B, {[],[],[]} }),
    
    C ! fun () ->
		R1 = mnesia:dirty_read({Tab,1}),
		R2 = mnesia:dirty_read({Tab,2}),
		R3 = mnesia:dirty_read({Tab,3}),
		{R1,R2,R3}
	end,
    ?match_receive({ C, {[],[],[]} }),
    
    ?match(ok,file:delete(install_backup)),
    ?verify_mnesia(Nodes, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%

cause_switch_before(doc)  ->
      ["interrupt the backup before iterating the retainer"];
cause_switch_before(suite)  ->  [];
cause_switch_before(Config) when is_list(Config) ->
   do_something_during_backup(cause_switch,pre,Config).

%%%%%%%%%%%%%%%

cause_switch_after(doc)  ->
      ["interrupt the backup after iterating the retainer"];
cause_switch_after(suite)  ->  [];
cause_switch_after(Config) when is_list(Config) ->
   do_something_during_backup(cause_switch,post,Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%

cause_abort_before(doc) ->
      ["interrupt the backup before iterating the retainer"];

cause_abort_before(suite) ->  [];
cause_abort_before(Config) when is_list(Config) ->
   do_something_during_backup(cause_abort,pre,Config).

%%%%%%%%%%%%%%%%%%

cause_abort_after(doc) ->
      ["interrupt the backup after iterating the retainer"];

cause_abort_after(suite) ->  [];
cause_abort_after(Config) when is_list(Config) ->
   do_something_during_backup(cause_abort,post,Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%

change_schema_before(doc) ->
      ["interrupt the backup before iterating the retainer"];
change_schema_before(suite) -> [];
change_schema_before(Config) when is_list(Config) ->
   do_something_during_backup(change_schema,pre,Config).

%%%%%%%%%%%%%%%%

change_schema_after(doc) ->
      ["interrupt the backup after iterating the retainer"];
change_schema_after(suite) -> [];
change_schema_after(Config) when is_list(Config) ->
   do_something_during_backup(change_schema,post,Config).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_something_during_backup(Action,DebugPoint,Config) ->
    ?is_debug_compiled,
    
    Nodes = ?acquire_nodes(3, Config),
    
    {success, [A,B,C]} = ?start_activities(Nodes),
    
    Node1 = node(A),
    Node2 = node(B),
    Node3 = node(C),
    
    TestPid = self(),
    %%?verbose("TestPid : ~p~n",[TestPid]),
    
    Tab = interrupt_table,
    Bak = interrupt_backup,
    Def = [{attributes, [key, value]},
           {ram_copies, [Node2,Node3]}],  
    %% necessary to test different types ???  
    
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    
    
    
    DebugId = {mnesia_log, tab_copier, DebugPoint},
    DebugFun = fun(PrevContext, EvalContext) ->
		       ?verbose("interrupt backup  pid ~p  #~p ~n context ~p ~n"
			    ,[self(),PrevContext,EvalContext]),
		       TestPid ! {self(),interrupt_backup_pre},
		       global:set_lock({{lock_for_backup, Tab}, self()},
				       Nodes,
				       infinity),
		       		       
		       %%?verbose("interrupt backup - continues ~n"),
		       ?deactivate_debug_fun(DebugId),
		       PrevContext+1
	       end,
    ?remote_activate_debug_fun(Node1,DebugId, DebugFun, 1),
    
    ?match(ok, mnesia:dirty_write({Tab, 1, 4711})),
    ?match(ok, mnesia:dirty_write({Tab, 2, 42})),
    ?match(ok, mnesia:dirty_write({Tab, 3, 256})),
    
    {ok, CPName, _NodeList} =
        mnesia:activate_checkpoint([{max, mnesia:system_info(tables)},
                                    {ram_overrides_dump,true}]),
    
    A ! fun () ->
		%%?verbose("node: ~p pid: ~p ~n",[node(),self()]),
		mnesia:table_info(Tab,where_to_read)
	end,

    ReadNode_a = receive { A, ReadNode_a_tmp } -> ReadNode_a_tmp end,
    ?verbose("ReadNode ~p ~n",[ReadNode_a]),
    
    global:set_lock({{lock_for_backup, Tab}, self()}, Nodes, infinity),
    
    A ! fun () ->    %% A shall perform the backup, so the test proc is
		%% able to do further actions in between
		mnesia:backup_checkpoint(CPName, Bak)
	end,
    
    %% catch the debug function of mnesia, stop the backup process 
    %% kill the node ReadNode_a and continue the backup process
    %% As there is a second replica of the table, the backup shall continue

    case receive_messages([interrupt_backup_pre]) of
	[{_AnsPid,interrupt_backup_pre}] -> ok
    end,
    
    case Action of
        cause_switch ->
	    mnesia_test_lib:kill_mnesia([ReadNode_a]),
	    timer:sleep(timer:seconds(1));
        cause_abort ->
	    mnesia_test_lib:kill_mnesia([Node2,Node3]),
	    timer:sleep(timer:seconds(1));
        change_schema ->
	    Tab2 = second_interrupt_table,
	    Def2 = [{attributes, [key, value]},
		    {ram_copies, Nodes}],  
	    
	    ?match({atomic, ok}, mnesia:create_table(Tab2, Def2))
    end,
    
    %%    AnsPid ! {self(),interrupt_backup_continue},
    global:del_lock({{lock_for_backup, Tab}, self()}, Nodes),
    
    case Action of
        cause_abort -> 
	    
            %% answer of A when finishing the backup
            ?match_receive({A,{error, _}}), 
	    
            ?match({error,{"Cannot install fallback",_}},
                   mnesia:install_fallback(Bak)); 
        _ ->      %% cause_switch, change_schema
	    
            ?match_receive({A,ok}), %% answer of A when finishing the backup
	    
            %% send a fun to that node where mnesia is still running
            WritePid = case ReadNode_a of
			   Node2 -> C; %%   node(C) == Node3
			   Node3 -> B
                       end,
            WritePid ! fun () ->
			       ?match(ok, mnesia:dirty_write({Tab, 1, 815})),
			       ?match(ok, mnesia:dirty_write({Tab, 2, 816})),
			       ok
                       end,
            ?match_receive({ WritePid, ok }),  
            ?match(ok, mnesia:install_fallback(Bak))
    end,        
    
    %% Stop and then start mnesia and check table consistency
    %%?verbose("Restarting Mnesia~n", []),
    mnesia_test_lib:kill_mnesia(Nodes),
    mnesia_test_lib:start_mnesia(Nodes,[Tab]),
    
    case Action of
        cause_switch ->
	    %% the backup should exist
	    cross_check_tables([A,B,C],Tab,{[{Tab,1,4711}],
					    [{Tab,2,42}],
					    [{Tab,3,256}] }),
	    ?match(ok,file:delete(Bak));
        cause_abort ->
	    %% the backup should  NOT exist
	    cross_check_tables([A,B,C],Tab,{[],[],[]}),
	    %% file does not exist 
	    ?match({error, _},file:delete(Bak)); 
        change_schema ->
	    %% the backup should exist
	    cross_check_tables([A,B,C],Tab,{[{Tab,1,4711}],
					    [{Tab,2,42}],
					    [{Tab,3,256}] }),
	    ?match(ok,file:delete(Bak))
    end,
    ?verify_mnesia(Nodes, []).

%% check the contents of the table
cross_check_tables([],_tab,_elements) -> ok;
cross_check_tables([Pid|Rest],Tab,{Val1,Val2,Val3}) ->
    Pid ! fun () ->
              R1 = mnesia:dirty_read({Tab,1}),
              R2 = mnesia:dirty_read({Tab,2}),
              R3 = mnesia:dirty_read({Tab,3}),
              {R1,R2,R3}
            end,
    ?match_receive({ Pid, {Val1, Val2, Val3 } }),
    cross_check_tables(Rest,Tab,{Val1,Val2,Val3} ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEGIN: Helpers for restart/set-master-node scenarios.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exercise A's choice of load source under different table master policies.
%% C stays active while B and A stop. A restarts in isolation, then reconnects
%% to C while B remains stopped. B and C have no master restrictions.
%%
%% chain: A trusts only [B] and cannot load directly from C. Its policy stays
%%        unchanged. B rejoins and starts, loads from C, then supplies A.
%%        The chain is the data transfer C -> B -> A, not a chain of policies.
%% multiple: A trusts [B, C] from the outset. Connecting to the active C is
%%           enough to load A, even though the unavailable B is listed first.
%% clear: A initially trusts [B] and waits despite C being connected. Clearing
%%        its masters to [] removes the restriction, allowing C -> A.
%% expand: A initially trusts [B] and waits despite C being connected. Setting
%%         its masters to [B, C] makes C eligible, allowing C -> A.
%%
%% clear and expand release A's wait by changing its policy while B is still
%% stopped, without restarting A or reconnecting C. Unlike multiple, expand
%% tests making an already-connected, initially forbidden source eligible.
-spec run_active_source_case(Config :: proplists:proplist(),
                             Mode :: chain | multiple | clear | expand) -> ok.
run_active_source_case(Config, Mode) ->
    run_restart_case(Config, fun(A, B, C) ->
        Nodes = [A, B, C],
        %% GIVEN three replicas, with a remote-only master policy on A.
        Tabs = create_tables(Nodes),
        Masters = case Mode of multiple -> [B, C]; _ -> [B] end,
        mnesia_node_set_masters(A, Tabs, Masters),
        mnesia_node_assert_master_policies(Tabs, [{A, Masters}, {B, []}, {C, []}]),
        %% C stays active. B must remember C as a potentially better copy.
        mnesia_node_stop(B, [A, C]),
        mnesia_node_stop(A, [C]),
        write_marker(C, Tabs, source_c),
        mnesia_node_assert_local_records(C, Tabs,
                                         fun(Tab) -> marked_records(Tab, source_c) end),
        mnesia_node_set_partitions([[A], [B], [C]]),
        mnesia_node_start(A),
        mnesia_node_assert_master_policies(Tabs, [{A, Masters}]),
        ?assertEqual(false, mnesia_node_call(A, fun() ->
            mnesia_recover:has_mnesia_down(C)
        end)),
        mnesia_node_assert_waiting(A, Tabs),

        %% WHEN A reconnects to C, while B remains stopped and isolated.
        mnesia_node_set_connected_groups([[A, C], [B]]),
        mnesia_node_connect([A, C]),
        mnesia_node_assert_active_replica(A, Tabs, C),
        case Mode of
            multiple ->
                %% THEN an available member is enough, even with B first.
                mnesia_node_assert_loaded_from(
                    A, Tabs, C, fun(Tab) -> marked_records(Tab, source_c) end);
            _ ->
                %% THEN C is registered but cannot supply A's local copy.
                mnesia_node_assert_waiting(A, Tabs)
        end,
        case Mode of
            chain ->
                %% WHEN B rejoins and starts normally, it first loads from C.
                mnesia_node_set_connected_groups([Nodes]),
                mnesia_node_start(B),
                ?assertEqual(false, mnesia_node_call(B, fun() ->
                    mnesia_recover:has_mnesia_down(C)
                end)),
                mnesia_node_assert_loaded_from(
                    B, Tabs, C, fun(Tab) -> marked_records(Tab, source_c) end),
                %% THEN A obtains that data from B, never directly from C.
                mnesia_node_assert_loaded_from(
                    A, Tabs, B, fun(Tab) -> marked_records(Tab, source_c) end);
            _ ->
                NewMasters = case Mode of
                                 clear -> [];
                                 expand -> [B, C];
                                 multiple -> Masters
                             end,
                %% WHEN a waiting node clears/expands its policy, there is no
                %% restart, reconnection, or new announcement from C.
                Controller = mnesia_node_call(A, fun() -> whereis(mnesia_controller) end),
                mnesia_node_set_masters(A, Tabs, NewMasters),
                mnesia_node_assert_loaded_from(
                    A, Tabs, C, fun(Tab) -> marked_records(Tab, source_c) end),
                ?assertEqual(Controller, mnesia_node_call(A, fun() ->
                    whereis(mnesia_controller)
                end)),
                mnesia_node_assert_topology([[A, C], [B]]),
                mnesia_node_assert_master_policies(Tabs, [{A, NewMasters}, {C, []}]),
                mnesia_node_set_connected_groups([Nodes]),
                mnesia_node_start(B)
        end,
        %% THEN all local copies contain the marker written while A/B were down.
        [mnesia_node_assert_locally_readable(N, Tabs) || N <- Nodes],
        [mnesia_node_assert_local_records(N, Tabs,
                                         fun(Tab) -> marked_records(Tab, source_c) end)
         || N <- Nodes],
        mnesia_node_assert_master_policies(Tabs, [{B, []}, {C, []}])
    end).

run_restart_case(Config, Test) ->
    case mnesia_test_lib:diskless(Config) of
        true -> ?skip("Master node settings do not survive a diskless restart", []);
        false -> ok
    end,
    [C | Peers] = Nodes = ?acquire_nodes(3, Config),
    [A, B] = lists:sort(Peers),
    Cookies = [{N, mnesia_node_call(N, fun erlang:get_cookie/0)} || N <- Nodes],
    try
        [begin
             ?assertEqual(disc_copies, mnesia_node_call(N, fun() ->
                 mnesia:table_info(schema, storage_type)
             end)),
             ?assertEqual([], mnesia_node_call(N, fun() ->
                 mnesia_recover:get_master_nodes(schema)
             end))
         end || N <- Nodes],
        Test(A, B, C),
        mnesia_node_assert_running(Nodes)
    after
        %% Even on assertion failure, remove policies and stop Mnesia before
        %% healing. The next acquire_nodes rebuilds fresh schemas. Intentional
        %% waiting/divergence is recovered and checked in each successful case.
        Cleanup = [catch mnesia_node_call(N, fun() ->
             stopped = mnesia:stop(),
             mnesia:set_master_nodes([])
         end) || N <- Nodes],
        [?assertEqual(true, mnesia_node_call(N, fun() -> erlang:set_cookie(Cookie) end))
         || {N, Cookie} <- Cookies],
        mnesia_node_set_connected_groups([Nodes]),
        ?assertEqual([ok || _ <- Nodes], Cleanup)
    end.

create_tables(Nodes) ->
    create_tables([{ram, [{ram_copies, Nodes}]},
                           {disc, [{disc_copies, Nodes}]}, %% Why both disc_only and disc copy?
                           {disc_only, [{disc_only_copies, Nodes}]}], Nodes).

create_tables(Definitions, Nodes) ->
    Tabs = [Tab || {Tab, _} <- Definitions],
    [begin
         ?assertEqual({atomic, ok}, mnesia:create_table(Tab, Copies)),
         ?assertEqual(false, mnesia:table_info(Tab, local_content)),
         ?assertEqual(false, mnesia:table_info(Tab, majority)),
         ?assertEqual(read_write, mnesia:table_info(Tab, access_mode)),
         ok = mnesia:sync_dirty(fun() ->
             [mnesia:write(Record) || Record <- expected_records(Tab)], ok
         end)
     end || {Tab, Copies} <- Definitions],
    [begin
         mnesia_node_assert_locally_readable(N, Tabs),
         mnesia_node_assert_local_records(N, Tabs, fun expected_records/1),
         ?assertEqual(ok, mnesia_node_call(N, fun mnesia:sync_log/0))
     end || N <- Nodes],
    mnesia_node_assert_master_policies(Tabs, [{N, []} || N <- lists:usort([node() | Nodes])]),
    Tabs.

write_marker(Node, Tabs, Value) ->
    ?assertEqual(ok, mnesia_node_call(Node, fun() ->
        mnesia:sync_dirty(fun() ->
            [mnesia:write({Tab, marker, Value}) || Tab <- Tabs], ok
        end)
    end)).

expected_records(Tab) ->
    [{Tab, K, K} || K <- lists:seq(1, 10)].

marked_records(Tab, Marker) ->
    expected_records(Tab) ++ [{Tab, marker, Marker}].

restart_records(ram) -> [];
restart_records(Tab) -> expected_records(Tab).

recover_follower(Authority, Follower, C, Tabs, Records) ->
    mnesia_node_stop(Follower, []),
    mnesia_node_set_masters(Follower, Tabs, [Authority]),
    mnesia_node_set_connected_groups([[Authority, Follower, C]]),
    mnesia_node_start(Follower),
    mnesia_node_assert_loaded_from(Follower, Tabs, Authority, Records),
    mnesia_node_assert_local_records(Authority, Tabs, Records),
    mnesia_node_assert_master_policies(Tabs, [{Follower, [Authority]}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% END: Helpers for restart/set-master-node scenarios.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
