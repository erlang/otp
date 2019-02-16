%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2018. All Rights Reserved.
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
         consistency_after_rename_of_node/1,
         consistency_after_restart_1_ram/1,
         consistency_after_restart_1_disc/1,
         consistency_after_restart_1_disc_only/1,
         consistency_after_restart_2_ram/1,
         consistency_after_restart_2_disc/1,
         consistency_after_restart_2_disc_only/1,
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

-include("mnesia_test_lib.hrl").

init_per_testcase(Func, Conf) ->
    mnesia_test_lib:init_per_testcase(Func, Conf).

end_per_testcase(Func, Conf) ->
    mnesia_test_lib:end_per_testcase(Func, Conf).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all() -> 
    [{group, consistency_after_restart},
     {group, consistency_after_dump_tables},
     {group, consistency_after_add_replica},
     {group, consistency_after_del_replica},
     {group, consistency_after_move_replica},
     {group, consistency_after_transform_table},
     consistency_after_change_table_copy_type,
     {group, consistency_after_restore},
     consistency_after_rename_of_node,
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
        Else -> ?warning("Recevied unexpected Msg~n ~p ~n", [Else])
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
    timer:sleep(timer:seconds(10)),
    mnesia_test_lib:kill_mnesia([Node1]),
    %% Start and wait for tables to be loaded on all nodes
    timer:sleep(timer:seconds(3)),
    ?match([], mnesia_test_lib:start_mnesia(Nodes,[account,branch,teller, history])), 
    mnesia_tpcb:stop(),
    ?match(ok, mnesia_tpcb:verify_tabs()),
    ?verify_mnesia(Nodes, []).

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
    timer:sleep(timer:seconds(10)),
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
    timer:sleep(timer:seconds(10)),
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
    timer:sleep(timer:seconds(10)),
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
    timer:sleep(timer:seconds(10)),
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

    Filter = fun(Tab) -> mnesia:foldl(fun(A, Acc) when size(A) == 3 -> [A|Acc];
					 (A, Acc) when size(A) == 4 -> Acc
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
    Delay = 5,
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
consistency_after_rename_of_node(doc) ->
    ["Skipped because it is an unimportant case."].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    Delay = 5,
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
    Delay = 5,
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
