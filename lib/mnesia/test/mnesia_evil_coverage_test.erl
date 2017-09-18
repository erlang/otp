%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(mnesia_evil_coverage_test).
-author('hakan@erix.ericsson.se').
-include("mnesia_test_lib.hrl").

-export([init_per_testcase/2, end_per_testcase/2,
         init_per_group/2, end_per_group/2,
         all/0, groups/0]).

-export([system_info/1, table_info/1, error_description/1,
         db_node_lifecycle/1, evil_delete_db_node/1, start_and_stop/1,
         checkpoint/1, table_lifecycle/1, storage_options/1,
         add_copy_conflict/1, add_copy_when_going_down/1,
         replica_management/1, clear_table_during_load/1,
         schema_availability/1, local_content/1,
         replica_location/1, user_properties/1, unsupp_user_props/1,
         sorted_ets/1,
         change_table_access_mode/1, change_table_load_order/1,
         set_master_nodes/1, offline_set_master_nodes/1,
         dump_tables/1, dump_log/1, wait_for_tables/1, force_load_table/1,
         snmp_open_table/1, snmp_close_table/1, snmp_get_next_index/1,
         snmp_get_row/1, snmp_get_mnesia_key/1, snmp_update_counter/1,
         snmp_order/1, subscribe_standard/1, subscribe_extended/1,
         foldl/1, info/1, schema_0/1, schema_1/1, view_0/1, view_1/1, view_2/1,
         lkill/1, kill/1,
         record_name_dirty_access_ram/1,
         record_name_dirty_access_disc/1,
         record_name_dirty_access_disc_only/1,
         record_name_dirty_access_xets/1]).

-export([info_check/8]).

-define(cleanup(N, Config),
	mnesia_test_lib:prepare_test_case([{reload_appls, [mnesia]}],
					  N, Config, ?FILE, ?LINE)).
init_per_testcase(Func, Conf) ->
    mnesia_test_lib:init_per_testcase(Func, Conf).

end_per_testcase(Func, Conf) ->
    mnesia_test_lib:end_per_testcase(Func, Conf).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all() -> 
    [system_info, table_info, error_description,
     db_node_lifecycle, evil_delete_db_node, start_and_stop,
     checkpoint, table_lifecycle, storage_options, 
     add_copy_conflict,
     add_copy_when_going_down, replica_management, clear_table_during_load,
     schema_availability, local_content,
     {group, table_access_modifications}, replica_location,
     {group, table_sync}, user_properties, unsupp_user_props,
     {group, record_name}, {group, snmp_access},
     {group, subscriptions}, {group, iteration},
     {group, debug_support}, sorted_ets,
     {mnesia_dirty_access_test, all},
     {mnesia_trans_access_test, all},
     {mnesia_evil_backup, all}].

groups() -> 
    [{table_access_modifications, [],
      [change_table_access_mode, change_table_load_order,
       set_master_nodes, offline_set_master_nodes]},
     {table_sync, [],
      [dump_tables, dump_log, wait_for_tables,
       force_load_table]},
     {snmp_access, [],
      [snmp_open_table, snmp_close_table, snmp_get_next_index,
       snmp_get_row, snmp_get_mnesia_key, snmp_update_counter,
       snmp_order]},
     {subscriptions, [],
      [subscribe_standard, subscribe_extended]},
     {iteration, [], [foldl]},
     {debug_support, [],
      [info, schema_0, schema_1, view_0, view_1, view_2,
       lkill, kill]},
     {record_name, [], [{group, record_name_dirty_access}]},
     {record_name_dirty_access, [],
      [record_name_dirty_access_ram,
       record_name_dirty_access_disc,
       record_name_dirty_access_disc_only,
       record_name_dirty_access_xets
      ]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Get meta info about Mnesia

system_info(suite) -> [];
system_info(Config) when is_list(Config) ->
    Nodes = ?acquire_nodes(all, Config),
    Ns = ?sort(Nodes),
    ?match(yes, mnesia:system_info(is_running)),
    ?match(Ns, ?sort(mnesia:system_info(db_nodes))),
    ?match(Ns, ?sort(mnesia:system_info(running_db_nodes))),
    ?match(A when is_atom(A), mnesia:system_info(debug)),
    ?match(L when is_list(L), mnesia:system_info(directory)),
    ?match(L when is_list(L), mnesia:system_info(log_version)),
    ?match({_, _}, mnesia:system_info(schema_version)),
    ?match(L when is_list(L), mnesia:system_info(tables)),
    ?match(L when is_list(L), mnesia:system_info(local_tables)),
    ?match(L when is_list(L), mnesia:system_info(held_locks)),
    ?match(L when is_list(L), mnesia:system_info(lock_queue)),
    ?match(L when is_list(L), mnesia:system_info(transactions)),
    ?match(I when is_integer(I), mnesia:system_info(transaction_failures)),
    ?match(I when is_integer(I), mnesia:system_info(transaction_commits)),
    ?match(I when is_integer(I), mnesia:system_info(transaction_restarts)),
    ?match(L when is_list(L), mnesia:system_info(checkpoints)),
    ?match(A when is_atom(A), mnesia:system_info(backup_module)),
    ?match(true, mnesia:system_info(auto_repair)),
    ?match({_, _}, mnesia:system_info(dump_log_interval)),
    ?match(A when is_atom(A), mnesia:system_info(dump_log_update_in_place)),
    ?match(I when is_integer(I), mnesia:system_info(transaction_log_writes)),
    ?match(I when is_integer(I), mnesia:system_info(send_compressed)),
    ?match(L when is_list(L), mnesia:system_info(all)),
    ?match(L when is_list(L), mnesia:system_info(backend_types)),
    ?match({'EXIT', {aborted, Reason }} when element(1, Reason) == badarg
           , mnesia:system_info(ali_baba)),
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Get meta info about table

table_info(suite) -> [];
table_info(Config) when is_list(Config) ->
    [Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config),
    
    Tab  = table_info,
    Type = bag,
    ValPos = 3,
    Attrs = [k, v],
    Arity = length(Attrs) +1,

    Schema = 
	case mnesia_test_lib:diskless(Config) of
	    true -> [{type, Type}, {attributes, Attrs}, {index, [ValPos]},
		     {ram_copies, [Node1, Node2]}, {ext_ets, [Node3]}];
	    false ->		
		[{type, Type}, {attributes, Attrs}, {index, [ValPos]},
		 {disc_only_copies, [Node1]}, {ram_copies, [Node2]},
		 {ext_ets, [Node3]}]
	end,
    ?match({atomic, ok}, mnesia:create_table(Tab, Schema)),

    Size = 10,
    Keys = lists:seq(1, Size),
    Records = [{Tab, A, 7} || A <- Keys],
    lists:foreach(fun(Rec) -> ?match(ok, mnesia:dirty_write(Rec)) end, Records),

    case mnesia_test_lib:diskless(Config) of
	true -> 
	    ?match(Nodes, mnesia:table_info(Tab, ram_copies));
	false ->              
	    ?match([Node3], mnesia:table_info(Tab, ext_ets)),
	    ?match([Node2], mnesia:table_info(Tab, ram_copies)),
	    ?match([Node1], mnesia:table_info(Tab, mnesia_test_lib:storage_type(disc_only_copies, Config)))
    end,
    Read = [Node1, Node2, Node3],
    Write = ?sort([Node1, Node2, Node3]),

    {[ok,ok,ok], []} = rpc:multicall(Nodes, ?MODULE, info_check,
				     [Tab, Read, Write, Size, Type, ValPos, Arity, Attrs]),

    ?match({atomic, Attrs}, mnesia:transaction(fun() -> mnesia:table_info(Tab, attributes) end)),

    ?match(L when is_list(L), mnesia:table_info(Tab, all)),

    %% Table info when table not loaded
    ?match({atomic, ok},
	   mnesia:create_table(tab_info, Schema)),
    ?match(stopped, mnesia:stop()),
    ?match(stopped, rpc:call(Node2, mnesia, stop, [])),
    ?match(ok, mnesia:start()),
    ?match(ok, mnesia:wait_for_tables([tab_info], 5000)),
    ?match(0, mnesia:table_info(tab_info, size)),
    ?verify_mnesia([Node1, Node3], [Node2]).

info_check(Tab, Read, Write, Size, Type, ValPos, Arity, Attrs) ->
    ?match(true, lists:member(mnesia:table_info(Tab, where_to_read), Read)),
    ?match(Write, ?sort(mnesia:table_info(Tab, where_to_write))),
    ?match(Mem when is_integer(Mem), mnesia:table_info(Tab, memory)),
    ?match(Size, mnesia:table_info(Tab, size)),
    ?match(Type, mnesia:table_info(Tab, type)),
    ?match([ValPos], mnesia:table_info(Tab, index)),
    ?match(Arity, mnesia:table_info(Tab, arity)),
    ?match(Attrs, mnesia:table_info(Tab, attributes)),
    ?match({Tab, '_', '_'}, mnesia:table_info(Tab, wild_pattern)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Check the error descriptions

error_description(suite) -> [];
error_description(Config) when is_list(Config) ->
    ?acquire_nodes(1, Config),
    Errors = [nested_transaction, badarg, no_transaction, combine_error,
              bad_index, already_exists, index_exists, no_exists, system_limit,
              mnesia_down, not_a_db_node, bad_type, node_not_running, 
              truncated_binary_file, active, illegal
             ],
    ?match(X when is_atom(X), mnesia:error_description({error, bad_error_msg})),
    ?match(X when is_tuple(X), mnesia:error_description({'EXIT', pid, bad})),
    %% This is real error msg
    ?match(X when is_tuple(X), mnesia:error_description(
                              {error, 
                               {"Cannot prepare checkpoint (bad reply)",
                                {{877,957351,758147},a@legolas},
                                {error,{node_not_running,a1@legolas}}}})),
    check_errors(error, Errors),
    check_errors(aborted, Errors),
    check_errors('EXIT', Errors).

check_errors(_Err, []) -> ok;
check_errors(Err, [Desc|R]) -> 
    ?match(X when is_list(X), mnesia:error_description({Err, Desc})),
    check_errors(Err, R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Add and drop db nodes

db_node_lifecycle(suite) -> [];
db_node_lifecycle(Config) when is_list(Config) ->
    [Node1, Node2, Node3] = AllNodes = ?acquire_nodes(3, Config),
    Tab = db_node_lifecycle,

    Who = fun(T) ->
		  L1 = mnesia:table_info(T, ram_copies),
		  L2 = mnesia:table_info(T, disc_copies),
		  L3 = mnesia:table_info(T, disc_only_copies),
		  L4 = mnesia:table_info(T, ext_ets),
		  L1 ++ L2 ++ L3 ++ L4
	  end,

    SNs = ?sort(AllNodes),

    Schema = [{name, Tab}, {ram_copies, [Node1, Node2]}],
    ?match({atomic, ok}, mnesia:create_table(Schema)),

    ?match([], mnesia_test_lib:stop_mnesia(AllNodes)),
    ?match(ok, mnesia:delete_schema(AllNodes)),
    ?match({error, _}, mnesia:create_schema(foo)),
    ?match({error, _}, mnesia:create_schema([foo])),
    ?match({error, _}, mnesia:create_schema([foo@bar])),
    ?match(ok, mnesia:start()),
    ?match(false, mnesia:system_info(use_dir)),
    ?match([ram_copies, disc_copies, disc_only_copies], mnesia:system_info(backend_types)),
    ?match({atomic, ok}, mnesia:create_table(Tab, [])),
    ?match({aborted, {has_no_disc, Node1}}, mnesia:dump_tables([Tab])),
    ?match({aborted, {has_no_disc, Node1}}, mnesia:change_table_copy_type(Tab, node(), disc_copies)),
    ?match({aborted, {has_no_disc, Node1}}, mnesia:change_table_copy_type(Tab, node(), disc_only_copies)),

    ?match(stopped, mnesia:stop()),

    ?match(ok, mnesia:create_schema(AllNodes, ?BACKEND)),
    ?match([], mnesia_test_lib:start_mnesia(AllNodes)),

    ?match([SNs, SNs, SNs], 
	   lists:map(fun lists:sort/1,
		     element(1, rpc:multicall(AllNodes, mnesia, table_info, 
					      [schema, disc_copies])))),

    ?match({aborted, {already_exists, schema, Node2, _}},
           mnesia:change_table_copy_type(schema, Node2, disc_copies)),
    ?match({atomic, ok},
           mnesia:change_table_copy_type(schema, Node2, ram_copies)),
    ?match({aborted, {already_exists, schema, Node2, _}},
           mnesia:change_table_copy_type(schema, Node2, ram_copies)),
    
    ?match({atomic, ok},
           mnesia:change_table_copy_type(schema, Node2, disc_copies)),

    ?match([SNs, SNs, SNs], 
	   lists:map(fun lists:sort/1,
		     element(1, rpc:multicall(AllNodes, mnesia, table_info, 
					      [schema, disc_copies])))),

    %% Delete the DB 

    Tab2 = disk_tab,
    Tab3 = not_local,
    Tab4 = local,
    Tab5 = remote,
    Tab6 = ext1,

    Tabs = [Schema,
	    [{name, Tab2},  {disc_copies, AllNodes}],
	    [{name, Tab3},  {ram_copies, [Node2, Node3]}],
	    [{name, Tab4},  {disc_only_copies, [Node1]}],
	    [{name, Tab5},  {disc_only_copies, [Node2]}],
	    [{name, Tab6},  {ext_ets, [Node1, Node2]}]
	   ],

    [?match({atomic, ok}, mnesia:create_table(T)) || T <- Tabs ],

    ?match({aborted, {active, _, Node2}}, 
	   mnesia:del_table_copy(schema, Node2)),

    ?match([], mnesia_test_lib:stop_mnesia([Node1])),
    ?match({aborted, {node_not_running, Node1}}, 
	   mnesia:del_table_copy(schema, Node2)),

    ?match([], mnesia_test_lib:start_mnesia([Node1],[Tab2,Tab4,Tab6])),
    ?match([], mnesia_test_lib:stop_mnesia([Node2])),
    ?match({atomic, ok}, mnesia:del_table_copy(schema, Node2)),

    %% Check
    RemNodes = AllNodes -- [Node2],

    ?match(RemNodes, mnesia:system_info(db_nodes)),
    ?match([Node1], Who(Tab)),
    ?match(RemNodes, Who(Tab2)),
    ?match([Node3], Who(Tab3)),
    ?match([Node1], Who(Tab4)),
    ?match({'EXIT', {aborted, {no_exists, _, _}}}, Who(Tab5)),
    ?match([Node1], Who(Tab6)),

    ?match({atomic, ok}, mnesia:change_table_copy_type(Tab2, Node3, ram_copies)),

    ?match({atomic, ok}, mnesia:change_table_copy_type(schema, Node3, ram_copies)),

    ?match([], mnesia_test_lib:stop_mnesia([Node3])),
    ?match({atomic, ok}, mnesia:del_table_copy(schema, Node3)),
    ?match([Node1], mnesia:system_info(db_nodes)),
    ?match([Node1], Who(Tab)),
    ?match([Node1], Who(Tab2)),
    ?match({'EXIT', {aborted, {no_exists, _, _}}}, Who(Tab3)),
    ?match([Node1], Who(Tab4)),
    ?match({'EXIT', {aborted, {no_exists, _, _}}}, Who(Tab5)),

    ?verify_mnesia([Node1], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Drop a db node when several disk resident nodes are down

evil_delete_db_node(suite) -> [];
evil_delete_db_node(Config) when is_list(Config) ->
    [Node1, Node2, Node3] = AllNodes = ?acquire_nodes(3, Config),
    Tab = evil_delete_db_node,

    ?match({atomic, ok}, mnesia:create_table(Tab, [{disc_copies, AllNodes}])),
    
    ?match([], mnesia_test_lib:stop_mnesia([Node2, Node3])),

    ?match({atomic, ok}, mnesia:del_table_copy(schema, Node2)),
    
    RemNodes = AllNodes -- [Node2],
    
    ?match(RemNodes, mnesia:system_info(db_nodes)),
    ?match(RemNodes, mnesia:table_info(Tab, disc_copies)),
    
    ?verify_mnesia([Node1], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Start and stop the system

start_and_stop(suite) -> [];
start_and_stop(Config) when is_list(Config) ->
    [Node1 | _] = Nodes = ?acquire_nodes(all, Config),

    ?match(stopped, rpc:call(Node1, mnesia, stop, [])),
    ?match(stopped, rpc:call(Node1, mnesia, stop, [])),
    ?match(ok, rpc:call(Node1, mnesia, start, [])),
    ?match(ok, rpc:call(Node1, mnesia, start, [])),
    ?match(stopped, rpc:call(Node1, mnesia, stop, [])),
    ?verify_mnesia(Nodes -- [Node1], [Node1]),
    ?match([], mnesia_test_lib:start_mnesia(Nodes)),
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Checkpoints and backup management

checkpoint(suite) -> [];
checkpoint(Config) when is_list(Config) ->
    checkpoint(2, Config),
    checkpoint(3, Config),
    ok.

checkpoint(NodeConfig, Config) ->
    [Node1 | _] = TabNodes = ?acquire_nodes(NodeConfig, Config),
    CreateTab = fun(Type, N, Ns) ->
                        Tab0 = lists:concat(["local_checkpoint_", Type, N]),
                        Tab = list_to_atom(Tab0),
                        catch mnesia:delete_table(Tab),
                        ?match({atomic, ok},
                               mnesia:create_table(Tab, [{Type, Ns}])),
                        Tab
                end,
    CreateTabs = fun(Type, Acc) ->
                         [CreateTab(Type, 1, [hd(TabNodes)]),
                          CreateTab(Type, 2, TabNodes),
                          CreateTab(Type, 3, [lists:last(TabNodes)])] ++
                             Acc
                 end,
    Types = [ram_copies, disc_copies, disc_only_copies, ext_ets],
    Tabs = lists:foldl(CreateTabs, [], Types),
    Recs = ?sort([{T, N, N} || T <- Tabs, N <- lists:seq(1, 10)]),
    lists:foreach(fun(R) -> ?match(ok, mnesia:dirty_write(R)) end, Recs),

    CpName = a_checkpoint_name,
    MinArgs = [{name, CpName}, {min, Tabs}, {allow_remote, false}],
    ?match({error, _}, rpc:call(Node1, mnesia, activate_checkpoint, [MinArgs])),

    MaxArgs = [{name, CpName}, {max, Tabs}, {allow_remote, true}],
    ?match({ok, CpName, L} when is_list(L),
	   rpc:call(Node1, mnesia, activate_checkpoint, [MaxArgs])),
    ?match(ok, rpc:call(Node1, mnesia, deactivate_checkpoint, [CpName])),

    Args = [{name, CpName}, {min, Tabs}, {allow_remote, true}],
    ?match({ok, CpName, L} when is_list(L),
           rpc:call(Node1, mnesia, activate_checkpoint, [Args])),
    Recs2 = ?sort([{T, K, 0} || {T, K, _} <- Recs]),
    lists:foreach(fun(R) -> ?match(ok, mnesia:dirty_write(R)) end, Recs2),
    ?match(ok, rpc:call(Node1, mnesia, deactivate_checkpoint, [CpName])),

    ?match({error, Reason1 } when element(1, Reason1) == no_exists,
           mnesia:deactivate_checkpoint(CpName)),
    ?match({error, Reason2 } when element(1, Reason2) == badarg,
           mnesia:activate_checkpoint(foo)),
    ?match({error, Reason3 } when element(1, Reason3) == badarg,
           mnesia:activate_checkpoint([{foo, foo}])),
    ?match({error, Reason4 } when element(1, Reason4) == badarg,
           mnesia:activate_checkpoint([{max, foo}])),
    ?match({error, Reason5 } when element(1, Reason5) == badarg,
           mnesia:activate_checkpoint([{min, foo}])),
    ?match({error, _}, mnesia:activate_checkpoint([{min, [foo@bar]}])),
    ?match({error, Reason6 } when element(1, Reason6) == badarg,
           mnesia:activate_checkpoint([{allow_remote, foo}])),

    Fun = fun(Tab) -> ?match({atomic, ok}, mnesia:delete_table(Tab)) end,
    lists:foreach(Fun, Tabs),
    ?verify_mnesia(TabNodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Create and delete tables

%% Get meta info about table

-define(vrl, mnesia_test_lib:verify_replica_location).

replica_location(suite) -> [];
replica_location(Config) when is_list(Config) ->
    [Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config),

    %% Create three replicas
    Check = fun(Tab, Schema) ->
		    ?match({atomic, ok}, mnesia:create_table([{name, Tab}|Schema])),
		    ?match([], ?vrl(Tab, [Node1], [Node2], [Node3], Nodes)),

		    %% Delete one replica
		    ?match({atomic, ok}, mnesia:del_table_copy(Tab, Node2)),
		    ?match([], ?vrl(Tab, [Node1], [], [Node3], Nodes)),

		    %% Move one replica
		    ?match({atomic, ok}, mnesia:move_table_copy(Tab, Node1, Node2)),
		    ?match([], ?vrl(Tab, [Node2], [], [Node3], Nodes)),

		    %% Change replica type
		    ?match({atomic, ok}, mnesia:change_table_copy_type(Tab, Node2, ram_copies)),
		    ?match([], ?vrl(Tab, [], [Node2], [Node3], Nodes))
	    end,
    Check(replica_location, [{disc_only_copies, [Node1]},
			     {ram_copies, [Node2]}, {disc_copies, [Node3]}]),

    Check(ext_location, [{disc_only_copies, [Node1]},
			 {ext_ets, [Node2]}, {disc_copies, [Node3]}]),

    ?verify_mnesia(Nodes, []).

table_lifecycle(suite) -> [];
table_lifecycle(Config) when is_list(Config) ->
    [Node1, Node2] = Nodes = ?acquire_nodes(2, Config),

    ?match({atomic, ok}, mnesia:create_table([{type, bag},
					      {ram_copies, [Node1]},
					      {attributes, [rajtan, tajtan]},
					      {name, order_of_args}])),
    ?match([], mnesia:dirty_read({order_of_args, 4711})),
    ?match({atomic, ok}, mnesia:create_table([{name, already_exists},
					      {ram_copies, [Node1]}])),
    ?match({aborted, Reason23 } when element(1, Reason23) ==already_exists,
	   mnesia:create_table([{name, already_exists},
				{ram_copies, [Node1]}])),
    ?match({aborted, Reason21 } when element(1, Reason21) == bad_type,
           mnesia:create_table([{name, bad_node}, {ram_copies, ["foo"]}])),
    ?match({aborted, Reason2} when element(1, Reason2) == bad_type,
           mnesia:create_table([{name, zero_arity}, {attributes, []}])),
    ?match({aborted, Reason3} when element(1, Reason3) == badarg,
           mnesia:create_table([])),
    ?match({aborted, Reason4} when element(1, Reason4) == badarg,
           mnesia:create_table(atom)),
    ?match({aborted, Reason5} when element(1, Reason5) == badarg,
           mnesia:create_table({cstruct, table_name_as_atom})),
    ?match({aborted, Reason6 } when element(1, Reason6) == bad_type,
           mnesia:create_table([{name, no_host}, {ram_copies, foo}])),
    ?match({aborted, Reason7 } when element(1, Reason7) == bad_type,
           mnesia:create_table([{name, no_host}, {disc_only_copies, foo}])),
    ?match({aborted, Reason8} when element(1, Reason8) == bad_type,
           mnesia:create_table([{name, no_host}, {disc_copies, foo}])),

    CreateFun =
        fun() -> ?match({aborted, nested_transaction},
                        mnesia:create_table([{name, nested_trans}])), ok
        end,
    ?match({atomic, ok}, mnesia:transaction(CreateFun)),
    ?match({atomic, ok}, mnesia:create_table([{name, remote_tab},
					      {ram_copies, [Node2]}])),

    ?match({atomic, ok}, mnesia:create_table([{name, a_brand_new_tab},
					      {ram_copies, [Node1]}])),
    ?match([], mnesia:dirty_read({a_brand_new_tab, 4711})),
    ?match({atomic, ok}, mnesia:delete_table(a_brand_new_tab)),
    ?match({'EXIT', {aborted, Reason31}} when element(1, Reason31) == no_exists,
           mnesia:dirty_read({a_brand_new_tab, 4711})),
    ?match({aborted, Reason41} when element(1, Reason41) == no_exists,
           mnesia:delete_table(a_brand_new_tab)),
    ?match({aborted, Reason9} when element(1, Reason9) == badarg,
           mnesia:create_table([])),

    ?match({atomic, ok}, mnesia:create_table([{name, nested_del_trans},
					      {ram_copies, [Node1]}])),

    DeleteFun = fun() -> ?match({aborted, nested_transaction},
                                mnesia:delete_table(nested_del_trans)), ok end,
    ?match({atomic, ok}, mnesia:transaction(DeleteFun)),

    ?match({aborted, Reason10} when element(1, Reason10) == bad_type,
           mnesia:create_table([{name, create_with_index}, {index, 2}])),
    ?match({aborted, Reason32} when element(1, Reason32) == bad_type,
           mnesia:create_table([{name, create_with_index}, {index, [-1]}])),
    ?match({aborted, Reason33} when element(1, Reason33) == bad_type,
           mnesia:create_table([{name, create_with_index}, {index, [0]}])),
    ?match({aborted, Reason34} when element(1, Reason34) == bad_type,
           mnesia:create_table([{name, create_with_index}, {index, [1]}])),
    ?match({aborted, Reason35} when element(1, Reason35) == bad_type,
           mnesia:create_table([{name, create_with_index}, {index, [2]}])),
    ?match({atomic, ok},
           mnesia:create_table([{name, create_with_index}, {index, [3]},
                                {ram_copies, [Node1]}])),

    ets:new(ets_table, [named_table]),
    ?match({aborted, _}, mnesia:create_table(ets_table, [{ram_copies, Nodes}])),
    ?match({aborted, _}, mnesia:create_table(ets_table, [{ram_copies, [Node1]}])),
    ets:delete(ets_table),
    ?match({atomic, ok}, mnesia:create_table(ets_table, [{ram_copies, [Node1]}])),
    ?match(Node1, rpc:call(Node1, mnesia_lib, val, [{ets_table,where_to_read}])),
    ?match(Node1, rpc:call(Node2, mnesia_lib, val, [{ets_table,where_to_read}])),
    ?match({atomic, ok}, mnesia:change_table_copy_type(ets_table, Node1, disc_only_copies)),    
    ?match(Node1, rpc:call(Node2, mnesia_lib, val, [{ets_table,where_to_read}])),
    
    ?verify_mnesia(Nodes, []).


storage_options(suite) -> [];
storage_options(Config) when is_list(Config) ->
    [N1,N2,N3] = Nodes = ?acquire_nodes(3, Config),

    ?match({aborted,_}, mnesia:create_table(a, [{storage_properties, [{ets,foobar}]}])),
    ?match({aborted,_}, mnesia:create_table(a, [{storage_properties, [{ets,[foobar]}]}])),
    ?match({aborted,_}, mnesia:create_table(a, [{storage_properties, [{ets,[duplicate_bag]}]}])),
    ?match({aborted,_}, mnesia:create_table(a, [{storage_properties, [{dets,[{type,bag}]}]}])),

    ?match({atomic, ok}, mnesia:create_table(a, [{ram_copies, [N1]},
						 {disc_only_copies, [N2]},
						 {storage_properties,
						  [{ets,[compressed]},
						   {dets, [{auto_save, 5000}]} ]}])),
    ?match(true, ets:info(a, compressed)),
    ?match(5000, rpc:call(N2, dets, info, [a, auto_save])),
    ?match(ok, mnesia:dirty_write({a,1,1})),
    ?match([{a,1,1}], mnesia:dirty_read({a,1})),
    mnesia:dump_log(),
    W2C1 = [{N2, disc_only_copies}, {N1, ram_copies}],
    ?match(W2C1, lists:sort(rpc:call(N2, mnesia_lib, val, [{a, where_to_commit}]))),
    ?match(W2C1, lists:sort(rpc:call(N3, mnesia_lib, val, [{a, where_to_commit}]))),
    ?match({atomic,ok}, mnesia:change_table_copy_type(a, N1, disc_only_copies)),
    W2C2 = [{N2, disc_only_copies}, {N1, disc_only_copies}],
    ?match(W2C2, lists:sort(rpc:call(N2, mnesia_lib, val, [{a, where_to_commit}]))),
    ?match(W2C2, lists:sort(rpc:call(N3, mnesia_lib, val, [{a, where_to_commit}]))),
    ?match(undefined, ets:info(a, compressed)),
    ?match(5000, dets:info(a, auto_save)),
    ?match({atomic,ok}, mnesia:change_table_copy_type(a, N1, disc_copies)),
    ?match(true, ets:info(a, compressed)),

    ?verify_mnesia(Nodes, []).


clear_table_during_load(suite) -> [];
clear_table_during_load(doc) ->
    ["Clear table caused during load caused a schema entry in the actual tab"];
clear_table_during_load(Config) when is_list(Config) ->
    Nodes = [_, Node2] = ?acquire_nodes(2, Config ++ [{tc_timeout, timer:minutes(2)}]),
    ?match({atomic,ok}, mnesia:create_table(cleartab, [{ram_copies, Nodes}])),
    Tester = self(),
    Bin = <<"Testingasdasd", 0:32000>>,
    Fill =  fun() -> [mnesia:write({cleartab, N, Bin}) || N <- lists:seq(1, 3000)], ok end,
    ?match({atomic, ok}, mnesia:sync_transaction(Fill)),

    StopAndStart = fun() ->
			   stopped = mnesia:stop(),
			   Tester ! {self(), stopped},
			   receive start_node -> ok end,
			   ok = mnesia:start(),
			   ok = mnesia:wait_for_tables([cleartab], 2000),
			   lists:foreach(fun({cleartab,_,_}) -> ok;
					    (What) -> Tester ! {failed, What},
						      unlink(Tester),
						      exit(foo)
					 end,
					 ets:tab2list(cleartab)),
			   Tester ! {self(), ok},
			   normal
		   end,

    Test = fun(N) ->
		   Pid = spawn_link(Node2, StopAndStart),
		   receive {Pid, stopped} -> ok end,
		   Pid ! start_node,
		   timer:sleep(N*10),
		   {atomic, ok} = mnesia:clear_table(cleartab),
		   receive
		       {Pid, ok} -> ok;
		       {failed, What} ->
			   io:format("Failed in ~p tries, with ~p~n",[N, What]),
			   exit({error, What});
		       {'EXIT', Pid, Reason} ->
			   exit({died, Reason})
		   end
	   end,
    [Test(N) || N <- lists:seq(1, 10)],
    ?verify_mnesia(Nodes, []).


add_copy_conflict(suite) -> [];
add_copy_conflict(doc) -> 
    ["Verify that OTP-5065 doesn't happen again, whitebox testing"];
add_copy_conflict(Config) when is_list(Config) ->
    Nodes = [Node1, Node2] = 
	?acquire_nodes(2, Config ++ [{tc_timeout, timer:minutes(2)}]),
    
    ?match({atomic, ok}, mnesia:create_table(a, [{ram_copies, Nodes}])),
    ?match({atomic, ok}, mnesia:create_table(b, [{ram_copies, Nodes}])),
    ?match({atomic, ok}, mnesia:create_table(test, [{ram_copies, [Node2]}])),            
    mnesia:stop(),
    ?match(ok,mnesia:start([{no_table_loaders, 1}])),

    verify_ll_queue(10),

    Self = self(),
    Test = fun() ->
		   Res = mnesia:add_table_copy(test, Node1, ram_copies),
		   Self ! {test, Res}
	   end,
    %% Create conflict with loader queue.
    spawn_link(Test),
    ?match_receive(timeout),
    %% Conflict ok
    mnesia_controller:unblock_controller(),
    
    ?match_receive({test, {atomic,ok}}),
    ?match(ok, mnesia:wait_for_tables([a,b], 3000)),
    ?verify_mnesia(Nodes, []),
    ?cleanup(1, Config).

verify_ll_queue(0) ->
    ?error("Couldn't find anything in queue~n",[]);
verify_ll_queue(N) ->
    ?match(granted,mnesia_controller:block_controller()),
    case mnesia_controller:get_info(1000) of
	{info,{state,_,true,[],_Loader,[],[],[],_,_,_,_,_,_}} ->
	    %% Very slow SMP machines havn't loaded it yet..
	    mnesia_controller:unblock_controller(),
	    timer:sleep(10),
	    verify_ll_queue(N-1);
	{info,{state,_,true,[],Loader,LL,[],[],_,_,_,_,_,_}} ->
	    io:format("~p~n", [{Loader,LL}]),
	    ?match([_], LL);  %% Verify that something is in the loader queue
	Else ->
	    ?error("No match ~p maybe the internal format has changed~n",[Else])
    end.

add_copy_when_going_down(suite) -> [];
add_copy_when_going_down(doc) -> 
    ["Tests abort when node we load from goes down"];
add_copy_when_going_down(Config) -> 
    [Node1, Node2] = 
	?acquire_nodes(2, Config ++ [{tc_timeout, timer:minutes(2)}]),
    ?match({atomic, ok}, mnesia:create_table(a, [{ram_copies, [Node1]}])),
    %% Grab a write lock 
    Tester = self(),
    WriteAndWait = fun() -> 
			   mnesia:write({a,1,1}),
			   Tester ! {self(), got_lock},
			   receive continue -> ok
			   end
		   end,
    Locker = spawn(fun() -> mnesia:transaction(WriteAndWait) end),
    receive {Locker, got_lock} -> ok end,

    spawn_link(fun() -> Res = rpc:call(Node2, mnesia, add_table_copy,
				       [a, Node2, ram_copies]),
			Tester ! {test, Res}
	       end),
    %% We have a lock here we should get a timeout
    ?match_receive(timeout),
    mnesia_test_lib:kill_mnesia([Node1]),
    ?match_receive({test,{aborted,_}}),
    ?verify_mnesia([Node2], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Add, drop and move replicas, change storage types
%% Change table layout (only arity change supported)

-record(replica_management, {k, v}).
-record(new_replica_management, {k, v, extra}).

-define(SS(R), lists:sort(element(1,R))).

replica_management(doc) ->
    "Add, drop and move replicas, change storage types.";
replica_management(suite) ->
    [];
replica_management(Config) when is_list(Config) ->
    %% add_table_copy/3, del_table_copy/2, move_table_copy/3,
    %% change_table_copy_type/3, transform_table/3

    Nodes = [Node1, Node2, Node3] = ?acquire_nodes(3, Config),

    Tab = replica_management,
    Attrs = record_info(fields, replica_management),

    %%
    %% Add, delete and change replicas
    %%
    ?match({atomic, ok},
           mnesia:create_table([{name, Tab}, {attributes, Attrs},
                                {ram_copies, [Node1]}, {ext_ets, [Node3]}])),
    [?match(ok, mnesia:dirty_write({Tab, K, K + 2})) || K <-lists:seq(1, 10)],
    ?match([], ?vrl(Tab, [], [Node1, Node3], [], Nodes)),
    %% R - -
    ?match({atomic, ok}, mnesia:dump_tables([Tab])),
    ?match({aborted,  Reason50 } when element(1, Reason50) == combine_error,
	   mnesia:add_table_copy(Tab, Node2, disc_copies)),
    ?match({aborted,  Reason51 } when element(1, Reason51) == combine_error,
           mnesia:change_table_copy_type(Tab, Node1, disc_copies)),
    ?match({atomic, ok}, mnesia:clear_table(Tab)),
    SyncedCheck = fun() ->
			  mnesia:lock({record,Tab,0}, write),
			  ?match([0,0,0], ?SS(rpc:multicall(Nodes, mnesia, table_info, [Tab, size])))
		  end,
    mnesia:transaction(SyncedCheck),

    ?match({[0,0,0], []}, rpc:multicall(Nodes, mnesia, table_info, [Tab, size])),
    ?match({atomic, ok}, mnesia:del_table_copy(Tab, Node1)),
    ?match({atomic, ok}, mnesia:del_table_copy(Tab, Node3)),
    ?match([], ?vrl(Tab, [], [], [], Nodes)),
    %% - - -
    ?match({aborted,Reason52} when element(1, Reason52) ==  no_exists,
           mnesia:add_table_copy(Tab, Node3, ram_copies)),

    ?match({atomic, ok}, mnesia:create_table([{name, Tab},
					      {attributes, Attrs},
					      {disc_copies, [Node1]}])),
    ?match([], ?vrl(Tab, [], [], [Node1], Nodes)),
    %% D - -
    [?match(ok, mnesia:dirty_write({Tab, K, K + 2})) || K <-lists:seq(1, 10)],

    ?match({aborted,  Reason53} when element(1, Reason53) == badarg,
           mnesia:add_table_copy(Tab, Node2, bad_storage_type)),
    ?match({atomic, ok}, mnesia:add_table_copy(Tab, Node2, disc_only_copies)),
    ?match([], ?vrl(Tab, [Node2], [], [Node1], Nodes)),
    ?match([0,10,10], ?SS(rpc:multicall(Nodes, mnesia, table_info, [Tab, size]))),
    %% D DO -
    ?match({atomic, ok}, mnesia:add_table_copy(Tab, Node3, ext_ets)),
    ?match([], ?vrl(Tab, [Node2], [Node3], [Node1], Nodes)),
    ?match([10,10,10], ?SS(rpc:multicall(Nodes, mnesia, table_info, [Tab, size]))),
    %% D DO R
    ?match({atomic, ok},
           mnesia:change_table_copy_type(Tab, Node1, disc_only_copies)),
    ?match([], ?vrl(Tab, [Node1, Node2], [Node3], [], Nodes)),
    ?match([10,10,10], ?SS(rpc:multicall(Nodes, mnesia, table_info, [Tab, size]))),
    %% DO DO R
    ?match({aborted,  Reason54} when element(1, Reason54) == already_exists,
           mnesia:add_table_copy(Tab, Node3, ram_copies)),
    ?match({atomic, ok}, mnesia:del_table_copy(Tab, Node1)),
    ?match([], ?vrl(Tab, [Node2], [Node3], [], Nodes)),
    %% - DO R
    ?match({aborted, _}, mnesia:del_table_copy(Tab, Node1)),
    ?match(Tab, ets:new(Tab, [named_table])),
    ?match({aborted, _}, mnesia:add_table_copy(Tab, Node1, disc_copies)),
    ?match(true, ets:delete(Tab)),
    ?match({atomic, ok}, mnesia:add_table_copy(Tab, Node1, disc_copies)),
    ?match([], ?vrl(Tab, [Node2], [Node3], [Node1], Nodes)),
    ?match([10,10,10], ?SS(rpc:multicall(Nodes, mnesia, table_info, [Tab, size]))),
    %% D DO R
    ?match({atomic, ok},mnesia:change_table_copy_type(Tab, Node3, disc_only_copies)),
    ?match([], ?vrl(Tab, [Node2, Node3], [], [Node1], Nodes)),
    ?match([10,10,10], ?SS(rpc:multicall(Nodes, mnesia, table_info, [Tab, size]))),

    %% D DO D0
    ?match({atomic, ok}, mnesia:change_table_copy_type(Tab, Node3, ext_ets)),
    ?match([], ?vrl(Tab, [Node2], [Node3], [Node1], Nodes)),
    ?match([10,10,10], ?SS(rpc:multicall(Nodes, mnesia, table_info, [Tab, size]))),
    %% D DO R
    ?match({atomic, ok},
           mnesia:change_table_copy_type(Tab, Node2, disc_copies)),
    ?match([], ?vrl(Tab, [], [Node3], [Node1,Node2], Nodes)),
    ?match([10,10,10], ?SS(rpc:multicall(Nodes, mnesia, table_info, [Tab, size]))),

    %% D D R
    ?match({atomic, ok}, mnesia:change_table_copy_type(Tab, Node1, disc_only_copies)),
    ?match([], ?vrl(Tab, [Node1], [Node3], [Node2], Nodes)),
    ?match([10,10,10], ?SS(rpc:multicall(Nodes, mnesia, table_info, [Tab, size]))),

    %% DO D R
    ?match(Tab, ets:new(Tab, [named_table])),
    ?match({aborted, _}, mnesia:change_table_copy_type(Tab, Node1, ram_copies)),
    ?match(true, ets:delete(Tab)),
    ?match({atomic, ok}, mnesia:change_table_copy_type(Tab, Node1, ram_copies)),
    ?match([], ?vrl(Tab, [], [Node3,Node1], [Node2], Nodes)),
    ?match([10,10,10], ?SS(rpc:multicall(Nodes, mnesia, table_info, [Tab, size]))),
    %% R D R
    ?match({atomic, ok}, mnesia:change_table_copy_type(Tab, Node1, disc_copies)),
    ?match([], ?vrl(Tab, [], [Node3], [Node2,Node1], Nodes)),
    ?match([10,10,10], ?SS(rpc:multicall(Nodes, mnesia, table_info, [Tab, size]))),

    %% D D R
    ?match({atomic, ok}, mnesia:change_table_copy_type(Tab, Node2, disc_only_copies)),
    ?match([], ?vrl(Tab, [Node2], [Node3], [Node1], Nodes)),
    ?match([10,10,10], ?SS(rpc:multicall(Nodes, mnesia, table_info, [Tab, size]))),

    %% D DO R
    ?match({atomic, ok}, mnesia:change_table_copy_type(Tab, Node3, disc_only_copies)),
    ?match([], ?vrl(Tab, [Node2, Node3], [], [Node1], Nodes)),
    ?match([10,10,10], ?SS(rpc:multicall(Nodes, mnesia, table_info, [Tab, size]))),
    %% D DO DO
    %% test clear
    ?match({atomic, ok}, mnesia:clear_table(Tab)),
    mnesia:transaction(SyncedCheck),

    %% rewrite for rest of testcase
    [?match(ok, mnesia:dirty_write({Tab, K, K + 2})) || K <-lists:seq(1, 10)],

    %% D DO DO
    ?match({atomic, ok}, mnesia:del_table_copy(Tab, Node2)),
    ?match([], ?vrl(Tab, [Node3], [], [Node1], Nodes)),
    %% D - DO
    ?match({aborted,  Reason55} when element(1, Reason55) == already_exists,
           mnesia:change_table_copy_type(Tab, Node1, disc_copies)),

    %%
    %% Move replica
    %%
    ?match({atomic, ok}, mnesia:move_table_copy(Tab, Node1, Node2)),
    ?match([], ?vrl(Tab, [Node3], [], [Node2], Nodes)),
    ?match([0,10,10], ?SS(rpc:multicall(Nodes, mnesia, table_info, [Tab, size]))),
    %% - D DO
    ?match({atomic, ok}, mnesia:change_table_copy_type(Tab, Node3, ext_ets)),
    ?match([], ?vrl(Tab, [], [Node3], [Node2], Nodes)),
    ?match([0,10,10], ?SS(rpc:multicall(Nodes, mnesia, table_info, [Tab, size]))),
    %% - D ER
    ?match({atomic, ok}, mnesia:move_table_copy(Tab, Node3, Node1)),
    ?match([], ?vrl(Tab, [], [Node1], [Node2], Nodes)),
    ?match([0,10,10], ?SS(rpc:multicall(Nodes, mnesia, table_info, [Tab, size]))),
    %% ER D -

    ?match({aborted, _}, mnesia:move_table_copy(Tab, Node1, Node2)),
    ?match({aborted, _}, mnesia:move_table_copy(Tab, Node3, Node2)),
    ?match({atomic, ok}, mnesia:move_table_copy(Tab, Node1, Node3)),
    %% - D ER
    ?match([], mnesia_test_lib:stop_mnesia([Node3])),
    ?match({atomic,ok}, mnesia:transaction(fun() -> mnesia:write({Tab, 43, sync_me}) end)),
    ?match([], ?vrl(Tab, [], [Node3], [Node2],Nodes -- [Node3])),
    %% - D ER
    ?match({aborted,Reason56} when element(1, Reason56) == not_active,
	   mnesia:move_table_copy(Tab, Node3, Node1)),
    ?match([], ?vrl(Tab, [], [Node3], [Node2],Nodes -- [Node3])),
    %% - D ER
    ?match([], mnesia_test_lib:start_mnesia([Node3])),
    ?match([], ?vrl(Tab, [], [Node3], [Node2], Nodes)),
    %% - D ER
    ?match([{Tab,43,sync_me}], mnesia:dirty_read({Tab,43})),
    
    %%
    %% Transformer
    %%

    NewAttrs = record_info(fields, new_replica_management),
    Transformer =
        fun(Rec) when is_record(Rec, replica_management) -> 
		#new_replica_management{k = Rec#replica_management.k,
					v = Rec#replica_management.v,
					extra = Rec#replica_management.k * 2}
        end,
    ?match({atomic, ok}, mnesia:transform_table(Tab, fun(R) -> R end, Attrs)),
    ?match({atomic, ok}, mnesia:transform_table(Tab, Transformer, NewAttrs, new_replica_management)),

    ERlist = [#new_replica_management{k = K, v = K+2, extra = K*2} || K <- lists:seq(1, 10)],
    ARlist = [hd(mnesia:dirty_read(Tab, K)) || K <- lists:seq(1, 10)],

    ?match(ERlist, ARlist),

    ?match({aborted,  Reason56} when element(1, Reason56) == bad_type,
           mnesia:transform_table(Tab, Transformer, 0)),
    ?match({aborted, Reason57} when element(1, Reason57) ==  bad_type,
           mnesia:transform_table(Tab, Transformer, -1)),
    ?match({aborted, Reason58} when element(1, Reason58) ==  bad_type,
           mnesia:transform_table(Tab, Transformer, [])),
    ?match({aborted,  Reason59} when element(1, Reason59) == bad_type,
           mnesia:transform_table(Tab, no_fun, NewAttrs)),
    ?match({aborted,  Reason59} when element(1, Reason59) == bad_type,
           mnesia:transform_table(Tab, fun(X) -> X end, NewAttrs, {tuple})),

    %% OTP-3878 
    ?match({atomic, ok}, mnesia:transform_table(Tab, ignore, 
                                                NewAttrs ++ [dummy])),
    ?match({atomic, ok}, mnesia:transform_table(Tab, ignore, 
                                                NewAttrs ++ [dummy], last_rec)),

    ARlist = [hd(mnesia:dirty_read(Tab, K)) || K <- lists:seq(1, 10)],
    ?match({'EXIT',{aborted,{bad_type,_}}},
           mnesia:dirty_write(Tab, #new_replica_management{})),
    ?match(ok, mnesia:dirty_write(Tab, {last_rec, k, v, e, dummy})),

    ?verify_mnesia(Nodes, []).

schema_availability(doc) ->
    ["Test that schema succeeds (or fails) as intended when some db nodes are down."];
schema_availability(suite) ->
    [];
schema_availability(Config) when is_list(Config) ->
    [N1, _N2, N3] = Nodes = ?acquire_nodes(3, Config),
    Tab = schema_availability,
    Storage = mnesia_test_lib:storage_type(ram_copies, Config),
    Def1 = [{Storage, [N1, N3]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def1)),

    N = 10,
    ?match(ok, mnesia:sync_dirty(fun() -> [mnesia:write({Tab, K, K + 2}) || K <- lists:seq(1, N)], ok end)),
    ?match({[N,0,N], []}, rpc:multicall(Nodes, mnesia, table_info, [Tab, size])),
    ?match([], mnesia_test_lib:kill_mnesia([N3])),
    ?match({[N,0,0], []}, rpc:multicall(Nodes, mnesia, table_info, [Tab, size])),

    ?match([], mnesia_test_lib:start_mnesia([N3], [Tab])),
    ?match({[N,0,N], []}, rpc:multicall(Nodes, mnesia, table_info, [Tab, size])),
    ?match([], mnesia_test_lib:kill_mnesia([N3])),

    ?match({atomic, ok}, mnesia:clear_table(Tab)),
    ?match({[0,0,0], []}, rpc:multicall(Nodes, mnesia, table_info, [Tab, size])),

    ?match([], mnesia_test_lib:start_mnesia([N3], [Tab])),
    ?match({[0,0,0], []}, rpc:multicall(Nodes, mnesia, table_info, [Tab, size])),

    ?verify_mnesia(Nodes, []).

-define(badrpc(Tab), {badrpc, {'EXIT', {aborted,{no_exists,Tab}}}}).

local_content(doc) ->
    ["Test local_content functionality, we want to see that correct"
     " properties gets propageted correctly between nodes"];
local_content(suite) -> [];
local_content(Config) when is_list(Config) ->
    [Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config), 
    Tab1 = local1,
    Def1 = [{local_content, true}, {ram_copies, Nodes}],
    Tab2 = local2,
    Def2 = [{local_content, true}, {disc_copies, [Node1]}],
    Tab3 = local3,
    Def3 = [{local_content, true}, {disc_only_copies, [Node1]}],
    Tab4 = local4,
    Def4 = [{local_content, true}, {ram_copies, [Node1]}],

    ?match({atomic, ok}, mnesia:create_table(Tab1, Def1)),
    ?match({atomic, ok}, mnesia:create_table(Tab2, Def2)),
    ?match({atomic, ok}, mnesia:create_table(Tab3, Def3)),
    ?match({atomic, ok}, mnesia:create_table(Tab4, Def4)),

    ?match(ok, rpc:call(Node1, mnesia, dirty_write, [{Tab1, 1, Node1}])),
    ?match(ok, rpc:call(Node2, mnesia, dirty_write, [{Tab1, 1, Node2}])),
    ?match(ok, rpc:call(Node3, mnesia, dirty_write, [{Tab1, 1, Node3}])),
    ?match(ok, rpc:call(Node1, mnesia, dirty_write, [{Tab2, 1, Node1}])),
    ?match(ok, rpc:call(Node1, mnesia, dirty_write, [{Tab3, 1, Node1}])),
    ?match(ok, rpc:call(Node1, mnesia, dirty_write, [{Tab4, 1, Node1}])),

    ?match(?badrpc(Tab2), rpc:call(Node2, mnesia, dirty_write, [{Tab2, 1, Node2}])),
    ?match(?badrpc(Tab3), rpc:call(Node2, mnesia, dirty_write, [{Tab3, 1, Node2}])),
    ?match(?badrpc(Tab4), rpc:call(Node2, mnesia, dirty_write, [{Tab4, 1, Node2}])),

    ?match({atomic, ok}, rpc:call(Node1, mnesia, add_table_copy, [Tab2, Node2, ram_copies])),    
    ?match({atomic, ok}, rpc:call(Node2, mnesia, add_table_copy, [Tab3, Node2, disc_copies])),
    ?match({atomic, ok}, rpc:call(Node3, mnesia, add_table_copy, [Tab4, Node2, disc_only_copies])),
    ?match([], rpc:call(Node2, mnesia, dirty_read, [{Tab2, 1}])),
    ?match([], rpc:call(Node2, mnesia, dirty_read, [{Tab3, 1}])),
    ?match([], rpc:call(Node2, mnesia, dirty_read, [{Tab4, 1}])),

    ?match(ok, rpc:call(Node2, mnesia, dirty_write, [{Tab2, 1, Node2}])),
    ?match(ok, rpc:call(Node2, mnesia, dirty_write, [{Tab3, 1, Node2}])),
    ?match(ok, rpc:call(Node2, mnesia, dirty_write, [{Tab4, 1, Node2}])),

    ?match([{Tab1, 1, Node1}], rpc:call(Node1, mnesia, dirty_read, [{Tab1, 1}])),
    ?match([{Tab2, 1, Node1}], rpc:call(Node1, mnesia, dirty_read, [{Tab2, 1}])),
    ?match([{Tab3, 1, Node1}], rpc:call(Node1, mnesia, dirty_read, [{Tab3, 1}])),
    ?match([{Tab4, 1, Node1}], rpc:call(Node1, mnesia, dirty_read, [{Tab4, 1}])),

    ?match([{Tab1, 1, Node2}], rpc:call(Node2, mnesia, dirty_read, [{Tab1, 1}])),
    ?match([{Tab2, 1, Node2}], rpc:call(Node2, mnesia, dirty_read, [{Tab2, 1}])),
    ?match([{Tab3, 1, Node2}], rpc:call(Node2, mnesia, dirty_read, [{Tab3, 1}])),
    ?match([{Tab4, 1, Node2}], rpc:call(Node2, mnesia, dirty_read, [{Tab4, 1}])),

    ?match([{Tab1, 1, Node3}], rpc:call(Node3, mnesia, dirty_read, [{Tab1, 1}])),
    ?match(?badrpc([_Tab2, 1]), rpc:call(Node3, mnesia, dirty_read, [{Tab2, 1}])),
    ?match(?badrpc([_Tab3, 1]), rpc:call(Node3, mnesia, dirty_read, [{Tab3, 1}])),
    ?match(?badrpc([_Tab4, 1]), rpc:call(Node3, mnesia, dirty_read, [{Tab4, 1}])),

    ?match({atomic, ok}, 
	   mnesia:change_table_copy_type(schema, Node3, ram_copies)),
    ?match([], mnesia_test_lib:stop_mnesia([Node3])),

    %% Added for OTP-44306
    ?match(ok, rpc:call(Node3, mnesia, start, [[{schema, ?BACKEND}]])),
    ?match({ok, _}, mnesia:change_config(extra_db_nodes, [Node3])),

    mnesia_test_lib:sync_tables([Node3], [Tab1]),

    ?match([], rpc:call(Node3, mnesia, dirty_read, [{Tab1, 1}])),

    ?match({atomic, ok}, rpc:call(Node1, mnesia, clear_table, [Tab1])),
    
    SyncedCheck = fun(Tab) ->
			  mnesia:lock({record,Tab,0}, write),
			  {OK, []} = rpc:multicall(Nodes, mnesia, table_info, [Tab, size]),
			  OK
		  end,
    ?match({atomic, [0,1,0]}, mnesia:transaction(SyncedCheck, [Tab1])),
    ?match({atomic, ok}, rpc:call(Node2, mnesia, clear_table, [Tab2])),    
    ?match({atomic, [1,0,0]}, mnesia:transaction(SyncedCheck, [Tab2])),
    ?match({atomic, ok}, rpc:call(Node2, mnesia, clear_table, [Tab3])),    
    ?match({atomic, [1,0,0]}, mnesia:transaction(SyncedCheck, [Tab3])),

    ?verify_mnesia(Nodes, []).


change_table_access_mode(suite) -> [];
change_table_access_mode(Config) when is_list(Config) -> 
    [Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config),
    Tab = test_access_mode_tab,

    Def = case  mnesia_test_lib:diskless(Config) of
	      true -> [{name, Tab}, {ram_copies, Nodes}];
	      false -> [{name, Tab}, {ram_copies, [Node1]}, 
			{disc_copies, [Node2]},
			{disc_only_copies, [Node3]}]
	  end,
    ?match({atomic, ok}, mnesia:create_table(Def)),

    Write = fun(What) -> mnesia:write({Tab, 1, What}) end,
    Read = fun() -> mnesia:read({Tab, 1}) end,

    ?match({atomic, ok}, mnesia:transaction(Write, [test_ok])),
    %% test read_only
    ?match({atomic, ok}, mnesia:change_table_access_mode(Tab, read_only)),
    ?match({aborted, _}, mnesia:transaction(Write, [nok])),
    ?match({'EXIT', {aborted, _}}, mnesia:dirty_write({Tab, 1, [nok]})),
    ?match({aborted, _}, rpc:call(Node2, mnesia, transaction, [Write, [nok]])),
    ?match({aborted, _}, rpc:call(Node3, mnesia, transaction, [Write, [nok]])),
    ?match({atomic, [{Tab, 1, test_ok}]}, mnesia:transaction(Read)),
    %% test read_write
    ?match({atomic, ok}, mnesia:change_table_access_mode(Tab, read_write)),
    ?match({atomic, ok}, mnesia:transaction(Write, [test_ok1])),
    ?match({atomic, [{Tab, 1, test_ok1}]}, mnesia:transaction(Read)),
    ?match({atomic, ok}, rpc:call(Node2, mnesia, transaction, [Write, [test_ok2]])),
    ?match({atomic, [{Tab, 1, test_ok2}]}, mnesia:transaction(Read)),
    ?match({atomic, ok}, rpc:call(Node3, mnesia, transaction, [Write, [test_ok3]])),
    ?match({atomic, [{Tab, 1, test_ok3}]}, mnesia:transaction(Read)),

    ?match({atomic, ok}, mnesia:delete_table(Tab)),

    Def4 = [{name, Tab}, {access_mode, read_only_bad}],
    ?match({aborted, {bad_type, _, _}}, mnesia:create_table(Def4)),

    Def2 = [{name, Tab}, {access_mode, read_only}],
    ?match({atomic, ok}, mnesia:create_table(Def2)),
    ?match({aborted, _}, mnesia:transaction(Write, [nok])),

    ?match({atomic, ok}, mnesia:change_table_access_mode(Tab, read_write)),
    ?match({atomic, ok}, mnesia:delete_table(Tab)),

    Def3 = [{name, Tab}, {mnesia_test_lib:storage_type(disc_copies, Config),
			  [Node1, Node2]},
	    {access_mode, read_write}],
    ?match({atomic, ok}, mnesia:create_table(Def3)),
    ?match({atomic, ok}, mnesia:transaction(Write, [ok])),
    ?match({atomic, ok}, mnesia:change_table_access_mode(Tab, read_only)),
    ?match({aborted, _}, mnesia:del_table_copy(Tab, Node2)),
    ?match({aborted, _}, mnesia:del_table_copy(Tab, Node1)),
    ?match({aborted, _}, mnesia:delete_table(Tab)),
    ?match({atomic, ok}, mnesia:change_table_access_mode(Tab, read_write)),

    ?match({aborted, {bad_type, _, _}},
	   mnesia:change_table_access_mode(Tab, strange_atom)),
    ?match({atomic, ok}, mnesia:delete_table(Tab)),

    ?match({aborted, {no_exists, _}}, 
	   mnesia:change_table_access_mode(err_tab, read_only)),
    ?match({aborted, {no_exists, _}}, 
	   mnesia:change_table_access_mode([Tab], read_only)),
    ?verify_mnesia(Nodes, []).

change_table_load_order(suite) -> [];
change_table_load_order(Config) when is_list(Config) ->
    [Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config),
    Tab1 = load_order_tab1,
    Tab2 = load_order_tab2,
    Tab3 = load_order_tab3,

    Def = case mnesia_test_lib:diskless(Config) of
	      true -> [{ram_copies, Nodes}]; 
	      false ->		  
		  [{ram_copies, [Node1]}, 
		   {disc_copies, [Node2]},
		   {disc_only_copies, [Node3]}]
	  end,
    ?match({atomic, ok}, mnesia:create_table(Tab1, Def)),
    ?match({atomic, ok}, mnesia:create_table(Tab2, Def)),
    ?match({atomic, ok}, mnesia:create_table(Tab3, Def)),

    ?match({aborted, _}, mnesia:change_table_load_order(Tab1, should_be_integer)),       
    ?match({aborted, _}, mnesia:change_table_load_order(err_tab, 5)),       
    ?match({aborted, _}, mnesia:change_table_load_order([err_tab], 5)),       
    ?match({atomic, ok}, mnesia:change_table_load_order(Tab1, 5)),       
    ?match({atomic, ok}, mnesia:change_table_load_order(Tab2, 4)),    
    ?match({atomic, ok}, mnesia:change_table_load_order(Tab3, 73)),

    ?match({aborted, _}, mnesia:change_table_load_order(schema, -32)),   

    ?verify_mnesia(Nodes, []).

set_master_nodes(suite) -> [];
set_master_nodes(Config) when is_list(Config) ->
    [Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config),
    Tab1 = master_node_tab1,
    Tab2 = master_node_tab2,
    Tab3 = master_node_tab3,
    Def1 = [{ram_copies, [Node1, Node2]}],
    Def2 = [{disc_copies, [Node2, Node3]}],
    Def3 = [{disc_only_copies, [Node3, Node1]}],
    ?match({atomic, ok}, mnesia:create_table(Tab1, Def1)),
    ?match({atomic, ok}, mnesia:create_table(Tab2, Def2)),
    ?match({atomic, ok}, mnesia:create_table(Tab3, Def3)),

    ?match({error, _}, mnesia:set_master_nodes(schema, ['hopefully@non.existing.node'])),
    ?match({error, _}, mnesia:set_master_nodes(badtab, [Node2, Node3])),
    ?match({error, _}, mnesia:set_master_nodes(Tab1, [Node3])),
    ?match([], mnesia:table_info(Tab1, master_nodes)),

    ?match(ok, mnesia:set_master_nodes(schema, [Node3, Node1])),
    ?match([Node3, Node1], mnesia:table_info(schema, master_nodes)),
    ?match(ok, mnesia:set_master_nodes(Tab1, [Node2])),
    ?match([Node2], mnesia:table_info(Tab1, master_nodes)),
    ?match(ok, mnesia:set_master_nodes(Tab1, [Node2, Node1])),
    ?match([Node2, Node1], mnesia:table_info(Tab1, master_nodes)),
    ?match(ok, mnesia:set_master_nodes(Tab2, [Node2])),  % Should set where_to_read to Node2!
    ?match([Node2], mnesia:table_info(Tab2, master_nodes)),
    ?match(ok, mnesia:set_master_nodes(Tab3, [Node3])),
    ?match([Node3], mnesia:table_info(Tab3, master_nodes)),
    ?match(ok, mnesia:set_master_nodes(Tab3, [])),
    ?match([], mnesia:table_info(Tab3, master_nodes)),

    ?match(ok, mnesia:set_master_nodes([Node1])),
    ?match([Node1], mnesia:table_info(schema, master_nodes)),
    ?match([Node1], mnesia:table_info(Tab1, master_nodes)),
    ?match([], mnesia:table_info(Tab2, master_nodes)),
    ?match([Node1], mnesia:table_info(Tab3, master_nodes)),

    ?match(ok, mnesia:set_master_nodes([Node1, Node2])),
    ?match([Node1, Node2], mnesia:table_info(schema, master_nodes)),
    ?match([Node1, Node2], mnesia:table_info(Tab1, master_nodes)),
    ?match([Node2], mnesia:table_info(Tab2, master_nodes)),
    ?match([Node1], mnesia:table_info(Tab3, master_nodes)),

    ?verify_mnesia(Nodes, []).

offline_set_master_nodes(suite) -> [];
offline_set_master_nodes(Config) when is_list(Config) ->
    [Node] = Nodes = ?acquire_nodes(1, Config),
    Tab1 = offline_master_node_tab1,
    Tab2 = offline_master_node_tab2,
    Tab3 = offline_master_node_tab3,
    Tabs = ?sort([Tab1, Tab2, Tab3]),
    Def1 = [{ram_copies, [Node]}],
    Def2 = [{disc_copies, [Node]}],
    Def3 = [{disc_only_copies, [Node]}],
    ?match({atomic, ok}, mnesia:create_table(Tab1, Def1)),
    ?match({atomic, ok}, mnesia:create_table(Tab2, Def2)),
    ?match({atomic, ok}, mnesia:create_table(Tab3, Def3)),
    ?match([], mnesia:system_info(master_node_tables)),
    ?match([], mnesia_test_lib:stop_mnesia([Node])),

    ?match(ok, mnesia:set_master_nodes(Tab1, [Node])),
    ?match(ok, mnesia:set_master_nodes(Tab2, [Node])),
    ?match(ok, mnesia:set_master_nodes(Tab3, [Node])),
    ?match([], mnesia_test_lib:start_mnesia([Node])),
    ?match(Tabs, ?sort(mnesia:system_info(master_node_tables))),

    ?match([], mnesia_test_lib:stop_mnesia([Node])),
    ?match(ok, mnesia:set_master_nodes(Tab1, [])),
    ?match(ok, mnesia:set_master_nodes(Tab2, [])),
    ?match(ok, mnesia:set_master_nodes(Tab3, [])),
    ?match([], mnesia_test_lib:start_mnesia([Node])),
    ?match([], mnesia:system_info(master_node_tables)),

    ?match([], mnesia_test_lib:stop_mnesia([Node])),
    ?match(ok, mnesia:set_master_nodes([Node])),
    ?match([], mnesia_test_lib:start_mnesia([Node])),
    AllTabs = ?sort([schema | Tabs]),
    ?match(AllTabs, ?sort(mnesia:system_info(master_node_tables))),

    ?match([], mnesia_test_lib:stop_mnesia([Node])),
    ?match(ok, mnesia:set_master_nodes([])),
    ?match([], mnesia_test_lib:start_mnesia([Node])),
    ?match([], mnesia:system_info(master_node_tables)),

    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Syncronize table with log or disc
%%

%% Dump ram tables on disc
dump_tables(suite) -> [];
dump_tables(Config) when is_list(Config) ->
    [Node1, Node2] = Nodes = ?acquire_nodes(2, Config),
    Tab = dump_tables,
    Schema = [{name, Tab}, {attributes, [k, v]}, {ram_copies, [Node2]}],
    ?match({atomic, ok}, mnesia:create_table(Schema)),

    %% Dump 10 records
    Size = 10,
    Keys = lists:seq(1, Size),
    Records = [{Tab, A, 7} || A <- Keys],
    lists:foreach(fun(Rec) -> ?match(ok, mnesia:dirty_write(Rec)) end, Records),

    AllKeys = fun() -> ?sort(mnesia:all_keys(Tab)) end,

    ?match({atomic, Keys}, mnesia:transaction(AllKeys)),
    ?match({atomic, ok}, mnesia:dump_tables([Tab])),

    %% Delete one record
    ?match(ok, mnesia:dirty_delete({Tab, 5})),
    Keys2 = lists:delete(5, Keys),

    ?match({atomic, Keys2}, mnesia:transaction(AllKeys)),

    %% Check that all 10 is restored after a stop
    ?match([], mnesia_test_lib:stop_mnesia([Node1, Node2])),
    ?match([], mnesia_test_lib:start_mnesia([Node1, Node2])),
    ?match(ok, mnesia:wait_for_tables([Tab], infinity)),

    ?match({atomic, Keys}, mnesia:transaction(AllKeys)),

    ?match({aborted,Reason} when element(1, Reason) == no_exists,
           mnesia:dump_tables([foo])),
    ?verify_mnesia(Nodes, []).

dump_log(suite) -> [];
dump_log(Config) when is_list(Config) ->
    [Node1, Node2] = Nodes = ?acquire_nodes(2, Config),
    Tab = dump_log,
    Schema = [{name, Tab}, {attributes, [k, v]}, {ram_copies, [Node1, Node2]}],
    ?match({atomic, ok}, mnesia:create_table(Schema)),
    Tab1 = dump_log1,
    Schema1 = [{name, Tab1}, {attributes, [k, v]}, {disc_copies, [Node1]}],
    ?match({atomic, ok}, mnesia:create_table(Schema1)),
    Tab2 = dump_log2,
    Schema2 = [{name, Tab2}, {attributes, [k, v]}, {disc_only_copies, [Node1]}],
    ?match({atomic, ok}, mnesia:create_table(Schema2)),

    ?match(ok, mnesia:dirty_write({Tab, 1, ok})),
    ?match(ok, mnesia:dirty_write({Tab1, 1, ok})),
    ?match(ok, mnesia:dirty_write({Tab2, 1, ok})),

    ?match(dumped, mnesia:dump_log()),
    ?match(dumped, rpc:call(Node2, mnesia, dump_log, [])),

    ?match({atomic, ok}, mnesia:change_table_copy_type(schema, Node2, ram_copies)),
    ?match(dumped, rpc:call(Node2, mnesia, dump_log, [])),

    Self = self(),
    spawn(fun() -> dump_log(100, Self) end),
    spawn(fun() -> dump_log(100, Self) end),

    ?match(ok, receive finished -> ok after 3000 -> timeout end),
    ?match(ok, receive finished -> ok after 3000 -> timeout end),
    
    ?verify_mnesia(Nodes, []).

dump_log(N, Tester) when N > 0 ->
    mnesia:dump_log(),
    dump_log(N-1, Tester);
dump_log(_, Tester) ->
    Tester ! finished.


wait_for_tables(doc) -> 
    ["Intf. test of wait_for_tables, see also force_load_table"];
wait_for_tables(suite) -> [];
wait_for_tables(Config) when is_list(Config) ->
    [Node1, Node2] = Nodes = ?acquire_nodes(2, Config),
    Tab = wf_tab,
    Schema = [{name, Tab}, {ram_copies, [Node1, Node2]}],
    ?match({atomic, ok}, mnesia:create_table(Schema)),
    ?match(ok, mnesia:wait_for_tables([wf_tab], infinity)),
    ?match(ok, mnesia:wait_for_tables([], timer:seconds(5))),
    ?match({timeout, [bad_tab]}, mnesia:wait_for_tables([bad_tab], timer:seconds(5))),
    ?match(ok, mnesia:wait_for_tables([wf_tab], 0)),
    ?match({error,_}, mnesia:wait_for_tables([wf_tab], -1)),
    ?verify_mnesia(Nodes, []).

force_load_table(suite) -> [];
force_load_table(Config) when is_list(Config) ->
    [Node1, Node2] = ?acquire_nodes(2, Config),
    Tab = wf_tab,

    Schema = [{name, Tab}, {disc_copies, [Node1, Node2]}],
    ?match({atomic, ok}, mnesia:create_table(Schema)),
    ?match(ok, mnesia:dirty_write({Tab, 1, test_ok})),
    mnesia_test_lib:kill_mnesia([Node1]),
    ?match(ok, rpc:call(Node2, mnesia, dirty_write, [{Tab, 1, test_nok}])),
    mnesia_test_lib:kill_mnesia([Node2]),
    %%    timer:sleep(timer:seconds(5)),
    ?match(ok, mnesia:start()),
    ?match({timeout, [Tab]}, mnesia:wait_for_tables([Tab], 5)),
    ?match({'EXIT', _}, mnesia:dirty_read({Tab, 1})),
    ?match(yes, mnesia:force_load_table(Tab)),
    ?match([{Tab, 1, test_ok}], mnesia:dirty_read({Tab, 1})),

    ?match({error, _}, mnesia:force_load_table(error_tab)),
    ?verify_mnesia([Node1], [Node2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

user_properties(doc) ->
    ["Test of reading, writing and deletion of user properties",
     "Plus initialization of user properties when a table is created",
     "Do also test mnesia:table_info(Tab, user_properties)"];
user_properties(suite) -> [];
user_properties(Config) when is_list(Config) ->
    [Node] = Nodes = ?acquire_nodes(1, Config),
    Tab1 = user_properties_1,
    Tab2 = user_properties_2,
    Tab3 = user_properties_3,
    Def1 = [{ram_copies, [Node]}, {user_properties, []}],
    Def2 = [{mnesia_test_lib:storage_type(disc_copies, Config), [Node]}],
    Def3 = [{mnesia_test_lib:storage_type(disc_only_copies, Config), [Node]}, 
	    {user_properties, []}],

    PropKey = my_prop,
    Prop = {PropKey, some, elements},
    Prop2 = {PropKey, some, other, elements},
    YourProp= {your_prop},
    ?match({atomic, ok}, mnesia:create_table(Tab1, Def1)),
    ?match({atomic, ok}, mnesia:create_table(Tab2, Def2)),
    ?match({atomic, ok}, mnesia:create_table(Tab3, Def3)),

    ?match([], mnesia:table_info(Tab1, user_properties)),
    ?match([], mnesia:table_info(Tab2, user_properties)),
    ?match([], mnesia:table_info(Tab3, user_properties)),

    ?match({'EXIT', {aborted, {no_exists, {Tab1, user_property, PropKey}}}},
	   mnesia:read_table_property(Tab1, PropKey)),
    ?match({'EXIT', {aborted, {no_exists, {Tab2, user_property, PropKey}}}},
	   mnesia:read_table_property(Tab2, PropKey)),
    ?match({'EXIT', {aborted, {no_exists, {Tab3, user_property, PropKey}}}},
	   mnesia:read_table_property(Tab3, PropKey)),

    ?match({atomic, ok}, mnesia:write_table_property(Tab1, Prop)),
    ?match({atomic, ok}, mnesia:write_table_property(Tab2, Prop)),
    ?match({atomic, ok}, mnesia:write_table_property(Tab3, Prop)),
    ?match({atomic, ok}, mnesia:write_table_property(Tab1, YourProp)),
    ?match({atomic, ok}, mnesia:write_table_property(Tab2, YourProp)),
    ?match({atomic, ok}, mnesia:write_table_property(Tab3, YourProp)),

    ?match(Prop, mnesia:read_table_property(Tab1, PropKey)),
    ?match(Prop, mnesia:read_table_property(Tab2, PropKey)),
    ?match(Prop, mnesia:read_table_property(Tab3, PropKey)),

    ?match({atomic, ok}, mnesia:write_table_property(Tab1, Prop2)),
    ?match({atomic, ok}, mnesia:write_table_property(Tab2, Prop2)),
    ?match({atomic, ok}, mnesia:write_table_property(Tab3, Prop2)),
    ?match(Prop2, mnesia:read_table_property(Tab1, PropKey)),
    ?match(Prop2, mnesia:read_table_property(Tab2, PropKey)),
    ?match(Prop2, mnesia:read_table_property(Tab3, PropKey)),

    ?match({atomic, ok}, mnesia:delete_table_property(Tab1, PropKey)),
    ?match({atomic, ok}, mnesia:delete_table_property(Tab2, PropKey)),
    ?match({atomic, ok}, mnesia:delete_table_property(Tab3, PropKey)),

    ?match([YourProp], mnesia:table_info(Tab1, user_properties)),
    ?match([YourProp], mnesia:table_info(Tab2, user_properties)),
    ?match([YourProp], mnesia:table_info(Tab3, user_properties)),

    Tab4 = user_properties_4,
    ?match({atomic, ok}, 
	   mnesia:create_table(Tab4, [{user_properties, [Prop]}])),

    ?match([Prop], mnesia:table_info(Tab4, user_properties)),

    %% Some error cases

    ?match({aborted, {bad_type, Tab1, {}}},
	   mnesia:write_table_property(Tab1, {})),
    ?match({aborted, {bad_type, Tab1, ali}},
	   mnesia:write_table_property(Tab1, ali)),

    Tab5 = user_properties_5,
    ?match({aborted, {bad_type, Tab5, {user_properties, {}}}}, 
	   mnesia:create_table(Tab5, [{user_properties, {}}])),
    ?match({aborted, {bad_type, Tab5, {user_properties, ali}}}, 
	   mnesia:create_table(Tab5, [{user_properties, ali}])),
    ?match({aborted, {bad_type, Tab5, {user_properties, [{}]}}}, 
	   mnesia:create_table(Tab5, [{user_properties, [{}]}])),
    ?match({aborted, {bad_type, Tab5, {user_properties, [ali]}}}, 
	   mnesia:create_table(Tab5, [{user_properties, [ali]}])),

    ?verify_mnesia(Nodes, []).


unsupp_user_props(doc) ->
    ["Simple test of adding user props in a schema_transaction"];
unsupp_user_props(suite) -> [];
unsupp_user_props(Config) when is_list(Config) ->
    [Node1] = ?acquire_nodes(1, Config),
    Tab1 = silly1,
    Tab2 = silly2,
    Storage = mnesia_test_lib:storage_type(ram_copies, Config),

    ?match({atomic, ok}, rpc:call(Node1, mnesia,
				  create_table, [Tab1, [{Storage, [Node1]}]])),
    ?match({atomic, ok}, rpc:call(Node1, mnesia,
				  create_table, [Tab2, [{Storage, [Node1]}]])),

    F1 = fun() ->
		 mnesia_schema:do_write_table_property(
		   silly1, {prop, propval1}),
		 mnesia_schema:do_write_table_property(
		   silly2, {prop, propval2}), % same key as above
		 mnesia_schema:do_write_table_property(
		   schema, {prop, propval3})  % same key as above
	 end,
    ?match({atomic, ok}, rpc:call(Node1, mnesia_schema,
				  schema_transaction, [F1])),

    ?match([{prop,propval1}], rpc:call(Node1, mnesia,
				       table_info, [silly1, user_properties])),
    ?match([{prop,propval2}], rpc:call(Node1, mnesia,
				       table_info, [silly2, user_properties])),
    ?match([_,{prop,propval3}], rpc:call(Node1, mnesia,
				       table_info, [schema, user_properties])),

    F2 = fun() ->
		 mnesia_schema:do_write_table_property(
		   silly1, {prop, propval1a}),
		 mnesia_schema:do_write_table_property(
		   silly1, {prop, propval1b})  % same key as above
	 end,
    ?match({atomic, ok}, rpc:call(Node1, mnesia_schema,
				  schema_transaction, [F2])),

    ?match([{prop,propval1b}], rpc:call(Node1, mnesia,
					table_info,
					[silly1, user_properties])),
    ?verify_mnesia([Node1], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


snmp_open_table(suite) -> [];
snmp_open_table(Config) when is_list(Config) -> 
    [Node1, Node2] = Nodes = ?acquire_nodes(2, Config),
    Tab1 = local_snmp_table,

    Storage = mnesia_test_lib:storage_type(disc_copies, Config),
    Def1 = 
	case mnesia_test_lib:diskless(Config) of
	    true -> [{ram_copies, Nodes}];
	    false ->		
		[{disc_copies, [Node1]}, {ram_copies, [Node2]}]
	end,

    Tab2 = ext_snmp_table,
    Def2 = [{Storage, [Node2]}],
    ErrTab = non_existing_tab,
    ?match({atomic, ok}, mnesia:create_table(Tab1, Def1)),
    ?match({atomic, ok}, mnesia:create_table(Tab2, Def2)),
    ?match({atomic, ok}, mnesia:snmp_open_table(Tab1, [{key, integer}])),
    ?match({atomic, ok}, mnesia:snmp_open_table(Tab2, [{key, integer}])),
    ?match({aborted, _}, mnesia:snmp_open_table(ErrTab, [{key, integer}])),
    ?verify_mnesia(Nodes, []).

snmp_close_table(suite) -> [];
snmp_close_table(Config) when is_list(Config) -> 
    [Node1, Node2] = Nodes = ?acquire_nodes(2, Config),
    Tab1 = local_snmp_table,
    Storage = mnesia_test_lib:storage_type(disc_copies, Config),
    Def1 = 
	case mnesia_test_lib:diskless(Config) of
	    true -> [{ram_copies, Nodes}];
	    false ->		
		[{disc_copies, [Node1]}, {ram_copies, [Node2]}]
	end,

    Tab2 = ext_snmp_table,
    Def2 = [{snmp, [{key, integer}]}, {Storage, [Node2]}],
    ErrTab = non_existing_tab,
    ?match({atomic, ok}, mnesia:create_table(Tab1, Def1)),
    ?match({atomic, ok}, mnesia:create_table(Tab2, Def2)),
    ?match({atomic, ok}, mnesia:create_table(no_snmp_tab, [])),
    add_some_records(Tab1, Tab2, 3),    
    ?match({atomic, ok}, mnesia:snmp_open_table(Tab1, [{key, integer}])),    
    add_some_records(Tab1, Tab2, 5),    
    ?match({atomic, ok}, mnesia:snmp_close_table(Tab1)),

    Transform = fun(Tab, Key) ->
			[{T,K,V}] = mnesia:read(Tab, Key, write),
			mnesia:delete(T,K, write),
			mnesia:write({T, {K,K}, V, 43+V})
		end,
    
    ?match({atomic, ok}, mnesia:transform_table(Tab1, ignore, [key,val,new])),
    ?match({atomic, ok},
	   mnesia:transaction(fun() ->
				      mnesia:write_lock_table(Tab1),
				      Keys = mnesia:select(Tab1, [{{'_','$1','_'}, [],
								   ['$1']}]),
				      [Transform(Tab1, Key) || Key <- Keys],
				      ok
			      end)),
    ?match([{Tab1, {1, 1}, 1, 44}], mnesia:dirty_read(Tab1, {1, 1})),
    ?match({atomic, ok}, mnesia:snmp_open_table(Tab1, [{key,{integer,integer}}])),

    ?match({atomic, ok}, mnesia:snmp_close_table(Tab2)),
    ?match({atomic, ok}, mnesia:transform_table(Tab2, ignore, [key,val,new])),
    ?match({atomic, ok},
	   mnesia:transaction(fun() ->
				      mnesia:write_lock_table(Tab2),
				      Keys = mnesia:select(Tab2, [{{'_','$1','_'}, [],
								   ['$1']}]),
				      [Transform(Tab2, Key) || Key <- Keys],
				      ok
			      end)),
    
    ?match({atomic, ok}, mnesia:snmp_open_table(Tab2, [{key,{integer,integer}}])), 
    
    %% Should be aborted ????
    ?match({atomic, ok}, mnesia:snmp_close_table(no_snmp_tab)),
    ?match({aborted, _}, mnesia:snmp_close_table(ErrTab)),
    ?verify_mnesia(Nodes, []).

snmp_get_next_index(suite) -> [];
snmp_get_next_index(Config) when is_list(Config) ->
    [Node1, Node2] = Nodes = ?acquire_nodes(2, Config),
    Tab1 = local_snmp_table,
    Storage = mnesia_test_lib:storage_type(disc_copies, Config),
    Def1 = 
	case mnesia_test_lib:diskless(Config) of
	    true -> [{ram_copies, Nodes}];
	    false ->		
		[{disc_copies, [Node1]}, {ram_copies, [Node2]}]
	end,

    Tab2 = ext_snmp_table,
    Def2 = [{Storage, [Node2]}],
    ?match({atomic, ok}, mnesia:create_table(Tab1, Def1)),
    ?match({atomic, ok}, mnesia:create_table(Tab2, Def2)),
    ?match({atomic, ok}, mnesia:snmp_open_table(Tab1, [{key, integer}])),
    ?match({atomic, ok}, mnesia:snmp_open_table(Tab2, [{key, integer}])),
    add_some_records(Tab1, Tab2, 1),
    Test = 
	fun() ->
		%% Test local tables
		{success, Res11} = ?match({ok, _}, mnesia:snmp_get_next_index(Tab1,[])),
		{ok, Index11} = Res11,
		{success, _Res12} = 
		    ?match(endOfTable, mnesia:snmp_get_next_index(Tab1, Index11)),
		?match({'EXIT',_}, mnesia:snmp_get_next_index(Tab1, endOfTable)),

		%% Test external table
		{success, Res21} =
		    ?match({ok, _}, mnesia:snmp_get_next_index(Tab2, [])),
		{ok, Index21} = Res21,
		{success, _Res22} = 
		    ?match(endOfTable, mnesia:snmp_get_next_index(Tab2, Index21)),

		{ok, Row} = mnesia:snmp_get_row(Tab1, Index11),		
		?match(ok, mnesia:dirty_delete(Tab1, hd(Index11))),

		?match(endOfTable, mnesia:snmp_get_next_index(Tab1,[])),

		ok = mnesia:dirty_write(Row), %% Reset to coming tests

		%% Test of non existing table
		%%?match(endOfTable, mnesia:snmp_get_next_index(ErrTab, [])),
		ok
	end,
    ?match(ok, Test()),
    ?match({atomic,ok}, mnesia:transaction(Test)),
    ?match(ok, mnesia:sync_dirty(Test)),
    ?match(ok, mnesia:activity(transaction,Test,mnesia)),

    %%io:format("**********Before ~p~n", [mnesia_lib:val({Tab1,snmp})]),
    %%io:format(" ~p ~n", [ets:tab2list(mnesia_lib:val({local_snmp_table,{index,snmp}}))]),
    ?match([], mnesia_test_lib:stop_mnesia(Nodes)),
    ?match([], mnesia_test_lib:start_mnesia(Nodes, [Tab1, Tab2])),
    %%io:format("**********After ~p~n", [mnesia_lib:val({Tab1,snmp})]),
    %%io:format(" ~p ~n", [ets:tab2list(mnesia_lib:val({local_snmp_table,{index,snmp}}))]),

    ?match(ok, Test()),
    ?match({atomic,ok}, mnesia:transaction(Test)),
    ?match(ok, mnesia:sync_dirty(Test)),
    ?match(ok, mnesia:activity(transaction,Test,mnesia)),

    ?verify_mnesia(Nodes, []).

add_some_records(Tab1, Tab2, N) ->
    Recs1 = [{Tab1, I, I} || I <- lists:reverse(lists:seq(1, N))],
    Recs2 = [{Tab2, I, I} || I <- lists:reverse(lists:seq(20, 20+N-1))],
    lists:foreach(fun(R) -> mnesia:dirty_write(R) end, Recs1),
    Fun = fun(R) -> mnesia:write(R) end,
    Trans = fun() -> lists:foreach(Fun, Recs2) end, 
    {atomic, ok} = mnesia:transaction(Trans),
    %% Sync things, so everything gets everywhere!
    {atomic, ok} = mnesia:sync_transaction(fun() -> mnesia:write(lists:last(Recs1)) end),
    {atomic, ok} = mnesia:sync_transaction(fun() -> mnesia:write(lists:last(Recs2)) end),
    ?sort(Recs1 ++ Recs2).

snmp_get_row(suite) -> [];
snmp_get_row(Config) when is_list(Config) -> 
    [Node1, Node2] = Nodes = ?acquire_nodes(2, Config),
    Tab1 = local_snmp_table,
    Storage = mnesia_test_lib:storage_type(disc_copies, Config),
    Def1 = 
	case mnesia_test_lib:diskless(Config) of
	    true -> [{ram_copies, Nodes}];
	    false ->		
		[{disc_copies, [Node1]}, {ram_copies, [Node2]}]
	end,
    Tab2 = ext_snmp_table,
    Def2 = [{Storage, [Node2]}],
    Tab3 = snmp_table,
    Def3 = [{Storage, [Node1]}, 
            {attributes, [key, data1, data2]}],    

    Setup = fun() ->
		    ?match({atomic, ok}, mnesia:create_table(Tab1, Def1)),
		    ?match({atomic, ok}, mnesia:create_table(Tab2, Def2)),
		    ?match({atomic, ok}, mnesia:create_table(Tab3, Def3)),
		    ?match({atomic, ok}, mnesia:snmp_open_table(Tab1, [{key, integer}])),
		    ?match({atomic, ok}, mnesia:snmp_open_table(Tab2, [{key, integer}])),
		    ?match({atomic, ok}, mnesia:snmp_open_table(
					   Tab3, [{key, {fix_string,integer}}])),
		    add_some_records(Tab1, Tab2, 1)
	    end,
    Clear = fun() ->
		    ?match({atomic, ok}, mnesia:delete_table(Tab1)),
		    ?match({atomic, ok}, mnesia:delete_table(Tab2)),
		    ?match({atomic, ok}, mnesia:delete_table(Tab3))
	    end,
    Test = 
	fun() ->
		%% Test local tables
		{success, Res11} = 
		    ?match({ok, [1]}, mnesia:snmp_get_next_index(Tab1,[])),
		{ok, Index11} = Res11,
		?match({ok, {Tab1,1,1}}, mnesia:snmp_get_row(Tab1, Index11)),    
		?match(endOfTable, mnesia:snmp_get_next_index(Tab1, Index11)),
		?match({'EXIT',_}, mnesia:snmp_get_row(Tab1, endOfTable)),
		?match(undefined, mnesia:snmp_get_row(Tab1, [73])),
		
		Add = fun() ->  mnesia:write({Tab3, {"f_string", 3}, data1, data2}) end,
		?match({atomic, ok}, mnesia:transaction(Add)),
                {success, {ok, Index31}} = ?match({ok, RowIndex31} when is_list(RowIndex31), 
						 mnesia:snmp_get_next_index(Tab3,[])),
		?match({ok, Row31} when is_tuple(Row31), 
					mnesia:snmp_get_row(Tab3, Index31)),
		?match(endOfTable, mnesia:snmp_get_next_index(Tab3, Index31)),
		Del = fun() -> mnesia:delete({Tab3,{"f_string",3}}) end,
		?match({atomic, ok}, mnesia:transaction(Del)),
		?match(undefined, mnesia:snmp_get_row(Tab3, "f_string" ++ [3])),
		?match(undefined, mnesia:snmp_get_row(Tab3, "f_string" ++ [73])),

                %% Test external table
                {success, Res21} = ?match({ok,[20]}, mnesia:snmp_get_next_index(Tab2, [])),
		{ok, Index21} = Res21,
                ?match({ok, Row2} when is_tuple(Row2), mnesia:snmp_get_row(Tab2, Index21)), 
                ?match(endOfTable, mnesia:snmp_get_next_index(Tab2, Index21)),
                %% Test of non existing table
                %% ?match(endOfTable, mnesia:snmp_get_next_index(ErrTab, [])),
                ok
	end,
    Setup(),
    ?match(ok, Test()),
    Clear(), Setup(),
    ?match({atomic,ok}, mnesia:transaction(Test)),
    Clear(), Setup(),
    ?match(ok, mnesia:sync_dirty(Test)),
    Clear(), Setup(),
    ?match(ok, mnesia:activity(transaction,Test,mnesia)),

    Clear(), Setup(),
    ?match([], mnesia_test_lib:stop_mnesia(Nodes)),
    ?match([], mnesia_test_lib:start_mnesia(Nodes, [Tab1, Tab2])),
    ?match(ok, Test()),
    Clear(), Setup(),
    ?match([], mnesia_test_lib:stop_mnesia(Nodes)),
    ?match([], mnesia_test_lib:start_mnesia(Nodes, [Tab1, Tab2])),
    ?match({atomic,ok}, mnesia:transaction(Test)),

    ?verify_mnesia(Nodes, []).

snmp_get_mnesia_key(suite) -> [];
snmp_get_mnesia_key(Config) when is_list(Config) ->
    [Node1, Node2] = Nodes = ?acquire_nodes(2, Config),
    Tab1 = local_snmp_table,
    Storage = mnesia_test_lib:storage_type(disc_copies, Config),
    Def1 = 
	case mnesia_test_lib:diskless(Config) of
	    true -> [{ram_copies, Nodes}];
	    false ->		
		[{disc_copies, [Node1]}, {ram_copies, [Node2]}]
	end,

    Tab2 = ext_snmp_table,
    Def2 = [{Storage, [Node2]}],

    Tab3 = fix_string,    
    Setup = fun() -> 
		    ?match({atomic, ok}, mnesia:create_table(Tab1, Def1)),
		    ?match({atomic, ok}, mnesia:create_table(Tab2, Def2)),
		    ?match({atomic, ok}, mnesia:create_table(Tab3, Def1)),
		    ?match({atomic, ok}, mnesia:snmp_open_table(Tab1, [{key, integer}])),
		    ?match({atomic, ok}, mnesia:snmp_open_table(Tab2, [{key, integer}])),
		    ?match({atomic, ok}, mnesia:snmp_open_table(Tab3, [{key, {fix_string,integer}}])),

		    add_some_records(Tab1, Tab2, 1)	    
	    end,
    Clear = fun() ->
		    ?match({atomic, ok}, mnesia:delete_table(Tab1)),
		    ?match({atomic, ok}, mnesia:delete_table(Tab2)),
		    ?match({atomic, ok}, mnesia:delete_table(Tab3))
	    end,
    Test = 
	fun() ->
		%% Test local tables
		{success, Res11} = 
		    ?match({ok, [1]}, mnesia:snmp_get_next_index(Tab1,[])),
		{ok, Index11} = Res11,
		?match({ok, 1}, mnesia:snmp_get_mnesia_key(Tab1, Index11)),    
		%% Test external tables
		{success, Res21} =
		    ?match({ok, [20]}, mnesia:snmp_get_next_index(Tab2, [])),
		{ok, Index21} = Res21,
		?match({ok, 20}, mnesia:snmp_get_mnesia_key(Tab2, Index21)),
		?match(undefined, mnesia:snmp_get_mnesia_key(Tab2, [97])),
		?match({'EXIT', _}, mnesia:snmp_get_mnesia_key(Tab2, 97)),

		?match({atomic,ok}, mnesia:transaction(fun() -> mnesia:delete({Tab1,1}) end)),
		?match(undefined, mnesia:snmp_get_mnesia_key(Tab1, Index11)),
		
		?match({atomic,ok},mnesia:transaction(fun() -> mnesia:write({Tab1,73,7}) end)),
		?match({ok, 73}, mnesia:snmp_get_mnesia_key(Tab1, [73])),
		?match({atomic,ok}, mnesia:transaction(fun() -> mnesia:delete({Tab1,73}) end)),
		?match(undefined, mnesia:snmp_get_mnesia_key(Tab1, [73])),

		?match({atomic,ok},mnesia:transaction(fun() -> mnesia:write({Tab3,{"S",5},7}) end)),
		?match({ok,{"S",5}}, mnesia:snmp_get_mnesia_key(Tab3, [$S,5])),
		?match({atomic,ok},mnesia:transaction(fun() -> mnesia:delete({Tab3,{"S",5}}) end)),
		?match(undefined, mnesia:snmp_get_mnesia_key(Tab3, [$S,5])),

		ok
	end,
    Setup(),
    ?match(ok, Test()),
    Clear(), Setup(),
    ?match({atomic,ok}, mnesia:transaction(Test)),
    Clear(), Setup(),
    ?match(ok, mnesia:sync_dirty(Test)),
    Clear(), Setup(),
    ?match(ok, mnesia:activity(transaction,Test,mnesia)),
    ?verify_mnesia(Nodes, []).

snmp_update_counter(doc) ->
    ["Verify that counters may be updated for tables with SNMP property"];
snmp_update_counter(suite) -> [];
snmp_update_counter(Config) when is_list(Config) ->
    [Node1] = Nodes = ?acquire_nodes(1, Config),
    Tab = snmp_update_counter,
    Def = [{attributes, [key, value]},
           {snmp, [{key, integer}]},
           {ram_copies, [Node1]}
          ],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    Oid = {Tab, 1},
    ?match([], mnesia:dirty_read(Oid)),
    ?match(ok, mnesia:dirty_write({Tab, 1, 1})),
    ?match([{Tab, _Key, 1}], mnesia:dirty_read(Oid)),
    ?match(3, mnesia:dirty_update_counter(Oid, 2)),
    ?match([{Tab, _Key, 3}], mnesia:dirty_read(Oid)),
    ?verify_mnesia(Nodes, []).

snmp_order(doc) ->
    ["Verify that sort order is correct in transactions and dirty variants"];
snmp_order(suite) -> [];
snmp_order(Config) when is_list(Config) ->
    [Node1] = Nodes = ?acquire_nodes(1, Config),
    Tab = snmp_order,
    Def = [{attributes, [key, value]},
           {snmp, [{key, {integer, integer, integer}}]},
           {ram_copies, [Node1]}
          ],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    Oid = {Tab, 1},
    ?match([], mnesia:dirty_read(Oid)),
    ?match({'EXIT', {aborted, _}}, mnesia:dirty_write({Tab, 1, 1})),
    [mnesia:dirty_write({Tab, {A,B,2}, default}) ||
	A <- lists:seq(1, 9, 2),
	B <- lists:seq(2, 8, 2)],
    
    Test1 = fun() ->
		    Keys0 = get_keys(Tab, []),
		    ?match(Keys0, lists:sort(Keys0)),
		    ?match([[1,2,2]|_], Keys0),
		    Keys1 = get_keys(Tab, [5]),
		    ?match(Keys1, lists:sort(Keys1)),
		    ?match([[5,2,2]|_], Keys1),
		    Keys2 = get_keys(Tab, [7, 4]),
		    ?match(Keys2, lists:sort(Keys2)),
		    ?match([[7,4,2]|_], Keys2),
		    ok
	    end,
    ?match(ok, Test1()),
    ?match({atomic, ok},mnesia:transaction(Test1)),
    ?match(ok,mnesia:sync_dirty(Test1)),
   
    Test2 = fun() ->
		    mnesia:write(Tab, {Tab,{0,0,2},updated}, write),
		    mnesia:write(Tab, {Tab,{5,3,2},updated}, write),
		    mnesia:write(Tab, {Tab,{10,10,2},updated}, write),
		    Keys0 = get_keys(Tab, []),
		    ?match([[0,0,2],[1,2,2]|_], Keys0),
		    ?match(Keys0, lists:sort(Keys0)),
		    
		    Keys1 = get_keys(Tab, [5]),
		    ?match([[5,2,2],[5,3,2]|_], Keys1),
		    ?match(Keys1, lists:sort(Keys1)),
		    
		    Keys2 = get_keys(Tab, [7,4]),
		    ?match([[7,4,2]|_], Keys2),
		    ?match(Keys2, lists:sort(Keys2)),
		    ?match([10,10,2], lists:last(Keys0)),
		    ?match([10,10,2], lists:last(Keys1)),
		    ?match([10,10,2], lists:last(Keys2)),
		    
		    ?match([[10,10,2]], get_keys(Tab, [10])),
		    ok
	    end,

    ?match({atomic, ok},mnesia:transaction(Test2)),

    ?verify_mnesia(Nodes, []).

get_keys(Tab, Key) ->
    case mnesia:snmp_get_next_index(Tab, Key) of
	endOfTable -> [];
	{ok, Next} ->
	    [Next|get_keys(Tab, Next)]
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(tab, {i, e1, e2}).			% Simple test table


subscribe_extended(doc) ->
    ["Test the extended set of events, test with and without checkpoints. "];
subscribe_extended(suite) ->
    [];
subscribe_extended(Config) when is_list(Config) ->
    [N1, N2]=Nodes=?acquire_nodes(2, Config),
    Tab1 = etab,
    Storage = mnesia_test_lib:storage_type(ram_copies, Config),
    Def1 = [{Storage, [N1, N2]}, {attributes, record_info(fields, tab)}],
    ?match({atomic, ok}, mnesia:create_table(Tab1, Def1)),

    Tab2 = bag,
    Def2 = [{Storage, [N1, N2]}, 
	    {type, bag},
	    {record_name, Tab1},
	    {attributes, record_info(fields, tab)}],
    ?match({atomic, ok}, mnesia:create_table(Tab2, Def2)),

    Tab3 = ctab,
    Def3 = [{Storage, [N1, N2]}],
    ?match({atomic, ok}, mnesia:create_table(Tab3, Def3)),

    ?match({ok, N1}, mnesia:subscribe({table, Tab1, detailed})),
    ?match({ok, N1}, mnesia:subscribe({table, Tab2, detailed})),
    ?match({ok, N1}, mnesia:subscribe({table, Tab3, detailed})),

    ?match({error, {already_exists, _}}, mnesia:subscribe({table, Tab1, simple})),
    ?match({error, {badarg, {table, Tab1, bad}}}, mnesia:subscribe({table, Tab1, bad})),

    ?match({ok, N1}, mnesia:subscribe(activity)),
    test_ext_sub(Tab1, Tab2, Tab3),
    
    ?match({ok, N1}, mnesia:unsubscribe(activity)),
    ?match({ok, N1}, mnesia:subscribe({table, Tab1, detailed})),
    ?match({atomic, ok}, mnesia:clear_table(Tab1)),
    ?match({mnesia_table_event, {delete, schema, {schema, Tab1}, [{schema, Tab1, _}],_}}, recv_event()),
    ?match({mnesia_table_event, {write, schema, {schema, Tab1, _}, [], _}}, recv_event()),

    ?match({atomic, ok}, mnesia:clear_table(Tab2)),
    ?match({mnesia_table_event, {delete, schema, {schema, Tab2}, [{schema, Tab2, _}],_}}, 
	   recv_event()),
    ?match({mnesia_table_event, {write, schema, {schema, Tab2, _}, [], _}}, recv_event()),

    ?match({ok, N1}, mnesia:unsubscribe({table, Tab2, detailed})),    
    {ok, _, _} = mnesia:activate_checkpoint([{name, testing}, 
					     {ram_overrides_dump, true}, 
					     {max, [Tab1, Tab2]}]),
    ?match({ok, N1}, mnesia:subscribe({table, Tab2, detailed})),
    ?match({ok, N1}, mnesia:subscribe(activity)),
    test_ext_sub(Tab1, Tab2, Tab3),

    ?verify_mnesia(Nodes, []).

test_ext_sub(Tab1, Tab2, Tab3) ->
    %% The basics 
    Rec1 = {Tab1, 1, 0, 0},
    Rec2 = {Tab1, 1, 1, 0},
    Rec3 = {Tab1, 2, 1, 0},
    Rec4 = {Tab1, 2, 2, 0},

    Write  = fun(Tab, Rec) -> 
		     mnesia:transaction(fun() -> mnesia:write(Tab, Rec, write) 
					end) 
	     end,
    Delete = fun(Tab, Rec) -> 
		     mnesia:transaction(fun() -> mnesia:delete(Tab, Rec, write) 
					end) 
	     end,
    DelObj = fun(Tab, Rec) -> 
		     mnesia:transaction(fun() -> mnesia:delete_object(Tab, Rec, write) 
					end) 
	     end,

    S = self(),
    D = {dirty, self()},
    %% SET 
    ?match(ok, mnesia:dirty_write(Rec1)),
    ?match({mnesia_table_event, {write, Tab1, Rec1, [], D}}, recv_event()), 
    ?match(ok, mnesia:dirty_write(Rec3)),
    ?match({mnesia_table_event, {write, Tab1, Rec3, [], D}}, recv_event()),    
    ?match(ok, mnesia:dirty_write(Rec1)),
    ?match({mnesia_table_event, {write, Tab1, Rec1, [Rec1], D}}, recv_event()),
    ?match({atomic, ok}, Write(Tab1, Rec2)),
    ?match({mnesia_table_event, {write, Tab1, Rec2, [Rec1], {tid,_,S}}}, recv_event()),    
    ?match({mnesia_activity_event, {complete, {tid,_,S}}}, recv_event()),
    ?match(ok, mnesia:dirty_delete({Tab1, 2})),
    ?match({mnesia_table_event, {delete, Tab1, {Tab1, 2}, [Rec3], D}}, recv_event()),
    ?match({atomic, ok}, DelObj(Tab1, Rec2)),
    ?match({mnesia_table_event, {delete, Tab1, Rec2, [Rec2], {tid,_,S}}}, recv_event()),
    ?match({mnesia_activity_event, {complete, {tid,_,S}}}, recv_event()),

    ?match({atomic, ok}, Delete(Tab1, 1)),
    ?match({mnesia_table_event, {delete, Tab1, {Tab1, 1}, [], {tid,_,S}}}, recv_event()),
    ?match({mnesia_activity_event, {complete, {tid,_,S}}}, recv_event()),
    ?match({ok, _N1}, mnesia:unsubscribe({table, Tab1, detailed})),

    %% BAG 

    ?match({atomic, ok}, Write(Tab2, Rec1)),
    ?match({mnesia_table_event, {write, Tab2, Rec1, [], {tid,_,S}}}, recv_event()),
    ?match({mnesia_activity_event, {complete, {tid,_,S}}}, recv_event()),
    ?match({atomic, ok}, Write(Tab2, Rec4)),
    ?match({mnesia_table_event, {write, Tab2, Rec4, [], {tid,_,S}}}, recv_event()),    
    ?match({mnesia_activity_event, {complete, {tid,_,S}}}, recv_event()),
    ?match({atomic, ok}, Write(Tab2, Rec3)),
    ?match({mnesia_table_event, {write, Tab2, Rec3, [Rec4], {tid,_,S}}}, recv_event()),    
    ?match({mnesia_activity_event, {complete, {tid,_,S}}}, recv_event()),
    ?match({atomic, ok}, Write(Tab2, Rec2)),
    ?match({mnesia_table_event, {write, Tab2, Rec2, [Rec1], {tid,_,S}}}, recv_event()),    
    ?match({mnesia_activity_event, {complete, {tid,_,S}}}, recv_event()),
    ?match({atomic, ok}, Write(Tab2, Rec1)),
    ?match({mnesia_table_event, {write, Tab2, Rec1, [Rec1, Rec2], {tid,_,S}}}, recv_event()),
    ?match({mnesia_activity_event, {complete, {tid,_,S}}}, recv_event()),
    ?match({atomic, ok}, DelObj(Tab2, Rec2)),
    ?match({mnesia_table_event, {delete, Tab2, Rec2, [Rec2], {tid,_,S}}}, recv_event()),
    ?match({mnesia_activity_event, {complete, {tid,_,S}}}, recv_event()),
    ?match({atomic, ok}, Delete(Tab2, 1)),
    ?match({mnesia_table_event, {delete, Tab2, {Tab2, 1}, [Rec1], {tid,_,S}}}, recv_event()),
    ?match({mnesia_activity_event, {complete, {tid,_,S}}}, recv_event()),
    ?match({atomic, ok}, Delete(Tab2, 2)),
    ?match({mnesia_table_event, {delete, Tab2, {Tab2, 2}, [Rec4, Rec3], {tid,_,S}}}, recv_event()),
    ?match({mnesia_activity_event, {complete, {tid,_,S}}}, recv_event()),

    %% COUNTERS

    Rec5 = {Tab3, counter, 0},
    ?match(ok, mnesia:dirty_write(Rec5)),
    ?match({mnesia_table_event, {write, Tab3, Rec5, [], D}}, recv_event()),
    ?match(1, mnesia:dirty_update_counter({Tab3, counter}, 1)),
    ?match({mnesia_table_event, {write, Tab3, {Tab3,counter,1}, [Rec5], D}}, recv_event()),
    ?match(ok, mnesia:dirty_delete({Tab3, counter})),
    ?match({mnesia_table_event, {delete, Tab3, {Tab3,counter},
				 [{Tab3,counter,1}], D}}, recv_event()),
    ok.


subscribe_standard(doc) ->
    ["Tests system events and the orignal table events"];
subscribe_standard(suite) -> [];
subscribe_standard(Config) when is_list(Config)-> 
    [N1, N2]=?acquire_nodes(2, Config),
    Tab = tab,

    Storage = mnesia_test_lib:storage_type(disc_copies, Config),
    Def = [{Storage, [N1, N2]}, {attributes, record_info(fields, tab)}],

    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),

    %% Check system events
    ?match({error, {badarg, foo}}, mnesia:unsubscribe(foo)),
    ?match({error, badarg}, mnesia:unsubscribe({table, foo})),
    mnesia:unsubscribe(activity),

    ?match({ok, N1}, mnesia:subscribe(system)),
    ?match({ok, N1}, mnesia:subscribe(activity)),

    ?match([], mnesia_test_lib:kill_mnesia([N2])),
    ?match({mnesia_system_event, {mnesia_down, N2}}, recv_event()),
    ?match(timeout, recv_event()),

    ?match([], mnesia_test_lib:start_mnesia([N2], [Tab])),
    ?match({mnesia_activity_event, _}, recv_event()),
    ?match({mnesia_system_event,{mnesia_up, N2}}, recv_event()),

    ?match(true, lists:member(self(), mnesia:system_info(subscribers))),
    ?match([], mnesia_test_lib:kill_mnesia([N1])),
    timer:sleep(500),
    mnesia_test_lib:flush(),
    ?match([], mnesia_test_lib:start_mnesia([N1], [Tab])),
    ?match(timeout, recv_event()),

    ?match({ok, N1}, mnesia:subscribe(system)),
    ?match({error, {already_exists, system}}, mnesia:subscribe(system)),
    ?match(stopped, mnesia:stop()),
    ?match({mnesia_system_event, {mnesia_down, N1}}, recv_event()),
    ?match({error, {node_not_running, N1}}, mnesia:subscribe(system)),
    ?match([], mnesia_test_lib:start_mnesia([N1, N2], [Tab])),

    %% Check table events
    ?match({ok, N1}, mnesia:subscribe(activity)),
    Old_Level = mnesia:set_debug_level(trace),
    ?match({ok, N1}, mnesia:subscribe({table,Tab})),

    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write(#tab{i=155}) end)),
    Self = self(),
    ?match({mnesia_table_event, {write, _, _}}, recv_event()),
    ?match({mnesia_activity_event, {complete, {tid, _, Self}}}, recv_event()),

    ?match({ok, N1}, mnesia:unsubscribe({table,Tab})),
    ?match({ok, N1}, mnesia:unsubscribe(activity)),

    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write(#tab{i=255}) end)),
    
    ?match(timeout, recv_event()),
    mnesia:set_debug_level(Old_Level),

    %% Check deletion of replica

    ?match({ok, N1}, mnesia:subscribe({table,Tab})),
    ?match({ok, N1}, mnesia:subscribe(activity)),
    ?match(ok, mnesia:dirty_write(#tab{i=355})),
    ?match({mnesia_table_event, {write, _, _}}, recv_event()),
    ?match({atomic, ok}, mnesia:del_table_copy(Tab, N1)),
    ?match({mnesia_activity_event, _}, recv_event()),
    ?match(ok, mnesia:dirty_write(#tab{i=455})),
    ?match(timeout, recv_event()),

    ?match({atomic, ok}, mnesia:move_table_copy(Tab, N2, N1)),
    ?match({mnesia_activity_event, _}, recv_event()),
    ?match({ok, N1}, mnesia:subscribe({table,Tab})),
    ?match(ok, mnesia:dirty_write(#tab{i=555})),
    ?match({mnesia_table_event, {write, _, _}}, recv_event()),
    ?match({atomic, ok}, mnesia:move_table_copy(Tab, N1, N2)),
    ?match({mnesia_activity_event, _}, recv_event()),
    ?match(ok, mnesia:dirty_write(#tab{i=655})),
    ?match(timeout, recv_event()),

    ?match({atomic, ok}, mnesia:add_table_copy(Tab, N1, ram_copies)),
    ?match({mnesia_activity_event, _}, recv_event()),
    ?match({ok, N1}, mnesia:subscribe({table,Tab})),
    ?match({error, {already_exists, {table,Tab, simple}}}, 
	   mnesia:subscribe({table,Tab})),
    ?match(ok, mnesia:dirty_write(#tab{i=755})),
    ?match({mnesia_table_event, {write, _, _}}, recv_event()),

    ?match({atomic, ok}, mnesia:delete_table(Tab)),
    ?match({mnesia_activity_event, _}, recv_event()),
    ?match(timeout, recv_event()),

    mnesia_test_lib:kill_mnesia([N1]),

    ?verify_mnesia([N2], [N1]).

recv_event() ->
    receive
	Event -> Event
    after 1000 -> 
	    timeout
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


foldl(suite) ->
    [];
foldl(doc) ->
    [""];
foldl(Config) when is_list(Config) ->
    Nodes = [_N1, N2] = ?acquire_nodes(2, Config),
    Tab1 = fold_local,
    Tab2 = fold_remote,
    Tab3 = fold_ordered, 
    ?match({atomic, ok}, mnesia:create_table(Tab1, [{ram_copies, Nodes}])),
    ?match({atomic, ok}, mnesia:create_table(Tab2, [{ram_copies, [N2]}, {type, bag}])),
    ?match({atomic, ok}, mnesia:create_table(Tab3, [{ram_copies, Nodes}, 
						    {type, ordered_set}])),    

    Tab1Els = [{Tab1, N, N} || N <- lists:seq(1, 10)],
    Tab2Els = ?sort([{Tab2, 1, 2} | [{Tab2, N, N} || N <- lists:seq(1, 10)]]),
    Tab3Els = [{Tab3, N, N} || N <- lists:seq(1, 10)],

    [mnesia:sync_transaction(fun() -> mnesia:write(E) end) || E <- Tab1Els],
    [mnesia:sync_transaction(fun() -> mnesia:write(E) end) || E <- Tab2Els],
    [mnesia:sync_transaction(fun() -> mnesia:write(E) end) || E <- Tab3Els],

    Fold = fun(Tab) ->
		   lists:reverse(mnesia:foldl(fun(E, A) -> [E | A] end, [], Tab))
	   end,
    Fold2 = fun(Tab, Lock) ->
		    lists:reverse(mnesia:foldl(fun(E, A) -> [E | A] end, [], Tab, Lock))
	    end,    
    Exit = fun(Tab) ->
		   lists:reverse(mnesia:foldl(fun(_E, _A) -> exit(testing) end, [], Tab))
	   end,
    %% Errors 
    ?match({aborted, _}, mnesia:transaction(Fold, [error])),
    ?match({aborted, _}, mnesia:transaction(fun(Tab) -> mnesia:foldl(badfun,[],Tab) end,
					    [Tab1])),    
    ?match({aborted, testing}, mnesia:transaction(Exit, [Tab1])), 
    ?match({aborted, _}, mnesia:transaction(Fold2, [Tab1, read_lock])),

    %% Success
    ?match({atomic, Tab1Els}, sort_res(mnesia:transaction(Fold, [Tab1]))),
    ?match({atomic, Tab2Els}, sort_res(mnesia:transaction(Fold, [Tab2]))),
    ?match({atomic, Tab3Els}, mnesia:transaction(Fold, [Tab3])),        

    ?match({atomic, Tab1Els}, sort_res(mnesia:transaction(Fold2, [Tab1, read]))),
    ?match({atomic, Tab1Els}, sort_res(mnesia:transaction(Fold2, [Tab1, write]))),

    ?match(Tab1Els, sort_res(mnesia:sync_dirty(Fold, [Tab1]))),
    ?match(Tab2Els, sort_res(mnesia:async_dirty(Fold, [Tab2]))),

    ?verify_mnesia(Nodes, []).	   

sort_res({atomic, List}) ->
    {atomic, ?sort(List)};
sort_res(Else) when is_list(Else) ->
    ?sort(Else);
sort_res(Else) ->
    Else.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

info(suite) -> [];
info(Config) when is_list(Config) ->
    Nodes = ?acquire_nodes(1, Config),
    ?match(ok, mnesia:info()),
    ?verify_mnesia(Nodes, []).

schema_0(suite) -> [];
schema_0(Config) when is_list(Config) ->
    Nodes = ?acquire_nodes(1, Config),
    ?match(ok, mnesia:schema()),
    ?verify_mnesia(Nodes, []).

schema_1(suite) -> [];
schema_1(Config) when is_list(Config) ->
    Nodes = ?acquire_nodes(1, Config),
    Tab = schema_1,
    ?match({atomic, ok}, mnesia:create_table(Tab, [])),
    ?match(ok, mnesia:schema(Tab)),
    ?verify_mnesia(Nodes, []).

view_0(suite) -> [];
view_0(Config) when is_list(Config) ->
    Nodes = ?acquire_nodes(1, Config),
    ?match(ok, mnesia_lib:view()),
    ?verify_mnesia(Nodes, []).

view_1(suite) -> [];
view_1(Config) when is_list(Config) ->
    Nodes = ?acquire_nodes(1, Config),
    BinCore = mnesia_lib:mkcore({crashinfo, "Just testing..."}),
    CoreFile = lists:concat(["MnesiaCore.", node(), ".view_1.", ?MODULE]),
    ?match(ok, file:write_file(CoreFile, BinCore)),
    ?match(ok, mnesia_lib:view(CoreFile)),
    ?match(ok, file:delete(CoreFile)),

    ?match(stopped, mnesia:stop()),
    Dir = mnesia:system_info(directory),
    ?match(eof, mnesia_lib:view(filename:join(Dir, "LATEST.LOG"))),
    ?match(ok, mnesia_lib:view(filename:join(Dir, "schema.DAT"))),
    ?verify_mnesia([], Nodes).

view_2(suite) -> [];
view_2(Config) when is_list(Config) ->
    Nodes = ?acquire_nodes(1, Config),
    BinCore = mnesia_lib:mkcore({crashinfo, "More testing..."}),
    File = lists:concat([?MODULE, "view_2.", node()]),
    ?match(ok, file:write_file(File, BinCore)),
    ?match(ok, mnesia_lib:view(File, core)),
    ?match(ok, file:delete(File)),

    ?match(stopped, mnesia:stop()),
    Dir = mnesia:system_info(directory),
    ?match(ok, file:rename(filename:join(Dir, "LATEST.LOG"), File)),
    ?match(eof, mnesia_lib:view(File, log)),
    ?match(ok, file:delete(File)),

    ?match(ok, file:rename(filename:join(Dir, "schema.DAT"), File)),
    ?match(ok, mnesia_lib:view(File, dat)),
    ?match(ok, file:delete(File)),
    ?verify_mnesia([], Nodes).

lkill(suite) -> [];
lkill(Config) when is_list(Config) ->
    [Node1, Node2] = ?acquire_nodes(2, Config),

    ?match(yes, rpc:call(Node1, mnesia, system_info, [is_running])),
    ?match(yes, rpc:call(Node2, mnesia, system_info, [is_running])),
    ?match(ok, rpc:call(Node2, mnesia, lkill, [])),
    ?match(yes, rpc:call(Node1, mnesia, system_info, [is_running])),
    ?match(no, rpc:call(Node2, mnesia, system_info, [is_running])),
    ?verify_mnesia([Node1], [Node2]).

kill(suite) -> [];
kill(Config) when is_list(Config) ->
    [Node1, Node2] = ?acquire_nodes(2, Config),

    ?match(yes, rpc:call(Node1, mnesia, system_info, [is_running])),
    ?match(yes, rpc:call(Node2, mnesia, system_info, [is_running])),
    ?match({_, []}, rpc:call(Node2, mnesia, kill, [])),
    ?match(no, rpc:call(Node1, mnesia, system_info, [is_running])),
    ?match(no, rpc:call(Node2, mnesia, system_info, [is_running])),
    ?verify_mnesia([], [Node1, Node2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



record_name_dirty_access_ram(suite) ->
    [];
record_name_dirty_access_ram(Config) when is_list(Config) ->
    record_name_dirty_access(ram_copies, Config).

record_name_dirty_access_disc(suite) ->
    [];
record_name_dirty_access_disc(Config) when is_list(Config) ->
    record_name_dirty_access(disc_copies, Config).

record_name_dirty_access_disc_only(suite) ->
    [];
record_name_dirty_access_disc_only(Config) when is_list(Config) ->
    record_name_dirty_access(disc_only_copies, Config).

record_name_dirty_access_xets(Config) when is_list(Config) ->
    record_name_dirty_access(ext_ets, Config).


record_name_dirty_access(Storage, Config) ->
    [Node1, _Node2] = Nodes = ?acquire_nodes(2, Config),

    List = lists:concat([record_name_dirty_access_, Storage]),
    Tab = list_to_atom(List),
    RecName = some_record,
    Attr = val,
    TabDef = [{type, bag},
	      {record_name, RecName},
	      {index, [Attr]},
	      {Storage, Nodes}],
    ?match({atomic, ok}, mnesia:create_table(Tab, TabDef)),

    ?match(RecName, mnesia:table_info(Tab, record_name)),

    ?match(ok, mnesia:dirty_write(Tab, {RecName, 2, 20})),
    ?match(ok, mnesia:dirty_write(Tab, {RecName, 2, 21})),
    ?match(ok, mnesia:dirty_write(Tab, {RecName, 2, 22})),

    %% Backup test
    BupFile = List ++ ".BUP",
    CpName = cpname,
    CpArgs = [{name, CpName}, {min, [Tab]}, {ram_overrides_dump, true}],
    ?match({ok, CpName, _}, mnesia:activate_checkpoint(CpArgs)),
    ?match(ok, mnesia:backup_checkpoint(CpName, BupFile)),
    ?match(ok, mnesia:deactivate_checkpoint(CpName)),

    ?match(ok, mnesia:dirty_write(Tab, {RecName, 1, 10})),
    ?match({ok, Node1}, mnesia:subscribe({table, Tab})),
    ?match(ok, mnesia:dirty_write(Tab, {RecName, 3, 10})),

    Twos =?sort( [{RecName, 2, 20}, {RecName, 2, 21}, {RecName, 2, 22}]),
    ?match(Twos, ?sort(mnesia:dirty_read(Tab, 2))),

    ?match(ok, mnesia:dirty_delete_object(Tab, {RecName, 2, 21})),

    Tens = ?sort([{RecName, 1, 10}, {RecName, 3, 10}]),
    TenPat = {RecName, '_', 10},
    ?match(Tens, ?sort(mnesia:dirty_match_object(Tab, TenPat))),
    ?match(Tens, ?sort(mnesia:dirty_select(Tab, [{TenPat, [], ['$_']}]))),

    %% Subscription test
    E = mnesia_table_event,
    ?match_receive({E, {write, {Tab, 3, 10}, _}}),
    ?match_receive({E, {delete_object, {Tab, 2, 21}, _}}),
    ?match({ok, Node1}, mnesia:unsubscribe({table, Tab})),

    ?match([], mnesia_test_lib:stop_mnesia([Node1])),
    ?match([], mnesia_test_lib:start_mnesia(Nodes, [Tab])),

    ?match(Tens, ?sort(mnesia:dirty_index_match_object(Tab, TenPat, Attr) )),
    ?match(Tens, ?sort(mnesia:dirty_index_read(Tab, 10, Attr))),

    ?match([1, 2, 3], ?sort(mnesia:dirty_all_keys(Tab))),

    ?match({ok, Node1}, mnesia:subscribe({table, Tab})),
    ?match(ok, mnesia:dirty_delete(Tab, 2)),
    ?match([], mnesia:dirty_read(Tab, 2)),

    ?match_receive({E, {delete, {Tab, 2}, _}}),
    ?match([], mnesia_test_lib:flush()),
    ?match({ok, Node1}, mnesia:unsubscribe({table, Tab})),

    %% Restore test
    ?match({atomic, [Tab]}, mnesia:restore(BupFile, [{recreate_tables, [Tab]}])),
    ?match(RecName, mnesia:table_info(Tab, record_name)),

    ?match(Twos, ?sort(mnesia:dirty_match_object(Tab, mnesia:table_info(Tab, wild_pattern)))),
    ?match(Twos, ?sort(mnesia:dirty_select(Tab, 
					   [{mnesia:table_info(Tab, wild_pattern),
					     [],['$_']}]))),

    %% Traverse backup test

    Fun = fun(Rec, {Good, Bad}) ->
		  ?verbose("BUP: ~p~n", [Rec]),
		  case Rec of
		      {T, K, V} when T == Tab ->
			  Good2 = Good ++ [{RecName, K, V}],
			  {[Rec], {?sort(Good2), Bad}};
		      {T, K} when T == Tab ->
			  Good2 = [G || G <- Good, element(2, G) /= K],
			  {[Rec], {?sort(Good2), Bad}};
		      _ when element(1, Rec) == schema ->
			  {[Rec], {Good, Bad}};
		      _ ->
			  Bad2 = Bad ++ [Rec],
			  {[Rec], {Good, ?sort(Bad2)}}
		  end
          end,

    ?match({ok, {Twos, []}}, mnesia:traverse_backup(BupFile, mnesia_backup, 
						    dummy, read_only, 
						    Fun, {[], []})),
    ?match(ok, file:delete(BupFile)),

    %% Update counter test

    CounterTab = list_to_atom(lists:concat([Tab, "_counter"])),
    CounterTabDef = [{record_name, some_counter}],
    C = my_counter,
    ?match({atomic, ok}, mnesia:create_table(CounterTab, CounterTabDef)),
    ?match(some_counter, mnesia:table_info(CounterTab, record_name)),
    ?match(0, mnesia:dirty_update_counter(CounterTab, gurka, -10)),
    ?match(10, mnesia:dirty_update_counter(CounterTab, C, 10)),
    ?match(11, mnesia:dirty_update_counter(CounterTab, C, 1)),
    ?match(4711, mnesia:dirty_update_counter(CounterTab, C, 4700)),
    ?match([{some_counter, C, 4711}], mnesia:dirty_read(CounterTab, C)),
    ?match(0, mnesia:dirty_update_counter(CounterTab, C, -4747)),

    %% Registry tests

    RegTab = list_to_atom(lists:concat([Tab, "_registry"])),
    RegTabDef = [{record_name, some_reg}],
    ?match(ok, mnesia_registry:create_table(RegTab, RegTabDef)),
    ?match(some_reg, mnesia:table_info(RegTab, record_name)),
    {success, RegRecs} =
	?match([_ | _], mnesia_registry_test:dump_registry(node(), RegTab)),

    R = ?sort(RegRecs),
    ?match(R, ?sort(mnesia_registry_test:restore_registry(node(), RegTab))),

    ?verify_mnesia(Nodes, []).

sorted_ets(suite) ->
    [];
sorted_ets(Config) when is_list(Config) ->
    [N1, N2, N3] = All = ?acquire_nodes(3, Config),

    Tab = sorted_tab,    
    Def = case  mnesia_test_lib:diskless(Config) of
	      true ->  [{name, Tab}, {type, ordered_set}, {ram_copies, All}];
	      false -> [{name, Tab}, {type, ordered_set},
			{ram_copies, [N1]}, 
			{disc_copies,[N2, N3]}]
	  end,

    ?match({atomic, ok}, mnesia:create_table(Def)),
    ?match({aborted, _}, mnesia:create_table(fel, [{disc_only_copies, N1}])),

    ?match([ok | _], 
	   [mnesia:dirty_write({Tab, {dirty, N}, N}) || N <- lists:seq(1, 10)]),    
    ?match({atomic, _}, 
	   mnesia:sync_transaction(fun() ->
					   [mnesia:write({Tab, {trans, N}, N}) || 
					       N <- lists:seq(1, 10)]
				   end)),

    List = mnesia:dirty_match_object({Tab, '_', '_'}),
    ?match(List, ?sort(List)),
    ?match(List, rpc:call(N2, mnesia, dirty_match_object, [{Tab, '_', '_'}])),
    ?match(List, rpc:call(N3, mnesia, dirty_match_object, [{Tab, '_', '_'}])),

    mnesia_test_lib:stop_mnesia(All),
    mnesia_test_lib:start_mnesia(All, [sorted_tab]),

    List = mnesia:dirty_match_object({Tab, '_', '_'}),
    ?match(List, ?sort(List)),
    ?match(List, rpc:call(N2, mnesia, dirty_match_object, [{Tab, '_', '_'}])),
    ?match(List, rpc:call(N3, mnesia, dirty_match_object, [{Tab, '_', '_'}])),

    ?match(List, rpc:call(N3, mnesia, dirty_select, [Tab, [{{Tab, '_', '_'},[],['$_']}]])),

    TransMatch = fun() ->
			 mnesia:write({Tab, {trans, 0}, 0}),
			 mnesia:write({Tab, {trans, 11}, 11}),
			 mnesia:match_object({Tab, '_', '_'})
		 end,
    TransSelect = fun() ->
			  mnesia:write({Tab, {trans, 0}, 0}),
			  mnesia:write({Tab, {trans, 11}, 11}),
			  mnesia:select(Tab, [{{Tab, '_', '_'},[],['$_']}])
		  end,

    TList = mnesia:transaction(TransMatch),
    STList = ?sort(TList),
    ?match(STList, TList),
    ?match(STList, rpc:call(N2, mnesia, transaction, [TransMatch])),
    ?match(STList, rpc:call(N3, mnesia, transaction, [TransMatch])),

    TSel = mnesia:transaction(TransSelect),
    ?match(STList, TSel),
    ?match(STList, rpc:call(N2, mnesia, transaction, [TransSelect])),
    ?match(STList, rpc:call(N3, mnesia, transaction, [TransSelect])),

    ?match({atomic, ok}, mnesia:create_table(rec, [{type, ordered_set}])),
    [ok = mnesia:dirty_write(R) || R <- [{rec,1,1}, {rec,2,1}]],
    ?match({atomic, ok}, mnesia:add_table_index(rec, 3)),    
    TestIt = fun() ->
		     ok = mnesia:write({rec,1,1}),
		     mnesia:index_read(rec, 1, 3)
	     end,
    ?match({atomic, [{rec,1,1}, {rec,2,1}]}, mnesia:transaction(TestIt)).


