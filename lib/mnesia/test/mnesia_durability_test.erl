%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(mnesia_durability_test).
-author('hakan@erix.ericsson.se').
-compile([export_all]).
-include("mnesia_test_lib.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_per_testcase(Func, Conf) ->
    mnesia_test_lib:init_per_testcase(Func, Conf).

end_per_testcase(Func, Conf) ->
    mnesia_test_lib:end_per_testcase(Func, Conf).

-record(test_rec,{key,val}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() -> 
    [{group, load_tables},
     {group, durability_of_dump_tables},
     durability_of_disc_copies,
     durability_of_disc_only_copies].

groups() -> 
    [{load_tables, [],
      [load_latest_data, load_local_contents_directly,
       load_directly_when_all_are_ram_copiesA,
       load_directly_when_all_are_ram_copiesB,
       {group, late_load_when_all_are_ram_copies_on_ram_nodes},
       load_when_last_replica_becomes_available,
       load_when_down_from_all_other_replica_nodes,
       late_load_transforms_into_disc_load,
       late_load_leads_to_hanging,
       force_load_when_nobody_intents_to_load,
       force_load_when_someone_has_decided_to_load,
       force_load_when_someone_else_has_loaded,
       force_load_when_we_has_loaded,
       force_load_on_a_non_local_table,
       force_load_when_the_table_does_not_exist,
       {group, load_tables_with_master_tables}]},
     {late_load_when_all_are_ram_copies_on_ram_nodes, [],
      [late_load_all_ram_cs_ram_nodes1,
       late_load_all_ram_cs_ram_nodes2]},
     {load_tables_with_master_tables, [],
      [master_nodes, starting_master_nodes,
       master_on_non_local_tables,
       remote_force_load_with_local_master_node]},
     {durability_of_dump_tables, [],
      [dump_ram_copies, dump_disc_copies, dump_disc_only]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_latest_data(doc) ->
    ["Base functionality, verify that the latest data is loaded"];
load_latest_data(suite) -> [];
load_latest_data(Config) when is_list(Config) ->
    [N1,N2,N3] = Nodes = ?acquire_nodes(3, Config),
    %%Create a replicated local table
    ?match({atomic,ok}, mnesia:create_table(t0, [{disc_copies,[N1,N2]}])),
    ?match({atomic,ok}, mnesia:create_table(t1, [{disc_copies,[N1,N2]}])),
    ?match({atomic,ok}, mnesia:create_table(t2, [{disc_copies,[N1,N2]}])),
    ?match({atomic,ok}, mnesia:create_table(t3, [{disc_copies,[N1,N2]}])),
    ?match({atomic,ok}, mnesia:create_table(t4, [{disc_copies,[N1,N2]}])),
    ?match({atomic,ok}, mnesia:create_table(t5, [{disc_copies,[N1,N2]}])),
    Rec1 = {t1, test, ok},
    Rec2 = {t1, test, 2},

    ?match([], mnesia_test_lib:kill_mnesia([N1])),
    ?match(ok, rpc:call(N2, mnesia, dirty_write, [Rec2])),
    ?match([], mnesia_test_lib:kill_mnesia([N2])),
    ?match([], mnesia_test_lib:kill_mnesia([N3])),
   
    ?match([], mnesia_test_lib:start_mnesia([N1], [])),   
    %% Should wait for N2
    ?match({timeout, [t1]}, rpc:call(N1, mnesia, wait_for_tables, [[t1], 1000])),
    ?match([], mnesia_test_lib:start_mnesia([N3], [])),   
    ?match({timeout, [t1]}, rpc:call(N1, mnesia, wait_for_tables, [[t1], 1000])),


    ?match([], mnesia_test_lib:start_mnesia([N2], [])),   
    ?match(ok, rpc:call(N2, mnesia, wait_for_tables, [[t1], 10000])),
    ?match(ok, rpc:call(N1, mnesia, wait_for_tables, [[t1], 10000])),
    %% We should find the record
    ?match([Rec2], rpc:call(N1, mnesia, dirty_read, [t1, test])),
    ?match([Rec2], rpc:call(N2, mnesia, dirty_read, [t1, test])),
    
    %% ok, lets switch order    
    ?match(ok, mnesia:dirty_delete_object(Rec1)),
    ?match(ok, mnesia:dirty_delete_object(Rec2)),
    %% redo

    ?match([], mnesia_test_lib:kill_mnesia([N2])),
    ?match(ok, mnesia:dirty_write(Rec1)),
    ?match([], mnesia_test_lib:kill_mnesia([N3])),
    ?match([], mnesia_test_lib:kill_mnesia([N1])),

    ?match([], mnesia_test_lib:start_mnesia([N2], [])),   
    %% Should wait for N1
    ?match({timeout, [t1]}, rpc:call(N2, mnesia, wait_for_tables, [[t1], 1000])),
    ?match([], mnesia_test_lib:start_mnesia([N3], [])),   
    ?match({timeout, [t1]}, rpc:call(N2, mnesia, wait_for_tables, [[t1], 1000])),
    ?match([], mnesia_test_lib:start_mnesia([N1], [])),
    ?match(ok, rpc:call(N2, mnesia, wait_for_tables, [[t1], 10000])),
    ?match(ok, rpc:call(N1, mnesia, wait_for_tables, [[t1], 10000])),
    %% We should find the record
    ?match([Rec1], rpc:call(N1, mnesia, dirty_read, [t1, test])),
    ?match([Rec1], rpc:call(N2, mnesia, dirty_read, [t1, test])),
    
    ?verify_mnesia(Nodes, []).


load_local_contents_directly(doc) ->
    ["Local contents shall always be loaded. Check this by having a local ",
     "table on two nodes N1, N2, stopping N1 before N2, an then verifying ",
     "that N1 can start without N2 being started."];
load_local_contents_directly(suite) -> [];
load_local_contents_directly(Config) when is_list(Config) ->
    [N1, N2] = Nodes = ?acquire_nodes(2, Config),
    %%Create a replicated local table
    ?match({atomic,ok},
           mnesia:create_table(test_rec,
                               [{local_content,true},
                                {disc_copies,Nodes},
                                {attributes,record_info(fields,test_rec)}]
                              ) ),
    %%Verify that it has local contents.
    ?match( true, mnesia:table_info(test_rec,local_content) ),
    %%Helper Funs
    Write_one = fun(Value) -> mnesia:write(#test_rec{key=1,val=Value}) end,
    Read_one  = fun(Key)   -> mnesia:read( {test_rec, Key}) end,
    %%Write a value one N1 that we may test against later
    ?match({atomic,ok},
           rpc:call( N1, mnesia, transaction, [Write_one,[11]] ) ),
    %%Stop Mnesia on N1
    %?match([], mnesia_test_lib:stop_mnesia([N1])),
    ?match([], mnesia_test_lib:kill_mnesia([N1])),

    %%Write a value on N2, same key but a different value
    ?match({atomic,ok},
           rpc:call( N2, mnesia, transaction, [Write_one,[22]] ) ),
    %%Stop Mnesia on N2
    %?match([], mnesia_test_lib:stop_mnesia([N2])),
    ?match([], mnesia_test_lib:kill_mnesia([N2])),

    %%Restart Mnesia on N1 verify that we can read from it without
    %%starting Mnesia on N2.
    ?match(ok, rpc:call(N1, mnesia, start, [])),
    ?match(ok, rpc:call(N1, mnesia, wait_for_tables, [[test_rec], 30000])),
    %%Read back the value
    ?match( {atomic,[#test_rec{key=1,val=11}]},
           rpc:call(N1, mnesia, transaction, [Read_one,[1]] ) ),
    %%Restart Mnesia on N2 and verify the contents there.
    ?match(ok, rpc:call(N2, mnesia, start, [])),
    ?match(ok, rpc:call(N2, mnesia, wait_for_tables, [[test_rec], 30000])),
    ?match( {atomic,[#test_rec{key=1,val=22}]},
           rpc:call(N2, mnesia, transaction, [Read_one,[1]] ) ),
    %%Check that the start of Mnesai on N2 did not affect the contents on N1
    ?match( {atomic,[#test_rec{key=1,val=11}]},
           rpc:call(N1, mnesia, transaction, [Read_one,[1]] ) ),
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_directly_when_all_are_ram_copiesA(doc) ->
    ["Tables that are RAM copies only shall also be loaded directly. ",
     "1. N1 and N2 has RAM copies of a table, stop N1 before N2. ",
     "2. When N1 starts he shall have access to the table ",
     "   without having to start N2" ];
load_directly_when_all_are_ram_copiesA(suite) -> [];
load_directly_when_all_are_ram_copiesA(Config) when is_list(Config) ->
    [N1, N2] = Nodes = ?acquire_nodes(2, Config),
    
    ?match({atomic,ok},
           mnesia:create_table(test_rec,
                               [{ram_copies,Nodes},
                                {attributes,record_info(fields,test_rec)}]
                              ) ),
    ?match( Nodes, mnesia:table_info(test_rec,ram_copies) ),
    ?match( [], mnesia:table_info(test_rec,disc_copies) ),
    ?match( [], mnesia:table_info(test_rec,disc_only_copies) ),
    Write_one = fun(Value) -> mnesia:write(#test_rec{key=2,val=Value}) end,
    Read_one  = fun()      -> mnesia:read({test_rec,2}) end,
    %%Write a value one N1 that we may test against later
    ?match({atomic,ok},
           rpc:call( N1, mnesia, transaction, [Write_one,[11]] ) ),
    %%Stop Mnesia on N1
    ?match([], mnesia_test_lib:kill_mnesia([N1])),
    %%Write a value and check result (on N2; not possible on N1
    %%since Mnesia is stopped there).
    ?match({atomic,ok}, rpc:call(N2,mnesia,transaction,[Write_one,[22]]) ),
    ?match({atomic,[#test_rec{key=2,val=22}]},
           rpc:call(N2,mnesia,transaction,[Read_one]) ),
    %%Stop Mnesia on N2
    ?match([], mnesia_test_lib:kill_mnesia([N2])),
    %%Restart Mnesia on N1 verify that we can access test_rec from
    %%N1 without starting Mnesia on N2.
    ?match(ok, rpc:call(N1, mnesia, start, [])),
    ?match(ok, rpc:call(N1, mnesia, wait_for_tables, [[test_rec], 30000])),
    ?match({atomic,[]}, rpc:call(N1,mnesia,transaction,[Read_one])),
    ?match({atomic,ok}, rpc:call(N1,mnesia,transaction,[Write_one,[33]])),
    ?match({atomic,[#test_rec{key=2,val=33}]},
           rpc:call(N1,mnesia,transaction,[Read_one])),
    %%Restart Mnesia on N2 and verify the contents there.
    ?match([], mnesia_test_lib:start_mnesia([N2], [test_rec])),
    ?match( {atomic,[#test_rec{key=2,val=33}]},
           rpc:call(N2, mnesia, transaction, [Read_one] ) ),
    %%Check that the start of Mnesai on N2 did not affect the contents on N1
    ?match( {atomic,[#test_rec{key=2,val=33}]},
           rpc:call(N1, mnesia, transaction, [Read_one] ) ),
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_directly_when_all_are_ram_copiesB(doc) ->
    ["Tables that are RAM copies only shall be loaded from a replicat ",
     "when possible. ",
     "1. N1 and N2 has RAM copies of a table, stop N1 before N2.",
     "2. Now start N2 first and then N1, N1 shall then load the table ",
     "   from N2."];
load_directly_when_all_are_ram_copiesB(suite) -> [];
load_directly_when_all_are_ram_copiesB(Config) when is_list(Config) ->
    [N1, N2] = Nodes = ?acquire_nodes(2, Config),
    ?match({atomic,ok},
           mnesia:create_table(test_rec,
                               [{ram_copies,Nodes},
                                {attributes,record_info(fields,test_rec)}]
                              ) ),
    ?match( Nodes, mnesia:table_info(test_rec,ram_copies) ),
    ?match( [], mnesia:table_info(test_rec,disc_copies) ),
    ?match( [], mnesia:table_info(test_rec,disc_only_copies) ),
    Write_one = fun(Value) -> mnesia:write(#test_rec{key=3,val=Value}) end,
    Read_one  = fun()      -> mnesia:read( {test_rec, 3}) end,
    %%Write a value one N1 that we may test against later
    ?match({atomic,ok},
           rpc:call( N1, mnesia, transaction, [Write_one,[11]] ) ),
    ?match({atomic,[#test_rec{key=3,val=11}]},
           rpc:call(N2,mnesia,transaction,[Read_one]) ),
    %%Stop Mnesia on N1
    ?match([], mnesia_test_lib:kill_mnesia([N1])),
    %%Write a value and check result (on N2; not possible on N1
    %%since Mnesia is stopped there).
    ?match({atomic,ok}, rpc:call(N2,mnesia,transaction,[Write_one,[22]]) ),
    ?match({atomic,[#test_rec{key=3,val=22}]},
           rpc:call(N2,mnesia,transaction,[Read_one]) ),
    %%Stop Mnesia on N2
    ?match([], mnesia_test_lib:kill_mnesia([N2])),
    %%Restart Mnesia on N2 verify that we can access test_rec from
    %%N2 without starting Mnesia on N1.
    ?match(ok, rpc:call(N2, mnesia, start, [])),
    ?match(ok, rpc:call(N2, mnesia, wait_for_tables, [[test_rec], 30000])),
    ?match({atomic,[]}, rpc:call(N2,mnesia,transaction,[Read_one])),
    ?match({atomic,ok}, rpc:call(N2,mnesia,transaction,[Write_one,[33]])),
    ?match({atomic,[#test_rec{key=3,val=33}]},
           rpc:call(N2,mnesia,transaction,[Read_one])),
    %%Restart Mnesia on N1 and verify the contents there.
    ?match([], mnesia_test_lib:start_mnesia([N1], [test_rec])),
    ?match( {atomic,[#test_rec{key=3,val=33}]},
           rpc:call(N1,mnesia,transaction,[Read_one])),
    %%Check that the start of Mnesai on N1 did not affect the contents on N2
    ?match( {atomic,[#test_rec{key=3,val=33}]},
           rpc:call(N2,mnesia,transaction,[Read_one])),
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


late_load_all_ram_cs_ram_nodes1(suite) -> [];
late_load_all_ram_cs_ram_nodes1(Config) when is_list(Config) ->
    [N1, N2] = mnesia_test_lib:prepare_test_case([{init_test_case, [mnesia]},
						  delete_schema,
						  {reload_appls, [mnesia]}],
						 2, Config, ?FILE, ?LINE),
    Res = late_load_when_all_are_ram_copies_on_ram_nodes(N1, [N2], Config),
    mnesia_test_lib:prepare_test_case([{reload_appls, [mnesia]}],
				      2, Config, ?FILE, ?LINE),
    Res.

late_load_all_ram_cs_ram_nodes2(suite) -> [];
late_load_all_ram_cs_ram_nodes2(Config) when is_list(Config) ->
    [N1, N2, N3] = mnesia_test_lib:prepare_test_case([{init_test_case, [mnesia]},
						      delete_schema,
						      {reload_appls, [mnesia]}],
						     3, Config, ?FILE, ?LINE),
    Res = late_load_when_all_are_ram_copies_on_ram_nodes(N1, [N2, N3], Config),
    mnesia_test_lib:prepare_test_case([{reload_appls, [mnesia]}],
				      3, Config, ?FILE, ?LINE),
    Res.

late_load_when_all_are_ram_copies_on_ram_nodes(DiscNode, RamNs, _Config)
        when DiscNode == node() ->
    ?match(ok, mnesia:create_schema([DiscNode])),
    ?match(ok, mnesia:start()),
    Nodes = [DiscNode | RamNs],
    Extra = [{extra_db_nodes, Nodes}],
    Ok = [ok || _ <- RamNs],
    ?match({Ok, []}, rpc:multicall(RamNs, mnesia, start, [Extra])),
    ?match([], wait_until_running(Nodes)),

    LastRam = lists:last(RamNs),
    %% ?match({atomic, ok},
    %%	   mnesia:add_table_copy(schema, LastRam, ram_copies)),
    Def = [{ram_copies, RamNs}, {attributes, record_info(fields, test_rec)}],
    ?match({atomic,ok}, mnesia:create_table(test_rec, Def)),
    ?verify_mnesia(Nodes, []),
    ?match([], mnesia_test_lib:stop_mnesia(RamNs)),
    ?match(stopped, mnesia:stop()),
    ?match(ok, mnesia:start()),

    Rec1 = #test_rec{key=3, val=33},
    Rec2 = #test_rec{key=4, val=44},
    
    FirstRam = hd(RamNs),
    ?match(ok, rpc:call(FirstRam, mnesia, start, [Extra])),
    ?match(ok, rpc:call(FirstRam, mnesia, wait_for_tables,
			[[test_rec], 30000])),
    ?match(ok, rpc:call(FirstRam, mnesia, dirty_write,[Rec1])),
    ?match(ok, mnesia:wait_for_tables([test_rec], 30000)),
    mnesia:dirty_write(Rec2),

    if
	FirstRam /= LastRam ->
	    ?match(ok, rpc:call(LastRam, mnesia, start, [Extra])),
	    ?match(ok, rpc:call(LastRam, mnesia, wait_for_tables,
				[[test_rec], 30000]));
	true ->
	    ignore
    end,
    ?match([Rec1], rpc:call(LastRam, mnesia, dirty_read, [{test_rec, 3}])),
    ?match([Rec2], rpc:call(LastRam, mnesia, dirty_read, [{test_rec, 4}])),
    ?verify_mnesia(Nodes, []).

wait_until_running(Nodes) ->
    wait_until_running(Nodes, 30).

wait_until_running(Nodes, Times) when Times > 0->
    Alive = mnesia:system_info(running_db_nodes),
    case Nodes -- Alive of
	[] ->
	    [];
	Remaining ->
	    timer:sleep(timer:seconds(1)),
	    wait_until_running(Remaining, Times - 1)
    end;
wait_until_running(Nodes, _) ->
    Nodes.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load_when_last_replica_becomes_available(doc) ->
    ["Check that when all Mnesia nodes die at the same instant, then the ",
     "replicated table shall be accessible when the last node is started ",
     "again.",
     "Checked by cheating. Start Mnesia on N1, N2, N3. Have a table ",
     "replicated on disc on all three nodes, fill in some transactions, ",
     "install a fallback. Restart mnesia on all nodes"
     "This is the cheat and it simulates that all nodes died at the same ",
     "time. Check that the table is only accessible after the last node ",
     "has come up."];
load_when_last_replica_becomes_available(suite) -> [];
load_when_last_replica_becomes_available(Config) when is_list(Config) ->
    [N1, N2, N3] = Nodes = ?acquire_nodes(3, Config),
    ?match({atomic,ok},
           mnesia:create_table(test_rec,
                               [{disc_copies,Nodes},
                                {attributes,record_info(fields,test_rec)}]
                              ) ),
    ?match( [], mnesia:table_info(test_rec,ram_copies) ),
    ?match( Nodes, mnesia:table_info(test_rec,disc_copies) ),
    ?match( [], mnesia:table_info(test_rec,disc_only_copies) ),
    Write_one = fun(Key,Val)->mnesia:write(#test_rec{key=Key,val=Val}) end,
    Read_one  = fun(Key)    ->mnesia:read( {test_rec, Key}) end,
    %%Write one value from each node.
    ?match({atomic,ok},rpc:call(N1,mnesia,transaction,[Write_one,[1,11]])),
    ?match({atomic,ok},rpc:call(N2,mnesia,transaction,[Write_one,[2,22]])),
    ?match({atomic,ok},rpc:call(N3,mnesia,transaction,[Write_one,[3,33]])),
    %%Check the values
    ?match({atomic,[#test_rec{key=1,val=11}]},
           rpc:call(N2,mnesia,transaction,[Read_one,[1]]) ),
    ?match({atomic,[#test_rec{key=2,val=22}]},
           rpc:call(N3,mnesia,transaction,[Read_one,[2]]) ),
    ?match({atomic,[#test_rec{key=3,val=33}]},
           rpc:call(N1,mnesia,transaction,[Read_one,[3]]) ),
    
    ?match(ok, mnesia:backup("test_last_replica")),
    ?match(ok, mnesia:install_fallback("test_last_replica")),
    file:delete("test_last_replica"),
    %%Stop Mnesia on all three nodes   
    ?match([], mnesia_test_lib:kill_mnesia(Nodes)),

    %%Start Mnesia on one node, make sure that test_rec is not available
    ?match(ok, rpc:call(N2, mnesia, start, [])),
    ?match({timeout,[test_rec]},
           rpc:call(N2, mnesia, wait_for_tables, [[test_rec], 10000])),
    ?match(ok, rpc:call(N1, mnesia, start, [])),
    ?match({timeout,[test_rec]},
           rpc:call(N1, mnesia, wait_for_tables, [[test_rec], 10000])),
    %%Start the third node
    ?match(ok, rpc:call(N3, mnesia, start, [])),
    %%Make sure that the table is loaded everywhere
    ?match(ok, rpc:call(N3, mnesia, wait_for_tables, [[test_rec], 30000])),
    ?match(ok, rpc:call(N2, mnesia, wait_for_tables, [[test_rec], 30000])),
    ?match(ok, rpc:call(N1, mnesia, wait_for_tables, [[test_rec], 30000])),

    %%Check the values
    ?match({atomic,[#test_rec{key=1,val=11}]},
           rpc:call(N2,mnesia,transaction,[Read_one,[1]]) ),
    ?match({atomic,[#test_rec{key=2,val=22}]},
           rpc:call(N3,mnesia,transaction,[Read_one,[2]]) ),
    ?match({atomic,[#test_rec{key=3,val=33}]},
           rpc:call(N1,mnesia,transaction,[Read_one,[3]]) ),
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_when_down_from_all_other_replica_nodes(doc) ->
    ["The table can be loaded if this node was the last one surviving. ",
     "Check this by having N1, N2, N3 and a table replicated on all those ",
     "nodes. Then kill them in the N1, N2, N3 order. Then start N3 and ",
     "verify that the table is available with correct contents."];
load_when_down_from_all_other_replica_nodes(suite) -> [];
load_when_down_from_all_other_replica_nodes(Config) when is_list(Config) ->
    [N1, N2, N3] = Nodes = ?acquire_nodes(3, Config),
    ?match({atomic,ok},
           mnesia:create_table(test_rec,
                               [{disc_copies,Nodes},
                                {attributes,record_info(fields,test_rec)}]
                              ) ),
    ?match( [], mnesia:table_info(test_rec,ram_copies) ),
    ?match( Nodes, mnesia:table_info(test_rec,disc_copies) ),
    ?match( [], mnesia:table_info(test_rec,disc_only_copies) ),
    Write_one = fun(Key,Val)->mnesia:write(#test_rec{key=Key,val=Val}) end,
    Read_one  = fun(Key)    ->mnesia:read( {test_rec, Key}) end,
    %%Write one value from each node.
    ?match({atomic,ok},rpc:call(N1,mnesia,transaction,[Write_one,[1,111]])),
    ?match({atomic,ok},rpc:call(N2,mnesia,transaction,[Write_one,[2,222]])),
    ?match({atomic,ok},rpc:call(N3,mnesia,transaction,[Write_one,[3,333]])),
    %%Check the values
    ?match({atomic,[#test_rec{key=1,val=111}]},
           rpc:call(N2,mnesia,transaction,[Read_one,[1]]) ),
    ?match({atomic,[#test_rec{key=2,val=222}]},
           rpc:call(N3,mnesia,transaction,[Read_one,[2]]) ),
    ?match({atomic,[#test_rec{key=3,val=333}]},
           rpc:call(N1,mnesia,transaction,[Read_one,[3]]) ),
    %%Stop Mnesia on all three nodes
    ?match([], mnesia_test_lib:kill_mnesia([N1])),
    ?match({atomic,ok},rpc:call(N2,mnesia,transaction,[Write_one,[22,22]])),
    ?match([], mnesia_test_lib:kill_mnesia([N2])),
    ?match({atomic,ok},rpc:call(N3,mnesia,transaction,[Write_one,[33,33]])),
    ?match([], mnesia_test_lib:kill_mnesia([N3])),
    ?verbose("Mnesia stoped on all three nodes.~n",[]),

    %%Start Mnesia on N3; wait for 'test_rec' table to load
    ?match(ok, rpc:call(N3, mnesia, start, [])),
    ?match(ok, rpc:call(N3, mnesia, wait_for_tables, [[test_rec], 30000])),

    %%Check the values
    ?match({atomic,[#test_rec{key=1,val=111}]},
           rpc:call(N3,mnesia,transaction,[Read_one,[1]]) ),
    ?match({atomic,[#test_rec{key=2,val=222}]},
           rpc:call(N3,mnesia,transaction,[Read_one,[2]]) ),
    ?match({atomic,[#test_rec{key=3,val=333}]},
           rpc:call(N3,mnesia,transaction,[Read_one,[3]]) ),
    ?match({atomic,[#test_rec{key=22,val=22}]},
           rpc:call(N3,mnesia,transaction,[Read_one,[22]]) ),
    ?match({atomic,[#test_rec{key=33,val=33}]},
           rpc:call(N3,mnesia,transaction,[Read_one,[33]]) ),
    ?verify_mnesia([N3], [N1, N2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

late_load_transforms_into_disc_load(doc) ->
    ["Difficult case that needs instrumentation of Mnesia.",
     "A table is force loaded, and Mnesia decides to load it from another ",
     "Mnesia node because it is avaliable there. The other Mnesia node then ",
     "dies in mid copy which shall make the first Mnesia node to really ",
     "force load from disc.",
     "Check this by starting N1 and N2 and replicating a table between ",
     "them. Then kill N1 before N2. The idea is to start N2 first, then ",
     "N1 and then do a force load on N1. This force load will load from ",
     "N2 BUT N2 must be killed after the decision to load from it has ",
     "been made. tricky."];

late_load_transforms_into_disc_load(suite) ->  [];
late_load_transforms_into_disc_load(Config) when is_list(Config) ->
    ?is_debug_compiled,
    
    [Node1, Node2] = Nodes = ?acquire_nodes(2, Config),
    
    {success, [A, B]} = ?start_activities(Nodes),
    
    ?match(Node1, node(A)),
    ?match(Node2, node(B)),

    Tab = late_load_table,
    Def = [{attributes, [key, value]},
           {disc_copies, Nodes}],  
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    ?match(ok, mnesia:dirty_write({Tab, 111, 4711})),
    ?match(ok, mnesia:dirty_write({Tab, 222, 42})),

    TestPid = self(),
    DebugId = {mnesia_loader, do_get_network_copy},
    DebugFun = fun(PrevContext, EvalContext) ->
		       ?verbose("interrupt late load,  pid ~p  #~p ~n context ~p ~n",
			    [self(),PrevContext,EvalContext]),
		       
		       mnesia_test_lib:kill_mnesia([Node2]),
		       TestPid ! {self(),debug_fun_was_called},

		       ?verbose("interrupt late_log - continues ~n",[]),
		       ?deactivate_debug_fun(DebugId),
		       PrevContext+1
	       end,
    ?remote_activate_debug_fun(Node1,DebugId, DebugFun, 1),
    
    %% kill mnesia on node1
    mnesia_test_lib:kill_mnesia([Node1]),    
    %% wait a while, so  that mnesia is really down 
    timer:sleep(timer:seconds(1)), 

    ?match(ok, rpc:call(Node2, mnesia, dirty_write, [{Tab, 222, 815}])),

    %% start Mnesia on node1
    ?match(ok,mnesia:start()),
    ?match(yes, mnesia:force_load_table(Tab)),
    ?match(ok, mnesia:wait_for_tables([Tab],timer:seconds(30))),
    
    receive_messages([debug_fun_was_called]),
    
    check_tables([A],[{Tab,111},{Tab,222}],[[{Tab,111,4711}],[{Tab,222,42}]]),
    ?verify_mnesia([Node1], [Node2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

late_load_leads_to_hanging(doc) ->
    ["Difficult case that needs instrumentation of Mnesia.",
     "A table is loaded, and Mnesia decides to load it from another ",
     "Mnesia node because it has the latest copy there. ",
     "The other Mnesia node then ",
     "dies in mid copy which shall make the first Mnesia node not to ",
     "force load from disc but to wait for the other node to come up again",
     "Check this by starting N1 and N2 and replicating a table between ",
     "them. Then kill N1 before N2. The idea is to start N2 first, then ",
     "N1. This load will load from ",
     "N2 BUT N2 must be killed after the decision to load from it has ",
     "been made. tricky."];

late_load_leads_to_hanging(suite) ->  [];
late_load_leads_to_hanging(Config) when is_list(Config) ->
    ?is_debug_compiled,

    [Node1, Node2] = Nodes = ?acquire_nodes(2, Config),
    
    Tab = late_load_table,
    Def = [{attributes, [key, value]},
           {disc_copies, Nodes}],  

    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    ?match(ok, mnesia:dirty_write({Tab, 111, 4711})),
    ?match(ok, mnesia:dirty_write({Tab, 222, 42})),

    DebugId = {mnesia_loader, do_get_network_copy},
    DebugFun = fun(PrevContext, EvalContext) ->
		       ?verbose("interrupt late load,  pid ~p  #~p ~n context ~p ~n",
			    [self(), PrevContext, EvalContext]),
		       mnesia_test_lib:kill_mnesia([Node2]),
		       ?verbose("interrupt late load - continues ~n",[]),
		       ?deactivate_debug_fun(DebugId),
		       PrevContext+1
	       end,

    ?remote_activate_debug_fun(Node1,DebugId, DebugFun, 1),
    mnesia_test_lib:kill_mnesia([Node1]),
    %% wait a while, so  that mnesia is really down 
    timer:sleep(timer:seconds(1)), 
    
    ?match(ok, rpc:call(Node2, mnesia, dirty_write, [{Tab, 333, 666}])),
    
    %% start Mnesia on node1
    ?match(ok, mnesia:start()),

    ?match({timeout, [Tab]}, mnesia:wait_for_tables([Tab], timer:seconds(2))),

    ?match({'EXIT', {aborted, _}}, mnesia:dirty_read({Tab, 222})),    
    %% mnesia on node1 is waiting for node2 coming up 
    
    ?match(ok, rpc:call(Node2, mnesia, start, [])),
    ?match(ok, mnesia:wait_for_tables([Tab], timer:seconds(30))),
    ?match([{Tab, 333, 666}], mnesia:dirty_read({Tab, 333})),   
    ?verify_mnesia([Node2, Node1], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

force_load_when_nobody_intents_to_load(doc) ->
    ["Normal force load. Start N1 N2, kill in N1, N2 order. Start N1 do ",
     "force load. Did it work?"];
force_load_when_nobody_intents_to_load(suite) -> [];
force_load_when_nobody_intents_to_load(Config) when is_list(Config) ->
    [N1, N2] = Nodes = ?acquire_nodes(2, Config),
    Table = test_rec,
    Trec1a = #test_rec{key=1,val=111},
    Trec1b = #test_rec{key=1,val=333},
    Trec2a = #test_rec{key=2,val=222},
    Trec3a = #test_rec{key=3,val=333},
    Trec3b = #test_rec{key=3,val=666},

    ?match({atomic,ok}, rpc:call(N1, mnesia,create_table,
                                 [Table,
                                  [{disc_copies,Nodes},
                                   {attributes,record_info(fields,test_rec)}
                                  ] ] ) ),
    ?match( [], mnesia:table_info(Table,ram_copies) ),
    ?match( Nodes, mnesia:table_info(Table,disc_copies) ),
    ?match( [], mnesia:table_info(Table,disc_only_copies) ),
    Write_one = fun(Rec) -> mnesia:write(Rec) end,
    Read_one  = fun(Key) -> mnesia:read({Table, Key}) end,
    %%Write one value
    ?match({atomic,ok},rpc:call(N1,mnesia,transaction,[Write_one,[Trec1a]])),
    %%Check it
    ?match({atomic,[Trec1a]},rpc:call(N2,mnesia,transaction,[Read_one,[1]]) ),
    %%Shut down mnesia on N1
    ?match([], mnesia_test_lib:stop_mnesia([N1])),
    %%Write and check value while N1 is down
    ?match({atomic,ok},rpc:call(N2,mnesia,transaction,[Write_one,[Trec1b]])),
    ?match({atomic,ok},rpc:call(N2,mnesia,transaction,[Write_one,[Trec2a]])),
    ?match({atomic,ok},rpc:call(N2,mnesia,transaction,[Write_one,[Trec3a]])),
    ?match({aborted,{node_not_running,N1}},
           rpc:call(N1,mnesia,transaction,[Read_one,[2]]) ),
    ?match({atomic,[Trec1b]},rpc:call(N2,mnesia,transaction,[Read_one,[1]]) ),
    ?match({atomic,[Trec2a]},rpc:call(N2,mnesia,transaction,[Read_one,[2]]) ),
    ?match({atomic,[Trec3a]},rpc:call(N2,mnesia,transaction,[Read_one,[3]]) ),
    %%Shut down Mnesia on N2
    ?match([], mnesia_test_lib:stop_mnesia([N2])),

    %%Restart Mnesia on N1
    ?match(ok, rpc:call(N1, mnesia, start, [])),
    %%Check that table is not available (waiting for N2)
    ?match({timeout,[Table]},
           rpc:call(N1, mnesia, wait_for_tables, [[Table], 3000])),

    %%Force load on N1
    ?match(yes,rpc:call(N1,mnesia,force_load_table,[Table])),
    %%Check values
    ?match({atomic,[Trec1a]},rpc:call(N1,mnesia,transaction,[Read_one,[1]]) ),
    ?match({atomic,[]},     rpc:call(N1,mnesia,transaction,[Read_one,[2]]) ),
    ?match({atomic,[]},     rpc:call(N1,mnesia,transaction,[Read_one,[3]]) ),
    %%Write a value for key=3
    ?match({atomic,ok},rpc:call(N1,mnesia,transaction,[Write_one,[Trec3b]])),

    %%Restart N2 and check values
    ?match(ok, rpc:call(N2, mnesia, start, [])),
    ?match(ok, rpc:call(N2, mnesia, wait_for_tables, [[Table], 30000])),

    ?match({atomic,[Trec1a]},rpc:call(N1,mnesia,transaction,[Read_one,[1]]) ),
    ?match({atomic,[Trec1a]},rpc:call(N2,mnesia,transaction,[Read_one,[1]]) ),

    ?match({atomic,[]},rpc:call(N1,mnesia,transaction,[Read_one,[2]]) ),
    ?match({atomic,[]},rpc:call(N2,mnesia,transaction,[Read_one,[2]]) ),

    ?match({atomic,[Trec3b]},rpc:call(N1,mnesia,transaction,[Read_one,[3]]) ),
    ?match({atomic,[Trec3b]},rpc:call(N2,mnesia,transaction,[Read_one,[3]]) ),
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

force_load_when_someone_has_decided_to_load(doc) ->
    ["Difficult case that needs instrumentation of Mnesia.",
     "Start N1 and N2, replicate table, kill in N1, N2 order. Start N2 ",
     "and start N1 before N2 has really loaded the table but after N2 has ",
     "decided to load it."];

force_load_when_someone_has_decided_to_load(suite) -> [];
force_load_when_someone_has_decided_to_load(Config) when is_list(Config) ->
    ?is_debug_compiled,
    
    [Node1, Node2] = Nodes = ?acquire_nodes(2, Config),   
    {success, [A, B]} = ?start_activities(Nodes),        
    ?match(Node1, node(A)), %% Just to check :)
    ?match(Node2, node(B)),

    Tab = late_load_table,
    Def = [{attributes, [key, value]}, {disc_copies, Nodes}],  

    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    ?match(ok, mnesia:dirty_write({Tab, 111, 4711})),
    ?match(ok, mnesia:dirty_write({Tab, 222, 42})),

    Self = self(),
    DebugId = {mnesia_controller, late_disc_load},
    DebugFun = fun(PrevContext, EvalContext) ->
		       ?verbose("interrupt late disc load,
                             pid ~p  #~p ~n context ~p ~n",
			    [self(),PrevContext,EvalContext]),
		       Self ! {self(), fun_in_postion},
		       wait_for_signal(),
		       ?verbose("interrupt late disc load - continues ~n",[]),
		       ?deactivate_debug_fun(DebugId),
		       PrevContext+1
	       end,
 
    %% kill mnesia on node1
    mnesia_test_lib:kill_mnesia([Node1]),
    %% wait a while, so  that mnesia is really down 
    timer:sleep(timer:seconds(1)), 

    ?match(ok, rpc:call(Node2, mnesia, dirty_write, [{Tab, 222, 815}])),
    %% kill mnesia on node2
    mnesia_test_lib:kill_mnesia([Node2]),
    %% wait a while, so that mnesia is really down 
    timer:sleep(timer:seconds(1)), 

    ?remote_activate_debug_fun(Node2,DebugId, DebugFun, 1),

    B ! fun() -> mnesia:start() end,    
    [{Mnesia_Pid, fun_in_postion}] = receive_messages([fun_in_postion]),    
    
    %% start Mnesia on node1
    A ! fun() -> mnesia:start() end, 
    ?match_receive(timeout),
% Got some problem with this testcase when we modified mnesia init 
% These test cases are very implementation dependent!
%    A ! fun() -> mnesia:wait_for_tables([Tab], 3000) end,
%    ?match_receive({A, {timeout, [Tab]}}),
    A ! fun() -> mnesia:force_load_table(Tab) end,
    ?match_receive(timeout),

    Mnesia_Pid ! continue,
    ?match_receive({B, ok}),
    ?match_receive({A, ok}),
    ?match_receive({A, yes}),

    B ! fun() -> mnesia:wait_for_tables([Tab], 10000) end,
    ?match_receive({B, ok}),
    ?match(ok, mnesia:wait_for_tables([Tab], timer:seconds(30))),
    ?match([{Tab, 222, 815}], mnesia:dirty_read({Tab, 222})),
    ?verify_mnesia(Nodes, []).

wait_for_signal() ->
    receive 
	continue -> ok
    %% Don't eat any other mnesia internal msg's
    after 
	timer:minutes(2) -> ?error("Timedout in wait_for_signal~n", [])
    end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

force_load_when_someone_else_has_loaded(doc) ->
    ["Normal case. Do a force load when somebody else has loaded the table. ",
     "Start N1, N2, kill in N1, N2 order. Start N2 load the table, start N1 ",
     "force load. Did it work? (i.e: did N1 load the table from N2 as that",
     "one is the latest version and it is available on N2)"];

force_load_when_someone_else_has_loaded(suite) -> [];
force_load_when_someone_else_has_loaded(Config) when is_list(Config) ->
    [N1, N2] = Nodes = ?acquire_nodes(2, Config),
    Table = test_rec,
    Trec1 = #test_rec{key=1,val=111},
    Trec2 = #test_rec{key=1,val=222},

    ?match({atomic,ok}, rpc:call(N1, mnesia,create_table,
                                 [Table,
                                  [{disc_copies,Nodes},
                                   {attributes,record_info(fields,test_rec)}
                                  ] ] ) ),
    ?match( [], mnesia:table_info(Table,ram_copies) ),
    ?match( Nodes, mnesia:table_info(Table,disc_copies) ),
    ?match( [], mnesia:table_info(Table,disc_only_copies) ),
    Write_one = fun(Rec) -> mnesia:write(Rec) end,
    Read_one  = fun(Key) -> mnesia:read({Table, Key}) end,
    %%Write one value
    ?match({atomic,ok},rpc:call(N1,mnesia,transaction,[Write_one,[Trec1]])),
    %%Check it
    ?match({atomic,[Trec1]},rpc:call(N2,mnesia,transaction,[Read_one,[1]]) ),
    %%Shut down mnesia
    ?match([], mnesia_test_lib:stop_mnesia([N1])),
    timer:sleep(500),
    ?match([], mnesia_test_lib:stop_mnesia([N2])),
    %%Restart Mnesia on N2;wait for tables to load
    ?match(ok, rpc:call(N2, mnesia, start, [])),
    ?match(ok, rpc:call(N2, mnesia, wait_for_tables, [[test_rec], 30000])),
    %%Write one value
    ?match({atomic,ok},rpc:call(N2,mnesia,transaction,[Write_one,[Trec2]])),
    %%Start on N1; force load
    ?match(ok, rpc:call(N1, mnesia, start, [])),
    %%Force load from file
    ?match(yes, rpc:call(N1,mnesia,force_load_table,[Table])),
    %%Check the value
    ?match({atomic,[Trec2]},rpc:call(N1,mnesia,transaction,[Read_one,[1]]) ),
       %%               === there must be a Trec2 here !!!!
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

force_load_when_we_has_loaded(doc) ->
    ["Force load a table we already have loaded"];
force_load_when_we_has_loaded(suite) -> [];
force_load_when_we_has_loaded(Config) when is_list(Config) ->
    [N1] = Nodes = ?acquire_nodes(1, Config),
    Table = test_rec,
    Trec1 = #test_rec{key=1,val=111},
    Trec2 = #test_rec{key=1,val=222},

    ?match({atomic,ok}, rpc:call(N1, mnesia,create_table,
                                 [Table,
                                  [{disc_copies,Nodes},
                                   {attributes,record_info(fields,test_rec)}
                                  ] ] ) ),
    ?match( [], mnesia:table_info(Table,ram_copies) ),
    ?match( Nodes, mnesia:table_info(Table,disc_copies) ),
    ?match( [], mnesia:table_info(Table,disc_only_copies) ),
    Write_one = fun(Rec) -> mnesia:write(Rec) end,
    Read_one  = fun(Key) -> mnesia:read({Table, Key}) end,
    %%Write one value
    ?match({atomic,ok},rpc:call(N1,mnesia,transaction,[Write_one,[Trec1]])),
    %%Check it
    ?match({atomic,[Trec1]},rpc:call(N1,mnesia,transaction,[Read_one,[1]]) ),
    %%Shut down mnesia
    ?match([], mnesia_test_lib:stop_mnesia(Nodes)),
    %%Restart Mnesia;wait for tables to load
    ?match([], mnesia_test_lib:start_mnesia(Nodes, [Table])),
    %%Write one value
    ?match({atomic,ok},rpc:call(N1,mnesia,transaction,[Write_one,[Trec2]])),
    %%Force load from file
    ?match(yes, rpc:call(N1,mnesia,force_load_table,[Table])),
    %%Check the value
    ?match({atomic,[Trec2]},rpc:call(N1,mnesia,transaction,[Read_one,[1]]) ),
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

force_load_on_a_non_local_table(doc) ->
    ["This is NOT allowed, the test case is a negative test",
     "Force load on a table that isn't replicated on this node."];
force_load_on_a_non_local_table(suite) -> [];
force_load_on_a_non_local_table(Config) when is_list(Config) ->
    [N1, N2, N3] = Nodes = ?acquire_nodes( 3, Config),
    TableNodes = lists:sublist(Nodes,2),
    Table = test_rec,
    Trec1 = #test_rec{key=1,val=11},

    ?match({atomic,ok}, rpc:call(N1, mnesia,create_table,
                                 [Table,
                                  [{disc_copies,TableNodes},
                                   {attributes,record_info(fields,test_rec)}
                                  ] ] ) ),
    ?match( [], mnesia:table_info(Table,ram_copies) ),
    ?match( TableNodes, mnesia:table_info(Table,disc_copies) ),
    ?match( [], mnesia:table_info(Table,disc_only_copies) ),
    Write_one = fun(Rec) -> mnesia:write(Rec) end,
    Read_one  = fun(Key) -> mnesia:read({Table, Key}) end,
    %%Write one value
    ?match({atomic,ok},rpc:call(N1,mnesia,transaction,[Write_one,[Trec1]])),
    %%Check it from the other nodes
    ?match({atomic,[Trec1]},rpc:call(N2,mnesia,transaction,[Read_one,[1]]) ),
    ?match({atomic,[Trec1]},rpc:call(N3,mnesia,transaction,[Read_one,[1]]) ),

    %%Make sure that Table is non-local
    ?match_inverse(N3, rpc:call(N3,mnesia,table_info,[Table,where_to_read])),
    %%Try to force load it
    ?match(yes, rpc:call(N3,mnesia,force_load_table,[Table])),
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

force_load_when_the_table_does_not_exist(doc) ->
    ["This is NOT allowed, the test case is a negative test",
     "Force load on a table that doesn't exist."];
force_load_when_the_table_does_not_exist(suite) -> [];
force_load_when_the_table_does_not_exist(Config) when is_list(Config) ->
    Nodes = ?acquire_nodes( 2, Config),
    
    %%Dummy table
    ?match({atomic,ok},
           mnesia:create_table(test_rec,
                               [{disc_copies,Nodes},
                                {attributes,record_info(fields,test_rec)}]
                              ) ),
    ?match( [], mnesia:table_info(test_rec,ram_copies) ),
    ?match( Nodes, mnesia:table_info(test_rec,disc_copies) ),
    ?match( [], mnesia:table_info(test_rec,disc_only_copies) ),
    Tab = dummy,
    %%Make sure that Tab is an unknown table
    ?match( false, lists:member(Tab,mnesia:system_info(tables)) ),
    ?match( {error, {no_exists, Tab}}, mnesia:force_load_table(Tab) ),
    ?verify_mnesia(Nodes, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-define(SDwrite(Tup), fun() -> mnesia:write(Tup) end).			            

master_nodes(suite) -> [];
master_nodes(Config) when is_list(Config) ->
    [A, B, C] = Nodes = ?acquire_nodes(3, Config),
    Tab = test_table_master_nodes,    
    ?match({atomic,ok}, mnesia:create_table(Tab, [{disc_copies, Nodes}])),

    %% Test one: Master A and the table should be loaded from A

    ?match(ok, rpc:call(A, mnesia, set_master_nodes, [Tab, [A]])),
    ?match({atomic, ok}, mnesia:sync_transaction(?SDwrite({Tab, 1, init}))),
    
    mnesia_test_lib:stop_mnesia([A]),
    ?match({atomic, ok}, rpc:call(B, mnesia, sync_transaction, [?SDwrite({Tab, 1, updated})])),
    ?match(ok, rpc:call(A, mnesia, start, [])),
    ?match(ok, rpc:call(A, mnesia, wait_for_tables, [[Tab], 3000])),
    
    ?match([{Tab, 1, init}], rpc:call(A, mnesia, dirty_read, [{Tab, 1}])),
    ?match([{Tab, 1, updated}], rpc:call(B, mnesia, dirty_read, [{Tab, 1}])),
    ?match([{Tab, 1, updated}], rpc:call(C, mnesia, dirty_read, [{Tab, 1}])),
    
    %% Test 2: Master [A,B] and B is Up the table should be loaded from B

    ?match(ok, rpc:call(A, mnesia, set_master_nodes, [Tab, [A, B]])),
    ?match({atomic, ok}, mnesia:sync_transaction(?SDwrite({Tab, 1, init}))),
    
    mnesia_test_lib:stop_mnesia([A]),
    ?match({atomic, ok}, rpc:call(B, mnesia, sync_transaction, [?SDwrite({Tab, 1, updated})])),
    ?match(ok, rpc:call(A, mnesia, start, [])),
    ?match(ok, rpc:call(A, mnesia, wait_for_tables, [[Tab], 3000])),
    
    ?match([{Tab, 1, updated}], rpc:call(A, mnesia, dirty_read, [{Tab, 1}])),
    ?match([{Tab, 1, updated}], rpc:call(B, mnesia, dirty_read, [{Tab, 1}])),
    ?match([{Tab, 1, updated}], rpc:call(C, mnesia, dirty_read, [{Tab, 1}])),

    %% Test 3: Master [A,B] and B is down the table should be loaded from A

    ?match(ok, rpc:call(A, mnesia, set_master_nodes, [Tab, [A, B]])),
    ?match({atomic, ok}, mnesia:sync_transaction(?SDwrite({Tab, 1, init}))),
    
    mnesia_test_lib:stop_mnesia([A]),
    ?match({atomic, ok}, rpc:call(B, mnesia, sync_transaction, [?SDwrite({Tab, 1, updated})])),
    mnesia_test_lib:stop_mnesia([B]),
    ?match(ok, rpc:call(A, mnesia, start, [])),
    ?match(ok, rpc:call(A, mnesia, wait_for_tables, [[Tab], 3000])),
    
    ?match(ok, rpc:call(B, mnesia, start, [])),
    ?match(ok, rpc:call(B, mnesia, wait_for_tables, [[Tab], 3000])),
    
    ?match([{Tab, 1, init}], rpc:call(A, mnesia, dirty_read, [{Tab, 1}])),
    ?match([{Tab, 1, _Unknown}], rpc:call(B, mnesia, dirty_read, [{Tab, 1}])),
    ?match([{Tab, 1, updated}], rpc:call(C, mnesia, dirty_read, [{Tab, 1}])),
    
    %% Test 4: Master [B] and B is Up the table should be loaded from B

    ?match(ok, rpc:call(A, mnesia, set_master_nodes, [Tab, [B]])),
    ?match({atomic, ok}, mnesia:sync_transaction(?SDwrite({Tab, 1, init}))),
    
    mnesia_test_lib:stop_mnesia([A]),
    ?match({atomic, ok}, rpc:call(B, mnesia, sync_transaction, [?SDwrite({Tab, 1, updated})])),
    ?match(ok, rpc:call(A, mnesia, start, [])),
    ?match(ok, rpc:call(A, mnesia, wait_for_tables, [[Tab], 3000])),
    
    ?match([{Tab, 1, updated}], rpc:call(A, mnesia, dirty_read, [{Tab, 1}])),
    ?match([{Tab, 1, updated}], rpc:call(B, mnesia, dirty_read, [{Tab, 1}])),
    ?match([{Tab, 1, updated}], rpc:call(C, mnesia, dirty_read, [{Tab, 1}])),
    
    %% Test 5: Master [B] and B is down the table should not be loaded

    ?match(ok, rpc:call(A, mnesia, set_master_nodes, [Tab, [B]])),
    ?match({atomic, ok}, mnesia:sync_transaction(?SDwrite({Tab, 1, init}))),
    
    mnesia_test_lib:stop_mnesia([A]),
    ?match({atomic, ok}, rpc:call(B, mnesia, sync_transaction, [?SDwrite({Tab, 1, updated})])),
    mnesia_test_lib:stop_mnesia([B]),
    ?match({atomic, ok}, rpc:call(C, mnesia, sync_transaction, [?SDwrite({Tab, 1, update_2})])),
    ?match(ok, rpc:call(A, mnesia, start, [])),
    ?match({timeout, [Tab]}, rpc:call(A, mnesia, wait_for_tables, [[Tab], 2000])),

    %% Test 6: Force load on table that couldn't be loaded due to master 
    %%         table setttings, loads other active replicas i.e. from C

    ?match(yes, rpc:call(A, mnesia, force_load_table, [Tab])),
    ?match(ok, rpc:call(A, mnesia, wait_for_tables, [[Tab], 3000])),
    
    ?match(ok, rpc:call(B, mnesia, start, [])),
    ?match(ok, rpc:call(B, mnesia, wait_for_tables, [[Tab], 3000])),
    
    ?match([{Tab, 1, update_2}], rpc:call(A, mnesia, dirty_read, [{Tab, 1}])),
    ?match([{Tab, 1, update_2}], rpc:call(B, mnesia, dirty_read, [{Tab, 1}])),
    ?match([{Tab, 1, update_2}], rpc:call(C, mnesia, dirty_read, [{Tab, 1}])),

    %% Test 7: Master [B] and B is down the table should not be loaded, 
    %%         force_load when there are no active replicas availible 
    %%         should generate a load of a local table

    ?match(ok, rpc:call(A, mnesia, set_master_nodes, [Tab, [B]])),
    ?match({atomic, ok}, mnesia:sync_transaction(?SDwrite({Tab, 1, init}))),
    
    mnesia_test_lib:stop_mnesia([A]),
    ?match({atomic, ok}, rpc:call(B, mnesia, sync_transaction, [?SDwrite({Tab, 1, updated})])),
    mnesia_test_lib:stop_mnesia([B, C]),
    ?match(ok, rpc:call(A, mnesia, start, [])),
    ?match({timeout, [Tab]}, rpc:call(A, mnesia, wait_for_tables, [[Tab], 2000])),

    ?match(yes, rpc:call(A, mnesia, force_load_table, [Tab])),
    ?match([{Tab, 1, init}], rpc:call(A, mnesia, dirty_read, [{Tab, 1}])),

    ?verify_mnesia([A], [B,C]).

starting_master_nodes(suite) -> [];
starting_master_nodes(doc) -> 
    ["Complementory to TEST 5 and 6 above, if the master node (B) starts"
     " and loads the table it should be loaded on the waiting node (A) "];
starting_master_nodes(Config) when is_list(Config) ->
    [A, B, C] = Nodes = ?acquire_nodes(3, Config),
    Tab = starting_master_nodes,    
    ?match({atomic,ok}, mnesia:create_table(Tab, [{disc_copies, Nodes}])),
    %% Start by checking TEST 5 above.
    
    ?match(ok, rpc:call(A, mnesia, set_master_nodes, [Tab, [B]])),
    ?match({atomic, ok}, mnesia:sync_transaction(?SDwrite({Tab, 1, init}))),
    mnesia_test_lib:stop_mnesia([A]),
    ?match({atomic, ok}, rpc:call(B, mnesia, sync_transaction, [?SDwrite({Tab, 1, updated})])),
    mnesia_test_lib:stop_mnesia([B]),
    ?match({atomic, ok}, rpc:call(C, mnesia, sync_transaction, [?SDwrite({Tab, 1, update_2})])),
    
    ?match(ok, rpc:call(A, mnesia, start, [])),
    ?match({timeout, [Tab]}, rpc:call(A, mnesia, wait_for_tables, [[Tab], 2000])),
    %% Start the B node and the table should be loaded on A!
    ?match(ok, rpc:call(B, mnesia, start, [])),
    ?match(ok, rpc:call(B, mnesia, wait_for_tables, [[Tab], 3000])),
    ?match(ok, rpc:call(A, mnesia, wait_for_tables, [[Tab], 3000])),
    
    ?verify_mnesia([A,B,C], []).


master_on_non_local_tables(suite) -> [];
master_on_non_local_tables(Config) when is_list(Config) ->
    [A, B, C] = Nodes = ?acquire_nodes(3, Config),
    Tab = test_table_non_local,    
    ?match({atomic,ok}, mnesia:create_table(Tab, [{disc_copies, [B, C]}])),

    ?match(ok, rpc:call(A, mnesia, set_master_nodes, [Tab, [B]])),
    ?match({atomic, ok}, mnesia:sync_transaction(?SDwrite({Tab, 1, init}))),

    %% Test 1: Test that table info are updated when master node comes up
    
    mnesia_test_lib:stop_mnesia([A, B]),
    ?match({atomic, ok}, rpc:call(C, mnesia, sync_transaction, [?SDwrite({Tab, 1, updated})])),   
    ?match(ok, rpc:call(A, mnesia, start, [])),
    
    ?match({timeout, [Tab]}, rpc:call(A, mnesia, wait_for_tables, [[Tab], 2000])),
    ErrorRead = {badrpc,{'EXIT', {aborted,{no_exists,[test_table_non_local,1]}}}},
    ErrorWrite = {badrpc,{'EXIT', {aborted,{no_exists,test_table_non_local}}}},
    ?match(ErrorRead, rpc:call(A, mnesia, dirty_read, [{Tab, 1}])),
    ?match(ErrorWrite, rpc:call(A, mnesia, dirty_write, [{Tab, 1, updated_twice}])),
    
    ?match(ok, rpc:call(B, mnesia, start, [])),
    ?match(ok, rpc:call(A, mnesia, wait_for_tables, [[Tab], 2000])),
    
    ?match([{Tab, 1, updated}], rpc:call(A, mnesia, dirty_read, [{Tab, 1}])),
    ?match(B, rpc:call(A, mnesia, table_info, [Tab, where_to_read])),
    ?match({atomic, ok}, rpc:call(A, mnesia, sync_transaction, [?SDwrite({Tab, 1, init})])),

    %% Test 2: Test that table info are updated after force_load

    mnesia_test_lib:stop_mnesia([A, B]),
    ?match({atomic, ok}, rpc:call(C, mnesia, sync_transaction, [?SDwrite({Tab, 1, updated})])),   
    ?match(ok, rpc:call(A, mnesia, start, [])),
    
    ?match({timeout, [Tab]}, rpc:call(A, mnesia, wait_for_tables, [[Tab], 2000])),
    ?match(yes, rpc:call(A, mnesia, force_load_table, [Tab])),
    ?match(C, rpc:call(A, mnesia, table_info, [Tab, where_to_read])),

    ?match([{Tab, 1, updated}], rpc:call(A, mnesia, dirty_read, [{Tab, 1}])),
    ?match({atomic, ok}, rpc:call(A, mnesia, sync_transaction, [?SDwrite({Tab, 1, updated_twice})])),
    
    ?match(ok, rpc:call(B, mnesia, start, [])),
    ?match(ok, rpc:call(B, mnesia, wait_for_tables, [[Tab], 10000])),

    ?match([{Tab, 1, updated_twice}], rpc:call(A, mnesia, dirty_read, [{Tab, 1}])),
    ?match([{Tab, 1, updated_twice}], rpc:call(B, mnesia, dirty_read, [{Tab, 1}])),
    ?match([{Tab, 1, updated_twice}], rpc:call(C, mnesia, dirty_read, [{Tab, 1}])),
    
    ?verify_mnesia(Nodes, []).

remote_force_load_with_local_master_node(doc) ->
    ["Force load a table on a remote node while the ",
     "local node is down. Start the local node and ",
     "verfify that the tables is loaded from disc locally "
     "if the local node has itself as master node and ",
     "the remote node has both the local and remote node ",
     "as master nodes"];
remote_force_load_with_local_master_node(suite) -> [];
remote_force_load_with_local_master_node(Config) when is_list(Config) ->
    [A, B] = Nodes = ?acquire_nodes(2, Config),

    Tab = remote_force_load_with_local_master_node,
    ?match({atomic,ok}, mnesia:create_table(Tab, [{disc_copies, Nodes}])),
    ?match(ok, rpc:call(A, mnesia, set_master_nodes, [Tab, [A, B]])),
    ?match(ok, rpc:call(B, mnesia, set_master_nodes, [Tab, [B]])),
    
    W = fun(Who) -> mnesia:write({Tab, who, Who}) end,
    ?match({atomic, ok}, rpc:call(A,mnesia, sync_transaction, [W, [a]])),
    ?match(stopped, rpc:call(A, mnesia, stop, [])),
    ?match({atomic, ok}, rpc:call(B, mnesia, sync_transaction, [W, [b]])),
    ?match(stopped, rpc:call(B, mnesia, stop, [])),

    ?match(ok, rpc:call(A, mnesia, start, [])),
    ?match(ok, rpc:call(A, mnesia, wait_for_tables, [[Tab], 3000])),
    ?match([{Tab, who, a}], rpc:call(A, mnesia, dirty_read, [{Tab, who}])),
    
    ?match(ok, rpc:call(B, mnesia, start, [])),
    ?match(ok, rpc:call(B, mnesia, wait_for_tables, [[Tab], 3000])),
    ?match([{Tab, who, b}], rpc:call(B, mnesia, dirty_read, [{Tab, who}])),

    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dump_ram_copies(doc) -> 
    ["Check that ram_copies tables are loaded with the"
     "contents that had been dumped before Mnesia",
     "was restarted. " ];
dump_ram_copies(suite)  -> [];
dump_ram_copies(Config) when is_list(Config)  ->
    Nodes = ?acquire_nodes(3, Config),
    {success, [P1,P2,P3]} = ?start_activities(Nodes),
    
    NP1 = node(P1),
    NP2 = node(P2),
    
    {A,B,C} = case node() of
		  NP1 ->
						%?verbose("first case ~n"),
		      {P3,P2,P1};
		  NP2 ->
						%?verbose("second case ~n"),
		      {P3,P1,P2};
		  _  ->
		      {P1,P2,P3}
	      end,
    
    Node1 = node(A),
    Node2 = node(B),
    Node3 = node(C),
    
    ?verbose(" A   pid:~p  node:~p ~n",[A,Node1]),
    ?verbose(" B   pid:~p  node:~p ~n",[B,Node2]),
    ?verbose(" C   pid:~p  node:~p ~n",[C,Node3]),
    
    
    %%  ram copies table on 2 nodes
    
    Tab = dump_table,
    Def = [{attributes, [key, value]},
           {ram_copies, [Node1,Node2]}],      
    
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    
    ?match(ok, mnesia:dirty_write({Tab, 1, 4711})),
    ?match(ok, mnesia:dirty_write({Tab, 2, 42})),
    ?match(ok, mnesia:dirty_write({Tab, 3, 256})),
    
    %%  dump the table
    
    ?match( {atomic,ok}, mnesia:dump_tables([Tab])),
    
    %% perform updates (they shall be lost after kill Mnesia )
    
    ?match(ok, mnesia:dirty_write({Tab, 1, 815})),
    ?match(ok, mnesia:dirty_write({Tab, 2, 915})),
    
    %% add another replica on node3
    mnesia:add_table_copy(Tab,Node3,ram_copies),
    
    %% all 3 replicas shall have the new contents
    cross_check_tables([A,B,C],Tab,
                       {[{Tab,1,815}],[{Tab,2,915}],[{Tab,3,256}]}),
    
    %% kill mnesia on node 3
    mnesia_test_lib:kill_mnesia([Node3]),
    
    %% wait a while, so  that mnesia is really down 
    timer:sleep(timer:seconds(2)), 
    
    mnesia_test_lib:kill_mnesia([Node1,Node2]),  %% kill them as well
    timer:sleep(timer:seconds(2)), 
    
    %% start Mnesia only on node 3
    ?verbose("starting mnesia on Node3~n",[]),
    
    %% test_lib:mnesia_start doesnt work, because it waits
    %% for the schema on all nodes ... ???
    ?match(ok,rpc:call(Node3,mnesia,start,[]) ),
    ?match(ok,rpc:call(Node3,mnesia,wait_for_tables,
		       [[Tab],timer:seconds(30)]   ) ),
    
    %% node3  shall have the conents of the dump
    cross_check_tables([C],Tab,{[{Tab,1,4711}],[{Tab,2,42}],[{Tab,3,256}]}),
    
    %% start Mnesia on the other 2 nodes, too
    mnesia_test_lib:start_mnesia([Node1,Node2],[Tab]),
    
    cross_check_tables([A,B,C],Tab,
		       {[{Tab,1,4711}],[{Tab,2,42}],[{Tab,3,256}]}),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Should be in evil test suite !!!

dump_disc_copies(doc) -> 
      ["Check that it is not possible to dump disc_copies tables"];
dump_disc_copies(suite)  -> [];
dump_disc_copies(Config) when is_list(Config)  ->
    do_dump_copies(Config, disc_copies).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Should be in evil test suite !!!
dump_disc_only(doc) -> 
       ["Check that it is not possible to dump disc_only_copies tables"];
dump_disc_only(suite)  -> [];
dump_disc_only(Config) when is_list(Config)  ->
    do_dump_copies(Config,disc_only_copies).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_dump_copies(Config,Copies) ->
    [Node1] = Nodes = ?acquire_nodes(1, Config),

    Tab = dump_copies,
    Def = [{attributes, [key, value]},
           {Copies, [Node1]}],      
    
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),

    ?match(ok, mnesia:dirty_write({Tab, 1, 4711})),
    ?match(ok, mnesia:dirty_write({Tab, 2, 42})),
    ?match(ok, mnesia:dirty_write({Tab, 3, 256})),

    %%  dump the table
    ?match( {aborted, {"Only allowed on ram_copies",Tab,[Node1]}},
              mnesia:dump_tables([Tab])),

    ?match(ok, mnesia:dirty_write({Tab, 1, 815})),
    ?match(ok, mnesia:dirty_write({Tab, 2, 915})),

    %% kill mnesia on node1
    mnesia_test_lib:kill_mnesia([Node1]),
    
    %% wait a while, so  that mnesia is really down 
    timer:sleep(timer:seconds(1)), 

    mnesia_test_lib:start_mnesia([Node1],[Tab]),
    
    ?match([{Tab, 1, 815}], mnesia:dirty_read({Tab,1}) ),
    ?match([{Tab, 2, 915}], mnesia:dirty_read({Tab,2}) ),
    ?match([{Tab, 3, 256}], mnesia:dirty_read({Tab,3}) ),
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
durability_of_disc_copies(doc) -> 
       ["Perform all possible kinds of updates on tables and check"
        "whether no data is lost after a restart of Mnesia.",
        "This test is done for disc_copies"];

durability_of_disc_copies(suite) -> [];
durability_of_disc_copies(Config) when is_list(Config) -> 
       do_disc_durability(Config,disc_copies).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

durability_of_disc_only_copies(doc) -> 
       ["Perform all possible kinds of updates on tables and check"
        "whether no data is lost after a restart of Mnesia.",
        "This test is done for disc_only_copies"];
durability_of_disc_only_copies(suite) -> [];
durability_of_disc_only_copies(Config) when is_list(Config) -> 
       do_disc_durability(Config,disc_only_copies).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_disc_durability(Config,CopyType) ->
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(1)}]),
    {success, [A,B,C]} = ?start_activities(Nodes),

    Tab_set = disc_durability_set,
    Def_set = [{attributes, [key, value]},
	       {CopyType, Nodes}],  

    Tab_bag = disc_durability_bag,
    Def_bag = [{attributes, [key, value]},
               {type, bag},
               {CopyType, Nodes}],  

    ?match({atomic, ok}, mnesia:create_table(Tab_set, Def_set)),
    ?match({atomic, ok}, mnesia:create_table(Tab_bag, Def_bag)),

    %% do updates     
    ?match({atomic, ok},
	   mnesia:transaction(fun()->
				      mnesia:write({Tab_set, 11, 1111}),
				      mnesia:write({Tab_set, 22, 2222}),
				      mnesia:write({Tab_set, 33, 3333}),
				      mnesia:write({Tab_set, 55, 5555})
			      end)),
    mnesia:dirty_write({Tab_set, 44, 4444}),

    ?match({atomic, ok},
	   mnesia:transaction(fun()->
				      mnesia:write({Tab_bag, 11, a_1111}),
				      mnesia:write({Tab_bag, 11, b_1111}),
				      mnesia:write({Tab_bag, 22, a_2222}),
				      mnesia:write({Tab_bag, 22, b_2222}),
				      mnesia:write({Tab_bag, 33, a_3333}),
				      mnesia:write({Tab_bag, 33, b_3333})
			      end)),  
    ?match({atomic, ok},
	   mnesia:transaction(fun()-> mnesia:delete({Tab_set, 22}) end)),  
    ?match(ok, mnesia:dirty_delete({Tab_set, 33})),
    ?match(5558, mnesia:dirty_update_counter({Tab_set, 55}, 3)),
    ?match({atomic, ok}, 
	   mnesia:transaction(fun()->
				      mnesia:delete_object({Tab_bag, 22, b_2222})
			      end)),  
    ?match(ok, mnesia:dirty_delete_object({Tab_bag, 33, b_3333})),
    ?match(10, mnesia:dirty_update_counter({Tab_set, counter}, 10)),
    ?match({atomic, ok},  % Also syncs update_counter
	   mnesia:sync_transaction(fun() -> mnesia:write({Tab_set,66,6666}) end)),  
    
    Updated = {[[{Tab_set,counter,10}],
		[{Tab_set,counter,10}],
		[{Tab_set,counter,10}]],[]},
    ?match(Updated, rpc:multicall(Nodes, mnesia, dirty_read, [Tab_set,counter])),
    
    %%  kill mnesia on all nodes, start it again and check the data
    mnesia_test_lib:kill_mnesia(Nodes),
    mnesia_test_lib:start_mnesia(Nodes,[Tab_set,Tab_bag]),
    
    ?log("Flushed ~p ~n", [mnesia_test_lib:flush()]), %% Debugging strange msgs..
    ?log("Processes ~p ~p ~p~n", [A,B,C]),
    check_tables([A,B,C], 
		 [{Tab_set,11}, {Tab_set,22},{Tab_set,33},
		  {Tab_set,44},{Tab_set,55}, {Tab_set,66},
		  {Tab_bag,11}, {Tab_bag,22},{Tab_bag,33},
		  {Tab_set, counter}],
		 [[{Tab_set, 11, 1111}], [], [], [{Tab_set, 44, 4444}],
		  [{Tab_set, 55, 5558}], [{Tab_set, 66, 6666}],
		  lists:sort([{Tab_bag, 11, a_1111},{Tab_bag, 11, b_1111}]),
		  [{Tab_bag, 22, a_2222}], [{Tab_bag, 33, a_3333}],
		  [{Tab_set, counter, 10}]]),

    timer:sleep(1000), %% Debugging strange msgs..
    ?log("Flushed ~p ~n", [mnesia_test_lib:flush()]),
    ?verify_mnesia(Nodes, []).

%% check the contents of the table
%%
%%  all the processes in the PidList shall find all 
%%  table entries in ValList

check_tables([],_vallist,_resultList) -> ok;
check_tables([Pid|Rest],ValList,ResultList) ->
    Pid ! fun () ->
              check_values(ValList)
            end,
    ?match_receive({ Pid, ResultList }),
    check_tables(Rest,ValList,ResultList).

check_values([]) -> [];
check_values([{Tab,Key}|Rest]) ->
        Ret = lists:sort(mnesia:dirty_read({Tab,Key})),
        [Ret|check_values(Rest)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  stolen from mnesia_recovery_test.erl:

receive_messages([]) -> [];
receive_messages(ListOfMsgs) ->
    receive 
        timeout ->     
            case lists:member(timeout, ListOfMsgs) of
                false -> 
                    ?warning("I (~p) have received unexpected msg~n ~p ~n",
                        [self(),timeout]),
                    receive_messages(ListOfMsgs);
                true -> 
                    ?verbose("I (~p) got msg ~p  ~n", [self(),timeout]),
                    [ timeout | receive_messages(ListOfMsgs -- [timeout])]
            end;

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
    after timer:seconds(40) -> 
            ?error("Timeout in receive msgs while waiting for ~p~n", 
                   [ListOfMsgs])
    end.  

