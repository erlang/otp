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
-module(mnesia_nice_coverage_test).
-author('hakan@erix.ericsson.se').

-export([init_per_testcase/2, end_per_testcase/2,
         init_per_group/2, end_per_group/2,
         all/0, groups/0]).

-export([nice/1]).

-include("mnesia_test_lib.hrl").

-record(nice_tab, {key, val}).

init_per_testcase(Func, Conf) ->
    mnesia_test_lib:init_per_testcase(Func, Conf).

end_per_testcase(Func, Conf) ->
    mnesia_test_lib:end_per_testcase(Func, Conf).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all() -> 
    [nice].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


nice(doc) -> [""];
nice(suite) -> [];
nice(Config) when is_list(Config) ->
    %% The whole test suite is one huge test case for the time beeing
    
    [Node1, Node2] = Nodes = ?acquire_nodes(2, Config), 
    Attrs = record_info(fields, nice_tab), 
    
    initialize(Attrs, Node1), 
    dirty_access(Node1), 
    success_and_fail(), 
    index_mgt(), 
    
    adm(Attrs, Node1, Node2), 
    snmp(Node1, Node2), 
    backup(Node1), 
    ?verify_mnesia(Nodes, []).

initialize(Attrs, Node1) ->
    ?match(Version when is_list(Version), mnesia:system_info(version)), 

    Schema = [{name, nice_tab}, 
	      {attributes, Attrs}, {ram_copies, [Node1]}], 

    ?match({_, _}, mnesia:system_info(schema_version)), 
    ?match({atomic, ok}, mnesia:create_table(Schema)), 

    ?match(ok, mnesia:info()), 
    ?match(set, mnesia:table_info(nice_tab, type)), 
    ?match(ok, mnesia:schema()), 
    ?match(ok, mnesia:schema(nice_tab)),
    ok.

dirty_access(Node1) ->
    TwoThree = #nice_tab{key=23, val=23}, 
    TwoFive  = #nice_tab{key=25, val=25}, 
    ?match([], mnesia:dirty_slot(nice_tab, 0)), 
    ?match(ok, mnesia:dirty_write(TwoThree)), 
    ?match([TwoThree], mnesia:dirty_read({nice_tab, 23})), 
    ?match(ok, mnesia:dirty_write(TwoFive)), 
    ?match(ok, mnesia:dirty_delete_object(TwoFive)), 

    ?match(23, mnesia:dirty_first(nice_tab)), 
    ?match('$end_of_table', mnesia:dirty_next(nice_tab, 23)), 
    ?match([TwoThree], mnesia:dirty_match_object(TwoThree)), 
    ?match(ok, mnesia:dirty_delete({nice_tab, 23})), 

    CounterSchema = [{ram_copies, [Node1]}], 
    ?match({atomic, ok}, mnesia:create_table(nice_counter_tab, CounterSchema)), 
    TwoFour  = {nice_counter_tab, 24, 24}, 
    ?match(ok, mnesia:dirty_write(TwoFour)), 
    ?match(34, mnesia:dirty_update_counter({nice_counter_tab, 24}, 10)), 
    TF = {nice_counter_tab, 24, 34}, 
    ?match([TF], mnesia:dirty_read({nice_counter_tab, 24})), 
    ?match(ok, mnesia:dirty_delete({nice_counter_tab, 24})), 
    ?match(ok, mnesia:dirty_delete_object(TF)),
    ok.

success_and_fail() ->
    ?match({atomic, a_good_trans}, mnesia:transaction(fun() ->good_trans()end)), 

    BadFun =
	fun() ->
		Two = #nice_tab{key=2, val=12}, 
		?match([Two], mnesia:match_object(#nice_tab{key='$1', val=12})), 
		?match([#nice_tab{key=3, val=13}], mnesia:wread({nice_tab, 3})), 
		?match(ok, mnesia:delete({nice_tab, 1})), 
		?match(ok, mnesia:delete_object(Two)), 
		mnesia:abort(bad_trans), 
		?match(bad, trans)
	end, 
    ?match({aborted, bad_trans}, mnesia:transaction(BadFun)), 
    ?match(L when is_list(L), mnesia:error_description(no_exists)), 
    ?match({atomic, ok}, mnesia:transaction(fun(A) -> lock(), A end, [ok])), 
    ?match({atomic, ok}, mnesia:transaction(fun(A) -> lock(), A end, [ok], 3)),
    ok.

good_trans() ->
    ?match([], mnesia:read(nice_tab, 3)), 
    ?match([], mnesia:read({nice_tab, 3})), 
    ?match(ok, mnesia:write(#nice_tab{key=14, val=4})), 
    ?match([14], mnesia:all_keys(nice_tab)), 

    Records = [ #nice_tab{key=K, val=K+10} || K <- lists:seq(1, 10) ], 
    Ok = [ ok || _ <- Records], 
    ?match(Ok, lists:map(fun(R) -> mnesia:write(R) end, Records)), 
    a_good_trans.


lock() ->
    ?match(ok, mnesia:s_write(#nice_tab{key=22, val=22})), 
    ?match(ok, mnesia:read_lock_table(nice_tab)), 
    ?match(ok, mnesia:write_lock_table(nice_tab)),
    ok.

index_mgt() ->
    UniversalRec = #nice_tab{key=4711, val=4711}, 
    ?match(ok, mnesia:dirty_write(UniversalRec)), 
    ValPos = #nice_tab.val, 
    ?match({atomic, ok}, mnesia:add_table_index(nice_tab, ValPos)), 

    IndexFun =
	fun() ->
		?match([UniversalRec], 
		       mnesia:index_read(nice_tab, 4711, ValPos)), 
		Pat = #nice_tab{key='$1', val=4711}, 
		?match([UniversalRec], 
		       mnesia:index_match_object(Pat, ValPos)), 
		index_trans
	end, 
    ?match({atomic, index_trans}, mnesia:transaction(IndexFun, infinity)), 
    ?match([UniversalRec], 
	   mnesia:dirty_index_read(nice_tab, 4711, ValPos)), 
    ?match([UniversalRec], 
	   mnesia:dirty_index_match_object(#nice_tab{key='$1', val=4711}, ValPos)), 

    ?match({atomic, ok}, mnesia:del_table_index(nice_tab, ValPos)),
    ok.

adm(Attrs, Node1, Node2) ->
    This = node(), 
    ?match({ok, This}, mnesia:subscribe(system)), 
    ?match({atomic, ok}, 
	   mnesia:add_table_copy(nice_tab, Node2, disc_only_copies)), 
    ?match({atomic, ok}, 
	   mnesia:change_table_copy_type(nice_tab, Node2, ram_copies)), 
    ?match({atomic, ok}, mnesia:del_table_copy(nice_tab, Node1)), 
    ?match(stopped, rpc:call(Node1, mnesia, stop, [])), 
    ?match([], mnesia_test_lib:start_mnesia([Node1, Node2], [nice_tab])), 
    ?match(ok, mnesia:wait_for_tables([schema], infinity)), 

    Transformer = fun(Rec) ->
			  list_to_tuple(tuple_to_list(Rec) ++ [initial_value])
		  end, 
    ?match({atomic, ok}, 
	   mnesia:transform_table(nice_tab, Transformer, Attrs ++ [extra])), 

    ?match({atomic, ok}, mnesia:delete_table(nice_tab)), 
    DumpSchema = [{name, nice_tab}, {attributes, Attrs}, {ram_copies, [Node2]}], 
    ?match({atomic, ok}, mnesia:create_table(DumpSchema)), 
    ?match({atomic, ok}, mnesia:dump_tables([nice_tab])), 
    ?match({atomic, ok}, mnesia:move_table_copy(nice_tab, Node2, Node1)), 

    ?match(yes, mnesia:force_load_table(nice_counter_tab)), 
    ?match(ok, mnesia:sync_log()),
    ?match(dumped, mnesia:dump_log()),
    ok.

backup(Node1) ->
    Tab = backup_nice,
    Def = [{disc_copies, [Node1]}], 
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),    
    ?match({ok,_,_}, mnesia:activate_checkpoint([{name, cp}, {max, [Tab]}])), 
    File = "nice_backup.BUP",
    File2 = "nice_backup2.BUP",
    File3 = "nice_backup3.BUP",
    ?match(ok, mnesia:backup_checkpoint(cp, File)), 
    ?match(ok, mnesia:backup_checkpoint(cp, File, mnesia_backup)), 
    ?match(ok, mnesia:deactivate_checkpoint(cp)), 
    ?match(ok, mnesia:backup(File)), 
    ?match(ok, mnesia:backup(File, mnesia_backup)), 

    Fun = fun(X, Acc) -> {[X], Acc} end,
    ?match({ok, 0}, mnesia:traverse_backup(File, File2, Fun, 0)), 
    ?match({ok, 0}, mnesia:traverse_backup(File, mnesia_backup, dummy, read_only, Fun, 0)), 
    ?match(ok, mnesia:install_fallback(File)), 
    ?match(ok, mnesia:uninstall_fallback()), 
    ?match(ok, mnesia:install_fallback(File, mnesia_backup)), 
    ?match(ok, mnesia:dump_to_textfile(File3)), 
    ?match({atomic, ok}, mnesia:load_textfile(File3)),
    ?match(ok, file:delete(File)),
    ?match(ok, file:delete(File2)),
    ?match(ok, file:delete(File3)),
    ok.

snmp(Node1, Node2) ->
    Tab = nice_snmp, 
    Def = [{disc_copies, [Node1]}, {ram_copies, [Node2]}], 
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    ?match({aborted, {badarg, Tab, _}}, mnesia:snmp_open_table(Tab, [])),
    ?match({atomic, ok}, mnesia:snmp_open_table(Tab, [{key, integer}])),
    ?match(endOfTable, mnesia:snmp_get_next_index(Tab, [0])), 
    ?match(undefined, mnesia:snmp_get_row(Tab, [0])), 
    ?match(undefined, mnesia:snmp_get_mnesia_key(Tab, [0])), 
    ?match({atomic, ok}, mnesia:snmp_close_table(Tab)),
    ok.

