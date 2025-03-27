%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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

-module(mnesia_external_backend_test).

-export([init_per_testcase/2, end_per_testcase/2,
         init_per_group/2, end_per_group/2,
         suite/0, all/0, groups/0]).

-export([
    conversion_from_external_to_disc_copies_should_not_result_in_data_loss_after_node_restart/1,
    backup_and_restore_should_work_with_external_backend/1,
    schema_creation_should_work_when_external_tables_exist/1,
    schema_merge_should_work_when_node_is_removed_from_the_cluster_and_later_rejoins/1
]).

-include("mnesia_test_lib.hrl").

-record(some_rec, {some_id :: atom(), some_int :: number(), some_string :: string()}).

all() -> [
    conversion_from_external_to_disc_copies_should_not_result_in_data_loss_after_node_restart,
    backup_and_restore_should_work_with_external_backend,
    schema_creation_should_work_when_external_tables_exist,
    schema_merge_should_work_when_node_is_removed_from_the_cluster_and_later_rejoins
].

groups() ->
    [].

init_per_testcase(Func, Conf) ->
    file:delete("bup0.BUP"),
    file:delete("bup1.BUP"),
    file:delete("bup2.BUP"),
    mnesia_test_lib:init_per_testcase(Func, Conf).

end_per_testcase(Func, Conf) ->
    mnesia_test_lib:end_per_testcase(Func, Conf).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

suite() -> [{ct_hooks,[{ts_install_cth,[{nodenames,1}]}]}].

conversion_from_external_to_disc_copies_should_not_result_in_data_loss_after_node_restart(Config) when is_list(Config) ->
    Data = [
        #some_rec{some_id = a, some_int = 1, some_string = "something" },
        #some_rec{some_id = b, some_int = 2, some_string = "anything"  },
        #some_rec{some_id = c, some_int = 3, some_string = "everything"},
        #some_rec{some_id = d, some_int = 4, some_string = "nothing"   }
    ],

    [Node] = ?acquire_nodes(1, Config),
    ?match({atomic, ok}, mnesia:create_table(table, [
        {type, ordered_set},
        {record_name, some_rec},
        {attributes, record_info(fields, some_rec)},
        {disc_copies, [Node]}
    ])),

    ?match(ok, mnesia:activity(transaction, fun() ->
        lists:foreach(fun(Elem) -> mnesia:write(table, Elem, write) end, Data)
    end)),

    ?match({atomic, ok}, mnesia:change_table_copy_type(table, Node, ext_ram_copies)),
    ?match(Data, mnesia:activity(transaction, fun() ->
        mnesia:match_object(table, #some_rec{_ = '_'}, read) end
    )),

    ?match({atomic, ok}, mnesia:change_table_copy_type(table, Node, disc_copies)),
    ?match(Data, mnesia:activity(transaction, fun() ->
        mnesia:match_object(table, #some_rec{_ = '_'}, read) end
    )),

    ?match(stopped, mnesia:stop()),
    ?match(ok, mnesia:start()),
    ?match(ok, mnesia:wait_for_tables([schema, table], 10000)),

    ?match(Data, mnesia:activity(transaction, fun() ->
        mnesia:match_object(table, #some_rec{_ = '_'}, read) end
    )),

    ?verify_mnesia([Node], []).

backup_and_restore_should_work_with_external_backend(Config) when is_list(Config) ->
    Data1 = [
        #some_rec{some_id = a, some_int = 1, some_string = "1"},
        #some_rec{some_id = b, some_int = 2, some_string = "2"},
        #some_rec{some_id = c, some_int = 3, some_string = "3"}
    ],
    Data2 = [
        #some_rec{some_id = d, some_int = 4, some_string = "4"},
        #some_rec{some_id = e, some_int = 5, some_string = "5"},
        #some_rec{some_id = f, some_int = 6, some_string = "6"}
    ],

    [Node] = ?acquire_nodes(1, Config),
    ?match({atomic, ok}, mnesia:create_table(table, [
        {type, set},
        {record_name, some_rec},
        {attributes, record_info(fields, some_rec)},
        {ext_disc_only_copies, [Node]}
    ])),

    ?match({atomic, ok}, mnesia:add_table_index(table, #some_rec.some_int)),
    ?match([], mnesia:dirty_match_object(table, #some_rec{_ = '_'})),
    ?match(ok, mnesia:backup("bup0.BUP")),

    ?match(ok, mnesia:activity(transaction, fun() ->
        lists:foreach(fun(Elem) -> mnesia:write(table, Elem, write) end, Data1)
    end)),
    ?match(ok, mnesia:backup("bup1.BUP")),

    ?match(ok, mnesia:activity(transaction, fun() ->
        lists:foreach(fun(Elem) -> mnesia:write(table, Elem, write) end, Data2)
    end)),
    ?match(ok, mnesia:backup("bup2.BUP")),

    ?match(ok, load_backup("bup0.BUP")),
    ?match([], mnesia:dirty_match_object(table, #some_rec{_ = '_'})),
    ?match([], mnesia:dirty_index_read(table, 2, #some_rec.some_int)),

    ?match(ok, load_backup("bup1.BUP")),
    Expected1 = sets:from_list(Data1),
    ?match(Expected1, sets:from_list(mnesia:dirty_match_object(table, #some_rec{_ = '_'}))),
    ?match([#some_rec{some_id = b, some_int = 2, some_string = "2"}], mnesia:dirty_index_read(table, 2, #some_rec.some_int)),

    ?match(ok, load_backup("bup2.BUP")),
    Expected2 = sets:from_list(lists:append(Data1, Data2)),
    ?match(Expected2, sets:from_list(mnesia:dirty_match_object(table, #some_rec{_ = '_'}))),
    ?match([#some_rec{some_id = b, some_int = 2, some_string = "2"}], mnesia:dirty_index_read(table, 2, #some_rec.some_int)),
    ?match([#some_rec{some_id = e, some_int = 5, some_string = "5"}], mnesia:dirty_index_read(table, 5, #some_rec.some_int)),

    ?verify_mnesia([Node], []).

schema_creation_should_work_when_external_tables_exist(Config) when is_list(Config) ->
    [Node] = ?acquire_nodes(1, Config),
    ?match({atomic, ok}, mnesia:create_table(table, [
        {type, set},
        {record_name, some_rec},
        {attributes, record_info(fields, some_rec)},
        {ext_disc_only_copies, [Node]}
    ])),
    
    ?match(stopped, mnesia:stop()),
    ?match(ok, mnesia:delete_schema([Node])),

    Ext = proplists:get_value(default_properties, Config, ?BACKEND),
    ?match(ok, mnesia:create_schema([Node], Ext)).

schema_merge_should_work_when_node_is_removed_from_the_cluster_and_later_rejoins(Config) when is_list(Config) ->
    [N1, N2] = All = ?acquire_nodes(2, Config),

    ?match({atomic,ok}, mnesia:create_table(table, [
        {type, set},
        {record_name, some_rec},
        {attributes, record_info(fields, some_rec)},
        {ext_ram_copies, [N1, N2]}
    ])),

    ?match([], mnesia_test_lib:kill_mnesia([N2])),
    ?match({atomic, ok}, mnesia:del_table_copy(schema, N2)),

    ?match([], mnesia_test_lib:start_mnesia([N2])),
    ?verify_mnesia(All, []).

load_backup(BUP) ->
    ?match(ok, mnesia:install_fallback(BUP)),
    ?match(stopped, mnesia:stop()),
    timer:sleep(3000),
    ?match(ok, mnesia:start()),
    ?match(ok, mnesia:wait_for_tables([schema, table], 5000)),
    ok.
