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
-module(mnesia_dirty_access_test).
-author('hakan@erix.ericsson.se').
-compile([export_all]).
-include("mnesia_test_lib.hrl").

init_per_testcase(Func, Conf) ->
    mnesia_test_lib:init_per_testcase(Func, Conf).

end_per_testcase(Func, Conf) ->
    mnesia_test_lib:end_per_testcase(Func, Conf).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all() -> 
    [{group, dirty_write}, {group, dirty_read},
     {group, dirty_update_counter}, {group, dirty_delete},
     {group, dirty_delete_object},
     {group, dirty_match_object}, {group, dirty_index},
     {group, dirty_iter}, {group, admin_tests}].

groups() -> 
    [{dirty_write, [],
      [dirty_write_ram, dirty_write_disc, dirty_write_disc_only,
       dirty_write_xets]},
     {dirty_read, [],
      [dirty_read_ram, dirty_read_disc, dirty_read_disc_only, dirty_read_xets]},
     {dirty_update_counter, [],
      [dirty_update_counter_ram, dirty_update_counter_disc,
       dirty_update_counter_disc_only, dirty_update_counter_xets]},
     {dirty_delete, [],
      [dirty_delete_ram, dirty_delete_disc,
       dirty_delete_disc_only, dirty_delete_xets]},
     {dirty_delete_object, [],
      [dirty_delete_object_ram, dirty_delete_object_disc,
       dirty_delete_object_disc_only, dirty_delete_object_xets]},
     {dirty_match_object, [],
      [dirty_match_object_ram, dirty_match_object_disc,
       dirty_match_object_disc_only, dirty_match_object_xets]},
     {dirty_index, [],
      [{group, dirty_index_match_object},
       {group, dirty_index_read},
       {group, dirty_index_update}]},
     {dirty_index_match_object, [],
      [dirty_index_match_object_ram, dirty_index_match_object_disc,
       dirty_index_match_object_disc_only, dirty_index_match_object_xets]},
     {dirty_index_read, [],
      [dirty_index_read_ram, dirty_index_read_disc,
       dirty_index_read_disc_only, dirty_index_read_xets]},
     {dirty_index_update, [],
      [dirty_index_update_set_ram,  dirty_index_update_set_disc,
       dirty_index_update_set_disc_only,  dirty_index_update_set_xets,
       dirty_index_update_bag_ram, dirty_index_update_bag_disc,
       dirty_index_update_bag_disc_only, dirty_index_update_bag_xets]},
     {dirty_iter, [],
      [dirty_iter_ram, dirty_iter_disc, dirty_iter_disc_only,
       dirty_iter_xets]},
     {admin_tests, [],
      [del_table_copy_1, del_table_copy_2, del_table_copy_3,
       add_table_copy_1, add_table_copy_2, add_table_copy_3,
       add_table_copy_4, move_table_copy_1, move_table_copy_2,
       move_table_copy_3, move_table_copy_4]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Write records dirty


dirty_write_ram(suite) -> [];
dirty_write_ram(Config) when is_list(Config) ->
    dirty_write(Config, ram_copies).

dirty_write_disc(suite) -> [];
dirty_write_disc(Config) when is_list(Config) ->
    dirty_write(Config, disc_copies).

dirty_write_disc_only(suite) -> [];
dirty_write_disc_only(Config) when is_list(Config) ->
    dirty_write(Config, disc_only_copies).

dirty_write_xets(Config) when is_list(Config) ->
    dirty_write(Config, ext_ets).

dirty_write(Config, Storage) ->
    [Node1] = Nodes = ?acquire_nodes(1, Config), 
    Tab = dirty_write, 
    Def = [{attributes, [k, v]}, {Storage, [Node1]}], 
    ?match({atomic, ok},  mnesia:create_table(Tab, Def)), 
    
    ?match({'EXIT', _},  mnesia:dirty_write([])), 
    ?match({'EXIT', _},  mnesia:dirty_write({Tab, 2})), 
    ?match({'EXIT', _},  mnesia:dirty_write({foo, 2})), 
    ?match(ok,  mnesia:dirty_write({Tab, 1, 2})), 

    ?match({atomic, ok},   mnesia:transaction(fun() -> 
	    mnesia:dirty_write({Tab, 1, 2}) end)), 
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Read records dirty


dirty_read_ram(suite) -> [];
dirty_read_ram(Config) when is_list(Config) ->
    dirty_read(Config, ram_copies).

dirty_read_disc(suite) -> [];
dirty_read_disc(Config) when is_list(Config) ->
    dirty_read(Config, disc_copies).

dirty_read_disc_only(suite) -> [];
dirty_read_disc_only(Config) when is_list(Config) ->
    dirty_read(Config, disc_only_copies).

dirty_read_xets(Config) when is_list(Config) ->
    dirty_read(Config, ext_ets).

dirty_read(Config, Storage) ->
    [Node1] = Nodes = ?acquire_nodes(1, Config), 
    Tab = dirty_read, 
    Def = [{type, bag}, {attributes, [k, v]}, {Storage, [Node1]}], 
    ?match({atomic, ok},  mnesia:create_table(Tab, Def)), 
    
    ?match({'EXIT', _},  mnesia:dirty_read([])), 
    ?match({'EXIT', _},  mnesia:dirty_read({Tab})), 
    ?match({'EXIT', _},   mnesia:dirty_read({Tab, 1, 2})), 
    ?match([],  mnesia:dirty_read({Tab, 1})), 
    ?match(ok,  mnesia:dirty_write({Tab, 1, 2})), 
    ?match([{Tab, 1, 2}],  mnesia:dirty_read({Tab, 1})), 
    ?match(ok,  mnesia:dirty_write({Tab, 1, 3})), 
    ?match([{Tab, 1, 2}, {Tab, 1, 3}],  mnesia:dirty_read({Tab, 1})), 

    ?match({atomic, [{Tab, 1, 2}, {Tab, 1, 3}]},  
	   mnesia:transaction(fun() -> mnesia:dirty_read({Tab, 1}) end)), 

    ?match(false, mnesia:async_dirty(fun() -> mnesia:is_transaction() end)),
    ?match(false, mnesia:sync_dirty(fun() -> mnesia:is_transaction() end)),
    ?match(false, mnesia:ets(fun() -> mnesia:is_transaction() end)),
    ?match(false, mnesia:activity(async_dirty, fun() -> mnesia:is_transaction() end)),
    ?match(false, mnesia:activity(sync_dirty,  fun() -> mnesia:is_transaction() end)),
    ?match(false, mnesia:activity(ets,         fun() -> mnesia:is_transaction() end)),

    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Update counter record dirty


dirty_update_counter_ram(suite) -> [];
dirty_update_counter_ram(Config) when is_list(Config) ->
    dirty_update_counter(Config, ram_copies).

dirty_update_counter_disc(suite) -> [];
dirty_update_counter_disc(Config) when is_list(Config) ->
    dirty_update_counter(Config, disc_copies).

dirty_update_counter_disc_only(suite) -> [];
dirty_update_counter_disc_only(Config) when is_list(Config) ->
    dirty_update_counter(Config, disc_only_copies).

dirty_update_counter_xets(Config) when is_list(Config) ->
    dirty_update_counter(Config, ext_ets).

dirty_update_counter(Config, Storage) ->
    [Node1] = Nodes = ?acquire_nodes(1, Config), 
    Tab = dirty_update_counter, 
    Def = [{attributes, [k, v]}, {Storage, [Node1]}], 
    ?match({atomic, ok},  mnesia:create_table(Tab, Def)), 
    ?match(ok,  mnesia:dirty_write({Tab, 1, 2})), 
        
    ?match({'EXIT', _},  mnesia:dirty_update_counter({Tab, 1}, [])), 
    ?match({'EXIT', _},  mnesia:dirty_update_counter({Tab}, 3)), 
    ?match({'EXIT', _},  mnesia:dirty_update_counter({foo, 1}, 3)), 
    ?match(5,  mnesia:dirty_update_counter({Tab, 1}, 3)), 
    ?match([{Tab, 1, 5}],  mnesia:dirty_read({Tab, 1})), 

    ?match({atomic, 8},  mnesia:transaction(fun() ->
	   mnesia:dirty_update_counter({Tab, 1}, 3) end)), 
 
    ?match(1,  mnesia:dirty_update_counter({Tab, foo}, 1)),
    ?match([{Tab, foo,1}], mnesia:dirty_read({Tab,foo})),

    ?match({ok,_}, mnesia:subscribe({table, Tab, detailed})),

    ?match(2, mnesia:dirty_update_counter({Tab, foo}, 1)),
    ?match([{Tab, foo,2}], mnesia:dirty_read({Tab,foo})),

    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Delete record dirty


dirty_delete_ram(suite) -> [];
dirty_delete_ram(Config) when is_list(Config) ->
    dirty_delete(Config, ram_copies).

dirty_delete_disc(suite) -> [];
dirty_delete_disc(Config) when is_list(Config) ->
    dirty_delete(Config, disc_copies).

dirty_delete_disc_only(suite) -> [];
dirty_delete_disc_only(Config) when is_list(Config) ->
    dirty_delete(Config, disc_only_copies).

dirty_delete_xets(Config) when is_list(Config) ->
    dirty_delete(Config, ext_ets).

dirty_delete(Config, Storage) ->
    [Node1] = Nodes = ?acquire_nodes(1, Config), 
    Tab = dirty_delete, 
    Def = [{type, bag}, {attributes, [k, v]}, {Storage, [Node1]}], 
    ?match({atomic, ok},  mnesia:create_table(Tab, Def)), 
    
    ?match({'EXIT', _},  mnesia:dirty_delete([])), 
    ?match({'EXIT', _},  mnesia:dirty_delete({Tab})), 
    ?match({'EXIT', _},  mnesia:dirty_delete({Tab, 1, 2})), 
    ?match(ok,  mnesia:dirty_delete({Tab, 1})), 
    ?match(ok,  mnesia:dirty_write({Tab, 1, 2})), 
    ?match(ok,  mnesia:dirty_delete({Tab, 1})), 
    ?match(ok,  mnesia:dirty_write({Tab, 1, 2})), 
    ?match(ok,  mnesia:dirty_write({Tab, 1, 2})), 
    ?match(ok,  mnesia:dirty_delete({Tab, 1})), 

    ?match(ok,  mnesia:dirty_write({Tab, 1, 2})), 
    ?match({atomic, ok},  mnesia:transaction(fun() ->
	   mnesia:dirty_delete({Tab, 1}) end)), 
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Delete matching record dirty


dirty_delete_object_ram(suite) -> [];
dirty_delete_object_ram(Config) when is_list(Config) ->
    dirty_delete_object(Config, ram_copies).

dirty_delete_object_disc(suite) -> [];
dirty_delete_object_disc(Config) when is_list(Config) ->
    dirty_delete_object(Config, disc_copies).

dirty_delete_object_disc_only(suite) -> [];
dirty_delete_object_disc_only(Config) when is_list(Config) ->
    dirty_delete_object(Config, disc_only_copies).

dirty_delete_object_xets(Config) when is_list(Config) ->
    dirty_delete_object(Config, ext_ets).

dirty_delete_object(Config, Storage) ->
    [Node1] = Nodes = ?acquire_nodes(1, Config), 
    Tab = dirty_delete_object, 
    Def = [{type, bag}, {attributes, [k, v]}, {Storage, [Node1]}], 
    ?match({atomic, ok},  mnesia:create_table(Tab, Def)), 

    OneRec = {Tab, 1, 2}, 
    ?match({'EXIT', _},  mnesia:dirty_delete_object([])), 
    ?match({'EXIT', _},  mnesia:dirty_delete_object({Tab})), 
    ?match({'EXIT', _},  mnesia:dirty_delete_object({Tab, 1})), 
    ?match(ok,  mnesia:dirty_delete_object(OneRec)), 
    ?match(ok,  mnesia:dirty_write(OneRec)), 
    ?match(ok,  mnesia:dirty_delete_object(OneRec)), 
    ?match(ok,  mnesia:dirty_write(OneRec)), 
    ?match(ok,  mnesia:dirty_write(OneRec)), 
    ?match(ok,  mnesia:dirty_delete_object(OneRec)), 

    ?match(ok,  mnesia:dirty_write(OneRec)), 
    ?match({atomic, ok},  mnesia:transaction(fun() ->
	   mnesia:dirty_delete_object(OneRec) end)), 

    ?match({'EXIT', {aborted, {bad_type, Tab, _}}}, mnesia:dirty_delete_object(Tab, {Tab, {['_']}, 21})),
    ?match({'EXIT', {aborted, {bad_type, Tab, _}}}, mnesia:dirty_delete_object(Tab, {Tab, {['$5']}, 21})),

    ?verify_mnesia(Nodes, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Read matching records dirty


dirty_match_object_ram(suite) -> [];
dirty_match_object_ram(Config) when is_list(Config) ->
    dirty_match_object(Config, ram_copies).

dirty_match_object_disc(suite) -> [];
dirty_match_object_disc(Config) when is_list(Config) ->
    dirty_match_object(Config, disc_copies).

dirty_match_object_disc_only(suite) -> [];
dirty_match_object_disc_only(Config) when is_list(Config) ->
    dirty_match_object(Config, disc_only_copies).

dirty_match_object_xets(Config) when is_list(Config) ->
    dirty_match_object(Config, ext_ets).

dirty_match_object(Config, Storage) ->
    [Node1] = Nodes = ?acquire_nodes(1, Config), 
    Tab = dirty_match, 
    Def = [{attributes, [k, v]}, {Storage, [Node1]}], 
    ?match({atomic, ok},  mnesia:create_table(Tab, Def)), 

    OneRec = {Tab, 1, 2}, 
    OnePat = {Tab, '$1', 2}, 
    ?match([], mnesia:dirty_match_object(OnePat)), 
    ?match(ok,  mnesia:dirty_write(OneRec)), 
    ?match([OneRec],  mnesia:dirty_match_object(OnePat)), 
    ?match({atomic, [OneRec]},  mnesia:transaction(fun() ->
	 mnesia:dirty_match_object(OnePat) end)), 
    
    ?match({'EXIT', _},  mnesia:dirty_match_object({foo, '$1', 2})), 
    ?match({'EXIT', _},  mnesia:dirty_match_object({[], '$1', 2})), 
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Dirty read matching records by using an index

dirty_index_match_object_ram(suite) -> [];
dirty_index_match_object_ram(Config) when is_list(Config) ->
    dirty_index_match_object(Config, ram_copies).

dirty_index_match_object_disc(suite) -> [];
dirty_index_match_object_disc(Config) when is_list(Config) ->
    dirty_index_match_object(Config, disc_copies).

dirty_index_match_object_disc_only(suite) -> [];
dirty_index_match_object_disc_only(Config) when is_list(Config) ->
    dirty_index_match_object(Config, disc_only_copies).

dirty_index_match_object_xets(Config) when is_list(Config) ->
    dirty_index_match_object(Config, ext_ets).

dirty_index_match_object(Config, Storage) ->
    [Node1] = Nodes = ?acquire_nodes(1, Config), 
    Tab = dirty_index_match_object, 
    ValPos = 3, 
    BadValPos = ValPos + 1, 
    Def = [{attributes, [k, v]}, {Storage, [Node1]}, {index, [ValPos]}], 
    ?match({atomic, ok},  mnesia:create_table(Tab, Def)), 
    
    ?match([],  mnesia:dirty_index_match_object({Tab, '$1', 2}, ValPos)), 
    OneRec = {Tab, 1, 2}, 
    ?match(ok,  mnesia:dirty_write(OneRec)), 
    
    ?match([OneRec],  mnesia:dirty_index_match_object({Tab, '$1', 2}, ValPos)), 
    ?match({'EXIT', _},  mnesia:dirty_index_match_object({Tab, '$1', 2}, BadValPos)), 
    ?match({'EXIT', _},  mnesia:dirty_index_match_object({foo, '$1', 2}, ValPos)), 
    ?match({'EXIT', _},  mnesia:dirty_index_match_object({[], '$1', 2}, ValPos)), 
    ?match({atomic, [OneRec]},  mnesia:transaction(fun() ->
           mnesia:dirty_index_match_object({Tab, '$1', 2}, ValPos) end)),     
    
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Read records by using an index


dirty_index_read_ram(suite) -> [];
dirty_index_read_ram(Config) when is_list(Config) ->
    dirty_index_read(Config, ram_copies).

dirty_index_read_disc(suite) -> [];
dirty_index_read_disc(Config) when is_list(Config) ->
    dirty_index_read(Config, disc_copies).

dirty_index_read_disc_only(suite) -> [];
dirty_index_read_disc_only(Config) when is_list(Config) ->
    dirty_index_read(Config, disc_only_copies).

dirty_index_read_xets(Config) when is_list(Config) ->
    dirty_index_read(Config, ext_ets).

dirty_index_read(Config, Storage) ->
    [Node1] = Nodes = ?acquire_nodes(1, Config), 
    Tab = dirty_index_read, 
    ValPos = 3, 
    BadValPos = ValPos + 1, 
    Def = [{type, set},
	   {attributes, [k, v]},
	   {Storage, [Node1]},
	   {index, [ValPos]}], 
    ?match({atomic, ok},  mnesia:create_table(Tab, Def)), 

    OneRec = {Tab, 1, 2}, 
    ?match([],  mnesia:dirty_index_read(Tab, 2, ValPos)), 
    ?match(ok,  mnesia:dirty_write(OneRec)), 
    ?match([OneRec],  mnesia:dirty_index_read(Tab, 2, ValPos)), 
    ?match({atomic, [OneRec]},  
	   mnesia:transaction(fun() -> mnesia:dirty_index_read(Tab, 2, ValPos) end)),
    ?match(42, mnesia:dirty_update_counter({Tab, 1}, 40)),
    ?match([{Tab,1,42}], mnesia:dirty_read({Tab, 1})), 
    ?match([],  mnesia:dirty_index_read(Tab, 2, ValPos)), 
    ?match([{Tab, 1, 42}],  mnesia:dirty_index_read(Tab, 42, ValPos)),

    ?match({'EXIT', _},  mnesia:dirty_index_read(Tab, 2, BadValPos)), 
    ?match({'EXIT', _},  mnesia:dirty_index_read(foo, 2, ValPos)), 
    ?match({'EXIT', _},  mnesia:dirty_index_read([], 2, ValPos)), 

    mnesia:dirty_write({Tab, 5, 1}),
    ?match(ok, index_read_loop(Tab, 0)),

    ?verify_mnesia(Nodes, []).


index_read_loop(Tab, N) when N =< 1000 ->
    spawn_link(fun() ->
		       mnesia:transaction(fun() -> mnesia:write({Tab, 5, 1}) end)
	       end),
    case mnesia:dirty_match_object({Tab, '_', 1}) of
	[{Tab, 5, 1}] ->
	    index_read_loop(Tab, N+1);
	Other -> {N, Other}
    end;
index_read_loop(_, _) ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dirty_index_update_set_ram(suite) -> [];
dirty_index_update_set_ram(Config) when is_list(Config) ->
    dirty_index_update_set(Config, ram_copies).

dirty_index_update_set_disc(suite) -> [];
dirty_index_update_set_disc(Config) when is_list(Config) ->
    dirty_index_update_set(Config, disc_copies).

dirty_index_update_set_disc_only(suite) -> [];
dirty_index_update_set_disc_only(Config) when is_list(Config) ->
    dirty_index_update_set(Config, disc_only_copies).

dirty_index_update_set_xets(Config) when is_list(Config) ->
    dirty_index_update_set(Config, ext_ets).

dirty_index_update_set(Config, Storage) ->
    [Node1] = Nodes = ?acquire_nodes(1, Config),
    Tab = index_test,
    ValPos = v1,
    ValPos2 = v3,
    Def = [{attributes, [k, v1, v2, v3]},
	   {Storage, [Node1]},
	   {index, [ValPos]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),

    Pat1 = {Tab, '$1',  2,   '$2', '$3'},
    Pat2 = {Tab, '$1', '$2', '$3', '$4'},
    Pat3 = {Tab, '_', '_', '_', {4, 14}},

    Rec1 = {Tab, 1, 2, 3, {4, 14}},
    Rec2 = {Tab, 2, 2, 13, 14},
    Rec3 = {Tab, 1, 12, 13, 14},
    Rec4 = {Tab, 4, 2, 13, 14},

    ?match([], mnesia:dirty_index_read(Tab, 2, ValPos)),
    ?match(ok, mnesia:dirty_write(Rec1)),
    ?match([Rec1], mnesia:dirty_index_read(Tab, 2, ValPos)),

    ?match(ok, mnesia:dirty_write(Rec2)),
    R1 = mnesia:dirty_index_read(Tab, 2, ValPos),
    ?match([Rec1, Rec2], lists:sort(R1)),

    ?match(ok, mnesia:dirty_write(Rec3)),
    R2 = mnesia:dirty_index_read(Tab, 2, ValPos),
    ?match([Rec2], lists:sort(R2)),
    ?match([Rec2], mnesia:dirty_index_match_object(Pat1, ValPos)),

    {atomic, R3} = mnesia:transaction(fun() -> mnesia:match_object(Pat2) end),
    ?match([Rec3, Rec2], lists:sort(R3)),

    ?match(ok, mnesia:dirty_write(Rec4)),
    R4 = mnesia:dirty_index_read(Tab, 2, ValPos),
    ?match([Rec2, Rec4], lists:sort(R4)),

    ?match(ok, mnesia:dirty_delete({Tab, 4})),
    ?match([Rec2], mnesia:dirty_index_read(Tab, 2, ValPos)),

    ?match({atomic, ok}, mnesia:del_table_index(Tab, ValPos)),
    ?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:write(Rec4) end)),
    ?match({atomic, ok}, mnesia:add_table_index(Tab, ValPos)),
    ?match({atomic, ok}, mnesia:add_table_index(Tab, ValPos2)),

    R5 = mnesia:dirty_match_object(Pat2),
    ?match([Rec3, Rec2, Rec4], lists:sort(R5)),

    R6 = mnesia:dirty_index_read(Tab, 2, ValPos),
    ?match([Rec2, Rec4], lists:sort(R6)),
    ?match([], mnesia:dirty_index_read(Tab, {4,14}, ValPos2)),
    R7 = mnesia:dirty_index_read(Tab, 14, ValPos2),
    ?match([Rec3, Rec2, Rec4], lists:sort(R7)),

    ?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:write(Rec1) end)),
    R8 = mnesia:dirty_index_read(Tab, 2, ValPos),
    ?match([Rec1, Rec2, Rec4], lists:sort(R8)),
    ?match([Rec1], mnesia:dirty_index_read(Tab, {4,14}, ValPos2)),
    ?match([Rec1], mnesia:dirty_match_object(Pat3)),
    ?match([Rec1], mnesia:dirty_index_match_object(Pat3, ValPos2)),

    R9 = mnesia:dirty_index_read(Tab, 14, ValPos2),
    ?match([Rec2, Rec4], lists:sort(R9)),

    ?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:delete_object(Rec2) end)),
    R10 = mnesia:dirty_index_read(Tab, 2, ValPos),
    ?match([Rec1, Rec4], lists:sort(R10)),
    ?match([Rec1], mnesia:dirty_index_read(Tab, {4,14}, ValPos2)),
    ?match([Rec4], mnesia:dirty_index_read(Tab, 14, ValPos2)),

    ?match(ok, mnesia:dirty_delete({Tab, 4})),
    R11 = mnesia:dirty_index_read(Tab, 2, ValPos),
    ?match([Rec1], lists:sort(R11)),
    ?match([Rec1], mnesia:dirty_index_read(Tab, {4,14}, ValPos2)),
    ?match([], mnesia:dirty_index_read(Tab, 14, ValPos2)),
    
    ?verify_mnesia(Nodes, []).

dirty_index_update_bag_ram(suite) -> [];
dirty_index_update_bag_ram(Config)when is_list(Config) ->
    dirty_index_update_bag(Config, ram_copies).

dirty_index_update_bag_disc(suite) -> [];
dirty_index_update_bag_disc(Config)when is_list(Config) ->
    dirty_index_update_bag(Config, disc_copies).

dirty_index_update_bag_disc_only(suite) -> [];
dirty_index_update_bag_disc_only(Config)when is_list(Config) ->
    dirty_index_update_bag(Config, disc_only_copies).

dirty_index_update_bag_xets(Config) when is_list(Config) ->
    dirty_index_update_bag(Config, ext_ets).

dirty_index_update_bag(Config, Storage) ->
    [Node1] = Nodes = ?acquire_nodes(1, Config), 
    Tab = index_test, 
    ValPos = v1, 
    ValPos2 = v3,
    Def = [{type, bag},
	   {attributes, [k, v1, v2, v3]}, 
	   {Storage, [Node1]},
	   {index, [ValPos]}], 
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)), 

    Pat1 = {Tab, '$1',  2,   '$2', '$3'},
    Pat2 = {Tab, '$1', '$2', '$3', '$4'}, 

    Rec1 = {Tab, 1, 2, 3, 4},    
    Rec2 = {Tab, 2, 2, 13, 14},  
    Rec3 = {Tab, 1, 12, 13, 14}, 
    Rec4 = {Tab, 4, 2, 13, 4}, 
    Rec5 = {Tab, 1, 2, 234, 14},

    %% Simple Index
    ?match([], mnesia:dirty_index_read(Tab, 2, ValPos)),
    ?match(ok, mnesia:dirty_write(Rec1)),
    ?match([Rec1], mnesia:dirty_index_read(Tab, 2, ValPos)),

    ?match(ok, mnesia:dirty_delete_object(Rec5)),
    ?match([Rec1], mnesia:dirty_index_read(Tab, 2, ValPos)),

    ?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:write(Rec2) end)), 
    R1 = mnesia:dirty_index_read(Tab, 2, ValPos),
    ?match([Rec1, Rec2], lists:sort(R1)),

    ?match(ok, mnesia:dirty_write(Rec3)),
    R2 = mnesia:dirty_index_read(Tab, 2, ValPos),
    ?match([Rec1, Rec2], lists:sort(R2)),

    R3 = mnesia:dirty_index_match_object(Pat1, ValPos),
    ?match([Rec1, Rec2], lists:sort(R3)),
 
    R4 = mnesia:dirty_match_object(Pat2),
    ?match([Rec1, Rec3, Rec2], lists:sort(R4)),
 
    ?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:write(Rec4) end)), 
    R5 = mnesia:dirty_index_read(Tab, 2, ValPos),
    ?match([Rec1, Rec2, Rec4], lists:sort(R5)),

    ?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:delete({Tab, 4}) end)), 
    R6 = mnesia:dirty_index_read(Tab, 2, ValPos),
    ?match([Rec1, Rec2], lists:sort(R6)),

    ?match(ok, mnesia:dirty_delete_object(Rec1)),
    ?match([Rec2], mnesia:dirty_index_read(Tab, 2, ValPos)),
    R7 = mnesia:dirty_match_object(Pat2),
    ?match([Rec3, Rec2], lists:sort(R7)),
    
    %% Two indexies
    ?match({atomic, ok}, mnesia:del_table_index(Tab, ValPos)),
    ?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:write(Rec1) end)),
    ?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:write(Rec4) end)),
    ?match({atomic, ok}, mnesia:add_table_index(Tab, ValPos)),
    ?match({atomic, ok}, mnesia:add_table_index(Tab, ValPos2)),
    
    R8 = mnesia:dirty_index_read(Tab, 2, ValPos),
    ?match([Rec1, Rec2, Rec4], lists:sort(R8)),

    R9 = mnesia:dirty_index_read(Tab, 4, ValPos2),
    ?match([Rec1, Rec4], lists:sort(R9)),
    R10 = mnesia:dirty_index_read(Tab, 14, ValPos2),
    ?match([Rec3, Rec2], lists:sort(R10)),

    ?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:write(Rec5) end)),
    R11 = mnesia:dirty_index_read(Tab, 2, ValPos),
    ?match([Rec1, Rec5, Rec2, Rec4], lists:sort(R11)),
    R12 = mnesia:dirty_index_read(Tab, 4, ValPos2),
    ?match([Rec1, Rec4], lists:sort(R12)),
    R13 = mnesia:dirty_index_read(Tab, 14, ValPos2),
    ?match([Rec5, Rec3, Rec2], lists:sort(R13)),

    ?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:delete_object(Rec1) end)),
    R14 = mnesia:dirty_index_read(Tab, 2, ValPos),
    ?match([Rec5, Rec2, Rec4], lists:sort(R14)),
    ?match([Rec4], mnesia:dirty_index_read(Tab, 4, ValPos2)),
    R15 = mnesia:dirty_index_read(Tab, 14, ValPos2),
    ?match([Rec5, Rec3, Rec2], lists:sort(R15)),

    ?match(ok, mnesia:dirty_delete_object(Rec5)),
    R16 = mnesia:dirty_index_read(Tab, 2, ValPos),
    ?match([Rec2, Rec4], lists:sort(R16)),
    ?match([Rec4], mnesia:dirty_index_read(Tab, 4, ValPos2)),
    R17 = mnesia:dirty_index_read(Tab, 14, ValPos2),
    ?match([Rec3, Rec2], lists:sort(R17)),

    ?match(ok, mnesia:dirty_write(Rec1)),
    ?match(ok, mnesia:dirty_delete({Tab, 1})),
    R18 = mnesia:dirty_index_read(Tab, 2, ValPos),
    ?match([Rec2, Rec4], lists:sort(R18)),
    ?match([Rec4], mnesia:dirty_index_read(Tab, 4, ValPos2)),
    R19 = mnesia:dirty_index_read(Tab, 14, ValPos2),
    ?match([Rec2], lists:sort(R19)),

    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Dirty iteration
%% dirty_slot,  dirty_first,  dirty_next

dirty_iter_ram(suite) -> [];
dirty_iter_ram(Config) when is_list(Config) ->
    dirty_iter(Config, ram_copies).

dirty_iter_disc(suite) -> [];
dirty_iter_disc(Config) when is_list(Config) ->
    dirty_iter(Config, disc_copies).

dirty_iter_disc_only(suite) -> [];
dirty_iter_disc_only(Config) when is_list(Config) ->
    dirty_iter(Config, disc_only_copies).

dirty_iter_xets(Config) when is_list(Config) ->
    dirty_iter(Config, ext_ets).

dirty_iter(Config, Storage) ->
    [Node1] = Nodes = ?acquire_nodes(1, Config), 
    Tab = dirty_iter, 
    Def = [{type, bag}, {attributes, [k, v]}, {Storage, [Node1]}], 
    ?match({atomic, ok},  mnesia:create_table(Tab, Def)), 

    ?match([], all_slots(Tab)), 
    ?match([], all_nexts(Tab)), 

    Keys = lists:seq(1, 5), 
    Records = [{Tab, A, B} || A <- Keys,  B <- lists:seq(1, 2)], 
    lists:foreach(fun(Rec) -> ?match(ok, mnesia:dirty_write(Rec)) end, Records), 

    SortedRecords = lists:sort(Records), 
    ?match(SortedRecords, lists:sort(all_slots(Tab))), 
    ?match(Keys, lists:sort(all_nexts(Tab))), 

    ?match({'EXIT', _}, mnesia:dirty_first(foo)), 
    ?match({'EXIT', _}, mnesia:dirty_next(foo, foo)), 
    ?match({'EXIT', _}, mnesia:dirty_slot(foo, 0)), 
    ?match({'EXIT', _}, mnesia:dirty_slot(foo, [])), 
    ?match({atomic, Keys},
	   mnesia:transaction(fun() -> lists:sort(all_nexts(Tab)) end)),     
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Returns a list of all keys in table
all_slots(Tab) ->
    all_slots(Tab, [], 0).

all_slots(_Tab, '$end_of_table', _) ->
    [];
all_slots(Tab, PrevRecords, PrevSlot) ->
    Records = mnesia:dirty_slot(Tab, PrevSlot), 
    PrevRecords ++ all_slots(Tab, Records, PrevSlot + 1).

%% Returns a list of all keys in table

all_nexts(Tab) ->
    FirstKey = mnesia:dirty_first(Tab), 
    all_nexts(Tab, FirstKey).

all_nexts(_Tab, '$end_of_table') ->
    [];
all_nexts(Tab, PrevKey) ->
    Key = mnesia:dirty_next(Tab, PrevKey), 
    [PrevKey] ++ all_nexts(Tab, Key).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_trans(Tab, Key, Acc) ->
    Update = 
	fun() -> 
		Res = (catch mnesia:read({Tab, Key})),
		case Res of 
		    [{Tab, Key, Extra, Acc}] ->
			mnesia:write({Tab,Key,Extra, Acc+1});
		    Val ->
			{read, Val, {acc, Acc}}
		end
	end,
    receive 
	{Pid, quit} -> Pid ! {self(), Acc}
    after
	3 -> 
	    case catch mnesia:sync_dirty(Update) of
		ok -> 	    
		    update_trans(Tab, Key, Acc+1);
		Else -> 
		    ?error("Dirty Operation failed on ~p (update no ~p) with ~p~n"
			   "Info w2read ~p w2write ~p w2commit ~p storage ~p ~n", 
			   [node(), 
			    Acc,
			    Else,
			    mnesia:table_info(Tab, where_to_read),
			    mnesia:table_info(Tab, where_to_write),
			    mnesia:table_info(Tab, where_to_commit),
			    mnesia:table_info(Tab, storage_type)])
	    end
    end.

del_table_copy_1(suite) -> [];
del_table_copy_1(Config) when is_list(Config) ->
    [_Node1, Node2, _Node3] = Nodes = ?acquire_nodes(3, Config),
    del_table(Node2, Node2, Nodes). %Called on same Node as deleted
del_table_copy_2(suite) -> [];
del_table_copy_2(Config) when is_list(Config) ->
    [Node1, Node2, _Node3] = Nodes = ?acquire_nodes(3, Config),
    del_table(Node1, Node2, Nodes). %Called from other Node 
del_table_copy_3(suite) -> [];
del_table_copy_3(Config) when is_list(Config) ->
    [_Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config),
    del_table(Node3, Node2, Nodes). %Called from Node w.o. table

del_table(CallFrom, DelNode, [Node1, Node2, Node3]) ->
    Tab = schema_ops,
    Def = [{disc_only_copies, [Node1]}, {ram_copies, [Node2]}, 
	   {attributes, [key, attr1, attr2]}],
    ?log("Test case removing table from ~w, with ~w~n", [DelNode, Def]),
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    insert(Tab, 1000),
    
    Pid1 = spawn_link(Node1, ?MODULE, update_trans, [Tab, 1, 0]),
    Pid2 = spawn_link(Node2, ?MODULE, update_trans, [Tab, 2, 0]),
    Pid3 = spawn_link(Node3, ?MODULE, update_trans, [Tab, 3, 0]),


    dbg:tracer(process, {fun(Msg,_) -> tracer(Msg) end, void}),          
    %%    dbg:n(Node2),
    %%    dbg:n(Node3),
    %% dbg:tp('_', []),     
    %% dbg:tpl(dets, [timestamp]), 
    dbg:p(Pid1, [m,c,timestamp]),  
    
    ?match({atomic, ok}, 
	   rpc:call(CallFrom, mnesia, del_table_copy, [Tab, DelNode])),

    Pid1 ! {self(), quit}, R1 = 
	receive {Pid1, Res1} -> Res1
	after 
	    5000 -> io:format("~p~n",[process_info(Pid1)]),error 
	end,
    Pid2 ! {self(), quit}, R2 = 
	receive {Pid2, Res2} -> Res2 
	after 
	    5000 -> error 
	end,
    Pid3 ! {self(), quit}, R3 = 
	receive {Pid3, Res3} -> Res3 
	after 
	    5000 -> error 
	end,
    verify_oids(Tab, Node1, Node2, Node3, R1, R2, R3),
    ?verify_mnesia([Node1, Node2, Node3], []).
    
tracer({trace_ts, _, send, Msg, Pid, {_,S,Ms}}) ->
    io:format("~p:~p ~p >> ~w ~n",[S,Ms,Pid,Msg]);
tracer({trace_ts, _, 'receive', Msg, {_,S,Ms}}) ->
    io:format("~p:~p << ~w ~n",[S,Ms,Msg]);


tracer(Msg) ->
    io:format("UMsg ~p ~n",[Msg]),
    ok.



add_table_copy_1(suite) -> [];
add_table_copy_1(Config) when is_list(Config) ->
    [Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config),
    Def = [{ram_copies, [Node1, Node2]}, 
	   {attributes, [key, attr1, attr2]}], 
    add_table(Node1, Node3, Nodes, Def).
%% Not so much diff from 1 but I got a feeling of a bug
%% should behave exactly the same but just checking the internal ordering 
add_table_copy_2(suite) -> [];
add_table_copy_2(Config) when is_list(Config) ->
    [Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config),
    Def = [{ram_copies, [Node1, Node2]}, 
	   {attributes, [key, attr1, attr2]}], 
    add_table(Node2, Node3, Nodes, Def).
add_table_copy_3(suite) -> [];
add_table_copy_3(Config) when is_list(Config) ->
    [Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config),
    Def = [{ram_copies, [Node1, Node2]},
	   {attributes, [key, attr1, attr2]}], 
    add_table(Node3, Node3, Nodes, Def).
add_table_copy_4(suite) -> [];
add_table_copy_4(Config) when is_list(Config) ->
    [Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config),
    Def = [{disc_only_copies, [Node1]}, 
	   {attributes, [key, attr1, attr2]}], 
    add_table(Node2, Node3, Nodes, Def).

add_table(CallFrom, AddNode, [Node1, Node2, Node3], Def) ->
    ?log("Test case adding table at ~w, with ~w~n", [AddNode, Def]),
    Tab = schema_ops,
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    insert(Tab, 1002),
    
    Pid1 = spawn_link(Node1, ?MODULE, update_trans, [Tab, 1, 0]),
    Pid2 = spawn_link(Node2, ?MODULE, update_trans, [Tab, 2, 0]),
    Pid3 = spawn_link(Node3, ?MODULE, update_trans, [Tab, 3, 0]),
    
    ?match({atomic, ok}, rpc:call(CallFrom, mnesia, add_table_copy, 
				  [Tab, AddNode, ram_copies])),
    Pid1 ! {self(), quit}, R1 = receive {Pid1, Res1} -> Res1 after 5000 -> error end,
    Pid2 ! {self(), quit}, R2 = receive {Pid2, Res2} -> Res2 after 5000 -> error end,
    Pid3 ! {self(), quit}, R3 = receive {Pid3, Res3} -> Res3 after 5000 -> error end,
    verify_oids(Tab, Node1, Node2, Node3, R1, R2, R3),
    ?verify_mnesia([Node1, Node2, Node3], []).

move_table_copy_1(suite) -> [];
move_table_copy_1(Config) when is_list(Config) ->
    [Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config),
    Def = [{ram_copies, [Node1, Node2]},
	   {attributes, [key, attr1, attr2]}], 
    move_table(Node1, Node1, Node3, Nodes, Def).
move_table_copy_2(suite) -> [];
move_table_copy_2(Config) when is_list(Config) ->
    [Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config),
    Def = [{ram_copies, [Node1, Node2]},
	   {attributes, [key, attr1, attr2]}], 
    move_table(Node2, Node1, Node3, Nodes, Def).
move_table_copy_3(suite) -> [];
move_table_copy_3(Config) when is_list(Config) ->
    [Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config),
    Def = [{ram_copies, [Node1, Node2]},
	   {attributes, [key, attr1, attr2]}], 
    move_table(Node3, Node1, Node3, Nodes, Def).
move_table_copy_4(suite) -> [];
move_table_copy_4(Config) when is_list(Config) ->
    [Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config),
    Def = [{ram_copies, [Node1]},
	   {attributes, [key, attr1, attr2]}], 
    move_table(Node2, Node1, Node3, Nodes, Def).

move_table(CallFrom, FromNode, ToNode, [Node1, Node2, Node3], Def) ->
    ?log("Test case move table from ~w to ~w, with ~w~n", [FromNode, ToNode, Def]),
    Tab = schema_ops,
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    insert(Tab, 1002),
    
    Pid1 = spawn_link(Node1, ?MODULE, update_trans, [Tab, 1, 0]),
    Pid2 = spawn_link(Node2, ?MODULE, update_trans, [Tab, 2, 0]),
    Pid3 = spawn_link(Node3, ?MODULE, update_trans, [Tab, 3, 0]),
    
    ?match({atomic, ok}, rpc:call(CallFrom, mnesia, move_table_copy, 
				  [Tab, FromNode, ToNode])),
    Pid1 ! {self(), quit}, 
    R1 = receive {Pid1, Res1} -> Res1 after 5000 -> ?error("timeout pid1~n", []) end,
    Pid2 ! {self(), quit}, 
    R2 = receive {Pid2, Res2} -> Res2 after 5000 -> ?error("timeout pid2~n", []) end,
    Pid3 ! {self(), quit}, 
    R3 = receive {Pid3, Res3} -> Res3 after 5000 -> ?error("timeout pid3~n", []) end,
    verify_oids(Tab, Node1, Node2, Node3, R1, R2, R3),
    ?verify_mnesia([Node1, Node2, Node3], []).

% Verify consistency between different nodes
% Due to limitations in the current dirty_ops this can wrong from time to time!
verify_oids(Tab, N1, N2, N3, R1, R2, R3) ->
    io:format("DEBUG 1=>~p 2=>~p 3=>~p~n", [R1,R2,R3]),
    ?match([{_, _, _, R1}], rpc:call(N1, mnesia, dirty_read, [{Tab, 1}])),
    ?match([{_, _, _, R1}], rpc:call(N2, mnesia, dirty_read, [{Tab, 1}])),
    ?match([{_, _, _, R1}], rpc:call(N3, mnesia, dirty_read, [{Tab, 1}])),
    ?match([{_, _, _, R2}], rpc:call(N1, mnesia, dirty_read, [{Tab, 2}])),
    ?match([{_, _, _, R2}], rpc:call(N2, mnesia, dirty_read, [{Tab, 2}])),
    ?match([{_, _, _, R2}], rpc:call(N3, mnesia, dirty_read, [{Tab, 2}])),
    ?match([{_, _, _, R3}], rpc:call(N1, mnesia, dirty_read, [{Tab, 3}])),
    ?match([{_, _, _, R3}], rpc:call(N2, mnesia, dirty_read, [{Tab, 3}])),
    ?match([{_, _, _, R3}], rpc:call(N3, mnesia, dirty_read, [{Tab, 3}])).

insert(_Tab, 0) -> ok;
insert(Tab, N) when N > 0 ->
    ok = mnesia:sync_dirty(fun() -> false = mnesia:is_transaction(), mnesia:write({Tab, N, N, 0}) end),
    insert(Tab, N-1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
