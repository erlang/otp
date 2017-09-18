%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
-module(mnesia_frag_test).
-author('hakan@erix.ericsson.se').
-include("mnesia_test_lib.hrl").

-export([init_per_testcase/2, end_per_testcase/2,
         init_per_group/2, end_per_group/2,
         all/0, groups/0]).


-export([nice_single/1, nice_multi/1, nice_access/1, iter_access/1,
         consistency/1, evil_create/1, evil_delete/1, evil_change/1, evil_combine/1,
         evil_loop/1, evil_delete_db_node/1]).


-export([frag_dist/1]).

init_per_testcase(Func, Conf) ->
    mnesia_test_lib:init_per_testcase(Func, Conf).

end_per_testcase(Func, Conf) ->
    mnesia_test_lib:end_per_testcase(Func, Conf).

-define(match_dist(ExpectedRes, Expr),
	case ?match(ExpectedRes, Expr) of
	    
	mnesia_test_lib:error(Format, Args,?FILE,?LINE)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() -> 
    [{group, light}, {group, medium}].

groups() -> 
    [{light, [], [{group, nice}, {group, evil}]},
     {medium, [], [consistency]},
     {nice, [],
      [nice_single, nice_multi, nice_access, iter_access]},
     {evil, [],
      [evil_create, evil_delete, evil_change, evil_combine,
       evil_loop, evil_delete_db_node]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


nice_single(suite) -> [];
nice_single(Config) when is_list(Config) ->
    [Node1, Node2] = Nodes = ?acquire_nodes(2, Config),

    %% Create a table with 2 fragments and 12 records
    Tab = nice_frag,
    Props = [{n_fragments, 2}, {node_pool, [Node1]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, [{frag_properties, Props}])),
    Records = [{Tab, N, -N} || N <- lists:seq(1, 12)],
    [frag_write(Tab, R)  || R <- Records],
    ?match([{Node1, 2}], frag_dist(Tab)),
    ?match([8, 4], frag_rec_dist(Tab)),

    %% Adding a new node to pool should not affect distribution
    ?match({atomic, ok}, mnesia:change_table_frag(Tab, {add_node, Node2})),
    Dist =  frag_dist(Tab), 
    ?match([{Node2, 0}, {Node1, 2}], Dist), 
    ?match([8, 4], frag_rec_dist(Tab)),

    %% Add new fragment hopefully on the new node
    ?match({atomic, ok}, mnesia:change_table_frag(Tab, {add_frag, Dist})),
    Dist2 =  frag_dist(Tab), 
    ?match([{Node2, 1}, {Node1, 2}], Dist2), 
    ?match([3, 4, 5], frag_rec_dist(Tab)),
    
    %% Add new fragment hopefully on the new node
    ?match({atomic, ok}, mnesia:change_table_frag(Tab, {add_frag, Dist2})),
    Dist3 =  frag_dist(Tab), 
    ?match([{Node1, 2}, {Node2, 2}], Dist3), 
    ?match([3, 2, 5, 2], frag_rec_dist(Tab)),

    %% Add new fragment hopefully on the new node
    ?match({atomic, ok}, mnesia:change_table_frag(Tab, {add_frag, Dist3})),
    Dist4 =  frag_dist(Tab), 
    ?match([{Node2, 2}, {Node1, 3}], Dist4), 
    ?match([_, _, _, _, _], frag_rec_dist(Tab)),

    %% Dropping a node in pool should not affect distribution
    ?match({atomic, ok}, mnesia:change_table_frag(Tab, {del_node, Node1})),
    ?match([{Node2, 2}, {Node1, 3}], frag_dist(Tab)),
    ?match([_, _, _, _, _], frag_rec_dist(Tab)),

    %% Dropping a fragment
    ?match({atomic, ok}, mnesia:change_table_frag(Tab, del_frag)),
    Dist5 =  frag_dist(Tab), 
    ?match([{Node2, 2}, {Node1, 2}], Dist5), 
    ?match([3, 2, 5, 2], frag_rec_dist(Tab)),
    
    %% Add new fragment hopefully on the new node
    ?match({atomic, ok}, mnesia:change_table_frag(Tab, {add_frag, Dist5})),
    Dist6 =  frag_dist(Tab), 
    ?match([{Node2, 3}, {Node1, 2}], Dist6), 
    ?match([_, _, _, _, _], frag_rec_dist(Tab)),

    %% Dropping all fragments but one
    ?match({atomic, ok}, mnesia:change_table_frag(Tab, del_frag)),
    ?match([3, 2, 5, 2], frag_rec_dist(Tab)),
    ?match({atomic, ok}, mnesia:change_table_frag(Tab, del_frag)),
    ?match([3, 4, 5], frag_rec_dist(Tab)),
    ?match({atomic, ok}, mnesia:change_table_frag(Tab, del_frag)),
    ?match([8, 4], frag_rec_dist(Tab)),
    ?match({atomic, ok}, mnesia:change_table_frag(Tab, del_frag)),
    ?match([{Node2, 0}, {Node1, 1}], frag_dist(Tab)), 
    ?match([12], frag_rec_dist(Tab)),
	     
    %% Defragmenting the table clears frag_properties
    ?match(Len when Len > 0,
		    length(mnesia:table_info(Tab, frag_properties))),
    ?match({atomic, ok}, mnesia:change_table_frag(Tab, deactivate)),
    ?match(0, length(mnesia:table_info(Tab, frag_properties))),

    %% Making the table fragmented again
    Props2 = [{n_fragments, 1}],
    ?match({atomic, ok}, mnesia:change_table_frag(Tab, {activate, Props2})),
    ?match({atomic, ok}, mnesia:change_table_frag(Tab, {add_frag, frag_dist(Tab)})),
    Dist7 = frag_dist(Tab),
    ?match([{Node1, 1}, {Node2, 1}], Dist7),
    ?match([8, 4], frag_rec_dist(Tab)),

    %% Deleting the fragmented table
    ?match({atomic, ok}, mnesia:delete_table(Tab)),
    ?match(false, lists:member(Tab, mnesia:system_info(tables))),
	     
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nice_multi(doc) ->
    ["Extending the nice case with one more node, ",
     "one more replica and a foreign key"];
nice_multi(suite) -> [];
nice_multi(Config) when is_list(Config) ->
    [Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config),

    %% Create a table with 2 fragments and 8 records
    Tab = frag_master,
    Name = frag_rec,
    Type = case mnesia_test_lib:diskless(Config) of 
	       true -> n_ram_copies;
	       false -> n_disc_copies
	   end,
    Props = [{n_fragments, 2},
	     {Type, 2},
	     {node_pool, [Node2, Node1]}],
    Def = [{frag_properties, Props},
	   {attributes, [id, data]},
	   {record_name, Name}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    [frag_write(Tab, {Name, Id, -Id})  || Id <- lists:seq(1, 8)],
    ?match([6, 2], frag_rec_dist(Tab)),
    ?match([{Node2, 2}, {Node1, 2}], frag_dist(Tab)),
    
    %% And connect another table to it, via a foreign key
    TabF = frag_slave,
    PropsF = [{foreign_key, {Tab, foreign_id}}],
    DefF = [{frag_properties, PropsF},
	    {attributes, [id, foreign_id]}],

    ?match({atomic, ok}, mnesia:create_table(TabF, DefF)),
    [frag_write(TabF, {TabF, {Id}, Id})  || Id <- lists:seq(1, 16)],
    ?match([10, 6], frag_rec_dist(TabF)),
    ?match([{Node2, 2}, {Node1, 2}], frag_dist(TabF)),

    %% Adding a new node to pool should not affect distribution
    ?match({atomic, ok}, mnesia:change_table_frag(Tab, {add_node, Node3})),
    Dist =  frag_dist(Tab), 
    ?match([{Node3, 0}, {Node2, 2}, {Node1, 2}], Dist), 
    ?match([6, 2], frag_rec_dist(Tab)),
    DistF =  frag_dist(TabF), 
    ?match([{Node3, 0}, {Node2, 2}, {Node1, 2}], DistF), 
    ?match([10, 6], frag_rec_dist(TabF)),

    %% Add new fragment hopefully on the new node
    ?match({atomic, ok}, mnesia:change_table_frag(Tab, {add_frag, Dist})),
    Dist2 =  frag_dist(Tab), 
    ?match([{Node3, 1},{Node1, 2},{Node2,3}], Dist2), 
    ?match([_, _, _], frag_rec_dist(Tab)),
    DistF2 =  frag_dist(TabF), 
    ?match([{Node3, 1},{Node1, 2},{Node2,3}], DistF2), 
    ?match([_, _, _], frag_rec_dist(TabF)),
    
    %% Add new fragment hopefully on the new node
    ?match({atomic, ok}, mnesia:change_table_frag(Tab, {add_frag, Dist2})),
    Dist3 =  frag_dist(Tab), 
    ?match([{Node3, 2},{Node2,3},{Node1, 3}], Dist3), 
    ?match([3, 0, 3, 2], frag_rec_dist(Tab)),
    DistF3 =  frag_dist(TabF), 
    ?match([{Node3, 2},{Node2,3},{Node1, 3}], DistF3), 
    ?match([3, 3, 7, 3], frag_rec_dist(TabF)),

    %% Add new fragment hopefully on the new node
    ?match({atomic, ok}, mnesia:change_table_frag(Tab, {add_frag, Dist3})),
    Dist4 =  frag_dist(Tab), 
    ?match([{Node1, 3}, {Node3, 3},{Node2, 4}], Dist4), 
    ?match([_, _, _, _, _], frag_rec_dist(Tab)),
    DistF4 =  frag_dist(TabF), 
    ?match([{Node1, 3}, {Node3, 3},{Node2, 4}], DistF4), 
    ?match([_, _, _, _, _], frag_rec_dist(TabF)),

    %% Dropping a node in pool should not affect distribution
    ?match({atomic, ok}, mnesia:change_table_frag(Tab, {del_node, Node1})),
    ?match([{Node3, 3},{Node2, 4}, {Node1, 3}], frag_dist(Tab)),
    ?match([_, _, _, _, _], frag_rec_dist(Tab)),
    ?match([{Node3, 3},{Node2, 4}, {Node1, 3}], frag_dist(TabF)),
    ?match([_, _, _, _, _], frag_rec_dist(TabF)),

    %% Dropping a fragment
    ?match({atomic, ok}, mnesia:change_table_frag(Tab, del_frag)),
    Dist5 =  frag_dist(Tab), 
    ?match([{Node3, 2},{Node2,3},{Node1, 3}], Dist5), 
    ?match([3, 0, 3, 2], frag_rec_dist(Tab)),
    DistF5 =  frag_dist(Tab), 
    ?match([{Node3, 2},{Node2,3},{Node1, 3}], DistF5), 
    ?match([3, 3, 7, 3], frag_rec_dist(TabF)),
    
    %% Add new fragment hopefully on the new node
    ?match({atomic, ok}, mnesia:change_table_frag(Tab, {add_frag, Dist5})),
    Dist6 =  frag_dist(Tab), 
    ?match([{Node3, 3},{Node2, 4},{Node1, 3}], Dist6), 
    ?match([_, _, _, _, _], frag_rec_dist(Tab)),
    DistF6 =  frag_dist(TabF), 
    ?match([{Node3, 3},{Node2, 4},{Node1, 3}], DistF6), 
    ?match([_, _, _, _, _], frag_rec_dist(TabF)),

    %% Dropping all fragments but one
    ?match({atomic, ok}, mnesia:change_table_frag(Tab, del_frag)),
    ?match([3, 0, 3, 2], frag_rec_dist(Tab)),
    ?match([3, 3, 7, 3], frag_rec_dist(TabF)),
    ?match({atomic, ok}, mnesia:change_table_frag(Tab, del_frag)),
    ?match([_, _, _], frag_rec_dist(Tab)),
    ?match([_, _, _], frag_rec_dist(TabF)),
    ?match({atomic, ok}, mnesia:change_table_frag(Tab, del_frag)),
    ?match([6, 2], frag_rec_dist(Tab)),
    ?match([10, 6], frag_rec_dist(TabF)),
    ?match({atomic, ok}, mnesia:change_table_frag(Tab, del_frag)),
    ?match([{Node3, 0}, {Node2, 1}, {Node1, 1}], frag_dist(Tab)), 
    ?match([8], frag_rec_dist(Tab)),
    ?match([{Node3, 0}, {Node2, 1}, {Node1, 1}], frag_dist(TabF)), 
    ?match([16], frag_rec_dist(TabF)),
	     
    %% Defragmenting the tables clears frag_properties
    ?match(Len when Len > 0,
		    length(mnesia:table_info(TabF, frag_properties))),
    ?match({atomic, ok}, mnesia:change_table_frag(TabF, deactivate)),
    ?match(0, length(mnesia:table_info(TabF, frag_properties))),
    ?match(Len when Len > 0,
		    length(mnesia:table_info(Tab, frag_properties))),
    ?match({atomic, ok}, mnesia:change_table_frag(Tab, deactivate)),
    ?match(0, length(mnesia:table_info(Tab, frag_properties))),

    %% Making the tables fragmented again
    Props2 = [{n_fragments, 1}, {node_pool, [Node1, Node2]}],
    ?match({atomic, ok}, mnesia:change_table_frag(Tab, {activate, Props2})),
    ?match({atomic, ok}, mnesia:delete_table(TabF)),
    ?match({atomic, ok}, mnesia:create_table(TabF, DefF)),
    [frag_write(TabF, {TabF, {Id}, Id})  || Id <- lists:seq(1, 16)],
    ?match({atomic, ok}, mnesia:change_table_frag(Tab, {add_frag, frag_dist(Tab)})),
    ?match([{Node1, 2}, {Node2, 2}], frag_dist(Tab)),
    ?match([6, 2], frag_rec_dist(Tab)),
    ?match([{Node1, 2}, {Node2, 2}], frag_dist(TabF)),
    ?match([10, 6], frag_rec_dist(TabF)),

    %% Deleting the fragmented tables
    ?match({atomic, ok}, mnesia:delete_table(TabF)),
    ?match(false, lists:member(TabF, mnesia:system_info(tables))),
    ?match({atomic, ok}, mnesia:delete_table(Tab)),
    ?match(false, lists:member(Tab, mnesia:system_info(tables))),
	     
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nice_access(doc) ->
    ["Cover entire callback interface"];
nice_access(suite) -> [];
nice_access(Config) when is_list(Config) ->
    Nodes = ?acquire_nodes(3, Config),

    Tab = frag_access,
    Pool = lists:sort(Nodes),
    Props = [{n_fragments, 20},
	     {n_ram_copies, 2},
	     {node_pool, Pool}],
    Def = [{frag_properties, Props},
	   {type, ordered_set},
	   {index, [val]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    [frag_write(Tab, {Tab, Id, Id})  || Id <- lists:seq(1, 400)],
    
    %% And connect another table to it, via a foreign key
    TabF = frag_access_slave,
    PropsF = [{foreign_key, {Tab, val}}],
    DefF = [{frag_properties, PropsF},
	    {index, [val]}],
    ?match({atomic, ok}, mnesia:create_table(TabF, DefF)),
    [frag_write(TabF, {TabF, Id, Id})  || Id <- lists:seq(1, 400)],

    ?match(done, mnesia:activity(transaction, fun do_access/3, [Tab, Tab, Pool], mnesia_frag)),
    ?match(done, mnesia:activity(transaction, fun do_access/3, [TabF, Tab, Pool], mnesia_frag)),

    ?verify_mnesia(Nodes, []).

do_access(Tab, Master, Pool) ->
    ?match(20, mnesia:table_info(Tab, n_fragments)),
    ?match(Pool, mnesia:table_info(Tab, node_pool)),
    ?match(2, mnesia:table_info(Tab, n_ram_copies)),
    ?match(0, mnesia:table_info(Tab, n_disc_copies)),
    ?match(0, mnesia:table_info(Tab, n_disc_only_copies)),
    ?match(20, length(mnesia:table_info(Tab, frag_names))),
    ?match(20, length(mnesia:table_info(Tab, frag_size))),
    ?match(20, length(mnesia:table_info(Tab, frag_memory))),
    PoolSize = length(Pool),
    ?match(PoolSize, length(mnesia:table_info(Tab, frag_dist))),
    ?match(400, mnesia:table_info(Tab, size)),
    ?match(I when is_integer(I), mnesia:table_info(Tab, memory)),
    ?match(Tab, mnesia:table_info(Tab, base_table)),

    Foreign = 
	if
	    Master == Tab ->
		?match(undefined, mnesia:table_info(Tab, foreign_key)),
		?match([_], mnesia:table_info(Tab, foreigners)),
                ?match({'EXIT', {aborted, {combine_error, Tab, frag_properties, {foreign_key, undefined}}}},
                    mnesia:read({Tab, 5}, 5, read)),
                fun({T, _K}) -> T end;
	    true ->
		?match({Master, 3}, mnesia:table_info(Tab, foreign_key)),
		?match([], mnesia:table_info(Tab, foreigners)),
                fun({T, K}) -> {T, K} end
	end,

    Attr = val,
    ?match(400, mnesia:table_info(Tab, size)),
    Count = fun(_, N) -> N + 1 end,
    ?match(400, mnesia:foldl(Count, 0, Tab)),
    ?match(400, mnesia:foldr(Count, 0, Tab)),
    ?match(ok, mnesia:write({Tab, [-1], 1})),
    ?match(401, length(mnesia:match_object(Tab, {Tab, '_', '_'}, read))),
    ?match(401, length(mnesia:select(Tab, [{{Tab, '_', '$1'}, [], ['$1']}], read))),

    First = mnesia:select(Tab, [{{Tab, '_', '$1'}, [], ['$1']}], 10, read),
    TestCont = fun('$end_of_table', Total, _This) -> 
		       Total;
		  ({Res,Cont1}, Total, This) ->
		       Cont = mnesia:select(Cont1),
		       This(Cont, length(Res) + Total, This)
	       end,
    ?match(401, TestCont(First, 0, TestCont)),

    %% OTP 
    [_, Frag2|_] = frag_names(Tab),
    Frag2key = mnesia:dirty_first(Frag2),
    ?match({[Frag2key],_},mnesia:select(Tab,[{{Tab,Frag2key,'$1'},[],['$1']}],100,read)),

    ?match([{Tab, [-1], 1}], mnesia:read(Foreign({Tab, 1}), [-1], read)),
    ?match(401, mnesia:foldl(Count, 0, Tab)),
    ?match(401, mnesia:foldr(Count, 0, Tab)),
    ?match(ok, mnesia:delete(Foreign({Tab, 2}), 2, write)),
    ?match([], mnesia:read(Foreign({Tab, 2}), 2, read)),
    ?match([{Tab, 3, 3}], mnesia:read(Foreign({Tab, 3}), 3, read)),
    ?match(400, mnesia:foldl(Count, 0, Tab)),
    ?match(400, mnesia:foldr(Count, 0, Tab)),
    ?match(ok, mnesia:delete_object({Tab, 3, 3})),
    ?match([], mnesia:read(Foreign({Tab, 3}), 3, read)),
    One = lists:sort([{Tab, 1, 1}, {Tab, [-1], 1}]),
    Pat = {Tab, '$1', 1},
    ?match(One, lists:sort(mnesia:match_object(Tab, Pat, read))),
    ?match([1,[-1]], lists:sort(mnesia:select(Tab, [{Pat, [], ['$1']}], read))),
    ?match([[[-1]]], lists:sort(mnesia:select(Tab, [{Pat, [{is_list, '$1'}], [['$1']]}], read))),
    ?match([[1, 100]], lists:sort(mnesia:select(Tab, [{Pat, [{is_integer, '$1'}], [['$1',100]]}], read))),
    ?match([1,[-1]], lists:sort(mnesia:select(Tab, [{Pat, [{is_list, '$1'}], ['$1']},{Pat, [{is_integer, '$1'}], ['$1']}], read))),
    ?match(One, lists:sort(mnesia:index_match_object(Tab, Pat, Attr, read) )),
    ?match(One, lists:sort(mnesia:index_read(Tab, 1, Attr))),
    Keys = mnesia:all_keys(Tab),
    ?match([-1], lists:max(Keys)),  %% OTP-3779
    ?match(399, length(Keys)),
    ?match(399, mnesia:foldl(Count, 0, Tab)),
    ?match(399, mnesia:foldr(Count, 0, Tab)),

    ?match(Pool, lists:sort(mnesia:lock({table, Tab}, write))),

    done.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

iter_access(doc) ->
    ["Cover table iteration via callback interface"];
iter_access(suite) -> [];
iter_access(Config) when is_list(Config) ->
    Nodes = ?acquire_nodes(3, Config),

    Tab = frag_access,
    Pool = lists:sort(Nodes),
    Props = [{n_fragments, 20},
	     {n_ram_copies, 2},
	     {node_pool, Pool}],
    Def = [{frag_properties, Props},
	   {type, ordered_set},
	   {index, [val]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    [frag_write(Tab, {Tab, Id, Id})  || Id <- lists:seq(1, 400)],

    FragNames = frag_names(Tab),
    RawRead = 
	fun(Frag) -> 
		Node = mnesia:table_info(Frag, where_to_read),
		{Frag, rpc:call(Node, ets, tab2list, [Frag])}
	end,
    
    ?match(done, mnesia:activity(transaction, fun nice_iter_access/3, [Tab, FragNames, RawRead], mnesia_frag)),

    FragNames = frag_names(Tab),
    [First, Second | _] = FragNames,
    [Last, LastButOne | _] = lists:reverse(FragNames),

    ?match({atomic, ok}, mnesia:clear_table(First)),
    ?match({atomic, ok}, mnesia:clear_table(Second)),
    ?match({atomic, ok}, mnesia:clear_table(lists:nth(8, FragNames))),
    ?match({atomic, ok}, mnesia:clear_table(lists:nth(9, FragNames))),
    ?match({atomic, ok}, mnesia:clear_table(lists:nth(10, FragNames))),
    ?match({atomic, ok}, mnesia:clear_table(lists:nth(11, FragNames))),
    ?match({atomic, ok}, mnesia:clear_table(LastButOne)),
    ?match({atomic, ok}, mnesia:clear_table(Last)),

    ?match(done, mnesia:activity(transaction, fun evil_iter_access/3, [Tab, FragNames, RawRead], mnesia_frag)),
    Size = fun(Table) -> mnesia:table_info(Table, size) end,
    ?match(true, 0 < mnesia:activity(transaction, Size, [Tab], mnesia_frag)),
    ?match({atomic, ok}, mnesia:activity(ets, fun() -> mnesia:clear_table(Tab) end, mnesia_frag)),
    ?match(0, mnesia:activity(transaction, Size, [Tab], mnesia_frag)),
    
    ?verify_mnesia(Nodes, []).

nice_iter_access(Tab, FragNames, RawRead) ->
    RawData = ?ignore(lists:map(RawRead, FragNames)),
    Keys = [K || {_, Recs} <- RawData, {_, K, _} <- Recs],
    ExpectedFirst = hd(Keys),
    ?match(ExpectedFirst, mnesia:first(Tab)),
    ExpectedLast = lists:last(Keys),
    ?match(ExpectedLast, mnesia:last(Tab)),
    
    ExpectedAllPrev = ['$end_of_table' | lists:droplast(Keys)],
    ?match(ExpectedAllPrev, lists:map(fun(K) -> mnesia:prev(Tab, K) end, Keys)),
    
    ExpectedAllNext = tl(Keys) ++ ['$end_of_table'],
    ?match(ExpectedAllNext, lists:map(fun(K) -> mnesia:next(Tab, K) end, Keys)),

    done.

evil_iter_access(Tab, FragNames, RawRead) ->
    RawData = ?ignore(lists:map(RawRead, FragNames)),
    Keys = [K || {_, Recs} <- RawData, {_, K, _} <- Recs],
    ExpectedFirst = hd(Keys),
    ?match(ExpectedFirst, mnesia:first(Tab)),
    ExpectedLast = lists:last(Keys),
    ?match(ExpectedLast, mnesia:last(Tab)),
    
    ExpectedAllPrev = ['$end_of_table' | lists:droplast(Keys)],
    ?match(ExpectedAllPrev, lists:map(fun(K) -> mnesia:prev(Tab, K) end, Keys)),
    
    ExpectedAllNext = tl(Keys) ++ ['$end_of_table'],
    ?match(ExpectedAllNext, lists:map(fun(K) -> mnesia:next(Tab, K) end, Keys)),

    done.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

consistency(doc) ->
    ["Add and delete fragments during TPC-B"];
consistency(suite) -> [];
consistency(Config) when is_list(Config) ->
    ?skip("Not yet implemented (NYI).~n", []),
    Nodes = ?acquire_nodes(2, Config),
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


evil_create(suite) -> [];
evil_create(Config) when is_list(Config) ->
    [Node1, _Node2] = Nodes = ?acquire_nodes(2, Config),
    
    Create = fun(T, D, P) -> mnesia:create_table(T, [{frag_properties, P}| D]) end,
    
    Tab = evil_create,
    %% Props in general
    ?match({aborted, {badarg, Tab, {frag_properties, no_list}}},
	   Create(Tab, [], no_list)),
    ?match({aborted, {badarg,Tab , [no_tuple]}},
	   Create(Tab, [], [no_tuple])),
    ?match({aborted,{badarg, Tab, bad_key}},
	   Create(Tab, [], [{bad_key, 7}])),

    %% n_fragments
    ?match({aborted,{badarg, Tab, [{n_fragments}]}},
	   Create(Tab, [], [{n_fragments}])),
    ?match({aborted,{badarg, Tab, [{n_fragments, 1, 1}]}},
	   Create(Tab, [], [{n_fragments, 1, 1}])),
    ?match({aborted, {bad_type,Tab, {n_fragments, a}}},
	   Create(Tab, [], [{n_fragments, a}])),
    ?match({aborted, {bad_type, Tab, {n_fragments, 0}}},
	   Create(Tab, [], [{n_fragments, 0}])),

    %% *_copies
    ?match({aborted, {bad_type, Tab, {n_ram_copies, -1}}},
	   Create(Tab, [], [{n_ram_copies, -1}, {n_fragments, 1}])),
    ?match({aborted, {bad_type, Tab, {n_disc_copies, -1}}},
	   Create(Tab, [], [{n_disc_copies, -1}, {n_fragments, 1}])),
    ?match({aborted, {bad_type, Tab, {n_disc_only_copies, -1}}},
	   Create(Tab, [], [{n_disc_only_copies, -1}, {n_fragments, 1}])),

    %% node_pool
    ?match({aborted, {bad_type, Tab, {node_pool, 0}}},
	   Create(Tab, [], [{node_pool, 0}])),
    ?match({aborted, {combine_error, Tab, "Too few nodes in node_pool"}},
	   Create(Tab, [], [{n_ram_copies, 2}, {node_pool, [Node1]}])),

    %% foreign_key
    ?match({aborted, {bad_type, Tab, {foreign_key, bad_key}}},
	   Create(Tab, [], [{foreign_key, bad_key}])), 
    ?match({aborted,{bad_type, Tab, {foreign_key, {bad_key}}}}, 
	   Create(Tab, [], [{foreign_key, {bad_key}}])), 
    ?match({aborted, {no_exists, {bad_tab, frag_properties}}},
	   Create(Tab, [], [{foreign_key, {bad_tab, val}}])), 
    ?match({aborted, {combine_error, Tab, {Tab, val}}},
	   Create(Tab, [], [{foreign_key, {Tab, val}}])),
    ?match({atomic, ok},
	   Create(Tab, [], [{n_fragments, 1}])),
	   
    ?match({aborted, {already_exists, Tab}},
	   Create(Tab, [], [{n_fragments, 1}])),

    Tab2 = evil_create2,
    ?match({aborted, {bad_type, no_attr}},
	   Create(Tab2, [], [{foreign_key, {Tab, no_attr}}])),
    ?match({aborted, {combine_error, Tab2, _, _, _}},
	   Create(Tab2, [], [{foreign_key, {Tab, val}},
			     {node_pool, [Node1]}])),
    ?match({aborted, {combine_error, Tab2, _, _, _}},
	   Create(Tab2, [], [{foreign_key, {Tab, val}},
			     {n_fragments, 2}])),
    ?match({atomic, ok},
	   Create(Tab2, [{attributes, [a, b, c]}], [{foreign_key, {Tab, c}}])),
    Tab3 = evil_create3,
    ?match({aborted, {combine_error, Tab3, _, _, _}},
	   Create(Tab3, [{attributes, [a, b]}], [{foreign_key, {Tab2, b}}])),
    ?match({atomic, ok},
	   Create(Tab3, [{attributes, [a, b]}], [{foreign_key, {Tab, b}}])),
    
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

evil_delete(suite) -> [];
evil_delete(Config) when is_list(Config) ->
    ?skip("Not yet implemented (NYI).~n", []),
    Nodes = ?acquire_nodes(2, Config),
    ?verify_mnesia(Nodes, []).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

evil_change(suite) -> [];
evil_change(Config) when is_list(Config) ->
    [N1,N2,_N3] = Nodes = ?acquire_nodes(3, Config),
    Create = fun(T, D, P) -> mnesia:create_table(T, [{frag_properties, P}| D]) end,
    Props1 = [{n_fragments, 2}, {node_pool, [N1]}],
    Tab1 = evil_change_ram,
    ?match({atomic, ok}, Create(Tab1, [], Props1)),
    
    ?match({atomic,ok}, mnesia:change_table_frag(Tab1, {add_frag, Nodes})),
    Dist10 =  frag_dist(Tab1), 
    ?match([{N1,3}], Dist10), 
    ?match({atomic, ok}, mnesia:change_table_frag(Tab1, {add_node, N2})),
    Dist11 =  frag_dist(Tab1),
    ?match([{N2,0},{N1,3}], Dist11),
    mnesia_test_lib:kill_mnesia([N2]),
    ?match({aborted,_}, mnesia:change_table_frag(Tab1, {add_frag, [N2,N1]})),
    ?verbose("~p~n",[frag_dist(Tab1)]),
    mnesia_test_lib:start_mnesia([N2]),

    Tab2 = evil_change_disc,
    ?match({atomic,ok}, Create(Tab2,[],[{n_disc_copies,1},{n_fragments,1},{node_pool,[N1,N2]}])),
    ?verbose("~p~n", [frag_dist(Tab2)]),
    ?match({atomic,ok}, mnesia:change_table_frag(Tab2, {add_frag, [N1,N2]})),
    _Dist20 =  frag_dist(Tab2), 
    mnesia_test_lib:kill_mnesia([N2]),
    ?match({atomic,ok}, mnesia:change_table_frag(Tab2, {add_frag, [N1,N2]})),
    ?match({aborted,_}, mnesia:change_table_frag(Tab2, {add_frag, [N2,N1]})),

    mnesia_test_lib:start_mnesia([N2]),
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

evil_combine(doc) -> ["Bug in mnesia_4.1.5. and earlier"];
evil_combine(suite) -> [];
evil_combine(Config) when is_list(Config) ->
    [Node1] = Nodes = ?acquire_nodes(1, Config),
    ?match({atomic, ok},mnesia:create_table(tab1, [{disc_copies, [Node1]},
						   {frag_properties, [{n_fragments, 2},
								      {node_pool, [Node1]},
								      {n_disc_copies, 1}]}])),
    ?match({atomic, ok},mnesia:create_table(tab2, [{disc_copies, [Node1]}])),
    mnesia:wait_for_tables([tab1, tab2], infinity),

    Add2 = fun() -> 
		   mnesia:transaction(fun() ->
					      mnesia:write({tab2,1,1})
				      end)
	   end,
    Fun = fun() ->
		  Add2(),
		  mnesia:write({tab1,9,10})
	  end,
    ?match(ok, mnesia:activity({transaction, 1}, Fun, [], mnesia_frag)),
    
    Read = fun(T, K) ->
		   mnesia:read(T, K, read)
	   end,
    
    ?match([{tab1,9,10}],mnesia:activity(async_dirty, Read, [tab1, 9], mnesia_frag)),
    ?match([{tab2,1,1}],mnesia:activity(async_dirty, Read, [tab2, 1], mnesia_frag)),
    
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

evil_loop(doc) -> ["Test select/[14]"];
evil_loop(suite) -> [];
evil_loop(Config) when is_list(Config) ->
    [Node1,_Node2] = ?acquire_nodes(2, Config), 
    Tab1 = ss_oset,
    Tab2 = ss_set,
    Tab3 = ss_bag,
    Tabs = [Tab1, Tab2, Tab3],
    RecName = ss,
    ?match({atomic, ok},  mnesia:create_table([{name, Tab1}, 
					       {ram_copies, [Node1]}, 
					       {record_name, RecName},
					       {type,  ordered_set}])),
    ?match({atomic, ok},  mnesia:create_table([{name, Tab2},
					       {record_name, RecName},
					       {ram_copies, [Node1]},
					       {type,  set}])),
    ?match({atomic, ok},  mnesia:create_table([{name, Tab3},
					       {record_name, RecName},
					       {ram_copies, [Node1]}, 
					       {type,  bag}])),
    Keys = [-3, -2] ++ lists:seq(1, 5, 2) ++ lists:seq(6, 10),
    Recs = [{RecName, K, K} || K <- Keys],
    [mnesia:dirty_write(Tab1, R) || R <- Recs],
    [mnesia:dirty_write(Tab2, R) || R <- Recs],
    [mnesia:dirty_write(Tab3, R) || R <- Recs],

    Activate = 
	fun(Tab) ->
		?match({atomic, ok}, mnesia:change_table_frag(Tab, {activate, []})),
		Dist = frag_dist(Tab),
		?match({atomic, ok}, mnesia:change_table_frag(Tab, {add_frag, Dist}))
	end,

    Activate(Tab1),
    Activate(Tab2),
    Activate(Tab3),

    Match  = fun(Tab) -> mnesia:match_object(Tab, {'_', '_', '_'}, write) end,
    Select = fun(Tab) -> mnesia:select(Tab, [{'_', [], ['$_']}]) end,
    Trans  = fun(Fun, Args) -> mnesia:activity(transaction, Fun, Args, mnesia_frag) end,
    LoopHelp = fun('$end_of_table',_) ->
		       [];
		  ({Res,Cont},Fun) -> 
		       Sel = mnesia:select(Cont),
		       Res ++ Fun(Sel, Fun)
	       end,
    SelLoop = fun(Table) -> 
		      Sel = mnesia:select(Table, [{'_', [], ['$_']}], 1, read),
		      LoopHelp(Sel, LoopHelp)
	      end,

    R1 = {RecName, 2, 2},
    R2 = {RecName, 4, 4},
    R3 = {RecName, 2, 3},
    R4 = {RecName, 3, 1},
    R5 = {RecName, 104, 104},
    W1 = fun(Tab,Search) ->
		 mnesia:write(Tab,R1,write),
		 mnesia:write(Tab,R2,write), 
		 Search(Tab)
	 end,
    S1 = lists:sort([R1, R2| Recs]),
    ?match(S1, sort_res(Trans(W1, [Tab1, Select]))),
    ?match(S1, sort_res(Trans(W1, [Tab1, Match]))),
    ?match(S1, sort_res(Trans(W1, [Tab1, SelLoop]))),
    ?match(S1, sort_res(Trans(W1, [Tab2, Select]))),
    ?match(S1, sort_res(Trans(W1, [Tab2, SelLoop]))),
    ?match(S1, sort_res(Trans(W1, [Tab2, Match]))),
    ?match(S1, sort_res(Trans(W1, [Tab3, Select]))),
    ?match(S1, sort_res(Trans(W1, [Tab3, SelLoop]))),
    ?match(S1, sort_res(Trans(W1, [Tab3, Match]))),
    [mnesia:dirty_delete_object(Frag, R) || R <- [R1, R2], 
					   Tab <- Tabs,
					   Frag <- frag_names(Tab)],

    W2 = fun(Tab, Search) -> 
		 mnesia:write(Tab, R3, write),
		 mnesia:write(Tab, R1, write), 
		 Search(Tab)
	 end,
    S2 = lists:sort([R1 | Recs]),
    S2Bag = lists:sort([R1, R3 | Recs]),
    io:format("S2 = ~p\n", [S2]),
    ?match(S2, sort_res(Trans(W2, [Tab1, Select]))),
    ?match(S2, sort_res(Trans(W2, [Tab1, SelLoop]))),
    ?match(S2, sort_res(Trans(W2, [Tab1, Match]))),
    ?match(S2, sort_res(Trans(W2, [Tab2, Select]))),
    ?match(S2, sort_res(Trans(W2, [Tab2, SelLoop]))),
    ?match(S2, sort_res(Trans(W2, [Tab2, Match]))),
    io:format("S2Bag = ~p\n", [S2Bag]),
    ?match(S2Bag, sort_res(Trans(W2, [Tab3, Select]))),
    ?match(S2Bag, sort_res(Trans(W2, [Tab3, SelLoop]))),
    ?match(S2Bag, sort_res(Trans(W2, [Tab3, Match]))),

    W3 = fun(Tab,Search) -> 
		 mnesia:write(Tab, R4, write),
		 mnesia:delete(Tab, element(2, R1), write), 
		 Search(Tab)
	 end,
    S3Bag = lists:sort([R4 | lists:delete(R1, Recs)]),
    S3 = lists:delete({RecName, 3, 3}, S3Bag),
    ?match(S3, sort_res(Trans(W3, [Tab1, Select]))),
    ?match(S3, sort_res(Trans(W3, [Tab1, SelLoop]))),
    ?match(S3, sort_res(Trans(W3, [Tab1, Match]))),
    ?match(S3, sort_res(Trans(W3, [Tab2, SelLoop]))),
    ?match(S3, sort_res(Trans(W3, [Tab2, Select]))),
    ?match(S3, sort_res(Trans(W3, [Tab2, Match]))),
    ?match(S3Bag, sort_res(Trans(W3, [Tab3, Select]))),
    ?match(S3Bag, sort_res(Trans(W3, [Tab3, SelLoop]))),
    ?match(S3Bag, sort_res(Trans(W3, [Tab3, Match]))),

    W4 = fun(Tab,Search) -> 
		 mnesia:delete(Tab, -1, write),
		 mnesia:delete(Tab, 4 , write),
		 mnesia:delete(Tab, 17, write),
		 mnesia:delete_object(Tab, {RecName, -1, x}, write),
		 mnesia:delete_object(Tab, {RecName, 4, x}, write),
		 mnesia:delete_object(Tab, {RecName, 42, x}, write),
		 mnesia:delete_object(Tab, R2, write),
		 mnesia:write(Tab, R5, write),
		 Search(Tab)
	 end,
    S4Bag = lists:sort([R5 | S3Bag]),
    S4    = lists:sort([R5 | S3]),
    ?match(S4, sort_res(Trans(W4, [Tab1, Select]))),
    ?match(S4, sort_res(Trans(W4, [Tab1, SelLoop]))),
    ?match(S4, sort_res(Trans(W4, [Tab1, Match]))),
    ?match(S4, sort_res(Trans(W4, [Tab2, Select]))),
    ?match(S4, sort_res(Trans(W4, [Tab2, SelLoop]))),
    ?match(S4, sort_res(Trans(W4, [Tab2, Match]))),
    ?match(S4Bag, sort_res(Trans(W4, [Tab3, Select]))),
    ?match(S4Bag, sort_res(Trans(W4, [Tab3, SelLoop]))),
    ?match(S4Bag, sort_res(Trans(W4, [Tab3, Match]))),
    [mnesia:dirty_delete_object(Tab, R) || R <- [{RecName, 3, 3}, R5], Tab <- Tabs],

    %% hmmm anything more??
    
    ?verify_mnesia([Node1], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

evil_delete_db_node(doc) ->
    ["Delete db_node with a repicated table with foreign key"];
evil_delete_db_node(suite) -> [];
evil_delete_db_node(Config) when is_list(Config) ->
    Nodes = lists:sort(?acquire_nodes(2, Config)),
    Local = node(),
    Remote = hd(Nodes -- [Local]),
    
    Type = case mnesia_test_lib:diskless(Config) of 
	       true  -> n_ram_copies;
	       false -> n_disc_copies
	   end,
    Tab = frag_master,
    ?match({atomic, ok}, mnesia:create_table(Tab, [{frag_properties, [{Type, 2}, {node_pool, Nodes}]}])),
    ExtraTab = frag_foreigner,
    ?match({atomic, ok}, mnesia:create_table(ExtraTab, [{frag_properties, [{foreign_key, {Tab, key}}, {node_pool, Nodes}]}])),
    
    GetPool = fun(T) -> 
		      case lists:keysearch(node_pool, 1, mnesia:table_info (T, frag_properties)) of
			  {value, {node_pool, N}} -> lists:sort(N);
			  false                   -> []
		      end
	      end,
    ?match(Nodes, GetPool(Tab)),
    ?match(Nodes, GetPool(ExtraTab)),


    ?match(stopped, rpc:call(Remote, mnesia, stop, [])),
    ?match({atomic, ok}, mnesia:del_table_copy(schema, Remote)),
	   
    ?match([Local], GetPool(Tab)),
    ?match([Local], GetPool(ExtraTab)),
     
    ?verify_mnesia([Local], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Misc convenient helpers

frag_write(Tab, Rec) ->
    Fun = fun() -> mnesia:write(Tab, Rec, write) end,
    mnesia:activity(sync_dirty, Fun, mnesia_frag).

frag_dist(Tab) ->
    Fun = fun() -> mnesia:table_info(Tab, frag_dist) end,
    mnesia:activity(sync_dirty, Fun, mnesia_frag).

frag_names(Tab) ->
    Fun = fun() -> mnesia:table_info(Tab, frag_names) end,
    mnesia:activity(sync_dirty, Fun, mnesia_frag).

frag_rec_dist(Tab) -> 
    Fun = fun() -> mnesia:table_info(Tab, frag_size) end,
    [Size || {_, Size} <- mnesia:activity(sync_dirty, Fun, mnesia_frag)].

sort_res(List) when is_list(List) ->
    lists:sort(List);
sort_res(Else) ->
    Else.
