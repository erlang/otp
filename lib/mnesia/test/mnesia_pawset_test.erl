-module(mnesia_pawset_test).

-author('sl955@cam.ac.uk').

-include("mnesia_test_lib.hrl").

-export([init_per_testcase/2, end_per_testcase/2, init_per_group/2, end_per_group/2,
         all/0, groups/0]).
-export([match_delete_ram/1, match_object_ram/1, match_object_with_index_ram/1,
         select_with_index_ram/1]).

init_per_testcase(Func, Conf) ->
    mnesia_test_lib:init_per_testcase(Func, Conf).

end_per_testcase(Func, Conf) ->
    mnesia_test_lib:end_per_testcase(Func, Conf).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all() ->
    [match_delete_ram, match_object_ram, {group, index_tests}].

groups() ->
    [{index_tests, [], [match_object_with_index_ram, select_with_index_ram]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

match_delete_ram(suite) ->
    [];
match_delete_ram(Config) when is_list(Config) ->
    match_delete(Config, ram_copies).

match_delete(Config, Storage) ->
    Nodes = [_NodeA, NodeA1, NodeA2] = NodeNames = ?acquire_nodes(3, Config),
    Tab = match_delete,
    Def = [{Storage, NodeNames}, {type, pawbag}, {attributes, [k, v]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    Reader = fun() -> mnesia:read(Tab, a) end,
    Writer = fun(K, V) -> mnesia:write({Tab, K, V}) end,
    ObjectDeleter = fun() -> mnesia:delete_object({Tab, a, 1}) end,
    ?match(ok,
           mnesia:activity(sync_ec,
                           fun() ->
                              Writer(a, 1),
                              Writer(a, 2)
                           end)),
    ?match([{Tab, a, 1}, {Tab, a, 2}], mnesia:activity(sync_ec, Reader)),
    ?match([{Tab, a, 1}, {Tab, a, 2}],
           rpc:call(NodeA1, mnesia, activity, [sync_ec, Reader])),    %% Delete the record
    ?match([{Tab, a, 1}, {Tab, a, 2}], rpc:call(NodeA2, mnesia, activity, [sync_ec, Reader])),
    ?match(ok, mnesia:sync_ec(ObjectDeleter)),

    ?match([{Tab, a, 2}], mnesia:async_ec(Reader)),
    ?verify_mnesia(Nodes, []).

match_object_ram(suite) ->
    [];
match_object_ram(Config) when is_list(Config) ->
    match_object(Config, ram_copies).

match_object(Config, Storage) ->
    Nodes = [_NodeA, NodeA1, NodeA2] = NodeNames = ?acquire_nodes(3, Config),
    Tab = match_object,
    Def = [{Storage, NodeNames}, {type, pawbag}, {attributes, [k, v]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    Reader = fun() -> mnesia:read(Tab, a) end,
    Writer = fun(K, V) -> mnesia:write({Tab, K, V}) end,
    ObjectMatcher = fun(Pat) -> mnesia:match_object(Pat) end,

    ?match(ok,
           mnesia:activity(sync_ec,
                           fun() ->
                              Writer(a, 1),
                              Writer(a, 2),
                              Writer(b, 2)
                           end)),

    ?match([{Tab, a, 1}, {Tab, a, 2}], mnesia:activity(sync_ec, Reader)),
    ?match([{Tab, b, 2}], mnesia:activity(sync_ec, fun() -> mnesia:read(Tab, b) end)),

    ?match([{Tab, a, 1}, {Tab, a, 2}],
           lists:sort(
               mnesia:async_ec(fun() -> ObjectMatcher({Tab, a, '_'}) end))),
    ?match([{Tab, a, 2}, {Tab, b, 2}],
           lists:sort(
               rpc:call(NodeA1, mnesia, async_ec, [fun() -> ObjectMatcher({Tab, '_', 2}) end]))),

    ?match(ok, mnesia:sync_ec(fun() -> mnesia:delete_object({Tab, a, 1}) end)),

    ?match([{Tab, a, 2}],
           rpc:call(NodeA2, mnesia, async_ec, [fun() -> ObjectMatcher({Tab, a, '_'}) end])),

    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Index related tests

match_object_with_index_ram(suite) ->
    [];
match_object_with_index_ram(Config) when is_list(Config) ->
    match_object_with_index(Config, ram_copies).

match_object_with_index(Config, Storage) ->
    Nodes = [_NodeA, NodeA1, NodeA2] = NodeNames = ?acquire_nodes(3, Config),
    Tab = match_object,
    Def = [{Storage, NodeNames}, {type, pawset}, {attributes, [k, v]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    ?match({atomic, ok}, mnesia:add_table_index(Tab, v)),
    Reader = fun() -> mnesia:read(Tab, a) end,
    Writer = fun(K, V) -> mnesia:write({Tab, K, V}) end,
    ObjectMatcher = fun(Pat) -> mnesia:match_object(Pat) end,

    ?match(ok,
           mnesia:activity(sync_ec,
                           fun() ->
                              Writer(a, 1),
                              Writer(b, 2),
                              Writer(c, 2)
                           end)),

    ?match([{Tab, a, 1}], mnesia:activity(sync_ec, Reader)),
    ?match([{Tab, b, 2}], mnesia:activity(sync_ec, fun() -> mnesia:read(Tab, b) end)),

    ?match([{Tab, a, 1}], mnesia:async_ec(fun() -> ObjectMatcher({Tab, a, '_'}) end)),
    ?match([{Tab, b, 2}, {Tab, c, 2}],
           lists:sort(
               rpc:call(NodeA1, mnesia, async_ec, [fun() -> ObjectMatcher({Tab, '_', 2}) end]))),

    ?match(ok, mnesia:sync_ec(fun() -> mnesia:delete_object({Tab, b, 2}) end)),

    ?match([{Tab, c, 2}],
           rpc:call(NodeA2, mnesia, async_ec, [fun() -> ObjectMatcher({Tab, '_', 2}) end])),

    ?verify_mnesia(Nodes, []).

select_with_index_ram(suite) ->
    [];
select_with_index_ram(Config) when is_list(Config) ->
    select_with_index(Config, ram_copies).

select_with_index(Config, Storage) ->
    Nodes = NodeNames = ?acquire_nodes(3, Config),
    Tab = select_with_index,
    Def = [{Storage, NodeNames}, {type, pawset}, {attributes, [k, v]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    ?match({atomic, ok}, mnesia:add_table_index(Tab, v)),
    Reader = fun() -> mnesia:read(Tab, a) end,
    Writer = fun(K, V) -> mnesia:write({Tab, K, V}) end,

    ?match(ok,
           mnesia:activity(sync_ec,
                           fun() ->
                              Writer(a, 1),
                              Writer(b, 2),
                              Writer(c, 2),
                              Writer(d, 10)
                           end)),

    ?match([{Tab, a, 1}], mnesia:activity(sync_ec, Reader)),
    ?match([{Tab, b, 2}], mnesia:activity(sync_ec, fun() -> mnesia:read(Tab, b) end)),

    ?match([1],
           mnesia:sync_ec(fun() -> mnesia:select(Tab, [{{Tab, a, '$2'}, [], ['$2']}]) end)),

    ?match([b, c],
           mnesia:sync_ec(fun() -> mnesia:select(Tab, [{{Tab, '$1', 2}, [], ['$1']}]) end)),

    ?match([],
           mnesia:async_ec(fun() -> mnesia:select(Tab, [{{Tab, d, '$1'}, ['<', '$1'], ['$_']}])
                           end)),

    ?verify_mnesia(Nodes, []).
