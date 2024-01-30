-module(mnesia_ec_test).

-author('sl955@cam.ac.uk').

-include("mnesia_test_lib.hrl").

-export([init_per_testcase/2, end_per_testcase/2, init_per_group/2, end_per_group/2,
         all/0, groups/0]).
-export([sync_ec_rw_ram/1, sync_ec_rw_compare_dirty_ram/1, sync_ec_rwd_ram/1]).
-export([async_ec_index_read_ram/1]).
-export([async_ec_rw_ram/1, async_ec_rwd_ram/1, async_ec_rw_compare_dirty_ram/1]).
-export([ec_rwd_block_ram/1, ec_block_40keys_ram/1, ec_write_block_ram/1,
         ec_delete_block_ram/1]).

init_per_testcase(Func, Conf) ->
    mnesia_test_lib:init_per_testcase(Func, Conf).

end_per_testcase(Func, Conf) ->
    mnesia_test_lib:end_per_testcase(Func, Conf).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all() ->
    [{group, ec_index_read},
     {group, ec_cc_crud},
     {group, ec_compare_dirty}].    % {group, ec_cc_crud_block}

groups() ->
    [{ec_cc_crud, [], [async_ec_rw_ram, async_ec_rwd_ram, sync_ec_rw_ram, sync_ec_rwd_ram]},
     {ec_index_read, [], [async_ec_index_read_ram]},
     {ec_compare_dirty, [], [async_ec_rw_compare_dirty_ram, sync_ec_rw_compare_dirty_ram]},
     {ec_cc_crud_block,
      [sequence],
      [ec_rwd_block_ram, ec_block_40keys_ram, ec_write_block_ram, ec_delete_block_ram]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Concurrent read and write
%% Requires a custom distrbution protocol https://github.com/rabbitmq/inet_tcp_proxy
%% for simulating network partitions. 
%% These require a modification of %% mnesia_test_lib on how nodes are started, 
%% as slave_start does not seem to support custom distribution protocols.
%% Disabled by default for now.

async_ec_rw_ram(suite) ->
    [];
async_ec_rw_ram(Config) when is_list(Config) ->
    ec_rw([{kind, async_ec} | Config], ram_copies).

sync_ec_rw_ram(suite) ->
    [];
sync_ec_rw_ram(Config) when is_list(Config) ->
    ec_rw([{kind, sync_ec} | Config], ram_copies).

ec_rw(Config, Storage) ->
    [_NodeA, NodeA1, NodeA2] = NodeNames = ?acquire_nodes(3, Config),
    Tab = ec_rw,
    Def = [{Storage, NodeNames}, {type, pawset}, {attributes, [k, v]}],
    Kind = mnesia_test_lib:lookup_config(kind, Config),
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),

    Reader = fun() -> mnesia:read(Tab, a) end,
    Writer = fun() -> mnesia:write({Tab, a, 1}) end,
    sleep_if_async(Kind, 1000),
    ?match(ok, mnesia:activity(Kind, Writer)),
    ?match([{Tab, a, 1}], mnesia:activity(Kind, Reader)),
    sleep_if_async(Kind, 1000),
    ?match([{Tab, a, 1}], mnesia:activity(Kind, Reader)),
    ?match([{Tab, a, 1}], rpc:call(NodeA1, mnesia, activity, [Kind, Reader])),
    ?match([{Tab, a, 1}], rpc:call(NodeA2, mnesia, activity, [Kind, Reader])).

async_ec_rwd_ram(suite) ->
    [];
async_ec_rwd_ram(Config) ->
    ec_rwd([{kind, async_ec} | Config], ram_copies).

sync_ec_rwd_ram(suite) ->
    [];
sync_ec_rwd_ram(Config) ->
    ec_rwd([{kind, sync_ec} | Config], ram_copies).

ec_rwd(Config, Storage) ->
    Nodes = [_NodeA, NodeA1, NodeA2] = ?acquire_nodes(3, Config),
    Tab = ec_rwd,
    Kind = mnesia_test_lib:lookup_config(kind, Config),
    Def = [{Storage, Nodes}, {type, pawset}, {attributes, [k, v]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    Writer = fun(K, V) -> mnesia:write({Tab, K, V}) end,
    Deleter = fun(K) -> mnesia:delete({Tab, K}) end,

    spawn(fun() ->
             lists:foreach(fun(N) -> mnesia:activity(Kind, fun() -> Writer(N, 4) end) end,
                           lists:seq(1, 20))
          end),
    WriteAndDelete =
        fun() ->
           lists:foreach(fun(N) -> mnesia:activity(Kind, fun() -> Writer(N, 4) end) end,
                         lists:seq(21, 30)),
           sleep_if_async(async_ec, 1000),
           lists:foreach(fun(N) -> mnesia:activity(Kind, fun() -> Deleter(N) end) end,
                         lists:seq(25, 29))
        end,
    spawn(NodeA1, WriteAndDelete),

    spawn(NodeA2,
          lists,
          foreach,
          [fun(N) -> mnesia:activity(Kind, fun() -> Writer(N, 4) end) end, lists:seq(31, 40)]),

    sleep_if_async(async_ec, 2000),
    Res = lists:seq(1, 40) -- lists:seq(25, 29),
    ?match(Res,
           lists:sort(
               mnesia:async_ec(fun() -> mnesia:all_keys(Tab) end))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sync_ec_rw_compare_dirty_ram(suite) ->
    [];
sync_ec_rw_compare_dirty_ram(Config) ->
    ec_rw_compare_dirty([{kind, sync_ec} | Config], ram_copies).

async_ec_rw_compare_dirty_ram(suite) ->
    [];
async_ec_rw_compare_dirty_ram(Config) ->
    ec_rw_compare_dirty([{kind, async_ec} | Config], ram_copies).

ec_rw_compare_dirty(Config, Storage) ->
    Nodes = [_NodeA, NodeA1, NodeA2] = ?acquire_nodes(3, Config),
    Tab1 = ec_rw,
    Tab2 = dirty_rw,
    Def1 = [{Storage, Nodes}, {type, pawset}, {attributes, [k, v]}],
    Def2 = [{Storage, Nodes}, {attributes, [k, v]}],
    Kind = mnesia_test_lib:lookup_config(kind, Config),
    ?match({atomic, ok}, mnesia:create_table(Tab1, Def1)),
    ?match({atomic, ok}, mnesia:create_table(Tab2, Def2)),

    Writer = fun(K, V) -> mnesia:write({Tab1, K, V}) end,
    spawn(fun() -> [mnesia:dirty_write({Tab2, N, 1}) || N <- lists:seq(1, 20)] end),
    spawn(fun() ->
             mnesia_rpc:call(NodeA1,
                             lists,
                             foreach,
                             [fun(N) -> mnesia:dirty_write({Tab2, N, 2}) end, lists:seq(21, 30)])
          end),
    spawn(fun() ->
             mnesia_rpc:call(NodeA2,
                             lists,
                             foreach,
                             [fun(N) -> mnesia:dirty_write({Tab2, N, 3}) end, lists:seq(31, 40)])
          end),

    spawn(fun() ->
             lists:foreach(fun(N) -> mnesia:activity(Kind, fun() -> Writer(N, 4) end) end,
                           lists:seq(1, 20))
          end),
    spawn(fun() ->
             mnesia_rpc:call(NodeA1,
                             lists,
                             foreach,
                             [fun(N) -> mnesia:activity(Kind, fun() -> Writer(N, 5) end) end,
                              lists:seq(21, 30)])
          end),
    spawn(fun() ->
             mnesia_rpc:call(NodeA2,
                             lists,
                             foreach,
                             [fun(N) -> mnesia:activity(Kind, fun() -> Writer(N, 6) end) end,
                              lists:seq(31, 40)])
          end),

    sleep_if_async(Kind, 1000),

    DirtyKeys =
        lists:sort(
            mnesia:dirty_all_keys(Tab2)),
    EcKeys =
        lists:sort(
            mnesia:activity(Kind, fun() -> mnesia:all_keys(Tab1) end)),
    ?match(DirtyKeys, EcKeys).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

async_ec_index_read_ram(suite) ->
    [];
async_ec_index_read_ram(Config) ->
    async_ec_index_read(Config, ram_copies).

async_ec_index_read(Config, Storage) ->
    Nodes = [_NodeA, NodeA1, NodeA2] = ?acquire_nodes(3, Config),
    Tab = ec_index_read,
    Def = [{Storage, Nodes}, {type, pawset}, {attributes, [k, v]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    ?match({atomic, ok}, mnesia:add_table_index(Tab, v)),

    Writer = fun(K, V) -> mnesia:write({Tab, K, V}) end,
    spawn(fun() ->
             lists:foreach(fun(N) -> mnesia:activity(async_ec, fun() -> Writer(N, 4) end) end,
                           lists:seq(1, 20))
          end),
    spawn(fun() ->
             mnesia_rpc:call(NodeA1,
                             lists,
                             foreach,
                             [fun(N) -> mnesia:activity(async_ec, fun() -> Writer(N, 5) end) end,
                              lists:seq(21, 30)])
          end),
    spawn(fun() ->
             mnesia_rpc:call(NodeA2,
                             lists,
                             foreach,
                             [fun(N) -> mnesia:activity(async_ec, fun() -> Writer(N, 6) end) end,
                              lists:seq(31, 40)])
          end),

    sleep_if_async(async_ec, 1000),

    Keys5 = mnesia:async_ec(fun() -> mnesia:index_read(Tab, 5, 3) end),
    Expected5 = [{Tab, N, 5} || N <- lists:seq(21, 30)],
    ?match(Expected5, lists:sort(Keys5)),
    Keys6 = mnesia:async_ec(fun() -> mnesia:index_match_object({Tab, '_', 6}, v) end),
    Expected6 = [{Tab, N, 6} || N <- lists:seq(31, 40)],
    ?match(Expected6, lists:sort(Keys6)),
    KeyNone = mnesia:async_ec(fun() -> mnesia:index_read(Tab, 7, 3) end),
    ?match([], KeyNone),

    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ec_write_block_ram(suite) ->
    [];
ec_write_block_ram(Config) when is_list(Config) ->
    ec_write_block([{is_port, true} | Config], ram_copies).

ec_write_block(Config, Storage) ->
    [NodeA, NodeA1, NodeA2] = NodeNames = ?acquire_nodes(3, Config),
    timer:sleep(500),
    Tab = ec_write_block,
    Def = [{Storage, NodeNames}, {type, pawset}, {attributes, [k, v]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),

    Reader = fun(K) -> mnesia:read(Tab, K) end,
    Writer = fun(K, V) -> mnesia:write({Tab, K, V}) end,
    BlockAndWrite =
        fun() ->
           % block connnection from NodeA1 to NodeA
           % if called at A1
           inet_tcp_proxy_dist:block(NodeA),
           timer:sleep(500),
           mnesia:async_ec(fun() -> Writer(a, 1) end)
        end,
    spawn(NodeA1, BlockAndWrite),
    timer:sleep(1000),
    ?match([], mnesia:async_ec(fun() -> Reader(a) end)),
    ?match(ok, mnesia:async_ec(fun() -> Writer(a, 3) end)),
    ?match([{Tab, a, 3}], mnesia:async_ec(fun() -> Reader(a) end)),

    % allow connection
    spawn(NodeA1, fun() -> inet_tcp_proxy_dist:allow(NodeA) end),
    timer:sleep(1000),

    ?match([{Tab, a, 1}], mnesia:async_ec(fun() -> Reader(a) end)),
    ?match([{Tab, a, 1}], rpc:call(NodeA1, mnesia, async_ec, [fun() -> Reader(a) end])),
    ?match([{Tab, a, 1}], rpc:call(NodeA2, mnesia, async_ec, [fun() -> Reader(a) end])).

ec_delete_block_ram(suite) ->
    [];
ec_delete_block_ram(Config) when is_list(Config) ->
    ec_delete_block([{is_port, true} | Config], ram_copies).

ec_delete_block(Config, Storage) ->
    [NodeA, NodeA1, NodeA2] = NodeNames = ?acquire_nodes(3, Config),
    timer:sleep(500),
    Tab = ec_delete_block,
    Def = [{Storage, NodeNames}, {type, pawset}, {attributes, [k, v]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),

    Reader = fun(K) -> mnesia:read(Tab, K) end,
    Writer = fun(K, V) -> mnesia:write({Tab, K, V}) end,
    Deleter = fun(K) -> mnesia:delete({Tab, K}) end,

    BlockAndDelete =
        fun() ->
           % block connnection from NodeA1 to NodeA
           % if called at A1
           inet_tcp_proxy_dist:block(NodeA),
           timer:sleep(500),
           mnesia:async_ec(fun() -> Deleter(a, 1) end)
        end,
    spawn(fun() -> mnesia:async_ec(fun() -> Writer(b, 2) end) end),
    spawn(NodeA1, fun() -> mnesia:async_ec(fun() -> Writer(a, 1) end) end),
    timer:sleep(1000),
    ?match([{Tab, a, 1}], mnesia:async_ec(fun() -> Reader(a) end)),

    spawn(NodeA1, BlockAndDelete),
    spawn(fun() -> mnesia:async_ec(fun() -> Deleter(a) end) end),

    timer:sleep(500),
    ?match([], mnesia:async_ec(fun() -> Reader(a) end)),
    ?match([{Tab, b, 2}], mnesia:async_ec(fun() -> Reader(b) end)),

    % allow connection
    spawn(NodeA1, fun() -> inet_tcp_proxy_dist:allow(NodeA) end),
    timer:sleep(1000),

    ?match([], mnesia:async_ec(fun() -> Reader(a) end)),
    ?match([], rpc:call(NodeA1, mnesia, async_ec, [fun() -> Reader(a) end])),
    ?match([], rpc:call(NodeA2, mnesia, async_ec, [fun() -> Reader(a) end])).

ec_rwd_block_ram(suite) ->
    [];
ec_rwd_block_ram(Config) when is_list(Config) ->
    ec_rwd_block([{is_port, true} | Config], ram_copies).

ec_rwd_block(Config, Storage) ->
    [NodeA, NodeA1, NodeA2] = NodeNames = ?acquire_nodes(3, Config),
    timer:sleep(500),
    Tab = ec_rwd_block,
    Def = [{Storage, NodeNames}, {type, pawset}, {attributes, [k, v]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),

    Reader = fun() -> mnesia:read(Tab, a) end,
    Writer = fun() -> mnesia:write({Tab, a, 1}) end,
    Deleter = fun() -> mnesia:delete({Tab, a}) end,
    BlockAndWrite =
        fun() ->
           inet_tcp_proxy_dist:block(NodeA),
           timer:sleep(500),
           mnesia:async_ec(Writer)
        end,
    spawn(NodeA1, BlockAndWrite),
    timer:sleep(1000),
    ?match([], mnesia:async_ec(Reader)),
    ?match(ok, mnesia:async_ec(Writer)),
    ?match([{Tab, a, 1}], mnesia:async_ec(Reader)),
    ?match(ok, mnesia:async_ec(Deleter)),

    spawn(NodeA1, fun() -> inet_tcp_proxy_dist:allow(NodeA) end),
    timer:sleep(1000),

    ?match([{Tab, a, 1}], mnesia:async_ec(Reader)),
    ?match([{Tab, a, 1}], rpc:call(NodeA1, mnesia, async_ec, [Reader])),
    ?match([{Tab, a, 1}], rpc:call(NodeA2, mnesia, async_ec, [Reader])).

ec_block_40keys_ram(suite) ->
    [];
ec_block_40keys_ram(Config) ->
    ec_block_40keys([{is_port, true} | Config], ram_copies).

ec_block_40keys(Config, Storage) ->
    Nodes = [_NodeA, NodeA1, NodeA2] = ?acquire_nodes(3, Config),
    Tab = ec_block_40keys,
    Def = [{Storage, Nodes}, {type, pawset}, {attributes, [k, v]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    Writer = fun(K, V) -> mnesia:write({Tab, K, V}) end,
    Deleter = fun(K) -> mnesia:delete({Tab, K}) end,

    spawn(fun() ->
             lists:foreach(fun(N) -> mnesia:async_ec(fun() -> Writer(N, 4) end) end,
                           lists:seq(1, 20))
          end),
    spawn(NodeA1, inet_tcp_proxy_dist, block, [NodeA2]),
    WriteAndDelete =
        fun() ->
           lists:foreach(fun(N) -> mnesia:async_ec(fun() -> Writer(N, 4) end) end,
                         lists:seq(21, 30)),
           sleep_if_async(async_ec, 1000),
           lists:foreach(fun(N) -> mnesia:async_ec(fun() -> Deleter(N) end) end, lists:seq(25, 29))
        end,
    spawn(NodeA1, WriteAndDelete),

    spawn(NodeA2,
          lists,
          foreach,
          [fun(N) -> mnesia:async_ec(fun() -> Writer(N, 4) end) end, lists:seq(31, 40)]),

    spawn(NodeA1, inet_tcp_proxy_dist, allow, [NodeA2]),

    sleep_if_async(async_ec, 3000),
    Res = lists:seq(1, 40) -- lists:seq(25, 29),
    ?match(Res,
           lists:sort(
               mnesia:async_ec(fun() -> mnesia:all_keys(Tab) end))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sleep_if_async(async_ec, Time) ->
    timer:sleep(Time);
sleep_if_async(sync_ec, _T) ->
    ok.
