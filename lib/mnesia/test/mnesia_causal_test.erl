-module(mnesia_causal_test).

-author('sl955@cam.ac.uk').

-include("mnesia_test_lib.hrl").

-export([init_per_testcase/2, end_per_testcase/2, init_per_group/2, end_per_group/2,
         all/0, groups/0]).
-export([empty_final_buffer_ram/1]).

init_per_testcase(Func, Conf) ->
    mnesia_test_lib:init_per_testcase(Func, Conf).

end_per_testcase(Func, Conf) ->
    mnesia_test_lib:end_per_testcase(Func, Conf).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all() ->
    [{group, causal_consistency},
     empty_final_buffer_ram].

groups() ->
    [{causal_consistency, [], []}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

empty_final_buffer_ram(suite) ->
    [];
empty_final_buffer_ram(Config) when is_list(Config) ->
    empty_final_buffer(Config, ram_copies).

empty_final_buffer(Config, Storage) ->
    Nodes = [_NodeA, NodeA1, NodeA2] = NodeNames = ?acquire_nodes(3, Config),
    Tab = empty_final_buffer,
    Def = [{Storage, NodeNames}, {type, pawset}, {attributes, [k, v]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),

    Writer = fun(K, V) -> mnesia:write({Tab, K, V}) end,
    ?match(ok,
           mnesia:activity(async_ec,
                           fun() ->
                              Writer(2, a),
                              Writer(1, a)
                           end)),
    spawn_monitor(NodeA1, mnesia, async_ec, [fun() -> [Writer(N, a) || N <- lists:seq(3, 10)] end]),
    spawn_monitor(NodeA1, mnesia, async_ec, [fun() -> [Writer(N, b) || N <- lists:seq(12, 20)] end]),

    timer:sleep(3000),

    ?match([], mnesia_causal:get_buffered()),
    ?match([], rpc:call(NodeA1, mnesia_causal, get_buffered, [])),
    ?match([], rpc:call(NodeA2, mnesia_causal, get_buffered, [])),

    ?verify_mnesia(Nodes, []).
