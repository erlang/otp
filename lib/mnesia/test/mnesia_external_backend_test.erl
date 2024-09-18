-module(mnesia_external_backend_test).

-export([init_per_testcase/2, end_per_testcase/2,
         init_per_group/2, end_per_group/2,
         suite/0, all/0, groups/0]).

-export([conversion_from_external_to_disc_copies_results_in_data_loss_after_node_restart/1]).

-include("mnesia_test_lib.hrl").

-record(some_rec, {some_id :: atom(), some_int :: number(), some_string :: string()}).

-define(acquire(N, Config),
        mnesia_test_lib:prepare_test_case([{init_test_case, [mnesia]},
                                           delete_schema],
                                          N, Config, ?FILE, ?LINE)).

all() -> 
    [conversion_from_external_to_disc_copies_results_in_data_loss_after_node_restart].

groups() ->
    [].

init_per_testcase(Func, Conf) ->
    mnesia_test_lib:init_per_testcase(Func, Conf).

end_per_testcase(Func, Conf) ->
    mnesia_test_lib:end_per_testcase(Func, Conf).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

suite() -> [{ct_hooks,[{ts_install_cth,[{nodenames,1}]}]}].

conversion_from_external_to_disc_copies_results_in_data_loss_after_node_restart(Config) when is_list(Config) ->
    Node = node(),
    Data = [
        #some_rec{some_id = a, some_int = 1, some_string = "something" },
        #some_rec{some_id = b, some_int = 2, some_string = "anything"  },
        #some_rec{some_id = c, some_int = 3, some_string = "everything"},
        #some_rec{some_id = d, some_int = 4, some_string = "nothing"   }
    ],

    [Node] = ?acquire(1, Config),
    ok = mnesia:create_schema([Node]),
    ok = mnesia:start(),
    {atomic, ok} = mnesia:add_backend_type(ext_ets, ext_test),
    {atomic, ok} = mnesia:add_backend_type(ext_dets, ext_test),
    {atomic, ok} = mnesia:create_table(table, [
        {type, ordered_set},
        {record_name, some_rec},
        {attributes, record_info(fields, some_rec)},
        {disc_copies, [Node]}
    ]),

    ok = mnesia:activity(transaction, fun() ->
        lists:foreach(fun(Elem) -> mnesia:write(table, Elem, write) end, Data)
    end),

    {atomic, ok} = mnesia:change_table_copy_type(table, Node, ext_ets),
    Data = mnesia:activity(transaction, fun() ->
        mnesia:match_object(table, #some_rec{_ = '_'}, read) end
    ),

    {atomic, ok} = mnesia:change_table_copy_type(table, Node, disc_copies),
    Data = mnesia:activity(transaction, fun() ->
        mnesia:match_object(table, #some_rec{_ = '_'}, read) end
    ),

    stopped = mnesia:stop(),
    ok = mnesia:start(),
    ok = mnesia:wait_for_tables([schema, table], 10000),

    Data = mnesia:activity(transaction, fun() ->
        mnesia:match_object(table, #some_rec{_ = '_'}, read) end
    ).
