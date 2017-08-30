-module(fun2ms).
-export([return/0]).
-include_lib("stdlib/include/ms_transform.hrl").

-record(snapshot, {id :: integer(), arg1 :: atom(), arg2 :: tuple()}).

return() ->
    TableId = ets:new(table, [public, {keypos, #snapshot.id}]),

    ets:insert(TableId, [#snapshot{id = 1, arg1 = hard, arg2 = {1,2}},
                         #snapshot{id = 2, arg1 = rock, arg2 = {1,2}},
                         #snapshot{id = 3, arg1 = hallelujah, arg2 =
                                       {1,2}}]),


    Example = ets:fun2ms(
                fun(#snapshot{id = Arg1, arg1 = Arg2}) ->
                        {Arg1, Arg2}
                end),

    ets:select(TableId, Example).
