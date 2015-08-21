-module(keydel).

-export([store/3]).

-record(att, {f}).

-type attachment() :: list().

-opaque att() :: #att{} | attachment().

-spec store(atom(), any(), att()) -> att().
store(Field, undefined, Att) when is_list(Att) ->
    lists:keydelete(Field, 1, Att);
store(Field, Value, Att) when is_list(Att) ->
    lists:keystore(Field, 1, Att, {Field, Value});
store(Field, Value, Att) ->
    store(Field, Value, upgrade(Att)).


-spec upgrade(#att{}) -> attachment().
upgrade(#att{} = Att) ->
    Map = lists:zip(
            record_info(fields, att),
            lists:seq(2, record_info(size, att))
           ),
    %% Don't store undefined elements since that is default
    [{F, element(I, Att)} || {F, I} <- Map, element(I, Att) /= undefined];
upgrade(Att) ->
    Att.
