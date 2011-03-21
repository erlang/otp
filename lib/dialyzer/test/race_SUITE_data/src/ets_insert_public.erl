%% This tests the presence of possible races due to an ets:lookup/ets:insert
%% combination. It takes into account any public ETS tables that might exist.

-module(ets_insert_public).

-export([main/1]).

%% Main
main(Foo) ->
    make_table(Foo),
    ets:insert(Foo, {counter, 0}),
    [{_, N}] = ets:lookup(Foo, counter),
    NewN = N + 1,
    ets:insert(Foo, {counter, NewN}),
    NewN.

make_table(Foo) ->
    init(Foo).

init(Foo) ->
    ets:new(Foo, [named_table, public]),
    ets:insert(Foo, {counter, 0}),
    {ok, feeling_good}.
