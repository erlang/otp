-module(db1).
-vsn("1.0").

-export([a/0]).

a() ->
    lists:non_existing_func("dummy_list"),
    b().

b() ->
    fe2:non_existing_func(),
    db2:non_existing_func(),
    fe1:a().
