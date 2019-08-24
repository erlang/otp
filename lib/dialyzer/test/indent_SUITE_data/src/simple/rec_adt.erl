-module(rec_adt).

-export([f/0, r1/0]).

-export_type([r1/0]).

-export_type([f/0, op_t/0, a/0]).

-opaque a() :: a | b.

-record(r1,
        {f1 :: a()}).

-opaque r1() :: #r1{}.

-opaque f() :: fun((_) -> _).

-opaque op_t() :: integer().

-spec f() -> f().

f() ->
    fun(_) -> 3 end.

-spec r1() -> r1().

r1() ->
    #r1{f1 = a}.
