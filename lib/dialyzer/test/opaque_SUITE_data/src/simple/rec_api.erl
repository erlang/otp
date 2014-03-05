-module(rec_api).

-export([t1/0, t2/0, adt_t1/0, adt_t1/1, adt_r1/0,
         t/1, t_adt/0, r/0, r_adt/0]).

-export_type([{a,0},{r1,0}]).

-export_type([f/0, op_t/0, r/0, tup/0]).

-opaque a() :: a | b.

-record(r1,
        {f1 :: a()}).

-opaque r1() :: #r1{}.

t1() ->
    A = #r1{f1 = a},
    {r1, a} = A.

t2() ->
    A = {r1, 10}, % violates the type of #r1{}
    {r1, 10} = A. % violates the type of #r1{}

adt_t1() ->
    R = rec_adt:r1(),
    {r1, a} = R. % breaks the opaqueness

-spec adt_t1(rec_adt:r1()) -> rec_adt:r1(). % invalid type spec

adt_t1(R) ->
    {r1, a} = R.

-spec adt_r1() -> rec_adt:r1(). % invalid type spec

adt_r1() ->
    #r1{f1 = a}.

-opaque f() :: fun((_) -> _).

-opaque op_t() :: integer().

-spec t(f()) -> _.

t(A) ->
    T = term(),
    %% 3(T), % cannot test this: dialyzer_dep deliberately crashes
    A(T).

-spec term() -> op_t().

term() ->
    3.

t_adt() ->
    A = rec_adt:f(),
    T = term(),
    A(T).

-record(r, {f = fun(_) -> 3 end :: f(), o = 1 :: op_t()}).

-opaque r() :: #r{}.

-opaque tup() :: {'r', f(), op_t()}.

-spec r() -> _.

r() ->
    {r, f(), 2}. % OK, f() is a local opaque type

-spec f() -> f().

f() ->
    fun(_) -> 3 end.

r_adt() ->
    {r, rec_adt:f(), 2}. % breaks the opaqueness
