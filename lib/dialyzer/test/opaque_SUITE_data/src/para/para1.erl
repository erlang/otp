-module(para1).

-compile(export_all).

%% Parameterized opaque types

-export_type([t/0, t/1]).

-opaque t() :: {integer(), integer()}.

-opaque t(A) :: {A, A}.

-type y(A) :: {A, A}.

tt1() ->
    I = t1(),
    A = t2(),
    A =:= I. % never 'true'

tt2() ->
    I = t0(),
    A = t2(),
    A =:= I. % never 'true'

tt3() ->
    I1 = t0(),
    I2 = t1(),
    I1 =:= I2. % never true

tt4() ->
    I1 = y1(),
    I2 = y2(),
    I1 =:= I2. % cannot evaluate to true

adt_tt1() ->
    I = adt_t1(),
    A = adt_t2(),
    A =:= I. % opaque attempt

adt_tt2() ->
    I = adt_t0(),
    A = adt_t2(),
    A =:= I. % opaque attempt

adt_tt3() ->
    I1 = adt_t0(),
    I2 = adt_t1(),
    I1 =:= I2. % opaque attempt

adt_tt4() ->
    I1 = adt_y1(),
    I2 = adt_y2(),
    I1 =:= I2. % cannot evaluate to true

-spec t0() -> t().

t0() ->
    {3, 2}.

-spec t1() -> t(integer()).

t1() ->
    {3, 3}.

-spec t2() -> t(atom()).

t2() ->
    {a, b}.

-spec y1() -> y(integer()).

y1() ->
    {3, 2}.

-spec y2() -> y(atom()).

y2() ->
    {a, b}.

adt_t0() ->
    para1_adt:t0().

adt_t1() ->
    para1_adt:t1().

adt_t2() ->
    para1_adt:t2().

adt_y1() ->
    para1_adt:y1().

adt_y2() ->
    para1_adt:y2().
