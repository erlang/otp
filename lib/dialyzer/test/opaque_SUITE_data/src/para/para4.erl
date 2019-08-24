-module(para4).

-compile(export_all).

-export_type([d_atom/0, d_integer/0, d_tuple/0, d_all/0]).

-export_type([t/1]).

-type ai() :: atom() | integer().

-type d(T) :: dict:dict(T, T).

-opaque d_atom() :: d(atom()).
-opaque d_integer() :: d(integer()).
-opaque d_tuple() :: d(tuple()).
-opaque d_all() :: d(ai()).

b(D) ->
    a(D) ++ i(D).

-spec a(d_atom()) -> [{atom(), atom()}]. % Invalid type spec

a(D) ->
    c(D).

-spec i(d_integer()) -> [{integer(), integer()}]. % Invalid type spec

i(D) ->
    c(D).

-spec t(d_tuple()) -> [{tuple(), tuple()}]. % Invalid type spec.

t(D) ->
    c(D).

-spec c(d_all()) -> [{ai(), ai()}].

c(D) ->
    dict:to_list(D).




-opaque t(A) :: {A, A}.

adt_tt5() ->
    I1 = adt_y1(),
    I2 = adt_y3(),
    I1 =:= I2.

adt_tt6() ->
    I1 = adt_y2(),
    I2 = adt_y3(),
    I1 =:= I2.

adt_tt7() ->
    I1 = adt_t1(),
    I2 = adt_t3(),
    I1 =:= I2. % opaque attempt

adt_tt8() ->
    I1 = adt_t2(),
    I2 = adt_t3(),
    I1 =:= I2. % opaque attempt

adt_tt9() ->
    I1 = adt_int2(),
    I2 = adt_int4(),
    I1 =:= I2. % opaque attempt

adt_tt10() ->
    I1 = adt_int2(),
    I2 = adt_int2_4(),
    I1 =:= I2. % opaque attempt

adt_tt11() ->
    I1 = adt_int5_7(),
    I2 = adt_int2_4(),
    I1 =:= I2. % opaque attempt

adt_tt12() ->
    I1 = adt_un1_2(),
    I2 = adt_un3_4(),
    I1 =:= I2. % opaque attempt

adt_tt13() ->
    I1 = adt_tup(),
    I2 = adt_tup2(),
    I1 =:= I2. % opaque attempt

adt_tt14() ->
    I1 = adt_map(),
    I2 = adt_map2(),
    I1 =:= I2.

y3() ->
    {a, 3}.

adt_t1() ->
    para4_adt:t1().

adt_t2() ->
    para4_adt:t2().

adt_t3() ->
    para4_adt:t3().

adt_y1() ->
    para4_adt:y1().

adt_y2() ->
    para4_adt:y2().

adt_y3() ->
    para4_adt:y3().

adt_int2() ->
    para4_adt:int2().

adt_int4() ->
    para4_adt:int4().

adt_int2_4() ->
    para4_adt:int2_4().

adt_int5_7() ->
    para4_adt:int5_7().

adt_un1_2() ->
    para4_adt:un1_2().

adt_un3_4() ->
    para4_adt:un3_4().

adt_tup() ->
    para4_adt:tup().

adt_tup2() ->
    para4_adt:tup2().

adt_map() ->
    para4_adt:map().

adt_map2() ->
    para4_adt:map2().
