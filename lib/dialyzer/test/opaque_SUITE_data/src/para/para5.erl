-module(para5).

-export([d/0, dd/0, da1/0]).

d() ->
    I1 = adt_d1(),
    I2 = adt_d2(),
    I1 =:= I2. % can never evaluate to true

dd() ->
    I1 = adt_d1(),
    I2 = adt_dd(),
    I1 =/= I2. % incompatible opaque types

da1() ->
    I1 = adt_da1(),
    I2 = adt_da2(),
    I1 =:= I2.

adt_d1() ->
    para5_adt:d1().

adt_d2() ->
    para5_adt:d2().

adt_dd() ->
    para5_adt:dd().

adt_da1() ->
    para5_adt:da1().

adt_da2() ->
    para5_adt:da2().
