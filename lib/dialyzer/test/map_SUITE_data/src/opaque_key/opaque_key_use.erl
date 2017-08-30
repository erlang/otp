-module(opaque_key_use).

-compile(export_all).

-export_type([t/0, t/1]).

-opaque t()  :: #{atom() => integer()}.
-opaque t(A) :: #{A => integer()}.

tt1() ->
    A = t0(),
    B = t1(),
    A =:= B. % never 'true'

-spec t0() -> t().
t0() -> #{a => 1}.

-spec t1() -> t(integer()).
t1() -> #{3 => 1}.

adt_tt1() ->
    A = adt_t0(),
    B = adt_t1(),
    A =:= B. % opaque attempt

adt_tt2() ->
    A = adt_t0(),
    B = adt_t1(),
    #{A => 1 % opaque key
     ,B => 2 % opaque key
     }.

adt_tt3() ->
    A = map_adt:t0(),
    #{A => 1}. % opaque key

adt_mm1() ->
    A = adt_t0(),
    M = adt_m0(),
    #{A := R} = M, % opaque attempt
    R.

%% adt_ms1() ->
%%     A = adt_t0(),
%%     M = adt_m0(),
%%     M#{A}. % opaque arg

adt_mu1() ->
    A = adt_t0(),
    M = adt_m0(),
    M#{A := 4}. % opaque arg

adt_mu2() ->
    A = adt_t0(),
    M = adt_m0(),
    M#{A => 4}. % opaque arg

adt_mu3() ->
    M = adt_m0(),
    M#{}. % opaque arg

adt_mtm1() ->
    A = adt_t0(),
    M = adt_mt0(),
    #{A := R} = M, % opaque key
    R.

%% adt_mts1() ->
%%     A = adt_t0(),
%%     M = adt_mt0(),
%%     M#{A}. % opaque key

adt_mtu1() ->
    A = adt_t0(),
    M = adt_mt0(),
    M#{A := 4}. % opaque key

adt_mtu2() ->
    A = adt_t0(),
    M = adt_mt0(),
    M#{A => 4}. % opaque key

adt_mtu3() ->
    M = adt_mt0(),
    M#{}. % Ok to not warn

adt_t0() ->
    opaque_key_adt:t0().

adt_t1() ->
    opaque_key_adt:t1().

adt_m0() ->
    opaque_key_adt:m0().

adt_mt0() ->
    opaque_key_adt:mt0().
