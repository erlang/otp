-module(opaque_key_adt).

-compile(export_all).

-export_type([t/0, t/1, m/0, s/1, sm/1]).

-opaque t()    :: #{atom() => integer()}.
-opaque t(A)   :: #{A => integer()}.

-opaque m()    :: #{t() => integer()}.
-type   mt()   :: #{t() => integer()}.

-opaque s(K)   :: #{K => integer(), integer() => atom()}.
-opaque sm(K)  :: #{K := integer(), integer() := atom()}.
-type   smt(K) :: #{K := integer(), integer() := atom()}.

-spec t0() -> t().
t0() -> #{}.

-spec t1() -> t(integer()).
t1() -> #{3 => 1}.

-spec m0() -> m().
m0() -> #{#{} => 3}.

-spec mt0() -> mt().
mt0() -> #{#{} => 3}.

-spec s0() -> s(atom()).
s0() -> #{}.

-spec s1() -> s(atom()).
s1() -> #{3 => a}.

-spec s2() -> s(atom() | 3).
s2() -> #{3 => a}. %% Contract breakage (not found)

-spec s3() -> s(atom() | 3).
s3() -> #{3 => 5, a => 6, 7 => 8}.

-spec s4() -> s(integer()).
s4() -> #{1 => a}. %% Contract breakage

-spec s5() -> s(1).
s5() -> #{2 => 3}. %% Contract breakage

-spec s6() -> s(1).
s6() -> #{1 => 3}.

-spec s7() -> s(integer()).
s7() -> #{1 => 3}.

-spec sm1() -> sm(1).
sm1() -> #{1 => 2, 3 => a}.

-spec smt1() -> smt(1).
smt1() -> #{3 => a}. %% Contract breakage

-spec smt2() -> smt(1).
smt2() -> #{1 => a}.  %% Contract breakage

-spec smt3() -> smt(q).
smt3() -> #{q => 1}. %% Slight contract breakage (probably requires better map type)

-spec smt4() -> smt(q).
smt4() -> #{q => 2, 3 => a}.

-spec smt5() -> smt(1).
smt5() -> #{1 => 2, 3 => a}.
