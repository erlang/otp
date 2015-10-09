-module(para2).

-compile(export_all).

%% More parameterized opaque types

-export_type([strange/1]).

-export_type([c1/0, c2/0]).

-export_type([circ/1, circ/2]).

-opaque strange(A) :: {B, B, A}.

-spec t(strange(integer())) -> strange(atom()).

t({3, 4, 5}) ->
    {a, b, c}.

-opaque c1() :: c2().
-opaque c2() :: c1().

c() ->
    A = c1(),
    B = c2(),
    A =:= B.

t() ->
    A = ct1(),
    B = ct2(),
    A =:= B. % can never evaluate to 'true'

-spec c1() -> c1().

c1() ->
    a.

-spec c2() -> c2().

c2() ->
    a.

-type ct1() :: ct2().
-type ct2() :: ct1().

-spec ct1() -> ct1().

ct1() ->
    a.

-spec ct2() -> ct2().

ct2() ->
    b.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

c_adt() ->
    A = c1_adt(),
    B = c2_adt(),
    A =:= B. % opaque attempt

t_adt() ->
    A = ct1_adt(),
    B = ct2_adt(),
    A =:= B. % can never evaluate to true

c1_adt() ->
    para2_adt:c1().

c2_adt() ->
    para2_adt:c2().

ct1_adt() ->
    para2_adt:ct1().

ct2_adt() ->
    para2_adt:ct2().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-opaque circ(A) :: circ(A, A).
-opaque circ(A, B) :: circ({A, B}).

tcirc() ->
    A = circ1(),
    B = circ2(),
    A =:= B. % can never evaluate to 'true'

-spec circ1() -> circ(integer()).

circ1() ->
    3.

-spec circ2() -> circ(integer(), integer()).

circ2() ->
    {3, 3}.

tcirc_adt() ->
    A = circ1_adt(),
    B = circ2_adt(),
    A =:= B. % opaque attempt (number of parameters differs)

circ1_adt() ->
    para2_adt:circ1().

circ2_adt() ->
    para2_adt:circ2().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

u_adt() ->
    A = u1_adt(),
    B = u2_adt(),
    %% The resulting types are equal, but not the parameters:
    A =:= B. % opaque attempt

u1_adt() ->
    para2_adt:u1().

u2_adt() ->
    para2_adt:u2().
