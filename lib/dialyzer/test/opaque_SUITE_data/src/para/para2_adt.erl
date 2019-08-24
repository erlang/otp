-module(para2_adt).

%% More parameterized opaque types

-export_type([c1/0, c2/0]).

-export_type([ct1/0, ct2/0]).

-export_type([circ/1, circ/2]).

-export_type([un/2]).

-export([c1/0, c2/0, ct1/0, ct2/0, circ1/0, circ2/0, u1/0, u2/0]).

-opaque c1() :: c2().
-opaque c2() :: c1().

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

-opaque circ(A) :: circ(A, A).
-opaque circ(A, B) :: circ({A, B}).

-spec circ1() -> circ(integer()).

circ1() ->
    3.

-spec circ2() -> circ(integer(), integer()).

circ2() ->
    {3, 3}.

-opaque un(A, B) :: A | B.

-spec u1() -> un(integer(), atom()).

u1() ->
    3.

-spec u2() -> un(atom(), integer()).

u2() ->
    3.
