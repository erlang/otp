-module(simple1_adt).

-export([d1/0, d2/0, i/0, n1/0, n2/0, o1/0, o2/0,
         c1/0, c2/0, bit1/0, a/0, i1/0, tuple/0,
         b1/0, b2/0, ty_i1/0]).

-export_type([o1/0, o2/0, d1/0, d2/0]).

-export_type([i1/0, i2/0, di1/0, di2/0]).

-export_type([ty_i1/0, c1/0, c2/0]).

-export_type([b1/0, b2/0]).

-export_type([bit1/0]).

-export_type([tuple1/0, a/0, i/0]).

%% Equal:

-opaque o1() :: a | b | c.

-opaque o2() :: a | b | c.

%% Disjoint:

-opaque d1() :: a | b | c.

-opaque d2() :: d | e | f.

%% One common element:

-opaque c1() :: a | b | c.

-opaque c2() :: c | e | f.

%% Equal integer range:

-opaque i1() :: 1 | 2.

-opaque i2() :: 1 | 2.

%% Disjoint integer range:

-opaque di1() :: 1 | 2.

-opaque di2() :: 3 | 4.


-type ty_i1() :: 1 | 2.

%% Boolean types

-opaque b1() :: boolean().

-opaque b2() :: boolean().

%% Binary types

-opaque bit1() :: binary().

%% Tuple types

-opaque tuple1() :: tuple().

%% Atom type

-opaque a() :: atom().

-opaque i() :: integer().

-spec d1() -> d1().

d1() -> a.

-spec d2() -> d2().

d2() -> d.

-spec i() -> i().

i() ->
    1.

-spec n1() -> o1().

n1() -> a.

-spec n2() -> o2().

n2() -> a.

-spec o1() -> o1().

o1() -> a.

-spec o2() -> o2().

o2() -> a.

-spec c1() -> c1().

c1() -> a.

-spec c2() -> c2().

c2() -> e.

-spec bit1() -> bit1().

bit1() ->
    <<"hej">>.

-spec a() -> a().

a() ->
    e.

-spec i1() -> i1().

i1() -> 1.

-spec tuple() -> tuple1().

tuple() -> {1,2}.

-spec b1() -> b1().

b1() -> true.

-spec b2() -> b2().

b2() -> false.

-spec ty_i1() -> ty_i1().

ty_i1() ->
    1.
