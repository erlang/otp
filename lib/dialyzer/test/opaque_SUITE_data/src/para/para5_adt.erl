-module(para5_adt).

-export([d1/0, d2/0, dd/0, da1/0, da2/0]).

-export_type([d/0, dd/1, da/2]).

-opaque d() :: 1 | 2.

-spec d1() -> d().

d1() ->
    1.

-spec d2() -> d().

d2() ->
    2.

-opaque dd(A) :: A.

-spec dd() -> dd(atom()).

dd() ->
    foo:atom().

-opaque da(A, B) :: {A, B}.

-spec da1() -> da(any(), atom()).

da1() ->
    {3, a}.

-spec da2() -> da(integer(), any()).

da2() ->
    {3, a}.
