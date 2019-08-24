-module(para1_adt).

-export([t0/0, t1/0, t2/0, y1/0, y2/0]).

-export_type([t/0, t/1, y/1]).

-opaque t() :: {integer(), integer()}.

-opaque t(A) :: {A, A}.

-type y(A) :: {A, A}.

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
