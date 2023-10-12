-module(para4_adt).

-export([t1/0, t2/0, t3/0, y1/0, y2/0, y3/0]).

-export([int2/0, int4/0, int2_4/0, int5_7/0]).

-export([un1_2/0, un3_4/0]).

-export([tup/0, tup2/0]).

-export([map/0, map2/0]).

-export_type([t/1, y/1, int/1, tup/1, un/1]).

-type ai() :: atom() | integer().

-opaque t(A) :: {A, A}.

-type y(A) :: {A, A}.

-opaque int(I) :: I.

-opaque un(I) :: atom() | I.

-opaque tup(T) :: T.

-spec t1() -> t(integer()).

t1() ->
    {i(), i()}.

-spec t2() -> t(atom()).

t2() ->
    {a(), a()}.

-spec t3() -> t(ai()).

t3() ->
    {ai(), ai()}.

-spec y1() -> y(integer()).

y1() ->
    {i(), i()}.

-spec y2() -> y(atom()).

y2() ->
    {a(), a()}.

-spec y3() -> y(ai()).

y3() ->
    {ai(), ai()}.

-spec a() -> atom().

a() ->
    foo:a().

-spec i() -> integer().

i() ->
    foo:i().

-spec ai() -> ai().

ai() ->
    foo:ai().

-spec int2() -> int(1..2).

int2() ->
    foo:int2().

-spec int4() -> int(1..4).

int4() ->
    foo:int4().

-spec int2_4() -> int(2..4).

int2_4() ->
    foo:int2_4().

-spec int5_7() -> int(5..7).

int5_7() ->
    foo:int5_7().

-spec un1_2() -> un(1..2).

un1_2() ->
    foo:un1_2().

-spec un3_4() -> un(3..4).

un3_4() ->
    foo:un3_4().

-spec tup() -> tup(tuple()).

tup() ->
    foo:tup().

-spec tup2() -> tup({_, _}).

tup2() ->
    foo:tup2().

-spec map() -> t(#{2 => b}).

map() ->
    foo:map().

-spec map2() -> t(#{1 => a}).

map2() ->
    foo:map2().
