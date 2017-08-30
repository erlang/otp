-module(is_rec).

-export([ri1/0, ri11/0, ri13/0, ri14/0, ri2/0, ri3/0, ri4/0, ri5/0,
         ri6/0, ri7/0, ri8/0]).

-record(r, {f1 :: integer()}).

ri1() ->
    A = simple1_adt:d1(),
    is_record(A, r). % opaque term 1

ri11() ->
    A = simple1_adt:d1(),
    I = '1-3'(),
    is_record(A, r, I). % opaque term 1

ri13() ->
    A = simple1_adt:d1(),
    if is_record(A, r) -> true end. % breaks the opacity

ri14() ->
    A = simple1_adt:d1(),
    if is_record({A, 1}, r) -> true end. % breaks the opacity

-type '1-3-t'() :: 1..3.

-spec '1-3'() -> '1-3-t'().

'1-3'() ->
    random:uniform(3).


-spec 'Atom'() -> atom().

'Atom'() ->
    a.

ri2() ->
    A = simple1_adt:d1(),
    R = 'Atom'(),
    is_record(A, R). % opaque term 1

ri3() ->
    A = simple1_adt:d1(),
    is_record(A, A, 1). % opaque term 2

ri4() ->
    A = simple1_adt:d1(),
    is_record(A, hipp:hopp(), 1). % opaque term 1

ri5() ->
    A = simple1_adt:d1(),
    is_record(A, A, hipp:hopp()). % opaque term 2

ri6() ->
    A = simple1_adt:d1(),
    if is_record(A, r) -> true end. % breaks opacity

ri7() ->
    A = simple1_adt:d1(),
    if is_record({r, A}, r) -> true end. % A violates #r{}

ri8() ->
    A = simple1_adt:d1(),
    is_record({A, 1}, r). % opaque term 1
