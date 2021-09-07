-module(binary_nonempty).

-export([t1/0, t2/0, t3/0, t4/0, t5/1, t6/1, t7/1]).

t1() ->
    t1(<<>>).

-spec t1(nonempty_bitstring()) -> foo.
t1(_B) ->
    foo.

t2() ->
    t2(<<>>).

-spec t2(nonempty_binary()) -> foo.
t2(_B) ->
    foo.

t3() ->
    t3(<<>>).

-spec t3(<<_:1, _:_*1>>) -> foo.
t3(_B) ->
    foo.

t4() ->
    t4(<<>>).

-spec t4(<<_:8, _:_*8>>) -> foo.
t4(_B) ->
    foo.

-spec t5(nonempty_binary()) -> foo.
t5(B) ->
    <<>> = B,
    foo.

-spec t6(<<>>) -> foo.
t6(B) ->
    B = <<"f">>, % The type is <<_:8>>.
    foo.

-spec t7(<<>>) -> foo.
t7(B) ->
    B = <<1:1>>, % The type is <<_:1>>.
    foo.
