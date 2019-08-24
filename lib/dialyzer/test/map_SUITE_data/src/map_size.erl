-module(map_size).

-export([t1/0, e1/0, t2/0, t3/0, t4/0, t5/1, t6/1, t7/1]).

t1() ->
    0 = maps:size(#{}),
    1 = maps:size(#{}).

e1() ->
    0 = map_size(#{}),
    1 = map_size(#{}).

t2() -> p(#{a=>x}).

p(M) when map_size(M) =:= 0 -> ok.

t3() ->
    1 = map_size(cio()),
    2 = map_size(cio()),
    3 = map_size(cio()),
    4 = map_size(cio()).

t4() ->
    0 = map_size(cio()).

t5(M) when map_size(M) =:= 0 ->
    #{a := _} = M. %% Only t5 has no local return; want better message

t6(M) when map_size(M) =:= 0 ->
    #{} = M.

t7(M=#{a := _}) when map_size(M) =:= 1 ->
    #{b := _} = M. %% We should warn here too

-spec cio() -> #{3 := ok, 9 => _, 11 => x}.
cio() -> binary_to_term(<<131,116,0,0,0,2,97,3,100,0,2,111,107,97,9,97,6>>).
