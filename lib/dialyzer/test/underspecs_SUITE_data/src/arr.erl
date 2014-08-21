-module(arr).

%% http://erlang.org/pipermail/erlang-questions/2014-August/080445.html

-define(A, array).

-export([test/3, test2/3, test3/3, test4/3, test5/3, test6/3]).

-spec test(?A:array(T), non_neg_integer(), T) -> ?A:array(T).

test(Array, N, Value) ->
    ?A:set(N, Value, Array).

-spec test2(?A:array(T), non_neg_integer(), T) -> ?A:array(T).

test2(Array, N, Value) when N > 0 ->
    ?A:set(N, Value, Array).

-spec test3(?A:array(T), non_neg_integer(), _) -> ?A:array(T).

test3(Array, N, Value) ->
    ?A:set(N, Value, Array).

-spec test4(?A:array(T), non_neg_integer(), _) -> ?A:array(T).

test4(Array, N, Value) when N > 0 ->
    ?A:set(N, Value, Array).

-spec test5(?A:array(T), non_neg_integer(), T) -> ?A:array(T).

test5(Array, N, Value) when is_integer(Value) ->
    ?A:set(N, Value, Array).

%% One would ideally want a warning also for test6(), but the current
%% analysis of parametrized opaque types is not strong enough to
%% discover this.
-spec test6(?A:array(integer()), non_neg_integer(), integer()) ->
                   ?A:array(any()).

test6(Array, N, Value) ->
     ?A:set(N, Value, Array).
