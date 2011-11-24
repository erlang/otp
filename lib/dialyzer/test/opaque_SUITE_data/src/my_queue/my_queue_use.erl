-module(my_queue_use).

-export([ok1/0, ok2/0, wrong1/0, wrong2/0, wrong3/0, wrong4/0, wrong5/0]).

ok1() ->
    my_queue_adt:is_empty(my_queue_adt:new()).

ok2() ->
    Q0 = my_queue_adt:new(),
    Q1 = my_queue_adt:add(42, Q0),
    {42, Q2} = my_queue_adt:dequeue(Q1),
    my_queue_adt:is_empty(Q2).

wrong1() ->
    my_queue_adt:is_empty([]).

wrong2() ->
    Q0 = [],
    my_queue_adt:add(42, Q0).

wrong3() ->
    Q0 = my_queue_adt:new(),
    Q1 = my_queue_adt:add(42, Q0),
    [42|Q2] = Q1,
    Q2.

wrong4() ->
    Q0 = my_queue_adt:new(),
    Q1 = my_queue_adt:add(42, Q0),
    Q1 =:= [].

wrong5() ->
    Q0 = my_queue_adt:new(),
    {42, Q2} = my_queue_adt:dequeue([42|Q0]),
    Q2.
