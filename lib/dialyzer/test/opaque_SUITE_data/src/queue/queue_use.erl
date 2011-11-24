-module(queue_use).

-export([ok1/0, ok2/0]).
-export([wrong1/0, wrong2/0, wrong3/0, wrong4/0, wrong5/0, wrong6/0, wrong7/0, wrong8/0]).

ok1() ->
    queue:is_empty(queue:new()).

ok2() ->
    Q0 = queue:new(),
    Q1 = queue:in(42, Q0),
    {{value, 42}, Q2} = queue:out(Q1),
    queue:is_empty(Q2).

%%--------------------------------------------------

wrong1() ->
    queue:is_empty({[],[]}).

wrong2() ->
    Q0 = {[],[]},
    queue:in(42, Q0).

wrong3() ->
    Q0 = queue:new(),
    Q1 = queue:in(42, Q0),
    {[42],Q2} = Q1,
    Q2.

wrong4() ->
    Q0 = queue:new(),
    Q1 = queue:in(42, Q0),
    Q1 =:= {[42],[]}.

wrong5() ->
    {F, _R} = queue:new(),
    F.

wrong6() ->
    {{value, 42}, Q2} = queue:out({[42],[]}),
    Q2.

%%--------------------------------------------------

-record(db, {p, q}).

wrong7() ->
    add_unique(42, #db{p = [], q = queue:new()}).

add_unique(E, DB) ->
    case is_in_queue(E, DB) of
	true -> DB;
	false -> DB#db{q = queue:in(E, DB#db.q)}
    end.

is_in_queue(P, #db{q = {L1,L2}}) ->
    lists:member(P, L1) orelse lists:member(P, L2).

%%--------------------------------------------------

wrong8() ->
    tuple_queue({42, gazonk}).

tuple_queue({F, Q}) ->
    queue:in(F, Q).
