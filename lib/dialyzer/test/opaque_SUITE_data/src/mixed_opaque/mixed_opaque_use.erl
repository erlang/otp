%%---------------------------------------------------------------------------
%% Test that tries some combinations of using more than one opaque data type
%% in the same function(s).
%%----------------------------------------------------------------------------
-module(mixed_opaque_use).

-export([ok1/1, ok2/0, wrong1/0]).

-define(REC, mixed_opaque_rec_adt).
-define(QUEUE, mixed_opaque_queue_adt).

%% Currently returning unions of opaque types is considered OK
ok1(Type) ->
    case Type of
	queue -> ?QUEUE:new();
	rec -> ?REC:new()
    end.

%% Constructing a queue of records is OK
ok2() ->
    Q0 = ?QUEUE:new(),
    R0 = ?REC:new(),
    Q1 = ?QUEUE:add(R0, Q0),
    {R1,_Q2} = ?QUEUE:dequeue(Q1),
    ?REC:get_a(R1).

%% But of course calling a function expecting some opaque type
%% with some other opaque typs is not OK
wrong1() ->
    Q = ?QUEUE:new(),
    ?REC:get_a(Q).
