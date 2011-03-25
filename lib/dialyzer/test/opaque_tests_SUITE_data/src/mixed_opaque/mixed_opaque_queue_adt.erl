%%---------------------------------------------------------------------------
%% A clone of 'queue_adt' so as to test its combination with 'rec_adt'
%%---------------------------------------------------------------------------
-module(mixed_opaque_queue_adt).

-export([new/0, add/2, dequeue/1, is_empty/1]).

-opaque my_queue() :: list().

-spec new() -> my_queue().
new() ->
    [].

-spec add(term(), my_queue()) -> my_queue().
add(E, Q) ->
    Q ++ [E].

-spec dequeue(my_queue()) -> {term(), my_queue()}.
dequeue([H|T]) ->
    {H, T}.

-spec is_empty(my_queue()) -> boolean().
is_empty([]) ->
    true;
is_empty([_|_]) ->
    false.
