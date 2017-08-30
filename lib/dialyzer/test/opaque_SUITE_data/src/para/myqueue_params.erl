-module(myqueue_params).

-export([new/0, in/2]).

-record(myqueue_params, {myqueue = myqueue:new() :: myqueue:myqueue(integer())}).

-type myqueue_params() :: #myqueue_params{myqueue ::
                                          myqueue:myqueue(integer())}.
-spec new() -> myqueue_params().
new() ->
    #myqueue_params{myqueue=myqueue:new()}.

-spec in(integer(), myqueue_params()) -> myqueue_params().
in(Item, #myqueue_params{myqueue=Q} = P) when is_integer(Item) ->
    P#myqueue_params{myqueue=myqueue:in(Item, Q)}.
