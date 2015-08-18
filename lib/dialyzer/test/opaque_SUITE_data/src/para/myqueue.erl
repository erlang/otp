-module(myqueue).

-export([new/0, in/2]).

-record(myqueue, {queue = queue:new() :: queue:queue({integer(), _})}).

-opaque myqueue(Item) :: #myqueue{queue :: queue:queue({integer(), Item})}.

-export_type([myqueue/1]).

-spec new() -> myqueue(_).
new() ->
    #myqueue{queue=queue:new()}.

-spec in(Item, myqueue(Item)) -> myqueue(Item).
in(Item, #myqueue{queue=Q}) ->
    #myqueue{queue=queue:in({1, Item}, Q)}.
