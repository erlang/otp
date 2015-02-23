%% The Great Computer Language Shootout
%% http://shootout.alioth.debian.org/

%% contributed by Alexey Shchepin <alexey@sevcom.net>
%% modified by Isaac Gouy

% module "lists" already exists in stdlib
-module(lists_test).
-export([main/0, main/1]).
-export([small/0,medium/0,big/0]).

small() -> 10.
medium() -> 16.
big() -> 20.

-define(SIZE, 100000).

main() -> main(["1"]).
main(Arg) ->
   N = Arg,
   test_lists_n(N),
   exit(ok).

test_lists_n(1) ->
   io:format("~p~n", [test_lists()]);
test_lists_n(N) ->
   test_lists(),
   test_lists_n(N-1).

test_lists() ->
   erase(),
   L1 = create_seq(),
   L2 = copy(L1),
   {L22, L3} = move1(L2, new()),
   {L33, L222} = move2(L3, L22),
   L11 = reverse(L1),
   ?SIZE = head(L11),
   test_eq(L11, L222),
   len(L11).

move1(From, To) ->
   case move_from_begin_to_end(From, To) of
      {NewFrom, NewTo} ->
         move1(NewFrom, NewTo);
      nil ->
         {From, To}
   end.

move2(From, To) ->
   case move_from_end_to_end(From, To) of
      {NewFrom, NewTo} ->
         move2(NewFrom, NewTo);
      nil ->
         {From, To}
   end.

% Implementation of doubly-linked lists

new() ->
   {nil, nil}.

new_el(Data) ->
   Ref = make_ref(),
   put(Ref, Data),
   Ref.

push_end({nil, nil}, Val) ->
   Data = {Val, nil, nil},
   NewEl = new_el(Data),
   {NewEl, NewEl};
push_end({First, Last}, Val) ->
   Data = {Val, Last, nil},
   NewEl = new_el(Data),
   put(Last, setelement(3, get(Last), NewEl)),
   {First, NewEl}.

head({First, _Last}) ->
   element(1, get(First)).

create_seq() ->
   create_seq(1, new()).
create_seq(N, List) ->
   if
      N =< ?SIZE ->
         create_seq(N+1, push_end(List, N));
      true ->
         List
   end.

copy({First, _Last}) ->
   copy1(First, new()).

copy1(nil, List) ->
   List;
copy1(El, List) ->
   {Val, _Prev, Next} = get(El),
   copy1(Next, push_end(List, Val)).

reverse({First, Last}) ->
   reverse_els(First),
   {Last, First}.

reverse_els(nil) ->
   ok;
reverse_els(El) ->
   {Val, Prev, Next} = get(El),
   put(El, {Val, Next, Prev}),
   reverse_els(Next).

move_from_begin_to_end({First1, Last1}, {First2, Last2}) ->
   if
      First1 == nil ->
         nil;
      true ->
         {Val, _Prev1, Next1} = get(First1),
         NewList1 = if
            Next1 == nil ->
               {nil, nil};
            true ->
               {Next1, Last1}
         end,
         NewList2 = if
            First2 == nil ->
               Data = {Val, nil, nil},
               put(First1, Data),
               {First1, First1};
            true ->
               Data = {Val, Last2, nil},
               put(First1, Data),
               put(Last2, setelement(3, get(Last2), First1)),
               {First2, First1}
         end,
      {NewList1, NewList2}
   end.

move_from_end_to_end({First1, Last1}, {First2, Last2}) ->
   if
      First1 == nil ->
         nil;
      true ->
         {Val, Prev1, _Next1} = get(Last1),
         NewList1 = if
            Prev1 == nil ->
               {nil, nil};
            true ->
               {First1, Prev1}
         end,
         NewList2 = if
            First2 == nil ->
               Data = {Val, nil, nil},
               put(Last1, Data),
               {Last1, Last1};
            true ->
               Data = {Val, Last2, nil},
               put(Last1, Data),
               put(Last2, setelement(3, get(Last2), Last1)),
               {First2, Last1}
         end,
      {NewList1, NewList2}
   end.

test_eq({First1, Last1}, {First2, Last2}) ->
   test_eq1(First1, First2).

test_eq1(nil, nil) ->
   ok;
test_eq1(El1, El2) ->
   {Val, _Prev1, Next1} = get(El1),
   {Val, _Prev2, Next2} = get(El2),
   test_eq1(Next1, Next2).

len({First, _Last}) ->
   len(First, 0).

len(nil, N) ->
   N;
len(El, N) ->
   len(element(3, get(El)), N + 1).

