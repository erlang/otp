%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%
%%
-module(queue).
-moduledoc """
Abstract data type for FIFO queues.

This module provides (double-ended) FIFO queues in an efficient manner.

All functions fail with reason `badarg` if arguments are of wrong type, for
example, queue arguments are not queues, indexes are not integers, and list
arguments are not lists. Improper lists cause internal crashes. An index out of
range for a queue also causes a failure with reason `badarg`.

Some functions, where noted, fail with reason `empty` for an empty queue.

The data representing a queue as used by this module is to be regarded as opaque
by other modules. In abstract terms, the representation is a composite type of
existing Erlang terms. See note on
[data types](`e:system:data_types.md#no_user_types`). Any code assuming
knowledge of the format is running on thin ice.

All operations have an amortized O(1) running time, except `all/2`, `any/2`,
`delete/2`, `delete_r/2`, `delete_with/2`, `delete_with_r/2`, `filter/2`,
`filtermap/2`, `fold/3`, `join/2`, `len/1`, `member/2`, `split/2` that have
O(n). To minimize the size of a queue minimizing the amount of garbage built by
queue operations, the queues do not contain explicit length information, and
that is why [`len/1`](`len/1`) is O(n). If better performance for this
particular operation is essential, it is easy for the caller to keep track of
the length.

Queues are double-ended. The mental picture of a queue is a line of people
(items) waiting for their turn. The queue front is the end with the item that
has waited the longest. The queue rear is the end an item enters when it starts
to wait. If instead using the mental picture of a list, the front is called head
and the rear is called tail.

Entering at the front and exiting at the rear are reverse operations on the
queue.

This module has three sets of interface functions: the _"Original API"_, the
_"Extended API"_, and the _"Okasaki API"_.

The "Original API" and the "Extended API" both use the mental picture of a
waiting line of items. Both have reverse operations suffixed "\_r".

The "Original API" item removal functions return compound terms with both the
removed item and the resulting queue. The "Extended API" contains alternative
functions that build less garbage and functions for just inspecting the queue
ends. Also the "Okasaki API" functions build less garbage.

The "Okasaki API" is inspired by "Purely Functional Data Structures" by Chris
Okasaki. It regards queues as lists. This API is by many regarded as strange and
avoidable. For example, many reverse operations have lexically reversed names,
some with more readable but perhaps less understandable aliases.
""".
-moduledoc(#{titles =>
                 [{function,<<"Original API">>},
                  {function,<<"Extended API">>},
                  {function,<<"Okasaki API">>}]}).

%% Creation, inspection and conversion
-export([new/0,is_queue/1,is_empty/1,len/1,to_list/1,from_list/1,member/2]).
%% Original style API
-export([in/2,in_r/2,out/1,out_r/1]).
%% Less garbage style API
-export([get/1,get_r/1,peek/1,peek_r/1,drop/1,drop_r/1]).

%% Higher level API
-export([reverse/1,join/2,split/2,filter/2,filtermap/2,fold/3,any/2,all/2,
	 delete/2,delete_r/2,delete_with/2,delete_with_r/2]).

%% Okasaki API from klacke
-export([cons/2,head/1,tail/1,
	 snoc/2,last/1,daeh/1,init/1,liat/1]).

-export_type([queue/0, queue/1]).

%% Mis-spelled, deprecated.
-export([lait/1]).
-deprecated([{lait,1,"use queue:liat/1 instead"}]).

%%--------------------------------------------------------------------------
%% Efficient implementation of double ended fifo queues
%%
%% Queue representation
%%
%% {RearList,FrontList}
%%
%% The first element in the queue is at the head of the FrontList
%% The last element in the queue is at the head of the RearList,
%% that is; the RearList is reversed.
%%

-doc "As returned by `new/0`.".
-opaque queue(Item) :: {list(Item), list(Item)}.

-type queue() :: queue(_).

%% Creation, inspection and conversion

%% O(1)
-doc "Returns an empty queue.".
-doc(#{title => <<"Original API">>}).
-spec new() -> queue(none()).
new() -> {[],[]}. %{RearList,FrontList}

%% O(1)
-doc """
Tests if `Term` is a queue and returns `true` if so, otherwise `false`. Note
that the test will return `true` for a term coinciding with the representation
of a queue, even when not constructed by thus module. See also note on
[data types](`e:system:data_types.md#no_user_types`).
""".
-doc(#{title => <<"Original API">>}).
-spec is_queue(Term :: term()) -> boolean().
is_queue({R,F}) when is_list(R), is_list(F) ->
    true;
is_queue(_) ->
    false.

%% O(1)
-doc "Tests if `Q` is empty and returns `true` if so, otherwise `false`.".
-doc(#{title => <<"Original API">>}).
-spec is_empty(Q :: queue()) -> boolean().
is_empty({[],[]}) ->
    true;
is_empty({In,Out}) when is_list(In), is_list(Out) ->
    false;
is_empty(Q) ->
    erlang:error(badarg, [Q]).

%% O(len(Q))
-doc "Calculates and returns the length of queue `Q`.".
-doc(#{title => <<"Original API">>}).
-spec len(Q :: queue()) -> non_neg_integer().
len({R,F}) when is_list(R), is_list(F) ->
    length(R)+length(F);
len(Q) ->
    erlang:error(badarg, [Q]).

%% O(len(Q))
-doc """
Returns a list of the items in the queue in the same order; the front item of
the queue becomes the head of the list.

_Example:_

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
{[5,4,3],[1,2]}
2> List == queue:to_list(Queue).
true
```
""".
-doc(#{title => <<"Original API">>}).
-spec to_list(Q :: queue(Item)) -> list(Item).
to_list({In,Out}) when is_list(In), is_list(Out) ->
    Out++lists:reverse(In, []);
to_list(Q) ->
    erlang:error(badarg, [Q]).

%% Create queue from list
%%
%% O(length(L))
-doc """
Returns a queue containing the items in `L` in the same order; the head item of
the list becomes the front item of the queue.
""".
-doc(#{title => <<"Original API">>}).
-spec from_list(L :: list(Item)) -> queue(Item).
from_list(L) when is_list(L) ->
    f2r(L);
from_list(L) ->
    erlang:error(badarg, [L]).

%% Return true or false depending on if element is in queue
%% 
%% O(length(Q)) worst case
-doc "Returns `true` if `Item` matches some element in `Q`, otherwise `false`.".
-doc(#{title => <<"Original API">>}).
-spec member(Item, Q :: queue(Item)) -> boolean().
member(X, {R,F}) when is_list(R), is_list(F) ->
    lists:member(X, R) orelse lists:member(X, F);
member(X, Q) ->
    erlang:error(badarg, [X,Q]).

%%--------------------------------------------------------------------------
%% Original style API

%% Append to tail/rear
%% Put at least one element in each list, if it is cheap
%%
%% O(1)
-doc """
Inserts `Item` at the rear of queue `Q1`. Returns the resulting queue `Q2`.

_Example:_

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
{[5,4,3],[1,2]}
2> Queue1 = queue:in(100, Queue).
{[100,5,4,3],[1,2]}
3> queue:to_list(Queue1).
[1,2,3,4,5,100]
```
""".
-doc(#{title => <<"Original API">>}).
-spec in(Item, Q1 :: queue(Item)) -> Q2 :: queue(Item).
in(X, {[_]=In,[]}) ->
    {[X], In};
in(X, {In,Out}) when is_list(In), is_list(Out) ->
    {[X|In],Out};
in(X, Q) ->
    erlang:error(badarg, [X,Q]).

%% Prepend to head/front
%% Put at least one element in each list, if it is cheap
%%
%% O(1)
-doc """
Inserts `Item` at the front of queue `Q1`. Returns the resulting queue `Q2`.

_Example:_

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
{[5,4,3],[1,2]}
2> Queue1 = queue:in_r(100, Queue).
{[5,4,3],[100,1,2]}
3> queue:to_list(Queue1).
[100,1,2,3,4,5]
```
""".
-doc(#{title => <<"Original API">>}).
-spec in_r(Item, Q1 :: queue(Item)) -> Q2 :: queue(Item).
in_r(X, {[],[_]=F}) ->
    {F,[X]};
in_r(X, {R,F}) when is_list(R), is_list(F) ->
    {R,[X|F]};
in_r(X, Q) ->
    erlang:error(badarg, [X,Q]).

%% Take from head/front
%%
%% O(1) amortized, O(len(Q)) worst case
-doc """
Removes the item at the front of queue `Q1`. Returns tuple
`{{value, Item}, Q2}`, where `Item` is the item removed and `Q2` is the
resulting queue. If `Q1` is empty, tuple `{empty, Q1}` is returned.

_Example:_

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
{[5,4,3],[1,2]}
2> {{value, 1=Item}, Queue1} = queue:out(Queue).
{{value,1},{[5,4,3],[2]}}
3> queue:to_list(Queue1).
[2,3,4,5]
```
""".
-doc(#{title => <<"Original API">>}).
-spec out(Q1 :: queue(Item)) ->
                 {{value, Item}, Q2 :: queue(Item)} |
                 {empty, Q1 :: queue(Item)}.
out({[],[]}=Q) ->
    {empty,Q};
out({[V],[]}) ->
    {{value,V},{[],[]}};
out({[Y|In],[]}) ->
    [V|Out] = lists:reverse(In, []),
    {{value,V},{[Y],Out}};
out({In,[V]}) when is_list(In) ->
    {{value,V},r2f(In)};
out({In,[V|Out]}) when is_list(In) ->
    {{value,V},{In,Out}};
out(Q) ->
    erlang:error(badarg, [Q]).

%% Take from tail/rear
%%
%% O(1) amortized, O(len(Q)) worst case
-doc """
Removes the item at the rear of queue `Q1`. Returns tuple `{{value, Item}, Q2}`,
where `Item` is the item removed and `Q2` is the new queue. If `Q1` is empty,
tuple `{empty, Q1}` is returned.

_Example:_

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
{[5,4,3],[1,2]}
2> {{value, 5=Item}, Queue1} = queue:out_r(Queue).
{{value,5},{[4,3],[1,2]}}
3> queue:to_list(Queue1).
[1,2,3,4]
```
""".
-doc(#{title => <<"Original API">>}).
-spec out_r(Q1 :: queue(Item)) ->
                 {{value, Item}, Q2 :: queue(Item)} |
                 {empty, Q1 :: queue(Item)}.
out_r({[],[]}=Q) ->
    {empty,Q};
out_r({[],[V]}) ->
    {{value,V},{[],[]}};
out_r({[],[Y|Out]}) ->
    [V|In] = lists:reverse(Out, []),
    {{value,V},{In,[Y]}};
out_r({[V],Out}) when is_list(Out) ->
    {{value,V},f2r(Out)};
out_r({[V|In],Out}) when is_list(Out) ->
    {{value,V},{In,Out}};
out_r(Q) ->
    erlang:error(badarg, [Q]).

%%--------------------------------------------------------------------------
%% Less garbage style API.

%% Return the first element in the queue
%%
%% O(1) since the queue is supposed to be well formed
-doc """
Returns `Item` at the front of queue `Q`.

Fails with reason `empty` if `Q` is empty.

_Example 1:_

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
{[5,4,3],[1,2]}
2> 1 == queue:get(Queue).
true
```
""".
-doc(#{title => <<"Extended API">>}).
-spec get(Q :: queue(Item)) -> Item.
get({[],[]}=Q) ->
    erlang:error(empty, [Q]);
get({R,F}) when is_list(R), is_list(F) ->
    get(R, F);
get(Q) ->
    erlang:error(badarg, [Q]).

-spec get(list(), list()) -> term().
get(R, [H|_]) when is_list(R) ->
    H;
get([H], []) ->
    H;
get([_|R], []) -> % malformed queue -> O(len(Q))
    lists:last(R).

%% Return the last element in the queue
%%
%% O(1) since the queue is supposed to be well formed
-doc """
Returns `Item` at the rear of queue `Q`.

Fails with reason `empty` if `Q` is empty.

_Example 1:_

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
{[5,4,3],[1,2]}
2> 5 == queue:get_r(Queue).
true
```
""".
-doc(#{title => <<"Extended API">>}).
-spec get_r(Q :: queue(Item)) -> Item.
get_r({[],[]}=Q) ->
    erlang:error(empty, [Q]);
get_r({[H|_],F}) when is_list(F) ->
    H;
get_r({[],[H]}) ->
    H;
get_r({[],[_|F]}) -> % malformed queue -> O(len(Q))
    lists:last(F);
get_r(Q) ->
    erlang:error(badarg, [Q]).

%% Return the first element in the queue
%%
%% O(1) since the queue is supposed to be well formed
-doc """
Returns tuple `{value, Item}`, where `Item` is the front item of `Q`, or `empty`
if `Q` is empty.

_Example 1:_

```erlang
1> queue:peek(queue:new()).
empty
2> Queue = queue:from_list([1,2,3,4,5]).
{[5,4,3],[1,2]}
3> queue:peek(Queue).
{value, 1}
```
""".
-doc(#{title => <<"Extended API">>}).
-spec peek(Q :: queue(Item)) -> empty | {value, Item}.
peek({[],[]}) ->
    empty;
peek({R,[H|_]}) when is_list(R) ->
    {value,H};
peek({[H],[]}) ->
    {value,H};
peek({[_|R],[]}) -> % malformed queue -> O(len(Q))
    {value,lists:last(R)};
peek(Q) ->
    erlang:error(badarg, [Q]).

%% Return the last element in the queue
%%
%% O(1) since the queue is supposed to be well formed
-doc """
Returns tuple `{value, Item}`, where `Item` is the rear item of `Q`, or `empty`
if `Q` is empty.

_Example 1:_

```erlang
1> queue:peek_r(queue:new()).
empty
2> Queue = queue:from_list([1,2,3,4,5]).
{[5,4,3],[1,2]}
3> queue:peek_r(Queue).
{value, 5}
```
""".
-doc(#{title => <<"Extended API">>}).
-spec peek_r(Q :: queue(Item)) -> empty | {value, Item}.
peek_r({[],[]}) ->
    empty;
peek_r({[H|_],F}) when is_list(F) ->
    {value,H};
peek_r({[],[H]}) ->
    {value,H};
peek_r({[],[_|R]}) -> % malformed queue -> O(len(Q))
    {value,lists:last(R)};
peek_r(Q) ->
    erlang:error(badarg, [Q]).

%% Remove the first element and return resulting queue
%%
%% O(1) amortized
-doc """
Returns a queue `Q2` that is the result of removing the front item from `Q1`.

Fails with reason `empty` if `Q1` is empty.

_Example:_

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
{[5,4,3],[1,2]}
2> Queue = queue:drop(Queue).
{[5,4,3],[2]}
3> queue:to_list(Queue1).
[2,3,4,5]
```
""".
-doc(#{title => <<"Extended API">>}).
-spec drop(Q1 :: queue(Item)) -> Q2 :: queue(Item).
drop({[],[]}=Q) ->
    erlang:error(empty, [Q]);
drop({[_],[]}) ->
    {[],[]};
drop({[Y|R],[]}) ->
    [_|F] = lists:reverse(R, []),
    {[Y],F};
drop({R, [_]}) when is_list(R) ->
    r2f(R);
drop({R, [_|F]}) when is_list(R) ->
    {R,F};
drop(Q) ->
    erlang:error(badarg, [Q]).

%% Remove the last element and return resulting queue
%%
%% O(1) amortized
-doc """
Returns a queue `Q2` that is the result of removing the rear item from `Q1`.

Fails with reason `empty` if `Q1` is empty.

_Example:_

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
{[5,4,3],[1,2]}
2> Queue = queue:drop_r(Queue).
{[4,3],[1,2]}
3> queue:to_list(Queue1).
[1,2,3,4]
```
""".
-doc(#{title => <<"Extended API">>}).
-spec drop_r(Q1 :: queue(Item)) -> Q2 :: queue(Item).
drop_r({[],[]}=Q) ->
    erlang:error(empty, [Q]);
drop_r({[],[_]}) ->
    {[],[]};
drop_r({[],[Y|F]}) ->
    [_|R] = lists:reverse(F, []),
    {R,[Y]};
drop_r({[_], F}) when is_list(F) ->
    f2r(F);
drop_r({[_|R], F}) when is_list(F) ->
    {R,F};
drop_r(Q) ->
    erlang:error(badarg, [Q]).

%%--------------------------------------------------------------------------
%% Higher level API

%% Return reversed queue
%%
%% O(1)
-doc "Returns a queue `Q2` containing the items of `Q1` in the reverse order.".
-doc(#{title => <<"Original API">>}).
-spec reverse(Q1 :: queue(Item)) -> Q2 :: queue(Item).
reverse({R,F}) when is_list(R), is_list(F) ->
    {F,R};
reverse(Q) ->
    erlang:error(badarg, [Q]).

%% Join two queues
%%
%% Q2 empty: O(1)
%% else:     O(len(Q1))
-doc """
Returns a queue `Q3` that is the result of joining `Q1` and `Q2` with `Q1` in
front of `Q2`.

_Example:_

```erlang
1> Queue1 = queue:from_list([1,3]).
{[3],[1]}
2> Queue2 = queue:from_list([2,4]).
{[4],[2]}
3> queue:to_list(queue:join(Queue1, Queue2)).
[1,3,2,4]
```
""".
-doc(#{title => <<"Original API">>}).
-spec join(Q1 :: queue(Item), Q2 :: queue(Item)) -> Q3 :: queue(Item).
join({R,F}=Q, {[],[]}) when is_list(R), is_list(F) ->
    Q;
join({[],[]}, {R,F}=Q) when is_list(R), is_list(F) ->
    Q;
join({R1,F1}, {R2,F2}) when is_list(R1), is_list(F1), is_list(R2), is_list(F2) ->
    {R2,F1++lists:reverse(R1,F2)};
join(Q1, Q2) ->
    erlang:error(badarg, [Q1,Q2]).

%% Split a queue in two
%%
%% N = 0..len(Q)
%% O(max(N, len(Q)))
-doc "Splits `Q1` in two. The `N` front items are put in `Q2` and the rest in `Q3`.".
-doc(#{title => <<"Original API">>}).
-spec split(N :: non_neg_integer(), Q1 :: queue(Item)) ->
                   {Q2 :: queue(Item),Q3 :: queue(Item)}.
split(0, {R,F}=Q) when is_list(R), is_list(F) ->
    {{[],[]},Q};
split(N, {R,F}=Q) when is_integer(N), N >= 1, is_list(R), is_list(F) ->
    Lf = erlang:length(F),
    if  N < Lf -> % Lf >= 2
	    [X|F1] = F,
	    split_f1_to_r2(N-1, R, F1, [], [X]);
        N > Lf ->
	    Lr = length(R),
	    M = Lr - (N-Lf),
	    if  M < 0 ->
		    erlang:error(badarg, [N,Q]);
		M > 0 ->
		    [X|R1] = R,
		    split_r1_to_f2(M-1, R1, F, [X], []);
		true -> % M == 0
		    {Q,{[],[]}}
	    end;
	true -> % N == Lf
	    {f2r(F),r2f(R)}
    end;
split(N, Q) ->
    erlang:error(badarg, [N,Q]).

%% Move N elements from F1 to R2
split_f1_to_r2(0, R1, F1, R2, F2) ->
    {{R2,F2},{R1,F1}};
split_f1_to_r2(N, R1, [X|F1], R2, F2) ->
    split_f1_to_r2(N-1, R1, F1, [X|R2], F2).

%% Move N elements from R1 to F2
split_r1_to_f2(0, R1, F1, R2, F2) ->
    {{R1,F1},{R2,F2}};
split_r1_to_f2(N, [X|R1], F1, R2, F2) ->
    split_r1_to_f2(N-1, R1, F1, R2, [X|F2]).

%% filter, or rather filtermap with insert, traverses in queue order
%% 
%% Fun(_) -> List: O(length(List) * len(Q))
%% else:           O(len(Q)
-doc """
Returns a queue `Q2` that is the result of calling `Fun(Item)` on all items in
`Q1`.

If `Fun(Item)` returns `true`, `Item` is copied to the result queue. If it
returns `false`, `Item` is not copied. If it returns a list, the list elements
are inserted instead of `Item` in the result queue.

_Example 1:_

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
{[5,4,3],[1,2]}
2> Queue1 = queue:filter(fun (E) -> E > 2 end, Queue).
{[5],[3,4]}
3> queue:to_list(Queue1).
[3,4,5]
```

So, `Fun(Item)` returning `[Item]` is thereby semantically equivalent to
returning `true`, just as returning `[]` is semantically equivalent to returning
`false`. But returning a list builds more garbage than returning an atom.

_Example 2:_

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
{[5,4,3],[1,2]}
2> Queue1 = queue:filter(fun (E) -> [E, E+1] end, Queue).
{[6,5,5,4,4,3],[1,2,2,3]}
3> queue:to_list(Queue1).
[1,2,2,3,3,4,4,5,5,6]
```
""".
-doc(#{title => <<"Original API">>}).
-spec filter(Fun, Q1 :: queue(Item)) -> Q2 :: queue(Item) when
      Fun :: fun((Item) -> boolean() | list(Item)).
filter(Fun, {R0,F0}) when is_function(Fun, 1), is_list(R0), is_list(F0) ->
    F = filter_f(Fun, F0),
    R = filter_r(Fun, R0),
    if R =:= [] ->
	    f2r(F);
       F =:= [] ->
	    r2f(R);
       true ->
	    {R,F}
    end;
filter(Fun, Q) ->
    erlang:error(badarg, [Fun,Q]).

%% Call Fun in head to tail order
filter_f(_, []) ->
    [];
filter_f(Fun, [X|F]) ->
    case Fun(X) of
	true ->
	    [X|filter_f(Fun, F)];
	[Y] ->
	    [Y|filter_f(Fun, F)];
	false ->
	    filter_f(Fun, F);
	[] ->
	    filter_f(Fun, F);
	L when is_list(L) ->
	    L++filter_f(Fun, F)
    end.

%% Call Fun in reverse order, i.e tail to head
%% and reverse list result from fun to match queue order
filter_r(_, []) ->
    [];
filter_r(Fun, [X|R0]) ->
    R = filter_r(Fun, R0),
    case Fun(X) of
	true ->
	    [X|R];
	[Y] ->
	    [Y|R];
	false ->
	    R;
	[] ->
	    R;
	L when is_list(L) ->
	    lists:reverse(L, R)
    end.

%% Filter and map a queue, traverses in queue order.
%%
%% O(len(Q1))
-doc """
Returns a queue `Q2` that is the result of calling `Fun(Item)` on all items in
`Q1`.

If `Fun(Item)` returns `true`, `Item` is copied to the result queue. If it
returns `false`, `Item` is not copied. If it returns `{true, NewItem}`, the
queue element at this position is replaced with `NewItem` in the result queue.

_Example 1:_

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
{[5,4,3],[1,2]}
2> Queue1 = queue:filtermap(fun (E) -> E > 2 end, Queue).
{[5],[3,4]}
3> queue:to_list(Queue1).
[3,4,5]
4> Queue1 = queue:filtermap(fun (E) -> {true, E+100} end, Queue).
{"ihg","ef"}
5> queue:to_list(Queue1).
"efghi
```
""".
-doc(#{title => <<"Original API">>,since => <<"OTP 24.0">>}).
-spec filtermap(Fun, Q1) -> Q2 when
      Fun :: fun((Item) -> boolean() | {'true', Value}),
      Q1 :: queue(Item),
      Q2 :: queue(Item | Value),
      Item :: term(),
      Value :: term().
filtermap(Fun, {R0, F0}) when is_function(Fun, 1), is_list(R0), is_list(F0) ->
    F = lists:filtermap(Fun, F0),
    R = filtermap_r(Fun, R0),
    if R =:= [] ->
	    f2r(F);
       F =:= [] ->
	    r2f(R);
       true ->
	    {R,F}
    end;
filtermap(Fun, Q) ->
    erlang:error(badarg, [Fun,Q]).

%% Call Fun in reverse order, i.e tail to head
filtermap_r(_, []) ->
    [];
filtermap_r(Fun, [X|R0]) ->
    R = filtermap_r(Fun, R0),
    case Fun(X) of
	true ->
	    [X|R];
	{true, Y} ->
	    [Y|R];
	false ->
	    R
    end.

%% Fold a function over a queue, in queue order.
%%
%% O(len(Q))
-doc """
Calls `Fun(Item, AccIn)` on successive items `Item` of `Queue`, starting with
`AccIn == Acc0`. The queue is traversed in queue order, that is, from front to
rear. `Fun/2` must return a new accumulator, which is passed to the next call.
The function returns the final value of the accumulator. `Acc0` is returned if
the queue is empty.

_Example:_

```erlang
1> queue:fold(fun(X, Sum) -> X + Sum end, 0, queue:from_list([1,2,3,4,5])).
15
2> queue:fold(fun(X, Prod) -> X * Prod end, 1, queue:from_list([1,2,3,4,5])).
120
```
""".
-doc(#{title => <<"Original API">>,since => <<"OTP 24.0">>}).
-spec fold(Fun, Acc0, Q :: queue(Item)) -> Acc1 when
      Fun :: fun((Item, AccIn) -> AccOut),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term().
fold(Fun, Acc0, {R, F}) when is_function(Fun, 2), is_list(R), is_list(F) ->
    Acc1 = lists:foldl(Fun, Acc0, F),
    lists:foldr(Fun, Acc1, R);
fold(Fun, Acc0, Q) ->
    erlang:error(badarg, [Fun, Acc0, Q]).

%% Check if any item satisfies the predicate, traverse in queue order.
%%
%% O(len(Q)) worst case
-doc """
Returns `true` if `Pred(Item)` returns `true` for at least one item `Item` in
`Q`, otherwise `false`.

_Example:_

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
2> queue:any(fun (E) -> E > 10 end, Queue).
false
3> queue:any(fun (E) -> E > 3 end, Queue).
true
```
""".
-doc(#{title => <<"Original API">>,since => <<"OTP 24.0">>}).
-spec any(Pred, Q :: queue(Item)) -> boolean() when
      Pred :: fun((Item) -> boolean()).
any(Pred, {R, F}) when is_function(Pred, 1), is_list(R), is_list(F) ->
    lists:any(Pred, F) orelse
    lists:any(Pred, R);
any(Pred, Q) ->
    erlang:error(badarg, [Pred, Q]).

%% Check if all items satisfy the predicate, traverse in queue order.
%%
%% O(len(Q)) worst case
-doc """
Returns `true` if `Pred(Item)` returns `true` for all items `Item` in `Q`,
otherwise `false`.

_Example:_

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
2> queue:all(fun (E) -> E > 3 end, Queue).
false
3> queue:all(fun (E) -> E > 0 end, Queue).
true
```
""".
-doc(#{title => <<"Original API">>,since => <<"OTP 24.0">>}).
-spec all(Pred, Q :: queue(Item)) -> boolean() when
      Pred :: fun((Item) -> boolean()).
all(Pred, {R, F}) when is_function(Pred, 1), is_list(R), is_list(F) ->
    lists:all(Pred, F) andalso
    lists:all(Pred, R);
all(Pred, Q) ->
    erlang:error(badarg, [Pred, Q]).

%% Delete the first occurence of an item in the queue,
%% according to queue order.
%%
%% O(len(Q1)) worst case
-doc """
Returns a copy of `Q1` where the first item matching `Item` is deleted, if there
is such an item.

_Example:_

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
2> Queue1 = queue:delete(3, Queue).
3> queue:member(3, Queue1).
false
```
""".
-doc(#{title => <<"Original API">>,since => <<"OTP 24.0">>}).
-spec delete(Item, Q1) -> Q2 when
      Item :: T,
      Q1 :: queue(T),
      Q2 :: queue(T),
      T :: term().
delete(Item, {R0, F0} = Q) when is_list(R0), is_list(F0) ->
    case delete_front(Item, F0) of
        false ->
            case delete_rear(Item, R0) of
                false ->
                    Q;
                [] ->
                    f2r(F0);
                R1 ->
                    {R1, F0}
            end;
        [] ->
            r2f(R0);
        F1 ->
            {R0, F1}
    end;
delete(Item, Q) ->
    erlang:error(badarg, [Item, Q]).

%% Delete the last occurence of an item in the queue,
%% according to queue order.
%%
%% O(len(Q1)) worst case
-doc """
Returns a copy of `Q1` where the last item matching `Item` is deleted, if there
is such an item.

_Example:_

```erlang
1> Queue = queue:from_list([1,2,3,4,3,5]).
2> Queue1 = queue:delete_r(3, Queue).
3> queue:to_list(Queue1).
[1,2,3,4,5]
```
""".
-doc(#{title => <<"Original API">>,since => <<"OTP 24.0">>}).
-spec delete_r(Item, Q1) -> Q2 when
      Item :: T,
      Q1 :: queue(T),
      Q2 :: queue(T),
      T :: term().
delete_r(Item, {R0, F0}) when is_list(R0), is_list(F0) ->
    {F1, R1}=delete(Item, {F0, R0}),
    {R1, F1};
delete_r(Item, Q) ->
    erlang:error(badarg, [Item, Q]).

delete_front(Item, [Item|Rest]) ->
    Rest;
delete_front(Item, [X|Rest]) ->
    case delete_front(Item, Rest) of
        false -> false;
        F -> [X|F]
    end;
delete_front(_, []) ->
    false.

delete_rear(Item, [X|Rest]) ->
    case delete_rear(Item, Rest) of
        false when X=:=Item ->
            Rest;
        false ->
            false;
        R ->
            [X|R]
    end;
delete_rear(_, []) ->
    false.

%% Delete the first occurence of an item in the queue
%% matching a predicate, according to queue order.
%%
%% O(len(Q1)) worst case
-doc """
Returns a copy of `Q1` where the first item for which `Pred` returns `true` is
deleted, if there is such an item.

_Example:_

```erlang
1> Queue = queue:from_list([100,1,2,3,4,5]).
2> Queue1 = queue:delete_with(fun (E) -> E > 0, Queue).
3> queue:to_list(Queue1).
[1,2,3,4,5]
```
""".
-doc(#{title => <<"Original API">>,since => <<"OTP 24.0">>}).
-spec delete_with(Pred, Q1) -> Q2 when
      Pred :: fun((Item) -> boolean()),
      Q1 :: queue(Item),
      Q2 :: queue(Item),
      Item :: term().
delete_with(Pred, {R0, F0} = Q) when is_function(Pred, 1), is_list(R0), is_list(F0) ->
    case delete_with_front(Pred, F0) of
	false ->
	    case delete_with_rear(Pred, R0) of
		false ->
		    Q;
		[] ->
		    f2r(F0);
		R1 ->
		    {R1, F0}
	    end;
	[] ->
	    r2f(R0);
	F1 ->
	    {R0, F1}
    end;
delete_with(Pred, Q) ->
    erlang:error(badarg, [Pred, Q]).

%% Delete the last occurence of an item in the queue
%% matching a predicate, according to queue order.
%%
%% O(len(Q1)) worst case
-doc """
Returns a copy of `Q1` where the last item for which `Pred` returns `true` is
deleted, if there is such an item.

_Example:_

```erlang
1> Queue = queue:from_list([1,2,3,4,5,100]).
2> Queue1 = queue:delete_with(fun (E) -> E > 10, Queue).
3> queue:to_list(Queue1).
[1,2,3,4,5]
```
""".
-doc(#{title => <<"Original API">>,since => <<"OTP 24.0">>}).
-spec delete_with_r(Pred, Q1) -> Q2 when
      Pred :: fun((Item) -> boolean()),
      Q1 :: queue(Item),
      Q2 :: queue(Item),
      Item :: term().
delete_with_r(Pred, {R0, F0}) when is_function(Pred, 1), is_list(R0), is_list(F0) ->
    {F1, R1} = delete_with(Pred, {F0, R0}),
    {R1, F1};
delete_with_r(Pred, Q) ->
    erlang:error(badarg, [Pred, Q]).

delete_with_front(Pred, [X|Rest]) ->
    case Pred(X) of
	true ->
	    Rest;
	false ->
	    case delete_with_front(Pred, Rest) of
		false ->
		    false;
		F ->
		    [X|F]
	    end
    end;
delete_with_front(_, []) ->
    false.

delete_with_rear(Pred, [X|Rest]) ->
    case delete_with_rear(Pred, Rest) of
	false ->
	    case Pred(X) of
		true ->
		    Rest;
		false ->
		    false
	    end;
	R ->
	    [X|R]
    end;
delete_with_rear(_, []) ->
    false.

%%--------------------------------------------------------------------------
%% Okasaki API inspired by an Erlang user contribution "deque.erl" 
%% by Claes Wikstrom <klacke@kaja.klacke.net> 1999.
%%
%% This implementation does not use the internal data format from Klacke's
%% doubly ended queues that was "shamelessly stolen" from 
%% "Purely Functional Data structures" by Chris Okasaki, since the data
%% format of this module must remain the same in case some application
%% has saved a queue in external format or sends it to an old node.
%%
%% This implementation tries to do the best of the situation and should 
%% be almost as efficient as Okasaki's queues, except for len/1 that
%% is O(n) in this implementation instead of O(1).
%%
%% The new representation in this module again adds length field and
%% fixes this, but it is not yet default.
%%
%% The implementation keeps at least one element in both the forward
%% and the reversed lists to ensure that i.e head/1 or last/1 will
%% not have to reverse a list to find the element.
%%
%% To be compatible with the old version of this module, as much data as 
%% possible is moved to the receiving side using lists:reverse/2 when data
%% is needed, except for two elements (when possible). These two elements
%% are kept to prevent alternating tail/1 and init/1 operations from 
%% moving data back and forth between the sides.
%%
%% An alternative would be to balance for equal list length when one side
%% is exhausted. Although this could be better for a general double
%% ended queue, it would more han double the amortized cost for 
%% the normal case (one way queue).

%% Cons to head
%%
-doc """
Inserts `Item` at the head of queue `Q1`. Returns the new queue `Q2`.

_Example:_

```erlang
1> Queue = queue:cons(0, queue:from_list([1,2,3])).
{[3,2],[0,1]}
2> queue:to_list(Queue).
[0,1,2,3]
```
""".
-doc(#{title => <<"Okasaki API">>}).
-spec cons(Item, Q1 :: queue(Item)) -> Q2 :: queue(Item).
cons(X, Q) ->
    in_r(X, Q).

%% Return head element
%%
%% Return the first element in the queue
%%
%% O(1) since the queue is supposed to be well formed
-doc """
Returns `Item` from the head of queue `Q`.

Fails with reason `empty` if `Q` is empty.

_Example 1:_

```erlang
1> queue:head(queue:from_list([1,2,3])).
1
```
""".
-doc(#{title => <<"Okasaki API">>}).
-spec head(Q :: queue(Item)) -> Item.
head({[],[]}=Q) ->
    erlang:error(empty, [Q]);
head({R,F}) when is_list(R), is_list(F) ->
    get(R, F);
head(Q) ->
    erlang:error(badarg, [Q]).

%% Remove head element and return resulting queue
%%
-doc """
Returns a queue `Q2` that is the result of removing the head item from `Q1`.

Fails with reason `empty` if `Q1` is empty.
""".
-doc(#{title => <<"Okasaki API">>}).
-spec tail(Q1 :: queue(Item)) -> Q2 :: queue(Item).
tail(Q) ->
    drop(Q).

%% Functions operating on the other end of the queue

%% Cons to tail
%%
-doc """
Inserts `Item` as the tail item of queue `Q1`. Returns the new queue `Q2`.

_Example:_

```erlang
1> Queue = queue:snoc(queue:from_list([1,2,3]), 4).
{[4,3,2],[1]}
2> queue:to_list(Queue).
[1,2,3,4]
```
""".
-doc(#{title => <<"Okasaki API">>}).
-spec snoc(Q1 :: queue(Item), Item) -> Q2 :: queue(Item).
snoc(Q, X) ->
    in(X, Q).

%% Return last element
-doc """
Returns the tail item of queue `Q`.

Fails with reason `empty` if `Q` is empty.

_Example 1:_

```erlang
1> queue:daeh(queue:from_list([1,2,3])).
3
```
""".
-doc(#{title => <<"Okasaki API">>}).
-spec daeh(Q :: queue(Item)) -> Item.
daeh(Q) -> get_r(Q).
-doc """
Returns the tail item of queue `Q`.

Fails with reason `empty` if `Q` is empty.

_Example:_

```erlang
1> queue:last(queue:from_list([1,2,3])).
3
```
""".
-doc(#{title => <<"Okasaki API">>}).
-spec last(Q :: queue(Item)) -> Item.
last(Q) -> get_r(Q).

%% Remove last element and return resulting queue
-doc """
Returns a queue `Q2` that is the result of removing the tail item from `Q1`.

Fails with reason `empty` if `Q1` is empty.

_Example:_

```erlang
1> Queue = queue:liat(queue:from_list([1,2,3])).
{[2],[1]}
2> queue:to_list(Queue).
[1,2]
```
""".
-doc(#{title => <<"Okasaki API">>}).
-spec liat(Q1 :: queue(Item)) -> Q2 :: queue(Item).
liat(Q) -> drop_r(Q).
-doc """
Returns a queue `Q2` that is the result of removing the tail item from `Q1`.

Fails with reason `empty` if `Q1` is empty.

The name [`lait/1`](`lait/1`) is a misspelling - do not use it anymore.
""".
-doc(#{title => <<"Okasaki API">>}).
-spec lait(Q1 :: queue(Item)) -> Q2 :: queue(Item).
lait(Q) -> drop_r(Q). %% Oops, mis-spelled 'tail' reversed. Forget this one.
-doc """
Returns a queue `Q2` that is the result of removing the tail item from `Q1`.

Fails with reason `empty` if `Q1` is empty.

_Example:_

```erlang
1> Queue = queue:init(queue:from_list([1,2,3])).
{[2],[1]}
2> queue:to_list(Queue).
[1,2]
```
""".
-doc(#{title => <<"Okasaki API">>}).
-spec init(Q1 :: queue(Item)) -> Q2 :: queue(Item).
init(Q) -> drop_r(Q).

%%--------------------------------------------------------------------------
%% Internal workers

-compile({inline, [{r2f,1},{f2r,1}]}).

%% Move half of elements from R to F, if there are at least three
r2f([]) ->
    {[],[]};
r2f([_]=R) ->
    {[],R};
r2f([Y,X]) ->
    {[Y],[X]};
r2f(List) ->
    {RR,FF} = lists:split(length(List) div 2, List),
    {RR,lists:reverse(FF, [])}.

%% Move half of elements from F to R, if there are enough
f2r([]) ->
    {[],[]};
f2r([_]=F) ->
    {F,[]};
f2r([X,Y]) ->
    {[Y],[X]};
f2r(List) ->
    {FF,RR} = lists:split(length(List) div 2, List),
    {lists:reverse(RR, []),FF}.
