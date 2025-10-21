%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright (C) 1999-2001 Richard Carlsson
%% Copyright Ericsson AB 2011-2025. All Rights Reserved.
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
%% =====================================================================
%% Ordered Sets implemented as General Balanced Trees
%%
%% An implementation of ordered sets using Prof. Arne Andersson's
%% General Balanced Trees. This can be much more efficient than using
%% ordered lists, for larger sets, but depends on the application. See
%% notes below for details.
%% ---------------------------------------------------------------------

-module(gb_sets).
-moduledoc """
Sets represented by general balanced trees.

This module provides ordered sets using Prof. Arne Andersson's General Balanced
Trees. Ordered sets can be much more efficient than using ordered lists, for
larger sets, but depends on the application.

The data representing a set as used by this module is to be regarded as opaque
by other modules. In abstract terms, the representation is a composite type of
existing Erlang terms. See note on
[data types](`e:system:data_types.md#no_user_types`). Any code assuming
knowledge of the format is running on thin ice.

This module considers two elements as different if and only if they do not
compare equal (`==`).

## Complexity Note

The complexity on set operations is bounded by either _O(|S|)_ or _O(|T| _
log(|S|))\*, where S is the largest given set, depending on which is fastest for
any particular function call. For operating on sets of almost equal size, this
implementation is about 3 times slower than using ordered-list sets directly.
For sets of very different sizes, however, this solution can be arbitrarily much
faster; in practical cases, often 10-100 times. This implementation is
particularly suited for accumulating elements a few at a time, building up a
large set (> 100-200 elements), and repeatedly testing for membership in the
current set.

As with normal tree structures, lookup (membership testing), insertion, and
deletion have logarithmic complexity.

## Compatibility

See the [Compatibility Section in the `sets` module](`m:sets#module-compatibility`)
for information about the compatibility of the different implementations of sets
in the Standard Library.

## See Also

`m:gb_trees`, `m:ordsets`, `m:sets`
""".

-export([empty/0, is_empty/1, size/1, singleton/1, is_member/2,
	 insert/2, add/2, delete/2, delete_any/2, balance/1, union/2,
	 union/1, intersection/2, intersection/1, is_equal/2,
	 is_disjoint/2, difference/2, is_subset/2, to_list/1,
	 from_list/1, from_ordset/1, smallest/1, largest/1,
	 take_smallest/1, take_largest/1, smaller/2, larger/2,
         iterator/1, iterator/2, iterator_from/2, iterator_from/3,
         next/1, filter/2, fold/3, map/2, filtermap/2, is_set/1]).

%% `sets' compatibility aliases:

-export([new/0, is_element/2, add_element/2, del_element/2,
	 subtract/2]).

%% GB-trees adapted from Sven-Olof Nyström's implementation for
%% representation of sets.
%%
%% Data structures:
%% - {Size, Tree}, where `Tree' is composed of nodes of the form:
%% - {Key, Smaller, Bigger}, and the "empty tree" node:
%% - nil.
%%
%% No attempt is made to balance trees after deletions. Since deletions
%% don't increase the height of a tree, this should be OK.
%%
%% Original balance condition h(T) <= ceil(c * log(|T|)) has been
%% changed to the similar (but not quite equivalent) condition 2 ^ h(T)
%% <= |T| ^ c. This should also be OK.
%%
%% Behaviour is logarithmic (as it should be).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Some macros.

-define(p, 2). % It seems that p = 2 is optimal for sorted keys

-define(pow(A, _), A * A). % correct with exponent as defined above.

-define(div2(X), X bsr 1).

-define(mul2(X), X bsl 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Some types.

-export_type([set/0, set/1, iter/0, iter/1]).

-type gb_set_node(Element) :: 'nil' | {Element, _, _}.
-doc "A general balanced set.".
-opaque set(Element) :: {non_neg_integer(), gb_set_node(Element)}.
-type set() :: set(_).
-doc "A general balanced set iterator.".
-opaque iter(Element) :: {ordered | reversed, [gb_set_node(Element)]}.
-type iter() :: iter(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-doc """
Returns a new empty set.

## Examples

```erlang
1> gb_sets:to_list(gb_sets:empty()).
[]
```
""".
-spec empty() -> Set when
      Set :: set(none()).

empty() ->
    {0, nil}.

-doc """
Returns a new empty set.

## Examples

```erlang
1> gb_sets:to_list(gb_sets:new()).
[]
```
""".
-spec new() -> Set when
      Set :: set(none()).

new() -> empty().

-doc """
Returns `true` if `Set` is an empty set; otherwise, returns `false`.

## Examples

```erlang
1> gb_sets:is_empty(gb_sets:new()).
true
2> gb_sets:is_empty(gb_sets:singleton(1)).
false
```
""".
-spec is_empty(Set) -> boolean() when
      Set :: set().

is_empty({0, nil}) ->
    true;
is_empty(_) ->
    false.

-doc """
Returns the number of elements in `Set`.

## Examples

```erlang
1> gb_sets:size(gb_sets:new()).
0
2> gb_sets:size(gb_sets:from_list([4,5,6])).
3
```
""".
-spec size(Set) -> non_neg_integer() when
      Set :: set().

size({Size, _}) ->
    Size.

-doc """
Returns `true` if `Set1` and `Set2` are equal, that is, if every element
of one set is also a member of the other set; otherwise, returns `false`.

## Examples

```erlang
1> Empty = gb_sets:new().
2> S = gb_sets:from_list([a,b]).
3> gb_sets:is_equal(S, S)
true
4> gb_sets:is_equal(S, Empty)
false
```
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec is_equal(Set1, Set2) -> boolean() when
      Set1 :: set(),
      Set2 :: set().

is_equal({Size, S1}, {Size, _} = S2)  ->
    try is_equal_1(S1, to_list(S2)) of
        [] ->
            true
    catch
        throw:not_equal ->
            false
    end;
is_equal({_, _}, {_, _}) ->
    false.

is_equal_1(nil, Keys) ->
    Keys;
is_equal_1({Key1, Smaller, Bigger}, Keys0) ->
    [Key2 | Keys] = is_equal_1(Smaller, Keys0),
    if
        Key1 == Key2 ->
            is_equal_1(Bigger, Keys);
        true ->
            throw(not_equal)
    end.

-doc """
Returns a set containing only element `Element`.

## Examples

```erlang
1> S = gb_sets:singleton(42).
2> gb_sets:to_list(S).
[42]
```
""".
-spec singleton(Element) -> set(Element).

singleton(Key) ->
    {1, {Key, nil, nil}}.

-doc(#{equiv => is_member(Element, Set)}).
-spec is_element(Element, Set) -> boolean() when
      Set :: set(Element).

is_element(Key, S) ->
    is_member(Key, S).

-doc """
Returns `true` if `Element` is an element of `Set`; otherwise, returns
`false`.

## Examples

```erlang
1> S = gb_sets:from_list([a,b,c]).
2> gb_sets:is_member(42, S).
false
3> gb_sets:is_member(b, S).
true
```
""".
-spec is_member(Element, Set) -> boolean() when
      Set :: set(Element).

is_member(Key, {_, T}) ->
    is_member_1(Key, T).

is_member_1(Key, {Key1, Smaller, _}) when Key < Key1 ->
    is_member_1(Key, Smaller);
is_member_1(Key, {Key1, _, Bigger}) when Key > Key1 ->
    is_member_1(Key, Bigger);
is_member_1(_, {_, _, _}) ->
    true;
is_member_1(_, nil) ->
    false.

-doc """
Returns a new set formed from `Set1` with `Element` inserted,
assuming `Element` is not already present.

Use `add/2` for inserting into a set where `Element` is potentially
already present.

## Examples

```erlang
1> S0 = gb_sets:new().
2> S1 = gb_sets:insert(7, S0).
3> gb_sets:to_list(S1).
[7]
4> S2 = gb_sets:insert(42, S1).
5> gb_sets:to_list(S2).
[7,42]
```
""".
-spec insert(Element, Set1) -> Set2 when
      Set1 :: set(Element),
      Set2 :: set(Element).

insert(Key, {S, T}) when is_integer(S), S >= 0 ->
    S1 = S + 1,
    {S1, insert_1(Key, T, ?pow(S1, ?p))}.

insert_1(Key, {Key1, Smaller, Bigger}, S) when Key < Key1 ->
    case insert_1(Key, Smaller, ?div2(S)) of
	{T1, H1, S1} when is_integer(H1), is_integer(S1) ->
	    T = {Key1, T1, Bigger},
	    {H2, S2} = count(Bigger),
	    H = ?mul2(erlang:max(H1, H2)),
	    SS = S1 + S2 + 1,
	    P = ?pow(SS, ?p),
	    if
		H > P -> 
		    balance(T, SS);
		true ->
		    {T, H, SS}
	    end;
	T1 ->
	    {Key1, T1, Bigger}
    end;
insert_1(Key, {Key1, Smaller, Bigger}, S) when Key > Key1 ->
    case insert_1(Key, Bigger, ?div2(S)) of
	{T1, H1, S1} when is_integer(H1), is_integer(S1) ->
	    T = {Key1, Smaller, T1},
	    {H2, S2} = count(Smaller),
	    H = ?mul2(erlang:max(H1, H2)),
	    SS = S1 + S2 + 1,
	    P = ?pow(SS, ?p),
	    if
		H > P -> 
		    balance(T, SS);
		true ->
		    {T, H, SS}
	    end;
	T1 ->
	    {Key1, Smaller, T1}
    end;
insert_1(Key, nil, 0) ->
    {{Key, nil, nil}, 1, 1};
insert_1(Key, nil, _) ->
    {Key, nil, nil};
insert_1(Key, _, _) ->
    erlang:error({key_exists, Key}).

count({_, nil, nil}) ->
    {1, 1};
count({_, Sm, Bi}) ->
    {H1, S1} = count(Sm),
    {H2, S2} = count(Bi),
    {?mul2(erlang:max(H1, H2)), S1 + S2 + 1};
count(nil) ->
    {1, 0}.

-doc """
Rebalances the tree representation of `Set1`.

This is rarely necessary, but can be motivated when a large number of
elements have been deleted from the tree without further
insertions. Forcing rebalancing can minimize lookup times, as deletion
does not rebalance the tree.

## Examples

```erlang
1> S0 = gb_sets:from_ordset(lists:seq(1, 100)).
2> Delete = fun(E, Set) -> gb_sets:delete(E, Set) end.
3> S1 = lists:foldl(Delete, S0, lists:seq(1, 50)).
4> gb_sets:size(S1).
50
5> S2 = gb_sets:balance(S1).
```
""".
-spec balance(Set1) -> Set2 when
      Set1 :: set(Element),
      Set2 :: set(Element).

balance({S, T}) when is_integer(S), S >= 0 ->
    {S, balance(T, S)}.

balance(T, S) ->
    balance_list(to_list_1(T), S).

balance_list(L, S) ->
    {T, _} = balance_list_1(L, S),
    T.

balance_list_1(L, S) when S > 1 ->
    Sm = S - 1,
    S2 = Sm div 2,
    S1 = Sm - S2,
    {T1, [K | L1]} = balance_list_1(L, S1),
    {T2, L2} = balance_list_1(L1, S2),
    T = {K, T1, T2},
    {T, L2};
balance_list_1([Key | L], 1) ->
    {{Key, nil, nil}, L};
balance_list_1(L, 0) ->
    {nil, L}.

-doc """
Returns a new set formed from `Set1` with `Element` inserted.

If `Element` is already an element in `Set1`, nothing is changed.

## Examples

```erlang
1> S0 = gb_sets:new().
2> S1 = gb_sets:add_element(7, S0).
3> gb_sets:to_list(S1).
[7]
4> S2 = gb_sets:add_element(42, S1).
5> S2 = gb_sets:add_element(42, S1).
6> gb_sets:to_list(S2).
[7,42]
```
""".
-spec add_element(Element, Set1) -> Set2 when
      Set1 :: set(Element),
      Set2 :: set(Element).

add_element(X, S) ->
    add(X, S).

-doc(#{equiv => add_element(Element, Set1)}).
-spec add(Element, Set1) -> Set2 when
      Set1 :: set(Element),
      Set2 :: set(Element).

add(X, S) ->
    case is_member(X, S) of
	true ->
	    S;    % we don't have to do anything here
	false ->
	    insert(X, S)
    end.

-doc """
Returns a set of the elements in `List`, where `List` can be unordered and
contain duplicates.

## Examples

```erlang
1> Unordered = [x,y,a,x,y,b,b,z]
2> gb_sets:to_list(gb_sets:from_list(Unordered)).
[a,b,x,y,z]
```
""".
-spec from_list(List) -> Set when
      List :: [Element],
      Set :: set(Element).

from_list(L) ->
    from_ordset(ordsets:from_list(L)).

-doc """
Turns an ordered list without duplicates `List` into a set.

See `from_list/1` for a function that accepts unordered lists with
duplicates.

## Examples

```erlang
1> Ordset = [1,2,3].
2> gb_sets:to_list(gb_sets:from_ordset(Ordset)).
[1,2,3]
```
""".
-spec from_ordset(List) -> Set when
      List :: [Element],
      Set :: set(Element).

from_ordset(L) ->
    S = length(L),
    {S, balance_list(L, S)}.

-doc(#{equiv => delete_any(Element, Set1)}).
-spec del_element(Element, Set1) -> Set2 when
      Set1 :: set(Element),
      Set2 :: set(Element).

del_element(Key, S) ->
    delete_any(Key, S).

-doc """
Returns a new set formed from `Set1` with `Element` removed.

If `Element` is not an element in `Set1`, nothing is changed.

## Examples

```erlang
1> S = gb_sets:from_list([a,b]).
2> gb_sets:to_list(gb_sets:delete_any(b, S)).
[a]
3> S = gb_sets:delete_any(x, S).
```
""".
-spec delete_any(Element, Set1) -> Set2 when
      Set1 :: set(Element),
      Set2 :: set(Element).

delete_any(Key, S) ->
    case is_member(Key, S) of
 	true ->
 	    delete(Key, S);
 	false ->
 	    S
    end.

-doc """
Returns a new set formed from `Set1` with `Element` removed, assuming
`Element` is present in `Set1`.

Use `delete_any/2` when deleting from a set where `Element` is potentially
missing.

## Examples

```erlang
1> S = gb_sets:from_list([a,b]).
2> gb_sets:to_list(gb_sets:delete(b, S)).
[a]
```
""".
-spec delete(Element, Set1) -> Set2 when
      Set1 :: set(Element),
      Set2 :: set(Element).

delete(Key, {S, T}) ->
    {S - 1, delete_1(Key, T)}.

delete_1(Key, {Key1, Smaller, Larger}) when Key < Key1 ->
    Smaller1 = delete_1(Key, Smaller),
    {Key1, Smaller1, Larger};
delete_1(Key, {Key1, Smaller, Bigger}) when Key > Key1 ->
    Bigger1 = delete_1(Key, Bigger),
    {Key1, Smaller, Bigger1};
delete_1(_, {_, Smaller, Larger}) ->
    merge(Smaller, Larger).

merge(Smaller, nil) ->
    Smaller;
merge(nil, Larger) ->
    Larger;
merge(Smaller, Larger) ->
    {Key, Larger1} = take_smallest1(Larger),
    {Key, Smaller, Larger1}.

-doc """
Returns `{Element, Set2}`, where `Element` is the smallest element in
`Set1`, and `Set2` is this set with `Element` deleted.

Assumes that `Set1` is not empty.

## Examples

```erlang
1> S0 = gb_sets:from_list([a,b,c]).
2> {Smallest,S1} = gb_sets:take_smallest(S0).
3> Smallest.
a
4> gb_sets:to_list(S1).
[b,c]
```
""".
-spec take_smallest(Set1) -> {Element, Set2} when
      Set1 :: set(Element),
      Set2 :: set(Element).

take_smallest({S, T}) ->
    {Key, Larger} = take_smallest1(T),
    {Key, {S - 1, Larger}}.

take_smallest1({Key, nil, Larger}) ->
    {Key, Larger};
take_smallest1({Key, Smaller, Larger}) ->
    {Key1, Smaller1} = take_smallest1(Smaller),
    {Key1, {Key, Smaller1, Larger}}.

-doc """
Returns the smallest element in `Set`.

Assumes that `Set` is not empty.

## Examples

```erlang
1> S = gb_sets:from_list([a,b,c]).
2> gb_sets:smallest(S).
a
```
""".
-spec smallest(Set) -> Element when
      Set :: set(Element).

smallest({_, T}) ->
    smallest_1(T).

smallest_1({Key, nil, _Larger}) ->
    Key;
smallest_1({_Key, Smaller, _Larger}) ->
    smallest_1(Smaller).

-doc """
Returns `{Element, Set2}`, where `Element` is the largest element in
`Set1`, and `Set2` is this set with `Element` deleted.

Assumes that `Set1` is not empty.

## Examples

```erlang
1> S0 = gb_sets:from_list([a,b,c]).
2> {Largest,S1} = gb_sets:take_largest(S0).
3> Largest.
c
4> gb_sets:to_list(S1).
[a,b]
```
""".
-spec take_largest(Set1) -> {Element, Set2} when
      Set1 :: set(Element),
      Set2 :: set(Element).

take_largest({S, T}) ->
    {Key, Smaller} = take_largest1(T),
    {Key, {S - 1, Smaller}}.

take_largest1({Key, Smaller, nil}) ->
    {Key, Smaller};
take_largest1({Key, Smaller, Larger}) ->
    {Key1, Larger1} = take_largest1(Larger),
    {Key1, {Key, Smaller, Larger1}}.

-doc """
Returns the largest element in `Set`.

Assumes that `Set` is not empty.

## Examples

```erlang
1> S = gb_sets:from_list([a,b,c]).
2> gb_sets:largest(S).
c
```
""".
-spec largest(Set) -> Element when
      Set :: set(Element).

largest({_, T}) ->
    largest_1(T).

largest_1({Key, _Smaller, nil}) ->
    Key;
largest_1({_Key, _Smaller, Larger}) ->
    largest_1(Larger).

-doc """
Returns `{found, Element2}`, where `Element2` is the greatest element strictly
less than `Element1`.

Returns `none` if no such element exists.

## Examples

```erlang
1> S = gb_sets:from_list([a,b,c]).
2> gb_sets:smaller(b, S).
{found,a}
3> gb_sets:smaller(z, S).
{found,c}
4> gb_sets:smaller(a, S).
none
```
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec smaller(Element1, Set) -> none | {found, Element2} when
    Element1 :: Element,
    Element2 :: Element,
    Set :: set(Element).
smaller(Key, {_, T}) ->
    smaller_1(Key, T).

smaller_1(_Key, nil) ->
    none;
smaller_1(Key, {Key1, _Smaller, Larger}) when Key > Key1 ->
    case smaller_1(Key, Larger) of
        none ->
            {found, Key1};
        Found ->
            Found
    end;
smaller_1(Key, {_Key, Smaller, _Larger}) ->
    smaller_1(Key, Smaller).

-doc """
Returns `{found, Element2}`, where `Element2` is the least element strictly
greater than `Element1`.

Returns `none` if no such element exists.

## Examples

```erlang
1> S = gb_sets:from_list([10,20,30]).
2> gb_sets:larger(1, S).
{found,10}
3> gb_sets:larger(10, S).
{found,20}
4> gb_sets:larger(19, S).
{found,20}
5> gb_sets:larger(30, S).
none
```
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec larger(Element1, Set) -> none | {found, Element2} when
    Element1 :: Element,
    Element2 :: Element,
    Set :: set(Element).
larger(Key, {_, T}) ->
    larger_1(Key, T).

larger_1(_Key, nil) ->
    none;
larger_1(Key, {Key1, Smaller, _Larger}) when Key < Key1 ->
    case larger_1(Key, Smaller) of
        none ->
            {found, Key1};
        Found ->
            Found
    end;
larger_1(Key, {_Key, _Smaller, Larger}) ->
    larger_1(Key, Larger).

-doc """
Returns the elements of `Set` as an ordered list.

```erlang
1> gb_sets:to_list(gb_sets:from_list([4,3,5,1,2])).
[1,2,3,4,5]
```
""".
-spec to_list(Set) -> List when
      Set :: set(Element),
      List :: [Element].

to_list({_, T}) ->
    to_list(T, []).

to_list_1(T) -> to_list(T, []).

to_list({Key, Small, Big}, L) ->
    to_list(Small, [Key | to_list(Big, L)]);
to_list(nil, L) -> L.

-doc """
Returns an iterator that can be used for traversing the entries of `Set`; see
`next/1`.

Equivalent to [`iterator(Set, ordered)`](`iterator/2`).
""".
-spec iterator(Set) -> Iter when
      Set :: set(Element),
      Iter :: iter(Element).

iterator(Set) ->
    iterator(Set, ordered).

-doc """
Returns an iterator that can be used for traversing the entries of `Set` in
either `ordered` or `reversed` direction; see `next/1`.

The implementation is very efficient; traversing the whole set using
[`next/1`](`next/1`) is only slightly slower than getting the list of
all elements using `to_list/1` and traversing that. The main advantage
of the iterator approach is that it avoids building the complete list
of all elements to be built in memory at once.

```erlang
1> S = gb_sets:from_ordset([1,2,3,4,5]).
2> Iter0 = gb_sets:iterator(S, ordered).
3> element(1, gb_sets:next(Iter0)).
1
4> Iter1 = gb_sets:iterator(S, reversed).
5> element(1, gb_sets:next(Iter1)).
5
```
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec iterator(Set, Order) -> Iter when
      Set :: set(Element),
      Iter :: iter(Element),
      Order :: ordered | reversed.

iterator({_, T}, ordered) ->
    {ordered, iterator_1(T, [])};
iterator({_, T}, reversed) ->
    {reversed, iterator_r(T, [])}.

%% The iterator structure is really just a list corresponding to the
%% call stack of an in-order traversal. This is quite fast.

iterator_1({_, nil, _} = T, As) ->
    [T | As];
iterator_1({_, L, _} = T, As) ->
    iterator_1(L, [T | As]);
iterator_1(nil, As) ->
    As.

iterator_r({_, _, nil} = T, As) ->
    [T | As];
iterator_r({_, _, R} = T, As) ->
    iterator_r(R, [T | As]);
iterator_r(nil, As) ->
    As.

-doc """
Returns an iterator that can be used for traversing the entries of `Set`; see
`next/1`.

Unlike the iterator returned by `iterator/1` or `iterator/2`, this
iterator starts with the first element greater than or equal to
`Element`.

Equivalent to [`iterator_from(Element, Set, ordered)`](`iterator_from/3`).

## Examples

```erlang
1> S = gb_sets:from_ordset([10,20,30,40,50]).
2> Iter = gb_sets:iterator_from(17, S).
3> element(1, gb_sets:next(Iter)).
20
```
""".
-doc(#{since => <<"OTP 18.0">>}).
-spec iterator_from(Element, Set) -> Iter when
      Set :: set(Element),
      Iter :: iter(Element).

iterator_from(Element, Set) ->
    iterator_from(Element, Set, ordered).

-doc """
Returns an iterator that can be used for traversing the entries of `Set`; see
`next/1`.

Unlike the iterator returned by `iterator/1` or `iterator/2`, this
iterator starts with the first element greater than or equal to
`Element`.

## Examples

```erlang
1> S = gb_sets:from_ordset([10,20,30,40,50]).
2> Iter = gb_sets:iterator_from(17, S, reversed).
3> element(1, gb_sets:next(Iter)).
10
```
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec iterator_from(Element, Set, Order) -> Iter when
      Set :: set(Element),
      Iter :: iter(Element),
      Order :: ordered | reversed.

iterator_from(S, {_, T}, ordered) ->
    {ordered, iterator_from_1(S, T, [])};
iterator_from(S, {_, T}, reversed) ->
    {reversed, iterator_from_r(S, T, [])}.

iterator_from_1(S, {K, _, T}, As) when K < S ->
    iterator_from_1(S, T, As);
iterator_from_1(_, {_, nil, _} = T, As) ->
    [T | As];
iterator_from_1(S, {_, L, _} = T, As) ->
    iterator_from_1(S, L, [T | As]);
iterator_from_1(_, nil, As) ->
    As.

iterator_from_r(S, {K, T, _}, As) when K > S ->
    iterator_from_r(S, T, As);
iterator_from_r(_, {_, _, nil} = T, As) ->
    [T | As];
iterator_from_r(S, {_, _, R} = T, As) ->
    iterator_from_r(S, R, [T | As]);
iterator_from_r(_, nil, As) ->
    As.

-doc """
Returns `{Element, Iter2}`, where `Element` is the first element referred to
by iterator `Iter1`, and `Iter2` is the new iterator to be used for traversing
the remaining elements, or the atom `none` if no elements remain.

```erlang
1> S = gb_sets:from_ordset([1,2,3,4,5]).
2> Iter0 = gb_sets:iterator(S).
3> {Element0, Iter1} = gb_sets:next(Iter0).
4> Element0.
1
5> {Element1, Iter2} = gb_sets:next(Iter1).
6> Element1.
2
```
""".
-spec next(Iter1) -> {Element, Iter2} | 'none' when
      Iter1 :: iter(Element),
      Iter2 :: iter(Element).

next({ordered, [{X, _, T} | As]}) ->
    {X, {ordered, iterator_1(T, As)}};
next({reversed, [{X, T, _} | As]}) ->
    {X, {reversed, iterator_r(T, As)}};
next({_, []}) ->
    none.


%% Set operations:


%% If |X| < |Y|, then we traverse the elements of X. The cost for
%% testing a single random element for membership in a tree S is
%% proportional to log(|S|); thus, if |Y| / |X| < c * log(|Y|), for some
%% c, it is more efficient to scan the ordered sequence of elements of Y
%% while traversing X (under the same ordering) in order to test whether
%% elements of X are already in Y. Since the `math' module does not have
%% a `log2'-function, we rewrite the condition to |X| < |Y| * c1 *
%% ln(|X|), where c1 = c / ln 2.

-define(c, 1.46).    % 1 / ln 2; this appears to be best

%% If the sets are not very different in size, i.e., if |Y| / |X| >= c *
%% log(|Y|), then the fastest way to do union (and the other similar set
%% operations) is to build the lists of elements, traverse these lists
%% in parallel while building a reversed ackumulator list, and finally
%% rebuild the tree directly from the ackumulator. Other methods of
%% traversing the elements can be devised, but they all have higher
%% overhead.

-doc """
Returns the union of `Set1` and `Set2`.

The union of two sets is a new set that contains all the elements from
both sets, without duplicates.

## Examples

```erlang
1> S0 = gb_sets:from_list([a,b,c,d]).
2> S1 = gb_sets:from_list([c,d,e,f]).
3> Union = gb_sets:union(S0, S1).
4> gb_sets:to_list(Union).
[a,b,c,d,e,f]
```
""".
-spec union(Set1, Set2) -> Set3 when
      Set1 :: set(Element),
      Set2 :: set(Element),
      Set3 :: set(Element).

union({N1, T1}, {N2, T2}) when is_integer(N1), is_integer(N2), N2 < N1 ->
    union(to_list_1(T2), N2, T1, N1);
union({N1, T1}, {N2, T2}) when is_integer(N1), is_integer(N2) ->
    union(to_list_1(T1), N1, T2, N2).

%% We avoid the expensive mathematical computations if there is little
%% chance at saving at least the same amount of time by making the right
%% choice of strategy. Recall that N1 < N2 here.

union(L, N1, T2, N2) when N2 < 10 ->
    %% Break even is about 7 for N1 = 1 and 10 for N1 = 2
    union_2(L, to_list_1(T2), N1 + N2);
union(L, N1, T2, N2) ->
    X = N1 * round(?c * math:log(N2)),
    if N2 < X ->
	    union_2(L, to_list_1(T2), N1 + N2);
       true ->
	    union_1(L, mk_set(N2, T2))
    end.

-spec mk_set(non_neg_integer(), gb_set_node(T)) -> set(T).

mk_set(N, T) ->
    {N, T}.

%% If the length of the list is in proportion with the size of the
%% target set, this version spends too much time doing lookups, compared
%% to the below version.

union_1([X | Xs], S) ->
    union_1(Xs, add(X, S));
union_1([], S) ->
    S.


%% If the length of the first list is too small in comparison with the
%% size of the target set, this version spends too much time scanning
%% the element list of the target set for possible membership, compared
%% with the above version.

%% Some notes on sequential scanning of ordered lists
%%
%% 1) We want to put the equality case last, if we can assume that the
%% probability for overlapping elements is relatively low on average.
%% Doing this also allows us to completely skip the (arithmetic)
%% equality test, since the term order is arithmetically total.
%%
%% 2) We always test for `smaller than' first, i.e., whether the head of
%% the left list is smaller than the head of the right list, and if the
%% `greater than' test should instead turn out to be true, we switch
%% left and right arguments in the recursive call under the assumption
%% that the same is likely to apply to the next element also,
%% statistically reducing the number of failed tests and automatically
%% adapting to cases of lists having very different lengths. This saves
%% 10-40% of the traversation time compared to a "fixed" strategy,
%% depending on the sizes and contents of the lists.
%%
%% 3) A tail recursive version using `lists:reverse/2' is about 5-10%
%% faster than a plain recursive version using the stack, for lists of
%% more than about 20 elements and small stack frames. For very short
%% lists, however (length < 10), the stack version can be several times
%% faster. As stack frames grow larger, the advantages of using
%% `reverse' could get greater.

union_2(Xs, Ys, S) ->
    union_2(Xs, Ys, [], S).    % S is the sum of the sizes here

union_2([X | Xs1], [Y | _] = Ys, As, S) when X < Y ->
    union_2(Xs1, Ys, [X | As], S);
union_2([X | _] = Xs, [Y | Ys1], As, S) when X > Y ->
    union_2(Ys1, Xs, [Y | As], S);
union_2([X | Xs1], [_ | Ys1], As, S) ->
    union_2(Xs1, Ys1, [X | As], S - 1);
union_2([], Ys, As, S) ->
    {S, balance_revlist(push(Ys, As), S)};
union_2(Xs, [], As, S) ->
    {S, balance_revlist(push(Xs, As), S)}.

push([X | Xs], As) ->
    push(Xs, [X | As]);
push([], As) ->
    As.

balance_revlist(L, S) when is_integer(S) ->
    {T, _} = balance_revlist_1(L, S),
    T.

balance_revlist_1(L, S) when S > 1 ->
    Sm = S - 1,
    S2 = Sm div 2,
    S1 = Sm - S2,
    {T2, [K | L1]} = balance_revlist_1(L, S1),
    {T1, L2} = balance_revlist_1(L1, S2),
    T = {K, T1, T2},
    {T, L2};
balance_revlist_1([Key | L], 1) ->
    {{Key, nil, nil}, L};
balance_revlist_1(L, 0) ->
    {nil, L}.

-doc """
Returns the union of a list of sets.

The union of multiple sets is a new set that contains all the elements from
all sets, without duplicates.

## Examples

```erlang
1> S0 = gb_sets:from_list([a,b,c,d]).
2> S1 = gb_sets:from_list([d,e,f]).
3> S2 = gb_sets:from_list([q,r])
4> Sets = [S0, S1, S2].
5> Union = gb_sets:union(Sets).
6> gb_sets:to_list(Union).
[a,b,c,d,e,f,q,r]
```
""".
-spec union(SetList) -> Set when
      SetList :: [set(Element),...],
      Set :: set(Element).

union([S | Ss]) ->
    union_list(S, Ss);
union([]) -> empty().

union_list(S, [S1 | Ss]) ->
    union_list(union(S, S1), Ss);
union_list(S, []) -> S.


%% The rest is modelled on the above.

-doc """
Returns the intersection of `Set1` and `Set2`.

The intersection of two sets is a new set that contains only the
elements that are present in both sets.

## Examples

```erlang
1> S0 = gb_sets:from_list([a,b,c,d]).
2> S1 = gb_sets:from_list([c,d,e,f]).
3> S2 = gb_sets:from_list([q,r]).
4> gb_sets:to_list(gb_sets:intersection(S0, S1)).
[c,d]
5> gb_sets:to_list(gb_sets:intersection(S1, S2)).
[]
```
""".
-spec intersection(Set1, Set2) -> Set3 when
      Set1 :: set(Element),
      Set2 :: set(Element),
      Set3 :: set(Element).

intersection({N1, T1}, {N2, T2}) when is_integer(N1), is_integer(N2), N2 < N1 ->
    intersection(to_list_1(T2), N2, T1, N1);
intersection({N1, T1}, {N2, T2}) when is_integer(N1), is_integer(N2) ->
    intersection(to_list_1(T1), N1, T2, N2).

intersection(L, _N1, T2, N2) when N2 < 10 ->
    intersection_2(L, to_list_1(T2));
intersection(L, N1, T2, N2) ->
    X = N1 * round(?c * math:log(N2)),
    if N2 < X ->
	    intersection_2(L, to_list_1(T2));
       true ->
	    intersection_1(L, T2)
    end.

%% We collect the intersecting elements in an accumulator list and count
%% them at the same time so we can balance the list afterwards.

intersection_1(Xs, T) ->
    intersection_1(Xs, T, [], 0).

intersection_1([X | Xs], T, As, N) ->
    case is_member_1(X, T) of
	true ->
	    intersection_1(Xs, T, [X | As], N + 1);
	false ->
	    intersection_1(Xs, T, As, N)
    end;
intersection_1([], _, As, N) ->
    {N, balance_revlist(As, N)}.


intersection_2(Xs, Ys) ->
    intersection_2(Xs, Ys, [], 0).

intersection_2([X | Xs1], [Y | _] = Ys, As, S) when X < Y ->
    intersection_2(Xs1, Ys, As, S);
intersection_2([X | _] = Xs, [Y | Ys1], As, S) when X > Y ->
    intersection_2(Ys1, Xs, As, S);
intersection_2([X | Xs1], [_ | Ys1], As, S) ->
    intersection_2(Xs1, Ys1, [X | As], S + 1);
intersection_2([], _, As, S) ->
    {S, balance_revlist(As, S)};
intersection_2(_, [], As, S) ->
    {S, balance_revlist(As, S)}.

-doc """
Returns the intersection of the non-empty list of sets.

The intersection of multiple sets is a new set that contains only the
elements that are present in all sets.

## Examples

```erlang
1> S0 = gb_sets:from_list([a,b,c,d]).
2> S1 = gb_sets:from_list([d,e,f]).
3> S2 = gb_sets:from_list([q,r])
4> Sets = [S0, S1, S2].
5> gb_sets:to_list(gb_sets:intersection([S0, S1, S2])).
[]
6> gb_sets:to_list(gb_sets:intersection([S0, S1])).
[d]
7> gb_sets:intersection([]).
** exception error: no function clause matching gb_sets:intersection([])
```
""".
-spec intersection(SetList) -> Set when
      SetList :: [set(Element),...],
      Set :: set(Element).

intersection([S | Ss]) ->
    intersection_list(S, Ss).

intersection_list(S, [S1 | Ss]) ->
    intersection_list(intersection(S, S1), Ss);
intersection_list(S, []) -> S.

-doc """
Returns `true` if `Set1` and `Set2` are disjoint; otherwise, returns
`false`.

Two sets are disjoint if they have no elements in common.

This function is equivalent to `gb_sets:intersection(Set1, Set2) =:= []`,
but faster.

## Examples

```erlang
1> S0 = gb_sets:from_list([a,b,c,d]).
2> S1 = gb_sets:from_list([d,e,f]).
3> S2 = gb_sets:from_list([q,r])
4> gb_sets:is_disjoint(S0, S1).
false
5> gb_sets:is_disjoint(S1, S2).
true
```
""".
-spec is_disjoint(Set1, Set2) -> boolean() when
      Set1 :: set(Element),
      Set2 :: set(Element).

is_disjoint({N1, T1}, {N2, T2}) when N1 < N2 ->
    is_disjoint_1(T1, T2);
is_disjoint({_, T1}, {_, T2}) ->
    is_disjoint_1(T2, T1).

is_disjoint_1({K1, Smaller1, Bigger}, {K2, Smaller2, _}=Tree) when K1 < K2 ->
    not is_member_1(K1, Smaller2) andalso
	is_disjoint_1(Smaller1, Smaller2) andalso
	is_disjoint_1(Bigger, Tree);
is_disjoint_1({K1, Smaller, Bigger1}, {K2, _, Bigger2}=Tree) when K1 > K2 ->
    not is_member_1(K1, Bigger2) andalso
	is_disjoint_1(Bigger1, Bigger2) andalso
	is_disjoint_1(Smaller, Tree);
is_disjoint_1({_K1, _, _}, {_K2, _, _}) ->	%K1 == K2
    false;
is_disjoint_1(nil, _) ->
    true;
is_disjoint_1(_, nil) ->
    true.

%% Note that difference is not symmetric. We don't use `delete' here,
%% since the GB-trees implementation does not rebalance after deletion
%% and so we could end up with very unbalanced trees indeed depending on
%% the sets. Therefore, we always build a new tree, and thus we need to
%% traverse the whole element list of the left operand.

-doc """
Returns the elements of `Set1` that are not elements in `Set2`.

## Examples

```erlang
1> S0 = gb_sets:from_list([a,b,c,d]).
2> S1 = gb_sets:from_list([c,d,e,f]).
3> gb_sets:to_list(gb_sets:subtract(S0, S1)).
[a,b]
4> gb_sets:to_list(gb_sets:subtract(S1, S0)).
[e,f]
```
""".
-spec subtract(Set1, Set2) -> Set3 when
      Set1 :: set(Element),
      Set2 :: set(Element),
      Set3 :: set(Element).

subtract(S1, S2) ->
    difference(S1, S2).

-doc(#{equiv => subtract(Set1, Set2)}).
-spec difference(Set1, Set2) -> Set3 when
      Set1 :: set(Element),
      Set2 :: set(Element),
      Set3 :: set(Element).

difference({N1, T1}, {N2, T2}) when is_integer(N1), N1 >= 0,
                                    is_integer(N2), N2 >= 0 ->
    difference(to_list_1(T1), N1, T2, N2).

difference(L, N1, T2, N2) when N2 < 10 ->
    difference_2(L, to_list_1(T2), N1);
difference(L, N1, T2, N2) ->
    X = N1 * round(?c * math:log(N2)),
    if N2 < X ->
	    difference_2(L, to_list_1(T2), N1);
       true ->
	    difference_1(L, T2)
    end.


difference_1(Xs, T) ->
    difference_1(Xs, T, [], 0).

difference_1([X | Xs], T, As, N) ->
    case is_member_1(X, T) of
	true ->
	    difference_1(Xs, T, As, N);
	false ->
	    difference_1(Xs, T, [X | As], N + 1)
    end;
difference_1([], _, As, N) ->
    {N, balance_revlist(As, N)}.


difference_2(Xs, Ys, S) ->
    difference_2(Xs, Ys, [], S).    % S is the size of the left set

difference_2([X | Xs1], [Y | _] = Ys, As, S) when X < Y ->
    difference_2(Xs1, Ys, [X | As], S);
difference_2([X | _] = Xs, [Y | Ys1], As, S) when X > Y ->
    difference_2(Xs, Ys1, As, S);
difference_2([_X | Xs1], [_Y | Ys1], As, S) ->
    difference_2(Xs1, Ys1, As, S - 1);
difference_2([], _Ys, As, S) ->
    {S, balance_revlist(As, S)};
difference_2(Xs, [], As, S) ->
    {S, balance_revlist(push(Xs, As), S)}.


%% Subset testing is much the same thing as set difference, but
%% without the construction of a new set.

-doc """
Returns `true` when every element of `Set1` is also a member of `Set2`;
otherwise, returns `false`.

## Examples

```erlang
1> S0 = gb_sets:from_list([a,b,c,d]).
2> S1 = gb_sets:from_list([c,d]).
3> gb_sets:is_subset(S1, S0).
true
4> gb_sets:is_subset(S0, S1).
false
5> gb_sets:is_subset(S0, S0).
true
```
""".
-spec is_subset(Set1, Set2) -> boolean() when
      Set1 :: set(Element),
      Set2 :: set(Element).

is_subset({N1, T1}, {N2, T2}) when is_integer(N1), N1 >= 0,
                                   is_integer(N2), N2 >= 0 ->
    is_subset(to_list_1(T1), N1, T2, N2).

is_subset(L, _N1, T2, N2) when N2 < 10 ->
    is_subset_2(L, to_list_1(T2));
is_subset(L, N1, T2, N2) ->
    X = N1 * round(?c * math:log(N2)),
    if N2 < X ->
	    is_subset_2(L, to_list_1(T2));
       true ->
	    is_subset_1(L, T2)
    end.


is_subset_1([X | Xs], T) ->
    case is_member_1(X, T) of
	true ->
	    is_subset_1(Xs, T);
	false ->
	    false
    end;
is_subset_1([], _) ->
    true.


is_subset_2([X | _], [Y | _]) when X < Y ->
    false;
is_subset_2([X | _] = Xs, [Y | Ys1]) when X > Y ->
    is_subset_2(Xs, Ys1);
is_subset_2([_ | Xs1], [_ | Ys1]) ->
    is_subset_2(Xs1, Ys1);
is_subset_2([], _) ->
    true;
is_subset_2(_, []) ->
    false.


%% For compatibility with `sets':

-doc """
Returns `true` if `Term` appears to be a set; otherwise, returns `false`.

> #### Note {: .info }
>
> This function will return `true` for any term that coincides with the
> representation of a `gb_set`, while not really being a `gb_set`, thus
> it might return false positive results. See also note on [data
> types](`e:system:data_types.md#no_user_types`).
>
> Furthermore, since gb_sets are opaque, calling this function on terms
> that are not gb_sets could result in `m:dialyzer` warnings.

## Examples

```erlang
1> gb_sets:is_set(gb_sets:new()).
true
2> gb_sets:is_set(gb_sets:singleton(42)).
true
3> gb_sets:is_set(0).
false
```
""".
-spec is_set(Term) -> boolean() when
      Term :: term().

is_set({0, nil}) -> true;
is_set({N, {_, _, _}}) when is_integer(N), N >= 0 -> true;
is_set(_) -> false.

-doc """
Filters elements in `Set1` using predicate function `Pred`.

## Examples

```erlang
1> S = gb_sets:from_list([1,2,3,4,5,6,7]).
2> IsEven = fun(N) -> N rem 2 =:= 0 end.
3> Filtered = gb_sets:filter(IsEven, S).
4> gb_sets:to_list(Filtered).
[2,4,6]
```
""".
-spec filter(Pred, Set1) -> Set2 when
      Pred :: fun((Element) -> boolean()),
      Set1 :: set(Element),
      Set2 :: set(Element).

filter(F, S) when is_function(F, 1) ->
    from_ordset([X || X <- to_list(S), F(X)]).

-doc """
Maps elements in `Set1` with mapping function `Fun`.

## Examples

```erlang
1> S = gb_sets:from_list([1,2,3,4,5,6,7]).
2> F = fun(N) -> N div 2 end.
3> Mapped = gb_sets:map(F, S).
4> gb_sets:to_list(Mapped).
[0,1,2,3]
```
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec map(Fun, Set1) -> Set2 when
      Fun :: fun((Element1) -> Element2),
      Set1 :: set(Element1),
      Set2 :: set(Element2).

map(F, {_, T}) when is_function(F, 1) ->
    from_list(map_1(T, F, [])).

map_1({Key, Small, Big}, F, L) ->
    map_1(Small, F, [F(Key) | map_1(Big, F, L)]);
map_1(nil, _F, L) -> L.

-doc """
Calls `Fun(Elem)` for each `Elem` of `Set1` to update or remove
elements from `Set1`.

`Fun/1` must return either a Boolean or a tuple `{true, Value}`. The
function returns the set of elements for which `Fun` returns a new
value, with `true` being equivalent to `{true, Elem}`.

`gb_sets:filtermap/2` behaves as if it were defined as follows:

```erlang
filtermap(Fun, Set1) ->
    gb_sets:from_list(lists:filtermap(Fun, Set1)).
```

## Examples

```erlang
1> S = gb_sets:from_list([2,4,5,6,8,9])
2> F = fun(X) ->
           case X rem 2 of
               0 -> {true, X div 2};
               1 -> false
           end
        end.
3> Set = gb_sets:filtermap(F, S).
4> gb_sets:to_list(Set).
[1,2,3,4]
```
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec filtermap(Fun, Set1) -> Set2 when
      Fun :: fun((Element1) -> boolean() | {true, Element2}),
      Set1 :: set(Element1),
      Set2 :: set(Element1 | Element2).

filtermap(F, {_, T}) when is_function(F, 1) ->
    from_list(filtermap_1(T, F, [])).

filtermap_1({Key, Small, Big}, F, L) ->
    case F(Key) of
        true ->
            filtermap_1(Small, F, [Key | filtermap_1(Big, F, L)]);
        {true,Val} ->
            filtermap_1(Small, F, [Val | filtermap_1(Big, F, L)]);
        false ->
            filtermap_1(Small, F, filtermap_1(Big, F, L))
    end;
filtermap_1(nil, _F, L) -> L.

-doc """
Folds `Function` over every element in `Set` and returns the final value of
the accumulator.

## Examples

```erlang
1> S = gb_sets:from_list([1,2,3,4]).
2> Plus = fun erlang:'+'/2.
3> gb_sets:fold(Plus, 0, S).
10
```
""".
-spec fold(Function, Acc0, Set) -> Acc1 when
      Function :: fun((Element, AccIn) -> AccOut),
      Acc0 :: Acc,
      Acc1 :: Acc,
      AccIn :: Acc,
      AccOut :: Acc,
      Set :: set(Element).

fold(F, A, {_, T}) when is_function(F, 2) ->
    fold_1(F, A, T).

fold_1(F, Acc0, {Key, Small, Big}) ->
    Acc1 = fold_1(F, Acc0, Small),
    Acc = F(Key, Acc1),
    fold_1(F, Acc, Big);
fold_1(_, Acc, _) ->
    Acc.
