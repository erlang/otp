%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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

-module(ordsets).
-moduledoc """
Functions for manipulating sets as ordered lists.

Sets are collections of elements with no duplicate elements. An `ordset` is a
representation of a set, where an ordered list is used to store the elements of
the set. An ordered list is more efficient than an unordered list. Elements are
ordered according to the _Erlang term order_.

This module provides the same interface as the `m:sets` module but with a
defined representation. One difference is that while `sets` considers two
elements as different if they do not match (`=:=`), this module considers two
elements as different if and only if they do not compare equal (`==`).

See the [Compatibility Section in the `sets` module](`m:sets#module-compatibility`)
for more information about the compatibility of the different implementations of
sets in the Standard Library.

## See Also

`m:gb_sets`, `m:sets`
""".

-export([new/0,is_set/1,size/1,is_empty/1,to_list/1,from_list/1]).
-export([is_element/2,add_element/2,del_element/2]).
-export([union/2,union/1,intersection/2,intersection/1]).
-export([is_equal/2, is_disjoint/2]).
-export([subtract/2,is_subset/2]).
-export([fold/3,filter/2,map/2,filtermap/2]).

-export_type([ordset/1]).

-doc "As returned by `new/0`.".
-type ordset(T) :: [T].

-doc """
Returns a new empty ordered set.

## Examples

```erlang
1> ordsets:new()
[]
```
""".
-spec new() -> [].

new() -> [].

-doc """
Returns `true` if `Ordset` is an ordered set of elements; otherwise,
returns `false`.

> #### Note {: .info }
>
> This function returns true for any ordered list, even if it was not
> constructed using the functions in this module.

## Examples

```erlang
1> ordsets:is_set(ordsets:from_list([a,x,13,{p,q}])).
true
2> ordsets:is_set([a,b,c]).
true
3> ordsets:is_set([z,a]).
false
4> ordsets:is_set({a,b}).
false
```
""".
-spec is_set(Ordset) -> boolean() when
      Ordset :: term().

is_set([E|Es]) -> is_set(Es, E);
is_set([]) -> true;
is_set(_) -> false.

is_set([E2|Es], E1) when E1 < E2 ->
    is_set(Es, E2);
is_set([_|_], _) -> false;
is_set([], _) -> true.

-doc """
Returns the number of elements in `Ordset`.

## Examples

```erlang
1> ordsets:size(ordsets:new()).
0
2> ordsets:size(ordsets:from_list([4,5,6])).
3
```
""".
-spec size(Ordset) -> non_neg_integer() when
      Ordset :: ordset(_).

size(S) -> length(S).

-doc """
Returns `true` if `Ordset` is an empty set; otherwise, returns `false`.

## Examples

```erlang
1> ordsets:is_empty(ordsets:new()).
true
2> ordsets:is_empty(ordsets:from_list([1])).
false
```
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec is_empty(Ordset) -> boolean() when
      Ordset :: ordset(_).

is_empty(S) -> S =:= [].

-doc """
Returns `true` if `Ordset1` and `Ordset2` are equal, that is, if every element
of one set is also a member of the other set; otherwise, returns `false`.

## Examples

```erlang
1> Empty = ordsets:new().
2> S = ordsets:from_list([a,b]).
3> ordsets:is_equal(S, S)
true
4> ordsets:is_equal(S, Empty)
false
```
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec is_equal(Ordset1, Ordset2) -> boolean() when
      Ordset1 :: ordset(_),
      Ordset2 :: ordset(_).

is_equal(S1, S2) when is_list(S1), is_list(S2) ->
    S1 == S2.

-doc """
Returns the elements of `Ordset` as a list.

## Examples

```erlang
1> S = ordsets:from_list([a,b]).
2> ordsets:to_list(S).
[a,b]
```
""".
-spec to_list(Ordset) -> List when
      Ordset :: ordset(T),
      List :: [T].

to_list(S) -> S.

-doc """
Returns an ordered set of the elements in `List`.

## Examples

```erlang
1> ordsets:from_list([a,b,a,b,b,c]).
[a,b,c]
```
""".
-spec from_list(List) -> Ordset when
      List :: [T],
      Ordset :: ordset(T).

from_list(L) ->
    lists:usort(L).

-doc """
Returns `true` if `Element` is an element of `Ordset`; otherwise, returns `false`.

## Examples

```erlang
1> S = ordsets:from_list([a,b,c]).
2> ordsets:is_element(42, S).
false
3> ordsets:is_element(b, S).
true
```
""".
-spec is_element(Element, Ordset) -> boolean() when
      Element :: term(),
      Ordset :: ordset(_).

is_element(E, [H|Es]) when E > H -> is_element(E, Es);
is_element(E, [H|_]) when E < H -> false;
is_element(_E, [_H|_]) -> true;			%E == H
is_element(_, []) -> false.

-doc """
Returns a new ordered set formed from `Ordset1` with `Element` inserted.

## Examples

```erlang
1> S0 = ordsets:new().
[]
2> S1 = ordsets:add_element(7, S0).
[7]
3> S2 = ordsets:add_element(42, S1).
[7,42]
4> ordsets:add_element(42, S2).
[7,42]
```
""".
-spec add_element(Element, Ordset1) -> Ordset2 when
      Element :: E,
      Ordset1 :: ordset(T),
      Ordset2 :: ordset(T | E).

add_element(E, [H|Es]) when E > H -> [H|add_element(E, Es)];
add_element(E, [H|_]=Set) when E < H -> [E|Set];
add_element(_E, [_H|_]=Set) -> Set;		%E == H
add_element(E, []) -> [E].

-doc """
Returns a copy of `Ordset1` with `Element` removed.

## Examples

```erlang
1> S = ordsets:from_list([a,b,c]).
2> ordsets:del_element(c, S).
[a,b]
3> ordsets:del_element(x, S).
[a,b,c]
```
""".
-spec del_element(Element, Ordset1) -> Ordset2 when
      Element :: term(),
      Ordset1 :: ordset(T),
      Ordset2 :: ordset(T).

del_element(E, [H|Es]) when E > H -> [H|del_element(E, Es)];
del_element(E, [H|_]=Set) when E < H -> Set;
del_element(_E, [_H|Es]) -> Es;			%E == H
del_element(_, []) -> [].

-doc """
Returns the union of `Ordset1` and `Ordset2`.

The union of two sets is a new set that contains all the elements from
both sets, without duplicates.

## Examples

```erlang
1> S0 = ordsets:from_list([a,b,c,d]).
2> S1 = ordsets:from_list([c,d,e,f]).
3> ordsets:union(S0, S1).
[a,b,c,d,e,f]
```
""".
-spec union(Ordset1, Ordset2) -> Ordset3 when
      Ordset1 :: ordset(T1),
      Ordset2 :: ordset(T2),
      Ordset3 :: ordset(T1 | T2).

union([E1|Es1], [E2|_]=Set2) when E1 < E2 ->
    [E1|union(Es1, Set2)];
union([E1|_]=Set1, [E2|Es2]) when E1 > E2 ->
    [E2|union(Es2, Set1)];			% switch arguments!
union([E1|Es1], [_E2|Es2]) ->			%E1 == E2
    [E1|union(Es1, Es2)];
union([], Es2) -> Es2;
union(Es1, []) -> Es1.

-doc """
Returns the union of a list of sets.

The union of multiple sets is a new set that contains all the elements from
all sets, without duplicates.

## Examples

```erlang
1> S0 = ordsets:from_list([a,b,c,d]).
2> S1 = ordsets:from_list([d,e,f]).
3> S2 = ordsets:from_list([q,r])
4> Sets = [S0, S1, S2].
5> ordsets:union(Sets).
[a,b,c,d,e,f,q,r]
```
""".
-spec union(OrdsetList) -> Ordset when
      OrdsetList :: [ordset(T)],
      Ordset :: ordset(T).

union(OrdsetList) ->
    lists:umerge(OrdsetList).

-doc """
Returns the intersection of `Ordset1` and `Ordset2`.

The intersection of two sets is a new set that contains only the
elements that are present in both sets.

## Examples

```erlang
1> S0 = ordsets:from_list([a,b,c,d]).
2> S1 = ordsets:from_list([c,d,e,f]).
3> S2 = ordsets:from_list([q,r]).
4> ordsets:intersection(S0, S1).
[c,d]
5> ordsets:intersection(S1, S2).
[]
```
""".
-spec intersection(Ordset1, Ordset2) -> Ordset3 when
      Ordset1 :: ordset(_),
      Ordset2 :: ordset(_),
      Ordset3 :: ordset(_).

intersection([E1|Es1], [E2|_]=Set2) when E1 < E2 ->
    intersection(Es1, Set2);
intersection([E1|_]=Set1, [E2|Es2]) when E1 > E2 ->
    intersection(Es2, Set1);			% switch arguments!
intersection([E1|Es1], [_E2|Es2]) ->		%E1 == E2
    [E1|intersection(Es1, Es2)];
intersection([], _) ->
    [];
intersection(_, []) ->
    [].

-doc """
Returns the intersection of the non-empty list of sets.

The intersection of multiple sets is a new set that contains only the
elements that are present in all sets.

## Examples

```erlang
1> S0 = ordsets:from_list([a,b,c,d]).
2> S1 = ordsets:from_list([d,e,f]).
3> S2 = ordsets:from_list([q,r])
4> Sets = [S0, S1, S2].
5> ordsets:intersection([S0, S1, S2]).
[]
6> ordsets:intersection([S0, S1]).
[d]
7> ordsets:intersection([]).
** exception error: no function clause matching ordsets:intersection([])
```
""".
-spec intersection(OrdsetList) -> Ordset when
      OrdsetList :: [ordset(_),...],
      Ordset :: ordset(_).

intersection([S1,S2|Ss]) ->
    intersection1(intersection(S1, S2), Ss);
intersection([S]) -> S.

intersection1(S1, [S2|Ss]) ->
    intersection1(intersection(S1, S2), Ss);
intersection1(S1, []) -> S1.

-doc """
Returns `true` if `Ordset1` and `Ordset2` are disjoint; otherwise,
returns `false`.

Two sets are disjoint if they have no elements in common.

This function is equivalent to `ordsets:intersection(Ordset1, Ordset2)
=:= []`, but faster.

## Examples

```erlang
1> S0 = ordsets:from_list([a,b,c,d]).
2> S1 = ordsets:from_list([d,e,f]).
3> S2 = ordsets:from_list([q,r])
4> ordsets:is_disjoint(S0, S1).
false
5> ordsets:is_disjoint(S1, S2).
true
```
""".
-spec is_disjoint(Ordset1, Ordset2) -> boolean() when
      Ordset1 :: ordset(_),
      Ordset2 :: ordset(_).

is_disjoint([E1|Es1], [E2|_]=Set2) when E1 < E2 ->
    is_disjoint(Es1, Set2);
is_disjoint([E1|_]=Set1, [E2|Es2]) when E1 > E2 ->
    is_disjoint(Es2, Set1);			% switch arguments!
is_disjoint([_E1|_Es1], [_E2|_Es2]) ->		%E1 == E2
    false;
is_disjoint([], _) ->
    true;
is_disjoint(_, []) ->
    true.

-doc """
Returns the elements of `Ordset1` that are not elements in `Ordset2`.

## Examples

```erlang
1> S0 = ordsets:from_list([a,b,c,d]).
2> S1 = ordsets:from_list([c,d,e,f]).
3> ordsets:subtract(S0, S1).
[a,b]
4> ordsets:subtract(S1, S0).
[e,f]
```
""".
-spec subtract(Ordset1, Ordset2) -> Ordset3 when
      Ordset1 :: ordset(_),
      Ordset2 :: ordset(_),
      Ordset3 :: ordset(_).

subtract([E1|Es1], [E2|_]=Set2) when E1 < E2 ->
    [E1|subtract(Es1, Set2)];
subtract([E1|_]=Set1, [E2|Es2]) when E1 > E2 ->
    subtract(Set1, Es2);
subtract([_E1|Es1], [_E2|Es2]) ->		%E1 == E2
    subtract(Es1, Es2);
subtract([], _) -> [];
subtract(Es1, []) -> Es1.

-doc """
Returns `true` when every element of `Ordset1` is also a member of `Ordset2`;
otherwise, returns `false`.

## Examples

```erlang
1> S0 = ordsets:from_list([a,b,c,d]).
2> S1 = ordsets:from_list([c,d]).
3> ordsets:is_subset(S1, S0).
true
4> ordsets:is_subset(S0, S1).
false
5> ordsets:is_subset(S0, S0).
true
```
""".
-spec is_subset(Ordset1, Ordset2) -> boolean() when
      Ordset1 :: ordset(_),
      Ordset2 :: ordset(_).

is_subset([E1|_], [E2|_]) when E1 < E2 ->	%E1 not in Set2
    false;
is_subset([E1|_]=Set1, [E2|Es2]) when E1 > E2 ->
    is_subset(Set1, Es2);
is_subset([_E1|Es1], [_E2|Es2]) ->		%E1 == E2
    is_subset(Es1, Es2);
is_subset([], _) -> true;
is_subset(_, []) -> false.

-doc """
Folds `Function` over every element in `Ordset` and returns the final value of
the accumulator.

## Examples

```erlang
1> S = ordsets:from_list([1,2,3,4]).
2> Plus = fun erlang:'+'/2.
3> ordsets:fold(Plus, 0, S).
10
```
""".
-spec fold(Function, Acc0, Ordset) -> Acc1 when
      Function :: fun((Element :: T, AccIn :: term()) -> AccOut :: term()),
      Ordset :: ordset(T),
      Acc0 :: term(),
      Acc1 :: term().

fold(F, Acc, Set) ->
    lists:foldl(F, Acc, Set).

-doc """
Filters elements in `Ordset1` using predicate function `Pred`.

## Examples

```erlang
1> S = ordsets:from_list([1,2,3,4,5,6,7]).
2> IsEven = fun(N) -> N rem 2 =:= 0 end.
3> ordsets:filter(IsEven, S).
[2,4,6]
```
""".
-spec filter(Pred, Ordset1) -> Ordset2 when
      Pred :: fun((Element :: T) -> boolean()),
      Ordset1 :: ordset(T),
      Ordset2 :: ordset(T).

filter(F, Set) ->
    lists:filter(F, Set).

-doc """
Maps elements in `Ordset1` with mapping function `Fun`.

## Examples

```erlang
1> S = ordsets:from_list([1,2,3,4,5,6,7]).
2> F = fun(N) -> N div 2 end.
3> ordsets:map(F, S).
[0,1,2,3]
```
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec map(Fun, Ordset1) -> Ordset2 when
    Fun :: fun((Element1 :: T1) -> Element2 :: T2),
    Ordset1 :: ordset(T1),
    Ordset2 :: ordset(T2).

map(F, Set) ->
    from_list(lists:map(F, Set)).

-doc """
Calls `Fun(Elem)` for each `Elem` of `Ordset1` to update or remove
elements from `Ordset1`.

`Fun/1` must return either a Boolean or a tuple `{true, Value}`. The
function returns the set of elements for which `Fun` returns a new
value, with `true` being equivalent to `{true, Elem}`.

`ordsets:filtermap/2` behaves as if it were defined as follows:

```erlang
filtermap(Fun, Ordset1) ->
    ordsets:from_list(lists:filtermap(Fun, Ordset1)).
```

## Examples

```erlang
1> S = ordsets:from_list([2,4,5,6,8,9])
2> F = fun(X) ->
           case X rem 2 of
               0 -> {true, X div 2};
               1 -> false
           end
        end.
3> ordsets:filtermap(F, S).
[1,2,3,4]
```
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec filtermap(Fun, Ordset1) -> Ordset2 when
      Fun :: fun((Element1 :: T1) -> boolean | ({true, Element2 :: T2})),
      Ordset1 :: ordset(T1),
      Ordset2 :: ordset(T1 | T2).

filtermap(F, Set) ->
    from_list(lists:filtermap(F, Set)).
