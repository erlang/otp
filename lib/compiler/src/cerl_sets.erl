%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2015. All Rights Reserved.
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

-module(cerl_sets).

%% Standard interface.
-export([new/0,is_set/1,size/1,to_list/1,from_list/1]).
-export([is_element/2,add_element/2,del_element/2]).
-export([union/2,union/1,intersection/2,intersection/1]).
-export([is_disjoint/2]).
-export([subtract/2,is_subset/2]).
-export([fold/3,filter/2]).

-export_type([set/0, set/1]).

%%------------------------------------------------------------------------------

-type set() :: set(_).
-opaque set(Element) :: #{Element => 'ok'}.

%%------------------------------------------------------------------------------

%% new() -> Set
-spec new() -> set().

new() -> #{}.

%% is_set(Set) -> boolean().
%%  Return 'true' if Set is a set of elements, else 'false'.
-spec is_set(Set) -> boolean() when
      Set :: term().

is_set(S) when is_map(S) -> true;
is_set(_) -> false.

%% size(Set) -> int().
%%  Return the number of elements in Set.
-spec size(Set) -> non_neg_integer() when
      Set :: set().

size(S) -> maps:size(S).

%% to_list(Set) -> [Elem].
%%  Return the elements in Set as a list.
-spec to_list(Set) -> List when
      Set :: set(Element),
      List :: [Element].

to_list(S) -> maps:keys(S).

%% from_list([Elem]) -> Set.
%%  Build a set from the elements in List.
-spec from_list(List) -> Set when
      List :: [Element],
      Set :: set(Element).
from_list(Ls) -> maps:from_list([{K,ok}||K<-Ls]).

%% is_element(Element, Set) -> boolean().
%%  Return 'true' if Element is an element of Set, else 'false'.
-spec is_element(Element, Set) -> boolean() when
      Set :: set(Element).

is_element(E,S) ->
    case S of
        #{E := _} -> true;
        _ -> false
    end.

%% add_element(Element, Set) -> Set.
%%  Return Set with Element inserted in it.
-spec add_element(Element, Set1) -> Set2 when
      Set1 :: set(Element),
      Set2 :: set(Element).

add_element(E,S) -> S#{E=>ok}.

-spec del_element(Element, Set1) -> Set2 when
      Set1 :: set(Element),
      Set2 :: set(Element).

%% del_element(Element, Set) -> Set.
%%  Return Set but with Element removed.
del_element(E,S) -> maps:remove(E,S).

%% union(Set1, Set2) -> Set
%%  Return the union of Set1 and Set2.
-spec union(Set1, Set2) -> Set3 when
      Set1 :: set(Element),
      Set2 :: set(Element),
      Set3 :: set(Element).

union(S1,S2) -> maps:merge(S1,S2).

%% union([Set]) -> Set
%%  Return the union of the list of sets.
-spec union(SetList) -> Set when
      SetList :: [set(Element)],
      Set :: set(Element).

union([S1,S2|Ss]) ->
    union1(union(S1, S2), Ss);
union([S]) -> S;
union([]) -> new().

union1(S1, [S2|Ss]) ->
    union1(union(S1, S2), Ss);
union1(S1, []) -> S1.

%% intersection(Set1, Set2) -> Set.
%%  Return the intersection of Set1 and Set2.
-spec intersection(Set1, Set2) -> Set3 when
      Set1 :: set(Element),
      Set2 :: set(Element),
      Set3 :: set(Element).

intersection(S1, S2) ->
    filter(fun (E) -> is_element(E, S1) end, S2).

%% intersection([Set]) -> Set.
%%  Return the intersection of the list of sets.
-spec intersection(SetList) -> Set when
      SetList :: [set(Element),...],
      Set :: set(Element).

intersection([S1,S2|Ss]) ->
    intersection1(intersection(S1, S2), Ss);
intersection([S]) -> S.

intersection1(S1, [S2|Ss]) ->
    intersection1(intersection(S1, S2), Ss);
intersection1(S1, []) -> S1.

%% is_disjoint(Set1, Set2) -> boolean().
%%  Check whether Set1 and Set2 are disjoint.
-spec is_disjoint(Set1, Set2) -> boolean() when
      Set1 :: set(Element),
      Set2 :: set(Element).

is_disjoint(S1, S2) when map_size(S1) > map_size(S2) ->
    is_disjoint_1(S1, maps:iterator(S2));
is_disjoint(S1, S2) ->
    is_disjoint_1(S2, maps:iterator(S1)).

is_disjoint_1(Set, Iter) ->
    case maps:next(Iter) of
        {K, _, NextIter} ->
            case Set of
                #{K := _} -> false;
                #{} -> is_disjoint_1(Set, NextIter)
            end;
        none ->
            true
    end.

%% subtract(Set1, Set2) -> Set.
%%  Return all and only the elements of Set1 which are not also in
%%  Set2.
-spec subtract(Set1, Set2) -> Set3 when
      Set1 :: set(Element),
      Set2 :: set(Element),
      Set3 :: set(Element).

subtract(S1, S2) ->
    filter(fun (E) -> not is_element(E, S2) end, S1).

%% is_subset(Set1, Set2) -> boolean().
%%  Return 'true' when every element of Set1 is also a member of
%%  Set2, else 'false'.
-spec is_subset(Set1, Set2) -> boolean() when
      Set1 :: set(Element),
      Set2 :: set(Element).

is_subset(S1, S2) when map_size(S1) > map_size(S2) ->
    false;
is_subset(S1, S2) ->
    is_subset_1(S2, maps:iterator(S1)).

is_subset_1(Set, Iter) ->
    case maps:next(Iter) of
        {K, _, NextIter} ->
            case Set of
                #{K := _} -> is_subset_1(Set, NextIter);
                #{} -> false
            end;
        none ->
            true
    end.

%% fold(Fun, Accumulator, Set) -> Accumulator.
%%  Fold function Fun over all elements in Set and return Accumulator.
-spec fold(Function, Acc0, Set) -> Acc1 when
      Function :: fun((Element, AccIn) -> AccOut),
      Set :: set(Element),
      Acc0 :: Acc,
      Acc1 :: Acc,
      AccIn :: Acc,
      AccOut :: Acc.

fold(Fun, Init, Set) ->
    fold_1(Fun, Init, maps:iterator(Set)).

fold_1(Fun, Acc, Iter) ->
    case maps:next(Iter) of
        {K, _, NextIter} ->
            fold_1(Fun, Fun(K,Acc), NextIter);
        none ->
            Acc
    end.

%% filter(Fun, Set) -> Set.
%%  Filter Set with Fun.
-spec filter(Pred, Set1) -> Set2 when
      Pred :: fun((Element) -> boolean()),
      Set1 :: set(Element),
      Set2 :: set(Element).

filter(Fun, Set) ->
    maps:from_list(filter_1(Fun, maps:iterator(Set))).

filter_1(Fun, Iter) ->
    case maps:next(Iter) of
        {K, _, NextIter} ->
            case Fun(K) of
                true ->
                    [{K,ok} | filter_1(Fun, NextIter)];
                false ->
                    filter_1(Fun, NextIter)
            end;
        none ->
            []
    end.
