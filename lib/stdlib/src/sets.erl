%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2024. All Rights Reserved.
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

%% The new version 2 has moved to use maps under the roof whenever a
%% map is given.

%% The previous version (version 1) uses the dynamic hashing techniques
%% by Per-Åke Larsson as described in "The Design and Implementation
%% of Dynamic Hashing for Sets and Tables in Icon" by Griswold and
%% Townsend.  Much of the terminology comes from that paper as well.

%% The segments are all of the same fixed size and we just keep
%% increasing the size of the top tuple as the table grows.  At the
%% end of the segments tuple we keep an empty segment which we use
%% when we expand the segments.  The segments are expanded by doubling
%% every time n reaches maxn instead of increasing the tuple one
%% element at a time.  It is easier and does not seem detrimental to
%% speed.  The same applies when contracting the segments.
%%
%% Note that as the order of the keys is undefined we may freely
%% reorder keys within in a bucket.

-module(sets).
-moduledoc """
Sets are collections of elements with no duplicate elements.

The data representing a set as used by this module is to be regarded as opaque
by other modules. In abstract terms, the representation is a composite type of
existing Erlang terms. See note on
[data types](`e:system:data_types.md#no_user_types`). Any code assuming
knowledge of the format is running on thin ice.

This module provides the same interface as the `m:ordsets` module but with an
undefined representation. One difference is that while this module considers two
elements as different if they do not match (`=:=`), `ordsets` considers two
elements as different if and only if they do not compare equal (`==`).

Erlang/OTP 24.0 introduced a new internal representation for sets which is more
performant. Developers can use this new representation by passing the
`{version, 2}` flag to `new/1` and `from_list/2`, such as
`sets:new([{version, 2}])`. This new representation will become the default in
future Erlang/OTP versions. Functions that work on two sets, such as `union/2`
and similar, will work with sets of different versions. In such cases, there is
no guarantee about the version of the returned set. Explicit conversion from the
old version to the new one can be done with
`sets:from_list(sets:to_list(Old), [{version,2}])`.

## Compatibility

The following functions in this module also exist and provide the same
functionality in the `m:gb_sets` and `m:ordsets` modules. That is, by only
changing the module name for each call, you can try out different set
representations.

- `add_element/2`
- `del_element/2`
- `filter/2`
- `filtermap/2`
- `fold/3`
- `from_list/1`
- `intersection/1`
- `intersection/2`
- `is_element/2`
- `is_empty/1`
- `is_equal/2`
- `is_set/1`
- `is_subset/2`
- `map/2`
- `new/0`
- `size/1`
- `subtract/2`
- `to_list/1`
- `union/1`
- `union/2`

> #### Note {: .info }
>
> While the three set implementations offer the same _functionality_ with
> respect to the aforementioned functions, their overall _behavior_ may differ.
> As mentioned, this module considers elements as different if and only if they
> do not match (`=:=`), while both `m:ordsets` and `m:gb_sets` consider elements
> as different if and only if they do not compare equal (`==`).
>
> _Example:_
>
> ```erlang
> 1> sets:is_element(1.0, sets:from_list([1])).
> false
> 2> ordsets:is_element(1.0, ordsets:from_list([1])).
> true
> 2> gb_sets:is_element(1.0, gb_sets:from_list([1])).
> true
> ```

## See Also

`m:gb_sets`, `m:ordsets`
""".
-compile([{nowarn_deprecated_function, [{erlang,phash,2}]}]).

%% Standard interface.
-export([new/0,is_set/1,size/1,is_empty/1,to_list/1,from_list/1]).
-export([is_element/2,add_element/2,del_element/2]).
-export([union/2,union/1,intersection/2,intersection/1]).
-export([is_equal/2, is_disjoint/2]).
-export([subtract/2,is_subset/2]).
-export([fold/3,filter/2,map/2,filtermap/2]).
-export([new/1, from_list/2]).

-export_type([set/0, set/1]).

%% This is the value used when sets are represented as maps.
%% We use an empty list instead of an atom as it is cheaper
%% to serialize.
-define(VALUE, []).

%% Note: mk_seg/1 must be changed too if seg_size is changed.
-define(seg_size, 16).
-define(max_seg, 32).
-define(expand_load, 5).
-define(contract_load, 3).
-define(exp_size, ?seg_size * ?expand_load).
-define(con_size, ?seg_size * ?contract_load).
-compile({no_auto_import,[size/1]}).

%%------------------------------------------------------------------------------

-type seg()          :: tuple().
-type segs(_Element) :: tuple().

%% Define a hash set.  The default values are the standard ones.
-record(set,
	{size=0              :: non_neg_integer(),	% Number of elements
	 n=?seg_size         :: non_neg_integer(),	% Number of active slots
	 maxn=?seg_size      :: pos_integer(),  	% Maximum slots
	 bso=?seg_size div 2 :: non_neg_integer(),      % Buddy slot offset
	 exp_size=?exp_size  :: non_neg_integer(),	% Size to expand at
	 con_size=?con_size  :: non_neg_integer(),	% Size to contract at
	 empty               :: seg(),			% Empty segment
	 segs                :: segs(_)			% Segments
	}).

-type set() :: set(_).

-doc "As returned by `new/0`.".
-opaque set(Element) :: #set{segs :: segs(Element)} | #{Element => ?VALUE}.

%%------------------------------------------------------------------------------

%% new() -> Set
-doc "Returns a new empty set.".
-spec new() -> set(none()).
new() ->
    Empty = mk_seg(?seg_size),
    #set{empty = Empty, segs = {Empty}}.

-doc "Returns a new empty set at the given version.".
-doc(#{since => <<"OTP 24.0">>}).
-spec new([{version, 1..2}]) -> set(none()).
new([{version, 2}]) ->
    #{};
new(Opts) ->
    case proplists:get_value(version, Opts, 1) of
        1 -> new();
        2 -> new([{version, 2}])
    end.

%% from_list([Elem]) -> Set.
%%  Build a set from the elements in List.
-doc "Returns a set of the elements in `List`.".
-spec from_list(List) -> Set when
      List :: [Element],
      Set :: set(Element).
from_list(Ls) ->
    lists:foldl(fun (E, S) -> add_element(E, S) end, new(), Ls).

-doc "Returns a set of the elements in `List` at the given version.".
-doc(#{since => <<"OTP 24.0">>}).
-spec from_list(List, [{version, 1..2}]) -> Set when
      List :: [Element],
      Set :: set(Element).
from_list(Ls, [{version, 2}]) ->
    maps:from_keys(Ls, ?VALUE);
from_list(Ls, Opts) ->
    case proplists:get_value(version, Opts, 1) of
        1 -> from_list(Ls);
        2 -> from_list(Ls, [{version, 2}])
    end.

%%------------------------------------------------------------------------------

%% is_set(Set) -> boolean().
%%  Return 'true' if Set is a set of elements, else 'false'.
-doc """
Returns `true` if `Set` appears to be a set of elements, otherwise `false`.

Note that the test is shallow and will return `true` for any term that coincides with
the possible representations of a set. See also note on [data types](`e:system:data_types.md#no_user_types`).
""".
-spec is_set(Set) -> boolean() when
      Set :: term().
is_set(#{}) -> true;
is_set(#set{}) -> true;
is_set(_) -> false.

%% size(Set) -> int().
%%  Return the number of elements in Set.
-doc "Returns the number of elements in `Set`.".
-spec size(Set) -> non_neg_integer() when
      Set :: set().
size(#{}=S) -> map_size(S);
size(#set{size=Size}) -> Size.

%% is_empty(Set) -> boolean().
%%  Return 'true' if Set is an empty set, otherwise 'false'.
-doc "Returns `true` if `Set` is an empty set, otherwise `false`.".
-doc(#{since => <<"OTP 21.0">>}).
-spec is_empty(Set) -> boolean() when
      Set :: set().
is_empty(#{}=S) -> map_size(S)=:=0;
is_empty(#set{size=Size}) -> Size=:=0.

%% is_equal(Set1, Set2) -> boolean().
%%  Return 'true' if Set1 and Set2 contain the same elements,
%%  otherwise 'false'.
-doc """
Returns `true` if `Set1` and `Set2` are equal, that is when every element of one
set is also a member of the respective other set, otherwise `false`.
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec is_equal(Set1, Set2) -> boolean() when
      Set1 :: set(),
      Set2 :: set().
is_equal(S1, S2) ->
    case size(S1) =:= size(S2) of
        true when S1 =:= S2 ->
            true;
        true ->
            canonicalize_v2(S1) =:= canonicalize_v2(S2);
        false ->
            false
    end.

canonicalize_v2(S) ->
    from_list(to_list(S), [{version, 2}]).

%% to_list(Set) -> [Elem].
%%  Return the elements in Set as a list.
-doc """
Returns the elements of `Set` as a list. The order of the returned elements is
undefined.
""".
-spec to_list(Set) -> List when
      Set :: set(Element),
      List :: [Element].
to_list(#{}=S) ->
    maps:keys(S);
to_list(#set{} = S) ->
    fold(fun (Elem, List) -> [Elem|List] end, [], S).

%% is_element(Element, Set) -> boolean().
%%  Return 'true' if Element is an element of Set, else 'false'.
-doc "Returns `true` if `Element` is an element of `Set`, otherwise `false`.".
-spec is_element(Element, Set) -> boolean() when
      Set :: set(Element).
is_element(E, #{}=S) ->
    case S of
        #{E := _} -> true;
        _ -> false
    end;
is_element(E, #set{}=S) ->
    Slot = get_slot(S, E),
    Bkt = get_bucket(S, Slot),
    lists:member(E, Bkt).

%% add_element(Element, Set) -> Set.
%%  Return Set with Element inserted in it.
-doc "Returns a new set formed from `Set1` with `Element` inserted.".
-spec add_element(Element, Set1) -> Set2 when
      Set1 :: set(Element),
      Set2 :: set(Element).
add_element(E, #{}=S) ->
    S#{E=>?VALUE};
add_element(E, #set{}=S0) ->
    Slot = get_slot(S0, E),
    Bkt = get_bucket(S0, Slot),
    case lists:member(E, Bkt) of
        true ->
            S0;
        false ->
            S1 = update_bucket(S0, Slot, [E | Bkt]),
            maybe_expand(S1)
    end.

%% del_element(Element, Set) -> Set.
%%  Return Set but with Element removed.
-doc "Returns `Set1`, but with `Element` removed.".
-spec del_element(Element, Set1) -> Set2 when
      Set1 :: set(Element),
      Set2 :: set(Element).
del_element(E, #{}=S) ->
    maps:remove(E, S);
del_element(E, #set{}=S0) ->
    Slot = get_slot(S0, E),
    Bkt = get_bucket(S0, Slot),
    case lists:member(E, Bkt) of
        false ->
            S0;
        true ->
            S1 = update_bucket(S0, Slot, lists:delete(E, Bkt)),
            maybe_contract(S1, 1)
    end.

%% update_bucket(Set, Slot, NewBucket) -> UpdatedSet.
%%  Replace bucket in Slot by NewBucket
-spec update_bucket(Set1, Slot, Bkt) -> Set2 when
      Set1 :: set(Element),
      Set2 :: set(Element),
      Slot :: non_neg_integer(),
      Bkt :: [Element].
update_bucket(Set, Slot, NewBucket) ->
    SegI = ((Slot-1) div ?seg_size) + 1,
    BktI = ((Slot-1) rem ?seg_size) + 1,
    Segs = Set#set.segs,
    Seg = element(SegI, Segs),
    Set#set{segs = setelement(SegI, Segs, setelement(BktI, Seg, NewBucket))}.

%% union(Set1, Set2) -> Set
%%  Return the union of Set1 and Set2.
-doc "Returns the merged (union) set of `Set1` and `Set2`.".
-spec union(Set1, Set2) -> Set3 when
      Set1 :: set(Element),
      Set2 :: set(Element),
      Set3 :: set(Element).
union(#{}=S1, #{}=S2) ->
    maps:merge(S1,S2);
union(S1, S2) ->
    case size(S1) < size(S2) of
	true ->
	    fold(fun (E, S) -> add_element(E, S) end, S2, S1);
	false ->
	    fold(fun (E, S) -> add_element(E, S) end, S1, S2)
    end.

%% union([Set]) -> Set
%%  Return the union of the list of sets.
-doc "Returns the merged (union) set of the list of sets.".
-spec union(SetList) -> Set when
      SetList :: [set(Element)],
      Set :: set(Element).
union([S1,S2|Ss]) ->
    union1(union(S1, S2), Ss);
union([S]) -> S;
union([]) -> new().

-spec union1(set(E), [set(E)]) -> set(E).
union1(S1, [S2|Ss]) ->
    union1(union(S1, S2), Ss);
union1(S1, []) -> S1.

%% intersection(Set1, Set2) -> Set.
%%  Return the intersection of Set1 and Set2.
-doc "Returns the intersection of `Set1` and `Set2`.".
-spec intersection(Set1, Set2) -> Set3 when
      Set1 :: set(Element),
      Set2 :: set(Element),
      Set3 :: set(Element).
intersection(#{}=S1, #{}=S2) ->
    case map_size(S1) < map_size(S2) of
        true ->
            Next = maps:next(maps:iterator(S1)),
            intersection_heuristic(Next, [], [], floor(map_size(S1) * 0.75), S1, S2);
        false ->
            Next = maps:next(maps:iterator(S2)),
            intersection_heuristic(Next, [], [], floor(map_size(S2) * 0.75), S2, S1)
    end;
intersection(S1, S2) ->
    case size(S1) < size(S2) of
        true ->
	    filter(fun (E) -> is_element(E, S2) end, S1);
        false ->
	    filter(fun (E) -> is_element(E, S1) end, S2)
    end.

%% If we are keeping more than 75% of the keys, then it is
%% cheaper to delete them. Stop accumulating and start deleting.
intersection_heuristic(Next, _Keep, Delete, 0, Acc, Reference) ->
  intersection_decided(Next, remove_keys(Delete, Acc), Reference);
intersection_heuristic({Key, _Value, Iterator}, Keep, Delete, KeepCount, Acc, Reference) ->
    Next = maps:next(Iterator),
    case Reference of
        #{Key := _} ->
            intersection_heuristic(Next, [Key | Keep], Delete, KeepCount - 1, Acc, Reference);
        _ ->
            intersection_heuristic(Next, Keep, [Key | Delete], KeepCount, Acc, Reference)
    end;
intersection_heuristic(none, Keep, _Delete, _Count, _Acc, _Reference) ->
    maps:from_keys(Keep, ?VALUE).

intersection_decided({Key, _Value, Iterator}, Acc0, Reference) ->
    Acc1 = case Reference of
        #{Key := _} -> Acc0;
        #{} -> maps:remove(Key, Acc0)
    end,
    intersection_decided(maps:next(Iterator), Acc1, Reference);
intersection_decided(none, Acc, _Reference) ->
    Acc.

remove_keys([K | Ks], Map) -> remove_keys(Ks, maps:remove(K, Map));
remove_keys([], Map) -> Map.

%% intersection([Set]) -> Set.
%%  Return the intersection of the list of sets.
-doc "Returns the intersection of the non-empty list of sets.".
-spec intersection(SetList) -> Set when
      SetList :: [set(Element),...],
      Set :: set(Element).
intersection([S1,S2|Ss]) ->
    intersection1(intersection(S1, S2), Ss);
intersection([S]) -> S.

-spec intersection1(set(E), [set(E)]) -> set(E).
intersection1(S1, [S2|Ss]) ->
    intersection1(intersection(S1, S2), Ss);
intersection1(S1, []) -> S1.

%% is_disjoint(Set1, Set2) -> boolean().
%%  Check whether Set1 and Set2 are disjoint.
-doc """
Returns `true` if `Set1` and `Set2` are disjoint (have no elements in common),
otherwise `false`.
""".
-spec is_disjoint(Set1, Set2) -> boolean() when
      Set1 :: set(Element),
      Set2 :: set(Element).
is_disjoint(#{}=S1, #{}=S2) ->
    if
	map_size(S1) < map_size(S2) ->
	    is_disjoint_1(S2, maps:iterator(S1));
	true ->
	    is_disjoint_1(S1, maps:iterator(S2))
    end;
is_disjoint(S1, S2) ->
    case size(S1) < size(S2) of
        true ->
	    fold(fun (_, false) -> false;
		(E, true) -> not is_element(E, S2)
	    end, true, S1);
        false ->
	    fold(fun (_, false) -> false;
		(E, true) -> not is_element(E, S1)
	    end, true, S2)
    end.

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
-doc "Returns only the elements of `Set1` that are not also elements of `Set2`.".
-spec subtract(Set1, Set2) -> Set3 when
      Set1 :: set(Element),
      Set2 :: set(Element),
      Set3 :: set(Element).

subtract(#{}=LHS, #{}=RHS) ->
    LSize = map_size(LHS),
    RSize = map_size(RHS),

    case RSize =< (LSize div 4) of
        true ->
            %% If we're guaranteed to keep more than 75% of the keys, it's
            %% always cheaper to delete them one-by-one from the start.
            Next = maps:next(maps:iterator(RHS)),
            subtract_decided(Next, LHS, RHS);
        false ->
            %% We might delete more than 25% of the keys. Dynamically
            %% transition to deleting elements one-by-one if we can determine
            %% that we'll keep more than 75%.
            KeepThreshold = (LSize * 3) div 4,
            Next = maps:next(maps:iterator(LHS)),
            subtract_heuristic(Next, [], [], KeepThreshold, LHS, RHS)
    end;
subtract(LHS, RHS) ->
    filter(fun (E) -> not is_element(E, RHS) end, LHS).

subtract_heuristic(Next, _Keep, Delete, 0, Acc, Reference) ->
    %% We've kept more than 75% of the keys, transition to removing them
    %% one-by-one.
    subtract_decided(Next, remove_keys(Delete, Acc), Reference);
subtract_heuristic({Key, _Value, Iterator}, Keep, Delete,
                   KeepCount, Acc, Reference) ->
    Next = maps:next(Iterator),
    case Reference of
        #{ Key := _ } ->
            subtract_heuristic(Next, Keep, [Key | Delete],
                               KeepCount, Acc, Reference);
        _ ->
            subtract_heuristic(Next, [Key | Keep], Delete,
                               KeepCount - 1, Acc, Reference)
    end;
subtract_heuristic(none, Keep, _Delete, _Count, _Acc, _Reference) ->
    maps:from_keys(Keep, ?VALUE).

subtract_decided({Key, _Value, Iterator}, Acc, Reference) ->
    case Reference of
        #{ Key := _ } ->
            subtract_decided(maps:next(Iterator),
                             maps:remove(Key, Acc),
                             Reference);
        _ ->
            subtract_decided(maps:next(Iterator), Acc, Reference)
    end;
subtract_decided(none, Acc, _Reference) ->
    Acc.

%% is_subset(Set1, Set2) -> boolean().
%%  Return 'true' when every element of Set1 is also a member of
%%  Set2, else 'false'.
-doc """
Returns `true` when every element of `Set1` is also a member of `Set2`,
otherwise `false`.
""".
-spec is_subset(Set1, Set2) -> boolean() when
      Set1 :: set(Element),
      Set2 :: set(Element).

is_subset(#{}=S1, #{}=S2) ->
    if
	map_size(S1) > map_size(S2) ->
	    false;
	true ->
	    is_subset_1(S2, maps:iterator(S1))
    end;
is_subset(S1, S2) ->
    fold(fun (E, Sub) -> Sub andalso is_element(E, S2) end, true, S1).

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
-doc """
Folds `Function` over every element in `Set` and returns the final value of the
accumulator. The evaluation order is undefined.
""".
-spec fold(Function, Acc0, Set) -> Acc1 when
      Function :: fun((Element, AccIn) -> AccOut),
      Set :: set(Element),
      Acc0 :: Acc,
      Acc1 :: Acc,
      AccIn :: Acc,
      AccOut :: Acc.
fold(F, Acc, #{}=D) when is_function(F, 2)->
    fold_1(F, Acc, maps:iterator(D));
fold(F, Acc, #set{}=D) when is_function(F, 2)->
    fold_set(F, Acc, D).

fold_1(Fun, Acc, Iter) ->
    case maps:next(Iter) of
        {K, _, NextIter} ->
            fold_1(Fun, Fun(K,Acc), NextIter);
        none ->
            Acc
    end.

%% filter(Fun, Set) -> Set.
%%  Filter Set with Fun.
-doc "Filters elements in `Set1` with boolean function `Pred`.".
-spec filter(Pred, Set1) -> Set2 when
      Pred :: fun((Element) -> boolean()),
      Set1 :: set(Element),
      Set2 :: set(Element).
filter(F, #{}=D) when is_function(F, 1)->
    %% For this purpose, it is more efficient to use
    %% maps:from_keys than a map comprehension.
    maps:from_keys([K || K := _ <- D, F(K)], ?VALUE);
filter(F, #set{}=D) when is_function(F, 1)->
    filter_set(F, D).

%% map(Fun, Set) -> Set.
%%  Map Set with Map.
-doc "Maps elements in `Set1` with mapping function `Fun`.".
-doc(#{since => <<"OTP 27.0">>}).
-spec map(Fun, Set1) -> Set2 when
      Fun :: fun((Element1) -> Element2),
      Set1 :: set(Element1),
      Set2 :: set(Element2).
map(F, #{}=D) when is_function(F, 1) ->
    %% For this purpose, it is more efficient to use
    %% maps:from_keys than a map comprehension.
    maps:from_keys([F(K) || K := _ <- D], ?VALUE);
map(F, #set{}=D) when is_function(F, 1) ->
    fold(fun(E, Acc) -> add_element(F(E), Acc) end,
         sets:new([{version, 1}]),
         D).

%% filtermap(Fun, Set) -> Set.
%%  Filter and map Set with Fun.
-doc "Filters and maps elements in `Set1` with function `Fun`.".
-doc(#{since => <<"OTP 27.0">>}).
-spec filtermap(Fun, Set1) -> Set2 when
      Fun :: fun((Element1) -> boolean() | {true, Element2}),
      Set1 :: set(Element1),
      Set2 :: set(Element1 | Element2).
filtermap(F, #{}=D) when is_function(F, 1) ->
    maps:from_keys(lists:filtermap(F, to_list(D)), ?VALUE);
filtermap(F, #set{}=D) when is_function(F, 1) ->
    fold(fun(E0, Acc) ->
             case F(E0) of
                 true -> add_element(E0, Acc);
                 {true, E1} -> add_element(E1, Acc);
                 false -> Acc
             end
         end,
         sets:new([{version, 1}]),
         D).

%% get_slot(Hashdb, Key) -> Slot.
%%  Get the slot.  First hash on the new range, if we hit a bucket
%%  which has not been split use the unsplit buddy bucket.
-spec get_slot(set(E), E) -> non_neg_integer().
get_slot(T, Key) ->
    H = erlang:phash(Key, T#set.maxn),
    if
	H > T#set.n -> H - T#set.bso;
	true -> H
    end.

%% get_bucket(Hashdb, Slot) -> Bucket.
-spec get_bucket(set(), non_neg_integer()) -> term().
get_bucket(T, Slot) -> get_bucket_s(T#set.segs, Slot).

%% fold_set(Fun, Acc, Dictionary) -> Dictionary.
%% filter_set(Fun, Dictionary) -> Dictionary.

%%  Work functions for fold and filter operations.  These traverse the
%%  hash structure rebuilding as necessary.  Note we could have
%%  implemented map and hash using fold but these should be faster.
%%  We hope!

fold_set(F, Acc, D) ->
    Segs = D#set.segs,
    fold_segs(F, Acc, Segs, tuple_size(Segs)).

fold_segs(F, Acc, Segs, I) when I >= 1 ->
    Seg = element(I, Segs),
    fold_segs(F, fold_seg(F, Acc, Seg, tuple_size(Seg)), Segs, I-1);
fold_segs(_, Acc, _, _) -> Acc.

fold_seg(F, Acc, Seg, I) when I >= 1 ->
    fold_seg(F, fold_bucket(F, Acc, element(I, Seg)), Seg, I-1);
fold_seg(_, Acc, _, _) -> Acc.

fold_bucket(F, Acc, [E|Bkt]) ->
    fold_bucket(F, F(E, Acc), Bkt);
fold_bucket(_, Acc, []) -> Acc.

filter_set(F, D) ->
    Segs0 = tuple_to_list(D#set.segs),
    {Segs1,Fc} = filter_seg_list(F, Segs0, [], 0),
    maybe_contract(D#set{segs = list_to_tuple(Segs1)}, Fc).

filter_seg_list(F, [Seg|Segs], Fss, Fc0) ->
    Bkts0 = tuple_to_list(Seg),
    {Bkts1,Fc1} = filter_bkt_list(F, Bkts0, [], Fc0),
    filter_seg_list(F, Segs, [list_to_tuple(Bkts1)|Fss], Fc1);
filter_seg_list(_, [], Fss, Fc) ->
    {lists:reverse(Fss, []),Fc}.

filter_bkt_list(F, [Bkt0|Bkts], Fbs, Fc0) ->
    {Bkt1,Fc1} = filter_bucket(F, Bkt0, [], Fc0),
    filter_bkt_list(F, Bkts, [Bkt1|Fbs], Fc1);
filter_bkt_list(_, [], Fbs, Fc) ->
    {lists:reverse(Fbs),Fc}.

filter_bucket(F, [E|Bkt], Fb, Fc) ->
    case F(E) of
	true -> filter_bucket(F, Bkt, [E|Fb], Fc);
	false -> filter_bucket(F, Bkt, Fb, Fc+1)
    end;
filter_bucket(_, [], Fb, Fc) -> {Fb,Fc}.

%% get_bucket_s(Segments, Slot) -> Bucket.
%% put_bucket_s(Segments, Slot, Bucket) -> NewSegments.

get_bucket_s(Segs, Slot) ->
    SegI = ((Slot-1) div ?seg_size) + 1,
    BktI = ((Slot-1) rem ?seg_size) + 1,
    element(BktI, element(SegI, Segs)).

put_bucket_s(Segs, Slot, Bkt) ->
    SegI = ((Slot-1) div ?seg_size) + 1,
    BktI = ((Slot-1) rem ?seg_size) + 1,
    Seg = setelement(BktI, element(SegI, Segs), Bkt),
    setelement(SegI, Segs, Seg).

-spec maybe_expand(set(E)) -> set(E).
maybe_expand(T0) when T0#set.size + 1 > T0#set.exp_size ->
    T = maybe_expand_segs(T0),			%Do we need more segments.
    N = T#set.n + 1,				%Next slot to expand into
    Segs0 = T#set.segs,
    Slot1 = N - T#set.bso,
    B = get_bucket_s(Segs0, Slot1),
    Slot2 = N,
    {B1,B2} = rehash(B, Slot1, Slot2, T#set.maxn),
    Segs1 = put_bucket_s(Segs0, Slot1, B1),
    Segs2 = put_bucket_s(Segs1, Slot2, B2),
    T#set{size = T#set.size + 1,
	  n = N,
	  exp_size = N * ?expand_load,
	  con_size = N * ?contract_load,
	  segs = Segs2};
maybe_expand(T) -> T#set{size = T#set.size + 1}.

-spec maybe_expand_segs(set(E)) -> set(E).
maybe_expand_segs(T) when T#set.n =:= T#set.maxn ->
    T#set{maxn = 2 * T#set.maxn,
	  bso  = 2 * T#set.bso,
	  segs = expand_segs(T#set.segs, T#set.empty)};
maybe_expand_segs(T) -> T.

-spec maybe_contract(set(E), non_neg_integer()) -> set(E).
maybe_contract(T, Dc) when T#set.size - Dc < T#set.con_size,
			   T#set.n > ?seg_size ->
    N = T#set.n,
    Slot1 = N - T#set.bso,
    Segs0 = T#set.segs,
    B1 = get_bucket_s(Segs0, Slot1),
    Slot2 = N,
    B2 = get_bucket_s(Segs0, Slot2),
    Segs1 = put_bucket_s(Segs0, Slot1, B1 ++ B2),
    Segs2 = put_bucket_s(Segs1, Slot2, []),	%Clear the upper bucket
    N1 = N - 1,
    maybe_contract_segs(T#set{size = T#set.size - Dc,
			      n = N1,
			      exp_size = N1 * ?expand_load,
			      con_size = N1 * ?contract_load,
			      segs = Segs2});
maybe_contract(T, Dc) -> T#set{size = T#set.size - Dc}.

-spec maybe_contract_segs(set(E)) -> set(E).
maybe_contract_segs(T) when T#set.n =:= T#set.bso ->
    T#set{maxn = T#set.maxn div 2,
	  bso  = T#set.bso div 2,
	  segs = contract_segs(T#set.segs)};
maybe_contract_segs(T) -> T.

%% rehash(Bucket, Slot1, Slot2, MaxN) -> {Bucket1,Bucket2}.
-spec rehash([T], integer(), pos_integer(), pos_integer()) -> {[T],[T]}.
rehash([E|T], Slot1, Slot2, MaxN) ->
    {L1,L2} = rehash(T, Slot1, Slot2, MaxN),
    case erlang:phash(E, MaxN) of
	Slot1 -> {[E|L1],L2};
	Slot2 -> {L1,[E|L2]}
    end;
rehash([], _, _, _) -> {[],[]}.

%% mk_seg(Size) -> Segment.
-spec mk_seg(16) -> seg().
mk_seg(16) -> {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]}.

%% expand_segs(Segs, EmptySeg) -> NewSegs.
%% contract_segs(Segs) -> NewSegs.
%%  Expand/contract the segment tuple by doubling/halving the number
%%  of segments.  We special case the powers of 2 upto 32, this should
%%  catch most case.  N.B. the last element in the segments tuple is
%%  an extra element containing a default empty segment.
-spec expand_segs(segs(E), seg()) -> segs(E).
expand_segs({B1}, Empty) ->
    {B1,Empty};
expand_segs({B1,B2}, Empty) ->
    {B1,B2,Empty,Empty};
expand_segs({B1,B2,B3,B4}, Empty) ->
    {B1,B2,B3,B4,Empty,Empty,Empty,Empty};
expand_segs({B1,B2,B3,B4,B5,B6,B7,B8}, Empty) ->
    {B1,B2,B3,B4,B5,B6,B7,B8,
     Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty};
expand_segs({B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,B14,B15,B16}, Empty) ->
    {B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,B14,B15,B16,
     Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,
     Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty};
expand_segs(Segs, Empty) ->
    list_to_tuple(tuple_to_list(Segs) 
    ++ lists:duplicate(tuple_size(Segs), Empty)).

-spec contract_segs(segs(E)) -> segs(E).
contract_segs({B1,_}) ->
    {B1};
contract_segs({B1,B2,_,_}) ->
    {B1,B2};
contract_segs({B1,B2,B3,B4,_,_,_,_}) ->
    {B1,B2,B3,B4};
contract_segs({B1,B2,B3,B4,B5,B6,B7,B8,_,_,_,_,_,_,_,_}) ->
    {B1,B2,B3,B4,B5,B6,B7,B8};
contract_segs({B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,B14,B15,B16,
	       _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_}) ->
    {B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,B14,B15,B16};
contract_segs(Segs) ->
    Ss = tuple_size(Segs) div 2,
    list_to_tuple(lists:sublist(tuple_to_list(Segs), 1, Ss)).
