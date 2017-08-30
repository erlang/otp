%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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

%% We use the dynamic hashing techniques by Per-Åke Larsson as
%% described in "The Design and Implementation of Dynamic Hashing for
%% Sets and Tables in Icon" by Griswold and Townsend.  Much of the
%% terminology comes from that paper as well.

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

%% Standard interface.
-export([new/0,is_set/1,size/1,to_list/1,from_list/1]).
-export([is_element/2,add_element/2,del_element/2]).
-export([union/2,union/1,intersection/2,intersection/1]).
-export([is_disjoint/2]).
-export([subtract/2,is_subset/2]).
-export([fold/3,filter/2]).

-export_type([set/0, set/1]).

%% Note: mk_seg/1 must be changed too if seg_size is changed.
-define(seg_size, 16).
-define(max_seg, 32).
-define(expand_load, 5).
-define(contract_load, 3).
-define(exp_size, ?seg_size * ?expand_load).
-define(con_size, ?seg_size * ?contract_load).

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

-opaque set(Element) :: #set{segs :: segs(Element)}.

%%------------------------------------------------------------------------------

%% new() -> Set
-spec new() -> set().
new() ->
    Empty = mk_seg(?seg_size),
    #set{empty = Empty, segs = {Empty}}.

%% is_set(Set) -> boolean().
%%  Return 'true' if Set is a set of elements, else 'false'.
-spec is_set(Set) -> boolean() when
      Set :: term().
is_set(#set{}) -> true;
is_set(_) -> false.

%% size(Set) -> int().
%%  Return the number of elements in Set.
-spec size(Set) -> non_neg_integer() when
      Set :: set().
size(S) -> S#set.size. 

%% to_list(Set) -> [Elem].
%%  Return the elements in Set as a list.
-spec to_list(Set) -> List when
      Set :: set(Element),
      List :: [Element].
to_list(S) ->
    fold(fun (Elem, List) -> [Elem|List] end, [], S).

%% from_list([Elem]) -> Set.
%%  Build a set from the elements in List.
-spec from_list(List) -> Set when
      List :: [Element],
      Set :: set(Element).
from_list(L) ->
    lists:foldl(fun (E, S) -> add_element(E, S) end, new(), L).

%% is_element(Element, Set) -> boolean().
%%  Return 'true' if Element is an element of Set, else 'false'.
-spec is_element(Element, Set) -> boolean() when
      Set :: set(Element).
is_element(E, S) ->
    Slot = get_slot(S, E),
    Bkt = get_bucket(S, Slot),
    lists:member(E, Bkt).

%% add_element(Element, Set) -> Set.
%%  Return Set with Element inserted in it.
-spec add_element(Element, Set1) -> Set2 when
      Set1 :: set(Element),
      Set2 :: set(Element).
add_element(E, S0) ->
    Slot = get_slot(S0, E),
    {S1,Ic} = on_bucket(fun (B0) -> add_bkt_el(E, B0, B0) end, S0, Slot),
    maybe_expand(S1, Ic).

-spec add_bkt_el(T, [T], [T]) -> {[T], 0 | 1}.
add_bkt_el(E, [E|_], Bkt) -> {Bkt,0};
add_bkt_el(E, [_|B], Bkt) ->
    add_bkt_el(E, B, Bkt);
add_bkt_el(E, [], Bkt) -> {[E|Bkt],1}.

%% del_element(Element, Set) -> Set.
%%  Return Set but with Element removed.
-spec del_element(Element, Set1) -> Set2 when
      Set1 :: set(Element),
      Set2 :: set(Element).
del_element(E, S0) ->
    Slot = get_slot(S0, E),
    {S1,Dc} = on_bucket(fun (B0) -> del_bkt_el(E, B0) end, S0, Slot),
    maybe_contract(S1, Dc).

-spec del_bkt_el(T, [T]) -> {[T], 0 | 1}.
del_bkt_el(E, [E|Bkt]) -> {Bkt,1};
del_bkt_el(E, [Other|Bkt0]) ->
    {Bkt1,Dc} = del_bkt_el(E, Bkt0),
    {[Other|Bkt1],Dc};
del_bkt_el(_, []) -> {[],0}.

%% union(Set1, Set2) -> Set
%%  Return the union of Set1 and Set2.
-spec union(Set1, Set2) -> Set3 when
      Set1 :: set(Element),
      Set2 :: set(Element),
      Set3 :: set(Element).
union(S1, S2) when S1#set.size < S2#set.size ->
    fold(fun (E, S) -> add_element(E, S) end, S2, S1);
union(S1, S2) ->
    fold(fun (E, S) -> add_element(E, S) end, S1, S2).

%% union([Set]) -> Set
%%  Return the union of the list of sets.
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
-spec intersection(Set1, Set2) -> Set3 when
      Set1 :: set(Element),
      Set2 :: set(Element),
      Set3 :: set(Element).
intersection(S1, S2) when S1#set.size < S2#set.size ->
    filter(fun (E) -> is_element(E, S2) end, S1);
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

-spec intersection1(set(E), [set(E)]) -> set(E).
intersection1(S1, [S2|Ss]) ->
    intersection1(intersection(S1, S2), Ss);
intersection1(S1, []) -> S1.

%% is_disjoint(Set1, Set2) -> boolean().
%%  Check whether Set1 and Set2 are disjoint.
-spec is_disjoint(Set1, Set2) -> boolean() when
      Set1 :: set(Element),
      Set2 :: set(Element).
is_disjoint(S1, S2) when S1#set.size < S2#set.size ->
    fold(fun (_, false) -> false;
	     (E, true) -> not is_element(E, S2)
	 end, true, S1);
is_disjoint(S1, S2) ->
    fold(fun (_, false) -> false;
	     (E, true) -> not is_element(E, S1)
	 end, true, S2).

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
is_subset(S1, S2) ->
    fold(fun (E, Sub) -> Sub andalso is_element(E, S2) end, true, S1).

%% fold(Fun, Accumulator, Set) -> Accumulator.
%%  Fold function Fun over all elements in Set and return Accumulator.
-spec fold(Function, Acc0, Set) -> Acc1 when
      Function :: fun((Element, AccIn) -> AccOut),
      Set :: set(Element),
      Acc0 :: Acc,
      Acc1 :: Acc,
      AccIn :: Acc,
      AccOut :: Acc.
fold(F, Acc, D) -> fold_set(F, Acc, D).

%% filter(Fun, Set) -> Set.
%%  Filter Set with Fun.
-spec filter(Pred, Set1) -> Set2 when
      Pred :: fun((Element) -> boolean()),
      Set1 :: set(Element),
      Set2 :: set(Element).
filter(F, D) -> filter_set(F, D).

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

%% on_bucket(Fun, Hashdb, Slot) -> {NewHashDb,Result}.
%%  Apply Fun to the bucket in Slot and replace the returned bucket.
-spec on_bucket(fun((_) -> {[_], 0 | 1}), set(E), non_neg_integer()) ->
	  {set(E), 0 | 1}.
on_bucket(F, T, Slot) ->
    SegI = ((Slot-1) div ?seg_size) + 1,
    BktI = ((Slot-1) rem ?seg_size) + 1,
    Segs = T#set.segs,
    Seg = element(SegI, Segs),
    B0 = element(BktI, Seg),
    {B1, Res} = F(B0),				%Op on the bucket.
    {T#set{segs = setelement(SegI, Segs, setelement(BktI, Seg, B1))},Res}.

%% fold_set(Fun, Acc, Dictionary) -> Dictionary.
%% filter_set(Fun, Dictionary) -> Dictionary.

%%  Work functions for fold and filter operations.  These traverse the
%%  hash structure rebuilding as necessary.  Note we could have
%%  implemented map and hash using fold but these should be faster.
%%  We hope!

fold_set(F, Acc, D) when is_function(F, 2) ->
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

filter_set(F, D) when is_function(F, 1) ->
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

-spec maybe_expand(set(E), 0 | 1) -> set(E).
maybe_expand(T0, Ic) when T0#set.size + Ic > T0#set.exp_size ->
    T = maybe_expand_segs(T0),			%Do we need more segments.
    N = T#set.n + 1,				%Next slot to expand into
    Segs0 = T#set.segs,
    Slot1 = N - T#set.bso,
    B = get_bucket_s(Segs0, Slot1),
    Slot2 = N,
    {B1,B2} = rehash(B, Slot1, Slot2, T#set.maxn),
    Segs1 = put_bucket_s(Segs0, Slot1, B1),
    Segs2 = put_bucket_s(Segs1, Slot2, B2),
    T#set{size = T#set.size + Ic,
	  n = N,
	  exp_size = N * ?expand_load,
	  con_size = N * ?contract_load,
	  segs = Segs2};
maybe_expand(T, Ic) -> T#set{size = T#set.size + Ic}.

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
