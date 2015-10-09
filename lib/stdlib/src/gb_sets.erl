%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2015. All Rights Reserved.
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
%% Copyright (C) 1999-2001 Richard Carlsson
%%
%% An implementation of ordered sets using Prof. Arne Andersson's
%% General Balanced Trees. This can be much more efficient than using
%% ordered lists, for larger sets, but depends on the application. See
%% notes below for details.
%% ---------------------------------------------------------------------
%% Notes:
%%
%% The complexity on set operations is bounded by either O(|S|) or O(|T|
%% * log(|S|)), where S is the largest given set, depending on which is
%% fastest for any particular function call. For operating on sets of
%% almost equal size, this implementation is about 3 times slower than
%% using ordered-list sets directly. For sets of very different sizes,
%% however, this solution can be arbitrarily much faster; in practical
%% cases, often between 10 and 100 times. This implementation is
%% particularly suited for ackumulating elements a few at a time,
%% building up a large set (more than 100-200 elements), and repeatedly
%% testing for membership in the current set.
%%
%% As with normal tree structures, lookup (membership testing),
%% insertion and deletion have logarithmic complexity.
%%
%% Operations:
%%
%% - empty(): returns empty set.
%%
%%   Alias: new(), for compatibility with `sets'.
%%
%% - is_empty(S): returns 'true' if S is an empty set, and 'false'
%%   otherwise.
%%
%% - size(S): returns the number of nodes in the set as an integer.
%%   Returns 0 (zero) if the set is empty.
%%
%% - singleton(X): returns a set containing only the element X.
%%
%% - is_member(X, S): returns `true' if element X is a member of set S,
%%   and `false' otherwise.
%%
%%   Alias: is_element(), for compatibility with `sets'.
%%
%% - insert(X, S): inserts element X into set S; returns the new set.
%%   *Assumes that the element is not present in S.*
%%
%% - add(X, S): adds element X to set S; returns the new set. If X is
%%   already an element in S, nothing is changed.
%%
%%   Alias: add_element(), for compatibility with `sets'.
%%
%% - delete(X, S): removes element X from set S; returns new set.
%%   Assumes that the element exists in the set.
%%
%% - delete_any(X, S): removes key X from set S if the key is present
%%   in the set, otherwise does nothing; returns new set.
%%
%%   Alias: del_element(), for compatibility with `sets'.
%%
%% - balance(S): rebalances the tree representation of S. Note that this
%%   is rarely necessary, but may be motivated when a large number of
%%   elements have been deleted from the tree without further
%%   insertions. Rebalancing could then be forced in order to minimise
%%   lookup times, since deletion only does not rebalance the tree.
%%
%% - union(S1, S2): returns a new set that contains each element that is
%%   in either S1 or S2 or both, and no other elements.
%%
%% - union(Ss): returns a new set that contains each element that is in
%%   at least one of the sets in the list Ss, and no other elements.
%%
%% - intersection(S1, S2): returns a new set that contains each element
%%   that is in both S1 and S2, and no other elements.
%%
%% - intersection(Ss): returns a new set that contains each element that
%%   is in all of the sets in the list Ss, and no other elements.
%%
%% - is_disjoint(S1, S2): returns `true' if none of the elements in S1
%%   occurs in S2.
%%
%% - difference(S1, S2): returns a new set that contains each element in
%%   S1 that is not also in S2, and no other elements.
%%
%%   Alias: subtract(), for compatibility with `sets'.
%%
%% - is_subset(S1, S2): returns `true' if each element in S1 is also a
%%   member of S2, and `false' otherwise.
%%
%% - to_list(S): returns an ordered list of all elements in set S. The
%%   list never contains duplicates.
%%
%% - from_list(List): creates a set containing all elements in List,
%%   where List may be unordered and contain duplicates.
%%
%% - from_ordset(L): turns an ordered-set list L into a set. The list
%%   must not contain duplicates.
%%
%% - smallest(S): returns the smallest element in set S. Assumes that
%%   the set S is nonempty.
%%
%% - largest(S): returns the largest element in set S. Assumes that the
%%   set S is nonempty.
%%
%% - take_smallest(S): returns {X, S1}, where X is the smallest element
%%   in set S, and S1 is the set S with element X deleted. Assumes that
%%   the set S is nonempty.
%%
%% - take_largest(S): returns {X, S1}, where X is the largest element in
%%   set S, and S1 is the set S with element X deleted. Assumes that the
%%   set S is nonempty.
%%
%% - iterator(S): returns an iterator that can be used for traversing
%%   the entries of set S; see `next'. The implementation of this is
%%   very efficient; traversing the whole set using `next' is only
%%   slightly slower than getting the list of all elements using
%%   `to_list' and traversing that. The main advantage of the iterator
%%   approach is that it does not require the complete list of all
%%   elements to be built in memory at one time.
%%
%% - iterator_from(X, S): returns an iterator that can be used for
%%   traversing the elements of set S greater than or equal to X;
%%   see `next'.
%%
%% - next(T): returns {X, T1} where X is the smallest element referred
%%   to by the iterator T, and T1 is the new iterator to be used for
%%   traversing the remaining elements, or the atom `none' if no
%%   elements remain.
%%
%% - filter(P, S): Filters set S using predicate function P. Included
%%   for compatibility with `sets'.
%%
%% - fold(F, A, S): Folds function F over set S with A as the initial
%%   ackumulator. Included for compatibility with `sets'.
%%
%% - is_set(S): returns 'true' if S appears to be a set, and 'false'
%%   otherwise. Not recommended; included for compatibility with `sets'.

-module(gb_sets).

-export([empty/0, is_empty/1, size/1, singleton/1, is_member/2,
	 insert/2, add/2, delete/2, delete_any/2, balance/1, union/2,
	 union/1, intersection/2, intersection/1, is_disjoint/2, difference/2,
	 is_subset/2, to_list/1, from_list/1, from_ordset/1, smallest/1,
	 largest/1, take_smallest/1, take_largest/1, iterator/1,
         iterator_from/2, next/1, filter/2, fold/3, is_set/1]).

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
-opaque set(Element) :: {non_neg_integer(), gb_set_node(Element)}.
-type set() :: set(_).
-opaque iter(Element) :: [gb_set_node(Element)].
-type iter() :: iter(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec empty() -> Set when
      Set :: set().

empty() ->
    {0, nil}.

-spec new() -> Set when
      Set :: set().

new() -> empty().

-spec is_empty(Set) -> boolean() when
      Set :: set().

is_empty({0, nil}) ->
    true;
is_empty(_) ->
    false.

-spec size(Set) -> non_neg_integer() when
      Set :: set().

size({Size, _}) ->
    Size.

-spec singleton(Element) -> set(Element).

singleton(Key) ->
    {1, {Key, nil, nil}}.

-spec is_element(Element, Set) -> boolean() when
      Set :: set(Element).

is_element(Key, S) ->
    is_member(Key, S).

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

-spec insert(Element, Set1) -> Set2 when
      Set1 :: set(Element),
      Set2 :: set(Element).

insert(Key, {S, T}) ->
    S1 = S + 1,
    {S1, insert_1(Key, T, ?pow(S1, ?p))}.

insert_1(Key, {Key1, Smaller, Bigger}, S) when Key < Key1 -> 
    case insert_1(Key, Smaller, ?div2(S)) of
	{T1, H1, S1} when is_integer(H1) ->
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
	{T1, H1, S1} when is_integer(H1) ->
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

-spec balance(Set1) -> Set2 when
      Set1 :: set(Element),
      Set2 :: set(Element).

balance({S, T}) ->
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

-spec add_element(Element, Set1) -> Set2 when
      Set1 :: set(Element),
      Set2 :: set(Element).

add_element(X, S) ->
    add(X, S).

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

-spec from_list(List) -> Set when
      List :: [Element],
      Set :: set(Element).

from_list(L) ->
    from_ordset(ordsets:from_list(L)).

-spec from_ordset(List) -> Set when
      List :: [Element],
      Set :: set(Element).

from_ordset(L) ->
    S = length(L),
    {S, balance_list(L, S)}.

-spec del_element(Element, Set1) -> Set2 when
      Set1 :: set(Element),
      Set2 :: set(Element).

del_element(Key, S) ->
    delete_any(Key, S).

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

-spec smallest(Set) -> Element when
      Set :: set(Element).

smallest({_, T}) ->
    smallest_1(T).

smallest_1({Key, nil, _Larger}) ->
    Key;
smallest_1({_Key, Smaller, _Larger}) ->
    smallest_1(Smaller).

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

-spec largest(Set) -> Element when
      Set :: set(Element).

largest({_, T}) ->
    largest_1(T).

largest_1({Key, _Smaller, nil}) ->
    Key;
largest_1({_Key, _Smaller, Larger}) ->
    largest_1(Larger).

-spec to_list(Set) -> List when
      Set :: set(Element),
      List :: [Element].

to_list({_, T}) ->
    to_list(T, []).

to_list_1(T) -> to_list(T, []).

to_list({Key, Small, Big}, L) ->
    to_list(Small, [Key | to_list(Big, L)]);
to_list(nil, L) -> L.

-spec iterator(Set) -> Iter when
      Set :: set(Element),
      Iter :: iter(Element).

iterator({_, T}) ->
    iterator(T, []).

%% The iterator structure is really just a list corresponding to the
%% call stack of an in-order traversal. This is quite fast.

iterator({_, nil, _} = T, As) ->
    [T | As];
iterator({_, L, _} = T, As) ->
    iterator(L, [T | As]);
iterator(nil, As) ->
    As.

-spec iterator_from(Element, Set) -> Iter when
      Set :: set(Element),
      Iter :: iter(Element).

iterator_from(S, {_, T}) ->
    iterator_from(S, T, []).

iterator_from(S, {K, _, T}, As) when K < S ->
    iterator_from(S, T, As);
iterator_from(_, {_, nil, _} = T, As) ->
    [T | As];
iterator_from(S, {_, L, _} = T, As) ->
    iterator_from(S, L, [T | As]);
iterator_from(_, nil, As) ->
    As.

-spec next(Iter1) -> {Element, Iter2} | 'none' when
      Iter1 :: iter(Element),
      Iter2 :: iter(Element).

next([{X, _, T} | As]) ->
    {X, iterator(T, As)};
next([]) ->
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

-spec union(Set1, Set2) -> Set3 when
      Set1 :: set(Element),
      Set2 :: set(Element),
      Set3 :: set(Element).

union({N1, T1}, {N2, T2}) when N2 < N1 ->
    union(to_list_1(T2), N2, T1, N1);
union({N1, T1}, {N2, T2}) ->
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

balance_revlist(L, S) ->
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

-spec intersection(Set1, Set2) -> Set3 when
      Set1 :: set(Element),
      Set2 :: set(Element),
      Set3 :: set(Element).

intersection({N1, T1}, {N2, T2}) when N2 < N1 ->
    intersection(to_list_1(T2), N2, T1, N1);
intersection({N1, T1}, {N2, T2}) ->
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

-spec intersection(SetList) -> Set when
      SetList :: [set(Element),...],
      Set :: set(Element).

intersection([S | Ss]) ->
    intersection_list(S, Ss).

intersection_list(S, [S1 | Ss]) ->
    intersection_list(intersection(S, S1), Ss);
intersection_list(S, []) -> S.

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

-spec subtract(Set1, Set2) -> Set3 when
      Set1 :: set(Element),
      Set2 :: set(Element),
      Set3 :: set(Element).

subtract(S1, S2) ->
    difference(S1, S2).

-spec difference(Set1, Set2) -> Set3 when
      Set1 :: set(Element),
      Set2 :: set(Element),
      Set3 :: set(Element).

difference({N1, T1}, {N2, T2}) ->
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

-spec is_subset(Set1, Set2) -> boolean() when
      Set1 :: set(Element),
      Set2 :: set(Element).

is_subset({N1, T1}, {N2, T2}) ->
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

-spec is_set(Term) -> boolean() when
      Term :: term().

is_set({0, nil}) -> true;
is_set({N, {_, _, _}}) when is_integer(N), N >= 0 -> true;
is_set(_) -> false.

-spec filter(Pred, Set1) -> Set2 when
      Pred :: fun((Element) -> boolean()),
      Set1 :: set(Element),
      Set2 :: set(Element).

filter(F, S) ->
    from_ordset([X || X <- to_list(S), F(X)]).

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
