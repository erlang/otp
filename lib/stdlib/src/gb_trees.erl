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
%% =====================================================================
%% General Balanced Trees - highly efficient dictionaries.
%%
%% Copyright (C) 1999-2001 Sven-Olof Nyström, Richard Carlsson
%%
%% An efficient implementation of Prof. Arne Andersson's General
%% Balanced Trees. These have no storage overhead compared to plain
%% unbalanced binary trees, and their performance is in general better
%% than AVL trees.
%% ---------------------------------------------------------------------
%% Operations:
%%
%% - empty(): returns empty tree.
%%
%% - is_empty(T): returns 'true' if T is an empty tree, and 'false'
%%   otherwise.
%%
%% - size(T): returns the number of nodes in the tree as an integer.
%%   Returns 0 (zero) if the tree is empty.
%%
%% - lookup(X, T): looks up key X in tree T; returns {value, V}, or
%%   `none' if the key is not present.
%%
%% - get(X, T): retreives the value stored with key X in tree T. Assumes
%%   that the key is present in the tree.
%%
%% - insert(X, V, T): inserts key X with value V into tree T; returns
%%   the new tree. Assumes that the key is *not* present in the tree.
%%
%% - update(X, V, T): updates key X to value V in tree T; returns the
%%   new tree. Assumes that the key is present in the tree.
%%
%% - enter(X, V, T): inserts key X with value V into tree T if the key
%%   is not present in the tree, otherwise updates key X to value V in
%%   T. Returns the new tree.
%%
%% - delete(X, T): removes key X from tree T; returns new tree. Assumes
%%   that the key is present in the tree.
%%
%% - delete_any(X, T): removes key X from tree T if the key is present
%%   in the tree, otherwise does nothing; returns new tree.
%%
%% - take(X, T): removes element with key X from tree T; returns new tree
%%   without removed element. Assumes that the key is present in the tree.
%%
%% - take_any(X, T): removes element with key X from tree T and returns
%%   a new tree if the key is present; otherwise does nothing and returns
%%   'error'.
%%
%% - balance(T): rebalances tree T. Note that this is rarely necessary,
%%   but may be motivated when a large number of entries have been
%%   deleted from the tree without further insertions. Rebalancing could
%%   then be forced in order to minimise lookup times, since deletion
%%   only does not rebalance the tree.
%%
%% - is_defined(X, T): returns `true' if key X is present in tree T, and
%%   `false' otherwise.
%%
%% - keys(T): returns an ordered list of all keys in tree T.
%%
%% - values(T): returns the list of values for all keys in tree T,
%%   sorted by their corresponding keys. Duplicates are not removed.
%%
%% - to_list(T): returns an ordered list of {Key, Value} pairs for all
%%   keys in tree T.
%%
%% - from_orddict(L): turns an ordered list L of {Key, Value} pairs into
%%   a tree. The list must not contain duplicate keys.
%%
%% - smallest(T): returns {X, V}, where X is the smallest key in tree T,
%%   and V is the value associated with X in T. Assumes that the tree T
%%   is nonempty.
%%
%% - largest(T): returns {X, V}, where X is the largest key in tree T,
%%   and V is the value associated with X in T. Assumes that the tree T
%%   is nonempty.
%%
%% - take_smallest(T): returns {X, V, T1}, where X is the smallest key
%%   in tree T, V is the value associated with X in T, and T1 is the
%%   tree T with key X deleted. Assumes that the tree T is nonempty.
%%
%% - take_largest(T): returns {X, V, T1}, where X is the largest key
%%   in tree T, V is the value associated with X in T, and T1 is the
%%   tree T with key X deleted. Assumes that the tree T is nonempty.
%%
%% - iterator(T): returns an iterator that can be used for traversing
%%   the entries of tree T; see `next'. The implementation of this is
%%   very efficient; traversing the whole tree using `next' is only
%%   slightly slower than getting the list of all elements using
%%   `to_list' and traversing that. The main advantage of the iterator
%%   approach is that it does not require the complete list of all
%%   elements to be built in memory at one time.
%%
%% - iterator_from(K, T): returns an iterator that can be used for
%%   traversing the entries of tree T with key greater than or
%%   equal to K; see `next'.
%%
%% - next(S): returns {X, V, S1} where X is the smallest key referred to
%%   by the iterator S, and S1 is the new iterator to be used for
%%   traversing the remaining entries, or the atom `none' if no entries
%%   remain.
%%
%% - map(F, T): maps the function F(K, V) -> V' to all key-value pairs
%%   of the tree T and returns a new tree T' with the same set of keys
%%   as T and the new set of values V'.

-module(gb_trees).

-export([empty/0, is_empty/1, size/1, lookup/2, get/2, insert/3,
	 update/3, enter/3, delete/2, delete_any/2, balance/1,
	 is_defined/2, keys/1, values/1, to_list/1, from_orddict/1,
	 smallest/1, largest/1, take/2, take_any/2,
         take_smallest/1, take_largest/1,
	 iterator/1, iterator_from/2, next/1, map/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Data structure:
%% - {Size, Tree}, where `Tree' is composed of nodes of the form:
%%   - {Key, Value, Smaller, Bigger}, and the "empty tree" node:
%%   - nil.
%%
%% I make no attempt to balance trees after deletions. Since deletions
%% don't increase the height of a tree, I figure this is OK.
%%
%% Original balance condition h(T) <= ceil(c * log(|T|)) has been
%% changed to the similar (but not quite equivalent) condition 2 ^ h(T)
%% <= |T| ^ c. I figure this should also be OK.
%%
%% Performance is comparable to the AVL trees in the Erlang book (and
%% faster in general due to less overhead); the difference is that
%% deletion works for my trees, but not for the book's trees. Behaviour
%% is logaritmic (as it should be).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Some macros.

-define(p, 2). % It seems that p = 2 is optimal for sorted keys

-define(pow(A, _), A * A). % correct with exponent as defined above.

-define(div2(X), X bsr 1). 

-define(mul2(X), X bsl 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Some types.

-export_type([tree/0, tree/2, iter/0, iter/2]).

-type gb_tree_node(K, V) :: 'nil'
                          | {K, V, gb_tree_node(K, V), gb_tree_node(K, V)}.
-opaque tree(Key, Value) :: {non_neg_integer(), gb_tree_node(Key, Value)}.
-type tree() :: tree(_, _).
-opaque iter(Key, Value) :: [gb_tree_node(Key, Value)].
-type iter() :: iter(_, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec empty() -> tree().

empty() ->
    {0, nil}.

-spec is_empty(Tree) -> boolean() when
      Tree :: tree().

is_empty({0, nil}) ->
    true;
is_empty(_) ->
    false.

-spec size(Tree) -> non_neg_integer() when
      Tree :: tree().

size({Size, _}) when is_integer(Size), Size >= 0 ->
    Size.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec lookup(Key, Tree) -> 'none' | {'value', Value} when
      Tree :: tree(Key, Value).

lookup(Key, {_, T}) ->
    lookup_1(Key, T).

%% The term order is an arithmetic total order, so we should not
%% test exact equality for the keys. (If we do, then it becomes
%% possible that neither `>', `<', nor `=:=' matches.) Testing '<'
%% and '>' first is statistically better than testing for
%% equality, and also allows us to skip the test completely in the
%% remaining case.

lookup_1(Key, {Key1, _, Smaller, _}) when Key < Key1 ->
    lookup_1(Key, Smaller);
lookup_1(Key, {Key1, _, _, Bigger}) when Key > Key1 ->
    lookup_1(Key, Bigger);
lookup_1(_, {_, Value, _, _}) ->
    {value, Value};
lookup_1(_, nil) ->
    none.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is a specialized version of `lookup'.

-spec is_defined(Key, Tree) -> boolean() when
      Tree :: tree(Key, Value :: term()).

is_defined(Key, {_, T}) ->
    is_defined_1(Key, T).

is_defined_1(Key, {Key1, _, Smaller, _}) when Key < Key1 ->
    is_defined_1(Key, Smaller);
is_defined_1(Key, {Key1, _, _, Bigger}) when Key > Key1 ->
    is_defined_1(Key, Bigger);
is_defined_1(_, {_, _, _, _}) ->
    true;
is_defined_1(_, nil) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is a specialized version of `lookup'.

-spec get(Key, Tree) -> Value when
      Tree :: tree(Key, Value).

get(Key, {_, T}) ->
    get_1(Key, T).

get_1(Key, {Key1, _, Smaller, _}) when Key < Key1 ->
    get_1(Key, Smaller);
get_1(Key, {Key1, _, _, Bigger}) when Key > Key1 ->
    get_1(Key, Bigger);
get_1(_, {_, Value, _, _}) ->
    Value.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec update(Key, Value, Tree1) -> Tree2 when
      Tree1 :: tree(Key, Value),
      Tree2 :: tree(Key, Value).

update(Key, Val, {S, T}) ->
    T1 = update_1(Key, Val, T),
    {S, T1}.

%% See `lookup' for notes on the term comparison order.

update_1(Key, Value, {Key1, V, Smaller, Bigger}) when Key < Key1 -> 
    {Key1, V, update_1(Key, Value, Smaller), Bigger};
update_1(Key, Value, {Key1, V, Smaller, Bigger}) when Key > Key1 ->
    {Key1, V, Smaller, update_1(Key, Value, Bigger)};
update_1(Key, Value, {_, _, Smaller, Bigger}) ->
    {Key, Value, Smaller, Bigger}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec insert(Key, Value, Tree1) -> Tree2 when
      Tree1 :: tree(Key, Value),
      Tree2 :: tree(Key, Value).

insert(Key, Val, {S, T}) when is_integer(S) ->
    S1 = S+1,
    {S1, insert_1(Key, Val, T, ?pow(S1, ?p))}.

insert_1(Key, Value, {Key1, V, Smaller, Bigger}, S) when Key < Key1 -> 
    case insert_1(Key, Value, Smaller, ?div2(S)) of
	{T1, H1, S1} ->
	    T = {Key1, V, T1, Bigger},
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
	    {Key1, V, T1, Bigger}
    end;
insert_1(Key, Value, {Key1, V, Smaller, Bigger}, S) when Key > Key1 -> 
    case insert_1(Key, Value, Bigger, ?div2(S)) of
	{T1, H1, S1} ->
	    T = {Key1, V, Smaller, T1},
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
	    {Key1, V, Smaller, T1}
    end;
insert_1(Key, Value, nil, S) when S =:= 0 ->
    {{Key, Value, nil, nil}, 1, 1};
insert_1(Key, Value, nil, _S) ->
    {Key, Value, nil, nil};
insert_1(Key, _, _, _) ->
    erlang:error({key_exists, Key}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec enter(Key, Value, Tree1) -> Tree2 when
      Tree1 :: tree(Key, Value),
      Tree2 :: tree(Key, Value).

enter(Key, Val, T) ->
    case is_defined(Key, T) of
	true ->
	    update(Key, Val, T);
	false ->
	    insert(Key, Val, T)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

count({_, _, nil, nil}) ->
    {1, 1};
count({_, _, Sm, Bi}) ->
    {H1, S1} = count(Sm),
    {H2, S2} = count(Bi),
    {?mul2(erlang:max(H1, H2)), S1 + S2 + 1};
count(nil) ->
    {1, 0}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec balance(Tree1) -> Tree2 when
      Tree1 :: tree(Key, Value),
      Tree2 :: tree(Key, Value).

balance({S, T}) ->
    {S, balance(T, S)}.

balance(T, S) ->
    balance_list(to_list_1(T), S).

balance_list(L, S) ->
    {T, []} = balance_list_1(L, S),
    T.

balance_list_1(L, S) when S > 1 ->
    Sm = S - 1,
    S2 = Sm div 2,
    S1 = Sm - S2,
    {T1, [{K, V} | L1]} = balance_list_1(L, S1),
    {T2, L2} = balance_list_1(L1, S2),
    T = {K, V, T1, T2},
    {T, L2};
balance_list_1([{Key, Val} | L], 1) ->
    {{Key, Val, nil, nil}, L};
balance_list_1(L, 0) ->
    {nil, L}.

-spec from_orddict(List) -> Tree when
      List :: [{Key, Value}],
      Tree :: tree(Key, Value).

from_orddict(L) ->
    S = length(L),
    {S, balance_list(L, S)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec delete_any(Key, Tree1) -> Tree2 when
      Tree1 :: tree(Key, Value),
      Tree2 :: tree(Key, Value).

delete_any(Key, T) ->
    case is_defined(Key, T) of
	true ->
	    delete(Key, T);
	false ->
	    T
    end.

%%% delete. Assumes that key is present.

-spec delete(Key, Tree1) -> Tree2 when
      Tree1 :: tree(Key, Value),
      Tree2 :: tree(Key, Value).

delete(Key, {S, T}) when is_integer(S), S >= 0 ->
    {S - 1, delete_1(Key, T)}.

%% See `lookup' for notes on the term comparison order.

delete_1(Key, {Key1, Value, Smaller, Larger}) when Key < Key1 ->
    Smaller1 = delete_1(Key, Smaller),
    {Key1, Value, Smaller1, Larger};
delete_1(Key, {Key1, Value, Smaller, Bigger}) when Key > Key1 ->
    Bigger1 = delete_1(Key, Bigger),
    {Key1, Value, Smaller, Bigger1};
delete_1(_, {_, _, Smaller, Larger}) ->
    merge(Smaller, Larger).

merge(Smaller, nil) ->
    Smaller;
merge(nil, Larger) ->
    Larger;
merge(Smaller, Larger) ->
    {Key, Value, Larger1} = take_smallest1(Larger),
    {Key, Value, Smaller, Larger1}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec take_any(Key, Tree1) -> {Value, Tree2} | 'error' when
      Tree1 :: tree(Key, _),
      Tree2 :: tree(Key, _),
      Key   :: term(),
      Value :: term().

take_any(Key, Tree) ->
    case is_defined(Key, Tree) of
        true -> take(Key, Tree);
        false -> error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec take(Key, Tree1) -> {Value, Tree2} when
      Tree1 :: tree(Key, _),
      Tree2 :: tree(Key, _),
      Key   :: term(),
      Value :: term().

take(Key, {S, T}) when is_integer(S), S >= 0 ->
    {Value, Res} = take_1(Key, T),
    {Value, {S - 1, Res}}.

take_1(Key, {Key1, Value, Smaller, Larger}) when Key < Key1 ->
    {Value2, Smaller1} = take_1(Key, Smaller),
    {Value2, {Key1, Value, Smaller1, Larger}};
take_1(Key, {Key1, Value, Smaller, Bigger}) when Key > Key1 ->
    {Value2, Bigger1} = take_1(Key, Bigger),
    {Value2, {Key1, Value, Smaller, Bigger1}};
take_1(_, {_Key, Value, Smaller, Larger}) ->
    {Value, merge(Smaller, Larger)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec take_smallest(Tree1) -> {Key, Value, Tree2} when
      Tree1 :: tree(Key, Value),
      Tree2 :: tree(Key, Value).

take_smallest({Size, Tree}) when is_integer(Size), Size >= 0 ->
    {Key, Value, Larger} = take_smallest1(Tree),
    {Key, Value, {Size - 1, Larger}}.

take_smallest1({Key, Value, nil, Larger}) ->
    {Key, Value, Larger};
take_smallest1({Key, Value, Smaller, Larger}) ->
    {Key1, Value1, Smaller1} = take_smallest1(Smaller),
    {Key1, Value1, {Key, Value, Smaller1, Larger}}.

-spec smallest(Tree) -> {Key, Value} when
      Tree :: tree(Key, Value).

smallest({_, Tree}) ->
    smallest_1(Tree).

smallest_1({Key, Value, nil, _Larger}) ->
    {Key, Value};
smallest_1({_Key, _Value, Smaller, _Larger}) ->
    smallest_1(Smaller).

-spec take_largest(Tree1) -> {Key, Value, Tree2} when
      Tree1 :: tree(Key, Value),
      Tree2 :: tree(Key, Value).

take_largest({Size, Tree}) when is_integer(Size), Size >= 0 ->
    {Key, Value, Smaller} = take_largest1(Tree),
    {Key, Value, {Size - 1, Smaller}}.

take_largest1({Key, Value, Smaller, nil}) ->
    {Key, Value, Smaller};
take_largest1({Key, Value, Smaller, Larger}) ->
    {Key1, Value1, Larger1} = take_largest1(Larger),
    {Key1, Value1, {Key, Value, Smaller, Larger1}}.

-spec largest(Tree) -> {Key, Value} when
      Tree :: tree(Key, Value).

largest({_, Tree}) ->
    largest_1(Tree).

largest_1({Key, Value, _Smaller, nil}) ->
    {Key, Value};
largest_1({_Key, _Value, _Smaller, Larger}) ->
    largest_1(Larger).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec to_list(Tree) -> [{Key, Value}] when
      Tree :: tree(Key, Value).
			   
to_list({_, T}) ->
    to_list(T, []).

to_list_1(T) -> to_list(T, []).

to_list({Key, Value, Small, Big}, L) ->
    to_list(Small, [{Key, Value} | to_list(Big, L)]);
to_list(nil, L) -> L.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec keys(Tree) -> [Key] when
      Tree :: tree(Key, Value :: term()).

keys({_, T}) ->
    keys(T, []).

keys({Key, _Value, Small, Big}, L) ->
    keys(Small, [Key | keys(Big, L)]);
keys(nil, L) -> L.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec values(Tree) -> [Value] when
      Tree :: tree(Key :: term(), Value).

values({_, T}) ->
    values(T, []).

values({_Key, Value, Small, Big}, L) ->
    values(Small, [Value | values(Big, L)]);
values(nil, L) -> L.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec iterator(Tree) -> Iter when
      Tree :: tree(Key, Value),
      Iter :: iter(Key, Value).

iterator({_, T}) ->
    iterator_1(T).

iterator_1(T) ->
    iterator(T, []).

%% The iterator structure is really just a list corresponding to
%% the call stack of an in-order traversal. This is quite fast.

iterator({_, _, nil, _} = T, As) ->
    [T | As];
iterator({_, _, L, _} = T, As) ->
    iterator(L, [T | As]);
iterator(nil, As) ->
    As.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec iterator_from(Key, Tree) -> Iter when
      Tree :: tree(Key, Value),
      Iter :: iter(Key, Value).

iterator_from(S, {_, T}) ->
    iterator_1_from(S, T).

iterator_1_from(S, T) ->
    iterator_from(S, T, []).

iterator_from(S, {K, _, _, T}, As) when K < S ->
    iterator_from(S, T, As);
iterator_from(_, {_, _, nil, _} = T, As) ->
    [T | As];
iterator_from(S, {_, _, L, _} = T, As) ->
    iterator_from(S, L, [T | As]);
iterator_from(_, nil, As) ->
    As.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec next(Iter1) -> 'none' | {Key, Value, Iter2} when
      Iter1 :: iter(Key, Value),
      Iter2 :: iter(Key, Value).

next([{X, V, _, T} | As]) ->
    {X, V, iterator(T, As)};
next([]) ->
    none.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec map(Function, Tree1) -> Tree2 when
      Function :: fun((K :: Key, V1 :: Value1) -> V2 :: Value2),
      Tree1 :: tree(Key, Value1),
      Tree2 :: tree(Key, Value2).

map(F, {Size, Tree}) when is_function(F, 2) ->
    {Size, map_1(F, Tree)}.

map_1(_, nil) -> nil;
map_1(F, {K, V, Smaller, Larger}) ->
    {K, F(K, V), map_1(F, Smaller), map_1(F, Larger)}.
