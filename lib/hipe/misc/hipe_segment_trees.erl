%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% Segment trees, with a delete operation.
%%%
%%% Keys are the (0-based) indices into the list passed to build/1.
%%%
%%% Range bounds are inclusive.
%%%

-module(hipe_segment_trees).

-export([build/1, intersect/2, delete/4]).

-record(segment_tree, {
	  lo            :: integer(),
	  hi            :: integer(),
	  root          :: tnode()
	 }).

%% X =< Mid belongs in Left
-define(NODE(Left, Right, Mid, Segments), {Left, Right, Mid, Segments}).

-define(POINT_LEAF(Val), Val).
-define(RANGE_LEAF(Lo, Hi), {Lo, Hi}).

-type segments() :: [non_neg_integer()].
-type leaf()     :: segments().
-type tnode()    :: ?NODE(tnode(), tnode(), integer(), segments()) | leaf().

-opaque tree() :: #segment_tree{} | nil.
-export_type([tree/0]).

%% @doc Builds a segment tree of the given intervals.
-spec build([{integer(), integer()}]) -> tree().
build(ListOfIntervals) ->
    case
	lists:usort(
	  lists:append(
	    [[Lo, Hi] || {Lo, Hi} <- ListOfIntervals, Lo =< Hi]))
    of
	[] -> nil;
	Endpoints ->
	    Tree0 = empty_tree_from_endpoints(Endpoints),
	    [Lo|_] = Endpoints,
	    Hi = lists:last(Endpoints),
	    Tree1 = insert_intervals(0, ListOfIntervals, Lo, Hi, Tree0),
	    Tree = squash_empty_subtrees(Tree1),
	    #segment_tree{lo=Lo, hi=Hi, root=Tree}
    end.

empty_tree_from_endpoints(Endpoints) ->
    Leaves = leaves(Endpoints),
    {T, [], _, _} = balanced_bst(Leaves, length(Leaves)),
    T.

leaves([Endpoint]) -> [?POINT_LEAF(Endpoint)];
leaves([A | [B|_] = Tail]) ->
    %% We omit the range leaf if it's empty
    case A<B-1 of
	true  -> [?POINT_LEAF(A),?RANGE_LEAF(A+1,B-1) | leaves(Tail)];
	false -> [?POINT_LEAF(A) | leaves(Tail)]
    end.

balanced_bst(L, S) when S > 1 ->
    Sm = S, %% - 1
    S2 = Sm div 2,
    S1 = Sm - S2,
    {Left, L1, LeftLo, LeftHi} = balanced_bst(L, S1),
    {Right, L2, _, RightHi} = balanced_bst(L1, S2),
    T = ?NODE(Left, Right, LeftHi, []),
    {T, L2, LeftLo, RightHi};
balanced_bst([?RANGE_LEAF(Lo, Hi) | L], 1) ->
    {[], L, Lo, Hi};
balanced_bst([?POINT_LEAF(Val) | L], 1) ->
    {[], L, Val, Val}.

insert_intervals(_Ix, [], _Lo, _Hi, Tree) -> Tree;
insert_intervals(Ix, [Int|Ints], Lo, Hi, Tree) ->
    insert_intervals(Ix + 1, Ints, Lo, Hi,
		     insert_interval(Ix, Int, Lo, Hi, Tree)).

insert_interval(_, {Lo, Hi}, _, _, Node) when Lo > Hi -> Node;
insert_interval(I, Int={Lo,Hi}, NLo, NHi,
		?NODE(Left0, Right0, Mid, Segments)) ->
    if Lo =< NLo, NHi =< Hi ->
	    ?NODE(Left0, Right0, Mid, [I|Segments]);
       true ->
	    Left = case intervals_intersect(Lo, Hi,    NLo, Mid) of
		       true -> insert_interval(I, Int, NLo, Mid, Left0);
		       false -> Left0
		   end,
	    Right = case intervals_intersect(Lo, Hi,    Mid+1, NHi) of
			true -> insert_interval(I, Int, Mid+1, NHi, Right0);
			false -> Right0
		   end,
	    ?NODE(Left, Right, Mid, Segments)
    end;
insert_interval(I, {_Lo,_Hi}, _NLo, _NHi, Leaf) -> [I|Leaf].

intervals_intersect(ALo, AHi, BLo, BHi) ->
    (ALo =< AHi) andalso (BLo =< BHi) %% both nonempty
	andalso nonempty_intervals_intersect(ALo, AHi, BLo, BHi).

%% Purely optional optimisation
squash_empty_subtrees(?NODE(Left0, Right0, Mid, Segs)) ->
    build_squash_node(squash_empty_subtrees(Left0),
		      squash_empty_subtrees(Right0),
		      Mid, Segs);
squash_empty_subtrees(Leaf) -> Leaf.

build_squash_node([], [], _, Segs) -> Segs;
build_squash_node(Left, Right, Mid, Segs) ->
    ?NODE(Left, Right, Mid, Segs).

%% @doc Returns the indices of the intervals in the tree that contains Point.
-spec intersect(integer(), tree()) -> [non_neg_integer()].
intersect(Point, nil) when is_integer(Point) -> [];
intersect(Point, #segment_tree{lo=Lo, hi=Hi, root=Root})
  when is_integer(Point) ->
    case Lo =< Point andalso Point =< Hi of
	false -> [];
	true -> intersect_1(Point, Root, [])
    end.

intersect_1(Point, ?NODE(Left, Right, Mid, Segs), Acc0) ->
    Child = if Point =< Mid -> Left; true -> Right end,
    intersect_1(Point, Child, Segs ++ Acc0);
intersect_1(_, LeafSegs, Acc) -> LeafSegs ++ Acc.

%% @doc Deletes the interval {Lo, Hi}, which had index Index in the list passed
%%      to build/1.
-spec delete(non_neg_integer(), integer(), integer(), tree()) -> tree().
delete(_, _, _, nil) -> nil;
delete(_, Lo, Hi, Tree) when Lo > Hi -> Tree;
delete(_, Lo, Hi, Tree = #segment_tree{lo=TLo, hi=THi})
  when Hi < TLo; Lo > THi -> Tree;
delete(Index, Lo, Hi, Tree = #segment_tree{lo=TLo, hi=THi, root=Root0})
  when is_integer(Lo), is_integer(Hi) ->
    Root = delete_1(Index, Lo, Hi, TLo, THi, Root0),
    Tree#segment_tree{root=Root}.

delete_1(I, Lo, Hi, NLo, NHi, ?NODE(Left0, Right0, Mid, Segments)) ->
    if Lo =< NLo, NHi =< Hi ->
	    ?NODE(Left0, Right0, Mid, delete_2(Segments, I));
       true ->
	    Left = case nonempty_intervals_intersect(Lo, Hi, NLo, Mid) of
		       true -> delete_1(I, Lo, Hi, NLo, Mid, Left0);
		       false -> Left0
		   end,
	    Right = case nonempty_intervals_intersect(Lo, Hi, Mid+1, NHi) of
			true -> delete_1(I, Lo, Hi, Mid+1, NHi, Right0);
			false -> Right0
		   end,
	    %% We could do build_squash_node here, is it worth it?
	    ?NODE(Left, Right, Mid, Segments)
    end;
delete_1(I, _Lo, _Hi, _NLo, _NHi, Leaf) -> delete_2(Leaf, I).

delete_2([I|Segs], I) -> Segs;
delete_2([S|Segs], I) -> [S|delete_2(Segs,I)].

-compile({inline,nonempty_intervals_intersect/4}).
nonempty_intervals_intersect(ALo, AHi, BLo, BHi) ->
    (BLo =< AHi) andalso (ALo =< BHi).
