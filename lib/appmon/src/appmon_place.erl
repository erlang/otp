%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%------------------------------------------------------------
%%
%% Places a Digraph in a tree-like manner. The vertices in the digraph
%% is updated with x and y positions. The operation is not atomic. The
%% digraph may be cyclic but edges must then have been labeled primary
%% or secondary and the set of primary links must make up a non-cyclic
%% graph (a tree).
%%
%%
%%		IMPLEMENTATION DETAIL
%%		---------------------
%%
%%		The placement algorithm is straightforward, place the
%%		nodes in the vertical plane (y-plane) and then place
%%		nodes in the horisontal plane (x-plane).
%%
%%		First all nodes are placed in the y (vertical) plane
%%		by a standard traversing of the tree. We then place
%%		the tree in the x (horisontal) plane. Each node is
%%		placed in the middle of its children as far to the
%%		left as possible, preferably at the left margin. Two
%%		things can make a node not be placed at the left
%%		margin and that is the case when a previous node has
%%		been placed at the same vertical level as the node we
%%		are trying to place (thus forcing a placement further
%%		to the right), and the second case is when the middle
%%		of the subtree of the node is not at the left margin
%%		(which it only is when the subtree is empty). The
%%		algorithm obviously depends on keeping track of the
%%		rightmost positions at all depths, and this
%%		information is also usefull when calculating the width
%%		of the tree.
%%
%%
%%
%%------------------------------------------------------------



-module(appmon_place).

-export([place/2]).

-include("appmon_dg.hrl").


-import(lists, [foreach/2, foldl/3]).


place(DG, Root) ->
    case appmon_dg:get(data, DG, Root) of
	false -> [0];
	_Other -> 
	    placey(DG, Root, 1),
	    placex(DG, Root, [])
    end.


%%------------------------------------------------------------
%%
%%
%%	Placing a graph in y plane
%%	--------------------------
%%
%%	Place nodes in the graph in the y plane rather stupidly
%%

placey(DG, V, Y) ->
    appmon_dg:set(y, DG, V, Y),
    Y1 = Y+1,
    foreach(fun(C) -> placey(DG, C, Y1) end, appmon_dg:get(out, DG, V)).




%%------------------------------------------------------------
%%
%%
%%	Place a tree in the x plane
%%	---------------------------	
%%
%%	Place nodes in the tree in the x plane. The goal of the x
%%	placement is to place all nodes as far to the left as possible
%%	while maintaining a nice tree shape.
%%
%%	To place a node we must first place its children, the
%%	intention is to place the current node in the middle and above
%%	its children. The calc_mid function will place the node in the
%%	middle of its children. If the node should be placed further
%%	to the right than the middle of its children, then its
%%	children are moved DeltaX positions to be balanced under the
%%	node.  Thus at the end the node and its children form a nice
%%	looking tree.
%%
%%	The function also maintains the 'rightmost x on each level'
%%	list LastX by putting its own position on top of the list
%%
%%

placex(DG, V, LastX) ->
    Ch = appmon_dg:get(out, DG, V),
    ChLX = foldl(fun(C, Accu) -> placex(DG, C, Accu) end,
		 tll(LastX),
		 Ch),
    
    Width	= appmon_dg:get(w, DG, V),
    MyX		= calc_mid(DG, Width, Ch),
    DeltaX	= calc_delta(MyX, hdd(LastX)+spacex()),

    appmon_dg:set(x, DG, V, MyX),
    move(DG, V, [MyX+Width | ChLX], DeltaX).


%%------------------------------------------------------------
%%
%%
%%	Move a subtree DeltaX positions to the right
%%	--------------------------------------------
%%
%%	Used when moving children to balance under an already placed
%%	parent. Note that the correct LastX depends on the ordering of
%%	the children which must be the same as when the children were
%%	first placed. It must be ensured that hdd(NewLastX) is the
%%	same as hdd(NewLastX)+DeltaX. If the order of children is
%%	preserved then so is hdd(LastX). Another solution would be to
%%	set hdd(LastX) from the parent
%%
%%	Note the two base clauses, one for the no-children case and
%%	one optimisation clause (unneccessary perhaps) for DeltaX==0
%%

move(_DG, _L, LastX, 0) -> LastX;
move(DG, V, LastX, DeltaX) -> move2(DG, V, LastX, DeltaX).

move2(DG, V, LastX, DeltaX) ->
    NewX = appmon_dg:get(x, DG, V)+DeltaX,
    appmon_dg:set(x, DG, V, NewX),
    ChLX = foldl(fun(C, LX) -> move2(DG, C, LX, DeltaX) end,
		 tll(LastX), 
		 appmon_dg:get(out, DG, V)),
    [erlang:max(NewX+appmon_dg:get(w, DG, V), hdd(LastX)) | ChLX].


%%------------------------------------------------------------
%%
%%
%%	Calculate the middle position of the children
%%	---------------------------------------------
%%
%%	Calculates the mid x position for a list of children. This
%%	position is later compared to the position dictated by LastX
%%	in calc_delta.

calc_mid(_DG, _Width, []) -> 0;
calc_mid(DG, Width, ChList) ->
    LeftMostX = appmon_dg:get(x, DG, hd(ChList)),
    Z2 = lists:last(ChList),
    RightMostX = appmon_dg:get(x, DG, Z2)+appmon_dg:get(w, DG, Z2),
    trunc((LeftMostX+RightMostX)/2)-trunc(Width/2).

calc_delta(Mid, Right) ->    
    if  Right>Mid	-> Right-Mid;
	true		-> 0
    end.



%% Special head and tail
%% Handles empty list in a non-standard way
tll([]) -> [];
tll([_|T]) -> T.
hdd([]) -> 0;
hdd([H|_]) -> H.

spacex() -> 20.					% Should be macro??
