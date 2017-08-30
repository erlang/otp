%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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

-module(hipe_moves).
-export([new/1,
	 update_movelist/3,
	 node_moves/2,
	 move_related/2,
	 node_movelist/2,
	 get_move/2,
	 is_empty_worklist/1,
	 worklist_get_and_remove/1,
	 remove_worklist/2,
	 remove_active/2,
	 add_worklist/2,
	 add_active/2,
	 member_active/2
	]).
-ifdef(DEBUG_PRINTOUTS).
-export([print_memberships/1]).
-endif.

-record(movesets,
	{worklist,    % Moves enabled for possible coalescing
	 membership,  % Maps move numbers to 'worklist' or 'active' or 'none'
	 moveinsns,   % Maps move numbers to move insns ({Dst,Src}-tuples)
	 movelist     % Mapping from node to list of moves it's associated with
	}).

%%-ifndef(DEBUG).
%%-define(DEBUG,true).
%%-endif.
-include("../main/hipe.hrl").

worklist(MoveSets) -> MoveSets#movesets.worklist.
movelist(MoveSets) -> MoveSets#movesets.movelist.

set_worklist(New_worklist, MoveSets) ->
  MoveSets#movesets{worklist = New_worklist}.
set_movelist(New_movelist, MoveSets) ->
  MoveSets#movesets{movelist = New_movelist}.

update_movelist(Node, MoveList, MoveSets) ->
  set_movelist(hipe_vectors:set(movelist(MoveSets), Node, MoveList),
	       MoveSets).

new(IG) ->
  {MoveList,NrMoves,MoveInsns} = hipe_ig:get_moves(IG),
  Worklist = case NrMoves of 0 -> []; _ -> lists:seq(0, NrMoves-1) end,
  #movesets{worklist	= Worklist,
	    membership	= hipe_bifs:array(NrMoves, 'worklist'),
	    moveinsns	= MoveInsns,
	    movelist	= MoveList}.

remove_worklist(Element, MoveSets) ->
  Membership = MoveSets#movesets.membership,
  %% check for 'worklist' membership here, if debugging
  hipe_bifs:array_update(Membership, Element, 'none'),
  %% Implementing this faithfully would require a SET structure, such
  %% as an ordset or a gb_set. However, removal of elements not at the
  %% head of the structure is a fairly infrequent event (only done by
  %% FreezeMoves()), so instead we let the elements remain but mark
  %% them as being removed. It is the task of worklist_get_and_remove()
  %% to filter out any stale elements.
  MoveSets.

remove_active(Element, MoveSets) ->
  Membership = MoveSets#movesets.membership,
  %% check for 'active' membership here, if debugging
  hipe_bifs:array_update(Membership, Element, 'none'),
  MoveSets.

add_worklist(Element, MoveSets) ->
  Membership = MoveSets#movesets.membership,
  %% check for 'none' membership here, if debugging
  hipe_bifs:array_update(Membership, Element, 'worklist'),
  set_worklist([Element | worklist(MoveSets)], MoveSets).

add_active(Element, MoveSets) ->
  Membership = MoveSets#movesets.membership,
  %% check for 'none' membership here, if debugging
  hipe_bifs:array_update(Membership, Element, 'active'),
  MoveSets.

member_active(Element, MoveSets) ->
  hipe_bifs:array_sub(MoveSets#movesets.membership, Element) =:= 'active'.

is_empty_worklist(MoveSets) ->
  %% This is an approximation. See worklist_get_and_remove().
  worklist(MoveSets) =:= [].

worklist_get_and_remove(MoveSets) ->
  worklist_get_and_remove(worklist(MoveSets), MoveSets#movesets.membership, MoveSets).

worklist_get_and_remove([], _Membership, MoveSets) ->
  {[], set_worklist([], MoveSets)};
worklist_get_and_remove([Move|Worklist], Membership, MoveSets) ->
  case hipe_bifs:array_sub(Membership, Move) of
    'worklist' ->
      hipe_bifs:array_update(Membership, Move, 'none'),
      {Move, set_worklist(Worklist, MoveSets)};
    _ ->
      worklist_get_and_remove(Worklist, Membership, MoveSets)
  end.

node_moves(Node, MoveSets) ->
  Associated = node_movelist(Node, MoveSets),
  Membership = MoveSets#movesets.membership,
  %% The ordsets:union() in hipe_coalescing_regalloc:combine()
  %% constrains us to return an ordset here.
  [X || X <- Associated, hipe_bifs:array_sub(Membership, X) =/= 'none'].

move_related(Node, MoveSets) ->
  %% Same as node_moves(Node, MoveSets) =/= [], but less expensive to compute.
  %% XXX: George&Appel'96 hints that this should be maintained as a per-node counter.
  move_related2(node_movelist(Node, MoveSets), MoveSets#movesets.membership).

move_related2([], _Membership) -> false;
move_related2([Move|MoveSets], Membership) ->
  case hipe_bifs:array_sub(Membership, Move) of
    'none' -> move_related2(MoveSets, Membership);
    _ -> true % 'active' or 'worklist'
  end.

node_movelist(Node, MoveSets) ->
  hipe_vectors:get(movelist(MoveSets), Node).

get_move(Move, MoveSets) ->
  element(Move+1, MoveSets#movesets.moveinsns).

%%----------------------------------------------------------------------
%% Print functions - only used for debugging

-ifdef(DEBUG_PRINTOUTS).
print_memberships(MoveSets) ->
  ?debug_msg("Move memeberships:\n", []),
  Membership = MoveSets#movesets.membership,
  NrMoves = hipe_bifs:array_length(Membership),
  print_membership(NrMoves, Membership).

print_membership(0, _) ->
  true;
print_membership(Element, Membership) ->
  NextElement = Element - 1,
  ?debug_msg("move ~w ~w\n", [NextElement, hipe_bifs:array_sub(Membership, NextElement)]),
  print_membership(NextElement, Membership).
-endif.

