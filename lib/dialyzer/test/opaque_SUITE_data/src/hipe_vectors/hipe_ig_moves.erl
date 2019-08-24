%% -*- erlang-indent-level: 2 -*-
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

-module(hipe_ig_moves).
-export([new/1,
	 new_move/3,
	 get_moves/1]).

%%-----------------------------------------------------------------------------
%% The main data structure; its fields are:
%%  - movelist  : mapping from temp to set of associated move numbers
%%  - nrmoves   : number of distinct move instructions seen so far
%%  - moveinsns : list of move instructions, in descending move number order
%%  - moveset   : set of move instructions

-record(ig_moves, {movelist                    :: movelist(),
		   nrmoves   = 0               :: non_neg_integer(),
		   moveinsns = []              :: [{_,_}],
		   moveset   = gb_sets:empty() :: gb_sets:set()}).

-type movelist() :: hipe_vectors:vector(ordsets:ordset(non_neg_integer())).

%%-----------------------------------------------------------------------------

-spec new(non_neg_integer()) -> #ig_moves{}.

new(NrTemps) ->
  MoveList = hipe_vectors:new(NrTemps, ordsets:new()),
  #ig_moves{movelist = MoveList}.

-spec new_move(_, _, #ig_moves{}) -> #ig_moves{}.

new_move(Dst, Src, IG_moves) ->
  MoveSet = IG_moves#ig_moves.moveset,
  MoveInsn = {Dst, Src},
  case gb_sets:is_member(MoveInsn, MoveSet) of
    true ->
      IG_moves;
    false ->
      MoveNr = IG_moves#ig_moves.nrmoves,
      Movelist0 = IG_moves#ig_moves.movelist,
      Movelist1 = add_movelist(MoveNr, Dst,
			       add_movelist(MoveNr, Src, Movelist0)),
      IG_moves#ig_moves{nrmoves = MoveNr+1,
			movelist = Movelist1,
			moveinsns = [MoveInsn|IG_moves#ig_moves.moveinsns],
			moveset = gb_sets:insert(MoveInsn, MoveSet)}
  end.

-spec add_movelist(non_neg_integer(), non_neg_integer(), movelist())
		  -> movelist().

add_movelist(MoveNr, Temp, MoveList) ->
  AssocMoves = hipe_vectors:get(MoveList, Temp),
  %% XXX: MoveNr does not occur in moveList[Temp], but the new list must be an
  %% ordset due to the ordsets:union in hipe_coalescing_regalloc:combine().
  hipe_vectors:set(MoveList, Temp, ordsets:add_element(MoveNr, AssocMoves)).

-spec get_moves(#ig_moves{}) -> {movelist(), non_neg_integer(), tuple()}.

get_moves(IG_moves) -> % -> {MoveList, NrMoves, MoveInsns}
  {IG_moves#ig_moves.movelist,
   IG_moves#ig_moves.nrmoves,
   list_to_tuple(lists:reverse(IG_moves#ig_moves.moveinsns))}.
