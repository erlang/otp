%% This software is copyrighted by Bjorn Gustavsson, and other parties.
%% The following terms apply to all files associated with the software unless
%% explicitly disclaimed in individual files.
%%
%% The authors hereby grant permission to use, copy, modify, distribute,
%% and license this software and its documentation for any purpose, provided
%% that existing copyright notices are retained in all copies and that this
%% notice is included verbatim in any distributions. No written agreement,
%% license, or royalty fee is required for any of the authorized uses.
%% Modifications to this software may be copyrighted by their authors
%% and need not follow the licensing terms described here, provided that
%% the new terms are clearly indicated on the first page of each file where
%% they apply.
%%
%% IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY
%% FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
%% ARISING OUT OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY
%% DERIVATIVES THEREOF, EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
%%
%% THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE
%% IS PROVIDED ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE
%% NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
%% MODIFICATIONS.
%%
%% GOVERNMENT USE: If you are acquiring this software on behalf of the
%% U.S. government, the Government shall have only "Restricted Rights"
%% in the software and related documentation as defined in the Federal
%% Acquisition Regulations (FARs) in Clause 52.227.19 (c) (2).  If you
%% are acquiring the software on behalf of the Department of Defense, the
%% software shall be classified as "Commercial Computer Software" and the
%% Government shall have only "Restricted Rights" as defined in Clause
%% 252.227-7013 (c) (1) of DFARs.  Notwithstanding the foregoing, the
%% authors grant the U.S. Government and others acting in its behalf
%% permission to use and distribute the software in accordance with the
%% terms specified in this license.


%%
%%  wings_edge.erl --
%%
%%     This module contains most edge command and edge utility functions.
%%

-module(wings_edge_cmd).

-export([loop_cut/1]).

-include("wings.hrl").

%%%
%%% The Loop Cut command.
%%%

loop_cut(St0) ->
    {Sel,St} = wings_sel:fold(fun loop_cut/3, {[],St0}, St0),
    wings_sel:set(body, Sel, St).

loop_cut(Edges, #we{name=Name,id=Id,fs=Ftab}=We0, {Sel,St0}) ->
    AdjFaces = wings_face:from_edges(Edges, We0),
    case loop_cut_partition(AdjFaces, Edges, We0, []) of
	[_] ->
	    io:format("Edge loop doesn't divide ~p into two parts.", [Name]);
	Parts0 ->
	    %% We arbitrarily decide that the largest part of the object
	    %% will be left unselected and will keep the name of the object.

	    Parts1 = [{gb_trees:size(P),P} || P <- Parts0],
	    Parts2 = lists:reverse(lists:sort(Parts1)),
	    [_|Parts] = [gb_sets:to_list(P) || {_,P} <- Parts2],

	    %% Also, this first part will also contain any sub-object
	    %% that was not reachable from any of the edges. Therefore,
	    %% we calculate the first part as the complement of the union
	    %% of all other parts.

	    FirstComplement = ordsets:union(Parts),
	    First = ordsets:subtract(gb_trees:keys(Ftab), FirstComplement),

	    We = wings_dissolve:complement(First, We0),
	    Shs = St0#st.shapes,
	    St = St0#st{shapes=gb_trees:update(Id, We, Shs)},
	    loop_cut_make_copies(Parts, We0, Sel, St)
    end.

loop_cut_make_copies([P|Parts], We0, Sel0, #st{onext=Id}=St0) ->
    Sel = [{Id,gb_sets:singleton(0)}|Sel0],
    We = wings_dissolve:complement(P, We0),
    St = wings_shape:insert(We, cut, St0),
    loop_cut_make_copies(Parts, We0, Sel, St);
loop_cut_make_copies([], _, Sel, St) -> {Sel,St}.

loop_cut_partition(Faces0, Edges, We, Acc) ->
    case gb_sets:is_empty(Faces0) of
	true -> Acc;
	false ->
	    {AFace,Faces1} = gb_sets:take_smallest(Faces0),
	    Reachable = collect_faces(AFace, Edges, We),
	    Faces = gb_sets:difference(Faces1, Reachable),
	    loop_cut_partition(Faces, Edges, We, [Reachable|Acc])
    end.

collect_faces(Face, Edges, We) ->
    collect_faces(gb_sets:singleton(Face), We, Edges, gb_sets:empty()).

collect_faces(Work0, We, Edges, Acc0) ->
    case gb_sets:is_empty(Work0) of
	true -> Acc0;
	false ->
	    {Face,Work1} = gb_sets:take_smallest(Work0),
	    Acc = gb_sets:insert(Face, Acc0),
	    Work = collect_maybe_add(Work1, Face, Edges, We, Acc),
	    collect_faces(Work, We, Edges, Acc)
    end.

collect_maybe_add(Work, Face, Edges, We, Res) ->
    wings_face:fold(
      fun(_, Edge, Rec, A) ->
	      case gb_sets:is_member(Edge, Edges) of
		  true -> A;
		  false ->
		      Of = wings_face:other(Face, Rec),
		      case gb_sets:is_member(Of, Res) of
			  true -> A;
			  false -> gb_sets:add(Of, A)
		      end
	      end
      end, Work, Face, We).
