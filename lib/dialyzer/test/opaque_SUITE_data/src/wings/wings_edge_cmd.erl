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
