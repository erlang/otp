%%
%%  wings_edge.erl --
%%
%%     This module contains most edge command and edge utility functions.
%%
%%  Copyright (c) 2001-2008 Bjorn Gustavsson.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_edge.erl,v 1.1 2009/01/25 18:55:33 kostis Exp $
%%

-module(wings_edge).

-export([dissolve_edges/2]).

-include("wings.hrl").

%%%
%%% Dissolve.
%%%

dissolve_edges(Edges0, We0) when is_list(Edges0) ->
    #we{es=Etab} = We1 = lists:foldl(fun internal_dissolve_edge/2, We0, Edges0),
    case [E || E <- Edges0, gb_trees:is_defined(E, Etab)] of
	Edges0 ->
	    %% No edge was deleted in the last pass. We are done.
	    We = wings_we:rebuild(We0#we{vc=undefined}),
	    wings_we:validate_mirror(We);
	Edges ->
	    dissolve_edges(Edges, We1)
    end;
dissolve_edges(Edges, We) ->
    dissolve_edges(gb_sets:to_list(Edges), We).

internal_dissolve_edge(Edge, #we{es=Etab}=We0) ->
    case gb_trees:lookup(Edge, Etab) of
	none -> We0;
	{value,#edge{ltpr=Same,ltsu=Same,rtpr=Same,rtsu=Same}} ->
	    Empty = gb_trees:empty(),
	    We0#we{vc=Empty,vp=Empty,es=Empty,fs=Empty,he=gb_sets:empty()};
	{value,#edge{rtpr=Back,ltsu=Back}=Rec} ->
	    merge_edges(backward, Edge, Rec, We0);
	{value,#edge{rtsu=Forward,ltpr=Forward}=Rec} ->
	    merge_edges(forward, Edge, Rec, We0);
	{value,Rec} ->
	    try dissolve_edge_1(Edge, Rec, We0) of
		We -> We
	    catch
		throw:hole -> We0
	    end
    end.

%% dissolve_edge_1(Edge, EdgeRecord, We) -> We
%%  Remove an edge and a face. If one of the faces is degenerated
%%  (only consists of two edges), remove that one. Otherwise, it
%%  doesn't matter which face we remove.
dissolve_edge_1(Edge, #edge{lf=Remove,rf=Keep,ltpr=Same,ltsu=Same}=Rec, We) ->
    dissolve_edge_2(Edge, Remove, Keep, Rec, We);
dissolve_edge_1(Edge, #edge{lf=Keep,rf=Remove}=Rec, We) ->
    dissolve_edge_2(Edge, Remove, Keep, Rec, We).

dissolve_edge_2(Edge, FaceRemove, FaceKeep,
		#edge{ltpr=LP,ltsu=LS,rtpr=RP,rtsu=RS},
		#we{fs=Ftab0,es=Etab0,he=Htab0}=We0) ->
    %% First change face for all edges surrounding the face we will remove.
    Etab1 = wings_face:fold(
	      fun (_, E, _, IntEtab) when E =:= Edge -> IntEtab;
		  (_, E, R, IntEtab) ->
		      case R of
			  #edge{lf=FaceRemove,rf=FaceKeep} ->
			      throw(hole);
			  #edge{rf=FaceRemove,lf=FaceKeep} ->
			      throw(hole);
			  #edge{lf=FaceRemove} ->
			      gb_trees:update(E, R#edge{lf=FaceKeep}, IntEtab);
			  #edge{rf=FaceRemove} ->
			      gb_trees:update(E, R#edge{rf=FaceKeep}, IntEtab)
		      end
	      end, Etab0, FaceRemove, We0),

    %% Patch all predecessors and successor of the edge we will remove.
    Etab2 = patch_edge(LP, RS, Edge, Etab1),
    Etab3 = patch_edge(LS, RP, Edge, Etab2),
    Etab4 = patch_edge(RP, LS, Edge, Etab3),
    Etab5 = patch_edge(RS, LP, Edge, Etab4),

    %% Remove the edge.
    Etab = gb_trees:delete(Edge, Etab5),
    Htab = hardness(Edge, soft, Htab0),

    %% Remove the face. Patch the face entry for the remaining face.
    Ftab1 = gb_trees:delete(FaceRemove, Ftab0),
    We1 = wings_facemat:delete_face(FaceRemove, We0),
    Ftab = gb_trees:update(FaceKeep, LP, Ftab1),

    %% Return result.
    We = We1#we{es=Etab,fs=Ftab,vc=undefined,he=Htab},
    AnEdge = gb_trees:get(FaceKeep, Ftab),
    case gb_trees:get(AnEdge, Etab) of
	#edge{lf=FaceKeep,ltpr=Same,ltsu=Same} ->
	    internal_dissolve_edge(AnEdge, We);
	#edge{rf=FaceKeep,rtpr=Same,rtsu=Same} ->
	    internal_dissolve_edge(AnEdge, We);
	_Other ->
	    case wings_we:is_face_consistent(FaceKeep, We) of
		true ->
		    We;
		false ->
		    io:format("Dissolving would cause a badly formed face.")
	    end
    end.

%%
%% We like winged edges, but not winged vertices (a vertex with
%% only two edges connected to it). We will remove the winged vertex
%% by joining the two edges connected to it.
%%

merge_edges(Dir, Edge, Rec, #we{es=Etab}=We) ->
    {Va,Vb,_,_,_,_,To,To} = half_edge(Dir, Rec),
    case gb_trees:get(To, Etab) of
	#edge{vs=Va,ve=Vb} ->
	    del_2edge_face(Dir, Edge, Rec, To, We);
	#edge{vs=Vb,ve=Va} ->
	    del_2edge_face(Dir, Edge, Rec, To, We);
	_Other ->
	    merge_1(Dir, Edge, Rec, To, We)
    end.

merge_1(Dir, Edge, Rec, To, #we{es=Etab0,fs=Ftab0,he=Htab0}=We) ->
    OtherDir = reverse_dir(Dir),
    {Vkeep,Vdelete,Lf,Rf,A,B,L,R} = half_edge(OtherDir, Rec),
    Etab1 = patch_edge(L, To, Edge, Etab0),
    Etab2 = patch_edge(R, To, Edge, Etab1),
    Etab3 = patch_half_edge(To, Vkeep, Lf, A, L, Rf, B, R, Vdelete, Etab2),
    Htab = hardness(Edge, soft, Htab0),
    Etab = gb_trees:delete(Edge, Etab3),
    #edge{lf=Lf,rf=Rf} = Rec,
    Ftab1 = update_face(Lf, To, Edge, Ftab0),
    Ftab = update_face(Rf, To, Edge, Ftab1),
    merge_2(To, We#we{es=Etab,fs=Ftab,he=Htab,vc=undefined}).

merge_2(Edge, #we{es=Etab}=We) ->
    %% If the merged edge is part of a two-edge face, we must
    %% remove that edge too.
    case gb_trees:get(Edge, Etab) of
	#edge{ltpr=Same,ltsu=Same} ->
	    internal_dissolve_edge(Edge, We);
	#edge{rtpr=Same,rtsu=Same} ->
	    internal_dissolve_edge(Edge, We);
	_Other -> We
    end.

update_face(Face, Edge, OldEdge, Ftab) ->
    case gb_trees:get(Face, Ftab) of
	OldEdge -> gb_trees:update(Face, Edge, Ftab);
	_Other -> Ftab
    end.

del_2edge_face(Dir, EdgeA, RecA, EdgeB,
	       #we{es=Etab0,fs=Ftab0,he=Htab0}=We) ->
    {_,_,Lf,Rf,_,_,_,_} = half_edge(reverse_dir(Dir), RecA),
    RecB = gb_trees:get(EdgeB, Etab0),
    Del = gb_sets:from_list([EdgeA,EdgeB]),
    EdgeANear = stabile_neighbor(RecA, Del),
    EdgeBNear = stabile_neighbor(RecB, Del),
    Etab1 = patch_edge(EdgeANear, EdgeBNear, EdgeA, Etab0),
    Etab2 = patch_edge(EdgeBNear, EdgeANear, EdgeB, Etab1),
    Etab3 = gb_trees:delete(EdgeA, Etab2),
    Etab = gb_trees:delete(EdgeB, Etab3),

    %% Patch hardness table.
    Htab1 = hardness(EdgeA, soft, Htab0),
    Htab = hardness(EdgeB, soft, Htab1),

    %% Patch the face table.
    #edge{lf=Klf,rf=Krf} = gb_trees:get(EdgeANear, Etab),
    KeepFaces = ordsets:from_list([Klf,Krf]),
    EdgeAFaces = ordsets:from_list([Lf,Rf]),
    [DelFace] = ordsets:subtract(EdgeAFaces, KeepFaces),
    Ftab1 = gb_trees:delete(DelFace, Ftab0),
    [KeepFace] = ordsets:intersection(KeepFaces, EdgeAFaces),
    Ftab2 = update_face(KeepFace, EdgeANear, EdgeA, Ftab1),
    Ftab = update_face(KeepFace, EdgeBNear, EdgeB, Ftab2),

    %% Return result.
    We#we{vc=undefined,es=Etab,fs=Ftab,he=Htab}.

stabile_neighbor(#edge{ltpr=Ea,ltsu=Eb,rtpr=Ec,rtsu=Ed}, Del) ->
    [Edge] = lists:foldl(fun(E, A) ->
				 case gb_sets:is_member(E, Del) of
				     true -> A;
				     false -> [E|A]
			   end
			 end, [], [Ea,Eb,Ec,Ed]),
    Edge.

%%%
%%% Setting hard/soft edges.
%%%

hardness(Edge, soft, Htab) -> gb_sets:delete_any(Edge, Htab);
hardness(Edge, hard, Htab) -> gb_sets:add(Edge, Htab).

%%%
%%% Utilities.
%%%

reverse_dir(forward) -> backward;
reverse_dir(backward) -> forward.

half_edge(backward, #edge{vs=Va,ve=Vb,lf=Lf,rf=Rf,a=A,b=B,ltsu=L,rtpr=R}) ->
    {Va,Vb,Lf,Rf,A,B,L,R};
half_edge(forward, #edge{ve=Va,vs=Vb,lf=Lf,rf=Rf,a=A,b=B,ltpr=L,rtsu=R}) ->
    {Va,Vb,Lf,Rf,A,B,L,R}.

patch_half_edge(Edge, V, FaceA, A, Ea, FaceB, B, Eb, OrigV, Etab) ->
    New = case gb_trees:get(Edge, Etab) of
	      #edge{vs=OrigV,lf=FaceA,rf=FaceB}=Rec ->
		  Rec#edge{a=A,vs=V,ltsu=Ea,rtpr=Eb};
	      #edge{vs=OrigV,lf=FaceB,rf=FaceA}=Rec ->
		  Rec#edge{a=B,vs=V,ltsu=Eb,rtpr=Ea};
	      #edge{ve=OrigV,lf=FaceA,rf=FaceB}=Rec ->
		  Rec#edge{b=B,ve=V,ltpr=Ea,rtsu=Eb};
	      #edge{ve=OrigV,lf=FaceB,rf=FaceA}=Rec ->
		  Rec#edge{b=A,ve=V,ltpr=Eb,rtsu=Ea}
	  end,
    gb_trees:update(Edge, New, Etab).

patch_edge(Edge, ToEdge, OrigEdge, Etab) ->
    New = case gb_trees:get(Edge, Etab) of
	      #edge{ltsu=OrigEdge}=R ->
		  R#edge{ltsu=ToEdge};
	      #edge{ltpr=OrigEdge}=R ->
		  R#edge{ltpr=ToEdge};
	      #edge{rtsu=OrigEdge}=R ->
		  R#edge{rtsu=ToEdge};
	      #edge{rtpr=OrigEdge}=R ->
		  R#edge{rtpr=ToEdge}
	  end,
    gb_trees:update(Edge, New, Etab).
