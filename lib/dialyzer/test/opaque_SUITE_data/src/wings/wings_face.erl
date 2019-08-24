%%
%%  wings_face.erl --
%%
%%     This module contains help routines for faces, such as fold functions
%%     face iterators.
%%

-module(wings_face).

-export([delete_bad_faces/2, fold/4, fold_faces/4, from_edges/2,
	 inner_edges/2, to_edges/2, other/2]).

-include("wings.hrl").

from_edges(Es, #we{es=Etab}) when is_list(Es) ->
    from_edges_1(Es, Etab, []);
from_edges(Es, We) ->
    from_edges(gb_sets:to_list(Es), We).

from_edges_1([E|Es], Etab, Acc) ->
    #edge{lf=Lf,rf=Rf} = gb_trees:get(E, Etab),
    from_edges_1(Es, Etab, [Lf,Rf|Acc]);
from_edges_1([], _, Acc) -> gb_sets:from_list(Acc).

%% other(Face, EdgeRecord) -> OtherFace
%%  Pick up the "other face" from an edge record.
other(Face, #edge{lf=Face,rf=Other}) -> Other;
other(Face, #edge{rf=Face,lf=Other}) -> Other.

%% to_edges(Faces, We) -> [Edge]
%%  Convert a set or list of faces to a list of edges.
to_edges(Fs, We) ->
    ordsets:from_list(to_edges_raw(Fs, We)).

%% inner_edges(Faces, We) -> [Edge]
%%  Given a set of faces, return all inner edges.
inner_edges(Faces, We) ->
    S = to_edges_raw(Faces, We),
    inner_edges_1(lists:sort(S), []).

inner_edges_1([E,E|T], In) ->
    inner_edges_1(T, [E|In]);
inner_edges_1([_|T], In) ->
    inner_edges_1(T, In);
inner_edges_1([], In) -> lists:reverse(In).

%% Fold over all edges surrounding a face.

fold(F, Acc, Face, #we{es=Etab,fs=Ftab}) ->
    Edge = gb_trees:get(Face, Ftab),
    fold(Edge, Etab, F, Acc, Face, Edge, not_done).

fold(LastEdge, _, _, Acc, _, LastEdge, done) -> Acc;
fold(Edge, Etab, F, Acc0, Face, LastEdge, _) ->
    case gb_trees:get(Edge, Etab) of
	#edge{ve=V,lf=Face,ltsu=NextEdge}=E ->
	    Acc = F(V, Edge, E, Acc0),
	    fold(NextEdge, Etab, F, Acc, Face, LastEdge, done);
	#edge{vs=V,rf=Face,rtsu=NextEdge}=E ->
	    Acc = F(V, Edge, E, Acc0),
	    fold(NextEdge, Etab, F, Acc, Face, LastEdge, done)
    end.

%% Fold over a set of faces.

fold_faces(F, Acc0, [Face|Faces], #we{es=Etab,fs=Ftab}=We) ->
    Edge = gb_trees:get(Face, Ftab),
    Acc = fold_faces_1(Edge, Etab, F, Acc0, Face, Edge, not_done),
    fold_faces(F, Acc, Faces, We);
fold_faces(_F, Acc, [], _We) -> Acc;
fold_faces(F, Acc, Faces, We) ->
    fold_faces(F, Acc, gb_sets:to_list(Faces), We).

fold_faces_1(LastEdge, _, _, Acc, _, LastEdge, done) -> Acc;
fold_faces_1(Edge, Etab, F, Acc0, Face, LastEdge, _) ->
    case gb_trees:get(Edge, Etab) of
	#edge{ve=V,lf=Face,ltsu=NextEdge}=E ->
	    Acc = F(Face, V, Edge, E, Acc0),
	    fold_faces_1(NextEdge, Etab, F, Acc, Face, LastEdge, done);
	#edge{vs=V,rf=Face,rtsu=NextEdge}=E ->
	    Acc = F(Face, V, Edge, E, Acc0),
	    fold_faces_1(NextEdge, Etab, F, Acc, Face, LastEdge, done)
    end.

%% Return an unsorted list of edges for the faces (with duplicates).

to_edges_raw(Faces, #we{es=Etab,fs=Ftab}) when is_list(Faces) ->
    to_edges_raw(Faces, Ftab, Etab, []);
to_edges_raw(Faces, We) ->
    to_edges_raw(gb_sets:to_list(Faces), We).

to_edges_raw([Face|Faces], Ftab, Etab, Acc0) ->
    Edge = gb_trees:get(Face, Ftab),
    Acc = to_edges_raw_1(Edge, Etab, Acc0, Face, Edge, not_done),
    to_edges_raw(Faces, Ftab, Etab, Acc);
to_edges_raw([], _, _, Acc) -> Acc.

to_edges_raw_1(LastEdge, _, Acc, _, LastEdge, done) -> Acc;
to_edges_raw_1(Edge, Etab, Acc, Face, LastEdge, _) ->
    case gb_trees:get(Edge, Etab) of
	#edge{lf=Face,ltsu=NextEdge} ->
	    to_edges_raw_1(NextEdge, Etab, [Edge|Acc], Face, LastEdge, done);
	#edge{rf=Face,rtsu=NextEdge} ->
	    to_edges_raw_1(NextEdge, Etab, [Edge|Acc], Face, LastEdge, done)
    end.

delete_bad_faces(Fs, #we{fs=Ftab,es=Etab}=We) when is_list(Fs) ->
    Es = bad_edges(Fs, Ftab, Etab, []),
    wings_edge:dissolve_edges(Es, We);
delete_bad_faces(Fs, We) ->
    delete_bad_faces(gb_sets:to_list(Fs), We).

bad_edges([F|Fs], Ftab, Etab, Acc) ->
    case gb_trees:lookup(F, Ftab) of
	{value,Edge} ->
	    case gb_trees:get(Edge, Etab) of
		#edge{ltpr=Same,ltsu=Same,rtpr=Same,rtsu=Same} ->
		    erlang:error({internal_error,one_edged_face,F});
		#edge{ltpr=Same,ltsu=Same} ->
		    bad_edges(Fs, Ftab, Etab, [Edge|Acc]);
		#edge{rtpr=Same,rtsu=Same} ->
		    bad_edges(Fs, Ftab, Etab, [Edge|Acc]);
		_ -> bad_edges(Fs, Ftab, Etab, Acc)
	    end;
	none -> bad_edges(Fs, Ftab, Etab, Acc)
    end;
bad_edges([], _, _, Acc) -> Acc.
