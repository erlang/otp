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
