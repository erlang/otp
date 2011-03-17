%%
%%  wings_sel.erl --
%%
%%     This module implements selection utilities.
%%

-module(wings_sel).

-export([face_regions/2, fold/3, set/3]).

-include("wings.hrl").

set(Mode, Sel, St) ->
    St#st{selmode=Mode, sel=lists:sort(Sel), sh=false}.

%%%
%%% Fold over the selection.
%%%

fold(F, Acc, #st{sel=Sel,shapes=Shapes}) ->
    fold_1(F, Acc, Shapes, Sel).

fold_1(F, Acc0, Shapes, [{Id,Items}|T]) ->
    We = gb_trees:get(Id, Shapes),
    ?ASSERT(We#we.id =:= Id),
    fold_1(F, F(Items, We, Acc0), Shapes, T);
fold_1(_F, Acc, _Shapes, []) -> Acc.

%%%
%%% Divide the face selection into regions where each face shares at least
%%% one edge with another face in the same region. Two faces can share a
%%% vertex without necessarily being in the same region.
%%%

face_regions(Faces, We) when is_list(Faces) ->
    face_regions_1(gb_sets:from_list(Faces), We);
face_regions(Faces, We) ->
    face_regions_1(Faces, We).

face_regions_1(Faces, We) ->
    find_face_regions(Faces, We, fun collect_face_fun/5, []).

find_face_regions(Faces0, We, Coll, Acc) ->
    case gb_sets:is_empty(Faces0) of
	true -> Acc;
	false ->
	    {Face,Faces1} = gb_sets:take_smallest(Faces0),
	    Ws = [Face],
	    {Reg,Faces} = collect_face_region(Ws, We, Coll, [], Faces1),
	    find_face_regions(Faces, We, Coll, [Reg|Acc])
    end.

collect_face_region([_|_]=Ws0, We, Coll, Reg0, Faces0) ->
    Reg = Ws0++Reg0,
    {Ws,Faces} = wings_face:fold_faces(Coll, {[],Faces0}, Ws0, We),
    collect_face_region(Ws, We, Coll, Reg, Faces);
collect_face_region([], _, _, Reg, Faces) ->
    {gb_sets:from_list(Reg),Faces}.

collect_face_fun(Face, _, _, Rec, {Ws,Faces}=A) ->
    Of = case Rec of
	     #edge{lf=Face,rf=Of0} -> Of0;
	     #edge{rf=Face,lf=Of0} -> Of0
	 end,
    case gb_sets:is_member(Of, Faces) of
	true -> {[Of|Ws],gb_sets:delete(Of, Faces)};
	false -> A
    end.
