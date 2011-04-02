%%
%%  wings_facemat.erl --
%%
%%     This module keeps tracks of the mapping from a face number
%%     to its material name.
%%
%%  Copyright (c) 2001-2005 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_facemat.erl,v 1.1 2009/01/25 18:55:33 kostis Exp $
%%
%%
%%

-module(wings_facemat).
-export([all/1,face/2,used_materials/1,mat_faces/2,
	 assign/2,assign/3,
	 delete_face/2,delete_faces/2,keep_faces/2,
	 hide_faces/1,show_faces/1,
	 renumber/2,gc/1,merge/1]).

-include("wings.hrl").
-import(lists, [keysearch/3,reverse/1,reverse/2,sort/1]).

%%%
%%% API functions for retrieving information.
%%%

%% all(We) -> [{Face,MaterialName}]
%%  Return materials for all faces as an ordered list.
all(#we{mat=M}=We) when is_atom(M) ->
    Vis = visible_faces(We),
    make_tab(Vis, M);
all(#we{mat=L}) when is_list(L) ->
    remove_invisible(L).

%% face(Face, We) -> MaterialName
%%  Return the material for the face Face.
face(_, #we{mat=M}) when is_atom(M) -> M;
face(Face, #we{mat=Tab}) ->
    {value,{_,Mat}} = keysearch(Face, 1, Tab),
    Mat.

%% used_materials(We) -> [MaterialName]
%%  Return an ordered list of all materials used in the We.
used_materials(#we{mat=M}) when is_atom(M) -> [M];
used_materials(#we{mat=L}) when is_list(L) ->
    used_materials_1(L, []).

%% mat_faces([{Face,Info}], We) -> [{Mat,[{Face,Info}]}]
%%  Group face tab into groups based on material.
%%  Used for displaying objects.
mat_faces(Ftab, #we{mat=AtomMat}) when is_atom(AtomMat) ->
    [{AtomMat,Ftab}];
mat_faces(Ftab, #we{mat=MatTab}) ->
    mat_faces_1(Ftab, remove_invisible(MatTab), []).

%%%
%%% API functions for updating material name mapping.
%%%

%% assign([{Face,MaterialName}], We) -> We'
%%  Assign materials.
assign([], We)  -> We;
assign([{F,M}|_]=FaceMs, We) when is_atom(M), is_integer(F) ->
    Tab = ordsets:from_list(FaceMs),
    assign_face_ms(Tab, We).

%% assign(MaterialName, Faces, We) -> We'
%%  Assign MaterialName to all faces Faces.
assign(Mat, _, #we{mat=Mat}=We) when is_atom(Mat) -> We;
assign(Mat, Fs, We) when is_atom(Mat), is_list(Fs) ->
    assign_1(Mat, Fs, We);
assign(Mat, Fs, We) when is_atom(Mat) ->
    assign_1(Mat, gb_sets:to_list(Fs), We).

%% delete_face(Face, We) -> We'
%%  Delete the material name mapping for the face Face.
delete_face(_, #we{mat=AtomMat}=We) when is_atom(AtomMat) -> We;
delete_face(Face, #we{mat=MatTab0}=We) ->
    MatTab = orddict:erase(Face, MatTab0),
    We#we{mat=MatTab}.

%% delete_face(Faces, We) -> We'
%%  Delete the material name mapping for all faces Faces.
delete_faces(_, #we{mat=AtomMat}=We) when is_atom(AtomMat) -> We;
delete_faces(Faces0, #we{mat=MatTab0}=We) when is_list(Faces0) ->
    Faces = sofs:from_external(Faces0, [face]),
    MatTab1 = sofs:from_external(MatTab0, [{face,mat}]),
    MatTab2 = sofs:drestriction(MatTab1, Faces),
    MatTab = sofs:to_external(MatTab2),
    We#we{mat=MatTab};
delete_faces(Faces, We) ->
    delete_faces(gb_sets:to_list(Faces), We).

%% keep_faces(Faces, We) -> We'
%%  Delete all the other material names mapping for all faces other Faces.
keep_faces(_, #we{mat=AtomMat}=We) when is_atom(AtomMat) -> We;
keep_faces([Face], We) ->
    Mat = face(Face,We),
    We#we{mat=[{Face,Mat}]};
keep_faces(Faces0, #we{mat=MatTab0}=We) when is_list(Faces0) ->
    Faces = sofs:from_external(Faces0, [face]),
    MatTab1 = sofs:from_external(MatTab0, [{face,mat}]),
    MatTab2 = sofs:restriction(MatTab1, Faces),
    MatTab = sofs:to_external(MatTab2),
    We#we{mat=MatTab};
keep_faces(Faces, We) ->
    keep_faces(gb_sets:to_list(Faces), We).

%% hide_faces(We) -> We'
%%  Update the material name mapping in the We to reflect
%%  the newly hidden faces in the face tab.
hide_faces(#we{mat=M}=We) when is_atom(M) -> We;
hide_faces(#we{mat=L0,fs=Ftab}=We) ->
    L = hide_faces_1(L0, Ftab, []),
    We#we{mat=L}.

%% show_faces(We) -> We'
%%  Update the material name mapping in the We to reflect
%%  that all faces are again visible.
show_faces(#we{mat=M}=We) when is_atom(M) -> We;
show_faces(#we{mat=L0}=We) ->
    L = show_faces_1(L0, []),
    We#we{mat=L}.

%% renumber(MaterialMapping, FaceOldToNew) -> MaterialMapping.
%%  Renumber face number in material name mapping.
renumber(Mat, _) when is_atom(Mat) -> Mat;
renumber(L, Fmap) when is_list(L) -> renumber_1(L, Fmap, []).

%% gc(We) -> We'
%%  Garbage collect the material mapping information, removing
%%  the mapping for any face no longer present in the face table.
gc(#we{mat=Mat}=We) when is_atom(Mat) -> We;
gc(#we{mat=Tab0,fs=Ftab}=We) ->
    Fs = sofs:from_external(gb_trees:keys(Ftab), [face]),
    Tab1 = sofs:from_external(Tab0, [{face,material}]),
    Tab2 = sofs:restriction(Tab1, Fs),
    Tab = sofs:to_external(Tab2),
    We#we{mat=compress(Tab)}.

%% merge([We]) -> [{Face,MaterialName}] | MaterialName.
%%  Merge materials for several objects.
merge([#we{mat=M}|Wes]=L) when is_atom(M) ->
    case merge_all_same(Wes, M) of
	true -> M;
	false -> merge_1(L, [])
    end;
merge(L) -> merge_1(L, []).

merge_1([#we{mat=M,es=Etab}|T], Acc) when is_atom(M) ->
    FsM = merge_2(gb_trees:values(Etab), M, []),
    merge_1(T, [FsM|Acc]);
merge_1([#we{mat=FsMs}|T], Acc) ->
    merge_1(T, [FsMs|Acc]);
merge_1([], Acc) -> lists:merge(Acc).

merge_2([#edge{lf=Lf,rf=Rf}|T], M, Acc) ->
    merge_2(T, M, [{Lf,M},{Rf,M}|Acc]);
merge_2([], _, Acc) -> ordsets:from_list(Acc).

merge_all_same([#we{mat=M}|Wes], M) -> merge_all_same(Wes, M);
merge_all_same([_|_], _) -> false;
merge_all_same([], _) -> true.

%%%
%%% Local functions.
%%%

assign_1(Mat, Fs, #we{fs=Ftab}=We) ->
    case length(Fs) =:= gb_trees:size(Ftab) of
	true -> We#we{mat=Mat};
	false -> assign_2(Mat, Fs, We)
    end.

assign_2(Mat, Fs0, #we{fs=Ftab,mat=Mat0}=We) when is_atom(Mat0) ->
    Fs = ordsets:from_list(Fs0),
    OtherFaces = ordsets:subtract(gb_trees:keys(Ftab), Fs),
    Tab0 = make_tab(OtherFaces, Mat0),
    Tab1 = make_tab(Fs, Mat),
    Tab = lists:merge(Tab0, Tab1),
    We#we{mat=Tab};
assign_2(Mat, Fs0, #we{mat=Tab0}=We) when is_list(Tab0) ->
    Fs = ordsets:from_list(Fs0),
    Tab1 = make_tab(Fs, Mat),
    Tab = mat_merge(Tab1, Tab0, []),
    We#we{mat=Tab}.

assign_face_ms(Tab, #we{fs=Ftab}=We) ->
    case length(Tab) =:= gb_trees:size(Ftab) of
	true -> We#we{mat=compress(Tab)};
	false -> assign_face_ms_1(Tab, We)
    end.

assign_face_ms_1(Tab1, #we{fs=Ftab,mat=Mat0}=We) when is_atom(Mat0) ->
    Tab0 = make_tab(gb_trees:keys(Ftab), Mat0),
    Tab = mat_merge(Tab1, Tab0, []),
    We#we{mat=Tab};
assign_face_ms_1(Tab1, #we{mat=Tab0}=We) when is_list(Tab0) ->
    Tab = mat_merge(Tab1, Tab0, []),
    We#we{mat=Tab}.

mat_merge([{Fn,_}|_]=Fns, [{Fo,_}=Fold|Fos], Acc) when Fo < Fn ->
    mat_merge(Fns, Fos, [Fold|Acc]);
mat_merge([{Fn,_}=Fnew|Fns], [{Fo,_}|_]=Fos, Acc) when Fo > Fn ->
    mat_merge(Fns, Fos, [Fnew|Acc]);
mat_merge([Fnew|Fns], [_|Fos], Acc) -> % Equality
    mat_merge(Fns, Fos, [Fnew|Acc]);
mat_merge([], Fos, Acc) ->
    rev_compress(Acc, Fos);
mat_merge(Fns, [], Acc) ->
    rev_compress(Acc, Fns).

make_tab(Fs, M) ->
    make_tab_1(Fs, M, []).

make_tab_1([F|Fs], M, Acc) ->
    make_tab_1(Fs, M, [{F,M}|Acc]);
make_tab_1([], _, Acc) -> reverse(Acc).


visible_faces(#we{fs=Ftab}) ->
    visible_faces_1(gb_trees:keys(Ftab)).

visible_faces_1([F|Fs]) when F < 0 ->
    visible_faces_1(Fs);
visible_faces_1(Fs) -> Fs.

remove_invisible([{F,_}|Fs]) when F < 0 ->
    remove_invisible(Fs);
remove_invisible(Fs) -> Fs.

hide_faces_1([{F,_}=P|Fms], Ftab, Acc) when F < 0 ->
    hide_faces_1(Fms, Ftab, [P|Acc]);
hide_faces_1([{F,M}=P|Fms], Ftab, Acc) ->
    case gb_trees:is_defined(F, Ftab) of
	false -> hide_faces_1(Fms, Ftab, [{-F-1,M}|Acc]);
	true -> hide_faces_1(Fms, Ftab, [P|Acc])
    end;
hide_faces_1([], _, Acc) -> sort(Acc).

show_faces_1([{F,M}|Fms], Acc) when F < 0 ->
    show_faces_1(Fms, [{-F-1,M}|Acc]);
show_faces_1(Fs, Acc) -> sort(Acc++Fs).

renumber_1([{F,M}|T], Fmap, Acc) ->
    renumber_1(T, Fmap, [{gb_trees:get(F, Fmap),M}|Acc]);
renumber_1([], _, Acc) -> sort(Acc).

%% rev_compress([{Face,Mat}], [{Face,Mat}]) -> [{Face,Mat}] | Mat.
%%  Reverse just like lists:reverse/2, but if all materials
%%  turns out to be just the same, return that material.
rev_compress(L, Acc) ->
    case same_mat(Acc) of
	[] -> reverse(L, Acc);
	M -> rev_compress_1(L, M, Acc)
    end.

rev_compress_1([{_,M}=E|T], M, Acc) ->
    %% Same material.
    rev_compress_1(T, M, [E|Acc]);
rev_compress_1([_|_]=L, _, Acc) ->
    %% Another material. Finish by using reverse/2.
    reverse(L, Acc);
rev_compress_1([], M, _) ->
    %% All materials turned out to be the same.
    M.

%% compress(MaterialTab) -> [{Face,Mat}] | Mat.
%%  Compress a face mapping if possible.
compress(M) when is_atom(M) -> M;
compress(L) when is_list(L) ->
    case same_mat(L) of
	[] -> L;
	M -> M
    end.

same_mat([]) -> [];
same_mat([{_,M}|T]) -> same_mat_1(T, M).

same_mat_1([{_,M}|T], M) -> same_mat_1(T, M);
same_mat_1([], M) -> M;
same_mat_1(_, _) -> [].

used_materials_1([{_,M}|T], [M|_]=Acc) ->
    used_materials_1(T, Acc);
used_materials_1([{_,M}|T], Acc) ->
    used_materials_1(T, [M|Acc]);
used_materials_1([], Acc) ->
    ordsets:from_list(Acc).

mat_faces_1([{F1,_}|_]=Fs, [{F2,_}|Ms], Acc) when F2 < F1 ->
    mat_faces_1(Fs, Ms, Acc);
mat_faces_1([{F,Info}|Fs], [{F,Mat}|Ms], Acc) ->
    mat_faces_1(Fs, Ms, [{Mat,{F,Info}}|Acc]);
mat_faces_1([], _, Acc) -> wings_util:rel2fam(Acc).
