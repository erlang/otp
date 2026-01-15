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
