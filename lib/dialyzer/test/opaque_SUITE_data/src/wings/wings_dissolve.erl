%%
%%  wings_dissolve.erl --
%%
%%     This module implements dissolve of faces.
%%

-module(wings_dissolve).

-export([faces/2, complement/2]).

-include("wings.hrl").

%% faces([Face], We) -> We'
%%  Dissolve the given faces.
faces([], We) -> We;
faces(Faces, #we{fs=Ftab0}=We) ->
    case gb_sets:is_empty(Faces) of
	true -> We;
	false when is_list(Faces) ->
	    Complement = ordsets:subtract(gb_trees:keys(Ftab0),
					  ordsets:from_list(Faces)),
	    dissolve_1(Faces, Complement, We);
	false ->
	    Complement = ordsets:subtract(gb_trees:keys(Ftab0),
					  gb_sets:to_list(Faces)),
	    dissolve_1(Faces, Complement, We)
    end.

faces([], _, We) -> We;
faces(Faces,Complement,We) ->
    case gb_sets:is_empty(Faces) of
        true -> We;
        false -> dissolve_1(Faces, Complement,We)
    end.

dissolve_1(Faces, Complement, We0) ->
    We1 = optimistic_dissolve(Faces,Complement,We0#we{vc=undefined}),
    NewFaces = wings_we:new_items_as_ordset(face, We0, We1),
    We2 = wings_face:delete_bad_faces(NewFaces, We1),
    We = wings_we:rebuild(We2),
    case wings_we:is_consistent(We) of
	true ->
	    We;
	false ->
	    io:format("Dissolving would cause an inconsistent object structure.")
    end.

%% complement([Face], We) -> We'
%%  Dissolve all faces BUT the given faces. Also invalidate the
%%  mirror face if it existed and was dissolved.
complement(Fs0, #we{fs=Ftab0}=We0) when is_list(Fs0) ->
    Fs = ordsets:subtract(gb_trees:keys(Ftab0), ordsets:from_list(Fs0)),
    case faces(Fs, Fs0, We0) of
        #we{mirror=none}=We -> We;
        #we{mirror=Face,fs=Ftab}=We ->
            case gb_trees:is_defined(Face, Ftab) of
                false -> We;
                true -> We#we{mirror=none}
            end
    end;
complement(Fs, We) -> complement(gb_sets:to_list(Fs), We).

optimistic_dissolve(Faces0, Compl, We0) ->
    %% Optimistically assume that we have a simple region without
    %% any holes.
    case outer_edge_loop(Faces0, We0) of
	error ->
	    %% Assumption was wrong. We need to partition the selection
	    %% and dissolve each partition in turn.
	    Parts = wings_sel:face_regions(Faces0, We0),
	    complex_dissolve(Parts, We0);
	[_|_]=Loop ->
	    %% Assumption was correct.
	    simple_dissolve(Faces0, Compl, Loop, We0)
    end.

%% simple_dissolve(Faces, Loop, We0) -> We
%%  Dissolve a region of faces with no holes and no
%%  repeated vertices in the outer edge loop.

simple_dissolve(Faces0, Compl, Loop, We0) ->
    Faces = to_gb_set(Faces0),
    OldFace = gb_sets:smallest(Faces),
    Mat = wings_facemat:face(OldFace, We0),
    We1 = fix_materials(Faces, Compl, We0),
    #we{es=Etab0,fs=Ftab0,he=Htab0} = We1,
    {Ftab1,Etab1,Htab} = simple_del(Faces, Ftab0, Etab0, Htab0, We1),
    {NewFace,We2} = wings_we:new_id(We1),
    Ftab = gb_trees:insert(NewFace, hd(Loop), Ftab1),
    Last = lists:last(Loop),
    Etab = update_outer([Last|Loop], Loop, NewFace, Ftab, Etab1),
    We = We2#we{es=Etab,fs=Ftab,he=Htab},
    wings_facemat:assign(Mat, [NewFace], We).

fix_materials(Del,Keep,We) ->
    case gb_sets:size(Del) < length(Keep) of
	true ->
	    wings_facemat:delete_faces(Del,We);
	false ->
	    wings_facemat:keep_faces(Keep,We)
    end.

to_gb_set(List) when is_list(List) ->
    gb_sets:from_list(List);
to_gb_set(S) -> S.

%% Delete faces and inner edges for a simple region.
simple_del(Faces, Ftab0, Etab0, Htab0, We) ->
    case {gb_trees:size(Ftab0),gb_sets:size(Faces)} of
	{AllSz,FaceSz} when AllSz < 2*FaceSz ->
	    %% At least half of the faces are selected.
	    %% It is faster to find the edges for the
	    %% unselected faces.
	    UnselFaces = ordsets:subtract(gb_trees:keys(Ftab0),
					  gb_sets:to_list(Faces)),

	    UnselSet = sofs:from_external(UnselFaces, [face]),
	    Ftab1 = sofs:from_external(gb_trees:to_list(Ftab0),
				       [{face,edge}]),
	    Ftab2 = sofs:restriction(Ftab1, UnselSet),
	    Ftab = gb_trees:from_orddict(sofs:to_external(Ftab2)),

	    Keep0 = wings_face:to_edges(UnselFaces, We),
	    Keep = sofs:set(Keep0, [edge]),
	    Etab1 = sofs:from_external(gb_trees:to_list(Etab0),
				       [{edge,info}]),
	    Etab2 = sofs:restriction(Etab1, Keep),
	    Etab = gb_trees:from_orddict(sofs:to_external(Etab2)),

	    Htab = simple_del_hard(Htab0, sofs:to_external(Keep), undefined),
	    {Ftab,Etab,Htab};
	{_,_} ->
	    Ftab = lists:foldl(fun(Face, Ft) ->
				       gb_trees:delete(Face, Ft)
			       end, Ftab0, gb_sets:to_list(Faces)),
	    Inner = wings_face:inner_edges(Faces, We),
	    Etab = lists:foldl(fun(Edge, Et) ->
				       gb_trees:delete(Edge, Et)
			       end, Etab0, Inner),
	    Htab = simple_del_hard(Htab0, undefined, Inner),
	    {Ftab,Etab,Htab}
    end.

simple_del_hard(Htab, Keep, Remove) ->
    case gb_sets:is_empty(Htab) of
	true -> Htab;
	false -> simple_del_hard_1(Htab, Keep, Remove)
    end.

simple_del_hard_1(Htab, Keep, undefined) ->
    gb_sets:intersection(Htab, gb_sets:from_ordset(Keep));
simple_del_hard_1(Htab, undefined, Remove) ->
    gb_sets:difference(Htab, gb_sets:from_ordset(Remove)).

%% complex([Partition], We0) -> We0
%%  The general dissolve.

complex_dissolve([Faces|T], We0) ->
    Face = gb_sets:smallest(Faces),
    Mat = wings_facemat:face(Face, We0),
    We1 = wings_facemat:delete_faces(Faces, We0),
    Parts = outer_edge_partition(Faces, We1),
    We = do_dissolve(Faces, Parts, Mat, We0, We1),
    complex_dissolve(T, We);
complex_dissolve([], We) -> We.

do_dissolve(Faces, Ess, Mat, WeOrig, We0) ->
    We1 = do_dissolve_faces(Faces, We0),
    Inner = wings_face:inner_edges(Faces, WeOrig),
    We2 = delete_inner(Inner, We1),
    #we{he=Htab0} = We = do_dissolve_1(Ess, Mat, We2),
    Htab = gb_sets:difference(Htab0, gb_sets:from_list(Inner)),
    We#we{he=Htab}.

do_dissolve_1([EdgeList|Ess], Mat, #we{es=Etab0,fs=Ftab0}=We0) ->
    {Face,We1} = wings_we:new_id(We0),
    Ftab = gb_trees:insert(Face, hd(EdgeList), Ftab0),
    Last = lists:last(EdgeList),
    Etab = update_outer([Last|EdgeList], EdgeList, Face, Ftab, Etab0),
    We2 = We1#we{es=Etab,fs=Ftab},
    We = wings_facemat:assign(Mat, [Face], We2),
    do_dissolve_1(Ess, Mat, We);
do_dissolve_1([], _Mat, We) -> We.

do_dissolve_faces(Faces, #we{fs=Ftab0}=We) ->
    Ftab = lists:foldl(fun(Face, Ft) ->
			       gb_trees:delete(Face, Ft)
		       end, Ftab0, gb_sets:to_list(Faces)),
    We#we{fs=Ftab}.

delete_inner(Inner, #we{es=Etab0}=We) ->
    Etab = lists:foldl(fun(Edge, Et) ->
			       gb_trees:delete(Edge, Et)
		       end, Etab0, Inner),
    We#we{es=Etab}.

update_outer([Pred|[Edge|Succ]=T], More, Face, Ftab, Etab0) ->
    #edge{rf=Rf} = R0 = gb_trees:get(Edge, Etab0),
    Rec = case gb_trees:is_defined(Rf, Ftab) of
	      true ->
		  ?ASSERT(false == gb_trees:is_defined(R0#edge.lf, Ftab)),
		  LS = succ(Succ, More),
		  R0#edge{lf=Face,ltpr=Pred,ltsu=LS};
	      false ->
		  ?ASSERT(true == gb_trees:is_defined(R0#edge.lf, Ftab)),
		  RS = succ(Succ, More),
		  R0#edge{rf=Face,rtpr=Pred,rtsu=RS}
	  end,
    Etab = gb_trees:update(Edge, Rec, Etab0),
    update_outer(T, More, Face, Ftab, Etab);
update_outer([_], _More, _Face, _Ftab, Etab) -> Etab.

succ([Succ|_], _More) -> Succ;
succ([], [Succ|_]) -> Succ.

%% outer_edge_loop(FaceSet,WingedEdge) -> [Edge] | error.
%%  Partition the outer edges of the FaceSet into a single closed loop.
%%  Return 'error' if the faces in FaceSet does not form a
%%  simple region without holes.
%%
%%  Equvivalent to
%%      case outer_edge_partition(FaceSet,WingedEdge) of
%%         [Loop] -> Loop;
%%         [_|_] -> error
%%      end.
%%  but faster.

outer_edge_loop(Faces, We) ->
    case lists:sort(collect_outer_edges(Faces, We)) of
	[] -> error;
	[{Key,Val}|Es0] ->
	    case any_duplicates(Es0, Key) of
		false ->
		    Es = gb_trees:from_orddict(Es0),
		    N = gb_trees:size(Es),
		    outer_edge_loop_1(Val, Es, Key, N, []);
		true -> error
	    end
    end.

outer_edge_loop_1({Edge,V}, _, V, 0, Acc) ->
    %% This edge completes the loop, and we have used all possible edges.
    [Edge|Acc];
outer_edge_loop_1({_,V}, _, V, _N, _) ->
    %% Loop is complete, but we haven't used all edges.
    error;
outer_edge_loop_1({_,_}, _, _, 0, _) ->
    %% We have used all possible edges, but somehow the loop
    %% is not complete. I can't see how this is possible.
    erlang:error(internal_error);
outer_edge_loop_1({Edge,Vb}, Es, EndV, N, Acc0) ->
    Acc = [Edge|Acc0],
    outer_edge_loop_1(gb_trees:get(Vb, Es), Es, EndV, N-1, Acc).

any_duplicates([{V,_}|_], V) -> true;
any_duplicates([_], _) -> false;
any_duplicates([{V,_}|Es], _) -> any_duplicates(Es, V).

%% outer_edge_partition(FaceSet, WingedEdge) -> [[Edge]].
%%  Partition the outer edges of the FaceSet. Each partion
%%  of edges form a closed loop with no repeated vertices.
%%    Outer edges are edges that have one face in FaceSet
%%  and one outside.
%%    It is assumed that FaceSet consists of one region returned by
%%  wings_sel:face_regions/2.

outer_edge_partition(Faces, We) ->
    F0 = collect_outer_edges(Faces, We),
    F = gb_trees:from_orddict(wings_util:rel2fam(F0)),
    partition_edges(F, []).

collect_outer_edges(Faces, We) when is_list(Faces) ->
    collect_outer_edges_1(Faces, gb_sets:from_list(Faces), We);
collect_outer_edges(Faces, We) ->
    collect_outer_edges_1(gb_sets:to_list(Faces), Faces, We).

collect_outer_edges_1(Fs0, Faces0, #we{fs=Ftab}=We) ->
    case {gb_trees:size(Ftab),gb_sets:size(Faces0)} of
	{AllSz,FaceSz} when AllSz < 2*FaceSz ->
	    Fs = ordsets:subtract(gb_trees:keys(Ftab), Fs0),
	    Faces = gb_sets:from_ordset(Fs),
	    Coll = collect_outer_edges_a(Faces),
	    wings_face:fold_faces(Coll, [], Fs, We);
	{_,_} ->
	    Coll = collect_outer_edges_b(Faces0),
	    wings_face:fold_faces(Coll, [], Fs0, We)
    end.

collect_outer_edges_a(Faces) ->
    fun(Face, _, Edge, #edge{ve=V,vs=OtherV,lf=Face,rf=Other}, Acc) ->
	    case gb_sets:is_member(Other, Faces) of
		false -> [{V,{Edge,OtherV}}|Acc];
		true -> Acc
	    end;
       (Face, _, Edge, #edge{ve=OtherV,vs=V,rf=Face,lf=Other}, Acc) ->
	    case gb_sets:is_member(Other, Faces) of
		false -> [{V,{Edge,OtherV}}|Acc];
		true -> Acc
	    end
    end.

collect_outer_edges_b(Faces) ->
    fun(Face, _, Edge, #edge{vs=V,ve=OtherV,lf=Face,rf=Other}, Acc) ->
	    case gb_sets:is_member(Other, Faces) of
		false -> [{V,{Edge,OtherV}}|Acc];
		true -> Acc
	    end;
       (Face, _, Edge, #edge{vs=OtherV,ve=V,rf=Face,lf=Other}, Acc) ->
	    case gb_sets:is_member(Other, Faces) of
		false -> [{V,{Edge,OtherV}}|Acc];
		true -> Acc
	    end
    end.

partition_edges(Es0, Acc) ->
    case gb_trees:is_empty(Es0) of
	true -> Acc;
	false ->
	    {Key,Val,Es1} = gb_trees:take_smallest(Es0),
	    {Cycle,Es} = part_collect_cycle(Key, Val, Es1, []),
	    partition_edges(Es, [Cycle|Acc])
    end.

%% part_collect_cycle(Vertex, VertexInfo, EdgeInfo, Acc0) ->
%%    none | {[Edge],EdgeInfo}
%%  Collect the cycle starting with Vertex.
%%
%%  Note: This function can only return 'none' when called
%%  recursively.

part_collect_cycle(_, repeated, _, _) ->
    %% Repeated vertex - we are not allowed to go this way.
    %% Can only happen if we were called recursively because
    %% a fork was encountered.
    none;
part_collect_cycle(_Va, [{Edge,Vb}], Es0, Acc0) ->
    %% Basic case. Only one way to go.
    Acc = [Edge|Acc0],
    case gb_trees:lookup(Vb, Es0) of
	none ->
	    {Acc,Es0};
	{value,Val} ->
	    Es = gb_trees:delete(Vb, Es0),
	    part_collect_cycle(Vb, Val, Es, Acc)
    end;
part_collect_cycle(Va, [Val|More], Es0, []) ->
    %% No cycle started yet and we have multiple choice of
    %% edges out from this vertex. It doesn't matter which
    %% edge we follow, so we'll follow the first one.
    {Cycle,Es} = part_collect_cycle(Va, [Val], Es0, []),
    {Cycle,gb_trees:insert(Va, More, Es)};
part_collect_cycle(Va, Edges, Es0, Acc) ->
    %% We have a partially collected cycle and we have a
    %% fork (multiple choice of edges). Here we must choose
    %% an edge that closes the cycle without passing Va
    %% again (because repeated vertices are not allowed).
    Es = gb_trees:insert(Va, repeated, Es0),
    part_fork(Va, Edges, Es, Acc, []).

part_fork(Va, [Val|More], Es0, Acc, Tried) ->
    %% Try to complete the cycle by following this edge.
    case part_collect_cycle(Va, [Val], Es0, Acc) of
	none ->
	    %% Failure - try the next edge.
	    part_fork(Va, More, Es0, Acc, [Val|Tried]);
	{Cycle,Es} ->
	    %% Found a cycle. Update the vertex information
	    %% with all edges remaining.
	    {Cycle,gb_trees:update(Va, lists:reverse(Tried, More), Es)}
    end;
part_fork(_, [], _, _, _) ->
    %% None of edges were possible. Can only happen if this function
    %% was called recursively (i.e. if we hit another fork while
    %% processing a fork).
    none.
