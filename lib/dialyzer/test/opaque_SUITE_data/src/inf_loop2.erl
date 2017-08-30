%% -*- erlang-indent-level: 2 -*-
%%----------------------------------------------------------------------------
%% Copy of inf_loop1.erl, where the calls mentioned below have been
%% restored.

%% Non-sensical (i.e., stripped-down) program that sends the analysis
%% into an infinite loop. The #we.es field was originally a gb_trees:tree()
%% but the programmer declared it as an array in order to change it to
%% that data type instead. In the file, there are two calls to function
%% gb_trees:get/2 which seem to be the ones responsible for sending the
%% analysis into an infinite loop. Currently, these calls are marked and
%% have been changed to gbee_trees:get/2 in order to be able to see that
%% the analysis works if these two calls are taken out of the picture.
%%----------------------------------------------------------------------------
-module(inf_loop2).

-export([command/1]).

-record(we, {id,
	     es = array:new() :: array:array(),
	     vp,
	     mirror = none}).
-record(edge, {vs,ve,a = none,b = none,lf,rf,ltpr,ltsu,rtpr,rtsu}).

command(St) ->
  State = drag_mode(offset_region),
  SetupSt = wings_sel_conv:more(St),
  Tvs = wings_sel:fold(fun(Faces, #we{id = Id} = We, Acc) ->
			   FaceRegions = wings_sel:face_regions(Faces, We),
			   {AllVs0,VsData} =
			     collect_offset_regions_data(FaceRegions, We, [], []),
			   AllVs = ordsets:from_list(AllVs0),
			   [{Id,{AllVs,offset_regions_fun(VsData, State)}}|Acc]
                       end,
                       [],
                       SetupSt),
    wings_drag:setup(Tvs, 42, [], St).

drag_mode(Type) ->
    {Mode,Norm} = wings_pref:get_value(Type, {average,loop}),
    {Type,Mode,Norm}.

collect_offset_regions_data([Faces|Regions], We, AllVs, VsData) ->
  {FaceNormTab,OuterEdges,RegVs} =
    some_fake_module:faces_data_0(Faces, We, [], [], []),
  {LoopNorm,LoopVsData,LoopVs} =
    offset_regions_loop_data(OuterEdges, Faces, We, FaceNormTab),
  Vs = RegVs -- LoopVs,
  RegVsData = vertex_normals(Vs, FaceNormTab, We, LoopVsData),
  collect_offset_regions_data(Regions, We, RegVs ++ AllVs,
			      [{LoopNorm,RegVsData}|VsData]);
collect_offset_regions_data([], _, AllVs, VsData) ->
  {AllVs,VsData}.

offset_regions_loop_data(Edges, Faces, We, FNtab) ->
  EdgeSet = gb_sets:from_list(Edges),
  offset_loop_data_0(EdgeSet, Faces, We, FNtab, [], [], []).

offset_loop_data_0(EdgeSet0, Faces, We, FNtab, LNorms, VData0, Vs0) ->
  case gb_sets:is_empty(EdgeSet0) of
    false ->
      {Edge,EdgeSet1} = gb_sets:take_smallest(EdgeSet0),
      {EdgeSet,VData,Links,LoopNorm,Vs} =
	offset_loop_data_1(Edge, EdgeSet1, Faces, We, FNtab, VData0, Vs0),
      offset_loop_data_0(EdgeSet, Faces, We, FNtab,
			 [{Links,LoopNorm}|LNorms], VData, Vs);
    true ->
      AvgLoopNorm = average_loop_norm(LNorms),
      {AvgLoopNorm,VData0,Vs0}
  end.

offset_loop_data_1(Edge, EdgeSet, _Faces,
                   #we{es = Etab, vp = Vtab} = We, FNtab, VData, Vs) ->
  #edge{vs = Va, ve = Vb, lf = Lf, ltsu = NextLeft} = gb_trees:get(Edge, Etab),
  VposA = gb_trees:get(Va, Vtab),
  VposB = gb_trees:get(Vb, Vtab),
  VDir = e3d_vec:sub(VposB, VposA),
  FNorm = wings_face:normal(Lf, We),
  EdgeData = gb_trees:get(NextLeft, Etab),
  offset_loop_data_2(NextLeft, EdgeData, Va, VposA, Lf, Edge, We, FNtab,
		     EdgeSet, VDir, [], [FNorm], VData, [], Vs, 0).

offset_loop_data_2(CurE, #edge{vs = Va, ve = Vb, lf = PrevFace,
			       rtsu = NextEdge, ltsu = IfCurIsMember},
                   Vb, VposB, PrevFace, LastE,
                   #we{mirror = M} = We,
                   FNtab, EdgeSet0, VDir, EDir0, VNorms0, VData0, VPs0, Vs0,
                   Links) ->
  Mirror = M == PrevFace,
  offset_loop_is_member(Mirror, Vb, Va, VposB, CurE, IfCurIsMember, VNorms0,
			NextEdge, EdgeSet0, VDir, EDir0, FNtab, PrevFace,
			LastE, We, VData0, VPs0, Vs0, Links).

offset_loop_is_member(Mirror, V1, V2, Vpos1, CurE, NextE, VNorms0, NEdge,
                      EdgeSet0, VDir, EDir0, FNtab, PFace, LastE, We,
                      VData0, VPs0, Vs0, Links) ->
  #we{es = Etab, vp = Vtab} = We,
  Vpos2 = gb_trees:get(V2, Vtab),
  Dir = e3d_vec:sub(Vpos2, Vpos1),
  NextVDir = e3d_vec:neg(Dir),
  EdgeSet = gb_sets:delete(CurE, EdgeSet0),
  EdgeData = gb_trees:get(NextE, Etab), %% HERE
  [FNorm|_] = VNorms0,
  VData = offset_loop_data_3(Mirror, V1, Vpos1, VNorms0, NEdge, VDir,
			     Dir, EDir0, FNtab, We, VData0),
  VPs = [Vpos1|VPs0],
  Vs = [V1|Vs0],
  offset_loop_data_2(NextE, EdgeData, V2, Vpos2, PFace, LastE, We, FNtab,
		     EdgeSet, NextVDir, [], [FNorm], VData, VPs, Vs, Links + 1).

offset_loop_data_3(false, V, Vpos, VNorms0, NextEdge,
                   VDir, Dir, EDir0, FNtab, We, VData0) ->
  #we{es = Etab} = We,
  VNorm = e3d_vec:norm(e3d_vec:add(VNorms0)),
  NV = wings_vertex:other(V, gb_trees:get(NextEdge, Etab)), %% HERE
  ANorm = vertex_normal(NV, FNtab, We),
  EDir = some_fake_module:average_edge_dir(VNorm, VDir, Dir, EDir0),
  AvgDir = some_fake_module:evaluate_vdata(VDir, Dir, VNorm),
  ScaledDir = some_fake_module:along_edge_scale_factor(VDir, Dir, EDir, ANorm),
  [{V,{Vpos,AvgDir,EDir,ScaledDir}}|VData0].

average_loop_norm([{_,LNorms}]) ->
  e3d_vec:norm(LNorms);
average_loop_norm([{LinksA,LNormA},{LinksB,LNormB}]) ->
  case LinksA < LinksB of
    true ->
      e3d_vec:norm(e3d_vec:add(e3d_vec:neg(LNormA), LNormB));
    false ->
      e3d_vec:norm(e3d_vec:add(e3d_vec:neg(LNormB), LNormA))
  end;
average_loop_norm(LNorms) ->
  LoopNorms = [Norm || {_,Norm} <- LNorms],
  e3d_vec:norm(e3d_vec:neg(e3d_vec:add(LoopNorms))).

vertex_normals([V|Vs], FaceNormTab, #we{vp = Vtab, mirror = M} = We, Acc) ->
  FaceNorms =
    wings_vertex:fold(fun(_, Face, _, A) when Face == M ->
			  [e3d_vec:neg(wings_face:normal(M, We))|A];
			 (_, Face, _, A) ->
			  [gb_trees:get(Face, FaceNormTab)|A]
		      end, [], V, We),
  VNorm = e3d_vec:norm(e3d_vec:add(FaceNorms)),
  Vpos = gb_trees:get(V, Vtab),
  vertex_normals(Vs, FaceNormTab, We, [{V,{Vpos,VNorm}}|Acc]);
vertex_normals([], _, _, Acc) ->
  Acc.

vertex_normal(V, FaceNormTab, #we{mirror = M} = We) ->
  wings_vertex:fold(fun(_, Face, _, A) when Face == M ->
			[e3d_vec:neg(wings_face:normal(Face, We))|A];
		       (_, Face, _, A) ->
			N = gb_trees:get(Face, FaceNormTab),
			case e3d_vec:is_zero(N) of
			  true -> A;
			  false -> [N|A]
			end
		    end, [], V, We).

offset_regions_fun(OffsetData, {_,Solution,_} = State) ->
  fun(new_mode_data, {NewState,_}) ->
      offset_regions_fun(OffsetData, NewState);
     ([Dist,_,_,Bump|_], A) ->
      lists:foldl(fun({LoopNormal,VsData}, VsAcc0) ->
		      lists:foldl(fun({V,{Vpos0,VNorm}}, VsAcc) ->
				      [{V,Vpos0}|VsAcc];
				     ({V,{Vpos0,Dir,EDir,ScaledEDir}}, VsAcc) ->
				      Vec = case Solution of
					      average -> Dir;
					      along_edges -> EDir;
					      scaled -> ScaledEDir
					    end,
				      [{V,Vpos0}|VsAcc]
				  end, VsAcc0, VsData)
		  end, A, OffsetData)
  end.
