%%
%%  wings_we.erl --
%%
%%     This module contains functions to build and manipulate
%%     we records (winged-edged records, the central data structure
%%     in Wings 3D).

-module(wings_we).

-export([rebuild/1, is_consistent/1, is_face_consistent/2, new_id/1,
	 new_items_as_ordset/3, validate_mirror/1, visible/1, visible_edges/1]).

-include("wings.hrl").

%%%
%%% API.
%%%

validate_mirror(#we{mirror=none}=We) -> We;
validate_mirror(#we{fs=Ftab,mirror=Face}=We) ->
    case gb_trees:is_defined(Face, Ftab) of
        false -> We#we{mirror=none};
        true -> We
    end.

%% rebuild(We) -> We'
%%  Rebuild any missing 'vc' and 'fs' tables. If there are
%%  fewer elements in the 'vc' table than in the 'vp' table,
%%  remove redundant entries in the 'vp' table. Updated id
%%  bounds.
rebuild(#we{vc=undefined,fs=undefined,es=Etab0}=We0) ->
    Etab = gb_trees:to_list(Etab0),
    Ftab = rebuild_ftab(Etab),
    VctList = rebuild_vct(Etab),
    We = We0#we{vc=gb_trees:from_orddict(VctList),fs=Ftab},
    rebuild_1(VctList, We);
rebuild(#we{vc=undefined,es=Etab}=We) ->
    VctList = rebuild_vct(gb_trees:to_list(Etab), []),
    rebuild_1(VctList, We#we{vc=gb_trees:from_orddict(VctList)});
rebuild(#we{fs=undefined,es=Etab}=We) ->
    Ftab = rebuild_ftab(gb_trees:to_list(Etab)),
    rebuild(We#we{fs=Ftab});
rebuild(We) -> update_id_bounds(We).

%%% Utilities for allocating IDs.

new_id(#we{next_id=Id}=We) ->
    {Id,We#we{next_id=Id+1}}.

%%% Returns sets of newly created items.

new_items_as_ordset(vertex, #we{next_id=Wid}, #we{next_id=NewWid,vp=Tab}) ->
    new_items_as_ordset_1(Tab, Wid, NewWid);
new_items_as_ordset(edge, #we{next_id=Wid}, #we{next_id=NewWid,es=Tab}) ->
    new_items_as_ordset_1(Tab, Wid, NewWid);
new_items_as_ordset(face, #we{next_id=Wid}, #we{next_id=NewWid,fs=Tab}) ->
    new_items_as_ordset_1(Tab, Wid, NewWid).

any_hidden(#we{fs=Ftab}) ->
    not gb_trees:is_empty(Ftab) andalso
	wings_util:gb_trees_smallest_key(Ftab) < 0.

%%%
%%% Local functions.
%%%

rebuild_1(VctList, #we{vc=Vct,vp=Vtab0}=We) ->
    case {gb_trees:size(Vct),gb_trees:size(Vtab0)} of
	{Same,Same} -> rebuild(We);
	{Sz1,Sz2} when Sz1 < Sz2 ->
	    Vtab = vertex_gc_1(VctList, gb_trees:to_list(Vtab0), []),
	    rebuild(We#we{vp=Vtab})
    end.

rebuild_vct(Es) ->
    rebuild_vct(Es, []).

rebuild_vct([{Edge,#edge{vs=Va,ve=Vb}}|Es], Acc0) ->
    Acc = rebuild_maybe_add(Va, Vb, Edge, Acc0),
    rebuild_vct(Es, Acc);
rebuild_vct([], VtoE) ->
    build_incident_tab(VtoE).

rebuild_ftab(Es) ->
    rebuild_ftab_1(Es, []).

rebuild_ftab_1([{Edge,#edge{lf=Lf,rf=Rf}}|Es], Acc0) ->
    Acc = rebuild_maybe_add(Lf, Rf, Edge, Acc0),
    rebuild_ftab_1(Es, Acc);
rebuild_ftab_1([], FtoE) ->
    gb_trees:from_orddict(build_incident_tab(FtoE)).

rebuild_maybe_add(Ka, Kb, E, [_,{Ka,_}|_]=Acc) ->
    [{Kb,E}|Acc];
rebuild_maybe_add(Ka, Kb, E, [_,{Kb,_}|_]=Acc) ->
    [{Ka,E}|Acc];
rebuild_maybe_add(Ka, Kb, E, [{Ka,_}|_]=Acc) ->
    [{Kb,E}|Acc];
rebuild_maybe_add(Ka, Kb, E, [{Kb,_}|_]=Acc) ->
    [{Ka,E}|Acc];
rebuild_maybe_add(Ka, Kb, E, Acc) ->
    [{Ka,E},{Kb,E}|Acc].

vertex_gc_1([{V,_}|Vct], [{V,_}=Vtx|Vpos], Acc) ->
    vertex_gc_1(Vct, Vpos, [Vtx|Acc]);
vertex_gc_1([_|_]=Vct, [_|Vpos], Acc) ->
    vertex_gc_1(Vct, Vpos, Acc);
vertex_gc_1([], _, Acc) ->
    gb_trees:from_orddict(lists:reverse(Acc)).

%%%
%%% Handling of hidden faces.
%%%

visible(#we{mirror=none,fs=Ftab}) ->
    visible_2(gb_trees:keys(Ftab));
visible(#we{mirror=Face,fs=Ftab}) ->
    visible_2(gb_trees:keys(gb_trees:delete(Face, Ftab))).

visible_2([F|Fs]) when F < 0 -> visible_2(Fs);
visible_2(Fs) -> Fs.

visible_edges(#we{es=Etab,mirror=Face}=We) ->
    case any_hidden(We) of
	false -> gb_trees:keys(Etab);
	true -> visible_es_1(gb_trees:to_list(Etab), Face, [])
    end.

visible_es_1([{E,#edge{lf=Lf,rf=Rf}}|Es], Face, Acc) ->
    if
	Lf < 0 ->
	    %% Left face hidden.
	    if
		Rf < 0; Rf =:= Face ->
		    %% Both faces invisible (in some way).
		    visible_es_1(Es, Face, Acc);
		true ->
		    %% Right face is visible.
		    visible_es_1(Es, Face, [E|Acc])
	    end;
	Lf =:= Face, Rf < 0 ->
	    %% Left face mirror, right face hidden.
	    visible_es_1(Es, Face, Acc);
	true ->
	    %% At least one face visible.
	    visible_es_1(Es, Face, [E|Acc])
    end;
visible_es_1([], _, Acc) -> ordsets:from_list(Acc).

update_id_bounds(#we{vp=Vtab,es=Etab,fs=Ftab}=We) ->
    case gb_trees:is_empty(Etab) of
	true -> We#we{next_id=0};
	false ->
	    LastId = lists:max([wings_util:gb_trees_largest_key(Vtab),
				wings_util:gb_trees_largest_key(Etab),
				wings_util:gb_trees_largest_key(Ftab)]),
	    We#we{next_id=LastId+1}
    end.

%% build_incident_tab([{Elem,Edge}]) -> [{Elem,Edge}]
%%      Elem = Face or Vertex
%%  Build the table of incident edges for either faces or vertices.
%%  Returns an ordered list where each Elem is unique.

build_incident_tab(ElemToEdgeRel) ->
    T = ets:new(?MODULE, [ordered_set]),
    ets:insert(T, ElemToEdgeRel),
    R = ets:tab2list(T),
    ets:delete(T),
    R.

%%%
%%% Calculate normals.
%%%

new_items_as_ordset_1(Tab, Wid, NewWid) when NewWid-Wid < 32 ->
    new_items_as_ordset_2(Wid, NewWid, Tab, []);
new_items_as_ordset_1(Tab, Wid, _NewWid) ->
    [Item || Item <- gb_trees:keys(Tab), Item >= Wid].

new_items_as_ordset_2(Wid, NewWid, Tab, Acc) when Wid < NewWid ->
    case gb_trees:is_defined(Wid, Tab) of
	true -> new_items_as_ordset_2(Wid+1, NewWid, Tab, [Wid|Acc]);
	false -> new_items_as_ordset_2(Wid+1, NewWid, Tab, Acc)
    end;
new_items_as_ordset_2(_Wid, _NewWid, _Tab, Acc) -> lists:reverse(Acc).

%%%
%%% Test the consistency of a #we{}.
%%%

is_consistent(#we{}=We) ->
    try
	validate_vertex_tab(We),
	validate_faces(We)
    catch error:_ -> false
    end.

is_face_consistent(Face, #we{fs=Ftab,es=Etab}) ->
    Edge = gb_trees:get(Face, Ftab),
    try validate_face(Face, Edge, Etab)
    catch error:_ -> false
    end.

validate_faces(#we{fs=Ftab,es=Etab}) ->
    validate_faces_1(gb_trees:to_list(Ftab), Etab).

validate_faces_1([{Face,Edge}|Fs], Etab) ->
    validate_face(Face, Edge, Etab),
    validate_faces_1(Fs, Etab);
validate_faces_1([], _) -> true.

validate_face(Face, Edge, Etab) ->
    Ccw = walk_face_ccw(Edge, Etab, Face, Edge, []),
    Edge = walk_face_cw(Edge, Etab, Face, Ccw),
    [V|Vs] = lists:sort(Ccw),
    validate_face_vertices(Vs, V).

validate_face_vertices([V|_], V) ->
    erlang:error(repeated_vertex);
validate_face_vertices([_], _) ->
    true;
validate_face_vertices([V|Vs], _) ->
    validate_face_vertices(Vs, V).

walk_face_ccw(LastEdge, _, _, LastEdge, [_|_]=Acc) -> Acc;
walk_face_ccw(Edge, Etab, Face, LastEdge, Acc) ->
    case gb_trees:get(Edge, Etab) of
	#edge{ve=V,lf=Face,ltpr=Next} ->
	    walk_face_ccw(Next, Etab, Face, LastEdge, [V|Acc]);
	#edge{vs=V,rf=Face,rtpr=Next} ->
	    walk_face_ccw(Next, Etab, Face, LastEdge, [V|Acc])
    end.

walk_face_cw(Edge, _, _, []) -> Edge;
walk_face_cw(Edge, Etab, Face, [V|Vs]) ->
    case gb_trees:get(Edge, Etab) of
	#edge{vs=V,lf=Face,ltsu=Next} ->
	    walk_face_cw(Next, Etab, Face, Vs);
	#edge{ve=V,rf=Face,rtsu=Next} ->
	    walk_face_cw(Next, Etab, Face, Vs)
    end.

validate_vertex_tab(#we{es=Etab,vc=Vct}) ->
    lists:foreach(fun({V,Edge}) ->
			  case gb_trees:get(Edge, Etab) of
			      #edge{vs=V} -> ok;
			      #edge{ve=V} -> ok
			  end
		  end, gb_trees:to_list(Vct)).
