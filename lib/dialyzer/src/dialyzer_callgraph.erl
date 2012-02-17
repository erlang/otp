%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%%-------------------------------------------------------------------
%%% File    : dialyzer_callgraph.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description : 
%%%
%%% Created : 30 Mar 2005 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------
-module(dialyzer_callgraph).

-export([add_edges/2,
	 all_nodes/1,
	 delete/1,
	 finalize/1,
	 is_escaping/2,
	 is_self_rec/2,
	 non_local_calls/1,
	 lookup_rec_var/2,
	 lookup_call_site/2,
	 lookup_label/2,
	 lookup_name/2,
	 modules/1,
	 module_deps/1,
	 %% module_postorder/1,
	 module_postorder_from_funs/2,
	 new/0,
	 mini_callgraph/1,
	 get_depends_on/2,
	 get_required_by/2,
	 in_neighbours/2,
	 renew_race_info/4,
	 renew_race_code/2,
	 renew_race_public_tables/2,
	 reset_from_funs/2,
	 scan_core_tree/2,
	 strip_module_deps/2,
	 remove_external/1,
	 to_dot/2,
	 to_ps/3]).

-export([cleanup/1, get_digraph/1, get_named_tables/1, get_public_tables/1,
         get_race_code/1, get_race_detection/1, race_code_new/1,
         put_digraph/2, put_race_code/2, put_race_detection/2,
         put_named_tables/2, put_public_tables/2, put_behaviour_api_calls/2,
	 get_behaviour_api_calls/1, dispose_race_server/1, duplicate/1]).

-export_type([callgraph/0, mfa_or_funlbl/0]).

-include("dialyzer.hrl").

%%----------------------------------------------------------------------

-type scc()	      :: [mfa_or_funlbl()].
-type mfa_calls()     :: [{mfa_or_funlbl(), mfa_or_funlbl()}].

%%-----------------------------------------------------------------------------
%% A callgraph is a directed graph where the nodes are functions and a
%% call between two functions is an edge from the caller to the callee.
%% 
%% calls	-  A mapping from call site (and apply site) labels
%%		   to the possible functions that can be called.
%% digraph	-  A digraph representing the callgraph. 
%%		   Nodes are represented as MFAs or labels.
%% esc		-  A set of all escaping functions as reported by dialyzer_dep.
%% name_map	-  A mapping from label to MFA.
%% rev_name_map	-  A reverse mapping of the name_map.
%% rec_var_map	-  A dict mapping from letrec bound labels to function names.
%%		   Only for top level functions (from module defs).
%% self_rec	-  A set containing all self recursive functions.
%%		   Note that this contains MFAs for named functions and labels
%%		   whenever applicable.
%%-----------------------------------------------------------------------------

-record(callgraph, {digraph        = digraph:new() :: digraph(),
		    active_digraph                 :: digraph(),
                    esc	           = sets:new()    :: set()  | ets:tid(),
                    name_map	   = dict:new()    :: dict() | ets:tid(),
                    rev_name_map   = dict:new()    :: dict() | ets:tid(),
                    rec_var_map    = dict:new()    :: dict() | ets:tid(),
                    self_rec	   = sets:new()    :: set()  | ets:tid(),
                    calls          = dict:new()    :: dict() | ets:tid(),
                    race_detection = false         :: boolean(),
		    race_data_server = new_race_data_server() :: pid()}).

-record(race_data_state, {race_code     = dict:new() :: dict(),
			  public_tables = []         :: [label()],
			  named_tables  = []         :: [string()],
			  beh_api_calls = []         :: [{mfa(), mfa()}]}).

%% Exported Types

-type callgraph() :: #callgraph{}.

%%----------------------------------------------------------------------

-spec new() -> callgraph().

new() ->
  #callgraph{}.

-spec mini_callgraph(callgraph()) -> callgraph().

mini_callgraph(#callgraph{digraph        = Digraph,
			  active_digraph = ActiveDigraph,
			  esc	         = Esc,
			  name_map       = NameMap,
			  rev_name_map   = RevNameMap,
			  rec_var_map    = RecVarMap,
			  self_rec       = SelfRecs,
			  calls          = Calls}) ->
  #callgraph{digraph        = Digraph,
	     active_digraph = ActiveDigraph,
	     esc	         = Esc,
	     name_map       = NameMap,
	     rev_name_map   = RevNameMap,
	     rec_var_map    = RecVarMap,
	     self_rec       = SelfRecs,
	     calls          = Calls}.

-spec delete(callgraph()) -> 'true'.

delete(#callgraph{digraph = Digraph}) ->
  digraph_delete(Digraph).

-spec all_nodes(callgraph()) -> [mfa()].

all_nodes(#callgraph{digraph = DG}) ->
  digraph_vertices(DG).

-spec lookup_rec_var(label(), callgraph()) -> 'error' | {'ok', mfa()}.

lookup_rec_var(Label, #callgraph{rec_var_map = RecVarMap}) 
  when is_integer(Label) ->
  ets_lookup_dict(Label, RecVarMap).

-spec lookup_call_site(label(), callgraph()) -> 'error' | {'ok', [_]}. % XXX: refine

lookup_call_site(Label, #callgraph{calls = Calls})
  when is_integer(Label) ->
  ets_lookup_dict(Label, Calls).

-spec lookup_name(label(), callgraph()) -> 'error' | {'ok', mfa()}.

lookup_name(Label, #callgraph{name_map = NameMap})
  when is_integer(Label) ->
  ets_lookup_dict(Label, NameMap).

-spec lookup_label(mfa_or_funlbl(), callgraph()) -> 'error' | {'ok', integer()}.

lookup_label({_,_,_} = MFA, #callgraph{rev_name_map = RevNameMap}) ->
  ets_lookup_dict(MFA, RevNameMap);
lookup_label(Label, #callgraph{}) when is_integer(Label) ->
  {ok, Label}.

-spec in_neighbours(mfa_or_funlbl(), callgraph()) -> 'none' | [mfa_or_funlbl(),...].

in_neighbours(Label, #callgraph{digraph = Digraph} = CG)
  when is_integer(Label) ->
  Name = case lookup_name(Label, CG) of
	   {ok, Val} -> Val;
	   error -> Label
	 end,
  digraph_in_neighbours(Name, Digraph);
in_neighbours({_, _, _} = MFA, #callgraph{digraph = Digraph}) ->
  digraph_in_neighbours(MFA, Digraph).

-spec is_self_rec(mfa_or_funlbl(), callgraph()) -> boolean().

is_self_rec(MfaOrLabel, #callgraph{self_rec = SelfRecs}) ->
  ets_lookup_set(MfaOrLabel, SelfRecs).

-spec is_escaping(label(), callgraph()) -> boolean().

is_escaping(Label, #callgraph{esc = Esc}) when is_integer(Label) ->
  ets_lookup_set(Label, Esc).

-type callgraph_edge() :: {mfa_or_funlbl(),mfa_or_funlbl()}.
-spec add_edges([callgraph_edge()], callgraph()) -> callgraph().

add_edges([], CG) ->
  CG;
add_edges(Edges, #callgraph{digraph = Digraph} = CG) ->
  CG#callgraph{digraph = digraph_add_edges(Edges, Digraph)}.

-spec add_edges([callgraph_edge()], [mfa_or_funlbl()], callgraph()) -> callgraph().

add_edges(Edges, MFAs, #callgraph{digraph = DG} = CG) ->
  DG = digraph_confirm_vertices(MFAs, DG),
  add_edges(Edges, CG).

-spec remove_external(callgraph()) -> {callgraph(), [tuple()]}.

remove_external(#callgraph{digraph = DG} = CG) ->
  {DG, External} = digraph_remove_external(DG),
  {CG, External}.

-spec non_local_calls(callgraph()) -> mfa_calls().

non_local_calls(#callgraph{digraph = DG}) ->
  Edges = digraph_edges(DG),
  find_non_local_calls(Edges, sets:new()).

-spec find_non_local_calls([{mfa_or_funlbl(), mfa_or_funlbl()}], set()) -> mfa_calls().

find_non_local_calls([{{M,_,_}, {M,_,_}}|Left], Set) ->
  find_non_local_calls(Left, Set);
find_non_local_calls([{{M1,_,_}, {M2,_,_}} = Edge|Left], Set) when M1 =/= M2 ->
  find_non_local_calls(Left, sets:add_element(Edge, Set));
find_non_local_calls([{{_,_,_}, Label}|Left], Set) when is_integer(Label) ->
  find_non_local_calls(Left, Set);  
find_non_local_calls([{Label, {_,_,_}}|Left], Set) when is_integer(Label) ->
  find_non_local_calls(Left, Set);
find_non_local_calls([{Label1, Label2}|Left], Set) when is_integer(Label1),
							is_integer(Label2) ->
  find_non_local_calls(Left, Set);
find_non_local_calls([], Set) ->
  sets:to_list(Set).

-spec get_depends_on(scc(), callgraph()) -> [scc()].

get_depends_on(SCC, #callgraph{active_digraph = DG}) ->
  digraph:out_neighbours(DG, SCC).

-spec get_required_by(scc(), callgraph()) -> [scc()].

get_required_by(SCC, #callgraph{active_digraph = DG}) ->
  digraph:in_neighbours(DG, SCC).

%%----------------------------------------------------------------------
%% Handling of modules & SCCs
%%----------------------------------------------------------------------

-spec modules(callgraph()) -> [module()].

modules(#callgraph{digraph = DG}) ->
  ordsets:from_list([M || {M,_F,_A} <- digraph_vertices(DG)]).

-spec module_postorder(callgraph()) -> [module()].

module_postorder(#callgraph{digraph = DG}) ->
  Edges = digraph_edges(DG),
  Nodes = ordsets:from_list([M || {M,_F,_A} <- digraph_vertices(DG)]),
  MDG = digraph:new([acyclic]),
  MDG1 = digraph_confirm_vertices(Nodes, MDG),
  MDG2 = create_module_digraph(Edges, MDG1),
  PostOrder = digraph_utils:postorder(MDG2),
  digraph:delete(MDG2),
  PostOrder.

%% The module deps of a module are modules that depend on the module
-spec module_deps(callgraph()) -> dict().

module_deps(#callgraph{digraph = DG}) ->
  Edges = digraph_edges(DG),
  Nodes = ordsets:from_list([M || {M,_F,_A} <- digraph_vertices(DG)]),
  MDG = digraph:new(),
  MDG1 = digraph_confirm_vertices(Nodes, MDG),
  MDG2 = create_module_digraph(Edges, MDG1),
  Deps = [{N, ordsets:from_list(digraph:in_neighbours(MDG2, N))}
	  || N <- Nodes],
  digraph_delete(MDG2),
  dict:from_list(Deps).

-spec strip_module_deps(dict(), set()) -> dict().

strip_module_deps(ModDeps, StripSet) ->
  FilterFun1 = fun(Val) -> not sets:is_element(Val, StripSet) end,
  MapFun = fun(_Key, ValSet) -> ordsets:filter(FilterFun1, ValSet) end,
  ModDeps1 = dict:map(MapFun, ModDeps),
  FilterFun2 = fun(_Key, ValSet) -> ValSet =/= [] end,
  dict:filter(FilterFun2, ModDeps1).

create_module_digraph([{{M, _, _}, {M, _, _}}|Left], MDG) ->
  create_module_digraph(Left, MDG);
create_module_digraph([{{M1, _, _}, {M2, _, _}}|Left], MDG) ->
  create_module_digraph(Left, digraph_add_edge(M1, M2, MDG));
create_module_digraph([{_, _}|Left], MDG) ->
  create_module_digraph(Left, MDG);
create_module_digraph([], MDG) ->
  MDG.

-spec finalize(callgraph()) -> {[scc()], callgraph()}.

finalize(#callgraph{digraph      = DG,
		    esc          = Esc,
		    name_map     = NameMap,
		    rev_name_map = RevNameMap,
		    rec_var_map  = RecVarMap,
		    self_rec     = SelfRec,
		    calls        = Calls
		   } = CG) ->
  [ETSEsc, ETSNameMap, ETSRevNameMap, ETSRecVarMap, ETSSelfRec, ETSCalls] =
    [ets:new(N,[public]) ||
      N <- [callgraph_esc, callgraph_name_map, callgraph_rev_name_map,
	    callgraph_rec_var_map, callgraph_self_rec, callgraph_calls]],
  [true,true] = [ets:insert(ETS, [{E} || E <- sets:to_list(Data)]) ||
		  {ETS, Data} <- [{ETSEsc, Esc}, {ETSSelfRec, SelfRec}]],
  [true, true, true, true] =
    [ets:insert(ETS, dict:to_list(Data)) ||
      {ETS, Data} <- [{ETSNameMap, NameMap}, {ETSRevNameMap, RevNameMap},
		      {ETSRecVarMap, RecVarMap}, {ETSCalls, Calls}]],
  {ActiveDG, Postorder} = digraph_finalize(DG),
  {Postorder, CG#callgraph{active_digraph = ActiveDG,
			   esc            = ETSEsc,
			   name_map       = ETSNameMap,
			   rev_name_map   = ETSRevNameMap,
			   rec_var_map    = ETSRecVarMap,
			   self_rec       = ETSSelfRec,
			   calls          = ETSCalls}}.

-spec reset_from_funs([mfa_or_funlbl()], callgraph()) -> {[scc()], callgraph()}.

reset_from_funs(Funs, #callgraph{digraph = DG,
				 active_digraph = OldActiveDG} = CG) ->
  digraph_delete(OldActiveDG),
  SubGraph = digraph_reaching_subgraph(Funs, DG),
  {NewActiveDG, Postorder} = digraph_finalize(SubGraph),
  digraph_delete(SubGraph),
  {Postorder, CG#callgraph{active_digraph = NewActiveDG}}.

-spec module_postorder_from_funs([mfa_or_funlbl()], callgraph()) -> [module()].

module_postorder_from_funs(Funs, #callgraph{digraph = DG} = CG) ->
  SubGraph = digraph_reaching_subgraph(Funs, DG),
  PO = module_postorder(CG#callgraph{digraph = SubGraph}),
  digraph_delete(SubGraph),
  PO.

ets_lookup_dict(Key, Table) ->
  try ets:lookup_element(Table, Key, 2) of
      Val -> {ok, Val}
  catch
    _:_ -> error
  end.

ets_lookup_set(Key, Table) ->
  case ets:lookup(Table, Key) of
      [] -> false;
      _ -> true
  end.

%%----------------------------------------------------------------------
%% Core code
%%----------------------------------------------------------------------

%% The core tree must be labeled as by cerl_trees:label/1 (or /2).
%% The set of labels in the tree must be disjoint from the set of
%% labels already occuring in the callgraph.

-spec scan_core_tree(cerl:c_module(), callgraph()) -> callgraph().

scan_core_tree(Tree, #callgraph{calls = OldCalls,
				esc = OldEsc,
				name_map = OldNameMap,
				rec_var_map = OldRecVarMap, 
				rev_name_map = OldRevNameMap,
				self_rec = OldSelfRec} = CG) ->
  %% Build name map and recursion variable maps.
  {NewNameMap, NewRevNameMap, NewRecVarMap} = 
    build_maps(Tree, OldRecVarMap, OldNameMap, OldRevNameMap),
  
  %% First find the module-local dependencies.
  {Deps0, EscapingFuns, Calls} = dialyzer_dep:analyze(Tree),
  NewCalls = dict:merge(fun(_Key, Val, Val) -> Val end, OldCalls, Calls),
  NewEsc = sets:union(sets:from_list(EscapingFuns), OldEsc),
  LabelEdges = get_edges_from_deps(Deps0),
  
  %% Find the self recursive functions. Named functions get both the
  %% key and their name for convenience.
  SelfRecs0 = lists:foldl(fun({Key, Key}, Acc) -> 
			      case dict:find(Key, NewNameMap) of
				error      -> [Key|Acc];
				{ok, Name} -> [Key, Name|Acc]
			      end;
			     (_, Acc) -> Acc
			  end, [], LabelEdges),
  SelfRecs = sets:union(sets:from_list(SelfRecs0), OldSelfRec),
  
  NamedEdges1 = name_edges(LabelEdges, NewNameMap),
  
  %% We need to scan for inter-module calls since these are not tracked
  %% by dialyzer_dep. Note that the caller is always recorded as the
  %% top level function. This is OK since the included functions are
  %% stored as scc with the parent.
  NamedEdges2 = scan_core_funs(Tree),

  %% Confirm all nodes in the tree.
  Names1 = lists:append([[X, Y] || {X, Y} <- NamedEdges1]),
  Names2 = ordsets:from_list(Names1),

  %% Get rid of the 'top' function from nodes and edges.
  Names3 = ordsets:del_element(top, Names2),
  NewNamedEdges2 =
    [E || {From, To} = E <- NamedEdges2, From =/= top, To =/= top],
  NewNamedEdges1 =
    [E || {From, To} = E <- NamedEdges1, From =/= top, To =/= top],
  NamedEdges3 = NewNamedEdges1 ++ NewNamedEdges2,
  CG1 = add_edges(NamedEdges3, Names3, CG),
  CG1#callgraph{calls = NewCalls,
                esc = NewEsc,
                name_map = NewNameMap,
                rec_var_map = NewRecVarMap, 
                rev_name_map = NewRevNameMap,
                self_rec = SelfRecs}.

build_maps(Tree, RecVarMap, NameMap, RevNameMap) ->
  %% We only care about the named (top level) functions. The anonymous
  %% functions will be analysed together with their parents. 
  Defs = cerl:module_defs(Tree),
  Mod = cerl:atom_val(cerl:module_name(Tree)),
  lists:foldl(fun({Var, Function}, {AccNameMap, AccRevNameMap, AccRecVarMap}) ->
		  FunName = cerl:fname_id(Var),
		  Arity = cerl:fname_arity(Var),
		  MFA = {Mod, FunName, Arity},
		  {dict:store(get_label(Function), MFA, AccNameMap),
		   dict:store(MFA, get_label(Function), AccRevNameMap),
		   dict:store(get_label(Var), MFA, AccRecVarMap)}
	      end, {NameMap, RevNameMap, RecVarMap}, Defs).

get_edges_from_deps(Deps) ->
  %% Convert the dependencies as produced by dialyzer_dep to a list of
  %% edges. Also, remove 'external' since we are not interested in
  %% this information.
  Edges = dict:fold(fun(external, _Set, Acc) -> Acc;
		       (Caller, Set, Acc)    ->
			[[{Caller, Callee} || Callee <- Set, 
					      Callee =/= external]|Acc]
		    end, [], Deps),
  lists:flatten(Edges).

name_edges(Edges, NameMap) ->
  %% If a label is present in the name map it is renamed. Otherwise
  %% keep the label as the identity.
  MapFun = fun(X) ->
	       case dict:find(X, NameMap) of
		 error -> X;
		 {ok, MFA} -> MFA
	       end
	   end,
  name_edges(Edges, MapFun, NameMap, []).

name_edges([{From, To}|Left], MapFun, NameMap, Acc) ->
  NewFrom = MapFun(From),
  NewTo = MapFun(To),
  name_edges(Left, MapFun, NameMap, [{NewFrom, NewTo}|Acc]);
name_edges([], _MapFun, _NameMap, Acc) ->
  Acc.

scan_core_funs(Tree) ->
  Defs = cerl:module_defs(Tree),
  Mod = cerl:atom_val(cerl:module_name(Tree)),
  DeepEdges = lists:foldl(fun({Var, Function}, Edges) ->
			      FunName = cerl:fname_id(Var),
			      Arity = cerl:fname_arity(Var),
			      MFA = {Mod, FunName, Arity},
			      [scan_one_core_fun(Function, MFA)|Edges]
			  end, [], Defs),
  lists:flatten(DeepEdges).

scan_one_core_fun(TopTree, FunName) ->
  FoldFun = fun(Tree, Acc) ->
		case cerl:type(Tree) of
		  call ->
		    CalleeM = cerl:call_module(Tree),
		    CalleeF = cerl:call_name(Tree),
		    A = length(cerl:call_args(Tree)),
		    case (cerl:is_c_atom(CalleeM) andalso 
			  cerl:is_c_atom(CalleeF)) of
		      true -> 
			M = cerl:atom_val(CalleeM),
			F = cerl:atom_val(CalleeF),
			case erl_bif_types:is_known(M, F, A) of
			  true -> Acc;
			  false -> [{FunName, {M, F, A}}|Acc]
			end;
		      false -> 
			%% We cannot handle run-time bindings
			Acc
		    end;
		  _ ->
		    %% Nothing that can introduce new edges in the callgraph.
		    Acc
		end
	    end,
  cerl_trees:fold(FoldFun, [], TopTree).

get_label(T) ->
  case cerl:get_ann(T) of
    [{label, L} | _] when is_integer(L) -> L;
    _ -> erlang:error({missing_label, T})
  end.

%%----------------------------------------------------------------------
%% Digraph
%%----------------------------------------------------------------------

digraph_add_edges([{From, To}|Left], DG) ->
  digraph_add_edges(Left, digraph_add_edge(From, To, DG));
digraph_add_edges([], DG) ->
  DG.

digraph_add_edge(From, To, DG) ->
  case digraph:vertex(DG, From) of
    false -> digraph:add_vertex(DG, From);
    {From, _} -> ok
  end,
  case digraph:vertex(DG, To) of
    false -> digraph:add_vertex(DG, To);
    {To, _} -> ok
  end,
  digraph:add_edge(DG, {From, To}, From, To, []),
  DG.

digraph_confirm_vertices([MFA|Left], DG) ->
  digraph:add_vertex(DG, MFA, confirmed),
  digraph_confirm_vertices(Left, DG);
digraph_confirm_vertices([], DG) ->
  DG.
  
digraph_remove_external(DG) ->
  Vertices = digraph:vertices(DG),
  Unconfirmed = remove_unconfirmed(Vertices, DG),
  {DG, Unconfirmed}.

remove_unconfirmed(Vertexes, DG) ->
  remove_unconfirmed(Vertexes, DG, []).

remove_unconfirmed([V|Left], DG, Unconfirmed) ->
  case digraph:vertex(DG, V) of
    {V, confirmed} -> remove_unconfirmed(Left, DG, Unconfirmed);
    {V, []} -> remove_unconfirmed(Left, DG, [V|Unconfirmed])
  end;
remove_unconfirmed([], DG, Unconfirmed) ->
  BadCalls = lists:append([digraph:in_edges(DG, V) || V <- Unconfirmed]),
  BadCallsSorted = lists:keysort(1, BadCalls),
  digraph:del_vertices(DG, Unconfirmed),
  BadCallsSorted.

digraph_delete(DG) ->
  digraph:delete(DG).

digraph_edges(DG) ->
  digraph:edges(DG).

digraph_vertices(DG) ->
  digraph:vertices(DG).

digraph_in_neighbours(V, DG) ->
  case digraph:in_neighbours(DG, V) of
    [] -> none;
    List -> List
  end.

digraph_postorder(Digraph) ->
  digraph_utils:topsort(Digraph).

digraph_finalize(DG) ->
  DG1 = digraph_utils:condensation(DG),
  Postorder = digraph_postorder(DG1),
  {DG1, Postorder}.

digraph_reaching_subgraph(Funs, DG) ->  
  Vertices = digraph_utils:reaching(Funs, DG),
  digraph_utils:subgraph(DG, Vertices).

%%----------------------------------------------------------------------
%% Races
%%----------------------------------------------------------------------

-spec renew_race_info(callgraph(), dict(), [label()], [string()]) ->
        callgraph().

renew_race_info(#callgraph{race_data_server = RaceDataServer} = CG,
		RaceCode, PublicTables, NamedTables) ->
  ok = race_data_server_cast(
	 {renew_race_info, {RaceCode, PublicTables, NamedTables}},
	 RaceDataServer),
  CG.

renew_race_info({RaceCode, PublicTables, NamedTables},
		#race_data_state{} = State) ->
  State#race_data_state{race_code = RaceCode,
			public_tables = PublicTables,
			named_tables = NamedTables}.

-spec renew_race_code(dialyzer_races:races(), callgraph()) -> callgraph().

renew_race_code(Races, #callgraph{race_data_server = RaceDataServer} = CG) ->
  Fun = dialyzer_races:get_curr_fun(Races),
  FunArgs = dialyzer_races:get_curr_fun_args(Races),
  Code = lists:reverse(dialyzer_races:get_race_list(Races)),
  ok = race_data_server_cast(
	 {renew_race_code, {Fun, FunArgs, Code}},
	 RaceDataServer),
  CG;
renew_race_code({Fun, FunArgs, Code},
		#race_data_state{race_code = RaceCode} = State) ->
  State#race_data_state{race_code = dict:store(Fun, [FunArgs, Code], RaceCode)}.

-spec renew_race_public_tables(label(), callgraph()) -> callgraph().

renew_race_public_tables(VarLabel,
			 #callgraph{race_data_server = RaceDataServer} = CG) ->
  ok =
    race_data_server_cast({renew_race_public_tables, VarLabel}, RaceDataServer),
  CG;
renew_race_public_tables(VarLabel,
			 #race_data_state{public_tables = PT} = State) ->
  State#race_data_state{public_tables = ordsets:add_element(VarLabel, PT)}.

-spec cleanup(callgraph()) -> callgraph().

cleanup(#callgraph{digraph = Digraph,
                   name_map = NameMap,
                   rev_name_map = RevNameMap,
		   race_data_server = RaceDataServer}) ->
  #callgraph{digraph = Digraph,
	     name_map = NameMap,
             rev_name_map = RevNameMap,
	     race_data_server = race_data_server_call(dup, RaceDataServer)}.

-spec duplicate(callgraph()) -> callgraph().

duplicate(#callgraph{race_data_server = RaceDataServer} = Callgraph) ->
  Callgraph#callgraph{
    race_data_server = race_data_server_call(dup, RaceDataServer)}.

-spec dispose_race_server(callgraph()) -> ok.

dispose_race_server(#callgraph{race_data_server = RaceDataServer}) ->
  race_data_server_cast(stop, RaceDataServer).

-spec get_digraph(callgraph()) -> digraph().

get_digraph(#callgraph{digraph = Digraph}) ->
  Digraph.

-spec get_named_tables(callgraph()) -> [string()].

get_named_tables(#callgraph{race_data_server = RaceDataServer}) ->
  race_data_server_call(get_named_tables, RaceDataServer).

-spec get_public_tables(callgraph()) -> [label()].

get_public_tables(#callgraph{race_data_server = RaceDataServer}) ->
  race_data_server_call(get_public_tables, RaceDataServer).

-spec get_race_code(callgraph()) -> dict().

get_race_code(#callgraph{race_data_server = RaceDataServer}) ->
  race_data_server_call(get_race_code, RaceDataServer).

-spec get_race_detection(callgraph()) -> boolean().

get_race_detection(#callgraph{race_detection = RD}) ->
  RD.

-spec get_behaviour_api_calls(callgraph()) -> [{mfa(), mfa()}].

get_behaviour_api_calls(#callgraph{race_data_server = RaceDataServer}) ->
  race_data_server_call(get_behaviour_api_calls, RaceDataServer).

-spec race_code_new(callgraph()) -> callgraph().

race_code_new(#callgraph{race_data_server = RaceDataServer} = CG) ->
  ok = race_data_server_cast(race_code_new, RaceDataServer),
  CG.

-spec put_digraph(digraph(), callgraph()) -> callgraph().

put_digraph(Digraph, Callgraph) ->
  Callgraph#callgraph{digraph = Digraph}.

-spec put_race_code(dict(), callgraph()) -> callgraph().

put_race_code(RaceCode, #callgraph{race_data_server = RaceDataServer} = CG) ->
  ok = race_data_server_cast({put_race_code, RaceCode}, RaceDataServer),
  CG.

-spec put_race_detection(boolean(), callgraph()) -> callgraph().

put_race_detection(RaceDetection, Callgraph) ->
  Callgraph#callgraph{race_detection = RaceDetection}.

-spec put_named_tables([string()], callgraph()) -> callgraph().

put_named_tables(NamedTables,
		 #callgraph{race_data_server = RaceDataServer} = CG) ->
  ok = race_data_server_cast({put_named_tables, NamedTables}, RaceDataServer),
  CG.

-spec put_public_tables([label()], callgraph()) -> callgraph().

put_public_tables(PublicTables,
		 #callgraph{race_data_server = RaceDataServer} = CG) ->
  ok = race_data_server_cast({put_public_tables, PublicTables}, RaceDataServer),
  CG.

-spec put_behaviour_api_calls([{mfa(), mfa()}], callgraph()) -> callgraph().

put_behaviour_api_calls(Calls,
		 #callgraph{race_data_server = RaceDataServer} = CG) ->
  ok = race_data_server_cast({put_behaviour_api_calls, Calls}, RaceDataServer),
  CG.


new_race_data_server() ->
  spawn(fun() -> race_data_server_loop(#race_data_state{}) end).

race_data_server_loop(State) ->
  receive
    {call, From, Ref, Query} ->
      Reply = race_data_server_handle_call(Query, State),
      From ! {Ref, Reply},
      race_data_server_loop(State);
    {cast, stop} ->
      ok;
    {cast, Message} ->
      NewState = race_data_server_handle_cast(Message, State),
      race_data_server_loop(NewState)
  end.

race_data_server_call(Query, Server) ->
  Ref = make_ref(),
  Server ! {call, self(), Ref, Query},
  receive
    {Ref, Reply} -> Reply
  end.

race_data_server_cast(Message, Server) ->
  Server ! {cast, Message},
  ok.

race_data_server_handle_cast(race_code_new, State) ->
  State#race_data_state{race_code = dict:new()};
race_data_server_handle_cast({Tag, Data}, State) ->
  case Tag of
    renew_race_info -> renew_race_info(Data, State);
    renew_race_code -> renew_race_code(Data, State);
    renew_race_public_tables -> renew_race_public_tables(Data, State);
    put_race_code -> State#race_data_state{race_code = Data};
    put_public_tables -> State#race_data_state{public_tables = Data};
    put_named_tables -> State#race_data_state{named_tables = Data};
    put_behaviour_api_calls -> State#race_data_state{beh_api_calls = Data}
  end.

race_data_server_handle_call(Query,
			     #race_data_state{race_code = RaceCode,
					      public_tables = PublicTables,
					      named_tables = NamedTables,
					      beh_api_calls = BehApiCalls}
			     = State) ->
  case Query of
    dup -> spawn(fun() -> race_data_server_loop(State) end);
    get_race_code -> RaceCode;
    get_public_tables -> PublicTables;
    get_named_tables -> NamedTables;
    get_behaviour_api_calls -> BehApiCalls
  end.

%%=============================================================================
%% Utilities for 'dot'
%%=============================================================================

-spec to_dot(callgraph(), file:filename()) -> 'ok'.

to_dot(#callgraph{digraph = DG, esc = Esc} = CG, File) ->
  Fun = fun(L) ->
	    case lookup_name(L, CG) of
	      error -> L;
	      {ok, Name} -> Name
	    end
	end,
  Escaping = [{Fun(L), {color, red}} 
	      || L <- sets:to_list(Esc), L =/= external],
  Vertices = digraph_edges(DG),
  hipe_dot:translate_list(Vertices, File, "CG", Escaping).

-spec to_ps(callgraph(), file:filename(), string()) -> 'ok'.

to_ps(#callgraph{} = CG, File, Args) ->
  Dot_File = filename:rootname(File) ++ ".dot",
  to_dot(CG, Dot_File),
  Command = io_lib:format("dot -Tps ~s -o ~s ~s", [Args, File, Dot_File]),
  _ = os:cmd(Command),
  ok.
