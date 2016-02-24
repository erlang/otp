%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2015. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
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
	 add_edges/3,
	 all_nodes/1,
	 delete/1,
	 finalize/1,
	 is_escaping/2,
	 is_self_rec/2,
	 non_local_calls/1,
	 lookup_letrec/2,
	 lookup_rec_var/2,
	 lookup_call_site/2,
	 lookup_label/2,
	 lookup_name/2,
	 modules/1,
	 module_deps/1,
	 %% module_postorder/1,
	 module_postorder_from_funs/2,
	 new/0,
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

-export_type([callgraph/0, mfa_or_funlbl/0, callgraph_edge/0, mod_deps/0]).

-include("dialyzer.hrl").

%%----------------------------------------------------------------------

-type scc()	      :: [mfa_or_funlbl()].
-type mfa_call()      :: {mfa_or_funlbl(), mfa_or_funlbl()}.
-type mfa_calls()     :: [mfa_call()].
-type mod_deps()      :: dict:dict(module(), [module()]).

%%-----------------------------------------------------------------------------
%% A callgraph is a directed graph where the nodes are functions and a
%% call between two functions is an edge from the caller to the callee.
%% 
%% calls	-  A mapping from call site (and apply site) labels
%%		   to the possible functions that can be called.
%% digraph	-  A digraph representing the callgraph. 
%%		   Nodes are represented as MFAs or labels.
%% esc		-  A set of all escaping functions as reported by dialyzer_dep.
%% letrec_map	-  A dict mapping from letrec bound labels to function labels.
%%		   Includes all functions.
%% name_map	-  A mapping from label to MFA.
%% rev_name_map	-  A reverse mapping of the name_map.
%% rec_var_map	-  A dict mapping from letrec bound labels to function names.
%%		   Only for top level functions (from module defs).
%% self_rec	-  A set containing all self recursive functions.
%%		   Note that this contains MFAs for named functions and labels
%%		   whenever applicable.
%%-----------------------------------------------------------------------------

%% Types with comment 'race' are due to dialyzer_races.erl.
-record(callgraph, {digraph        = digraph:new() :: digraph:graph(),
		    active_digraph                 :: active_digraph()
                                                    | 'undefined', % race
                    esc	                           :: ets:tid()
                                                    | 'undefined', % race
                    letrec_map                     :: ets:tid()
                                                    | 'undefined', % race
                    name_map	                   :: ets:tid(),
                    rev_name_map                   :: ets:tid(),
                    rec_var_map                    :: ets:tid()
                                                    | 'undefined', % race
                    self_rec	                   :: ets:tid()
                                                    | 'undefined', % race
                    calls                          :: ets:tid()
                                                    | 'undefined', % race
                    race_detection = false         :: boolean(),
		    race_data_server = dialyzer_race_data_server:new() :: pid()}).

%% Exported Types

-opaque callgraph() :: #callgraph{}.

-type active_digraph() :: {'d', digraph:graph()} | {'e', ets:tid(), ets:tid()}.

%%----------------------------------------------------------------------

-spec new() -> callgraph().

new() ->
  [ETSEsc, ETSNameMap, ETSRevNameMap, ETSRecVarMap, ETSLetrecMap, ETSSelfRec, ETSCalls] =
    [ets:new(N,[public, {read_concurrency, true}]) ||
      N <- [callgraph_esc, callgraph_name_map, callgraph_rev_name_map,
	    callgraph_rec_var_map, callgraph_letrec_map, callgraph_self_rec, callgraph_calls]],
  #callgraph{esc            = ETSEsc,
	     letrec_map     = ETSLetrecMap,
	     name_map       = ETSNameMap,
	     rev_name_map   = ETSRevNameMap,
	     rec_var_map    = ETSRecVarMap,
	     self_rec       = ETSSelfRec,
	     calls          = ETSCalls}.

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

-spec lookup_letrec(label(), callgraph()) -> 'error' | {'ok', label()}.

lookup_letrec(Label, #callgraph{letrec_map = LetrecMap})
  when is_integer(Label) ->
  ets_lookup_dict(Label, LetrecMap).

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
-spec add_edges([callgraph_edge()], callgraph()) -> ok.

add_edges([], _CG) ->
  ok;
add_edges(Edges, #callgraph{digraph = Digraph}) ->
  digraph_add_edges(Edges, Digraph).

-spec add_edges([callgraph_edge()], [mfa_or_funlbl()], callgraph()) -> ok.

add_edges(Edges, MFAs, #callgraph{digraph = DG} = CG) ->
  digraph_confirm_vertices(MFAs, DG),
  add_edges(Edges, CG).

-spec remove_external(callgraph()) -> {callgraph(), [tuple()]}.

remove_external(#callgraph{digraph = DG} = CG) ->
  {DG, External} = digraph_remove_external(DG),
  {CG, External}.

-spec non_local_calls(callgraph()) -> mfa_calls().

non_local_calls(#callgraph{digraph = DG}) ->
  Edges = digraph_edges(DG),
  find_non_local_calls(Edges, sets:new()).

-type call_tab() :: sets:set(mfa_call()).

-spec find_non_local_calls([{mfa_or_funlbl(), mfa_or_funlbl()}], call_tab()) ->
        mfa_calls().

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

-spec get_depends_on(scc() | module(), callgraph()) -> [scc()].

get_depends_on(SCC, #callgraph{active_digraph = {'e', Out, _In}}) ->
  case ets_lookup_dict(SCC, Out) of
    {ok, Value} -> Value;
    error -> []
  end;
get_depends_on(SCC, #callgraph{active_digraph = {'d', DG}}) ->
  digraph:out_neighbours(DG, SCC).

-spec get_required_by(scc() | module(), callgraph()) -> [scc()].

get_required_by(SCC, #callgraph{active_digraph = {'e', _Out, In}}) ->
  case ets_lookup_dict(SCC, In) of
    {ok, Value} -> Value;
    error -> []
  end;
get_required_by(SCC, #callgraph{active_digraph = {'d', DG}}) ->
  digraph:in_neighbours(DG, SCC).

%%----------------------------------------------------------------------
%% Handling of modules & SCCs
%%----------------------------------------------------------------------

-spec modules(callgraph()) -> [module()].

modules(#callgraph{digraph = DG}) ->
  ordsets:from_list([M || {M,_F,_A} <- digraph_vertices(DG)]).

-spec module_postorder(callgraph()) -> {[module()], {'d', digraph:graph()}}.

module_postorder(#callgraph{digraph = DG}) ->
  Edges = lists:foldl(fun edge_fold/2, sets:new(), digraph_edges(DG)),
  Nodes = sets:from_list([M || {M,_F,_A} <- digraph_vertices(DG)]),
  MDG = digraph:new([acyclic]),
  digraph_confirm_vertices(sets:to_list(Nodes), MDG),
  Foreach = fun({M1,M2}) -> digraph:add_edge(MDG, M1, M2) end,
  lists:foreach(Foreach, sets:to_list(Edges)),
  {digraph_utils:topsort(MDG), {'d', MDG}}.

edge_fold({{M1,_,_},{M2,_,_}}, Set) ->
  case M1 =/= M2 of
    true  -> sets:add_element({M1,M2},Set);
    false -> Set
  end;
edge_fold(_, Set) -> Set.


%% The module deps of a module are modules that depend on the module
-spec module_deps(callgraph()) -> mod_deps().

module_deps(#callgraph{digraph = DG}) ->
  Edges = lists:foldl(fun edge_fold/2, sets:new(), digraph_edges(DG)),
  Nodes = sets:from_list([M || {M,_F,_A} <- digraph_vertices(DG)]),
  MDG = digraph:new(),
  digraph_confirm_vertices(sets:to_list(Nodes), MDG),
  Foreach = fun({M1,M2}) -> digraph:add_edge(MDG, M1, M2) end,
  lists:foreach(Foreach, sets:to_list(Edges)),
  Deps = [{N, ordsets:from_list(digraph:in_neighbours(MDG, N))}
	  || N <- sets:to_list(Nodes)],
  digraph_delete(MDG),
  dict:from_list(Deps).

-spec strip_module_deps(mod_deps(), sets:set(module())) -> mod_deps().

strip_module_deps(ModDeps, StripSet) ->
  FilterFun1 = fun(Val) -> not sets:is_element(Val, StripSet) end,
  MapFun = fun(_Key, ValSet) -> ordsets:filter(FilterFun1, ValSet) end,
  ModDeps1 = dict:map(MapFun, ModDeps),
  FilterFun2 = fun(_Key, ValSet) -> ValSet =/= [] end,
  dict:filter(FilterFun2, ModDeps1).

-spec finalize(callgraph()) -> {[scc()], callgraph()}.

finalize(#callgraph{digraph = DG} = CG) ->
  {ActiveDG, Postorder} = condensation(DG),
  {Postorder, CG#callgraph{active_digraph = ActiveDG}}.

-spec reset_from_funs([mfa_or_funlbl()], callgraph()) -> {[scc()], callgraph()}.

reset_from_funs(Funs, #callgraph{digraph = DG, active_digraph = ADG} = CG) ->
  active_digraph_delete(ADG),
  SubGraph = digraph_reaching_subgraph(Funs, DG),
  {NewActiveDG, Postorder} = condensation(SubGraph),
  digraph_delete(SubGraph),
  {Postorder, CG#callgraph{active_digraph = NewActiveDG}}.

-spec module_postorder_from_funs([mfa_or_funlbl()], callgraph()) ->
        {[module()], callgraph()}.

module_postorder_from_funs(Funs, #callgraph{digraph = DG,
					    active_digraph = ADG} = CG) ->
  active_digraph_delete(ADG),
  SubGraph = digraph_reaching_subgraph(Funs, DG),
  {PO, Active} = module_postorder(CG#callgraph{digraph = SubGraph}),
  digraph_delete(SubGraph),
  {PO, CG#callgraph{active_digraph = Active}}.

ets_lookup_dict(Key, Table) ->
  try ets:lookup_element(Table, Key, 2) of
      Val -> {ok, Val}
  catch
    _:_ -> error
  end.

ets_lookup_set(Key, Table) ->
  ets:lookup(Table, Key) =/= [].

%%----------------------------------------------------------------------
%% Core code
%%----------------------------------------------------------------------

%% The core tree must be labeled as by cerl_trees:label/1 (or /2).
%% The set of labels in the tree must be disjoint from the set of
%% labels already occuring in the callgraph.

-spec scan_core_tree(cerl:c_module(), callgraph()) ->
        {[mfa_or_funlbl()], [callgraph_edge()]}.

scan_core_tree(Tree, #callgraph{calls = ETSCalls,
				esc = ETSEsc,
				letrec_map = ETSLetrecMap,
				name_map = ETSNameMap,
				rec_var_map = ETSRecVarMap,
				rev_name_map = ETSRevNameMap,
				self_rec = ETSSelfRec}) ->
  %% Build name map and recursion variable maps.
  build_maps(Tree, ETSRecVarMap, ETSNameMap, ETSRevNameMap, ETSLetrecMap),

  %% First find the module-local dependencies.
  {Deps0, EscapingFuns, Calls, Letrecs} = dialyzer_dep:analyze(Tree),
  true = ets:insert(ETSCalls, dict:to_list(Calls)),
  true = ets:insert(ETSLetrecMap, dict:to_list(Letrecs)),
  true = ets:insert(ETSEsc, [{E} || E <- EscapingFuns]),

  LabelEdges = get_edges_from_deps(Deps0),
  
  %% Find the self recursive functions. Named functions get both the
  %% key and their name for convenience.
  SelfRecs0 = lists:foldl(fun({Key, Key}, Acc) -> 
			      case ets_lookup_dict(Key, ETSNameMap) of
				error      -> [Key|Acc];
				{ok, Name} -> [Key, Name|Acc]
			      end;
			     (_, Acc) -> Acc
			  end, [], LabelEdges),
  true = ets:insert(ETSSelfRec, [{S} || S <- SelfRecs0]),
  
  NamedEdges1 = name_edges(LabelEdges, ETSNameMap),
  
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
  {Names3, NamedEdges3}.

build_maps(Tree, ETSRecVarMap, ETSNameMap, ETSRevNameMap, ETSLetrecMap) ->
  %% We only care about the named (top level) functions. The anonymous
  %% functions will be analysed together with their parents. 
  Defs = cerl:module_defs(Tree),
  Mod = cerl:atom_val(cerl:module_name(Tree)),
  Fun =
    fun({Var, Function}) ->
	FunName = cerl:fname_id(Var),
	Arity = cerl:fname_arity(Var),
	MFA = {Mod, FunName, Arity},
	FunLabel = get_label(Function),
	VarLabel = get_label(Var),
	true = ets:insert(ETSLetrecMap, {VarLabel, FunLabel}),
	true = ets:insert(ETSNameMap, {FunLabel, MFA}),
	true = ets:insert(ETSRevNameMap, {MFA, FunLabel}),
	true = ets:insert(ETSRecVarMap, {VarLabel, MFA})
    end,
  lists:foreach(Fun, Defs).

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

name_edges(Edges, ETSNameMap) ->
  %% If a label is present in the name map it is renamed. Otherwise
  %% keep the label as the identity.
  MapFun = fun(X) ->
	       case ets_lookup_dict(X, ETSNameMap) of
		 error -> X;
		 {ok, MFA} -> MFA
	       end
	   end,
  name_edges(Edges, MapFun, []).

name_edges([{From, To}|Left], MapFun, Acc) ->
  NewFrom = MapFun(From),
  NewTo = MapFun(To),
  name_edges(Left, MapFun, [{NewFrom, NewTo}|Acc]);
name_edges([], _MapFun, Acc) ->
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
		    CalleeArgs = cerl:call_args(Tree),
		    A = length(CalleeArgs),
		    case (cerl:is_c_atom(CalleeM) andalso 
			  cerl:is_c_atom(CalleeF)) of
		      true -> 
			M = cerl:atom_val(CalleeM),
			F = cerl:atom_val(CalleeF),
			case erl_bif_types:is_known(M, F, A) of
			  true ->
			    case {M, F, A} of
			      {erlang, make_fun, 3} ->
				[CA1, CA2, CA3] = CalleeArgs,
				case
				  cerl:is_c_atom(CA1) andalso
				  cerl:is_c_atom(CA2) andalso
				  cerl:is_c_int(CA3)
				of
				  true ->
				    MM = cerl:atom_val(CA1),
				    FF = cerl:atom_val(CA2),
				    AA = cerl:int_val(CA3),
				    case erl_bif_types:is_known(MM, FF, AA) of
				      true -> Acc;
				      false -> [{FunName, {MM, FF, AA}}|Acc]
				    end;
				  false ->
				    Acc
				end;
			      _ ->
				Acc
			    end;
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
  digraph_add_edge(From, To, DG),
  digraph_add_edges(Left, DG);
digraph_add_edges([], _DG) ->
  ok.

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
  ok.

digraph_confirm_vertices([MFA|Left], DG) ->
  digraph:add_vertex(DG, MFA, confirmed),
  digraph_confirm_vertices(Left, DG);
digraph_confirm_vertices([], _DG) ->
  ok.
  
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

active_digraph_delete({'d', DG}) ->
  digraph:delete(DG);
active_digraph_delete({'e', Out, In}) ->
  ets:delete(Out),
  ets:delete(In).

digraph_edges(DG) ->
  digraph:edges(DG).

digraph_vertices(DG) ->
  digraph:vertices(DG).

digraph_in_neighbours(V, DG) ->
  case digraph:in_neighbours(DG, V) of
    [] -> none;
    List -> List
  end.

digraph_reaching_subgraph(Funs, DG) ->  
  Vertices = digraph_utils:reaching(Funs, DG),
  digraph_utils:subgraph(DG, Vertices).

%%----------------------------------------------------------------------
%% Races
%%----------------------------------------------------------------------

-spec renew_race_info(callgraph(), dict:dict(), [label()], [string()]) ->
        callgraph().

renew_race_info(#callgraph{race_data_server = RaceDataServer} = CG,
		RaceCode, PublicTables, NamedTables) ->
  ok = dialyzer_race_data_server:cast(
	 {renew_race_info, {RaceCode, PublicTables, NamedTables}},
	 RaceDataServer),
  CG.

-spec renew_race_code(dialyzer_races:races(), callgraph()) -> callgraph().

renew_race_code(Races, #callgraph{race_data_server = RaceDataServer} = CG) ->
  Fun = dialyzer_races:get_curr_fun(Races),
  FunArgs = dialyzer_races:get_curr_fun_args(Races),
  Code = lists:reverse(dialyzer_races:get_race_list(Races)),
  ok = dialyzer_race_data_server:cast(
	 {renew_race_code, {Fun, FunArgs, Code}},
	 RaceDataServer),
  CG.

-spec renew_race_public_tables(label(), callgraph()) -> callgraph().

renew_race_public_tables(VarLabel,
			 #callgraph{race_data_server = RaceDataServer} = CG) ->
  ok =
    dialyzer_race_data_server:cast({renew_race_public_tables, VarLabel}, RaceDataServer),
  CG.

-spec cleanup(callgraph()) -> callgraph().

cleanup(#callgraph{digraph = Digraph,
                   name_map = NameMap,
                   rev_name_map = RevNameMap,
		   race_data_server = RaceDataServer}) ->
  #callgraph{digraph = Digraph,
	     name_map = NameMap,
             rev_name_map = RevNameMap,
	     race_data_server = dialyzer_race_data_server:duplicate(RaceDataServer)}.

-spec duplicate(callgraph()) -> callgraph().

duplicate(#callgraph{race_data_server = RaceDataServer} = Callgraph) ->
  Callgraph#callgraph{
    race_data_server = dialyzer_race_data_server:duplicate(RaceDataServer)}.

-spec dispose_race_server(callgraph()) -> ok.

dispose_race_server(#callgraph{race_data_server = RaceDataServer}) ->
  dialyzer_race_data_server:stop(RaceDataServer).

-spec get_digraph(callgraph()) -> digraph:graph().

get_digraph(#callgraph{digraph = Digraph}) ->
  Digraph.

-spec get_named_tables(callgraph()) -> [string()].

get_named_tables(#callgraph{race_data_server = RaceDataServer}) ->
  dialyzer_race_data_server:call(get_named_tables, RaceDataServer).

-spec get_public_tables(callgraph()) -> [label()].

get_public_tables(#callgraph{race_data_server = RaceDataServer}) ->
  dialyzer_race_data_server:call(get_public_tables, RaceDataServer).

-spec get_race_code(callgraph()) -> dict:dict().

get_race_code(#callgraph{race_data_server = RaceDataServer}) ->
  dialyzer_race_data_server:call(get_race_code, RaceDataServer).

-spec get_race_detection(callgraph()) -> boolean().

get_race_detection(#callgraph{race_detection = RD}) ->
  RD.

-spec get_behaviour_api_calls(callgraph()) -> [{mfa(), mfa()}].

get_behaviour_api_calls(#callgraph{race_data_server = RaceDataServer}) ->
  dialyzer_race_data_server:call(get_behaviour_api_calls, RaceDataServer).

-spec race_code_new(callgraph()) -> callgraph().

race_code_new(#callgraph{race_data_server = RaceDataServer} = CG) ->
  ok = dialyzer_race_data_server:cast(race_code_new, RaceDataServer),
  CG.

-spec put_digraph(digraph:graph(), callgraph()) -> callgraph().

put_digraph(Digraph, Callgraph) ->
  Callgraph#callgraph{digraph = Digraph}.

-spec put_race_code(dict:dict(), callgraph()) -> callgraph().

put_race_code(RaceCode, #callgraph{race_data_server = RaceDataServer} = CG) ->
  ok = dialyzer_race_data_server:cast({put_race_code, RaceCode}, RaceDataServer),
  CG.

-spec put_race_detection(boolean(), callgraph()) -> callgraph().

put_race_detection(RaceDetection, Callgraph) ->
  Callgraph#callgraph{race_detection = RaceDetection}.

-spec put_named_tables([string()], callgraph()) -> callgraph().

put_named_tables(NamedTables,
		 #callgraph{race_data_server = RaceDataServer} = CG) ->
  ok = dialyzer_race_data_server:cast({put_named_tables, NamedTables}, RaceDataServer),
  CG.

-spec put_public_tables([label()], callgraph()) -> callgraph().

put_public_tables(PublicTables,
		 #callgraph{race_data_server = RaceDataServer} = CG) ->
  ok = dialyzer_race_data_server:cast({put_public_tables, PublicTables}, RaceDataServer),
  CG.

-spec put_behaviour_api_calls([{mfa(), mfa()}], callgraph()) -> callgraph().

put_behaviour_api_calls(Calls,
		 #callgraph{race_data_server = RaceDataServer} = CG) ->
  ok = dialyzer_race_data_server:cast({put_behaviour_api_calls, Calls}, RaceDataServer),
  CG.

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
	      || L <- [E || {E} <- ets:tab2list(Esc)], L =/= external],
  Vertices = digraph_edges(DG),
  hipe_dot:translate_list(Vertices, File, "CG", Escaping).

-spec to_ps(callgraph(), file:filename(), string()) -> 'ok'.

to_ps(#callgraph{} = CG, File, Args) ->
  Dot_File = filename:rootname(File) ++ ".dot",
  to_dot(CG, Dot_File),
  Command = io_lib:format("dot -Tps ~s -o ~s ~s", [Args, File, Dot_File]),
  _ = os:cmd(Command),
  ok.

condensation(G) ->
  SCs = digraph_utils:strong_components(G),
  V2I = ets:new(condensation_v2i, []),
  I2C = ets:new(condensation_i2c, []),
  I2I = ets:new(condensation_i2i, [bag]),
  CFun =
    fun(SC, N) ->
	lists:foreach(fun(V) -> true = ets:insert(V2I, {V,N}) end, SC),
	true = ets:insert(I2C, {N, SC}),
	N + 1
    end,
  lists:foldl(CFun, 1, SCs),
  Fun1 =
    fun({V1, V2}) ->
        I1 = ets:lookup_element(V2I, V1, 2),
        I2 = ets:lookup_element(V2I, V2, 2),
	I1 =:= I2 orelse ets:insert(I2I, {I1, I2})
    end,
  lists:foreach(Fun1, digraph:edges(G)),
  Fun3 =
    fun({I1, I2}, {Out, In}) ->
        SC1 = ets:lookup_element(I2C, I1, 2),
        SC2 = ets:lookup_element(I2C, I2, 2),
        {dict:append(SC1, SC2, Out), dict:append(SC2, SC1, In)}
    end,
  {OutDict, InDict} = ets:foldl(Fun3, {dict:new(), dict:new()}, I2I),
  [OutETS, InETS] =
    [ets:new(Name,[{read_concurrency, true}]) ||
      Name <- [callgraph_deps_out, callgraph_deps_in]],
  ets:insert(OutETS, dict:to_list(OutDict)),
  ets:insert(InETS, dict:to_list(InDict)),
  ets:delete(V2I),
  ets:delete(I2C),
  ets:delete(I2I),
  {{'e', OutETS, InETS}, SCs}.
