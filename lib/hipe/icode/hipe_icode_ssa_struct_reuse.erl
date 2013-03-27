%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2009. All Rights Reserved.
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
%%%=======================================================================
%% File        : hipe_icode_ssa_struct_reuse.erl
%% Author      : Ragnar Osterlund <ragoster@gmail.com>
%%               student at the compiler techniques 2 course at UU 2007
%% Description : HiPE module that removes redundant or partially redundant
%%		 structure creations from Icode.
%%		 It does so by inserting redundant expressions as late
%%		 as possible in the CFG, with the exception of loops where
%%		 expressions are moved to just before the loop head.
%%		 Current Icode instructions that can be moved are mktuple()
%%		 and cons() primop calls. It also handles cases like 
%%		 f({Z}) -> {Z}. It does so by looking at the structure of
%%		 the match, and recognizes tuples and conses.
%%=======================================================================

-module(hipe_icode_ssa_struct_reuse).

-export([struct_reuse/1]).

-include("../main/hipe.hrl").
-include("hipe_icode.hrl").
-include("hipe_icode_primops.hrl").
-include("../flow/cfg.hrl").

-define(SET,  ordset).
-define(SETS, ordsets).
%%-define(DEBUG, true).

-define(MKTUPLE, mktuple).
-define(CONS, cons).
-define(SR_INSTR_TYPE, sr_instr_type).
-define(SR_STRUCT_INSTR_TYPE, sr_struct_instr_type).

-type struct_type()  :: {?CONS | ?MKTUPLE, icode_term_arg(), any()}.
-type struct_elems() :: {icode_var(), non_neg_integer(), icode_term_arg()}.

%% DATATYPE AREA

%%-----------------------------------------------------------------------------
%% maps
%% The maps are used to identify variables and expressions. 
%% The maps are:
%%
%% expr - a map that contains value numbered structure expressions, ie
%% mktuple and cons expression. The key is the value number and the value
%% is an expr record.
%%
%% instr - maps the semantic instruction to an expression value number, 
%% that is, a key in the expr map above.
%%
%% var - maps variables to expression value numbers. These variables are
%% defined or used by the structure expressions.

-record(maps, {var   = gb_trees:empty() :: gb_tree(),
	       instr = gb_trees:empty() :: gb_tree(),
	       expr  = gb_trees:empty() :: gb_tree()}).

maps_var(#maps{var = Out}) -> Out.
maps_instr(#maps{instr = Out}) -> Out.
maps_expr(#maps{expr = Out}) -> Out.

maps_expr_keys(Maps) -> gb_trees:keys(maps_expr(Maps)).
maps_expr_values(Maps) -> gb_trees:values(maps_expr(Maps)).

maps_instr_lookup(Instr, Maps) -> gb_trees:lookup(Instr, maps_instr(Maps)).
maps_instr_enter(Instr, ExprId, Maps) -> 
  NewInstr = gb_trees:enter(Instr, ExprId, maps_instr(Maps)),
  Maps#maps{instr = NewInstr}.

maps_expr_get(Id, Maps) -> gb_trees:get(Id, maps_expr(Maps)).
maps_expr_enter(Expr, Maps) -> 
  NewExprMap = gb_trees:enter(expr_id(Expr), Expr, maps_expr(Maps)),
  Maps#maps{expr = NewExprMap}.

maps_var_get(Var, Maps) -> gb_trees:get(Var, maps_var(Maps)).
maps_var_lookup(Var, #maps{var = VarMap}) -> gb_trees:lookup(Var, VarMap).
maps_var_enter(Var, Info, Maps = #maps{var = VarMap}) -> 
  NewMap = gb_trees:enter(Var, Info, VarMap),
  Maps#maps{var = NewMap}.
maps_var_insert(Var, Info, Maps = #maps{var = VarMap}) -> 
  NewMap = gb_trees:insert(Var, Info, VarMap),
  Maps#maps{var = NewMap}.

maps_balance(Maps) ->
  Maps#maps{instr = gb_trees:balance(maps_instr(Maps)),
	    expr = gb_trees:balance(maps_expr(Maps)),
	    var = gb_trees:balance(maps_var(Maps))}.

maps_expr_key_enter(Expr, Maps) ->
  NewMaps = maps_instr_enter(expr_key(Expr), expr_id(Expr), Maps),
  maps_expr_enter(Expr, NewMaps).

%%-----------------------------------------------------------------------------
%% expr
%% An expression record. Contains information about a structure expression. 
%% The fields are:
%%
%% id - the value number of the expression
%% key - the semantic instruction, as defined in icode, with destination
%%	 removed and arguments rewritten.
%% defs - destination variable to hold the value of the expression.
%% direct_replace - indicates whether the expression shall be replaced wherever
%%	 it occurs, although it might not have been inserted. This is used for
%%	 the expressions that are detected by the icode type constructs.
%% inserts - a list of node labels that will insert this expression
%% use - a list of expression value numbers that use the value of this
%%	 expression

-record(expr, {id 	      = none        :: 'none' | non_neg_integer(),
	       key	      = none        :: 'none' | tuple(), % illegal_icode_instr()
	       defs 	      = none        :: 'none' | [icode_var()],
	       direct_replace = false       :: boolean(),
	       inserts 	      = ?SETS:new() :: ?SETS:?SET(_),
	       use 	      = ?SETS:new() :: ?SETS:?SET(_)}).

expr_id(#expr{id = Out}) -> Out.
expr_defs(#expr{defs = Out}) -> Out.
expr_key(#expr{key = Out}) -> Out.
expr_inserts(#expr{inserts = Out}) -> Out.
expr_use(#expr{use = Out}) -> Out.
expr_direct_replace(#expr{direct_replace = Out}) -> Out.

expr_use_add(Expr = #expr{use = UseSet}, Use) -> 
  Expr#expr{use = ?SETS:add_element(Use, UseSet)}.

%% expr_key_set(Expr, In) -> Expr#expr{key = In}.
expr_direct_replace_set(Expr, In) -> Expr#expr{direct_replace = In}.
expr_inserts_set(Expr, In) -> Expr#expr{inserts = In}.

expr_create(Key, Defs) ->
  NewExprId = new_expr_id(), 
  #expr{id = NewExprId, key = Key, defs = Defs}.
	
%%-----------------------------------------------------------------------------
%% varinfo
%% A variable mapping info. Contains info about variable references.
%% The fields are:
%%
%% use - a set of expression value numbers that use this variable
%% ref - the variable which value this variable will be assigned
%%      when expression is replaced. This is encoded as {N, M} where
%%	N is the expression value number and M is the nth destination
%%	variable defined by the expression N.
%% elem - indicates that this variable has been detected to be a part of
%%	a tuple. The field contains a {V, N} tuple where V is the variable
%%	that refers to the structure that this variable is an element in
%%	and N is the position that the element occurs on in the tuple. Eg.
%%	{{var, 3}, 2} means that the variable {var, 3} refers to a tuple
%%	in which this variable is on second place.
%% exprid - a expression value number which is the expression that
%%          the variable is defined by.

-record(varinfo, {use = ?SETS:new() :: ?SETS:?SET(_),
		  ref = none        :: 'none' | {non_neg_integer(), non_neg_integer()},
		  elem = none       :: 'none' | {icode_var(), non_neg_integer()},
		  exprid = none     :: 'none' | non_neg_integer()}).

varinfo_exprid(#varinfo{exprid = Out}) -> Out.

varinfo_use_add(#varinfo{use = UseSet} = I, Use) -> 
  I#varinfo{use = ?SETS:add_element(Use, UseSet)}.

%%-----------------------------------------------------------------------------
%% node - a node in the temp CFG. 
%% 
%% label - the label of the node in the original CFG
%% pred - a list of predecessors to this node
%% succ - a list of successors to this node
%% code - code from CFG filtered to only contain structure instructions
%% non_struct_defs - a list of variable definitions that are not defined
%% 	by structures
%% up_expr - upwards exposed expression value numbers
%% killed_expr - killed expressions value numbers
%% sub_inserts - a set of labels of nodes that defines one or more
%% 	expressions and that are in a subtree of this node
%% inserts - a set of expression value numbers to be inserted into the node
%% antic_in - a set of expression value numbers that are anticipated into
%%      the node
%% antic_out - a set of expression value numbers that are anticipated out of
%%      the node
%% phi - a tree of node labels which is defined in phi functions in the node
%% varmap - a list of variable tuples {V1, V2} that maps a variable that are
%% 	the output of phi functions in sub blocks, V1, into a variable
%%      flowing from the block of this node, V2.
%% struct_type - a list of {V, N} tuples that indicates that V is a tuple
%%      with N elements. These are added from the icode primop type().
%% struct_elems - a list of {VD, N, VS} tuples where VD is a variable in the N'th position
%% 	in VS. These are added from the icode primop unsafe_element()

-record(node, {
  label       		= none             :: 'none' | icode_lbl(),
  pred			= none             :: 'none' | [icode_lbl()],
  succ 			= none             :: 'none' | [icode_lbl()],
  code     		= []               :: [tuple()], % [illegal_icode_instr()]
  phi			= gb_trees:empty() :: gb_tree(),
  varmap		= []               :: [{icode_var(), icode_var()}],
  pre_loop		= false            :: boolean(),
  non_struct_defs 	= gb_sets:new()    :: gb_set(),
  up_expr     		= none             :: 'none' | ?SETS:?SET(_),
  killed_expr 		= none             :: 'none' | ?SETS:?SET(_),
  sub_inserts		= ?SETS:new()      :: ?SETS:?SET(_),
  inserts 		= ?SETS:new()      :: ?SETS:?SET(_),
  antic_in    		= none             :: 'none' | ?SETS:?SET(_),
  antic_out   		= none             :: 'none' | ?SETS:?SET(_),
  struct_type		= []               :: [struct_type()],
  struct_elems		= []               :: [struct_elems()]}).

node_sub_inserts(#node{sub_inserts = Out}) -> Out.
node_inserts(#node{inserts = Out}) -> Out.
node_antic_out(#node{antic_out = Out}) -> Out.
node_antic_in(#node{antic_in = Out}) -> Out.
node_killed_expr(#node{killed_expr = Out}) -> Out.
node_pred(#node{pred = Out}) -> Out.
node_succ(#node{succ = Out}) -> Out.
node_label(#node{label = Out}) -> Out.
node_code(#node{code = Out}) -> Out.
node_non_struct_defs(#node{non_struct_defs = Out}) -> Out.
node_up_expr(#node{up_expr = Out}) -> Out.
node_pre_loop(#node{pre_loop = Out}) -> Out.
node_struct_type(#node{struct_type = Out}) -> Out.
%% node_atom_type(#node{atom_type = Out}) -> Out.
node_struct_elems(#node{struct_elems = Out}) -> Out.

node_pre_loop_set(Node) -> Node#node{pre_loop = true}.

node_phi_add(Node = #node{phi = Phi}, Pred, Value) ->
  NewList = 
    case gb_trees:lookup(Pred, Phi) of
      {value, List} -> [Value | List];
      none -> [Value]
    end,
  Node#node{phi = gb_trees:enter(Pred, NewList, Phi)}.

node_phi_get(#node{phi = Phi}, Pred) ->
  case gb_trees:lookup(Pred, Phi) of
    {value, List} -> List;
    none -> []
  end.

node_code_add(Node = #node{code = Code}, Instr) ->
  Node#node{code = [Instr | Code]}.

node_code_rev(Node = #node{code = Code}) ->
  Node#node{code = lists:reverse(Code)}.

node_struct_type_add(Node = #node{struct_type = T}, Value) ->
  Node#node{struct_type = [Value | T]}.

%% node_atom_type_add(Node = #node{atom_type = T}, Value) ->
%%   Node#node{atom_type = [Value | T]}.

node_struct_elems_add(Node = #node{struct_elems = T}, Value) ->
  Node#node{struct_elems = [Value | T]}.

node_non_struct_defs_list(Node) -> 
  gb_sets:to_list(node_non_struct_defs(Node)).

node_non_struct_instr_add(Node, Instr) -> 
  DefList = hipe_icode:defines(Instr),
  Tmp = gb_sets:union(node_non_struct_defs(Node), gb_sets:from_list(DefList)),
  Node#node{non_struct_defs = Tmp}.

node_set_sub_inserts(Node, In) -> Node#node{sub_inserts = In}.

node_add_insert(Node, In) -> 
  NewIns = ?SETS:add_element(In, node_inserts(Node)),
  Node#node{inserts = NewIns}.

node_union_sub_inserts(Node, SubIns) -> 
  NewSubIns = ?SETS:union(SubIns, node_sub_inserts(Node)),
  node_set_sub_inserts(Node, NewSubIns).

node_varmap_set(Node, Vars) ->
  Node#node{varmap = Vars}.

node_varmap_lookup(#node{varmap = Varmap}, Var) ->
  case lists:keyfind(Var, 1, Varmap) of
    {_, NewVar} -> NewVar;
    false -> Var
  end.
 
node_create(Label, Pred, Succ) ->
  #node{label = Label, pred = Pred, succ = Succ}.

%%-----------------------------------------------------------------------------
%% nodes - describes the new temporary CFG
%%
%% domtree - the dominator tree of the original CFG
%% labels - the labels of the original CFG, filtered to only contain non fail trace paths
%% postorder - the postorder walk of labels of the original CFG, filtered to only contain non fail trace paths
%% rev_postorder - reverse of postorder.
%% start_label - the start basic block label.
%% all_expr - all expression value numbers that the CFG defines
%% tree - the tree of nodes, with labels as keys and node records as values

-record(nodes, {
  domtree	                   :: hipe_dominators:domTree(),
  labels 	= none             :: 'none' | [icode_lbl()],
  postorder 	= none             :: 'none' | [icode_lbl()],            
  start_label	= none             :: 'none' | icode_lbl(),
  rev_postorder = none             :: 'none' | [icode_lbl()],
  all_expr	= none             :: 'none' | [non_neg_integer()],
  tree  	= gb_trees:empty() :: gb_tree()}).

nodes_postorder(#nodes{postorder = Out}) -> Out.
nodes_rev_postorder(#nodes{rev_postorder = Out}) -> Out.
nodes_tree(#nodes{tree = Out}) -> Out.
nodes_domtree(#nodes{domtree = Out}) -> Out.
nodes_start_label(#nodes{start_label = Out}) -> Out.

nodes_tree_is_empty(#nodes{tree = Tree}) ->
  gb_trees:is_empty(Tree).

nodes_tree_set(Tree, Nodes) -> Nodes#nodes{tree = Tree}.
nodes_all_expr_set(AllExpr, Nodes) -> Nodes#nodes{all_expr = AllExpr}.

nodes_tree_values(Nodes) -> 
  gb_trees:values(nodes_tree(Nodes)).

get_node(Label, Nodes) ->
  gb_trees:get(Label, nodes_tree(Nodes)).

enter_node(Node, Nodes) ->
  nodes_tree_set(gb_trees:enter(node_label(Node), Node, nodes_tree(Nodes)), Nodes).

remove_node(Node, Nodes) ->
  nodes_tree_set(gb_trees:delete(node_label(Node), nodes_tree(Nodes)), Nodes).

nodes_create() -> #nodes{}.

%%-----------------------------------------------------------------------------
%% update
%% record used when updating the CFG, keeping track of which expressions
%% have been inserted and their mappings to variable names.
%%
%% inserted - maps an expression to a list of variables
%% del_red_test - flag that is set to true when the reduction test
%% 	has been inserted is used to move the reduction test.

-record(update, {inserted     = gb_trees:empty() :: gb_tree(),
		 del_red_test = false            :: boolean()}).

update_inserted_lookup(#update{inserted = Inserted}, ExprId) ->
  gb_trees:lookup(ExprId, Inserted).

update_inserted_add_new(Update = #update{inserted = Inserted}, ExprId, Defs) ->
  VarList = [case hipe_icode:is_var(Def) of
	       true -> hipe_icode:mk_new_var();
	       false ->
		 case hipe_icode:is_reg(Def) of
		   true -> hipe_icode:mk_new_reg();
		   false ->
		     true = hipe_icode:is_fvar(Def),
		     hipe_icode:mk_new_fvar()
		 end
	     end || Def <- Defs],
  NewInserted = gb_trees:enter(ExprId, VarList, Inserted),
  {Update#update{inserted = NewInserted}, VarList}.

update_inserted_add(Update = #update{inserted = Inserted}, ExprId, Defs) ->
  Update#update{inserted = gb_trees:enter(ExprId, Defs, Inserted)}.

update_del_red_test(#update{del_red_test = DelRed}) -> DelRed.
update_del_red_test_set(Update) -> 
  Update#update{del_red_test = true}.

%%-----------------------------------------------------------------------------
%% CODE AREA

%%-----------------------------------------------------------------------------
%% Main function called from the hipe_main module

-spec struct_reuse(#cfg{}) -> #cfg{}.

struct_reuse(CFG) ->
  %% debug_init_case_count(?SR_INSTR_TYPE),
  %% debug_init_case_count(?SR_STRUCT_INSTR_TYPE),

  %% debug_function({wings_ask,ask_unzip,3}, CFG),
  %% debug_function(nil, CFG),
  %% set_debug_flag(true),
  %% debug_struct("CFG In: ", CFG),
  %% debug_cfg_pp(CFG),

  init_expr_id(),

  Nodes = construct_nodes(CFG),

  case nodes_tree_is_empty(Nodes) of
    false ->
      Maps = create_maps(Nodes),

      Nodes3 = init_nodes(Nodes, Maps),
      Nodes4 = calc_anticipated(Nodes3),

      {Nodes5, Maps3} = calc_inserts(Nodes4, Maps),

      Nodes6 = update_nodes_inserts(Nodes5, Maps3),

      %% debug_list("ExprMap: ", gb_trees:to_list(Maps3#maps.expr)),
      %% debug_list("VarMap: ", gb_trees:to_list(maps_var(Maps3))),
      %% debug_nodes(Nodes6),

      %% update the cfg
      CFG1 = rewrite_cfg(CFG, Nodes6, Maps3),
      CFG2 = hipe_icode_ssa:remove_dead_code(CFG1),
      CFGOut = hipe_icode_ssa_copy_prop:cfg(CFG2),
      %% CFGOut = CFG1,

      %% print_struct("CFG: ", CFG),
      %% debug_cfg_pp(CFG),
      %% debug_cfg_pp(CFGOut),

      %% debug_print_case_count(?SR_STRUCT_INSTR_TYPE),
      %% debug_print_case_count(?SR_INSTR_TYPE),
      %% debug("Done~n"),
      %% debug_struct("CFG Out: ", CFGOut),
      CFGOut;
    true ->
      CFG
  end.

%%-----------------------------------------------------------------------------
%% Calculate simplified CFG with all fail paths removed

construct_nodes(CFG) ->
  %% all important dominator tree
  DomTree = hipe_dominators:domTree_create(CFG),

  %% construct initial nodes
  {Nodes, NonFailSet} = nodes_from_cfg(CFG, DomTree),

  %% remove nodes on fail paths
  NewNodes = prune_nodes(Nodes, NonFailSet),

  %% fill in misc node tree info
  Postorder = [Label || Label <- hipe_icode_cfg:postorder(CFG),
			gb_sets:is_member(Label, NonFailSet)],

  %% check postorder is valid
  PostOrderTmp = hipe_icode_cfg:postorder(CFG),
  LabelsTmp = hipe_icode_cfg:labels(CFG),
  case length(PostOrderTmp) =/= length(LabelsTmp) of
    true ->
      print("Warning, Postorder and Labels differ!~n"),
      print_struct("Postorder: ", PostOrderTmp),
      print_struct("Labels: ", LabelsTmp);
    false ->
      done
  end,

  RevPostorder = lists:reverse(Postorder),

  StartLabel = hipe_icode_cfg:start_label(CFG),
  NewTree = gb_trees:balance(nodes_tree(NewNodes)),
  
  NewNodes#nodes{postorder = Postorder, 
		 rev_postorder = RevPostorder,
		 start_label = StartLabel,
		 tree = NewTree, 
		 domtree = DomTree}.

%%-----------------------------------------------------------------------------
%% Constructs a tree of nodes, one node for each basic block in CFG

nodes_from_cfg(CFG, DomTree) ->
  lists:foldl(fun(Label, {NodesAcc, NonFailAcc}) -> 
    Code = hipe_bb:code(hipe_icode_cfg:bb(CFG, Label)),
    Pred = hipe_icode_cfg:pred(CFG, Label),
    Succ = hipe_icode_cfg:succ(CFG, Label),
    %% debug_struct("Label: ", Label),
    %% debug_struct("Code: ", Code),
		  
    %% Find all structures and phi functions.
    %% Find all defines in this bb that are not from structures
    %% and add them to NonStructDefs, later to be used for calculating upwards
    %% exposed expressions, and killed expressions.
    %% Also find all non fail blocks, ie backtrace from return blocks,
    %% and add them to NewNonFailAcc

    Node = node_create(Label, Pred, Succ),

    {NewNode, NewNonFailAcc, PreLoopPreds} = 
      lists:foldl(fun(Instr, {NodeAcc, NFAcc, PLPAcc}) ->
	case instr_type(Instr) of
	  struct -> 
	    {node_code_add(NodeAcc, Instr), NFAcc, PLPAcc};
	  return -> 
	    {NodeAcc, get_back_trace_rec(CFG, Label, NFAcc), PLPAcc};
	  {struct_elems, NumElem, DstVar, SrcVar} ->
	    NewNodeAcc = node_struct_elems_add(NodeAcc, {DstVar, NumElem, SrcVar}),
	    {node_non_struct_instr_add(NewNodeAcc, Instr), NFAcc, PLPAcc};
	  {struct_type, NumElems, Var, Type} ->
	    {node_struct_type_add(NodeAcc, {Type, Var, NumElems}), NFAcc, PLPAcc};
	  {tuple_arity, Var, Cases} ->
	    NewNodeAcc = 
	      lists:foldl(fun(Case, NAcc) ->
			      case Case of
				{{const, {flat, Arity}}, _} ->
				  Tuple = {?MKTUPLE, Var, Arity},
				  node_struct_type_add(NAcc, Tuple);
				_ -> NAcc
			      end
			  end, NodeAcc, Cases),
	    {NewNodeAcc, NFAcc, PLPAcc};
	  %% {atom_type, Atom, Var} ->
	  %%   {node_atom_type_add(NodeAcc, {Var, Atom}), NFAcc, PLPAcc};
	  phi ->
	    Def = hipe_icode:phi_dst(Instr),
	    Part = lists:foldl(fun(P = {Pr, PredVar}, {IsDef, NotDom}) -> 
	      case hipe_dominators:domTree_dominates(Label, Pr, DomTree) of
		false ->
		  {IsDef, [P | NotDom]};
		true ->
		  {IsDef andalso PredVar =:= Def, NotDom}
	      end
	    end, {true, []}, hipe_icode:phi_arglist(Instr)),

	    case Part of
	      {true, [{P, V}]} -> 
		%% This is the only case recognized so far. All phi
		%% sub block references a static variable that is
		%% assigned the same value again in the phi function.
		{node_phi_add(NodeAcc, P, {Def, V}),
		 NFAcc, ?SETS:add_element(P, PLPAcc)};

	      {false, [{P, _}]} -> 
		{node_non_struct_instr_add(NodeAcc, Instr),
		 NFAcc, ?SETS:add_element(P, PLPAcc)};

	      _ -> 
		{node_non_struct_instr_add(NodeAcc, Instr), NFAcc, PLPAcc}
	    end;
	  _ ->
	    {node_non_struct_instr_add(NodeAcc, Instr), NFAcc, PLPAcc}
	end
      end, {Node, NonFailAcc, ?SETS:new()}, Code),

      %% insert the new node
      NewNodesAcc = enter_node(node_code_rev(NewNode), NodesAcc),

      %% Set the pre loop flag of all nodes that are predecessor to this node
      %% and that are the first nodes prior to a loop.
      NewNodesAcc2 = 
	lists:foldl(fun(Lbl, NsAcc) ->
	  PredNode = get_node(Lbl, NsAcc),
	  NewPredNode = node_pre_loop_set(PredNode),
	  NewPredNode2 = node_varmap_set(NewPredNode, node_phi_get(NewNode, Lbl)),

	  enter_node(NewPredNode2, NsAcc)
	end, NewNodesAcc, PreLoopPreds),

      {NewNodesAcc2, NewNonFailAcc}
  end, {nodes_create(), gb_sets:new()}, hipe_icode_cfg:reverse_postorder(CFG)).

%%-----------------------------------------------------------------------------
%% Get all labels from Label to root of CFG, ie backtraces from Label.

get_back_trace_rec(CFG, Label, LabelSet) -> 
  %% debug_struct("Label :", Label),
  %% debug_struct("Set :", gb_sets:to_list(LabelSet)),
  case gb_sets:is_member(Label, LabelSet) of 
    false ->
      Preds = hipe_icode_cfg:pred(CFG, Label),
      lists:foldl(fun(Lbl, SetAcc) ->
		      get_back_trace_rec(CFG, Lbl, SetAcc) 
		  end, gb_sets:add(Label, LabelSet), Preds);
    true -> LabelSet
  end.

%%-----------------------------------------------------------------------------
%% Remove all fail block paths and successors and predecessors
%% That are on fail paths

prune_nodes(Nodes, NonFailSet) ->
  lists:foldl(fun(Node, NodesAcc) -> 
   case gb_sets:is_member(node_label(Node), NonFailSet) of
      true ->
	NewSucc = [L || L <- node_succ(Node), gb_sets:is_member(L, NonFailSet)],
	NewPred = [L || L <- node_pred(Node), gb_sets:is_member(L, NonFailSet)],
	enter_node(Node#node{succ = NewSucc, pred = NewPred}, NodesAcc);
      false ->
	remove_node(Node, NodesAcc)
    end
  end, Nodes, nodes_tree_values(Nodes)).

%%-----------------------------------------------------------------------------
%% Map calculations. 

%%-----------------------------------------------------------------------------
%% Create a maps structure from the Nodes record

create_maps(Nodes) ->
  Maps = lists:foldl(fun(Label, MapsAcc) ->
    Node = get_node(Label, Nodes),
    NewMapsAcc = maps_from_node_struct_type(MapsAcc, Node),
    NewMapsAcc2 = maps_from_node_struct_elems(NewMapsAcc, Node),
    %% NewMapsAcc3 = maps_from_node_atom_type(NewMapsAcc2, Node),
    maps_from_node_code(NewMapsAcc2, Node)
  end, #maps{}, nodes_rev_postorder(Nodes)),
  maps_balance(Maps).

%%-----------------------------------------------------------------------------
%% Add all elements in the struct_type list of Node to Maps as expressions

maps_from_node_struct_type(Maps, Node) ->
  %% debug_struct("Node Label: ", node_label(Node)),
  %% debug_struct("Node Tuple Type: ", node_struct_type(Node)),
  lists:foldl(fun({Type, Var, Size}, MapsAcc) ->
		  Key = create_elem_expr_key(Size, Var, []),
		  InstrKey = hipe_icode:mk_primop([], Type, Key),
		  NewExpr2 = expr_create(InstrKey, [Var]),
		  NewExpr3 = expr_direct_replace_set(NewExpr2, true),
		  maps_expr_key_enter(NewExpr3, MapsAcc)
	      end, Maps, node_struct_type(Node)).

create_elem_expr_key(0, _, Key) -> Key;
create_elem_expr_key(N, Var, Key) -> 
  create_elem_expr_key(N - 1, Var, [{Var, N} | Key]).

%%-----------------------------------------------------------------------------
%%maps_from_node_atom_type(Maps, Node) ->
%%  lists:foldl(fun({Var, Atom}, MapsAcc) ->
%%    case maps_var_lookup(Var, MapsAcc) of
%%      none ->
%%	MapsAcc;
%%      {value, #varinfo{elem = none}} -> 
%%	MapsAcc;
%%      {value, #varinfo{elem = {Src, Num, ExprId}}} -> 
%%	Expr = maps_expr_get(ExprId, MapsAcc),
%%	Key = expr_key(Expr),
%%
%%	Filter = fun(Arg) -> 
%%	  case Arg of
%%	    {Src, Num, ExprId} -> 
%%	      hipe_icode:mk_const(Atom);
%%	    _ -> 
%%	      Arg
%%	  end end,
%%
%%	NewKey = replace_call_variables(Filter, Key),
%%	NewExpr = expr_create(NewKey, expr_defs(Expr)),
%%	maps_expr_key_enter(NewExpr, MapsAcc)
%%    end
%%  end, Maps, node_atom_type(Node)).

%%-----------------------------------------------------------------------------
%% Add all struct_elemns in Node to Maps as variables

maps_from_node_struct_elems(Maps, Node) ->
  lists:foldl(fun({Dst, Num, Src}, MapsAcc) ->
		  VarInfo = #varinfo{elem = {Src, Num}},
		  maps_var_insert(Dst, VarInfo, MapsAcc)
	      end, Maps, node_struct_elems(Node)).

%%-----------------------------------------------------------------------------
%% Get all expressions defined by the Node and insert them into Maps.
%% Also insert information about all affected variables into Maps.

maps_from_node_code(Maps, Node) ->
  %% debug_struct("Node Label: ", Label),
  %% debug_struct("Node Code: ", Code),
  %% Label = node_label(Node),
  lists:foldl(fun(Instr, MapsAcc) ->
    %% create two keys that are used to reference this structure creation
    %% instruction, so that we can lookup its expression value number
    %% later.
    InstrKey = hipe_icode:call_dstlist_update(Instr, []),

    %% Fetch the two keys from the instruction
    {HasElems, RefKey, ElemKey} = 
      replace_call_vars_elems(MapsAcc, InstrKey),

    %% create a new expr record or lookup an existing one.
    case HasElems of 
      true ->
	%% The instruction contains uses of variables that are
	%% part of another structure.
	case maps_instr_lookup(ElemKey, MapsAcc) of
	  {value, ExprId} -> 
	    %% The instruction is equal to a structure that has
	    %% already been created. This is the f({Z}) -> {Z}
	    %% optimization. I.e. there is no need to create {Z} again.
	    %% Also lookup if ExprId is defining a variable that is
	    %% already an element in another structure. If so,
	    %% use that element. This takes care of nested structures
	    %% such as f({X, {Y, Z}}) -> {X, {Y, Z}}.

	    #expr{defs = [Var]} = maps_expr_get(ExprId, MapsAcc),
	    StructElem = 
	      case maps_var_lookup(Var, MapsAcc) of
		{value, #varinfo{elem = Elem, exprid = none}} when Elem =/= none ->
		  Elem;
		_ -> none
	      end,
	    Defines = hipe_icode:defines(Instr),
	    maps_varinfos_create(Defines, ExprId, StructElem, MapsAcc);
	  none ->
	    %% create a new expression
	    maps_expr_varinfos_create(Instr, RefKey, MapsAcc)	
	end;
      false ->
	%% create a new expression
	maps_expr_varinfos_create(Instr, RefKey, MapsAcc)
    end
  end, Maps, node_code(Node)).

%%-----------------------------------------------------------------------------
%% Creates varinfo structures with exprid set to ExprId for all
%% variables contained in Defines. These are put into MapsIn.

maps_varinfos_create(Defines, ExprId, Elem, MapsIn) ->
  VarInfo = #varinfo{exprid = ExprId, elem = Elem},
  {MapsOut, _} = 
    lists:foldl(fun (Def, {Maps, NumAcc}) -> 
		    NewVarInfo = VarInfo#varinfo{ref = {ExprId, NumAcc}},
		    {maps_var_insert(Def, NewVarInfo, Maps), NumAcc + 1}
		end, {MapsIn, 1}, Defines),
  MapsOut.

%%-----------------------------------------------------------------------------
%% Creates a new expression from RefKey if RefKey is not already reffering
%% to an expression. Also creates varinfo structures for all variables defined
%% and used by Instr. Result is put in Maps.

maps_expr_varinfos_create(Instr, RefKey, Maps) ->
  Defines = hipe_icode:defines(Instr),
  {ExprId, Maps2} =
    case maps_instr_lookup(RefKey, Maps) of
      {value, EId} -> 
	{EId, Maps};
      none ->
	NewExpr = expr_create(RefKey, Defines),
	{expr_id(NewExpr), maps_expr_key_enter(NewExpr, Maps)}
    end,
  Maps3 = maps_varinfos_create(Defines, ExprId, none, Maps2),
  update_maps_var_use(Instr, ExprId, Maps3).

%%-----------------------------------------------------------------------------
%% A variable replacement function that returns a tuple of three elements
%% {T, K1, K2}, where T indicates if Instr contained variables that where
%% elements of other structures, K1 is the Instr with all variables that
%% references another structure replaced, and K2 is K1 but also with all
%% variables that are elements of other structures replaced.

replace_call_vars_elems(Maps, Instr) ->
  VarMap = maps_var(Maps),
  {HasElems, Vars, Elems} = 
    lists:foldr(fun(Arg, {HasElems, Vars, Elems}) -> 
      case hipe_icode:is_const(Arg) of
	false -> 
	  case gb_trees:lookup(Arg, VarMap) of
	    none ->
	      {HasElems, [Arg | Vars], [Arg | Elems]};
	    {value, #varinfo{ref = none, elem = none}} -> 
	      {HasElems, [Arg | Vars], [Arg | Elems]};
	    {value, #varinfo{ref = Ref, elem = none}} -> 
	      {HasElems, [Ref | Vars], [Ref | Elems]};
	    {value, #varinfo{ref = none, elem = Elem}} -> 
	      {true, [Arg | Vars], [Elem | Elems]};
	    {value, #varinfo{ref = Ref, elem = Elem}} -> 
	      {true, [Ref | Vars], [Elem | Elems]}
	  end;
	true ->
	  {HasElems, [Arg | Vars], [Arg | Elems]}
      end end, {false, [], []}, hipe_icode:args(Instr)),
  {HasElems, hipe_icode:call_args_update(Instr, Vars), 
  hipe_icode:call_args_update(Instr, Elems)}.

%%-----------------------------------------------------------------------------
%% Updates the usage information of all variables used by Instr to also
%% contain Id and updates Maps to contain the new variable information.
%% Also updates the expressions where the updated variables are used to
%% contain the use information.

update_maps_var_use(Instr, Id, Maps) ->
  lists:foldl(fun(Use, MapsAcc) ->
		  VarInfo = get_varinfo(Use, MapsAcc),
		  NewVarInfo = varinfo_use_add(VarInfo, Id),
		  MapsAcc2 = maps_var_enter(Use, NewVarInfo, MapsAcc),
		  case varinfo_exprid(VarInfo) of
		    none ->
		      MapsAcc2;
		    VarExprId -> 
		      Expr = maps_expr_get(VarExprId, MapsAcc2),
		      NewExpr = expr_use_add(Expr, Id),
		      maps_expr_enter(NewExpr, MapsAcc2)
		  end
	      end, Maps, hipe_icode:uses(Instr)).

%%-----------------------------------------------------------------------------
%% Looks up an old variable info or creates a new one if none is found.

get_varinfo(Var, Maps) ->
  case maps_var_lookup(Var, Maps) of
    {value, Info} -> 
      Info;
    none -> 
      #varinfo{}
  end.

%%-----------------------------------------------------------------------------
%% filters all arguments to a function call Instr that are not constants
%% through the Filter function, and replaces the arguments in Instr with
%% the result.

replace_call_variables(Filter, Instr) ->
  NewArgs = [case hipe_icode:is_const(Arg) of
	       false -> Filter(Arg);
	       true -> Arg
	     end || Arg <- hipe_icode:args(Instr)],
  hipe_icode:call_args_update(Instr, NewArgs).

%%-----------------------------------------------------------------------------
%% Init nodes from node local expression information

init_nodes(Nodes, Maps) ->
  AllExpr = maps_expr_keys(Maps),
  lists:foldl(fun(Node, NodesAcc) -> 
    UEExpr = calc_up_exposed_expr(maps_var(Maps), Node),
    %% print_list("Up ExprSet: ", ?SETS:to_list(UEExpr)),

    KilledExpr = calc_killed_expr(Node, Maps),
    %% print_list("Killed: ", ?SETS:to_list(KilledExpr)),

    %% End nodes have no anticipated out
    AnticOut =
      case node_succ(Node) of
	[] ->
	  ?SETS:new();
	_ ->
	  AllExpr
      end,
    enter_node(Node#node{up_expr = UEExpr,
			 killed_expr = KilledExpr,
			 antic_out = AnticOut}, NodesAcc)
  end, nodes_all_expr_set(AllExpr, Nodes), nodes_tree_values(Nodes)).

%%-----------------------------------------------------------------------------
%% Calculate the upwards exposed expressions for a node.

calc_up_exposed_expr(VarMap, Node) ->
  %% debug_struct("UpExpr label: ", node_label(Node)),
  NonStructDefs = node_non_struct_defs(Node),
  {_, ExprIdSet} = 
    lists:foldl(fun(Instr, {NotToUseAcc, ExprIdAcc}) ->
      Defs = hipe_icode:defines(Instr),
      Uses = hipe_icode:uses(Instr),
      IsNotToUse = 
	lists:any(fun(Use) -> gb_sets:is_member(Use, NotToUseAcc) end, Uses),
	case IsNotToUse of
	  false ->
	    NewExprIdAcc = 
	      lists:foldl(fun(Def, Acc) ->
		#varinfo{exprid = Id} = gb_trees:get(Def, VarMap),
		?SETS:add_element(Id, Acc) end, ExprIdAcc, Defs),
	    {NotToUseAcc, NewExprIdAcc};
	  true ->
	    NewNotToUse =
	      gb_sets:union(gb_sets:from_list(Defs), NotToUseAcc),
	    {NewNotToUse, ExprIdAcc}
	end
    end, {NonStructDefs, ?SETS:new()}, node_code(Node)),
    ExprIdSet.

%%-----------------------------------------------------------------------------
%% Calculate killed expression for node

calc_killed_expr(Node, Maps) ->
  calc_killed_expr_defs(node_non_struct_defs_list(Node), ?SETS:new(), Maps).

calc_killed_expr_defs(Defs, UseSet, Maps) ->
  lists:foldl(fun(Def, Acc) -> 
		  case maps_var_lookup(Def, Maps) of
		    none ->
		      Acc;
		    {value, #varinfo{use = Use}} ->
		      ?SETS:union(Acc, calc_killed_expr_use(Use, Maps))
		  end
	      end, UseSet, Defs).

calc_killed_expr_use(ExprIds, Maps) ->
  ?SETS:fold(fun(Id, Acc) -> 
		 Expr = maps_expr_get(Id, Maps),
		 ?SETS:union(Acc, calc_killed_expr_use(expr_use(Expr), Maps))
	     end, ExprIds, ExprIds).

%%-----------------------------------------------------------------------------
%% Calculate the anticipated in and anticipated out sets for each node

calc_anticipated(NodesIn) ->
  calc_anticipated_rec(NodesIn, nodes_postorder(NodesIn)).

calc_anticipated_rec(NodesIn, []) -> NodesIn;
calc_anticipated_rec(NodesIn, WorkIn) ->
  {NodesOut, WorkOut} = 
  lists:foldl(fun(Label, {NodesAcc, WorkAcc}) ->
    Node = get_node(Label, NodesAcc),

    %debug_struct("~nNode Label: ", Label),

    AnticIn = ?SETS:union(node_up_expr(Node), 
      ?SETS:subtract(node_antic_out(Node), node_killed_expr(Node))),

    %debug_struct("AnticIn: ", AnticIn),
    case (node_antic_in(Node) =:= AnticIn) of
      false ->
	NewNodes1 = enter_node(Node#node{antic_in = AnticIn}, NodesAcc),
	Preds = node_pred(Node),
	%debug_struct("Preds: ", Preds),

	NewNodes2 = 
	lists:foldl(fun(Label2, NodesAcc2) ->
	  PredNode = get_node(Label2, NodesAcc2),
	  AnticOut = ?SETS:intersection(AnticIn, node_antic_out(PredNode)),
	  %debug_struct("Pred Node Label: ", Label2),
	  %debug_struct("Pred AnticOut: ", AnticOut),
	  
	  enter_node(PredNode#node{antic_out = AnticOut}, NodesAcc2) 
	end, NewNodes1, Preds),

	NewWork = add_work_list(Preds, WorkAcc),
	%debug_struct("New Work: ", NewWork),

	{NewNodes2, NewWork};
      true ->
	{NodesAcc, WorkAcc}
    end
    end, {NodesIn, new_work()}, WorkIn),

  calc_anticipated_rec(NodesOut, get_work_list(WorkOut)).

%%-----------------------------------------------------------------------------
%% Function that adds inserts to expressions from nodes which either
%% have an upwards exposed expression or dominate more than one node
%% that inserts the same expression or the node is a prior to loop
%% node. The inserted info is stored in the #expr records in the expr
%% map of the #maps structure.

calc_inserts(NodesIn, MapsIn) ->
  DomTree = nodes_domtree(NodesIn),

  lists:foldl(fun(Label, {NodesAcc, MapsAcc}) ->
    Node = get_node(Label, NodesAcc),

    %% get some basic properties.
    UpExpr = node_up_expr(Node), 
    AnticOut = node_antic_out(Node),
    SubIns = node_sub_inserts(Node),

    %% debug_struct("Label: ", Label),

    {HasIns, NewMapsAcc} = 
      ?SETS:fold(fun(ExprId, {HasInsAcc, MapsAcc2}) ->
	Expr = maps_expr_get(ExprId, MapsAcc2),

	ExprIns = expr_inserts(Expr),
	ExprSubIns = ?SETS:intersection(ExprIns, SubIns),

	%% There are three cases when to insert an expression
	%% 1. The expression is defined at least twice in the subtree of this
	%%    node, that is length(ExprSubIns) > 1.
	%% 2. It is defined in the node and is upwards exposed.
	%% 3. The node is a block just above a loop, so we should move
	%%    all anticipated expressions to the node.
	
	case length(ExprSubIns) > 1 orelse ?SETS:is_element(ExprId, UpExpr) 
	  orelse node_pre_loop(Node) of
	  true ->
	    %% get labels of all sub blocks that inserts the expression and
	    %% that are dominated by the current node.
	    Dominates = 
	      ?SETS:filter(fun(SubLabel) ->
		hipe_dominators:domTree_dominates(Label, SubLabel, DomTree) 
	      end, ExprSubIns),

	    %% remove inserts labels from insert labelset.
	    NewIns = ?SETS:subtract(ExprIns, Dominates),
	    NewIns2 = ?SETS:add_element(Label, NewIns),

	    %% update the node.
	    NewMaps =
	      maps_expr_enter(expr_inserts_set(Expr, NewIns2), MapsAcc2),
	    {true, NewMaps};
	  false ->
	    {HasInsAcc, MapsAcc2}
	end
      end, {false, MapsAcc}, ?SETS:union(AnticOut, UpExpr)),
    
    %% Check if there was an insert into this node,
    %% and if so add to the sub inserts set.
    NewSubIns = 
      case HasIns of
	true ->
	  ?SETS:add_element(Label, SubIns);
	false ->
	  SubIns
      end,
    
    %% update sub inserts for all predecessors to the node.
    NewNodes2 = 
      lists:foldl(fun(PredLabel, NodesAcc2) ->
	PredNode = get_node(PredLabel, NodesAcc2),	
	enter_node(node_union_sub_inserts(PredNode, NewSubIns), NodesAcc2) 
      end, NodesAcc, node_pred(Node)),

    {NewNodes2, NewMapsAcc}

  end, {NodesIn, MapsIn}, nodes_postorder(NodesIn)).

%%-----------------------------------------------------------------------------
%% Update the insert sets of each node in the node tree.
%% That is, move the insert information from the expressions to
%% the actual nodes that perform the inserts.

update_nodes_inserts(Nodes, Maps) ->
  lists:foldl(fun(Expr, NodesAcc) ->
		  ExprId = expr_id(Expr),
		  ?SETS:fold(fun(Label, NsAcc) ->
				 Nd = get_node(Label, NsAcc),
				 enter_node(node_add_insert(Nd, ExprId), NsAcc)
			     end, NodesAcc, expr_inserts(Expr))
	      end, Nodes, maps_expr_values(Maps)).

%%-----------------------------------------------------------------------------
%% Rewrite CFG functions

%%-----------------------------------------------------------------------------
%% Do the code updating from the info in the nodes and maps structures. This
%% is a proxy function for rewrite_cfg/6
rewrite_cfg(CFG, Nodes, Maps) ->
  {NewCFG, _Visited} =
    rewrite_cfg(CFG, ?SETS:new(), #update{}, Nodes, Maps, [nodes_start_label(Nodes)]),
  %% debug_struct("Visited: ", _Visited),
  NewCFG.

%%-----------------------------------------------------------------------------
%% rewrite_cfg
%% traverse the CFG in reverse postorder and rewrite each basic block before
%% rewriteing its children. Pass along to each BB update the mappings of
%% inserted expressions in the Update record.

rewrite_cfg(CFG, Visited, Update, Nodes, Maps, Labels) ->
  lists:foldl(fun(Label, {CFGAcc, VisitedAcc}) -> 
    case ?SETS:is_element(Label, VisitedAcc) of
      false ->
	%% debug_struct("Visit: ", Label),
	Node = get_node(Label, Nodes),
	NewVisitedAcc = ?SETS:add_element(Label, VisitedAcc),
	{NewCFGAcc, NewUpdate} = rewrite_bb(CFGAcc, Update, Maps, Node),
	%% debug_struct("Update inserted: ", update_inserted_list(NewUpdate)),
	rewrite_cfg(NewCFGAcc, NewVisitedAcc, NewUpdate, Nodes, Maps, node_succ(Node));
      true ->
	{CFGAcc, VisitedAcc}
    end
  end, {CFG, Visited}, Labels).

%%-----------------------------------------------------------------------------
%% rewrite one single basic block in the CFG as described by the properties
%% in the Node for that block. Uses the Maps and Update info to lookup
%% the instructions and expressions to insert or delete.

rewrite_bb(CFG, Update, Maps, Node) ->
  #node{pre_loop = PreLoop, label = Label, up_expr = UpExpr, inserts = Inserts} = Node, 

  Code = hipe_bb:code(hipe_icode_cfg:bb(CFG, Label)),

  %debug_struct("RW Label: ", Label),
  %debug_struct("Inserts", Inserts),

  DelRed = update_del_red_test(Update),
  Delete = ?SETS:subtract(UpExpr, Inserts),

  %% local function that gets the instruction and defines list of an
  %% expression id in the current node and and returns them.
  GetInstrFunc = fun(Expr) ->
		     Instr = expr_key(Expr),
		     Defs = expr_defs(Expr),
		     NewInstr = 
		       if
			 PreLoop ->
			   replace_call_variables(fun(Var) ->
						      node_varmap_lookup(Node,
									 Var)
						  end,
						  Instr);
			 true ->
			   Instr
		       end,
		     {NewInstr, Defs}
		 end,

  %% go through all expressions defined by the node and replace
  %% or remove them as indicated by the delete set. Also perform
  %% reduction test replacement if neccessary.
  {[CodeLast | CodeRest], NewUpdate, LocalAcc} = 
    lists:foldl(fun(Instr, {CodeAcc,  UpdateAcc, LocalAcc}) ->
      case struct_instr_type(Instr) of
	struct ->
	  Defs = hipe_icode:defines(Instr),

	  #varinfo{exprid = ExprId} = maps_var_get(hd(Defs), Maps),

	  Expr = maps_expr_get(ExprId, Maps),
	  DirectReplace = expr_direct_replace(Expr),

	  %% Creates move intstructions from Vars to Defs
	  RemoveFuncVars = fun(Vars) ->
	    CodeAcc2 = mk_defs_moves(CodeAcc, Defs, Vars),
	    {CodeAcc2, UpdateAcc, LocalAcc} end,

	  %% Looks up an already inserted ExprId and makes moves
	  %% of variables from that expression to this expression.
	  RemoveFunc = fun() ->
	    {value, Vars} = update_inserted_lookup(UpdateAcc, ExprId),
	    RemoveFuncVars(Vars) end,

	  %% Is ExprId already inserted?
	  IsLocal = ?SETS:is_element(ExprId, LocalAcc),

	  case DirectReplace of
	    true -> 
	      %% The Instr is reffering to an expression that is 
	      %% defined as an identical already present instruction, 
	      %% and can be removed directly.
	      RemoveFuncVars(expr_defs(Expr));
	    false when IsLocal ->
	      %% The instruction has already been inserted.
	      RemoveFunc();
	    _ ->
	      case ?SETS:is_element(ExprId, Delete) of
		true ->
		  %% should not be inserted
		  RemoveFunc();
		_ ->
		  %% Should remain
		  UpdateAcc2 = update_inserted_add(UpdateAcc, ExprId, Defs),
		  LocalAcc2 = ?SETS:add_element(ExprId, LocalAcc),
		  {[Instr | CodeAcc], UpdateAcc2, LocalAcc2}
	      end
	  end;
	redtest when DelRed ->
	  %% delete reduction test
	  {CodeAcc, UpdateAcc, LocalAcc};
	_ ->
	  {[Instr | CodeAcc], UpdateAcc, LocalAcc}
      end
    end, {[], Update, ?SETS:new()}, Code),

  %debug_struct("RW Label 2: ", Label),

  %% calculate the inserts that are new to this node, that is
  %% the expressions that are in Inserts but not in UpExpr,
  %% and that have not been added already,
  %% that is not present in LocalAcc
  NewInserts = ?SETS:subtract(?SETS:subtract(Inserts, UpExpr), LocalAcc),

  {NewCodeRest, NewUpdate2} = 
    ?SETS:fold(fun(ExprId, {CodeAcc, UpdateAcc}) ->
      Expr = maps_expr_get(ExprId, Maps),
      {ExprInstr, Defs} = GetInstrFunc(Expr),
      {UpdateAcc2, NewDefs} = update_inserted_add_new(UpdateAcc, ExprId, Defs),

      %% check if there exists an identical expression, so that
      %% this expression can be replaced directly.
      CodeAcc2 = 
	case expr_direct_replace(Expr) of
	  false ->
	    NewInstr = rewrite_expr(UpdateAcc2, ExprInstr, NewDefs),
	    [NewInstr | CodeAcc];
	  true ->
	    mk_defs_moves(CodeAcc, NewDefs, Defs)
	end,
      {CodeAcc2, UpdateAcc2}
    end, {CodeRest, NewUpdate}, NewInserts),

  NewCode = lists:reverse([CodeLast | NewCodeRest]),

  %% Check if we are to insert new reduction test here...
  {NewCode2, NewUpdate3} = 
    case PreLoop andalso ?SETS:size(Inserts) > 0 andalso not DelRed of
      true ->
	{[hipe_icode:mk_primop([], redtest, []) | NewCode], update_del_red_test_set(NewUpdate2)};
      false ->
	{NewCode, NewUpdate2}
    end,

  NewBB = hipe_bb:mk_bb(NewCode2),
  NewCFG = hipe_icode_cfg:bb_add(CFG, Label, NewBB),

  {NewCFG, NewUpdate3}.

%%-----------------------------------------------------------------------------
%% Create a new structure instruction from Instr with destination Defs
%% from the insert mapping in Update.

rewrite_expr(Update, Instr, Defs) ->
  NewInstr = 
    replace_call_variables(fun(Ref) ->
      case Ref of
	{ExprId, Num} when is_integer(ExprId) ->
	  {value, DefList} = update_inserted_lookup(Update, ExprId),
	  lists:nth(Num, DefList);
	_ -> Ref
      end end, Instr),
  hipe_icode:call_dstlist_update(NewInstr, Defs).

%%-----------------------------------------------------------------------------
%% Make move instructions from Defs list to all variables in
%% the Refs list and insert into Code.

mk_defs_moves(Code, [], []) -> Code;
mk_defs_moves(Code, [Ref | Refs], [Def | Defs]) -> 
  mk_defs_moves([hipe_icode:mk_move(Ref, Def) | Code], Refs, Defs).

%%-----------------------------------------------------------------------------
%% Utilities

new_work() ->
  {[], gb_sets:new()}.

add_work_list(List, Work) ->
  lists:foldl(fun(Label, WorkAcc) ->
    add_work_label(Label, WorkAcc) end, Work, List).

add_work_label(Label, {List, Set}) ->
  case gb_sets:is_member(Label, Set) of
    false ->
      {[Label | List], gb_sets:add(Label, Set)};
    true ->
      {List, Set}
  end.

get_work_list({List, _}) -> 
  lists:reverse(List).

%%-----------------------------------------------------------------------------
%% instr_type
%% gets a tag for the type of instruction that is passed in I

struct_instr_type(I) ->
  case I of
    #icode_call{type = primop, 'fun' = mktuple} -> 
      %%debug_count_case(?SR_STRUCT_INSTR_TYPE, #call{type = primop, 'fun' = mktuple}),
      struct;
    #icode_call{type = primop, 'fun' = cons} -> 
      %%debug_count_case(?SR_STRUCT_INSTR_TYPE, #call{type = primop, 'fun' = cons}),
      struct;
    #icode_call{type = primop, 'fun' = redtest} -> 
      %%debug_count_case(?SR_STRUCT_INSTR_TYPE, #call{type = primop, 'fun' = redtest}),
      redtest;
    _ ->
      %%debug_count_case(?SR_STRUCT_INSTR_TYPE, other),
      other 
  end.

instr_type(I) ->
  case I of
    %#call{type = primop, dstlist = List} when length(List) >= 1 -> struct;
    #icode_call{type = primop, 'fun' = {unsafe_element, Elem}, dstlist = [DstVar], args = [SrcVar]} -> 
      %%debug_count_case(?SR_INSTR_TYPE, #call{type = primop, 'fun' = {unsafe_element, num}}),
      {struct_elems, Elem, DstVar, SrcVar};
    #icode_phi{} -> 
      %%debug_count_case(?SR_INSTR_TYPE,#phi{}),
      phi;
    #icode_enter{} ->
      %%debug_count_case(?SR_INSTR_TYPE,#enter{}),
      return;
    #icode_return{} ->
      %%debug_count_case(?SR_INSTR_TYPE,#return{}),
      return;
    #icode_call{type = primop, 'fun' = mktuple} -> 
      %%debug_count_case(?SR_INSTR_TYPE, #call{type = primop, 'fun' = mktuple}),
      struct;
    #icode_call{type = primop, 'fun' = cons} ->
      %%debug_count_case(?SR_INSTR_TYPE, #call{type = primop, 'fun' = cons}),
      struct;
    #icode_call{type = primop, 'fun' = redtest} ->
      %%debug_count_case(?SR_INSTR_TYPE, #call{type = primop, 'fun' = redtest}),
      redtest;
    #icode_type{test = {tuple, Size}, args = [Var]} ->
      %%debug_count_case(?SR_INSTR_TYPE, #type{type = {tuple, size}}),
      {struct_type, Size, Var, ?MKTUPLE};
    #icode_type{test = cons, args = [Var]} ->
      %%debug_count_case(?SR_INSTR_TYPE,#type{type = cons}),
      {struct_type, 2, Var, ?CONS};
    %#type{type = {atom, Atom}, args = [Var]} -> {atom_type, Atom, Var};
    #icode_call{type = primop, 'fun' = unsafe_hd,
		dstlist = [DstVar], args = [SrcVar]} ->
      %%debug_count_case(?SR_INSTR_TYPE,#call{type = primop, 'fun' = unsafe_hd}),
      {struct_elems, 1, DstVar, SrcVar};
    #icode_call{type = primop, 'fun' = unsafe_tl,
		dstlist = [DstVar], args = [SrcVar]} ->
      %%debug_count_case(?SR_INSTR_TYPE, #call{type = primop, 'fun' = unsafe_tl}),
      {struct_elems, 2, DstVar, SrcVar};
    #icode_switch_tuple_arity{term = Var, cases = Cases} -> 
      %%debug_count_case(?SR_INSTR_TYPE,#switch_tuple_arity{}),
      {tuple_arity, Var, Cases};
    _ -> other
  end.
    
%%-----------------------------------------------------------------------------
%% Expression ID counter

init_expr_id() ->
  put({struct_reuse, expr_id_count}, 0).

-spec new_expr_id() -> non_neg_integer().
new_expr_id() ->
  V = get({struct_reuse, expr_id_count}),
  put({struct_reuse, expr_id_count}, V+1),
  V.

%%-----------------------------------------------------------------------------
%% Debug and print functions

print_struct(String, Struct) ->
  io:format(String),
  erlang:display(Struct).

print(String) ->
  io:format(String).

-ifdef(DEBUG).

debug_count_case(Type, Case) -> 
  Cases = get(Type), 
  NewCases = 
    case gb_trees:lookup(Case, Cases) of
      {value, Value} -> gb_trees:enter(Case, Value + 1, Cases);
      none -> gb_trees:insert(Case, 1, Cases)
    end,
  put(Type, NewCases).

debug_init_case_count(Type) -> 
  case get(Type) of
    undefined -> put(Type, gb_trees:empty());
    _ -> ok
  end.

debug_print_case_count(Type) ->
  Cases = get(Type), 
  debug_struct("Case type: ", Type),
  debug_list("Cases: ", gb_trees:to_list(Cases)).

set_debug_flag(Value) ->
  put({struct_reuse, debug}, Value).

get_debug_flag() -> get({struct_reuse, debug}).

debug_function(FuncName, CFG) ->
  Linear = hipe_icode_cfg:cfg_to_linear(CFG),
  Func = hipe_icode:icode_fun(Linear),
  case Func =:= FuncName orelse FuncName =:= nil of
    true -> 
      set_debug_flag(true),
      %% debug_struct("Code: ", hipe_icode_cfg:bb(CFG, 15)),
      debug_struct("~nFunction name :", Func);
    false -> 
      set_debug_flag(undefined)
  end.

debug_cfg_pp(CFG) ->
  case get_debug_flag() of
    true -> hipe_icode_cfg:pp(CFG);
    _ -> none
  end.

debug_struct(String, Struct) ->
  case get_debug_flag() of
    true ->
      io:format(String),
      erlang:display(Struct);
    _ -> none
  end.

debug(String) ->
  case get_debug_flag() of
    true -> io:format(String);
    _ -> none
  end.

debug_list(String, List) ->
  case get_debug_flag() of
    true -> print_list(String, List);
    _ -> none
  end.

print_list(String, List) ->
  io:format(String),
  io:format("~n"),
  print_list_rec(List),
  io:format("~n").

print_list_rec([]) -> ok;
print_list_rec([Struct | List]) ->
  erlang:display(Struct),
  print_list_rec(List).

debug_nodes(Nodes) ->
  lists:foreach(fun(Node) -> debug_node(Node) end, nodes_tree_values(Nodes)).

debug_node(Node) ->
  case get_debug_flag() of
    true ->
      print_struct("Node Label: ", Node#node.label),
      print_struct("Code: ", Node#node.code),
      print_struct("Phi: ", Node#node.phi),
      print_struct("PreLoop: ", Node#node.pre_loop),
      print_struct("Preds: ", Node#node.pred),
      print_struct("Succ: ", Node#node.succ),
      print_struct("Up Expr: ", Node#node.up_expr),
      print_struct("Kill : ", Node#node.killed_expr),
      print_struct("AnticIn: ", Node#node.antic_in), 
      print_struct("AnticOut: ", Node#node.antic_out),
      print_struct("SubInserts: ", Node#node.sub_inserts),
      print_struct("Inserts: ", Node#node.inserts),
      print_struct("NonStructDefs: ", Node#node.non_struct_defs),
      print_struct("Params: ", Node#node.struct_type),
      print_struct("Elems: ", Node#node.struct_elems),
      io:format("~n");
    _ -> none
  end.

-endif.
