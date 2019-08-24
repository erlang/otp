%% -*- erlang-indent-level: 2 -*-
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

%%%-------------------------------------------------------------------
%%% File    : dialyzer_typesig.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description :
%%%
%%% Created : 25 Apr 2005 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------

-module(dialyzer_typesig).

-export([analyze_scc/7]).
-export([get_safe_underapprox/2]).

%%-import(helper, %% 'helper' could be any module doing sanity checks...
-import(erl_types,
        [t_has_var/1, t_inf/2, t_is_equal/2, t_is_subtype/2,
        t_subtract/2, t_subtract_list/2, t_sup/1, t_sup/2,t_unify/2]).

-import(erl_types,
	[t_any/0, t_atom/0, t_atom_vals/1,
	 t_binary/0, t_bitstr/0, t_bitstr/2, t_bitstr_concat/1, t_boolean/0,
	 t_collect_vars/1, t_cons/2, t_cons_hd/1, t_cons_tl/1,
	 t_float/0, t_from_range/2, t_from_term/1,
	 t_fun/0, t_fun/2, t_fun_args/1, t_fun_range/1,
         t_integer/0,
	 t_is_any/1, t_is_atom/1, t_is_any_atom/2, t_is_cons/1,
	 t_is_float/1, t_is_fun/1,
	 t_is_integer/1, t_non_neg_integer/0,
	 t_is_list/1, t_is_nil/1, t_is_none/1, t_is_number/1,
	 t_is_singleton/1, t_is_none_or_unit/1,

         t_limit/2, t_list/0, t_list/1,
	 t_list_elements/1, t_nonempty_list/1, t_maybe_improper_list/0,
	 t_module/0, t_number/0, t_number_vals/1,
	 t_pid/0, t_port/0, t_product/1, t_reference/0,
	 t_subst/2,
	 t_timeout/0, t_tuple/0, t_tuple/1,
         t_var/1, t_var_name/1,
	 t_none/0, t_unit/0,
	 t_map/0, t_map/1, t_map_get/2, t_map_put/2
     ]).

-include("dialyzer.hrl").

%%-----------------------------------------------------------------------------

-type dep()      :: integer().  %% type variable names used as constraint ids
-type deps()     :: ordsets:ordset(dep()).

-type type_var() :: erl_types:erl_type(). %% actually: {'c','var',_,_}

-record(fun_var, {'fun' :: fun((_) -> erl_types:erl_type()), deps :: deps(),
		  origin :: integer() | 'undefined'}).

-type constr_op()    :: 'eq' | 'sub'.
-type fvar_or_type() :: #fun_var{} | erl_types:erl_type().

-record(constraint, {lhs  :: erl_types:erl_type(),
		     op   :: constr_op(),
		     rhs  :: fvar_or_type(),
		     deps :: deps()}).

-type constraint() :: #constraint{}.

-type mask() :: ordsets:ordset(non_neg_integer()).

-record(constraint_list, {type :: 'conj' | 'disj',
			  list :: [constr()],
                          deps :: deps(),
                          masks ::  #{dep() => mask()} | 'undefined',
			  id   :: {'list', dep()} | 'undefined'}).

-type constraint_list() :: #constraint_list{}.

-record(constraint_ref, {id :: type_var(), deps :: deps()}).

-type constraint_ref() :: #constraint_ref{}.

-type constr() :: constraint() | constraint_list() | constraint_ref().

-type types() :: erl_types:type_table().

-type typesig_funmap() :: #{type_var() => type_var()}.

-type prop_types() :: orddict:orddict(label(), erl_types:erl_type()).
-type dict_prop_types() :: dict:dict(label(), erl_types:erl_type()).

-record(state, {callgraph                :: dialyzer_callgraph:callgraph()
                                          | 'undefined',
                cserver                  :: dialyzer_codeserver:codeserver(),
		cs          = []         :: [constr()],
		cmap        = maps:new() :: #{type_var() => constr()},
		fun_map     = maps:new() :: typesig_funmap(),
		fun_arities = maps:new() :: #{type_var() => arity()},
		in_match    = false      :: boolean(),
		in_guard    = false      :: boolean(),
		module                   :: module(),
		name_map    = maps:new() :: #{mfa() => cerl:c_fun()},
		next_label  = 0          :: label(),
		self_rec                 :: 'false' | erl_types:erl_type(),
		plt                      :: dialyzer_plt:plt()
                                          | 'undefined',
		prop_types  = dict:new() :: dict_prop_types(),
                mod_records = []         :: [{module(), types()}],
		scc         = []         :: ordsets:ordset(type_var()),
		mfas                     :: [mfa()],
                solvers     = []         :: [solver()]
	       }).

-type state() :: #state{}.

%%-----------------------------------------------------------------------------

-define(TYPE_LIMIT, 4).
-define(INTERNAL_TYPE_LIMIT, 5).

%%-define(DEBUG, true).
%%-define(DEBUG_CONSTRAINTS, true).
-ifdef(DEBUG).
-define(DEBUG_NAME_MAP, true).
-define(DEBUG_LOOP_DETECTION, true).
-endif.
%%-define(DEBUG_NAME_MAP, true).
%%-define(DEBUG_LOOP_DETECTION, true).

-ifdef(DEBUG).
-define(debug(__String, __Args), io:format(__String, __Args)).
-define(mk_fun_var(Fun, Vars), mk_fun_var(?LINE, Fun, Vars)).
-define(pp_map(S, M), pp_map(S, M)).
-else.
-define(debug(__String, __Args), ok).
-define(mk_fun_var(Fun, Vars), mk_fun_var(Fun, Vars)).
-define(pp_map(S, M), ok).
-endif.

%% ============================================================================
%%
%%  The analysis.
%%
%% ============================================================================

%%-----------------------------------------------------------------------------
%% Analysis of strongly connected components.
%%
%% analyze_scc(SCC, NextLabel, CallGraph, CodeServer,
%%             PLT, PropTypes, Solvers) -> FunTypes
%%
%% SCC       - [{MFA}]
%% NextLabel - An integer that is higher than any label in the code.
%% CallGraph - A callgraph as produced by dialyzer_callgraph.erl
%%             Note: The callgraph must have been built with all the
%%                   code that the SCC is a part of.
%% PLT       - A dialyzer PLT. This PLT should contain available information
%%             about functions that can be called by this SCC.
%% PropTypes - A dictionary.
%% FunTypes  - A dictionary.
%% Solvers   - User specified solvers.
%%-----------------------------------------------------------------------------

-spec analyze_scc([mfa()], label(),
		  dialyzer_callgraph:callgraph(),
                  dialyzer_codeserver:codeserver(),
		  dialyzer_plt:plt(), prop_types(), [solver()]) -> prop_types().

analyze_scc(SCC, NextLabel, CallGraph, CServer, Plt, PropTypes, Solvers0) ->
  Solvers = solvers(Solvers0),
  State1 = new_state(SCC, NextLabel, CallGraph, CServer, Plt, PropTypes,
                     Solvers),
  DefSet = add_def_list(maps:values(State1#state.name_map), sets:new()),
  State2 = traverse_scc(SCC, CServer, DefSet, State1),
  State3 = state__finalize(State2),
  Funs = state__scc(State3),
  pp_constrs_scc(Funs, State3),
  constraints_to_dot_scc(Funs, State3),
  T = solve(Funs, State3),
  orddict:from_list(maps:to_list(T)).

solvers([]) -> [v2];
solvers(Solvers) -> Solvers.

%% ============================================================================
%%
%%  Gets the constraints by traversing the code.
%%
%% ============================================================================

traverse_scc([{M,_,_}=MFA|Left], Codeserver, DefSet, AccState) ->
  TmpState1 = state__set_module(AccState, M),
  Def = dialyzer_codeserver:lookup_mfa_code(MFA, Codeserver),
  DummyLetrec = cerl:c_letrec([Def], cerl:c_atom(foo)),
  TmpState2 = state__new_constraint_context(TmpState1),
  {NewAccState, _} = traverse(DummyLetrec, DefSet, TmpState2),
  traverse_scc(Left, Codeserver, DefSet, NewAccState);
traverse_scc([], _Codeserver, _DefSet, AccState) ->
  AccState.

traverse(Tree, DefinedVars, State) ->
  ?debug("Handling ~p\n", [cerl:type(Tree)]),
  case cerl:type(Tree) of
    alias ->
      Var = cerl:alias_var(Tree),
      Pat = cerl:alias_pat(Tree),
      DefinedVars1 = add_def(Var, DefinedVars),
      {State1, PatVar} = traverse(Pat, DefinedVars1, State),
      State2 = state__store_conj(mk_var(Var), eq, PatVar, State1),
      {State2, PatVar};
    apply ->
      Args = cerl:apply_args(Tree),
      Arity = length(Args),
      Op = cerl:apply_op(Tree),
      {State0, ArgTypes} = traverse_list(Args, DefinedVars, State),
      {State1, OpType} = traverse(Op, DefinedVars, State0),
      {State2, FunType} = state__get_fun_prototype(OpType, Arity, State1),
      State3 = state__store_conj(FunType, eq, OpType, State2),
      State4 = state__store_conj(mk_var(Tree), sub, t_fun_range(FunType),
				 State3),
      State5 = state__store_conj_lists(ArgTypes, sub, t_fun_args(FunType),
				       State4),
      case state__lookup_apply(Tree, State) of
	unknown ->
	  {State5, mk_var(Tree)};
	FunLabels ->
	  case get_apply_constr(FunLabels, mk_var(Tree), ArgTypes, State5) of
	    error -> {State5, mk_var(Tree)};
	    {ok, State6} -> {State6, mk_var(Tree)}
	  end
      end;
    binary ->
      {State1, SegTypes} = traverse_list(cerl:binary_segments(Tree),
					 DefinedVars, State),
      Type = ?mk_fun_var(fun(Map) ->
			     TmpSegTypes = lookup_type_list(SegTypes, Map),
			     t_bitstr_concat(TmpSegTypes)
			 end, SegTypes),
      {state__store_conj(mk_var(Tree), sub, Type, State1), mk_var(Tree)};
    bitstr ->
      Size = cerl:bitstr_size(Tree),
      UnitVal = cerl:int_val(cerl:bitstr_unit(Tree)),
      Val = cerl:bitstr_val(Tree),
      {State1, [SizeType, ValType]} =
	traverse_list([Size, Val], DefinedVars, State),
      {State2, TypeConstr, BinValTypeConstr} =
	case cerl:bitstr_bitsize(Tree) of
	  all ->
            T = t_bitstr(UnitVal, 0),
            {State1, T, T};
	  utf ->
            %% contains an integer number of bytes
            T = t_binary(),
            {State1, T, T};
	  N when is_integer(N) ->
            {State1, t_bitstr(0, N), t_bitstr(1, N)};
	  any -> % Size is not a literal
            T1 = ?mk_fun_var(bitstr_constr(SizeType, UnitVal), [SizeType]),
            T2 =
              ?mk_fun_var(bitstr_constr(SizeType, UnitVal, match), [SizeType]),
	    {state__store_conj(SizeType, sub, t_non_neg_integer(), State1),
             T1, T2}
	end,
      ValTypeConstr =
	case cerl:concrete(cerl:bitstr_type(Tree)) of
	  binary -> BinValTypeConstr;
	  float ->
	    case state__is_in_match(State1) of
	      true -> t_float();
	      false -> t_number()
	    end;
	  integer ->
	    case state__is_in_match(State1) of
	      true ->
		Flags = cerl:concrete(cerl:bitstr_flags(Tree)),
		?mk_fun_var(bitstr_val_constr(SizeType, UnitVal, Flags),
			    [SizeType]);
	      false -> t_integer()
	    end;
	  utf8  -> t_integer();
	  utf16 -> t_integer();
	  utf32 -> t_integer()
	end,
      State3 = state__store_conj(ValType, sub, ValTypeConstr, State2),
      State4 = state__store_conj(mk_var(Tree), sub, TypeConstr, State3),
      {State4, mk_var(Tree)};
    'case' ->
      Arg = cerl:case_arg(Tree),
      Clauses = filter_match_fail(cerl:case_clauses(Tree)),
      {State1, ArgVar} = traverse(Arg, DefinedVars, State),
      handle_clauses(Clauses, mk_var(Tree), ArgVar, DefinedVars, State1);
    call ->
      handle_call(Tree, DefinedVars, State);
    'catch' ->
      %% XXX: Perhaps there is something to say about this.
      {State, mk_var(Tree)};
    cons ->
      Hd = cerl:cons_hd(Tree),
      Tl = cerl:cons_tl(Tree),
      {State1, [HdVar, TlVar]} = traverse_list([Hd, Tl], DefinedVars, State),
      case cerl:is_literal(fold_literal_maybe_match(Tree, State)) of
	true ->
	  %% We do not need to do anything more here.
	  {State, t_cons(HdVar, TlVar)};
	false ->
	  ConsVar = mk_var(Tree),
	  ConsType = ?mk_fun_var(fun(Map) ->
				     t_cons(lookup_type(HdVar, Map),
					    lookup_type(TlVar, Map))
				 end, [HdVar, TlVar]),
	  HdType = ?mk_fun_var(fun(Map) ->
				   Cons = lookup_type(ConsVar, Map),
				   case t_is_cons(Cons) of
				     false -> t_any();
				     true -> t_cons_hd(Cons)
				   end
			       end, [ConsVar]),
	  TlType = ?mk_fun_var(fun(Map) ->
				   Cons = lookup_type(ConsVar, Map),
				   case t_is_cons(Cons) of
				     false -> t_any();
				     true -> t_cons_tl(Cons)
				   end
			       end, [ConsVar]),
	  State2 = state__store_conj_lists([HdVar, TlVar, ConsVar], sub,
					   [HdType, TlType, ConsType],
					   State1),
	  {State2, ConsVar}
      end;
    'fun' ->
      Body = cerl:fun_body(Tree),
      Vars = cerl:fun_vars(Tree),
      DefinedVars1 = add_def_list(Vars, DefinedVars),
      State0 = state__new_constraint_context(State),
      FunFailType =
	case state__prop_domain(cerl_trees:get_label(Tree), State0) of
	  error -> t_fun(length(Vars), t_none());
	  {ok, Dom} -> t_fun(Dom, t_none())
	end,
      TreeVar = mk_var(Tree),
      State2 =
	try
	  State1 = case state__add_prop_constrs(Tree, State0) of
		     not_called -> State0;
		     PropState -> PropState
		   end,
	  {BodyState, BodyVar} = traverse(Body, DefinedVars1, State1),
	  state__store_conj(TreeVar, eq,
			    t_fun(mk_var_list(Vars), BodyVar), BodyState)
	catch
	  throw:error ->
	    state__store_conj(TreeVar, eq, FunFailType, State0)
	end,
      Cs = state__cs(State2),
      State3 = state__store_constrs(TreeVar, Cs, State2),
      Ref = mk_constraint_ref(TreeVar, get_deps(Cs)),
      OldCs = state__cs(State),
      State4 = state__new_constraint_context(State3),
      State5 = state__store_conj_list([OldCs, Ref], State4),
      State6 = state__store_fun_arity(Tree, State5),
      State7 = state__add_fun_to_scc(TreeVar, State6),
      {State7, TreeVar};
    'let' ->
      Vars = cerl:let_vars(Tree),
      Arg = cerl:let_arg(Tree),
      Body = cerl:let_body(Tree),
      {State1, ArgVars} = traverse(Arg, DefinedVars, State),
      State2 = state__store_conj(t_product(mk_var_list(Vars)), eq,
				 ArgVars, State1),
      DefinedVars1 = add_def_list(Vars, DefinedVars),
      traverse(Body, DefinedVars1, State2);
    letrec ->
      Defs = cerl:letrec_defs(Tree),
      Body = cerl:letrec_body(Tree),
      Funs = [Fun || {_Var, Fun} <- Defs],
      Vars = [Var || {Var, _Fun} <- Defs],
      State1 = state__store_funs(Vars, Funs, State),
      DefinedVars1 = add_def_list(Vars, DefinedVars),
      {State2, _} = traverse_list(Funs, DefinedVars1, State1),
      traverse(Body, DefinedVars1, State2);
    literal ->
      %% Maps are special; a literal pattern matches more than just the value
      %% constructed by the literal. For example #{} constructs the empty map,
      %% but matches every map.
      case state__is_in_match(State) of
	true ->
	  Tree1 = dialyzer_utils:refold_pattern(Tree),
	  case cerl:is_literal(Tree1) of
	    false -> traverse(Tree1, DefinedVars, State);
	    true -> {State, t_from_term(cerl:concrete(Tree))}
	  end;
	_ -> {State, t_from_term(cerl:concrete(Tree))}
      end;
    module ->
      Defs = cerl:module_defs(Tree),
      Funs = [Fun || {_Var, Fun} <- Defs],
      Vars = [Var || {Var, _Fun} <- Defs],
      DefinedVars1 = add_def_list(Vars, DefinedVars),
      State1 = state__store_funs(Vars, Funs, State),
      FoldFun = fun(Fun, AccState) ->
		    {S, _} = traverse(Fun, DefinedVars1,
				      state__new_constraint_context(AccState)),
		    S
		end,
      lists:foldl(FoldFun, State1, Funs);
    primop ->
      case cerl:atom_val(cerl:primop_name(Tree)) of
	match_fail -> throw(error);
	raise -> throw(error);
	bs_init_writable -> {State, t_from_term(<<>>)};
	build_stacktrace ->
          V = mk_var(Tree),
          Type = erl_bif_types:type(erlang, build_stacktrace, 0),
          State1 = state__store_conj(V, sub, Type, State),
          {State1, V};
	Other -> erlang:error({'Unsupported primop', Other})
      end;
    'receive' ->
      Clauses = filter_match_fail(cerl:receive_clauses(Tree)),
      Timeout = cerl:receive_timeout(Tree),
      case (cerl:is_c_atom(Timeout) andalso
	    (cerl:atom_val(Timeout) =:= infinity)) of
	true ->
	  handle_clauses(Clauses, mk_var(Tree), [], DefinedVars, State);
 	false ->
	  Action = cerl:receive_action(Tree),
	  {State1, TimeoutVar} = traverse(Timeout, DefinedVars, State),
	  State2 = state__store_conj(TimeoutVar, sub, t_timeout(), State1),
	  handle_clauses(Clauses, mk_var(Tree), [], Action, DefinedVars, State2)
     end;
    seq ->
      Body = cerl:seq_body(Tree),
      Arg = cerl:seq_arg(Tree),
      {State1, _} = traverse(Arg, DefinedVars, State),
      traverse(Body, DefinedVars, State1);
    'try' ->
      handle_try(Tree, DefinedVars, State);
    tuple ->
      Elements = cerl:tuple_es(Tree),
      {State1, EVars} = traverse_list(Elements, DefinedVars, State),
      {State2, TupleType} =
	case cerl:is_literal(fold_literal_maybe_match(Tree, State1)) of
	  true ->
	    %% We do not need to do anything more here.
	    {State, t_tuple(EVars)};
	  false ->
	    %% We have the same basic problem as in products, but we want to
	    %% make sure that everything that can be used as tags for the
	    %% disjoint unions stays in the tuple.
	    Fun = fun(Var, AccState) ->
		      case t_has_var(Var) of
			true ->
			  {AccState1, NewVar} = state__mk_var(AccState),
			  {NewVar,
			   state__store_conj(Var, eq, NewVar, AccState1)};
			false ->
			  {Var, AccState}
		      end
		  end,
	    {NewEvars, TmpState} = lists:mapfoldl(Fun, State1, EVars),
	    {TmpState, t_tuple(NewEvars)}
	end,
      case Elements of
	[Tag|Fields] ->
	  case cerl:is_c_atom(Tag) andalso is_literal_record(Tree) of
	    true ->
              %% Check if a record is constructed.
              Arity = length(Fields),
              case lookup_record(State2, cerl:atom_val(Tag), Arity) of
                {error, State3} -> {State3, TupleType};
                {ok, RecType, State3} ->
                  State4 = state__store_conj(TupleType, sub, RecType, State3),
                  {State4, TupleType}
              end;
	    false -> {State2, TupleType}
          end;
	[] -> {State2, TupleType}
      end;
    map ->
      Entries = cerl:map_es(Tree),
      MapFoldFun = fun(Entry, AccState) ->
		       AccState1 = state__set_in_match(AccState, false),
		       {AccState2, KeyVar} = traverse(cerl:map_pair_key(Entry),
						      DefinedVars, AccState1),
		       AccState3 = state__set_in_match(
				     AccState2, state__is_in_match(AccState)),
		       {AccState4, ValVar} = traverse(cerl:map_pair_val(Entry),
						      DefinedVars, AccState3),
		       {{KeyVar, ValVar}, AccState4}
		   end,
      {Pairs, State1} = lists:mapfoldl(MapFoldFun, State, Entries),
      %% We mustn't recurse into map arguments to matches. Not only are they
      %% syntactically only allowed to be the literal #{}, but that would also
      %% cause an infinite recursion, since traverse/3 unfolds literals with
      %% maps in them using dialyzer_utils:reflow_pattern/1.
      {State2, ArgVar} =
	case state__is_in_match(State) of
	  false -> traverse(cerl:map_arg(Tree), DefinedVars, State1);
	  true -> {State1, t_map()}
	end,
      MapVar = mk_var(Tree),
      MapType = ?mk_fun_var(
		   fun(Map) ->
		       lists:foldl(
			 fun({K,V}, TypeAcc) ->
			     t_map_put({lookup_type(K, Map),
					lookup_type(V, Map)},
				       TypeAcc)
			 end, t_inf(t_map(), lookup_type(ArgVar, Map)),
			 Pairs)
		   end, [ArgVar | lists:append([[K,V] || {K,V} <- Pairs])]),
      %% TODO: does the "same element appearing several times" problem apply
      %% here too?
      Fun =
	fun({KeyVar, ValVar}, {AccState, ShadowKeys}) ->
	    %% If Val is known to be the last association of Key (i.e. Key
	    %% is not in ShadowKeys), Val must be a subtype of what is
	    %% associated to Key in Tree
	    TypeFun =
	      fun(Map) ->
		  KeyType = lookup_type(KeyVar, Map),
		  case t_is_singleton(KeyType) of
		    false -> t_any();
		    true ->
		      MT = t_inf(lookup_type(MapVar, Map), t_map()),
		      case t_is_none_or_unit(MT) of
			true -> t_none();
			false ->
			  DisjointFromKeyType =
			    fun(ShadowKey) ->
                                ST = t_inf(lookup_type(ShadowKey, Map),
                                           KeyType),
				t_is_none_or_unit(ST)
			    end,
			  case lists:all(DisjointFromKeyType, ShadowKeys) of
			    true -> t_map_get(KeyType, MT);
			    %% A later association might shadow this one
			    false -> t_any()
			  end
		      end
		  end
	      end,
	    ValType = ?mk_fun_var(TypeFun, [KeyVar, MapVar | ShadowKeys]),
	    {state__store_conj(ValVar, sub, ValType, AccState),
	     [KeyVar | ShadowKeys]}
	end,
      %% Accumulate shadowing keys right-to-left
      {State3, _} = lists:foldr(Fun, {State2, []}, Pairs),
      %% In a map expression, Arg must contain all keys that are inserted with
      %% the exact (:=) operator, and are known (i.e. are not in ShadowedKeys)
      %% to not have been introduced by a previous association
      State4 =
	case state__is_in_match(State) of
	  true -> State3;
	  false ->
	    ArgFun =
	      fun(Map) ->
		  FoldFun =
		    fun({{KeyVar, _}, Entry}, {AccType, ShadowedKeys}) ->
			OpTree = cerl:map_pair_op(Entry),
			KeyType = lookup_type(KeyVar, Map),
			AccType1 =
			  case cerl:is_literal(OpTree) andalso
			    cerl:concrete(OpTree) =:= exact of
			    true ->
                              ST = t_inf(ShadowedKeys, KeyType),
                              case t_is_none_or_unit(ST) of
				true ->
				  t_map_put({KeyType, t_any()}, AccType);
				false ->
				  AccType
			      end;
			    false ->
			      AccType
			  end,
			{AccType1, t_sup(KeyType, ShadowedKeys)}
		    end,
		  %% Accumulate shadowed keys left-to-right
		  {ResType, _} = lists:foldl(FoldFun, {t_map(), t_none()},
					     lists:zip(Pairs, Entries)),
		  ResType
	      end,
	    ArgType = ?mk_fun_var(ArgFun, [KeyVar || {KeyVar, _} <- Pairs]),
	    state__store_conj(ArgVar, sub, ArgType, State3)
	end,
      {state__store_conj(MapVar, sub, MapType, State4), MapVar};
    values ->
      %% We can get into trouble when unifying products that have the
      %% same element appearing several times. Handle these cases by
      %% introducing fresh variables and constraining them to be equal
      %% to the original ones. This is similar to what happens in
      %% pattern matching where the matching is done on fresh
      %% variables and guards assert that the matching is correct.
      Elements = cerl:values_es(Tree),
      {State1, EVars} = traverse_list(Elements, DefinedVars, State),
      Arity = length(EVars),
      Unique = length(ordsets:from_list(EVars)),
      case Arity =:= Unique of
	true -> {State1, t_product(EVars)};
	false ->
	  {State2, Vars} = state__mk_vars(Arity, State1),
	  State3 = state__store_conj_lists(Vars, eq, EVars, State2),
	  {State3, t_product(Vars)}
      end;
    var ->
      case is_def(Tree, DefinedVars) of
	true -> {State, mk_var(Tree)};
	false ->
	  %% If we are analyzing SCCs this can be a function variable.
	  case state__lookup_undef_var(Tree, State) of
	    error -> erlang:error({'Undefined variable', Tree});
	    {ok, Type} ->
	      {State1, NewVar} = state__mk_var(State),
	      {state__store_conj(NewVar, sub, Type, State1), NewVar}
	  end
      end;
    Other ->
      erlang:error({'Unsupported type', Other})
  end.

traverse_list(Trees, DefinedVars, State) ->
  traverse_list(Trees, DefinedVars, State, []).

traverse_list([Tree|Tail], DefinedVars, State, Acc) ->
  {State1, Var} = traverse(Tree, DefinedVars, State),
  traverse_list(Tail, DefinedVars, State1, [Var|Acc]);
traverse_list([], _DefinedVars, State, Acc) ->
  {State, lists:reverse(Acc)}.

add_def(Var, Set) ->
  sets:add_element(cerl_trees:get_label(Var), Set).

add_def_list([H|T], Set) ->
  add_def_list(T, add_def(H, Set));
add_def_list([], Set) ->
  Set.

add_def_from_tree(T, DefinedVars) ->
  Vars = cerl_trees:fold(fun(X, Acc) ->
			     case cerl:is_c_var(X) of
			       true -> [X|Acc];
			       false -> Acc
			     end
			 end, [], T),
  add_def_list(Vars, DefinedVars).

add_def_from_tree_list([H|T], DefinedVars) ->
  add_def_from_tree_list(T, add_def_from_tree(H, DefinedVars));
add_def_from_tree_list([], DefinedVars) ->
  DefinedVars.

is_def(Var, Set) ->
  sets:is_element(cerl_trees:get_label(Var), Set).

%%----------------------------------------
%% Try
%%

handle_try(Tree, DefinedVars, State) ->
  Arg = cerl:try_arg(Tree),
  Vars = cerl:try_vars(Tree),
  EVars = cerl:try_evars(Tree),
  Body = cerl:try_body(Tree),
  Handler = cerl:try_handler(Tree),
  State1 = state__new_constraint_context(State),
  {ArgBodyState, BodyVar} =
    try
      {State2, ArgVar} = traverse(Arg, DefinedVars, State1),
      DefinedVars1 = add_def_list(Vars, DefinedVars),
      {State3, BodyVar1} = traverse(Body, DefinedVars1, State2),
      State4 = state__store_conj(t_product(mk_var_list(Vars)), eq, ArgVar,
				 State3),
      {State4, BodyVar1}
    catch
      throw:error ->
	{State1, t_none()}
    end,
  State6 = state__new_constraint_context(ArgBodyState),
  {HandlerState, HandlerVar} =
    try
      DefinedVars2 = add_def_list([X || X <- EVars, cerl:is_c_var(X)],
				  DefinedVars),
      traverse(Handler, DefinedVars2, State6)
    catch
      throw:error ->
	{State6, t_none()}
    end,
  ArgBodyCs = state__cs(ArgBodyState),
  HandlerCs = state__cs(HandlerState),
  TreeVar = mk_var(Tree),
  OldCs = state__cs(State),
  case state__is_in_guard(State) of
    true ->
      Conj1 = mk_conj_constraint_list([ArgBodyCs,
				       mk_constraint(BodyVar,
                                                     eq,
                                                     TreeVar)]),
      Disj = mk_disj_constraint_list([Conj1,
				      mk_constraint(HandlerVar,
                                                    eq,
                                                    TreeVar)]),
      NewState1 = state__new_constraint_context(HandlerState),
      Conj2 = mk_conj_constraint_list([OldCs, Disj]),
      NewState2 = state__store_conj(Conj2, NewState1),
      {NewState2, TreeVar};
    false ->
      {NewCs, ReturnVar} =
	case {t_is_none(BodyVar), t_is_none(HandlerVar)} of
	  {false, false} ->
	    Conj1 =
	      mk_conj_constraint_list([ArgBodyCs,
				       mk_constraint(TreeVar,
                                                     eq,
                                                     BodyVar)]),
	    Conj2 =
	      mk_conj_constraint_list([HandlerCs,
				       mk_constraint(TreeVar,
                                                     eq,
                                                     HandlerVar)]),
	    Disj = mk_disj_constraint_list([Conj1, Conj2]),
	    {Disj, TreeVar};
	  {false, true} ->
	    {mk_conj_constraint_list([ArgBodyCs,
				      mk_constraint(TreeVar,
                                                    eq,
                                                    BodyVar)]),
	     BodyVar};
	  {true, false} ->
	    {mk_conj_constraint_list([HandlerCs,
				      mk_constraint(TreeVar,
                                                    eq,
                                                    HandlerVar)]),
	     HandlerVar};
	  {true, true} ->
	    ?debug("Throw failed\n", []),
	    throw(error)
	end,
      Conj = mk_conj_constraint_list([OldCs, NewCs]),
      NewState1 = state__new_constraint_context(HandlerState),
      NewState2 = state__store_conj(Conj, NewState1),
      {NewState2, ReturnVar}
  end.

%%----------------------------------------
%% Call
%%

handle_call(Call, DefinedVars, State) ->
  Args = cerl:call_args(Call),
  Mod = cerl:call_module(Call),
  Fun = cerl:call_name(Call),
  Dst = mk_var(Call),
  case cerl:is_c_atom(Mod) andalso cerl:is_c_atom(Fun) of
    true ->
      M = cerl:atom_val(Mod),
      F = cerl:atom_val(Fun),
      A = length(Args),
      MFA = {M, F, A},
      {State1, ArgVars} = traverse_list(Args, DefinedVars, State),
      case state__lookup_rec_var_in_scope(MFA, State) of
	error ->
	  case get_bif_constr(MFA, Dst, ArgVars, State1) of
	    none ->
	      {get_plt_constr(MFA, Dst, ArgVars, State1), Dst};
	    C ->
	      {state__store_conj(C, State1), Dst}
	  end;
	{ok, Var} ->
	  %% This is part of the SCC currently analyzed.
	  %% Intercept and change this to an apply instead.
	  ?debug("Found the call to ~tw\n", [MFA]),
	  Label = cerl_trees:get_label(Call),
	  Apply = cerl:ann_c_apply([{label, Label}], Var, Args),
	  traverse(Apply, DefinedVars, State)
      end;
    false ->
      {State1, MF} = traverse_list([Mod, Fun], DefinedVars, State),
      {state__store_conj_lists(MF, sub, [t_module(), t_atom()], State1), Dst}
  end.

get_plt_constr(MFA, Dst, ArgVars, State) ->
  Plt = state__plt(State),
  PltRes = dialyzer_plt:lookup(Plt, MFA),
  SCCMFAs = State#state.mfas,
  Contract =
    case lists:member(MFA, SCCMFAs) of
      true -> none;
      false -> dialyzer_plt:lookup_contract(Plt, MFA)
    end,
  case Contract of
    none ->
      case PltRes of
	none -> State;
	{value, {PltRetType, PltArgTypes}} ->
	  state__store_conj_lists([Dst|ArgVars], sub,
				  [PltRetType|PltArgTypes], State)
      end;
    {value, #contract{args = GenArgs} = C} ->
      {RetType, ArgCs} =
	case PltRes of
	  none ->
	    {?mk_fun_var(fun(Map) ->
			     ArgTypes = lookup_type_list(ArgVars, Map),
                             get_contract_return(C, ArgTypes)
			 end, ArgVars), GenArgs};
	  {value, {PltRetType, PltArgTypes}} ->
	    %% Need to combine the contract with the success typing.
	    {?mk_fun_var(
		fun(Map) ->
		    ArgTypes = lookup_type_list(ArgVars, Map),
                    CRet = get_contract_return(C, ArgTypes),
		    t_inf(CRet, PltRetType)
		end, ArgVars),
	     [t_inf(X, Y) || {X, Y} <- lists:zip(GenArgs, PltArgTypes)]}
	end,
      state__store_conj_lists([Dst|ArgVars], sub, [RetType|ArgCs], State)
  end.

get_contract_return(C, ArgTypes) ->
  dialyzer_contracts:get_contract_return(C, ArgTypes).

filter_match_fail([Clause] = Cls) ->
  Body = cerl:clause_body(Clause),
  case cerl:type(Body) of
    primop ->
      case cerl:atom_val(cerl:primop_name(Body)) of
	match_fail -> [];
	raise -> [];
	_ -> Cls
      end;
    _ -> Cls
  end;
filter_match_fail([H|T]) ->
  [H|filter_match_fail(T)];
filter_match_fail([]) ->
  %% This can actually happen, for example in
  %%      receive after 1 -> ok end
  [].

%% If there is a significant number of clauses, we cannot apply the
%% list subtraction scheme since it causes the analysis to be too
%% slow. Typically, this only affects automatically generated files.
%% The dataflow analysis doesn't suffer from this, so we will get some
%% information anyway.
-define(MAX_NOF_CLAUSES, 15).

handle_clauses(Clauses, TopVar, Arg, DefinedVars, State) ->
  handle_clauses(Clauses, TopVar, Arg, none, DefinedVars, State).

handle_clauses([], _, _, Action, DefinedVars, State) when Action =/= none ->
  %% Can happen when a receive has no clauses, see filter_match_fail.
  traverse(Action, DefinedVars, State);
handle_clauses(Clauses, TopVar, Arg, Action, DefinedVars, State) ->
  SubtrTypeList =
    if length(Clauses) > ?MAX_NOF_CLAUSES -> overflow;
       true -> []
    end,
  {State1, CList} = handle_clauses_1(Clauses, TopVar, Arg, DefinedVars,
				     State, SubtrTypeList, []),
  {NewCs, NewState} =
    case Action of
      none ->
	if CList =:= [] -> throw(error);
	   true -> {CList, State1}
	end;
      _ ->
	try
	  {State2, ActionVar} = traverse(Action, DefinedVars, State1),
	  TmpC = mk_constraint(TopVar, eq, ActionVar),
	  ActionCs = mk_conj_constraint_list([state__cs(State2),TmpC]),
	  {[ActionCs|CList], State2}
	catch
	  throw:error ->
	    if CList =:= [] -> throw(error);
	       true -> {CList, State1}
	    end
	end
    end,
  OldCs = state__cs(State),
  NewCList = mk_disj_constraint_list(NewCs),
  FinalState = state__new_constraint_context(NewState),
  {state__store_conj_list([OldCs, NewCList], FinalState), TopVar}.

handle_clauses_1([Clause|Tail], TopVar, Arg, DefinedVars,
		 State, SubtrTypes, Acc) ->
  State0 = state__new_constraint_context(State),
  Pats = cerl:clause_pats(Clause),
  Guard = cerl:clause_guard(Clause),
  Body = cerl:clause_body(Clause),
  NewSubtrTypes =
    case SubtrTypes =:= overflow of
      true -> overflow;
      false ->
	ordsets:add_element(get_safe_underapprox(Pats, Guard), SubtrTypes)
    end,
  try
    DefinedVars1 = add_def_from_tree_list(Pats, DefinedVars),
    State1 = state__set_in_match(State0, true),
    {State2, PatVars} = traverse_list(Pats, DefinedVars1, State1),
    State3 =
      case Arg =:= [] of
	true -> State2;
        false ->
	  S = state__store_conj(Arg, eq, t_product(PatVars), State2),
	  case SubtrTypes =:= overflow of
	    true -> S;
	    false ->
	      SubtrPatVar = ?mk_fun_var(fun(Map) ->
					    TmpType = lookup_type(Arg, Map),
					    t_subtract_list(TmpType, SubtrTypes)
					end, [Arg]),
	      state__store_conj(Arg, sub, SubtrPatVar, S)
	  end
      end,
    State4 = handle_guard(Guard, DefinedVars1, State3),
    {State5, BodyVar} = traverse(Body, DefinedVars1,
				 state__set_in_match(State4, false)),
    State6 = state__store_conj(TopVar, eq, BodyVar, State5),
    Cs = state__cs(State6),
    handle_clauses_1(Tail, TopVar, Arg, DefinedVars, State6,
		     NewSubtrTypes, [Cs|Acc])
  catch
    throw:error ->
      handle_clauses_1(Tail, TopVar, Arg, DefinedVars,
		       State, NewSubtrTypes, Acc)
  end;
handle_clauses_1([], _TopVar, _Arg, _DefinedVars, State, _SubtrType, Acc) ->
  {state__new_constraint_context(State), Acc}.

-spec get_safe_underapprox([cerl:c_values()], cerl:cerl()) -> erl_types:erl_type().

get_safe_underapprox(Pats, Guard) ->
  try
    Map1 = cerl_trees:fold(fun(X, Acc) ->
			       case cerl:is_c_var(X) of
				 true ->
				   maps:put(cerl_trees:get_label(X), t_any(),
                                            Acc);
				 false -> Acc
			       end
			   end, maps:new(), cerl:c_values(Pats)),
    {Type, Map2} = get_underapprox_from_guard(Guard, Map1),
    Map3 = case t_is_none(t_inf(t_from_term(true), Type)) of
	     true -> throw(dont_know);
	     false ->
	       case cerl:is_c_var(Guard) of
		 false -> Map2;
		 true ->
		   maps:put(cerl_trees:get_label(Guard),
                            t_from_term(true), Map2)
	       end
	   end,
    {Ts, _Map4} = get_safe_underapprox_1(Pats, [], Map3),
    t_product(Ts)
  catch
    throw:dont_know -> t_none()
  end.

get_underapprox_from_guard(Tree, Map) ->
  True = t_from_term(true),
  case cerl:type(Tree) of
    call ->
      case {cerl:concrete(cerl:call_module(Tree)),
	    cerl:concrete(cerl:call_name(Tree)),
	    length(cerl:call_args(Tree))} of
	{erlang, is_function, 2} ->
	  [Fun, Arity] = cerl:call_args(Tree),
	  case cerl:is_c_int(Arity) of
	    false -> throw(dont_know);
	    true ->
	      {FunType, Map1} = get_underapprox_from_guard(Fun, Map),
	      Inf = t_inf(FunType, t_fun(cerl:int_val(Arity), t_any())),
	      case t_is_none(Inf) of
		true -> throw(dont_know);
		false ->
		  {True, maps:put(cerl_trees:get_label(Fun), Inf, Map1)}
	      end
	  end;
	MFA ->
	  case get_type_test(MFA) of
	    {ok, Type} ->
	      [Arg0] = cerl:call_args(Tree),
              Arg = cerl:fold_literal(Arg0),
	      {ArgType, Map1} = get_underapprox_from_guard(Arg, Map),
	      Inf = t_inf(Type, ArgType),
	      case t_is_none(Inf) of
		true -> throw(dont_know);
		false ->
		  case cerl:is_literal(Arg) of
		    true -> {True, Map1};
		    false ->
		      {True, maps:put(cerl_trees:get_label(Arg), Inf, Map1)}
		  end
	      end;
	    error ->
	      case MFA of
		{erlang, '=:=', 2} -> throw(dont_know);
		{erlang, '==', 2} -> throw(dont_know);
		{erlang, 'and', 2} ->
		  [Arg1_0, Arg2_0] = cerl:call_args(Tree),
                  Arg1 = cerl:fold_literal(Arg1_0),
                  Arg2 = cerl:fold_literal(Arg2_0),
		  case ((cerl:is_c_var(Arg1) orelse cerl:is_literal(Arg1))
			andalso
			(cerl:is_c_var(Arg2) orelse cerl:is_literal(Arg2))) of
		    true ->
		      {Arg1Type, _} = get_underapprox_from_guard(Arg1, Map),
		      {Arg2Type, _} = get_underapprox_from_guard(Arg2, Map),
		      case (t_is_equal(True, Arg1Type) andalso
			    t_is_equal(True, Arg2Type)) of
			true -> {True, Map};
			false -> throw(dont_know)
		      end;
		    false ->
		      throw(dont_know)
		  end;
		{erlang, 'or', 2} -> throw(dont_know);
		_ -> throw(dont_know)
	      end
	  end
      end;
    var ->
      Type =
	case maps:find(cerl_trees:get_label(Tree), Map) of
	  error -> throw(dont_know);
	  {ok, T} -> T
	end,
      {Type, Map};
    literal ->
      case cerl:unfold_literal(Tree) of
	Tree ->
	  Type =
	    case cerl:concrete(Tree) of
	      Int when is_integer(Int) -> t_from_term(Int);
	      Atom when is_atom(Atom) -> t_from_term(Atom);
	      _Other -> throw(dont_know)
	    end,
	  {Type, Map};
	OtherTree ->
	  get_underapprox_from_guard(OtherTree, Map)
      end;
    _ ->
      throw(dont_know)
  end.

%%
%% The guard test {erlang, is_function, 2} is handled specially by the
%% function get_underapprox_from_guard/2
%%
get_type_test({erlang, is_atom, 1}) ->      {ok, t_atom()};
get_type_test({erlang, is_boolean, 1}) ->   {ok, t_boolean()};
get_type_test({erlang, is_binary, 1}) ->    {ok, t_binary()};
get_type_test({erlang, is_bitstring, 1}) -> {ok, t_bitstr()};
get_type_test({erlang, is_float, 1}) ->     {ok, t_float()};
get_type_test({erlang, is_function, 1}) ->  {ok, t_fun()};
get_type_test({erlang, is_integer, 1}) ->   {ok, t_integer()};
get_type_test({erlang, is_list, 1}) ->      {ok, t_list()};
get_type_test({erlang, is_map, 1}) ->       {ok, t_map()};
get_type_test({erlang, is_number, 1}) ->    {ok, t_number()};
get_type_test({erlang, is_pid, 1}) ->       {ok, t_pid()};
get_type_test({erlang, is_port, 1}) ->      {ok, t_port()};
%% get_type_test({erlang, is_record, 2}) ->    {ok, t_tuple()};
%% get_type_test({erlang, is_record, 3}) ->    {ok, t_tuple()};
get_type_test({erlang, is_reference, 1}) -> {ok, t_reference()};
get_type_test({erlang, is_tuple, 1}) ->     {ok, t_tuple()};
get_type_test({M, F, A}) when is_atom(M), is_atom(F), is_integer(A) -> error.

bitstr_constr(SizeType, UnitVal) ->
  bitstr_constr(SizeType, UnitVal, construct).

bitstr_constr(SizeType, UnitVal, ConstructOrMatch) ->
  Unit =
    case ConstructOrMatch of
      construct -> 0;
      match -> 1
    end,
  fun(Map) ->
      TmpSizeType = lookup_type(SizeType, Map),
      case t_is_subtype(TmpSizeType, t_non_neg_integer()) of
	true ->
	  case t_number_vals(TmpSizeType) of
	    [OneSize] -> t_bitstr(Unit, OneSize * UnitVal);
	    _ ->
	      MinSize = erl_types:number_min(TmpSizeType),
	      t_bitstr(UnitVal, MinSize * UnitVal)
	  end;
	false ->
	  t_bitstr(UnitVal, 0)
      end
  end.

bitstr_val_constr(SizeType, UnitVal, Flags) ->
  fun(Map) ->
      TmpSizeType = lookup_type(SizeType, Map),
      case t_is_subtype(TmpSizeType, t_non_neg_integer()) of
	true ->
	  case erl_types:number_max(TmpSizeType) of
	    N when is_integer(N), N < 128 -> %% Avoid illegal arithmetic
	      TotalSizeVal = N * UnitVal,
	      {RangeMin, RangeMax} =
		case lists:member(signed, Flags) of
		  true -> {-(1 bsl (TotalSizeVal - 1)),
			   1 bsl (TotalSizeVal - 1) - 1};
		  false -> {0, 1 bsl TotalSizeVal - 1}
		end,
	      t_from_range(RangeMin, RangeMax);
	    _ ->
	      t_integer()
	  end;
	false ->
	  t_integer()
      end
  end.

get_safe_underapprox_1([Pat0|Left], Acc, Map) ->
  %% Maps should be treated as patterns, not as literals
  Pat = dialyzer_utils:refold_pattern(Pat0),
  case cerl:type(Pat) of
    alias ->
      APat = cerl:alias_pat(Pat),
      AVar = cerl:alias_var(Pat),
      {[VarType], Map1} = get_safe_underapprox_1([AVar], [], Map),
      {[PatType], Map2} = get_safe_underapprox_1([APat], [], Map1),
      Inf = t_inf(VarType, PatType),
      case t_is_none(Inf) of
	true -> throw(dont_know);
	false ->
	  Map3 = maps:put(cerl_trees:get_label(AVar), Inf, Map2),
	  get_safe_underapprox_1(Left, [Inf|Acc], Map3)
      end;
    binary ->
      %% TODO: Can maybe do something here
      throw(dont_know);
    cons ->
      {[Hd, Tl], Map1} =
	get_safe_underapprox_1([cerl:cons_hd(Pat), cerl:cons_tl(Pat)], [], Map),
      case t_is_any(Tl) of
	true -> get_safe_underapprox_1(Left, [t_nonempty_list(Hd)|Acc], Map1);
	false -> throw(dont_know)
      end;
    literal ->
      case cerl:unfold_literal(Pat) of
	Pat ->
	  Type =
	    case cerl:concrete(Pat) of
	      Int when is_integer(Int) -> t_from_term(Int);
	      Atom when is_atom(Atom) -> t_from_term(Atom);
	      [] -> t_from_term([]);
	      _Other -> throw(dont_know)
	    end,
	  get_safe_underapprox_1(Left, [Type|Acc], Map);
	OtherPat ->
	  get_safe_underapprox_1([OtherPat|Left], Acc, Map)
      end;
    tuple ->
      Es = cerl:tuple_es(Pat),
      {Ts, Map1} = get_safe_underapprox_1(Es, [], Map),
      Type = t_tuple(Ts),
      get_safe_underapprox_1(Left, [Type|Acc], Map1);
    map ->
      %% Some assertions in case the syntax gets more premissive in the future
      true = #{} =:= cerl:concrete(cerl:map_arg(Pat)),
      true = lists:all(fun(P) ->
			   cerl:is_literal(Op = cerl:map_pair_op(P)) andalso
			     exact =:= cerl:concrete(Op)
		       end, cerl:map_es(Pat)),
      KeyTrees = lists:map(fun cerl:map_pair_key/1, cerl:map_es(Pat)),
      ValTrees = lists:map(fun cerl:map_pair_val/1, cerl:map_es(Pat)),
      %% Keys must not be underapproximated. Overapproximations are safe.
      Keys = get_safe_overapprox(KeyTrees),
      {Vals, Map1} = get_safe_underapprox_1(ValTrees, [], Map),
      case lists:all(fun erl_types:t_is_singleton/1, Keys) of
	false -> throw(dont_know);
	true -> ok
      end,
      SortedPairs = lists:sort(lists:zip(Keys, Vals)),
      %% We need to deal with duplicates ourselves
      SquashDuplicates =
	fun SquashDuplicates([{K,First},{K,Second}|List]) ->
	    case t_is_none(Inf = t_inf(First, Second)) of
	      true -> throw(dont_know);
	      false -> [{K, Inf}|SquashDuplicates(List)]
	    end;
	    SquashDuplicates([Good|Rest]) ->
	    [Good|SquashDuplicates(Rest)];
	    SquashDuplicates([]) -> []
	end,
      Type = t_map(SquashDuplicates(SortedPairs)),
      get_safe_underapprox_1(Left, [Type|Acc], Map1);
    values ->
      Es = cerl:values_es(Pat),
      {Ts, Map1} = get_safe_underapprox_1(Es, [], Map),
      Type = t_product(Ts),
      get_safe_underapprox_1(Left, [Type|Acc], Map1);
    var ->
      case maps:find(cerl_trees:get_label(Pat), Map) of
	error -> throw(dont_know);
	{ok, VarType} -> get_safe_underapprox_1(Left, [VarType|Acc], Map)
      end
  end;
get_safe_underapprox_1([], Acc, Map) ->
  {lists:reverse(Acc), Map}.

get_safe_overapprox(Pats) ->
  lists:map(fun get_safe_overapprox_1/1, Pats).

get_safe_overapprox_1(Pat) ->
  case cerl:is_literal(Lit = cerl:fold_literal(Pat)) of
    true  -> t_from_term(cerl:concrete(Lit));
    false -> t_any()
  end.

%%----------------------------------------
%% Guards
%%

handle_guard(Guard, DefinedVars, State) ->
  True = t_from_term(true),
  State1 = state__set_in_guard(State, true),
  State2 = state__new_constraint_context(State1),
  {State3, Return} = traverse(Guard, DefinedVars, State2),
  State4 = state__store_conj(Return, eq, True, State3),
  Cs = state__cs(State4),
  NewCs = mk_disj_norm_form(Cs),
  OldCs = state__cs(State),
  State5 = state__set_in_guard(State4, state__is_in_guard(State)),
  State6 = state__new_constraint_context(State5),
  state__store_conj(mk_conj_constraint_list([OldCs, NewCs]), State6).

%%=============================================================================
%%
%%  BIF constraints
%%
%%=============================================================================

get_bif_constr({erlang, Op, 2}, Dst, Args = [Arg1, Arg2], _State)
  when Op =:= '+'; Op =:= '-'; Op =:= '*' ->
  ReturnType = ?mk_fun_var(fun(Map) ->
			       TmpArgTypes = lookup_type_list(Args, Map),
			       bif_return(erlang, Op, 2, TmpArgTypes)
			   end, Args),
  ArgFun =
    fun(A, Pos) ->
	F =
	  fun(Map) ->
	      DstType = lookup_type(Dst, Map),
	      AType = lookup_type(A, Map),
	      case t_is_integer(DstType) of
		true ->
		  case t_is_integer(AType) of
		    true ->
		      eval_inv_arith(Op, Pos, DstType, AType);
		    false  ->
		      %% This must be temporary.
		      t_integer()
		  end;
		false ->
		  case t_is_float(DstType) of
		    true ->
		      case t_is_integer(AType) of
			true -> t_float();
			false -> t_number()
		      end;
		    false ->
		      t_number()
		  end
	      end
	  end,
	?mk_fun_var(F, [Dst, A])
    end,
  Arg1FunVar = ArgFun(Arg2, 2),
  Arg2FunVar = ArgFun(Arg1, 1),
  mk_conj_constraint_list([mk_constraint(Dst, sub, ReturnType),
			   mk_constraint(Arg1, sub, Arg1FunVar),
			   mk_constraint(Arg2, sub, Arg2FunVar)]);
get_bif_constr({erlang, Op, 2}, Dst, [Arg1, Arg2] = Args, _State)
  when Op =:= '<'; Op =:= '=<'; Op =:= '>'; Op =:= '>=' ->
  ArgFun =
    fun(LocalArg1, LocalArg2, LocalOp) ->
	fun(Map) ->
	    DstType = lookup_type(Dst, Map),
	    IsTrue = t_is_any_atom(true, DstType),
	    IsFalse = t_is_any_atom(false, DstType),
	    case IsTrue orelse IsFalse of
	      true ->
		Arg1Type = lookup_type(LocalArg1, Map),
		Arg2Type = lookup_type(LocalArg2, Map),
		case t_is_integer(Arg1Type) andalso t_is_integer(Arg2Type) of
		  true ->
		    Max1 = erl_types:number_max(Arg1Type),
		    Min1 = erl_types:number_min(Arg1Type),
		    Max2 = erl_types:number_max(Arg2Type),
		    Min2 = erl_types:number_min(Arg2Type),
		    case LocalOp of
		      '=<' ->
			if IsTrue  -> t_from_range(Min1, Max2);
			   IsFalse -> t_from_range(range_inc(Min2), Max1)
			end;
		      '<'  ->
			if IsTrue  -> t_from_range(Min1, range_dec(Max2));
			   IsFalse -> t_from_range(Min2, Max1)
			end;
		      '>=' ->
			if IsTrue  -> t_from_range(Min2, Max1);
			   IsFalse -> t_from_range(Min1, range_dec(Max2))
			end;
		      '>'  ->
			if IsTrue  -> t_from_range(range_inc(Min2), Max1);
			   IsFalse -> t_from_range(Min1, Max2)
			end
		    end;
		  false -> t_any()
		end;
	      false -> t_any()
	    end
	end
    end,
  {Arg1Fun, Arg2Fun} =
    case Op of
      '<'  -> {ArgFun(Arg1, Arg2, '<'),  ArgFun(Arg2, Arg1, '>=')};
      '=<' -> {ArgFun(Arg1, Arg2, '=<'), ArgFun(Arg2, Arg1, '>=')};
      '>'  -> {ArgFun(Arg1, Arg2, '>'),  ArgFun(Arg2, Arg1, '<')};
      '>=' -> {ArgFun(Arg1, Arg2, '>='), ArgFun(Arg2, Arg1, '=<')}
    end,
  DstArgs = [Dst, Arg1, Arg2],
  Arg1Var = ?mk_fun_var(Arg1Fun, DstArgs),
  Arg2Var = ?mk_fun_var(Arg2Fun, DstArgs),
  DstVar = ?mk_fun_var(fun(Map) ->
			   TmpArgTypes = lookup_type_list(Args, Map),
			   bif_return(erlang, Op, 2, TmpArgTypes)
		       end, Args),
  mk_conj_constraint_list([mk_constraint(Dst, sub, DstVar),
			   mk_constraint(Arg1, sub, Arg1Var),
			   mk_constraint(Arg2, sub, Arg2Var)]);
get_bif_constr({erlang, '++', 2}, Dst, [Hd, Tl] = Args, _State) ->
  HdFun = fun(Map) ->
	      DstType = lookup_type(Dst, Map),
	      case t_is_cons(DstType) of
		true -> t_list(t_cons_hd(DstType));
		false ->
		  case t_is_list(DstType) of
		    true ->
		      case t_is_nil(DstType) of
			true -> DstType;
			false -> t_list(t_list_elements(DstType))
		      end;
		    false -> t_list()
 		  end
	      end
	  end,
  TlFun = fun(Map) ->
	      DstType = lookup_type(Dst, Map),
	      case t_is_cons(DstType) of
		true -> t_sup(t_cons_tl(DstType), DstType);
		false ->
		  case t_is_list(DstType) of
		    true ->
		      case t_is_nil(DstType) of
			true -> DstType;
			false -> t_list(t_list_elements(DstType))
		      end;
		    false -> t_any()
		  end
	      end
	  end,
  DstL = [Dst],
  HdVar = ?mk_fun_var(HdFun, DstL),
  TlVar = ?mk_fun_var(TlFun, DstL),
  ArgTypes = erl_bif_types:arg_types(erlang, '++', 2),
  ReturnType = ?mk_fun_var(fun(Map) ->
			       TmpArgTypes = lookup_type_list(Args, Map),
			       bif_return(erlang, '++', 2, TmpArgTypes)
			   end, Args),
  Cs = mk_constraints(Args, sub, ArgTypes),
  mk_conj_constraint_list([mk_constraint(Dst, sub, ReturnType),
			   mk_constraint(Hd, sub, HdVar),
			   mk_constraint(Tl, sub, TlVar)
			   |Cs]);
get_bif_constr({erlang, is_atom, 1}, Dst, [Arg], State) ->
  get_bif_test_constr(Dst, Arg, t_atom(), State);
get_bif_constr({erlang, is_binary, 1}, Dst, [Arg], State) ->
  get_bif_test_constr(Dst, Arg, t_binary(), State);
get_bif_constr({erlang, is_bitstring, 1}, Dst, [Arg], State) ->
  get_bif_test_constr(Dst, Arg, t_bitstr(), State);
get_bif_constr({erlang, is_boolean, 1}, Dst, [Arg], State) ->
  get_bif_test_constr(Dst, Arg, t_boolean(), State);
get_bif_constr({erlang, is_float, 1}, Dst, [Arg], State) ->
  get_bif_test_constr(Dst, Arg, t_float(), State);
get_bif_constr({erlang, is_function, 1}, Dst, [Arg], State) ->
  get_bif_test_constr(Dst, Arg, t_fun(), State);
get_bif_constr({erlang, is_function, 2}, Dst, [Fun, Arity], _State) ->
  ArgFun = fun(Map) ->
	       DstType = lookup_type(Dst, Map),
	       case t_is_any_atom(true, DstType) of
		 true ->
		   ArityType = lookup_type(Arity, Map),
		   case t_number_vals(ArityType) of
		     unknown -> t_fun();
		     Vals -> t_sup([t_fun(X, t_any()) || X <- Vals])
		   end;
		 false -> t_any()
	       end
	   end,
  ArgV = ?mk_fun_var(ArgFun, [Dst, Arity]),
  mk_conj_constraint_list([mk_constraint(Dst, sub, t_boolean()),
			   mk_constraint(Arity, sub, t_integer()),
			   mk_constraint(Fun, sub, ArgV)]);
get_bif_constr({erlang, is_integer, 1}, Dst, [Arg], State) ->
  get_bif_test_constr(Dst, Arg, t_integer(), State);
get_bif_constr({erlang, is_list, 1}, Dst, [Arg], State) ->
  get_bif_test_constr(Dst, Arg, t_maybe_improper_list(), State);
get_bif_constr({erlang, is_map, 1}, Dst, [Arg], State) ->
  get_bif_test_constr(Dst, Arg, t_map(), State);
get_bif_constr({erlang, is_number, 1}, Dst, [Arg], State) ->
  get_bif_test_constr(Dst, Arg, t_number(), State);
get_bif_constr({erlang, is_pid, 1}, Dst, [Arg], State) ->
  get_bif_test_constr(Dst, Arg, t_pid(), State);
get_bif_constr({erlang, is_port, 1}, Dst, [Arg], State) ->
  get_bif_test_constr(Dst, Arg, t_port(), State);
get_bif_constr({erlang, is_reference, 1}, Dst, [Arg], State) ->
  get_bif_test_constr(Dst, Arg, t_reference(), State);
get_bif_constr({erlang, is_record, 2}, Dst, [Var, Tag] = Args, _State) ->
  ArgFun = fun(Map) ->
	       case t_is_any_atom(true, lookup_type(Dst, Map)) of
		 true -> t_tuple();
		 false -> t_any()
	       end
	   end,
  ArgV = ?mk_fun_var(ArgFun, [Dst]),
  DstFun = fun(Map) ->
	       TmpArgTypes = lookup_type_list(Args, Map),
	       bif_return(erlang, is_record, 2, TmpArgTypes)
	   end,
  DstV = ?mk_fun_var(DstFun, Args),
  mk_conj_constraint_list([mk_constraint(Dst, sub, DstV),
			   mk_constraint(Tag, sub, t_atom()),
			   mk_constraint(Var, sub, ArgV)]);
get_bif_constr({erlang, is_record, 3}, Dst, [Var, Tag, Arity] = Args, State) ->
  %% TODO: Revise this to make it precise for Tag and Arity.
  ArgFun =
    fun(Map) ->
	case t_is_any_atom(true, lookup_type(Dst, Map)) of
	  true ->
	    ArityType = lookup_type(Arity, Map),
	    case t_is_integer(ArityType) of
	      true ->
		case t_number_vals(ArityType) of
		  [ArityVal] ->
		    TagType = lookup_type(Tag, Map),
		    case t_is_atom(TagType) of
		      true ->
			AnyElems = lists:duplicate(ArityVal-1, t_any()),
			GenRecord = t_tuple([TagType|AnyElems]),
			case t_atom_vals(TagType) of
			  [TagVal] ->
			    case lookup_record(State, TagVal, ArityVal - 1) of
			      {ok, Type, _NewState} ->
                                Type;
			      {error, _NewState} -> GenRecord
			    end;
			  _ -> GenRecord
			end;
		      false -> t_tuple(ArityVal)
		    end;
		  _ -> t_tuple()
		end;
	      false -> t_tuple()
	    end;
	  false -> t_any()
	end
    end,
  ArgV = ?mk_fun_var(ArgFun, [Tag, Arity, Dst]),
  DstFun = fun(Map) ->
	       [TmpVar, TmpTag, TmpArity] = lookup_type_list(Args, Map),
               TmpArgTypes = [TmpVar,TmpTag,TmpArity],
	       bif_return(erlang, is_record, 3, TmpArgTypes)
	   end,
  DstV = ?mk_fun_var(DstFun, Args),
  mk_conj_constraint_list([mk_constraint(Dst, sub, DstV),
			   mk_constraint(Arity, sub, t_integer()),
			   mk_constraint(Tag, sub, t_atom()),
			   mk_constraint(Var, sub, ArgV)]);
get_bif_constr({erlang, is_tuple, 1}, Dst, [Arg], State) ->
  get_bif_test_constr(Dst, Arg, t_tuple(), State);
get_bif_constr({erlang, 'and', 2}, Dst, [Arg1, Arg2] = Args, _State) ->
  True = t_from_term(true),
  False = t_from_term(false),
  ArgFun = fun(Var) ->
	       fun(Map) ->
		   DstType = lookup_type(Dst, Map),
		   case t_is_any_atom(true, DstType) of
		     true -> True;
		     false ->
		       case t_is_any_atom(false, DstType) of
			 true ->
			   case
                             t_is_any_atom(true, lookup_type(Var, Map))
                           of
			     true -> False;
			     false -> t_boolean()
			   end;
			 false ->
			   t_boolean()
		       end
		   end
	       end
	   end,
  DstFun = fun(Map) ->
	       Arg1Type = lookup_type(Arg1, Map),
	       case t_is_any_atom(false, Arg1Type) of
		 true -> False;
		 false ->
		   Arg2Type = lookup_type(Arg2, Map),
		   case t_is_any_atom(false, Arg2Type) of
		     true -> False;
		     false ->
		       case (t_is_any_atom(true, Arg1Type)
			     andalso t_is_any_atom(true, Arg2Type)) of
			 true -> True;
			 false -> t_boolean()
		       end
		   end
	       end
	   end,
  ArgV1 = ?mk_fun_var(ArgFun(Arg2), [Arg2, Dst]),
  ArgV2 = ?mk_fun_var(ArgFun(Arg1), [Arg1, Dst]),
  DstV = ?mk_fun_var(DstFun, Args),
  mk_conj_constraint_list([mk_constraint(Dst, sub, DstV),
			   mk_constraint(Arg1, sub, ArgV1),
			   mk_constraint(Arg2, sub, ArgV2)]);
get_bif_constr({erlang, 'or', 2}, Dst, [Arg1, Arg2] = Args, _State) ->
  True = t_from_term(true),
  False = t_from_term(false),
  ArgFun = fun(Var) ->
	       fun(Map) ->
		   DstType = lookup_type(Dst, Map),
		   case t_is_any_atom(false, DstType) of
		     true -> False;
		     false ->
		       case t_is_any_atom(true, DstType) of
			 true ->
			   case
                             t_is_any_atom(false, lookup_type(Var, Map))
                           of
			     true -> True;
			     false -> t_boolean()
			   end;
			 false ->
			   t_boolean()
		       end
		   end
	       end
	   end,
  DstFun = fun(Map) ->
	       Arg1Type = lookup_type(Arg1, Map),
	       case t_is_any_atom(true, Arg1Type) of
		 true -> True;
		 false ->
		   Arg2Type = lookup_type(Arg2, Map),
		   case t_is_any_atom(true, Arg2Type) of
		     true -> True;
		     false ->
		       case (t_is_any_atom(false, Arg1Type)
			     andalso t_is_any_atom(false, Arg2Type)) of
			 true -> False;
			 false -> t_boolean()
		       end
		   end
	       end
	   end,
  ArgV1 = ?mk_fun_var(ArgFun(Arg2), [Arg2, Dst]),
  ArgV2 = ?mk_fun_var(ArgFun(Arg1), [Arg1, Dst]),
  DstV = ?mk_fun_var(DstFun, Args),
  F = fun(A) ->
	  try [mk_constraint(A, sub, True)]
	  catch throw:error -> []
	  end
      end,
  Constrs = F(Arg1) ++ F(Arg2),
  Disj = mk_disj_constraint_list([mk_constraint(Dst, sub, False)|Constrs]),
  mk_conj_constraint_list([mk_constraint(Dst, sub, DstV),
			   mk_constraint(Arg1, sub, ArgV1),
			   mk_constraint(Arg2, sub, ArgV2),
			   Disj]);
get_bif_constr({erlang, 'not', 1}, Dst, [Arg] = Args, _State) ->
  True = t_from_term(true),
  False = t_from_term(false),
  Fun = fun(Var) ->
	    fun(Map) ->
		Type = lookup_type(Var, Map),
		case t_is_any_atom(true, Type) of
		  true -> False;
		  false ->
		    case t_is_any_atom(false, Type) of
		      true -> True;
		      false -> t_boolean()
		    end
		end
	    end
	end,
  ArgV = ?mk_fun_var(Fun(Dst), [Dst]),
  DstV = ?mk_fun_var(Fun(Arg), Args),
  mk_conj_constraint_list([mk_constraint(Arg, sub, ArgV),
			   mk_constraint(Dst, sub, DstV)]);
get_bif_constr({erlang, '=:=', 2}, Dst, [Arg1, Arg2] = Args, _State) ->
  ArgFun =
    fun(Self, OtherVar) ->
	fun(Map) ->
	    DstType = lookup_type(Dst, Map),
	    OtherVarType = lookup_type(OtherVar, Map),
	    case t_is_any_atom(true, DstType) of
	      true -> OtherVarType;
	      false ->
		case t_is_any_atom(false, DstType) of
		  true ->
		    case is_singleton_type(OtherVarType) of
		      true -> t_subtract(lookup_type(Self, Map), OtherVarType);
		      false -> t_any()
		    end;
		  false ->
		    t_any()
		end
	    end
	end
    end,
  DstFun = fun(Map) ->
	       ArgType1 = lookup_type(Arg1, Map),
	       ArgType2 = lookup_type(Arg2, Map),
	       case t_is_none(t_inf(ArgType1, ArgType2)) of
		 true -> t_from_term(false);
		 false -> t_boolean()
	       end
	   end,
  DstArgs = [Dst, Arg1, Arg2],
  ArgV1 = ?mk_fun_var(ArgFun(Arg1, Arg2), DstArgs),
  ArgV2 = ?mk_fun_var(ArgFun(Arg2, Arg1), DstArgs),
  DstV = ?mk_fun_var(DstFun, Args),
  mk_conj_constraint_list([mk_constraint(Dst, sub, DstV),
			   mk_constraint(Arg1, sub, ArgV1),
			   mk_constraint(Arg2, sub, ArgV2)]);
get_bif_constr({erlang, '==', 2}, Dst, [Arg1, Arg2] = Args, _State) ->
  DstFun = fun(Map) ->
	       TmpArgTypes = lookup_type_list(Args, Map),
	       bif_return(erlang, '==', 2, TmpArgTypes)
	   end,
  ArgFun =
    fun(Var, Self) ->
	fun(Map) ->
	    VarType = lookup_type(Var, Map),
	    DstType = lookup_type(Dst, Map),
	    case is_singleton_non_number_type(VarType) of
	      true ->
		case t_is_any_atom(true, DstType) of
		  true -> VarType;
		  false ->
		    case t_is_any_atom(false, DstType) of
		      true -> t_subtract(lookup_type(Self, Map), VarType);
		      false -> t_any()
		    end
		end;
	      false ->
		case t_is_any_atom(true, DstType) of
		  true ->
		    case t_is_number(VarType) of
		      true -> t_number();
		      false ->
			case t_is_atom(VarType) of
			  true -> VarType;
			  false -> t_any()
			end
		    end;
		  false ->
		    t_any()
		end
	    end
	end
    end,
  DstV = ?mk_fun_var(DstFun, Args),
  ArgL = [Arg1, Arg2, Dst],
  ArgV1 = ?mk_fun_var(ArgFun(Arg2, Arg1), ArgL),
  ArgV2 = ?mk_fun_var(ArgFun(Arg1, Arg2), ArgL),
  mk_conj_constraint_list([mk_constraint(Dst, sub, DstV),
			   mk_constraint(Arg1, sub, ArgV1),
			   mk_constraint(Arg2, sub, ArgV2)]);
get_bif_constr({erlang, element, 2} = _BIF, Dst, Args,
               #state{cs = Constrs}) ->
  GenType = erl_bif_types:type(erlang, element, 2),
  case t_is_none(GenType) of
    true -> ?debug("Bif: ~w failed\n", [_BIF]), throw(error);
    false ->
      Fun = fun(Map) ->
		ATs2 = lookup_type_list(Args, Map),
		bif_return(erlang, element, 2, ATs2)
	    end,
      ReturnType = ?mk_fun_var(Fun, Args),
      ArgTypes = erl_bif_types:arg_types(erlang, element, 2),
      Cs = mk_constraints(Args, sub, ArgTypes),
      NewCs =
        case find_element(Args, Constrs) of
          'unknown' -> Cs;
          Elem -> [mk_constraint(Dst, eq, Elem)|Cs]
        end,
      mk_conj_constraint_list([mk_constraint(Dst, sub, ReturnType)|NewCs])
  end;
get_bif_constr({M, F, A} = _BIF, Dst, Args, _State) ->
  GenType = erl_bif_types:type(M, F, A),
  case t_is_none(GenType) of
    true -> ?debug("Bif: ~w failed\n", [_BIF]), throw(error);
    false ->
      ReturnType = ?mk_fun_var(fun(Map) ->
                                  TmpArgTypes = lookup_type_list(Args, Map),
                                  bif_return(M, F, A, TmpArgTypes)
			      end, Args),
      case erl_bif_types:is_known(M, F, A) of
	false ->
	  case t_is_any(GenType) of
	    true ->
	      none;
	    false ->
	      mk_constraint(Dst, sub, ReturnType)
	  end;
	true ->
	  ArgTypes = erl_bif_types:arg_types(M, F, A),
	  Cs = mk_constraints(Args, sub, ArgTypes),
	  mk_conj_constraint_list([mk_constraint(Dst, sub, ReturnType)|Cs])
      end
  end.

eval_inv_arith('+', _Pos, Dst, Arg) ->
  bif_return(erlang, '-', 2, [Dst, Arg]);
eval_inv_arith('*', _Pos, Dst, Arg) ->
  Zero = t_from_term(0),
  case t_is_none(t_inf(Arg, Zero)) of
    false -> t_integer();
    true ->
      TmpRet = bif_return(erlang, 'div', 2, [Dst, Arg]),
      %% If 0 is not part of the result, it cannot be part of the argument.
      case t_is_subtype(Zero, Dst) of
	false -> t_subtract(TmpRet, Zero);
	true -> TmpRet
      end
  end;
eval_inv_arith('-', 1, Dst, Arg) ->
  bif_return(erlang, '-', 2, [Arg, Dst]);
eval_inv_arith('-', 2, Dst, Arg) ->
  bif_return(erlang, '+', 2, [Arg, Dst]).

range_inc(neg_inf) -> neg_inf;
range_inc(pos_inf) -> pos_inf;
range_inc(Int) when is_integer(Int) -> Int + 1.

range_dec(neg_inf) -> neg_inf;
range_dec(pos_inf) -> pos_inf;
range_dec(Int) when is_integer(Int) -> Int - 1.

get_bif_test_constr(Dst, Arg, Type, _State) ->
  ArgFun = fun(Map) ->
	       DstType = lookup_type(Dst, Map),
	       case t_is_any_atom(true, DstType) of
		 true -> Type;
		 false -> t_any()
	       end
	   end,
  ArgV = ?mk_fun_var(ArgFun, [Dst]),
  DstFun = fun(Map) ->
	       ArgType = lookup_type(Arg, Map),
	       case t_is_none(t_inf(ArgType, Type)) of
		 true ->
                   t_from_term(false);
		 false ->
		   case t_is_subtype(ArgType, Type) of
		     true -> t_from_term(true);
		     false -> t_boolean()
		   end
	       end
	   end,
  DstV = ?mk_fun_var(DstFun, [Arg]),
  mk_conj_constraint_list([mk_constraint(Dst, sub, DstV),
			   mk_constraint(Arg, sub, ArgV)]).

%%=============================================================================
%%
%%  Constraint solver.
%%
%%=============================================================================

solve([Fun], State) ->
  ?debug("============ Analyzing Fun: ~tw ===========\n",
	 [debug_lookup_name(Fun)]),
  solve_fun(Fun, map_new(), State);
solve([_|_] = SCC, State) ->
  ?debug("============ Analyzing SCC: ~tw ===========\n",
	 [[debug_lookup_name(F) || F <- SCC]]),
  Users = comp_users(SCC, State),
  solve_scc(SCC, map_new(), State, Users, _ToSolve=SCC, false).

comp_users(SCC, State) ->
  Vars0 = [{Fun, state__get_rec_var(Fun, State)} || Fun <- SCC],
  Vars = lists:sort([t_var_name(Var) || {_, {ok, Var}} <- Vars0]),
  family([{t_var(V), F} ||
           F <- SCC,
           V <- ordsets:intersection(get_deps(state__get_cs(F, State)),
                                     Vars)]).

solve_fun(Fun, FunMap, State) ->
  Cs = state__get_cs(Fun, State),
  Deps = get_deps(Cs),
  Ref = mk_constraint_ref(Fun, Deps),
  %% Note that functions are always considered to succeed.
  NewMap = solve(Fun, Ref, FunMap, State),
  NewType = lookup_type(Fun, NewMap),
  NewFunMap1 = case state__get_rec_var(Fun, State) of
		 error -> FunMap;
		 {ok, Var} -> enter_type(Var, NewType, FunMap)
	       end,
  enter_type(Fun, NewType, NewFunMap1).

solve_scc(SCC, Map, State, Users, ToSolve, TryingUnit) ->
  Vars0 = [{Fun, state__get_rec_var(Fun, State)} || Fun <- SCC],
  Vars = [Var || {_, {ok, Var}} <- Vars0],
  Funs = [Fun || {Fun, {ok, _}} <- Vars0],
  Types = unsafe_lookup_type_list(Funs, Map),
  RecTypes = [t_limit(Type, ?TYPE_LIMIT) || Type <- Types],
  CleanMap = lists:foldl(fun(Fun, AccFunMap) ->
			     erase_type(t_var_name(Fun), AccFunMap)
			 end, Map, ToSolve),
  Map1 = enter_type_lists(Vars, RecTypes, CleanMap),
  ?debug("Checking SCC: ~tw\n", [[debug_lookup_name(F) || F <- SCC]]),
  SolveFun = fun(X, Y) -> scc_fold_fun(X, Y, State) end,
  Map2 = lists:foldl(SolveFun, Map1, ToSolve),
  Updated = updated_vars_only(Vars, Map, Map2),
  case Updated =:= [] of
    true ->
      ?debug("SCC ~tw reached fixpoint\n", [SCC]),
      NewTypes = unsafe_lookup_type_list(Funs, Map2),
      case erl_types:any_none([t_fun_range(T) || T <- NewTypes])
	andalso TryingUnit =:= false of
	true ->
	  UnitTypes =
	    [case t_is_none(t_fun_range(T)) of
	       false -> T;
	       true -> t_fun(t_fun_args(T), t_unit())
	     end || T <- NewTypes],
	  Map3 = enter_type_lists(Funs, UnitTypes, Map2),
	  solve_scc(SCC, Map3, State, Users, SCC, true);
	false ->
	  Map2
      end;
    false ->
      ?debug("SCC ~tw did not reach fixpoint\n", [SCC]),
      ToSolve1 = affected(Updated, Users),
      solve_scc(SCC, Map2, State, Users, ToSolve1, TryingUnit)
  end.

affected(Updated, Users) ->
  lists:umerge([case lists:keyfind(V, 1, Users) of
                  {V, Vs} -> Vs;
                  false -> []
                end || V <- Updated]).

scc_fold_fun(F, FunMap, State) ->
  Deps = get_deps(state__get_cs(F, State)),
  Cs = mk_constraint_ref(F, Deps),
  %% Note that functions are always considered to succeed.
  Map = solve(F, Cs, FunMap, State),
  NewType0 = unsafe_lookup_type(F, Map),
  NewType = t_limit(NewType0, ?TYPE_LIMIT),
  NewFunMap = case state__get_rec_var(F, State) of
		{ok, R} ->
		  enter_type(R, NewType, enter_type(F, NewType, FunMap));
		error ->
		  enter_type(F, NewType, FunMap)
	      end,
  ?debug("Done solving for function ~tw :: ~ts\n", [debug_lookup_name(F),
                                                    format_type(NewType)]),
  NewFunMap.

solve(Fun, Cs, FunMap, State) ->
  Solvers = State#state.solvers,
  R = [solver(S, solve_fun(S, Fun, Cs, FunMap, State)) || S <- Solvers],
  check_solutions(R, Fun, no_solver, no_map).

solver(Solver, SolveFun) ->
  ?debug("Start solver ~w\n", [Solver]),
  try timer:tc(SolveFun) of
    {Time, {ok, Map}} ->
      ?debug("End solver ~w (~w microsecs)\n", [Solver, Time]),
      {Solver, Map, Time};
    {_, _R} ->
      ?debug("Solver ~w returned unexpected result:\n  ~P\n",
             [Solver, _R, 60]),
      throw(error)
  catch E:R:S ->
      io:format("Solver ~w failed: ~w:~p\n ~tp\n", [Solver, E, R, S]),
      throw(error)
  end.

solve_fun(v1, _Fun, Cs, FunMap, State) ->
  fun() ->
      {ok, _MapDict, NewMap} = solve_ref_or_list(Cs, FunMap, map_new(), State),
      {ok, NewMap}
  end;
solve_fun(v2, Fun, _Cs, FunMap, State) ->
  fun() -> v2_solve_ref(Fun, FunMap, State) end.

check_solutions([], _Fun, _S, Map) ->
  Map;
check_solutions([{S1,Map1,_Time1}|Maps], Fun, S, Map) ->
  ?debug("Solver ~w needed ~w microsecs\n", [S1, _Time1]),
  case Map =:= no_map orelse sane_maps(Map, Map1, [Fun], S, S1) of
    true ->
      check_solutions(Maps, Fun, S1, Map1);
    false ->
      ?debug("Constraint solvers do not agree on ~w\n", [Fun]),
      ?pp_map(atom_to_list(S), Map),
      ?pp_map(atom_to_list(S1), Map1),
      io:format("A bug was found. Please report it, and use the option "
                "`--solver v1' until the bug has been fixed.\n"),
      throw(error)
  end.

sane_maps(Map1, Map2, Keys, _S1, _S2) ->
  lists:all(fun(Key) ->
                V1 = unsafe_lookup_type(Key, Map1),
                V2 = unsafe_lookup_type(Key, Map2),
                case t_is_equal(V1, V2) of
                  true -> true;
                  false ->
                    ?debug("Constraint solvers do not agree on ~w\n", [Key]),
                    ?debug("~w: ~ts\n",
                           [_S1, format_type(unsafe_lookup_type(Key, Map1))]),
                    ?debug("~w: ~ts\n",
                           [_S2, format_type(unsafe_lookup_type(Key, Map2))]),
                    false
                end
            end, Keys).

%% Solver v2

-record(v2_state, {constr_data = maps:new() :: map(),
		   state :: state()}).

v2_solve_ref(Fun, Map, State) ->
  V2State = #v2_state{state = State},
  {ok, NewMap, _, _} = v2_solve_reference(Fun, Map, V2State),
  {ok, NewMap}.

v2_solve(#constraint{}=C, Map, V2State) ->
  case solve_one_c(C, Map) of
    error ->
      report_failed_constraint(C, Map),
      {error, V2State};
    {ok, {NewMap, U}} ->
      {ok, NewMap, V2State, U}
  end;
v2_solve(#constraint_list{type = disj}=C, Map, V2State) ->
  v2_solve_disjunct(C, Map, V2State);
v2_solve(#constraint_list{type = conj}=C, Map, V2State) ->
  v2_solve_conjunct(C, Map, V2State);
v2_solve(#constraint_ref{id = Id}, Map, V2State) ->
  v2_solve_reference(Id, Map, V2State).

v2_solve_reference(Id, Map, V2State0) ->
  ?debug("Checking ref to fun: ~tw\n", [debug_lookup_name(Id)]),
  ?pp_map("Map", Map),
  pp_constr_data("solve_ref", V2State0),
  Map1 = restore_local_map(V2State0, Id, Map),
  State = V2State0#v2_state.state,
  Cs = state__get_cs(Id, State),
  Res =
    case state__is_self_rec(Id, State) of
      true -> v2_solve_self_recursive(Cs, Map1, Id, t_none(), V2State0);
      false -> v2_solve(Cs, Map1, V2State0)
    end,
  {FunType, V2State} =
    case Res of
      {error, V2State1} ->
        ?debug("Error solving for function ~tp\n", [debug_lookup_name(Id)]),
        Arity = state__fun_arity(Id, State),
        FunType0 =
          case state__prop_domain(t_var_name(Id), State) of
            error -> t_fun(Arity, t_none());
            {ok, Dom} -> t_fun(Dom, t_none())
          end,
        {FunType0, V2State1};
      {ok, NewMap, V2State1, U} ->
        ?debug("Done solving fun: ~tp\n", [debug_lookup_name(Id)]),
        FunType0 = lookup_type(Id, NewMap),
        V2State2 = save_local_map(V2State1, Id, U, NewMap),
        {FunType0, V2State2}
    end,
  ?debug("ref Id=~w Assigned ~ts\n", [Id, format_type(FunType)]),
  {NewMap1, U1} = enter_var_type(Id, FunType, Map),
  {NewMap2, U2} =
    case state__get_rec_var(Id, State) of
      {ok, Var} -> enter_var_type(Var, FunType, NewMap1);
      error -> {NewMap1, []}
    end,
  {ok, NewMap2, V2State, lists:umerge(U1, U2)}.

v2_solve_self_recursive(Cs, Map, Id, RecType0, V2State0) ->
  ?debug("Solving self recursive ~tw\n", [debug_lookup_name(Id)]),
  State = V2State0#v2_state.state,
  {ok, RecVar} = state__get_rec_var(Id, State),
  ?debug("OldRecType ~ts\n", [format_type(RecType0)]),
  RecType = t_limit(RecType0, ?TYPE_LIMIT),
  {Map1, U0} = enter_var_type(RecVar, RecType, Map),
  V2State1 = save_updated_vars1(V2State0, Cs, U0), % Probably not necessary
  case v2_solve(Cs, Map1, V2State1) of
    {error, _V2State}=Error ->
      case t_is_none(RecType0) of
	true ->
	  %% Try again and assume that this is a non-terminating function.
	  Arity = state__fun_arity(Id, State),
	  NewRecType = t_fun(lists:duplicate(Arity, t_any()), t_unit()),
	  v2_solve_self_recursive(Cs, Map, Id, NewRecType, V2State0);
	false ->
	  Error
      end;
    {ok, NewMap, V2State, U} ->
      ?pp_map("recursive finished", NewMap),
      NewRecType = unsafe_lookup_type(Id, NewMap),
      case is_equal(NewRecType, RecType0) of
	true ->
          {NewMap2, U1} = enter_var_type(RecVar, NewRecType, NewMap),
	  {ok, NewMap2, V2State, lists:umerge(U, U1)};
	false ->
	  v2_solve_self_recursive(Cs, Map, Id, NewRecType, V2State0)
      end
  end.

enter_var_type(Var, Type, Map0) ->
  {Map, Vs} = enter_type2(Var, Type, Map0),
  {Map, [t_var_name(V) || V <- Vs]}.

v2_solve_disjunct(Disj, Map, V2State0) ->
  #constraint_list{type = disj, id = _Id, list = Cs, masks = Masks} = Disj,
  ?debug("disjunct Id=~w~n", [_Id]),
  ?pp_map("Map", Map),
  pp_constr_data("disjunct", V2State0),
  case get_flags(V2State0, Disj) of
    {V2State1, failed_list} -> {error, V2State1}; % cannot happen
    {V2State1, Flags} when Flags =/= [] ->
      {ok, V2State, Eval, UL, MapL0, Uneval, Failed} =
        v2_solve_disj(Flags, Cs, 1, Map, V2State1, [], [], [], [], false),
      ?debug("disj ending _Id=~w Eval=~w, |Uneval|=~w |UL|=~w~n",
             [_Id, Eval, length(Uneval), length(UL)]),
      if Eval =:= [], Uneval =:= [] ->
           {error, failed_list(Disj, V2State0)};
         true ->
           {Is0, UnIds} = lists:unzip(Uneval),
           MapL = [restore_local_map(V2State, Id, Map) ||
                    Id <- UnIds] ++ MapL0,
           %% If some branch has just failed every variable of the
           %% non-failed branches need to be checked, not just the
           %% updated ones.
           U0 = case Failed of
                  false -> lists:umerge(UL);
                  true -> constrained_keys(MapL)
                end,
           if U0 =:= [] -> {ok, Map, V2State, []};
              true ->
                NotFailed = lists:umerge(Is0, Eval),
                U1 = [V || V <- U0,
                           var_occurs_everywhere(V, Masks, NotFailed)],
                NewMap = join_maps(U1, MapL, Map),
                ?pp_map("NewMap", NewMap),
                U = updated_vars_only(U1, Map, NewMap),
                ?debug("disjunct finished _Id=~w\n", [_Id]),
                {ok, NewMap, V2State, U}
           end
      end
  end.

var_occurs_everywhere(V, Masks, NotFailed) ->
  ordsets:is_subset(NotFailed, get_mask(V, Masks)).

v2_solve_disj([I|Is], [C|Cs], I, Map0, V2State0, UL, MapL, Eval, Uneval,
              Failed0) ->
  Id = C#constraint_list.id,
  Map1 = restore_local_map(V2State0, Id, Map0),
  case v2_solve(C, Map1, V2State0) of
    {error, V2State} ->
      ?debug("disj error I=~w~n", [I]),
      Failed = Failed0 orelse not is_failed_list(C, V2State0),
      v2_solve_disj(Is, Cs, I+1, Map0, V2State, UL, MapL, Eval, Uneval, Failed);
    {ok, Map, V2State1, U} ->
      ?debug("disj I=~w U=~w~n", [I, U]),
      V2State = save_local_map(V2State1, Id, U, Map),
      ?pp_map("DMap", Map),
      v2_solve_disj(Is, Cs, I+1, Map0, V2State, [U|UL], [Map|MapL],
                    [I|Eval], Uneval, Failed0)
  end;
v2_solve_disj([], [], _I, _Map, V2State, UL, MapL, Eval, Uneval, Failed) ->
  {ok, V2State, lists:reverse(Eval), UL, MapL, lists:reverse(Uneval), Failed};
v2_solve_disj([every_i], Cs, I, Map, V2State, UL, MapL, Eval, Uneval, Failed) ->
  NewIs = case Cs of
            [] -> [];
            _ -> [I, every_i]
          end,
  v2_solve_disj(NewIs, Cs, I, Map, V2State, UL, MapL, Eval, Uneval, Failed);
v2_solve_disj(Is, [C|Cs], I, Map, V2State, UL, MapL, Eval, Uneval0, Failed) ->
  Uneval = [{I,C#constraint_list.id} ||
             not is_failed_list(C, V2State)] ++ Uneval0,
  v2_solve_disj(Is, Cs, I+1, Map, V2State, UL, MapL, Eval, Uneval, Failed).

save_local_map(#v2_state{constr_data = ConData}=V2State, Id, U, Map) ->
  Part0 = [{V,maps:get(V, Map)} || V <- U],
  Part1 =
    case maps:find(Id, ConData) of
      error -> []; % cannot happen
      {ok, {Part2,[]}} -> Part2
    end,
  ?debug("save local map Id=~w:\n", [Id]),
  Part = lists:ukeymerge(1, lists:keysort(1, Part0), Part1),
  ?pp_map("New Part", maps:from_list(Part0)),
  ?pp_map("Old Part", maps:from_list(Part1)),
  ?pp_map(" => Part", maps:from_list(Part)),
  V2State#v2_state{constr_data = maps:put(Id, {Part,[]}, ConData)}.

restore_local_map(#v2_state{constr_data = ConData}, Id, Map0) ->
  case maps:find(Id, ConData) of
    error -> Map0;
    {ok, failed} -> Map0;
    {ok, {[],_}} -> Map0;
    {ok, {Part0,U}} ->
      Part = [KV || {K,_V} = KV <- Part0, not lists:member(K, U)],
      ?debug("restore local map Id=~w U=~w\n", [Id, U]),
      ?pp_map("Part", maps:from_list(Part)),
      ?pp_map("Map0", Map0),
      Map = lists:foldl(fun({K,V}, D) -> maps:put(K, V, D) end, Map0, Part),
      ?pp_map("Map", Map),
      Map
  end.

v2_solve_conjunct(Conj, Map, V2State0) ->
  #constraint_list{type = conj, list = Cs} = Conj,
  ?debug("conjunct Id=~w~n", [Conj#constraint_list.id]),
  IsFlat = case Cs of [#constraint{}|_] -> true; _ -> false end,
  case get_flags(V2State0, Conj) of
    {V2State, failed_list} -> {error, V2State};
    {V2State, Flags} ->
      v2_solve_conj(Flags, Cs, 1, Map, Conj, IsFlat, V2State, [], [], [],
                    Map, Flags)
  end.

%% LastMap and LastFlags are used for loop detection.
v2_solve_conj([I|Is], [Cs|Tail], I, Map0, Conj, IsFlat, V2State0,
              UL, NewFs0, VarsUp, LastMap, LastFlags) ->
  ?debug("conj Id=~w I=~w~n", [Conj#constraint_list.id, I]),
  true = IsFlat =:= is_record(Cs, constraint),
  pp_constr_data("conj", V2State0),
  case v2_solve(Cs, Map0, V2State0) of
    {error, V2State1} -> {error, failed_list(Conj, V2State1)};
    {ok, Map, V2State1, []} ->
      v2_solve_conj(Is, Tail, I+1, Map, Conj, IsFlat, V2State1,
                    UL, NewFs0, VarsUp, LastMap, LastFlags);
    {ok, Map, V2State1, U} when IsFlat -> % optimization
      %% It is ensured by enumerate_constraints() that every
      %% #constraint{} has a conjunct as parent, and that such a
      %% parent has nothing but #constraint{}:s as children, a fact
      %% which is used here to simplify the flag calculation.
      Mask = lists:umerge([get_mask(V, Conj#constraint_list.masks) || V <- U]),
      {Is1, NewF} = add_mask_to_flags(Is, Mask, I, []),
      NewFs = [NewF|NewFs0],
      v2_solve_conj(Is1, Tail, I+1, Map, Conj, IsFlat, V2State1,
                    [U|UL], NewFs, VarsUp, LastMap, LastFlags);
    {ok, Map, V2State1, U} ->
      #constraint_list{masks = Masks, list = AllCs} = Conj,
      M = lists:keydelete(I, 1, vars_per_child(U, Masks)),
      {V2State2, NewF0} = save_updated_vars_list(AllCs, M, V2State1),
      {NewF, F} = lists:splitwith(fun(J) -> J < I end, NewF0),
      Is1 = umerge_mask(Is, F),
      NewFs = [NewF|NewFs0],
      v2_solve_conj(Is1, Tail, I+1, Map, Conj, IsFlat, V2State2,
                    [U|UL], NewFs, VarsUp, LastMap, LastFlags)
  end;
v2_solve_conj([], _Cs, _I, Map, Conj, IsFlat, V2State, UL, NewFs, VarsUp,
             LastMap, LastFlags) ->
  U = lists:umerge(UL),
  case lists:umerge(NewFs) of
    [] ->
      ?debug("conjunct finished Id=~w\n", [Conj#constraint_list.id]),
      {ok, Map, V2State, lists:umerge([U|VarsUp])};
    NewFlags when NewFlags =:= LastFlags, Map =:= LastMap ->
      %% A loop was detected! The cause is some bug, possibly in erl_types.
      %% The evaluation continues, but the results can be wrong.
      report_detected_loop(Conj),
      {ok, Map, V2State, lists:umerge([U|VarsUp])};
    NewFlags ->
      #constraint_list{type = conj, list = Cs} = Conj,
      v2_solve_conj(NewFlags, Cs, 1, Map, Conj, IsFlat, V2State,
                    [], [], [U|VarsUp], Map, NewFlags)
  end;
v2_solve_conj([every_i], Cs, I, Map, Conj, IsFlat, V2State, UL, NewFs, VarsUp,
              LastMap, LastFlags) ->
  NewIs = case Cs of
            [] -> [];
            _ -> [I, every_i]
          end,
  v2_solve_conj(NewIs, Cs, I, Map, Conj, IsFlat, V2State, UL, NewFs, VarsUp,
                LastMap, LastFlags);
v2_solve_conj(Is, [_|Tail], I, Map, Conj, IsFlat, V2State, UL, NewFs, VarsUp,
             LastMap, LastFlags) ->
  v2_solve_conj(Is, Tail, I+1, Map, Conj, IsFlat, V2State, UL, NewFs, VarsUp,
               LastMap, LastFlags).

-ifdef(DEBUG_LOOP_DETECTION).
report_detected_loop(Conj) ->
  io:format("A loop was detected in ~w\n", [Conj#constraint_list.id]).
-else.
report_detected_loop(_) ->
  ok.
-endif.

add_mask_to_flags(Flags, [Im|M], I, L) when I > Im ->
  add_mask_to_flags(Flags, M, I, [Im|L]);
add_mask_to_flags(Flags, [_|M], _I, L) ->
  {umerge_mask(Flags, M), lists:reverse(L)}.

umerge_mask([every_i]=Is, _F) ->
  Is;
umerge_mask(Is, F) ->
  lists:umerge(Is, F).

get_mask(V, Masks) ->
  case maps:find(V, Masks) of
    error -> [];
    {ok, M} -> M
  end.

get_flags(#v2_state{constr_data = ConData}=V2State0, C) ->
  #constraint_list{id = Id, list = Cs, masks = Masks} = C,
  case maps:find(Id, ConData) of
    error ->
      ?debug("get_flags Id=~w Flags=all ~w\n", [Id, length(Cs)]),
      V2State = V2State0#v2_state{constr_data = maps:put(Id, {[],[]}, ConData)},
      {V2State, [every_i]};
    {ok, failed} ->
      {V2State0, failed_list};
    {ok, {Part,U}} when U =/= [] ->
      ?debug("get_flags Id=~w U=~w\n", [Id, U]),
      V2State = V2State0#v2_state{constr_data = maps:put(Id, {Part,[]}, ConData)},
      save_updated_vars_list(Cs, vars_per_child(U, Masks), V2State)
  end.

vars_per_child(U, Masks) ->
  family([{I, V} || V <- lists:usort(U), I <- get_mask(V, Masks)]).

save_updated_vars_list(Cs, IU, V2State) ->
  save_updated_vars_list1(Cs, IU, V2State, 1, []).

save_updated_vars_list1([C|Cs], [{I,U}|IU], V2State0, I, Is) ->
  V2State = save_updated_vars(C, U, V2State0),
  save_updated_vars_list1(Cs, IU, V2State, I+1, [I|Is]);
save_updated_vars_list1([], [], V2State, _I, Is) ->
  {V2State, lists:reverse(Is)};
save_updated_vars_list1([_|Cs], IU, V2State, I, Is) ->
  save_updated_vars_list1(Cs, IU, V2State, I+1, Is).

save_updated_vars(#constraint{}, _, V2State) ->
  V2State;
save_updated_vars(#constraint_list{}=C, U, V2State0) ->
  save_updated_vars1(V2State0, C, U);
save_updated_vars(#constraint_ref{id = Id}, U, V2State) ->
  Cs = state__get_cs(Id, V2State#v2_state.state),
  save_updated_vars(Cs, U, V2State).

save_updated_vars1(V2State, C, U) ->
  #v2_state{constr_data = ConData} = V2State,
  #constraint_list{id = Id} = C,
  case maps:find(Id, ConData) of
    error -> V2State; % error means everything is flagged
    {ok, failed} -> V2State;
    {ok, {Part,U0}} ->
      %% Duplicates are not so common; let masks/2 remove them.
      U1 = U ++ U0,
      V2State#v2_state{constr_data = maps:put(Id, {Part,U1}, ConData)}
  end.

-ifdef(DEBUG).
pp_constr_data(_Tag, #v2_state{constr_data = D}) ->
  io:format("Constr data at ~p\n", [_Tag]),
  _ = [begin
         case _PartU of
           {_Part, _U} ->
             io:format("Id: ~w Vars: ~w\n", [_Id, _U]),
             [?pp_map("Part", maps:from_list(_Part)) || _Part =/= []];
           failed ->
             io:format("Id: ~w failed list\n", [_Id])
         end
       end ||
        {_Id, _PartU} <- lists:keysort(1, maps:to_list(D))],
  ok.

-else.
pp_constr_data(_Tag, _V2State) ->
  ok.
-endif.

failed_list(#constraint_list{id = Id}, #v2_state{constr_data = D}=V2State) ->
  ?debug("error list ~w~n", [Id]),
  V2State#v2_state{constr_data = maps:put(Id, failed, D)}.

is_failed_list(#constraint_list{id = Id}, #v2_state{constr_data = D}) ->
  maps:find(Id, D) =:= {ok, failed}.

%% Solver v1

solve_ref_or_list(#constraint_ref{id = Id, deps = Deps},
		  Map, MapDict, State) ->
  {OldLocalMap, Check} =
    case maps:find(Id, MapDict) of
      error -> {map_new(), false};
      {ok, M} -> {M, true}
    end,
  ?debug("Checking ref to fun: ~tw\n", [debug_lookup_name(Id)]),
  %% Note: mk_constraint_ref() has already removed Id from Deps. The
  %% reason for doing it there is that it makes it easy for
  %% calculate_masks() to make the corresponding adjustment for
  %% version v2.
  CheckDeps = ordsets:del_element(t_var_name(Id), Deps),
  true = CheckDeps =:= Deps,
  case Check andalso maps_are_equal(OldLocalMap, Map, CheckDeps) of
    true ->
      ?debug("Equal\n", []),
      {ok, MapDict, Map};
    false ->
      ?debug("Not equal. Solving\n", []),
      Cs = state__get_cs(Id, State),
      Res =
	case state__is_self_rec(Id, State) of
	  true -> solve_self_recursive(Cs, Map, MapDict, Id, t_none(), State);
	  false -> solve_ref_or_list(Cs, Map, MapDict, State)
	end,
      {NewMapDict, FunType} =
	case Res of
	  {error, NewMapDict0} ->
	    ?debug("Error solving for function ~tp\n", [debug_lookup_name(Id)]),
	    Arity = state__fun_arity(Id, State),
	    FunType0 =
	      case state__prop_domain(t_var_name(Id), State) of
		error -> t_fun(Arity, t_none());
		{ok, Dom} -> t_fun(Dom, t_none())
	      end,
	    {NewMapDict0, FunType0};
	  {ok, NewMapDict0, NewMap} ->
	    ?debug("Done solving fun: ~tp\n", [debug_lookup_name(Id)]),
	    FunType0 = lookup_type(Id, NewMap),
	    {NewMapDict0, FunType0}
	end,
      ?debug("  Id=~w Assigned ~ts\n", [Id, format_type(FunType)]),
      NewMap1 = enter_type(Id, FunType, Map),
      NewMap2 =
	case state__get_rec_var(Id, State) of
	  {ok, Var} -> enter_type(Var, FunType, NewMap1);
	  error -> NewMap1
	end,
      {ok, maps:put(Id, NewMap2, NewMapDict), NewMap2}
  end;
solve_ref_or_list(#constraint_list{type=Type, list = Cs, deps = Deps, id = Id},
		  Map, MapDict, State) ->
  {OldLocalMap, Check} =
    case maps:find(Id, MapDict) of
      error -> {map_new(), false};
      {ok, M} -> {M, true}
    end,
  ?debug("Checking ref to list: ~w\n", [Id]),
  if
    OldLocalMap =:= error -> {error, MapDict};
    true ->
      case Check andalso maps_are_equal(OldLocalMap, Map, Deps) of
        true ->
          ?debug("~tw equal ~w\n", [Type, Id]),
          {ok, MapDict, Map};
        false ->
          ?debug("~tw not equal: ~w. Solving\n", [Type, Id]),
          solve_clist(Cs, Type, Id, Deps, MapDict, Map, State)
      end
  end.

solve_self_recursive(Cs, Map, MapDict, Id, RecType0, State) ->
  ?debug("Solving self recursive ~tw\n", [debug_lookup_name(Id)]),
  {ok, RecVar} = state__get_rec_var(Id, State),
  ?debug("OldRecType ~ts\n", [format_type(RecType0)]),
  RecType = t_limit(RecType0, ?TYPE_LIMIT),
  Map1 = enter_type(RecVar, RecType, erase_type(t_var_name(Id), Map)),
  ?pp_map("Map1", Map1),
  case solve_ref_or_list(Cs, Map1, MapDict, State) of
    {error, _} = Error ->
      case t_is_none(RecType0) of
	true ->
	  %% Try again and assume that this is a non-terminating function.
	  Arity = state__fun_arity(Id, State),
	  NewRecType = t_fun(lists:duplicate(Arity, t_any()), t_unit()),
	  solve_self_recursive(Cs, Map, MapDict, Id, NewRecType, State);
	false ->
	  Error
      end;
    {ok, NewMapDict, NewMap} ->
      ?pp_map("NewMap", NewMap),
      NewRecType = unsafe_lookup_type(Id, NewMap),
      case is_equal(NewRecType, RecType0) of
	true ->
	  {ok, NewMapDict, enter_type(RecVar, NewRecType, NewMap)};
	false ->
	  solve_self_recursive(Cs, Map, MapDict, Id, NewRecType, State)
      end
  end.

solve_clist(Cs, conj, Id, Deps, MapDict, Map, State) ->
  case solve_cs(Cs, Map, MapDict, State) of
    {error, NewMapDict} ->
      {error, maps:put(Id, error, NewMapDict)};
    {ok, NewMapDict, NewMap} = Ret ->
      case Cs of
	[_] ->
	  %% Just a special case for one conjunctive constraint.
	  Ret;
	_ ->
	  case maps_are_equal(Map, NewMap, Deps) of
	    true -> {ok, maps:put(Id, NewMap, NewMapDict), NewMap};
	    false -> solve_clist(Cs, conj, Id, Deps, NewMapDict, NewMap, State)
	  end
      end
  end;
solve_clist(Cs, disj, Id, _Deps, MapDict, Map, State) ->
  Fun = fun(C, Dict) ->
	    case solve_ref_or_list(C, Map, Dict, State) of
	      {ok, NewDict, NewMap} -> {{ok, NewMap}, NewDict};
	      {error, _NewDict} = Error -> Error
	    end
	end,
  {Maps, NewMapDict} = lists:mapfoldl(Fun, MapDict, Cs),
  case [X || {ok, X} <- Maps] of
    [] -> {error, maps:put(Id, error, NewMapDict)};
    MapList ->
      NewMap = join_maps(MapList),
      {ok, maps:put(Id, NewMap, NewMapDict), NewMap}
  end.

solve_cs([#constraint_ref{} = C|Tail], Map, MapDict, State) ->
  case solve_ref_or_list(C, Map, MapDict, State) of
    {ok, NewMapDict, Map1} -> solve_cs(Tail, Map1, NewMapDict, State);
    {error, _NewMapDict} = Error -> Error
  end;
solve_cs([#constraint_list{} = C|Tail], Map, MapDict, State) ->
  case solve_ref_or_list(C, Map, MapDict, State) of
    {ok, NewMapDict, Map1} -> solve_cs(Tail, Map1, NewMapDict, State);
    {error, _NewMapDict} = Error -> Error
  end;
solve_cs([#constraint{} = C|Tail], Map, MapDict, State) ->
  case solve_one_c(C, Map) of
    error ->
      report_failed_constraint(C, Map),
      {error, MapDict};
    {ok, {NewMap, _U}} ->
      solve_cs(Tail, NewMap, MapDict, State)
  end;
solve_cs([], Map, MapDict, _State) ->
  {ok, MapDict, Map}.

solve_one_c(#constraint{lhs = Lhs, rhs = Rhs, op = Op}, Map) ->
  LhsType = lookup_type(Lhs, Map),
  RhsType = lookup_type(Rhs, Map),
  Inf = t_inf(LhsType, RhsType),
  ?debug("Solving: ~ts :: ~ts ~w ~ts :: ~ts\n\tInf: ~ts\n",
	 [format_type(Lhs), format_type(LhsType), Op,
	  format_type(Rhs), format_type(RhsType), format_type(Inf)]),
  case t_is_none(Inf) of
    true -> error;
    false ->
      case Op of
	sub -> solve_subtype(Lhs, Inf, Map);
	eq ->
	  case solve_subtype(Lhs, Inf, Map) of
	    error -> error;
	    {ok, {Map1, U1}} ->
              case solve_subtype(Rhs, Inf, Map1) of
                error -> error;
                {ok, {Map2, U2}} -> {ok, {Map2, lists:umerge(U1, U2)}}
              end
	  end
      end
  end.

solve_subtype(Type, Inf, Map) ->
  %% case cerl:is_literal(Type) of
  %%   true ->
  %%     case t_is_subtype(t_from_term(cerl:concrete(Type)), Inf) of
  %%	true -> {ok, Map};
  %%	false -> error
  %%     end;
  %%   false ->
      try t_unify(Type, Inf) of
	{_, List} -> {ok, enter_type_list(List, Map)}
      catch
	throw:{mismatch, _T1, _T2} ->
	  ?debug("Mismatch between ~ts and ~ts\n",
		 [format_type(_T1), format_type(_T2)]),
	  error
      end.
  %% end.

report_failed_constraint(_C, _Map) ->
  ?debug("+++++++++++\nFailed: ~ts :: ~ts ~w ~ts :: ~ts\n+++++++++++\n",
         [format_type(_C#constraint.lhs),
          format_type(lookup_type(_C#constraint.lhs, _Map)),
          _C#constraint.op,
          format_type(_C#constraint.rhs),
          format_type(lookup_type(_C#constraint.rhs, _Map))]).

%% ============================================================================
%%
%%  Maps and types.
%%
%% ============================================================================

map_new() ->
  maps:new().

join_maps([Map]) ->
  Map;
join_maps(Maps) ->
  Keys = constrained_keys(Maps),
  join_maps(Keys, Maps, map_new()).

constrained_keys(Maps) ->
  lists:foldl(fun(TmpMap, AccKeys) ->
                  [Key || Key <- AccKeys, maps:is_key(Key, TmpMap)]
              end,
              maps:keys(hd(Maps)), tl(Maps)).

join_maps([Key|Left], Maps = [Map|MapsLeft], AccMap) ->
  NewType = join_one_key(Key, MapsLeft, lookup_type(Key, Map)),
  NewAccMap = enter_type(Key, NewType, AccMap),
  join_maps(Left, Maps, NewAccMap);
join_maps([], _Maps, AccMap) ->
  AccMap.

join_one_key(Key, [Map|Maps], Type) ->
  case t_is_any(Type) of
    true -> Type;
    false ->
      NewType = lookup_type(Key, Map),
      case is_equal(NewType, Type) of
	true  -> join_one_key(Key, Maps, Type);
	false -> join_one_key(Key, Maps, t_sup(NewType, Type))
      end
  end;
join_one_key(_Key, [], Type) ->
  Type.

maps_are_equal(Map1, Map2, Deps) ->
  NewDeps = prune_keys(Map1, Map2, Deps),
  maps_are_equal_1(Map1, Map2, NewDeps).

maps_are_equal_1(Map1, Map2, [H|Tail]) ->
  T1 = lookup_type(H, Map1),
  T2 = lookup_type(H, Map2),
  case is_equal(T1, T2) of
    true -> maps_are_equal_1(Map1, Map2, Tail);
    false ->
      ?debug("~w: ~ts =/= ~ts\n", [H, format_type(T1), format_type(T2)]),
      false
  end;
maps_are_equal_1(_Map1, _Map2, []) ->
  true.

-define(PRUNE_LIMIT, 100).

prune_keys(Map1, Map2, Deps) ->
  %% This is only worthwhile if the number of deps is reasonably large,
  %% and also bigger than the number of elements in the maps.
  NofDeps = length(Deps),
  case NofDeps > ?PRUNE_LIMIT of
    true ->
      Keys1 = maps:keys(Map1),
      case length(Keys1) > NofDeps of
	true ->
	  Set1 = lists:sort(Keys1),
	  Set2 = lists:sort(maps:keys(Map2)),
	  ordsets:intersection(ordsets:union(Set1, Set2), Deps);
	false ->
	  Deps
      end;
    false ->
      Deps
  end.

enter_type(Key, Val, Map) when is_integer(Key) ->
  ?debug("Entering ~ts :: ~ts\n", [format_type(t_var(Key)), format_type(Val)]),
  %% Keep any() in the map if it is opaque:
  case is_equal(Val, t_any()) of
    true ->
      erase_type(Key, Map);
    false ->
      LimitedVal = t_limit(Val, ?INTERNAL_TYPE_LIMIT),
      case is_equal(LimitedVal, Val) of
	true -> ok;
	false -> ?debug("LimitedVal ~ts\n", [format_type(LimitedVal)])
      end,
      case maps:find(Key, Map) of
        {ok, Value} ->
          case is_equal(Value, LimitedVal) of
            true -> Map;
            false -> map_store(Key, LimitedVal, Map)
          end;
	error -> map_store(Key, LimitedVal, Map)
      end
  end;
enter_type(Key, Val, Map) ->
  KeyName = t_var_name(Key),
  enter_type(KeyName, Val, Map).

enter_type_lists([Key|KeyTail], [Val|ValTail], Map) ->
  Map1 = enter_type(Key, Val, Map),
  enter_type_lists(KeyTail, ValTail, Map1);
enter_type_lists([], [], Map) ->
  Map.

enter_type_list(KeyVals, Map) ->
  enter_type_list(KeyVals, Map, []).

enter_type_list([{Key, Val}|Tail], Map, U0) ->
  {Map1,U1} = enter_type2(Key, Val, Map),
  enter_type_list(Tail, Map1, U1++U0);
enter_type_list([], Map, U) ->
  {Map, ordsets:from_list(U)}.

enter_type2(Key, Val, Map) ->
  Map1 = enter_type(Key, Val, Map),
  {Map1, [Key || not is_same(Key, Map, Map1)]}.

map_store(Key, Val, Map) ->
  ?debug("Storing ~tw :: ~ts\n", [Key, format_type(Val)]),
  maps:put(Key, Val, Map).

erase_type(Key, Map) ->
  maps:remove(Key, Map).

lookup_type_list(List, Map) ->
  [lookup_type(X, Map) || X <- List].

unsafe_lookup_type(Key, Map) ->
  case maps:find(t_var_name(Key), Map) of
    {ok, Type} -> Type;
    error -> t_none()
  end.

unsafe_lookup_type_list(List, Map) ->
  [unsafe_lookup_type(X, Map) || X <- List].

lookup_type(Key, Map) when is_integer(Key) ->
  case maps:find(Key, Map) of
    error -> t_any();
    {ok, Val} -> Val
  end;
lookup_type(#fun_var{'fun' = Fun}, Map) ->
  Fun(Map);
lookup_type(Key, Map) ->
  %% Seems unused and dialyzer complains about it -- commented out.
  %% case cerl:is_literal(Key) of
  %%   true -> t_from_term(cerl:concrete(Key));
  %%   false ->
  t_subst(Key, Map).
  %% end.

mk_var(Var) ->
  case cerl:is_literal(Var) of
    true -> Var;
    false ->
      case cerl:is_c_values(Var) of
	true -> t_product(mk_var_no_lit_list(cerl:values_es(Var)));
	false -> t_var(cerl_trees:get_label(Var))
      end
  end.

mk_var_list(List) ->
  [mk_var(X) || X <- List].

mk_var_no_lit(Var) ->
  case cerl:is_literal(Var) of
    true -> t_from_term(cerl:concrete(Var));
    false -> mk_var(Var)
  end.

mk_var_no_lit_list(List) ->
  [mk_var_no_lit(X) || X <- List].

updated_vars_only(U, OldMap, NewMap) ->
  [V || V <- U, not is_same(V, OldMap, NewMap)].

is_same(Key, Map1, Map2) ->
  is_equal(lookup_type(Key, Map1), lookup_type(Key, Map2)).

is_equal(Type1, Type2) ->
  t_is_equal(Type1, Type2).

%% ============================================================================
%%
%%  The State.
%%
%% ============================================================================

new_state(MFAs, NextLabel, CallGraph, CServer, Plt, PropTypes0, Solvers) ->
  List_SCC =
    [begin
       {Var, Label} = dialyzer_codeserver:lookup_mfa_var_label(MFA, CServer),
       {{MFA, Var}, t_var(Label)}
   end || MFA <- MFAs],
  {List, SCC} = lists:unzip(List_SCC),
  NameMap = maps:from_list(List),
  SelfRec =
    case SCC of
      [OneF] ->
	Label = t_var_name(OneF),
	case dialyzer_callgraph:is_self_rec(Label, CallGraph) of
	  true -> OneF;
	  false -> false
	end;
      _Many -> false
    end,
  PropTypes = dict:from_list(PropTypes0),
  #state{callgraph = CallGraph, name_map = NameMap, next_label = NextLabel,
	 prop_types = PropTypes, plt = Plt, scc = ordsets:from_list(SCC),
	 mfas = MFAs, self_rec = SelfRec, solvers = Solvers,
         cserver = CServer}.

state__set_module(State, Module) ->
  State#state{module = Module}.

state__set_in_match(State, Bool) ->
  State#state{in_match = Bool}.

state__is_in_match(#state{in_match = Bool}) ->
  Bool.

state__set_in_guard(State, Bool) ->
  State#state{in_guard = Bool}.

state__is_in_guard(#state{in_guard = Bool}) ->
  Bool.

state__get_fun_prototype(Op, Arity, State) ->
  case t_is_fun(Op) of
    true -> {State, Op};
    false ->
      {State1, [Ret|Args]} = state__mk_vars(Arity+1, State),
      Fun = t_fun(Args, Ret),
      {State1, Fun}
  end.

state__lookup_rec_var_in_scope(MFA, #state{name_map = NameMap}) ->
  maps:find(MFA, NameMap).

state__store_fun_arity(Tree, #state{fun_arities = Map} = State) ->
  Arity = length(cerl:fun_vars(Tree)),
  Id = mk_var(Tree),
  State#state{fun_arities = maps:put(Id, Arity, Map)}.

state__fun_arity(Id, #state{fun_arities = Map}) ->
  maps:get(Id, Map).

state__lookup_undef_var(Tree, #state{callgraph = CG, plt = Plt}) ->
  Label = cerl_trees:get_label(Tree),
  case dialyzer_callgraph:lookup_rec_var(Label, CG) of
    error -> error;
    {ok, MFA} ->
      case dialyzer_plt:lookup(Plt, MFA) of
	none -> error;
	{value, {RetType, ArgTypes}} ->
          {ok, t_fun(ArgTypes, RetType)}
      end
  end.

state__lookup_apply(Tree, #state{callgraph = Callgraph}) ->
  Apply = cerl_trees:get_label(Tree),
  case dialyzer_callgraph:lookup_call_site(Apply, Callgraph) of
    error ->
      unknown;
    {ok, List} ->
      case lists:member(external, List) of
	true -> unknown;
	false -> List
      end
  end.

get_apply_constr(FunLabels, Dst, ArgTypes, #state{callgraph = CG} = State) ->
  MFAs = [dialyzer_callgraph:lookup_name(Label, CG) || Label <- FunLabels],
  case lists:member(error, MFAs) of
    true -> error;
    false ->
      Constrs0 =
	[begin
	   State1 = state__new_constraint_context(State),
	   try get_plt_constr(MFA, Dst, ArgTypes, State1) of
	       State2 -> state__cs(State2)
	   catch
	     throw:error -> error
	   end
	 end || {ok, MFA} <- MFAs],
      case [C || C <- Constrs0, C =/= error] of
	[] -> throw(error);
	Constrs ->
	  ApplyConstr = mk_disj_constraint_list(Constrs),
	  {ok, state__store_conj(ApplyConstr, State)}
      end
  end.

state__scc(#state{scc = SCC}) ->
  SCC.

state__add_fun_to_scc(Fun, #state{scc = SCC} = State) ->
  State#state{scc = ordsets:add_element(Fun, SCC)}.

state__plt(#state{plt = PLT}) ->
  PLT.

state__new_constraint_context(State) ->
  State#state{cs = []}.

state__prop_domain(FunLabel, #state{prop_types = PropTypes}) ->
 case dict:find(FunLabel, PropTypes) of
    error -> error;
    {ok, {_Range_Fun, Dom}} -> {ok, Dom};
    {ok, FunType} -> {ok, t_fun_args(FunType)}
  end.

state__add_prop_constrs(Tree, #state{prop_types = PropTypes} = State) ->
  Label = cerl_trees:get_label(Tree),
  case dict:find(Label, PropTypes) of
    error -> State;
    {ok, FunType} ->
      case t_fun_args(FunType) of
	unknown -> State;
	ArgTypes ->
	  case erl_types:any_none(ArgTypes) of
	    true -> not_called;
	    false ->
	      ?debug("Adding propagated constr: ~ts for function ~tw\n",
		     [format_type(FunType), debug_lookup_name(mk_var(Tree))]),
	      FunVar = mk_var(Tree),
	      state__store_conj(FunVar, sub, FunType, State)
	  end
      end
  end.

state__cs(#state{cs = Cs}) ->
  mk_conj_constraint_list(Cs).

state__store_conj(C, #state{cs = Cs} = State) ->
  State#state{cs = [C|Cs]}.

state__store_conj_list([H|T], State) ->
  State1 = state__store_conj(H, State),
  state__store_conj_list(T, State1);
state__store_conj_list([], State) ->
  State.

state__store_conj(Lhs, Op, Rhs, #state{cs = Cs} = State) ->
  State#state{cs = [mk_constraint(Lhs, Op, Rhs)|Cs]}.

state__store_conj_lists(List1, Op, List2, State) ->
  {NewList1, NewList2} = strip_of_any_constrs(List1, List2),
  state__store_conj_lists_1(NewList1, Op, NewList2, State).

strip_of_any_constrs(List1, List2) ->
  strip_of_any_constrs(List1, List2, [], []).

strip_of_any_constrs([T1|Left1], [T2|Left2], Acc1, Acc2) ->
  case t_is_any(T1) orelse constraint_opnd_is_any(T2) of
    true -> strip_of_any_constrs(Left1, Left2, Acc1, Acc2);
    false -> strip_of_any_constrs(Left1, Left2, [T1|Acc1], [T2|Acc2])
  end;
strip_of_any_constrs([], [], Acc1, Acc2) ->
  {Acc1, Acc2}.

state__store_conj_lists_1([Arg1|Arg1Tail], Op, [Arg2|Arg2Tail], State) ->
  State1 = state__store_conj(Arg1, Op, Arg2, State),
  state__store_conj_lists_1(Arg1Tail, Op, Arg2Tail, State1);
state__store_conj_lists_1([], _Op, [], State) ->
  State.

state__mk_var(#state{next_label = NL} = State) ->
  {State#state{next_label = NL+1}, t_var(NL)}.

state__mk_vars(N, #state{next_label = NL} = State) ->
  NewLabel = NL + N,
  Vars = [t_var(X) || X <- lists:seq(NL, NewLabel-1)],
  {State#state{next_label = NewLabel}, Vars}.

state__store_constrs(Id, Cs, #state{cmap = Map} = State) ->
  NewMap = maps:put(Id, Cs, Map),
  State#state{cmap = NewMap}.

state__get_cs(Var, #state{cmap = Map}) ->
  maps:get(Var, Map).

state__is_self_rec(Fun, #state{self_rec = SelfRec}) ->
  not (SelfRec =:= 'false') andalso is_equal(Fun, SelfRec).

state__store_funs(Vars0, Funs0, #state{fun_map = Map} = State) ->
  debug_make_name_map(Vars0, Funs0),
  Vars = mk_var_list(Vars0),
  Funs = mk_var_list(Funs0),
  NewMap = lists:foldl(fun({Var, Fun}, MP) -> maps:put(Fun, Var, MP) end,
		       Map, lists:zip(Vars, Funs)),
  State#state{fun_map = NewMap}.

state__get_rec_var(Fun, #state{fun_map = Map}) ->
  maps:find(Fun, Map).

state__finalize(State) ->
  State1 = state__new_constraint_context(State),
  State2 = enumerate_constraints(State1),
  order_fun_constraints(State2).

%% ============================================================================
%%
%%  Constraints
%%
%% ============================================================================

-spec mk_constraint(erl_types:erl_type(),
                    constr_op(),
                    fvar_or_type()) -> #constraint{}.

mk_constraint(Lhs, Op, Rhs) ->
  case t_is_any(Lhs) orelse constraint_opnd_is_any(Rhs) of
    false ->
      Deps = find_constraint_deps([Lhs, Rhs]),
      C = mk_constraint_1(Lhs, Op, Rhs, Deps),
      case Deps =:= [] of
	true ->
	  %% This constraint is constant. Solve it immediately.
	  case solve_one_c(C, map_new()) of
	    error -> throw(error);
	    _R ->
	      %% This is always true, keep it anyway for logistic reasons
	      C
	  end;
	false ->
	  C
      end;
    true ->
      mk_constraint_any(Op)
  end.

mk_constraint_any(Op) ->
  mk_constraint_1(t_any(), Op, t_any(), []).

%% the following function is used so that we do not call
%% erl_types:t_is_any/1 with a term other than an erl_type()
-spec constraint_opnd_is_any(fvar_or_type()) -> boolean().

constraint_opnd_is_any(#fun_var{}) -> false;
constraint_opnd_is_any(Type) -> t_is_any(Type).

-ifdef(DEBUG).

-spec mk_fun_var(integer(),
                 fun((_) -> erl_types:erl_type()),
                 [erl_types:erl_type()]) -> #fun_var{}.

mk_fun_var(Line, Fun, Types) ->
  Deps = [t_var_name(Var) || Var <- t_collect_vars(t_product(Types))],
  #fun_var{'fun' = Fun, deps = ordsets:from_list(Deps), origin = Line}.

pp_map(S, Map) ->
  ?debug("\t~s: ~p\n",
            [S, [{X, lists:flatten(format_type(Y))} ||
                  {X, Y} <- lists:keysort(1, maps:to_list(Map))]]).

-else.

-spec mk_fun_var(fun((_) -> erl_types:erl_type()), [erl_types:erl_type()]) -> #fun_var{}.

mk_fun_var(Fun, Types) ->
  Deps = [t_var_name(Var) || Var <- t_collect_vars(t_product(Types))],
  #fun_var{'fun' = Fun, deps = ordsets:from_list(Deps)}.

-endif.

-spec get_deps(constr()) -> deps().

get_deps(#constraint{deps = D}) -> D;
get_deps(#constraint_list{deps = D}) -> D;
get_deps(#constraint_ref{deps = D}) -> D.

-spec find_constraint_deps([fvar_or_type()]) -> deps().

find_constraint_deps(List) ->
  ordsets:from_list(find_constraint_deps(List, [])).

find_constraint_deps([#fun_var{deps = Deps}|Tail], Acc) ->
  find_constraint_deps(Tail, [Deps|Acc]);
find_constraint_deps([Type|Tail], Acc) ->
  NewAcc = [[t_var_name(D) || D <- t_collect_vars(Type)]|Acc],
  find_constraint_deps(Tail, NewAcc);
find_constraint_deps([], Acc) ->
  lists:append(Acc).

mk_constraint_1(Lhs, eq, Rhs, Deps) when Lhs < Rhs ->
  #constraint{lhs = Lhs, op = eq, rhs = Rhs, deps = Deps};
mk_constraint_1(Lhs, eq, Rhs, Deps) ->
  #constraint{lhs = Rhs, op = eq, rhs = Lhs, deps = Deps};
mk_constraint_1(Lhs, Op, Rhs, Deps) ->
  #constraint{lhs = Lhs, op = Op, rhs = Rhs, deps = Deps}.

mk_constraints([Lhs|LhsTail], Op, [Rhs|RhsTail]) ->
  [mk_constraint(Lhs, Op, Rhs) |
   mk_constraints(LhsTail, Op, RhsTail)];
mk_constraints([], _Op, []) ->
  [].

mk_constraint_ref(Id, Deps) ->
  %% See also solve_ref_or_list(), #constraint_ref{}.
  Ds = ordsets:del_element(t_var_name(Id), Deps),
  #constraint_ref{id = Id, deps = Ds}.

mk_constraint_list(Type, List) ->
  List1 = ordsets:from_list(lift_lists(Type, List)),
  case Type of
    conj ->
      List2 = ordsets:filter(fun(X) -> get_deps(X) =/= [] end, List1),
      mk_constraint_list_cont(Type, List2);
    disj ->
      case lists:any(fun(X) -> get_deps(X) =:= [] end, List1) of
	true -> mk_constraint_list_cont(Type, [mk_constraint_any(eq)]);
	false -> mk_constraint_list_cont(Type, List1)
      end
  end.

mk_constraint_list_cont(Type, List) ->
  Deps = calculate_deps(List),
  case Deps =:= [] of
    true -> #constraint_list{type = conj,
			     list = [mk_constraint_any(eq)],
			     deps = []};
    false -> #constraint_list{type = Type, list = List, deps = Deps}
  end.

lift_lists(Type, List) ->
  lift_lists(Type, List, []).

lift_lists(Type, [#constraint_list{type = Type, list = List}|Tail], Acc) ->
  lift_lists(Type, Tail, List++Acc);
lift_lists(Type, [C|Tail], Acc) ->
  lift_lists(Type, Tail, [C|Acc]);
lift_lists(_Type, [], Acc) ->
  Acc.

update_constraint_list(CL, List) ->
  CL#constraint_list{list = List}.

%% We expand guard constraints into dijunctive normal form to gain
%% precision in simple guards. However, because of the exponential
%% growth of this expansion in the presens of disjunctions we can even
%% get into trouble while expanding.
%%
%% To limit this we only expand when the number of disjunctions are
%% below a certain limit. This limit is currently set based on the
%% behaviour of boolean 'or'.
%%
%%         V1 = V2 or V3
%%
%% Gives us in simplified form the constraints
%%
%%         <Some cs> * ((V1 = true) + (V2 = true) + (V1 = false))
%%
%% and thus a three-parted disjunction. If want to allow for two
%% levels of disjunction we need to have 3^2 = 9 disjunctions. If we
%% want three levels we need 3^3 = 27 disjunctions. More than that
%% seems unnecessary and tends to blow up.
%%
%% Note that by not expanding we lose some precision, but we get a
%% safe over approximation.

-define(DISJ_NORM_FORM_LIMIT, 28).

mk_disj_norm_form(#constraint_list{} = CL) ->
  try
    List1 = expand_to_conjunctions(CL),
    mk_disj_constraint_list(List1)
  catch
    throw:too_many_disj -> CL
  end.

expand_to_conjunctions(#constraint_list{type = conj, list = List}) ->
  List1 = [C || C <- List, is_simple_constraint(C)],
  List2 = [expand_to_conjunctions(C) || #constraint_list{} = C <- List],
  case List2 =:= [] of
    true -> [mk_conj_constraint_list(List1)];
    false ->
      case List2 of
	[JustOneList] ->
	  [mk_conj_constraint_list([L|List1]) || L <- JustOneList];
	_ ->
	  combine_conj_lists(List2, List1)
      end
  end;
expand_to_conjunctions(#constraint_list{type = disj, list = List}) ->
  if length(List) > ?DISJ_NORM_FORM_LIMIT -> throw(too_many_disj);
     true -> ok
  end,
  List1 = [C || C <- List, is_simple_constraint(C)],
  %% Just an assert.
  [] = [C || #constraint{} = C <- List1],
  Expanded = lists:append([expand_to_conjunctions(C)
                           || #constraint_list{} = C <- List]),
  ReturnList = Expanded ++ List1,
  if length(ReturnList) > ?DISJ_NORM_FORM_LIMIT -> throw(too_many_disj);
     true -> ReturnList
  end.

is_simple_constraint(#constraint{}) -> true;
is_simple_constraint(#constraint_ref{}) -> true;
is_simple_constraint(#constraint_list{}) -> false.

combine_conj_lists([List1, List2|Left], Prefix) ->
  NewList = [mk_conj_constraint_list([L1, L2]) || L1 <- List1, L2 <- List2],
  if length(NewList) > ?DISJ_NORM_FORM_LIMIT -> throw(too_many_disj);
     true -> ok
  end,
  combine_conj_lists([NewList|Left], Prefix);
combine_conj_lists([List], Prefix) ->
  [mk_conj_constraint_list([mk_conj_constraint_list(Prefix), L]) || L <- List].

calculate_deps(List) ->
  calculate_deps(List, []).

calculate_deps([H|Tail], Acc) ->
  Deps = get_deps(H),
  calculate_deps(Tail, [Deps|Acc]);
calculate_deps([], []) -> [];
calculate_deps([], [L]) -> L;
calculate_deps([], Acc) ->
  lists:umerge(Acc).

mk_conj_constraint_list(List) ->
  mk_constraint_list(conj, List).

mk_disj_constraint_list([NotReallyAList]) ->
  NotReallyAList;
mk_disj_constraint_list(List) ->
  %% Make sure each element in the list is either a conjunction or a
  %% ref. Wrap single constraints into conjunctions.
  List1 = [wrap_simple_constr(C) || C <- List],
  mk_constraint_list(disj, List1).

wrap_simple_constr(#constraint{} = C) -> mk_conj_constraint_list([C]);
wrap_simple_constr(#constraint_list{} = C) -> C;
wrap_simple_constr(#constraint_ref{} = C) -> C.

enumerate_constraints(State) ->
  Cs = [mk_constraint_ref(Id, get_deps(state__get_cs(Id, State)))
	|| Id <- state__scc(State)],
  {_, _, NewState} = enumerate_constraints(Cs, 0, [], State),
  NewState.

enumerate_constraints([#constraint_ref{id = Id} = C|Tail], N, Acc, State) ->
  Cs = state__get_cs(Id, State),
  {[NewCs], NewN, NewState1} = enumerate_constraints([Cs], N, [], State),
  NewState2 = state__store_constrs(Id, NewCs, NewState1),
  enumerate_constraints(Tail, NewN+1, [C|Acc], NewState2);
enumerate_constraints([#constraint_list{type = conj, list = List} = C|Tail],
		      N, Acc, State) ->
  %% Separate the flat constraints from the deep ones to make a
  %% separate fixpoint iteration over the flat ones for speed.
  {Flat, Deep} = lists:partition(fun(#constraint{}) -> true;
				    (#constraint_list{}) -> false;
				    (#constraint_ref{}) -> false
				 end, List),
  {NewFlat, N1, State1} = enumerate_constraints(Flat, N, [], State),
  {NewDeep, N2, State2} = enumerate_constraints(Deep, N1, [], State1),
  {NewList, N3} =
    if
      NewFlat =:= [] -> {NewDeep, N2};
      NewDeep =:= [] -> {NewFlat, N2};
      true ->
        TmpCList = mk_conj_constraint_list(NewFlat),
        {[TmpCList#constraint_list{id = {list, N2}}| NewDeep],
         N2 + 1}
    end,
  NewAcc = [C#constraint_list{list = NewList, id = {list, N3}}|Acc],
  enumerate_constraints(Tail, N3+1, NewAcc, State2);
enumerate_constraints([#constraint_list{list = List, type = disj} = C|Tail],
		      N, Acc, State) ->
  {NewList, NewN, NewState} = enumerate_constraints(List, N, [], State),
  NewAcc = [C#constraint_list{list = NewList, id = {list, NewN}}|Acc],
  enumerate_constraints(Tail, NewN+1, NewAcc, NewState);
enumerate_constraints([#constraint{} = C|Tail], N, Acc, State) ->
  enumerate_constraints(Tail, N, [C|Acc], State);
enumerate_constraints([], N, Acc, State) ->
  {lists:reverse(Acc), N, State}.

%% Put the fun ref constraints last in any conjunction since we need
%% to separate the environment from the interior of the function.
order_fun_constraints(State) ->
  Cs = [mk_constraint_ref(Id, get_deps(state__get_cs(Id, State)))
	|| Id <- state__scc(State)],
  order_fun_constraints(Cs, State).

order_fun_constraints([#constraint_ref{id = Id}|Tail], State) ->
  Cs = state__get_cs(Id, State),
  {[Cs1], State1} = order_fun_constraints([Cs], [], [], State),
  NewCs = Cs1#constraint_list{deps = Cs#constraint_list.deps},
  NewState = state__store_constrs(Id, NewCs, State1),
  order_fun_constraints(Tail, NewState);
order_fun_constraints([], State) ->
  State.

order_fun_constraints([#constraint_ref{} = C|Tail], Funs, Acc, State) ->
  order_fun_constraints(Tail, [C|Funs], Acc, State);
order_fun_constraints([#constraint_list{list = List,
                                        type = Type,
                                        masks = OldMasks} = C|Tail],
		      Funs, Acc, State) ->
  case OldMasks of
    undefined ->
      {NewList, NewState} =
        case Type of
          conj -> order_fun_constraints(List, [], [], State);
          disj ->
            FoldFun = fun(X, AccState) ->
                          {[NewX], NewAccState} =
                            order_fun_constraints([X], [], [], AccState),
                          {NewX, NewAccState}
                      end,
            lists:mapfoldl(FoldFun, State, List)
        end,
      NewList2 = reset_deps(NewList, State),
      C1 = update_constraint_list(C, NewList2),
      Masks = calculate_masks(NewList, 1, []),
      NewAcc = [update_masks(C1, Masks)|Acc],
      order_fun_constraints(Tail, Funs, NewAcc, NewState);
    M when is_map(M) ->
      order_fun_constraints(Tail, Funs, [C|Acc], State)
  end;
order_fun_constraints([#constraint{} = C|Tail], Funs, Acc, State) ->
  order_fun_constraints(Tail, Funs, [C|Acc], State);
order_fun_constraints([], Funs, Acc, State) ->
  NewState = order_fun_constraints(Funs, State),
  {lists:reverse(Acc)++Funs, NewState}.

update_masks(C, Masks) ->
  C#constraint_list{masks = Masks}.

reset_deps(ConstrList, #state{solvers = Solvers}) ->
  case lists:member(v1, Solvers) of
    true ->
      ConstrList;
    false ->
      [reset_deps(Constr) || Constr <- ConstrList]
  end.

reset_deps(#constraint{}=C) -> C#constraint{deps = []};
reset_deps(#constraint_list{}=C) -> C#constraint_list{deps = []};
reset_deps(#constraint_ref{}=C) -> C#constraint_ref{deps = []}.

calculate_masks([C|Cs], I, L0) ->
  calculate_masks(Cs, I+1, [{V, I} || V <- get_deps(C)] ++ L0);
calculate_masks([], _I, L) ->
  M = family(L),
  maps:from_list(M).

%% ============================================================================
%%
%%  Utilities.
%%
%% ============================================================================

bif_return(M, F, A, Xs) ->
  erl_bif_types:type(M, F, A, Xs).

is_singleton_non_number_type(Type) ->
  case t_is_number(Type) of
    true -> false;
    false -> is_singleton_type(Type)
  end.

is_singleton_type(Type) ->
  case t_is_atom(Type) of
    true ->
      case t_atom_vals(Type) of
	unknown -> false;
	[_] -> true;
	[_|_] -> false
      end;
    false ->
      case t_is_integer(Type) of
	true ->
	  case t_number_vals(Type) of
	    unknown -> false;
	    [_] -> true;
	    [_|_] -> false
	  end;
	false ->
	  t_is_nil(Type)
      end
  end.

find_element(Args, Cs) ->
  [Pos, Tuple] = Args,
  case t_is_number(Pos) of
    true ->
      case erl_types:t_number_vals(Pos) of
        'unknown' -> 'unknown';
        [I] ->
          case find_constraint(Tuple, Cs) of
            'unknown' -> 'unknown';
            #constraint{lhs = ExTuple} ->
              case erl_types:t_is_tuple(ExTuple) of
                true ->
                  Elems = erl_types:t_tuple_args(ExTuple),
                  Elem = lists:nth(I, Elems),
                  case erl_types:t_is_var(Elem) of
                    true -> Elem;
                    false -> 'unknown'
                  end;
                false -> 'unknown'
              end
          end;
        _ -> 'unknown'
      end;
    false -> 'unknown'
  end.

find_constraint(_Tuple, []) ->
  'unknown';
find_constraint(Tuple, [#constraint{op = 'eq', rhs = Tuple} = C|_]) ->
  C;
find_constraint(Tuple, [#constraint_list{list = List}|Cs]) ->
  find_constraint(Tuple, List ++ Cs);
find_constraint(Tuple, [_|Cs]) ->
  find_constraint(Tuple, Cs).

-spec fold_literal_maybe_match(cerl:cerl(), state()) -> cerl:cerl().

fold_literal_maybe_match(Tree0, State) ->
  Tree1 = cerl:fold_literal(Tree0),
  case state__is_in_match(State) of
    false -> Tree1;
    true -> dialyzer_utils:refold_pattern(Tree1)
  end.

lookup_record(State, Tag, Arity) ->
  #state{module = M, mod_records = ModRecs, cserver = CServer} = State,
  {State1, Rec} =
    case lists:keyfind(M, 1, ModRecs) of
      {M, Rec0} ->
        {State, Rec0};
      false ->
        Rec0 = dialyzer_codeserver:lookup_mod_records(M, CServer),
        NewModRecs = [{M, Rec0}|ModRecs],
        {State#state{mod_records = NewModRecs}, Rec0}
    end,
  case erl_types:lookup_record(Tag, Arity, Rec) of
    {ok, Fields} ->
      RecType =
        t_tuple([t_from_term(Tag)|
                 [FieldType || {_FieldName, _Abstr, FieldType} <- Fields]]),
      {ok, RecType, State1};
    error ->
      {error, State1}
  end.

is_literal_record(Tree) ->
  Ann = cerl:get_ann(Tree),
  lists:member(record, Ann).

family(L) ->
  dialyzer_utils:family(L).

%% ============================================================================
%%
%%  Pretty printer and debug facilities.
%%
%% ============================================================================

-ifdef(DEBUG_CONSTRAINTS).
-ifndef(DEBUG).
-define(DEBUG, true).
-endif.
-endif.

-ifdef(DEBUG).
format_type(#fun_var{deps = Deps, origin = Origin}) ->
  L = [format_type(t_var(X)) || X <- Deps],
  io_lib:format("Fun@L~p(~ts)", [Origin, join_chars(L, ",")]);
format_type(Type) ->
  case cerl:is_literal(Type) of
    true -> io_lib:format("~tw", [cerl:concrete(Type)]);
    false -> erl_types:t_to_string(Type)
  end.

join_chars([], _Sep) ->
  [];
join_chars([H|T], Sep) ->
  [H|[[Sep,X] || X <- T]].

debug_lookup_name(Var) ->
  case maps:find(t_var_name(Var), get(dialyzer_typesig_map)) of
    error -> Var;
    {ok, Name} -> Name
  end.
-endif.

-ifdef(DEBUG_NAME_MAP).
debug_make_name_map(Vars, Funs) ->
  Map = get(dialyzer_typesig_map),
  NewMap =
    if Map =:= undefined -> debug_make_name_map(Vars, Funs, maps:new());
       true              -> debug_make_name_map(Vars, Funs, Map)
    end,
  put(dialyzer_typesig_map, NewMap).

debug_make_name_map([Var|VarLeft], [Fun|FunLeft], Map) ->
  Name = {cerl:fname_id(Var), cerl:fname_arity(Var)},
  FunLabel = cerl_trees:get_label(Fun),
  debug_make_name_map(VarLeft, FunLeft, maps:put(FunLabel, Name, Map));
debug_make_name_map([], [], Map) ->
  Map.

-else.
debug_make_name_map(_Vars, _Funs) ->
  ok.
-endif.

-ifdef(DEBUG_CONSTRAINTS).
pp_constrs_scc(SCC, State) ->
  [pp_constrs(Fun, state__get_cs(Fun, State), State) || Fun <- SCC].

pp_constrs(Fun, Cs, State) ->
  io:format("Constraints for fun: ~tw", [debug_lookup_name(Fun)]),
  MaxDepth = pp_constraints(Cs, State),
  io:format("Depth: ~w\n", [MaxDepth]).

pp_constraints(Cs, State) ->
  Res = pp_constraints([Cs], 0, 0, State),
  io:nl(),
  Res.

pp_constraints([List|Tail], Level, MaxDepth, State) when is_list(List) ->
  pp_constraints(List++Tail, Level, MaxDepth, State);
pp_constraints([#constraint_ref{id = Id}|Left], Level, MaxDepth, State) ->
  Cs = state__get_cs(Id, State),
  pp_indent(Level),
  io:format("%Ref ~w%", [t_var_name(Id)]),
  pp_constraints([Cs|Left], Level, MaxDepth, State);
pp_constraints([#constraint{}=C], Level, MaxDepth, _State) ->
  pp_op(C, Level),
  erlang:max(Level, MaxDepth);
pp_constraints([#constraint{}=C|Tail], Level, MaxDepth, State) ->
  pp_op(C, Level),
  pp_constraints(Tail, Level, MaxDepth, State);
pp_constraints([#constraint_list{type = Type, list = List, id = Id}|Tail],
	       Level, MaxDepth, State) ->
  pp_indent(Level),
  case Type of
    conj -> io:format("Conj ~w (", [Id]);
    disj -> io:format("Disj ~w (", [Id])
  end,
  NewMaxDepth = pp_constraints(List, Level+1, MaxDepth, State),
  io:format(")"),
  case Tail =:= [] of
    true  -> NewMaxDepth + 1;
    false -> pp_constraints(Tail, Level, NewMaxDepth, State)
  end.

pp_op(#constraint{lhs = Lhs, op = Op, rhs = Rhs}, Level) ->
  pp_indent(Level),
  io:format("~ts ~w ~ts", [format_type(Lhs), Op, format_type(Rhs)]).

pp_indent(Level) ->
  io:format("\n~*s", [Level*2, ""]).
-else.
pp_constrs_scc(_SCC, _State) ->
  ok.
-endif.

-ifdef(TO_DOT).

constraints_to_dot_scc(SCC, State) ->
  %% TODO: handle Unicode names.
  io:format("SCC: ~tp\n", [SCC]),
  Name = lists:flatten([format_lookup_name(debug_lookup_name(Fun))
                        || Fun <- SCC]),
  Cs = [state__get_cs(Fun, State) || Fun <- SCC],
  constraints_to_dot(Cs, Name, State).

format_lookup_name({FunName, Arity}) ->
  TupleS = io_lib:format("{~ts,~w}", [atom_to_list(FunName), Arity]),
  io_lib:format("~tw", [list_to_atom(lists:flatten(TupleS))]).

constraints_to_dot(Cs0, Name, State) ->
  NofCs = length(Cs0),
  Cs = lists:zip(lists:seq(1, NofCs), Cs0),
  {Graph, Opts, _N} = constraints_to_nodes(Cs, NofCs + 1, 1, [], [], State),
  hipe_dot:translate_list(Graph, "/tmp/cs.dot", "foo", Opts),
  %% "-T ps" works for Latin-1. hipe_dot cannot handle UTF-8 either.
  Res = os:cmd("dot -o /tmp/"++ Name ++ ".ps -T ps /tmp/cs.dot"),
  io:format("Res: ~ts~n", [Res]),
  ok.

constraints_to_nodes([{Name, #constraint_list{type = Type, list = List, id=Id}}
		      |Left], N, Level, Graph, Opts, State) ->
  N1 = N + length(List),
  NewList = lists:zip(lists:seq(N, N1 - 1), List),
  Names = [SubName || {SubName, _C} <- NewList],
  Edges = [{Name, SubName} || SubName <- Names],
  ThisNode = [{Name, Opt} || Opt <- [{label,
				      lists:flatten(io_lib:format("~w", [Id]))},
				     {shape, get_shape(Type)},
				     {level, Level}]],
  {NewGraph, NewOpts, N2} = constraints_to_nodes(NewList, N1, Level+1,
						 [Edges|Graph],
						 [ThisNode|Opts], State),
  constraints_to_nodes(Left, N2, Level, NewGraph, NewOpts, State);
constraints_to_nodes([{Name, #constraint{lhs = Lhs, op = Op, rhs = Rhs}}|Left],
		     N, Level, Graph, Opts, State) ->
  Label = lists:flatten(io_lib:format("~ts ~w ~ts",
				      [format_type(Lhs), Op,
				       format_type(Rhs)])),
  ThisNode = [{Name, Opt} || Opt <- [{label, Label}, {level, Level}]],
  NewOpts = [ThisNode|Opts],
  constraints_to_nodes(Left, N, Level, Graph, NewOpts, State);
constraints_to_nodes([{Name, #constraint_ref{id = Id0}}|Left],
		     N, Level, Graph, Opts, State) ->
  Id = debug_lookup_name(Id0),
  CList = state__get_cs(Id0, State),
  ThisNode = [{Name, Opt} || Opt <- [{label,
				      lists:flatten(io_lib:format("~w", [Id]))},
				     {shape, ellipse},
				     {level, Level}]],
  NewList = [{N, CList}],
  {NewGraph, NewOpts, N1} = constraints_to_nodes(NewList, N + 1, Level + 1,
						 [{Name, N}|Graph],
						 [ThisNode|Opts], State),
  constraints_to_nodes(Left, N1, Level, NewGraph, NewOpts, State);
constraints_to_nodes([], N, _Level, Graph, Opts, _State) ->
  {lists:flatten(Graph), lists:flatten(Opts), N}.

get_shape(conj) -> box;
get_shape(disj) -> diamond.

-else.
constraints_to_dot_scc(_SCC, _State) ->
  ok.
-endif.
