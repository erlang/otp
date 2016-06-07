%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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

%% @doc Basic functions on Core Erlang abstract syntax trees.
%%
%% <p>Syntax trees are defined in the module <a
%% href="cerl"><code>cerl</code></a>.</p>
%%
%% @type cerl() = cerl:cerl()

-module(cerl_trees).

-export([depth/1, fold/3, free_variables/1, get_label/1, label/1, label/2, 
	 map/2, mapfold/3, mapfold/4, size/1, variables/1]).

-import(cerl, [alias_pat/1, alias_var/1, ann_c_alias/3, ann_c_apply/3,
	       ann_c_binary/2, ann_c_bitstr/6, ann_c_call/4,
	       ann_c_case/3, ann_c_catch/2, ann_c_clause/4,
	       ann_c_cons_skel/3, ann_c_fun/3, ann_c_let/4,
	       ann_c_letrec/3, ann_c_module/5, ann_c_primop/3,
	       ann_c_receive/4, ann_c_seq/3, ann_c_try/6,
	       ann_c_tuple_skel/2, ann_c_values/2, apply_args/1,
	       apply_op/1, binary_segments/1, bitstr_val/1,
	       bitstr_size/1, bitstr_unit/1, bitstr_type/1,
	       bitstr_flags/1, call_args/1, call_module/1, call_name/1,
	       case_arg/1, case_clauses/1, catch_body/1, clause_body/1,
	       clause_guard/1, clause_pats/1, clause_vars/1, concrete/1,
	       cons_hd/1, cons_tl/1, fun_body/1, fun_vars/1, get_ann/1,
	       let_arg/1, let_body/1, let_vars/1, letrec_body/1,
	       letrec_defs/1, letrec_vars/1, module_attrs/1,
	       module_defs/1, module_exports/1, module_name/1,
	       module_vars/1, primop_args/1, primop_name/1,
	       receive_action/1, receive_clauses/1, receive_timeout/1,
	       seq_arg/1, seq_body/1, set_ann/2, subtrees/1, try_arg/1,
	       try_body/1, try_vars/1, try_evars/1, try_handler/1,
	       tuple_es/1, type/1, update_c_alias/3, update_c_apply/3,
	       update_c_binary/2, update_c_bitstr/6, update_c_call/4,
	       update_c_case/3, update_c_catch/2, update_c_clause/4,
	       update_c_cons/3, update_c_cons_skel/3, update_c_fun/3,
	       update_c_let/4, update_c_letrec/3, update_c_module/5,
	       update_c_primop/3, update_c_receive/4, update_c_seq/3,
	       update_c_try/6, update_c_tuple/2, update_c_tuple_skel/2,
	       update_c_values/2, values_es/1, var_name/1,

	       map_arg/1, map_es/1,
	       ann_c_map/3,
	       update_c_map/3,
	       is_c_map_pattern/1, ann_c_map_pattern/2,
	       map_pair_key/1,map_pair_val/1,map_pair_op/1,
	       ann_c_map_pair/4,
	       update_c_map_pair/4
	   ]).


%% ---------------------------------------------------------------------

%% @spec depth(Tree::cerl()) -> integer()
%%
%% @doc Returns the length of the longest path in the tree.  A leaf
%% node has depth zero, the tree representing "<code>{foo,
%% bar}</code>" has depth one, etc.

-spec depth(cerl:cerl()) -> non_neg_integer().

depth(T) ->
    case subtrees(T) of
	[] ->
	    0;
	Gs ->
	    1 + lists:foldl(fun (G, A) -> erlang:max(depth_1(G), A) end, 0, Gs)
    end.

depth_1(Ts) ->
    lists:foldl(fun (T, A) -> erlang:max(depth(T), A) end, 0, Ts).



%% @spec size(Tree::cerl()) -> integer()
%%
%% @doc Returns the number of nodes in <code>Tree</code>.

-spec size(cerl:cerl()) -> non_neg_integer().

size(T) ->
    fold(fun (_, S) -> S + 1 end, 0, T).


%% ---------------------------------------------------------------------

%% @spec map(Function, Tree::cerl()) -> cerl()
%%
%%	   Function = (cerl()) -> cerl()
%%	   
%% @doc Maps a function onto the nodes of a tree. This replaces each
%% node in the tree by the result of applying the given function on
%% the original node, bottom-up.
%%
%% @see mapfold/3

-spec map(fun((cerl:cerl()) -> cerl:cerl()), cerl:cerl()) -> cerl:cerl().

map(F, T) ->
    F(map_1(F, T)).

map_1(F, T) ->
    case type(T) of
 	literal ->
	    case concrete(T) of
		[_ | _] ->
		    update_c_cons(T, map(F, cons_hd(T)),
				  map(F, cons_tl(T)));
		V when tuple_size(V) > 0 ->
		    update_c_tuple(T, map_list(F, tuple_es(T)));
		_ ->
		    T
	    end;
 	var ->
 	    T;
	values ->
 	    update_c_values(T, map_list(F, values_es(T)));
	cons ->
	    update_c_cons_skel(T, map(F, cons_hd(T)),
			       map(F, cons_tl(T)));
 	tuple ->
	    update_c_tuple_skel(T, map_list(F, tuple_es(T)));
 	map ->
	    update_c_map(T, map(F, map_arg(T)), map_list(F, map_es(T)));
	map_pair ->
	    update_c_map_pair(T, map(F, map_pair_op(T)),
                                 map(F, map_pair_key(T)),
                                 map(F, map_pair_val(T)));
 	'let' ->
	    update_c_let(T, map_list(F, let_vars(T)),
			 map(F, let_arg(T)),
			 map(F, let_body(T)));
	seq ->
 	    update_c_seq(T, map(F, seq_arg(T)),
			 map(F, seq_body(T)));
 	apply ->
	    update_c_apply(T, map(F, apply_op(T)),
			   map_list(F, apply_args(T)));
 	call ->
 	    update_c_call(T, map(F, call_module(T)),
			  map(F, call_name(T)),
			  map_list(F, call_args(T)));
 	primop ->
	    update_c_primop(T, map(F, primop_name(T)),
			    map_list(F, primop_args(T)));
 	'case' ->
 	    update_c_case(T, map(F, case_arg(T)),
			  map_list(F, case_clauses(T)));
 	clause ->
 	    update_c_clause(T, map_list(F, clause_pats(T)),
			    map(F, clause_guard(T)),
			    map(F, clause_body(T)));
 	alias ->
	    update_c_alias(T, map(F, alias_var(T)),
			   map(F, alias_pat(T)));
 	'fun' ->
	    update_c_fun(T, map_list(F, fun_vars(T)),
			 map(F, fun_body(T)));
 	'receive' ->
	    update_c_receive(T, map_list(F, receive_clauses(T)),
			     map(F, receive_timeout(T)),
			     map(F, receive_action(T)));
 	'try' ->
 	    update_c_try(T, map(F, try_arg(T)),
			 map_list(F, try_vars(T)),
			 map(F, try_body(T)),
			 map_list(F, try_evars(T)),
			 map(F, try_handler(T)));
 	'catch' ->
	    update_c_catch(T, map(F, catch_body(T)));
	binary ->
	    update_c_binary(T, map_list(F, binary_segments(T)));
	bitstr ->
	    update_c_bitstr(T, map(F, bitstr_val(T)),
			    map(F, bitstr_size(T)),
			    map(F, bitstr_unit(T)),
			    map(F, bitstr_type(T)),
			    map(F, bitstr_flags(T)));
	letrec ->
	    update_c_letrec(T, map_pairs(F, letrec_defs(T)),
			    map(F, letrec_body(T)));
	module ->
	    update_c_module(T, map(F, module_name(T)),
			    map_list(F, module_exports(T)),
			    map_pairs(F, module_attrs(T)),
			    map_pairs(F, module_defs(T)))
    end.

map_list(F, [T | Ts]) ->
    [map(F, T) | map_list(F, Ts)];
map_list(_, []) ->
    [].

map_pairs(F, [{T1, T2} | Ps]) ->
    [{map(F, T1), map(F, T2)} | map_pairs(F, Ps)];
map_pairs(_, []) ->
    [].


%% @spec fold(Function, Unit::term(), Tree::cerl()) -> term()
%%
%%    Function = (cerl(), term()) -> term()
%%
%% @doc Does a fold operation over the nodes of the tree. The result
%% is the value of <code>Function(X1, Function(X2, ... Function(Xn,
%% Unit) ... ))</code>, where <code>X1, ..., Xn</code> are the nodes
%% of <code>Tree</code> in a post-order traversal.
%%
%% @see mapfold/3

-spec fold(fun((cerl:cerl(), term()) -> term()), term(), cerl:cerl()) -> term().

fold(F, S, T) ->
    F(T, fold_1(F, S, T)).

fold_1(F, S, T) ->
    case type(T) of
 	literal ->
	    case concrete(T) of
		[_ | _] ->
		    fold(F, fold(F, S, cons_hd(T)), cons_tl(T));
		V when tuple_size(V) > 0 ->
		    fold_list(F, S, tuple_es(T));
		_ ->
		    S
	    end;
 	var ->
 	    S;
	values ->
 	    fold_list(F, S, values_es(T));
	cons ->
	    fold(F, fold(F, S, cons_hd(T)), cons_tl(T));
	tuple ->
	    fold_list(F, S, tuple_es(T));
	map ->
	    fold_list(F, S, map_es(T));
	map_pair ->
	    fold(F,
		fold(F,
		    fold(F, S, map_pair_op(T)),
		    map_pair_key(T)),
		map_pair_val(T));
 	'let' ->
	    fold(F, fold(F, fold_list(F, S, let_vars(T)),
			 let_arg(T)),
		 let_body(T));
	seq ->
	    fold(F, fold(F, S, seq_arg(T)), seq_body(T));
	apply ->
	    fold_list(F, fold(F, S, apply_op(T)), apply_args(T));
 	call ->
	    fold_list(F, fold(F, fold(F, S, call_module(T)),
			      call_name(T)),
		      call_args(T));
 	primop ->
	    fold_list(F, fold(F, S, primop_name(T)), primop_args(T));
 	'case' ->
	    fold_list(F, fold(F, S, case_arg(T)), case_clauses(T));
 	clause ->
	    fold(F, fold(F, fold_list(F, S, clause_pats(T)),
			 clause_guard(T)),
		 clause_body(T));
 	alias ->
	    fold(F, fold(F, S, alias_var(T)), alias_pat(T));
 	'fun' ->
	    fold(F, fold_list(F, S, fun_vars(T)), fun_body(T));
 	'receive' ->
	    fold(F, fold(F, fold_list(F, S, receive_clauses(T)),
			 receive_timeout(T)),
		 receive_action(T));
 	'try' ->
	    fold(F, fold_list(F, fold(F, fold_list(F, fold(F, S, try_arg(T)),
						   try_vars(T)),
				      try_body(T)),
			      try_evars(T)),
		 try_handler(T));
 	'catch' ->
	    fold(F, S, catch_body(T));
	binary ->
	    fold_list(F, S, binary_segments(T));
	bitstr ->
	    fold(F,
		 fold(F,
		      fold(F,
			   fold(F,
				fold(F, S, bitstr_val(T)),
				bitstr_size(T)),
			   bitstr_unit(T)),
		      bitstr_type(T)),
		 bitstr_flags(T));
	letrec ->
	    fold(F, fold_pairs(F, S, letrec_defs(T)), letrec_body(T));
	module ->
	    fold_pairs(F, 
		       fold_pairs(F, 
				  fold_list(F,
					    fold(F, S, module_name(T)),
					    module_exports(T)),
				  module_attrs(T)),
		       module_defs(T))
    end.

fold_list(F, S, [T | Ts]) ->
    fold_list(F, fold(F, S, T), Ts);
fold_list(_, S, []) ->
    S.

fold_pairs(F, S, [{T1, T2} | Ps]) ->
    fold_pairs(F, fold(F, fold(F, S, T1), T2), Ps);
fold_pairs(_, S, []) ->
    S.


%% @spec mapfold(Function, Initial::term(), Tree::cerl()) ->
%%           {cerl(), term()}
%%
%%    Function = (cerl(), term()) -> {cerl(), term()}
%%
%% @doc Does a combined map/fold operation on the nodes of the
%% tree. This is similar to <code>map/2</code>, but also propagates a
%% value from each application of <code>Function</code> to the next,
%% starting with the given value <code>Initial</code>, while doing a
%% post-order traversal of the tree, much like <code>fold/3</code>.
%%
%% This is the same as mapfold/4, with an identity function as the
%% pre-operation.
%%
%% @see map/2
%% @see fold/3
%% @see mapfold/4

-spec mapfold(fun((cerl:cerl(), term()) -> {cerl:cerl(), term()}),
	      term(), cerl:cerl()) -> {cerl:cerl(), term()}.

mapfold(F, S0, T) ->
  mapfold(fun(T0, A) -> {T0, A} end, F, S0, T).


%% @spec mapfold(Pre, Post, Initial::term(), Tree::cerl()) ->
%%           {cerl(), term()}
%%
%%    Pre = Post = (cerl(), term()) -> {cerl(), term()}
%%
%% @doc Does a combined map/fold operation on the nodes of the
%% tree. It begins by calling <code>Pre</code> on the tree, using the
%% <code>Initial</code> value. It then deconstructs the top node of
%% the returned tree and recurses on the children, using the returned
%% value as the new initial and carrying the returned values from one
%% call to the next. Finally it reassembles the top node from the
%% children, calls <code>Post</code> on it and returns the result.

-spec mapfold(fun((cerl:cerl(), term()) -> {cerl:cerl(), term()}),
              fun((cerl:cerl(), term()) -> {cerl:cerl(), term()}),
	      term(), cerl:cerl()) -> {cerl:cerl(), term()}.

mapfold(Pre, Post, S00, T0) ->
    {T, S0} = Pre(T0, S00),
    case type(T) of
 	literal ->
	    case concrete(T) of
		[_ | _] ->
		    {T1, S1} = mapfold(Pre, Post, S0, cons_hd(T)),
		    {T2, S2} = mapfold(Pre, Post, S1, cons_tl(T)),
		    Post(update_c_cons(T, T1, T2), S2);
		V when tuple_size(V) > 0 ->
		    {Ts, S1} = mapfold_list(Pre, Post, S0, tuple_es(T)),
		    Post(update_c_tuple(T, Ts), S1);
		_ ->
		    Post(T, S0)
	    end;
 	var ->
	    Post(T, S0);
	values ->
	    {Ts, S1} = mapfold_list(Pre, Post, S0, values_es(T)),
	    Post(update_c_values(T, Ts), S1);
	cons ->
	    {T1, S1} = mapfold(Pre, Post, S0, cons_hd(T)),
	    {T2, S2} = mapfold(Pre, Post, S1, cons_tl(T)),
	    Post(update_c_cons_skel(T, T1, T2), S2);
 	tuple ->
	    {Ts, S1} = mapfold_list(Pre, Post, S0, tuple_es(T)),
	    Post(update_c_tuple_skel(T, Ts), S1);
	map ->
	    {M , S1} = mapfold(Pre, Post, S0, map_arg(T)),
	    {Ts, S2} = mapfold_list(Pre, Post, S1, map_es(T)),
	    Post(update_c_map(T, M, Ts), S2);
	map_pair ->
	    {Op,  S1} = mapfold(Pre, Post, S0, map_pair_op(T)),
	    {Key, S2} = mapfold(Pre, Post, S1, map_pair_key(T)),
	    {Val, S3} = mapfold(Pre, Post, S2, map_pair_val(T)),
	    Post(update_c_map_pair(T,Op,Key,Val), S3);
 	'let' ->
	    {Vs, S1} = mapfold_list(Pre, Post, S0, let_vars(T)),
	    {A, S2} = mapfold(Pre, Post, S1, let_arg(T)),
	    {B, S3} = mapfold(Pre, Post, S2, let_body(T)),
	    Post(update_c_let(T, Vs, A, B), S3);
	seq ->
	    {A, S1} = mapfold(Pre, Post, S0, seq_arg(T)),
	    {B, S2} = mapfold(Pre, Post, S1, seq_body(T)),
	    Post(update_c_seq(T, A, B), S2);
 	apply ->
	    {E, S1} = mapfold(Pre, Post, S0, apply_op(T)),
	    {As, S2} = mapfold_list(Pre, Post, S1, apply_args(T)),
	    Post(update_c_apply(T, E, As), S2);
 	call ->
	    {M, S1} = mapfold(Pre, Post, S0, call_module(T)),
	    {N, S2} = mapfold(Pre, Post, S1, call_name(T)),
	    {As, S3} = mapfold_list(Pre, Post, S2, call_args(T)),
	    Post(update_c_call(T, M, N, As), S3);
 	primop ->
	    {N, S1} = mapfold(Pre, Post, S0, primop_name(T)),
	    {As, S2} = mapfold_list(Pre, Post, S1, primop_args(T)),
	    Post(update_c_primop(T, N, As), S2);
 	'case' ->
	    {A, S1} = mapfold(Pre, Post, S0, case_arg(T)),
	    {Cs, S2} = mapfold_list(Pre, Post, S1, case_clauses(T)),
	    Post(update_c_case(T, A, Cs), S2);
 	clause ->
	    {Ps, S1} = mapfold_list(Pre, Post, S0, clause_pats(T)),
	    {G, S2} = mapfold(Pre, Post, S1, clause_guard(T)),
	    {B, S3} = mapfold(Pre, Post, S2, clause_body(T)),
	    Post(update_c_clause(T, Ps, G, B), S3);
 	alias ->
	    {V, S1} = mapfold(Pre, Post, S0, alias_var(T)),
	    {P, S2} = mapfold(Pre, Post, S1, alias_pat(T)),
	    Post(update_c_alias(T, V, P), S2);
 	'fun' ->
	    {Vs, S1} = mapfold_list(Pre, Post, S0, fun_vars(T)),
	    {B, S2} = mapfold(Pre, Post, S1, fun_body(T)),
	    Post(update_c_fun(T, Vs, B), S2);
 	'receive' ->
	    {Cs, S1} = mapfold_list(Pre, Post, S0, receive_clauses(T)),
	    {E, S2} = mapfold(Pre, Post, S1, receive_timeout(T)),
	    {A, S3} = mapfold(Pre, Post, S2, receive_action(T)),
	    Post(update_c_receive(T, Cs, E, A), S3);
 	'try' ->
	    {E, S1} = mapfold(Pre, Post, S0, try_arg(T)),
	    {Vs, S2} = mapfold_list(Pre, Post, S1, try_vars(T)),
	    {B, S3} = mapfold(Pre, Post, S2, try_body(T)),
	    {Evs, S4} = mapfold_list(Pre, Post, S3, try_evars(T)),
	    {H, S5} = mapfold(Pre, Post, S4, try_handler(T)),
	    Post(update_c_try(T, E, Vs, B, Evs, H), S5);
 	'catch' ->
	    {B, S1} = mapfold(Pre, Post, S0, catch_body(T)),
	    Post(update_c_catch(T, B), S1);
	binary ->
	    {Ds, S1} = mapfold_list(Pre, Post, S0, binary_segments(T)),
	    Post(update_c_binary(T, Ds), S1);
	bitstr ->
	    {Val, S1} = mapfold(Pre, Post, S0, bitstr_val(T)),
	    {Size, S2} = mapfold(Pre, Post, S1, bitstr_size(T)),
	    {Unit, S3} = mapfold(Pre, Post, S2, bitstr_unit(T)),
	    {Type, S4} = mapfold(Pre, Post, S3, bitstr_type(T)),
	    {Flags, S5} = mapfold(Pre, Post, S4, bitstr_flags(T)),
	    Post(update_c_bitstr(T, Val, Size, Unit, Type, Flags), S5);
	letrec ->
	    {Ds, S1} = mapfold_pairs(Pre, Post, S0, letrec_defs(T)),
	    {B, S2} = mapfold(Pre, Post, S1, letrec_body(T)),
	    Post(update_c_letrec(T, Ds, B), S2);
	module ->
	    {N, S1} = mapfold(Pre, Post, S0, module_name(T)),
	    {Es, S2} = mapfold_list(Pre, Post, S1, module_exports(T)),
	    {As, S3} = mapfold_pairs(Pre, Post, S2, module_attrs(T)),
	    {Ds, S4} = mapfold_pairs(Pre, Post, S3, module_defs(T)),
	    Post(update_c_module(T, N, Es, As, Ds), S4)
    end.

mapfold_list(Pre, Post, S0, [T | Ts]) ->
    {T1, S1} = mapfold(Pre, Post, S0, T),
    {Ts1, S2} = mapfold_list(Pre, Post, S1, Ts),
    {[T1 | Ts1], S2};
mapfold_list(_, _, S, []) ->
    {[], S}.

mapfold_pairs(Pre, Post, S0, [{T1, T2} | Ps]) ->
    {T3, S1} = mapfold(Pre, Post, S0, T1),
    {T4, S2} = mapfold(Pre, Post, S1, T2),
    {Ps1, S3} = mapfold_pairs(Pre, Post, S2, Ps),
    {[{T3, T4} | Ps1], S3};
mapfold_pairs(_, _, S, []) ->
    {[], S}.


%% ---------------------------------------------------------------------

%% @spec variables(Tree::cerl()) -> [var_name()]
%%
%%	    var_name() = integer() | atom() | {atom(), integer()}
%%
%% @doc Returns an ordered-set list of the names of all variables in
%% the syntax tree. (This includes function name variables.) An
%% exception is thrown if <code>Tree</code> does not represent a
%% well-formed Core Erlang syntax tree.
%%
%% @see free_variables/1

-spec variables(cerl:cerl()) -> [cerl:var_name()].

variables(T) ->
    variables(T, false).


%% @spec free_variables(Tree::cerl()) -> [var_name()]
%%
%% @doc Like <code>variables/1</code>, but only includes variables
%% that are free in the tree.
%%
%% @see variables/1

-spec free_variables(cerl:cerl()) -> [cerl:var_name()].

free_variables(T) ->
    variables(T, true).


%% This is not exported

variables(T, S) ->
    case type(T) of
	literal ->
	    [];
	var ->
	    [var_name(T)];
	values ->
	    vars_in_list(values_es(T), S);
	cons ->
	    ordsets:union(variables(cons_hd(T), S),
			  variables(cons_tl(T), S));
	tuple ->
	    vars_in_list(tuple_es(T), S);
	map ->
	    vars_in_list([map_arg(T)|map_es(T)], S);
	map_pair ->
	    vars_in_list([map_pair_op(T),map_pair_key(T),map_pair_val(T)], S);
	'let' ->
	    Vs = variables(let_body(T), S),
	    Vs1 = var_list_names(let_vars(T)),
	    Vs2 = case S of
		      true ->
			  ordsets:subtract(Vs, Vs1);
		      false ->
			  ordsets:union(Vs, Vs1)
		  end,
	    ordsets:union(variables(let_arg(T), S), Vs2);
	seq ->
	    ordsets:union(variables(seq_arg(T), S),
			  variables(seq_body(T), S));
	apply ->
	    ordsets:union(
	      variables(apply_op(T), S),
	      vars_in_list(apply_args(T), S));
	call ->
	    ordsets:union(variables(call_module(T), S),
			  ordsets:union(
			    variables(call_name(T), S),
			    vars_in_list(call_args(T), S)));
	primop ->
	    vars_in_list(primop_args(T), S);
	'case' ->
	    ordsets:union(variables(case_arg(T), S),
			  vars_in_list(case_clauses(T), S));
	clause ->
	    Vs = ordsets:union(variables(clause_guard(T), S),
			       variables(clause_body(T), S)),
	    Vs1 = vars_in_list(clause_pats(T), S),
	    case S of
		true ->
		    ordsets:subtract(Vs, Vs1);
		false ->
		    ordsets:union(Vs, Vs1)
	    end;
	alias ->
	    ordsets:add_element(var_name(alias_var(T)),
				variables(alias_pat(T)));
	'fun' ->
	    Vs = variables(fun_body(T), S),
	    Vs1 = var_list_names(fun_vars(T)),
	    case S of
		true ->
		    ordsets:subtract(Vs, Vs1);
		false ->
		    ordsets:union(Vs, Vs1)
	    end;
	'receive' ->
	    ordsets:union(
	      vars_in_list(receive_clauses(T), S),
	      ordsets:union(variables(receive_timeout(T), S),
			    variables(receive_action(T), S)));
	'try' ->
	    Vs = variables(try_body(T), S),
	    Vs1 = var_list_names(try_vars(T)),
	    Vs2 = case S of
		      true ->
			  ordsets:subtract(Vs, Vs1);
		      false ->
			  ordsets:union(Vs, Vs1)
		  end,
	    Vs3 = variables(try_handler(T), S),
	    Vs4 = var_list_names(try_evars(T)),
	    Vs5 = case S of
		      true ->
			  ordsets:subtract(Vs3, Vs4);
		      false ->
			  ordsets:union(Vs3, Vs4)
		  end,
	    ordsets:union(variables(try_arg(T), S),
			  ordsets:union(Vs2, Vs5));
	'catch' ->
	    variables(catch_body(T), S);
	binary ->
	    vars_in_list(binary_segments(T), S);
	bitstr ->
	    ordsets:union(variables(bitstr_val(T), S),
			  variables(bitstr_size(T), S));
	letrec ->
	    Vs = vars_in_defs(letrec_defs(T), S),
	    Vs1 = ordsets:union(variables(letrec_body(T), S), Vs),
	    Vs2 = var_list_names(letrec_vars(T)),
	    case S of
		true ->
		    ordsets:subtract(Vs1, Vs2);
		false ->
		    ordsets:union(Vs1, Vs2)
	    end;
	module ->
	    Vs = vars_in_defs(module_defs(T), S),
	    Vs1 = ordsets:union(vars_in_list(module_exports(T), S), Vs),
	    Vs2 = var_list_names(module_vars(T)),
	    case S of
		true ->
		    ordsets:subtract(Vs1, Vs2);
		false ->
		    ordsets:union(Vs1, Vs2)
	    end
    end.

vars_in_list(Ts, S) ->
    vars_in_list(Ts, S, []).

vars_in_list([T | Ts], S, A) ->
    vars_in_list(Ts, S, ordsets:union(variables(T, S), A));
vars_in_list([], _, A) ->
    A.

%% Note that this function only visits the right-hand side of function
%% definitions.

vars_in_defs(Ds, S) ->
    vars_in_defs(Ds, S, []).

vars_in_defs([{_, Post} | Ds], S, A) ->
    vars_in_defs(Ds, S, ordsets:union(variables(Post, S), A));
vars_in_defs([], _, A) ->
    A.

%% This amounts to insertion sort. Since the lists are generally short,
%% it is hardly worthwhile to use an asymptotically better sort.

var_list_names(Vs) ->
    var_list_names(Vs, []).

var_list_names([V | Vs], A) ->
    var_list_names(Vs, ordsets:add_element(var_name(V), A));
var_list_names([], A) ->
    A.


%% ---------------------------------------------------------------------

%% label(Tree::cerl()) -> {cerl(), integer()}
%%
%% @equiv label(Tree, 0)

-spec label(cerl:cerl()) -> {cerl:cerl(), integer()}.

label(T) ->
    label(T, 0).

%% @spec label(Tree::cerl(), N::integer()) -> {cerl(), integer()}
%%
%% @doc Labels each expression in the tree. A term <code>{label,
%% L}</code> is prefixed to the annotation list of each expression node,
%% where L is a unique number for every node, except for variables (and
%% function name variables) which get the same label if they represent
%% the same variable. Constant literal nodes are not labeled.
%%
%% <p>The returned value is a tuple <code>{NewTree, Max}</code>, where
%% <code>NewTree</code> is the labeled tree and <code>Max</code> is 1
%% plus the largest label value used. All previous annotation terms on
%% the form <code>{label, X}</code> are deleted.</p>
%%
%% <p>The values of L used in the tree is a dense range from
%% <code>N</code> to <code>Max - 1</code>, where <code>N =&lt; Max
%% =&lt; N + size(Tree)</code>. Note that it is possible that no
%% labels are used at all, i.e., <code>N = Max</code>.</p>
%%
%% <p>Note: All instances of free variables will be given distinct
%% labels.</p>
%%
%% @see label/1
%% @see size/1

-spec label(cerl:cerl(), integer()) -> {cerl:cerl(), integer()}.

label(T, N) ->
    label(T, N, dict:new()).

label(T, N, Env) ->
    case type(T) of
 	literal ->
	    %% Constant literals are not labeled.
	    {T, N};
	var ->
            {As, N1} =
                case dict:find(var_name(T), Env) of
		    {ok, L} ->
		        {A, _} = label_ann(T, L),
		        {A, N};
                    error ->
		        label_ann(T, N)
                end,
	    {set_ann(T, As), N1};
	values ->
	    {Ts, N1} = label_list(values_es(T), N, Env),
	    {As, N2} = label_ann(T, N1),
	    {ann_c_values(As, Ts), N2};
	cons ->
	    {T1, N1} = label(cons_hd(T), N, Env),
	    {T2, N2} = label(cons_tl(T), N1, Env),
	    {As, N3} = label_ann(T, N2),
	    {ann_c_cons_skel(As, T1, T2), N3};
 	tuple ->
	    {Ts, N1} = label_list(tuple_es(T), N, Env),
	    {As, N2} = label_ann(T, N1),
	    {ann_c_tuple_skel(As, Ts), N2};
 	map ->
	    case is_c_map_pattern(T) of
		false ->
		    {M,  N1} = label(map_arg(T), N, Env),
		    {Ts, N2} = label_list(map_es(T), N1, Env),
		    {As, N3} = label_ann(T, N2),
		    {ann_c_map(As, M, Ts), N3};
		true ->
		    {Ts, N1} = label_list(map_es(T), N, Env),
		    {As, N2} = label_ann(T, N1),
		    {ann_c_map_pattern(As, Ts), N2}
	    end;
	map_pair ->
	    {Op,  N1} = label(map_pair_op(T), N, Env),
	    {Key, N2} = label(map_pair_key(T), N1, Env),
	    {Val, N3} = label(map_pair_val(T), N2, Env),
	    {As,  N4} = label_ann(T, N3),
	    {ann_c_map_pair(As,Op,Key,Val), N4};
 	'let' ->
	    {A, N1} = label(let_arg(T), N, Env),
	    {Vs, N2, Env1} = label_vars(let_vars(T), N1, Env),
	    {B, N3} = label(let_body(T), N2, Env1),
	    {As, N4} = label_ann(T, N3),
	    {ann_c_let(As, Vs, A, B), N4};
	seq ->
	    {A, N1} = label(seq_arg(T), N, Env),
	    {B, N2} = label(seq_body(T), N1, Env),
	    {As, N3} = label_ann(T, N2),
 	    {ann_c_seq(As, A, B), N3};
 	apply ->
	    {E, N1} = label(apply_op(T), N, Env),
	    {Es, N2} = label_list(apply_args(T), N1, Env),
	    {As, N3} = label_ann(T, N2),
	    {ann_c_apply(As, E, Es), N3};
 	call ->
	    {M, N1} = label(call_module(T), N, Env),
	    {F, N2} = label(call_name(T), N1, Env),
	    {Es, N3} = label_list(call_args(T), N2, Env),
	    {As, N4} = label_ann(T, N3),
 	    {ann_c_call(As, M, F, Es), N4};
 	primop ->
	    {F, N1} = label(primop_name(T), N, Env),
	    {Es, N2} = label_list(primop_args(T), N1, Env),
	    {As, N3} = label_ann(T, N2),
	    {ann_c_primop(As, F, Es), N3};
 	'case' ->
	    {A, N1} = label(case_arg(T), N, Env),
	    {Cs, N2} = label_list(case_clauses(T), N1, Env),
	    {As, N3} = label_ann(T, N2),
 	    {ann_c_case(As, A, Cs), N3};
 	clause ->
	    {_, N1, Env1} = label_vars(clause_vars(T), N, Env),
	    {Ps, N2} = label_list(clause_pats(T), N1, Env1),
	    {G, N3} = label(clause_guard(T), N2, Env1),
	    {B, N4} = label(clause_body(T), N3, Env1),
	    {As, N5} = label_ann(T, N4),
	    {ann_c_clause(As, Ps, G, B), N5};
 	alias ->
	    {V, N1} = label(alias_var(T), N, Env),
	    {P, N2} = label(alias_pat(T), N1, Env),
	    {As, N3} = label_ann(T, N2),
	    {ann_c_alias(As, V, P), N3};
 	'fun' ->
	    {Vs, N1, Env1} = label_vars(fun_vars(T), N, Env),
	    {B, N2} = label(fun_body(T), N1, Env1),
	    {As, N3} = label_ann(T, N2),
	    {ann_c_fun(As, Vs, B), N3};
 	'receive' ->
	    {Cs, N1} = label_list(receive_clauses(T), N, Env),
	    {E, N2} = label(receive_timeout(T), N1, Env),
	    {A, N3} = label(receive_action(T), N2, Env),
	    {As, N4} = label_ann(T, N3),
	    {ann_c_receive(As, Cs, E, A), N4};
 	'try' ->
	    {E, N1} = label(try_arg(T), N, Env),
	    {Vs, N2, Env1} = label_vars(try_vars(T), N1, Env),
	    {B, N3} = label(try_body(T), N2, Env1),
	    {Evs, N4, Env2} = label_vars(try_evars(T), N3, Env),
	    {H, N5} = label(try_handler(T), N4, Env2),
	    {As, N6} = label_ann(T, N5),
	    {ann_c_try(As, E, Vs, B, Evs, H), N6};
 	'catch' ->
	    {B, N1} = label(catch_body(T), N, Env),
	    {As, N2} = label_ann(T, N1),
	    {ann_c_catch(As, B), N2};
	binary ->
	    {Ds, N1} = label_list(binary_segments(T), N, Env),
	    {As, N2} = label_ann(T, N1),
	    {ann_c_binary(As, Ds), N2};
	bitstr ->
	    {Val, N1} = label(bitstr_val(T), N, Env),
	    {Size, N2} = label(bitstr_size(T), N1, Env),
	    {Unit, N3} = label(bitstr_unit(T), N2, Env),
	    {Type, N4} = label(bitstr_type(T), N3, Env),
	    {Flags, N5} = label(bitstr_flags(T), N4, Env),
	    {As, N6} = label_ann(T, N5),
	    {ann_c_bitstr(As, Val, Size, Unit, Type, Flags), N6};
	letrec ->
	    {_, N1, Env1} = label_vars(letrec_vars(T), N, Env),
	    {Ds, N2} = label_defs(letrec_defs(T), N1, Env1),
	    {B, N3} = label(letrec_body(T), N2, Env1),
	    {As, N4} = label_ann(T, N3),
	    {ann_c_letrec(As, Ds, B), N4};
	module ->
	    %% The module name is not labeled.
	    {_, N1, Env1} = label_vars(module_vars(T), N, Env),
	    {Ts, N2} = label_defs(module_attrs(T), N1, Env1),
	    {Ds, N3} = label_defs(module_defs(T), N2, Env1),
	    {Es, N4} = label_list(module_exports(T), N3, Env1),
	    {As, N5} = label_ann(T, N4),
	    {ann_c_module(As, module_name(T), Es, Ts, Ds), N5}
    end.

label_list([T | Ts], N, Env) ->
    {T1, N1} = label(T, N, Env),
    {Ts1, N2} = label_list(Ts, N1, Env),
    {[T1 | Ts1], N2};
label_list([], N, _Env) ->
    {[], N}.

label_vars([T | Ts], N, Env) ->
    Env1 = dict:store(var_name(T), N, Env),
    {As, N1} = label_ann(T, N),
    T1 = set_ann(T, As),
    {Ts1, N2, Env2} = label_vars(Ts, N1, Env1),
    {[T1 | Ts1], N2, Env2};
label_vars([], N, Env) ->
    {[], N, Env}.

label_defs([{F, T} | Ds], N, Env) ->
    {F1, N1} = label(F, N, Env),
    {T1, N2} = label(T, N1, Env),
    {Ds1, N3} = label_defs(Ds, N2, Env),
    {[{F1, T1} | Ds1], N3};
label_defs([], N, _Env) ->
    {[], N}.

label_ann(T, N) ->
    {[{label, N} | filter_labels(get_ann(T))], N + 1}.

filter_labels([{label, _} | As]) ->
    filter_labels(As);
filter_labels([A | As]) ->
    [A | filter_labels(As)];
filter_labels([]) ->
    [].

-spec get_label(cerl:cerl()) -> 'top' | integer().

get_label(T) ->
    case get_ann(T) of
	[{label, L} | _] -> L;
	_ -> throw({missing_label, T})
    end.
