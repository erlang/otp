%% -*- erlang-indent-level: 4 -*-
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
%% @copyright 2001-2002 Richard Carlsson
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @doc Type analysis of Core Erlang programs.

%% TODO: filters must handle conjunctions for better precision!
%% TODO: should get filters from patterns as well as guards.
%% TODO: unused functions are being included in the analysis.

-module(cerl_typean).

-export([core_transform/2, analyze/1, pp_hook/0]).
%%-export([analyze/2, analyze/5, annotate/1, annotate/2, annotate/5]).

-import(erl_types, [t_any/0, t_atom/0, t_atom_vals/1, t_binary/0,
		    t_cons/2, t_cons_hd/1, t_cons_tl/1, t_float/0,
		    t_fun/0, t_fun/2, t_from_range/2, t_from_term/1,
		    t_inf/2, t_integer/0,
		    t_is_any/1, t_is_atom/1, t_is_cons/1, t_is_list/1,
		    t_is_maybe_improper_list/1, t_is_none/1, t_is_tuple/1,
		    t_limit/2, t_list_elements/1, t_maybe_improper_list/0,
		    t_none/0, t_number/0, t_pid/0, t_port/0, t_product/1,
		    t_reference/0, t_sup/2, t_to_tlist/1, t_tuple/0, t_tuple/1,
		    t_tuple_args/1, t_tuple_size/1, t_tuple_subtypes/1]).

-import(cerl, [ann_c_fun/3, ann_c_var/2, alias_pat/1, alias_var/1,
	       apply_args/1, apply_op/1, atom_val/1, bitstr_size/1,
	       bitstr_val/1, bitstr_type/1, bitstr_flags/1, binary_segments/1, 
	       c_letrec/2, c_nil/0,
	       c_values/1, call_args/1, call_module/1, call_name/1,
	       case_arg/1, case_clauses/1, catch_body/1, clause_body/1,
	       clause_guard/1, clause_pats/1, concrete/1, cons_hd/1,
	       cons_tl/1, fun_body/1, fun_vars/1, get_ann/1, int_val/1,
	       is_c_atom/1, is_c_int/1, let_arg/1, let_body/1, let_vars/1,
	       letrec_body/1, letrec_defs/1, module_defs/1,
	       module_defs/1, module_exports/1, pat_vars/1,
	       primop_args/1, primop_name/1, receive_action/1,
	       receive_clauses/1, receive_timeout/1, seq_arg/1,
	       seq_body/1, set_ann/2, try_arg/1, try_body/1,
	       try_evars/1, try_handler/1, try_vars/1, tuple_arity/1,
	       tuple_es/1, type/1, values_es/1, var_name/1]).

-import(cerl_trees, [get_label/1]).

-ifdef(DEBUG).
-define(ANNOTATE(X), case erl_types:t_to_string(X) of Q when length(Q) < 255 -> list_to_atom(Q); Q -> Q end).
-else.
-define(ANNOTATE(X), X).
-endif.

%% Limit for type representation depth.
-define(DEF_LIMIT, 3).


%% @spec core_transform(Module::cerl_records(), Options::[term()]) ->
%%           cerl_records()
%%
%% @doc Annotates a module represented by records with type
%% information. See <code>annotate/1</code> for details.
%%
%% <p>Use the compiler option <code>{core_transform, cerl_typean}</code>
%% to insert this function as a compilation pass.</p>
%%
%% @see module/2

-spec core_transform(cerl:cerl(), [term()]) -> cerl:cerl().

core_transform(Code, _Opts) ->
    {Code1, _} = cerl_trees:label(cerl:from_records(Code)),
    %% io:fwrite("Running type analysis..."),
    %% {T1,_} = statistics(runtime),
    {Code2, _, _} = annotate(Code1),
    %% {T2,_} = statistics(runtime),
    %% io:fwrite("(~w ms).\n", [T2 - T1]),
    cerl:to_records(Code2).


%% =====================================================================
%% annotate(Tree) -> {Tree1, Type, Vars}
%%
%%	    Tree = cerl:cerl()
%%
%%	Analyzes `Tree' (see `analyze') and appends terms `{type, Type}'
%%	to the annotation list of each fun-expression node and
%%	apply-expression node of `Tree', respectively, where `Labels' is
%%	an ordered-set list of labels of fun-expressions in `Tree',
%%	possibly also containing the atom `external', corresponding to
%%	the dependency information derived by the analysis. Any previous
%%	such annotations are removed from `Tree'. `Tree1' is the
%%	modified tree; for details on `OutList', `Outputs' ,
%%	`Dependencies' and `Escapes', see `analyze'.
%%
%%	Note: `Tree' must be annotated with labels in order to use this
%%	function; see `analyze' for details.

annotate(Tree) ->
    annotate(Tree, ?DEF_LIMIT).

annotate(Tree, Limit) ->
    {_, _, Esc, Dep, Par} = cerl_closurean:analyze(Tree),
    annotate(Tree, Limit, Esc, Dep, Par).

annotate(Tree, Limit, Esc, Dep, Par) ->
    {Type, Out, Vars} = analyze(Tree, Limit, Esc, Dep, Par),
    DelAnn = fun (T) -> set_ann(T, delete_ann(type, get_ann(T))) end,
    SetType = fun (T, Dict) ->
		      case dict:find(get_label(T), Dict) of
			  {ok, X} ->
			      case t_is_any(X) of
				  true ->
				      DelAnn(T);
				  false ->
				      set_ann(T, append_ann(type,
							    ?ANNOTATE(X),
							    get_ann(T)))
			      end;
			  error ->
			      DelAnn(T)
		      end
	      end,
    F = fun (T) ->
		case type(T) of
		    var ->
			SetType(T, Vars);
		    apply ->
			SetType(T, Out);
		    call ->
			SetType(T, Out);
		    primop ->
			SetType(T, Out);
		    'fun' ->
			SetType(T, Out);
		    _ ->
			DelAnn(T)
		end
	end,
    {cerl_trees:map(F, Tree), Type, Vars}.

append_ann(Tag, Val, [X | Xs]) ->
    if tuple_size(X) >= 1, element(1, X) =:= Tag -> 
	    append_ann(Tag, Val, Xs);
       true ->
	    [X | append_ann(Tag, Val, Xs)]
    end;
append_ann(Tag, Val, []) ->
    [{Tag, Val}].

delete_ann(Tag, [X | Xs]) ->
    if tuple_size(X) >= 1, element(1, X) =:= Tag -> 
	    delete_ann(Tag, Xs);
       true ->
	    [X | delete_ann(Tag, Xs)]
    end;
delete_ann(_, []) ->
    [].


%% =====================================================================
%% analyze(Tree) -> {OutList, Outputs, Dependencies}
%%
%%	    Tree = cerl:cerl()
%%	    OutList = [LabelSet] | none
%%	    Outputs = dict(integer(), OutList)
%%	    Dependencies = dict(integer(), LabelSet)
%%	    LabelSet = ordset(Label)
%%	    Label = integer() | external
%%
%%	Analyzes a module or an expression represented by `Tree'.
%%
%%	The returned `OutList' is a list of sets of labels of
%%	fun-expressions which correspond to the possible closures in the
%%	value list produced by `Tree' (viewed as an expression; the
%%	"value" of a module contains its exported functions). The atom
%%	`none' denotes missing or conflicting information.
%%
%%	The atom `external' in any label set denotes any possible
%%	function outside `Tree', including those in `Escapes'.
%%
%%	`Outputs' is a mapping from the labels of fun-expressions in
%%	`Tree' to corresponding lists of sets of labels of
%%	fun-expressions (or the atom `none'), representing the possible
%%	closures in the value lists returned by the respective
%%	functions.
%%
%%	`Dependencies' is a similar mapping from the labels of
%%	fun-expressions and apply-expressions in `Tree' to sets of
%%	labels of corresponding fun-expressions which may contain call
%%	sites of the functions or be called from the call sites,
%%	respectively. Any such label not defined in `Dependencies'
%%	represents an unreachable function or a dead or faulty
%%	application.
%%
%%	`Escapes' is the set of labels of fun-expressions in `Tree' such
%%	that corresponding closures may be accessed from outside `Tree'.
%%
%%	Note: `Tree' must be annotated with labels (as done by the
%%	function `cerl_trees:label/1') in order to use this function.
%%	The label annotation `{label, L}' (where L should be an integer)
%%	must be the first element of the annotation list of each node in
%%	the tree. Instances of variables bound in `Tree' which denote
%%	the same variable must have the same label; apart from this,
%%	labels should be unique. Constant literals do not need to be
%%	labeled.

-record(state, {k, vars, out, dep, work, funs, envs}).

%% Note: In order to keep our domain simple, we assume that all remote
%% calls and primops return a single value, if any.

%% We wrap the given syntax tree T in a fun-expression labeled `top',
%% which is initially in the set of escaped labels. `top' will be
%% visited at least once.
%%
%% We create a separate function labeled `external', defined as:
%% "External = fun () -> Any", which will represent any and all
%% functions outside T, and whose return value has unknown type.

-type label()    :: integer() | 'external' | 'top'.
-type ordset(X)  :: [X].  % XXX: TAKE ME OUT
-type labelset() :: ordset(label()).
-type outlist()  :: [labelset()] | 'none'.

-spec analyze(cerl:cerl()) -> {outlist(), dict:dict(), dict:dict()}.

analyze(Tree) ->
    analyze(Tree, ?DEF_LIMIT).

analyze(Tree, Limit) ->
    {_, _, Esc, Dep, Par} = cerl_closurean:analyze(Tree),
    analyze(Tree, Limit, Esc, Dep, Par).

analyze(Tree, Limit, Esc0, Dep0, Par) ->
    %% Note that we use different name spaces for variable labels and
    %% function/call site labels. We assume that the labeling of Tree
    %% only uses integers, not atoms.
    LabelExtL = [{label, external}],
    External = ann_c_var(LabelExtL, {external, 1}),
    ExtFun = ann_c_fun(LabelExtL, [], ann_c_var([{label, any}], 'Any')),
%%%     io:fwrite("external fun:\n~s.\n",
%%% 	      [cerl_prettypr:format(ExtFun, [noann, {paper, 80}])]),
    LabelTopL = [{label, top}],
    Top = ann_c_var(LabelTopL, {top, 0}),
    TopFun = ann_c_fun(LabelTopL, [], Tree),

    %% The "start fun" just makes the initialisation easier. It is not
    %% itself in the call graph.
    StartFun =  ann_c_fun([{label, start}], [],
			  c_letrec([{External, ExtFun}, {Top, TopFun}],
				   c_nil())),
%%%     io:fwrite("start fun:\n~s.\n",
%%% 	      [cerl_prettypr:format(StartFun, [{paper, 80}])]),

    %% Gather a database of all fun-expressions in Tree and initialise
    %% their outputs and parameter variables. All escaping functions can
    %% receive any values as inputs. Also add an extra dependency edge
    %% from each fun-expression label to its parent fun-expression.
%%%     io:fwrite("Escape: ~p.\n",[Esc0]),
    Esc = sets:from_list(Esc0),
    Any = t_any(),
    None = t_none(),
    Funs0 = dict:new(),
    Vars0 = dict:store(any, Any, dict:new()),
    Out0 = dict:store(top, None,
		      dict:store(external, None, dict:new())),
    Envs0 = dict:store(top, dict:new(),
		       dict:store(external, dict:new(), dict:new())),
    F = fun (T, S = {Fs, Vs, Os, Es}) ->
		case type(T) of
		    'fun' ->
			L = get_label(T),
			As = fun_vars(T),
			X = case sets:is_element(L, Esc) of
				true -> Any;
				false -> None
			    end,
			{dict:store(L, T, Fs),
			 bind_vars_single(As, X, Vs),
			 dict:store(L, None, Os),
			 dict:store(L, dict:new(), Es)};
		    _ ->
			S
		end
	end,
    {Funs, Vars, Out, Envs} = cerl_trees:fold(F, {Funs0, Vars0, Out0,
						  Envs0}, StartFun),

    %% Add dependencies from funs to their parent funs.
    Dep = lists:foldl(fun ({L, L1}, D) -> add_dep(L, L1, D) end,
		      Dep0, dict:to_list(Par)),

    %% Enter the fixpoint iteration at the StartFun.
    St = loop(TopFun, top, #state{vars = Vars,
				  out = Out,
				  dep = Dep,
				  work = init_work(),
				  funs = Funs,
				  envs = Envs,
				  k = Limit}),
    {dict:fetch(top, St#state.out),
     tidy_dict([top, external], St#state.out),
     tidy_dict([any], St#state.vars)}.

tidy_dict([X | Xs], D) ->
    tidy_dict(Xs, dict:erase(X, D));
tidy_dict([], D) ->
    D.

loop(T, L, St0) ->
%%%     io:fwrite("analyzing: ~w.\n",[L]),
%%%     io:fwrite("work: ~w.\n", [Queue0]),
    Env = dict:fetch(L, St0#state.envs),
    X0 = dict:fetch(L, St0#state.out),
    {X1, St1} = visit(fun_body(T), Env, St0),
    X = limit(X1, St1#state.k),
    {W, M} = case equal(X0, X) of
		 true ->
		     {St1#state.work, St1#state.out};
		 false ->
%%%        		     io:fwrite("out (~w) changed: ~s <- ~s.\n",
%%%        			       [L, erl_types:t_to_string(X),
%%%    				erl_types:t_to_string(X0)]),
		     M1 = dict:store(L, X, St1#state.out),
		     case dict:find(L, St1#state.dep) of
			 {ok, S} ->
%%% 			     io:fwrite("adding work: ~w.\n", [S]),
			     {add_work(S, St1#state.work), M1};
			 error ->
			     {St1#state.work, M1}
		     end
	     end,
    St2 = St1#state{out = M},
    case take_work(W) of
	{ok, L1, W1} ->
 	    T1 = dict:fetch(L1, St2#state.funs),
 	    loop(T1, L1, St2#state{work = W1});
	none ->
	    St2
    end.

visit(T, Env, St) ->
    case type(T) of
	literal ->
	    {t_from_term(concrete(T)), St};
	var ->
	    %% If a variable is not already in the store at this point,
	    %% we initialize it to 'none()'.
	    L = get_label(T),
	    Vars = St#state.vars,
	    case dict:find(L, Vars) of
		{ok, X} ->
		    case dict:find(var_name(T), Env) of
			{ok, X1} ->
%%%   			    io:fwrite("filtered variable reference: ~w:~s.\n",
%%%   				      [var_name(T), erl_types:t_to_string(X1)]),
			    {meet(X, X1), St};
			error ->
			    {X, St}
		    end;
		error ->
		    X = t_none(),
		    Vars1 = dict:store(L, X, Vars),
		    St1 = St#state{vars = Vars1},
		    {X, St1}
	    end;
	'fun' ->
	    %% Must revisit the fun also, because its environment might
	    %% have changed. (We don't keep track of such dependencies.)
	    L = get_label(T),
	    Xs = [dict:fetch(get_label(V), St#state.vars)
		  || V <- fun_vars(T)],
	    X = dict:fetch(L, St#state.out),
	    St1 = St#state{work = add_work([L], St#state.work),
			   envs = dict:store(L, Env, St#state.envs)},
	    {t_fun(Xs, X), St1};
	values ->
	    {Xs, St1} = visit_list(values_es(T), Env, St),
	    {t_product(Xs), St1};
	cons ->
	    {[X1, X2], St1} = visit_list([cons_hd(T), cons_tl(T)], Env, St),
	    {t_cons(X1, X2), St1};
	tuple ->
	    {Xs, St1} = visit_list(tuple_es(T), Env, St),
	    {t_tuple(Xs), St1};
	'let' ->
	    {X, St1} = visit(let_arg(T), Env, St),
	    LetVars = let_vars(T),
	    St1Vars = St1#state.vars,
	    Vars = case t_is_any(X) orelse t_is_none(X) of
		       true ->
			   bind_vars_single(LetVars, X, St1Vars);
		       false ->
			   bind_vars(LetVars, t_to_tlist(X), St1Vars)
		   end,
	    visit(let_body(T), Env, St1#state{vars = Vars});
	seq ->
	    {_, St1} = visit(seq_arg(T), Env, St),
	    visit(seq_body(T), Env, St1);
	apply ->
	    {_F, St1} = visit(apply_op(T), Env, St),
	    {As, St2} = visit_list(apply_args(T), Env, St1),
	    L = get_label(T),
	    Ls = get_deps(L, St#state.dep),
	    Out = St2#state.out,
	    X = join_list([dict:fetch(L1, Out) || L1 <- Ls]),
	    Out1 = dict:store(L, X, Out),
	    {X, call_site(Ls, As, St2#state{out = Out1})};
	call ->
	    M = call_module(T),
	    F = call_name(T),
	    As = call_args(T),
	    {[X1, X2], St1} = visit_list([M, F], Env, St),
	    {Xs, St2} = visit_list(As, Env, St1),
%%% 	    io:fwrite("call: ~w:~w(~w).\n",[X1,X2,Xs]),
	    X = case {t_atom_vals(X1), t_atom_vals(X2)} of
		    {[M1], [F1]} ->
			A = length(As),
%%% 			io:fwrite("known call: ~w:~w/~w.\n",
%%% 				  [M1, F1, A]),
			call_type(M1, F1, A, Xs);
		    _ ->
			t_any()
		end,
	    L = get_label(T),
	    {X, St2#state{out = dict:store(L, X, St2#state.out)}};
	primop ->
	    As = primop_args(T),
	    {Xs, St1} = visit_list(As, Env, St),
	    F = atom_val(primop_name(T)),
	    A = length(As),
	    L = get_label(T),
	    X = primop_type(F, A, Xs),
	    {X, St1#state{out = dict:store(L, X, St1#state.out)}};
	'case' ->
	    {X, St1} = visit(case_arg(T), Env, St),
	    Xs = case t_is_any(X) orelse t_is_none(X) of
		     true ->
			 [X || _ <- cerl:case_clauses(T)];
		     false ->
			 t_to_tlist(X)
		 end,
	    join_visit_clauses(Xs, case_clauses(T), Env, St1);
	'receive' ->
	    Any = t_any(),
	    {X1, St1} = join_visit_clauses([Any], receive_clauses(T),
					   Env, St),
	    {X2, St2} = visit(receive_timeout(T), Env, St1),
	    case t_is_atom(X2) andalso (t_atom_vals(X2) =:= [infinity]) of
		true ->
		    {X1, St2};
		false ->
		    {X3, St3} = visit(receive_action(T), Env, St2),
		    {join(X1, X3), St3}
	    end;
	'try' ->
	    {X, St1} = visit(try_arg(T), Env, St),
	    Any = t_any(),
	    Atom = t_atom(),
	    TryVars = try_vars(T),
	    St1Vars = St1#state.vars,
	    Vars = case t_is_any(X) orelse t_is_none(X) of
		       true ->
			   bind_vars_single(TryVars, X, St1Vars);
		       false ->
			   bind_vars(TryVars, t_to_tlist(X), St1Vars)
		   end,
	    {X1, St2} = visit(try_body(T), Env, St1#state{vars = Vars}),
	    EVars = bind_vars(try_evars(T), [Atom, Any, Any], St2#state.vars),
	    {X2, St3} = visit(try_handler(T), Env, St2#state{vars = EVars}),
	    {join(X1, X2), St3};
	'catch' ->
	    {_, St1} = visit(catch_body(T), Env, St),
	    {t_any(), St1};
	binary ->
	    {_, St1} = visit_list(binary_segments(T), Env, St),
	    {t_binary(), St1};
	bitstr ->
	    %% The other fields are constant literals.
	    {_, St1} = visit(bitstr_val(T), Env, St),
	    {_, St2} = visit(bitstr_size(T), Env, St1),
	    {t_none(), St2};
	letrec ->
	    %% All the bound funs should be revisited, because the
	    %% environment might have changed.
	    Vars = bind_defs(letrec_defs(T), St#state.vars,
			     St#state.out),
	    Ls = [get_label(F) || {_, F} <- letrec_defs(T)],
	    St1 = St#state{work = add_work(Ls, St#state.work),
			   vars = Vars},
	    visit(letrec_body(T), Env, St1);
	module ->
	    %% We handle a module as a sequence of function variables in
	    %% the body of a `letrec'.
	    {_, St1} = visit(c_letrec(module_defs(T),
				      c_values(module_exports(T))),
			     Env, St),
	    {t_none(), St1}
    end.

visit_clause(T, Xs, Env, St) ->
    Env1 = Env,
    Vars = bind_pats(clause_pats(T), Xs, St#state.vars),
    G = clause_guard(T),
    {_, St1} = visit(G, Env1, St#state{vars = Vars}),
    Env2 = guard_filters(G, Env1),
    visit(clause_body(T), Env2, St1).

%% We assume correct value-list typing.

visit_list([T | Ts], Env, St) ->
    {X, St1} = visit(T, Env, St),
    {Xs, St2} = visit_list(Ts, Env, St1),
    {[X | Xs], St2};
visit_list([], _Env, St) ->
    {[], St}.

join_visit_clauses(Xs, [T | Ts], Env, St) ->
    {X1, St1} = visit_clause(T, Xs, Env, St),
    {X2, St2} = join_visit_clauses(Xs, Ts, Env, St1),
    {join(X1, X2), St2};
join_visit_clauses(_, [], _Env, St) ->
    {t_none(), St}.

bind_defs([{V, F} | Ds], Vars, Out) ->
    Xs = [dict:fetch(get_label(V1), Vars) || V1 <- fun_vars(F)],
    X = dict:fetch(get_label(F), Out),
    bind_defs(Ds, dict:store(get_label(V), t_fun(Xs, X), Vars), Out);
bind_defs([], Vars, _Out) ->
    Vars.

bind_pats(Ps, Xs, Vars) ->
    if length(Xs) =:= length(Ps) ->
	    bind_pats_list(Ps, Xs, Vars);
       true ->
	    bind_pats_single(Ps, t_none(), Vars)
    end.

bind_pats_list([P | Ps], [X | Xs], Vars) ->
    Vars1 = bind_pat_vars(P, X, Vars),
    bind_pats_list(Ps, Xs, Vars1);
bind_pats_list([], [], Vars) ->
    Vars.

bind_pats_single([P | Ps], X, Vars) ->
    bind_pats_single(Ps, X, bind_pat_vars(P, X, Vars));
bind_pats_single([], _X, Vars) ->
    Vars.

bind_pat_vars(P, X, Vars) ->
    case type(P) of
	var ->
	    dict:store(get_label(P), X, Vars);
	literal ->
	    Vars;
	cons ->
	    case t_is_cons(X) of
		true ->
		    %% If X is "nonempty proper list of X1", then the
		    %% head has type X1 and the tail has type "proper
		    %% list of X1". (If X is just "cons cell of X1",
		    %% then both head and tail have type X1.)
		    Vars1 = bind_pat_vars(cons_hd(P), t_cons_hd(X),
					  Vars),
		    bind_pat_vars(cons_tl(P), t_cons_tl(X), Vars1);
		false ->
		    case t_is_list(X) of
			true ->
			    %% If X is "proper list of X1", then the
			    %% head has type X1 and the tail has type
			    %% "proper list of X1", i.e., type X.
			    Vars1 = bind_pat_vars(cons_hd(P),
						  t_list_elements(X),
						  Vars),
			    bind_pat_vars(cons_tl(P), X, Vars1);
			false ->
			    case t_is_maybe_improper_list(X) of
				true ->
				    %% If X is "cons cell of X1", both
				    %% the head and tail have type X1.
				    X1 = t_list_elements(X),
				    Vars1 = bind_pat_vars(cons_hd(P),
							  X1, Vars),
				    bind_pat_vars(cons_tl(P), X1,
						  Vars1);
				false ->
				    bind_vars_single(pat_vars(P),
						     top_or_bottom(X),
						     Vars)
			    end
		    end
	    end;
	tuple ->
	    case t_is_tuple(X) of
		true ->
		    case t_tuple_subtypes(X) of
			unknown -> 
			    bind_vars_single(pat_vars(P), top_or_bottom(X), 
					     Vars);
			[Tuple] ->
			    case t_tuple_size(Tuple) =:= tuple_arity(P) of
				true ->
				    bind_pats_list(tuple_es(P), 
						   t_tuple_args(Tuple), Vars);
				
				false ->
				    bind_vars_single(pat_vars(P), 
						     top_or_bottom(X), Vars)
			    end;
			List when is_list(List) ->
			    bind_vars_single(pat_vars(P), top_or_bottom(X), 
					     Vars)
		    end;
		false ->
		    bind_vars_single(pat_vars(P), top_or_bottom(X), Vars)
	    end;
	binary ->
	    bind_pats_single(binary_segments(P), t_none(), Vars);
	bitstr ->
	    %% Only the Value field is a new binding. Size is already
	    %% bound, and the other fields are constant literals.
	    %% We could create a filter for Size being an integer().
	    Size = bitstr_size(P),
	    ValType = 
		case concrete(bitstr_type(P)) of
		    float -> t_float();
		    binary -> t_binary();
		    integer ->
			case is_c_int(Size) of
			    false -> t_integer();
			    true -> 
				SizeVal = int_val(Size),
				Flags = concrete(bitstr_flags(P)),
				case lists:member(signed, Flags) of
				    true -> 
					t_from_range(-(1 bsl (SizeVal - 1)),
						     1 bsl (SizeVal - 1) - 1);
				    false -> 
					t_from_range(0,1 bsl SizeVal - 1)
				end
			end
		end,
	    bind_pat_vars(bitstr_val(P), ValType, Vars);
	alias ->
	    P1 = alias_pat(P),
	    Vars1 = bind_pat_vars(P1, X, Vars),
	    dict:store(get_label(alias_var(P)), pat_type(P1, Vars1),
		       Vars1)
    end.

pat_type(P, Vars) ->
    case type(P) of
	var ->
	    dict:fetch(get_label(P), Vars);
	literal ->
	    t_from_term(concrete(P));
	cons ->
	    t_cons(pat_type(cons_hd(P), Vars), 
		   pat_type(cons_tl(P), Vars));
	tuple ->
	    t_tuple([pat_type(E, Vars) || E <- tuple_es(P)]);
	binary ->
	    t_binary();
	alias ->
	    pat_type(alias_pat(P), Vars)
    end.

bind_vars(Vs, Xs, Vars) ->
    if length(Vs) =:= length(Xs) ->
	    bind_vars_list(Vs, Xs, Vars);
       true ->
	    bind_vars_single(Vs, t_none(), Vars)
    end.

bind_vars_list([V | Vs], [X | Xs], Vars) ->
    bind_vars_list(Vs, Xs, dict:store(get_label(V), X, Vars));
bind_vars_list([], [], Vars) ->
    Vars.

bind_vars_single([V | Vs], X, Vars) ->
    bind_vars_single(Vs, X, dict:store(get_label(V), X, Vars));
bind_vars_single([], _X, Vars) ->
    Vars.

add_dep(Source, Target, Deps) ->
    case dict:find(Source, Deps) of
	{ok, X} ->
	    case set__is_member(Target, X) of
		true ->
		    Deps;
		false ->
%%%		    io:fwrite("new dep: ~w <- ~w.\n", [Target, Source]),
		    dict:store(Source, set__add(Target, X), Deps)
	    end;
	error ->
%%%	    io:fwrite("new dep: ~w <- ~w.\n", [Target, Source]),
	    dict:store(Source, set__singleton(Target), Deps)
    end.

%% This handles a call site, updating parameter variables with respect
%% to the actual parameters.

call_site(Ls, Xs, St) ->
%%     io:fwrite("call site: ~w ~s.\n",
%% 	      [Ls, erl_types:t_to_string(erl_types:t_product(Xs))]),
    {W, V} = call_site(Ls, Xs, St#state.work, St#state.vars,
		       St#state.funs, St#state.k),
    St#state{work = W, vars = V}.

call_site([L | Ls], Xs, W, V, Fs, Limit) ->
    Vs = fun_vars(dict:fetch(L, Fs)),
    case bind_args(Vs, Xs, V, Limit) of
	{V1, true} ->
	    call_site(Ls, Xs, add_work([L], W), V1, Fs, Limit);
	{V1, false} ->
	    call_site(Ls, Xs, W, V1, Fs, Limit)
    end;
call_site([], _, W, V, _, _) ->
    {W, V}.

%% If the arity does not match the call, nothing is done here.

bind_args(Vs, Xs, Vars, Limit) ->
    if length(Vs) =:= length(Xs) ->
	    bind_args(Vs, Xs, Vars, Limit, false);
       true ->
	    {Vars, false}
    end.

bind_args([V | Vs], [X | Xs], Vars, Limit, Ch) ->
    L = get_label(V),
    {Vars1, Ch1} = bind_arg(L, X, Vars, Limit, Ch),
    bind_args(Vs, Xs, Vars1, Limit, Ch1);
bind_args([], [], Vars, _Limit, Ch) ->
    {Vars, Ch}.

%% bind_arg(L, X, Vars, Limit) ->
%%     bind_arg(L, X, Vars, Limit, false).

bind_arg(L, X, Vars, Limit, Ch) ->
    X0 = dict:fetch(L, Vars),
    X1 = limit(join(X, X0), Limit),
    case equal(X0, X1) of
	true ->
	    {Vars, Ch};
	false ->
%%%     	    io:fwrite("arg (~w) changed: ~s <- ~s + ~s.\n",
%%%  		      [L, erl_types:t_to_string(X1),
%%% 		       erl_types:t_to_string(X0),
%%%  		       erl_types:t_to_string(X)]),
	    {dict:store(L, X1, Vars), true}
    end.

%% Domain: type(), defined in module `erl_types'.

meet(X, Y) -> t_inf(X, Y).

join(X, Y) -> t_sup(X, Y).

join_list([Xs | Xss]) ->
    join(Xs, join_list(Xss));
join_list([]) ->
    t_none().

equal(X, Y) -> X =:= Y.

limit(X, K) -> t_limit(X, K).

top_or_bottom(T) ->
    case t_is_none(T) of
	true ->
	    T;
	false ->
	    t_any()
    end.

strict(Xs, T) ->
    case erl_types:any_none(Xs) of
	true ->
	    t_none();
	false ->
	    T
    end.

%% Set abstraction for label sets.

%% set__new() -> [].

set__singleton(X) -> [X].

%% set__to_list(S) -> S.

%% set__from_list(S) -> ordsets:from_list(S).

%% set__union(X, Y) -> ordsets:union(X, Y).

set__add(X, S) -> ordsets:add_element(X, S).

set__is_member(X, S) -> ordsets:is_element(X, S).    

%% set__subtract(X, Y) -> ordsets:subtract(X, Y).

%% set__equal(X, Y) -> X =:= Y.

%% A simple but efficient functional queue.

queue__new() -> {[], []}.

queue__put(X, {In, Out}) -> {[X | In], Out}.

queue__get({In, [X | Out]}) -> {ok, X, {In, Out}};
queue__get({[], _}) -> empty;
queue__get({In, _}) ->
    [X | In1] = lists:reverse(In),
    {ok, X, {[], In1}}.

%% The work list - a queue without repeated elements.

init_work() ->
    {queue__put(external, queue__new()), sets:new()}.

add_work(Ls, {Q, Set}) ->
    add_work(Ls, Q, Set).

%% Note that the elements are enqueued in order.

add_work([L | Ls], Q, Set) ->
    case sets:is_element(L, Set) of
	true ->
	    add_work(Ls, Q, Set);
	false ->
	    add_work(Ls, queue__put(L, Q), sets:add_element(L, Set))
    end;
add_work([], Q, Set) ->
    {Q, Set}.

take_work({Queue0, Set0}) ->
    case queue__get(Queue0) of
	{ok, L, Queue1} ->
	    Set1 = sets:del_element(L, Set0),
	    {ok, L, {Queue1, Set1}};
	empty ->
	    none
    end.

get_deps(L, Dep) ->
    case dict:find(L, Dep) of
	{ok, Ls} -> Ls;
	error -> []
    end.

%% Type information for built-in functions. We do not check that the
%% arguments have the correct type; if the call would actually fail,
%% rather than return a value, this is a safe overapproximation.

primop_type(match_fail, 1, _) -> t_none();
primop_type(_, _, Xs) -> strict(Xs, t_any()).

call_type(M, F, A, Xs) ->
    erl_bif_types:type(M, F, A, Xs).

guard_filters(T, Env) ->
    guard_filters(T, Env, dict:new()).

guard_filters(T, Env, Vars) ->
    case type(T) of
	call ->
	    M = call_module(T),
	    F = call_name(T),
	    case is_c_atom(M) andalso is_c_atom(F) of
		true ->
		    As = call_args(T),
		    case {atom_val(M), atom_val(F), length(As)} of
			{erlang, 'and', 2} ->
			    [A1, A2] = As,
			    guard_filters(A1, guard_filters(A2, Env));
			{erlang, is_atom, 1} ->
			    filter(As, t_atom(), Env);
			{erlang, is_binary, 1} ->
			    filter(As, t_binary(), Env);
			{erlang, is_float, 1} ->
			    filter(As, t_float(), Env);
			{erlang, is_function, 1} ->
			    filter(As, t_fun(), Env);
			{erlang, is_integer, 1} ->
			    filter(As, t_integer(), Env);
			{erlang, is_list, 1} ->
			    filter(As, t_maybe_improper_list(), Env);
			{erlang, is_number, 1} ->
			    filter(As, t_number(), Env);
			{erlang, is_pid, 1} ->
			    filter(As, t_pid(), Env);
			{erlang, is_port, 1} ->
			    filter(As, t_port(), Env);
			{erlang, is_reference, 1} ->
			    filter(As, t_reference(), Env);
			{erlang, is_tuple, 1} ->
			    filter(As, t_tuple(), Env);
			_ ->
			    Env
		    end;
		false ->
		    Env
	    end;
	var ->
	    case dict:find(var_name(T), Vars) of
		{ok, T1} ->
		    guard_filters(T1, Env, Vars);
		error ->
		    Env
	    end;
	'let' ->
	    case let_vars(T) of
		[V] ->
		    guard_filters(let_body(T), Env,
				  dict:store(var_name(V), let_arg(T),
					     Vars));
		_ ->
		    Env
	    end;
	values ->
	    case values_es(T) of
		[T1] ->
		    guard_filters(T1, Env, Vars);
		_ ->
		    Env
	    end;
	_ ->
	    Env
    end.

filter(As, X, Env) ->
    [A] = As,
    case type(A) of
	var ->
	    V = var_name(A),
	    case dict:find(V, Env) of
		{ok, X1} ->
		    dict:store(V, meet(X, X1), Env);
		error ->
		    dict:store(V, X, Env)
	    end;
	_ ->
	    Env
    end.

%% Callback hook for cerl_prettypr:

-spec pp_hook() -> fun((cerl:cerl(), _, fun((_,_) -> any())) -> any()).

pp_hook() ->
    fun pp_hook/3.

pp_hook(Node, Ctxt, Cont) ->
    As = cerl:get_ann(Node),
    As1 = proplists:delete(type, proplists:delete(label, As)),
    As2 = proplists:delete(typesig, proplists:delete(file, As1)),
    D = Cont(cerl:set_ann(Node, []), Ctxt),
    T = case proplists:lookup(type, As) of
	    {type, T0} -> T0;
	    none ->
		case proplists:lookup(typesig, As) of
		    {typesig, T0} -> T0;
		    none -> t_any()
		end
	end,
    D1 = case erl_types:t_is_any(T) of
	     true ->
		 D;
	     false ->
		 case cerl:is_literal(Node) of
		     true ->
			 D;
		     false ->
			 S = erl_types:t_to_string(T),
			 Q = prettypr:beside(prettypr:text("::"),
					     prettypr:text(S)),
			 prettypr:beside(D, Q)
		 end
	 end,
    cerl_prettypr:annotate(D1, As2, Ctxt).

%% =====================================================================
