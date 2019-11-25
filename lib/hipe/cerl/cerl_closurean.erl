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
%% @doc Closure analysis of Core Erlang programs.

%% TODO: might need a "top" (`any') element for any-length value lists.

-module(cerl_closurean).

-export([analyze/1, annotate/1]).
%% The following functions are exported from this module since they
%% are also used by Dialyzer (file dialyzer/src/dialyzer_dep.erl)
-export([is_escape_op/2, is_escape_op/3, is_literal_op/2, is_literal_op/3]).

-import(cerl, [ann_c_apply/3, ann_c_fun/3, ann_c_var/2, apply_args/1,
	       apply_op/1, atom_val/1, bitstr_size/1, bitstr_val/1,
	       binary_segments/1, c_letrec/2, c_seq/2, c_tuple/1,
	       c_nil/0, call_args/1, call_module/1, call_name/1,
	       case_arg/1, case_clauses/1, catch_body/1, clause_body/1,
	       clause_guard/1, clause_pats/1, cons_hd/1, cons_tl/1,
	       fun_body/1, fun_vars/1, get_ann/1, is_c_atom/1,
	       let_arg/1, let_body/1, let_vars/1, letrec_body/1,
	       letrec_defs/1, module_defs/1, module_defs/1,
	       module_exports/1, pat_vars/1, primop_args/1,
	       primop_name/1, receive_action/1, receive_clauses/1,
	       receive_timeout/1, seq_arg/1, seq_body/1, set_ann/2,
	       try_arg/1, try_body/1, try_vars/1, try_evars/1,
	       try_handler/1, tuple_es/1, type/1, values_es/1]).

-import(cerl_trees, [get_label/1]).

%% ===========================================================================

-type label()    :: integer() | 'top' | 'external' | 'external_call'.
-type ordset(X)  :: [X].  % XXX: TAKE ME OUT
-type labelset() :: ordset(label()).
-type outlist()  :: [labelset()] | 'none'.
-type escapes()  :: labelset().

%% ===========================================================================
%% annotate(Tree) -> {Tree1, OutList, Outputs, Escapes, Dependencies, Parents}
%%
%%	    Tree = cerl:cerl()
%%
%%	Analyzes `Tree' (see `analyze') and appends terms `{callers,
%%	Labels}' and `{calls, Labels}' to the annotation list of each
%%	fun-expression node and apply-expression node of `Tree',
%%	respectively, where `Labels' is an ordered-set list of labels of
%%	fun-expressions in `Tree', possibly also containing the atom
%%	`external', corresponding to the dependency information derived
%%	by the analysis. Any previous such annotations are removed from
%%	`Tree'. `Tree1' is the modified tree; for details on `OutList',
%%	`Outputs' , `Dependencies', `Escapes' and `Parents', see
%%	`analyze'.
%%
%%	Note: `Tree' must be annotated with labels in order to use this
%%	function; see `analyze' for details.

-spec annotate(cerl:cerl()) ->
        {cerl:cerl(), outlist(), dict:dict(),
         escapes(), dict:dict(), dict:dict()}.

annotate(Tree) ->
    {Xs, Out, Esc, Deps, Par} = analyze(Tree),
    F = fun (T) ->
		case type(T) of
		    'fun' ->
			L = get_label(T),
			X = case dict:find(L, Deps) of
				{ok, X1} -> X1;
				error -> set__new()
			    end,
			set_ann(T, append_ann(callers,
					      set__to_list(X),
					      get_ann(T)));
		    apply ->
			L = get_label(T),
			X = case dict:find(L, Deps) of
				{ok, X1} -> X1;
				error -> set__new()
			    end,
			set_ann(T, append_ann(calls,
					      set__to_list(X),
					      get_ann(T)));
		    _ ->
%%%			set_ann(T, [])   % debug
			T
		end
	end,
    {cerl_trees:map(F, Tree), Xs, Out, Esc, Deps, Par}.

append_ann(Tag, Val, [X | Xs]) ->
    if tuple_size(X) >= 1, element(1, X) =:= Tag -> 
	    append_ann(Tag, Val, Xs);
       true ->
	    [X | append_ann(Tag, Val, Xs)]
    end;
append_ann(Tag, Val, []) ->
    [{Tag, Val}].

%% =====================================================================
%% analyze(Tree) -> {OutList, Outputs, Escapes, Dependencies, Parents}
%%
%%	    Tree = cerl()
%%	    OutList = [LabelSet] | none
%%	    Outputs = dict(Label, OutList)
%%	    Escapes = LabelSet
%%	    Dependencies = dict(Label, LabelSet)
%%	    LabelSet = ordset(Label)
%%	    Label = integer() | top | external | external_call
%%	    Parents = dict(Label, Label)
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
%%	function outside `Tree', including those in `Escapes'. The atom
%%	`top' denotes the top-level expression `Tree'.
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
%%	`Parents' is a mapping from labels of fun-expressions in `Tree'
%%	to the corresponding label of the nearest containing
%%	fun-expression or top-level expression. This can be used to
%%	extend the dependency graph, for certain analyses.
%%
%%	Note: `Tree' must be annotated with labels (as done by the
%%	function `cerl_trees:label/1') in order to use this function.
%%	The label annotation `{label, L}' (where L should be an integer)
%%	must be the first element of the annotation list of each node in
%%	the tree. Instances of variables bound in `Tree' which denote
%%	the same variable must have the same label; apart from this,
%%	labels should be unique. Constant literals do not need to be
%%	labeled.

-record(state, {vars, out, dep, work, funs, par}).

%% Note: In order to keep our domain simple, we assume that all remote
%% calls and primops return a single value, if any.

%% We use the terms `closure', `label', `lambda' and `fun-expression'
%% interchangeably. The exact meaning in each case can be grasped from
%% the context.
%%
%% Rules:
%%   1) The implicit top level lambda escapes.
%%   2) A lambda returned by an escaped lambda also escapes.
%%   3) An escaped lambda can be passed an external lambda as argument.
%%   4) A lambda passed as argument to an external lambda also escapes.
%%   5) An argument passed to an unknown operation escapes.
%%   6) A call to an unknown operation can return an external lambda.
%%
%% Escaped lambdas become part of the set of external lambdas, but this
%% does not need to be represented explicitly.

%% We wrap the given syntax tree T in a fun-expression labeled `top',
%% which is initially in the set of escaped labels. `top' will be
%% visited at least once.
%%
%% We create a separate function labeled `external', defined as:
%% "'external'/1 = fun (Escape) -> do apply 'external'/1(apply Escape())
%% 'external'/1", which will represent any and all functions outside T,
%% and which returns itself, and contains a recursive call; this models
%% rules 2 and 4 above. It will be revisited if the set of escaped
%% labels changes, or at least once. Its parameter `Escape' is a
%% variable labeled `escape', which will hold the set of escaped labels.
%% initially it contains `top' and `external'.

-spec analyze(cerl:cerl()) ->
        {outlist(), dict:dict(), escapes(), dict:dict(), dict:dict()}.

analyze(Tree) ->
    %% Note that we use different name spaces for variable labels and
    %% function/call site labels, so we can reuse some names here. We
    %% assume that the labeling of Tree only uses integers, not atoms.
    External = ann_c_var([{label, external}], {external, 1}),
    Escape = ann_c_var([{label, escape}], 'Escape'),
    ExtBody = c_seq(ann_c_apply([{label, loop}], External,
				[ann_c_apply([{label, external_call}],
					     Escape, [])]),
		    External),
    ExtFun = ann_c_fun([{label, external}], [Escape], ExtBody),
%%%     io:fwrite("external fun:\n~s.\n",
%%% 	      [cerl_prettypr:format(ExtFun, [noann])]),
    Top = ann_c_var([{label, top}], {top, 0}),
    TopFun = ann_c_fun([{label, top}], [], Tree),

    %% The "start fun" just makes the initialisation easier. It will not
    %% be marked as escaped, and thus cannot be called.
    StartFun =  ann_c_fun([{label, start}], [],
			  c_letrec([{External, ExtFun}, {Top, TopFun}],
				   c_nil())),
%%%     io:fwrite("start fun:\n~s.\n",
%%% 	      [cerl_prettypr:format(StartFun, [noann])]),

    %% Gather a database of all fun-expressions in Tree and initialise
    %% all their outputs and parameter variables. Bind all module- and
    %% letrec-defined variables to their corresponding labels.
    Funs0 = dict:new(),
    Vars0 = dict:new(),
    Out0 = dict:new(),
    Empty = empty(),
    F = fun (T, S = {Fs, Vs, Os}) ->
		case type(T) of
		    'fun' ->
			L = get_label(T),
			As = fun_vars(T),
			{dict:store(L, T, Fs),
			 bind_vars_single(As, Empty, Vs),
			 dict:store(L, none, Os)};
		    letrec ->
			{Fs, bind_defs(letrec_defs(T), Vs), Os};
		    module ->
			{Fs, bind_defs(module_defs(T), Vs), Os};
		    _ ->
			S
		end
	end,
    {Funs, Vars, Out} = cerl_trees:fold(F, {Funs0, Vars0, Out0},
					StartFun),

    %% Initialise Escape to the minimal set of escaped labels.
    Vars1 = dict:store(escape, from_label_list([top, external]), Vars),

    %% Enter the fixpoint iteration at the StartFun.
    St = loop(StartFun, start, #state{vars = Vars1,
				      out = Out,
				      dep = dict:new(),
				      work = init_work(),
				      funs = Funs,
				      par = dict:new()}),
%%%     io:fwrite("dependencies: ~p.\n",
%%%  	      [[{X, set__to_list(Y)}
%%%   		|| {X, Y} <- dict:to_list(St#state.dep)]]),
    {dict:fetch(top, St#state.out),
     tidy_dict([start, top, external], St#state.out),
     dict:fetch(escape, St#state.vars),
     tidy_dict([loop], St#state.dep),
     St#state.par}.

tidy_dict([X | Xs], D) ->
    tidy_dict(Xs, dict:erase(X, D));
tidy_dict([], D) ->
    D.

loop(T, L, St0) ->
%%%     io:fwrite("analyzing: ~w.\n", [L]),
%%%     io:fwrite("work: ~w.\n", [St0#state.work]),
    Xs0 = dict:fetch(L, St0#state.out),
    {Xs, St1} = visit(fun_body(T), L, St0),
    {W, M} = case equal(Xs0, Xs) of
		 true ->
		     {St1#state.work, St1#state.out};
		 false ->
%%%  		     io:fwrite("out (~w) changed: ~w <- ~w.\n",
%%%  			       [L, Xs, Xs0]),
		     M1 = dict:store(L, Xs, St1#state.out),
		     case dict:find(L, St1#state.dep) of
			 {ok, S} ->
			     {add_work(set__to_list(S), St1#state.work),
			      M1};
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

visit(T, L, St) ->
    case type(T) of
	literal ->
	    {[empty()], St};
	var ->
	    %% If a variable is not already in the store here, we
	    %% initialize it to empty().
	    L1 = get_label(T),
	    Vars = St#state.vars,
	    case dict:find(L1, Vars) of
		{ok, X} ->
		    {[X], St};
		error ->
		    X = empty(),
		    St1 = St#state{vars = dict:store(L1, X, Vars)},
		    {[X], St1}
	    end;
	'fun' ->
	    %% Must revisit the fun also, because its environment might
	    %% have changed. (We don't keep track of such dependencies.)
	    L1 = get_label(T),
	    St1 = St#state{work = add_work([L1], St#state.work),
			   par = set_parent([L1], L, St#state.par)},
	    {[singleton(L1)], St1};
	values ->
	    visit_list(values_es(T), L, St);
	cons ->
	    {Xs, St1} = visit_list([cons_hd(T), cons_tl(T)], L, St),
	    {[join_single_list(Xs)], St1};
	tuple ->
	    {Xs, St1} = visit_list(tuple_es(T), L, St),
	    {[join_single_list(Xs)], St1};
	'let' ->
	    {Xs, St1} = visit(let_arg(T), L, St),
	    Vars = bind_vars(let_vars(T), Xs, St1#state.vars),
	    visit(let_body(T), L, St1#state{vars = Vars});
	seq ->
	    {_, St1} = visit(seq_arg(T), L, St),
	    visit(seq_body(T), L, St1);
	apply ->
	    {Xs, St1} = visit(apply_op(T), L, St),
	    {As, St2} = visit_list(apply_args(T), L, St1),
	    case Xs of
		[X] ->
		    %% We store the dependency from the call site to the
		    %% called functions
		    Ls = set__to_list(X),
		    Out = St2#state.out,
		    Xs1 = join_list([dict:fetch(Lx, Out) || Lx <- Ls]),
		    St3 = call_site(Ls, L, As, St2),
		    L1 = get_label(T),
		    D = dict:store(L1, X, St3#state.dep),
		    {Xs1, St3#state{dep = D}};
		none ->
		    {none, St2}
	    end;
	call ->
	    M = call_module(T),
	    F = call_name(T),
	    {_, St1} = visit(M, L, St),
	    {_, St2} = visit(F, L, St1),
	    {Xs, St3} = visit_list(call_args(T), L, St2),
	    remote_call(M, F, Xs, St3);
	primop ->
	    As = primop_args(T),
	    {Xs, St1} = visit_list(As, L, St),
	    primop_call(atom_val(primop_name(T)), length(Xs), Xs, St1);
	'case' ->
	    {Xs, St1} = visit(case_arg(T), L, St),
	    visit_clauses(Xs, case_clauses(T), L, St1);
	'receive' ->
	    X = singleton(external),
	    {Xs1, St1} = visit_clauses([X], receive_clauses(T), L, St),
	    {_, St2} = visit(receive_timeout(T), L, St1),
	    {Xs2, St3} = visit(receive_action(T), L, St2),
	    {join(Xs1, Xs2), St3};
	'try' ->
	    {Xs1, St1} = visit(try_arg(T), L, St),
	    X = singleton(external),
	    Vars = bind_vars(try_vars(T), [X], St1#state.vars),
	    {Xs2, St2} = visit(try_body(T), L, St1#state{vars = Vars}),
	    Evars = bind_vars(try_evars(T), [X, X, X], St2#state.vars),
	    {Xs3, St3} = visit(try_handler(T), L, St2#state{vars = Evars}),
	    {join(join(Xs1, Xs2), Xs3), St3};
	'catch' ->
	    {_, St1} = visit(catch_body(T), L, St),
	    {[singleton(external)], St1};
	binary ->
	    {_, St1} = visit_list(binary_segments(T), L, St),
	    {[empty()], St1};
	bitstr ->
	    %% The other fields are constant literals.
	    {_, St1} = visit(bitstr_val(T), L, St),
	    {_, St2} = visit(bitstr_size(T), L, St1),
	    {none, St2};
	letrec ->
	    %% All the bound funs should be revisited, because the
	    %% environment might have changed.
	    Ls = [get_label(F) || {_, F} <- letrec_defs(T)],
	    St1 = St#state{work = add_work(Ls, St#state.work),
			   par = set_parent(Ls, L, St#state.par)},
	    visit(letrec_body(T), L, St1);
	module ->
	    %% All the exported functions escape, and can thus be passed
	    %% any external closures as arguments. We regard a module as
	    %% a tuple of function variables in the body of a `letrec'.
	    visit(c_letrec(module_defs(T), c_tuple(module_exports(T))),
		  L, St)
    end.

visit_clause(T, Xs, L, St) ->
    Vars = bind_pats(clause_pats(T), Xs, St#state.vars),
    {_, St1} = visit(clause_guard(T), L, St#state{vars = Vars}),
    visit(clause_body(T), L, St1).

%% We assume correct value-list typing.

visit_list([T | Ts], L, St) ->
    {Xs, St1} = visit(T, L, St),
    {Xs1, St2} = visit_list(Ts, L, St1),
    X = case Xs of
	    [X1] -> X1;
	    none -> none
	end,
    {[X | Xs1], St2};
visit_list([], _L, St) ->
    {[], St}.

visit_clauses(Xs, [T | Ts], L, St) ->
    {Xs1, St1} = visit_clause(T, Xs, L, St),
    {Xs2, St2} = visit_clauses(Xs, Ts, L, St1),
    {join(Xs1, Xs2), St2};
visit_clauses(_, [], _L, St) ->
    {none, St}.

bind_defs([{V, F} | Ds], Vars) ->
    bind_defs(Ds, dict:store(get_label(V), singleton(get_label(F)),
			     Vars));
bind_defs([], Vars) ->
    Vars.

bind_pats(Ps, none, Vars) ->
    bind_pats_single(Ps, empty(), Vars);
bind_pats(Ps, Xs, Vars) ->
    if length(Xs) =:= length(Ps) ->
	    bind_pats_list(Ps, Xs, Vars);
       true ->
	    bind_pats_single(Ps, empty(), Vars)
    end.

bind_pats_list([P | Ps], [X | Xs], Vars) ->
    bind_pats_list(Ps, Xs, bind_vars_single(pat_vars(P), X, Vars));
bind_pats_list([], [], Vars) ->
    Vars.

bind_pats_single([P | Ps], X, Vars) ->
    bind_pats_single(Ps, X, bind_vars_single(pat_vars(P), X, Vars));
bind_pats_single([], _X, Vars) ->
    Vars.

bind_vars(Vs, none, Vars) ->
    bind_vars_single(Vs, empty(), Vars);
bind_vars(Vs, Xs, Vars) ->
    if length(Vs) =:= length(Xs) ->
	    bind_vars_list(Vs, Xs, Vars);
       true ->
	    bind_vars_single(Vs, empty(), Vars)
    end.

bind_vars_list([V | Vs], [X | Xs], Vars) ->
    bind_vars_list(Vs, Xs, dict:store(get_label(V), X, Vars));
bind_vars_list([], [], Vars) ->
    Vars.

bind_vars_single([V | Vs], X, Vars) ->
    bind_vars_single(Vs, X, dict:store(get_label(V), X, Vars));
bind_vars_single([], _X, Vars) ->
    Vars.

%% This handles a call site - adding dependencies and updating parameter
%% variables with respect to the actual parameters. The 'external'
%% function is handled specially, since it can get an arbitrary number
%% of arguments, which must be unified into a single argument.

call_site(Ls, L, Xs, St) ->
%%%     io:fwrite("call site: ~w -> ~w (~w).\n", [L, Ls, Xs]),
    {D, W, V} = call_site(Ls, L, Xs, St#state.dep, St#state.work,
			  St#state.vars, St#state.funs),
    St#state{dep = D, work = W, vars = V}.

call_site([external | Ls], T, Xs, D, W, V, Fs) ->
    D1 = add_dep(external, T, D),
    X = join_single_list(Xs),
    case bind_arg(escape, X, V) of
	{V1, true} ->
%%%   	    io:fwrite("escape changed: ~w <- ~w + ~w.\n",
%%%   		      [dict:fetch(escape, V1), dict:fetch(escape, V),
%%%   		       X]),
	    {W1, V2} = update_esc(set__to_list(X), W, V1, Fs),
	    call_site(Ls, T, Xs, D1, add_work([external], W1), V2, Fs);
	{V1, false} ->
	    call_site(Ls, T, Xs, D1, W, V1, Fs)
    end;
call_site([L | Ls], T, Xs, D, W, V, Fs) ->
    D1 = add_dep(L, T, D),
    Vs = fun_vars(dict:fetch(L, Fs)),
    case bind_args(Vs, Xs, V) of
	{V1, true} ->
	    call_site(Ls, T, Xs, D1, add_work([L], W), V1, Fs);
	{V1, false} ->
	    call_site(Ls, T, Xs, D1, W, V1, Fs)
    end;
call_site([], _, _, D, W, V, _) ->
    {D, W, V}.

%% Note that `visit' makes sure all lambdas are visited at least once.
%% For every called function, we add a dependency from the *called*
%% function to the function containing the call site.

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

%% If the arity does not match the call, nothing is done here.

bind_args(Vs, Xs, Vars) ->
    if length(Vs) =:= length(Xs) ->
	    bind_args(Vs, Xs, Vars, false);
       true ->
	    {Vars, false}
    end.

bind_args([V | Vs], [X | Xs], Vars, Ch) ->
    L = get_label(V),
    {Vars1, Ch1} = bind_arg(L, X, Vars, Ch),
    bind_args(Vs, Xs, Vars1, Ch1);
bind_args([], [], Vars, Ch) ->
    {Vars, Ch}.

bind_args_single(Vs, X, Vars) ->
    bind_args_single(Vs, X, Vars, false).

bind_args_single([V | Vs], X, Vars, Ch) ->
    L = get_label(V),
    {Vars1, Ch1} = bind_arg(L, X, Vars, Ch),
    bind_args_single(Vs, X, Vars1, Ch1);
bind_args_single([], _, Vars, Ch) ->
    {Vars, Ch}.

bind_arg(L, X, Vars) ->
    bind_arg(L, X, Vars, false).

bind_arg(L, X, Vars, Ch) ->
    X0 = dict:fetch(L, Vars),
    X1 = join_single(X, X0),
    case equal_single(X0, X1) of
	true ->
	    {Vars, Ch};
	false ->
%%% 	    io:fwrite("arg (~w) changed: ~w <- ~w + ~w.\n",
%%% 		      [L, X1, X0, X]),
	    {dict:store(L, X1, Vars), true}
    end.

%% This handles escapes from things like primops and remote calls.

%% escape(none, St) ->
%%    St;
escape([X], St) ->
    Vars = St#state.vars,
    X0 = dict:fetch(escape, Vars),
    X1 = join_single(X, X0),
    case equal_single(X0, X1) of
	true ->
	    St;
	false ->
%%% 	    io:fwrite("escape changed: ~w <- ~w + ~w.\n", [X1, X0, X]),
%%% 	    io:fwrite("updating escaping funs: ~w.\n", [set__to_list(X)]),
	    Vars1 = dict:store(escape, X1, Vars),
	    {W, Vars2} = update_esc(set__to_list(set__subtract(X, X0)),
				    St#state.work, Vars1,
				    St#state.funs),
	    St#state{work = add_work([external], W), vars = Vars2}
    end.

%% For all escaping lambdas, since they might be called from outside the
%% program, all their arguments may be an external lambda. (Note that we
%% only have to include the `external' label once per escaping lambda.)
%% If the escape set has changed, we need to revisit the `external' fun.

update_esc(Ls, W, V, Fs) ->
    update_esc(Ls, singleton(external), W, V, Fs).

%% The external lambda is skipped here - the Escape variable is known to
%% contain `external' from the start.

update_esc([external | Ls], X, W, V, Fs) ->
    update_esc(Ls, X, W, V, Fs);
update_esc([L | Ls], X, W, V, Fs) ->
    Vs = fun_vars(dict:fetch(L, Fs)),
    case bind_args_single(Vs, X, V) of
	{V1, true} ->
	    update_esc(Ls, X, add_work([L], W), V1, Fs);
	{V1, false} ->
	    update_esc(Ls, X, W, V1, Fs)
    end;
update_esc([], _, W, V, _) ->
    {W, V}.

set_parent([L | Ls], L1, D) ->
    set_parent(Ls, L1, dict:store(L, L1, D));
set_parent([], _L1, D) ->
    D.

%% Handle primop calls: (At present, we assume that all unknown primops
%% yield exactly one value. This might have to be changed.)

primop_call(F, A, Xs, St0) ->
    case is_pure_op(F, A) of
	%% XXX: this case is currently not possible -- commented out.
	%% true ->
	%%    case is_literal_op(F, A) of
	%%	true -> {[empty()], St0};
	%%	false -> {[join_single_list(Xs)], St0}
	%%    end;
	false ->
	    St1 = case is_escape_op(F, A) of
		      true -> escape([join_single_list(Xs)], St0);
		      false -> St0
		  end,
	    case is_literal_op(F, A) of
		true -> {none, St1};
		false -> {[singleton(external)], St1}
	    end
    end.

%% Handle remote-calls: (At present, we assume that all unknown calls
%% yield exactly one value. This might have to be changed.)

remote_call(M, F, Xs, St) ->
    case is_c_atom(M) andalso is_c_atom(F) of
	true ->
	    remote_call_1(atom_val(M), atom_val(F), length(Xs), Xs, St);
	false ->
	    %% Unknown function
	    {[singleton(external)], escape([join_single_list(Xs)], St)}
    end.

remote_call_1(M, F, A, Xs, St0) ->
    case is_pure_op(M, F, A) of
	true ->
	    case is_literal_op(M, F, A) of
		true -> {[empty()], St0};
		false -> {[join_single_list(Xs)], St0}
	    end;
	false ->
	    St1 = case is_escape_op(M, F, A) of
		      true -> escape([join_single_list(Xs)], St0);
		      false -> St0
		  end,
	    case is_literal_op(M, F, A) of
		true -> {[empty()], St1};
		false -> {[singleton(external)], St1}
	    end
    end.

%% Domain: none | [Vs], where Vs = set(integer()).

join(none, Xs2) -> Xs2;
join(Xs1, none) -> Xs1;
join(Xs1, Xs2) ->
    if length(Xs1) =:= length(Xs2) ->
	    join_1(Xs1, Xs2);
       true ->
	    none
    end.

join_1([X1 | Xs1], [X2 | Xs2]) ->
    [join_single(X1, X2) | join_1(Xs1, Xs2)];
join_1([], []) ->
    [].

empty() -> set__new().

singleton(X) -> set__singleton(X).

from_label_list(X) -> set__from_list(X).

join_single(none, Y) -> Y;
join_single(X, none) -> X;
join_single(X, Y) -> set__union(X, Y).

join_list([Xs | Xss]) ->
    join(Xs, join_list(Xss));
join_list([]) ->
    none.

join_single_list([X | Xs]) ->
    join_single(X, join_single_list(Xs));
join_single_list([]) ->
    empty().

equal(none, none) -> true;
equal(none, _) -> false;
equal(_, none) -> false;
equal(X1, X2) -> equal_1(X1, X2).

equal_1([X1 | Xs1], [X2 | Xs2]) ->
    equal_single(X1, X2) andalso equal_1(Xs1, Xs2);
equal_1([], []) -> true;
equal_1(_, _) -> false.

equal_single(X, Y) -> set__equal(X, Y).

%% Set abstraction for label sets in the domain.

set__new() -> [].

set__singleton(X) -> [X].

set__to_list(S) -> S.

set__from_list(S) -> ordsets:from_list(S).

set__union(X, Y) -> ordsets:union(X, Y).

set__add(X, S) -> ordsets:add_element(X, S).

set__is_member(X, S) -> ordsets:is_element(X, S).    

set__subtract(X, Y) -> ordsets:subtract(X, Y).

set__equal(X, Y) -> X =:= Y.

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
    {queue__new(), sets:new()}.

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

%% Escape operators may let their arguments escape. Unless we know
%% otherwise, and the function is not pure, we assume this is the case.
%% Error-raising functions (fault/match_fail) are not considered as
%% escapes (but throw/exit are). Zero-argument functions need not be
%% listed.

-spec is_escape_op(atom(), arity()) -> boolean().

is_escape_op(match_fail, 1) -> false;
is_escape_op(recv_wait_timeout, 1) -> false;
is_escape_op(F, A) when is_atom(F), is_integer(A) -> true.

-spec is_escape_op(atom(), atom(), arity()) -> boolean().

is_escape_op(erlang, error, 1) -> false;
is_escape_op(erlang, error, 2) -> false;
is_escape_op(M, F, A) when is_atom(M), is_atom(F), is_integer(A) -> true.

%% "Literal" operators will never return functional values even when
%% found in their arguments. Unless we know otherwise, we assume this is
%% not the case. (More functions can be added to this list, if needed
%% for better precision. Note that the result of `term_to_binary' still
%% contains an encoding of the closure.)

-spec is_literal_op(atom(), arity()) -> boolean().

is_literal_op(recv_wait_timeout, 1) -> true;
is_literal_op(match_fail, 1) -> true;
is_literal_op(F, A) when is_atom(F), is_integer(A) -> false.

-spec is_literal_op(atom(), atom(), arity()) -> boolean().

is_literal_op(erlang, '+', 2) -> true;
is_literal_op(erlang, '-', 2) -> true;
is_literal_op(erlang, '*', 2) -> true;
is_literal_op(erlang, '/', 2) -> true;
is_literal_op(erlang, '=:=', 2) -> true;
is_literal_op(erlang, '==', 2) -> true;
is_literal_op(erlang, '=/=', 2) -> true;
is_literal_op(erlang, '/=', 2) -> true;
is_literal_op(erlang, '<', 2) -> true;
is_literal_op(erlang, '=<', 2) -> true;
is_literal_op(erlang, '>', 2) -> true;
is_literal_op(erlang, '>=', 2) -> true;
is_literal_op(erlang, 'and', 2) -> true;
is_literal_op(erlang, 'or', 2) -> true;
is_literal_op(erlang, 'not', 1) -> true;
is_literal_op(erlang, length, 1) -> true;
is_literal_op(erlang, size, 1) -> true;
is_literal_op(erlang, fun_info, 1) -> true;
is_literal_op(erlang, fun_info, 2) -> true;
is_literal_op(erlang, fun_to_list, 1) -> true;
is_literal_op(erlang, throw, 1) -> true;
is_literal_op(erlang, exit, 1) -> true;
is_literal_op(erlang, error, 1) -> true;
is_literal_op(erlang, error, 2) -> true;
is_literal_op(M, F, A) when is_atom(M), is_atom(F), is_integer(A) -> false.

%% Pure functions neither affect the state, nor depend on it.

is_pure_op(F, A) when is_atom(F), is_integer(A) -> false.

is_pure_op(M, F, A) -> erl_bifs:is_pure(M, F, A).

%% =====================================================================
