%% =====================================================================
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
%% Message analysis of Core Erlang programs.
%%
%% Copyright (C) 2002 Richard Carlsson
%%
%% Author contact: richardc@it.uu.se
%% =====================================================================

%% TODO: might need a "top" (`any') element for any-length value lists.

-module(cerl_messagean).

-export([annotate/1]).

-import(cerl, [alias_pat/1, alias_var/1, ann_c_var/2, ann_c_fun/3,
	       apply_args/1, apply_op/1, atom_val/1, bitstr_size/1,
	       bitstr_val/1, binary_segments/1, c_letrec/2,
	       ann_c_tuple/2, c_nil/0, call_args/1, call_module/1,
	       call_name/1, case_arg/1, case_clauses/1, catch_body/1,
	       clause_body/1, clause_guard/1, clause_pats/1, cons_hd/1,
	       cons_tl/1, fun_body/1, fun_vars/1, get_ann/1, int_val/1,
	       is_c_atom/1, is_c_int/1, let_arg/1, let_body/1,
	       let_vars/1, letrec_body/1, letrec_defs/1, module_defs/1,
	       module_defs/1, module_exports/1, pat_vars/1,
	       primop_args/1, primop_name/1, receive_action/1,
	       receive_clauses/1, receive_timeout/1, seq_arg/1,
	       seq_body/1, set_ann/2, try_arg/1, try_body/1, try_vars/1,
	       try_evars/1, try_handler/1, tuple_es/1, type/1,
	       values_es/1]).

-import(cerl_trees, [get_label/1]).

-define(DEF_LIMIT, 4).

%% -export([test/1, test1/1, ttest/1]).

%% ttest(F) ->
%%     {T, _} = cerl_trees:label(user_default:read(F)),
%%     {Time0, _} = erlang:statistics(runtime),
%%     analyze(T),
%%     {Time1, _} = erlang:statistics(runtime),
%%     Time1 - Time0.

%% test(F) ->
%%     {T, _} = cerl_trees:label(user_default:read(F)),
%%     {Time0, _} = erlang:statistics(runtime),
%%     {Esc, _Vars} = analyze(T),
%%     {Time1, _} = erlang:statistics(runtime),
%%     io:fwrite("messages: ~p.\n", [Esc]),
%%     Set = sets:from_list(Esc),
%%     H = fun (Node, Ctxt, Cont) ->
%% 		Doc = case get_ann(Node) of
%% 			  [{label, L} | _] ->
%% 			      B = sets:is_element(L, Set),
%% 			      bf(Node, Ctxt, Cont, B);
%% 			  _ ->
%% 			      bf(Node, Ctxt, Cont, false)
%% 		      end,
%% 		case type(Node) of
%% 		    cons -> color(Doc);
%% 		    tuple -> color(Doc);
%% 		    _ -> Doc
%% 		end
%% 	end,
%%     {ok, FD} = file:open("out.html",[write]),
%%     Txt = cerl_prettypr:format(T, [{hook, H},{user,false}]),
%%     io:put_chars(FD, "<pre>\n"),
%%     io:put_chars(FD, html(Txt)),
%%     io:put_chars(FD, "</pre>\n"),
%%     file:close(FD),
%%     {ok, Time1 - Time0}.

%% test1(F) ->
%%     {T, _} = cerl_trees:label(user_default:read(F)),
%%     {Time0, _} = erlang:statistics(runtime),
%%     {T1, Esc, Vars} = annotate(T),
%%     {Time1, _} = erlang:statistics(runtime),
%%     io:fwrite("messages: ~p.\n", [Esc]),
%% %%%     io:fwrite("vars: ~p.\n", [[X || X <- dict:to_list(Vars)]]),
%%     T2 = hhl_transform:transform(T1, Vars),
%%     Set = sets:from_list(Esc),
%%     H = fun (Node, Ctxt, Cont) ->
%% 		case get_ann(Node) of
%% 		    [{label, L} | _] ->
%% 			B = sets:is_element(L, Set),
%% 			bf(Node, Ctxt, Cont, B);
%% 		    _ ->
%% 			bf(Node, Ctxt, Cont, false)
%% 		end
%%     end,
%%     {ok, FD} = file:open("out.html",[write]),
%%     Txt = cerl_prettypr:format(T2, [{hook, H},{user,false}]),
%%     io:put_chars(FD, "<pre>\n"),
%%     io:put_chars(FD, html(Txt)),
%%     io:put_chars(FD, "</pre>\n"),
%%     file:close(FD),
%%     {ok, Time1 - Time0}.

%% html(Cs) ->
%%     html(Cs, []).

%% html([$#, $< | Cs], As) ->
%%     html_1(Cs, [$< | As]);
%% html([$< | Cs], As) ->
%%     html(Cs, ";tl&" ++ As);
%% html([$> | Cs], As) ->
%%     html(Cs, ";tg&" ++ As);
%% html([$& | Cs], As) ->
%%     html(Cs, ";pma&" ++ As);
%% html([C | Cs], As) ->
%%     html(Cs, [C | As]);
%% html([], As) ->
%%     lists:reverse(As).

%% html_1([$> | Cs], As) ->
%%     html(Cs, [$> | As]);
%% html_1([C | Cs], As) ->
%%     html_1(Cs, [C | As]).

%% bf(Node, Ctxt, Cont, B) ->
%%     B0 = cerl_prettypr:get_ctxt_user(Ctxt),
%%     if B /= B0 ->
%% 	    Ctxt1 = cerl_prettypr:set_ctxt_user(Ctxt, B),
%% 	    Doc = Cont(Node, Ctxt1),
%% 	    case B of
%% 		true ->
%% 		    Start = "<b>",
%% 		    End = "</b>";
%% 		false ->
%% 		    Start = "</b>",
%% 		    End = "<b>"
%% 	    end,
%% 	    markup(Doc, Start, End);
%%        true ->
%% 	    Cont(Node, Ctxt)
%%     end.

%% color(Doc) ->
%% %    Doc.
%%     markup(Doc, "<font color=blue>", "</font>").

%% markup(Doc, Start, End) ->
%%     prettypr:beside(
%%       prettypr:null_text([$# | Start]),
%%       prettypr:beside(Doc,
%% 		      prettypr:null_text([$# | End]))).


%% =====================================================================
%% annotate(Tree) -> {Tree1, Escapes, Vars}
%%
%%	    Tree = cerl:cerl()
%%
%%	Analyzes `Tree' (see `analyze') and appends a term 'escapes', to
%%	the annotation list of each constructor expression node and of
%%	`Tree', corresponding to the escape information derived by the
%%	analysis. Any previous such annotations are removed from `Tree'.
%%	`Tree1' is the modified tree; for details on `OutList',
%%	`Outputs' , `Dependencies', `Escapes' and `Parents', see
%%	`analyze'.
%%
%%	Note: `Tree' must be annotated with labels in order to use this
%%	function; see `analyze' for details.

-type label()   :: integer() | 'external' | 'top'.
-type ordset(X) :: [X].  % XXX: TAKE ME OUT

-spec annotate(cerl:cerl()) -> {cerl:cerl(), ordset(label()), dict:dict()}.

annotate(Tree) ->
    {Esc0, Vars} = analyze(Tree),
    Esc = sets:from_list(Esc0),
    F = fun (T) ->
		case type(T) of
		    literal -> T;
%%% 		    var ->
%%% 			L = get_label(T),
%%% 			T1 = ann_escape(T, L, Esc),
%%% 			X = dict:fetch(L, Vars),
%%% 			set_ann(T1, append_ann({s,X}, get_ann(T1)));
		    _ ->
			L = get_label(T),
			ann_escape(T, L, Esc)
		end
	end,
    {cerl_trees:map(F, Tree), Esc0, Vars}.

ann_escape(T, L, Esc) ->
    case sets:is_element(L, Esc) of
	true ->
	    set_ann(T, append_ann(escapes, get_ann(T)));
	false ->
	    T
    end.

append_ann(Tag, [X | Xs]) ->
    if tuple_size(X) >= 1, element(1, X) =:= Tag -> 
	    append_ann(Tag, Xs);
       true ->
	    [X | append_ann(Tag, Xs)]
    end;
append_ann(Tag, []) ->
    [Tag].


%% =====================================================================
%% analyze(Tree) -> Escapes
%%
%%	    Tree = cerl:cerl()
%%	    Escapes = ordset(Label)
%%	    Label = integer() | external | top
%%
%%	Analyzes a module or an expression represented by `Tree'.
%%
%%	`Escapes' is the set of labels of constructor expressions in
%%	`Tree' such that the created values may be accessed from outside
%%	`Tree'.
%%
%%	Note: `Tree' must be annotated with labels (as done by the
%%	function `cerl_trees:label/1') in order to use this function.
%%	The label annotation `{label, L}' (where L should be an integer)
%%	must be the first element of the annotation list of each node in
%%	the tree. Instances of variables bound in `Tree' which denote
%%	the same variable must have the same label; apart from this,
%%	labels should be unique. Constant literals do not need to be
%%	labeled.

-record(state, {vars, out, dep, work, funs, k}).

%% Note: We assume that all remote calls and primops return a single
%% value.

%% The analysis determines which objects (identified by the
%% corresponding "cons-point" labels in the code) are likely to be
%% passed in a message. (If so, we say that they "escape".) It is always
%% safe to assume either case, because the send operation will assure
%% that things are copied if necessary. This analysis tries to
%% anticipate that copying will be done.
%%
%% Rules:
%%   1) An object passed as message argument (or part of such an
%%   argument) to a known send-operation, will probably be a message.
%%   2) A received value is always a message (safe).
%%   3) The external function can return any object (unsafe).
%%   4) A function called from the external function can receive any
%%   object (unsafe) as argument.
%%   5) Unknown functions/operations can return any object (unsafe).

%% We wrap the given syntax tree T in a fun-expression labeled `top',
%% which is initially in the set of escaped labels. `top' will be
%% visited at least once.
%%
%% We create a separate function labeled `external', defined as:
%% "'external'/1 = fun () -> Any", which will represent any and all
%% functions outside T, and which returns the 'unsafe' value.

analyze(Tree) ->
    analyze(Tree, ?DEF_LIMIT).

analyze(Tree, Limit) ->
    {_, _, Esc, Dep, _Par} = cerl_closurean:analyze(Tree),
%%%     io:fwrite("dependencies: ~w.\n", [dict:to_list(Dep)]),
    analyze(Tree, Limit, Dep, Esc).

analyze(Tree, Limit, Dep0, Esc0) ->
    %% Note that we use different name spaces for variable labels and
    %% function/call site labels, so we can reuse some names here. We
    %% assume that the labeling of Tree only uses integers, not atoms.
    Any = ann_c_var([{label, any}], 'Any'),
    External = ann_c_var([{label, external}], {external, 1}),
    ExtFun = ann_c_fun([{label, external}], [], Any),
%%%     io:fwrite("external fun:\n~s.\n",
%%%   	      [cerl_prettypr:format(ExtFun, [noann, {paper, 80}])]),
    Top = ann_c_var([{label, top}], {top, 0}),
    TopFun = ann_c_fun([{label, top}], [], Tree),

    %% The "start fun" just makes the initialisation easier. It is not
    %% itself in the call graph.
    StartFun =  ann_c_fun([{label, start}], [],
			  c_letrec([{External, ExtFun}, {Top, TopFun}],
				   c_nil())),
%%%     io:fwrite("start fun:\n~s.\n",
%%%  	      [cerl_prettypr:format(StartFun, [{paper, 80}])]),

    %% Initialise the Any and Escape variables. Gather a database of all
    %% fun-expressions in Tree and initialise their outputs and parameter
    %% variables. All escaping functions can receive any values as
    %% inputs. Bind all module- and letrec-defined variables to their
    %% corresponding labels.
    Esc = sets:from_list(Esc0),
    Unsafe = unsafe(),
    Empty = empty(),
    Funs0 = dict:new(),
    Vars0 = dict:store(escape, empty(), 
		       dict:store(any, Unsafe, dict:new())),
    Out0 = dict:new(),
    F = fun (T, S = {Fs, Vs, Os}) ->
		case type(T) of
		    'fun' ->
			L = get_label(T),
			As = fun_vars(T),
			X = case sets:is_element(L, Esc) of
				true -> Unsafe;
				false -> Empty
			    end,
			{dict:store(L, T, Fs),
			 bind_vars_single(As, X, Vs),
			 dict:store(L, none, Os)};
		    letrec ->
			{Fs, bind_defs(letrec_defs(T), Vs), Os};
		    module ->
			{Fs, bind_defs(module_defs(T), Vs), Os};
		    _ ->
			S
		end
	end,
    {Funs, Vars, Out} = cerl_trees:fold(F, {Funs0, Vars0, Out0}, StartFun),

    %% Add the dependency for the loop in 'external':
    Dep = add_dep(loop, external, Dep0),

    %% Enter the fixpoint iteration at the StartFun.
    St = loop(StartFun, start, #state{vars = Vars,
				      out = Out,
				      dep = Dep,
				      work = init_work(),
				      funs = Funs,
				      k = Limit}),
    Ms = labels(dict:fetch(escape, St#state.vars)),
    {Ms, St#state.vars}.

loop(T, L, St0) ->
%%%     io:fwrite("analyzing: ~w.\n",[L]),
%%%     io:fwrite("work: ~w.\n", [St0#state.work]),
    Xs0 = dict:fetch(L, St0#state.out),
    {Xs1, St1} = visit(fun_body(T), L, St0),
    Xs = limit(Xs1, St1#state.k),
    {W, M} = case equal(Xs0, Xs) of
		 true ->
		     {St1#state.work, St1#state.out};
		 false ->
%%%       		     io:fwrite("out (~w) changed: ~w <- ~w.\n",
%%%       			       [L, Xs, Xs0]),
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
%%%     io:fwrite("visiting: ~w.\n",[type(T)]),
    case type(T) of
	literal ->
	    %% This is (or should be) a constant, even if it's compound,
	    %% so it's bugger all whether it is sent or not.
	    case cerl:concrete(T) of
		[] -> {[empty()], St};
		X when is_atom(X) -> {[empty()], St};
		X when is_integer(X) -> {[empty()], St};
		X when is_float(X) -> {[empty()], St};
		_ ->
		    exit({not_literal, T})
	    end;
	var ->
	    %% If a variable is not already in the store here, it must
	    %% be free in the program.
	    L1 = get_label(T),
	    Vars = St#state.vars,
	    case dict:find(L1, Vars) of
		{ok, X} ->
		    {[X], St};
		error ->
%%% 		    io:fwrite("free var: ~w.\n",[L1]),
		    X = unsafe(),
		    St1 = St#state{vars = dict:store(L1, X, Vars)},
		    {[X], St1}
	    end;
	'fun' ->
	    %% Must revisit the fun also, because its environment might
	    %% have changed. (We don't keep track of such dependencies.)
	    L1 = get_label(T),
	    St1 = St#state{work = add_work([L1], St#state.work)},
	    %% Currently, lambda expressions can only be locally
	    %% allocated, and therefore we have to force copying by
	    %% treating them as "unsafe" for now.
	    {[unsafe()], St1};
	    %% {[singleton(L1)], St1};
	values ->
	    visit_list(values_es(T), L, St);
	cons ->
	    {[X1, X2], St1} = visit_list([cons_hd(T), cons_tl(T)], L, St),
	    L1 = get_label(T),
	    X = make_cons(L1, X1, X2),
	    %% Also store the values of the elements.
 	    Hd = get_hd(X),
 	    Tl = get_tl(X),
 	    St2 = St1#state{vars = dict:store(L1, [Hd, Tl], St1#state.vars)},
	    {[X], St2};
	tuple ->
	    {Xs, St1} = visit_list(tuple_es(T), L, St),
	    L1 = get_label(T),
	    %% Also store the values of the elements.
	    St2 = St1#state{vars = dict:store(L1, Xs, St1#state.vars)},
	    {[struct(L1, Xs)], St2};
	'let' ->
	    {Xs, St1} = visit(let_arg(T), L, St),
	    Vars = bind_vars(let_vars(T), Xs, St1#state.vars),
	    visit(let_body(T), L, St1#state{vars = Vars});
	seq ->
	    {_, St1} = visit(seq_arg(T), L, St),
	    visit(seq_body(T), L, St1);
	apply ->
	    {_F, St1} = visit(apply_op(T), L, St),
	    {As, St2} = visit_list(apply_args(T), L, St1),
	    L1 = get_label(T),
	    Ls = get_deps(L1, St#state.dep),
	    Out = St2#state.out,
	    Xs1 = join_list([dict:fetch(X, Out) || X <- Ls]),
	    {Xs1, call_site(Ls, As, St2)};
	call ->
	    M = call_module(T),
	    F = call_name(T),
	    As = call_args(T),
	    {_, St1} = visit(M, L, St),
	    {_, St2} = visit(F, L, St1),
	    {Xs, St3} = visit_list(As, L, St2),
	    L1 = get_label(T),
	    remote_call(M, F, Xs, As, L1, St3);
	primop ->
 	    As = primop_args(T),
 	    {Xs, St1} = visit_list(As, L, St),
 	    F = atom_val(primop_name(T)),
 	    primop_call(F, length(Xs), Xs, As, St1);
	'case' ->
	    {Xs, St1} = visit(case_arg(T), L, St),
	    visit_clauses(Xs, case_clauses(T), L, St1);
	'receive' ->
	    %% The received value is of course a message, so it
	    %% is 'empty()', not 'unsafe()'.
	    X = empty(),
	    {Xs1, St1} = visit_clauses([X], receive_clauses(T), L, St),
	    {_, St2} = visit(receive_timeout(T), L, St1),
	    {Xs2, St3} = visit(receive_action(T), L, St2),
	    {join(Xs1, Xs2), St3};
	'try' ->
	    {Xs1, St1} = visit(try_arg(T), L, St),
	    X = unsafe(),
	    Vars = bind_vars(try_vars(T), Xs1, St1#state.vars),
	    {Xs2, St2} = visit(try_body(T), L, St1#state{vars = Vars}),
	    EVars = bind_vars(try_evars(T), [X, X, X], St2#state.vars),
	    {Xs3, St3} = visit(try_handler(T), L, St2#state{vars = EVars}),
	    {join(Xs2, Xs3), St3};
	'catch' ->
	    %% If we catch an exception, we can get unsafe data.
	    {Xs, St1} = visit(catch_body(T), L, St),
	    {join([unsafe()], Xs), St1};
	binary ->
	    %% Binaries are heap objects, but we don't have special
	    %% shared-heap allocation operators for them at the moment.
	    %% They must therefore be treated as unsafe.
	    {_, St1} = visit_list(binary_segments(T), L, St),
	    {[unsafe()], St1};
	bitstr ->
	    %% The other fields are constant literals.
	    {_, St1} = visit(bitstr_val(T), L, St),
	    {_, St2} = visit(bitstr_size(T), L, St1),
	    {none, St2};
	letrec ->
	    %% All the bound funs should be revisited, because the
	    %% environment might have changed.
	    Ls = [get_label(F) || {_, F} <- letrec_defs(T)],
	    St1 = St#state{work = add_work(Ls, St#state.work)},
	    visit(letrec_body(T), L, St1);
	module ->
	    %% We regard a module as a tuple of function variables in
	    %% the body of a `letrec'.
	    visit(c_letrec(module_defs(T),
			   ann_c_tuple([{label, get_label(T)}],
				       module_exports(T))),
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
	    _ -> empty()
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
    bind_defs(Ds, dict:store(get_label(V), singleton(get_label(F)), Vars));
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

%% The lists might not be of the same length.

bind_pats_list([P | Ps], [X | Xs], Vars) ->
    bind_pats_list(Ps, Xs, bind_pat_vars(P, X, Vars));
bind_pats_list(Ps, [], Vars) ->
    bind_pats_single(Ps, empty(), Vars);
bind_pats_list([], _, Vars) ->
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
	    bind_pats_list([cons_hd(P), cons_tl(P)],
			   [get_hd(X), get_tl(X)], Vars);
	tuple ->
	    case elements(X) of
		none ->
		    bind_vars_single(pat_vars(P), X, Vars);
		Xs ->
		    bind_pats_list(tuple_es(P), Xs, Vars)
	    end;
	binary ->
	    %% See the handling of binary-expressions.
	    bind_pats_single(binary_segments(P), unsafe(), Vars);
	bitstr ->
	    %% See the handling of binary-expressions.
	    bind_pats_single([bitstr_val(P), bitstr_size(P)],
			     unsafe(), Vars);
	alias ->
	    P1 = alias_pat(P),
	    Vars1 = bind_pat_vars(P1, X, Vars),
	    dict:store(get_label(alias_var(P)), X, Vars1)
    end.

%%% %% This is the "exact" version of list representation, which simply
%%% %% mimics the actual cons, head and tail operations.
%%% make_cons(L, X1, X2) ->
%%%     struct(L1, [X1, X2]).
%%% get_hd(X) ->
%%%     case elements(X) of
%%% 	none -> X;
%%% 	[X1 | _] -> X1;
%%% 	_ -> empty()
%%%     end.
%%% get_tl(X) ->
%%%     case elements(X) of
%%%  	none -> X;
%%%  	[_, X2 | _] -> X2;
%%%  	_ -> empty()
%%%     end.

%% This version does not unnecessarily confuse spine labels with element
%% labels, and is safe. However, it loses precision if cons cells are
%% used for other things than proper lists.

make_cons(L, X1, X2) ->
    %% join subtypes and cons locations
    join_single(struct(L, [X1]), X2).

get_hd(X) ->
    case elements(X) of
 	none -> X;
 	[X1 | _] -> X1;    % First element represents list subtype.
 	_ -> empty()
    end.

get_tl(X) -> X.   % Tail of X has same type as X.

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

%% This handles a call site, updating parameter variables with respect
%% to the actual parameters. The 'external' function is handled
%% specially, since it can get an arbitrary number of arguments. For our
%% purposes here, calls to the external function can be ignored.

call_site(Ls, Xs, St) ->
%%%     io:fwrite("call site: ~w -> ~w (~w).\n", [L, Ls, Xs]),
    {W, V} = call_site(Ls, Xs, St#state.work, St#state.vars,
		       St#state.funs, St#state.k),
    St#state{work = W, vars = V}.

call_site([external | Ls], Xs, W, V, Fs, Limit) ->
    call_site(Ls, Xs, W, V, Fs, Limit);
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
    X1 = limit_single(join_single(X, X0), Limit),
    case equal_single(X0, X1) of
	true ->
	    {Vars, Ch};
	false ->
%%%     	    io:fwrite("arg (~w) changed: ~w <- ~w + ~w.\n",
%%%      		      [L, X1, X0, X]),
	    {dict:store(L, X1, Vars), true}
    end.

%% This handles escapes from things like primops and remote calls.

escape(Xs, Ns, St) ->
    escape(Xs, Ns, 1, St).

escape([_ | Xs], Ns=[N1 | _], N, St) when is_integer(N1), N1 > N ->
    escape(Xs, Ns, N + 1, St);
escape([X | Xs], [N | Ns], N, St) ->
    Vars = St#state.vars,
    X0 = dict:fetch(escape, Vars),
    X1 = join_single(X, X0),
    case equal_single(X0, X1) of
	true ->
	    escape(Xs, Ns, N + 1, St);
	false ->
%%%    	    io:fwrite("escape changed: ~w <- ~w + ~w.\n", [X1, X0, X]),
	    Vars1 = dict:store(escape, X1, Vars),
	    escape(Xs, Ns, N + 1, St#state{vars = Vars1})
    end;
escape(Xs, [_ | Ns], N, St) ->
    escape(Xs, Ns, N + 1, St);
escape(_, _, _, St) ->
    St.

%% Handle primop calls: (At present, we assume that all unknown calls
%% yield exactly one value. This might have to be changed.)

primop_call(F, A, Xs, _As, St0) ->
    %% St1 = case is_escape_op(F, A) of
    %%	      [] -> St0;
    %%	      Ns -> escape(Xs, Ns, St0)
    %%	  end,
    St1 = St0,
    case is_imm_op(F, A) of
	true ->
	    {[empty()], St1};
	false ->
	    call_unknown(Xs, St1)
    end.

%% Handle remote-calls: (At present, we assume that all unknown calls
%% yield exactly one value. This might have to be changed.)

remote_call(M, F, Xs, As, L, St) ->
    case is_c_atom(M) andalso is_c_atom(F) of
	true ->
	    remote_call_1(atom_val(M), atom_val(F), length(Xs),
			  Xs, As, L, St);
	false ->
	    %% Unknown function
	    call_unknown(Xs, St)
    end.

%% When calling an unknown function, we assume that the result does
%% *not* contain any of the constructors in its arguments (but it could
%% return locally allocated data that we don't know about). Note that
%% even a "pure" function can still cons up new data.

call_unknown(_Xs, St) ->
    {[unsafe()], St}.

%% We need to handle some important standard functions in order to get
%% decent precision.
%% TODO: foldl, map, mapfoldl

remote_call_1(erlang, hd, 1, [X], _As, _L, St) ->
    {[get_hd(X)], St};
remote_call_1(erlang, tl, 1, [X], _As, _L, St) ->
    {[get_tl(X)], St};
remote_call_1(erlang, element, 2, [_,X], [N|_], _L, St) ->
    case elements(X) of
	none -> {[X], St};
	Xs ->
	    case is_c_int(N) of
		true ->
		    N1 = int_val(N),
		    if is_integer(N1), 1 =< N1, N1 =< length(Xs) ->
			    {[nth(N1, Xs)], St};
		       true ->
			    {none, St}
		    end;
		false ->
		    %% Even if we don't know which element is selected,
		    %% we know that the top level is never part of the
		    %% returned value.
		    {[join_single_list(Xs)], St}
	    end
    end;
remote_call_1(erlang, setelement, 3, [_,X, Y], [N|_], L, St) ->
    %% The constructor gets the label of the call operation.
    case elements(X) of
	none -> {[join_single(singleton(L), join_single(X, Y))], St};
	Xs ->
	    case is_c_int(N) of
		true ->
		    N1 = int_val(N),
		    if is_integer(N1), 1 =< N1, N1 =< length(Xs) ->
			    Xs1 = set_nth(N1, Y, Xs),
			    {[struct(L, Xs1)], St};
		       true ->
			    {none, St}
		    end;
		false ->
		    %% Even if we don't know which element is selected,
		    %% we know that the top level is never part of the
		    %% returned value (a new tuple is always created).
		    Xs1 = [join_single(Y, X1) || X1 <- Xs],
		    {[struct(L, Xs1)], St}
	    end
    end;
remote_call_1(erlang, '++', 2, [X1,X2], _As, _L, St) ->
    %% Note: this is unsafe for non-proper lists! (See make_cons/3).
    %% No safe version is implemented.
    {[join_single(X1, X2)], St};
remote_call_1(erlang, '--', 2, [X1,_X2], _As, _L, St) ->
    {[X1], St};
remote_call_1(lists, append, 2, Xs, As, L, St) ->
    remote_call_1(erlang, '++', 2, Xs, As, L, St);
remote_call_1(lists, subtract, 2, Xs, As, L, St) ->
    remote_call_1(erlang, '--', 2, Xs, As, L, St);
remote_call_1(M, F, A, Xs, _As, _L, St0) ->
    St1 = case is_escape_op(M, F, A) of
	      [] -> St0;
	      Ns -> escape(Xs, Ns, St0)
	  end,
    case is_imm_op(M, F, A) of
	true ->
	    {[empty()], St1};
	false ->
	    call_unknown(Xs, St1)
    end.

%% 1-based n:th-element list selector and update function.

nth(1, [X | _Xs]) -> X;
nth(N, [_X | Xs]) when N > 1 -> nth(N - 1, Xs).

set_nth(1, Y, [_X | Xs]) -> [Y | Xs];
set_nth(N, Y, [X | Xs]) when N > 1 -> [X | set_nth(N - 1, Y, Xs)].

%% Domain: none | [V], where V = {S, none} | {S, [V]}, S = set(integer()).

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

join_list([Xs | Xss]) ->
    join(Xs, join_list(Xss));
join_list([]) ->
    none.

empty() -> {set__new(), []}.

singleton(X) -> {set__singleton(X), []}.

struct(X, Xs) -> {set__singleton(X), Xs}.

elements({_, Xs}) -> Xs.

unsafe() -> {set__singleton(unsafe), none}.

equal(none, none) -> true;
equal(none, _) -> false;
equal(_, none) -> false;
equal(X1, X2) -> equal_1(X1, X2).

equal_1([X1 | Xs1], [X2 | Xs2]) ->
    equal_single(X1, X2) andalso equal_1(Xs1, Xs2);
equal_1([], []) -> true;
equal_1(_, _) -> false.

equal_single({S1, none}, {S2, none}) ->
    set__equal(S1, S2);
equal_single({_, none}, _) ->
    false;
equal_single(_, {_, none}) ->
    false;
equal_single({S1, Vs1}, {S2, Vs2}) ->
    set__equal(S1, S2) andalso equal_single_lists(Vs1, Vs2).

equal_single_lists([X1 | Xs1], [X2 | Xs2]) ->
    equal_single(X1, X2) andalso equal_single_lists(Xs1, Xs2);
equal_single_lists([], []) ->
    true;
equal_single_lists(_, _) ->
    false.

join_single({S, none}, V) ->
    {set__union(S, labels(V)), none};
join_single(V, {S, none}) ->
    {set__union(S, labels(V)), none};
join_single({S1, Vs1}, {S2, Vs2}) ->
    {set__union(S1, S2), join_single_lists(Vs1, Vs2)}.

join_single_list([V | Vs]) ->
    join_single(V, join_single_list(Vs));
join_single_list([]) ->
    empty().

%% If one list has more elements that the other, and N is the length of
%% the longer list, then the result has N elements.

join_single_lists([V1], [V2]) ->
    [join_single(V1, V2)];
join_single_lists([V1 | Vs1], [V2 | Vs2]) ->
    [join_single(V1, V2) | join_single_lists(Vs1, Vs2)];
join_single_lists([], Vs) -> Vs;
join_single_lists(Vs, []) -> Vs.

collapse(V) ->
    {labels(V), none}.

%% collapse_list([]) ->
%%     empty();
%% collapse_list(Vs) ->
%%     {labels_list(Vs), none}.

labels({S, none}) -> S;
labels({S, []}) -> S;
labels({S, Vs}) -> set__union(S, labels_list(Vs)).

labels_list([V]) ->
    labels(V);
labels_list([V | Vs]) ->
    set__union(labels(V), labels_list(Vs)).

limit(none, _K) -> none;
limit(X, K) -> limit_list(X, K).

limit_list([X | Xs], K) ->
    [limit_single(X, K) | limit_list(Xs, K)];
limit_list([], _) ->
    [].

limit_single({_, none} = V, _K) ->
    V;
limit_single({_, []} = V, _K) ->
    V;
limit_single({S, Vs}, K) when K > 0 ->
    {S, limit_list(Vs, K - 1)};
limit_single(V, _K) ->
    collapse(V).

%% Set abstraction for label sets in the domain.

%% set__is_empty([]) -> true;
%% set__is_empty(_) -> false.

set__new() -> [].

set__singleton(X) -> [X].

set__to_list(S) -> S.

%% set__from_list(S) -> ordsets:from_list(S).

set__union(X, Y) -> ordsets:union(X, Y).

set__add(X, S) -> ordsets:add_element(X, S).

set__is_member(X, S) -> ordsets:is_element(X, S).    

%% set__subtract(X, Y) -> ordsets:subtract(X, Y).

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

get_deps(L, Dep) ->
    case dict:find(L, Dep) of
	{ok, Ls} -> Ls;
	error -> []
    end.

%% Escape operators may let their arguments escape. For this analysis,
%% only send-operations are considered as causing escapement, and only
%% in specific arguments.

%% is_escape_op(_F, _A) -> [].

-spec is_escape_op(atom(), atom(), arity()) -> [arity()].

is_escape_op(erlang, '!', 2) -> [2];
is_escape_op(erlang, send, 2) -> [2];
is_escape_op(erlang, spawn, 1) -> [1];
is_escape_op(erlang, spawn, 3) -> [3];
is_escape_op(erlang, spawn, 4) -> [4];
is_escape_op(erlang, spawn_link, 3) -> [3];
is_escape_op(erlang, spawn_link, 4) -> [4];
is_escape_op(_M, _F, _A) -> [].

%% "Immediate" operators will never return heap allocated data. This is
%% of course true for operators that never return, like 'exit/1'. (Note
%% that floats are always heap allocated objects, and that most integer
%% arithmetic can return a bignum on the heap.)

-spec is_imm_op(atom(), arity()) -> boolean().

is_imm_op(match_fail, 1) -> true; 
is_imm_op(_, _) -> false.

-spec is_imm_op(atom(), atom(), arity()) -> boolean().

is_imm_op(erlang, self, 0) -> true;
is_imm_op(erlang, '=:=', 2) -> true;
is_imm_op(erlang, '==', 2) -> true;
is_imm_op(erlang, '=/=', 2) -> true;
is_imm_op(erlang, '/=', 2) -> true;
is_imm_op(erlang, '<', 2) -> true;
is_imm_op(erlang, '=<', 2) -> true;
is_imm_op(erlang, '>', 2) -> true;
is_imm_op(erlang, '>=', 2) -> true;
is_imm_op(erlang, 'and', 2) -> true;
is_imm_op(erlang, 'or', 2) -> true;
is_imm_op(erlang, 'xor', 2) -> true;
is_imm_op(erlang, 'not', 1) -> true;
is_imm_op(erlang, is_alive, 0) -> true;
is_imm_op(erlang, is_atom, 1) -> true;
is_imm_op(erlang, is_binary, 1) -> true;
is_imm_op(erlang, is_builtin, 3) -> true;
is_imm_op(erlang, is_float, 1) -> true;
is_imm_op(erlang, is_function, 1) -> true;
is_imm_op(erlang, is_integer, 1) -> true;
is_imm_op(erlang, is_list, 1) -> true;
is_imm_op(erlang, is_number, 1) -> true;
is_imm_op(erlang, is_pid, 1) -> true;
is_imm_op(erlang, is_port, 1) -> true;
is_imm_op(erlang, is_process_alive, 1) -> true;
is_imm_op(erlang, is_reference, 1) -> true;
is_imm_op(erlang, is_tuple, 1) -> true;
is_imm_op(erlang, length, 1) -> true;    % never a bignum
is_imm_op(erlang, list_to_atom, 1) -> true;
is_imm_op(erlang, node, 0) -> true;
is_imm_op(erlang, node, 1) -> true;
is_imm_op(erlang, throw, 1) -> true;
is_imm_op(erlang, exit, 1) -> true;
is_imm_op(erlang, error, 1) -> true;
is_imm_op(erlang, error, 2) -> true;
is_imm_op(_M, _F, _A) -> false.
